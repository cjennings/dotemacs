;;; test-vterm-tmux-history.el --- Tests for tmux history capture UX -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercises the Emacs-owned history buffer used to copy text from the
;; current tmux pane without entering tmux copy-mode.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(setq load-prefer-newer t)
(defvar vterm-mode-map (make-sparse-keymap))
(defvar vterm-copy-mode-map (make-sparse-keymap))
(keymap-set vterm-mode-map "C-c C-t" #'ignore)
(require 'vterm-config)
(require 'testutil-vterm-buffers)

(defmacro test-vterm-tmux-history--with-tmux-mock (responses &rest body)
  "Run BODY with `process-file' mocked for tmux RESPONSES.

RESPONSES is an alist of (ARGS EXIT-CODE OUTPUT)."
  (declare (indent 1))
  `(let ((calls nil))
     (cl-letf (((symbol-function 'process-file)
                (lambda (program _infile destination _display &rest args)
                  (push (cons program args) calls)
                  (let* ((entry (seq-find
                                 (lambda (candidate)
                                   (equal (car candidate) args))
                                 ,responses))
                         (exit-code (or (cadr entry) 1))
                         (output (or (caddr entry) "")))
                    (when destination
                      (let ((buffer (cond
                                     ((eq destination t) (current-buffer))
                                     ((bufferp destination) destination)
                                     ((consp destination) (car destination)))))
                        (when (bufferp buffer)
                          (with-current-buffer buffer
                            (insert output)))))
                    exit-code))))
       ,@body)))

(ert-deftest test-vterm-tmux-history--pane-id-for-tty-matches-client ()
  "Normal: current vterm pty maps to the active pane for that tmux client."
  (test-vterm-tmux-history--with-tmux-mock
      '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
         "/dev/pts/1\t%1\n/dev/pts/8\t%8\n"))
    (should (equal (cj/vterm--tmux-pane-id-for-tty "/dev/pts/8") "%8"))))

(ert-deftest test-vterm-tmux-history--capture-pane-uses-full-history ()
  "Normal: capture asks tmux for joined full pane history."
  (test-vterm-tmux-history--with-tmux-mock
      '((("capture-pane" "-p" "-J" "-S" "-" "-E" "-" "-t" "%8") 0
         "first line\nsecond line\n"))
    (should (equal (cj/vterm--tmux-capture-pane "%8")
                   "first line\nsecond line\n"))))

(ert-deftest test-vterm-tmux-history-open-renders-read-only-history-buffer ()
  "Normal: command renders tmux history in a normal Emacs buffer."
  (let ((origin (cj/test--make-fake-vterm-buffer "*test-vterm-history-origin*")))
    (unwind-protect
        (with-current-buffer origin
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process) "/dev/pts/8"))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _)
                       (set-buffer buffer)
                       buffer)))
            (test-vterm-tmux-history--with-tmux-mock
                '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
                   "/dev/pts/8\t%8\n")
                  (("capture-pane" "-p" "-J" "-S" "-" "-E" "-" "-t" "%8") 0
                   "history http://example.com\n"))
              (cj/vterm-tmux-history)
              (should (eq major-mode 'cj/vterm-tmux-history-mode))
              (should buffer-read-only)
              (should (string-match-p "history http://example.com"
                                      (buffer-string)))))))
      (cj/test--kill-buffers-matching-prefix "*vterm tmux history")
      (when (buffer-live-p origin)
        (kill-buffer origin))))

(ert-deftest test-vterm-tmux-history-copy-copies-region-and-returns ()
  "Normal: M-w copies the region, kills history buffer, and restores origin."
  (let ((origin (get-buffer-create "*test-vterm-history-return*"))
        (kill-ring nil))
    (unwind-protect
        (let ((history (get-buffer-create "*vterm tmux history: test*")))
          (with-current-buffer origin
            (erase-buffer)
            (insert "origin")
            (goto-char (point-min)))
          (switch-to-buffer origin)
          (let ((origin-window (selected-window)))
            (with-current-buffer history
              (cj/vterm-tmux-history-mode)
              (let ((inhibit-read-only t))
                (insert "alpha\nbeta\ngamma\n"))
              (setq-local cj/vterm-tmux-history--origin-buffer origin)
              (setq-local cj/vterm-tmux-history--origin-window origin-window)
              (setq-local cj/vterm-tmux-history--origin-point (point-min))
              (goto-char (point-min))
              (set-mark (point))
              (goto-char (point-at-eol 2))
              (activate-mark)
              (cj/vterm-tmux-history-copy-and-quit))
            (should (equal (car kill-ring) "alpha\nbeta"))
            (should-not (buffer-live-p history))
            (should (eq (current-buffer) origin))
            (should (= (point) (point-min)))))
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

(ert-deftest test-vterm-keymap-includes-history-and-copy-bindings ()
  "Normal: personal vterm map owns the high-level vterm UX commands."
  (should (member "C-;" vterm-keymap-exceptions))
  (should-not (eq (keymap-lookup cj/custom-keymap "X c") #'vterm-copy-mode))
  (should (eq (keymap-lookup cj/custom-keymap "x C") #'cj/vterm-tmux-history))
  (should (eq (keymap-lookup cj/custom-keymap "x c") #'vterm-copy-mode))
  (should (equal (keymap-lookup vterm-mode-map "C-;") cj/custom-keymap))
  (should (eq (keymap-lookup vterm-mode-map "C-; x C") #'cj/vterm-tmux-history))
  (should (eq (keymap-lookup vterm-mode-map "C-; x c") #'vterm-copy-mode))
  (should-not (keymap-lookup vterm-mode-map "C-c C-t")))

(ert-deftest test-vterm-keymap-prompt-navigation ()
  "Normal: n/p navigate prompts, capital N creates a new vterm buffer."
  (should (eq (keymap-lookup cj/custom-keymap "x n") #'vterm-next-prompt))
  (should (eq (keymap-lookup cj/custom-keymap "x p") #'vterm-previous-prompt))
  (should (eq (keymap-lookup cj/custom-keymap "x N") #'vterm)))

(ert-deftest test-vterm-pause-not-bound-to-copy-mode ()
  "Normal: <pause> is no longer wired as a vterm-copy-mode entry point.
The personal `C-; x c' binding is the canonical entry; <pause> is rare on
modern keyboards and was redundant."
  (let ((binding (keymap-lookup vterm-mode-map "<pause>")))
    (should-not (eq binding #'vterm-copy-mode))))

(ert-deftest test-vterm-copy-mode-cancel-keys ()
  "Normal: copy mode has explicit copy and no-copy exits."
  (should (eq (keymap-lookup vterm-copy-mode-map "C-g")
              #'cj/vterm-copy-mode-cancel))
  (should (eq (keymap-lookup vterm-copy-mode-map "<escape>")
              #'cj/vterm-copy-mode-cancel))
  (should (eq (keymap-lookup vterm-copy-mode-map "M-w")
              #'vterm-copy-mode-done)))

(ert-deftest test-vterm-copy-mode-cancel-errors-outside-copy-mode ()
  "Error: `cj/vterm-copy-mode-cancel' refuses to run when not in copy mode."
  (with-temp-buffer
    (should-error (cj/vterm-copy-mode-cancel) :type 'user-error)))

(ert-deftest test-vterm-current-tmux-pane-id-rejects-non-vterm-buffer ()
  "Error: pane-id lookup refuses a buffer that is not in `vterm-mode'."
  (with-temp-buffer
    (should-error (cj/vterm--current-tmux-pane-id) :type 'user-error)))

(ert-deftest test-vterm-current-tmux-pane-id-accepts-ai-vterm-named-buffer ()
  "Normal: an AI-vterm-named buffer still resolves by process TTY.

The copy path belongs to `vterm-mode', not to `*vterm*'-named buffers.
A buffer named like `claude [repo]' (ai-vterm.el's naming) is a
`vterm-mode' buffer and must inherit tmux history copy.  The pane lookup
keys off the live process TTY, never the buffer name -- so the
AI-vterm name neither helps nor blocks resolution."
  (let ((claude (cj/test--make-fake-vterm-buffer "claude [emacs.d]")))
    (unwind-protect
        (with-current-buffer claude
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process) "/dev/pts/8")))
            (test-vterm-tmux-history--with-tmux-mock
                '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
                   "/dev/pts/1\t%1\n/dev/pts/8\t%8\n"))
              (should (equal (cj/vterm--current-tmux-pane-id) "%8")))))
      (when (buffer-live-p claude)
        (kill-buffer claude)))))

(provide 'test-vterm-tmux-history)
;;; test-vterm-tmux-history.el ends here

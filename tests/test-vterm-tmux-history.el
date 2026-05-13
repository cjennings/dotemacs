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
        (save-window-excursion
          (switch-to-buffer origin)
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process) "/dev/pts/8")))
            (test-vterm-tmux-history--with-tmux-mock
                '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
                   "/dev/pts/8\t%8\n")
                  (("capture-pane" "-p" "-J" "-S" "-" "-E" "-" "-t" "%8") 0
                   "history http://example.com\n"))
              (cj/vterm-tmux-history)
              (should (eq major-mode 'cj/vterm-tmux-history-mode))
              (should buffer-read-only)
              (should (string-match-p "history http://example.com"
                                      (buffer-string))))))
      (cj/test--kill-buffers-matching-prefix "*vterm tmux history")
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

(ert-deftest test-vterm-tmux-history-replaces-origin-buffer-in-same-window ()
  "Normal: the history view replaces the origin in the selected window.

Before the in-place change, `cj/vterm-tmux-history' used `pop-to-buffer'
which could split or hand the buffer to a different window.  The fix
uses `switch-to-buffer' so reading scrollback keeps the agent's frame
slot."
  (let ((origin (cj/test--make-fake-vterm-buffer "*test-vterm-history-inplace*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer origin)
          (let ((win (selected-window)))
            (should (eq (window-buffer win) origin))
            (should (one-window-p))
            (cl-letf (((symbol-function 'get-buffer-process)
                       (lambda (_buffer) 'fake-process))
                      ((symbol-function 'process-tty-name)
                       (lambda (_process) "/dev/pts/8")))
              (test-vterm-tmux-history--with-tmux-mock
                  '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
                     "/dev/pts/8\t%8\n")
                    (("capture-pane" "-p" "-J" "-S" "-" "-E" "-" "-t" "%8") 0
                     "scrollback line\n"))
                (cj/vterm-tmux-history)))
            ;; Same window, no split, history buffer now in the slot.
            (should (one-window-p))
            (should (eq (selected-window) win))
            (should (string-prefix-p
                     "*vterm tmux history:"
                     (buffer-name (window-buffer win))))))
      (cj/test--kill-buffers-matching-prefix "*vterm tmux history")
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

(ert-deftest test-vterm-tmux-history-quit-returns-to-origin ()
  "Normal: q / <escape> / C-g (cj/vterm-tmux-history-quit) kills the history
buffer and restores the origin buffer, window, and point."
  (let ((origin (get-buffer-create "*test-vterm-history-return*")))
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
              (cj/vterm-tmux-history-quit))
            (should-not (buffer-live-p history))
            (should (eq (current-buffer) origin))
            (should (= (point) (point-min)))))
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

(ert-deftest test-vterm-tmux-history-mode-keymap ()
  "Normal: in the history buffer M-w copies without quitting; q, <escape>,
and C-g quit back to the vterm; RET is left unbound (no special exit)."
  (should (eq (keymap-lookup cj/vterm-tmux-history-mode-map "M-w")
              #'kill-ring-save))
  (should (eq (keymap-lookup cj/vterm-tmux-history-mode-map "q")
              #'cj/vterm-tmux-history-quit))
  (should (eq (keymap-lookup cj/vterm-tmux-history-mode-map "<escape>")
              #'cj/vterm-tmux-history-quit))
  (should (eq (keymap-lookup cj/vterm-tmux-history-mode-map "C-g")
              #'cj/vterm-tmux-history-quit))
  (should-not (keymap-lookup cj/vterm-tmux-history-mode-map "RET")))

(ert-deftest test-vterm-keymap-includes-history-and-copy-bindings ()
  "Normal: personal vterm map owns the high-level vterm UX commands."
  (should (member "C-;" vterm-keymap-exceptions))
  (should-not (eq (keymap-lookup cj/custom-keymap "X c") #'vterm-copy-mode))
  (should (eq (keymap-lookup cj/custom-keymap "x h") #'cj/vterm-tmux-history))
  (should (eq (keymap-lookup cj/custom-keymap "x c") #'vterm-copy-mode))
  (should (equal (keymap-lookup vterm-mode-map "C-;") cj/custom-keymap))
  (should (eq (keymap-lookup vterm-mode-map "C-; x h") #'cj/vterm-tmux-history))
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

(ert-deftest test-vterm-copy-mode-keys ()
  "Normal: copy mode mirrors the history buffer -- M-w copies without
leaving; C-g, <escape>, and q leave without copying; RET is unbound."
  (should (eq (keymap-lookup vterm-copy-mode-map "M-w")
              #'kill-ring-save))
  (should (eq (keymap-lookup vterm-copy-mode-map "C-g")
              #'cj/vterm-copy-mode-cancel))
  (should (eq (keymap-lookup vterm-copy-mode-map "<escape>")
              #'cj/vterm-copy-mode-cancel))
  (should (eq (keymap-lookup vterm-copy-mode-map "q")
              #'cj/vterm-copy-mode-cancel))
  (should-not (keymap-lookup vterm-copy-mode-map "RET"))
  (should-not (keymap-lookup vterm-copy-mode-map "<return>")))

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
A buffer named like `agent [repo]' (ai-vterm.el's naming) is a
`vterm-mode' buffer and must inherit tmux history copy.  The pane lookup
keys off the live process TTY, never the buffer name -- so the
AI-vterm name neither helps nor blocks resolution."
  (let ((agent (cj/test--make-fake-vterm-buffer "agent [emacs.d]")))
    (unwind-protect
        (with-current-buffer agent
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process) "/dev/pts/8")))
            (test-vterm-tmux-history--with-tmux-mock
                '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
                   "/dev/pts/1\t%1\n/dev/pts/8\t%8\n"))
              (should (equal (cj/vterm--current-tmux-pane-id) "%8")))))
      (when (buffer-live-p agent)
        (kill-buffer agent)))))

(provide 'test-vterm-tmux-history)
;;; test-vterm-tmux-history.el ends here

;;; test-term-tmux-history.el --- Tests for the EAT terminal copy-mode + tmux history -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercises the terminal UX carried into eat-config for the EAT agent
;; terminals: the Emacs-owned tmux history buffer, the copy-mode-dwim engine
;; pick, the tmux pane-id / attached-client predicates, and the C-; x menu
;; bindings.  Agents run EAT over tmux, so copy-mode is tmux's own copy-mode.
;;
;; eat is required (so eat-config's `with-eval-after-load' fires for the C-<up>
;; bind) before eat-config; tmux is mocked via `process-file', so nothing spawns.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(setq load-prefer-newer t)
(require 'eat)
(require 'eat-config)
(require 'testutil-ghostel-buffers)

(defmacro test-term-tmux-history--with-tmux-mock (responses &rest body)
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

;;; tmux helpers

(ert-deftest test-term-tmux-history--pane-id-for-tty-matches-client ()
  "Normal: current terminal pty maps to the active pane for that tmux client."
  (test-term-tmux-history--with-tmux-mock
      '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
         "/dev/pts/1\t%1\n/dev/pts/8\t%8\n"))
    (should (equal (cj/term--tmux-pane-id-for-tty "/dev/pts/8") "%8"))))

(ert-deftest test-term-tmux-history--capture-pane-uses-full-history ()
  "Normal: capture asks tmux for joined full pane history."
  (test-term-tmux-history--with-tmux-mock
      '((("capture-pane" "-p" "-J" "-S" "-" "-E" "-" "-t" "%8") 0
         "first line\nsecond line\n"))
    (should (equal (cj/term--tmux-capture-pane "%8")
                   "first line\nsecond line\n"))))

(ert-deftest test-term-current-tmux-pane-id-rejects-non-eat-buffer ()
  "Error: pane-id lookup refuses a buffer that is not in `eat-mode'."
  (with-temp-buffer
    (should-error (cj/term--current-tmux-pane-id) :type 'user-error)))

(ert-deftest test-term-current-tmux-pane-id-accepts-agent-named-buffer ()
  "Normal: an agent-named eat buffer resolves by process TTY, not buffer name."
  (let ((agent (cj/test--make-fake-eat-buffer "agent [emacs.d]")))
    (unwind-protect
        (with-current-buffer agent
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process &rest _) "/dev/pts/8")))
            (test-term-tmux-history--with-tmux-mock
                '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
                   "/dev/pts/1\t%1\n/dev/pts/8\t%8\n"))
              (should (equal (cj/term--current-tmux-pane-id) "%8")))))
      (when (buffer-live-p agent)
        (kill-buffer agent)))))

;;; tmux history buffer

(ert-deftest test-term-tmux-history-open-renders-read-only-history-buffer ()
  "Normal: the command renders tmux history in a normal Emacs buffer."
  (let ((origin (cj/test--make-fake-eat-buffer "*test-term-history-origin*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer origin)
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process &rest _) "/dev/pts/8")))
            (test-term-tmux-history--with-tmux-mock
                '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
                   "/dev/pts/8\t%8\n")
                  (("capture-pane" "-p" "-J" "-S" "-" "-E" "-" "-t" "%8") 0
                   "history http://example.com\n"))
              (cj/term-tmux-history)
              (should (eq major-mode 'cj/term-tmux-history-mode))
              (should buffer-read-only)
              (should (string-match-p "history http://example.com"
                                      (buffer-string))))))
      (cj/test--kill-buffers-matching-prefix "*terminal tmux history")
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

(ert-deftest test-term-tmux-history-quit-returns-to-origin ()
  "Normal: quit kills the history buffer and restores origin buffer/window/point."
  (let ((origin (get-buffer-create "*test-term-history-return*")))
    (unwind-protect
        (let ((history (get-buffer-create "*terminal tmux history: test*")))
          (with-current-buffer origin
            (erase-buffer)
            (insert "origin")
            (goto-char (point-min)))
          (switch-to-buffer origin)
          (let ((origin-window (selected-window)))
            (with-current-buffer history
              (cj/term-tmux-history-mode)
              (let ((inhibit-read-only t))
                (insert "alpha\nbeta\ngamma\n"))
              (setq-local cj/term-tmux-history--origin-buffer origin)
              (setq-local cj/term-tmux-history--origin-window origin-window)
              (setq-local cj/term-tmux-history--origin-point (point-min))
              (cj/term-tmux-history-quit))
            (should-not (buffer-live-p history))
            (should (eq (current-buffer) origin))
            (should (= (point) (point-min)))))
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

(ert-deftest test-term-tmux-history-mode-keymap ()
  "Normal: M-w copies; q/<escape>/C-g quit; RET is left unbound."
  (should (eq (keymap-lookup cj/term-tmux-history-mode-map "M-w") #'kill-ring-save))
  (should (eq (keymap-lookup cj/term-tmux-history-mode-map "q")
              #'cj/term-tmux-history-quit))
  (should (eq (keymap-lookup cj/term-tmux-history-mode-map "<escape>")
              #'cj/term-tmux-history-quit))
  (should (eq (keymap-lookup cj/term-tmux-history-mode-map "C-g")
              #'cj/term-tmux-history-quit))
  (should-not (keymap-lookup cj/term-tmux-history-mode-map "RET")))

;;; in-tmux-p predicate

(ert-deftest test-term-in-tmux-p-true-when-client-attached ()
  "Normal: predicate returns t when tmux reports a client for our tty."
  (let ((agent (cj/test--make-fake-eat-buffer "agent [emacs.d]")))
    (unwind-protect
        (with-current-buffer agent
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process &rest _) "/dev/pts/8")))
            (test-term-tmux-history--with-tmux-mock
                '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
                   "/dev/pts/8\t%8\n"))
              (should (cj/term--in-tmux-p)))))
      (when (buffer-live-p agent)
        (kill-buffer agent)))))

(ert-deftest test-term-in-tmux-p-nil-when-not-eat-mode ()
  "Boundary: predicate refuses non-eat buffers without calling tmux."
  (with-temp-buffer
    (let ((tmux-called nil))
      (cl-letf (((symbol-function 'process-file)
                 (lambda (&rest _) (setq tmux-called t) 0)))
        (should-not (cj/term--in-tmux-p))
        (should-not tmux-called)))))

(ert-deftest test-term-in-tmux-p-nil-when-tmux-fails ()
  "Error: predicate swallows tmux failures and returns nil."
  (let ((agent (cj/test--make-fake-eat-buffer "agent [emacs.d]")))
    (unwind-protect
        (with-current-buffer agent
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process &rest _) "/dev/pts/8")))
            (test-term-tmux-history--with-tmux-mock
                '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 1
                   "no server running"))
              (should-not (cj/term--in-tmux-p)))))
      (when (buffer-live-p agent)
        (kill-buffer agent)))))

;;; copy-mode (tmux path -- the agent terminal case)

(ert-deftest test-term-copy-mode-dwim-sends-tmux-prefix-when-attached ()
  "Normal: with tmux attached, dwim writes C-b [ then C-a into the pty so tmux
enters copy-mode with the cursor at column 0."
  (let ((agent (cj/test--make-fake-eat-buffer "agent [emacs.d]"))
        (sent nil))
    (unwind-protect
        (with-current-buffer agent
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process &rest _) "/dev/pts/8"))
                    ((symbol-function 'cj/--term-send-string)
                     (lambda (s) (push s sent))))
            (test-term-tmux-history--with-tmux-mock
                '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
                   "/dev/pts/8\t%8\n"))
              (cj/term-copy-mode-dwim)
              (should (equal sent '("\C-b[\C-a"))))))
      (when (buffer-live-p agent)
        (kill-buffer agent)))))

(ert-deftest test-term-copy-mode-up-tmux-enters-then-scrolls-up ()
  "Normal: from a live (non-copy) tmux pane, C-<up> enters copy-mode then sends
the up-arrow, so one stroke both enters copy-mode and scrolls up."
  (let ((agent (cj/test--make-fake-eat-buffer "agent [emacs.d]"))
        (sent nil))
    (unwind-protect
        (with-current-buffer agent
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process &rest _) "/dev/pts/8"))
                    ((symbol-function 'cj/--term-send-string)
                     (lambda (s) (push s sent))))
            (test-term-tmux-history--with-tmux-mock
                '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
                   "/dev/pts/8\t%8\n")
                  (("display-message" "-p" "-t" "%8" "#{pane_in_mode}") 0 "0\n"))
              (cj/term-copy-mode-up)
              (should (equal (reverse sent) '("\C-b[\C-a" "\e[A"))))))
      (when (buffer-live-p agent)
        (kill-buffer agent)))))

(ert-deftest test-term-copy-mode-up-tmux-already-in-mode-just-scrolls ()
  "Normal: when the tmux pane is already in copy-mode, C-<up> only sends the
up-arrow -- it does not re-enter and reset the cursor."
  (let ((agent (cj/test--make-fake-eat-buffer "agent [emacs.d]"))
        (sent nil))
    (unwind-protect
        (with-current-buffer agent
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process &rest _) "/dev/pts/8"))
                    ((symbol-function 'cj/--term-send-string)
                     (lambda (s) (push s sent))))
            (test-term-tmux-history--with-tmux-mock
                '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
                   "/dev/pts/8\t%8\n")
                  (("display-message" "-p" "-t" "%8" "#{pane_in_mode}") 0 "1\n"))
              (cj/term-copy-mode-up)
              (should (equal (reverse sent) '("\e[A"))))))
      (when (buffer-live-p agent)
        (kill-buffer agent)))))

;;; bindings

(ert-deftest test-term-keymap-history-and-copy-bindings ()
  "Normal: the C-; x terminal map owns the tmux-history and copy-mode commands."
  (should (eq (keymap-lookup cj/custom-keymap "x h") #'cj/term-tmux-history))
  (should (eq (keymap-lookup cj/custom-keymap "x c") #'cj/term-copy-mode-dwim))
  (should (eq (keymap-lookup cj/custom-keymap "x t") #'cj/term-toggle)))

(ert-deftest test-term-copy-mode-up-bound-in-eat-semi-char-map ()
  "Normal: C-<up> enters copy-mode + scrolls up from inside an EAT terminal."
  (should (eq (keymap-lookup eat-semi-char-mode-map "C-<up>")
              #'cj/term-copy-mode-up)))

(provide 'test-term-tmux-history)
;;; test-term-tmux-history.el ends here

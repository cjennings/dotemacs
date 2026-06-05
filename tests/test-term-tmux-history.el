;;; test-term-tmux-history.el --- Tests for term-config tmux history + menu UX -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercises the term-config (ghostel) terminal UX: the Emacs-owned tmux
;; history buffer, the copy-mode-dwim engine pick, the tmux pane-id /
;; attached-client predicates, and the C-; x menu bindings.
;;
;; ghostel is required (which defines `ghostel-mode-map' /
;; `ghostel-keymap-exceptions' and lets term-config's `with-eval-after-load'
;; fire) before term-config.  `(require 'ghostel)' does not load the native
;; module; tmux is mocked via `process-file', so nothing spawns.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(setq load-prefer-newer t)
(require 'ghostel)
(require 'term-config)
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

(ert-deftest test-term-tmux-history-open-renders-read-only-history-buffer ()
  "Normal: command renders tmux history in a normal Emacs buffer."
  (let ((origin (cj/test--make-fake-ghostel-buffer "*test-term-history-origin*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer origin)
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process) "/dev/pts/8")))
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

(ert-deftest test-term-tmux-history-replaces-origin-buffer-in-same-window ()
  "Normal: the history view replaces the origin in the selected window.

`cj/term-tmux-history' uses `switch-to-buffer' so reading scrollback keeps
the terminal's frame slot rather than splitting or popping a new window."
  (let ((origin (cj/test--make-fake-ghostel-buffer "*test-term-history-inplace*")))
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
              (test-term-tmux-history--with-tmux-mock
                  '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
                     "/dev/pts/8\t%8\n")
                    (("capture-pane" "-p" "-J" "-S" "-" "-E" "-" "-t" "%8") 0
                     "scrollback line\n"))
                (cj/term-tmux-history)))
            (should (one-window-p))
            (should (eq (selected-window) win))
            (should (string-prefix-p
                     "*terminal tmux history:"
                     (buffer-name (window-buffer win))))))
      (cj/test--kill-buffers-matching-prefix "*terminal tmux history")
      (when (buffer-live-p origin)
        (kill-buffer origin)))))

(ert-deftest test-term-tmux-history-quit-returns-to-origin ()
  "Normal: q / <escape> / C-g (cj/term-tmux-history-quit) kills the history
buffer and restores the origin buffer, window, and point."
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
  "Normal: in the history buffer M-w copies without quitting; q, <escape>,
and C-g quit back to the terminal; RET is left unbound (no special exit)."
  (should (eq (keymap-lookup cj/term-tmux-history-mode-map "M-w")
              #'kill-ring-save))
  (should (eq (keymap-lookup cj/term-tmux-history-mode-map "q")
              #'cj/term-tmux-history-quit))
  (should (eq (keymap-lookup cj/term-tmux-history-mode-map "<escape>")
              #'cj/term-tmux-history-quit))
  (should (eq (keymap-lookup cj/term-tmux-history-mode-map "C-g")
              #'cj/term-tmux-history-quit))
  (should-not (keymap-lookup cj/term-tmux-history-mode-map "RET")))

(ert-deftest test-term-keymap-includes-history-and-copy-bindings ()
  "Normal: the personal terminal map owns the high-level UX commands, and C-;
reaches Emacs inside ghostel buffers so the prefix works there."
  (should (member "C-;" ghostel-keymap-exceptions))
  (should (eq (keymap-lookup cj/custom-keymap "x h") #'cj/term-tmux-history))
  (should (eq (keymap-lookup cj/custom-keymap "x c") #'cj/term-copy-mode-dwim))
  (should (equal (keymap-lookup ghostel-mode-map "C-;") cj/custom-keymap))
  (should (eq (keymap-lookup ghostel-mode-map "C-; x h") #'cj/term-tmux-history))
  (should (eq (keymap-lookup ghostel-mode-map "C-; x c") #'cj/term-copy-mode-dwim)))

(ert-deftest test-term-keymap-prompt-navigation ()
  "Normal: n/p navigate prompts, capital N creates a new terminal buffer."
  (should (eq (keymap-lookup cj/custom-keymap "x n") #'ghostel-next-prompt))
  (should (eq (keymap-lookup cj/custom-keymap "x p") #'ghostel-previous-prompt))
  (should (eq (keymap-lookup cj/custom-keymap "x N") #'ghostel)))

(ert-deftest test-term-current-tmux-pane-id-rejects-non-ghostel-buffer ()
  "Error: pane-id lookup refuses a buffer that is not in `ghostel-mode'."
  (with-temp-buffer
    (should-error (cj/term--current-tmux-pane-id) :type 'user-error)))

(ert-deftest test-term-current-tmux-pane-id-accepts-agent-named-buffer ()
  "Normal: an agent-named ghostel buffer resolves by process TTY.

The pane lookup keys off the live process TTY, never the buffer name, so a
buffer named `agent [repo]' (ai-term.el's naming) resolves like any other
ghostel-mode terminal."
  (let ((agent (cj/test--make-fake-ghostel-buffer "agent [emacs.d]")))
    (unwind-protect
        (with-current-buffer agent
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process) "/dev/pts/8")))
            (test-term-tmux-history--with-tmux-mock
                '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
                   "/dev/pts/1\t%1\n/dev/pts/8\t%8\n"))
              (should (equal (cj/term--current-tmux-pane-id) "%8")))))
      (when (buffer-live-p agent)
        (kill-buffer agent)))))

(ert-deftest test-term-in-tmux-p-true-when-client-attached ()
  "Normal: predicate returns t when tmux reports a client for our tty."
  (let ((agent (cj/test--make-fake-ghostel-buffer "agent [emacs.d]")))
    (unwind-protect
        (with-current-buffer agent
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process) "/dev/pts/8")))
            (test-term-tmux-history--with-tmux-mock
                '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
                   "/dev/pts/8\t%8\n"))
              (should (cj/term--in-tmux-p)))))
      (when (buffer-live-p agent)
        (kill-buffer agent)))))

(ert-deftest test-term-in-tmux-p-nil-when-no-matching-client ()
  "Boundary: predicate returns nil when tmux runs but our tty has no client."
  (let ((agent (cj/test--make-fake-ghostel-buffer "agent [emacs.d]")))
    (unwind-protect
        (with-current-buffer agent
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process) "/dev/pts/8")))
            (test-term-tmux-history--with-tmux-mock
                '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
                   "/dev/pts/1\t%1\n"))
              (should-not (cj/term--in-tmux-p)))))
      (when (buffer-live-p agent)
        (kill-buffer agent)))))

(ert-deftest test-term-in-tmux-p-nil-when-tmux-fails ()
  "Error: predicate swallows tmux failures and returns nil."
  (let ((agent (cj/test--make-fake-ghostel-buffer "agent [emacs.d]")))
    (unwind-protect
        (with-current-buffer agent
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process) "/dev/pts/8")))
            (test-term-tmux-history--with-tmux-mock
                '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 1
                   "no server running"))
              (should-not (cj/term--in-tmux-p)))))
      (when (buffer-live-p agent)
        (kill-buffer agent)))))

(ert-deftest test-term-in-tmux-p-nil-when-not-ghostel-mode ()
  "Boundary: predicate refuses non-ghostel buffers without calling tmux."
  (with-temp-buffer
    (let ((tmux-called nil))
      (cl-letf (((symbol-function 'process-file)
                 (lambda (&rest _) (setq tmux-called t) 0)))
        (should-not (cj/term--in-tmux-p))
        (should-not tmux-called)))))

(ert-deftest test-term-copy-mode-dwim-sends-tmux-prefix-when-attached ()
  "Normal: with tmux attached, dwim writes C-b [ into the pty so tmux enters
its own copy-mode against the full pane history."
  (let ((agent (cj/test--make-fake-ghostel-buffer "agent [emacs.d]"))
        (sent nil)
        (copy-mode-called nil))
    (unwind-protect
        (with-current-buffer agent
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process) "/dev/pts/8"))
                    ((symbol-function 'ghostel-send-string)
                     (lambda (s) (push s sent)))
                    ((symbol-function 'ghostel-copy-mode)
                     (lambda () (setq copy-mode-called t))))
            (test-term-tmux-history--with-tmux-mock
                '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 0
                   "/dev/pts/8\t%8\n"))
              (cj/term-copy-mode-dwim)
              (should (equal sent '("\C-b[")))
              (should-not copy-mode-called))))
      (when (buffer-live-p agent)
        (kill-buffer agent)))))

(ert-deftest test-term-copy-mode-dwim-falls-back-without-tmux ()
  "Boundary: without tmux, dwim calls `ghostel-copy-mode' and sends nothing."
  (let ((agent (cj/test--make-fake-ghostel-buffer "agent [emacs.d]"))
        (sent nil)
        (copy-mode-called nil))
    (unwind-protect
        (with-current-buffer agent
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (_buffer) 'fake-process))
                    ((symbol-function 'process-tty-name)
                     (lambda (_process) "/dev/pts/8"))
                    ((symbol-function 'ghostel-send-string)
                     (lambda (s) (push s sent)))
                    ((symbol-function 'ghostel-copy-mode)
                     (lambda () (setq copy-mode-called t))))
            (test-term-tmux-history--with-tmux-mock
                '((("list-clients" "-F" "#{client_tty}\t#{pane_id}") 1
                   "no server running"))
              (cj/term-copy-mode-dwim)
              (should-not sent)
              (should copy-mode-called))))
      (when (buffer-live-p agent)
        (kill-buffer agent)))))

(ert-deftest test-term-prefix-and-f12-in-keymap-exceptions ()
  "Regression: C-; and F12 are in `ghostel-keymap-exceptions' and the rebuilt
semi-char map no longer forwards them to the pty, so the prefix keymap and the
F12 toggle reach Emacs inside ghostel buffers."
  (dolist (key '("C-;" "<f12>"))
    (should (member key ghostel-keymap-exceptions)))
  (should-not (eq (keymap-lookup ghostel-semi-char-mode-map "<f12>")
                  'ghostel--send-event)))

(ert-deftest test-term-window-nav-keys-in-keymap-exceptions ()
  "Regression: windmove (S-arrows) and buffer-move (C-M-arrows) are in
`ghostel-keymap-exceptions' so they reach Emacs from inside a ghostel buffer
instead of being forwarded to the terminal program."
  (dolist (key '("S-<up>" "S-<down>" "S-<left>" "S-<right>"
                 "C-M-<up>" "C-M-<down>" "C-M-<left>" "C-M-<right>"))
    (should (member key ghostel-keymap-exceptions)))
  (should-not (eq (keymap-lookup ghostel-semi-char-mode-map "C-M-<left>")
                  'ghostel--send-event)))

(ert-deftest test-term-c-spc-forwarded-not-set-mark ()
  "Regression: C-SPC is forwarded to the terminal, not bound to the global
`set-mark-command'.  ghostel only forwards the `C-@' event, so without this an
Emacs region gets stuck in the ghostel buffer and tmux copy-mode's
begin-selection never starts."
  (should (eq (keymap-lookup ghostel-mode-map "C-SPC") #'cj/term-send-C-SPC)))

(provide 'test-term-tmux-history)
;;; test-term-tmux-history.el ends here

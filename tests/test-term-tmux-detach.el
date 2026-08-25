;;; test-term-tmux-detach.el --- Tests for cj/term-tmux-detach -*- lexical-binding: t; -*-

;;; Commentary:
;; A keyboard C-b inside the Claude Code pane does not reach tmux as a prefix
;; (it lands as stray text), so detaching needs the same pty string path
;; `cj/term-copy-mode-dwim' uses for C-b [.  These tests pin that path and
;; the no-tmux fallback.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)

;; Same shape as test-term-tmux-history.el: `make test' runs with no
;; package-initialize, so eat has to be made loadable here before eat-config.
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(setq load-prefer-newer t)
(require 'eat)
(require 'eat-config)

(ert-deftest test-eat-config-tmux-detach-sends-prefix-and-d-when-attached ()
  "Normal: with tmux attached, the command writes C-b d into the pty, nothing else."
  (let ((sent nil))
    (cl-letf (((symbol-function 'cj/term--in-tmux-p) (lambda () t))
              ((symbol-function 'cj/--term-send-string) (lambda (s) (push s sent))))
      (cj/term-tmux-detach)
      (should (equal sent '("\C-bd"))))))

(ert-deftest test-eat-config-tmux-detach-does-nothing-without-tmux ()
  "Boundary: with no tmux client, nothing is written and the user is told why.
Writing C-b d into a plain shell would type a control character into it."
  (let ((sent nil)
        (told nil))
    (cl-letf (((symbol-function 'cj/term--in-tmux-p) (lambda () nil))
              ((symbol-function 'cj/--term-send-string) (lambda (s) (push s sent)))
              ((symbol-function 'message) (lambda (fmt &rest args)
                                            (setq told (apply #'format fmt args)))))
      (cj/term-tmux-detach)
      (should-not sent)
      (should (string-match-p "tmux" told)))))

(ert-deftest test-eat-config-tmux-detach-survives-dead-process ()
  "Error: with tmux reported attached but no live pty, the command returns
without signalling.  `cj/--term-send-string' already guards on
`process-live-p'; this pins that the detach path relies on it rather than
calling `process-send-string' directly."
  (with-temp-buffer
    (cl-letf (((symbol-function 'cj/term--in-tmux-p) (lambda () t)))
      (should-not (condition-case err
                      (progn (cj/term-tmux-detach) nil)
                    (error err))))))

(ert-deftest test-eat-config-tmux-detach-bound-on-term-map ()
  "Normal: the command sits on the terminal map next to copy-mode (\"c\")."
  (should (eq (keymap-lookup cj/term-map "d") #'cj/term-tmux-detach)))

(provide 'test-term-tmux-detach)
;;; test-term-tmux-detach.el ends here

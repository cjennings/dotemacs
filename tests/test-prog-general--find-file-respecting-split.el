;;; test-prog-general--find-file-respecting-split.el --- split-aware file open -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/--find-file-respecting-split' opens a file in another window when the
;; frame is split and in the current window otherwise.  It backs
;; `cj/open-project-root-todo' (C-c p t) so the project todo lands in the
;; other pane instead of replacing the buffer in the selected window.
;;
;; `find-file' / `find-file-other-window' are stubbed -- they are the file-I/O
;; boundary -- while the window layout is real.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'prog-general)

(ert-deftest test-prog-general--find-file-respecting-split-single-window-uses-current ()
  "Normal: a single-window frame opens the file in the current window."
  (save-window-excursion
    (delete-other-windows)
    (let (current-arg other-called)
      (cl-letf (((symbol-function 'find-file)
                 (lambda (f &rest _) (setq current-arg f)))
                ((symbol-function 'find-file-other-window)
                 (lambda (_f &rest _) (setq other-called t))))
        (cj/--find-file-respecting-split "/tmp/proj/todo.org"))
      (should (equal current-arg "/tmp/proj/todo.org"))
      (should-not other-called))))

(ert-deftest test-prog-general--find-file-respecting-split-two-windows-uses-other ()
  "Normal: a two-window split opens the file in the other window."
  (save-window-excursion
    (delete-other-windows)
    (split-window-right)
    (let (other-arg current-called)
      (cl-letf (((symbol-function 'find-file-other-window)
                 (lambda (f &rest _) (setq other-arg f)))
                ((symbol-function 'find-file)
                 (lambda (_f &rest _) (setq current-called t))))
        (cj/--find-file-respecting-split "/tmp/proj/todo.org"))
      (should (equal other-arg "/tmp/proj/todo.org"))
      (should-not current-called))))

(ert-deftest test-prog-general--find-file-respecting-split-three-windows-uses-other ()
  "Boundary: more than two windows still counts as split -> other window."
  (save-window-excursion
    (delete-other-windows)
    (split-window-right)
    (split-window-below)
    (let (other-called current-called)
      (cl-letf (((symbol-function 'find-file-other-window)
                 (lambda (_f &rest _) (setq other-called t)))
                ((symbol-function 'find-file)
                 (lambda (_f &rest _) (setq current-called t))))
        (cj/--find-file-respecting-split "/tmp/proj/todo.org"))
      (should other-called)
      (should-not current-called))))

(provide 'test-prog-general--find-file-respecting-split)
;;; test-prog-general--find-file-respecting-split.el ends here

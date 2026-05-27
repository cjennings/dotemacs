;;; test-prog-general-open-project-daily-prep.el --- daily-prep opener -*- lexical-binding: t; -*-

;;; Commentary:
;; `cj/open-project-daily-prep' (C-c p d) opens inbox/today-prep.org under the
;; current Projectile project root, via `cj/--find-file-respecting-split' so it
;; lands in the other window when the frame is split and reuses the current
;; window otherwise (matching `cj/open-project-root-todo').  It is
;; project-scoped: projects without a prep file get a message instead.
;; `projectile-project-root' (external project state) and
;; `cj/--find-file-respecting-split' (the window/visit boundary) are stubbed;
;; real temp directories drive the file-exists check, and the message branches
;; are checked via the command's return value rather than by stubbing `message'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'prog-general)

(ert-deftest test-prog-general-daily-prep-opens-existing-respecting-split ()
  "Normal: in a project with a prep file, open it respecting the split."
  (let* ((root (file-name-as-directory (make-temp-file "cj-prep-" t)))
         (prep (expand-file-name "inbox/today-prep.org" root))
         opened)
    (unwind-protect
        (progn
          (make-directory (expand-file-name "inbox" root) t)
          (write-region "" nil prep)
          (cl-letf (((symbol-function 'projectile-project-root) (lambda () root))
                    ((symbol-function 'cj/--find-file-respecting-split)
                     (lambda (f) (setq opened f))))
            (cj/open-project-daily-prep))
          (should (equal opened prep)))
      (delete-directory root t))))

(ert-deftest test-prog-general-daily-prep-missing-file-messages ()
  "Boundary: in a project without a prep file, do not open; report it."
  (let* ((root (file-name-as-directory (make-temp-file "cj-prep-" t)))
         opened result)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'projectile-project-root) (lambda () root))
                    ((symbol-function 'find-file-other-window) (lambda (f) (setq opened f))))
            (setq result (cj/open-project-daily-prep)))
          (should-not opened)
          (should (string-match-p "No inbox/today-prep.org" result)))
      (delete-directory root t))))

(ert-deftest test-prog-general-daily-prep-not-in-project-messages ()
  "Error: outside a Projectile project, do not open; report it."
  (let (opened result)
    (cl-letf (((symbol-function 'projectile-project-root) (lambda () nil))
              ((symbol-function 'find-file-other-window) (lambda (f) (setq opened f))))
      (setq result (cj/open-project-daily-prep)))
    (should-not opened)
    (should (string-match-p "Not in a Projectile project" result))))

(provide 'test-prog-general-open-project-daily-prep)
;;; test-prog-general-open-project-daily-prep.el ends here

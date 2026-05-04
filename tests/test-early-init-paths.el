;;; test-early-init-paths.el --- Tests for early-init path construction -*- lexical-binding: t; -*-

;;; Commentary:

;; Load early-init.el with package side effects stubbed so path construction can
;; be tested without touching real package archives or the network.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'package)

(defvar cj/use-online-repos nil
  "Test binding for early-init online repository setup.")

(defconst test-early-init-paths--repo-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root for early-init path tests.")

(defun test-early-init-paths--load-with-temp-home (home emacs-home)
  "Load early-init.el with HOME and EMACS-HOME as isolated roots."
  (let ((process-environment (copy-sequence process-environment))
        (user-emacs-directory (file-name-as-directory emacs-home))
        (package-user-dir (expand-file-name "elpa/" emacs-home))
        (package-archives nil)
        (package-archive-priorities nil)
        (debug-on-error nil)
        (debug-on-quit nil))
    (setq cj/use-online-repos nil)
    (setenv "HOME" home)
    (cl-letf (((symbol-function 'package-initialize) #'ignore)
              ((symbol-function 'package-installed-p) (lambda (_package) t))
              ((symbol-function 'package-refresh-contents) #'ignore)
              ((symbol-function 'package-install) #'ignore))
      (load (expand-file-name "early-init.el" test-early-init-paths--repo-root) nil t))
    (list :user-home-dir user-home-dir
          :elpa-mirror-location elpa-mirror-location
          :localrepo-location localrepo-location
          :gnu elpa-mirror-gnu-location
          :nongnu elpa-mirror-nongnu-location
          :melpa elpa-mirror-melpa-location
          :stable-melpa elpa-mirror-stable-melpa-location
          :package-archives package-archives
          :package-archive-priorities package-archive-priorities)))

(ert-deftest test-early-init-paths-normal-expanded-under-home ()
  "Normal: local mirror paths expand under HOME, not beside it."
  (let* ((home (make-temp-file "early-init-home-" t))
         (emacs-home (make-temp-file "early-init-emacs-" t))
         (_ (dolist (dir '(".localrepo"
                           ".elpa-mirrors/gnu"
                           ".elpa-mirrors/nongnu"
                           ".elpa-mirrors/melpa"
                           ".elpa-mirrors/stable-melpa"))
              (make-directory (expand-file-name dir emacs-home) t)))
         ;; Mirror directories intentionally live under HOME, while .localrepo
         ;; lives under user-emacs-directory.
         (_ (dolist (dir '(".elpa-mirrors/gnu"
                           ".elpa-mirrors/nongnu"
                           ".elpa-mirrors/melpa"
                           ".elpa-mirrors/stable-melpa"))
              (make-directory (expand-file-name dir home) t)))
         (result (test-early-init-paths--load-with-temp-home home emacs-home)))
    (should (string= (plist-get result :user-home-dir) home))
    (should (string= (plist-get result :elpa-mirror-location)
                     (expand-file-name ".elpa-mirrors/" home)))
    (should (string= (plist-get result :localrepo-location)
                     (expand-file-name ".localrepo/" emacs-home)))
    (should (string= (plist-get result :gnu)
                     (expand-file-name ".elpa-mirrors/gnu/" home)))
    (should-not (string= (plist-get result :elpa-mirror-location)
                         (concat home ".elpa-mirrors/")))))

(ert-deftest test-early-init-paths-normal-local-archives-use-expanded-paths ()
  "Normal: local package archives use expanded path constants."
  (let* ((home (make-temp-file "early-init-home-" t))
         (emacs-home (make-temp-file "early-init-emacs-" t)))
    (make-directory (expand-file-name ".localrepo/" emacs-home) t)
    (dolist (dir '(".elpa-mirrors/gnu"
                   ".elpa-mirrors/nongnu"
                   ".elpa-mirrors/melpa"
                   ".elpa-mirrors/stable-melpa"))
      (make-directory (expand-file-name dir home) t))
    (let* ((result (test-early-init-paths--load-with-temp-home home emacs-home))
           (archives (plist-get result :package-archives)))
      (should (equal (cdr (assoc "localrepo" archives))
                     (expand-file-name ".localrepo/" emacs-home)))
      (should (equal (cdr (assoc "gnu-local" archives))
                     (expand-file-name ".elpa-mirrors/gnu/" home)))
      (should (equal (cdr (assoc "nongnu-local" archives))
                     (expand-file-name ".elpa-mirrors/nongnu/" home)))
      (should (equal (cdr (assoc "melpa-local" archives))
                     (expand-file-name ".elpa-mirrors/melpa/" home)))
      (should (equal (cdr (assoc "melpa-stable-local" archives))
                     (expand-file-name ".elpa-mirrors/stable-melpa/" home))))))

(provide 'test-early-init-paths)
;;; test-early-init-paths.el ends here

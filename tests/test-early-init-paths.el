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

(cl-defun test-early-init-paths--load-with-temp-home
    (home emacs-home &key online package-installed-p package-archive-contents)
  "Load early-init.el with HOME and EMACS-HOME as isolated roots.
When ONLINE is non-nil, allow online archive construction.  When
PACKAGE-INSTALLED-P is nil, simulate a missing `use-package'."
  (let ((process-environment (copy-sequence process-environment))
        (user-emacs-directory (file-name-as-directory emacs-home))
        (package-user-dir (expand-file-name "elpa/" emacs-home))
        (package-archives nil)
        (package-archive-priorities nil)
        (package-archive-contents package-archive-contents)
        (debug-on-error nil)
        (debug-on-quit nil)
        (package-initialize-count 0)
        (package-refresh-count 0)
        (package-install-count 0))
    (setq cj/use-online-repos online)
    (setenv "HOME" home)
    (cl-letf (((symbol-function 'package-initialize)
               (lambda () (setq package-initialize-count
                                 (1+ package-initialize-count))))
              ((symbol-function 'package-installed-p)
               (lambda (_package) package-installed-p))
              ((symbol-function 'package-refresh-contents)
               (lambda () (setq package-refresh-count
                                 (1+ package-refresh-count))))
              ((symbol-function 'package-install)
               (lambda (_package) (setq package-install-count
                                         (1+ package-install-count)))))
      (load (expand-file-name "early-init.el" test-early-init-paths--repo-root) nil t))
    (list :user-home-dir user-home-dir
          :elpa-mirror-location elpa-mirror-location
          :localrepo-location localrepo-location
          :gnu elpa-mirror-gnu-location
          :nongnu elpa-mirror-nongnu-location
          :melpa elpa-mirror-melpa-location
          :stable-melpa elpa-mirror-stable-melpa-location
          :package-archives package-archives
          :package-archive-priorities package-archive-priorities
          :package-initialize-count package-initialize-count
          :package-refresh-count package-refresh-count
          :package-install-count package-install-count)))

(defun test-early-init-paths--make-online-cache-files (package-user-dir)
  "Create fresh online archive cache files under PACKAGE-USER-DIR."
  (dolist (archive '("gnu" "nongnu" "melpa" "melpa-stable"))
    (let ((cache-file (expand-file-name
                       (format "archives/%s/archive-contents" archive)
                       package-user-dir)))
      (make-directory (file-name-directory cache-file) t)
      (write-region "()" nil cache-file nil 'silent))))

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
         (result (test-early-init-paths--load-with-temp-home
                  home emacs-home :package-installed-p t)))
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
    (let* ((result (test-early-init-paths--load-with-temp-home
                    home emacs-home :package-installed-p t))
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

(ert-deftest test-early-init-paths-normal-local-archives-only-when-directories-exist ()
  "Normal: local archives are added only for existing local directories."
  (let* ((home (make-temp-file "early-init-home-" t))
         (emacs-home (make-temp-file "early-init-emacs-" t)))
    (make-directory (expand-file-name ".localrepo/" emacs-home) t)
    (make-directory (expand-file-name ".elpa-mirrors/gnu/" home) t)
    (let* ((result (test-early-init-paths--load-with-temp-home
                    home emacs-home :package-installed-p t))
           (archives (plist-get result :package-archives)))
      (should (assoc "localrepo" archives))
      (should (assoc "gnu-local" archives))
      (should-not (assoc "nongnu-local" archives))
      (should-not (assoc "melpa-local" archives))
      (should-not (assoc "melpa-stable-local" archives)))))

(ert-deftest test-early-init-paths-normal-online-disabled-omits-online-archives ()
  "Normal: `cj/use-online-repos' nil omits online archives and refreshes."
  (let* ((home (make-temp-file "early-init-home-" t))
         (emacs-home (make-temp-file "early-init-emacs-" t))
         (result (test-early-init-paths--load-with-temp-home
                  home emacs-home
                  :online nil
                  :package-installed-p t))
         (archives (plist-get result :package-archives)))
    (should-not (assoc "gnu" archives))
    (should-not (assoc "nongnu" archives))
    (should-not (assoc "melpa" archives))
    (should-not (assoc "melpa-stable" archives))
    (should (= 0 (plist-get result :package-refresh-count)))
    (should (= 0 (plist-get result :package-install-count)))))

(ert-deftest test-early-init-paths-normal-local-priorities-exceed-online-priorities ()
  "Normal: local archive priorities are higher than online priorities."
  (let* ((home (make-temp-file "early-init-home-" t))
         (emacs-home (make-temp-file "early-init-emacs-" t))
         (package-user-dir (expand-file-name "elpa/" emacs-home)))
    (make-directory (expand-file-name ".localrepo/" emacs-home) t)
    (dolist (dir '(".elpa-mirrors/gnu"
                   ".elpa-mirrors/nongnu"
                   ".elpa-mirrors/melpa"
                   ".elpa-mirrors/stable-melpa"))
      (make-directory (expand-file-name dir home) t))
    (test-early-init-paths--make-online-cache-files package-user-dir)
    (let* ((result (test-early-init-paths--load-with-temp-home
                    home emacs-home
                    :online t
                    :package-installed-p t))
           (priorities (plist-get result :package-archive-priorities)))
      (dolist (pair '(("localrepo" . "gnu")
                      ("gnu-local" . "gnu")
                      ("nongnu-local" . "nongnu")
                      ("melpa-local" . "melpa")
                      ("melpa-stable-local" . "melpa-stable")))
        (should (> (cdr (assoc (car pair) priorities))
                   (cdr (assoc (cdr pair) priorities)))))
      (should (= 0 (plist-get result :package-refresh-count))))))

(ert-deftest test-early-init-paths-normal-online-fresh-caches-do-not-refresh ()
  "Normal: online archives with fresh caches do not refresh package contents."
  (let* ((home (make-temp-file "early-init-home-" t))
         (emacs-home (make-temp-file "early-init-emacs-" t)))
    (test-early-init-paths--make-online-cache-files
     (expand-file-name "elpa/" emacs-home))
    (let ((result (test-early-init-paths--load-with-temp-home
                   home emacs-home
                   :online t
                   :package-installed-p t)))
      (should (assoc "gnu" (plist-get result :package-archives)))
      (should (= 0 (plist-get result :package-refresh-count))))))

(ert-deftest test-early-init-paths-normal-online-missing-cache-refreshes-once ()
  "Normal: missing online archive cache requests one refresh."
  (let* ((home (make-temp-file "early-init-home-" t))
         (emacs-home (make-temp-file "early-init-emacs-" t))
         (result (test-early-init-paths--load-with-temp-home
                  home emacs-home
                  :online t
                  :package-installed-p t)))
    (should (assoc "gnu" (plist-get result :package-archives)))
    (should (= 1 (plist-get result :package-refresh-count)))
    (should (= 0 (plist-get result :package-install-count)))))

(provide 'test-early-init-paths)
;;; test-early-init-paths.el ends here

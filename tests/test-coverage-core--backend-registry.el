;;; test-coverage-core--backend-registry.el --- Tests for coverage backend registry -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the backend registry.
;;
;; A backend is a plist with at least :name, :detect, :run, and :lcov-path
;; keys.  `cj/coverage-register-backend' adds or replaces an entry.
;; `cj/--coverage-backend-for-project' resolves which backend applies to
;; a project root, honoring an optional override (buffer-local
;; `cj/coverage-backend' from .dir-locals.el in real use).

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'coverage-core)

(defmacro test-coverage-registry-with-empty (&rest body)
  "Run BODY with `cj/coverage-backends' rebound to an empty list."
  (declare (indent 0))
  `(let ((cj/coverage-backends nil))
	 ,@body))

;;; Normal cases

(ert-deftest test-coverage-backend-register-adds-entry ()
  "Normal: registering a backend makes it retrievable by name."
  (test-coverage-registry-with-empty
	(cj/coverage-register-backend
	 '(:name elisp :detect (lambda (_) t) :run ignore :lcov-path ignore))
	(should (= 1 (length cj/coverage-backends)))
	(should (eq 'elisp (plist-get (car cj/coverage-backends) :name)))))

(ert-deftest test-coverage-backend-register-replaces-in-place ()
  "Normal: re-registering by name replaces the existing entry at the same position."
  (test-coverage-registry-with-empty
	(cj/coverage-register-backend
	 '(:name elisp :detect (lambda (_) nil) :run ignore :lcov-path ignore))
	(cj/coverage-register-backend
	 '(:name python :detect (lambda (_) nil) :run ignore :lcov-path ignore))
	(cj/coverage-register-backend
	 '(:name elisp :detect (lambda (_) t) :run ignore :lcov-path ignore))
	(should (= 2 (length cj/coverage-backends)))
	(should (eq 'elisp (plist-get (nth 0 cj/coverage-backends) :name)))
	(should (eq 'python (plist-get (nth 1 cj/coverage-backends) :name)))
	(should (funcall (plist-get (nth 0 cj/coverage-backends) :detect) "/tmp"))))

(ert-deftest test-coverage-backend-for-project-first-detect-wins ()
  "Normal: resolution returns the first backend whose :detect matches."
  (test-coverage-registry-with-empty
	(cj/coverage-register-backend
	 '(:name a :detect (lambda (_) nil) :run ignore :lcov-path ignore))
	(cj/coverage-register-backend
	 '(:name b :detect (lambda (_) t)   :run ignore :lcov-path ignore))
	(cj/coverage-register-backend
	 '(:name c :detect (lambda (_) t)   :run ignore :lcov-path ignore))
	(let ((backend (cj/--coverage-backend-for-project "/tmp")))
	  (should (eq 'b (plist-get backend :name))))))

;;; Boundary cases

(ert-deftest test-coverage-backend-for-project-empty-registry ()
  "Boundary: empty registry returns nil, not an error."
  (test-coverage-registry-with-empty
	(should (null (cj/--coverage-backend-for-project "/tmp")))))

(ert-deftest test-coverage-backend-for-project-no-match ()
  "Boundary: no backend's :detect matches returns nil."
  (test-coverage-registry-with-empty
	(cj/coverage-register-backend
	 '(:name a :detect (lambda (_) nil) :run ignore :lcov-path ignore))
	(cj/coverage-register-backend
	 '(:name b :detect (lambda (_) nil) :run ignore :lcov-path ignore))
	(should (null (cj/--coverage-backend-for-project "/tmp")))))

(ert-deftest test-coverage-backend-for-project-override-bypasses-detect ()
  "Boundary: OVERRIDE returns the named backend without calling :detect."
  (test-coverage-registry-with-empty
	(cj/coverage-register-backend
	 '(:name a :detect (lambda (_) nil) :run ignore :lcov-path ignore))
	(cj/coverage-register-backend
	 '(:name b :detect (lambda (_) nil) :run ignore :lcov-path ignore))
	(let ((backend (cj/--coverage-backend-for-project "/tmp" 'b)))
	  (should (eq 'b (plist-get backend :name))))))

(ert-deftest test-coverage-backend-for-project-detect-receives-root ()
  "Boundary: the :detect function is called with the project root."
  (test-coverage-registry-with-empty
	(let ((captured nil))
	  (cj/coverage-register-backend
	   `(:name a
			   :detect ,(lambda (root) (setq captured root) t)
			   :run ignore :lcov-path ignore))
	  (cj/--coverage-backend-for-project "/my/root")
	  (should (equal "/my/root" captured)))))

;;; Error cases

(ert-deftest test-coverage-backend-for-project-override-unknown-errors ()
  "Error: OVERRIDE that names an unregistered backend signals user-error."
  (test-coverage-registry-with-empty
	(cj/coverage-register-backend
	 '(:name a :detect (lambda (_) t) :run ignore :lcov-path ignore))
	(should-error (cj/--coverage-backend-for-project "/tmp" 'bogus)
				  :type 'user-error)))

(provide 'test-coverage-core--backend-registry)
;;; test-coverage-core--backend-registry.el ends here

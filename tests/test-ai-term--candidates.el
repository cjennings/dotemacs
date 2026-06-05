;;; test-ai-term--candidates.el --- Tests for cj/--ai-term-candidates -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the project-candidate walker.  Two kinds of search root:
;;
;; - project root  (a single project dir, e.g. ~/.emacs.d) -- include if it
;;   itself contains .ai/protocols.org
;; - container root (e.g. ~/code, ~/projects) -- scan immediate children;
;;   include each child that contains .ai/protocols.org
;;
;; Tests build a temp directory tree with fake .ai/protocols.org markers
;; and let-bind the search-root customs at it.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(defun test-ai-term--make-marker (dir)
  "Create DIR/.ai/protocols.org so DIR registers as an AI-agent project."
  (let ((ai-dir (expand-file-name ".ai" dir)))
    (make-directory ai-dir t)
    (write-region "" nil (expand-file-name "protocols.org" ai-dir))))

(defmacro test-ai-term--with-fixture (root &rest body)
  "Bind ROOT to a fresh temp directory; remove on exit; run BODY."
  (declare (indent 1) (debug t))
  `(let ((,root (make-temp-file "ai-term-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,root t))))

(ert-deftest test-ai-term--candidates-project-root-with-marker ()
  "Normal: a project root containing .ai/protocols.org is included."
  (test-ai-term--with-fixture root
    (let ((proj (expand-file-name "emacs-d-fake" root)))
      (make-directory proj)
      (test-ai-term--make-marker proj)
      (let ((cj/ai-term-project-roots (list proj))
            (cj/ai-term-container-roots nil))
        (should (equal (cj/--ai-term-candidates)
                       (list (expand-file-name proj))))))))

(ert-deftest test-ai-term--candidates-project-root-without-marker ()
  "Boundary: a project root without .ai/protocols.org is excluded."
  (test-ai-term--with-fixture root
    (let ((proj (expand-file-name "no-ai" root)))
      (make-directory proj)
      (let ((cj/ai-term-project-roots (list proj))
            (cj/ai-term-container-roots nil))
        (should (null (cj/--ai-term-candidates)))))))

(ert-deftest test-ai-term--candidates-container-includes-children-with-marker ()
  "Normal: a container's children with .ai/protocols.org are included."
  (test-ai-term--with-fixture root
    (let ((container (expand-file-name "code" root))
          (foo (expand-file-name "code/foo" root))
          (bar (expand-file-name "code/bar" root)))
      (make-directory container)
      (make-directory foo)
      (make-directory bar)
      (test-ai-term--make-marker foo)
      (test-ai-term--make-marker bar)
      (let* ((cj/ai-term-project-roots nil)
             (cj/ai-term-container-roots (list container))
             (got (sort (cj/--ai-term-candidates) #'string<)))
        (should (equal got
                       (sort (list (expand-file-name foo)
                                   (expand-file-name bar))
                             #'string<)))))))

(ert-deftest test-ai-term--candidates-container-skips-children-without-marker ()
  "Boundary: a container's children without .ai/protocols.org are skipped."
  (test-ai-term--with-fixture root
    (let ((container (expand-file-name "code" root))
          (foo (expand-file-name "code/foo" root))
          (bare (expand-file-name "code/bare" root)))
      (make-directory container)
      (make-directory foo)
      (make-directory bare)
      (test-ai-term--make-marker foo)
      (let ((cj/ai-term-project-roots nil)
            (cj/ai-term-container-roots (list container)))
        (should (equal (cj/--ai-term-candidates)
                       (list (expand-file-name foo))))))))

(ert-deftest test-ai-term--candidates-container-skips-non-directory-entries ()
  "Boundary: a container's non-directory entries are ignored."
  (test-ai-term--with-fixture root
    (let ((container (expand-file-name "code" root))
          (foo (expand-file-name "code/foo" root))
          (stray (expand-file-name "code/README.txt" root)))
      (make-directory container)
      (make-directory foo)
      (test-ai-term--make-marker foo)
      (write-region "" nil stray)
      (let ((cj/ai-term-project-roots nil)
            (cj/ai-term-container-roots (list container)))
        (should (equal (cj/--ai-term-candidates)
                       (list (expand-file-name foo))))))))

(ert-deftest test-ai-term--candidates-nonexistent-root-is-skipped ()
  "Error: a nonexistent search root is skipped silently, no error raised."
  (test-ai-term--with-fixture root
    (let ((cj/ai-term-project-roots
           (list (expand-file-name "does-not-exist" root)))
          (cj/ai-term-container-roots
           (list (expand-file-name "also-missing" root))))
      (should (null (cj/--ai-term-candidates))))))

(ert-deftest test-ai-term--candidates-empty-roots-yield-empty-list ()
  "Boundary: nil roots yield nil."
  (let ((cj/ai-term-project-roots nil)
        (cj/ai-term-container-roots nil))
    (should (null (cj/--ai-term-candidates)))))

(ert-deftest test-ai-term--candidates-mixed-roots ()
  "Normal: project + container roots combine in one result list."
  (test-ai-term--with-fixture root
    (let ((emacs-d (expand-file-name "emacs-d" root))
          (container (expand-file-name "code" root))
          (foo (expand-file-name "code/foo" root)))
      (make-directory emacs-d)
      (make-directory container)
      (make-directory foo)
      (test-ai-term--make-marker emacs-d)
      (test-ai-term--make-marker foo)
      (let* ((cj/ai-term-project-roots (list emacs-d))
             (cj/ai-term-container-roots (list container))
             (got (sort (cj/--ai-term-candidates) #'string<)))
        (should (equal got
                       (sort (list (expand-file-name emacs-d)
                                   (expand-file-name foo))
                             #'string<)))))))

(provide 'test-ai-term--candidates)
;;; test-ai-term--candidates.el ends here

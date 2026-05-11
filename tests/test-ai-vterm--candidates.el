;;; test-ai-vterm--candidates.el --- Tests for cj/--ai-vterm-candidates -*- lexical-binding: t; -*-

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
(require 'ai-vterm)

(defun test-ai-vterm--make-marker (dir)
  "Create DIR/.ai/protocols.org so DIR registers as an AI-agent project."
  (let ((ai-dir (expand-file-name ".ai" dir)))
    (make-directory ai-dir t)
    (write-region "" nil (expand-file-name "protocols.org" ai-dir))))

(defmacro test-ai-vterm--with-fixture (root &rest body)
  "Bind ROOT to a fresh temp directory; remove on exit; run BODY."
  (declare (indent 1) (debug t))
  `(let ((,root (make-temp-file "ai-vterm-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,root t))))

(ert-deftest test-ai-vterm--candidates-project-root-with-marker ()
  "Normal: a project root containing .ai/protocols.org is included."
  (test-ai-vterm--with-fixture root
    (let ((proj (expand-file-name "emacs-d-fake" root)))
      (make-directory proj)
      (test-ai-vterm--make-marker proj)
      (let ((cj/ai-vterm-project-roots (list proj))
            (cj/ai-vterm-container-roots nil))
        (should (equal (cj/--ai-vterm-candidates)
                       (list (expand-file-name proj))))))))

(ert-deftest test-ai-vterm--candidates-project-root-without-marker ()
  "Boundary: a project root without .ai/protocols.org is excluded."
  (test-ai-vterm--with-fixture root
    (let ((proj (expand-file-name "no-ai" root)))
      (make-directory proj)
      (let ((cj/ai-vterm-project-roots (list proj))
            (cj/ai-vterm-container-roots nil))
        (should (null (cj/--ai-vterm-candidates)))))))

(ert-deftest test-ai-vterm--candidates-container-includes-children-with-marker ()
  "Normal: a container's children with .ai/protocols.org are included."
  (test-ai-vterm--with-fixture root
    (let ((container (expand-file-name "code" root))
          (foo (expand-file-name "code/foo" root))
          (bar (expand-file-name "code/bar" root)))
      (make-directory container)
      (make-directory foo)
      (make-directory bar)
      (test-ai-vterm--make-marker foo)
      (test-ai-vterm--make-marker bar)
      (let* ((cj/ai-vterm-project-roots nil)
             (cj/ai-vterm-container-roots (list container))
             (got (sort (cj/--ai-vterm-candidates) #'string<)))
        (should (equal got
                       (sort (list (expand-file-name foo)
                                   (expand-file-name bar))
                             #'string<)))))))

(ert-deftest test-ai-vterm--candidates-container-skips-children-without-marker ()
  "Boundary: a container's children without .ai/protocols.org are skipped."
  (test-ai-vterm--with-fixture root
    (let ((container (expand-file-name "code" root))
          (foo (expand-file-name "code/foo" root))
          (bare (expand-file-name "code/bare" root)))
      (make-directory container)
      (make-directory foo)
      (make-directory bare)
      (test-ai-vterm--make-marker foo)
      (let ((cj/ai-vterm-project-roots nil)
            (cj/ai-vterm-container-roots (list container)))
        (should (equal (cj/--ai-vterm-candidates)
                       (list (expand-file-name foo))))))))

(ert-deftest test-ai-vterm--candidates-container-skips-non-directory-entries ()
  "Boundary: a container's non-directory entries are ignored."
  (test-ai-vterm--with-fixture root
    (let ((container (expand-file-name "code" root))
          (foo (expand-file-name "code/foo" root))
          (stray (expand-file-name "code/README.txt" root)))
      (make-directory container)
      (make-directory foo)
      (test-ai-vterm--make-marker foo)
      (write-region "" nil stray)
      (let ((cj/ai-vterm-project-roots nil)
            (cj/ai-vterm-container-roots (list container)))
        (should (equal (cj/--ai-vterm-candidates)
                       (list (expand-file-name foo))))))))

(ert-deftest test-ai-vterm--candidates-nonexistent-root-is-skipped ()
  "Error: a nonexistent search root is skipped silently, no error raised."
  (test-ai-vterm--with-fixture root
    (let ((cj/ai-vterm-project-roots
           (list (expand-file-name "does-not-exist" root)))
          (cj/ai-vterm-container-roots
           (list (expand-file-name "also-missing" root))))
      (should (null (cj/--ai-vterm-candidates))))))

(ert-deftest test-ai-vterm--candidates-empty-roots-yield-empty-list ()
  "Boundary: nil roots yield nil."
  (let ((cj/ai-vterm-project-roots nil)
        (cj/ai-vterm-container-roots nil))
    (should (null (cj/--ai-vterm-candidates)))))

(ert-deftest test-ai-vterm--candidates-mixed-roots ()
  "Normal: project + container roots combine in one result list."
  (test-ai-vterm--with-fixture root
    (let ((emacs-d (expand-file-name "emacs-d" root))
          (container (expand-file-name "code" root))
          (foo (expand-file-name "code/foo" root)))
      (make-directory emacs-d)
      (make-directory container)
      (make-directory foo)
      (test-ai-vterm--make-marker emacs-d)
      (test-ai-vterm--make-marker foo)
      (let* ((cj/ai-vterm-project-roots (list emacs-d))
             (cj/ai-vterm-container-roots (list container))
             (got (sort (cj/--ai-vterm-candidates) #'string<)))
        (should (equal got
                       (sort (list (expand-file-name emacs-d)
                                   (expand-file-name foo))
                             #'string<)))))))

(provide 'test-ai-vterm--candidates)
;;; test-ai-vterm--candidates.el ends here

;;; test-ai-vterm--pick-project.el --- Tests for cj/--ai-vterm-pick-project -*- lexical-binding: t; -*-

;;; Commentary:
;; The picker presents abbreviated paths to `completing-read', then
;; returns the absolute path corresponding to the user's choice.  Empty
;; candidate set raises a `user-error' rather than offering an empty
;; prompt.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-vterm)

(ert-deftest test-ai-vterm--pick-project-returns-absolute-path-of-choice ()
  "Normal: user picks a candidate, picker returns its absolute path."
  (cl-letf (((symbol-function 'cj/--ai-vterm-candidates)
             (lambda () '("/home/u/code/foo" "/home/u/code/bar")))
            ((symbol-function 'completing-read)
             (lambda (_p collection &rest _)
               ;; Pick the one whose display form matches ~/code/bar
               ;; (collection is alist of display . abs)
               (car (cl-find-if
                     (lambda (cell) (string-match-p "bar" (car cell)))
                     collection)))))
    (should (equal (cj/--ai-vterm-pick-project) "/home/u/code/bar"))))

(ert-deftest test-ai-vterm--pick-project-empty-candidates-raises-user-error ()
  "Error: no candidates -> user-error rather than empty prompt."
  (cl-letf (((symbol-function 'cj/--ai-vterm-candidates) (lambda () nil)))
    (should-error (cj/--ai-vterm-pick-project) :type 'user-error)))

(ert-deftest test-ai-vterm--pick-project-presents-abbreviated-paths ()
  "Normal: the completing-read collection holds abbreviated display forms."
  (let (received-collection)
    (cl-letf (((symbol-function 'cj/--ai-vterm-candidates)
               (lambda () (list (expand-file-name "~/code/foo"))))
              ((symbol-function 'completing-read)
               (lambda (_p collection &rest _)
                 (setq received-collection collection)
                 (caar collection))))
      (cj/--ai-vterm-pick-project)
      (should (equal (caar received-collection) "~/code/foo")))))

(provide 'test-ai-vterm--pick-project)
;;; test-ai-vterm--pick-project.el ends here

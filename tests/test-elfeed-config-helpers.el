;;; test-elfeed-config-helpers.el --- Tests for elfeed stream/process helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Coverage for the elfeed-config entry-processing helper:
;;   - cj/elfeed-process-entries: applies an action to each selected entry,
;;     marking them read; errors when nothing is selected, skips entries with
;;     no link, and (by default) catches per-entry action errors.
;;
;; The cj/elfeed-process-entries tests build real `elfeed-entry' structs
;; rather than stubbing the `elfeed-entry-link' accessor.  A byte-compiled
;; cj/elfeed-process-entries inlines that accessor -- the cl-defstruct
;; compiler macro expands `(elfeed-entry-link e)' into an `elfeed-entry-p'
;; check plus an `aref' -- so a function stub is bypassed and the inlined
;; check type-rejects a fake entry.  Using genuine structs survives both the
;; interpreted and the byte-compiled load.  The elfeed-search UI boundary
;; (selection, tagging, redisplay) is still stubbed.  `skip-unless' guards
;; the struct-building tests so an environment without the elfeed package
;; skips rather than errors.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
;; Put the installed packages on the load-path so the real `elfeed-entry'
;; struct is available -- the process-entries tests build genuine structs
;; rather than fakes (see Commentary).  `skip-unless' covers the rare
;; environment where the package isn't installed.
(package-initialize)
(require 'elfeed-config)
(require 'elfeed nil t)

;;; cj/elfeed-process-entries

(defun cj/test--elfeed-entry (link)
  "Return a real `elfeed-entry' whose link slot is LINK."
  (elfeed-entry--create :link link))

(ert-deftest test-elfeed-process-entries-normal-applies-action-and-marks-read ()
  "Normal: each selected entry's link is passed to the action and untagged."
  (skip-unless (featurep 'elfeed))
  (let* ((e1 (cj/test--elfeed-entry "http://e1"))
         (e2 (cj/test--elfeed-entry "http://e2"))
         (acted nil) (untagged nil))
    (cl-letf (((symbol-function 'elfeed-search-selected) (lambda (&rest _) (list e1 e2)))
              ((symbol-function 'elfeed-untag) (lambda (e &rest _) (push e untagged)))
              ((symbol-function 'elfeed-search-update-entry) #'ignore)
              ((symbol-function 'use-region-p) (lambda () t)))
      (cj/elfeed-process-entries (lambda (link) (push link acted)) "open")
      (should (equal '("http://e1" "http://e2") (nreverse acted)))
      (should (equal (list e1 e2) (nreverse untagged))))))

(ert-deftest test-elfeed-process-entries-error-no-selection ()
  "Error: no selected entries signals rather than silently doing nothing."
  (cl-letf (((symbol-function 'elfeed-search-selected) (lambda (&rest _) nil)))
    (should-error (cj/elfeed-process-entries #'ignore "open") :type 'error)))

(ert-deftest test-elfeed-process-entries-boundary-skips-entry-with-no-link ()
  "Boundary: an entry with no link is untagged but not passed to the action."
  (skip-unless (featurep 'elfeed))
  (let ((entry (cj/test--elfeed-entry nil))
        (acted 0))
    (cl-letf (((symbol-function 'elfeed-search-selected) (lambda (&rest _) (list entry)))
              ((symbol-function 'elfeed-untag) #'ignore)
              ((symbol-function 'elfeed-search-update-entry) #'ignore)
              ((symbol-function 'use-region-p) (lambda () t)))
      (cj/elfeed-process-entries (lambda (_) (cl-incf acted)) "open")
      (should (= 0 acted)))))

(ert-deftest test-elfeed-process-entries-error-default-catches-action-error ()
  "Error: by default a per-entry action error is caught (messaged), not raised."
  (skip-unless (featurep 'elfeed))
  (let ((entry (cj/test--elfeed-entry "http://e1")))
    (cl-letf (((symbol-function 'elfeed-search-selected) (lambda (&rest _) (list entry)))
              ((symbol-function 'elfeed-untag) #'ignore)
              ((symbol-function 'elfeed-search-update-entry) #'ignore)
              ((symbol-function 'use-region-p) (lambda () t))
              ((symbol-function 'message) #'ignore))
      ;; Returns normally despite the action erroring.
      (cj/elfeed-process-entries (lambda (_) (error "boom")) "open")
      (should t))))

(ert-deftest test-elfeed-process-entries-boundary-skip-error-handling-propagates ()
  "Boundary: with SKIP-ERROR-HANDLING, a per-entry action error propagates."
  (skip-unless (featurep 'elfeed))
  (let ((entry (cj/test--elfeed-entry "http://e1")))
    (cl-letf (((symbol-function 'elfeed-search-selected) (lambda (&rest _) (list entry)))
              ((symbol-function 'elfeed-untag) #'ignore)
              ((symbol-function 'elfeed-search-update-entry) #'ignore)
              ((symbol-function 'use-region-p) (lambda () t)))
      (should-error (cj/elfeed-process-entries (lambda (_) (error "boom")) "open" t)))))

(provide 'test-elfeed-config-helpers)
;;; test-elfeed-config-helpers.el ends here

;;; test-org-capture-config-target-cache.el --- Tests for capture target cache -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for cached file+headline org-capture target lookup.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-capture)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-capture-config)

(defun test-org-capture-target-cache--reset ()
  "Reset target cache between tests."
  (clrhash cj/org-capture--file-headline-target-cache))

(defmacro test-org-capture-target-cache--with-temp-org-file (contents &rest body)
  "Create a temporary Org file with CONTENTS, then run BODY.
The file-visiting buffer is killed after BODY returns."
  (declare (indent 1))
  `(let ((file (make-temp-file "org-capture-target-cache" nil ".org" ,contents)))
     (unwind-protect
         (progn ,@body)
       (when-let ((buffer (find-buffer-visiting file)))
         (kill-buffer buffer))
       (when (file-exists-p file)
         (delete-file file)))))

(ert-deftest test-org-capture-target-cache-second-file-headline-lookup-reuses-marker ()
  "Normal: repeated file+headline target resolution avoids a second headline scan."
  (test-org-capture-target-cache--reset)
  (unwind-protect
      (test-org-capture-target-cache--with-temp-org-file
          "* Inbox\n** Existing task\n"
        (let ((scan-count 0)
              (original-re-search-forward (symbol-function 're-search-forward)))
          (cl-letf (((symbol-function 're-search-forward)
                     (lambda (regexp &optional bound noerror count)
                       (when (and (stringp regexp)
                                  (string-match-p "Inbox" regexp))
                         (cl-incf scan-count))
                       (funcall original-re-search-forward
                                regexp bound noerror count))))
            (let ((org-capture-plist `(:target (file+headline ,file "Inbox"))))
              (org-capture-set-target-location)
              (org-capture-set-target-location))
            (should (= 1 scan-count)))))
    (test-org-capture-target-cache--reset)))

(ert-deftest test-org-capture-target-cache-validates-marker-headline ()
  "Boundary: a cached marker is invalid when its heading no longer matches."
  (test-org-capture-target-cache--reset)
  (unwind-protect
      (test-org-capture-target-cache--with-temp-org-file
          "* Inbox\n"
        (let ((org-capture-plist `(:target (file+headline ,file "Inbox"))))
          (org-capture-set-target-location)
          (let* ((key (cj/org-capture--file-headline-cache-key file "Inbox"))
                 (marker (gethash key cj/org-capture--file-headline-target-cache)))
            (should (cj/org-capture--headline-marker-valid-p marker "Inbox"))
            (with-current-buffer (marker-buffer marker)
              (save-excursion
                (goto-char marker)
                (insert "Renamed ")))
            (should-not
             (cj/org-capture--headline-marker-valid-p marker "Inbox")))))
    (test-org-capture-target-cache--reset)))

(ert-deftest test-org-capture-target-cache-creates-missing-headline-and-caches-it ()
  "Boundary: missing file+headline targets are created and cached."
  (test-org-capture-target-cache--reset)
  (unwind-protect
      (test-org-capture-target-cache--with-temp-org-file
          "#+title: Empty\n"
        (let ((org-capture-plist `(:target (file+headline ,file "Inbox"))))
          (org-capture-set-target-location)
          (with-current-buffer (org-capture-get :buffer)
            (goto-char (org-capture-get :pos))
            (should (looking-at-p "\\* Inbox")))
          (let* ((key (cj/org-capture--file-headline-cache-key file "Inbox"))
                 (marker (gethash key cj/org-capture--file-headline-target-cache)))
            (should (cj/org-capture--headline-marker-valid-p marker "Inbox")))))
    (test-org-capture-target-cache--reset)))

(ert-deftest test-org-capture-target-cache-recovers-after-buffer-killed ()
  "Boundary: killing the marker's buffer invalidates the cache entry.
The next capture rescans (and refreshes), without crashing on the stale marker."
  (test-org-capture-target-cache--reset)
  (unwind-protect
      (test-org-capture-target-cache--with-temp-org-file
          "* Inbox\n"
        (let ((org-capture-plist `(:target (file+headline ,file "Inbox"))))
          (org-capture-set-target-location)
          (let* ((key (cj/org-capture--file-headline-cache-key file "Inbox"))
                 (marker (gethash key cj/org-capture--file-headline-target-cache)))
            (kill-buffer (marker-buffer marker))
            (should-not (cj/org-capture--headline-marker-valid-p marker "Inbox"))
            ;; A second resolution must succeed without erroring on the stale marker.
            (org-capture-set-target-location)
            (let ((fresh (gethash key cj/org-capture--file-headline-target-cache)))
              (should (cj/org-capture--headline-marker-valid-p fresh "Inbox"))))))
    (test-org-capture-target-cache--reset)))

(ert-deftest test-org-capture-target-cache-clear-empties-the-hash ()
  "Normal: `cj/org-capture-clear-target-cache' actually empties the cache."
  (test-org-capture-target-cache--reset)
  (unwind-protect
      (test-org-capture-target-cache--with-temp-org-file
          "* Inbox\n"
        (let ((org-capture-plist `(:target (file+headline ,file "Inbox"))))
          (org-capture-set-target-location)
          (should (= 1 (hash-table-count cj/org-capture--file-headline-target-cache)))
          ;; Suppress the user-facing message during the test.
          (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
            (cj/org-capture-clear-target-cache))
          (should (= 0 (hash-table-count cj/org-capture--file-headline-target-cache)))))
    (test-org-capture-target-cache--reset)))

(ert-deftest test-org-capture-target-cache-falls-through-for-non-file+headline ()
  "Boundary: targets that aren't `file+headline' (e.g. plain `file', `file+olp',
`file+function') fall through to the original `org-capture-set-target-location'
unchanged."
  (test-org-capture-target-cache--reset)
  (dolist (target '((file "/tmp/fall-through.org")
                    (file+olp "/tmp/fall-through.org" "Tasks" "Inbox")
                    (file+function "/tmp/fall-through.org" ignore)))
    (let ((called-with nil))
      (cj/org-capture--set-target-location-advice
       (lambda (&optional received) (setq called-with (list :ran received)))
       target)
      (should (equal called-with (list :ran target))))))

(provide 'test-org-capture-config-target-cache)
;;; test-org-capture-config-target-cache.el ends here

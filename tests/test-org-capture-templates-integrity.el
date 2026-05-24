;;; test-org-capture-templates-integrity.el --- Capture-template smoke tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Org capture templates are assembled across several feature modules
;; (org-capture-config, quick-video-capture, org-contacts-config, ...), so a
;; duplicate dispatch key or a file target pointing at an unset path variable
;; would be easy to miss.  These smoke tests load the cleanly-loadable capture
;; modules, apply their lazy additions, and assert key uniqueness and that
;; symbol-valued file targets resolve to non-empty path strings.
;;
;; Webclipper templates register only when org-web-tools is installed and are
;; covered by their own test (test-org-webclipper-commands.el), so they are out
;; of scope here.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'user-constants)
(require 'org)
(require 'org-capture)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-capture-config)
(require 'quick-video-capture)
(require 'org-contacts-config)

(defun test-capture--assembled-templates ()
  "Return `org-capture-templates' with the cleanly-loadable lazy additions.
Contacts registers its template via `with-eval-after-load' on load; the
video-download template is registered on demand here."
  (cj/setup-video-download)
  org-capture-templates)

(defun test-capture--file-target-path-symbol (entry)
  "Return ENTRY's file-target path symbol, or nil for literal/lambda targets.
Only plain symbol path slots are returned; a literal \"\" (the video
template's no-save target) or a lambda (the drill file pickers) yields nil."
  (when (>= (length entry) 4)
    (let ((target (nth 3 entry)))
      (when (and (consp target)
                 (memq (car target)
                       '(file file+headline file+olp file+datetree
                         file+olp+datetree file+function)))
        (let ((path (nth 1 target)))
          (and (symbolp path) path))))))

;;; Smoke Cases

(ert-deftest test-org-capture-template-keys-are-unique ()
  "Smoke: no two capture templates share a dispatch key."
  (let* ((keys (mapcar #'car (test-capture--assembled-templates)))
         (deduped (cl-remove-duplicates keys :test #'equal :from-end t)))
    (should (equal keys deduped))))

(ert-deftest test-org-capture-file-targets-point-at-nonempty-paths ()
  "Smoke: every symbol-valued file target resolves to a non-empty string path.
Literal-string targets and lambda targets are intentionally excluded."
  (dolist (entry (test-capture--assembled-templates))
    (let ((sym (test-capture--file-target-path-symbol entry)))
      (when sym
        (should (boundp sym))
        (should (stringp (symbol-value sym)))
        (should-not (string-empty-p (symbol-value sym)))))))

(provide 'test-org-capture-templates-integrity)
;;; test-org-capture-templates-integrity.el ends here

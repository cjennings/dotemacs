;;; repro-preview-cache.el --- Does dirvish's text preview disturb an org buffer's element cache? -*- lexical-binding: t -*-

;; Replays `dirvish--preview-file-maybe-truncate' (dirvish.el:653-684) against an
;; org file that is ALSO open for real, then kills the preview buffer the way
;; dirvish does (`dirvish--kill-buffer', dirvish.el:416).  The kill is the step
;; that matters: killing an org-mode buffer with a `buffer-file-name' is when
;; org-persist writes that file's cache.

(require 'org)
(require 'org-element)
(require 'org-persist)

(setq org-element-use-cache t
      org-element-cache-persistent t
      org-element--cache-self-verify t
      org-element--cache-self-verify-frequency 1.0)

(defun repro--build-cache (buf)
  "Actually populate the element cache of BUF by walking it."
  (with-current-buffer buf
    (org-element-cache-map (lambda (_el) nil) :granularity 'element)
    (and org-element--cache (avl-tree-size org-element--cache))))

(defun repro--warnings ()
  (when (get-buffer "*Warnings*")
    (with-current-buffer "*Warnings*" (string-trim (buffer-string)))))

(let* ((src (expand-file-name "todo.org" default-directory))
       (file (make-temp-file "repro-" nil ".org"))
       (threshold 1048576))
  (copy-file src file t)
  (let ((size (nth 7 (file-attributes file))))
    (message "== fixture %s (%d bytes, %s threshold) =="
             (file-name-nondirectory file) size
             (if (>= size threshold) "over" "under")))

  (let ((real (find-file-noselect file)))
    (message "real buffer cache after full walk: %S" (repro--build-cache real))

    (message "CONTROL: preview step skipped entirely")

    ;; --- dirvish kills the preview buffer when the session ends ---
    (let ((kill-buffer-query-functions nil)
          (preview (format "PREVIEW :: %s" (file-name-nondirectory file))))
      (when (get-buffer preview) (kill-buffer preview))
      (message "preview buffer %S killed" preview))

    ;; --- is the real buffer still coherent? ---
    (with-current-buffer real
      ;; Edit it the way Craig would after stepping back from dirvish.
      (goto-char (point-max))
      (let ((inhibit-read-only t)) (insert "\n* Repro edit\n"))
      (condition-case err
          (progn (org-element-cache-map (lambda (_el) nil) :granularity 'element)
                 (message "post-kill cache-map: clean (size=%S)"
                          (avl-tree-size org-element--cache)))
        (error (message "post-kill cache-map: ERROR %S" err)))
      (set-buffer-modified-p nil))

    (message "warnings=%s" (or (repro--warnings) "none")))

  (delete-file file))

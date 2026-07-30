;;; hostile-a-errorbranch.el --- attack the dirvish.el:680 error branch -*- lexical-binding: t -*-

;; dirvish--preview-file-maybe-truncate (dirvish.el:671-680) wraps
;;   (set-auto-mode) (font-lock-mode 1) (and so-long-detected-p (error ...))
;; in a condition-case.  On ANY error the buffer is KILLED at line 680 and
;; `dirvish-preview-setup-hook' at line 682 is NEVER reached.  By then
;; set-auto-mode has already put the buffer in org-mode with buffer-file-name
;; pointing at the real file.  So the shipped fix cannot run on that path.

(require 'org) (require 'org-element) (require 'org-persist)
(add-to-list 'load-path (expand-file-name "modules" default-directory))
(require 'dirvish-config)

(setq org-element-use-cache t
      org-element-cache-persistent t
      org-element--cache-self-verify t
      org-element--cache-self-verify-frequency 1.0)

(defun h--walk (buf)
  (with-current-buffer buf
    (org-element-cache-map (lambda (_e) nil) :granularity 'element)
    (and org-element--cache (avl-tree-size org-element--cache))))

;; Faithful replay of dirvish.el:657-684, including the condition-case and the
;; error-branch kill.  ERROR-INJECT non-nil simulates so-long-detected-p / any
;; other signal raised AFTER set-auto-mode has entered org-mode.
(defun h--preview (file error-inject)
  (with-current-buffer (get-buffer-create "*preview-temp*")
    (let ((threshold 1048576) info jka-compr-verbose)
      (with-silent-modifications
        (setq buffer-read-only t)
        (insert-file-contents file nil 0 threshold)
        (setq buffer-file-name file)
        (goto-char (point-min))
        (rename-buffer (format "PREVIEW :: 999 :: %s" (file-name-nondirectory file))))
      (condition-case err
          (let ((enable-dir-local-variables nil) (enable-local-variables :safe)
                (non-essential t) (inhibit-message t))
            (setq-local delay-mode-hooks t)
            (set-auto-mode) (font-lock-mode 1)
            (and error-inject (error "No preview of file with long lines")))
        (error (setq info (error-message-string err))))
      (message "  preview: mode=%s file=%s persist-local=%s registered-kill-hook=%s"
               major-mode (and buffer-file-name t)
               (local-variable-p 'org-element-cache-persistent)
               (and (memq 'org-persist-write-all-buffer kill-buffer-hook) t))
      (if info
          (progn (message "  ERROR BRANCH taken (%s) -> kill WITHOUT hook" info)
                 (let (kill-buffer-query-functions) (kill-buffer (current-buffer))))
        (run-hooks 'dirvish-preview-setup-hook)
        (message "  ok branch: hook ran, persist-local=%s value=%s"
                 (local-variable-p 'org-element-cache-persistent)
                 org-element-cache-persistent)
        (let (kill-buffer-query-functions) (kill-buffer (current-buffer)))))))

(defun h--trial (label error-inject)
  (let* ((src (expand-file-name "todo.org" default-directory))
         (file (make-temp-file "hostile-" nil ".org")))
    (copy-file src file t)
    (message "== %s ==" label)
    (let ((real (find-file-noselect file)))
      (message "  real cache: %S" (h--walk real))
      (h--preview file error-inject)
      (with-current-buffer real
        (goto-char (point-max))
        (condition-case err
            (progn (let ((inhibit-read-only t)) (insert "\n* Hostile edit\n"))
                   (org-element-cache-map (lambda (_e) nil) :granularity 'element)
                   (message "  RESULT: CLEAN (size=%S)" (avl-tree-size org-element--cache)))
          (error (message "  RESULT: CORRUPT -> %S" err)))
        (set-buffer-modified-p nil))
      (let (kill-buffer-query-functions) (kill-buffer real)))
    (delete-file file)))

(h--trial "control: no error, hook runs (shipped fix active)" nil)
(h--trial "attack: error after set-auto-mode -> hook skipped" t)

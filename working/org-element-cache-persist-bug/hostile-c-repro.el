;;; -*- lexical-binding: t -*-
(setq org-persist-directory (expand-file-name "hostile-c-persist" default-directory))
(require 'org)
(require 'org-element)
(require 'org-persist)
(let* ((f (expand-file-name "hostile-c-real.org" default-directory)))
  (with-temp-file f (insert "* Head one\nbody\n* Head two\n"))
  ;; 1. the REAL buffer, as Craig has open
  (let ((real (find-file-noselect f)))
    (with-current-buffer real (org-element-at-point (point-max)))
    (princ (format "real registered: %S\n"
                   (and (org-persist--find-index
                         `(:container ,(org-persist--normalize-container
                                        `((elisp org-element--cache) (version ,org-element-cache-version)))
                           :associated ,(org-persist--normalize-associated real)))
                        t)))
    ;; 2. dirvish preview: second org buffer claiming the same file name
    (let ((prev (get-buffer-create "PREVIEW :: hostile-c-real.org")))
      (with-current-buffer prev
        (insert-file-contents f)
        (setq buffer-file-name f)
        (delay-mode-hooks (org-mode))
        ;; the commit's hook
        (setq-local org-element-cache-persistent nil)
        ;; 3. anything that resets the cache in this live preview buffer
        (org-element-cache-reset))
      (princ (format "real registered AFTER preview reset: %S\n"
                     (and (org-persist--find-index
                           `(:container ,(org-persist--normalize-container
                                          `((elisp org-element--cache) (version ,org-element-cache-version)))
                             :associated ,(org-persist--normalize-associated real)))
                          t))))))

;;; org-export-config.el --- Org Export Configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; --------------------------------- Org Export --------------------------------
;; backends for exporting from org-mode

(use-package ox
  :ensure nil
  :after org
  :config
  (setq org-export-coding-system 'utf-8)        ;; force utf-8 in org
  (setq org-export-headline-levels 6)           ;; export headlines 6 levels deep
  (setq org-export-initial-scope 'subtree)      ;; export the current subtree by default
  (setq org-export-with-author nil)             ;; export without author by default
  (setq org-export-with-section-numbers nil)    ;; export without section numbers by default
  (setq org-export-with-tags nil)               ;; export without tags by default
  (setq org-export-with-tasks nil)              ;; export without tasks by default
  (setq org-export-with-toc nil))               ;; export without table of contents by default

;; markdown
(use-package ox-md
  :ensure nil ;; built-in
  :after ox)

;; hugo markdown
(use-package ox-hugo
  :after ox)

;; beamer presentations
(use-package ox-beamer
  :ensure nil ;; built-in
  :after ox)

(provide 'org-export-config)
;;; org-export-config.el ends here.

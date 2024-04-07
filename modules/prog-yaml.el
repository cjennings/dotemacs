;;; prog-yaml --- YAML Settings -*- lexical-binding: t; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

(use-package yaml-mode
  :defer .5
  :commands (yaml-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))

(add-hook 'yaml-mode-hook ' flycheck-mode-hook)

(provide 'prog-yaml)
;;; prog-yaml.el ends here

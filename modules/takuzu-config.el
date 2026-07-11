;;; takuzu-config.el --- Takuzu (Binairo) game configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Wire the standalone takuzu game package (developed at ~/code/takuzu).
;; Play with M-x takuzu.  Switch to :vc once it is published on GitHub.

;;; Code:

(use-package takuzu
  :load-path "~/code/takuzu"
  :commands (takuzu)
  :custom
  (takuzu-default-size 6)
  (takuzu-default-difficulty 'easy))

(provide 'takuzu-config)
;;; takuzu-config.el ends here

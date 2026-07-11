;;; duet-config.el --- DUET dual-pane commander configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Personal configuration glue for the DUET package, developed locally at
;; ~/code/duet.  Keybindings, defcustom values, and connection storage live
;; here; the package itself stays free of personal opinions.
;;
;; Not yet required from init.el — DUET is a pre-alpha skeleton.  Wire it in
;; once Stage 1 provides usable commands.

;;; Code:

(use-package duet
  :load-path "~/code/duet"
  :ensure nil
  :commands (duet))

(provide 'duet-config)
;;; duet-config.el ends here

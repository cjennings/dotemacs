;;; restclient-config.el --- REST API Client Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/P.
;; Load shape: eager.
;; Eager reason: none; API exploration, a command-loaded deferral candidate.
;; Top-level side effects: package configuration via use-package.
;; Runtime requires: none (configures packages via use-package).
;; Direct test load: yes.
;;
;; Integrates restclient.el for interactive API exploration from within Emacs.
;;
;; Write HTTP requests in plain text buffers, execute with C-c C-c, see
;; results inline.  Supports .rest files with variable substitution for
;; reusable API templates.
;;
;; Keybindings (C-; R prefix):
;; - C-; R n : New scratch *restclient* buffer
;; - C-; R o : Open a .rest file (defaults to data/)

;;; Code:

;; --------------------------------- Constants ---------------------------------

(defvar cj/restclient-data-dir (expand-file-name "data/" user-emacs-directory)
  "Directory containing .rest API template files.")

;; -------------------------------- use-package --------------------------------

(use-package restclient
  :ensure t
  :defer t
  :mode ("\\.rest\\'" . restclient-mode))

(use-package restclient-jq
  :ensure t
  :if (executable-find "jq")
  :after restclient)

;; ----------------------------- Public Functions ------------------------------

(defun cj/restclient-new-buffer ()
  "Open a scratch *restclient* buffer in `restclient-mode'."
  (interactive)
  (let ((buf (get-buffer-create "*restclient*")))
    (switch-to-buffer buf)
    (unless (derived-mode-p 'restclient-mode)
      (restclient-mode))))

(defun cj/restclient-open-file ()
  "Prompt for a .rest file to open, defaulting to the data/ directory."
  (interactive)
  (let ((file (read-file-name "Open .rest file: " cj/restclient-data-dir nil t nil
                              (lambda (f)
                                (or (file-directory-p f)
                                    (string-match-p "\\.rest\\'" f))))))
    (find-file file)))

;; -------------------------------- Keybindings --------------------------------

(global-set-key (kbd "C-; R n") #'cj/restclient-new-buffer)
(global-set-key (kbd "C-; R o") #'cj/restclient-open-file)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; R" "REST client"
    "C-; R n" "new scratch buffer"
    "C-; R o" "open .rest file"))

(provide 'restclient-config)
;;; restclient-config.el ends here

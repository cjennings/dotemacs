;;; restclient-config.el --- REST API Client Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; Integrates restclient.el for interactive API exploration from within Emacs.
;;
;; Write HTTP requests in plain text buffers, execute with C-c C-c, see
;; results inline.  Supports .rest files with variable substitution for
;; reusable API templates.
;;
;; Includes SkyFi satellite imagery API integration with automatic key
;; injection from authinfo.gpg (key never stored on disk).
;;
;; Keybindings (C-; R prefix):
;; - C-; R n : New scratch *restclient* buffer
;; - C-; R o : Open a .rest file (defaults to data/)
;; - C-; R s : Open SkyFi API template

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

;; ----------------------------- Private Helpers -------------------------------

(defun cj/restclient--inject-skyfi-key ()
  "Replace the :skyfi-key variable line with the real key from authinfo.
Only acts when the buffer is in `restclient-mode', the filename ends
in \"skyfi-api.rest\", and a :skyfi-key line exists.  If the auth
lookup returns nil, leaves the buffer unchanged."
  (when (and (derived-mode-p 'restclient-mode)
             buffer-file-name
             (string-match-p "skyfi-api\\.rest\\'" buffer-file-name))
    (let ((key (condition-case nil
                   (cj/skyfi-api-key)
                 (error nil))))
      (when key
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^:skyfi-key = .*$" nil t)
            (replace-match (format ":skyfi-key = %s" key))))))))

;; ----------------------------- Public Functions ------------------------------

(defun cj/skyfi-api-key ()
  "Fetch SkyFi API key from authinfo.gpg."
  (cj/auth-source-secret "app.skyfi.com" "apikey"))

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

(defun cj/restclient-skyfi-buffer ()
  "Open the SkyFi API template file.
Runs the key-injection hook after opening."
  (interactive)
  (let ((skyfi-file (expand-file-name "skyfi-api.rest" cj/restclient-data-dir)))
    (unless (file-exists-p skyfi-file)
      (user-error "SkyFi template not found: %s" skyfi-file))
    (find-file skyfi-file)
    (cj/restclient--inject-skyfi-key)))

;; -------------------------------- Keybindings --------------------------------

(global-set-key (kbd "C-; R n") #'cj/restclient-new-buffer)
(global-set-key (kbd "C-; R o") #'cj/restclient-open-file)
(global-set-key (kbd "C-; R s") #'cj/restclient-skyfi-buffer)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; R" "REST client"
    "C-; R n" "new scratch buffer"
    "C-; R o" "open .rest file"
    "C-; R s" "SkyFi API template"))

(provide 'restclient-config)
;;; restclient-config.el ends here

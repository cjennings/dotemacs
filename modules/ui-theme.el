;;; ui-theme.el ---  UI Theme Configuration and Persistence -*- lexical-binding: t; -*-
;; Craig Jennings <c@cjennings.net>
;;
;;; Commentary:

;; Add custom theme files to "themes" subdirectory.
;; Load other preferred themes via use-package.
;; cj/switch-theme function unloads previous themes then applies the chosen theme.
;; Persist themes between sessions using a file to store the theme name.
;; The persist file can live in a sync'd dir, so theme choice may persist across machines.

;;; Code:

;; ----------------------------------- Themes ----------------------------------
;; theme choices and settings

;; downloaded custom themes go in themes subdirectory
(setq custom-safe-themes t)  ;; trust all custom themes
(add-to-list 'custom-theme-load-path
			 (concat user-emacs-directory "themes"))

(use-package ef-themes)
(use-package github-dark-vscode-theme)
(use-package madhat2r-theme)
(use-package adwaita-dark-theme)

;; ------------------------------- Switch Themes -------------------------------
;; loads themes in completing read, then persists via the functions below

(defun cj/switch-themes ()
  "Function to switch themes and save chosen theme name for persistence.
Unloads any other applied themes before applying the chosen theme."
  (interactive)
  (let ((chosentheme ""))
    (setq chosentheme
		  (completing-read "Load custom theme: "
						   (mapcar 'symbol-name
								   (custom-available-themes))))
    (mapcar #'disable-theme custom-enabled-themes)
    (load-theme (intern chosentheme) t))
  (cj/save-theme-to-file))
(global-set-key (kbd "M-L") 'cj/switch-themes)

;; ----------------------------- Theme Persistence -----------------------------
;; persistence utility functions used by switch themes.

(defvar theme-file (concat sync-dir "emacs-theme.persist")
  "The location of the file to persist the theme name.")

(defvar fallback-theme-name "wombat"
  "The name of the theme to fallback on.
This is used then there's no file, or the theme name doesn't match
any of the installed themes. If theme name is 'nil', there will be
no theme.")

(defun cj/read-file-contents (filename)
  "Read FILENAME and return its content as a string.
If FILENAME isn't readable, return nil."
  (if (file-readable-p filename)
      (with-temp-buffer
		(insert-file-contents filename)
		(buffer-string))
	nil))

(defun cj/write-file-contents (content filename)
  "Write CONTENT to FILENAME.
If FILENAME isn't writeable, return nil. If successful, return t."
  (if (file-writable-p filename)
      (progn
		(with-temp-buffer
		  (insert content)
		  (write-file filename))
		t)
	nil))

(defun cj/get-active-theme-name ()
  "Return the name of the active UI theme as a string."
  (symbol-name (car custom-enabled-themes)))

(defun cj/save-theme-to-file ()
  "Save the string representing the current theme to the theme-file."
  (if (equal (cj/write-file-contents (cj/get-active-theme-name) theme-file) nil)
      (message "Cannot save theme: %s is unwriteable" theme-file)
    (message "%s theme saved to %s" (cj/get-active-theme-name) theme-file)))

(defun cj/load-fallback-theme (msg)
  "Display MSG and load ui-theme fallback-theme-name.
Used to handle errors with loading persisted theme."
  (message (concat msg (format " Loading fallback theme %s" fallback-theme-name)))
  (load-theme (intern fallback-theme-name) t))

(defun cj/load-theme-from-file ()
  "Apply the thame name contained in theme-file as the active UI theme.
If the theme is nil, it disables all current themes. If an error occurs
loading the file name, the fallback-theme-name is applied and saved."
  (let ((theme-name (cj/read-file-contents theme-file)))
	;; if theme-name is nil, unload all themes and load fallback theme
	(if (or (string= theme-name "nil") (not theme-name))
		(progn
			(mapcar #'disable-theme custom-enabled-themes)
			(cj/load-fallback-theme "Theme file not found or theme name in it is nil."))
	  ;; apply theme name or if error, load fallback theme
	  (condition-case err
		  (load-theme (intern theme-name) t)
		(error
		   (cj/load-fallback-theme (concat "Error loading " theme-name
										   ".")))))))

(cj/load-theme-from-file)

(provide 'ui-theme)
;;; ui-theme.el ends here.

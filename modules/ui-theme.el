;;; ui-theme.el --- UI Theme Configuration and Persistence -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:

;; This module provides a theme management system with persistence across
;; Emacs sessions.
;;
;; - Theme selection via interactive interface (M-L)
;; - Automatic persistence of theme preference to a configurable file
;; - Safe theme loading with fallback mechanism
;; - Support for custom themes in the "themes" subdirectory
;;
;; The persistence mechanism saves theme choices to a file that can be
;; synchronized across machines, ensuring consistent appearance across
;; multiple computers.

;;; Code:

(require 'user-constants)

(eval-when-compile (defvar org-dir))

;; ----------------------------------- Themes ----------------------------------
;; theme choices and settings

;; downloaded custom themes go in themes subdirectory
(setq custom-safe-themes t)  ;; trust all custom themes
(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory "themes"))

;; ------------------------------- Switch Themes -------------------------------
;; loads themes in completing read, then persists via the functions below

(defun cj/switch-themes ()
  "Function to switch themes and save chosen theme name for persistence.
Unloads any other applied themes before applying the chosen theme."
  (interactive)
  (let ((chosentheme (completing-read "Load custom theme: "
                                      (mapcar #'symbol-name
                                              (custom-available-themes)))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (intern chosentheme) t))
  (cj/save-theme-to-file))

(keymap-global-set "M-L" #'cj/switch-themes)

;; ----------------------------- Theme Persistence -----------------------------
;; persistence utility functions used by switch themes.

(defvar theme-file (concat org-dir "emacs-theme.persist")
  "The location of the file to persist the theme name.
If you want your theme change to persist across instances, put this in a
directory that is sync'd across machines with this configuration.")

(defvar fallback-theme-name "modus-vivendi"
  "The name of the theme to fallback on.
This is used when there's no file, or the theme name doesn't match
any of the installed themes. This should be a built-in theme. If theme name is
`nil', there will be no theme.")

(defun cj/read-file-contents (filename)
  "Read FILENAME and return its content as a string.

If FILENAME isn't readable, return nil."
  (when (file-readable-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (string-trim (buffer-string)))))

(defun cj/write-file-contents (content filename)
  "Write CONTENT to FILENAME.
If FILENAME isn't writeable, return nil. If successful, return t."
  (when (file-writable-p filename)
    (condition-case err
        (progn
          (with-temp-buffer
            (insert content)
            (write-file filename))
          t)
      (error
       (message "Error writing to %s: %s" filename (error-message-string err))
       nil))))

(defun cj/get-active-theme-name ()
  "Return the name of the active UI theme as a string.
Returns fallback-theme-name if no theme is active."
  (if custom-enabled-themes
      (symbol-name (car custom-enabled-themes))
    fallback-theme-name))

(defun cj/save-theme-to-file ()
  "Save the string representing the current theme to the theme-file."
  (if (not (cj/write-file-contents (cj/get-active-theme-name) theme-file))
      (message "Cannot save theme: %s is unwriteable" theme-file)
    (message "%s theme saved to %s" (cj/get-active-theme-name) theme-file)))

(defun cj/load-fallback-theme (msg)
  "Display MSG and load ui-theme fallback-theme-name.
Used to handle errors with loading persisted theme."
  (message "%s Loading fallback theme %s" msg fallback-theme-name)
  (load-theme (intern fallback-theme-name) t))

(defun cj/load-theme-from-file ()
  "Apply the theme name contained in theme-file as the active UI theme.
If the theme is nil, it disables all current themes. If an error occurs
loading the file name, the fallback-theme-name is applied and saved."
  (let ((theme-name (cj/read-file-contents theme-file)))
    ;; if theme-name is nil, unload all themes and load fallback theme
    (if (not theme-name)
        (progn
          (mapc #'disable-theme custom-enabled-themes)
          (cj/load-fallback-theme "Theme file not found or empty."))
      ;; Check if theme is 'nil' string
      (if (string= theme-name "nil")
          (mapc #'disable-theme custom-enabled-themes)
        ;; apply theme name or if error, load fallback theme
        (condition-case err
            (load-theme (intern theme-name) t)
          (error
           (cj/load-fallback-theme
            (format "Error loading theme %s: %s."
                    theme-name (error-message-string err)))))))))

(cj/load-theme-from-file)

(provide 'ui-theme)
;;; ui-theme.el ends here

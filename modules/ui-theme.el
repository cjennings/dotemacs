;;; ui-theme.el --- UI Theme Configuration and Persistence -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: C.
;; Load shape: eager.
;; Eager reason: theme persistence and the theme keybinding; the theme itself is
;;   loaded by an explicit call in init.el.
;; Top-level side effects: registers the custom theme load path, binds a theme key.
;; Runtime requires: none.
;; Direct test load: yes.
;;
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

;; ----------------------------------- Themes ----------------------------------
;; theme choices and settings

;; downloaded custom themes go in themes subdirectory
(setq custom-safe-themes t)  ;; trust all custom themes
(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory "themes"))

;; ------------------------------- Switch Themes -------------------------------
;; loads themes in completing read, then persists via the functions below

(require 'system-lib)

(defun cj/switch-themes ()
  "Function to switch themes and save chosen theme name for persistence.
Unloads any other applied themes before applying the chosen theme."
  (interactive)
  (let ((chosentheme (completing-read "Load custom theme: "
                                      (cj/completion-table
                                       'theme
                                       (mapcar #'symbol-name
                                               (custom-available-themes))))))
    (cj/theme-disable-all)
    (cj/theme-load-name chosentheme))
  (cj/save-theme-to-file))

(keymap-global-set "M-S-l" #'cj/switch-themes)  ;; was M-L, overrides downcase-word

;; ----------------------------- Theme Persistence -----------------------------
;; persistence utility functions used by switch themes.

(defgroup cj/ui-theme nil
  "Theme persistence settings."
  :group 'faces)

(defcustom theme-file (expand-file-name "persist/emacs-theme" user-emacs-directory)
  "The location of the file to persist the theme name.
If you want your theme change to persist across instances, put this in a
directory that is sync'd across machines with this configuration."
  :type 'file
  :group 'cj/ui-theme)

(defcustom fallback-theme-name "modus-vivendi"
  "The name of the theme to fallback on.
This is used when there's no file, or the theme name doesn't match
any of the installed themes. It must be available wherever this config is
loaded, since the fallback has no further fallback. modus-vivendi ships with
Emacs, so it is present on every machine that loads this config, which makes
it the right default. If theme name is `nil', there will be no theme."
  :type 'string
  :group 'cj/ui-theme)

(defun cj/theme-read-file-contents (filename)
  "Read FILENAME and return its content as a string.

If FILENAME isn't readable, return nil."
  (when (file-readable-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (string-trim (buffer-string)))))

(defun cj/theme-write-file-contents (content filename)
  "Write CONTENT to FILENAME.
If FILENAME isn't writeable, return nil. If successful, return t."
  (when (file-writable-p filename)
    (condition-case err
        (progn
          (write-region content nil filename nil 'silent)
          t)
      (error
       (message "Error writing to %s: %s" filename (error-message-string err))
       nil))))

(defun cj/theme-disable-all ()
  "Disable all currently enabled custom themes."
  (mapc #'disable-theme (copy-sequence custom-enabled-themes)))

(defun cj/theme-load-name (theme-name)
  "Load THEME-NAME without confirmation."
  (load-theme (intern theme-name) t))

(defun cj/theme-load-fallback (msg)
  "Display MSG and load `fallback-theme-name'."
  (message "%s Loading fallback theme %s" msg fallback-theme-name)
  (cj/theme-load-name fallback-theme-name))

(defun cj/theme-apply-persisted-name (theme-name)
  "Apply persisted THEME-NAME.
Nil or empty THEME-NAME loads the fallback theme. The literal string
\"nil\" disables all themes without loading a replacement."
  (cj/theme-disable-all)
  (cond
   ((or (null theme-name) (string-empty-p theme-name))
    (cj/theme-load-fallback "Theme file not found or empty."))
   ((string= theme-name "nil")
    nil)
   (t
    (condition-case err
        (cj/theme-load-name theme-name)
      (error
       (cj/theme-load-fallback
        (format "Error loading theme %s: %s."
                theme-name (error-message-string err))))))))

(defun cj/get-active-theme-name ()
  "Return the name of the active UI theme as a string.
Returns fallback-theme-name if no theme is active."
  (if custom-enabled-themes
      (symbol-name (car custom-enabled-themes))
    fallback-theme-name))

(defun cj/save-theme-to-file ()
  "Save the string representing the current theme to the theme-file."
  (if (not (cj/theme-write-file-contents (cj/get-active-theme-name) theme-file))
      (message "Cannot save theme: %s is unwriteable" theme-file)
    (message "%s theme saved to %s" (cj/get-active-theme-name) theme-file)))

(defun cj/load-theme-from-file ()
  "Apply the theme name contained in theme-file as the active UI theme.
If the theme is nil, it disables all current themes. If an error occurs
loading the file name, the fallback-theme-name is applied and saved."
  (cj/theme-apply-persisted-name (cj/theme-read-file-contents theme-file)))

(provide 'ui-theme)
;;; ui-theme.el ends here

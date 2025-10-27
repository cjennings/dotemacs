;;; browser-config.el --- Browser Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;; This module provides browser selection and configuration for Emacs.
;; It automatically discovers available browsers on the system, allows the user
;; to choose their preferred browser via completing-read, and persists the choice
;; to a file. Works with all link types including org-mode links.
;;
;; Interactive Commands:
;;   M-x cj/choose-browser - Select default browser from available options

;;; Code:

(require 'cl-lib)

;; Persistence file for storing browser choice
(defvar cj/browser-choice-file
  (expand-file-name "browser-choice.el" user-emacs-directory)
  "File to persist the user's browser choice.")

;; Browser definitions: (executable-name browse-function display-name [program-var])
;; Use nil for executable-name to indicate a built-in Emacs browser (always available)
(defvar cj/browser-definitions
  '((nil                    eww-browse-url        "EWW (Emacs Browser)" nil)
    ("google-chrome"        browse-url-chrome     "Google Chrome"     browse-url-chrome-program)
    ("google-chrome-stable" browse-url-chrome     "Google Chrome"     browse-url-chrome-program)
    ("chrome"               browse-url-chrome     "Chrome"            browse-url-chrome-program)
    ("chromium"             browse-url-chromium   "Chromium"          browse-url-chromium-program)
    ("chromium-browser"     browse-url-chromium   "Chromium"          browse-url-chromium-program)
    ("firefox"              browse-url-firefox    "Firefox"           browse-url-firefox-program)
    ("brave"                browse-url-chrome     "Brave"             browse-url-chrome-program)
    ("brave-browser"        browse-url-chrome     "Brave Browser"     browse-url-chrome-program)
    ("microsoft-edge"       browse-url-chrome     "Microsoft Edge"    browse-url-chrome-program)
    ("vivaldi"              browse-url-chrome     "Vivaldi"           browse-url-chrome-program)
    ("opera"                browse-url-chrome     "Opera"             browse-url-chrome-program)
    ("qutebrowser"          browse-url-generic    "qutebrowser"       browse-url-generic-program))
  "List of browser definitions.
Each entry is (EXECUTABLE BROWSE-FUNCTION DISPLAY-NAME [PROGRAM-VAR]).
Use nil for EXECUTABLE to indicate a built-in Emacs browser (always available).")

(defun cj/discover-browsers ()
  "Discover available browsers on the system.
Returns a list of plists with :executable, :function, :name, and :path.
Includes built-in Emacs browsers (those with nil executable)."
  (let ((found-browsers '())
        (seen-names (make-hash-table :test 'equal)))
    (dolist (def cj/browser-definitions)
      (let* ((executable (nth 0 def))
             (browse-fn (nth 1 def))
             (display-name (nth 2 def))
             (program-var (nth 3 def))
             (path (when executable (executable-find executable)))
             ;; Built-in browsers (nil executable) are always available
             (available-p (or (null executable) path)))
        (when (and available-p (not (gethash display-name seen-names)))
          (puthash display-name t seen-names)
          (push (list :executable executable
                      :function browse-fn
                      :name display-name
                      :path path
                      :program-var program-var)
                found-browsers))))
    (nreverse found-browsers)))

(defun cj/save-browser-choice (browser-plist)
  "Save BROWSER-PLIST to the persistence file."
  (with-temp-file cj/browser-choice-file
    (insert ";;; Browser choice - Auto-generated\n")
    (insert (format "(setq cj/saved-browser-choice '%S)\n" browser-plist))))

(defun cj/load-browser-choice ()
  "Load browser choice from the persistence file.
Returns the browser plist if found, nil otherwise."
  (when (file-exists-p cj/browser-choice-file)
    (condition-case nil
        (progn
          (load cj/browser-choice-file)
          (when (boundp 'cj/saved-browser-choice)
            cj/saved-browser-choice))
      (error nil))))

(defun cj/--do-apply-browser-choice (browser-plist)
  "Apply the browser settings from BROWSER-PLIST.
Returns: \\='success if applied successfully,
         \\='invalid-plist if browser-plist is nil or missing required keys."
  (if (null browser-plist)
      'invalid-plist
    (let ((browse-fn (plist-get browser-plist :function))
          (executable (plist-get browser-plist :executable))
          (path (plist-get browser-plist :path))
          (program-var (plist-get browser-plist :program-var)))
      (if (null browse-fn)
          'invalid-plist
        ;; Set the main browse-url function
        (setq browse-url-browser-function browse-fn)
        ;; Set the specific browser program variable if it exists
        (when program-var
          (set program-var (or path executable)))
        'success))))

(defun cj/apply-browser-choice (browser-plist)
  "Apply the browser settings from BROWSER-PLIST."
  (pcase (cj/--do-apply-browser-choice browser-plist)
    ('success (message "Default browser set to: %s" (plist-get browser-plist :name)))
    ('invalid-plist (message "Invalid browser configuration"))))

(defun cj/--do-choose-browser (browser-plist)
  "Save and apply BROWSER-PLIST as the default browser.
Returns: \\='success if browser was saved and applied,
         \\='save-failed if save operation failed,
         \\='invalid-plist if browser-plist is invalid."
  (condition-case _err
      (progn
        (cj/save-browser-choice browser-plist)
        (let ((result (cj/--do-apply-browser-choice browser-plist)))
          (if (eq result 'success)
              'success
            'invalid-plist)))
    (error 'save-failed)))

(defun cj/choose-browser ()
  "Interactively choose a browser from available options.
Persists the choice for future sessions."
  (interactive)
  (let* ((browsers (cj/discover-browsers))
         (choices (mapcar (lambda (b) (plist-get b :name)) browsers)))
    (if (null browsers)
        (message "No supported browsers found on system PATH")
      (let* ((choice (completing-read "Choose default browser: " choices nil t))
             (selected (cl-find-if (lambda (b)
                                     (string= (plist-get b :name) choice))
                                   browsers)))
        (when selected
          (pcase (cj/--do-choose-browser selected)
            ('success (message "Default browser set to: %s" (plist-get selected :name)))
            ('save-failed (message "Failed to save browser choice"))
            ('invalid-plist (message "Invalid browser configuration"))))))))

;; Initialize: Load saved choice or use first available browser
(defun cj/--do-initialize-browser ()
  "Initialize browser configuration.
Returns: (cons \\='loaded browser-plist) if saved choice was loaded,
         (cons \\='first-available browser-plist) if using first discovered browser,
         (cons \\='no-browsers nil) if no browsers found."
  (let ((saved-choice (cj/load-browser-choice)))
    (if saved-choice
        (cons 'loaded saved-choice)
      ;; No saved choice - try to set first available browser
      (let ((browsers (cj/discover-browsers)))
        (if browsers
            (cons 'first-available (car browsers))
          (cons 'no-browsers nil))))))

(defun cj/initialize-browser ()
  "Initialize browser configuration on startup."
  (let ((result (cj/--do-initialize-browser)))
    (pcase (car result)
      ('loaded
       (cj/--do-apply-browser-choice (cdr result)))
      ('first-available
       (let ((browser (cdr result)))
         (cj/--do-apply-browser-choice browser)
         (message "No browser configured. Using %s. Run M-x cj/choose-browser to change."
                  (plist-get browser :name))))
      ('no-browsers
       (message "No supported browsers found")))))

;; Run initialization
(cj/initialize-browser)

(keymap-global-set "C-; B" #'cj/choose-browser)

(provide 'browser-config)
;;; browser-config.el ends here

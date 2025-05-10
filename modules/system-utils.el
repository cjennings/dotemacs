;;; system-utils --- System-Wide Utilities -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

(use-package disable-mouse
  :config
  (global-disable-mouse-mode))


;; ---------------------------------- Ibuffer ----------------------------------

(global-set-key [remap list-buffers] 'ibuffer) ;; use ibuffer, not list-buffers

(use-package nerd-icons-ibuffer
  :defer .5
  :after nerd-icons
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  (setq nerd-icons-ibuffer-icon t)
  (setq nerd-icons-ibuffer-color-icon t)
  (setq  nerd-icons-ibuffer-human-readable-size t))

;; ------------------------ Killing Buffers And Windows ------------------------
;; Accidentally killing buffers can lose data. these functions override common
;; buffer killing functions and  buries buffers on the
;; 'cj/buffer-bury-alive-list' list rather than killing them. Allows for
;; interactive adding to the 'cj/buffer-bury-alive-list' via 'C-u C-x k'

;; BUFFER BURY ALIVE LIST
(defvar cj/buffer-bury-alive-list '("*dashboard*"
									"*scratch*"
									"*Messages*"
									"*ChatGPT*")
  "Buffers that shouldn't be killed, but buried instead.")

;; KILL BUFFER AND WINDOW
(defun cj/kill-buffer-and-window ()
  "Kill current buffer and window.
Buries buffers instead if they are on the cj/buffer-bury-alive-list."
  (interactive)
  (let ((target-buffer (current-buffer)))
    (delete-window)
    (cj/kill-buffer-or-bury-alive target-buffer)))
(global-set-key (kbd "M-C") 'cj/kill-buffer-and-window)

;; KILL OTHER WINDOW
(defun cj/kill-other-window ()
  "Close the next window and kill any buffer in it.
Buries buffers instead if they are on the cj/buffer-bury-alive-list."
  (interactive)
  (other-window 1)
  (let ((target-buffer (current-buffer)))
    (if (not (one-window-p)) (delete-window))
    (cj/kill-buffer-or-bury-alive target-buffer)))
(global-set-key (kbd "M-O") 'cj/kill-other-window)

;; KILL ALL OTHER BUFFERS AND WINDOWS
(defun cj/kill-all-other-buffers-and-windows ()
  "Save buffers, then kill all other buffers and windows.
Buries buffers instead if they are on the cj/buffer-bury-alive-list."
  (interactive)
  (save-some-buffers)
  (delete-other-windows)
  (mapc 'cj/kill-buffer-or-bury-alive (delq (current-buffer) (buffer-list))))
(global-set-key (kbd "M-M") 'cj/kill-all-other-buffers-and-windows)

;; KILL BUFFER OR BURY ALIVE
(defun cj/kill-buffer-or-bury-alive (target-buffer)
  "Bury buffers on the bury-instead-list rather than killing them.
With a prefix, add the TARGET-BUFFER to \='cj/buffer-bury-alive-list\='."
  (interactive "bKill or Add to bury (don't kill) buffer list: ")
  (with-current-buffer target-buffer
    (if current-prefix-arg
        (progn
          (add-to-list 'cj/buffer-bury-alive-list (buffer-name (current-buffer)))
          (message "Added %s to bury-alive-list" (buffer-name (current-buffer))))
      (if (member (buffer-name (current-buffer)) cj/buffer-bury-alive-list)
          (bury-buffer)
        (kill-buffer (current-buffer))))))
(global-set-key [remap kill-buffer] #'cj/kill-buffer-or-bury-alive)

;; --------------------------- Emacs Server Shutdown ---------------------------
;; shuts down the Emacs server. useful with emacsclient.

(defun server-shutdown ()
  "Save buffers, quit, and shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))
(global-set-key (kbd "C-<f10>") 'server-shutdown)
(global-set-key (kbd "<f10>") 'save-buffers-kill-terminal)

;; --------------------------------- Free Keys ---------------------------------
;; Displays free keybindings. Allows indicating a specific key prefix.

(use-package free-keys
  :defer 1
  :bind ("C-h C-k" . free-keys))

;; --------------------------------- Sudo Edit ---------------------------------
;; Edit a file the current buffer is visiting as sudo user.

(use-package sudo-edit
  :defer 1
  :bind ("C-x M-f" . sudo-edit))

;; --------------------------- Open File With Command --------------------------
;; opens the current buffer's file with a command. Prompts if interactive.

(defun cj/open-file-with-command (command)
  "Asynchronously open the file assocated with the current buffer with COMMAND.
Don't automatically display output buffers, but keep them in buffer list."
  (interactive "MOpen with program: ")
  (let ((display-buffer-keywords
		 '(("*Async Shell Command*" display-buffer-no-window (nil)))))
    (add-to-list 'display-buffer-alist display-buffer-keywords))
  (async-shell-command (format "%s \"%s\"" command buffer-file-name)))

;; --------------------------------- Open With ---------------------------------
;; automatically opens files with specific programs using file extensions.

(use-package openwith
  :defer 1
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4" "webp"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               "mpv"
               '(file))
         ;; removed jpg from list below as dashboard was opening nxiv
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif"))
               "nsxiv"
               '(file))
         (list (openwith-make-extension-regexp
                '("odt" "odf"))
               "libreoffice"
               '(file))
         (list (openwith-make-extension-regexp
                '("cbr" "cbz"))
               "zathura"
               '(file)))))

;; --------------------------------- Which Key ---------------------------------
;; displays key bindings following your currently entered incomplete command

(use-package which-key
  :defer 1
  :config
  (setq which-key-idle-delay 3.0)
  (setq which-key-popup-type 'side-window)
  (add-to-list 'which-key-replacement-alist '((nil . "digit-argument") . t))
  (which-key-setup-side-window-right-bottom)
  (which-key-mode 1))

;; ------------------------------- Scratch Buffer ------------------------------
;; make the scratch buffer joyful, org-mode by default, and persistent.
;; coding tip: org babel + code blocks allows for more flexibility and comments.

(defvar scratch-emacs-version-and-system (concat ";; Welcome to Emacs "
                                                 emacs-version " running on "
                                                 system-configuration ".\n"))
(defvar scratch-greet (concat ";; Emacs ♥ you, "  user-login-name ". "
                              "Happy Hacking!\n\n"))


(setq initial-scratch-message (concat scratch-emacs-version-and-system
                                      scratch-greet))

;; make scratch buffer an org-mode buffer
(setq initial-major-mode 'org-mode)

;; -------------------------------- World Clock --------------------------------
;; displays current time in various timezones

(use-package time
  :ensure nil ;; built-in
  :defer .5
  :bind ("C-x c" . world-clock)
  :config
  (setq world-clock-list
        '(("Pacific/Honolulu"       "  Honolulu")
          ("America/Los_Angeles"    "  San Francisco")
          ("America/Chicago"        "  New Orleans, Chicago")
          ("America/New_York"       "  New York")
          ("Etc/UTC"                "  UTC ====================================")
          ("Europe/London"          "  London")
          ("Europe/Paris"           "  Paris, Berlin, Rome, Barcelona")
          ("Europe/Athens"          "  Athens, Istanbul, Kyiv, Moscow, Tel Aviv")
          ("Asia/Yerevan"           "  Yerevan")
          ("Asia/Kolkata"           "  India")
          ("Asia/Shanghai"          "  Shanghai, Singapore")
          ("Asia/Tokyo"             "  Tokyo, Seoul")))
  (setq world-clock-time-format " %a, %d %b @  %I:%M %p %Z"))

;; ---------------------------------- Calendar ---------------------------------
;; providing simple shortcuts
;; backward and forward day are ','  and '.'
;; shift & meta moves by week or year
;; C-. jumps to today
;; consistent with scheduling in org-mode

(use-package calendar
  :defer .5
  :ensure nil ;; built-in
  :bind
  ("M-#" . calendar)
  (:map calendar-mode-map
        ("," . calendar-backward-day)
        ("." . calendar-forward-day)
        ("<" . calendar-backward-month)
        (">" . calendar-forward-month)
        ("M-," . calendar-backward-year)
        ("M-." . calendar-forward-year)))

;; --------------------------------- Dictionary --------------------------------
;; install Webster's dictionary in StarDict format
;; http://jsomers.net/blog/dictionary

(use-package sdcv
  :defer 1
  :bind ("C-h d" . 'sdcv-search-input))

;; ------------------------------ -Keyboard Macros -----------------------------
;; note that this leverages simple, easy to remember shortcuts
;;
;; start a macro with C-f3, perform your actions, then finish with C-f3 (same key)
;; run your macro by pressing f3
;; if you wish to save and edit it (provide a keybinding), use C-u M-f3
;; otherwise, jsut save it with M-f3 and call it with M-x (name you provided)

(defun kbd-macro-start-or-end ()
  "Begins a keyboard macro definition, or if one's in progress, finish it."
  (interactive)
  (if defining-kbd-macro
      (end-kbd-macro)
    (start-kbd-macro nil)))
(global-set-key (kbd "C-<f3>") 'kbd-macro-start-or-end)
(global-set-key (kbd"<f3>") 'call-last-kbd-macro)
(global-set-key (kbd"M-<f3>") 'cj/save-maybe-edit-macro)

(defun cj/save-maybe-edit-macro (name)
  "Save a macro in `macros-file'.
Save the last defined macro as NAME at the end of your `macros-file'
The `macros-file' is defined in the constants section of the `init.el').
The function offers the option to open the `macros-file' for editing when called
with a prefix argument."
  (interactive "SName of the macro (w/o spaces): ")
  (kmacro-name-last-macro name)
  (find-file macros-file)
  (goto-char (point-max))
  (newline)
  (insert-kbd-macro name)
  (newline)
  ;; Save changes and switch back to previous buffer
  (save-buffer)
  (switch-to-buffer (other-buffer (current-buffer) 1))
  ;; Check for the prefix argument and open the macros-file to edit
  (if current-prefix-arg
      (progn
        (find-file macros-file)
        (goto-char (point-max))))
  ;; Return name for convenience
  name)

;; now load all saved macros, creating an empty macro-file if it doesn't exist.
(if (file-exists-p macros-file)
    (load macros-file)
  (progn
    (write-region ";;; -*- lexical-binding: t -*-\n" nil macros-file)
    (message "Saved macros file not found, so created: %s" macros-file)))

;; -------------------------------- Abbrev Mode --------------------------------
;; word abbreviations mode. used to auto-correct spelling (see flyspell-config)

(use-package abbrev-mode
  :ensure nil ;; built-in
  :defer .5
  :custom
  (abbrev-file-name (concat user-emacs-directory "assets/abbrev_defs"))
  :config
  (abbrev-mode)) ;; use abbrev mode everywhere

;; -------------------------------- Log Silently -------------------------------
;; utility to silently log to the Messages buffer (for debugging/warning)

(defun cj/log-silently (text)
  "Send TEXT to the Messages buffer bypassing the echo area."
  (let ((inhibit-read-only t)
        (messages-buffer (get-buffer "*Messages*")))
    (with-current-buffer messages-buffer
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert text)
      (unless (bolp)
        (insert "\n")))))

;; ----------------------------------- Proced ----------------------------------
;; yes, there's a process monitor in Emacs

(use-package proced
  :defer .5
  :ensure nil ;;built-in
  :commands proced
  :bind ("C-M-p" . proced)
  :custom
  (proced-auto-update-flag t)
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t)
  (proced-format 'custom)
  :config
  (add-to-list
   'proced-format-alist
   '(custom user pid ppid sess tree pcpu pmem rss start time state (args
																	comm))))

;; ------------------------------------ TMR ------------------------------------

(use-package tmr
  :defer .5
  :bind ("M-t" . tmr-prefix-map)
  :config
   (setq tmr-sound-file
		 "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga")
   (setq tmr-notification-urgency 'normal)
   (setq tmr-descriptions-list 'tmr-description-history))

;; ------------------------------- Who Called Me? ------------------------------
;; convenience function to display which function called a message

(defun who-called-me? (old-fun format &rest args)
  "Display the function that called a message.
OLD-FUN: The original function that is being overridden.
FORMAT : The format string used in the original function's `message` call.
ARGS   : The variables to populate the placeholders in the format string.

This function works by overriding an existing function, usually `message`,
with a new version that prints a trace of the function call stack before the
original message."
  (let ((trace nil) (n 1) (frame nil))
    (while (setf frame (backtrace-frame n))
      (setf n     (1+ n)
            trace (cons (cadr frame) trace)) )
    (apply old-fun (concat "<<%S>>\n" format) (cons trace args))))

;; uncomment this line for the underlying function
;; (advice-add 'message :around #'who-called-me?)

(provide 'system-utils)
;;; system-utils.el ends here

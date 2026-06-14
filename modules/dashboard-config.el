;;; dashboard-config.el --- Dashboard Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 2 (Core UX).
;; Category: C/S.
;; Load shape: eager.
;; Eager reason: builds the startup dashboard landing page.
;; Top-level side effects: initializes and opens the dashboard buffer at startup
;;   via use-package.
;; Runtime requires: none.
;; Direct test load: conditional (builds the dashboard buffer on load).
;;
;; Note:
;; Nerd-Icons Cheat Sheet: https://www.nerdfonts.com/cheat-sheet

;;; Code:

(eval-when-compile (require 'undead-buffers))
(declare-function cj/make-buffer-undead "undead-buffers" (string))
(autoload 'cj/make-buffer-undead "undead-buffers" nil t)
(declare-function ghostel "ghostel" (&optional arg))

;; ------------------------ Dashboard Bookmarks Override -----------------------
;; overrides the bookmark insertion from the dashboard package to provide an
;; option that only shows the bookmark name, avoiding the path. Paths are often
;; too long and the truncation options aren't aesthetically pleasing. Should be
;; accompanied by the setting (setq dashboard-bookmarks-show-path nil) in
;; config.

(defvar dashboard-bookmarks-item-format "%s"
  "Format to use when showing the base of the file name.")

(defun dashboard-insert-bookmarks (list-size)
  "Add the list of LIST-SIZE items of bookmarks."
  (require 'bookmark)
  (dashboard-insert-section
   "Bookmarks:"
   (dashboard-subseq (bookmark-all-names) list-size)
   list-size
   'bookmarks
   (dashboard-get-shortcut 'bookmarks)
   `(lambda (&rest _) (bookmark-jump ,el))
   (if-let* ((filename el)
             (path (bookmark-get-filename el))
             (path-shorten (dashboard-shorten-path path 'bookmarks)))
       (cl-case dashboard-bookmarks-show-path
         (`align
          (unless dashboard--bookmarks-cache-item-format
            (let* ((len-align (dashboard--align-length-by-type 'bookmarks))
                   (new-fmt (dashboard--generate-align-format
                             dashboard-bookmarks-item-format len-align)))
              (setq dashboard--bookmarks-cache-item-format new-fmt)))
          (format dashboard--bookmarks-cache-item-format filename path-shorten))
         (`nil filename)
         (t (format dashboard-bookmarks-item-format filename path-shorten)))
     el)))

;; ------------------------- Banner Title Centering Fix ------------------------
;; The default centering can be off due to font width calculations.
;; This override allows manual adjustment via dashboard-banner-title-offset.

(defvar dashboard-banner-title-offset 3
  "Offset to adjust banner title centering.
Positive values shift left, negative values shift right.
Adjust this if the title doesn't appear centered under the banner image.")

(defconst cj/dashboard--configured-startupify-list
  '(dashboard-insert-banner
    dashboard-insert-banner-title
    dashboard-insert-newline
    dashboard-insert-navigator
    ;; dashboard-insert-init-info  ; Disabled: package count and startup time
    dashboard-insert-newline
    dashboard-insert-items)
  "Normal dashboard sections for this configuration.")

(defconst cj/dashboard--theme-test-startupify-list
  '(dashboard-insert-banner
    dashboard-insert-banner-title
    dashboard-insert-newline
    dashboard-insert-navigator
    dashboard-insert-newline
    dashboard-insert-init-info
    dashboard-insert-newline
    dashboard-insert-items
    dashboard-insert-newline
    dashboard-insert-footer)
  "Dashboard sections used while checking theme face coverage.")

(defconst cj/dashboard--configured-items
  '((projects . 5)
    (bookmarks . 5)
    (recents . 10))
  "Normal dashboard item sections for this configuration.")

(defconst cj/dashboard--theme-test-items
  '((projects . 5)
    (bookmarks . 5)
    (recents . 10)
    (theme-test-empty . 1))
  "Dashboard item sections used while checking theme face coverage.")

(defconst cj/dashboard--configured-item-generators
  '((projects . dashboard-insert-projects)
    (bookmarks . dashboard-insert-bookmarks)
    (recents . dashboard-insert-recents))
  "Normal dashboard item generators for this configuration.")

;; Forward declarations: these are dashboard's own variables, assigned by the
;; layout-toggle commands below before the dashboard package loads.
(defvar dashboard-startupify-list)
(defvar dashboard-item-generators)
(defvar dashboard-items)
(defvar dashboard-set-footer)

(defun cj/dashboard-insert-theme-test-empty (_list-size)
  "Insert an empty Dashboard section to display `dashboard-no-items-face'."
  (dashboard-insert-section
   "No Items:"
   nil
   1
   'theme-test-empty
   nil
   #'ignore
   ""))

(defconst cj/dashboard--theme-test-item-generators
  (append cj/dashboard--configured-item-generators
          '((theme-test-empty . cj/dashboard-insert-theme-test-empty)))
  "Dashboard item generators used while checking theme face coverage.")

(defun cj/dashboard-refresh ()
  "Refresh and show the dashboard buffer, keeping the window at the top."
  (when (fboundp 'dashboard-refresh-buffer)
    (dashboard-refresh-buffer))
  (when (get-buffer "*dashboard*")
    (switch-to-buffer "*dashboard*")
    (goto-char (point-min))
    (set-window-start (selected-window) (point-min))))

(defun cj/dashboard-restore-configured-layout ()
  "Restore the normal dashboard layout after theme-face testing."
  (interactive)
  (setq dashboard-startupify-list cj/dashboard--configured-startupify-list)
  (setq dashboard-item-generators cj/dashboard--configured-item-generators)
  (setq dashboard-items cj/dashboard--configured-items)
  (setq dashboard-set-footer nil)
  (cj/dashboard-refresh))

(defun cj/dashboard-show-theme-test-layout ()
  "Show all Dashboard elements useful for live theme inspection.
This temporarily enables init info, footer, and a deliberately empty section so
the live dashboard exposes `dashboard-no-items-face'.  Use
`cj/dashboard-restore-configured-layout' to return to the normal layout."
  (interactive)
  (setq dashboard-startupify-list cj/dashboard--theme-test-startupify-list)
  (setq dashboard-item-generators cj/dashboard--theme-test-item-generators)
  (setq dashboard-items cj/dashboard--theme-test-items)
  (setq dashboard-set-footer t)
  (cj/dashboard-refresh))

;; --------------------------- Launcher Definitions ----------------------------
;; Single source of truth for the dashboard launchers.  Both the navigator
;; icon rows and the dashboard-mode-map keybindings derive from this table, so
;; a launcher is added or reordered in exactly one place (no icon-row/keymap
;; drift).  Each entry: (KEY ICON-FN ICON-NAME LABEL TOOLTIP ACTION); ACTION is
;; a zero-argument function run by both the icon button and the key.

(defconst cj/dashboard--launchers
  (list
   (list "c" #'nerd-icons-faicon  "nf-fa-code"         "Code"       "Switch Project"         (lambda () (projectile-switch-project)))
   (list "d" #'nerd-icons-faicon  "nf-fa-folder_o"     "Files"      "Dirvish File Manager"   (lambda () (dirvish user-home-dir)))
   (list "t" #'nerd-icons-devicon "nf-dev-terminal"    "Terminal"   "Launch Terminal"        (lambda () (ghostel)))
   (list "a" #'nerd-icons-mdicon  "nf-md-calendar"     "Agenda"     "Main Org Agenda"        (lambda () (cj/main-agenda-display)))
   (list "r" #'nerd-icons-faicon  "nf-fa-rss_square"   "Feeds"      "Elfeed Feed Reader"     (lambda () (cj/elfeed-open)))
   (list "b" #'nerd-icons-faicon  "nf-fae-book_open_o" "Books"      "Calibre Ebook Reader"   (lambda () (calibredb)))
   (list "f" #'nerd-icons-mdicon  "nf-md-school"       "Flashcards" "Org-Drill"              (lambda () (cj/drill-start)))
   (list "m" #'nerd-icons-mdicon  "nf-md-music"        "Music"      "EMMS Music Player"      (lambda () (cj/music-playlist-toggle) (cj/music-playlist-load)))
   (list "e" #'nerd-icons-faicon  "nf-fa-envelope"     "Email"      "Mu4e Email Client"      (lambda () (mu4e)))
   (list "i" #'nerd-icons-faicon  "nf-fa-comments"     "IRC"        "Emacs Relay Chat"       (lambda () (cj/erc-switch-to-buffer-with-completion)))
   (list "g" #'nerd-icons-faicon  "nf-fa-telegram"     "Telegram"   "Telega Telegram Client" (lambda () (cj/telega)))
   (list "s" #'nerd-icons-faicon  "nf-fa-slack"        "Slack"      "Slack Client"           (lambda () (cj/slack-start)))
   (list "l" #'nerd-icons-octicon "nf-oct-issue_tracks" "Linear"    "Linear Issue Tracker"   (lambda () (pearl-list-issues))))
  "Dashboard launcher table: (KEY ICON-FN ICON-NAME LABEL TOOLTIP ACTION).
Drives both `dashboard-navigator-buttons' and the dashboard-mode-map keys.")

(defconst cj/dashboard--row-sizes '(4 4 3 2)
  "Navigator row lengths.  Must sum to the number of `cj/dashboard--launchers'.
The last row groups Slack and Linear together.")

(defun cj/dashboard--navigator-button (l)
  "Build a `dashboard-navigator-buttons' entry from launcher L."
  (let ((icon-fn (nth 1 l)) (icon-name (nth 2 l))
        (label (nth 3 l)) (tooltip (nth 4 l)) (action (nth 5 l)))
    (list (funcall icon-fn icon-name) label tooltip
          (lambda (&rest _) (funcall action)) nil " " "")))

(defun cj/dashboard--navigator-rows ()
  "Build navigator rows from `cj/dashboard--launchers', grouped per
`cj/dashboard--row-sizes'.  Any launchers beyond the declared row sizes form a
final row, so a newly added launcher still shows up even if the sizes weren't
updated."
  (let ((launchers cj/dashboard--launchers) rows)
    (dolist (size cj/dashboard--row-sizes)
      (let (row)
        (dotimes (_ size)
          (when launchers
            (push (cj/dashboard--navigator-button (pop launchers)) row)))
        (when row (push (nreverse row) rows))))
    (when launchers
      (push (mapcar #'cj/dashboard--navigator-button launchers) rows))
    (nreverse rows)))

(defun cj/dashboard--bind-launchers (map)
  "Bind each launcher KEY in MAP to a command that runs its ACTION."
  (dolist (l cj/dashboard--launchers)
    (let ((key (nth 0 l)) (action (nth 5 l)))
      (define-key map (kbd key) (lambda () (interactive) (funcall action))))))

;; ----------------------------- Display Dashboard -----------------------------
;; convenience function to redisplay dashboard and kill all other windows

(defun cj/dashboard-only ()
  "Switch to *dashboard* buffer, kill other buffers and windows, go to top.
Reset `window-start' alongside point so a previously-scrolled view
doesn't leak into this display when the buffer is taller than the
window."
  (interactive)
  (if (get-buffer "*dashboard*")
	  (progn
		(switch-to-buffer "*dashboard*")
		(cj/kill-all-other-buffers-and-windows))
	(when (fboundp 'dashboard-open)
	  (dashboard-open)))
  (goto-char (point-min))
  (set-window-start (selected-window) (point-min)))

;; --------------------------------- Dashboard ---------------------------------
;; a useful startup screen for Emacs

(defun cj/--dashboard-exclude-emms-from-recentf ()
  "Exclude the EMMS history file from recentf.
Adds to `recentf-exclude' so entries set elsewhere (e.g. in
system-defaults) are preserved rather than overwritten."
  (require 'recentf)
  (add-to-list 'recentf-exclude "/emms/history"))

(use-package dashboard
  :demand t
  :hook (emacs-startup . cj/dashboard-only)
  :bind ("<f1>" . cj/dashboard-only)
  :custom
  (dashboard-projects-backend 'projectile)

  (dashboard-item-generators cj/dashboard--configured-item-generators)

  (dashboard-items cj/dashboard--configured-items)

  (dashboard-startupify-list cj/dashboard--configured-startupify-list)
  :config

  ;; == banner
  ;; The banner is an RGBA PNG with real transparency.  On this non-pgtk build
  ;; Emacs composites the alpha against one solid background color at render
  ;; time and caches the flat pixmap -- it can't re-blend against a window
  ;; background that changes later.  The dashboard is exempt from dimming (see
  ;; the never-dim predicate in auto-dim-config.el), so its background stays at
  ;; the theme color in every window.  That makes the constant face background
  ;; the right thing to composite against -- no :mask and no forced :background
  ;; needed; the native alpha over the theme background leaves the transparent
  ;; edges seamless.
  (setq dashboard-startup-banner (concat user-emacs-directory "assets/M-x_butterfly.png"))
  (setq dashboard-banner-logo-title "Emacs: The Editor That Saves Your Soul")

  ;; == general
  (dashboard-setup-startup-hook)                                      ;; run dashboard post emacs init
  (cj/make-buffer-undead "*dashboard*")                               ;; make this buffer unkillable

  (if (< (length command-line-args) 2)
      (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))) ;; don't display dashboard if opening a file
  (setq dashboard-display-icons-p t)                                  ;; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons)                              ;; use `nerd-icons' package
  (setq dashboard-center-content t)                                   ;; horizontally center dashboard content
  (setq dashboard-bookmarks-show-path nil)                            ;; don't show paths in bookmarks
  (setq dashboard-recentf-show-base t)                                ;; show filename, not full path
  (setq dashboard-recentf-item-format "%s")
  (cj/--dashboard-exclude-emms-from-recentf)                            ;; exclude EMMS history from recent files
  (setq dashboard-set-footer nil)                                     ;; don't show footer and quotes

  ;; == navigation
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons (cj/dashboard--navigator-rows))

  ;; == content
  (setq dashboard-show-shortcuts nil) ;; don't show dashboard item abbreviations
  (when (get-buffer "*dashboard*")
    (dashboard-refresh-buffer))
  ) ;; end use-package dashboard

;; ------------------------ Dashboard Keybindings ------------------------------

(with-eval-after-load 'dashboard
  ;; Disable 'q' to quit dashboard
  (define-key dashboard-mode-map (kbd "q") nil)

  ;; Launcher keys, derived from `cj/dashboard--launchers' (same source as the
  ;; navigator icons, so key order can't drift from the icon-row order).
  (cj/dashboard--bind-launchers dashboard-mode-map))

;; Override banner title centering (must be after dashboard-widgets loads)
(with-eval-after-load 'dashboard-widgets
  (defun dashboard-insert-banner-title ()
    "Insert `dashboard-banner-logo-title' with adjustable centering offset."
    (when dashboard-banner-logo-title
      (let* ((title dashboard-banner-logo-title)
             (start (point)))
        (insert (propertize title 'face 'dashboard-banner-logo-title))
        (let* ((end (point))
               (width (string-width title))
               (adjusted-center (+ (/ (float width) 2) dashboard-banner-title-offset))
               (prefix (propertize " " 'display `(space . (:align-to (- center ,adjusted-center))))))
          (add-text-properties start end `(line-prefix ,prefix indent-prefix ,prefix))))
      (insert "\n"))))

(provide 'dashboard-config)
;;; dashboard-config.el ends here.

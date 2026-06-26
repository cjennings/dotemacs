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

(require 'system-lib)  ;; cj/exclude-from-global-font-lock
(eval-when-compile (require 'undead-buffers))
(declare-function cj/make-buffer-undead "undead-buffers" (string))
(autoload 'cj/make-buffer-undead "undead-buffers" nil t)
(declare-function cj/term-toggle "eat-config")

;; ------------------------------ Declarations -------------------------------
;; These functions and variables belong to lazily-loaded packages or to other
;; cj modules; declaring them keeps the byte-compiler quiet without forcing an
;; eager require.  Behavior is unchanged -- the symbols still resolve at runtime
;; once their owning package/module loads.

;; dashboard package internals used by the bookmark-insertion override.
(declare-function dashboard-insert-section "dashboard")
(declare-function dashboard-subseq "dashboard")
(declare-function dashboard-get-shortcut "dashboard")
(declare-function dashboard-shorten-path "dashboard")
(declare-function dashboard--align-length-by-type "dashboard")
(declare-function dashboard--generate-align-format "dashboard")
(declare-function dashboard-refresh-buffer "dashboard")
(declare-function dashboard-open "dashboard")
(defvar dashboard-bookmarks-show-path)
(defvar dashboard--bookmarks-cache-item-format)

;; bookmark.el (required at runtime inside `dashboard-insert-bookmarks').
(declare-function bookmark-all-names "bookmark")
(declare-function bookmark-get-filename "bookmark")

;; recentf.el (required at runtime inside the exclude helper).
(defvar recentf-exclude)

;; nerd-icons glyph functions used in the launcher table.
(declare-function nerd-icons-faicon "nerd-icons")
(declare-function nerd-icons-devicon "nerd-icons")
(declare-function nerd-icons-mdicon "nerd-icons")
(declare-function nerd-icons-codicon "nerd-icons")
(declare-function nerd-icons-octicon "nerd-icons")

;; user-constants.el provides the home-directory constant.
(defvar user-home-dir)

;; Launcher actions defined in other cj modules.
(declare-function cj/main-agenda-display "org-agenda-config")
(declare-function cj/elfeed-open "elfeed-config")
(declare-function cj/drill-start "org-drill-config")
(declare-function cj/music-playlist-toggle "music-config")
(declare-function cj/music-playlist-load "music-config")
(declare-function cj/erc-switch-to-buffer-with-completion "erc-config")
(declare-function cj/telega "telega-config")
(declare-function cj/slack-start "slack-config")
(declare-function cj/signel-message "signal-config")
(declare-function cj/kill-all-other-buffers-and-windows "undead-buffers")

;; External package commands invoked by launchers.
(declare-function mu4e "mu4e")
(declare-function pearl-list-issues "pearl")

;; ------------------------ Dashboard Bookmarks Override -----------------------
;; overrides the bookmark insertion from the dashboard package to provide an
;; option that only shows the bookmark name, avoiding the path. Paths are often
;; too long and the truncation options aren't aesthetically pleasing. Should be
;; accompanied by the setting (setq dashboard-bookmarks-show-path nil) in
;; config.

(defvar dashboard-bookmarks-item-format "%s"
  "Format to use when showing the base of the file name.")

;; `el' is bound dynamically by dashboard's section-insertion machinery, which the
;; override below plugs into.  Declare it so the byte-compiler reads the
;; references as that special variable rather than a free variable.  The name is
;; dashboard's, not ours, so the missing-prefix lint is suppressed rather than
;; renamed (renaming would break the dynamic binding dashboard supplies).
(with-suppressed-warnings ((lexical el))
  (defvar el))

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
   (list "t" #'nerd-icons-devicon "nf-dev-terminal"    "Terminal"   "Launch Terminal"        (lambda () (cj/term-toggle)))
   (list "a" #'nerd-icons-mdicon  "nf-md-calendar"     "Agenda"     "Main Org Agenda"        (lambda () (cj/main-agenda-display)))
   (list "r" #'nerd-icons-faicon  "nf-fa-rss_square"   "Feeds"      "Elfeed Feed Reader"     (lambda () (cj/elfeed-open)))
   (list "b" #'nerd-icons-codicon "nf-cod-library"     "Books"      "Calibre Ebook Reader"   (lambda () (calibredb)))
   (list "f" #'nerd-icons-mdicon  "nf-md-school"       "Flashcards" "Org-Drill"              (lambda () (cj/drill-start)))
   (list "m" #'nerd-icons-mdicon  "nf-md-music"        "Music"      "EMMS Music Player"      (lambda () (cj/music-playlist-toggle) (cj/music-playlist-load)))
   (list "e" #'nerd-icons-faicon  "nf-fa-envelope"     "Email"      "Mu4e Email Client"      (lambda () (mu4e)))
   (list "i" #'nerd-icons-faicon  "nf-fa-comments"     "IRC"        "Emacs Relay Chat"       (lambda () (cj/erc-switch-to-buffer-with-completion)))
   (list "G" #'nerd-icons-faicon  "nf-fa-telegram"     "Telegram"   "Telega Telegram Client" (lambda () (cj/telega)))
   (list "s" #'nerd-icons-faicon  "nf-fa-slack"        "Slack"      "Slack Client"           (lambda () (cj/slack-start)))
   (list "l" #'nerd-icons-octicon "nf-oct-issue_tracks" "Linear"    "Linear Issue Tracker"   (lambda () (pearl-list-issues)))
   (list "S" #'nerd-icons-mdicon  "nf-md-message"      "Signal"     "Signal Messenger"       (lambda () (cj/signel-message))))
  "Dashboard launcher table: (KEY ICON-FN ICON-NAME LABEL TOOLTIP ACTION).
Drives both `dashboard-navigator-buttons' and the dashboard-mode-map keys.")

(defconst cj/dashboard--row-sizes '(4 4 3 3)
  "Navigator row lengths.  Must sum to the number of `cj/dashboard--launchers'.
The last row groups Slack, Linear, and Signal together.")

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
  ;; Refresh so re-showing the dashboard always lands on fresh content.
  (when (fboundp 'dashboard-refresh-buffer)
    (dashboard-refresh-buffer))
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

;; Keep global font-lock out of the dashboard buffer.  Dashboard colors its
;; banner title (`dashboard-banner-logo-title') and section headings
;; (`dashboard-heading') with the `face' text property; `global-font-lock-mode'
;; owns `face' and strips manually-applied ones it didn't set, so with font-lock
;; running the banner and headings fall back to the default face.  Excluding
;; dashboard-mode lets those text-property faces survive.  (Item and navigator
;; colors ride a `dashboard-items-face' overlay, which font-lock leaves alone.)
(cj/exclude-from-global-font-lock 'dashboard-mode)

(use-package dashboard
  :demand t
  :hook (emacs-startup . cj/dashboard-only)
  :bind ("<f1>" . cj/dashboard-only)
  :custom
  (dashboard-projects-backend 'projectile)

  (dashboard-item-generators
   '((projects . dashboard-insert-projects)
     (bookmarks . dashboard-insert-bookmarks)
     (recents . dashboard-insert-recents)))

  (dashboard-items '((projects . 5)
                     (bookmarks . 5)
                     (recents . 10)))

  (dashboard-startupify-list
   '(dashboard-insert-banner
     dashboard-insert-banner-title
     dashboard-insert-newline
     dashboard-insert-navigator
     ;; dashboard-insert-init-info  ; Disabled: package count and startup time
     dashboard-insert-newline
     dashboard-insert-items))
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
  (setq dashboard-set-file-icons t)                                   ;; per-filetype icons on the list items (nerd-icons colors them by type)
  (setq dashboard-set-heading-icons t)                                ;; nerd-icons on the section titles (Projects/Bookmarks/Recent)
  (setq dashboard-center-content t)                                   ;; horizontally center dashboard content
  (setq dashboard-bookmarks-show-path nil)                            ;; don't show paths in bookmarks
  (setq dashboard-recentf-show-base t)                                ;; show filename, not full path
  (setq dashboard-recentf-item-format "%s")
  (cj/--dashboard-exclude-emms-from-recentf)                            ;; exclude EMMS history from recent files

  ;; == navigation
  ;; footer and navigator visibility are controlled by `dashboard-startupify-list'
  ;; above (footer omitted, navigator included); the dashboard-set-* toggles are
  ;; obsolete as of dashboard 1.9.0.
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

  ;; 'g' refreshes the dashboard (the dired/magit convention). Telegram moved to
  ;; 'G' in the launcher table to free it.
  (define-key dashboard-mode-map (kbd "g") #'dashboard-refresh-buffer)

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

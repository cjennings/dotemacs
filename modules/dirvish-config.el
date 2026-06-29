;;; dirvish-config.el --- Dired/Dirvish Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 3 (Domain Workflow).
;; Category: D/P.
;; Load shape: eager.
;; Eager reason: none; file manager, a command/hook-loaded deferral candidate.
;; Top-level side effects: three add-hook, package configuration via use-package.
;; Runtime requires: user-constants, system-utils, host-environment, system-lib,
;;   external-open-lib.
;; Direct test load: yes.
;;
;; Enhanced file management via Dirvish (modern dired replacement) with icons,
;; previews, and quick access directories (press 'g'). Includes utilities for
;; ediff, playlist creation, path copying, and external file manager integration.
;;
;; Key Bindings:
;; - d: Diff/ediff selected files (cj/dired-ediff-files)
;; - D: Delete (dired-do-delete; mark with m for batches)
;; - g: Quick access menu (jump to predefined directories)
;; - G: Search with deadgrep in current directory
;; - f: Open system file manager in current directory
;; - o/O: Open file with xdg-open/custom command
;; - l: Copy org-link with relative file path (project-relative or home-relative)
;; - p: Copy absolute file path
;; - P: Print the file at point via CUPS
;; - S: Study — start an org-drill session on the .org file at point
;; - M-D (Meta-Shift-d): DWIM shell commands menu
;; - TAB: Toggle subtree expansion
;; - F11: Toggle sidebar view

;;; Code:

(require 'user-constants)   ;; code-dir, music-dir, pix-dir et al. used at load time
(require 'system-utils)     ;; cj/xdg-open, cj/open-file-with-command bound to keys
(require 'host-environment)
(require 'system-lib)
(require 'external-open-lib)

(declare-function cj/drill-this-file "org-drill-config")

;; Dirvish/Dired functions called from lazy-loaded packages.
(declare-function dirvish-peek-mode "dirvish")
(declare-function dirvish-side-follow-mode "dirvish")
(declare-function dirvish-quit "dirvish")
(declare-function dired-get-marked-files "dired")
(declare-function dired-dwim-target-directory "dired-aux")
(declare-function dired-get-file-for-visit "dired")
(declare-function dired-get-filename "dired")
(declare-function dired-mark "dired")
(declare-function dired-current-directory "dired")
(declare-function dired-file-name-at-point "dired-x")
(declare-function dired-find-file "dired")
(declare-function project-roots "project")

;; External package variables referenced before their package loads.
(defvar ediff-after-quit-hook-internal)
(defvar dirvish-side-attributes)

;; mark files in dirvish, attach in mu4e
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;;; ----------------------------- Dired Ediff Files -----------------------------

(defun cj/--ediff-pair-from-files (files prompt-fn newer-than-p)
  "Return a (OLDER . NEWER) cons for ediff'ing FILES.

FILES is the list of marked file paths.  PROMPT-FN is a thunk used to
acquire a second file when only one is marked.  NEWER-THAN-P is a binary
predicate (a b) -> non-nil when A is newer than B.

Signals `user-error' for zero or 3+ files; the latter matches the original
contract, the former replaces a latent crash where the caller fell through
to (file-newer-than-file-p nil ...).

Pure helper used by `cj/dired-ediff-files'."
  (let ((n (length files)))
    (cond
     ((zerop n)
      (user-error "No files marked"))
     ((> n 2)
      (user-error "No more than 2 files should be marked"))
     (t
      (let ((file1 (car files))
            (file2 (or (cadr files) (funcall prompt-fn))))
        (if (funcall newer-than-p file1 file2)
            (cons file2 file1)
          (cons file1 file2)))))))

(defun cj/dired-ediff-files ()
  "Ediff two selected files within Dired."
  (interactive)
  (let* ((wnd (current-window-configuration))
         (pair (cj/--ediff-pair-from-files
                (dired-get-marked-files)
                (lambda ()
                  (read-file-name "Ediff with file: " (dired-dwim-target-directory)))
                #'file-newer-than-file-p)))
    (ediff-files (car pair) (cdr pair))
    (add-hook 'ediff-after-quit-hook-internal
              (lambda ()
                (setq ediff-after-quit-hook-internal nil)
                (set-window-configuration wnd)))))

;; ------------------------ Create Playlist From Marked ------------------------

(defvar cj/audio-file-extensions
  '("mp3" "flac" "m4a" "wav" "ogg" "aac" "opus" "aiff" "alac" "wma")
  "List of audio file extensions (lowercase, no dot).
Used to filter files for M3U playlists.")

(defun cj/--playlist-filter-audio (files extensions)
  "Return the elements of FILES whose extension matches EXTENSIONS.

Pure helper used by `cj/dired-create-playlist-from-marked'.  EXTENSIONS
is a list of lowercase extension strings (no dot).  A file with no
extension never matches.  Comparison downcases the file's extension so
mixed-case names match."
  (cl-remove-if-not
   (lambda (f)
     (let ((ext (file-name-extension f)))
       (and ext (member (downcase ext) extensions))))
   files))

(defun cj/--playlist-sanitize-name (name)
  "Strip a trailing `.m3u' suffix from NAME and return the result.
Pure helper.  An embedded `.m3u' that isn't at the end stays put."
  (replace-regexp-in-string "\\.m3u\\'" "" name))

(defun cj/--playlist-name-safe-p (name)
  "Return non-nil when NAME is a safe bare playlist filename.
A safe name is non-empty and carries no directory separator, so it can't
steer `cj/dired-create-playlist-from-marked' to write outside `music-dir'
through a `../' or absolute path.  Pure helper."
  (and (not (string-empty-p name))
       (not (string-match-p "/" name))))

(defun cj/--playlist-resolve-target ()
  "Prompt for a playlist name and return the .m3u path to write under `music-dir'.
Re-prompt until the name is a safe bare filename (no `/').  When the target
already exists, ask whether to overwrite, cancel, or rename: overwrite returns
the path, cancel signals a `user-error', rename re-prompts.  Interactive
prompting only -- the caller does the file write."
  (let ((base-name nil)
        (playlist-path nil)
        (done nil))
    (while (not done)
      (setq base-name (cj/--playlist-sanitize-name
                       (read-string "Playlist name (without .m3u): ")))
      (cond
       ((not (cj/--playlist-name-safe-p base-name))
        (message "Playlist name must be a bare filename, without '/'."))
       (t
        (setq playlist-path (expand-file-name (concat base-name ".m3u") music-dir))
        (if (not (file-exists-p playlist-path))
            (setq done t)
          (let ((choice (read-char-choice
                         (format "Playlist '%s' exists. [o]verwrite, [c]ancel, [r]ename? "
                                 (file-name-nondirectory playlist-path))
                         '(?o ?c ?r))))
            (cl-case choice
              (?o (setq done t))
              (?c (user-error "Cancelled playlist creation"))
              (?r (setq done nil))))))))
    playlist-path))

(defun cj/dired-create-playlist-from-marked ()
  "Create an .m3u playlist file from marked files in Dired (or Dirvish).
Filters for audio files, prompts for the playlist name, and saves the resulting
.m3u in the directory specified by =music-dir=. Interactive use only."
  (interactive)
  (let* ((marked-files (dired-get-marked-files))
         (audio-files (cj/--playlist-filter-audio
                       marked-files cj/audio-file-extensions))
         (count (length audio-files)))
    (if (zerop count)
        (user-error "No audio files marked (extensions: %s)"
                    (string-join cj/audio-file-extensions ", "))
      (let ((playlist-path (cj/--playlist-resolve-target)))
        (with-temp-file playlist-path
          (dolist (af audio-files)
            (insert af "\n")))
        (message "Wrote playlist %s with %d tracks"
                 (file-name-nondirectory playlist-path) count)))))

;;; ----------------------------------- Dired -----------------------------------

(use-package dired
  :ensure nil ;; built-in
  :defer t
  :bind
  (:map dired-mode-map
        ([remap dired-summary] . which-key-show-major-mode)
        ("E" . wdired-change-to-wdired-mode) ;; edit names and properties in buffer
        ("e" . cj/dired-ediff-files)         ;; ediff files
        ("d" . cj/dired-ediff-files)         ;; d = diff, matching C-; b / ibuffer (was dired-flag-file-deletion)
        ("D" . dired-do-delete))             ;; D = delete (d no longer flags; mark with m, then D)
  :custom
  (dired-use-ls-dired nil)                             ;; non GNU FreeBSD doesn't support a "--dired" switch
  :config
  (setq dired-listing-switches "-l --almost-all --human-readable --group-directories-first")
  (setq dired-dwim-target t)
  (setq dired-clean-up-buffers-too t)                  ;; offer to kill buffers associated deleted files and dirs
  (setq dired-clean-confirm-killing-deleted-buffers nil) ;; don't ask; just kill buffers associated with deleted files
  (setq dired-recursive-copies (quote always))         ;; "always" means no asking
  (setq dired-recursive-deletes (quote top)))          ;; "top" means ask once

;; which-key labels for the d=diff / D=delete pair (shown in the major-mode
;; popup via `which-key-show-major-mode').
(with-eval-after-load 'which-key
  (which-key-add-major-mode-key-based-replacements 'dired-mode
    "d" "diff (ediff files)"
    "D" "delete file"))

;; note: disabled as it prevents marking and moving files to another directory
;; (setq dired-kill-when-opening-new-dired-buffer t)   ;; don't litter by leaving buffers when navigating directories

(add-hook 'dired-mode-hook 'auto-revert-mode)          ;; auto revert dired when files change

;;; --------------------------- Dired Open HTML In EWW --------------------------

(defun cj/--html-file-p (file)
  "Return non-nil when FILE has a `.html' or `.htm' extension.

Match is case-insensitive (`.HTML' counts) and anchored at end so
embedded `html' in the middle of a name doesn't match.  Pure helper
used by `cj/dirvish-open-html-in-eww'."
  (let ((case-fold-search t))
    (and (string-match-p "\\.html?\\'" file) t)))

(defun cj/dirvish-open-html-in-eww ()
  "Open HTML file at point in dired/dirvish using eww."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (cj/--html-file-p file)
        (eww-open-file file)
      (message "Not an HTML file: %s" file))))

;;; ------------------------ Dired Mark All Visible Files -----------------------

(defun cj/dired-mark-all-visible-files ()
  "Mark all visible files in Dired mode."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      ;; dired-mark advances point itself, so only advance manually on the
      ;; lines it isn't called for (directories, headers, totals). Use
      ;; dired-get-filename to identify real file lines; it returns nil on
      ;; non-file lines (no error with the second arg).
      (let ((fn (dired-get-filename nil t)))
        (if (and fn (not (file-directory-p fn)))
            (dired-mark 1)
          (forward-line 1))))))

;;; ------------------------ Dirvish Duplicate File Copy ------------------------

(defun cj/--duplicate-file-name (file)
  "Return FILE's path with `-copy' inserted before the extension.

Pure helper used by `cj/dirvish-duplicate-file'.  Examples:
  /tmp/report.pdf      -> /tmp/report-copy.pdf
  /home/foo/.bashrc    -> /home/foo/.bashrc-copy
  doc.txt              -> doc-copy.txt
  /tmp/archive.tar.gz  -> /tmp/archive.tar-copy.gz"
  (let* ((dir (file-name-directory file))
         (base (file-name-base file))
         (ext (or (file-name-extension file t) ""))
         (new-name (concat base "-copy" ext)))
    (if dir
        (expand-file-name new-name dir)
      new-name)))

(defun cj/dirvish-duplicate-file ()
  "Duplicate the file at point with `-copy' suffix before the extension.
Examples:
  report.pdf → report-copy.pdf
  script.el → script-copy.el
  README → README-copy"
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (unless file
      (user-error "No file at point"))
    (when (file-directory-p file)
      (user-error "Cannot duplicate directories, only files"))
    (let* ((new-path (cj/--duplicate-file-name file))
           (new-name (file-name-nondirectory new-path)))
      (when (file-exists-p new-path)
        (unless (y-or-n-p (format "File '%s' already exists. Overwrite? " new-name))
          (user-error "Cancelled")))
      (copy-file file new-path t)
      (revert-buffer)
      (message "Duplicated: %s → %s"
               (file-name-nondirectory file) new-name))))

;;; ----------------------------- Dirvish Hard Delete ---------------------------

(defun cj/--dirvish-hard-delete-command (files)
  "Return the `sudo rm -rf' shell command that force-deletes FILES.
Each path is shell-quoted and the list is preceded by `--' so a
leading-dash filename can't be misread as an option.  Pure helper used by
`cj/dirvish-hard-delete'."
  (concat "sudo rm -rf -- "
          (mapconcat #'shell-quote-argument files " ")))

(defun cj/dirvish-hard-delete ()
  "Force-delete the marked files (or the file at point) via `sudo rm -rf'.
This bypasses the trash and is IRREVERSIBLE.  Prompts with the exact
targets named before running."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (unless files
      (user-error "No file at point"))
    (let ((targets (mapconcat #'file-name-nondirectory files ", ")))
      (when (yes-or-no-p
             (format "Force-delete (sudo rm -rf, NO undo): %s? " targets))
        (let ((status (shell-command (cj/--dirvish-hard-delete-command files))))
          ;; Revert either way so the listing reflects whatever was removed,
          ;; but only claim success when `rm' actually exited 0 -- a failed or
          ;; cancelled `sudo' must not report files gone that are still there.
          (revert-buffer)
          (if (zerop status)
              (message "Force-deleted: %s" targets)
            (message "Hard delete failed (exit %d) -- see *Shell Command Output*"
                     status)))))))

;;; ------------------------------ Dirvish Print File ---------------------------

(defvar cj/dirvish-print-extensions
  '("pdf" "ps" "eps" "txt" "text" "org" "md" "markdown" "log" "conf"
    "el" "py" "sh" "c" "h" "json" "yaml" "yml" "csv" "tex"
    "png" "jpg" "jpeg" "gif")
  "File extensions `cj/dirvish-print-file' will send to the printer.
Matched case-insensitively.  CUPS filters handle each of these directly,
so PDFs and images print without a separate dialog.")

(defun cj/--printable-file-p (file)
  "Return non-nil when FILE's extension is in `cj/dirvish-print-extensions'.
Match is case-insensitive; a file with no extension is not printable.
Pure helper used by `cj/dirvish-print-file'."
  (when-let* ((ext (file-name-extension file)))
    (and (member (downcase ext) cj/dirvish-print-extensions) t)))

(defun cj/--print-program ()
  "Return the CUPS print command (`lp' preferred, `lpr' as fallback), or nil."
  (or (executable-find "lp") (executable-find "lpr")))

(defun cj/dirvish-print-file ()
  "Print the file at point on the default printer via CUPS (`lp'/`lpr').
Refuses directories and file types not in `cj/dirvish-print-extensions'.
Shadows dired's `P' (`dired-do-print') with this type-aware version."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (unless file
      (user-error "No file at point"))
    (when (file-directory-p file)
      (user-error "Cannot print directories"))
    (unless (cj/--printable-file-p file)
      (user-error "Not a printable file type: %s" (file-name-nondirectory file)))
    (let ((program (or (cj/--print-program)
                       (user-error "No `lp' or `lpr' found — is CUPS installed?")))
          (name (file-name-nondirectory file)))
      (when (y-or-n-p (format "Print %s on the default printer? " name))
        (with-temp-buffer
          (let* ((code (call-process program nil t nil file))
                 (out (string-trim (buffer-string))))
            (if (zerop code)
                (message "Printing %s%s" name
                         (if (string-empty-p out) "" (concat " — " out)))
              (user-error "Print failed (exit %d)%s" code
                          (if (string-empty-p out) "" (concat ": " out))))))))))

;;; ------------------------------ Dirvish Drill File ---------------------------

(defun cj/dirvish-drill-file ()
  "Open the Org file at point and start an `org-drill' session on it.
Bound to `S' (\"study\") in `dirvish-mode-map'; refuses anything but
a `.org' file."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (unless (and file (not (file-directory-p file)) (string-suffix-p ".org" file t))
      (user-error "Not an Org file at point"))
    (find-file file)
    (cj/drill-this-file)))

;;; ----------------------- Dirvish Open File Manager Here ----------------------

(defun cj/dirvish-open-file-manager-here ()
  "Open system's default file manager in the current dired/dirvish directory.
Always opens the file manager in the directory currently being displayed,
regardless of what file or subdirectory the point is on."
  (interactive)
  (let ((current-dir (dired-current-directory)))
    (if (and current-dir (file-exists-p current-dir))
        (progn
          (message "Opening file manager in %s..." current-dir)
          ;; Use pipe instead of pty for the async call-process below.
          (let ((process-connection-type nil)
                (program (cj/external-open-command)))
            (if program
                (call-process program nil 0 nil current-dir)
              (shell-command (format "xdg-open %s &"
                                     (shell-quote-argument current-dir))))))
      (message "Could not determine current directory."))))

(defun cj/--wallpaper-program-for (env)
  "Return the (PROGRAM PRE-FILE-ARG...) list for setting wallpaper under ENV.

ENV is a display-server symbol: `x11' picks feh with --bg-fill, `wayland'
picks the `set-wallpaper' script (on PATH from dotfiles; it wraps the awww
backend and persists the choice to waypaper's config).  Any other value
returns nil so the caller can surface an \"unknown display server\" error.

Pure helper used by `cj/set-wallpaper'."
  (pcase env
    ('x11     '("feh" "--bg-fill"))
    ('wayland '("set-wallpaper"))
    (_ nil)))

(defun cj/set-wallpaper ()
  "Set the image at point as the desktop wallpaper.
Uses feh on X11, the `set-wallpaper' script on Wayland."
  (interactive)
  (let* ((raw (dired-file-name-at-point))
         (file (and raw (expand-file-name raw)))
         (env (cond ((env-x11-p) 'x11)
                    ((env-wayland-p) 'wayland)
                    (t nil)))
         (cmd (cj/--wallpaper-program-for env)))
    (unless file
      (user-error "No file at point"))
    (if (null cmd)
        (message "Unknown display server (not X11 or Wayland)")
      (when-let ((path (cj/executable-find-or-warn
                        (car cmd) "wallpaper setter" 'dirvish-config)))
        (apply #'call-process path nil 0 nil
               (append (cdr cmd) (list file)))
        (message "Wallpaper set: %s (%s)"
                 (file-name-nondirectory file) (car cmd))))))

;;; ------------------------- Dirvish Hyprland Popup ----------------------------

;; The Hyprland Super+F popup opens an emacsclient frame named "dirvish" (window
;; rules float/size/center it by that name) and runs `cj/dirvish-popup', rooted
;; at home.  `q' in that frame runs `cj/dirvish-popup-quit', which quits Dirvish
;; and deletes the popup frame so a stray launch never orphans it; `q' in any
;; other frame quits Dirvish normally.  The launcher script calls this command
;; instead of plain `dirvish'.  This mirrors the Super+N quick-capture
;; popup (see `cj/quick-capture' in org-capture-config.el).

(defun cj/--dirvish-popup-frame ()
  "Return a live frame named \"dirvish\" (the Hyprland popup), or nil."
  (seq-find (lambda (f)
              (and (frame-live-p f)
                   (equal (frame-parameter f 'name) "dirvish")))
            (frame-list)))

(defun cj/dirvish-popup ()
  "Open Dirvish in the Hyprland popup frame (frame \"dirvish\"), rooted at home.
The launcher script calls this through =emacsclient -c -e=.  `q'
(`cj/dirvish-popup-quit') closes the frame.

Selects the \"dirvish\" frame by name before opening rather than trusting the
ambient selected frame: the launching =emacsclient -c -e= runs before Hyprland
settles focus on the new float, so =(selected-frame)= is still the daemon's main
frame and Dirvish would otherwise open there."
  (interactive)
  (let ((frame (cj/--dirvish-popup-frame)))
    (when frame (select-frame-set-input-focus frame))
    (dirvish (expand-file-name "~/"))))

(defun cj/dirvish-popup-focus-existing ()
  "Raise and focus the live dirvish popup frame, returning t; nil if none.
The launcher script calls this before creating a frame, so a second Super+F
re-uses the open popup instead of spawning a second one (the popup is a
single-instance, transient launcher -- use =C-x d= for several independent
Dirvish sessions)."
  (let ((popup (cj/--dirvish-popup-frame)))
    (when popup
      (select-frame-set-input-focus popup)
      t)))

(defun cj/dirvish-popup-quit ()
  "Quit Dirvish.  In the Hyprland popup frame (\"dirvish\"), delete the frame too.
Bound to `q' in `dirvish-mode-map'.  A normal Dirvish session (any other frame)
quits as usual; only the popup frame is torn down, so the Super+F launch never
leaves an empty frame behind."
  (interactive)
  (let ((popup (cj/--dirvish-popup-frame)))
    (if (and popup (eq popup (selected-frame)))
        (progn
          (ignore-errors (dirvish-quit))
          (when (frame-live-p popup) (delete-frame popup)))
      (dirvish-quit))))

(defun cj/--dirvish-popup-reap-on-delete (frame)
  "Quit the Dirvish session when the Super+F popup FRAME is closed any way.
`q' runs `cj/dirvish-popup-quit', but closing the Hyprland float directly (or
letting it lose focus) bypasses that and orphans the session's dired buffers --
the \"leaves a load of buffers around\" symptom.  As a `delete-frame-functions'
hook this fires on every close path; `dirvish-quit' reaps the session's buffers
(verified: a navigated session drops back to baseline on quit).  Scoped to the
popup frame so ordinary `C-x d' sessions -- where multiple dired buffers are
wanted for mark-and-move -- are untouched."
  (when (and (frame-live-p frame)
             (equal (frame-parameter frame 'name) "dirvish"))
    (with-selected-frame frame
      (ignore-errors (dirvish-quit)))))

(add-hook 'delete-frame-functions #'cj/--dirvish-popup-reap-on-delete)

(defun cj/--dirvish-popup-selected-p ()
  "Return non-nil when the selected frame is the dirvish popup frame."
  (let ((popup (cj/--dirvish-popup-frame)))
    (and popup (eq popup (selected-frame)))))

(defun cj/dirvish-popup-find-file ()
  "Open the file at point.
In the Hyprland popup frame the popup is a context-free launcher: files open
through the OS handler (`cj/xdg-open' -> xdg-open), so nothing lands inside the
throwaway frame and the launch is independent of the running Emacs session (a
text/code file opens its own new emacsclient frame, not your working session --
use =C-x d= when you want a file in the session you're in).  Directories are
entered normally so you can keep browsing.  The popup then dismisses itself on
focus loss.  Outside the popup this is exactly `dired-find-file'."
  (interactive)
  (if (cj/--dirvish-popup-selected-p)
      (let ((file (dired-get-file-for-visit)))
        (if (file-directory-p file)
            (dired-find-file)
          (cj/xdg-open file)))
    (dired-find-file)))

(defun cj/--dirvish-popup-focus-watch (&rest _)
  "Dismiss the dirvish popup frame once it loses focus.
Armed only after the popup has actually held focus (a per-frame flag), so the
frame is never torn down during its own creation, before Hyprland settles focus
on the new float.  Installed on `after-focus-change-function'; a no-op whenever
no popup frame is live."
  (let ((popup (cj/--dirvish-popup-frame)))
    (when popup
      (if (frame-focus-state popup)
          (set-frame-parameter popup 'cj-dirvish-popup-had-focus t)
        (when (frame-parameter popup 'cj-dirvish-popup-had-focus)
          (delete-frame popup))))))

;; Install idempotently: remove any prior copy before adding, so re-loading the
;; module updates the watch rather than stacking duplicate copies.
(remove-function after-focus-change-function #'cj/--dirvish-popup-focus-watch)
(add-function :after after-focus-change-function #'cj/--dirvish-popup-focus-watch)

;;; ---------------------------------- Dirvish ----------------------------------

(use-package dirvish
  :defer 0.5
  :init
  (dirvish-override-dired-mode)
  :custom
  ;; This MUST be in :custom section, not :config
  (dirvish-quick-access-entries
   `(("h"  "~/"                                             "home")
     ("cx" ,code-dir                                        "code directory")
     ("ex" ,user-emacs-directory                            "emacs home")
     ("es" ,sounds-dir                                      "notification sounds")
     ("ra" ,video-recordings-dir                            "video recordings")
     ("rv" ,audio-recordings-dir                            "audio recordings")
     ("dl" ,dl-dir                                          "downloads")
     ("dr" ,(concat org-dir "/drill/")                      "drill files")
     ("dt" ,(concat dl-dir "/torrents/complete/")           "torrents")
     ("dx" "~/documents/"                                   "documents")
     ("db" "~/documents/dropbox/"                           "dropbox")
     ("gd" "~/documents/google-drive/"                      "google-drive")
     ("lx" "~/archive/lectures/"                            "lectures")
     ("mb" "/media/backup/"                                 "backup directory")
     ("mx" "~/music/"                                       "music")
     ("pdx" "~/projects/home/documents/"                    "documents area")
     ("pdl" "~/projects/home/danneel/"                      "project danneel")
     ("pcl" "~/projects/home/clipper/"                       "clipper area")
     ("pwk" "~/projects/work/"                               "project work")
     ("pl" "~/projects/home/elibrary/"                      "elibrary area")
     ("pf" "~/projects/home/finances/"                      "project finances")
     ("pjr" "~/projects/home/jr-estate/"                    "project jr-estate")
     ("phx" "~/projects/home/health/"                       "health area")
     ("phl" "~/projects/home/"                              "project home")
     ("pk" "~/projects/home/kit/"                           "kit area")
     ("pn" "~/projects/nextjob/"                            "project nextjob")
     ("ps" ,(concat pix-dir "/screenshots/")                "pictures screenshots")
     ("px" ,pix-dir                                         "pictures directory")
     ("wp" ,(concat pix-dir "/wallpaper/")                  "pictures wallpaper")
     ("rcj" "/sshx:cjennings@cjennings.net:~"               "remote c@cjennings.net")
     ("rtl" "/sshx:cjennings@truenas.local:~"               "remote cjennings@truenas.local")
     ("rtt" "/sshx:cjennings@truenas:~"                      "remote cjennings@truenas (tailscale)")
     ("rbk" "/sshx:cjennings@truenas.local:/mnt/vault/backups/" "remote truenas backups")
     ("rcg" "/sshx:git@cjennings.net:~"                     "remote git@cjennings.net")
     ("rsb" "/sshx:cjennings@wolf.usbx.me:/home/cjennings/" "remote seedbox")
     ("sx" ,sync-dir                                        "sync directory")
     ("so" ,(concat sync-dir "/org/")                       "sync/org directory")
     ("sr" ,(concat sync-dir "/recordings/")                "sync/recordings directory")
     ("spv" ,(concat sync-dir "/phone/videos/")             "sync/phone/videos directory")
     ("tg" ,(concat org-dir "/text.games/")                 "text games")
     ("vr" ,video-recordings-dir                            "video recordings directory")
     ("vx" ,videos-dir                                      "videos")))
  :config
  ;; Add the extensions directory to load-path
  (let ((extensions-dir (expand-file-name "extensions"
                                          (file-name-directory (locate-library "dirvish")))))
    (when (file-directory-p extensions-dir)
      (add-to-list 'load-path extensions-dir)))

  ;; Load dirvish modules with error checking
  (let ((dirvish-modules '(dirvish-emerge
                           dirvish-subtree
                           dirvish-narrow
                           dirvish-history
                           dirvish-ls
                           dirvish-yank
                           dirvish-quick-access
                           dirvish-collapse
                           dirvish-rsync
                           dirvish-vc
                           dirvish-icons
                           dirvish-side
                           dirvish-peek)))
    (dolist (module dirvish-modules)
      (condition-case err
          (require module)
        (error
         (message "Failed to load %s: %s" module (error-message-string err))))))

  ;; Enable peek mode with error checking
  (condition-case err
      (dirvish-peek-mode 1)
    (error (message "Failed to enable dirvish-peek-mode: %s" (error-message-string err))))

  ;; Enable side-follow mode with error checking
  (condition-case err
      (dirvish-side-follow-mode 1)
    (error (message "Failed to enable dirvish-side-follow-mode: %s"
                    (error-message-string err))))

  ;; Your other configuration settings
  (setq dirvish-attributes '(nerd-icons file-size))
  (setq dirvish-side-attributes '(nerd-icons file-size))  ;; Explicitly set for sidebar
  (setq dirvish-preview-dispatchers '(image gif video audio epub pdf archive))
  (setq dirvish-use-mode-line nil)
  (setq dirvish-use-header-line nil)
  :bind
  (("C-x d"   . dirvish)
   ("C-x C-d" . dirvish)
   ("C-x D"   . dirvish)
   ("<f11>"   . dirvish-side)
   :map dirvish-mode-map
   ("bg"      . cj/set-wallpaper)
   ("/"       . dirvish-narrow)
   ("<left>"  . dired-up-directory)
   ("RET"     . cj/dirvish-popup-find-file)   ; popup: launch file externally; else normal
   ("<right>" . cj/dirvish-popup-find-file)
   ("C-,"     . dirvish-history-go-backward)
   ("C-."     . dirvish-history-go-forward)
   ("F"       . dirvish-file-info-menu)
   ("G"       . revert-buffer)
   ("h"       . cj/dirvish-open-html-in-eww)  ;; it does what it says it does
   ("l"       . (lambda () (interactive) (cj/dired-copy-path-as-kill t nil))) ;; copy as org-link, relative path
   ("M"       . cj/dired-mark-all-visible-files)
   ("M-e"     . dirvish-emerge-menu)
   ("M-l"     . dirvish-ls-switches-menu)
   ("M-m"     . dirvish-mark-menu)
   ("M-p"     . dirvish-peek-toggle)
   ("M-s"     . dirvish-setup-menu)
   ("TAB"     . dirvish-subtree-toggle)
   ("d"       . cj/dirvish-duplicate-file)
   ("D"       . cj/dirvish-hard-delete)
   ("f"       . cj/dirvish-open-file-manager-here)
   ("g"       . dirvish-quick-access)
   ("o"       . cj/xdg-open)
   ("O"       . cj/open-file-with-command)  ; Prompts for command to run
   ("p"       . (lambda () (interactive) (cj/dired-copy-path-as-kill nil t)))
   ("P"       . cj/dirvish-print-file)
   ("q"       . cj/dirvish-popup-quit)   ; quit; in the Hyprland popup frame, close it
   ("r"       . dirvish-rsync)
   ("S"       . cj/dirvish-drill-file)  ; Study: org-drill the .org file at point
   ("s"       . dirvish-quicksort)
   ("v"       . dirvish-vc-menu)
   ("y"       . dirvish-yank-menu)))

;;; ----------------------------- Dired Text Greying ----------------------------

;; `default' is remapped buffer-locally to `shadow' inside dired/dirvish (see
;; `cj/--dired-text-greyout' below) so plain files read grey, with icon color
;; the only accent.  The dired text faces themselves are left to the theme.

(defun cj/--dired-text-greyout ()
  "Buffer-local: render `default' in `shadow' so plain files read grey."
  (face-remap-add-relative 'default 'shadow))

(add-hook 'dired-mode-hook #'cj/--dired-text-greyout)

;;; ---------------------------- Dired Hide Dotfiles ----------------------------

(use-package dired-hide-dotfiles
  :after dired
  :hook
  ;; Auto-hide dotfiles when entering dired/dirvish
  ((dired-mode . dired-hide-dotfiles-mode)
   (dirvish-mode . dired-hide-dotfiles-mode))
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode)))

;; --------------------------------- Copy Path ---------------------------------

(defun cj/--dired-resolve-display-path (file project-root home-dir
                                             &optional force-absolute)
  "Return a (PATH . PATH-TYPE) cons describing how to display FILE.

PATH-TYPE is one of \"absolute\", \"project-relative\", or \"home-relative\".

Resolution order: FORCE-ABSOLUTE wins over everything; otherwise an active
PROJECT-ROOT yields a project-relative path; otherwise a file under
HOME-DIR yields a `~/'-prefixed home-relative path (or the bare \"~\"
glyph when FILE is the home dir itself); otherwise the absolute FILE.

Pure helper used by `cj/dired-copy-path-as-kill'."
  (cond
   (force-absolute
    (cons file "absolute"))
   (project-root
    (cons (file-relative-name file project-root) "project-relative"))
   ((string-prefix-p home-dir file)
    (let ((relative-from-home (file-relative-name file home-dir)))
      (cons (if (string= relative-from-home ".")
                "~"
              (concat "~/" relative-from-home))
            "home-relative")))
   (t
    (cons file "absolute"))))

(defun cj/dired-copy-path-as-kill (&optional as-org-link force-absolute)
  "Copy path of file at point in Dired/Dirvish.
Copies relative path from project root if in a project, otherwise from home
directory (with ~ prefix) if applicable, otherwise the absolute path.
With prefix arg or when AS-ORG-LINK is non-nil, format as \='org-mode\=' link.
When FORCE-ABSOLUTE is non-nil, always copy the absolute path."
  (interactive "P")
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a Dired buffer"))
  (let ((file (dired-get-filename nil t)))
    (unless file
      (user-error "No file at point"))
    (let* ((file-name (file-name-nondirectory file))
           (resolved (cj/--dired-resolve-display-path
                      file (cj/get-project-root)
                      (expand-file-name "~") force-absolute))
           (path (car resolved))
           (path-type (cdr resolved))
           (output (if as-org-link
                       (format "[[file:%s][%s]]" path file-name)
                     path)))
      (kill-new output)
      (message "Copied %s path%s: %s"
               path-type
               (if as-org-link " as org-link" "")
               (if (> (length output) 60)
                   (concat (substring output 0 57) "...")
                 output)))))

(defun cj/get-project-root ()
  "Get project root using projectile or project.el.
Returns nil if not in a project."
  (cond
   ;; Try projectile first if available
   ((and (fboundp 'projectile-project-root)
         (ignore-errors (projectile-project-root))))

   ;; Fallback to project.el
   ((and (fboundp 'project-current)
         (project-current))
    (let ((proj (project-current)))
      (if (fboundp 'project-root)
          (project-root proj)
        ;; Compatibility with older versions
        (car (project-roots proj)))))

   ;; No project found
   (t nil)))



(provide 'dirvish-config)
;;; dirvish-config.el ends here.

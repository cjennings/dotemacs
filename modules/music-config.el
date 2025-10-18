;;; music-config.el --- EMMS configuration with MPD integration -*- coding: utf-8; lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Comprehensive music management in Emacs via EMMS with MPD backend.
;; Focus: simple, modular helpers; consistent error handling; streamlined UX.
;;
;; Highlights:
;; - Fuzzy add: select files/dirs; dirs have trailing /; case-insensitive; stable order
;; - Recursive directory add
;; - Dired/Dirvish integration (add selection)
;; - M3U playlist save/load/edit/reload
;; - Radio station M3U creation
;; - Playlist window toggling
;; - MPD as player
;;
;; Enhancements applied:
;; 1) Reorganized/grouped functions; unified helpers
;; 3) Standardized error handling (user-error for user-facing errors; consistent messages)
;; 4) Removed redundant wrappers in favor of binding to EMMS functions directly
;;
;; Bug fixes:
;; - cj/music-playlist-edit validates file, and always opens it correctly even after save prompt
;; - Keep cj/music-playlist-file in sync even when emms-playlist-clear is called elsewhere
;;
;;; Code:

(eval-when-compile (require 'emms))
(eval-when-compile (require 'emms-source-playlist))
(eval-when-compile (require 'emms-setup))
(eval-when-compile (require 'emms-player-mpd))
(eval-when-compile (require 'emms-playlist-mode))
(eval-when-compile (require 'emms-source-file))
(eval-when-compile (require 'emms-source-playlist))

(require 'cl-lib)
(require 'subr-x)

;;; Settings (no Customize)

(defvar cj/music-root (expand-file-name "~/music")
  "Root directory of your music collection.")

(defvar cj/music-m3u-root cj/music-root
  "Directory where M3U playlists are saved and loaded.")

(defvar cj/music-keymap-prefix (kbd "C-; m")
  "Prefix keybinding for all music commands. Currently not auto-bound.")

(defvar cj/music-file-extensions '("aac", "flac", "m4a", "mp3", "ogg", "opus", "wav")
  "List of valid music file extensions.")

(defvar cj/music-playlist-buffer-name "*EMMS-Playlist*"
  "Name of the EMMS playlist buffer used by this configuration.")

;;; Buffer-local state

(defvar-local cj/music-playlist-file nil
  "M3U file associated with the current playlist buffer. Set on load/save.")

;;; Helpers: file/dir/m3u/playlist

(defun cj/music--valid-file-p (file)
  "Return non-nil if FILE has an accepted music extension (case-insensitive)."
  (when-let ((ext (file-name-extension file)))
    (member (downcase ext) cj/music-file-extensions)))

(defun cj/music--valid-directory-p (dir)
  "Return non-nil if DIR is a non-hidden directory."
  (and (file-directory-p dir)
	   (not (string-prefix-p "." (file-name-nondirectory
								  (directory-file-name dir))))))

(defun cj/music--collect-entries-recursive (root)
  "Return sorted relative paths of all subdirs and music files under ROOT.
Directories are suffixed with /; files are plain. Hidden dirs/files skipped."
  (let ((base (file-name-as-directory root))
        (acc '()))
    (cl-labels ((collect (dir)
                  (when (cj/music--valid-directory-p dir)
                    (dolist (entry (directory-files dir t "^[^.].*" t))
                      (cond
                       ((cj/music--valid-directory-p entry)
                        (let ((rel (string-remove-prefix base entry)))
                          (push (concat rel "/") acc))
                        (collect entry))
                       ((and (file-regular-p entry)
                             (cj/music--valid-file-p entry))
                        (push (string-remove-prefix base entry) acc)))))))
      (collect base))
    (sort acc #'string-lessp)))

(defun cj/music--completion-table (candidates)
  "Completion table for CANDIDATES preserving order and case-insensitive match."
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata
          (display-sort-function . identity)
          (cycle-sort-function . identity)
          (completion-ignore-case . t))
      (complete-with-action action candidates string pred))))

(defun cj/music--ensure-playlist-buffer ()
  "Ensure EMMS playlist buffer exists and is in playlist mode. Return buffer."
  (let ((buffer (get-buffer-create cj/music-playlist-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'emms-playlist-mode)
        (emms-playlist-mode)))
    buffer))

(defun cj/music--m3u-file-tracks (m3u-file)
  "Return list of absolute track paths from M3U-FILE. Ignore # comment lines."
  (when (and m3u-file (file-exists-p m3u-file))
    (with-temp-buffer
      (insert-file-contents m3u-file)
      (let ((dir (file-name-directory m3u-file))
            (tracks '()))
        (goto-char (point-min))
        (while (re-search-forward "^[^#].*$" nil t)
          (let ((line (string-trim (match-string 0))))
            (unless (string-empty-p line)
              (push (if (or (file-name-absolute-p line)
                            (string-match-p "\`\(https?\|mms\)://" line))
                        line
                      (expand-file-name line dir))
                    tracks))))
        (nreverse tracks)))))

(defun cj/music--playlist-tracks ()
  "Return list of track names from current EMMS playlist buffer."
  (let ((tracks '()))
    (with-current-buffer (cj/music--ensure-playlist-buffer)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when-let ((track (emms-playlist-track-at (point))))
            (push (emms-track-name track) tracks))
          (forward-line 1))))
    (nreverse tracks)))

(defun cj/music--get-m3u-files ()
  "Return list of (BASENAME . FULLPATH) conses for M3Us in cj/music-m3u-root."
  (let ((files (directory-files cj/music-m3u-root t "\\.m3u\\'" t)))
    (mapcar (lambda (f) (cons (file-name-nondirectory f) f)) files)))

(defun cj/music--get-m3u-basenames ()
  "Return list of M3U basenames (no extension) in cj/music-m3u-root."
  (mapcar (lambda (pair) (file-name-sans-extension (car pair)))
          (cj/music--get-m3u-files)))

(defun cj/music--safe-filename (name)
  "Return NAME made filesystem-safe by replacing bad chars with underscores."
  (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" name))

(defun cj/music--playlist-modified-p ()
  "Return non-nil if current playlist differs from its associated M3U file."
  (and cj/music-playlist-file
       (let ((file-tracks (cj/music--m3u-file-tracks cj/music-playlist-file))
             (current-tracks (cj/music--playlist-tracks)))
         (not (equal file-tracks current-tracks)))))

(defun cj/music--assert-valid-playlist-file ()
  "Assert that the current playlist buffer has a valid associated M3U file.
Signals user-error if missing or deleted."
  (with-current-buffer (cj/music--ensure-playlist-buffer)
    (cond
     ((not cj/music-playlist-file)
      (user-error "No playlist file associated; playlist exists only in memory"))
     ((not (file-exists-p cj/music-playlist-file))
      (user-error "Playlist file no longer exists: %s"
                  (file-name-nondirectory cj/music-playlist-file))))))

;;; Commands: add/select

;;;###autoload
(defun cj/music-add-directory-recursive (directory)
  "Add all music files under DIRECTORY recursively to the EMMS playlist."
  (interactive
   (list (read-directory-name "Add directory recursively: " cj/music-root nil t)))
  (unless (file-directory-p directory)
    (user-error "Not a directory: %s" directory))
  (emms-add-directory-tree directory)
  (message "Added recursively: %s" directory))

;;;###autoload
(defun cj/music-fuzzy-select-and-add ()
  "Select a music file or directory and add to EMMS playlist.
Directories (trailing /) are added recursively; files added singly."
  (interactive)
  (let* ((completion-ignore-case t)
         (candidates (cj/music--collect-entries-recursive cj/music-root))
         (choice-rel (completing-read "Choose music file or directory: "
                                      (cj/music--completion-table candidates)
                                      nil t))
         (cleaned (if (string-suffix-p "/" choice-rel)
                      (substring choice-rel 0 -1)
                    choice-rel))
         (abs (expand-file-name cleaned cj/music-root)))
    (if (file-directory-p abs)
        (cj/music-add-directory-recursive abs)
      (emms-add-file abs))
    (message "Added to playlist: %s" choice-rel)))

;;; Commands: playlist management (load/save/clear/reload/edit)

;;;###autoload
(defun cj/music-playlist-load ()
  "Load an M3U playlist from cj/music-m3u-root.
Replaces current playlist."
  (interactive)
  (let* ((pairs (cj/music--get-m3u-files)))
    (when (null pairs)
      (user-error "No M3U files found in %s" cj/music-m3u-root))
    (let* ((choice-name (completing-read "Select playlist: " (mapcar #'car pairs) nil t))
           (choice-file (cdr (assoc choice-name pairs))))
      (unless (and choice-file (file-exists-p choice-file))
        (user-error "Playlist file does not exist: %s" choice-name))
      (emms-playlist-clear)
      (emms-play-playlist choice-file)
      (with-current-buffer (cj/music--ensure-playlist-buffer)
        (setq cj/music-playlist-file choice-file))
      (message "Loaded playlist: %s" choice-name))))

;;;###autoload
(defun cj/music-playlist-save ()
  "Save current EMMS playlist to a file in cj/music-m3u-root.
Offers completion over existing names but allows new names."
  (interactive)
  (let* ((existing (cj/music--get-m3u-basenames))
         (default-name (if cj/music-playlist-file
                           (file-name-sans-extension (file-name-nondirectory cj/music-playlist-file))
                         (format-time-string "playlist-%Y%m%d-%H%M%S")))
         (chosen (completing-read "Save playlist as: " existing nil nil nil nil default-name))
         (filename (if (string-suffix-p ".m3u" chosen) chosen (concat chosen ".m3u")))
         (full (expand-file-name filename cj/music-m3u-root)))
    (when (and (file-exists-p full)
               (not (yes-or-no-p (format "Overwrite %s? " filename))))
      (user-error "Aborted saving playlist"))
    (with-current-buffer (cj/music--ensure-playlist-buffer)
      (let ((emms-source-playlist-ask-before-overwrite nil))
        (emms-playlist-save 'm3u full))
      (setq cj/music-playlist-file full))
    (message "Saved playlist: %s" filename)))

;;;###autoload
(defun cj/music-playlist-clear ()
  "Stop playback and empty the playlist."
  (interactive)
  (emms-stop)
  (emms-playlist-clear)
  ;; Advice also clears, but do it eagerly for this command
  (with-current-buffer (cj/music--ensure-playlist-buffer)
    (setq cj/music-playlist-file nil))
  (message "Playlist cleared"))

;;;###autoload
(defun cj/music-playlist-reload ()
  "Reload current playlist from its associated M3U file."
  (interactive)
  (cj/music--assert-valid-playlist-file)
  (let* ((file-path (with-current-buffer (cj/music--ensure-playlist-buffer)
                      cj/music-playlist-file))
         (name (file-name-nondirectory file-path)))
    (emms-playlist-clear)
    (emms-play-playlist file-path)
    (with-current-buffer (cj/music--ensure-playlist-buffer)
      (setq cj/music-playlist-file file-path))
    (message "Reloaded playlist: %s" name)))

;;;###autoload
(defun cj/music-playlist-edit ()
  "Open the playlist's M3U file in other window, prompting to save if modified."
  (interactive)
  (cj/music--assert-valid-playlist-file)
  (with-current-buffer (cj/music--ensure-playlist-buffer)
    (let ((path cj/music-playlist-file))
      (when (cj/music--playlist-modified-p)
        (when (yes-or-no-p "Playlist modified. Save before editing? ")
          (let ((emms-source-playlist-ask-before-overwrite nil))
            (emms-playlist-save 'm3u path))))
      ;; Re-validate existence before opening
      (if (file-exists-p path)
          (find-file-other-window path)
        (user-error "Playlist file no longer exists: %s"
                    (file-name-nondirectory path))))))

;;; Commands: UI

;;;###autoload
(defun cj/music-playlist-toggle ()
  "Toggle the EMMS playlist buffer in a right side window."
  (interactive)
  (let* ((buf-name cj/music-playlist-buffer-name)
         (buffer (get-buffer buf-name))
         (win (and buffer (get-buffer-window buffer))))
    (if win
        (progn
          (delete-window win)
          (message "Playlist window closed"))
      (progn
        (cj/emms--setup)
        (setq buffer (cj/music--ensure-playlist-buffer))
        (setq win (display-buffer-in-side-window buffer '((side . right) (window-width . 0.35))))
        (select-window win)
        (with-current-buffer buffer
          (if (and (fboundp 'emms-playlist-current-selected-track)
                   (emms-playlist-current-selected-track))
              (emms-playlist-mode-center-current)
            (goto-char (point-min))))
        (let ((count (with-current-buffer buffer
                       (count-lines (point-min) (point-max)))))
          (message (if (> count 0)
                       (format "Playlist displayed (%d tracks)" count)
                     "Playlist displayed (empty)")))))))

;;;###autoload
(defun cj/music-playlist-show ()
  "Show the EMMS playlist buffer in the current window.
Initializes EMMS if needed."
  (interactive)
  (let ((emms-was-loaded (featurep 'emms))
        (buffer-exists (get-buffer cj/music-playlist-buffer-name))
        (has-content nil))
    (cj/emms--setup)
    (when buffer-exists
      (with-current-buffer cj/music-playlist-buffer-name
        (setq has-content (> (point-max) (point-min)))))
    (switch-to-buffer (cj/music--ensure-playlist-buffer))
    (cond
     ((not emms-was-loaded) (message "EMMS started. Current playlist empty"))
     ((and buffer-exists has-content) (message "EMMS running. Displaying current playlist"))
     (t (message "EMMS running. Current playlist empty")))))

;;; Dired/Dirvish integration

(with-eval-after-load 'dirvish
  (defun cj/music-add-dired-selection ()
	"Add selected files/dirs in Dired/Dirvish to the EMMS playlist.
Dirs added recursively."
	(interactive)
	(unless (derived-mode-p 'dired-mode)
	  (user-error "This command must be run in a Dired buffer"))
	(let ((files (if (use-region-p)
					 (dired-get-marked-files)
				   (list (dired-get-file-for-visit)))))
	  (when (null files)
		(user-error "No files selected"))
	  (dolist (file files)
		(cond
		 ((file-directory-p file) (cj/music-add-directory-recursive file))
		 ((cj/music--valid-file-p file) (emms-add-file file))
		 (t (message "Skipping non-music file: %s" file))))
	  (message "Added %d item(s) to playlist" (length files))))

  (define-key dirvish-mode-map "p" #'cj/music-add-dired-selection))

;;; EMMS setup and keybindings

(use-package emms
  :defer t
  :init
  (defvar cj/music-map (make-sparse-keymap) "Keymap for music commands.")
  :commands (emms-mode-line-mode)
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (require 'emms-playlist-mode)
  (require 'emms-source-file)
  (require 'emms-source-playlist)

  (emms-all)

  ;; Use only mpd to play
  (add-to-list 'emms-player-list 'emms-player-mpd)

  ;; MPD configuration
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (setq emms-player-mpd-music-directory cj/music-root)
  (condition-case err
	  (emms-player-mpd-connect)
	(error (message "Failed to connect to MPD: %s" err)))

  ;; Basic EMMS configuration
  (setq emms-source-file-default-directory cj/music-root)
  (setq emms-playlist-buffer-name cj/music-playlist-buffer-name)
  (setq emms-playlist-default-major-mode 'emms-playlist-mode)

  ;; note setopt as variable is customizeable
  (setopt emms-player-mpd-supported-regexp
		 (apply #'emms-player-simple-regexp cj/music-file-extensions))

  ;; Keep cj/music-playlist-file in sync if playlist is cleared
  (defun cj/music--after-playlist-clear (&rest _)
	(when-let ((buf (get-buffer cj/music-playlist-buffer-name)))
	  (with-current-buffer buf
		(setq cj/music-playlist-file nil))))

  ;; Ensure we don't stack duplicate advice on reload
  (when (advice-member-p #'cj/music--after-playlist-clear 'emms-playlist-clear)
	(advice-remove 'emms-playlist-clear #'cj/music--after-playlist-clear))

  (advice-add 'emms-playlist-clear :after #'cj/music--after-playlist-clear)

  :bind-keymap
  ("C-; m" . cj/music-map)

  :bind
  (:map emms-playlist-mode-map
        ;; Playback
        ("p"   . emms-playlist-mode-go)
        ("SPC" . emms-pause)
        ("s"   . emms-stop)
        ("x"   . emms-shuffle)
        ("q"   . emms-playlist-mode-bury-buffer)
        ;; Manipulation
        ("a" . cj/music-fuzzy-select-and-add)
        ("C" . cj/music-playlist-clear)
        ("L" . cj/music-playlist-load)
		("e" . cj/music-playlist-edit)
        ("R" . cj/music-playlist-reload)
        ("S" . cj/music-playlist-save)
        ;; Track reordering (bind directly to EMMS commands; no wrappers)
        ("C-<up>"   . emms-playlist-mode-shift-track-up)
        ("C-<down>" . emms-playlist-mode-shift-track-down)
        ;; Radio
        ("r" . cj/music-create-radio-station)
        ;; Volume (MPD)
        ("-" . emms-volume-lower)
        ("=" . emms-volume-raise))
  (:map cj/music-map
        ("m" . cj/music-playlist-toggle)
        ("M" . cj/music-playlist-show)
        ("a" . cj/music-fuzzy-select-and-add)
        ("r" . cj/music-create-radio-station)
        ("SPC" . emms-pause)
        ("s" . emms-stop)
        ("p" . emms-playlist-mode-go)
        ("x" . emms-shuffle)))

;; Quick toggle key
(global-unset-key (kbd "<f10>"))
(global-set-key (kbd "<f10>") #'cj/music-playlist-toggle)

;;; Minimal ensure-loaded setup for on-demand use

(defun cj/emms--setup ()
  "Ensure EMMS is loaded and quiet in mode line."
  (unless (featurep 'emms)
	(require 'emms))

  (emms-playing-time-disable-display)
  (emms-mode-line-mode -1))

;;; Radio station creation

;;;###autoload
(defun cj/music-create-radio-station (name url)
  "Create a radio station M3U playlist with NAME and URL in cj/music-m3u-root."
  (interactive
   (list (read-string "Radio station name: ")
         (read-string "Stream URL: ")))
  (when (string-empty-p name)
    (user-error "Radio station name cannot be empty"))
  (when (string-empty-p url)
    (user-error "Stream URL cannot be empty"))
  (let* ((safe (cj/music--safe-filename name))
         (file (expand-file-name (concat safe "_Radio.m3u") cj/music-m3u-root))
         (content (format "#EXTM3U\n#EXTINF:-1,%s\n%s\n" name url)))
    (when (and (file-exists-p file)
               (not (yes-or-no-p (format "Overwrite %s? " (file-name-nondirectory file)))))
      (user-error "Aborted creating radio station"))
    (with-temp-file file
      (insert content))
    (message "Created radio station: %s" (file-name-nondirectory file))))

(provide 'music-config)
;;; music-config.el ends here

;;; music-config.el --- EMMS configuration with MPD integration -*- coding: utf-8; lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; This module provides a comprehensive music management system for Emacs using
;; EMMS (Emacs MultiMedia System) with MPD (Music Player Daemon) as the backend.
;; It streamlines music library navigation, playlist management, and playback
;; control within Emacs.
;;
;; Features:
;; - Fuzzy file/directory selection with case-insensitive, alphanumeric ordering
;; - Recursive directory addition to playlists
;; - Integration with Dired/Dirvish for adding files from file managers
;; - M3U playlist saving and loading for playlist portability
;; - Radio station URL to m3u playlist creation
;; - Track reordering within playlists
;; - MPD for playback control
;;
;; Setup:
;; 1. Ensure MPD is installed and running on your system
;; 2. Set `cj/music-root' to your music library directory (default: ~/music)
;; 3. Set `cj/music-m3u-root' to your playlist directory (default: ~/music)
;; 4. Customize `cj/music-file-extensions' if you use formats beyond the defaults
;;
;; Usage:
;; All music commands are accessed through the prefix `C-; m' by default.
;; - `C-; m m' - Show EMMS playlist buffer
;; - `C-; m a' - Add music via fuzzy search (files or directories)
;; - `C-; m l' - Load an existing M3U playlist
;; - `C-; m s' - Save current playlist as M3U
;; - `C-; m c' - Clear current playlist
;; - `C-; m r' - Create a radio station playlist
;; - `C-; m SPC' - Pause/resume playback
;;
;; When the playlist is active, omit the prefix and enter the keys directly.
;; Control + up and down arrows will reorder the playlist files.
;;
;;
;; The fuzzy search interface (`C-; m a') presents your music library in a
;; hierarchical view with directories marked by trailing slashes. Selection
;; maintains strict alphanumeric ordering even during narrowing, and matching
;; is case-insensitive. Selecting a directory adds all music files within it
;; recursively.
;;
;; Custom functions use the "cj/" namespace to avoid conflicts with built-in
;; EMMS functions. The configuration is designed to be testable, with core
;; functions defined separately from the use-package declaration.
;;
;; Requirements:
;; - MPD (Music Player Daemon) running on localhost:6600
;;
;; Custom functions are defined separately from the use-package declaration to facilitate unit testing.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; Custom Variables

(defgroup cj/music nil
  "Music configuration settings."
  :group 'multimedia)

(defcustom cj/music-root (expand-file-name "~/music")
  "Root directory of your music collection."
  :type 'directory
  :group 'cj/music)

(defcustom cj/music-m3u-root cj/music-root
  "Directory where M3U playlists are saved and loaded."
  :type 'directory
  :group 'cj/music)

(defcustom cj/music-keymap-prefix (kbd "C-; m")
  "Prefix keybinding for all music commands."
  :type 'key-sequence
  :group 'cj/music)

(defcustom cj/music-file-extensions '("flac" "mp3" "opus" "wav" "m4a" "aac" "ogg")
  "List of valid music file extensions."
  :type '(repeat string)
  :group 'cj/music)

;;; Local Variables

(defvar-local cj/music-playlist-file nil
  "The M3U file associated with the current playlist buffer.
Set when loading or saving a playlist.")

;;; Helper Functions

(defun cj/music--valid-file-p (file)
  "Return t if FILE is a music file with accepted extensions.
The check is case-insensitive."
  (when-let ((ext (file-name-extension file)))
	(member (downcase ext) cj/music-file-extensions)))

(defun cj/music--valid-directory-p (dir)
  "Return t if DIR is a directory and is not hidden.
Hidden directories are those starting with a dot."
  (and (file-directory-p dir)
	   (not (string-prefix-p "." (file-name-nondirectory (directory-file-name dir))))))

(defun cj/music--collect-entries-recursive (root)
  "Recursively collect all non-hidden directories and music files under ROOT.
Return a list of relative paths (from ROOT) sorted alphanumerically.
Directories and files are mixed and sorted together."
  (let ((base (file-name-as-directory root))
		(candidates '()))
	(cl-labels ((collect (dir)
				  (when (cj/music--valid-directory-p dir)
					(let ((entries (directory-files dir t "^[^.]" t)))
					  (dolist (entry (sort entries #'string-lessp))
						(cond
						 ((cj/music--valid-directory-p entry)
						  (push (string-remove-prefix base entry) candidates)
						  (collect entry))
						 ((and (file-regular-p entry)
							   (cj/music--valid-file-p entry))
						  (push (string-remove-prefix base entry) candidates))))))))
	  (collect base))
	(nreverse candidates)))

(defun cj/music--ensure-playlist-buffer ()
  "Ensure EMMS playlist buffer exists and is in playlist mode.
Returns the buffer or signals an error if it cannot be created."
  (let ((buffer (get-buffer-create "*EMMS Playlist*")))
	(with-current-buffer buffer
	  (unless (eq major-mode 'emms-playlist-mode)
		(emms-playlist-mode)))
	buffer))

;; Helper function to extract tracks from M3U file
(defun cj/music--m3u-file-tracks (m3u-file)
  "Extract a list of track filenames from M3U-FILE.
Returns a list of absolute paths."
  (when (file-exists-p m3u-file)
	(with-temp-buffer
	  (insert-file-contents m3u-file)
	  (let ((dir (file-name-directory m3u-file))
			(tracks '()))
		(goto-char (point-min))
		(while (re-search-forward "^[^#].*$" nil t)
		  (let ((track (match-string 0)))
			(unless (string-empty-p (string-trim track))
			  (push (if (or (file-name-absolute-p track)
							(string-match "\\=\\(https?\\|mms\\)://" track))
						track
					  (expand-file-name track dir))
					tracks))))
		(nreverse tracks)))))

;; Helper function to get current playlist tracks
(defun cj/music--playlist-tracks ()
  "Get list of track names from the current EMMS playlist buffer."
  (let ((tracks '()))
	(with-current-buffer (cj/music--ensure-playlist-buffer)
	  (save-excursion
		(goto-char (point-min))
		(while (not (eobp))
		  (when-let ((track (emms-playlist-track-at (point))))
			(push (emms-track-name track) tracks))
		  (forward-line 1))))
	(nreverse tracks)))

;;; Interactive Commands

;;;###autoload
(defun cj/music-add-directory-recursive (directory)
  "Add all music files under DIRECTORY recursively to the EMMS playlist.
DIRECTORY defaults to `cj/music-root' if called non-interactively."
  (interactive
   (list (read-directory-name "Add directory recursively: "
							  cj/music-root nil t)))
  (unless (file-directory-p directory)
	(user-error "Not a directory: %s" directory))
  (emms-add-directory-tree directory)
  (message "Added recursively: %s" directory))

(defun cj/music--collect-entries-recursive (root)
  "Recursively collect all non-hidden directories and music files under ROOT.
Return a list of relative paths (from ROOT) sorted alphanumerically.
Directories have trailing '/' and everything is sorted together."
  (let ((base (file-name-as-directory root))
		(candidates '()))
	(cl-labels ((collect (dir)
				  (when (cj/music--valid-directory-p dir)
					(let ((entries (directory-files dir t "^[^.]" t)))
					  (dolist (entry entries)
						(cond
						 ;; If it's a directory, add it with trailing / and recurse
						 ((cj/music--valid-directory-p entry)
						  (let ((rel-path (string-remove-prefix base entry)))
							(push (concat rel-path "/") candidates))
						  (collect entry))
						 ;; If it's a music file, add it
						 ((and (file-regular-p entry)
							   (cj/music--valid-file-p entry))
						  (push (string-remove-prefix base entry) candidates))))))))
	  (collect base))
	;; Sort all candidates together alphanumerically
	(sort candidates #'string-lessp)))

(defun cj/music--completion-table (candidates)
  "Create a completion table that maintains the order of CANDIDATES.
Provides case-insensitive matching while preserving sort order."
  (lambda (string pred action)
	(if (eq action 'metadata)
		'(metadata
		  (display-sort-function . identity)
		  (cycle-sort-function . identity)
		  (completion-ignore-case . t))
	  (complete-with-action action candidates string pred))))

;;;###autoload
(defun cj/music-fuzzy-select-and-add ()
  "Select a music file or directory using fuzzy completion and add to playlist.
Shows relative paths from =cj/music-root' with directories having trailing slashes.
Selecting a directory adds it recursively, selecting a file adds that single file.
Matching is case-insensitive."
  (interactive)
  ;; Ensure case-insensitive completion locally
  (let* ((completion-ignore-case t)
		 (candidates (cj/music--collect-entries-recursive cj/music-root))
		 (choice-rel (completing-read "Choose music file or directory: "
									  (cj/music--completion-table candidates)
									  nil t))
         (cleaned-choice (if (string-suffix-p "/" choice-rel)
							 (substring choice-rel 0 -1)
						   choice-rel))
		 (choice-abs (expand-file-name cleaned-choice cj/music-root)))
	(if (file-directory-p choice-abs)
		(cj/music-add-directory-recursive choice-abs)
	  (emms-add-file choice-abs))
	(message "Added %s to EMMS playlist" choice-rel)))

;;;###autoload
(defun cj/music-playlist-load ()
  "Select and load an M3U playlist file from =cj/music-m3u-root'.
Clears the current playlist before loading and tracks the source file."
  (interactive)
  (let* ((m3u-files (directory-files cj/music-m3u-root t "\\.m3u\\'" t))
		 (m3u-names (mapcar #'file-name-nondirectory m3u-files)))
	(when (null m3u-files)
	  (user-error "No M3U files found in %s" cj/music-m3u-root))
	(let* ((choice-name (completing-read "Select playlist: " m3u-names nil t))
		   (choice-file (expand-file-name choice-name cj/music-m3u-root)))
	  (unless (file-exists-p choice-file)
		(user-error "Playlist file does not exist: %s" choice-file))
	  (emms-playlist-clear)
	  (emms-play-playlist choice-file)
	  ;; Track the loaded file
	  (with-current-buffer (cj/music--ensure-playlist-buffer)
		(setq cj/music-playlist-file choice-file))
	  (message "Loaded playlist: %s" choice-name))))

;;;###autoload

(defun cj/music-playlist-save ()
  "Save the current EMMS playlist to a file in =cj/music-m3u-root'.
Offers existing playlist names for completion but allows entering new names.
Automatically adds .m3u extension if not present.
Tracks the saved file for future reference."
  (interactive)
  (let* ((m3u-files (directory-files cj/music-m3u-root nil "\\.m3u\\'" t))
		 (m3u-names-no-ext (mapcar (lambda (f)
									 (file-name-sans-extension f))
								   m3u-files))
         (chosen-name (completing-read "Save playlist as: "
									   m3u-names-no-ext
									   nil nil nil nil
									   (format-time-string "playlist-%Y%m%d-%H%M%S")))
		 (filename (if (string-suffix-p ".m3u" chosen-name)
					   chosen-name
					 (concat chosen-name ".m3u")))
		 (full-path (expand-file-name filename cj/music-m3u-root)))

	(when (and (file-exists-p full-path)
			   (not (yes-or-no-p (format "Overwrite %s? " filename))))
	  (user-error "Aborted saving playlist"))
	(let ((buffer (cj/music--ensure-playlist-buffer)))
	  (with-current-buffer buffer
		;; Use 'never to never prompt for overwrite since we already asked
		(let ((emms-source-playlist-ask-before-overwrite nil))
		  (emms-playlist-save 'm3u full-path))
		(setq cj/music-playlist-file full-path)))

	(message "Saved playlist to %s" filename)))

;;;###autoload
(defun cj/music-move-track-up ()
  "Move the current track one line up in the EMMS playlist buffer."
  (interactive)
  (with-current-buffer (cj/music--ensure-playlist-buffer)
	(emms-playlist-mode-move-up)))

;;;###autoload
(defun cj/music-move-track-down ()
  "Move the current track one line down in the EMMS playlist buffer."
  (interactive)
  (with-current-buffer (cj/music--ensure-playlist-buffer)
	(emms-playlist-mode-move-down)))

;;;###autoload
(defun cj/music-create-radio-station (name url)
  "Create a radio station M3U playlist file with NAME and URL.
The file is saved in `cj/music-m3u-root' as NAME.m3u.
Prompts before overwriting an existing file."
  (interactive
   (list (read-string "Radio station name: ")
		 (read-string "Stream URL: ")))
  (when (string-empty-p name)
	(user-error "Radio station name cannot be empty"))
  (when (string-empty-p url)
	(user-error "Stream URL cannot be empty"))
  (let* ((safe-name (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" name))
		 (file-path (expand-file-name (concat safe-name "_Radio.m3u") cj/music-m3u-root))
		 (content (format "#EXTM3U\n#EXTINF:-1,%s\n%s\n" name url)))
	(when (and (file-exists-p file-path)
			   (not (yes-or-no-p (format "File %s exists. Overwrite? "
										 (file-name-nondirectory file-path)))))
	  (user-error "Aborted creating radio station file"))
	(with-temp-file file-path
	  (insert content))
	(message "Created radio station playlist: %s" (file-name-nondirectory file-path))))

;;;###autoload
(defun cj/music-playlist-clear ()
  "Stops playing then empties the playlist."
  (interactive)
  (emms-stop)
  (emms-playlist-clear)
  (setq cj/music-playlist-file nil)
  (message "EMMS playlist cleared."))

(defun cj/music-playlist-reload ()
  "Reload the current playlist from its associated M3U file.
Clears the current playlist and reloads from disk without confirmation.
Errors if no file is associated or if the file doesn't exist."
  (interactive)
  (with-current-buffer (cj/music--ensure-playlist-buffer)
	(cond
	 ;; No file associated
	 ((not cj/music-playlist-file)
	  (user-error "No playlist file to reload - playlist exists only in memory"))

	 ;; File doesn't exist
	 ((not (file-exists-p cj/music-playlist-file))
	  (user-error "Playlist file no longer exists: %s"
				  (file-name-nondirectory cj/music-playlist-file)))

	 ;; Reload the playlist
	 (t
	  (let ((file-name (file-name-nondirectory cj/music-playlist-file)))
		(emms-playlist-clear)
		(emms-play-playlist cj/music-playlist-file)
		;; Restore the file association (emms-playlist-clear might have cleared it)
		(setq cj/music-playlist-file cj/music-playlist-file)
		(message "Reloaded playlist: %s" file-name))))))

;;;###autoload
(defun cj/music-playlist-edit ()
  "Open the playlist's M3U file in other window.
If the playlist has been modified, prompt to save first.
If no file is associated with the playlist, show an error."
  (interactive)
  (with-current-buffer (cj/music--ensure-playlist-buffer)
	(cond
	 ;; No file associated
	 ((not cj/music-playlist-file)
	  (message "Playlist not yet saved."))

	 ;; File doesn't exist (was deleted?)
	 ((not (file-exists-p cj/music-playlist-file))
	  (message "Playlist file no longer exists: %s"
			   (file-name-nondirectory cj/music-playlist-file)))

	 ;; Check for modifications
	 (t
	  (let ((file-tracks (cj/music--m3u-file-tracks cj/music-playlist-file))
			(current-tracks (cj/music--playlist-tracks)))
		(if (equal file-tracks current-tracks)
			;; No changes, open directly
			(find-file-other-window cj/music-playlist-file)
		  ;; Changes detected, prompt
		  (when (yes-or-no-p "Playlist has been modified. Save before editing? ")
			(emms-playlist-save 'm3u cj/music-playlist-file)
			(find-file-other-window cj/music-playlist-file))))))))

;;;###autoload
(defun cj/music-playlist-toggle ()
  "Toggle the visibility of the EMMS playlist buffer in a side window.
Opens the playlist in a right side window if not visible, or closes it if visible."
  (interactive)
  (let* ((buf-name "*EMMS Playlist*")
		 (buffer (get-buffer buf-name))
		 (win (and buffer (get-buffer-window buffer))))
	(if win
		;; Window exists, close it
		(progn
		  (delete-window win)
		  (message "EMMS Playlist window closed."))
	  ;; Window doesn't exist, create/show it
	  (progn
		;; Ensure EMMS is loaded
		(unless (featurep 'emms)
		  (require 'emms)
		  (require 'emms-setup)
		  (require 'emms-playlist-mode)
		  (emms-all)
		  (emms-default-players))

		;; Ensure playlist buffer exists
		(setq buffer (cj/music--ensure-playlist-buffer))

		;; Display in side window
		(setq win
			  (display-buffer-in-side-window
			   buffer
			   '((side . right)
				 (window-width . 0.35))))  ; Slightly narrower than AI window

		;; Select the window and move to appropriate position
		(select-window win)
		(with-current-buffer buffer
		  ;; If there's a current track, go to it; otherwise go to beginning
		  (if (and (fboundp 'emms-playlist-current-selected-track)
				   (emms-playlist-current-selected-track))
			  (emms-playlist-mode-center-current)
			(goto-char (point-min))))

		;; Provide feedback
		(let ((track-count (with-current-buffer buffer
							 (count-lines (point-min) (point-max)))))
		  (if (> track-count 0)
			  (message "EMMS Playlist displayed (%d tracks)." track-count)
			(message "EMMS Playlist displayed (empty).")))))))

;;;###autoload
(defun cj/music-playlist-show ()
  "Show the EMMS playlist buffer, initializing EMMS if necessary.
If EMMS is not loaded, loads it first. Switches to the playlist buffer
in the current window and provides appropriate feedback."
  (interactive)
  (let ((emms-was-loaded (featurep 'emms))
		(playlist-buffer-exists nil)
		(playlist-has-content nil))

	;; Load EMMS if not already loaded
	(unless emms-was-loaded
	  (require 'emms)
	  (require 'emms-setup)
	  (require 'emms-playlist-mode)
	  (emms-all)
	  (emms-default-players))

	;; Check if playlist buffer exists
	(when (get-buffer "*EMMS Playlist*")
	  (setq playlist-buffer-exists t)
	  (with-current-buffer "*EMMS Playlist*"
		(setq playlist-has-content (> (point-max) (point-min)))))

	;; Ensure playlist buffer exists and switch to it
	(switch-to-buffer (cj/music--ensure-playlist-buffer))

	;; Provide appropriate feedback
	(cond
	 ((not emms-was-loaded)
	  (message "EMMS started. Current Playlist empty."))
	 ((and playlist-buffer-exists playlist-has-content)
	  (message "EMMS running. Displaying Current Playlist."))
	 (t
	  (message "EMMS running. Current Playlist empty.")))))

;; ------------------------------- EMMS Settings -------------------------------

(use-package emms
  :defer t
  :init
  ;; Create music keymap before package loads
  (defvar cj/music-map (make-sparse-keymap)
	"Keymap for music commands.")

  :commands (emms-mode-line-mode)
  :config
  ;; Basic EMMS setup
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (require 'emms-playlist-mode)
  (require 'emms-source-file)
  (require 'emms-source-playlist)

  (emms-all)
  (emms-default-players)

  ;; MPD configuration
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost"
		emms-player-mpd-server-port "6600"
		emms-player-mpd-music-directory cj/music-root)

  ;; EMMS settings
  (setq emms-source-file-default-directory cj/music-root
		emms-playlist-buffer-name "*EMMS Playlist*"
		emms-playlist-default-major-mode 'emms-playlist-mode)

  ;; modeline shows nothing
  (emms-playing-time-disable-display)
  (emms-mode-line-mode -1)

  ;; if mpv, don't display album art (interruptive)
  (add-to-list 'emms-player-mpv-parameters "--no-audio-display")

  ;; Start MPD connection
  (condition-case err
	  (emms-player-mpd-connect)
	(error (message "Failed to connect to MPD: %s" err)))

  :bind-keymap
  ("C-; m" . cj/music-map)

  :bind
  (:map emms-playlist-mode-map

		;; playlist playing
		("p"   . emms-playlist-mode-go)                    ;; start playing the playlist
		("SPC" . emms-pause)                               ;; pause playing the playlist
		("s"   . emms-stop)                                ;; stop playing the playlist
		("x"   . emms-shuffle)                             ;; shuffle the playlist
		("q"   . emms-playlist-mode-bury-buffer)           ;; quit the playlist

		;; playlist maniuplation
		("a" . cj/music-fuzzy-select-and-add)              ;; add to playlist
		("C" . cj/music-playlist-clear)                    ;; clear playlist
		("L" . cj/music-playlist-load)                     ;; load an existing playlist
		("e" . cj/music-playlist-edit)                     ;; edit an existing playlist
		("R" . cj/music-playlist-reload)                   ;; reload an existing playlist
		("S" . cj/music-playlist-save)                     ;; save current playlist

		;; playlist track reordering
		("C-<up>"   . emms-playlist-mode-shift-track-up)   ;; move track earlier
		("C-<down>" . emms-playlist-mode-shift-track-down) ;; move track later

		;; Create Radio station m3u
		("r" . cj/music-create-radio-station)

		;; Volume controls (MPD)
		("-" . emms-volume-lower)
		("=" . emms-volume-raise))

  (:map cj/music-map
		;; EMMS show playlist.
		("m" . cj/music-playlist-toggle)
		("M" . cj/music-playlist-show)

		;; Add artists and albums (directories) and songs (files) in fuzzy search
		("a" . cj/music-fuzzy-select-and-add)

		;; Create Radio station m3u
		("r" . cj/music-create-radio-station)

		;; Playback controls
		("SPC" . emms-pause)
		("s" . emms-stop)
		("p" . emms-playlist-mode-go)
		("x" . emms-shuffle)))

(global-unset-key (kbd "<f10>"))
(global-set-key (kbd "<f10>")     #'cj/music-playlist-toggle)

;; ------------------------- Music Add Dired Selection -------------------------

(defun cj/music-add-dired-selection ()
  "Add selected files or directories in Dired/Dirvish to the EMMS playlist.
If region is active, add marked files. Otherwise, add file at point.
Directories are added recursively."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
	(user-error "This command must be run in a Dired buffer"))
  (let ((files (if (use-region-p)
				   (dired-get-marked-files)
				 (list (dired-get-file-for-visit)))))
	(when (null files)
	  (user-error "No files selected"))
	(dolist (file files)
	  (if (file-directory-p file)
		  (cj/music-add-directory-recursive file)
		(if (cj/music--valid-file-p file)
			(emms-add-file file)
		  (message "Skipping non-music file: %s" file))))
	(message "Added %d item(s) to EMMS playlist" (length files))))

(with-eval-after-load 'dired
  (define-key dirvish-mode-map "p" 'cj/music-add-dired-selection))

(provide 'music-config)
;;; music-config.el ends here

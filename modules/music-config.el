;;; music-config.el --- EMMS configuration with MPV backend -*- coding: utf-8; lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/P/S.
;; Load shape: eager.
;; Eager reason: none; optional music workflow that registers a music keymap.
;; Top-level side effects: defines C-; m map, one global key, package config.
;; Runtime requires: subr-x, user-constants, keybindings.
;; Direct test load: yes.
;;
;; EMMS setup using an mpv subprocess player, M3U playlist helpers, fuzzy
;; file/directory adds, Dired/Dirvish integration, radio-station creation, and
;; playlist window toggling.
;;
;; The playlist keymap intentionally follows ncmpcpp where it maps cleanly, with
;; EMMS-specific additions for M3U editing and consume mode.

;;; Code:

(require 'subr-x)
(require 'user-constants)
(require 'keybindings)  ;; provides cj/custom-keymap
(require 'cj-window-geometry-lib) ;; cj/preferred-dock-direction (F10 dock side)
(require 'cj-window-toggle-lib)  ;; side-window size memory (F10 toggle)
(require 'system-lib)            ;; cj/confirm-strong (overwrite confirms)

;; Declare these foreign package vars special so `let'-binding them below
;; compiles as a dynamic bind, not a dead lexical local -- otherwise emms /
;; orderless never see the binding (the lexical-binding foreign-special-var trap).
(defvar orderless-smart-case)
(defvar emms-source-playlist-ask-before-overwrite)
(defvar emms-playlist-buffer-p)
(defvar emms-playlist-buffer)
(defvar emms-random-playlist)
(defvar emms-playlist-selected-marker)
(defvar emms-source-file-default-directory)
(defvar emms-player-playing-p)
(defvar emms-player-paused-p)
(defvar emms-playlist-mode-map)
(defvar dirvish-mode-map)

;; Playlist-header faces.  Defined here so the `cj/music--header-text'
;; references are valid (an undefined face spams "Invalid face reference" on
;; every render).  Appearance inherits themed base faces so the active theme
;; owns the colors -- the literal values were dropped in the route-colors pass.
(defface cj/music-header-face '((t :inherit shadow))
  "Playlist-header field labels (Playlist, Current, Mode, Keys).")
(defface cj/music-header-value-face '((t :inherit default))
  "Playlist-header field values.")
(defface cj/music-mode-on-face '((t :inherit warning))
  "Active mode indicator in the playlist header.")
(defface cj/music-mode-off-face '((t :inherit shadow))
  "Inactive mode indicator in the playlist header.")
(defface cj/music-keyhint-face '((t :inherit shadow))
  "Key hints in the playlist header.")

;; Foreign functions used lazily after their packages load.
(declare-function emms-playlist-mode "emms-playlist-mode")
(declare-function emms-playlist-track-at "emms-playlist-mode")
(declare-function emms-playlist-mode-kill-track "emms-playlist-mode")
(declare-function emms-track-name "emms")
(declare-function emms-track-type "emms")
(declare-function emms-track-get "emms")
(declare-function emms-track-simple-description "emms")
(declare-function emms-playlist-current-selected-track "emms")
(declare-function emms-playlist-select "emms")
(declare-function emms-playlist-clear "emms")
(declare-function emms-playlist-save "emms-source-playlist")
(declare-function emms-start "emms")
(declare-function emms-random "emms")
(declare-function emms-next "emms")
(declare-function emms-previous "emms")
(declare-function dired-get-marked-files "dired")
(declare-function dired-get-file-for-visit "dired")
(declare-function face-remap-remove-relative "face-remap")

;;; Settings (no Customize)

(defvar cj/music-root music-dir
  "Root directory of your music collection.")

(defvar cj/music-m3u-root cj/music-root
  "Directory M3U playlists are saved to (the single writable target).
Reading and selection union `cj/music-m3u-roots'; only saving and radio-station
creation write here.")

(defvar cj/music-m3u-roots
  (list cj/music-root
        (expand-file-name "~/.local/share/mpd/playlists/"))
  "Directories to source M3U playlists from, in precedence order.
Both the local-library playlists (`cj/music-root') and the dotfiles-tracked
internet-radio playlists (MPD's playlist_directory) surface together for
selection and loading.  Earlier directories win on a basename collision.
Missing directories are skipped.  Saving still targets the single
`cj/music-m3u-root'.")

(defvar cj/music-file-extensions '("aac" "flac" "m4a" "mp3" "ogg" "opus" "wav")
  "List of valid music file extensions.")

(defvar cj/music-seek-seconds 5
  "Seconds to move when seeking forward or backward in the current track.")

(defvar cj/music-playlist-buffer-name "*EMMS-Playlist*"
  "Name of the EMMS playlist buffer used by this configuration.")

;;; Subprocess mpv player (reliable playback)

;; The IPC player (emms-player-mpv) drives mpv over a socket -- start mpv idle,
;; connect, send loadfile.  That handshake was leaving mpv loaded but never
;; streaming, so playback silently failed.  Driving mpv with the track as a
;; direct argument -- the invocation that plays every time -- is the reliable
;; path.  --no-config isolates this mpv from the interactive/video mpv setup so
;; the two cannot interfere.  Pause is in place via process signals; in-track
;; seek is not available with a subprocess player (the trade for reliability).

(declare-function emms-player "emms")
(declare-function emms-player-set "emms")
(declare-function emms-player-simple-start "emms-player-simple")
(declare-function emms-player-simple-stop "emms-player-simple")
(defvar emms-player-simple-process-name)
(defvar emms-player-cj/music-mpv)

(defvar cj/music--mpv-regex
  (concat "\\(?:\\." (regexp-opt cj/music-file-extensions) "\\'\\)"
          "\\|\\`\\(?:https?\\|mms\\)://")
  "Track names the subprocess mpv player handles: music files or stream URLs.")

(defvar cj/music--mpv-socket
  (expand-file-name "emms/mpv-control.sock" user-emacs-directory)
  "IPC control socket for the subprocess mpv player.
mpv opens it per playback via --input-ipc-server.  It does NOT affect startup:
mpv still plays the track passed as a direct argument, so the reliable start is
unchanged.  The socket only carries control commands (seek) to the already
playing process, which is where the old idle + loadfile handshake failed.")

(defun cj/music--mpv-start (track)
  "Play TRACK by running mpv with the track name as a direct argument."
  (emms-player-simple-start (emms-track-name track)
                            'emms-player-cj/music-mpv
                            "mpv"
                            (list "--no-video" "--no-config" "--really-quiet"
                                  (concat "--input-ipc-server=" cj/music--mpv-socket))))

(defun cj/music--mpv-stop ()
  "Stop the mpv subprocess."
  (emms-player-simple-stop))

(defun cj/music--mpv-playable-p (track)
  "Return non-nil if the subprocess mpv player can play TRACK."
  (and (executable-find "mpv")
       (memq (emms-track-type track) '(file url))
       (string-match cj/music--mpv-regex (emms-track-name track))))

(defun cj/music--mpv-pause ()
  "Pause the mpv subprocess in place by stopping it (SIGSTOP)."
  (let ((proc (get-process emms-player-simple-process-name)))
    (when (and proc (process-live-p proc))
      (signal-process proc 'SIGSTOP))))

(defun cj/music--mpv-resume ()
  "Resume the paused mpv subprocess (SIGCONT)."
  (let ((proc (get-process emms-player-simple-process-name)))
    (when (and proc (process-live-p proc))
      (signal-process proc 'SIGCONT))))

(defun cj/music--mpv-command (json)
  "Send JSON (a one-line mpv IPC command) to the control socket.
A no-op when nothing is playing or the socket is gone, so it never errors."
  (when (file-exists-p cj/music--mpv-socket)
    (ignore-errors
      (let ((proc (make-network-process :name "cj-music-mpv-cmd"
                                        :family 'local
                                        :service cj/music--mpv-socket
                                        :noquery t)))
        (unwind-protect
            (progn (process-send-string proc (concat json "\n"))
                   (accept-process-output proc 0.1))
          (delete-process proc))))))

(defun cj/music-seek-forward ()
  "Seek `cj/music-seek-seconds' seconds forward in the current track."
  (interactive)
  (cj/music--mpv-command
   (format "{\"command\": [\"seek\", %d, \"relative\"]}" cj/music-seek-seconds)))

(defun cj/music-seek-backward ()
  "Seek `cj/music-seek-seconds' seconds backward in the current track."
  (interactive)
  (cj/music--mpv-command
   (format "{\"command\": [\"seek\", %d, \"relative\"]}" (- cj/music-seek-seconds))))

;;; Buffer-local state

(defvar-local cj/music-playlist-file nil
  "M3U file associated with the current playlist buffer. Set on load/save.")

;;; Helpers: file/dir/m3u/playlist

(defun cj/music--valid-file-p (file)
  "Return non-nil if FILE has an accepted music extension (case-insensitive)."
  (when (and file (stringp file))
    (when-let ((ext (file-name-extension file)))
      (member (downcase ext) cj/music-file-extensions))))

(defun cj/music--valid-directory-p (dir)
  "Return non-nil if DIR is a non-hidden directory."
  (when (and dir (stringp dir) (not (string-empty-p dir)))
    (and (file-directory-p dir)
         (not (string-prefix-p "." (file-name-nondirectory
                                    (directory-file-name dir)))))))

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
  "Completion table for CANDIDATES preserving order and case-insensitive match.
Tags the `cj-music-file' category and annotates each candidate (a path relative
to `cj/music-root', with a trailing slash for directories) with its size and
modification date so marginalia can show them."
  (let ((annotate (cj/completion-file-annotator
                   (lambda (c)
                     (expand-file-name
                      (if (string-suffix-p "/" c) (substring c 0 -1) c)
                      cj/music-root)))))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata
            (category . cj-music-file)
            (annotation-function . ,annotate)
            (display-sort-function . identity)
            (cycle-sort-function . identity)
            (completion-ignore-case . t))
        (complete-with-action action candidates string pred)))))

(defun cj/music--ensure-playlist-buffer ()
  "Ensure EMMS playlist buffer exists and is in playlist mode. Return buffer."
  (let ((buffer (get-buffer-create cj/music-playlist-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'emms-playlist-mode)
        (emms-playlist-mode))
      (setq emms-playlist-buffer-p t))
    ;; Set this as the current EMMS playlist buffer
    (setq emms-playlist-buffer buffer)
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
                            (string-match-p "\\`\\(https?\\|mms\\)://" line))
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

(defun cj/music--dedup-m3u-files (paths)
  "Return (BASENAME . PATH) conses for PATHS, first occurrence of a basename winning.
Pure helper: since `cj/music--get-m3u-files' scans `cj/music-m3u-roots' in order,
an earlier directory shadows a same-named playlist in a later one."
  (let ((seen (make-hash-table :test 'equal))
        (result '()))
    (dolist (p paths (nreverse result))
      (let ((base (file-name-nondirectory p)))
        (unless (gethash base seen)
          (puthash base t seen)
          (push (cons base p) result))))))

(defun cj/music--get-m3u-files ()
  "Return (BASENAME . FULLPATH) conses for M3Us across `cj/music-m3u-roots'.
Directories are scanned in order and missing ones skipped; on a basename
collision the earlier directory wins."
  (cj/music--dedup-m3u-files
   (cl-loop for dir in cj/music-m3u-roots
            when (file-directory-p dir)
            append (directory-files dir t "\\.m3u\\'" t))))

(defun cj/music--get-m3u-basenames ()
  "Return list of M3U basenames (no extension) across `cj/music-m3u-roots'."
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

(defun cj/music--assert-m3u-files-exist ()
  "Assert that M3U files exist across `cj/music-m3u-roots'.
Returns the list of (BASENAME . FULLPATH) conses. Signals user-error if none."
  (let ((files (cj/music--get-m3u-files)))
    (when (null files)
      (user-error "No M3U files found in %s"
                  (string-join cj/music-m3u-roots ", ")))
    files))

(defun cj/music--sync-playlist-file (file-path)
  "Set the playlist buffer's associated M3U file to FILE-PATH and reset point."
  (with-current-buffer (cj/music--ensure-playlist-buffer)
    (setq cj/music-playlist-file file-path)
    (goto-char (point-min))))

(defun cj/music--select-m3u-file (prompt)
  "Prompt user to select an M3U file with PROMPT.
Returns the full path to the selected file, or nil if cancelled."
  (let* ((m3u-files (cj/music--assert-m3u-files-exist))
         (choices (append (mapcar #'car m3u-files) '("(Cancel)")))
         (choice (completing-read prompt choices nil t)))
    (unless (string= choice "(Cancel)")
      (cdr (assoc choice m3u-files)))))

;;; Commands: add/select

(defun cj/music-add-directory-recursive (directory)
  "Add all music files under DIRECTORY recursively to the EMMS playlist."
  (interactive
   (list (read-directory-name "Add directory recursively: " cj/music-root nil t)))
  (unless (file-directory-p directory)
    (user-error "Not a directory: %s" directory))
  (cj/music--ensure-playlist-buffer)
  (emms-add-directory-tree directory)
  (message "Added recursively: %s" directory))


(defun cj/music-fuzzy-select-and-add ()
  "Select a music file or directory and add to EMMS playlist.
Directories (trailing /) are added recursively; files added singly."
  (interactive)
  (let* ((completion-ignore-case t)
         (orderless-smart-case nil)
         (candidates (cj/music--collect-entries-recursive cj/music-root))
         (choice-rel (completing-read "Choose music file or directory: "
                                      (cj/music--completion-table candidates)
                                      nil t))
         (cleaned (if (string-suffix-p "/" choice-rel)
                      (substring choice-rel 0 -1)
                    choice-rel))
         (abs (expand-file-name cleaned cj/music-root)))
    (cj/music--ensure-playlist-buffer)
    (if (file-directory-p abs)
        (cj/music-add-directory-recursive abs)
      (emms-add-file abs))
    (message "Added to playlist: %s" choice-rel)))

;;; Commands: playlist management (load/save/clear/reload/edit)

(defun cj/music--append-track-to-m3u-file (track-path m3u-file)
  "Append TRACK-PATH to M3U-FILE. Signals error on failure.
Pure function for testing - no user interaction.
TRACK-PATH should be an absolute path.
M3U-FILE should be an existing, writable M3U file path."
  (unless (file-exists-p m3u-file)
    (error "M3U file does not exist: %s" m3u-file))
  (unless (file-writable-p m3u-file)
    (error "M3U file is not writable: %s" m3u-file))

  ;; Convert absolute path to relative path from music root
  (let ((relative-path (if (file-name-absolute-p track-path)
                           (file-relative-name track-path cj/music-root)
                         track-path)))
    ;; Determine if we need a leading newline
    (let ((needs-prefix-newline nil)
          (file-size (file-attribute-size (file-attributes m3u-file))))
      (when (> file-size 0)
        ;; Read the last character of the file to check if it ends with newline
        (with-temp-buffer
          (insert-file-contents m3u-file nil (max 0 (1- file-size)) file-size)
          (setq needs-prefix-newline (not (= (char-after (point-min)) ?\n)))))

      ;; Append the track with proper newline handling
      (with-temp-buffer
        (when needs-prefix-newline
          (insert "\n"))
        (insert relative-path "\n")
        (write-region (point-min) (point-max) m3u-file t 0))))
  t)


(defun cj/music-append-track-to-playlist ()
  "Append track at point to a selected M3U playlist file.
Prompts for M3U file selection with completion. Allows cancellation."
  (interactive)
  (unless (derived-mode-p 'emms-playlist-mode)
    (user-error "This command must be run in the EMMS playlist buffer"))
  (let ((track (emms-playlist-track-at (point))))
    (unless track
      (user-error "No track at point"))
    (let* ((track-path (emms-track-name track))
           (m3u-file (cj/music--select-m3u-file "Append track to playlist: ")))
      (if (not m3u-file)
          (message "Cancelled")
        (condition-case err
            (progn
              (cj/music--append-track-to-m3u-file track-path m3u-file)
              (message "Added '%s' to %s"
                       (file-name-nondirectory track-path)
                       (file-name-nondirectory m3u-file)))
          (error (message "Failed to append track: %s" (error-message-string err))))))))


(defun cj/music-playlist-load ()
  "Load an M3U playlist from cj/music-m3u-root.
Replaces current playlist."
  (interactive)
  (let* ((pairs (cj/music--assert-m3u-files-exist))
         (choice-name (completing-read "Select playlist: " (mapcar #'car pairs) nil t))
         (choice-file (cdr (assoc choice-name pairs))))
    (unless (and choice-file (file-exists-p choice-file))
      (user-error "Playlist file does not exist: %s" choice-name))
    (emms-playlist-clear)
    (emms-play-playlist choice-file)
    (cj/music--sync-playlist-file choice-file)
    (message "Loaded playlist: %s" choice-name)))


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
               (not (cj/confirm-strong (format "Overwrite %s? " filename))))
      (user-error "Aborted saving playlist"))
    (with-current-buffer (cj/music--ensure-playlist-buffer)
      (let ((emms-source-playlist-ask-before-overwrite nil))
        (emms-playlist-save 'm3u full)))
    (cj/music--sync-playlist-file full)
    (message "Saved playlist: %s" filename)))


(defun cj/music-playlist-clear ()
  "Stop playback and empty the playlist."
  (interactive)
  (emms-stop)
  (emms-playlist-clear)
  ;; Advice also clears, but do it eagerly for this command
  (with-current-buffer (cj/music--ensure-playlist-buffer)
    (setq cj/music-playlist-file nil))
  (message "Playlist cleared"))


(defun cj/music-playlist-reload ()
  "Reload current playlist from its associated M3U file."
  (interactive)
  (cj/music--assert-valid-playlist-file)
  (let* ((file-path (with-current-buffer (cj/music--ensure-playlist-buffer)
                      cj/music-playlist-file))
         (name (file-name-nondirectory file-path)))
    (emms-playlist-clear)
    (emms-play-playlist file-path)
    (cj/music--sync-playlist-file file-path)
    (message "Reloaded playlist: %s" name)))


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

;;; Commands: random-aware navigation

(defvar cj/music--random-history nil
  "List of recently played track names during random mode, most recent first.")

(defvar cj/music--random-history-max 50
  "Maximum number of tracks to keep in random playback history.")

(defun cj/music--record-random-history ()
  "Push the current track onto random history if random mode is active.
Intended for use on `emms-player-started-hook'."
  (when emms-random-playlist
    (when-let ((track (emms-playlist-current-selected-track)))
      (let ((name (emms-track-name track)))
        (unless (equal name (car cj/music--random-history))
          (push name cj/music--random-history)
          (when (> (length cj/music--random-history) cj/music--random-history-max)
            (setq cj/music--random-history
                  (seq-take cj/music--random-history cj/music--random-history-max))))))))

(defun cj/music-next ()
  "Play next track. Respects random mode — picks a random track if active."
  (interactive)
  (if emms-random-playlist
      (emms-random)
    (emms-next)))

(defun cj/music--find-track-in-playlist (track-name)
  "Return buffer position of TRACK-NAME in the current playlist, or nil."
  (with-current-buffer (cj/music--ensure-playlist-buffer)
    (save-excursion
      (goto-char (point-min))
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (when-let ((track (emms-playlist-track-at (point))))
            (when (equal (emms-track-name track) track-name)
              (setq found (point))))
          (unless found (forward-line 1)))
        found))))

(defun cj/music-previous ()
  "Play previous track. In random mode, go back through playback history."
  (interactive)
  (if (and emms-random-playlist cj/music--random-history)
      (let* ((track-name (pop cj/music--random-history))
             (pos (cj/music--find-track-in-playlist track-name)))
        (if pos
            (progn
              (emms-playlist-select pos)
              (emms-start))
          (message "Track no longer in playlist: %s"
                   (file-name-nondirectory track-name))))
    (when (and emms-random-playlist (null cj/music--random-history))
      (message "No random history to go back to"))
    (emms-previous)))

;;; Commands: consume mode

(defvar cj/music-consume-mode nil
  "Non-nil means consume mode is active.
When enabled, tracks are removed from the playlist after they finish playing.")

(defun cj/music--consume-track ()
  "Remove the just-finished track from the playlist.
Intended for use on `emms-player-finished-hook'."
  (when cj/music-consume-mode
    (with-current-buffer (cj/music--ensure-playlist-buffer)
      (when (and emms-playlist-selected-marker
                 (marker-position emms-playlist-selected-marker))
        (save-excursion
          (goto-char emms-playlist-selected-marker)
          (emms-playlist-mode-kill-track))))))

(defun cj/music-toggle-consume ()
  "Toggle consume mode. When active, tracks are removed after playing."
  (interactive)
  (setq cj/music-consume-mode (not cj/music-consume-mode))
  (if cj/music-consume-mode
      (add-hook 'emms-player-finished-hook #'cj/music--consume-track)
    (remove-hook 'emms-player-finished-hook #'cj/music--consume-track))
  (message "Consume mode %s" (if cj/music-consume-mode "enabled" "disabled")))

;;; Commands: UI

;;; Minimal ensure-loaded setup for on-demand use

(defun cj/emms--setup ()
  "Ensure EMMS is loaded and quiet in mode line."
  (unless (featurep 'emms)
    (require 'emms))

)


(defvar cj/music-playlist-window-height 0.3
  "Default fraction of frame height for the F10 music playlist side window.
Used when the playlist docks at the bottom and hasn't been resized and
toggled off this session; after that, the toggled-off height is remembered
in `cj/--music-playlist-height'.")

(defvar cj/music-playlist-window-width 0.4
  "Default fraction of frame width for the F10 music playlist side window.
Used when the playlist docks as a right-side column (see
`cj/--music-playlist-side') and hasn't been resized this session; after
that the toggled-off width is remembered in `cj/--music-playlist-width'.")

(defvar cj/--music-playlist-height nil
  "Last height fraction the playlist was toggled off at while docked bottom.
nil means fall back to `cj/music-playlist-window-height'.  In-memory only --
resets each Emacs session.")

(defvar cj/--music-playlist-width nil
  "Last width fraction the playlist was toggled off at while docked right.
nil means fall back to `cj/music-playlist-window-width'.  In-memory only --
resets each Emacs session.")

(defun cj/--music-playlist-side ()
  "Return the side the F10 playlist should dock on: `right' or `bottom'.
Docks as a right-side column only when a side-by-side split would leave
both panes at least `cj/window-dock-min-columns' wide (the playlist's
share is `cj/music-playlist-window-width'); otherwise docks at the bottom.
See `cj/preferred-dock-direction'."
  (if (eq (cj/preferred-dock-direction (frame-width)
                                       cj/music-playlist-window-width)
          'right)
      'right
    'bottom))

(defun cj/music-playlist-toggle ()
  "Toggle the EMMS playlist buffer in a bottom side window.
The window opens at `cj/music-playlist-window-height'; if it has been
resized and toggled off this session, it reopens at that remembered height."
  (interactive)
  (let* ((buf-name cj/music-playlist-buffer-name)
         (buffer (get-buffer buf-name))
         (win (and buffer (get-buffer-window buffer))))
    (if win
        (progn
          ;; Capture the resized size into the var matching the window's
          ;; actual side, so width and height memories stay independent.
          ;; Guard the parameter lookup: a dead or non-window WIN (the
          ;; capture helpers tolerate one) must not error here.
          (let ((side (if (window-live-p win)
                          (or (window-parameter win 'window-side) 'bottom)
                        'bottom)))
            (if (memq side '(left right))
                (cj/side-window-capture-size win side 'cj/--music-playlist-width)
              (cj/side-window-capture-size win 'bottom 'cj/--music-playlist-height)))
          (delete-window win)
          (message "Playlist window closed"))
      (progn
        (cj/emms--setup)
        (setq buffer (cj/music--ensure-playlist-buffer))
        (let* ((side (cj/--music-playlist-side))
               (right (eq side 'right)))
          (setq win (cj/side-window-display
                     buffer side
                     (if right 'cj/--music-playlist-width 'cj/--music-playlist-height)
                     (if right cj/music-playlist-window-width
                       cj/music-playlist-window-height))))
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

(defun cj/music-add-dired-selection ()
  "Add selected files/dirs in Dired/Dirvish to the EMMS playlist.
Dirs added recursively."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "This command must be run in a Dired buffer"))
  (cj/music--ensure-playlist-buffer)
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

(with-eval-after-load 'dirvish
  (keymap-set dirvish-mode-map "+" #'cj/music-add-dired-selection))

;;; EMMS setup and keybindings

;; Music/EMMS keymap
(defvar-keymap cj/music-map
  :doc "Keymap for music commands"
  "m" #'cj/music-playlist-toggle
  "M" #'cj/music-playlist-show
  "a" #'cj/music-fuzzy-select-and-add
  "R" #'cj/music-create-radio-station
  "SPC" #'emms-pause
  "s" #'emms-stop
  "n" #'cj/music-next
  "p" #'cj/music-previous
  "g" #'emms-playlist-mode-go
  "Z" #'emms-shuffle
  "r" #'emms-toggle-repeat-playlist
  "t" #'emms-toggle-repeat-track
  "z" #'emms-toggle-random-playlist
  "x" #'cj/music-toggle-consume)

(cj/register-prefix-map "m" cj/music-map)
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; m" "music menu"
    "C-; m m" "toggle playlist"
    "C-; m M" "show playlist"
    "C-; m a" "add music"
    "C-; m R" "create radio"
    "C-; m SPC" "pause"
    "C-; m s" "stop"
    "C-; m n" "next track"
    "C-; m p" "previous track"
    "C-; m g" "goto playlist"
    "C-; m Z" "shuffle"
    "C-; m r" "repeat playlist"
    "C-; m t" "repeat track"
    "C-; m z" "random"
    "C-; m x" "consume"))

;;; Playlist display helpers
;;
;; Defined at top level (not inside the `emms' use-package `:config') so the
;; byte-compiler sees them; they touch EMMS only at call time, after load.

(defun cj/music--after-playlist-clear (&rest _)
  "Forget the associated M3U file after the playlist is cleared."
  (when-let ((buf (get-buffer cj/music-playlist-buffer-name)))
    (with-current-buffer buf
      (setq cj/music-playlist-file nil))))

(defun cj/music--format-duration (seconds)
  "Convert SECONDS to a \"M:SS\" string."
  (when (and seconds (numberp seconds) (> seconds 0))
    (format "%d:%02d" (/ seconds 60) (mod seconds 60))))

;; ---------------------------- Display-name layer -----------------------------
;; A track maps to a display NAME (shared by the header's Current line and the
;; playlist row renderer) plus, on a row, a dim type glyph and right-aligned
;; meta.  The pure pieces (name resolution, #EXTINF-label extraction, host
;; tidying, progress-bar fill) carry the tests; the glyph, the :align-to meta,
;; and the disk-backed name-map are exercised live.

(defun cj/music--tidy-host (url)
  "Return a readable host label for URL: scheme and path dropped, a leading
\"www.\" removed, and a multi-label host reduced to its last two labels
\(ice6.somafm.com -> somafm.com).  A string with no scheme://host is returned
unchanged, so a non-URL name shows as-is instead of erroring."
  (if (string-match "\\`[a-zA-Z]+://\\(?:[^@/]*@\\)?\\([^:/?#]+\\)" url)
      (let* ((host (replace-regexp-in-string "\\`www\\." "" (match-string 1 url)))
             (labels (split-string host "\\." t)))
        (if (> (length labels) 2)
            (string-join (last labels 2) ".")
          host))
    url))

(defun cj/music--m3u-entries (text)
  "Parse M3U TEXT into an alist of (STREAM-URL . PLIST).
Each PLIST carries :name (the #EXTINF label), :uuid (#RADIOBROWSERUUID), and
:favicon (#RADIOBROWSERFAVICON), read from the comment lines preceding the url.
A url with no #EXTINF is skipped; fields reset after each url so nothing leaks
between stations."
  (let ((name nil) (uuid nil) (favicon nil) (entries '()))
    (dolist (line (split-string text "[\r\n]+" t) (nreverse entries))
      (cond
       ((string-match "\\`#EXTINF:[^,]*,\\(.*\\)\\'" line)
        (setq name (match-string 1 line)))
       ((string-match "\\`#RADIOBROWSERUUID:\\(.*\\)\\'" line)
        (setq uuid (match-string 1 line)))
       ((string-match "\\`#RADIOBROWSERFAVICON:\\(.*\\)\\'" line)
        (setq favicon (match-string 1 line)))
       ((string-prefix-p "#" line))     ; other comment
       (t (when name
            (push (cons line (list :name name :uuid uuid :favicon favicon))
                  entries))
          (setq name nil uuid nil favicon nil))))))

(defun cj/music--m3u-labels (text)
  "Alist of (STREAM-URL . #EXTINF-LABEL) parsed from M3U TEXT.
A thin projection of `cj/music--m3u-entries' onto the label field."
  (mapcar (lambda (e) (cons (car e) (plist-get (cdr e) :name)))
          (cj/music--m3u-entries text)))

(defvar cj/music--radio-metadata-cache nil
  "Cached url->plist metadata (:name :uuid :favicon) across `cj/music-m3u-roots'.
Built lazily so a row render never re-scans disk; cleared by
`cj/music--refresh-radio-name-map' when a station is created or a playlist
loads.")

(defun cj/music--radio-metadata ()
  "Alist of stream-url -> plist metadata, unioned across all playlist roots.
Reads each .m3u once and caches the result; both name resolution and the
cover-art layer read from it."
  (or cj/music--radio-metadata-cache
      (setq cj/music--radio-metadata-cache
            (cl-loop for (_base . path) in (cj/music--get-m3u-files)
                     append (cj/music--m3u-entries
                             (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string)))))))

(defun cj/music--radio-name-map ()
  "Alist of stream-url -> station label, derived from the cached metadata."
  (mapcar (lambda (e) (cons (car e) (plist-get (cdr e) :name)))
          (cj/music--radio-metadata)))

(defun cj/music--refresh-radio-name-map ()
  "Clear the cached radio metadata so the next render rebuilds it."
  (setq cj/music--radio-metadata-cache nil))

(defun cj/music--display-name (track &optional name-map)
  "Human display name for TRACK, name only (no duration).
A tagged track (file or url) shows \"Artist - Title\" or the bare title; an
untagged file shows its filename; a url track resolves to its #EXTINF label
from NAME-MAP (an alist of url->label), else a tidied host.  Unknown types
fall back to `emms-track-simple-description'."
  (let ((type (emms-track-type track))
        (title (emms-track-get track 'info-title))
        (artist (emms-track-get track 'info-artist))
        (name (emms-track-name track)))
    (cond
     (title (if artist (format "%s - %s" artist title) title))
     ((eq type 'file) (file-name-sans-extension (file-name-nondirectory name)))
     ((eq type 'url) (or (cdr (assoc name name-map)) (cj/music--tidy-host name)))
     (t (emms-track-simple-description track)))))

(defun cj/music--format-meta (track)
  "Right-aligned meta string for TRACK's row: a file's duration as \"[M:SS]\",
empty when there's no duration (a live stream, or an untimed file)."
  (let ((dur (cj/music--format-duration (emms-track-get track 'info-playing-time))))
    (if dur (format "[%s]" dur) "")))

(defun cj/music--bar-fill (elapsed total width)
  "Filled-cell count for a WIDTH-cell progress bar at ELAPSED/TOTAL seconds.
Returns 0..WIDTH, or the symbol `indeterminate' when TOTAL is nil or
non-positive (a live stream).  A nil ELAPSED counts as zero."
  (if (or (null total) (<= total 0))
      'indeterminate
    (let ((ratio (min 1.0 (max 0.0 (/ (float (or elapsed 0)) total)))))
      (round (* ratio width)))))

(defun cj/music--type-glyph (track)
  "A leading glyph for TRACK: a broadcast icon for a stream, a note for a file.
Uses nerd-icons when available; otherwise a plain marker, so a TTY or a
fontless frame never shows a tofu box."
  (let ((stream (eq (emms-track-type track) 'url)))
    (or (and (fboundp 'nerd-icons-mdicon)
             (ignore-errors
               (nerd-icons-mdicon (if stream "nf-md-broadcast" "nf-md-music_note")
                                  :face 'cj/music-header-face)))
        (if stream "»" "•"))))

(defun cj/music--row-string (track)
  "Playlist row for TRACK: a dim type glyph, the display name, and the meta
right-aligned to the window edge with a resize-safe :align-to space.
This is `emms-track-description-function'."
  (let* ((name (cj/music--display-name track (cj/music--radio-name-map)))
         (glyph (cj/music--type-glyph track))
         (meta (cj/music--format-meta track)))
    (if (string-empty-p meta)
        (concat glyph " " name)
      (concat glyph " " name
              (propertize " " 'display
                          `(space :align-to (- right ,(1+ (length meta)))))
              (propertize meta 'face 'cj/music-keyhint-face)))))

(defun cj/music--now-playing-suffix (track)
  "Trailing status for the header Current line: on-air for a stream, the
duration for a timed file, empty otherwise."
  (if (eq (emms-track-type track) 'url)
      "  ◉ on air"
    (let ((d (cj/music--format-duration (emms-track-get track 'info-playing-time))))
      (if d (format "  %s" d) ""))))

;; Multi-line header overlay
(defvar-local cj/music--header-overlay nil
  "Overlay displaying the playlist header.")

(defun cj/music--header-text ()
  "Build a multi-line header string for the playlist buffer overlay."
  (let* ((pl-name (if cj/music-playlist-file
                      (file-name-sans-extension
                       (file-name-nondirectory cj/music-playlist-file))
                    "Untitled"))
         (track-count (count-lines (point-min) (point-max)))
         (now-playing (cond
                       ((not emms-player-playing-p) "Stopped")
                       (emms-player-paused-p "Paused")
                       (t (let ((track (emms-playlist-current-selected-track)))
                            (if track
                                (concat (cj/music--display-name
                                         track (cj/music--radio-name-map))
                                        (cj/music--now-playing-suffix track))
                              "Playing")))))
         (mode-indicator
          (lambda (key label active)
            (let ((face (if active 'cj/music-mode-on-face 'cj/music-mode-off-face)))
              (propertize (format "[%s] %s" key label) 'face face)))))
    (concat
     (propertize "Playlist" 'face 'cj/music-header-face)
     (propertize " : " 'face 'cj/music-header-face)
     (propertize (format "%s (%d)" pl-name track-count) 'face 'cj/music-header-value-face)
     "\n"
     (propertize "Current " 'face 'cj/music-header-face)
     (propertize " : " 'face 'cj/music-header-face)
     (propertize now-playing 'face 'cj/music-header-value-face)
     "\n"
     (propertize "Mode    " 'face 'cj/music-header-face)
     (propertize " : " 'face 'cj/music-header-face)
     (funcall mode-indicator "r" "repeat" (bound-and-true-p emms-repeat-playlist))
     "  "
     (funcall mode-indicator "s" "single" (bound-and-true-p emms-repeat-track))
     "  "
     (funcall mode-indicator "z" "random" (bound-and-true-p emms-random-playlist))
     "  "
     (funcall mode-indicator "x" "consume" cj/music-consume-mode)
     "\n"
     (propertize "Keys    " 'face 'cj/music-header-face)
     (propertize " : " 'face 'cj/music-header-face)
     (propertize "a:add  c:clear  L:load  S:stop  SPC:pause  <>:skip  ↑↓:move  C-↑↓:reorder  q:dismiss"
                 'face 'cj/music-keyhint-face)
     "\n"
     (propertize "Radio   " 'face 'cj/music-header-face)
     (propertize " : " 'face 'cj/music-header-face)
     (propertize "n:by name  t:by tag  m:enter manually"
                 'face 'cj/music-keyhint-face)
     "\n"
     ;; A full-width rule under the header, resize-safe (redisplay recomputes
     ;; the :align-to span rather than a hardcoded character count).
     (propertize " " 'face '(:strike-through t :inherit shadow)
                 'display '(space :align-to right))
     "\n\n")))

(defun cj/music--update-header ()
  "Insert or update the multi-line header overlay in the playlist buffer."
  (when-let ((buf (get-buffer cj/music-playlist-buffer-name)))
    (with-current-buffer buf
      (unless cj/music--header-overlay
        (setq cj/music--header-overlay (make-overlay (point-min) (point-min)))
        (overlay-put cj/music--header-overlay 'priority 100))
      (move-overlay cj/music--header-overlay (point-min) (point-min))
      (overlay-put cj/music--header-overlay 'before-string
                   (cj/music--header-text)))))

(defvar-local cj/music--bg-remap-cookie nil
  "Cookie for the active-window background face remapping.")

(defun cj/music--update-active-bg (&rest _)
  "Toggle playlist buffer background based on whether its window is selected."
  (when-let ((buf (get-buffer cj/music-playlist-buffer-name)))
    (with-current-buffer buf
      (let ((active (eq buf (window-buffer (selected-window)))))
        (cond
         ((and active (not cj/music--bg-remap-cookie))
          (setq cj/music--bg-remap-cookie
                (face-remap-add-relative 'default)))
         ((and (not active) cj/music--bg-remap-cookie)
          (face-remap-remove-relative cj/music--bg-remap-cookie)
          (setq cj/music--bg-remap-cookie nil)))))))

(defun cj/music--setup-playlist-display ()
  "Set up header overlay and focus tracking in the playlist buffer."
  (setq header-line-format nil)
  (cj/music--update-header)
  (add-hook 'window-selection-change-functions #'cj/music--update-active-bg nil t))

(use-package emms
  :defer t
  :init
  ;; Set buffer name BEFORE emms loads to prevent default buffer creation
  (setq emms-playlist-buffer-name "*EMMS-Playlist*")
  :commands (emms-mode-line-mode)
  :config
  (require 'emms-setup)
  (require 'emms-player-simple)
  (require 'emms-playlist-mode)
  (require 'emms-source-file)
  (require 'emms-source-playlist)

  ;; Basic EMMS configuration - MUST be set before emms-all
  (setq emms-source-file-default-directory cj/music-root)
  (setq emms-playlist-default-major-mode 'emms-playlist-mode)

  ;; Use the reliable subprocess mpv player (built above) - MUST be set before emms-all
  (setq emms-player-cj/music-mpv
        (emms-player #'cj/music--mpv-start #'cj/music--mpv-stop
                     #'cj/music--mpv-playable-p))
  (emms-player-set emms-player-cj/music-mpv 'pause #'cj/music--mpv-pause)
  (emms-player-set emms-player-cj/music-mpv 'resume #'cj/music--mpv-resume)
  (setq emms-player-list '(emms-player-cj/music-mpv))

  ;; Now initialize EMMS
  (emms-all)

  ;; Disable modeline display (keep modeline clean)
  (emms-playing-time-display-mode -1)
  (emms-mode-line-mode -1)

  ;; Keep cj/music-playlist-file in sync if playlist is cleared.
  ;; Ensure we don't stack duplicate advice on reload.
  (advice-remove 'emms-playlist-clear #'cj/music--after-playlist-clear)
  (advice-add 'emms-playlist-clear :after #'cj/music--after-playlist-clear)

  ;;; Playlist display

  ;; Track description: show "Artist - Title [M:SS]" instead of file paths
  (setq emms-track-description-function #'cj/music--row-string)

  (add-hook 'emms-playlist-mode-hook #'cj/music--setup-playlist-display)
  (add-hook 'emms-player-started-hook #'cj/music--record-random-history)
  (add-hook 'emms-player-started-hook #'cj/music--update-header)
  (add-hook 'emms-player-stopped-hook #'cj/music--update-header)
  (add-hook 'emms-player-paused-hook #'cj/music--update-header)
  (add-hook 'emms-player-finished-hook #'cj/music--update-header)
  (add-hook 'emms-playlist-cleared-hook #'cj/music--update-header)

  ;; Refresh header immediately when toggling modes
  (dolist (fn '(emms-toggle-repeat-playlist
                emms-toggle-repeat-track
                emms-toggle-random-playlist
                cj/music-toggle-consume))
    (advice-add fn :after (lambda (&rest _) (cj/music--update-header))))

  :bind
  (:map emms-playlist-mode-map
        ;; Playback
        ("p"   . emms-playlist-mode-go)
        ("SPC" . emms-pause)
        ("s"   . emms-stop)
        ("n"   . cj/music-next)
        (">"   . cj/music-next)
        ("P"   . cj/music-previous)
        ("<"   . cj/music-previous)
        ("f"   . cj/music-seek-forward)
        ("b"   . cj/music-seek-backward)
        ("q"   . emms-playlist-mode-bury-buffer)
        ("a"   . cj/music-fuzzy-select-and-add)
        ;; Toggles (aligned with ncmpcpp)
        ("r"   . emms-toggle-repeat-playlist)
        ("t"   . emms-toggle-repeat-track)
        ("z"   . emms-toggle-random-playlist)
        ("x"   . cj/music-toggle-consume)
        ("Z"   . emms-shuffle)
        ;; Info
        ("i"   . emms-show)
        ("o"   . emms-playlist-mode-center-current)
        ;; Manipulation
        ("A" . cj/music-append-track-to-playlist)
        ("c" . cj/music-playlist-clear)
        ("C" . cj/music-playlist-clear)
        ("L" . cj/music-playlist-load)
        ("E" . cj/music-playlist-edit)
        ("g" . cj/music-playlist-reload)
        ("S" . cj/music-playlist-save)
        ;; Track reordering
        ("S-<up>"   . emms-playlist-mode-shift-track-up)
        ("S-<down>" . emms-playlist-mode-shift-track-down)
        ("C-<up>"   . emms-playlist-mode-shift-track-up)
        ("C-<down>" . emms-playlist-mode-shift-track-down)
        ;; Volume
        ("+" . emms-volume-raise)
        ("=" . emms-volume-raise)
        ("-" . emms-volume-lower)))

(keymap-global-set "<f10>" #'cj/music-playlist-toggle)

;;; Radio station creation

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
               (not (cj/confirm-strong (format "Overwrite %s? " (file-name-nondirectory file)))))
      (user-error "Aborted creating radio station"))
    (with-temp-file file
      (insert content))
    (cj/music--refresh-radio-name-map)
    (message "Created radio station: %s" (file-name-nondirectory file))))

;; The manual name+URL creator is bound to m in the radio row below (see the
;; with-eval-after-load block near the radio-browser lookup), not R.

;; --------------------------- Radio-browser Lookup ----------------------------
;; Search radio-browser.info and turn a selection into a playable radio .m3u.
;; Spec: docs/specs/2026-07-06-radio-browser-lookup-spec.org.  The pure pieces
;; (parse / emit / format / filename) carry the tests; the network GET and the
;; interactive command are exercised live.

(require 'url)

(defvar cj/music-radio-server "de1.api.radio-browser.info"
  "Default radio-browser API host.
On a connection failure the client falls back to a host from /json/servers.")

(defvar cj/music-radio-user-agent "cj-emacs-music/1.0 (radio-browser lookup)"
  "User-Agent sent with radio-browser requests.
The project asks clients to identify themselves.")

(defvar cj/music-radio-search-limit 30
  "Maximum number of stations a radio-browser search returns.")

(defvar cj/music-radio-save-dir (expand-file-name "~/.local/share/mpd/playlists/")
  "Directory new radio-browser stations are written to (the radio home).")

(defvar cj/music-radio-filename-suffix "-Radio"
  "Suffix appended to a created station's basename, before the .m3u extension.
Marks a lookup-created station in the playlist directory.")

(defun cj/music-radio--parse-search (json-text)
  "Parse a radio-browser JSON-TEXT array into a list of station plists.
Signals a `user-error' rather than a raw parse error when JSON-TEXT is not
JSON (a gateway HTML page or a rate-limit notice), so a bad response reads as a
clear message instead of a stack trace."
  (condition-case nil
      (json-parse-string json-text :object-type 'plist :array-type 'list :null-object nil)
    (error (user-error "radio-browser returned an unreadable response"))))

(defun cj/music-radio--station-url (st)
  "Best stream URL for station ST: url_resolved, then url, then nil."
  (let ((r (plist-get st :url_resolved))
        (u (plist-get st :url)))
    (cond ((and (stringp r) (not (string-empty-p r))) r)
          ((and (stringp u) (not (string-empty-p u))) u))))

(defun cj/music-radio--station-m3u (st)
  "Return the .m3u file text for station ST, or nil when it has no stream URL.
Matches the existing radio files: #EXTM3U, an optional #RADIOBROWSERUUID line,
#EXTINF:1,<name>, and the stream URL."
  (let* ((url (cj/music-radio--station-url st))
         ;; Strip newlines so an unexpected multi-line name can't inject extra
         ;; m3u lines; the API returns single-line names, but it's external data.
         (name (replace-regexp-in-string "[\r\n]+" " "
                                         (or (plist-get st :name) "Radio")))
         (uuid (plist-get st :stationuuid))
         (favicon (plist-get st :favicon)))
    (when url
      (concat "#EXTM3U\n"
              (if (and (stringp uuid) (not (string-empty-p uuid)))
                  (format "#RADIOBROWSERUUID:%s\n" uuid)
                "")
              ;; Capture the favicon at creation so the cover-art layer needs
              ;; no byuuid lookup later; same newline-strip guard as the name.
              (if (and (stringp favicon) (not (string-empty-p favicon)))
                  (format "#RADIOBROWSERFAVICON:%s\n"
                          (replace-regexp-in-string "[\r\n]+" " " favicon))
                "")
              (format "#EXTINF:1,%s\n%s\n" name url)))))

(defun cj/music-radio--tags-snippet (tags n)
  "Return the first N comma-separated TAGS as a trimmed display string.
TAGS is a comma-separated string or nil; nil or empty yields the empty string."
  (if (and (stringp tags) (not (string-empty-p tags)))
      (string-join (seq-take (split-string tags "," t "[ \t]*") n) ", ")
    ""))

(defun cj/music-radio--format-candidate (st)
  "Marginalia annotation for station ST.
Variant B: codec, bitrate, country, votes, and the first few tags."
  (let ((codec (or (plist-get st :codec) ""))
        (bitrate (let ((b (plist-get st :bitrate)))
                   (if (and (integerp b) (> b 0)) (format "%dk" b) "")))
        (cc (or (plist-get st :countrycode) ""))
        (votes (or (plist-get st :votes) 0))
        (tags (cj/music-radio--tags-snippet (plist-get st :tags) 3)))
    (format "%-4s %-5s %-2s ♥%d  %s" codec bitrate cc votes tags)))

(defun cj/music-radio--disambiguate-name (name uuid taken)
  "Return a filesystem-safe basename for NAME, unique against the TAKEN list.
On a collision, append a short fragment of UUID.  Pure helper: keeps two
same-named stations picked in one search from overwriting each other."
  (let ((base (cj/music--safe-filename name)))
    (if (member base taken)
        (concat base "_" (substring (or uuid "x") 0 (min 8 (length (or uuid "x")))))
      base)))

(defun cj/music-radio--search-url (server query &optional field)
  "Build the radio-browser station-search URL for QUERY against SERVER.
FIELD is the search field: \"name\" (default) or \"tag\"."
  (format "https://%s/json/stations/search?%s=%s&limit=%d&hidebroken=true&order=votes&reverse=true"
          server (or field "name") (url-hexify-string query) cj/music-radio-search-limit))

(defun cj/music-radio--http-get (url)
  "GET URL with the radio-browser User-Agent; return the response body or nil."
  (let ((url-request-extra-headers `(("User-Agent" . ,cj/music-radio-user-agent))))
    (when-let* ((buf (url-retrieve-synchronously url t t 15)))
      (with-current-buffer buf
        (goto-char (point-min))
        (prog1 (when (re-search-forward "\n\n" nil t)
                 (buffer-substring-no-properties (point) (point-max)))
          (kill-buffer buf))))))

(defun cj/music-radio--search-fallback (query &optional field)
  "Fetch an alternate radio-browser host and retry the QUERY/FIELD search once."
  (when-let* ((body (cj/music-radio--http-get
                     "https://all.api.radio-browser.info/json/servers"))
              (servers (cj/music-radio--parse-search body))
              (host (plist-get (car servers) :name)))
    (cj/music-radio--http-get (cj/music-radio--search-url host query field))))

(defun cj/music-radio--search (query &optional field)
  "Search radio-browser for QUERY on FIELD; return a list of station plists.
FIELD is \"name\" (default) or \"tag\".  Tries `cj/music-radio-server' first,
then falls back to a host from /json/servers once.  Signals a `user-error' when
nothing responds."
  (let ((body (or (ignore-errors
                    (cj/music-radio--http-get
                     (cj/music-radio--search-url cj/music-radio-server query field)))
                  (ignore-errors (cj/music-radio--search-fallback query field)))))
    (unless body (user-error "radio-browser: no response (network down?)"))
    (cj/music-radio--parse-search body)))

(defun cj/music-radio--candidates (stations)
  "Return an alist of (DISPLAY . STATION) for STATIONS with unique display keys.
DISPLAY is the station name; a repeated name gets its codec/bitrate appended,
then a numeric suffix, so completing-read keys never collide and each maps back
to one station.  Pure helper."
  (let ((seen (make-hash-table :test 'equal))
        (out '()))
    (dolist (st stations (nreverse out))
      (let* ((name (string-trim (or (plist-get st :name) "(unnamed)")))
             (disp name)
             (n 2))
        (when (gethash disp seen)
          (setq disp (format "%s (%s%s)" name (or (plist-get st :codec) "")
                             (let ((b (plist-get st :bitrate)))
                               (if (and (integerp b) (> b 0)) (format " %dk" b) "")))))
        (while (gethash disp seen)
          (setq disp (format "%s #%d" name n))
          (setq n (1+ n)))
        (puthash disp t seen)
        (push (cons disp st) out)))))

(defun cj/music-radio--write-stations (stations dir)
  "Write each station in STATIONS as an .m3u into DIR.
Skips a station with no stream URL and disambiguates a filename that collides
with an already-written one this run.  Returns a plist (:written PATHS :skipped
NAMES).  Creates DIR when absent."
  (make-directory dir t)
  (let ((taken '()) (written '()) (skipped '()))
    (dolist (st stations)
      (let ((m3u (cj/music-radio--station-m3u st)))
        (if (not m3u)
            (push (or (plist-get st :name) "(unnamed)") skipped)
          (let* ((base (cj/music-radio--disambiguate-name
                        (or (plist-get st :name) "Radio")
                        (plist-get st :stationuuid) taken))
                 (path (expand-file-name
                        (concat base cj/music-radio-filename-suffix ".m3u") dir)))
            (push base taken)
            (with-temp-file path (insert m3u))
            (push path written)))))
    ;; New .m3u files landed, so the cached url->label map is stale.
    (when written (cj/music--refresh-radio-name-map))
    (list :written (nreverse written) :skipped (nreverse skipped))))

(defun cj/music-radio--completion-table (candidates)
  "Completion table over CANDIDATES carrying the Variant-B marginalia affix."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata
          (category . cj-radio-station)
          (affixation-function
           . ,(lambda (cands)
                (mapcar (lambda (c)
                          (let ((st (cdr (assoc c candidates))))
                            ;; The "[done]" sentinel has no station, so it gets
                            ;; no annotation rather than a bogus "0" row.
                            (list c "" (if st
                                           (concat "   "
                                                   (propertize (cj/music-radio--format-candidate st)
                                                               'face 'completions-annotations))
                                         ""))))
                        cands))))
      (complete-with-action action (mapcar #'car candidates) string pred))))

(defun cj/music-radio--pick-loop (candidates)
  "Repeatedly prompt to pick from CANDIDATES until \"[done]\" is chosen.
CANDIDATES is a (DISPLAY . STATION) alist.  Returns the chosen station plists in
selection order; each pick is removed from the pool so it can't be chosen twice."
  (let ((pool (copy-sequence candidates))
        (chosen '())
        (done nil))
    (while (and (not done) pool)
      (let ((pick (completing-read
                   (format "Add station (%d picked, RET [done] to finish): "
                           (length chosen))
                   (cj/music-radio--completion-table (cons '("[done]") pool))
                   nil t)))
        (if (equal pick "[done]")
            (setq done t)
          (when-let ((cell (assoc pick pool)))
            (push (cdr cell) chosen)
            (setq pool (delq cell pool))))))
    (nreverse chosen)))

(defun cj/music-radio--play (paths)
  "Play the first .m3u in PATHS immediately (interrupting), enqueue the rest."
  (when paths
    (emms-play-playlist (car paths))
    (dolist (p (cdr paths))
      (emms-add-playlist p))))

(defun cj/music-radio--search-and-play (query field)
  "Search radio-browser for QUERY on FIELD, pick stations, then create and play.
FIELD is \"name\" or \"tag\".  Lists matching stations (annotated with codec,
bitrate, country, votes, and tags), lets you pick several one at a time, writes
each as an .m3u into `cj/music-radio-save-dir', then plays the selection through
mpv (interrupting whatever was playing)."
  (when (string-empty-p (string-trim query))
    (user-error "Empty search"))
  (let* ((stations (cj/music-radio--search query field))
         (candidates (cj/music-radio--candidates stations)))
    (unless candidates
      (user-error "No stations found for %s %S" field query))
    (let ((chosen (cj/music-radio--pick-loop candidates)))
      (unless chosen
        (user-error "No stations selected"))
      (let* ((result (cj/music-radio--write-stations chosen cj/music-radio-save-dir))
             (written (plist-get result :written))
             (skipped (plist-get result :skipped)))
        (when written
          (cj/music-radio--play written))
        (message "Created %d station%s%s%s"
                 (length written)
                 (if (= (length written) 1) "" "s")
                 (if skipped
                     (format ", skipped %d with no URL (%s)"
                             (length skipped) (string-join skipped ", "))
                   "")
                 (if written
                     (format " — playing %s"
                             (file-name-sans-extension
                              (file-name-nondirectory (car written))))
                   ""))))))

(defun cj/music-radio-search-by-name (query)
  "Search radio-browser.info by station name, then create and play a selection."
  (interactive "sRadio search (name): ")
  (cj/music-radio--search-and-play query "name"))

(defun cj/music-radio-search-by-tag (tag)
  "Search radio-browser.info by tag/genre, then create and play a selection."
  (interactive "sRadio search (tag): ")
  (cj/music-radio--search-and-play tag "tag"))

;; ------------------------------- Cover art -----------------------------------
;; A track maps to a local cover-image path: a cached favicon/album art, or a
;; shipped vinyl placeholder.  `cj/music-art--for-track' is non-blocking (it
;; reads only the cache) so the row renderer can call it during redisplay;
;; `cj/music-art--ensure' does the network fetch off the render path.  The pure
;; pieces (cache key, favicon URL, image validation) carry the tests; the fetch
;; is a live smoke test.  Consumed by the Phase 3 fancy render.

(require 'image)

(defvar cj/music-art-cache-dir
  (expand-file-name "music-art/" (expand-file-name "data/" user-emacs-directory))
  "Directory holding fetched or extracted cover art, keyed by station UUID or a
file hash.  Gitignored runtime state; `cj/music-clear-art-cache' empties it.")

(defvar cj/music-art-placeholder
  (expand-file-name "vinyl-placeholder.svg"
                    (expand-file-name "assets/" user-emacs-directory))
  "Shipped vinyl-record placeholder shown when a track has no cover art.")

(defun cj/music-art--cache-key (track &optional entries)
  "Stable cache-file basename (no extension) for TRACK.
A url with a #RADIOBROWSERUUID in ENTRIES keys on the uuid so a station shares
one cached logo; any other url keys on a hash of its address; a file keys on a
hash of its path."
  (let ((name (emms-track-name track)))
    (if (eq (emms-track-type track) 'url)
        (let ((uuid (plist-get (cdr (assoc name entries)) :uuid)))
          (if (and (stringp uuid) (not (string-empty-p uuid)))
              uuid
            (concat "url-" (sha1 name))))
      (concat "file-" (sha1 name)))))

(defun cj/music-art--favicon-url (track &optional entries)
  "Direct favicon image URL for a url TRACK from its captured
#RADIOBROWSERFAVICON, or nil.  A station with only a uuid resolves via a byuuid
lookup elsewhere; a file track has no favicon URL."
  (when (eq (emms-track-type track) 'url)
    (let ((fav (plist-get (cdr (assoc (emms-track-name track) entries)) :favicon)))
      (and (stringp fav) (not (string-empty-p fav)) fav))))

(defun cj/music-art--valid-image-p (data)
  "Non-nil when DATA looks like a displayable image (a recognizable image
header), so an empty body, an HTML error page, or a text response is rejected
before it is cached."
  (and (stringp data) (not (string-empty-p data))
       (image-type-from-data data) t))

(defun cj/music-art--cached-file (key)
  "Return an existing cached art file for KEY (any extension), or nil."
  (car (file-expand-wildcards
        (expand-file-name (concat key ".*") cj/music-art-cache-dir))))

(defun cj/music-art--file-cover (track)
  "Return a sibling cover image (cover/folder/front .jpg/.jpeg/.png) next to a
file TRACK, or nil.  Embedded-tag art extraction is deferred (vNext)."
  (when (eq (emms-track-type track) 'file)
    (when-let ((dir (file-name-directory (emms-track-name track))))
      (cl-loop for base in '("cover" "folder" "front")
               thereis (cl-loop for ext in '("jpg" "jpeg" "png")
                                for f = (expand-file-name (concat base "." ext) dir)
                                when (file-exists-p f) return f)))))

(defun cj/music-art--fetch-to-cache (url key)
  "Fetch URL and, if it is a valid image, write it into the art cache under KEY.
Returns the cached path, or nil on a failed or non-image response.  Blocks on
the network, so call it off the redisplay path.  Only http/https URLs are
fetched, so an external favicon field can't point the reader at a file:// or
other-scheme resource."
  (when-let* (((string-match-p "\\`https?://" url))
              (data (cj/music-radio--http-get url))
              ((cj/music-art--valid-image-p data)))
    (make-directory cj/music-art-cache-dir t)
    (let ((path (expand-file-name
                 (concat key "." (symbol-name (image-type-from-data data)))
                 cj/music-art-cache-dir))
          (coding-system-for-write 'binary))
      (with-temp-file path
        (set-buffer-multibyte nil)
        (insert data))
      path)))

(defun cj/music-art--byuuid-favicon (uuid)
  "Look up station UUID via radio-browser byuuid and return its favicon URL, or
nil.  The fallback for a legacy station that carries a uuid but no captured
favicon.  Blocks on the network."
  (when-let* ((body (cj/music-radio--http-get
                     (format "https://%s/json/stations/byuuid/%s"
                             cj/music-radio-server uuid)))
              (stations (ignore-errors (cj/music-radio--parse-search body)))
              (fav (plist-get (car stations) :favicon)))
    (and (stringp fav) (not (string-empty-p fav)) fav)))

(defun cj/music-art--for-track (track)
  "Local cover-art path for TRACK, WITHOUT any network: an already-cached file,
a sibling cover for a local file, else the vinyl placeholder.  Never blocks, so
the row renderer can call it during redisplay; `cj/music-art--ensure' does the
fetch off the render path."
  (let ((key (cj/music-art--cache-key track (cj/music--radio-metadata))))
    (or (cj/music-art--cached-file key)
        (cj/music-art--file-cover track)
        cj/music-art-placeholder)))

(defun cj/music-art--ensure (track)
  "Fetch and cache TRACK's cover art if it is not cached yet.  Blocks on the
network, so call it off the redisplay path.  Returns the cached path, or nil
when there is nothing to fetch."
  (let* ((entries (cj/music--radio-metadata))
         (key (cj/music-art--cache-key track entries)))
    (unless (cj/music-art--cached-file key)
      (when (eq (emms-track-type track) 'url)
        (let ((fav (or (cj/music-art--favicon-url track entries)
                       (let ((uuid (plist-get (cdr (assoc (emms-track-name track)
                                                          entries))
                                              :uuid)))
                         (and (stringp uuid) (not (string-empty-p uuid))
                              (cj/music-art--byuuid-favicon uuid))))))
          (and fav (cj/music-art--fetch-to-cache fav key)))))))

(defun cj/music-clear-art-cache ()
  "Delete every cached cover-art file so art is re-fetched on next need."
  (interactive)
  (when (file-directory-p cj/music-art-cache-dir)
    (dolist (f (directory-files cj/music-art-cache-dir t "\\`[^.]"))
      (delete-file f)))
  (message "Cleared music art cache: %s" cj/music-art-cache-dir))

;; Radio row in the playlist buffer: n = search by name, t = search by tag,
;; m = enter a station by hand.  This moves the "single" mode toggle off t to s
;; and emms-stop off s to S (see the header's Mode/Keys/Radio rows).
(with-eval-after-load 'emms
  (keymap-set emms-playlist-mode-map "n" #'cj/music-radio-search-by-name)
  (keymap-set emms-playlist-mode-map "t" #'cj/music-radio-search-by-tag)
  (keymap-set emms-playlist-mode-map "m" #'cj/music-create-radio-station)
  (keymap-set emms-playlist-mode-map "s" #'emms-toggle-repeat-track)
  (keymap-set emms-playlist-mode-map "S" #'emms-stop))

(provide 'music-config)
;;; music-config.el ends here

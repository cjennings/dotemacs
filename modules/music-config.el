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
  "Directory where M3U playlists are saved and loaded.")

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

(defun cj/music--assert-m3u-files-exist ()
  "Assert that M3U files exist in cj/music-m3u-root.
Returns the list of (BASENAME . FULLPATH) conses. Signals user-error if none."
  (let ((files (cj/music--get-m3u-files)))
    (when (null files)
      (user-error "No M3U files found in %s" cj/music-m3u-root))
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

(defun cj/music--track-description (track)
  "Return a human-readable description of TRACK.
For tagged tracks: \"Artist - Title [M:SS]\".
For file tracks without tags: filename without path or extension.
For URL tracks: decoded URL."
  (let ((type (emms-track-type track))
        (title (emms-track-get track 'info-title))
        (artist (emms-track-get track 'info-artist))
        (duration (emms-track-get track 'info-playing-time))
        (name (emms-track-name track)))
    (cond
     ;; Tagged track with title
     (title
      (let ((dur-str (cj/music--format-duration duration))
            (parts '()))
        (when artist (push artist parts))
        (push title parts)
        (let ((desc (string-join (nreverse parts) " - ")))
          (if dur-str (format "%s  [%s]" desc dur-str) desc))))
     ;; File without tags — show clean filename
     ((eq type 'file)
      (file-name-sans-extension (file-name-nondirectory name)))
     ;; URL — decode percent-encoded characters
     ((eq type 'url)
      (decode-coding-string (url-unhex-string name) 'utf-8))
     ;; Fallback
     (t (emms-track-simple-description track)))))

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
                                (cj/music--track-description track)
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
     (funcall mode-indicator "t" "single" (bound-and-true-p emms-repeat-track))
     "  "
     (funcall mode-indicator "z" "random" (bound-and-true-p emms-random-playlist))
     "  "
     (funcall mode-indicator "x" "consume" cj/music-consume-mode)
     "\n"
     (propertize "Keys    " 'face 'cj/music-header-face)
     (propertize " : " 'face 'cj/music-header-face)
     (propertize "a:add  c:clear  L:load  S:save  SPC:pause  <>:skip  ↑↓:move  C-↑↓:reorder  q:dismiss"
                 'face 'cj/music-keyhint-face)
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
  (setq emms-track-description-function #'cj/music--track-description)

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
    (message "Created radio station: %s" (file-name-nondirectory file))))

;; Bound here rather than in the emms `:bind' so use-package does not emit a
;; redundant autoload that collides with this same-file definition.
(with-eval-after-load 'emms
  (keymap-set emms-playlist-mode-map "R" #'cj/music-create-radio-station))

(provide 'music-config)
;;; music-config.el ends here

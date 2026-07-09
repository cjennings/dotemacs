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
;;
;; The player has two render paths.  In a graphical frame with `cj/music-fancy-ui'
;; on (the default), it draws the fancy hi-fi surface: a now-playing hero with
;; cover art (station favicon / sibling album art / a shipped vinyl placeholder,
;; cached under data/music-art/), a serif title, and a block progress bar that
;; advances from mpv's percent-pos while a file plays.  A TTY frame, or the
;; toggle off, falls back to the plain text player (names, a dim glyph, a thin
;; status line).  `cj/music-clear-art-cache' empties the art cache.

;;; Code:

(require 'subr-x)
(require 'user-constants)
(require 'keybindings)  ;; provides cj/custom-keymap
(require 'cj-window-toggle-lib)  ;; side-window size memory (F10 toggle)
(require 'system-lib)            ;; cj/confirm-strong (overwrite confirms)

;; Declare these foreign package vars special so `let'-binding them below
;; compiles as a dynamic bind, not a dead lexical local -- otherwise emms /
;; orderless never see the binding (the lexical-binding foreign-special-var trap).
(defvar orderless-smart-case)
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

;; Fancy-render faces (Phase 3).  Amber comes from the themed `warning' face so
;; the active theme (dupre) owns the color; the serif family is applied at
;; render time from `cj/music-title-family'.
(defface cj/music-title-face '((t :inherit cj/music-header-value-face :weight bold))
  "Now-playing title in the fancy player.")
(defface cj/music-subtitle-face '((t :inherit shadow))
  "Now-playing subtitle (station or album) in the fancy player.")
(defface cj/music-bar-fill-face '((t :inherit warning))
  "Filled portion of the fancy progress bar (amber).")
(defface cj/music-bar-empty-face '((t :inherit shadow))
  "Empty portion of the fancy progress bar.")

(defgroup cj/music nil
  "Personal EMMS music-player tweaks."
  :group 'emms)

(defcustom cj/music-fancy-ui t
  "When non-nil and the frame is graphical, render the fancy hi-fi player:
cover art, a serif now-playing hero, and a progress bar.  Nil, or a TTY frame,
falls back to the plain text player (names, a dim glyph, a thin status line)."
  :type 'boolean
  :group 'cj/music)

(defcustom cj/music-title-family
  (if (boundp 'cj/nov-reading-font-family) cj/nov-reading-font-family "Merriweather")
  "Serif family for the fancy now-playing title, mirroring the nov reading view."
  :type 'string
  :group 'cj/music)

(defvar cj/music-hero-size 96
  "Pixel height of the now-playing hero cover image.")
(defvar cj/music-thumb-size 22
  "Pixel height of a playlist row's cover thumbnail.")
(defvar cj/music-bar-width 24
  "Cell width of the now-playing progress bar.")
(defvar cj/music-bar-interval 1
  "Seconds between progress-bar redraws while a track is playing and visible.")

;; Foreign functions used lazily after their packages load.
(declare-function emms-playlist-mode "emms-playlist-mode")
(declare-function emms-playlist-track-at "emms-playlist-mode")
(declare-function emms-playlist-mode-kill-track "emms-playlist-mode")
(declare-function emms-track-name "emms")
(declare-function emms-track-type "emms")
(declare-function emms-track-get "emms")
(declare-function emms-track "emms")
(declare-function emms-track-set "emms")
(declare-function emms-track-simple-description "emms")
(declare-function emms-playlist-current-selected-track "emms")
(declare-function emms-playlist-select "emms")
(declare-function emms-playlist-selected-track "emms")
(declare-function emms-playlist-clear "emms")
(declare-function emms-playlist-insert-track "emms")
(declare-function emms-stop "emms")
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

(defun cj/music--mpv-get-property (prop)
  "Query the mpv IPC socket for PROP and return its value, or nil.
Reads the reply (unlike `cj/music--mpv-command', which only sends), so the
progress bar can read percent-pos.  Blocks briefly, so call it off redisplay."
  (when (file-exists-p cj/music--mpv-socket)
    (ignore-errors
      (let ((out "") proc)
        (setq proc (make-network-process
                    :name "cj-music-mpv-get" :family 'local
                    :service cj/music--mpv-socket :noquery t
                    :filter (lambda (_p s) (setq out (concat out s)))))
        (unwind-protect
            (progn
              (process-send-string
               proc (format "{\"command\":[\"get_property\",\"%s\"]}\n" prop))
              (accept-process-output proc 0.2)
              (cl-loop for line in (split-string out "\n" t)
                       for obj = (ignore-errors
                                   (json-parse-string line :object-type 'plist
                                                      :null-object nil))
                       when (and obj (plist-member obj :data))
                       return (plist-get obj :data)))
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

(defun cj/music--playlist-track-objects ()
  "Return the track objects from the current EMMS playlist buffer, in order."
  (let ((tracks '()))
    (with-current-buffer (cj/music--ensure-playlist-buffer)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when-let ((track (emms-playlist-track-at (point))))
            (push track tracks))
          (forward-line 1))))
    (nreverse tracks)))

(defun cj/music--playlist-tracks ()
  "Return list of track names from current EMMS playlist buffer."
  (mapcar #'emms-track-name (cj/music--playlist-track-objects)))

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


(defun cj/music--m3u-track-lines (track entries)
  "The .m3u lines for TRACK.
A file track is its bare absolute path.  A url track carries its station
metadata — an #EXTINF label plus #RADIOBROWSERUUID / #RADIOBROWSERFAVICON when
known — read from the track's properties first, then its ENTRIES metadata (a
loaded legacy playlist has entries but no properties), so a saved station
keeps its display name and cover art on reload."
  (let ((name (emms-track-name track)))
    (if (not (eq (emms-track-type track) 'url))
        (concat name "\n")
      (let* ((meta (cdr (assoc name entries)))
             (label (or (emms-track-get track 'info-title)
                        (plist-get meta :name)
                        (cj/music--tidy-host name)))
             (uuid (or (emms-track-get track 'radio-uuid)
                       (plist-get meta :uuid)))
             (favicon (or (emms-track-get track 'radio-favicon)
                          (plist-get meta :favicon))))
        (concat
         (if (and (stringp uuid) (not (string-empty-p uuid)))
             (format "#RADIOBROWSERUUID:%s\n" uuid)
           "")
         (if (and (stringp favicon) (not (string-empty-p favicon)))
             (format "#RADIOBROWSERFAVICON:%s\n"
                     (replace-regexp-in-string "[\r\n]+" " " favicon))
           "")
         (format "#EXTINF:-1,%s\n"
                 (replace-regexp-in-string "[\r\n]+" " " label))
         name "\n")))))

(defun cj/music--m3u-text (tracks entries)
  "The full .m3u file text for TRACKS, station metadata from ENTRIES.
The stock EMMS m3u writer emits bare URLs; this emitter writes the comment
lines `cj/music--m3u-entries' parses, so save -> load round-trips."
  (concat "#EXTM3U\n"
          (mapconcat (lambda (tr) (cj/music--m3u-track-lines tr entries))
                     tracks "")))

(defun cj/music--write-playlist-file (path tracks entries)
  "Write TRACKS to PATH as .m3u text, station metadata from ENTRIES.
Refreshes the radio metadata cache since a new .m3u just landed."
  (with-temp-file path
    (insert (cj/music--m3u-text tracks entries)))
  (cj/music--refresh-radio-name-map))

(defun cj/music--save-default-name (tracks file entries)
  "The name the save prompt should offer.
FILE (the playlist's associated .m3u) wins when present.  Otherwise the first
url track's station name — its title property, else its #EXTINF label from
ENTRIES.  Nil when neither applies (the caller falls back to a timestamp)."
  (if file
      (file-name-sans-extension (file-name-nondirectory file))
    (cl-loop for tr in tracks
             when (eq (emms-track-type tr) 'url)
             thereis (or (emms-track-get tr 'info-title)
                         (plist-get (cdr (assoc (emms-track-name tr) entries))
                                    :name)))))

(defun cj/music--save-directory (tracks)
  "Directory a saved playlist targets.
An all-stream queue is a radio playlist and saves into
`cj/music-radio-save-dir'; anything else saves into `cj/music-m3u-root'."
  (if (and tracks
           (cl-every (lambda (tr) (eq (emms-track-type tr) 'url)) tracks))
      cj/music-radio-save-dir
    cj/music-m3u-root))

(defun cj/music-playlist-save ()
  "Save the current EMMS playlist to an .m3u file.
An all-stream queue saves into `cj/music-radio-save-dir' (the radio playlist
home); anything else saves into `cj/music-m3u-root'.  A queue of freshly
looked-up stations pre-fills the first station's name in the prompt; a
playlist with an associated file keeps that file's name as the default.
Station metadata (name, uuid, favicon) is written with each stream so a
reloaded playlist keeps its display name and cover art."
  (interactive)
  (let* ((tracks (cj/music--playlist-track-objects))
         (entries (cj/music--radio-metadata))
         (existing (cj/music--get-m3u-basenames))
         (assoc-file (with-current-buffer (cj/music--ensure-playlist-buffer)
                       cj/music-playlist-file))
         (prefill (and (null assoc-file)
                       (cj/music--save-default-name tracks nil entries)))
         (default-name (or (cj/music--save-default-name tracks assoc-file entries)
                           (format-time-string "playlist-%Y%m%d-%H%M%S")))
         (chosen (completing-read "Save playlist as: " existing nil nil
                                  prefill nil default-name))
         (filename (if (string-suffix-p ".m3u" chosen) chosen (concat chosen ".m3u")))
         (dir (cj/music--save-directory tracks))
         (full (expand-file-name filename dir)))
    (when (string-empty-p (string-trim chosen))
      (user-error "Playlist name cannot be empty"))
    (when (and (file-exists-p full)
               (not (cj/confirm-strong (format "Overwrite %s? " filename))))
      (user-error "Aborted saving playlist"))
    (make-directory dir t)
    (cj/music--write-playlist-file full tracks entries)
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
          (cj/music--write-playlist-file path
                                         (cj/music--playlist-track-objects)
                                         (cj/music--radio-metadata))))
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
Used when the playlist hasn't been resized and toggled off this session;
after that, the toggled-off height is remembered in
`cj/--music-playlist-height'.")

(defvar cj/--music-playlist-height nil
  "Last height fraction the playlist was toggled off at.
nil means fall back to `cj/music-playlist-window-height'.  In-memory only --
resets each Emacs session.")

(defun cj/music-playlist-toggle ()
  "Toggle the EMMS playlist buffer in a bottom side window.
The playlist always docks at the bottom, whatever the frame's shape.  It
used to dock as a right-side column on a wide frame (via
`cj/preferred-dock-direction'), which split a wide frame three ways --
unexpected often enough that Craig retired the rule (2026-07-09).

The window opens at `cj/music-playlist-window-height'; if it has been
resized and toggled off this session, it reopens at that remembered height."
  (interactive)
  (let* ((buf-name cj/music-playlist-buffer-name)
         (buffer (get-buffer buf-name))
         (win (and buffer (get-buffer-window buffer))))
    (if win
        (progn
          (cj/side-window-capture-size win 'bottom 'cj/--music-playlist-height)
          (delete-window win)
          (message "Playlist window closed"))
      (progn
        (cj/emms--setup)
        (setq buffer (cj/music--ensure-playlist-buffer))
        (setq win (cj/side-window-display
                   buffer 'bottom 'cj/--music-playlist-height
                   cj/music-playlist-window-height))
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
(defvar-keymap cj/music-radio-map
  :doc "Radio prefix: mirrors the playlist buffer's n/t/m radio row."
  "n" #'cj/music-radio-search-by-name
  "t" #'cj/music-radio-search-by-tag
  "m" #'cj/music-create-radio-station)

(defvar-keymap cj/music-map
  :doc "Keymap for music commands (all lowercase, chord-friendly)"
  "m" #'cj/music-playlist-toggle
  "v" #'cj/music-playlist-show
  "a" #'cj/music-fuzzy-select-and-add
  "r" cj/music-radio-map
  "SPC" #'emms-pause
  "s" #'emms-stop
  "n" #'cj/music-next
  "p" #'cj/music-previous
  "g" #'emms-playlist-mode-go
  "u" #'emms-shuffle
  "l" #'emms-toggle-repeat-playlist
  "t" #'emms-toggle-repeat-track
  "z" #'emms-toggle-random-playlist
  "x" #'cj/music-toggle-consume)

(cj/register-prefix-map "m" cj/music-map)
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; m" "music menu"
    "C-; m m" "toggle playlist"
    "C-; m v" "show playlist"
    "C-; m a" "add music"
    "C-; m r" "+radio"
    "C-; m r n" "radio by name"
    "C-; m r t" "radio by tag"
    "C-; m r m" "radio manual entry"
    "C-; m SPC" "pause"
    "C-; m s" "stop"
    "C-; m n" "next track"
    "C-; m p" "previous track"
    "C-; m g" "goto playlist"
    "C-; m u" "shuffle"
    "C-; m l" "repeat playlist"
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
  "Playlist row for TRACK: a lead glyph or cover thumbnail, the display name,
and the meta right-aligned to the window edge with a resize-safe :align-to
space.  In the fancy render the lead is a thumbnail and the name is serif.
This is `emms-track-description-function'."
  (let* ((name (cj/music--display-name track (cj/music--radio-name-map)))
         (meta (cj/music--format-meta track))
         (fancy (cj/music--fancy-p))
         (lead (if-let* ((fancy)
                         (img (cj/music--image (cj/music-art--for-track track)
                                               cj/music-thumb-size)))
                   (propertize " " 'display img)
                 (cj/music--type-glyph track)))
         (label (if fancy
                    (propertize name 'face (list :family cj/music-title-family
                                                 :inherit 'cj/music-title-face))
                  name)))
    (if (string-empty-p meta)
        (concat lead " " label)
      (concat lead " " label
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

;; ------------------------------ Fancy render ---------------------------------
;; The GUI hero (cover image + serif title + bar) and thumbnailed serif rows,
;; gated on a graphical frame + `cj/music-fancy-ui'.  Cover art comes from the
;; non-blocking `cj/music-art--for-track' (defined with the art layer below).

(defun cj/music--fancy-p ()
  "Non-nil when the fancy render applies: a graphical frame with the
`cj/music-fancy-ui' toggle on.  Decided per redisplay, so a TTY frame and a GUI
frame in the same session can differ."
  (and cj/music-fancy-ui (display-graphic-p)))

(defun cj/music--image (path height)
  "Image spec for PATH scaled to HEIGHT px, or nil when it can't be displayed
\(no image support, an unreadable file, an unavailable format)."
  (when (and path (file-readable-p path))
    (ignore-errors
      (create-image path nil nil :height height :ascent 'center))))

(defun cj/music--bar-string (fill width)
  "Render a WIDTH-cell block progress bar with FILL filled cells.
FILL `indeterminate' (a live stream) renders an on-air marker instead."
  (if (eq fill 'indeterminate)
      (propertize "◉ on air" 'face 'cj/music-subtitle-face)
    (let ((n (max 0 (min fill width))))
      (concat (propertize (make-string n ?█) 'face 'cj/music-bar-fill-face)
              (propertize (make-string (- width n) ?░)
                          'face 'cj/music-bar-empty-face)))))

(defun cj/music--current-bar (track)
  "The progress bar for TRACK: a stream is indeterminate; a file fills from
mpv's percent-pos."
  (if (eq (emms-track-type track) 'url)
      (cj/music--bar-string 'indeterminate cj/music-bar-width)
    (let ((pct (cj/music--mpv-get-property "percent-pos")))
      (cj/music--bar-string
       (cj/music--bar-fill (and (numberp pct) pct) 100 cj/music-bar-width)
       cj/music-bar-width))))

(defun cj/music--hero-header (track)
  "Fancy now-playing hero for TRACK: cover image, serif amber title, subtitle,
and the progress bar, stacked vertically."
  (let* ((img (cj/music--image (cj/music-art--for-track track) cj/music-hero-size))
         (title (cj/music--display-name track (cj/music--radio-name-map)))
         (sub (if (eq (emms-track-type track) 'url)
                  "radio"
                (or (emms-track-get track 'info-album) ""))))
    (concat
     (if img (concat (propertize " " 'display img) "\n") "")
     (propertize title 'face (list :family cj/music-title-family
                                   :inherit 'cj/music-title-face))
     "\n"
     (if (string-empty-p sub)
         ""
       (concat (propertize sub 'face 'cj/music-subtitle-face) "\n"))
     (cj/music--current-bar track)
     "\n")))

;; Multi-line header overlay
(defvar-local cj/music--header-overlay nil
  "Overlay displaying the playlist header.")

(defun cj/music--playlist-string ()
  "The \"Playlist : NAME (N)\" header line."
  (let ((pl-name (if cj/music-playlist-file
                     (file-name-sans-extension
                      (file-name-nondirectory cj/music-playlist-file))
                   "Untitled"))
        (track-count (count-lines (point-min) (point-max))))
    (concat (propertize "Playlist" 'face 'cj/music-header-face)
            (propertize " : " 'face 'cj/music-header-face)
            (propertize (format "%s (%d)" pl-name track-count)
                        'face 'cj/music-header-value-face)
            "\n")))

(defun cj/music--controls-string ()
  "The Mode / Keys / Radio control lines and the closing full-width rule.
The rule uses a resize-safe :align-to span, not a hardcoded character count."
  (let ((mode-indicator
         (lambda (key label active)
           (let ((face (if active 'cj/music-mode-on-face 'cj/music-mode-off-face)))
             (propertize (format "[%s] %s" key label) 'face face)))))
    (concat
     (propertize "Mode    " 'face 'cj/music-header-face)
     (propertize " : " 'face 'cj/music-header-face)
     (funcall mode-indicator "r" "repeat" (bound-and-true-p emms-repeat-playlist)) "  "
     (funcall mode-indicator "s" "single" (bound-and-true-p emms-repeat-track)) "  "
     (funcall mode-indicator "z" "random" (bound-and-true-p emms-random-playlist)) "  "
     (funcall mode-indicator "x" "consume" cj/music-consume-mode) "\n"
     (propertize "Keys    " 'face 'cj/music-header-face)
     (propertize " : " 'face 'cj/music-header-face)
     (propertize "a:add  c:clear  L:load  v:save  S:stop  SPC:pause  <>:skip  ↑↓:move  C-↑↓:reorder  q:dismiss"
                 'face 'cj/music-keyhint-face) "\n"
     (propertize "Radio   " 'face 'cj/music-header-face)
     (propertize " : " 'face 'cj/music-header-face)
     (propertize "n:by name  t:by tag  m:enter manually"
                 'face 'cj/music-keyhint-face) "\n"
     (propertize " " 'face '(:strike-through t :inherit shadow)
                 'display '(space :align-to right))
     "\n\n")))

(defun cj/music--current-track ()
  "The selected track when one is playing or paused, else nil."
  (and emms-player-playing-p
       (ignore-errors (emms-playlist-current-selected-track))))

(defun cj/music--text-header ()
  "The plain text header: Playlist, Current, then the controls."
  (let ((now (cond ((not emms-player-playing-p) "Stopped")
                   (emms-player-paused-p "Paused")
                   (t (let ((track (cj/music--current-track)))
                        (if track
                            (concat (cj/music--display-name
                                     track (cj/music--radio-name-map))
                                    (cj/music--now-playing-suffix track))
                          "Playing"))))))
    (concat (cj/music--playlist-string)
            (propertize "Current " 'face 'cj/music-header-face)
            (propertize " : " 'face 'cj/music-header-face)
            (propertize now 'face 'cj/music-header-value-face) "\n"
            (cj/music--controls-string))))

(defun cj/music--fancy-header ()
  "The fancy header: Playlist, the now-playing hero when a track plays, then
the controls."
  (let ((track (cj/music--current-track)))
    (concat (cj/music--playlist-string)
            (if track
                (cj/music--hero-header track)
              (concat (propertize "Current " 'face 'cj/music-header-face)
                      (propertize " : " 'face 'cj/music-header-face)
                      (propertize (if emms-player-paused-p "Paused" "Stopped")
                                  'face 'cj/music-header-value-face)
                      "\n"))
            (cj/music--controls-string))))

(defun cj/music--header-text ()
  "Build the playlist header overlay string: fancy in a graphical frame with
`cj/music-fancy-ui' on, plain text otherwise."
  (if (cj/music--fancy-p)
      (cj/music--fancy-header)
    (cj/music--text-header)))

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

;; Progress-bar redraw timer and cover-art pre-warm (Phase 3).
(defvar cj/music--bar-timer nil
  "Repeating timer redrawing the progress bar while a track plays.")

(defun cj/music--bar-tick ()
  "Redraw the header when the player buffer is visible and a track is playing.
The timer keeps running while idle/paused; it just skips the redraw."
  (when (and emms-player-playing-p (not emms-player-paused-p)
             (get-buffer-window cj/music-playlist-buffer-name t))
    (cj/music--update-header)))

(defun cj/music--start-bar-timer (&rest _)
  "Start the progress-bar redraw timer if it is not already running."
  (unless cj/music--bar-timer
    (setq cj/music--bar-timer
          (run-at-time t cj/music-bar-interval #'cj/music--bar-tick))))

(defun cj/music--stop-bar-timer (&rest _)
  "Stop the progress-bar redraw timer."
  (when cj/music--bar-timer
    (cancel-timer cj/music--bar-timer)
    (setq cj/music--bar-timer nil)))

(defun cj/music--do-prewarm-art ()
  "Fetch the current track's cover art, then refresh the header so the fetched
art replaces the placeholder.  Blocks on the network; runs off an idle timer."
  (when-let ((track (cj/music--current-track)))
    (when (cj/music-art--ensure track)
      (cj/music--update-header))))

(defun cj/music--prewarm-art (&rest _)
  "Schedule a cover-art fetch for the current track during idle, so a slow
fetch never blocks playback start (the emms-player-started-hook).  A no-op
unless fancy."
  (when (cj/music--fancy-p)
    (run-with-idle-timer 0.2 nil #'cj/music--do-prewarm-art)))

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

  ;; Fancy render: run the bar timer only across a playing span, and pre-warm
  ;; the current track's cover art off the redisplay path.
  (add-hook 'emms-player-started-hook #'cj/music--start-bar-timer)
  (add-hook 'emms-player-started-hook #'cj/music--prewarm-art)
  (add-hook 'emms-player-stopped-hook #'cj/music--stop-bar-timer)
  (add-hook 'emms-player-finished-hook #'cj/music--stop-bar-timer)

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
        ("v" . cj/music-playlist-save)
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
  "Queue and play a radio station from a hand-entered NAME and URL.
The station becomes a url track in the playlist (NAME as its title) and
playback starts.  Nothing is written to disk — save the queue with the normal
playlist save, where NAME pre-fills the prompt."
  (interactive
   (list (read-string "Radio station name: ")
         (read-string "Stream URL: ")))
  (when (string-empty-p name)
    (user-error "Radio station name cannot be empty"))
  (when (string-empty-p url)
    (user-error "Stream URL cannot be empty"))
  (cj/emms--setup)
  (cj/music-radio--enqueue-and-play
   (list (cj/music-radio--station-track (list :name name :url url))))
  (message "Queued radio station: %s" name))

;; The manual name+URL creator is bound to m in the radio row below (see the
;; with-eval-after-load block near the radio-browser lookup), not R.

;; --------------------------- Radio-browser Lookup ----------------------------
;; Search radio-browser.info and queue a selection as playing url tracks, each
;; carrying its station metadata as track properties.  Nothing is written at
;; pick time; the playlist save writes the metadata back out as .m3u comment
;; lines.  Spec: docs/specs/2026-07-06-radio-browser-lookup-spec.org.  The
;; pure pieces (parse / track-build / format) carry the tests; the network GET
;; and the interactive command are exercised live.

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
  "Directory radio playlists are saved to (the radio home).
The playlist save targets it when every track in the queue is a stream.")

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

(defun cj/music-radio--station-track (st)
  "Return an EMMS url track for station ST, or nil when it has no stream URL.
The track carries the station name as `info-title' plus `radio-uuid' and
`radio-favicon' properties, so display names and cover art need no .m3u on
disk; the playlist save writes the same metadata back out as comment lines.
Newlines in the external name/favicon are flattened so they can't inject
extra .m3u lines at save time."
  (when-let ((url (cj/music-radio--station-url st)))
    (let ((track (emms-track 'url url))
          (name (replace-regexp-in-string "[\r\n]+" " "
                                          (or (plist-get st :name) "Radio")))
          (uuid (plist-get st :stationuuid))
          (favicon (plist-get st :favicon)))
      (emms-track-set track 'info-title name)
      (when (and (stringp uuid) (not (string-empty-p uuid)))
        (emms-track-set track 'radio-uuid uuid))
      (when (and (stringp favicon) (not (string-empty-p favicon)))
        (emms-track-set track 'radio-favicon
                        (replace-regexp-in-string "[\r\n]+" " " favicon)))
      track)))

(defun cj/music-radio--enqueue-and-play (tracks)
  "Append TRACKS to the playlist buffer and play the first of them.
Interrupts whatever is playing; the rest of the queue is left in place.  A nil
TRACKS is a no-op."
  (when tracks
    (cj/emms--setup)
    (with-current-buffer (cj/music--ensure-playlist-buffer)
      (let ((first-pos nil))
        (save-excursion
          (dolist (tr tracks)
            (goto-char (point-max))
            (unless first-pos (setq first-pos (point)))
            (emms-playlist-insert-track tr)))
        (emms-playlist-select first-pos)))
    (when emms-player-playing-p (emms-stop))
    (emms-start)))

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

(defun cj/music-radio--search-and-play (query field)
  "Search radio-browser for QUERY on FIELD, pick stations, then queue and play.
FIELD is \"name\" or \"tag\".  Lists matching stations (annotated with codec,
bitrate, country, votes, and tags), lets you pick several one at a time, adds
each to the playlist as a url track carrying its station metadata, and plays
the first pick (interrupting whatever was playing).  Nothing is written to
disk; save the queue with the normal playlist save, where the station name
pre-fills the prompt."
  (when (string-empty-p (string-trim query))
    (user-error "Empty search"))
  (cj/emms--setup)
  (let* ((stations (cj/music-radio--search query field))
         (candidates (cj/music-radio--candidates stations)))
    (unless candidates
      (user-error "No stations found for %s %S" field query))
    (let ((chosen (cj/music-radio--pick-loop candidates)))
      (unless chosen
        (user-error "No stations selected"))
      (let ((tracks (delq nil (mapcar #'cj/music-radio--station-track chosen)))
            (skipped (cl-loop for st in chosen
                              unless (cj/music-radio--station-url st)
                              collect (or (plist-get st :name) "(unnamed)"))))
        (cj/music-radio--enqueue-and-play tracks)
        (message "Queued %d station%s%s%s"
                 (length tracks)
                 (if (= (length tracks) 1) "" "s")
                 (if skipped
                     (format ", skipped %d with no URL (%s)"
                             (length skipped) (string-join skipped ", "))
                   "")
                 (if tracks
                     (format " — playing %s"
                             (emms-track-get (car tracks) 'info-title))
                   ""))))))

(defun cj/music-radio-search-by-name (query)
  "Search radio-browser.info by station name, then queue and play a selection."
  (interactive "sRadio search (name): ")
  (cj/music-radio--search-and-play query "name"))

(defun cj/music-radio-search-by-tag (tag)
  "Search radio-browser.info by tag/genre, then queue and play a selection."
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
A url with a station uuid — the track's `radio-uuid' property, else a
#RADIOBROWSERUUID in ENTRIES — keys on the uuid so a station shares one cached
logo; any other url keys on a hash of its address; a file keys on a hash of
its path."
  (let ((name (emms-track-name track)))
    (if (eq (emms-track-type track) 'url)
        (let ((uuid (or (emms-track-get track 'radio-uuid)
                        (plist-get (cdr (assoc name entries)) :uuid))))
          (if (and (stringp uuid) (not (string-empty-p uuid)))
              uuid
            (concat "url-" (sha1 name))))
      (concat "file-" (sha1 name)))))

(defun cj/music-art--favicon-url (track &optional entries)
  "Direct favicon image URL for a url TRACK, or nil.
The track's `radio-favicon' property wins, then its captured
#RADIOBROWSERFAVICON from ENTRIES.  A station with only a uuid resolves via a
byuuid lookup elsewhere; a file track has no favicon URL."
  (when (eq (emms-track-type track) 'url)
    (let ((fav (or (emms-track-get track 'radio-favicon)
                   (plist-get (cdr (assoc (emms-track-name track) entries))
                              :favicon))))
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
                       (let ((uuid (or (emms-track-get track 'radio-uuid)
                                       (plist-get (cdr (assoc (emms-track-name track)
                                                              entries))
                                                  :uuid))))
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

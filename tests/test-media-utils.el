;;; test-media-utils.el --- Tests for media-utils.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Coverage for the media playback/download helpers.  All external boundaries
;; are mocked: `executable-find' decides which players are present, and
;; `start-process' / `start-process-shell-command' capture the command instead
;; of launching anything.  The tests assert the integration logic — which
;; players are reported available, and how the play/download command strings
;; are built — not that mpv/yt-dlp themselves work.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'media-utils)

;; media-utils.el declares videos-dir with a bare (defvar videos-dir), which is
;; only file-local; declare it special here so the let-binding below is dynamic
;; and reaches cj/yt-dl-it (which reads videos-dir for default-directory).
(defvar videos-dir nil)

;; ---------------------- cj/get-available-media-players -----------------------

(ert-deftest test-media-get-available-players-filters-by-executable ()
  "Normal: only players whose :command is on PATH are reported."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd &rest _) (and (member cmd '("mpv" "vlc")) cmd))))
    (let ((result (cj/get-available-media-players)))
      (should (memq 'mpv result))
      (should (memq 'vlc result))
      (should-not (memq 'mplayer result)))))

(ert-deftest test-media-get-available-players-none-installed ()
  "Boundary: with nothing on PATH, the list is empty."
  (cl-letf (((symbol-function 'executable-find) (lambda (_ &rest _) nil)))
    (should-not (cj/get-available-media-players))))

;; ----------------------------- cj/media-play-it ------------------------------

(ert-deftest test-media-play-it-direct-playback-command ()
  "Normal: a player that needs no stream URL launches an argv process, no yt-dlp."
  (let (captured cj/default-media-player)
    (setq cj/default-media-player 'mpv)
    (cl-letf (((symbol-function 'executable-find) (lambda (_ &rest _) "/usr/bin/mpv"))
              ((symbol-function 'start-process)
               (lambda (&rest args) (setq captured args) 'proc))
              ((symbol-function 'set-process-sentinel) #'ignore)
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'cj/log-silently) #'ignore))
      (cj/media-play-it "https://example.com/v"))
    ;; (NAME BUFFER PROGRAM . ARGS) -- program + args are the argv.
    (should (equal (nthcdr 2 captured) '("mpv" "https://example.com/v")))
    (should-not (member "yt-dlp" captured))))

(ert-deftest test-media-play-it-stream-url-resolves-via-yt-dlp ()
  "Normal: a stream-URL player resolves through a yt-dlp -g capture, then
launches the player with the resolved URL as argv -- no shell either step."
  (let (yt-argv captured cj/default-media-player)
    (setq cj/default-media-player 'vlc)
    (cl-letf (((symbol-function 'executable-find) (lambda (_ &rest _) "/usr/bin/x"))
              ((symbol-function 'call-process)
               (lambda (program _infile _dest _display &rest args)
                 (setq yt-argv (cons program args))
                 (insert "https://stream.example.com/resolved\n")
                 0))
              ((symbol-function 'start-process)
               (lambda (&rest args) (setq captured args) 'proc))
              ((symbol-function 'set-process-sentinel) #'ignore)
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'cj/log-silently) #'ignore))
      (cj/media-play-it "https://example.com/v"))
    (should (equal yt-argv
                   '("yt-dlp" "-f" "22/18/best" "-g" "https://example.com/v")))
    (should (equal (nthcdr 2 captured)
                   '("vlc" "https://stream.example.com/resolved")))))

(ert-deftest test-media-play-it-missing-player-errors ()
  "Error: an unavailable player command signals an error before launching."
  (let (cj/default-media-player)
    (setq cj/default-media-player 'mpv)
    (cl-letf (((symbol-function 'executable-find) (lambda (_ &rest _) nil)))
      (should-error (cj/media-play-it "https://example.com/v")))))

(ert-deftest test-media-play-it-missing-yt-dlp-errors ()
  "Error: a stream-URL player with no yt-dlp on PATH aborts before resolving."
  (let (cj/default-media-player)
    (setq cj/default-media-player 'vlc)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (cmd &rest _) (and (equal cmd "vlc") "/usr/bin/vlc"))))
      (should-error (cj/media-play-it "https://example.com/v")))))

(ert-deftest test-media-play-it-yt-dlp-failure-errors ()
  "Error: a non-zero yt-dlp exit surfaces as an error, player never launches."
  (let (launched cj/default-media-player)
    (setq cj/default-media-player 'vlc)
    (cl-letf (((symbol-function 'executable-find) (lambda (_ &rest _) "/usr/bin/x"))
              ((symbol-function 'call-process)
               (lambda (&rest _) (insert "ERROR: no video\n") 1))
              ((symbol-function 'start-process)
               (lambda (&rest _) (setq launched t) 'proc))
              ((symbol-function 'message) #'ignore))
      (should-error (cj/media-play-it "https://example.com/v")))
    (should-not launched)))

(ert-deftest test-media-play-it-yt-dlp-empty-output-errors ()
  "Error: a zero-exit yt-dlp with no output still errors, player never launches."
  (let (launched cj/default-media-player)
    (setq cj/default-media-player 'vlc)
    (cl-letf (((symbol-function 'executable-find) (lambda (_ &rest _) "/usr/bin/x"))
              ((symbol-function 'call-process) (lambda (&rest _) 0))
              ((symbol-function 'start-process)
               (lambda (&rest _) (setq launched t) 'proc))
              ((symbol-function 'message) #'ignore))
      (should-error (cj/media-play-it "https://example.com/v")))
    (should-not launched)))

;; ------------------------------- cj/yt-dl-it ---------------------------------

(ert-deftest test-media-yt-dl-it-errors-without-yt-dlp ()
  "Error: a missing yt-dlp aborts the download."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd &rest _) (unless (equal cmd "yt-dlp") "/usr/bin/x"))))
    (should-error (cj/yt-dl-it "https://example.com/v"))))

(ert-deftest test-media-yt-dl-it-errors-without-tsp ()
  "Error: yt-dlp present but tsp missing aborts the download."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd &rest _) (unless (equal cmd "tsp") "/usr/bin/x"))))
    (should-error (cj/yt-dl-it "https://example.com/v"))))

(ert-deftest test-media-yt-dl-it-builds-tsp-yt-dlp-process ()
  "Normal: with both tools present, the URL is queued via tsp + yt-dlp."
  (let (captured (videos-dir "/tmp/videos"))
    (cl-letf (((symbol-function 'executable-find) (lambda (_ &rest _) "/usr/bin/x"))
              ((symbol-function 'start-process)
               (lambda (&rest args) (setq captured args) 'proc))
              ((symbol-function 'set-process-sentinel) #'ignore)
              ((symbol-function 'message) #'ignore))
      (cj/yt-dl-it "https://example.com/v"))
    (should (member "tsp" captured))
    (should (member "yt-dlp" captured))
    (should (member "https://example.com/v" captured))))

;; ---------------------------- cj/select-media-player -------------------------

(ert-deftest test-media-select-player-normal-sets-default-on-choice ()
  "Normal: choosing an available player updates `cj/default-media-player'."
  (let ((cj/media-players '((mpv . (:name "mpv" :command "mpv"))
                            (vlc . (:name "VLC" :command "vlc"))))
        (cj/default-media-player 'vlc))
    (cl-letf (((symbol-function 'cj/get-available-media-players)
               (lambda () '(mpv vlc)))
              ((symbol-function 'completing-read) (lambda (&rest _) "mpv"))
              ((symbol-function 'message) #'ignore))
      (cj/select-media-player)
      (should (eq cj/default-media-player 'mpv)))))

(ert-deftest test-media-select-player-boundary-no-match-keeps-default ()
  "Boundary: a selection matching no player leaves the default unchanged."
  (let ((cj/media-players '((mpv . (:name "mpv" :command "mpv"))
                            (vlc . (:name "VLC" :command "vlc"))))
        (cj/default-media-player 'vlc))
    (cl-letf (((symbol-function 'cj/get-available-media-players)
               (lambda () '(mpv vlc)))
              ((symbol-function 'completing-read) (lambda (&rest _) "Nonexistent"))
              ((symbol-function 'message) #'ignore))
      (cj/select-media-player)
      (should (eq cj/default-media-player 'vlc)))))

(provide 'test-media-utils)
;;; test-media-utils.el ends here

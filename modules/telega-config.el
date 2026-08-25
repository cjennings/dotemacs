;;; telega-config.el --- Telega Telegram client config -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/P.
;; Load shape: eager.
;; Eager reason: none; optional Telegram client that registers a keymap, a
;;   command-loaded deferral candidate.
;; Top-level side effects: registers a telega keymap under cj/custom-keymap,
;;   package config.
;; Runtime requires: keybindings.
;; Direct test load: yes (requires keybindings explicitly).
;;
;; Configures telega.el (https://github.com/zevlg/telega.el) as an
;; in-Emacs Telegram client.
;;
;; TDLib (Telegram Database Library) runs in a docker container via
;; `telega-use-docker' so a fresh-clone install does not need a
;; system-level TDLib build.  The image is built locally from
;; =docker/telega-server/Dockerfile= with =make telega-image= (see the
;; pin section below for why it is not pulled from the registry);
;; =scripts/setup-telega.sh= covers the rest of a fresh clone.
;;
;; First-run auth (phone number + Telegram verification code) is
;; interactive and happens inside `M-x telega'.  This module does not
;; script it.
;;
;; Install:
;;
;;   M-x package-refresh-contents
;;   M-x package-install RET telega
;;
;; The refresh is important.  MELPA rotates dated snapshot tarballs out
;; from under the cached archive index periodically, so if the local
;; archive-contents file points at a snapshot that no longer exists on
;; the server the install fails with a 404.  Refreshing pulls a current
;; index.  This module deliberately sets `:ensure nil' so a stale
;; archive doesn't take Emacs init down at startup; if the package
;; isn't installed yet, `C-; T' surfaces a clear "install telega" error
;; until the install runs once.
;;
;; Launcher: =C-; T= (mnemonic: Telegram).  Previously `C-; G' because
;; `T' was contested between org-table and transcription menus -- both
;; have been moved (org-table flattened under `C-; O', transcription
;; cleared to M-x), so `T' is now telega's outright.

;;; Code:

(require 'keybindings)
(require 'system-lib)                   ; cj/log-silently, used by the death alert

(use-package telega
  :defer t
  :ensure nil
  :commands (telega)
  :custom
  (telega-use-docker t)
  :config
  ;; Without this, incoming Telegram messages are invisible unless their
  ;; buffer is on screen -- telega ships desktop notifications but leaves
  ;; the mode off by default.  Runs at telega load (M-x telega), respects
  ;; telega's own per-chat mute settings.  From the 2026-06 config audit;
  ;; routing through a shared messenger notifier is the unification task.
  (telega-notifications-mode 1))

;; --------------------------- telega Docker Image Pin -------------------------
;; telega picks its container image in `telega-docker--image-name', which only
;; pins to a version tag when `telega-tdlib-min-version' equals
;; `telega-tdlib-max-version' and the version ends in ".0".  Here min is
;; "1.8.66" and max is nil, so that test never passes and the image is always
;; "zevlg/telega-server:latest" -- a floating tag.  The elpa package is fixed
;; at whatever version was installed, so the server can be replaced underneath
;; a static elisp without anything announcing it.
;;
;; The pin used to be a registry digest.  It became a local tag on 2026-08-25:
;; the telega package raised its tdlib floor to 1.8.66, and upstream's only
;; image at that version fails to start (libglycin missing,
;; zevlg/telega.el#596).  docker/telega-server/Dockerfile derives a working
;; image from that upstream digest plus the one missing package, and
;; `make telega-image' builds it under the tag below.  The digest guarantee
;; now lives in the Dockerfile's FROM line; the Makefile owns the tag and a
;; test holds this default equal to it.  Set to nil to hand the choice back
;; to telega.

(defcustom cj/telega-docker-image
  "cj/telega-server:1.8.66-glycin"
  "Container image reference for `telega-server', or nil for telega's default.
The default names the image `make telega-image' builds locally from
docker/telega-server/Dockerfile, whose base is pinned by upstream digest.
It must match TELEGA_IMAGE in the Makefile; `cj/telega' refuses to launch
when the image is not present, since docker would otherwise try to pull a
local-only tag from the registry and fail confusingly."
  :type '(choice (const :tag "Let telega infer the image" nil)
                 (string :tag "Image reference"))
  :group 'telega-docker)

(defun cj/--telega-docker-pinned-image ()
  "Return the configured image pin, or nil when none is usable.
A blank or non-string setting yields nil rather than reaching the docker
command line, where it would fail in a way that looks unrelated to this."
  (when (stringp cj/telega-docker-image)
    (let ((pin (string-trim cj/telega-docker-image)))
      (unless (string-empty-p pin) pin))))

(defun cj/--telega-docker-image-name (orig-fun &rest args)
  "Return the pinned telega-server image, else call ORIG-FUN with ARGS.
`:around' advice on `telega-docker--image-name', so clearing the pin
restores telega's own inference instead of breaking the image name."
  (or (cj/--telega-docker-pinned-image)
      (apply orig-fun args)))

(with-eval-after-load 'telega-util
  (advice-add 'telega-docker--image-name :around #'cj/--telega-docker-image-name))

;; ------------------------- telega-server Death Alert -------------------------
;; telega's own sentinel reports an abnormal server exit with `message', which
;; scrolls out of the echo area unseen.  That is how a dead server reads as a
;; quiet Telegram: the scan stops partway and the unscanned chats look like
;; chats with nothing in them.  It has happened twice (2026-07-10, 2026-07-27),
;; caught both times only because something downstream noticed.  A desktop
;; notification makes the death itself visible.

(declare-function notifications-notify "notifications")

(defun cj/--telega-server-death-p (status)
  "Return non-nil when exit STATUS means the server died abnormally.
Any non-zero integer counts, which covers both a non-zero exit code and a
fatal signal number (`process-exit-status' reports SIGSEGV as 11).  A
non-integer STATUS returns nil rather than signalling: this runs inside
telega's sentinel, where an error would abort telega's own cleanup."
  (and (integerp status)
       (not (zerop status))))

(defun cj/--telega-server-exit-status (proc)
  "Return PROC's exit status, or nil when it can't be determined.
Guarded because the sentinel hands over whatever process object it has,
and a bad one must not break telega's status handling."
  (condition-case nil
      (and (processp proc) (process-exit-status proc))
    (error nil)))

(defun cj/--telega-server-death-body (status event)
  "Build the notification body for a server death with STATUS and EVENT.
EVENT is the sentinel's event string, which carries a trailing newline that
would render as dead space in a desktop notification."
  (let ((detail (string-trim (or event ""))))
    (concat (format "telega-server died (status %s). " status)
            (unless (string-empty-p detail) (concat detail ". "))
            "Telegram coverage is down until it restarts.")))

(defun cj/--telega-server-send-notification (title body)
  "Deliver a desktop notification with TITLE and BODY.
Prefers the external notify script (persistent, so it waits rather than
auto-dismissing while away), falling back to `notifications-notify'.
Mirrors `cj/slack--send-notification'."
  (let ((script (executable-find "notify")))
    (if script
        (start-process "telega-death-notify" nil script "fail" title body "--persist")
      (unless (fboundp 'notifications-notify)
        (require 'notifications))
      (notifications-notify :title title :body body))))

(defun cj/--telega-server-notify-death (proc event)
  "Notify when the telega-server PROC dies abnormally.  EVENT is its event string.
Installed as `:after' advice on `telega-server--sentinel'.  Silent on a
clean exit, so quitting telega deliberately never pages.

The whole body is guarded: a notifier failure here would otherwise escape
into telega's sentinel and abort its status handling and relogin path."
  (condition-case err
      (let ((status (cj/--telega-server-exit-status proc)))
        (when (cj/--telega-server-death-p status)
          (cj/--telega-server-send-notification
           "Telegram: telega-server died"
           (cj/--telega-server-death-body status event))))
    (error
     (cj/log-silently
      (format "telega death notify failed: %s" (error-message-string err))))))

;; Named, never a lambda: anonymous advice can't be `advice-remove'd by
;; reference, so a live daemon keeps running it after the source stops
;; installing it.
(with-eval-after-load 'telega-server
  (advice-add 'telega-server--sentinel :after #'cj/--telega-server-notify-death))

(defun cj/--telega-docker-image-present-p (image)
  "Return non-nil when IMAGE exists in the local docker image store.
Uses `docker image inspect' rather than `docker images': the listing hides
digest-pulled and untagged images, and this check exists because that
listing lied once.  Any failure (docker absent, daemon down) reads as
not-present, which routes the user to the same make target."
  (condition-case nil
      (zerop (call-process "docker" nil nil nil "image" "inspect" image))
    (error nil)))

(defun cj/--telega-missing-image-message (image)
  "Return the user-facing message for a pinned IMAGE that is not built yet."
  (format "telega-server image %s is not built -- run `make telega-image' in %s"
          image (abbreviate-file-name user-emacs-directory)))

(defun cj/telega ()
  "Launch telega.el with a helpful message when it isn't installed yet.

The =telega= Emacs package uses =:ensure nil= in this config so a
stale MELPA archive index can't take startup down with a 404.  The
trade-off: a fresh clone needs a one-time install before this
launcher works.  Without this wrapper, the autoload stub fails with
the cryptic =Cannot open load file: telega=; with it, the user gets
pointed at =scripts/setup-telega.sh= and the manual fallback.

When `cj/telega-docker-image' is set, the image must already be built:
it is a local tag, so a missing one would send docker to the registry
for something that was never there.  The check is skipped with no pin,
where telega infers and pulls its own image."
  (interactive)
  (unless (or (featurep 'telega)
              (locate-library "telega"))
    (user-error
     (concat "telega not installed -- run scripts/setup-telega.sh, "
             "or `M-x package-install RET telega'")))
  (let ((image (cj/--telega-docker-pinned-image)))
    (when (and image (not (cj/--telega-docker-image-present-p image)))
      (user-error "%s" (cj/--telega-missing-image-message image))))
  (telega))

(cj/register-command "T" #'cj/telega)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; T" "telegram (telega)"))

(provide 'telega-config)
;;; telega-config.el ends here

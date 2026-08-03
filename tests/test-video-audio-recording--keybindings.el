;;; test-video-audio-recording--keybindings.el --- recording toggle keybinding placement -*- lexical-binding: t; -*-

;;; Commentary:
;; The two recording toggles get a fast chord alongside the C-; r prefix: F9
;; starts/stops video, S-F9 starts/stops audio.
;;
;; Reaching them from inside an EAT buffer turns on which key categories each
;; input mode claims.  Semi-char mode -- the default, and where agent buffers
;; sit -- is built from (:ascii :arrow :navigation) and never claims function
;; keys, so F9 already fell through to the global map there.  Char mode adds
;; :function, binding f1 through f63 to `eat-self-input', and it is a minor
;; mode, so its map outranks `eat-mode-map'.  The char-mode entries are the
;; load-bearing ones; the semi-char entry is belt-and-braces.
;;
;; :function claims only the unmodified keys, which is why getting this wrong
;; split the pair rather than breaking it outright: S-F9 toggled audio in a
;; char-mode buffer while F9 went to the program under the cursor.
;;
;; These tests require eat first so the module's `with-eval-after-load' fires.
;; The char-mode cases resolve through `key-binding' in a fixture that
;; reproduces minor-mode precedence, because reading a binding back out of the
;; map the module just wrote proves nothing about which map wins on a keypress.

;;; Code:

(require 'ert)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'eat)
(require 'video-audio-recording)

;;; Normal

(ert-deftest test-video-audio-recording-f9-bound-globally ()
  "Normal: F9 toggles video recording, S-F9 toggles audio recording."
  (should (eq (lookup-key (current-global-map) (kbd "<f9>"))
              #'cj/video-recording-toggle))
  (should (eq (lookup-key (current-global-map) (kbd "S-<f9>"))
              #'cj/audio-recording-toggle)))

(ert-deftest test-video-audio-recording-f9-bound-in-eat-semi-char-mode-map ()
  "Normal: both chords are bound in `eat-semi-char-mode-map'.
Redundant rather than load-bearing: semi-char is built without :function, so a
function key already falls through to the global map.  Asserted anyway so the
entry cannot be dropped silently while the comment explaining it stays."
  (should (eq (keymap-lookup eat-semi-char-mode-map "<f9>")
              #'cj/video-recording-toggle))
  (should (eq (keymap-lookup eat-semi-char-mode-map "S-<f9>")
              #'cj/audio-recording-toggle)))

(ert-deftest test-video-audio-recording-f9-bound-in-eat-mode-map ()
  "Normal: both chords are bound in `eat-mode-map', the major-mode map every
EAT buffer carries regardless of input mode."
  (should (eq (keymap-lookup eat-mode-map "<f9>")
              #'cj/video-recording-toggle))
  (should (eq (keymap-lookup eat-mode-map "S-<f9>")
              #'cj/audio-recording-toggle)))

(ert-deftest test-video-audio-recording-f9-bound-in-eat-char-mode-maps ()
  "Normal: both chords are bound in the two char-mode maps.
Char mode is built with EAT's :function category, which binds f1 through f63
to `eat-self-input'.  These entries are what override that."
  (dolist (map (list eat-char-mode-map eat-eshell-char-mode-map))
    (should (eq (keymap-lookup map "<f9>") #'cj/video-recording-toggle))
    (should (eq (keymap-lookup map "S-<f9>") #'cj/audio-recording-toggle))))

;;; Boundary

(ert-deftest test-video-audio-recording-f9-chords-are-distinct ()
  "Boundary: the shifted and unshifted chords resolve to different commands.
A copy-paste binding both to the same toggle would satisfy every
binding-is-present assertion above, so assert the difference directly."
  (should-not (eq (lookup-key (current-global-map) (kbd "<f9>"))
                  (lookup-key (current-global-map) (kbd "S-<f9>")))))

(defun test-video-audio-recording--in-char-mode (body)
  "Run BODY in a buffer wired the way a live EAT char-mode buffer is.
`eat--char-mode' is a minor mode, so its map is consulted ahead of the
major-mode map.  Reproducing that ordering is the point: reading a binding
back out of the map the module just wrote proves nothing about which map wins
when a key is actually pressed."
  (with-temp-buffer
    (use-local-map eat-mode-map)
    (let ((minor-mode-overriding-map-alist
           (list (cons 'eat--char-mode eat-char-mode-map)))
          (eat--char-mode t))
      (funcall body))))

(ert-deftest test-video-audio-recording-f9-resolves-in-char-mode ()
  "Boundary: both chords resolve to the toggles through the real precedence
chain in a char-mode buffer.  Before this override F9 resolved to
`eat-self-input' and went to the program under the cursor, while S-F9 reached
Emacs — so the pair silently split, audio recording and video not."
  (test-video-audio-recording--in-char-mode
   (lambda ()
     (should (eq (key-binding (kbd "<f9>")) #'cj/video-recording-toggle))
     (should (eq (key-binding (kbd "S-<f9>")) #'cj/audio-recording-toggle)))))

;;; Error

(ert-deftest test-video-audio-recording-char-mode-fixture-really-is-char-mode ()
  "Error (positive control): the char-mode fixture genuinely puts EAT's map in
front.  F8 sits in the same :function category as F9 and this module never
touches it, so it must still reach `eat-self-input'.  If it resolves anywhere
else the fixture is inert, and the resolution test above would pass without
ever consulting `eat-char-mode-map' — which is precisely how the first cut of
this file missed that F9 was being swallowed there."
  (test-video-audio-recording--in-char-mode
   (lambda ()
     (should (eq (key-binding (kbd "<f8>")) #'eat-self-input)))))

(ert-deftest test-video-audio-recording-f9-targets-are-commands ()
  "Error: a key bound to a non-interactive function fails at press time with a
`commandp' error rather than at load, so assert both targets are real commands."
  (should (commandp (lookup-key (current-global-map) (kbd "<f9>"))))
  (should (commandp (lookup-key (current-global-map) (kbd "S-<f9>")))))

(ert-deftest test-video-audio-recording-prefix-bindings-still-reachable ()
  "Error/regression (positive control): the fast chords must not disturb the
C-; r prefix path.  Without this, deleting the prefix map outright would leave
every assertion above green."
  (should (eq (keymap-lookup cj/record-map "v") #'cj/video-recording-toggle))
  (should (eq (keymap-lookup cj/record-map "a") #'cj/audio-recording-toggle))
  (should (eq (keymap-lookup cj/custom-keymap "r") cj/record-map)))

(provide 'test-video-audio-recording--keybindings)
;;; test-video-audio-recording--keybindings.el ends here

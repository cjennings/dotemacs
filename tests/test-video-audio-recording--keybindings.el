;;; test-video-audio-recording--keybindings.el --- recording toggle keybinding placement -*- lexical-binding: t; -*-

;;; Commentary:
;; The two recording toggles live only under the C-; r prefix.  They used to
;; have a fast chord too: F9 for video, S-F9 for audio, claimed globally and in
;; every EAT map so char mode's :function category could not swallow them.
;;
;; That chord went away on 2026-09-22.  With Fn Lock on the Framework 13, the
;; F-key row sends plain function keys, so a bare F9 press started a screen
;; recording.  The pair is removed as a unit: a lone S-F9 audio chord on a live
;; F-key row is one fat-finger from an unwanted recording, and the module
;; treated the two as one feature.
;;
;; These tests pin the removal.  The char-mode case resolves through
;; `key-binding' in a fixture that reproduces minor-mode precedence, because
;; reading a map the module no longer writes proves nothing about which map wins
;; on a keypress.  They require eat first so any `with-eval-after-load' the
;; module still carries would have fired.

;;; Code:

(require 'ert)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'eat)
(require 'video-audio-recording)

(defconst test-video-audio-recording--toggles
  '(cj/video-recording-toggle cj/audio-recording-toggle)
  "The two commands the retired F9 chords used to reach.")

(defun test-video-audio-recording--bound-to-toggle-p (binding)
  "Return non-nil when BINDING is one of the recording toggles."
  (memq binding test-video-audio-recording--toggles))

;;; Normal

(ert-deftest test-video-audio-recording-f9-not-bound-globally ()
  "Normal/regression: neither F9 nor S-F9 reaches a recording toggle globally."
  (should-not (test-video-audio-recording--bound-to-toggle-p
               (lookup-key (current-global-map) (kbd "<f9>"))))
  (should-not (test-video-audio-recording--bound-to-toggle-p
               (lookup-key (current-global-map) (kbd "S-<f9>")))))

(ert-deftest test-video-audio-recording-f9-not-bound-in-eat-maps ()
  "Normal/regression: none of the four EAT maps carries a recording toggle on
F9 or S-F9.  The module used to write all four so the chord survived char
mode; the removal has to reach every one of them."
  (dolist (map (list eat-semi-char-mode-map eat-mode-map
                     eat-char-mode-map eat-eshell-char-mode-map))
    (should-not (test-video-audio-recording--bound-to-toggle-p
                 (keymap-lookup map "<f9>")))
    (should-not (test-video-audio-recording--bound-to-toggle-p
                 (keymap-lookup map "S-<f9>")))))

;;; Boundary

(defun test-video-audio-recording--in-char-mode (body)
  "Run BODY in a buffer wired the way a live EAT char-mode buffer is.
`eat--char-mode' is a minor mode, so its map is consulted ahead of the
major-mode map.  Reproducing that ordering is the point: the assertion is
about which map wins when a key is actually pressed."
  (with-temp-buffer
    (use-local-map eat-mode-map)
    (let ((minor-mode-overriding-map-alist
           (list (cons 'eat--char-mode eat-char-mode-map)))
          (eat--char-mode t))
      (funcall body))))

(ert-deftest test-video-audio-recording-f9-reaches-program-in-char-mode ()
  "Boundary: in a char-mode buffer F9 goes to the program under the cursor
again.  EAT's :function category binds f1 through f63 to `eat-self-input';
the module's override used to sit in front of it, and now nothing does."
  (test-video-audio-recording--in-char-mode
   (lambda ()
     (should (eq (key-binding (kbd "<f9>")) #'eat-self-input)))))

;;; Error

(ert-deftest test-video-audio-recording-char-mode-fixture-really-is-char-mode ()
  "Error (positive control): the char-mode fixture genuinely puts EAT's map in
front.  F8 sits in the same :function category as F9 and this module never
touched it, so it must reach `eat-self-input'.  If it resolves anywhere else
the fixture is inert and the F9 assertion above passes for the wrong reason."
  (test-video-audio-recording--in-char-mode
   (lambda ()
     (should (eq (key-binding (kbd "<f8>")) #'eat-self-input)))))

(ert-deftest test-video-audio-recording-prefix-bindings-still-reachable ()
  "Error/regression (positive control): removing the fast chords must leave
the C-; r prefix path intact.  Without this, deleting the toggles outright
would leave every not-bound assertion above green."
  (should (eq (keymap-lookup cj/record-map "v") #'cj/video-recording-toggle))
  (should (eq (keymap-lookup cj/record-map "a") #'cj/audio-recording-toggle))
  (should (eq (keymap-lookup cj/custom-keymap "r") cj/record-map))
  (should (commandp #'cj/video-recording-toggle))
  (should (commandp #'cj/audio-recording-toggle)))

(provide 'test-video-audio-recording--keybindings)
;;; test-video-audio-recording--keybindings.el ends here

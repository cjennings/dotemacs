;;; test-modeline-config-segments.el --- small modeline segments -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the small pure segment helpers added in the 2026-07-01
;; modeline overhaul: macro indicator, remote-host tag, narrowing
;; indicator, position/region info, and the padding space.

;;; Code:

(require 'ert)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'modeline-config)

;; ---------------------------- Macro Indicator --------------------------------

(ert-deftest test-modeline-config-macro-indicator-shows-when-defining ()
  "Normal: MACRO text with the error face while a macro is recording."
  (let ((defining-kbd-macro t))
    (let ((s (cj/--modeline-macro-indicator)))
      (should (stringp s))
      (should (string-match-p "MACRO" s))
      (should (eq (get-text-property (string-match "MACRO" s) 'face s)
                  'error)))))

(ert-deftest test-modeline-config-macro-indicator-nil-when-idle ()
  "Boundary: nil when no macro is being defined."
  (let ((defining-kbd-macro nil))
    (should-not (cj/--modeline-macro-indicator))))

;; ---------------------------- Remote Host Tag --------------------------------

(ert-deftest test-modeline-config-remote-host-shows-host ()
  "Normal: remote default-directory yields an @host tag."
  (with-temp-buffer
    (setq default-directory "/ssh:velox:/home/cjennings/")
    (let ((s (cj/--modeline-remote-host)))
      (should (stringp s))
      (should (string-match-p "@velox" s)))))

(ert-deftest test-modeline-config-remote-host-nil-when-local ()
  "Boundary: nil for a local directory."
  (with-temp-buffer
    (setq default-directory "/tmp/")
    (should-not (cj/--modeline-remote-host))))

;; --------------------------- Narrowing Indicator -----------------------------

(ert-deftest test-modeline-config-narrow-indicator-shows-when-narrowed ()
  "Normal: narrowed buffer yields the Narrow tag."
  (with-temp-buffer
    (insert "line one\nline two\nline three\n")
    (narrow-to-region 1 9)
    (let ((s (cj/--modeline-narrow-indicator)))
      (should (stringp s))
      (should (string-match-p "Narrow" s)))))

(ert-deftest test-modeline-config-narrow-indicator-nil-when-widened ()
  "Boundary: nil when the buffer is not narrowed."
  (with-temp-buffer
    (insert "text")
    (should-not (cj/--modeline-narrow-indicator))))

;; --------------------------- Position / Region Info ---------------------------

(ert-deftest test-modeline-config-position-info-line-column-percent ()
  "Normal: no region yields L: C: plus a percentage-through-buffer."
  (with-temp-buffer
    (insert (make-string 200 ?x))
    (goto-char (point-min))
    (deactivate-mark)
    (let ((s (cj/--modeline-position-info)))
      (should (stringp s))
      (should (string-match-p "L:" s))
      (should (string-match-p "C:" s))
      ;; point-based percent, %%-escaped for the mode-line construct pass
      (should (string-match-p "%" s)))))

(ert-deftest test-modeline-config-position-info-region-lines-chars ()
  "Normal: an active region yields selection info instead of position."
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (goto-char (point-min))
    (push-mark (point) t t)
    (goto-char 9)                       ; through "one\ntwo\n"
    (let ((transient-mark-mode t))
      (let ((s (cj/--modeline-position-info)))
        (should (stringp s))
        (should (string-match-p "2 lines" s))
        (should (string-match-p "8 chars" s))))))

(ert-deftest test-modeline-config-position-info-single-char-region ()
  "Boundary: a one-char region reports 1 line, 1 char."
  (with-temp-buffer
    (insert "abc")
    (goto-char 1)
    (push-mark (point) t t)
    (goto-char 2)
    (let ((transient-mark-mode t))
      (let ((s (cj/--modeline-position-info)))
        (should (string-match-p "1 line" s))
        (should (string-match-p "1 char" s))))))

;; ------------------------------- Padding --------------------------------------

(ert-deftest test-modeline-config-padding-absolute-height-face ()
  "Normal: padding space carries a face with an absolute integer :height.
The height is anchored to the frame default (not the current buffer's
`default'), so a buffer that remaps `default' larger — nov's reading view,
`text-scale-mode' — no longer inflates the modeline."
  (let ((cj/modeline-height-factor 1.2))
    (let* ((s (cj/--modeline-padding))
           (face (get-text-property 0 'face s))
           (h (plist-get face :height)))
      (should (stringp s))
      (should (integerp h))
      (should (= h (round (* 1.2 (face-attribute 'default :height nil t))))))))

(ert-deftest test-modeline-config-padding-plain-at-factor-one ()
  "Boundary: factor 1.0 (or nil) yields a plain space, no height styling."
  (dolist (factor (list 1.0 nil))
    (let ((cj/modeline-height-factor factor))
      (let ((s (cj/--modeline-padding)))
        (should (stringp s))
        (should-not (get-text-property 0 'display s))
        (should-not (get-text-property 0 'face s))))))

(provide 'test-modeline-config-segments)
;;; test-modeline-config-segments.el ends here

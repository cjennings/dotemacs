;;; test-calibredb-epub-config.el --- Tests for ebook config helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Focuses on project-owned helpers in calibredb-epub-config rather than
;; CalibreDB/Nov internals.  The Nov layout helpers get the most attention:
;; the text-width math, the idempotency of `cj/nov-update-layout' (it must not
;; shrink the column each time it runs), and the cold-open re-render.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'calibredb-epub-config)
(require 'nov nil t)  ; for the nov-mode-map keybinding test; harmless if absent

(declare-function cj/nov--text-width "calibredb-epub-config" (total-cols))

;;; ----------------------------- cj/nov--text-width ---------------------------

(ert-deftest test-calibredb-epub-nov-text-width-applies-margin ()
  "Normal: 25% margins leave 50% of the usable columns for text."
  (let ((cj/nov-margin-percent 25)
        (cj/nov-min-text-width 40))
    (should (= 60 (cj/nov--text-width 120)))))

(ert-deftest test-calibredb-epub-nov-text-width-clamps-large-margin ()
  "Boundary: a margin percent above 25 is clamped to 25, so text never drops
below 50% of the usable columns."
  (let ((cj/nov-margin-percent 80)
        (cj/nov-min-text-width 40))
    (should (= 60 (cj/nov--text-width 120)))))

(ert-deftest test-calibredb-epub-nov-text-width-clamps-negative-margin ()
  "Boundary: a negative margin percent is clamped up to 0 (text takes everything)."
  (let ((cj/nov-margin-percent -10)
        (cj/nov-min-text-width 40))
    (should (= 120 (cj/nov--text-width 120)))))

(ert-deftest test-calibredb-epub-nov-text-width-honours-minimum ()
  "Boundary: a narrow window still yields at least `cj/nov-min-text-width'."
  (let ((cj/nov-margin-percent 25)
        (cj/nov-min-text-width 40))
    (should (= 40 (cj/nov--text-width 50)))))

(ert-deftest test-calibredb-epub-nov-default-margin-gives-roughly-three-quarter-text ()
  "Normal: the default `cj/nov-margin-percent' leaves ~3/4 of the window for text."
  (should (= 76 (cj/nov--text-width 100))))

;;; ----------------------- cj/nov--text-width-for-window ----------------------

(ert-deftest test-calibredb-epub-nov-text-width-for-window-fresh ()
  "Normal: with no margins set yet, the natural width is the body width."
  (let ((cj/nov-margin-percent 25)
        (cj/nov-min-text-width 40))
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'win))
              ((symbol-function 'window-body-width) (lambda (_) 120))
              ((symbol-function 'window-margins) (lambda (_) '(nil . nil))))
      (should (= 60 (cj/nov--text-width-for-window))))))

(ert-deftest test-calibredb-epub-nov-text-width-for-window-idempotent ()
  "Boundary: re-running with margins already set returns the same width.
The body width is now narrower because margins were applied, but the natural
width (body + margins) is unchanged, so the column does not shrink.  Without
this, every layout pass would shave the column by another margin fraction."
  (let ((cj/nov-margin-percent 25)
        (cj/nov-min-text-width 40))
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'win))
              ((symbol-function 'window-body-width) (lambda (_) 60))
              ((symbol-function 'window-margins) (lambda (_) '(30 . 30))))
      (should (= 60 (cj/nov--text-width-for-window))))))

(ert-deftest test-calibredb-epub-nov-text-width-for-window-no-window ()
  "Boundary: a buffer with no visible window still gets a usable width."
  (let ((cj/nov-margin-percent 25)
        (cj/nov-min-text-width 40))
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
      (should (= 40 (cj/nov--text-width-for-window))))))

;;; ---------------------------- cj/nov-update-layout --------------------------

(ert-deftest test-calibredb-epub-nov-update-layout-is-a-command ()
  "Normal: `cj/nov-update-layout' can be invoked with `M-x'."
  (should (commandp #'cj/nov-update-layout)))

;;; --------------------- cj/nov-widen-text / cj/nov-narrow-text ---------------

(ert-deftest test-calibredb-epub-nov-adjust-margin-steps-and-clamps ()
  "Normal/Boundary: adjusting the margin moves by DELTA, clamped to 0..25."
  (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
    (let ((cj/nov-margin-percent 12))
      (cj/--nov-adjust-margin -2)
      (should (= 10 cj/nov-margin-percent))
      (cj/--nov-adjust-margin 100)
      (should (= 25 cj/nov-margin-percent))    ; 50%-text floor
      (cj/--nov-adjust-margin -100)
      (should (= 0 cj/nov-margin-percent)))))  ; 100%-text ceiling

(ert-deftest test-calibredb-epub-nov-widen-text-decreases-margin ()
  "Normal: `cj/nov-widen-text' gives the column more of the window."
  (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
    (let ((cj/nov-margin-percent 12)
          (cj/nov-margin-step 2))
      (cj/nov-widen-text)
      (should (= 10 cj/nov-margin-percent)))))

(ert-deftest test-calibredb-epub-nov-narrow-text-increases-margin ()
  "Normal: `cj/nov-narrow-text' gives the column less of the window."
  (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
    (let ((cj/nov-margin-percent 12)
          (cj/nov-margin-step 2))
      (cj/nov-narrow-text)
      (should (= 14 cj/nov-margin-percent)))))

(ert-deftest test-calibredb-epub-nov-width-commands-are-commands ()
  "Normal: the width-adjust commands are `M-x'-able."
  (should (commandp #'cj/nov-widen-text))
  (should (commandp #'cj/nov-narrow-text)))

(ert-deftest test-calibredb-epub-nov-width-commands-bound-in-nov-mode-map ()
  "Normal: +/= widen and -/_ narrow the text column in `nov-mode-map'."
  (skip-unless (and (require 'nov nil t) (boundp 'nov-mode-map)))
  (should (eq (keymap-lookup nov-mode-map "+") #'cj/nov-widen-text))
  (should (eq (keymap-lookup nov-mode-map "=") #'cj/nov-widen-text))
  (should (eq (keymap-lookup nov-mode-map "-") #'cj/nov-narrow-text))
  (should (eq (keymap-lookup nov-mode-map "_") #'cj/nov-narrow-text)))

;;; -------------------------- cj/nov-apply-preferences ------------------------

(ert-deftest test-calibredb-epub-nov-apply-preferences-rerenders-document ()
  "Normal: applying preferences re-renders the document so the first page
lands inside the margins it just configured."
  (let (rendered)
    (cl-letf (((symbol-function 'nov-render-document) (lambda () (setq rendered t))))
      (with-temp-buffer
        (cj/nov-apply-preferences)
        (should rendered)))))

;;; ----------------------------- cj/nov-open-external -------------------------

(ert-deftest test-calibredb-epub-open-external-uses-zathura ()
  "Normal: named Nov external-open command delegates to zathura."
  (let (command)
    (cl-letf (((symbol-function 'cj/open-file-with-command)
               (lambda (cmd) (setq command cmd))))
      (cj/nov-open-external)
      (should (equal command "zathura")))))

(provide 'test-calibredb-epub-config)
;;; test-calibredb-epub-config.el ends here

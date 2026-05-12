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

(defmacro test-calibredb-epub--in-nov-buffer (&rest body)
  "Run BODY in a temp buffer faking `nov-mode' and a 200-column window.
`get-buffer-window' / `window-body-width' / `window-margins' /
`set-window-margins' / `set-window-fringes' are stubbed; BODY must stub
`nov-render-document' before anything that reaches it."
  (declare (indent 0))
  `(with-temp-buffer
     (setq-local major-mode 'nov-mode)
     (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'win))
               ((symbol-function 'window-body-width) (lambda (_) 200))
               ((symbol-function 'window-margins) (lambda (_) '(nil . nil)))
               ((symbol-function 'set-window-margins) (lambda (&rest _) nil))
               ((symbol-function 'set-window-fringes) (lambda (&rest _) nil)))
       ,@body)))

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

(ert-deftest test-calibredb-epub-nov-default-margin-gives-80-percent-text ()
  "Normal: the default `cj/nov-margin-percent' leaves 80% of the window for text."
  (should (= 80 (cj/nov--text-width 100))))

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

(ert-deftest test-calibredb-epub-nov-update-layout-reflows-when-width-changes ()
  "Normal: a changed text width updates `nov-text-width' and re-renders.
nov fills the text to `nov-text-width' itself, so a width change requires a
re-render of the document."
  (let ((cj/nov-margin-percent 10)
        rendered)
    (test-calibredb-epub--in-nov-buffer
      (setq-local nov-text-width 50)
      (cl-letf (((symbol-function 'nov-render-document) (lambda () (setq rendered t))))
        (cj/nov-update-layout))
      (should (= 160 nov-text-width))   ; 80% of the 200-column window
      (should rendered))))

(ert-deftest test-calibredb-epub-nov-update-layout-skips-reflow-when-width-unchanged ()
  "Boundary: when the width is already current, do not re-render the document."
  (let ((cj/nov-margin-percent 10)
        rendered)
    (test-calibredb-epub--in-nov-buffer
      (setq-local nov-text-width 160)   ; already 80% of 200
      (cl-letf (((symbol-function 'nov-render-document) (lambda () (setq rendered t))))
        (cj/nov-update-layout))
      (should (= 160 nov-text-width))
      (should-not rendered))))

(ert-deftest test-calibredb-epub-nov-update-layout-centers-with-equal-margins ()
  "Normal: the text block is centered with equal left/right window margins."
  (let ((cj/nov-margin-percent 10)
        margins)
    (test-calibredb-epub--in-nov-buffer
      (cl-letf (((symbol-function 'nov-render-document) #'ignore)
                ((symbol-function 'set-window-margins)
                 (lambda (_win l r) (setq margins (list l r)))))
        (cj/nov-update-layout))
      ;; (200 - 160) / 2 = 20 columns each side
      (should (equal margins '(20 20))))))

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
lands at the width it just configured."
  (let (rendered)
    (cl-letf (((symbol-function 'nov-render-document) (lambda () (setq rendered t))))
      (with-temp-buffer
        (cj/nov-apply-preferences)
        (should rendered)))))

(ert-deftest test-calibredb-epub-nov-apply-preferences-sets-integer-text-width ()
  "Normal: applying preferences sets `nov-text-width' to a column count, not t,
so nov's `shr' fills the text itself rather than relying on visual-fill-column."
  (cl-letf (((symbol-function 'nov-render-document) #'ignore))
    (with-temp-buffer
      (cj/nov-apply-preferences)
      (should (integerp nov-text-width)))))

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

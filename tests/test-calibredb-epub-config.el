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
               ((symbol-function 'window-body-width) (lambda (&rest _) 200))
               ((symbol-function 'window-margins) (lambda (&rest _) '(nil . nil)))
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
              ((symbol-function 'window-body-width) (lambda (&rest _) 120))
              ((symbol-function 'window-margins) (lambda (&rest _) '(nil . nil))))
      (should (= 60 (cj/nov--text-width-for-window))))))

(ert-deftest test-calibredb-epub-nov-text-width-for-window-idempotent ()
  "Boundary: re-running with margins already set returns the same width.
The body width is now narrower because margins were applied, but the natural
width (body + margins) is unchanged, so the column does not shrink.  Without
this, every layout pass would shave the column by another margin fraction."
  (let ((cj/nov-margin-percent 25)
        (cj/nov-min-text-width 40))
    (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'win))
              ((symbol-function 'window-body-width) (lambda (&rest _) 60))
              ((symbol-function 'window-margins) (lambda (&rest _) '(30 . 30))))
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

;;; ----------------------- cj/nov--natural-window-width -----------------------

(ert-deftest test-calibredb-epub-nov-natural-window-width-no-margins ()
  "Normal: with no margins set, the natural width equals `window-body-width'."
  (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'win))
            ((symbol-function 'window-body-width) (lambda (&rest _) 100))
            ((symbol-function 'window-margins) (lambda (&rest _) '(nil . nil))))
    (should (= 100 (cj/nov--natural-window-width)))))

(ert-deftest test-calibredb-epub-nov-natural-window-width-adds-margins ()
  "Boundary: with margins set, the natural width adds them back to the body."
  (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) 'win))
            ((symbol-function 'window-body-width) (lambda (&rest _) 60))
            ((symbol-function 'window-margins) (lambda (&rest _) '(20 . 20))))
    (should (= 100 (cj/nov--natural-window-width)))))

(ert-deftest test-calibredb-epub-nov-natural-window-width-no-window-fallback ()
  "Boundary: when no window shows the buffer, the helper returns 80."
  (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
    (should (= 80 (cj/nov--natural-window-width)))))

;;; ----------------------- cj/--nov-image-padding-cols ------------------------

(ert-deftest test-calibredb-epub-image-padding-centers-narrow-image ()
  "Normal: a half-column-wide image is centered with quarter-width padding."
  (should (= 20 (cj/--nov-image-padding-cols 80 320 8))))

(ert-deftest test-calibredb-epub-image-padding-zero-when-image-fills-column ()
  "Boundary: an image exactly as wide as the column needs no padding."
  (should (= 0 (cj/--nov-image-padding-cols 40 320 8))))

(ert-deftest test-calibredb-epub-image-padding-zero-when-image-overflows ()
  "Boundary: an image wider than the column clamps the padding to 0."
  (should (= 0 (cj/--nov-image-padding-cols 80 1000 8))))

(ert-deftest test-calibredb-epub-image-padding-clamps-zero-font-width ()
  "Boundary: a zero `font-width-px' is clamped up to 1 so the divide stays safe."
  (should (= 0 (cj/--nov-image-padding-cols 80 320 0))))

;;; --------------------------- cj/calibredb-clear-filters ---------------------

(ert-deftest test-calibredb-epub-clear-filters-resets-state ()
  "Normal: clearing the filters resets all five filter vars and the page index."
  (skip-unless (require 'calibredb-search nil t))
  (let ((calibredb-tag-filter-p t)
        (calibredb-favorite-filter-p t)
        (calibredb-author-filter-p t)
        (calibredb-date-filter-p t)
        (calibredb-format-filter-p t)
        (calibredb-search-current-page 7))
    (cl-letf (((symbol-function 'calibredb-search-keyword-filter) #'ignore))
      (cj/calibredb-clear-filters))
    (should-not calibredb-tag-filter-p)
    (should-not calibredb-favorite-filter-p)
    (should-not calibredb-author-filter-p)
    (should-not calibredb-date-filter-p)
    (should-not calibredb-format-filter-p)
    (should (= 1 calibredb-search-current-page))))

(ert-deftest test-calibredb-epub-clear-filters-refreshes-listing ()
  "Normal: clearing the filters re-runs the keyword filter with an empty query
so the search buffer rebuilds against the now-unfiltered set."
  (skip-unless (require 'calibredb-search nil t))
  (let ((calibredb-tag-filter-p nil)
        (calibredb-favorite-filter-p nil)
        (calibredb-author-filter-p nil)
        (calibredb-date-filter-p nil)
        (calibredb-format-filter-p nil)
        (calibredb-search-current-page 1)
        passed)
    (cl-letf (((symbol-function 'calibredb-search-keyword-filter)
               (lambda (kw) (setq passed kw))))
      (cj/calibredb-clear-filters))
    (should (equal "" passed))))

;;; --------------------------- cj/force-nov-mode-for-epub ---------------------

(ert-deftest test-calibredb-epub-force-nov-mode-on-epub-calls-nov-mode ()
  "Normal: a .epub buffer with nov-mode bound dispatches to `nov-mode' and
does not fall through to the original mode dispatcher."
  (skip-unless (fboundp 'nov-mode))
  (let (orig-called nov-called)
    (cl-letf (((symbol-function 'nov-mode)
               (lambda () (setq nov-called t))))
      (with-temp-buffer
        (setq buffer-file-name "/tmp/sample.epub")
        (cj/force-nov-mode-for-epub
         (lambda (&rest _) (setq orig-called t)))))
    (should nov-called)
    (should-not orig-called)))

(ert-deftest test-calibredb-epub-force-nov-mode-passes-through-non-epub ()
  "Boundary: a non-epub buffer falls through to the original mode dispatcher."
  (let (orig-called)
    (with-temp-buffer
      (setq buffer-file-name "/tmp/sample.txt")
      (cj/force-nov-mode-for-epub
       (lambda (&rest _) (setq orig-called t))))
    (should orig-called)))

(ert-deftest test-calibredb-epub-force-nov-mode-passes-through-no-filename ()
  "Boundary: a buffer with no associated filename falls through to the
original mode dispatcher."
  (let (orig-called)
    (with-temp-buffer
      (cj/force-nov-mode-for-epub
       (lambda (&rest _) (setq orig-called t))))
    (should orig-called)))

(ert-deftest test-calibredb-epub-force-nov-mode-passes-through-when-nov-missing ()
  "Error: a .epub buffer falls through to the original dispatcher when nov-mode
is not defined (the require failed and there is nothing to dispatch to)."
  (let ((saved (and (fboundp 'nov-mode) (symbol-function 'nov-mode)))
        orig-called)
    (when saved (fmakunbound 'nov-mode))
    (unwind-protect
        (cl-letf (((symbol-function 'require)
                   ;; Pretend the (require 'nov nil t) call fails too.
                   (lambda (&rest _) nil)))
          (with-temp-buffer
            (setq buffer-file-name "/tmp/sample.epub")
            (cj/force-nov-mode-for-epub
             (lambda (&rest _) (setq orig-called t)))))
      (when saved (fset 'nov-mode saved)))
    (should orig-called)))

;;; ---------------------------- cj/nov--metadata-get --------------------------

(ert-deftest test-calibredb-epub-metadata-get-symbol-key ()
  "Normal: a symbol key returns the matching alist value."
  (let ((nov-metadata '((title . "Moby Dick"))))
    (should (equal "Moby Dick" (cj/nov--metadata-get 'title)))))

(ert-deftest test-calibredb-epub-metadata-get-string-fallback ()
  "Normal: when the alist is keyed by string and the caller passes a symbol,
the helper retries with the string form."
  (let ((nov-metadata '(("title" . "Moby Dick"))))
    (should (equal "Moby Dick" (cj/nov--metadata-get 'title)))))

(ert-deftest test-calibredb-epub-metadata-get-unwraps-single-element-list ()
  "Boundary: a single-element list value is unwrapped to its element so the
result is a plain string that the jump-to-calibredb query can interpolate."
  (let ((nov-metadata '((title . ("Moby Dick")))))
    (should (equal "Moby Dick" (cj/nov--metadata-get 'title)))))

(ert-deftest test-calibredb-epub-metadata-get-keeps-multi-element-list ()
  "Boundary: a multi-element list value passes through as-is."
  (let ((nov-metadata '((creator . ("Melville" "Whittaker")))))
    (should (equal '("Melville" "Whittaker") (cj/nov--metadata-get 'creator)))))

(ert-deftest test-calibredb-epub-metadata-get-missing-key-returns-nil ()
  "Error: a key with no value in the alist returns nil."
  (let ((nov-metadata '((title . "Moby Dick"))))
    (should-not (cj/nov--metadata-get 'creator))))

;;; ----------------------------- cj/nov--file-path ----------------------------

(ert-deftest test-calibredb-epub-file-path-returns-buffer-file-name-in-nov-mode ()
  "Normal: in `nov-mode' the helper returns `buffer-file-name'."
  (with-temp-buffer
    (setq-local major-mode 'nov-mode)
    (setq buffer-file-name "/tmp/sample.epub")
    (should (equal "/tmp/sample.epub" (cj/nov--file-path)))))

(ert-deftest test-calibredb-epub-file-path-falls-back-to-nov-file-name ()
  "Boundary: in `nov-mode' with no `buffer-file-name', the helper falls back
to nov.el's own `nov-file-name' (set by `nov-mode' from the visited file)."
  (with-temp-buffer
    (setq-local major-mode 'nov-mode)
    (setq buffer-file-name nil)
    (setq-local nov-file-name "/tmp/sample.epub")
    (should (equal "/tmp/sample.epub" (cj/nov--file-path)))))

(ert-deftest test-calibredb-epub-file-path-returns-nil-outside-nov-mode ()
  "Error: outside `nov-mode' the helper returns nil regardless of file name."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/sample.epub")
    (should-not (cj/nov--file-path))))

;;; --------------------------- cj/nov-jump-to-calibredb -----------------------

(defmacro test-calibredb-epub--with-calibredb-stubs (file metadata-alist &rest body)
  "Run BODY with `cj/nov--file-path' and `cj/nov--metadata-get' stubbed.
FILE is returned by the file-path stub; METADATA-ALIST is the alist consulted
by the metadata stub.  `calibredb', `calibredb-find-create-search-buffer', and
`message' are stubbed so the body never touches the real library."
  (declare (indent 2))
  `(let ((calibredb-search-current-page 99)
         passed)
     (cl-letf (((symbol-function 'cj/nov--file-path) (lambda () ,file))
               ((symbol-function 'cj/nov--metadata-get)
                (lambda (key) (alist-get key ,metadata-alist nil nil #'equal)))
               ((symbol-function 'calibredb) (lambda () nil))
               ((symbol-function 'calibredb-find-create-search-buffer)
                (lambda () (current-buffer)))
               ((symbol-function 'calibredb-search-keyword-filter)
                (lambda (kw) (setq passed kw)))
               ((symbol-function 'message) (lambda (&rest _) nil)))
       ,@body
       passed)))

(ert-deftest test-calibredb-epub-jump-to-calibredb-uses-id-when-present ()
  "Normal: a parent directory of the form \"Title (NNN)\" yields an id query."
  (let ((result (test-calibredb-epub--with-calibredb-stubs
                    "/books/Moby Dick (42)/moby.epub"
                    '((title . "Moby Dick") (creator . "Melville"))
                  (cj/nov-jump-to-calibredb))))
    (should (equal "id:42" result))))

(ert-deftest test-calibredb-epub-jump-to-calibredb-falls-back-to-title-and-author ()
  "Normal: without an id, the query combines title and author with `and'."
  (let ((result (test-calibredb-epub--with-calibredb-stubs
                    "/books/Some Book/some.epub"
                    '((title . "Moby Dick") (creator . "Melville"))
                  (cj/nov-jump-to-calibredb))))
    (should (equal "title:\"Moby Dick\" and authors:\"Melville\"" result))))

(ert-deftest test-calibredb-epub-jump-to-calibredb-empty-query-without-metadata ()
  "Boundary: with no id and no metadata, the helper clears the keyword filter."
  (let ((result (test-calibredb-epub--with-calibredb-stubs
                    "/books/Untitled/untitled.epub"
                    nil
                  (cj/nov-jump-to-calibredb))))
    (should (equal "" result))))

(provide 'test-calibredb-epub-config)
;;; test-calibredb-epub-config.el ends here

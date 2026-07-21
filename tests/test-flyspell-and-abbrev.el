;;; test-flyspell-and-abbrev.el --- Tests for flyspell-and-abbrev.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Coverage for the testable seams in flyspell-and-abbrev.el:
;;
;; - `cj/--require-spell-checker' (executable-find gate, mocked PATH)
;; - `cj/find-previous-flyspell-overlay' (overlay sort + flyspell/face filter,
;;   exercised against synthetic overlays so no aspell is needed)
;; - `cj/flyspell-on-for-buffer-type' (prog-mode vs text-mode dispatch, with
;;   the flyspell entry points mocked)
;;
;; `cj/flyspell-then-abbrev' is left to manual testing — it orchestrates
;; flyspell's interactive correction UI, and pinning it would mean mocking
;; flyspell internals rather than testing our logic.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'user-constants) ;; org-dir, read by the ispell :config below
(require 'flyspell)
(require 'flyspell-and-abbrev)

(defun test-flyspell--make-incorrect-overlay (beg end)
  "Make a flyspell `incorrect' overlay spanning BEG..END in the current buffer."
  (let ((o (make-overlay beg end)))
    (overlay-put o 'flyspell-overlay t)
    (overlay-put o 'face 'flyspell-incorrect)
    o))

;; ------------------------- org src-block skip entry ---------------------------

(ert-deftest test-flyspell-ispell-skip-entry-matches-src-block-lines ()
  "Normal: the ispell skip entry matches real org src-block delimiters.
The old entry used \"#+\" (one-or-more #), which matches no real
begin_src line, so ispell spell-checked inside every org code block."
  (require 'ispell)
  (let ((entry (seq-find (lambda (e)
                           (and (consp e) (stringp (car e))
                                (string-match-p "BEGIN_SRC" (car e))))
                         ispell-skip-region-alist)))
    (should entry)
    (let ((case-fold-search t))
      (should (string-match-p (car entry) "#+BEGIN_SRC emacs-lisp"))
      (should (string-match-p (car entry) "#+begin_src python"))
      (should (string-match-p (cdr entry) "#+end_src")))))

;; ------------------------ cj/--require-spell-checker -------------------------

(ert-deftest test-flyspell-require-spell-checker-present ()
  "Normal: a checker on PATH means no error."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd &rest _) (equal cmd (car cj/--spell-checker-executables)))))
    (should-not (cj/--require-spell-checker))))

(ert-deftest test-flyspell-require-spell-checker-missing ()
  "Error: no checker on PATH signals user-error."
  (cl-letf (((symbol-function 'executable-find) (lambda (_ &rest _) nil)))
    (should-error (cj/--require-spell-checker) :type 'user-error)))

;; --------------------- cj/find-previous-flyspell-overlay ---------------------

(ert-deftest test-flyspell-find-previous-overlay-returns-start ()
  "Normal: a flyspell overlay before POSITION returns its start position."
  (with-temp-buffer
    (insert "hello world")
    (test-flyspell--make-incorrect-overlay 1 6)
    (should (= (cj/find-previous-flyspell-overlay (point-max)) 1))))

(ert-deftest test-flyspell-find-previous-overlay-picks-closest ()
  "Normal: with several, the closest overlay before POSITION wins."
  (with-temp-buffer
    (insert "alpha beta gamma delta")
    (test-flyspell--make-incorrect-overlay 1 6)    ; alpha
    (test-flyspell--make-incorrect-overlay 12 17)  ; gamma
    (should (= (cj/find-previous-flyspell-overlay (point-max)) 12))))

(ert-deftest test-flyspell-find-previous-overlay-none ()
  "Boundary: no overlays at all yields nil."
  (with-temp-buffer
    (insert "nothing flagged here")
    (should-not (cj/find-previous-flyspell-overlay (point-max)))))

(ert-deftest test-flyspell-find-previous-overlay-skips-non-flyspell ()
  "Boundary: an overlay without the flyspell marker is ignored."
  (with-temp-buffer
    (insert "plain overlay text")
    (let ((o (make-overlay 1 6)))
      (overlay-put o 'face 'flyspell-incorrect)) ; face but not a flyspell overlay
    (should-not (cj/find-previous-flyspell-overlay (point-max)))))

;; --------------------- cj/flyspell-on-for-buffer-type ------------------------

(ert-deftest test-flyspell-on-for-buffer-type-prog-mode ()
  "Normal: in a prog-mode buffer, flyspell-prog-mode is used."
  (let (prog-called)
    (cl-letf (((symbol-function 'flyspell-prog-mode) (lambda () (setq prog-called t)))
              ((symbol-function 'flyspell-mode)
               (lambda (&rest _) (error "flyspell-mode should not run in prog-mode")))
              ((symbol-function 'flyspell-buffer) #'ignore))
      (with-temp-buffer
        (emacs-lisp-mode)
        (cj/flyspell-on-for-buffer-type)))
    (should prog-called)))

(ert-deftest test-flyspell-on-for-buffer-type-text-mode ()
  "Normal: in a text-mode buffer, flyspell-mode is used."
  (let (mode-called)
    (cl-letf (((symbol-function 'flyspell-mode) (lambda (&rest _) (setq mode-called t)))
              ((symbol-function 'flyspell-prog-mode)
               (lambda () (error "flyspell-prog-mode should not run in text-mode")))
              ((symbol-function 'flyspell-buffer) #'ignore))
      (with-temp-buffer
        (text-mode)
        (cj/flyspell-on-for-buffer-type)))
    (should mode-called)))

;; --------------------------- cj/flyspell-then-abbrev -------------------------

(ert-deftest test-flyspell-then-abbrev-enables-mode-not-bare-rescan ()
  "Regression: cj/flyspell-then-abbrev routes the initial scan through
cj/flyspell-on-for-buffer-type, which enables flyspell-mode so it sticks.
The bare flyspell-buffer it replaced never turned the mode on, so the guard
never tripped and every C-' press re-scanned the whole buffer (O(buffer) per
keypress in large files)."
  (let (on-called scan-called)
    (cl-letf (((symbol-function 'cj/--require-spell-checker) #'ignore)
              ((symbol-function 'cj/flyspell-on-for-buffer-type)
               (lambda () (setq on-called t)))
              ((symbol-function 'flyspell-buffer)
               (lambda (&rest _) (setq scan-called t)))
              ((symbol-function 'cj/flyspell-goto-previous-misspelling)
               (lambda (&rest _) nil)))
      (with-temp-buffer
        (text-mode)
        (cj/flyspell-then-abbrev nil)))
    (should on-called)
    (should-not scan-called)))

(provide 'test-flyspell-and-abbrev)
;;; test-flyspell-and-abbrev.el ends here

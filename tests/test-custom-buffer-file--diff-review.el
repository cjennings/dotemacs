;;; test-custom-buffer-file--diff-review.el --- navigable diff review pieces -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure-logic tests for the navigable diff review added to the disk-changed save
;; prompts: the diff command argument builders (context + width), first-hunk
;; positioning, the generated review keymap (menu keys act inside the diff, q
;; goes back, TAB/S-TAB navigate), the merge menu entry, and the reworked
;; read-choice loop (d enters the review; a review choice terminates the
;; prompt).  The recursive-edit itself and the live window interaction are
;; exercised manually, not here.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'custom-buffer-file)

(declare-function cj/--difft-args "custom-buffer-file" (file1 file2 context width))
(declare-function cj/--unified-diff-args "custom-buffer-file" (file1 file2 context))
(declare-function cj/--diff-review-keymap "custom-buffer-file" (choices))
(declare-function cj/--diff-review-back "custom-buffer-file" ())
(declare-function cj/--diff-section-next "custom-buffer-file" ())
(declare-function cj/--diff-section-prev "custom-buffer-file" ())
(declare-function cj/--diff-review "custom-buffer-file" (diff-buffer choices))
(declare-function cj/--diff-with-regular-diff "custom-buffer-file" (file1 file2 buffer))
(declare-function cj/--diff-with-difftastic "custom-buffer-file" (file1 file2 buffer))
(declare-function cj/--buffer-differs-choices "custom-buffer-file" ())
(declare-function cj/--buffer-differs-action "custom-buffer-file" (key))
(declare-function cj/--buffer-differs-dispatch "custom-buffer-file" (buffer action))
(declare-function cj/--save-some-buffers-choices "custom-buffer-file" ())
(declare-function cj/--read-choice-with-diff "custom-buffer-file" (prompt choices show-diff-fn))

(defvar cj/--diff-review-choice)
(defvar cj/diff-context-lines)

;;; ------------------------------ arg builders --------------------------------

(ert-deftest test-cbf-difft-args-context-and-width ()
  "Normal: difft args carry the context and width knobs plus both files."
  (let ((args (cj/--difft-args "/a.el" "/b.el" 1 143)))
    (should (member "--context" args))
    (should (member "1" args))
    (should (member "--width" args))
    (should (member "143" args))
    (should (member "/a.el" args))
    (should (member "/b.el" args))))

(ert-deftest test-cbf-difft-args-color-and-display ()
  "Normal: difft args keep forced color and the side-by-side display."
  (let ((args (cj/--difft-args "/a" "/b" 3 80)))
    (should (member "--color" args))
    (should (member "always" args))
    (should (member "side-by-side-show-both" args))))

(ert-deftest test-cbf-difft-args-zero-context ()
  "Boundary: context 0 is passed through, not dropped."
  (let ((args (cj/--difft-args "/a" "/b" 0 80)))
    (should (member "--context" args))
    (should (member "0" args))))

(ert-deftest test-cbf-unified-diff-args-context ()
  "Normal: unified diff args use -U<context> and both files, in order."
  (should (equal (cj/--unified-diff-args "/a" "/b" 1) '("-U1" "/a" "/b"))))

(ert-deftest test-cbf-unified-diff-args-zero-context ()
  "Boundary: context 0 produces -U0."
  (should (equal (car (cj/--unified-diff-args "/a" "/b" 0)) "-U0")))

;;; ------------------------- first-hunk positioning ---------------------------

(defun test-cbf-review--two-files (line-changed body-fn)
  "Make two 60-line temp files differing at LINE-CHANGED; call BODY-FN with both."
  (let ((f1 (make-temp-file "cbf-rev-a-" nil ".txt"))
        (f2 (make-temp-file "cbf-rev-b-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file f1
            (dotimes (i 60) (insert (format "line %d stays the same\n" (1+ i)))))
          (with-temp-file f2
            (dotimes (i 60)
              (insert (if (= (1+ i) line-changed)
                          (format "line %d CHANGED in buffer\n" (1+ i))
                        (format "line %d stays the same\n" (1+ i))))))
          (funcall body-fn f1 f2))
      (when (file-exists-p f1) (delete-file f1))
      (when (file-exists-p f2) (delete-file f2)))))

(ert-deftest test-cbf-regular-diff-lands-on-first-hunk ()
  "Normal: after rendering a unified diff, point sits on the first @@ hunk line."
  (test-cbf-review--two-files
   30
   (lambda (f1 f2)
     (with-temp-buffer
       (cj/--diff-with-regular-diff f1 f2 (current-buffer))
       (should (looking-at "@@"))))))

(ert-deftest test-cbf-difftastic-lands-past-headers ()
  "Normal: after rendering difftastic output, point sits on hunk content, not the headers."
  (skip-unless (executable-find "difft"))
  (test-cbf-review--two-files
   30
   (lambda (f1 f2)
     (with-temp-buffer
       (cj/--diff-with-difftastic f1 f2 (current-buffer))
       (should (> (line-number-at-pos) 2))          ; past our two-line header
       (should-not (looking-at-p ".* --- "))         ; past difft's file header
       (should-not (eolp))))))                       ; on a content line, not a blank

;;; ------------------------------ review keymap -------------------------------

(ert-deftest test-cbf-review-keymap-binds-menu-keys ()
  "Normal: every buffer-differs menu key except d becomes a review binding."
  (let ((map (cj/--diff-review-keymap (cj/--buffer-differs-choices))))
    (dolist (key '(?s ?w ?r ?m ?c))
      (should (commandp (lookup-key map (vector key)))))
    (should-not (lookup-key map (vector ?d)))))

(ert-deftest test-cbf-review-keymap-q-goes-back-when-free ()
  "Normal: q maps to back-to-menu when the menu does not claim q."
  (let ((map (cj/--diff-review-keymap (cj/--buffer-differs-choices))))
    (should (eq (lookup-key map "q") #'cj/--diff-review-back))))

(ert-deftest test-cbf-review-keymap-q-stays-menu-key-when-claimed ()
  "Boundary: in the save-some loop q is a menu action, not back-to-menu."
  (let ((map (cj/--diff-review-keymap (cj/--save-some-buffers-choices))))
    (should (commandp (lookup-key map "q")))
    (should-not (eq (lookup-key map "q") #'cj/--diff-review-back))))

(ert-deftest test-cbf-review-keymap-navigation-bound ()
  "Normal: TAB and S-TAB navigate hunks in every review."
  (let ((map (cj/--diff-review-keymap (cj/--buffer-differs-choices))))
    (should (eq (lookup-key map (kbd "TAB")) #'cj/--diff-section-next))
    (should (eq (lookup-key map (kbd "<backtab>")) #'cj/--diff-section-prev))))

(ert-deftest test-cbf-review-keymap-np-navigate-when-free ()
  "Normal: n/p navigate hunks when the menu does not claim them."
  (let ((map (cj/--diff-review-keymap (cj/--buffer-differs-choices))))
    (should (eq (lookup-key map "n") #'cj/--diff-section-next))
    (should (eq (lookup-key map "p") #'cj/--diff-section-prev))))

(ert-deftest test-cbf-review-keymap-n-stays-menu-key-when-claimed ()
  "Boundary: in the save-some loop n is the skip action, not navigation."
  (let ((map (cj/--diff-review-keymap (cj/--save-some-buffers-choices))))
    (should (commandp (lookup-key map "n")))
    (should-not (eq (lookup-key map "n") #'cj/--diff-section-next))))

(ert-deftest test-cbf-review-keymap-escape-always-back ()
  "Normal: ESC goes back to the menu even when the menu claims q (save-some loop)."
  (let ((map (cj/--diff-review-keymap (cj/--save-some-buffers-choices))))
    (should (eq (lookup-key map (kbd "<escape>")) #'cj/--diff-review-back))))

(ert-deftest test-cbf-review-choice-command-records-key ()
  "Normal: a review action command records its key and exits the review."
  (let ((map (cj/--diff-review-keymap (cj/--buffer-differs-choices)))
        (cj/--diff-review-choice nil)
        (exited nil))
    (cl-letf (((symbol-function 'exit-recursive-edit)
               (lambda (&rest _) (setq exited t))))
      (funcall (lookup-key map "s"))
      (should (eq cj/--diff-review-choice ?s))
      (should exited))))

(ert-deftest test-cbf-review-back-leaves-choice-nil ()
  "Normal: back-to-menu exits the review without recording a choice."
  (let ((cj/--diff-review-choice nil)
        (exited nil))
    (cl-letf (((symbol-function 'exit-recursive-edit)
               (lambda (&rest _) (setq exited t))))
      (cj/--diff-review-back)
      (should-not cj/--diff-review-choice)
      (should exited))))

;;; ---------------------------- merge menu entry ------------------------------

(ert-deftest test-cbf-buffer-differs-choices-include-merge ()
  "Normal: the disk-changed menu offers m for merge, described via ediff."
  (let ((entry (assq ?m (cj/--buffer-differs-choices))))
    (should entry)
    (should (string-match-p "ediff" (or (nth 2 entry) "")))))

(ert-deftest test-cbf-buffer-differs-action-merge ()
  "Normal: m maps to the merge action."
  (should (eq (cj/--buffer-differs-action ?m) 'merge)))

(ert-deftest test-cbf-buffer-differs-dispatch-merge-launches-ediff ()
  "Normal: dispatching merge launches ediff-current-file in the conflicted buffer."
  (let ((launched-in nil))
    (cl-letf (((symbol-function 'ediff-current-file)
               (lambda (&rest _) (setq launched-in (current-buffer)))))
      (with-temp-buffer
        (cj/--buffer-differs-dispatch (current-buffer) 'merge)
        (should (eq launched-in (current-buffer)))))))

(ert-deftest test-cbf-buffer-differs-dispatch-merge-keeps-buffer-modified ()
  "Boundary: merge neither saves nor reverts -- the buffer's state is untouched."
  (cl-letf (((symbol-function 'ediff-current-file) (lambda (&rest _) nil)))
    (with-temp-buffer
      (insert "content")
      (set-buffer-modified-p t)
      (cj/--buffer-differs-dispatch (current-buffer) 'merge)
      (should (buffer-modified-p)))))

;;; ------------------------- read-choice loop rework --------------------------

(defun test-cbf-review--run-loop (rmc-keys review-results &optional diff-buffer)
  "Drive cj/--read-choice-with-diff with scripted inputs.
RMC-KEYS are successive read-multiple-choice answers; REVIEW-RESULTS are
successive cj/--diff-review returns.  DIFF-BUFFER (or a fresh one) is what the
show-diff-fn yields.  Returns (RESULT RMC-CALLS REVIEW-CALLS)."
  (let ((rmc-calls 0) (review-calls 0) (keys rmc-keys) (reviews review-results))
    (cl-letf (((symbol-function 'read-multiple-choice)
               (lambda (&rest _)
                 (setq rmc-calls (1+ rmc-calls))
                 (list (pop keys))))
              ((symbol-function 'cj/--diff-review)
               (lambda (&rest _)
                 (setq review-calls (1+ review-calls))
                 (pop reviews))))
      (let ((result (cj/--read-choice-with-diff
                     "Prompt" (cj/--buffer-differs-choices)
                     (lambda () diff-buffer))))
        (list result rmc-calls review-calls)))))

(ert-deftest test-cbf-read-choice-review-choice-terminates ()
  "Normal: a key chosen inside the diff review terminates the prompt directly."
  (with-temp-buffer
    (pcase-let ((`(,result ,rmc-calls ,review-calls)
                 (test-cbf-review--run-loop '(?d) '(?s) (current-buffer))))
      (should (eq result ?s))
      (should (= rmc-calls 1))
      (should (= review-calls 1)))))

(ert-deftest test-cbf-read-choice-review-back-reprompts ()
  "Normal: leaving the review without a choice re-shows the menu prompt."
  (with-temp-buffer
    (pcase-let ((`(,result ,rmc-calls ,review-calls)
                 (test-cbf-review--run-loop '(?d ?c) '(nil) (current-buffer))))
      (should (eq result ?c))
      (should (= rmc-calls 2))
      (should (= review-calls 1)))))

(ert-deftest test-cbf-read-choice-no-diff-skips-review ()
  "Error: when the diff cannot render (no differences), d re-prompts without a review."
  (pcase-let ((`(,result ,rmc-calls ,review-calls)
               (test-cbf-review--run-loop '(?d ?c) '(?s) nil)))
    (should (eq result ?c))
    (should (= rmc-calls 2))
    (should (= review-calls 0))))

(ert-deftest test-cbf-read-choice-plain-key-never-reviews ()
  "Boundary: a terminating menu key never enters the review."
  (with-temp-buffer
    (pcase-let ((`(,result ,rmc-calls ,review-calls)
                 (test-cbf-review--run-loop '(?s) '() (current-buffer))))
      (should (eq result ?s))
      (should (= rmc-calls 1))
      (should (= review-calls 0)))))

(provide 'test-custom-buffer-file--diff-review)
;;; test-custom-buffer-file--diff-review.el ends here

;;; test-custom-buffer-file--save-some-buffers.el --- save-loop prompt pieces -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure-logic tests for the read-multiple-choice save loop that replaces
;; save-some-buffers' terse map-y-or-n-p prompt: the key->action mapping (what
;; to do with this buffer, and how the choice steers the rest of the loop) and
;; the labeled choice list.  The interactive loop, the file saves, and the
;; override wiring are exercised live, not here.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'custom-buffer-file)

(declare-function cj/--save-some-buffers-action "custom-buffer-file" (key))
(declare-function cj/--save-some-buffers-choices "custom-buffer-file" ())

;;; --------------------- cj/--save-some-buffers-action ------------------------
;; Each result is (THIS-ACTION . LOOP-EFFECT):
;;   THIS-ACTION ∈ save | clean-save | skip | diff
;;   LOOP-EFFECT ∈ continue | save-rest | stop | reprompt

(ert-deftest test-cbf-ssb-action-save ()
  "Normal: y saves this buffer and continues prompting."
  (should (equal (cj/--save-some-buffers-action ?y) '(save . continue))))

(ert-deftest test-cbf-ssb-action-skip ()
  "Normal: n skips this buffer and continues."
  (should (equal (cj/--save-some-buffers-action ?n) '(skip . continue))))

(ert-deftest test-cbf-ssb-action-clean-save ()
  "Normal: w cleans whitespace, saves this buffer, and continues."
  (should (equal (cj/--save-some-buffers-action ?w) '(clean-save . continue))))

(ert-deftest test-cbf-ssb-action-save-rest ()
  "Boundary: ! saves this buffer and all remaining without asking."
  (should (equal (cj/--save-some-buffers-action ?!) '(save . save-rest))))

(ert-deftest test-cbf-ssb-action-save-this-stop ()
  "Boundary: . saves this buffer and skips the rest."
  (should (equal (cj/--save-some-buffers-action ?.) '(save . stop))))

(ert-deftest test-cbf-ssb-action-quit ()
  "Boundary: q saves no more buffers (skips this and the rest)."
  (should (equal (cj/--save-some-buffers-action ?q) '(skip . stop))))

(ert-deftest test-cbf-ssb-action-diff ()
  "Normal: d views the diff and re-prompts rather than resolving."
  (should (equal (cj/--save-some-buffers-action ?d) '(diff . reprompt))))

(ert-deftest test-cbf-ssb-action-unknown ()
  "Error: an unmapped key returns nil."
  (should-not (cj/--save-some-buffers-action ?z)))

;;; --------------------- cj/--save-some-buffers-choices -----------------------

(ert-deftest test-cbf-ssb-choices-cover-all-keys ()
  "Normal: the choice list offers every save-loop key, each labeled."
  (let ((choices (cj/--save-some-buffers-choices)))
    (dolist (key '(?y ?n ?w ?d ?! ?. ?q))
      (let ((entry (assq key choices)))
        (should entry)
        ;; entry is (KEY NAME &optional DESC); NAME must be a non-empty label.
        (should (stringp (nth 1 entry)))
        (should (> (length (nth 1 entry)) 0))))))

(ert-deftest test-cbf-ssb-choices-terse-names ()
  "Boundary: inline names are single words so the menu takes minimum space."
  (dolist (entry (cj/--save-some-buffers-choices))
    (let ((name (nth 1 entry)))
      (should (stringp name))
      (should-not (string-match-p " " name)))))

(ert-deftest test-cbf-ssb-choices-clean-mentions-whitespace ()
  "Normal: the clean-and-save choice is labeled with whitespace."
  (let ((entry (assq ?w (cj/--save-some-buffers-choices))))
    (should (string-match-p "whitespace"
                            (mapconcat #'identity (cdr entry) " ")))))

;;; ---------------------- cj/--save-some-buffers-plan -------------------------
;; The pure planner: given the candidate BUFFERS and a KEY-FN that yields a
;; (non-diff) key per buffer, resolve each to `save' / `clean-save' / `skip',
;; honoring ! (save the rest) and . / q (stop after this).  Buffers are opaque
;; here (symbols stand in), so the planner is testable without real buffers.

(declare-function cj/--save-some-buffers-plan "custom-buffer-file" (buffers key-fn))

(ert-deftest test-cbf-ssb-plan-mixed ()
  "Normal: y / n / w resolve per-buffer to save / skip / clean-save."
  (let* ((keys '((a . ?y) (b . ?n) (c . ?w)))
         (plan (cj/--save-some-buffers-plan
                '(a b c) (lambda (buf) (cdr (assq buf keys))))))
    (should (equal plan '((a . save) (b . skip) (c . clean-save))))))

(ert-deftest test-cbf-ssb-plan-save-rest ()
  "Boundary: ! saves this and all remaining without consulting KEY-FN again."
  (let* ((asked nil)
         (plan (cj/--save-some-buffers-plan
                '(a b c)
                (lambda (buf) (push buf asked) (if (eq buf 'a) ?! ?n)))))
    (should (equal plan '((a . save) (b . save) (c . save))))
    ;; key-fn consulted only for the first buffer; the rest ride save-all.
    (should (equal asked '(a)))))

(ert-deftest test-cbf-ssb-plan-save-this-stop ()
  "Boundary: . saves this buffer and skips the rest."
  (let ((plan (cj/--save-some-buffers-plan
               '(a b c) (lambda (buf) (if (eq buf 'a) ?. ?y)))))
    (should (equal plan '((a . save) (b . skip) (c . skip))))))

(ert-deftest test-cbf-ssb-plan-quit-skips-all ()
  "Boundary: q skips this buffer and all remaining."
  (let ((plan (cj/--save-some-buffers-plan
               '(a b c) (lambda (_) ?q))))
    (should (equal plan '((a . skip) (b . skip) (c . skip))))))

(ert-deftest test-cbf-ssb-plan-empty ()
  "Boundary: no candidate buffers yields an empty plan."
  (should-not (cj/--save-some-buffers-plan '() (lambda (_) ?y))))

(provide 'test-custom-buffer-file--save-some-buffers)
;;; test-custom-buffer-file--save-some-buffers.el ends here

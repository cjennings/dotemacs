;;; test-org-config-finalize-task.el --- Tests for cj/org-finalize-task -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers `cj/org-finalize-task' and its helpers: the dated-rewrite vs
;; close-in-place dispatch, the two heading transforms, and the guard.
;;
;; The journal-copy hook (`org-after-todo-state-change-hook') is bound to
;; nil in every buffer test so the org-roam daily side effect never fires --
;; that hook is the external boundary, mocked out here.  `org-todo-keywords'
;; is set to the project's full sequence so DOING / VERIFY / CANCELLED resolve
;; as keywords inside the temp buffers regardless of load state.

;;; Code:

(require 'ert)
(require 'org)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'org-config)

;; Fixed instant so the stamp is deterministic in shape.  The exact value and
;; tz offset are still system-dependent, so assertions match the FORMAT, not a
;; literal string (per testing.md: assert behavior, not exact text).
(defconst test-finalize--time (encode-time 24 18 14 22 5 2026)
  "2026-05-22 14:18:24, local time, for deterministic stamp shape.")

(defmacro test-finalize--with-heading (text &rest body)
  "Insert TEXT in a temp Org buffer, point on the first heading, run BODY.
Returns the resulting buffer string.  Binds the project keyword set, nils
the todo-state-change hook (no journal side effect), and inhibits logging."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((org-todo-keywords
            '((sequence "TODO" "PROJECT" "DOING" "WAITING" "VERIFY"
                        "STALLED" "DELEGATED" "|" "FAILED" "DONE" "CANCELLED")))
           (org-mode-hook nil)          ; isolate from unrelated hooks (org-tidy etc.)
           (org-after-todo-state-change-hook nil)
           (org-inhibit-logging t))
       (insert ,text)
       (org-mode)
       (goto-char (point-min))
       ,@body
       (buffer-string))))

;; ---------------------------- predicate --------------------------------------

(ert-deftest test-org-finalize-dated-p-level3-true ()
  "Normal: a level-3 sub-task takes the dated rewrite."
  (should (cj/--org-finalize-dated-p 3 "TODO")))

(ert-deftest test-org-finalize-dated-p-level2-false ()
  "Normal: a level-2 task stays task-shaped (close in place)."
  (should-not (cj/--org-finalize-dated-p 2 "TODO")))

(ert-deftest test-org-finalize-dated-p-verify-true-at-level2 ()
  "Boundary: VERIFY flips to dated at all depths (todo-format exception)."
  (should (cj/--org-finalize-dated-p 2 "VERIFY")))

(ert-deftest test-org-finalize-dated-p-level1-false ()
  "Boundary: a level-1 heading is not dated."
  (should-not (cj/--org-finalize-dated-p 1 "TODO")))

;; ------------------------- dated rewrite -------------------------------------

(ert-deftest test-org-finalize-rewrite-dated-strips-and-prepends ()
  "Normal: keyword and cookie gone, tag kept, stamp prepended in exact format."
  (let ((out (test-finalize--with-heading "*** TODO [#A] Buy milk :shop:\nbody\n"
               (cj/--org-finalize-rewrite-dated test-finalize--time))))
    (should (string-match-p
             "^\\*\\*\\* [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Z][a-z]\\{2\\} @ [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} [-+][0-9]\\{4\\} Buy milk[ \t]+:shop:"
             out))
    (should-not (string-match-p "TODO" out))
    (should-not (string-match-p "\\[#A\\]" out))))

(ert-deftest test-org-finalize-rewrite-dated-no-priority ()
  "Boundary: a heading without a priority cookie still rewrites."
  (let ((out (test-finalize--with-heading "*** DOING Refactor thing\n"
               (cj/--org-finalize-rewrite-dated test-finalize--time))))
    (should (string-match-p "Refactor thing" out))
    (should-not (string-match-p "DOING" out))))

(ert-deftest test-org-finalize-rewrite-dated-no-tags ()
  "Boundary: a heading without tags rewrites cleanly."
  (let ((out (test-finalize--with-heading "*** TODO Plain task\n"
               (cj/--org-finalize-rewrite-dated test-finalize--time))))
    (should (string-match-p "Plain task" out))
    (should-not (string-match-p "TODO" out))))

;; ------------------------- close in place ------------------------------------

(ert-deftest test-org-finalize-close-in-place-adds-date-only-closed ()
  "Normal: a date-only CLOSED line is added; keyword retained, no time part."
  (let ((out (test-finalize--with-heading "** DONE [#A] Ship it :rel:\n"
               (cj/--org-finalize-close-in-place test-finalize--time))))
    (should (string-match-p
             "CLOSED: \\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [A-Z][a-z]\\{2\\}\\]" out))
    (should (string-match-p "DONE" out))
    (should-not (string-match-p "CLOSED: \\[[0-9-]+ [A-Z][a-z]+ [0-9]+:[0-9]+" out))))

;; ------------------------- command / guard -----------------------------------

(ert-deftest test-org-finalize-task-errors-outside-org ()
  "Error: the command refuses to run outside Org."
  (with-temp-buffer
    (fundamental-mode)
    (should-error (cj/org-finalize-task "DONE") :type 'user-error)))

(ert-deftest test-org-finalize-task-errors-on-non-task-heading ()
  "Error: a heading with no actionable keyword cannot be finalized."
  (with-temp-buffer
    (let ((org-mode-hook nil)
          (org-after-todo-state-change-hook nil))
      (insert "* Just a section\n")
      (org-mode)
      (goto-char (point-min))
      (should-error (cj/org-finalize-task "DONE") :type 'user-error))))

(ert-deftest test-org-finalize-task-level3-produces-dated-entry ()
  "Integration: finalizing a level-3 task yields a dated entry, no CLOSED cruft."
  (let ((out (test-finalize--with-heading "*** TODO [#B] Wire the thing :feat:\n"
               (cj/org-finalize-task "DONE" test-finalize--time))))
    (should (string-match-p "^\\*\\*\\* [0-9]\\{4\\}-.*Wire the thing" out))
    (should-not (string-match-p "TODO\\|\\[#B\\]\\|CLOSED" out))))

(ert-deftest test-org-finalize-task-level2-keeps-keyword-adds-closed ()
  "Integration: finalizing a level-2 task keeps the keyword and adds CLOSED."
  (let ((out (test-finalize--with-heading "** TODO [#A] Big task :proj:\n"
               (cj/org-finalize-task "DONE" test-finalize--time))))
    (should (string-match-p "DONE" out))
    (should (string-match-p "CLOSED: \\[" out))
    (should-not (string-match-p "TODO" out))))

;; ------------------------- keybinding wiring ---------------------------------

(ert-deftest test-org-finalize-task-bound-on-org-map ()
  "Normal: the command is bound to `d' under the org prefix (C-; O d)."
  (should (eq (keymap-lookup cj/org-map "d") #'cj/org-finalize-task)))

(provide 'test-org-config-finalize-task)
;;; test-org-config-finalize-task.el ends here

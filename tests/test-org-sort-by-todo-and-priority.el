;;; test-org-sort-by-todo-and-priority.el --- Tests for cj/org-sort-by-todo-and-priority -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for cj/org-sort-by-todo-and-priority function.
;; Tests multi-level sorting: TODO status (TODO before DONE) and priority (A before B before C).
;;
;; Testing approach:
;; - Use real org-mode buffers (don't mock org-sort-entries)
;; - Trust org-mode framework works correctly
;; - Test OUR integration logic: calling org-sort-entries twice in correct order
;; - Verify final sort order matches expected TODO/priority combination
;;
;; The function uses stable sorting:
;; 1. First sort by priority (A, B, C, D, none)
;; 2. Then sort by TODO status (TODO before DONE)
;; Result: Priority order preserved within each TODO state group

;;; Code:

(require 'ert)
(require 'org)
(require 'org-config)  ; Defines cj/org-sort-by-todo-and-priority

;;; Test Helpers

(defun test-org-sort-by-todo-and-priority--create-buffer (content)
  "Create a temporary org-mode buffer with CONTENT.
Returns the buffer object.
Disables org-mode hooks to avoid missing package dependencies in batch mode."
  (let ((buf (generate-new-buffer "*test-org-sort*")))
    (with-current-buffer buf
      ;; Disable hooks to prevent org-superstar and other package loads
      (let ((org-mode-hook nil))
        (org-mode))
      (insert content)
      (goto-char (point-min)))
    buf))

(defun test-org-sort-by-todo-and-priority--get-entry-order (buffer)
  "Extract ordered list of TODO states and priorities from BUFFER.
Returns list of strings like \"TODO [#A]\" or \"DONE\" for each heading."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (entries)
      (org-map-entries
       (lambda ()
         (let* ((todo-state (org-get-todo-state))
                ;; Get heading: no-tags, no-todo, KEEP priority, no-comment
                (heading (org-get-heading t t nil t))
                ;; Extract priority cookie from heading text
                (priority (when (string-match "\\[#\\([A-Z]\\)\\]" heading)
                           (match-string 1 heading))))
           (push (if priority
                     (format "%s [#%s]" (or todo-state "") priority)
                   (or todo-state ""))
                 entries)))
       nil 'tree)
      (nreverse entries))))

(defun test-org-sort-by-todo-and-priority--sort-children (buffer)
  "Position cursor on parent heading in BUFFER and sort its children.
Moves to first * heading (Parent) and calls sort function to sort children."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward "^\\* " nil t)
      (beginning-of-line)
      (cj/org-sort-by-todo-and-priority))))

;;; Normal Cases

(ert-deftest test-org-sort-by-todo-and-priority-normal-mixed-todo-done-sorts-correctly ()
  "Test mixed TODO and DONE entries with various priorities sort correctly.

Input: TODO [#A], DONE [#B], TODO [#C], DONE [#A]
Expected: TODO [#A], TODO [#C], DONE [#A], DONE [#B]"
  (let* ((content "* Parent
** TODO [#A] First task
** DONE [#B] Second task
** TODO [#C] Third task
** DONE [#A] Fourth task
")
         (buf (test-org-sort-by-todo-and-priority--create-buffer content)))
    (unwind-protect
        (progn
          (test-org-sort-by-todo-and-priority--sort-children buf)
          (let ((order (test-org-sort-by-todo-and-priority--get-entry-order buf)))
            (should (equal order '("" "TODO [#A]" "TODO [#C]" "DONE [#A]" "DONE [#B]")))))
      (kill-buffer buf))))

(ert-deftest test-org-sort-by-todo-and-priority-normal-multiple-todos-sorts-by-priority ()
  "Test multiple TODO entries sort by priority A before B before C.

Input: TODO [#C], TODO [#A], TODO [#B]
Expected: TODO [#A], TODO [#B], TODO [#C]"
  (let* ((content "* Parent
** TODO [#C] Task C
** TODO [#A] Task A
** TODO [#B] Task B
")
         (buf (test-org-sort-by-todo-and-priority--create-buffer content)))
    (unwind-protect
        (progn
          (test-org-sort-by-todo-and-priority--sort-children buf)
          (let ((order (test-org-sort-by-todo-and-priority--get-entry-order buf)))
            (should (equal order '("" "TODO [#A]" "TODO [#B]" "TODO [#C]")))))
      (kill-buffer buf))))

(ert-deftest test-org-sort-by-todo-and-priority-normal-multiple-dones-sorts-by-priority ()
  "Test multiple DONE entries sort by priority A before B before C.

Input: DONE [#C], DONE [#A], DONE [#B]
Expected: DONE [#A], DONE [#B], DONE [#C]"
  (let* ((content "* Parent
** DONE [#C] Done C
** DONE [#A] Done A
** DONE [#B] Done B
")
         (buf (test-org-sort-by-todo-and-priority--create-buffer content)))
    (unwind-protect
        (progn
          (test-org-sort-by-todo-and-priority--sort-children buf)
          (let ((order (test-org-sort-by-todo-and-priority--get-entry-order buf)))
            (should (equal order '("" "DONE [#A]" "DONE [#B]" "DONE [#C]")))))
      (kill-buffer buf))))

(ert-deftest test-org-sort-by-todo-and-priority-normal-same-priority-todo-before-done ()
  "Test entries with same priority sort TODO before DONE.

Input: DONE [#A], TODO [#A]
Expected: TODO [#A], DONE [#A]"
  (let* ((content "* Parent
** DONE [#A] Done task
** TODO [#A] Todo task
")
         (buf (test-org-sort-by-todo-and-priority--create-buffer content)))
    (unwind-protect
        (progn
          (test-org-sort-by-todo-and-priority--sort-children buf)
          (let ((order (test-org-sort-by-todo-and-priority--get-entry-order buf)))
            (should (equal order '("" "TODO [#A]" "DONE [#A]")))))
      (kill-buffer buf))))

;;; Boundary Cases

(ert-deftest test-org-sort-by-todo-and-priority-boundary-empty-section-no-error ()
  "Test sorting empty section does not signal error.

Input: Heading with no children
Expected: No error, no change"
  (let* ((content "* Parent\n")
         (buf (test-org-sort-by-todo-and-priority--create-buffer content)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (should-not (condition-case err
                          (progn
                            (cj/org-sort-by-todo-and-priority)
                            nil)
                        (error err))))
      (kill-buffer buf))))

(ert-deftest test-org-sort-by-todo-and-priority-boundary-single-todo-no-change ()
  "Test sorting single TODO entry does not change order.

Input: Single TODO [#A]
Expected: Same order (no change)"
  (let* ((content "* Parent
** TODO [#A] Only task
")
         (buf (test-org-sort-by-todo-and-priority--create-buffer content)))
    (unwind-protect
        (progn
          (test-org-sort-by-todo-and-priority--sort-children buf)
          (let ((order (test-org-sort-by-todo-and-priority--get-entry-order buf)))
            (should (equal order '("" "TODO [#A]")))))
      (kill-buffer buf))))

(ert-deftest test-org-sort-by-todo-and-priority-boundary-single-done-no-change ()
  "Test sorting single DONE entry does not change order.

Input: Single DONE [#B]
Expected: Same order (no change)"
  (let* ((content "* Parent
** DONE [#B] Only task
")
         (buf (test-org-sort-by-todo-and-priority--create-buffer content)))
    (unwind-protect
        (progn
          (test-org-sort-by-todo-and-priority--sort-children buf)
          (let ((order (test-org-sort-by-todo-and-priority--get-entry-order buf)))
            (should (equal order '("" "DONE [#B]")))))
      (kill-buffer buf))))

(ert-deftest test-org-sort-by-todo-and-priority-boundary-all-todos-sorts-by-priority ()
  "Test all TODO entries sort by priority only.

Input: TODO [#C], TODO [#A], TODO [#B]
Expected: TODO [#A], TODO [#B], TODO [#C]"
  (let* ((content "* Parent
** TODO [#C] Task C
** TODO [#A] Task A
** TODO [#B] Task B
")
         (buf (test-org-sort-by-todo-and-priority--create-buffer content)))
    (unwind-protect
        (progn
          (test-org-sort-by-todo-and-priority--sort-children buf)
          (let ((order (test-org-sort-by-todo-and-priority--get-entry-order buf)))
            (should (equal order '("" "TODO [#A]" "TODO [#B]" "TODO [#C]")))))
      (kill-buffer buf))))

(ert-deftest test-org-sort-by-todo-and-priority-boundary-all-dones-sorts-by-priority ()
  "Test all DONE entries sort by priority only.

Input: DONE [#B], DONE [#D], DONE [#A]
Expected: DONE [#A], DONE [#B], DONE [#D]"
  (let* ((content "* Parent
** DONE [#B] Done B
** DONE [#D] Done D
** DONE [#A] Done A
")
         (buf (test-org-sort-by-todo-and-priority--create-buffer content)))
    (unwind-protect
        (progn
          (test-org-sort-by-todo-and-priority--sort-children buf)
          (let ((order (test-org-sort-by-todo-and-priority--get-entry-order buf)))
            (should (equal order '("" "DONE [#A]" "DONE [#B]" "DONE [#D]")))))
      (kill-buffer buf))))

(ert-deftest test-org-sort-by-todo-and-priority-boundary-no-priorities-sorts-by-todo ()
  "Test entries without priorities sort by TODO status only.

Input: TODO (no priority), DONE (no priority), TODO (no priority)
Expected: TODO, TODO, DONE"
  (let* ((content "* Parent
** TODO Task 1
** DONE Task 2
** TODO Task 3
")
         (buf (test-org-sort-by-todo-and-priority--create-buffer content)))
    (unwind-protect
        (progn
          (test-org-sort-by-todo-and-priority--sort-children buf)
          (let ((order (test-org-sort-by-todo-and-priority--get-entry-order buf)))
            (should (equal order '("" "TODO" "TODO" "DONE")))))
      (kill-buffer buf))))

(ert-deftest test-org-sort-by-todo-and-priority-boundary-unprioritized-after-prioritized ()
  "Test unprioritized entries appear after prioritized within TODO/DONE groups.

Input: TODO (no priority), TODO [#A], DONE [#B], DONE (no priority)
Expected: TODO [#A], TODO (no priority), DONE [#B], DONE (no priority)"
  (let* ((content "* Parent
** TODO Task no priority
** TODO [#A] Task A
** DONE [#B] Done B
** DONE Done no priority
")
         (buf (test-org-sort-by-todo-and-priority--create-buffer content)))
    (unwind-protect
        (progn
          (test-org-sort-by-todo-and-priority--sort-children buf)
          (let ((order (test-org-sort-by-todo-and-priority--get-entry-order buf)))
            (should (equal order '("" "TODO [#A]" "TODO" "DONE [#B]" "DONE")))))
      (kill-buffer buf))))

;;; Error Cases

(ert-deftest test-org-sort-by-todo-and-priority-error-non-org-buffer-signals-error ()
  "Test calling in non-org-mode buffer signals user-error.

Input: fundamental-mode buffer
Expected: user-error"
  (let ((buf (generate-new-buffer "*test-non-org*")))
    (unwind-protect
        (with-current-buffer buf
          (fundamental-mode)
          (should-error (cj/org-sort-by-todo-and-priority) :type 'user-error))
      (kill-buffer buf))))

(provide 'test-org-sort-by-todo-and-priority)
;;; test-org-sort-by-todo-and-priority.el ends here

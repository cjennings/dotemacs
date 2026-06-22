;;; test-jumper--register-hygiene.el --- Tests for jumper register hygiene -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for three related jumper.el defects from the 2026-06 config audit:
;;
;; 1. Register collisions on removal — removal shifted the vector but never
;;    freed the dropped register char, and a later store allocated by
;;    `jumper--next-index' (a char a surviving slot might still hold),
;;    silently overwriting that slot's marker. Store now allocates the first
;;    free char in the live slice; removal clears the freed register.
;; 2. Dead-marker errors — `jumper--with-marker-at' guarded `markerp' but not
;;    buffer liveness, so after the buffer holding a location was killed,
;;    store/jump signaled wrong-type errors. Dead entries are now skipped.
;; 3. Single-location toggle never toggled back — the `already-there' branch
;;    did nothing; it now jumps to the last-location register when set.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'jumper)

(defvar test-jumper-hyg--orig-registers nil)
(defvar test-jumper-hyg--orig-index nil)

(defun test-jumper-hyg-setup ()
  "Reset jumper state and the registers it uses to a clean slate."
  (setq test-jumper-hyg--orig-registers jumper--registers)
  (setq test-jumper-hyg--orig-index jumper--next-index)
  (setq jumper--registers (make-vector jumper-max-locations nil))
  (setq jumper--next-index 0)
  (dotimes (i jumper-max-locations)
    (set-register (+ ?0 i) nil))
  (set-register jumper--last-location-register nil))

(defun test-jumper-hyg-teardown ()
  "Restore jumper state."
  (setq jumper--registers test-jumper-hyg--orig-registers)
  (setq jumper--next-index test-jumper-hyg--orig-index))

;;; Defect 1 — register collisions on removal

(ert-deftest test-jumper-hyg-store-after-remove-reuses-freed-register ()
  "Normal: storing after a removal reuses the freed char, not next-index.
Removing index 0 of [0 1 2] leaves the live slice holding chars 1 and 2;
the next store must take the freed char 0, never 2 (which slot 1 still holds)."
  (test-jumper-hyg-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line 1\nline 2\nline 3\nline 4")
        (goto-char (point-min))
        (jumper--do-store-location)            ; ?0 @ line 1
        (forward-line 1) (jumper--do-store-location) ; ?1 @ line 2
        (forward-line 1) (jumper--do-store-location) ; ?2 @ line 3
        (jumper--do-remove-location 0)         ; live slice now [?1 ?2]
        (forward-line 1)                       ; line 4
        (let ((reg (jumper--do-store-location)))
          (should (= reg ?0))                  ; freed char reused
          (should (= (aref jumper--registers 2) ?0))
          (should (= jumper--next-index 3))))
    (test-jumper-hyg-teardown)))

(ert-deftest test-jumper-hyg-store-after-remove-preserves-survivor ()
  "Normal: the surviving slot's marker is not clobbered by the reused store.
After removing index 0 and storing a new location, jumping to the slot that
holds the old top register must still land on its original line."
  (test-jumper-hyg-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line 1\nline 2\nline 3\nline 4")
        (goto-char (point-min))
        (jumper--do-store-location)                  ; ?0 @ line 1
        (forward-line 1) (jumper--do-store-location) ; ?1 @ line 2
        (let ((line3 (progn (forward-line 1) (point))))
          (jumper--do-store-location)                ; ?2 @ line 3
          (jumper--do-remove-location 0)             ; slot1 now holds ?2 @ line3
          (goto-char (point-max)) (jumper--do-store-location) ; reuse ?0
          (goto-char (point-min))
          (jumper--do-jump-to-location 1)            ; slot1 = old line-3 marker
          (should (= (point) line3))))
    (test-jumper-hyg-teardown)))

(ert-deftest test-jumper-hyg-remove-clears-freed-register ()
  "Boundary: removing a location clears its register so the marker is freed."
  (test-jumper-hyg-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "test")
        (goto-char (point-min))
        (jumper--do-store-location)            ; ?0
        (should (get-register ?0))
        (jumper--do-remove-location 0)
        (should (null (get-register ?0))))
    (test-jumper-hyg-teardown)))

;;; Defect 2 — dead-marker entries are skipped, not errored

(ert-deftest test-jumper-hyg-with-marker-at-dead-buffer-returns-nil ()
  "Error: a marker whose buffer was killed yields nil, not a wrong-type error."
  (test-jumper-hyg-setup)
  (let ((buf (generate-new-buffer "jumper-dead-test")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "content")
            (goto-char (point-min))
            (jumper--do-store-location))       ; ?0 points into buf
          (kill-buffer buf)                    ; marker now detached
          (should (null (jumper--with-marker-at 0 (lambda () 'ran)))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (test-jumper-hyg-teardown))))

(ert-deftest test-jumper-hyg-location-exists-p-survives-dead-buffer ()
  "Boundary: location-exists-p does not error when a stored buffer is dead."
  (test-jumper-hyg-setup)
  (let ((buf (generate-new-buffer "jumper-dead-test-2")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "content")
            (goto-char (point-min))
            (jumper--do-store-location))
          (kill-buffer buf)
          (should (null (jumper--location-exists-p))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (test-jumper-hyg-teardown))))

(ert-deftest test-jumper-hyg-candidates-skip-dead-buffer ()
  "Boundary: the candidate list omits a location whose buffer was killed."
  (test-jumper-hyg-setup)
  (let ((buf (generate-new-buffer "jumper-dead-test-3")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "content")
            (goto-char (point-min))
            (jumper--do-store-location))
          (kill-buffer buf)
          (should (null (jumper--location-candidates))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (test-jumper-hyg-teardown))))

;;; Defect 3 — single-location toggle returns to the previous spot

(ert-deftest test-jumper-hyg-toggle-back-when-last-set ()
  "Normal: toggling at the only location jumps back to the last-location register.
Jump to the location (which records the prior spot in 'z); toggling again while
sitting on the location returns to that prior spot."
  (test-jumper-hyg-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line 1\nline 2\nline 3")
        (goto-char (point-min))
        (jumper--do-store-location)            ; store @ line 1
        (let ((away (point-max)))
          (goto-char away)
          (jumper--do-jump-to-location nil)    ; jump to line 1, 'z := away
          (should (= (point) (point-min)))
          (let ((result (jumper--do-jump-to-location nil))) ; toggle back
            (should (eq result 'jumped-back))
            (should (= (point) away)))))
    (test-jumper-hyg-teardown)))

(ert-deftest test-jumper-hyg-toggle-at-location-no-last-stays ()
  "Boundary: toggling at the location with no last-location set returns
'already-there and does not move point."
  (test-jumper-hyg-setup)
  (unwind-protect
      (with-temp-buffer
        (insert "line 1\nline 2")
        (goto-char (point-min))
        (jumper--do-store-location)
        (let ((result (jumper--do-jump-to-location nil)))
          (should (eq result 'already-there))
          (should (= (point) (point-min)))))
    (test-jumper-hyg-teardown)))

(provide 'test-jumper--register-hygiene)
;;; test-jumper--register-hygiene.el ends here

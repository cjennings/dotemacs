;;; test-flyspell-config-functions.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;; Evaluate the buffer, then run (ert-all-tests).

;;; Code:

(add-to-list 'load-path (concat user-emacs-directory "modules"))
(require 'flyspell-config)

;; --------------------------- Flyspell Overlay Tests --------------------------

(ert-deftest cj/flyspell-overlay-test-positive ()
  "Simplest positive test for \='cj/find-previous-flyspell-overlay\='.
With one  misspelling, cj/find-previous-flyspell-overlay should return the
character position at the beginning of the misspelled word."
  (with-temp-buffer
	(let ((misspelled "mispeled")
		  (overlay-pos))
	  ;; insert some text
	  (insert (format "some text for testing. %s" misspelled))

	  ;; trigger flyspell and wait for it to complete
	  (flyspell-buffer)
	  (sit-for 1)

	  ;; call the function with position at end of the buffer
	  (setq overlay-pos (cj/find-previous-flyspell-overlay (point-max)))

	  ;; test flyspell-auto-correct-previous-pos is at char position of 'mispeled'.
	  (should (eq (- (point-max) (length misspelled)) overlay-pos)))))

(ert-deftest cj/flyspell-overlay-test-negative ()
  "Simplest negative test for \='cj/find-previous-flyspell-overlay\='.
With no misspelled words, cj/find-previous-flyspell-overlay should return nil."
  (with-temp-buffer
	(insert "This is a correctly spelled sentence.")
	(flyspell-buffer)
	;; No overlay should exist, so test the result is nil.
	(should-not (cj/find-previous-flyspell-overlay (point-max)))))

(ert-deftest cj/flyspell-overlay-test-positive-multiple ()
  "Positive test for \='cj/find-previous-flyspell-overlay\='.
With several misspellings above and below, cj/find-previous-flyspell-overlay
should return the character position at the beginning of the previous misspelled
word."
  (with-temp-buffer
	(let ((misspelled0 "incorect")
		  (misspelled1 "wrongg")
		  (misspelled2 "erroor")
		  (misspelled3 "mistken")
		  (actual-pos)
		  (expected-pos)
		  (between-pos))

	  ;; insert some text with misspellings
	  (insert (format "flyspell should catch this: %s" misspelled0))
	  (insert (format "flyspell should catch this: %s" misspelled1))

	  ;; calculate the overlay's expected position based on our current position
	  (setq expected-pos (- (point) (length misspelled1)))

	  ;; calculate a position in between misspellings
	  (setq between-pos (+ expected-pos (length misspelled1) 5))

	  ;; insert the rest of the misspellings
	  (insert (format "flyspell should catch this: %s" misspelled2))
	  (insert (format "flyspell should catch this: %s" misspelled3))

	  ;; trigger Flyspell and wait for it to identify all misspellings.
	  (flyspell-buffer)
	  (sit-for 1)

	  ;; call the function with position in between misspellings
	  (setq actual-pos (cj/find-previous-flyspell-overlay between-pos))
	  (should (eq expected-pos actual-pos)))))


(ert-deftest cj/flyspell-goto-previous-misspelling-positive ()
  "Positive test for \='cj/flyspell-goto-previous-misspelling\='.
With a simple misspelling above, cj/flyspell-goto-previous-misspelling
should land on the next misspelled word."
  (with-temp-buffer
    (let ((misspelled-word "incorect")
          (actual-word))

      ;; insert some text with misspellings
      (insert (format "flyspell should catch this: %s" misspelled-word))

      ;; trigger Flyspell and wait for it to identify all misspellings.
      (flyspell-buffer)
      (sit-for 1)

      ;; call the function with position in between misspellings
      (setq actual-word (cj/flyspell-goto-previous-misspelling (point-max)))
      (should (string= misspelled-word actual-word)))))

(ert-deftest cj/flyspell-goto-previous-misspelling-negative ()
  "Negative test for \='cj/flyspell-goto-previous-misspelling\='.
With no misspellings, cj/flyspell-goto-previous-misspelling return nil."
  (with-temp-buffer
    (let ((expected nil)
          (result))

      ;; insert some text with misspellings
      (insert (format "None of these words are misspelled."))

      ;; trigger Flyspell and wait for it to identify all misspellings.
      (flyspell-buffer)
      (sit-for 1)

      ;; call the function with position in between misspellings
      (setq result (cj/flyspell-goto-previous-misspelling (point-max)))
      (message "result is %s" result)
      (should (eq result expected)))))

(ert-deftest cj/flyspell-goto-previous-misspelling-positive-multiple ()
  "Positive test for \='cj/flyspell-goto-previous-misspelling\='.
With several misspellings above and below, cj/flyspell-goto-previous-misspelling
should return the misspelled word just previous to the position of the cursor."
  (with-temp-buffer
    (let ((misspelled0 "incorect")
          (misspelled1 "wrongg")
          (misspelled2 "erroor")
          (misspelled3 "mistken")
          (result)
          (between-pos))

      ;; insert some text with misspellings
      (insert (format "flyspell should catch this: %s\n" misspelled0))
      (insert (format "flyspell should catch this: %s\n" misspelled1))

      ;; calculate a position in between misspellings
      (setq between-pos (+ (point) (length misspelled1) 5))

      ;; insert the rest of the misspellings
      (insert (format "flyspell should catch this: %s\n" misspelled2))
      (insert (format "flyspell should catch this: %s\n" misspelled3))

      ;; trigger Flyspell and wait for it to identify all misspellings.
      (flyspell-buffer)
      (sit-for 1)

      ;; call the function with position in between misspellings
      (setq result (cj/flyspell-goto-previous-misspelling between-pos))
      (should (string= result misspelled1)))))

(provide 'test-flyspell-config-functions)
;;; test-flyspell-config-functions.el ends here.

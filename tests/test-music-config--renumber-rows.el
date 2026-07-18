;;; test-music-config--renumber-rows.el --- Tests for playlist row numbering -*- coding: utf-8; lexical-binding: t; -*-
;;
;; Author: Craig Jennings <c@cjennings.net>
;;
;;; Commentary:
;; Playlist rows carry a numeric overlay prefix so the cursor stays visible
;; when it sits on a cover-art thumbnail and each row's position in the list
;; is readable.  The renumber walks the buffer and rebuilds the overlays; a
;; buffer-local after-change hook debounces it behind an idle timer.  Overlays
;; leave the buffer text untouched (EMMS owns it), so these tests drive plain
;; temp buffers.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'music-config)

(defun test-music-renumber--numbers (buffer)
  "Return the overlay number strings in BUFFER, in position order."
  (with-current-buffer buffer
    (mapcar (lambda (ov) (overlay-get ov 'before-string))
            (sort (seq-filter (lambda (ov) (overlay-get ov 'cj-music-row-number))
                              (overlays-in (point-min) (point-max)))
                  (lambda (a b) (< (overlay-start a) (overlay-start b)))))))

;;; Normal Cases

(ert-deftest test-music-renumber-rows-numbers-each-line ()
  "Normal: every non-blank line gets a sequential number overlay."
  (with-temp-buffer
    (insert "track one\ntrack two\ntrack three\n")
    (cj/music--renumber-rows (current-buffer))
    (should (equal (mapcar #'substring-no-properties
                           (test-music-renumber--numbers (current-buffer)))
                   '("  1 " "  2 " "  3 ")))))

(ert-deftest test-music-renumber-rows-idempotent ()
  "Normal: renumbering twice leaves one overlay per line, not two."
  (with-temp-buffer
    (insert "track one\ntrack two\n")
    (cj/music--renumber-rows (current-buffer))
    (cj/music--renumber-rows (current-buffer))
    (should (= 2 (length (test-music-renumber--numbers (current-buffer)))))))

;;; Boundary Cases

(ert-deftest test-music-renumber-rows-skips-blank-lines ()
  "Boundary: blank lines are not numbered and don't advance the count."
  (with-temp-buffer
    (insert "track one\n\ntrack two\n")
    (cj/music--renumber-rows (current-buffer))
    (should (equal (mapcar #'substring-no-properties
                           (test-music-renumber--numbers (current-buffer)))
                   '("  1 " "  2 ")))))

(ert-deftest test-music-renumber-rows-empty-buffer-no-overlays ()
  "Boundary: an empty buffer gets no overlays and no error."
  (with-temp-buffer
    (cj/music--renumber-rows (current-buffer))
    (should-not (test-music-renumber--numbers (current-buffer)))))

;;; Error Cases

(ert-deftest test-music-renumber-rows-dead-buffer-noop ()
  "Error: renumbering a killed buffer is a silent no-op (the debounce timer
can fire after the playlist buffer is gone)."
  (let ((buf (generate-new-buffer " *test-renumber-dead*")))
    (kill-buffer buf)
    (should-not (cj/music--renumber-rows buf))))

;;; Hook wiring

(ert-deftest test-music-renumber-ensure-playlist-buffer-wires-hook ()
  "Normal: the playlist buffer gets the debounced renumber on after-change."
  (let (created)
    (cl-letf (((symbol-function 'emms-playlist-mode) #'ignore))
      (unwind-protect
          (progn
            (setq created (cj/music--ensure-playlist-buffer))
            (with-current-buffer created
              (should (member #'cj/music--schedule-renumber after-change-functions))))
        (when (buffer-live-p created) (kill-buffer created))))))

(provide 'test-music-config--renumber-rows)
;;; test-music-config--renumber-rows.el ends here

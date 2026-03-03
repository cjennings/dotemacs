;;; test-prog-json--json-format-buffer.el --- Tests for cj/json-format-buffer -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the cj/json-format-buffer function in prog-json.el.
;; Tests both the jq path and the built-in fallback path.

;;; Code:

(require 'ert)
(require 'json)
(require 'prog-json)

;;; Normal Cases — jq path

(ert-deftest test-prog-json--json-format-buffer-normal-formats-object ()
  "Compact JSON object is pretty-printed with sorted keys."
  (with-temp-buffer
    (insert "{\"zebra\":1,\"alpha\":2}")
    (cj/json-format-buffer)
    (should (string= (string-trim (buffer-string))
                     "{\n  \"alpha\": 2,\n  \"zebra\": 1\n}"))))

(ert-deftest test-prog-json--json-format-buffer-normal-formats-array ()
  "Compact JSON array is pretty-printed."
  (with-temp-buffer
    (insert "[1,2,3]")
    (cj/json-format-buffer)
    (should (string= (string-trim (buffer-string))
                     "[\n  1,\n  2,\n  3\n]"))))

(ert-deftest test-prog-json--json-format-buffer-normal-nested ()
  "Nested JSON is pretty-printed with sorted keys at all levels."
  (with-temp-buffer
    (insert "{\"b\":{\"d\":1,\"c\":2},\"a\":3}")
    (cj/json-format-buffer)
    (should (string-match-p "\"a\": 3" (buffer-string)))
    (should (string-match-p "\"c\": 2" (buffer-string)))
    ;; "a" should appear before "b" (sorted)
    (should (< (string-match "\"a\"" (buffer-string))
               (string-match "\"b\"" (buffer-string))))))

(ert-deftest test-prog-json--json-format-buffer-normal-already-formatted ()
  "Already-formatted JSON is unchanged."
  (let ((formatted "{\n  \"alpha\": 1,\n  \"beta\": 2\n}\n"))
    (with-temp-buffer
      (insert formatted)
      (cj/json-format-buffer)
      (should (string= (buffer-string) formatted)))))

;;; Boundary Cases

(ert-deftest test-prog-json--json-format-buffer-boundary-empty-object ()
  "Empty JSON object formats cleanly."
  (with-temp-buffer
    (insert "{}")
    (cj/json-format-buffer)
    (should (string= (string-trim (buffer-string)) "{}"))))

(ert-deftest test-prog-json--json-format-buffer-boundary-empty-array ()
  "Empty JSON array formats cleanly."
  (with-temp-buffer
    (insert "[]")
    (cj/json-format-buffer)
    (should (string= (string-trim (buffer-string)) "[]"))))

(ert-deftest test-prog-json--json-format-buffer-boundary-scalar-string ()
  "Bare JSON string scalar formats without error."
  (with-temp-buffer
    (insert "\"hello\"")
    (cj/json-format-buffer)
    (should (string= (string-trim (buffer-string)) "\"hello\""))))

(ert-deftest test-prog-json--json-format-buffer-boundary-unicode ()
  "JSON with unicode characters is preserved."
  (with-temp-buffer
    (insert "{\"emoji\":\"\\u2764\",\"name\":\"café\"}")
    (cj/json-format-buffer)
    (should (string-match-p "café" (buffer-string)))))

;;; Fallback path — built-in formatter

(ert-deftest test-prog-json--json-format-buffer-fallback-formats-without-jq ()
  "Falls back to built-in formatter when jq is not found."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
    (with-temp-buffer
      (insert "{\"b\":1,\"a\":2}")
      (cj/json-format-buffer)
      ;; Built-in formatter should pretty-print (key order may vary)
      (should (string-match-p "\"a\"" (buffer-string)))
      (should (string-match-p "\"b\"" (buffer-string)))
      ;; Should be multi-line (formatted, not compact)
      (should (> (count-lines (point-min) (point-max)) 1)))))

;;; Error Cases

(ert-deftest test-prog-json--json-format-buffer-error-invalid-json ()
  "Invalid JSON produces an error, does not silently corrupt buffer."
  (with-temp-buffer
    (insert "{not valid json}")
    (let ((original (buffer-string)))
      ;; jq will fail on invalid JSON — buffer should not be emptied
      (condition-case _err
          (cj/json-format-buffer)
        (error nil))
      ;; Buffer should still have content (not wiped)
      (should (> (length (buffer-string)) 0)))))

(provide 'test-prog-json--json-format-buffer)
;;; test-prog-json--json-format-buffer.el ends here

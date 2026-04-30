;;; test-config-utilities--validate-timestamps-in-buffer.el --- Tests for cj/--validate-timestamps-in-buffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `cj/--validate-timestamps-in-buffer'.  The function walks
;; an org buffer's headlines, checks DEADLINE/SCHEDULED/TIMESTAMP
;; properties plus inline timestamps, and returns a list of tuples
;; for every timestamp where `org-time-string-to-absolute' returns
;; nil.  Tests use real org parsing on real timestamps and mock the
;; validator at the boundary so an arbitrary timestamp can be marked
;; invalid for the test.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'config-utilities)
(require 'org)
(require 'org-element)

(defmacro test-config-utilities--with-org-buffer (content &rest body)
  "Run BODY in a temp buffer holding CONTENT in `org-mode'."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (let ((org-mode-hook nil))
       (insert ,content)
       (org-mode)
       ,@body)))

(defmacro test-config-utilities--with-invalid-timestamps (invalid-set &rest body)
  "Run BODY with `org-time-string-to-absolute' returning nil for any
timestamp string in INVALID-SET, normal behaviour otherwise."
  (declare (indent 1) (debug t))
  `(let ((--invalid-set ,invalid-set)
         (--orig (symbol-function 'org-time-string-to-absolute)))
     (cl-letf (((symbol-function 'org-time-string-to-absolute)
                (lambda (s &rest args)
                  (if (member s --invalid-set)
                      (error "test-mocked invalid timestamp: %s" s)
                    (apply --orig s args)))))
       ,@body)))

(ert-deftest test-config-utilities-validate-buffer-no-timestamps-returns-empty ()
  "Boundary: a buffer with no timestamps returns an empty list."
  (test-config-utilities--with-org-buffer "* Heading\nJust prose.\n"
    (should-not (cj/--validate-timestamps-in-buffer "/x.org"))))

(ert-deftest test-config-utilities-validate-buffer-empty-buffer-returns-empty ()
  "Boundary: an empty buffer returns an empty list."
  (test-config-utilities--with-org-buffer ""
    (should-not (cj/--validate-timestamps-in-buffer "/x.org"))))

(ert-deftest test-config-utilities-validate-buffer-valid-timestamps-returns-empty ()
  "Normal: every valid timestamp passes; the result is empty."
  (test-config-utilities--with-org-buffer
      "* Headline
DEADLINE: <2026-04-30 Thu>
"
    (should-not (cj/--validate-timestamps-in-buffer "/x.org"))))

(ert-deftest test-config-utilities-validate-buffer-deadline-flagged-when-invalid ()
  "Normal: an invalid DEADLINE is flagged with property \"DEADLINE\"."
  (test-config-utilities--with-org-buffer
      "* Bad
DEADLINE: <2026-04-30 Thu>
"
    (test-config-utilities--with-invalid-timestamps '("<2026-04-30 Thu>")
      (let ((result (cj/--validate-timestamps-in-buffer "/x.org")))
        (should (= 1 (length result)))
        (cl-destructuring-bind (file _pos head prop ts) (car result)
          (should (equal file "/x.org"))
          (should (equal head "Bad"))
          (should (equal prop "DEADLINE"))
          (should (equal ts "<2026-04-30 Thu>")))))))

(ert-deftest test-config-utilities-validate-buffer-scheduled-flagged-when-invalid ()
  "Normal: an invalid SCHEDULED is flagged with property \"SCHEDULED\"."
  (test-config-utilities--with-org-buffer
      "* Sched
SCHEDULED: <2026-05-01 Fri>
"
    (test-config-utilities--with-invalid-timestamps '("<2026-05-01 Fri>")
      (let ((result (cj/--validate-timestamps-in-buffer "/x.org")))
        (should (= 1 (length result)))
        (should (equal "SCHEDULED" (nth 3 (car result))))))))

(ert-deftest test-config-utilities-validate-buffer-inline-flagged-as-inline ()
  "Normal: an invalid inline timestamp in headline contents is flagged
with property \"inline timestamp\"."
  (test-config-utilities--with-org-buffer
      "* Body has timestamp
Some prose mentioning <2026-06-01 Mon> in passing.
"
    (test-config-utilities--with-invalid-timestamps '("<2026-06-01 Mon>")
      (let ((result (cj/--validate-timestamps-in-buffer "/x.org")))
        (should (= 1 (length result)))
        (should (equal "inline timestamp" (nth 3 (car result))))))))

(ert-deftest test-config-utilities-validate-buffer-multiple-invalid-collected-in-order ()
  "Normal: multiple invalid timestamps are returned in document order."
  (test-config-utilities--with-org-buffer
      "* First
DEADLINE: <2026-01-01 Thu>
* Second
SCHEDULED: <2026-02-02 Mon>
"
    (test-config-utilities--with-invalid-timestamps
        '("<2026-01-01 Thu>" "<2026-02-02 Mon>")
      (let ((result (cj/--validate-timestamps-in-buffer "/x.org")))
        (should (= 2 (length result)))
        (should (equal "First" (nth 2 (nth 0 result))))
        (should (equal "Second" (nth 2 (nth 1 result))))))))

(ert-deftest test-config-utilities-validate-buffer-mixed-valid-and-invalid ()
  "Boundary: buffer with one valid and one invalid timestamp returns
only the invalid one."
  (test-config-utilities--with-org-buffer
      "* Good
DEADLINE: <2026-04-30 Thu>
* Bad
DEADLINE: <2026-12-25 Fri>
"
    (test-config-utilities--with-invalid-timestamps '("<2026-12-25 Fri>")
      (let ((result (cj/--validate-timestamps-in-buffer "/x.org")))
        (should (= 1 (length result)))
        (should (equal "Bad" (nth 2 (car result))))))))

(provide 'test-config-utilities--validate-timestamps-in-buffer)
;;; test-config-utilities--validate-timestamps-in-buffer.el ends here

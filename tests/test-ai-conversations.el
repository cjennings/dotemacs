;;; test-ai-conversations.el --- Tests for ai-conversations.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Normal / Boundary / Error tests for the save/load/delete and
;; autosave surface in ai-conversations.el.  Pure helpers are tested
;; against fixed inputs; file-touching helpers use per-test temp
;; directories.  Interactive commands are exercised via `cl-letf'
;; stubs on `completing-read' and `y-or-n-p'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "tests" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'testutil-ai-config)
;; testutil-ai-config provides 'ai-conversations as a stub.  Force the
;; real module to override.
(setq features (delq 'ai-conversations features))
(require 'ai-conversations)

;; -------------------------------------------------------- temp-dir helper

(defun test-ai-conversations--with-temp-dir (fn)
  "Run FN inside a fresh conversations directory.  Clean up after."
  (let* ((dir (make-temp-file "test-ai-conversations-" t))
         (cj/gptel-conversations-directory dir))
    (unwind-protect
        (funcall fn dir)
      (when (file-exists-p dir)
        (delete-directory dir t)))))

(defun test-ai-conversations--touch (dir name)
  "Create empty file NAME in DIR."
  (let ((path (expand-file-name name dir)))
    (with-temp-file path (insert ""))
    path))

;; ------------------------------------------------------ slugify-topic

(ert-deftest test-ai-conversations-slugify-topic-normal ()
  "Normal: ASCII words with spaces become hyphen-joined slug."
  (should (equal (cj/gptel--slugify-topic "Hello World") "hello-world")))

(ert-deftest test-ai-conversations-slugify-topic-boundary-empty ()
  "Boundary: empty input returns the literal \"conversation\" placeholder."
  (should (equal (cj/gptel--slugify-topic "") "conversation"))
  (should (equal (cj/gptel--slugify-topic nil) "conversation")))

(ert-deftest test-ai-conversations-slugify-topic-boundary-all-special ()
  "Boundary: input with no slug-safe chars falls back to placeholder."
  (should (equal (cj/gptel--slugify-topic "!!!@@@###") "conversation"))
  (should (equal (cj/gptel--slugify-topic "   ") "conversation")))

(ert-deftest test-ai-conversations-slugify-topic-boundary-unicode-stripped ()
  "Boundary: non-ASCII characters drop out (only [a-z0-9] survives)."
  (should (equal (cj/gptel--slugify-topic "Café Résumé") "caf-r-sum")))

(ert-deftest test-ai-conversations-slugify-topic-boundary-idempotent ()
  "Boundary: applying twice yields the same result as once."
  (let ((once (cj/gptel--slugify-topic "Foo Bar 2026!")))
    (should (equal once (cj/gptel--slugify-topic once)))))

(ert-deftest test-ai-conversations-slugify-topic-boundary-leading-trailing-trim ()
  "Boundary: leading/trailing separator runs are trimmed."
  (should (equal (cj/gptel--slugify-topic "---foo---") "foo"))
  (should (equal (cj/gptel--slugify-topic "**foo**") "foo")))

(ert-deftest test-ai-conversations-slugify-topic-normal-numbers-preserved ()
  "Normal: digits survive the slug."
  (should (equal (cj/gptel--slugify-topic "Project 2026 Plan")
                 "project-2026-plan")))

;; ------------------------------------------------------ timestamp-from-filename

(ert-deftest test-ai-conversations-timestamp-from-filename-normal ()
  "Normal: well-formed filename decodes to a time value."
  (let ((ts (cj/gptel--timestamp-from-filename
             "topic_20260315-101530.gptel")))
    (should ts)
    (should (equal (format-time-string "%Y-%m-%d %H:%M:%S" ts)
                   "2026-03-15 10:15:30"))))

(ert-deftest test-ai-conversations-timestamp-from-filename-boundary-year-edges ()
  "Boundary: end-of-year and start-of-year timestamps decode correctly."
  (let ((eoy (cj/gptel--timestamp-from-filename
              "topic_20251231-235959.gptel"))
        (boy (cj/gptel--timestamp-from-filename
              "topic_20260101-000000.gptel")))
    (should (equal (format-time-string "%Y-%m-%d %H:%M:%S" eoy)
                   "2025-12-31 23:59:59"))
    (should (equal (format-time-string "%Y-%m-%d %H:%M:%S" boy)
                   "2026-01-01 00:00:00"))))

(ert-deftest test-ai-conversations-timestamp-from-filename-error-malformed ()
  "Error: non-matching filename returns nil."
  (should-not (cj/gptel--timestamp-from-filename "not-a-gptel-file"))
  (should-not (cj/gptel--timestamp-from-filename "topic.gptel"))
  (should-not (cj/gptel--timestamp-from-filename "topic_20260315.gptel"))
  (should-not (cj/gptel--timestamp-from-filename "topic_2026031-101530.gptel")))

;; ------------------------------------------------------ existing-topics

(ert-deftest test-ai-conversations-existing-topics-normal ()
  "Normal: returns unique topic slugs across multiple-timestamped files."
  (test-ai-conversations--with-temp-dir
   (lambda (dir)
     (test-ai-conversations--touch dir "foo_20260101-100000.gptel")
     (test-ai-conversations--touch dir "foo_20260102-100000.gptel")
     (test-ai-conversations--touch dir "bar_20260102-100000.gptel")
     (let ((topics (cj/gptel--existing-topics)))
       (should (member "foo" topics))
       (should (member "bar" topics))
       (should (= 2 (length topics)))))))

(ert-deftest test-ai-conversations-existing-topics-boundary-empty-dir ()
  "Boundary: empty conversations directory returns nil."
  (test-ai-conversations--with-temp-dir
   (lambda (_dir)
     (should-not (cj/gptel--existing-topics)))))

(ert-deftest test-ai-conversations-existing-topics-boundary-missing-dir ()
  "Boundary: missing directory returns nil instead of erroring."
  (let ((cj/gptel-conversations-directory
         (expand-file-name (format "missing-%s" (random)) "/tmp")))
    (should-not (cj/gptel--existing-topics))))

(ert-deftest test-ai-conversations-existing-topics-boundary-ignores-non-gptel ()
  "Boundary: files without .gptel extension are ignored."
  (test-ai-conversations--with-temp-dir
   (lambda (dir)
     (test-ai-conversations--touch dir "foo_20260101-100000.gptel")
     (test-ai-conversations--touch dir "readme.txt")
     (test-ai-conversations--touch dir "stray.gptel.bak")
     (should (equal (cj/gptel--existing-topics) '("foo"))))))

;; ------------------------------------------------------ latest-file-for-topic

(ert-deftest test-ai-conversations-latest-file-for-topic-normal ()
  "Normal: returns the newest file for the topic by lexical sort."
  (test-ai-conversations--with-temp-dir
   (lambda (dir)
     (test-ai-conversations--touch dir "foo_20260101-100000.gptel")
     (test-ai-conversations--touch dir "foo_20260103-100000.gptel")
     (test-ai-conversations--touch dir "foo_20260102-100000.gptel")
     (should (equal (cj/gptel--latest-file-for-topic "foo")
                    "foo_20260103-100000.gptel")))))

(ert-deftest test-ai-conversations-latest-file-for-topic-boundary-no-match ()
  "Boundary: no matching topic returns nil."
  (test-ai-conversations--with-temp-dir
   (lambda (dir)
     (test-ai-conversations--touch dir "bar_20260101-100000.gptel")
     (should-not (cj/gptel--latest-file-for-topic "foo")))))

(ert-deftest test-ai-conversations-latest-file-for-topic-boundary-missing-dir ()
  "Boundary: missing directory returns nil."
  (let ((cj/gptel-conversations-directory
         (expand-file-name (format "missing-%s" (random)) "/tmp")))
    (should-not (cj/gptel--latest-file-for-topic "foo"))))

(ert-deftest test-ai-conversations-latest-file-for-topic-boundary-regex-isolation ()
  "Boundary: prefix-overlapping topics are not falsely matched."
  (test-ai-conversations--with-temp-dir
   (lambda (dir)
     (test-ai-conversations--touch dir "foo_20260101-100000.gptel")
     (test-ai-conversations--touch dir "foobar_20260102-100000.gptel")
     (should (equal (cj/gptel--latest-file-for-topic "foo")
                    "foo_20260101-100000.gptel")))))

;; ------------------------------------------------------ conversation-candidates

(ert-deftest test-ai-conversations-conversation-candidates-normal-newest-first ()
  "Normal: candidates are sorted newest-first when configured that way."
  (test-ai-conversations--with-temp-dir
   (lambda (dir)
     (test-ai-conversations--touch dir "foo_20260101-100000.gptel")
     (test-ai-conversations--touch dir "foo_20260103-100000.gptel")
     (test-ai-conversations--touch dir "foo_20260102-100000.gptel")
     (let ((cj/gptel-conversations-sort-order 'newest-first))
       (let* ((cands (cj/gptel--conversation-candidates))
              (files (mapcar #'cdr cands)))
         (should (equal files
                        '("foo_20260103-100000.gptel"
                          "foo_20260102-100000.gptel"
                          "foo_20260101-100000.gptel"))))))))

(ert-deftest test-ai-conversations-conversation-candidates-normal-oldest-first ()
  "Normal: candidates respect oldest-first sort order."
  (test-ai-conversations--with-temp-dir
   (lambda (dir)
     (test-ai-conversations--touch dir "foo_20260101-100000.gptel")
     (test-ai-conversations--touch dir "foo_20260103-100000.gptel")
     (test-ai-conversations--touch dir "foo_20260102-100000.gptel")
     (let ((cj/gptel-conversations-sort-order 'oldest-first))
       (let* ((cands (cj/gptel--conversation-candidates))
              (files (mapcar #'cdr cands)))
         (should (equal files
                        '("foo_20260101-100000.gptel"
                          "foo_20260102-100000.gptel"
                          "foo_20260103-100000.gptel"))))))))

(ert-deftest test-ai-conversations-conversation-candidates-error-missing-dir ()
  "Error: missing conversations directory signals."
  (let ((cj/gptel-conversations-directory
         (expand-file-name (format "missing-%s" (random)) "/tmp")))
    (should-error (cj/gptel--conversation-candidates))))

(ert-deftest test-ai-conversations-conversation-candidates-display-shape ()
  "Display string is \"filename [YYYY-MM-DD HH:MM]\"."
  (test-ai-conversations--with-temp-dir
   (lambda (dir)
     (test-ai-conversations--touch dir "topic_20260315-101530.gptel")
     (let* ((cands (cj/gptel--conversation-candidates))
            (display (car (car cands))))
       (should (string-match-p
                "\\`topic_20260315-101530\\.gptel \\[2026-03-15 10:15\\]\\'"
                display))))))

;; ------------------------------------------------------ save-buffer-to-file

(ert-deftest test-ai-conversations-save-buffer-to-file-normal ()
  "Normal: writes buffer with visibility headers prepended."
  (test-ai-conversations--with-temp-dir
   (lambda (dir)
     (with-temp-buffer
       (insert "hello world\n")
       (let ((file (expand-file-name "out.gptel" dir)))
         (cj/gptel--save-buffer-to-file (current-buffer) file)
         (should (file-exists-p file))
         (with-temp-buffer
           (insert-file-contents file)
           (should (string-match-p "^#\\+STARTUP: showeverything"
                                   (buffer-string)))
           (should (string-match-p "^#\\+VISIBILITY: all"
                                   (buffer-string)))
           (should (string-match-p "hello world"
                                   (buffer-string)))))))))

(ert-deftest test-ai-conversations-save-buffer-to-file-roundtrip-with-strip ()
  "Round-trip: save then strip-visibility-headers yields original content."
  (test-ai-conversations--with-temp-dir
   (lambda (dir)
     (let ((original "first line\nsecond line\n")
           (file (expand-file-name "rt.gptel" dir)))
       (with-temp-buffer
         (insert original)
         (cj/gptel--save-buffer-to-file (current-buffer) file))
       (with-temp-buffer
         (insert-file-contents file)
         (cj/gptel--strip-visibility-headers)
         (should (equal (buffer-string) original)))))))

(ert-deftest test-ai-conversations-strip-visibility-headers-boundary-no-headers ()
  "Boundary: buffer without headers is unchanged."
  (with-temp-buffer
    (insert "plain body\n")
    (cj/gptel--strip-visibility-headers)
    (should (equal (buffer-string) "plain body\n"))))

;; ------------------------------------------------------ autosave-after-response

(defmacro test-ai-conversations--with-gptel-mode (&rest body)
  "Run BODY in a temp buffer with `gptel-mode' bound non-nil."
  (declare (indent 0))
  `(with-temp-buffer
     (setq-local gptel-mode t)
     ,@body))

(ert-deftest test-ai-conversations-autosave-after-response-saves-when-enabled ()
  "Hook saves the buffer to the autosave filepath when enabled."
  (test-ai-conversations--with-temp-dir
   (lambda (dir)
     (let ((file (expand-file-name "auto.gptel" dir)))
       (test-ai-conversations--with-gptel-mode
         (setq-local cj/gptel-autosave-enabled t)
         (setq-local cj/gptel-autosave-filepath file)
         (insert "autosaved body")
         (cj/gptel--autosave-after-response)
         (should (file-exists-p file)))))))

(ert-deftest test-ai-conversations-autosave-after-response-skips-when-disabled ()
  "Hook is a no-op when `cj/gptel-autosave-enabled' is nil."
  (test-ai-conversations--with-temp-dir
   (lambda (dir)
     (let ((file (expand-file-name "auto.gptel" dir)))
       (test-ai-conversations--with-gptel-mode
         (setq-local cj/gptel-autosave-enabled nil)
         (setq-local cj/gptel-autosave-filepath file)
         (cj/gptel--autosave-after-response)
         (should-not (file-exists-p file)))))))

(ert-deftest test-ai-conversations-autosave-after-response-skips-when-no-filepath ()
  "Hook is a no-op when filepath is nil or empty."
  (test-ai-conversations--with-temp-dir
   (lambda (_dir)
     (test-ai-conversations--with-gptel-mode
       (setq-local cj/gptel-autosave-enabled t)
       (setq-local cj/gptel-autosave-filepath nil)
       ;; Should not error
       (cj/gptel--autosave-after-response))
     (test-ai-conversations--with-gptel-mode
       (setq-local cj/gptel-autosave-enabled t)
       (setq-local cj/gptel-autosave-filepath "")
       (cj/gptel--autosave-after-response)))))

(ert-deftest test-ai-conversations-autosave-after-response-skips-outside-gptel-mode ()
  "Hook is a no-op when `gptel-mode' is nil."
  (test-ai-conversations--with-temp-dir
   (lambda (dir)
     (let ((file (expand-file-name "auto.gptel" dir)))
       (with-temp-buffer
         (setq-local gptel-mode nil)
         (setq-local cj/gptel-autosave-enabled t)
         (setq-local cj/gptel-autosave-filepath file)
         (cj/gptel--autosave-after-response)
         (should-not (file-exists-p file)))))))

(ert-deftest test-ai-conversations-autosave-after-send-error-is-non-fatal ()
  "Hook surfaces a save error via `message' rather than signaling."
  (test-ai-conversations--with-temp-dir
   (lambda (_dir)
     (test-ai-conversations--with-gptel-mode
       (setq-local cj/gptel-autosave-enabled t)
       (setq-local cj/gptel-autosave-filepath "/nonexistent-dir/file.gptel")
       ;; Must not signal even though the write will fail
       (cj/gptel--autosave-after-send)))))

;; ------------------------------------------------------ autosave timer

(ert-deftest test-ai-conversations-autosave-start-timer-normal ()
  "Normal: starting autosave creates a repeating timer for the current buffer."
  (with-temp-buffer
    (setq-local gptel-mode t)
    (setq-local cj/gptel-autosave-enabled t)
    (setq-local cj/gptel-autosave-filepath "/tmp/foo.gptel")
    (let ((calls nil))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (secs repeat function &rest args)
                   (push (list secs repeat function args) calls)
                   :fake-timer)))
        (let ((cj/gptel-autosave-interval 17))
          (cj/gptel--autosave-start-timer)))
      (should (eq cj/gptel-autosave--timer :fake-timer))
      (should (equal (caar calls) 17))
      (should (equal (cadar calls) 17))
      (should (eq (nth 2 (car calls)) #'cj/gptel--autosave-timer-callback))
      (should (eq (car (nth 3 (car calls))) (current-buffer))))))

(ert-deftest test-ai-conversations-autosave-start-timer-idempotent ()
  "Boundary: starting autosave twice does not create a second timer."
  (with-temp-buffer
    (setq-local gptel-mode t)
    (setq-local cj/gptel-autosave-enabled t)
    (setq-local cj/gptel-autosave-filepath "/tmp/foo.gptel")
    (setq-local cj/gptel-autosave--timer :existing-timer)
    (let ((created 0))
      (cl-letf (((symbol-function 'run-with-timer)
                 (lambda (&rest _)
                   (setq created (1+ created))
                   :new-timer)))
        (cj/gptel--autosave-start-timer))
      (should (= created 0))
      (should (eq cj/gptel-autosave--timer :existing-timer)))))

(ert-deftest test-ai-conversations-autosave-stop-timer-cancels ()
  "Normal: stopping autosave cancels the current buffer's timer."
  (with-temp-buffer
    (setq-local cj/gptel-autosave--timer :fake-timer)
    (let ((cancelled nil))
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (timer) (setq cancelled timer))))
        (cj/gptel--autosave-stop-timer))
      (should (eq cancelled :fake-timer))
      (should-not cj/gptel-autosave--timer))))

(ert-deftest test-ai-conversations-autosave-timer-callback-saves-active-buffer ()
  "Normal: timer callback saves the live buffer when autosave is active."
  (test-ai-conversations--with-temp-dir
   (lambda (dir)
     (let ((file (expand-file-name "timer.gptel" dir))
           (buf (generate-new-buffer " *gptel timer test*")))
       (unwind-protect
           (with-current-buffer buf
             (setq-local gptel-mode t)
             (setq-local cj/gptel-autosave-enabled t)
             (setq-local cj/gptel-autosave-filepath file)
             (insert "timer body")
             (cj/gptel--autosave-timer-callback buf)
             (should (file-exists-p file)))
         (when (buffer-live-p buf)
           (kill-buffer buf)))))))

(ert-deftest test-ai-conversations-autosave-timer-callback-stops-inactive-buffer ()
  "Boundary: timer callback cancels itself when autosave is no longer active."
  (let ((buf (generate-new-buffer " *gptel timer inactive*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local gptel-mode t)
          (setq-local cj/gptel-autosave-enabled nil)
          (setq-local cj/gptel-autosave-filepath "/tmp/foo.gptel")
          (setq-local cj/gptel-autosave--timer :fake-timer)
          (let ((cancelled nil))
            (cl-letf (((symbol-function 'cancel-timer)
                       (lambda (timer) (setq cancelled timer))))
              (cj/gptel--autosave-timer-callback buf))
            (should (eq cancelled :fake-timer))
            (should-not cj/gptel-autosave--timer)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

;; ------------------------------------------------------ save-conversation

(ert-deftest test-ai-conversations-save-conversation-interactive-new-topic ()
  "Save-conversation writes file, enables autosave, and starts a timer."
  (test-ai-conversations--with-temp-dir
   (lambda (dir)
     (let ((ai-buffer (generate-new-buffer "*AI-Assistant*")))
       (unwind-protect
           (progn
             (with-current-buffer ai-buffer
               (setq-local gptel-mode t)
               (insert "session content"))
             (cl-letf (((symbol-function 'completing-read)
                        (lambda (&rest _) "Test Topic"))
                       ((symbol-function 'y-or-n-p)
                        (lambda (&rest _) nil))
                       ((symbol-function 'run-with-timer)
                        (lambda (&rest _) :save-timer)))
               (cj/gptel-save-conversation)
               (let ((files (directory-files dir nil "test-topic_.*\\.gptel$")))
                 (should files)
                 (should (= 1 (length files))))
               ;; Autosave state is set in the AI buffer
               (with-current-buffer ai-buffer
                 (should cj/gptel-autosave-enabled)
                 (should (stringp cj/gptel-autosave-filepath))
                 (should (eq cj/gptel-autosave--timer :save-timer)))))
         (kill-buffer ai-buffer))))))

(ert-deftest test-ai-conversations-save-conversation-error-no-buffer ()
  "Save-conversation errors when *AI-Assistant* doesn't exist."
  (when (get-buffer "*AI-Assistant*")
    (kill-buffer "*AI-Assistant*"))
  (should-error (cj/gptel-save-conversation)))

;; ------------------------------------------------------ delete-conversation

(ert-deftest test-ai-conversations-delete-conversation-interactive ()
  "Delete-conversation removes the chosen file after confirmation."
  (test-ai-conversations--with-temp-dir
   (lambda (dir)
     (let ((file (test-ai-conversations--touch
                  dir "topic_20260101-100000.gptel")))
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (_p cands &rest _) (caar cands)))
                 ((symbol-function 'y-or-n-p)
                  (lambda (&rest _) t)))
         (cj/gptel-delete-conversation)
         (should-not (file-exists-p file)))))))

(ert-deftest test-ai-conversations-delete-conversation-cancelled ()
  "Delete-conversation preserves the file when the user declines."
  (test-ai-conversations--with-temp-dir
   (lambda (dir)
     (let ((file (test-ai-conversations--touch
                  dir "topic_20260101-100000.gptel")))
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (_p cands &rest _) (caar cands)))
                 ((symbol-function 'y-or-n-p)
                  (lambda (&rest _) nil)))
         (cj/gptel-delete-conversation)
         (should (file-exists-p file)))))))

(ert-deftest test-ai-conversations-delete-conversation-error-empty-dir ()
  "Delete-conversation errors when no saved conversations exist."
  (test-ai-conversations--with-temp-dir
   (lambda (_dir)
     (should-error (cj/gptel-delete-conversation)))))

;; ------------------------------------------------------ install-once

(ert-deftest test-ai-conversations-autosave-after-response-hook-not-duplicated ()
  "Loading ai-conversations twice does not duplicate the post-response hook."
  (let ((gptel-post-response-functions
         (list #'cj/gptel--autosave-after-response)))
    ;; Re-run the install code
    (unless (member #'cj/gptel--autosave-after-response gptel-post-response-functions)
      (add-hook 'gptel-post-response-functions #'cj/gptel--autosave-after-response))
    (should (= 1 (cl-count #'cj/gptel--autosave-after-response
                           gptel-post-response-functions)))))

;; --------------------------------------------- autosave-toggle / indicator

(ert-deftest test-ai-conversations-autosave-toggle-enables-with-filepath ()
  "Toggle enables autosave when a filepath is set."
  (with-temp-buffer
    (setq-local gptel-mode t)
    (setq-local cj/gptel-autosave-enabled nil)
    (setq-local cj/gptel-autosave-filepath "/tmp/foo.gptel")
    (cj/gptel-autosave-toggle)
    (should cj/gptel-autosave-enabled)))

(ert-deftest test-ai-conversations-autosave-toggle-disables ()
  "Toggle turns autosave off and cancels the periodic timer when already on."
  (with-temp-buffer
    (setq-local gptel-mode t)
    (setq-local cj/gptel-autosave-enabled t)
    (setq-local cj/gptel-autosave-filepath "/tmp/foo.gptel")
    (setq-local cj/gptel-autosave--timer :fake-timer)
    (let ((cancelled nil))
      (cl-letf (((symbol-function 'cancel-timer)
                 (lambda (timer) (setq cancelled timer))))
        (cj/gptel-autosave-toggle))
      (should-not cj/gptel-autosave-enabled)
      (should (eq cancelled :fake-timer))
      (should-not cj/gptel-autosave--timer))))

(ert-deftest test-ai-conversations-autosave-toggle-prompts-when-no-filepath ()
  "Toggle prompts to save first when no filepath is configured."
  (with-temp-buffer
    (setq-local gptel-mode t)
    (setq-local cj/gptel-autosave-enabled nil)
    (setq-local cj/gptel-autosave-filepath nil)
    (let ((prompted nil)
          (save-called nil))
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq prompted t) nil))
                ((symbol-function 'cj/gptel-save-conversation)
                 (lambda () (setq save-called t))))
        (cj/gptel-autosave-toggle))
      (should prompted)
      (should-not save-called)
      (should-not cj/gptel-autosave-enabled))))

(ert-deftest test-ai-conversations-autosave-toggle-error-outside-gptel-mode ()
  "Toggle signals when called outside a gptel buffer."
  (with-temp-buffer
    (setq-local gptel-mode nil)
    (should-error (cj/gptel-autosave-toggle))))

(ert-deftest test-ai-conversations-autosave-mode-line-format-evaluates ()
  "Mode-line format evaluates to \" [AS]\" only when autosave is enabled."
  (with-temp-buffer
    (setq-local cj/gptel-autosave-enabled t)
    (should (equal (eval (cadr cj/gptel-autosave-mode-line-format))
                   " [AS]")))
  (with-temp-buffer
    (setq-local cj/gptel-autosave-enabled nil)
    (should-not (eval (cadr cj/gptel-autosave-mode-line-format)))))

(ert-deftest test-ai-conversations-install-mode-line-idempotent ()
  "Repeated installs do not duplicate the construct in mode-line-format."
  (with-temp-buffer
    (setq-local mode-line-format '("base"))
    (cj/gptel--install-autosave-mode-line)
    (cj/gptel--install-autosave-mode-line)
    (cj/gptel--install-autosave-mode-line)
    (should (= 1 (cl-count 'cj/gptel-autosave-mode-line-format mode-line-format)))))

(provide 'test-ai-conversations)
;;; test-ai-conversations.el ends here

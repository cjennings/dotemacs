;;; test-ai-term--pick-project.el --- Tests for cj/--ai-term-pick-project -*- lexical-binding: t; -*-

;;; Commentary:
;; The picker presents abbreviated paths to `completing-read' (projects
;; with a live tmux session first, then alphabetical), then returns the
;; absolute path corresponding to the user's choice.  An empty candidate
;; set raises a `user-error' rather than offering an empty prompt.  The
;; collection is a completion table that pins display order (so Vertico
;; doesn't re-sort and defeat the active-first grouping).

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-term)

(defun test-ai-term--collection-strings (collection)
  "Return the candidate display strings from a completing-read COLLECTION.
Works whether COLLECTION is an alist or a completion-table function."
  (all-completions "" collection))

(ert-deftest test-ai-term--pick-project-returns-absolute-path-of-choice ()
  "Normal: user picks a candidate, picker returns its absolute path."
  (cl-letf (((symbol-function 'cj/--ai-term-candidates)
             (lambda () '("/home/u/code/foo" "/home/u/code/bar")))
            ((symbol-function 'cj/--ai-term-live-tmux-sessions)
             (lambda () nil))
            ((symbol-function 'completing-read)
             (lambda (_p collection &rest _)
               (seq-find (lambda (s) (string-match-p "bar" s))
                         (test-ai-term--collection-strings collection)))))
    (should (equal (cj/--ai-term-pick-project) "/home/u/code/bar"))))

(ert-deftest test-ai-term--pick-project-empty-candidates-raises-user-error ()
  "Error: no candidates -> user-error rather than empty prompt."
  (cl-letf (((symbol-function 'cj/--ai-term-candidates) (lambda () nil)))
    (should-error (cj/--ai-term-pick-project) :type 'user-error)))

(ert-deftest test-ai-term--pick-project-presents-abbreviated-paths ()
  "Normal: the completing-read collection holds abbreviated display forms."
  (let (received-strings)
    (cl-letf (((symbol-function 'cj/--ai-term-candidates)
               (lambda () (list (expand-file-name "~/code/foo"))))
              ((symbol-function 'cj/--ai-term-live-tmux-sessions)
               (lambda () nil))
              ((symbol-function 'completing-read)
               (lambda (_p collection &rest _)
                 (setq received-strings (test-ai-term--collection-strings collection))
                 (car received-strings))))
      (cj/--ai-term-pick-project)
      (should (equal (car received-strings) "~/code/foo")))))

(ert-deftest test-ai-term--pick-project-active-sessions-sort-first ()
  "Normal: a project with a live tmux session leads; it carries [detached]."
  (let ((cj/ai-term-tmux-session-prefix "aiv-")
        received-strings)
    (cl-letf (((symbol-function 'cj/--ai-term-candidates)
               (lambda () '("/c/foo" "/c/bar" "/c/baz")))
              ((symbol-function 'cj/--ai-term-live-tmux-sessions)
               (lambda () '("aiv-baz")))
              ((symbol-function 'completing-read)
               (lambda (_p collection &rest _)
                 (setq received-strings (test-ai-term--collection-strings collection))
                 (car received-strings))))
      (cj/--ai-term-pick-project)
      (should (equal received-strings
                     '("/c/baz [detached]" "/c/bar" "/c/foo"))))))

(ert-deftest test-ai-term--format-candidate-flags-running-project ()
  "Normal: a path whose agent buffer has a live process gets a [running] suffix."
  (let* ((path (expand-file-name "~/code/already-running"))
         (buffer-name (cj/--ai-term-buffer-name path))
         (buf (get-buffer-create buffer-name)))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--ai-term-process-live-p)
                   (lambda (b) (eq b buf))))
          (should (equal (cj/--ai-term-format-candidate path)
                         (format "%s [running]" (abbreviate-file-name path)))))
      (kill-buffer buf))))

(ert-deftest test-ai-term--format-candidate-flags-detached-session ()
  "Normal: no buffer but a matching tmux session -> [detached] suffix."
  (let* ((cj/ai-term-tmux-session-prefix "aiv-")
         (path (expand-file-name "~/code/has-session"))
         (bn (cj/--ai-term-buffer-name path)))
    (when (get-buffer bn) (kill-buffer bn))
    (should (equal (cj/--ai-term-format-candidate
                    path (list (cj/--ai-term-tmux-session-name path)))
                   (format "%s [detached]" (abbreviate-file-name path))))))

(ert-deftest test-ai-term--format-candidate-running-beats-detached ()
  "Boundary: a live buffer wins over a matching session -> [running], not [detached]."
  (let* ((cj/ai-term-tmux-session-prefix "aiv-")
         (path (expand-file-name "~/code/both"))
         (bn (cj/--ai-term-buffer-name path))
         (buf (get-buffer-create bn)))
    (unwind-protect
        (cl-letf (((symbol-function 'cj/--ai-term-process-live-p)
                   (lambda (b) (eq b buf))))
          (should (equal (cj/--ai-term-format-candidate
                          path (list (cj/--ai-term-tmux-session-name path)))
                         (format "%s [running]" (abbreviate-file-name path)))))
      (kill-buffer buf))))

(ert-deftest test-ai-term--format-candidate-omits-flag-when-not-running ()
  "Boundary: a path with no buffer or no live process -> plain abbreviated path."
  (let ((path (expand-file-name "~/code/not-running")))
    ;; Make sure no agent buffer exists for this path.
    (let ((bn (cj/--ai-term-buffer-name path)))
      (when (get-buffer bn) (kill-buffer bn)))
    (should (equal (cj/--ai-term-format-candidate path)
                   (abbreviate-file-name path)))))

(provide 'test-ai-term--pick-project)
;;; test-ai-term--pick-project.el ends here

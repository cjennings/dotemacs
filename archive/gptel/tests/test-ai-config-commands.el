;;; test-ai-config-commands.el --- Tests for ai-config interactive commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Sibling tests cover the pure helpers (model-to-string, build-model-list,
;; current-model-selection, fresh-org-prefix, backend-and-model).  This
;; file covers the user-facing wrappers:
;;
;;   cj/gptel--available-backends
;;   cj/gptel-change-model
;;   cj/gptel-add-file
;;   cj/gptel-add-this-buffer
;;   cj/toggle-gptel
;;   cj/gptel-context-clear

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-config)

;; Top-level defvars so let-bindings reach the dynamic binding under
;; lexical scope.
(defvar gptel-backend nil)
(defvar gptel-model nil)
(defvar gptel-claude-backend nil)
(defvar gptel-chatgpt-backend nil)
(defvar gptel-context--alist nil)

;;; cj/gptel--available-backends

(ert-deftest test-ai-available-backends-returns-claude-and-chatgpt ()
  "Normal: both backends present become alist entries."
  (let ((gptel-claude-backend 'claude-obj)
        (gptel-chatgpt-backend 'chatgpt-obj))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'cj/ensure-gptel-backends) #'ignore))
      (let ((result (cj/gptel--available-backends)))
        (should (equal (assoc "Anthropic - Claude" result)
                       '("Anthropic - Claude" . claude-obj)))
        (should (equal (assoc "OpenAI - ChatGPT" result)
                       '("OpenAI - ChatGPT" . chatgpt-obj)))))))

(ert-deftest test-ai-available-backends-skips-nil-entries ()
  "Boundary: only configured backends appear in the alist."
  (let ((gptel-claude-backend nil)
        (gptel-chatgpt-backend 'chatgpt-only))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'cj/ensure-gptel-backends) #'ignore))
      (let ((result (cj/gptel--available-backends)))
        (should-not (assoc "Anthropic - Claude" result))
        (should (assoc "OpenAI - ChatGPT" result))))))

;;; cj/gptel-change-model

(ert-deftest test-ai-change-model-global-sets-globals-and-messages ()
  "Normal: choosing 'global' sets `gptel-backend' and `gptel-model'
globally and reports via `message'."
  (let ((gptel-backend 'old-backend)
        (gptel-model 'old-model)
        (gptel-claude-backend 'claude-obj)
        (gptel-chatgpt-backend nil)
        msg)
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'cj/ensure-gptel-backends) #'ignore)
              ((symbol-function 'gptel-backend-models)
               (lambda (_) '("claude-opus-4-7")))
              ((symbol-function 'completing-read)
               (lambda (prompt &rest _)
                 (if (string-prefix-p "Set model for" prompt)
                     "global"
                   "Anthropic - Claude: claude-opus-4-7")))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (cj/gptel-change-model))
    (should (eq gptel-backend 'claude-obj))
    (should (eq gptel-model 'claude-opus-4-7))
    (should (string-match-p "global" msg))))

;;; cj/gptel-add-file

(ert-deftest test-ai-add-file-outside-projectile-uses-read-file-name ()
  "Normal: without projectile, add-file routes through read-file-name."
  (let* ((target (make-temp-file "cj-ai-add-file-" nil ".org"))
         added)
    (unwind-protect
        (cl-letf (((symbol-function 'featurep)
                   (lambda (sym &rest _) (not (eq sym 'projectile))))
                  ((symbol-function 'read-file-name)
                   (lambda (&rest _) target))
                  ((symbol-function 'gptel-add-file)
                   (lambda (f) (setq added f)))
                  ((symbol-function 'message) #'ignore))
          (cj/gptel-add-file))
      (delete-file target))
    (should (equal added target))))

;;; cj/gptel-add-this-buffer

(ert-deftest test-ai-add-this-buffer-calls-gptel-add-with-prefix ()
  "Normal: add-this-buffer calls `gptel-add' with the prefix-arg form."
  (let (gptel-add-args msg)
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'gptel-add)
               (lambda (&rest args) (setq gptel-add-args args)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (cj/gptel-add-this-buffer))
    (should (equal gptel-add-args '((4))))
    (should (string-match-p "to GPTel context" msg))))

;;; cj/toggle-gptel

(ert-deftest test-ai-toggle-gptel-hides-when-visible ()
  "Normal: when the AI buffer is showing in a window, toggle hides it."
  (let ((buffer (get-buffer-create "*AI-Assistant*"))
        deleted-window)
    (unwind-protect
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (&rest _) 'fake-window))
                  ((symbol-function 'delete-window)
                   (lambda (w) (setq deleted-window w))))
          (cj/toggle-gptel))
      (kill-buffer buffer))
    (should (eq deleted-window 'fake-window))))

;;; cj/gptel-context-clear

(ert-deftest test-ai-context-clear-uses-remove-all-when-available ()
  "Normal: with `gptel-context-remove-all' present, it is called."
  (let (called msg)
    (cl-letf (((symbol-function 'gptel-context-remove-all)
               (lambda () (setq called t)))
              ((symbol-function 'call-interactively)
               (lambda (fn &rest _) (funcall fn)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (cj/gptel-context-clear))
    (should called)
    (should (string-match-p "cleared" msg))))

(ert-deftest test-ai-context-clear-resets-alist-as-fallback ()
  "Boundary: when no clear function exists but the alist does, it gets
nilled directly."
  (let ((gptel-context--alist '("item1" "item2"))
        msg)
    ;; Make sure the fboundp branches are skipped.
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (not (memq sym '(gptel-context-remove-all
                                  gptel-context-clear)))))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (cj/gptel-context-clear))
    (should-not gptel-context--alist)
    (should (string-match-p "cleared" msg))))

(provide 'test-ai-config-commands)
;;; test-ai-config-commands.el ends here

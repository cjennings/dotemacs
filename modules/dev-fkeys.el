;;; dev-fkeys.el --- Developer F-key dispatchers -*- lexical-binding: t -*-

;;; Commentary:
;; Project-aware F-key block for developer workflows:
;;
;;   F4    completing-read of compile/run candidates filtered by project type
;;   C-F4  fast path: compile only (no-op on interpreted projects)
;;   M-F4  fast path: clean + rebuild (no-op on interpreted projects)
;;   S-F4  recompile (built-in)
;;   F6    project tests (Phase 1 stopgap; Phase 2 will replace with the
;;         polyglot test runner spec'd in todo.org)
;;
;; Project-type detection runs against the projectile root and falls back to
;; 'unknown when no marker matches. Interpreted markers are checked before
;; compiled markers, so a Python or Node project that also has a Makefile
;; for tasks classifies as interpreted.
;;
;; F7 (coverage) is wired in coverage-core.el. F5 is reserved for the debug
;; ticket and intentionally left unbound here.

;;; Code:

(require 'cl-lib)

(declare-function projectile-compile-project "projectile" (arg))
(declare-function projectile-run-project "projectile" (arg))
(declare-function projectile-project-root "projectile" (&optional dir))
(declare-function projectile-test-project "projectile" (arg))
(declare-function recompile "compile" (&optional edit-command))

;; ---------- Project root ----------

(defun cj/--f4-project-root ()
  "Return projectile project root, or nil. Never errors.
Some projectile configurations signal an error when called outside a
known project; this wrapper degrades to nil so the F4 dispatcher routes
to the \\='unknown branch instead of crashing."
  (when (fboundp 'projectile-project-root)
    (condition-case nil
        (projectile-project-root)
      (error nil))))

;; ---------- Project-type detection ----------

(defconst cj/--f4-interpreted-markers
  '("pyproject.toml" "requirements.txt" "Pipfile" "package.json")
  "Markers that classify a project as interpreted (Run-only menu).")

(defconst cj/--f4-compiled-markers
  '("go.mod" "Cargo.toml" "CMakeLists.txt" "Makefile" "Eask")
  "Markers that classify a project as compiled (full Compile/Run/Clean menu).")

(defun cj/--f4-any-marker-p (root markers)
  "Return non-nil if any of MARKERS exists at ROOT."
  (cl-some (lambda (m) (file-exists-p (expand-file-name m root))) markers))

(defun cj/--detect-project-type (root)
  "Classify project at ROOT as \\='compiled, \\='interpreted, or \\='unknown.
Interpreted markers are checked before compiled markers, so a Python or
Node project that also has a Makefile for tasks classifies as interpreted.
Returns \\='unknown when ROOT is nil or no marker matches."
  (cond
   ((not root) 'unknown)
   ((cj/--f4-any-marker-p root cj/--f4-interpreted-markers) 'interpreted)
   ((cj/--f4-any-marker-p root cj/--f4-compiled-markers) 'compiled)
   (t 'unknown)))

;; ---------- Clean command derivation ----------

(defun cj/--f4-derive-clean-cmd (root)
  "Pick a clean shell command for the project at ROOT, or nil.
First marker matched wins. ROOT may be nil; nil and a path with no
recognized markers both return nil."
  (when root
    (cond
     ((file-exists-p (expand-file-name "go.mod" root))         "go clean ./...")
     ((file-exists-p (expand-file-name "Cargo.toml" root))     "cargo clean")
     ((file-exists-p (expand-file-name "Eask" root))           "eask clean")
     ((file-exists-p (expand-file-name "Makefile" root))       "make clean")
     ((file-exists-p (expand-file-name "CMakeLists.txt" root)) "cmake --build build --target clean"))))

;; ---------- Action handlers ----------

(defun cj/--f4-compile-and-run-impl ()
  "Run `projectile-compile-project', then `projectile-run-project' on success.
Installs a one-shot `compilation-finish-functions' hook to chain the run."
  (add-hook 'compilation-finish-functions
            (cj/--f4-make-once-hook
             (lambda () (projectile-run-project nil))))
  (projectile-compile-project nil))

(defun cj/--f4-dispatch (action)
  "Route ACTION (a symbol from `cj/--f4-candidates') to its handler.
Signals `user-error' on an unrecognized symbol or nil."
  (pcase action
    ('compile-only    (projectile-compile-project nil))
    ('run-only        (projectile-run-project nil))
    ('compile-and-run (cj/--f4-compile-and-run-impl))
    ('clean-rebuild   (cj/--f4-clean-rebuild-impl (cj/--f4-project-root)))
    ('compile-plain   (call-interactively #'compile))
    (_ (user-error "Unknown F4 action: %s" action))))

(defun cj/--f4-clean-rebuild-impl (root)
  "Run the heuristic clean for project at ROOT, then rebuild on success.
Signals `user-error' when no clean command can be derived. The rebuild
side reuses `projectile-compile-project' so the per-project compile
command (prompted-and-cached by projectile) drives the build."
  (let ((clean-cmd (cj/--f4-derive-clean-cmd root)))
    (unless clean-cmd
      (user-error "Clean + Rebuild: no clean command for this project type"))
    (add-hook 'compilation-finish-functions
              (cj/--f4-make-once-hook
               (lambda () (projectile-compile-project nil))))
    (let ((default-directory root))
      (compile clean-cmd))))

;; ---------- One-shot compilation-finish hook ----------

(defun cj/--f4-make-once-hook (then-fn)
  "Build a one-shot `compilation-finish-functions' hook that chains THEN-FN.
The returned lambda removes itself from `compilation-finish-functions' on
first invocation regardless of status, then calls THEN-FN only if the
status string starts with \"finished\" (the convention used by compile.el
for a successful compile)."
  (let (hook)
    (setq hook
          (lambda (_buf status)
            (remove-hook 'compilation-finish-functions hook)
            (when (and (stringp status)
                       (string-prefix-p "finished" status))
              (funcall then-fn))))
    hook))

;; ---------- Candidate menus ----------

(defun cj/--f4-candidates (project-type)
  "Return alist of (LABEL . ACTION) for the F4 menu given PROJECT-TYPE.
The first entry is the default, selected on RET in completing-read.
Compiled projects get the full menu, interpreted projects get Run only,
anything else (including nil and unrecognized symbols) falls through to
a single Compile entry that calls plain `compile'."
  (pcase project-type
    ('compiled    '(("Compile + Run"   . compile-and-run)
                    ("Compile"         . compile-only)
                    ("Run"             . run-only)
                    ("Clean + Rebuild" . clean-rebuild)))
    ('interpreted '(("Run"             . run-only)))
    (_            '(("Compile"         . compile-plain)))))

;; ---------- Interactive wrappers ----------

(defun cj/f4-compile-and-run ()
  "Project-aware F4 dispatcher.
Prompts via `completing-read' with a candidate set filtered by project
type (compiled / interpreted / unknown), then dispatches the chosen
label's action."
  (interactive)
  (let* ((root (cj/--f4-project-root))
         (project-type (cj/--detect-project-type root))
         (candidates (cj/--f4-candidates project-type))
         (default (caar candidates))
         (label (completing-read
                 (format "F4 (%s): " project-type)
                 (mapcar #'car candidates) nil t nil nil default))
         (action (cdr (assoc label candidates))))
    (cj/--f4-dispatch action)))

(defun cj/f4-compile-only ()
  "C-F4 fast path: compile only.
Compiled projects run `projectile-compile-project'. Interpreted projects
get a no-op message. Outside any project, falls back to interactive
`compile'."
  (interactive)
  (let* ((root (cj/--f4-project-root))
         (project-type (cj/--detect-project-type root)))
    (pcase project-type
      ('compiled    (projectile-compile-project nil))
      ('interpreted (message "C-F4: not a compiled language"))
      (_            (call-interactively #'compile)))))

(defun cj/f4-clean-rebuild ()
  "M-F4 fast path: clean + rebuild.
Compiled projects run the heuristic clean + projectile-compile-project
chain. Interpreted projects and unrecognized projects get a no-op
message."
  (interactive)
  (let* ((root (cj/--f4-project-root))
         (project-type (cj/--detect-project-type root)))
    (pcase project-type
      ('compiled    (cj/--f4-clean-rebuild-impl root))
      ('interpreted (message "M-F4: not a compiled language"))
      (_            (message "M-F4: no project detected")))))

;; ---------- Bindings ----------

(keymap-global-set "<f4>"   #'cj/f4-compile-and-run)
(keymap-global-set "C-<f4>" #'cj/f4-compile-only)
(keymap-global-set "M-<f4>" #'cj/f4-clean-rebuild)
(keymap-global-set "S-<f4>" #'recompile)
;; Phase 1 stopgap. Phase 2 replaces this with the polyglot test runner
;; spec'd in todo.org. Without a global F6 binding here, the per-language
;; F6→format bindings in prog-c/python/shell would have nothing to fall
;; back on after this commit drops them, leaving F6 on its useless Emacs
;; default (`2C-command').
(keymap-global-set "<f6>" #'projectile-test-project)

(provide 'dev-fkeys)
;;; dev-fkeys.el ends here.

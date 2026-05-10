;;; dev-fkeys.el --- Developer F-key dispatchers -*- lexical-binding: t -*-

;;; Commentary:
;; Project-aware F-key block for developer workflows:
;;
;;   F4    completing-read of compile/run candidates filtered by project type
;;   C-F4  fast path: compile only (no-op on interpreted projects)
;;   M-F4  fast path: clean + rebuild (no-op on interpreted projects)
;;   S-F4  recompile (built-in)
;;   F6    completing-read of test candidates: All tests / Current file's tests
;;   C-F6  fast path: current file's tests
;;
;; F4 project-type detection runs against the projectile root and falls back
;; to \\='unknown when no marker matches. Interpreted markers are checked
;; before compiled markers, so a Python or Node project that also has a
;; Makefile for tasks classifies as interpreted.
;;
;; F6 \"All tests\" delegates to `projectile-test-project'. F6 \"Current
;; file's tests\" detects the language by extension, derives the runner
;; command (elisp via the project Makefile, Python via pytest, Go via the
;; package), and pipes through `compile' from the projectile root.
;; TypeScript / JavaScript are detected but punted for v1 — the function
;; signals a user-error rather than guessing a runner.
;;
;; M-F6 is reserved for Phase 2b (\"Run a test...\" menu entry with
;; per-language test-name discovery). Phase 2b also adds buffer-local
;; last-test memory and tree-sitter-based discovery for Python / Go /
;; TypeScript. The tree-sitter discovery uses a capture-then-filter pattern
;; (queries without `:match' / `:equal' / `:pred' predicates, with the
;; pattern filter applied in Elisp) to sidestep Emacs bug #79687 — Emacs
;; 30.2 emits unsuffixed `#match' predicates that libtree-sitter 0.26
;; rejects. The fix lives on Emacs master (commit b0143530) and is
;; targeted at Emacs 31; it has not been backported to the emacs-30
;; branch as of 2026-05-03. See Mike Olson's writeup at
;; https://mwolson.org/blog/emacs/2026-04-20-fixing-typescript-ts-mode-in-emacs-30-2/
;; for the same workaround applied to font-lock.
;;
;; F7 (coverage) is wired in coverage-core.el. F5 is reserved for the debug
;; ticket and intentionally left unbound here.

;;; Code:

(require 'cl-lib)
(require 'system-lib)

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
    ('compile-only    (projectile-compile-project current-prefix-arg))
    ('run-only        (projectile-run-project current-prefix-arg))
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

;; ---------- Projectile cache revert on failure ----------
;;
;; Without this, a one-off typo at projectile's prompt poisons the per-
;; project cmd cache: every subsequent invocation pre-fills the broken
;; value. The capture/finish-hook pair installed by `:around' advice on
;; the three projectile cmd-runners reverts the cache to its prior value
;; if the compile fails AND the cmd was modified. A test that fails
;; because of a real code bug (cmd unchanged) leaves the cache alone.
;; Revert state is captured before Projectile runs, then closed over by a
;; buffer-local compilation finish hook so overlapping compiles cannot
;; overwrite or consume one another's revert metadata.

(defun cj/--projectile-capture-cmd (map-symbol)
  "Capture the cached cmd at the project root in MAP-SYMBOL.
MAP-SYMBOL is the symbol of a projectile cmd-map (e.g.
`projectile-compile-cmd-map'). Return nil when the project root cannot
be resolved or MAP-SYMBOL is unbound (projectile not loaded)."
  (let ((root (cj/--f4-project-root)))
    (when (and root (boundp map-symbol))
      (let ((prior (gethash root (symbol-value map-symbol))))
        (list :map map-symbol :root root :prior prior)))))

(defun cj/--projectile-revert-state-on-fail (state status)
  "Apply projectile cache revert STATE when STATUS is a failed compile.
Reverts the cmd-map entry only when the compile failed AND the cmd was
modified from the captured prior value AND that prior was non-nil. The
unchanged-and-failed case (test fails because of a real bug) leaves the
cache alone."
  (when (and state (stringp status)
             (not (string-prefix-p "finished" status)))
    (let* ((map     (plist-get state :map))
           (root    (plist-get state :root))
           (prior   (plist-get state :prior))
           (current (and (boundp map) (gethash root (symbol-value map)))))
      (when (and root prior (boundp map)
                 (not (equal prior current)))
        (puthash root prior (symbol-value map))))))

(defun cj/--projectile-make-revert-on-fail-hook (state)
  "Return a one-shot buffer-local finish hook for projectile revert STATE."
  (let (hook)
    (setq hook
          (lambda (buf status)
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (remove-hook 'compilation-finish-functions hook t)))
            (cj/--projectile-revert-state-on-fail state status)))
    hook))

(defun cj/--projectile-compilation-buffer (result)
  "Return the compilation buffer represented by RESULT, or nil."
  (cond
   ((bufferp result) result)
   ((processp result) (process-buffer result))
   (t nil)))

(defun cj/--projectile-around-revert (map-symbol orig-fn &rest args)
  "Around-advice for projectile cmd-runners.
MAP-SYMBOL identifies which cmd-map to capture (compile / test / run).
Captures the prior cached cmd, invokes ORIG-FN with ARGS, then installs a
one-shot buffer-local revert-on-failure hook on the returned compilation
buffer when possible."
  (let* ((state (cj/--projectile-capture-cmd map-symbol))
         (result (apply orig-fn args))
         (buffer (cj/--projectile-compilation-buffer result)))
    (when (and state (buffer-live-p buffer))
      (with-current-buffer buffer
        (add-hook 'compilation-finish-functions
                  (cj/--projectile-make-revert-on-fail-hook state)
                  nil t)))
    result))

(defun cj/projectile-reset-cmds ()
  "Clear projectile's cached compile/test/run cmds for the current project.
Use when projectile's auto-detected default was wrong to begin with and
you want to start fresh — the next F4 / F6 invocation will re-derive
projectile's project-type default."
  (interactive)
  (let ((root (cj/--f4-project-root)))
    (unless root
      (user-error "F-keys: no project detected"))
    (dolist (map '(projectile-compile-cmd-map
                   projectile-test-cmd-map
                   projectile-run-cmd-map))
      (when (boundp map)
        (remhash root (symbol-value map))))
    (message "Cleared projectile compile/test/run cache for %s" root)))

;; ---------- F6 language detection ----------

(defconst cj/--f6-extension-language-map
  '(("el"   . elisp)
    ("py"   . python)
    ("go"   . go)
    ("ts"   . typescript)
    ("tsx"  . typescript)
    ("js"   . javascript)
    ("jsx"  . javascript))
  "Map of file-extension string to language symbol.
Used by `cj/--f6-language-detect'.")

(defun cj/--f6-language-detect (filename)
  "Classify FILENAME's language by extension.
Returns one of \\='elisp, \\='python, \\='go, \\='typescript, \\='javascript,
or \\='unknown. Match is case-insensitive. Nil or extensionless input
returns \\='unknown."
  (if (or (null filename) (string-empty-p filename))
      'unknown
    (let ((ext (file-name-extension filename)))
      (or (and ext
               (cdr (assoc (downcase ext) cj/--f6-extension-language-map)))
          'unknown))))

;; ---------- F6 test-file detection ----------

(defun cj/--f6-buffer-is-test-file-p (filename)
  "Return non-nil if FILENAME's basename matches a test-file naming convention.
Per language: elisp basenames start with `test-'; Python basenames start
with `test_' or end with `_test.py'; Go basenames end with `_test.go';
TypeScript / JavaScript basenames contain `.test.' or `.spec.'. Files
whose extension we don't classify return nil even if their name happens
to match a generic pattern."
  (when (and filename (not (string-empty-p filename)))
    (let ((language (cj/--f6-language-detect filename))
          (base (file-name-nondirectory filename)))
      (pcase language
        ('elisp      (string-prefix-p "test-" base))
        ('python     (or (string-prefix-p "test_" base)
                         (string-suffix-p "_test.py" base)))
        ('go         (string-suffix-p "_test.go" base))
        ((or 'typescript 'javascript)
                     (or (string-match-p "\\.test\\." base)
                         (string-match-p "\\.spec\\." base)))
        (_           nil)))))

;; ---------- F6 source-stem extraction ----------

(defun cj/--f6-source-stem (filename)
  "Return the source-module stem from FILENAME, or nil for nil/empty input.
Strips directory, extension, and any test-pattern prefix or suffix.
For elisp test files like `test-foo--bar.el', drops everything from
`--' onward so the result is the source module name (`foo'). Unsupported
languages fall back to the basename without extension."
  (when (and filename (not (string-empty-p filename)))
    (let ((language (cj/--f6-language-detect filename))
          (base (file-name-base filename)))
      (pcase language
        ('elisp
         (let ((stripped (if (string-prefix-p "test-" base)
                             (substring base 5)
                           base)))
           (if (string-match "--" stripped)
               (substring stripped 0 (match-beginning 0))
             stripped)))
        ('python
         (cond
          ((string-prefix-p "test_" base) (substring base 5))
          ((string-suffix-p "_test" base) (substring base 0 -5))
          (t base)))
        ('go
         (if (string-suffix-p "_test" base)
             (substring base 0 -5)
           base))
        (_ base)))))

;; ---------- F6 test-runner command builder ----------

(defun cj/--f6-test-runner-cmd-for (language is-test-file rel-path stem rel-dir)
  "Return shell command to run tests for the given primitives, or nil.
LANGUAGE is the language symbol; IS-TEST-FILE is non-nil when the file
itself is a test file; REL-PATH and REL-DIR are the path and directory
relative to project root; STEM is the source-module stem.

For elisp test files the command runs only that file via
`make test-file FILE='. For elisp source files it picks up the matching
tests by name regex via `make test-name TEST=^test-<stem>-'. Python
source files map to `pytest tests/test_<stem>.py'; Python test files run
the file directly. Go runs the package containing the file.
TypeScript / JavaScript and unknown languages return nil."
  (pcase language
    ('elisp
     (if is-test-file
         ;; The project Makefile prepends `tests/' to FILE, so pass the
         ;; basename only — passing the rel-path produces `tests/tests/...'.
         (format "make test-file FILE=%s"
                 (cj/shell-quote-argument-readable
                  (file-name-nondirectory rel-path)))
       (format "make test-name TEST=%s"
               (cj/shell-quote-argument-readable
                (format "^test-%s-" stem)))))
    ('python
     (if is-test-file
         (format "pytest %s" (cj/shell-quote-argument-readable rel-path))
       (format "pytest %s"
               (cj/shell-quote-argument-readable
                (format "tests/test_%s.py" stem)))))
    ('go
     (format "go test %s"
             (cj/shell-quote-argument-readable
              (if (string-empty-p rel-dir)
                  "./"
                (format "./%s" rel-dir)))))
    (_ nil)))

;; ---------- F6 current-file orchestrator ----------

(defun cj/--f6-current-file-tests-impl (file project-root)
  "Run the tests for FILE within PROJECT-ROOT.
Detects language, derives the runner command via
`cj/--f6-test-runner-cmd-for', and invokes `compile' with
`default-directory' bound to PROJECT-ROOT. Signals `user-error' for nil
inputs or files in languages without a runner."
  (unless file
    (user-error "F6: no file backing this buffer"))
  (unless project-root
    (user-error "F6: no project detected"))
  (let* ((language (cj/--f6-language-detect file))
         (rel-path (file-relative-name file project-root))
         (rel-dir  (file-relative-name (file-name-directory file) project-root))
         (rel-dir  (cond
                    ((string= rel-dir "./") "")
                    ((string-suffix-p "/" rel-dir)
                     (substring rel-dir 0 -1))
                    (t rel-dir)))
         (stem     (cj/--f6-source-stem file))
         (is-test  (cj/--f6-buffer-is-test-file-p file))
         (cmd      (cj/--f6-test-runner-cmd-for
                    language is-test rel-path stem rel-dir)))
    (unless cmd
      (user-error "F6: no test runner for %s files" language))
    (let ((default-directory project-root))
      (compile cmd))))

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
      ('compiled    (projectile-compile-project current-prefix-arg))
      ('interpreted (message "C-F4: not a compiled language"))
      (_            (call-interactively #'compile)))))

(defun cj/f6-test-runner ()
  "F6 top-level test menu.
Prompts via `completing-read' between \"All tests\" (delegates to
`projectile-test-project') and \"Current file's tests\" (delegates to
`cj/--f6-current-file-tests-impl')."
  (interactive)
  (let* ((candidates '("All tests" "Current file's tests"))
         (label (completing-read "F6: " candidates nil t nil nil (car candidates))))
    (pcase label
      ("All tests" (projectile-test-project current-prefix-arg))
      ("Current file's tests"
       (cj/--f6-current-file-tests-impl
        (buffer-file-name) (cj/--f4-project-root))))))

(defun cj/f6-current-file-tests ()
  "C-F6 fast path: run tests for the current buffer's file.
Resolves `buffer-file-name' and projectile root, then delegates to
`cj/--f6-current-file-tests-impl'."
  (interactive)
  (cj/--f6-current-file-tests-impl
   (buffer-file-name) (cj/--f4-project-root)))

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

;; ---------- Projectile advice ----------

(defun cj/--projectile-compile-around-revert (orig-fn &rest args)
  "Around advice for `projectile-compile-project' command-cache revert."
  (apply #'cj/--projectile-around-revert
         'projectile-compile-cmd-map orig-fn args))

(defun cj/--projectile-test-around-revert (orig-fn &rest args)
  "Around advice for `projectile-test-project' command-cache revert."
  (apply #'cj/--projectile-around-revert
         'projectile-test-cmd-map orig-fn args))

(defun cj/--projectile-run-around-revert (orig-fn &rest args)
  "Around advice for `projectile-run-project' command-cache revert."
  (apply #'cj/--projectile-around-revert
         'projectile-run-cmd-map orig-fn args))

(defconst cj/--projectile-revert-advice-specs
  '((projectile-compile-project . cj/--projectile-compile-around-revert)
    (projectile-test-project    . cj/--projectile-test-around-revert)
    (projectile-run-project     . cj/--projectile-run-around-revert))
  "Projectile command runners and their command-cache revert advice.")

(defun cj/--projectile-install-revert-advice ()
  "Install Projectile command-cache revert advice when Projectile is available."
  (dolist (spec cj/--projectile-revert-advice-specs)
    (let ((target (car spec))
          (advice (cdr spec)))
      (when (and (fboundp target)
                 (not (advice-member-p advice target)))
        (advice-add target :around advice)))))

(defun cj/--projectile-register-revert-advice ()
  "Install Projectile revert advice now, or after Projectile loads."
  (if (featurep 'projectile)
      (cj/--projectile-install-revert-advice)
    (eval-after-load 'projectile
      (list 'cj/--projectile-install-revert-advice))))

(cj/--projectile-register-revert-advice)

;; ---------- Bindings ----------

(eval-when-compile (defvar cj/custom-keymap)) ;; defined in keybindings.el

;; Skip the binding if cj/custom-keymap isn't loaded yet (e.g. when this
;; module is required directly in batch tests).
(when (boundp 'cj/custom-keymap)
  (keymap-set cj/custom-keymap "P" #'cj/projectile-reset-cmds))

(keymap-global-set "<f4>"   #'cj/f4-compile-and-run)
(keymap-global-set "C-<f4>" #'cj/f4-compile-only)
(keymap-global-set "M-<f4>" #'cj/f4-clean-rebuild)
(keymap-global-set "S-<f4>" #'recompile)
(keymap-global-set "<f6>"   #'cj/f6-test-runner)
(keymap-global-set "C-<f6>" #'cj/f6-current-file-tests)
;; M-<f6> reserved for Phase 2b ("Run a test..." with last-test memory).

(provide 'dev-fkeys)
;;; dev-fkeys.el ends here.

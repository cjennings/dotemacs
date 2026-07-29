;;; test-auto-dim-config.el --- Tests for the auto-dim-other-buffers config -*- lexical-binding: t; -*-

;;; Commentary:
;; auto-dim-config configures the local auto-dim-other-buffers fork: dim only
;; non-selected windows within Emacs (not the whole frame on focus-out), drop
;; fringe from the dimmed faces to avoid flicker on this non-pgtk build, and
;; enable the global mode.  Guarded with `skip-unless' because the fork lives
;; in ~/code and may be absent on a clean checkout.
;;
;; The vterm dim-integration tests were removed when the terminal engine moved
;; off vterm.  EAT (the current engine) renders in real Emacs faces and uses the
;; `default' face for its background, so terminal buffers dim like any other
;; buffer with no dedicated integration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defconst test-auto-dim--fork
  (expand-file-name "~/code/auto-dim-other-buffers.el")
  "Local fork directory the module loads via `:load-path'.")

(ert-deftest test-auto-dim-config-applies-settings ()
  "Normal: loading the module enables the mode with the chosen settings."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (unwind-protect
      (progn
        (should (bound-and-true-p auto-dim-other-buffers-mode))
        (should (null auto-dim-other-buffers-dim-on-focus-out))
        ;; Config intent only: this asserts the value the module just set, so it
        ;; cannot fail even if the fork inverts what the flag MEANS.  The two
        ;; behavioral tests below are what actually pin the behavior.
        (should (null auto-dim-other-buffers-dim-on-switch-to-minibuffer))
        (should-not (assq 'fringe auto-dim-other-buffers-affected-faces)))
    (when (fboundp 'auto-dim-other-buffers-mode)
      (auto-dim-other-buffers-mode -1))))

(defmacro test-auto-dim--with-two-windows (win-a win-b &rest body)
  "Bind WIN-A and WIN-B to two live windows with the mode on, then run BODY.
Restores the window configuration, the buffers, and the mode's PRIOR state.
Restoring rather than force-disabling matters: `auto-dim-other-buffers-mode' is
global, and switching it off unconditionally left a later test in this file
asserting the mode is on with it off.  That only stayed hidden because ERT runs
tests alphabetically and the asserting test sorts first."
  (declare (indent 2))
  `(let ((config (current-window-configuration))
         (was-on (bound-and-true-p auto-dim-other-buffers-mode)))
     (unwind-protect
         (let* ((,win-a (selected-window))
                (,win-b (split-window)))
           (set-window-buffer ,win-a (get-buffer-create " *adob-a*"))
           (set-window-buffer ,win-b (get-buffer-create " *adob-b*"))
           (select-window ,win-a)
           (auto-dim-other-buffers-mode 1)
           ,@body)
       (when (fboundp 'auto-dim-other-buffers-mode)
         (auto-dim-other-buffers-mode (if was-on 1 -1)))
       (set-window-configuration config)
       (dolist (name '(" *adob-a*" " *adob-b*"))
         (when (get-buffer name) (kill-buffer name))))))

(ert-deftest test-auto-dim-config-minibuffer-entry-leaves-previous-window-lit ()
  "Normal: with the flag nil, entering the minibuffer leaves the previous window lit.
This is the half that works, and the reason the flag is set to nil.

Deliberately asserts the composite behavior rather than naming one function.
Selecting the minibuffer fires the mode's own hooks, so the observable outcome is
not attributable to the explicit `adob--update' call alone -- and the observable
outcome is what the setting promises the user.

The `win-b' assertions are positive controls.  Without them this test passes
against an implementation where dimming is broken everywhere, which looks
identical to the implementation being correct."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (test-auto-dim--with-two-windows win-a win-b
    (adob--rescan-windows)
    (should (null (window-parameter win-a 'adob--dim)))
    (should (window-parameter win-b 'adob--dim))
    (select-window (minibuffer-window))
    (adob--update)
    (should (null (window-parameter win-a 'adob--dim)))
    (should (window-parameter win-b 'adob--dim))))

(ert-deftest test-auto-dim-config-minibuffer-entry-dims-when-flag-is-t ()
  "Boundary: with the flag t, entering the minibuffer DOES dim the previous window.
This is what gives the nil setting meaning.  Without it the suite never shows the
flag changing anything, so the config assertion in
`test-auto-dim-config-applies-settings' has nothing standing behind it."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (test-auto-dim--with-two-windows win-a win-b
    (let ((auto-dim-other-buffers-dim-on-switch-to-minibuffer t))
      (adob--rescan-windows)
      (should (null (window-parameter win-a 'adob--dim)))
      (select-window (minibuffer-window))
      (adob--update)
      (should (window-parameter win-a 'adob--dim)))))

(ert-deftest test-auto-dim-config-rescan-ignores-the-minibuffer-flag ()
  "Error: `adob--rescan-windows' dims everything on a minibuffer selection.
Known defect in the fork, pinned here rather than left undocumented.  The
rescan is on `window-configuration-change-hook' and dims by window identity
alone -- and `(window-list nil \\='n)' excludes the minibuffer, so when the
minibuffer is selected nothing matches and every window dims.  A completion
popup is the everyday case: `adob--update' honours the flag, this does not.

Expected to fail until the fork honours the flag in the rescan too.  When it
starts passing, ERT reports an unexpected pass -- that is the signal to drop
this test and stop treating the gap as open.

The `win-b' positive control is load-bearing here.  Three different broken
implementations -- dimming disabled everywhere, the rescan never setting the
parameter, `adob--update' made a no-op -- all produce an unexpected pass that
would otherwise read as \"the fork fixed it\".  Asserting that `win-b' is still
dimmed separates a real fix from dimming having broken."
  :expected-result :failed
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (test-auto-dim--with-two-windows win-a win-b
    (adob--rescan-windows)
    (should (null (window-parameter win-a 'adob--dim)))
    (should (window-parameter win-b 'adob--dim))
    (select-window (minibuffer-window))
    (adob--rescan-windows)
    (should (window-parameter win-b 'adob--dim))
    (should (null (window-parameter win-a 'adob--dim)))))

(defconst test-auto-dim--flat-dimmed-org-faces
  (append (mapcar (lambda (n) (intern (format "org-level-%d" n)))
                  (number-sequence 1 8))
          '(org-link org-tag
            ;; Document header: #+TITLE:, #+AUTHOR:, #+ARCHIVE: and their values.
            org-document-title org-document-info org-document-info-keyword
            org-meta-line
            ;; Inline markup and blocks.
            org-code org-verbatim org-block-begin-line org-block-end-line
            ;; Drawers, properties, planning lines.
            org-drawer org-special-keyword org-property-value org-date
            ;; Tables and the fold indicator.
            org-table org-table-row org-ellipsis))
  "Org faces that must flat-dim to the `auto-dim-other-buffers' face.
These carry structure, not status: nothing about them needs to stay
readable in a window the user is not looking at.  Excluded on purpose are
`org-todo' and `org-priority' (keyword class -- see the -dim variant test
below) and `org-hide' (needs `auto-dim-other-buffers-hide' so folded text
stays hidden).")

(defconst test-auto-dim--flat-dimmed-link-faces
  '(link link-visited)
  "Built-in link faces that must flat-dim, distinct from `org-link'.
They fontify links in help, info, and customize buffers.  Both carry
`:underline t', which survives the relative remap, so a dimmed link still
reads as a link.")

(defconst test-auto-dim--flat-dimmed-superstar-faces
  '(org-superstar-header-bullet org-superstar-item org-superstar-first)
  "org-superstar faces that must flat-dim.
org-superstar puts its own face ahead of the org face beneath, so a heading
star renders as (org-superstar-header-bullet org-level-1) and wins over the
dimmed org-level-1.  Without these, bullets stay lit in an unfocused window
even though every face under them dims.
`org-superstar-leading' is excluded on purpose -- see the test below.")

(defconst test-auto-dim--hide-class-faces
  '(org-hide org-superstar-leading org-indent)
  "Faces whose foreground IS the background colour.
That is what makes them invisible.  They take `auto-dim-other-buffers-hide',
never the flat dim, which would paint them visible grey.")

(defconst test-auto-dim--no-foreground-faces
  '(bold italic underline)
  "Faces that carry no foreground, even through inheritance.
They set weight, slant or underline only, so text wearing them takes its
colour from `default', which is already remapped.  They need no entry and
must not gain one, or the alist grows entries that do nothing.")

(defconst test-auto-dim--keyword-dim-variants
  '((org-faces-todo       . org-faces-todo-dim)
    (org-faces-doing      . org-faces-doing-dim)
    (org-faces-priority-a . org-faces-priority-a-dim))
  "Sample of keyword faces that must keep dedicated -dim variants.")

(ert-deftest test-auto-dim-config-org-structure-faces-flat-dim ()
  "Normal: org heading, link, and tag faces remap to the flat dim face."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (dolist (face test-auto-dim--flat-dimmed-org-faces)
    (let ((entry (assq face auto-dim-other-buffers-affected-faces)))
      (should entry)
      (should (eq 'auto-dim-other-buffers (car (cdr entry))))
      (should (null (cdr (cdr entry)))))))

(ert-deftest test-auto-dim-config-link-faces-flat-dim ()
  "Normal: the built-in `link' and `link-visited' faces flat-dim.
Without these, links in help, info, and customize buffers stay lit while
the rest of an unfocused window fades.  `org-link' is a separate face and
is covered above."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (dolist (face test-auto-dim--flat-dimmed-link-faces)
    (let ((entry (assq face auto-dim-other-buffers-affected-faces)))
      (should entry)
      (should (eq 'auto-dim-other-buffers (car (cdr entry))))
      (should (null (cdr (cdr entry)))))))

(ert-deftest test-auto-dim-config-link-underline-survives-the-remap ()
  "Boundary: the dim face sets no `:underline', so the link cue survives.
A relative remap layers the dim face over the base face, so an underline
the dim face does not specify falls through from `link'.  If the theme ever
gives `auto-dim-other-buffers' an `:underline', dimmed links stop looking
like links and this test says so."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (should (eq 'unspecified
              (face-attribute 'auto-dim-other-buffers :underline nil nil))))

(ert-deftest test-auto-dim-config-superstar-bullets-flat-dim ()
  "Normal: org-superstar's heading stars and list bullets flat-dim.
They were the last thing left lit in an unfocused org window."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (dolist (face test-auto-dim--flat-dimmed-superstar-faces)
    (let ((entry (assq face auto-dim-other-buffers-affected-faces)))
      (should entry)
      (should (eq 'auto-dim-other-buffers (car (cdr entry))))
      (should (null (cdr (cdr entry)))))))

(ert-deftest test-auto-dim-config-superstar-leading-uses-hide-face ()
  "Error: `org-superstar-leading' takes the -hide face, never the flat dim.
Its foreground is the background colour, which is what keeps hidden leading
stars invisible.  Flat-dimming it would give them the dim face's visible grey
and reveal stars the user chose to hide.  Same contract as `org-hide'."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (should-not (memq 'org-superstar-leading
                    test-auto-dim--flat-dimmed-superstar-faces))
  (let ((entry (assq 'org-superstar-leading auto-dim-other-buffers-affected-faces)))
    (should entry)
    (should (eq 'auto-dim-other-buffers-hide (car (cdr entry))))))

(ert-deftest test-auto-dim-config-hide-class-faces-use-hide-face ()
  "Error: every background-coloured face takes the -hide face.
`org-hide', `org-superstar-leading' and `org-indent' all resolve to the
background colour, which is what keeps folded text, leading stars and indent
prefixes invisible.  Flat-dimming any of them reveals what the user hid."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (dolist (face test-auto-dim--hide-class-faces)
    (let ((entry (assq face auto-dim-other-buffers-affected-faces)))
      (should entry)
      (should (eq 'auto-dim-other-buffers-hide (car (cdr entry)))))))

(ert-deftest test-auto-dim-config-no-org-face-left-unmapped ()
  "Boundary: a fontified org buffer uses no face we forgot to handle.
Four rounds of this bug all had the same shape: a face nobody enumerated,
sitting ahead of a mapped face in a face list and outranking it.  This walks
a representative buffer, collects every face it actually uses (including the
`line-prefix' and `wrap-prefix' org-indent hangs its faces on), and fails on
anything that is neither mapped nor deliberately excluded.

Built-in org only.  org-superstar and org-drill are elpa packages, and the
test run has no `package-initialize', so their faces are pinned by name in
the tests above instead."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (require 'org)
  (let ((used (make-hash-table :test #'eq))
        (allowed (append test-auto-dim--no-foreground-faces
                         ;; Keyword class: deliberately unmapped so status stays
                         ;; readable in an unfocused window.  Pinned by
                         ;; test-auto-dim-config-todo-priority-faces-not-flat-dimmed.
                         '(org-todo org-priority)
                         (mapcar #'car auto-dim-other-buffers-affected-faces))))
    (with-temp-buffer
      (insert "#+TITLE: T\n#+AUTHOR: A\n\n* H1 :tag:\n** TODO [#A] task\n"
              "DEADLINE: <2026-07-10 Fri>\n:PROPERTIES:\n:K: v\n:END:\n"
              "Body ~verbatim~ =code= [[https://x.org][link]].\n"
              "| a | b |\n|---+---|\n| 1 | 2 |\n"
              "#+begin_src sh\necho hi\n#+end_src\n"
              "- [X] done item\n")
      (org-mode)
      (font-lock-ensure)
      (let ((p (point-min)))
        (while (< p (point-max))
          (dolist (f (let ((v (get-text-property p 'face)))
                       (if (listp v) v (list v))))
            (when (and f (symbolp f)) (puthash f t used)))
          (dolist (prop '(line-prefix wrap-prefix))
            (let ((s (get-text-property p prop)))
              (when (stringp s)
                (dolist (f (let ((v (get-text-property 0 'face s)))
                             (if (listp v) v (list v))))
                  (when (and f (symbolp f)) (puthash f t used))))))
          (setq p (1+ p)))))
    (let (unmapped)
      (maphash (lambda (face _v)
                 (unless (memq face allowed) (push face unmapped)))
               used)
      (should (equal nil (sort unmapped #'string<))))))

(ert-deftest test-auto-dim-config-keyword-faces-keep-dim-variants ()
  "Boundary: org TODO-keyword faces keep dedicated -dim variants, not flat dim.
Keyword status is scanned across unfocused windows, so it earns a variant;
heading colour does not.  Guards the flat-dim change from over-reaching."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (dolist (pair test-auto-dim--keyword-dim-variants)
    (let ((entry (assq (car pair) auto-dim-other-buffers-affected-faces)))
      (should entry)
      (should (eq (cdr pair) (car (cdr entry)))))))

(ert-deftest test-auto-dim-config-todo-priority-faces-not-flat-dimmed ()
  "Boundary: `org-todo' and `org-priority' are never flat-dimmed.
They are keyword class.  Dimming them would erase the status colour the
-dim variants exist to preserve, so they stay out of the flat-dim set."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (dolist (face '(org-todo org-priority))
    (should-not (memq face test-auto-dim--flat-dimmed-org-faces))
    (let ((entry (assq face auto-dim-other-buffers-affected-faces)))
      (should-not (and entry (eq 'auto-dim-other-buffers (car (cdr entry))))))))

(ert-deftest test-auto-dim-config-org-hide-uses-hide-face ()
  "Boundary: `org-hide' remaps to the -hide face, not the flat dim face.
Flat-dimming it would give folded text a visible foreground."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (let ((entry (assq 'org-hide auto-dim-other-buffers-affected-faces)))
    (should entry)
    (should (eq 'auto-dim-other-buffers-hide (car (cdr entry))))))

(ert-deftest test-auto-dim-config-never-dim-dashboard-exempts-dashboard ()
  "Normal: the *dashboard* buffer is exempt from dimming."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (let* ((existed (get-buffer "*dashboard*"))
         (buffer (or existed (get-buffer-create "*dashboard*"))))
    (unwind-protect
        (should (cj/auto-dim--never-dim-dashboard-p buffer))
      (unless existed (kill-buffer buffer)))))

(ert-deftest test-auto-dim-config-never-dim-dashboard-near-miss-name-dims ()
  "Boundary: a buffer whose name only resembles the dashboard is not exempt."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (let ((buffer (get-buffer-create "dashboard")))
    (unwind-protect
        (should-not (cj/auto-dim--never-dim-dashboard-p buffer))
      (kill-buffer buffer))))

(ert-deftest test-auto-dim-config-never-dim-dashboard-other-buffer-dims ()
  "Error: an ordinary buffer is not exempt from dimming."
  (skip-unless (file-directory-p test-auto-dim--fork))
  (require 'auto-dim-config)
  (with-temp-buffer
    (should-not (cj/auto-dim--never-dim-dashboard-p (current-buffer)))))

(provide 'test-auto-dim-config)
;;; test-auto-dim-config.el ends here

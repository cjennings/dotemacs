;;; org-reveal-config.el --- Reveal.js Presentation Configuration -*- lexical-binding: t; coding: utf-8; -*-
;; author: Craig Jennings <c@cjennings.net>

;;; Commentary:
;;
;; Layer: 4 (Optional).
;; Category: O/D/P.
;; Load shape: eager.
;; Eager reason: none; presentation export is a command-loaded deferral
;;   candidate for Phase 4.
;; Top-level side effects: registers a presentation prefix keymap under
;;   cj/custom-keymap; package configuration via use-package.
;; Runtime requires: keybindings.
;; Direct test load: yes (requires keybindings explicitly).
;;
;; Integrates ox-reveal for creating reveal.js presentations from Org files.
;;
;; Fully offline workflow using a local reveal.js clone (managed by
;; scripts/setup-reveal.sh) and self-contained HTML export.
;;
;; Slides use a 16:9 canvas with a smaller base font than the stock theme,
;; and a script in the export shrinks any slide whose content would
;; otherwise be clipped (see "Slide Sizing" below).
;;
;; Keybindings (C-; p prefix):
;; - C-; p SPC : Ensure headers, export, and open in browser
;; - C-; p e : Export to self-contained HTML and open in browser
;; - C-; p p : Start live preview (re-exports on save)
;; - C-; p s : Stop live preview
;; - C-; p h : Insert #+REVEAL_ header block at top of current buffer
;; - C-; p H : Remove reveal.js headers from current buffer
;; - C-; p n : Create new presentation file (prompts for title and location)

;;; Code:

(require 'keybindings)                   ;; cj/register-prefix-map, cj/custom-keymap

;; Forward declarations for byte-compiler (ox-reveal loaded via use-package)
(defvar org-reveal-root)
(defvar org-reveal-single-file)
(defvar org-reveal-plugins)
(defvar org-reveal-highlight-css)
(defvar org-reveal-init-options)
(defvar org-reveal-head-preamble)
(defvar org-reveal-extra-script)
(declare-function org-reveal-export-to-html "ox-reveal")

;; --------------------------------- Constants ---------------------------------

(defconst cj/reveal-root
  (expand-file-name "reveal.js" user-emacs-directory)
  "Local reveal.js installation directory.")

(defconst cj/reveal-default-theme "black"
  "Default reveal.js theme for new presentations.")

(defconst cj/reveal-default-transition "none"
  "Default reveal.js slide transition for new presentations.")

;; ------------------------------- Slide Sizing --------------------------------
;;
;; reveal.js lays every slide out on a fixed canvas and scales that canvas
;; to the window; content taller than the canvas is clipped, not scrolled.
;; Three things keep a dense slide readable instead of cut off:
;;
;; - a 16:9 canvas (the shape of every screen the slides land on), so lines
;;   wrap later than on the stock 960x700 frame;
;; - a smaller base font and heading scale, set through the theme's CSS
;;   variables in the head preamble so any reveal.js 4+ theme picks them up;
;; - a script, spliced in after Reveal.initialize, that shrinks any slide
;;   whose content still overflows.  It floors at half size: a slide that
;;   overflows even there needs splitting, not shrinking.
;;
;; The script rides ox-reveal's extra-script slot, so a file that sets its
;; own #+REVEAL_EXTRA_SCRIPT replaces it.  Print view (?print-pdf) paginates
;; instead of clipping, so the fit is skipped there and a printed deck can
;; differ from the presented one.

(defconst cj/reveal-canvas-width 1280
  "Width of the reveal.js slide canvas in CSS pixels.")

(defconst cj/reveal-canvas-height 720
  "Height of the reveal.js slide canvas in CSS pixels.")

(defconst cj/reveal-base-font-size "30px"
  "Base font size for slide text, as a px value.
The stock themes use 42px, which fits about six short lines on a slide.
Keep it in px: ox-reveal runs the head preamble through `format-spec',
so a % unit would break the export.")

(defun cj/--reveal-init-options ()
  "Return the Reveal.initialize options shared by default and template.
One line with no trailing comma, so ox-reveal can splice it into the
Reveal.initialize call."
  (format "slideNumber:true, hash:true, transition:'%s', width:%d, height:%d"
          cj/reveal-default-transition
          cj/reveal-canvas-width
          cj/reveal-canvas-height))

(defun cj/--reveal-head-preamble ()
  "Return the <style> element for the document head.
Sets the house font sizes through the theme variables and stops the
theme from upper-casing headings."
  (format "<style>
:root {
  --r-main-font-size: %s;
  --r-heading1-size: 2.2em;
  --r-heading2-size: 1.5em;
  --r-heading3-size: 1.2em;
  --r-heading4-size: 1em;
}
.reveal h1, .reveal h2, .reveal h3, .reveal p { text-transform: none; }
</style>"
          cj/reveal-base-font-size))

(defun cj/--reveal-fit-script ()
  "Return the JavaScript that shrinks overflowing slides to the canvas.
ox-reveal emits it inside the <script> element right after
Reveal.initialize, so Reveal already exists: every slide is fitted once
reveal is ready and the current one refitted on each slide change.
Hidden slides are shown for the measurement only, so the display state
reveal manages is restored before anything paints."
  "// Shrink any slide whose content overflows the canvas height so it scales
// down instead of being clipped.  Floors at half size: a slide that still
// overflows there needs splitting, not shrinking.
(function () {
  var FLOOR = 0.5;    // never below half size: split the slide instead
  var MARGIN = 0.05;  // breathing room above the bottom edge of the canvas
  var PASSES = 8;     // fixed-height content converges slowly; cap the loop
  var busy = false;   // layout() re-enters through slidechanged in scroll view
  // Overview boxes every slide at the canvas height and print view paginates,
  // so a measurement there is meaningless.
  function skip() {
    return (Reveal.isPrintView && Reveal.isPrintView())
        || (Reveal.isOverview && Reveal.isOverview());
  }
  // Shrink SLIDE until it fits; return true when its font size changed.
  function fit(slide) {
    if (!slide || skip()) { return false; }
    var limit = Reveal.getConfig().height * (1 - MARGIN);
    var before = slide.style.fontSize;
    var scale = 1;
    slide.style.fontSize = '';
    for (var pass = 0; pass < PASSES && scale > FLOOR && slide.scrollHeight > limit; pass++) {
      scale = Math.max(FLOOR, scale * limit / slide.scrollHeight);
      slide.style.fontSize = (scale * 100).toFixed(1) + '%';
    }
    if (scale === FLOOR && slide.scrollHeight > limit) {
      console.warn('reveal fit: slide still overflows at half size, split it: '
                   + (slide.id || '(no id)'));
    }
    return slide.style.fontSize !== before;
  }
  function shown(el, fn) {
    var saved = el.style.display;
    el.style.display = 'block';
    try { fn(); } finally { el.style.display = saved; }
  }
  function fitAll() {
    busy = true;
    try {
      Reveal.getSlides().forEach(function (slide) {
        var parent = slide.parentNode;
        var run = function () { shown(slide, function () { fit(slide); }); };
        if (parent && parent.classList && parent.classList.contains('stack')) {
          shown(parent, run);
        } else {
          run();
        }
      });
      Reveal.layout();
    } finally { busy = false; }
  }
  function refit(slide) {
    if (busy) { return; }
    busy = true;
    try { if (fit(slide)) { Reveal.layout(); } } finally { busy = false; }
  }
  if (Reveal.isReady()) { fitAll(); } else { Reveal.on('ready', fitAll); }
  Reveal.on('slidechanged', function (event) { refit(event.currentSlide); });
  Reveal.on('overviewhidden', function () { refit(Reveal.getCurrentSlide()); });
})();")

;; --------------------------------- ox-reveal ---------------------------------

(use-package ox-reveal
  :after ox
  :config
  (setq org-reveal-root (concat "file://" cj/reveal-root))
  (setq org-reveal-single-file t)
  (setq org-reveal-plugins '(highlight notes search zoom))
  (setq org-reveal-highlight-css "%r/plugin/highlight/monokai.css")
  (setq org-reveal-init-options (cj/--reveal-init-options))
  (setq org-reveal-head-preamble (cj/--reveal-head-preamble))
  (setq org-reveal-extra-script (cj/--reveal-fit-script)))

;; ----------------------------- Private Helpers -------------------------------

(defun cj/--reveal-header-template (title)
  "Return the reveal.js header block string for TITLE."
  (unless (stringp title)
    (user-error "Title must be a string"))
  (format "#+TITLE: %s
#+AUTHOR: %s
#+DATE: %s
#+REVEAL_ROOT: %s
#+REVEAL_THEME: %s
#+REVEAL_INIT_OPTIONS: %s
#+REVEAL_PLUGINS: (highlight notes search zoom)
#+REVEAL_HIGHLIGHT_CSS: %%r/plugin/highlight/monokai.css
#+OPTIONS: toc:nil num:nil date:nil timestamp:nil author:t ^:nil

" title (user-full-name) (format-time-string "%Y-%m-%d")
    (concat "file://" cj/reveal-root)
    cj/reveal-default-theme
    (cj/--reveal-init-options)))

(defun cj/--reveal-title-to-filename (title)
  "Convert TITLE to a slug-based .org filename.
Downcases TITLE, replaces whitespace runs with hyphens, appends .org."
  (concat (replace-regexp-in-string "[[:space:]]+" "-" (downcase title))
          ".org"))

(defconst cj/--reveal-header-keywords
  '("TITLE"
    "AUTHOR"
    "DATE"
    "REVEAL_ROOT"
    "REVEAL_THEME"
    "REVEAL_INIT_OPTIONS"
    "REVEAL_PLUGINS"
    "REVEAL_HIGHLIGHT_CSS"
    "OPTIONS")
  "Org keywords inserted by `cj/--reveal-header-template'.")

(defun cj/--reveal-keyword-regexp (keyword)
  "Return a regexp matching an Org metadata line for KEYWORD."
  (format "^#\\+%s:[^\n]*\n?" (regexp-quote keyword)))

(defun cj/--reveal-has-header-p ()
  "Return non-nil when the current buffer already has reveal.js headers."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^#\\+REVEAL_\\|^#\\+REVEAL_ROOT:" nil t)))

(defun cj/--reveal-remove-headers ()
  "Remove reveal.js header lines inserted by this module.
Returns the number of lines removed."
  (let ((removed 0))
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
                  (seq-some
                   (lambda (keyword)
                     (looking-at-p (cj/--reveal-keyword-regexp keyword)))
                   cj/--reveal-header-keywords))
        (delete-region (line-beginning-position)
                       (min (point-max) (1+ (line-end-position))))
        (setq removed (1+ removed)))
      (while (looking-at-p "\n")
        (delete-char 1)))
    removed))

(defun cj/--reveal-ensure-root ()
  "Signal a `user-error' when the local reveal.js checkout is missing.
Export needs the clone at `cj/reveal-root', installed by
scripts/setup-reveal.sh.  Without it the exporter produces a broken
presentation, so fail early with an actionable message."
  (unless (file-directory-p cj/reveal-root)
    (user-error "Local reveal.js not found at %s — run scripts/setup-reveal.sh"
                cj/reveal-root)))

(defun cj/--reveal-preview-export-on-save ()
  "Export current org buffer to reveal.js HTML silently.
Intended for use as a buffer-local `after-save-hook'."
  (when (derived-mode-p 'org-mode)
    (let ((inhibit-message t))
      (org-reveal-export-to-html))))

(defun cj/--reveal-ensure-header ()
  "Insert reveal.js headers when the current Org buffer does not have them."
  (unless (cj/--reveal-has-header-p)
    (let ((title (read-from-minibuffer "Presentation title: ")))
      (save-excursion
        (goto-char (point-min))
        (insert (cj/--reveal-header-template title)))
      t)))

;; ----------------------------- Public Functions ------------------------------

(defun cj/reveal-export ()
  "Export current Org buffer to self-contained reveal.js HTML and open in browser."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (cj/--reveal-ensure-root)
  (let ((html-file (org-reveal-export-to-html)))
    (when html-file
      (browse-url-of-file html-file)
      (message "Opened presentation: %s" html-file))))

(defun cj/reveal-preview-start ()
  "Start live preview: re-export to HTML on every save.
Opens the presentation in a browser on first call.  Subsequent saves
re-export silently; refresh the browser to see changes."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (cj/--reveal-ensure-root)
  (add-hook 'after-save-hook #'cj/--reveal-preview-export-on-save nil t)
  (let ((html-file (org-reveal-export-to-html)))
    (when html-file
      (browse-url-of-file html-file)))
  (message "Live preview started — save to re-export, refresh browser to update"))

(defun cj/reveal-preview-stop ()
  "Stop live preview by removing the after-save-hook."
  (interactive)
  (remove-hook 'after-save-hook #'cj/--reveal-preview-export-on-save t)
  (message "Live preview stopped"))

(defun cj/reveal-present ()
  "Ensure reveal headers, export current Org buffer, and open it in browser."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (cj/--reveal-ensure-header)
  (when (buffer-modified-p)
    (if buffer-file-name
        (save-buffer)
      (call-interactively #'write-file)))
  (cj/reveal-export))

(defun cj/reveal-insert-header ()
  "Insert a #+REVEAL_ header block at the top of the current Org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (when (cj/--reveal-has-header-p)
    (user-error "Reveal headers already present"))
  (cj/--reveal-ensure-header)
  (message "Inserted reveal.js headers"))

(defun cj/reveal-remove-headers ()
  "Remove reveal.js headers inserted by this module from the current Org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let ((removed (cj/--reveal-remove-headers)))
    (message "Removed %d reveal.js header line%s"
             removed
             (if (= removed 1) "" "s"))))

(defun cj/reveal-new ()
  "Create a new reveal.js presentation file.
Prompts for a title and save location, then opens the file with
reveal.js headers pre-filled."
  (interactive)
  (let* ((title (read-from-minibuffer "Presentation title: "))
         (default-dir (expand-file-name "~/"))
         (file (read-file-name "Save presentation to: " default-dir nil nil
                               (cj/--reveal-title-to-filename title))))
    (when (file-exists-p file)
      (user-error "File already exists: %s" file))
    (find-file file)
    (insert (cj/--reveal-header-template title))
    (insert "* Slide 1\n\n")
    (save-buffer)
    (message "New presentation: %s" file)))

;; -------------------------------- Keybindings --------------------------------

;; A registered prefix keymap, not raw `global-set-key' chains: binding
;; "C-; p ..." directly depends on keybindings.el having already made "C-;"
;; a live prefix (otherwise "non-prefix key" errors), while
;; `cj/register-prefix-map' binds into `cj/custom-keymap' with no load-order
;; dependency beyond requiring keybindings.
(defvar-keymap cj/reveal-map
  :doc "Keymap for reveal.js presentation commands."
  "SPC" #'cj/reveal-present
  "e"   #'cj/reveal-export
  "p"   #'cj/reveal-preview-start
  "s"   #'cj/reveal-preview-stop
  "h"   #'cj/reveal-insert-header
  "H"   #'cj/reveal-remove-headers
  "n"   #'cj/reveal-new)

(cj/register-prefix-map "p" cj/reveal-map "presentations")

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-; p SPC" "present current buffer"
    "C-; p e" "export & open"
    "C-; p p" "start live preview"
    "C-; p s" "stop live preview"
    "C-; p h" "insert headers"
    "C-; p H" "remove headers"
    "C-; p n" "new presentation"))

(provide 'org-reveal-config)
;;; org-reveal-config.el ends here

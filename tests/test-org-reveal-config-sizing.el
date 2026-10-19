;;; test-org-reveal-config-sizing.el --- Tests for reveal.js slide sizing and auto-fit -*- lexical-binding: t; -*-

;;; Commentary:
;; Dense slides overflowed reveal.js's 960x700 canvas at the theme's 42px
;; base font and were clipped with no way to scroll.  The module now ships
;; a 16:9 canvas with a smaller base font (via the head preamble) and a
;; script, spliced in after Reveal.initialize, that shrinks any slide whose
;; content still overflows.  These tests pin the three builders and run the
;; fit algorithm under node against a stub Reveal API.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'keybindings)
(require 'org-reveal-config)

(defun test-org-reveal-config--count (regexp string)
  "Return how many times REGEXP matches in STRING."
  (let ((n 0) (start 0))
    (while (string-match regexp string start)
      (setq n (1+ n)
            start (match-end 0)))
    n))

;; ------------------------------ Init options -------------------------------

(ert-deftest test-org-reveal-config-init-options-normal-has-behaviour-and-canvas ()
  "Normal: the shared init options carry slide numbers, hash, transition and the canvas size."
  (let ((opts (cj/--reveal-init-options)))
    (should (string-match-p "slideNumber:true" opts))
    (should (string-match-p "hash:true" opts))
    (should (string-match-p (format "transition:'%s'" cj/reveal-default-transition) opts))
    (should (string-match-p (format "width:%d" cj/reveal-canvas-width) opts))
    (should (string-match-p (format "height:%d" cj/reveal-canvas-height) opts))))

(ert-deftest test-org-reveal-config-init-options-boundary-splices-into-initialize ()
  "Boundary: one line, no trailing comma, so it splices into Reveal.initialize({...})."
  (let ((opts (cj/--reveal-init-options)))
    (should-not (string-match-p "\n" opts))
    (should-not (string-suffix-p "," (string-trim opts)))))

(ert-deftest test-org-reveal-config-init-options-normal-canvas-is-16-9 ()
  "Normal: the canvas is a 16:9 frame, the shape of every projector and monitor the slides land on."
  (should (= (* 9 cj/reveal-canvas-width) (* 16 cj/reveal-canvas-height))))

(ert-deftest test-org-reveal-config-header-template-normal-uses-shared-init-options ()
  "Normal: the header template emits the same init options as the global default, so a file never drifts from the module."
  (let ((result (cj/--reveal-header-template "Sizing")))
    (should (string-match-p
             (format "^#\\+REVEAL_INIT_OPTIONS: %s$"
                     (regexp-quote (cj/--reveal-init-options)))
             result))))

;; ------------------------------ Head preamble ------------------------------

(ert-deftest test-org-reveal-config-head-preamble-normal-sets-font-sizes ()
  "Normal: the preamble lowers the theme's base font and heading sizes through the theme variables."
  (let ((css (cj/--reveal-head-preamble)))
    (should (string-match-p (format "--r-main-font-size: *%s"
                                    (regexp-quote cj/reveal-base-font-size))
                            css))
    (should (string-match-p "--r-heading1-size" css))
    (should (string-match-p "--r-heading2-size" css))))

(ert-deftest test-org-reveal-config-head-preamble-normal-keeps-no-caps-rule ()
  "Normal: the rule that stops the theme upper-casing headings survives the rewrite."
  (should (string-match-p "text-transform: *none" (cj/--reveal-head-preamble))))

(ert-deftest test-org-reveal-config-head-preamble-boundary-one-style-element ()
  "Boundary: exactly one <style> element, opened and closed."
  (let ((css (cj/--reveal-head-preamble)))
    (should (= 1 (test-org-reveal-config--count "<style" css)))
    (should (= 1 (test-org-reveal-config--count "</style>" css)))))

;; -------------------------------- Fit script -------------------------------

(ert-deftest test-org-reveal-config-fit-script-normal-hooks-reveal-events ()
  "Normal: the script fits every slide once reveal is ready and refits on each slide change."
  (let ((js (cj/--reveal-fit-script)))
    (should (string-match-p "Reveal\\.on('ready'" js))
    (should (string-match-p "Reveal\\.on('slidechanged'" js))
    (should (string-match-p "Reveal\\.on('overviewhidden'" js))
    (should (string-match-p "isOverview" js))
    (should (string-match-p "scrollHeight" js))
    (should (string-match-p "getConfig()\\.height" js))))

(ert-deftest test-org-reveal-config-fit-script-boundary-no-script-tags ()
  "Boundary: ox-reveal splices the text inside its own <script> element, so it must carry no tags."
  (let ((js (cj/--reveal-fit-script)))
    (should-not (string-match-p "<script" js))
    (should-not (string-match-p "</script" js))))

(defconst test-org-reveal-config--fit-harness
  "const MODE = '%s';
let overview = false;
const make = (base, fixed, inStack) => {
  const s = { style: { fontSize: '', display: 'none' },
              parentNode: { classList: { contains: (c) => inStack && c === 'stack' },
                            style: { display: 'none' } } };
  Object.defineProperty(s, 'scrollHeight', { get() {
    const m = /^([\\d.]+)%%$/.exec(s.style.fontSize);
    const h = fixed + Math.round(base * (m ? parseFloat(m[1]) / 100 : 1));
    return overview ? Math.max(h, 720) : h; } });
  return s;
};
const slides = [make(600, 0, false), make(684, 0, true), make(1368, 0, true),
                make(100000, 0, false), make(900, 100, false), make(500, 450, true)];
const handlers = {};
let layouts = 0;
let warnings = 0;
console.warn = () => { warnings++; };
global.Reveal = {
  getConfig: () => ({ height: 720 }),
  getSlides: () => slides,
  getCurrentSlide: () => slides[2],
  layout: () => { layouts++;
                  if (MODE === 'reentrant' && handlers.slidechanged) {
                    handlers.slidechanged({ currentSlide: slides[2] }); } },
  isReady: () => MODE === 'ready',
  isPrintView: () => false,
  isOverview: () => overview,
  on: (name, fn) => { handlers[name] = fn; } };
%s
const readyHandler = Boolean(handlers.ready);
if (handlers.ready) { handlers.ready(); }
if (MODE === 'overview') {
  overview = true;
  handlers.slidechanged({ currentSlide: slides[0] });
  overview = false;
  handlers.overviewhidden();
}
if (MODE === 'reentrant') {
  slides[4].style.fontSize = '';
  handlers.slidechanged({ currentSlide: slides[4] });
}
handlers.slidechanged({ currentSlide: slides[2] });
handlers.slidechanged({ currentSlide: null });
process.stdout.write(JSON.stringify({
  sizes: slides.map(s => s.style.fontSize),
  displays: slides.map(s => s.style.display),
  parents: slides.map(s => s.parentNode.style.display),
  overflow: slides.map(s => s.scrollHeight > 684),
  readyHandler, overviewHandler: Boolean(handlers.overviewhidden),
  layouts, warnings }));
"
  "Node harness: a stub Reveal API around the fit script.
The first %s is the harness MODE (normal, ready, overview, reentrant) and
the second is the script.  Each stub slide's scrollHeight is a fixed part
plus a text part that scales with the font-size the script sets, so a
later measuring pass sees the effect of the earlier one.  Slides 1, 2 and
5 sit in a .stack parent, as every ox-reveal content slide does.  The
canvas is 720px, so the usable limit with the 5% margin is 684px.  In
overview the stub floors scrollHeight at the 720px box, the way reveal's
overview CSS does.")

(defun test-org-reveal-config--run-fit-harness (&optional mode)
  "Run the fit script under node in harness MODE; return the parsed result."
  (let ((file (make-temp-file "reveal-fit-" nil ".js"))
        (out (generate-new-buffer " *fit-harness*")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert (format test-org-reveal-config--fit-harness
                            (or mode "normal")
                            (cj/--reveal-fit-script))))
          (let ((status (call-process "node" nil out nil file)))
            (unless (eq status 0)
              (error "node exited %s: %s" status
                     (with-current-buffer out (buffer-string))))
            (with-current-buffer out
              (goto-char (point-min))
              (json-parse-buffer :object-type 'alist :array-type 'list))))
      (delete-file file)
      (kill-buffer out))))

(ert-deftest test-org-reveal-config-fit-script-normal-shrinks-only-overflowing-slides ()
  "Normal: a slide twice the usable height is scaled to 50%; slides that fit are left alone."
  (skip-unless (executable-find "node"))
  (let-alist (test-org-reveal-config--run-fit-harness)
    (should (equal "" (nth 0 .sizes)))
    (should (equal "" (nth 1 .sizes)))
    (should (equal "50.0%" (nth 2 .sizes)))))

(ert-deftest test-org-reveal-config-fit-script-normal-converges-with-fixed-height-content ()
  "Normal: content with a fixed-height part needs several passes and still lands inside the limit."
  (skip-unless (executable-find "node"))
  (let-alist (test-org-reveal-config--run-fit-harness)
    (should (string-match-p "\\`6[0-9]\\.[0-9]%\\'" (nth 4 .sizes)))
    (should (eq :false (nth 4 .overflow)))))

(ert-deftest test-org-reveal-config-fit-script-boundary-floors-at-half-size ()
  "Boundary: content that would need less than half size stops at the floor, and only those slides are reported."
  (skip-unless (executable-find "node"))
  (let-alist (test-org-reveal-config--run-fit-harness)
    (should (equal "50.0%" (nth 3 .sizes)))
    (should (equal "50.0%" (nth 5 .sizes)))
    (should (eq t (nth 3 .overflow)))
    (should (eq t (nth 5 .overflow)))
    (should (= 2 .warnings))))

(ert-deftest test-org-reveal-config-fit-script-boundary-restores-display-and-relayouts ()
  "Boundary: measuring hidden slides and their stack parents leaves display as reveal set it, and layout runs afterwards."
  (skip-unless (executable-find "node"))
  (let-alist (test-org-reveal-config--run-fit-harness)
    (should (equal '("none" "none" "none" "none" "none" "none") .displays))
    (should (equal '("none" "none" "none" "none" "none" "none") .parents))
    (should (> .layouts 0))))

(ert-deftest test-org-reveal-config-fit-script-normal-fits-at-once-when-reveal-is-ready ()
  "Normal: when reveal is already ready the slides are fitted immediately instead of waiting for an event."
  (skip-unless (executable-find "node"))
  (let-alist (test-org-reveal-config--run-fit-harness "ready")
    (should (eq :false .readyHandler))
    (should (equal "50.0%" (nth 2 .sizes)))))

(ert-deftest test-org-reveal-config-fit-script-boundary-overview-is-left-alone ()
  "Boundary: in overview every slide measures as the full box, so nothing is refitted until the overview closes."
  (skip-unless (executable-find "node"))
  (let-alist (test-org-reveal-config--run-fit-harness "overview")
    (should (equal "" (nth 0 .sizes)))
    (should (eq t .overviewHandler))))

(ert-deftest test-org-reveal-config-fit-script-error-null-slide-is-ignored ()
  "Error: a slidechanged event without a current slide is ignored rather than thrown."
  (skip-unless (executable-find "node"))
  (should (test-org-reveal-config--run-fit-harness)))

(ert-deftest test-org-reveal-config-fit-script-error-reentrant-layout-does-not-recurse ()
  "Error: a layout that fires slidechanged again (reveal's scroll view does) must not recurse without end."
  (skip-unless (executable-find "node"))
  (let-alist (test-org-reveal-config--run-fit-harness "reentrant")
    (should (>= .layouts 2))))

(provide 'test-org-reveal-config-sizing)
;;; test-org-reveal-config-sizing.el ends here

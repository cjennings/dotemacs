;;; test-eat-config--xtwinops.el --- Tests for the EAT XTWINOPS window-size reply -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the XTWINOPS (CSI <n> t) window-size responder.  eat 0.9.4
;; has no CSI <n> t handler, so it silently drops the window-size requests
;; tmux 3.7b sends to learn the cell pixel size it needs before it will emit
;; Sixel -- images then never render inside EAT.  The module answers three
;; requests: 14 (text area in pixels), 16 (cell size in pixels), 18 (text area
;; in characters).
;;
;; Two pure pieces carry the logic and are tested here directly:
;; - `cj/--eat-xtwinops-report' computes the reply string from the display and
;;   cell dimensions.
;; - `cj/--eat-xtwinops-queries' extracts the request numbers from a chunk of
;;   terminal output.
;; The thin accessor glue (`cj/--eat-send-window-size-report', which reads the
;; live `eat--t-term' struct) is verified in the running daemon, since eat's
;; structs are not loadable under `make test' (no package-initialize).  The
;; detector's dispatch is tested against a recording stub of the responder.

;;; Code:

(require 'ert)

;; Stub keymap dep before loading the module (matches the other module tests).
(defvar cj/custom-keymap (make-sparse-keymap)
  "Stub keymap for testing.")

(require 'eat-config)

;;; --------------------------- reply computation ----------------------------

(ert-deftest test-eat-config-xtwinops-report-normal-14-text-area-pixels ()
  "Normal: request 14 reports text-area size in pixels (rows*ch by cols*cw)."
  ;; 80x24 chars, 10x20 px cells -> height 24*20=480, width 80*10=800.
  (should (equal (cj/--eat-xtwinops-report 14 80 24 10 20)
                 "\e[4;480;800t")))

(ert-deftest test-eat-config-xtwinops-report-normal-16-cell-pixels ()
  "Normal: request 16 reports the cell size in pixels (height then width)."
  (should (equal (cj/--eat-xtwinops-report 16 80 24 10 20)
                 "\e[6;20;10t")))

(ert-deftest test-eat-config-xtwinops-report-normal-18-text-area-chars ()
  "Normal: request 18 reports the text-area size in characters (rows then cols)."
  (should (equal (cj/--eat-xtwinops-report 18 80 24 10 20)
                 "\e[8;24;80t")))

(ert-deftest test-eat-config-xtwinops-report-boundary-unit-cells ()
  "Boundary: with eat's default 1x1 px cells, pixel dims equal the char dims."
  (should (equal (cj/--eat-xtwinops-report 14 80 24 1 1) "\e[4;24;80t"))
  (should (equal (cj/--eat-xtwinops-report 16 80 24 1 1) "\e[6;1;1t")))

(ert-deftest test-eat-config-xtwinops-report-error-unknown-request-is-nil ()
  "Error: any request number other than 14/16/18 returns nil (unanswered)."
  (should (null (cj/--eat-xtwinops-report 15 80 24 10 20)))
  (should (null (cj/--eat-xtwinops-report 24 80 24 10 20)))
  (should (null (cj/--eat-xtwinops-report nil 80 24 10 20))))

;;; ----------------------------- query detection ----------------------------

(ert-deftest test-eat-config-xtwinops-queries-normal-single ()
  "Normal: a lone CSI 14 t query is detected."
  (should (equal (cj/--eat-xtwinops-queries "\e[14t") '(14))))

(ert-deftest test-eat-config-xtwinops-queries-normal-embedded-multiple ()
  "Normal: several queries embedded in other output are returned in order."
  (should (equal (cj/--eat-xtwinops-queries "foo\e[14tbar\e[18tbaz\e[16t")
                 '(14 18 16))))

(ert-deftest test-eat-config-xtwinops-queries-boundary-none ()
  "Boundary: output with no XTWINOPS query returns nil, including empty."
  (should (null (cj/--eat-xtwinops-queries "")))
  (should (null (cj/--eat-xtwinops-queries "hello\e[0m\e[2J"))))

(ert-deftest test-eat-config-xtwinops-queries-error-unhandled-ops-ignored ()
  "Error: CSI t ops we do not answer (and parametrized forms) are not matched.
`\\e[24t' is a resize op, `\\e[3;14t' carries a leading param -- neither is the
bare CSI 14/16/18 t we answer, so both are ignored."
  (should (null (cj/--eat-xtwinops-queries "\e[24t")))
  (should (null (cj/--eat-xtwinops-queries "\e[3;14t")))
  (should (null (cj/--eat-xtwinops-queries "\e[114t"))))

;;; --------------------------- advice-needed guard --------------------------

(ert-deftest test-eat-config-xtwinops-advice-needed-normal-eat-0-9-4 ()
  "Normal: on eat 0.9.4 (no upstream CSI t clause) the advice is needed."
  ;; The upstream parser clause defines `eat--t-send-window-size-report';
  ;; 0.9.4 does not, so it must be unbound here.
  (should-not (fboundp 'eat--t-send-window-size-report))
  (should (cj/--eat-xtwinops-advice-needed-p)))

(ert-deftest test-eat-config-xtwinops-advice-needed-boundary-upstream-ships ()
  "Boundary: once upstream defines the parser clause, the advice must NOT
install -- both would answer and tmux gets a double reply."
  (cl-letf (((symbol-function 'eat--t-send-window-size-report) #'ignore))
    (should-not (cj/--eat-xtwinops-advice-needed-p))))

;;; ------------------------- detector dispatch (seam) -----------------------

(ert-deftest test-eat-config-xtwinops-answer-dispatches-once-per-query ()
  "Integration: the detector calls the responder once per query, in order.
Mocks only our own responder seam (`cj/--eat-send-window-size-report'); the
detector under test does the real query extraction."
  (let ((calls '()))
    (cl-letf (((symbol-function 'cj/--eat-send-window-size-report)
               (lambda (n) (push n calls))))
      (cj/--eat-answer-xtwinops "\e[14t\e[16t\e[18t"))
    (should (equal (nreverse calls) '(14 16 18)))))

(ert-deftest test-eat-config-xtwinops-answer-no-query-no-call ()
  "Boundary: output with no query never calls the responder."
  (let ((called nil))
    (cl-letf (((symbol-function 'cj/--eat-send-window-size-report)
               (lambda (_n) (setq called t))))
      (cj/--eat-answer-xtwinops "no query here\e[2J"))
    (should-not called)))

(provide 'test-eat-config--xtwinops)
;;; test-eat-config--xtwinops.el ends here

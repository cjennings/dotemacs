;;; test-meta-native-comp-speed.el --- Guard native-comp-speed at 2 -*- lexical-binding: t -*-

;;; Commentary:
;; Meta test pinning `native-comp-speed' to 2 or lower.
;;
;; At speed 3 the native compiler emits direct calls for functions in the
;; same compilation unit, bypassing the symbol's function cell.  Any test
;; that `cl-letf'-mocks a module's own internal helper then silently
;; exercises the real code instead of the mock.  This bit for real on
;; 2026-07-01: the video-audio-recording tests mocked
;; `cj/recording--wayland-p' and `cj/recording--validate-system-audio',
;; the speed-3 eln called the real ones, and `make test' launched actual
;; wf-recorder screen recordings and rewrote the configured audio device
;; from live pactl state.
;;
;; Speed 2 is the highest level that preserves redefinition semantics.

;;; Code:

(require 'ert)

(ert-deftest test-meta-native-comp-speed-preserves-redefinition-semantics ()
  "Normal: `native-comp-speed' stays at or below 2 after config loads.
Speed 3 breaks function-cell redirection for same-compilation-unit
calls, silently bypassing `cl-letf' mocks in the test suite."
  (skip-unless (and (fboundp 'native-comp-available-p)
                    (native-comp-available-p)))
  ;; system-defaults sets the value inside `with-eval-after-load'
  ;; comp-run, so load comp-run first to make that form fire.
  (require 'comp-run)
  (require 'system-defaults)
  (should (<= native-comp-speed 2)))

(provide 'test-meta-native-comp-speed)
;;; test-meta-native-comp-speed.el ends here

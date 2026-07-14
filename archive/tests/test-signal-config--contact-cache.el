;;; test-signal-config--contact-cache.el --- Contact-cache lifecycle tests -*- lexical-binding: t; -*-

;;; Commentary:
;; The picker's contact cache has two lifecycle bugs from the 2026-06 config
;; audit: (1) its docstring promised clearing on signel-stop but nothing
;; cleared it, so a stale list survived a relink/reconnect; (2) a
;; fetched-and-empty list was cached as nil, indistinguishable from a cold
;; cache, so a zero-contact account re-ran the blocking fetch (up to
;; `cj/signel-fetch-timeout') on every picker open.  The fix names the empty
;; state with an `empty' sentinel and clears the cache via a named function
;; advised onto `signel-stop'.
;;
;; Boundary mocks only (the RPC send, the prompt); the cache logic runs
;; real.  The ensure-started branches the audit called untested were
;; already covered in test-signal-config.el -- that claim was stale.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'signal-config)

;;; ------------------------- clear-on-stop (F1) ------------------------------

(ert-deftest test-signal-config-clear-contact-cache-resets ()
  "Normal: the clear function empties the cache back to cold (nil)."
  (let ((cj/signel--contact-cache '(("Al (+1)" . "+1"))))
    (cj/signel--clear-contact-cache)
    (should (null cj/signel--contact-cache))))

(ert-deftest test-signal-config-clear-contact-cache-advises-stop ()
  "Normal: `signel-stop' carries the clear advice, so the docstring's
\"cleared on stop/restart\" promise is real."
  (should (advice-member-p #'cj/signel--clear-contact-cache 'signel-stop)))

;;; ------------------------- empty sentinel (F2) ------------------------------

(ert-deftest test-signal-config-fetch-empty-caches-sentinel ()
  "Boundary: a fetched-and-empty list caches the `empty' sentinel, not nil.
nil means cold cache; without the sentinel a zero-contact account re-ran
the blocking fetch on every picker open."
  (let ((cj/signel--contact-cache nil)
        (captured-callback nil))
    (cl-letf (((symbol-function 'signel--send-rpc)
               (lambda (_method _params _buf callback)
                 (setq captured-callback callback))))
      (cj/signel--fetch-contacts)
      (funcall captured-callback '()))
    (should (eq cj/signel--contact-cache 'empty))))

(ert-deftest test-signal-config-cached-contacts-unwraps-sentinel ()
  "Normal: the cache reader returns the alist, and nil for the sentinel."
  (let ((cj/signel--contact-cache '(("Al (+1)" . "+1"))))
    (should (equal (cj/signel--cached-contacts) '(("Al (+1)" . "+1")))))
  (let ((cj/signel--contact-cache 'empty))
    (should (null (cj/signel--cached-contacts)))))

(provide 'test-signal-config--contact-cache)
;;; test-signal-config--contact-cache.el ends here

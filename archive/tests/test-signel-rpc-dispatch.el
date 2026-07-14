;;; test-signel-rpc-dispatch.el --- Tests for signel JSON-RPC success-result dispatch -*- lexical-binding: t; -*-

;;; Commentary:
;; signel's JSON-RPC dispatch (signel.el in the fork at ~/code/signel) routes
;; incoming `receive' notifications and errors, but successful
;; `((id . N) (result . VALUE))' responses had no path until this work added a
;; request-callback table.  These tests cover the new behavior: a registered
;; callback fires with the result and is then removed; an error response
;; also removes the handler so a retry starts clean; an unregistered id is a
;; silent no-op; passing SUCCESS-CALLBACK to `signel--send-rpc' registers it
;; under the returned id.
;;
;; The dispatch tests exercise `signel--dispatch' directly with synthetic JSON
;; alists; no live process is needed.  The send-rpc test stubs `get-process'
;; and `process-send-string' so it doesn't require a running signal-cli.

;;; Code:

(require 'ert)
(require 'cl-lib)

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "~/code/signel")))
(require 'signel)

(defun test-signel-rpc--reset ()
  "Reset signel dispatch state to a clean baseline before each test."
  (clrhash signel--request-handler-map)
  (clrhash signel--request-buffer-map)
  (setq signel--rpc-id-counter 0))

(ert-deftest test-signel-rpc-dispatch-result-invokes-callback ()
  "Normal: a result response with a registered id fires the callback with the
result value and removes the handler."
  (test-signel-rpc--reset)
  (let ((captured nil))
    (puthash 7 (lambda (val) (setq captured val)) signel--request-handler-map)
    (signel--dispatch '((jsonrpc . "2.0") (id . 7)
                        (result . ((contacts . [1 2 3])))))
    (should (equal captured '((contacts . [1 2 3]))))
    (should-not (gethash 7 signel--request-handler-map))))

(ert-deftest test-signel-rpc-dispatch-unknown-id-is-noop ()
  "Boundary: a result response with an unregistered id is a silent no-op:
neither receive nor error handler fires, and the handler map stays empty."
  (test-signel-rpc--reset)
  (let ((called nil))
    (cl-letf (((symbol-function 'signel--handle-error)
               (lambda (&rest _) (setq called 'error)))
              ((symbol-function 'signel--handle-receive)
               (lambda (&rest _) (setq called 'receive))))
      (signel--dispatch '((jsonrpc . "2.0") (id . 99) (result . "anything"))))
    (should-not called)
    (should (zerop (hash-table-count signel--request-handler-map)))))

(ert-deftest test-signel-rpc-dispatch-error-cleans-up-handler ()
  "Error: an error response with a registered id removes the handler without
firing the callback, leaving the map clean for a retry."
  (test-signel-rpc--reset)
  (let ((fired nil))
    (puthash 11 (lambda (&rest _) (setq fired t))
             signel--request-handler-map)
    (cl-letf (((symbol-function 'signel--handle-error) (lambda (&rest _) nil)))
      (signel--dispatch '((jsonrpc . "2.0") (id . 11)
                          (error . ((code . -1) (message . "boom"))))))
    (should-not fired)
    (should-not (gethash 11 signel--request-handler-map))))

(ert-deftest test-signel-rpc-send-rpc-registers-success-callback ()
  "Normal: passing a SUCCESS-CALLBACK to `signel--send-rpc' stores it under
the returned id so the matching response can route to it."
  (test-signel-rpc--reset)
  (let ((cb (lambda (_) 'ok))
        (sent nil))
    (cl-letf (((symbol-function 'get-process) (lambda (&rest _) 'fake-proc))
              ((symbol-function 'process-send-string)
               (lambda (_ s) (setq sent s)))
              ((symbol-function 'signel--log) (lambda (&rest _) nil)))
      (let ((id (signel--send-rpc "listContacts" nil nil cb)))
        (should (eq cb (gethash id signel--request-handler-map)))
        (should (stringp sent))))))

(ert-deftest test-signel-rpc-stop-clears-handler-map ()
  "Normal (reconnect-invalidation): `signel-stop' clears the handler map so a
restart starts with no stale callbacks waiting for responses that will never
arrive."
  (test-signel-rpc--reset)
  (puthash 13 (lambda (&rest _) nil) signel--request-handler-map)
  (cl-letf (((symbol-function 'get-process) (lambda (&rest _) nil)))
    (signel-stop))
  (should (zerop (hash-table-count signel--request-handler-map))))

(provide 'test-signel-rpc-dispatch)
;;; test-signel-rpc-dispatch.el ends here

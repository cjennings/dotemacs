;;; test-erc-config-connected-servers.el --- cj/erc-connected-servers -*- lexical-binding: t; -*-

;;; Commentary:
;; cj/erc-connected-servers must return only ERC *server* buffers with a live
;; process.  The original test compared a buffer's own erc-server-process to the
;; same buffer-local value inside `with-current-buffer', which is always true, so
;; it returned every ERC buffer (channels, queries, dead connections).  These
;; tests stub `erc-buffer-list' and the two ERC predicates
;; (`erc-server-or-unjoined-channel-buffer-p' and `erc-server-process-alive')
;; so the classification is exercised without a real IRC connection.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'erc-config)

(ert-deftest test-erc-connected-servers-keeps-only-live-server-buffers ()
  "Normal: only buffers that are ERC server buffers with a live process are
returned; a channel buffer and a dead-connection server buffer are excluded."
  (let ((b-server  (generate-new-buffer " *erc-server*"))
        (b-channel (generate-new-buffer " *erc-#chan*"))
        (b-dead    (generate-new-buffer " *erc-dead*")))
    (unwind-protect
        (cl-letf (((symbol-function 'erc-buffer-list)
                   (lambda (&rest _) (list b-server b-channel b-dead)))
                  ((symbol-function 'erc-server-or-unjoined-channel-buffer-p)
                   (lambda (&rest _) (memq (current-buffer) (list b-server b-dead))))
                  ((symbol-function 'erc-server-process-alive)
                   (lambda (&rest _) (eq (current-buffer) b-server))))
          (should (equal (cj/erc-connected-servers)
                         (list (buffer-name b-server)))))
      (mapc #'kill-buffer (list b-server b-channel b-dead)))))

(ert-deftest test-erc-connected-servers-empty-when-none-alive ()
  "Boundary: no live server buffers yields an empty list."
  (let ((b-channel (generate-new-buffer " *erc-#chan*")))
    (unwind-protect
        (cl-letf (((symbol-function 'erc-buffer-list)
                   (lambda (&rest _) (list b-channel)))
                  ((symbol-function 'erc-server-or-unjoined-channel-buffer-p) (lambda (&rest _) nil))
                  ((symbol-function 'erc-server-process-alive) (lambda (&rest _) nil)))
          (should (null (cj/erc-connected-servers))))
      (kill-buffer b-channel))))

(provide 'test-erc-config-connected-servers)
;;; test-erc-config-connected-servers.el ends here

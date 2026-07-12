;;; test-text-config.el --- Tests for text-config -*- lexical-binding: t; -*-

;;; Commentary:
;; Covers cj/prettify-compose-block-markers-p, the prettify-symbols compose
;; predicate that forces org src-block markers (#+begin_src / #+end_src) to
;; compose while deferring to the default predicate for everything else.

;;; Code:

(require 'ert)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'text-config)

(ert-deftest test-text-config-compose-markers-composes-begin-src ()
  "Normal: the begin-src marker always composes.
The bounds are arbitrary because the marker branch short-circuits before
touching the buffer."
  (should (cj/prettify-compose-block-markers-p 1 12 "#+begin_src")))

(ert-deftest test-text-config-compose-markers-composes-end-and-upcased ()
  "Boundary: the end marker and the upcased variants also compose."
  (should (cj/prettify-compose-block-markers-p 1 10 "#+end_src"))
  (should (cj/prettify-compose-block-markers-p 1 12 "#+BEGIN_SRC"))
  (should (cj/prettify-compose-block-markers-p 1 10 "#+END_SRC")))

(ert-deftest test-text-config-compose-markers-defers-for-non-markers ()
  "Error: a non-marker match defers to the default predicate.
The predicate must return exactly what `prettify-symbols-default-compose-p'
returns for a symbol that is not a block marker, so `lambda' keeps the
standard boundary check."
  (with-temp-buffer
    (insert "x lambda y")
    (goto-char (point-min))
    (re-search-forward "lambda")
    (let ((start (match-beginning 0))
          (end (match-end 0)))
      (should (eq (cj/prettify-compose-block-markers-p start end "lambda")
                  (prettify-symbols-default-compose-p start end "lambda"))))))

(ert-deftest test-text-config-edit-indirect-bound-on-reachable-key ()
  "Error/regression: edit-indirect-region is bound on M-I -- the event
Meta+Shift+i actually produces -- not the unreachable M-S-i, which no
keypress generates so it silently fell through to M-i tab-to-tab-stop."
  (should (eq (key-binding (kbd "M-I")) #'edit-indirect-region))
  (should-not (eq (key-binding (kbd "M-S-i")) #'edit-indirect-region)))

(ert-deftest test-text-config-accent-uses-completion-agnostic-backend ()
  "Regression: C-` invokes accent-menu, which reads through the minibuffer
and so survives a Company->Corfu migration, rather than accent-company,
whose company backend would break silently once Company is gone."
  (should (eq (key-binding (kbd "C-`")) #'accent-menu)))

(provide 'test-text-config)
;;; test-text-config.el ends here

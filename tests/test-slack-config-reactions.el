;;; test-slack-config-reactions.el --- Tests for Slack reaction helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for local Slack reaction workflow hardening and shortlist selection.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'slack-config)

(defvar slack-current-buffer)

(ert-deftest test-slack-config-reaction-echo-error-removes-local-hook ()
  "Error: malformed reaction hover errors remove the local post-command hook."
  (with-temp-buffer
    (add-hook 'post-command-hook #'slack-reaction-echo-description nil t)
    (cl-letf (((symbol-function 'message) (lambda (&rest _args) nil)))
      (cj/slack--safe-reaction-echo-description
       (lambda (&rest _args) (error "bad reaction"))))
    (should-not (memq #'slack-reaction-echo-description post-command-hook))))

(ert-deftest test-slack-config-reaction-echo-success-keeps-local-hook ()
  "Normal: successful reaction hover leaves the local hook installed."
  (with-temp-buffer
    (add-hook 'post-command-hook #'slack-reaction-echo-description nil t)
    (cj/slack--safe-reaction-echo-description (lambda (&rest _args) :ok))
    (should (memq #'slack-reaction-echo-description post-command-hook))))

(ert-deftest test-slack-config-reaction-candidates-include-common-and-fallback ()
  "Normal: common reaction candidates include frequent choices and Other."
  (let ((candidates (cj/slack--reaction-candidates)))
    (should (member '("Other..." . :other) candidates))
    (should (cl-some (lambda (candidate)
                       (and (string-match-p "thumbsup" (car candidate))
                            (equal "thumbsup" (cdr candidate))))
                     candidates))
    (should (cl-some (lambda (candidate)
                       (and (string-match-p "pray" (car candidate))
                            (equal "pray" (cdr candidate))))
                     candidates))))

(ert-deftest test-slack-config-select-reaction-common-choice ()
  "Normal: selecting a common display candidate returns its Slack emoji name."
  (let* ((candidates (cj/slack--reaction-candidates))
         (choice (car (cl-find-if (lambda (candidate)
                                    (equal "pray" (cdr candidate)))
                                  candidates))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _args) choice)))
      (should (equal "pray" (cj/slack-select-reaction :team))))))

(ert-deftest test-slack-config-select-reaction-other-falls-back-to-upstream ()
  "Boundary: Other delegates to emacs-slack's full emoji input."
  (cl-letf (((symbol-function 'completing-read)
             (lambda (&rest _args) "Other..."))
            ((symbol-function 'slack-message-reaction-input)
             (lambda (team)
               (should (eq team :team))
               "custom_emoji")))
    (should (equal "custom_emoji" (cj/slack-select-reaction :team)))))

(ert-deftest test-slack-config-reaction-key-uses-local-command ()
  "Normal: C-; S ! uses the local safer reaction command."
  (should (eq (keymap-lookup cj/slack-keymap "!")
              #'cj/slack-message-add-reaction)))

(ert-deftest test-slack-config-message-add-reaction-dispatches-selected-reaction ()
  "Normal: local reaction command sends selected reaction to Slack."
  (let ((slack-current-buffer :buffer)
        called)
    (cl-letf (((symbol-function 'slack-buffer-team)
               (lambda (buffer)
                 (should (eq buffer :buffer))
                 :team))
              ((symbol-function 'cj/slack-select-reaction)
               (lambda (team)
                 (should (eq team :team))
                 "pray"))
              ((symbol-function 'slack-get-ts)
               (lambda () "123.456"))
              ((symbol-function 'slack-buffer-add-reaction-to-message)
               (lambda (buffer reaction ts)
                 (setq called (list buffer reaction ts)))))
      (cj/slack-message-add-reaction)
      (should (equal called '(:buffer "pray" "123.456"))))))

(ert-deftest test-slack-config-message-add-reaction-errors-outside-slack-buffer ()
  "Error: invoking the reaction command outside a Slack buffer fails clearly."
  (let ((slack-current-buffer nil))
    (should-error (cj/slack-message-add-reaction) :type 'user-error)))

(provide 'test-slack-config-reactions)
;;; test-slack-config-reactions.el ends here

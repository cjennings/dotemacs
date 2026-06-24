;;; test-ai-mcp-helpers.el --- Tests for pure helpers in ai-mcp.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Normal / Boundary / Error tests for the side-effect-free helpers in
;; ai-mcp.el: secrets redaction, confirm-policy classifier, description
;; normalizer, Claude-config reader (mtime-cached), env / secret-args
;; resolution, server-alist builder.  No real `~/.claude.json' reads;
;; fixtures are written to per-test temp files.  No real subprocesses
;; or network calls.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'ai-mcp)

;; -------------------------------------------------------- fixtures

(defconst test-ai-mcp--sentinel "REDACTED_TEST_SECRET"
  "Sentinel that must never appear in any user-facing output.")

(defconst test-ai-mcp--fixture-json
  "{
  \"mcpServers\": {
    \"drawio\": {
      \"type\": \"stdio\",
      \"command\": \"npx\",
      \"args\": [\"-y\", \"@drawio/mcp\"]
    },
    \"google-calendar\": {
      \"type\": \"stdio\",
      \"command\": \"npx\",
      \"args\": [\"-y\", \"@cocal/google-calendar-mcp\"],
      \"env\": {
        \"GOOGLE_OAUTH_CREDENTIALS\": \"REDACTED_TEST_SECRET\"
      }
    },
    \"google-docs-personal\": {
      \"type\": \"stdio\",
      \"command\": \"npx\",
      \"args\": [\"-y\", \"@a-bonus/google-docs-mcp\"],
      \"env\": {
        \"GOOGLE_CLIENT_ID\": \"REDACTED_TEST_SECRET\",
        \"GOOGLE_CLIENT_SECRET\": \"REDACTED_TEST_SECRET\",
        \"GOOGLE_MCP_PROFILE\": \"personal\"
      }
    },
    \"figma\": {
      \"type\": \"stdio\",
      \"command\": \"npx\",
      \"args\": [\"-y\", \"figma-developer-mcp\", \"--figma-api-key=REDACTED_TEST_SECRET\", \"--stdio\"]
    },
    \"linear\": {
      \"type\": \"http\",
      \"url\": \"https://mcp.linear.app/mcp\"
    },
    \"slack-deepsat\": {
      \"type\": \"sse\",
      \"url\": \"http://127.0.0.1:13080/sse\"
    }
  }
}"
  "Fixture matching the shape of a real ~/.claude.json mcpServers tree.")

(defun test-ai-mcp--write-fixture (&optional content)
  "Write CONTENT (defaults to the standard fixture) to a temp file.
Return the file path."
  (let ((tmp (make-temp-file "test-ai-mcp-" nil ".json")))
    (with-temp-file tmp
      (insert (or content test-ai-mcp--fixture-json)))
    tmp))

(defmacro test-ai-mcp--with-fixture (var &rest body)
  "Bind VAR to a fresh fixture file path and BODY-eval.  Clean up after."
  (declare (indent 1))
  `(let ((,var (test-ai-mcp--write-fixture))
         (cj/mcp--config-cache nil))
     (unwind-protect (progn ,@body)
       (when (file-exists-p ,var) (delete-file ,var)))))

;; -------------------------------------------------------- redact

(ert-deftest test-ai-mcp-redact-token-eq-normal ()
  "Normal: --token=VALUE has the value replaced by ***."
  (should (equal (cj/mcp--redact "--token=abc123") "--token=***")))

(ert-deftest test-ai-mcp-redact-token-spaced-boundary ()
  "Boundary: --token VALUE (space separator) is also redacted."
  (should (equal (cj/mcp--redact "--token abc123") "--token ***")))

(ert-deftest test-ai-mcp-redact-secret-flag-normal ()
  "Normal: --secret=VALUE is redacted."
  (should (equal (cj/mcp--redact "--secret=topsecret") "--secret=***")))

(ert-deftest test-ai-mcp-redact-password-flag-normal ()
  "Normal: --password=VALUE is redacted."
  (should (equal (cj/mcp--redact "--password=hunter2") "--password=***")))

(ert-deftest test-ai-mcp-redact-figma-api-key-normal ()
  "Normal: --figma-api-key=VALUE is redacted (covers the figma case)."
  (should (equal (cj/mcp--redact "--figma-api-key=figd_xyz")
                 "--figma-api-key=***")))

(ert-deftest test-ai-mcp-redact-authorization-header-normal ()
  "Normal: Authorization header value (scheme + token) is masked."
  (should (equal (cj/mcp--redact "Authorization: Bearer ghp_xyz123")
                 "Authorization: ***")))

(ert-deftest test-ai-mcp-redact-url-token-normal ()
  "Normal: ?token=VALUE in a URL is masked."
  (should (equal (cj/mcp--redact "https://api.example/v1?token=abc123&page=2")
                 "https://api.example/v1?token=***&page=2")))

(ert-deftest test-ai-mcp-redact-no-secrets-boundary ()
  "Boundary: a string with no known secrets is returned unchanged."
  (should (equal (cj/mcp--redact "hello world, nothing secret here")
                 "hello world, nothing secret here")))

(ert-deftest test-ai-mcp-redact-empty-string-boundary ()
  "Boundary: empty string returns empty string."
  (should (equal (cj/mcp--redact "") "")))

(ert-deftest test-ai-mcp-redact-multiple-secrets-boundary ()
  "Boundary: multiple secrets in one string are all redacted."
  (let* ((input "--token=abc --secret=xyz --password=qwe")
         (out (cj/mcp--redact input)))
    (should (equal out "--token=*** --secret=*** --password=***"))))

(ert-deftest test-ai-mcp-redact-nil-input-error ()
  "Error: nil input returns nil rather than signaling."
  (should (null (cj/mcp--redact nil))))

(ert-deftest test-ai-mcp-redact-sentinel-never-leaks ()
  "Sentinel REDACTED_TEST_SECRET is replaced wherever it lives in a secret slot."
  (dolist (input (list (format "--token=%s" test-ai-mcp--sentinel)
                       (format "--figma-api-key=%s" test-ai-mcp--sentinel)
                       (format "Authorization: Bearer %s" test-ai-mcp--sentinel)
                       (format "https://x/y?token=%s" test-ai-mcp--sentinel)))
    (let ((out (cj/mcp--redact input)))
      (should-not (string-match-p test-ai-mcp--sentinel out)))))

;; -------------------------------------------------------- confirm-p

(ert-deftest test-ai-mcp-confirm-p-write-pattern-normal ()
  "Normal: a write-prefixed tool name returns t."
  (should (cj/mcp--confirm-p "mcp__linear__create_issue")))

(ert-deftest test-ai-mcp-confirm-p-read-pattern-normal ()
  "Normal: a read-prefixed tool name returns nil."
  (should-not (cj/mcp--confirm-p "mcp__linear__list_issues")))

(ert-deftest test-ai-mcp-confirm-p-unknown-fails-closed-boundary ()
  "Boundary: a name matching neither read nor write defaults to t (fail closed)."
  (should (cj/mcp--confirm-p "mcp__linear__frobnicate")))

(ert-deftest test-ai-mcp-confirm-p-explicit-remote-name-boundary ()
  "Boundary: REMOTE-NAME arg overrides the prefix-strip of GPTEL-NAME."
  ;; The gptel-name claims read, but the explicit remote-name is a write
  ;; verb, so confirm should still fire.
  (should (cj/mcp--confirm-p "mcp__linear__list_issues" "create_issue")))

(ert-deftest test-ai-mcp-confirm-p-override-wins-boundary ()
  "Boundary: cj/mcp-tool-confirm-overrides wins over the classifier."
  (let ((cj/mcp-tool-confirm-overrides
         '(("mcp__linear__create_issue" . nil))))
    (should-not (cj/mcp--confirm-p "mcp__linear__create_issue"))))

;; -------------------------------------------------------- normalize-description

(ert-deftest test-ai-mcp-normalize-description-read-normal ()
  "Normal: a read tool gets the bare [SERVER] prefix."
  (should (equal
           (cj/mcp--normalize-description
            "linear"
            '(:name "list_issues" :description "List issues in a Linear team."))
           "[linear] List issues in a Linear team.")))

(ert-deftest test-ai-mcp-normalize-description-write-normal ()
  "Normal: a write tool gets [SERVER WRITE] prefix."
  (should (equal
           (cj/mcp--normalize-description
            "linear"
            '(:name "create_issue" :description "Create a new Linear issue."))
           "[linear WRITE] Create a new Linear issue.")))

(ert-deftest test-ai-mcp-normalize-description-unknown-boundary ()
  "Boundary: a tool matching neither classifier gets [SERVER ?] prefix."
  (should (equal
           (cj/mcp--normalize-description
            "google-keep"
            '(:name "frobnicate" :description "Do the frob thing."))
           "[google-keep ?] Do the frob thing.")))

(ert-deftest test-ai-mcp-normalize-description-missing-upstream-boundary ()
  "Boundary: missing upstream description falls back to a placeholder."
  (should (equal
           (cj/mcp--normalize-description
            "linear"
            '(:name "list_issues"))
           "[linear] (no description provided by server)")))

;; -------------------------------------------------------- read-claude-config

(ert-deftest test-ai-mcp-read-claude-config-good-fixture-normal ()
  "Normal: parsing a well-formed fixture returns :ok t and the parsed data."
  (test-ai-mcp--with-fixture path
    (let ((result (cj/mcp--read-claude-config path)))
      (should (plist-get result :ok))
      (should (plist-get (plist-get result :data) :mcpServers)))))

(ert-deftest test-ai-mcp-read-claude-config-missing-file-error ()
  "Error: missing file returns :ok nil with :reason missing-file."
  (let ((cj/mcp--config-cache nil)
        (path "/nonexistent/path/never-will-exist.json"))
    (let ((result (cj/mcp--read-claude-config path)))
      (should-not (plist-get result :ok))
      (should (eq (plist-get result :reason) 'missing-file)))))

(ert-deftest test-ai-mcp-read-claude-config-malformed-json-error ()
  "Error: malformed JSON returns :ok nil with :reason malformed-json and a message."
  (let ((cj/mcp--config-cache nil)
        (tmp (make-temp-file "test-ai-mcp-malformed-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "{ this is not valid json ::: "))
          (let ((result (cj/mcp--read-claude-config tmp)))
            (should-not (plist-get result :ok))
            (should (eq (plist-get result :reason) 'malformed-json))
            (should (stringp (plist-get result :message)))))
      (delete-file tmp))))

(ert-deftest test-ai-mcp-read-claude-config-empty-object-boundary ()
  "Boundary: an empty JSON object parses to ok with empty data plist."
  (let ((cj/mcp--config-cache nil)
        (tmp (make-temp-file "test-ai-mcp-empty-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "{}"))
          (let ((result (cj/mcp--read-claude-config tmp)))
            (should (plist-get result :ok))
            ;; :mcpServers is absent; plist-get returns nil.
            (should-not (plist-get (plist-get result :data) :mcpServers))))
      (delete-file tmp))))

(ert-deftest test-ai-mcp-read-claude-config-cache-hit-boundary ()
  "Boundary: a second read with the same mtime reuses the cache.
We detect cache reuse by mutating the cached :data alist after the first
read and verifying the second read returns the mutated value."
  (test-ai-mcp--with-fixture path
    (let* ((first (cj/mcp--read-claude-config path))
           (cache cj/mcp--config-cache))
      (should (plist-get first :ok))
      ;; Mutate the cached :data so a cache-hit returns the marker.
      (plist-put cache :data '(:sentinel cache-was-hit))
      (let ((second (cj/mcp--read-claude-config path)))
        (should (equal (plist-get second :data) '(:sentinel cache-was-hit)))))))

(ert-deftest test-ai-mcp-read-claude-config-cache-invalidate-on-mtime-boundary ()
  "Boundary: changing the file's mtime forces a reparse."
  (test-ai-mcp--with-fixture path
    (let* ((first (cj/mcp--read-claude-config path))
           (cache cj/mcp--config-cache))
      (should (plist-get first :ok))
      ;; Poison the cache, then bump mtime; the next read should reparse.
      (plist-put cache :data '(:sentinel cache-was-hit))
      (set-file-times path (time-add (current-time) 2))
      ;; Update the cache var since set-file-times changed file mtime.
      (setq cj/mcp--config-cache cache)
      (let ((second (cj/mcp--read-claude-config path)))
        ;; Real reparse should give us the real data, not the sentinel.
        (should (plist-get (plist-get second :data) :mcpServers))))))

(ert-deftest test-ai-mcp-read-claude-config-missing-mcpservers-boundary ()
  "Boundary: a valid JSON without :mcpServers parses but the subtree is nil."
  (let ((cj/mcp--config-cache nil)
        (tmp (make-temp-file "test-ai-mcp-no-mcp-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "{\"other\": 1}"))
          (let ((result (cj/mcp--read-claude-config tmp)))
            (should (plist-get result :ok))
            (should-not (plist-get (plist-get result :data) :mcpServers))))
      (delete-file tmp))))

;; -------------------------------------------------------- get-env / get-secret-arg

(ert-deftest test-ai-mcp-get-env-known-server-with-env-normal ()
  "Normal: env-bearing server returns its env plist."
  (test-ai-mcp--with-fixture path
    (let* ((cj/mcp-claude-config path)
           (env (cj/mcp--get-env "google-calendar")))
      (should (equal (plist-get env :GOOGLE_OAUTH_CREDENTIALS)
                     test-ai-mcp--sentinel)))))

(ert-deftest test-ai-mcp-get-env-known-server-without-env-boundary ()
  "Boundary: a server with no env subtree returns nil."
  (test-ai-mcp--with-fixture path
    (let* ((cj/mcp-claude-config path))
      (should-not (cj/mcp--get-env "drawio")))))

(ert-deftest test-ai-mcp-get-env-unknown-server-error ()
  "Error: unknown server returns nil without signaling."
  (test-ai-mcp--with-fixture path
    (let* ((cj/mcp-claude-config path))
      (should-not (cj/mcp--get-env "no-such-server")))))

(ert-deftest test-ai-mcp-get-secret-arg-figma-normal ()
  "Normal: figma's --figma-api-key= value is extracted from args."
  (test-ai-mcp--with-fixture path
    (let* ((cj/mcp-claude-config path)
           (value (cj/mcp--get-secret-arg "figma" "--figma-api-key")))
      (should (equal value test-ai-mcp--sentinel)))))

(ert-deftest test-ai-mcp-get-secret-arg-missing-flag-error ()
  "Error: a flag not in the server's args returns nil."
  (test-ai-mcp--with-fixture path
    (let* ((cj/mcp-claude-config path)
           (value (cj/mcp--get-secret-arg "figma" "--no-such-flag")))
      (should (null value)))))

;; -------------------------------------------------------- build-server-alist

(ert-deftest test-ai-mcp-build-server-alist-all-enabled-normal ()
  "Normal: with default specs and all-enabled list, alist has all 9 entries."
  (test-ai-mcp--with-fixture path
    (let* ((cj/mcp-claude-config path)
           (alist (cj/mcp--build-server-alist)))
      (should (= (length alist) 9))
      ;; Every name appears.
      (dolist (name '("linear" "notion" "figma" "slack-deepsat" "drawio"
                      "google-calendar" "google-docs-personal"
                      "google-docs-work" "google-keep"))
        (should (assoc name alist))))))

(ert-deftest test-ai-mcp-build-server-alist-filter-by-enabled-boundary ()
  "Boundary: enabled subset of names produces a subset alist."
  (test-ai-mcp--with-fixture path
    (let* ((cj/mcp-claude-config path)
           (alist (cj/mcp--build-server-alist
                   cj/mcp-server-specs
                   '("drawio" "linear"))))
      (should (= (length alist) 2))
      (should (assoc "drawio" alist))
      (should (assoc "linear" alist))
      (should-not (assoc "figma" alist)))))

(ert-deftest test-ai-mcp-build-server-alist-stdio-shape-normal ()
  "Normal: a stdio entry has :type, :command, :args (no :url)."
  (test-ai-mcp--with-fixture path
    (let* ((cj/mcp-claude-config path)
           (alist (cj/mcp--build-server-alist
                   cj/mcp-server-specs '("drawio"))))
      (let ((entry (cdr (assoc "drawio" alist))))
        (should (equal (plist-get entry :type) "stdio"))
        (should (equal (plist-get entry :command) "npx"))
        (should (listp (plist-get entry :args)))
        (should-not (plist-get entry :url))))))

(ert-deftest test-ai-mcp-build-server-alist-http-shape-normal ()
  "Normal: an http entry has :type and :url (no :command)."
  (test-ai-mcp--with-fixture path
    (let* ((cj/mcp-claude-config path)
           (alist (cj/mcp--build-server-alist
                   cj/mcp-server-specs '("linear"))))
      (let ((entry (cdr (assoc "linear" alist))))
        (should (equal (plist-get entry :type) "http"))
        (should (equal (plist-get entry :url) "https://mcp.linear.app/mcp"))
        (should-not (plist-get entry :command))))))

(ert-deftest test-ai-mcp-build-server-alist-sse-shape-normal ()
  "Normal: an sse entry has :type and :url."
  (test-ai-mcp--with-fixture path
    (let* ((cj/mcp-claude-config path)
           (alist (cj/mcp--build-server-alist
                   cj/mcp-server-specs '("slack-deepsat"))))
      (let ((entry (cdr (assoc "slack-deepsat" alist))))
        (should (equal (plist-get entry :type) "sse"))
        (should (equal (plist-get entry :url)
                       "http://127.0.0.1:13080/sse"))))))

(ert-deftest test-ai-mcp-build-server-alist-env-merge-normal ()
  "Normal: env-bearing server has its env plist merged into the entry."
  (test-ai-mcp--with-fixture path
    (let* ((cj/mcp-claude-config path)
           (alist (cj/mcp--build-server-alist
                   cj/mcp-server-specs '("google-calendar"))))
      (let* ((entry (cdr (assoc "google-calendar" alist)))
             (env (plist-get entry :env)))
        (should env)
        (should (equal (plist-get env :GOOGLE_OAUTH_CREDENTIALS)
                       test-ai-mcp--sentinel))))))

(ert-deftest test-ai-mcp-build-server-alist-secret-args-splice-normal ()
  "Normal: figma's --figma-api-key= is spliced into :args from Claude config."
  (test-ai-mcp--with-fixture path
    (let* ((cj/mcp-claude-config path)
           (alist (cj/mcp--build-server-alist
                   cj/mcp-server-specs '("figma"))))
      (let* ((entry (cdr (assoc "figma" alist)))
             (args (plist-get entry :args))
             (api-arg (cl-find-if
                       (lambda (a) (string-prefix-p "--figma-api-key=" a))
                       args)))
        (should api-arg)
        (should (equal api-arg (format "--figma-api-key=%s"
                                       test-ai-mcp--sentinel)))))))

(ert-deftest test-ai-mcp-build-server-alist-no-mutation-boundary ()
  "Boundary: building the alist does not mutate `cj/mcp-server-specs'."
  (test-ai-mcp--with-fixture path
    (let* ((cj/mcp-claude-config path)
           (snapshot (copy-tree cj/mcp-server-specs)))
      (cj/mcp--build-server-alist)
      (should (equal cj/mcp-server-specs snapshot)))))

(provide 'test-ai-mcp-helpers)
;;; test-ai-mcp-helpers.el ends here

;;; ai-mcp.el --- MCP server integration for GPTel -*- lexical-binding: t; coding: utf-8; -*-
;; Author: Craig Jennings <c@cjennings.net>
;; Maintainer: Craig Jennings <c@cjennings.net>
;; Version 0.1
;; Package-Requires: ((emacs "30.1") (mcp "0.1.0") (gptel "0.9.8"))
;; Keywords: convenience, tools, ai
;;
;;; Commentary:
;; Wires mcp.el's MCP server inventory into GPTel.  GPTel agents gain
;; access to the MCP servers Claude Code already uses (linear, notion,
;; figma, slack-deepsat, drawio, google-calendar, google-docs-personal,
;; google-docs-work, google-keep), with write-confirmation gating and a
;; doctor for diagnosing prerequisites.
;;
;; Design doc: docs/specs/mcp-el-gptel-integration-spec-doing.org
;;
;; File organization (seven sections, populated by phases):
;;   1. Constants and defcustoms         <- this phase
;;   2. Public commands                  <- later phase
;;   3. Pure helpers                     <- this phase
;;   4. mcp.el compatibility layer       <- later phase
;;   5. Registration pipeline            <- later phase
;;   6. Async state machine              <- later phase
;;   7. UI                               <- later phase

;;; Code:

(require 'cl-lib)
(require 'json)

;;;; --- 1. Constants and defcustoms -----------------------------------

(defgroup cj/ai-mcp nil
  "MCP server integration for GPTel."
  :group 'gptel
  :prefix "cj/")

(defcustom cj/mcp-claude-config
  (expand-file-name "~/.claude.json")
  "Path to the Claude Code config that holds MCP server env vars.
The config is read at server-spawn time and cached by mtime."
  :type 'file
  :group 'cj/ai-mcp)

(defconst cj/mcp-server-specs
  '((:name "linear"
     :transport http
     :url "https://mcp.linear.app/mcp"
     :auth in-protocol
     :risk write-capable)
    (:name "notion"
     :transport http
     :url "https://mcp.notion.com/mcp"
     :auth in-protocol
     :risk write-capable)
    (:name "figma"
     :transport stdio
     :command "npx"
     :args ("-y" "figma-developer-mcp" "--stdio")
     :secret-args ("--figma-api-key" :figma-api-key)
     :auth args-token
     :risk arg-leak)
    (:name "slack-deepsat"
     :transport sse
     :url "http://127.0.0.1:13080/sse"
     :auth local
     :risk write-capable)
    (:name "drawio"
     :transport stdio
     :command "npx"
     :args ("-y" "@drawio/mcp")
     :auth none
     :risk none)
    (:name "google-calendar"
     :transport stdio
     :command "npx"
     :args ("-y" "@cocal/google-calendar-mcp")
     :env (:GOOGLE_OAUTH_CREDENTIALS t)
     :auth oauth
     :risk write-capable)
    (:name "google-docs-personal"
     :transport stdio
     :command "npx"
     :args ("-y" "@a-bonus/google-docs-mcp")
     :env (:GOOGLE_CLIENT_ID t :GOOGLE_CLIENT_SECRET t :GOOGLE_MCP_PROFILE t)
     :auth oauth
     :risk write-capable)
    (:name "google-docs-work"
     :transport stdio
     :command "npx"
     :args ("-y" "@a-bonus/google-docs-mcp")
     :env (:GOOGLE_CLIENT_ID t :GOOGLE_CLIENT_SECRET t :GOOGLE_MCP_PROFILE t)
     :auth oauth
     :risk write-capable)
    (:name "google-keep"
     :transport stdio
     :command "uvx"
     :args ("--from" "keep-mcp" "python" "-m" "server.cli")
     :env (:GOOGLE_EMAIL t :GOOGLE_MASTER_TOKEN t)
     :auth token
     :risk write-capable))
  "Static, secret-free description of the MCP servers we wire to GPTel.
Each entry is a plist describing one server.  `:env' values are
placeholders (t) replaced at spawn time from `cj/mcp-claude-config'.
`:secret-args' (e.g. for figma) names the flag whose value is pulled
from the Claude config's args at spawn time.")

(defcustom cj/mcp-enabled-servers
  (mapcar (lambda (s) (plist-get s :name)) cj/mcp-server-specs)
  "List of MCP server names to start.
Defaults to every server in `cj/mcp-server-specs'.  Set to a
shorter list to disable specific servers without editing the
spec.  Changes take effect on next `cj/mcp-restart-failed' or
Emacs restart."
  :type '(repeat string)
  :group 'cj/ai-mcp)

(defcustom cj/mcp-start-on-entry-points
  '(toggle-gptel)
  "GPTel entry points that trigger MCP startup.
Symbols correspond to commands: `toggle-gptel', `gptel-send',
`gptel-quick-ask', `gptel-rewrite-with-directive',
`gptel-magit-generate-message'.  Default: only full chat
\(`toggle-gptel')."
  :type '(repeat symbol)
  :group 'cj/ai-mcp)

(defcustom cj/mcp-startup-timeout 30
  "Seconds before a still-starting MCP server is marked failed."
  :type 'integer
  :group 'cj/ai-mcp)

(defcustom cj/mcp-tool-timeout 60
  "Seconds before an in-flight MCP tool call times out."
  :type 'integer
  :group 'cj/ai-mcp)

(defcustom cj/mcp-tool-confirm-overrides nil
  "Per-tool confirmation overrides.
Alist mapping fully qualified MCP tool name (e.g.,
\"mcp__linear__create_issue\") to t or nil.  Wins over the
pattern-based classifier in `cj/mcp--confirm-p'."
  :type '(alist :key-type string :value-type boolean)
  :group 'cj/ai-mcp)

(defcustom cj/mcp-tool-audit-log-enabled t
  "When non-nil, append metadata for every MCP tool call to the audit log."
  :type 'boolean
  :group 'cj/ai-mcp)

;; Classifier patterns: name prefixes that indicate read vs write.

(defconst cj/mcp--write-name-patterns
  '("\\`create\\b" "\\`update\\b" "\\`delete\\b" "\\`remove\\b"
    "\\`send\\b" "\\`post\\b" "\\`add\\b" "\\`move\\b"
    "\\`invite\\b" "\\`share\\b" "\\`upload\\b" "\\`set\\b"
    "\\`patch\\b" "\\`import\\b" "\\`sync\\b" "\\`merge\\b"
    "\\`close\\b" "\\`reopen\\b" "\\`archive\\b" "\\`unarchive\\b"
    "\\`approve\\b" "\\`reject\\b" "\\`label\\b" "\\`assign\\b"
    "\\`reply\\b" "\\`comment\\b" "\\`trash\\b" "\\`restore\\b"
    "\\`pin\\b" "\\`unpin\\b" "\\`copy\\b" "\\`rename\\b")
  "Tool-name prefixes that indicate a write/mutate operation.
Matched after the `mcp__SERVER__' prefix is stripped.")

(defconst cj/mcp--read-name-patterns
  '("\\`get\\b" "\\`list\\b" "\\`read\\b" "\\`search\\b"
    "\\`find\\b" "\\`fetch\\b" "\\`view\\b" "\\`query\\b"
    "\\`describe\\b" "\\`show\\b" "\\`check\\b")
  "Tool-name prefixes that indicate a read-only operation.")

;; Secret-pattern list for redaction.  Each entry is (REGEX
;; . GROUP-NUMBER); the substring matched by GROUP-NUMBER is replaced
;; with "***".

(defconst cj/mcp--secret-redaction-patterns
  '(("\\(--token\\)\\(=\\|\\s-+\\)\\(\\S-+\\)" . 3)
    ("\\(--secret\\)\\(=\\|\\s-+\\)\\(\\S-+\\)" . 3)
    ("\\(--password\\)\\(=\\|\\s-+\\)\\(\\S-+\\)" . 3)
    ("\\(--figma-api-key\\)\\(=\\|\\s-+\\)\\(\\S-+\\)" . 3)
    ("\\(Authorization:\\s-*\\)\\(\\S-[^\"\n]*\\)" . 2)
    ("\\([?&]token=\\)\\([^&[:space:]\"]+\\)" . 2))
  "List of (REGEX . GROUP-NUMBER) for masking secrets in user-facing strings.
Applied in order by `cj/mcp--redact'.")

;;;; --- 3. Pure helpers -----------------------------------------------

;; ---- secrets redaction ----

(defun cj/mcp--redact (str)
  "Return STR with known secret patterns replaced by `***'.
Returns nil when STR is not a string.  See
`cj/mcp--secret-redaction-patterns' for the matched patterns."
  (when (stringp str)
    (let ((result str))
      (dolist (entry cj/mcp--secret-redaction-patterns result)
        (let ((re (car entry))
              (group (cdr entry))
              (start 0))
          (while (and (< start (length result))
                      (string-match re result start))
            (setq result
                  (concat (substring result 0 (match-beginning group))
                          "***"
                          (substring result (match-end group))))
            (setq start (+ (match-beginning group) 3))))))))

;; ---- confirm-policy classifier ----

(defun cj/mcp--strip-name-prefix (name)
  "Strip the `mcp__SERVER__' prefix from NAME, if present."
  (replace-regexp-in-string "\\`mcp__[^_]+__" "" name))

(defun cj/mcp--name-matches-p (name patterns)
  "Non-nil if NAME matches any regexp in PATTERNS."
  (cl-some (lambda (p) (string-match-p p name)) patterns))

(defun cj/mcp--confirm-p (gptel-name &optional remote-name)
  "Return non-nil if a tool should register with `:confirm t'.
GPTEL-NAME is the fully qualified `mcp__SERVER__TOOL' string.
REMOTE-NAME, if provided, overrides the prefix-strip of GPTEL-NAME.

Decision order:
1. `cj/mcp-tool-confirm-overrides' alist entry wins.
2. Bare name matches a write pattern → t.
3. Bare name matches a read pattern → nil.
4. Neither → t (fail closed)."
  (let ((override (assoc gptel-name cj/mcp-tool-confirm-overrides)))
    (cond
     (override (cdr override))
     (t
      (let ((bare (or remote-name (cj/mcp--strip-name-prefix gptel-name))))
        (cond
         ((cj/mcp--name-matches-p bare cj/mcp--write-name-patterns) t)
         ((cj/mcp--name-matches-p bare cj/mcp--read-name-patterns) nil)
         (t t)))))))

;; ---- description normalizer ----

(defun cj/mcp--normalize-description (server-name raw-tool)
  "Return a normalized description string for RAW-TOOL from SERVER-NAME.
Prefix `[SERVER]' for reads, `[SERVER WRITE]' for writes,
`[SERVER ?]' for unknown classification, then the upstream
description unchanged."
  (let* ((remote-name (plist-get raw-tool :name))
         (upstream (or (plist-get raw-tool :description)
                       "(no description provided by server)"))
         (suffix (cond
                  ((cj/mcp--name-matches-p remote-name
                                           cj/mcp--write-name-patterns)
                   " WRITE")
                  ((cj/mcp--name-matches-p remote-name
                                           cj/mcp--read-name-patterns)
                   "")
                  (t " ?"))))
    (format "[%s%s] %s" server-name suffix upstream)))

;; ---- Claude config reader (mtime-cached, structured returns) ----

(defvar cj/mcp--config-cache nil
  "Cache for the parsed Claude config.
Plist of (:path P :mtime M :data PARSED) or nil when empty.")

(defun cj/mcp--invalidate-config-cache ()
  "Force the next `cj/mcp--read-claude-config' call to reparse."
  (setq cj/mcp--config-cache nil))

(defun cj/mcp--read-claude-config (&optional path)
  "Return a structured plist describing the Claude config state.
PATH defaults to `cj/mcp-claude-config'.

Result shape:
  (:ok t :data PLIST)
  (:ok nil :reason missing-file)
  (:ok nil :reason unreadable)
  (:ok nil :reason malformed-json :message STR)

The parsed result is cached by (PATH, MTIME); subsequent calls
reparse only if the file has changed."
  (let ((path (or path cj/mcp-claude-config)))
    (cond
     ((not (file-exists-p path))
      (list :ok nil :reason 'missing-file))
     ((not (file-readable-p path))
      (list :ok nil :reason 'unreadable))
     (t
      (let ((mtime (file-attribute-modification-time
                    (file-attributes path))))
        (if (and cj/mcp--config-cache
                 (equal (plist-get cj/mcp--config-cache :path) path)
                 (equal (plist-get cj/mcp--config-cache :mtime) mtime))
            (list :ok t :data (plist-get cj/mcp--config-cache :data))
          (condition-case err
              (let* ((json-object-type 'plist)
                     (json-array-type 'list)
                     (data (with-temp-buffer
                             (insert-file-contents path)
                             (goto-char (point-min))
                             (json-read))))
                (setq cj/mcp--config-cache
                      (list :path path :mtime mtime :data data))
                (list :ok t :data data))
            (error
             (setq cj/mcp--config-cache nil)
             (list :ok nil :reason 'malformed-json
                   :message (error-message-string err))))))))))

;; ---- env / secret-args resolution ----

(defun cj/mcp--get-server-entry (server-name &optional config-result)
  "Return the parsed Claude-config entry plist for SERVER-NAME.
CONFIG-RESULT, if provided, is a return value from
`cj/mcp--read-claude-config' (avoids re-reading).  Returns nil
when the config is unavailable or SERVER-NAME is unknown."
  (let ((result (or config-result (cj/mcp--read-claude-config))))
    (when (plist-get result :ok)
      (let* ((data (plist-get result :data))
             (servers (plist-get data :mcpServers))
             (server-key (intern (concat ":" server-name))))
        (plist-get servers server-key)))))

(defun cj/mcp--get-env (server-name &optional config-result)
  "Return the env plist for SERVER-NAME from the parsed Claude config.
CONFIG-RESULT, if provided, is reused to avoid re-reading the
config.  Returns nil when the config is unavailable, the server
is unknown, or the server has no env section."
  (plist-get (cj/mcp--get-server-entry server-name config-result) :env))

(defun cj/mcp--get-secret-arg (server-name flag &optional config-result)
  "Return the secret value for SERVER-NAME's FLAG from the Claude config.
FLAG is the option name (e.g. \"--figma-api-key\").  Returns the
value following `FLAG=' in the server entry's args, or nil if
not found."
  (let* ((entry (cj/mcp--get-server-entry server-name config-result))
         (args (plist-get entry :args))
         (prefix (concat flag "=")))
    (cl-some
     (lambda (a)
       (when (and (stringp a) (string-prefix-p prefix a))
         (substring a (length prefix))))
     args)))

;; ---- server-alist builder (pure transform from specs + config) ----

(defun cj/mcp--resolve-env (env-spec server-name config-result)
  "Return a flat (KEY1 VAL1 KEY2 VAL2 ...) list for ENV-SPEC.
ENV-SPEC is a plist of `(:VAR1 t :VAR2 t)`.  Values come from
SERVER-NAME's env subtree in the parsed Claude config.  Vars
without a value are omitted."
  (let ((source-env (cj/mcp--get-env server-name config-result))
        (result nil))
    (cl-loop for (key _placeholder) on env-spec by #'cddr
             do (let ((value (plist-get source-env key)))
                  (when value
                    (push key result)
                    (push value result))))
    (nreverse result)))

(defun cj/mcp--resolve-args (args secret-args-spec server-name config-result)
  "Return ARGS with `:secret-args' placeholders filled in.
SECRET-ARGS-SPEC is (FLAG-STRING SLOT-KEYWORD).  When the value is
available in the Claude config, append `FLAG=VALUE' to ARGS;
otherwise return ARGS unchanged."
  (if (not secret-args-spec)
      args
    (let* ((flag (car secret-args-spec))
           (value (cj/mcp--get-secret-arg server-name flag config-result)))
      (if value
          (append args (list (format "%s=%s" flag value)))
        args))))

(defun cj/mcp--spec-to-alist-entry (spec config-result)
  "Translate one SPEC plist into a `(NAME . PLIST)' alist entry.
Pulls env values from CONFIG-RESULT; splices `:secret-args' into
`:args' for stdio specs that declare one."
  (let* ((name (plist-get spec :name))
         (transport (plist-get spec :transport))
         (entry (list :type (symbol-name transport)))
         (env-spec (plist-get spec :env))
         (secret-args-spec (plist-get spec :secret-args)))
    (pcase transport
      ('stdio
       (setq entry (append entry
                           (list :command (plist-get spec :command)
                                 :args (cj/mcp--resolve-args
                                        (plist-get spec :args)
                                        secret-args-spec
                                        name
                                        config-result)))))
      ((or 'http 'sse)
       (setq entry (append entry
                           (list :url (plist-get spec :url))))))
    (when env-spec
      (let ((env-pairs (cj/mcp--resolve-env env-spec name config-result)))
        (when env-pairs
          (setq entry (append entry (list :env env-pairs))))))
    (cons name entry)))

(defun cj/mcp--build-server-alist (&optional specs enabled-names config-result)
  "Return an alist suitable for `mcp-hub-servers'.
SPECS defaults to `cj/mcp-server-specs'.  ENABLED-NAMES defaults
to `cj/mcp-enabled-servers'.  CONFIG-RESULT, if provided, is a
parsed Claude-config result (reused for env/secret resolution).
Does not mutate SPECS."
  (let* ((specs (or specs cj/mcp-server-specs))
         (enabled-names (or enabled-names cj/mcp-enabled-servers))
         (config-result (or config-result (cj/mcp--read-claude-config))))
    (delq nil
          (mapcar
           (lambda (spec)
             (let ((name (plist-get spec :name)))
               (when (member name enabled-names)
                 (cj/mcp--spec-to-alist-entry spec config-result))))
           specs))))

(provide 'ai-mcp)
;;; ai-mcp.el ends here

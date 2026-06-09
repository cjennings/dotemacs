import json, os
HERE=os.path.dirname(os.path.abspath(__file__))
# Pure color-math core, inlined verbatim into the page so the browser runs the
# same code the Node tests import (one source of truth). Strip the ES-module
# `export` line(s) — a top-level export is a syntax error in a classic <script>.
# test-colormath.mjs applies the identical strip and asserts the page carries this
# body verbatim (inline-integrity), so the two copies cannot drift.
COLORMATH_BODY='\n'.join(
    l for l in open(os.path.join(HERE,'colormath.js')).read().splitlines()
    if not l.startswith('export')).rstrip()
ns={}
src=open(os.path.join(HERE,'samples.py')).read()
exec(src[:src.index('cols=')], ns)
SAMPLES={"Elisp":ns['ELS'],"Go":ns['GOS'],"Python":ns['PYS'],"TypeScript":ns['TSS'],"Java":ns['JAS'],"C":ns['CS'],"C++":ns['CPS'],"Shell":ns['SHS']}
COLS=ns['COLS']
MAP={k:v[0] for k,v in COLS.items()}; BOLD={k:v[1] for k,v in COLS.items()}; MAP['str']='#5d9b86'; MAP['bg']='#000000'
PALETTE=[["#67809c","blue"],["#e8bd30","gold"],["#9b5fd0","regal"],["#2ba178","emerald"],["#5d9b86","sage"],
 ["#cb6b4d","terracotta"],["#be9e74","tan"],["#ffffff","white"],["#a9b2bb","silver"],["#838d97","steel"],
 ["#5e6770","pewter"],["#2f343a","gunmetal"],["#264364","navy"],["#000000","ground"],["#1a1714","bg-dim"]]
CATS=[["bg","background (ground)","Aa Bb 123"],["p","fg · default text","other / whitespace"],["kw","keyword","class  def  if  return"],["bi","builtin","len  echo  printf"],
 ["pp","preprocessor","#include  #define"],["fnd","function · def","resolve  push"],
 ["fnc","function · call","printf  rsync  get"],["dec","decorator","@dataclass"],
 ["ty","type / class","int  str  Order  Queue"],["prop","property / field","id  name  items"],
 ["con","constant","None  nil  NULL  true"],["num","number","8080  100  -1"],
 ["str","string",'"dupre"  "fmt"'],["esc","escape","\\n  \\t"],["re","regexp","/^#[0-9a-f]+/"],
 ["doc","docstring",'"""..."""'],["cm","comment","# reject nil"],["cmd","comment delim","#  //  ;;"],
 ["var","variable / use","value  key  self"],["op","operator",":  =  ->  =="],
 ["punc","punctuation","{ }  ( )  ;"]]
UI_FACES=[["cursor","cursor","Aa|"],["region","region (selection)","selected text"],
 ["hl-line","hl-line (current line)","current line"],["highlight","highlight","hover"],
 ["mode-line","mode-line","status active"],["mode-line-inactive","mode-line-inactive","status idle"],
 ["fringe","fringe","| |"],["line-number","line-number","  42"],
 ["line-number-current-line","line-number-current-line","> 42"],["minibuffer-prompt","minibuffer-prompt","M-x "],
 ["isearch","isearch (match)","match"],["lazy-highlight","lazy-highlight","other match"],
 ["isearch-fail","isearch-fail","no match"],["show-paren-match","show-paren-match","( )"],
 ["show-paren-mismatch","show-paren-mismatch",") ("],["link","link","https://"],
 ["error","error","error!"],["warning","warning","warning"],
 ["success","success","ok"],["vertical-border","vertical-border","|"]]
UIMAP={"cursor":{"fg":None,"bg":"#a9b2bb"},"region":{"fg":None,"bg":"#264364"},
 "hl-line":{"fg":None,"bg":"#1a1714"},"highlight":{"fg":None,"bg":"#2f343a"},
 "mode-line":{"fg":"#cdced1","bg":"#2f343a"},"mode-line-inactive":{"fg":"#838d97","bg":"#1a1714"},
 "fringe":{"fg":None,"bg":"#0d0b0a"},"line-number":{"fg":"#5e6770","bg":None},
 "line-number-current-line":{"fg":"#e8bd30","bg":"#1a1714"},"minibuffer-prompt":{"fg":"#67809c","bg":None},
 "isearch":{"fg":"#0d0b0a","bg":"#e8bd30"},"lazy-highlight":{"fg":"#0d0b0a","bg":"#838d97"},
 "isearch-fail":{"fg":"#cb6b4d","bg":None},"show-paren-match":{"fg":None,"bg":"#264364"},
 "show-paren-mismatch":{"fg":"#0d0b0a","bg":"#cb6b4d"},"link":{"fg":"#67809c","bg":None},
 "error":{"fg":"#cb6b4d","bg":None},"warning":{"fg":"#e8bd30","bg":None},
 "success":{"fg":"#5d9b86","bg":None},"vertical-border":{"fg":"#2f343a","bg":None}}
# link is underlined by default (matches the built-in link face).
UIMAP["link"]["underline"]=True
# Tier-3 package faces (Phase 2): complete own-defface sets for org/magit/elfeed,
# built from face-name lists + a curated seed-color map. Prominent faces are
# seeded; the long tail seeds to the default foreground for the user to tune.
ORG_FACES=("org-document-title org-document-info org-document-info-keyword "
 "org-level-1 org-level-2 org-level-3 org-level-4 org-level-5 org-level-6 org-level-7 org-level-8 "
 "org-headline-todo org-headline-done org-todo org-done org-priority org-tag org-tag-group "
 "org-special-keyword org-drawer org-property-value org-checkbox org-checkbox-statistics-todo "
 "org-checkbox-statistics-done org-warning org-link org-footnote org-date org-sexp-date "
 "org-date-selected org-target org-macro org-cite org-cite-key org-block org-block-begin-line "
 "org-block-end-line org-code org-verbatim org-inline-src-block org-quote org-verse "
 "org-latex-and-related org-table org-table-header org-table-row org-formula org-column "
 "org-column-title org-list-dt org-meta-line org-ellipsis org-hide org-indent org-archived "
 "org-default org-dispatcher-highlight org-agenda-structure org-agenda-structure-secondary "
 "org-agenda-structure-filter org-agenda-date org-agenda-date-today org-agenda-date-weekend "
 "org-agenda-date-weekend-today org-agenda-current-time org-agenda-done org-agenda-dimmed-todo-face "
 "org-agenda-calendar-event org-agenda-calendar-sexp org-agenda-calendar-daterange org-agenda-diary "
 "org-agenda-clocking org-agenda-column-dateline org-agenda-restriction-lock org-agenda-filter-category "
 "org-agenda-filter-effort org-agenda-filter-regexp org-agenda-filter-tags org-scheduled "
 "org-scheduled-today org-scheduled-previously org-upcoming-deadline org-upcoming-distant-deadline "
 "org-imminent-deadline org-time-grid org-clock-overlay org-mode-line-clock org-mode-line-clock-overrun").split()
MAGIT_FACES=("magit-section-heading magit-section-secondary-heading magit-section-heading-selection "
 "magit-section-highlight magit-section-child-count magit-diff-added magit-diff-added-highlight "
 "magit-diff-removed magit-diff-removed-highlight magit-diff-context magit-diff-context-highlight "
 "magit-diff-file-heading magit-diff-file-heading-highlight magit-diff-file-heading-selection "
 "magit-diff-hunk-heading magit-diff-hunk-heading-highlight magit-diff-hunk-heading-selection "
 "magit-diff-hunk-region magit-diff-lines-heading magit-diff-lines-boundary magit-diff-base "
 "magit-diff-base-highlight magit-diff-our magit-diff-our-highlight magit-diff-their "
 "magit-diff-their-highlight magit-diff-conflict-heading magit-diff-conflict-heading-highlight "
 "magit-diff-revision-summary magit-diff-revision-summary-highlight magit-diff-whitespace-warning "
 "magit-diffstat-added magit-diffstat-removed magit-branch-current magit-branch-local "
 "magit-branch-remote magit-branch-remote-head magit-branch-upstream magit-branch-warning "
 "magit-head magit-tag magit-hash magit-filename magit-dimmed magit-keyword magit-keyword-squash "
 "magit-refname magit-refname-stash magit-refname-wip magit-refname-pullreq magit-log-author "
 "magit-log-date magit-log-graph magit-header-line magit-header-line-key magit-header-line-log-select "
 "magit-process-ok magit-process-ng magit-mode-line-process magit-mode-line-process-error "
 "magit-bisect-good magit-bisect-bad magit-bisect-skip magit-blame-heading magit-blame-highlight "
 "magit-blame-hash magit-blame-name magit-blame-date magit-blame-summary magit-blame-dimmed "
 "magit-blame-margin magit-cherry-equivalent magit-cherry-unmatched magit-signature-good "
 "magit-signature-bad magit-signature-untrusted magit-signature-expired magit-signature-expired-key "
 "magit-signature-revoked magit-signature-error magit-reflog-commit magit-reflog-amend "
 "magit-reflog-merge magit-reflog-checkout magit-reflog-reset magit-reflog-rebase "
 "magit-reflog-cherry-pick magit-reflog-remote magit-reflog-other magit-sequence-pick "
 "magit-sequence-stop magit-sequence-part magit-sequence-head magit-sequence-drop magit-sequence-done "
 "magit-sequence-onto magit-sequence-exec magit-left-margin").split()
ELFEED_FACES=("elfeed-search-date-face elfeed-search-title-face elfeed-search-unread-title-face "
 "elfeed-search-feed-face elfeed-search-tag-face elfeed-search-unread-count-face "
 "elfeed-search-filter-face elfeed-search-last-update-face elfeed-log-date-face "
 "elfeed-log-error-level-face elfeed-log-warn-level-face elfeed-log-info-level-face "
 "elfeed-log-debug-level-face").split()
ORG_SEED={
 "org-document-title":{"fg":"gold","bold":True,"height":1.5},"org-document-info":{"fg":"steel"},
 "org-document-info-keyword":{"fg":"pewter","inherit":"fixed-pitch"},
 "org-level-1":{"fg":"blue","bold":True,"height":1.3},"org-level-2":{"fg":"gold","height":1.2},
 "org-level-3":{"fg":"regal","height":1.15},"org-level-4":{"fg":"emerald","height":1.1},
 "org-level-5":{"fg":"terracotta"},"org-level-6":{"fg":"tan"},"org-level-7":{"fg":"sage"},"org-level-8":{"fg":"steel"},
 "org-headline-done":{"fg":"pewter"},"org-todo":{"fg":"terracotta","bold":True},"org-done":{"fg":"sage","bold":True},
 "org-priority":{"fg":"gold","bold":True},"org-tag":{"fg":"tan"},"org-tag-group":{"fg":"tan"},
 "org-special-keyword":{"fg":"pewter"},"org-drawer":{"fg":"pewter"},"org-property-value":{"fg":"steel"},
 "org-checkbox":{"fg":"gold","inherit":"fixed-pitch"},"org-checkbox-statistics-todo":{"fg":"terracotta"},
 "org-checkbox-statistics-done":{"fg":"sage"},"org-warning":{"fg":"terracotta","bold":True},
 "org-link":{"fg":"blue"},"org-footnote":{"fg":"blue"},"org-date":{"fg":"steel","inherit":"fixed-pitch"},
 "org-sexp-date":{"fg":"steel"},"org-date-selected":{"fg":"ground","bg":"gold"},"org-target":{"fg":"regal"},
 "org-macro":{"fg":"regal"},"org-cite":{"fg":"blue"},"org-cite-key":{"fg":"blue"},
 "org-block":{"fg":"white","bg":"bg-dim","inherit":"fixed-pitch"},
 "org-block-begin-line":{"fg":"pewter","bg":"bg-dim","inherit":"fixed-pitch"},
 "org-block-end-line":{"fg":"pewter","bg":"bg-dim","inherit":"fixed-pitch"},
 "org-code":{"fg":"terracotta","inherit":"fixed-pitch"},"org-verbatim":{"fg":"steel","inherit":"fixed-pitch"},
 "org-inline-src-block":{"fg":"terracotta","inherit":"fixed-pitch"},"org-quote":{"fg":"silver","italic":True},
 "org-verse":{"fg":"silver","italic":True},"org-latex-and-related":{"fg":"gold"},
 "org-table":{"fg":"steel","inherit":"fixed-pitch"},"org-table-header":{"fg":"white","bold":True,"bg":"gunmetal"},
 "org-formula":{"fg":"terracotta"},"org-column":{"bg":"gunmetal"},"org-column-title":{"fg":"white","bold":True,"bg":"gunmetal"},
 "org-list-dt":{"fg":"gold","bold":True},"org-meta-line":{"fg":"pewter","inherit":"fixed-pitch"},
 "org-ellipsis":{"fg":"pewter"},"org-hide":{"fg":"ground"},"org-indent":{"fg":"ground"},
 "org-archived":{"fg":"pewter"},"org-dispatcher-highlight":{"fg":"gold","bold":True,"bg":"navy"},
 "org-agenda-structure":{"fg":"blue","bold":True,"height":1.1},"org-agenda-structure-secondary":{"fg":"blue"},
 "org-agenda-structure-filter":{"fg":"terracotta","bold":True},"org-agenda-date":{"fg":"steel","height":1.05},
 "org-agenda-date-today":{"fg":"gold","bold":True,"height":1.05},"org-agenda-date-weekend":{"fg":"steel","bold":True},
 "org-agenda-date-weekend-today":{"fg":"gold","bold":True},"org-agenda-current-time":{"fg":"gold"},
 "org-agenda-done":{"fg":"sage"},"org-agenda-dimmed-todo-face":{"fg":"pewter"},
 "org-agenda-calendar-event":{"fg":"white"},"org-agenda-calendar-sexp":{"fg":"steel"},
 "org-agenda-calendar-daterange":{"fg":"steel"},"org-agenda-diary":{"fg":"sage"},
 "org-agenda-clocking":{"bg":"navy"},"org-agenda-column-dateline":{"bg":"gunmetal"},
 "org-agenda-restriction-lock":{"bg":"terracotta"},"org-agenda-filter-category":{"fg":"gold","bold":True},
 "org-agenda-filter-effort":{"fg":"gold","bold":True},"org-agenda-filter-regexp":{"fg":"gold","bold":True},
 "org-agenda-filter-tags":{"fg":"gold","bold":True},"org-scheduled":{"fg":"sage"},
 "org-scheduled-today":{"fg":"sage","bold":True},"org-scheduled-previously":{"fg":"terracotta"},
 "org-upcoming-deadline":{"fg":"gold"},"org-upcoming-distant-deadline":{"fg":"tan"},
 "org-imminent-deadline":{"fg":"terracotta","bold":True},"org-time-grid":{"fg":"tan"},
 "org-clock-overlay":{"bg":"navy"},"org-mode-line-clock":{"fg":"steel"},"org-mode-line-clock-overrun":{"fg":"terracotta","bold":True}}
MAGIT_SEED={
 "magit-section-heading":{"fg":"gold","bold":True},"magit-section-secondary-heading":{"fg":"tan","bold":True},
 "magit-section-heading-selection":{"fg":"gold","bg":"navy"},"magit-section-highlight":{"bg":"bg-dim"},
 "magit-section-child-count":{"fg":"pewter"},"magit-diff-added":{"fg":"sage"},
 "magit-diff-added-highlight":{"fg":"sage","bg":"bg-dim"},"magit-diff-removed":{"fg":"terracotta"},
 "magit-diff-removed-highlight":{"fg":"terracotta","bg":"bg-dim"},"magit-diff-context":{"fg":"pewter"},
 "magit-diff-context-highlight":{"fg":"silver","bg":"bg-dim"},"magit-diff-file-heading":{"fg":"white","bold":True},
 "magit-diff-file-heading-highlight":{"fg":"white","bold":True,"bg":"bg-dim"},
 "magit-diff-hunk-heading":{"fg":"steel","bg":"gunmetal"},"magit-diff-hunk-heading-highlight":{"fg":"white","bg":"gunmetal"},
 "magit-diffstat-added":{"fg":"sage"},"magit-diffstat-removed":{"fg":"terracotta"},
 "magit-branch-current":{"fg":"blue","bold":True},"magit-branch-local":{"fg":"blue"},
 "magit-branch-remote":{"fg":"sage"},"magit-branch-remote-head":{"fg":"sage","bold":True},
 "magit-head":{"fg":"blue","bold":True},"magit-tag":{"fg":"gold"},"magit-hash":{"fg":"pewter"},
 "magit-filename":{"fg":"steel"},"magit-dimmed":{"fg":"pewter"},"magit-keyword":{"fg":"regal"},
 "magit-keyword-squash":{"fg":"terracotta"},"magit-refname":{"fg":"pewter"},"magit-log-author":{"fg":"tan"},
 "magit-log-date":{"fg":"steel"},"magit-log-graph":{"fg":"pewter"},
 "magit-header-line":{"fg":"white","bold":True,"bg":"gunmetal"},"magit-process-ok":{"fg":"sage","bold":True},
 "magit-process-ng":{"fg":"terracotta","bold":True},"magit-mode-line-process":{"fg":"sage"},
 "magit-mode-line-process-error":{"fg":"terracotta"},"magit-bisect-good":{"fg":"sage"},
 "magit-bisect-bad":{"fg":"terracotta"},"magit-bisect-skip":{"fg":"gold"},
 "magit-blame-heading":{"fg":"steel","bg":"gunmetal"},"magit-blame-hash":{"fg":"pewter"},
 "magit-blame-name":{"fg":"tan"},"magit-blame-date":{"fg":"steel"},"magit-cherry-equivalent":{"fg":"regal"},
 "magit-cherry-unmatched":{"fg":"sage"},"magit-signature-good":{"fg":"sage"},
 "magit-signature-bad":{"fg":"terracotta","bold":True},"magit-signature-untrusted":{"fg":"gold"},
 "magit-signature-expired":{"fg":"tan"},"magit-diff-whitespace-warning":{"bg":"terracotta"},
 "magit-reflog-commit":{"fg":"sage"},"magit-reflog-amend":{"fg":"regal"},"magit-reflog-merge":{"fg":"sage"},
 "magit-reflog-checkout":{"fg":"blue"},"magit-reflog-reset":{"fg":"terracotta"},"magit-reflog-rebase":{"fg":"regal"},
 "magit-reflog-cherry-pick":{"fg":"sage"},"magit-reflog-remote":{"fg":"steel"},"magit-reflog-other":{"fg":"steel"},
 "magit-sequence-pick":{"fg":"white"},"magit-sequence-stop":{"fg":"terracotta"},"magit-sequence-done":{"fg":"pewter"},
 "magit-sequence-head":{"fg":"blue"}}
ELFEED_SEED={
 "elfeed-search-date-face":{"fg":"steel"},"elfeed-search-title-face":{"fg":"silver"},
 "elfeed-search-unread-title-face":{"fg":"white","bold":True},"elfeed-search-feed-face":{"fg":"sage"},
 "elfeed-search-tag-face":{"fg":"tan"},"elfeed-search-unread-count-face":{"fg":"gold"},
 "elfeed-search-filter-face":{"fg":"blue","bold":True},"elfeed-search-last-update-face":{"fg":"pewter"},
 "elfeed-log-date-face":{"fg":"steel"},"elfeed-log-error-level-face":{"fg":"terracotta","bold":True},
 "elfeed-log-warn-level-face":{"fg":"gold"},"elfeed-log-info-level-face":{"fg":"sage"},
 "elfeed-log-debug-level-face":{"fg":"pewter"}}
# ghostel (terminal): the 16 ANSI colors plus default and the fake cursor.
GHOSTEL_FACES=("ghostel-default ghostel-fake-cursor ghostel-fake-cursor-box "
 "ghostel-color-black ghostel-color-red ghostel-color-green ghostel-color-yellow "
 "ghostel-color-blue ghostel-color-magenta ghostel-color-cyan ghostel-color-white "
 "ghostel-color-bright-black ghostel-color-bright-red ghostel-color-bright-green ghostel-color-bright-yellow "
 "ghostel-color-bright-blue ghostel-color-bright-magenta ghostel-color-bright-cyan ghostel-color-bright-white").split()
GHOSTEL_SEED={
 "ghostel-default":{"fg":"#cdced1"},"ghostel-fake-cursor":{"fg":"#000000","bg":"silver"},"ghostel-fake-cursor-box":{"fg":"silver"},
 "ghostel-color-black":{"fg":"pewter"},"ghostel-color-red":{"fg":"terracotta"},"ghostel-color-green":{"fg":"emerald"},"ghostel-color-yellow":{"fg":"gold"},
 "ghostel-color-blue":{"fg":"blue"},"ghostel-color-magenta":{"fg":"regal"},"ghostel-color-cyan":{"fg":"sage"},"ghostel-color-white":{"fg":"silver"},
 "ghostel-color-bright-black":{"fg":"steel"},"ghostel-color-bright-red":{"fg":"#de4949"},"ghostel-color-bright-green":{"fg":"#84b068"},"ghostel-color-bright-yellow":{"fg":"#eed376"},
 "ghostel-color-bright-blue":{"fg":"#7a9abe"},"ghostel-color-bright-magenta":{"fg":"#b07fd0"},"ghostel-color-bright-cyan":{"fg":"#7fc0a8"},"ghostel-color-bright-white":{"fg":"white"}}
DASHBOARD_FACES=("dashboard-banner-logo-title dashboard-text-banner dashboard-heading "
 "dashboard-items-face dashboard-navigator dashboard-no-items-face dashboard-footer-face dashboard-footer-icon-face").split()
DASHBOARD_SEED={
 "dashboard-banner-logo-title":{"fg":"gold","bold":True},"dashboard-text-banner":{"fg":"steel"},"dashboard-heading":{"fg":"blue","bold":True},
 "dashboard-items-face":{"fg":"#cdced1"},"dashboard-navigator":{"fg":"blue"},"dashboard-no-items-face":{"fg":"pewter"},
 "dashboard-footer-face":{"fg":"tan"},"dashboard-footer-icon-face":{"fg":"gold"}}
# mu4e is not in the generated inventory (not loaded when it was built), so its
# face list is curated from the set the dupre theme already themes.
MU4E_FACES=("mu4e-title-face mu4e-context-face mu4e-modeline-face mu4e-ok-face mu4e-warning-face "
 "mu4e-header-title-face mu4e-header-key-face mu4e-header-value-face mu4e-header-face mu4e-header-highlight-face mu4e-header-marks-face "
 "mu4e-unread-face mu4e-flagged-face mu4e-replied-face mu4e-forwarded-face mu4e-draft-face mu4e-trashed-face mu4e-moved-face mu4e-related-face "
 "mu4e-contact-face mu4e-special-header-value-face mu4e-attach-number-face mu4e-url-number-face mu4e-link-face "
 "mu4e-cited-1-face mu4e-cited-2-face mu4e-cited-3-face mu4e-cited-4-face mu4e-cited-5-face mu4e-cited-6-face mu4e-cited-7-face "
 "mu4e-footer-face mu4e-region-code mu4e-system-face mu4e-highlight-face mu4e-compose-header-face mu4e-compose-separator-face").split()
MU4E_SEED={
 "mu4e-title-face":{"fg":"blue","bold":True},"mu4e-context-face":{"fg":"blue","bold":True},"mu4e-modeline-face":{"fg":"silver"},"mu4e-ok-face":{"fg":"sage","bold":True},"mu4e-warning-face":{"fg":"gold","bold":True},
 "mu4e-header-title-face":{"fg":"blue","bold":True},"mu4e-header-key-face":{"fg":"blue","bold":True},"mu4e-header-value-face":{"fg":"silver"},"mu4e-header-face":{"fg":"#cdced1"},"mu4e-header-highlight-face":{"bg":"gunmetal"},"mu4e-header-marks-face":{"fg":"gold"},
 "mu4e-unread-face":{"fg":"white","bold":True},"mu4e-flagged-face":{"fg":"gold"},"mu4e-replied-face":{"fg":"silver"},"mu4e-forwarded-face":{"fg":"silver"},"mu4e-draft-face":{"fg":"steel","italic":True},"mu4e-trashed-face":{"fg":"pewter","strike":True},"mu4e-moved-face":{"fg":"steel","italic":True},"mu4e-related-face":{"fg":"steel","italic":True},
 "mu4e-contact-face":{"fg":"#cdced1"},"mu4e-special-header-value-face":{"fg":"silver"},"mu4e-attach-number-face":{"fg":"blue","bold":True},"mu4e-url-number-face":{"fg":"blue","bold":True},"mu4e-link-face":{"fg":"blue","underline":True},
 "mu4e-cited-1-face":{"fg":"silver"},"mu4e-cited-2-face":{"fg":"steel"},"mu4e-cited-3-face":{"fg":"sage"},"mu4e-cited-4-face":{"fg":"pewter"},"mu4e-cited-5-face":{"fg":"tan"},"mu4e-cited-6-face":{"fg":"terracotta"},"mu4e-cited-7-face":{"fg":"regal"},
 "mu4e-footer-face":{"fg":"pewter"},"mu4e-region-code":{"bg":"bg-dim"},"mu4e-system-face":{"fg":"pewter","italic":True},"mu4e-highlight-face":{"fg":"gold","bold":True},"mu4e-compose-header-face":{"fg":"blue","bold":True},"mu4e-compose-separator-face":{"fg":"pewter"}}
LSP_FACES=("lsp-signature-face lsp-signature-highlight-function-argument lsp-signature-posframe "
 "lsp-face-highlight-read lsp-face-highlight-write lsp-face-highlight-textual lsp-face-rename lsp-rename-placeholder-face "
 "lsp-inlay-hint-face lsp-inlay-hint-parameter-face lsp-inlay-hint-type-face lsp-details-face "
 "lsp-installation-buffer-face lsp-installation-finished-buffer-face").split()
LSP_SEED={
 "lsp-signature-face":{"fg":"silver"},"lsp-signature-highlight-function-argument":{"fg":"gold","bold":True},"lsp-signature-posframe":{"bg":"bg-dim"},
 "lsp-face-highlight-read":{"bg":"navy"},"lsp-face-highlight-write":{"bg":"#3d2f4a"},"lsp-face-highlight-textual":{"bg":"gunmetal"},
 "lsp-face-rename":{"bg":"gunmetal","bold":True},"lsp-rename-placeholder-face":{"fg":"gold","bold":True},
 "lsp-inlay-hint-face":{"fg":"pewter","italic":True},"lsp-inlay-hint-parameter-face":{"fg":"steel","italic":True},"lsp-inlay-hint-type-face":{"fg":"sage","italic":True},
 "lsp-details-face":{"fg":"pewter","italic":True},"lsp-installation-buffer-face":{"fg":"blue"},"lsp-installation-finished-buffer-face":{"fg":"sage"}}
GITGUTTER_FACES=("git-gutter:added git-gutter:modified git-gutter:deleted git-gutter:unchanged git-gutter:separator").split()
GITGUTTER_SEED={
 "git-gutter:added":{"fg":"emerald"},"git-gutter:modified":{"fg":"gold"},"git-gutter:deleted":{"fg":"terracotta"},
 "git-gutter:unchanged":{"fg":"pewter"},"git-gutter:separator":{"fg":"steel"}}
FLYCHECK_FACES=("flycheck-error flycheck-warning flycheck-info flycheck-fringe-error flycheck-fringe-warning flycheck-fringe-info "
 "flycheck-delimited-error flycheck-error-delimiter flycheck-error-list-error flycheck-error-list-warning flycheck-error-list-info "
 "flycheck-error-list-error-message flycheck-error-list-checker-name flycheck-error-list-column-number flycheck-error-list-line-number "
 "flycheck-error-list-filename flycheck-error-list-id flycheck-error-list-id-with-explainer flycheck-error-list-highlight flycheck-verify-select-checker").split()
FLYCHECK_SEED={
 "flycheck-error":{"fg":"terracotta"},"flycheck-warning":{"fg":"gold"},"flycheck-info":{"fg":"blue"},
 "flycheck-fringe-error":{"fg":"terracotta"},"flycheck-fringe-warning":{"fg":"gold"},"flycheck-fringe-info":{"fg":"blue"},
 "flycheck-delimited-error":{"fg":"terracotta"},"flycheck-error-delimiter":{"fg":"terracotta"},
 "flycheck-error-list-error":{"fg":"terracotta"},"flycheck-error-list-warning":{"fg":"gold"},"flycheck-error-list-info":{"fg":"blue"},
 "flycheck-error-list-error-message":{"fg":"#cdced1"},"flycheck-error-list-checker-name":{"fg":"steel"},
 "flycheck-error-list-column-number":{"fg":"pewter"},"flycheck-error-list-line-number":{"fg":"pewter"},"flycheck-error-list-filename":{"fg":"blue"},
 "flycheck-error-list-id":{"fg":"steel"},"flycheck-error-list-id-with-explainer":{"fg":"steel","bold":True},
 "flycheck-error-list-highlight":{"bg":"gunmetal"},"flycheck-verify-select-checker":{"fg":"gold"}}
DIRED_FACES=("dired-header dired-directory dired-symlink dired-broken-symlink dired-special dired-set-id "
 "dired-perm-write dired-mark dired-marked dired-flagged dired-ignored dired-warning").split()
DIRED_SEED={
 "dired-header":{"fg":"blue","bold":True},"dired-directory":{"fg":"blue","bold":True},"dired-symlink":{"fg":"sage"},
 "dired-broken-symlink":{"fg":"#de4949","bold":True},"dired-special":{"fg":"regal"},"dired-set-id":{"fg":"terracotta"},
 "dired-perm-write":{"fg":"silver"},"dired-mark":{"fg":"gold"},"dired-marked":{"fg":"gold","bold":True},
 "dired-flagged":{"fg":"terracotta","bold":True},"dired-ignored":{"fg":"pewter"},"dired-warning":{"fg":"gold","bold":True}}
DIRVISH_FACES=("dirvish-inactive dirvish-free-space dirvish-hl-line dirvish-hl-line-inactive "
 "dirvish-file-modes dirvish-file-link-number dirvish-file-user-id dirvish-file-group-id dirvish-file-size dirvish-file-time "
 "dirvish-file-inode-number dirvish-file-device-number dirvish-subtree-guide dirvish-subtree-state "
 "dirvish-collapse-dir-face dirvish-collapse-empty-dir-face dirvish-collapse-file-face dirvish-emerge-group-title "
 "dirvish-media-info-heading dirvish-media-info-property-key dirvish-narrow-match-face-0 dirvish-narrow-match-face-1 "
 "dirvish-narrow-match-face-2 dirvish-narrow-match-face-3 dirvish-narrow-split dirvish-proc-running dirvish-proc-finished "
 "dirvish-proc-failed dirvish-git-commit-message-face dirvish-vc-added-state dirvish-vc-edited-state dirvish-vc-removed-state "
 "dirvish-vc-conflict-state dirvish-vc-locked-state dirvish-vc-missing-state dirvish-vc-needs-merge-face "
 "dirvish-vc-needs-update-state dirvish-vc-unregistered-face").split()
DIRVISH_SEED={
 "dirvish-inactive":{"fg":"pewter"},"dirvish-free-space":{"fg":"sage"},"dirvish-hl-line":{"bg":"gunmetal"},"dirvish-hl-line-inactive":{"bg":"bg-dim"},
 "dirvish-file-modes":{"fg":"steel"},"dirvish-file-link-number":{"fg":"pewter"},"dirvish-file-user-id":{"fg":"blue"},"dirvish-file-group-id":{"fg":"steel"},
 "dirvish-file-size":{"fg":"sage"},"dirvish-file-time":{"fg":"pewter"},"dirvish-file-inode-number":{"fg":"pewter"},"dirvish-file-device-number":{"fg":"pewter"},
 "dirvish-subtree-guide":{"fg":"pewter"},"dirvish-subtree-state":{"fg":"steel"},"dirvish-collapse-dir-face":{"fg":"blue"},
 "dirvish-collapse-empty-dir-face":{"fg":"pewter"},"dirvish-collapse-file-face":{"fg":"silver"},"dirvish-emerge-group-title":{"fg":"gold","bold":True},
 "dirvish-media-info-heading":{"fg":"blue","bold":True},"dirvish-media-info-property-key":{"fg":"steel"},
 "dirvish-narrow-match-face-0":{"fg":"gold","bold":True},"dirvish-narrow-match-face-1":{"fg":"blue","bold":True},
 "dirvish-narrow-match-face-2":{"fg":"emerald","bold":True},"dirvish-narrow-match-face-3":{"fg":"regal","bold":True},"dirvish-narrow-split":{"fg":"pewter"},
 "dirvish-proc-running":{"fg":"gold"},"dirvish-proc-finished":{"fg":"sage"},"dirvish-proc-failed":{"fg":"terracotta"},
 "dirvish-git-commit-message-face":{"fg":"tan","italic":True},"dirvish-vc-added-state":{"fg":"sage"},"dirvish-vc-edited-state":{"fg":"gold"},
 "dirvish-vc-removed-state":{"fg":"terracotta"},"dirvish-vc-conflict-state":{"fg":"terracotta","bold":True},"dirvish-vc-locked-state":{"fg":"blue"},
 "dirvish-vc-missing-state":{"fg":"terracotta"},"dirvish-vc-needs-merge-face":{"fg":"gold"},"dirvish-vc-needs-update-state":{"fg":"gold"},
 "dirvish-vc-unregistered-face":{"fg":"pewter"}}
ORGDRILL_FACES=("org-drill-hidden-cloze-face org-drill-visible-cloze-face org-drill-visible-cloze-hint-face").split()
ORGDRILL_SEED={"org-drill-hidden-cloze-face":{"fg":"#000000","bg":"steel"},"org-drill-visible-cloze-face":{"fg":"gold","bold":True},"org-drill-visible-cloze-hint-face":{"fg":"pewter","italic":True}}
ORGNOTER_FACES=("org-noter-notes-exist-face org-noter-no-notes-exist-face").split()
ORGNOTER_SEED={"org-noter-notes-exist-face":{"fg":"sage"},"org-noter-no-notes-exist-face":{"fg":"pewter"}}
SIGNEL_FACES=("signel-timestamp-face signel-my-msg-face signel-other-msg-face signel-error-face").split()
SIGNEL_SEED={"signel-timestamp-face":{"fg":"pewter"},"signel-my-msg-face":{"fg":"blue"},"signel-other-msg-face":{"fg":"silver"},"signel-error-face":{"fg":"terracotta","bold":True}}
PEARL_FACES=("pearl-preamble-summary pearl-editable-comment pearl-readonly-comment pearl-modified-highlight pearl-modified-local pearl-modified-unknown").split()
PEARL_SEED={"pearl-preamble-summary":{"fg":"blue","bold":True},"pearl-editable-comment":{"fg":"silver"},"pearl-readonly-comment":{"fg":"pewter","italic":True},"pearl-modified-highlight":{"bg":"navy"},"pearl-modified-local":{"fg":"gold"},"pearl-modified-unknown":{"fg":"pewter"}}
CALIBREDB_FACES=("calibredb-search-header-library-name-face calibredb-search-header-library-path-face calibredb-search-header-total-face calibredb-search-header-filter-face calibredb-search-header-sort-face calibredb-search-header-highlight-face "
 "calibredb-id-face calibredb-title-face calibredb-author-face calibredb-format-face calibredb-size-face calibredb-tag-face calibredb-date-face calibredb-mark-face calibredb-series-face calibredb-publisher-face calibredb-pubdate-face "
 "calibredb-language-face calibredb-comment-face calibredb-archive-face calibredb-favorite-face calibredb-file-face calibredb-ids-face calibredb-highlight-face calibredb-current-page-button-face calibredb-mouse-face "
 "calibredb-title-detailed-view-face calibredb-edit-annotation-header-title-face").split()
CALIBREDB_SEED={
 "calibredb-search-header-library-name-face":{"fg":"blue","bold":True},"calibredb-search-header-library-path-face":{"fg":"pewter"},"calibredb-search-header-total-face":{"fg":"sage"},"calibredb-search-header-filter-face":{"fg":"gold"},"calibredb-search-header-sort-face":{"fg":"steel"},"calibredb-search-header-highlight-face":{"fg":"gold","bold":True},
 "calibredb-id-face":{"fg":"pewter"},"calibredb-title-face":{"fg":"blue","bold":True},"calibredb-author-face":{"fg":"sage"},"calibredb-format-face":{"fg":"steel"},"calibredb-size-face":{"fg":"pewter"},"calibredb-tag-face":{"fg":"tan"},"calibredb-date-face":{"fg":"pewter"},"calibredb-mark-face":{"fg":"gold","bold":True},"calibredb-series-face":{"fg":"regal"},"calibredb-publisher-face":{"fg":"steel"},"calibredb-pubdate-face":{"fg":"pewter"},
 "calibredb-language-face":{"fg":"steel"},"calibredb-comment-face":{"fg":"silver","italic":True},"calibredb-archive-face":{"fg":"pewter"},"calibredb-favorite-face":{"fg":"gold"},"calibredb-file-face":{"fg":"blue"},"calibredb-ids-face":{"fg":"pewter"},"calibredb-highlight-face":{"fg":"gold","bold":True},"calibredb-current-page-button-face":{"fg":"blue","bold":True},"calibredb-mouse-face":{"bg":"gunmetal"},
 "calibredb-title-detailed-view-face":{"fg":"gold","bold":True},"calibredb-edit-annotation-header-title-face":{"fg":"blue","bold":True}}
ERC_FACES=("erc-header-line erc-timestamp-face erc-notice-face erc-default-face erc-current-nick-face erc-my-nick-face erc-my-nick-prefix-face erc-nick-default-face erc-nick-prefix-face erc-button-nick-default-face "
 "erc-nick-msg-face erc-direct-msg-face erc-action-face erc-keyword-face erc-pal-face erc-fool-face erc-dangerous-host-face erc-error-face erc-input-face erc-prompt-face erc-command-indicator-face erc-information "
 "erc-button erc-bold-face erc-italic-face erc-underline-face erc-inverse-face erc-spoiler-face erc-fill-wrap-merge-indicator-face erc-keep-place-indicator-arrow erc-keep-place-indicator-line").split()
ERC_SEED={
 "erc-header-line":{"fg":"white","bg":"gunmetal","bold":True},"erc-timestamp-face":{"fg":"pewter"},"erc-notice-face":{"fg":"steel"},"erc-default-face":{"fg":"#cdced1"},"erc-current-nick-face":{"fg":"gold","bold":True},"erc-my-nick-face":{"fg":"gold","bold":True},"erc-my-nick-prefix-face":{"fg":"gold"},"erc-nick-default-face":{"fg":"blue"},"erc-nick-prefix-face":{"fg":"sage"},"erc-button-nick-default-face":{"fg":"blue"},
 "erc-nick-msg-face":{"fg":"regal"},"erc-direct-msg-face":{"fg":"regal"},"erc-action-face":{"fg":"sage","italic":True},"erc-keyword-face":{"fg":"gold","bold":True},"erc-pal-face":{"fg":"emerald"},"erc-fool-face":{"fg":"pewter"},"erc-dangerous-host-face":{"fg":"terracotta","bold":True},"erc-error-face":{"fg":"terracotta","bold":True},"erc-input-face":{"fg":"silver"},"erc-prompt-face":{"fg":"blue","bold":True},"erc-command-indicator-face":{"fg":"steel","bold":True},"erc-information":{"fg":"steel"},
 "erc-button":{"fg":"blue"},"erc-bold-face":{"bold":True},"erc-italic-face":{"italic":True},"erc-underline-face":{"fg":"silver","underline":True},"erc-inverse-face":{"fg":"#000000","bg":"silver"},"erc-spoiler-face":{"fg":"#000000","bg":"gunmetal"},"erc-fill-wrap-merge-indicator-face":{"fg":"pewter"},"erc-keep-place-indicator-arrow":{"fg":"gold"},"erc-keep-place-indicator-line":{"bg":"bg-dim"}}
SLACK_FACES=("slack-room-info-title-face slack-room-info-title-room-name-face slack-room-info-section-title-face slack-room-info-section-label-face slack-room-unread-face "
 "slack-message-output-header slack-message-output-text slack-message-output-reaction slack-message-output-reaction-pressed slack-message-deleted-face slack-new-message-marker-face slack-all-thread-buffer-thread-header-face "
 "slack-message-mention-face slack-message-mention-me-face slack-message-mention-keyword-face slack-channel-button-face "
 "slack-mrkdwn-bold-face slack-mrkdwn-italic-face slack-mrkdwn-code-face slack-mrkdwn-code-block-face slack-mrkdwn-strike-face slack-mrkdwn-blockquote-face slack-mrkdwn-list-face "
 "slack-attachment-header slack-attachment-footer slack-attachment-pad slack-attachment-field-title slack-message-attachment-preview-header-face slack-preview-face slack-block-highlight-source-overlay-face "
 "slack-message-action-face slack-message-action-primary-face slack-message-action-danger-face "
 "slack-button-block-element-face slack-button-primary-block-element-face slack-button-danger-block-element-face slack-select-block-element-face slack-overflow-block-element-face slack-date-picker-block-element-face "
 "slack-dialog-title-face slack-dialog-element-label-face slack-dialog-element-hint-face slack-dialog-element-placeholder-face slack-dialog-element-error-face slack-dialog-submit-button-face slack-dialog-cancel-button-face slack-dialog-select-element-input-face "
 "slack-user-active-face slack-user-dnd-face slack-user-profile-header-face slack-user-profile-property-name-face slack-profile-image-face "
 "slack-search-result-message-header-face slack-search-result-message-username-face "
 "slack-modeline-has-unreads-face slack-modeline-channel-has-unreads-face slack-modeline-thread-has-unreads-face").split()
SLACK_SEED={
 "slack-room-info-title-face":{"fg":"blue","bold":True},"slack-room-info-title-room-name-face":{"fg":"gold","bold":True},"slack-room-info-section-title-face":{"fg":"blue","bold":True},"slack-room-info-section-label-face":{"fg":"steel"},"slack-room-unread-face":{"fg":"white","bold":True},
 "slack-message-output-header":{"fg":"blue","bold":True},"slack-message-output-text":{"fg":"#cdced1"},"slack-message-output-reaction":{"fg":"steel"},"slack-message-output-reaction-pressed":{"fg":"gold","bold":True},"slack-message-deleted-face":{"fg":"pewter","italic":True},"slack-new-message-marker-face":{"fg":"terracotta","bold":True},"slack-all-thread-buffer-thread-header-face":{"fg":"blue","bold":True},
 "slack-message-mention-face":{"fg":"blue"},"slack-message-mention-me-face":{"fg":"gold","bg":"navy","bold":True},"slack-message-mention-keyword-face":{"fg":"gold","bold":True},"slack-channel-button-face":{"fg":"blue"},
 "slack-mrkdwn-bold-face":{"bold":True},"slack-mrkdwn-italic-face":{"italic":True},"slack-mrkdwn-code-face":{"fg":"terracotta"},"slack-mrkdwn-code-block-face":{"fg":"terracotta","bg":"bg-dim"},"slack-mrkdwn-strike-face":{"fg":"pewter","strike":True},"slack-mrkdwn-blockquote-face":{"fg":"silver","italic":True},"slack-mrkdwn-list-face":{"fg":"silver"},
 "slack-attachment-header":{"fg":"blue","bold":True},"slack-attachment-footer":{"fg":"pewter"},"slack-attachment-pad":{"fg":"pewter"},"slack-attachment-field-title":{"fg":"steel","bold":True},"slack-message-attachment-preview-header-face":{"fg":"blue"},"slack-preview-face":{"fg":"silver"},"slack-block-highlight-source-overlay-face":{"bg":"bg-dim"},
 "slack-message-action-face":{"fg":"blue"},"slack-message-action-primary-face":{"fg":"sage"},"slack-message-action-danger-face":{"fg":"terracotta"},
 "slack-button-block-element-face":{"fg":"silver"},"slack-button-primary-block-element-face":{"fg":"sage","bold":True},"slack-button-danger-block-element-face":{"fg":"terracotta","bold":True},"slack-select-block-element-face":{"fg":"blue"},"slack-overflow-block-element-face":{"fg":"steel"},"slack-date-picker-block-element-face":{"fg":"blue"},
 "slack-dialog-title-face":{"fg":"blue","bold":True},"slack-dialog-element-label-face":{"fg":"steel"},"slack-dialog-element-hint-face":{"fg":"pewter","italic":True},"slack-dialog-element-placeholder-face":{"fg":"pewter"},"slack-dialog-element-error-face":{"fg":"terracotta"},"slack-dialog-submit-button-face":{"fg":"sage","bold":True},"slack-dialog-cancel-button-face":{"fg":"silver"},"slack-dialog-select-element-input-face":{"fg":"silver"},
 "slack-user-active-face":{"fg":"sage"},"slack-user-dnd-face":{"fg":"terracotta"},"slack-user-profile-header-face":{"fg":"blue","bold":True},"slack-user-profile-property-name-face":{"fg":"steel"},"slack-profile-image-face":{"fg":"pewter"},
 "slack-search-result-message-header-face":{"fg":"blue"},"slack-search-result-message-username-face":{"fg":"gold","bold":True},
 "slack-modeline-has-unreads-face":{"fg":"gold"},"slack-modeline-channel-has-unreads-face":{"fg":"gold","bold":True},"slack-modeline-thread-has-unreads-face":{"fg":"gold"}}
TELEGA_FACES=("telega-root-heading telega-tracking telega-unread-unmuted-modeline telega-username telega-user-online-status telega-user-non-online-status telega-secret-title telega-contact-birthdays-today "
 "telega-muted-count telega-unmuted-count telega-mention-count telega-has-chatbuf-brackets telega-delim-face telega-shadow telega-link telega-blue telega-red "
 "telega-msg-heading telega-msg-user-title telega-msg-self-title telega-msg-deleted telega-msg-sponsored telega-msg-inline-reply telega-msg-inline-forward telega-msg-inline-other "
 "telega-entity-type-bold telega-entity-type-italic telega-entity-type-underline telega-entity-type-strikethrough telega-entity-type-code telega-entity-type-pre telega-entity-type-blockquote telega-entity-type-mention telega-entity-type-hashtag telega-entity-type-cashtag telega-entity-type-botcommand telega-entity-type-texturl telega-entity-type-spoiler "
 "telega-reaction telega-reaction-chosen telega-reaction-paid telega-reaction-paid-chosen telega-highlight-text-face telega-button-highlight "
 "telega-chat-prompt telega-chat-prompt-aux telega-chat-input-attachment telega-topic-button telega-filter-active telega-filter-button-active telega-filter-button-inactive telega-checklist-stats-done telega-checklist-stats-todo "
 "telega-box-button telega-box-button-active telega-box-button-default-active telega-box-button-default-passive telega-box-button-primary-active telega-box-button-primary-passive telega-box-button-success-active telega-box-button-success-passive telega-box-button-danger-active telega-box-button-danger-passive telega-box-button-ui-active telega-box-button-ui-passive telega-box-button2-active telega-box-button2-passive telega-box-button2-white-foreground "
 "telega-describe-item-title telega-describe-section-title telega-describe-subsection-title telega-enckey-00 telega-enckey-01 telega-enckey-10 telega-enckey-11 "
 "telega-palette-builtin-blue telega-palette-builtin-green telega-palette-builtin-orange telega-palette-builtin-purple "
 "telega-webpage-title telega-webpage-subtitle telega-webpage-header telega-webpage-subheader telega-webpage-outline telega-webpage-fixed telega-webpage-preformatted telega-webpage-marked telega-webpage-strike-through telega-webpage-chat-link telega-link-preview-sitename telega-link-preview-title").split()
TELEGA_SEED={
 "telega-root-heading":{"fg":"blue","bold":True},"telega-tracking":{"fg":"gold"},"telega-unread-unmuted-modeline":{"fg":"gold","bold":True},"telega-username":{"fg":"blue"},"telega-user-online-status":{"fg":"sage"},"telega-user-non-online-status":{"fg":"pewter"},"telega-secret-title":{"fg":"sage"},"telega-contact-birthdays-today":{"fg":"gold"},
 "telega-muted-count":{"fg":"pewter"},"telega-unmuted-count":{"fg":"gold","bold":True},"telega-mention-count":{"fg":"gold","bold":True},"telega-has-chatbuf-brackets":{"fg":"steel"},"telega-delim-face":{"fg":"pewter"},"telega-shadow":{"fg":"pewter"},"telega-link":{"fg":"blue"},"telega-blue":{"fg":"blue"},"telega-red":{"fg":"terracotta"},
 "telega-msg-heading":{"fg":"steel"},"telega-msg-user-title":{"fg":"blue","bold":True},"telega-msg-self-title":{"fg":"gold","bold":True},"telega-msg-deleted":{"fg":"pewter","italic":True},"telega-msg-sponsored":{"fg":"pewter","italic":True},"telega-msg-inline-reply":{"fg":"steel"},"telega-msg-inline-forward":{"fg":"sage"},"telega-msg-inline-other":{"fg":"pewter"},
 "telega-entity-type-bold":{"bold":True},"telega-entity-type-italic":{"italic":True},"telega-entity-type-underline":{"fg":"silver","underline":True},"telega-entity-type-strikethrough":{"fg":"pewter","strike":True},"telega-entity-type-code":{"fg":"terracotta"},"telega-entity-type-pre":{"fg":"terracotta","bg":"bg-dim"},"telega-entity-type-blockquote":{"fg":"silver","italic":True},"telega-entity-type-mention":{"fg":"blue"},"telega-entity-type-hashtag":{"fg":"blue"},"telega-entity-type-cashtag":{"fg":"sage"},"telega-entity-type-botcommand":{"fg":"sage"},"telega-entity-type-texturl":{"fg":"blue"},"telega-entity-type-spoiler":{"fg":"gunmetal","bg":"gunmetal"},
 "telega-reaction":{"fg":"steel"},"telega-reaction-chosen":{"fg":"gold","bold":True},"telega-reaction-paid":{"fg":"gold"},"telega-reaction-paid-chosen":{"fg":"gold","bold":True},"telega-highlight-text-face":{"fg":"#000000","bg":"gold"},"telega-button-highlight":{"fg":"gold","bold":True},
 "telega-chat-prompt":{"fg":"blue","bold":True},"telega-chat-prompt-aux":{"fg":"steel"},"telega-chat-input-attachment":{"fg":"sage"},"telega-topic-button":{"fg":"blue"},"telega-filter-active":{"fg":"gold","bold":True},"telega-filter-button-active":{"fg":"#000000","bg":"gold"},"telega-filter-button-inactive":{"fg":"steel"},"telega-checklist-stats-done":{"fg":"sage"},"telega-checklist-stats-todo":{"fg":"steel"},
 "telega-box-button":{"fg":"blue"},"telega-box-button-active":{"fg":"#000000","bg":"blue"},"telega-box-button-default-active":{"fg":"#000000","bg":"silver"},"telega-box-button-default-passive":{"fg":"steel"},"telega-box-button-primary-active":{"fg":"#000000","bg":"blue"},"telega-box-button-primary-passive":{"fg":"blue"},"telega-box-button-success-active":{"fg":"#000000","bg":"emerald"},"telega-box-button-success-passive":{"fg":"sage"},"telega-box-button-danger-active":{"fg":"#000000","bg":"terracotta"},"telega-box-button-danger-passive":{"fg":"terracotta"},"telega-box-button-ui-active":{"fg":"#000000","bg":"gold"},"telega-box-button-ui-passive":{"fg":"gold"},"telega-box-button2-active":{"fg":"#000000","bg":"blue"},"telega-box-button2-passive":{"fg":"steel"},"telega-box-button2-white-foreground":{"fg":"white"},
 "telega-describe-item-title":{"fg":"steel","bold":True},"telega-describe-section-title":{"fg":"blue","bold":True},"telega-describe-subsection-title":{"fg":"blue"},"telega-enckey-00":{"fg":"pewter"},"telega-enckey-01":{"fg":"sage"},"telega-enckey-10":{"fg":"gold"},"telega-enckey-11":{"fg":"blue"},
 "telega-palette-builtin-blue":{"fg":"blue"},"telega-palette-builtin-green":{"fg":"emerald"},"telega-palette-builtin-orange":{"fg":"terracotta"},"telega-palette-builtin-purple":{"fg":"regal"},
 "telega-webpage-title":{"fg":"blue","bold":True},"telega-webpage-subtitle":{"fg":"steel"},"telega-webpage-header":{"fg":"gold","bold":True},"telega-webpage-subheader":{"fg":"gold"},"telega-webpage-outline":{"fg":"pewter"},"telega-webpage-fixed":{"fg":"terracotta"},"telega-webpage-preformatted":{"fg":"terracotta","bg":"bg-dim"},"telega-webpage-marked":{"fg":"#000000","bg":"gold"},"telega-webpage-strike-through":{"fg":"pewter","strike":True},"telega-webpage-chat-link":{"fg":"blue"},"telega-link-preview-sitename":{"fg":"steel"},"telega-link-preview-title":{"fg":"blue","bold":True}}
# shr is built-in (not in the inventory). It is the HTML renderer behind nov
# (EPUB), eww, elfeed article view, and HTML mail, so theming it themes them all.
SHR_FACES=("shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6 shr-text shr-link shr-selected-link "
 "shr-code shr-mark shr-strike-through shr-sup shr-abbreviation shr-sliced-image").split()
SHR_SEED={
 "shr-h1":{"fg":"gold","bold":True,"height":1.4},"shr-h2":{"fg":"blue","bold":True,"height":1.2},"shr-h3":{"fg":"blue","bold":True},"shr-h4":{"fg":"silver","bold":True},"shr-h5":{"fg":"steel","bold":True},"shr-h6":{"fg":"pewter","bold":True},
 "shr-text":{"fg":"#cdced1"},"shr-link":{"fg":"blue","underline":True},"shr-selected-link":{"fg":"gold","bold":True,"underline":True},"shr-code":{"fg":"terracotta","bg":"bg-dim"},"shr-mark":{"fg":"#000000","bg":"gold"},"shr-strike-through":{"fg":"pewter","strike":True},"shr-sup":{"fg":"steel"},"shr-abbreviation":{"fg":"steel","italic":True},"shr-sliced-image":{"fg":"pewter"}}
def _faces(names,prefix,seed):
    out=[]
    for f in names:
        lbl=(f[len(prefix):] if f.startswith(prefix) else f).replace("-face","").replace("-"," ")
        out.append([f,lbl,seed.get(f,{})])
    return out
APPS={"org-mode":{"label":"org-mode","preview":"org","faces":_faces(ORG_FACES,"org-",ORG_SEED)},
 "magit":{"label":"magit","preview":"magit","faces":_faces(MAGIT_FACES,"magit-",MAGIT_SEED)},
 "elfeed":{"label":"elfeed","preview":"elfeed","faces":_faces(ELFEED_FACES,"elfeed-",ELFEED_SEED)},
 "mu4e":{"label":"mu4e","preview":"mu4e","faces":_faces(MU4E_FACES,"mu4e-",MU4E_SEED)},
 "ghostel":{"label":"ghostel","preview":"ghostel","faces":_faces(GHOSTEL_FACES,"ghostel-",GHOSTEL_SEED)},
 "dashboard":{"label":"dashboard","preview":"dashboard","faces":_faces(DASHBOARD_FACES,"dashboard-",DASHBOARD_SEED)},
 "lsp-mode":{"label":"lsp-mode","preview":"lsp","faces":_faces(LSP_FACES,"lsp-",LSP_SEED)},
 "git-gutter":{"label":"git-gutter","preview":"gitgutter","faces":_faces(GITGUTTER_FACES,"git-gutter:",GITGUTTER_SEED)},
 "flycheck":{"label":"flycheck","preview":"flycheck","faces":_faces(FLYCHECK_FACES,"flycheck-",FLYCHECK_SEED)},
 "dired":{"label":"dired","preview":"dired","faces":_faces(DIRED_FACES,"dired-",DIRED_SEED)},
 "dirvish":{"label":"dirvish","preview":"dirvish","faces":_faces(DIRVISH_FACES,"dirvish-",DIRVISH_SEED)},
 "calibredb":{"label":"calibredb","preview":"calibredb","faces":_faces(CALIBREDB_FACES,"calibredb-",CALIBREDB_SEED)},
 "erc":{"label":"erc","preview":"erc","faces":_faces(ERC_FACES,"erc-",ERC_SEED)},
 "org-drill":{"label":"org-drill","preview":"orgdrill","faces":_faces(ORGDRILL_FACES,"org-drill-",ORGDRILL_SEED)},
 "org-noter":{"label":"org-noter","preview":"orgnoter","faces":_faces(ORGNOTER_FACES,"org-noter-",ORGNOTER_SEED)},
 "signel":{"label":"signel","preview":"signel","faces":_faces(SIGNEL_FACES,"signel-",SIGNEL_SEED)},
 "pearl":{"label":"pearl","preview":"pearl","faces":_faces(PEARL_FACES,"pearl-",PEARL_SEED)},
 "slack":{"label":"slack","preview":"slack","faces":_faces(SLACK_FACES,"slack-",SLACK_SEED)},
 "telega":{"label":"telega","preview":"telega","faces":_faces(TELEGA_FACES,"telega-",TELEGA_SEED)},
 "shr":{"label":"shr (HTML: nov/eww/mail)","preview":"shr","faces":_faces(SHR_FACES,"shr-",SHR_SEED)}}
# Phase 6: merge the generated all-package inventory (refresh with build-inventory.el).
# Bespoke apps stay; every other installed package becomes an editable generic app.
_inv_path=os.path.join(HERE,"package-inventory.json")
if os.path.exists(_inv_path):
    _INV=json.load(open(_inv_path))
    _BESPOKE={"magit","elfeed","org","org-mode","mu4e","ghostel","dashboard","lsp-mode","git-gutter","flycheck","dired","dirvish","calibredb","erc","org-drill","org-noter","signel","pearl","slack","telega","shr"}
    for _pkg in sorted(_INV):
        if _pkg in _BESPOKE or _pkg in APPS: continue
        APPS[_pkg]={"label":_pkg,"preview":"generic","faces":[
            [f,(f[len(_pkg)+1:] if f.startswith(_pkg+"-") else f).replace("-face","").replace("-"," "),{}]
            for f in _INV[_pkg]]}
HTML = """<!doctype html><meta charset=utf-8><title>theme-studio</title>
<style>
 body{background:#0d0b0a;color:#cdced1;font:15px/1.55 monospace;margin:20px}
 h1{font-size:22px;font-weight:normal;color:#e8bd30;margin:26px 0 10px;border-bottom:1px solid #252321;padding-bottom:6px}
 h2{font-size:10pt;color:#8a9496;font-weight:normal;margin:0 0 4px}
 .wrap{display:flex;flex-wrap:nowrap;overflow-x:auto;gap:14px;padding-bottom:10px}
 .col{flex:0 0 auto;width:460px}
 pre{background:#0d0b0a;border:1px solid #252321;border-radius:8px;padding:14px 16px;font-size:12pt;overflow:auto;white-space:pre}
 table.leg{border-collapse:collapse} table.leg td{padding:4px 12px;vertical-align:middle}
 table.leg th{cursor:pointer;color:#b4b1a2;text-align:left;padding:4px 12px;user-select:none;font-weight:normal}
 table.leg th:hover{color:#e8bd30}
 select.chip{appearance:none;border:1px solid #00000060;border-radius:5px;padding:5px 10px;font:bold 14px monospace;width:160px;cursor:pointer}
 .cat{color:#b4b1a2} .ex{font-size:17px}
 .sbtn{width:26px;height:24px;border:1px solid #3a3a3a;border-radius:3px;background:#eaeaea;color:#111;cursor:pointer;font-size:15px;margin-right:2px;padding:0}
 .sbtn.on{background:#0d0b0a;color:#cdced1;border-color:#8a9496}
 .pals{display:flex;gap:8px;flex-wrap:wrap}
 .pchip{width:128px;height:58px;border-radius:6px;border:1px solid #555;position:relative;display:flex;flex-direction:column;align-items:center;justify-content:center;cursor:grab}
 .pchip.drag{opacity:.4} .pchip.sel{outline:3px solid #e8bd30;outline-offset:2px} .pchip.over{outline:2px dashed #e8bd30;outline-offset:1px} .pchip input.nm{background:transparent;border:none;text-align:center;font:bold 10pt monospace;width:108px;outline:none}
 .pchip .mv{position:absolute;bottom:-1px;background:none;border:none;cursor:pointer;font-size:22px;line-height:1;font-weight:bold;opacity:.5;padding:0 5px} .pchip .mv:hover{opacity:1} .pchip .mv.l{left:0} .pchip .mv.r{right:0}
 .pchip .hx{font-size:10pt;opacity:.8} .pchip .rm{position:absolute;top:2px;right:5px;background:none;border:none;cursor:pointer;font-size:14px;font-weight:bold;opacity:.7}
 .pchip .lock{position:absolute;top:3px;right:5px;font-size:10px;opacity:.6}
 .palctl{margin-top:12px;display:flex;gap:8px;align-items:center;flex-wrap:wrap}
 .palctl input[type=text]{background:#161412;border:1px solid #252321;color:#cdced1;border-radius:4px;padding:5px 8px;font:10pt monospace}
 .palctl input[type=text]::placeholder{color:#b4b1a2;opacity:1}
 .palctl{position:relative}
 .swatch{width:128px;height:58px;border:1px solid #555;border-radius:6px;cursor:pointer;background:#888}
 .picker{display:none;position:absolute;top:66px;left:0;z-index:60;background:#161412;border:1px solid #3a3a3a;border-radius:8px;padding:12px;box-shadow:0 10px 30px #000b;width:470px}
 .picker .prow{display:flex;gap:10px}
 .sv{position:relative;width:400px;height:320px;border-radius:4px;cursor:crosshair}
 .svmask{position:absolute;inset:0;pointer-events:none;border-radius:4px}
 .pmode{margin:2px 2px 8px;font:10pt monospace;color:#b4b1a2;display:flex;gap:6px;align-items:center}
 .pmode button{background:#252321;color:#cdced1;border:1px solid #3a3a3a;border-radius:4px;padding:2px 9px;font:10pt monospace;cursor:pointer}
 .pmode button.on{background:#e8bd30;color:#000;border-color:#e8bd30}
 .svcur{position:absolute;width:16px;height:16px;border:2px solid #fff;border-radius:50%;transform:translate(-50%,-50%);box-shadow:0 0 0 1px #0008;pointer-events:none}
 .hue{position:relative;width:34px;height:320px;border-radius:4px;cursor:ns-resize;background:linear-gradient(to bottom,#f00,#ff0,#0f0,#0ff,#00f,#f0f,#f00)}
 .huecur{position:absolute;left:-2px;right:-2px;height:4px;background:#fff;border:1px solid #0008;transform:translateY(-50%);pointer-events:none}
 .pinfo{display:flex;justify-content:space-between;margin:10px 2px 4px;font:12pt monospace;color:#cdced1}
 .pinfo2{margin:0 2px 9px;font:10pt monospace;color:#9aa3ad}
 .pinfo2 span{cursor:default}
 .pkchips{display:flex;flex-wrap:wrap;gap:5px} .pkchips .pc{width:28px;height:28px;border-radius:3px;border:1px solid #555;cursor:pointer}
 .palctl button,.filebar button,.fbtn{background:#252321;color:#e8bd30;border:1px solid #3a3a3a;border-radius:4px;padding:6px 12px;font:10pt monospace;cursor:pointer}
 #palmsg{font:10pt monospace;opacity:0;transition:opacity .35s;margin-left:6px}
 #export{width:100%;height:180px;margin-top:10px;background:#0d0b0a;color:#a4ac64;border:1px solid #252321;border-radius:6px;font:10pt monospace;padding:10px}
 .filebar{margin:6px 0 0;display:flex;gap:8px;align-items:center}
 #pagetitle{font-size:30px;color:#cdced1;font-weight:normal;border:none;margin:4px 0 18px;padding:0}
 .cols{display:flex;gap:28px;align-items:flex-start} .cols.stretch{align-items:stretch}
 .pane{min-width:0} .pane.grow{flex:1} .pane.saveload{flex:0 0 auto;margin-left:auto}
 .pane h1{margin-top:0}
 .filebar.end{justify-content:flex-end} .langbar{margin-bottom:10px;display:flex;gap:8px;align-items:center}
 .pkgbar{margin:0 0 10px;display:flex;gap:8px;align-items:center;flex-wrap:wrap}
 .pkgbar button{background:#252321;color:#e8bd30;border:1px solid #3a3a3a;border-radius:4px;padding:6px 12px;font:10pt monospace;cursor:pointer}
 .hstep{background:#161412;border:1px solid #252321;color:#cdced1;border-radius:4px;padding:3px 4px;font:10pt monospace;width:56px}
 #pkgbody td{padding:3px 8px}
 #codepre{width:100%;box-sizing:border-box}
 .mock{border:1px solid #252321;border-radius:8px;overflow:hidden;font:12pt/1.7 monospace;display:flex;flex-direction:column}
 .mock .mbuf{flex:1} .mock .ln{display:flex;align-items:stretch;white-space:pre}
 .mock .fr{width:14px;flex:0 0 auto;border-right:1px solid #ffffff14} .mock .num{width:36px;flex:0 0 auto;text-align:right;padding-right:10px}
 .mock .cd{flex:1;padding-left:8px} .mock .bar,.mock .echo{padding:4px 10px;white-space:pre}
 #codepre [data-k],.mock [data-k],.mock [data-face]{cursor:pointer}
 @keyframes flashcell{0%,55%{background:#e8bd3066}100%{background:transparent}}
 tr.flash td{animation:flashcell 1.1s ease-out}
 @keyframes flashtok{0%,55%{background:#e8bd30aa;color:#000}100%{background:transparent}}
 .flashtok{animation:flashtok 1.1s ease-out;border-radius:2px}
</style>
<h1 id="pagetitle">Untitled: theme</h1>
<div class="cols">
 <section class="pane grow">
  <h1>palette</h1>
  <div class="pals" id="pals"></div>
  <div class="palctl">
   <div id="swatch" class="swatch" title="open color picker"></div>
   <input type="text" id="newhexstr" placeholder="#rrggbb" value="#888888" oninput="syncHex()" onkeydown="if(event.key==='Enter')applyEdit()" style="width:110px">
   <input type="text" id="newname" placeholder="name" onkeydown="if(event.key==='Enter')applyEdit()">
   <button onclick="addColor()">+ add color</button>
   <button onclick="updateColor()">&#8635; update selected</button>
   <span id="palmsg"></span>
   <div id="picker" class="picker">
    <div class="prow">
     <div id="sv" class="sv"><canvas id="svmask" class="svmask"></canvas><div id="svcur" class="svcur"></div></div>
     <div id="hue" class="hue"><div id="huecur" class="huecur"></div></div>
    </div>
    <div class="pinfo"><span id="pkhex">#888888</span><span id="pkcon"></span></div>
    <div class="pinfo2"><span id="pkoklch" title="OKLCH perceptual coordinates: lightness L (0..1), chroma C, hue H in degrees"></span><span id="pkapca"></span></div>
    <div class="pmode">limit <button data-m="any" class="on">any</button><button data-m="aa">AA+</button><button data-m="aaa">AAA</button></div>
    <div id="pkchips" class="pkchips"></div>
   </div>
  </div>
 </section>
 <section class="pane saveload">
  <h1>export, import, and save</h1>
  <div class="filebar end">
   <label style="color:#b4b1a2">theme name</label><input type="text" id="themename" value="" placeholder="untitled" oninput="updateTitle()" style="background:#161412;border:1px solid #252321;color:#cdced1;border-radius:4px;padding:5px 8px;font:10pt monospace;width:200px">
  </div>
  <div class="filebar end">
   <button id="savebtn" onclick="saveTheme()" style="display:none">&#128190; save</button>
   <button onclick="exportTheme()">&#11015; export</button>
   <button class="fbtn" onclick="importTheme()">&#11014; import</button><input type="file" id="fileinput" accept=".json" onchange="importFile(event)" style="display:none">
   <button id="jsonbtn" onclick="toggleJSON()">show</button>
  </div>
  <textarea id="export" style="display:none" readonly></textarea>
 </section>
</div>
<h1>code/color assignments</h1>
<div class="cols">
 <section class="pane">
  <table class="leg" id="legtable"><thead><tr><th onclick="srt(1)">elements &#9651;</th><th onclick="srt(0)">color &#9651;</th><th>style</th><th>example</th><th title="WCAG contrast of this color on the background">contrast</th></tr></thead><tbody id="legbody"></tbody></table>
 </section>
 <section class="pane grow">
  <div class="langbar"><label style="color:#b4b1a2">language</label><select id="langsel" class="chip" style="width:auto;font:bold 10pt monospace" onchange="renderCode()"></select></div>
  <pre id="codepre"></pre>
 </section>
</div>
<h1>ui faces</h1>
<div class="cols stretch">
 <section class="pane">
  <table class="leg" id="uitable"><thead><tr><th onclick="srtTable('uibody',0)">face &#9651;</th><th onclick="srtTable('uibody',1)">foreground &#9651;</th><th onclick="srtTable('uibody',2)">background &#9651;</th><th>style</th><th>preview</th></tr></thead><tbody id="uibody"></tbody></table>
 </section>
 <section class="pane grow" style="display:flex;flex-direction:column">
  <div class="langbar"><label style="color:#b4b1a2">live buffer preview</label></div>
  <div id="mockframe" class="mock"></div>
 </section>
</div>
<h1>package faces</h1>
<div class="pkgbar">
 <label style="color:#b4b1a2">application</label><select id="appsel" class="chip" style="width:auto;font:bold 10pt monospace"></select>
 <label style="color:#b4b1a2">filter</label><input id="pkgfilter" type="text" placeholder="face name" oninput="buildPkgTable()" style="background:#161412;border:1px solid #252321;color:#cdced1;border-radius:4px;padding:5px 8px;font:10pt monospace;width:160px">
 <button onclick="resetApp()">&#8635; reset all</button>
</div>
<div class="cols stretch">
 <section class="pane">
  <table class="leg" id="pkgtable"><thead><tr><th onclick="srtTable('pkgbody',0)">face &#9651;</th><th onclick="srtTable('pkgbody',1)">fg &#9651;</th><th onclick="srtTable('pkgbody',2)">bg &#9651;</th><th>style</th><th onclick="srtTable('pkgbody',4)">inherit &#9651;</th><th onclick="srtTable('pkgbody',5)">size &#9651;</th><th onclick="srtTable('pkgbody',6)">contrast &#9651;</th><th></th></tr></thead><tbody id="pkgbody"></tbody></table>
 </section>
 <section class="pane grow" style="display:flex;flex-direction:column">
  <div class="langbar"><label id="pkgprevlabel" style="color:#b4b1a2">preview</label></div>
  <div id="pkgpreview" class="mock" style="overflow:auto"></div>
 </section>
</div>
<script>
const SAMPLES=SAMPLES_J, CATS=CATS_J, UI_FACES=UIFACES_J, APPS=APPS_J;
let MAP=MAP_J, PALETTE=PALETTE_J, BOLD=BOLD_J, ITALIC={}, UIMAP=UIMAP_J;
// --- tier-3 package faces: pure state helpers (Phase 1) ---
function pname(n){if(!n)return null;if(/^#/.test(n))return n;const p=PALETTE.find(p=>p[1]===n);return p?p[0]:null;}
function seedPkgmap(){const m={};for(const app in APPS){m[app]={};for(const row of APPS[app].faces){const face=row[0],d=row[2]||{};m[app][face]={fg:pname(d.fg),bg:pname(d.bg),bold:!!d.bold,italic:!!d.italic,underline:!!d.underline,strike:!!d.strike,inherit:d.inherit||null,height:d.height||1,source:'default'};}}return m;}
function packagesForExport(map){const out={};for(const app in map){const faces={};for(const face in map[app]){const f=map[app][face];if(f.source==='default'||f.source==='user'||f.source==='cleared'){const o={fg:f.fg,bg:f.bg,bold:f.bold,italic:f.italic,underline:!!f.underline,strike:!!f.strike,inherit:f.inherit,source:f.source};if(f.height&&f.height!==1)o.height=f.height;faces[face]=o;}}if(Object.keys(faces).length)out[app]=faces;}return out;}
function mergePackagesInto(map,pkgs){if(!pkgs)return;for(const app in pkgs){if(!map[app])map[app]={};for(const face in pkgs[app]){const f=pkgs[app][face]||{};map[app][face]={fg:f.fg??null,bg:f.bg??null,bold:!!f.bold,italic:!!f.italic,underline:!!f.underline,strike:!!f.strike,inherit:f.inherit??null,height:f.height||1,source:f.source||'user'};}}}
let PKGMAP=seedPkgmap();
function esc(t){return t.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');}
// Pure color-math core (lin/rl/contrast/rating/hsv2rgb/rgb2hsv/hex2rgb/rgb2hex,
// plus OKLab/OKLCH/APCA/deltaE), inlined verbatim from colormath.js. normHex,
// textOn, and ratingColor stay below as UI-boundary helpers.
COLORMATH_J
function textOn(h){const L=rl(h);return ((L+0.05)/0.05)>(1.05/(L+0.05))?'#000':'#fff';}
function ratingColor(r){return r>=7?'#5d9b86':r>=4.5?'#a9b2bb':'#cb6b4d';}
function cid(l){return l.replace(/\\W/g,'');}
function buildLangSel(){const s=document.getElementById('langsel');s.innerHTML='';for(const lang in SAMPLES){const o=document.createElement('option');o.value=lang;o.textContent=lang;s.appendChild(o);}}
function renderCode(){
  const lang=document.getElementById('langsel').value;let html='';
  for(const line of SAMPLES[lang]){
    if(line.length===0){html+='\\n';continue;}
    for(const [k,t] of line){const c=MAP[k]||'#cdced1';const w=BOLD[k]?'bold':'normal';const s=ITALIC[k]?'italic':'normal';
      html+=`<span data-k="${k}" style="color:${c};font-weight:${w};font-style:${s}">${esc(t)}</span>`;}
    html+='\\n';}
  const cp=document.getElementById('codepre');cp.innerHTML=html;
  cp.onclick=(e)=>{const s=e.target.closest('[data-k]');if(s)flashAssign(s.dataset.k);};
  buildMockFrame();
}
function buildTable(){
  const tb=document.getElementById('legbody');tb.innerHTML='';
  for(const [kind,label,ex] of CATS){
    const tr=document.createElement('tr');tr.dataset.kind=kind;
    const sel=document.createElement('select');sel.className='chip';
    const cur=MAP[kind];const have=PALETTE.some(p=>p[0]===cur);
    const list=have?PALETTE:[[cur,'(gone) '+cur],...PALETTE];
    for(const [hex,name] of list){const o=document.createElement('option');o.value=hex;o.textContent=name+'  '+hex;o.style.background=hex;o.style.color=textOn(hex);sel.appendChild(o);}
    sel.value=cur;
    const exTd=document.createElement('td');exTd.className='ex';exTd.textContent=ex;
    const crTd=document.createElement('td');crTd.style.whiteSpace='nowrap';crTd.style.fontSize='10pt';
    function styleChip(){sel.style.background=sel.value;sel.style.color=textOn(sel.value);}
    function styleEx(){exTd.style.color=(kind==='bg'?MAP['p']:MAP[kind]);exTd.style.background=MAP['bg'];exTd.style.fontWeight=BOLD[kind]?'bold':'normal';exTd.style.fontStyle=ITALIC[kind]?'italic':'normal';}
    function styleCr(){const r=contrast((kind==='bg'?MAP['p']:MAP[kind]),MAP['bg']);crTd.innerHTML=`<span style="color:${ratingColor(r)}">${r.toFixed(1)}  ${rating(r)}</span>`;}
    styleChip();styleEx();styleCr();
    sel.onchange=()=>{MAP[kind]=sel.value;styleChip();styleEx();styleCr();renderCode();if(kind==='bg'){applyGround();buildTable();}};
    // style buttons
    const stTd=document.createElement('td');
    if(kind!=='bg'){const defs=[['B','a','bold'],['I','a','italic']];
    const btns={};
    defs.forEach(([id,ch,mode])=>{const b=document.createElement('button');b.className='sbtn';b.style.fontWeight=mode==='bold'?'bold':'normal';b.style.fontStyle=mode==='italic'?'italic':'normal';b.textContent=ch;
      b.onclick=()=>{if(mode==='bold'){BOLD[kind]=!BOLD[kind];}else{ITALIC[kind]=!ITALIC[kind];}refresh();renderCode();styleEx();};
      btns[mode]=b;stTd.appendChild(b);});
    function refresh(){btns.bold.classList.toggle('on',!!BOLD[kind]);btns.italic.classList.toggle('on',!!ITALIC[kind]);}
    refresh();}
    const c0=document.createElement('td');c0.appendChild(sel);
    const c2=document.createElement('td');c2.className='cat';c2.textContent=label;c2.style.cursor='pointer';c2.title='flash this category in the code';c2.onclick=()=>flashTokens(kind);
    tr.appendChild(c2);tr.appendChild(c0);tr.appendChild(stTd);tr.appendChild(exTd);tr.appendChild(crTd);
    tb.appendChild(tr);}
}
let dragFrom=null,selectedIdx=null;
function renderPalette(){
  const p=document.getElementById('pals');p.innerHTML='';
  PALETTE.forEach((pc,i)=>{const [hex,name]=pc;const tc=textOn(hex);
    const locked=(hex===MAP['bg']||hex===MAP['p']);
    const d=document.createElement('div');d.className='pchip'+(i===selectedIdx?' sel':'');d.style.background=hex;d.draggable=true;
    const lft=i>0?`<button class="mv l" title="move left" style="color:${tc}">&#8249;</button>`:'';
    const rgt=i<PALETTE.length-1?`<button class="mv r" title="move right" style="color:${tc}">&#8250;</button>`:'';
    const rm=locked?`<span class="lock" title="${hex===MAP['bg']?'background':'foreground'} — can't remove" style="color:${tc}">&#128274;</span>`:`<button class="rm" title="remove" style="color:${tc}">×</button>`;
    d.innerHTML=`${rm}${lft}${rgt}<input class="nm" value="${name}" style="color:${tc}"><div class="hx" style="color:${tc}">${hex}</div>`;
    if(!locked)d.querySelector('.rm').onclick=(e)=>{e.stopPropagation();PALETTE.splice(i,1);if(selectedIdx===i)selectedIdx=null;renderPalette();buildTable();buildUITable();};
    if(lft)d.querySelector('.mv.l').onclick=(e)=>{e.stopPropagation();moveColor(i,-1);};
    if(rgt)d.querySelector('.mv.r').onclick=(e)=>{e.stopPropagation();moveColor(i,1);};
    d.querySelector('.nm').onchange=(e)=>{PALETTE[i][1]=e.target.value;buildTable();buildUITable();};
    d.onclick=(e)=>{if(e.target.closest('.rm')||e.target.closest('.nm')||e.target.closest('.mv'))return;selectColor(i);};
    d.ondragstart=()=>{dragFrom=i;d.classList.add('drag');};
    d.ondragend=()=>{d.classList.remove('drag');document.querySelectorAll('.pchip.over').forEach(x=>x.classList.remove('over'));};
    d.ondragover=(e)=>{e.preventDefault();if(dragFrom!==null&&dragFrom!==i)d.classList.add('over');};
    d.ondragleave=()=>d.classList.remove('over');
    d.ondrop=(e)=>{e.preventDefault();d.classList.remove('over');if(dragFrom===null||dragFrom===i)return;const m=PALETTE.splice(dragFrom,1)[0];PALETTE.splice(i,0,m);dragFrom=null;selectedIdx=null;renderPalette();buildTable();buildUITable();};
    p.appendChild(d);});
  buildUITable();if(document.getElementById('pkgbody'))buildPkgTable();
}
function notify(msg,err){const m=document.getElementById('palmsg');if(!m)return;m.textContent=msg;m.style.color=err?'#cb6b4d':'#8a9496';m.style.opacity='1';clearTimeout(m._t);m._t=setTimeout(()=>{m.style.opacity='0';},err?4000:2800);}
function applyEdit(){if(selectedIdx!==null)updateColor();else addColor();}
function moveColor(i,dir){const j=i+dir;if(j<0||j>=PALETTE.length)return;const t=PALETTE[i];PALETTE[i]=PALETTE[j];PALETTE[j]=t;if(selectedIdx===i)selectedIdx=j;else if(selectedIdx===j)selectedIdx=i;renderPalette();buildTable();buildUITable();}
function selectColor(i){selectedIdx=i;const [hex,name]=PALETTE[i];setHex(hex);document.getElementById('newname').value=name;renderPalette();notify('editing "'+name+'" — change the value, then Enter (or Update selected) to save',false);}
function updateColor(){
  if(selectedIdx===null){notify('click a palette color to select it first',true);return;}
  const i=selectedIdx,oldHex=PALETTE[i][0];
  const newHex=curHex();
  const newName=(document.getElementById('newname').value.trim())||PALETTE[i][1];
  if(PALETTE.some((p,j)=>j!==i&&p[1].toLowerCase()===newName.toLowerCase())){notify('another color is already named "'+newName+'" — names must be unique',true);return;}
  PALETTE[i]=[newHex,newName];
  for(const k in MAP){if(MAP[k]===oldHex)MAP[k]=newHex;}
  for(const f in UIMAP){if(UIMAP[f].fg===oldHex)UIMAP[f].fg=newHex;if(UIMAP[f].bg===oldHex)UIMAP[f].bg=newHex;}
  for(const ap in PKGMAP)for(const fc in PKGMAP[ap]){const o=PKGMAP[ap][fc];if(o.fg===oldHex)o.fg=newHex;if(o.bg===oldHex)o.bg=newHex;}
  closePicker();renderPalette();buildTable();buildUITable();renderCode();applyGround();notify('updated "'+newName+'"',false);
}
function normHex(s){s=s.trim();if(/^[0-9a-fA-F]{6}$/.test(s))s='#'+s;return /^#[0-9a-fA-F]{6}$/.test(s)?s.toLowerCase():null;}
function curHex(){return normHex(document.getElementById('newhexstr').value)||'#888888';}
let pkH=0,pkS=0,pkV=0.5,pickerOn=false;
let pkMode='any';
function pkThresh(){return pkMode==='aa'?4.5:pkMode==='aaa'?7:0;}
function drawMask(){const cv=document.getElementById('svmask');if(!cv)return;const sv=document.getElementById('sv'),w=cv.width=sv.clientWidth,h=cv.height=sv.clientHeight,ctx=cv.getContext('2d');ctx.clearRect(0,0,w,h);const T=pkThresh();if(!T)return;ctx.fillStyle='rgba(8,7,6,0.66)';const step=4;for(let x=0;x<w;x+=step){const S=x/w;for(let y=0;y<h;y+=step){const V=1-y/h,[r,g,b]=hsv2rgb(pkH,S,V);if(contrast(rgb2hex(r,g,b),MAP['bg'])<T)ctx.fillRect(x,y,step,step);}}}
function paintPicker(){const sv=document.getElementById('sv');if(!sv)return;sv.style.background=`linear-gradient(to top,#000,rgba(0,0,0,0)),linear-gradient(to right,#fff,rgba(255,255,255,0)),hsl(${pkH},100%,50%)`;const w=sv.clientWidth,h=sv.clientHeight,hh=document.getElementById('hue').clientHeight;document.getElementById('svcur').style.left=(pkS*w)+'px';document.getElementById('svcur').style.top=((1-pkV)*h)+'px';document.getElementById('huecur').style.top=((pkH/360)*hh)+'px';drawMask();}
function pkReadout(h){const e=document.getElementById('pkhex');if(e)e.textContent=h;const c=document.getElementById('pkcon');if(c){const r=contrast(h,MAP['bg']);c.textContent=r.toFixed(1)+'  '+rating(r);c.style.color=ratingColor(r);}
 const o=document.getElementById('pkoklch');if(o){const lch=oklab2oklch(srgb2oklab(h));o.textContent='OKLCH '+lch.L.toFixed(3)+' '+lch.C.toFixed(3)+' '+Math.round(lch.H)+'\\u00b0';}
 const a=document.getElementById('pkapca');if(a){const lc=apca(h,MAP['bg']);a.textContent='APCA Lc '+lc.toFixed(0);a.title='APCA Lc '+lc.toFixed(1)+' (APCA-W3 0.1.9), text on the ground color. Positive = dark text on a light background, negative = light text on a dark background.';}}
function syncHex(){const v=normHex(document.getElementById('newhexstr').value);if(!v)return;document.getElementById('swatch').style.background=v;[pkH,pkS,pkV]=rgb2hsv(...hex2rgb(v));if(pickerOn)paintPicker();pkReadout(v);}
function setHex(h){h=normHex(h)||h;document.getElementById('newhexstr').value=h;document.getElementById('swatch').style.background=h;[pkH,pkS,pkV]=rgb2hsv(...hex2rgb(h));if(pickerOn)paintPicker();pkReadout(h);}
function pkSet(){const hex=rgb2hex(...hsv2rgb(pkH,pkS,pkV));document.getElementById('newhexstr').value=hex;document.getElementById('swatch').style.background=hex;paintPicker();pkReadout(hex);}
function buildPkChips(){const c=document.getElementById('pkchips');if(!c)return;c.innerHTML='';const T=pkThresh();PALETTE.forEach(([hex,name])=>{const s=document.createElement('div');s.className='pc';s.style.background=hex;s.title=name+' '+hex;const ok=!T||contrast(hex,MAP['bg'])>=T;if(!ok){s.style.opacity='0.22';s.title+=' (below '+pkMode.toUpperCase()+')';}s.onclick=()=>{if(ok)setHex(hex);};c.appendChild(s);});}
function openPicker(){pickerOn=true;[pkH,pkS,pkV]=rgb2hsv(...hex2rgb(curHex()));buildPkChips();document.getElementById('picker').style.display='block';paintPicker();pkReadout(curHex());setTimeout(()=>document.addEventListener('pointerdown',pkOutside),0);}
function closePicker(){if(!pickerOn)return;pickerOn=false;const p=document.getElementById('picker');if(p)p.style.display='none';document.removeEventListener('pointerdown',pkOutside);}
function pkOutside(e){if(!e.target.closest('#picker')&&!e.target.closest('#swatch'))closePicker();}
function pkDrag(el,fn){el.addEventListener('pointerdown',e=>{e.preventDefault();fn(e);const mv=ev=>fn(ev),up=()=>{document.removeEventListener('pointermove',mv);document.removeEventListener('pointerup',up);};document.addEventListener('pointermove',mv);document.addEventListener('pointerup',up);});}
function initPicker(){const sw=document.getElementById('swatch');if(!sw)return;sw.style.background=curHex();sw.onclick=()=>pickerOn?closePicker():openPicker();
  pkDrag(document.getElementById('sv'),e=>{const r=document.getElementById('sv').getBoundingClientRect();pkS=Math.max(0,Math.min(1,(e.clientX-r.left)/r.width));pkV=1-Math.max(0,Math.min(1,(e.clientY-r.top)/r.height));pkSet();});
  pkDrag(document.getElementById('hue'),e=>{const r=document.getElementById('hue').getBoundingClientRect();pkH=Math.max(0,Math.min(1,(e.clientY-r.top)/r.height))*360;pkSet();});
  document.querySelectorAll('.pmode button').forEach(b=>b.onclick=()=>{pkMode=b.dataset.m;document.querySelectorAll('.pmode button').forEach(x=>x.classList.toggle('on',x===b));drawMask();buildPkChips();});}
function addColor(){const h=curHex();const name=document.getElementById('newname').value.trim();
  if(!name){notify('name the color before adding it',true);return;}
  if(PALETTE.some(p=>p[1].toLowerCase()===name.toLowerCase())){notify('a color named "'+name+'" already exists — select it and use Update selected to change its value',true);return;}
  PALETTE.push([h,name]);document.getElementById('newname').value='';selectedIdx=null;closePicker();renderPalette();buildTable();notify('added "'+name+'"',false);}
function themeName(){return (document.getElementById('themename').value||'theme').trim()||'theme';}
function fileSlug(){return themeName().replace(/[^A-Za-z0-9._-]+/g,'-').replace(/^-+|-+$/g,'')||'theme';}
function exportObj(){const a={};CATS.forEach(c=>a[c[0]]=MAP[c[0]]);const o={name:themeName(),palette:PALETTE,assignments:a,bold:Object.keys(BOLD).filter(k=>BOLD[k]),italic:Object.keys(ITALIC).filter(k=>ITALIC[k]),ui:UIMAP};const pk=packagesForExport(PKGMAP);if(Object.keys(pk).length)o.packages=pk;return o;}
function exportState(){const t=document.getElementById('export');t.value=JSON.stringify(exportObj(),null,1);t.style.display='block';t.focus();t.select();}
function toggleJSON(){const t=document.getElementById('export'),b=document.getElementById('jsonbtn');if(t.style.display==='block'){t.style.display='none';b.textContent='show';}else{exportState();b.textContent='hide';}}
function updateTitle(){const n=document.getElementById('themename').value.trim();document.getElementById('pagetitle').textContent=(n||'Untitled')+': theme';const sb=document.getElementById('savebtn');if(sb){sb.style.display=n||fileHandle?'':'none';sb.title=fileHandle?'overwrite the imported/saved file':'choose where to save';}}
let fileHandle=null;
function exportTheme(){const blob=new Blob([JSON.stringify(exportObj(),null,1)],{type:'application/json'});const a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download=fileSlug()+'.json';a.click();}
async function saveTheme(){const data=JSON.stringify(exportObj(),null,1);
  if(!window.showSaveFilePicker){exportTheme();notify('saved via download (browser has no Save-File support)',false);return;}
  try{if(!fileHandle)fileHandle=await window.showSaveFilePicker({suggestedName:fileSlug()+'.json',types:[{description:'theme JSON',accept:{'application/json':['.json']}}]});
    const w=await fileHandle.createWritable();await w.write(data);await w.close();notify('saved "'+themeName()+'"',false);updateTitle();
  }catch(e){if(e&&e.name!=='AbortError')notify('save failed: '+e.message,true);}}
function applyImported(text){const d=JSON.parse(text);if(d.name)document.getElementById('themename').value=d.name;if(d.palette)PALETTE=d.palette;if(d.assignments)Object.assign(MAP,d.assignments);
  BOLD={};(d.bold||[]).forEach(k=>BOLD[k]=true);ITALIC={};(d.italic||[]).forEach(k=>ITALIC[k]=true);
  if(d.ui)Object.assign(UIMAP,d.ui);
  PKGMAP=seedPkgmap();if(d.packages)mergePackagesInto(PKGMAP,d.packages);
  renderPalette();buildTable();buildUITable();buildPkgTable();buildPkgPreview();renderCode();applyGround();updateTitle();}
// File-input fallback (no File System Access API): no writable handle, so save still prompts.
function importFile(ev){const f=ev.target.files[0];if(!f)return;const r=new FileReader();
  r.onload=()=>{try{applyImported(r.result);fileHandle=null;updateTitle();}catch(e){alert('bad theme file: '+e.message);}};
  r.readAsText(f);ev.target.value='';}
// Preferred import: keep the file handle so a later save overwrites the same file.
async function importTheme(){
  if(!window.showOpenFilePicker){const fi=document.getElementById('fileinput');if(fi)fi.click();return;}
  try{const [h]=await window.showOpenFilePicker({types:[{description:'theme JSON',accept:{'application/json':['.json']}}]});
    const file=await h.getFile();applyImported(await file.text());fileHandle=h;updateTitle();
    notify('imported "'+(themeName()||file.name)+'" — save now overwrites it',false);
  }catch(e){if(e&&e.name!=='AbortError')notify('import failed: '+e.message,true);}}
function applyGround(){document.querySelectorAll('pre').forEach(p=>p.style.background=MAP['bg']);document.querySelectorAll('.ex').forEach(e=>e.style.background=MAP['bg']);}
function uf(f){return UIMAP[f]||{};}
function udeco(o){return `font-weight:${o.bold?'bold':'normal'};font-style:${o.italic?'italic':'normal'};text-decoration:${(o.underline?'underline ':'')+(o.strike?'line-through':'')||'none'}`;}
function flashRow(tr){if(!tr)return;tr.scrollIntoView({block:'center',behavior:'smooth'});tr.classList.remove('flash');void tr.offsetWidth;tr.classList.add('flash');}
function flashEl(el){if(!el)return;el.scrollIntoView({block:'nearest',inline:'nearest',behavior:'smooth'});el.classList.remove('flashtok');void el.offsetWidth;el.classList.add('flashtok');}
// Flash every matching element but scroll only the first into view, so a face
// that maps to several preview spans still lands the viewport on the first.
function flashEls(els){els=[...els];if(!els.length)return;els[0].scrollIntoView({block:'nearest',inline:'nearest',behavior:'smooth'});els.forEach(el=>{el.classList.remove('flashtok');void el.offsetWidth;el.classList.add('flashtok');});}
function flashTokens(kind){const sp=document.querySelectorAll('#codepre [data-k="'+kind+'"]');if(sp.length){flashEls(sp);return;}const row=document.querySelector('#legbody tr[data-kind="'+kind+'"]');if(row)flashEl(row.querySelector('.ex'));}
function flashAssign(k){flashRow(document.querySelector(`#legbody tr[data-kind="${k}"]`));}
function flashUi(f){flashRow(document.querySelector(`#uibody tr[data-face="${f}"]`));}
function flashUiPreview(f){const sp=document.querySelectorAll(`#mockframe [data-face="${f}"]`);if(sp.length){flashEls(sp);return;}const cell=document.getElementById('uiprev-'+f);if(cell)flashEl(cell);}
function flashPkg(f){flashRow(document.querySelector(`#pkgbody tr[data-face="${f}"]`));}
function flashPkgPreview(f){const sp=document.querySelectorAll(`#pkgpreview [data-face="${f}"]`);if(sp.length){flashEls(sp);return;}const row=document.querySelector(`#pkgbody tr[data-face="${f}"]`);if(row)flashEl(row.querySelector('.cat'));}
function mockSpan(k,t){return `<span data-k="${k}" style="color:${MAP[k]||MAP['p']};font-weight:${BOLD[k]?'bold':'normal'};font-style:${ITALIC[k]?'italic':'normal'}">${esc(t)}</span>`;}
function syncMockHeight(){const t=document.getElementById('uitable'),m=document.getElementById('mockframe');if(!t||!m)return;const lb=m.previousElementSibling,lbh=lb?lb.getBoundingClientRect().height+10:30;m.style.height=Math.max(t.getBoundingClientRect().height-lbh,220)+'px';}
function buildMockFrame(){
  const fr=document.getElementById('mockframe');if(!fr)return;
  const bg=MAP['bg'],fg=MAP['p'];
  const ln=uf('line-number'),lnc=uf('line-number-current-line'),hl=uf('hl-line'),hil=uf('highlight'),reg=uf('region'),isr=uf('isearch'),isf=uf('isearch-fail'),laz=uf('lazy-highlight'),par=uf('show-paren-match'),parx=uf('show-paren-mismatch'),cur=uf('cursor'),ml=uf('mode-line'),mli=uf('mode-line-inactive'),mb=uf('minibuffer-prompt'),frng=uf('fringe'),vb=uf('vertical-border'),lnk=uf('link'),err=uf('error'),wrn=uf('warning'),suc=uf('success');
  const lines=[
    {t:[['cmd',';; '],['cm','init.el - your config']]},
    {t:[['punc','('],['kw','require'],['p',' '],['con',"'cl-lib"],['punc',')']]},
    {t:[]},
    {t:[['punc','('],['kw','defun'],['p',' '],['fnd','cj/greet'],['p',' '],['punc','('],['var','name'],['punc',')']]},
    {t:[['p','  '],['punc','('],['fnc','message'],['p',' '],['str','"hi %s"'],['p',' '],['var','name'],['punc','))']],cur:1},
    {t:[['p','  '],['punc','('],['kw','setq'],['p',' '],['var','count'],['p',' '],['num','42'],['punc',')']],region:1},
    {plain:'  (if (> count 0)',match:1},
    {plain:'    (setq total (+ total count))',hl:1},
    {t:[['p','      '],['punc','('],['fnc','process'],['p',' '],['var','items'],['punc',')']]},
    {plain:'    (cl-incf count)',lazy:1},
    {t:[['p','  '],['punc','('],['kw','setq'],['p',' '],['var','done'],['p',' '],['con','t'],['punc',')']],paren:1},
    {plain:'    (oops nested))',mismatch:1}
  ];
  let buf='';
  lines.forEach((L,i)=>{
    const isc=L.cur;
    const nFg=isc?(lnc.fg||fg):(ln.fg||fg), nBg=isc?(lnc.bg||'transparent'):(ln.bg||'transparent');
    const rowBg=isc?(hl.bg||'transparent'):'transparent';
    let cd;
    if(L.plain){
      if(L.match)cd=`<span data-face="isearch" style="color:${isr.fg||fg};background:${isr.bg||'transparent'}">${esc(L.plain)}</span>`;
      else if(L.lazy)cd=`<span data-face="lazy-highlight" style="color:${laz.fg||fg};background:${laz.bg||'transparent'}">${esc(L.plain)}</span>`;
      else if(L.hl)cd=`<span data-face="highlight" style="background:${hil.bg||'transparent'};color:${hil.fg||fg}">${esc(L.plain)}</span>`;
      else if(L.mismatch)cd=esc(L.plain.slice(0,-1))+`<span data-face="show-paren-mismatch" style="background:${parx.bg||'transparent'};color:${parx.fg||fg};font-weight:bold">${esc(L.plain.slice(-1))}</span>`;
      else cd=esc(L.plain);
    } else if(L.paren){cd=L.t.map(([k,t],j)=>j===L.t.length-1?`<span data-face="show-paren-match" style="background:${par.bg||'transparent'};color:${par.fg||MAP[k]||fg};font-weight:bold">${esc(t)}</span>`:mockSpan(k,t)).join('');}
    else{cd=L.t.map(([k,t])=>mockSpan(k,t)).join('');if(L.region)cd=`<span data-face="region" style="background:${reg.bg||'transparent'}">${cd}</span>`;}
    if(isc)cd+=`<span data-face="cursor" style="background:${cur.bg||fg};color:${bg}"> </span>`;
    const nFace=isc?'line-number-current-line':'line-number';
    buf+=`<div class="ln" style="background:${rowBg}"><span class="fr" data-face="fringe" style="background:${frng.bg||bg}"></span><span class="num" data-face="${nFace}" style="color:${nFg};background:${nBg}">${i+1}</span><span class="cd">${cd||'&nbsp;'}</span></div>`;
  });
  let html=`<div class="mbuf" style="display:flex;background:${bg}"><div style="flex:1;min-width:0">${buf}</div><div data-face="vertical-border" title="vertical-border" style="width:3px;flex:0 0 auto;background:${vb.fg||vb.bg||'#2f343a'}"></div></div>`;
  html+=`<div class="bar" data-face="mode-line" style="background:${ml.bg||fg};color:${ml.fg||bg};${udeco(ml)}">  init.el      (Emacs Lisp)      L5      git:main  </div>`;
  html+=`<div class="bar" data-face="mode-line-inactive" style="background:${mli.bg||bg};color:${mli.fg||fg};${udeco(mli)}">  *Messages*      (Fundamental)  </div>`;
  html+=`<div class="echo" style="color:${fg}"><span data-face="minibuffer-prompt" style="color:${mb.fg||fg};${udeco(mb)}">I-search:</span> count   <span data-face="isearch-fail" style="color:${isf.fg||fg};background:${isf.bg||'transparent'};${udeco(isf)}">zzz [no match]</span></div>`;
  html+=`<div class="echo"><span data-face="link" style="color:${lnk.fg||fg};${udeco(lnk)}">https://gnu.org</span>   <span data-face="error" style="color:${err.fg||fg};${udeco(err)}">error</span>   <span data-face="warning" style="color:${wrn.fg||fg};${udeco(wrn)}">warning</span>   <span data-face="success" style="color:${suc.fg||fg};${udeco(suc)}">ok</span></div>`;
  fr.innerHTML=html;fr.style.background=bg;fr.style.color=fg;
  fr.onclick=(e)=>{const u=e.target.closest('[data-face]');if(u){flashUi(u.dataset.face);return;}const k=e.target.closest('[data-k]');if(k)flashAssign(k.dataset.k);};
}
function colorDropdown(value,onpick){
  const sel=document.createElement('select');sel.className='chip';
  const none=document.createElement('option');none.value='';none.textContent='— none —';none.style.background='#161412';none.style.color='#b4b1a2';sel.appendChild(none);
  for(const [hex,name] of PALETTE){const o=document.createElement('option');o.value=hex;o.textContent=name+'  '+hex;o.style.background=hex;o.style.color=textOn(hex);sel.appendChild(o);}
  sel.value=value||'';
  function style(){if(sel.value){sel.style.background=sel.value;sel.style.color=textOn(sel.value);}else{sel.style.background='#161412';sel.style.color='#b4b1a2';}}
  style();
  sel.onchange=()=>{style();onpick(sel.value||null);};
  return sel;
}
function uiSelect(face,attr){return colorDropdown(UIMAP[face][attr],v=>{UIMAP[face][attr]=v;paintUI(face);buildMockFrame();});}
const BASE_INHERITS=['fixed-pitch','variable-pitch','default','link','bold','italic','shadow'];
function seedFace(d){return {fg:pname(d.fg),bg:pname(d.bg),bold:!!d.bold,italic:!!d.italic,underline:!!d.underline,strike:!!d.strike,inherit:d.inherit||null,height:d.height||1,source:'default'};}
function curApp(){const s=document.getElementById('appsel');return s&&s.value?s.value:Object.keys(APPS)[0];}
function pkgEffFg(app,face,seen){seen=seen||{};const f=PKGMAP[app]&&PKGMAP[app][face];if(!f||seen[face])return null;seen[face]=1;if(f.fg)return f.fg;if(f.inherit&&PKGMAP[app][f.inherit])return pkgEffFg(app,f.inherit,seen);return null;}
function pkgEffBg(app,face,seen){seen=seen||{};const f=PKGMAP[app]&&PKGMAP[app][face];if(!f||seen[face])return null;seen[face]=1;if(f.bg)return f.bg;if(f.inherit&&PKGMAP[app][f.inherit])return pkgEffBg(app,f.inherit,seen);return null;}
function buildAppSel(){const s=document.getElementById('appsel');if(!s)return;s.innerHTML='';for(const app in APPS){const o=document.createElement('option');o.value=app;o.textContent=APPS[app].label;s.appendChild(o);}s.onchange=pkgChanged;}
function pkgChanged(){buildPkgTable();buildPkgPreview();syncPkgHeight();}
function buildPkgTable(){
  const app=curApp(),tb=document.getElementById('pkgbody');if(!tb)return;tb.innerHTML='';
  const flt=(document.getElementById('pkgfilter').value||'').trim().toLowerCase();
  const inh=[''].concat(BASE_INHERITS).concat(APPS[app].faces.map(r=>r[0]));
  for(const [face,label,def] of APPS[app].faces){
    if(flt&&!(face.toLowerCase().includes(flt)||label.toLowerCase().includes(flt)))continue;
    const f=PKGMAP[app][face],tr=document.createElement('tr');tr.dataset.face=face;
    const c0=document.createElement('td');c0.className='cat';c0.textContent=label;c0.title=face;c0.style.cursor='pointer';c0.onclick=()=>flashPkgPreview(face);
    const cf=document.createElement('td');cf.appendChild(colorDropdown(f.fg,v=>{f.fg=v;f.source='user';pkgChanged();}));
    const cb=document.createElement('td');cb.appendChild(colorDropdown(f.bg,v=>{f.bg=v;f.source='user';pkgChanged();}));
    const cw=document.createElement('td');[['B','bold'],['I','italic'],['U','underline'],['S','strike']].forEach(([ch,at])=>{const b=document.createElement('button');b.className='sbtn'+(f[at]?' on':'');b.textContent='a';b.style.fontWeight=at==='bold'?'bold':'normal';b.style.fontStyle=at==='italic'?'italic':'normal';b.style.textDecoration=at==='underline'?'underline':at==='strike'?'line-through':'none';b.title=at;b.onclick=()=>{f[at]=!f[at];f.source='user';pkgChanged();};cw.appendChild(b);});
    const ci=document.createElement('td');const isel=document.createElement('select');isel.className='chip';isel.style.cssText='width:150px;font:10pt monospace';inh.forEach(o=>{const op=document.createElement('option');op.value=o;op.textContent=o||'— none —';isel.appendChild(op);});isel.value=f.inherit||'';isel.onchange=()=>{f.inherit=isel.value||null;f.source='user';pkgChanged();};ci.appendChild(isel);
    const ch=document.createElement('td');const hin=document.createElement('input');hin.type='number';hin.min='0.8';hin.max='2.5';hin.step='0.05';hin.value=f.height||1;hin.className='hstep';hin.onchange=()=>{f.height=parseFloat(hin.value)||1;f.source='user';pkgChanged();};ch.appendChild(hin);
    const cc=document.createElement('td');cc.style.fontSize='10pt';cc.style.whiteSpace='nowrap';const efg=pkgEffFg(app,face)||MAP['p'],ebg=pkgEffBg(app,face)||MAP['bg'],r=contrast(efg,ebg);cc.innerHTML=`<span style="color:${ratingColor(r)}">${r.toFixed(1)}  ${rating(r)}</span>`;
    const cr=document.createElement('td');const rb=document.createElement('button');rb.className='sbtn';rb.textContent='↺';rb.title='reset to default';rb.onclick=()=>{PKGMAP[app][face]=seedFace(def);pkgChanged();};cr.appendChild(rb);
    tr.append(c0,cf,cb,cw,ci,ch,cc,cr);tb.appendChild(tr);
  }
  applyTableSort('pkgbody');
}
function ofs(app,face){const f=PKGMAP[app][face]||{},fg=pkgEffFg(app,face)||MAP['p'],bg=pkgEffBg(app,face);const dec=(f.underline?'underline ':'')+(f.strike?'line-through':'');return `color:${fg};${bg?'background:'+bg+';':''}font-weight:${f.bold?'bold':'normal'};font-style:${f.italic?'italic':'normal'};text-decoration:${dec.trim()||'none'};font-size:${(f.height||1)}em`;}
function os(app,face,txt){return `<span data-face="${face}" style="${ofs(app,face)}">${txt}</span>`;}
function renderOrgPreview(){const a='org-mode',L=[];
  L.push(os(a,'org-document-info-keyword','#+TITLE:')+' '+os(a,'org-document-title','Project Notes'));
  L.push(os(a,'org-document-info-keyword','#+AUTHOR:')+' '+os(a,'org-document-info','Craig Jennings'));
  L.push(os(a,'org-meta-line','#+STARTUP: overview'));
  L.push('');
  L.push(os(a,'org-level-1','* Inbox')+'  '+os(a,'org-tag',':work:')+os(a,'org-tag-group',':@office:'));
  L.push(os(a,'org-level-2','** ')+os(a,'org-todo','TODO')+os(a,'org-level-2',' Draft the spec')+' '+os(a,'org-priority','[#A]')+' '+os(a,'org-tag',':spec:'));
  L.push('   '+os(a,'org-special-keyword','SCHEDULED:')+' '+os(a,'org-date','&lt;2026-06-08 Sun&gt;')+'  '+os(a,'org-special-keyword','DEADLINE:')+' '+os(a,'org-date','&lt;2026-06-12 Thu&gt;'));
  L.push('   '+os(a,'org-drawer',':PROPERTIES:'));
  L.push('   '+os(a,'org-special-keyword',':ID:')+'       '+os(a,'org-property-value','abc-123-def'));
  L.push('   '+os(a,'org-drawer',':END:'));
  L.push('   '+os(a,'org-list-dt','- term ::')+' definition, with a '+os(a,'org-footnote','[fn:1]')+' note.');
  L.push('   '+os(a,'org-checkbox','[X]')+' done item   '+os(a,'org-checkbox-statistics-done','[2/2]'));
  L.push('   '+os(a,'org-checkbox','[ ]')+' open item   '+os(a,'org-checkbox-statistics-todo','[0/3]')+'  '+os(a,'org-warning','(!)'));
  L.push(os(a,'org-level-2','** ')+os(a,'org-done','DONE')+os(a,'org-headline-done',' Ship the tool'));
  L.push(os(a,'org-level-3','*** ')+os(a,'org-headline-todo','Heading three'));
  L.push(os(a,'org-level-4','**** four')+' / '+os(a,'org-level-5','***** five')+' / '+os(a,'org-level-6','****** six')+' / '+os(a,'org-level-7','******* seven')+' / '+os(a,'org-level-8','******** eight'));
  L.push('   Inline '+os(a,'org-code','=code=')+', '+os(a,'org-verbatim','~verbatim~')+', '+os(a,'org-inline-src-block','src_py{1+1}')+',');
  L.push('   a '+os(a,'org-link','[[https://gnu.org][link]]')+', a '+os(a,'org-target','&lt;&lt;target&gt;&gt;')+', a '+os(a,'org-macro','{{{macro}}}')+',');
  L.push('   a '+os(a,'org-cite','[cite:')+os(a,'org-cite-key','@knuth1984')+os(a,'org-cite',']')+', a date '+os(a,'org-sexp-date','&lt;%%(diary-float 6 5 2)&gt;')+'.');
  L.push('   '+os(a,'org-quote','#+begin_quote')+' a '+os(a,'org-verse','verse')+' line, latex '+os(a,'org-latex-and-related','$E = mc^2$')+'.');
  L.push('');
  L.push('   '+os(a,'org-block-begin-line','#+begin_src elisp'));
  L.push('   '+os(a,'org-block','  (message "hi")'));
  L.push('   '+os(a,'org-block-end-line','#+end_src'));
  L.push('');
  L.push('   '+os(a,'org-table-header','| name | hex     |'));
  L.push('   '+os(a,'org-table','|------+---------|'));
  L.push('   '+os(a,'org-table-row','| blue | #67809c |')+'  '+os(a,'org-formula',':=vsum(@2)'));
  L.push('   '+os(a,'org-column-title','Effort')+' '+os(a,'org-column','| 0:30 |')+'   '+os(a,'org-archived','* archived')+os(a,'org-ellipsis',' ...'));
  L.push('');
  L.push(os(a,'org-agenda-structure','Week-agenda (W23):'));
  L.push(os(a,'org-agenda-date','Monday      8 June 2026'));
  L.push(os(a,'org-agenda-date-today','Tuesday     9 June 2026')+' '+os(a,'org-agenda-current-time','10:24')+' '+os(a,'org-time-grid','----------'));
  L.push(os(a,'org-agenda-date-weekend','Saturday   13 June')+' / '+os(a,'org-agenda-date-weekend-today','wknd-today'));
  L.push('  '+os(a,'org-scheduled-previously','Sched.past:')+' overdue   '+os(a,'org-agenda-done','x done item'));
  L.push('  '+os(a,'org-scheduled','Scheduled:')+' a task   '+os(a,'org-scheduled-today','due today'));
  L.push('  '+os(a,'org-imminent-deadline','Deadline!')+' / '+os(a,'org-upcoming-deadline','upcoming')+' / '+os(a,'org-upcoming-distant-deadline','distant'));
  L.push('  '+os(a,'org-agenda-dimmed-todo-face','dimmed todo')+'  '+os(a,'org-agenda-diary','diary')+'  '+os(a,'org-agenda-clocking','clocking'));
  L.push('  '+os(a,'org-agenda-calendar-event','cal-event')+' / '+os(a,'org-agenda-calendar-sexp','cal-sexp')+' / '+os(a,'org-agenda-calendar-daterange','range'));
  L.push('  '+os(a,'org-agenda-structure-secondary','secondary')+' '+os(a,'org-agenda-structure-filter','filter')+' '+os(a,'org-agenda-restriction-lock','lock')+' '+os(a,'org-agenda-column-dateline','col-date'));
  L.push('  Filters: '+os(a,'org-agenda-filter-category','cat')+' '+os(a,'org-agenda-filter-tags','tags')+' '+os(a,'org-agenda-filter-effort','effort')+' '+os(a,'org-agenda-filter-regexp','re'));
  L.push('  '+os(a,'org-mode-line-clock','[0:45]')+' / '+os(a,'org-mode-line-clock-overrun','[OVER]')+'   '+os(a,'org-dispatcher-highlight','[d]ispatch'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;
}
function renderMagitPreview(){const a='magit',L=[];
  L.push(os(a,'magit-header-line',' Magit: dotemacs ')+'  '+os(a,'magit-header-line-key','g')+os(a,'magit-header-line-log-select',' refresh'));
  L.push(os(a,'magit-head','Head:')+'     '+os(a,'magit-branch-current','main')+'  '+os(a,'magit-diff-revision-summary','Ship the tool'));
  L.push(os(a,'magit-head','Merge:')+'    '+os(a,'magit-branch-remote','origin/main')+'  '+os(a,'magit-branch-local','main'));
  L.push(os(a,'magit-head','Push:')+'     '+os(a,'magit-branch-remote-head','origin/main'));
  L.push(os(a,'magit-head','Upstream:')+' '+os(a,'magit-branch-upstream','origin/main')+'  '+os(a,'magit-branch-warning','(diverged)'));
  L.push('');
  L.push(os(a,'magit-section-heading','Untracked files')+' '+os(a,'magit-section-child-count','(2)'));
  L.push('  '+os(a,'magit-filename','notes.txt')+'   '+os(a,'magit-dimmed','(ignored sibling)'));
  L.push(os(a,'magit-section-highlight','  scratch.el   (highlighted row)'));
  L.push('');
  L.push(os(a,'magit-section-heading','Unstaged changes')+' '+os(a,'magit-section-child-count','(1)'));
  L.push(os(a,'magit-diff-file-heading','modified   generate.py'));
  L.push(os(a,'magit-diff-hunk-heading','@@ -1,4 +1,5 @@ def main'));
  L.push(os(a,'magit-diff-context','   unchanged context'));
  L.push(os(a,'magit-diff-removed','- old line')+os(a,'magit-diff-whitespace-warning','   '));
  L.push(os(a,'magit-diff-added','+ new line'));
  L.push('');
  L.push(os(a,'magit-section-heading','Staged changes')+'   '+os(a,'magit-diffstat-added','++++')+os(a,'magit-diffstat-removed','--'));
  L.push(os(a,'magit-diff-file-heading-highlight','modified   README.md   (highlighted heading)'));
  L.push(os(a,'magit-diff-hunk-heading-highlight','@@ hunk heading highlight @@'));
  L.push(os(a,'magit-diff-added-highlight','+ added highlight')+'   '+os(a,'magit-diff-removed-highlight','- removed highlight'));
  L.push(os(a,'magit-diff-context-highlight','  context highlight'));
  L.push('');
  L.push(os(a,'magit-section-heading','Stashes'));
  L.push('  '+os(a,'magit-refname-stash','stash@{0}')+'  '+os(a,'magit-refname-wip','wip')+'  '+os(a,'magit-refname-pullreq','pr/42')+'  '+os(a,'magit-refname','refs/heads/x'));
  L.push('');
  L.push(os(a,'magit-section-heading','Recent commits'));
  L.push(os(a,'magit-log-graph','* ')+os(a,'magit-hash','b5b1869f')+' '+os(a,'magit-log-date','06-08')+' '+os(a,'magit-log-author','Craig')+'  enlarge the picker');
  L.push(os(a,'magit-log-graph','* ')+os(a,'magit-hash','4fa5e995')+' '+os(a,'magit-log-date','06-07')+' '+os(a,'magit-log-author','Craig')+'  '+os(a,'magit-keyword','[feat]')+' picker');
  L.push(os(a,'magit-log-graph','* ')+os(a,'magit-hash','de07e01a')+' '+os(a,'magit-log-date','06-05')+' '+os(a,'magit-log-author','Craig')+'  '+os(a,'magit-tag','v0.3')+' '+os(a,'magit-keyword-squash','!squash'));
  L.push('');
  L.push(os(a,'magit-section-secondary-heading','Merge conflict')+'  '+os(a,'magit-diff-lines-heading','lines 10-14')+os(a,'magit-diff-lines-boundary','|'));
  L.push('  '+os(a,'magit-diff-conflict-heading','=======')+'  '+os(a,'magit-diff-conflict-heading-highlight','(hl)'));
  L.push('  '+os(a,'magit-diff-base','base')+'/'+os(a,'magit-diff-base-highlight','base-hl')+'  '+os(a,'magit-diff-our','ours')+'/'+os(a,'magit-diff-our-highlight','ours-hl')+'  '+os(a,'magit-diff-their','theirs')+'/'+os(a,'magit-diff-their-highlight','theirs-hl'));
  L.push('  '+os(a,'magit-diff-hunk-region','hunk-region')+'  '+os(a,'magit-diff-file-heading-selection','file-sel')+'  '+os(a,'magit-diff-hunk-heading-selection','hunk-sel')+'  '+os(a,'magit-section-heading-selection','sec-sel')+'  '+os(a,'magit-diff-revision-summary-highlight','rev-sum-hl'));
  L.push('');
  L.push(os(a,'magit-section-heading','Reflog'));
  L.push('  '+os(a,'magit-reflog-commit','commit')+' '+os(a,'magit-reflog-amend','amend')+' '+os(a,'magit-reflog-merge','merge')+' '+os(a,'magit-reflog-checkout','checkout')+' '+os(a,'magit-reflog-reset','reset')+' '+os(a,'magit-reflog-rebase','rebase')+' '+os(a,'magit-reflog-cherry-pick','cherry-pick')+' '+os(a,'magit-reflog-remote','remote')+' '+os(a,'magit-reflog-other','other'));
  L.push(os(a,'magit-section-heading','Rebase sequence'));
  L.push('  '+os(a,'magit-sequence-pick','pick')+' '+os(a,'magit-sequence-stop','stop')+' '+os(a,'magit-sequence-part','part')+' '+os(a,'magit-sequence-head','head')+' '+os(a,'magit-sequence-drop','drop')+' '+os(a,'magit-sequence-done','done')+' '+os(a,'magit-sequence-onto','onto')+' '+os(a,'magit-sequence-exec','exec'));
  L.push(os(a,'magit-section-heading','Bisect / Cherry / Process'));
  L.push('  '+os(a,'magit-bisect-good','good')+' '+os(a,'magit-bisect-bad','bad')+' '+os(a,'magit-bisect-skip','skip')+'   '+os(a,'magit-cherry-equivalent','equivalent')+' '+os(a,'magit-cherry-unmatched','unmatched'));
  L.push('  '+os(a,'magit-process-ok','OK')+' '+os(a,'magit-process-ng','NG')+'   '+os(a,'magit-mode-line-process','[fetch]')+' '+os(a,'magit-mode-line-process-error','[error]'));
  L.push(os(a,'magit-section-heading','Blame'));
  L.push(os(a,'magit-blame-margin','margin')+os(a,'magit-blame-heading',' b5b1869f '))
  L.push('  '+os(a,'magit-blame-hash','b5b1869f')+' '+os(a,'magit-blame-name','Craig')+' '+os(a,'magit-blame-date','2026-06-08')+' '+os(a,'magit-blame-summary','enlarge picker')+' '+os(a,'magit-blame-highlight','hl')+' '+os(a,'magit-blame-dimmed','dim'));
  L.push(os(a,'magit-section-heading','Signatures')+os(a,'magit-left-margin','  '));
  L.push('  '+os(a,'magit-signature-good','good')+' '+os(a,'magit-signature-bad','bad')+' '+os(a,'magit-signature-untrusted','untrusted')+' '+os(a,'magit-signature-expired','expired')+' '+os(a,'magit-signature-expired-key','expired-key')+' '+os(a,'magit-signature-revoked','revoked')+' '+os(a,'magit-signature-error','error'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderElfeedPreview(){const a='elfeed',L=[];
  L.push(os(a,'elfeed-search-filter-face','@6-months-ago +unread')+'   '+os(a,'elfeed-search-unread-count-face','3/120')+'   '+os(a,'elfeed-search-last-update-face','updated 02:24'));
  L.push('');
  L.push(os(a,'elfeed-search-date-face','2026-06-08')+'  '+os(a,'elfeed-search-feed-face','Planet Emacs')+'  '+os(a,'elfeed-search-unread-title-face','New release of Magit')+'  '+os(a,'elfeed-search-tag-face',':emacs:'));
  L.push(os(a,'elfeed-search-date-face','2026-06-07')+'  '+os(a,'elfeed-search-feed-face','LWN')+'  '+os(a,'elfeed-search-unread-title-face','Kernel 6.18 lands')+'  '+os(a,'elfeed-search-tag-face',':linux:'));
  L.push(os(a,'elfeed-search-date-face','2026-06-05')+'  '+os(a,'elfeed-search-feed-face','Hacker News')+'  '+os(a,'elfeed-search-title-face','Show HN: a theme editor')+'  '+os(a,'elfeed-search-tag-face',':show:'));
  L.push('');
  L.push(os(a,'elfeed-log-date-face','02:24:01')+'  '+os(a,'elfeed-log-info-level-face','INFO ')+' updated 12 feeds');
  L.push(os(a,'elfeed-log-date-face','02:24:02')+'  '+os(a,'elfeed-log-warn-level-face','WARN ')+' slow feed: example.com');
  L.push(os(a,'elfeed-log-date-face','02:24:03')+'  '+os(a,'elfeed-log-error-level-face','ERROR')+' failed: bad.example');
  L.push(os(a,'elfeed-log-date-face','02:24:04')+'  '+os(a,'elfeed-log-debug-level-face','DEBUG')+' parsed 340 entries');
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderGhostelPreview(){const a='ghostel',L=[];
  L.push(os(a,'ghostel-default','craig@host')+' '+os(a,'ghostel-color-green','~/code')+' $ ls'+os(a,'ghostel-fake-cursor',' ')+os(a,'ghostel-fake-cursor-box','[ ]'));
  L.push('');
  L.push(os(a,'ghostel-default','normal:')+'  '+os(a,'ghostel-color-black','black')+' '+os(a,'ghostel-color-red','red')+' '+os(a,'ghostel-color-green','green')+' '+os(a,'ghostel-color-yellow','yellow')+' '+os(a,'ghostel-color-blue','blue')+' '+os(a,'ghostel-color-magenta','magenta')+' '+os(a,'ghostel-color-cyan','cyan')+' '+os(a,'ghostel-color-white','white'));
  L.push(os(a,'ghostel-default','bright:')+'  '+os(a,'ghostel-color-bright-black','black')+' '+os(a,'ghostel-color-bright-red','red')+' '+os(a,'ghostel-color-bright-green','green')+' '+os(a,'ghostel-color-bright-yellow','yellow')+' '+os(a,'ghostel-color-bright-blue','blue')+' '+os(a,'ghostel-color-bright-magenta','magenta')+' '+os(a,'ghostel-color-bright-cyan','cyan')+' '+os(a,'ghostel-color-bright-white','white'));
  L.push('');
  L.push(os(a,'ghostel-default','default terminal output, 256-color text and a blinking ')+os(a,'ghostel-fake-cursor','cursor')+'.');
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderDashboardPreview(){const a='dashboard',L=[];
  L.push(os(a,'dashboard-text-banner','   ___ _ __ ___   __ _  ___ ___'));
  L.push(os(a,'dashboard-banner-logo-title','   Welcome back, Craig'));
  L.push('');
  L.push(os(a,'dashboard-heading','Recent Files'));
  L.push('  '+os(a,'dashboard-items-face','init.el'));
  L.push('  '+os(a,'dashboard-items-face','notes.org'));
  L.push(os(a,'dashboard-heading','Bookmarks'));
  L.push('  '+os(a,'dashboard-no-items-face','-- no items --'));
  L.push('');
  L.push(os(a,'dashboard-navigator','[ Projects ]  [ Recent ]  [ Agenda ]'));
  L.push(os(a,'dashboard-footer-icon-face','*')+' '+os(a,'dashboard-footer-face','Happy hacking, Craig!'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderMu4ePreview(){const a='mu4e',L=[];
  L.push(os(a,'mu4e-title-face','mu4e')+'  '+os(a,'mu4e-context-face','[Personal]')+'  '+os(a,'mu4e-ok-face','online')+'  '+os(a,'mu4e-warning-face','2 retry')+'  '+os(a,'mu4e-modeline-face','12/340'));
  L.push('');
  L.push(os(a,'mu4e-header-title-face','Date        Flags  From          Subject'));
  L.push(os(a,'mu4e-header-value-face','2026-06-08')+'  '+os(a,'mu4e-header-marks-face','N')+'    '+os(a,'mu4e-unread-face','Alice')+'         '+os(a,'mu4e-unread-face','Unread message'));
  L.push(os(a,'mu4e-header-value-face','2026-06-07')+'  '+os(a,'mu4e-header-marks-face','R')+'    '+os(a,'mu4e-header-face','Bob')+'           '+os(a,'mu4e-replied-face','Replied thread'));
  L.push(os(a,'mu4e-header-value-face','2026-06-06')+'  '+os(a,'mu4e-header-marks-face','F')+'    '+os(a,'mu4e-header-face','Carol')+'         '+os(a,'mu4e-forwarded-face','Forwarded note'));
  L.push(os(a,'mu4e-header-value-face','2026-06-05')+'  '+os(a,'mu4e-header-marks-face','D')+'    '+os(a,'mu4e-draft-face','(draft)')+'       '+os(a,'mu4e-draft-face','Draft in progress'));
  L.push(os(a,'mu4e-header-value-face','2026-06-04')+'  '+os(a,'mu4e-header-marks-face','T')+'    '+os(a,'mu4e-trashed-face','Dan')+'           '+os(a,'mu4e-moved-face','Trashed and moved'));
  L.push(os(a,'mu4e-header-highlight-face','2026-06-03  !    Eve           Flagged ')+os(a,'mu4e-flagged-face','important')+os(a,'mu4e-related-face',' (related)'));
  L.push('');
  L.push(os(a,'mu4e-header-key-face','From:')+'   '+os(a,'mu4e-contact-face','Alice &lt;alice@example.com&gt;'));
  L.push(os(a,'mu4e-header-key-face','To:')+'     '+os(a,'mu4e-special-header-value-face','craig, list@gnu.org'));
  L.push(os(a,'mu4e-header-key-face','Attach:')+' '+os(a,'mu4e-attach-number-face','[1]')+' report.pdf   link '+os(a,'mu4e-url-number-face','[2]')+' '+os(a,'mu4e-link-face','https://gnu.org'));
  L.push('');
  L.push('  body with a '+os(a,'mu4e-highlight-face','search hit')+' and '+os(a,'mu4e-region-code','code region')+'.');
  L.push('  '+os(a,'mu4e-cited-1-face','&gt; level 1')+' '+os(a,'mu4e-cited-2-face','&gt;&gt; 2')+' '+os(a,'mu4e-cited-3-face','&gt;&gt;&gt; 3')+' '+os(a,'mu4e-cited-4-face','&gt; 4')+' '+os(a,'mu4e-cited-5-face','&gt; 5')+' '+os(a,'mu4e-cited-6-face','&gt; 6')+' '+os(a,'mu4e-cited-7-face','&gt; 7'));
  L.push('  '+os(a,'mu4e-system-face','*** system message ***')+'   '+os(a,'mu4e-footer-face','-- sent with mu4e'));
  L.push('');
  L.push(os(a,'mu4e-compose-header-face','Subject:')+' new mail');
  L.push(os(a,'mu4e-compose-separator-face','--text follows this line--'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderLspPreview(){const a='lsp-mode',L=[];
  L.push(os(a,'lsp-signature-face','process(')+os(a,'lsp-signature-highlight-function-argument','items: list')+os(a,'lsp-signature-face',') -> None'));
  L.push(os(a,'lsp-signature-posframe',' docs: iterate over items and process each one '));
  L.push('');
  L.push('def process(items):');
  L.push('    n = len(items)'+os(a,'lsp-inlay-hint-type-face',': int'));
  L.push('    handle('+os(a,'lsp-inlay-hint-parameter-face','arg:')+'n)'+os(a,'lsp-inlay-hint-face','   # hint'));
  L.push('    '+os(a,'lsp-face-highlight-read','value')+' = '+os(a,'lsp-face-highlight-write','value')+' + '+os(a,'lsp-face-highlight-textual','value'));
  L.push('    rename '+os(a,'lsp-face-rename','oldName')+' to '+os(a,'lsp-rename-placeholder-face','newName'));
  L.push('    getName()   '+os(a,'lsp-details-face','str   the cached getter'));
  L.push('');
  L.push(os(a,'lsp-installation-buffer-face','Installing pyright...')+'   '+os(a,'lsp-installation-finished-buffer-face','done.'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderGitGutterPreview(){const a='git-gutter',L=[];
  L.push(os(a,'git-gutter:added','+')+os(a,'git-gutter:separator','|')+' added line of code');
  L.push(os(a,'git-gutter:modified','~')+os(a,'git-gutter:separator','|')+' modified line of code');
  L.push(os(a,'git-gutter:deleted','_')+os(a,'git-gutter:separator','|')+' (deleted lines marker)');
  L.push(os(a,'git-gutter:unchanged',' ')+os(a,'git-gutter:separator','|')+' '+os(a,'git-gutter:unchanged','unchanged line of code'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderFlycheckPreview(){const a='flycheck',L=[];
  L.push(os(a,'flycheck-fringe-error','E')+os(a,'flycheck-fringe-warning','W')+os(a,'flycheck-fringe-info','I')+'  x = '+os(a,'flycheck-error','undefined_name')+'('+os(a,'flycheck-warning','unused_arg')+')  '+os(a,'flycheck-info','# note'));
  L.push('       '+os(a,'flycheck-delimited-error','[')+os(a,'flycheck-error-delimiter','err')+os(a,'flycheck-delimited-error',']'));
  L.push('');
  L.push(os(a,'flycheck-error-list-checker-name','pyright')+'   '+os(a,'flycheck-verify-select-checker','(selected checker)'));
  L.push(os(a,'flycheck-error-list-filename','main.py')+':'+os(a,'flycheck-error-list-line-number','12')+':'+os(a,'flycheck-error-list-column-number','4')+'  '+os(a,'flycheck-error-list-error','error')+'    '+os(a,'flycheck-error-list-error-message','undefined name x')+'  '+os(a,'flycheck-error-list-id','[E0602]'));
  L.push(os(a,'flycheck-error-list-filename','main.py')+':'+os(a,'flycheck-error-list-line-number','18')+':'+os(a,'flycheck-error-list-column-number','1')+'  '+os(a,'flycheck-error-list-warning','warning')+'  '+os(a,'flycheck-error-list-error-message','unused import')+'  '+os(a,'flycheck-error-list-id-with-explainer','[W0611?]'));
  L.push(os(a,'flycheck-error-list-highlight','main.py:20    '+os(a,'flycheck-error-list-info','info')+'     highlighted row'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderDiredPreview(){const a='dired',L=[];
  L.push(os(a,'dired-header','/home/craig/code:'));
  L.push('  '+os(a,'dired-perm-write','drwxr-xr-x')+'  craig  4096  '+os(a,'dired-directory','src/'));
  L.push('  -rw-r--r--  craig   120  notes.org');
  L.push('  '+os(a,'dired-perm-write','lrwxrwxrwx')+'  craig    18  '+os(a,'dired-symlink','latest -> v2.1'));
  L.push('  lrwxrwxrwx  craig    --  '+os(a,'dired-broken-symlink','dead -> gone'));
  L.push(os(a,'dired-flagged','D')+' -rw-r--r--  craig    40  deleteme.tmp');
  L.push(os(a,'dired-mark','*')+' '+os(a,'dired-marked','-rw-r--r--  craig   210  marked.txt'));
  L.push('  -rw-r--r--  craig     0  '+os(a,'dired-ignored','.gitignore'));
  L.push('  '+os(a,'dired-set-id','-rwsr-xr-x')+'  root    900  setuid.bin');
  L.push('  '+os(a,'dired-special','prw-r--r--')+'  craig     0  fifo.pipe');
  L.push(os(a,'dired-warning','! disk space low on /home'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderDirvishPreview(){const a='dirvish',L=[];
  L.push(os(a,'dirvish-inactive','~/code')+'    '+os(a,'dirvish-free-space','[free 24G]'));
  L.push(os(a,'dirvish-hl-line',' '+os(a,'dirvish-file-modes','-rw-r--r--')+' '+os(a,'dirvish-file-link-number','1')+' '+os(a,'dirvish-file-user-id','craig')+' '+os(a,'dirvish-file-group-id','staff')+' '+os(a,'dirvish-file-size','4.0K')+' '+os(a,'dirvish-file-time','Jun  8 02:24')+'  init.el '));
  L.push(' '+os(a,'dirvish-file-modes','drwxr-xr-x')+' '+os(a,'dirvish-file-link-number','5')+' '+os(a,'dirvish-file-user-id','craig')+' '+os(a,'dirvish-file-group-id','staff')+' '+os(a,'dirvish-file-size','  - ')+' '+os(a,'dirvish-file-time','Jun  7 18:00')+'  '+os(a,'dirvish-collapse-dir-face','src')+os(a,'dirvish-subtree-state','+')+os(a,'dirvish-subtree-guide',' |'));
  L.push(os(a,'dirvish-hl-line-inactive',' inactive-window current line '));
  L.push(' inode '+os(a,'dirvish-file-inode-number','1048576')+'  dev '+os(a,'dirvish-file-device-number','8,1')+'   '+os(a,'dirvish-collapse-empty-dir-face','empty/')+' '+os(a,'dirvish-collapse-file-face','file.txt'));
  L.push(' VC '+os(a,'dirvish-vc-added-state','A')+os(a,'dirvish-vc-edited-state','M')+os(a,'dirvish-vc-removed-state','D')+os(a,'dirvish-vc-conflict-state','C')+os(a,'dirvish-vc-locked-state','L')+os(a,'dirvish-vc-missing-state','!')+os(a,'dirvish-vc-needs-merge-face','m')+os(a,'dirvish-vc-needs-update-state','u')+os(a,'dirvish-vc-unregistered-face','?'));
  L.push(' git '+os(a,'dirvish-git-commit-message-face','feat: enlarge the picker'));
  L.push(' '+os(a,'dirvish-media-info-heading','Media')+'  '+os(a,'dirvish-media-info-property-key','Dimensions:')+' 1920x1080');
  L.push(' proc '+os(a,'dirvish-proc-running','running')+' / '+os(a,'dirvish-proc-finished','finished')+' / '+os(a,'dirvish-proc-failed','failed'));
  L.push(' narrow '+os(a,'dirvish-narrow-match-face-0','m0')+' '+os(a,'dirvish-narrow-match-face-1','m1')+' '+os(a,'dirvish-narrow-match-face-2','m2')+' '+os(a,'dirvish-narrow-match-face-3','m3')+os(a,'dirvish-narrow-split',' | ')+os(a,'dirvish-emerge-group-title','Group: images'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderCalibredbPreview(){const a='calibredb',L=[];
  L.push(os(a,'calibredb-search-header-library-name-face','Calibre')+'  '+os(a,'calibredb-search-header-library-path-face','~/books')+'  '+os(a,'calibredb-search-header-total-face','412 books')+'  '+os(a,'calibredb-search-header-filter-face','tag:scifi')+'  '+os(a,'calibredb-search-header-sort-face','sort:date')+'  '+os(a,'calibredb-search-header-highlight-face','[*]'));
  L.push('');
  L.push(os(a,'calibredb-id-face','1')+'  '+os(a,'calibredb-title-face','Dune')+'  '+os(a,'calibredb-author-face','Herbert')+'  '+os(a,'calibredb-format-face','EPUB')+'  '+os(a,'calibredb-size-face','2.1M')+'  '+os(a,'calibredb-tag-face',':scifi:')+'  '+os(a,'calibredb-date-face','2026-06-08'));
  L.push(os(a,'calibredb-mark-face','*')+os(a,'calibredb-id-face','2')+'  '+os(a,'calibredb-title-face','Foundation')+'  '+os(a,'calibredb-author-face','Asimov')+'  '+os(a,'calibredb-series-face','[Foundation #1]')+'  '+os(a,'calibredb-publisher-face','Bantam')+'  '+os(a,'calibredb-pubdate-face','1951'));
  L.push('');
  L.push(os(a,'calibredb-title-detailed-view-face','Foundation (detailed)')+'   '+os(a,'calibredb-language-face','eng')+'  '+os(a,'calibredb-favorite-face','* fav')+'  '+os(a,'calibredb-archive-face','archived'));
  L.push(os(a,'calibredb-ids-face','isbn:0553293354')+'  '+os(a,'calibredb-file-face','foundation.epub')+'  '+os(a,'calibredb-comment-face','A classic of the genre.'));
  L.push(os(a,'calibredb-edit-annotation-header-title-face','Annotations')+'  '+os(a,'calibredb-highlight-face','highlighted passage')+'  '+os(a,'calibredb-current-page-button-face','[page 42]')+'  '+os(a,'calibredb-mouse-face','hover row'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderErcPreview(){const a='erc',L=[];
  L.push(os(a,'erc-header-line',' #emacs on Libera.Chat  18 users '));
  L.push(os(a,'erc-timestamp-face','[10:24]')+' '+os(a,'erc-notice-face','*** alice has joined #emacs'));
  L.push(os(a,'erc-timestamp-face','[10:25]')+' &lt;'+os(a,'erc-my-nick-prefix-face','@')+os(a,'erc-my-nick-face','craig')+'&gt; '+os(a,'erc-default-face','hello everyone'));
  L.push(os(a,'erc-timestamp-face','[10:25]')+' &lt;'+os(a,'erc-nick-prefix-face','+')+os(a,'erc-nick-default-face','bob')+'&gt; '+os(a,'erc-input-face','hi craig, see ')+os(a,'erc-button','this link')+os(a,'erc-input-face',' cc ')+os(a,'erc-button-nick-default-face','@alice'));
  L.push(os(a,'erc-timestamp-face','[10:26]')+' '+os(a,'erc-action-face','* craig waves')+'   '+os(a,'erc-keyword-face','emacs')+' '+os(a,'erc-pal-face','&lt;friend&gt;')+' '+os(a,'erc-fool-face','&lt;troll&gt;')+' '+os(a,'erc-dangerous-host-face','&lt;bad@host&gt;'));
  L.push(os(a,'erc-timestamp-face','[10:27]')+' '+os(a,'erc-direct-msg-face','(DM)')+' &lt;'+os(a,'erc-nick-msg-face','bob')+'&gt; psst   '+os(a,'erc-current-nick-face','craig')+'   '+os(a,'erc-information','-info-'));
  L.push(os(a,'erc-error-face','*** ERROR: connection reset'));
  L.push(os(a,'erc-command-indicator-face','/help')+'   '+os(a,'erc-bold-face','bold')+' '+os(a,'erc-italic-face','italic')+' '+os(a,'erc-underline-face','underline')+' '+os(a,'erc-inverse-face','inverse')+' '+os(a,'erc-spoiler-face','spoiler'));
  L.push(os(a,'erc-keep-place-indicator-arrow','&gt;')+os(a,'erc-keep-place-indicator-line',' ---- last read ---- ')+os(a,'erc-fill-wrap-merge-indicator-face','+'));
  L.push(os(a,'erc-prompt-face','craig&gt;')+' '+os(a,'erc-input-face','type a message...'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderOrgdrillPreview(){const a='org-drill',L=[];
  L.push('Q: The capital of France is '+os(a,'org-drill-hidden-cloze-face','[...]')+'.');
  L.push('A: The capital of France is '+os(a,'org-drill-visible-cloze-face','Paris')+'.');
  L.push('   '+os(a,'org-drill-visible-cloze-hint-face','hint: P____'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderOrgnoterPreview(){const a='org-noter',L=[];
  L.push('org-noter   paper.pdf');
  L.push('  page 1   '+os(a,'org-noter-notes-exist-face','[notes]'));
  L.push('  page 2   '+os(a,'org-noter-no-notes-exist-face','[no notes]'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderSignelPreview(){const a='signel',L=[];
  L.push(os(a,'signel-timestamp-face','[10:24]')+' '+os(a,'signel-my-msg-face','Me: hey, are we still on for tonight?'));
  L.push(os(a,'signel-timestamp-face','[10:25]')+' '+os(a,'signel-other-msg-face','Alice: yes! see you at 7'));
  L.push(os(a,'signel-error-face','(failed to send -- retrying)'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderPearlPreview(){const a='pearl',L=[];
  L.push(os(a,'pearl-preamble-summary','PEARL-42  Fix the broken picker'));
  L.push('State: '+os(a,'pearl-modified-local','In Progress')+'   Priority: '+os(a,'pearl-modified-highlight','High')+'   Estimate: '+os(a,'pearl-modified-unknown','?'));
  L.push('  '+os(a,'pearl-editable-comment','&gt; add a comment (editable)'));
  L.push('  '+os(a,'pearl-readonly-comment','&gt; created by automation (read-only)'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderShrPreview(){const a='shr',L=[];
  L.push(os(a,'shr-text','shr renders nov (EPUB), eww (web), elfeed, and HTML mail.'));
  L.push('');
  L.push(os(a,'shr-h1','Chapter One: The Beginning'));
  L.push(os(a,'shr-h2','A Section Heading'));
  L.push(os(a,'shr-h3','A subsection')+'   '+os(a,'shr-h4','h4')+' / '+os(a,'shr-h5','h5')+' / '+os(a,'shr-h6','h6'));
  L.push(os(a,'shr-text','Body text flows in shr-text, with a ')+os(a,'shr-link','hyperlink')+os(a,'shr-text',' and a ')+os(a,'shr-selected-link','focused link')+os(a,'shr-text',','));
  L.push(os(a,'shr-text','some ')+os(a,'shr-code','inline_code()')+os(a,'shr-text',', a ')+os(a,'shr-mark','highlighted mark')+os(a,'shr-text',', ')+os(a,'shr-strike-through','struck out')+os(a,'shr-text',', a footnote')+os(a,'shr-sup','[1]')+os(a,'shr-text',','));
  L.push(os(a,'shr-text','an ')+os(a,'shr-abbreviation','HTML')+os(a,'shr-text',' abbreviation, and an ')+os(a,'shr-sliced-image','[image]')+os(a,'shr-text',' slice.'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderSlackPreview(){const a='slack',L=[];
  L.push(os(a,'slack-room-info-title-room-name-face','#general')+'  '+os(a,'slack-room-info-title-face','Acme Workspace'));
  L.push(os(a,'slack-room-info-section-title-face','Topic')+'  '+os(a,'slack-room-info-section-label-face','daily standup')+'   '+os(a,'slack-room-unread-face','3 unread'));
  L.push(os(a,'slack-new-message-marker-face','---------------- new messages ----------------'));
  L.push(os(a,'slack-message-output-header','craig  10:24'));
  L.push('  '+os(a,'slack-message-output-text','hey ')+os(a,'slack-message-mention-me-face','@craig')+os(a,'slack-message-output-text',', see ')+os(a,'slack-message-mention-face','@alice')+os(a,'slack-message-output-text',' in ')+os(a,'slack-channel-button-face','#general')+'  '+os(a,'slack-message-mention-keyword-face','urgent'));
  L.push('  '+os(a,'slack-mrkdwn-bold-face','*bold*')+' '+os(a,'slack-mrkdwn-italic-face','_italic_')+' '+os(a,'slack-mrkdwn-code-face','`code`')+' '+os(a,'slack-mrkdwn-strike-face','~strike~'));
  L.push('  '+os(a,'slack-mrkdwn-blockquote-face','&gt; quoted')+'   '+os(a,'slack-mrkdwn-list-face','- item'));
  L.push('  '+os(a,'slack-mrkdwn-code-block-face','``` code block ```'));
  L.push('  '+os(a,'slack-message-output-reaction',':thumbsup: 3')+'  '+os(a,'slack-message-output-reaction-pressed',':heart: 1')+'   '+os(a,'slack-message-deleted-face','(message deleted)'));
  L.push('  '+os(a,'slack-all-thread-buffer-thread-header-face','Thread: 2 replies'));
  L.push(os(a,'slack-attachment-header','Attachment')+'  '+os(a,'slack-attachment-field-title','Field:')+' val  '+os(a,'slack-message-attachment-preview-header-face','Preview')+'  '+os(a,'slack-preview-face','snippet')+os(a,'slack-attachment-pad',' | ')+os(a,'slack-attachment-footer','footer'));
  L.push(os(a,'slack-block-highlight-source-overlay-face',' highlighted source block '));
  L.push('Actions: '+os(a,'slack-message-action-face','Edit')+' '+os(a,'slack-message-action-primary-face','Approve')+' '+os(a,'slack-message-action-danger-face','Delete'));
  L.push('Blocks: '+os(a,'slack-button-block-element-face','[Button]')+os(a,'slack-button-primary-block-element-face','[Primary]')+os(a,'slack-button-danger-block-element-face','[Danger]')+os(a,'slack-select-block-element-face','[Select v]')+os(a,'slack-overflow-block-element-face','[...]')+os(a,'slack-date-picker-block-element-face','[Date]'));
  L.push('Dialog: '+os(a,'slack-dialog-title-face','Title')+'  '+os(a,'slack-dialog-element-label-face','Label')+' '+os(a,'slack-dialog-element-hint-face','(hint)')+' '+os(a,'slack-dialog-element-placeholder-face','placeholder')+' '+os(a,'slack-dialog-element-error-face','error')+'  '+os(a,'slack-dialog-select-element-input-face','[input v]')+' '+os(a,'slack-dialog-submit-button-face','[Submit]')+os(a,'slack-dialog-cancel-button-face','[Cancel]'));
  L.push('Users: '+os(a,'slack-user-active-face','alice (active)')+' '+os(a,'slack-user-dnd-face','bob (dnd)')+'  '+os(a,'slack-profile-image-face','[img]')+' '+os(a,'slack-user-profile-header-face','Profile')+' '+os(a,'slack-user-profile-property-name-face','Title:')+' Dev');
  L.push('Search: '+os(a,'slack-search-result-message-header-face','#general')+' '+os(a,'slack-search-result-message-username-face','craig'));
  L.push('Modeline: '+os(a,'slack-modeline-has-unreads-face','* unreads')+' '+os(a,'slack-modeline-channel-has-unreads-face','#ch')+' '+os(a,'slack-modeline-thread-has-unreads-face','thread'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function renderTelegaPreview(){const a='telega',L=[];
  L.push(os(a,'telega-root-heading','Telegram')+'  '+os(a,'telega-tracking','[tracking]')+'  '+os(a,'telega-unread-unmuted-modeline','5 unread'));
  L.push(os(a,'telega-has-chatbuf-brackets','[')+os(a,'telega-username','Alice')+os(a,'telega-has-chatbuf-brackets',']')+' '+os(a,'telega-user-online-status','online')+'  '+os(a,'telega-unmuted-count','3')+' '+os(a,'telega-mention-count','@2')+os(a,'telega-delim-face',' | ')+os(a,'telega-secret-title','Secret')+' '+os(a,'telega-muted-count','muted'));
  L.push(os(a,'telega-username','Bob')+' '+os(a,'telega-user-non-online-status','last seen recently')+'   '+os(a,'telega-contact-birthdays-today','birthday today')+'   '+os(a,'telega-shadow','shadow')+' '+os(a,'telega-link','link')+' '+os(a,'telega-blue','blue')+' '+os(a,'telega-red','red'));
  L.push('');
  L.push(os(a,'telega-msg-heading','Today'));
  L.push(os(a,'telega-msg-user-title','Alice')+'  '+os(a,'telega-msg-inline-reply','| reply to Bob')+'  '+os(a,'telega-msg-inline-forward','fwd from Carol')+'  '+os(a,'telega-msg-inline-other','via bot'));
  L.push('  '+os(a,'telega-entity-type-bold','bold')+' '+os(a,'telega-entity-type-italic','italic')+' '+os(a,'telega-entity-type-underline','underline')+' '+os(a,'telega-entity-type-strikethrough','strike')+' '+os(a,'telega-entity-type-code','code')+' '+os(a,'telega-entity-type-spoiler','spoiler'));
  L.push('  '+os(a,'telega-entity-type-pre','pre block')+'  '+os(a,'telega-entity-type-blockquote','&gt; quote')+'  '+os(a,'telega-entity-type-mention','@user')+' '+os(a,'telega-entity-type-hashtag','#tag')+' '+os(a,'telega-entity-type-cashtag','$USD')+' '+os(a,'telega-entity-type-botcommand','/start')+' '+os(a,'telega-entity-type-texturl','link'));
  L.push(os(a,'telega-msg-self-title','Me')+'  '+os(a,'telega-reaction',':+1: 2')+' '+os(a,'telega-reaction-chosen',':heart: 1')+' '+os(a,'telega-reaction-paid',':star: 5')+' '+os(a,'telega-reaction-paid-chosen',':star: paid')+'   '+os(a,'telega-msg-deleted','(deleted)')+' '+os(a,'telega-msg-sponsored','Sponsored'));
  L.push('  checklist '+os(a,'telega-checklist-stats-done','2 done')+' / '+os(a,'telega-checklist-stats-todo','3 todo')+'   '+os(a,'telega-highlight-text-face','search hit')+'   '+os(a,'telega-button-highlight','[active btn]'));
  L.push(os(a,'telega-chat-prompt','&gt;')+' '+os(a,'telega-chat-prompt-aux','reply')+' '+os(a,'telega-chat-input-attachment','[photo.jpg]')+'   '+os(a,'telega-topic-button','# Topic')+'   '+os(a,'telega-filter-active','Main')+' '+os(a,'telega-filter-button-active','[Unread]')+os(a,'telega-filter-button-inactive','[All]'));
  L.push('Buttons '+os(a,'telega-box-button','[box]')+os(a,'telega-box-button-active','[on]')+os(a,'telega-box-button-default-active','[def]')+os(a,'telega-box-button-default-passive','[def-]')+os(a,'telega-box-button-primary-active','[pri]')+os(a,'telega-box-button-primary-passive','[pri-]')+os(a,'telega-box-button-success-active','[ok]')+os(a,'telega-box-button-success-passive','[ok-]'));
  L.push('        '+os(a,'telega-box-button-danger-active','[del]')+os(a,'telega-box-button-danger-passive','[del-]')+os(a,'telega-box-button-ui-active','[ui]')+os(a,'telega-box-button-ui-passive','[ui-]')+os(a,'telega-box-button2-active','[b2]')+os(a,'telega-box-button2-passive','[b2-]')+os(a,'telega-box-button2-white-foreground','[b2w]'));
  L.push('Describe '+os(a,'telega-describe-section-title','Section')+' '+os(a,'telega-describe-subsection-title','Sub')+' '+os(a,'telega-describe-item-title','Item:')+'   enckey '+os(a,'telega-enckey-00','00')+os(a,'telega-enckey-01','01')+os(a,'telega-enckey-10','10')+os(a,'telega-enckey-11','11'));
  L.push('Palette '+os(a,'telega-palette-builtin-blue','blue')+' '+os(a,'telega-palette-builtin-green','green')+' '+os(a,'telega-palette-builtin-orange','orange')+' '+os(a,'telega-palette-builtin-purple','purple'));
  L.push(os(a,'telega-link-preview-sitename','example.com')+'  '+os(a,'telega-link-preview-title','Link preview title'));
  L.push('Webpage '+os(a,'telega-webpage-title','Title')+' '+os(a,'telega-webpage-subtitle','Subtitle')+' '+os(a,'telega-webpage-header','Header')+' '+os(a,'telega-webpage-subheader','Subheader')+' '+os(a,'telega-webpage-outline','outline')+' '+os(a,'telega-webpage-fixed','fixed')+' '+os(a,'telega-webpage-preformatted','pre')+' '+os(a,'telega-webpage-marked','marked')+' '+os(a,'telega-webpage-strike-through','strike')+' '+os(a,'telega-webpage-chat-link','chat-link'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\\n')}</div>`;}
function genericPreview(app){let h='<div style="padding:10px 14px;font:12pt/1.8 monospace">';for(const [face,label,def] of APPS[app].faces){const f=PKGMAP[app][face],efg=pkgEffFg(app,face)||MAP['p'],ebg=pkgEffBg(app,face);h+=`<div data-face="${face}" style="color:${efg};${ebg?'background:'+ebg+';':''}font-weight:${f.bold?'bold':'normal'};font-style:${f.italic?'italic':'normal'};font-size:${(f.height||1)}em">${esc(label)}</div>`;}return h+'</div>';}
function buildPkgPreview(){const app=curApp(),p=document.getElementById('pkgpreview');if(!p)return;const pv=APPS[app].preview;const bespoke=['org','magit','elfeed','ghostel','dashboard','mu4e','lsp','gitgutter','flycheck','dired','dirvish','calibredb','erc','orgdrill','orgnoter','signel','pearl','slack','telega','shr'].includes(pv);p.innerHTML=pv==='org'?renderOrgPreview():pv==='magit'?renderMagitPreview():pv==='elfeed'?renderElfeedPreview():pv==='ghostel'?renderGhostelPreview():pv==='dashboard'?renderDashboardPreview():pv==='mu4e'?renderMu4ePreview():pv==='lsp'?renderLspPreview():pv==='gitgutter'?renderGitGutterPreview():pv==='flycheck'?renderFlycheckPreview():pv==='dired'?renderDiredPreview():pv==='dirvish'?renderDirvishPreview():pv==='calibredb'?renderCalibredbPreview():pv==='erc'?renderErcPreview():pv==='orgdrill'?renderOrgdrillPreview():pv==='orgnoter'?renderOrgnoterPreview():pv==='signel'?renderSignelPreview():pv==='pearl'?renderPearlPreview():pv==='slack'?renderSlackPreview():pv==='telega'?renderTelegaPreview():pv==='shr'?renderShrPreview():genericPreview(app);p.style.background=MAP['bg'];p.onclick=(e)=>{const u=e.target.closest('[data-face]');if(u)flashPkg(u.dataset.face);};const lbl=document.getElementById('pkgprevlabel');if(lbl)lbl.textContent=bespoke?(APPS[app].label+' preview'):'preview (generic — face names in their own colors)';}
function resetApp(){const app=curApp();PKGMAP[app]={};for(const [face,label,d] of APPS[app].faces)PKGMAP[app][face]=seedFace(d);pkgChanged();}
function syncPkgHeight(){const t=document.getElementById('pkgtable'),m=document.getElementById('pkgpreview');if(!t||!m)return;const lb=m.previousElementSibling,lbh=lb?lb.getBoundingClientRect().height+10:30;m.style.height=Math.max(t.getBoundingClientRect().height-lbh,220)+'px';}
function paintUI(face){const pv=document.getElementById('uiprev-'+face);if(!pv)return;const o=UIMAP[face];pv.style.color=o.fg||MAP['p'];pv.style.background=o.bg||MAP['bg'];pv.style.fontWeight=o.bold?'bold':'normal';pv.style.fontStyle=o.italic?'italic':'normal';pv.style.textDecoration=(o.underline?'underline ':'')+(o.strike?'line-through':'')||'none';}
function buildUITable(){
  const tb=document.getElementById('uibody');tb.innerHTML='';
  for(const [face,label,ex] of UI_FACES){
    const tr=document.createElement('tr');tr.dataset.face=face;
    const c0=document.createElement('td');c0.className='cat';c0.textContent=label;c0.style.cursor='pointer';c0.title='flash this face in the live preview';c0.onclick=()=>flashUiPreview(face);
    const cF=document.createElement('td');cF.appendChild(uiSelect(face,'fg'));
    const cB=document.createElement('td');cB.appendChild(uiSelect(face,'bg'));
    const cS=document.createElement('td');[['B','bold'],['I','italic'],['U','underline'],['S','strike']].forEach(([ch,at])=>{const b=document.createElement('button');b.className='sbtn'+(UIMAP[face][at]?' on':'');b.textContent='a';b.style.fontWeight=at==='bold'?'bold':'normal';b.style.fontStyle=at==='italic'?'italic':'normal';b.style.textDecoration=at==='underline'?'underline':at==='strike'?'line-through':'none';b.title=at;b.onclick=()=>{UIMAP[face][at]=!UIMAP[face][at];paintUI(face);buildMockFrame();};cS.appendChild(b);});
    const cP=document.createElement('td');cP.className='ex';cP.id='uiprev-'+face;cP.textContent=ex;cP.style.padding='4px 10px';cP.style.borderRadius='4px';
    tr.appendChild(c0);tr.appendChild(cF);tr.appendChild(cB);tr.appendChild(cS);tr.appendChild(cP);tb.appendChild(tr);paintUI(face);
  }
  applyTableSort('uibody');
}
let D={};
function srt(c){const tb=document.getElementById('legbody');const r=[...tb.rows];D[c]=!D[c];
  r.sort((a,b)=>{const x=(c===0?a.querySelector('select').value:a.cells[0].innerText).toLowerCase(),
                       y=(c===0?b.querySelector('select').value:b.cells[0].innerText).toLowerCase();
                 return (x<y?-1:x>y?1:0)*(D[c]?1:-1);});r.forEach(x=>tb.appendChild(x));}
// Generic header-click sort for the package and UI tables. Reads a select
// value, a numeric input, or cell text (numeric when the text leads with a
// number, e.g. contrast or size). The sort is remembered per table and
// re-applied after a rebuild so editing a face does not reset it.
let tableSort={};
function cellVal(td){if(!td)return '';const s=td.querySelector('select');if(s)return s.value.toLowerCase();const i=td.querySelector('input');if(i)return parseFloat(i.value)||0;const t=td.innerText.trim();const n=parseFloat(t);return (!isNaN(n)&&/^[-\\d.]/.test(t))?n:t.toLowerCase();}
function srtTable(tbId,col){tableSort[tbId]={col,asc:!(tableSort[tbId]&&tableSort[tbId].col===col&&tableSort[tbId].asc)};applyTableSort(tbId);}
function applyTableSort(tbId){const s=tableSort[tbId];if(!s)return;const tb=document.getElementById(tbId);if(!tb)return;const dir=s.asc?1:-1;const r=[...tb.rows];r.sort((a,b)=>{const x=cellVal(a.cells[s.col]),y=cellVal(b.cells[s.col]);return ((typeof x==='number'&&typeof y==='number')?x-y:(x<y?-1:x>y?1:0))*dir;});r.forEach(x=>tb.appendChild(x));}
buildLangSel();buildAppSel();renderPalette();buildTable();buildUITable();renderCode();applyGround();updateTitle();initPicker();buildPkgTable();buildPkgPreview();syncMockHeight();syncPkgHeight();
addEventListener('resize',()=>{syncMockHeight();syncPkgHeight();});
// Phase-1 self-test (open with #selftest): seed -> export -> import -> compare.
function pkgSelftest(){
  const seeded=seedPkgmap();
  seeded['org-mode']['org-level-2']={fg:'#e8bd30',bg:null,bold:false,italic:false,inherit:'org-level-1',height:1.2,source:'user'};
  const exp=packagesForExport(seeded);
  const round=seedPkgmap();mergePackagesInto(round,exp);
  const roundtrip=JSON.stringify(exp)===JSON.stringify(packagesForExport(round));
  let oldjson=true;try{const m=seedPkgmap();mergePackagesInto(m,undefined);oldjson=!!(m['org-mode']&&m['org-mode']['org-todo'].source==='default');}catch(e){oldjson=false;}
  const l2=exp['org-mode']['org-level-2'];
  const inherited=l2.inherit==='org-level-1'&&l2.source==='user';
  const height=l2.height===1.2 && !('height' in (exp['org-mode']['org-todo']));
  const sc=seedPkgmap();sc['org-mode']['org-todo']={fg:null,bg:null,bold:false,italic:false,inherit:null,height:1,source:'cleared'};
  const cleared='org-todo' in packagesForExport(sc)['org-mode'];
  const su=seedPkgmap();mergePackagesInto(su,{'zzz-pkg':{'zzz-face':{fg:'#112233',source:'user'}}});
  const unknown=!!(su['zzz-pkg']&&su['zzz-pkg']['zzz-face'].fg==='#112233');
  PKGMAP['__cyc']={a:{fg:null,bg:null,bold:false,italic:false,inherit:'b',height:1,source:'user'},b:{fg:null,bg:null,bold:false,italic:false,inherit:'a',height:1,source:'user'}};
  let cyc=true;try{pkgEffFg('__cyc','a');}catch(e){cyc=false;}delete PKGMAP['__cyc'];
  const verdict=(roundtrip&&oldjson&&inherited&&height&&cleared&&unknown&&cyc)?'PASS':'FAIL';
  document.title='SELFTEST '+verdict;
  const d=document.createElement('div');d.id='selftest';d.textContent='SELFTEST '+verdict+' roundtrip='+roundtrip+' oldjson='+oldjson+' inherit='+inherited+' height='+height+' cleared='+cleared+' unknown='+unknown+' cycle='+cyc;document.body.appendChild(d);
}
if(location.hash==='#selftest')pkgSelftest();
if(location.hash.startsWith('#pick')){openPicker();const m=location.hash.slice(5);if(m){const b=document.querySelector('.pmode button[data-m="'+m+'"]');if(b)b.click();}}
if(location.hash==='#cursortest'){document.getElementById('newhexstr').value='#67809c';openPicker();const sc=document.getElementById('svcur'),hc=document.getElementById('huecur');const L=parseFloat(sc.style.left||'0'),T=parseFloat(sc.style.top||'0'),H=parseFloat(hc.style.top||'0');const ok=L>1&&T>1&&H>1;document.title='CURSORTEST '+(ok?'PASS':'FAIL');const d=document.createElement('div');d.id='cursortest';d.textContent='CURSORTEST '+(ok?'PASS':'FAIL')+' left='+sc.style.left+' top='+sc.style.top+' hue='+hc.style.top;document.body.appendChild(d);}
if(location.hash.startsWith('#app')){const ap=location.hash.slice(4),s=document.getElementById('appsel');if(s&&ap){s.value=ap;pkgChanged();}}
if(location.hash==='#readouttest'){const hex='#67809c';document.getElementById('newhexstr').value=hex;openPicker();pkReadout(hex);
 const o=document.getElementById('pkoklch').textContent,a=document.getElementById('pkapca').textContent,w=document.getElementById('pkcon').textContent;
 const lch=oklab2oklch(srgb2oklab(hex));
 const expO='OKLCH '+lch.L.toFixed(3)+' '+lch.C.toFixed(3)+' '+Math.round(lch.H)+'\\u00b0';
 const expA='APCA Lc '+apca(hex,MAP['bg']).toFixed(0);
 const r=contrast(hex,MAP['bg']),expW=r.toFixed(1)+'  '+rating(r);
 const wired=o===expO&&a===expA&&w===expW;
 const sane=Math.abs(lch.L-0.591)<0.01&&Math.abs(lch.C-0.052)<0.01&&Math.abs(lch.H-251.6)<2;
 const ok=wired&&sane;document.title='READOUTTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='readouttest';d.textContent='READOUTTEST '+(ok?'PASS':'FAIL')+' oklch='+o+' | apca='+a+' | wcag='+w;document.body.appendChild(d);}
</script>"""
HTML=(HTML.replace("COLORMATH_J",COLORMATH_BODY)
 .replace("SAMPLES_J",json.dumps(SAMPLES))
 .replace("PALETTE_J",json.dumps(PALETTE)).replace("CATS_J",json.dumps(CATS))
 .replace("UIFACES_J",json.dumps(UI_FACES)).replace("UIMAP_J",json.dumps(UIMAP)).replace("APPS_J",json.dumps(APPS))
 .replace("BOLD_J",json.dumps(BOLD)).replace("MAP_J",json.dumps(MAP)))
OUT=os.path.join(HERE,'theme-studio.html')
open(OUT,"w").write(HTML)
print("wrote",OUT)

import json, os, re
from default_faces import DefaultFaces
HERE=os.path.dirname(os.path.abspath(__file__))

def strip_exports(src):
    """Drop ES-module `export`/`import` lines so the body loads as a classic <script>.

    A top-level `export` (or `import`) is a syntax error outside a module, so it
    must go before the body is spliced into the page. Imports are stripped too so a
    pure module may import a peer for its own unit tests (e.g. app-util.js imports
    rl from colormath.js) while the inlined copy relies on the peer already being
    in the page. The .mjs inline-integrity tests apply the identical strip and
    assert the page carries the result verbatim, so the two copies cannot drift.
    NOTE: this is line-based — each export/import statement must stay on a single
    line or the continuation lines survive.
    """
    return '\n'.join(l for l in src.splitlines()
                     if not (l.startswith('export') or l.startswith('import'))).rstrip()

# Pure color-math core, inlined verbatim into the page so the browser runs the
# same code the Node tests import (one source of truth).
COLORMATH_BODY=strip_exports(open(os.path.join(HERE,'colormath.js')).read())
# The app's stylesheet and script, kept as real files so they get JS/CSS tooling
# (highlight, brace-check, lint) and so the logic is unit-testable. They are
# inlined into the page the same way colormath.js is: a placeholder in the
# template, filled at generate time. app.js carries the data placeholders
# (MAP_J, PALETTE_J, COLORMATH_J, ...); those are filled after it is spliced in.
STYLES=open(os.path.join(HERE,'styles.css')).read()
APP_BODY=open(os.path.join(HERE,'app.js')).read()
# Pure package-model + dropdown logic, inlined into the page (and unit-tested via
# test-app-core.mjs) the same way colormath.js is.
APP_CORE_BODY=strip_exports(open(os.path.join(HERE,'app-core.js')).read())
# Pure color/UI-boundary helpers (normHex/ratingColor/textOn), unit-tested via
# test-app-util.mjs. Its `import rl` line is stripped on inline (rl is already in
# the page from the colormath core).
APP_UTIL_BODY=strip_exports(open(os.path.join(HERE,'app-util.js')).read())
ns={}
src=open(os.path.join(HERE,'samples.py')).read()
exec(src[:src.index('cols=')], ns)
SAMPLES={"Elisp":ns['ELS'],"Go":ns['GOS'],"Python":ns['PYS'],"TypeScript":ns['TSS'],"Java":ns['JAS'],"C":ns['CS'],"C++":ns['CPS'],"Shell":ns['SHS']}
COLS=ns['COLS']
DEFAULT_FACES_PATH=os.path.join(HERE,'emacs-default-faces.json')
DEFAULTS=DefaultFaces.from_path(DEFAULT_FACES_PATH)
MAP={k:'' for k in COLS}; MAP['bg']='#000000'; MAP['p']='#ffffff'
BOLD={k:False for k in COLS}
ITALIC_MAP={k:False for k in COLS}
def column_id(name):
    return re.sub(r'[+-]\d+$', '', name or 'color')

def normalize_palette(palette):
    return [[p[0], p[1] if len(p) > 1 else 'color', p[2] if len(p) > 2 else column_id(p[1] if len(p) > 1 else 'color')]
            for p in palette]

if DEFAULTS.available:
    MAP['bg']=DEFAULTS.color('default','background') or MAP['bg']
    MAP['p']=DEFAULTS.color('default','foreground') or MAP['p']
    for cat,faces in DEFAULTS.data.get('syntax-map',{}).items():
        faces=faces or []
        if cat in ('bg','p') or not faces: continue
        face=faces[0]
        c=DEFAULTS.color(face,'foreground')
        if c: MAP[cat]=c
        eff=DEFAULTS.face(face,True)
        BOLD[cat]=eff.get('weight')=='bold'
        ITALIC_MAP[cat]=eff.get('slant')=='italic'
else:
    BOLD={k:v[1] for k,v in COLS.items()}
    ITALIC_MAP={k:False for k in COLS}

PALETTE=[[MAP['bg'],"bg","ground"],[MAP['p'],"fg","ground"]]
CATS=[["bg","bg (ground)","Aa Bb 123"],["p","fg","other / whitespace"],["kw","keyword","class  def  if  return"],["bi","builtin","len  echo  printf"],
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
UIMAP={f[0]:{"fg":None,"bg":None,"bold":False,"italic":False,"underline":False,"strike":False} for f in UI_FACES}
if DEFAULTS.available:
    UIMAP={f[0]:dict({"fg":None,"bg":None,"bold":False,"italic":False,"underline":False,"strike":False},**DEFAULTS.seed(f[0],False)) for f in UI_FACES}

# Optional palette seed: THEME_STUDIO_SEED=<file.json> seeds the tool's starting
# palette / assignments / bold / italic / UI from a theme.json (path relative to
# this dir), instead of the hardcoded defaults above. Unset leaves them unchanged.
# Placed after every default it overrides (notably UIMAP) so the merge has targets.
# Mirrors what the in-page Import does, so reseed and import agree.
LOCKS=[]; ITALIC=[k for k,v in ITALIC_MAP.items() if v]
# THEME_STUDIO_SEED=<file>.json opens an existing theme as the starting point.
# Unset starts empty: only bg/fg are in the palette.
_seed=os.environ.get('THEME_STUDIO_SEED')
_d={}
if _seed:
    _d=json.load(open(os.path.join(HERE,_seed)))
    if _d.get('palette'): PALETTE=_d['palette']
    if _d.get('assignments'): MAP.update(_d['assignments'])
    if 'bold' in _d: BOLD={k:(k in _d['bold']) for k in BOLD}
    if 'italic' in _d: ITALIC=_d['italic']
    if _d.get('ui'):
        for _k,_v in _d['ui'].items(): UIMAP[_k]=_v
    if 'locks' in _d: LOCKS=_d['locks']
PALETTE=normalize_palette(PALETTE)
if not DEFAULTS.available:
    # These faces carry a fixed style in Emacs's built-in definitions. Fallback
    # only; normal generation uses emacs-default-faces.json above.
    UIMAP["link"]["underline"]=True
    for _f in ("lazy-highlight","show-paren-match"): UIMAP[_f]["underline"]=True
    for _f in ("error","warning","success"): UIMAP[_f]["bold"]=True
    for _f in ("mode-line","mode-line-inactive"): UIMAP[_f]["box"]={"style":"released","width":1,"color":None}
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
 "magit-sequence-onto magit-sequence-exec magit-left-margin "
 "git-commit-comment-action git-commit-comment-branch-local git-commit-comment-branch-remote "
 "git-commit-comment-detached git-commit-comment-file git-commit-comment-heading git-commit-keyword "
 "git-commit-nonempty-second-line git-commit-overlong-summary git-commit-summary "
 "git-commit-trailer-token git-commit-trailer-value").split()
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
 "org-special-keyword":{"fg":"pewter","bold":True},"org-drawer":{"fg":"pewter"},"org-property-value":{"fg":"steel"},
 "org-checkbox":{"fg":"gold","inherit":"fixed-pitch"},"org-checkbox-statistics-todo":{"fg":"terracotta"},
 "org-checkbox-statistics-done":{"fg":"sage"},"org-warning":{"fg":"terracotta","bold":True},
 "org-link":{"fg":"blue","underline":True},"org-footnote":{"fg":"blue"},"org-date":{"fg":"steel","inherit":"fixed-pitch"},
 "org-sexp-date":{"fg":"steel"},"org-date-selected":{"fg":"ground","bg":"gold"},"org-target":{"fg":"regal"},
 "org-macro":{"fg":"regal"},"org-cite":{"fg":"blue","underline":True},"org-cite-key":{"fg":"blue","underline":True},
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
 "magit-branch-current":{"fg":"blue","bold":True,"box":{"style":"line","width":1,"color":None}},"magit-branch-local":{"fg":"blue"},
 "magit-branch-remote":{"fg":"sage"},"magit-branch-remote-head":{"fg":"sage","bold":True,"box":{"style":"line","width":1,"color":None}},
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
 "mu4e-unread-face mu4e-flagged-face mu4e-replied-face mu4e-forwarded-face mu4e-draft-face mu4e-trashed-face mu4e-related-face "
 "mu4e-contact-face mu4e-special-header-value-face mu4e-url-number-face mu4e-link-face "
 ""
 "mu4e-footer-face mu4e-region-code mu4e-system-face mu4e-highlight-face mu4e-compose-separator-face").split()
MU4E_SEED={
 "mu4e-title-face":{"fg":"blue","bold":True},"mu4e-context-face":{"fg":"blue","bold":True},"mu4e-modeline-face":{"fg":"silver"},"mu4e-ok-face":{"fg":"sage","bold":True},"mu4e-warning-face":{"fg":"gold","bold":True},
 "mu4e-header-title-face":{"fg":"blue","bold":True},"mu4e-header-key-face":{"fg":"blue","bold":True},"mu4e-header-value-face":{"fg":"silver"},"mu4e-header-face":{"fg":"#cdced1"},"mu4e-header-highlight-face":{"bg":"gunmetal","underline":True},"mu4e-header-marks-face":{"fg":"gold"},
 "mu4e-unread-face":{"fg":"white","bold":True},"mu4e-flagged-face":{"fg":"gold"},"mu4e-replied-face":{"fg":"silver"},"mu4e-forwarded-face":{"fg":"silver"},"mu4e-draft-face":{"fg":"steel","italic":True},"mu4e-trashed-face":{"fg":"pewter","strike":True},"mu4e-related-face":{"fg":"steel","italic":True},
 "mu4e-contact-face":{"fg":"#cdced1"},"mu4e-special-header-value-face":{"fg":"silver"},"mu4e-url-number-face":{"fg":"blue","bold":True},"mu4e-link-face":{"fg":"blue","underline":True},
 "mu4e-footer-face":{"fg":"pewter"},"mu4e-region-code":{"bg":"bg-dim"},"mu4e-system-face":{"fg":"pewter","italic":True},"mu4e-highlight-face":{"fg":"gold","bold":True},"mu4e-compose-separator-face":{"fg":"pewter"}}
LSP_FACES=("lsp-signature-face lsp-signature-highlight-function-argument lsp-signature-posframe "
 "lsp-face-highlight-read lsp-face-highlight-write lsp-face-highlight-textual lsp-face-rename lsp-rename-placeholder-face "
 "lsp-inlay-hint-face lsp-inlay-hint-parameter-face lsp-inlay-hint-type-face lsp-details-face "
 "lsp-installation-buffer-face lsp-installation-finished-buffer-face").split()
LSP_SEED={
 "lsp-signature-face":{"fg":"silver"},"lsp-signature-highlight-function-argument":{"fg":"gold","bold":True},"lsp-signature-posframe":{"bg":"bg-dim"},
 "lsp-face-highlight-read":{"bg":"navy","underline":True},"lsp-face-highlight-write":{"bg":"#3d2f4a","bold":True},"lsp-face-highlight-textual":{"bg":"gunmetal"},
 "lsp-face-rename":{"bg":"gunmetal","bold":True,"underline":True},"lsp-rename-placeholder-face":{"fg":"gold","bold":True},
 "lsp-inlay-hint-face":{"fg":"pewter","italic":True},"lsp-inlay-hint-parameter-face":{"fg":"steel","italic":True},"lsp-inlay-hint-type-face":{"fg":"sage","italic":True},
 "lsp-details-face":{"fg":"pewter","italic":True,"height":0.8},"lsp-installation-buffer-face":{"fg":"blue"},"lsp-installation-finished-buffer-face":{"fg":"sage"}}
GITGUTTER_FACES=("git-gutter:added git-gutter:modified git-gutter:deleted git-gutter:unchanged git-gutter:separator").split()
GITGUTTER_SEED={
 "git-gutter:added":{"fg":"emerald","bold":True},"git-gutter:modified":{"fg":"gold","bold":True},"git-gutter:deleted":{"fg":"terracotta","bold":True},
 "git-gutter:unchanged":{"fg":"pewter"},"git-gutter:separator":{"fg":"steel","bold":True}}
FLYCHECK_FACES=("flycheck-error flycheck-warning flycheck-info flycheck-fringe-error flycheck-fringe-warning flycheck-fringe-info "
 "flycheck-delimited-error flycheck-error-delimiter flycheck-error-list-error flycheck-error-list-warning flycheck-error-list-info "
 "flycheck-error-list-error-message flycheck-error-list-checker-name flycheck-error-list-column-number flycheck-error-list-line-number "
 "flycheck-error-list-filename flycheck-error-list-id flycheck-error-list-id-with-explainer flycheck-error-list-highlight flycheck-verify-select-checker").split()
FLYCHECK_SEED={
 "flycheck-error":{"fg":"terracotta","underline":True},"flycheck-warning":{"fg":"gold","underline":True},"flycheck-info":{"fg":"blue","underline":True},
 "flycheck-fringe-error":{"fg":"terracotta"},"flycheck-fringe-warning":{"fg":"gold"},"flycheck-fringe-info":{"fg":"blue"},
 "flycheck-delimited-error":{"fg":"terracotta"},"flycheck-error-delimiter":{"fg":"terracotta"},
 "flycheck-error-list-error":{"fg":"terracotta"},"flycheck-error-list-warning":{"fg":"gold"},"flycheck-error-list-info":{"fg":"blue"},
 "flycheck-error-list-error-message":{"fg":"#cdced1"},"flycheck-error-list-checker-name":{"fg":"steel"},
 "flycheck-error-list-column-number":{"fg":"pewter"},"flycheck-error-list-line-number":{"fg":"pewter"},"flycheck-error-list-filename":{"fg":"blue"},
 "flycheck-error-list-id":{"fg":"steel"},"flycheck-error-list-id-with-explainer":{"fg":"steel","bold":True,"box":{"style":"released","width":1,"color":None}},
 "flycheck-error-list-highlight":{"bg":"gunmetal","bold":True},"flycheck-verify-select-checker":{"fg":"gold","box":{"style":"released","width":1,"color":None}}}
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
 "dirvish-media-info-heading":{"fg":"blue","bold":True},"dirvish-media-info-property-key":{"fg":"steel","italic":True},
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
ORGNOTER_SEED={"org-noter-notes-exist-face":{"fg":"sage","bold":True},"org-noter-no-notes-exist-face":{"fg":"pewter","bold":True}}
SIGNEL_FACES=("signel-timestamp-face signel-my-msg-face signel-other-msg-face signel-error-face").split()
SIGNEL_SEED={"signel-timestamp-face":{"fg":"pewter"},"signel-my-msg-face":{"fg":"blue"},"signel-other-msg-face":{"fg":"silver"},"signel-error-face":{"fg":"terracotta","bold":True}}
PEARL_FACES=("pearl-preamble-summary pearl-editable-comment pearl-readonly-comment pearl-modified-highlight pearl-modified-local pearl-modified-unknown").split()
PEARL_SEED={"pearl-preamble-summary":{"fg":"blue","bold":True},"pearl-editable-comment":{"fg":"silver"},"pearl-readonly-comment":{"fg":"pewter","italic":True},"pearl-modified-highlight":{"bg":"navy"},"pearl-modified-local":{"fg":"gold"},"pearl-modified-unknown":{"fg":"pewter"}}
CALIBREDB_FACES=("calibredb-search-header-library-name-face calibredb-search-header-library-path-face calibredb-search-header-total-face calibredb-search-header-filter-face calibredb-search-header-sort-face calibredb-search-header-highlight-face "
 "calibredb-id-face calibredb-title-face calibredb-author-face calibredb-format-face calibredb-size-face calibredb-tag-face calibredb-date-face calibredb-mark-face calibredb-series-face calibredb-publisher-face calibredb-pubdate-face "
 "calibredb-language-face calibredb-comment-face calibredb-archive-face calibredb-favorite-face calibredb-file-face calibredb-ids-face calibredb-highlight-face calibredb-current-page-button-face calibredb-mouse-face "
 "calibredb-title-detailed-view-face calibredb-edit-annotation-header-title-face").split()
CALIBREDB_SEED={
 "calibredb-search-header-library-name-face":{"fg":"blue","bold":True},"calibredb-search-header-library-path-face":{"fg":"pewter"},"calibredb-search-header-total-face":{"fg":"sage"},"calibredb-search-header-filter-face":{"fg":"gold"},"calibredb-search-header-sort-face":{"fg":"steel"},"calibredb-search-header-highlight-face":{"fg":"gold","bold":True,"underline":True},
 "calibredb-id-face":{"fg":"pewter"},"calibredb-title-face":{"fg":"blue","bold":True},"calibredb-author-face":{"fg":"sage"},"calibredb-format-face":{"fg":"steel"},"calibredb-size-face":{"fg":"pewter"},"calibredb-tag-face":{"fg":"tan"},"calibredb-date-face":{"fg":"pewter"},"calibredb-mark-face":{"fg":"gold","bold":True},"calibredb-series-face":{"fg":"regal"},"calibredb-publisher-face":{"fg":"steel"},"calibredb-pubdate-face":{"fg":"pewter"},
 "calibredb-language-face":{"fg":"steel"},"calibredb-comment-face":{"fg":"silver","italic":True},"calibredb-archive-face":{"fg":"pewter"},"calibredb-favorite-face":{"fg":"gold"},"calibredb-file-face":{"fg":"blue"},"calibredb-ids-face":{"fg":"pewter"},"calibredb-highlight-face":{"fg":"gold","bold":True},"calibredb-current-page-button-face":{"fg":"blue","bold":True,"height":1.1},"calibredb-mouse-face":{"bg":"gunmetal"},
 "calibredb-title-detailed-view-face":{"fg":"gold","bold":True},"calibredb-edit-annotation-header-title-face":{"fg":"blue","bold":True}}
ERC_FACES=("erc-header-line erc-timestamp-face erc-notice-face erc-default-face erc-current-nick-face erc-my-nick-face erc-my-nick-prefix-face erc-nick-default-face erc-nick-prefix-face erc-button-nick-default-face "
 "erc-nick-msg-face erc-direct-msg-face erc-action-face erc-keyword-face erc-pal-face erc-fool-face erc-dangerous-host-face erc-error-face erc-input-face erc-prompt-face erc-command-indicator-face erc-information "
 "erc-button erc-bold-face erc-italic-face erc-underline-face erc-inverse-face erc-spoiler-face erc-fill-wrap-merge-indicator-face erc-keep-place-indicator-arrow erc-keep-place-indicator-line").split()
ERC_SEED={
 "erc-header-line":{"fg":"white","bg":"gunmetal","bold":True},"erc-timestamp-face":{"fg":"pewter"},"erc-notice-face":{"fg":"steel","bold":True},"erc-default-face":{"fg":"#cdced1"},"erc-current-nick-face":{"fg":"gold","bold":True},"erc-my-nick-face":{"fg":"gold","bold":True},"erc-my-nick-prefix-face":{"fg":"gold","bold":True},"erc-nick-default-face":{"fg":"blue","bold":True},"erc-nick-prefix-face":{"fg":"sage","bold":True},"erc-button-nick-default-face":{"fg":"blue","bold":True},
 "erc-nick-msg-face":{"fg":"regal","bold":True},"erc-direct-msg-face":{"fg":"regal"},"erc-action-face":{"fg":"sage","italic":True},"erc-keyword-face":{"fg":"gold","bold":True},"erc-pal-face":{"fg":"emerald","bold":True},"erc-fool-face":{"fg":"pewter"},"erc-dangerous-host-face":{"fg":"terracotta","bold":True},"erc-error-face":{"fg":"terracotta","bold":True},"erc-input-face":{"fg":"silver"},"erc-prompt-face":{"fg":"blue","bold":True},"erc-command-indicator-face":{"fg":"steel","bold":True},"erc-information":{"fg":"steel"},
 "erc-button":{"fg":"blue","bold":True},"erc-bold-face":{"bold":True},"erc-italic-face":{"italic":True},"erc-underline-face":{"fg":"silver","underline":True},"erc-inverse-face":{"fg":"#000000","bg":"silver"},"erc-spoiler-face":{"fg":"#000000","bg":"gunmetal"},"erc-fill-wrap-merge-indicator-face":{"fg":"pewter"},"erc-keep-place-indicator-arrow":{"fg":"gold"},"erc-keep-place-indicator-line":{"bg":"bg-dim"}}
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
 "slack-room-info-title-face":{"fg":"blue","bold":True},"slack-room-info-title-room-name-face":{"fg":"gold","bold":True},"slack-room-info-section-title-face":{"fg":"blue","bold":True},"slack-room-info-section-label-face":{"fg":"steel","bold":True},"slack-room-unread-face":{"fg":"white","bold":True},
 "slack-message-output-header":{"fg":"blue","bold":True,"underline":True},"slack-message-output-text":{"fg":"#cdced1"},"slack-message-output-reaction":{"fg":"steel","box":{"style":"released","width":1,"color":None}},"slack-message-output-reaction-pressed":{"fg":"gold","bold":True,"box":{"style":"released","width":1,"color":None}},"slack-message-deleted-face":{"fg":"pewter","italic":True},"slack-new-message-marker-face":{"fg":"terracotta","bold":True},"slack-all-thread-buffer-thread-header-face":{"fg":"blue","bold":True},
 "slack-message-mention-face":{"fg":"blue"},"slack-message-mention-me-face":{"fg":"gold","bg":"navy","bold":True},"slack-message-mention-keyword-face":{"fg":"gold","bold":True},"slack-channel-button-face":{"fg":"blue","underline":True},
 "slack-mrkdwn-bold-face":{"bold":True},"slack-mrkdwn-italic-face":{"italic":True},"slack-mrkdwn-code-face":{"fg":"terracotta"},"slack-mrkdwn-code-block-face":{"fg":"terracotta","bg":"bg-dim"},"slack-mrkdwn-strike-face":{"fg":"pewter","strike":True},"slack-mrkdwn-blockquote-face":{"fg":"silver","italic":True},"slack-mrkdwn-list-face":{"fg":"silver"},
 "slack-attachment-header":{"fg":"blue","bold":True},"slack-attachment-footer":{"fg":"pewter"},"slack-attachment-pad":{"fg":"pewter"},"slack-attachment-field-title":{"fg":"steel","bold":True},"slack-message-attachment-preview-header-face":{"fg":"blue","bold":True},"slack-preview-face":{"fg":"silver"},"slack-block-highlight-source-overlay-face":{"bg":"bg-dim"},
 "slack-message-action-face":{"fg":"blue","box":{"style":"released","width":1,"color":None}},"slack-message-action-primary-face":{"fg":"sage","box":{"style":"released","width":1,"color":None}},"slack-message-action-danger-face":{"fg":"terracotta","box":{"style":"released","width":1,"color":None}},
 "slack-button-block-element-face":{"fg":"silver","box":{"style":"released","width":1,"color":None}},"slack-button-primary-block-element-face":{"fg":"sage","bold":True,"box":{"style":"released","width":1,"color":None}},"slack-button-danger-block-element-face":{"fg":"terracotta","bold":True,"box":{"style":"released","width":1,"color":None}},"slack-select-block-element-face":{"fg":"blue","box":{"style":"released","width":1,"color":None}},"slack-overflow-block-element-face":{"fg":"steel","box":{"style":"released","width":1,"color":None}},"slack-date-picker-block-element-face":{"fg":"blue","box":{"style":"released","width":1,"color":None}},
 "slack-dialog-title-face":{"fg":"blue","bold":True},"slack-dialog-element-label-face":{"fg":"steel","bold":True},"slack-dialog-element-hint-face":{"fg":"pewter","italic":True},"slack-dialog-element-placeholder-face":{"fg":"pewter"},"slack-dialog-element-error-face":{"fg":"terracotta"},"slack-dialog-submit-button-face":{"fg":"sage","bold":True,"box":{"style":"released","width":1,"color":None}},"slack-dialog-cancel-button-face":{"fg":"silver","box":{"style":"released","width":1,"color":None}},"slack-dialog-select-element-input-face":{"fg":"silver","box":{"style":"released","width":1,"color":None}},
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
 "telega-box-button":{"fg":"blue","box":{"style":"released","width":1,"color":None}},"telega-box-button-active":{"fg":"#000000","bg":"blue","box":{"style":"released","width":1,"color":None}},"telega-box-button-default-active":{"fg":"#000000","bg":"silver","box":{"style":"released","width":1,"color":None}},"telega-box-button-default-passive":{"fg":"steel","box":{"style":"released","width":1,"color":None}},"telega-box-button-primary-active":{"fg":"#000000","bg":"blue","box":{"style":"released","width":1,"color":None}},"telega-box-button-primary-passive":{"fg":"blue","box":{"style":"released","width":1,"color":None}},"telega-box-button-success-active":{"fg":"#000000","bg":"emerald","box":{"style":"released","width":1,"color":None}},"telega-box-button-success-passive":{"fg":"sage","box":{"style":"released","width":1,"color":None}},"telega-box-button-danger-active":{"fg":"#000000","bg":"terracotta","box":{"style":"released","width":1,"color":None}},"telega-box-button-danger-passive":{"fg":"terracotta","box":{"style":"released","width":1,"color":None}},"telega-box-button-ui-active":{"fg":"#000000","bg":"gold","box":{"style":"released","width":1,"color":None}},"telega-box-button-ui-passive":{"fg":"gold","box":{"style":"released","width":1,"color":None}},"telega-box-button2-active":{"fg":"#000000","bg":"blue"},"telega-box-button2-passive":{"fg":"steel"},"telega-box-button2-white-foreground":{"fg":"white"},
 "telega-describe-item-title":{"fg":"steel","bold":True},"telega-describe-section-title":{"fg":"blue","bold":True,"underline":True},"telega-describe-subsection-title":{"fg":"blue"},"telega-enckey-00":{"fg":"pewter"},"telega-enckey-01":{"fg":"sage"},"telega-enckey-10":{"fg":"gold"},"telega-enckey-11":{"fg":"blue"},
 "telega-palette-builtin-blue":{"fg":"blue"},"telega-palette-builtin-green":{"fg":"emerald"},"telega-palette-builtin-orange":{"fg":"terracotta"},"telega-palette-builtin-purple":{"fg":"regal"},
 "telega-webpage-title":{"fg":"blue","bold":True},"telega-webpage-subtitle":{"fg":"steel"},"telega-webpage-header":{"fg":"gold","bold":True},"telega-webpage-subheader":{"fg":"gold"},"telega-webpage-outline":{"fg":"pewter"},"telega-webpage-fixed":{"fg":"terracotta"},"telega-webpage-preformatted":{"fg":"terracotta","bg":"bg-dim"},"telega-webpage-marked":{"fg":"#000000","bg":"gold"},"telega-webpage-strike-through":{"fg":"pewter","strike":True},"telega-webpage-chat-link":{"fg":"blue"},"telega-link-preview-sitename":{"fg":"steel"},"telega-link-preview-title":{"fg":"blue","bold":True}}
# shr is built-in (not in the inventory). It is the HTML renderer behind nov
# (EPUB), eww, elfeed article view, and HTML mail, so theming it themes them all.
SHR_FACES=("shr-h1 shr-h2 shr-h3 shr-h4 shr-h5 shr-h6 shr-text shr-link shr-selected-link "
 "shr-code shr-mark shr-strike-through shr-sup shr-abbreviation shr-sliced-image").split()
SHR_SEED={
 "shr-h1":{"fg":"gold","bold":True,"height":1.4},"shr-h2":{"fg":"blue","bold":True,"height":1.2},"shr-h3":{"fg":"blue","bold":True},"shr-h4":{"fg":"silver","bold":True},"shr-h5":{"fg":"steel","bold":True},"shr-h6":{"fg":"pewter","bold":True},
 "shr-text":{"fg":"#cdced1"},"shr-link":{"fg":"blue","underline":True},"shr-selected-link":{"fg":"gold","bold":True,"underline":True},"shr-code":{"fg":"terracotta","bg":"bg-dim"},"shr-mark":{"fg":"#000000","bg":"gold"},"shr-strike-through":{"fg":"pewter","strike":True},"shr-sup":{"fg":"steel","height":0.8},"shr-abbreviation":{"fg":"steel","italic":True,"underline":True}}
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
if DEFAULTS.available:
    for _app in APPS.values():
        for _row in _app["faces"]:
            _row[2]=DEFAULTS.seed(_row[0],False)
# Apply seed theme package overrides when THEME_STUDIO_SEED is set: each full
# per-face spec (color + structure) replaces the hardcoded face seed before render.
if _seed and _d.get('packages'):
    for _app,_pkfaces in _d['packages'].items():
        if _app in APPS:
            for _row in APPS[_app]['faces']:
                if _row[0] in _pkfaces: _row[2]=_pkfaces[_row[0]]

def add_palette_color(value, label=None):
    if not value: return
    if any((p[0] or '').lower()==str(value).lower() for p in PALETTE): return
    name=label or DEFAULTS.label(value,'color-'+str(len(PALETTE)))
    base=name
    n=2
    used={p[1].lower() for p in PALETTE}
    while name.lower() in used:
        name=base+'-'+str(n); n+=1
    PALETTE.append([value,name,column_id(name)])

if DEFAULTS.available:
    for _k,_v in MAP.items():
        add_palette_color(_v, 'bg' if _k=='bg' else 'fg' if _k=='p' else None)
    for _face,_spec in UIMAP.items():
        add_palette_color(_spec.get('fg'))
        add_palette_color(_spec.get('bg'))
    for _app in APPS.values():
        for _face,_label,_spec in _app['faces']:
            add_palette_color(_spec.get('fg'))
            add_palette_color(_spec.get('bg'))

PALETTE=normalize_palette(PALETTE)
HTML = """<!doctype html><meta charset=utf-8><title>theme-studio</title>
<style>
STYLES_CSS</style>
<div class="topbar">
 <h1 id="pagetitle">Untitled: theme</h1>
 <div class="saveload">
  <div class="filebar end">
   <label style="color:#b4b1a2">theme name</label><input type="text" id="themename" value="" placeholder="untitled" oninput="updateTitle()" style="background:#161412;border:1px solid #252321;color:#cdced1;border-radius:4px;padding:5px 8px;font:10pt monospace;width:200px">
   <button id="savebtn" onclick="saveTheme()" style="display:none">&#128190; save</button>
   <button onclick="exportTheme()">&#11015; export</button>
   <button class="fbtn" onclick="importTheme()">&#11014; import</button><input type="file" id="fileinput" accept=".json" onchange="importFile(event)" style="display:none">
   <button id="jsonbtn" onclick="toggleJSON()">show</button>
  </div>
  <textarea id="export" style="display:none" readonly></textarea>
 </div>
</div>
 <section class="pane grow">
  <h1>palette</h1>
  <div class="palctl">
   <div id="swatch" class="swatch" title="open color picker"></div>
   <input type="text" id="newhexstr" placeholder="#rrggbb" value="#888888" oninput="syncHex()" onkeydown="if(event.key==='Enter')applyEdit()" style="width:110px">
   <input type="text" id="newname" placeholder="name" onkeydown="if(event.key==='Enter')applyEdit()">
   <button onclick="addColor()">+ add color</button>
   <button onclick="updateColor()">&#8635; update selected</button>
   <span id="palmsg"></span>
   <div id="picker" class="picker">
    <div class="prow">
     <div id="sv" class="sv"><canvas id="svmask" class="svmask"></canvas><div id="svsafe" class="svsafe" style="display:none"></div><div id="svcur" class="svcur"></div></div>
     <div id="hue" class="hue"><div id="huecur" class="huecur"></div></div>
    </div>
    <div class="pmodel">edit <button data-pm="hsv" class="on">HSV</button><button data-pm="oklch">OKLCH</button></div>
    <div class="pmodel" title="in OKLCH mode, shade the lightness too light to keep this overlay face readable over its foreground set">safe for <select id="safefor" onchange="setSafeFace(this.value)"><option value="">none</option></select></div>
    <div class="oklchctl" id="oklchctl">
     <div class="ocrow"><label title="perceptual lightness">L</label><input type="range" id="okL" min="0" max="1" step="0.001"><input type="number" id="okLn" min="0" max="1" step="0.001"></div>
     <div class="ocrow"><label title="chroma (colorfulness)">C</label><input type="range" id="okC" min="0" max="0.4" step="0.001"><input type="number" id="okCn" min="0" max="0.4" step="0.001"></div>
     <div class="ocrow"><label title="hue angle, degrees">H</label><input type="range" id="okH" min="0" max="360" step="1"><input type="number" id="okHn" min="0" max="360" step="1"></div>
     <div class="pclamp" id="pkclamp"></div>
    </div>
    <div class="pinfo"><span id="pkhex">#888888</span><span id="pkcon"></span></div>
    <div class="pinfo2"><span id="pkoklch" title="OKLCH perceptual coordinates: lightness L (0..1), chroma C, hue H in degrees"></span><span id="pkapca"></span></div>
    <div class="pmode">limit <button data-m="any" class="on">any</button><button data-m="aa">AA+</button><button data-m="aaa">AAA</button></div>
    <div id="pkchips" class="pkchips"></div>
   </div>
  </div>
  <div class="pals" id="pals"></div>
  <div class="palwarn" id="palwarn"></div>
 </section>
<h1>code/color assignments</h1>
<div class="cols">
 <section class="pane">
  <div class="legctl"><button class="fbtn" onclick="clearUnlocked()" title="reset every unlocked element to default (reads as plain foreground text); locked rows are left untouched">clear unlocked</button></div>
  <table class="leg" id="legtable"><thead><tr><th onclick="srtTable('legbody',0)">elements &#9651;</th><th title="lock a decided element↔color association"></th><th onclick="srtTable('legbody',2)">color &#9651;</th><th>style</th><th title="WCAG contrast of this color on the background">contrast</th><th>example</th></tr></thead><tbody id="legbody"></tbody></table>
 </section>
 <section class="pane grow">
  <div class="langbar"><label style="color:#b4b1a2">language</label><select id="langsel" class="chip" style="width:auto;font:bold 10pt monospace" onchange="renderCode()"></select></div>
  <pre id="codepre"></pre>
 </section>
</div>
<h1>ui faces</h1>
<div class="cols stretch">
 <section class="pane">
  <div class="legctl"><button class="fbtn" onclick="clearUnlockedUI()" title="reset every unlocked UI face to default (no foreground/background); locked rows are left untouched">clear unlocked</button></div>
  <table class="leg" id="uitable"><thead><tr><th onclick="srtTable('uibody',0)">face &#9651;</th><th title="lock a decided face"></th><th onclick="srtTable('uibody',2)">foreground &#9651;</th><th onclick="srtTable('uibody',3)">background &#9651;</th><th>style</th><th onclick="srtTable('uibody',5)" title="WCAG contrast: this face's foreground on its background (or the ground)">contrast &#9651;</th><th>preview</th><th title="face :box (border)">box</th></tr></thead><tbody id="uibody"></tbody></table>
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
 <button class="fbtn" onclick="clearUnlockedPkg()" title="reset every unlocked face in this app to default (no fg/bg); locked rows are left untouched">clear unlocked</button>
</div>
<div class="cols stretch">
 <section class="pane">
  <table class="leg" id="pkgtable"><thead><tr><th onclick="srtTable('pkgbody',0)">face &#9651;</th><th title="lock a decided face"></th><th onclick="srtTable('pkgbody',2)">fg &#9651;</th><th onclick="srtTable('pkgbody',3)">bg &#9651;</th><th>style</th><th onclick="srtTable('pkgbody',5)">contrast &#9651;</th><th onclick="srtTable('pkgbody',6)">inherit &#9651;</th><th onclick="srtTable('pkgbody',7)">size &#9651;</th><th title="face :box (border)">box</th><th></th></tr></thead><tbody id="pkgbody"></tbody></table>
 </section>
 <section class="pane grow" style="display:flex;flex-direction:column">
  <div class="langbar"><label id="pkgprevlabel" style="color:#b4b1a2">preview</label></div>
  <div id="pkgpreview" class="mock" style="overflow:auto"></div>
 </section>
</div>
<script>
APP_JS</script>"""
# Fill the data placeholders. str.replace is literal (no backref interpretation),
# so backslashes in the inlined JS survive intact — the escaping-bug class that
# the triple-quoted string used to cause is gone now that app.js is a real file.
# Caveat: these tokens are replaced everywhere they appear, including inside code
# comments. Don't write a placeholder name (COLORMATH_J, APP_CORE_J, ...) in
# prose in any inlined file, or that prose gets the body spliced into it too.
def fill_data(s):
    return (s.replace("COLORMATH_J",COLORMATH_BODY)
     .replace("APP_CORE_J",APP_CORE_BODY)
     .replace("APP_UTIL_J",APP_UTIL_BODY)
     .replace("SAMPLES_J",json.dumps(SAMPLES))
     .replace("PALETTE_J",json.dumps(PALETTE)).replace("CATS_J",json.dumps(CATS))
     .replace("UIFACES_J",json.dumps(UI_FACES)).replace("UIMAP_J",json.dumps(UIMAP)).replace("APPS_J",json.dumps(APPS))
     .replace("BOLD_J",json.dumps(BOLD)).replace("MAP_J",json.dumps(MAP)).replace("LOCKS_J",json.dumps(LOCKS)).replace("ITALIC_J",json.dumps({k:True for k in ITALIC})))

# Splice the stylesheet and script in first, then fill the data placeholders they
# carry. The page contains app.js exactly as fill_data(APP_BODY) renders it —
# APP_FILLED is that rendering, the handle the inline-integrity test asserts on.
HTML=fill_data(HTML.replace("STYLES_CSS",STYLES).replace("APP_JS",APP_BODY))
APP_FILLED=fill_data(APP_BODY)
OUT=os.path.join(HERE,'theme-studio.html')

if __name__=='__main__':
    open(OUT,"w").write(HTML)
    print("wrote",OUT)

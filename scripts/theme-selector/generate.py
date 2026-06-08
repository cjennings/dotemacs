import json, os
HERE=os.path.dirname(os.path.abspath(__file__))
ns={}
src=open(os.path.join(HERE,'samples.py')).read()
exec(src[:src.index('cols=')], ns)
SAMPLES={"Elisp":ns['ELS'],"Go":ns['GOS'],"Python":ns['PYS'],"TypeScript":ns['TSS'],"Shell":ns['SHS'],"C/C++":ns['CS']}
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
def _faces(names,prefix,seed):
    out=[]
    for f in names:
        lbl=(f[len(prefix):] if f.startswith(prefix) else f).replace("-face","").replace("-"," ")
        out.append([f,lbl,seed.get(f,{})])
    return out
APPS={"org-mode":{"label":"org-mode","preview":"org","faces":_faces(ORG_FACES,"org-",ORG_SEED)},
 "magit":{"label":"magit","preview":"magit","faces":_faces(MAGIT_FACES,"magit-",MAGIT_SEED)},
 "elfeed":{"label":"elfeed","preview":"elfeed","faces":_faces(ELFEED_FACES,"elfeed-",ELFEED_SEED)}}
# Phase 6: merge the generated all-package inventory (refresh with build-inventory.el).
# Bespoke apps stay; every other installed package becomes an editable generic app.
_inv_path=os.path.join(HERE,"package-inventory.json")
if os.path.exists(_inv_path):
    _INV=json.load(open(_inv_path))
    _BESPOKE={"magit","elfeed","org","org-mode"}
    for _pkg in sorted(_INV):
        if _pkg in _BESPOKE or _pkg in APPS: continue
        APPS[_pkg]={"label":_pkg,"preview":"generic","faces":[
            [f,(f[len(_pkg)+1:] if f.startswith(_pkg+"-") else f).replace("-face","").replace("-"," "),{}]
            for f in _INV[_pkg]]}
HTML = """<!doctype html><meta charset=utf-8><title>theme-selector</title>
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
 .pinfo{display:flex;justify-content:space-between;margin:10px 2px 8px;font:12pt monospace;color:#cdced1}
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
 #codepre .flashtok,.ex.flashtok{animation:flashtok 1.1s ease-out;border-radius:2px}
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
   <label class="fbtn">&#11014; import<input type="file" accept=".json" onchange="importFile(event)" style="display:none"></label>
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
  <table class="leg" id="uitable"><thead><tr><th>face</th><th>foreground</th><th>background</th><th>preview</th></tr></thead><tbody id="uibody"></tbody></table>
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
  <table class="leg" id="pkgtable"><thead><tr><th>face</th><th>fg</th><th>bg</th><th>weight</th><th>inherit</th><th>size</th><th>contrast</th><th></th></tr></thead><tbody id="pkgbody"></tbody></table>
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
function seedPkgmap(){const m={};for(const app in APPS){m[app]={};for(const row of APPS[app].faces){const face=row[0],d=row[2]||{};m[app][face]={fg:pname(d.fg),bg:pname(d.bg),bold:!!d.bold,italic:!!d.italic,inherit:d.inherit||null,height:d.height||1,source:'default'};}}return m;}
function packagesForExport(map){const out={};for(const app in map){const faces={};for(const face in map[app]){const f=map[app][face];if(f.source==='default'||f.source==='user'||f.source==='cleared'){const o={fg:f.fg,bg:f.bg,bold:f.bold,italic:f.italic,inherit:f.inherit,source:f.source};if(f.height&&f.height!==1)o.height=f.height;faces[face]=o;}}if(Object.keys(faces).length)out[app]=faces;}return out;}
function mergePackagesInto(map,pkgs){if(!pkgs)return;for(const app in pkgs){if(!map[app])map[app]={};for(const face in pkgs[app]){const f=pkgs[app][face]||{};map[app][face]={fg:f.fg??null,bg:f.bg??null,bold:!!f.bold,italic:!!f.italic,inherit:f.inherit??null,height:f.height||1,source:f.source||'user'};}}}
let PKGMAP=seedPkgmap();
function esc(t){return t.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');}
function lin(c){c/=255;return c<=0.03928?c/12.92:Math.pow((c+0.055)/1.055,2.4);}
function rl(h){return 0.2126*lin(parseInt(h.substr(1,2),16))+0.7152*lin(parseInt(h.substr(3,2),16))+0.0722*lin(parseInt(h.substr(5,2),16));}
function textOn(h){const L=rl(h);return ((L+0.05)/0.05)>(1.05/(L+0.05))?'#000':'#fff';}
function contrast(a,b){const L1=rl(a),L2=rl(b),hi=Math.max(L1,L2),lo=Math.min(L1,L2);return (hi+0.05)/(lo+0.05);}
function rating(r){return r>=7?'AAA':r>=4.5?'AA':'FAIL';}
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
function hsv2rgb(h,s,v){h=(h%360+360)%360/360;const i=Math.floor(h*6),f=h*6-i,p=v*(1-s),q=v*(1-f*s),t=v*(1-(1-f)*s);let r,g,b;switch(((i%6)+6)%6){case 0:[r,g,b]=[v,t,p];break;case 1:[r,g,b]=[q,v,p];break;case 2:[r,g,b]=[p,v,t];break;case 3:[r,g,b]=[p,q,v];break;case 4:[r,g,b]=[t,p,v];break;default:[r,g,b]=[v,p,q];}return[Math.round(r*255),Math.round(g*255),Math.round(b*255)];}
function rgb2hsv(r,g,b){r/=255;g/=255;b/=255;const mx=Math.max(r,g,b),mn=Math.min(r,g,b),d=mx-mn;let h=0;if(d){if(mx===r)h=((g-b)/d+6)%6;else if(mx===g)h=(b-r)/d+2;else h=(r-g)/d+4;h*=60;}return[h,mx?d/mx:0,mx];}
function hex2rgb(h){return[parseInt(h.substr(1,2),16),parseInt(h.substr(3,2),16),parseInt(h.substr(5,2),16)];}
function rgb2hex(r,g,b){return '#'+[r,g,b].map(x=>Math.max(0,Math.min(255,x)).toString(16).padStart(2,'0')).join('');}
let pkH=0,pkS=0,pkV=0.5,pickerOn=false;
let pkMode='any';
function pkThresh(){return pkMode==='aa'?4.5:pkMode==='aaa'?7:0;}
function drawMask(){const cv=document.getElementById('svmask');if(!cv)return;const sv=document.getElementById('sv'),w=cv.width=sv.clientWidth,h=cv.height=sv.clientHeight,ctx=cv.getContext('2d');ctx.clearRect(0,0,w,h);const T=pkThresh();if(!T)return;ctx.fillStyle='rgba(8,7,6,0.66)';const step=4;for(let x=0;x<w;x+=step){const S=x/w;for(let y=0;y<h;y+=step){const V=1-y/h,[r,g,b]=hsv2rgb(pkH,S,V);if(contrast(rgb2hex(r,g,b),MAP['bg'])<T)ctx.fillRect(x,y,step,step);}}}
function paintPicker(){const sv=document.getElementById('sv');if(!sv)return;sv.style.background=`linear-gradient(to top,#000,rgba(0,0,0,0)),linear-gradient(to right,#fff,rgba(255,255,255,0)),hsl(${pkH},100%,50%)`;const w=sv.clientWidth,h=sv.clientHeight,hh=document.getElementById('hue').clientHeight;document.getElementById('svcur').style.left=(pkS*w)+'px';document.getElementById('svcur').style.top=((1-pkV)*h)+'px';document.getElementById('huecur').style.top=((pkH/360)*hh)+'px';drawMask();}
function pkReadout(h){const e=document.getElementById('pkhex');if(e)e.textContent=h;const c=document.getElementById('pkcon');if(c){const r=contrast(h,MAP['bg']);c.textContent=r.toFixed(1)+'  '+rating(r);c.style.color=ratingColor(r);}}
function syncHex(){const v=normHex(document.getElementById('newhexstr').value);if(!v)return;document.getElementById('swatch').style.background=v;[pkH,pkS,pkV]=rgb2hsv(...hex2rgb(v));if(pickerOn)paintPicker();pkReadout(v);}
function setHex(h){h=normHex(h)||h;document.getElementById('newhexstr').value=h;document.getElementById('swatch').style.background=h;[pkH,pkS,pkV]=rgb2hsv(...hex2rgb(h));if(pickerOn)paintPicker();pkReadout(h);}
function pkSet(){const hex=rgb2hex(...hsv2rgb(pkH,pkS,pkV));document.getElementById('newhexstr').value=hex;document.getElementById('swatch').style.background=hex;paintPicker();pkReadout(hex);}
function buildPkChips(){const c=document.getElementById('pkchips');if(!c)return;c.innerHTML='';const T=pkThresh();PALETTE.forEach(([hex,name])=>{const s=document.createElement('div');s.className='pc';s.style.background=hex;s.title=name+' '+hex;const ok=!T||contrast(hex,MAP['bg'])>=T;if(!ok){s.style.opacity='0.22';s.title+=' (below '+pkMode.toUpperCase()+')';}s.onclick=()=>{if(ok)setHex(hex);};c.appendChild(s);});}
function openPicker(){pickerOn=true;[pkH,pkS,pkV]=rgb2hsv(...hex2rgb(curHex()));buildPkChips();paintPicker();pkReadout(curHex());document.getElementById('picker').style.display='block';setTimeout(()=>document.addEventListener('pointerdown',pkOutside),0);}
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
function updateTitle(){const n=document.getElementById('themename').value.trim();document.getElementById('pagetitle').textContent=(n||'Untitled')+': theme';const sb=document.getElementById('savebtn');if(sb)sb.style.display=n?'':'none';fileHandle=null;}
let fileHandle=null;
function exportTheme(){const blob=new Blob([JSON.stringify(exportObj(),null,1)],{type:'application/json'});const a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download=fileSlug()+'.json';a.click();}
async function saveTheme(){const data=JSON.stringify(exportObj(),null,1);
  if(!window.showSaveFilePicker){exportTheme();notify('saved via download (browser has no Save-File support)',false);return;}
  try{if(!fileHandle)fileHandle=await window.showSaveFilePicker({suggestedName:fileSlug()+'.json',types:[{description:'theme JSON',accept:{'application/json':['.json']}}]});
    const w=await fileHandle.createWritable();await w.write(data);await w.close();notify('saved "'+themeName()+'"',false);
  }catch(e){if(e&&e.name!=='AbortError')notify('save failed: '+e.message,true);}}
function importFile(ev){const f=ev.target.files[0];if(!f)return;const r=new FileReader();
  r.onload=()=>{try{const d=JSON.parse(r.result);if(d.name)document.getElementById('themename').value=d.name;if(d.palette)PALETTE=d.palette;if(d.assignments)Object.assign(MAP,d.assignments);
    BOLD={};(d.bold||[]).forEach(k=>BOLD[k]=true);ITALIC={};(d.italic||[]).forEach(k=>ITALIC[k]=true);
    if(d.ui)Object.assign(UIMAP,d.ui);
    PKGMAP=seedPkgmap();if(d.packages)mergePackagesInto(PKGMAP,d.packages);
    renderPalette();buildTable();buildUITable();buildPkgTable();buildPkgPreview();renderCode();applyGround();updateTitle();}catch(e){alert('bad theme file: '+e.message);}};
  r.readAsText(f);ev.target.value='';}
function applyGround(){document.querySelectorAll('pre').forEach(p=>p.style.background=MAP['bg']);document.querySelectorAll('.ex').forEach(e=>e.style.background=MAP['bg']);}
function uf(f){return UIMAP[f]||{};}
function flashRow(tr){if(!tr)return;tr.scrollIntoView({block:'center',behavior:'smooth'});tr.classList.remove('flash');void tr.offsetWidth;tr.classList.add('flash');}
function flashEl(el){if(!el)return;el.classList.remove('flashtok');void el.offsetWidth;el.classList.add('flashtok');}
function flashTokens(kind){const sp=document.querySelectorAll('#codepre [data-k="'+kind+'"]');if(sp.length){sp.forEach(flashEl);return;}const row=document.querySelector('#legbody tr[data-kind="'+kind+'"]');if(row)flashEl(row.querySelector('.ex'));}
function flashAssign(k){flashRow(document.querySelector(`#legbody tr[data-kind="${k}"]`));}
function flashUi(f){flashRow(document.querySelector(`#uibody tr[data-face="${f}"]`));}
function flashUiPreview(f){const sp=document.querySelectorAll(`#mockframe [data-face="${f}"]`);if(sp.length){sp.forEach(flashEl);return;}const cell=document.getElementById('uiprev-'+f);if(cell)flashEl(cell);}
function flashPkg(f){flashRow(document.querySelector(`#pkgbody tr[data-face="${f}"]`));}
function flashPkgPreview(f){const sp=document.querySelectorAll(`#pkgpreview [data-face="${f}"]`);if(sp.length){sp.forEach(flashEl);return;}const row=document.querySelector(`#pkgbody tr[data-face="${f}"]`);if(row)flashEl(row.querySelector('.cat'));}
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
  html+=`<div class="bar" data-face="mode-line" style="background:${ml.bg||fg};color:${ml.fg||bg}">  init.el      (Emacs Lisp)      L5      git:main  </div>`;
  html+=`<div class="bar" data-face="mode-line-inactive" style="background:${mli.bg||bg};color:${mli.fg||fg}">  *Messages*      (Fundamental)  </div>`;
  html+=`<div class="echo" style="color:${fg}"><span data-face="minibuffer-prompt" style="color:${mb.fg||fg}">I-search:</span> count   <span data-face="isearch-fail" style="color:${isf.fg||fg};background:${isf.bg||'transparent'}">zzz [no match]</span></div>`;
  html+=`<div class="echo"><span data-face="link" style="color:${lnk.fg||fg};text-decoration:underline">https://gnu.org</span>   <span data-face="error" style="color:${err.fg||fg}">error</span>   <span data-face="warning" style="color:${wrn.fg||fg}">warning</span>   <span data-face="success" style="color:${suc.fg||fg}">ok</span></div>`;
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
function seedFace(d){return {fg:pname(d.fg),bg:pname(d.bg),bold:!!d.bold,italic:!!d.italic,inherit:d.inherit||null,height:d.height||1,source:'default'};}
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
    const cw=document.createElement('td');[['B','bold'],['I','italic']].forEach(([ch,at])=>{const b=document.createElement('button');b.className='sbtn'+(f[at]?' on':'');b.textContent='a';b.style.fontWeight=at==='bold'?'bold':'normal';b.style.fontStyle=at==='italic'?'italic':'normal';b.onclick=()=>{f[at]=!f[at];f.source='user';pkgChanged();};cw.appendChild(b);});
    const ci=document.createElement('td');const isel=document.createElement('select');isel.className='chip';isel.style.cssText='width:150px;font:10pt monospace';inh.forEach(o=>{const op=document.createElement('option');op.value=o;op.textContent=o||'— none —';isel.appendChild(op);});isel.value=f.inherit||'';isel.onchange=()=>{f.inherit=isel.value||null;f.source='user';pkgChanged();};ci.appendChild(isel);
    const ch=document.createElement('td');const hin=document.createElement('input');hin.type='number';hin.min='0.8';hin.max='2.5';hin.step='0.05';hin.value=f.height||1;hin.className='hstep';hin.onchange=()=>{f.height=parseFloat(hin.value)||1;f.source='user';pkgChanged();};ch.appendChild(hin);
    const cc=document.createElement('td');cc.style.fontSize='10pt';cc.style.whiteSpace='nowrap';const efg=pkgEffFg(app,face)||MAP['p'],ebg=pkgEffBg(app,face)||MAP['bg'],r=contrast(efg,ebg);cc.innerHTML=`<span style="color:${ratingColor(r)}">${r.toFixed(1)}  ${rating(r)}</span>`;
    const cr=document.createElement('td');const rb=document.createElement('button');rb.className='sbtn';rb.textContent='↺';rb.title='reset to default';rb.onclick=()=>{PKGMAP[app][face]=seedFace(def);pkgChanged();};cr.appendChild(rb);
    tr.append(c0,cf,cb,cw,ci,ch,cc,cr);tb.appendChild(tr);
  }
}
function ofs(app,face){const f=PKGMAP[app][face]||{},fg=pkgEffFg(app,face)||MAP['p'],bg=pkgEffBg(app,face);return `color:${fg};${bg?'background:'+bg+';':''}font-weight:${f.bold?'bold':'normal'};font-style:${f.italic?'italic':'normal'};font-size:${(f.height||1)}em`;}
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
function genericPreview(app){let h='<div style="padding:10px 14px;font:12pt/1.8 monospace">';for(const [face,label,def] of APPS[app].faces){const f=PKGMAP[app][face],efg=pkgEffFg(app,face)||MAP['p'],ebg=pkgEffBg(app,face);h+=`<div data-face="${face}" style="color:${efg};${ebg?'background:'+ebg+';':''}font-weight:${f.bold?'bold':'normal'};font-style:${f.italic?'italic':'normal'};font-size:${(f.height||1)}em">${esc(label)}</div>`;}return h+'</div>';}
function buildPkgPreview(){const app=curApp(),p=document.getElementById('pkgpreview');if(!p)return;const pv=APPS[app].preview;const bespoke=pv==='org'||pv==='magit'||pv==='elfeed';p.innerHTML=pv==='org'?renderOrgPreview():pv==='magit'?renderMagitPreview():pv==='elfeed'?renderElfeedPreview():genericPreview(app);p.style.background=MAP['bg'];p.onclick=(e)=>{const u=e.target.closest('[data-face]');if(u)flashPkg(u.dataset.face);};const lbl=document.getElementById('pkgprevlabel');if(lbl)lbl.textContent=bespoke?(APPS[app].label+' preview'):'preview (generic — face names in their own colors)';}
function resetApp(){const app=curApp();PKGMAP[app]={};for(const [face,label,d] of APPS[app].faces)PKGMAP[app][face]=seedFace(d);pkgChanged();}
function syncPkgHeight(){const t=document.getElementById('pkgtable'),m=document.getElementById('pkgpreview');if(!t||!m)return;const lb=m.previousElementSibling,lbh=lb?lb.getBoundingClientRect().height+10:30;m.style.height=Math.max(t.getBoundingClientRect().height-lbh,220)+'px';}
function paintUI(face){const pv=document.getElementById('uiprev-'+face);if(!pv)return;pv.style.color=UIMAP[face].fg||MAP['p'];pv.style.background=UIMAP[face].bg||MAP['bg'];}
function buildUITable(){
  const tb=document.getElementById('uibody');tb.innerHTML='';
  for(const [face,label,ex] of UI_FACES){
    const tr=document.createElement('tr');tr.dataset.face=face;
    const c0=document.createElement('td');c0.className='cat';c0.textContent=label;c0.style.cursor='pointer';c0.title='flash this face in the live preview';c0.onclick=()=>flashUiPreview(face);
    const cF=document.createElement('td');cF.appendChild(uiSelect(face,'fg'));
    const cB=document.createElement('td');cB.appendChild(uiSelect(face,'bg'));
    const cP=document.createElement('td');cP.className='ex';cP.id='uiprev-'+face;cP.textContent=ex;cP.style.padding='4px 10px';cP.style.borderRadius='4px';
    tr.appendChild(c0);tr.appendChild(cF);tr.appendChild(cB);tr.appendChild(cP);tb.appendChild(tr);paintUI(face);
  }
}
let D={};
function srt(c){const tb=document.getElementById('legbody');const r=[...tb.rows];D[c]=!D[c];
  r.sort((a,b)=>{const x=(c===0?a.querySelector('select').value:a.cells[0].innerText).toLowerCase(),
                       y=(c===0?b.querySelector('select').value:b.cells[0].innerText).toLowerCase();
                 return (x<y?-1:x>y?1:0)*(D[c]?1:-1);});r.forEach(x=>tb.appendChild(x));}
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
if(location.hash.startsWith('#app')){const ap=location.hash.slice(4),s=document.getElementById('appsel');if(s&&ap){s.value=ap;pkgChanged();}}
</script>"""
HTML=(HTML.replace("SAMPLES_J",json.dumps(SAMPLES))
 .replace("PALETTE_J",json.dumps(PALETTE)).replace("CATS_J",json.dumps(CATS))
 .replace("UIFACES_J",json.dumps(UI_FACES)).replace("UIMAP_J",json.dumps(UIMAP)).replace("APPS_J",json.dumps(APPS))
 .replace("BOLD_J",json.dumps(BOLD)).replace("MAP_J",json.dumps(MAP)))
OUT=os.path.join(HERE,'theme-selector.html')
open(OUT,"w").write(HTML)
print("wrote",OUT)

import json, os, re
from app_inventory import add_inventory_apps, apply_default_face_seeds, apply_package_overrides, face_rows
from default_faces import DefaultFaces
from face_data import *
from face_specs import ui_face_spec
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
# Palette panel actions and rendering. This is stateful browser code, split from
# app.js because color-column behavior changes often and benefits from locality.
PALETTE_ACTIONS_BODY=strip_exports(open(os.path.join(HERE,'palette-actions.js')).read())
# Browser hash gates, split from app.js so the application code is not buried
# under the test harness while still shipping one self-contained HTML file.
BROWSER_GATES_BODY=strip_exports(open(os.path.join(HERE,'browser-gates.js')).read())
ns={}
src=open(os.path.join(HERE,'samples.py')).read()
exec(src[:src.index('cols=')], ns)
SAMPLES={"Elisp":ns['ELS'],"Go":ns['GOS'],"Python":ns['PYS'],"TypeScript":ns['TSS'],"Java":ns['JAS'],"C":ns['CS'],"C++":ns['CPS'],"Rust":ns['RUSTS'],"Zig":ns['ZIGS'],"Shell":ns['SHS']}
COLS=ns['COLS']
DEFAULT_FACES_PATH=os.path.join(HERE,'emacs-default-faces.json')
DEFAULTS=DefaultFaces.from_path(DEFAULT_FACES_PATH)
MAP={k:'' for k in COLS}; MAP['bg']='#000000'; MAP['p']='#ffffff'
BOLD={k:False for k in COLS}
ITALIC_MAP={k:False for k in COLS}
def column_id(name):
    name = name or 'color'
    if re.fullmatch(r'color-\d+', name):
        return name
    name = re.sub(r'[+-]\d+$', '', name)
    return re.sub(r'\d+$', '', name) or 'color'

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
UIMAP={f[0]:ui_face_spec() for f in UI_FACES}
if DEFAULTS.available:
    UIMAP={f[0]:ui_face_spec(DEFAULTS.seed(f[0],False)) for f in UI_FACES}

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
# Bespoke package face lists and seed defaults live in face_data.py.
APPS={"org-mode":{"label":"org-mode","preview":"org","faces":face_rows(ORG_FACES,"org-",ORG_SEED)},
 "magit":{"label":"magit","preview":"magit","faces":face_rows(MAGIT_FACES,"magit-",MAGIT_SEED)},
 "elfeed":{"label":"elfeed","preview":"elfeed","faces":face_rows(ELFEED_FACES,"elfeed-",ELFEED_SEED)},
 "mu4e":{"label":"mu4e","preview":"mu4e","faces":face_rows(MU4E_FACES,"mu4e-",MU4E_SEED)},
 "ghostel":{"label":"ghostel","preview":"ghostel","faces":face_rows(GHOSTEL_FACES,"ghostel-",GHOSTEL_SEED)},
 "dashboard":{"label":"dashboard","preview":"dashboard","faces":face_rows(DASHBOARD_FACES,"dashboard-",DASHBOARD_SEED)},
 "lsp-mode":{"label":"lsp-mode","preview":"lsp","faces":face_rows(LSP_FACES,"lsp-",LSP_SEED)},
 "git-gutter":{"label":"git-gutter","preview":"gitgutter","faces":face_rows(GITGUTTER_FACES,"git-gutter:",GITGUTTER_SEED)},
 "flycheck":{"label":"flycheck","preview":"flycheck","faces":face_rows(FLYCHECK_FACES,"flycheck-",FLYCHECK_SEED)},
 "dired":{"label":"dired","preview":"dired","faces":face_rows(DIRED_FACES,"dired-",DIRED_SEED)},
 "dirvish":{"label":"dirvish","preview":"dirvish","faces":face_rows(DIRVISH_FACES,"dirvish-",DIRVISH_SEED)},
 "calibredb":{"label":"calibredb","preview":"calibredb","faces":face_rows(CALIBREDB_FACES,"calibredb-",CALIBREDB_SEED)},
 "erc":{"label":"erc","preview":"erc","faces":face_rows(ERC_FACES,"erc-",ERC_SEED)},
 "org-drill":{"label":"org-drill","preview":"orgdrill","faces":face_rows(ORGDRILL_FACES,"org-drill-",ORGDRILL_SEED)},
 "org-noter":{"label":"org-noter","preview":"orgnoter","faces":face_rows(ORGNOTER_FACES,"org-noter-",ORGNOTER_SEED)},
 "signel":{"label":"signel","preview":"signel","faces":face_rows(SIGNEL_FACES,"signel-",SIGNEL_SEED)},
 "pearl":{"label":"pearl","preview":"pearl","faces":face_rows(PEARL_FACES,"pearl-",PEARL_SEED)},
 "slack":{"label":"slack","preview":"slack","faces":face_rows(SLACK_FACES,"slack-",SLACK_SEED)},
 "telega":{"label":"telega","preview":"telega","faces":face_rows(TELEGA_FACES,"telega-",TELEGA_SEED)},
 "shr":{"label":"shr (HTML: nov/eww/mail)","preview":"shr","faces":face_rows(SHR_FACES,"shr-",SHR_SEED)}}
# Phase 6: merge the generated all-package inventory (refresh with build-inventory.el).
# Bespoke apps stay; every other installed package becomes an editable generic app.
_inv_path=os.path.join(HERE,"package-inventory.json")
add_inventory_apps(APPS, _inv_path)
apply_default_face_seeds(APPS, DEFAULTS)
# Apply seed theme package overrides when THEME_STUDIO_SEED is set: each full
# per-face spec (color + structure) replaces the hardcoded face seed before render.
if _seed:
    apply_package_overrides(APPS, _d.get('packages'))

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
HTML=open(os.path.join(HERE,'theme-studio.template.html')).read()
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
     .replace("PALETTE_ACTIONS_J",PALETTE_ACTIONS_BODY)
     .replace("BROWSER_GATES_J",BROWSER_GATES_BODY)
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

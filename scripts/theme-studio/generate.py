import json, os, re
from app_inventory import add_inventory_apps, apply_default_face_seeds, apply_package_overrides, face_rows
from default_faces import DefaultFaces
from face_data import *
from face_specs import face_spec, ui_face_spec
HERE=os.path.dirname(os.path.abspath(__file__))

def read_text(name):
    with open(os.path.join(HERE,name)) as src:
        return src.read()

def read_json(name):
    return json.loads(read_text(name))

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
COLORMATH_BODY=strip_exports(read_text('colormath.js'))
# The app's stylesheet and script, kept as real files so they get JS/CSS tooling
# (highlight, brace-check, lint) and so the logic is unit-testable. They are
# inlined into the page the same way colormath.js is: a placeholder in the
# template, filled at generate time. app.js carries the data placeholders
# (MAP_J, PALETTE_J, COLORMATH_J, ...); those are filled after it is spliced in.
STYLES=read_text('styles.css')
APP_BODY=read_text('app.js')
# Pure package-model + dropdown logic, inlined into the page (and unit-tested via
# test-app-core.mjs) the same way colormath.js is.
APP_CORE_BODY=strip_exports(read_text('app-core.js'))
# Pure color/UI-boundary helpers (normHex/ratingColor/textOn), unit-tested via
# test-app-util.mjs. Its `import rl` line is stripped on inline (rl is already in
# the page from the colormath core).
APP_UTIL_BODY=strip_exports(read_text('app-util.js'))
# Pure palette-generator planner and its browser UI panel, split from the shared
# app core so generation behavior and panel wiring can evolve locally.
PALETTE_GENERATOR_CORE_BODY=strip_exports(read_text('palette-generator-core.js'))
PALETTE_GENERATOR_UI_BODY=strip_exports(read_text('palette-generator-ui.js'))
# Palette panel actions and rendering. This is stateful browser code, split from
# app.js because color-column behavior changes often and benefits from locality.
PALETTE_ACTIONS_BODY=strip_exports(read_text('palette-actions.js'))
# Browser hash gates, split from app.js so the application code is not buried
# under the test harness while still shipping one self-contained HTML file.
BROWSER_GATES_BODY=strip_exports(read_text('browser-gates.js'))
COLOR_NAMES=read_json('color-names.json')
ns={}
src=read_text('samples.py')
exec(src[:src.index('# THEME_STUDIO_DATA_END')], ns)
SAMPLES={"Elisp":ns['ELS'],"Go":ns['GOS'],"Python":ns['PYS'],"TypeScript":ns['TSS'],"Java":ns['JAS'],"C":ns['CS'],"C++":ns['CPS'],"Rust":ns['RUSTS'],"Zig":ns['ZIGS'],"Shell":ns['SHS']}
COLS=ns['COLS']
DEFAULT_FACES_PATH=os.path.join(HERE,'emacs-default-faces.json')
DEFAULTS=DefaultFaces.from_path(DEFAULT_FACES_PATH)
def column_id(name):
    name = name or 'color'
    if re.fullmatch(r'color-\d+', name):
        return name
    name = re.sub(r'[+-]\d+$', '', name)
    return re.sub(r'\d+$', '', name) or 'color'

def normalize_palette(palette):
    return [[p[0], p[1] if len(p) > 1 else 'color', p[2] if len(p) > 2 else column_id(p[1] if len(p) > 1 else 'color')]
            for p in palette]

def initial_maps(cols,defaults):
    map_={k:'' for k in cols}
    map_['bg']='#000000'
    map_['p']='#ffffff'
    bold={k:False for k in cols}
    italic={k:False for k in cols}
    if defaults.available:
        map_['bg']=defaults.color('default','background') or map_['bg']
        map_['p']=defaults.color('default','foreground') or map_['p']
        for cat,faces in defaults.data.get('syntax-map',{}).items():
            faces=faces or []
            if cat in ('bg','p') or not faces:
                continue
            face=faces[0]
            color=defaults.color(face,'foreground')
            if color:
                map_[cat]=color
            eff=defaults.face(face,True)
            bold[cat]=eff.get('weight')=='bold'
            italic[cat]=eff.get('slant')=='italic'
    else:
        bold={k:v[1] for k,v in cols.items()}
        italic={k:False for k in cols}
    return map_,bold,italic

def apply_builtin_fallback_styles(uimap):
    """Fill the small set of style defaults used when no Emacs snapshot exists."""
    uimap["link"]["underline"]=True
    for face in ("lazy-highlight","show-paren-match"):
        uimap[face]["underline"]=True
    for face in ("error","warning","success"):
        uimap[face]["bold"]=True
    for face in ("mode-line","mode-line-inactive"):
        uimap[face]["box"]={"style":"released","width":1,"color":None}

def build_uimap(ui_faces,defaults):
    if defaults.available:
        return {face[0]:ui_face_spec(defaults.seed(face[0],False)) for face in ui_faces}
    uimap={face[0]:ui_face_spec() for face in ui_faces}
    apply_builtin_fallback_styles(uimap)
    return uimap

def build_syntax(cols,map_,bold,italic,defaults):
    syntax={k:face_spec({"fg": map_.get(k) or None, "bold": bool(bold.get(k)), "italic": bool(italic.get(k))}) for k in cols}
    if defaults.available:
        for cat,faces in defaults.data.get('syntax-map',{}).items():
            if cat in syntax and faces:
                syntax[cat]=face_spec(defaults.seed(faces[0], False))
        syntax['bg']=face_spec({"fg": map_['bg']})
        syntax['p']=face_spec({"fg": map_['p']})
    return syntax

def load_seed_data(seed):
    return read_json(seed) if seed else {}

def apply_seed_basics(data,palette,uimap,locks):
    if data.get('palette'):
        palette=data['palette']
    if data.get('ui'):
        for key,value in data['ui'].items():
            uimap[key]=value
    if 'locks' in data:
        locks=data['locks']
    return palette,uimap,locks

def apply_syntax_seed(data,syntax,map_):
    if not data.get('syntax'):
        return
    for key,value in data['syntax'].items():
        if key in syntax:
            syntax[key]=face_spec(value)
            map_[key]=syntax[key].get('fg') or ''

def add_palette_color(palette,defaults,value,label=None):
    if not value:
        return
    if any((p[0] or '').lower()==str(value).lower() for p in palette):
        return
    name=label or defaults.label(value,'color-'+str(len(palette)))
    base=name
    n=2
    used={(p[1] if len(p) > 1 else '').lower() for p in palette}
    while name.lower() in used:
        name=base+'-'+str(n); n+=1
    palette.append([value,name,column_id(name)])

def add_default_palette_colors(palette,map_,syntax,uimap,apps,defaults):
    for key,value in map_.items():
        add_palette_color(palette,defaults,value,'bg' if key=='bg' else 'fg' if key=='p' else None)
    for spec in syntax.values():
        add_palette_color(palette,defaults,spec.get('fg'))
        add_palette_color(palette,defaults,spec.get('bg'))
        if spec.get('box'):
            add_palette_color(palette,defaults,spec['box'].get('color'))
    for _face,spec in uimap.items():
        add_palette_color(palette,defaults,spec.get('fg'))
        add_palette_color(palette,defaults,spec.get('bg'))
        if spec.get('box'):
            add_palette_color(palette,defaults,spec['box'].get('color'))
    for app in apps.values():
        for _face,_label,spec in app['faces']:
            add_palette_color(palette,defaults,spec.get('fg'))
            add_palette_color(palette,defaults,spec.get('bg'))
            if spec.get('box'):
                add_palette_color(palette,defaults,spec['box'].get('color'))

def apply_seed_packages(apps,data,seed):
    if seed:
        apply_package_overrides(apps,data.get('packages'))

MAP,BOLD,ITALIC_MAP=initial_maps(COLS,DEFAULTS)

PALETTE=[[MAP['bg'],"bg","ground"],[MAP['p'],"fg","ground"]]
CATS=[["bg","bg (ground)","Aa Bb 123"],["p","fg","other / whitespace"],["kw","keyword","class  def  if  return"],["bi","builtin","len  echo  printf"],
 ["pp","preprocessor","#include  #define"],["fnd","function · def","resolve  push"],
 ["fnc","function · call","printf  rsync  get"],["dec","decorator → type","@dataclass"],
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
UIMAP=build_uimap(UI_FACES,DEFAULTS)

# Optional palette seed: THEME_STUDIO_SEED=<file.json> seeds the tool's starting
# palette / syntax / UI from a theme.json (path relative to
# this dir), instead of the hardcoded defaults above. Unset leaves them unchanged.
# Placed after every default it overrides (notably UIMAP) so the merge has targets.
# Mirrors what the in-page Import does, so reseed and import agree.
LOCKS=[]
# THEME_STUDIO_SEED=<file>.json opens an existing theme as the starting point.
# Unset starts empty: only bg/fg are in the palette.
_seed=os.environ.get('THEME_STUDIO_SEED')
_d=load_seed_data(_seed)
PALETTE,UIMAP,LOCKS=apply_seed_basics(_d,PALETTE,UIMAP,LOCKS)
PALETTE=normalize_palette(PALETTE)
SYNTAX=build_syntax(COLS,MAP,BOLD,ITALIC_MAP,DEFAULTS)
apply_syntax_seed(_d if _seed else {},SYNTAX,MAP)
# Bespoke package face lists and seed defaults live in face_data.py. Each entry
# is (key, label, preview, FACES, prefix, SEED); add an app by adding one row.
_BESPOKE_APPS=[
 ("org-mode","org-mode","org",ORG_FACES,"org-",ORG_SEED),
 ("magit","magit","magit",MAGIT_FACES,"magit-",MAGIT_SEED),
 ("elfeed","elfeed","elfeed",ELFEED_FACES,"elfeed-",ELFEED_SEED),
 ("mu4e","mu4e","mu4e",MU4E_FACES,"mu4e-",MU4E_SEED),
 ("ghostel","ghostel","ghostel",GHOSTEL_FACES,"ghostel-",GHOSTEL_SEED),
 ("auto-dim-other-buffers","auto-dim","autodim",AUTODIM_FACES,"auto-dim-other-buffers-",AUTODIM_SEED),
 ("dashboard","dashboard","dashboard",DASHBOARD_FACES,"dashboard-",DASHBOARD_SEED),
 ("lsp-mode","lsp-mode","lsp",LSP_FACES,"lsp-",LSP_SEED),
 ("git-gutter","git-gutter","gitgutter",GITGUTTER_FACES,"git-gutter:",GITGUTTER_SEED),
 ("flycheck","flycheck","flycheck",FLYCHECK_FACES,"flycheck-",FLYCHECK_SEED),
 ("dired","dired","dired",DIRED_FACES,"dired-",DIRED_SEED),
 ("dirvish","dirvish","dirvish",DIRVISH_FACES,"dirvish-",DIRVISH_SEED),
 ("calibredb","calibredb","calibredb",CALIBREDB_FACES,"calibredb-",CALIBREDB_SEED),
 ("erc","erc","erc",ERC_FACES,"erc-",ERC_SEED),
 ("org-drill","org-drill","orgdrill",ORGDRILL_FACES,"org-drill-",ORGDRILL_SEED),
 ("org-noter","org-noter","orgnoter",ORGNOTER_FACES,"org-noter-",ORGNOTER_SEED),
 ("signel","signel","signel",SIGNEL_FACES,"signel-",SIGNEL_SEED),
 ("pearl","pearl","pearl",PEARL_FACES,"pearl-",PEARL_SEED),
 ("slack","slack","slack",SLACK_FACES,"slack-",SLACK_SEED),
 ("telega","telega","telega",TELEGA_FACES,"telega-",TELEGA_SEED),
 ("shr","shr (HTML: nov/eww/mail)","shr",SHR_FACES,"shr-",SHR_SEED),
]
APPS={key:{"label":label,"preview":preview,"faces":face_rows(faces,prefix,seed)}
      for key,label,preview,faces,prefix,seed in _BESPOKE_APPS}
# Phase 6: merge the generated all-package inventory (refresh with build-inventory.el).
# Bespoke apps stay; every other installed package becomes an editable generic app.
_inv_path=os.path.join(HERE,"package-inventory.json")
add_inventory_apps(APPS, _inv_path)
apply_default_face_seeds(APPS, DEFAULTS)
# Apply seed theme package overrides when THEME_STUDIO_SEED is set: each full
# per-face spec (color + structure) replaces the hardcoded face seed before render.
apply_seed_packages(APPS,_d,_seed)

if DEFAULTS.available:
    add_default_palette_colors(PALETTE,MAP,SYNTAX,UIMAP,APPS,DEFAULTS)

PALETTE=normalize_palette(PALETTE)
HTML=read_text('theme-studio.template.html')
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
     .replace("PALETTE_GENERATOR_CORE_J",PALETTE_GENERATOR_CORE_BODY)
     .replace("PALETTE_GENERATOR_UI_J",PALETTE_GENERATOR_UI_BODY)
     .replace("PALETTE_ACTIONS_J",PALETTE_ACTIONS_BODY)
     .replace("BROWSER_GATES_J",BROWSER_GATES_BODY)
     .replace("COLOR_NAMES_J",json.dumps(COLOR_NAMES))
     .replace("SAMPLES_J",json.dumps(SAMPLES))
     .replace("PALETTE_J",json.dumps(PALETTE)).replace("CATS_J",json.dumps(CATS))
     .replace("UIFACES_J",json.dumps(UI_FACES)).replace("UIMAP_J",json.dumps(UIMAP)).replace("APPS_J",json.dumps(APPS))
     .replace("SYNTAX_J",json.dumps(SYNTAX)).replace("MAP_J",json.dumps(MAP)).replace("LOCKS_J",json.dumps(LOCKS)))

# Splice the stylesheet and script in first, then fill the data placeholders they
# carry. The page contains app.js exactly as fill_data(APP_BODY) renders it —
# APP_FILLED is that rendering, the handle the inline-integrity test asserts on.
HTML=fill_data(HTML.replace("STYLES_CSS",STYLES).replace("APP_JS",APP_BODY))
APP_FILLED=fill_data(APP_BODY)
OUT=os.path.join(HERE,'theme-studio.html')

def render_theme_studio(out_path=OUT):
    with open(out_path,"w") as out:
        out.write(HTML)
    print("wrote",out_path)

if __name__=='__main__':
    render_theme_studio()

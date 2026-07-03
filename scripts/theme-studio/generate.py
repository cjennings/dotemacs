import json, os, re, base64
from app_inventory import add_inventory_apps, add_nerd_icons_app, apply_default_face_seeds, apply_package_overrides, face_rows
from default_faces import DefaultFaces
from face_data import *
from face_specs import face_spec, ui_face_spec, migrate_legacy
HERE=os.path.dirname(os.path.abspath(__file__))

def read_text(name):
    with open(os.path.join(HERE,name)) as src:
        return src.read()

def read_json(name):
    return json.loads(read_text(name))

NERD_ICONS_LEGEND_FIELDS = ("key", "label", "face", "category", "glyph")
NERD_ICONS_GALLERY_GLYPH_FIELDS = ("glyph", "name")

_NO_ARTIFACT = object()  # distinguishes absent/malformed from a file that parsed to null

def _load_nerd_icons_artifact(path, kind, tail):
    """Open and JSON-parse the nerd-icons artifact at PATH. Return the parsed value,
    or _NO_ARTIFACT (with a KIND/TAIL-labeled warning) when absent or malformed.
    Shared skeleton for the legend and gallery loaders."""
    if not os.path.exists(path):
        print(f"WARNING: nerd-icons {kind} absent ({path}); {tail}")
        return _NO_ARTIFACT
    try:
        with open(path) as src:
            return json.load(src)
    except (json.JSONDecodeError, OSError) as exc:
        print(f"WARNING: nerd-icons {kind} malformed ({path}: {exc}); {tail}")
        return _NO_ARTIFACT

def load_nerd_icons_legend(path=None):
    """Return the nerd-icons legend rows, or None when the artifact is unusable.

    The legend is captured by build-nerd-icons-legend.el into nerd-icons-legend.json.
    The artifact is a JSON object {legend, gallery}; a legacy bare array is read as
    the legend directly (back-compat). Absent, malformed, empty, or carrying a row
    without all five string fields (key/label/face/category/glyph) -> None, with a
    warning, so the caller falls back to the generic nerd-icons app instead of
    erroring. nerd-icons not being installed at capture time yields an empty/absent
    file, which lands here as None.
    """
    path = path or os.path.join(HERE, "nerd-icons-legend.json")
    data = _load_nerd_icons_artifact(path, "legend", "generic nerd-icons app")
    if data is _NO_ARTIFACT:
        return None
    rows = data.get("legend") if isinstance(data, dict) else data
    if not isinstance(rows, list) or not rows:
        print(f"WARNING: nerd-icons legend empty ({path}); generic nerd-icons app")
        return None
    for row in rows:
        if not (isinstance(row, dict)
                and all(isinstance(row.get(f), str) and row.get(f)
                        for f in NERD_ICONS_LEGEND_FIELDS)):
            print(f"WARNING: nerd-icons legend row invalid ({row!r}); generic nerd-icons app")
            return None
    return rows

def load_nerd_icons_gallery(path=None):
    """Return the nerd-icons gallery groups, or None when absent/unusable.

    The gallery (the full colored catalog) rides nerd-icons-legend.json under the
    "gallery" key: a list of {face, hue, glyphs:[{glyph,name}]} groups captured by
    build-nerd-icons-legend.el, one group per color face, ordered by hue. A legacy
    array-only artifact (legend, no gallery), an absent/malformed file, or a
    structurally invalid group -> None, so the caller simply omits the gallery while
    the legend data still loads. Never raises.
    """
    path = path or os.path.join(HERE, "nerd-icons-legend.json")
    data = _load_nerd_icons_artifact(path, "gallery", "legend without gallery")
    if data is _NO_ARTIFACT:
        return None
    groups = data.get("gallery") if isinstance(data, dict) else None
    if not isinstance(groups, list) or not groups:
        return None  # legacy/array-only artifact: legend present, no gallery — not an error
    for group in groups:
        if not (isinstance(group, dict)
                and isinstance(group.get("face"), str) and group["face"].startswith("nerd-icons-")
                and isinstance(group.get("hue"), (int, float))
                and isinstance(group.get("glyphs"), list) and group["glyphs"]):
            print(f"WARNING: nerd-icons gallery group invalid ({group!r}); legend without gallery")
            return None
        for entry in group["glyphs"]:
            if not (isinstance(entry, dict)
                    and all(isinstance(entry.get(f), str) and entry.get(f)
                            for f in NERD_ICONS_GALLERY_GLYPH_FIELDS)):
                print(f"WARNING: nerd-icons gallery glyph invalid ({entry!r}); legend without gallery")
                return None
    return groups

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
# Inline the embedded nerd font as a base64 data: URI. The @font-face in
# styles.css references the woff2 by a relative path so the source stays editable;
# here that url is rewritten to a self-contained data: URI at generate time. The
# payoff is portability — the page renders the glyphs on any clone, with no
# dependency on a separately-shipped font file or a system-installed copy — and it
# removes any question about how a file:// font url loads across browsers.
# (The tofu bug this feature chased was NOT a load failure: the confirmed causes
# were a double-quoted font-family inside an inline style attribute, which
# silently dropped the family — see previews.js PREVIEW_FONT — and a woff2 encoded
# by woff2_compress that headed Chrome/Firefox reject; the woff2 is now encoded by
# fontTools via `make font`. The data: URI is the durable self-contained form, not
# the fix for those two bugs.)
_FONT_WOFF2='SymbolsNerdFontMono-Regular.woff2'
if os.path.exists(os.path.join(HERE,_FONT_WOFF2)):
    with open(os.path.join(HERE,_FONT_WOFF2),'rb') as _ff:
        _FONT_B64=base64.b64encode(_ff.read()).decode('ascii')
    STYLES=STYLES.replace('url("%s")'%_FONT_WOFF2,
                          'url("data:font/woff2;base64,%s")'%_FONT_B64)
APP_BODY=read_text('app.js')
# Custom dropdown / detail-editor / expander factories, split from app.js for
# navigability and spliced in at the CONTROLS_J token. Raw (no imports/exports).
CONTROLS_BODY=read_text('controls.js')
# Bespoke per-package preview renderers, spliced into the page <script> via the
# PREVIEWS_J token in app.js. No imports/exports, so read raw.
PREVIEWS_BODY=read_text('previews.js')
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
# Face docstrings (first line each), dumped from a live Emacs via
# face-docs-dump.el. Two maps: "faces" keyed by real face name (UI + package
# tables), "syntax" keyed by theme-studio category (the syntax table). Inlined so
# the element hovers can show each face's docstring on top of the existing title.
_face_docs=read_json('face-docs.json')
FACE_DOCS=_face_docs['faces']
SYNTAX_DOCS=_face_docs['syntax']
ns={}
src=read_text('samples.py')
exec(src[:src.index('# THEME_STUDIO_DATA_END')], ns)
SAMPLES={"Elisp":ns['ELS'],"Go":ns['GOS'],"Python":ns['PYS'],"TypeScript":ns['TSS'],"Java":ns['JAS'],"C":ns['CS'],"C++":ns['CPS'],"Rust":ns['RUSTS'],"Zig":ns['ZIGS'],"Shell":ns['SHS'],
 "Racket":ns['RACKETS'],"Scheme":ns['SCHEMES'],"Haskell":ns['HASKELLS'],"OCaml":ns['OCAMLS'],"Scala":ns['SCALAS'],"Kotlin":ns['KOTLINS'],"Swift":ns['SWIFTS'],"Lua":ns['LUAS'],"Ruby":ns['RUBYS'],"Perl":ns['PERLS'],"R":ns['RLANGS'],"Erlang":ns['ERLANGS'],"SQL":ns['SQLS'],"PHP":ns['PHPS'],"Ada":ns['ADAS'],"Fortran":ns['FORTRANS'],"MATLAB":ns['MATLABS'],"Assembly":ns['ASMS'],"HTML":ns['HTMLS']}
COLS=ns['COLS']
DEFAULT_FACES_PATH=os.path.join(HERE,'emacs-default-faces.json')

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
    uimap["link"]["underline"]={"style":"line","color":None}
    for face in ("lazy-highlight","show-paren-match"):
        uimap[face]["underline"]={"style":"line","color":None}
    for face in ("error","warning","success"):
        uimap[face]["weight"]="bold"
    for face in ("mode-line","mode-line-inactive"):
        uimap[face]["box"]={"style":"released","width":1,"color":None}

# The chrome faces that carry their own absolute height pin. header-line and
# mode-line-inactive are deliberately absent: both inherit mode-line, so the
# pin reaches them through the chain and a seed of their own would duplicate
# state. The line-number pair is seeded individually because the generated
# theme's explicit specs leave their :inherit unspecified at runtime.
CHROME_HEIGHT_SEEDS = ("mode-line", "tab-bar", "tab-line",
                       "line-number", "line-number-current-line")


def apply_chrome_height_defaults(uimap):
    """Seed absolute heights on the chrome so it never tracks the buffer default.

    Chrome :height is unspecified in stock Emacs, so it follows the buffer's
    default face height -- a buffer that remaps default larger (the
    nov-reading view) inflates its modeline and gutters with it. A fixed
    1/10pt integer pins each bar. 130 matches the configured laptop
    default-height; every seed is editable in the studio's size column
    (theme-studio-editable-height-spec)."""
    for name in CHROME_HEIGHT_SEEDS:
        face = uimap.get(name)
        if face and face.get("height") is None:
            face["height"] = 130
            face["heightMode"] = "abs"

def apply_hover_box_default(uimap):
    """Seed the mode-line hover face's box.

    `mode-line-highlight` (applied via mouse-face to the clickable mode-line
    segments) is absent from the captured Emacs snapshot, so seed() returns
    blank for it in both branches below. Emacs's stock default is a raised
    released-button box; default to that so the studio reflects current
    behavior, then let the user flatten or recolor it. A future snapshot that
    captures the face wins (the box-already-set guard leaves it alone)."""
    face=uimap.get("mode-line-highlight")
    if face and not face.get("box"):
        face["box"]={"style":"released","width":1,"color":None}

def build_uimap(ui_faces,defaults):
    if defaults.available:
        uimap={face[0]:ui_face_spec(defaults.seed(face[0],False)) for face in ui_faces}
    else:
        uimap={face[0]:ui_face_spec() for face in ui_faces}
        apply_builtin_fallback_styles(uimap)
    apply_hover_box_default(uimap)
    apply_chrome_height_defaults(uimap)
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
            uimap[key]=migrate_legacy(value)
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

def _harvest_spec_colors(palette,defaults,spec):
    """Add a face spec's fg, bg, and box color (if any) to the palette, in order."""
    add_palette_color(palette,defaults,spec.get('fg'))
    add_palette_color(palette,defaults,spec.get('bg'))
    if spec.get('box'):
        add_palette_color(palette,defaults,spec['box'].get('color'))

def add_default_palette_colors(palette,map_,syntax,uimap,apps,defaults):
    for key,value in map_.items():
        add_palette_color(palette,defaults,value,'bg' if key=='bg' else 'fg' if key=='p' else None)
    for spec in syntax.values():
        _harvest_spec_colors(palette,defaults,spec)
    for _face,spec in uimap.items():
        _harvest_spec_colors(palette,defaults,spec)
    for app in apps.values():
        for _face,_label,spec in app['faces']:
            _harvest_spec_colors(palette,defaults,spec)

def apply_seed_packages(apps,data,seed):
    if seed:
        apply_package_overrides(apps,data.get('packages'))

CATS=[["bg","bg (ground)","Aa Bb 123"],["p","fg","other / whitespace"],["kw","keyword","class  def  if  return"],["bi","builtin","len  echo  printf"],
 ["pp","preprocessor","#include  #define"],["fnd","function · def","resolve  push"],
 ["fnc","function · call","printf  rsync  get"],["dec","decorator → type","@dataclass"],
 ["ty","type / class","int  str  Order  Queue"],["prop","property / field","id  name  items"],
 ["con","constant","None  nil  NULL  true"],["num","number","8080  100  -1"],
 ["str","string",'"dupre"  "fmt"'],["esc","escape","\\n  \\t"],["re","regexp","/^#[0-9a-f]+/"],
 ["rxgb","regexp backslash","\\\\("],["rxgc","regexp construct","\\( \\)"],
 ["doc","docstring",'"""..."""'],["dmark","doc mark","\\[cmd]  `sym'"],
 ["cm","comment","# reject nil"],["cmd","comment delim","#  //  ;;"],
 ["var","variable / use","value  key  self"],["op","operator",":  =  ->  =="],
 ["neg","negation char","!"],["punc","punctuation","{ }  ( )  ;"],
 ["warn","warn","TODO  FIXME"]]
UI_FACES=[["cursor","cursor","Aa|"],["region","region (selection)","selected text"],
 ["hl-line","hl-line (current line)","current line"],["highlight","highlight","hover"],
 ["mode-line","mode-line","status active"],
 ["mode-line-highlight","mode-line-highlight (hover)","git:main"],
 ["mode-line-inactive","mode-line-inactive","status idle"],
 ["header-line","header-line","breadcrumb / doc info"],
 ["tab-bar","tab-bar","tabs"],
 ["tab-line","tab-line","buffer tabs"],
 ["fringe","fringe","| |"],["line-number","line-number","  42"],
 ["line-number-current-line","line-number-current-line","> 42"],["minibuffer-prompt","minibuffer-prompt","M-x "],
 ["isearch","isearch (match)","match"],["lazy-highlight","lazy-highlight","other match"],
 ["isearch-fail","isearch-fail","no match"],["show-paren-match","show-paren-match","( )"],
 ["show-paren-mismatch","show-paren-mismatch",") ("],["link","link","https://"],
 ["error","error","error!"],["warning","warning","warning"],
 ["success","success","ok"],["vertical-border","vertical-border","|"]]

OUT=os.path.join(HERE,'theme-studio.html')
_CACHE={}

def _build():
    """Assemble the page, caching the derived data + HTML. Deferred from import
    so a consumer that only needs the cheap module constants (e.g.
    face_coverage.py reading UI_FACES) does not pay the full DEFAULTS + inventory
    + fill cost; the file write stays __main__-guarded as before."""
    if _CACHE:
        return _CACHE
    DEFAULTS=DefaultFaces.from_path(DEFAULT_FACES_PATH)
    MAP,BOLD,ITALIC_MAP=initial_maps(COLS,DEFAULTS)
    PALETTE=[[MAP['bg'],"bg","ground"],[MAP['p'],"fg","ground"]]
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
    # Bespoke apps are single-sourced as BESPOKE_APP_SPECS in face_data.py (one
    # row per app: key, label, preview, FACES, prefix, SEED).
    APPS={key:{"label":label,"preview":preview,"hover":APP_HOVERS.get(key,""),"faces":face_rows(faces,prefix,seed)}
          for key,label,preview,faces,prefix,seed in BESPOKE_APP_SPECS}
    # Phase 6: merge the generated all-package inventory (refresh with build-inventory.el).
    # Bespoke apps stay; every other installed package becomes an editable generic app.
    _inv_path=os.path.join(HERE,"package-inventory.json")
    # nerd-icons becomes a bespoke filetype-legend app when its captured legend is
    # valid; otherwise add_inventory_apps below makes it a plain generic app (the
    # fallback). Must precede add_inventory_apps so the generic path skips it.
    add_nerd_icons_app(APPS, _inv_path, load_nerd_icons_legend(), load_nerd_icons_gallery())
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
         .replace("CONTROLS_J",CONTROLS_BODY)
         .replace("PREVIEWS_J",PREVIEWS_BODY)
         .replace("APP_UTIL_J",APP_UTIL_BODY)
         .replace("PALETTE_GENERATOR_CORE_J",PALETTE_GENERATOR_CORE_BODY)
         .replace("PALETTE_GENERATOR_UI_J",PALETTE_GENERATOR_UI_BODY)
         .replace("PALETTE_ACTIONS_J",PALETTE_ACTIONS_BODY)
         .replace("BROWSER_GATES_J",BROWSER_GATES_BODY)
         .replace("COLOR_NAMES_J",json.dumps(COLOR_NAMES))
         .replace("FACE_DOCS_J",json.dumps(FACE_DOCS)).replace("SYNTAX_DOCS_J",json.dumps(SYNTAX_DOCS))
         .replace("SAMPLES_J",json.dumps(SAMPLES))
         .replace("PALETTE_J",json.dumps(PALETTE)).replace("CATS_J",json.dumps(CATS))
         .replace("UIFACES_J",json.dumps(UI_FACES)).replace("UIMAP_J",json.dumps(UIMAP)).replace("APPS_J",json.dumps(APPS))
         .replace("SYNTAX_J",json.dumps(SYNTAX)).replace("MAP_J",json.dumps(MAP)).replace("LOCKS_J",json.dumps(LOCKS)))

    # Splice the stylesheet and script in first, then fill the data placeholders they
    # carry. The page contains app.js exactly as fill_data(APP_BODY) renders it —
    # APP_FILLED is that rendering, the handle the inline-integrity test asserts on.
    HTML=fill_data(HTML.replace("STYLES_CSS",STYLES).replace("APP_JS",APP_BODY))
    APP_FILLED=fill_data(APP_BODY)
    _CACHE.update(DEFAULTS=DEFAULTS, MAP=MAP, BOLD=BOLD, ITALIC_MAP=ITALIC_MAP,
                  PALETTE=PALETTE, UIMAP=UIMAP, LOCKS=LOCKS, SYNTAX=SYNTAX,
                  APPS=APPS, HTML=HTML, APP_FILLED=APP_FILLED)
    return _CACHE

def __getattr__(name):
    # PEP 562: lazily expose any built attribute (HTML, MAP, APPS, ...). Every
    # other name is a real module global and never reaches here.
    built = _build()
    if name in built:
        return built[name]
    raise AttributeError(f"module {__name__!r} has no attribute {name!r}")

def render_theme_studio(out_path=OUT):
    with open(out_path,"w") as out:
        out.write(_build()['HTML'])
    print("wrote",out_path)

if __name__=='__main__':
    render_theme_studio()

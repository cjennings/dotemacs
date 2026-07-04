// seed-core.js — the theme-studio seeding engine (Phase 1).
//
// The seed model as data and the pure seed() operation. This is
// theme-coloring-guide.org made executable: a named palette (OKLCH-generated
// shades over a handful of dupre anchor hues), a role-to-treatment table (the
// guide's seed table), and a face-to-role map for each of the three owned tiers
// (syntax, UI, org). seed(model) classifies every face and applies the table,
// producing default assignments in the shape the import path already consumes.
//
// Pure: no DOM, no side effects. node imports this module for its unit tests;
// generate.py strips the import line and inlines the body into the page (below
// the colormath core, so oklchOf/oklch2hex are already present) so the browser
// #seedtest runs the same code. One source of truth, like colormath.js.
//
// Scope (Package scope in the spec): seed() owns syntax, UI, and org among
// packages. The other ~20 bespoke packages keep their curated APPS seeds, so
// seed().packages carries only org-mode; the rest flow through seedPkgmap().
import { oklchOf, oklch2hex } from './colormath.js';

// --- anchors -------------------------------------------------------------
// The base hues, taken from the bundled dupre palette. Each accent family is
// anchored here; its quieter/brighter shades are OKLCH-derived (below). Neutrals
// are taken directly — no ground is pure-white text, and pure black stays the
// ground only (guide principle 5).
const ANCHORS = {
  ground: '#000000', 'bg-dim': '#1a1714',
  fg: '#a9b2bb',        // base identity (dupre silver): comfortable, not pure white
  'muted-fg': '#838d97', // structure lane (dupre steel)
  comment: '#5e6770',    // low-contrast comment lane (dupre pewter)
  blue: '#67809c', gold: '#e8bd30', regal: '#9b5fd0',
  sage: '#5d9b86', terracotta: '#cb6b4d',
};

// --- OKLCH shade helpers -------------------------------------------------
// Step an anchor by a lightness delta and a chroma multiplier, hue held. A
// quieter shade is darker + lower chroma; a brighter shade is the reverse.
function shade(hex, dL, cMul) {
  const { L, C, H } = oklchOf(hex);
  return oklch2hex(clampL(L + dL), Math.max(0, C * cMul), H).hex;
}
// A color placed by absolute OKLCH — used for the signal hues, which are
// conventional angles rather than shifts of a syntax accent.
function atHue(L, C, H) { return oklch2hex(clampL(L), C, ((H % 360) + 360) % 360).hex; }
function clampL(L) { return L < 0 ? 0 : L > 1 ? 1 : L; }

// The heading ramp: one hue across four descending lightness steps (level 1
// strongest). Deeper org levels cycle through these past level 4.
function headingRamp(anchorHex) {
  const { C, H } = oklchOf(anchorHex);
  return [0.78, 0.68, 0.58, 0.48].map((L) => oklch2hex(L, C, H).hex);
}

// Build the named swatch set + heading ramp from the anchors. The blue-grey
// builtin and gold-quiet call are the two shades dupre lacks and gains here.
function buildModel(anchors = ANCHORS) {
  const a = anchors;
  const swatch = {
    ground: a.ground, 'bg-dim': a['bg-dim'], fg: a.fg,
    'muted-fg': a['muted-fg'], comment: a.comment,
    blue: a.blue,
    'blue-grey': shade(a.blue, -0.05, 0.5),   // builtin: blue hue, lower chroma/lightness
    gold: a.gold,
    'gold-quiet': shade(a.gold, -0.08, 0.6),  // call: quieter same-hue gold
    regal: a.regal,
    sage: a.sage,
    'sage-muted': shade(a.sage, -0.03, 0.6),  // docstring
    'sage-bright': shade(a.sage, 0.08, 1.15), // escape
    teal: (() => { const { L, C, H } = oklchOf(a.sage); return oklch2hex(clampL(L + 0.05), C * 1.1, H - 25).hex; })(), // regexp
    terracotta: a.terracotta,
    red: atHue(0.62, 0.15, 29),   // signal: error / deletion
    amber: atHue(0.80, 0.14, 75), // signal: warning / modified
    green: atHue(0.72, 0.14, 145),// signal: success / addition
    tint: atHue(0.32, 0.03, oklchOf(a.blue).H),        // transient state bg (quiet)
    'tint-strong': atHue(0.42, 0.06, oklchOf(a.blue).H), // active match chip
  };
  return { swatch, ramp: headingRamp(a.blue), roles: ROLES };
}

// --- the role-to-treatment table (the guide's seed table as data) --------
// Each role maps to a swatch, an optional weight/slant/underline, and a channel
// (fg is identity, bg is state). channel defaults to fg.
const ROLES = {
  base:      { swatch: 'fg' },
  structure: { swatch: 'muted-fg' },
  control:   { swatch: 'blue', weight: 'bold' },
  builtin:   { swatch: 'blue-grey' },
  def:       { swatch: 'gold', weight: 'bold' },
  call:      { swatch: 'gold-quiet' },
  type:      { swatch: 'regal' },
  string:    { swatch: 'sage' },
  docstring: { swatch: 'sage-muted', slant: 'italic' },
  escape:    { swatch: 'sage-bright' },
  regexp:    { swatch: 'teal' },
  literal:   { swatch: 'terracotta' },
  comment:   { swatch: 'comment', slant: 'italic' },
  sig_error: { swatch: 'red' },
  sig_warn:  { swatch: 'amber' },
  sig_ok:    { swatch: 'green' },
  sig_link:  { swatch: 'blue', underline: true },
  state:     { swatch: 'tint', channel: 'bg' },
};

// A blank full face spec; seed fills only the fields a role sets.
function blankSpec() {
  return { fg: null, bg: null, weight: null, slant: null, underline: null, strike: null, inherit: null, height: null };
}

// Resolve a ROLES role against the model into a face spec.
function resolveRole(model, role) {
  const r = model.roles[role];
  const hex = model.swatch[r.swatch];
  const s = blankSpec();
  if (r.channel === 'bg') s.bg = hex; else s.fg = hex;
  if (r.weight) s.weight = r.weight;
  if (r.slant) s.slant = r.slant;
  if (r.underline) s.underline = { style: 'line', color: null };
  return s;
}

// --- face-to-role maps ---------------------------------------------------

// Syntax: CATS key -> role. bg is handled specially (the ground).
const SYNTAX_ROLES = {
  p: 'base', var: 'base',
  op: 'structure', punc: 'structure', neg: 'structure', cmd: 'structure',
  kw: 'control', pp: 'control',
  bi: 'builtin',
  fnd: 'def', fnc: 'call',
  dec: 'type', ty: 'type', prop: 'type',
  con: 'literal', num: 'literal',
  str: 'string', doc: 'docstring',
  esc: 'escape', dmark: 'escape',
  re: 'regexp', rxgb: 'regexp', rxgc: 'regexp',
  cm: 'comment',
  warn: 'sig_warn',
};

// UI: face -> either a ROLES role (state/signal/link/control) or an inline
// chrome spec. Chrome is inherently multi-attribute (fg + bg, active vs idle),
// so it does not force through the single-swatch role resolver.
function uiSeed(model) {
  const sw = model.swatch, out = {};
  const role = (r) => resolveRole(model, r);
  const spec = (o) => Object.assign(blankSpec(), o);
  // Transient state: background tint, no foreground. lazy-highlight (other
  // matches) shares the quiet tint; isearch (current match) gets a louder chip.
  for (const f of ['region', 'hl-line', 'highlight', 'show-paren-match', 'lazy-highlight']) out[f] = role('state');
  out.isearch = spec({ bg: sw['tint-strong'] });           // active match, louder chip
  // Signals (convention hues) with a weight for redundancy.
  out.error = spec({ fg: sw.red, weight: 'bold' });
  out.warning = spec({ fg: sw.amber, weight: 'bold' });
  out.success = spec({ fg: sw.green, weight: 'bold' });
  out['isearch-fail'] = spec({ fg: sw.red, weight: 'bold' });
  out['show-paren-mismatch'] = spec({ bg: sw.red });        // shape + color, not color alone
  out.link = role('sig_link');
  // Chrome: active brighter than idle (guide principle 3).
  out['mode-line'] = spec({ fg: sw.fg, bg: sw['bg-dim'] });
  out['mode-line-inactive'] = spec({ fg: sw['muted-fg'], bg: sw['bg-dim'] });
  out['mode-line-highlight'] = spec({ fg: sw.fg });
  for (const f of ['header-line', 'tab-bar', 'tab-line']) out[f] = spec({ fg: sw['muted-fg'], bg: sw['bg-dim'] });
  out['line-number'] = spec({ fg: sw.comment });
  out['line-number-current-line'] = spec({ fg: sw.fg });
  out.fringe = spec({ fg: sw.comment });
  out['vertical-border'] = spec({ fg: sw['bg-dim'] });
  out['minibuffer-prompt'] = role('control');
  out.cursor = spec({ bg: sw.fg });
  return out;
}

// Org: face -> role/heading/inline. Faces not named here seed to base.
const ORG_MARKUP = ['org-meta-line', 'org-drawer', 'org-special-keyword', 'org-property-value',
  'org-block-begin-line', 'org-block-end-line', 'org-ellipsis', 'org-tag', 'org-date',
  'org-document-info-keyword', 'org-macro', 'org-target', 'org-footnote-def'];
const ORG_CODE = ['org-code', 'org-verbatim', 'org-inline-src-block'];
const ORG_LINK = ['org-link', 'org-cite', 'org-cite-key', 'org-footnote'];
const ORG_EMPHASIS = ['org-quote', 'org-verse'];

function orgSeed(model, orgFaces) {
  const sw = model.swatch, out = {};
  const role = (r) => resolveRole(model, r);
  const spec = (o) => Object.assign(blankSpec(), o);
  for (const face of orgFaces) {
    const lvl = /^org-level-([1-8])$/.exec(face);
    if (lvl) {
      const i = Number(lvl[1]);
      out[face] = spec({ fg: model.ramp[(i - 1) % model.ramp.length], weight: i === 1 ? 'bold' : null });
    } else if (face === 'org-document-title') {
      out[face] = spec({ fg: sw.gold, weight: 'bold' });
    } else if (ORG_CODE.includes(face)) {
      out[face] = spec({ fg: sw.terracotta, inherit: 'fixed-pitch' });
    } else if (face === 'org-block') {
      out[face] = spec({ bg: sw['bg-dim'], inherit: 'fixed-pitch' });
    } else if (ORG_LINK.includes(face)) {
      out[face] = spec({ fg: sw.blue, underline: { style: 'line', color: null } });
    } else if (ORG_MARKUP.includes(face)) {
      out[face] = spec({ fg: sw['muted-fg'] });
    } else if (face === 'org-todo' || face === 'org-imminent-deadline') {
      out[face] = spec({ fg: sw.red, weight: 'bold' });
    } else if (face === 'org-upcoming-deadline') {
      out[face] = spec({ fg: sw.amber });
    } else if (face === 'org-scheduled' || face === 'org-scheduled-today') {
      out[face] = spec({ fg: sw.comment });
    } else if (face === 'org-done' || face === 'org-headline-done' || face === 'org-agenda-done') {
      out[face] = spec({ fg: sw.comment, strike: { color: null } });
    } else if (ORG_EMPHASIS.includes(face)) {
      out[face] = spec({ slant: 'italic' });
    } else {
      out[face] = role('base');
    }
  }
  return out;
}

// The org faces the engine seeds. Kept in step with ORG_FACES in face_data.py;
// a face present here but absent there (or the reverse) simply seeds/omits it.
// The representative set the guide names is what matters for the tier.
const ORG_FACES = ('org-document-title org-document-info org-document-info-keyword '
  + 'org-level-1 org-level-2 org-level-3 org-level-4 org-level-5 org-level-6 org-level-7 org-level-8 '
  + 'org-headline-done org-todo org-done org-priority org-tag org-special-keyword org-drawer '
  + 'org-property-value org-warning org-link org-cite org-cite-key org-footnote org-date '
  + 'org-macro org-target org-block org-block-begin-line org-block-end-line org-code org-verbatim '
  + 'org-inline-src-block org-quote org-verse org-meta-line org-ellipsis '
  + 'org-scheduled org-scheduled-today org-upcoming-deadline org-imminent-deadline '
  + 'org-agenda-done org-table org-formula').split(' ');

// --- seed(): apply the table through each tier's face-to-role map --------
// Returns {syntax, ui, packages} default assignments. packages carries only
// org-mode (Package scope); the non-org curated defaults flow through
// seedPkgmap() over the APPS dicts, untouched by the engine.
function seed(model, opts = {}) {
  const cats = opts.cats || CATS_KEYS;
  const orgFaces = opts.orgFaces || ORG_FACES;
  const syntax = {};
  for (const k of cats) {
    if (k === 'bg') { syntax.bg = Object.assign(blankSpec(), { fg: model.swatch.ground }); continue; }
    const rname = SYNTAX_ROLES[k] || 'base';
    syntax[k] = resolveRole(model, rname);
  }
  return { syntax, ui: uiSeed(model), packages: { 'org-mode': orgSeed(model, orgFaces) } };
}

// The syntax categories the engine seeds, kept in step with CATS in generate.py.
// Passed explicitly by the page (opts.cats) from the live CATS so the two never
// drift; this literal is the standalone/test default.
const CATS_KEYS = ['bg', 'p', 'kw', 'bi', 'pp', 'fnd', 'fnc', 'dec', 'ty', 'prop',
  'con', 'num', 'str', 'esc', 're', 'rxgb', 'rxgc', 'doc', 'dmark', 'cm', 'cmd',
  'var', 'op', 'neg', 'punc', 'warn'];

export { ANCHORS, buildModel, seed, ROLES, SYNTAX_ROLES, ORG_FACES, CATS_KEYS };

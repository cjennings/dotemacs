// Unit tests for the seeding engine (seed-core.js): the seed model as data and
// the pure seed() operation. seed() projects theme-coloring-guide.org's role/seed
// table onto the three owned tiers (syntax, UI, org), reusing colormath.js OKLCH
// generation for the palette shades and heading ramp.
// Run: node --test scripts/theme-studio/

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { buildModel, seed, ROLES } from './seed-core.js';
import { oklchOf } from './colormath.js';

const here = fileURLToPath(new URL('.', import.meta.url));
const HEX = /^#[0-9a-f]{6}$/;

// --- the model: OKLCH-generated palette shades ---------------------------

test('buildModel: every swatch is a valid in-gamut hex', () => {
  const m = buildModel();
  for (const [name, hex] of Object.entries(m.swatch)) {
    assert.match(hex, HEX, `swatch ${name} is not a hex: ${hex}`);
  }
});

test('buildModel: builtin blue-grey is the blue hue at lower chroma and lightness', () => {
  const m = buildModel();
  const blue = oklchOf(m.swatch.blue), grey = oklchOf(m.swatch['blue-grey']);
  assert.ok(grey.C < blue.C, 'blue-grey should have lower chroma than blue');
  assert.ok(grey.L < blue.L, 'blue-grey should be darker than blue');
  assert.ok(Math.abs(grey.H - blue.H) < 15, 'blue-grey should keep the blue hue');
});

test('buildModel: the heading ramp descends in lightness, level 1 strongest', () => {
  const m = buildModel();
  assert.equal(m.ramp.length, 4);
  for (let i = 1; i < m.ramp.length; i++) {
    assert.ok(oklchOf(m.ramp[i - 1]).L > oklchOf(m.ramp[i]).L,
      `ramp step ${i} should be darker than step ${i - 1}`);
  }
});

// --- seed(): syntax tier -------------------------------------------------

test('seed: syntax builtin (bi) resolves to blue-grey', () => {
  const m = buildModel(), s = seed(m).syntax;
  assert.equal(s.bi.fg, m.swatch['blue-grey']);
  assert.notEqual(s.bi.weight, 'bold');
});

test('seed: syntax definition (fnd) is gold and bold; call (fnc) is quieter gold, not bold', () => {
  const m = buildModel(), s = seed(m).syntax;
  assert.equal(s.fnd.fg, m.swatch.gold);
  assert.equal(s.fnd.weight, 'bold');
  assert.equal(s.fnc.fg, m.swatch['gold-quiet']);
  assert.notEqual(s.fnc.weight, 'bold');
});

test('seed: syntax base + structure + keyword + literal + string land on their swatches', () => {
  const m = buildModel(), s = seed(m).syntax;
  assert.equal(s.var.fg, m.swatch.fg);       // base identity
  assert.equal(s.p.fg, m.swatch.fg);
  assert.equal(s.op.fg, m.swatch['muted-fg']); // structure
  assert.equal(s.punc.fg, m.swatch['muted-fg']);
  assert.equal(s.kw.fg, m.swatch.blue);        // control
  assert.equal(s.kw.weight, 'bold');
  assert.equal(s.num.fg, m.swatch.terracotta); // literal
  assert.equal(s.con.fg, m.swatch.terracotta);
  assert.equal(s.str.fg, m.swatch.sage);       // string
  assert.equal(s.ty.fg, m.swatch.regal);       // type
});

test('seed: docstring and comment take italic; comment is the low-contrast lane', () => {
  const m = buildModel(), s = seed(m).syntax;
  assert.equal(s.doc.slant, 'italic');
  assert.equal(s.cm.slant, 'italic');
  assert.equal(s.cm.fg, m.swatch.comment);
});

test('seed: regexp and escape use the teal/bright-green lanes; bg is the ground', () => {
  const m = buildModel(), s = seed(m).syntax;
  assert.equal(s.re.fg, m.swatch.teal);
  assert.equal(s.rxgb.fg, m.swatch.teal);
  assert.equal(s.esc.fg, m.swatch['sage-bright']);
  assert.equal(s.bg.fg, m.swatch.ground);
});

// --- seed(): UI tier -----------------------------------------------------

test('seed: transient state faces are background-only (no foreground)', () => {
  const m = buildModel(), u = seed(m).ui;
  for (const f of ['region', 'hl-line', 'highlight', 'show-paren-match']) {
    assert.ok(u[f].bg, `${f} should carry a background tint`);
    assert.ok(!u[f].fg, `${f} should not set a foreground`);
  }
});

test('seed: link is blue and underlined (redundant encoding)', () => {
  const m = buildModel(), u = seed(m).ui;
  assert.equal(u.link.fg, m.swatch.blue);
  assert.ok(u.link.underline, 'link should be underlined');
});

test('seed: signal faces sit on the convention hues, with weight for redundancy', () => {
  const m = buildModel(), u = seed(m).ui;
  assert.equal(u.error.fg, m.swatch.red);
  assert.equal(u.error.weight, 'bold');
  assert.equal(u.warning.fg, m.swatch.amber);
  assert.equal(u.success.fg, m.swatch.green);
  assert.equal(u['isearch-fail'].fg, m.swatch.red);
});

test('seed: active chrome differs from idle chrome', () => {
  const m = buildModel(), u = seed(m).ui;
  assert.notEqual(u['mode-line'].fg, u['mode-line-inactive'].fg);
  assert.notEqual(u['line-number-current-line'].fg, u['line-number'].fg);
});

// --- seed(): org package tier -------------------------------------------

test('seed: packages carries only org-mode (non-org bespoke packages untouched)', () => {
  const m = buildModel(), p = seed(m).packages;
  assert.deepEqual(Object.keys(p), ['org-mode']);
});

test('seed: org headings ramp — level 1 strongest and bold, deeper levels quieter', () => {
  const m = buildModel(), org = seed(m).packages['org-mode'];
  assert.equal(org['org-level-1'].weight, 'bold');
  assert.ok(oklchOf(org['org-level-1'].fg).L > oklchOf(org['org-level-2'].fg).L,
    'org-level-1 should be lighter (stronger) than org-level-2');
});

test('seed: org code-like faces reuse the syntax literal lane', () => {
  const m = buildModel(), org = seed(m).packages['org-mode'];
  assert.equal(org['org-code'].fg, m.swatch.terracotta);
  assert.equal(org['org-code'].inherit, 'fixed-pitch');
});

test('seed: org link underlined; org-done receded with strikethrough; org-todo warm', () => {
  const m = buildModel(), org = seed(m).packages['org-mode'];
  assert.ok(org['org-link'].underline, 'org-link should be underlined');
  assert.ok(org['org-done'].strike, 'org-done should be struck through');
  assert.equal(org['org-todo'].fg, m.swatch.red);
});

// --- purity --------------------------------------------------------------

test('seed: pure — two calls deep-equal and the model is not mutated', () => {
  const m = buildModel();
  const before = JSON.stringify(m);
  const a = seed(m), b = seed(m);
  assert.deepEqual(a, b);
  assert.equal(JSON.stringify(m), before, 'seed() must not mutate the model');
});

test('ROLES: the table exposes the guide roles as data', () => {
  assert.equal(ROLES.builtin.swatch, 'blue-grey');
  assert.equal(ROLES.def.swatch, 'gold');
  assert.equal(ROLES.def.weight, 'bold');
  assert.equal(ROLES.state.channel, 'bg');
  assert.equal(ROLES.sig_link.underline, true);
});

// --- inline-integrity ----------------------------------------------------
// The page must carry seed-core.js's body (sans import/export) verbatim — the
// same strip generate.py applies. Requires `python3 generate.py`.
import { stripInlinedBody } from './inline-strip.mjs';

test('inline-integrity: theme-studio.html contains the seed-core.js body verbatim', () => {
  const body = stripInlinedBody(readFileSync(here + 'seed-core.js', 'utf8'));
  const html = readFileSync(here + 'theme-studio.html', 'utf8');
  assert.ok(html.includes(body), 'generated page is missing the seed-core.js body verbatim');
});

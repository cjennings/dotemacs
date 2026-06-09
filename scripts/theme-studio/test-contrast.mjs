// Unit tests for the background-contrast safety core (app-core.js): fgSetFor,
// floor, and lMax. Phase 3 of the palette-ramps spec. A background overlay sits
// behind many foregrounds at once, so its real constraint is the worst-case
// (minimum) contrast over that foreground set, and the lightest background that
// keeps the floor above the target. Pure, no DOM. Run: node --test scripts/theme-studio/

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { fgSetFor, floor, lMax, COVERED_FACES } from './app-core.js';
import { contrast, oklch2hex } from './colormath.js';

const DEFAULT_FG = '#f0fef0';
const stateWith = (syntaxAssignments) => ({ covered: COVERED_FACES, syntaxAssignments, defaultFg: DEFAULT_FG });

// --- fgSetFor ---------------------------------------------------------------

test('fgSetFor: Normal — covered face gets default fg plus the distinct syntax colors', () => {
  const r = fgSetFor('region', stateWith([
    { role: 'keyword', hex: '#67809c' },
    { role: 'string', hex: '#a3b18a' },
  ]));
  assert.equal(r.reason, undefined);
  assert.equal(r.set.length, 3); // default + 2 syntax
  const hexes = r.set.map(e => e.hex);
  assert.ok(hexes.includes('#f0fef0') && hexes.includes('#67809c') && hexes.includes('#a3b18a'));
  assert.equal(r.set.find(e => e.hex === '#67809c').label, 'keyword');
  assert.equal(r.set.find(e => e.hex === '#f0fef0').label, 'default');
});

test('fgSetFor: Boundary — a syntax hex equal to the default collapses, role label wins', () => {
  const r = fgSetFor('region', { covered: COVERED_FACES, syntaxAssignments: [{ role: 'keyword', hex: '#f0fef0' }], defaultFg: '#f0fef0' });
  assert.equal(r.set.length, 1);
  assert.equal(r.set[0].label, 'keyword'); // role preferred over 'default'
});

test('fgSetFor: Boundary — null/blank syntax hexes are dropped', () => {
  const r = fgSetFor('isearch', stateWith([{ role: 'a', hex: null }, { role: 'b', hex: '#112233' }]));
  assert.equal(r.set.length, 2); // default + the one real hex
  assert.ok(r.set.some(e => e.hex === '#112233'));
});

test('fgSetFor: Error — a face outside the covered set is out-of-scope', () => {
  const r = fgSetFor('mode-line', stateWith([{ role: 'keyword', hex: '#67809c' }]));
  assert.deepEqual(r, { set: [], reason: 'out-of-scope' });
});

test('fgSetFor: Error — a covered face with no syntax assignments is empty', () => {
  const r = fgSetFor('hl-line', stateWith([]));
  assert.deepEqual(r, { set: [], reason: 'empty' });
});

test('fgSetFor: Normal — every covered face is in scope', () => {
  for (const f of COVERED_FACES) {
    const r = fgSetFor(f, stateWith([{ role: 'kw', hex: '#67809c' }]));
    assert.equal(r.reason, undefined, `${f} should be covered`);
  }
});

// --- floor ------------------------------------------------------------------

test('floor: Normal — the keyword-blue worst case sets the floor and is named', () => {
  // sterling's keyword blue is the darkest foreground; against a lifted highlight
  // background it is the limiting color while the light default still clears.
  const fgSet = [{ hex: '#f0fef0', label: 'default' }, { hex: '#67809c', label: 'keyword' }];
  const bg = '#202830';
  const r = floor(bg, fgSet);
  assert.equal(r.limitingHex, '#67809c');
  assert.equal(r.limitingLabel, 'keyword');
  assert.ok(Math.abs(r.ratio - contrast('#67809c', bg)) < 1e-9);
  assert.ok(r.ratio < contrast('#f0fef0', bg), 'the floor is below the default-fg contrast');
});

test('floor: Boundary — a single-entry set makes that entry the limit', () => {
  const r = floor('#000000', [{ hex: '#67809c', label: 'keyword' }]);
  assert.equal(r.limitingHex, '#67809c');
  assert.ok(Math.abs(r.ratio - contrast('#67809c', '#000000')) < 1e-9);
});

test('floor: Error — an empty set returns nulls, not a bogus ratio', () => {
  assert.deepEqual(floor('#000000', []), { ratio: null, limitingHex: null, limitingLabel: null });
});

// --- lMax -------------------------------------------------------------------

const F = (L, chroma, hue, fgSet) => floor(oklch2hex(L, chroma, hue).hex, fgSet).ratio;

test('lMax: Normal — finds the lightest safe background; the floor brackets the target', () => {
  const fgSet = [{ hex: '#f0fef0', label: 'default' }, { hex: '#67809c', label: 'keyword' }];
  const r = lMax(0, 0, fgSet, 4.5);
  assert.equal(r.status, 'ok');
  assert.ok(r.L > 0 && r.L < 1);
  assert.ok(F(r.L, 0, 0, fgSet) >= 4.5 - 0.05, 'floor at L_max clears the target');
  assert.ok(F(Math.min(1, r.L + 0.05), 0, 0, fgSet) < 4.5, 'just above L_max the floor fails');
});

test('lMax: Boundary — no L satisfies the target when a foreground is too dark', () => {
  const r = lMax(0, 0, [{ hex: '#1a1a1a', label: 'dim' }], 4.5);
  assert.equal(r.status, 'none'); // even pure-black background can't lift #1a1a1a to AA
});

test('lMax: Boundary — an empty foreground set is vacuously safe everywhere', () => {
  const r = lMax(0, 0, [], 4.5);
  assert.deepEqual(r, { L: 1, status: 'all' });
});

test('lMax: Boundary — requesting an unreachable chroma at the ceiling reports clamp', () => {
  const fgSet = [{ hex: '#f0fef0', label: 'default' }, { hex: '#67809c', label: 'keyword' }];
  const r = lMax(250, 0.3, fgSet, 4.5); // 0.3 chroma is out of gamut at the dark ceiling
  assert.equal(r.status, 'clamp');
  assert.ok(r.L > 0 && r.L < 1);
});

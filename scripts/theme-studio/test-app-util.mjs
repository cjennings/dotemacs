// Unit tests for the pure color/UI-boundary helpers (app-util.js).
// Run: node --test scripts/theme-studio/

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { normHex, ratingColor, textOn } from './app-util.js';

const here = fileURLToPath(new URL('.', import.meta.url));

test('normHex: Normal — adds the #, lowercases, accepts an existing #', () => {
  assert.equal(normHex('67809C'), '#67809c');
  assert.equal(normHex('#E8BD30'), '#e8bd30');
  assert.equal(normHex('#67809c'), '#67809c');
});

test('normHex: Boundary — trims surrounding whitespace; empty is null', () => {
  assert.equal(normHex('  67809c  '), '#67809c');
  assert.equal(normHex(''), null);
  assert.equal(normHex('   '), null);
  assert.equal(normHex('abc'), null); // 3-digit shorthand is not accepted
});

test('normHex: Error — bad characters and wrong length give null', () => {
  assert.equal(normHex('#gggggg'), null);
  assert.equal(normHex('#12345'), null);   // 5 digits
  assert.equal(normHex('#1234567'), null); // 7 digits
  assert.equal(normHex('red'), null);
});

test('ratingColor: Normal — AAA green, AA grey, fail red', () => {
  assert.equal(ratingColor(10), '#5d9b86');
  assert.equal(ratingColor(5), '#a9b2bb');
  assert.equal(ratingColor(2), '#cb6b4d');
});

test('ratingColor: Boundary — the AAA (7) and AA (4.5) thresholds are inclusive', () => {
  assert.equal(ratingColor(7), '#5d9b86');
  assert.equal(ratingColor(6.99), '#a9b2bb');
  assert.equal(ratingColor(4.5), '#a9b2bb');
  assert.equal(ratingColor(4.49), '#cb6b4d');
});

test('ratingColor: Error — zero and negative ratios are the fail color', () => {
  assert.equal(ratingColor(0), '#cb6b4d');
  assert.equal(ratingColor(-1), '#cb6b4d');
});

test('textOn: Normal — white text on black, black text on white', () => {
  assert.equal(textOn('#000000'), '#fff');
  assert.equal(textOn('#ffffff'), '#000');
});

test('textOn: Boundary — straddles the ~0.179 luminance crossover', () => {
  assert.equal(textOn('#707070'), '#fff'); // just below the crossover
  assert.equal(textOn('#777777'), '#000'); // just above the crossover
});

// Inline-integrity: the page must carry app-util.js's body (sans import/export)
// verbatim — the same strip generate.py applies. Requires `python3 generate.py`.
const stripModule = (s) =>
  s.split('\n').filter((l) => !(l.startsWith('export') || l.startsWith('import')))
    .join('\n').replace(/\s+$/, '');

test('inline-integrity: theme-studio.html contains the app-util.js body verbatim', () => {
  const body = stripModule(readFileSync(here + 'app-util.js', 'utf8'));
  const html = readFileSync(here + 'theme-studio.html', 'utf8');
  assert.ok(html.includes(body), 'generated page is missing the app-util.js body verbatim');
});

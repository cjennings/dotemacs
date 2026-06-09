// Unit tests for the pure app logic (app-core.js): the package-face model and
// the dropdown option list. These are the functions Stage 7 made importable.
// Run: node --test scripts/theme-studio/

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import {
  nameToHex, buildPkgmap, packagesForExport, mergePackagesInto, effResolve, optList, slugify,
} from './app-core.js';

const here = fileURLToPath(new URL('.', import.meta.url));
const PAL = [['#67809c', 'blue'], ['#e8bd30', 'gold']];

test('nameToHex: Normal — resolves a palette name to its hex', () => {
  assert.equal(nameToHex('blue', PAL), '#67809c');
  assert.equal(nameToHex('gold', PAL), '#e8bd30');
});

test('nameToHex: Normal — a raw #hex passes through unchanged', () => {
  assert.equal(nameToHex('#abcdef', PAL), '#abcdef');
});

test('nameToHex: Boundary/Error — null, empty, and unknown names give null', () => {
  assert.equal(nameToHex(null, PAL), null);
  assert.equal(nameToHex('', PAL), null);
  assert.equal(nameToHex(undefined, PAL), null);
  assert.equal(nameToHex('chartreuse', PAL), null);
});

test('optList: Normal — default entry then the whole palette', () => {
  assert.deepEqual(optList('#67809c', PAL), [['', '— default —'], ...PAL]);
});

test('optList: Boundary — empty cur is "have", so no (gone) entry', () => {
  assert.deepEqual(optList('', PAL), [['', '— default —'], ...PAL]);
});

test('optList: Error — a cur not in the palette is surfaced as (gone) first', () => {
  const list = optList('#123456', PAL);
  assert.deepEqual(list[0], ['', '— default —']);
  assert.deepEqual(list[1], ['#123456', '(gone) #123456']);
  assert.deepEqual(list.slice(2), PAL);
});

test('buildPkgmap: Normal — seeds faces, resolving names and applying defaults', () => {
  const apps = { 'org-mode': { faces: [
    ['org-todo', 'todo', { fg: 'blue', bold: true }],
    ['org-done', 'done', { inherit: 'org-todo' }],
  ] } };
  const m = buildPkgmap(apps, PAL);
  assert.equal(m['org-mode']['org-todo'].fg, '#67809c');
  assert.equal(m['org-mode']['org-todo'].bold, true);
  assert.equal(m['org-mode']['org-todo'].source, 'default');
  assert.equal(m['org-mode']['org-todo'].height, 1);
  assert.equal(m['org-mode']['org-done'].inherit, 'org-todo');
  assert.equal(m['org-mode']['org-done'].fg, null);
});

test('buildPkgmap: Boundary — a face with no default dict still seeds blank', () => {
  const m = buildPkgmap({ a: { faces: [['f', 'f']] } }, PAL);
  assert.deepEqual(m.a.f, {
    fg: null, bg: null, bold: false, italic: false, underline: false,
    strike: false, inherit: null, height: 1, box: null, source: 'default',
  });
});

test('effResolve: Normal — a face with a value returns it', () => {
  const m = { a: { f: { fg: '#67809c', inherit: null } } };
  assert.equal(effResolve(m, 'a', 'f', 'fg'), '#67809c');
});

test('effResolve: Normal — follows the inherit chain when unset', () => {
  const m = { a: {
    base: { bg: '#0d0b0a', inherit: null },
    mid: { bg: null, inherit: 'base' },
    leaf: { bg: null, inherit: 'mid' },
  } };
  assert.equal(effResolve(m, 'a', 'leaf', 'bg'), '#0d0b0a');
});

test('effResolve: Boundary — unset with no inherit, or a missing face, gives null', () => {
  const m = { a: { f: { fg: null, inherit: null } } };
  assert.equal(effResolve(m, 'a', 'f', 'fg'), null);
  assert.equal(effResolve(m, 'a', 'nope', 'fg'), null);
});

test('effResolve: Error — an inherit cycle terminates at null, no overflow', () => {
  const m = { a: { x: { fg: null, inherit: 'y' }, y: { fg: null, inherit: 'x' } } };
  assert.equal(effResolve(m, 'a', 'x', 'fg'), null);
});

test('packagesForExport: Normal — exports sourced faces, omits height 1', () => {
  const m = { a: { f: {
    fg: '#67809c', bg: null, bold: true, italic: false, underline: false,
    strike: false, inherit: null, height: 1, source: 'user',
  } } };
  const out = packagesForExport(m);
  assert.equal(out.a.f.fg, '#67809c');
  assert.equal(out.a.f.source, 'user');
  assert.ok(!('height' in out.a.f), 'height 1 is omitted');
});

test('packagesForExport: Boundary — keeps a non-default height', () => {
  const m = { a: { f: { fg: null, bg: null, source: 'user', height: 1.2 } } };
  assert.equal(packagesForExport(m).a.f.height, 1.2);
});

test('packagesForExport: Error — faces with an unknown source are skipped', () => {
  const m = { a: { f: { fg: '#67809c', source: 'system' } } };
  assert.deepEqual(packagesForExport(m), {});
});

test('mergePackagesInto: Normal — fills missing fields with defaults', () => {
  const m = {};
  mergePackagesInto(m, { a: { f: { fg: '#112233' } } });
  assert.deepEqual(m.a.f, {
    fg: '#112233', bg: null, bold: false, italic: false, underline: false,
    strike: false, inherit: null, height: 1, box: null, source: 'user',
  });
});

test('mergePackagesInto: Boundary — undefined pkgs is a no-op', () => {
  const m = { a: { f: { fg: '#000000' } } };
  mergePackagesInto(m, undefined);
  assert.deepEqual(m, { a: { f: { fg: '#000000' } } });
});

test('slugify: Normal — spaces and punctuation collapse to single dashes', () => {
  assert.equal(slugify('My Cool Theme'), 'My-Cool-Theme');
  assert.equal(slugify('dupre revised'), 'dupre-revised');
  assert.equal(slugify('keeps.dots_and-dashes'), 'keeps.dots_and-dashes');
});

test('slugify: Boundary — leading/trailing junk is trimmed', () => {
  assert.equal(slugify('  spaced  '), 'spaced');
  assert.equal(slugify('!!!edges!!!'), 'edges');
  assert.equal(slugify(''), 'theme'); // empty falls back
});

test('slugify: Error — an all-disallowed name falls back to "theme"', () => {
  assert.equal(slugify('!!!'), 'theme');
  assert.equal(slugify('   '), 'theme');
});

// Guards the one-source-of-truth contract, same as the colormath integrity test:
// the page must carry app-core.js's body (sans exports) verbatim. Requires
// `python3 generate.py` to have run first.
const stripExports = (s) =>
  s.split('\n').filter((l) => !(l.startsWith('export') || l.startsWith('import'))).join('\n').replace(/\s+$/, '');

test('inline-integrity: theme-studio.html contains the app-core.js body verbatim', () => {
  const body = stripExports(readFileSync(here + 'app-core.js', 'utf8'));
  const html = readFileSync(here + 'theme-studio.html', 'utf8');
  assert.ok(html.includes(body), 'generated page is missing the app-core.js body verbatim');
});

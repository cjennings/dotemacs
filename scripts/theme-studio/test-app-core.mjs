// Unit tests for the pure app logic (app-core.js): the package-face model and
// the dropdown option list. These are the functions Stage 7 made importable.
// Run: node --test scripts/theme-studio/

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import {
  nameToHex, normalizePkgFace, buildPkgmap, packagesForExport, mergePackagesInto, effResolve, optList, paletteOptionList, spanNeighborHex, slugify,
  clearPalettePlan, deletePaletteColumnPlan, groundColumnMembersFromPalette, areAllLocked, lockToggleLabel, toggleLockSet,
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

test('paletteOptionList: Normal — color choices follow visual column ordering', () => {
  const pal = [
    ['#67809c', 'blue'],
    ['#0d0b0a', 'bg'],
    ['#808080', 'gray'],
    ['#c0402a', 'red'],
    ['#f0fef0', 'fg'],
  ];
  const list = paletteOptionList('#67809c', pal, { bg: '#0d0b0a', fg: '#f0fef0' });
  assert.deepEqual(list.slice(0, 3), [['', '— default —'], ['#0d0b0a', 'bg'], ['#f0fef0', 'fg']]);
  assert.ok(list.findIndex(([, name]) => name === 'blue') < list.findIndex(([, name]) => name === 'gray'), 'palette column order is preserved');
  assert.ok(list.findIndex(([, name]) => name === 'gray') < list.findIndex(([, name]) => name === 'red'), 'later columns stay later');
});

test('paletteOptionList: Normal — colors within each column are lightest to darkest', () => {
  const pal = [
    ['#111111', 'bg', 'ground'],
    ['#eeeeee', 'fg', 'ground'],
    ['#444444', 'gray-dark', 'gray'],
    ['#cccccc', 'gray-light', 'gray'],
    ['#888888', 'gray-mid', 'gray'],
    ['#330000', 'red-dark', 'red'],
    ['#dd8888', 'red-light', 'red'],
  ];
  const list = paletteOptionList('', pal, { bg: '#111111', fg: '#eeeeee' });
  assert.deepEqual(list.slice(0, 3).map(([, name]) => name), ['— default —', 'bg', 'fg']);
  assert.deepEqual(
    list.filter(([, name]) => name.startsWith('gray')).map(([, name]) => name),
    ['gray-light', 'gray-mid', 'gray-dark'],
  );
  assert.ok(list.findIndex(([, name]) => name === 'gray-dark') < list.findIndex(([, name]) => name === 'red-light'), 'column order is still left-to-right');
  assert.deepEqual(
    list.filter(([, name]) => name.startsWith('red')).map(([, name]) => name),
    ['red-light', 'red-dark'],
  );
});

test('paletteOptionList: Boundary — assignment-only ground colors are selectable', () => {
  const list = paletteOptionList('', [['#67809c', 'blue']], { bg: '#0d0b0a', fg: '#f0fef0' });
  assert.ok(list.some(([hex, name]) => hex === '#0d0b0a' && name === 'bg'));
  assert.ok(list.some(([hex, name]) => hex === '#f0fef0' && name === 'fg'));
});

test('paletteOptionList: Boundary — bg-like imported colors remain selectable outside ground', () => {
  const pal = [['#0d0b0a', 'bg2'], ['#0d0b0a', 'bg', 'ground'], ['#f0fef0', 'fg', 'ground']];
  const list = paletteOptionList('', pal, { bg: '#0d0b0a', fg: '#f0fef0' });
  assert.deepEqual(list.slice(0, 4), [['', '— default —'], ['#0d0b0a', 'bg'], ['#f0fef0', 'fg'], ['#0d0b0a', 'bg2']]);
});

test('paletteOptionList: Error — a cur outside palette and ground is surfaced as gone', () => {
  const list = paletteOptionList('#123456', PAL, { bg: '#0d0b0a', fg: '#f0fef0' });
  assert.deepEqual(list[0], ['', '— default —']);
  assert.deepEqual(list[1], ['#123456', '(gone) #123456']);
});

test('spanNeighborHex: Normal — steps lighter and darker within the current column', () => {
  const pal = [
    ['#222222', 'gray-dark', 'gray'],
    ['#888888', 'gray-mid', 'gray'],
    ['#dddddd', 'gray-light', 'gray'],
    ['#330000', 'red-dark', 'red'],
  ];
  const ground = { bg: '#000000', fg: '#ffffff' };
  assert.equal(spanNeighborHex('#888888', pal, ground, 1), '#dddddd');
  assert.equal(spanNeighborHex('#888888', pal, ground, -1), '#222222');
  assert.equal(spanNeighborHex('#dddddd', pal, ground, 1), null);
  assert.equal(spanNeighborHex('#222222', pal, ground, -1), null);
});

test('spanNeighborHex: Normal — ground steps by lightness too', () => {
  const pal = [
    ['#ffffff', 'bg', 'ground'],
    ['#777777', 'ground+1', 'ground'],
    ['#000000', 'fg', 'ground'],
  ];
  const ground = { bg: '#ffffff', fg: '#000000' };
  assert.equal(spanNeighborHex('#777777', pal, ground, 1), '#ffffff');
  assert.equal(spanNeighborHex('#777777', pal, ground, -1), '#000000');
});

test('spanNeighborHex: Boundary — default and gone colors cannot step', () => {
  assert.equal(spanNeighborHex('', PAL, { bg: '#000000', fg: '#ffffff' }, 1), null);
  assert.equal(spanNeighborHex('#123456', PAL, { bg: '#000000', fg: '#ffffff' }, 1), null);
});

test('clearPalettePlan: Normal — removes non-ground colors and records recoverable names', () => {
  const plan = clearPalettePlan([
    ['#0d0b0a', 'bg', 'ground'],
    ['#f0fef0', 'fg', 'ground'],
    ['#67809c', 'blue', 'blue'],
    ['#92acc2', 'blue+1', 'blue'],
  ], { bg: '#0d0b0a', fg: '#f0fef0' });
  assert.deepEqual(plan.palette, [['#0d0b0a', 'bg', 'ground'], ['#f0fef0', 'fg', 'ground']]);
  assert.deepEqual(plan.removed, [{ hex: '#67809c', name: 'blue' }, { hex: '#92acc2', name: 'blue+1' }]);
});

test('clearPalettePlan: Boundary — synthesizes missing bg and fg endpoints', () => {
  const plan = clearPalettePlan([['#67809c', 'blue', 'blue']], { bg: '#000000', fg: '#ffffff' });
  assert.deepEqual(plan.palette, [['#000000', 'bg', 'ground'], ['#ffffff', 'fg', 'ground']]);
  assert.deepEqual(plan.removed, [{ hex: '#67809c', name: 'blue' }]);
});

test('clearPalettePlan: Boundary — same-hex imported colors are not ground endpoints', () => {
  const plan = clearPalettePlan([
    ['#0d0b0a', 'bg2', 'bg2'],
    ['#0d0b0a', 'bg', 'ground'],
    ['#f0fef0', 'fg', 'ground'],
  ], { bg: '#0d0b0a', fg: '#f0fef0' });
  assert.deepEqual(plan.palette, [['#0d0b0a', 'bg', 'ground'], ['#f0fef0', 'fg', 'ground']]);
  assert.deepEqual(plan.removed, [{ hex: '#0d0b0a', name: 'bg2' }]);
});

test('deletePaletteColumnPlan: Normal — removes one stable column and keeps ground plus neighbors', () => {
  const plan = deletePaletteColumnPlan([
    ['#0d0b0a', 'bg', 'ground'],
    ['#f0fef0', 'fg', 'ground'],
    ['#c0402a', 'red', 'red'],
    ['#3a6ea5', 'blue', 'blue'],
    ['#92acc2', 'blue+1', 'blue'],
    ['#808080', 'gray', 'gray'],
  ], { bg: '#0d0b0a', fg: '#f0fef0' }, 'blue');
  assert.deepEqual(plan.palette.map(p => p[1]), ['bg', 'fg', 'red', 'gray']);
  assert.deepEqual(plan.removed, [{ hex: '#3a6ea5', name: 'blue' }, { hex: '#92acc2', name: 'blue+1' }]);
});

test('deletePaletteColumnPlan: Boundary — never deletes ground entries', () => {
  const plan = deletePaletteColumnPlan([
    ['#0d0b0a', 'bg', 'ground'],
    ['#555555', 'ground+1', 'ground'],
    ['#f0fef0', 'fg', 'ground'],
  ], { bg: '#0d0b0a', fg: '#f0fef0' }, 'ground');
  assert.deepEqual(plan.palette.map(p => p[1]), ['bg', 'ground+1', 'fg']);
  assert.deepEqual(plan.removed, []);
});

test('groundColumnMembersFromPalette: Normal — sorts bg, ground+N steps, then fg', () => {
  const members = groundColumnMembersFromPalette([
    ['#ffffff', 'bg', 'ground'],
    ['#333333', 'ground+2', 'ground'],
    ['#bbbbbb', 'ground+1', 'ground'],
    ['#000000', 'fg', 'ground'],
  ], { bg: '#ffffff', fg: '#000000' });
  assert.deepEqual(members.map(m => m.name), ['bg', 'ground+1', 'ground+2', 'fg']);
});

test('lock helpers: Normal — label and toggle operate on the full key set', () => {
  const keys = ['a', 'b', 'c'];
  assert.equal(areAllLocked(keys, new Set(['a', 'b'])), false);
  assert.equal(lockToggleLabel(keys, new Set(['a', 'b'])), 'lock all');
  const locked = toggleLockSet(keys, new Set(['a']));
  assert.deepEqual([...locked].sort(), keys);
  assert.equal(lockToggleLabel(keys, locked), 'unlock all');
  assert.deepEqual([...toggleLockSet(keys, locked)].sort(), []);
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

test('normalizePkgFace: Normal — fills every package face field', () => {
  assert.deepEqual(normalizePkgFace({ fg: 'blue', bold: true, inherit: 'base' }, 'default', PAL), {
    fg: '#67809c', bg: null, bold: true, italic: false, underline: false,
    strike: false, inherit: 'base', height: 1, box: null, source: 'default',
  });
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

test('inline-integrity: theme-studio.html contains palette-actions.js verbatim', () => {
  const body = stripExports(readFileSync(here + 'palette-actions.js', 'utf8'));
  const html = readFileSync(here + 'theme-studio.html', 'utf8');
  assert.ok(html.includes(body), 'generated page is missing palette-actions.js verbatim');
});

test('inline-integrity: theme-studio.html contains browser-gates.js verbatim', () => {
  const body = stripExports(readFileSync(here + 'browser-gates.js', 'utf8'));
  const html = readFileSync(here + 'theme-studio.html', 'utf8');
  assert.ok(html.includes(body), 'generated page is missing browser-gates.js verbatim');
});

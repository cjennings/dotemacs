// Unit tests for the pure app logic (app-core.js): the package-face model and
// the dropdown option list. These are the functions Stage 7 made importable.
// Run: node --test scripts/theme-studio/

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import {
  nameToHex, migrateLegacyFace, normalizePkgFace, buildPkgmap, packagesForExport, mergePackagesInto, effResolve, resolveSyntaxFg, resolveUiAttr, dropdownRowTextColor, paletteOptionList, spanNeighborHex, slugify,
  clearPalettePlan, deletePaletteColumnPlan, groundColumnMembersFromPalette, areAllLocked, lockToggleLabel, toggleLockSet,
  galleryModel, appViewKeysSorted, faceBoxNonDefaults, stepViewIndex,
} from './app-core.js';
import { planPaletteGenerator, entriesForGeneratedColumn } from './palette-generator-core.js';
import { oklch2hex, deltaE } from './colormath.js';

const here = fileURLToPath(new URL('.', import.meta.url));
const PAL = [['#67809c', 'blue'], ['#e8bd30', 'gold']];
const COLOR_NAMES = JSON.parse(readFileSync(new URL('./color-names.json', import.meta.url), 'utf8'));

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

const GALLERY_PAL = [
  ['#111111', 'bg', 'ground'],
  ['#eeeeee', 'fg', 'ground'],
  ['#444444', 'gray-dark', 'gray'],
  ['#cccccc', 'gray-light', 'gray'],
  ['#888888', 'gray-mid', 'gray'],
  ['#330000', 'red-dark', 'red'],
  ['#dd8888', 'red-light', 'red'],
];
const GALLERY_GROUND = { bg: '#111111', fg: '#eeeeee' };
const allCells = m => m.rows.flatMap(r => r.cells);

test('galleryModel: Normal — ground row then one row per family, default cell present', () => {
  const m = galleryModel('#888888', GALLERY_PAL, GALLERY_GROUND);
  assert.equal(m.default.hex, '');
  assert.equal(m.gone, null);
  assert.equal(m.rows[0].kind, 'ground');
  assert.deepEqual(m.rows[0].cells.map(c => c.hex), ['#111111', '#eeeeee']);
  const cols = m.rows.filter(r => r.kind === 'column');
  assert.equal(cols.length, 2, 'one row per color family');
  assert.deepEqual(
    cols.find(r => r.column === 'gray').cells.map(c => c.hex),
    ['#444444', '#888888', '#cccccc'],
    'family members run dark to light',
  );
});

test('galleryModel: Normal — exactly the current color is selected', () => {
  const m = galleryModel('#888888', GALLERY_PAL, GALLERY_GROUND);
  const selected = allCells(m).filter(c => c.selected);
  assert.deepEqual(selected.map(c => c.hex), ['#888888']);
  assert.equal(m.default.selected, false);
});

test('galleryModel: Boundary — empty cur selects the default cell, nothing in the grid', () => {
  const m = galleryModel('', GALLERY_PAL, GALLERY_GROUND);
  assert.equal(m.default.selected, true);
  assert.equal(m.gone, null);
  assert.equal(allCells(m).filter(c => c.selected).length, 0);
});

test('galleryModel: Error — a cur outside the palette surfaces a selected (gone) cell', () => {
  const m = galleryModel('#abcdef', GALLERY_PAL, GALLERY_GROUND);
  assert.ok(m.gone, 'gone cell exists');
  assert.equal(m.gone.hex, '#abcdef');
  assert.equal(m.gone.name, '(gone)');
  assert.equal(m.gone.selected, true);
  assert.equal(allCells(m).filter(c => c.selected).length, 0, 'no grid cell claims the gone color');
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
  assert.deepEqual(list[1], ['#123456', '(gone)']);
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

test('planPaletteGenerator: Normal — builds deterministic preview columns without mutating palette', () => {
  const pal = [['#0d0b0a', 'bg', 'ground'], ['#f0fef0', 'fg', 'ground'], ['#67809c', 'blue', 'blue']];
  const before = JSON.stringify(pal);
  const plan = planPaletteGenerator(pal, { bg: '#0d0b0a', fg: '#f0fef0' }, {
    sourceMode: 'bg-fg',
    scheme: 'syntax-balanced',
    baseHue: 250,
    accentCount: 5,
    spanCount: 2,
    chromaMode: 'balanced',
    contrastMode: 'aa',
  });
  assert.equal(JSON.stringify(pal), before, 'planner is pure');
  assert.equal(plan.sourceMode, 'bg-fg');
  assert.equal(plan.scheme, 'syntax-balanced');
  assert.equal(plan.columns.length, 5);
  assert.equal(plan.summary.generated, 5);
  assert.equal(plan.summary.rejected, 0);
  assert.ok(plan.summary.minContrast >= 4.5);
  assert.ok(!/^generated-/.test(plan.columns[0].name), 'generated bases get nearest color names');
  assert.equal(plan.columns[0].members.length, 1);
  assert.ok(plan.columns.every(c => c.columnId && c.members.some(m => m.offset === 0 && m.hex === c.baseHex)));
});

test('planPaletteGenerator: Normal — random scheme varies candidate bases for inspiration', () => {
  const pal = [['#0d0b0a', 'bg', 'ground'], ['#f0fef0', 'fg', 'ground']];
  const seq = [0.10, 0.80, 0.35, 0.65, 0.90, 0.20, 0.48, 0.72, 0.04, 0.55, 0.30, 0.88];
  let at = 0;
  const rng = () => seq[at++ % seq.length];
  const a = planPaletteGenerator(pal, { bg: '#0d0b0a', fg: '#f0fef0' }, { scheme: 'random', accentCount: 4, spanCount: 0, rng });
  const b = planPaletteGenerator(pal, { bg: '#0d0b0a', fg: '#f0fef0' }, { scheme: 'random', accentCount: 4, spanCount: 0, rng });
  assert.equal(a.scheme, 'random');
  assert.equal(a.columns.length, 4);
  assert.notDeepEqual(a.columns.map(c => c.baseHex), b.columns.map(c => c.baseHex));
  assert.ok(a.columns.every(c => c.contrast >= 4.5));
  assert.ok(a.columns.some(c => c.C >= 0.11), 'random candidates include more saturated colors than the quiet default');
});

test('planPaletteGenerator: Boundary — omitted scheme defaults to random', () => {
  const plan = planPaletteGenerator([], { bg: '#000000', fg: '#ffffff' }, { accentCount: 1, spanCount: 0, rng: () => 0.25 });
  assert.equal(plan.scheme, 'random');
});

test('planPaletteGenerator: Boundary — omitted accent count defaults to five', () => {
  const plan = planPaletteGenerator([], { bg: '#000000', fg: '#ffffff' }, { spanCount: 0, rng: () => 0.25 });
  assert.equal(plan.accentCount, 5);
  assert.equal(plan.columns.length, 5);
});

test('planPaletteGenerator: Boundary — accent count is clamped to the supported range', () => {
  const low = planPaletteGenerator([], { bg: '#000000', fg: '#ffffff' }, { accentCount: -4, spanCount: 0, rng: () => 0.25 });
  const high = planPaletteGenerator([], { bg: '#000000', fg: '#ffffff' }, { accentCount: 99, spanCount: 0, rng: () => 0.25 });
  assert.equal(low.accentCount, 1);
  assert.equal(low.columns.length, 1);
  assert.equal(high.accentCount, 12);
  assert.equal(high.columns.length, 12);
});

test('planPaletteGenerator: Boundary — unknown source mode falls back to bg/fg', () => {
  const plan = planPaletteGenerator([], { bg: '#000000', fg: '#ffffff' }, {
    sourceMode: 'mystery',
    baseHue: 17,
    accentCount: 1,
    spanCount: 0,
  });
  assert.equal(plan.sourceMode, 'bg-fg');
  assert.notEqual(Math.round(plan.baseHue), 17);
});

test('planPaletteGenerator: Normal — intent near palette uses base columns as anchors', () => {
  const pal = [
    ['#0d0b0a', 'bg', 'ground'],
    ['#f0fef0', 'fg', 'ground'],
    ['#67809c', 'blue', 'blue'],
    ['#e8bd30', 'gold', 'gold'],
    ['#ffffff', 'blue+1', 'blue'],
  ];
  const plan = planPaletteGenerator(pal, { bg: '#0d0b0a', fg: '#f0fef0' }, {
    intent: 'near-palette',
    sourceMode: 'palette',
    vibe: 'balanced',
    accentCount: 4,
    spanCount: 0,
    rng: () => 0.5,
  });
  const anchorHues = ['#67809c', '#e8bd30'].map(hex => Math.round(planPaletteGenerator([], { bg: '#000000', fg: '#ffffff' }, { sourceMode: 'selected', selectedHex: hex, accentCount: 1, spanCount: 0, rng: () => 0.1 }).baseHue));
  assert.equal(plan.intent, 'near-palette');
  assert.equal(plan.sourceMode, 'palette');
  assert.equal(plan.columns.length, 4);
  assert.ok(plan.columns.every(c => anchorHues.some(h => Math.abs((((c.hue - h + 540) % 360) - 180)) <= 20)));
  assert.ok(plan.columns.every(c => c.C >= 0.075 && c.C <= 0.13));
});

test('planPaletteGenerator: Normal — intent fill gaps targets underused hue regions', () => {
  const pal = [['#67809c', 'blue', 'blue'], ['#e8bd30', 'gold', 'gold'], ['#cb6b4d', 'terra', 'terra']];
  const plan = planPaletteGenerator(pal, { bg: '#000000', fg: '#ffffff' }, {
    intent: 'fill-gaps',
    sourceMode: 'palette',
    vibe: 'muted',
    accentCount: 3,
    spanCount: 0,
    rng: () => 0.5,
  });
  assert.equal(plan.intent, 'fill-gaps');
  assert.equal(plan.columns.length, 3);
  assert.ok(plan.columns.every(c => c.C <= 0.09), 'muted vibe keeps chroma lower');
  assert.ok(new Set(plan.columns.map(c => Math.round(c.hue))).size > 1, 'gap fill proposes multiple hue regions');
});

test('planPaletteGenerator: Normal — fill gaps chooses missing perceptual colors', () => {
  const anchor = (L,C,H) => oklch2hex(L, C, H).hex;
  const pal = [
    [anchor(0.25, 0.08, 40), 'dark-warm', 'dark-warm'],
    [anchor(0.75, 0.08, 40), 'light-warm', 'light-warm'],
    [anchor(0.50, 0.08, 220), 'mid-cool', 'mid-cool'],
  ];
  const plan = planPaletteGenerator(pal, { bg: '#000000', fg: '#ffffff' }, {
    intent: 'fill-gaps',
    sourceMode: 'palette',
    vibe: 'balanced',
    contrastMode: 'none',
    accentCount: 5,
    spanCount: 0,
    rng: () => 0.5,
  });
  const Ls = plan.columns.map(c => c.L), nearestAnchor = plan.columns.map(c => Math.min(...pal.map(p => deltaE(c.baseHex, p[0]))));
  assert.equal(plan.columns.length, 5);
  assert.ok(Math.max(...Ls) - Math.min(...Ls) > 0.35, 'candidate spread includes lightness, not hue alone');
  assert.ok(nearestAnchor.every(d => d > 0.14), 'candidates stay perceptually away from existing anchors: '+nearestAnchor.join(','));
});

test('planPaletteGenerator: Boundary — fill gaps with no anchors falls back to random-style candidates', () => {
  const seq = [0.10, 0.20, 0.30, 0.40, 0.50, 0.60];
  let at = 0;
  const plan = planPaletteGenerator([], { bg: '#000000', fg: '#ffffff' }, {
    intent: 'fill-gaps',
    sourceMode: 'none',
    vibe: 'balanced',
    accentCount: 3,
    spanCount: 0,
    rng: () => seq[at++ % seq.length],
  });
  assert.equal(plan.columns.length, 3);
  assert.ok(new Set(plan.columns.map(c => Math.round(c.hue))).size > 1);
});

test('planPaletteGenerator: Normal — fill hue gaps rewards underrepresented hue regions', () => {
  const ground = { bg: '#000000', fg: '#ffffff' };
  const baseCfg = { sourceMode: 'bg-fg', vibe: 'balanced', contrastMode: 'aa', accentCount: 5, spanCount: 0, rng: () => 0.5 };
  const plain = planPaletteGenerator([], ground, { ...baseCfg, intent: 'fill-gaps' });
  const hueAware = planPaletteGenerator([], ground, { ...baseCfg, intent: 'fill-hue-gaps' });
  const yellowish = c => c.hue >= 45 && c.hue <= 105;
  assert.equal(hueAware.intent, 'fill-hue-gaps');
  assert.notDeepEqual(hueAware.columns.map(c => Math.round(c.hue)), plain.columns.map(c => Math.round(c.hue)));
  assert.ok(hueAware.columns.some(yellowish), 'hue-aware fill should surface a yellow/gold region early');
  assert.ok(!plain.columns.some(yellowish), 'plain perceptual fill stays available as a separate behavior');
});

test('planPaletteGenerator: Normal — intent complements generates opposite anchor colors', () => {
  const plan = planPaletteGenerator([['#67809c', 'blue', 'blue']], { bg: '#000000', fg: '#ffffff' }, {
    intent: 'complements',
    sourceMode: 'palette',
    vibe: 'bold',
    accentCount: 2,
    spanCount: 0,
    rng: () => 0.5,
  });
  assert.equal(plan.intent, 'complements');
  assert.ok(plan.columns.every(c => c.C >= 0.12));
  assert.ok(plan.columns.every(c => Math.abs((((c.hue - (plan.baseHue + 180) + 540) % 360) - 180)) <= 20));
});

test('planPaletteGenerator: Normal — intent bridges generates colors between anchors', () => {
  const pal = [['#67809c', 'blue', 'blue'], ['#e8bd30', 'gold', 'gold'], ['#cb6b4d', 'terra', 'terra']];
  const plan = planPaletteGenerator(pal, { bg: '#000000', fg: '#ffffff' }, {
    intent: 'bridges',
    sourceMode: 'palette',
    vibe: 'balanced',
    accentCount: 3,
    spanCount: 0,
    rng: () => 0.5,
  });
  assert.equal(plan.intent, 'bridges');
  assert.equal(plan.columns.length, 3);
  assert.ok(new Set(plan.columns.map(c => Math.round(c.hue))).size > 1);
  assert.ok(plan.columns.every(c => c.C >= 0.075 && c.C <= 0.13));
});

test('planPaletteGenerator: Normal — harmony intents generate expected hue families', () => {
  const planFor = intent => planPaletteGenerator([], { bg: '#000000', fg: '#ffffff' }, {
    intent,
    sourceMode: 'selected',
    selectedHex: '#67809c',
    baseHue: 250,
    vibe: 'balanced',
    accentCount: 4,
    spanCount: 0,
    rng: () => 0.5,
  });
  const cfg = intent => planFor(intent).columns.map(c => Math.round(c.hue));
  const base = Math.round(planFor('triadic').baseHue);
  const dist = (a, b) => Math.abs((((a - b + 540) % 360) - 180));
  assert.ok(cfg('complementary').every(h => dist(h, (base + 180) % 360) <= 1));
  assert.deepEqual(cfg('triadic').slice(0, 3).map(h => Math.round((h - base + 360) % 360)), [0, 120, 240]);
  assert.deepEqual(cfg('square').map(h => Math.round((h - base + 360) % 360)), [0, 90, 180, 270]);
  assert.deepEqual(cfg('tetradic').map(h => Math.round((h - base + 360) % 360)), [0, 60, 180, 240]);
  assert.ok(new Set(cfg('rainbow')).size === 4);
  assert.ok(cfg('monochromatic').every(h => dist(h, base) <= 3));
  assert.ok(cfg('split-complementary').slice(0, 2).every(h => dist(h, (base + 150) % 360) <= 1 || dist(h, (base + 210) % 360) <= 1));
  assert.ok(cfg('analogous').slice(0, 2).every(h => dist(h, (base + 330) % 360) <= 1 || dist(h, (base + 30) % 360) <= 1));
});

test('planPaletteGenerator: Normal — harmony intents work with palette anchors', () => {
  const pal = [['#67809c', 'blue', 'blue'], ['#e8bd30', 'gold', 'gold']];
  const intents = ['complementary', 'analogous', 'split-complementary', 'triadic', 'tetradic', 'square', 'monochromatic', 'rainbow'];
  for (const intent of intents) {
    const plan = planPaletteGenerator(pal, { bg: '#000000', fg: '#ffffff' }, {
      intent,
      sourceMode: 'palette',
      vibe: 'balanced',
      accentCount: 4,
      spanCount: 0,
      rng: () => 0.5,
    });
    assert.equal(plan.sourceMode, 'palette', intent);
    assert.equal(plan.intent, intent);
    assert.equal(plan.columns.length, 4, intent);
    assert.ok(plan.columns.every(c => c.contrast >= 4.5), intent);
  }
});

test('planPaletteGenerator: Boundary — empty palette source falls back to configured hue', () => {
  const plan = planPaletteGenerator([], { bg: '#000000', fg: '#ffffff' }, {
    intent: 'near-palette',
    sourceMode: 'palette',
    baseHue: 123,
    vibe: 'balanced',
    accentCount: 2,
    spanCount: 0,
    rng: () => 0.5,
  });
  assert.equal(plan.sourceMode, 'palette');
  assert.equal(plan.columns.length, 2);
  assert.ok(Math.abs(plan.baseHue - 123) < 0.001);
  assert.ok(plan.columns.every(c => Math.abs((((c.hue - 123 + 540) % 360) - 180)) <= 20));
});

test('planPaletteGenerator: Boundary — invalid selected source falls back to bg/fg', () => {
  const plan = planPaletteGenerator([], { bg: '#000000', fg: '#ffffff' }, {
    intent: 'near-selected',
    sourceMode: 'selected',
    selectedHex: 'not-a-color',
    baseHue: 22,
    accentCount: 1,
    spanCount: 0,
  });
  assert.equal(plan.sourceMode, 'bg-fg');
  assert.notEqual(Math.round(plan.baseHue), 22);
});

test('planPaletteGenerator: Boundary — high contrast can reject every candidate', () => {
  const plan = planPaletteGenerator([], { bg: '#777777', fg: '#ffffff' }, {
    intent: 'random',
    sourceMode: 'none',
    vibe: 'pastel',
    contrastMode: 'aaa',
    accentCount: 4,
    spanCount: 0,
    rng: () => 0.5,
  });
  assert.equal(plan.columns.length, 0);
  assert.equal(plan.rejected.length, 4);
  assert.equal(plan.summary.rejected, 4);
  assert.equal(plan.summary.minContrast, null);
});

test('planPaletteGenerator: Boundary — contrast none keeps otherwise rejected candidates', () => {
  const plan = planPaletteGenerator([], { bg: '#777777', fg: '#ffffff' }, {
    intent: 'random',
    sourceMode: 'none',
    vibe: 'pastel',
    contrastMode: 'none',
    accentCount: 4,
    spanCount: 0,
    rng: () => 0.5,
  });
  assert.equal(plan.columns.length, 4);
  assert.equal(plan.rejected.length, 0);
  assert.ok(plan.summary.minContrast < 7);
});

test('planPaletteGenerator: Normal — warm and cool vibes bias candidate hue families', () => {
  const warm = planPaletteGenerator([], { bg: '#000000', fg: '#ffffff' }, { intent: 'random', vibe: 'warm', accentCount: 4, spanCount: 0, rng: () => 0.25 });
  const cool = planPaletteGenerator([], { bg: '#000000', fg: '#ffffff' }, { intent: 'random', vibe: 'cool', accentCount: 4, spanCount: 0, rng: () => 0.25 });
  assert.ok(warm.columns.every(c => c.hue < 90 || c.hue > 340), 'warm stays in red/orange/yellow hue families');
  assert.ok(cool.columns.every(c => c.hue > 140 && c.hue < 310), 'cool stays in green/cyan/blue/violet hue families');
});

test('planPaletteGenerator: Normal — added vibes produce distinct chroma ranges', () => {
  const mk = vibe => planPaletteGenerator([], { bg: '#000000', fg: '#ffffff' }, { intent: 'random', vibe, accentCount: 1, spanCount: 0, rng: () => 0.5 }).columns[0].C;
  assert.ok(mk('pastel') < mk('deep'));
  assert.ok(mk('jewel') > mk('balanced'));
  assert.ok(mk('neon') > mk('jewel'));
  assert.ok(mk('strange') > mk('bold'));
});

test('planPaletteGenerator: Boundary — selected source uses the selected color hue', () => {
  const plan = planPaletteGenerator([['#67809c', 'blue', 'blue']], { bg: '#101010', fg: '#f0f0f0' }, {
    sourceMode: 'selected',
    selectedHex: '#e8bd30',
    accentCount: 3,
    spanCount: 0,
  });
  assert.equal(plan.sourceMode, 'selected');
  assert.equal(plan.columns.length, 3);
  assert.equal(plan.columns[0].members.length, 1);
  assert.ok(Math.abs(plan.baseHue - 91) < 8, 'gold-ish selected color drives the hue base');
});

test('planPaletteGenerator: Boundary — generated names avoid palette collisions', () => {
  const plan = planPaletteGenerator([['#111111', 'steel-blue', 'steel-blue']], { bg: '#000000', fg: '#ffffff' }, {
    sourceMode: 'selected',
    selectedHex: '#67809c',
    accentCount: 2,
    spanCount: 0,
    rng: () => 0.5,
    colorNames: COLOR_NAMES,
  });
  assert.ok(plan.columns.every(c => !/^generated/.test(c.name)), 'generated candidates use nearest color names');
  assert.equal(new Set(plan.columns.map(c => c.name)).size, 2, 'nearest names are uniqued');
  assert.ok(plan.columns.every(c => !/[+-]\d+$/.test(c.name)), 'unique generated base names do not look like span offsets');
});

test('color name table: Normal — uses filtered X11/CSS-style names', () => {
  assert.ok(COLOR_NAMES.length > 100);
  assert.ok(COLOR_NAMES.some(([name]) => name === 'steel-blue'));
  assert.ok(COLOR_NAMES.some(([name]) => name === 'dark-olive'));
  assert.ok(!COLOR_NAMES.some(([name]) => name === 'dark-olive-green'));
  assert.ok(COLOR_NAMES.some(([name]) => name === 'medium-aquamarine'));
  assert.ok(COLOR_NAMES.some(([name]) => name === 'medium-turquoise'));
  assert.ok(COLOR_NAMES.every(([name]) => !/\d+$/.test(name)), 'numbered variants are excluded');
  assert.ok(COLOR_NAMES.every(([, hex]) => /^#[0-9a-f]{6}$/.test(hex)), 'colors are normalized hex');
});

test('planPaletteGenerator: Boundary — missing neutral source falls back to configured hue', () => {
  const plan = planPaletteGenerator([], {}, { baseHue: 42, accentCount: 1, spanCount: 0 });
  assert.ok(Math.abs(plan.baseHue - 42) < 0.001);
  assert.equal(plan.columns.length, 1);
});

test('planPaletteGenerator: Normal — analogous and triadic schemes choose distinct hue layouts', () => {
  const base = { baseHue: 30, accentCount: 3, spanCount: 0 };
  const analogous = planPaletteGenerator([], { bg: '#000000', fg: '#ffffff' }, { ...base, scheme: 'analogous' });
  const triadic = planPaletteGenerator([], { bg: '#000000', fg: '#ffffff' }, { ...base, scheme: 'triadic' });
  assert.notDeepEqual(
    analogous.columns.map(c => Math.round(c.hue)),
    triadic.columns.map(c => Math.round(c.hue)),
  );
  assert.ok(Math.abs(triadic.columns[1].hue - ((triadic.columns[0].hue + 120) % 360)) < 1);
});

test('entriesForGeneratedColumn: Normal — converts one preview column to stable palette entries', () => {
  const plan = planPaletteGenerator([], { bg: '#000000', fg: '#ffffff' }, { accentCount: 1, spanCount: 1 });
  const entries = entriesForGeneratedColumn(plan.columns[0]);
  assert.equal(entries.length, 1);
  assert.ok(entries.every(e => e[2] === plan.columns[0].columnId));
  assert.deepEqual(entries.map(e => e[1]), [plan.columns[0].name]);
});

test('entriesForGeneratedColumn: Boundary — empty and partial columns are safe', () => {
  assert.deepEqual(entriesForGeneratedColumn(null), []);
  assert.deepEqual(entriesForGeneratedColumn({ name: 'candidate', members: [{ hex: '#123456', name: 'candidate' }] }), [['#123456', 'candidate', 'candidate']]);
  assert.deepEqual(entriesForGeneratedColumn({ members: [{ hex: '#abcdef', name: 'unnamed' }] }), [['#abcdef', 'unnamed', 'generated']]);
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
  assert.equal(m['org-mode']['org-todo'].weight, 'bold'); // legacy bold migrated on seed
  assert.equal(m['org-mode']['org-todo'].source, 'default');
  assert.equal(m['org-mode']['org-todo'].height, 1);
  assert.equal(m['org-mode']['org-done'].inherit, 'org-todo');
  assert.equal(m['org-mode']['org-done'].fg, null);
});

test('normalizePkgFace: Normal — fills every package face field', () => {
  assert.deepEqual(normalizePkgFace({ fg: 'blue', bold: true, inherit: 'base' }, 'default', PAL), {
    fg: '#67809c', bg: null, 'distant-fg': null, family: null, weight: 'bold',
    slant: null, underline: null, strike: null, overline: null,
    inherit: 'base', height: 1, box: null, inverse: false, extend: false,
    source: 'default',
  });
});

test('migrateLegacyFace: Normal — legacy booleans become the new shape', () => {
  assert.deepEqual(
    migrateLegacyFace({ bold: true, italic: true, underline: true, strike: true }),
    { weight: 'bold', slant: 'italic', underline: { style: 'line', color: null }, strike: { color: null } },
  );
});

test('migrateLegacyFace: Boundary — false booleans clear, explicit weight/slant win', () => {
  const m = migrateLegacyFace({ bold: false, italic: false, underline: false, strike: false });
  assert.ok(!('weight' in m), 'bold:false sets no weight');
  assert.ok(!('slant' in m), 'italic:false sets no slant');
  assert.equal(m.underline, null);
  assert.equal(m.strike, null);
  assert.ok(!('bold' in m) && !('italic' in m), 'legacy booleans are removed');
  // an explicit weight/slant already set is not overwritten by the legacy flag
  assert.equal(migrateLegacyFace({ bold: true, weight: 'light' }).weight, 'light');
  assert.equal(migrateLegacyFace({ italic: true, slant: 'oblique' }).slant, 'oblique');
});

test('migrateLegacyFace: Boundary — a new-shape face passes through unchanged (idempotent)', () => {
  const f = { weight: 'semibold', slant: 'oblique', underline: { style: 'wave', color: '#abcdef' }, strike: { color: null } };
  assert.deepEqual(migrateLegacyFace(f), f);
  assert.deepEqual(migrateLegacyFace(migrateLegacyFace(f)), f);
});

test('normalizePkgFace: Normal — carries the additive attribute model', () => {
  const f = normalizePkgFace({
    fg: 'blue', 'distant-fg': '#222222', family: 'Iosevka',
    overline: { color: '#abcdef' }, inverse: true, extend: 1, height: 1.4,
  }, 'user', PAL);
  assert.equal(f['distant-fg'], '#222222');
  assert.equal(f.family, 'Iosevka');
  assert.deepEqual(f.overline, { color: '#abcdef' });
  assert.equal(f.inverse, true);
  assert.equal(f.extend, true); // coerced to boolean
  assert.equal(f.height, 1.4);
});

test('normalizePkgFace: Boundary — distant-fg resolves through the palette', () => {
  const f = normalizePkgFace({ 'distant-fg': 'blue' }, 'user', PAL);
  assert.equal(f['distant-fg'], '#67809c');
});

test('buildPkgmap: Boundary — a face with no default dict still seeds blank', () => {
  const m = buildPkgmap({ a: { faces: [['f', 'f']] } }, PAL);
  assert.deepEqual(m.a.f, {
    fg: null, bg: null, 'distant-fg': null, family: null, weight: null,
    slant: null, underline: null, strike: null, overline: null,
    inherit: null, height: 1, box: null, inverse: false, extend: false,
    source: 'default',
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
    fg: '#67809c', bg: null, weight: 'bold', slant: null, underline: null,
    strike: null, inherit: null, height: 1, source: 'user',
  } } };
  const out = packagesForExport(m);
  assert.equal(out.a.f.fg, '#67809c');
  assert.equal(out.a.f.weight, 'bold');
  assert.equal(out.a.f.source, 'user');
  assert.ok(!('slant' in out.a.f), 'unset slant is omitted');
  assert.ok(!('height' in out.a.f), 'height 1 is omitted');
});

test('packagesForExport: Normal — emits weight/slant/underline/strike only when set', () => {
  const m = { a: { f: normalizePkgFace({
    fg: '#67809c', weight: 'semibold', slant: 'oblique',
    underline: { style: 'wave', color: '#abcdef' }, strike: { color: null },
  }, 'user') } };
  const o = packagesForExport(m).a.f;
  assert.equal(o.weight, 'semibold');
  assert.equal(o.slant, 'oblique');
  assert.deepEqual(o.underline, { style: 'wave', color: '#abcdef' });
  assert.deepEqual(o.strike, { color: null });
});

test('packagesForExport: Boundary — keeps a non-default height', () => {
  const m = { a: { f: { fg: null, bg: null, source: 'user', height: 1.2 } } };
  assert.equal(packagesForExport(m).a.f.height, 1.2);
});

test('packagesForExport: Error — faces with an unknown source are skipped', () => {
  const m = { a: { f: { fg: '#67809c', source: 'system' } } };
  assert.deepEqual(packagesForExport(m), {});
});

test('packagesForExport: Normal — emits additive attrs only when set', () => {
  const m = { a: { f: normalizePkgFace({
    fg: '#67809c', 'distant-fg': '#222222', family: 'Iosevka',
    overline: { color: '#abcdef' }, inverse: true, extend: true,
  }, 'user') } };
  const o = packagesForExport(m).a.f;
  assert.equal(o['distant-fg'], '#222222');
  assert.equal(o.family, 'Iosevka');
  assert.deepEqual(o.overline, { color: '#abcdef' });
  assert.equal(o.inverse, true);
  assert.equal(o.extend, true);
});

test('packagesForExport: Boundary — unset additive attrs are omitted', () => {
  const m = { a: { f: normalizePkgFace({ fg: '#67809c' }, 'user') } };
  const o = packagesForExport(m).a.f;
  for (const k of ['distant-fg', 'family', 'overline', 'inverse', 'extend']) {
    assert.ok(!(k in o), k + ' is omitted when unset');
  }
});

test('mergePackagesInto: Normal — fills missing fields with defaults', () => {
  const m = {};
  mergePackagesInto(m, { a: { f: { fg: '#112233' } } });
  assert.deepEqual(m.a.f, {
    fg: '#112233', bg: null, 'distant-fg': null, family: null, weight: null,
    slant: null, underline: null, strike: null, overline: null,
    inherit: null, height: 1, box: null, inverse: false, extend: false,
    source: 'user',
  });
});

test('mergePackagesInto: Normal — migrates a legacy preset face on import', () => {
  const m = {};
  mergePackagesInto(m, { a: { f: { fg: '#112233', bold: true, italic: true, underline: true } } });
  assert.equal(m.a.f.weight, 'bold');
  assert.equal(m.a.f.slant, 'italic');
  assert.deepEqual(m.a.f.underline, { style: 'line', color: null });
  assert.ok(!('bold' in m.a.f) && !('italic' in m.a.f), 'legacy booleans dropped');
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

test('inline-integrity: theme-studio.html contains palette-generator-core.js verbatim', () => {
  const body = stripExports(readFileSync(here + 'palette-generator-core.js', 'utf8'));
  const html = readFileSync(here + 'theme-studio.html', 'utf8');
  assert.ok(html.includes(body), 'generated page is missing palette-generator-core.js verbatim');
});

test('inline-integrity: theme-studio.html contains palette-generator-ui.js verbatim', () => {
  const body = stripExports(readFileSync(here + 'palette-generator-ui.js', 'utf8'));
  const html = readFileSync(here + 'theme-studio.html', 'utf8');
  assert.ok(html.includes(body), 'generated page is missing palette-generator-ui.js verbatim');
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

// resolveSyntaxFg: an unset syntax category resolves through the Emacs inherit
// chain (the way the generated theme renders), not to the flat default fg.
test('resolveSyntaxFg: a set category uses its own foreground', () => {
  assert.equal(resolveSyntaxFg('kw', { kw: { fg: '#aaaaaa' } }, '#ffffff'), '#aaaaaa');
});
test('resolveSyntaxFg: unset cmd inherits cm (comment-delimiter -> comment)', () => {
  assert.equal(resolveSyntaxFg('cmd', { cm: { fg: '#888888' }, cmd: { fg: null } }, '#ffffff'), '#888888');
});
test('resolveSyntaxFg: unset doc inherits str (doc -> string)', () => {
  assert.equal(resolveSyntaxFg('doc', { str: { fg: '#00aa00' }, doc: { fg: null } }, '#ffffff'), '#00aa00');
});
test('resolveSyntaxFg: unset prop inherits var (property-name -> variable-name)', () => {
  assert.equal(resolveSyntaxFg('prop', { var: { fg: '#0000aa' }, prop: { fg: null } }, '#ffffff'), '#0000aa');
});
test('resolveSyntaxFg: unset fnc inherits fnd (function-call -> function-name)', () => {
  assert.equal(resolveSyntaxFg('fnc', { fnd: { fg: '#aa00aa' }, fnc: { fg: null } }, '#ffffff'), '#aa00aa');
});
test('resolveSyntaxFg: dec is pinned to ty even when dec has its own fg', () => {
  assert.equal(resolveSyntaxFg('dec', { ty: { fg: '#9b5fd0' }, dec: { fg: '#e8bd30' } }, '#ffffff'), '#9b5fd0');
});
test('resolveSyntaxFg: an unset chain bottoms out at the default fg', () => {
  assert.equal(resolveSyntaxFg('cmd', { cm: { fg: null }, cmd: { fg: null } }, '#ffffff'), '#ffffff');
});
test('resolveSyntaxFg: a category with no inherit and no fg uses the default fg', () => {
  assert.equal(resolveSyntaxFg('kw', { kw: { fg: null } }, '#ffffff'), '#ffffff');
});

// resolveUiAttr: an unset ui face attribute resolves through the Emacs built-in
// ui inherit chain (mode-line-inactive -> mode-line, line-number-current-line ->
// line-number), returning null when nothing up the chain is set (caller floors it).
test('resolveUiAttr: a set ui face uses its own attribute', () => {
  assert.equal(resolveUiAttr('mode-line', 'fg', { 'mode-line': { fg: '#111111' } }), '#111111');
});
test('resolveUiAttr: unset mode-line-inactive inherits mode-line', () => {
  assert.equal(resolveUiAttr('mode-line-inactive', 'bg',
    { 'mode-line': { bg: '#222222' }, 'mode-line-inactive': { bg: null } }), '#222222');
});
test('resolveUiAttr: unset line-number-current-line inherits line-number', () => {
  assert.equal(resolveUiAttr('line-number-current-line', 'fg',
    { 'line-number': { fg: '#333333' }, 'line-number-current-line': { fg: null } }), '#333333');
});
test('resolveUiAttr: returns null when nothing up the chain is set', () => {
  assert.equal(resolveUiAttr('mode-line-inactive', 'fg',
    { 'mode-line': { fg: null }, 'mode-line-inactive': { fg: null } }), null);
});
test('resolveUiAttr: a face with no inherit and an unset attribute returns null', () => {
  assert.equal(resolveUiAttr('region', 'bg', { 'region': { bg: null } }), null);
});

// dropdownRowTextColor: a popup row showing a real palette color inherits the
// popup foreground (legible on the fixed dark popup); only the filled default
// row uses a contrast color against its own background. textOn is stubbed so the
// test asserts the decision, not the contrast math.
const stubTextOn = (h) => (h === '#000000' ? '#fff' : '#000');
test('dropdownRowTextColor: a real palette color inherits the popup fg (empty)', () => {
  assert.equal(dropdownRowTextColor('#2a3a5a', '#2a3a5a', stubTextOn), '');
});
test('dropdownRowTextColor: a dark swatch still inherits (regression: blues were unreadable)', () => {
  assert.equal(dropdownRowTextColor('#000000', '#000000', stubTextOn), '');
});
test('dropdownRowTextColor: the filled default row contrasts against its fill', () => {
  assert.equal(dropdownRowTextColor('', '#cdced1', stubTextOn), '#000');
});
test('dropdownRowTextColor: a default row with no fill inherits (empty)', () => {
  assert.equal(dropdownRowTextColor('', '', stubTextOn), '');
});

// appViewKeysSorted: the assignment-view dropdown lists package apps
// alphabetically by display label, independent of the APPS build order
// (generate.py emits bespoke apps first, then inventory apps).
test('appViewKeysSorted: sorts app keys by display label, case-insensitive', () => {
  const apps = { dashboard: { label: 'Dashboard' }, magit: { label: 'Magit' },
                 alert: { label: 'alert' }, 'web-mode': { label: 'web-mode' } };
  assert.deepEqual(appViewKeysSorted(apps), ['alert', 'dashboard', 'magit', 'web-mode']);
});
test('appViewKeysSorted: bespoke-then-inventory build order comes out alphabetical', () => {
  const apps = { magit: { label: 'Magit' }, dashboard: { label: 'Dashboard' },
                 alert: { label: 'alert' }, consult: { label: 'consult' } };
  assert.deepEqual(appViewKeysSorted(apps), ['alert', 'consult', 'dashboard', 'magit']);
});
test('appViewKeysSorted: empty or nullish input yields an empty list', () => {
  assert.deepEqual(appViewKeysSorted({}), []);
  assert.deepEqual(appViewKeysSorted(null), []);
  assert.deepEqual(appViewKeysSorted(undefined), []);
});
test('appViewKeysSorted: an app with no label falls back to its key for ordering', () => {
  const apps = { zebra: {}, apple: { label: 'apple' } };
  assert.deepEqual(appViewKeysSorted(apps), ['apple', 'zebra']);
});

// faceBoxNonDefaults: which of the six per-face setting boxes differ from the
// face's seed default, so the table can mark them.  fg/bg are compared as the
// caller passes them (already hex-resolved), the rest by value.
test('faceBoxNonDefaults: a face equal to its default flags nothing', () => {
  const f = { fg: '#abc', bg: null, bold: false, italic: false, underline: false, strike: false, inherit: null, height: 1, box: null };
  assert.deepEqual(faceBoxNonDefaults(f, { ...f }),
    { fg: false, bg: false, style: false, inherit: false, height: false, box: false });
});
test('faceBoxNonDefaults: a non-1 height flags only the height box', () => {
  const def = { height: 1 };
  assert.deepEqual(faceBoxNonDefaults({ height: 1.1 }, def),
    { fg: false, bg: false, style: false, inherit: false, height: true, box: false });
});
test('faceBoxNonDefaults: a set fg over an empty default flags fg', () => {
  assert.equal(faceBoxNonDefaults({ fg: '#8ea85e' }, {}).fg, true);
  assert.equal(faceBoxNonDefaults({}, {}).fg, false);
});
test('faceBoxNonDefaults: any style attr differing flags the style box once', () => {
  assert.equal(faceBoxNonDefaults({ weight: 'bold' }, { weight: null }).style, true);
  assert.equal(faceBoxNonDefaults({ slant: 'italic' }, {}).style, true);
  assert.equal(faceBoxNonDefaults({ underline: { style: 'line', color: null } }, {}).style, true);
  assert.equal(faceBoxNonDefaults({ weight: 'bold' }, { weight: 'bold' }).style, false);
});
test('faceBoxNonDefaults: inherit and box differences are flagged', () => {
  assert.equal(faceBoxNonDefaults({ inherit: 'bold' }, { inherit: null }).inherit, true);
  assert.equal(faceBoxNonDefaults({ box: { style: 'line' } }, { box: null }).box, true);
  assert.equal(faceBoxNonDefaults({ box: { style: 'line' } }, { box: { style: 'line' } }).box, false);
});
test('faceBoxNonDefaults: nullish inputs flag nothing', () => {
  assert.deepEqual(faceBoxNonDefaults(null, null),
    { fg: false, bg: false, style: false, inherit: false, height: false, box: false });
});

// stepViewIndex: the prev/next arrows step the view-dropdown selection, clamped
// to the option range (no wrap).
test('stepViewIndex: steps forward and back within range', () => {
  assert.equal(stepViewIndex(2, 5, 1), 3);
  assert.equal(stepViewIndex(2, 5, -1), 1);
});
test('stepViewIndex: clamps at both ends, no wrap', () => {
  assert.equal(stepViewIndex(0, 5, -1), 0);
  assert.equal(stepViewIndex(4, 5, 1), 4);
});
test('stepViewIndex: a single option or empty list stays put', () => {
  assert.equal(stepViewIndex(0, 1, 1), 0);
  assert.equal(stepViewIndex(0, 1, -1), 0);
  assert.equal(stepViewIndex(3, 0, -1), 3);
  assert.equal(stepViewIndex(0, 0, 1), 0);
});

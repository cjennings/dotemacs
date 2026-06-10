// Unit tests for the color-families model (app-core.js): grouping a flat palette
// into hue families, regenerating a family's ramp, ranking members by lightness,
// and planning the assignment re-point across a regenerate. Phase 1 of the
// color-families spec. Pure, no DOM. Run: node --test scripts/theme-studio/

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { familiesFromPalette, regenFamily, rankByLightness, stepRepointPlan, sortFamilies } from './app-core.js';
import { oklch2hex, srgb2oklab, oklab2oklch } from './colormath.js';

// Build a palette entry at a controlled OKLCH hue so clustering is deterministic.
const at = (L, C, H, name) => [oklch2hex(L, C, H).hex, name || ('c' + H)];

// --- familiesFromPalette ----------------------------------------------------

test('familiesFromPalette: Normal — separated hues split into one family each', () => {
  const pal = [at(0.6, 0.1, 30, 'red'), at(0.6, 0.1, 150, 'green'), at(0.6, 0.1, 270, 'blue')];
  const { ground, families } = familiesFromPalette(pal, { bg: '#000000', fg: '#ffffff' });
  assert.equal(families.length, 3, 'three separated hues -> three families');
  assert.equal(ground.length, 2, 'ground strip carries bg and fg');
  for (const f of families) assert.equal(f.members.length, 1);
});

test('familiesFromPalette: Boundary — hues sharing an anchor stay one family', () => {
  const pal = [at(0.55, 0.1, 250, 'b1'), at(0.6, 0.1, 256, 'b2')]; // both nearest the blue anchor
  const { families } = familiesFromPalette(pal, { bg: '#000000', fg: '#ffffff' });
  assert.equal(families.length, 1, 'a near hue-pair is one family');
  assert.equal(families[0].members.length, 2);
});

test('familiesFromPalette: Boundary — different anchors split (blue vs teal)', () => {
  const pal = [at(0.6, 0.1, 255, 'b'), at(0.6, 0.1, 200, 'c')];
  const { families } = familiesFromPalette(pal, { bg: '#000000', fg: '#ffffff' });
  assert.equal(families.length, 2);
});

test('familiesFromPalette: Normal — yellow and green land in separate families', () => {
  const pal = [at(0.7, 0.12, 100, 'yellow'), at(0.6, 0.12, 145, 'green')];
  const { families } = familiesFromPalette(pal, { bg: '#000000', fg: '#ffffff' });
  assert.equal(families.length, 2, 'yellow and green are separate anchors');
});

test('familiesFromPalette: Boundary — an intermediate chain does not merge yellow into green', () => {
  // gold, olive, yellow-green, green: anchor assignment buckets by nearest, no single-linkage chaining
  const pal = [at(0.7, 0.1, 90, 'gold'), at(0.65, 0.1, 110, 'olive'), at(0.6, 0.1, 130, 'yg'), at(0.55, 0.1, 150, 'green')];
  const { families } = familiesFromPalette(pal, { bg: '#000000', fg: '#ffffff' });
  assert.equal(families.length, 2, 'two anchors (yellow, green), not one chained family');
});

test('familiesFromPalette: Boundary — a pale tint keeps its hue while a mid gray goes neutral', () => {
  const paleBlue = oklch2hex(0.9, 0.03, 255).hex;   // light, faint -> still blue
  const midGray = oklch2hex(0.6, 0.025, 100).hex;   // mid, faint -> reads neutral
  const { families } = familiesFromPalette([[paleBlue, 'paleblue'], [midGray, 'graytone']], { bg: '#000000', fg: '#ffffff' });
  const neutral = families.find(f => f.neutral);
  assert.ok(neutral && neutral.members.some(m => m.name === 'graytone'), 'mid faint color is neutral');
  assert.ok(families.some(f => !f.neutral && f.members.some(m => m.name === 'paleblue')), 'pale tint stays chromatic');
});

test('familiesFromPalette: Boundary — near-neutral colors form a separate family', () => {
  const pal = [at(0.6, 0.1, 250, 'blue'), at(0.5, 0.004, 250, 'gray')]; // gray below the chroma threshold
  const { families } = familiesFromPalette(pal, { bg: '#000000', fg: '#ffffff' });
  const neutral = families.find(f => f.neutral);
  assert.ok(neutral, 'a neutral family exists');
  assert.ok(neutral.members.some(m => m.name === 'gray'));
  assert.ok(families.some(f => !f.neutral && f.members.some(m => m.name === 'blue')));
});

test('familiesFromPalette: Boundary — ground hex absent from the palette still forms the strip', () => {
  const pal = [at(0.6, 0.1, 250, 'blue')];
  const { ground } = familiesFromPalette(pal, { bg: '#0d0b0a', fg: '#f0fef0' });
  assert.equal(ground.length, 2);
  assert.ok(ground.some(g => g.hex.toLowerCase() === '#0d0b0a' && g.role === 'bg'));
  assert.ok(ground.some(g => g.role === 'fg'));
});

test('familiesFromPalette: Boundary — a chip at a ground hex is not duplicated into a family', () => {
  const pal = [['#0d0b0a', 'ground'], at(0.6, 0.1, 250, 'blue')];
  const { ground, families } = familiesFromPalette(pal, { bg: '#0d0b0a', fg: '#f0fef0' });
  assert.ok(ground.some(g => g.hex.toLowerCase() === '#0d0b0a'));
  assert.ok(!families.some(f => f.members.some(m => m.hex.toLowerCase() === '#0d0b0a')), 'ground chip stays out of families');
});

// --- regenFamily ------------------------------------------------------------

test('regenFamily: Normal — n steps each side plus the base, ordered by offset', () => {
  const r = regenFamily('#67809c', 2);
  assert.equal(r.members.length, 5);
  assert.deepEqual(r.members.map(m => m.offset), [-2, -1, 0, 1, 2]);
  assert.equal(r.members.find(m => m.offset === 0).hex, '#67809c');
});

test('regenFamily: Boundary — n=0 is the base alone, no ramp() clamp to 1', () => {
  const r = regenFamily('#67809c', 0);
  assert.deepEqual(r.members, [{ hex: '#67809c', offset: 0, clamped: false }]);
});

test('regenFamily: Error — a malformed base returns a structured bad-hex', () => {
  assert.deepEqual(regenFamily('nope', 2), { members: [], error: 'bad-hex' });
});

// --- rankByLightness --------------------------------------------------------

test('rankByLightness: Normal — offsets are signed distance from the base by lightness', () => {
  const members = regenFamily('#67809c', 2).members.map(m => m.hex);
  const ranked = rankByLightness(members, '#67809c');
  const base = ranked.find(m => m.hex === '#67809c');
  assert.equal(base.offset, 0);
  const sorted = [...ranked].sort((a, b) => a.offset - b.offset);
  assert.deepEqual(sorted.map(m => m.offset), [-2, -1, 0, 1, 2]);
});

test('rankByLightness: Boundary — a base not among the members ranks by nearest lightness', () => {
  const members = ['#222222', '#888888', '#dddddd'];
  const ranked = rankByLightness(members, '#8a8a8a'); // near the mid member
  const mid = ranked.find(m => m.hex === '#888888');
  assert.equal(mid.offset, 0, 'nearest-lightness member is the base rank');
});

// --- stepRepointPlan --------------------------------------------------------

test('stepRepointPlan: Normal — surviving offsets map old->new, changed hex only', () => {
  const oldR = [{ hex: '#111111', offset: -1 }, { hex: '#222222', offset: 0 }, { hex: '#333333', offset: 1 }];
  const neu = [{ hex: '#111111', offset: -1 }, { hex: '#aaaaaa', offset: 0 }, { hex: '#444444', offset: 1 }];
  const { map, removed } = stepRepointPlan(oldR, neu);
  assert.deepEqual(removed, []);
  assert.deepEqual(map, [['#222222', '#aaaaaa'], ['#333333', '#444444']]); // -1 unchanged, skipped
});

test('stepRepointPlan: Boundary — an offset with no new counterpart is removed, not repointed', () => {
  const oldR = [{ hex: '#000033', offset: -3 }, { hex: '#222222', offset: 0 }];
  const neu = [{ hex: '#222222', offset: 0 }]; // count dropped, -3 gone
  const { map, removed } = stepRepointPlan(oldR, neu);
  assert.deepEqual(map, []);
  assert.deepEqual(removed, ['#000033']);
});

// --- sortFamilies -----------------------------------------------------------

const fam = (baseHex, neutral, members) => ({ base: baseHex, neutral: !!neutral, members: (members || [baseHex]).map(h => ({ hex: h, name: h })) });

test('sortFamilies: Normal — chromatic families order by base hue', () => {
  const fams = [fam(oklch2hex(0.6, 0.1, 270).hex), fam(oklch2hex(0.6, 0.1, 30).hex), fam(oklch2hex(0.6, 0.1, 150).hex)];
  const sorted = sortFamilies(fams);
  const hues = sorted.map(f => Math.round(oklab2oklch(srgb2oklab(f.base)).H));
  for (let i = 1; i < hues.length; i++) assert.ok(hues[i] > hues[i - 1], 'ascending hue: ' + hues.join(','));
});

test('sortFamilies: Boundary — neutral families pin ahead of chromatic ones', () => {
  const sorted = sortFamilies([fam(oklch2hex(0.6, 0.1, 200).hex, false), fam('#808080', true)]);
  assert.equal(sorted[0].neutral, true, 'neutral first');
  assert.equal(sorted[1].neutral, false);
});

test('sortFamilies: Normal — members within a family sort dark to light', () => {
  const members = ['#dddddd', '#222222', '#888888'];
  const sorted = sortFamilies([fam(oklch2hex(0.6, 0.1, 200).hex, false, members)]);
  const ls = sorted[0].members.map(m => oklab2oklch(srgb2oklab(m.hex)).L);
  for (let i = 1; i < ls.length; i++) assert.ok(ls[i] > ls[i - 1], 'ascending lightness');
});

test('sortFamilies: Boundary — order is (hue, then lightness); a hue tie falls to lightness', () => {
  const bases = [oklch2hex(0.6, 0.1, 200).hex, oklch2hex(0.5, 0.1, 200).hex, oklch2hex(0.6, 0.1, 40).hex];
  const sorted = sortFamilies(bases.map(b => fam(b, false)));
  const key = h => { const c = oklab2oklch(srgb2oklab(h)); return [Math.round(c.H), c.L]; };
  for (let i = 1; i < sorted.length; i++) {
    const [h0, l0] = key(sorted[i - 1].base), [h1, l1] = key(sorted[i].base);
    assert.ok(h0 < h1 || (h0 === h1 && l0 <= l1), `order at ${i}: hue ${h0}/${h1} L ${l0.toFixed(3)}/${l1.toFixed(3)}`);
  }
});

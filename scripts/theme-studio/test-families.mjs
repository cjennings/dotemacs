// Unit tests for the color-families model (app-core.js): grouping a flat palette
// into hue families, regenerating a family's ramp, ranking members by lightness,
// and planning the assignment re-point across a regenerate. Phase 1 of the
// color-families spec. Pure, no DOM. Run: node --test scripts/theme-studio/

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { familiesFromPalette, regenFamily, rankByLightness, stepRepointPlan } from './app-core.js';
import { oklch2hex } from './colormath.js';

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

test('familiesFromPalette: Boundary — hues within the gap stay one family', () => {
  const pal = [at(0.55, 0.1, 250, 'b1'), at(0.6, 0.1, 256, 'b2')]; // 6° apart < 25°
  const { families } = familiesFromPalette(pal, { bg: '#000000', fg: '#ffffff' });
  assert.equal(families.length, 1, 'a near hue-pair is one family');
  assert.equal(families[0].members.length, 2);
});

test('familiesFromPalette: Boundary — hues past the gap split', () => {
  const pal = [at(0.6, 0.1, 250, 'b'), at(0.6, 0.1, 200, 'c')]; // 50° apart > 25°
  const { families } = familiesFromPalette(pal, { bg: '#000000', fg: '#ffffff' });
  assert.equal(families.length, 2);
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

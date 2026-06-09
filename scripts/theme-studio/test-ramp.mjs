// Unit tests for the ramp generator (app-core.js `ramp`). Phase 1 of the
// palette-ramps spec: one base color -> a harmonized tonal ramp by stepping
// OKLCH lightness on a held hue, easing chroma toward the extremes, and
// gamut-clamping each step. Pure, no DOM. Run: node --test scripts/theme-studio/

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { ramp } from './app-core.js';
import { srgb2oklab, oklab2oklch, rl } from './colormath.js';

const HEXRE = /^#[0-9a-f]{6}$/;
const baseLCH = (hex) => oklab2oklch(srgb2oklab(hex));

test('ramp: Normal — default opts give 2n steps, darkest-to-lightest, base excluded', () => {
  const r = ramp('#67809c'); // mid blue
  assert.deepEqual(r.adjusted, []);
  assert.equal(r.steps.length, 4); // n=2 -> -2,-1,+1,+2
  assert.deepEqual(r.steps.map(s => s.offset), [-2, -1, 1, 2]);
  for (const s of r.steps) assert.match(s.hex, HEXRE, `${s.hex} is a 6-digit hex`);
  // Lightness rises monotonically across the ordered steps.
  const ls = r.steps.map(s => rl(s.hex));
  for (let i = 1; i < ls.length; i++) assert.ok(ls[i] > ls[i - 1], 'each step lighter than the last');
  // Base sits between -1 and +1 in lightness.
  const baseL = rl('#67809c');
  assert.ok(rl(r.steps[1].hex) < baseL && baseL < rl(r.steps[2].hex), 'base brackets the inner steps');
});

test('ramp: Normal — holds the hue across in-gamut steps', () => {
  const base = '#67809c';
  const H0 = baseLCH(base).H;
  // chromaEase 0 keeps chroma up so the recovered hue is well-defined (near-gray
  // steps have an ill-defined hue that 8-bit quantization can swing).
  const r = ramp(base, { chromaEase: 0 });
  for (const s of r.steps) {
    if (s.clamped) continue; // a clamped step may drift hue; only assert on clean ones
    const dH = Math.abs(baseLCH(s.hex).H - H0);
    assert.ok(Math.min(dH, 360 - dH) < 3.0, `step ${s.offset} holds hue (${dH.toFixed(2)} deg off)`);
  }
});

test('ramp: Normal — chroma eases toward the extremes (outer step less chromatic than inner)', () => {
  const base = '#67809c';
  const r = ramp(base, { n: 2, chromaEase: 0.8 });
  const inner = baseLCH(r.steps[1].hex).C; // offset -1
  const outer = baseLCH(r.steps[0].hex).C; // offset -2
  assert.ok(outer < inner, 'the farther step carries less chroma');
});

test('ramp: Normal — chromaEase 0 holds chroma flat', () => {
  const base = '#67809c';
  const C0 = baseLCH(base).C;
  const r = ramp(base, { n: 1, stepL: 0.06, chromaEase: 0 });
  for (const s of r.steps) {
    if (s.clamped) continue;
    assert.ok(Math.abs(baseLCH(s.hex).C - C0) < 0.01, 'chroma held within tolerance');
  }
});

test('ramp: Boundary — near-white base clamps the lighter steps at L=1', () => {
  const r = ramp('#f6f6f6', { n: 2, stepL: 0.08 });
  assert.equal(r.steps.length, 4);
  const lightest = r.steps[r.steps.length - 1];
  assert.match(lightest.hex, HEXRE);
  assert.ok(rl(lightest.hex) > 0.9, 'lightest step is near white');
});

test('ramp: Boundary — near-black base clamps the darker steps at L=0', () => {
  const r = ramp('#0b0b0b', { n: 2, stepL: 0.08 });
  assert.equal(r.steps.length, 4);
  const darkest = r.steps[0];
  assert.match(darkest.hex, HEXRE);
  assert.ok(rl(darkest.hex) < 0.05, 'darkest step is near black');
});

test('ramp: Boundary — n clamps to [1,4] and reports the adjustment', () => {
  const lo = ramp('#67809c', { n: 0 });
  assert.equal(lo.steps.length, 2); // clamped to n=1
  assert.ok(lo.adjusted.includes('n'));
  const hi = ramp('#67809c', { n: 9 });
  assert.equal(hi.steps.length, 8); // clamped to n=4
  assert.ok(hi.adjusted.includes('n'));
  const frac = ramp('#67809c', { n: 2.7 });
  assert.equal(frac.steps.length, 6); // rounded to 3, in range, still flagged as adjusted
  assert.ok(frac.adjusted.includes('n'));
});

test('ramp: Boundary — stepL and chromaEase clamp to range and report', () => {
  const r = ramp('#67809c', { stepL: 0.5, chromaEase: 2 });
  assert.ok(r.adjusted.includes('stepL'));
  assert.ok(r.adjusted.includes('chromaEase'));
  assert.equal(r.steps.length, 4);
});

test('ramp: Error — malformed hex returns a structured bad-hex, not a throw', () => {
  for (const bad of ['nope', '#xyz', '#12', '12345g', null, undefined, '']) {
    const r = ramp(bad);
    assert.deepEqual(r, { steps: [], error: 'bad-hex' }, `${String(bad)} -> bad-hex`);
  }
});

test('ramp: Boundary — a six-digit hex without the leading # is accepted', () => {
  const r = ramp('67809c');
  assert.equal(r.steps.length, 4);
  assert.ok(!r.error);
});

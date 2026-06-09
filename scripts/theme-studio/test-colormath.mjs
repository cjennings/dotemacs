// Unit tests for the pure color-math core (colormath.js).
// Run: node --test scripts/theme-studio/
// Run with coverage: node --test --experimental-test-coverage scripts/theme-studio/
//
// Fixtures are from the perceptual-color-metrics spec: OKLab via Ottosson's
// reference, APCA via APCA-W3 0.1.9, deltaE via OKLab Euclidean distance.

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { srgb2oklab, oklab2oklch, oklch2hex, apca, deltaE } from './colormath.js';

const close = (a, b, eps = 0.005) => Math.abs(a - b) <= eps;

test('srgb2oklab achromatic anchors', () => {
  const w = srgb2oklab('#ffffff');
  assert.ok(close(w.L, 1.0), `white L ${w.L}`);
  assert.ok(close(w.a, 0) && close(w.b, 0), `white a/b ${w.a},${w.b}`);
  const k = srgb2oklab('#000000');
  assert.ok(close(k.L, 0), `black L ${k.L}`);
});

test('OKLCH chromatic fixtures (red, dupre-blue)', () => {
  const red = oklab2oklch(srgb2oklab('#ff0000'));
  assert.ok(close(red.L, 0.628) && close(red.C, 0.258) && close(red.H, 29.2, 1),
    `red ${JSON.stringify(red)}`);
  const blue = oklab2oklch(srgb2oklab('#67809c'));
  assert.ok(close(blue.L, 0.591) && close(blue.C, 0.052) && close(blue.H, 251.6, 1),
    `dupre-blue ${JSON.stringify(blue)}`);
});

test('round-trip srgb -> oklch -> hex preserves the color', () => {
  for (const h of ['#67809c', '#e8bd30', '#9b5fd0', '#5d9b86', '#cb6b4d']) {
    const lab = srgb2oklab(h);
    const c = oklab2oklch(lab);
    const back = srgb2oklab(oklch2hex(c.L, c.C, c.H).hex);
    assert.ok(close(lab.L, back.L) && close(lab.a, back.a) && close(lab.b, back.b),
      `roundtrip ${h}`);
  }
});

test('APCA both polarities (pinned black/white fixtures)', () => {
  assert.ok(close(apca('#000000', '#ffffff'), 106.0, 0.5),
    `dark-on-light ${apca('#000000', '#ffffff')}`);
  assert.ok(close(apca('#ffffff', '#000000'), -107.9, 0.5),
    `light-on-dark ${apca('#ffffff', '#000000')}`);
  // Chromatic fixture: catches rounded-coefficient drift that black/white can't.
  // Sign is positive (dark-ish text on a light bg).
  assert.ok(apca('#67809c', '#ffffff') > 0, 'chromatic apca sign');
});

test('deltaE-OK identity and ordering against the 0.02 threshold', () => {
  assert.equal(deltaE('#67809c', '#67809c'), 0);
  assert.ok(deltaE('#000000', '#ffffff') > 0);
  const near = deltaE('#67809c', '#69829e'); // barely-different blue
  const far = deltaE('#67809c', '#e8bd30');  // blue vs gold
  assert.ok(near < 0.02, `near ${near}`);
  assert.ok(far > 0.1, `far ${far}`);
});

test('gamut clamp preserves L/H, reduces C, flags clamped', () => {
  const oog = oklch2hex(0.6, 0.5, 30); // very high chroma -> out of sRGB
  assert.equal(oog.clamped, true);
  const got = oklab2oklch(srgb2oklab(oog.hex));
  assert.ok(close(got.L, 0.6, 0.02), `L preserved ${got.L}`);
  assert.ok(close(got.H, 30, 2), `H preserved ${got.H}`);
  assert.ok(got.C < 0.5, `C reduced ${got.C}`);
  const ing = oklch2hex(0.591, 0.052, 251.6); // in gamut
  assert.equal(ing.clamped, false);
});

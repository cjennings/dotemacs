// Unit tests for the pure color-math core (colormath.js).
// Run: node --test scripts/theme-studio/
// Run with coverage: node --test --experimental-test-coverage scripts/theme-studio/
//
// Fixtures are from the perceptual-color-metrics spec: OKLab via Ottosson's
// reference, APCA via APCA-W3 0.1.9, deltaE via OKLab Euclidean distance.

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import {
  srgb2oklab, oklab2oklch, oklch2oklab, oklch2hex, apca, deltaE,
  hex2rgb, rl, contrast, rating, hsv2rgb, rgb2hsv, rgb2hex,
  oklab2lrgb, inGamut, lrgb2hex, planeCell, paletteWarnings,
} from './colormath.js';

const close = (a, b, eps = 0.005) => Math.abs(a - b) <= eps;
const here = fileURLToPath(new URL('.', import.meta.url));
// Same export-strip generate.py applies before inlining (drop `export` lines, rstrip).
const stripExports = (s) =>
  s.split('\n').filter((l) => !l.startsWith('export')).join('\n').replace(/\s+$/, '');

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

test('hex2rgb parses channels', () => {
  assert.deepEqual(hex2rgb('#000000'), [0, 0, 0]);
  assert.deepEqual(hex2rgb('#ffffff'), [255, 255, 255]);
  assert.deepEqual(hex2rgb('#67809c'), [0x67, 0x80, 0x9c]);
});

test('WCAG relative luminance anchors', () => {
  assert.ok(close(rl('#ffffff'), 1.0, 1e-9), `white ${rl('#ffffff')}`);
  assert.ok(close(rl('#000000'), 0.0, 1e-9), `black ${rl('#000000')}`);
  assert.ok(rl('#ff0000') < rl('#00ff00'), 'green brighter than red'); // 0.2126 vs 0.7152
});

test('WCAG contrast: symmetry, identity, and known extremes', () => {
  assert.ok(close(contrast('#000000', '#ffffff'), 21, 1e-6), 'black/white = 21:1');
  assert.equal(contrast('#67809c', '#67809c'), 1); // identical colors
  assert.ok(close(contrast('#0d0b0a', '#67809c'), contrast('#67809c', '#0d0b0a'), 1e-12),
    'order-independent');
  // dupre keyword-blue on ground, a real palette pair (sanity, not a hand-typed number).
  assert.ok(contrast('#67809c', '#0d0b0a') > 4.5, 'dupre blue clears AA on ground');
});

test('rating bands at the AA/AAA boundaries', () => {
  assert.equal(rating(7.0), 'AAA');
  assert.equal(rating(6.99), 'AA');
  assert.equal(rating(4.5), 'AA');
  assert.equal(rating(4.49), 'FAIL');
  assert.equal(rating(0), 'FAIL');
});

test('hsv2rgb primaries and achromatic edges', () => {
  assert.deepEqual(hsv2rgb(0, 1, 1), [255, 0, 0]);
  assert.deepEqual(hsv2rgb(120, 1, 1), [0, 255, 0]);
  assert.deepEqual(hsv2rgb(240, 1, 1), [0, 0, 255]);
  assert.deepEqual(hsv2rgb(0, 0, 1), [255, 255, 255]); // s=0 -> grey (white)
  assert.deepEqual(hsv2rgb(0, 0, 0), [0, 0, 0]);        // v=0 -> black
  assert.deepEqual(hsv2rgb(360, 1, 1), [255, 0, 0]);    // hue wraps
});

test('rgb2hsv inverts hsv2rgb (saturation/value), hue for chromatic inputs', () => {
  assert.deepEqual(rgb2hsv(255, 0, 0), [0, 1, 1]);
  assert.deepEqual(rgb2hsv(0, 0, 0), [0, 0, 0]); // black: h and s undefined -> 0
  const [h, s, v] = rgb2hsv(0, 255, 0);
  assert.ok(close(h, 120, 1e-9) && s === 1 && v === 1, `green hsv ${h},${s},${v}`);
});

test('hsv <-> rgb round-trip property over random colors', () => {
  let seed = 1234567; // deterministic LCG: no Math.random, repeatable failures
  const rnd = () => (seed = (seed * 1103515245 + 12345) & 0x7fffffff) / 0x7fffffff;
  for (let i = 0; i < 500; i++) {
    const rgb = [Math.floor(rnd() * 256), Math.floor(rnd() * 256), Math.floor(rnd() * 256)];
    const [h, s, v] = rgb2hsv(...rgb);
    assert.deepEqual(hsv2rgb(h, s, v), rgb, `round-trip ${rgb}`);
  }
});

test('rgb2hex formats and clamps out-of-range channels', () => {
  assert.equal(rgb2hex(0, 0, 0), '#000000');
  assert.equal(rgb2hex(255, 255, 255), '#ffffff');
  assert.equal(rgb2hex(0x67, 0x80, 0x9c), '#67809c');
  assert.equal(rgb2hex(-5, 300, 128), '#00ff80'); // clamps below 0 and above 255
});

test('oklab2lrgb / lrgb2hex round-trip through known sRGB colors', () => {
  for (const h of ['#000000', '#ffffff', '#67809c', '#e8bd30']) {
    const lab = srgb2oklab(h);
    assert.equal(lrgb2hex(oklab2lrgb(lab.L, lab.a, lab.b)), h, `round-trip ${h}`);
  }
});

test('inGamut flags reachable vs unreachable OKLCH (forward-only gamut test)', () => {
  // dupre-blue is a real sRGB color -> in gamut.
  const ok = oklch2oklab(0.591, 0.052, 251.6);
  assert.equal(inGamut(oklab2lrgb(ok.L, ok.a, ok.b)), true, 'reachable');
  // very high chroma at mid lightness -> outside sRGB.
  const bad = oklch2oklab(0.7, 0.4, 140);
  assert.equal(inGamut(oklab2lrgb(bad.L, bad.a, bad.b)), false, 'unreachable');
  // the in-gamut verdict must agree with oklch2hex's clamped flag (the plane and
  // the commit path share one gamut boundary).
  assert.equal(inGamut(oklab2lrgb(ok.L, ok.a, ok.b)), !oklch2hex(0.591, 0.052, 251.6).clamped);
  assert.equal(inGamut(oklab2lrgb(bad.L, bad.a, bad.b)), !oklch2hex(0.7, 0.4, 140).clamped);
});

test('planeCell: reachable cell returns its exact hex, agrees with oklch2hex', () => {
  // Normal: a low-chroma blue is reachable; the hex matches the commit path.
  const cell = planeCell(0.591, 0.052, 251.6);
  assert.equal(cell.inGamut, true);
  assert.equal(cell.hex, oklch2hex(0.591, 0.052, 251.6).hex);
});

test('planeCell: C=0 is the achromatic grey for its lightness', () => {
  // Boundary: zero chroma -> a neutral grey, always in gamut, hue irrelevant.
  const a = planeCell(0.5, 0, 0), b = planeCell(0.5, 0, 251.6);
  assert.equal(a.inGamut, true);
  assert.equal(a.hex, b.hex, 'hue must not matter at C=0');
  assert.equal(a.hex[1], a.hex[3]); // r==g==b nibble: grey
});

test('planeCell: out-of-gamut chroma is flagged, no hex', () => {
  // Error/boundary: chroma past sRGB at this L/H.
  const cell = planeCell(0.7, 0.4, 140);
  assert.equal(cell.inGamut, false);
  assert.equal(cell.hex, null);
  assert.equal(cell.inGamut, !oklch2hex(0.7, 0.4, 140).clamped); // shares the boundary
});

test('paletteWarnings: a near-identical pair warns, named, with its ΔE', () => {
  const { warnings, overflow, nearest } = paletteWarnings(
    [['#0d0b0a', 'ground'], ['#cdced1', 'fg'], ['#67809c', 'blue'], ['#69829e', 'blue2']]);
  assert.equal(warnings.length, 1);
  assert.equal(overflow, 0);
  const w = warnings[0];
  assert.deepEqual([w.aName, w.bName], ['blue', 'blue2']);
  assert.ok(w.dE > 0 && w.dE < 0.02, `dE ${w.dE}`);
  assert.equal(nearest.length, 4);
  assert.ok(nearest[2] < 0.02 && nearest[3] < 0.02, 'blue/blue2 are each other’s nearest');
});

test('paletteWarnings: a well-spread palette warns about nothing', () => {
  const { warnings, overflow } = paletteWarnings(
    [['#0d0b0a', 'ground'], ['#cdced1', 'fg'], ['#67809c', 'blue'], ['#e8bd30', 'gold'], ['#cb6b4d', 'terra']]);
  assert.equal(warnings.length, 0);
  assert.equal(overflow, 0);
});

test('paletteWarnings: boundary cases — empty, single, identical', () => {
  assert.deepEqual(paletteWarnings([]), { warnings: [], overflow: 0, nearest: [] });
  const one = paletteWarnings([['#67809c', 'blue']]);
  assert.deepEqual(one.warnings, []);
  assert.deepEqual(one.nearest, [Infinity]); // no neighbor
  const dup = paletteWarnings([['#67809c', 'a'], ['#67809c', 'b']]);
  assert.equal(dup.warnings.length, 1);
  assert.equal(dup.warnings[0].dE, 0); // identical colors -> ΔE 0
});

test('paletteWarnings: closest-first ordering and cap with overflow', () => {
  // Seven near-identical colors -> C(7,2)=21 sub-threshold pairs.
  const pal = [['#0d0b0a', 'ground'], ['#cdced1', 'fg']];
  for (let k = 0; k < 7; k++) pal.push(['#' + (0x67 + k).toString(16).padStart(2, '0') + '809c', 'c' + k]);
  const { warnings, overflow } = paletteWarnings(pal, 0.02, 5);
  assert.equal(warnings.length, 5, 'capped at 5');
  assert.equal(overflow, 16, '21 pairs - 5 shown');
  for (let i = 1; i < warnings.length; i++)
    assert.ok(warnings[i].dE >= warnings[i - 1].dE, 'ascending by ΔE');
});

test('paletteWarnings: threshold is inclusive-exclusive at the boundary', () => {
  // A custom threshold lets a pair fall just inside or just outside.
  const pal = [['#67809c', 'a'], ['#69829e', 'b']]; // dE ~0.0067
  assert.equal(paletteWarnings(pal, 0.0067).warnings.length, 0, 'd < threshold is strict');
  assert.equal(paletteWarnings(pal, 0.007).warnings.length, 1, 'just above the pair distance');
});

// Guards the one-source-of-truth contract: the page must carry colormath.js's
// body (sans exports) verbatim, so the inlined copy and the tested module cannot
// drift. Requires `python3 generate.py` to have run first.
test('inline-integrity: theme-studio.html contains the colormath.js body verbatim', () => {
  const body = stripExports(readFileSync(here + 'colormath.js', 'utf8'));
  const html = readFileSync(here + 'theme-studio.html', 'utf8');
  assert.ok(html.includes(body), 'generated page is missing the colormath.js body verbatim');
});

// colormath.js — pure color-math core for theme-studio.
//
// One source of truth: node imports this module (tests); generate.py inlines its
// body into the page (stripping the trailing export block) so the browser runs
// the same code. No DOM, no side effects.
//
// Algorithms: OKLab/OKLCH from Bjorn Ottosson (2020,
// https://bottosson.github.io/posts/oklab/); APCA from APCA-W3 0.1.9
// (https://github.com/Myndex/apca-w3); deltaE is OKLab Euclidean distance.

function hex2rgb(h) {
  return [parseInt(h.substr(1, 2), 16), parseInt(h.substr(3, 2), 16), parseInt(h.substr(5, 2), 16)];
}

// sRGB transfer (0..1 channel <-> linear-light).
function lin(c) { return c <= 0.04045 ? c / 12.92 : Math.pow((c + 0.055) / 1.055, 2.4); }
function delin(c) { return c <= 0.0031308 ? 12.92 * c : 1.055 * Math.pow(c, 1 / 2.4) - 0.055; }
function clamp01(c) { return c < 0 ? 0 : c > 1 ? 1 : c; }

function srgb2oklab(hex) {
  const [R, G, B] = hex2rgb(hex);
  const r = lin(R / 255), g = lin(G / 255), b = lin(B / 255);
  const l = 0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b;
  const m = 0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b;
  const s = 0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b;
  const l_ = Math.cbrt(l), m_ = Math.cbrt(m), s_ = Math.cbrt(s);
  return {
    L: 0.2104542553 * l_ + 0.7936177850 * m_ - 0.0040720468 * s_,
    a: 1.9779984951 * l_ - 2.4285922050 * m_ + 0.4505937099 * s_,
    b: 0.0259040371 * l_ + 0.7827717662 * m_ - 0.8086757660 * s_,
  };
}

function oklab2oklch(lab) {
  let H = Math.atan2(lab.b, lab.a) * 180 / Math.PI;
  if (H < 0) H += 360;
  return { L: lab.L, C: Math.hypot(lab.a, lab.b), H };
}

function oklch2oklab(L, C, H) {
  const hr = H * Math.PI / 180;
  return { L, a: C * Math.cos(hr), b: C * Math.sin(hr) };
}

// OKLab -> linear sRGB (may fall outside [0,1] when out of gamut).
function oklab2lrgb(L, a, b) {
  const l_ = L + 0.3963377774 * a + 0.2158037573 * b;
  const m_ = L - 0.1055613458 * a - 0.0638541728 * b;
  const s_ = L - 0.0894841775 * a - 1.2914855480 * b;
  const l = l_ * l_ * l_, m = m_ * m_ * m_, s = s_ * s_ * s_;
  return [
    4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
    -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
    -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s,
  ];
}

function inGamut(lrgb) {
  const e = 1e-4;
  return lrgb.every(c => c >= -e && c <= 1 + e);
}

function lrgb2hex(lrgb) {
  return '#' + lrgb.map(c => {
    const v = Math.round(clamp01(delin(clamp01(c))) * 255);
    return v.toString(16).padStart(2, '0');
  }).join('');
}

// OKLCH -> in-gamut sRGB hex. When the requested chroma is unreachable, reduce C
// by binary search holding L and H fixed; report whether clamping happened.
function oklch2hex(L, C, H) {
  const lab0 = oklch2oklab(L, C, H);
  const lrgb0 = oklab2lrgb(lab0.L, lab0.a, lab0.b);
  if (inGamut(lrgb0)) return { hex: lrgb2hex(lrgb0), clamped: false };
  let lo = 0, hi = C;
  for (let i = 0; i < 24; i++) {
    const mid = (lo + hi) / 2;
    const lab = oklch2oklab(L, mid, H);
    if (inGamut(oklab2lrgb(lab.L, lab.a, lab.b))) lo = mid; else hi = mid;
  }
  const lab = oklch2oklab(L, lo, H);
  return { hex: lrgb2hex(oklab2lrgb(lab.L, lab.a, lab.b)), clamped: true };
}

// APCA-W3 0.1.9. Returns signed Lc: positive for dark-text-on-light, negative
// for light-text-on-dark. Constants transcribed verbatim from the pinned source.
function apcaY(hex) {
  const [R, G, B] = hex2rgb(hex);
  return 0.2126729 * Math.pow(R / 255, 2.4)
    + 0.7151522 * Math.pow(G / 255, 2.4)
    + 0.0721750 * Math.pow(B / 255, 2.4);
}

function apca(textHex, bgHex) {
  const blkThrs = 0.022, blkClmp = 1.414, deltaYmin = 0.0005;
  const normBG = 0.56, normTXT = 0.57, revTXT = 0.62, revBG = 0.65;
  const scaleBoW = 1.14, scaleWoB = 1.14, loBoWoffset = 0.027, loWoBoffset = 0.027, loClip = 0.1;
  let Ytxt = apcaY(textHex), Ybg = apcaY(bgHex);
  Ytxt = Ytxt > blkThrs ? Ytxt : Ytxt + Math.pow(blkThrs - Ytxt, blkClmp);
  Ybg = Ybg > blkThrs ? Ybg : Ybg + Math.pow(blkThrs - Ybg, blkClmp);
  if (Math.abs(Ybg - Ytxt) < deltaYmin) return 0;
  let out;
  if (Ybg > Ytxt) {
    const sapc = (Math.pow(Ybg, normBG) - Math.pow(Ytxt, normTXT)) * scaleBoW;
    out = sapc < loClip ? 0 : sapc - loBoWoffset;
  } else {
    const sapc = (Math.pow(Ybg, revBG) - Math.pow(Ytxt, revTXT)) * scaleWoB;
    out = sapc > -loClip ? 0 : sapc + loWoBoffset;
  }
  return out * 100;
}

// deltaE-OK: Euclidean distance in OKLab.
function deltaE(aHex, bHex) {
  const x = srgb2oklab(aHex), y = srgb2oklab(bHex);
  return Math.hypot(x.L - y.L, x.a - y.a, x.b - y.b);
}

// --- WCAG 2.x relative luminance + contrast (migrated from the page inline) ---
// rl reuses the canonical lin() above. On 8-bit channels lin's 0.04045 cutoff is
// byte-identical to the WCAG 0.03928 piecewise the inline copy used — no channel
// value falls between the two thresholds (10/255 = 0.0392, 11/255 = 0.0431) — so
// every #rrggbb contrast value is preserved exactly.
function rl(hex) {
  const [R, G, B] = hex2rgb(hex);
  return 0.2126 * lin(R / 255) + 0.7152 * lin(G / 255) + 0.0722 * lin(B / 255);
}

function contrast(aHex, bHex) {
  const L1 = rl(aHex), L2 = rl(bHex), hi = Math.max(L1, L2), lo = Math.min(L1, L2);
  return (hi + 0.05) / (lo + 0.05);
}

function rating(r) { return r >= 7 ? 'AAA' : r >= 4.5 ? 'AA' : 'FAIL'; }

// --- HSV <-> sRGB for the color picker (migrated from the page inline) ---
function hsv2rgb(h, s, v) {
  h = (h % 360 + 360) % 360 / 360;
  const i = Math.floor(h * 6), f = h * 6 - i, p = v * (1 - s), q = v * (1 - f * s), t = v * (1 - (1 - f) * s);
  let r, g, b;
  switch (((i % 6) + 6) % 6) {
    case 0: [r, g, b] = [v, t, p]; break;
    case 1: [r, g, b] = [q, v, p]; break;
    case 2: [r, g, b] = [p, v, t]; break;
    case 3: [r, g, b] = [p, q, v]; break;
    case 4: [r, g, b] = [t, p, v]; break;
    default: [r, g, b] = [v, p, q];
  }
  return [Math.round(r * 255), Math.round(g * 255), Math.round(b * 255)];
}

function rgb2hsv(r, g, b) {
  r /= 255; g /= 255; b /= 255;
  const mx = Math.max(r, g, b), mn = Math.min(r, g, b), d = mx - mn;
  let h = 0;
  if (d) {
    if (mx === r) h = ((g - b) / d + 6) % 6;
    else if (mx === g) h = (b - r) / d + 2;
    else h = (r - g) / d + 4;
    h *= 60;
  }
  return [h, mx ? d / mx : 0, mx];
}

function rgb2hex(r, g, b) {
  return '#' + [r, g, b].map(x => Math.max(0, Math.min(255, x)).toString(16).padStart(2, '0')).join('');
}

export { srgb2oklab, oklab2oklch, oklch2oklab, oklch2hex, apca, deltaE, hex2rgb, lin, rl, contrast, rating, hsv2rgb, rgb2hsv, rgb2hex };

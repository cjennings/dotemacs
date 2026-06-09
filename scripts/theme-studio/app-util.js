// Pure color/UI-boundary helpers: hex-input parsing, the contrast-rating status
// color, and the readable text color for a background. These are kept out of
// colormath.js (the pure math core) but are unit-tested and inlined into the page
// the same way. textOn leans on rl from colormath; the import is for the tests —
// generate.py strips it on inline, where rl is already present from the inlined
// colormath core.
import { rl } from './colormath.js';

// Normalize a hex string: trim, accept an optional leading #, require exactly six
// hex digits, lowercase the result. Returns null for anything else.
function normHex(s){s=s.trim();if(/^[0-9a-fA-F]{6}$/.test(s))s='#'+s;return /^#[0-9a-fA-F]{6}$/.test(s)?s.toLowerCase():null;}

// Map a WCAG contrast ratio to a status color: AAA green (>=7), AA grey (>=4.5),
// otherwise the fail red.
function ratingColor(r){return r>=7?'#5d9b86':r>=4.5?'#a9b2bb':'#cb6b4d';}

// Pick black or white text for a background hex, by WCAG relative luminance.
function textOn(h){const L=rl(h);return ((L+0.05)/0.05)>(1.05/(L+0.05))?'#000':'#fff';}

export { normHex, ratingColor, textOn };

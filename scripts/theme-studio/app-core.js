// Pure app logic — the package-face model and the dropdown option list — with no
// DOM and no module globals (every dependency is a parameter). It is unit-tested
// directly (test-app-core.mjs) and inlined into the page like colormath.js, so
// the browser runs the same code the tests import. The app.js wrappers (pname,
// seedPkgmap, ddList, pkgEffFg, pkgEffBg) are thin delegators that pass the
// live PALETTE / APPS / PKGMAP into these.
//
// The imports below are for the Node tests; generate.py strips them on inline,
// where normHex (app-util.js) and the colormath helpers are already present from
// the bodies inlined above this one.
import { normHex } from './app-util.js';
import { oklch2hex, srgb2oklab, oklab2oklch, contrast } from './colormath.js';

// Resolve a palette name (or a raw #hex) to a hex; null when the name is unknown.
function nameToHex(n,palette){if(!n)return null;if(/^#/.test(n))return n;const p=palette.find(p=>p[1]===n);return p?p[0]:null;}

// Seed the package-face map from the app inventory's per-face defaults.
function buildPkgmap(apps,palette){const m={};for(const app in apps){m[app]={};for(const row of apps[app].faces){const face=row[0],d=row[2]||{};m[app][face]={fg:nameToHex(d.fg,palette),bg:nameToHex(d.bg,palette),bold:!!d.bold,italic:!!d.italic,underline:!!d.underline,strike:!!d.strike,inherit:d.inherit||null,height:d.height||1,box:d.box||null,source:'default'};}}return m;}

// The package faces worth exporting (anything seeded or user-touched), trimmed.
function packagesForExport(map){const out={};for(const app in map){const faces={};for(const face in map[app]){const f=map[app][face];if(f.source==='default'||f.source==='user'||f.source==='cleared'){const o={fg:f.fg,bg:f.bg,bold:f.bold,italic:f.italic,underline:!!f.underline,strike:!!f.strike,inherit:f.inherit,source:f.source};if(f.height&&f.height!==1)o.height=f.height;if(f.box)o.box=f.box;faces[face]=o;}}if(Object.keys(faces).length)out[app]=faces;}return out;}

// Merge an imported package block into a face map, filling missing fields.
function mergePackagesInto(map,pkgs){if(!pkgs)return;for(const app in pkgs){if(!map[app])map[app]={};for(const face in pkgs[app]){const f=pkgs[app][face]||{};map[app][face]={fg:f.fg??null,bg:f.bg??null,bold:!!f.bold,italic:!!f.italic,underline:!!f.underline,strike:!!f.strike,inherit:f.inherit??null,height:f.height||1,box:f.box??null,source:f.source||'user'};}}}

// Effective fg/bg for a package face, following its inherit chain. seen guards
// against an inherit cycle (returns null rather than recursing forever).
function effResolve(map,app,face,attr,seen){seen=seen||{};const f=map[app]&&map[app][face];if(!f||seen[face])return null;seen[face]=1;if(f[attr])return f[attr];if(f.inherit&&map[app][f.inherit])return effResolve(map,app,f.inherit,attr,seen);return null;}

// Standard swatch-dropdown option list: a default entry, then the palette. When
// cur is set but no longer in the palette, surface it as a "(gone)" entry first.
function optList(cur,palette){const have=cur===''||palette.some(p=>p[0]===cur);return [['','— default —'],...(have?palette:[[cur,'(gone) '+cur],...palette])];}

// Turn a theme name into a safe filename slug: collapse runs of disallowed
// characters to a single dash, trim leading/trailing dashes, fall back to 'theme'.
function slugify(name){return name.replace(/[^A-Za-z0-9._-]+/g,'-').replace(/^-+|-+$/g,'')||'theme';}

// Generate a tonal ramp from one base color: 2n steps at offsets -n..-1 and
// +1..+n (the base itself is excluded — it already lives in the palette),
// ordered darkest -> lightest. Holds the OKLCH hue, steps lightness by stepL per
// stop, and eases chroma toward the extremes (quadratic in |offset|/n, so only
// the farthest step loses most of its color). Every step is gamut-clamped and
// carries its own clamped flag. Returns {steps:[{hex,clamped,offset}], adjusted}
// where adjusted names any knob clamped/rounded into range, or {steps:[],
// error:'bad-hex'} for an unparseable base. Pure — opts are clamped, never thrown.
function ramp(baseHex,opts){
  const hex=typeof baseHex==='string'?normHex(baseHex):null;
  if(!hex)return {steps:[],error:'bad-hex'};
  const o=opts||{},adjusted=[];
  const knob=(name,def,lo,hi,isInt)=>{
    const v=o[name];
    if(typeof v!=='number'||!isFinite(v))return def;
    const r=isInt?Math.round(v):v,c=Math.min(hi,Math.max(lo,r));
    if(c!==v)adjusted.push(name);
    return c;
  };
  const n=knob('n',2,1,4,true),stepL=knob('stepL',0.08,0.04,0.12,false),chromaEase=knob('chromaEase',0.5,0,1,false);
  const {L:L0,C:C0,H:H0}=oklab2oklch(srgb2oklab(hex));
  const steps=[];
  for(let off=-n;off<=n;off++){
    if(off===0)continue;
    const L=Math.min(1,Math.max(0,L0+off*stepL));
    const t=Math.abs(off)/n,C=C0*(1-chromaEase*t*t);
    const {hex:h,clamped}=oklch2hex(L,C,H0);
    steps.push({hex:h,clamped,offset:off});
  }
  return {steps,adjusted};
}

// --- background-contrast safety (palette-ramps spec, Phase 3) ----------------
// An overlay background sits behind many foregrounds at once, so its real
// constraint is the worst-case contrast over the whole set, not one fg/bg pair.

// The closed v1 set of code-overlay faces whose worst-case floor we compute.
// Other overlay faces (secondary-selection, isearch-fail, ...) are vNext, added
// explicitly rather than by a heuristic. Shared by app.js and the tests.
const COVERED_FACES=['region','hl-line','highlight','lazy-highlight','isearch'];

// A covered face's foreground set: the distinct syntax-token colors plus the
// default foreground, each labeled (syntax role preferred, else 'default').
// state = {covered:[face], syntaxAssignments:[{role,hex}], defaultFg}. Returns
// {set:[{hex,label}]}, or {set:[],reason} where reason is 'out-of-scope' (the
// face isn't in the covered set) or 'empty' (no syntax assignments constrain it).
function fgSetFor(face,state){
  const covered=(state&&state.covered)||COVERED_FACES;
  if(!covered.includes(face))return {set:[],reason:'out-of-scope'};
  const syn=((state&&state.syntaxAssignments)||[]).filter(a=>a&&a.hex);
  if(!syn.length)return {set:[],reason:'empty'};
  const byHex=new Map();
  const add=(hex,label,isRole)=>{const k=hex.toLowerCase(),cur=byHex.get(k);if(!cur)byHex.set(k,{hex:k,label});else if(isRole&&cur.label==='default')cur.label=label;};
  if(state&&state.defaultFg)add(state.defaultFg,'default',false);
  for(const a of syn)add(a.hex,a.role||a.hex,true);
  return {set:[...byHex.values()]};
}

// Worst-case (minimum) WCAG contrast of a background against a foreground set,
// with the limiting foreground's hex and label. fgSet is fgSetFor's set. An empty
// set returns nulls so the caller can show the no-set readout instead of a floor.
function floor(bgHex,fgSet){
  if(!fgSet||!fgSet.length)return {ratio:null,limitingHex:null,limitingLabel:null};
  let best=Infinity,lh=null,ll=null;
  for(const f of fgSet){const r=contrast(f.hex,bgHex);if(r<best){best=r;lh=f.hex;ll=f.label;}}
  return {ratio:best,limitingHex:lh,limitingLabel:ll};
}

// The lightest background at (hue, chroma) whose worst-case floor over fgSet still
// clears target (a WCAG ratio). Scans L up from black to bracket the first
// dark-side crossing, then binary-searches it to tol 0.001. status:
//   'ok'    - a ceiling L was found
//   'none'  - even pure black fails (a foreground is too dark for the target)
//   'all'   - no foreground set to constrain (vacuously safe everywhere)
//   'clamp' - the ceiling L can't hold the requested chroma (gamut-clamped there)
function lMax(hue,chroma,fgSet,target){
  if(!fgSet||!fgSet.length)return {L:1,status:'all'};
  const at=(L)=>{const {hex,clamped}=oklch2hex(L,chroma,hue);return {r:floor(hex,fgSet).ratio,clamped};};
  if(at(0).r<target)return {L:null,status:'none'};
  let loL=0,hiL=null;
  for(let L=0.01;L<=1+1e-9;L+=0.01){const c=Math.min(L,1);if(at(c).r<target){hiL=c;break;}loL=c;}
  if(hiL===null)return {L:1,status:'all'};
  for(let i=0;i<20;i++){const mid=(loL+hiL)/2;if(at(mid).r>=target)loL=mid;else hiL=mid;}
  return {L:loL,status:at(loL).clamped?'clamp':'ok'};
}

export { nameToHex, buildPkgmap, packagesForExport, mergePackagesInto, effResolve, optList, slugify, ramp, fgSetFor, floor, lMax, COVERED_FACES };

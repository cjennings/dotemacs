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
import { oklch2hex, srgb2oklab, oklab2oklch, oklab2lrgb, lrgb2hex, inGamut, contrast } from './colormath.js';

// Resolve a palette name (or a raw #hex) to a hex; null when the name is unknown.
function nameToHex(n,palette){if(!n)return null;if(/^#/.test(n))return n;const p=palette.find(p=>p[1]===n);return p?p[0]:null;}

function normalizePkgFace(d,source,palette){
  d=d||{};
  const resolve=(v)=>palette?nameToHex(v,palette):v;
  return {fg:resolve(d.fg)??null,bg:resolve(d.bg)??null,bold:!!d.bold,italic:!!d.italic,underline:!!d.underline,strike:!!d.strike,inherit:d.inherit??null,height:d.height||1,box:d.box??null,source:source||d.source||'user'};
}

// Seed the package-face map from the app inventory's per-face defaults.
function buildPkgmap(apps,palette){const m={};for(const app in apps){m[app]={};for(const row of apps[app].faces){m[app][row[0]]=normalizePkgFace(row[2],'default',palette);}}return m;}

// The package faces worth exporting (anything seeded or user-touched), trimmed.
function packagesForExport(map){const out={};for(const app in map){const faces={};for(const face in map[app]){const f=map[app][face];if(f.source==='default'||f.source==='user'||f.source==='cleared'){const o={fg:f.fg,bg:f.bg,bold:f.bold,italic:f.italic,underline:!!f.underline,strike:!!f.strike,inherit:f.inherit,source:f.source};if(f.height&&f.height!==1)o.height=f.height;if(f.box)o.box=f.box;faces[face]=o;}}if(Object.keys(faces).length)out[app]=faces;}return out;}

// Merge an imported package block into a face map, filling missing fields.
function mergePackagesInto(map,pkgs){if(!pkgs)return;for(const app in pkgs){if(!map[app])map[app]={};for(const face in pkgs[app]){const f=pkgs[app][face]||{};map[app][face]=normalizePkgFace(f,f.source||'user');}}}

// Effective fg/bg for a package face, following its inherit chain. seen guards
// against an inherit cycle (returns null rather than recursing forever).
function effResolve(map,app,face,attr,seen){seen=seen||{};const f=map[app]&&map[app][face];if(!f||seen[face])return null;seen[face]=1;if(f[attr])return f[attr];if(f.inherit&&map[app][f.inherit])return effResolve(map,app,f.inherit,attr,seen);return null;}

// Emacs built-in inherit chains for the syntax categories theme studio exposes.
// An unset category foreground resolves the way the generated theme renders in
// Emacs: build-theme.el writes no override for an unset face, so Emacs falls back
// to the face's own :inherit -- comment-delimiter->comment, doc->string,
// property-name->variable-name, function-call->function-name -- not to the
// default foreground.
const SYNTAX_INHERIT={cmd:'cm',doc:'str',prop:'var',fnc:'fnd'};

// Effective foreground for a syntax category, following the Emacs inherit chain.
// SYNTAX maps category -> face object with an optional `fg`; DEFAULTFG is the
// theme's default foreground (the chain's floor). `dec` (decorator) is pinned to
// `ty`: Emacs has no decorator face and renders decorators with
// font-lock-type-face, so a dec color set in the studio would never reach Emacs.
function resolveSyntaxFg(cat,syntax,defaultFg){
  let k=(cat==='dec')?'ty':cat;
  const seen={};
  while(k&&!seen[k]){
    seen[k]=1;
    const fg=syntax[k]&&syntax[k].fg;
    if(fg)return fg;
    k=SYNTAX_INHERIT[k];
  }
  return defaultFg;
}

// Emacs built-in inherit chains for the ui faces whose parent is also a studio ui
// face, so an unset attribute previews the way Emacs renders it: mode-line-inactive
// inherits mode-line, line-number-current-line inherits line-number.
const UI_INHERIT={'mode-line-inactive':'mode-line','line-number-current-line':'line-number'};

// First set value of ATTR ('fg'/'bg') for ui FACE, walking UI_INHERIT; null when
// nothing up the chain is set. The caller applies its own floor (default fg,
// ground, or transparent), since that floor differs per attribute and face.
function resolveUiAttr(face,attr,uimap){
  let f=face;
  const seen={};
  while(f&&!seen[f]){
    seen[f]=1;
    const v=uimap[f]&&uimap[f][attr];
    if(v)return v;
    f=UI_INHERIT[f];
  }
  return null;
}

// Standard swatch-dropdown option list: a default entry, then the palette. When
// cur is set but no longer in the palette, surface it as a "(gone)" entry first.
function optList(cur,palette){const have=cur===''||palette.some(p=>p[0]===cur);return [['','— default —'],...(have?palette:[[cur,'(gone)'],...palette])];}

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
  const add=(hex,label,name,isRole)=>{const k=hex.toLowerCase(),cur=byHex.get(k);if(!cur)byHex.set(k,{hex:k,label,name:name||label});else if(isRole&&cur.label==='default'){cur.label=label;cur.name=name||label;}};
  if(state&&state.defaultFg)add(state.defaultFg,'default','default',false);
  for(const a of syn)add(a.hex,a.role||a.hex,a.name||a.role||a.hex,true);
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

// --- color columns -----------------------------------------------------------
// Columns are structural, not inferred by color. Generated ramp entries are named
// base-1/base/base+1 and remain in that base column regardless of their hex. A
// manually-added color starts as its own singleton column. The flat palette stays
// the editable truth; these pure functions group it, regenerate a ramp, and plan
// assignment re-point across a regenerate.

function oklchOf(hex){return oklab2oklch(srgb2oklab(hex));}
function isReservedGroundLikeName(name){return /^(bg|fg)(?:[-_+].+|\d.*)$/i.test(name||'');}
function isPureEndpointHex(hex){const h=(hex||'').toLowerCase();return h==='#ffffff'||h==='#000000';}
function interpOklabHex(a,b,t,offset){
  const lab={L:a.L+(b.L-a.L)*t,a:a.a+(b.a-a.a)*t,b:a.b+(b.b-a.b)*t};
  const lrgb=oklab2lrgb(lab.L,lab.a,lab.b);
  return {hex:lrgb2hex(lrgb),offset,clamped:!inGamut(lrgb)};
}
function columnStem(name){name=name||'color';if(/^color-\d+$/.test(name))return name;name=name.replace(/[+-]\d+$/,'');return name.replace(/\d+$/,'')||'color';}
function columnOffset(name){const m=(name||'').match(/([+-]\d+)$/);return m?parseInt(m[1],10):0;}
function legacyColumnStem(name){return isReservedGroundLikeName(name)?name:columnStem(name);}
function legacyColumnOffset(name){return isReservedGroundLikeName(name)?0:columnOffset(name);}
function columnIdOf(entry){return (entry&&entry[2])||legacyColumnStem(entry&&entry[1]);}
function groundRoleOfEntry(entry,ground){
  if(!entry)return null;
  const [hex,name]=entry,col=entry[2],n=(name||'').toLowerCase(),h=(hex||'').toLowerCase();
  const bg=(ground&&ground.bg||'').toLowerCase(),fg=(ground&&ground.fg||'').toLowerCase();
  if(/^ground[+-]\d+$/i.test(name||''))return 'step';
  if(col==='ground'){
    if(bg&&h===bg)return 'bg';
    if(fg&&h===fg)return 'fg';
    return 'step';
  }
  if(bg&&h===bg&&(n==='bg'||n==='ground'))return 'bg';
  if(fg&&h===fg&&n==='fg')return 'fg';
  return null;
}
function nameOfGroundRole(palette,ground,role){
  const found=palette.find(entry=>groundRoleOfEntry(entry,ground)===role);
  return found?found[1]:null;
}

function normalizePaletteEntryCore(entry){
  const hex=entry&&entry[0],name=(entry&&entry[1])||'color';
  return [hex,name,(entry&&entry[2])||columnIdOf(entry)];
}

function groundColumnMembersFromPalette(palette,ground){
  const byRole={bg:null,fg:null,steps:[]};
  for(const entry of palette){
    const role=groundRoleOfEntry(entry,ground);
    if(role==='bg'||role==='fg')byRole[role]={hex:entry[0],name:entry[1]};
    else if(role==='step')byRole.steps.push({hex:entry[0],name:entry[1]});
  }
  const stepIndex=m=>{const x=(m.name||'').match(/^ground[+-](\d+)$/i);return x?parseInt(x[1],10):Infinity;};
  byRole.steps.sort((a,b)=>stepIndex(a)-stepIndex(b));
  return [byRole.bg||{hex:ground&&ground.bg,name:'bg'},...byRole.steps,byRole.fg||{hex:ground&&ground.fg,name:'fg'}].filter(m=>m.hex);
}

function clearPalettePlan(palette,ground){
  const normalized=palette.map(normalizePaletteEntryCore),removed=[],keep=[];
  normalized.filter(entry=>!groundRoleOfEntry(entry,ground)).forEach(([hex,name])=>{if(name)removed.push({hex,name});});
  const addEndpoint=(role,hex,name)=>{
    const found=normalized.find(entry=>groundRoleOfEntry(entry,ground)===role);
    if(found)keep.push(found);else if(hex)keep.push([hex,name,'ground']);
  };
  addEndpoint('bg',ground&&ground.bg,'bg');
  addEndpoint('fg',ground&&ground.fg,'fg');
  return {palette:keep,removed};
}

function deletePaletteColumnPlan(palette,ground,columnId){
  const normalized=palette.map(normalizePaletteEntryCore),removed=[],keep=[];
  for(const entry of normalized){
    if(groundRoleOfEntry(entry,ground)||columnIdOf(entry)!==columnId)keep.push(entry);
    else removed.push({hex:entry[0],name:entry[1]});
  }
  return {palette:keep,removed};
}

function areAllLocked(keys,locked){
  const has=k=>locked instanceof Set?locked.has(k):Array.isArray(locked)&&locked.includes(k);
  return !!(keys&&keys.length)&&keys.every(has);
}
function lockToggleLabel(keys,locked){return areAllLocked(keys,locked)?'unlock all':'lock all';}
function toggleLockSet(keys,locked){
  const next=new Set(locked||[]),all=areAllLocked(keys,next);
  (keys||[]).forEach(k=>all?next.delete(k):next.add(k));
  return next;
}

// Group a flat palette into the ground strip plus structural columns. ground is
// {bg,fg}; those endpoint hexes form the pinned ground column even when absent
// from the palette, and ground+N entries are reserved for that column. Everything
// else groups by its stable column id, not by OKLCH hue/chroma or display name.
// Legacy two-field entries fall back to their generated-name stem until edited.
function columnsFromPalette(palette,ground){
  const bg=ground&&ground.bg,fg=ground&&ground.fg;
  const groundStrip=[];
  if(bg)groundStrip.push({hex:bg,role:'bg',name:nameOfGroundRole(palette,ground,'bg')});
  if(fg)groundStrip.push({hex:fg,role:'fg',name:nameOfGroundRole(palette,ground,'fg')});
  const byColumn=new Map(),columns=[];
  for(const entry of palette){
    const [hex,name]=entry;
    if(groundRoleOfEntry(entry,ground))continue;
    const column=columnIdOf(entry),offset=entry[2]?columnOffset(name):legacyColumnOffset(name);
    if(!byColumn.has(column))byColumn.set(column,{column,members:[]});
    byColumn.get(column).members.push({hex,name,offset,column});
  }
  for(const f of byColumn.values()){
    const base=(f.members.find(m=>m.offset===0)||f.members[0]).hex;
    columns.push({base,column:f.column,stem:f.column,members:f.members.map(m=>({hex:m.hex,name:m.name,column:m.column}))});
  }
  return {ground:groundStrip,columns};
}
// Regenerate a column's members as a symmetric span around the base: n=0 is the
// base alone, n>=1 divides the OKLab intervals black..base and base..white into
// n interior steps per side. Pure black/white endpoint duplicates and rounded
// base duplicates are skipped. {members:[{hex,offset,clamped}]} or
// {members:[],error:'bad-hex'}.
function regenColumn(baseHex,n,opts){
  const hex=typeof baseHex==='string'?normHex(baseHex):null;
  if(!hex)return {members:[],error:'bad-hex'};
  const k=Math.min(8,Math.max(0,Math.round(n||0)));
  if(k===0)return {members:[{hex,offset:0,clamped:false}]};
  const base=srgb2oklab(hex),black=srgb2oklab('#000000'),white=srgb2oklab('#ffffff'),steps=[];
  for(let i=1;i<=k;i++){
    const dark=interpOklabHex(black,base,i/(k+1),i-k-1);
    const light=interpOklabHex(base,white,i/(k+1),i);
    steps.push(dark,light);
  }
  const members=[...steps.filter(s=>!isPureEndpointHex(s.hex)&&s.hex.toLowerCase()!==hex),{hex,offset:0,clamped:false}].sort((a,b)=>a.offset-b.offset);
  return {members};
}
// Rank a column's current member hexes by lightness and give each a signed offset
// from the base (the matching hex, or the nearest by lightness if the base isn't
// present). Lets a regenerate match old positions to new ramp offsets.
function rankByLightness(memberHexes,baseHex){
  const items=memberHexes.map(h=>({hex:h,L:oklchOf(h).L})).sort((a,b)=>a.L-b.L);
  let bi=items.findIndex(m=>m.hex.toLowerCase()===(baseHex||'').toLowerCase());
  if(bi<0){const bl=oklchOf(baseHex).L;let best=Infinity;items.forEach((m,i)=>{const d=Math.abs(m.L-bl);if(d<best){best=d;bi=i;}});}
  return items.map((m,i)=>({hex:m.hex,offset:i-bi}));
}
// Plan the assignment re-point for a regenerate: for each old ranked member, the
// new member at the same offset is the same position. {map:[[old,new]]} for
// positions whose hex changed; {removed:[hex]} for positions with no new
// counterpart (the caller leaves their references a visible "(gone)").
function stepRepointPlan(oldRanked,newMembers){
  const byOff=new Map(newMembers.map(m=>[m.offset,m.hex])),map=[],removed=[];
  for(const o of oldRanked){
    const nh=byOff.get(o.offset);
    if(nh===undefined)removed.push(o.hex);
    else if(nh.toLowerCase()!==o.hex.toLowerCase())map.push([o.hex,nh]);
  }
  return {map,removed};
}

// Preserve structural order. Generated ramps are inserted in offset order, and
// columns are emitted in first-seen palette order. No color sorting happens here.
function sortColumnMembers(column){return Object.assign({},column,{members:[...column.members]});}
function sortColumns(columns){return columns.map(sortColumnMembers);}
function lightestFirstMembers(members){return [...members].sort((a,b)=>oklchOf(b.hex).L-oklchOf(a.hex).L);}

// Dropdown order for color selection mirrors the visual palette organization:
// bg/fg first, then structural columns in display order. Within each group,
// choices run lightest-to-darkest. Stored palette order stays untouched; this is
// selection-only organization.
function paletteOptionList(cur,palette,ground){
  const have=cur===''||palette.some(p=>p[0]===cur)||[ground&&ground.bg,ground&&ground.fg].filter(Boolean).includes(cur);
  const out=[['','— default —']],seen=new Set();
  if(!have)out.push([cur,'(gone)']);
  const add=(hex,name)=>{if(!hex)return;const key=hex.toLowerCase()+'|'+(name||'');if(seen.has(key))return;seen.add(key);out.push([hex,name||hex]);};
  const grouped=columnsFromPalette(palette,ground||{});
  const groundMembers=grouped.ground.map(g=>({hex:g.hex,name:g.name||g.role}))
    .concat(palette.filter(entry=>groundRoleOfEntry(entry,ground)==='step').map(([hex,name])=>({hex,name})));
  groundMembers.forEach(m=>add(m.hex,m.name));
  sortColumns(grouped.columns).forEach(f=>lightestFirstMembers(f.members).forEach(m=>add(m.hex,m.name)));
  return out;
}
function spanNeighborHex(cur,palette,ground,dir){
  if(!cur)return null;
  const wanted=(cur||'').toLowerCase(),groups=[],byLight=(a,b)=>oklchOf(a.hex).L-oklchOf(b.hex).L;
  const addGroup=members=>{
    const seen=new Set(),g=[];
    members.filter(m=>m&&m.hex).sort(byLight).forEach(m=>{const h=m.hex.toLowerCase();if(!seen.has(h)){seen.add(h);g.push(m);}});
    if(g.length)groups.push(g);
  };
  addGroup(groundColumnMembersFromPalette(palette,ground||{}));
  sortColumns(columnsFromPalette(palette,ground||{}).columns).forEach(f=>addGroup(f.members));
  for(const g of groups){
    const i=g.findIndex(m=>(m.hex||'').toLowerCase()===wanted);
    if(i<0)continue;
    const next=g[i+(dir>0?1:-1)];
    return next?next.hex:null;
  }
  return null;
}

export { nameToHex, normalizePkgFace, buildPkgmap, packagesForExport, mergePackagesInto, effResolve, resolveSyntaxFg, resolveUiAttr, optList, paletteOptionList, spanNeighborHex, slugify, ramp, fgSetFor, floor, lMax, COVERED_FACES, columnsFromPalette, regenColumn, rankByLightness, stepRepointPlan, sortColumns, sortColumnMembers, groundRoleOfEntry, groundColumnMembersFromPalette, clearPalettePlan, deletePaletteColumnPlan, areAllLocked, lockToggleLabel, toggleLockSet };

const SAMPLES=SAMPLES_J, CATS=CATS_J, UI_FACES=UIFACES_J, APPS=APPS_J;
const COLOR_NAMES=COLOR_NAMES_J;
const FACE_DOCS=FACE_DOCS_J, SYNTAX_DOCS=SYNTAX_DOCS_J; // face/category -> docstring first line, for element hovers
let MAP=MAP_J, PALETTE=PALETTE_J, SYNTAX=SYNTAX_J, UIMAP=UIMAP_J;
let LOCKED=new Set(LOCKS_J);   // rows whose choice is decided (controls disabled, skipped by erase/reset batch actions)
const DELTAE_MIN=0.02; // OKLab ΔE below this = colors too close to tell apart (perceptual-metrics spec)
const DEFAULT_UIMAP=JSON.parse(JSON.stringify(UIMAP));
function syntaxBlank(k){return {fg:MAP[k]||null,bg:null,'distant-fg':null,family:null,weight:null,slant:null,underline:null,strike:null,overline:null,box:null,inverse:false,extend:false,inherit:null,height:null,heightMode:null};}
function syncSyntaxCache(k){const s=SYNTAX[k]||syntaxBlank(k);MAP[k]=s.fg||'';}
function syncAllSyntaxCache(){CATS.forEach(c=>syncSyntaxCache(c[0]));}
function syncSyntaxFromCache(){CATS.forEach(c=>{const k=c[0];syntaxFace(k).fg=MAP[k]||null;});}
function syntaxFace(k){if(!SYNTAX[k])SYNTAX[k]=syntaxBlank(k);return SYNTAX[k];}
function setSyntaxFg(k,hex){syntaxFace(k).fg=hex||null;syncSyntaxCache(k);}
syncAllSyntaxCache();
const DEFAULT_SYNTAX=JSON.parse(JSON.stringify(SYNTAX));
// --- tier-3 package faces: pure state helpers (Phase 1) ---
// Thin wrappers over the pure logic in app-core.js (inlined further down),
// passing the live module state. packagesForExport / mergePackagesInto live in
// the core verbatim and are used by name.
function pname(n){return nameToHex(n,PALETTE);}
function seedPkgmap(){return buildPkgmap(APPS,PALETTE);}
let PKGMAP=seedPkgmap();
// Preview-locate registry (preview-locate spec). One cached, module-level
// registry rebuilt once per assignment / import / reset / view-switch batch — at
// the top of the two preview renderers (buildPkgPreview, buildMockFrame), which
// every such path funnels through before spans render. Never rebuilt per hover or
// per span. locate-onpane is recomputed from the current view at render time
// (isLocateOnPane), never stored here. Built lazily (not at declaration): the
// inlined buildLocateRegistry / UI_INHERIT from app-core.js are spliced below
// this point, so an init call here would hit the const's temporal dead zone.
let LOCATE_REG={};
function rebuildLocateRegistry(){LOCATE_REG=buildLocateRegistry(APPS,PKGMAP,UIMAP,MAP);return LOCATE_REG;}
function esc(t){return t.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');}
// Pure color-math core (lin/rl/contrast/rating/hsv2rgb/rgb2hsv/hex2rgb/rgb2hex,
// plus OKLab/OKLCH/APCA/deltaE), inlined verbatim from colormath.js.
COLORMATH_J
// Pure package-model + dropdown logic, inlined verbatim from app-core.js. The
// wrappers above (pname/seedPkgmap/ddList/pkgEffFg/pkgEffBg) delegate here.
APP_CORE_J
// Pure color/UI-boundary helpers (normHex, ratingColor, textOn), inlined from
// app-util.js. textOn uses rl from the colormath core above.
APP_UTIL_J
// The seeding engine (seed model + pure seed()), inlined from seed-core.js. Uses
// oklchOf/oklch2hex from the colormath core above; the #seedtest gate runs seed().
SEED_CORE_J
// Pure palette-generator planner and browser-side generator panel.
PALETTE_GENERATOR_CORE_J
PALETTE_GENERATOR_UI_J
// The contrast-cell readout shared by every table: a WCAG ratio colored by its
// table verdict. Callers compute r for their own fg/bg.
function verdictFor(r,target=4.5){return r>=target?'PASS':'FAIL';}
function crHtml(r){return `<span style="color:${ratingColor(r)}" title="${esc(contrastTitle(r))}">${r.toFixed(1)}</span>`;}
// Effective fg/bg with the standard fallback: an unset foreground reads as the
// default fg (MAP['p']), an unset background as the ground (MAP['bg']). All three
// tiers resolve their raw value through these before measuring or rendering.
function effFg(v){return v||MAP['p'];}
function effBg(v){return v||MAP['bg'];}
// The ground pair (background + default foreground), passed to every app-core
// helper that needs to resolve ground roles. Was the literal {bg:MAP['bg'],
// fg:MAP['p']} repeated across app.js, palette-actions.js, and the browser gates.
function groundPair(){return {bg:MAP['bg'],fg:MAP['p']};}
function cid(l){return l.replace(/\W/g,'');}
function buildLangSel(){const s=document.getElementById('langsel');s.innerHTML='';for(const lang of Object.keys(SAMPLES).sort((a,b)=>a.localeCompare(b))){const o=document.createElement('option');o.value=lang;o.textContent=lang;s.appendChild(o);}if(SAMPLES['Elisp'])s.value='Elisp';}
function renderCode(){
  const lang=document.getElementById('langsel').value;let html='';
  for(const line of SAMPLES[lang]){
    if(line.length===0){html+='\n';continue;}
    for(const [k,t] of line)html+=`<span data-k="${k}" style="${syntaxStyle(k)}">${esc(t)}</span>`;
    html+='\n';}
  const cp=document.getElementById('codepre');cp.innerHTML=html;
  cp.onclick=(e)=>{const s=e.target.closest('[data-k]');if(s)flashAssign(s.dataset.k);};
  buildMockFrame();
}
CONTROLS_J
// Expand/collapse every row in a table at once, then sync the per-row triangles.
function setAllExpanded(tableId,expand){
  const tb=document.getElementById(tableId);if(!tb)return;
  tb.querySelectorAll('tr.detailrow').forEach(d=>{d.style.display=expand?'':'none';const k=d.dataset.detailFor;if(k){expand?EXPANDED.add(k):EXPANDED.delete(k);}});
  tb.querySelectorAll('.exptoggle').forEach(b=>{b.textContent=expand?'▼':'▶';b.classList.toggle('on',expand);});
}
// The header-level expand/collapse-all toggle for a table. Its label and triangle
// track the aggregate: any row open -> ▼ collapse all; all closed -> ▶ expand all.
const EXPALL_TABLE={syntaxexpandall:'legbody',uiexpandall:'uibody',pkgexpandall:'pkgbody'};
function syncExpandAllBtns(){
  for(const id in EXPALL_TABLE){const btn=document.getElementById(id);const tb=document.getElementById(EXPALL_TABLE[id]);if(!btn||!tb)continue;
    const anyOpen=[...tb.querySelectorAll('tr.detailrow')].some(d=>d.style.display!=='none');
    btn.textContent=anyOpen?'▼ collapse all':'▶ expand all';}
}
function toggleAllExpanded(id){
  const tableId=EXPALL_TABLE[id],tb=document.getElementById(tableId);if(!tb)return;
  const anyOpen=[...tb.querySelectorAll('tr.detailrow')].some(d=>d.style.display!=='none');
  setAllExpanded(tableId,!anyOpen);syncExpandAllBtns();
}
// Column count for a table's detail-row colspan, read from its header so the
// expander never hardcodes a width that drifts when a column is added.
function tableColCount(tableId){const h=document.querySelector('#'+tableId+' thead tr');return h?h.cells.length:1;}
// Apply a batch action to every editable row in a tier. keyFn maps a row entry to
// its lock key, or null to skip the row entirely (syntax bg and the default fg);
// resetFn does the actual clearing. Locked rows are left untouched.
function clearUnlockedRows(items,keyFn,resetFn){
  for(const it of items){const k=keyFn(it);if(k===null)continue;if(!LOCKED.has(k))resetFn(it);}
}
function rebuildColorTables(){
  buildTable();buildUITable();buildPkgTable();// buildPkgTable self-guards when #pkgbody is absent
}
function refreshPaletteState(opts={}){
  renderPalette();rebuildColorTables();
  if(opts.pkgPreview)buildPkgPreview();
  if(opts.code!==false)renderCode();
  if(opts.ground!==false)applyGround();
  if(opts.covered)repaintCovered();
}
function syntaxLockKeys(){return CATS.map(c=>c[0]);}
function uiLockKeys(){return UI_FACES.map(f=>'ui:'+f[0]);}
function pkgLockKeys(){const app=curApp();return APPS[app].faces.map(f=>'pkg:'+app+':'+f[0]);}
function tierLockKeys(tier){return tier==='syntax'?syntaxLockKeys():tier==='ui'?uiLockKeys():pkgLockKeys();}
function updateLockToggle(tier){
  const ids={syntax:'syntaxlocktoggle',ui:'uilocktoggle',pkg:'pkglocktoggle'},b=document.getElementById(ids[tier]);if(!b)return;
  b.textContent=lockToggleLabel(tierLockKeys(tier),LOCKED);
}
function updateLockToggles(){updateLockToggle('syntax');updateLockToggle('ui');updateLockToggle('pkg');updateViewLockIndicators();}
function toggleAllLocks(tier){
  const all=areAllLocked(tierLockKeys(tier),LOCKED);
  LOCKED=toggleLockSet(tierLockKeys(tier),LOCKED);
  if(tier==='syntax')buildTable();else if(tier==='ui')buildUITable();else buildPkgTable();
  updateLockToggles();
  notify((all?'unlocked ':'locked ')+(tier==='pkg'?'package':tier)+' rows',false);
}
function clearUnlocked(){
  clearUnlockedRows(CATS,c=>(c[0]==='bg'||c[0]==='p')?null:c[0],c=>{SYNTAX[c[0]]=syntaxBlank(c[0]);SYNTAX[c[0]].fg=null;syncSyntaxCache(c[0]);});
  buildTable();renderCode();notify('erased editable syntax elements',false);
}
function resetUnlocked(){
  clearUnlockedRows(CATS,c=>c[0],c=>{const k=c[0];SYNTAX[k]=JSON.parse(JSON.stringify(DEFAULT_SYNTAX[k]||syntaxBlank(k)));syncSyntaxCache(k);});
  rebuildColorTables();buildPkgPreview();renderCode();applyGround();repaintCovered();
  notify('reset editable syntax elements to captured defaults',false);
}
function clearUnlockedUI(){
  clearUnlockedRows(UI_FACES,f=>'ui:'+f[0],f=>{UIMAP[f[0]]=uiFaceBlank();});
  buildUITable();buildMockFrame();notify('erased editable UI faces',false);
}
function resetUnlockedUI(){
  clearUnlockedRows(UI_FACES,f=>'ui:'+f[0],f=>{UIMAP[f[0]]=JSON.parse(JSON.stringify(DEFAULT_UIMAP[f[0]]||uiFaceBlank()));});
  buildUITable();buildMockFrame();notify('reset editable UI faces to captured defaults',false);
}
function clearUnlockedPkg(){
  const app=curApp();
  clearUnlockedRows(APPS[app].faces,f=>'pkg:'+app+':'+f[0],f=>{PKGMAP[app][f[0]]=normalizePkgFace({source:'cleared'},'cleared');});
  pkgChanged();notify('erased editable '+app+' faces',false);
}
function buildTable(){
  const tb=document.getElementById('legbody');tb.innerHTML='';
  for(const [kind,label,ex] of CATS){
    const tr=document.createElement('tr');tr.dataset.kind=kind;
    const sf=syntaxFace(kind),cur=sf.fg||'',list=ddList(cur);
    const exTd=document.createElement('td');exTd.className='ex';exTd.textContent=ex;
    const crTd=document.createElement('td');crTd.style.whiteSpace='nowrap';crTd.style.fontSize='10pt';
    function rowFg(){return kind==='bg'?MAP['p']:effFg(syntaxFace(kind).fg);}
    function rowBg(){return syntaxFace(kind).bg||MAP['bg'];}
    function styleEx(){const s=syntaxFace(kind);exTd.style.color=rowFg();exTd.style.background=rowBg();exTd.style.fontWeight=cssWeight(s.weight);exTd.style.fontStyle=s.slant||'normal';exTd.style.textDecoration=(s.underline?'underline ':'')+(s.strike?'line-through':'')||'none';exTd.style.boxShadow=boxCss(s.box,rowBg());}
    function styleCr(){const r=contrast(rowFg(),rowBg());crTd.innerHTML=crHtml(r);}
    const dd=mkColorDropdown(list,cur,(hex)=>{const s=syntaxFace(kind);s.fg=hex||null;syncSyntaxCache(kind);styleEx();styleCr();renderCode();if(kind==='bg'||kind==='p'){applyGround();buildTable();buildPkgTable();buildPkgPreview();}repaintCovered();},{compact:true,defaultHex:rowFg()});
    const bgd=mkColorDropdown(ddList(sf.bg||''),sf.bg||'',hex=>{const s=syntaxFace(kind);s.bg=hex||null;styleEx();styleCr();renderCode();repaintCovered();},{compact:true,defaultHex:rowBg()});
    styleEx();styleCr();
    const stTd=document.createElement('td');
    const stCtls=mkStyleControls(syntaxFace(kind),()=>{styleEx();renderCode();},{defaultHex:rowFg()});
    const stCluster=document.createElement('div');stCluster.className='stylecluster';stCtls.forEach(c=>stCluster.appendChild(c));stTd.appendChild(stCluster);
    const c0=document.createElement('td');c0.appendChild(dd);
    const cB=document.createElement('td');cB.appendChild(bgd);
    const cX=document.createElement('td');const boxCtl=mkBoxControl(()=>syntaxFace(kind).box,b=>{syntaxFace(kind).box=b;styleEx();renderCode();},{compact:true});cX.appendChild(boxCtl);
    const exp=mkExpander(syntaxFace(kind),tableColCount('legtable'),()=>{styleEx();renderCode();},{expandKey:kind,showInherit:true,inheritOptions:[''].concat(BASE_INHERITS),defaultHex:rowFg(),ndCheck:()=>overflowNonDefault(syntaxFace(kind),DEFAULT_SYNTAX[kind],true)});
    exp.detail.dataset.detailFor=kind;
    const lkTd=mkLockCell(kind,[dd,bgd,...stCtls,boxCtl,...exp.locks]);
    const c2=document.createElement('td');c2.className='cat';c2.title=composeHoverTitle(SYNTAX_DOCS[kind],c2.title);c2.appendChild(exp.btn);
    const c2lbl=document.createElement('span');c2lbl.textContent=' '+label;c2lbl.style.cursor='pointer';c2lbl.title='flash this category in the code';c2lbl.onclick=()=>flashTokens(kind);c2.appendChild(c2lbl);
    tr.appendChild(lkTd);tr.appendChild(c2);tr.appendChild(c0);tr.appendChild(cB);tr.appendChild(stTd);tr.appendChild(cX);tr.appendChild(crTd);tr.appendChild(exTd);
    tb.appendChild(tr);tb.appendChild(exp.detail);}
  updateLockToggle('syntax');syncExpandAllBtns();
}
PALETTE_ACTIONS_J
function notify(msg,err){const m=document.getElementById('palmsg');if(!m)return;m.textContent=msg;m.style.color=err?'#cb6b4d':'#8a9496';m.style.opacity='1';clearTimeout(m._t);m._t=setTimeout(()=>{m.style.opacity='0';},err?4000:2800);}
function applyEdit(){if(selectedIdx!==null)updateColor();else addColor();}
function selectColor(i){selectedIdx=i;GEN_SELECTION=null;const [hex,name]=PALETTE[i];setHex(hex);document.getElementById('newname').value=name;renderPalette();renderGeneratorPreview();notify('editing "'+name+'" — change the value, then Enter (or Update selected) to save',false);}
function updateColor(){
  if(selectedIdx===null){notify('click a palette color to select it first',true);return;}
  const i=selectedIdx,oldHex=PALETTE[i][0],oldRole=groundRoleOfEntry(PALETTE[i],groundPair());
  const newHex=curHex();
  const newName=(document.getElementById('newname').value.trim())||PALETTE[i][1];
  if(PALETTE.some((p,j)=>j!==i&&p[1].toLowerCase()===newName.toLowerCase())){notify('another color is already named "'+newName+'" — names must be unique',true);return;}
  const isGroundEdit=oldRole==='bg'||oldRole==='fg';
  // If the edited color is a column base with a ramp, recolor the whole column: regenerate from the new base at the same count.
  const columns=columnsFromPalette(PALETTE,groundPair()).columns;
  const column=isGroundEdit?null:columns.find(f=>f.base.toLowerCase()===oldHex.toLowerCase());
  const count=column?Math.max(0,...rankByLightness(column.members.map(m=>m.hex),column.base).map(m=>Math.abs(m.offset))):0;
  const columnId=isGroundEdit?'ground':(PALETTE[i][2]||columnStem(PALETTE[i][1]));
  PALETTE[i]=[newHex,newName,columnId];
  const duplicateOldHex=PALETTE.some((p,j)=>j!==i&&p[0].toLowerCase()===oldHex.toLowerCase());
  if(isGroundEdit)repointHex(oldHex,newHex);
  else if(!duplicateOldHex&&oldHex!==MAP['bg']&&oldHex!==MAP['p'])repointHex(oldHex,newHex);
  if(column&&count>0){
    const oldHexes=column.members.map(m=>m.hex.toLowerCase()===oldHex.toLowerCase()?newHex:m.hex);
    regenColumnInPlace(oldHexes,newHex,newName,count,column.column||columnId);
    closePicker();selectedIdx=null;refreshPaletteState();notify('recolored "'+newName+'" column from the new base',false);return;
  }
  closePicker();refreshPaletteState();notify('updated "'+newName+'"',false);
}
const DEFAULT_PICKER_HEX='#67809c';
let [pkH,pkS,pkV]=rgb2hsv(...hex2rgb(DEFAULT_PICKER_HEX)),pickerOn=false;
function curHex(){return normHex(document.getElementById('newhexstr').value)||DEFAULT_PICKER_HEX;}
let pkMode='any';   // contrast mask: any / aa / aaa (what constraint to mask)
let pkModel='hsv';  // color model for editing: hsv / oklch (orthogonal to pkMode)
const OKLCH_CMAX=0.4; // chroma axis range for the C×L plane (and the C dial); past sRGB at most hues, so the gamut grey shows the reachable region
function pkThresh(){return pkMode==='aa'?4.5:pkMode==='aaa'?7:0;}
function drawMask(){const cv=document.getElementById('svmask');if(!cv)return;const sv=document.getElementById('sv'),w=cv.width=sv.clientWidth,h=cv.height=sv.clientHeight,ctx=cv.getContext('2d');ctx.clearRect(0,0,w,h);const T=pkThresh();if(!T)return;ctx.fillStyle='rgba(8,7,6,0.66)';const step=4;for(let x=0;x<w;x+=step){const S=x/w;for(let y=0;y<h;y+=step){const V=1-y/h,[r,g,b]=hsv2rgb(pkH,S,V);if(contrast(rgb2hex(r,g,b),MAP['bg'])<T)ctx.fillRect(x,y,step,step);}}}
// Phase 4b: the SV box becomes a Chroma×Lightness plane in OKLCH mode. Per cell
// the in-gamut test is forward-only (oklch→oklab→linear-rgb + range check), never
// the binary search — that is reserved for committing a color. The rendered
// bitmap is cached on (hue, dims, mask, bg) so dragging C/L (fixed hue) reuses it.
let _planeCache={key:null,data:null};
function paintOklchPlane(H){
  const cv=document.getElementById('svmask');if(!cv)return;
  const sv=document.getElementById('sv'),w=cv.width=sv.clientWidth,h=cv.height=sv.clientHeight,ctx=cv.getContext('2d');
  const T=pkThresh(),key=Math.round(H)+'|'+w+'|'+h+'|'+pkMode+'|'+MAP['bg'];
  if(_planeCache.key===key&&_planeCache.data){ctx.putImageData(_planeCache.data,0,0);return;}
  const step=4;
  for(let x=0;x<w;x+=step){const C=(x/w)*OKLCH_CMAX;
    for(let y=0;y<h;y+=step){const L=1-y/h,cell=planeCell(L,C,H);
      if(!cell.inGamut){ctx.fillStyle='#15120f';ctx.fillRect(x,y,step,step);continue;}
      ctx.fillStyle=cell.hex;ctx.fillRect(x,y,step,step);
      if(T&&contrast(cell.hex,MAP['bg'])<T){ctx.fillStyle='rgba(8,7,6,0.66)';ctx.fillRect(x,y,step,step);}}}
  _planeCache={key,data:ctx.getImageData(0,0,w,h)};
}
// --- safe-lightness guidance (spec Phase 5) ----------------------------------
let pkSafeFace='';   // covered overlay face the picker's lightness is checked against (or '')
function setSafeFace(f){pkSafeFace=f;if(pickerOn)paintPicker();}
// Shade the band of the C×L plane whose lightness is too light to keep pkSafeFace
// readable over its foreground set, with the L_max ceiling as the band's lower
// edge. One marker computed via lMax at the current chroma, not a per-pixel mask.
function paintSafeBand(C,H){
  const el=document.getElementById('svsafe');if(!el)return;
  if(!pkSafeFace||pkModel!=='oklch'){el.style.display='none';return;}
  const fs=fgSetForFace(pkSafeFace);
  if(fs.reason||!fs.set.length){el.style.display='none';return;}
  const sv=document.getElementById('sv'),h=sv.clientHeight,res=lMax(H,C,fs.set,WORST_TARGET);
  if(res.status==='all'){el.style.display='none';return;}
  el.style.display='block';el.style.top='0px';
  el.style.height=(res.status==='none'?h:Math.max(0,(1-res.L)*h))+'px';
  el.title='safe-lightness ceiling for '+pkSafeFace+' ('+(res.status==='none'?'no safe lightness — a foreground is too dark':'L_max '+res.L.toFixed(3)+(res.status==='clamp'?', chroma-clamped':''))+')';
}
function paintPicker(){const sv=document.getElementById('sv');if(!sv)return;
  const w=sv.clientWidth,h=sv.clientHeight,hh=document.getElementById('hue').clientHeight;
  if(pkModel==='oklch'){const [L,C,H]=readOklch();sv.style.background='#15120f';paintOklchPlane(H);
    document.getElementById('svcur').style.left=(Math.min(1,C/OKLCH_CMAX)*w)+'px';
    document.getElementById('svcur').style.top=((1-L)*h)+'px';
    document.getElementById('huecur').style.top=((H/360)*hh)+'px';paintSafeBand(C,H);return;}
  const sb=document.getElementById('svsafe');if(sb)sb.style.display='none';
  sv.style.background=`linear-gradient(to top,#000,rgba(0,0,0,0)),linear-gradient(to right,#fff,rgba(255,255,255,0)),hsl(${pkH},100%,50%)`;
  document.getElementById('svcur').style.left=(pkS*w)+'px';document.getElementById('svcur').style.top=((1-pkV)*h)+'px';document.getElementById('huecur').style.top=((pkH/360)*hh)+'px';drawMask();}
function pkReadout(h){const e=document.getElementById('pkhex');if(e)e.textContent=h;const c=document.getElementById('pkcon');if(c){const r=contrast(h,MAP['bg']);c.textContent=r.toFixed(1)+'  '+rating(r);c.style.color=ratingColor(r);}
 const o=document.getElementById('pkoklch');if(o){const lch=oklab2oklch(srgb2oklab(h));o.textContent='OKLCH '+lch.L.toFixed(3)+' '+lch.C.toFixed(3)+' '+Math.round(lch.H)+'\u00b0';}
 const a=document.getElementById('pkapca');if(a){const lc=apca(h,MAP['bg']);a.textContent='APCA Lc '+lc.toFixed(0);a.title='APCA Lc '+lc.toFixed(1)+' (APCA-W3 0.1.9), text on the ground color. Positive = dark text on a light background, negative = light text on a dark background.';}}
function previewPickerHex(hex){if(pickerOn&&selectedIdx!==null)previewSelectedChip(hex);}
function syncHex(){const v=normHex(document.getElementById('newhexstr').value);if(!v)return;document.getElementById('swatch').style.background=v;[pkH,pkS,pkV]=rgb2hsv(...hex2rgb(v));if(pickerOn)paintPicker();pkReadout(v);previewPickerHex(v);}
function setHex(h){h=normHex(h)||h;document.getElementById('newhexstr').value=h;document.getElementById('swatch').style.background=h;[pkH,pkS,pkV]=rgb2hsv(...hex2rgb(h));if(pickerOn)paintPicker();pkReadout(h);previewPickerHex(h);}
function pkSet(){const hex=rgb2hex(...hsv2rgb(pkH,pkS,pkV));document.getElementById('newhexstr').value=hex;document.getElementById('swatch').style.background=hex;paintPicker();pkReadout(hex);previewPickerHex(hex);if(pkModel==='oklch')oklchInputsFromHex(hex);}
// --- OKLCH editing model (Phase 4a): L/C/H dials orthogonal to the HSV square ---
function setOklchInputs(L,C,H){
  const put=(id,v)=>{const e=document.getElementById(id);if(e)e.value=v;};
  put('okL',L.toFixed(3));put('okLn',L.toFixed(3));put('okC',C.toFixed(3));put('okCn',C.toFixed(3));
  const h=String(Math.round(H));put('okH',h);put('okHn',h);}
function oklchInputsFromHex(hex){const lch=oklab2oklch(srgb2oklab(normHex(hex)||DEFAULT_PICKER_HEX));setOklchInputs(lch.L,lch.C,lch.H);}
function readOklch(){return [parseFloat(document.getElementById('okL').value)||0,parseFloat(document.getElementById('okC').value)||0,parseFloat(document.getElementById('okH').value)||0];}
function pkClampStatus(on){const s=document.getElementById('pkclamp');if(!s)return;s.classList.toggle('show',on);s.textContent=on?'chroma clamped to sRGB':'';}
function pkOklchSet(){const [L,C,H]=readOklch();const {hex,clamped}=oklch2hex(L,C,H);
  document.getElementById('newhexstr').value=hex;document.getElementById('swatch').style.background=hex;
  [pkH,pkS,pkV]=rgb2hsv(...hex2rgb(hex));paintPicker();pkReadout(hex);previewPickerHex(hex);
  if(clamped)oklchInputsFromHex(hex); // snap the dials to the reachable color
  pkClampStatus(clamped);}
function setPkModel(m){pkModel=m;document.querySelectorAll('.pmodel button').forEach(x=>x.classList.toggle('on',x.dataset.pm===m));
  const oc=document.getElementById('oklchctl');if(oc)oc.classList.toggle('show',m==='oklch');
  if(m==='oklch')oklchInputsFromHex(curHex());else pkClampStatus(false);}
function buildPkChips(){const c=document.getElementById('pkchips');if(!c)return;c.innerHTML='';const T=pkThresh();PALETTE.forEach(([hex,name])=>{const s=document.createElement('div');s.className='pc';s.style.background=hex;s.title=name+' '+hex;const ok=!T||contrast(hex,MAP['bg'])>=T;if(!ok){s.style.opacity='0.22';s.title+=' (below '+pkMode.toUpperCase()+')';}s.onclick=()=>{if(ok)setHex(hex);};c.appendChild(s);});}
function openPicker(){pickerOn=true;[pkH,pkS,pkV]=rgb2hsv(...hex2rgb(curHex()));buildPkChips();document.getElementById('picker').style.display='block';setPkModel(pkModel);paintPicker();pkReadout(curHex());previewPickerHex(curHex());setTimeout(()=>document.addEventListener('pointerdown',pkOutside),0);}
function closePicker(){if(!pickerOn)return;restoreSelectedChip();pickerOn=false;const p=document.getElementById('picker');if(p)p.style.display='none';document.removeEventListener('pointerdown',pkOutside);}
function pkOutside(e){if(!e.target.closest('#picker')&&!e.target.closest('#swatch'))closePicker();}
function pkDrag(el,fn){el.addEventListener('pointerdown',e=>{e.preventDefault();fn(e);const mv=ev=>fn(ev),up=()=>{document.removeEventListener('pointermove',mv);document.removeEventListener('pointerup',up);};document.addEventListener('pointermove',mv);document.addEventListener('pointerup',up);});}
function initPicker(){const sw=document.getElementById('swatch');if(!sw)return;sw.style.background=curHex();sw.onclick=()=>pickerOn?closePicker():openPicker();
  const sf=document.getElementById('safefor');if(sf&&sf.options.length<=1)COVERED_FACES.forEach(f=>{const o=document.createElement('option');o.value=f;o.textContent=f;sf.appendChild(o);});
  pkDrag(document.getElementById('sv'),e=>{const r=document.getElementById('sv').getBoundingClientRect();const fx=Math.max(0,Math.min(1,(e.clientX-r.left)/r.width)),fy=Math.max(0,Math.min(1,(e.clientY-r.top)/r.height));
    if(pkModel==='oklch'){setOklchInputs(1-fy,fx*OKLCH_CMAX,readOklch()[2]);pkOklchSet();}else{pkS=fx;pkV=1-fy;pkSet();}});
  pkDrag(document.getElementById('hue'),e=>{const r=document.getElementById('hue').getBoundingClientRect();const fy=Math.max(0,Math.min(1,(e.clientY-r.top)/r.height));
    if(pkModel==='oklch'){const [L,C]=readOklch();setOklchInputs(L,C,fy*360);pkOklchSet();}else{pkH=fy*360;pkSet();}});
  document.querySelectorAll('.pmode button').forEach(b=>b.onclick=()=>{pkMode=b.dataset.m;document.querySelectorAll('.pmode button').forEach(x=>x.classList.toggle('on',x===b));paintPicker();buildPkChips();});
  document.querySelectorAll('.pmodel button').forEach(b=>b.onclick=()=>setPkModel(b.dataset.pm));
  [['okL','okLn',3],['okC','okCn',3],['okH','okHn',0]].forEach(([r,n,dp])=>{
    const re=document.getElementById(r),ne=document.getElementById(n);
    if(re)re.addEventListener('input',()=>{if(ne)ne.value=(+re.value).toFixed(dp);pkOklchSet();});
    if(ne)ne.addEventListener('input',()=>{if(re)re.value=ne.value;pkOklchSet();});});}
function addColor(){const h=curHex();const name=document.getElementById('newname').value.trim();
  if(!name){notify('name the color before adding it',true);return;}
  if(PALETTE.some(p=>p[1].toLowerCase()===name.toLowerCase())){notify('a color named "'+name+'" already exists — select it and use Update selected to change its value',true);return;}
  PALETTE.push([h,name,columnIdOf([h,name])]);const healed=healGone(name,h);document.getElementById('newname').value='';selectedIdx=null;GEN_SELECTION=null;closePicker();
  refreshPaletteState({code:healed,ground:healed,pkgPreview:healed});
  renderGeneratorPreview();
  notify(healed?('added "'+name+'" and reconnected its face references'):('added "'+name+'"'),false);}
function themeName(){return (document.getElementById('themename').value||'theme').trim()||'theme';}
function fileSlug(){return slugify(themeName());}
function exportObj(){normalizePalette();const o={name:themeName(),palette:PALETTE,syntax:SYNTAX,ui:UIMAP};if(LOCKED.size)o.locks=[...LOCKED];const pk=packagesForExport(PKGMAP);if(Object.keys(pk).length)o.packages=pk;return o;}
function exportState(){const t=document.getElementById('export');t.value=JSON.stringify(exportObj(),null,1);t.style.display='block';t.focus();t.select();}
function toggleJSON(){const t=document.getElementById('export'),b=document.getElementById('jsonbtn');if(t.style.display==='block'){t.style.display='none';b.textContent='show';}else{exportState();b.textContent='hide';}}
function updateTitle(){const n=document.getElementById('themename').value.trim();document.getElementById('pagetitle').textContent=(n||'Untitled')+': theme';}
// Export the theme JSON. Prefer the File System Access API (showSaveFilePicker)
// so re-exporting overwrites the chosen file in place -- a blob download routes
// through the browser's downloads folder, which uniquifies a re-save as
// "name (1).json" rather than replacing it. Fall back to the blob download where
// the API is absent (mirrors importTheme's showOpenFilePicker/fileinput fallback).
async function exportTheme(){
  const data=JSON.stringify(exportObj(),null,1);
  if(!window.showSaveFilePicker){const blob=new Blob([data],{type:'application/json'});const a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download=fileSlug()+'.json';a.click();return;}
  try{const h=await window.showSaveFilePicker({suggestedName:fileSlug()+'.json',types:[{description:'theme JSON',accept:{'application/json':['.json']}}]});
    const w=await h.createWritable();await w.write(data);await w.close();
    notify('saved "'+fileSlug()+'.json"',false);
  }catch(e){if(e&&e.name!=='AbortError')notify('export failed: '+e.message,true);}}
function applyImported(text){const d=JSON.parse(text);lastGone={};if(d.name)document.getElementById('themename').value=d.name;if(d.palette)PALETTE=d.palette.map(normalizePaletteEntry);
  if(!d.syntax)throw new Error('theme JSON is missing syntax; convert older files first');
  SYNTAX={};CATS.forEach(c=>{const k=c[0];SYNTAX[k]=Object.assign(syntaxBlank(k),migrateLegacyFace(d.syntax[k]||{}));});syncAllSyntaxCache();
  LOCKED=new Set(d.locks||[]);
  if(d.ui)for(const k in d.ui)UIMAP[k]=Object.assign(uiFaceBlank(),migrateLegacyFace(d.ui[k]));
  PKGMAP=seedPkgmap();if(d.packages)mergePackagesInto(PKGMAP,d.packages);
  refreshPaletteState({pkgPreview:true});updateTitle();}
function importFile(ev){const f=ev.target.files[0];if(!f)return;const r=new FileReader();
  r.onload=()=>{try{applyImported(r.result);updateTitle();}catch(e){alert('bad theme file: '+e.message);}};
  r.readAsText(f);ev.target.value='';}
async function importTheme(){
  if(!window.showOpenFilePicker){const fi=document.getElementById('fileinput');if(fi)fi.click();return;}
  try{const [h]=await window.showOpenFilePicker({types:[{description:'theme JSON',accept:{'application/json':['.json']}}]});
    const file=await h.getFile();applyImported(await file.text());updateTitle();
    notify('imported "'+(themeName()||file.name)+'"',false);
  }catch(e){if(e&&e.name!=='AbortError')notify('import failed: '+e.message,true);}}
// The blanket covers only the code panes and syntax example cells. UI-face
// preview cells also carry .ex, but a face with its own bg must keep it, so
// those rows repaint through paintUI (which also re-rates the contrast cell
// against the new ground for faces without their own bg).
function applyGround(){document.querySelectorAll('pre').forEach(p=>p.style.background=MAP['bg']);UI_FACES.forEach(([f])=>{if(document.getElementById('uiprev-'+f))paintUI(f);});}
function uf(f){return UIMAP[f]||{};}
// Map a weight name to a CSS font-weight for the live previews. The named
// weights light/medium/semibold/heavy aren't CSS keywords, so resolve to the
// numeric scale; an unset weight renders normal.
// cssWeight, boxCss, faceDecoration, and faceCss live in app-core.js now.
// udeco keeps its own (untrimmed) decoration form, so it stays here.
function udeco(o){return 'font-weight:'+cssWeight(o.weight)+';font-style:'+(o.slant||'normal')+';text-decoration:'+((o.underline?'underline ':'')+(o.strike?'line-through':'')||'none');}
function syntaxStyle(k){const s=syntaxFace(k),fg=(k==='bg'?MAP['p']:resolveSyntaxFg(k,SYNTAX,MAP['p'])),bg=s.bg||null;return faceCss(s,fg,bg,{boxBg:bg||MAP['bg']});}
// The per-row box control: none / line / raised / pressed plus optional line
// color. get()/set() read and write the face's box object (null = no box).
// Box control: a 2x2 cluster of radio buttons for the four box styles (no box /
// line / pressed / raised), plus a compact color swatch shown only while a box
// style is active. Replaces the old wide select+swatch to reclaim column width.
// Box control: a 2x2 cluster of the four box styles (no box / line / pressed /
// raised) plus a compact color swatch shown while a style is active. Shares the
// cluster/dropdown/paint machinery with mkLineStyleControl; it differs only in
// that its state object carries `width`, so it passes a toState builder.
function mkBoxControl(get,set,opts={}){
  return mkLineStyleControl(
    [['','no box',''],['line','line box','□'],['pressed','pressed','▼'],['released','raised','▲']],
    get,set,
    Object.assign({styled:true,toState:(v,cur)=>({style:v,width:(cur&&cur.width)||1,color:(cur&&cur.color)||null})},opts));}
function flashRow(tr){if(!tr)return;tr.scrollIntoView({block:'center',behavior:'smooth'});tr.classList.remove('flash');void tr.offsetWidth;tr.classList.add('flash');}
// Unified preview-locate click dispatch (preview-locate spec, Phases 4-5). One
// handler for every preview surface replaces the per-surface data-face branches:
// find the clicked data-face element, resolve its owner (data-owner-app, or
// DEFAULTOWNER for a bare span emitted by the generic / auto-dim / UI-mock
// renderers that pre-date previewSpan), and flash its assignment row only when it
// is on-pane. An owner-tagged off-pane / unassigned element is inert; a bare span
// is a current-pane element by construction, so it stays clickable. No persistent
// selection — flashRow is scroll + flash only. The data-k syntax-click path stays
// separate (handled by each caller before delegating here).
function locateClick(e,defaultOwner){
  const u=e.target.closest('[data-face]');if(!u)return;
  if(u.dataset.ownerApp&&!u.classList.contains('locate-onpane'))return;
  const owner=u.dataset.ownerApp||defaultOwner;
  if(owner==='@ui')flashUi(u.dataset.face);else flashPkg(u.dataset.face);
}
function flashEl(el){if(!el)return;el.scrollIntoView({block:'nearest',inline:'nearest',behavior:'smooth'});el.classList.remove('flashtok');void el.offsetWidth;el.classList.add('flashtok');}
// Flash every matching element but scroll only the first into view, so a face
// that maps to several preview spans still lands the viewport on the first.
function flashEls(els){els=[...els];if(!els.length)return;els[0].scrollIntoView({block:'nearest',inline:'nearest',behavior:'smooth'});els.forEach(el=>{el.classList.remove('flashtok');void el.offsetWidth;el.classList.add('flashtok');});}
function flashTokens(kind){const sp=document.querySelectorAll('#codepre [data-k="'+kind+'"]');if(sp.length){flashEls(sp);return;}const row=document.querySelector('#legbody tr[data-kind="'+kind+'"]');if(row)flashEl(row.querySelector('.ex'));}
function flashAssign(k){flashRow(document.querySelector(`#legbody tr[data-kind="${k}"]`));}
function flashUi(f){flashRow(document.querySelector(`#uibody tr[data-face="${f}"]`));}
function flashUiPreview(f){const sp=document.querySelectorAll(`#mockframe [data-face="${f}"]`);if(sp.length){flashEls(sp);return;}const cell=document.getElementById('uiprev-'+f);if(cell)flashEl(cell);}
function flashPkg(f){flashRow(document.querySelector(`#pkgbody tr[data-face="${f}"]`));}
function flashPkgPreview(f){const sp=document.querySelectorAll(`#pkgpreview [data-face="${f}"]`);if(sp.length){flashEls(sp);return;}const row=document.querySelector(`#pkgbody tr[data-face="${f}"]`);if(row)flashEl(row.querySelector('.cat'));}
function mockSpan(k,t){return `<span data-k="${k}" style="${syntaxStyle(k)}">${esc(t)}</span>`;}
function uiCss(o,fgv,bgv,opts={}){const fg=fgv===undefined?effFg(o.fg):fgv,bg=bgv===undefined?o.bg:bgv;return faceCss(o,fg,bg,{noBg:opts.noBg,fontSize:heightCssValue(o),boxBg:bg||MAP['bg']});}
// Size a preview pane to its faces table, minus the label bar above it. Shared by
// the UI mock and the package preview, which differ only in their element IDs.
function syncPaneHeight(tableId,paneId){const t=document.getElementById(tableId),m=document.getElementById(paneId);if(!t||!m)return;const lb=m.previousElementSibling,lbh=lb?lb.getBoundingClientRect().height+10:30;m.style.height=Math.max(t.getBoundingClientRect().height-lbh,220)+'px';}
function buildMockFrame(){
  const fr=document.getElementById('mockframe');if(!fr)return;
  rebuildLocateRegistry();
  const bg=MAP['bg'],fg=MAP['p'];
  const ln=uf('line-number'),lnc=uf('line-number-current-line'),hl=uf('hl-line'),hil=uf('highlight'),reg=uf('region'),isr=uf('isearch'),isf=uf('isearch-fail'),laz=uf('lazy-highlight'),par=uf('show-paren-match'),parx=uf('show-paren-mismatch'),cur=uf('cursor'),ml=uf('mode-line'),mli=uf('mode-line-inactive'),mlh=uf('mode-line-highlight'),mb=uf('minibuffer-prompt'),frng=uf('fringe'),vb=uf('vertical-border'),lnk=uf('link'),err=uf('error'),wrn=uf('warning'),suc=uf('success');
  const lines=[
    {t:[['cmd',';; '],['cm','init.el - your config']]},
    {t:[['punc','('],['kw','require'],['p',' '],['con',"'cl-lib"],['punc',')']]},
    {t:[]},
    {t:[['punc','('],['kw','defun'],['p',' '],['fnd','cj/greet'],['p',' '],['punc','('],['var','name'],['punc',')']]},
    {t:[['p','  '],['punc','('],['fnc','message'],['p',' '],['str','"hi %s"'],['p',' '],['var','name'],['punc','))']],cur:1},
    {t:[['p','  '],['punc','('],['kw','setq'],['p',' '],['var','count'],['p',' '],['num','42'],['punc',')']],region:1},
    {t:[['p','  '],['punc','('],['kw','if'],['p',' '],['punc','('],['op','>'],['p',' '],['var','count'],['p',' '],['num','0'],['punc',')']],match:1},
    {t:[['p','    '],['punc','('],['kw','setq'],['p',' '],['var','total'],['p',' '],['punc','('],['op','+'],['p',' '],['var','total'],['p',' '],['var','count'],['punc','))']],hl:1},
    {t:[['p','      '],['punc','('],['fnc','process'],['p',' '],['var','highlights'],['punc',')']],cont:1,high:1},
    {t:[['p','    '],['punc','('],['fnc','cl-incf'],['p',' '],['var','count'],['punc',')']],lazy:1},
    {t:[['p','  '],['punc','('],['kw','setq'],['p',' '],['var','done'],['p',' '],['con','t'],['punc',')']],paren:1},
    {t:[['p','    '],['punc','('],['fnc','oops'],['p',' '],['var','nested'],['punc','))']],mismatch:1}
  ];
  // An overlay face (region, highlight, isearch, lazy-highlight) merges the way
  // Emacs does: its background applies, and its foreground overrides the tokens
  // only when set — otherwise the underlying syntax colors show through.
  const overlay=(tokens,face,dface)=>{
    const inner=face.fg
      ? `<span style="color:${face.fg}">${tokens.map(([,t])=>esc(t)).join('')}</span>`
      : tokens.map(([k,t])=>mockSpan(k,t)).join('');
    return `<span data-face="${dface}" style="${uiCss(face,face.fg||'inherit',face.bg||'transparent')}">${inner}</span>`;
  };
  // Emacs box cursor: it sits on the character at point, drawn in the configured
  // cursor background, with the glyph in the configured cursor foreground.
  // Falls back to a trailing block only if the line has no glyph (point at EOL).
  const withCursor=(tokens)=>{
    let out='',placed=false;
    const cell=ch=>`<span data-face="cursor" style="${uiCss(cur,cur.fg||bg,cur.bg||fg)}">${esc(ch)}</span>`;
    for(const [k,t] of tokens){
      const m=placed?-1:t.search(/\S/);
      if(m>=0){
        if(m>0)out+=mockSpan(k,t.slice(0,m));
        out+=cell(t[m]);
        if(t.length>m+1)out+=mockSpan(k,t.slice(m+1));
        placed=true;
      } else out+=mockSpan(k,t);
    }
    if(!placed)out+=cell(' ');
    return out;
  };
  let buf='';
  lines.forEach((L,i)=>{
    const isc=L.cur;
    const nFg=isc?(resolveUiAttr('line-number-current-line','fg',UIMAP)||fg):(ln.fg||fg), nBg=isc?(resolveUiAttr('line-number-current-line','bg',UIMAP)||'transparent'):(ln.bg||'transparent');
    const rowFace=isc?hl:null,rowStyle=rowFace?uiCss(rowFace,rowFace.fg||'inherit',rowFace.bg||'transparent'):'background:transparent';
    let cd;
    if(isc)cd=withCursor(L.t);
    else if(L.region)cd=overlay(L.t,reg,'region');
    else if(L.high)cd=overlay(L.t,hil,'highlight');
    else if(L.match)cd=overlay(L.t,isr,'isearch');
    else if(L.lazy)cd=overlay(L.t,laz,'lazy-highlight');
    else if(L.hl)cd=overlay(L.t,hl,'hl-line');
    else if(L.paren)cd=L.t.map(([k,t],j)=>j===L.t.length-1?`<span data-face="show-paren-match" style="${uiCss(par,par.fg||syntaxFace(k).fg||fg,par.bg||'transparent')}">${esc(t)}</span>`:mockSpan(k,t)).join('');
    else if(L.mismatch)cd=L.t.map(([k,t],j)=>{if(j!==L.t.length-1)return mockSpan(k,t);const head=t.slice(0,-1),bad=t.slice(-1);return (head?mockSpan(k,head):'')+`<span data-face="show-paren-mismatch" style="${uiCss(parx,parx.fg||syntaxFace(k).fg||fg,parx.bg||'transparent')}">${esc(bad)}</span>`;}).join('');
    else cd=L.t.map(([k,t])=>mockSpan(k,t)).join('');
    const nFace=isc?'line-number-current-line':'line-number';
    buf+=`<div class="ln" ${rowFace?'data-face="hl-line" ':''}style="${rowStyle}"><span class="fr" data-face="fringe" style="${uiCss(frng,frng.fg||fg,frng.bg||bg)};text-align:center;font-size:10px;overflow:hidden" title="fringe">${L.cont?'&#8618;':''}</span><span class="num" data-face="${nFace}" style="${uiCss(isc?lnc:ln,nFg,nBg)}">${i+1}</span><span class="cd">${cd||'&nbsp;'}</span></div>`;
  });
  let html=`<div class="mbuf" style="background:${bg}"><div class="mbuftext">${buf}</div><div class="vborder" data-face="vertical-border" title="vertical-border" style="background:${vb.fg||vb.bg||'#2f343a'}"></div></div>`;
  const mlhStyle=uiCss(mlh,mlh.fg||ml.fg||bg,mlh.bg||ml.bg||fg);
  html+=`<div class="bar" data-face="mode-line" style="${uiCss(ml,ml.fg||bg,ml.bg||fg)}">  init.el      (Emacs Lisp)      L5      <span data-face="mode-line-highlight" title="mode-line-highlight (hover)" style="${mlhStyle}">git:main</span>  </div>`;
  html+=`<div class="bar" data-face="mode-line-inactive" style="${uiCss(mli,resolveUiAttr('mode-line-inactive','fg',UIMAP)||fg,resolveUiAttr('mode-line-inactive','bg',UIMAP)||bg)}">  *Messages*      (Fundamental)</div>`;
  html+=`<div class="echo" style="color:${fg}"><span data-face="minibuffer-prompt" style="${uiCss(mb,mb.fg||fg,mb.bg||null)}">I-search:</span> count   <span data-face="isearch-fail" style="${uiCss(isf,isf.fg||fg,isf.bg||'transparent')}">zzz [no match]</span></div>`;
  html+=`<div class="echo"><span data-face="link" style="${uiCss(lnk,lnk.fg||fg,lnk.bg||null)}">https://gnu.org</span>   <span data-face="error" style="${uiCss(err,err.fg||fg,err.bg||null)}">error</span>   <span data-face="warning" style="${uiCss(wrn,wrn.fg||fg,wrn.bg||null)}">warning</span>   <span data-face="success" style="${uiCss(suc,suc.fg||fg,suc.bg||null)}">ok</span></div>`;
  fr.innerHTML=html;fr.style.background=bg;fr.style.color=fg;
  fr.onclick=(e)=>{if(e.target.closest('[data-face]')){locateClick(e,'@ui');return;}const k=e.target.closest('[data-k]');if(k)flashAssign(k.dataset.k);};
}
// All three tiers share one dropdown — the swatch div from mkColorDropdown. The
// native <select> rendered swatch colors unreliably on Linux Chrome, so it is
// gone. '' (the default entry) maps back to null in the stored model.
function uiSelect(face,attr){const cur=UIMAP[face][attr]||'';
  return mkColorDropdown(ddList(cur),cur,h=>{UIMAP[face][attr]=h||null;paintUI(face);buildMockFrame();},{compact:true,defaultHex:attr==='fg'?effFg(null):effBg(null)});}
const BASE_INHERITS=['fixed-pitch','variable-pitch','default','link','bold','italic','shadow'];
function uiFaceBlank(){return {fg:null,bg:null,'distant-fg':null,family:null,weight:null,slant:null,underline:null,strike:null,overline:null,box:null,inverse:false,extend:false,inherit:null,height:null,heightMode:null};}
function seedFace(d){return normalizePkgFace({fg:pname(d.fg),bg:pname(d.bg),'distant-fg':pname(d['distant-fg']),family:d.family,weight:d.weight,slant:d.slant,bold:d.bold,italic:d.italic,underline:d.underline,strike:d.strike,overline:d.overline,inherit:d.inherit,height:d.height,heightMode:d.heightMode,box:d.box,inverse:d.inverse,extend:d.extend},'default');}
function curApp(){const s=document.getElementById('viewsel');const v=s&&s.value;return (v&&v[0]!=='@')?v:Object.keys(APPS)[0];}
function pkgEffFg(app,face,seen){return effResolve(PKGMAP,app,face,'fg',seen);}
function pkgEffBg(app,face,seen){return effResolve(PKGMAP,app,face,'bg',seen);}
// One dropdown drives the whole assignment panel: two editor entries (@code,
// @ui) then a non-selectable "package faces" optgroup holding every app,
// alphabetically by label. onViewChange shows exactly one of the three view blocks.
// Lock keys for one view value (@code / @ui / a package app), so the view
// dropdown can flag a view whose every element is locked.
function viewLockKeys(v){
  if(v==='@code')return syntaxLockKeys();
  if(v==='@ui')return uiLockKeys();
  return (APPS[v]?APPS[v].faces:[]).map(f=>'pkg:'+v+':'+f[0]);
}
// Prefix a lock glyph on every view whose elements are all locked; leave the rest
// bare. The base label rides in dataset.label so re-running never stacks glyphs.
function updateViewLockIndicators(){const s=document.getElementById('viewsel');if(!s)return;
  for(const o of s.querySelectorAll('option')){const base=o.dataset.label||o.textContent;
    o.textContent=(areAllLocked(viewLockKeys(o.value),LOCKED)?'🔒 ':'')+base;}}
function buildViewSel(){const s=document.getElementById('viewsel');if(!s)return;s.innerHTML='';
  const mk=(v,t,ti)=>{const o=document.createElement('option');o.value=v;o.dataset.label=t;o.textContent=t;if(ti)o.title=ti;return o;};
  s.appendChild(mk('@code','color/code assignments'));
  s.appendChild(mk('@ui','ui faces'));
  const og=document.createElement('optgroup');og.label='package faces';
  for(const app of appViewKeysSorted(APPS))og.appendChild(mk(app,APPS[app].label,APPS[app].hover));
  s.appendChild(og);updateViewLockIndicators();}
// The ‹ › buttons flanking the dropdown step the selection by DIR and re-render
// the view (faces table + preview), so you can walk the list without reopening it.
function stepView(dir){
  const s=document.getElementById('viewsel');if(!s)return;
  const i=stepViewIndex(s.selectedIndex,s.options.length,dir);
  if(i!==s.selectedIndex){s.selectedIndex=i;onViewChange();}
}
// The ‹ › buttons flanking the language dropdown step the selection by DIR and
// re-render the code sample + package preview, mirroring the view-dropdown nav.
function stepLang(dir){
  const s=document.getElementById('langsel');if(!s)return;
  const i=stepViewIndex(s.selectedIndex,s.options.length,dir);
  if(i!==s.selectedIndex){s.selectedIndex=i;renderCode();buildPkgPreview();}
}
function onViewChange(){const s=document.getElementById('viewsel');const v=(s&&s.value)||'@code';
  const show=(id,on)=>{const e=document.getElementById(id);if(e)e.style.display=on?'':'none';};
  show('view-code',v==='@code');show('view-ui',v==='@ui');show('view-pkg',v[0]!=='@');
  if(s)s.title=(v[0]!=='@'&&APPS[v]&&APPS[v].hover)?APPS[v].hover:'';
  if(v==='@code')renderCode();
  else if(v==='@ui'){buildUITable();buildMockFrame();syncPaneHeight('uitable','mockframe');}
  else pkgChanged();}
function pkgChanged(){buildPkgTable();buildPkgPreview();syncPaneHeight('pkgtable','pkgpreview');}
function buildPkgTable(){
  const app=curApp(),tb=document.getElementById('pkgbody');if(!tb)return;tb.innerHTML='';
  const flt=(document.getElementById('pkgfilter').value||'').trim().toLowerCase();
  const inh=[''].concat(BASE_INHERITS).concat(APPS[app].faces.map(r=>r[0]));
  for(const row of APPS[app].faces){
    const face=row[0],label=row[1];
    if(flt&&!(face.toLowerCase().includes(flt)||label.toLowerCase().includes(flt)))continue;
    const f=PKGMAP[app][face],tr=document.createElement('tr');tr.dataset.face=face;
    const def=normalizePkgFace(row[2]||{},'default',PALETTE);
    const nd=faceBoxNonDefaults(
      {fg:nameToHex(f.fg,PALETTE),bg:nameToHex(f.bg,PALETTE),weight:f.weight,slant:f.slant,underline:f.underline,strike:f.strike,inherit:f.inherit,height:f.height,box:f.box},
      {fg:nameToHex(def.fg,PALETTE),bg:nameToHex(def.bg,PALETTE),weight:def.weight,slant:def.slant,underline:def.underline,strike:def.strike,inherit:def.inherit,height:def.height,box:def.box});
    const exp=mkExpander(f,tableColCount('pkgtable'),()=>{f.source='user';pkgChanged();},{expandKey:face,showInherit:true,inheritOptions:inh,defaultHex:effFg(pkgEffFg(app,face)),ndCheck:()=>overflowNonDefault(f,def,true)});
    exp.detail.dataset.detailFor=face;
    const c0=document.createElement('td');c0.className='cat';c0.title=composeHoverTitle(FACE_DOCS[face],face);c0.appendChild(exp.btn);
    const c0lbl=document.createElement('span');c0lbl.textContent=' '+label;c0lbl.style.cursor='pointer';c0lbl.onclick=()=>flashPkgPreview(face);c0.appendChild(c0lbl);
    const fgd=mkColorDropdown(ddList(f.fg||''),f.fg||'',h=>{f.fg=h||null;f.source='user';pkgChanged();},{compact:true,defaultHex:effFg(pkgEffFg(app,face))}),
          bgd=mkColorDropdown(ddList(f.bg||''),f.bg||'',h=>{f.bg=h||null;f.source='user';pkgChanged();},{compact:true,defaultHex:effBg(pkgEffBg(app,face))});
    const cf=document.createElement('td');cf.appendChild(fgd);
    const cb=document.createElement('td');cb.appendChild(bgd);
    const cw=document.createElement('td');
    const pkCtls=mkStyleControls(f,()=>{f.source='user';pkgChanged();},{defaultHex:effFg(pkgEffFg(app,face))});
    const pkCluster=document.createElement('div');pkCluster.className='stylecluster';pkCtls.forEach(c=>pkCluster.appendChild(c));cw.appendChild(pkCluster);
    const cc=document.createElement('td');cc.style.fontSize='10pt';cc.style.whiteSpace='nowrap';const efg=effFg(pkgEffFg(app,face)),ebg=effBg(pkgEffBg(app,face)),r=contrast(efg,ebg);cc.innerHTML=crHtml(r);
    const cx=document.createElement('td');const boxCtl=mkBoxControl(()=>f.box,b=>{f.box=b;f.source='user';pkgChanged();},{compact:true});cx.appendChild(boxCtl);
    const cH=document.createElement('td');cH.className='sizecell';
    const hk=heightControlKind(face,f,def);let hCtl=null;
    if(hk){hCtl=mkHeightControl(f,hk,()=>{f.source='user';pkgChanged();});cH.appendChild(hCtl.el);
      if((f.height||1)!==(def.height||1))cH.classList.add('nd');}
    else{const na=document.createElement('span');na.textContent='\u2014';na.style.opacity='0.4';na.title='no height control: this face inherits its size';cH.appendChild(na);}
    const cL=mkLockCell('pkg:'+app+':'+face,[fgd,bgd,...pkCtls,boxCtl,...(hCtl?hCtl.controls:[]),...exp.locks]);
    if(nd.fg)cf.classList.add('nd');if(nd.bg)cb.classList.add('nd');if(nd.style)cw.classList.add('nd');
    if(nd.box)cx.classList.add('nd');
    tr.append(cL,c0,cf,cb,cw,cx,cc,cH);tb.appendChild(tr);tb.appendChild(exp.detail);
  }
  applyTableSort('pkgbody');
  updateLockToggle('pkg');syncExpandAllBtns();
}
// The per-package preview renderers live in previews.js, spliced here so the
// PACKAGE_PREVIEWS registry below can reference them.
PREVIEWS_J
const PACKAGE_PREVIEWS={
  autodim:renderAutodimPreview,markdown:renderMarkdownPreview,
  org:renderOrgPreview,magit:renderMagitPreview,elfeed:renderElfeedPreview,eat:renderEatPreview,
  dashboard:renderDashboardPreview,mu4e:renderMu4ePreview,gnus:renderGnusPreview,orgfaces:renderOrgFacesPreview,lsp:renderLspPreview,gitgutter:renderGitGutterPreview,
  flycheck:renderFlycheckPreview,dired:renderDiredPreview,dirvish:renderDirvishPreview,calibredb:renderCalibredbPreview,
  novreading:renderNovReadingPreview,aiterm:renderAiTermPreview,
  company:renderCompanyPreview,companybox:renderCompanyBoxPreview,transient:renderTransientPreview,
  magitsection:renderMagitSectionPreview,rainbowdelims:renderRainbowDelimitersPreview,
  webmode:renderWebModePreview,
  vertico:renderVerticoPreview,marginalia:renderMarginaliaPreview,consult:renderConsultPreview,
  embark:renderEmbarkPreview,orderless:renderOrderlessPreview,
  emms:renderEmmsPreview,orgroam:renderOrgRoamPreview,hltodo:renderHlTodoPreview,
  symboloverlay:renderSymbolOverlayPreview,
  indentguides:renderIndentGuidesPreview,yasnippet:renderYasnippetPreview,
  prescient:renderPrescientPreview,flyspellcorrect:renderFlyspellCorrectPreview,
  tmr:renderTmrPreview,wttrin:renderWttrinPreview,alert:renderAlertPreview,
  orgsuperstar:renderOrgSuperstarPreview,nerdiconscompletion:renderNerdIconsCompletionPreview,
  ghostel:renderGhostelPreview,alltheicons:renderAllTheIconsPreview,
  ansicolor:renderAnsiColorPreview,
  erc:renderErcPreview,orgdrill:renderOrgdrillPreview,orgnoter:renderOrgnoterPreview,signel:renderSignelPreview,
  pearl:renderPearlPreview,slack:renderSlackPreview,telega:renderTelegaPreview,shr:renderShrPreview,
  nerdicons:renderNerdIconsPreview
};
// Preview panes for an app. Most apps have a single pane (the dropdown shows its
// name and is disabled). nerd-icons is the one multi-pane app: one pane per font
// size, so the designer can view the icon grid at different sizes — pt because
// Emacs sizes fonts in :height (1/10 pt), so a pane maps to a real buffer size.
const NERD_ICON_SIZES_PT=[10,12,14,16,20,24,32,48];
const NERD_ICON_DEFAULT_PT=14;
function previewPanes(app){
  // Multi-pane only when nerd-icons actually has a gallery to size. If the gallery
  // capture failed (nerd-icons absent, alists changed, empty), the grid renderer
  // falls back to the generic preview, so offering size panes would be a lie — the
  // dropdown collapses to one pane and is disabled.
  if(app==='nerd-icons'&&APPS[app]&&Array.isArray(APPS[app].gallery)&&APPS[app].gallery.length)
    return NERD_ICON_SIZES_PT.map(pt=>({label:'nerd-icons — '+pt+' pt',size:pt}));
  return [{label:PACKAGE_PREVIEWS[APPS[app].preview]?APPS[app].label:'generic (face names in their own colors)'}];
}
function defaultPaneIdx(app){
  if(app==='nerd-icons')return Math.max(0,NERD_ICON_SIZES_PT.indexOf(NERD_ICON_DEFAULT_PT));
  return 0;
}
// Per-app selected pane index, so a chosen size survives edits and revisits.
const PREV_PANE={};
// The ‹ › buttons flanking the preview dropdown (and the Left/Right arrows) step the
// pane by DIR, clamped, and re-render. No-op on a disabled (single-pane) dropdown.
function stepPreviewPane(dir){
  const s=document.getElementById('pkgprevsel');if(!s||s.disabled)return;
  const i=stepViewIndex(s.selectedIndex,s.options.length,dir);
  if(i!==s.selectedIndex){PREV_PANE[curApp()]=i;buildPkgPreview();}
}
function buildPkgPreview(){
  const app=curApp(),p=document.getElementById('pkgpreview');if(!p)return;
  rebuildLocateRegistry();
  const panes=previewPanes(app);
  let idx=PREV_PANE[app];
  if(idx==null||idx>=panes.length){idx=defaultPaneIdx(app);PREV_PANE[app]=idx;}
  const pane=panes[idx],renderer=PACKAGE_PREVIEWS[APPS[app].preview];
  // A pane carrying a size is a nerd-icons size variant; render the grid at it.
  p.innerHTML=pane.size!=null?renderNerdIconsPreview(pane.size):(renderer?renderer():genericPreview(app));
  p.style.background=MAP['bg'];
  p.onclick=(e)=>locateClick(e,app);
  // The pane dropdown: disabled when there's only one pane (it just names the
  // preview), enabled when there are several (it selects which one shows).
  const sel=document.getElementById('pkgprevsel');
  if(sel){
    sel.innerHTML=panes.map((pn,i)=>`<option value="${i}">${esc(pn.label)}</option>`).join('');
    sel.value=String(idx);
    sel.disabled=panes.length<2;
    sel.onchange=()=>{PREV_PANE[app]=+sel.value;buildPkgPreview();};
    // Left/Right arrows step the panes when the dropdown is focused (Up/Down already
    // do, natively); re-grab + refocus the select, since the rebuild drops focus.
    sel.onkeydown=(e)=>{
      if(e.key!=='ArrowLeft'&&e.key!=='ArrowRight')return;
      e.preventDefault();
      stepPreviewPane(e.key==='ArrowRight'?1:-1);
      const s=document.getElementById('pkgprevsel');if(s)s.focus();
    };
    // The flanking ‹ › buttons follow the dropdown's enabled state.
    const pb=document.getElementById('pkgprevprev'),nb=document.getElementById('pkgprevnext');
    if(pb)pb.disabled=panes.length<2;
    if(nb)nb.disabled=panes.length<2;
  }
  // Per-element wayfinding rides each preview span's own hover title (face + value);
  // no separate info line.
}
function resetApp(){const app=curApp();for(const [face,,d] of APPS[app].faces)if(!LOCKED.has('pkg:'+app+':'+face))PKGMAP[app][face]=seedFace(d);pkgChanged();notify('reset editable '+app+' faces to package defaults',false);}
// --- worst-case readout for the covered overlay faces (spec Phase 4) ---------
// Default WCAG target for the worst-case verdict (AA). AAA is selectable.
let WORST_TARGET=4.5;
// The live v1 foreground set for a covered overlay face: the syntax-token colors
// (every assignable category except the ground) plus the default foreground.
function fgSetForFace(face){
  const syntaxAssignments=CATS.filter(c=>c[0]!=='bg'&&c[0]!=='p').map(c=>({role:c[0],name:c[1],hex:effFg(syntaxFace(c[0]).fg)}));
  return fgSetFor(face,{covered:COVERED_FACES,syntaxAssignments,defaultFg:MAP['p']});
}
function coveredContrastReport(face){
  if(uf(face).fg)return null;
  const r=fgSetForFace(face);
  if(r.reason==='out-of-scope')return null;
  if(r.reason==='empty'||!r.set.length)return {empty:true};
  const bg=effBg(uf(face).bg);
  const rows=r.set.map(f=>{
    const ratio=contrast(f.hex,bg);
    return {label:f.label,name:f.name||f.label,hex:f.hex,ratio,verdict:verdictFor(ratio,WORST_TARGET)};
  }).sort((a,b)=>a.ratio-b.ratio);
  return {bg,rows,worst:rows[0],failures:rows.filter(x=>x.ratio<WORST_TARGET)};
}
function failureTitle(report){
  if(!report||!report.failures||!report.failures.length)return '';
  const lines=['failing covered-text contrasts against '+report.bg+':'];
  report.failures.forEach(f=>lines.push(`${f.ratio.toFixed(1)} FAIL  ${f.label} (${f.name}) ${f.hex}`));
  return lines.join('\n');
}
// The worst-case contrast cell for a covered face: the floor over its foreground
// set against its effective background. Returns null for an out-of-scope face so
// the caller keeps the single-pair readout.
function worstCellHtml(face){
  const report=coveredContrastReport(face);
  if(report===null)return null;
  if(report.empty)return '<span title="this overlay has no syntax foreground set yet">no fg set</span>';
  return `<span style="color:${ratingColor(report.worst.ratio)}" title="${esc(failureTitle(report)||'all covered text clears '+WORST_TARGET.toFixed(1))}">${report.worst.ratio.toFixed(1)}</span>`;
}
// Repaint every covered overlay face (their floors depend on the syntax palette,
// so a syntax-color edit has to refresh them even though it doesn't rebuild the table).
function repaintCovered(){COVERED_FACES.forEach(f=>{if(UIMAP[f]&&document.getElementById('uicr-'+f))paintUI(f);});}
function paintUI(face){const pv=document.getElementById('uiprev-'+face);if(!pv)return;const o=UIMAP[face];pv.style.color=effFg(o.fg);pv.style.background=effBg(o.bg);pv.style.fontWeight=cssWeight(o.weight);pv.style.fontStyle=o.slant||'normal';pv.style.fontSize=heightCssValue(o)||'';pv.style.textDecoration=(o.underline?'underline ':'')+(o.strike?'line-through':'')||'none';pv.style.boxShadow=boxCss(o.box,effBg(o.bg));
  const report=coveredContrastReport(face);
  pv.title='';
  const cr=document.getElementById('uicr-'+face);if(cr){cr.title='';const wc=worstCellHtml(face);if(wc!==null){cr.title=report.empty?'this overlay has no syntax foreground set yet':(failureTitle(report)||'all covered text clears '+WORST_TARGET.toFixed(1));cr.innerHTML=wc;}else{const efg=effFg(o.fg),ebg=effBg(o.bg),r=contrast(efg,ebg);cr.innerHTML=crHtml(r);}}}
function buildUITable(){
  const tb=document.getElementById('uibody');tb.innerHTML='';
  for(const [face,label,ex] of UI_FACES){
    const tr=document.createElement('tr');tr.dataset.face=face;
    const exp=mkExpander(UIMAP[face],tableColCount('uitable'),()=>{paintUI(face);buildMockFrame();},{expandKey:face,showInherit:true,inheritOptions:[''].concat(BASE_INHERITS),defaultHex:effFg(UIMAP[face].fg),ndCheck:()=>overflowNonDefault(UIMAP[face],DEFAULT_UIMAP[face],true)});
    exp.detail.dataset.detailFor=face;
    const c0=document.createElement('td');c0.className='cat';c0.title=composeHoverTitle(FACE_DOCS[face],c0.title);c0.appendChild(exp.btn);
    const c0lbl=document.createElement('span');c0lbl.textContent=' '+label;c0lbl.style.cursor='pointer';c0lbl.title='flash this face in the live preview';c0lbl.onclick=()=>flashUiPreview(face);c0.appendChild(c0lbl);
    // Emacs draws the cursor as a rectangle: its fg colors the glyph sitting on
    // it and its bg is the cursor color, but weight/slant/underline/strike and
    // box are no-ops on it. Show only fg+bg for the cursor row; mute the rest.
    const cursorOnly=(face==='cursor');
    const naCell=t=>{const s=document.createElement('span');s.textContent='—';s.style.opacity='0.4';s.title=t;return s;};
    const fgSel=uiSelect(face,'fg'),bgSel=uiSelect(face,'bg');
    const cF=document.createElement('td');cF.appendChild(fgSel);
    const cB=document.createElement('td');cB.appendChild(bgSel);
    const cS=document.createElement('td');
    const stCtls=cursorOnly?[]:mkStyleControls(UIMAP[face],()=>{paintUI(face);buildMockFrame();},{defaultHex:effFg(UIMAP[face].fg)});
    if(cursorOnly){cS.appendChild(naCell('Emacs ignores weight/slant/underline/strike on the cursor face'));}
    else{const uiCluster=document.createElement('div');uiCluster.className='stylecluster';stCtls.forEach(c=>uiCluster.appendChild(c));cS.appendChild(uiCluster);}
    const cC=document.createElement('td');cC.id='uicr-'+face;cC.style.whiteSpace='nowrap';cC.style.fontSize='10pt';
    const cP=document.createElement('td');cP.className='ex';cP.id='uiprev-'+face;cP.textContent=ex;cP.style.padding='4px 10px';cP.style.borderRadius='4px';
    const cX=document.createElement('td');const boxCtl=cursorOnly?null:mkBoxControl(()=>UIMAP[face].box,b=>{UIMAP[face].box=b;paintUI(face);buildMockFrame();},{compact:true});
    if(cursorOnly){cX.appendChild(naCell('Emacs ignores the box attribute on the cursor face'));}else{cX.appendChild(boxCtl);}
    const cH=document.createElement('td');cH.className='sizecell';
    const hk=heightControlKind(face,UIMAP[face],DEFAULT_UIMAP[face]);let hCtl=null;
    if(hk){hCtl=mkHeightControl(UIMAP[face],hk,()=>{paintUI(face);buildMockFrame();});cH.appendChild(hCtl.el);
      if((UIMAP[face].height||1)!==((DEFAULT_UIMAP[face]&&DEFAULT_UIMAP[face].height)||1))cH.classList.add('nd');}
    else{cH.appendChild(naCell('no height control: this face inherits its size (chrome and seeded heading faces expose one)'));}
    const cL=mkLockCell('ui:'+face,(cursorOnly?[fgSel,bgSel]:[fgSel,bgSel,...stCtls,boxCtl]).concat(hCtl?hCtl.controls:[],exp.locks));
    tr.appendChild(cL);tr.appendChild(c0);tr.appendChild(cF);tr.appendChild(cB);tr.appendChild(cS);tr.appendChild(cX);tr.appendChild(cC);tr.appendChild(cH);tr.appendChild(cP);tb.appendChild(tr);tb.appendChild(exp.detail);paintUI(face);
  }
  applyTableSort('uibody');
  updateLockToggle('ui');syncExpandAllBtns();
}
// Generic header-click sort, shared by all three tables. Reads a swatch
// dropdown's value, a select value, a numeric input, or cell text (numeric when
// the text leads with a number, e.g. contrast or size). The UI and package
// tables remember the sort (applyTableSort runs on rebuild) so editing a row
// does not reset it; the syntax table sorts on click only.
let tableSort={};
function cellVal(td){if(!td)return '';const dd=td.querySelector('.cdd');if(dd)return (dd.dataset.val||'').toLowerCase();const s=td.querySelector('select');if(s)return s.value.toLowerCase();const i=td.querySelector('input');if(i)return parseFloat(i.value)||0;const t=td.innerText.trim();const n=parseFloat(t);return (!isNaN(n)&&/^[-\d.]/.test(t))?n:t.toLowerCase();}
function srtTable(tbId,col){tableSort[tbId]={col,asc:!(tableSort[tbId]&&tableSort[tbId].col===col&&tableSort[tbId].asc)};applyTableSort(tbId);}
function applyTableSort(tbId){const s=tableSort[tbId];if(!s)return;const tb=document.getElementById(tbId);if(!tb)return;const dir=s.asc?1:-1;
  // Sort only the main rows; each expander detail row rides along right after its
  // parent (matched by data-detail-for) so a sort never separates the pair.
  const details={};[...tb.rows].forEach(x=>{if(x.classList.contains('detailrow'))details[x.dataset.detailFor]=x;});
  const mains=[...tb.rows].filter(x=>!x.classList.contains('detailrow'));
  mains.sort((a,b)=>{const x=cellVal(a.cells[s.col]),y=cellVal(b.cells[s.col]);return ((typeof x==='number'&&typeof y==='number')?x-y:(x<y?-1:x>y?1:0))*dir;});
  mains.forEach(x=>{tb.appendChild(x);const key=x.dataset.face||x.dataset.kind;if(key&&details[key])tb.appendChild(details[key]);});}
function initApp(){
  paletteShowFull=false;  // open collapsed to base colors; the arrow expands the spans
  buildLangSel();buildViewSel();renderPalette();rebuildColorTables();renderCode();applyGround();
  initGeneratorControls();
  updateTitle();initPicker();buildPkgPreview();syncPaneHeight('uitable','mockframe');syncPaneHeight('pkgtable','pkgpreview');
  onViewChange();
}
initApp();
addEventListener('resize',()=>{syncPaneHeight('uitable','mockframe');syncPaneHeight('pkgtable','pkgpreview');});
// #preview=<app-key>[&theme=<json-url>]: select that app on load and hide the
// topbar + palette so its face table + live preview render from the top of the
// page (headless screenshots can't scroll reliably). An optional theme URL is
// fetched and imported first, so shots show a real theme instead of untitled
// (fresh headless profiles have no localStorage; Chrome needs
// --allow-file-access-from-files for a file:// fetch). Drives the screenshot
// harness (screenshot-previews.sh), same hash-URL pattern as the browser gates.
// The title flip lets the harness confirm the selection landed.
if(location.hash.startsWith('#preview=')){
  const q=location.hash.slice(9).split('&theme=');
  const k=decodeURIComponent(q[0]);
  const showApp=()=>{
    if(!APPS[k]&&k[0]!=='@')return; // '@ui'/'@code' view keys shoot too
    const s=document.getElementById('viewsel');
    if(!s)return;
    s.value=k;onViewChange();document.title='PREVIEW '+k;
    document.querySelectorAll('.topbar, body > section')
      .forEach(e=>{e.style.display='none';});};
  if(q[1])fetch(decodeURIComponent(q[1])).then(r=>r.text())
    .then(t=>{applyImported(t);showApp();})
    .catch(()=>{document.title='PREVIEW THEME-LOAD-FAILED';showApp();});
  else showApp();
}
BROWSER_GATES_J

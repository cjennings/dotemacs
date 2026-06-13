const SAMPLES=SAMPLES_J, CATS=CATS_J, UI_FACES=UIFACES_J, APPS=APPS_J;
let MAP=MAP_J, PALETTE=PALETTE_J, BOLD=BOLD_J, ITALIC=ITALIC_J, UIMAP=UIMAP_J;
let LOCKED=new Set(LOCKS_J);   // syntax categories whose element↔color is decided (dropdown disabled, skipped by clear-unlocked)
const DELTAE_MIN=0.02; // OKLab ΔE below this = colors too close to tell apart (perceptual-metrics spec)
// --- tier-3 package faces: pure state helpers (Phase 1) ---
// Thin wrappers over the pure logic in app-core.js (inlined further down),
// passing the live module state. packagesForExport / mergePackagesInto live in
// the core verbatim and are used by name.
function pname(n){return nameToHex(n,PALETTE);}
function seedPkgmap(){return buildPkgmap(APPS,PALETTE);}
let PKGMAP=seedPkgmap();
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
// The contrast-cell readout shared by every table: a WCAG ratio colored by its
// AA/AAA rating, with the rating word. Callers compute r for their own fg/bg.
function crHtml(r){return `<span style="color:${ratingColor(r)}">${r.toFixed(1)}  ${rating(r)}</span>`;}
// Effective fg/bg with the standard fallback: an unset foreground reads as the
// default fg (MAP['p']), an unset background as the ground (MAP['bg']). All three
// tiers resolve their raw value through these before measuring or rendering.
function effFg(v){return v||MAP['p'];}
function effBg(v){return v||MAP['bg'];}
function cid(l){return l.replace(/\W/g,'');}
function buildLangSel(){const s=document.getElementById('langsel');s.innerHTML='';for(const lang in SAMPLES){const o=document.createElement('option');o.value=lang;o.textContent=lang;s.appendChild(o);}}
function renderCode(){
  const lang=document.getElementById('langsel').value;let html='';
  for(const line of SAMPLES[lang]){
    if(line.length===0){html+='\n';continue;}
    for(const [k,t] of line){const c=effFg(MAP[k])||'#cdced1';const w=BOLD[k]?'bold':'normal';const s=ITALIC[k]?'italic':'normal';
      html+=`<span data-k="${k}" style="color:${c};font-weight:${w};font-style:${s}">${esc(t)}</span>`;}
    html+='\n';}
  const cp=document.getElementById('codepre');cp.innerHTML=html;
  cp.onclick=(e)=>{const s=e.target.closest('[data-k]');if(s)flashAssign(s.dataset.k);};
  buildMockFrame();
}
// Custom color dropdown: a real swatch + name + hex per row, since native
// <option> background colors render unreliably on Linux Chrome. The popup is
// fixed-positioned on <body> so a table's overflow can't clip it.
let _ddPop=null;
function closeColorDropdown(){if(_ddPop){_ddPop.remove();_ddPop=null;}}
document.addEventListener('pointerdown',e=>{if(_ddPop&&!e.target.closest('.cdd')&&!e.target.closest('.cddpop'))closeColorDropdown();});
function mkColorDropdown(options,cur,onPick){
  const t=document.createElement('div');t.className='cdd';t.tabIndex=0;
  const nameOf=h=>{const o=options.find(p=>p[0]===h);return o?o[1]:(h||'none');};
  function paint(){t.style.background=cur||'#161412';t.style.color=cur?textOn(cur):'#b4b1a2';t.dataset.val=cur||'';
    t.innerHTML=`<span class="cddsw" style="background:${cur||'transparent'}"></span>${esc(nameOf(cur))}`;}
  paint();
  t.onclick=(e)=>{e.stopPropagation();if(t.dataset.locked==='1')return;if(_ddPop){closeColorDropdown();return;}
    const pop=document.createElement('div');pop.className='cddpop';
    for(const [hex,name] of options){const row=document.createElement('div');row.className='cddrow'+(hex===cur?' sel':'');
      row.innerHTML=`<span class="cddsw" style="background:${hex||'transparent'}"></span><span class="cddnm">${esc(name)}</span><span class="cddhx">${hex||''}</span>`;
      row.onclick=(ev)=>{ev.stopPropagation();cur=hex;paint();closeColorDropdown();onPick(hex);};
      pop.appendChild(row);}
    document.body.appendChild(pop);const r=t.getBoundingClientRect();
    pop.style.left=r.left+'px';pop.style.minWidth=r.width+'px';
    pop.style.top=(r.bottom+2)+'px';
    const ph=pop.getBoundingClientRect().height;
    if(r.bottom+ph>window.innerHeight-6)pop.style.top=Math.max(6,r.top-ph-2)+'px';
    _ddPop=pop;};
  t.setValue=h=>{cur=h;paint();};
  return t;}
// Standard option list for a swatch dropdown: a "default" entry, then the
// palette in the same ground/column order as the palette panel. If cur is set
// but no longer in the palette, surface it as a "(gone)" entry so the row still
// shows what it points at. Shared by all three tiers.
function ddList(cur){return paletteOptionList(cur,PALETTE,{bg:MAP['bg'],fg:MAP['p']});}
// Shared lock toggle for any table row. lockKey is namespaced per tier (bare
// syntax kind, 'ui:'+face, 'pkg:'+app+':'+face). els are the row's editable
// controls — native selects/buttons/inputs are disabled; the custom swatch
// dropdown (a div) gets data-locked so its onclick refuses to open.
function mkLockCell(lockKey,els){
  const td=document.createElement('td');td.style.textAlign='center';
  const lk=document.createElement('button');lk.className='lockbtn';
  function paint(){const on=LOCKED.has(lockKey);lk.textContent=on?'🔒':'🔓';lk.classList.toggle('on',on);
    lk.title=on?'locked — click to unlock':'click to lock this decision';
    (els||[]).forEach(el=>{if(!el)return;
      if(el.tagName==='SELECT'||el.tagName==='BUTTON'||el.tagName==='INPUT')el.disabled=on;
      else{el.dataset.locked=on?'1':'';el.classList.toggle('locked',on);}});}
  lk.onclick=()=>{LOCKED.has(lockKey)?LOCKED.delete(lockKey):LOCKED.add(lockKey);paint();updateLockToggles();};
  paint();td.appendChild(lk);return td;}
// B/I/U/S style buttons shared by the UI and package tables. isOn(attr) reads the
// current state of an attribute, onToggle(attr) flips it and repaints. Returns
// the button list so the caller appends them and hands them to mkLockCell.
function mkStyleButtons(isOn,onToggle){
  return ['bold','italic','underline','strike'].map(at=>{
    const b=document.createElement('button');b.className='sbtn'+(isOn(at)?' on':'');b.textContent='a';
    b.style.fontWeight=at==='bold'?'bold':'normal';b.style.fontStyle=at==='italic'?'italic':'normal';
    b.style.textDecoration=at==='underline'?'underline':at==='strike'?'line-through':'none';b.title=at;
    b.onclick=()=>{onToggle(at);b.classList.toggle('on',!!isOn(at));};return b;});}
// Reset every unlocked row in a tier to its default. keyFn maps a row entry to
// its lock key, or null to skip the row entirely (syntax bg and the default fg);
// resetFn does the actual clearing. Locked rows are left untouched.
function clearUnlockedRows(items,keyFn,resetFn){
  for(const it of items){const k=keyFn(it);if(k===null)continue;if(!LOCKED.has(k))resetFn(it);}
}
function syntaxLockKeys(){return CATS.map(c=>c[0]);}
function uiLockKeys(){return UI_FACES.map(f=>'ui:'+f[0]);}
function pkgLockKeys(){const app=curApp();return APPS[app].faces.map(f=>'pkg:'+app+':'+f[0]);}
function tierLockKeys(tier){return tier==='syntax'?syntaxLockKeys():tier==='ui'?uiLockKeys():pkgLockKeys();}
function updateLockToggle(tier){
  const ids={syntax:'syntaxlocktoggle',ui:'uilocktoggle',pkg:'pkglocktoggle'},b=document.getElementById(ids[tier]);if(!b)return;
  b.textContent=lockToggleLabel(tierLockKeys(tier),LOCKED);
}
function updateLockToggles(){updateLockToggle('syntax');updateLockToggle('ui');updateLockToggle('pkg');}
function toggleAllLocks(tier){
  const all=areAllLocked(tierLockKeys(tier),LOCKED);
  LOCKED=toggleLockSet(tierLockKeys(tier),LOCKED);
  if(tier==='syntax')buildTable();else if(tier==='ui')buildUITable();else buildPkgTable();
  updateLockToggles();
  notify((all?'unlocked ':'locked ')+(tier==='pkg'?'package':tier)+' rows',false);
}
function clearUnlocked(){
  clearUnlockedRows(CATS,c=>(c[0]==='bg'||c[0]==='p')?null:c[0],c=>{MAP[c[0]]='';});
  buildTable();renderCode();notify('cleared unlocked elements to default',false);
}
function clearUnlockedUI(){
  clearUnlockedRows(UI_FACES,f=>'ui:'+f[0],f=>{UIMAP[f[0]]=uiFaceBlank();});
  buildUITable();buildMockFrame();notify('cleared unlocked UI faces to default',false);
}
function clearUnlockedPkg(){
  const app=curApp();
  clearUnlockedRows(APPS[app].faces,f=>'pkg:'+app+':'+f[0],f=>{PKGMAP[app][f[0]]=normalizePkgFace({source:'cleared'},'cleared');});
  pkgChanged();notify('cleared unlocked '+app+' faces to default',false);
}
function buildTable(){
  const tb=document.getElementById('legbody');tb.innerHTML='';
  for(const [kind,label,ex] of CATS){
    const tr=document.createElement('tr');tr.dataset.kind=kind;
    const cur=MAP[kind]||'';const list=ddList(cur);
    const exTd=document.createElement('td');exTd.className='ex';exTd.textContent=ex;
    const crTd=document.createElement('td');crTd.style.whiteSpace='nowrap';crTd.style.fontSize='10pt';
    function styleEx(){exTd.style.color=(kind==='bg'?MAP['p']:effFg(MAP[kind]));exTd.style.background=MAP['bg'];exTd.style.fontWeight=BOLD[kind]?'bold':'normal';exTd.style.fontStyle=ITALIC[kind]?'italic':'normal';}
    function styleCr(){const r=contrast((kind==='bg'?MAP['p']:effFg(MAP[kind])),MAP['bg']);crTd.innerHTML=crHtml(r);}
    const dd=mkColorDropdown(list,cur,(hex)=>{MAP[kind]=hex;styleEx();styleCr();renderCode();if(kind==='bg'||kind==='p'){applyGround();buildTable();buildPkgTable();buildPkgPreview();}repaintCovered();});
    styleEx();styleCr();
    const lkTd=mkLockCell(kind,[dd]);
    // style buttons
    const stTd=document.createElement('td');
    if(kind!=='bg'){const defs=[['B','a','bold'],['I','a','italic']];
    const btns={};
    defs.forEach(([id,ch,mode])=>{const b=document.createElement('button');b.className='sbtn';b.style.fontWeight=mode==='bold'?'bold':'normal';b.style.fontStyle=mode==='italic'?'italic':'normal';b.textContent=ch;
      b.onclick=()=>{if(mode==='bold'){BOLD[kind]=!BOLD[kind];}else{ITALIC[kind]=!ITALIC[kind];}refresh();renderCode();styleEx();};
      btns[mode]=b;stTd.appendChild(b);});
    function refresh(){btns.bold.classList.toggle('on',!!BOLD[kind]);btns.italic.classList.toggle('on',!!ITALIC[kind]);}
    refresh();}
    const c0=document.createElement('td');c0.appendChild(dd);
    const c2=document.createElement('td');c2.className='cat';c2.textContent=label;c2.style.cursor='pointer';c2.title='flash this category in the code';c2.onclick=()=>flashTokens(kind);
    tr.appendChild(c2);tr.appendChild(lkTd);tr.appendChild(c0);tr.appendChild(stTd);tr.appendChild(crTd);tr.appendChild(exTd);
    tb.appendChild(tr);}
  updateLockToggle('syntax');
}
PALETTE_ACTIONS_J
function notify(msg,err){const m=document.getElementById('palmsg');if(!m)return;m.textContent=msg;m.style.color=err?'#cb6b4d':'#8a9496';m.style.opacity='1';clearTimeout(m._t);m._t=setTimeout(()=>{m.style.opacity='0';},err?4000:2800);}
function applyEdit(){if(selectedIdx!==null)updateColor();else addColor();}
function selectColor(i){selectedIdx=i;const [hex,name]=PALETTE[i];setHex(hex);document.getElementById('newname').value=name;renderPalette();notify('editing "'+name+'" — change the value, then Enter (or Update selected) to save',false);}
function updateColor(){
  if(selectedIdx===null){notify('click a palette color to select it first',true);return;}
  const i=selectedIdx,oldHex=PALETTE[i][0],oldRole=groundRoleOfEntry(PALETTE[i],{bg:MAP['bg'],fg:MAP['p']});
  const newHex=curHex();
  const newName=(document.getElementById('newname').value.trim())||PALETTE[i][1];
  if(PALETTE.some((p,j)=>j!==i&&p[1].toLowerCase()===newName.toLowerCase())){notify('another color is already named "'+newName+'" — names must be unique',true);return;}
  // If the edited color is a column base with a ramp, recolor the whole column: regenerate from the new base at the same count.
  const columns=columnsFromPalette(PALETTE,{bg:MAP['bg'],fg:MAP['p']}).columns;
  const column=columns.find(f=>f.base.toLowerCase()===oldHex.toLowerCase());
  const count=column?Math.max(0,...rankByLightness(column.members.map(m=>m.hex),column.base).map(m=>Math.abs(m.offset))):0;
  const columnId=PALETTE[i][2]||columnStem(PALETTE[i][1]);
  PALETTE[i]=[newHex,newName,columnId];
  const duplicateOldHex=PALETTE.some((p,j)=>j!==i&&p[0].toLowerCase()===oldHex.toLowerCase());
  if(oldRole==='bg'||oldRole==='fg')repointHex(oldHex,newHex);
  else if(!duplicateOldHex&&oldHex!==MAP['bg']&&oldHex!==MAP['p'])repointHex(oldHex,newHex);
  if(column&&count>0){
    const oldHexes=column.members.map(m=>m.hex.toLowerCase()===oldHex.toLowerCase()?newHex:m.hex);
    regenColumnInPlace(oldHexes,newHex,newName,count,column.column||columnId);
    closePicker();selectedIdx=null;renderPalette();buildTable();buildUITable();renderCode();applyGround();notify('recolored "'+newName+'" column from the new base',false);return;
  }
  closePicker();renderPalette();buildTable();buildUITable();renderCode();applyGround();notify('updated "'+newName+'"',false);
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
function syncHex(){const v=normHex(document.getElementById('newhexstr').value);if(!v)return;document.getElementById('swatch').style.background=v;[pkH,pkS,pkV]=rgb2hsv(...hex2rgb(v));if(pickerOn)paintPicker();pkReadout(v);}
function setHex(h){h=normHex(h)||h;document.getElementById('newhexstr').value=h;document.getElementById('swatch').style.background=h;[pkH,pkS,pkV]=rgb2hsv(...hex2rgb(h));if(pickerOn)paintPicker();pkReadout(h);}
function pkSet(){const hex=rgb2hex(...hsv2rgb(pkH,pkS,pkV));document.getElementById('newhexstr').value=hex;document.getElementById('swatch').style.background=hex;paintPicker();pkReadout(hex);if(pkModel==='oklch')oklchInputsFromHex(hex);}
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
  [pkH,pkS,pkV]=rgb2hsv(...hex2rgb(hex));paintPicker();pkReadout(hex);
  if(clamped)oklchInputsFromHex(hex); // snap the dials to the reachable color
  pkClampStatus(clamped);}
function setPkModel(m){pkModel=m;document.querySelectorAll('.pmodel button').forEach(x=>x.classList.toggle('on',x.dataset.pm===m));
  const oc=document.getElementById('oklchctl');if(oc)oc.classList.toggle('show',m==='oklch');
  if(m==='oklch')oklchInputsFromHex(curHex());else pkClampStatus(false);}
function buildPkChips(){const c=document.getElementById('pkchips');if(!c)return;c.innerHTML='';const T=pkThresh();PALETTE.forEach(([hex,name])=>{const s=document.createElement('div');s.className='pc';s.style.background=hex;s.title=name+' '+hex;const ok=!T||contrast(hex,MAP['bg'])>=T;if(!ok){s.style.opacity='0.22';s.title+=' (below '+pkMode.toUpperCase()+')';}s.onclick=()=>{if(ok)setHex(hex);};c.appendChild(s);});}
function openPicker(){pickerOn=true;[pkH,pkS,pkV]=rgb2hsv(...hex2rgb(curHex()));buildPkChips();document.getElementById('picker').style.display='block';setPkModel(pkModel);paintPicker();pkReadout(curHex());setTimeout(()=>document.addEventListener('pointerdown',pkOutside),0);}
function closePicker(){if(!pickerOn)return;pickerOn=false;const p=document.getElementById('picker');if(p)p.style.display='none';document.removeEventListener('pointerdown',pkOutside);}
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
  PALETTE.push([h,name,columnIdOf([h,name])]);const healed=healGone(name,h);document.getElementById('newname').value='';selectedIdx=null;closePicker();
  renderPalette();buildTable();buildUITable();
  if(healed){renderCode();applyGround();if(document.getElementById('pkgbody'))buildPkgTable();buildPkgPreview();}
  notify(healed?('added "'+name+'" and reconnected its assignments'):('added "'+name+'"'),false);}
function themeName(){return (document.getElementById('themename').value||'theme').trim()||'theme';}
function fileSlug(){return slugify(themeName());}
function exportObj(){normalizePalette();const a={};CATS.forEach(c=>a[c[0]]=MAP[c[0]]);const o={name:themeName(),palette:PALETTE,assignments:a,bold:Object.keys(BOLD).filter(k=>BOLD[k]),italic:Object.keys(ITALIC).filter(k=>ITALIC[k]),ui:UIMAP};if(LOCKED.size)o.locks=[...LOCKED];const pk=packagesForExport(PKGMAP);if(Object.keys(pk).length)o.packages=pk;return o;}
function exportState(){const t=document.getElementById('export');t.value=JSON.stringify(exportObj(),null,1);t.style.display='block';t.focus();t.select();}
function toggleJSON(){const t=document.getElementById('export'),b=document.getElementById('jsonbtn');if(t.style.display==='block'){t.style.display='none';b.textContent='show';}else{exportState();b.textContent='hide';}}
function updateTitle(){const n=document.getElementById('themename').value.trim();document.getElementById('pagetitle').textContent=(n||'Untitled')+': theme';const sb=document.getElementById('savebtn');if(sb){sb.style.display=n||fileHandle?'':'none';sb.title=fileHandle?'overwrite the imported/saved file':'choose where to save';}}
let fileHandle=null;
function exportTheme(){const blob=new Blob([JSON.stringify(exportObj(),null,1)],{type:'application/json'});const a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download=fileSlug()+'.json';a.click();}
async function saveTheme(){const data=JSON.stringify(exportObj(),null,1);
  if(!window.showSaveFilePicker){exportTheme();notify('saved via download (browser has no Save-File support)',false);return;}
  try{if(!fileHandle)fileHandle=await window.showSaveFilePicker({suggestedName:fileSlug()+'.json',types:[{description:'theme JSON',accept:{'application/json':['.json']}}]});
    const w=await fileHandle.createWritable();await w.write(data);await w.close();notify('saved "'+themeName()+'"',false);updateTitle();
  }catch(e){if(e&&e.name!=='AbortError')notify('save failed: '+e.message,true);}}
function applyImported(text){const d=JSON.parse(text);lastGone={};if(d.name)document.getElementById('themename').value=d.name;if(d.palette)PALETTE=d.palette.map(normalizePaletteEntry);if(d.assignments)Object.assign(MAP,d.assignments);
  BOLD={};(d.bold||[]).forEach(k=>BOLD[k]=true);ITALIC={};(d.italic||[]).forEach(k=>ITALIC[k]=true);
  LOCKED=new Set(d.locks||[]);
  if(d.ui)Object.assign(UIMAP,d.ui);
  PKGMAP=seedPkgmap();if(d.packages)mergePackagesInto(PKGMAP,d.packages);
  renderPalette();buildTable();buildUITable();buildPkgTable();buildPkgPreview();renderCode();applyGround();updateTitle();}
// File-input fallback (no File System Access API): no writable handle, so save still prompts.
function importFile(ev){const f=ev.target.files[0];if(!f)return;const r=new FileReader();
  r.onload=()=>{try{applyImported(r.result);fileHandle=null;updateTitle();}catch(e){alert('bad theme file: '+e.message);}};
  r.readAsText(f);ev.target.value='';}
// Preferred import: keep the file handle so a later save overwrites the same file.
async function importTheme(){
  if(!window.showOpenFilePicker){const fi=document.getElementById('fileinput');if(fi)fi.click();return;}
  try{const [h]=await window.showOpenFilePicker({types:[{description:'theme JSON',accept:{'application/json':['.json']}}]});
    const file=await h.getFile();applyImported(await file.text());fileHandle=h;updateTitle();
    notify('imported "'+(themeName()||file.name)+'" — save now overwrites it',false);
  }catch(e){if(e&&e.name!=='AbortError')notify('import failed: '+e.message,true);}}
// The blanket covers only the code panes and syntax example cells. UI-face
// preview cells also carry .ex, but a face with its own bg must keep it, so
// those rows repaint through paintUI (which also re-rates the contrast cell
// against the new ground for faces without their own bg).
function applyGround(){document.querySelectorAll('pre').forEach(p=>p.style.background=MAP['bg']);document.querySelectorAll('#legbody .ex').forEach(e=>e.style.background=MAP['bg']);UI_FACES.forEach(([f])=>{if(document.getElementById('uiprev-'+f))paintUI(f);});}
function uf(f){return UIMAP[f]||{};}
function udeco(o){return `font-weight:${o.bold?'bold':'normal'};font-style:${o.italic?'italic':'normal'};text-decoration:${(o.underline?'underline ':'')+(o.strike?'line-through':'')||'none'}`;}
// A face's :box, rendered as an inset box-shadow (no layout shift). Returns the
// box-shadow VALUE (or '' for no box). 'line' is a flat border in the box color
// (or the face's own color when unset); 'released'/'pressed' are the 3D button
// styles Emacs draws, derived from the background so they read on any color.
function boxCss(b,bg){if(!b||!b.style)return '';const w=b.width||1;
  if(b.style==='released'||b.style==='pressed'){
    // Emacs derives the 3D edges from the face's background (reliefColors,
    // ported from xterm.c); the translucent pair is only the no-bg fallback.
    const r=bg?reliefColors(bg):{hl:null,sh:null};
    const hl=r.hl||'#ffffff33',sh=r.sh||'#00000066';
    const [a,z]=b.style==='released'?[hl,sh]:[sh,hl];
    return `inset ${w}px ${w}px 0 ${a},inset -${w}px -${w}px 0 ${z}`;}
  return `inset 0 0 0 ${w}px ${b.color||'currentColor'}`;}
// The per-row box control: none / line / raised / pressed. get()/set() read and
// write the face's box object (null = no box).
function mkBoxSelect(get,set){const s=document.createElement('select');s.className='chip';s.style.cssText='width:84px;font:10pt monospace';
  [['','no box'],['line','line'],['released','raised'],['pressed','pressed']].forEach(([v,l])=>{const o=document.createElement('option');o.value=v;o.textContent=l;s.appendChild(o);});
  const cur=get();s.value=cur&&cur.style?cur.style:'';
  s.onchange=()=>set(s.value?{style:s.value,width:1,color:null}:null);return s;}
function flashRow(tr){if(!tr)return;tr.scrollIntoView({block:'center',behavior:'smooth'});tr.classList.remove('flash');void tr.offsetWidth;tr.classList.add('flash');}
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
function mockSpan(k,t){return `<span data-k="${k}" style="color:${effFg(MAP[k])};font-weight:${BOLD[k]?'bold':'normal'};font-style:${ITALIC[k]?'italic':'normal'}">${esc(t)}</span>`;}
function syncMockHeight(){const t=document.getElementById('uitable'),m=document.getElementById('mockframe');if(!t||!m)return;const lb=m.previousElementSibling,lbh=lb?lb.getBoundingClientRect().height+10:30;m.style.height=Math.max(t.getBoundingClientRect().height-lbh,220)+'px';}
function buildMockFrame(){
  const fr=document.getElementById('mockframe');if(!fr)return;
  const bg=MAP['bg'],fg=MAP['p'];
  const ln=uf('line-number'),lnc=uf('line-number-current-line'),hl=uf('hl-line'),hil=uf('highlight'),reg=uf('region'),isr=uf('isearch'),isf=uf('isearch-fail'),laz=uf('lazy-highlight'),par=uf('show-paren-match'),parx=uf('show-paren-mismatch'),cur=uf('cursor'),ml=uf('mode-line'),mli=uf('mode-line-inactive'),mb=uf('minibuffer-prompt'),frng=uf('fringe'),vb=uf('vertical-border'),lnk=uf('link'),err=uf('error'),wrn=uf('warning'),suc=uf('success');
  const lines=[
    {t:[['cmd',';; '],['cm','init.el - your config']]},
    {t:[['punc','('],['kw','require'],['p',' '],['con',"'cl-lib"],['punc',')']]},
    {t:[]},
    {t:[['punc','('],['kw','defun'],['p',' '],['fnd','cj/greet'],['p',' '],['punc','('],['var','name'],['punc',')']]},
    {t:[['p','  '],['punc','('],['fnc','message'],['p',' '],['str','"hi %s"'],['p',' '],['var','name'],['punc','))']],cur:1},
    {t:[['p','  '],['punc','('],['kw','setq'],['p',' '],['var','count'],['p',' '],['num','42'],['punc',')']],region:1},
    {t:[['p','  '],['punc','('],['kw','if'],['p',' '],['punc','('],['op','>'],['p',' '],['var','count'],['p',' '],['num','0'],['punc',')']],match:1},
    {t:[['p','    '],['punc','('],['kw','setq'],['p',' '],['var','total'],['p',' '],['punc','('],['op','+'],['p',' '],['var','total'],['p',' '],['var','count'],['punc','))']],hl:1},
    {t:[['p','      '],['punc','('],['fnc','process'],['p',' '],['var','items'],['punc',')']],cont:1},
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
    return `<span data-face="${dface}" style="background:${face.bg||'transparent'};${udeco(face)}">${inner}</span>`;
  };
  // Emacs box cursor: it sits on the character at point, drawn in the frame
  // background over the cursor color (the cursor face's foreground is ignored).
  // Falls back to a trailing block only if the line has no glyph (point at EOL).
  const withCursor=(tokens)=>{
    let out='',placed=false;
    const cell=ch=>`<span data-face="cursor" style="background:${cur.bg||fg};color:${bg}">${esc(ch)}</span>`;
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
    const nFg=isc?(lnc.fg||fg):(ln.fg||fg), nBg=isc?(lnc.bg||'transparent'):(ln.bg||'transparent');
    const rowBg=isc?(hl.bg||'transparent'):'transparent';
    let cd;
    if(isc)cd=withCursor(L.t);
    else if(L.region)cd=overlay(L.t,reg,'region');
    else if(L.hl)cd=overlay(L.t,hil,'highlight');
    else if(L.match)cd=overlay(L.t,isr,'isearch');
    else if(L.lazy)cd=overlay(L.t,laz,'lazy-highlight');
    else if(L.paren)cd=L.t.map(([k,t],j)=>j===L.t.length-1?`<span data-face="show-paren-match" style="background:${par.bg||'transparent'};color:${par.fg||MAP[k]||fg};${udeco(par)}">${esc(t)}</span>`:mockSpan(k,t)).join('');
    else if(L.mismatch)cd=L.t.map(([k,t],j)=>{if(j!==L.t.length-1)return mockSpan(k,t);const head=t.slice(0,-1),bad=t.slice(-1);return (head?mockSpan(k,head):'')+`<span data-face="show-paren-mismatch" style="background:${parx.bg||'transparent'};color:${parx.fg||MAP[k]||fg};${udeco(parx)}">${esc(bad)}</span>`;}).join('');
    else cd=L.t.map(([k,t])=>mockSpan(k,t)).join('');
    const nFace=isc?'line-number-current-line':'line-number';
    buf+=`<div class="ln" style="background:${rowBg}"><span class="fr" data-face="fringe" style="background:${frng.bg||bg};color:${frng.fg||fg};text-align:center;font-size:10px;overflow:hidden" title="fringe">${L.cont?'&#8618;':''}</span><span class="num" data-face="${nFace}" style="color:${nFg};background:${nBg};${udeco(isc?lnc:ln)}">${i+1}</span><span class="cd">${cd||'&nbsp;'}</span></div>`;
  });
  let html=`<div class="mbuf" style="display:flex;background:${bg}"><div style="flex:1;min-width:0">${buf}</div><div data-face="vertical-border" title="vertical-border" style="width:3px;flex:0 0 auto;background:${vb.fg||vb.bg||'#2f343a'}"></div></div>`;
  const mlbx=boxCss(ml.box,ml.bg||bg),mlibx=boxCss(mli.box,mli.bg||bg);
  html+=`<div class="bar" data-face="mode-line" style="background:${ml.bg||fg};color:${ml.fg||bg};${udeco(ml)}${mlbx?';box-shadow:'+mlbx:''}">  init.el      (Emacs Lisp)      L5      git:main  </div>`;
  html+=`<div class="bar" data-face="mode-line-inactive" style="background:${mli.bg||bg};color:${mli.fg||fg};${udeco(mli)}${mlibx?';box-shadow:'+mlibx:''}">  *Messages*      (Fundamental)  </div>`;
  html+=`<div class="echo" style="color:${fg}"><span data-face="minibuffer-prompt" style="color:${mb.fg||fg};${udeco(mb)}">I-search:</span> count   <span data-face="isearch-fail" style="color:${isf.fg||fg};background:${isf.bg||'transparent'};${udeco(isf)}">zzz [no match]</span></div>`;
  html+=`<div class="echo"><span data-face="link" style="color:${lnk.fg||fg};${udeco(lnk)}">https://gnu.org</span>   <span data-face="error" style="color:${err.fg||fg};${udeco(err)}">error</span>   <span data-face="warning" style="color:${wrn.fg||fg};${udeco(wrn)}">warning</span>   <span data-face="success" style="color:${suc.fg||fg};${udeco(suc)}">ok</span></div>`;
  fr.innerHTML=html;fr.style.background=bg;fr.style.color=fg;
  fr.onclick=(e)=>{const u=e.target.closest('[data-face]');if(u){flashUi(u.dataset.face);return;}const k=e.target.closest('[data-k]');if(k)flashAssign(k.dataset.k);};
}
// All three tiers share one dropdown — the swatch div from mkColorDropdown. The
// native <select> rendered swatch colors unreliably on Linux Chrome, so it is
// gone. '' (the default entry) maps back to null in the stored model.
function uiSelect(face,attr){const cur=UIMAP[face][attr]||'';
  return mkColorDropdown(ddList(cur),cur,h=>{UIMAP[face][attr]=h||null;paintUI(face);buildMockFrame();});}
const BASE_INHERITS=['fixed-pitch','variable-pitch','default','link','bold','italic','shadow'];
function uiFaceBlank(){return {fg:null,bg:null,bold:false,italic:false,underline:false,strike:false};}
function seedFace(d){return normalizePkgFace({fg:pname(d.fg),bg:pname(d.bg),bold:d.bold,italic:d.italic,underline:d.underline,strike:d.strike,inherit:d.inherit,height:d.height,box:d.box},'default');}
function curApp(){const s=document.getElementById('appsel');return s&&s.value?s.value:Object.keys(APPS)[0];}
function pkgEffFg(app,face,seen){return effResolve(PKGMAP,app,face,'fg',seen);}
function pkgEffBg(app,face,seen){return effResolve(PKGMAP,app,face,'bg',seen);}
function buildAppSel(){const s=document.getElementById('appsel');if(!s)return;s.innerHTML='';for(const app in APPS){const o=document.createElement('option');o.value=app;o.textContent=APPS[app].label;s.appendChild(o);}s.onchange=pkgChanged;}
function pkgChanged(){buildPkgTable();buildPkgPreview();syncPkgHeight();}
function buildPkgTable(){
  const app=curApp(),tb=document.getElementById('pkgbody');if(!tb)return;tb.innerHTML='';
  const flt=(document.getElementById('pkgfilter').value||'').trim().toLowerCase();
  const inh=[''].concat(BASE_INHERITS).concat(APPS[app].faces.map(r=>r[0]));
  for(const [face,label,def] of APPS[app].faces){
    if(flt&&!(face.toLowerCase().includes(flt)||label.toLowerCase().includes(flt)))continue;
    const f=PKGMAP[app][face],tr=document.createElement('tr');tr.dataset.face=face;
    const c0=document.createElement('td');c0.className='cat';c0.textContent=label;c0.title=face;c0.style.cursor='pointer';c0.onclick=()=>flashPkgPreview(face);
    const fgd=mkColorDropdown(ddList(f.fg||''),f.fg||'',h=>{f.fg=h||null;f.source='user';pkgChanged();}),
          bgd=mkColorDropdown(ddList(f.bg||''),f.bg||'',h=>{f.bg=h||null;f.source='user';pkgChanged();});
    const cf=document.createElement('td');cf.appendChild(fgd);
    const cb=document.createElement('td');cb.appendChild(bgd);
    const cw=document.createElement('td');
    const pkBtns=mkStyleButtons(at=>f[at],at=>{f[at]=!f[at];f.source='user';pkgChanged();});
    pkBtns.forEach(b=>cw.appendChild(b));
    const ci=document.createElement('td');const isel=document.createElement('select');isel.className='chip';isel.style.cssText='width:150px;font:10pt monospace';inh.forEach(o=>{const op=document.createElement('option');op.value=o;op.textContent=o||'— none —';isel.appendChild(op);});isel.value=f.inherit||'';isel.onchange=()=>{f.inherit=isel.value||null;f.source='user';pkgChanged();};ci.appendChild(isel);
    const ch=document.createElement('td');const hin=document.createElement('input');hin.type='number';hin.min='0.8';hin.max='2.5';hin.step='0.05';hin.value=f.height||1;hin.className='hstep';hin.onchange=()=>{f.height=parseFloat(hin.value)||1;f.source='user';pkgChanged();};ch.appendChild(hin);
    const cc=document.createElement('td');cc.style.fontSize='10pt';cc.style.whiteSpace='nowrap';const efg=effFg(pkgEffFg(app,face)),ebg=effBg(pkgEffBg(app,face)),r=contrast(efg,ebg);cc.innerHTML=crHtml(r);
    const cx=document.createElement('td');const boxSel=mkBoxSelect(()=>f.box,b=>{f.box=b;f.source='user';pkgChanged();});cx.appendChild(boxSel);
    const cr=document.createElement('td');const rb=document.createElement('button');rb.className='sbtn';rb.textContent='↺';rb.title='reset to default';rb.onclick=()=>{PKGMAP[app][face]=seedFace(def);pkgChanged();};cr.appendChild(rb);
    const cL=mkLockCell('pkg:'+app+':'+face,[fgd,bgd,...pkBtns,isel,hin,boxSel,rb]);
    tr.append(c0,cL,cf,cb,cw,cc,ci,ch,cx,cr);tb.appendChild(tr);
  }
  applyTableSort('pkgbody');
  updateLockToggle('pkg');
}
function ofs(app,face){const f=PKGMAP[app][face]||{},fg=effFg(pkgEffFg(app,face)),bg=pkgEffBg(app,face);const dec=(f.underline?'underline ':'')+(f.strike?'line-through':'');const bx=boxCss(f.box,bg||MAP['bg']);return `color:${fg};${bg?'background:'+bg+';':''}font-weight:${f.bold?'bold':'normal'};font-style:${f.italic?'italic':'normal'};text-decoration:${dec.trim()||'none'};font-size:${(f.height||1)}em${bx?';box-shadow:'+bx:''}`;}
function os(app,face,txt){return `<span data-face="${face}" style="${ofs(app,face)}">${txt}</span>`;}
function renderOrgPreview(){const a='org-mode',L=[];
  L.push(os(a,'org-document-info-keyword','#+TITLE:')+' '+os(a,'org-document-title','Project Notes'));
  L.push(os(a,'org-document-info-keyword','#+AUTHOR:')+' '+os(a,'org-document-info','Craig Jennings'));
  L.push(os(a,'org-meta-line','#+STARTUP: overview'));
  L.push('');
  L.push(os(a,'org-level-1','* Inbox')+'  '+os(a,'org-tag',':work:')+os(a,'org-tag-group',':@office:'));
  L.push(os(a,'org-level-2','** ')+os(a,'org-todo','TODO')+os(a,'org-level-2',' Draft the spec')+' '+os(a,'org-priority','[#A]')+' '+os(a,'org-tag',':spec:'));
  L.push('   '+os(a,'org-special-keyword','SCHEDULED:')+' '+os(a,'org-date','&lt;2026-06-08 Sun&gt;')+'  '+os(a,'org-special-keyword','DEADLINE:')+' '+os(a,'org-date','&lt;2026-06-12 Thu&gt;'));
  L.push('   '+os(a,'org-drawer',':PROPERTIES:'));
  L.push('   '+os(a,'org-special-keyword',':ID:')+'       '+os(a,'org-property-value','abc-123-def'));
  L.push('   '+os(a,'org-drawer',':END:'));
  L.push('   '+os(a,'org-list-dt','- term ::')+' definition, with a '+os(a,'org-footnote','[fn:1]')+' note.');
  L.push('   '+os(a,'org-checkbox','[X]')+' done item   '+os(a,'org-checkbox-statistics-done','[2/2]'));
  L.push('   '+os(a,'org-checkbox','[ ]')+' open item   '+os(a,'org-checkbox-statistics-todo','[0/3]')+'  '+os(a,'org-warning','(!)'));
  L.push(os(a,'org-level-2','** ')+os(a,'org-done','DONE')+os(a,'org-headline-done',' Ship the tool'));
  L.push(os(a,'org-level-3','*** ')+os(a,'org-todo','TODO')+os(a,'org-headline-todo',' Heading three'));
  L.push(os(a,'org-level-4','**** four')+' / '+os(a,'org-level-5','***** five')+' / '+os(a,'org-level-6','****** six')+' / '+os(a,'org-level-7','******* seven')+' / '+os(a,'org-level-8','******** eight'));
  L.push('   Inline '+os(a,'org-code','=code=')+', '+os(a,'org-verbatim','~verbatim~')+', '+os(a,'org-inline-src-block','src_py{1+1}')+',');
  L.push('   a '+os(a,'org-link','[[https://gnu.org][link]]')+', a '+os(a,'org-target','&lt;&lt;target&gt;&gt;')+', a '+os(a,'org-macro','{{{macro}}}')+',');
  L.push('   a '+os(a,'org-cite','[cite:')+os(a,'org-cite-key','@knuth1984')+os(a,'org-cite',']')+', a date '+os(a,'org-sexp-date','&lt;%%(diary-float 6 5 2)&gt;')+'.');
  L.push('   '+os(a,'org-quote','#+begin_quote')+' a '+os(a,'org-verse','verse')+' line, latex '+os(a,'org-latex-and-related','$E = mc^2$')+'.');
  L.push('');
  L.push('   '+os(a,'org-block-begin-line','#+begin_src elisp'));
  L.push('   '+os(a,'org-block','  (message "hi")'));
  L.push('   '+os(a,'org-block-end-line','#+end_src'));
  L.push('');
  L.push('   '+os(a,'org-table-header','| name | hex     |'));
  L.push('   '+os(a,'org-table','|------+---------|'));
  L.push('   '+os(a,'org-table-row','| blue | #67809c |')+'  '+os(a,'org-formula',':=vsum(@2)'));
  L.push('   '+os(a,'org-column-title','Effort')+' '+os(a,'org-column','| 0:30 |')+'   '+os(a,'org-archived','* archived')+os(a,'org-ellipsis',' ...'));
  L.push('');
  L.push(os(a,'org-agenda-structure','Week-agenda (W23):'));
  L.push(os(a,'org-agenda-date','Monday      8 June 2026'));
  L.push(os(a,'org-agenda-date-today','Tuesday     9 June 2026')+' '+os(a,'org-agenda-current-time','10:24')+' '+os(a,'org-time-grid','----------'));
  L.push(os(a,'org-agenda-date-weekend','Saturday   13 June')+' / '+os(a,'org-agenda-date-weekend-today','wknd-today'));
  L.push('  '+os(a,'org-scheduled-previously','Sched.past:')+' overdue   '+os(a,'org-agenda-done','x done item'));
  L.push('  '+os(a,'org-scheduled','Scheduled:')+' a task   '+os(a,'org-scheduled-today','due today'));
  L.push('  '+os(a,'org-imminent-deadline','Deadline!')+' / '+os(a,'org-upcoming-deadline','upcoming')+' / '+os(a,'org-upcoming-distant-deadline','distant'));
  L.push('  '+os(a,'org-agenda-dimmed-todo-face','dimmed todo')+'  '+os(a,'org-agenda-diary','diary')+'  '+os(a,'org-agenda-clocking','clocking'));
  L.push('  '+os(a,'org-agenda-calendar-event','cal-event')+' / '+os(a,'org-agenda-calendar-sexp','cal-sexp')+' / '+os(a,'org-agenda-calendar-daterange','range'));
  L.push('  '+os(a,'org-agenda-structure-secondary','secondary')+' '+os(a,'org-agenda-structure-filter','filter')+' '+os(a,'org-agenda-restriction-lock','lock')+' '+os(a,'org-agenda-column-dateline','col-date'));
  L.push('  Filters: '+os(a,'org-agenda-filter-category','cat')+' '+os(a,'org-agenda-filter-tags','tags')+' '+os(a,'org-agenda-filter-effort','effort')+' '+os(a,'org-agenda-filter-regexp','re'));
  L.push('  '+os(a,'org-mode-line-clock','[0:45]')+' / '+os(a,'org-mode-line-clock-overrun','[OVER]')+'   '+os(a,'org-dispatcher-highlight','[d]ispatch'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;
}
function renderMagitPreview(){const a='magit',L=[];
  L.push(os(a,'magit-header-line',' Magit: dotemacs ')+'  '+os(a,'magit-header-line-key','g')+os(a,'magit-header-line-log-select',' refresh'));
  L.push(os(a,'magit-head','Head:')+'     '+os(a,'magit-branch-current','main')+'  '+os(a,'magit-diff-revision-summary','Ship the tool'));
  L.push(os(a,'magit-head','Merge:')+'    '+os(a,'magit-branch-remote','origin/main')+'  '+os(a,'magit-branch-local','main'));
  L.push(os(a,'magit-head','Push:')+'     '+os(a,'magit-branch-remote-head','origin/main'));
  L.push(os(a,'magit-head','Upstream:')+' '+os(a,'magit-branch-upstream','origin/main')+'  '+os(a,'magit-branch-warning','(diverged)'));
  L.push('');
  L.push(os(a,'magit-section-heading','Untracked files')+' '+os(a,'magit-section-child-count','(2)'));
  L.push('  '+os(a,'magit-filename','notes.txt')+'   '+os(a,'magit-dimmed','(ignored sibling)'));
  L.push(os(a,'magit-section-highlight','  scratch.el   (highlighted row)'));
  L.push('');
  L.push(os(a,'magit-section-heading','Unstaged changes')+' '+os(a,'magit-section-child-count','(1)'));
  L.push(os(a,'magit-diff-file-heading','modified   generate.py'));
  L.push(os(a,'magit-diff-hunk-heading','@@ -1,4 +1,5 @@ def main'));
  L.push(os(a,'magit-diff-context','   unchanged context'));
  L.push(os(a,'magit-diff-removed','- old line')+os(a,'magit-diff-whitespace-warning','   '));
  L.push(os(a,'magit-diff-added','+ new line'));
  L.push('');
  L.push(os(a,'magit-section-heading','Staged changes')+'   '+os(a,'magit-diffstat-added','++++')+os(a,'magit-diffstat-removed','--'));
  L.push(os(a,'magit-diff-file-heading-highlight','modified   README.md   (highlighted heading)'));
  L.push(os(a,'magit-diff-hunk-heading-highlight','@@ hunk heading highlight @@'));
  L.push(os(a,'magit-diff-added-highlight','+ added highlight')+'   '+os(a,'magit-diff-removed-highlight','- removed highlight'));
  L.push(os(a,'magit-diff-context-highlight','  context highlight'));
  L.push('');
  L.push(os(a,'magit-section-heading','Stashes'));
  L.push('  '+os(a,'magit-refname-stash','stash@{0}')+'  '+os(a,'magit-refname-wip','wip')+'  '+os(a,'magit-refname-pullreq','pr/42')+'  '+os(a,'magit-refname','refs/heads/x'));
  L.push('');
  L.push(os(a,'magit-section-heading','Recent commits'));
  L.push(os(a,'magit-log-graph','* ')+os(a,'magit-hash','b5b1869f')+' '+os(a,'magit-log-date','06-08')+' '+os(a,'magit-log-author','Craig')+'  enlarge the picker');
  L.push(os(a,'magit-log-graph','* ')+os(a,'magit-hash','4fa5e995')+' '+os(a,'magit-log-date','06-07')+' '+os(a,'magit-log-author','Craig')+'  '+os(a,'magit-keyword','[feat]')+' picker');
  L.push(os(a,'magit-log-graph','* ')+os(a,'magit-hash','de07e01a')+' '+os(a,'magit-log-date','06-05')+' '+os(a,'magit-log-author','Craig')+'  '+os(a,'magit-tag','v0.3')+' '+os(a,'magit-keyword-squash','!squash'));
  L.push('');
  L.push(os(a,'magit-section-secondary-heading','Merge conflict')+'  '+os(a,'magit-diff-lines-heading','lines 10-14')+os(a,'magit-diff-lines-boundary','|'));
  L.push('  '+os(a,'magit-diff-conflict-heading','=======')+'  '+os(a,'magit-diff-conflict-heading-highlight','(hl)'));
  L.push('  '+os(a,'magit-diff-base','base')+'/'+os(a,'magit-diff-base-highlight','base-hl')+'  '+os(a,'magit-diff-our','ours')+'/'+os(a,'magit-diff-our-highlight','ours-hl')+'  '+os(a,'magit-diff-their','theirs')+'/'+os(a,'magit-diff-their-highlight','theirs-hl'));
  L.push('  '+os(a,'magit-diff-hunk-region','hunk-region')+'  '+os(a,'magit-diff-file-heading-selection','file-sel')+'  '+os(a,'magit-diff-hunk-heading-selection','hunk-sel')+'  '+os(a,'magit-section-heading-selection','sec-sel')+'  '+os(a,'magit-diff-revision-summary-highlight','rev-sum-hl'));
  L.push('');
  L.push(os(a,'magit-section-heading','Reflog'));
  L.push('  '+os(a,'magit-reflog-commit','commit')+' '+os(a,'magit-reflog-amend','amend')+' '+os(a,'magit-reflog-merge','merge')+' '+os(a,'magit-reflog-checkout','checkout')+' '+os(a,'magit-reflog-reset','reset')+' '+os(a,'magit-reflog-rebase','rebase')+' '+os(a,'magit-reflog-cherry-pick','cherry-pick')+' '+os(a,'magit-reflog-remote','remote')+' '+os(a,'magit-reflog-other','other'));
  L.push(os(a,'magit-section-heading','Rebase sequence'));
  L.push('  '+os(a,'magit-sequence-pick','pick')+' '+os(a,'magit-sequence-stop','stop')+' '+os(a,'magit-sequence-part','part')+' '+os(a,'magit-sequence-head','head')+' '+os(a,'magit-sequence-drop','drop')+' '+os(a,'magit-sequence-done','done')+' '+os(a,'magit-sequence-onto','onto')+' '+os(a,'magit-sequence-exec','exec'));
  L.push(os(a,'magit-section-heading','Bisect / Cherry / Process'));
  L.push('  '+os(a,'magit-bisect-good','good')+' '+os(a,'magit-bisect-bad','bad')+' '+os(a,'magit-bisect-skip','skip')+'   '+os(a,'magit-cherry-equivalent','equivalent')+' '+os(a,'magit-cherry-unmatched','unmatched'));
  L.push('  '+os(a,'magit-process-ok','OK')+' '+os(a,'magit-process-ng','NG')+'   '+os(a,'magit-mode-line-process','[fetch]')+' '+os(a,'magit-mode-line-process-error','[error]'));
  L.push(os(a,'magit-section-heading','Blame'));
  L.push(os(a,'magit-blame-margin','margin')+os(a,'magit-blame-heading',' b5b1869f '))
  L.push('  '+os(a,'magit-blame-hash','b5b1869f')+' '+os(a,'magit-blame-name','Craig')+' '+os(a,'magit-blame-date','2026-06-08')+' '+os(a,'magit-blame-summary','enlarge picker')+' '+os(a,'magit-blame-highlight','hl')+' '+os(a,'magit-blame-dimmed','dim'));
  L.push(os(a,'magit-section-heading','Signatures')+os(a,'magit-left-margin','  '));
  L.push('  '+os(a,'magit-signature-good','good')+' '+os(a,'magit-signature-bad','bad')+' '+os(a,'magit-signature-untrusted','untrusted')+' '+os(a,'magit-signature-expired','expired')+' '+os(a,'magit-signature-expired-key','expired-key')+' '+os(a,'magit-signature-revoked','revoked')+' '+os(a,'magit-signature-error','error'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderElfeedPreview(){const a='elfeed',L=[];
  L.push(os(a,'elfeed-search-filter-face','@6-months-ago +unread')+'   '+os(a,'elfeed-search-unread-count-face','3/120')+'   '+os(a,'elfeed-search-last-update-face','updated 02:24'));
  L.push('');
  L.push(os(a,'elfeed-search-date-face','2026-06-08')+'  '+os(a,'elfeed-search-feed-face','Planet Emacs')+'  '+os(a,'elfeed-search-unread-title-face','New release of Magit')+'  '+os(a,'elfeed-search-tag-face',':emacs:'));
  L.push(os(a,'elfeed-search-date-face','2026-06-07')+'  '+os(a,'elfeed-search-feed-face','LWN')+'  '+os(a,'elfeed-search-unread-title-face','Kernel 6.18 lands')+'  '+os(a,'elfeed-search-tag-face',':linux:'));
  L.push(os(a,'elfeed-search-date-face','2026-06-05')+'  '+os(a,'elfeed-search-feed-face','Hacker News')+'  '+os(a,'elfeed-search-title-face','Show HN: a theme editor')+'  '+os(a,'elfeed-search-tag-face',':show:'));
  L.push('');
  L.push(os(a,'elfeed-log-date-face','02:24:01')+'  '+os(a,'elfeed-log-info-level-face','INFO ')+' updated 12 feeds');
  L.push(os(a,'elfeed-log-date-face','02:24:02')+'  '+os(a,'elfeed-log-warn-level-face','WARN ')+' slow feed: example.com');
  L.push(os(a,'elfeed-log-date-face','02:24:03')+'  '+os(a,'elfeed-log-error-level-face','ERROR')+' failed: bad.example');
  L.push(os(a,'elfeed-log-date-face','02:24:04')+'  '+os(a,'elfeed-log-debug-level-face','DEBUG')+' parsed 340 entries');
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderGhostelPreview(){const a='ghostel',L=[];
  L.push(os(a,'ghostel-default','craig@host')+' '+os(a,'ghostel-color-green','~/code')+' $ ls'+os(a,'ghostel-fake-cursor',' ')+os(a,'ghostel-fake-cursor-box','[ ]'));
  L.push('');
  L.push(os(a,'ghostel-default','normal:')+'  '+os(a,'ghostel-color-black','black')+' '+os(a,'ghostel-color-red','red')+' '+os(a,'ghostel-color-green','green')+' '+os(a,'ghostel-color-yellow','yellow')+' '+os(a,'ghostel-color-blue','blue')+' '+os(a,'ghostel-color-magenta','magenta')+' '+os(a,'ghostel-color-cyan','cyan')+' '+os(a,'ghostel-color-white','white'));
  L.push(os(a,'ghostel-default','bright:')+'  '+os(a,'ghostel-color-bright-black','black')+' '+os(a,'ghostel-color-bright-red','red')+' '+os(a,'ghostel-color-bright-green','green')+' '+os(a,'ghostel-color-bright-yellow','yellow')+' '+os(a,'ghostel-color-bright-blue','blue')+' '+os(a,'ghostel-color-bright-magenta','magenta')+' '+os(a,'ghostel-color-bright-cyan','cyan')+' '+os(a,'ghostel-color-bright-white','white'));
  L.push('');
  L.push(os(a,'ghostel-default','default terminal output, 256-color text and a blinking ')+os(a,'ghostel-fake-cursor','cursor')+'.');
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderDashboardPreview(){const a='dashboard',L=[];
  L.push(os(a,'dashboard-text-banner','   ___ _ __ ___   __ _  ___ ___'));
  L.push(os(a,'dashboard-banner-logo-title','   Welcome back, Craig'));
  L.push('');
  L.push(os(a,'dashboard-heading','Recent Files'));
  L.push('  '+os(a,'dashboard-items-face','init.el'));
  L.push('  '+os(a,'dashboard-items-face','notes.org'));
  L.push(os(a,'dashboard-heading','Bookmarks'));
  L.push('  '+os(a,'dashboard-no-items-face','-- no items --'));
  L.push('');
  L.push(os(a,'dashboard-navigator','[ Projects ]  [ Recent ]  [ Agenda ]'));
  L.push(os(a,'dashboard-footer-icon-face','*')+' '+os(a,'dashboard-footer-face','Happy hacking, Craig!'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderMu4ePreview(){const a='mu4e',L=[];
  L.push(os(a,'mu4e-title-face','mu4e')+'  '+os(a,'mu4e-context-face','[Personal]')+'  '+os(a,'mu4e-ok-face','online')+'  '+os(a,'mu4e-warning-face','2 retry')+'  '+os(a,'mu4e-modeline-face','12/340'));
  L.push('');
  L.push(os(a,'mu4e-header-title-face','Date        Flags  From          Subject'));
  L.push(os(a,'mu4e-header-value-face','2026-06-08')+'  '+os(a,'mu4e-header-marks-face','N')+'    '+os(a,'mu4e-unread-face','Alice')+'         '+os(a,'mu4e-unread-face','Unread message'));
  L.push(os(a,'mu4e-header-value-face','2026-06-07')+'  '+os(a,'mu4e-header-marks-face','R')+'    '+os(a,'mu4e-header-face','Bob')+'           '+os(a,'mu4e-replied-face','Replied thread'));
  L.push(os(a,'mu4e-header-value-face','2026-06-06')+'  '+os(a,'mu4e-header-marks-face','F')+'    '+os(a,'mu4e-header-face','Carol')+'         '+os(a,'mu4e-forwarded-face','Forwarded note'));
  L.push(os(a,'mu4e-header-value-face','2026-06-05')+'  '+os(a,'mu4e-header-marks-face','D')+'    '+os(a,'mu4e-draft-face','(draft)')+'       '+os(a,'mu4e-draft-face','Draft in progress'));
  L.push(os(a,'mu4e-header-value-face','2026-06-04')+'  '+os(a,'mu4e-header-marks-face','T')+'    '+os(a,'mu4e-trashed-face','Dan')+'           '+os(a,'mu4e-moved-face','Trashed and moved'));
  L.push(os(a,'mu4e-header-highlight-face','2026-06-03  !    Eve           Flagged ')+os(a,'mu4e-flagged-face','important')+os(a,'mu4e-related-face',' (related)'));
  L.push('');
  L.push(os(a,'mu4e-header-key-face','From:')+'   '+os(a,'mu4e-contact-face','Alice &lt;alice@example.com&gt;'));
  L.push(os(a,'mu4e-header-key-face','To:')+'     '+os(a,'mu4e-special-header-value-face','craig, list@gnu.org'));
  L.push(os(a,'mu4e-header-key-face','Attach:')+' '+os(a,'mu4e-attach-number-face','[1]')+' report.pdf   link '+os(a,'mu4e-url-number-face','[2]')+' '+os(a,'mu4e-link-face','https://gnu.org'));
  L.push('');
  L.push('  body with a '+os(a,'mu4e-highlight-face','search hit')+' and '+os(a,'mu4e-region-code','code region')+'.');
  L.push('  '+os(a,'mu4e-cited-1-face','&gt; level 1')+' '+os(a,'mu4e-cited-2-face','&gt;&gt; 2')+' '+os(a,'mu4e-cited-3-face','&gt;&gt;&gt; 3')+' '+os(a,'mu4e-cited-4-face','&gt; 4')+' '+os(a,'mu4e-cited-5-face','&gt; 5')+' '+os(a,'mu4e-cited-6-face','&gt; 6')+' '+os(a,'mu4e-cited-7-face','&gt; 7'));
  L.push('  '+os(a,'mu4e-system-face','*** system message ***')+'   '+os(a,'mu4e-footer-face','-- sent with mu4e'));
  L.push('');
  L.push(os(a,'mu4e-compose-header-face','Subject:')+' new mail');
  L.push(os(a,'mu4e-compose-separator-face','--text follows this line--'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderLspPreview(){const a='lsp-mode',L=[];
  L.push(os(a,'lsp-signature-face','process(')+os(a,'lsp-signature-highlight-function-argument','items: list')+os(a,'lsp-signature-face',') -> None'));
  L.push(os(a,'lsp-signature-posframe',' docs: iterate over items and process each one '));
  L.push('');
  L.push('def process(items):');
  L.push('    n = len(items)'+os(a,'lsp-inlay-hint-type-face',': int'));
  L.push('    handle('+os(a,'lsp-inlay-hint-parameter-face','arg:')+'n)'+os(a,'lsp-inlay-hint-face','   # hint'));
  L.push('    '+os(a,'lsp-face-highlight-read','value')+' = '+os(a,'lsp-face-highlight-write','value')+' + '+os(a,'lsp-face-highlight-textual','value'));
  L.push('    rename '+os(a,'lsp-face-rename','oldName')+' to '+os(a,'lsp-rename-placeholder-face','newName'));
  L.push('    getName()   '+os(a,'lsp-details-face','str   the cached getter'));
  L.push('');
  L.push(os(a,'lsp-installation-buffer-face','Installing pyright...')+'   '+os(a,'lsp-installation-finished-buffer-face','done.'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderGitGutterPreview(){const a='git-gutter',L=[];
  L.push(os(a,'git-gutter:added','+')+os(a,'git-gutter:separator','|')+' added line of code');
  L.push(os(a,'git-gutter:modified','~')+os(a,'git-gutter:separator','|')+' modified line of code');
  L.push(os(a,'git-gutter:deleted','_')+os(a,'git-gutter:separator','|')+' (deleted lines marker)');
  L.push(os(a,'git-gutter:unchanged',' ')+os(a,'git-gutter:separator','|')+' '+os(a,'git-gutter:unchanged','unchanged line of code'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderFlycheckPreview(){const a='flycheck',L=[];
  L.push(os(a,'flycheck-fringe-error','E')+os(a,'flycheck-fringe-warning','W')+os(a,'flycheck-fringe-info','I')+'  x = '+os(a,'flycheck-error','undefined_name')+'('+os(a,'flycheck-warning','unused_arg')+')  '+os(a,'flycheck-info','# note'));
  L.push('       '+os(a,'flycheck-error-delimiter','[')+os(a,'flycheck-delimited-error','err')+os(a,'flycheck-error-delimiter',']'));
  L.push('');
  L.push(os(a,'flycheck-error-list-checker-name','pyright')+'   '+os(a,'flycheck-verify-select-checker','(selected checker)'));
  L.push(os(a,'flycheck-error-list-filename','main.py')+':'+os(a,'flycheck-error-list-line-number','12')+':'+os(a,'flycheck-error-list-column-number','4')+'  '+os(a,'flycheck-error-list-error','error')+'    '+os(a,'flycheck-error-list-error-message','undefined name x')+'  '+os(a,'flycheck-error-list-id','[E0602]'));
  L.push(os(a,'flycheck-error-list-filename','main.py')+':'+os(a,'flycheck-error-list-line-number','18')+':'+os(a,'flycheck-error-list-column-number','1')+'  '+os(a,'flycheck-error-list-warning','warning')+'  '+os(a,'flycheck-error-list-error-message','unused import')+'  '+os(a,'flycheck-error-list-id-with-explainer','[W0611?]'));
  L.push(os(a,'flycheck-error-list-highlight','main.py:20    '+os(a,'flycheck-error-list-info','info')+'     highlighted row'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderDiredPreview(){const a='dired',L=[];
  L.push(os(a,'dired-header','/home/craig/code:'));
  L.push('  '+os(a,'dired-perm-write','drwxr-xr-x')+'  craig  4096  '+os(a,'dired-directory','src/'));
  L.push('  -rw-r--r--  craig   120  notes.org');
  L.push('  '+os(a,'dired-perm-write','lrwxrwxrwx')+'  craig    18  '+os(a,'dired-symlink','latest -> v2.1'));
  L.push('  lrwxrwxrwx  craig    --  '+os(a,'dired-broken-symlink','dead -> gone'));
  L.push(os(a,'dired-flagged','D')+' -rw-r--r--  craig    40  deleteme.tmp');
  L.push(os(a,'dired-mark','*')+' '+os(a,'dired-marked','-rw-r--r--  craig   210  marked.txt'));
  L.push('  -rw-r--r--  craig     0  '+os(a,'dired-ignored','.gitignore'));
  L.push('  '+os(a,'dired-set-id','-rwsr-xr-x')+'  root    900  setuid.bin');
  L.push('  '+os(a,'dired-special','prw-r--r--')+'  craig     0  fifo.pipe');
  L.push(os(a,'dired-warning','! disk space low on /home'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderDirvishPreview(){const a='dirvish',L=[];
  L.push(os(a,'dirvish-inactive','~/code')+'    '+os(a,'dirvish-free-space','[free 24G]'));
  L.push(os(a,'dirvish-hl-line',' '+os(a,'dirvish-file-modes','-rw-r--r--')+' '+os(a,'dirvish-file-link-number','1')+' '+os(a,'dirvish-file-user-id','craig')+' '+os(a,'dirvish-file-group-id','staff')+' '+os(a,'dirvish-file-size','4.0K')+' '+os(a,'dirvish-file-time','Jun  8 02:24')+'  init.el '));
  L.push(' '+os(a,'dirvish-file-modes','drwxr-xr-x')+' '+os(a,'dirvish-file-link-number','5')+' '+os(a,'dirvish-file-user-id','craig')+' '+os(a,'dirvish-file-group-id','staff')+' '+os(a,'dirvish-file-size','  - ')+' '+os(a,'dirvish-file-time','Jun  7 18:00')+'  '+os(a,'dirvish-collapse-dir-face','src')+os(a,'dirvish-subtree-state','+')+os(a,'dirvish-subtree-guide',' |'));
  L.push(os(a,'dirvish-hl-line-inactive',' inactive-window current line '));
  L.push(' inode '+os(a,'dirvish-file-inode-number','1048576')+'  dev '+os(a,'dirvish-file-device-number','8,1')+'   '+os(a,'dirvish-collapse-empty-dir-face','empty/')+' '+os(a,'dirvish-collapse-file-face','file.txt'));
  L.push(' VC '+os(a,'dirvish-vc-added-state','A')+os(a,'dirvish-vc-edited-state','M')+os(a,'dirvish-vc-removed-state','D')+os(a,'dirvish-vc-conflict-state','C')+os(a,'dirvish-vc-locked-state','L')+os(a,'dirvish-vc-missing-state','!')+os(a,'dirvish-vc-needs-merge-face','m')+os(a,'dirvish-vc-needs-update-state','u')+os(a,'dirvish-vc-unregistered-face','?'));
  L.push(' git '+os(a,'dirvish-git-commit-message-face','feat: enlarge the picker'));
  L.push(' '+os(a,'dirvish-media-info-heading','Media')+'  '+os(a,'dirvish-media-info-property-key','Dimensions:')+' 1920x1080');
  L.push(' proc '+os(a,'dirvish-proc-running','running')+' / '+os(a,'dirvish-proc-finished','finished')+' / '+os(a,'dirvish-proc-failed','failed'));
  L.push(' narrow '+os(a,'dirvish-narrow-match-face-0','m0')+' '+os(a,'dirvish-narrow-match-face-1','m1')+' '+os(a,'dirvish-narrow-match-face-2','m2')+' '+os(a,'dirvish-narrow-match-face-3','m3')+os(a,'dirvish-narrow-split',' | ')+os(a,'dirvish-emerge-group-title','Group: images'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderCalibredbPreview(){const a='calibredb',L=[];
  L.push(os(a,'calibredb-search-header-library-name-face','Calibre')+'  '+os(a,'calibredb-search-header-library-path-face','~/books')+'  '+os(a,'calibredb-search-header-total-face','412 books')+'  '+os(a,'calibredb-search-header-filter-face','tag:scifi')+'  '+os(a,'calibredb-search-header-sort-face','sort:date')+'  '+os(a,'calibredb-search-header-highlight-face','[*]'));
  L.push('');
  L.push(os(a,'calibredb-id-face','1')+'  '+os(a,'calibredb-title-face','Dune')+'  '+os(a,'calibredb-author-face','Herbert')+'  '+os(a,'calibredb-format-face','EPUB')+'  '+os(a,'calibredb-size-face','2.1M')+'  '+os(a,'calibredb-tag-face',':scifi:')+'  '+os(a,'calibredb-date-face','2026-06-08'));
  L.push(os(a,'calibredb-mark-face','*')+os(a,'calibredb-id-face','2')+'  '+os(a,'calibredb-title-face','Foundation')+'  '+os(a,'calibredb-author-face','Asimov')+'  '+os(a,'calibredb-series-face','[Foundation #1]')+'  '+os(a,'calibredb-publisher-face','Bantam')+'  '+os(a,'calibredb-pubdate-face','1951'));
  L.push('');
  L.push(os(a,'calibredb-title-detailed-view-face','Foundation (detailed)')+'   '+os(a,'calibredb-language-face','eng')+'  '+os(a,'calibredb-favorite-face','* fav')+'  '+os(a,'calibredb-archive-face','archived'));
  L.push(os(a,'calibredb-ids-face','isbn:0553293354')+'  '+os(a,'calibredb-file-face','foundation.epub')+'  '+os(a,'calibredb-comment-face','A classic of the genre.'));
  L.push(os(a,'calibredb-edit-annotation-header-title-face','Annotations')+'  '+os(a,'calibredb-highlight-face','highlighted passage')+'  '+os(a,'calibredb-current-page-button-face','[page 42]')+'  '+os(a,'calibredb-mouse-face','hover row'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderErcPreview(){const a='erc',L=[];
  L.push(os(a,'erc-header-line',' #emacs on Libera.Chat  18 users '));
  L.push(os(a,'erc-timestamp-face','[10:24]')+' '+os(a,'erc-notice-face','*** alice has joined #emacs'));
  L.push(os(a,'erc-timestamp-face','[10:25]')+' &lt;'+os(a,'erc-my-nick-prefix-face','@')+os(a,'erc-my-nick-face','craig')+'&gt; '+os(a,'erc-input-face','hello everyone'));
  L.push(os(a,'erc-timestamp-face','[10:25]')+' &lt;'+os(a,'erc-nick-prefix-face','+')+os(a,'erc-nick-default-face','bob')+'&gt; '+os(a,'erc-default-face','hi craig, see ')+os(a,'erc-button','this link')+os(a,'erc-default-face',' cc ')+os(a,'erc-button-nick-default-face','@alice'));
  L.push(os(a,'erc-timestamp-face','[10:26]')+' '+os(a,'erc-action-face','* craig waves')+'   '+os(a,'erc-keyword-face','emacs')+' '+os(a,'erc-pal-face','&lt;friend&gt;')+' '+os(a,'erc-fool-face','&lt;troll&gt;')+' '+os(a,'erc-dangerous-host-face','&lt;bad@host&gt;'));
  L.push(os(a,'erc-timestamp-face','[10:27]')+' '+os(a,'erc-direct-msg-face','(DM)')+' &lt;'+os(a,'erc-nick-msg-face','bob')+'&gt; psst   '+os(a,'erc-current-nick-face','craig')+'   '+os(a,'erc-information','-info-'));
  L.push(os(a,'erc-error-face','*** ERROR: connection reset'));
  L.push(os(a,'erc-command-indicator-face','/help')+'   '+os(a,'erc-bold-face','bold')+' '+os(a,'erc-italic-face','italic')+' '+os(a,'erc-underline-face','underline')+' '+os(a,'erc-inverse-face','inverse')+' '+os(a,'erc-spoiler-face','spoiler'));
  L.push(os(a,'erc-keep-place-indicator-arrow','&gt;')+os(a,'erc-keep-place-indicator-line',' ---- last read ---- ')+os(a,'erc-fill-wrap-merge-indicator-face','+'));
  L.push(os(a,'erc-prompt-face','craig&gt;')+' '+os(a,'erc-input-face','type a message...'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderOrgdrillPreview(){const a='org-drill',L=[];
  L.push('Q: The capital of France is '+os(a,'org-drill-hidden-cloze-face','[...]')+'.');
  L.push('A: The capital of France is '+os(a,'org-drill-visible-cloze-face','Paris')+'.');
  L.push('   '+os(a,'org-drill-visible-cloze-hint-face','hint: P____'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderOrgnoterPreview(){const a='org-noter',L=[];
  L.push('org-noter   paper.pdf');
  L.push('  page 1   '+os(a,'org-noter-notes-exist-face','[notes]'));
  L.push('  page 2   '+os(a,'org-noter-no-notes-exist-face','[no notes]'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderSignelPreview(){const a='signel',L=[];
  L.push(os(a,'signel-timestamp-face','[10:24]')+' '+os(a,'signel-my-msg-face','Me: hey, are we still on for tonight?'));
  L.push(os(a,'signel-timestamp-face','[10:25]')+' '+os(a,'signel-other-msg-face','Alice: yes! see you at 7'));
  L.push(os(a,'signel-error-face','(failed to send -- retrying)'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderPearlPreview(){const a='pearl',L=[];
  L.push(os(a,'pearl-preamble-summary','PEARL-42  Fix the broken picker'));
  L.push('State: '+os(a,'pearl-modified-local','In Progress')+'   Priority: '+os(a,'pearl-modified-highlight','High')+'   Estimate: '+os(a,'pearl-modified-unknown','?'));
  L.push('  '+os(a,'pearl-editable-comment','&gt; add a comment (editable)'));
  L.push('  '+os(a,'pearl-readonly-comment','&gt; created by automation (read-only)'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderShrPreview(){const a='shr',L=[];
  L.push(os(a,'shr-text','shr renders nov (EPUB), eww (web), elfeed, and HTML mail.'));
  L.push('');
  L.push(os(a,'shr-h1','Chapter One: The Beginning'));
  L.push(os(a,'shr-h2','A Section Heading'));
  L.push(os(a,'shr-h3','A subsection')+'   '+os(a,'shr-h4','h4')+' / '+os(a,'shr-h5','h5')+' / '+os(a,'shr-h6','h6'));
  L.push(os(a,'shr-text','Body text flows in shr-text, with a ')+os(a,'shr-link','hyperlink')+os(a,'shr-text',' and a ')+os(a,'shr-selected-link','focused link')+os(a,'shr-text',','));
  L.push(os(a,'shr-text','some ')+os(a,'shr-code','inline_code()')+os(a,'shr-text',', a ')+os(a,'shr-mark','highlighted mark')+os(a,'shr-text',', ')+os(a,'shr-strike-through','struck out')+os(a,'shr-text',', a footnote')+os(a,'shr-sup','[1]')+os(a,'shr-text',','));
  L.push(os(a,'shr-text','an ')+os(a,'shr-abbreviation','HTML')+os(a,'shr-text',' abbreviation, and an ')+os(a,'shr-sliced-image','[image]')+os(a,'shr-text',' slice.'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderSlackPreview(){const a='slack',L=[];
  L.push(os(a,'slack-room-info-title-room-name-face','#general')+'  '+os(a,'slack-room-info-title-face','Acme Workspace'));
  L.push(os(a,'slack-room-info-section-title-face','Topic')+'  '+os(a,'slack-room-info-section-label-face','daily standup')+'   '+os(a,'slack-room-unread-face','3 unread'));
  L.push(os(a,'slack-new-message-marker-face','---------------- new messages ----------------'));
  L.push(os(a,'slack-message-output-header','craig  10:24'));
  L.push('  '+os(a,'slack-message-output-text','hey ')+os(a,'slack-message-mention-me-face','@craig')+os(a,'slack-message-output-text',', see ')+os(a,'slack-message-mention-face','@alice')+os(a,'slack-message-output-text',' in ')+os(a,'slack-channel-button-face','#general')+'  '+os(a,'slack-message-mention-keyword-face','urgent'));
  L.push('  '+os(a,'slack-mrkdwn-bold-face','*bold*')+' '+os(a,'slack-mrkdwn-italic-face','_italic_')+' '+os(a,'slack-mrkdwn-code-face','`code`')+' '+os(a,'slack-mrkdwn-strike-face','~strike~'));
  L.push('  '+os(a,'slack-mrkdwn-blockquote-face','&gt; quoted')+'   '+os(a,'slack-mrkdwn-list-face','- item'));
  L.push('  '+os(a,'slack-mrkdwn-code-block-face','``` code block ```'));
  L.push('  '+os(a,'slack-message-output-reaction',':thumbsup: 3')+'  '+os(a,'slack-message-output-reaction-pressed',':heart: 1')+'   '+os(a,'slack-message-deleted-face','(message deleted)'));
  L.push('  '+os(a,'slack-all-thread-buffer-thread-header-face','Thread: 2 replies'));
  L.push(os(a,'slack-attachment-header','Attachment')+'  '+os(a,'slack-attachment-field-title','Field:')+' val  '+os(a,'slack-message-attachment-preview-header-face','Preview')+'  '+os(a,'slack-preview-face','snippet')+os(a,'slack-attachment-pad',' | ')+os(a,'slack-attachment-footer','footer'));
  L.push(os(a,'slack-block-highlight-source-overlay-face',' highlighted source block '));
  L.push('Actions: '+os(a,'slack-message-action-face','Edit')+' '+os(a,'slack-message-action-primary-face','Approve')+' '+os(a,'slack-message-action-danger-face','Delete'));
  L.push('Blocks: '+os(a,'slack-button-block-element-face','[Button]')+os(a,'slack-button-primary-block-element-face','[Primary]')+os(a,'slack-button-danger-block-element-face','[Danger]')+os(a,'slack-select-block-element-face','[Select v]')+os(a,'slack-overflow-block-element-face','[...]')+os(a,'slack-date-picker-block-element-face','[Date]'));
  L.push('Dialog: '+os(a,'slack-dialog-title-face','Title')+'  '+os(a,'slack-dialog-element-label-face','Label')+' '+os(a,'slack-dialog-element-hint-face','(hint)')+' '+os(a,'slack-dialog-element-placeholder-face','placeholder')+' '+os(a,'slack-dialog-element-error-face','error')+'  '+os(a,'slack-dialog-select-element-input-face','[input v]')+' '+os(a,'slack-dialog-submit-button-face','[Submit]')+os(a,'slack-dialog-cancel-button-face','[Cancel]'));
  L.push('Users: '+os(a,'slack-user-active-face','alice (active)')+' '+os(a,'slack-user-dnd-face','bob (dnd)')+'  '+os(a,'slack-profile-image-face','[img]')+' '+os(a,'slack-user-profile-header-face','Profile')+' '+os(a,'slack-user-profile-property-name-face','Title:')+' Dev');
  L.push('Search: '+os(a,'slack-search-result-message-header-face','#general')+' '+os(a,'slack-search-result-message-username-face','craig'));
  L.push('Modeline: '+os(a,'slack-modeline-has-unreads-face','* unreads')+' '+os(a,'slack-modeline-channel-has-unreads-face','#ch')+' '+os(a,'slack-modeline-thread-has-unreads-face','thread'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function renderTelegaPreview(){const a='telega',L=[];
  L.push(os(a,'telega-root-heading','Telegram')+'  '+os(a,'telega-tracking','[tracking]')+'  '+os(a,'telega-unread-unmuted-modeline','5 unread'));
  L.push(os(a,'telega-has-chatbuf-brackets','[')+os(a,'telega-username','Alice')+os(a,'telega-has-chatbuf-brackets',']')+' '+os(a,'telega-user-online-status','online')+'  '+os(a,'telega-unmuted-count','3')+' '+os(a,'telega-mention-count','@2')+os(a,'telega-delim-face',' | ')+os(a,'telega-secret-title','Secret')+' '+os(a,'telega-muted-count','muted'));
  L.push(os(a,'telega-username','Bob')+' '+os(a,'telega-user-non-online-status','last seen recently')+'   '+os(a,'telega-contact-birthdays-today','birthday today')+'   '+os(a,'telega-shadow','shadow')+' '+os(a,'telega-link','link')+' '+os(a,'telega-blue','blue')+' '+os(a,'telega-red','red'));
  L.push('');
  L.push(os(a,'telega-msg-heading','Today'));
  L.push(os(a,'telega-msg-user-title','Alice')+'  '+os(a,'telega-msg-inline-reply','| reply to Bob')+'  '+os(a,'telega-msg-inline-forward','fwd from Carol')+'  '+os(a,'telega-msg-inline-other','via bot'));
  L.push('  '+os(a,'telega-entity-type-bold','bold')+' '+os(a,'telega-entity-type-italic','italic')+' '+os(a,'telega-entity-type-underline','underline')+' '+os(a,'telega-entity-type-strikethrough','strike')+' '+os(a,'telega-entity-type-code','code')+' '+os(a,'telega-entity-type-spoiler','spoiler'));
  L.push('  '+os(a,'telega-entity-type-pre','pre block')+'  '+os(a,'telega-entity-type-blockquote','&gt; quote')+'  '+os(a,'telega-entity-type-mention','@user')+' '+os(a,'telega-entity-type-hashtag','#tag')+' '+os(a,'telega-entity-type-cashtag','$USD')+' '+os(a,'telega-entity-type-botcommand','/start')+' '+os(a,'telega-entity-type-texturl','link'));
  L.push(os(a,'telega-msg-self-title','Me')+'  '+os(a,'telega-reaction',':+1: 2')+' '+os(a,'telega-reaction-chosen',':heart: 1')+' '+os(a,'telega-reaction-paid',':star: 5')+' '+os(a,'telega-reaction-paid-chosen',':star: paid')+'   '+os(a,'telega-msg-deleted','(deleted)')+' '+os(a,'telega-msg-sponsored','Sponsored'));
  L.push('  checklist '+os(a,'telega-checklist-stats-done','2 done')+' / '+os(a,'telega-checklist-stats-todo','3 todo')+'   '+os(a,'telega-highlight-text-face','search hit')+'   '+os(a,'telega-button-highlight','[active btn]'));
  L.push(os(a,'telega-chat-prompt','&gt;')+' '+os(a,'telega-chat-prompt-aux','reply')+' '+os(a,'telega-chat-input-attachment','[photo.jpg]')+'   '+os(a,'telega-topic-button','# Topic')+'   '+os(a,'telega-filter-active','Main')+' '+os(a,'telega-filter-button-active','[Unread]')+os(a,'telega-filter-button-inactive','[All]'));
  L.push('Buttons '+os(a,'telega-box-button','[box]')+os(a,'telega-box-button-active','[on]')+os(a,'telega-box-button-default-active','[def]')+os(a,'telega-box-button-default-passive','[def-]')+os(a,'telega-box-button-primary-active','[pri]')+os(a,'telega-box-button-primary-passive','[pri-]')+os(a,'telega-box-button-success-active','[ok]')+os(a,'telega-box-button-success-passive','[ok-]'));
  L.push('        '+os(a,'telega-box-button-danger-active','[del]')+os(a,'telega-box-button-danger-passive','[del-]')+os(a,'telega-box-button-ui-active','[ui]')+os(a,'telega-box-button-ui-passive','[ui-]')+os(a,'telega-box-button2-active','[b2]')+os(a,'telega-box-button2-passive','[b2-]')+os(a,'telega-box-button2-white-foreground','[b2w]'));
  L.push('Describe '+os(a,'telega-describe-section-title','Section')+' '+os(a,'telega-describe-subsection-title','Sub')+' '+os(a,'telega-describe-item-title','Item:')+'   enckey '+os(a,'telega-enckey-00','00')+os(a,'telega-enckey-01','01')+os(a,'telega-enckey-10','10')+os(a,'telega-enckey-11','11'));
  L.push('Palette '+os(a,'telega-palette-builtin-blue','blue')+' '+os(a,'telega-palette-builtin-green','green')+' '+os(a,'telega-palette-builtin-orange','orange')+' '+os(a,'telega-palette-builtin-purple','purple'));
  L.push(os(a,'telega-link-preview-sitename','example.com')+'  '+os(a,'telega-link-preview-title','Link preview title'));
  L.push('Webpage '+os(a,'telega-webpage-title','Title')+' '+os(a,'telega-webpage-subtitle','Subtitle')+' '+os(a,'telega-webpage-header','Header')+' '+os(a,'telega-webpage-subheader','Subheader')+' '+os(a,'telega-webpage-outline','outline')+' '+os(a,'telega-webpage-fixed','fixed')+' '+os(a,'telega-webpage-preformatted','pre')+' '+os(a,'telega-webpage-marked','marked')+' '+os(a,'telega-webpage-strike-through','strike')+' '+os(a,'telega-webpage-chat-link','chat-link'));
  return `<div style="padding:12px 16px;font:12pt/1.7 monospace;white-space:pre">${L.join('\n')}</div>`;}
function genericPreview(app){let h='<div style="padding:10px 14px;font:12pt/1.8 monospace">';for(const [face,label,def] of APPS[app].faces){const f=PKGMAP[app][face],efg=effFg(pkgEffFg(app,face)),ebg=pkgEffBg(app,face);h+=`<div data-face="${face}" style="color:${efg};${ebg?'background:'+ebg+';':''}font-weight:${f.bold?'bold':'normal'};font-style:${f.italic?'italic':'normal'};font-size:${(f.height||1)}em">${esc(label)}</div>`;}return h+'</div>';}
function buildPkgPreview(){const app=curApp(),p=document.getElementById('pkgpreview');if(!p)return;const pv=APPS[app].preview;const bespoke=['org','magit','elfeed','ghostel','dashboard','mu4e','lsp','gitgutter','flycheck','dired','dirvish','calibredb','erc','orgdrill','orgnoter','signel','pearl','slack','telega','shr'].includes(pv);p.innerHTML=pv==='org'?renderOrgPreview():pv==='magit'?renderMagitPreview():pv==='elfeed'?renderElfeedPreview():pv==='ghostel'?renderGhostelPreview():pv==='dashboard'?renderDashboardPreview():pv==='mu4e'?renderMu4ePreview():pv==='lsp'?renderLspPreview():pv==='gitgutter'?renderGitGutterPreview():pv==='flycheck'?renderFlycheckPreview():pv==='dired'?renderDiredPreview():pv==='dirvish'?renderDirvishPreview():pv==='calibredb'?renderCalibredbPreview():pv==='erc'?renderErcPreview():pv==='orgdrill'?renderOrgdrillPreview():pv==='orgnoter'?renderOrgnoterPreview():pv==='signel'?renderSignelPreview():pv==='pearl'?renderPearlPreview():pv==='slack'?renderSlackPreview():pv==='telega'?renderTelegaPreview():pv==='shr'?renderShrPreview():genericPreview(app);p.style.background=MAP['bg'];p.onclick=(e)=>{const u=e.target.closest('[data-face]');if(u)flashPkg(u.dataset.face);};const lbl=document.getElementById('pkgprevlabel');if(lbl)lbl.textContent=bespoke?(APPS[app].label+' preview'):'preview (generic — face names in their own colors)';}
function resetApp(){const app=curApp();PKGMAP[app]={};for(const [face,label,d] of APPS[app].faces)PKGMAP[app][face]=seedFace(d);pkgChanged();}
function syncPkgHeight(){const t=document.getElementById('pkgtable'),m=document.getElementById('pkgpreview');if(!t||!m)return;const lb=m.previousElementSibling,lbh=lb?lb.getBoundingClientRect().height+10:30;m.style.height=Math.max(t.getBoundingClientRect().height-lbh,220)+'px';}
// --- worst-case readout for the covered overlay faces (spec Phase 4) ---------
// Default WCAG target for the worst-case verdict (AA). AAA is selectable.
let WORST_TARGET=4.5;
// The live v1 foreground set for a covered overlay face: the syntax-token colors
// (every assignable category except the ground) plus the default foreground.
function fgSetForFace(face){
  const syntaxAssignments=CATS.filter(c=>c[0]!=='bg'&&c[0]!=='p').map(c=>({role:c[0],hex:effFg(MAP[c[0]])}));
  return fgSetFor(face,{covered:COVERED_FACES,syntaxAssignments,defaultFg:MAP['p']});
}
// The worst-case contrast cell for a covered face: the floor over its foreground
// set against its effective background, naming the limiting foreground. Returns
// null for an out-of-scope face so the caller keeps the single-pair readout.
function worstCellHtml(face){
  const r=fgSetForFace(face);
  if(r.reason==='out-of-scope')return null;
  if(r.reason==='empty'||!r.set.length)return '<span title="this overlay has no syntax foreground set yet">no fg set</span>';
  const bg=effBg(uf(face).bg),fl=floor(bg,r.set),verdict=fl.ratio>=WORST_TARGET?'PASS':'FAIL';
  const s='worst: '+fl.limitingLabel+' '+fl.limitingHex+' — '+fl.ratio.toFixed(1)+' '+verdict;
  return `<span style="color:${ratingColor(fl.ratio)}" title="${esc(s)}">${esc(s)}</span>`;
}
// Repaint every covered overlay face (their floors depend on the syntax palette,
// so a syntax-color edit has to refresh them even though it doesn't rebuild the table).
function repaintCovered(){COVERED_FACES.forEach(f=>{if(UIMAP[f]&&document.getElementById('uicr-'+f))paintUI(f);});}
function paintUI(face){const pv=document.getElementById('uiprev-'+face);if(!pv)return;const o=UIMAP[face];pv.style.color=effFg(o.fg);pv.style.background=effBg(o.bg);pv.style.fontWeight=o.bold?'bold':'normal';pv.style.fontStyle=o.italic?'italic':'normal';pv.style.textDecoration=(o.underline?'underline ':'')+(o.strike?'line-through':'')||'none';pv.style.boxShadow=boxCss(o.box,effBg(o.bg));
  const cr=document.getElementById('uicr-'+face);if(cr){const w=worstCellHtml(face);if(w!==null){cr.innerHTML=w;}else{const efg=effFg(o.fg),ebg=effBg(o.bg),r=contrast(efg,ebg);cr.innerHTML=crHtml(r);}}}
function buildUITable(){
  const tb=document.getElementById('uibody');tb.innerHTML='';
  for(const [face,label,ex] of UI_FACES){
    const tr=document.createElement('tr');tr.dataset.face=face;
    const c0=document.createElement('td');c0.className='cat';c0.textContent=label;c0.style.cursor='pointer';c0.title='flash this face in the live preview';c0.onclick=()=>flashUiPreview(face);
    const fgSel=uiSelect(face,'fg'),bgSel=uiSelect(face,'bg');
    const cF=document.createElement('td');cF.appendChild(fgSel);
    const cB=document.createElement('td');cB.appendChild(bgSel);
    const cS=document.createElement('td');
    const stBtns=mkStyleButtons(at=>UIMAP[face][at],at=>{UIMAP[face][at]=!UIMAP[face][at];paintUI(face);buildMockFrame();});
    stBtns.forEach(b=>cS.appendChild(b));
    const cC=document.createElement('td');cC.id='uicr-'+face;cC.style.whiteSpace='nowrap';cC.style.fontSize='10pt';
    const cP=document.createElement('td');cP.className='ex';cP.id='uiprev-'+face;cP.textContent=ex;cP.style.padding='4px 10px';cP.style.borderRadius='4px';
    const cX=document.createElement('td');const boxSel=mkBoxSelect(()=>UIMAP[face].box,b=>{UIMAP[face].box=b;paintUI(face);buildMockFrame();});cX.appendChild(boxSel);
    const cL=mkLockCell('ui:'+face,[fgSel,bgSel,...stBtns,boxSel]);
    tr.appendChild(c0);tr.appendChild(cL);tr.appendChild(cF);tr.appendChild(cB);tr.appendChild(cS);tr.appendChild(cC);tr.appendChild(cP);tr.appendChild(cX);tb.appendChild(tr);paintUI(face);
  }
  applyTableSort('uibody');
  updateLockToggle('ui');
}
// Generic header-click sort, shared by all three tables. Reads a swatch
// dropdown's value, a select value, a numeric input, or cell text (numeric when
// the text leads with a number, e.g. contrast or size). The UI and package
// tables remember the sort (applyTableSort runs on rebuild) so editing a row
// does not reset it; the syntax table sorts on click only.
let tableSort={};
function cellVal(td){if(!td)return '';const dd=td.querySelector('.cdd');if(dd)return (dd.dataset.val||'').toLowerCase();const s=td.querySelector('select');if(s)return s.value.toLowerCase();const i=td.querySelector('input');if(i)return parseFloat(i.value)||0;const t=td.innerText.trim();const n=parseFloat(t);return (!isNaN(n)&&/^[-\d.]/.test(t))?n:t.toLowerCase();}
function srtTable(tbId,col){tableSort[tbId]={col,asc:!(tableSort[tbId]&&tableSort[tbId].col===col&&tableSort[tbId].asc)};applyTableSort(tbId);}
function applyTableSort(tbId){const s=tableSort[tbId];if(!s)return;const tb=document.getElementById(tbId);if(!tb)return;const dir=s.asc?1:-1;const r=[...tb.rows];r.sort((a,b)=>{const x=cellVal(a.cells[s.col]),y=cellVal(b.cells[s.col]);return ((typeof x==='number'&&typeof y==='number')?x-y:(x<y?-1:x>y?1:0))*dir;});r.forEach(x=>tb.appendChild(x));}
buildLangSel();buildAppSel();renderPalette();buildTable();buildUITable();renderCode();applyGround();updateTitle();initPicker();buildPkgTable();buildPkgPreview();syncMockHeight();syncPkgHeight();
addEventListener('resize',()=>{syncMockHeight();syncPkgHeight();});
BROWSER_GATES_J

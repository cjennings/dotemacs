const SAMPLES=SAMPLES_J, CATS=CATS_J, UI_FACES=UIFACES_J, APPS=APPS_J;
const COLOR_NAMES=COLOR_NAMES_J;
const FACE_DOCS=FACE_DOCS_J, SYNTAX_DOCS=SYNTAX_DOCS_J; // face/category -> docstring first line, for element hovers
let MAP=MAP_J, PALETTE=PALETTE_J, SYNTAX=SYNTAX_J, UIMAP=UIMAP_J;
let LOCKED=new Set(LOCKS_J);   // rows whose choice is decided (controls disabled, skipped by erase/reset batch actions)
const DELTAE_MIN=0.02; // OKLab ΔE below this = colors too close to tell apart (perceptual-metrics spec)
const DEFAULT_UIMAP=JSON.parse(JSON.stringify(UIMAP));
function syntaxBlank(k){return {fg:MAP[k]||null,bg:null,'distant-fg':null,family:null,weight:null,slant:null,underline:null,strike:null,overline:null,box:null,inverse:false,extend:false,inherit:null,height:null};}
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
function buildLangSel(){const s=document.getElementById('langsel');s.innerHTML='';for(const lang in SAMPLES){const o=document.createElement('option');o.value=lang;o.textContent=lang;s.appendChild(o);}}
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
// Custom color dropdown: a real swatch + name + hex per row, since native
// <option> background colors render unreliably on Linux Chrome. The popup is
// fixed-positioned on <body> so a table's overflow can't clip it.
let _ddPop=null;
function closeColorDropdown(){if(_ddPop){_ddPop.remove();_ddPop=null;}}
document.addEventListener('pointerdown',e=>{if(_ddPop&&!e.target.closest('.cdd')&&!e.target.closest('.cddpop'))closeColorDropdown();});
function mkColorDropdown(options,cur,onPick,opts={}){
  const wrap=document.createElement('div');wrap.className='cstep';
  const left=document.createElement('button'),right=document.createElement('button');
  left.className='cstepbtn';right.className='cstepbtn';left.type=right.type='button';
  left.textContent='‹';right.textContent='›';left.title='move to next darker color in this column';right.title='move to next lighter color in this column';
  const t=document.createElement('div');t.className='cdd'+(opts.compact?' compact':'');t.tabIndex=0;
  const nameOf=h=>{const o=options.find(p=>p[0]===h);return o?o[1]:(h||'none');};
  const displayHex=h=>h||(opts.defaultHex||'');
  const displayName=h=>h?nameOf(h):(opts.defaultName||nameOf(h));
  function step(dir){if(wrap.dataset.locked==='1')return;const next=spanNeighborHex(cur,PALETTE,groundPair(),dir);if(!next)return;cur=next;paint();onPick(next);}
  function paintStepButtons(){
    const locked=wrap.dataset.locked==='1';
    left.disabled=locked||!spanNeighborHex(cur,PALETTE,groundPair(),-1);
    right.disabled=locked||!spanNeighborHex(cur,PALETTE,groundPair(),1);
  }
  function paint(){const shown=displayHex(cur),nm=displayName(cur),ttl=cur?(nm+' '+cur):(nm+(shown?' -> '+shown:''));t.style.background=shown||'#161412';t.style.color=shown?textOn(shown):'#b4b1a2';t.dataset.val=cur||'';t.title=ttl;t.classList.toggle('is-default',!cur);t.classList.toggle('gone',!!cur&&nameOf(cur)==='(gone)');
    t.innerHTML=opts.compact?`<span class="cddsw" style="background:${shown||'transparent'}"></span>`:`<span class="cddsw" style="background:${shown||'transparent'}"></span>${esc(nm)}`;paintStepButtons();}
  paint();
  left.onclick=e=>{e.stopPropagation();step(-1);};
  right.onclick=e=>{e.stopPropagation();step(1);};
  t.onclick=(e)=>{e.stopPropagation();if(wrap.dataset.locked==='1')return;if(_ddPop){closeColorDropdown();return;}
    // 2D gallery: a grid of swatches in the palette-panel shape (ground strip,
    // then one row per family) instead of a long vertical list. galleryModel is
    // the shared pure layout (app-core.js).
    const pop=document.createElement('div');pop.className='cddpop cddgrid';
    const model=galleryModel(cur,PALETTE,groundPair());
    const pick=(hex)=>{cur=hex;paint();closeColorDropdown();onPick(hex);};
    const head=document.createElement('div');head.className='cddghead';
    const def=document.createElement('button');def.type='button';
    def.className='cddgdef'+(model.default.selected?' sel':'');
    def.textContent=opts.defaultName||'default';def.title='clear — use the default';
    def.onclick=(ev)=>{ev.stopPropagation();pick('');};head.appendChild(def);
    if(model.gone){const g=document.createElement('span');g.className='cddgc gone sel';
      g.style.background=model.gone.hex;g.title='(gone) '+model.gone.hex;head.appendChild(g);
      const gl=document.createElement('span');gl.className='cddglbl';gl.textContent='(gone) '+model.gone.hex;head.appendChild(gl);}
    pop.appendChild(head);
    for(const row of model.rows){const rr=document.createElement('div');rr.className='cddgrow';
      for(const c of row.cells){const sw=document.createElement('button');sw.type='button';
        sw.className='cddgc'+(c.selected?' sel':'');sw.style.background=c.hex;
        sw.dataset.hex=c.hex;sw.dataset.name=c.name;sw.title=c.name+' '+c.hex;
        sw.onclick=(ev)=>{ev.stopPropagation();pick(c.hex);};rr.appendChild(sw);}
      pop.appendChild(rr);}
    document.body.appendChild(pop);const r=t.getBoundingClientRect();
    pop.style.left=r.left+'px';pop.style.minWidth=r.width+'px';
    pop.style.top=(r.bottom+2)+'px';
    const ph=pop.getBoundingClientRect().height;
    if(r.bottom+ph>window.innerHeight-6)pop.style.top=Math.max(6,r.top-ph-2)+'px';
    const pr=pop.getBoundingClientRect();
    if(pr.right>window.innerWidth-6)pop.style.left=Math.max(6,window.innerWidth-6-pr.width)+'px';
    _ddPop=pop;};
  t.setValue=h=>{cur=h;paint();};
  wrap.setValue=h=>{cur=h;paint();};
  wrap.syncLocked=paintStepButtons;
  wrap.appendChild(left);wrap.appendChild(t);wrap.appendChild(right);paintStepButtons();
  return wrap;}
// Standard option list for a swatch dropdown: a "default" entry, then the
// palette in the same ground/column order as the palette panel. If cur is set
// but no longer in the palette, surface it as a "(gone)" entry so the row still
// shows what it points at. Shared by all three tiers.
function ddList(cur){return paletteOptionList(cur,PALETTE,groundPair());}
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
      else{el.dataset.locked=on?'1':'';el.classList.toggle('locked',on);if(el.syncLocked)el.syncLocked();}});}
  lk.onclick=()=>{LOCKED.has(lockKey)?LOCKED.delete(lockKey):LOCKED.add(lockKey);paint();updateLockToggles();};
  paint();td.appendChild(lk);return td;}
// The in-row style controls, shared by the syntax / UI / package tables: a weight
// selector, a slant selector, and box-like underline and strike controls. Each
// edit mutates the face object and calls onChange to repaint. Returns the control
// elements so the caller lays them out and hands them to mkLockCell.
const WEIGHT_OPTS=[['','wt'],['light','light'],['normal','normal'],['medium','medium'],['semibold','semi'],['bold','bold'],['heavy','heavy']];
const SLANT_OPTS=[['','sl'],['normal','normal'],['italic','italic'],['oblique','oblique']];
function mkEnumSelect(opts,get,set,title){
  const s=document.createElement('select');s.className='chip stylesel';s.title=title;
  for(const [v,label] of opts){const o=document.createElement('option');o.value=v;o.textContent=label;s.appendChild(o);}
  s.value=get()||'';s.onchange=()=>set(s.value||null);return s;}
// Underline control: none / line / wave glyph buttons plus a color swatch shown
// while a style is active. Mirrors mkBoxControl; get()/set() read and write the
// underline object ({style,color}) or null.
function mkLineStyleControl(states,get,set,opts={}){const wrap=document.createElement('div');wrap.className='boxctl';
  const cluster=document.createElement('div');cluster.className='boxcluster';const btns={};
  states.forEach(([v,title,glyph])=>{const b=document.createElement('button');b.className='boxbtn';b.dataset.style=v;b.textContent=glyph;b.title=title;
    b.onclick=()=>{const cur=get();set(v?(opts.toState?opts.toState(v,cur):Object.assign({color:(cur&&cur.color)||null},opts.styled?{style:v}:{})):null);paint();};
    cluster.appendChild(b);btns[v]=b;});
  const dd=mkColorDropdown(ddList((get()&&get().color)||''),(get()&&get().color)||'',h=>{const cur=get();if(!cur)return;set(Object.assign({},cur,{color:h||null}));paint();},{compact:true,defaultHex:opts.defaultHex});
  function paint(){const cur=get(),active=opts.styled?(cur&&cur.style?cur.style:''):(cur?'on':'');
    for(const v in btns)btns[v].classList.toggle('on',v===active);
    dd.style.display=active?'':'none';dd.setValue(cur&&cur.color?cur.color:'');
    const locked=wrap.dataset.locked==='1';for(const v in btns)btns[v].disabled=locked;
    const ddoff=locked||!active;dd.dataset.locked=ddoff?'1':'';dd.classList.toggle('locked',ddoff);if(dd.syncLocked)dd.syncLocked();}
  wrap.syncLocked=()=>paint();wrap.append(cluster,dd);paint();return wrap;}
function mkUnderlineControl(get,set,opts={}){
  return mkLineStyleControl([['','no underline',''],['line','underline','_'],['wave','wavy underline','~']],get,set,Object.assign({styled:true},opts));}
function mkStrikeControl(get,set,opts={}){
  return mkLineStyleControl([['','no strike',''],['on','strike-through','S']],get,set,Object.assign({styled:false},opts));}
// In-row style controls: weight + slant selectors and a strike control. The
// underline control lives in the per-row expander (it carries the wave/color
// detail), keeping the row compact.
function mkStyleControls(face,onChange,opts={}){
  const w=mkEnumSelect(WEIGHT_OPTS,()=>face.weight,v=>{face.weight=v;onChange();},'font weight');
  const s=mkEnumSelect(SLANT_OPTS,()=>face.slant,v=>{face.slant=v;onChange();},'font slant');
  const k=mkStrikeControl(()=>face.strike,v=>{face.strike=v;onChange();},opts);
  return [w,s,k];}
function mkOverlineControl(get,set,opts={}){
  return mkLineStyleControl([['','no overline',''],['on','overline','O']],get,set,Object.assign({styled:false},opts));}
function mkCheck(get,set){const c=document.createElement('input');c.type='checkbox';c.className='detailcheck';c.checked=!!get();c.onchange=()=>set(c.checked);return c;}
// The per-row attribute editor revealed by the expander: distant-fg, family,
// overline, inverse, extend, and (for ui/syntax, where inherit/height have no
// inline column) inherit + height. Each control mutates FACE and calls onChange.
// Returns the element plus the interactive controls so the row's lock cell can
// disable them. opts.inheritOptions and opts.showInheritHeight gate the last two.
function mkDetailEditor(face,onChange,opts={}){
  const wrap=document.createElement('div');wrap.className='detailedit';const locks=[];
  const add=(label,el)=>{const g=document.createElement('label');g.className='detailfield';const s=document.createElement('span');s.textContent=label;g.append(s,el);wrap.appendChild(g);locks.push(el);};
  const df=mkColorDropdown(ddList(face['distant-fg']||''),face['distant-fg']||'',h=>{face['distant-fg']=h||null;onChange();},{compact:true,defaultHex:opts.defaultHex});
  add('distant fg',df);
  const fam=document.createElement('input');fam.type='text';fam.className='detailinput';fam.placeholder='font family';fam.value=face.family||'';fam.onchange=()=>{face.family=fam.value.trim()||null;onChange();};
  add('family',fam);
  add('underline',mkUnderlineControl(()=>face.underline,v=>{face.underline=v;onChange();},opts));
  add('overline',mkOverlineControl(()=>face.overline,v=>{face.overline=v;onChange();},opts));
  add('inverse',mkCheck(()=>face.inverse,v=>{face.inverse=v;onChange();}));
  add('extend',mkCheck(()=>face.extend,v=>{face.extend=v;onChange();}));
  if(opts.showInheritHeight){
    const isel=document.createElement('select');isel.className='chip detailsel';
    (opts.inheritOptions||['']).forEach(o=>{const op=document.createElement('option');op.value=o;op.textContent=o||'— none —';isel.appendChild(op);});
    isel.value=face.inherit||'';isel.onchange=()=>{face.inherit=isel.value||null;onChange();};add('inherit',isel);
    const hin=document.createElement('input');hin.type='number';hin.min=''+HEIGHT_MIN;hin.max=''+HEIGHT_MAX;hin.step='0.05';hin.className='hstep';hin.value=face.height||1;hin.onchange=()=>{const raw=hin.value,h=clampHeight(raw);face.height=h;hin.value=h==null?1:h;if(h!=null&&parseFloat(raw)!==h)notify('height clamped to '+h+' (allowed '+HEIGHT_MIN+'–'+HEIGHT_MAX+')',false);onChange();};add('height',hin);
  }
  return {el:wrap,locks};}
// Wire a per-row expander: a toggle button plus a hidden detail row (colspan
// across the table) holding mkDetailEditor. The caller drops the button into a
// cell, adds the returned locks to the row's lock cell, and inserts detailRow
// right after the main row.
function mkExpander(face,colspan,onChange,opts={}){
  const detail=document.createElement('tr');detail.className='detailrow';detail.style.display='none';
  const btn=document.createElement('button');btn.className='exptoggle';btn.textContent='⋯';
  // Flag the toggle when collapsed and at least one hidden attribute differs from
  // the default, so a non-default attribute is never invisible. ndCheck re-runs
  // after every edit (for tiers whose onChange does not rebuild the row).
  const ndCheck=opts.ndCheck||(()=>false);
  const refreshNd=()=>{const nd=ndCheck();btn.classList.toggle('exp-nd',nd);btn.title=nd?'more attributes (some differ from default)':'more attributes';};
  const wrapped=()=>{onChange();refreshNd();};
  const td=document.createElement('td');td.colSpan=colspan;const {el,locks}=mkDetailEditor(face,wrapped,opts);td.appendChild(el);detail.appendChild(td);
  btn.onclick=()=>{const open=detail.style.display==='none';detail.style.display=open?'':'none';btn.classList.toggle('on',open);};
  refreshNd();
  return {btn,detail,locks};}
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
  buildTable();buildUITable();if(document.getElementById('pkgbody'))buildPkgTable();
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
function updateLockToggles(){updateLockToggle('syntax');updateLockToggle('ui');updateLockToggle('pkg');}
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
    const exp=mkExpander(syntaxFace(kind),tableColCount('legtable'),()=>{styleEx();renderCode();},{showInheritHeight:true,inheritOptions:[''].concat(BASE_INHERITS),defaultHex:rowFg(),ndCheck:()=>overflowNonDefault(syntaxFace(kind),DEFAULT_SYNTAX[kind],true)});
    exp.detail.dataset.detailFor=kind;
    const lkTd=mkLockCell(kind,[dd,bgd,...stCtls,boxCtl,...exp.locks]);
    const c2=document.createElement('td');c2.className='cat';c2.title=composeHoverTitle(SYNTAX_DOCS[kind],c2.title);c2.appendChild(exp.btn);
    const c2lbl=document.createElement('span');c2lbl.textContent=' '+label;c2lbl.style.cursor='pointer';c2lbl.title='flash this category in the code';c2lbl.onclick=()=>flashTokens(kind);c2.appendChild(c2lbl);
    tr.appendChild(c2);tr.appendChild(lkTd);tr.appendChild(c0);tr.appendChild(cB);tr.appendChild(stTd);tr.appendChild(crTd);tr.appendChild(exTd);tr.appendChild(cX);
    tb.appendChild(tr);tb.appendChild(exp.detail);}
  updateLockToggle('syntax');
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
function uiCss(o,fgv,bgv,opts={}){const fg=fgv===undefined?effFg(o.fg):fgv,bg=bgv===undefined?o.bg:bgv;return faceCss(o,fg,bg,{noBg:opts.noBg,boxBg:bg||MAP['bg']});}
function syncMockHeight(){const t=document.getElementById('uitable'),m=document.getElementById('mockframe');if(!t||!m)return;const lb=m.previousElementSibling,lbh=lb?lb.getBoundingClientRect().height+10:30;m.style.height=Math.max(t.getBoundingClientRect().height-lbh,220)+'px';}
function buildMockFrame(){
  const fr=document.getElementById('mockframe');if(!fr)return;
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
  html+=`<div class="bar" data-face="mode-line" style="${uiCss(ml,ml.fg||bg,ml.bg||fg)}">  init.el      (Emacs Lisp)      L5      <span data-face="mode-line-highlight" title="mode-line-highlight (mode-line hover)" style="${mlhStyle}">git:main</span>  </div>`;
  html+=`<div class="bar" data-face="mode-line-inactive" style="${uiCss(mli,resolveUiAttr('mode-line-inactive','fg',UIMAP)||fg,resolveUiAttr('mode-line-inactive','bg',UIMAP)||bg)}">  *Messages*      (Fundamental)</div>`;
  html+=`<div class="echo" style="color:${fg}"><span data-face="minibuffer-prompt" style="${uiCss(mb,mb.fg||fg,mb.bg||null)}">I-search:</span> count   <span data-face="isearch-fail" style="${uiCss(isf,isf.fg||fg,isf.bg||'transparent')}">zzz [no match]</span></div>`;
  html+=`<div class="echo"><span data-face="link" style="${uiCss(lnk,lnk.fg||fg,lnk.bg||null)}">https://gnu.org</span>   <span data-face="error" style="${uiCss(err,err.fg||fg,err.bg||null)}">error</span>   <span data-face="warning" style="${uiCss(wrn,wrn.fg||fg,wrn.bg||null)}">warning</span>   <span data-face="success" style="${uiCss(suc,suc.fg||fg,suc.bg||null)}">ok</span></div>`;
  fr.innerHTML=html;fr.style.background=bg;fr.style.color=fg;
  fr.onclick=(e)=>{const u=e.target.closest('[data-face]');if(u){flashUi(u.dataset.face);return;}const k=e.target.closest('[data-k]');if(k)flashAssign(k.dataset.k);};
}
// All three tiers share one dropdown — the swatch div from mkColorDropdown. The
// native <select> rendered swatch colors unreliably on Linux Chrome, so it is
// gone. '' (the default entry) maps back to null in the stored model.
function uiSelect(face,attr){const cur=UIMAP[face][attr]||'';
  return mkColorDropdown(ddList(cur),cur,h=>{UIMAP[face][attr]=h||null;paintUI(face);buildMockFrame();},{compact:true,defaultHex:attr==='fg'?effFg(null):effBg(null)});}
const BASE_INHERITS=['fixed-pitch','variable-pitch','default','link','bold','italic','shadow'];
function uiFaceBlank(){return {fg:null,bg:null,'distant-fg':null,family:null,weight:null,slant:null,underline:null,strike:null,overline:null,box:null,inverse:false,extend:false,inherit:null,height:null};}
function seedFace(d){return normalizePkgFace({fg:pname(d.fg),bg:pname(d.bg),'distant-fg':pname(d['distant-fg']),family:d.family,weight:d.weight,slant:d.slant,bold:d.bold,italic:d.italic,underline:d.underline,strike:d.strike,overline:d.overline,inherit:d.inherit,height:d.height,box:d.box,inverse:d.inverse,extend:d.extend},'default');}
function curApp(){const s=document.getElementById('viewsel');const v=s&&s.value;return (v&&v[0]!=='@')?v:Object.keys(APPS)[0];}
function pkgEffFg(app,face,seen){return effResolve(PKGMAP,app,face,'fg',seen);}
function pkgEffBg(app,face,seen){return effResolve(PKGMAP,app,face,'bg',seen);}
// One dropdown drives the whole assignment panel: two editor entries (@code,
// @ui) then a non-selectable "package faces" optgroup holding every app,
// alphabetically by label. onViewChange shows exactly one of the three view blocks.
function buildViewSel(){const s=document.getElementById('viewsel');if(!s)return;s.innerHTML='';
  const mk=(v,t)=>{const o=document.createElement('option');o.value=v;o.textContent=t;return o;};
  s.appendChild(mk('@code','color/code assignments'));
  s.appendChild(mk('@ui','ui faces'));
  const og=document.createElement('optgroup');og.label='package faces';
  for(const app of appViewKeysSorted(APPS))og.appendChild(mk(app,APPS[app].label));
  s.appendChild(og);}
// The ‹ › buttons flanking the dropdown step the selection by DIR and re-render
// the view (faces table + preview), so you can walk the list without reopening it.
function stepView(dir){
  const s=document.getElementById('viewsel');if(!s)return;
  const i=stepViewIndex(s.selectedIndex,s.options.length,dir);
  if(i!==s.selectedIndex){s.selectedIndex=i;onViewChange();}
}
function onViewChange(){const s=document.getElementById('viewsel');const v=(s&&s.value)||'@code';
  const show=(id,on)=>{const e=document.getElementById(id);if(e)e.style.display=on?'':'none';};
  show('view-code',v==='@code');show('view-ui',v==='@ui');show('view-pkg',v[0]!=='@');
  if(v==='@code')renderCode();
  else if(v==='@ui'){buildUITable();buildMockFrame();syncMockHeight();}
  else pkgChanged();}
function pkgChanged(){buildPkgTable();buildPkgPreview();syncPkgHeight();}
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
    const exp=mkExpander(f,tableColCount('pkgtable'),()=>{f.source='user';pkgChanged();},{showInheritHeight:true,inheritOptions:inh,defaultHex:effFg(pkgEffFg(app,face)),ndCheck:()=>overflowNonDefault(f,def,true)});
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
    const cL=mkLockCell('pkg:'+app+':'+face,[fgd,bgd,...pkCtls,boxCtl,...exp.locks]);
    if(nd.fg)cf.classList.add('nd');if(nd.bg)cb.classList.add('nd');if(nd.style)cw.classList.add('nd');
    if(nd.box)cx.classList.add('nd');
    tr.append(c0,cL,cf,cb,cw,cc,cx);tb.appendChild(tr);tb.appendChild(exp.detail);
  }
  applyTableSort('pkgbody');
  updateLockToggle('pkg');
}
// The per-package preview renderers live in previews.js, spliced here so the
// PACKAGE_PREVIEWS registry below can reference them.
PREVIEWS_J
const PACKAGE_PREVIEWS={
  autodim:renderAutodimPreview,markdown:renderMarkdownPreview,
  org:renderOrgPreview,magit:renderMagitPreview,elfeed:renderElfeedPreview,ghostel:renderGhostelPreview,
  dashboard:renderDashboardPreview,mu4e:renderMu4ePreview,gnus:renderGnusPreview,orgfaces:renderOrgFacesPreview,lsp:renderLspPreview,gitgutter:renderGitGutterPreview,
  flycheck:renderFlycheckPreview,dired:renderDiredPreview,dirvish:renderDirvishPreview,calibredb:renderCalibredbPreview,
  erc:renderErcPreview,orgdrill:renderOrgdrillPreview,orgnoter:renderOrgnoterPreview,signel:renderSignelPreview,
  pearl:renderPearlPreview,slack:renderSlackPreview,telega:renderTelegaPreview,shr:renderShrPreview
};
function buildPkgPreview(){
  const app=curApp(),p=document.getElementById('pkgpreview');if(!p)return;
  const renderer=PACKAGE_PREVIEWS[APPS[app].preview];
  p.innerHTML=renderer?renderer():genericPreview(app);
  p.style.background=MAP['bg'];
  p.onclick=(e)=>{const u=e.target.closest('[data-face]');if(u)flashPkg(u.dataset.face);};
  const lbl=document.getElementById('pkgprevlabel');if(lbl)lbl.textContent=renderer?(APPS[app].label+' preview'):'preview (generic — face names in their own colors)';
}
function resetApp(){const app=curApp();for(const [face,,d] of APPS[app].faces)if(!LOCKED.has('pkg:'+app+':'+face))PKGMAP[app][face]=seedFace(d);pkgChanged();notify('reset editable '+app+' faces to package defaults',false);}
function syncPkgHeight(){const t=document.getElementById('pkgtable'),m=document.getElementById('pkgpreview');if(!t||!m)return;const lb=m.previousElementSibling,lbh=lb?lb.getBoundingClientRect().height+10:30;m.style.height=Math.max(t.getBoundingClientRect().height-lbh,220)+'px';}
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
function paintUI(face){const pv=document.getElementById('uiprev-'+face);if(!pv)return;const o=UIMAP[face];pv.style.color=effFg(o.fg);pv.style.background=effBg(o.bg);pv.style.fontWeight=cssWeight(o.weight);pv.style.fontStyle=o.slant||'normal';pv.style.textDecoration=(o.underline?'underline ':'')+(o.strike?'line-through':'')||'none';pv.style.boxShadow=boxCss(o.box,effBg(o.bg));
  const report=coveredContrastReport(face);
  pv.title='';
  const cr=document.getElementById('uicr-'+face);if(cr){cr.title='';if(report!==null){if(report.empty){cr.title='this overlay has no syntax foreground set yet';cr.innerHTML='<span title="this overlay has no syntax foreground set yet">no fg set</span>';}else{const title=failureTitle(report)||'all covered text clears '+WORST_TARGET.toFixed(1);cr.title=title;cr.innerHTML=`<span style="color:${ratingColor(report.worst.ratio)}" title="${esc(title)}">${report.worst.ratio.toFixed(1)}</span>`;}}else{const efg=effFg(o.fg),ebg=effBg(o.bg),r=contrast(efg,ebg);cr.innerHTML=crHtml(r);}}}
function buildUITable(){
  const tb=document.getElementById('uibody');tb.innerHTML='';
  for(const [face,label,ex] of UI_FACES){
    const tr=document.createElement('tr');tr.dataset.face=face;
    const exp=mkExpander(UIMAP[face],tableColCount('uitable'),()=>{paintUI(face);buildMockFrame();},{showInheritHeight:true,inheritOptions:[''].concat(BASE_INHERITS),defaultHex:effFg(UIMAP[face].fg),ndCheck:()=>overflowNonDefault(UIMAP[face],DEFAULT_UIMAP[face],true)});
    exp.detail.dataset.detailFor=face;
    const c0=document.createElement('td');c0.className='cat';c0.title=composeHoverTitle(FACE_DOCS[face],c0.title);c0.appendChild(exp.btn);
    const c0lbl=document.createElement('span');c0lbl.textContent=' '+label;c0lbl.style.cursor='pointer';c0lbl.title='flash this face in the live preview';c0lbl.onclick=()=>flashUiPreview(face);c0.appendChild(c0lbl);
    const fgSel=uiSelect(face,'fg'),bgSel=uiSelect(face,'bg');
    const cF=document.createElement('td');cF.appendChild(fgSel);
    const cB=document.createElement('td');cB.appendChild(bgSel);
    const cS=document.createElement('td');
    const stCtls=mkStyleControls(UIMAP[face],()=>{paintUI(face);buildMockFrame();},{defaultHex:effFg(UIMAP[face].fg)});
    const uiCluster=document.createElement('div');uiCluster.className='stylecluster';stCtls.forEach(c=>uiCluster.appendChild(c));cS.appendChild(uiCluster);
    const cC=document.createElement('td');cC.id='uicr-'+face;cC.style.whiteSpace='nowrap';cC.style.fontSize='10pt';
    const cP=document.createElement('td');cP.className='ex';cP.id='uiprev-'+face;cP.textContent=ex;cP.style.padding='4px 10px';cP.style.borderRadius='4px';
    const cX=document.createElement('td');const boxCtl=mkBoxControl(()=>UIMAP[face].box,b=>{UIMAP[face].box=b;paintUI(face);buildMockFrame();},{compact:true});cX.appendChild(boxCtl);
    const cL=mkLockCell('ui:'+face,[fgSel,bgSel,...stCtls,boxCtl,...exp.locks]);
    tr.appendChild(c0);tr.appendChild(cL);tr.appendChild(cF);tr.appendChild(cB);tr.appendChild(cS);tr.appendChild(cC);tr.appendChild(cP);tr.appendChild(cX);tb.appendChild(tr);tb.appendChild(exp.detail);paintUI(face);
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
  updateTitle();initPicker();buildPkgPreview();syncMockHeight();syncPkgHeight();
  onViewChange();
}
initApp();
addEventListener('resize',()=>{syncMockHeight();syncPkgHeight();});
BROWSER_GATES_J

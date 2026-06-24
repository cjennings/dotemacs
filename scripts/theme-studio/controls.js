// controls.js -- the custom dropdown / detail-editor / expander control
// factories, extracted from app.js for navigability. Inlined raw at the
// CONTROLS_J token: these are hoisting function declarations plus the
// dropdown popup state, so the token's position preserves execution order.
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
  function step(dir){if(wrap.dataset.locked==='1')return;const next=spanNeighborHex(cur,PALETTE,groundPair(),dir);if(!next)return;cur=next;paint();onPick(next);}
  function paintStepButtons(){
    const locked=wrap.dataset.locked==='1';
    left.disabled=locked||!spanNeighborHex(cur,PALETTE,groundPair(),-1);
    right.disabled=locked||!spanNeighborHex(cur,PALETTE,groundPair(),1);
  }
  function paint(){const shown=cur||(opts.defaultHex||''),nm=cur?nameOf(cur):(opts.defaultName||nameOf(cur)),ttl=cur?(nm+' '+cur):(nm+(shown?' -> '+shown:''));t.style.background=shown||'#161412';t.style.color=shown?textOn(shown):'#b4b1a2';t.dataset.val=cur||'';t.title=ttl;t.classList.toggle('is-default',!cur);t.classList.toggle('gone',!!cur&&nameOf(cur)==='(gone)');
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
const WEIGHT_OPTS=[['light','light'],['normal','normal'],['medium','medium'],['semibold','semibold'],['bold','bold'],['heavy','heavy']];
const SLANT_OPTS=[['normal','normal'],['italic','italic'],['oblique','oblique']];
// A compact custom dropdown for an enum attribute (weight / slant), themed like
// the color dropdown. The trigger shows the current value drawn in its own weight
// or slant; the popup lists each option drawn with the attribute applied, so the
// choice previews itself. opts.styleFor(value) returns the preview style props
// ({fontWeight} / {fontStyle}); opts.placeholder is the unset-state label.
function mkEnumDropdown(options,get,set,opts={}){
  const t=document.createElement('div');t.className='cdd enumdd';t.tabIndex=0;
  const styleFor=opts.styleFor||(()=>({}));
  const labelOf=v=>{const o=options.find(p=>p[0]===v);return o?o[1]:'';};
  function applyPreview(el,v){el.style.fontWeight='';el.style.fontStyle='';const s=styleFor(v);if(s.fontWeight)el.style.fontWeight=s.fontWeight;if(s.fontStyle)el.style.fontStyle=s.fontStyle;}
  function paint(){const v=get()||'';t.dataset.val=v;t.classList.toggle('is-default',!v);
    t.textContent=v?labelOf(v):(opts.placeholder||'set');applyPreview(t,v);t.title=opts.title||'';}
  paint();
  t.onclick=(e)=>{e.stopPropagation();if(t.dataset.locked==='1')return;if(_ddPop){closeColorDropdown();return;}
    const pop=document.createElement('div');pop.className='cddpop enumpop';const cur=get()||'';
    const pick=v=>{set(v||null);paint();closeColorDropdown();};
    const def=document.createElement('button');def.type='button';
    def.className='enumopt enumdef'+(cur===''?' sel':'');def.textContent='default';
    def.title='clear — use the default';def.onclick=ev=>{ev.stopPropagation();pick('');};pop.appendChild(def);
    for(const [v,label] of options){const b=document.createElement('button');b.type='button';
      b.className='enumopt'+(v===cur?' sel':'');b.textContent=label;applyPreview(b,v);
      b.onclick=ev=>{ev.stopPropagation();pick(v);};pop.appendChild(b);}
    document.body.appendChild(pop);const r=t.getBoundingClientRect();
    pop.style.left=r.left+'px';pop.style.minWidth=r.width+'px';pop.style.top=(r.bottom+2)+'px';
    const ph=pop.getBoundingClientRect().height;
    if(r.bottom+ph>window.innerHeight-6)pop.style.top=Math.max(6,r.top-ph-2)+'px';
    _ddPop=pop;};
  t.setValue=()=>paint();t.syncLocked=()=>paint();
  return t;}
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
  const w=mkEnumDropdown(WEIGHT_OPTS,()=>face.weight,v=>{face.weight=v;onChange();},{placeholder:'weight',title:'font weight',styleFor:v=>({fontWeight:cssWeight(v)})});
  const s=mkEnumDropdown(SLANT_OPTS,()=>face.slant,v=>{face.slant=v;onChange();},{placeholder:'slant',title:'font slant',styleFor:v=>({fontStyle:v||'normal'})});
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
// Hover help for each expander field, so the detail labels explain themselves the
// way the table-header labels do. Keyed by the label text passed to add().
const DETAIL_HOVERS={
  'distant fg':'foreground swapped in when the text sits on a background too close to its own color to read (Emacs :distant-foreground)',
  'family':'font family for this face; blank inherits the default (Emacs :family)',
  'underline':'underline style and color (Emacs :underline)',
  'overline':'a line drawn above the text (Emacs :overline)',
  'inverse':'swap the foreground and background (Emacs :inverse-video)',
  'extend':'extend the background past the end of the line to the window edge (Emacs :extend)',
  'inherit':'base face this one inherits unset attributes from (Emacs :inherit)',
  'height':'text size as a scaling factor of the inherited height, 0.1 to 2.0 (Emacs :height)'
};
function mkDetailEditor(face,onChange,opts={}){
  const wrap=document.createElement('div');wrap.className='detailedit';const locks=[];
  const add=(label,el)=>{const g=document.createElement('label');g.className='detailfield';g.title=DETAIL_HOVERS[label]||'';const s=document.createElement('span');s.textContent=label;g.append(s,el);wrap.appendChild(g);locks.push(el);};
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
// Which rows have their detail expanded, keyed by the row's element/face key.
// Held outside the DOM so a table rebuild (a package edit rebuilds the whole
// table) re-opens the rows that were open, instead of collapsing them under the
// user — editing a value in an open expander must not close it.
let EXPANDED=new Set();
function mkExpander(face,colspan,onChange,opts={}){
  const detail=document.createElement('tr');detail.className='detailrow';detail.style.display='none';
  if(opts.expandKey&&EXPANDED.has(opts.expandKey))detail.style.display='';
  const btn=document.createElement('button');btn.className='exptoggle';
  // The disclosure triangle shows the row's state: ▶ collapsed, ▼ expanded.
  const setGlyph=()=>{const open=detail.style.display!=='none';btn.textContent=open?'▼':'▶';btn.classList.toggle('on',open);};
  // Flag the toggle when collapsed and at least one hidden attribute differs from
  // the default, so a non-default attribute is never invisible. ndCheck re-runs
  // after every edit (for tiers whose onChange does not rebuild the row).
  const ndCheck=opts.ndCheck||(()=>false);
  const refreshNd=()=>{const nd=ndCheck();btn.classList.toggle('exp-nd',nd);btn.title=nd?'more attributes (some differ from default)':'more attributes';};
  const wrapped=()=>{onChange();refreshNd();};
  const td=document.createElement('td');td.colSpan=colspan;const {el,locks}=mkDetailEditor(face,wrapped,opts);td.appendChild(el);detail.appendChild(td);
  btn.onclick=()=>{const willOpen=detail.style.display==='none';detail.style.display=willOpen?'':'none';
    if(opts.expandKey){willOpen?EXPANDED.add(opts.expandKey):EXPANDED.delete(opts.expandKey);}
    setGlyph();syncExpandAllBtns();};
  refreshNd();setGlyph();
  return {btn,detail,locks};}

function clearPalette(){
  normalizePalette();
  const plan=clearPalettePlan(PALETTE,{bg:MAP['bg'],fg:MAP['p']});
  plan.removed.forEach(({hex,name})=>rememberGone(hex,name));
  PALETTE=plan.palette;selectedIdx=null;
  renderPalette();buildTable();buildUITable();if(document.getElementById('pkgbody'))buildPkgTable();renderCode();applyGround();
  notify('cleared palette to bg and fg',false);
}
let selectedIdx=null;
// When a named palette color is deleted, remember its hex keyed by name so that
// recreating a color with the same name can re-bind the assignments still pointing
// at the old (now "(gone)") hex. Consumed once per name; cleared on import.
let lastGone={};
// Re-point every assignment — syntax map, UI faces, package faces — from one hex
// to another. Used when a palette color's value is edited and when a deleted name
// is recreated.
function repointHex(oldHex,newHex){
  if(oldHex===newHex)return;
  for(const k in MAP){if(MAP[k]===oldHex)MAP[k]=newHex;}
  for(const f in UIMAP){if(UIMAP[f].fg===oldHex)UIMAP[f].fg=newHex;if(UIMAP[f].bg===oldHex)UIMAP[f].bg=newHex;}
  for(const ap in PKGMAP)for(const fc in PKGMAP[ap]){const o=PKGMAP[ap][fc];if(o.fg===oldHex)o.fg=newHex;if(o.bg===oldHex)o.bg=newHex;}
}
// On adding a color, if its name matches a recently-deleted one, re-bind the
// stranded assignments to the new hex. Returns true when a heal context existed.
function healGone(name,newHex){const k=name.toLowerCase();if(!(k in lastGone))return false;const g=lastGone[k];delete lastGone[k];repointHex(g,newHex);return true;}
function rememberGone(hex,name){if(name)lastGone[name.toLowerCase()]=hex;}
function normalizePaletteEntry(entry){
  const hex=entry&&entry[0],name=(entry&&entry[1])||'color';
  return [hex,name,(entry&&entry[2])||columnIdOf(entry)];
}
function normalizePalette(){PALETTE=PALETTE.map(normalizePaletteEntry);}
// The ground column is explicit: bg pins the top endpoint, fg pins the bottom
// endpoint, and generated ground-N steps live between them.
function groundColumnMembers(){
  return groundColumnMembersFromPalette(PALETTE,{bg:MAP['bg'],fg:MAP['p']});
}
function groundSpanCount(){return PALETTE.filter(entry=>groundRoleOfEntry(entry,{bg:MAP['bg'],fg:MAP['p']})==='step').length;}
function groundSpanControl(){
  const d=document.createElement('div');d.className='fcount';
  d.innerHTML=`<span title="number of ground colors between bg and fg">span <input type="number" min="0" max="8" value="${groundSpanCount()}"></span>`;
  d.querySelector('input').onchange=(e)=>setGroundSpan(Math.max(0,Math.min(8,parseInt(e.target.value,10)||0)));
  return d;
}
function setGroundSpan(n){
  const old=PALETTE.filter(entry=>groundRoleOfEntry(entry,{bg:MAP['bg'],fg:MAP['p']})==='step');
  const bg=srgb2oklab(MAP['bg']),fg=srgb2oklab(MAP['p']);
  const entries=[];
  for(let i=1;i<=n;i++){
    const t=i/(n+1);
    const lab={L:bg.L+(fg.L-bg.L)*t,a:bg.a+(fg.a-bg.a)*t,b:bg.b+(fg.b-bg.b)*t};
    entries.push([lrgb2hex(oklab2lrgb(lab.L,lab.a,lab.b)),'ground-'+i,'ground']);
  }
  for(const [oldHex,oldName] of old){
    const next=entries.find(([,name])=>name===oldName);
    if(next&&next[0].toLowerCase()!==oldHex.toLowerCase())repointHex(oldHex,next[0]);
  }
  for(let i=PALETTE.length-1;i>=0;i--)if(groundRoleOfEntry(PALETTE[i],{bg:MAP['bg'],fg:MAP['p']})==='step')PALETTE.splice(i,1);
  let at=PALETTE.findIndex(entry=>groundRoleOfEntry(entry,{bg:MAP['bg'],fg:MAP['p']})==='bg');
  if(at<0)at=0; else at+=1;
  PALETTE.splice(Math.min(at,PALETTE.length),0,...entries);
  selectedIdx=null;renderPalette();buildTable();buildUITable();renderCode();applyGround();
  notify('set ground span to '+n,false);
}
// Pairwise OKLab ΔE over the palette. Returns the sub-threshold pairs (sorted
// closest-first) and each color's nearest-neighbor distance for its chip title.
// Pure pairwise ΔE analysis lives in colormath.js (paletteWarnings); this renders it.
function renderPaletteWarnings(warnings,overflow){
  const w=document.getElementById('palwarn');if(!w)return;
  if(!warnings.length){w.style.display='none';w.innerHTML='';return;}
  let html='<div class="pwh">too-similar colors</div>';
  html+=warnings.map(p=>`<div class="pwl">${esc(p.aName+' / '+p.bName)} — \u0394E ${p.dE.toFixed(3)}, hard to distinguish</div>`).join('');
  if(overflow>0)html+=`<div class="pwl">and ${overflow} more</div>`;
  w.innerHTML=html;w.style.display='block';
}
// One palette chip for PALETTE[i], with its remove / rename / select handlers.
// Families sort deterministically, so the old move-arrow / drag reordering is gone.
function paletteChip(i,nearest){
  const [hex,name]=PALETTE[i],tc=textOn(hex),nde=nearest[i];
  const role=groundRoleOfEntry(PALETTE[i],{bg:MAP['bg'],fg:MAP['p']});
  const locked=(role==='bg'||role==='fg');
  const d=document.createElement('div');d.className='pchip'+(i===selectedIdx?' sel':'');d.style.background=hex;
  d.title=name+' '+hex+(nde===Infinity||nde===undefined?'':' — nearest ΔE '+nde.toFixed(3));
  const rm=locked?`<span class="lock" title="${role==='bg'?'background':'foreground'} — can't remove" style="color:${tc}">&#128274;</span>`:`<button class="rm" title="remove" style="color:${tc}">×</button>`;
  d.innerHTML=`${rm}<input class="nm" value="${name}" readonly style="color:${tc}"><div class="hx" style="color:${tc}">${hex}</div>`;
  if(!locked)d.querySelector('.rm').onclick=(e)=>{e.stopPropagation();rememberGone(hex,name);PALETTE.splice(i,1);if(selectedIdx===i)selectedIdx=null;renderPalette();buildTable();buildUITable();};
  const nm=d.querySelector('.nm');
  const finishNameEdit=()=>{nm.readOnly=true;nm.classList.remove('editing');};
  nm.onclick=(e)=>{e.preventDefault();e.stopPropagation();selectColor(i);};
  nm.ondblclick=(e)=>{e.preventDefault();e.stopPropagation();nm.readOnly=false;nm.classList.add('editing');nm.focus();nm.setSelectionRange(0,0);};
  nm.onblur=finishNameEdit;
  nm.onkeydown=(e)=>{if(e.key==='Enter'){e.preventDefault();nm.blur();}else if(e.key==='Escape'){e.preventDefault();nm.value=PALETTE[i][1];nm.blur();}};
  nm.onchange=(e)=>{PALETTE[i][1]=e.target.value;buildTable();buildUITable();};
  d.onclick=(e)=>{if(e.target.closest('.rm'))return;selectColor(i);};
  return d;
}
function paletteIndexByHexName(hex,name){
  for(let i=0;i<PALETTE.length;i++)if(PALETTE[i][0]===hex&&PALETTE[i][1]===name)return i;
  return -1;
}
function selectColumnBase(f){
  const baseMember=f.members.find(m=>m.hex.toLowerCase()===f.base.toLowerCase())||f.members[0];
  const i=paletteIndexByHexName(baseMember.hex,baseMember.name);
  if(i>=0)selectColor(i);
}
function isGroundEntry(entry){
  return !!groundRoleOfEntry(entry,{bg:MAP['bg'],fg:MAP['p']});
}
function moveColumn(columnId,dir){
  normalizePalette();
  const columns=sortColumns(columnsFromPalette(PALETTE,{bg:MAP['bg'],fg:MAP['p']}).columns);
  const pos=columns.findIndex(f=>f.column===columnId);
  const next=columns[pos+dir];
  if(pos<0||!next)return;
  const moving=[],rest=[];
  PALETTE.forEach(entry=>{
    if(!isGroundEntry(entry)&&columnIdOf(entry)===columnId)moving.push(entry);
    else rest.push(entry);
  });
  const nextPositions=[];
  rest.forEach((entry,i)=>{if(!isGroundEntry(entry)&&columnIdOf(entry)===next.column)nextPositions.push(i);});
  if(!nextPositions.length)return;
  const at=dir<0?nextPositions[0]:nextPositions[nextPositions.length-1]+1;
  PALETTE=rest.slice(0,at).concat(moving,rest.slice(at));
  selectedIdx=null;renderPalette();buildTable();buildUITable();renderCode();applyGround();
  notify('moved "'+columnId+'" '+(dir<0?'left':'right'),false);
}
function deleteColumn(columnId,label){
  normalizePalette();
  const plan=deletePaletteColumnPlan(PALETTE,{bg:MAP['bg'],fg:MAP['p']},columnId);
  if(!plan.removed.length){notify('nothing to delete in "'+(label||columnId)+'"',true);return;}
  const title=label||columnId;
  if(!confirm('Delete color column "'+title+'"?\n\nThis removes '+plan.removed.length+' palette color(s). Existing face assignments will stay on their old hex values and show as "(gone)".'))return;
  plan.removed.forEach(({hex,name})=>rememberGone(hex,name));
  PALETTE=plan.palette;selectedIdx=null;
  renderPalette();buildTable();buildUITable();renderCode();applyGround();
  notify('deleted column "'+title+'" — '+plan.removed.length+' color(s) now show "(gone)" where used',false);
}
function columnHeader(f,position,count){
  const h=document.createElement('div');h.className='fhead';
  const label=(f.members.find(m=>m.hex.toLowerCase()===f.base.toLowerCase())||{}).name||f.column||f.base;
  h.innerHTML=`<button class="cmove left" title="move column left" ${position===0?'disabled':''}>&#8249;</button><button class="ctitle" title="select base color"></button><button class="cmove right" title="move column right" ${position===count-1?'disabled':''}>&#8250;</button><button class="cdel" title="delete column with confirmation">×</button>`;
  h.querySelector('.ctitle').textContent=label;
  h.querySelector('.ctitle').onclick=()=>selectColumnBase(f);
  h.querySelector('.left').onclick=(e)=>{e.stopPropagation();moveColumn(f.column,-1);};
  h.querySelector('.right').onclick=(e)=>{e.stopPropagation();moveColumn(f.column,1);};
  h.querySelector('.cdel').onclick=(e)=>{e.stopPropagation();deleteColumn(f.column,label);};
  return h;
}
// Render the palette as structural color columns: pinned ground column, then
// first-seen palette columns. Grouping uses the stable column id stored on each
// palette entry, so renaming a color never moves it.
function renderPalette(){
  normalizePalette();
  const p=document.getElementById('pals');p.innerHTML='';
  const {warnings,overflow,nearest}=paletteWarnings(PALETTE,DELTAE_MIN,5);
  const {ground,columns}=columnsFromPalette(PALETTE,{bg:MAP['bg'],fg:MAP['p']});
  const used=new Set();
  const idxOf=(hex,name)=>{for(let i=0;i<PALETTE.length;i++)if(!used.has(i)&&PALETTE[i][0]===hex&&PALETTE[i][1]===name){used.add(i);return i;}return -1;};
  const strip=(cls)=>{const s=document.createElement('div');s.className='fstrip'+(cls||'');p.appendChild(s);return s;};
  if(ground.length){
    const gs=strip(' ground');gs.dataset.column='ground';
    const gh=document.createElement('div');gh.className='fhead';gh.textContent='ground';gs.appendChild(gh);
    gs.appendChild(groundSpanControl());
    groundColumnMembers().forEach(m=>{
      const i=idxOf(m.hex,m.name);
      if(i>=0)gs.appendChild(paletteChip(i,nearest));
      else{const tc=textOn(m.hex),sw=document.createElement('div');sw.className='pchip';sw.style.background=m.hex;sw.title=(m.name||'ground')+' '+m.hex;
        sw.innerHTML=`<input class="nm" value="${m.name||'ground'}" disabled style="color:${tc}"><div class="hx" style="color:${tc}">${m.hex}</div>`;gs.appendChild(sw);}
    });
  }
  // The too-similar warning stays on the full flat palette: a generated ramp's
  // steps are a stepL apart (well above the warning's ΔE threshold), so they never
  // trigger it, and any pair that does is a genuine near-duplicate worth flagging.
  const ordered=sortColumns(columns);
  ordered.forEach((f,pos)=>{
    const s=strip('');s.dataset.column=f.column||f.base;
    s.appendChild(columnHeader(f,pos,ordered.length));
    s.appendChild(columnCountControl(f));
    f.members.forEach(m=>{const i=idxOf(m.hex,m.name);if(i>=0)s.appendChild(paletteChip(i,nearest));});
  });
  renderPaletteWarnings(warnings,overflow);
  buildUITable();if(document.getElementById('pkgbody'))buildPkgTable();
}
// The per-column count control under a chromatic strip. Its value is the column's
// current per-side reach; setting N regenerates the column as base ±N.
function columnCountControl(f){
  const per=Math.max(0,...rankByLightness(f.members.map(m=>m.hex),f.base).map(m=>Math.abs(m.offset)));
  const d=document.createElement('div');d.className='fcount';
  d.innerHTML=`<span title="set the column span: N generated steps on each side of the base — this replaces the column">span &#177; <input type="number" min="0" max="4" value="${per}"></span>`;
  d.querySelector('input').onchange=(e)=>setColumnCount(f.base,Math.max(0,Math.min(4,parseInt(e.target.value,10)||0)));
  return d;
}
// Regenerate a column as a symmetric base ±N ramp, replacing its current members.
// References to a surviving position (matched by signed lightness rank) follow the
// new hex; references to a position removed by lowering N leave their old hex,
// which is no longer in the palette and so renders as "(gone)".
// Replace oldHexes in the palette with a fresh base ±n ramp, repointing surviving
// references and leaving removed ones on their now-gone hex. Returns the removed
// count, or null on a bad base. Shared by the count control and the base edit.
function regenColumnInPlace(oldHexes,baseHex,baseName,n,columnId){
  const r=regenColumn(baseHex,n,{});
  if(r.error){notify('cannot regenerate from '+baseHex,true);return null;}
  const plan=stepRepointPlan(rankByLightness(oldHexes,baseHex),r.members);
  const oldSet=new Set(oldHexes.map(h=>h.toLowerCase()));
  let at=PALETTE.length;
  for(let i=0;i<PALETTE.length;i++)if(oldSet.has(PALETTE[i][0].toLowerCase())){at=i;break;}
  for(let i=PALETTE.length-1;i>=0;i--)if(oldSet.has(PALETTE[i][0].toLowerCase()))PALETTE.splice(i,1);
  const col=columnId||columnStem(baseName);
  const entries=r.members.map(m=>[m.hex,m.offset===0?baseName:baseName+(m.offset>0?'+'+m.offset:String(m.offset)),col]);
  PALETTE.splice(Math.min(at,PALETTE.length),0,...entries);
  for(const [o,nw] of plan.map)repointHex(o,nw);
  return plan.removed.length;
}
function setColumnCount(baseHex,n){
  const {columns}=columnsFromPalette(PALETTE,{bg:MAP['bg'],fg:MAP['p']});
  const column=columns.find(f=>f.base.toLowerCase()===baseHex.toLowerCase());
  if(!column)return;
  const baseName=(column.members.find(m=>m.hex.toLowerCase()===baseHex.toLowerCase())||{}).name||'color';
  const removed=regenColumnInPlace(column.members.map(m=>m.hex),baseHex,baseName,n,column.column);
  if(removed===null)return;
  selectedIdx=null;renderPalette();buildTable();buildUITable();renderCode();applyGround();
  notify('regenerated "'+baseName+'" to ±'+n+(removed?(' — '+removed+' removed step(s) show "(gone)" where used'):''),false);
}

// Shared gate harness. Each call site keeps its literal location.hash==='#NAMEtest'
// check (run-tests.sh greps it); gate() owns the ok/notes/A setup and the verdict
// postamble. Note format standardized to ' fails=note1,note2'.
function gate(id, body){
  const name=id.toUpperCase();
  let ok=true;const notes=[];
  const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
  body(A);
  const verdict=name+' '+(ok?'PASS':'FAIL');
  document.title=verdict;
  const d=document.createElement('div');d.id=id;
  d.textContent=verdict+(notes.length?' fails='+notes.join(','):'');
  document.body.appendChild(d);
}
function withSavedState(keys, body){
  // Snapshot the named studio globals, run BODY, then restore them in a finally
  // so opening the studio at a #gate hash doesn't leave its state mutated for
  // interactive use. Each key maps to a [get, set, clone] triple over the live
  // let-binding. Scope the keys to what the gate actually touches.
  // JSON clone (not structuredClone): the studio data objects carry values
  // structuredClone throws on, and a JSON round-trip of the data is exactly what
  // the gates' own local saves already use.
  const jc=x=>JSON.parse(JSON.stringify(x));
  const reg={
    PALETTE:[()=>PALETTE, v=>{PALETTE=v;}, jc],
    MAP:[()=>MAP, v=>{MAP=v;}, jc],
    SYNTAX:[()=>SYNTAX, v=>{SYNTAX=v;}, jc],
    UIMAP:[()=>UIMAP, v=>{UIMAP=v;}, jc],
    PKGMAP:[()=>PKGMAP, v=>{PKGMAP=v;}, jc],
    LOCKED:[()=>LOCKED, v=>{LOCKED.clear();for(const k of v)LOCKED.add(k);}, s=>new Set(s)],
  };
  const snap=keys.map(k=>[k, reg[k][2](reg[k][0]())]);
  try{ body(); }
  finally{ for(const [k,v] of snap) reg[k][1](v); }
}
// Shared preview-face validator for the #mdtest / #mupreviewtest / #gnustest
// gates: render HTML into a detached div, then assert it exercises at least
// MINCOUNT data-faces, that every data-face is a real face of the package
// (drawn from FACES, the app's face rows), and that each face in REQUIRED is
// present. A is the gate's assertion collector; NAME labels the failure note.
function assertPreviewFaces(A, html, faces, minCount, name, required){
  const box=document.createElement('div');box.innerHTML=html;
  const els=[...box.querySelectorAll('[data-face]')];
  const used=els.map(e=>e.dataset.face);
  A(used.length>=minCount,'preview exercises many faces ('+used.length+')');
  // Owner-aware validity: an element's owner is its data-owner-app, defaulting to
  // this preview's app (the one whose face rows are in FACES) when the attribute is
  // absent. A package owner's valid faces come from APPS[owner].faces; the @ui
  // owner's from UIMAP keys. An unknown owner has no face set, so its elements are
  // flagged -- an intentional off-pane span (real face of a real owner) passes,
  // while a bad owner fails.
  const defaultValid=new Set((faces||[]).map(r=>r[0]));
  const facesOf=owner=>owner==='@ui'?new Set(Object.keys(UIMAP)):(APPS[owner]?new Set(APPS[owner].faces.map(r=>r[0])):null);
  const bad=els.filter(e=>{const o=e.dataset.ownerApp,valid=o?facesOf(o):defaultValid;return !valid||!valid.has(e.dataset.face);}).map(e=>e.dataset.face);
  A(bad.length===0,'every data-face is a real '+name+' face; bad='+bad.join(','));
  for(const f of required) A(used.includes(f),'preview includes '+f);
}
// Phase-1 self-test (open with #selftest): seed -> export -> import -> compare.
function pkgSelftest(){
  const seeded=seedPkgmap();
  seeded['org-mode']['org-level-2']={fg:'#e8bd30',bg:null,weight:null,slant:null,inherit:'org-level-1',height:1.2,heightMode:'rel',source:'user'};
  const exp=packagesForExport(seeded);
  const round=seedPkgmap();mergePackagesInto(round,exp);
  const roundtrip=JSON.stringify(exp)===JSON.stringify(packagesForExport(round));
  let oldjson=true;try{const m=seedPkgmap();mergePackagesInto(m,undefined);oldjson=!!(m['org-mode']&&m['org-mode']['org-todo'].source==='default');}catch(e){oldjson=false;}
  const l2=exp['org-mode']['org-level-2'];
  const inherited=l2.inherit==='org-level-1'&&l2.source==='user';
  const height=l2.height===1.2 && l2.heightMode==='rel' && !('height' in (exp['org-mode']['org-todo'])) && !('heightMode' in (exp['org-mode']['org-todo']));
  const sc=seedPkgmap();sc['org-mode']['org-todo']={fg:null,bg:null,weight:null,slant:null,inherit:null,height:1,source:'cleared'};
  const cleared='org-todo' in packagesForExport(sc)['org-mode'];
  const su=seedPkgmap();mergePackagesInto(su,{'zzz-pkg':{'zzz-face':{fg:'#112233',source:'user'}}});
  const unknown=!!(su['zzz-pkg']&&su['zzz-pkg']['zzz-face'].fg==='#112233');
  PKGMAP['__cyc']={a:{fg:null,bg:null,weight:null,slant:null,inherit:'b',height:1,source:'user'},b:{fg:null,bg:null,weight:null,slant:null,inherit:'a',height:1,source:'user'}};
  let cyc=true;try{pkgEffFg('__cyc','a');}catch(e){cyc=false;}delete PKGMAP['__cyc'];
  const verdict=(roundtrip&&oldjson&&inherited&&height&&cleared&&unknown&&cyc)?'PASS':'FAIL';
  document.title='SELFTEST '+verdict;
  const d=document.createElement('div');d.id='selftest';d.textContent='SELFTEST '+verdict+' roundtrip='+roundtrip+' oldjson='+oldjson+' inherit='+inherited+' height='+height+' cleared='+cleared+' unknown='+unknown+' cycle='+cyc;document.body.appendChild(d);
}
if(location.hash==='#selftest')pkgSelftest();
// Lock-mechanism gate (open with #locktest): two behaviors the refactor must
// preserve, across all three tiers. (1) Locking a row disables its controls via
// the shared mkLockCell. (2) reset/erase batch actions update editable rows but
// leave locked rows (syntax bare-kind, ui:, pkg: keys) untouched.
if(location.hash==='#locktest')gate('locktest',A=>withSavedState(['PALETTE','MAP','SYNTAX','UIMAP','PKGMAP','LOCKED'],()=>{
 const cssRgb=h=>{const [r,g,b]=hex2rgb(h);return 'rgb('+r+', '+g+', '+b+')';};
 LOCKED.clear();buildTable();
 {const k=CATS.map(c=>c[0]).filter(k=>k!=='bg'&&k!=='p')[0];
  const tr=document.querySelector('#legbody tr[data-kind="'+k+'"]'),step=tr.querySelector('.cstep'),lb=tr.querySelector('.lockbtn');
  A(step.dataset.locked!=='1','syntax-dd-starts-unlocked');lb.click();
  A(step.dataset.locked==='1'&&step.classList.contains('locked')&&step.querySelector('.cstepbtn').disabled,'syntax-lock-disables-dd');lb.click();
  A(step.dataset.locked!=='1'&&!step.classList.contains('locked'),'syntax-unlock-reenables-dd');}
 LOCKED.clear();buildUITable();
 {const f=UI_FACES[0][0];
  const tr=document.querySelector('#uibody tr[data-face="'+f+'"]'),step=tr.querySelector('.cstep'),lb=tr.querySelector('.lockbtn');
  A(step.dataset.locked!=='1','ui-dd-starts-unlocked');lb.click();
  A(step.dataset.locked==='1'&&step.classList.contains('locked')&&step.querySelector('.cstepbtn').disabled,'ui-lock-disables-dd');lb.click();
  A(step.dataset.locked!=='1'&&!step.classList.contains('locked'),'ui-unlock-reenables-dd');}
 {UIMAP['region'].fg=null;UIMAP['region'].bg='#888888';buildUITable();
  const tr=document.querySelector('#uibody tr[data-face="region"]'),fg=tr.cells[2].querySelector('.cdd'),bg=tr.cells[3].querySelector('.cdd');
  A(fg.classList.contains('is-default'),'compact default color button has default outline class');
  A(!bg.classList.contains('is-default'),'compact assigned color button does not have default outline class');}
 {setSyntaxFg('kw','');buildTable();
  const dd=document.querySelector('#legbody tr[data-kind="kw"] .cdd');
  A(dd&&dd.style.backgroundColor===cssRgb(MAP['p']),'syntax default fg swatch shows inherited fg color');}
 {UIMAP['fringe'].bg=null;buildUITable();
  const dd=document.querySelector('#uibody tr[data-face="fringe"]').cells[3].querySelector('.cdd');
  A(dd&&dd.style.backgroundColor===cssRgb(MAP['bg']),'ui default bg swatch shows inherited ground color');}
 {const app=curApp(),face=APPS[app].faces[0][0];PKGMAP[app][face].fg=null;PKGMAP[app][face].inherit=null;buildPkgTable();
  const dd=document.querySelector('#pkgbody tr[data-face="'+face+'"]').cells[2].querySelector('.cdd');
  A(dd&&dd.style.backgroundColor===cssRgb(MAP['p']),'package default fg swatch shows inherited/default fg color');}
 {PALETTE=[['#000000','bg','ground'],['#ffffff','fg','ground'],['#222222','gray-dark','gray'],['#888888','gray-mid','gray'],['#dddddd','gray-light','gray']];setSyntaxFg('bg','#000000');setSyntaxFg('p','#ffffff');setSyntaxFg('kw','#888888');LOCKED.clear();buildTable();
  const tr=document.querySelector('#legbody tr[data-kind="kw"]'),btns=tr.querySelectorAll('.cstepbtn');btns[1].click();
  A(MAP['kw']==='#dddddd'&&tr.querySelector('.cdd').dataset.val==='#dddddd','syntax right arrow steps to lighter color');btns[0].click();
  A(MAP['kw']==='#888888','syntax left arrow steps to darker color');}
 {UIMAP['region'].bg='#888888';LOCKED.clear();buildUITable();const tr=document.querySelector('#uibody tr[data-face="region"]'),btns=tr.cells[3].querySelectorAll('.cstepbtn');btns[1].click();
  A(UIMAP['region'].bg==='#dddddd','ui right arrow steps to lighter color');btns[0].click();
  A(UIMAP['region'].bg==='#888888','ui left arrow steps to darker color');}
 {const app=curApp(),face=APPS[app].faces[0][0];PKGMAP[app][face].fg='#888888';LOCKED.clear();buildPkgTable();const tr=document.querySelector('#pkgbody tr[data-face="'+face+'"]'),btns=tr.cells[2].querySelectorAll('.cstepbtn');btns[1].click();
  A(PKGMAP[app][face].fg==='#dddddd','pkg right arrow steps to lighter color');btns[0].click();
  A(PKGMAP[app][face].fg==='#888888','pkg left arrow steps to darker color');}
 {const ks=CATS.map(c=>c[0]).filter(k=>k!=='bg'&&k!=='p'),k1=ks[0],k2=ks[1];
  setSyntaxFg(k1,'#111111');setSyntaxFg(k2,'#222222');LOCKED.clear();LOCKED.add(k1);clearUnlocked();
  A(MAP[k1]==='#111111','syntax-erase-keeps-locked');A(MAP[k2]==='','syntax-erase-wipes-unlocked');}
 {const ks=CATS.map(c=>c[0]).filter(k=>k!=='bg'&&k!=='p'),k1=ks[0],k2=ks[1];
  setSyntaxFg(k1,'#111111');setSyntaxFg(k2,'#222222');LOCKED.clear();LOCKED.add(k1);resetUnlocked();
  A(MAP[k1]==='#111111','syntax-reset-keeps-locked');A(MAP[k2]===DEFAULT_SYNTAX[k2].fg,'syntax-reset-restores-unlocked-default');}
 {const f1=UI_FACES[0][0],f2=UI_FACES[1][0];
  UIMAP[f1].fg='#111111';UIMAP[f2].fg='#222222';LOCKED.clear();LOCKED.add('ui:'+f1);clearUnlockedUI();
  A(UIMAP[f1].fg==='#111111','ui-erase-keeps-locked');A(UIMAP[f2].fg===null,'ui-erase-wipes-unlocked');}
 {const f1=UI_FACES[0][0],f2=UI_FACES[1][0];
  UIMAP[f1].fg='#111111';UIMAP[f2].fg='#222222';LOCKED.clear();LOCKED.add('ui:'+f1);resetUnlockedUI();
  A(UIMAP[f1].fg==='#111111','ui-reset-keeps-locked');A(JSON.stringify(UIMAP[f2])===JSON.stringify(DEFAULT_UIMAP[f2]),'ui-reset-restores-unlocked-default');}
 {const app=curApp(),pf=APPS[app].faces.map(r=>r[0]),p1=pf[0],p2=pf[1];
  PKGMAP[app][p1].fg='#111111';PKGMAP[app][p2].fg='#222222';LOCKED.clear();LOCKED.add('pkg:'+app+':'+p1);clearUnlockedPkg();
  A(PKGMAP[app][p1].fg==='#111111','pkg-erase-keeps-locked');A(PKGMAP[app][p2].fg===null,'pkg-erase-wipes-unlocked');}
 {const app=curApp(),rows=APPS[app].faces,p1=rows[0][0],p2=rows[1][0],d2=rows[1][2];
  PKGMAP[app][p1].fg='#111111';PKGMAP[app][p2]=normalizePkgFace({fg:'#222222',bg:'#333333',bold:true,source:'user'},'user');
  LOCKED.clear();LOCKED.add('pkg:'+app+':'+p1);resetApp();
  A(PKGMAP[app][p1].fg==='#111111','pkg-reset-keeps-locked');
  A(JSON.stringify(PKGMAP[app][p2])===JSON.stringify(seedFace(d2)),'pkg-reset-restores-unlocked-default');}
 {LOCKED.clear();buildTable();const b=document.getElementById('syntaxlocktoggle');A(b&&b.textContent==='lock all','syntax toggle starts as lock all');b.click();
  A(syntaxLockKeys().every(k=>LOCKED.has(k))&&b.textContent==='unlock all','syntax lock-all locks every syntax row and flips label');b.click();
  A(syntaxLockKeys().every(k=>!LOCKED.has(k))&&b.textContent==='lock all','syntax unlock-all clears every syntax lock and flips label');}
 {LOCKED.clear();buildUITable();const b=document.getElementById('uilocktoggle');A(b&&b.textContent==='lock all','ui toggle starts as lock all');b.click();
  A(uiLockKeys().every(k=>LOCKED.has(k))&&b.textContent==='unlock all','ui lock-all locks every UI row and flips label');b.click();
  A(uiLockKeys().every(k=>!LOCKED.has(k))&&b.textContent==='lock all','ui unlock-all clears every UI lock and flips label');}
 {LOCKED.clear();buildPkgTable();const b=document.getElementById('pkglocktoggle');A(b&&b.textContent==='lock all','pkg toggle starts as lock all');b.click();
  A(pkgLockKeys().every(k=>LOCKED.has(k))&&b.textContent==='unlock all','pkg lock-all locks every current package row and flips label');b.click();
  A(pkgLockKeys().every(k=>!LOCKED.has(k))&&b.textContent==='lock all','pkg unlock-all clears every current package lock and flips label');}
 {LOCKED.clear();const app=curApp(),faces=APPS[app].faces.map(r=>r[0]),filter=document.getElementById('pkgfilter');
  if(filter&&faces.length>1){filter.value=faces[0];buildPkgTable();const b=document.getElementById('pkglocktoggle');b.click();
    A(faces.every(face=>LOCKED.has('pkg:'+app+':'+face)),'pkg lock-all covers the whole package even when filtered');
    filter.value='';buildPkgTable();}}
 }));
// Sort gate (open with #sorttest): all three tables now share srtTable/cellVal.
// Verifies the syntax table (which used to have its own srt) sorts by color
// value and by element name, that a repeat click reverses, and that the UI and
// package tables still sort. Guards the unified sort for the later stages.
if(location.hash==='#sorttest')gate('sorttest',A=>{
 const ddVals=tb=>[...document.querySelectorAll('#'+tb+' tr:not(.detailrow)')].map(tr=>{const dd=tr.cells[2].querySelector('.cdd');return dd?(dd.dataset.val||''):'';});
 const txtVals=tb=>[...document.querySelectorAll('#'+tb+' tr:not(.detailrow)')].map(tr=>tr.cells[1].innerText.trim().toLowerCase());
 const asc=a=>a.every((v,i)=>i===0||a[i-1]<=v),desc=a=>a.every((v,i)=>i===0||a[i-1]>=v);
 buildTable();
 srtTable('legbody',2);A(asc(ddVals('legbody')),'legbody-color-asc');
 srtTable('legbody',2);A(desc(ddVals('legbody')),'legbody-color-desc');
 srtTable('legbody',1);A(asc(txtVals('legbody')),'legbody-elements-asc');
 buildUITable();srtTable('uibody',1);A(asc(txtVals('uibody')),'uibody-face-asc');
 buildPkgTable();srtTable('pkgbody',2);A(asc(ddVals('pkgbody')),'pkgbody-fg-asc');
 });
// Live-buffer rendering gate (open with #mocktest): pins the face-faithfulness
// fixes so they cannot silently regress — overlay faces keep syntax colors and
// honor their styles, the cursor sits on a glyph, line numbers honor weight, the
// fringe shows its foreground indicator, and the mode-line carries its box.
if(location.hash==='#mocktest')gate('mocktest',A=>withSavedState(['UIMAP','PKGMAP'],()=>{
 const Q=s=>document.querySelector('#mockframe '+s);
 buildMockFrame();
 A(Q('[data-face="highlight"] [data-k]'),'highlight-keeps-token-colors');
 A(Q('[data-face="region"] [data-k]'),'region-keeps-token-colors');
 const curCell=Q('[data-face="cursor"]');
 A(curCell&&curCell.textContent.trim().length===1,'cursor-on-glyph');
 UIMAP['cursor']={fg:'#112233',bg:'#aabbcc',weight:null,slant:null,underline:null,strike:null,box:null};buildMockFrame();
 const curStyled=Q('[data-face="cursor"]'),curSt=curStyled&&curStyled.getAttribute('style')||'';
 A(curSt.includes('#112233')&&curSt.includes('#aabbcc'),'cursor preview honors fg and bg: '+curSt);
 UIMAP['hl-line']={fg:'#112233',bg:'#aabbcc',weight:null,slant:null,underline:null,strike:null,box:null};buildMockFrame();
 const hlStyled=Q('[data-face="hl-line"]'),hlSt=hlStyled&&hlStyled.getAttribute('style')||'';
 A(hlSt.includes('#112233')&&hlSt.includes('#aabbcc'),'hl-line preview honors fg and bg: '+hlSt);
 UIMAP['link']={fg:'#112233',bg:'#aabbcc',weight:null,slant:null,underline:{style:'line',color:null},strike:null,box:null};buildMockFrame();
 const linkStyled=Q('[data-face="link"]'),linkSt=linkStyled&&linkStyled.getAttribute('style')||'';
 A(linkSt.includes('#112233')&&linkSt.includes('#aabbcc'),'inline UI face preview honors fg and bg: '+linkSt);
 const missing=UI_FACES.map(f=>f[0]).filter(f=>!Q('[data-face="'+f+'"]'));
 A(missing.length===0,'all UI faces are represented in live buffer preview: '+missing.join(','));
 buildTable();buildUITable();buildPkgTable();
 [['#legbody tr[data-kind="kw"]',5],['#uibody tr[data-face="mode-line"]',5],['#pkgbody tr',5]].forEach(([sel,idx])=>{
   const cell=document.querySelector(sel)?.cells[idx],ctl=cell&&cell.querySelector('.boxctl');
   A(cell&&ctl&&ctl.getBoundingClientRect().width<=cell.getBoundingClientRect().width,'box control fits its table cell for '+sel);
 });
 const laz=Q('[data-face="lazy-highlight"]');
 A(laz&&/background:\s*(?!transparent)/.test(laz.getAttribute('style')||''),'overlay-honors-background-style');
 A([...document.querySelectorAll('#mockframe .fr')].some(e=>e.textContent.trim()),'fringe-indicator-present');
 const mlbar=Q('[data-face="mode-line"]');
 A(mlbar&&/box-shadow/.test(mlbar.getAttribute('style')||''),'mode-line-box');
 const textBox=Q('.mbuftext'),border=Q('[data-face="vertical-border"]'),mock=document.getElementById('mockframe');
 if(textBox&&border&&mock){
   const tr=textBox.getBoundingClientRect(),br=border.getBoundingClientRect();
   const ch=parseFloat(getComputedStyle(textBox).fontSize)*0.65;
   A(br.left-tr.right<=ch*4.8,'vertical-border-near-text');
 }else A(false,'vertical-border-layout-elements-present');
 UIMAP['line-number-current-line'].weight='bold';buildMockFrame();
 const curNum=Q('[data-face="line-number-current-line"]');
 A(curNum&&/font-weight:\s*700/.test(curNum.getAttribute('style')||''),'line-number-honors-weight');
 UIMAP['region'].weight=null;UIMAP['region'].slant=null;UIMAP['region'].underline=null;buildUITable();
 const regionRow=[...document.querySelectorAll('#uibody tr')].find(r=>r.dataset.face==='region');
 const pickEnum=(dd,label)=>{dd.click();const o=[..._ddPop.querySelectorAll('.enumopt')].find(b=>b.textContent===label);if(o)o.click();};
 const uiWeight=regionRow.querySelector('.enumdd');
 A(uiWeight&&uiWeight.dataset.val==='','ui weight dropdown starts empty when model is unset');
 pickEnum(uiWeight,'bold');
 A(UIMAP['region'].weight==='bold','ui weight dropdown writes the model');
 const app=curApp(),face=APPS[app].faces[0][0];PKGMAP[app][face].weight=null;buildPkgTable();
 const pkgWeight=()=>document.querySelector('#pkgbody tr[data-face="'+face+'"] .enumdd');
 A(pkgWeight()&&pkgWeight().dataset.val==='','pkg weight dropdown starts empty when model is unset');
 pickEnum(pkgWeight(),'heavy');
 A(PKGMAP[app][face].weight==='heavy'&&PKGMAP[app][face].source==='user','pkg weight dropdown writes the model and marks the face edited');
 }));
// Cursor-row gate (open with #cursorrowtest): the cursor face honors only fg
// (the glyph on it) and bg (the cursor color); weight/slant/underline/strike and
// box are no-ops, so the row mutes them to a dash while non-cursor rows keep them.
if(location.hash==='#cursorrowtest')gate('cursorrowtest',A=>{
  buildUITable();
  const rows=[...document.querySelectorAll('#uibody tr')];
  const cur=rows.find(r=>r.dataset.face==='cursor');
  A(!!cur,'cursor row present');
  A(!!cur.cells[2].querySelector('.cdd'),'cursor keeps the fg swatch');
  A(!!cur.cells[3].querySelector('.cdd'),'cursor keeps the bg swatch');
  A(!cur.cells[4].querySelector('.enumdd')&&cur.cells[4].textContent.includes('—'),'cursor mutes the style controls');
  A(cur.cells[5].textContent.includes('—'),'cursor mutes the box control');
  const ml=rows.find(r=>r.dataset.face==='mode-line');
  A(!!ml.cells[4].querySelector('.enumdd'),'non-cursor rows keep the style controls');
});
// Palette-generator gate (open with #generatortest): previewing is non-mutating,
// clicking a generated tile loads the existing selector, adding creates a normal
// singleton base column, and appending a preview column commits all span members
// under one stable column id.
if(location.hash==='#generatortest')gate('generatortest',A=>{
 const before=JSON.stringify(PALETTE);
 A(document.getElementById('genaccents').value==='5','default accent count is 5');
 A(document.getElementById('gensource').value==='palette','default generator source is palette');
 A(document.querySelector('label:has(#genintent)').title==='what kind of candidate colors to look for','intent label hover explains the control');
 A(document.getElementById('genintent').title.includes('Pure exploration'),'intent select hover explains random');
 A([...document.getElementById('genintent').options].some(o=>o.value==='fill-hue-gaps'),'fill hue gaps intent is available');
 document.getElementById('genintent').value='fill-hue-gaps';syncGeneratorControls();
 A(document.getElementById('genintent').title.includes('underrepresented hue regions'),'fill hue gaps hover explains hue bonus');
 document.getElementById('genintent').value='near-palette';syncGeneratorControls();
 A(document.getElementById('genintent').title.includes('current palette base colors'),'intent select hover updates for selected intent');
 A(!document.getElementById('genscheme'),'legacy scheme control is removed from the generator UI');
 A(!document.getElementById('genhue'),'legacy base hue control is removed from the generator UI');
 document.getElementById('genaccents').value='3';
 document.getElementById('genintent').value='fill-gaps';
 document.getElementById('genvibe').value='warm';
 document.getElementById('gensource').value='palette';
 previewGenerator();
 A(GEN_PROPOSAL&&GEN_PROPOSAL.intent==='fill-gaps'&&GEN_PROPOSAL.vibe==='warm'&&GEN_PROPOSAL.sourceMode==='palette','intent/vibe/source feed the generator proposal');
 A(JSON.stringify(PALETTE)===before,'preview does not mutate palette');
 A(document.querySelectorAll('#genpreview .gencol').length===3,'renders requested preview columns');
 A(parseInt(getComputedStyle(document.querySelector('#genpreview .genchip')).width,10)===128,'generated candidate tile width matches palette tiles');
 A(parseInt(getComputedStyle(document.querySelector('#genpreview .genchip')).height,10)===58,'generated candidate tile height matches palette tiles');
 A([...document.querySelectorAll('#genpreview .gencol')].every(c=>c.querySelectorAll('.genchip').length===1),'preview columns show only one base tile each');
 const tile=document.querySelector('#genpreview .genchip[data-col="0"][data-member="0"]');
 A(!!tile,'base generated tile exists');
 const tileHex=tile&&tile.dataset.hex,tileName=tile&&tile.dataset.name;
  if(tile)tile.click();
 A(document.getElementById('newhexstr').value===tileHex,'generated tile loads selector hex');
 A(document.getElementById('newname').value===tileName,'generated tile loads selector name');
 document.getElementById('genaccents').value='2';
 previewGenerator();
 A(document.querySelectorAll('#genpreview .gencol').length===2,'preview again replaces old preview columns');
 A(!document.querySelector('#genpreview .genchip.sel'),'preview again clears generated tile selection');
 document.getElementById('genaccents').value='3';
 previewGenerator();
 addColor();
 A(PALETTE.some(p=>p[0]===tileHex&&p[1]===tileName),'add color commits selected generated tile');
 const afterSingle=PALETTE.length;
 previewGenerator();
 const append=document.querySelector('#genpreview .genappend[data-col="0"]');
 A(!!append,'append generated column button exists');
 if(append)append.click();
 A(PALETTE.length===afterSingle+1,'append commits one generated base color');
 const added=PALETTE.slice(-1);
 A(new Set(added.map(p=>p[2])).size===1,'appended generated span has one stable column id');
 A(!/[+-]\d+$/.test(added[0][1]),'appended generated color is a base name, not a signed span neighbor');
 GEN_PROPOSAL={summary:{generated:1,rejected:0,minContrast:null},columns:[{name:'medium-aquamarine',members:[{name:'medium-aquamarine',hex:'#66cdaa',offset:0,columnId:'medium-aquamarine'}]}]};
 renderGeneratorPreview();
 A(document.querySelector('#genpreview .genchip .gn').textContent==='medium aquamarine','generated tile names display spaces instead of hyphens');
 });
// Auto-dim gate (open with #autodimtest): the bespoke split preview shows the
// selected language in both panes -- the left in real syntax colors, the right
// collapsed to the single auto-dim-other-buffers face -- and tracks the langsel.
if(location.hash==='#autodimtest')gate('autodimtest',A=>{
 const langs=Object.keys(SAMPLES),ls=document.getElementById('langsel');
 ls.value=langs[0];
 const box=document.createElement('div');box.innerHTML=renderAutodimPreview();
 A(box.innerHTML.includes('>normal<'),'left pane has a "normal" header');
 A(box.innerHTML.includes('>auto-dim<'),'right pane has an "auto-dim" header');
 A(box.querySelectorAll('[data-k]').length>0,'focused pane carries per-token syntax spans');
 const dimFace=box.querySelector('[data-face="auto-dim-other-buffers"]');
 A(!!dimFace,'dimmed pane uses the auto-dim-other-buffers face');
 A(dimFace&&dimFace.querySelectorAll('[data-k]').length===0,'dimmed code is uniform, no per-token syntax');
 A(!!box.querySelector('[data-face="auto-dim-other-buffers-hide"]'),'the hide face is represented');
 if(langs.length>1){const t1=box.textContent;ls.value=langs[1];
  const box2=document.createElement('div');box2.innerHTML=renderAutodimPreview();
  A(box2.textContent!==t1,'preview tracks the language selector');ls.value=langs[0];}
 });
if(location.hash.startsWith('#pick')){openPicker();const m=location.hash.slice(5);if(m){const b=document.querySelector('.pmode button[data-m="'+m+'"]');if(b)b.click();}}
if(location.hash==='#cursortest'){document.getElementById('newhexstr').value='#67809c';openPicker();const sc=document.getElementById('svcur'),hc=document.getElementById('huecur');const L=parseFloat(sc.style.left||'0'),T=parseFloat(sc.style.top||'0'),H=parseFloat(hc.style.top||'0');const ok=L>1&&T>1&&H>1;document.title='CURSORTEST '+(ok?'PASS':'FAIL');const d=document.createElement('div');d.id='cursortest';d.textContent='CURSORTEST '+(ok?'PASS':'FAIL')+' left='+sc.style.left+' top='+sc.style.top+' hue='+hc.style.top;document.body.appendChild(d);}
if(location.hash.startsWith('#app')){const ap=location.hash.slice(4),s=document.getElementById('appsel');if(s&&ap){s.value=ap;pkgChanged();}}
if(location.hash==='#planetest'){let ok=true;const notes=[];
 document.getElementById('newhexstr').value='#67809c';openPicker();setPkModel('oklch');paintPicker();
 const sv=document.getElementById('sv'),cv=document.getElementById('svmask'),ctx=cv.getContext('2d');
 const [L,C,H]=readOklch();
 const expLeft=Math.min(1,C/OKLCH_CMAX)*sv.clientWidth,expTop=(1-L)*sv.clientHeight;
 const gotLeft=parseFloat(document.getElementById('svcur').style.left),gotTop=parseFloat(document.getElementById('svcur').style.top);
 if(Math.abs(gotLeft-expLeft)>2||Math.abs(gotTop-expTop)>2){ok=false;notes.push('crosshair off got '+gotLeft.toFixed(1)+','+gotTop.toFixed(1)+' exp '+expLeft.toFixed(1)+','+expTop.toFixed(1));}
 const Coog=0.38,Loog=0.5,labO=oklch2oklab(Loog,Coog,H),oog=!inGamut(oklab2lrgb(labO.L,labO.a,labO.b));
 const oogX=Math.min(cv.width-2,Math.round((Coog/OKLCH_CMAX)*cv.width)),oogY=Math.round((1-Loog)*cv.height);
 const dO=ctx.getImageData(oogX,oogY,1,1).data,greyO=Math.abs(dO[0]-0x15)<10&&Math.abs(dO[1]-0x12)<10&&Math.abs(dO[2]-0x0f)<10;
 if(oog&&!greyO){ok=false;notes.push('OOG cell not masked rgb '+dO[0]+','+dO[1]+','+dO[2]);}
 const inX=Math.round((0.03/OKLCH_CMAX)*cv.width),inY=Math.round(0.5*cv.height);
 const dI=ctx.getImageData(inX,inY,1,1).data,greyI=Math.abs(dI[0]-0x15)<10&&Math.abs(dI[1]-0x12)<10&&Math.abs(dI[2]-0x0f)<10;
 if(greyI){ok=false;notes.push('in-gamut cell rendered as OOG grey rgb '+dI[0]+','+dI[1]+','+dI[2]);}
 document.title='PLANETEST '+(ok?'PASS':'FAIL');const d=document.createElement('div');d.id='planetest';d.textContent='PLANETEST '+(ok?'PASS':'FAIL')+(notes.length?' | '+notes.join(' ; '):'');document.body.appendChild(d);}
if(location.hash==='#oklchtest'){let ok=true;const notes=[];
 document.getElementById('newhexstr').value='#67809c';openPicker();
 const before=document.getElementById('newhexstr').value;
 setPkModel('oklch');
 if(pkModel!=='oklch'){ok=false;notes.push('model not oklch');}
 if(!document.getElementById('oklchctl').classList.contains('show')){ok=false;notes.push('oklch dials hidden');}
 if(document.getElementById('newhexstr').value!==before){ok=false;notes.push('color changed on model switch: '+document.getElementById('newhexstr').value);}
 pkMode='any';document.querySelector('.pmode button[data-m="aa"]').click();
 if(pkModel!=='oklch'){ok=false;notes.push('mask toggle reset model');}
 if(pkMode!=='aa'){ok=false;notes.push('mask did not set aa');}
 setPkModel('hsv');
 if(pkMode!=='aa'){ok=false;notes.push('model switch reset mask to '+pkMode);}
 if(pkModel!=='hsv'){ok=false;notes.push('model not hsv after switch');}
 setPkModel('oklch');setOklchInputs(0.591,0.052,251.6);pkOklchSet();
 const driven=document.getElementById('newhexstr').value,dl=oklab2oklch(srgb2oklab(driven));
 if(!(Math.abs(dl.L-0.591)<0.02&&Math.abs(dl.C-0.052)<0.02)){ok=false;notes.push('dials did not drive color: '+driven);}
 const {clamped}=oklch2hex(0.7,0.4,140);setOklchInputs(0.7,0.4,140);pkOklchSet();
 if(!(clamped&&document.getElementById('pkclamp').classList.contains('show'))){ok=false;notes.push('clamp status missing for out-of-gamut C');}
 document.title='OKLCHTEST '+(ok?'PASS':'FAIL');const d=document.createElement('div');d.id='oklchtest';d.textContent='OKLCHTEST '+(ok?'PASS':'FAIL')+(notes.length?' | '+notes.join(' ; '):'');document.body.appendChild(d);}
if(location.hash==='#readouttest'){const hex='#67809c';document.getElementById('newhexstr').value=hex;openPicker();pkReadout(hex);
 const o=document.getElementById('pkoklch').textContent,a=document.getElementById('pkapca').textContent,w=document.getElementById('pkcon').textContent;
 const lch=oklab2oklch(srgb2oklab(hex));
 const expO='OKLCH '+lch.L.toFixed(3)+' '+lch.C.toFixed(3)+' '+Math.round(lch.H)+'\u00b0';
 const expA='APCA Lc '+apca(hex,MAP['bg']).toFixed(0);
 const r=contrast(hex,MAP['bg']),expW=r.toFixed(1)+'  '+rating(r);
 const wired=o===expO&&a===expA&&w===expW;
 const sane=Math.abs(lch.L-0.591)<0.01&&Math.abs(lch.C-0.052)<0.01&&Math.abs(lch.H-251.6)<2;
 const ok=wired&&sane;document.title='READOUTTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='readouttest';d.textContent='READOUTTEST '+(ok?'PASS':'FAIL')+' oklch='+o+' | apca='+a+' | wcag='+w;document.body.appendChild(d);}
// Worst-case readout gate (open with #contrasttest): a covered overlay face shows
// the floor over its foreground set and names the limiting foreground, an
// out-of-scope face keeps the single-pair readout, and an empty set reads "no fg set".
if(location.hash==='#contrasttest')gate('contrasttest',A=>{
 const saveMAP=Object.assign({},MAP),saveUI=JSON.parse(JSON.stringify(UIMAP));
 CATS.forEach(c=>{if(c[0]!=='bg'&&c[0]!=='p')setSyntaxFg(c[0],'');});
 setSyntaxFg('p','#f0fef0');setSyntaxFg('kw','#67809c');setSyntaxFg('str','#a3b18a');setSyntaxFg('bg','#000000');
 UIMAP['region']={fg:null,bg:'#202830',weight:null,slant:null,underline:null,strike:null};
 buildUITable();
 const cell=document.getElementById('uicr-region');
 A(cell&&/^\d+\.\d$/.test(cell.textContent.trim()),'region shows a bare worst-case number (no PASS/FAIL word): '+(cell&&cell.textContent));
 A(cell&&!cell.textContent.includes('#67809c'),'compact readout omits limiting fg details: '+(cell&&cell.textContent));
 A(cell&&cell.title.includes('kw (keyword) #67809c'),'hover names failing keyword blue: '+(cell&&cell.title));
 A(!document.querySelector('#uiprev-region .crerr'),'region preview no longer carries a failing-contrast badge');
 const firstFail=cell.title.split('\n')[1];
 A(firstFail&&firstFail.includes('kw (keyword) #67809c'),'failures are sorted from worst first (in the cell hover): '+firstFail);
 const fl=floor('#202830',fgSetForFace('region').set);
 A(fl.limitingHex==='#67809c','floor limiting is blue, got '+fl.limitingHex);
 A(Math.abs(fl.ratio-contrast('#67809c','#202830'))<1e-9,'floor ratio matches blue-on-bg');
 UIMAP['region']={fg:'#f0fef0',bg:'#202830',weight:null,slant:null,underline:null,strike:null};
 buildUITable();
 const pairCell=document.getElementById('uicr-region'),pairWant=contrast('#f0fef0','#202830');
 A(pairCell&&Math.abs(parseFloat(pairCell.textContent)-pairWant)<0.06,'region with explicit fg rates its own fg/bg pair: got '+(pairCell&&pairCell.textContent.trim())+' want '+pairWant.toFixed(1));
 A(!document.querySelector('#uiprev-region .crerr'),'region with explicit fg does not show covered-text error badge');
 A(pairCell&&!pairCell.title.includes('#67809c'),'region explicit fg hover omits underlying syntax failures: '+(pairCell&&pairCell.title));
 const ml=document.getElementById('uicr-mode-line');
 A(worstCellHtml('mode-line')===null,'mode-line is out of scope (single-pair)');
 A(ml&&/^\d/.test(ml.textContent.trim()),'mode-line cell is a numeric ratio: '+(ml&&ml.textContent));
 UIMAP['region']={fg:null,bg:'#202830',weight:null,slant:null,underline:null,strike:null};
 setSyntaxFg('p','');CATS.forEach(c=>{if(c[0]!=='bg')setSyntaxFg(c[0],'');});buildUITable();
 const empty=document.getElementById('uicr-region');
 A(empty&&empty.textContent.trim()==='no fg set','empty set reads the no-set message: '+(empty&&empty.textContent));
 // A two-color face (own fg AND own bg) rates its own pair, never the ground bg.
 UIMAP['mode-line']={fg:'#112233',bg:'#aabbcc',weight:null,slant:null,underline:null,strike:null};
 buildUITable();
 const two=document.getElementById('uicr-mode-line'),twoWant=contrast('#112233','#aabbcc');
 A(two&&Math.abs(parseFloat(two.textContent)-twoWant)<0.06,'ui two-color face rates own fg-on-bg: got '+(two&&two.textContent.trim())+' want '+twoWant.toFixed(1));
 const tApp=Object.keys(APPS)[0],tFace=APPS[tApp].faces[0][0],savePF=JSON.parse(JSON.stringify(PKGMAP[tApp][tFace]));
 Object.assign(PKGMAP[tApp][tFace],{fg:'#112233',bg:'#aabbcc',inherit:null});buildPkgTable();
 const prow=document.querySelector('#pkgbody tr[data-face="'+tFace+'"]'),pcell=prow&&prow.children[6];
 A(pcell&&Math.abs(parseFloat(pcell.textContent)-twoWant)<0.06,'pkg two-color face rates own fg-on-bg: got '+(pcell&&pcell.textContent.trim())+' want '+twoWant.toFixed(1));
 PKGMAP[tApp][tFace]=savePF;buildPkgTable();
 // A ground-bg change must not clobber a face's own preview bg, must leave a
 // two-color ratio alone, and must re-rate a ground-dependent face's cell.
 UIMAP['fringe']={fg:'#ddeeff',bg:null,weight:null,slant:null,underline:null,strike:null};
 buildUITable();
 setSyntaxFg('bg','#440000');applyGround();
 const pv=document.getElementById('uiprev-mode-line');
 A(pv&&pv.style.background==='rgb(170, 187, 204)','ground change keeps a face own preview bg: got '+(pv&&pv.style.background));
 const twoAfter=document.getElementById('uicr-mode-line');
 A(twoAfter&&Math.abs(parseFloat(twoAfter.textContent)-twoWant)<0.06,'ground change leaves a two-color ratio alone: got '+(twoAfter&&twoAfter.textContent.trim()));
 const frc=document.getElementById('uicr-fringe'),frWant=contrast('#ddeeff','#440000');
 A(frc&&Math.abs(parseFloat(frc.textContent)-frWant)<0.06,'ground change re-rates a ground-dependent face: got '+(frc&&frc.textContent.trim())+' want '+frWant.toFixed(1));
 // A default-fg (p) change through the real syntax dropdown re-rates a face
 // whose fg falls back to it. Drives the DOM so the handler wiring is pinned.
 UIMAP['fringe']={fg:null,bg:'#aabbcc',weight:null,slant:null,underline:null,strike:null};
 buildUITable();
 const pLocked=LOCKED.has('p');if(pLocked){LOCKED.delete('p');buildTable();}
 const pdd=document.querySelector('#legbody tr[data-kind="p"] .cdd');
 if(pdd){pdd.click();
  const pHex=PALETTE.find(p=>p[0]!==MAP['p'])[0];
  const prow=[...document.querySelectorAll('.cddpop .cddgc')].find(c=>c.dataset.hex===pHex);
  if(prow)prow.click();
  const pf=document.getElementById('uicr-fringe'),pfWant=contrast(pHex,'#aabbcc');
  A(prow&&pf&&Math.abs(parseFloat(pf.textContent)-pfWant)<0.06,'default-fg change re-rates a p-fallback face: got '+(pf&&pf.textContent.trim())+' want '+pfWant.toFixed(1));
 }else A(false,'syntax table has a p row with a dropdown');
 if(pLocked){LOCKED.add('p');buildTable();}
 for(const k in MAP)delete MAP[k];Object.assign(MAP,saveMAP);syncSyntaxFromCache();for(const f in UIMAP)delete UIMAP[f];Object.assign(UIMAP,saveUI);buildUITable();applyGround();
 });
// Bevel gate (open with #beveltest): released/pressed boxes derive their
// highlight and shadow from the face's effective bg per Emacs's relief
// algorithm, and pressed draws the shadow edge first.
if(location.hash==='#beveltest')gate('beveltest',A=>{
 const saveUI=JSON.parse(JSON.stringify(UIMAP)),saveP=PALETTE.slice(),savePK=JSON.parse(JSON.stringify(PKGMAP));
 UIMAP['mode-line']={fg:'#d8dee9',bg:'#30343c',weight:null,slant:null,underline:null,strike:null,box:{style:'released',width:1,color:null}};
 buildUITable();
 const pv=document.getElementById('uiprev-mode-line');
 const bs=pv&&pv.style.boxShadow;
 A(bs&&bs.includes('rgb(113, 118, 127)'),'released highlight derives from the face bg (#71767f): '+bs);
 A(bs&&bs.includes('rgb(15, 17, 22)'),'released shadow derives from the face bg (#0f1116): '+bs);
 UIMAP['mode-line'].box={style:'pressed',width:1,color:null};paintUI('mode-line');
 const bs2=pv&&pv.style.boxShadow;
 A(bs2&&bs2.includes('rgb(15, 17, 22)')&&bs2.includes('rgb(113, 118, 127)')&&bs2.indexOf('rgb(15, 17, 22)')<bs2.indexOf('rgb(113, 118, 127)'),'pressed swaps the pair (shadow edge first): '+bs2);
 UIMAP['mode-line'].box={style:'line',width:1,color:'#ff0000'};paintUI('mode-line');
 A(pv&&pv.style.boxShadow.includes('rgb(255, 0, 0)'),'line style keeps its explicit color: '+(pv&&pv.style.boxShadow));
 UIMAP['mode-line'].box={style:'released',width:1,color:'#ff0000'};paintUI('mode-line');
 const bs3=pv&&pv.style.boxShadow;
 A(bs3&&bs3.includes('rgb(255, 42, 42)')&&bs3.includes('rgb(143, 0, 0)'),'released style derives relief from explicit box color: '+bs3);
 PALETTE=[['#ff0000','red','red'],['#30343c','slate','slate']];
 buildUITable();
 const mlrow=document.querySelector('#uibody tr[data-face="mode-line"]'),boxCell=mlrow&&mlrow.cells[5],lineBtn=boxCell&&boxCell.querySelector('.boxbtn[data-style="line"]'),boxDd=boxCell&&boxCell.querySelector('.cdd');
 if(lineBtn&&boxDd){lineBtn.click();boxDd.click();const redRow=[...document.querySelectorAll('.cddpop .cddgc')].find(c=>(c.dataset.name||'').includes('red'));if(redRow)redRow.click();}
 A(UIMAP['mode-line'].box&&UIMAP['mode-line'].box.color==='#ff0000','UI box color dropdown writes box.color');
 const app=curApp(),face=APPS[app].faces[0][0];PKGMAP[app][face].box={style:'line',width:1,color:null};buildPkgTable();
 const prow=document.querySelector('#pkgbody tr[data-face="'+face+'"]'),pbox=prow&&prow.cells[5],pdd=pbox&&pbox.querySelector('.cdd');
 if(pdd){pdd.click();const redRow=[...document.querySelectorAll('.cddpop .cddgc')].find(c=>(c.dataset.name||'').includes('red'));if(redRow)redRow.click();}
 A(PKGMAP[app][face].box&&PKGMAP[app][face].box.color==='#ff0000','package box color dropdown writes box.color');
 PALETTE=saveP;PKGMAP=savePK;for(const f in UIMAP)delete UIMAP[f];Object.assign(UIMAP,saveUI);buildUITable();buildPkgTable();
 });
// Gallery gate (open with #gallerytest): the color dropdown opens a 2D grid in
// the palette-panel shape. Driven on a throwaway dropdown so no real face state
// is mutated. Covers: grid opens, every palette color has a cell, a cell click
// fires onPick + updates the trigger, the pick highlights on reopen, the default
// chip clears.
if(location.hash==='#gallerytest')gate('gallerytest',A=>withSavedState(['MAP','SYNTAX'],()=>{
 let picked='__none__';
 const dd=mkColorDropdown(ddList(''),'',(hex)=>{picked=hex;},{});
 document.body.appendChild(dd);
 const trig=dd.querySelector('.cdd');trig.click();
 const pop=document.querySelector('.cddpop.cddgrid');
 A(pop,'dropdown opens a grid popup');
 const cells=pop?[...pop.querySelectorAll('.cddgc')]:[];
 A(cells.length>=PALETTE.length,'grid covers every palette color: '+cells.length+' cells for '+PALETTE.length+' palette colors');
 A(pop&&pop.querySelector('.cddgdef'),'grid has a default chip');
 A(pop&&pop.querySelector('.cddgrow'),'grid lays the colors out in rows');
 const target=PALETTE.find(p=>p[0]!==MAP['bg'])||PALETTE[0];
 const cell=cells.find(c=>c.dataset.hex===target[0]);
 A(cell,'the target color has a cell');
 if(cell){cell.click();
  A(picked===target[0],'clicking a cell calls onPick with the color: '+picked);
  A(trig.dataset.val===target[0],'the trigger button updates to the picked color: '+trig.dataset.val);
  trig.click();
  const sel=document.querySelector('.cddpop.cddgrid .cddgc.sel');
  A(sel&&sel.dataset.hex===target[0],'the picked color is highlighted on reopen: '+(sel&&sel.dataset.hex));
  closeColorDropdown();}
 trig.click();const defc=document.querySelector('.cddpop.cddgrid .cddgdef');if(defc)defc.click();
 A(picked==='','the default chip clears the assignment: '+JSON.stringify(picked));
 dd.remove();closeColorDropdown();
 }));
// Preview-link gate (open with #previewlinktest): known bespoke-preview face
// mappings stay wired to the face that Emacs actually uses.
if(location.hash==='#previewlinktest')gate('previewlinktest',A=>{
 const box=document.createElement('div');
 box.innerHTML=renderOrgPreview();
 const headline=[...box.querySelectorAll('[data-face="org-headline-todo"]')].find(e=>e.textContent.includes('Heading three'));
 A(!!headline&&headline.previousElementSibling&&headline.previousElementSibling.dataset.face==='org-todo','org headline-todo follows a TODO keyword span');
 box.innerHTML=renderFlycheckPreview();
 const delim=[...box.querySelectorAll('[data-face="flycheck-error-delimiter"]')].map(e=>e.textContent).join('');
 const enclosed=[...box.querySelectorAll('[data-face="flycheck-delimited-error"]')].map(e=>e.textContent).join('');
 A(delim==='[]','flycheck delimiters use flycheck-error-delimiter');
 A(enclosed==='err','flycheck enclosed text uses flycheck-delimited-error');
 box.innerHTML=renderErcPreview();
 const own=[...box.querySelectorAll('[data-face="erc-input-face"]')].some(e=>e.textContent.includes('hello everyone'));
 const bob=[...box.querySelectorAll('[data-face="erc-default-face"]')].some(e=>e.textContent.includes('hi craig'));
 A(own,'erc own sent message uses erc-input-face');
 A(bob,'erc remote message uses erc-default-face');
 });
// Safe-lightness gate (open with #safetest): the OKLCH picker shades the unsafe
// lightness band for a selected covered face and hides it when no face is selected.
if(location.hash==='#safetest')gate('safetest',A=>withSavedState(['MAP','SYNTAX'],()=>{
 const saveMAP=Object.assign({},MAP);
 setSyntaxFg('p','#f0fef0');setSyntaxFg('kw','#67809c');setSyntaxFg('bg','#000000');
 document.getElementById('newhexstr').value='#202830';openPicker();setPkModel('oklch');
 setSafeFace('region');
 const band=document.getElementById('svsafe');
 A(band&&band.style.display==='block','safe band shows for an in-scope face');
 A(band&&parseFloat(band.style.height)>0,'safe band has a positive height: '+(band&&band.style.height));
 setSafeFace('');
 A(band&&band.style.display==='none','safe band hidden when no face is selected');
 for(const k in MAP)delete MAP[k];Object.assign(MAP,saveMAP);syncSyntaxFromCache();
 setPkModel('hsv');closePicker();
 }));
// Gone-rebind gate (open with #healtest): deleting a named color then recreating
// the name re-points face references stranded on the old hex to the new color.
if(location.hash==='#healtest')gate('healtest',A=>{
 const saveP=PALETTE.slice(),saveM=Object.assign({},MAP),saveU=JSON.parse(JSON.stringify(UIMAP)),savePK=JSON.parse(JSON.stringify(PKGMAP)),saveG=Object.assign({},lastGone),saveSel=selectedIdx;
 PALETTE=[['#0d0b0a','ground'],['#cdced1','fg'],['#67809c','blue']];setSyntaxFg('kw','#67809c');lastGone={};selectedIdx=null;renderPalette();buildTable();
 const blue=[...document.querySelectorAll('#pals .pchip')].find(c=>c.querySelector('.nm')&&c.querySelector('.nm').value==='blue');
 A(!!(blue&&blue.querySelector('.rm')),'blue chip has a remove button');
 if(blue&&blue.querySelector('.rm'))blue.querySelector('.rm').click();
 A(!PALETTE.some(p=>p[1]==='blue'),'blue was deleted');
 A(lastGone['blue']==='#67809c','delete recorded the gone name->hex');
 document.getElementById('newhexstr').value='#5a7a9a';document.getElementById('newname').value='blue';selectedIdx=null;addColor();
 A(MAP['kw']==='#5a7a9a','face reference re-bound to the recreated name, got '+MAP['kw']);
 A(!('blue' in lastGone),'heal consumed the gone entry');
 PALETTE=saveP;for(const k in MAP)delete MAP[k];Object.assign(MAP,saveM);syncSyntaxFromCache();for(const f in UIMAP)delete UIMAP[f];Object.assign(UIMAP,saveU);PKGMAP=savePK;lastGone=saveG;selectedIdx=saveSel;
 renderPalette();buildTable();buildUITable();if(document.getElementById('pkgbody'))buildPkgTable();
 });
// Column-strip gate (open with #columntest): the palette renders as a pinned
// ground column plus structural columns, chips keep their controls, and renaming
// a color leaves it in the same strip because the column id is stable.
if(location.hash==='#columntest')gate('columntest',A=>{
 const saveP=PALETTE.slice(),saveM=Object.assign({},MAP),saveG=Object.assign({},lastGone),saveSel=selectedIdx;
 setSyntaxFg('bg','#0d0b0a');setSyntaxFg('p','#f0fef0');
 PALETTE=[['#0d0b0a','ground'],['#f0fef0','fg'],['#c0402a','red'],['#3a6ea5','blue'],['#808080','gray']];selectedIdx=null;renderPalette();
 const strips=[...document.querySelectorAll('#pals .fstrip')];
 A(strips.length&&strips[0].dataset.column==='ground','ground column is pinned first');
 A(strips[0].querySelectorAll('.pchip').length===2,'ground column carries bg + fg endpoints');
 A(!!strips[0].querySelector('.fhead + .fcount + .pchip'),'span control sits between header and tiles for ground');
 A(strips.length>=4,'ground + red + blue + gray columns, got '+strips.length);
 PALETTE=[['#3a6ea5','blue','blue']];setSyntaxFg('bg','#0d0b0a');setSyntaxFg('p','#f0fef0');selectedIdx=null;renderPalette();
 const fgChip=[...document.querySelectorAll('#pals .fstrip[data-column="ground"] .pchip')].find(c=>c.querySelector('.nm')&&c.querySelector('.nm').value==='fg');
 A(!!fgChip&&!fgChip.querySelector('.nm').disabled,'missing fg endpoint is normalized into a selectable real chip');
 if(fgChip)fgChip.click();
 A(selectedIdx!==null&&PALETTE[selectedIdx]&&PALETTE[selectedIdx][1]==='fg'&&document.getElementById('newhexstr').value.toLowerCase()==='#f0fef0','clicking normalized fg selects it and updates the color editor');
 PALETTE=[['#0d0b0a','ground'],['#f0fef0','fg'],['#c0402a','red'],['#3a6ea5','blue'],['#808080','gray']];selectedIdx=null;renderPalette();
 const resetStrips=[...document.querySelectorAll('#pals .fstrip')];
 const blueHead=resetStrips.find(s=>s.dataset.column==='blue')&&resetStrips.find(s=>s.dataset.column==='blue').querySelector('.ctitle');
 A(!!blueHead,'normal column header has a selectable title');
 if(blueHead)blueHead.click();
 A(selectedIdx!==null&&PALETTE[selectedIdx][1]==='blue'&&document.getElementById('newhexstr').value.toLowerCase()==='#3a6ea5','clicking a column title selects its base color');
 const blueRight=resetStrips.find(s=>s.dataset.column==='blue')&&resetStrips.find(s=>s.dataset.column==='blue').querySelector('.cmove.right');
 if(blueRight)blueRight.click();
 const moved=[...document.querySelectorAll('#pals .fstrip')].map(s=>s.dataset.column);
 A(moved.indexOf('blue')>moved.indexOf('gray'),'right arrow moves a color column after its neighbor');
 A(!document.querySelector('#pals .fstrip[data-column="ground"] .cdel'),'ground column has no delete button');
 const redChip=[...document.querySelectorAll('#pals .pchip')].find(c=>c.querySelector('.nm')&&c.querySelector('.nm').value==='red');
 A(!!redChip&&!!redChip.querySelector('.rm')&&!!redChip.querySelector('.nm'),'a column chip keeps remove + rename controls');
 if(redChip){
  const redName=redChip.querySelector('.nm');selectedIdx=null;redName.click();
  A(selectedIdx!==null&&PALETTE[selectedIdx][1]==='red','single-clicking a tile name selects the whole tile');
  // Re-query the chip from the live DOM each time: selecting re-renders the
  // palette, so a node captured earlier is detached and getComputedStyle on it
  // returns "" (match -> null). Look it up by the current selection index.
  const liveChip=()=>document.querySelector('#pals .pchip[data-palette-index="'+selectedIdx+'"]');
  const chipHex=chip=>{const m=chip&&getComputedStyle(chip).backgroundColor.match(/\d+/g);return m?rgb2hex(...m.slice(0,3).map(Number)):null;};
  openPicker();setHex('#00ff00');
  A(chipHex(liveChip())==='#00ff00','picker edits preview on the selected palette chip');
  closePicker();
  A(chipHex(liveChip())==='#c0402a'&&PALETTE[selectedIdx][0]==='#c0402a','closing picker restores selected chip without mutating palette');
  A(redName.readOnly===true&&!redName.classList.contains('editing'),'single-clicking a tile name does not enter name edit mode');
  redName.dispatchEvent(new MouseEvent('dblclick',{bubbles:true,cancelable:true}));
  A(redName.readOnly===false&&redName.classList.contains('editing'),'double-clicking a tile name enters edit mode');
  A(redName.selectionStart===0&&redName.selectionEnd===0,'double-clicking places the cursor at the beginning of the name');
  redName.blur();
 }
 const redColumn=redChip&&redChip.closest('.fstrip').dataset.column;
 const ri=PALETTE.findIndex(p=>p[1]==='red');PALETTE[ri][1]='zztop-absurd';renderPalette();
 const renamed=[...document.querySelectorAll('#pals .pchip')].find(c=>c.querySelector('.nm')&&c.querySelector('.nm').value==='zztop-absurd');
 A(!!renamed&&renamed.closest('.fstrip').dataset.column===redColumn,'a renamed color stays in the same strip');
 PALETTE=[['#0d0b0a','bg','ground'],['#f0fef0','fg','ground'],['#0d0b0a','bg2'],['#0d0b0a','bg-alt']];setSyntaxFg('bg','#0d0b0a');setSyntaxFg('p','#f0fef0');selectedIdx=null;renderPalette();
 const bg2Chip=[...document.querySelectorAll('#pals .pchip')].find(c=>c.querySelector('.nm')&&c.querySelector('.nm').value==='bg2');
 A(!!bg2Chip&&bg2Chip.closest('.fstrip').dataset.column==='bg2'&&!!bg2Chip.querySelector('.rm')&&!bg2Chip.querySelector('.lock'),'same-hex bg2 remains a normal removable color column chip');
 if(bg2Chip){bg2Chip.click();document.getElementById('newhexstr').value='#101820';document.getElementById('newname').value='bg2';updateColor();}
 A(MAP['bg']==='#0d0b0a','editing same-hex bg2 does not repoint the real bg assignment');
 A(PALETTE.some(p=>p[1]==='bg2'&&p[0]==='#101820'),'editing same-hex bg2 updates only that palette tile');
 PALETTE=[['#0d0b0a','bg','ground'],['#f0fef0','fg','ground'],['#c0402a','red','red'],['#3a6ea5','blue','blue'],['#92acc2','blue+1','blue'],['#808080','gray','gray']];
 setSyntaxFg('kw','#92acc2');lastGone={};selectedIdx=PALETTE.findIndex(p=>p[1]==='blue+1');renderPalette();
 const del=document.querySelector('#pals .fstrip[data-column="blue"] .cdel');
  A(!!del,'normal column has a delete button');
 const beforeDelete=PALETTE.map(p=>p.join('|')).join('||'),oldConfirm=window.confirm;
 window.confirm=()=>false;
 if(del)del.click();
 A(PALETTE.map(p=>p.join('|')).join('||')===beforeDelete,'canceling column delete leaves the palette unchanged');
 window.confirm=()=>true;
 if(del)del.click();
 window.confirm=oldConfirm;
 A(!PALETTE.some(p=>p[2]==='blue'),'column delete removes every entry with the stable column id');
 A(PALETTE.some(p=>p[1]==='red')&&PALETTE.some(p=>p[1]==='gray'),'column delete leaves neighboring columns alone');
 A(PALETTE.some(p=>groundRoleOfEntry(p,groundPair())==='bg')&&PALETTE.some(p=>groundRoleOfEntry(p,groundPair())==='fg'),'column delete leaves ground entries alone');
 A(MAP['kw']==='#92acc2','column delete leaves face references on removed hexes');
 buildTable();
 const goneTitle=document.querySelector('#legbody tr[data-kind="kw"] .cdd')?.title||'';
 A(goneTitle==='(gone) #92acc2','gone color hover has one hex value: '+goneTitle);
 A(lastGone['blue']==='#3a6ea5'&&lastGone['blue+1']==='#92acc2','column delete records every removed name for recovery');
 A(selectedIdx===null,'column delete clears selected color');
 PALETTE=[['#0d0b0a','bg','ground'],['#f0fef0','fg','ground'],['#c0402a','red','red'],['#3a6ea5','blue','blue'],['#92acc2','blue+1','blue']];
 setSyntaxFg('kw','#3a6ea5');selectedIdx=2;clearPalette();
 A(PALETTE.length===2&&PALETTE.every(p=>groundRoleOfEntry(p,groundPair())),'clear palette leaves only bg and fg tiles');
 A(!PALETTE.some(p=>p[1]==='red'||p[1]==='blue'||p[1]==='blue+1'),'clear palette removes normal color columns and spans');
 A(MAP['kw']==='#3a6ea5','clear palette leaves existing face references on gone hexes');
 A(lastGone['blue']==='#3a6ea5'&&lastGone['blue+1']==='#92acc2','clear palette records removed names for recovery');
 A(selectedIdx===null,'clear palette clears selected color');
 PALETTE=saveP;for(const k in MAP)delete MAP[k];Object.assign(MAP,saveM);syncSyntaxFromCache();lastGone=saveG;selectedIdx=saveSel;renderPalette();
 });
// Count-control gate (open with #counttest): the per-column count regenerates the
// column — count up adds symmetric steps, count down drops the extremes, a
// reference to a surviving step follows the new hex, a reference to a removed step
// is left on its old (now-gone) hex.
if(location.hash==='#counttest')gate('counttest',A=>{
 const saveP=PALETTE.slice(),saveM=Object.assign({},MAP),saveU=JSON.parse(JSON.stringify(UIMAP)),saveSel=selectedIdx;
 paletteShowFull=true;  // this gate asserts span tiles, so render the full palette
 setSyntaxFg('bg','#204060');setSyntaxFg('p','#f0fef0');
 PALETTE=[['#204060','bg'],['#f0fef0','fg']];
 setGroundSpan(2);
 A(MAP['bg']==='#204060'&&MAP['p']==='#f0fef0','spanning ground keeps bg/fg assignments on endpoints');
 A(PALETTE.some(p=>p[1]==='ground+1')&&PALETTE.some(p=>p[1]==='ground+2'),'spanning ground adds interior ground+N entries');
 A(document.querySelector('#pals .fstrip[data-column="ground"] .fhead + .fcount + .pchip'),'ground span control renders before tiles');
 setSyntaxFg('bg','#ffffff');setSyntaxFg('p','#000000');
 PALETTE=[['#ffffff','bg'],['#bbbbbb','ground+1','ground'],['#777777','ground+2','ground'],['#000000','fg']];
 renderPalette();
 const groundNames=[...document.querySelectorAll('#pals .fstrip[data-column="ground"] .pchip .nm')].map(e=>e.value);
 A(groundNames.join('|')==='bg|ground+1|ground+2|fg','ground column order is bg, ground steps, fg even when bg is lighter: '+groundNames.join('|'));
 setSyntaxFg('bg','#204060');setSyntaxFg('p','#f0fef0');
 setGroundSpan(1);
 A(!PALETTE.some(p=>p[1]==='ground+2'),'lowering ground span removes dropped interior steps');
 PALETTE=[['#204060','bg'],['#f0fef0','fg'],['#e0e0e0','near-white','near-white']];
 setColumnCount('#e0e0e0',4);
 A(!PALETTE.some(p=>p[0].toLowerCase()==='#ffffff'&&p[1]!=='fg'),'spanning a near-white base skips generated pure-white tiles');
 PALETTE=[['#204060','bg'],['#f0fef0','fg'],['#101010','near-black','near-black']];
 setColumnCount('#101010',4);
 A(!PALETTE.some(p=>p[0].toLowerCase()==='#000000'&&p[1]!=='bg'),'spanning a near-black base skips generated pure-black tiles');
 PALETTE=[['#204060','bg'],['#f0fef0','fg']];
 regenColumn('#67809c',2,{ground:groundPair()}).members.forEach(m=>PALETTE.push([m.hex,m.offset===0?'blue':'blue'+(m.offset>0?'+'+m.offset:m.offset)]));
 const innerOld=regenColumn('#67809c',2,{ground:groundPair()}).members.find(m=>m.offset===1).hex; // survives a count change
 const outerOld=regenColumn('#67809c',2,{ground:groundPair()}).members.find(m=>m.offset===2).hex; // dropped on count-down
 UIMAP['region']={fg:null,bg:innerOld,weight:null,slant:null,underline:null,strike:null};
 UIMAP['highlight']={fg:null,bg:outerOld,weight:null,slant:null,underline:null,strike:null};
 selectedIdx=null;renderPalette();
 const blueSpanInput=document.querySelector('#pals .fstrip[data-column="blue"] .fcount input');
 A(blueSpanInput&&blueSpanInput.max==='8','normal column span control allows up to 8 per side');
 setColumnCount('#67809c',1);
 const palHexes=new Set(PALETTE.map(p=>p[0].toLowerCase()));
 A(!palHexes.has(outerOld.toLowerCase()),'outer step removed from palette on count down');
 A(UIMAP['highlight'].bg.toLowerCase()===outerOld.toLowerCase(),'a removed-step reference stays on its old (gone) hex');
 const newInner=regenColumn('#67809c',1,{ground:groundPair()}).members.find(m=>m.offset===1).hex;
 A(UIMAP['region'].bg.toLowerCase()===newInner.toLowerCase(),'a surviving-step reference followed the regenerate, got '+UIMAP['region'].bg);
 setColumnCount('#67809c',3);
 const want3=regenColumn('#67809c',3,{ground:groundPair()}).members.map(m=>m.hex.toLowerCase());
 const have=new Set(PALETTE.map(p=>p[0].toLowerCase()));
 A(want3.every(h=>have.has(h)),'count up to 3 adds all 7 span colors to the palette');
 {const _lum=h=>{const n=parseInt(h.slice(1),16),r=(n>>16&255)/255,g=(n>>8&255)/255,b=(n&255)/255;const f=c=>c<=0.03928?c/12.92:((c+0.055)/1.055)**2.4;return 0.2126*f(r)+0.7152*f(g)+0.0722*f(b);};
  const lo=_lum(MAP['bg']),hi=_lum(MAP['p']),blue=PALETTE.filter(p=>p[2]==='blue').map(p=>_lum(p[0]));
  A(blue.length&&blue.every(L=>L>=lo-1e-6&&L<=hi+1e-6),'generated span stays within the bg/fg bounds');}
 PALETTE=saveP;for(const k in MAP)delete MAP[k];Object.assign(MAP,saveM);syncSyntaxFromCache();for(const f in UIMAP)delete UIMAP[f];Object.assign(UIMAP,saveU);selectedIdx=saveSel;renderPalette();
 });
// Base-edit + ground-edit gate (open with #baseedittest): editing a column base
// recolors the whole column at the same count and references follow; editing a
// ground swatch writes the bg/fg assignment.
if(location.hash==='#baseedittest')gate('baseedittest',A=>{
 const saveP=PALETTE.slice(),saveM=Object.assign({},MAP),saveU=JSON.parse(JSON.stringify(UIMAP)),saveSel=selectedIdx;
 setSyntaxFg('bg','#0d0b0a');setSyntaxFg('p','#f0fef0');
 PALETTE=[['#0d0b0a','ground'],['#f0fef0','fg']];
 regenColumn('#67809c',2,{ground:groundPair()}).members.forEach(m=>PALETTE.push([m.hex,m.offset===0?'blue':'blue'+(m.offset>0?'+'+m.offset:m.offset)]));
 UIMAP['region']={fg:null,bg:'#67809c',weight:null,slant:null,underline:null,strike:null};
 renderPalette();buildUITable();
 selectedIdx=PALETTE.findIndex(p=>p[0].toLowerCase()==='#67809c');
 document.getElementById('newhexstr').value='#3a8a8a';document.getElementById('newname').value='teal';
 updateColor();
 const column=columnsFromPalette(PALETTE,groundPair()).columns[0];
 A(column&&column.members.some(m=>m.hex.toLowerCase()==='#3a8a8a'),'column base recolored to the new hex');
 A(column&&column.members.length===5,'count preserved (±2 → 5 members), got '+(column&&column.members.length));
 A(!new Set(PALETTE.map(p=>p[0].toLowerCase())).has('#67809c'),'old base removed from palette');
 A(UIMAP['region'].bg.toLowerCase()==='#3a8a8a','a reference to the base followed to the new base hex');
 // ground edit: select bg, change hex, MAP.bg follows
 selectedIdx=PALETTE.findIndex(p=>p[0].toLowerCase()==='#0d0b0a');
 document.getElementById('newhexstr').value='#101010';document.getElementById('newname').value='ground';
 updateColor();
 A(MAP['bg'].toLowerCase()==='#101010','editing the bg swatch wrote the bg assignment, got '+MAP['bg']);
 // fg edit: even when a normal column shares the old fg hex, editing fg must not regenerate that column as fg-*.
 setSyntaxFg('bg','#0d0b0a');setSyntaxFg('p','#e0e0e0');
 PALETTE=[['#0d0b0a','bg','ground'],['#e0e0e0','fg','ground'],['#c0c0c0','silver-1','silver'],['#e0e0e0','silver','silver'],['#f4f4f4','silver+1','silver']];
 selectedIdx=PALETTE.findIndex(p=>p[1]==='fg');
 document.getElementById('newhexstr').value='#d8d8d8';document.getElementById('newname').value='fg';
 updateColor();
 A(MAP['p'].toLowerCase()==='#d8d8d8','editing the fg swatch wrote the fg assignment, got '+MAP['p']);
 A(PALETTE.some(p=>p[1]==='silver'&&p[2]==='silver'),'editing fg does not rename a same-hex normal column base');
 A(!PALETTE.some(p=>/^fg[+-]\d+$/.test(p[1])),'editing fg does not generate fg span tiles from a same-hex normal column');
 A(PALETTE.find(p=>p[1]==='fg')[2]==='ground','editing fg preserves the ground column id');
 PALETTE=saveP;for(const k in MAP)delete MAP[k];Object.assign(MAP,saveM);syncSyntaxFromCache();for(const f in UIMAP)delete UIMAP[f];Object.assign(UIMAP,saveU);selectedIdx=saveSel;renderPalette();
 });
// Round-trip gate (open with #roundtriptest): export stays a flat palette with
// stable column ids, and import does not need color-derived column reconstruction.
if(location.hash==='#roundtriptest')gate('roundtriptest',A=>{
 const stable=o=>Array.isArray(o)?o.map(stable):(o&&typeof o==='object'?Object.fromEntries(Object.keys(o).sort().map(k=>[k,stable(o[k])])):o);
 const diff=(a,b,p='')=>{if(JSON.stringify(a)===JSON.stringify(b))return '';if(typeof a!==typeof b||!a||!b||typeof a!=='object')return p+': '+JSON.stringify(a)+' != '+JSON.stringify(b);
  const ks=[...new Set([...Object.keys(a),...Object.keys(b)])].sort();for(const k of ks){const d=diff(a[k],b[k],p?p+'.'+k:k);if(d)return d;}return p;};
 const saveP=PALETTE.slice(),saveM=Object.assign({},MAP),saveL=new Set(LOCKED);
 PALETTE=[['#ffffff','bg','ground'],['#000000','fg','ground'],['#224466','blue','blue'],['#446688','renamed-blue','blue']];
 setSyntaxFg('bg','#ffffff');setSyntaxFg('p','#000000');
 LOCKED=new Set(['kw','ui:region','pkg:'+curApp()+':'+APPS[curApp()].faces[0][0]]);
 const before=JSON.stringify(exportObj());
 applyImported(before);
 const after=JSON.stringify(exportObj());
 const bObj=stable(JSON.parse(before)),aObj=stable(JSON.parse(after));
 A(JSON.stringify(bObj)===JSON.stringify(aObj),'export → import → export is semantically stable: '+diff(bObj,aObj));
 const obj=JSON.parse(after);
 A(Array.isArray(obj.palette)&&obj.palette.every(e=>Array.isArray(e)&&e.length>=3&&typeof e[2]==='string'),'exported palette carries flat [hex,name,columnId] entries');
 A(obj.palette.some(e=>e[1]==='renamed-blue'&&e[2]==='blue'),'renamed color keeps its stable column id through export/import');
 A(obj.locks&&obj.locks.includes('kw')&&obj.locks.includes('ui:region'),'lock state survives export/import');
 PALETTE=saveP;for(const k in MAP)delete MAP[k];Object.assign(MAP,saveM);syncSyntaxFromCache();LOCKED=saveL;
 });
// View-selector gate (open with #viewtest): the assignment panel is driven by a
// single #viewsel dropdown -- two editor entries (@code, @ui) then a "package
// faces" optgroup of every app, alphabetically by label -- and switching it
// shows exactly one of the three view blocks.
if(location.hash==='#viewtest')gate('viewtest',A=>{
 const sel=document.getElementById('viewsel');
 A(!!sel,'viewsel-exists');
 if(sel){
  A(sel.options[0]&&sel.options[0].value==='@code','first-option-code');
  A(sel.options[1]&&sel.options[1].value==='@ui','second-option-ui');
  const og=sel.querySelector('optgroup');
  A(og&&og.label==='package faces','package-faces-optgroup');
  if(og){const appOpts=[...og.querySelectorAll('option')].map(o=>o.value);
   A(JSON.stringify(appOpts)===JSON.stringify(appViewKeysSorted(APPS)),'optgroup-lists-apps-alphabetically');}
  const vis=id=>{const e=document.getElementById(id);return !!e&&e.style.display!=='none';};
  sel.value='@code';onViewChange();
  A(vis('view-code')&&!vis('view-ui')&&!vis('view-pkg'),'code-view-only');
  sel.value='@ui';onViewChange();
  A(!vis('view-code')&&vis('view-ui')&&!vis('view-pkg'),'ui-view-only');
  const firstApp=Object.keys(APPS)[0];sel.value=firstApp;onViewChange();
  A(!vis('view-code')&&!vis('view-ui')&&vis('view-pkg'),'pkg-view-only');
  A(curApp()===firstApp,'curApp-returns-selected-app');
  A(!document.querySelector('#pkgbody .sbtn[title="reset to default"]'),'no-per-row-reset-button');
 }
 });
// Non-default-marker gate (open with #ndtest): a per-face setting cell gets the
// .nd corner flag only when its value differs from the face's seed default. Cell
// order in a pkg row: 0 lock, 1 label, 2 fg, 3 bg, 4 style, 5 box, 6 contrast.
// inherit + height live in the row expander, so a non-default height flags the
// expander toggle (exp-nd) rather than an inline cell.
if(location.hash==='#ndtest')gate('ndtest',A=>withSavedState(['PKGMAP','LOCKED'],()=>{
 LOCKED.clear();
 const app=curApp(),row=APPS[app].faces[0],face=row[0];
 PKGMAP[app][face]=seedFace(row[2]||{});buildPkgTable();
 const tr0=document.querySelector('#pkgbody tr[data-face="'+face+'"]');
 A(tr0&&![...tr0.cells].some(c=>c.classList.contains('nd')),'default-face-has-no-marker');
 PKGMAP[app][face].height=1.7;PKGMAP[app][face].source='user';buildPkgTable();
 const tr1=document.querySelector('#pkgbody tr[data-face="'+face+'"]');
 A(tr1.querySelector('.exptoggle').classList.contains('exp-nd'),'nondefault-height-flags-expander');
 A(!tr1.cells[4].classList.contains('nd'),'unchanged-style-box-stays-unmarked');
 PKGMAP[app][face].height=(row[2]&&row[2].height)||1;PKGMAP[app][face].weight=seedFace(row[2]||{}).weight==='bold'?null:'bold';buildPkgTable();
 const tr2=document.querySelector('#pkgbody tr[data-face="'+face+'"]');
 A(tr2.cells[4].classList.contains('nd'),'toggled-weight-marks-style-box');
 A(!tr2.querySelector('.exptoggle').classList.contains('exp-nd'),'restored-height-unflags-expander');
 PKGMAP[app][face]=seedFace(row[2]||{});buildPkgTable();
 }));
// Contrast-cell gate (open with #crtest): the per-face contrast column shows a
// bare colored number (no PASS/FAIL word); the WCAG verdict lives in the hover.
if(location.hash==='#crtest')gate('crtest',A=>{
 const app=curApp(),face=APPS[app].faces[0][0];buildPkgTable();
 const cell=document.querySelector('#pkgbody tr[data-face="'+face+'"]').cells[6];
 const span=cell&&cell.querySelector('span');
 A(span&&/^\d+\.\d$/.test(span.textContent.trim()),'contrast cell is a bare number: '+(span&&span.textContent));
 A(span&&!/PASS|FAIL/.test(span.textContent),'no PASS/FAIL word in the contrast cell');
 A(span&&span.title&&/(passes|fails) WCAG/i.test(span.title),'contrast cell carries a WCAG hover: '+(span&&span.title));
 });
// View-nav gate (open with #navtest): the prev/next arrows flanking the view
// dropdown step the selection (clamped, no wrap) and re-render the view.
if(location.hash==='#navtest')gate('navtest',A=>{
 const sel=document.getElementById('viewsel'),prev=document.getElementById('viewprev'),next=document.getElementById('viewnext');
 A(!!prev&&!!next,'nav arrows exist');
 if(sel&&prev&&next){
  const vis=id=>{const e=document.getElementById(id);return !!e&&e.style.display!=='none';};
  sel.selectedIndex=0;onViewChange();
  next.click();A(sel.selectedIndex===1,'next advances the selection');
  prev.click();A(sel.selectedIndex===0,'prev steps back');
  prev.click();A(sel.selectedIndex===0,'prev clamps at the first option');
  sel.selectedIndex=sel.options.length-1;onViewChange();
  next.click();A(sel.selectedIndex===sel.options.length-1,'next clamps at the last option');
  sel.selectedIndex=2;onViewChange();
  A(sel.options[2]&&sel.options[2].value[0]!=='@'&&vis('view-pkg'),'stepping to a package shows the pkg view');
 }
 });
// Markdown-preview gate (open with #mdtest): markdown-mode has a dedicated README
// renderer, and every data-face it emits is a real markdown-mode face.
if(location.hash==='#mdtest')gate('mdtest',A=>{
 A(APPS['markdown-mode']&&APPS['markdown-mode'].preview==='markdown','markdown-mode wired to the markdown preview');
 A(!!PACKAGE_PREVIEWS['markdown'],'markdown renderer registered');
 if(PACKAGE_PREVIEWS['markdown']&&APPS['markdown-mode']){
  assertPreviewFaces(A, PACKAGE_PREVIEWS['markdown'](), APPS['markdown-mode'].faces, 15, 'markdown',
   ['markdown-header-face-1','markdown-bold-face','markdown-inline-code-face','markdown-blockquote-face','markdown-gfm-checkbox-face','markdown-table-face']);
 }
 });
// mu4e-preview gate (open with #mupreviewtest): the mu4e preview is a realistic
// headers list + message view, and every data-face it emits is a real mu4e face.
if(location.hash==='#mupreviewtest')gate('mupreviewtest',A=>{
 assertPreviewFaces(A, renderMu4ePreview(), APPS['mu4e']&&APPS['mu4e'].faces, 20, 'mu4e',
  ['mu4e-unread-face','mu4e-flagged-face','mu4e-replied-face','mu4e-draft-face','mu4e-trashed-face','mu4e-header-highlight-face','mu4e-header-marks-face','mu4e-contact-face','mu4e-compose-separator-face']);
 });
// gnus-preview gate (open with #gnustest): gnus is its own view package (it drives
// the mu4e article view), and every data-face its preview emits is a real gnus face.
if(location.hash==='#gnustest')gate('gnustest',A=>{
 A(!!APPS['gnus'],'gnus is a registered view package');
 A(APPS['gnus']&&APPS['gnus'].preview==='gnus','gnus uses the gnus preview renderer');
 assertPreviewFaces(A, renderGnusPreview(), APPS['gnus']&&APPS['gnus'].faces, 20, 'gnus',
  ['gnus-header-name','gnus-header-from','gnus-header-subject','gnus-cite-1','gnus-cite-attribution','gnus-signature','gnus-button','gnus-emphasis-highlight-words']);
 });
// nerd-icons legend gate (open with #nerdiconstest): nerd-icons is a bespoke
// filetype-legend app; every glyph span is a real nerd-icons face, the dir row
// models nerd-icons-yellow, and recoloring a face repaints every row mapped to it.
if(location.hash==='#nerdiconstest')gate('nerdiconstest',A=>{
 A(!!APPS['nerd-icons'],'nerd-icons is a registered app');
 A(APPS['nerd-icons']&&APPS['nerd-icons'].preview==='nerdicons','nerd-icons uses the nerdicons preview renderer');
 A(!!PACKAGE_PREVIEWS['nerdicons'],'nerdicons renderer registered');
 const legend=(APPS['nerd-icons']&&APPS['nerd-icons'].legend)||[];
 A(Array.isArray(legend)&&legend.length>=10,'legend has the curated rows ('+legend.length+')');
 const dir=legend.find(r=>r.key==='dir');
 A(dir&&dir.face==='nerd-icons-yellow','dir row models nerd-icons-yellow');
 // Gallery: the full colored catalog as a grid — one row per color face, rows
 // ordered by hue so families cluster, each color's distinct icons deduped.
 const gallery=(APPS['nerd-icons']&&APPS['nerd-icons'].gallery)||[];
 A(Array.isArray(gallery)&&gallery.length>=30,'gallery has the color groups ('+gallery.length+')');
 const hues=gallery.map(g=>g.hue);
 A(hues.every((hu,i)=>i===0||hues[i-1]<=hu),'gallery rows ordered by hue (families cluster)');
 A(gallery.every(g=>typeof g.face==='string'&&g.face.indexOf('nerd-icons-')===0&&typeof g.hue==='number'&&Array.isArray(g.glyphs)&&g.glyphs.length>0),'every gallery group is a real nerd-icons face with a hue and glyphs');
 A(gallery.every(g=>g.glyphs.every(e=>e.glyph&&e.name)),'every gallery glyph carries glyph and icon name');
 A(gallery.every(g=>new Set(g.glyphs.map(e=>e.name)).size===g.glyphs.length),'icons are deduplicated within each color row');
 if(PACKAGE_PREVIEWS['nerdicons']&&APPS['nerd-icons']){
  // assertPreviewFaces over the grid — every data-face, across the ~314 deduped
  // glyph cells and the per-row swatches, is a real nerd-icons face with a valid owner.
  assertPreviewFaces(A, renderNerdIconsPreview(), APPS['nerd-icons'].faces, 10, 'nerd-icons',
   ['nerd-icons-purple','nerd-icons-yellow','nerd-icons-blue','nerd-icons-dblue']);
  // Recoloring a face repaints every element in its row (the swatch + each glyph
  // cell), since os reads the live registry.
  withSavedState(['PKGMAP'],()=>{
   const target='nerd-icons-purple',gGroup=gallery.find(g=>g.face===target);
   const expected=gGroup?1+gGroup.glyphs.length:0;
   A(!!gGroup,'gallery has a '+target+' row');
   PKGMAP['nerd-icons']=PKGMAP['nerd-icons']||{};
   PKGMAP['nerd-icons'][target]={fg:'#abcdef',bg:null,weight:null,slant:null,inherit:null,height:1,source:'user'};
   const box=document.createElement('div');box.innerHTML=renderNerdIconsPreview();
   const els=[...box.querySelectorAll('[data-face="'+target+'"]')];
   A(els.length===expected,'every '+target+' element rendered, swatch+glyphs ('+els.length+'/'+expected+')');
   A(els.length>0&&els.every(e=>/#abcdef/i.test(e.getAttribute('style')||'')),'recolor repaints every element in the row');
  });
  // Export/import round-trip over an assigned nerd-icons color; the separate
  // nerd-icons-completion app (dir-face) is untouched by the nerd-icons pane.
  const m=seedPkgmap();
  m['nerd-icons']['nerd-icons-blue']={fg:'#123456',bg:null,weight:null,slant:null,inherit:null,height:1,source:'user'};
  const exp=packagesForExport(m);
  A(exp['nerd-icons']&&exp['nerd-icons']['nerd-icons-blue']&&exp['nerd-icons']['nerd-icons-blue'].fg==='#123456','assigned nerd-icons color exports');
  const round=seedPkgmap();mergePackagesInto(round,exp);
  A(round['nerd-icons']&&round['nerd-icons']['nerd-icons-blue'].fg==='#123456','nerd-icons color re-imports to the same state');
  A(!(exp['nerd-icons']&&('nerd-icons-completion-dir-face' in exp['nerd-icons'])),'dir-face stays out of the nerd-icons app');
 }
 });
// Preview-pane dropdown gate (open with #previewpanetest): the preview label is a
// "preview:" dropdown. A single-pane app shows its name disabled; nerd-icons is
// multi-pane (one pane per font size in pt), enabled, and selecting a size renders
// the grid at it. Locate is unaffected — the flash targets whatever pane is rendered.
if(location.hash==='#previewpanetest')gate('previewpanetest',A=>{
 const np=previewPanes('nerd-icons');
 A(np.length===NERD_ICON_SIZES_PT.length&&np.length>1,'nerd-icons is multi-pane, one per size ('+np.length+')');
 A(np.every(p=>typeof p.size==='number'&&/ pt$/.test(p.label)),'each nerd-icons pane carries a pt size and a label');
 A(NERD_ICON_SIZES_PT[defaultPaneIdx('nerd-icons')]===NERD_ICON_DEFAULT_PT,'nerd-icons defaults to '+NERD_ICON_DEFAULT_PT+' pt');
 const single=Object.keys(APPS).find(k=>k!=='nerd-icons');
 A(previewPanes(single).length===1,'a non-nerd-icons app has a single pane ('+single+')');
 // size drives the rendered glyph font-size; no arg defaults to 14 pt
 const small=renderNerdIconsPreview(10),big=renderNerdIconsPreview(24);
 A(/font-size:10pt/.test(small)&&!/font-size:24pt/.test(small),'10 pt pane renders glyphs at 10pt');
 A(/font-size:24pt/.test(big)&&!/font-size:10pt/.test(big),'24 pt pane renders glyphs at 24pt');
 A(/font-size:14pt/.test(renderNerdIconsPreview()),'default (no-arg) pane renders glyphs at 14 pt');
 // gallery-absent fallback: the dropdown must not promise sizes it can't render —
 // with no gallery, one pane only and the grid falls back to the generic preview.
 const savedG=APPS['nerd-icons'].gallery;delete APPS['nerd-icons'].gallery;
 A(previewPanes('nerd-icons').length===1,'no gallery -> single pane (dropdown disabled)');
 A(!/ni-gallery/.test(renderNerdIconsPreview()),'no gallery -> grid falls back to the generic preview');
 APPS['nerd-icons'].gallery=savedG;
 // DOM wiring: dropdown enabled+populated on nerd-icons, disabled on a single-pane app
 const vs=document.getElementById('viewsel'),saved=vs&&vs.value;
 if(vs){
  vs.value='nerd-icons';
  if(curApp()==='nerd-icons'){
   PREV_PANE['nerd-icons']=99;       // a stale, out-of-range selection
   buildPkgPreview();
   const sel=document.getElementById('pkgprevsel');
   A(+sel.value===defaultPaneIdx('nerd-icons'),'a stale pane index resets to the default');
   A(!sel.disabled&&sel.options.length===NERD_ICON_SIZES_PT.length,'nerd-icons: dropdown enabled with one option per size');
   // Left/Right arrows step the size, clamped at the ends.
   PREV_PANE['nerd-icons']=0;buildPkgPreview();
   document.getElementById('pkgprevsel').dispatchEvent(new KeyboardEvent('keydown',{key:'ArrowRight',bubbles:true}));
   A(PREV_PANE['nerd-icons']===1,'ArrowRight steps to the next size');
   document.getElementById('pkgprevsel').dispatchEvent(new KeyboardEvent('keydown',{key:'ArrowLeft',bubbles:true}));
   document.getElementById('pkgprevsel').dispatchEvent(new KeyboardEvent('keydown',{key:'ArrowLeft',bubbles:true}));
   A(PREV_PANE['nerd-icons']===0,'ArrowLeft steps back and clamps at the first size');
   // The visible ‹ › buttons step the size too, and clamp.
   PREV_PANE['nerd-icons']=0;buildPkgPreview();
   document.getElementById('pkgprevnext').click();
   A(PREV_PANE['nerd-icons']===1,'the > button steps to the next size');
   document.getElementById('pkgprevprev').click();
   document.getElementById('pkgprevprev').click();
   A(PREV_PANE['nerd-icons']===0,'the < button steps back and clamps at the first size');
   A(!document.getElementById('pkgprevprev').disabled&&!document.getElementById('pkgprevnext').disabled,'the nav buttons are enabled when multi-pane');
   // The glyph actually computes to the selected point size (pt -> px): 24 pt = 32 px.
   PREV_PANE['nerd-icons']=NERD_ICON_SIZES_PT.indexOf(24);buildPkgPreview();
   const gw=document.querySelector('#pkgpreview .ni-cell > span');
   const gpx=gw?parseFloat(getComputedStyle(gw).fontSize):0;
   A(Math.abs(gpx-32)<2,'24 pt glyph computes to ~32 px, so the point size renders to size ('+gpx+' px)');
   PREV_PANE['nerd-icons']=defaultPaneIdx('nerd-icons');
   vs.value=single;buildPkgPreview();
   A(sel.disabled&&sel.options.length===1,'single-pane app: dropdown disabled with one option');
   A(document.getElementById('pkgprevprev').disabled&&document.getElementById('pkgprevnext').disabled,'single-pane app: the nav buttons are disabled too');
  }
  vs.value=saved;buildPkgPreview();
 }
});
// picker-distinct gate (open with #pickertest): the color picker panel must stand
// out from the page background. It carries a highlighted gold accent border, and its
// background is meaningfully lighter than the body so the two are easy to tell apart.
if(location.hash==='#pickertest')gate('pickertest',A=>{
 const pk=document.getElementById('picker');A(!!pk,'picker element exists');
 if(pk){const cs=getComputedStyle(pk),body=getComputedStyle(document.body);
  const bc=(cs.borderTopColor.match(/\d+/g)||[]).slice(0,3).map(Number);
  const pkbg=(cs.backgroundColor.match(/\d+/g)||[]).slice(0,3).map(Number);
  const bdbg=(body.backgroundColor.match(/\d+/g)||[]).slice(0,3).map(Number);
  A(bc.join(',')==='232,189,48','picker carries the gold accent border (got '+cs.borderTopColor+')');
  const lift=pkbg.map((c,i)=>c-bdbg[i]);
  A(lift.every(d=>d>=12),'picker background is clearly lighter than the page (per-channel lift '+lift.join(',')+')');
 }
 });
// Box-cluster gate (open with #boxtest): the box control is a 2x2 cluster of
// four radio buttons (none / line / pressed / raised); the color swatch shows
// only while a box style is active.
if(location.hash==='#boxtest')gate('boxtest',A=>{
 LOCKED.clear();const f=UI_FACES.map(x=>x[0]).find(x=>x!=='cursor');const saveBox=UIMAP[f].box;  // cursor has no box control by design
 UIMAP[f].box=null;buildUITable();
 const cell=document.querySelector('#uibody tr[data-face="'+f+'"]').cells[5];
 A(!!cell.querySelector('.boxcluster'),'box-cluster-present');
 A(cell.querySelectorAll('.boxbtn').length===4,'four-box-buttons');
 const dd=cell.querySelector('.cstep');
 A(dd&&dd.style.display==='none','color-hidden-when-no-box');
 const lineBtn=cell.querySelector('.boxbtn[data-style="line"]');lineBtn.click();
 A(UIMAP[f].box&&UIMAP[f].box.style==='line','line-click-sets-style');
 A(lineBtn.classList.contains('on'),'line-button-active');
 A(dd.style.display!=='none','color-shown-when-box-active');
 cell.querySelector('.boxbtn[data-style=""]').click();
 A(UIMAP[f].box===null,'blank-click-clears-box');
 A(dd.style.display==='none','color-hidden-again-after-clear');
 UIMAP[f].box=saveBox;buildUITable();
 });
// Style-cluster gate (open with #styletest): the style cell holds a weight
// selector, a slant selector, and box-like underline and strike controls.
if(location.hash==='#styletest')gate('styletest',A=>{
 buildUITable();const f=UI_FACES.map(x=>x[0]).find(x=>x!=='cursor');  // cursor row has no style cluster by design
 const cell=document.querySelector('#uibody tr[data-face="'+f+'"]').cells[4];
 const cluster=cell.querySelector('.stylecluster');
 A(!!cluster,'style-cluster-present');
 const dds=cluster?cluster.querySelectorAll('.enumdd'):[];
 A(dds.length===2,'weight-and-slant-custom-dropdowns-present');
 dds[0]&&dds[0].click();
 const wopts=_ddPop?[..._ddPop.querySelectorAll('.enumopt')]:[];
 A(wopts.some(b=>b.textContent==='semibold'),'weight-dropdown-spells-out-the-curated-range: '+wopts.map(b=>b.textContent).join(','));
 const wbold=wopts.find(b=>b.textContent==='bold');
 A(wbold&&wbold.style.fontWeight==='700','weight-options-preview-their-own-weight: bold renders 700, got '+(wbold&&wbold.style.fontWeight));
 closeColorDropdown();
 dds[1]&&dds[1].click();
 const sopts=_ddPop?[..._ddPop.querySelectorAll('.enumopt')]:[];
 A(sopts.some(b=>b.textContent==='oblique'),'slant-dropdown-offers-oblique: '+sopts.map(b=>b.textContent).join(','));
 const sital=sopts.find(b=>b.textContent==='italic');
 A(sital&&sital.style.fontStyle==='italic','slant-options-preview-their-own-slant: italic renders italic');
 closeColorDropdown();
 A(cluster&&cluster.querySelectorAll('.boxctl').length===1,'strike-control-in-row-underline-moved-to-expander');
 });
// Expander gate (open with #expandtest): the per-row "more" toggle reveals a
// detail row with the overflow attribute editor, and its controls write the model.
if(location.hash==='#expandtest')gate('expandtest',A=>{
 buildUITable();
 const row=document.querySelector('#uibody tr[data-face="region"]');
 const detail=document.querySelector('#uibody tr.detailrow[data-detail-for="region"]');
 A(!!detail,'detail-row-present');
 A(detail&&detail.style.display==='none','detail-row-hidden-by-default');
 const btn=row.querySelector('.exptoggle');
 A(!!btn,'expander-toggle-present');
 btn&&btn.click();
 A(detail&&detail.style.display!=='none','toggle-reveals-detail-row');
 const ed=detail&&detail.querySelector('.detailedit');
 A(ed&&ed.querySelectorAll('.detailfield').length>=6,'detail-editor-has-the-overflow-fields');
 // ui faces also expose inherit + height in the expander
 A(ed&&ed.querySelector('select.detailsel'),'ui-expander-offers-inherit');
 A(ed&&ed.querySelector('input.hstep'),'ui-expander-offers-height');
 // underline moved into the expander; its wave style writes a styled object
 const uiUnder=ed&&ed.querySelector('.boxctl .boxbtn[data-style="wave"]');
 A(!!uiUnder,'underline-control-in-expander');
 uiUnder&&uiUnder.click();
 A(UIMAP['region'].underline&&UIMAP['region'].underline.style==='wave','underline-control-writes-a-wavy-object');
 // family text input writes the model
 const fam=ed&&ed.querySelector('input.detailinput');
 if(fam){fam.value='Iosevka';fam.dispatchEvent(new Event('change'));}
 A(UIMAP['region'].family==='Iosevka','family-input-writes-the-model');
 // inverse checkbox writes the model
 const inv=ed&&ed.querySelector('input.detailcheck');
 if(inv){inv.checked=true;inv.dispatchEvent(new Event('change'));}
 A(UIMAP['region'].inverse===true,'inverse-checkbox-writes-the-model');
 // a hidden non-default attribute flags the collapsed toggle (reset region to its
 // default first, since the edits above left several overflow attrs changed)
 UIMAP['region']=JSON.parse(JSON.stringify(DEFAULT_UIMAP['region']));buildUITable();
 const cleanbtn=document.querySelector('#uibody tr[data-face="region"] .exptoggle');
 A(cleanbtn&&!cleanbtn.classList.contains('exp-nd'),'toggle-unflagged-when-overflow-matches-default');
 UIMAP['region']=JSON.parse(JSON.stringify(DEFAULT_UIMAP['region']));UIMAP['region'].overline={color:null};buildUITable();
 const ndbtn=document.querySelector('#uibody tr[data-face="region"] .exptoggle');
 A(ndbtn&&ndbtn.classList.contains('exp-nd'),'collapsed-toggle-flags-a-hidden-non-default-attr');
 // package expander now exposes inherit + height (folded out of inline columns)
 buildPkgTable();const pface=APPS[curApp()].faces[0][0];
 const pdetail=document.querySelector('#pkgbody tr.detailrow[data-detail-for="'+pface+'"]');
 A(pdetail&&pdetail.querySelector('select.detailsel'),'package-expander-offers-inherit');
 });
// Height-clamp gate (open with #heighttest): the expander height field coerces a
// typed value into [HEIGHT_MIN,HEIGHT_MAX] and writes the clamped number back, so
// an out-of-range type/paste can't reach the model. Guards the fact that an
// <input type=number> min/max only constrain its steppers, never typed text.
if(location.hash==='#heighttest')gate('heighttest',A=>{
 const face=UI_FACES[0][0],save=JSON.parse(JSON.stringify(UIMAP[face]));
 buildUITable();
 const hin=()=>document.querySelector('#uibody tr.detailrow[data-detail-for="'+face+'"] .hstep');
 const typeHeight=(v)=>{const h=hin();h.value=v;h.dispatchEvent(new Event('change'));};
 typeHeight('5');
 A(UIMAP[face].height===HEIGHT_MAX,'above-max-clamps-to-ceiling: '+UIMAP[face].height);
 A(hin().value===''+HEIGHT_MAX,'field-shows-the-clamped-ceiling: '+hin().value);
 typeHeight('0.05');
 A(UIMAP[face].height===HEIGHT_MIN,'below-floor-clamps-to-floor: '+UIMAP[face].height);
 typeHeight('1.2');
 A(UIMAP[face].height===1.2,'in-range-value-passes-through: '+UIMAP[face].height);
 typeHeight('');
 A(UIMAP[face].height===null,'blank-unsets-to-null: '+UIMAP[face].height);
 UIMAP[face]=save;buildUITable();
 });
// Language-dropdown gate (open with #langtest): the language list is sorted
// alphabetically with Elisp pinned as the default selection, and the ‹ › arrows
// step the selection (clamped, no wrap).
if(location.hash==='#langtest')gate('langtest',A=>{
 buildLangSel();
 const s=document.getElementById('langsel');
 const labels=[...s.options].map(o=>o.value);
 const sorted=[...labels].sort((a,b)=>a.localeCompare(b));
 A(JSON.stringify(labels)===JSON.stringify(sorted),'languages are alphabetical: '+labels.join(','));
 A(s.value==='Elisp','Elisp is the default selection: '+s.value);
 s.selectedIndex=0;stepLang(-1);
 A(s.selectedIndex===0,'prev clamps at the first language');
 stepLang(1);
 A(s.selectedIndex===1,'next steps forward one');
 s.selectedIndex=s.options.length-1;stepLang(1);
 A(s.selectedIndex===s.options.length-1,'next clamps at the last language');
 });
// View-lock-indicator gate (open with #viewlocktest): the view dropdown prefixes a
// lock glyph on a view whose every element is locked, and clears it otherwise.
if(location.hash==='#viewlocktest')gate('viewlocktest',A=>withSavedState(['LOCKED'],()=>{
 LOCKED.clear();updateViewLockIndicators();
 const s=document.getElementById('viewsel'),codeOpt=()=>[...s.options].find(o=>o.value==='@code');
 A(codeOpt()&&!codeOpt().textContent.startsWith('🔒'),'unlocked view shows no lock glyph: '+(codeOpt()&&codeOpt().textContent));
 syntaxLockKeys().forEach(k=>LOCKED.add(k));updateViewLockIndicators();
 A(codeOpt()&&codeOpt().textContent.startsWith('🔒'),'fully-locked view shows the lock glyph: '+(codeOpt()&&codeOpt().textContent));
 A(codeOpt()&&codeOpt().textContent.includes('color/code assignments'),'glyph prefixes the base label, not replaces it');
 LOCKED.delete(syntaxLockKeys()[0]);updateViewLockIndicators();
 A(codeOpt()&&!codeOpt().textContent.startsWith('🔒'),'unlocking one element clears the glyph');
 LOCKED.clear();updateViewLockIndicators();
 }));
// Detail-hover gate (open with #detailhovertest): every label in the expander
// detail row carries an explanatory hover, the way the table-header labels do.
if(location.hash==='#detailhovertest')gate('detailhovertest',A=>{
 buildUITable();
 const f=UI_FACES[0][0],detail=document.querySelector('#uibody tr.detailrow[data-detail-for="'+f+'"]');
 const fields=detail?[...detail.querySelectorAll('.detailfield')]:[];
 A(fields.length>0,'detail row has fields');
 A(fields.every(g=>g.title&&g.title.length>0),'every detail field has a hover: '+fields.map(g=>g.querySelector('span').textContent+(g.title?'+':'-')).join(' '));
 const inh=fields.find(g=>g.querySelector('span').textContent==='inherit');
 A(inh&&/inherit/i.test(inh.title),'inherit field hover mentions inheritance: '+(inh&&inh.title));
 });
// Expand/collapse-all gate (open with #expandalltest): the header toggle opens or
// closes every row's detail at once, the per-row triangles track state (▶ closed,
// ▼ open), and the header button's label follows the aggregate.
if(location.hash==='#expandalltest')gate('expandalltest',A=>{
 buildUITable();
 const tb=document.getElementById('uibody'),btn=document.getElementById('uiexpandall');
 const details=()=>[...tb.querySelectorAll('tr.detailrow')];
 const open=()=>details().filter(d=>d.style.display!=='none').length;
 const firstTog=()=>tb.querySelector('.exptoggle');
 A(firstTog()&&firstTog().textContent==='▶','row toggle starts collapsed (▶): '+(firstTog()&&firstTog().textContent));
 A(btn&&btn.textContent.indexOf('▶')===0&&/expand all/.test(btn.textContent),'button starts ▶ expand all: '+(btn&&btn.textContent));
 toggleAllExpanded('uiexpandall');
 A(open()===details().length&&open()>0,'expand all opens every row: '+open()+'/'+details().length);
 A(firstTog().textContent==='▼','row toggles flip to ▼ after expand all');
 A(btn.textContent.indexOf('▼')===0&&/collapse all/.test(btn.textContent),'button flips to ▼ collapse all: '+btn.textContent);
 toggleAllExpanded('uiexpandall');
 A(open()===0,'collapse all closes every row');
 A(firstTog().textContent==='▶','row toggles return to ▶ after collapse all');
 firstTog().click();
 A(open()===1,'a single row toggle opens just that row');
 A(btn.textContent.indexOf('▼')===0,'button reflects a single open row as ▼ collapse all');
 });
// Expander-persistence gate (open with #expandpersisttest): a package edit rebuilds
// the whole table, so an open expander must reopen instead of collapsing under the
// user. Editing a value inside the open expander must not close the row.
if(location.hash==='#expandpersisttest')gate('expandpersisttest',A=>withSavedState(['PKGMAP'],()=>{
 EXPANDED.clear();
 const app=curApp(),face=APPS[app].faces[0][0];buildPkgTable();
 const row=()=>document.querySelector('#pkgbody tr[data-face="'+face+'"]');
 const detail=()=>document.querySelector('#pkgbody tr.detailrow[data-detail-for="'+face+'"]');
 A(detail()&&detail().style.display==='none','expander starts collapsed');
 row().querySelector('.exptoggle').click();
 A(detail()&&detail().style.display!=='none','expander opens on toggle');
 const hin=detail().querySelector('.hstep');hin.value='1.4';hin.dispatchEvent(new Event('change'));
 A(detail()&&detail().style.display!=='none','expander stays open after an in-expander edit rebuilds the row');
 A(PKGMAP[app][face].height===1.4,'the in-expander edit still wrote the model');
 row().querySelector('.exptoggle').click();buildPkgTable();
 A(detail()&&detail().style.display==='none','a collapsed expander stays collapsed across a rebuild');
 EXPANDED.clear();buildPkgTable();
 }));
// Palette default-state gate (open with #paldefaulttest): the studio opens with
// the palette collapsed to base colors so the span tints don't crowd the first
// view. initApp() ran at page load, so the live toggle reflects the opening state.
if(location.hash==='#paldefaulttest')gate('paldefaulttest',A=>{
 const tg=document.getElementById('paltoggle');
 A(!!tg,'palette toggle present after boot');
 A(tg&&tg.textContent==='▶','palette opens collapsed to base colors (arrow shows right-pointing ▶)');
 });
// Palette display-toggle gate (open with #paltoggletest): the arrow control
// collapses each column to its base color and expands back to full spans.
if(location.hash==='#paltoggletest')gate('paltoggletest',A=>{
 const saveP=PALETTE.slice(),saveM=Object.assign({},MAP);
 paletteShowFull=true;  // start expanded so the first click collapses to base-only
 setSyntaxFg('bg','#101010');setSyntaxFg('p','#f0f0f0');
 PALETTE=[['#101010','bg','ground'],['#f0f0f0','fg','ground']];
 regenColumn('#67809c',2,{ground:groundPair()}).members.forEach(m=>PALETTE.push([m.hex,m.offset===0?'blue':'blue'+(m.offset>0?'+'+m.offset:m.offset),'blue']));
 renderPalette();
 const tg=document.getElementById('paltoggle');
 A(!!tg,'palette-toggle-present');
 const blueChips=()=>document.querySelectorAll('#pals .fstrip[data-column="blue"] .pchip').length;
 A(blueChips()===5,'full-mode-shows-base-and-spans');
 tg.click();
 A(blueChips()===1,'base-only-shows-just-the-base');
 A(document.getElementById('paltoggle').textContent==='▶','base-only-shows-right-arrow');
 document.getElementById('paltoggle').click();
 A(blueChips()===5,'toggling-back-restores-spans');
 PALETTE=saveP;for(const k in MAP)delete MAP[k];Object.assign(MAP,saveM);syncSyntaxFromCache();renderPalette();
 });
// Unused-tile gate (open with #unusedtest): a palette color referenced nowhere
// in the theme gets the .unused flag; a column with no used members gets
// .unused-col; referenced colors stay unflagged.
if(location.hash==='#unusedtest')gate('unusedtest',A=>{
 const saveP=PALETTE.slice(),saveM=Object.assign({},MAP),saveSyn=JSON.parse(JSON.stringify(SYNTAX)),saveU=JSON.parse(JSON.stringify(UIMAP));
 setSyntaxFg('bg','#101010');setSyntaxFg('p','#f0f0f0');
 PALETTE=[['#101010','bg','ground'],['#f0f0f0','fg','ground'],['#67809c','blue','blue'],['#123456','teal','teal']];
 for(const f in UIMAP)UIMAP[f]={fg:null,bg:null,weight:null,slant:null,underline:null,strike:null};
 setSyntaxFg('kw','#67809c');
 renderPalette();
 const tealStrip=document.querySelector('#pals .fstrip[data-column="teal"]');
 const blueStrip=document.querySelector('#pals .fstrip[data-column="blue"]');
 const tealChip=tealStrip&&tealStrip.querySelector('.pchip');
 const blueChip=blueStrip&&blueStrip.querySelector('.pchip');
 A(tealChip&&tealChip.classList.contains('unused'),'unreferenced-tile-flagged');
 A(blueChip&&!blueChip.classList.contains('unused'),'referenced-tile-not-flagged');
 A(tealStrip&&tealStrip.classList.contains('unused-col'),'all-unused-column-flagged');
 A(blueStrip&&!blueStrip.classList.contains('unused-col'),'used-column-not-flagged');
 PALETTE=saveP;for(const k in MAP)delete MAP[k];Object.assign(MAP,saveM);for(const k in SYNTAX)delete SYNTAX[k];Object.assign(SYNTAX,saveSyn);for(const f in UIMAP)delete UIMAP[f];Object.assign(UIMAP,saveU);syncSyntaxFromCache();renderPalette();
 });
// Gone-assignment gate (open with #gonetest): a swatch whose assigned color is
// no longer in the palette gets the .gone flag; an assignment to a present color
// does not.
if(location.hash==='#gonetest')gate('gonetest',A=>{
 const saveP=PALETTE.slice(),saveM=Object.assign({},MAP),saveU=JSON.parse(JSON.stringify(UIMAP));
 setSyntaxFg('bg','#101010');setSyntaxFg('p','#f0f0f0');
 PALETTE=[['#101010','bg','ground'],['#f0f0f0','fg','ground'],['#67809c','blue','blue']];
 UIMAP['region']={fg:null,bg:'#deadbe',weight:null,slant:null,underline:null,strike:null};
 UIMAP['highlight']={fg:null,bg:'#67809c',weight:null,slant:null,underline:null,strike:null};
 buildUITable();
 const goneDd=document.querySelector('#uibody tr[data-face="region"]').cells[3].querySelector('.cdd');
 const okDd=document.querySelector('#uibody tr[data-face="highlight"]').cells[3].querySelector('.cdd');
 A(goneDd&&goneDd.classList.contains('gone'),'assignment-to-missing-color-flagged');
 A(okDd&&!okDd.classList.contains('gone'),'assignment-to-present-color-not-flagged');
 PALETTE=saveP;for(const k in MAP)delete MAP[k];Object.assign(MAP,saveM);for(const f in UIMAP)delete UIMAP[f];Object.assign(UIMAP,saveU);syncSyntaxFromCache();buildUITable();
 });
// Tile-usage-hover gate (open with #usagetest): a tile's title lists the
// "view area > element" pairings that use its color, under the name/hex line.
if(location.hash==='#usagetest')gate('usagetest',A=>{
 const saveP=PALETTE.slice(),saveM=Object.assign({},MAP),saveU=JSON.parse(JSON.stringify(UIMAP));
 setSyntaxFg('bg','#101010');setSyntaxFg('p','#f0f0f0');
 PALETTE=[['#101010','bg','ground'],['#f0f0f0','fg','ground'],['#67809c','blue','blue']];
 const f0=UI_FACES[0][0],f0label=UI_FACES[0][1]||f0;
 for(const f in UIMAP)UIMAP[f]={fg:null,bg:null,weight:null,slant:null,underline:null,strike:null};
 UIMAP[f0]={fg:null,bg:'#67809c',weight:null,slant:null,underline:null,strike:null};
 renderPalette();
 const blueChip=document.querySelector('#pals .fstrip[data-column="blue"] .pchip');
 A(blueChip&&blueChip.title.includes('ui faces > '+f0label),'hover-title-lists-ui-face-usage');
 A(blueChip&&blueChip.title.split('\n').length>1,'usage-list-on-its-own-line-under-current-info');
 PALETTE=saveP;for(const k in MAP)delete MAP[k];Object.assign(MAP,saveM);for(const f in UIMAP)delete UIMAP[f];Object.assign(UIMAP,saveU);syncSyntaxFromCache();renderPalette();
 });
// Element-docstring hovers (open with #hovertest): each table's category cell
// carries the face's Emacs docstring on top of its prior hover text, and the
// existing label-span hints are left intact (added in addition, not replaced).
if(location.hash==='#hovertest')gate('hovertest',A=>{
 buildTable();buildUITable();buildPkgTable();
 const synCell=document.querySelector('#legbody tr[data-kind="kw"] .cat');
 A(synCell&&synCell.title===SYNTAX_DOCS['kw'],'syntax cat cell shows the category face docstring: '+(synCell&&synCell.title));
 const synLbl=document.querySelector('#legbody tr[data-kind="kw"] .cat span');
 A(synLbl&&synLbl.title==='flash this category in the code','syntax label-span hint left intact');
 const uiCell=document.querySelector('#uibody tr[data-face="mode-line"] .cat');
 A(uiCell&&uiCell.title===FACE_DOCS['mode-line'],'ui cat cell shows the face docstring: '+(uiCell&&uiCell.title));
 const app=curApp(),docFace=APPS[app].faces.map(r=>r[0]).find(f=>FACE_DOCS[f]);
 A(docFace,'a package face with a docstring exists to test');
 if(docFace){const pkgCell=document.querySelector('#pkgbody tr[data-face="'+docFace+'"] .cat');
   A(pkgCell&&pkgCell.title===FACE_DOCS[docFace]+'\n\n'+docFace,'package cat cell shows docstring on top of the face name: '+(pkgCell&&JSON.stringify(pkgCell.title)));}
 });
// Export via the File System Access API (open with #savetest): exportTheme writes
// the theme JSON straight to the picked file handle and closes it, so re-exporting
// overwrites in place instead of the browser uniquifying to "name (1).json".
if(location.hash==='#savetest'){(async()=>{let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
 let written='',closed=false,pickerArgs=null;
 const orig=window.showSaveFilePicker;
 window.showSaveFilePicker=async(opts)=>{pickerArgs=opts;return {name:'WIP.json',createWritable:async()=>({write:async d=>{written+=d;},close:async()=>{closed=true;}})};};
 try{
   await exportTheme();
   A(written===JSON.stringify(exportObj(),null,1),'export writes the theme JSON to the picked file');
   A(closed,'writable stream is closed so the file is committed');
   A(pickerArgs&&/\.json$/.test(pickerArgs.suggestedName||''),'picker suggests a .json name: '+(pickerArgs&&pickerArgs.suggestedName));
 }catch(e){A(false,'exportTheme threw: '+e.message);}
 finally{window.showSaveFilePicker=orig;}
 document.title='SAVETEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='savetest';d.textContent='SAVETEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);})();}
// Preview-locate registry gate (open with #locatetest): the cached LOCATE_REG is
// built over both data-face surfaces, keyed owner-qualified, and rebuilt (no stale
// entry) when an assignment changes. Grows across the locate phases.
if(location.hash==='#locatetest')gate('locatetest',A=>withSavedState(['PKGMAP','UIMAP','MAP'],()=>{
 const app=curApp(),pface=APPS[app].faces[0][0],uface=UI_FACES[0][0];
 rebuildLocateRegistry();
 const pkg=locateFaceMeta(app,pface,LOCATE_REG);
 A(pkg&&pkg.surface==='package'&&pkg.owner===app,'package face is a package-owned registry entry: '+(pkg&&pkg.owner));
 const ui=locateFaceMeta('@ui',uface,LOCATE_REG);
 A(ui&&ui.surface==='ui'&&ui.owner==='@ui','ui face is a @ui-owned registry entry: '+(ui&&ui.owner));
 // owner-qualified: a package face name under the @ui owner must not resolve to
 // the package entry (and vice versa) — the key carries the owner.
 A(locateFaceMeta('@ui',pface,LOCATE_REG).unassigned,'a package face under the @ui owner is unassigned, not collided');
 // rebuild-after-edit: a changed fg shows up only after the registry rebuilds.
 PKGMAP[app][pface].fg='#abcdef';PKGMAP[app][pface].source='user';
 rebuildLocateRegistry();
 A(locateFaceMeta(app,pface,LOCATE_REG).value.fg==='#abcdef','registry rebuild reflects the edited fg, no stale value');
 // Phase 2a: os delegates to previewSpan, which emits the locate attributes and
 // classes an on-pane (current-app) span.
 const box=document.createElement('div');box.innerHTML=os(app,pface,'x');
 const sp=box.querySelector('[data-face]');
 A(sp&&sp.dataset.ownerApp===app,'os span carries data-owner-app = the owning app: '+(sp&&sp.dataset.ownerApp));
 A(sp&&sp.dataset.face===pface,'os span keeps data-face');
 A(sp&&sp.classList.contains('locate-onpane'),'an on-pane (current-app) span gets the locate-onpane class');
 const other=Object.keys(APPS).find(k=>k!==app);
 if(other){const oface=APPS[other].faces[0][0],b2=document.createElement('div');b2.innerHTML=previewSpan(other,oface,'y');const s2=b2.querySelector('[data-face]');
  A(s2&&s2.dataset.ownerApp===other&&!s2.classList.contains('locate-onpane'),'an off-pane owner span carries its owner and no locate-onpane class');}
 // Phase 2c: previewSpan renders a @ui face off a package preview in its real
 // color (the cross-surface path), and marks it off-pane while a package is viewed.
 const ub=document.createElement('div');ub.innerHTML=previewSpan('@ui',uface,'z');
 const us=ub.querySelector('[data-face]');
 const uiFg=effFg(resolveUiAttr(uface,'fg',UIMAP));
 A(us&&us.dataset.ownerApp==='@ui'&&us.dataset.face===uface,'cross-surface @ui span carries owner @ui + data-face');
 A(us&&us.getAttribute('style').includes('color:'+uiFg),'cross-surface @ui span renders the ui face effective fg: '+(us&&us.getAttribute('style')));
 A(us&&!us.classList.contains('locate-onpane'),'a @ui span off a package preview is off-pane (no locate-onpane)');
 // Phase 2b: the owner-aware assertPreviewFaces accepts intentional off-pane and
 // @ui spans but rejects a bad owner.
 {const other2=Object.keys(APPS).find(k=>k!==app);
  const okHtml=os(app,pface,'a')+(other2?previewSpan(other2,APPS[other2].faces[0][0],'b'):'')+previewSpan('@ui',uface,'c');
  const probe=(h)=>{let fails=0;assertPreviewFaces((c)=>{if(!c)fails++;},h,APPS[app].faces,1,app,[]);return fails;};
  A(probe(okHtml)===0,'owner-aware validator accepts intentional off-pane + @ui spans');
  A(probe('<span data-owner-app="nope" data-face="'+pface+'">x</span>')>0,'owner-aware validator rejects a bad owner');}
}));
// Gate-only showcase fixture (open with #showcasetest): a synthetic host
// package-preview context renders one package-owned off-pane span and one @ui
// (minibuffer-prompt) off-pane span. Each appears in its owner's real color, is
// hover-only (no locate-onpane class), and passes the owner-aware validator. No
// user-facing preview changes -- the first real cross-owner preview (org-agenda or
// the completion preview) becomes the organic showcase later.
if(location.hash==='#showcasetest')gate('showcasetest',A=>withSavedState(['PKGMAP','UIMAP','MAP'],()=>{
 const host=curApp(),other=Object.keys(APPS).find(k=>k!==host);
 A(!!other,'a second package app exists to own an off-pane span');
 A(!!UIMAP['minibuffer-prompt'],'minibuffer-prompt is a real UI face');
 rebuildLocateRegistry();
 const oface=other&&APPS[other].faces[0][0];
 const fixture=os(host,APPS[host].faces[0][0],'host')
   +(other?previewSpan(other,oface,'pkg-offpane'):'')
   +previewSpan('@ui','minibuffer-prompt','prompt');
 const box=document.createElement('div');box.innerHTML=fixture;
 const pkgSpan=other&&box.querySelector('[data-owner-app="'+other+'"]'),uiSpan=box.querySelector('[data-owner-app="@ui"]');
 if(other){const want=(ofs(other,oface).match(/color:([^;]+)/)||[])[1];
  A(pkgSpan&&want&&pkgSpan.getAttribute('style').includes('color:'+want),'package-owned off-pane span renders its owner color: '+want);}
 const uiWant=effFg(resolveUiAttr('minibuffer-prompt','fg',UIMAP));
 A(uiSpan&&uiSpan.getAttribute('style').includes('color:'+uiWant),'@ui off-pane span renders the minibuffer-prompt color: '+uiWant);
 A(pkgSpan&&!pkgSpan.classList.contains('locate-onpane'),'package off-pane span is hover-only (no locate-onpane)');
 A(uiSpan&&!uiSpan.classList.contains('locate-onpane'),'@ui off-pane span is hover-only (no locate-onpane)');
 let fails=0;assertPreviewFaces((c)=>{if(!c)fails++;},fixture,APPS[host].faces,1,host,[]);
 A(fails===0,'the owner-aware validator passes the showcase fixture');
}));
// Hover gate (open with #locatehovertest): every previewSpan element carries the
// full locate title (effective value + source note), and hovering an element
// updates the preview-label info line to "section > face — value", restored on
// leave. The title is the deterministic fallback; the info line is the immediate
// surface.
if(location.hash==='#locatehovertest')gate('locatehovertest',A=>withSavedState(['PKGMAP','UIMAP','MAP'],()=>{
 const app=curApp(),face=APPS[app].faces[0][0];
 PKGMAP[app][face]={fg:'#123456',bg:null,inherit:null,source:'user'};
 rebuildLocateRegistry();
 const box=document.createElement('div');box.innerHTML=os(app,face,'x');
 const sp=box.querySelector('[data-face]');
 A(sp&&sp.getAttribute('title')===formatLocateTitle(locateFaceMeta(app,face,LOCATE_REG)),'span title equals formatLocateTitle: '+(sp&&sp.getAttribute('title')));
 A(sp&&/fg #123456 \(direct\)/.test(sp.getAttribute('title')),'direct-fg title shows the effective fg + direct note');
 PKGMAP[app][face]={fg:null,bg:null,inherit:null,source:'cleared'};
 rebuildLocateRegistry();
 const cb=document.createElement('div');cb.innerHTML=os(app,face,'x');
 A(/cleared, rendering as default/.test(cb.querySelector('[data-face]').getAttribute('title')),'cleared face title carries the cleared-rendering note');
 // Wayfinding is the per-span hover title (above); there is no separate info line.
}));
// Click + cursor gate (open with #locateclicktest): an on-pane element carries the
// locate-onpane class (pointer cursor) and clicking flashes its assignment row via
// the unified locateClick dispatcher; an off-pane element has no class (default
// cursor) and clicking flashes nothing. The UI mock's bare spans still flash their
// row through the same dispatcher (Phase 5 unification).
if(location.hash==='#locateclicktest')gate('locateclicktest',A=>withSavedState(['PKGMAP','UIMAP','MAP','LOCKED'],()=>{
 LOCKED.clear();
 const app=curApp(),face=APPS[app].faces[0][0];
 buildPkgTable();buildPkgPreview();rebuildLocateRegistry();
 const p=document.getElementById('pkgpreview');
 // on-pane: class present, click flashes the assignment row
 p.innerHTML=os(app,face,'click me');
 const onSpan=p.querySelector('[data-owner-app]');
 A(onSpan&&onSpan.classList.contains('locate-onpane'),'on-pane span carries the locate-onpane class (pointer cursor)');
 const prow=()=>document.querySelector('#pkgbody tr[data-face="'+face+'"]');
 if(prow())prow().classList.remove('flash');
 onSpan.dispatchEvent(new MouseEvent('click',{bubbles:true}));
 A(prow()&&prow().classList.contains('flash'),'clicking an on-pane span flashes its assignment row');
 // off-pane: no class, click flashes nothing
 const other=Object.keys(APPS).find(k=>k!==app);
 if(other){const oface=APPS[other].faces[0][0];
  p.innerHTML=previewSpan(other,oface,'off');
  const offSpan=p.querySelector('[data-owner-app]');
  A(offSpan&&!offSpan.classList.contains('locate-onpane'),'off-pane span has no locate-onpane class (default cursor)');
  [...document.querySelectorAll('#pkgbody tr')].forEach(tr=>tr.classList.remove('flash'));
  offSpan.dispatchEvent(new MouseEvent('click',{bubbles:true}));
  A(document.querySelectorAll('#pkgbody tr.flash').length===0,'clicking an off-pane span leaves all rows unflashed');}
 // Phase 5: the UI mock's bare data-face spans still flash their row via locateClick
 buildUITable();buildMockFrame();
 const mface=UI_FACES[0][0],mspan=document.querySelector('#mockframe [data-face="'+mface+'"]');
 if(mspan){const urow=()=>document.querySelector('#uibody tr[data-face="'+mface+'"]');
  if(urow())urow().classList.remove('flash');
  mspan.dispatchEvent(new MouseEvent('click',{bubbles:true}));
  A(urow()&&urow().classList.contains('flash'),'a UI mock span still flashes its row through the unified dispatcher');}
}));
// Embedded-font gate (open with #fonttest): the nerd-icons legend, dashboard
// navigator, and package previews render their glyphs in a real nerd font
// instead of tofu. Verifies (1) the ThemeStudioNerd @font-face is registered,
// (2) previewLines actually APPLIES that family — the div is parsed into the DOM
// and getComputedStyle must resolve to ThemeStudioNerd (a double-quoted family in
// the inline style attribute silently drops it, so a plain string match would
// false-pass), and (3) the embedded woff2 loads AND covers the glyph codepoints
// the previews use — both a BMP glyph (U+F121) and a supplementary-plane Material
// Design glyph (U+F0474), the range most likely missing from a partial font.
// Async: it awaits the font load, then appends the verdict (the runner's
// virtual-time budget covers it).
if(location.hash==='#fonttest'){
  const fam='ThemeStudioNerd',notes=[];
  const bmp='',supp='\u{f0474}';
  const finish=()=>{const v='FONTTEST '+(notes.length?'FAIL':'PASS');document.title=v;
    const d=document.createElement('div');d.id='fonttest';
    d.textContent=v+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);};
  const registered=[...document.fonts].some(f=>f.family.replace(/["']/g,'')===fam);
  if(!registered)notes.push('no-fontface');
  // Parse the actual previewLines output into the DOM and read the resolved
  // font-family off the rendered element — not a substring of the HTML string. A
  // double-quoted family name inside the inline style="..." attribute terminates
  // the attribute early and silently drops the font-family, so a string match
  // passes while the rendered font is empty; computed style catches that.
  const probe=document.createElement('div');probe.innerHTML=previewLines(['x']);
  document.body.appendChild(probe);
  const inner=probe.firstElementChild;
  const ff=inner?getComputedStyle(inner).fontFamily:'';
  if(ff.indexOf(fam)<0)notes.push('previews-font-not-applied('+(ff||'empty')+')');
  Promise.all([document.fonts.load('16px "'+fam+'"',bmp),document.fonts.load('16px "'+fam+'"',supp)]).then(()=>{
    if(!document.fonts.check('16px "'+fam+'"',bmp))notes.push('bmp-glyph-missing');
    if(!document.fonts.check('16px "'+fam+'"',supp))notes.push('supp-glyph-missing');
    probe.remove();finish();
  }).catch(e=>{probe.remove();notes.push('load-error:'+(e&&e.message||e));finish();});
}

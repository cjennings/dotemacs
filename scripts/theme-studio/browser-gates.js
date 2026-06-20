// Phase-1 self-test (open with #selftest): seed -> export -> import -> compare.
function pkgSelftest(){
  const seeded=seedPkgmap();
  seeded['org-mode']['org-level-2']={fg:'#e8bd30',bg:null,weight:null,slant:null,inherit:'org-level-1',height:1.2,source:'user'};
  const exp=packagesForExport(seeded);
  const round=seedPkgmap();mergePackagesInto(round,exp);
  const roundtrip=JSON.stringify(exp)===JSON.stringify(packagesForExport(round));
  let oldjson=true;try{const m=seedPkgmap();mergePackagesInto(m,undefined);oldjson=!!(m['org-mode']&&m['org-mode']['org-todo'].source==='default');}catch(e){oldjson=false;}
  const l2=exp['org-mode']['org-level-2'];
  const inherited=l2.inherit==='org-level-1'&&l2.source==='user';
  const height=l2.height===1.2 && !('height' in (exp['org-mode']['org-todo']));
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
if(location.hash==='#locktest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='LOCKTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='locktest';d.textContent='LOCKTEST '+(ok?'PASS':'FAIL')+(notes.length?' | '+notes.join(' ; '):'');document.body.appendChild(d);}
// Sort gate (open with #sorttest): all three tables now share srtTable/cellVal.
// Verifies the syntax table (which used to have its own srt) sorts by color
// value and by element name, that a repeat click reverses, and that the UI and
// package tables still sort. Guards the unified sort for the later stages.
if(location.hash==='#sorttest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
 const ddVals=tb=>[...document.querySelectorAll('#'+tb+' tr:not(.detailrow)')].map(tr=>{const dd=tr.cells[2].querySelector('.cdd');return dd?(dd.dataset.val||''):'';});
 const txtVals=tb=>[...document.querySelectorAll('#'+tb+' tr:not(.detailrow)')].map(tr=>tr.cells[0].innerText.trim().toLowerCase());
 const asc=a=>a.every((v,i)=>i===0||a[i-1]<=v),desc=a=>a.every((v,i)=>i===0||a[i-1]>=v);
 buildTable();
 srtTable('legbody',2);A(asc(ddVals('legbody')),'legbody-color-asc');
 srtTable('legbody',2);A(desc(ddVals('legbody')),'legbody-color-desc');
 srtTable('legbody',0);A(asc(txtVals('legbody')),'legbody-elements-asc');
 buildUITable();srtTable('uibody',0);A(asc(txtVals('uibody')),'uibody-face-asc');
 buildPkgTable();srtTable('pkgbody',2);A(asc(ddVals('pkgbody')),'pkgbody-fg-asc');
 document.title='SORTTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='sorttest';d.textContent='SORTTEST '+(ok?'PASS':'FAIL')+(notes.length?' | '+notes.join(' ; '):'');document.body.appendChild(d);}
// Live-buffer rendering gate (open with #mocktest): pins the face-faithfulness
// fixes so they cannot silently regress — overlay faces keep syntax colors and
// honor their styles, the cursor sits on a glyph, line numbers honor weight, the
// fringe shows its foreground indicator, and the mode-line carries its box.
if(location.hash==='#mocktest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 [['#legbody tr[data-kind="kw"]',7],['#uibody tr[data-face="mode-line"]',7],['#pkgbody tr',6]].forEach(([sel,idx])=>{
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
 document.title='MOCKTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='mocktest';d.textContent='MOCKTEST '+(ok?'PASS':'FAIL')+(notes.length?' | '+notes.join(' ; '):'');document.body.appendChild(d);}
// Palette-generator gate (open with #generatortest): previewing is non-mutating,
// clicking a generated tile loads the existing selector, adding creates a normal
// singleton base column, and appending a preview column commits all span members
// under one stable column id.
if(location.hash==='#generatortest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='GENERATORTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='generatortest';d.textContent='GENERATORTEST '+(ok?'PASS':'FAIL')+(notes.length?' | '+notes.join(' ; '):'');document.body.appendChild(d);}
// Auto-dim gate (open with #autodimtest): the bespoke split preview shows the
// selected language in both panes -- the left in real syntax colors, the right
// collapsed to the single auto-dim-other-buffers face -- and tracks the langsel.
if(location.hash==='#autodimtest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='AUTODIMTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='autodimtest';d.textContent='AUTODIMTEST '+(ok?'PASS':'FAIL')+(notes.length?' | '+notes.join(' ; '):'');document.body.appendChild(d);}
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
if(location.hash==='#contrasttest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 const prow=document.querySelector('#pkgbody tr[data-face="'+tFace+'"]'),pcell=prow&&prow.children[5];
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
 document.title='CONTRASTTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='contrasttest';d.textContent='CONTRASTTEST '+(ok?'PASS':'FAIL')+(notes.length?' | '+notes.join(' ; '):'');document.body.appendChild(d);}
// Bevel gate (open with #beveltest): released/pressed boxes derive their
// highlight and shadow from the face's effective bg per Emacs's relief
// algorithm, and pressed draws the shadow edge first.
if(location.hash==='#beveltest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 const mlrow=document.querySelector('#uibody tr[data-face="mode-line"]'),boxCell=mlrow&&mlrow.cells[7],lineBtn=boxCell&&boxCell.querySelector('.boxbtn[data-style="line"]'),boxDd=boxCell&&boxCell.querySelector('.cdd');
 if(lineBtn&&boxDd){lineBtn.click();boxDd.click();const redRow=[...document.querySelectorAll('.cddpop .cddgc')].find(c=>(c.dataset.name||'').includes('red'));if(redRow)redRow.click();}
 A(UIMAP['mode-line'].box&&UIMAP['mode-line'].box.color==='#ff0000','UI box color dropdown writes box.color');
 const app=curApp(),face=APPS[app].faces[0][0];PKGMAP[app][face].box={style:'line',width:1,color:null};buildPkgTable();
 const prow=document.querySelector('#pkgbody tr[data-face="'+face+'"]'),pbox=prow&&prow.cells[6],pdd=pbox&&pbox.querySelector('.cdd');
 if(pdd){pdd.click();const redRow=[...document.querySelectorAll('.cddpop .cddgc')].find(c=>(c.dataset.name||'').includes('red'));if(redRow)redRow.click();}
 A(PKGMAP[app][face].box&&PKGMAP[app][face].box.color==='#ff0000','package box color dropdown writes box.color');
 PALETTE=saveP;PKGMAP=savePK;for(const f in UIMAP)delete UIMAP[f];Object.assign(UIMAP,saveUI);buildUITable();buildPkgTable();
 document.title='BEVELTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='beveltest';d.textContent='BEVELTEST '+(ok?'PASS':'FAIL')+(notes.length?' | '+notes.join(' ; '):'');document.body.appendChild(d);}
// Gallery gate (open with #gallerytest): the color dropdown opens a 2D grid in
// the palette-panel shape. Driven on a throwaway dropdown so no real face state
// is mutated. Covers: grid opens, every palette color has a cell, a cell click
// fires onPick + updates the trigger, the pick highlights on reopen, the default
// chip clears.
if(location.hash==='#gallerytest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='GALLERYTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='gallerytest';d.textContent='GALLERYTEST '+(ok?'PASS':'FAIL')+(notes.length?' | '+notes.join(' ; '):'');document.body.appendChild(d);}
// Preview-link gate (open with #previewlinktest): known bespoke-preview face
// mappings stay wired to the face that Emacs actually uses.
if(location.hash==='#previewlinktest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='PREVIEWLINKTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='previewlinktest';d.textContent='PREVIEWLINKTEST '+(ok?'PASS':'FAIL')+(notes.length?' | '+notes.join(' ; '):'');document.body.appendChild(d);}
// Safe-lightness gate (open with #safetest): the OKLCH picker shades the unsafe
// lightness band for a selected covered face and hides it when no face is selected.
if(location.hash==='#safetest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='SAFETEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='safetest';d.textContent='SAFETEST '+(ok?'PASS':'FAIL')+(notes.length?' | '+notes.join(' ; '):'');document.body.appendChild(d);}
// Gone-rebind gate (open with #healtest): deleting a named color then recreating
// the name re-points face references stranded on the old hex to the new color.
if(location.hash==='#healtest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='HEALTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='healtest';d.textContent='HEALTEST '+(ok?'PASS':'FAIL')+(notes.length?' | '+notes.join(' ; '):'');document.body.appendChild(d);}
// Column-strip gate (open with #columntest): the palette renders as a pinned
// ground column plus structural columns, chips keep their controls, and renaming
// a color leaves it in the same strip because the column id is stable.
if(location.hash==='#columntest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='COLUMNTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='columntest';d.textContent='COLUMNTEST '+(ok?'PASS':'FAIL')+(notes.length?' | '+notes.join(' ; '):'');document.body.appendChild(d);}
// Count-control gate (open with #counttest): the per-column count regenerates the
// column — count up adds symmetric steps, count down drops the extremes, a
// reference to a surviving step follows the new hex, a reference to a removed step
// is left on its old (now-gone) hex.
if(location.hash==='#counttest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='COUNTTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='counttest';d.textContent='COUNTTEST '+(ok?'PASS':'FAIL')+(notes.length?' | '+notes.join(' ; '):'');document.body.appendChild(d);}
// Base-edit + ground-edit gate (open with #baseedittest): editing a column base
// recolors the whole column at the same count and references follow; editing a
// ground swatch writes the bg/fg assignment.
if(location.hash==='#baseedittest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='BASEEDITTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='baseedittest';d.textContent='BASEEDITTEST '+(ok?'PASS':'FAIL')+(notes.length?' | '+notes.join(' ; '):'');document.body.appendChild(d);}
// Round-trip gate (open with #roundtriptest): export stays a flat palette with
// stable column ids, and import does not need color-derived column reconstruction.
if(location.hash==='#roundtriptest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='ROUNDTRIPTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='roundtriptest';d.textContent='ROUNDTRIPTEST '+(ok?'PASS':'FAIL')+(notes.length?' | '+notes.join(' ; '):'');document.body.appendChild(d);}
// View-selector gate (open with #viewtest): the assignment panel is driven by a
// single #viewsel dropdown -- two editor entries (@code, @ui) then a "package
// faces" optgroup of every app, alphabetically by label -- and switching it
// shows exactly one of the three view blocks.
if(location.hash==='#viewtest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='VIEWTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='viewtest';d.textContent='VIEWTEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);}
// Non-default-marker gate (open with #ndtest): a per-face setting cell gets the
// .nd corner flag only when its value differs from the face's seed default. Cell
// order in a pkg row: 0 label, 1 lock, 2 fg, 3 bg, 4 style, 5 contrast, 6 box.
// inherit + height live in the row expander, so a non-default height flags the
// expander toggle (exp-nd) rather than an inline cell.
if(location.hash==='#ndtest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='NDTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='ndtest';d.textContent='NDTEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);}
// Contrast-cell gate (open with #crtest): the per-face contrast column shows a
// bare colored number (no PASS/FAIL word); the WCAG verdict lives in the hover.
if(location.hash==='#crtest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
 const app=curApp(),face=APPS[app].faces[0][0];buildPkgTable();
 const cell=document.querySelector('#pkgbody tr[data-face="'+face+'"]').cells[5];
 const span=cell&&cell.querySelector('span');
 A(span&&/^\d+\.\d$/.test(span.textContent.trim()),'contrast cell is a bare number: '+(span&&span.textContent));
 A(span&&!/PASS|FAIL/.test(span.textContent),'no PASS/FAIL word in the contrast cell');
 A(span&&span.title&&/(passes|fails) WCAG/i.test(span.title),'contrast cell carries a WCAG hover: '+(span&&span.title));
 document.title='CRTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='crtest';d.textContent='CRTEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);}
// View-nav gate (open with #navtest): the prev/next arrows flanking the view
// dropdown step the selection (clamped, no wrap) and re-render the view.
if(location.hash==='#navtest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='NAVTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='navtest';d.textContent='NAVTEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);}
// Markdown-preview gate (open with #mdtest): markdown-mode has a dedicated README
// renderer, and every data-face it emits is a real markdown-mode face.
if(location.hash==='#mdtest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
 A(APPS['markdown-mode']&&APPS['markdown-mode'].preview==='markdown','markdown-mode wired to the markdown preview');
 A(!!PACKAGE_PREVIEWS['markdown'],'markdown renderer registered');
 if(PACKAGE_PREVIEWS['markdown']&&APPS['markdown-mode']){
  const box=document.createElement('div');box.innerHTML=PACKAGE_PREVIEWS['markdown']();
  const valid=new Set(APPS['markdown-mode'].faces.map(r=>r[0]));
  const used=[...box.querySelectorAll('[data-face]')].map(e=>e.dataset.face);
  A(used.length>=15,'preview exercises many faces ('+used.length+')');
  const bad=used.filter(f=>!valid.has(f));
  A(bad.length===0,'every data-face is a real markdown face; bad='+bad.join(','));
  for(const f of ['markdown-header-face-1','markdown-bold-face','markdown-inline-code-face','markdown-blockquote-face','markdown-gfm-checkbox-face','markdown-table-face'])
   A(used.includes(f),'preview includes '+f);
 }
 document.title='MDTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='mdtest';d.textContent='MDTEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);}
// mu4e-preview gate (open with #mupreviewtest): the mu4e preview is a realistic
// headers list + message view, and every data-face it emits is a real mu4e face.
if(location.hash==='#mupreviewtest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
 const box=document.createElement('div');box.innerHTML=renderMu4ePreview();
 const valid=new Set((APPS['mu4e']&&APPS['mu4e'].faces||[]).map(r=>r[0]));
 const used=[...box.querySelectorAll('[data-face]')].map(e=>e.dataset.face);
 A(used.length>=20,'preview exercises many faces ('+used.length+')');
 const bad=used.filter(f=>!valid.has(f));
 A(bad.length===0,'every data-face is a real mu4e face; bad='+bad.join(','));
 for(const f of ['mu4e-unread-face','mu4e-flagged-face','mu4e-replied-face','mu4e-draft-face','mu4e-trashed-face','mu4e-header-highlight-face','mu4e-header-marks-face','mu4e-contact-face','mu4e-compose-separator-face'])
  A(used.includes(f),'preview includes '+f);
 document.title='MUPREVIEWTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='mupreviewtest';d.textContent='MUPREVIEWTEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);}
// gnus-preview gate (open with #gnustest): gnus is its own view package (it drives
// the mu4e article view), and every data-face its preview emits is a real gnus face.
if(location.hash==='#gnustest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
 A(!!APPS['gnus'],'gnus is a registered view package');
 A(APPS['gnus']&&APPS['gnus'].preview==='gnus','gnus uses the gnus preview renderer');
 const box=document.createElement('div');box.innerHTML=renderGnusPreview();
 const valid=new Set((APPS['gnus']&&APPS['gnus'].faces||[]).map(r=>r[0]));
 const used=[...box.querySelectorAll('[data-face]')].map(e=>e.dataset.face);
 A(used.length>=20,'preview exercises many faces ('+used.length+')');
 const bad=used.filter(f=>!valid.has(f));
 A(bad.length===0,'every data-face is a real gnus face; bad='+bad.join(','));
 for(const f of ['gnus-header-name','gnus-header-from','gnus-header-subject','gnus-cite-1','gnus-cite-attribution','gnus-signature','gnus-button','gnus-emphasis-highlight-words'])
  A(used.includes(f),'preview includes '+f);
 document.title='GNUSTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='gnustest';d.textContent='GNUSTEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);}
// picker-distinct gate (open with #pickertest): the color picker panel must stand
// out from the page background. It carries a highlighted gold accent border, and its
// background is meaningfully lighter than the body so the two are easy to tell apart.
if(location.hash==='#pickertest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
 const pk=document.getElementById('picker');A(!!pk,'picker element exists');
 if(pk){const cs=getComputedStyle(pk),body=getComputedStyle(document.body);
  const bc=(cs.borderTopColor.match(/\d+/g)||[]).slice(0,3).map(Number);
  const pkbg=(cs.backgroundColor.match(/\d+/g)||[]).slice(0,3).map(Number);
  const bdbg=(body.backgroundColor.match(/\d+/g)||[]).slice(0,3).map(Number);
  A(bc.join(',')==='232,189,48','picker carries the gold accent border (got '+cs.borderTopColor+')');
  const lift=pkbg.map((c,i)=>c-bdbg[i]);
  A(lift.every(d=>d>=12),'picker background is clearly lighter than the page (per-channel lift '+lift.join(',')+')');
 }
 document.title='PICKERTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='pickertest';d.textContent='PICKERTEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);}
// Box-cluster gate (open with #boxtest): the box control is a 2x2 cluster of
// four radio buttons (none / line / pressed / raised); the color swatch shows
// only while a box style is active.
if(location.hash==='#boxtest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
 LOCKED.clear();const f=UI_FACES[0][0];const saveBox=UIMAP[f].box;
 UIMAP[f].box=null;buildUITable();
 const cell=document.querySelector('#uibody tr[data-face="'+f+'"]').cells[7];
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
 document.title='BOXTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='boxtest';d.textContent='BOXTEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);}
// Style-cluster gate (open with #styletest): the style cell holds a weight
// selector, a slant selector, and box-like underline and strike controls.
if(location.hash==='#styletest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
 buildUITable();const f=UI_FACES[0][0];
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
 document.title='STYLETEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='styletest';d.textContent='STYLETEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);}
// Expander gate (open with #expandtest): the per-row "more" toggle reveals a
// detail row with the overflow attribute editor, and its controls write the model.
if(location.hash==='#expandtest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='EXPANDTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='expandtest';d.textContent='EXPANDTEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);}
// Height-clamp gate (open with #heighttest): the expander height field coerces a
// typed value into [HEIGHT_MIN,HEIGHT_MAX] and writes the clamped number back, so
// an out-of-range type/paste can't reach the model. Guards the fact that an
// <input type=number> min/max only constrain its steppers, never typed text.
if(location.hash==='#heighttest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='HEIGHTTEST '+(ok?'PASS':'FAIL');
 const hd=document.createElement('div');hd.id='heighttest';hd.textContent='HEIGHTTEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(hd);}
// Language-dropdown gate (open with #langtest): the language list is sorted
// alphabetically with Elisp pinned as the default selection, and the ‹ › arrows
// step the selection (clamped, no wrap).
if(location.hash==='#langtest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='LANGTEST '+(ok?'PASS':'FAIL');
 const ld=document.createElement('div');ld.id='langtest';ld.textContent='LANGTEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(ld);}
// Palette default-state gate (open with #paldefaulttest): the studio opens with
// the palette collapsed to base colors so the span tints don't crowd the first
// view. initApp() ran at page load, so the live toggle reflects the opening state.
if(location.hash==='#paldefaulttest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
 const tg=document.getElementById('paltoggle');
 A(!!tg,'palette toggle present after boot');
 A(tg&&tg.textContent==='▶','palette opens collapsed to base colors (arrow shows right-pointing ▶)');
 document.title='PALDEFAULTTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='paldefaulttest';d.textContent='PALDEFAULTTEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);}
// Palette display-toggle gate (open with #paltoggletest): the arrow control
// collapses each column to its base color and expands back to full spans.
if(location.hash==='#paltoggletest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='PALTOGGLETEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='paltoggletest';d.textContent='PALTOGGLETEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);}
// Unused-tile gate (open with #unusedtest): a palette color referenced nowhere
// in the theme gets the .unused flag; a column with no used members gets
// .unused-col; referenced colors stay unflagged.
if(location.hash==='#unusedtest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='UNUSEDTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='unusedtest';d.textContent='UNUSEDTEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);}
// Gone-assignment gate (open with #gonetest): a swatch whose assigned color is
// no longer in the palette gets the .gone flag; an assignment to a present color
// does not.
if(location.hash==='#gonetest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='GONETEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='gonetest';d.textContent='GONETEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);}
// Tile-usage-hover gate (open with #usagetest): a tile's title lists the
// "view area > element" pairings that use its color, under the name/hex line.
if(location.hash==='#usagetest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='USAGETEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='usagetest';d.textContent='USAGETEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);}
// Element-docstring hovers (open with #hovertest): each table's category cell
// carries the face's Emacs docstring on top of its prior hover text, and the
// existing label-span hints are left intact (added in addition, not replaced).
if(location.hash==='#hovertest'){let ok=true;const notes=[];const A=(c,n)=>{if(!c){ok=false;notes.push(n);}};
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
 document.title='HOVERTEST '+(ok?'PASS':'FAIL');
 const d=document.createElement('div');d.id='hovertest';d.textContent='HOVERTEST '+(ok?'PASS':'FAIL')+(notes.length?' fails='+notes.join(','):'');document.body.appendChild(d);}
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

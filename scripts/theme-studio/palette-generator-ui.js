// Browser-side palette-generator panel. The pure planner lives in
// palette-generator-core.js; this file only gathers controls, renders previews,
// and commits selected generated colors into normal palette entries.
let GEN_PROPOSAL=null, GEN_SELECTION=null;
const GENERATOR_CONTROLS={
  genintent:{
    labelTitle:'what kind of candidate colors to look for',
    options:[
      ['random','random','Pure exploration: reroll unrelated candidate base colors.'],
      ['near-palette','near palette','Generate candidates near the current palette base colors.'],
      ['fill-gaps','fill gaps','Find missing perceptual colors using OKLab distance.'],
      ['fill-hue-gaps','fill hue gaps','Find missing perceptual colors while rewarding underrepresented hue regions.'],
      ['complements','complements','Generate colors opposite palette or selected anchors.'],
      ['bridges','bridges','Generate colors between existing palette anchors.'],
      ['near-selected','near selected','Generate candidates near the current selector color.'],
      ['complementary','complementary','Use the hue opposite the anchor.'],
      ['analogous','analogous','Use neighboring hues around the anchor.'],
      ['split-complementary','split complementary','Use hues on both sides of the anchor complement.'],
      ['triadic','triadic','Use three hues spaced 120 degrees apart.'],
      ['tetradic','tetradic','Use two complementary hue pairs.'],
      ['square','square','Use four hues spaced 90 degrees apart.'],
      ['monochromatic','monochromatic','Stay near one hue and vary color character.'],
      ['rainbow','rainbow','Spread candidates evenly around the full hue wheel.'],
    ],
  },
  genvibe:{
    labelTitle:'the character of generated candidate colors',
    options:[
      ['bold','bold','Higher chroma, assertive candidate colors.'],
      ['balanced','balanced','Moderate chroma candidate colors.'],
      ['muted','muted','Lower chroma, quieter candidate colors.'],
      ['pastel','pastel','Light, low-chroma candidate colors.'],
      ['deep','deep','Lower-lightness, richer candidate colors.'],
      ['jewel','jewel','Saturated, rich candidate colors.'],
      ['earthy','earthy','Warmer, reduced-chroma earth-tone candidates.'],
      ['neon','neon','Very high-chroma candidate colors.'],
      ['strange','strange','More unusual, high-variance candidate colors.'],
      ['warm','warm','Bias candidates toward red, orange, and yellow.'],
      ['cool','cool','Bias candidates toward green, cyan, blue, and violet.'],
    ],
  },
  gensource:{
    labelTitle:'where starting hues come from',
    options:[
      ['palette','palette','Use current base color columns as anchors; span tiles are ignored.'],
      ['none','none','Use no anchors; useful for pure random exploration.'],
      ['bg-fg','bg/fg','Use the current background and foreground as anchors.'],
      ['selected','selected','Use the current selector tile as the anchor.'],
    ],
  },
  gencontrast:{
    labelTitle:'minimum contrast against the current bg',
    options:[
      ['aa','AA','Require WCAG AA contrast against the current background.'],
      ['aaa','AAA','Require WCAG AAA contrast against the current background.'],
      ['none','none','Do not reject candidates by WCAG contrast.'],
    ],
  },
};
function generatorOptionTitle(id,value){
  const ctl=GENERATOR_CONTROLS[id];
  const row=ctl&&ctl.options.find(o=>o[0]===value);
  return row?row[2]:'';
}
function populateGeneratorSelects(){
  Object.entries(GENERATOR_CONTROLS).forEach(([id,ctl])=>{
    const el=document.getElementById(id);if(!el)return;
    const cur=el.value||el.dataset.default||ctl.options[0][0];
    el.innerHTML='';
    ctl.options.forEach(([value,label])=>{const o=document.createElement('option');o.value=value;o.textContent=label;el.appendChild(o);});
    el.value=ctl.options.some(o=>o[0]===cur)?cur:ctl.options[0][0];
    const label=el.closest('label');if(label)label.title=ctl.labelTitle;
  });
}
function genConfig(){
  const intent=document.getElementById('genintent'),vibe=document.getElementById('genvibe'),
    source=document.getElementById('gensource'),
    accents=document.getElementById('genaccents'),contrastSel=document.getElementById('gencontrast');
  return {
    intent:intent?intent.value:'random',
    vibe:vibe?vibe.value:'bold',
    sourceMode:source?source.value:'palette',
    scheme:'random',
    baseHue:250,
    accentCount:accents?parseInt(accents.value,10):5,
    spanCount:0,
    contrastMode:contrastSel?contrastSel.value:'aa',
    selectedHex:curHex(),
    colorNames:COLOR_NAMES,
  };
}
function syncGeneratorControls(){syncGeneratorSelectTitles();}
function syncGeneratorSelectTitles(){
  Object.keys(GENERATOR_CONTROLS).forEach(id=>{const el=document.getElementById(id);if(el)el.title=generatorOptionTitle(id,el.value);});
}
function setGenMessage(msg,err){const m=document.getElementById('genmsg');if(!m)return;m.textContent=msg||'';m.style.color=err?'#cb6b4d':'#8a9496';}
function renderGeneratorPreview(){
  const host=document.getElementById('genpreview');if(!host)return;host.innerHTML='';
  if(!GEN_PROPOSAL){setGenMessage('',false);return;}
  GEN_PROPOSAL.columns.forEach((col,ci)=>{
    const strip=document.createElement('div');strip.className='gencol';
    const head=document.createElement('div');head.className='genhead';
    head.innerHTML=`<span title="${esc(col.name)}">${esc(col.name)}</span><button class="genappend" data-col="${ci}" title="append this generated column to the palette">+</button>`;
    head.querySelector('.genappend').onclick=()=>appendGeneratedColumn(ci);
    strip.appendChild(head);
    col.members.forEach((m,mi)=>{
      const chip=document.createElement('div'),tc=textOn(m.hex);
      chip.className='genchip'+(GEN_SELECTION&&GEN_SELECTION.hex===m.hex&&GEN_SELECTION.name===m.name?' sel':'');
      chip.dataset.col=String(ci);chip.dataset.member=String(mi);chip.dataset.hex=m.hex;chip.dataset.name=m.name;
      chip.style.background=m.hex;chip.style.color=tc;chip.title=m.name+' '+m.hex+(m.clamped?' (sRGB clamped)':'');
      chip.innerHTML=`<div class="gn">${esc(m.name.replace(/-/g,' '))}</div><div class="gh">${m.hex}</div>`;
      chip.onclick=()=>selectGeneratedTile(ci,mi);
      strip.appendChild(chip);
    });
    host.appendChild(strip);
  });
  const s=GEN_PROPOSAL.summary;
  setGenMessage(s.generated+' column(s) previewed'+(s.rejected?(', '+s.rejected+' rejected'):'')+(s.minContrast?(', min '+s.minContrast.toFixed(1)+':1'):''),false);
}
function resetGeneratorPreviewState(){
  GEN_PROPOSAL=null;GEN_SELECTION=null;
  renderGeneratorPreview();
}
function previewGenerator(){
  const cfg=genConfig();
  resetGeneratorPreviewState();
  GEN_PROPOSAL=planPaletteGenerator(PALETTE,{bg:MAP['bg'],fg:MAP['p']},cfg);
  renderGeneratorPreview();
}
function clearGeneratorPreview(){resetGeneratorPreviewState();}
function selectGeneratedTile(ci,mi){
  if(!GEN_PROPOSAL||!GEN_PROPOSAL.columns[ci])return;
  const m=GEN_PROPOSAL.columns[ci].members[mi];if(!m)return;
  selectedIdx=null;GEN_SELECTION={column:ci,member:mi,hex:m.hex,name:m.name};
  setHex(m.hex);document.getElementById('newname').value=m.name;
  renderPalette();renderGeneratorPreview();
  notify('loaded generated "'+m.name+'" into the selector - add it to commit',false);
}
function appendGeneratedColumn(ci){
  if(!GEN_PROPOSAL||!GEN_PROPOSAL.columns[ci])return;
  const colName=GEN_PROPOSAL.columns[ci].name, entries=entriesForGeneratedColumn(GEN_PROPOSAL.columns[ci]);
  const existing=new Set(PALETTE.map(p=>(p[1]||'').toLowerCase()));
  if(entries.some(e=>existing.has(e[1].toLowerCase()))){notify('generated names already exist - preview again for fresh names',true);return;}
  PALETTE.push(...entries);GEN_SELECTION=null;selectedIdx=null;
  refreshPaletteState();previewGenerator();
  notify('added generated column "'+colName+'"',false);
}
function initGeneratorControls(){
  populateGeneratorSelects();
  Object.keys(GENERATOR_CONTROLS).forEach(id=>{const el=document.getElementById(id);if(el)el.onchange=syncGeneratorControls;});
  syncGeneratorControls();
}

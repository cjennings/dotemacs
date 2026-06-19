// Pure palette-generator planner. It depends on the shared palette-column model
// from app-core.js, but owns candidate hue selection, naming, contrast filtering,
// and conversion from preview columns to palette entries.
import { normHex } from './app-util.js';
import { oklch2hex, contrast, deltaE, oklchOf, isPureEndpointHex } from './colormath.js';
import { columnsFromPalette } from './app-core.js';

function generatedExistingNames(palette){
  return new Set((palette||[]).map(p=>(p&&p[1]||'').toLowerCase()).filter(Boolean));
}
const DEFAULT_COLOR_NAMES=[['black','#000000'],['white','#ffffff'],['red','#ff0000'],['green','#008000'],['blue','#0000ff'],['yellow','#ffff00'],['cyan','#00ffff'],['magenta','#ff00ff'],['gray','#808080']];
function nearestColorName(hex,colorNames){
  const h=typeof hex==='string'?normHex(hex):null;
  if(!h)return 'generated';
  let best=(colorNames&&colorNames[0]&&colorNames[0][0])||DEFAULT_COLOR_NAMES[0][0],bd=Infinity;
  for(const [name,nhex] of (colorNames&&colorNames.length?colorNames:DEFAULT_COLOR_NAMES)){const d=deltaE(h,nhex);if(d<bd){bd=d;best=name;}}
  return best;
}
function uniqueGeneratedName(base,used){
  let name=base||'generated',i=2;
  if(!used.has(name.toLowerCase())){used.add(name.toLowerCase());return name;}
  while(used.has((name+'-alt'+i).toLowerCase()))i++;
  const out=name+'-alt'+i;used.add(out.toLowerCase());return out;
}
function hueOfHex(hex,fallback){
  const h=typeof hex==='string'?normHex(hex):null;
  if(!h)return fallback;
  const lch=oklchOf(h);
  return Number.isFinite(lch.H)?lch.H:fallback;
}
function generatorSourceHue(palette,ground,cfg){
  const fallback=((cfg&&typeof cfg.baseHue==='number'&&isFinite(cfg.baseHue))?cfg.baseHue:250)%360;
  if(cfg&&cfg.sourceMode==='palette'){const hs=paletteBaseHues(palette,ground);return hs.length?hs[0]:(fallback+360)%360;}
  if(cfg&&cfg.sourceMode==='selected'&&typeof cfg.selectedHex==='string'&&normHex(cfg.selectedHex))return hueOfHex(cfg.selectedHex,fallback);
  const bg=hueOfHex(ground&&ground.bg,fallback),fg=hueOfHex(ground&&ground.fg,fallback);
  if(Math.abs(bg-fallback)>0.001||Math.abs(fg-fallback)>0.001)return ((bg+fg)/2+360)%360;
  return (fallback+360)%360;
}
function generatorHues(baseHue,scheme,count,rng){
  const n=Math.max(1,Math.min(12,Math.round(count||8))), b=((baseHue%360)+360)%360;
  if(scheme==='random'){
    const rnd=typeof rng==='function'?rng:Math.random;
    return Array.from({length:n},()=>Math.floor(rnd()*360));
  }
  if(scheme==='analogous'){
    const spread=Math.min(120,Math.max(30,n*14)), start=b-spread/2;
    return Array.from({length:n},(_,i)=>(start+(n===1?0:(spread*i)/(n-1))+360)%360);
  }
  if(scheme==='triadic'){
    const offsets=[0,120,240,30,150,270,60,180,300,90,210,330];
    return offsets.slice(0,n).map(o=>(b+o)%360);
  }
  if(scheme==='manual')return Array.from({length:n},(_,i)=>(b+(i*360)/n)%360);
  return Array.from({length:n},(_,i)=>(b+(i*360)/n)%360);
}
function generatorChroma(mode){
  return mode==='subdued'?0.055:mode==='vivid'?0.13:0.085;
}
function generatorTarget(mode){return mode==='aaa'?7:mode==='none'?0:4.5;}
function jitterHue(h,rng,spread){
  const rnd=typeof rng==='function'?rng:Math.random;
  return (h+(rnd()*2-1)*spread+360)%360;
}
function paletteBaseHues(palette,ground){
  const cols=columnsFromPalette(palette||[],ground||{}).columns;
  return cols.map(c=>hueOfHex(c.base,0)).filter(Number.isFinite);
}
function paletteBaseHexes(palette,ground){
  return columnsFromPalette(palette||[],ground||{}).columns.map(c=>normHex(c.base)).filter(Boolean);
}
function sourceAnchorHues(palette,ground,cfg,baseHue){
  const mode=cfg&&cfg.sourceMode||'bg-fg';
  if(mode==='none')return [];
  if(mode==='palette')return paletteBaseHues(palette,ground);
  if(mode==='selected'&&typeof (cfg&&cfg.selectedHex)==='string'&&normHex(cfg.selectedHex))return [hueOfHex(cfg.selectedHex,baseHue)];
  const anchors=[];
  if(ground&&ground.bg)anchors.push(hueOfHex(ground.bg,baseHue));
  if(ground&&ground.fg)anchors.push(hueOfHex(ground.fg,baseHue));
  return anchors.filter(Number.isFinite);
}
function sourceAnchorHexes(palette,ground,cfg){
  const mode=cfg&&cfg.sourceMode||'bg-fg';
  if(mode==='none')return [];
  if(mode==='palette')return paletteBaseHexes(palette,ground);
  if(mode==='selected'&&typeof (cfg&&cfg.selectedHex)==='string'&&normHex(cfg.selectedHex))return [normHex(cfg.selectedHex)];
  return [ground&&ground.bg,ground&&ground.fg].map(h=>typeof h==='string'?normHex(h):null).filter(Boolean);
}
function bridgeHues(anchors,count,rng){
  if(anchors.length<2)return generatorHues(anchors[0]||250,'random',count,rng);
  const sorted=[...anchors].sort((a,b)=>a-b),pairs=[];
  for(let i=0;i<sorted.length;i++){
    const a=sorted[i],b=sorted[(i+1)%sorted.length]+(i===sorted.length-1?360:0);
    pairs.push(((a+b)/2)%360);
  }
  return Array.from({length:count},(_,i)=>jitterHue(pairs[i%pairs.length],rng,10));
}
function repeatOffsets(base,offsets,count){
  return Array.from({length:count},(_,i)=>(base+offsets[i%offsets.length]+360)%360);
}
function harmonyHues(intent,src,baseHue,count,rng){
  const b=src&&src.length?src[0]:baseHue, n=Math.max(1,Math.min(12,Math.round(count||8)));
  if(intent==='complementary')return repeatOffsets(b,[180],n);
  if(intent==='analogous')return repeatOffsets(b,[-30,30,-60,60,0],n);
  if(intent==='split-complementary')return repeatOffsets(b,[150,210,0],n);
  if(intent==='triadic')return repeatOffsets(b,[0,120,240],n);
  if(intent==='tetradic')return repeatOffsets(b,[0,60,180,240],n);
  if(intent==='square')return repeatOffsets(b,[0,90,180,270],n);
  if(intent==='monochromatic')return Array.from({length:n},()=>jitterHue(b,rng,3));
  if(intent==='rainbow')return Array.from({length:n},(_,i)=>(b+(i*360)/n)%360);
  return null;
}
function intentHues(intent,anchors,baseHue,count,rng){
  const n=Math.max(1,Math.min(12,Math.round(count||8))), src=anchors&&anchors.length?anchors:[baseHue];
  const harmony=harmonyHues(intent,src,baseHue,n,rng);
  if(harmony)return harmony;
  if(intent==='near-palette'||intent==='near-selected')return Array.from({length:n},(_,i)=>jitterHue(src[i%src.length],rng,18));
  if(intent==='fill-gaps')return generatorHues(src[0]||baseHue,'random',n,rng);
  if(intent==='complements')return Array.from({length:n},(_,i)=>jitterHue((src[i%src.length]+180)%360,rng,18));
  if(intent==='bridges')return bridgeHues(src,n,rng);
  return generatorHues(baseHue,'random',n,rng);
}
function vibeHueBias(hues,vibe,rng){
  const rnd=typeof rng==='function'?rng:Math.random;
  const pick=bands=>bands[Math.floor(rnd()*bands.length)];
  if(vibe==='warm')return hues.map(()=>jitterHue(pick([12,28,44,58]),rng,14));
  if(vibe==='cool')return hues.map(()=>jitterHue(pick([170,195,220,250,278]),rng,16));
  if(vibe==='earthy')return hues.map(h=>jitterHue([28,42,58,82,112].reduce((a,b)=>Math.abs(b-h)<Math.abs(a-h)?b:a,42),rng,12));
  return hues;
}
function candidateLightnesses(bgHex,vibe){
  const bgL=typeof bgHex==='string'&&normHex(bgHex)?oklchOf(bgHex).L:0;
  if(vibe==='pastel')return bgL>0.55?[0.74,0.68,0.80,0.62,0.86,0.56,0.50]:[0.82,0.88,0.76,0.92,0.70,0.64];
  if(vibe==='deep'||vibe==='jewel')return bgL>0.55?[0.30,0.24,0.36,0.18,0.42,0.48]:[0.56,0.62,0.50,0.68,0.44,0.74];
  return bgL>0.55
    ? [0.34,0.28,0.40,0.22,0.46,0.16,0.52,0.10,0.58]
    : [0.70,0.76,0.64,0.82,0.58,0.88,0.52,0.94,0.46];
}
function randomChroma(rng){
  const rnd=typeof rng==='function'?rng:Math.random;
  return 0.10+rnd()*0.09;
}
function vibeChroma(vibe,rng){
  const rnd=typeof rng==='function'?rng:Math.random;
  if(vibe==='muted')return 0.045+rnd()*0.035;
  if(vibe==='pastel')return 0.035+rnd()*0.045;
  if(vibe==='deep')return 0.085+rnd()*0.055;
  if(vibe==='jewel')return 0.12+rnd()*0.075;
  if(vibe==='earthy')return 0.055+rnd()*0.04;
  if(vibe==='warm'||vibe==='cool')return 0.08+rnd()*0.06;
  if(vibe==='neon')return 0.18+rnd()*0.09;
  if(vibe==='strange')return 0.145+rnd()*0.095;
  if(vibe==='balanced')return 0.075+rnd()*0.045;
  return 0.12+rnd()*0.07;
}
function accentCandidateForHue(hue,ground,cfg){
  const C=cfg&&cfg.vibe?vibeChroma(cfg.vibe,cfg.rng):(cfg&&cfg.scheme==='random'?randomChroma(cfg.rng):generatorChroma(cfg&&cfg.chromaMode)), target=generatorTarget(cfg&&cfg.contrastMode), bg=ground&&ground.bg;
  let best=null;
  for(const L of candidateLightnesses(bg,cfg&&cfg.vibe)){
    const c=oklch2hex(L,C,hue), r=bg?contrast(c.hex,bg):Infinity;
    const item={hex:c.hex,L,C,hue,contrast:r,clamped:c.clamped};
    if(!best||r>best.contrast)best=item;
    if(r>=target&&!isPureEndpointHex(c.hex))return item;
  }
  return best&&best.contrast>=target&&!isPureEndpointHex(best.hex)?best:null;
}
function candidateForHueLightness(hue,L,C,ground,cfg){
  const target=generatorTarget(cfg&&cfg.contrastMode),bg=ground&&ground.bg,c=oklch2hex(L,C,hue),r=bg?contrast(c.hex,bg):Infinity;
  return r>=target&&!isPureEndpointHex(c.hex)?{hex:c.hex,L,C,hue,contrast:r,clamped:c.clamped}:null;
}
function minDistanceToSet(hex,set){
  return set.length?Math.min(...set.map(h=>deltaE(hex,h))):Infinity;
}
function hueDistance(a,b){return Math.abs((((a-b+540)%360)-180));}
function anchorHueSet(hexes){
  return hexes.map(hex=>oklchOf(hex)).filter(lch=>lch.C>0.025&&Number.isFinite(lch.H)).map(lch=>lch.H);
}
function minHueDistance(hue,hues){
  return hues.length?Math.min(...hues.map(h=>hueDistance(hue,h))):180;
}
function perceptualGapCandidates(palette,ground,cfg,sourceMode,baseHue,count,scheme,intent,hueAware){
  const anchors=sourceAnchorHexes(palette,ground,Object.assign({},cfg,{sourceMode}));
  if(anchors.length<2){
    return intentHues('fill-gaps',sourceAnchorHues(palette,ground,Object.assign({},cfg,{sourceMode}),baseHue),baseHue,count,cfg.rng)
      .map(hue=>accentCandidateForHue(hue,ground,Object.assign({},cfg,{scheme,intent}))).filter(Boolean);
  }
  const C=cfg&&cfg.vibe?vibeChroma(cfg.vibe,cfg.rng):(scheme==='random'?randomChroma(cfg.rng):generatorChroma(cfg&&cfg.chromaMode));
  const hueStep=10,hueOffset=(typeof cfg.rng==='function'?cfg.rng():Math.random())*hueStep;
  const pool=[],seen=new Set();
  for(let hue=hueOffset;hue<360;hue+=hueStep){
    for(const L of candidateLightnesses(ground&&ground.bg,cfg&&cfg.vibe)){
      const cand=candidateForHueLightness(hue,L,C,ground,cfg);
      if(!cand)continue;
      const key=cand.hex.toLowerCase();
      if(seen.has(key))continue;
      seen.add(key);pool.push(cand);
    }
  }
  const picked=[],occupied=[...anchors];
  const occupiedHues=anchorHueSet(anchors);
  while(picked.length<count&&pool.length){
    let bestI=-1,bestScore=-1,bestContrast=-1;
    for(let i=0;i<pool.length;i++){
      const cand=pool[i],perceptual=minDistanceToSet(cand.hex,occupied);
      const hueBonus=hueAware?0.10*(minHueDistance(cand.hue,occupiedHues)/180):0;
      const score=perceptual+hueBonus;
      if(score>bestScore+1e-9||(Math.abs(score-bestScore)<1e-9&&cand.contrast>bestContrast)){
        bestI=i;bestScore=score;bestContrast=cand.contrast;
      }
    }
    const cand=pool.splice(bestI,1)[0];
    picked.push(cand);occupied.push(cand.hex);occupiedHues.push(cand.hue);
  }
  return picked;
}
function generatedMembers(baseHex,baseName,spanCount,columnId){
  const hex=typeof baseHex==='string'?normHex(baseHex):null;
  return hex?[{hex,name:baseName,offset:0,clamped:false,columnId}]:[];
}
function planPaletteGenerator(palette,ground,config){
  const cfg=config||{};
  const requestedSource=cfg.sourceMode||'bg-fg', resolvedSource=requestedSource==='selected'
    ? (typeof cfg.selectedHex==='string'&&normHex(cfg.selectedHex)?'selected':'bg-fg')
    : requestedSource;
  const sourceMode=['selected','palette','none','bg-fg'].includes(resolvedSource)?resolvedSource:'bg-fg';
  const scheme=cfg.scheme||'random', intent=cfg.intent||(cfg.scheme&&cfg.scheme!=='random'?'scheme':'random'), count=Math.max(1,Math.min(12,Math.round(cfg.accentCount??5)));
  const spanCount=Math.max(0,Math.min(8,Math.round(cfg.spanCount??2)));
  const used=generatedExistingNames(palette), baseHue=generatorSourceHue(palette,ground,Object.assign({},cfg,{sourceMode}));
  const anchors=sourceAnchorHues(palette,ground,Object.assign({},cfg,{sourceMode}),baseHue);
  const columns=[], rejected=[];
  const candidates=(intent==='fill-gaps'||intent==='fill-hue-gaps')
    ? perceptualGapCandidates(palette,ground,cfg,sourceMode,baseHue,count,scheme,intent,intent==='fill-hue-gaps').map(cand=>({cand,hue:cand&&cand.hue}))
    : vibeHueBias(intent&&intent!=='manual'&&intent!=='scheme'?intentHues(intent,anchors,baseHue,count,cfg.rng):generatorHues(baseHue,scheme,count,cfg.rng),cfg.vibe,cfg.rng)
      .map(hue=>({hue,cand:accentCandidateForHue(hue,ground,Object.assign({},cfg,{scheme,intent}))}));
  for(const {cand,hue} of candidates){
    if(!cand){rejected.push({hue,reason:'contrast'});continue;}
    const name=uniqueGeneratedName(nearestColorName(cand.hex,cfg.colorNames),used), columnId=name;
    const members=generatedMembers(cand.hex,name,spanCount,columnId);
    columns.push({name,columnId,baseHex:cand.hex,L:cand.L,C:cand.C,hue:cand.hue,contrast:cand.contrast,clamped:cand.clamped,members});
  }
  const contrasts=columns.map(c=>c.contrast).filter(Number.isFinite);
  return {
    sourceMode,
    scheme,
    intent,
    vibe: cfg.vibe||null,
    baseHue,
    accentCount:count,
    spanCount,
    columns,
    rejected,
    summary:{
      generated:columns.length,
      rejected:rejected.length,
      clamped:columns.reduce((n,c)=>n+(c.clamped?1:0)+c.members.filter(m=>m.clamped).length,0),
      minContrast:contrasts.length?Math.min(...contrasts):null,
    },
  };
}
function entriesForGeneratedColumn(column){
  if(!column||!Array.isArray(column.members))return [];
  const columnId=column.columnId||column.name||'generated';
  return column.members.map(m=>[m.hex,m.name,columnId]);
}

export { planPaletteGenerator, entriesForGeneratedColumn };

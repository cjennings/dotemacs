import json, re, os
HERE=os.path.dirname(os.path.abspath(__file__))
ns={}
src=open(os.path.join(HERE,'samples.py')).read()
exec(src[:src.index('cols=')], ns)
SAMPLES={"Elisp":ns['ELS'],"Go":ns['GOS'],"Python":ns['PYS'],"TypeScript":ns['TSS'],"Shell":ns['SHS'],"C/C++":ns['CS']}
COLS=ns['COLS']
MAP={k:v[0] for k,v in COLS.items()}; BOLD={k:v[1] for k,v in COLS.items()}; MAP['str']='#5d9b86'; MAP['bg']='#0d0b0a'
PALETTE=[["#67809c","blue"],["#e8bd30","gold"],["#9b5fd0","regal"],["#2ba178","emerald"],["#5d9b86","sage"],
 ["#cb6b4d","terracotta"],["#be9e74","tan"],["#cdced1","white"],["#a9b2bb","silver"],["#838d97","steel"],
 ["#5e6770","pewter"],["#2f343a","gunmetal"],["#264364","navy"],["#0d0b0a","ground"],["#1a1714","bg-dim"]]
CATS=[["bg","background (ground)","Aa Bb 123"],["p","fg · default text","other / whitespace"],["kw","keyword","class  def  if  return"],["bi","builtin","len  echo  printf"],
 ["pp","preprocessor","#include  #define"],["fnd","function · def","resolve  push"],
 ["fnc","function · call","printf  rsync  get"],["dec","decorator","@dataclass"],
 ["ty","type / class","int  str  Order  Queue"],["prop","property / field","id  name  items"],
 ["con","constant","None  nil  NULL  true"],["num","number","8080  100  -1"],
 ["str","string",'"dupre"  "fmt"'],["esc","escape","\\n  \\t"],["re","regexp","/^#[0-9a-f]+/"],
 ["doc","docstring",'"""..."""'],["cm","comment","# reject nil"],["cmd","comment delim","#  //  ;;"],
 ["var","variable / use","value  key  self"],["op","operator",":  =  ->  =="],
 ["punc","punctuation","{ }  ( )  ;"]]
UI_FACES=[["cursor","cursor","Aa|"],["region","region (selection)","selected text"],
 ["hl-line","hl-line (current line)","current line"],["highlight","highlight","hover"],
 ["mode-line","mode-line","status active"],["mode-line-inactive","mode-line-inactive","status idle"],
 ["fringe","fringe","| |"],["line-number","line-number","  42"],
 ["line-number-current-line","line-number-current-line","> 42"],["minibuffer-prompt","minibuffer-prompt","M-x "],
 ["isearch","isearch (match)","match"],["lazy-highlight","lazy-highlight","other match"],
 ["isearch-fail","isearch-fail","no match"],["show-paren-match","show-paren-match","( )"],
 ["show-paren-mismatch","show-paren-mismatch",") ("],["link","link","https://"],
 ["error","error","error!"],["warning","warning","warning"],
 ["success","success","ok"],["vertical-border","vertical-border","|"]]
UIMAP={"cursor":{"fg":None,"bg":"#a9b2bb"},"region":{"fg":None,"bg":"#264364"},
 "hl-line":{"fg":None,"bg":"#1a1714"},"highlight":{"fg":None,"bg":"#2f343a"},
 "mode-line":{"fg":"#cdced1","bg":"#2f343a"},"mode-line-inactive":{"fg":"#838d97","bg":"#1a1714"},
 "fringe":{"fg":None,"bg":"#0d0b0a"},"line-number":{"fg":"#5e6770","bg":None},
 "line-number-current-line":{"fg":"#e8bd30","bg":"#1a1714"},"minibuffer-prompt":{"fg":"#67809c","bg":None},
 "isearch":{"fg":"#0d0b0a","bg":"#e8bd30"},"lazy-highlight":{"fg":"#0d0b0a","bg":"#838d97"},
 "isearch-fail":{"fg":"#cb6b4d","bg":None},"show-paren-match":{"fg":None,"bg":"#264364"},
 "show-paren-mismatch":{"fg":"#0d0b0a","bg":"#cb6b4d"},"link":{"fg":"#67809c","bg":None},
 "error":{"fg":"#cb6b4d","bg":None},"warning":{"fg":"#e8bd30","bg":None},
 "success":{"fg":"#5d9b86","bg":None},"vertical-border":{"fg":"#2f343a","bg":None}}
def cid(l): return re.sub(r'\W','',l)
code_cont="".join(f'<div class="col"><h2>{l}</h2><pre id="code-{cid(l)}"></pre></div>' for l in SAMPLES)
HTML = """<!doctype html><meta charset=utf-8><title>theme-selector</title>
<style>
 body{background:#0d0b0a;color:#cdced1;font:15px/1.55 monospace;margin:20px}
 h1{font-size:22px;font-weight:normal;color:#e8bd30;margin:26px 0 10px;border-bottom:1px solid #252321;padding-bottom:6px}
 h2{font-size:10pt;color:#8a9496;font-weight:normal;margin:0 0 4px}
 .wrap{display:flex;flex-wrap:nowrap;overflow-x:auto;gap:14px;padding-bottom:10px}
 .col{flex:0 0 auto;width:460px}
 pre{background:#0d0b0a;border:1px solid #252321;border-radius:8px;padding:14px 16px;font-size:19px;overflow:auto;white-space:pre}
 table.leg{border-collapse:collapse} table.leg td{padding:4px 12px;vertical-align:middle}
 table.leg th{cursor:pointer;color:#b4b1a2;text-align:left;padding:4px 12px;user-select:none;font-weight:normal}
 table.leg th:hover{color:#e8bd30}
 select.chip{appearance:none;border:1px solid #00000060;border-radius:5px;padding:5px 10px;font:bold 14px monospace;width:160px;cursor:pointer}
 .cat{color:#b4b1a2} .ex{font-size:17px}
 .sbtn{width:26px;height:24px;border:1px solid #3a3a3a;border-radius:3px;background:#eaeaea;color:#111;cursor:pointer;font-size:15px;margin-right:2px;padding:0}
 .sbtn.on{background:#0d0b0a;color:#cdced1;border-color:#8a9496}
 .pals{display:flex;gap:8px;flex-wrap:wrap}
 .pchip{width:128px;height:58px;border-radius:6px;border:1px solid #00000060;position:relative;display:flex;flex-direction:column;align-items:center;justify-content:center;cursor:grab}
 .pchip.drag{opacity:.4} .pchip input.nm{background:transparent;border:none;text-align:center;font:bold 10pt monospace;width:108px;outline:none}
 .pchip .hx{font-size:10pt;opacity:.8} .pchip .rm{position:absolute;top:2px;right:5px;background:none;border:none;cursor:pointer;font-size:14px;font-weight:bold;opacity:.7}
 .palctl{margin-top:12px;display:flex;gap:8px;align-items:center;flex-wrap:wrap}
 .palctl input[type=text]{background:#161412;border:1px solid #252321;color:#cdced1;border-radius:4px;padding:5px 8px;font:10pt monospace}
 .palctl input[type=text]::placeholder{color:#b4b1a2;opacity:1}
 .palctl input[type=color]{width:128px;height:58px;border:1px solid #00000060;border-radius:6px;padding:2px;cursor:pointer}
 .palctl button,.filebar button,.fbtn{background:#252321;color:#e8bd30;border:1px solid #3a3a3a;border-radius:4px;padding:6px 12px;font:10pt monospace;cursor:pointer}
 #export{width:100%;height:180px;margin-top:10px;background:#0d0b0a;color:#a4ac64;border:1px solid #252321;border-radius:6px;font:10pt monospace;padding:10px}
 .filebar{margin:6px 0 0;display:flex;gap:8px;align-items:center}
 #pagetitle{font-size:30px;color:#cdced1;font-weight:normal;border:none;margin:4px 0 18px;padding:0}
</style>
<h1 id="pagetitle">Untitled: color palette</h1>
<h1>code samples</h1>
<div class="wrap">CODE_CONT</div>
<h1>color &rarr; category — chip reassigns · N/B/I sets weight &amp; slant · click a header to sort</h1>
<table class="leg" id="legtable"><thead><tr><th onclick="srt(0)">color &#9651;</th><th>style</th><th onclick="srt(1)">category &#9651;</th><th>example</th></tr></thead><tbody id="legbody"></tbody></table>
<h1>UI / interface faces — foreground &amp; background per face</h1>
<table class="leg" id="uitable"><thead><tr><th>face</th><th>foreground</th><th>background</th><th>preview</th></tr></thead><tbody id="uibody"></tbody></table>
<h1>palette — add / remove / rename / drag to reorder</h1>
<div class="pals" id="pals"></div>
<div class="palctl">
 <input type="color" id="newhex" value="#888888" oninput="syncHex('swatch')">
 <input type="text" id="newhexstr" placeholder="#rrggbb" value="#888888" oninput="syncHex('text')" style="width:110px">
 <input type="text" id="newname" placeholder="name">
 <button onclick="addColor()">+ add color</button>
</div>
<h1>save / load theme</h1>
<div class="filebar">
 <label style="color:#b4b1a2">theme name</label><input type="text" id="themename" value="" placeholder="untitled" oninput="updateTitle()" style="background:#161412;border:1px solid #252321;color:#cdced1;border-radius:4px;padding:5px 8px;font:10pt monospace;width:200px">
</div>
<div class="filebar">
 <button onclick="download()">&#11015; download &lt;name&gt;.json</button>
 <label class="fbtn">&#11014; load theme.json<input type="file" accept=".json" onchange="importFile(event)" style="display:none"></label>
 <button id="jsonbtn" onclick="toggleJSON()">show JSON</button>
</div>
<textarea id="export" style="display:none" readonly></textarea>
<script>
const SAMPLES=SAMPLES_J, CATS=CATS_J, UI_FACES=UIFACES_J;
let MAP=MAP_J, PALETTE=PALETTE_J, BOLD=BOLD_J, ITALIC={}, UIMAP=UIMAP_J;
function esc(t){return t.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');}
function lin(c){c/=255;return c<=0.03928?c/12.92:Math.pow((c+0.055)/1.055,2.4);}
function rl(h){return 0.2126*lin(parseInt(h.substr(1,2),16))+0.7152*lin(parseInt(h.substr(3,2),16))+0.0722*lin(parseInt(h.substr(5,2),16));}
function textOn(h){const L=rl(h);return ((L+0.05)/0.05)>(1.05/(L+0.05))?'#000':'#fff';}
function cid(l){return l.replace(/\\W/g,'');}
function renderCode(){
  for(const lang in SAMPLES){let html='';
    for(const line of SAMPLES[lang]){
      if(line.length===0){html+='\\n';continue;}
      for(const [k,t] of line){const c=MAP[k]||'#cdced1';const w=BOLD[k]?'bold':'normal';const s=ITALIC[k]?'italic':'normal';
        html+=`<span style="color:${c};font-weight:${w};font-style:${s}">${esc(t)}</span>`;}
      html+='\\n';}
    document.getElementById('code-'+cid(lang)).innerHTML=html;}
}
function buildTable(){
  const tb=document.getElementById('legbody');tb.innerHTML='';
  for(const [kind,label,ex] of CATS){
    const tr=document.createElement('tr');
    const sel=document.createElement('select');sel.className='chip';
    const cur=MAP[kind];const have=PALETTE.some(p=>p[0]===cur);
    const list=have?PALETTE:[[cur,'(gone) '+cur],...PALETTE];
    for(const [hex,name] of list){const o=document.createElement('option');o.value=hex;o.textContent=name+'  '+hex;o.style.background=hex;o.style.color=textOn(hex);sel.appendChild(o);}
    sel.value=cur;
    const exTd=document.createElement('td');exTd.className='ex';exTd.textContent=ex;
    function styleChip(){sel.style.background=sel.value;sel.style.color=textOn(sel.value);}
    function styleEx(){exTd.style.color=(kind==='bg'?MAP['p']:MAP[kind]);exTd.style.background=MAP['bg'];exTd.style.fontWeight=BOLD[kind]?'bold':'normal';exTd.style.fontStyle=ITALIC[kind]?'italic':'normal';}
    styleChip();styleEx();
    sel.onchange=()=>{MAP[kind]=sel.value;styleChip();styleEx();renderCode();if(kind==='bg')applyGround();};
    // style buttons
    const stTd=document.createElement('td');
    if(kind!=='bg'){const defs=[['N','a','normal'],['B','a','bold'],['I','a','italic']];
    const btns={};
    defs.forEach(([id,ch,mode])=>{const b=document.createElement('button');b.className='sbtn';b.style.fontWeight=mode==='bold'?'bold':'normal';b.style.fontStyle=mode==='italic'?'italic':'normal';b.textContent=ch;
      b.onclick=()=>{if(mode==='normal'){BOLD[kind]=false;ITALIC[kind]=false;}else if(mode==='bold'){BOLD[kind]=!BOLD[kind];}else{ITALIC[kind]=!ITALIC[kind];}refresh();renderCode();styleEx();};
      btns[mode]=b;stTd.appendChild(b);});
    function refresh(){btns.normal.classList.toggle('on',!BOLD[kind]&&!ITALIC[kind]);btns.bold.classList.toggle('on',!!BOLD[kind]);btns.italic.classList.toggle('on',!!ITALIC[kind]);}
    refresh();}
    const c0=document.createElement('td');c0.appendChild(sel);
    const c2=document.createElement('td');c2.className='cat';c2.textContent=label;
    tr.appendChild(c0);tr.appendChild(stTd);tr.appendChild(c2);tr.appendChild(exTd);
    tb.appendChild(tr);}
}
let dragFrom=null;
function renderPalette(){
  const p=document.getElementById('pals');p.innerHTML='';
  PALETTE.forEach((pc,i)=>{const [hex,name]=pc;const tc=textOn(hex);
    const d=document.createElement('div');d.className='pchip';d.style.background=hex;d.draggable=true;
    d.innerHTML=`<button class="rm" title="remove" style="color:${tc}">×</button><input class="nm" value="${name}" style="color:${tc}"><div class="hx" style="color:${tc}">${hex}</div>`;
    d.querySelector('.rm').onclick=()=>{PALETTE.splice(i,1);renderPalette();buildTable();};
    d.querySelector('.nm').onchange=(e)=>{PALETTE[i][1]=e.target.value;buildTable();buildUITable();};
    d.ondragstart=()=>{dragFrom=i;d.classList.add('drag');};
    d.ondragend=()=>d.classList.remove('drag');
    d.ondragover=(e)=>e.preventDefault();
    d.ondrop=(e)=>{e.preventDefault();if(dragFrom===null||dragFrom===i)return;const m=PALETTE.splice(dragFrom,1)[0];PALETTE.splice(i,0,m);dragFrom=null;renderPalette();buildTable();};
    p.appendChild(d);});
  buildUITable();
}
function normHex(s){s=s.trim();if(/^[0-9a-fA-F]{6}$/.test(s))s='#'+s;return /^#[0-9a-fA-F]{6}$/.test(s)?s.toLowerCase():null;}
function syncHex(src){const sw=document.getElementById('newhex'),tx=document.getElementById('newhexstr');
  if(src==='swatch'){tx.value=sw.value;}else{const h=normHex(tx.value);if(h)sw.value=h;}}
function addColor(){const h=normHex(document.getElementById('newhexstr').value)||document.getElementById('newhex').value;const name=document.getElementById('newname').value||h;PALETTE.push([h,name]);document.getElementById('newname').value='';renderPalette();buildTable();}
function themeName(){return (document.getElementById('themename').value||'theme').trim()||'theme';}
function fileSlug(){return themeName().replace(/[^A-Za-z0-9._-]+/g,'-').replace(/^-+|-+$/g,'')||'theme';}
function exportObj(){const a={};CATS.forEach(c=>a[c[0]]=MAP[c[0]]);return {name:themeName(),palette:PALETTE,assignments:a,bold:Object.keys(BOLD).filter(k=>BOLD[k]),italic:Object.keys(ITALIC).filter(k=>ITALIC[k]),ui:UIMAP};}
function exportState(){const t=document.getElementById('export');t.value=JSON.stringify(exportObj(),null,1);t.style.display='block';t.focus();t.select();}
function toggleJSON(){const t=document.getElementById('export'),b=document.getElementById('jsonbtn');if(t.style.display==='block'){t.style.display='none';b.textContent='show JSON';}else{exportState();b.textContent='hide JSON';}}
function updateTitle(){const n=document.getElementById('themename').value.trim();document.getElementById('pagetitle').textContent=(n||'Untitled')+': color palette';}
function download(){const blob=new Blob([JSON.stringify(exportObj(),null,1)],{type:'application/json'});const a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download=fileSlug()+'.json';a.click();}
function importFile(ev){const f=ev.target.files[0];if(!f)return;const r=new FileReader();
  r.onload=()=>{try{const d=JSON.parse(r.result);if(d.name)document.getElementById('themename').value=d.name;if(d.palette)PALETTE=d.palette;if(d.assignments)Object.assign(MAP,d.assignments);
    BOLD={};(d.bold||[]).forEach(k=>BOLD[k]=true);ITALIC={};(d.italic||[]).forEach(k=>ITALIC[k]=true);
    if(d.ui)Object.assign(UIMAP,d.ui);
    renderPalette();buildTable();buildUITable();renderCode();applyGround();updateTitle();}catch(e){alert('bad theme file: '+e.message);}};
  r.readAsText(f);ev.target.value='';}
function applyGround(){document.querySelectorAll('pre').forEach(p=>p.style.background=MAP['bg']);document.querySelectorAll('.ex').forEach(e=>e.style.background=MAP['bg']);}
function uiSelect(face,attr){
  const sel=document.createElement('select');sel.className='chip';
  const none=document.createElement('option');none.value='';none.textContent='— none —';none.style.background='#161412';none.style.color='#b4b1a2';sel.appendChild(none);
  for(const [hex,name] of PALETTE){const o=document.createElement('option');o.value=hex;o.textContent=name+'  '+hex;o.style.background=hex;o.style.color=textOn(hex);sel.appendChild(o);}
  sel.value=UIMAP[face][attr]||'';
  function style(){if(sel.value){sel.style.background=sel.value;sel.style.color=textOn(sel.value);}else{sel.style.background='#161412';sel.style.color='#b4b1a2';}}
  style();
  sel.onchange=()=>{UIMAP[face][attr]=sel.value||null;style();paintUI(face);};
  return sel;
}
function paintUI(face){const pv=document.getElementById('uiprev-'+face);if(!pv)return;pv.style.color=UIMAP[face].fg||MAP['p'];pv.style.background=UIMAP[face].bg||MAP['bg'];}
function buildUITable(){
  const tb=document.getElementById('uibody');tb.innerHTML='';
  for(const [face,label,ex] of UI_FACES){
    const tr=document.createElement('tr');
    const c0=document.createElement('td');c0.className='cat';c0.textContent=label;
    const cF=document.createElement('td');cF.appendChild(uiSelect(face,'fg'));
    const cB=document.createElement('td');cB.appendChild(uiSelect(face,'bg'));
    const cP=document.createElement('td');cP.className='ex';cP.id='uiprev-'+face;cP.textContent=ex;cP.style.padding='4px 10px';cP.style.borderRadius='4px';
    tr.appendChild(c0);tr.appendChild(cF);tr.appendChild(cB);tr.appendChild(cP);tb.appendChild(tr);paintUI(face);
  }
}
let D={};
function srt(c){const tb=document.getElementById('legbody');const r=[...tb.rows];D[c]=!D[c];
  r.sort((a,b)=>{const x=(c===0?a.querySelector('select').value:a.cells[2].innerText).toLowerCase(),
                       y=(c===0?b.querySelector('select').value:b.cells[2].innerText).toLowerCase();
                 return (x<y?-1:x>y?1:0)*(D[c]?1:-1);});r.forEach(x=>tb.appendChild(x));}
renderPalette();buildTable();buildUITable();renderCode();applyGround();updateTitle();
</script>"""
HTML=(HTML.replace("CODE_CONT",code_cont).replace("SAMPLES_J",json.dumps(SAMPLES))
 .replace("PALETTE_J",json.dumps(PALETTE)).replace("CATS_J",json.dumps(CATS))
 .replace("UIFACES_J",json.dumps(UI_FACES)).replace("UIMAP_J",json.dumps(UIMAP))
 .replace("BOLD_J",json.dumps(BOLD)).replace("MAP_J",json.dumps(MAP)))
OUT=os.path.join(HERE,'theme-selector.html')
open(OUT,"w").write(HTML)
print("wrote",OUT)

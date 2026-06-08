GROUND="#0d0b0a"
COLS={
 'kw':("#67809c",True),'bi':("#67809c",False),'pp':("#67809c",False),
 'fnd':("#a9b2bb",True),'fnc':("#a9b2bb",False),'dec':("#e8bd30",False),
 'ty':("#9b5fd0",False),'prop':("#838d97",False),
 'con':("#cb6b4d",False),'num':("#cb6b4d",False),'esc':("#cb6b4d",False),
 'str':("#2ba178",False),'re':("#5d9b86",False),'doc':("#5d9b86",False),
 'cm':("#be9e74",False),'cmd':("#a9b2bb",False),
 'var':("#e8bd30",False),'op':("#a9b2bb",False),'punc':("#a9b2bb",False),'p':("#ffffff",False),
}
NAMES={"#67809c":"blue","#e8bd30":"gold","#9b5fd0":"regal","#2ba178":"emerald","#cb6b4d":"terracotta","#be9e74":"tan","#5d9b86":"sage","#cdced1":"white","#a9b2bb":"silver","#838d97":"steel","#5e6770":"pewter","#2f343a":"gunmetal","#264364":"navy"}
def esc(t): return t.replace("&","&amp;").replace("<","&lt;").replace(">","&gt;")
def span(k,t):
    c,b=COLS[k]; w=";font-weight:bold" if b else ""
    return f'<span style="color:{c}{w}">{esc(t)}</span>'
def render(lines): return "\n".join("".join(span(k,t) for k,t in ln) or "&nbsp;" for ln in lines)

PYS=[
 [('cmd','#'),('cm',' theme.py')],
 [('kw','from'),('p',' '),('var','dataclasses'),('p',' '),('kw','import'),('p',' '),('var','dataclass'),('punc',','),('p',' '),('var','field')],
 [],
 [('con','DEFAULT_PORT'),('op',':'),('p',' '),('ty','int'),('p',' '),('op','='),('p',' '),('num','8080')],
 [('con','HEX'),('p',' '),('op','='),('p',' '),('var','re'),('op','.'),('fnc','compile'),('punc','('),('re','r"#[0-9a-f]{6}"'),('punc',')')],
 [],
 [('dec','@dataclass')],
 [('kw','class'),('p',' '),('ty','Theme'),('op',':')],
 [('p','    '),('doc','"""A color theme."""')],
 [('p','    '),('prop','name'),('op',':'),('p',' '),('ty','str'),('p',' '),('op','='),('p',' '),('str','"dupre"')],
 [('p','    '),('prop','colors'),('op',':'),('p',' '),('ty','dict'),('p',' '),('op','='),('p',' '),('fnc','field'),('punc','('),('prop','default_factory'),('op','='),('ty','dict'),('punc',')')],
 [],
 [('p','    '),('kw','def'),('p',' '),('fnd','resolve'),('punc','('),('var','self'),('punc',','),('p',' '),('var','key'),('op',':'),('p',' '),('ty','str'),('punc',')'),('p',' '),('op','->'),('p',' '),('ty','str'),('p',' '),('op','|'),('p',' '),('con','None'),('op',':')],
 [('p','        '),('cmd','#'),('cm',' fallback to none')],
 [('p','        '),('var','v'),('p',' '),('op','='),('p',' '),('var','self'),('op','.'),('prop','colors'),('op','.'),('fnc','get'),('punc','('),('var','key'),('punc',','),('p',' '),('str','"'),('esc','\\t'),('str','none"'),('punc',')')],
 [('p','        '),('kw','if'),('p',' '),('bi','len'),('punc','('),('var','v'),('punc',')'),('p',' '),('op','=='),('p',' '),('num','0'),('op',':'),('p',' '),('kw','return'),('p',' '),('con','None')],
 [('p','        '),('kw','return'),('p',' '),('var','v')],
 [],
 [('p','    '),('dec','@property')],
 [('p','    '),('kw','def'),('p',' '),('fnd','size'),('punc','('),('var','self'),('punc',')'),('p',' '),('op','->'),('p',' '),('ty','int'),('op',':')],
 [('p','        '),('kw','return'),('p',' '),('bi','len'),('punc','('),('var','self'),('op','.'),('prop','colors'),('punc',')')],
 [],
 [('var','theme'),('p',' '),('op','='),('p',' '),('ty','Theme'),('punc','('),('str','"dupre"'),('punc',')')],
 [('fnc','print'),('punc','('),('var','theme'),('op','.'),('fnc','resolve'),('punc','('),('str','"bg"'),('punc','))')],
]
ELS=[
 [('cmd',';;'),('cm',' cache.el')],
 [('punc','('),('kw','require'),('p',' '),('con',"'cl-lib"),('punc',')')],
 [],
 [('punc','('),('kw','defvar'),('p',' '),('var','cache--tbl'),('p',' '),('punc','('),('fnc','make-hash-table'),('p',' '),('con',':test'),('p',' '),('con',"'equal"),('punc','))')],
 [('p','  '),('doc','"Memo table.")')],
 [],
 [('punc','('),('kw','defun'),('p',' '),('fnd','cache-get'),('p',' '),('punc','('),('var','key'),('punc',')')],
 [('p','  '),('doc','"Return cached value for KEY."')],
 [('p','  '),('punc','('),('kw','or'),('p',' '),('punc','('),('bi','gethash'),('p',' '),('var','key'),('p',' '),('var','cache--tbl'),('punc',')')],
 [('p','      '),('punc','('),('kw','let'),('p',' '),('punc','(('),('var','v'),('p',' '),('punc','('),('fnc','compute'),('p',' '),('var','key'),('p',' '),('num','42'),('punc','))) ')],
 [('p','        '),('punc','('),('fnc','puthash'),('p',' '),('var','key'),('p',' '),('var','v'),('p',' '),('var','cache--tbl'),('punc',') '),('var','v'),('punc','))))')],
 [],
 [('punc','('),('kw','defun'),('p',' '),('fnd','cache-clear'),('p',' '),('punc','()')],
 [('p','  '),('doc','"Empty the memo table."')],
 [('p','  '),('punc','('),('kw','interactive'),('punc',')')],
 [('p','  '),('punc','('),('fnc','clrhash'),('p',' '),('var','cache--tbl'),('punc',')')],
 [('p','  '),('punc','('),('fnc','message'),('p',' '),('str','"cleared'),('esc','\\n'),('str','"'),('punc','))')],
 [],
 [('punc','('),('kw','defun'),('p',' '),('fnd','cache-keys'),('p',' '),('punc','()')],
 [('p','  '),('doc','"Return all keys."')],
 [('p','  '),('punc','('),('kw','let'),('p',' '),('punc','(('),('var','acc'),('p',' '),('con','nil'),('punc','))')],
 [('p','    '),('punc','('),('fnc','maphash'),('p',' '),('punc','('),('kw','lambda'),('p',' '),('punc','('),('var','k'),('p',' '),('var','_v'),('punc',')'),('p',' '),('punc','('),('fnc','push'),('p',' '),('var','k'),('p',' '),('var','acc'),('punc','))')],
 [('p','      '),('var','cache--tbl'),('punc',')'),('p',' '),('var','acc'),('punc','))')],
 [],
 [('punc','('),('kw','provide'),('p',' '),('con',"'cache"),('punc',')')],
]
GOS=[
 [('cmd','//'),('cm',' queue.go')],
 [('kw','package'),('p',' '),('var','main')],
 [],
 [('kw','import'),('p',' '),('str','"fmt"')],
 [],
 [('kw','const'),('p',' '),('con','MaxItems'),('p',' '),('op','='),('p',' '),('num','100')],
 [],
 [('kw','type'),('p',' '),('ty','Order'),('p',' '),('kw','struct'),('p',' '),('punc','{')],
 [('p','    '),('prop','ID'),('p','   '),('ty','int')],
 [('p','    '),('prop','Name'),('p',' '),('ty','string')],
 [('punc','}')],
 [],
 [('kw','func'),('p',' '),('punc','('),('var','q'),('p',' '),('op','*'),('ty','Queue'),('punc',')'),('p',' '),('fnd','Push'),('punc','('),('var','o'),('p',' '),('op','*'),('ty','Order'),('punc',')'),('p',' '),('ty','error'),('p',' '),('punc','{')],
 [('p','    '),('cmd','//'),('cm',' reject nil')],
 [('p','    '),('kw','if'),('p',' '),('var','o'),('p',' '),('op','=='),('p',' '),('con','nil'),('p',' '),('punc','{')],
 [('p','        '),('kw','return'),('p',' '),('fnc','fmt.Errorf'),('punc','('),('str','"nil'),('esc','\\n'),('str','"'),('punc',')')],
 [('p','    '),('punc','}')],
 [('p','    '),('var','q'),('op','.'),('prop','items'),('p',' '),('op','='),('p',' '),('bi','append'),('punc','('),('var','q'),('op','.'),('prop','items'),('punc',','),('p',' '),('var','o'),('punc',')')],
 [('p','    '),('kw','return'),('p',' '),('con','nil')],
 [('punc','}')],
 [],
 [('kw','func'),('p',' '),('fnd','main'),('punc','()'),('p',' '),('punc','{')],
 [('p','    '),('fnc','fmt.Println'),('punc','('),('op','&'),('ty','Queue'),('punc','{}'),('punc',')')],
 [('punc','}')],
]
TSS=[
 [('cmd','//'),('cm',' orders.ts')],
 [('kw','import'),('p',' '),('punc','{'),('p',' '),('ty','Order'),('p',' '),('punc','}'),('p',' '),('kw','from'),('p',' '),('str','"./types"')],
 [],
 [('kw','export'),('p',' '),('kw','interface'),('p',' '),('ty','Queue'),('p',' '),('punc','{')],
 [('p','  '),('prop','max'),('op',':'),('p',' '),('ty','number'),('punc',';')],
 [('p','  '),('prop','items'),('op',':'),('p',' '),('ty','Order'),('punc','[];')],
 [('punc','}')],
 [],
 [('dec','@Injectable'),('punc','()')],
 [('kw','export'),('p',' '),('kw','class'),('p',' '),('ty','OrderQueue'),('p',' '),('kw','implements'),('p',' '),('ty','Queue'),('p',' '),('punc','{')],
 [('p','  '),('kw','private'),('p',' '),('prop','re'),('p',' '),('op','='),('p',' '),('re','/^#[0-9a-f]{6}$/i'),('punc',';')],
 [],
 [('p','  '),('fnd','push'),('punc','('),('var','o'),('op',':'),('p',' '),('ty','Order'),('punc',')'),('op',':'),('p',' '),('ty','boolean'),('p',' '),('punc','{')],
 [('p','    '),('kw','if'),('p',' '),('punc','('),('var','o'),('p',' '),('op','==='),('p',' '),('con','null'),('punc',')'),('p',' '),('kw','return'),('p',' '),('con','false'),('punc',';')],
 [('p','    '),('var','console'),('op','.'),('fnc','log'),('punc','('),('str','`id '),('punc','${'),('var','o'),('op','.'),('prop','id'),('punc','}'),('esc','\\n'),('str','`'),('punc',');')],
 [('p','    '),('kw','return'),('p',' '),('con','true'),('punc',';')],
 [('p','  '),('punc','}')],
 [('punc','}')],
 [],
 [('kw','const'),('p',' '),('con','LIMIT'),('op',':'),('p',' '),('ty','number'),('p',' '),('op','='),('p',' '),('num','50'),('punc',';')],
 [('kw','const'),('p',' '),('var','q'),('p',' '),('op','='),('p',' '),('kw','new'),('p',' '),('ty','OrderQueue'),('punc','()'),('punc',';')],
 [('var','q'),('op','.'),('fnd','push'),('punc','('),('punc','{'),('p',' '),('prop','id'),('op',':'),('p',' '),('num','1'),('p',' '),('punc','}'),('p',' '),('kw','as'),('p',' '),('ty','Order'),('punc',')'),('punc',';')],
 [('var','console'),('op','.'),('fnc','log'),('punc','('),('var','q'),('op','.'),('prop','max'),('punc',')'),('punc',';')],
 [('kw','const'),('p',' '),('var','cap'),('p',' '),('op','='),('p',' '),('var','Math'),('op','.'),('bi','max'),('punc','('),('con','LIMIT'),('punc',','),('p',' '),('num','0'),('punc',')'),('punc',';')],
]

CS=[
 [('cmd','/**'),('doc',' Order queue. */')],
 [('pp','#include'),('p',' '),('str','<stdio.h>')],
 [('pp','#include'),('p',' '),('str','<stdlib.h>')],
 [('pp','#define'),('p',' '),('con','MAX_PORT'),('p',' '),('num','8080')],
 [],
 [('kw','typedef'),('p',' '),('kw','struct'),('p',' '),('punc','{')],
 [('p','    '),('ty','int'),('p','   '),('prop','id'),('punc',';')],
 [('p','    '),('kw','const'),('p',' '),('ty','char'),('p',' '),('op','*'),('prop','name'),('punc',';')],
 [('punc','}'),('p',' '),('ty','Order'),('punc',';')],
 [],
 [('cmd','//'),('cm',' returns -1 on null input')],
 [('ty','int'),('p',' '),('fnd','push'),('punc','('),('ty','Order'),('p',' '),('op','*'),('var','o'),('punc',')'),('p',' '),('dec','__attribute__'),('punc','(('),('dec','nonnull'),('punc','))'),('p',' '),('punc','{')],
 [('p','    '),('kw','if'),('p',' '),('punc','('),('var','o'),('p',' '),('op','=='),('p',' '),('con','NULL'),('punc',')'),('p',' '),('kw','return'),('p',' '),('num','-1'),('punc',';')],
 [('p','    '),('fnc','printf'),('punc','('),('str','"id=%d'),('esc','\\n'),('str','"'),('punc',','),('p',' '),('var','o'),('op','->'),('prop','id'),('punc',');')],
 [('p','    '),('kw','return'),('p',' '),('num','0'),('punc',';')],
 [('punc','}')],
 [],
 [('ty','int'),('p',' '),('fnd','main'),('punc','('),('ty','void'),('punc',')'),('p',' '),('punc','{')],
 [('p','    '),('ty','Order'),('p',' '),('var','o'),('p',' '),('op','='),('p',' '),('punc','{'),('p',' '),('op','.'),('prop','id'),('p',' '),('op','='),('p',' '),('num','1'),('punc',','),('p',' '),('op','.'),('prop','name'),('p',' '),('op','='),('p',' '),('str','"dupre"'),('p',' '),('punc','}'),('punc',';')],
 [('p','    '),('ty','Order'),('p',' '),('op','*'),('var','p2'),('p',' '),('op','='),('p',' '),('bi','malloc'),('punc','('),('bi','sizeof'),('punc','('),('ty','Order'),('punc','))'),('punc',';')],
 [('p','    '),('fnc','push'),('punc','('),('op','&'),('var','o'),('punc',')'),('punc',';')],
 [('p','    '),('bi','free'),('punc','('),('var','p2'),('punc',')'),('punc',';')],
 [('p','    '),('kw','return'),('p',' '),('num','0'),('punc',';')],
 [('punc','}')],
]
CPS=[
 [('cmd','/**'),('doc',' A color theme. */')],
 [('pp','#include'),('p',' '),('str','<string>')],
 [('pp','#include'),('p',' '),('str','<regex>')],
 [('pp','#pragma'),('p',' '),('pp','once')],
 [],
 [('kw','namespace'),('p',' '),('var','dupre'),('p',' '),('punc','{')],
 [],
 [('kw','template'),('op','<'),('kw','typename'),('p',' '),('ty','T'),('op','>'),('p',' '),('kw','class'),('p',' '),('ty','Theme'),('p',' '),('punc','{')],
 [('kw','public'),('op',':')],
 [('p','    '),('kw','static'),('p',' '),('kw','constexpr'),('p',' '),('ty','int'),('p',' '),('con','MAX'),('p',' '),('op','='),('p',' '),('num','0x20'),('punc',';')],
 [('p','    '),('ty','std'),('op','::'),('ty','string'),('p',' '),('prop','name_'),('p',' '),('op','='),('p',' '),('str','"dupre"'),('punc',';')],
 [],
 [('p','    '),('dec','[[nodiscard]]'),('p',' '),('ty','T'),('p',' '),('fnd','resolve'),('punc','('),('kw','const'),('p',' '),('ty','std'),('op','::'),('ty','string'),('op','&'),('p',' '),('var','key'),('punc',')'),('p',' '),('kw','const'),('p',' '),('punc','{')],
 [('p','        '),('cmd','//'),('cm',' validate against a hex pattern')],
 [('p','        '),('kw','static'),('p',' '),('ty','std'),('op','::'),('ty','regex'),('p',' '),('var','re'),('punc','('),('re','R"(#[0-9a-f]{6})"'),('punc',')'),('punc',';')],
 [('p','        '),('kw','if'),('p',' '),('punc','('),('var','key'),('op','.'),('fnc','empty'),('punc','()'),('punc',')'),('p',' '),('kw','return'),('p',' '),('con','nullptr'),('punc',';')],
 [('p','        '),('kw','return'),('p',' '),('ty','T'),('punc','{'),('var','key'),('punc','}'),('punc',';')],
 [('p','    '),('punc','}')],
 [('punc','}'),('punc',';')],
 [],
 [('ty','int'),('p',' '),('fnd','main'),('punc','()'),('p',' '),('punc','{')],
 [('p','    '),('kw','auto'),('p',' '),('var','t'),('p',' '),('op','='),('p',' '),('ty','Theme'),('op','<'),('ty','int'),('op','>'),('punc','{}'),('punc',';')],
 [('p','    '),('bi','static_cast'),('op','<'),('ty','int'),('op','>'),('punc','('),('var','t'),('op','.'),('prop','name_'),('op','.'),('fnc','size'),('punc','())'),('punc',';')],
 [('p','    '),('ty','std'),('op','::'),('fnc','printf'),('punc','('),('str','"%s'),('esc','\\n'),('str','"'),('punc',','),('p',' '),('var','t'),('op','.'),('prop','name_'),('op','.'),('fnc','c_str'),('punc','())'),('punc',';')],
 [('p','    '),('kw','return'),('p',' '),('num','0'),('punc',';')],
 [('punc','}')],
]
JAS=[
 [('cmd','/**'),('doc',' A color theme. */')],
 [('kw','package'),('p',' '),('var','com'),('op','.'),('var','dupre'),('punc',';')],
 [('kw','import'),('p',' '),('var','java'),('op','.'),('var','util'),('op','.'),('var','regex'),('op','.'),('ty','Pattern'),('punc',';')],
 [],
 [('dec','@Deprecated')],
 [('kw','public'),('p',' '),('kw','final'),('p',' '),('kw','class'),('p',' '),('ty','Theme'),('p',' '),('punc','{')],
 [('p','    '),('kw','private'),('p',' '),('kw','static'),('p',' '),('kw','final'),('p',' '),('ty','int'),('p',' '),('con','MAX_PORT'),('p',' '),('op','='),('p',' '),('num','8080'),('punc',';')],
 [('p','    '),('kw','private'),('p',' '),('kw','final'),('p',' '),('ty','String'),('p',' '),('prop','name'),('p',' '),('op','='),('p',' '),('str','"dupre"'),('punc',';')],
 [('p','    '),('kw','private'),('p',' '),('kw','static'),('p',' '),('kw','final'),('p',' '),('ty','Pattern'),('p',' '),('con','HEX'),('p',' '),('op','='),('p',' '),('ty','Pattern'),('op','.'),('fnc','compile'),('punc','('),('re','"#[0-9a-f]{6}"'),('punc',')'),('punc',';')],
 [],
 [('p','    '),('dec','@Override')],
 [('p','    '),('kw','public'),('p',' '),('ty','String'),('p',' '),('fnd','resolve'),('punc','('),('ty','String'),('p',' '),('var','key'),('punc',')'),('p',' '),('punc','{')],
 [('p','        '),('cmd','//'),('cm',' fall back to null')],
 [('p','        '),('kw','if'),('p',' '),('punc','('),('var','key'),('op','.'),('fnc','isEmpty'),('punc','()'),('punc',')'),('p',' '),('kw','return'),('p',' '),('con','null'),('punc',';')],
 [('p','        '),('kw','return'),('p',' '),('var','key'),('op','.'),('fnc','strip'),('punc','('),('punc',')'),('op','+'),('str','"'),('esc','\\t'),('str','"'),('punc',';')],
 [('p','    '),('punc','}')],
 [],
 [('p','    '),('kw','public'),('p',' '),('kw','static'),('p',' '),('ty','void'),('p',' '),('fnd','main'),('punc','('),('ty','String'),('punc','[]'),('p',' '),('var','args'),('punc',')'),('p',' '),('punc','{')],
 [('p','        '),('ty','var'),('p',' '),('var','t'),('p',' '),('op','='),('p',' '),('kw','new'),('p',' '),('ty','Theme'),('punc','()'),('punc',';')],
 [('p','        '),('ty','System'),('op','.'),('prop','out'),('op','.'),('fnc','println'),('punc','('),('var','t'),('op','.'),('fnc','resolve'),('punc','('),('str','"bg"'),('punc','))'),('punc',';')],
 [('p','    '),('punc','}')],
 [('punc','}')],
]
SHS=[
 [('cmd','#!'),('cm','/bin/bash')],
 [('cmd','#'),('cm',' deploy.sh')],
 [('bi','set'),('p',' '),('op','-'),('var','euo'),('p',' '),('var','pipefail')],
 [],
 [('var','PORT'),('op','='),('num','8080')],
 [('var','NAME'),('op','='),('str','"dupre"')],
 [],
 [('fnd','deploy'),('punc','()'),('p',' '),('punc','{')],
 [('p','    '),('kw','local'),('p',' '),('var','target'),('op','='),('str','"$1"')],
 [('p','    '),('kw','if'),('p',' '),('punc','[['),('p',' '),('op','-z'),('p',' '),('str','"$target"'),('p',' '),('punc',']]'),('punc',';'),('p',' '),('kw','then')],
 [('p','        '),('bi','echo'),('p',' '),('str','"no target"')],
 [('p','        '),('kw','return'),('p',' '),('num','1')],
 [('p','    '),('kw','fi')],
 [('p','    '),('fnc','rsync'),('p',' '),('op','-az'),('p',' '),('str','"$NAME"'),('p',' '),('str','"$target"')],
 [('punc','}')],
 [],
 [('fnd','main'),('punc','()'),('p',' '),('punc','{')],
 [('p','    '),('kw','for'),('p',' '),('var','host'),('p',' '),('kw','in'),('p',' '),('str','"$@"'),('punc',';'),('p',' '),('kw','do')],
 [('p','        '),('fnc','deploy'),('p',' '),('str','"$host"'),('p',' '),('op','||'),('p',' '),('bi','exit'),('p',' '),('num','1')],
 [('p','    '),('kw','done')],
 [('p','    '),('bi','echo'),('p',' '),('op','-e'),('p',' '),('str','"all done'),('esc','\\n'),('str','"')],
 [('punc','}')],
 [],
 [('fnc','main'),('p',' '),('str','"$@"')],
]

cols="".join(f'<div class="col"><h2>{n}</h2><pre>{render(s)}</pre></div>' for n,s in [("Elisp",ELS),("Go",GOS),("Python",PYS),("TypeScript",TSS),("Java",JAS),("C",CS),("C++",CPS),("Shell",SHS)])
legend_rows=[
 ("keyword (bold)","kw","class def if return import"),("builtin","bi","len range print"),
 ("function — definition (bold)","fnd","resolve cache-get push"),("function — call","fnc","get append fmt.Errorf"),
 ("decorator / attribute","dec","@dataclass @Injectable"),("type / class","ty","str dict Order Queue boolean"),
 ("property / field / key","prop","name colors items id re"),("constant","con","None nil true MaxItems :test"),
 ("number","num","8080 100 42 0"),("string","str",'"dupre" "fmt" `id`'),("escape","esc",r'\t \n'),
 ("regexp","re",'/^#[0-9a-f]{6}$/i'),("docstring","doc",'"""..." "Memo table."'),
 ("comment","cm","# reject nil // fallback"),("comment delimiter","cmd","# // ;; /*"),
 ("variable / use","var","v key self q console"),("operator","op",": = -> | == === . *"),
 ("punctuation / bracket","punc","{ } ( ) [ ] , ;"),
]
def lrow(label,k,ex):
    c,b=COLS[k]
    return f'<tr><td class="sw" style="background:{c}"></td><td class="nm">{NAMES.get(c,"")}</td><td class="hx">{c}</td><td class="cat">{label}</td><td class="ex" style="color:{c}{";font-weight:bold" if b else ""}">{esc(ex)}</td></tr>'
legend="".join(lrow(l,k,e) for l,k,e in legend_rows)
def grp(title,items):
    sw="".join(f'<div class="m"><div class="psw" style="background:{h}"></div><div class="lb">{n}<br>{h}</div></div>' for n,h in items)
    return f'<div class="g"><div class="gt">{title}</div><div class="ramp">{sw}</div></div>'
palette=(grp("ground / foreground",[("ground","#0d0b0a"),("bg-dim","#1a1714"),("fg","#cdced1")])
 + grp("syntax hues",[("blue · keyword","#67809c"),("gold · variable","#e8bd30"),("regal · type","#9b5fd0"),("emerald · string","#2ba178"),("terracotta · const/num","#cb6b4d"),("tan · comment","#be9e74")])
 + grp("metallic greyscale (structural)",[("gunmetal","#2f343a"),("metal","#474e56"),("pewter","#5e6770"),("steel · property","#838d97"),("silver · fn/op/punct","#a9b2bb"),("bright · fg","#cdced1")])
 + grp("special green + fills",[("muted emerald · doc/regexp","#5d9b86"),("navy fill","#264364"),("gunmetal fill","#2f343a")]))
html=f'''<!doctype html><meta charset=utf-8><title>dupre revision — canonical</title>
<style>body{{background:{GROUND};color:#cdced1;font:15px/1.55 monospace;margin:20px}}
 h1{{font-size:22px;font-weight:normal;color:#e8bd30;margin:26px 0 10px;border-bottom:1px solid #252321;padding-bottom:6px}}
 h2{{font-size:13px;color:#8a9496;font-weight:normal;margin:0 0 4px}}
 .wrap{{display:flex;flex-wrap:nowrap;overflow-x:auto;gap:14px;padding-bottom:10px}} .col{{flex:0 0 auto;width:460px}}
 pre{{background:#0d0b0a;border:1px solid #252321;border-radius:8px;padding:14px 16px;font-size:19px;overflow:auto}}
 table.leg{{border-collapse:collapse}} table.leg td{{padding:3px 10px;vertical-align:middle}} table.leg th{{cursor:pointer;color:#b4b1a2;text-align:left;padding:3px 10px;user-select:none;font-weight:normal}} table.leg th:hover{{color:#e8bd30}}
 .sw{{width:26px;height:16px;border-radius:3px;border:1px solid #00000060}} .nm{{color:#c0c5ca;font-size:12px}} .hx{{color:#969385;font-size:11px}} .cat{{color:#b4b1a2}} .ex{{font-size:18px}}
 .g{{margin:4px 0 10px}} .gt{{color:#8a9496;font-size:12px;margin-bottom:4px}}
 .ramp{{display:flex;gap:8px;flex-wrap:wrap}} .m{{text-align:center}} .psw{{width:120px;height:30px;border-radius:5px;border:1px solid #00000060}} .lb{{font-size:10px;color:#969385;margin-top:3px}}</style>
<h1>code samples</h1>
<div class="wrap">{cols}</div>
<h1>color &rarr; tree-sitter category assignment &mdash; click a header to sort</h1>
<table class="leg" id="legtable"><thead><tr><th></th><th onclick="srt(1)">color &#9651;</th><th onclick="srt(2)">hex &#9651;</th><th onclick="srt(3)">category &#9651;</th><th>example</th></tr></thead><tbody>{legend}</tbody></table>
<h1>palette</h1>
{palette}\n<script>let D={{}};function srt(c){{const t=document.querySelector("#legtable tbody");const r=[...t.rows];D[c]=!D[c];r.sort((a,b)=>{{const x=a.cells[c].innerText.trim().toLowerCase(),y=b.cells[c].innerText.trim().toLowerCase();return (x<y?-1:x>y?1:0)*(D[c]?1:-1)}});r.forEach(x=>t.appendChild(x))}}</script>'''
open("/tmp/dupre-canon.html","w").write(html)
print("wrote /tmp/dupre-canon.html")

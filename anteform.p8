pico-8 cartridge // http://www.pico-8.com
version 18
__lua__
-- anteform
-- by feneric



-- a function for logging to an output log file.
-- function logit(entry)
--   printh(entry,'anteform.out')
-- end

-- register json context here
_g={}

-- json parser
-- from: https://gist.github.com/tylerneylon/59f4bcf316be525b30ab
table_delims={['{']="}",['[']="]"}

function match(s,tokens)
  for i=1,#tokens do
    if(s==sub(tokens,i,i)) return true
  end
  return false
end

function skip_delim(wrkstr, pos, delim) -- , err_if_missing)
 if sub(wrkstr,pos,pos)!=delim then
  -- if(err_if_missing) assert'delimiter missing'
  return pos,false
 end
 return pos+1,true
end

function parse_str_val(wrkstr, pos, quoted, val)
  val=val or ''
  -- if pos>#wrkstr then
  --   assert'end of input found while parsing string.'
  -- end
  local c=sub(wrkstr,pos,pos)
  if(quoted and c=='"') return _g[val] or val,pos+1
  if(not quoted and match(c,":,]}")) return _g[val] or val,pos
  return parse_str_val(wrkstr,pos+1,quoted,val..c)
end

function parse_num_val(wrkstr,pos,val)
  val=val or ''
  -- if pos>#wrkstr then
  --   assert'end of input found while parsing string.'
  -- end
  local c=sub(wrkstr,pos,pos)
  if(not tonum(c.."0")) return tonum(val),pos
  return parse_num_val(wrkstr,pos+1,val..c)
end
-- public values and functions.

function json_parse(wrkstr, pos, end_delim)
  pos=pos or 1
  -- if(pos>#wrkstr) assert'reached unexpected end of input.'
  local first=sub(wrkstr,pos,pos)
  if table_delims[first] then
    local obj,key,delim_found={},true,true
    pos+=1
    while true do
      key,pos=json_parse(wrkstr, pos, table_delims[first])
      if(key==nil) return obj,pos
      -- if not delim_found then assert'comma missing between table items.' end
      if first=="{" then
        pos=skip_delim(wrkstr,pos,':',true)  -- true -> error if missing.
        obj[key],pos=json_parse(wrkstr,pos)
      else
        add(obj,key)
      end
      pos,delim_found=skip_delim(wrkstr, pos, ',')
  end
  elseif first=='"' then
    -- parse a string (or a reference to a global object)
    return parse_str_val(wrkstr,pos+1,true)
  elseif tonum(first.."0") then
    -- parse a number.
    return parse_num_val(wrkstr, pos)
  elseif first==end_delim then  -- end of an object or array.
    return nil,pos+1
  else
    -- parse true, false
    for lit_str,lit_val in pairs({['true']=true,['false']=false}) do
      local lit_end=pos+#lit_str-1
      if (sub(wrkstr,pos,lit_end)==lit_str) return lit_val,lit_end+1
    end
    -- parse unqoted string, invalid json but saves source bytes
    return parse_str_val(wrkstr,pos,false)
    -- assert'invalid json token'
  end
end

-- initialization data
fullheight,fullwidth,halfheight,halfwidth=11,13,5,6

-- set up the various messages
winmsg="\n\n\n\n\n\n  congratulations, you've won!\n\n\n\n\n\n\n\n\n\n    press p for game menu,\n anything else to continue and\n      explore a bit more."
losemsg="\n\n\n\n\n\n      you've been killed!\n          you lose!\n\n\n\n\n\n\n\n\n\n\n\n    press p for game menu"
helpmsg="anteform commands:\n\na: attack\nc: concentration action\nd: dialog, talk, buy\ne: enter, board, mount, climb,\n   descend\np: pause, save, load, help\nf: use flashlight; force chest\ns: sit & wait\nw: wearing & wielding\nx: examine, look (repeat to\n   search or read)\n\nfor commands with options (like\nconcentrating or buying) use the\nfirst character from the list,\nor anything else to cancel."
msg=helpmsg

-- anyobj is our root object. all others inherit from it to
-- save space and reduce redundancy.
anyobj={f=1,mva=0,nm=0,mvp=0,hd=0,ch=0,z=0}

function makemetaobj(base,basetype)
  return setmetatable(base,{__index=base.ot or basetype})
end

-- the types of terrain that exist in the game. each is
-- given a name and a list of allowed monster types.
terrains=json_parse'[plains,bare ground,hills,scrub,swamp,forest,foothills,mountains,tall mountain,filing cabinet,bed,water,water,deep water,deep water,bridge,brick road,brick,mismatched brick,stone,stone,road,barred window,window,brick,ladder down,ladder up,door,locked door,open door,sign,crater,cave,facility,monastery,cabin,village,helipad,fountain,chair,desk]'
-- counter terrain types are special as they can be talked
-- over (essential for purchasing). there are a lot of them
-- as all the letters are represented.
for num=1,31 do
  add(terrains,"counter")
end

terrainmonsters={}
for num=1,38 do
  add(terrainmonsters,{})
end

-- basetypes are the objects we mean to use to make objects.
-- they inherit (often indirectly) from our root object.
basetypes=json_parse('[{gp:0,hp:10,ch:1,dmg:13,t:[1,2,3,4,5,6,7,8,10,16,17,26,27,30,31,32,38,40,41],hos:1,ar:1,dex:8,exp:2,mva:1},{mny:0,newm:0,mxm:0,mxy:64,fri:1,mxx:128,mn:0,mnx:80},{sf:1,fri:false,dg:1,mnx:1,mxm:27,mxy:9,newm:25,mxx:9,mny:1,sy:1,sz:1,mn:0,sx:1},{i:70,p:1,ia:70,n:boat,fm:1,f:2},{i:94,p:1,ia:94,n:chest,szm:11,shm:-2},{i:39,p:1,iseq:12,fi:1,n:fountain},{i:27,p:1,ia:27,n:ladder up,szm:20,shm:12},{i:26,p:1,ia:26,n:ladder down,szm:20,shm:-3},{i:80,ar:0,exp:1,gp:10,hos:false,t:[1,2,3,4,11,17,22,30,40]},{i:104,hp:23,ch:0,dmg:18,t:[1,2,3,4,5,6,7,8,9,10,16,17,22,26,27,30,31,32,38,40,41],ar:3,po:1,d:[chirp chirp!],exp:9},{i:102,gp:10,d:[ahhrg!],ch:0,dmg:18,n:zombie,exp:8,t:[1,2,3,4,5,6,7,8,9,10,16,17,22,26,27,30,31,32,38,40,41]},{exp:5,ch:2},{i:82,cs:[{},[[4,5],[15,4]]],n:hunter,d:[the woods are scary now.,i\'m safer at home.]},{i:90,ar:12,hp:85,dmg:60,cs:[{},[[15,4]]],n:cop,d:[thanks for your help.,this is beyond my ability.]},{i:77,fi:1,n:merchant,cs:[{},[[1,4],[4,15],[6,1],[14,13]],[[1,4],[6,5],[14,10]],[[1,4],[4,15],[6,1],[14,3]]]},{i:81,fi:1,n:lady,cs:[{},[[2,9],[4,15],[13,14]],[[2,10],[4,15],[13,9]],[[2,11],[13,3]]]},{i:92,n:scientist,cs:[{},[[6,12],[15,4]],[[6,12]],[[15,4]]]},{i:78,n:sunbather,fi:1,cs:[{},[[8,12],[15,4]],[[8,12]],[[15,4]],[[8,14]]],d:[i saw the weirdo!,i\'m glad we\'ve got locks.]},{i:79,cs:[{},[[8,12],[15,4]],[[8,12]],[[15,4]]],fi:1,n:bodybuilder,d:[weird stuff up north.,we\'re vacationing indoors.]},{i:84,n:monk,d:[you are welcome here.,though we are troubled.],ac:[{},[[2,5],[15,4]],[[2,4]],[2,5]]},{i:86,n:student,cs:[{},[[15,4]],[[1,2],[15,4]],[[1,2]]]},{i:75,n:child,cs:[{},[[15,4]],[[11,14],[3,8],[15,4]],[[11,14],[3,8]]],d:[the animals aren\'t right.,mom says stay inside.]},{i:88,cs:[{},[[1,5],[8,2],[4,1],[2,12],[15,4]]],n:citizen},{mch:food,n:grocer},{mch:armor,n:clerk},{mch:weapons,n:vendor},{mch:hospital,n:medic},{mch:guild,n:dealer},{i:81,n:bartender,mch:bar},{i:100},{},{n:woods weirdo,ch:-1,t:[6]},{n:worker ant},{i:110,n:winged ant,t:[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,22,25,26,27,30,31,32,38,40,41]},{i:118,hp:64,dmg:23,n:soldier ant,exp:12},{i:125,n:queen ant,hp:235,dmg:42},{i:123,exp:5,mva:0,hp:10,dmg:5,fi:1,n:ant larva,t:[22]},{i:124,n:ant eggs,hp:5,ia:124},{i:106,gp:8,hp:5,po:1,exp:4,n:large spider},{i:108,gp:2,hp:4,dmg:9,t:[1,2,3,4,5,6,7,8,10,16,17,22,26,27,30,31,32,38,40,41],po:1,n:large rat,exp:2,eat:1},{i:96,n:coyote,ch:3,d:[grrr!]},{i:120,n:lynx,ch:3,d:[grar!]},{i:112,hp:7,po:1,dmg:9,ch:1,ns:[snake,serpent],t:[4,5,6,7]},{i:114,n:rattlesnake,exp:6,po:1},{i:116,hp:20,exp:7,n:large eel,t:[5,12,13,14,15,16]},{i:95,hp:8,po:1,fi:1,ch:1,n:scorpion},{i:122,gp:10,eat:1,fi:1,exp:2,cs:[{},[[3,9],[11,10]],[[3,14],[11,15]]],n:slime,t:[22,23]},{i:98,hp:25,exp:9,ns:[big catfish,sturgeon],t:[12,13,14,15,16]}]')
shiptype=basetypes[4]

-- set our base objects base values. the latter portion is
-- our bestiary. it holds all the different monster types that can
-- be epython ordereddictncountered in the game. it builds off of the basic types
-- already defined so most do not need many changes. actual
-- monsters in the game are instances of creatures found in the
-- bestiary.
for basetypenum=1,#basetypes do
  local basetype
  local objecttype=basetypes[basetypenum]
  if basetypenum<9 then
    basetype=anyobj
  elseif basetypenum<13 then
    basetype=basetypes[1]
  elseif basetypenum<24 then
    basetype=basetypes[9]
  elseif basetypenum<30 then
    basetype=basetypes[15]
  elseif basetypenum<33 then
    basetype=basetypes[11]
  elseif basetypenum<39 then
    basetype=basetypes[10]
  else
    basetype=basetypes[12]
  end
  objecttype.id=basetypenum
  makemetaobj(objecttype,basetype)
  if basetypenum>29 then
    for terrain in all(objecttype.t) do
      add(terrainmonsters[terrain],objecttype)
    end
  end
end

-- check to see whether or not a desired purchase can
-- be made.
function checkpurchase(prompt,checkfunc,purchasefunc)
  update_lines(prompt)
  cmd=yield()
  desireditem=checkfunc(cmd)
  return desireditem and purchasefunc(desireditem) or desireditem==false and "you cannot afford that." or "no sale."
end

function purchasedetail(desireditem)
  if desireditem and desireditem.p then
    return hero.gp>=desireditem.p and desireditem
  else
    return nil
  end
end

-- makes a purchase if all is in order.
function purchase(prompt,itemtype,attribute)
  return checkpurchase(prompt,
    function(cmd)
      return purchasedetail(itemtype[cmd])
    end,
    function(desireditem)
      if hero[attribute]>=desireditem.a then
        return "that is not an upgrade."
      else
        hero.gp-=desireditem.p
        hero[attribute]=desireditem.a
        return "the "..desireditem.n.." is yours."
      end
    end
  )
end

-- a table of functions to perform the different merchant
-- operations. while there is a lot in common between them,
-- there are a lot of differences, too.
shop={
  food=function()
    return checkpurchase({"$12 for 25 food; a\80\80\82\79\86\69? "},
      function(cmd)
        return cmd=='a' and hero.gp>=12 or nil
      end,
      function()
        hero.gp-=12
        hero.fd=not_over_32767(hero.fd+25)
        return "you got more food."
      end
    )
  end,
  armor=function()
    return purchase({"buy \131cloth $12, \139leather $99,", "or \145flak $300: "},armors,'ar')
  end,
  weapons=function()
    return purchase({"buy d\65\71\71\69\82 $8, c\76\85\66 $40,","or a\88\69 $75: "},weapons,'dmg')
  end,
  hospital=function()
    return checkpurchase({"choose f\73\82\83\84 \65\73\68 ($8), c\85\82\69","($10), or m\69\68\73\67 ($25): "},
      function(cmd)
        desiredspell=spells[cmd]
        return purchasedetail(desiredspell)
      end,
      function(desiredspell)
        sfx(3)
        hero.gp-=desiredspell.p
        if desiredspell.n=='cure' then
          -- perform cure
          hero.st=band(hero.st,14)
        else
          -- perform healing
          increasehp(desiredspell.a)
        end
        return desiredspell.n.." is performed!"
      end
    )
  end,
  bar=function()
    return checkpurchase({"$5 per drink; a\80\80\82\79\86\69? "},
      function(cmd)
        return cmd=='a' and hero.gp>=5 or nil
      end,
      function()
        hero.gp-=5
        rumors=json_parse('[i think they\'re aliens.,funny citronella smell.,they smell like lemons.,they act like zombies.,they look burned.,the cult knows something.,they spook the animals.]')
        update_lines{"while socializing, you hear:"}
        return '"'..rumors[flr(rnd(7)+1)]..'"'
      end
    )
  end,
  guild=function()
    return checkpurchase({"4 \139batts $12 or a \145key $23: "},
      function(cmd)
        local desiredtool=tools[cmd]
        return purchasedetail(desiredtool)
      end,
      function(desireditem)
        hero.gp-=desireditem.p
        hero[desireditem.attr]+=desireditem.q
        return "you purchase "..desireditem.n
      end
    )
  end
}

-- add numerical references to names by amounts
function makenameforamount(itemtype)
  nameforamount={}
  for itemcmd,item in pairs(itemtype) do
    nameforamount[item.a]=item.n
  end
  nameforamount[0]='none'
  return nameforamount
end

-- armor definitions
armors=json_parse('{south:{n:cloth,a:8,p:12},west:{n:leather,a:23,p:99},east:{n:flak,a:40,p:300},north:{n:vest,a:70}}')
armornames=makenameforamount(armors)

-- weapon definitions
weapons=json_parse('{d:{n:dagger,a:8,p:8},c:{n:club,a:12,p:40},a:{n:axe,a:18,p:75},s:{n:shotgun,a:40}}')
weaponnames=makenameforamount(weapons)

-- spell definitions
spells=json_parse('{a:{n:aim,c:3,a:1},f:{n:first aid,c:5,a:1,p:8},c:{n:cure,c:7,p:10},x:{n:medic,a:6,p:25}}')

-- tool definitions
tools=json_parse('{west:{n:"4 batteries",attr:ts,p:12,q:4},east:{n:a key,attr:keys,p:23,q:1}}')

function setmap()
  local songstrt=curmap.ss
  curmap=maps[mapnum]
  contents=curmap.con
  if(songstrt and curmap.ss)music(curmap.ss)
  hero.hd=0
end

function initobjs()
  -- the maps structure holds information about all of the regular
  -- places in the game, dungeons as well as towns.
  maps=json_parse('[{mny:24,sn:[{x:107,msg:[an engagement ring; steve,was ready to propose.],y:27},{x:102,msg:[mary studied astronomy,as a hobby.],y:33},{x:107,msg:[lou was a mixed martial,arts champion.],y:33}],n:village,mxx:112,c:[{x:91,y:27,id:26},{pn:fred,y:30,x:89,id:13},{x:85,y:27,id:25},{x:100,y:44,id:24},{x:108,y:37,id:29},{x:87,d:[welcome to anteform valley.,we\'re all glad you\'re here.],y:37,id:16},{pn:anne,y:51,x:86,id:22},{x:80,y:52,pn:flip,d:[greybeard hid his treasure!,it\'s on big sister island!],id:22},{x:109,y:40,pn:gwen,d:["steve, lou, & mary are gone.",i\'m not straying far.],id:23},{x:98,y:47,pn:ralph,d:[radio square is southwest.,folks are missing there too.],id:23},{x:108,y:30,pn:sally,d:[please find steve.,our rooms are adjacent.],id:21},{pn:bruce,y:37,x:105,id:14}],mxy:56,i:[{x:103,y:38,id:6}],ex:57,sy:54,sx:96,ey:37},{sn:[{x:120,msg:[a myrmecology paper by,dr. greene.],y:6},{x:105,msg:[a paper on irregular growth,in animals.],y:11},{x:120,msg:[data on valley insect,populations.],y:11}],n:thinktank,c:[{x:116,mva:0,y:18,pn:artemis,d:[spooky stuff\'s afoot.,missing people; crazy animals.],id:14},{x:110,y:17,id:24},{x:106,y:1,id:27},{x:111,y:7,pn:dr. wong,d:[concentrated it can burn.,a simple carboxyl.],id:17},{x:120,y:20,pn:dr. tetrado,d:[find dr. tucker.,he\'s figured it out.],id:17},{x:110,y:22,pn:dr. greene,d:[several of us are missing.,those researching up north.],id:17}],i:[{x:123,tm:3,y:4,tz:0,tx:126,ty:33,id:8}],mxy:24,mnx:104,ex:26,sy:23,sx:116,ey:33},{mny:32,sn:[{x:123,msg:[a paper on insect,pheromones.],y:33},{x:124,msg:[a paper on formic acid,& its effects.],y:33},{x:123,msg:[a paper on ants controlling,aphids.],y:35},{x:113,msg:[a diagram of modified aphid,brains.],y:33}],n:the basement,c:[{x:115,y:34,pn:dr. tucker,d:[it\'s semiochemicals.,controlled living corpses.],id:17},{x:119,y:33,pn:dr. agawon,d:[they\'re literally brain dead.,higher functions burnt out.],id:17}],mnx:112,mxy:43,sy:33,sx:126,i:[{x:126,tm:2,y:33,tz:0,tx:123,ty:4,id:7}]},{sn:[{x:92,msg:he\'ll rise again!,y:20},{x:100,msg:[a secret prophesy about the,eschaton starting here.],y:9}],n:monastery,mxx:105,c:[{pn:bro. meinrad,y:21,x:89,id:20},{x:85,y:15,id:27},{x:99,y:15,id:24},{x:92,y:1,pn:sis. pat,d:[i saw the flash in heaven.,the animals now punish us.],id:20},{x:82,y:5,pn:learner jo,d:[i found the star jelly.,i think it turned the beasts.],id:21},{x:90,y:6,pn:sis. gail,d:[god sent us a sign.,i saw his star fall to earth.],id:20}],mxy:24,i:[{x:84,tm:5,y:9,tz:0,tx:113,ty:31,id:7},{x:92,y:6,id:6}],ex:34,sy:23,sx:92,ey:17},{mny:24,sn:[{x:126,msg:[a list of the missing;,many monks & nuns are gone.],y:29}],n:the top floor,c:[{x:117,y:27,pn:bro. stamos,d:[we know of your quest.,we will help as we can.],id:20},{x:125,y:30,pn:mother francine,d:[some of us were taken.,they are now possessed.],id:20},{x:126,y:26,pn:father ted,d:[our dead brothers & sisters.,they are beset by demons.],id:20}],mnx:112,mxy:33,sy:31,sx:113,i:[{x:113,tm:4,y:31,tz:0,tx:84,ty:9,id:8}]},{mny:56,sn:[{x:105,msg:[no one\'s been here in awhile.,seems the hermit\'s missing too.],y:61}],n:hermit cabin,mxx:113,i:[{x:110,y:61,id:4}],mnx:103,ex:3,sy:57,sx:111,ey:47},{mny:43,sn:[{x:114,msg:[the dj was investigating,the monks for the news.],y:53},{x:114,msg:[some expired coupons & a,copy of \'coyote waits\'.],y:56},{x:114,msg:[a zombie comic book; someone,drew a robe on the zombie.],y:59}],n:radio square,c:[{x:114,y:45,id:26},{x:123,y:60,id:24},{x:123,y:44,id:28},{x:126,y:46,pn:becky,d:[the hermit has a boat.,west past the billabong.],id:13},{x:114,y:48,pn:jack,d:[it\'s in the n.e. cabin.,you can borrow my shotgun.],id:13},{x:121,y:53,pn:dj jazzy joe,d:[the monks have seen them.,the woods weirdos.],i:80,id:15},{x:124,y:58,pn:emma,d:[hang out in the bar none.,good woods weirdos discussion.],id:23}],mnx:112,ex:34,sy:62,sx:119,ey:53},{mny:56,n:southern cabin,mxx:104,c:[{x:101,y:59,pn:sue,d:[you can borrow my vest.,you\'ll need it.],id:13}],mnx:96,ex:38,sy:62,sx:100,ey:60},{mny:56,n:pennisula cabin,mxx:104,c:[{x:99,y:58,pn:jim,d:[weird stuff up north.,we\'re vacationing indoors.],id:19},{x:98,y:58,pn:daisy,d:[i saw the weirdo!,i\'m glad we\'ve got locks.],id:18}],mnx:96,ex:56,sy:62,sx:100,ey:24},{mny:56,n:lakeside cabin,mxx:104,c:[{x:98,y:58,pn:jane,d:[find my friend to the south.,she\'ll help.],id:18}],mnx:96,ex:41,sy:62,sx:100,ey:39},{mny:56,sn:[{x:101,msg:[a sketch of an ant,moving a rock.],y:60}],n:western cabin,mxx:104,mnx:96,ex:21,sy:62,sx:100,ey:28},{mny:56,mxx:104,sx:100,mnx:96,ex:75,sy:62,n:hunting cabin,ey:3},{mny:56,mxm:15,n:the queen\'s chamber,mxx:96,ss:14,c:[{x:83,y:60,id:36},{x:93,y:58,id:37},{x:89,y:57,id:35},{x:89,y:62,id:35},{x:94,y:57,id:35}],i:[{x:94,tm:16,y:62,tz:1,tx:3,ty:6,id:7},{x:83,y:59,id:38},{x:84,y:59,id:38},{x:84,y:60,id:38}],mnx:80,newm:26,sy:31,sx:113,fri:false},{l:[[0,-196,782,13263,15564,12288,16380,16384],[2,-12481,961,12348,16332,12,-3124,192],[12301,13260,192,15612,12348,13056,-3076,192]],i:[{x:1,y:8,z:1,id:7},{x:8,y:3,z:2,id:7},{x:8,y:1,z:3,id:7},{x:4,y:8,z:3,id:5}],ex:57,sy:8,n:greybeard\'s cave,ey:33},{l:[[205,15360,2876,16320,12351,-3328,1020,-19711],[48,16128,14332,768,-244,780,13119,28672]],n:vetusaur mine,ss:14,c:[{x:7,z:1,y:8,ch:-2,id:33}],i:[{x:8,y:8,z:1,id:7},{x:8,tm:0,z:1,y:1,tz:0,tx:3,ty:5,id:7},{x:3,y:3,z:2,id:7},{x:1,y:8,z:2,id:7}],ex:3,sy:8,sx:8,ey:9},{ss:14,sx:4,l:[[204,12480,13308,12300,16332,14348,16380,256]],i:[{x:4,y:8,z:1,id:7},{x:3,tm:13,z:1,y:6,tz:0,tx:94,ty:62,id:8}],ex:7,sy:8,n:formika mine,ey:3}]')
  -- map 0 is special; it's the world map, the overview map.
  maps[0]=json_parse('{n:anteform valley,mnx:0,mny:0,mxx:80,mxy:64,newm:10,mxm:11,fri:false,ss:0,sn:[{x:64,y:43,msg:nw village},{x:68,y:40,msg:n monastery},{x:68,y:40,msg:w thinktank},{x:58,y:21,msg:w monastery},{x:24,y:3,msg:[a meteorite hit here. it\'s,corrupted the water.]}]}')

  -- the creatures structure holds the live copy saying which
  -- creatures (both human and monster) are where in the world.
  -- individually they are instances of bestiary objects or
  -- occupation type objects.
  creatures={}

  -- perform the per-map data structure initializations.
  for mapnum=0,#maps do
    curmap=maps[mapnum]
    if mapnum>0 then
      makemetaobj(curmap,basetypes[curmap.l and 3 or 2])
    end
    curmap.w,curmap.h=curmap.mxx-curmap.mnx,curmap.mxy-curmap.mny
    creatures[mapnum],curmap.con={},{}
    for num=curmap.mnx-1,curmap.mxx+1 do
      curmap.con[num]={}
      for inner=curmap.mny-1,curmap.mxy+1 do
        curmap.con[num][inner]={}
      end
    end
    for item in all(curmap.i) do
      item.ot,xcoord,ycoord,zcoord=basetypes[item.id],item.x,item.y,item.z or 0
      curmap.con[xcoord][ycoord][zcoord]=makemetaobj(item)
      -- automatically make a corresponding ladder down for every ladder up
      if item.ot.n=='ladder up' and curmap.dg then
        zcoord-=1
        curmap.con[xcoord][ycoord][zcoord]=makemetaobj{ot=basetypes[8]}
      end
    end
    for creature in all(curmap.c) do
      creature.mn=mapnum
      creature.ot=basetypes[creature.id]
      definemonster(creature)
    end
  end

  -- the hero is the player character. although human, it has
  -- enough differences that there is no advantage to inheriting
  -- the human type.
  hero=json_parse('{i:0,ar:0,dmg:0,x:67,y:50,z:0,exp:0,lvl:0,str:8,int:8,dex:8,st:0,hd:0,f:0,gp:20,fd:45,mvp:0,mp:8,hp:24,keys:3,ts:2,lit:0}')
  hero.color=rnd(10)>6 and 4 or 15

  -- make the map info global for efficiency
  mapnum=0
  setmap()

  boatx,boaty,phase,cycle,turn,turnmade=0,0,0,0,0,false
  _update,_draw=world_update,world_draw
  draw_state=world_draw
end

-- the lines list holds the text output displayed on the screen.
lines={"","","","",">"}
numoflines=5
curline=numoflines

-- initialization routines

function _init()
  initobjs()
  menuitem(1,"list commands",listcommands)
  menuitem(2,"save game",savegame)
  menuitem(3,"load game",loadgame)
  menuitem(4,"new game",run)
  processinput=cocreate(inputprocessor)
  cartdata("anteform0")
end

function listcommands()
  msg=helpmsg
  if _draw~=msg_draw then
    draw_state=_draw
    _draw=msg_draw
  end
end

attrlist=json_parse'[ar,dmg,x,y,str,int,dex,st,i,color,f,keys,ts,exp,lvl,gp,fd,mp,hp]'

function savegame()
  if mapnum>0 then
    update_lines{"sorry, only outside."}
  else
    local storagenum=0
    for heroattr in all(attrlist) do
      dset(storagenum,hero[heroattr])
      storagenum+=1
    end
    if hero.i>1 then
      boatx,boaty=0,0
    end
    dset(storagenum,combinevalues(boatx*256+boaty))
    dset(storagenum+1,phase)
    storagenum+=2
    for creaturenum=1,11 do
      local creature=creatures[0][creaturenum]
      if creature then
        dset(storagenum,creature.id)
        dset(storagenum+1,combinevalues(creature.x*256+creature.y))
      else
        dset(storagenum,0)
      end
      storagenum+=2
    end
    update_lines{"game saved."}
  end
end

function separatevalues(comboval)
  return lshr(band(comboval,0xff00),8),band(comboval,0xff)
end

function loadgame()
  initobjs()
  local storagenum=0
  for heroattr in all(attrlist) do
    hero[heroattr]=dget(storagenum)
    storagenum+=1
  end
  boatx,boaty=separatevalues(dget(storagenum))
  phase=dget(storagenum+1)
  if boatx>0 or boaty>0 then
    maps[0].con[boatx][boaty][0]=makemetaobj{ot=basetypes[4]}
  end
  storagenum+=2
  for creaturenum=1,11 do
    creatureid=dget(storagenum)
    if (creatureid==0) break
    creaturex,creaturey=separatevalues(dget(storagenum+1))
    definemonster{ot=basetypes[creatureid],x=creaturex,y=creaturey,mn=0}
    storagenum+=2
  end
  update_lines{"game loaded."}
end

buttons={
  "west", "east", "north", "south",
  "c", "x", "p", "?", "s", "f", "e", "d", "w", "a"
}

function getbutton(btnpress)
  local bitcount=1
  while btnpress>1 do
    btnpress=lshr(btnpress,1)
    bitcount+=1
  end
  return buttons[bitcount] or 'none'
end

function checkspell(cmd,extra)
  local spell=spells[cmd]
  if hero.mp>=spell.c then
    hero.mp-=spell.c
    update_lines{spell.n.." performed! "..(extra or '')}
    return true
  else
    update_lines{"not enough ap."}
    return false
  end
end

function exitdungeon(targx,targy,targmapnum)
  hero.x,hero.y,hero.z,hero.f,hero.lit,mapnum=targx or curmap.ex,targy or curmap.ey,0,0,0,targmapnum or curmap.mn
  setmap()
  _draw=world_draw
end

function entermap(targmap,targmapnum,targx,targy,targz)
  hero.x,hero.y=targx or targmap.sx,targy or targmap.sy
  mapnum=targmapnum
  setmap()
  if (targmap.dg) _draw,hero.f,hero.z=dungeon_draw,targmap.sf,targmap.sz
  return "entering "..targmap.n.."."
end

function inputprocessor(cmd)
  while true do
    local spots=calculatemoves(hero)
    local xcoord,ycoord,zcoord=hero.x,hero.y,hero.z
    local curobj=contents[xcoord][ycoord][zcoord]
    local curobjname=curobj and curobj.n or nil
    if _draw==msg_draw then
      if cmd!='p' and hero.hp>0 then
        _draw=draw_state
      end
    elseif cmd=='west' then
      if curmap.dg then
        hero.f-=1
        if hero.f<1 then
          hero.f=4
        end
        update_lines{"turn left"}
        turnmade=true
      else
        hero.x,hero.y=checkmove(spots[2],ycoord,cmd)
      end
    elseif cmd=='east' then
      if curmap.dg then
        hero.f+=1
        if hero.f>4 then
          hero.f=1
        end
        update_lines{"turn right."}
        turnmade=true
      else
        hero.x,hero.y=checkmove(spots[4],ycoord,cmd)
      end
    elseif cmd=='north' then
      if curmap.dg then
        hero.x,hero.y,hero.z=checkdungeonmove(1)
      else
        hero.x,hero.y=checkmove(xcoord,spots[1],cmd)
      end
    elseif cmd=='south' then
      if curmap.dg then
        hero.x,hero.y,hero.z=checkdungeonmove(-1)
      else
        hero.x,hero.y=checkmove(xcoord,spots[3],cmd)
      end
    elseif cmd=='c' then
      update_lines{"choose a\73\77, f\73\82\83\84 \65\73\68, or","c\85\82\69: "}
      cmd=yield()
      if cmd=='c' then
        -- cast cure
        if checkspell(cmd) then
          sfx(3)
          hero.st=band(hero.st,14)
        end
      elseif cmd=='f' then
        -- cast healing
        if checkspell(cmd) then
          sfx(3)
          increasehp(spells[cmd].a*hero.int)
        end
      elseif cmd=='a' then
        -- cast offensive spell
        if checkspell(cmd,'dir:') then
          local spelldamage=rnd(spells[cmd].a*hero.int)
          if not getdirection(spots,attack_results,spelldamage) then
            update_lines{'nothing to target.'}
          end
        end
      else
        update_lines{"command: huh?"}
      end
      turnmade=true
    elseif cmd=='x' then
      update_lines{"examine dir:"}
      if not getdirection(spots,look_results) then
        if cmd=='x' then
          local response={"search","you find nothing."}
          signcontents=check_sign(xcoord,ycoord)
          if signcontents then
            response=signcontents
          elseif xcoord==99 and ycoord==58 and mapnum==12 and hero.dmg<40 then
           -- search response
           response[2]="you find a shotgun!"
           hero.dmg=40
          elseif xcoord==101 and ycoord==59 and mapnum==8 and hero.ar<70 then
           -- search response
           response[2]="you find a bulletproof vest!"
           hero.ar=70
          end
          update_lines(response)
        else
          update_lines{"examine: huh?"}
        end
      end
      turnmade=true
    elseif cmd=='p' then
      update_lines{"pause / game menu"}
    elseif cmd=='s' then
      turnmade=true
      update_lines{"sit and wait."}
    elseif cmd=='f' then
      turnmade=true
      if curobjname=='chest' then
        hero.gp+=500
        update_lines{"you find $500."}
        contents[xcoord][ycoord][zcoord]=nil
      elseif curmap.dg and hero.lit<1 then
        if hero.ts>1 then
          hero.lit=80
          hero.ts-=1
          update_lines{"the flashlight is lit."}
        else
          update_lines{"you have no batteries."}
        end
      else
        update_lines{"nothing here."}
      end
    elseif cmd=='e' then
      turnmade=true
      local msg="nothing to enter."
      if curobjname=='ladder up' or curobjname=='ladder down' then
        if curmap.dg then
          if zcoord==curmap.sz and xcoord==curmap.sx and ycoord==curmap.sy then
            msg="exiting "..curmap.n.."."
            exitdungeon()
          elseif curobjname=='ladder up' then
            msg="ascending."
            hero.z-=1
          else
            msg="descending."
            hero.z+=1
          end
        end
        if curobj.tm then
          if curmap.dg then
            exitdungeon(curobj.tx,curobj.ty,curobj.tm)
          else
            msg=entermap(maps[curobj.tm],curobj.tm,curobj.tx,curobj.ty,curobj.tz)
          end
        end
      elseif hero.i>0 then
        msg="exiting sailboat."
        contents[xcoord][ycoord][zcoord]=makemetaobj{f=hero.f,ot=shiptype}
        hero.i,hero.f=0,0
        boatx,boaty=hero.x,hero.y
      elseif curobjname=='boat' then
        msg="boarding sailboat."
        hero.i,hero.f=70,curobj.f
        contents[xcoord][ycoord][zcoord]=nil
      end
      for loopmapnum=1,#maps do
        local loopmap=maps[loopmapnum]
        if mapnum==loopmap.mn and xcoord==loopmap.ex and ycoord==loopmap.ey then
          --relock cabin doors to preserve mimesis
          mset(99,61,29)
          msg=entermap(loopmap,loopmapnum)
        elseif xcoord==3 and ycoord==5 then
          msg=entermap(maps[15],15,8,1,1)
        end
      end
      update_lines{msg}
    elseif cmd=='d' then
      update_lines{"dialog dir:"}
      if not getdirection(spots,dialog_results) then
        update_lines{"dialog: huh?"}
      end
      turnmade=true
    elseif cmd=='w' then
      update_lines{
        "worn: "..armornames[hero.ar].."; wield: "..weaponnames[hero.dmg],
        hero.ts..' batteries & '..hero.keys..' skeleton keys.'
      }
    elseif cmd=='a' then
      update_lines{"attack dir:"}
      if not getdirection(spots,attack_results) then
        update_lines{"attack: huh?"}
      end
      turnmade=true
    end
    if hero.lit>1 then
      hero.lit-=1
      if hero.lit<1 then
        update_lines{"battery died!"}
      end
    end
    if _draw==dungeon_draw and hero.lit<1 then
      update_lines{"it's dark!"}
    end
    cmd=yield()
    if phase==0 then
      phase=1
      definemonster{ot=basetypes[32],x=31,y=4,mn=0}
    end
  end
end

function getdirection(spots,resultfunc,magic,adir)
  if curmap.dg then
    adir=dngdirections[hero.f]
  elseif not adir then
    adir=yield()
  end
  if adir=='east' then
    resultfunc(adir,spots[4],hero.y,magic)
  elseif adir=='west' then
    resultfunc(adir,spots[2],hero.y,magic)
  elseif adir=='north' then
    resultfunc(adir,hero.x,spots[1],magic)
  elseif adir=='south' then
    resultfunc(adir,hero.x,spots[3],magic)
  else
    return false
  end
  return true
end

function update_lines(msg)
  local prompt="> "
  for curmsg in all(msg) do
    lines[curline]=prompt..curmsg
    curline+=1
    if(curline>numoflines)curline=1
    prompt=""
  end
  lines[curline]=">"
end

function definemonster(monster)
  local objtype,xcoord,ycoord,zcoord=monster.ot,monster.x,monster.y,monster.z or 0
  monster.ot=objtype
  makemetaobj(monster)
  if objtype.ns then
    monster.n=objtype.ns[flr(rnd(#objtype.ns)+1)]
  end
  --if(objtype.is)monster.i=objtype.is[flr(rnd(#objtype.is)+1)]
  if(objtype.cs)monster.co=objtype.cs[flr(rnd(#objtype.cs)+1)]
  monster.iseq,monster.ia=flr(rnd(30)),false
  add(creatures[monster.mn],monster)
  maps[monster.mn].con[xcoord][ycoord][zcoord]=monster
  return monster
end

function create_monster()
  local monsterx,monstery,monsterz=flr(rnd(curmap.w))+curmap.mnx,flr(rnd(curmap.h))+curmap.mny,curmap.dg and flr(rnd(#curmap.l)+1) or 0
  if contents[monsterx][monstery][monsterz] or monsterx==hero.x and monstery==hero.y and monsterz==hero.z then
    -- don't create a monster where there already is one
    monsterx=nil
  end
  if monsterx then
    local monsterspot=mget(monsterx,monstery)
    if (curmap.dg) monsterspot=getdungeonblk(monsterx,monstery,monsterz,1)
    for objtype in all(terrainmonsters[monsterspot]) do
      if rnd(200)<objtype.ch then
        definemonster{ot=objtype,x=monsterx,y=monstery,z=monsterz,mn=mapnum}
        break
      end
    end
  end
end

function deducthp(damage)
  hero.hp-=ceil(damage)
  if (hero.hp<=0) msg,_draw=losemsg,msg_draw -- draw_state=_draw
end

function deductfood(amount)
  hero.fd-=amount
  if hero.fd<=0 then
    sfx(1,3,8)
    hero.fd=0
    update_lines{"starving!"}
    deducthp(1)
  end
end

function not_over_32767(num)
  return num<-16383 and 32767 or num
end

function increasexp(amount)
  hero.exp=not_over_32767(hero.exp+amount)
  if hero.exp>=hero.lvl^2*10 then
    hero.lvl+=1
    increasehp(12)
    update_lines{"you went up a level!"}
  end
end

function increasehp(amount)
  hero.hp=not_over_32767(min(hero.hp+amount,hero.str*(hero.lvl+3)))
end

-- world updates

function checkdungeonmove(direction)
  local newx,newy,newz=hero.x,hero.y,hero.z
  local cmd=direction>0 and 'advance' or 'retreat'
  local item
  local iscreature=false
  if hero.f==1 then
    newy-=direction
  elseif hero.f==2 then
    newx+=direction
  elseif hero.f==3 then
    newy+=direction
  else
    newx-=direction
  end
  result=getdungeonblk(newx,newy,newz)
  item=contents[xcoord][newy][newz]
  if item and item.hp then
    iscreature=true
  end
  if result==3 or iscreature then
    blocked(cmd)
    newx,newy=hero.x,hero.y
  else
    sfx(0)
    update_lines{cmd}
  end
  turnmade=true
  return newx,newy,newz
end

function checkexit(xcoord,ycoord)
  if xcoord>=curmap.mxx or xcoord<curmap.mnx or ycoord>=curmap.mxy or ycoord<curmap.mny then
    update_lines{cmd,"exiting "..curmap.n.."."}
    mapnum=0
    return true
  end
end

function blocked(cmd)
  sfx(4)
  update_lines{cmd,"blocked!"}
end

-- this is kind of weird; the ship icons ought to be rearranged
directions={north=1,west=2,south=3,east=4}
dngdirections={"north","east","south","west"}

function checkmove(xcoord,ycoord,cmd)
  local movesuccess=true
  local newloc=mget(xcoord,ycoord)
  local movecost=band(fget(newloc),3)
  local water=fget(newloc,2)
  local impassable=fget(newloc,3)
  local content=contents[xcoord][ycoord][hero.z]
  --update_lines(""..xcoord..","..ycoord.." "..newloc.." "..movecost.." "..fget(newloc))
  if hero.i>0 then
    hero.f=directions[cmd]
    local terraintype=mget(xcoord,ycoord)
    if checkexit(xcoord,ycoord) then
      xcoord,ycoord=curmap.ex,curmap.ey
      setmap()
    elseif content then
      movesuccess=blocked(cmd)
    elseif terraintype<12 or terraintype>16 then
      update_lines{cmd,"must exit boat first."}
      movesuccess=false
    else
      update_lines{cmd}
    end
  else
    if checkexit(xcoord,ycoord) then
      xcoord,ycoord=curmap.ex,curmap.ey
      setmap()
    elseif content then
      if not content.p then
      movesuccess=blocked(cmd)
      end
    elseif newloc==28 then
      update_lines{cmd,"open door."}
      movesuccess=false
      mset(xcoord,ycoord,30)
    elseif newloc==29 then
      if hero.keys>0 then
        update_lines{cmd,"you jimmy the door."}
        hero.keys-=1
        mset(xcoord,ycoord,30)
      else
        update_lines{cmd,"the door is locked."}
      end
      movesuccess=false
    elseif impassable then
      movesuccess=blocked(cmd)
    elseif water then
      movesuccess=false
      update_lines{cmd,"not without a boat."}
    elseif movecost>hero.mvp then
      hero.mvp+=1
      movesuccess=false
      update_lines{cmd,"slow progress."}
    else
      hero.mvp=0
      update_lines{cmd}
    end
  end
  if movesuccess then
    if (hero.i==0) sfx(0)
    if newloc==5 and rnd(10)>6 then
      update_lines{cmd,"poisoned!"}
      hero.st=bor(hero.st,1)
    end
  else
    xcoord,ycoord=hero.x,hero.y
  end
  turnmade=true
  return xcoord,ycoord
end

function check_sign(xcoord,ycoord)
  for sign in all(curmap.sn) do
    if xcoord==sign.x and ycoord==sign.y then
      if mget(xcoord,ycoord)==31 then
        -- it's an actual sign
        return {" (read sign)",sign.msg}
      elseif xcoord==hero.x and ycoord==hero.y then
        -- it's in a desk or filing cabinet or something
        return sign.msg
      end
    end
  end
end

function look_results(ldir,xcoord,ycoord)
  local cmd,signcontents,content="examine: "..ldir,check_sign(xcoord,ycoord),contents[xcoord][ycoord][hero.z] or nil
  if signcontents then
    update_lines{cmd..signcontents[1],signcontents[2]}
  elseif content then
    update_lines{cmd,(content.pn or content.n)}
  elseif curmap.dg then
    update_lines{cmd,getdungeonblk(xcoord,ycoord,hero.z)==0 and 'passage' or 'wall'}
  else
    update_lines{cmd,terrains[mget(xcoord,ycoord)]}
  end
end

function dialog_results(ddir,xcoord,ycoord)
  local cmd="dialog: "..ddir
  if terrains[mget(xcoord,ycoord)]=='counter' then
    return getdirection(calculatemoves({x=xcoord,y=ycoord}),dialog_results,nil,ddir)
  end
  local dialog_target=contents[xcoord][ycoord][hero.z]
  if dialog_target then
    if dialog_target.mch then
      update_lines{shop[dialog_target.mch]()}
    elseif dialog_target.d then
      update_lines{cmd,'"'..dialog_target.d[flr(rnd(#dialog_target.d)+1)]..'"'}
    else
      update_lines{cmd,'no response!'}
    end
  else
    update_lines{cmd,'no one to talk with.'}
  end
end

function attack_results(adir,xcoord,ycoord,magic)
  local cmd="attack: "..adir
  local zcoord,creature=hero.z,contents[xcoord][ycoord][hero.z]
  local damage=flr(rnd(hero.str+hero.lvl+hero.dmg))
  damage+=(magic or 0)
  --if creature then
    --logit('creature: '..(creature.name or 'nil')..' '..(creature.x or 'nil')..','..(creature.y or 'nil')..','..(creature.z or 'nil'))
  --else
    --logit('creature: nil '..xcoord..','..ycoord..' '..adir)
  --end
  if creature and creature.hp then
    articlecreach=(creature.pn or "the "..creature.n)
    if magic or rnd(hero.dex+hero.lvl*8)>rnd(creature.dex+creature.ar) then
      damage-=rnd(creature.ar)
      creature.hd=3
      sfx(1)
      creature.hp-=damage
      if creature.hp<=0 then
        hero.gp=not_over_32767(hero.gp+creature.gp)
        increasexp(creature.exp)
        contents[xcoord][ycoord][zcoord]=nil
        update_lines{cmd,creature.n..' killed; xp+'..creature.exp..' $+'..creature.gp}
        if creature.ch==-1 then
          phase,basetypes[30].ch,basetypes[31].ch=2,3,4
          update_lines{'a zombie?!'}
        elseif creature.ch==-2 then -- vetusaur mine
          update_lines{'an ant bigger than a cow!'}
          phase,basetypes[33].ch,basetypes[34].ch,basetypes[35].ch,basetypes[37].ch,basetypes[30].ch,basetypes[31].ch,basetypes[40].ch=3,7,3,5,2,2,3,0
        elseif creature.i>124 then
          msg=winmsg
          _draw=msg_draw
        end
        del(creatures[mapnum],creature)
      else
        update_lines{cmd,'you hit '..articlecreach..'!'}
      end
      if curmap.fri then
        for townie in all(creatures[mapnum]) do
          townie.hos=1
          townie.d={"criminal!"}
          townie.mva=1
        end
      end
    else
      update_lines{cmd,'you miss '..articlecreach..'!'}
    end
  elseif mget(xcoord,ycoord)==29 then
    -- bash locked door
    sfx(1)
    if(not magic)deducthp(1)
    if rnd(damage)>12 then
      update_lines{cmd,'you break it open!'}
      mset(xcoord,ycoord,30)
    else
      update_lines{cmd,'the door holds.'}
    end
  else
    update_lines{cmd,'nothing to attack.'}
    --logit('hero: '..hero.x..','..hero.y..','..hero.z..' '..hero.f..' target: '..xcoord..','..ycoord..' '..cmd)
  end
end

function squaredistance(x1,y1,x2,y2)
  return abs(x1-x2)+abs(y1-y2)
end

function calculatemoves(creature)
  local maxx,maxy=curmap.mxx,curmap.mxy
  local creaturex,creaturey=creature.x,creature.y
  local eastspot,westspot=(creaturex+curmap.w-1)%maxx,(creaturex+1)%maxx
  local northspot,southspot=(creaturey+curmap.h-1)%maxy,(creaturey+1)%maxy
  northspot,southspot,eastspot,westspot=creaturey-1,creaturey+1,creaturex-1,creaturex+1
  if creature~=hero then
    northspot,southspot,eastspot,westspot=max(northspot,curmap.mny),min(southspot,maxy-1),max(eastspot,curmap.mnx),min(westspot,maxx-1)
  end
  return {northspot,eastspot,southspot,westspot}
end

function movecreatures()
  local gothit=false
  local actualdistance=500
  local xcoord,ycoord,zcoord=hero.x,hero.y,hero.z
  for creaturenum,creature in pairs(creatures[mapnum]) do
    local cfacing,desiredx,desiredy,desiredz=creature.f,creature.x,creature.y,creature.z
    if desiredz==zcoord then
      while creature.mva>=creature.nm do
        local spots=calculatemoves(creature)
        --foreach(spots,logit)
        if creature.hos then
          -- most creatures are hostile; move toward player
          local bestfacing=0
          actualdistance=squaredistance(creature.x,creature.y,xcoord,ycoord)
          local currentdistance=actualdistance
          local bestdistance=currentdistance
          for facing=1,4 do
            if facing%2==1 then
              currentdistance=squaredistance(creature.x,spots[facing],xcoord,ycoord)
            else
              currentdistance=squaredistance(spots[facing],creature.y,xcoord,ycoord)
            end
            if currentdistance<bestdistance+rnd()-.5 then
              bestdistance,bestfacing=currentdistance,facing
            end
          end
          if bestfacing%2==1 then
            desiredy=spots[bestfacing]
          else
            desiredx=spots[bestfacing]
          end
          creature.f=bestfacing
        else
          -- neutral & friendly creatures just do their own thing
          if rnd()<.5 then
            if cfacing and rnd()<.5 then
              if cfacing%2==1 then
                desiredy=spots[cfacing]
              else
                desiredx=spots[cfacing]
              end
            else
              local facing=flr(rnd(4)+1)
              creature.f=facing
              if facing%2==1 then
                desiredy=spots[facing]
              else
                desiredx=spots[facing]
              end
            end
          end
        end
        local newloc=mget(desiredx,desiredy)
        if (curmap.dg) newloc=getdungeonblk(desiredx,desiredy,desiredz,1)
        local canmove=false
        for terrain in all(creature.t) do
          if newloc==terrain and creature.mva>creature.nm then
            canmove=true
            break
          end
        end
        creature.nm+=1
        if creature.hos and actualdistance<=1 then
          local hero_dodge=hero.dex+2*hero.lvl
          local creature_msg="the "..creature.n
          if creature.eat and hero.fd>0 and rnd(creature.dex*23)>rnd(hero_dodge) then
            sfx(2)
            update_lines{creature_msg.." eats!"}
            deductfood(flr(rnd(6)))
            gothit=true
            delay(9)
          elseif creature.th and hero.gp>0 and rnd(creature.dex*20)>rnd(hero_dodge) then
            sfx(2)
            local amountstolen=min(ceil(rnd(5)),hero.gp)
            hero.gp-=amountstolen
            creature.gp+=amountstolen
            update_lines{creature_msg.." steals!"}
            gothit=true
            delay(9)
          elseif creature.po and rnd(creature.dex*15)>rnd(hero_dodge) and band(hero.st,1)~=1 then
            sfx(1)
            hero.st=bor(hero.st,1)
            update_lines{"poisoned by the "..creature.n.."!"}
            gothit=true
            delay(3)
          elseif rnd(creature.dex*64)>rnd(hero_dodge+hero.ar) then
            hero.gothit=true
            sfx(1)
            local damage=max(rnd(creature.dmg)-rnd(hero.ar),0)
            deducthp(damage)
            update_lines{creature_msg.." hits!"}
            --logit('hit by creature '..(creature.n or 'nil')..' '..creature.x..','..creature.y..','..creature.z)
            gothit=true
            delay(3)
            hero.hd=3
            hero.hc=creature.ac
          else
            update_lines{creature_msg.." misses."}
          end
          break
        elseif canmove then
          local movecost=band(fget(newloc),3)
          creature.mvp+=1
          if creature.mvp>=movecost and not contents[desiredx][desiredy][zcoord] and not (desiredx==xcoord and desiredy==ycoord and desiredz==zcoord) then
            contents[creature.x][creature.y][creature.z]=nil
            contents[desiredx][desiredy][desiredz]=creature
            creature.x,creature.y=desiredx,desiredy
            creature.mvp=0
            break
          end
        end
      end
      creature.nm=0
    end
  end
  return gothit
end

function world_update()
  local btnpress=btnp()
  if btnpress~=0 then
    coresume(processinput,getbutton(btnpress))
  end
  if turnmade then
    turnmade=false
    turn+=1
    if turn%500==0 then
      increasehp(1)
    end
    if turn%50==0 then
      deductfood(1)
    end
    if turn%10==0 then
      hero.mp=not_over_32767(min(hero.mp+1,hero.int*(hero.lvl+1)))
    end
    if turn%5==0 and band(hero.st,1)==1 then
      deducthp(1)
      sfx(1,3,8)
      update_lines{"feeling sick!"}
    end
    gothit=movecreatures()
    if #creatures[mapnum]<curmap.mxm and rnd(10)<curmap.newm then
      create_monster()
    end
  end
end

-- drawing routines

function delay(numofcycles)
  for delaycount=1,numofcycles do
    flip()
  end
end

function animatespr(sprnum)
  local sprloc=flr(sprnum/16)*512+sprnum%16*4
  if(fget(sprnum,7))then
    for row=0,448,64 do
      reload(sprloc+row,sprloc+row,4)
      fset(sprnum,band(fget(sprnum),64))
    end
  else
    for row=0,448,64 do
      memcpy(sprloc+row,sprloc+row+4,4)
      fset(sprnum,7,true)
    end
  end
  fset(sprnum,2,true)
end

function print_r(str, x, ...)
  print(str, x - 4 * #tostr(str), ...)
end

function draw_stats()
  local linestart,lineend=106,127
  print("cond",linestart,0,5)
  print_r(band(hero.st,1)==1 and 'p' or 'g',lineend,0,6)
  print("lvl",linestart,8,5)
  print_r(hero. lvl,lineend,8,6)
  print("hp",linestart,16,5)
  print_r(hero.hp,lineend,16,6)
  print("ap",linestart,24,5)
  print_r(hero.mp,lineend,24,6)
  print("$",linestart,32,5)
  print_r(hero.gp,lineend,32,6)
  print("f",linestart,40,5)
  print_r(hero.fd,lineend,40,6)
  print("exp",linestart,48,5)
  print_r(hero.exp,lineend,55,6)
  print("dex",linestart,63,5)
  print_r(hero.dex,lineend,63,6)
  print("int",linestart,71,5)
  print_r(hero.int,lineend,71,6)
  print("str",linestart,79,5)
  print_r(hero.str,lineend,79,6)
  for linenum=1,numoflines do
    print(lines[(curline-linenum)%numoflines+1],0,129-linenum*8)
  end
end

function substitutecolors(colorsubs)
  for colorsub in all(colorsubs or {}) do
    pal(colorsub[1],colorsub[2])
  end
end

function itemdrawprep(item)
  local flipped=false
  if item.iseq then
    item.iseq-=1
    if item.iseq<0 then
      item.iseq=23
      if item.ia then
        item.ia=false
        --if(item.i==nil)update_lines{"item.i nil"}
        if(item.fi==nil)item.i-=1
      else
        item.ia=true
        --if(item.i==nil)update_lines{"item.i nil"}
        if(item.fi==nil)item.i+=1
      end
    end
    if item.fi then
      flipped=item.ia
    end
  end
  palt(0,false)
  substitutecolors(item.co)
  return flipped
end

function draw_map(x,y,scrtx,scrty,width,height)
  map(x,y,scrtx*8,scrty*8,width,height)
  for contentsx=x,x+width-1 do
    for contentsy=y,y+height-1 do
      local item=contents[contentsx][contentsy][0]
      if item then
        local flipped=itemdrawprep(item)
        local f=item.fm and item.f or 0
        spr(item.i+f,(contentsx-x+scrtx)*8,(contentsy-y+scrty)*8,1,1,flipped)
        pal()
        if item.hd>0 then
          substitutecolors(item.hc)
          spr(127,(contentsx-x+scrtx)*8,(contentsy-y+scrty)*8)
          pal()
          item.hd-=1
        end
      end
    end
  end
end

function getdungeonblk(mapx,mapy,mapz,asterrain)
  local blk=0
  if mapx>=curmap.mxx or mapx<curmap.mnx or mapy>=curmap.mxy or mapy<curmap.mny then
    blk=3
  else
    local row=curmap.l[mapz][mapy]
    row=flr(shr(row,(curmap.w-mapx)*2))
    blk=band(row,3)
  end
  return asterrain and (blk>1 and 20 or 22) or blk
end

function triplereverse(triple)
  triple[1],triple[3]=triple[3],triple[1]
end

function getdungeonblks(mapx,mapy,mapz,facing)
  local blks={}
  if facing%2==0 then
    -- we're looking for a column
    for viewy=mapy-1,mapy+1 do
      add(blks,{
        b=getdungeonblk(mapx,viewy,mapz),
        x=mapx,
        y=viewy
      })
    end
    if (facing==4) triplereverse(blks)
  else
    -- we're looking for a row
    for viewx=mapx-1,mapx+1 do
      add(blks,{
        b=getdungeonblk(viewx,mapy,mapz),
        x=viewx,
        y=mapy
      })
    end
    if (facing==3) triplereverse(blks)
  end
  return blks
end

function getdungeonview(mapx,mapy,mapz,facing)
  local blks={}
  local viewx,viewy=mapx,mapy
  if facing%2==0 then
    for viewx=mapx+4-facing,mapx+2-facing,-1 do
      add(blks,getdungeonblks(viewx,viewy,mapz,facing))
    end
    if (facing==4) triplereverse(blks)
  else
    for viewy=mapy-3+facing,mapy-1+facing do
      add(blks,getdungeonblks(viewx,viewy,mapz,facing))
    end
    if (facing==3) triplereverse(blks)
  end
  return blks
end

function dungeon_draw()
  cls()
  if hero.lit>0 then
    local view=getdungeonview(hero.x,hero.y,hero.z,hero.f)
    for depthindex,row in pairs(view) do
      local depthin,depthout=(depthindex-1)*10,depthindex*10
      local topouter,topinner,bottomouter,bottominner=30-depthout,30-depthin,52+depthout,52+depthin
      local lowextreme,highextreme,middle,lowerase,higherase=30-depthout*2,52+depthout*2,42,31-depthin,51+depthin
      if row[1].b==3 then
        rectfill(topouter,topouter,topinner,bottomouter,0)
        line(lowextreme,topouter,topouter,topouter,5)
        line(topouter,topouter,topinner,topinner)
        line(topouter,bottomouter,topinner,bottominner)
        line(lowextreme,bottomouter,topouter,bottomouter)
      end
      if row[3].b==3 then
        rectfill(bottominner,topinner,bottomouter,bottomouter,0)
        line(bottomouter,topouter,highextreme,topouter,5)
        line(bottominner,topinner,bottomouter,topouter)
        line(bottominner,bottominner,bottomouter,bottomouter)
        line(bottomouter,bottomouter,highextreme,bottomouter)
      end
      if depthindex>1 then
        local leftoneback,centeroneback,rightoneback=view[depthindex-1][1].b,view[depthindex-1][2].b,view[depthindex-1][3].b
        if (row[1].b==centeroneback and row[1].b==3) or
          (row[1].b~=leftoneback) then
          line(topinner,topinner,topinner,bottominner,5)
        end
        if (row[3].b==centeroneback and row[3].b==3) or
          (row[3].b~=rightoneback) then
          line(bottominner,topinner,bottominner,bottominner,5)
        end
        if centeroneback==3 and leftoneback==3 and row[1].b~=3 then
          line(topinner,lowerase,topinner,higherase,0)
        end
        if centeroneback==3 and rightoneback==3 and row[3].b~=3 then
          line(bottominner,lowerase,bottominner,higherase,0)
        end
      end
      if row[2].b==3 then
        rectfill(topouter,topouter,bottomouter,bottomouter,0)
        line(topouter,topouter,bottomouter,topouter,5)
        line(topouter,bottomouter,bottomouter,bottomouter)
        if row[1].b<3 then
          line(topouter,topouter,topouter,bottomouter)
        end
        if row[3].b<3 then
          line(bottomouter,topouter,bottomouter,bottomouter)
        end
      end
      dungeondrawobject(row[2].x,row[2].y,hero.z,3-depthindex)
    end
    rectfill(82,0,112,82,0)
  end
  draw_stats()
end

function dungeondrawobject(xcoord,ycoord,zcoord,distance)
  if xcoord>0 and ycoord>0 then
    local item=contents[xcoord][ycoord][zcoord]
    if item then
      local flipped,distancemod,shm,szm=itemdrawprep(item),distance*3,item.shm or 0,item.szm or 0
      local xoffset,yoffset=20+distancemod+(szm*(distance+1)/8),35-(3-distance)*shm
      local isize=60-szm-distancemod*4
      sspr(item.i%16*8,flr(item.i/16)*8,8,8,xoffset,yoffset,isize,isize,flipped)
      pal()
      if item.hd>0 then
        palt(0,true)
        substitutecolors(item.hc)
        sspr(120,56,8,8,xoffset,yoffset,isize,isize)
        pal()
        item.hd-=1
        palt(0,false)
      end
    end
  end
end

function msg_draw()
  cls()
  print(msg)
end

function world_draw()
  local maxx,maxy,minx,miny=curmap.mxx,curmap.mxy,curmap.mnx,curmap.mny
  local width,height=curmap.w,curmap.h
  local xtraleft,xtratop,xtrawidth,xtraheight,scrtx,scrty,left,right=0,0,0,0,0,0,hero.x-halfwidth,hero.x+halfwidth
  if left<minx then
    xtrawidth=minx-left
    scrtx=xtrawidth
    left=minx
  elseif right>=maxx then
    xtrawidth=right-maxx+1
    right=maxx
  end
  local top,bottom=hero.y-halfheight,hero.y+halfheight
  if top<miny then
    xtraheight=miny-top
    scrty=xtraheight
    top=miny
  elseif bottom>=maxy then
    xtraheight=bottom-maxy+1
    bottom=maxy
  end
  local mainwidth,mainheight=min(fullwidth-xtrawidth,curmap.w),min(fullheight-xtraheight,curmap.h)
  if cycle%16==0 then
    for sprnum=12,14,2 do
      animatespr(sprnum)
    end
  end
  cycle+=1
  cls()
  draw_map(left,top,scrtx,scrty,mainwidth,mainheight)
  palt(0,false)
  if hero.color==4 and hero.i==0 then
    substitutecolors{{4,1},{15,4}}
  end
  spr(hero.i+hero.f,48,40)
  pal()
  palt()
  if hero.hd>0 then
    substitutecolors(hero.hc)
    spr(127,48,40)
    pal()
    hero.hd-=1
  end
  draw_stats()
end

__gfx__
000ff000000003000000040000000400000000000000000000330000000005000005500000006000055555600277772000000000000000000000111011100000
000ff565030000000400000000404040000300000100100003333000005050500050050000066600666656600277772001000101101010000011010001000011
00888676000000000000000004040004003330000033000003333330050500050050005000d006006ff656600288882010101010010101011100001000101100
f8888565000300000004000040004000000300300000000000330333500050000500000505000d00666656600088880000010000000000100000000000000000
08ccc5000000000300000004000404000000033300001001030033330005050050005500050500506ff656600088880000000000000000000001110011000001
00c0cc00000000000000000000400400003000300000033033330330005005005005005050005050666656600288882001000101101010000110100010000110
00c00c000000300000004000040000400333000010010000333300000500005000500005000050056ff656600222222010101010010101011000010101011000
01100110300000004000000000000000003000000330000003300000000000000000000000000000666656000200002000010000000000100000000000000000
006666004440444055505550fff0fff055005550550055500000050055555555ccccccccfff0fff0000000005545545522222222222222222222222200000000
065555600000000000000000000000005550555055505550050000005d0d0d05c000000c00000000004004000044440025555502255555022500000200055000
650000564044404450555055f0f4f0ff5055505550555055000000005d0d0d05c000000cf0fff0ff004444000040040025555502255555022500000204444440
444444440000000000000000000000000055505500505055000500005d0d0d05c000000c00000000004004000044440025555502255555022500000207777770
446666444440444055505550fff0fff05550555055555550000000055d0d0d05c000000cfff0fff0004444000040040025559502255595029590000207777770
465555640000000000000000000000005550055555500555000000005d0d0d05c000000c00000000004004000044440025555502255aa5022500000207777770
654444564044404450555055f0fff0ff5055505550555055000050005d0d0d05c000000cf0fff0ff004444000040040025555502255555022500000200055000
6500005600000000000000000000000000550000005500005000000055555555cccccccc00000000554554550000000025555502255555022500000200055000
0000050000050500000550000087800000000000000040000666666000c01c000000220000000000555555555555555555555555555555555555555555555555
005050500050505050066005087778000008800000044400690000860c17c7100002882000ffffff000660000666660000666600066666000666666006666660
05055505550000509596695984474480008448000004250060900806010c70c0000288200ffffff2006006000600006006000060060000600600000006000000
5050005000066005555555554ff7ff400844448000052400600980060c07c01000028820ffffff42060000600666660006000000060000600666600006666000
00500050006006005d5665d5f5fff5f0042245400400444060089006010c70c00088882024994242066666600600006006000000060000600600000006000000
000555000560065055566555fff4fff00422454044404240608009066c07c0160888822024444202060000600600006006000060060000600600000006000000
055000505060060055566555fff4fff004224440424042406800009666cccc660200202020000200060000600666660000666600066666000666666006000000
00000000006006000000000000000000000000004240000006666660566666650200200020000200555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
00666600060000600666660006666600060006000600000006600660060000600066660006666600006666000666660000666600066666000600006006000600
06000060060000600006000000006000060060000600000006066060066000600600006006000060060000600600006006000000000600000600006006000600
06000000066666600006000000006000066600000600000006000060060600600600006006000060060000600600006000666600000600000600006000606000
06066660060000600006000000006000060060000600000006000060060060600600006006666600060600600666660000000060000600000600006000606000
06000060060000600006000006006000060006000600000006000060060006600600006006000000060060600600006006000060000600000600006000060000
00666600060000600666660000660000060000600666660006000060060000600066660006000000006666000600006000666600000600000066660000060000
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
5555555555555555555555555555555555555555555555555555555500626000000067000067600000760000000000000000000000044000000ff000000ff000
0600006000666600060006000666666000000000000000000066000006626600000677000677760000776000000ff000000ff00000044004f00ff000000ff00f
0600006006000060060006000000060000000000000000000600600007626700006777000777770000777600000ff000000ff00000eeeeeeff8ff8ffffffffff
0600006000000060006060000000600006666660000000000066000007626700066777000777770000777660f3bbbb0000bbbb004eeeee000088880ff0ffff00
00600600000666000006000000060000000000000000000006006060076267000066770007747700007766000b0bb3bffb3bb3b00e666600000ff000000ff000
0066660000000000000600000060000000000000000000000600060007727700400620000664660000026004000bb000000bb0f0006006000008800000088000
006006000006600000060000066666600000000000000000006660600444440004444444044444004444444000111100001111000060060000f00f0000f00f00
55555555555555555555555555555555555555555555555555555555004440000044444000444000044444000110011001100110011001100ff00ff00ff00ff0
000ff00000044000500ff000000ff000400ff000400ff00000011000000110000002200500022000000ff000000ff000000ff00000ff00000000000000008880
600ff00000044004500ff000000ff000400ff000400ff00f000ff00f000ff000000ff00f000ff000000ff000f00ff00f000ff000f0ff00000444444000055008
9999990600d22d44503433005034330f402222204022222260111110f111110000111110f111110500cccc00ffccccff07767000777670f04444444400522500
0099999904dddd005344344353443443f222222240222222f11110006011111ff11110000011111f0fccacf000ccac007776777f777677704446644400588500
00099000044dd000f433430f543343004222220ff2222200010110000001100001011000000110000ffccff0000cc000f0767070007677005555555505055080
00111000000dd00040304300f0304300400222004222200000cccc0000cccc000088880000888800000cc000000cc00000717000007170004444444408000888
0010110000dddd000010010040100100402222004022220000c00c0000c00c000040040000400400005005000050050000101000001010004444444488800808
044004400dddddd00110011001100110402222004022220001100110011001100440044004400440055005500550055001101100011011000000000080800000
00404000004040000000000100100000000550000005500040055000000550000101101000011000000000000000000000000000000000006001100661011016
00444000004440000110550000105501400550000005500540055000000550051011110101011010000000000500005000004440044400006601106666011066
00848400008484000005666010056660400110000001105040111100401111500010010015100151050000500500005000004004400400006660066666600666
00f5f44000f5f4400555556005555551405115000551150040111100451111000501105050011005056006505560065500444400004444000661166056611665
000f4445000f44455866850145866840050110504001100045111050500111005110011550100105566556655065560504555440044555405010010550100105
00444f400f444f404666640004666600000110054001100040011005401110005108801501088010565885655658856505858540045858501108801101088010
00f40ff0ff040f4405005140045005440010010040100100001111004011110001011010010110100605506006055060045e54400445e5401001100110011001
0ff0004400440ff00466000000066000055005500550055000111100401111000000000001000010000000000600006000400400004004000000000010000001
00000000000000000000000000000000001003900010000000011000010110100000000000000000000330000000000000000000000110000101101000000000
00000000000000000000000000000000100000301000093001011010100110010040400000404000003333000004400000007f00011111101011110100900900
00bb008b00b00b80000008f000008f00000100300001030010100101151001510044490000444000033b3330004ff40007f0ff00101551011215512109088090
0b00b0b00b0b00b0500004000050400001033031013303010501105050011005008489900084890003333b33004444000ff000000201102020011002008aa800
0b00b0b00b00b0b00f00ff000f00ff000030033003003100501001055010010507f4f70007f4f79033b33333004ff400007f07f02015510220155102008aa800
00b00bb0b000bb00044444f0004444f010300100010100001108801101088010047f7400007f794033333b300044440000f7fff0111881110118811009088090
000b00000b00000000ffff0000ffff00010100001000000010011001100110014494940004044990033b33000048840007fff00010c11c0110c11c0100900900
00000000000000000000000000000000000000100010010000c00c0010c00c01000099004400000000033000000440000ff00000000110001001100100000000
909090807070704040606060606060606010101010404010101010c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c0404010c0c0c0c010
10303020203070907070707080809090202020401111402020204011114020202020209191919191111191919191912041414141414141414141414141414141
909090807070404040606060606060106060601010101040101022c0c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c010701240c0c0c0c010
103020202030708080709080809090902020202011111111111111111111112020202091118292911111919282119120419282114192821151a0a0a0a041b141
90909080707040404060606060601010606060101010104010101010c0c0c0c0c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c01010c0c0e0e0c010
102030202030708090707080809080902020202020202020202020202020112020202091b01111d11111d11111b0912041111111411111114141414141411141
90908080704040606060606060606060601060601010101010104010c0101010c0c0c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c0c0c0e0e0c0c010
10101020307080908090709080908090202020209191919191919191202011202020209191919191c1c191919191912041115454411154544182929282411141
9080808070704060606060606060106060101010404010101040c0c0c01010101010c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c0c0c0c0c0c0c01010
10301010303080807070707080808090202020209154d38304735491202011202020202020111111111191b2a2b3912041111192411111924111111111411141
90908080707040404040404010101010101010101010101010100110101010101010c0c0e0e0e0c0c0e0e0e0e0e0e0e0e0c0c0c0c0c0c0c0c0521010c0c01010
102030102020307070707070808090902020202091a011111111a0914020112020202020111140c0401191111111912041111182411111824111111111411141
9090908070704040101010101010101010101010404040401010c01010101010101010c0c0c0c0c0c0c0c0c0c0e0e0e0c0c0c0c0c01040104010104040401010
1020201020203030303070708080909020202020915413a25353549140401111111111111140c072c091738373e2912041111111411111114111111111411141
9090808070701010c0c0c0c0c0c0c0101010c0c0c0c0c0c0c0c0c010401040401010101010d0101010421010c0c0c0c0c0c0c010101010404040101040101010
103020102020203030307070809090902020202091111111111111111111112020202020111140c040111111111191204141d1414141d1414141d14141411141
90908080707010c0c01010101010c0c0c0c0c01010101010c0101040401040402010101010d01010101010101010101010101010101040707040101010101010
30303010f12030202030707080809090202020209111111111111191404011202020202020111111111111111111912041111111a0111111a011111111d11141
90908080707010c010101010101010101010101010101010c0104040401040203010101010011010401010101010101010101010101010101010401010101010
10404010102020202030707080809090202020209191919191919191402011202020202020101010919191919191912041a011111111111111111111a041a041
908080c0c0c0c0c01060606060101010101010101010101001101010101030202020303010d01030304020101020204010404020202020202010101010101010
10101010102020307070707080809090202020202020202020202020202011202091919191919110101010101010202041414141414141414141414141414141
908090807070101010606060101010101010101010c0c0c0c0104040302020202020202010d01010103020202020203010202030302020302020202020101010
f1101010102020202070807080808090209191919191919191919120202011202091f28383d29110919191919191202091919191919120919191919191919191
9090808070706060604010101010101010c0c0c0c0c0101010102030203020202020202010d0d0d0102020303020202010203030302030303020202020202020
20201010102030202070808080808090209154c3c213838353549120202011204091111111119110919282111191202091a273d3e29120911111111111111191
90908080907040101010404010101010c0c0c01010101020202020202020202020202020101010d0103020203030202010303020202030303020202020202020
30201010102020202070708080909090208111111111111111118120202011404091c3d383939110918211111191202091111111119120910413a2d3c213a291
909080908090104040404040c0c0c0c0c010c050c0501020202020202020202020202020202010c0104030202020203010303020303030302020202020202020
20301010103020302070707080809090209111111111111111119120202011111111111111119110911111b0b09120209113e373d39120c11111111111111191
90809042707040c0c0c0c0c0c01010101050c0c0c0c05010202020202020202020202020201010c0104030203020303010302020303030202030303030303020
2030101010303020207070708080909020815454541111545454812020201140409111111111911091d191919191202091111111119120911173e2e2d2141191
908090c0c0c0c0c04040404040404050c0c0c0c0c0c0501020202020202020202010101010101001101010101010101010202030303030202020303030302020
20101010101030202030707080809090209182828211118282829120202011104091919191919110111110101010202091111111119120919191919191919191
9080808070704060606060604040505050c0c0c0c0501010202020202020202020104040202010c0c0c0c0c0c0c0c0c0c0c0c030202020203020202020202030
20101010101020203030707080809090208154545411115454548110101011101010101010101010111091919191912091911191919120202020202020202020
909090807070606060606060404040101010105050501050102020202020202020104010101010c010807070303030202030c0c0d03030303020302020302030
20101062101020203030707080809090209182828211118282829140104011101010101010101010111091b0b011912020402040202020202020202020202020
909080807040404060606040401020202020101010505050102020202020202040101010c0c0c0c0809080703030303020303030d03020202020202030302020
2010101010102030303070708080909020911111111111111111c111111111101010101010101010111091111111912020202020202020212121212121212020
908080807070404040404040103030303030202020201010202020202020202040101010c01010809090909090303030202020d0d02030302030203030303020
202010101020203030307070808090902091919191919191919191401040111111111111111111111111d111118291202091919191202021b3a2d22383212020
908080807070704040403030303030303030303030303020202020202020204040105210c01090907070707090302030303030d0303030202030203030303030
30202020202030303030707080809090201010101010101010101010101020201120202010101010101091118292912020919282914020211111111111212020
909080807070704040303030303030303030303030303030303030303020304040401010c01090708090907070802020202020d0202020202020203030808030
3020303030303030307070809080909020101010101010101010101010102020112020201010101010109191919191202091b011c12020c11111111111212020
909080807070807070803030307030303030303030303030303030303030303040404070c01090709070709080707030303030d0303020707070203030707070
3030303030303070708070708080909020202020202020202020202020202020112020202020202020202020202020202091919191402021435353b354212020
909080808070707080703030708080307070707070707030303030303030303080808070c080907090709090909090803030d0d0303020708080703080808080
80303030303080808080808080809090414141414141414141414141414141412020202020202020202020202020202020919282914020212121212121212020
90908080908070907070803030308070707070808080703030303030307070808070c0c0c080907070707070709080707030d030707070707080708070909090
7070303030307070708090808090909041616161616161614161416161616141209191919191912091919191401020202091b011c12020202020202020202020
90908080809080708070707070807070707080808070707070707070707070707070c080909070909090909070907070d0d0d080807070707070707090709070
70707070707070808080908080909090416141514161616141616161616161412091b0b01111912091b011911010102020919191914020204082828282824020
90909080808080807080907070707070707090909080807070707070707080808070c0907070707070707090708090d0d0707070708080807070707070707070
70808070808070809080909080809090416141616161616161616161614161412091111111829120911111c110c0101020919282914020209153e373c2139120
90909090808080809080808080808080808080809090808080808080808080808080c08090804280809080808080908080808080808090808080808080808080
808080808090808080808080808090904161416161616161616161616141614120911111829291209111119110c0c01020918211c12020209111111111119120
90809090809080808080909080808080808080808080808070808080809090909080c08090808080809090808090909080808080808080809080808080808080
9090909080808080808080808080909041614141416161614161616161416141209191d191919120919282911001c0c02091b0b0914020209163e373c2139120
90808080809090909090909080909090908080809090909090908080909080808080909090909090908080809090808080808080909090909090909090909080
808090909090908090908080909090904161616161616161416141616141b14120201020101020209191919140c0c0c020919191912020204082828282824020
90909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090
909090909090909090909090909090904141414141414141414141414141414120202020202020202020202020c0c0c020202020202020202020202020202020
__label__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000001d6776d00000000000000000000000000000000000000000000000000000
000000000001d50000000000000000000000000000000000000000000000000000056776777d0000000000000000000000000000000000000000000000000000
00000000056776000000000000000000000000000000000000000000000000000016775577760000000000000000000000000000000000000000000000000000
00000000067776000000000000000000000000000005d500000000000000000000d77600677d0000000000000000000000000000000000000000000000000000
000000001777775000000000000000000000000005677d0000000000000000000177760005500000000000000000000000000000000000000000000000000000
00000000d77777600000000000000000000000000d777d0000000000000000000577760000000000000000000000000000000000000000000000000000000000
0000000067d777710000000000000000000000000677750000000000000000000577760000000000000000000000000000000000000000000000000000000000
0000000d7617777d000000015ddd001d6d50000056777d551000005d66d100005d7776555000001dd66d500000015d6d01d650015ddd001d6d50001d66500000
00000006750677760000006777771d77777d000d77777777600056777776500d7777777771000d777777761000677776167771d777775d7777751d77777d0000
00000057600d7777100000567777676777771005d67776dd5005677516777105d67776ddd000d77750d7776100d6777667777556777777677777776777771000
00000067d0017777d0000001777765006777500006777500000677600d777d000d7776000005777d00067776000577776d7760007777d101777761016777d000
000001775111677760000000677710006777d0000677750000d777d0057776000d7776000006777500057777500177775055100067775000677750006777d000
00000d777777777775000000677710006777d0000677750000677766667776000d7776000007777500007777d0017777000000006777500067775000d777d000
00000676ddddd7777d000000677710006777d000067775000067776666666d000d7776000057777500006777d0017777000000006777500067775000d777d000
000057600000067776000000677710006777d0000677750001777750000000000d7776000057777500006777d0017777000000006777500067775000d777d000
000067d00000057777500000677710006777d00006777500006777d0000000000d7776000017777500006777d0017777000000006777500067775000d777d000
000177500000006777d00000677710006777d000067775000067776000005d000d7776000006777d000067775001777700000000677750006777d000d777d000
000676000000006777610001777750006777d00006777d0000d77776505d76000d777600000d7777100177760005777700000000677750006777d00067776000
01d77651000001d7777d50157777d105677765100d777766d006777777777501567777510000677761167775001d7777d10000157777d1057777610567776510
5777777d0000d77777777d6777777d577777776005777776500167777776500d7777777600000d77777776500577777776000067777776577777765777777760
1555555500001555555551555555550555555550005d66d1000005d666500005555555550000001d666d50000055555555000055555555155555550555555550
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
09999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999990
99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
99910000000100000003000050005000000660050003003000010000000100000001000000010000000300000003000040004000000400000004000000040999
99900000000000000000000300050500006006000000033300000000000000000000000000000000000000030000000300040400000000040000000400000999
99900101010001010000000000500500056006500030003001000101010001010100010101000101000000000000000000400400000000000000000000000999
99901010101010100000300005000050506006000333000010101010101010101010101010101010000030000000300004000040000040000000400000004999
99910000000100003000000000000000006006000030000000010000000100000001000000010000300000003000000000000000400000004000000040000999
99901110000000000000000000000300000003000000000000000000000011100000111000000000000003000000030000000400000004000000040000000999
99910100010001010100010103000000030000000100010101000101001101000011010001000101030000000300000004000000004040400400000004000999
99900010101010101010101000000000000000001010101010101010110000101100001010101010000000000000000000000000040400040000000000000999
99900000000100000001000000030000000300000001000000010000000000000000000000010000000300000003000000040000400040000004000000040999
99911100000000000000000000000003000000030000000000000000000111000001110000000000000000030000000300000004000404000000000400000999
99901000010001010100010100000000000000000100010101000101011010000110100001000101000000000000000000000000004004000000000000000999
99900101101010101010101000003000000030001010101010101010100001011000010110101010000030000000300000004000040000400000400000004999
99900000000100000001000030000000300000000001000000010000000000000000000000010000300000003000000040000000000000004000000040000999
99901110000011100000000000000000000000000000000000001110000011100000000000000000000003000000030000000300000003000000040000000999
99910100001101000100010101000101010001010100010100110100001101000100010101000101030000000300000003000000030000000400000000404999
99900010110000101010101010101010101010101010101011000010110000101010101010101010000000000000000000000000000000000000000004040999
99900000000000000001000000010000000100000001000000000000000000000001000000010000000300000003000000030000000300000004000040004999
99911100000111000000000000000000000000000000000000011100000111000000000000000000000000030000000300000003000000030000000400040999
99901000011010000100010101000101010001010100010101101000011010000100010101000101000000000000000000000000000000000000000000400999
99900101100001011010101010101010101010101010101010000101100001011010101010101010000030000000300000003000000030000000400004000999
99900000000000000001000000010000000100000001000000000000000000000001000000010000300000003000000030000000300000004000000000000999
99901110000000000000000000000000000000000000000000000000000000000000000000000300000003000000030000000400000003000000030000000999
99910100010001010100010101000101010001010100010101000101010001010100010103000000030000000300000000404040030000000300000000404999
99900010101010101010101010101010101010101010101010101010101010101010101000000000000000000000000004040004000000000000000004040999
99900000000100000001000000010000000100000001000000010000000100000001000000030000000300000003000040004000000300000003000040004999
99911100000000000000000000000000000000000000000000000000000000000000000000000003000000030000000300040400000000030000000300040999
99901000010001010100010101000101010001010100010101000101010001010100010100000000000000000000000000400400000000000000000000400999
99900101101010101010101010101010101010101010101010101010101010101010101000003000000030000000300004000040000030000000300004000999
99900000000100000001000000010000000100000001000000010000000100000001000030000000300000003000000000000000300000003000000000000999
99900000000000000000000000000000000040000000030000000300000000000000000000000300000003000000030000000400000004000000030000000999
99900101010001010100010101000101000444000300000003000000010001010100010103000000030000000300000004000000004040400300000004000999
99901010101010101010101010101010000425000000000000000000101010101010101000000000000000000000000000000000040400040000000000000999
99910000000100000001000000010000000524000003000000030000000100000001000000030000000300000003000000040000400040000003000000040999
99900000000000000000000000000000040044400000000300000003000000000000000000000003000000030000000300000004000404000000000300000999
99900101010001010100010101000101444042400000000000000000010001010100010100000000000000000000000000000000004004000000000000000999
99901010101010101010101010101010424042400000300000003000101010101010101000003000000030000000300000004000040000400000300000004999
99910000000100000001000000010000424000003000000030000000000100000001000030000000300000003000000040000000000000003000000040000999
99900300000000000000030000000000000003000000030000000000000000000000000000000300000003000000030000000400000004000000030000000999
99900000000300000300000000030000030000000300000000030000000300000003000003000000030000000300000004000000040000000300000004000999
99900000003330000000000000333000000000000000000000333000003330000033300000000000000000000000000000000000000000000000000000000999
99930000000300300003000000030030000300000003000000030030000300300003003000030000000300000003000000040000000400000003000000040999
99900003000003330000000300000333000000030000000300000333000003330000033300000003000000030000000300000004000000040000000300000999
99900000003000300000000000300030000000000000000000300030003000300030003000000000000000000000000000000000000000000000000000000999
99903000033300000000300003330000000030000000300003330000033300000333000000003000000030000000300000004000000040000000300000004999
99900000003000003000000000300000300000003000000000300000003000000030000030000000300000003000000040000000400000003000000040000999
99900300000003000000000000000000000000000000030000000300000000000004400000000300000003000000030000000400000004000000030000000999
99900000030000000003000000030000000300000300000003000000000300000004456503000000030000000300000000404040040000000300000004000999
99900000000000000033300000333000003330000000000000000000003330000088867600000000000000000000000004040004000000000000000000000999
99930000000300000003003000030030000300300003000000030000000300304888856500030000000300000003000040004000000400000003000000040999
999000030000000300000333000003330000033300000003000000030000033308ccc50000000003000000030000000300040400000000040000000300000999
999000000000000000300030003000300030003000000000000000000030003000c0cc0000000000000000000000000000400400000000000000000000000999
999030000000300003330000033300000333000000003000000030000333000000c00c0000003000000030000000300004000040000040000000300000004999
99900000300000000030000000300000003000003000000030000000003000000110011030000000300000003000000000000000400000003000000040000999
99900300000000000000050000000500000000000000030000000300000003000000030000000300000003000000040000000400000004000000030000000999
99900000000300000050505000505050000300000300000003000000030000000300000003000000030000000040404000404040004040400300000000055999
99900000003330000505000505050005003330000000000000000000000000000000000000000000000000000404000404040004040400040000000004444999
9993000000030030500050005000500000030030000300000003000000030000000300000003000000030000400040004000400040004000000300000777f999
9990000300000333000505000005050000000333000000030000000300000003000000030000000300000003000404000004040000040400000000030777f999
9990000000300030005005000050050000300030000000000000000000000000000000000000000000000000004004000040040000400400000000000777f999
99903000033300000500005005000050033300000000300000003000000030000000300000003000000030000400004004000040040000400000300000055999
99900000003000000000000000000000003000003000000030000000300000003000000030000000300000000000000000000000000000003000000000055999
99900300000003000000030000000300000003000000000000000300000003000000030000000300000003000000030000000000000000000000030000000999
99900000030000000300000003000000030000000003000003000000030000000300000003000000030000000300000000030000000300000300000003000999
99900000000000000000000000000000000000000033300000000000000000000000000000000000000000000000000000333000003330000000000000000999
99930000000300000003000000030000000300000003003000030000000300000003000000030000000300000003000000030030000300300003000000030999
99900003000000030000000300000003000000030000033300000003000000030000000300000003000000030000000300000333000003330000000300000999
99900000000000000000000000000000000000000030003000000000000000000000000000000000000000000000000000300030003000300000000000000999
99903000000030000000300000003000000030000333000000003000000030000000300000003000000030000000300003330000033300000000300000003999
99900000300000003000000030000000300000000030000030000000300000003000000030000000300000003000000000300000003000003000000030000999
99900400000004000000040000000400000003000000030000000300000003000000030000000300000003000000030000000300000003000000030000000999
99900000040000000400000004000000030000000300000003000000030000000300000003000000030000000300000003000000030000000300000003000999
99900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000999
99940000000400000004000000040000000300000003000000030000000300000003000000030000000300000003000000030000000300000003000000030999
99900004000000040000000400000004000000030000000300000003000000030000000300000003000000030000000300000003000000030000000300000999
99900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000999
99904000000040000000400000004000000030000000300000003000000030000000300000003000000030000000300000003000000030000000300000003999
99900000400000004000000040000000300000003000000030000000300000003000000030000000300000003000000030000000300000003000000030000999
99900400000004000000040000000400000004000000040000000400000004000000030000000300000003000000030000000300000000000000030000000999
99900000040000000040404004000000040000000400000004000000040000000300000003000000030000000300000003000000000550000300000003000999
99900000000000000404000400000000000000000000000000000000000000000000000000000000000000000000000000000000044444400000000000000999
99940000000400004000400000040000000400000004000000040000000400000003000000030000000300000003000000030000077777700003000000030999
99900004000000040004040000000004000000040000000400000004000000040000000300000003000000030000000300000003077777700000000300000999
99900000000000000040040000000000000000000000000000000000000000000000000000000000000000000000000000000000077777700000000000000999
99904000000040000400004000004000000040000000400000004000000040000000300000003000000030000000300000003000000550000000300000003999
99900000400000000000000040000000400000004000000040000000400000003000000030000000300000003000000030000000000550003000000030000999
99900400000004000000040000000400000004000000040000000400000004000000040000000400000004000000040000000400000003000000030000000999
99900000004040400040404000404040040000000400000004000000040000000400000004000000040000000400000004000000030000000300000003000999
99900000040400040404000404040004000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000999
99940000400040004000400040004000000400000004000000040000000400000004000000040000000400000004000000040000000300000003000000030999
99900004000404000004040000040400000000040000000400000004000000040000000400000004000000040000000400000004000000030000000300000999
99900000004004000040040000400400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000999
99904000040000400400004004000040000040000000400000004000000040000000400000004000000040000000400000004000000030000000300000003999
99900000000000000000000000000000400000004000000040000000400000004000000040000000400000004000000040000000300000003000000030000999
99900400000004000000040000000400000004000000040000000400000004000000040000000400000004000000040000000400000003000000030000000999
99900000004040400040404000404040040000000400000004000000040000000400000004000000040000000040404004000000030000000300000003000999
99900000040400040404000404040004000000000000000000000000000000000000000000000000000000000404000400000000000000000000000000000999
99940000400040004000400040004000000400000004000000040000000400000004000000040000000400004000400000040000000300000003000000030999
99900004000404000004040000040400000000040000000400000004000000040000000400000004000000040004040000000004000000030000000300000999
99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
09999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999990

__gff__
0000000101010201020b00000484048400000800080000080808000008080000010000000000000000000808080808080808080808080808080808080808080808080808080808000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909141414141418141414141414181414141414141814141414141414141414141814141414141414141814141414141414
0909090909090909090909090808090909090909090808090909080809090909080909080808090909090908080909090909090909090909090909080808080909090809090909090909090909090909140404010101010101010104040401010101010101010404141111111404010101010101010101010101010101040414
090909080808080808090909090808080808080908080808080909080908080807080909090808080807080808080809090808080808080808080808080808080908080808080809090808080808090914040114141414141414010101010114141414141414010414362e2d1401010101010101010101010101010101010414
09090808080808210908080808080808080808080c0c0c082009080806060608080808080809080808080808080808080808080908080808080808080808090808080808090809080808092408080909140101140b110b110b141814181418140b110b110b140101141111111c02141414141414141414141414141414010114
090909080709090907070707070707070707080808070c0c0c07070606070606060606080907070707070707070707080807080808070707070708090809070909080909090809080909080909080909140101140b110b110b141111111111140b110b110b140101141111111404151111111111111111111111111a14010114
090908210907070707070707070707070c0c0707070707070c0707070606060706060707070d0d0d0d0d0d0d0d0d0d070707080808070707070708080807070709070809090808080808090808090909140101140b110b110b14110c0c0c11140b110b110b140101141414141414141414141414141414141414141414010114
0909090909070505070505050505050c0c070707040407040c07070404060607070707070d0d06060606060606060d0d0d0d0707070d0d0d0d0d0d07070709070909080909080709090809080808090914010114111111111114110c270c11141111111111140101142928111114292811111429281111142928111114010114
0909080c0c0c0c0c0c050505050c0c0c05060606060404040c0c0c0c04040606040407070d0606060606060606060607070d0d0d0d0d040207070d07070707080808090809090709070808090908080918010114292811111114110c0c0c11141111112829140101181111111114111111111411111111141111111114010118
09090809080707050c0c0c0c0c0c050c05060606060604040404040c0c0c040406060d0d0d0606060606060606060606060707020202020202020d0203070707030707080809090907090808090808091401011414141411111d11111111111d11111414141401011414141d141414141d141414141d141414141d1414010114
0909092109080805050505050505050c050606060606060606060604040c01040d0d0d06060606060606060606060606060606040204040202030d020203030302030307070809090908090809080909140101141b111d11111418141114181411111d110a140101141111111111111111111111111111111111111114010114
0909080c0c080707050505050505050c050606060606060606060606040c010110040604060606060606060606060606060606040404020202020d0d0d0d0d0d020202030708080808080908080809091401011414141414141401141c14011414141414141401011414171414171414171414141d1414141414141414010114
090808080c070705050c0c0c0c0c050c050406060606060606060606010c0c0c0c0c0c060406060606060606060606060606060404020204020202020203020d0d0d0202070708080908070809090909140101010404040404010101110101010404040404010101140a111111111111110a14111111140a0a0a0a0a14010114
090908080c070505050c0505050c050c0504060606060606060606060604040404040c0c06040606060606060606060606060604040204020202020202020202030d0d0d020707080908090708080809140101010101010101010101110101010101010101010101141111111111111111111d1111111d111111110a14010114
090909050c0705050c0c0505050c0c0c0c0104040406060606060606060604040404010c0c0606060606060606060606060404040202020402020202020202020202030d0d0d0d070808080909080909140119191919191919190101110101191919191919191901143d313237343d2a373414111111140a0a0a0a0a14010114
09090c0c0c0c0c0c0c050505050505050c0c0c01040404040406060606040404040401040c0c06060606060606060606040404020402040202020202020202020302020202030d0d0d080708080809091401193c39323b323d190401110104194535322f2e4419011414141414141414141414141d1414141414141414010114
09090c0807050505050505050505050505010c0c0c0c0c01040404040404040404040104040c040404060606060606040401020402020202040202020202020202020202020202020d0d0d0d0d0d0909140119111111111111190401110104191111111111111901140101010101010101040404020404040101010101010414
090c0c070707050505050505050505010101010101050c0c0c0c0c0c0104010401010101040c040404040404060606040101010101010202020202020202020202020202020202030202070708090909180119312e2a353d3119040111010419452b3b2e2a2d1901180112121212121212121201020101010101010101040418
0909070707070505050505050505050101010101010101010c01010c0c0c0101010c2301010c04040404040404040404010101010101010101010202020202020202020203020202020707070809080914011911111111111119040111010419111111111111190114011211111111111111120102011414141414141d141414
090907080705050c0c0c0c0505050505050101010101010c0c010101010c0c0c0c0c0101010c0101040104010101010101010101010101010101010102020202020202020202020702020708080908091401191111111111111904011101041911111111111119011401122c2a373d2e2e371201020114112829141111110b14
090908080c0c0c0c05050c0c0c0c0c0c0505010c0c0c0c0c01010101010101010c0d01010110010101010101010101010101010101010101010101010202020202020303020202020707070708080809140119191911191919190111111101191919191119191901140112111111111111111201020114111128142811111114
0909080807070505050505050505050c0c0c0c0c010101010101010401010101010d0d01010d0101010101010101010101010101010101010101010101020302020202020202030207070707080809091401010104110401010101111f110101010104110401010114011211111111111111120102021d111111142928111114
090909080707040505050505050501010101010c01010101040101010101010101010d0d010d0d0101010c0c0c0c0c0c010101010101010404011f010101020303020202020202070207070808080909140101010111111111111111111111111111111101010101140112121212111212121201020114111111142811111114
0909080807040404040404010101010101010c0c0101040101010101010101010c0c0c0c0c0c0c0c0c0c0c010c0c0e0c0c0c0c0c0c0c0c0c040101010101010103020202030202020207070808090909140101010101010101040404110404040101010101010101140101010101020202020202020114110b0b1411110b0b14
090808080707040407070701010101010c0c0c0101010101040104010101010c0c0e0e0c0c01010c0e0e0c0c0c0e0e0e0e0e0e0e0e0e0c0c040104040101010101020202020202020207070708080909141414141418141414140411111104141414141814141414141414141414141814141402020214141414141414141414
090808080707070707070c0c0c0c0c0c0c01010101040101010101010101010c0e0e0e0e0c0c0c0c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c2401040c0101010101020302020202030202070708080909020202020202020202020202020202020202020202020202020202020202020214141414141414141414141414141414
0909080c0c0c0c0c0c0c0c07070c010101010101010101040101010101010c0c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c01010c0c0c0c0c0101030202020202020202070708080909021919191919191919191919191919190202021919191919191919191919190214111d111111111d11110b140b111114
090909080707070707070707070c010104010101010101010101010101010c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c0c0c0c0c0c0c0c010303020202020202020707080808090219332a2c342e3d19313e373d454619040202192a392a3b3d362e373d3c190214111411111128142811111411112814
090908080707070707070707070c0101010104010101010101010101010c0c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c0e0e0c0c0c0c01020202020302020202070708080909021911111111111119111111111111190402021911282919111119292811190214111411111129142911111c11112914
0908080807070707070c0c0c0c0c01010101010101240101010101010c0c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c0c0c010102020202020202020207070808090902193b2a2c342e3d19452f323c314519040202190b11111d11111d11110b190214111114141414141414141414141414
0909080807070707070c0401010101040101010101010104040101010c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c010c0c0101020203020202020203070708080909021911111111111119111111111111190402021919191919111119191919190214111511111129142911111c11112914
090908080c0c0c0c0c0c04040404010101010101040401010401010c0c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c01040c010101020203020203030303070708080809021911111111111113111111111111190402021911282919111119292811190214111411111128142811111411112814
0909080907070707040404040606040101010101040101010104010c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c0c0c0c0c0c01010102030302030307030307070808090902191919111119191919191111191919020202190b11111d11111c11110b1902141a14111111111d11110b140b111114
__sfx__
000100002365024650206501e64000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00010000245202a6403075034660367703667034770326602f6602d75029650297502a640296402b7402c6502e6502f7502f6502d640296302563022720000000000000000000000000000000000000000000000
0001000017070160601505016050170501a06021060290702f0700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000200000f2710e1710e1710d1710d1710c1710c1751e2711d7711d7711d7711b7711b7711b771197711977119771177311771500100001000050000000000000000000000000000000000000000000000000000
000100001263113341164511335117451133511544111341144310e331114210b3210c411073110a41104611236011f6002f50029500235001e5001e500225002750029500255001f5001e500000000000000000
011800002b4252b42529425264252942529425264252442526425264252442523425244252342521420214251c4201c4201c4201c4201c4251d4001d4201d4251c4201c4201c4201c42515425174251a4251d425
011800001c4201c4201c4201c4201c425000001d4201d4251f4201f4201f4201f4201f4201f4201f4201f4251f4201f4201f4201f4201f4201f4201f4201f4251f4201f4201f4201f4251f4201f4251842517425
0118000000000000000000000000000000000000000000000000000000000000000000000000001d4251f4252242022425224202242520420204251d4251f4252242522420224251f4201f4201f4201f4201f425
011800000000000000000000000000000000000000000000000000000000000000000000000000000000000013320133201332013320133201332013320133251332013320133201332013320133201332013325
011800001a4201a4201a4201a4201a4201a425000001a4251d4251d4201d4251f4201f4201f4201f4201f425244202442024420244202442524000244202442522420224202242022420224251f425224251f425
011800000000022420224251f4201f4201f425000001a4251d4251d4201d4251f4201f4201f4201f4201f4250000024420244252442024420244250000000000224251f4201f4251f4201f4201f4201f4201f425
011800000030013320133251732017320173250030017325153251532015325133201332013320133201332500300133201332513320133201332500300133251332517320173251732017320173201732017325
011800001a4201a4201a4201a4201a425000001a4201a4251d4251d4201d4251f4201f4251f425234251f425244202442024420244202442524000244202442522420224202242022420224251f425224251f425
011800002240022420224251f4201f4201f4251a4051a4251d4251d4201d4251f4201f4201f4201f4201f4250000024420244252442024420244250000000000224251f4201f4251f4201f4201f4201f4201f425
011800001330013320133251732017320173250030017325153251532015325133201332013320133201332500300133201332513320133201332500300133251332517320173251732017320173201732017325
011800002442024420244202442024425000002442024425234202342023420234202342500000234252442526420264202642026425244202442024420244252342023420234202342521420214202142021425
01180000000002442024425244202442024425000001f425224251f4201f4251f4201f4201f4252342524425264202642526425264252442524425244252442523425234252342523425214251d4251d4201d425
011800000030013320133251732017320173250030017325133251732017325173201732017325133251532517320173251732517325153251532515325153251332513325133251332511325153251532015325
011800001f4201f4201f4201f4251d4201d425000001f4251f42515425174251a4251f4201f4252142021425234202342023420234252442024425000002b4252b4252b425294252642529425294252642524425
011800000000022425214251f4251d4251a4251d4251f4251f4201f4201f4201f4201f4201f4201f4201f4250000022425214251f4251d4251a4251d4251f4251f4201f4201f4201f4201f4201f4201f4201f425
01180000003001732515325133251132011325153251332513320133201332013320133201332013320133250c300173251532513325113201132515325133251332013320133201332013320133201332013325
0118000026425264252442523425244252342521420214251f4201f4201f4201f4201f425000001d4201d4251f4201f4201f4201f42515425174251a4251d4251f4201f4201f4201f4201f425000001d4201d425
011800001f4201f4201f4201f4201f4201f4201f4201f425000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011800001332013320133201332013320133201332013325000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011800001f4201f4201f4201f4201f4201f4201f4201f4251f4201f4201f4201f4201f4201f4201f4201f4251f4201f425000001f4201f4251f42518425174251a4201a4201a4201a4201a425000001a4201a425
011800000000000000000000000000000000001d4251f425234202342523425234251f4251f4251d4251f425224251f425224251f4201f4201f4201f4201f4250000022420224251f4201f4201f425000001a425
011800000000000000000000000000000000000000000000133201332013320133201332013320133201332513320133201332013320133201332013320133250000013320133251732017320173250000017325
011800001d4251d4201d4251f4201f4201f4201f4201f4251f4201f4201f4201f4201f4201f4201f4201f4251f4201f425000001f4201f4251f42518425174251a4201a4201a4201a4201a425000001a4201a425
011800001d4251d4201d4251f4201f4201f42500000000001f4201f4201f4201f4201f4201f4201f4201f4251f4201f4201f4201f4201f4201f4201f4201f4250000022420224251f4201f4201f425000001a425
011800001532515320153251332013320133250000000000173251732517325173251732517325173251832519325183251732513320133201332013320133250000013325173201732017325000000000017325
011800001d4251d4201d4251f4201f4251f425234251f425244202442024420244202442500000244202442522420224202242022420224251f425224251f4252442024420244202442024425000002442024425
011800001d4251d4201d4251f4201f4201f4201f4201f425000002842028425284202842028425000001f425224251f4201f4251f4201f4201f4201f4201f425000002442024425244202442024425000001f425
011800001532515320153251332013320133201332013325000001332013325133201332013325000001332513325173201732517320173201732017320173250000013320133251332013320133250000013325
011800002242022420224202242022425000002242524425264202642026420264252442024420244202442522420224202242022425214202142021420214251f4201f4201f4201f4251d4201d425000001f425
01180000224251f4201f4251f4201f4201f42522425244252642026425264252642524425244252442524425224252242522425224251d4251d4251d4201d4250000022425214251f4251d4251a4251d4251f425
011800001332517320173251732017320173251332515325173201732517325173251532515325153251532513325133251332513325113251532515320153250000017325153251332511320113251532513325
011800001f42515425174251a4251f4201f4252142021425224202242022420224252242022425000002b4252b4252b4252942526425294252942526425244252642526425244252242524425224252142021425
011800001f4201f4201f4201f4201f4201f4201f4201f4250000022425214251f4251d4251a4251d4251f4251f4201f4201f4201f4201f4201f4201f4201f4251f4201f4201f4201f4201f4201f4201f4201f425
011800001332013320133201332013320133201332013325000001732515325133251132011325153251332513320133201332013320133201332013320133251332013320133201332013320133201332013325
011800001f4201f4201f4201f4201f425000001d4201d4251f4201f4201f4201f42515425174251a4251d4251f4201f4201f4201f4201f425000001d4201d4251f4201f4201f4201f4201f4201f4201f4201f425
010c0000000000940009400104001040015400154001040010400114001140011400000000000000000000000000009400094001040010400154001540018400184001c4001c4001c40000000000000000000000
011800001c12521125211252112021125231252412024125231252412024125211251f1201f1201f1201f1251c1051c1251f1201f1201f1201f12524100211251c12521125211252112021125231252412024125
011800001712017125171251712017125171251712017125171251712017125171251712017125171251712017125171251712017120171252110021100231001712017125171251712017125171251712017125
011800001332013325003000e3200e325003000a3200a3250f3050e3200e325003000a3200a3250e3050e3200e325003001132011325003000e3200e325003001332013325003000e3200e325003000a3200a325
011800002312524120241252612528120281202812028125001002412528120281202812028125241252612528120281252812528125261252412526120261252612526120261252f12524120241252412524125
011800001712517120171251712517120171251712517120171251712515120151201512500100001000010017125171051710517100171051710517125171051710517100171051710517125171051710517100
01180000003000632006325003000a3200a325003001132011325003000e3200e3200e1250e3000e305003000732513305003000e3000e3050030005325163050030012300123050030007325163050030011300
0118000023125211252312023125231252312524125261252812028125281052812028125241052412024125261052312023125231051c125211252112521120211251f125211202112021125231052410526105
0118000017105171051512015125151050010000100001001f1201f125001001d1201d125001001c1201c125001001a1201a125001001712500100001000010000100001001510500100001001a1201a1251a125
01180000003001530002320023251c30021300213001c30013320133251d3001132011325003001032010325003000e3200e3251d300073251c3001a3001830017300153001530015300153000e3200e3250e325
01180000211202112021125394003940032400211202112021125364003640036400364000000000000000000000032400354003b4003140032400314003b4001f1251c1251f125241202412500000241251f125
011800001f1201f125354002112021125324002312023125211251f1201f1251d1251a1251f1251f1251f1201f1252112522120221252112522120221251f1251a1201a1201a1201a1201a1251a1251a1201a120
011800001332013325243001532015325243001732524300003002130021300213001332500300003000e32500300243001332524300243000e32524300243001132520300203000e32500300003001132500300
01180000241252812028125244002440024400244002440000000214002140021400000000000000000000001f1251c1251f12524120241252340023125201252312528125241252612528120281252812528125
011800001a1201a1201a1251f1251a1251f1251f1251f1201f125211252212022125211252212022125241252212022120221202212022125221252112021120211253440034400344001f125000000000000000
01180000344000e32534300343001332534300343000e32535300353001332535300003000e32500300003001132534300343000e32538300383001232538300393000e325393003930013325000000000000000
011800002612524125261202612526125261202612523125241202412524125241252312521125231202312523125231252412526125281202812539400394003b4003b400261202612500000000000000000000
0118000034400344001d125394003c4003b40039400394051c12539400394003940000000000001a125000003c4003b4003c400394003b400394003b4001f1201f125394003940039400000001d1201d12500000
011800001c3001c3001132523300283002830023300233000f32524300243002430000300003000e325003001d3001d300233002330028300283002b30007320073252f3002f3002f30000300053200532500000
011800002412024125234002340028400284002312023125244002440024400244001c125211252112521120211251f125211202112021125214001f4001e4002112021120211251c40000000000002112021125
01180000000002d4002d4001a1201a1253940039400344003440018120181253540017125000000000000000000002d4002d40034400344001a1201a1251a1252612026125241252212022125211251f12500000
01180000000002d4002d4000432004325394003940034400344000232002325354000732500000000000000000000344003440035400354000e3200e3250e3251a3201a325183251732017325153251332500000
010c00000000016120161251610016105221002210022100221002210022100221002210500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010c000004500073200732507305073000730007300073000730007300073000730007305000000c5010e501155011d50122501255010000100001095010c50111501175011d5012450129501000010000100001
__music__
00 05514344
00 06070844
00 090a0b44
00 0c0d0e44
00 0f101144
00 12131444
00 15161744
00 18191a44
00 1b1c1d44
00 1e1f2044
00 21222344
00 24252644
00 27686944
02 285a6244
01 292a2b44
00 2c2d2e44
00 2f303144
00 32333444
00 35363744
00 38393a44
00 3b3c3d44
02 3e3f7f44
00 7b7c7d44
00 7e7f7a44
00 656f7b44
00 64707c44
00 65717d44
00 66724344
00 67734344
00 68744344
00 69754344

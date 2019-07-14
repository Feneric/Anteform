pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- anteform
-- by feneric

-- a function for logging to an output log file.
function logit(entry)
  printh(entry,'anteform.out')
end

-- register json context here
_tok={
 ['true']=true,
 ['false']=false}
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

function skip_delim(wrkstr, pos, delim, err_if_missing)
 if sub(wrkstr,pos,pos)!=delim then
  -- if(err_if_missing) assert'delimiter missing'
  return pos,false
 end
 return pos+1,true
end

function parse_str_val(wrkstr, pos, val)
  val=val or ''
  -- if pos>#wrkstr then
  --   assert'end of input found while parsing string.'
  -- end
  local c=sub(wrkstr,pos,pos)
  if(c=='"') return _g[val] or val,pos+1
  return parse_str_val(wrkstr,pos+1,val..c)
end

function parse_num_val(wrkstr,pos,val)
  val=val or ''
  -- if pos>#wrkstr then
  --   assert'end of input found while parsing string.'
  -- end
  local c=sub(wrkstr,pos,pos)
  -- support base 10, 16 and 2 numbers
  if(not match(c,"-xb0123456789abcdef.")) return tonum(val),pos
  return parse_num_val(wrkstr,pos+1,val..c)
end
-- public values and functions.

function json_parse(wrkstr, pos, end_delim)
  pos=pos or 1
  -- if(pos>#wrkstr) assert'reached unexpected end of input.'
  local first=sub(wrkstr,pos,pos)
  if match(first,"{[") then
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
    return parse_str_val(wrkstr,pos+1)
  elseif match(first,"-0123456789") then
    -- parse a number.
    return parse_num_val(wrkstr, pos)
  elseif first==end_delim then  -- end of an object or array.
    return nil,pos+1
  else  -- parse true, false
    for lit_str,lit_val in pairs(_tok) do
      local lit_end=pos+#lit_str-1
      if sub(wrkstr,pos,lit_end)==lit_str then return lit_val,lit_end+1 end
    end
    -- assert'invalid json token'
  end
end

-- initialization data
fullheight,fullwidth,halfheight,halfwidth=11,13,5,6
zombiecount=1
boatx,boaty=0,0

-- set up the various messages
winmsg="\n\n\n\n\n\n  congratulations, you've won!\n\n\n\n\n\n\n\n\n\n    press p to get game menu,\n anything else to continue and\n      explore a bit more."
losemsg="\n\n\n\n\n\n      you've been killed!\n          you lose!\n\n\n\n\n\n\n\n\n\n\n\n    press p to get game menu"
helpmsg="anteform commands:\n\na: attack\nc: concentrate on action\nd: dialog, talk, buy\ne: enter, board, mount, climb,\n   descend\np: pause, save, load, help\nf: use flashlight; force chest\ns: sit & wait\nw: wearing & wielding\nx: examine, look (repeat to\n   search or read)\n\nfor commands with options (like\nconcentrating or buying) use the\nfirst character from the list,\nor anything else to cancel."
msg=helpmsg

-- anyobj is our root object. all others inherit from it to
-- save space and reduce redundancy.
anyobj={f=1,mva=0,nm=0,mvp=0,hd=0,ch=0,z=0}

function makemetaobj(base,basetype)
  return setmetatable(base,{__index=base.ot or basetype})
end

-- the types of terrain that exist in the game. each is
-- given a name and a list of allowed monster types.
terrains={"plains","bare ground","hills","scrub","swamp","forest","foothills","mountains","tall mountain","filing cabinet","bed","water","water","deep water","deep water","brick","brick road","brick","mismatched brick","stone","stone","road","barred window","window","bridge","ladder down","ladder up","door","locked door","open door","sign","crater","cave","facility","monastery","town","village","helipad","fountain","chair","desk"}
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
basetypes=json_parse('[{"dmg":13,"ch":1,"t":[1,2,3,4,5,6,7,8,10,16,17,22,26,27,30,31,32,33,38,40,41],"exp":2,"hp":10,"gp":0,"hos":1,"dex":8,"mva":1,"ar":1},{"mxy":64,"mxm":0,"newm":0,"fri":1,"mny":0,"mnx":80,"mxx":128,"mn":0},{"ss":17,"sz":1,"c":{},"mny":1,"newm":25,"mxy":9,"mn":0,"sy":1,"sx":1,"mxm":27,"fri":false,"mxx":9,"mnx":1,"sf":1,"dg":1},{"fm":1,"p":1,"ia":70,"i":70,"n":"sailboat","f":2},{"p":1,"shm":-2,"ia":94,"i":94,"n":"chest","szm":11},{"iseq":12,"szm":14,"fi":1,"p":1,"i":39,"n":"fountain","shm":-2},{"p":1,"shm":12,"ia":27,"i":27,"n":"ladder up","szm":20},{"p":1,"shm":-3,"ia":26,"i":26,"n":"ladder down","szm":20},{"ar":0,"gp":10,"hos":false,"i":80,"n":"human","exp":1},{"d":["chirp chirp!","gree gree!"],"po":1,"hp":23,"ch":0,"n":"giant ant","exp":8},{"d":["ahhrg!","urg!"],"exp":8,"gp":10,"ch":0,"dmg":14,"n":"zombie","dex":6},{"ch":3,"hp":12,"gp":0,"exp":5,"dex":10,"n":"animal","ar":0},{"d":["the woods are scary now.","i\'m safer at home."],"dex":9,"hp":12,"dmg":20,"ar":3,"i":82,"n":"hunter","cs":[{},[[4,5],[15,4]]]},{"d":["i\'m doing my best.","i\'m trying to stop it."],"ar":12,"hp":85,"dmg":60,"mva":0,"i":90,"n":"cop","cs":[{},[[15,4]]]},{"d":["consume!","stuff makes you happy!"],"fi":1,"i":77,"n":"merchant","cs":[{},[[1,4],[4,15],[6,1],[14,13]],[[1,4],[6,5],[14,10]],[[1,4],[4,15],[6,1],[14,3]]]},{"d":["pardon me.","well i never."],"fi":1,"i":81,"n":"lady","cs":[{},[[2,9],[4,15],[13,14]],[[2,10],[4,15],[13,9]],[[2,11],[13,3]]]},{"d":["this isn\'t right.","this isn\'t even wrong."],"i":92,"n":"scientist","cs":[{},[[6,12],[15,4]],[[6,12]],[[15,4]]]},{"d":["you\'re blocking the sun!","gonna get toasted!"],"fi":1,"dex":12,"i":78,"n":"sunbather","cs":[{},[[8,12],[15,4]],[[8,12]],[[15,4]],[[8,14]]]},{"d":["check out these pecs!","i\'m jacked!"],"fi":1,"dex":12,"i":79,"n":"bodybuilder","cs":[{},[[8,12],[15,4]],[[8,12]],[[15,4]]]},{"d":["be at peace.","meditate on it."],"i":84,"n":"monk","ac":[{},[[2,5],[15,4]],[[2,4]],[2,5]]},{"d":["gotta do homework.","weekend is almost here."],"i":86,"n":"student","cs":[{},[[15,4]],[[1,2],[15,4]],[[1,2]]]},{"d":["the animals aren\'t right.","mom says to stay inside."],"i":75,"n":"child","cs":[{},[[15,4]],[[11,14],[3,8],[15,4]],[[11,14],[3,8]]]},{"d":["spooky stuff\'s afoot.","missing people; crazy animals."],"i":88,"n":"citizen","cs":[{},[[1,5],[8,2],[4,1],[2,12],[15,4]]]},{"n":"grocer","mch":"food"},{"n":"clerk","mch":"armor"},{"n":"vendor","mch":"weapons"},{"n":"medic","mch":"hospital"},{"n":"dealer","mch":"guild"},{"mch":"bar","i":81,"n":"bartender"},{"i":100},{"n":"robed zombie","i":102},{"n":"worker ant"},{"t":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,22,25,26,27,30,31,32,33,38,40,41],"i":110,"n":"winged ant","exp":9},{"exp":12,"i":118,"n":"soldier ant","hp":50},{"hp":120,"exp":20,"i":125,"n":"queen ant","dmg":35},{"exp":5,"hp":10,"fi":1,"dmg":5,"i":123,"n":"ant larva","mva":0},{"exp":10,"hp":5,"fi":1,"dmg":0,"i":124,"n":"ant eggs","mva":0},{"po":1,"hp":5,"gp":8,"i":106,"n":"large spider","exp":4},{"exp":2,"po":1,"hp":4,"gp":2,"eat":1,"i":108,"n":"large rat","dmg":10},{"d":["grrr!"],"i":96,"n":"coyote"},{"d":["grar!"],"i":120,"n":"lynx"},{"ns":["snake","serpent"],"po":1,"hp":7,"ch":1,"i":112,"t":[4,5,6,7],"dmg":8},{"po":1,"hp":8,"ch":1,"i":114,"n":"rattlesnake","exp":6},{"t":[5,12,13,14,15,16],"hp":20,"i":116,"n":"large eel","exp":7},{"po":1,"hp":8,"fi":1,"i":95,"n":"scorpion","ch":1},{"fi":1,"ch":0,"n":"slime","t":[17,22,23],"exp":2,"gp":10,"eat":1,"i":122,"cs":[{},[[3,9],[11,10]],[[3,14],[11,15]]]},{"ns":["big catfish","sturgeon"],"hp":25,"ch":2,"i":98,"exp":9,"t":[12,13,14,15,16]}]')
-- give our base objects ns for convenience & efficiency.
shiptype=basetypes[4]

-- set our base objects base values. the latter portion is
-- our bestiary. it holds all the different monster types that can
-- be encountered in the game. it builds off of the basic types
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
  elseif basetypenum<32 then
    basetype=basetypes[11]
  elseif basetypenum<38 then
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
    return checkpurchase({"$15 for 25 food; a\80\80\82\79\86\69? "},
      function(cmd)
        if cmd=='a' then
          return hero.gp>=15
        else
          return nil
        end
      end,
      function()
        hero.gp-=15
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
        if cmd=='a' then
          return hero.gp>=5
        else
          return nil
        end
      end,
      function()
        hero.gp-=5
        rumors=json_parse('["they\'re not hunters.","i think they\'re aliens.","funny citronella smell.","they smell like lemons.","they act like zombies.","they look burned.","the cult knows something.","they spook the animals."]')
        update_lines{"while socializing, you hear:"}
        return '"'..rumors[flr(rnd(8)+1)]..'"'
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
armors=json_parse('{"south":{"n":"cloth","a":8,"p":12},"west":{"n":"leather","a":23,"p":99},"east":{"n":"flak","a":40,"p":300},"north":{"n":"vest","a":70}}')
armornames=makenameforamount(armors)

-- weapon definitions
weapons=json_parse('{"d":{"n":"dagger","a":8,"p":8},"c":{"n":"club","a":12,"p":40},"a":{"n":"axe","a":18,"p":75},"s":{"n":"shotgun","a":40}}')
weaponnames=makenameforamount(weapons)

-- spell definitions
spells=json_parse('{"a":{"n":"aim","c":3,"a":1},"f":{"n":"first aid","c":5,"a":1,"p":8},"c":{"n":"cure","c":7,"p":10},"x":{"n":"medic","c":17,"a":6,"p":25}}')

-- tool definitions
tools=json_parse('{"west":{"n":"4 batteries","attr":"ts","p":12,"q":4},"east":{"n":"a key","attr":"keys","p":23,"q":1}}')

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
  maps=json_parse('[{"n":"village","mny":24,"ey":37,"i":[{"id":6,"x":103,"y":38}],"sy":54,"signs":[{"y":27,"x":107,"msg":["an engagement ring; steve","was ready to propose."]},{"y":33,"x":102,"msg":["mary studied astronomy","as a hobby."]},{"y":33,"x":107,"msg":["lou was a mixed martial","arts champion."]}],"sx":96,"ex":57,"mxy":56,"mxx":112,"c":[{"id":26,"x":91,"y":27},{"id":13,"x":89,"y":30},{"id":25,"x":85,"y":27},{"id":24,"x":100,"y":44},{"id":29,"x":108,"y":37},{"d":["welcome to anteform valley.","we\'re all glad you\'re here."],"id":16,"x":87,"y":37},{"id":22,"x":86,"y":51},{"d":["greybeard hid his treasure!","it\'s on big sister island!"],"id":22,"x":80,"y":52},{"d":["steve, lou, & mary are gone.","i\'m not straying far."],"id":23,"x":109,"y":40},{"d":["radio square is southwest.","folks are missing there too."],"id":23,"x":98,"y":47},{"n":"sally","id":21,"x":108,"y":30,"d":["please find steve.","our rooms are adjacent."]},{"mva":1,"id":14,"x":105,"y":37}]},{"n":"thinktank","ey":33,"i":[{"id":8,"x":123,"tx":126,"tz":0,"y":4,"tm":3,"ty":33}],"sy":23,"sx":116,"ex":26,"mnx":104,"mxy":24,"c":[{"id":14,"x":116,"y":18},{"id":24,"x":110,"y":17},{"id":27,"x":106,"y":1},{"d":["natural science is dangerous.","who\'d have thought?"],"id":17,"x":109,"y":12},{"d":["concentrated it can burn.","a simple carboxyl."],"id":17,"x":110,"y":22},{"d":["by \'form\' they meant acid.","it\'s always been here."],"id":17,"x":125,"y":15}]},{"n":"the basement","sx":126,"mny":32,"i":[{"id":7,"x":126,"tx":123,"tz":0,"y":33,"tm":2,"ty":4}],"mnx":112,"mxy":41,"sy":33,"c":{}},{"n":"monastery","ey":17,"i":[{"id":7,"x":84,"tx":113,"tz":0,"y":9,"tm":5,"ty":31},{"id":6,"x":92,"y":6}],"sy":23,"signs":[{"y":20,"x":92,"msg":"he\'ll rise again!"}],"sx":92,"ex":34,"mxy":24,"mxx":105,"c":[{"n":"brother meinrad","id":20,"x":89,"y":21,"d":["you are welcome in peace.","you come at a bad time."]},{"id":27,"x":85,"y":15},{"id":24,"x":99,"y":15},{"n":"brother anthony","id":20,"x":81,"y":11},{"n":"sister pat","id":20,"x":92,"y":1,"d":["i saw the flash in heaven.","the animals now punish us."]},{"n":"learner jo","id":21,"x":103,"y":6,"d":["we have been cursed.","so many have been taken."]},{"n":"learner brent","id":21,"x":82,"y":5,"d":["i found the star jelly.","i think it turned the beasts."]},{"n":"brother dominic","id":20,"x":90,"y":6,"d":["god sent us a sign.","i saw his star fall to earth."]}]},{"n":"the top floor","sx":113,"mny":24,"i":[{"id":8,"x":113,"tx":84,"tz":0,"y":31,"tm":4,"ty":9}],"mnx":112,"mxy":33,"sy":31,"c":{}},{"n":"hermit cabin","mny":56,"ey":47,"i":[{"id":4,"x":110,"y":61}],"sy":57,"sx":111,"ex":3,"mnx":103,"mxx":113},{"n":"radio square","mny":43,"ey":53,"sy":62,"signs":[{"y":53,"x":114,"msg":["a reminder to investigate","monks for radio news show."]}],"sx":119,"ex":34,"mnx":112,"c":[{"id":26,"x":114,"y":45},{"id":24,"x":123,"y":60},{"id":28,"x":123,"y":44},{"d":["the hermit has a sailboat.","west past the billabong."],"id":13,"x":126,"y":46},{"d":["it\'s in the northeast cabin.","you can borrow my shotgun."],"id":13,"x":114,"y":48},{"id":15,"x":121,"y":53,"d":["the monks have seen them.","the woods weirdos."],"i":80},{"d":["hang out in the bar none.","good woods weirdos discussion."],"id":23,"x":124,"y":58}]},{"n":"southern cabin","mny":56,"ey":60,"sy":62,"sx":100,"ex":38,"mnx":96,"mxx":104,"c":[{"d":["you can borrow my vest.","you\'ll need it."],"id":13,"x":101,"y":59}]},{"n":"pennisula cabin","mny":56,"ey":24,"sy":62,"sx":100,"ex":56,"mnx":96,"mxx":104,"c":[{"id":19,"x":99,"y":58},{"id":18,"x":98,"y":58}]},{"n":"lakeside cabin","mny":56,"ex":41,"mxx":104,"ey":39,"mnx":96,"sy":62,"sx":100},{"n":"western cabin","mny":56,"ex":21,"mxx":104,"ey":28,"mnx":96,"sy":62,"sx":100},{"n":"hunting cabin","mny":56,"ex":75,"mxx":104,"ey":3,"mnx":96,"sy":62,"sx":100},{"n":"the queen\'s chamber","fri":false,"mny":56,"i":[{"id":7,"x":94,"tx":3,"tz":1,"y":62,"tm":16,"ty":6}],"sy":31,"sx":113,"mnx":80,"mxx":96,"c":[{"id":35,"x":84,"y":60},{"id":37,"x":83,"y":59},{"id":37,"x":83,"y":60},{"id":37,"x":84,"y":61},{"id":36,"x":84,"y":59},{"id":36,"x":93,"y":58},{"id":34,"x":89,"y":57},{"id":34,"x":89,"y":62},{"id":34,"x":94,"y":57},{"id":33,"x":82,"y":57},{"id":33,"x":82,"y":62}]},{"n":"greybeard\'s cave","ex":57,"l":[[0,-196,782,13263,15564,12288,16380,16384],[2,-12481,961,12348,16332,12,-3124,192],[12301,13260,192,15612,12348,13056,-3076,192]],"ey":33,"i":[{"z":1,"id":7,"x":1,"y":8},{"z":2,"id":7,"x":8,"y":3},{"z":3,"id":7,"x":8,"y":1},{"z":3,"id":5,"x":4,"y":8}],"sy":8,"c":[{"z":3,"id":46,"x":1,"y":8},{"z":3,"id":46,"x":4,"y":3},{"z":3,"id":46,"x":3,"y":5}]},{"n":"vetusaur mine","sx":8,"ex":3,"l":[[205,15360,2876,16320,12351,-3328,1020,-19711],[48,16128,14332,768,-244,780,13119,28672]],"ey":9,"i":[{"z":1,"id":7,"x":8,"y":8},{"tz":0,"id":7,"x":8,"tx":3,"z":1,"y":1,"tm":0,"ty":5},{"z":2,"id":7,"x":3,"y":3},{"z":2,"id":7,"x":1,"y":8}],"sy":8,"c":{}},{"n":"formika mine","sx":4,"ex":7,"l":[[204,12480,13308,12300,16332,14348,16380,256]],"ey":3,"i":[{"z":1,"id":7,"x":4,"y":8},{"tz":0,"id":8,"x":3,"tx":94,"z":1,"y":6,"tm":13,"ty":62}],"sy":8,"c":{}}]')
  -- map 0 is special; it's the world map, the overview map.
  maps[0]=json_parse('{"n":"world","mnx":0,"mny":0,"mxx":80,"mxy":64,"wrap":false,"newm":10,"mxm":12,"fri":false,"ss":0,"signs":[{"x":66,"y":43,"msg":"northwest - village"},{"x":68,"y":40,"msg":"north - monastery"},{"x":68,"y":40,"msg":"west - thinktank"},{"x":58,"y":21,"msg":"west - monastery"}]}')

  -- the creatures structure holds the live copy saying which
  -- creatures (both human and monster) are where in the world.
  -- individually they are instances of bestiary objects or
  -- occupation type objects.
  creatures={}

  -- perform the per-map data structure initializations.
  for mapnum=0,#maps do
    local maptype
    curmap=maps[mapnum]
    if mapnum>0 then
      if curmap.l then
        maptype=basetypes[3]
      else
        maptype=basetypes[2]
      end
      makemetaobj(curmap,maptype)
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
  hero=json_parse('{"i":0,"ar":0,"dmg":0,"x":67,"y":50,"z":0,"exp":0,"lvl":0,"str":8,"int":8,"dex":8,"st":0,"hd":0,"f":0,"gp":20,"fd":25,"mvp":0,"mp":8,"hp":24,"keys":0,"ts":2,"lit":0}')
  hero.color=rnd(10)>6 and 4 or 15
 
  -- make the map info global for efficiency
  mapnum=0
  setmap()

  turn=0
  turnmade=false
  cycle=0
  _update=world_update
  _draw=world_draw
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
  cartdata("minima0")
end

function listcommands()
  msg=helpmsg
  if _draw~=msg_draw then
    draw_state=_draw
    _draw=msg_draw
  end
end

attrlist={'ar','dmg','x','y','str','int','dex','st','i','color','f','keys','ts','exp','lvl','gp','fd','mp','hp'}

function combinevalues(highval,lowval)
  return bor(shl(highval,8),lowval)
end

function savegame()
  if mapnum~=0 then
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
    dset(storagenum,boatx)
    dset(storagenum+1,boaty)
    storagenum+=2
    for creaturenum=1,12 do
      local creature=creatures[0][creaturenum]
      if creature then
        dset(storagenum,creature.id)
        dset(storagenum+1,combinevalues(creature.x,creature.y))
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
  boatx=dget(storagenum)
  boaty=dset(storagenum+1)
  if boatx>0 or boaty>0 then
    maps[0].con[boatx][boaty][0]=makemetaobj{ot=basetypes[4]}
  end
  storagenum+=2
  for creaturenum=1,12 do
    creatureid=dget(storagenum)
    if creatureid~=0 then
      creaturex,creaturey=separatevalues(dget(storagenum+1))
      definemonster{ot=basetypes[creatureid],x=creaturex,y=creaturey,mn=0}
      storagenum+=2
    else
      break
    end
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
    update_lines{spell.n.." is performed! "..(extra or '')}
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
  if targmap.dg then
     _draw=dungeon_draw
     hero.f,hero.z=targmap.sf,targmap.sz
  end
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
      update_lines{"choose a\73\77, f\73\82\83\84 \65\73\68, c\85\82\69,","m\69\68\73\67: "}
      cmd=yield()
      if cmd=='c' then
        -- cast cure
        if checkspell(cmd) then
          sfx(3)
          hero.st=band(hero.st,14)
        end
      elseif cmd=='f' or cmd=='x' then
        -- cast healing
        if checkspell(cmd) then
          sfx(3)
          increasehp(spells[cmd].a*hero.int/2)
        end
      elseif cmd=='w' or cmd=='a' then
        -- cast offensive spell
        if checkspell(cmd,'dir:') then
          local spelldamage=rnd(spells[cmd].a*hero.int/2)
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
        local chestgold=ceil(rnd(100)+100)
        hero.gp+=chestgold
        update_lines{"you find "..chestgold.." gp."}
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
          if curmap.dg and not maps[curobj.tm].dg then
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
      elseif curobjname=='sailboat' then
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
        update_lines{"the battery died."}
      end
    end
    if _draw==dungeon_draw and hero.lit<1 then
      update_lines{"it's dark!"}
    end
    cmd=yield()
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
  if monster.pn then
    monster.n=monster.pn
  elseif objtype.ns then
    monster.n=objtype.ns[flr(rnd(#objtype.ns)+1)]
  end
  --if(objtype.is)monster.i=objtype.is[flr(rnd(#objtype.is)+1)]
  if(objtype.cs)monster.co=objtype.cs[flr(rnd(#objtype.cs)+1)]
  monster.iseq=flr(rnd(30))
  monster.ia=false
  add(creatures[monster.mn],monster)
  maps[monster.mn].con[xcoord][ycoord][zcoord]=monster
  return monster
end

function create_monster()
  local monsterx=flr(rnd(curmap.w))+curmap.mnx
  local monstery=flr(rnd(curmap.h))+curmap.mny
  local monsterz=curmap.dg and flr(rnd(#curmap.l)+1) or 0
  if contents[monsterx][monstery][monsterz] or monsterx==hero.x and monstery==hero.y and monsterz==hero.z then
    -- don't create a monster where there already is one
    monsterx=nil
  end
  if monsterx then
    local monsterspot=mget(monsterx,monstery)
    if curmap.dg then
      monsterspot=getdungeonblk(monsterx,monstery,monsterz,1)
    end
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
  if hero.hp<=0 then
    msg=losemsg
    -- draw_state=_draw
    _draw=msg_draw
  end
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
  return min(num,32767)
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
  local newx,newy=hero.x,hero.y
  local xcoord,ycoord,zcoord=hero.x,hero.y,hero.z
  local cmd=direction>0 and 'advance' or 'retreat'
  local item
  local iscreature=false
  if hero.f==1 then
    newy-=direction
    result=getdungeonblk(xcoord,newy,zcoord)
    item=contents[xcoord][newy][zcoord]
  elseif hero.f==2 then
    newx+=direction
    result=getdungeonblk(newx,ycoord,zcoord)
    item=contents[newx][ycoord][zcoord]
  elseif hero.f==3 then
    newy+=direction
    result=getdungeonblk(xcoord,newy,zcoord)
    item=contents[xcoord][newy][zcoord]
  else
    newx-=direction
    result=getdungeonblk(newx,ycoord,zcoord)
    item=contents[newx][ycoord][zcoord]
  end
  if item and item.hp then
    iscreature=true
  end
  if result==3 or iscreature then
    blocked(cmd)
  else
    xcoord,ycoord=newx,newy
    sfx(0)
    update_lines{cmd}
  end
  turnmade=true
  return xcoord,ycoord,zcoord
end

function checkexit(xcoord,ycoord)
  if not curmap.wrap and(xcoord>=curmap.mxx or xcoord<curmap.mnx or ycoord>=curmap.mxy or ycoord<curmap.mny) then
    update_lines{cmd,"exiting "..curmap.n.."."}
    mapnum=0
    return true
  else
    return false
  end
end

function blocked(cmd)
  sfx(5)
  update_lines{cmd,"blocked!"}
  return false
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
    if hero.i==0 then
      sfx(0)
    end
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
  local response=nil
  for sign in all(curmap.signs) do
    if xcoord==sign.x and ycoord==sign.y then
      if mget(xcoord,ycoord)==31 then
       -- it's an actual sign
       response={"(read sign)",sign.msg}
      else
       -- it's in a desk or filing cabinet or something
       response=sign.msg
      end
      break
    end
  end
  return response
end

function look_results(ldir,xcoord,ycoord)
  local cmd,signcontents,content="examine: "..ldir,check_sign(xcoord,ycoord),contents[xcoord][ycoord][hero.z] or nil
  if signcontents then
    update_lines{cmd..signcontents[1],signcontents[2]}
  elseif content then
    update_lines{cmd,content.n}
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
  if magic then
    damage+=magic
  end
  --if creature then
    --logit('creature: '..(creature.name or 'nil')..' '..(creature.x or 'nil')..','..(creature.y or 'nil')..','..(creature.z or 'nil'))
  --else
    --logit('creature: nil '..xcoord..','..ycoord..' '..adir)
  --end
  if creature and creature.hp then
    if magic or rnd(hero.dex+hero.lvl*8)>rnd(creature.dex+creature.ar) then
      damage-=rnd(creature.ar)
      creature.hd=3
      sfx(1)
      creature.hp-=damage
      if creature.hp<=0 then
        hero.gp=not_over_32767(hero.gp+creature.gp)
        increasexp(creature.exp)
        contents[xcoord][ycoord][zcoord]=nil
        update_lines{cmd,creature.n..' killed; xp+'..creature.exp..' gp+'..creature.gp}
        if creature.n=='queen ant' then
          msg=winmsg
          _draw=msg_draw
        end
        del(creatures[mapnum],creature)
      else
        update_lines{cmd,'you hit the '..creature.n..'!'}
      end
      if curmap.fri then
        for townie in all(creatures[mapnum]) do
          townie.hos=1
          townie.d={"you're a lawbreaker!","criminal!"}
          if townie.n=='cop' then
            townie.mva=1
          end
        end
      end
    else
      update_lines{cmd,'you miss the '..creature.n..'!'}
    end
  elseif mget(xcoord,ycoord)==29 then
    -- bash locked door
    sfx(1)
    if(not magic)deducthp(1)
    if rnd(damage)>12 then
      update_lines{cmd,'you break open the door!'}
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
  local dx=abs(x1-x2)
  if curmap.wrap and dx>curmap.w/2 then
    dx=curmap.w-dx;
  end
  local dy=abs(y1-y2)
  if curmap.wrap and dy>curmap.h/2 then
    dy=curmap.h-dy;
  end
  return dx+dy
end

function calculatemoves(creature)
  local maxx,maxy=curmap.mxx,curmap.mxy
  local creaturex,creaturey=creature.x,creature.y
  local eastspot,westspot=(creaturex+curmap.w-1)%maxx,(creaturex+1)%maxx
  local northspot,southspot=(creaturey+curmap.h-1)%maxy,(creaturey+1)%maxy
  if not curmap.wrap then
    northspot,southspot,eastspot,westspot=creaturey-1,creaturey+1,creaturex-1,creaturex+1
    if creature~=hero then
      northspot,southspot,eastspot,westspot=max(northspot,curmap.mny),min(southspot,maxy-1),max(eastspot,curmap.mnx),min(westspot,maxx-1)
    end
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
            if currentdistance<bestdistance or (currentdistance==bestdistance and rnd(10)<5) then
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
          if rnd(10)<5 then
            if cfacing and rnd(10)<5 then
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
        if curmap.dg then
          newloc=getdungeonblk(desiredx,desiredy,desiredz,1)
        end
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
  for delaycount=0,numofcycles do
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

function draw_stats()
  local linestart,midlinestart,longlinestart=106,110,119
  print("cond",linestart,0,5)
  print(band(hero.st,1)==1 and 'p' or 'g',125,0,6)
  print("lvl",linestart,8,5)
  print(hero.lvl,longlinestart,8,6)
  print("hp",linestart,16,5)
  print(hero.hp,linestart+8,16,6)
  print("ap",linestart,24,5)
  print(hero.mp,linestart+8,24,6)
  print("$",linestart,32,5)
  print(hero.gp,midlinestart,32,6)
  print("f",linestart,40,5)
  print(hero.fd,midlinestart,40,6)
  print("exp",linestart,48,5)
  print(hero.exp,linestart,55,6)
  print("dex",linestart,63,5)
  print(hero.dex,longlinestart,63,6)
  print("int",linestart,71,5)
  print(hero.int,longlinestart,71,6)
  print("str",linestart,79,5)
  print(hero.str,longlinestart,79,6)
  for linenum=1,numoflines do
    print(lines[(curline-linenum)%numoflines+1],0,128-linenum*8)
  end
end

function substitutecolors(colorsubs)
  if colorsubs then
    for colorsub in all(colorsubs) do
      pal(colorsub[1],colorsub[2])
    end
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
  local tmp=triple[1]
  triple[1]=triple[3]
  triple[3]=tmp
end

function getdungeonblks(mapx,mapy,mapz,facing)
  local blks={}
  if facing%2==0 then
    -- we're looking for a column
    for viewy=mapy-1,mapy+1 do
      add(blks,{
        blk=getdungeonblk(mapx,viewy,mapz),
        x=mapx,
        y=viewy
      })
    end
    if facing==4 then
      triplereverse(blks)
    end
  else
    -- we're looking for a row
    for viewx=mapx-1,mapx+1 do
      add(blks,{
        blk=getdungeonblk(viewx,mapy,mapz),
        x=viewx,
        y=mapy
      })
    end
    if facing==3 then
      triplereverse(blks)
    end
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
    if facing==4 then
       triplereverse(blks)
    end
  else
    for viewy=mapy-3+facing,mapy-1+facing do
      add(blks,getdungeonblks(viewx,viewy,mapz,facing))
    end
    if facing==3 then
      triplereverse(blks)
    end
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
      if row[1].blk==3 then
        rectfill(topouter,topouter,topinner,bottomouter,0)
        line(lowextreme,topouter,topouter,topouter,5)
        line(topouter,topouter,topinner,topinner)
        line(topouter,bottomouter,topinner,bottominner)
        line(lowextreme,bottomouter,topouter,bottomouter)
      end
      if row[3].blk==3 then
        rectfill(bottominner,topinner,bottomouter,bottomouter,0)
        line(bottomouter,topouter,highextreme,topouter,5)
        line(bottominner,topinner,bottomouter,topouter)
        line(bottominner,bottominner,bottomouter,bottomouter)
        line(bottomouter,bottomouter,highextreme,bottomouter)
      end
      if depthindex>1 then
        local leftoneback,centeroneback,rightoneback=view[depthindex-1][1].blk,view[depthindex-1][2].blk,view[depthindex-1][3].blk
        if (row[1].blk==centeroneback and row[1].blk==3) or
          (row[1].blk~=leftoneback) then
          line(topinner,topinner,topinner,bottominner,5)
        end
        if (row[3].blk==centeroneback and row[3].blk==3) or
          (row[3].blk~=rightoneback) then
          line(bottominner,topinner,bottominner,bottominner,5)
        end
        if centeroneback==3 and leftoneback==3 and row[1].blk~=3 then
          line(topinner,lowerase,topinner,higherase,0)
        end
        if centeroneback==3 and rightoneback==3 and row[3].blk~=3 then
          line(bottominner,lowerase,bottominner,higherase,0)
        end
      end
      if row[2].blk==3 then
        rectfill(topouter,topouter,bottomouter,bottomouter,0)
        line(topouter,topouter,bottomouter,topouter,5)
        line(topouter,bottomouter,bottomouter,bottomouter)
        if row[1].blk<3 then
          line(topouter,topouter,topouter,bottomouter)
        end
        if row[3].blk<3 then
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
  local width,height,wrap=curmap.w,curmap.h,curmap.wrap
  local xtraleft,xtratop,xtrawidth,xtraheight,scrtx,scrty,left,right=0,0,0,0,0,0,hero.x-halfwidth,hero.x+halfwidth
  if left<minx then
    xtrawidth=minx-left
    scrtx=xtrawidth
    if wrap then
      xtraleft=left%width+minx
    end
    left=minx
  elseif right>=maxx then
    if wrap then
      xtrawidth=fullwidth-right+maxx-1
      scrtx=xtrawidth
      xtraleft=left
      left,right=minx,right%width+minx
    else
      xtrawidth=right-maxx+1
      right=maxx
    end
  end
  local top,bottom=hero.y-halfheight,hero.y+halfheight
  if top<miny then
    xtraheight=miny-top
    scrty=xtraheight
    if wrap then
      xtratop=top%height+miny
    end
    top=miny
  elseif bottom>=maxy then
    if wrap then
      xtraheight=fullheight-bottom+maxy-1
      scrty=xtraheight
      scrty,xtratop=xtraheight,top
      top,bottom=miny,bottom%height+miny
    else
      xtraheight=bottom-maxy+1
      bottom=maxy
    end
  end
  local mainwidth,mainheight=min(fullwidth-xtrawidth,curmap.w),min(fullheight-xtraheight,curmap.h)
  if cycle%16==0 then
    for sprnum=12,14,2 do
      animatespr(sprnum)
    end
  end
  cycle+=1
  cls()
  if wrap then
    if xtrawidth then
      -- we have an edge
      draw_map(xtraleft,top,0,scrty,xtrawidth,mainheight)
    end
    if xtraheight then
      -- we have a top
      draw_map(left,xtratop,scrtx,0,mainwidth,xtraheight)
    end
    if xtrawidth and xtraheight then
      -- we have a corner
      draw_map(xtraleft,xtratop,0,0,xtrawidth,xtraheight)
    end
  end
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
440000444440444055505550fff0fff055005550550055500000050055555555ccccccccfff0fff0000000005545545555555555555555555555555500000000
444444440000000000000000000000005550555055505550050000005d0d0d05c000000c00000000004004000044440054444405544444055400000500055000
000000004044404450555055f0f4f0ff5055505550555055000000005d0d0d05c000000cf0fff0ff004444000040040054444405544444055400000504444440
000000000000000000000000000000000055505500505055000500005d0d0d05c000000c00000000004004000044440054444405544444055400000507777770
000000004440444055505550fff0fff05550555055555550000000055d0d0d05c000000cfff0fff0004444000040040054449405544494059490000507777770
000000000000000000000000000000005550055555500555000000005d0d0d05c000000c00000000004004000044440054444405544aa4055400000507777770
444444444044404450555055f0fff0ff5055505550555055000050005d0d0d05c000000cf0fff0ff004444000040040054444405544444055400000500055000
4400004400000000000000000000000000550000005500005000000055555555cccccccc00000000554554550000000054444405544444055400000500055000
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
0600006006000060060006000666666000000000000000000066000006626600000677000677760000776000000ff000000ff00000044004f00ff000000ff00f
0600006000600600060006000000060000000000000000000600600007626700006777000777770000777600000ff000000ff00000eeeeeeff8ff8ffffffffff
0600006000066000006060000000600006666660000000000066000007626700066777000777770000777660f3bbbb0000bbbb004eeeee000088880ff0ffff00
00600600006006000006000000060000000000000000000006006060076267000066770007747700007766000b0bb3bffb3bb3b00e666600000ff000000ff000
0066660006000060000600000060000000000000000000000600060007727700400620000664660000026004000bb000000bb0f0006006000008800000088000
006006000600006000060000066666600000000000000000006660600444440004444444044444004444444000111100001111000060060000f00f0000f00f00
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
00bb008b00b00b80000008f000008f00000100300001030010100101151001510044490000444000033b3330004ff40007f0ff00101001011510015109088090
0b00b0b00b0b00b0500004000050400001033031013303010501105050011005008489900084890003333b33004444000ff000000501105050011005008aa800
0b00b0b00b00b0b00f00ff000f00ff000030033003003100501001055010010507f4f70007f4f79033b33333004ff400007f07f05010010550100105008aa800
00b00bb0b000bb00044444f0004444f010300100010100001108801101088010047f7400007f794033333b300044440000f7fff0111881110118811009088090
000b00000b00000000ffff0000ffff00010100001000000010011001100110014494940004044990033b33000048840007fff00010c11c0110c11c0100900900
00000000000000000000000000000000000000100010010000c00c0010c00c01000099004400000000033000000440000ff00000000110001001100100000000
909090807070704040606060606060606010101010101010106110c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c0404010c0c0c0c010
61202020203070907070707080809090202020401111402020204011114020202020209191919191111191919191912041414141414141414141414141414141
909090807070404040606060606060106060601010101010106122c0c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c010701240c0c0c0c010
612020202030708080709080809090902020202011111111111111111111112020202091118292911111919282119120419282614192826151a0a0a0a041b141
90909080707040404060606060601010606060101010101010611010c0c0c0c0c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c01010c0c0e0e0c010
612020202030708090707080809090902020202020202020202020202020112020202091b01111d11111d11111b0912041616161416161614141414141416141
90908080704040606060606060606060601060601010101010611010c0101010c0c0c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c0c0c0e0e0c0c010
61616120307080908090709080909090202020209191919191919191202011202020209191919191c1c191919191912041615454416154544182929282416141
9090808070704060606060606060106060101010101010101061c0c0c01010101010c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c0c0c0c0c0c0c01010
61206161303080807070707080809090202020209154d38304735491202011202020202020111111111191b2a2b3912041616192416161924161616161416141
90908080707040404040404010101010101010101010101010610161616110101010c0c0e0e0e0c0c0e0e0e0e0e0e0e0e0c0c0c0c0c0c0c0c0526110c0c01010
612020612020307070707070808090902020202091a011111111a0914020112020202020111140c0401191111111912041616182416161824161616161416141
9090908070704040101010101010101010101010101010101010c01010611010101010c0c0c0c0c0c0c0c0c0c0e0e0e0c0c0c0c0c01010101010611010101010
6120206120203030303070708080909020202020915413a25353549140401111111111111140c072c091738373e291204141d1414141d1414141d14141416141
9090808070701010c0c0c0c0c0c0c0101010c0c0c0c0c0c0c0c0c010206161616161101010d0101061421010c0c0c0c0c0c0c010101010101010616161616161
612020612020203030307070809090902020202091111111111111111111112020202020111140c0401111111111912041616161616161616161616161d16141
90908080707010c0c01010101010c0c0c0c0c01010101010c0101010206120202061101010d01010611010101010101010101010101020202010611010101010
20202061f12020202030707080809090202020209111111111111191404011202020202020111111111111111111912041414141414141414141414141414141
90908080707010c010101010101010101010101010101010c0102020206120202061616161016161616161616161616161616161616161616161616161616110
1010106110202020203070708080909020202020919191919191919140201120202020202010101091919191919191204161616141b141616161616161616141
909080c0c0c0c0c01060606060101061616161616161616101616161616120202020202010d01020202020202020202061202020202020202010101010106161
6161616110202020707070708080909020202020202020202020202020201120209191919191911010101010101020204141414141414141d141414141414141
909090807070101010606060101010611010101010c0c0c0c0102020202020202020202010d01010102020202020202061202030302020302020202020101010
1010f161102020202070807080809090209191919191919191919120202011202091f28383d29110919191919191202091919191919120919191919191919191
9090808070706060604010616161616110c0c0c0c0c0101010102020202020202020202010d0d0d0102020202020202061203030302030303020202020202020
20201061102020202070808080809090209154c3c213838353549120202011204091111111119110919282111191202091a273d3e29120911111111111111191
90908080907040101010404010101010c0c0c01010101020202020202020202020202020101010d0102020202020202061303020202030303020202020202020
20201061102020202070708080909090208111111111111111118120202011404091c3d383939110918211111191202091111111119120910413a2d3c213a291
909080908090104040404040c0c0c0c0c010c050c0501020202020202020202020202020202010c0102020202020202061303020303030302020202020202020
20201061102020202070707080809090209111111111111111119120202011111111111111119110911111b0b09120209113e373d39120c11111111111111191
90909042707040c0c0c0c0c0c01010101050c0c0c0c05010202020202020202020202020202010c0102020202020202061302020303030202030303030303020
2020106110202020207070708080909020815454541111545454812020201140409111111111911091d19191919120209111111111912091545473e2e2d25491
909090c0c0c0c0c04040404040404010c0c0c0c0c0c0501020202020202020202061616161616101616161616161616161202030303030202020303030302020
20101061101020202030707080809090209182828211118282829120202011104091919191919110111110101010202091111111119120919191919191919191
9090808070704060606060604040101050c0c0c0c0501010202020202020202020614040202010c0c0c0c0c0c0c0c0c0c0c0c030202020203020202020202030
20106161611020203030707080809090208154545411115454548110101011101010101010101010111091919191912091911191919120202020202020202020
909090807070606060606060404040101010105050101020202020202020202020614010101010c010807070303030202030c0c0d03030303020302020302030
20106162611020203030707080809090209182828211118282829140104011101010101010101010111091b0b011912020402040202020202020202020202020
909080807040404060606040401020202020101010202020202020202020202040611010c0c0c0c0809080703030303020303030d03020202020202030302020
2010616161102030303070708080909020911111111111111111c111111111101010101010101010111091111111912020202020202020212121212121212020
909080807070404040404040103030303030202020202020202020202020202040611010c01010809090909090303030202020d0d02030302030203030303020
201010101010203030307070808090902091919191919191919191401040111111111111111111111111d111118291202091919191202021b3a2d22383212020
909080807070704040403030303030303030303030303020202020202020204040615210c01090707070707090302030303030d0303030202030203030303030
30202020202030303030707080809090201010101010101010101010101020201120202010101010101091118292912020919282914020216161616161212020
909080807070704040303030303030303030303030303030303030303020304040401010c01090709090907070802020202020d0202020202020203030808030
3020303030303030307070809080909020101010101010101010101010102020112020201010101010109191919191202091b011c12020c16161616161212020
909080807070807070803030307030303030303030303030303030303030303040404070c01090709070707070707030303030d0303020707070203030707070
3030303030303070708070708080909020202020202020202020202020202020112020202020202020202020202020202091919191402021435353b354212020
909080808070707080703030708080307070707070707030303030303030303080808070c080907090709090909090803030d0d0303020708080703080808080
80303030303080808080808080809090414141414141414141414141414141412020202020202020202020202020202020919282914020212121212121212020
90908080908070907070803030308070707070808080703030303030307070808070c0c0c080907070707070709070707030d030707070707080708070909090
7070303030307070708090808090909041306161616161613061306161616141209191919191912091919191401020202091b011c12020202020202020202020
90908080809080708070707070807070707080808070707070707070707070707070c080909070909090909070907070d0d0d080807070707070707090709070
70707070707070808080908080909090416161306120202061202061616161412091b0b01111912091b011911010102020919191914020204082828282824020
90909080808080807080907070707070707090909080807070707070707080808070c0907070707070707090708090d0d0707070708080807070707070707070
70808070808070809080909080809090416130202061202020202061614161412091111111829120911111c110c0101020919282914020209153e373c2139120
90909090808080809080808080808080808080809090808080808080808080808080c08090804280809080808080908080808080808090808080808080808080
808080808090808080808080808090904161302020206120612020616141614120911111829291209111119110c0c01020918211c12020209111111111119120
90909090909080808080909080808080808080808080808070808080809090909080c08090808080809090808090909080808080808080809080808080808080
9090909080808080808080808080909041616130202020612061616161416141209191d191919120919282911001c0c02091b0b0914020209163e373c2139120
90909090909090909090909090909090909090909090909090909090909080809090909090909090908090909090909090909090909090909090909090909090
909090909090909090909090909090904130616161616161306130616141b14120201061102020209191919140c0c0c020919191912020204082828282824020
90909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090
909090909090909090909090909090904141414141414141414141414141414120202020202020202020202020c0c0c020202020202020202020202020202020
__label__
01d10000000000000000000005d67d00016765000000000000000000000000000056765000000000000000000000000000000000000000000000000000000000
06777777777600000000167777777600577777d00000000000000000000000000d77777500000000000000000000000000000000000000000000000000000000
0d7777777777d0000000d77777777100777777700000000000000000000000001777777700000000000000000000000000000000000000000000000000000000
0057777777776000000067777777d005777777750000000000000000000000005777777710000000000000000000000000000000000000000000000000000000
000d7777777771000001777777775005777777710000000000000000000000005777777710000000000000000000000000000000000000000000000000000000
00057777777775000005777777775000777777700000000000000000000000000777777600000000000000000000000000000000000000000000000000000000
00017777777776000006777777775000177777500000000000000000000000000577777100000000000000000000000000000000000000000000000000000000
0000777777777700000677777777500000d667d00000000000000000000000000016667d00000000000000000000000000000000000000000000000000000000
00007777777777100017777777775000001d77700000000d6d01dd6dd50000000001d777000000056d015d66d5000005d6d51000000565111155d6d500000000
000077777777775000d7777777775000567777700055dd6777d77777776000001567777700000d67776777777761056777777600001777766777777760000000
000077777777776000677777777750d7777777700677777777777777777600067777777700d6777777777777777767777777776100d777777777777776000000
000077777777777001776777777750777777777006777777777777777777d0177777777706777777777777777777777777777776006777777777777777500000
00007777777777710d775777777750d7777777700067777777777777777771067777777706777777777777777777777777777777606777777777777777600000
000077677777777d067657777777500d77777770005777777777777777777600677777770577777777777777777777777777777771d777777616777777710000
0000776d77777776077d57777777500d7777777000577777777d567777777700d777777700677777777d567777777765677777777617777777d1777777750000
0000776577777776d77557777777500d77777770005777777760007777777750d777777700d77777775001777777770006777777760d777777d07777777d0000
0000776077777777777057777777500d777777700057777777d000d7777777d0d777777700d77777770000777777760001777777771056777610d77777760000
00007760677777777760577777775005777777700057777777d0001777777760d777777700d77777770000777777760000677777775005777777677777760000
00007760577777777750d77777775005777777700017777777d00006777777605777777700d77777770000777777760000677777775067777777777777771000
00007760177777777700d77777775005777777700017777777d00006777777605777777700d77777770000777777760000d77777775677777777777777775000
00017760067777777600d77777775005777777700017777777d00006777777605777777700577777770000777777770000d77777776777777777777777775000
00017760067777777500d77777775005777777700017777777d00006777777605777777700577777770000777777770000d7777777777777777777777777d000
00057760057777777100d77777775001777777700017777777d00006777777605777777700577777770000777777770000d777777777777776d7777777776000
00057770017777776000d7777777d005777777710017777777d00006777777d05777777700577777770000777777770000d77777777777777505d67777776000
000d777000677777d00067777777d005777777710017777777d00006777777505777777700577777770000777777770000d77777777777777500067777777100
0006777100577777500067777777d0057777777100177777776000067777771057777777005777777700007777777700006777777d7777777d00577777777100
00077775000777771000677777776005777777710057777777700007777776005777777700d777777700007777777700006777777167777777dd677777777d00
00577776000677760001777777777005777777750057777777700017777775005777777750d7777777100077777777100077777760d777777777777777777600
006777776005777d000d777777777d067777777710677777777500d777776001777777777567777777600577777777600d777777005777777777777777777750
067777777d0077710057777777777777777777777677777777775077777750167777777777777777777657777777777d0777777d000577777777777777777775
6777777777106760006777777777777777777777777777777777d677777d001777777777777777777777777777777776d777776000005677777777d777777776
1777777776005750005777777777776677777777d5677777777657777770000d7777777776667777776d16666666666567777700000000d7777761017777777d
0000000000000000000000000000000000000000000000000000d777760000000000000000000000000000000000000d77776000000000001110000000000110
00000000000000000000000000000000000000000000000000006777d000000000000000000000000000000000000006777d0000000000000000000000000000
0000000000000000000000000000000000000000000000000000d6d000000000000000000000000000000000000000016d100000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00
0cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc0
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccc0000000000000000000000000000000000000000000000000000000000000000000000000000000000000101101010001011100000101110000110ccccc
cccc000000330000003300000033000000055000000550000005500000330000003300000033000000000000000000000000111000001110000011100000cccc
ccc01000033330000333300003333000005005000050050000500500033330000333300003333000000300000100010100110100001101000011010000110ccc
ccc30000033333300333333003333330005000500050005000500050033333300333333003333330003330001010101011000010110000101100001011000ccc
ccc00000003303330033033300330333050000050500000505000005003303330033033300330333000300300001000000000000000000000000000000000ccc
ccc01001030033330300333303003333500055005000550050005500030033330300333303003333000003330000000000011100000111000001110000011ccc
ccc00330333303303333033033330330500500505005005050050050333303303333033033330330003000300100010101101000011010000110100001101ccc
ccc10000333300003333000033330000005000050050000500500005333300003333000033330000033300001010101010000101100001011000010110000ccc
ccc00000033000000330000003300000000000000000000000000000033000000330000003300000003000000001000000000000000000000000000000000ccc
ccc00000000000000000050000000500000550000005500000055000000550000000050000000500000000000000000000000000000011100000111000001ccc
ccc01000010010000050505000505050005005000050050000500500005005000050505000505050010001010100010101000101001101000011010000110ccc
ccc30000003300000505000505050005005000500050005000500050005000500505000505050005101010101010101010101010110000101100001011000ccc
ccc00000000000005000500050005000050000050500000505000005050000055000500050005000000100000001000000010000000000000000000000000ccc
ccc01001000010010005050000050500500055005000550050005500500055000005050000050500000000000000000000000000000111000001110000011ccc
ccc00330000003300050050000500500500500505005005050050050500500500050050000500500010001010100010101000101011010000110100001101ccc
ccc10000100100000500005005000050005000050050000500500005005000050500005005000050101010101010101010101010100001011000010110000ccc
ccc00000033000000000000000000000000000000000000000000000000000000000000000000000000100000001000000010000000000000000000000000ccc
ccc30000000005000000050000055000000550000005500000006000000550000000050000000500000000000000000000000000000000000000000000001ccc
ccc33000005050500050505000500500005005000050050000066600005005000050505000505050010001010003000000030000010001010100010100110ccc
ccc33330050500050505000500500050005000500050005000d00600005000500505000505050005101010100033300000333000101010101010101011000ccc
ccc30333500050005000500005000005050000050500000505000d00050000055000500050005000000100000003003000030030000100000001000000000ccc
ccc03333000505000005050050005500500055005000550005050050500055000005050000050500000000000000033300000333000000000000000000011ccc
ccc30330005005000050050050050050500500505005005050005050500500500050050000500500010001010030003000300030010001010100010101101ccc
ccc30000050000500500005000500005005000050050000500005005005000050500005005000050101010100333000003330000101010101010101010000ccc
ccc00000000000000000000000000000000000000000000000000000000000000000000000000000000100000030000000300000000100000001000000000ccc
ccc00000000000000000050000000500000005000000050000000500000005000000030000000300000000000000000000000300000003000000000000000ccc
ccc30000000300000050505000505050005050500050505000505050005050500300000003000000010001015505505503000000030000000100010101000ccc
ccc33000003330000505000505050005050500050505000505050005050500050000000000000000101010106666666600000000000000001010101010101ccc
ccc30030000300305000500050005000500050005000500050005000500050000003000000030000000100006566665600030000000300000001000000010ccc
ccc00333000003330005050000050500000505000005050000050500000505000000000300000003000000006565565600000003000000030000000000000ccc
ccc00030003000300050050000500500005005000050050000500500005005000000000000000000010001016664466600000000000000000100010101000ccc
ccc30000033300000500005005000050050000500500005005000050050000500000300000003000101010106664466600003000000030001010101010101ccc
ccc00000003000000000000000000000000000000000000000000000000000003000000030000000000100000000000030000000300000000001000000010ccc
ccc00000000000000000000000000500000005000000050000000500600ff0000000030000000300000000000000030000000300000003000000000000000ccc
ccc30000000300000003000000505050005050500050505000505050600ff5500300000003000000010001010300000003000000030000000100010101000ccc
ccc33000003330000033300005050005050500050505000505050005609855550000000000000000101010100000000000000000000000001010101010101ccc
ccc30030000300300003003050005000500050005000500050005000688955550003000000030000000100000003000000030000000300000001000000010ccc
ccc00333000003330000033300050500000505000005050000050500f89995500000000300000003000000000000000300000003000000030000000000000ccc
ccc00030003000300030003000500500005005000050050000500500009095500000000000000000010001010000000000000000000000000100010101000ccc
ccc30000033300000333000005000050050000500500005005000050004004000000300000003000101010100000300000003000000030001010101010101ccc
ccc00000003000000030000000000000000000000000000000000000044004403000000030000000000100003000000030000000300000000001000000010ccc
ccc00000000000000000000000000000000000000000050000000500000003000000030000000300000003000000030000000300000003000000000000000ccc
ccc30000000300000003000000030000000300000050505000505050030000000300000003000000030000000300000003000000030000000100010101000ccc
ccc33000003330000033300000333000003330000505000505050005000000000000000000000000000000000000000000000000000000001010101010101ccc
ccc30030000300300003003000030030000300305000500050005000000300000003000000030000000300000003000000030000000300000001000000010ccc
ccc00333000003330000033300000333000003330005050000050500000000030000000300000003000000030000000300000003000000030000000000000ccc
ccc00030003000300030003000300030003000300050050000500500000000000000000000000000000000000000000000000000000000000100010101000ccc
ccc30000033300000333000003330000033300000500005005000050000030000000300000003000000030000000300000003000000030001010101010101ccc
ccc00000003000000030000000300000003000000000000000000000300000003000000030000000300000003000000030000000300000000001000000010ccc
ccc00300000003000000030000000300000000000000050000000300000003000000030000000300000003000000030000000300000003000000000000000ccc
ccc00000030000000300000003000000000300000050505003000000030000000300000003000000030000000300000003000000030000000100010101000ccc
ccc00000000000000000000000000000003330000505000500000000000000000000000000000000000000000000000000000000000000001010101010101ccc
ccc30000000300000003000000030000000300305000500000030000000300000003000000030000000300000003000000030000000300000001000000010ccc
ccc00003000000030000000300000003000003330005050000000003000000030000000300000003000000030000000300000003000000030000000000000ccc
ccc00000000000000000000000000000003000300050050000000000000000000000000000000000000000000000000000000000000000000100010101000ccc
ccc03000000030000000300000003000033300000500005000003000000030000000300000003000000030000000300000003000000030001010101010101ccc
ccc00000300000003000000030000000003000000000000030000000300000003000000030000000300000003000000030000000300000000001000000010ccc
ccc00300000003000000030000000300000003000000030000000300000003000000030000000300000003000000030000000000000000000000000000000ccc
ccc00000030000000300000003000000030000000300000003000000030000000300000003000000030000000300000001000101010001010100010100030ccc
ccc00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010101010101010101010101000333ccc
ccc30000000300000003000000030000000300000003000000030000000300000003000000030000000300000003000000010000000100000001000000030ccc
ccc00003000000030000000300000003000000030000000300000003000000030000000300000003000000030000000300000000000000000000000000000ccc
ccc00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000101010001010100010100300ccc
ccc03000000030000000300000003000000030000000300000003000000030000000300000003000000030000000300010101010101010101010101003330ccc
ccc00000300000003000000030000000300000003000000030000000300000003000000030000000300000003000000000010000000100000001000000300ccc
ccc00500000005000033000000330000000000000000000000000000000003000000030000000300000000000000000000000000000000000000000000000ccc
ccc05050005050500333300003333000000300000003000000030000030000000300000003000000010001010100010100030000010001010100010100030ccc
ccc50005050500050333333003333330003330000033300000333000000000000000000000000000101010101010101000333000101010101010101000333ccc
ccc05000500050000033033300330333000300300003003000030030000300000003000000030000000100000001000000030030000100000001000000030ccc
ccc50500000505000300333303003333000003330000033300000333000000030000000300000003000000000000000000000333000000000000000000000ccc
ccc00500005005003333033033330330003000300030003000300030000000000000000000000000010001010100010100300030010001010100010100300ccc
ccc00050050000503333000033330000033300000333000003330000000030000000300000003000101010101010101003330000101010101010101003330ccc
ccc00000000000000330000003300000003000000030000000300000300000003000000030000000000100000001000000300000000100000001000000300ccc
ccc00500000005000033000000000000000000000033000000330000000000000000030000000000003300000000000000000000000000000000000000000ccc
ccc05050005050500333300000030000000300000333300003333000000300000300000001000101033330000003000000030000010001010100010100030ccc
ccc50005050500050333333000333000003330000333333003333330003330000000000010101010033333300033300000333000101010101010101000333ccc
ccc05000500050000033033300030030000300300033033300330333000300300003000000010000003303330003003000030030000100000001000000030ccc
ccc50500000505000300333300000333000003330300333303003333000003330000000300000000030033330000033300000333000000000000000000000ccc
ccc00500005005003333033000300030003000303333033033330330003000300000000001000101333303300030003000300030010001010100010100300ccc
ccc00050050000503333000003330000033300003333000033330000033300000000300010101010333300000333000003330000101010101010101003330ccc
ccc00000000000000330000000300000003000000330000003300000003000003000000000010000033000000030000000300000000100000001000000300ccc
ccc00500000005000000000000000000000000000033000000330000003300000000000000000000003300000000000000000000000000000000000000000ccc
ccc05050005050500003000000030000000300000333300003333000033330000003000001000101033330000003000000030000010001010100010101000ccc
ccc50005050500050033300000333000003330000333333003333330033333300033300010101010033333300033300000333000101010101010101010100ccc
cccc500050005000000300300003003000030030003303330033033300330333000300300001000000330333000300300003003000010000000100000001cccc
ccccc5000005050000000333000003330000033303003333030033330300333300000333000000000300333300000333000003330000000000000000000ccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
0cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc0
00cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc00

__gff__
0000000101010201020b00000484048400000800080000080808000008080000010000000000000000000808080808080808080808080808080808080808080808080808080808000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909090909141414141418141414141414181414141414141814141414141414141414141814141414141414141814141414141414
0909090909090909090909090909090909090909090909090909090909090909090909080909090909090909090909090909090909090909090909090909090909090909090909090909090909090909140404010101010101010104040401010101010101010404141111111404161616161616161616161616161616040414
090908080808080808090909090808080808080908080808080909080908080807080909090808080807080808080809090808080808080808080808080808080908080808080809090808080808090914040114141414141414010101010114141414141414010414362e2d1416161616161616161616161616161616160414
09090808080808210908080808080808080808080c0c0c082009080808080808080808080809080808080808080808080808080908080808080808080808090808080808090809080808092408080909140101140b110b110b141814181418140b110b110b140101141111111c16141414141414141414141414141414161614
090909080709090907070707070707070707080808070c0c0c07070606070808080808080907070707070707070707080807080808070707070708090809070909080909090809080909080909080909140101140b110b110b141111111111140b110b110b140101141111111404151616161616161616161616161a14161614
090908210907070707070707070707070c0c0707070707070c0707070606070707070707070d0d0d0d0d0d0d0d0d0d070707080808070707070708080807070709070809090808080808090808090909140101140b110b110b14110c0c0c11140b110b110b140101141414141414141414141414141414141414141414161614
0909090909070505070505050505050c0c070707040407040c07070404060607070707070d0d06060606060606060d0d0d0d0707070d0d0d0d0d0d07070709070909080909080709090809080808090914010114111111111114110c270c11141111111111140101142928161614292816161429281616142928161614161614
0909080c0c0c0c0c0c050505050c0c0c05060606060404040c0c0c0c04040606040407070d0606060606060606060607070d0d0d0d0d040207070d07070707080808090809090709070808090908090918010114292811111114110c0c0c11141111112829140101181616161614161616161416161616141616161614161618
09090809080707050c0c0c0c0c0c050c05060606060604040404040c0c0c040406060d0d0d0606060606060606060606060707020202020202020d0203070707030707080809090907090808090809091401011414141411111d11111111111d11111414141401011414141d141414141d141414141d141414141d1414161614
0909092109080805050505050505050c050606060606060606060604040c01040d0d0d06060606060606060606060606060606040204040202030d020203030302030307070809090908090809080909140101141b111d11111418141114181411111d110a140101141616161616161616161616161616161616161614161614
0909080c0c080707050505050505050c050606060606060606060606040c010110040604060606060606060606060606060606040404020202020d0d0d0d0d0d020202030708080808080908080809091401011414141414141401141c14011414141414141401011414171414171414171414141d1414141414141414161614
090908080c070705050c0c0c0c0c050c050406060606060606060606010c0c0c0c0c0c060406060606060606060606060606060404020204020202020203020d0d0d0202070708080908070809090909140101010404040404010101110101010404040404010101140a161616161616160a14161616140a0a0a0a0a14161614
090908080c070505050c0505050c050c0504060606060606060606060604040404040c0c06040606060606060606060606060604040204020202020202020202030d0d0d020707080908090708080909140101010101010101010101110101010101010101010101141616161616161616161d1616161d161616160a14161614
090909080c0705050c0c0505050c0c0c0c0104040406060606060606060604040404010c0c0606060606060606060606060404040202020402020202020202020202030d0d0d0d070808080909080909140119191919191919190101110101191919191919191901143d313237343d2a373414161616140a0a0a0a0a14161614
0909080c0c0c0c0c0c050505050505050c0c0c01040404040406060606040404040401040c0c06060606060606060606040404020402040202020202020202020302020202030d0d0d080708080809091401193c39323b323d190401110104194535322f2e4419011414141414141414141414141d1414141414141414161614
0909080807050505050505050505050505010c0c0c0c0c01040404040404040404040104040c040404060606060606040401020402020202040202020202020202020202020202020d0d0d0d0d0d0909140119111111111111190401110104191111111111111901141616161616161616040404160404041616161616160414
090908070707050505050505050505010101010101010c0c0c0c0c0c0104010401010101040c040404040404060606040101010101010202020202020202020202020202020202030202070708090909180119312e2a353d3119040111010419452b3b2e2a2d1901181612121212121212121216161616161616161616040418
0909070707070505050505050505050101010101010101010c01010c0c0c0101010c2316010c04040404040404040404010101010101010101010202020202020202020203020202020707070809080914011911111111111119040111010419111111111111190114161211111111111111121616161414141414141d141414
090907080705050c0c0c0c0505050505050101010101010c0c010101010c0c0c0c0c0116010c0101040104010101010101010101010101010101010102020202020202020202020702020708080909091401191111111111111904011101041911111111111119011416122c2a373d2e2e371216161614112829141111110b14
090908080c0c0c0c05050c0c0c0c0c0c0505010c0c0c0c0c01010101010101010c0d01161610161616161616161616161616010101010101010101010202020202020202020202020707070708080909140119191911191919190111111101191919191119191901141612161616161616161216161614111128142811111114
0909080807070505050505050505050c0c0c0c0c010101010101010401010101010d0d01010d0101010101010101010101161616161616161616010101020202020202020202030207070707080809091401010104110401010101111f110101010104110401010114161216161616161616121616161d111111142928111114
090909080707040505050505050501010101010c01010101040101010101010101010d0d010d0d0101010c0c0c0c0c0c010101010101010404161f010101020202020202020202070207070808080909140101010111111111111111111111111111111101010101141612121212161212121216161614111111142811111114
0909080807040404040404010101010101010c0c0101040101010101010101010c0c0c0c0c0c0c0c0c0c0c0e0c0c0e0c0c0c0c0c0c0c0c0c041616161616161602020202030202020207070808090909140101010101010101040404110404040101010101010101141616161616161616161616161614110b0b1411110b0b14
090908080707040407070701010101010c0c0c0101010101040104010101010c0c0e0e0e0c0c0c0c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c041604040101011616020202020202020207070708080909141414141418141414140411111104141414141814141414141414141414141814141416161614141414141414141414
090908080707070707070c0c0c0c0c0c0c01010101040101010101010101010c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c2416040c0101010116020202020202030202070708080909020202020202020202020202020202020202020202020202020202020202020214141414141414141414141414141414
0909080c0c0c0c0c0c0c0c07070c010101010101010101040101010101010c0c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c01010c0c0c0c0c0116020202020202020202070708080909021919191919191919191919191919190202021919191919191919191919190214111d111411111d11110b140b111114
090909080707070707070707070c010104010101010101010101010101010c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c0c0c0c0c0c0c0c160202020202020202020707080809090219332a2c342e3d19313e373d454619040202192a392a3b3d362e373d3c190214111411141128142811111411112814
090908080707070707070707070c0101010104010116161616160101010c0c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c0e0e0c0c0c0c16020202020202020202070708080909021911111111111119111111111111190402021911282919111119292811190214111411111129142911111c11112914
0909080807070707070c0c0c0c0c01010101010101240101011601010c0c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c0c0c011602020202020202020207070808090902193b2a2c342e3d19452f323c314519040202190b11111d11111d11110b190214111414141414141414141414141414
0909080807070707070c0401010101040101010101010101011601010c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c010c0c0116020202020202020203070708080909021911111111111119111111111111190402021919191919111119191919190214111511111129142911111c11112914
090908080c0c0c0c0c0c04040404010101010101010101010116010c0c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c01040c010116020202020203030303070708080909021911111111111113111111111111190402021911282919111119292811190214111411141128142811111411112814
0909080907070707040404040606040101010101010101010116010c0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0e0c0c0c0c0c0c0c01011602020202030307030307070808090902191919111119191919191111191919020202190b11111d11111c11110b1902141a14111411111d11110b140b111114
__sfx__
000100002365024650206501e64000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00010000245202a6403075034660367703667034770326602f6602d75029650297502a640296402b7402c6502e6502f7502f6502d640296302563022720000000000000000000000000000000000000000000000
0001000017070160601505016050170501a06021060290702f0700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00020000276432f4732f4732f4732f4732f4732f4733b1733b2733b2733b2733b2733b2733b2733b2733b2733b2733b2333851500000000000000000000000000000000000000000000000000000000000000000
0002000002643024730347305473074730a4730d47311173162731b2731f27324273292732d27331273362733b2733e2333f51500000000000000000000000000000000000000000000000000000000000000000
000100001263113341164511335117451133511544111341144310e331114210b3210c411073110a41104611236011f6002f50029500235001e5001e500225002750029500255001f5001e500000000000000000
01200000134201342013420134200e4200e4200e4200e420134201342013420134200e4200e4200e4200e420134201342013420134200e4200e4200e4200e420134201342013420134200e4200e4200e4200e420
01200000134201342013420134200e4200e4200e4200e420134201342013420134200e4200e4200e4200e42013420134201a4201a4200e4200e4201a4201a42013420134201a4201a4200e4200e4201a4201a420
0120000013420134201a4201a4200e4200e4201a4201a42013420134201a4201a4200e4200e4200e4200e42013420134201a4201a4200e4200e4201a4201a42013420134201a4201a4200e4200e4201a4201a420
0120000013420134201a4201a4200e4200e4201a4201a42013420134201a4201a4200e4200e4200e4200e420134201342013420134200e4200e4200e4200e420134201342013420134200e4200e4200e4200e420
01200000134201342013420134200e4200e4200e4200e420134201342013420134200e4200e4200e4200e420134201342013420134200e4200e4200e4200e420134201542016420184201a4201a4201a4201a420
01200000184201842018420184201142011420114201142016420184201a4201b4201d4201d4201e4201e4201f4201f4201f4201f420184201842018420184201a42016420154201342015420134201542013420
0120000013420134201a4201a4200e4200e4201a4201a42013420134201a4201a4200e4200e4201a4201a42013420134201a4201a4200e4200e4201a4201a42013420134201a4201a4200e4200e4200e4200e420
012000000c4200c420114201142016420164201742017420184201a4201c4201e4201f4201f4201d4201d4202042020420204202042021420204201e4201c4201e4201e4201e4201e42020420204202642026420
0120000025420214201c420214201942019420194201942024420214201c4202142018420184201842018420234201f4201a4201f42017420174201742017420224201f4201a4201f42016420164201842018420
012000000c4200c420114201142016420164201742017420184201a4201c4201e4201f4201f4201e4201e4202042020420204202042021420204201e4201c4201e4201e4201e4201e42020420204202642026420
012000001f4201f42013420134201a4201a4200e4200e420134201342013420004000040000400004000040000400004000040000400004000040000400004000040000400004000040000400004000040000400
01200000004000040000400004000040000400004000040000400004000040000400004000040000400264202b4202d4202e4202b4202d4202e420304202d4202e4202e4203242032420304202e4202d42030420
012000002e4202d4202b4202e4202d4202d4202a4202a4202b4202b4202642026420244002440026420264202b4202d4202e4202b4202d4202e420304202d4202e4202e4203242032420304202e4202d42030420
012000002e4202d4202b4202e4202d4202d4202a4202a4202b4202b4202642026420244002440026420264202b4202d4202e4202e4202d4202e42030420304202e420304203242032420304202e4202d4202d420
012000002e4202d4202b4202b4202d4202b4202a4202a4202b4202d4202b4202b420264202642026420264202b4202d4202e4202b4202d4202e420304202d4202e4202e4203242032420304202e4202d42030420
012000002e4202d4202b4202e4202d4202d4202a4202a4202b4202b4202642026420244002440026420264202d4202b4202d4202b4202a4202a420334203342032420324202e4202e4202e4202a4002e4202e420
01200000304202e420304202e4202d4202d420334203342032420324202e4202e4202e4202e4202d4202d4202e4202e4202b4202b420294202942027420274202642026420264202642029420294202942029420
012000002b4202d4202e4202b4202d4202e420304202d4202e4202e4203242032420304202e4202d420304202e4202d4202b4202e4202d4202d4202a4202a4202b4202b420264202642000000000002642026420
012000002b4202d4202e4202e4202d4202e42030420304202e420304203242032420304202e4202d4202d4202e4202d4202b4202b4202d4202b4202a4202a4202b4202d4202b4202b42026420264202642026420
012000002742026420244202442026420264202642026420284202642024420244202642026420274202742028420284202f4202f420314202f4202d4202c4202d4202d4202d4202d4202f4202f4202c4202c420
012000002d420284202542028420214202142021420214202d420284202442028420214202142021420214202b4202642023420264201f4201f4201f4201f4202b4202642022420264201f4201f4201e4201e420
012000002742026420244202442026420264202642026420284202642024420244202642026420274202742028420284202f4202f420314202f4202d4202c4202d4202d4202d4202d4202f4202f4202c4202c420
012000002b4202d4202e4203042032420334203642032420374203742037420320001800022000210002100022000210001f0001f000210001f0001e0001e0000000000000000000000000000000000000000000
01200000220051a0051f005220051a0051f005210051e005000000000000000000000000000000000000000022420264202b42022420264202b4202d4202a4202b4202b4202e4202e4202d4202b4202a4202d420
012000002b4202b42026420264202a4202a42024420244202242026420214201f4201a4201b4201e420244202a420284202a42028420264202642030420304202e4202e4202b4202b4202b4202b4002b4202b420
012000002d4202b4202d4202b420294202942030420304202e4202e4202942029420294202942026420264202b4202b4202742027420214202142024420244202242022420224202242021420214202142021420
0120000022420264202b4202e420264202b4202d4202a4202b4202b4202e4202e4202d4202b4202a4202d4202b4202b42026420264202a4202a4202442024420224201a420224201f4201a4201b4201e42024420
012000002442022420214202142022420224201f4201f420244202342021420214202342023425234202342523420234202342023420284202842028420284002842028420274202742028420284202842028420
01200000284202542021420254201c4201c4201c4201c420284202442021420244201c4201c4201c4201c42026420234201f420234201a4201a4201a4201a42026420224201f420224201a4201a4251a4201a420
0120000022420214201f42022420214201f4201e420214201f4201f4201f420000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011800000942009420104201042015420154201042010420114201142011420114200000000000000000000009420094201042010420154201542018420184201c4201c4201c4201c42000000000000000000000
011800000942009420104201042015420154201042010420114201142011420114200000000000000000000010420104201142011420104200e4200c4200b4200942009420094200942000000000000000000000
01180000000001a4201a42021420214202642026420214202142022420224202242000000000000000000000000001a4201a420214202142026420264201d420294202d4202d4202d42000000000000000000000
01180000000001a4201a420214202142026420264202142021420224202242022420000000000000000000000000021420214202242022420214201f4201d4201c4201a4201a4201a42000000000000000000000
01180000000000942009420104201042015420154201042010420114201142011420000000000000000000000000009420094201042010420154201542018420184201c4201c4201c42000000000000000000000
01180000000000942009420104201042015420154201042010420114201142011420000000000000000000000000010420104201142011420104200e4200c4200b42009420094200942000000000000000000000
011800002442024420244202442024420244202442024420214202142021420214200040000400004000040024420244202442024420244202442024420244201f4201f4201f4201f4201c000000000000000000
011800002442024420244202442024420244202442024420214202142021420214200000000000000000000020420204202042020420234202342023420234202442024420244202442000000000000000000000
01180000244202442028420284202d4202d420284202842027420274202742027420000000000000000000002142024420284202c4202d4202b42028420284202b4202b4202b4202b42000000000000000000000
011800002442024420284202842024420244202842028420294202942029420294200000000000000000000028420264202842024420264202442026420234202142021420214202142000000000000000000000
0118000015420154201c4201c42021420214201c4201c4201d4201d4201d4201d4200000000000000000000021420214201c4201c420214202142024420244202842028420284202842000000000000000000000
0118000015420154201c4201c42021420214201c4201c4201d4201d4201d4201d420000000000000000000001c4201c4201d4201d4201c4201a42018420174201542015420154201542000000000000000000000
011800000000015420154201c4201c42021420214201c4201c4201d4201d4201d4201d4200000000000000000000015420154201c4201c4202142021420244202442028420284202842028420000000000000000
011800000000015420154201c4201c42021420214201c4201c4201d4201d4201d4201d420000000000000000000001c4201c4201d4201d4201c4201a420184201742015420154201542015420000000000000000
01180000000003542035420394203942032420324203942039420364203642036420364000000000000000000000032420354203b4203142032420314203b4203b42030420304203042030400000000000000000
011800000000035420354203942039420324203242039420394203642036420364203640000000000000000000000394203742039420354203742035420374203442032420324203242032400000000000000000
011800000000024420244202442024420244202442024420000002142021420214200000000000000000000000000244202442024420244202442024420244200000020420204202042000000000000000000000
011800000000024420244202442024420244202442024420000002142021420214200000000000000000000000000204202042020420234202342023420244202442024420244202442000000000000000000000
011800003441034410344103441034410344103441034415334103341033410334100000000000000000000034410344103441034410344103441034410344153441034410344103441000000000000000000000
011800003441034410344103441034410344103441034415354103541035410354100000000000000000000034410344103441034410384103841038410384103941039410394103941000000000000000000000
01180000344103441039410394103c4103b410394103941539410394103941039410000000000000000000003041034410394103b4103c4103b41039410394103b4103b4103b4103b41000000000000000000000
01180000344103441039410394103c4103b410394103941539410394103941039410000000000000000000003c4103b4103c410394103b410394103b410384103941039410394103941000000000000000000000
011800001c4101c41023410234102841028410234102341024410244102441024410000000000000000000001d4101d410234102341028410284102b4102b4102f4102f4102f4102f41000000000000000000000
011800001c4101c41023410234102841028410234102341024410244102441024410000000000000000000002341023410244102441023410214101f4101e4101c4101c4101c4101c41000000000000000000000
01180000000002d4102d41034410344103941039410344103441035410354103541035410000000000000000000002d4102d410344103441039410394103c4103c41034410344103441034410000000000000000
01180000000002d4102d4103441034410394103941034410344103541035410354103541000000000000000000000344103441035410354103441032410304102f4102d4102d4102d4102d410000000000000000
011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000100001050112501195012050124501000010e50111501165011b501235012a50100001000010c5010e501155011d50122501255010000100001095010c50111501175011d5012450129501000010000100001
__music__
00 06114344
00 07124344
00 08134344
00 09141d44
00 0a151e44
00 0b161f44
00 0c172044
00 0c182044
00 0d192144
00 0e1a2244
00 06174344
00 0c172044
00 0d1b2144
00 0e1a2244
00 06182044
00 0c182044
02 101c2344
01 246a4344
00 25424344
00 242a3644
00 252b3744
00 242c3844
00 252d3944
00 242e3a44
00 252f3b44
00 24303c44
00 25313d44
00 26324344
00 27334344
00 28344344
02 29354344


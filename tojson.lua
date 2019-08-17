#!/usr/bin/lua
--[[
Convert Minima Lua tables into JSON for more compact storage that uses fewer tokens.
--]]

json = require 'json'

-- For this to work in PICO-8, everything has to be crammed into a single line.
-- PICO-8 also wants embedded apostrophe characters escaped.
-- This returns the full line including the appropriate `json_parse` call so
-- it can be copied and pasted directly in.
function outputStructure(structureName, structure)
  structureJson = string.gsub(json.encode(structure), "'", "\\'")
  print(string.format("%s=json_parse('%s')", structureName, structureJson))
end

-- Our basetypes structure includes all of our objects and our bestiary.
basetypes={
  {
    -- base creature
    hp=10,
    ar=1,
    dmg=13,
    dex=8,
    hos=1,
    t={1,2,3,4,5,6,7,8,10,16,17,26,27,30,31,32,38,40,41},
    mva=1,
    gp=0,
    exp=2,
    ch=1
  },{
    -- base town
    mn=0,
    mnx=80,
    mxx=128,
    mny=0,
    mxy=64,
    newm=0,
    mxm=0,
    fri=1
  },{
    -- base dungeon
    mn=0,
    dg=1,
    sx=1,
    sy=1,
    sz=1,
    sf=1,
    mnx=1,
    mny=1,
    mxx=9,
    mxy=9,
    newm=25,
    mxm=27,
    fri=false
  },{
    i=70,
    ia=70,
    n="boat",
    fm=1,
    f=2,
    p=1
  },{
    i=94,
    ia=94,
    n="chest",
    shm=-2,
    szm=11,
    p=1
  },{
    i=39,
    fi=1,
    iseq=12,
    n="fountain",
    p=1
  },{
    i=27,
    ia=27,
    n="ladder up",
    shm=12,
    szm=20,
    p=1
  },{
    i=26,
    ia=26,
    n="ladder down",
    shm=-3,
    szm=20,
    p=1
  },{
    -- base human
    i=80,
    ar=0,
    t={1,2,3,4,11,17,22,30,40},
    hos=false,
    gp=10,
    exp=1
  },{
    -- giant ant
    i=104,
    dmg=18,
    ar=3,
    hp=23,
    ch=0,
    t={1,2,3,4,5,6,7,8,9,10,16,17,22,26,27,30,31,32,38,40,41},
    exp=9,
    po=1,
    d={"chirp chirp!"}
  },{
    n="zombie",
    i=102,
    t={1,2,3,4,5,6,7,8,9,10,16,17,22,26,27,30,31,32,38,40,41},
    dmg=18,
    gp=10,
    exp=8,
    ch=0,
    d={"ahhrg!"}
  },{
    -- base animal
    exp=5,
    ch=2
  },{
    i=82,
    cs={{},{{4,5},{15,4}}},
    n="hunter",
    d={"the woods are scary now.","i\'m safer at home."}
  },{
    i=90,
    cs={{},{{15,4}}},
    n="cop",
    hp=85,
    dmg=60,
    ar=12,
    d={"thanks for your help.","this is beyond my ability."}
  },{
    i=77,
    fi=1,
    cs={{},{{1,4},{4,15},{6,1},{14,13}},{{1,4},{6,5},{14,10}},{{1,4},{4,15},{6,1},{14,3}}},
    n="merchant"
  },{
    i=81,
    fi=1,
    cs={{},{{2,9},{4,15},{13,14}},{{2,10},{4,15},{13,9}},{{2,11},{13,3}}},
    n="lady"
  },{
    i=92,
    n="scientist",
    cs={{},{{6,12},{15,4}},{{6,12}},{{15,4}}}
  },{
    i=78,
    fi=1,
    n="sunbather",
    cs={{},{{8,12},{15,4}},{{8,12}},{{15,4}},{{8,14}}},
    d={"i saw the weirdo!","i\'m glad we\'ve got locks."}
  },{
    i=79,
    fi=1,
    cs={{},{{8,12},{15,4}},{{8,12}},{{15,4}}},
    n="bodybuilder",
    d={"weird stuff up north.","we\'re vacationing indoors."}
  },{
    i=84,
    ac={{},{{2,5},{15,4}},{{2,4}},{2,5}},
    n="monk",
    d={"you are welcome here.","though we are troubled."}
  },{
    n="student",
    i=86,
    cs={{},{{15,4}},{{1,2},{15,4}},{{1,2}}}
  },{
    n="child",
    i=75,
    cs={{},{{15,4}},{{11,14},{3,8},{15,4}},{{11,14},{3,8}}},
    d={"the animals aren\'t right.","mom says stay inside."}
  },{
    i=88,
    cs={{},{{1,5},{8,2},{4,1},{2,12},{15,4}}},
    n="citizen"
  },{
    n="grocer",
    mch='food'
  },{
    n="clerk",
    mch='armor'
  },{
    n="vendor",
    mch='weapons'
  },{
    n="medic",
    mch='hospital'
  },{
    n="dealer",
    mch='guild'
  },{
    n="bartender",
    mch='bar',
    i=81
  },{
    -- zombie
    i=100
  },{
    -- robed zombie
  },{
    n="woods weirdo",
    t={6},
    ch=-1
  },{
    n="worker ant",
  },{
    i=110,
    n="winged ant",
    t={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,22,25,26,27,30,31,32,38,40,41}
  },{
    i=118,
    n="soldier ant",
    hp=64,
    dmg=23,
    exp=12
  },{
    i=125,
    n="queen ant",
    hp=235,
    dmg=42
  },{
    i=123,
    fi=1,
    n="ant larva",
    t={22},
    mva=0,
    hp=10,
    dmg=5,
    exp=5
  },{
    i=124,
    ia=124,
    n="ant eggs",
    hp=5
  },{
    i=106,
    n="large spider",
    hp=5,
    po=1,
    gp=8,
    exp=4
  },{
    i=108,
    n="large rat",
    t={1,2,3,4,5,6,7,8,10,16,17,22,26,27,30,31,32,38,40,41},
    hp=4,
    dmg=9,
    gp=2,
    po=1,
    eat=1,
    exp=2
  },{
    i=96,
    n="coyote",
    d={"grrr!"},
    ch=3
  },{
    i=120,
    n="lynx",
    d={"grar!"},
    ch=3
  },{
    i=112,
    ns={"snake","serpent"},
    hp=7,
    po=1,
    dmg=9,
    t={4,5,6,7},
    ch=1
  },{
    i=114,
    n="rattlesnake",
    po=1,
    exp=6
  },{
    i=116,
    n="large eel",
    hp=20,
    t={5,12,13,14,15,16},
    exp=7
  },{
    i=95,
    fi=1,
    n="scorpion",
    hp=8,
    po=1,
    ch=1
  },{
    i=122,
    cs={{},{{3,9},{11,10}},{{3,14},{11,15}}},
    fi=1,
    n="slime",
    gp=10,
    t={22,23},
    eat=1,
    exp=2
  },{
    i=98,
    ns={"big catfish","sturgeon"},
    hp=25,
    t={12,13,14,15,16},
    exp=9
  }
}
-- We want to make a table of nd i so we can refer to them easily
-- in the next section in such a way that they automatically get converted
-- to raw numbers for the final PICO-8 version.
thing = {}
for basetypeIdx, basetype in pairs(basetypes) do
  -- We don't always have a n; when we do, use it. If we have a list of
  -- possible ones, use the first.
  local n = basetype.n or (basetype.ns and basetype.ns[1])
  -- Otherwise just ignore it.
  if n then
    thing[n] = basetypeIdx
  end
end
-- write out the resulting basetypes & bestiary structure string for copying &
-- pasting into PICO-8 source
outputStructure('basetypes', basetypes)
-- copying is easier with a blank line between
print()

-- Our maps structure includes all of our communities and dungeons.
-- We can use the thing table defined above to make desired objects explicit.
maps={
  {
    n="village",
    ex=57,
    ey=37,
    sx=96,
    sy=54,
    mny=24,
    mxx=112,
    mxy=56,
    sn={
      {x=107,y=27,msg={"an engagement ring; steve","was ready to propose."}},
      {x=102,y=33,msg={"mary studied astronomy","as a hobby."}},
      {x=107,y=33,msg={"lou was a mixed martial","arts champion."}}
    },
    i={
      {id=thing['fountain'],x=103,y=38}
    },
    c={
      {id=thing['vendor'],x=91,y=27},
      {id=thing['hunter'],pn="fred",x=89,y=30},
      {id=thing['clerk'],x=85,y=27},
      {id=thing['grocer'],x=100,y=44},
      {id=thing['bartender'],x=108,y=37},
      {id=thing['lady'],x=87,y=37,d={"welcome to anteform valley.","we\'re all glad you\'re here."}},
      {id=thing['child'],pn="anne",x=86,y=51},
      {id=thing['child'],pn="flip",x=80,y=52,d={"greybeard hid his treasure!","it\'s on big sister island!"}},
      {id=thing['citizen'],pn="gwen",x=109,y=40,d={"steve, lou, & mary are gone.","i\'m not straying far."}},
      {id=thing['citizen'],pn="ralph",x=98,y=47,d={"radio square is southwest.","folks are missing there too."}},
      {id=thing['student'],pn="sally",x=108,y=30,d={"please find steve.","our rooms are adjacent."}},
      {id=thing['cop'],pn="bruce",x=105,y=37}
    }
  },{
    n="thinktank",
    ex=26,
    ey=33,
    sx=116,
    sy=23,
    mnx=104,
    mxy=24,
    sn={
      {x=120,y=6,msg={"a myrmecology paper by","dr. greene."}},
      {x=105,y=11,msg={"a paper on irregular growth","in animals."}},
      {x=120,y=11,msg={"data on valley insect","populations."}}
    },
    i={
      {id=thing['ladder down'],x=123,y=4,tm=3,tx=126,ty=33,tz=0}
    },
    c={
      {id=thing['cop'],pn="artemis",x=116,y=18,mva=0,d={"spooky stuff\'s afoot.","missing people; crazy animals."}},
      {id=thing['grocer'],x=110,y=17},
      {id=thing['medic'],x=106,y=1},
      {id=thing['scientist'],pn="dr. wong",x=111,y=7,d={"concentrated it can burn.","a simple carboxyl."}},
      {id=thing['scientist'],pn="dr. tetrado",x=120,y=20,d={"find dr. tucker.","he\'s figured it out."}},
      {id=thing['scientist'],pn="dr. greene",x=110,y=22,d={"several of us are missing.","those researching up north."}}
    }
  },{
    n="the basement",
    sx=126,
    sy=33,
    mnx=112,
    mny=32,
    mxy=43,
    sn={
      {x=123,y=33,msg={"a paper on insect","pheromones."}},
      {x=124,y=33,msg={"a paper on formic acid","& its effects."}},
      {x=123,y=35,msg={"a paper on ants controlling","aphids."}},
      {x=113,y=33,msg={"a diagram of modified aphid","brains."}}
    },
    i={
      {id=thing['ladder up'],x=126,y=33,tm=2,tx=123,ty=4,tz=0}
    },
    c={
      {id=thing['scientist'],pn="dr. tucker",x=115,y=34,d={"it\'s semiochemicals.","controlled living corpses."}},
      {id=thing['scientist'],pn="dr. agawon",x=119,y=33,d={"they\'re literally brain dead.","higher functions burnt out."}},
    }
  },{
    n="monastery",
    ex=34,
    ey=17,
    sx=92,
    sy=23,
    mxx=105,
    mxy=24,
    sn={
      {x=92,y=20,msg="he\'ll rise again!"},
      {x=100,y=9,msg={"a secret prophesy about the","eschaton starting here."}}
    },
    i={
      {id=thing['ladder up'],x=84,y=9,tm=5,tx=113,ty=31,tz=0},
      {id=thing['fountain'],x=92,y=6}
    },
    c={
      {id=thing['monk'],pn="bro. meinrad",x=89,y=21},
      {id=thing['medic'],x=85,y=15},
      {id=thing['grocer'],x=99,y=15},
      {id=thing['monk'],pn="sis. pat",x=92,y=1,d={"i saw the flash in heaven.","the animals now punish us."}},
      {id=thing['student'],pn="learner jo",x=82,y=5,d={"i found the star jelly.","i think it turned the beasts."}},
      {id=thing['monk'],pn="sis. gail",x=90,y=6,d={"god sent us a sign.","i saw his star fall to earth."}}
    }
  },{
    n="the top floor",
    sx=113,
    sy=31,
    mnx=112,
    mny=24,
    mxy=33,
    sn={
      {x=126,y=29,msg={"a list of the missing;","many monks & nuns are gone."}}
    },
    i={
      {id=thing['ladder down'],x=113,y=31,tm=4,tx=84,ty=9,tz=0}
    },
    c={
      {id=thing['monk'],pn="bro. stamos",x=117,y=27,d={"we know of your quest.","we will help as we can."}},
      {id=thing['monk'],pn="mother francine",x=125,y=30,d={"some of us were taken.","they are now possessed."}},
      {id=thing['monk'],pn="father ted",x=126,y=26,d={"our dead brothers & sisters.","they are beset by demons."}},
    }
  },{
    n="hermit cabin",
    ex=3,
    ey=47,
    sx=111,
    sy=57,
    mnx=103,
    mny=56,
    mxx=113,
    sn={
      {x=105,y=61,msg={"no one\'s been here in awhile.","seems the hermit\'s missing too."}}
    },
    i={
      {id=thing['boat'],x=110,y=61}
    }
  },{
    n="radio square",
    ex=34,
    ey=53,
    sx=119,
    sy=62,
    mnx=112,
    mny=43,
    sn={
      {x=114,y=53,msg={"the dj was investigating","the monks for the news."}},
      {x=114,y=56,msg={"some expired coupons & a","copy of \'coyote waits\'."}},
      {x=114,y=59,msg={"a zombie comic book; someone","drew a robe on the zombie."}}
    },
    c={
      {id=thing['vendor'],x=114,y=45},
      {id=thing['grocer'],x=123,y=60},
      {id=thing['dealer'],x=123,y=44},
      {id=thing['hunter'],pn="becky",x=126,y=46,d={"the hermit has a boat.","west past the billabong."}},
      {id=thing['hunter'],pn="jack",x=114,y=48,d={"it\'s in the n.e. cabin.","you can borrow my shotgun."}},
      {id=thing['merchant'],pn="dj jazzy joe",x=121,y=53,i=80,d={"the monks have seen them.","the woods weirdos."}},
      {id=thing['citizen'],pn="emma",x=124,y=58,d={"hang out in the bar none.","good woods weirdos discussion."}}
    }
  },{
    n="southern cabin",
    ex=38,
    ey=60,
    sx=100,
    sy=62,
    mnx=96,
    mxx=104,
    mny=56,
    c={
      {id=thing['hunter'],pn="sue",x=101,y=59,d={"you can borrow my vest.","you\'ll need it."}}
    }
  },{
    n="pennisula cabin",
    ex=56,
    ey=24,
    sx=100,
    sy=62,
    mnx=96,
    mxx=104,
    mny=56,
    c={
      {id=thing['bodybuilder'],pn="jim",x=99,y=58,d={"weird stuff up north.","we\'re vacationing indoors."}},
      {id=thing['sunbather'],pn="daisy",x=98,y=58,d={"i saw the weirdo!","i\'m glad we\'ve got locks."}}
    }
  },{
    n="lakeside cabin",
    ex=41,
    ey=39,
    sx=100,
    sy=62,
    mnx=96,
    mxx=104,
    mny=56,
    c={
      {id=thing['sunbather'],pn="jane",x=98,y=58,d={"find my friend to the south.","she\'ll help."}}
    }
  },{
    n="western cabin",
    ex=21,
    ey=28,
    sx=100,
    sy=62,
    mnx=96,
    mxx=104,
    mny=56,
    sn={
      {x=101,y=60,msg={"a sketch of an ant","moving a rock."}}
    },
  },{
    n="hunting cabin",
    ex=75,
    ey=3,
    sx=100,
    sy=62,
    mnx=96,
    mxx=104,
    mny=56
  },{
    n="the queen\'s chamber",
    sx=113,
    sy=31,
    mnx=80,
    mny=56,
    mxx=96,
    fri=false,
    ss=14,
    newm=26,
    mxm=15,
    i={
      {id=thing['ladder up'],x=94,y=62,tm=16,tx=3,ty=6,tz=1},
      {id=thing['ant eggs'],x=83,y=59},
      {id=thing['ant eggs'],x=84,y=59},
      {id=thing['ant eggs'],x=84,y=60}
    },
    c={
      {id=thing['queen ant'],x=83,y=60},
      {id=thing['ant larva'],x=93,y=58},
      {id=thing['soldier ant'],x=89,y=57},
      {id=thing['soldier ant'],x=89,y=62},
      {id=thing['soldier ant'],x=94,y=57}
    }
  },{
    n="greybeard\'s cave",
    ex=57,
    ey=33,
    sy=8,
    l={
      {
        0x0000,
        0xff3c,
        0x030e,
        0x33cf,
        0x3ccc,
        0x3000,
        0x3ffc,
        0x4000
      },{
        0x0002,
        0xcf3f,
        0x03c1,
        0x303c,
        0x3fcc,
        0x000c,
        0xf3cc,
        0x00c0
      },{
        0x300d,
        0x33cc,
        0x00c0,
        0x3cfc,
        0x303c,
        0x3300,
        0xf3fc,
        0x00c0
      }
    },
    i={
      {id=thing['ladder up'],x=1,y=8,z=1},
      {id=thing['ladder up'],x=8,y=3,z=2},
      {id=thing['ladder up'],x=8,y=1,z=3},
      {id=thing['chest'],x=4,y=8,z=3}
    }
  },{
    n="vetusaur mine",
    ex=3,
    ey=9,
    sx=8,
    sy=8,
    ss=14,
    l={
      {
        0x00cd,
        0x3c00,
        0x0b3c,
        0x3fc0,
        0x303f,
        0xf300,
        0x03fc,
        0xb301
      },{
        0x0030,
        0x3f00,
        0x37fc,
        0x0300,
        0xff0c,
        0x030c,
        0x333f,
        0x7000
      }
    },
    i={
      {id=thing['ladder up'],x=8,y=8,z=1},
      {id=thing['ladder up'],x=8,y=1,z=1,tm=0,tx=3,ty=5,tz=0},
      {id=thing['ladder up'],x=3,y=3,z=2},
      {id=thing['ladder up'],x=1,y=8,z=2}
    },
    c={
      {id=thing['worker ant'],x=7,y=8,z=1,ch=-2}
    }
  },{
    n="formika mine",
    ex=7,
    ey=3,
    sx=4,
    sy=8,
    ss=14,
    l={
      {
        0x00cc,
        0x30c0,
        0x33fc,
        0x300c,
        0x3fcc,
        0x380c,
        0x3ffc,
        0x0100
      }
    },
    i={
      {id=thing['ladder up'],x=4,y=8,z=1},
      {id=thing['ladder down'],x=3,y=6,z=1,tm=13,tx=94,ty=62,tz=0}
    }
  }
}
-- Map by map...
for mapIdx, map in pairs(maps) do
  -- Only for dungeons...
  if map.l then
    -- Level by level...
    for levelIdx, level in pairs(map.l) do
      -- Row by row...
      for rowIdx, row in pairs(level) do
        -- We need to convert values outside PICO-8's range.
        if row > 32767 then
          level[rowIdx] = row - 65536
        end
      end
    end
  end
end
-- write out the resulting maps structure string for copying & pasting
-- into PICO-8 source
outputStructure('maps', maps)

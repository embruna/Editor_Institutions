#FUNCTION TO CORRECT EDITOR NAMES, IDs, and INSTITUTIONS
  editor_cleaner <- function(DATAFILE) {
    
    
    # DATAFILE<-ALLDATA
    # SOME NOTES:
    
    
    DATAFILE$FIRST_NAME<-trimws(DATAFILE$FIRST_NAME)
    DATAFILE$LAST_NAME<-trimws(DATAFILE$LAST_NAME)
    str(DATAFILE$editor_id)
    DATAFILE$INST<-as.factor(DATAFILE$INST)
    levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"University of Missouri Columbia",
                               "University of Washington Seattle",
                               "dsir land resources",
                               "csic estacion experimental de zonas aridas",
                               "csic national museum of natural sciences",
                               "University of Washington Bothell",
                               "james cook university",
                               "ARC Centre of Excellence for Coral Reef Studies",
                               "university of montana",
                               "Flathead Lake Biological Station",
                               "university of notre dame",
                               "mount allison university",
                               "cinvestav irapuato",
                               "neri",
                               "memorial university of newfoundland",
                               "unaffiliated",
                               "Chinese Academy of Sciences",
                               "University of Massachusetts at Amherst",
                               "University of Chicago",
                               "University of Calgary",
                               "University of Adelaide",
                               "University of Alberta",
                               "US Forest Service",
                               "University of Bristol",
                               "University of Canterbury",
                               "Stanford University",
                               "NERC Centre for Population Biology",
                               "Cardiff University",
                               "University of Arizona",
                               "University of Arizona",
                               "University of Antwerp",
                               "california state university long beach",
                               "dsir land resources",
                               "max planck institute for chemical ecology",
                               "university college dublin",
                               "university of sheffield",
                               "leibniz institute for zoo and wildlife research",
                               "kansas state university",
                               "csiro ecosystem sciences",
                               "CNRS Centre dEcologie Fonctionnelle et Evolutive",
                               "Forestry and Forest Products Research Institute",
                               "University of Minnesota Duluth",
                               "University of Minnesota Crookston",
                               "University of Toronto Mississauga",
                               "State University of New York College of Environmental Science and Forestry",
                               "Calyx, Inc.","University of North Carolina Charlotte",
                               "Smithsonian National Museum of Natural History",
                               "Smithsonian National Zoological Park",
                               "australian national university",
                               "university of california irvine",
                               "university of california davis",
                               "university of california berkeley",
                               "Laboratoire Associe de Modelisation des Plantes",
                               "Southern Illinois University",
                               "universite libre de bruxelles",
                               "Free University Amsterdam",
                               "universitat osnabruck",
                               "southern illinois u",
                               "Aarhus University",
                               "University of St. Andrews",
                               "university of st andrews",
                               "university of uppsala",
                               "NERC Centre for Population Biology",
                               "Massey University",
                               "University of Bialystok",
                               "manaaki whenua landcare research",
                               "queens university belfast",
                               "university of wisconsin milwaukee",
                               "university of copenhagen",
                               "no one by this name",
                               "university of sydney",
                               "university of vienna",
                               "Fundacion Cedrela",
                               "university of gottingen",
                               "smithsonian national museum of natural history",
                               "university of wageningen",
                               "University of Missouri St Louis",
                               "university of colorado boulder",
                               "university of chicago",
                               "australian national university",
                               "georgia institute of technology",
                               "mcmaster university",
                               "university of toronto",
                               "university of illinois",
                               "university of texas austin",
                               "university of maryland baltimore county",
                               "university of st andrews",
                               "university of oxford",
                               "university of minnesota duluth",
                               "indiana university",
                               "indiana university",
                               "duke university",
                               "stanford university",
                               "university of utah",
                               "university of minnesota",
                               "university of chicago",
                               "michigan state university",
                               "smithsonian national museum of natural history",
                               "university of california santa barbara",
                               "university of paris",
                               "norther arizona university",
                               "university of southampton",
                               "university of cape town",
                               "arizona state university",
                               "wildlife institute of india",
                               "ashoka trust for research in ecology and the environment",
                               "university of leiden",
                               "university of oklahoma",
                               "university of liverpool",
                               "station biologique de la tour du valat",
                               "unaffiliated",
                               "australian national university",
                               "universidade estadual de campinas",
                               "missouri botanical garden",
                               "university of illinois urbana champaign",
                               "usgs patuxent wildlife research center",
                               "curtin university of technology",
                               "smithsonian tropical research institute",
                               "university of british columbia",
                               "university of houston",
                               "kent state university",
                               "university of minnesota",
                               "michigan state university",
                               "university of southern california",
                               "university of queensland",
                               "monash university",
                               "university of oxford",
                               "university of edinburgh",
                               "indiana university",
                               "university of georgia",
                               "university of helsinki",
                               "monash university",
                               "university of arizona",
                               "university of iowa",
                               "university of british columbia",
                               "unversity of michigan",
                               "cnrs roscoff biological station",
                               "university of leiden",
                               "university of georgia",
                               "iowa state university",
                               "university of exeter",
                               "university of strathclyde",
                               "flinders university",
                               "worldfish centre",
                               "netherlands institute of ecology",
                               "western ecosystem technology",
                               "university of guelph",
                               "university of aberdeen",
                               "tel aviv university",
                               "queen mary university of london",
                               "imperial college london",
                               "university of oxford",
                               "university of cantebury",
                               "soafd fisheries laboratory",
                               "university of glasgow ",
                               "australian national university",
                               "liverpool john moores university",
                               "silsoe research institute",
                               "purdue university",
                               "university of oxford",
                               "game conservancy",
                               "university of essex",
                               "university of reading",
                               "institute of terrestrial ecology",
                               "university of nottingham",
                               "central electricity research laboratories",
                               "imperial college london",
                               "central science laboratory",
                               "afrc institute for grassland and animal production",
                               "fauna and flora international",
                               "institute of terrestrial ecology",
                               "university of hong kong",
                               "university of washington",
                               "natural history museum london",
                               "nerc centre for ecology and hydrology",
                               "universidade federal de goias",
                               "csic - ipna",
                               "durham university",
                               "auckland university of technology",
                               "university of evora",
                               "universite de montpellier",
                               "university of zurich",
                               "university college london",
                               "goethe university frankfurt",
                               "university of gottingen",
                               "university of otago",
                               "university of california san diego",
                               "yale university",
                               "museo de la plata",
                               "university of umea",
                               "university of umea",
                               "chinese academy of sciences",
                               "queens university belfast",
                               "university of kwazulu natal",
                               "university of ft hare",
                               "university of athens",
                               "university of edinburgh",
                               "royal botanic gardens kew",
                               "royal botanic garden edinburgh",
                               "university of wisconsin",
                               "university of azores",
                               "national & kapodistrian university",
                               "university of amsterdam",
                               "university of hull",
                               "radboud university nijmegen",
                               "university of wales",
                               "cardiff university",
                               "university of sheffield",
                               "cambridge university",
                               "swansea university",
                               "oxford brookes university",
                               "university of essex",
                               "radboud university nijmegen",
                               "leiden university",
                               "wageningen university and research",
                               "university college dublin",
                               "zoological society of london",
                               "lancaster university",
                               "university of sydney",
                               "vrije universiteit amsterdam",
                               "university of toronto",
                               "wesleyan university",
                               "university of wales",
                               "western sydney university",
                               "university of calgary",
                               "lancaster university",
                               "university of indiana",
                               "swiss federal institute of technology",
                               "university of umea",
                               "simon fraser university",
                               "university of vienna",
                               "university of texas arlington",
                               "university of minnesota",
                               "smithsonian tropical research institute",
                               "university of michigan",
                               "royal holloway university of london",
                               "university of basel",
                               "university of turku ",
                               "florida international university",
                               "cnrs institut ecologie et environnement",
                               "university of florida",
                               "george washington university",
                               "university of bielefeld",
                               "estonian university of life sciences",
                               "universite de rennes 1",
                               "university of vienna",
                               "university of california san diego",
                               "rice university",
                               "utah state university",
                               "university of queensland",
                               "university of kansas",
                               "university of innsbruck ",
                               "technical university of munich",
                               "queens university",
                               "lund university",
                               "geological survey of denmark",
                               "swedish university of agricultural sciences",
                               "university of copenhagen",
                               "Oxford Forestry Institute",
                               "agricultural university of norway",
                               "norwegian university of science and technology",
                               "university of helsinki",
                               "swiss federal institute of aquatic science and technology",
                               "university of california santa cruz",
                               "british trust for ornithology",
                               "evergreen state college",
                               "university of freiburg",
                               "swedish university of agricultural sciences",
                               "university of hong kong",
                               "mediterranean institute for advanced studies",
                               "technical university of berlin",
                               "university of virginia",
                               "university of aberdeen",
                               "university of cambridge",
                               "university of haifa-oranim",
                               "university of eastern finland",
                               "vrije universiteit amsterdam",
                               "university of north carolina chapel hill",
                               "university college dublin"
                               )
    #### 
    # These corrections are from PJ review of files
    #####
    
    
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="Cymerman"& DATAFILE$JOURNAL=="LECO"]<-"Hepinstall-Cymerman"
    
    
    DATAFILE$INST[DATAFILE$LAST_NAME=="Voigt"& DATAFILE$JOURNAL=="OECOL" &
                    (DATAFILE$YEAR==2013|DATAFILE$YEAR==2014)]<-"leibniz institute for zoo and wildlife research"
    
    DATAFILE$INST[DATAFILE$LAST_NAME=="Rees"& DATAFILE$JOURNAL=="JECOL" &
                    DATAFILE$YEAR==2012]<-"university of sheffield"
    
    DATAFILE$INST[DATAFILE$LAST_NAME=="Holzner"& DATAFILE$JOURNAL=="PLANTECOL" &
                    (DATAFILE$YEAR>1984 & DATAFILE$YEAR<1988)]<-"university of vienna"
    
    DATAFILE$INST_CHECK[DATAFILE$LAST_NAME=="Holzner"& DATAFILE$JOURNAL=="PLANTECOL" &
                    (DATAFILE$YEAR>1984 & DATAFILE$YEAR<1988)]<-"2x inst"
    
    DATAFILE$CITY[DATAFILE$LAST_NAME=="Korner"& DATAFILE$JOURNAL=="OECOL" &
                          (DATAFILE$YEAR>1989 & DATAFILE$YEAR<2015)]<-"Basel"
    
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="Korner"& DATAFILE$JOURNAL=="OECOL" &
                    (DATAFILE$YEAR>1989 & DATAFILE$YEAR<2015)]<-"Switzerland"
    
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Bever" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="OECOL"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Lichstein" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="OECOL"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Gough" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="OECOL"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Heimpel" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="OECOL"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Ibanez" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="OECOL"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Layman" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="OECOL"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Le Galliard" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="OECOL"),]

    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Masters" & DATAFILE$YEAR==2010 & DATAFILE$JOURNAL=="JBIOG"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Holland" & DATAFILE$YEAR==1985 & DATAFILE$JOURNAL=="JBIOG"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Lambers" & DATAFILE$YEAR==1993 & DATAFILE$JOURNAL=="PLANTECOL"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Bond" & DATAFILE$YEAR==2004 & DATAFILE$JOURNAL=="PLANTECOL"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Kollmann" & DATAFILE$YEAR==2007 & DATAFILE$JOURNAL=="PLANTECOL"),]
    
    
    DATAFILE$CITY[DATAFILE$LAST_NAME=="Overton"& DATAFILE$JOURNAL=="LECO"]<-NA
    
    DATAFILE$geo.code[DATAFILE$LAST_NAME=="Betts"& DATAFILE$JOURNAL=="LECO"]<-"CAN"
    
    DATAFILE$LAST_NAME[DATAFILE$FIRST_NAME=="Kimberly" & DATAFILE$JOURNAL=="LECO" & DATAFILE$YEAR==2004]<-"With"
    
    DATAFILE$MIDDLE_NAME[DATAFILE$FIRST_NAME=="Kimberly" & DATAFILE$JOURNAL=="LECO" & DATAFILE$YEAR==2004]<-"A"
    
    DATAFILE$editor_id[DATAFILE$FIRST_NAME=="Kimberly" & DATAFILE$JOURNAL=="LECO" & DATAFILE$YEAR==2004]<-"2113"
    
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="Lookinbill" & DATAFILE$JOURNAL=="LECO"]<-"Lookingbill"
    
    
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="Duggan" & DATAFILE$YEAR==2008),] #not in the front matter
    
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="VanDer" & DATAFILE$JOURNAL=="LECO"]<-"VanDerMaarel"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="MOYLE" & DATAFILE$FIRST_NAME=="PETER"]<-"Moyle"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="LI" & DATAFILE$FIRST_NAME=="Feng-Min"]<-"Li"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="DeWalt" & DATAFILE$FIRST_NAME=="Saara"]<-"Dewalt"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="DeVictor" & DATAFILE$FIRST_NAME=="Vincent"]<-"Devictor"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="COE" & DATAFILE$FIRST_NAME=="M"]<-"Coe"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="MONOD"]<-"Monod"
    
    DATAFILE$FIRST_NAME[DATAFILE$LAST_NAME=="Moyle" & DATAFILE$FIRST_NAME=="PETER"]<-"Peter"
    DATAFILE$FIRST_NAME[DATAFILE$LAST_NAME=="MONOD"]<-"Theodore"
    DATAFILE$UNIT[DATAFILE$LAST_NAME=="Meffe" & DATAFILE$JOURNAL=="CONBIO" & DATAFILE$INST=="University of Montana"]<-NA
    DATAFILE$UNIT[DATAFILE$LAST_NAME=="Whitmore"]<-"Oxford Forestry Institute"
    DATAFILE$UNIT[DATAFILE$LAST_NAME=="Gustafson"& DATAFILE$FIRST_NAME=="E"]<-"North Central Research Station"
    DATAFILE$UNIT[DATAFILE$LAST_NAME=="With"& DATAFILE$FIRST_NAME=="Kimberly" & DATAFILE$YEAR==2004]<-"Division of Biology"
    DATAFILE$INST[DATAFILE$LAST_NAME=="With"& DATAFILE$FIRST_NAME=="Kimberly" & DATAFILE$YEAR==2004]<-"kansas state university"
    DATAFILE$STATE[DATAFILE$LAST_NAME=="With"& DATAFILE$FIRST_NAME=="Kimberly" & DATAFILE$YEAR==2004]<-"KS"
    DATAFILE$CITY[DATAFILE$LAST_NAME=="With"& DATAFILE$FIRST_NAME=="Kimberly" & DATAFILE$YEAR==2004]<-"Manhattan"
    
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="VanDonk" & DATAFILE$JOURNAL=="ECOLOGY"]<-"1037"
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="VanDerMaarel" & DATAFILE$JOURNAL=="LECO"]<-"1033"
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Smith" & DATAFILE$FIRST_NAME=="Melinda"]<-"2566"
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Smith" & DATAFILE$FIRST_NAME=="Danny"]<-NA
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Moyle" & DATAFILE$FIRST_NAME=="Peter"]<-2874
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Li" & DATAFILE$FIRST_NAME=="Feng-Min"]<-1098
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Lee" & DATAFILE$FIRST_NAME=="Carol"]<-3860
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Fry" & DATAFILE$FIRST_NAME=="James"]<-3891
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Dewalt" & DATAFILE$FIRST_NAME=="Saara"]<-3311
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Devictor" & DATAFILE$FIRST_NAME=="Vincent"]<-3681
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Coe" & DATAFILE$FIRST_NAME=="M"]<-2332
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="Krams" & DATAFILE$FIRST_NAME=="Indrikis"]<-70
    
    
    
    
    
    ##########
    DATAFILE$INST[DATAFILE$LAST_NAME=="James" & DATAFILE$FIRST_NAME=="Helen" & DATAFILE$JOURNAL=="AUK"]<-"smithsonian national museum of natural history"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Lonsdale" & DATAFILE$JOURNAL=="JAPE" & DATAFILE$FIRST_NAME=="W"& (DATAFILE$YEAR>1998| DATAFILE$YEAR<2003)]<-"csiro ecosystem sciences"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Heil" & DATAFILE$JOURNAL=="JECOL" & DATAFILE$FIRST_NAME=="Martin" & DATAFILE$INST=="max planck university"]<-"max planck institute for chemical ecology"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Heil" & DATAFILE$JOURNAL=="JECOL" & DATAFILE$FIRST_NAME=="Martin" & DATAFILE$YEAR>2009]<-"cinvestav irapuato"
    DATAFILE$INST[DATAFILE$LAST_NAME=="McGlone" & DATAFILE$JOURNAL=="JBIOG" & DATAFILE$INST=="christchurch"]<-"dsir land resources"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Williams" & DATAFILE$JOURNAL=="FUNECOL" & DATAFILE$FIRST_NAME=="Caroline"& (DATAFILE$YEAR>2008| DATAFILE$YEAR<2012)]<-"university of california berkeley"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Wainwright" & DATAFILE$JOURNAL=="FUNECOL" & DATAFILE$FIRST_NAME=="Peter" & (DATAFILE$YEAR>2008| DATAFILE$YEAR<2012)]<-"university of california davis"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Treseder" & DATAFILE$JOURNAL=="FUNECOL" & DATAFILE$FIRST_NAME=="Kathleen"& (DATAFILE$YEAR>2008| DATAFILE$YEAR<2016)]<-"university of california irvine"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Carroll" & DATAFILE$JOURNAL=="FUNECOL" & DATAFILE$FIRST_NAME=="Scott"& (DATAFILE$YEAR>2006| DATAFILE$YEAR<2016)]<-"university of california davis"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Campbell" & DATAFILE$JOURNAL=="FUNECOL" & DATAFILE$FIRST_NAME=="Diane"]<-"university of california irvine"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Farquhar" & DATAFILE$JOURNAL=="FUNECOL" & DATAFILE$FIRST_NAME=="Graham"]<-"australian national university"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Fernandez-Juricic" & DATAFILE$JOURNAL=="JAPE" & DATAFILE$YEAR==2008]<-"california state university long beach"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Zuidema" & DATAFILE$JOURNAL=="JECOL" & DATAFILE$YEAR==2007]<-"university of wageningen"
    DATAFILE$INST[DATAFILE$LAST_NAME=="VanDerPutten" & DATAFILE$JOURNAL=="OIKOS"]<-"university of wageningen"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Lieth" & DATAFILE$FIRST_NAME=="Helmut" & DATAFILE$JOURNAL=="JBIOG"]<-"universitat osnabruck"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Polle" & DATAFILE$FIRST_NAME=="Andrea" & DATAFILE$JOURNAL=="NEWPHYTE"]<-"university of gottingen"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Wardle" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="PLANTECOL"]<-"university of sydney"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Fenchel" & DATAFILE$FIRST_NAME=="Tom" & DATAFILE$JOURNAL=="FUNECOL"]<-"university of copenhagen"
    DATAFILE$INST[DATAFILE$LAST_NAME=="White" & DATAFILE$JOURNAL=="JECOL" & DATAFILE$YEAR==1992]<-"university college dublin"
    DATAFILE$INST[DATAFILE$LAST_NAME=="DeSteven" & DATAFILE$JOURNAL=="ECOLOGY" & DATAFILE$YEAR==1999]<-"university of wisconsin milwaukee"
    DATAFILE$INST[DATAFILE$LAST_NAME=="VanDerMaarel" & DATAFILE$JOURNAL=="LECO"]<-"university of uppsala"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Sprules" & DATAFILE$FIRST_NAME=="Gary"]<-"University of Toronto Mississauga"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Wagner" & DATAFILE$FIRST_NAME=="Helene"]<-"University of Toronto Mississauga"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Kotanen" & DATAFILE$FIRST_NAME=="Peter"]<-"University of Toronto Mississauga"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Loiselle" & DATAFILE$FIRST_NAME=="Bette"]<-"University of Missouri St Louis"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Ricklefs" & DATAFILE$FIRST_NAME=="Robert"]<-"University of Missouri St Louis"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Renner" & DATAFILE$FIRST_NAME=="Susanne"]<-"University of Missouri St Louis"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Sork" & DATAFILE$FIRST_NAME=="Victoria"]<-"University of Missouri St Louis"
    # DATAFILE$INST[DATAFILE$LAST_NAME=="Parmentier"]<-"universite libre de bruxelles"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Debussche"]<-"CNRS Centre dEcologie Fonctionnelle et Evolutive"
    # levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"Forestry and Forest Products Research Institute","University of Minnesota Duluth","University of Minnesota Crookston")
    DATAFILE$INST[DATAFILE$LAST_NAME=="Fujimori"]<-"Forestry and Forest Products Research Institute"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Johnson" & DATAFILE$FIRST_NAME=="Lucinda"]<-"University of Minnesota Duluth"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Parmentier" & DATAFILE$JOURNAL=="BITR"]<-"universite libre de bruxelles"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Moen" & DATAFILE$FIRST_NAME=="Ron"]<-"University of Minnesota Duluth"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Sterner" & DATAFILE$FIRST_NAME=="Robert"]<-"University of Minnesota Duluth"
    DATAFILE$INST[DATAFILE$LAST_NAME=="VanDerHeijden" & DATAFILE$FIRST_NAME=="Marcel"]<-"Free University Amsterdam"
    DATAFILE$INST[DATAFILE$LAST_NAME=="VanDamme" & DATAFILE$FIRST_NAME=="Raoul"]<-"University of Antwerp"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Badyaev" & DATAFILE$FIRST_NAME=="Alexander"]<-"University of Arizona"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Davidowitz" & DATAFILE$FIRST_NAME=="Goggy"]<-"University of Arizona"
    DATAFILE$INST[DATAFILE$LAST_NAME=="McGraw" & DATAFILE$FIRST_NAME=="Kevin"]<-"arizona state university"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Cotter" & DATAFILE$FIRST_NAME=="Sheena"]<-"queens university belfast"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Norden" & DATAFILE$JOURNAL=="FUNECOL"]<-"Fundacion Cedrela"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Jones" & DATAFILE$INST=="Ascot"]<-"NERC Centre for Population Biology"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Jones" & DATAFILE$INST=="Cardiff"]<-"Cardiff University"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Raubenheimer" & DATAFILE$FIRST_NAME=="David"]<-"Massey University"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Niu" & DATAFILE$FIRST_NAME=="Shuli"]<-"Chinese Academy of Sciences"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Konarzewski" & DATAFILE$INST=="Bialystok"]<-"University of Bialystok"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Newman" & DATAFILE$INST=="Bristol"]<-"University of Bristol"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Bergelson" & DATAFILE$INST=="Chicago"]<-"University of Chicago"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Turnbull" & DATAFILE$INST=="Christchurch"]<-"University of Canterbury"
    DATAFILE$INST[DATAFILE$LAST_NAME=="McGlone" & DATAFILE$INST=="Christchurch"]<-"manaaki whenua landcare research"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Boggs" & DATAFILE$INST=="Colorado"]<-"Stanford University"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Wiersma" & DATAFILE$FIRST_NAME=="Jochum"]<-"University of Minnesota Crookston"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Smith" & DATAFILE$FIRST_NAME=="Madeleine"]<-"University of Minnesota Crookston"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Sims" & DATAFILE$FIRST_NAME=="Albert"]<-"University of Minnesota Crookston"
    DATAFILE$INST[DATAFILE$JOURNAL=="AMNAT" & DATAFILE$LAST_NAME=="Case"]<-"university of california san diego"
    # DATAFILE$INST[DATAFILE$LAST_NAME=="Noon"]<-"Colorado State University"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Burgess"]<-"State University of New York College of Environmental Science and Forestry"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Fragoso"]<-"State University of New York College of Environmental Science and Forestry"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Yanai"]<-"State University of New York College of Environmental Science and Forestry"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Hall" & DATAFILE$FIRST_NAME=="Charles" & DATAFILE$JOURNAL=="CONBIO"]<-
      "State University of New York College of Environmental Science and Forestry"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Pressey" & DATAFILE$YEAR==2006 & DATAFILE$JOURNAL=="CONBIO"]<-
      "unaffiliated"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Damgaard" & (DATAFILE$YEAR>2005 & DATAFILE$YEAR<2010) & DATAFILE$JOURNAL=="FUNECOL"]<-
      "neri"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Leroux" & DATAFILE$YEAR==2015 & DATAFILE$JOURNAL=="FUNECOL"]<-
      "memorial university of newfoundland"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Noon" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="CONBIO"]<-
      "university of colorado boulder"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Noon" & DATAFILE$YEAR==2014 & DATAFILE$JOURNAL=="CONBIO"]<-
      "university of colorado boulder"
    DATAFILE$INST[DATAFILE$INST=="University of Missouri"]<-"University of Missouri Columbia" 
    DATAFILE$INST[DATAFILE$JOURNAL=="AMNAT" & DATAFILE$LAST_NAME=="Case"]<-"university of california san diego"
    # DATAFILE$INST[DATAFILE$LAST_NAME=="Noon"]<-"Colorado State University"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Bowers" & DATAFILE$CITY=="Vadnais Heights"]<-"Calyx, Inc."
    DATAFILE$INST[DATAFILE$LAST_NAME=="Debussche"]<-"CNRS Centre dEcologie Fonctionnelle et Evolutive"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Whitmore"]<-"university of oxford"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Hails"]<-"university of oxford"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Leamy"]<-"University of North Carolina Charlotte"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Gustafson"& DATAFILE$FIRST_NAME=="E"]<-"US Forest Service"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Labandeira"& DATAFILE$FIRST_NAME=="Conrad"]<-"Smithsonian National Museum of Natural History"
    DATAFILE$INST[DATAFILE$LAST_NAME=="SEIDENSTZICKER"& DATAFILE$FIRST_NAME=="JOHN"]<-"Smithsonian National Zoological Park"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Kleiman"& DATAFILE$FIRST_NAME=="Devra"]<-"Smithsonian National Zoological Park"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Houllier" & (DATAFILE$YEAR>1998 & DATAFILE$YEAR<2003)]<-NA
    DATAFILE$INST[DATAFILE$LAST_NAME=="Houllier" & (DATAFILE$YEAR>1998 & DATAFILE$YEAR<2003)]<-"Laboratoire Associe de Modelisation des Plantes"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Watling"& DATAFILE$INST=="Adelaide"]<-"University of Adelaide"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Boutin" & DATAFILE$INST=="Alberta"]<-"University of Alberta"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Fox" & DATAFILE$INST=="Alberta"]<-"University of Calgary"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Patek" & DATAFILE$INST=="Amherst"]<-"University of Massachusetts at Amherst"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Mcgraw" & DATAFILE$INST=="Arizona"]<-"arizona state university"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Marzluff" & DATAFILE$INST=="University of Washington"]<-"University of Washington Seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Felsenstein" & DATAFILE$INST=="University of Washington"]<-"University of Washington Seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Tewksbury" & DATAFILE$INST=="University of Washington"]<-"University of Washington Seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Kareiva" & DATAFILE$INST=="University of Washington"]<-"University of Washington Seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Williams-Guillen" & DATAFILE$INST=="University of Washington"]<-"University of Washington Bothell"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Slatkin" & DATAFILE$INST=="University of Washington"]<-"University of Washington Seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Huey" & DATAFILE$INST=="University of Washington"]<-"University of Washington Seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Olmstead" & DATAFILE$INST=="University of Washington"]<-"University of Washington Seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Rohwer" & DATAFILE$INST=="University of Washington"]<-"University of Washington Seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Brubaker" & DATAFILE$INST=="University of Washington"]<-"University of Washington Seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Naiman" & DATAFILE$INST=="University of Washington"]<-"University of Washington Seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Schindler" & DATAFILE$INST=="University of Washington"]<-"University of Washington Seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Wingfield" & DATAFILE$INST=="University of Washington"]<-"University of Washington Seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Thomas" & DATAFILE$INST=="University of Washington"]<-"University of Washington Seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Klicka" & DATAFILE$INST=="University of Washington"]<-"University of Washington Seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Bryson" & DATAFILE$INST=="University of Washington"]<-"University of Washington Seattle"
    
    
    
    
    
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="VanDerHeijden"]<-"Switzerland"
    # levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"State University of New York College of Environmental Science and Forestry")
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="Bieber"]<-"Austria"
    DATAFILE$COUNTRY[DATAFILE$FIRST_NAME=="Jeannine" & DATAFILE$LAST_NAME=="Cavender-Bares"]<-"USA"
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="VanDerHeijden"]<-"Switzerland"
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="Gandon"]<-"France"
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="Westing"&DATAFILE$FIRST_NAME=="Arthur"]<-"Sweden"
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="Galdon"&DATAFILE$FIRST_NAME=="Luis"]<-"Spain"
    
    
    
    DATAFILE$CITY[DATAFILE$LAST_NAME=="VanDerHeijden" & DATAFILE$INST=="Amsterdam"]<-"Amsterdam"
    
  return(DATAFILE)
  
}
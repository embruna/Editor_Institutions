#FUNCTION TO CORRECT EDITOR NAMES, IDs, and INSTITUTIONS
  editor_cleaner <- function(DATAFILE) {
    
    
    # DATAFILE<-ALLDATA
    # SOME NOTES:

    DATAFILE$JOURNAL<-tolower(DATAFILE$JOURNAL)
    DATAFILE$FIRST_NAME<-tolower(DATAFILE$FIRST_NAME)
    DATAFILE$MIDDLE_NAME<-tolower(DATAFILE$MIDDLE_NAME)
    DATAFILE$LAST_NAME<-tolower(DATAFILE$LAST_NAME)

    DATAFILE$INST<-gsub("Universit\x8at","universitat", DATAFILE$INST)
    DATAFILE$INST<-gsub("L\x9fneburg","lunenburg", DATAFILE$INST)
    DATAFILE$INST[DATAFILE$INST=="\xa0Forestry and Forest Products Research Institute"]<-"forestry and forest products research institute"
    DATAFILE$INST<-gsub("G\xf6ttingen","gottingen", DATAFILE$INST)
    DATAFILE$INST[DATAFILE$INST=="Laboratoire Associe de Modelisation des Plantes\xa0(AMAP)"]<-"laboratoire associe de modelisation des plantes (amap)"
    DATAFILE$INST[DATAFILE$INST=="Universit\x84t Z\x81rich-Irchel"]<-"university of zurich irchel"
    DATAFILE$INST<-gsub("M\x82xico","mexico", DATAFILE$INST)
    DATAFILE$INST<-gsub("Universit\x82","universite", DATAFILE$INST)
    DATAFILE$INST<-gsub("Universit\xe9","universite", DATAFILE$INST)
    DATAFILE$INST<-gsub("Landscape\xa0Ecology","landscape ecology", DATAFILE$INST)
    DATAFILE$INST<-gsub("RWTH Aachen\xcaUniversity","rwth aachen university", DATAFILE$INST)
    DATAFILE$INST<-gsub("Research\xca","research", DATAFILE$INST)
    DATAFILE$INST<-gsub("Macquarie University \xca","macquarie university", DATAFILE$INST)
    DATAFILE$INST<-gsub("Universit\x8at","universitat", DATAFILE$INST)
    DATAFILE$INST<-gsub("Universit\x82","universite", DATAFILE$INST)
    DATAFILE$INST<-gsub("Aut\xfc\xbe\x8c\xa6\x84\xbcnoma","autonoma", DATAFILE$INST)
    DATAFILE$INST<-gsub("Montr\xfc\xbe\x8e\x96\x94\xbcal","montreal", DATAFILE$INST)
    DATAFILE$INST<-gsub("<a0>Universit<e9>","universite", DATAFILE$INST)
    DATAFILE$INST<-gsub("\xfc\xbe\x8c\x86\x84\xbc","", DATAFILE$INST)
    DATAFILE$INST[DATAFILE$INST=="Universit\xfc\xbe\x8c\xa3\xa0\xbc Montpellier II"]<-"universite montpellier 2"
    DATAFILE$INST[DATAFILE$INST=="Universit\xfc\xbe\x8d\x83\xa0\xbct Z\xfc\xbe\x8c\x93\xa0\xbcrich Irchel"]<-"university of zurich irchel"
    DATAFILE$INST<-gsub("Landscape<a0>Ecology","landscape ecology", DATAFILE$INST)
    DATAFILE$INST<-gsub("Universit<8a>t","universitat", DATAFILE$INST)
        DATAFILE$INST<-gsub("Leibniz Institute for Zoo and Wildlife Research\xfc\xbe\x98\x96\x8c\xbc",
                        "leibniz institute for zoo and wildlife research", DATAFILE$INST)
    DATAFILE$INST[DATAFILE$INST=="<a0>Universit<e9> Claude Bernard Lyon 1"]<-"universite claude bernard lyon 1"
    DATAFILE$INST<-tolower(DATAFILE$INST)

    DATAFILE$UNIT[DATAFILE$UNIT=="Estaci<f3>n Biol<f3>gica de Do<f1>ana"]<-"estacion biologica de donana"
    DATAFILE$UNIT[DATAFILE$UNIT=="Estaci\xf3n Biol\xf3gica de Do\xf1ana"]<-"estacion biologica de donana"
    DATAFILE$UNIT[DATAFILE$UNIT=="Institut f\x99r Biologie (II)"]<-"INSTitut fur biologie (ii)"
    DATAFILE$UNIT[DATAFILE$UNIT=="FB Biologie/Chemie/<80>kologie"]<-"fb biologie/chemiekologie"
    DATAFILE$UNIT[DATAFILE$UNIT=="FB Biologie/Chemie/\x80kologie"]<-"fb biologie/chemiekologie"
    DATAFILE$UNIT[DATAFILE$UNIT=="Departamento de Ecolog<90>a"]<-"departamento de ecologia"
    DATAFILE$UNIT[DATAFILE$UNIT=="Departamento de Ecolog\x90a"]<-"departamento de ecologia"
    DATAFILE$UNIT[DATAFILE$UNIT=="Institut f<99>r Biologie (II)"]<-"INSTitut fur biologie (ii)"
    DATAFILE$UNIT<-tolower(DATAFILE$UNIT)
    

    DATAFILE$UNIT[DATAFILE$LAST_NAME=="fiala" & DATAFILE$JOURNAL=="jte" & (DATAFILE$YEAR==2014 | DATAFILE$YEAR==2015)]<-"department of animal ecology and tropical biology (zoology iii)"
    DATAFILE$CITY[DATAFILE$LAST_NAME=="fiala" & DATAFILE$JOURNAL=="jte" & (DATAFILE$YEAR==2014 | DATAFILE$YEAR==2015)]<-NA
    DATAFILE$CITY<-gsub("Z\x81rich","zurich",DATAFILE$CITY)
    DATAFILE$CITY<-gsub("W\x9frzburg","wurzburg",DATAFILE$CITY)
    DATAFILE$CITY<-gsub("K\x9aln","cologne",DATAFILE$CITY)
    DATAFILE$CITY<-gsub("G\x9attingen","gottingen",DATAFILE$CITY)
    DATAFILE$CITY<-gsub("M\x9fnchen","munich",DATAFILE$CITY)
    DATAFILE$CITY<-tolower(DATAFILE$CITY)

    DATAFILE$STATE<-gsub("Z\x81rich","zurich",DATAFILE$STATE)
    DATAFILE$STATE<-tolower(DATAFILE$STATE)

    DATAFILE$COUNTRY<-tolower(DATAFILE$COUNTRY)


    DATAFILE$FIRST_NAME<-trimws(DATAFILE$FIRST_NAME)
    DATAFILE$LAST_NAME<-trimws(DATAFILE$LAST_NAME)
    DATAFILE$INST<-trimws(DATAFILE$INST)
    DATAFILE$UNIT<-trimws(DATAFILE$UNIT)
    # str(DATAFILE$editor_id)
    
    # DATAFILE$INST<-as.factor(DATAFILE$INST)
    # levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),
    #                            "university of washington seattle",
    #                            "dsir land resources",
    #                            "csic estacion experimental de zonas aridas",
    #                            "csic national museum of natural sciences",
    #                            "university of washington bothell",
    #                            "james cook university",
    #                            "arc centre of excellence for coral reef studies",
    #                            "university of montana",
    #                            "flathead lake biological station",
    #                            "university of notre dame",
    #                            "mount allison university",
    #                            "cinvestav irapuato",
    #                            "neri",
    #                            "memorial university of newfoundland",
    #                            "unaffiliated",
    #                            "chinese academy of sciences",
    #                            "university of massachusetts at amherst",
    #                            "university of chicago",
    #                            "university of calgary",
    #                            "university of adelaide",
    #                            "university of alberta",
    #                            "us forest service",
    #                            "university of bristol",
    #                            "university of canterbury",
    #                            "stanford university",
    #                            "nerc centre for population biology",
    #                            "cardiff university",
    #                            "university of arizona",
    #                            "university of arizona",
    #                            "university of antwerp",
    #                            "california state university long beach",
    #                            "dsir land resources",
    #                            "max planck institute for chemical ecology",
    #                            "university college dublin",
    #                            "university of sheffield",
    #                            "leibniz institute for zoo and wildlife research",
    #                            "kansas state university",
    #                            "csiro ecosystem sciences",
    #                            "cnrs centre decologie fonctionnelle et evolutive",
    #                            "forestry and forest products research institute",
    #                            "university of minnesota duluth",
    #                            "university of minnesota crookston",
    #                            "university of toronto mississauga",
    #                            "state university of new york college of environmental science and forestry",
    #                            "calyx, inc.","university of north carolina charlotte",
    #                            "smithsonian national museum of natural history",
    #                            "smithsonian national zoological park",
    #                            "australian national university",
    #                            "university of california irvine",
    #                            "university of california davis",
    #                            "university of california berkeley",
    #                            "laboratoire associe de modelisation des plantes",
    #                            "southern illinois university",
    #                            "universite libre de bruxelles",
    #                            "free university amsterdam",
    #                            "universitat osnabruck",
    #                            "southern illinois u",
    #                            "aarhus university",
    #                            "university of st. andrews",
    #                            "university of st andrews",
    #                            "university of uppsala",
    #                            "nerc centre for population biology",
    #                            "massey university",
    #                            "university of bialystok",
    #                            "manaaki whenua landcare research",
    #                            "queens university belfast",
    #                            "university of wisconsin milwaukee",
    #                            "university of copenhagen",
    #                            "no one by this name",
    #                            "university of sydney",
    #                            "university of vienna",
    #                            "fundacion cedrela",
    #                            "university of gottingen",
    #                            "smithsonian national museum of natural history",
    #                            "university of wageningen",
    #                            "university of missouri st louis",
    #                            "university of colorado boulder",
    #                            "university of chicago",
    #                            "australian national university",
    #                            "georgia institute of technology",
    #                            "mcmaster university",
    #                            "university of toronto",
    #                            "university of illinois",
    #                            "university of texas austin",
    #                            "university of maryland baltimore county",
    #                            "university of st andrews",
    #                            "university of oxford",
    #                            "university of minnesota duluth",
    #                            "indiana university",
    #                            "indiana university",
    #                            "duke university",
    #                            "stanford university",
    #                            "university of utah",
    #                            "university of minnesota",
    #                            "university of chicago",
    #                            "michigan state university",
    #                            "smithsonian national museum of natural history",
    #                            "university of california santa barbara",
    #                            "university of paris",
    #                            "norther arizona university",
    #                            "university of southampton",
    #                            "university of cape town",
    #                            "arizona state university",
    #                            "wildlife institute of india",
    #                            "ashoka trust for research in ecology and the environment",
    #                            "university of leiden",
    #                            "university of oklahoma",
    #                            "university of liverpool",
    #                            "station biologique de la tour du valat",
    #                            "unaffiliated",
    #                            "australian national university",
    #                            "universidade estadual de campinas",
    #                            "missouri botanical garden",
    #                            "university of illinois urbana champaign",
    #                            "usgs patuxent wildlife research center",
    #                            "curtin university of technology",
    #                            "smithsonian tropical research institute",
    #                            "university of british columbia",
    #                            "university of houston",
    #                            "kent state university",
    #                            "university of minnesota",
    #                            "michigan state university",
    #                            "university of southern california",
    #                            "university of queensland",
    #                            "monash university",
    #                            "university of oxford",
    #                            "university of edinburgh",
    #                            "indiana university",
    #                            "university of georgia",
    #                            "university of helsinki",
    #                            "monash university",
    #                            "university of arizona",
    #                            "university of iowa",
    #                            "unversity of michigan",
    #                            "cnrs roscoff biological station",
    #                            "university of leiden",
    #                            "university of georgia",
    #                            "iowa state university",
    #                            "university of exeter",
    #                            "university of strathclyde",
    #                            "flinders university",
    #                            "worldfish centre",
    #                            "netherlands institute of ecology",
    #                            "western ecosystem technology",
    #                            "university of guelph",
    #                            "university of aberdeen",
    #                            "tel aviv university",
    #                            "queen mary university of london",
    #                            "imperial college london",
    #                            "university of oxford",
    #                            "university of cantebury",
    #                            "soafd fisheries laboratory",
    #                            "university of glasgow ",
    #                            "australian national university",
    #                            "liverpool john moores university",
    #                            "silsoe research institute",
    #                            "purdue university",
    #                            "university of oxford",
    #                            "game conservancy",
    #                            "university of essex",
    #                            "university of reading",
    #                            "institute of terrestrial ecology",
    #                            "university of nottingham",
    #                            "central electricity research laboratories",
    #                            "imperial college london",
    #                            "central science laboratory",
    #                            "afrc institute for grassland and animal production",
    #                            "fauna and flora international",
    #                            "institute of terrestrial ecology",
    #                            "university of hong kong",
    #                            "university of washington",
    #                            "natural history museum london",
    #                            "nerc centre for ecology and hydrology",
    #                            "universidade federal de goias",
    #                            "csic - ipna",
    #                            "durham university",
    #                            "auckland university of technology",
    #                            "university of evora",
    #                            "universite de montpellier",
    #                            "university of zurich",
    #                            "university college london",
    #                            "goethe university frankfurt",
    #                            "university of gottingen",
    #                            "university of otago",
    #                            "university of california san diego",
    #                            "yale university",
    #                            "museo de la plata",
    #                            "university of umea",
    #                            "university of umea",
    #                            "chinese academy of sciences",
    #                            "queens university belfast",
    #                            "university of kwazulu natal",
    #                            "university of ft hare",
    #                            "university of athens",
    #                            "university of edinburgh",
    #                            "royal botanic gardens kew",
    #                            "royal botanic garden edinburgh",
    #                            "university of wisconsin",
    #                            "university of azores",
    #                            "national & kapodistrian university",
    #                            "university of amsterdam",
    #                            "university of hull",
    #                            "radboud university nijmegen",
    #                            "university of wales",
    #                            "cardiff university",
    #                            "university of sheffield",
    #                            "cambridge university",
    #                            "swansea university",
    #                            "oxford brookes university",
    #                            "university of essex",
    #                            "radboud university nijmegen",
    #                            "leiden university",
    #                            "wageningen university and research",
    #                            "university college dublin",
    #                            "zoological society of london",
    #                            "lancaster university",
    #                            "university of sydney",
    #                            "vrije universiteit amsterdam",
    #                            "university of toronto",
    #                            "wesleyan university",
    #                            "university of wales",
    #                            "western sydney university",
    #                            "university of calgary",
    #                            "lancaster university",
    #                            "university of indiana",
    #                            "swiss federal institute of technology",
    #                            "university of umea",
    #                            "simon fraser university",
    #                            "university of vienna",
    #                            "university of texas arlington",
    #                            "university of minnesota",
    #                            "smithsonian tropical research institute",
    #                            "university of michigan",
    #                            "royal holloway university of london",
    #                            "university of basel",
    #                            "university of turku ",
    #                            "florida international university",
    #                            "cnrs institut ecologie et environnement",
    #                            "university of florida",
    #                            "george washington university",
    #                            "university of bielefeld",
    #                            "estonian university of life sciences",
    #                            "universite de rennes 1",
    #                            "university of vienna",
    #                            "university of california san diego",
    #                            "rice university",
    #                            "utah state university",
    #                            "university of queensland",
    #                            "university of kansas",
    #                            "university of innsbruck ",
    #                            "technical university of munich",
    #                            "queens university",
    #                            "lund university",
    #                            "geological survey of denmark",
    #                            "swedish university of agricultural sciences",
    #                            "university of copenhagen",
    #                            "oxford forestry institute",
    #                            "agricultural university of norway",
    #                            "norwegian university of science and technology",
    #                            "university of helsinki",
    #                            "swiss federal institute of aquatic science and technology",
    #                            "university of california santa cruz",
    #                            "british trust for ornithology",
    #                            "evergreen state college",
    #                            "university of freiburg",
    #                            "swedish university of agricultural sciences",
    #                            "university of hong kong",
    #                            "mediterranean institute for advanced studies",
    #                            "technical university of berlin",
    #                            "university of virginia",
    #                            "university of aberdeen",
    #                            "university of cambridge",
    #                            "university of haifa-oranim",
    #                            "university of eastern finland",
    #                            "vrije universiteit amsterdam",
    #                            "university of north carolina chapel hill",
    #                            "university college dublin"
    #                            )
    # #### 
    # These corrections are from PJ review of files
    #####
    
    
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="cymerman"& DATAFILE$JOURNAL=="leco"]<-"hepinstall-cymerman"
    
    
    DATAFILE$INST[DATAFILE$LAST_NAME=="voigt"& DATAFILE$JOURNAL=="oecol" &
                    (DATAFILE$YEAR==2013|DATAFILE$YEAR==2014)]<-"leibniz institute for zoo and wildlife research"
    
    DATAFILE$INST[DATAFILE$LAST_NAME=="rees"& DATAFILE$JOURNAL=="jecol" &
                    DATAFILE$YEAR==2012]<-"university of sheffield"
    
    DATAFILE$INST[DATAFILE$LAST_NAME=="holzner"& DATAFILE$JOURNAL=="plantecol" &
                    (DATAFILE$YEAR>1984 & DATAFILE$YEAR<1988)]<-"university of vienna"
    
    DATAFILE$INST_CHECK[DATAFILE$LAST_NAME=="holzner"& DATAFILE$JOURNAL=="plantecol" &
                    (DATAFILE$YEAR>1984 & DATAFILE$YEAR<1988)]<-"2x INST"
    
    DATAFILE$CITY[DATAFILE$LAST_NAME=="korner"& DATAFILE$JOURNAL=="oecol" &
                          (DATAFILE$YEAR>1989 & DATAFILE$YEAR<2015)]<-"basel"
    
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="korner"& DATAFILE$JOURNAL=="oecol" &
                    (DATAFILE$YEAR>1989 & DATAFILE$YEAR<2015)]<-"switzerland"
    
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="bever" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="oecol"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="lichstein" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="oecol"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="gough" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="oecol"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="heimpel" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="oecol"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="ibanez" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="oecol"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="layman" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="oecol"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="le galliard" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="oecol"),]

    
    
    
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="masters" & DATAFILE$YEAR==2010 & DATAFILE$JOURNAL=="jbiog"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="holland" & DATAFILE$YEAR==1985 & DATAFILE$JOURNAL=="jbiog"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="lambers" & DATAFILE$YEAR==1993 & DATAFILE$JOURNAL=="plantecol"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="bond" & DATAFILE$YEAR==2004 & DATAFILE$JOURNAL=="plantecol"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="kollmann" & DATAFILE$YEAR==2007 & DATAFILE$JOURNAL=="plantecol"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="mueller" & DATAFILE$YEAR==2011 & DATAFILE$JOURNAL=="oecol"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="wieser" & DATAFILE$editor_id=="162" & DATAFILE$JOURNAL=="oecol"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="andelman" & DATAFILE$editor_id=="3270" & DATAFILE$JOURNAL=="biocon"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="holland" & DATAFILE$FIRST_NAME=="j." & DATAFILE$JOURNAL=="amnat"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="uy" & DATAFILE$FIRST_NAME=="j." & DATAFILE$JOURNAL=="amnat"),]
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="dustin" & DATAFILE$FIRST_NAME=="marshall" & DATAFILE$JOURNAL=="evol"),]
    
    
    DATAFILE$CITY[DATAFILE$LAST_NAME=="overton"& DATAFILE$JOURNAL=="leco"]<-NA
    
    DATAFILE$geo.code[DATAFILE$LAST_NAME=="betts"& DATAFILE$JOURNAL=="leco"]<-"can"
    
    DATAFILE$LAST_NAME[DATAFILE$FIRST_NAME=="kimberly" & DATAFILE$JOURNAL=="leco" & DATAFILE$YEAR==2004]<-"with"
    
    DATAFILE$MIDDLE_NAME[DATAFILE$FIRST_NAME=="kimberly" & DATAFILE$JOURNAL=="leco" & DATAFILE$YEAR==2004]<-"a"
    
    DATAFILE$editor_id[DATAFILE$FIRST_NAME=="kimberly" & DATAFILE$JOURNAL=="leco" & DATAFILE$YEAR==2004]<-"2113"
    
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="lookinbill" & DATAFILE$JOURNAL=="leco"]<-"lookingbill"

    
    
    DATAFILE<-DATAFILE[!(DATAFILE$LAST_NAME=="duggan" & DATAFILE$YEAR==2008),] #not in the front matter
    
    
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="van de pol" & DATAFILE$FIRST_NAME=="martin"]<-"vanderpol"
    
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="muller" & DATAFILE$editor_id=="534"]<-"mueller"
    
    DATAFILE$FIRST_NAME[DATAFILE$LAST_NAME=="ashton" & DATAFILE$FIRST_NAME=="p" & DATAFILE$MIDDLE_NAME=="ms"]<-"mark"
    DATAFILE$MIDDLE_NAME[DATAFILE$LAST_NAME=="ashton" & DATAFILE$FIRST_NAME=="mark"]<-"s"
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="ashton" & DATAFILE$FIRST_NAME=="mark"]<-"2278"
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="baudry" & DATAFILE$JOURNAL=="jape"]<-"1456"
    
    
    DATAFILE$FIRST_NAME[DATAFILE$LAST_NAME=="dyer" & DATAFILE$FIRST_NAME=="andy" & 
                          DATAFILE$JOURNAL=="jecol"]<-"andrew"

    DATAFILE$FIRST_NAME[DATAFILE$LAST_NAME=="franklin" & DATAFILE$FIRST_NAME=="j" &
                         DATAFILE$JOURNAL=="leco"]<-"janet"
    
  
    DATAFILE$FIRST_NAME[DATAFILE$LAST_NAME=="bonduriansky" & DATAFILE$FIRST_NAME=="russel"]<-"russell"
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="bonduriansky" & DATAFILE$FIRST_NAME=="russell"]<-"2989"
    DATAFILE$FIRST_NAME[DATAFILE$LAST_NAME=="chen" & DATAFILE$FIRST_NAME=="j" & DATAFILE$INST=="university of toledo"]<-"jiquan"
    DATAFILE$FIRST_NAME[DATAFILE$LAST_NAME=="chen" & DATAFILE$FIRST_NAME=="j" & DATAFILE$INST=="michigan state university"]<-"jiquan"
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="chen" & DATAFILE$FIRST_NAME=="jiquan"]<-"3885"
    
    
    
    
    DATAFILE$FIRST_NAME[DATAFILE$LAST_NAME=="gaillard" & DATAFILE$FIRST_NAME=="jean-michelle"]<-"jean-michel"
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="gaillard" & DATAFILE$FIRST_NAME=="jean-michel"
                       & DATAFILE$JOURNAL=="amnat"]<-"1589"
    
    
    
    
    
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="vander" & DATAFILE$JOURNAL=="leco"]<-"vandermaarel"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="moyle" & DATAFILE$FIRST_NAME=="peter"]<-"moyle"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="li" & DATAFILE$FIRST_NAME=="feng-min"]<-"li"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="dewalt" & DATAFILE$FIRST_NAME=="saara"]<-"dewalt"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="devictor" & DATAFILE$FIRST_NAME=="vincent"]<-"devictor"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="coe" & DATAFILE$FIRST_NAME=="m"]<-"coe"
    DATAFILE$LAST_NAME[DATAFILE$LAST_NAME=="monod"]<-"monod"
    
    DATAFILE$FIRST_NAME[DATAFILE$LAST_NAME=="moyle" & DATAFILE$FIRST_NAME=="peter"]<-"peter"
    DATAFILE$FIRST_NAME[DATAFILE$LAST_NAME=="monod"]<-"theodore"
    DATAFILE$UNIT[DATAFILE$LAST_NAME=="meffe" & DATAFILE$JOURNAL=="conbio" & DATAFILE$INST=="university of montana"]<-NA
    DATAFILE$UNIT[DATAFILE$LAST_NAME=="whitmore"]<-"oxford forestry institute"
    DATAFILE$UNIT[DATAFILE$LAST_NAME=="gustafson"& DATAFILE$FIRST_NAME=="e"]<-"north central research station"
    DATAFILE$UNIT[DATAFILE$LAST_NAME=="with"& DATAFILE$FIRST_NAME=="kimberly" & DATAFILE$YEAR==2004]<-"division of biology"
    DATAFILE$INST[DATAFILE$LAST_NAME=="with"& DATAFILE$FIRST_NAME=="kimberly" & DATAFILE$YEAR==2004]<-"kansas STATE university"
    DATAFILE$STATE[DATAFILE$LAST_NAME=="with"& DATAFILE$FIRST_NAME=="kimberly" & DATAFILE$YEAR==2004]<-"ks"
    DATAFILE$CITY[DATAFILE$LAST_NAME=="with"& DATAFILE$FIRST_NAME=="kimberly" & DATAFILE$YEAR==2004]<-"manhattan"
    
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="vandonk" & DATAFILE$JOURNAL=="ecology"]<-"1037"
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="vandermaarel" & DATAFILE$JOURNAL=="leco"]<-"1033"
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="smith" & DATAFILE$FIRST_NAME=="melinda"]<-"2566"
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="smith" & DATAFILE$FIRST_NAME=="danny"]<-NA
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="moyle" & DATAFILE$FIRST_NAME=="peter"]<-2874
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="li" & DATAFILE$FIRST_NAME=="feng-min"]<-1098
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="lee" & DATAFILE$FIRST_NAME=="carol"]<-3860
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="fry" & DATAFILE$FIRST_NAME=="james"]<-3891
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="dewalt" & DATAFILE$FIRST_NAME=="saara"]<-3311
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="devictor" & DATAFILE$FIRST_NAME=="vincent"]<-3681
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="coe" & DATAFILE$FIRST_NAME=="m"]<-2332
    DATAFILE$editor_id[DATAFILE$LAST_NAME=="krams" & DATAFILE$FIRST_NAME=="indrikis"]<-70
    
    
    
    
    
    ##########
    DATAFILE$INST[DATAFILE$LAST_NAME=="harrison" & DATAFILE$FIRST_NAME=="maria" &
                    DATAFILE$JOURNAL=="newphyte"& DATAFILE$YEAR==2004]<-"Boyce Thompson Institute"
    
    
    DATAFILE$FIRST_NAME[DATAFILE$LAST_NAME=="lawes" & DATAFILE$FIRST_NAME=="mike" &
                          DATAFILE$JOURNAL=="plantecol"]<-"michael"
    
    
    
    DATAFILE$INST[DATAFILE$LAST_NAME=="pearman" &
                    DATAFILE$FIRST_NAME=="peter" &
                    DATAFILE$JOURNAL=="wsl swiss federal research institute"]<-
      "wsl swiss federal institute for forest snow and landscape research birmensdorf"
    
    
    DATAFILE$INST[DATAFILE$LAST_NAME=="williams" & DATAFILE$FIRST_NAME=="d" &
                    DATAFILE$MIDDLE_NAME=="a" & DATAFILE$JOURNAL=="jecol"]<-"university of aberdeen"
    
    DATAFILE$INST[DATAFILE$LAST_NAME=="james" & DATAFILE$FIRST_NAME=="helen" & DATAFILE$JOURNAL=="auk"]<-"smithsonian national museum of natural history"
    DATAFILE$INST[DATAFILE$LAST_NAME=="lonsdale" & DATAFILE$JOURNAL=="jape" & DATAFILE$FIRST_NAME=="w"& (DATAFILE$YEAR>1998| DATAFILE$YEAR<2003)]<-"csiro ecosystem sciences"
    DATAFILE$INST[DATAFILE$LAST_NAME=="heil" & DATAFILE$JOURNAL=="jecol" & DATAFILE$FIRST_NAME=="martin" & DATAFILE$INST=="max planck university"]<-"max planck institute for chemical ecology"
    DATAFILE$INST[DATAFILE$LAST_NAME=="heil" & DATAFILE$JOURNAL=="jecol" & DATAFILE$FIRST_NAME=="martin" & DATAFILE$YEAR>2009]<-"cinvestav irapuato"
    DATAFILE$INST[DATAFILE$LAST_NAME=="mcglone" & DATAFILE$JOURNAL=="jbiog" & DATAFILE$INST=="christchurch"]<-"dsir land resources"
    DATAFILE$INST[DATAFILE$LAST_NAME=="williams" & DATAFILE$JOURNAL=="funecol" & DATAFILE$FIRST_NAME=="caroline"& (DATAFILE$YEAR>2008| DATAFILE$YEAR<2012)]<-"university of california berkeley"
    DATAFILE$INST[DATAFILE$LAST_NAME=="wainwright" & DATAFILE$JOURNAL=="funecol" & DATAFILE$FIRST_NAME=="peter" & (DATAFILE$YEAR>2008| DATAFILE$YEAR<2012)]<-"university of california davis"
    DATAFILE$INST[DATAFILE$LAST_NAME=="treseder" & DATAFILE$JOURNAL=="funecol" & DATAFILE$FIRST_NAME=="kathleen"& (DATAFILE$YEAR>2008| DATAFILE$YEAR<2016)]<-"university of california irvine"
    DATAFILE$INST[DATAFILE$LAST_NAME=="carroll" & DATAFILE$JOURNAL=="funecol" & DATAFILE$FIRST_NAME=="scott"& (DATAFILE$YEAR>2006| DATAFILE$YEAR<2016)]<-"university of california davis"
    DATAFILE$INST[DATAFILE$LAST_NAME=="campbell" & DATAFILE$JOURNAL=="funecol" & DATAFILE$FIRST_NAME=="diane"]<-"university of california irvine"
    DATAFILE$INST[DATAFILE$LAST_NAME=="farquhar" & DATAFILE$JOURNAL=="funecol" & DATAFILE$FIRST_NAME=="graham"]<-"australian national university"
    DATAFILE$INST[DATAFILE$LAST_NAME=="fernandez-juricic" & DATAFILE$JOURNAL=="jape" & DATAFILE$YEAR==2008]<-"california STATE university long beach"
    DATAFILE$INST[DATAFILE$LAST_NAME=="zuidema" & DATAFILE$JOURNAL=="jecol" & DATAFILE$YEAR==2007]<-"university of wageningen"
    DATAFILE$INST[DATAFILE$LAST_NAME=="vanderputten" & DATAFILE$JOURNAL=="oikos"]<-"university of wageningen"
    DATAFILE$INST[DATAFILE$LAST_NAME=="lieth" & DATAFILE$FIRST_NAME=="helmut" & DATAFILE$JOURNAL=="jbiog"]<-"universitat osnabruck"
    DATAFILE$INST[DATAFILE$LAST_NAME=="polle" & DATAFILE$FIRST_NAME=="andrea" & DATAFILE$JOURNAL=="newphyte"]<-"university of gottingen"
    DATAFILE$INST[DATAFILE$LAST_NAME=="wardle" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="plantecol"]<-"university of sydney"
    DATAFILE$INST[DATAFILE$LAST_NAME=="fenchel" & DATAFILE$FIRST_NAME=="tom" & DATAFILE$JOURNAL=="funecol"]<-"university of copenhagen"
    DATAFILE$INST[DATAFILE$LAST_NAME=="white" & DATAFILE$JOURNAL=="jecol" & DATAFILE$YEAR==1992]<-"university college dublin"
    DATAFILE$INST[DATAFILE$LAST_NAME=="desteven" & DATAFILE$JOURNAL=="ecology" & DATAFILE$YEAR==1999]<-"university of wisconsin milwaukee"
    DATAFILE$INST[DATAFILE$LAST_NAME=="vandermaarel" & DATAFILE$JOURNAL=="leco"]<-"university of uppsala"
    DATAFILE$INST[DATAFILE$LAST_NAME=="sprules" & DATAFILE$FIRST_NAME=="gary"]<-"university of toronto mississauga"
    DATAFILE$INST[DATAFILE$LAST_NAME=="wagner" & DATAFILE$FIRST_NAME=="helene"]<-"university of toronto mississauga"
    DATAFILE$INST[DATAFILE$LAST_NAME=="kotanen" & DATAFILE$FIRST_NAME=="peter"]<-"university of toronto mississauga"
    DATAFILE$INST[DATAFILE$LAST_NAME=="loiselle" & DATAFILE$FIRST_NAME=="bette"]<-"university of missouri st louis"
    DATAFILE$INST[DATAFILE$LAST_NAME=="ricklefs" & DATAFILE$FIRST_NAME=="robert"]<-"university of missouri st louis"
    DATAFILE$INST[DATAFILE$LAST_NAME=="renner" & DATAFILE$FIRST_NAME=="susanne"]<-"university of missouri st louis"
    DATAFILE$INST[DATAFILE$LAST_NAME=="sork" & DATAFILE$FIRST_NAME=="victoria"]<-"university of missouri st louis"
    # DATAFILE$INST[DATAFILE$LAST_NAME=="parmentier"]<-"universite libre de bruxelles"
    DATAFILE$INST[DATAFILE$LAST_NAME=="debussche"]<-"cnrs centre decologie fonctionnelle et evolutive"
    # levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"forestry and forest products research institute","university of minnesota duluth","university of minnesota crookston")
    DATAFILE$INST[DATAFILE$LAST_NAME=="fujimori"]<-"forestry and forest products research institute"
    DATAFILE$INST[DATAFILE$LAST_NAME=="johnson" & DATAFILE$FIRST_NAME=="lucinda"]<-"university of minnesota duluth"
    DATAFILE$INST[DATAFILE$LAST_NAME=="parmentier" & DATAFILE$JOURNAL=="bitr"]<-"universite libre de bruxelles"
    DATAFILE$INST[DATAFILE$LAST_NAME=="moen" & DATAFILE$FIRST_NAME=="ron"]<-"university of minnesota duluth"
    DATAFILE$INST[DATAFILE$LAST_NAME=="sterner" & DATAFILE$FIRST_NAME=="robert"]<-"university of minnesota duluth"
    DATAFILE$INST[DATAFILE$LAST_NAME=="vanderheijden" & DATAFILE$FIRST_NAME=="marcel"]<-"free university amsterdam"
    DATAFILE$INST[DATAFILE$LAST_NAME=="vandamme" & DATAFILE$FIRST_NAME=="raoul"]<-"university of antwerp"
    DATAFILE$INST[DATAFILE$LAST_NAME=="badyaev" & DATAFILE$FIRST_NAME=="alexander"]<-"university of arizona"
    DATAFILE$INST[DATAFILE$LAST_NAME=="davidowitz" & DATAFILE$FIRST_NAME=="goggy"]<-"university of arizona"
    DATAFILE$INST[DATAFILE$LAST_NAME=="mcgraw" & DATAFILE$FIRST_NAME=="kevin"]<-"arizona STATE university"
    DATAFILE$INST[DATAFILE$LAST_NAME=="cotter" & DATAFILE$FIRST_NAME=="sheena"]<-"queens university belfast"
    DATAFILE$INST[DATAFILE$LAST_NAME=="norden" & DATAFILE$JOURNAL=="funecol"]<-"fundacion cedrela"
    DATAFILE$INST[DATAFILE$LAST_NAME=="jones" & DATAFILE$INST=="ascot"]<-"nerc centre for population biology"
    DATAFILE$INST[DATAFILE$LAST_NAME=="jones" & DATAFILE$INST=="cardiff"]<-"cardiff university"
    DATAFILE$INST[DATAFILE$LAST_NAME=="raubenheimer" & DATAFILE$FIRST_NAME=="david"]<-"massey university"
    DATAFILE$INST[DATAFILE$LAST_NAME=="niu" & DATAFILE$FIRST_NAME=="shuli"]<-"chinese academy of sciences"
    DATAFILE$INST[DATAFILE$LAST_NAME=="konarzewski" & DATAFILE$INST=="bialystok"]<-"university of bialystok"
    DATAFILE$INST[DATAFILE$LAST_NAME=="newman" & DATAFILE$INST=="bristol"]<-"university of bristol"
    DATAFILE$INST[DATAFILE$LAST_NAME=="bergelson" & DATAFILE$INST=="chicago"]<-"university of chicago"
    DATAFILE$INST[DATAFILE$LAST_NAME=="turnbull" & DATAFILE$INST=="christchurch"]<-"university of canterbury"
    DATAFILE$INST[DATAFILE$LAST_NAME=="mcglone" & DATAFILE$INST=="christchurch"]<-"manaaki whenua landcare research"
    DATAFILE$INST[DATAFILE$LAST_NAME=="boggs" & DATAFILE$INST=="colorado"]<-"stanford university"
    DATAFILE$INST[DATAFILE$LAST_NAME=="wiersma" & DATAFILE$FIRST_NAME=="jochum"]<-"university of minnesota crookston"
    DATAFILE$INST[DATAFILE$LAST_NAME=="smith" & DATAFILE$FIRST_NAME=="madeleine"]<-"university of minnesota crookston"
    DATAFILE$INST[DATAFILE$LAST_NAME=="sims" & DATAFILE$FIRST_NAME=="albert"]<-"university of minnesota crookston"
    DATAFILE$INST[DATAFILE$JOURNAL=="amnat" & DATAFILE$LAST_NAME=="case"]<-"university of california san diego"
    # DATAFILE$INST[DATAFILE$LAST_NAME=="noon"]<-"colorado STATE university"
    DATAFILE$INST[DATAFILE$LAST_NAME=="burgess"]<-"STATE university of new york college of environmental science and forestry"
    DATAFILE$INST[DATAFILE$LAST_NAME=="fragoso"]<-"STATE university of new york college of environmental science and forestry"
    DATAFILE$INST[DATAFILE$LAST_NAME=="yanai"]<-"STATE university of new york college of environmental science and forestry"
    DATAFILE$INST[DATAFILE$LAST_NAME=="hall" & DATAFILE$FIRST_NAME=="charles" & DATAFILE$JOURNAL=="conbio"]<-
      "STATE university of new york college of environmental science and forestry"
    DATAFILE$INST[DATAFILE$LAST_NAME=="pressey" & DATAFILE$YEAR==2006 & DATAFILE$JOURNAL=="conbio"]<-
      "unaffiliated"
    DATAFILE$INST[DATAFILE$LAST_NAME=="damgaard" & (DATAFILE$YEAR>2005 & DATAFILE$YEAR<2010) & DATAFILE$JOURNAL=="funecol"]<-
      "neri"
    DATAFILE$INST[DATAFILE$LAST_NAME=="leroux" & DATAFILE$YEAR==2015 & DATAFILE$JOURNAL=="funecol"]<-
      "memorial university of newfoundland"
    DATAFILE$INST[DATAFILE$LAST_NAME=="noon" & DATAFILE$YEAR==2012 & DATAFILE$JOURNAL=="conbio"]<-
      "university of colorado boulder"
    DATAFILE$INST[DATAFILE$LAST_NAME=="noon" & DATAFILE$YEAR==2014 & DATAFILE$JOURNAL=="conbio"]<-
      "university of colorado boulder"
    DATAFILE$INST[DATAFILE$INST=="university of missouri"]<-"university of missouri columbia" 
    DATAFILE$INST[DATAFILE$JOURNAL=="amnat" & DATAFILE$LAST_NAME=="case"]<-"university of california san diego"
    # DATAFILE$INST[DATAFILE$LAST_NAME=="noon"]<-"colorado STATE university"
    DATAFILE$INST[DATAFILE$LAST_NAME=="bowers" & DATAFILE$CITY=="vadnais heights"]<-"calyx, inc."
    DATAFILE$INST[DATAFILE$LAST_NAME=="debussche"]<-"cnrs centre decologie fonctionnelle et evolutive"
    DATAFILE$INST[DATAFILE$LAST_NAME=="whitmore"]<-"university of oxford"
    DATAFILE$INST[DATAFILE$LAST_NAME=="hails"]<-"university of oxford"
    DATAFILE$INST[DATAFILE$LAST_NAME=="leamy"]<-"university of north carolina charlotte"
    DATAFILE$INST[DATAFILE$LAST_NAME=="gustafson"& DATAFILE$FIRST_NAME=="e"]<-"us forest service"
    DATAFILE$INST[DATAFILE$LAST_NAME=="labandeira"& DATAFILE$FIRST_NAME=="conrad"]<-"smithsonian national museum of natural history"
    DATAFILE$INST[DATAFILE$LAST_NAME=="seidenstzicker"& DATAFILE$FIRST_NAME=="john"]<-"smithsonian national zoological park"
    DATAFILE$INST[DATAFILE$LAST_NAME=="kleiman"& DATAFILE$FIRST_NAME=="devra"]<-"smithsonian national zoological park"
    DATAFILE$INST[DATAFILE$LAST_NAME=="houllier" & (DATAFILE$YEAR>1998 & DATAFILE$YEAR<2003)]<-NA
    DATAFILE$INST[DATAFILE$LAST_NAME=="houllier" & (DATAFILE$YEAR>1998 & DATAFILE$YEAR<2003)]<-"laboratoire associe de modelisation des plantes"
    DATAFILE$INST[DATAFILE$LAST_NAME=="watling"& DATAFILE$INST=="adelaide"]<-"university of adelaide"
    DATAFILE$INST[DATAFILE$LAST_NAME=="boutin" & DATAFILE$INST=="alberta"]<-"university of alberta"
    DATAFILE$INST[DATAFILE$LAST_NAME=="fox" & DATAFILE$INST=="alberta"]<-"university of calgary"
    DATAFILE$INST[DATAFILE$LAST_NAME=="patek" & DATAFILE$INST=="amherst"]<-"university of massachusetts at amherst"
    DATAFILE$INST[DATAFILE$LAST_NAME=="mcgraw" & DATAFILE$INST=="arizona"]<-"arizona STATE university"
    DATAFILE$INST[DATAFILE$LAST_NAME=="marzluff" & DATAFILE$INST=="university of washington"]<-"university of washington seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="felsenstein" & DATAFILE$INST=="university of washington"]<-"university of washington seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="tewksbury" & DATAFILE$INST=="university of washington"]<-"university of washington seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="kareiva" & DATAFILE$INST=="university of washington"]<-"university of washington seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="williams-guillen" & DATAFILE$INST=="university of washington"]<-"university of washington bothell"
    DATAFILE$INST[DATAFILE$LAST_NAME=="slatkin" & DATAFILE$INST=="university of washington"]<-"university of washington seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="huey" & DATAFILE$INST=="university of washington"]<-"university of washington seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="olmstead" & DATAFILE$INST=="university of washington"]<-"university of washington seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="rohwer" & DATAFILE$INST=="university of washington"]<-"university of washington seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="brubaker" & DATAFILE$INST=="university of washington"]<-"university of washington seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="naiman" & DATAFILE$INST=="university of washington"]<-"university of washington seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="schindler" & DATAFILE$INST=="university of washington"]<-"university of washington seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="wingfield" & DATAFILE$INST=="university of washington"]<-"university of washington seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="thomas" & DATAFILE$INST=="university of washington"]<-"university of washington seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="klicka" & DATAFILE$INST=="university of washington"]<-"university of washington seattle"
    DATAFILE$INST[DATAFILE$LAST_NAME=="bryson" & DATAFILE$INST=="university of washington"]<-"university of washington seattle"
    DATAFILE$INST[DATAFILE$editor_id==1578 & DATAFILE$INST=="university of new york"]<-"suny college of environmental science and forestry"
    # I used "castle" for lovejoy becasue as a vice-president he is not in a unit the way the others were
    DATAFILE$INST[DATAFILE$LAST_NAME=="lovejoy" & DATAFILE$INST=="smithsonian institution"]<-"smithsonian institution-castle"
    
    
    
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="vanderheijden"]<-"switzerland"
    # levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"STATE university of new york college of environmental science and forestry")
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="bieber"]<-"austria"
    DATAFILE$COUNTRY[DATAFILE$FIRST_NAME=="jeannine" & DATAFILE$LAST_NAME=="cavender-bares"]<-"usa"
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="vanderheijden"]<-"switzerland"
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="gandon"]<-"france"
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="westing"&DATAFILE$FIRST_NAME=="arthur"]<-"sweden"
    DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="galdon"&DATAFILE$FIRST_NAME=="luis"]<-"spain"
    
    
    
    DATAFILE$CITY[DATAFILE$LAST_NAME=="vanderheijden" & DATAFILE$INST=="amsterdam"]<-"amsterdam"
    
  return(DATAFILE)
  
}
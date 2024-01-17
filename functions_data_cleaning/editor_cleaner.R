# FUNCTION TO CORRECT EDITOR NAMES, IDs, and instITUTIONS
editor_cleaner <- function(datafile) {
  # datafile<-alldata


  datafile$journal <- tolower(datafile$journal)
  datafile$first_name <- tolower(datafile$first_name)
  datafile$middle_name <- tolower(datafile$middle_name)
  datafile$last_name <- tolower(datafile$last_name)

  
  # 
  # summary(as.factor(Encoding("\xca")))
  # which(as.factor(Encoding(datafile$inst))=="UTF-8")
  # stringi::stri_enc_list()
  # datafile$inst[1365]
  # stringi::stri_enc_toascii(datafile$inst[12376])
  # foo<-str_conv(datafile$inst[12376], "ASCII")
  # 
  # 
  # 
  # foo<-iconv(c(datafile$inst),to="ASCII//TRANSLIT")  
  # foo <- gsub("\\'", '', foo)
  # foo<-as_tibble(unique(foo))
  # str(foo)
  # summary(as.factor(Encoding(foo$value)))

  
  
  datafile <- datafile %>% 
    mutate(inst2=(iconv(c(datafile$inst),to="ASCII//TRANSLIT")))  %>% 
    mutate(inst2=gsub("\\'", '', inst2)) %>% 
    mutate(inst_check=(inst==inst2)) %>% 
    relocate(c(inst2,inst_check),.after="inst")
  
  # 
  # 
  # datafile$inst[21269]
  # 
    
  
  # foo<-which(as_tibble(datafile$inst==datafile$inst2)==FALSE)
  
  # foo<-as_tibble(stringi::stri_enc_toascii(datafile$inst))
  # summary(as.factor(datafile$inst))
  # summary(as.factor(foo$value))
  # 
  datafile$inst <- gsub("Universit\x8at", "universitat", datafile$inst,useBytes = TRUE)
  datafile$inst <- gsub("L\x9fneburg", "lunenburg", datafile$inst,useBytes = TRUE)
  datafile$inst[datafile$inst == "\xa0Forestry and Forest Products Research Institute"] <- "forestry and forest products research institute"
  datafile$inst <- gsub("G\xf6ttingen", "gottingen", datafile$inst,useBytes = TRUE)
  datafile$inst[datafile$inst == "Laboratoire Associe de Modelisation des Plantes\xa0(AMAP)"] <- "laboratoire associe de modelisation des plantes (amap)"
  datafile$inst[datafile$inst == "Universit\x84t Z\x81rich-Irchel"] <- "university of zurich irchel"
  datafile$inst <- gsub("M\x82xico", "mexico", datafile$inst,useBytes = TRUE)
  datafile$inst <- gsub("Universit\x82", "universite", datafile$inst,useBytes = TRUE)
  datafile$inst <- gsub("Universit\xe9", "universite", datafile$inst,useBytes = TRUE)
  datafile$inst <- gsub("Landscape\xa0Ecology", "landscape ecology", datafile$inst,useBytes = TRUE)
  datafile$inst <- gsub("RWTH Aachen\xcaUniversity", "rwth aachen university", datafile$inst,useBytes = TRUE)
  datafile$inst <- gsub("Research\xca", "research", datafile$inst,useBytes = TRUE)
  datafile$inst <- gsub("Macquarie University \xca", "macquarie university", datafile$inst,useBytes = TRUE)
  datafile$inst <- gsub("Universit\x8at", "universitat", datafile$inst,useBytes = TRUE)
  datafile$inst <- gsub("Universit\x82", "universite", datafile$inst,useBytes = TRUE)
  datafile$inst <- gsub("Aut\xfc\xbe\x8c\xa6\x84\xbcnoma", "autonoma", datafile$inst,useBytes = TRUE)
  datafile$inst <- gsub("Montr\xfc\xbe\x8e\x96\x94\xbcal", "montreal", datafile$inst,useBytes = TRUE)
  datafile$inst <- gsub("<a0>Universit<e9>", "universite", datafile$inst,useBytes = TRUE)
  datafile$inst <- gsub("\xfc\xbe\x8c\x86\x84\xbc", "", datafile$inst,useBytes = TRUE)
  datafile$inst[datafile$inst == "Universit\xfc\xbe\x8c\xa3\xa0\xbc Montpellier II"] <- "universite montpellier 2"
  datafile$inst[datafile$inst == "Universit\xfc\xbe\x8d\x83\xa0\xbct Z\xfc\xbe\x8c\x93\xa0\xbcrich Irchel"] <- "university of zurich irchel"
  datafile$inst <- gsub("Landscape<a0>Ecology", "landscape ecology", datafile$inst,useBytes = TRUE)
  datafile$inst <- gsub("Universit<8a>t", "universitat", datafile$inst,useBytes = TRUE)
  datafile$inst <- gsub(
    "Leibniz Institute for Zoo and Wildlife Research\xfc\xbe\x98\x96\x8c\xbc",
    "leibniz institute for zoo and wildlife research", datafile$inst
  ,useBytes = TRUE)
  datafile$inst[datafile$inst == "<a0>Universit<e9> Claude Bernard Lyon 1"] <- "universite claude bernard lyon 1"
  datafile$inst <- tolower(datafile$inst)

  datafile$unit[datafile$unit == "Estaci<f3>n Biol<f3>gica de Do<f1>ana"] <- "estacion biologica de donana"
  datafile$unit[datafile$unit == "Estaci\xf3n Biol\xf3gica de Do\xf1ana"] <- "estacion biologica de donana"
  datafile$unit[datafile$unit == "Institut f\x99r Biologie (II)"] <- "institut fur biologie (ii)"
  datafile$unit[datafile$unit == "FB Biologie/Chemie/<80>kologie"] <- "fb biologie/chemiekologie"
  datafile$unit[datafile$unit == "FB Biologie/Chemie/\x80kologie"] <- "fb biologie/chemiekologie"
  datafile$unit[datafile$unit == "Departamento de Ecolog<90>a"] <- "departamento de ecologia"
  datafile$unit[datafile$unit == "Departamento de Ecolog\x90a"] <- "departamento de ecologia"
  datafile$unit[datafile$unit == "Institut f<99>r Biologie (II)"] <- "institut fur biologie (ii)"
  datafile$unit <- tolower(datafile$unit)

  datafile$city <- gsub("Z\x81rich", "zurich", datafile$city,useBytes = TRUE)
  datafile$city <- gsub("W\x9frzburg", "wurzburg", datafile$city,useBytes = TRUE)
  datafile$city <- gsub("K\x9aln", "cologne", datafile$city,useBytes = TRUE)
  datafile$city <- gsub("G\x9attingen", "gottingen", datafile$city,useBytes = TRUE)
  datafile$city <- gsub("M\x9fnchen", "munich", datafile$city,useBytes = TRUE)
  datafile$city <- tolower(datafile$city)
  
  
  datafile$state <- gsub("Z\x81rich", "zurich", datafile$state,useBytes = TRUE)
  datafile$state <- tolower(datafile$state)
  
  datafile$country <- tolower(datafile$country)
  
  
  
  datafile$unit[datafile$last_name == "fiala" &
    datafile$journal == "jte" &
    (datafile$year == 2014 | datafile$year == 2015)] <- "department of animal ecology and tropical biology (zoology iii)"
  datafile$city[datafile$last_name == "fiala" & datafile$journal == "jte" &
    (datafile$year == 2014 | datafile$year == 2015)] <- NA
  
  
  



  datafile$first_name <- trimws(datafile$first_name)
  datafile$last_name <- trimws(datafile$last_name)
  datafile$inst <- trimws(datafile$inst)
  datafile$unit <- trimws(datafile$unit)

  # ####
  # These corrections are from PJ review of files
  #####


  datafile$last_name[datafile$last_name == "cymerman" &
    datafile$journal == "leco"] <- "hepinstall-cymerman"


  datafile$inst[datafile$last_name == "voigt" &
    datafile$journal == "oecol" &
    (datafile$year == 2013 | datafile$year == 2014)] <-
    "leibniz institute for zoo and wildlife research"

  datafile$inst[datafile$last_name == "rees" &
    datafile$journal == "jecol" &
    datafile$year == 2012] <- "university of sheffield"

  datafile$inst[datafile$last_name == "holzner" &
    datafile$journal == "plantecol" &
    (datafile$year > 1984 & datafile$year < 1988)] <- "university of vienna"

  datafile$inst_CHECK[datafile$last_name == "holzner" &
    datafile$journal == "plantecol" &
    (datafile$year > 1984 & datafile$year < 1988)] <- "2x inst"

  datafile$city[datafile$last_name == "korner" &
    datafile$journal == "oecol" &
    (datafile$year > 1989 & datafile$year < 2015)] <- "basel"

  datafile$country[datafile$last_name == "korner" &
    datafile$journal == "oecol" &
    (datafile$year > 1989 & datafile$year < 2015)] <- "switzerland"

  datafile <- datafile[!(datafile$last_name == "bever" &
    datafile$year == 2012 &
    datafile$journal == "oecol"), ]
  datafile <- datafile[!(datafile$last_name == "lichstein" &
    datafile$year == 2012 &
    datafile$journal == "oecol"), ]
  datafile <- datafile[!(datafile$last_name == "gough" &
    datafile$year == 2012 &
    datafile$journal == "oecol"), ]
  datafile <- datafile[!(datafile$last_name == "heimpel" &
    datafile$year == 2012 &
    datafile$journal == "oecol"), ]
  datafile <- datafile[!(datafile$last_name == "ibanez" &
    datafile$year == 2012 &
    datafile$journal == "oecol"), ]
  datafile <- datafile[!(datafile$last_name == "layman" &
    datafile$year == 2012 &
    datafile$journal == "oecol"), ]
  datafile <- datafile[!(datafile$last_name == "le galliard" &
    datafile$year == 2012 &
    datafile$journal == "oecol"), ]




  datafile <- datafile[!(datafile$last_name == "masters" &
    datafile$year == 2010 &
    datafile$journal == "jbiog"), ]
  datafile <- datafile[!(datafile$last_name == "holland" &
    datafile$year == 1985 &
    datafile$journal == "jbiog"), ]
  datafile <- datafile[!(datafile$last_name == "lambers" &
    datafile$year == 1993 &
    datafile$journal == "plantecol"), ]
  datafile <- datafile[!(datafile$last_name == "bond" &
    datafile$year == 2004 &
    datafile$journal == "plantecol"), ]
  datafile <- datafile[!(datafile$last_name == "kollmann" &
    datafile$year == 2007 &
    datafile$journal == "plantecol"), ]
  datafile <- datafile[!(datafile$last_name == "mueller" &
    datafile$year == 2011 &
    datafile$journal == "oecol"), ]
  datafile <- datafile[!(datafile$last_name == "wieser" &
    datafile$editor_id == "162" &
    datafile$journal == "oecol"), ]
  datafile <- datafile[!(datafile$last_name == "andelman" &
    datafile$editor_id == "3270" &
    datafile$journal == "biocon"), ]
  datafile <- datafile[!(datafile$last_name == "holland" &
    datafile$first_name == "j." &
    datafile$journal == "amnat"), ]
  datafile <- datafile[!(datafile$last_name == "uy" &
    datafile$first_name == "j." &
    datafile$journal == "amnat"), ]
  datafile <- datafile[!(datafile$last_name == "dustin" &
    datafile$first_name == "marshall" &
    datafile$journal == "evol"), ]


  datafile$city[datafile$last_name == "overton" &
    datafile$journal == "leco"] <- NA

  datafile$geo.code[datafile$last_name == "betts" &
    datafile$journal == "leco"] <- "can"

  datafile$last_name[datafile$first_name == "kimberly" &
    datafile$journal == "leco" &
    datafile$year == 2004] <- "with"

  datafile$middle_name[datafile$first_name == "kimberly" &
    datafile$journal == "leco" &
    datafile$year == 2004] <- "a"

  datafile$editor_id[datafile$first_name == "kimberly" &
    datafile$journal == "leco" &
    datafile$year == 2004] <- "2113"

  datafile$last_name[datafile$last_name == "lookinbill" &
    datafile$journal == "leco"] <- "lookingbill"



  datafile <- datafile[!(datafile$last_name == "duggan" &
    datafile$year == 2008), ] # not in the front matter


  datafile$last_name[datafile$last_name == "van de pol" &
    datafile$first_name == "martin"] <- "vanderpol"

  datafile$last_name[datafile$last_name == "muller" &
    datafile$editor_id == "534"] <- "mueller"

  datafile$first_name[datafile$last_name == "ashton" &
    datafile$first_name == "p" &
    datafile$middle_name == "ms"] <- "mark"
  datafile$middle_name[datafile$last_name == "ashton" &
    datafile$first_name == "mark"] <- "s"
  datafile$editor_id[datafile$last_name == "ashton" &
    datafile$first_name == "mark"] <- "2278"
  datafile$editor_id[datafile$last_name == "baudry" &
    datafile$journal == "jape"] <- "1456"


  datafile$first_name[datafile$last_name == "dyer" &
    datafile$first_name == "andy" &
    datafile$journal == "jecol"] <- "andrew"

  datafile$first_name[datafile$last_name == "franklin" &
    datafile$first_name == "j" &
    datafile$journal == "leco"] <- "janet"


  datafile$first_name[datafile$last_name == "bonduriansky" &
    datafile$first_name == "russel"] <- "russell"
  datafile$editor_id[datafile$last_name == "bonduriansky" &
    datafile$first_name == "russell"] <- "2989"
  datafile$first_name[datafile$last_name == "chen" &
    datafile$first_name == "j" &
    datafile$inst == "university of toledo"] <- "jiquan"
  datafile$first_name[datafile$last_name == "chen" &
    datafile$first_name == "j" &
    datafile$inst == "michigan state university"] <- "jiquan"
  datafile$editor_id[datafile$last_name == "chen" &
    datafile$first_name == "jiquan"] <- "3885"




  datafile$first_name[datafile$last_name == "gaillard" &
    datafile$first_name == "jean-michelle"] <- "jean-michel"
  datafile$editor_id[datafile$last_name == "gaillard" &
    datafile$first_name == "jean-michel"
  & datafile$journal == "amnat"] <- "1589"





  datafile$last_name[datafile$last_name == "vander" &
    datafile$journal == "leco"] <- "vandermaarel"
  datafile$last_name[datafile$last_name == "moyle" &
    datafile$first_name == "peter"] <- "moyle"
  datafile$last_name[datafile$last_name == "li" &
    datafile$first_name == "feng-min"] <- "li"
  datafile$last_name[datafile$last_name == "dewalt" &
    datafile$first_name == "saara"] <- "dewalt"
  datafile$last_name[datafile$last_name == "devictor" &
    datafile$first_name == "vincent"] <- "devictor"
  datafile$last_name[datafile$last_name == "coe" &
    datafile$first_name == "m"] <- "coe"
  datafile$last_name[datafile$last_name == "monod"] <- "monod"

  datafile$first_name[datafile$last_name == "moyle" &
    datafile$first_name == "peter"] <- "peter"
  datafile$first_name[datafile$last_name == "monod"] <- "theodore"
  datafile$unit[datafile$last_name == "meffe" &
    datafile$journal == "conbio" &
    datafile$inst == "university of montana"] <- NA
  datafile$unit[datafile$last_name == "whitmore"] <- "oxford forestry institute"
  datafile$unit[datafile$last_name == "gustafson" &
    datafile$first_name == "e"] <- "north central research station"
  datafile$unit[datafile$last_name == "with" &
    datafile$first_name == "kimberly" &
    datafile$year == 2004] <- "division of biology"
  datafile$inst[datafile$last_name == "with" &
    datafile$first_name == "kimberly" &
    datafile$year == 2004] <- "kansas state university"
  datafile$state[datafile$last_name == "with" &
    datafile$first_name == "kimberly" &
    datafile$year == 2004] <- "ks"
  datafile$city[datafile$last_name == "with" &
    datafile$first_name == "kimberly" &
    datafile$year == 2004] <- "manhattan"

  datafile$editor_id[datafile$last_name == "vandonk" &
    datafile$journal == "ecology"] <- "1037"
  datafile$editor_id[datafile$last_name == "vandermaarel" &
    datafile$journal == "leco"] <- "1033"
  datafile$editor_id[datafile$last_name == "smith" &
    datafile$first_name == "melinda"] <- "2566"
  datafile$editor_id[datafile$last_name == "smith" &
    datafile$first_name == "danny"] <- NA
  datafile$editor_id[datafile$last_name == "moyle" &
    datafile$first_name == "peter"] <- 2874
  datafile$editor_id[datafile$last_name == "li" &
    datafile$first_name == "feng-min"] <- 1098
  datafile$editor_id[datafile$last_name == "lee" &
    datafile$first_name == "carol"] <- 3860
  datafile$editor_id[datafile$last_name == "fry" &
    datafile$first_name == "james"] <- 3891
  datafile$editor_id[datafile$last_name == "dewalt" &
    datafile$first_name == "saara"] <- 3311
  datafile$editor_id[datafile$last_name == "devictor" &
    datafile$first_name == "vincent"] <- 3681
  datafile$editor_id[datafile$last_name == "coe" &
    datafile$first_name == "m"] <- 2332
  datafile$editor_id[datafile$last_name == "krams" &
    datafile$first_name == "indrikis"] <- 70





  ##########
  datafile$inst[datafile$last_name == "harrison" &
    datafile$first_name == "maria" &
    datafile$journal == "newphyte" &
    datafile$year == 2004] <- "Boyce Thompson Institute"


  datafile$first_name[datafile$last_name == "lawes" &
    datafile$first_name == "mike" &
    datafile$journal == "plantecol"] <- "michael"



  datafile$inst[datafile$last_name == "pearman" &
    datafile$first_name == "peter" &
    datafile$journal == "wsl swiss federal research institute"] <-
    "wsl swiss federal institute for forest snow and landscape research birmensdorf"


  datafile$inst[datafile$last_name == "williams" &
    datafile$first_name == "d" &
    datafile$middle_name == "a" &
    datafile$journal == "jecol"] <- "university of aberdeen"

  datafile$inst[datafile$last_name == "james" &
    datafile$first_name == "helen" &
    datafile$journal == "auk"] <- "smithsonian national museum of natural history"
  datafile$inst[datafile$last_name == "lonsdale" &
    datafile$journal == "jape" &
    datafile$first_name == "w" &
    (datafile$year > 1998 | datafile$year < 2003)] <- "csiro ecosystem sciences"
  datafile$inst[datafile$last_name == "heil" &
    datafile$journal == "jecol" &
    datafile$first_name == "martin" &
    datafile$inst == "max planck university"] <- "max planck institute for chemical ecology"
  datafile$inst[datafile$last_name == "heil" &
    datafile$journal == "jecol" &
    datafile$first_name == "martin" &
    datafile$year > 2009] <- "cinvestav irapuato"
  datafile$inst[datafile$last_name == "mcglone" &
    datafile$journal == "jbiog" &
    datafile$inst == "christchurch"] <- "dsir land resources"
  datafile$inst[datafile$last_name == "williams" &
    datafile$journal == "funecol" &
    datafile$first_name == "caroline" &
    (datafile$year > 2008 | datafile$year < 2012)] <- "university of california berkeley"
  datafile$inst[datafile$last_name == "wainwright" &
    datafile$journal == "funecol" &
    datafile$first_name == "peter" &
    (datafile$year > 2008 | datafile$year < 2012)] <- "university of california davis"
  datafile$inst[datafile$last_name == "treseder" &
    datafile$journal == "funecol" &
    datafile$first_name == "kathleen" &
    (datafile$year > 2008 | datafile$year < 2016)] <- "university of california irvine"
  datafile$inst[datafile$last_name == "carroll" &
    datafile$journal == "funecol" &
    datafile$first_name == "scott" &
    (datafile$year > 2006 | datafile$year < 2016)] <- "university of california davis"
  datafile$inst[datafile$last_name == "campbell" &
    datafile$journal == "funecol" &
    datafile$first_name == "diane"] <- "university of california irvine"
  datafile$inst[datafile$last_name == "farquhar" &
    datafile$journal == "funecol" &
    datafile$first_name == "graham"] <- "australian national university"
  datafile$inst[datafile$last_name == "fernandez-juricic" &
    datafile$journal == "jape" &
    datafile$year == 2008] <- "california state university long beach"
  datafile$inst[datafile$last_name == "zuidema" &
    datafile$journal == "jecol" &
    datafile$year == 2007] <- "university of wageningen"
  datafile$inst[datafile$last_name == "vanderputten" &
    datafile$journal == "oikos"] <- "university of wageningen"
  datafile$inst[datafile$last_name == "lieth" &
    datafile$first_name == "helmut" &
    datafile$journal == "jbiog"] <- "universitat osnabruck"
  datafile$inst[datafile$last_name == "polle" &
    datafile$first_name == "andrea" &
    datafile$journal == "newphyte"] <- "university of gottingen"
  datafile$inst[datafile$last_name == "wardle" &
    datafile$year == 2012 & datafile$journal == "plantecol"] <- "university of sydney"
  datafile$inst[datafile$last_name == "fenchel" &
    datafile$first_name == "tom" &
    datafile$journal == "funecol"] <- "university of copenhagen"
  datafile$inst[datafile$last_name == "white" &
    datafile$journal == "jecol" &
    datafile$year == 1992] <- "university college dublin"
  datafile$inst[datafile$last_name == "desteven" &
    datafile$journal == "ecology" & datafile$year == 1999] <- "university of wisconsin milwaukee"
  datafile$inst[datafile$last_name == "vandermaarel" &
    datafile$journal == "leco"] <- "university of uppsala"
  datafile$inst[datafile$last_name == "sprules" &
    datafile$first_name == "gary"] <- "university of toronto mississauga"
  datafile$inst[datafile$last_name == "wagner" &
    datafile$first_name == "helene"] <- "university of toronto mississauga"
  datafile$inst[datafile$last_name == "kotanen" &
    datafile$first_name == "peter"] <- "university of toronto mississauga"
  datafile$inst[datafile$last_name == "loiselle" &
    datafile$first_name == "bette"] <- "university of missouri st louis"
  datafile$inst[datafile$last_name == "ricklefs" &
    datafile$first_name == "robert"] <- "university of missouri st louis"
  datafile$inst[datafile$last_name == "renner" &
    datafile$first_name == "susanne"] <- "university of missouri st louis"
  datafile$inst[datafile$last_name == "sork" &
    datafile$first_name == "victoria"] <- "university of missouri st louis"
  # datafile$inst[datafile$last_name=="parmentier"]<-"universite libre de bruxelles"
  datafile$inst[datafile$last_name == "debussche"] <- "cnrs centre decologie fonctionnelle et evolutive"
  # levels(datafile$inst) <- c(levels(datafile$inst),"forestry and forest products research institute","university of minnesota duluth","university of minnesota crookston")
  datafile$inst[datafile$last_name == "fujimori"] <- "forestry and forest products research institute"
  datafile$inst[datafile$last_name == "johnson" &
    datafile$first_name == "lucinda"] <- "university of minnesota duluth"
  datafile$inst[datafile$last_name == "parmentier" &
    datafile$journal == "bitr"] <- "universite libre de bruxelles"
  datafile$inst[datafile$last_name == "moen" &
    datafile$first_name == "ron"] <- "university of minnesota duluth"
  datafile$inst[datafile$last_name == "sterner" &
    datafile$first_name == "robert"] <- "university of minnesota duluth"
  datafile$inst[datafile$last_name == "vanderheijden" &
    datafile$first_name == "marcel"] <- "free university amsterdam"
  datafile$inst[datafile$last_name == "vandamme" &
    datafile$first_name == "raoul"] <- "university of antwerp"
  datafile$inst[datafile$last_name == "badyaev" &
    datafile$first_name == "alexander"] <- "university of arizona"
  datafile$inst[datafile$last_name == "davidowitz" &
    datafile$first_name == "goggy"] <- "university of arizona"
  datafile$inst[datafile$last_name == "mcgraw" &
    datafile$first_name == "kevin"] <- "arizona state university"
  datafile$inst[datafile$last_name == "cotter" &
    datafile$first_name == "sheena"] <- "queens university belfast"
  datafile$inst[datafile$last_name == "norden" &
    datafile$journal == "funecol"] <- "fundacion cedrela"
  datafile$inst[datafile$last_name == "jones" &
    datafile$inst == "ascot"] <- "nerc centre for population biology"
  datafile$inst[datafile$last_name == "jones" &
    datafile$inst == "cardiff"] <- "cardiff university"
  datafile$inst[datafile$last_name == "raubenheimer" &
    datafile$first_name == "david"] <- "massey university"
  datafile$inst[datafile$last_name == "niu" &
    datafile$first_name == "shuli"] <- "chinese academy of sciences"
  datafile$inst[datafile$last_name == "konarzewski" &
    datafile$inst == "bialystok"] <- "university of bialystok"
  datafile$inst[datafile$last_name == "newman" &
    datafile$inst == "bristol"] <- "university of bristol"
  datafile$inst[datafile$last_name == "bergelson" &
    datafile$inst == "chicago"] <- "university of chicago"
  datafile$inst[datafile$last_name == "turnbull" &
    datafile$inst == "christchurch"] <- "university of canterbury"
  datafile$inst[datafile$last_name == "mcglone" &
    datafile$inst == "christchurch"] <- "manaaki whenua landcare research"
  datafile$inst[datafile$last_name == "boggs" &
    datafile$inst == "colorado"] <- "stanford university"
  datafile$inst[datafile$last_name == "wiersma" &
    datafile$first_name == "jochum"] <- "university of minnesota crookston"
  datafile$inst[datafile$last_name == "smith" &
    datafile$first_name == "madeleine"] <- "university of minnesota crookston"
  datafile$inst[datafile$last_name == "sims" &
    datafile$first_name == "albert"] <- "university of minnesota crookston"
  datafile$inst[datafile$journal == "amnat" &
    datafile$last_name == "case"] <- "university of california san diego"
  # datafile$inst[datafile$last_name=="noon"]<-"colorado state university"
  datafile$inst[datafile$last_name == "burgess"] <- "state university of new york college of environmental science and forestry"
  datafile$inst[datafile$last_name == "fragoso"] <- "state university of new york college of environmental science and forestry"
  datafile$inst[datafile$last_name == "yanai"] <- "state university of new york college of environmental science and forestry"
  # 
  # datafile$state[datafile$last_name == "cinner" &
  #                 datafile$journal == "conbio" & 
  #                datafile$inst == "columbia university"] <-"ny"
  
  # 
  # datafile$country[datafile$last_name == "cinner" &
  #                  datafile$journal == "conbio" & 
  #                 datafile$inst == "columbia university"] <-"USA"
  # # 
  # datafile$city[datafile$last_name == "cinner" &
  #                    datafile$journal == "conbio" & 
  #                    datafile$inst == "james cook university"] <-"townsville"
  
  datafile$inst[datafile$last_name == "hall" &
    datafile$first_name == "charles" &
    datafile$journal == "conbio"] <-
    "state university of new york college of environmental science and forestry"
  datafile$inst[datafile$last_name == "pressey" &
    datafile$year == 2006 & datafile$journal == "conbio"] <-
    "unaffiliated"
  datafile$inst[datafile$last_name == "damgaard" &
    (datafile$year > 2005 & datafile$year < 2010) & datafile$journal == "funecol"] <-
    "neri"
  datafile$inst[datafile$last_name == "leroux" &
    datafile$year == 2015 & datafile$journal == "funecol"] <-
    "memorial university of newfoundland"
  datafile$inst[datafile$last_name == "noon" &
    datafile$year == 2012 & datafile$journal == "conbio"] <-
    "university of colorado boulder"
  datafile$inst[datafile$last_name == "noon" &
    datafile$year == 2014 & datafile$journal == "conbio"] <-
    "university of colorado boulder"
  datafile$inst[datafile$inst == "university of missouri"] <- "university of missouri columbia"
  datafile$inst[datafile$journal == "amnat" &
    datafile$last_name == "case"] <- "university of california san diego"
  # datafile$inst[datafile$last_name=="noon"]<-"colorado state university"
  datafile$inst[datafile$last_name == "bowers" &
    datafile$city == "vadnais heights"] <- "calyx, inc."
  datafile$inst[datafile$last_name == "debussche"] <- "cnrs centre decologie fonctionnelle et evolutive"
  datafile$inst[datafile$last_name == "whitmore"] <- "university of oxford"
  datafile$inst[datafile$last_name == "hails"] <- "university of oxford"
  datafile$inst[datafile$last_name == "leamy"] <- "university of north carolina charlotte"
  datafile$inst[datafile$last_name == "gustafson" &
    datafile$first_name == "e"] <- "us forest service"
  datafile$inst[datafile$last_name == "labandeira" &
    datafile$first_name == "conrad"] <- "smithsonian national museum of natural history"
  datafile$inst[datafile$last_name == "seidenstzicker" &
    datafile$first_name == "john"] <- "smithsonian national zoological park"
  datafile$inst[datafile$last_name == "kleiman" &
    datafile$first_name == "devra"] <- "smithsonian national zoological park"
  datafile$inst[datafile$last_name == "houllier" &
    (datafile$year > 1998 & datafile$year < 2003)] <- NA
  datafile$inst[datafile$last_name == "houllier" &
    (datafile$year > 1998 & datafile$year < 2003)] <- "laboratoire associe de modelisation des plantes"
  datafile$inst[datafile$last_name == "watling" &
    datafile$inst == "adelaide"] <- "university of adelaide"
  datafile$inst[datafile$last_name == "boutin" &
    datafile$inst == "alberta"] <- "university of alberta"
  datafile$inst[datafile$last_name == "fox" &
    datafile$inst == "alberta"] <- "university of calgary"
  datafile$inst[datafile$last_name == "patek" &
    datafile$inst == "amherst"] <- "university of massachusetts at amherst"
  datafile$inst[datafile$last_name == "mcgraw" &
    datafile$inst == "arizona"] <- "arizona state university"
  datafile$inst[datafile$last_name == "marzluff" &
    datafile$inst == "university of washington"] <- "university of washington seattle"
  datafile$inst[datafile$last_name == "felsenstein" &
    datafile$inst == "university of washington"] <- "university of washington seattle"
  datafile$inst[datafile$last_name == "tewksbury" &
    datafile$inst == "university of washington"] <- "university of washington seattle"
  datafile$inst[datafile$last_name == "kareiva" &
    datafile$inst == "university of washington"] <- "university of washington seattle"
  datafile$inst[datafile$last_name == "williams-guillen" &
    datafile$inst == "university of washington"] <- "university of washington bothell"
  datafile$inst[datafile$last_name == "slatkin" &
    datafile$inst == "university of washington"] <- "university of washington seattle"
  datafile$inst[datafile$last_name == "huey" &
    datafile$inst == "university of washington"] <- "university of washington seattle"
  datafile$inst[datafile$last_name == "olmstead" &
    datafile$inst == "university of washington"] <- "university of washington seattle"
  datafile$inst[datafile$last_name == "rohwer" &
    datafile$inst == "university of washington"] <- "university of washington seattle"
  datafile$inst[datafile$last_name == "brubaker" &
    datafile$inst == "university of washington"] <- "university of washington seattle"
  datafile$inst[datafile$last_name == "naiman" &
    datafile$inst == "university of washington"] <- "university of washington seattle"
  datafile$inst[datafile$last_name == "schindler" &
    datafile$inst == "university of washington"] <- "university of washington seattle"
  datafile$inst[datafile$last_name == "wingfield" &
    datafile$inst == "university of washington"] <- "university of washington seattle"
  datafile$inst[datafile$last_name == "thomas" &
    datafile$inst == "university of washington"] <- "university of washington seattle"
  datafile$inst[datafile$last_name == "klicka" &
    datafile$inst == "university of washington"] <- "university of washington seattle"
  datafile$inst[datafile$last_name == "bryson" &
    datafile$inst == "university of washington"] <- "university of washington seattle"
  datafile$inst[datafile$editor_id == 1578 &
    datafile$inst == "university of new york"] <- "suny college of environmental science and forestry"
  # I used "castle" for lovejoy becasue as a vice-president he is not in a unit the way the others were
  datafile$inst[datafile$last_name == "lovejoy" &
    datafile$inst == "smithsonian institution"] <- "smithsonian institution-castle"



  datafile$country[datafile$last_name == "vanderheijden"] <- "switzerland"
  # levels(datafile$inst) <- c(levels(datafile$inst),"state university of new york college of environmental science and forestry")
  datafile$country[datafile$last_name == "bieber"] <- "austria"
  datafile$country[datafile$first_name == "jeannine" &
    datafile$last_name == "cavender-bares"] <- "usa"
  datafile$country[datafile$last_name == "vanderheijden"] <- "switzerland"
  datafile$country[datafile$last_name == "gandon"] <- "france"
  datafile$country[datafile$last_name == "westing" &
    datafile$first_name == "arthur"] <- "sweden"
  datafile$country[datafile$last_name == "galdon" &
    datafile$first_name == "luis"] <- "spain"



  datafile$city[datafile$last_name == "vanderheijden" & datafile$inst == "amsterdam"] <- "amsterdam"

  return(datafile)
}

# str(datafile$editor_id)

# datafile$inst<-as.factor(datafile$inst)
# levels(datafile$inst) <- c(levels(datafile$inst),
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

#FUNCTION TO SYSTEMATIZE UNIVERSITY NAMES AND LOCATIONS
institution_cleaner <- function(DATAFILE) {
  # DATAFILE<-ALLDATA
  # SOME NOTES:
  
  # 
  # ANUTECH is a spin-off company started by ANU
  # BFH: Fed Research Center for Foresty and Forest Products - Abbreviation 
  # BFW: Austrian Research Center for Forests
  # BOKU: university of natural resources and applied life sciences vienna
  # CSIRO: Commonwealth Scientific and Industrial Research Organization
  # IADIZA: CONICET Instituto Argentino de Investigaciones de las Zonas Aridas
  # ICRAF: International Center for Research in Agroforestry
  # IFREMER: Institut Français de Recherche pour l'exploitation de la Mer
  # IMEDEA: Instituto Mediterraneo de Estudios Avanzadoslyon
  
  # IUCN: International Union for Conservation of Nature and Natural Resources
  # MWRDGC: Metropolitan Water Reclamation District of Greater Chicago
  # NIOO-KNAW: Netherlands Institue of Ecology
  # SEGC: Station DEtudes des Gorilles et Chimpanzes
  # WSL: Swiss Federal Research Institute for Forest Snow and Landscape Research
  # UMCES: University of Maryland Center for Environmental Science. UMCES is one of 12 institutions within the University System of Maryland.
  # UMR EcoFoG: CNRS Unite mixte de recherche Ecologie des forets de Guyane
  # SLU: Swedish University of Agricultural Sciences
  # CSIC: Consejo Superior de Investigaciones Científicas
  # U of N Georgia: Gainesville State College merged with N Georgia COllege and State University
  
# DATAFILE$COUNTRY<-as.factor(DATAFILE$COUNTRY)
# levels(DATAFILE$COUNTRY)
    DATAFILE$INST[DATAFILE$INST==""]<-NA
  DATAFILE$INST[DATAFILE$INST=="N/A"]<-NA
  DATAFILE$INST[DATAFILE$INST=="."]<-NA
  
  
  
  DATAFILE$COUNTRY<-as.character(DATAFILE$COUNTRY)

  
  # 

DATAFILE$COUNTRY[DATAFILE$INST=="university of new south wales"]<-"australia"
DATAFILE$COUNTRY[DATAFILE$INST=="university of guelph"]<-"canada"
DATAFILE$COUNTRY[DATAFILE$INST=="instituto mediterraneo de estudios avanzados (imedea)"]<-"spain"
##############################################################
# COrrecting the country in whihc an Editor is based
##############################################################
# DATAFILE$COUNTRY<-as.factor(DATAFILE$COUNTRY)
# DATAFILE<-droplevels(DATAFILE)
# levels(DATAFILE$COUNTRY)
##############################################################
# STILL TO DO 
##############################################################

# NEED TO CONFIRM WHAT PART OF USSR IN WHICH THE AUTHOR WAS BASED
levels(DATAFILE$COUNTRY) <- c(levels(DATAFILE$COUNTRY),"russia","scotland","northern ireland")
DATAFILE$COUNTRY[DATAFILE$COUNTRY=="ussr"]<-"russia"

#delete these
which(DATAFILE$COUNTRY=="")

##############################################################
##############################################################
# clean-up of institutions  
##############################################################
##############################################################
DATAFILE$INST[DATAFILE$INST=="."]<-NA
DATAFILE<-as.data.frame(DATAFILE)
# DATAFILE$journal<-as.factor(DATAFILE$journal)
# correcting the institution where an editor is based
# levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"state university of new york college of environmental science and forestry")
# levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"forestry and forest products research institute")
# levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"university of minnesota duluth")
# levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"university of minnesota crookston")
# # levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"university of toronto mississauga")
# levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"cnrs centre decologie fonctionnelle et evolutive")
##############################################################
##############################################################
# correcting or systematizing the name/speclling of an institution
##############################################################
##############################################################
# levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"university of missouri columbia")


DATAFILE$INST<-as.factor(DATAFILE$INST)
levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"university of missouri columbia",
                           "lancaster university",
                           "landcaster university",
                           "lancaster",
                           "csiro",
                           "australian commonwealth scientific and research organization",
                           "cnrs centre decologie fonctionnelle et evolutive",
                           "forestry and forest products research institute",
                           "university of minnesota duluth",
                           "university of minnesota crookston",
                           "university of toronto mississauga",
                           "state university of new york college of environmental science and forestry",
                           "calyx, inc.","university of north carolina charlotte",
                           "smithsonian national museum of natural history",
                           "smithsonian national zoological park",
                           "laboratoire associe de modelisation des plantes",
                           "conicet consejo nacional de investigaciones cientificas y tecnicas",
                           "fao food and agriculture organization",
                           "southern illinois university",
                           "universite libre de bruxelles",
                           "southern illinois u",
                           "aarhus university",
                           "franklin and marshall university",
                           "university of st. andrews",
                           "university of st andrews",
                           "university of pittsburgh",
                           "university of uppsala",
                           "nerc centre for population biology",
                           "massey university",
                           "university of bialystok",
                           "manaaki whenua landcare research",
                           "university of newcastle",
                           "university of natural resources and applied life sciences vienna",
                           "universidade federal de goias",
                           "universidade de brasilia",
                           "boku",
                           "bangor university",
                           "csic donana biological station",
                           "imperial college london",
                           "institute of terrestrial ecology",
                           "joint nature conservation committee",
                           "kings college",
                           "macaulay land use research institute",
                           "nerc centre for ecology and hydrology",
                           "nerc centre for ecology and hydrology banchory",
                           "nerc centre for ecology and hydrology monks wood",
                           "netherlands institute of ecology nioo knaw",
                           "northeastern university",
                           "now the usda arid land agricultural research center",
                           "oregon state university",
                           "pennsylvania state university",
                           "southwest texas state university",
                           "technical university of darmstadt",
                           "truman state university",
                           "universidad de alicante",
                           "universidad de vigo",
                           "universidad rey juan carlos",
                           "universitat osnabruck",
                           "university of costa rica",
                           "university of exeter",
                           "university of glasgow",
                           "university of kwazulu natal",
                           "university of london imperial college of science and technology",
                           "university of montreal",
                           "university of oregon",
                           "university of oxford",
                           "university of oxford",
                           "university of quebec",
                           "university of salento",
                           "university of umea",
                           "university of wageningen",
                           "university of waikato",
                           "utrecht university",
                           "wageningen agricultural university and research center alterra",
                           "institute of terrestrial ecology")


DATAFILE$CITY[DATAFILE$CITY=="w<9f>rzburg"]<-"wurzburg"
DATAFILE$CITY[DATAFILE$CITY=="k<9a>ln"]<-"cologne"
DATAFILE$CITY[DATAFILE$CITY=="g<9a>ttingen"]<-"gottingen"
DATAFILE$CITY[DATAFILE$CITY=="z<81>rich"]<-"zurich"
DATAFILE$CITY[DATAFILE$CITY=="m<9f>nchen"]<-"munich"

DATAFILE$STATE[DATAFILE$STATE=="z<81>rich"]<-NA

# DATAFILE$UNIT[DATAFILE$UNIT=="estaci<f3>n biol<f3>gica de do<f1>ana"]<-"estacion biologica de donana"
# DATAFILE$UNIT[DATAFILE$UNIT=="departamento de ecolog<90>a"]<-"departamento de ecologia"
# DATAFILE$UNIT[DATAFILE$UNIT=="fb biologie/chemie/<80>kologie"]<-"fb biologie/chemiekologie"
# DATAFILE$UNIT[DATAFILE$UNIT=="institut f<99>r biologie (ii)"]<-"institut fur biologie (ii)"





DATAFILE$INST<-as.character(DATAFILE$INST)
DATAFILE$INST<-trimws(DATAFILE$INST, which = "left")
DATAFILE$INST<-trimws(DATAFILE$INST, which = "right")
DATAFILE$INST<-gsub("  ", " ", DATAFILE$INST)
DATAFILE$INST<-gsub("- ", "-", DATAFILE$INST)
DATAFILE$INST<-gsub(" -", "-", DATAFILE$INST)
DATAFILE$INST<-gsub(" - ", "-", DATAFILE$INST)
DATAFILE$INST<-gsub(" at ", "-", DATAFILE$INST)
DATAFILE$INST<-gsub("the ", "", DATAFILE$INST)
DATAFILE$INST<-gsub("univ ", "university ", DATAFILE$INST)
DATAFILE$INST<-gsub("univ ", "university ", DATAFILE$INST)
DATAFILE$INST<-gsub("calif ", "california ", DATAFILE$INST)
DATAFILE$INST<-gsub("calif ", "california ", DATAFILE$INST)
DATAFILE$INST<-gsub("universtity ", "university ", DATAFILE$INST)
DATAFILE$INST<-gsub("univesity ", "university ", DATAFILE$INST)
DATAFILE$INST<-gsub("universityof", "university of ", DATAFILE$INST)
DATAFILE$INST<-gsub("ofarizona", "of arizona", DATAFILE$INST)
DATAFILE$INST<-gsub("unviersity", "university", DATAFILE$INST)
DATAFILE$INST<-gsub("unviversityof", "university of", DATAFILE$INST)
DATAFILE$INST<-gsub("univesrity", "university", DATAFILE$INST)
DATAFILE$INST<-gsub("unniversity","university",DATAFILE$INST)
DATAFILE$INST<-gsub("museum natl hist nat","national museum of natural history france",DATAFILE$INST)
DATAFILE$INST<-gsub("university<ca>of<ca>california<ca>santa<ca>cruz","university of california santa cruz",DATAFILE$INST)
DATAFILE$INST<-gsub("uc santa cruz","university of california santa cruz",DATAFILE$INST)
# todo: gsub arkansas one is not working inside the function and i have no idea why.
DATAFILE$INST<-gsub("arkansas","arkansas", DATAFILE$INST)
DATAFILE$INST<-gsub("uva","university of virginia",DATAFILE$INST)
# DATAFILE$INST<-gsub("us forest serv","us forest service",DATAFILE$INST)

DATAFILE$INST<-gsub("double check",NA,DATAFILE$INST)
DATAFILE$INST<-gsub("n.c. state","north carolina state university",DATAFILE$INST)
DATAFILE$INST<-gsub("uc davis","university of california davis",DATAFILE$INST)

DATAFILE$INST<-gsub("univ connecticut","university of connecticut",DATAFILE$INST)
DATAFILE$INST<-gsub("univ los andes","universidad de los andes",DATAFILE$INST)
DATAFILE$INST<-gsub("univ nacl autonoma mexico","universidad nacional autonoma de mexico",DATAFILE$INST)
DATAFILE$INST<-gsub("ume¥ university","umea university",DATAFILE$INST)
# DATAFILE$INST<-gsub("university of arkansas","university of arkansas",DATAFILE$INST)
DATAFILE$INST<-gsub(" of ", " of ", DATAFILE$INST)
DATAFILE$INST<-gsub("of california ", "of california-", DATAFILE$INST)
# DATAFILE$INST<-gsub("u ", "university of ", DATAFILE$INST)
DATAFILE$INST<-gsub("u t", "university of t", DATAFILE$INST)
DATAFILE$INST<-gsub("u w", "university of w", DATAFILE$INST)
DATAFILE$INST<-gsub("u i", "university of i", DATAFILE$INST)
DATAFILE$INST<-gsub("u s", "university of s", DATAFILE$INST)
DATAFILE$INST<-gsub("u o", "university of o", DATAFILE$INST)
DATAFILE$INST<-gsub("u m", "university of m", DATAFILE$INST)
DATAFILE$INST<-gsub("u k", "university of k", DATAFILE$INST)
DATAFILE$INST<-gsub("u l", "university of l", DATAFILE$INST)
DATAFILE$INST<-gsub("u g", "university of g", DATAFILE$INST)
DATAFILE$INST<-gsub("u f", "university of f", DATAFILE$INST)
DATAFILE$INST<-gsub("u d", "university of d", DATAFILE$INST)
DATAFILE$INST<-gsub("u c", "university of c", DATAFILE$INST)
DATAFILE$INST<-gsub("u b", "university of b", DATAFILE$INST)



DATAFILE$INST<-gsub("california, ", "california-", DATAFILE$INST)
DATAFILE$INST<-gsub("u california ", "university of california-", DATAFILE$INST)
# DATAFILE$INST<-gsub("u. of", "university of", DATAFILE$INST)
DATAFILE$INST<-gsub("univ. of", "university of", DATAFILE$INST)
DATAFILE$INST<-gsub("machigan", "michigan", DATAFILE$INST)
DATAFILE$INST<-gsub("n. prairie", "northern prairie", DATAFILE$INST)
DATAFILE$INST<-gsub("pacific s.w. research station-us forest service", "usfs-pacific southwest research station", DATAFILE$INST)
DATAFILE$INST<-gsub("n.b.s. ", "usgs-", DATAFILE$INST)
DATAFILE$INST<-gsub("u.s. ", "us ", DATAFILE$INST)
DATAFILE$INST<-gsub("illimois", "illinois", DATAFILE$INST)
DATAFILE$INST<-gsub("univerisity", "university", DATAFILE$INST)
DATAFILE$INST<-gsub("univeristy", "university", DATAFILE$INST)
DATAFILE$INST<-gsub("univerist", "university", DATAFILE$INST)
DATAFILE$INST<-gsub("universit ", "university", DATAFILE$INST)
DATAFILE$INST<-gsub(" inpa", "", DATAFILE$INST)
DATAFILE$INST<-gsub("--", " ", DATAFILE$INST)
DATAFILE$INST<-gsub("-", " ", DATAFILE$INST)









DATAFILE$INST<-gsub(" (csiro)", " ", DATAFILE$INST)
DATAFILE$INST<-gsub(" csiro", " ", DATAFILE$INST)
DATAFILE$INST<-gsub(",cas", " ", DATAFILE$INST)
DATAFILE$INST<-gsub("nacaional", "nacional", DATAFILE$INST)
DATAFILE$INST<-gsub("british colombia", "british columbia", DATAFILE$INST)
DATAFILE$INST<-gsub("smithosonian", "smithsonian", DATAFILE$INST)
DATAFILE$INST<-gsub("kansas", "kansas", DATAFILE$INST)
DATAFILE$INST<-gsub("sao paolo", "sao paulo", DATAFILE$INST)
DATAFILE$INST<-gsub("illinois", "illinois", DATAFILE$INST)
DATAFILE$INST<-gsub("wuerzburg", "wurzburg", DATAFILE$INST)
DATAFILE$INST<-gsub("latrobe", "la trobe", DATAFILE$INST)
DATAFILE$INST<-gsub("no one by", "no one by", DATAFILE$INST)
DATAFILE$INST<-gsub("botannical", "botanical", DATAFILE$INST)
DATAFILE$INST<-gsub("berkely", "berkeley", DATAFILE$INST)
DATAFILE$INST<-gsub("commonweath", "commonwealth", DATAFILE$INST)
DATAFILE$INST<-gsub("archibold", "archbold", DATAFILE$INST)
DATAFILE$INST<-gsub("loisisana", "louisiana", DATAFILE$INST)
DATAFILE$INST<-gsub("lousiana", "louisiana", DATAFILE$INST)
DATAFILE$INST<-gsub("bringham", "brigham", DATAFILE$INST)
DATAFILE$INST<-gsub("connetticut", "connecticut", DATAFILE$INST)
DATAFILE$INST<-gsub("unversity", "university", DATAFILE$INST)
DATAFILE$INST<-gsub(",", "", DATAFILE$INST)
DATAFILE$INST<-gsub("westleyan", "wesleyan", DATAFILE$INST)
DATAFILE$INST<-gsub("'", "", DATAFILE$INST)
DATAFILE$INST<-gsub("veterinary&", "veterinary and", DATAFILE$INST)
DATAFILE$INST<-gsub("&", "and", DATAFILE$INST)
DATAFILE$INST<-gsub("virginia tech", "virginia polytechnic institute and state university", DATAFILE$INST)
DATAFILE$INST<-gsub("austrailan", "australian", DATAFILE$INST)
DATAFILE$INST<-gsub("indian institute of sciences", "indian institute of science", DATAFILE$INST)
DATAFILE$INST<-gsub("kings", "kings", DATAFILE$INST)
DATAFILE$INST<-gsub("louisisana", "louisiana", DATAFILE$INST)
DATAFILE$INST<-gsub("university queensland", "university of queensland", DATAFILE$INST)
DATAFILE$INST<-gsub("universrity", "university", DATAFILE$INST)
DATAFILE$INST<-gsub("canadian forestry", "canadian forest", DATAFILE$INST)
DATAFILE$INST<-gsub("mighican", "michigan", DATAFILE$INST)
DATAFILE$INST<-gsub("de montpellier ii", "montpellier ii", DATAFILE$INST)
DATAFILE$INST<-gsub("north arizona", "northern arizona", DATAFILE$INST)
DATAFILE$INST<-gsub("wsl", "wsl", DATAFILE$INST)
DATAFILE$INST<-gsub("alabama in", "alabama", DATAFILE$INST)
DATAFILE$INST<-gsub("fonctionelle", "fonctionnelle", DATAFILE$INST)
DATAFILE$INST<-gsub("-cnrs", "", DATAFILE$INST)
# DATAFILE$INST<-gsub("university of sherbooke", "universite de sherbooke", DATAFILE$INST)
DATAFILE$INST<-gsub("smithsonian institute", "smithsonian institution", DATAFILE$INST)
DATAFILE$INST<-gsub("minnestoa", "minnesota", DATAFILE$INST)
DATAFILE$INST<-gsub("university of lausanne", "universite de lausanne", DATAFILE$INST)
DATAFILE$INST[DATAFILE$INST=="institute of ecosystem studies"]<-"cary institute of ecosystem studies"
DATAFILE$INST[DATAFILE$INST=="institute of ecosystem studies"]<-"cary institute of ecosystem studies"
DATAFILE$INST<-gsub("environment bangalore", "environment", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="university of aberdeen"  ]<-"scotland"
DATAFILE$INST[DATAFILE$INST=="auburn"] <- "auburn university"
DATAFILE$INST[DATAFILE$INST=="auburn u"] <- "auburn university"
DATAFILE$INST[DATAFILE$INST=="michigan state"] <- "michigan state university"
DATAFILE$INST[DATAFILE$INST=="berkeley"] <- "university of california berkeley"
DATAFILE$INST[DATAFILE$INST=="berkeley"] <- "university of california berkeley"
DATAFILE$INST[DATAFILE$INST=="duke"] <- "duke university"
DATAFILE$INST[DATAFILE$INST=="texas a and m"] <- "texas a and m university"
DATAFILE$INST[DATAFILE$INST=="purdue"] <- "purdue university"
DATAFILE$INST[DATAFILE$INST=="princeton"] <- "princeton university"
DATAFILE$INST[DATAFILE$INST=="oxford"] <- "oxford university"
DATAFILE$INST[DATAFILE$INST=="los alamos"] <- "los alamos national laboratory"
DATAFILE$INST[DATAFILE$INST=="stazione zoologica \"anton dohrn\""] <- "stazione zoologica anton dohrn"
DATAFILE$INST[DATAFILE$INST=="cambridge"] <- "university of cambridge"
DATAFILE$INST[DATAFILE$INST=="princeton"]<-"princeton university"
DATAFILE$INST[DATAFILE$INST=="lsu"]<-"louisiana state university"
# DATAFILE$INST[DATAFILE$INST=="wsl swiss federal research institute"]<-"swiss federal research institute wsl"
DATAFILE$UNIT[DATAFILE$INST=="pacific s.w. research station-us forest service"]<-"usfs pacific southwest research station"
# DATAFILE$INST[DATAFILE$INST=="pacific s.w. research station-us forest service"]<-"us forest service"
DATAFILE$INST[DATAFILE$INST=="university fo california berkeley"]<-"university of california berkeley"
DATAFILE$INST[DATAFILE$INST=="us geological survey"]<-"usgs"

DATAFILE$INST[DATAFILE$INST=="uf"]<-"university of florida"
DATAFILE$INST[DATAFILE$INST=="cal state bakersfield"]<-"california state university bakersfield"
DATAFILE$INST[DATAFILE$INST=="universityof wisconsin-milwaukee"]<-"university of wisconsin milwaukee"
DATAFILE$INST[DATAFILE$INST=="colorado state"]<-"colorado state university"
DATAFILE$INST[DATAFILE$INST=="cornell"]<-"cornell university"
DATAFILE$INST[DATAFILE$INST=="east carolina u"]<-"east carolina university"
DATAFILE$INST[DATAFILE$INST=="florida international u"]<-"florida international university"
DATAFILE$INST[DATAFILE$INST=="fsu"]<-"florida state university"
DATAFILE$INST[DATAFILE$INST=="uga"]<-"university of georgia"
DATAFILE$INST[DATAFILE$INST=="harvard"]<-"harvard university"
DATAFILE$INST[DATAFILE$INST=="illinois state"]<-"illinois state university"
DATAFILE$INST[DATAFILE$INST=="indiana u"]<-"indiana university"
DATAFILE$INST[DATAFILE$INST=="institute of ecosystem"]<-"institute of ecosystem studies"
DATAFILE$INST[DATAFILE$INST=="iowa state"]<-"iowa state university"
DATAFILE$INST[DATAFILE$INST=="john carroll u"]<-"john carroll university"
DATAFILE$INST[DATAFILE$INST=="knoxville"]<-"university of tennessee"
DATAFILE$INST[DATAFILE$INST=="louisiana state"]<-"louisiana state university"
DATAFILE$INST[DATAFILE$INST=="marshall unin"]<-"marshall university"
DATAFILE$INST[DATAFILE$INST=="franklin and farshall"]<-"franklin and marshall university"
DATAFILE$INST[DATAFILE$INST=="university of bangor"]<-"bangor university"
DATAFILE$INST[DATAFILE$INST=="miami u"]<-"miami university"
DATAFILE$INST[DATAFILE$INST=="montana state"]<-"montana state university"
DATAFILE$INST[DATAFILE$INST=="north dakota state"]<-"north dakota state university"
DATAFILE$INST[DATAFILE$INST=="ohio state"]<-"ohio state university"
DATAFILE$INST[DATAFILE$INST=="oregon state"]<-"oregon state university"
DATAFILE$INST[DATAFILE$INST=="rensselaer poly"]<-"rensselaer polytechnic institute"
DATAFILE$INST[DATAFILE$INST=="rutgers u"]<-"rutgers university"
DATAFILE$INST[DATAFILE$INST=="stanford"]<-"stanford university"
DATAFILE$INST[DATAFILE$INST=="stanford"]<-"stanford university"

DATAFILE$INST[DATAFILE$INST=="state university of new york at binghamton"]<-"suny binghamton"
DATAFILE$INST[DATAFILE$INST=="binghamton university-suny"]<-"suny binghamton"
DATAFILE$INST[DATAFILE$INST=="texas state"]<-"texas state university"
DATAFILE$INST[DATAFILE$INST=="the university of southwestern louisiana"]<-"university of southwestern louisiana"
DATAFILE$INST[DATAFILE$INST=="u arizona"]<-"university of arizona"
DATAFILE$INST[DATAFILE$INST=="university of california irvine"]<-"university of california irvine"
DATAFILE$INST[DATAFILE$INST=="university of california riverside"]<-"university of california riverside"
DATAFILE$INST[DATAFILE$INST=="california state university of san marcos"]<-"california state university san marcos"
DATAFILE$INST[DATAFILE$INST=="university of california at san diego"]<-"university of california san diego"
DATAFILE$INST[DATAFILE$INST=="university of california santa cruz"]<-"university of california santa cruz"
DATAFILE$INST[DATAFILE$INST=="uc santa cruz"]<-"university of california santa cruz"
DATAFILE$INST[DATAFILE$INST=="uc merced, university of california"]<-"university of california merced"
DATAFILE$INST[DATAFILE$INST=="umsl"]<-"university of missouri st louis"
DATAFILE$INST[DATAFILE$INST=="university of carolina aiken"]<-"university of south carolina aiken"
DATAFILE$INST[DATAFILE$INST=="university of massachusetts boston"]<-"university of massachusetts boston"
# DATAFILE$INST[DATAFILE$INST=="university of nevada las vegas"]<-"university of nevada-las vegas"
# DATAFILE$INST[DATAFILE$INST=="university of texas austin"]<-"university of texas austin"
DATAFILE$INST[DATAFILE$INST=="vanderbilt"]<-"vanderbilt university"
DATAFILE$INST[DATAFILE$INST=="virginia commonweath u"]<-"virginia commonweath university"
DATAFILE$INST[DATAFILE$INST=="washington state"]<-"washington state university"
DATAFILE$INST[DATAFILE$INST=="washington u"]<-"washington university"
DATAFILE$INST[DATAFILE$INST=="willamette u"]<-"willamette university"
DATAFILE$INST[DATAFILE$INST=="university of pennsylvania, "]<-"university of pennsylvania"
DATAFILE$INST[DATAFILE$INST=="university of pittsburg"]<-"university of pittsburgh"
DATAFILE$INST[DATAFILE$INST=="university of tennesee"]<-"university of tennessee"
DATAFILE$INST[DATAFILE$INST=="university of tennese"]<-"university of tennessee"
DATAFILE$INST[DATAFILE$INST=="acad natural science philadelphia"]<-"academy of natural science philadelphia"
DATAFILE$INST[DATAFILE$INST=="am mus nat hist"]<-"american museum of natural history"
DATAFILE$INST[DATAFILE$INST=="coloradostate univ"]<-"colorado state university"
DATAFILE$INST[DATAFILE$INST=="evergreen state college washington"]<-"evergreen state college"
DATAFILE$INST[DATAFILE$INST=="filed museum of natural history"]<-"field museum of natural history"
DATAFILE$INST[DATAFILE$INST=="george mason university virginia"]<-"george mason university"
DATAFILE$INST[DATAFILE$INST=="insitute of ecosystem studies"]<-"cary institute of ecosystem studies"
DATAFILE$INST[DATAFILE$INST=="kenyon college ohio"]<-"kenyon college"
DATAFILE$INST[DATAFILE$INST=="louisiana state univeristy"]<-"louisiana state university"
DATAFILE$INST[DATAFILE$INST=="milwaukee public mus"]<-"milwaukee public museum"
DATAFILE$INST[DATAFILE$INST=="missouri botanical gardens"]<-"missouri botanical garden"
# DATAFILE$INST[DATAFILE$INST=="oklahoma museum of natural history"]<-"oklahoma mus nat his"
DATAFILE$INST[DATAFILE$INST=="smithsonian(stri)"]<-"smithsonian tropical research institute"
DATAFILE$INST[DATAFILE$INST=="stri smithsonian"]<-"smithsonian tropical research institute"
DATAFILE$INST[DATAFILE$INST=="u connecticut"]<-"university of connecticut"
DATAFILE$INST[DATAFILE$INST=="university of alabama huntsville"]<-"university of alabama huntsville"
DATAFILE$INST[DATAFILE$INST=="university of connetticut storrs"]<-"university of connecticut"
DATAFILE$INST[DATAFILE$INST=="university of michgan"]<-"university of michigan"
DATAFILE$INST[DATAFILE$INST=="university of missouri st louis"]<-"university of missouri st louis"
DATAFILE$INST[DATAFILE$INST=="university of missouri_st louis"]<-"university of missouri st louis"
DATAFILE$INST[DATAFILE$INST=="university of wisconsin milwaukee"]<-"university of wisconsin milwaukee"
DATAFILE$INST[DATAFILE$INST=="u california irvine"]<-"university of california irvine"
DATAFILE$INST[DATAFILE$INST=="u california riverside"]<-"university of california riverside"
DATAFILE$INST[DATAFILE$INST=="university of california at san diego"]<-"university of california san diego"
DATAFILE$INST[DATAFILE$INST=="u california santa barbara"]<-"university of california santa barbara"
DATAFILE$INST[DATAFILE$INST=="u california santa cruz"]<-"university of california santa cruz"
DATAFILE$INST[DATAFILE$INST=="university of california los angeles"]<-"university of california los angeles"
DATAFILE$INST[DATAFILE$INST=="university of california at los angeles"]<-"university of california los angeles"
DATAFILE$INST[DATAFILE$INST=="university of california berkeley"]<-"university of california berkeley"
DATAFILE$INST[DATAFILE$INST=="university of california davis"]<-"university of california davis"
DATAFILE$INST[DATAFILE$INST=="university of california santa barbara"]<-"university of california santa barbara"
DATAFILE$INST[DATAFILE$INST=="university of california san diego"]<-"university of california san diego"
DATAFILE$INST[DATAFILE$INST=="university of california san diego"]<-"university of california san diego"
DATAFILE$INST[DATAFILE$INST=="rensselaer poly"]<-"rensselaer polytechnic institute"
DATAFILE$INST[DATAFILE$INST=="oklahoma mus nat his"]<-"oklahoma museum of natural history"
DATAFILE$INST[DATAFILE$INST=="us forest service pacific southwest research station"]<-"usfs pacific southwest research station"
DATAFILE$INST[DATAFILE$INST=="u.s.f.w.s. national wetlands research center"]<-"usfws national wetlands research center"
DATAFILE$INST[DATAFILE$INST=="arizona state university west"]<-"arizona state university west campus"
DATAFILE$INST[DATAFILE$INST=="naval research laboratory dc"]<-"usonr naval research laboratory"
DATAFILE$INST[DATAFILE$INST=="temple-inland forest"]<-"temple-inland forest products"
DATAFILE$INST[DATAFILE$INST=="usda forest service"]<-"us forest service"
DATAFILE$INST[DATAFILE$INST=="usda forest service rocky mountain research station"]<-"usfs rocky mountain research station"
DATAFILE$INST[DATAFILE$INST=="university of newcastle upon tyne"]<-"newcastle university"
DATAFILE$INST[DATAFILE$INST=="university of newcastle upon tuyne"]<-"newcastle university"
DATAFILE$INST[DATAFILE$INST=="usda forest service southern research station"]<-"usfs southern research station"
DATAFILE$INST[DATAFILE$INST=="usfs-pacific southwest research station"]<-"usfs pacific southwest research station"
DATAFILE$INST[DATAFILE$INST=="usfws-national wetlands research center"]<-"usfs national wetlands research center"
# DATAFILE$UNIT[DATAFILE$INST=="usgs forest and rangeland ecosystem science center"]<-"forest and rangeland ecosystem science center"
# DATAFILE$INST[DATAFILE$INST=="usgs forest and rangeland ecosystem science center"]<-"us geological survey"
# DATAFILE$UNIT[DATAFILE$INST=="usgs-northern prairie wildlife research center"]<-"northern prairie wildlife research center"
DATAFILE$INST[DATAFILE$INST=="usgs-northern prairie wildlife research center"]<-"usgs northern prairie wildlife research center"
DATAFILE$INST[DATAFILE$INST=="national museum of natural history"]<-"smithsonian national museum of natural history"
DATAFILE$INST[DATAFILE$INST=="smithsonian"]<-"smithsonian national museum of natural history" #eb verified
DATAFILE$INST[DATAFILE$INST=="usda"]<-"usda us department of agriculture"
# DATAFILE$UNIT[DATAFILE$INST=="usda cooperative state research, education and extension service (csrees)"]<-"usda cooperative state research, education, and extension service"
DATAFILE$INST[DATAFILE$INST=="usda cooperative state research, education and extension service (csrees)"]<-"usda cooperative state research, education, and extension service"
DATAFILE$INST[DATAFILE$INST=="virginia tech"]<-"virginia polytechnic institute and state university"
DATAFILE$INST[DATAFILE$INST=="university of pennsylvania,"]<-"university of pennsylvania"
DATAFILE$INST[DATAFILE$INST=="division of reptiles and amphibians smithsonian institute washington dc"]<-"smithsonian national museum of natural history"
DATAFILE$INST[DATAFILE$INST=="university of california-at los angeles"]<-"university of california los angeles"
DATAFILE$INST[DATAFILE$INST=="national museum of natural history, smithsonian institution"]<-"smithsonian national museum of natural history"

DATAFILE$UNIT[DATAFILE$INST=="citrus research and education center"]<-"citrus research and education center"
DATAFILE$INST[DATAFILE$INST=="citrus research and education center"]<-"university of florida"
DATAFILE$INST[DATAFILE$INST=="ohio u"]<-"ohio university"
DATAFILE$INST[DATAFILE$INST=="university of south carolina" & DATAFILE$CITY=="columbia"]<-"university of south carolina columbia"
# DATAFILE$UNIT[DATAFILE$INST=="us department of agriculture cooperative state research, education, and extension service"]<-"cooperative state research, education, and extension service"
DATAFILE$INST[DATAFILE$INST=="us department of agriculture cooperative state research, education, and extension service"]<-"usda cooperative state research, education, and extension service"
DATAFILE$INST[DATAFILE$LAST_NAME=="bowers" & DATAFILE$CITY=="vadnais heights"]<-"calyx inc"
DATAFILE$UNIT[DATAFILE$INST=="arnold arboretum of harvard university"]<-"arnold arboretum"
DATAFILE$INST[DATAFILE$INST=="arnold arboretum of harvard university"]<-"harvard university"
DATAFILE$UNIT[DATAFILE$INST=="museum of comparative zoology"]<-"museum of comparative zoology"
DATAFILE$INST[DATAFILE$INST=="museum of comparative zoology"]<-"harvard university"
DATAFILE$INST[DATAFILE$INST=="suny"]<-"suny state university of new york"
DATAFILE$INST[DATAFILE$INST=="state university of new york at stony brook"]<-"suny stony brook"
DATAFILE$INST[DATAFILE$INST=="state university of new york syracuse"]<-"suny syracuse"
# DATAFILE$INST[DATAFILE$INST=="arnold arboretum of harvard university"]<-"harvard university arnold arboretum"
DATAFILE$UNIT[DATAFILE$INST=="harvard medical school"]<-"medical school"
DATAFILE$INST[DATAFILE$INST=="harvard medical school"]<-"harvard university"
DATAFILE$INST[DATAFILE$INST=="australian research center for urban "]<-"smithsonian tropical research institute"
DATAFILE$INST<-gsub(" catie","", DATAFILE$INST)
DATAFILE$INST<-gsub("catie ","", DATAFILE$INST)
DATAFILE$INST<-gsub(" (unesp)","", DATAFILE$INST)
DATAFILE$INST[DATAFILE$INST=="universidade estadual paulista (unesp)"]<-"universidade estadual paulista"
DATAFILE$INST<-gsub("darwin university darwin","darwin university", DATAFILE$INST)
DATAFILE$INST<-gsub("technion-","", DATAFILE$INST)
DATAFILE$INST<-gsub("university california","university of california", DATAFILE$INST)
DATAFILE$INST<-gsub("israel institute of technology","technion israel institute of technology", DATAFILE$INST)
# DATAFILE$INST<-gsub("ossietzky","", DATAFILE$INST)
DATAFILE$INST<-gsub(",usda","", DATAFILE$INST)
DATAFILE$INST<-gsub("ufz centre","helmholtz centre", DATAFILE$INST)
DATAFILE$INST<-gsub(" ufz","", DATAFILE$INST)
DATAFILE$INST<-gsub("helmholtz centre for environmental research","helmholtz centre for environmental research ufz", DATAFILE$INST)
DATAFILE$INST<-gsub("a& m univ","a & m university", DATAFILE$INST)
DATAFILE$INST<-gsub("aandm","a & m", DATAFILE$INST)
DATAFILE$INST<-gsub("in cornwall","cornwall", DATAFILE$INST)
DATAFILE$INST<-gsub("suny","state university of new york", DATAFILE$INST)
# DATAFILE$INST<-gsub("usgs","us geological survey", DATAFILE$INST)
DATAFILE$INST<-gsub("university of mexico","universidad nacional autonoma de mexico", DATAFILE$INST)
DATAFILE$INST<-gsub("nc agricultural","north carolina agricultural", DATAFILE$INST)
DATAFILE$INST<-gsub("for forest, snow, and landscape research","wsl", DATAFILE$INST)
DATAFILE$INST<-gsub(" company","", DATAFILE$INST)
DATAFILE$INST<-gsub("csiro","csiro", DATAFILE$INST)
DATAFILE$INST<-gsub("state university university","state university", DATAFILE$INST)
DATAFILE$INST<-gsub("north carolina aandt state university","north carolina a & t state university", DATAFILE$INST)
DATAFILE$INST<-gsub(" manaaki whenua","", DATAFILE$INST)
DATAFILE$INST<-gsub(" headquarters","", DATAFILE$INST)
DATAFILE$INST<-gsub("vuniversity","university", DATAFILE$INST)
DATAFILE$INST<-gsub(" (imedea)","", DATAFILE$INST)
DATAFILE$INST<-gsub(" (unicamp)","", DATAFILE$INST)
DATAFILE$INST<-gsub("dhistoire","dhistoire naturelle", DATAFILE$INST)
DATAFILE$INST<-gsub("naturelle naturelle","naturelle", DATAFILE$INST)
DATAFILE$INST<-gsub("museum national dhistoire naturelle", "national museum of natural history france", DATAFILE$INST)
DATAFILE$INST<-gsub("oregan", "oregon", DATAFILE$INST)
DATAFILE$INST<-gsub("philips university","philips university marburg", DATAFILE$INST)
# DATAFILE$INST<-gsub("(csic)", "", DATAFILE$INST)
DATAFILE$INST[DATAFILE$INST=="university of mexico"]<-"universidad nacional autonoma de mexico"
DATAFILE$INST[DATAFILE$INST=="cary cary institute of ecosystem studies"]<-"cary institute of ecosystem studies"
DATAFILE$INST[DATAFILE$INST=="university of california nceas"]<-"national center for ecological analysis and synthesis"
DATAFILE$INST<-gsub("khorasan agricultural and natural resources res ctr","khorasan agricultural and natural resources research center", DATAFILE$INST)
DATAFILE$INST[DATAFILE$INST=="texas technical university"]<-"texas tech university" 
DATAFILE$INST[DATAFILE$INST=="mississippi state univ"]<-"mississippi state university" 
DATAFILE$INST[DATAFILE$INST=="empresa brasileira de pesquisa agropecuária"]<-"empresa brasileira de pesquisa agropecuaria" 
DATAFILE$INST[DATAFILE$INST=="technion technion israel institute of technology"]<-"technion israel institute of technology"
DATAFILE$INST[DATAFILE$INST=="universidad catolica de chile"]<-"pontifica universidad catolica de chile"
DATAFILE$INST[DATAFILE$INST=="university of marburg"]<-"philipps university marburg"
DATAFILE$INST[DATAFILE$INST=="norwegian inst nat management"]<-"norwegian institute for nature research"
DATAFILE$INST[DATAFILE$INST=="philipps university"]<-"philipps university marburg"
DATAFILE$INST[DATAFILE$INST=="museum dhistoire naturelle"]<-"national museum of natural history france"
DATAFILE$INST[DATAFILE$INST=="national autonomous university of mexico"]<-"universidad nacional autonoma de mexico"
DATAFILE$INST[DATAFILE$INST=="landcare research"]<-"manaaki whenua landcare research"
DATAFILE$UNIT[DATAFILE$INST=="usgs/nrii"]<-"nrii" 
DATAFILE$INST[DATAFILE$INST=="usgs/nrii"]<-"usgs national resources inventory"
DATAFILE$UNIT[DATAFILE$INST=="florida international university and center for tropical plant conservation"]<-"center for tropical plant conservation" 
DATAFILE$INST[DATAFILE$INST=="florida international university and center for tropical plant conservation"]<-"florida international university" 
DATAFILE$INST[DATAFILE$INST=="ecole normale supérieure"]<-"ecole normale superieure" 
DATAFILE$INST[DATAFILE$INST=="université de sherbrooke"]<-"university of sherbrooke"
DATAFILE$INST[DATAFILE$INST=="pontificia universidad católica de chile"]<-"pontificia universidad catolica de chile" 
DATAFILE$INST[DATAFILE$INST=="university of tromsø"]<-"university of tromso" 
# DATAFILE$INST[DATAFILE$INST=="universit\xfc\xbe\x8c\xa3\xa0\xbc montpellier ii"]<-"universite montpellier ii" 
# DATAFILE$INST[DATAFILE$INST=="universit\xfc\xbe\x8d\x83\xa0\xbct z\xfc\xbe\x8c\x93\xa0\xbcrich irchel"]<-"university of zurich irchel" 
DATAFILE$INST[DATAFILE$INST=="texas a & m univ."]<-"texas a & m university"
DATAFILE$INST[DATAFILE$INST=="texas a & m"]<-"texas a & m university"
DATAFILE$INST[DATAFILE$INST=="aberdeen"]<-"university of aberdeen"
DATAFILE$INST[DATAFILE$INST=="aberdeen"]<-"university of aberdeen"
DATAFILE$INST[DATAFILE$INST=="cambridge university"]<-"university of cambridge"
DATAFILE$INST[DATAFILE$INST=="universityof"]<-"university of"
DATAFILE$INST[DATAFILE$INST=="aberystwyth"]<-"aberystwyth university"
DATAFILE$INST[DATAFILE$INST=="alabama a and m"]<-"alabama a and m university"
DATAFILE$INST[DATAFILE$INST=="alterra"]<-"wageningen university and research"
DATAFILE$INST[DATAFILE$INST=="waginen university"]<-"wageningen university and research"
DATAFILE$INST[DATAFILE$INST=="wageningen"]<-"wageningen university and research"
DATAFILE$INST[DATAFILE$INST=="wageningen university"]<-"wageningen university and research"
DATAFILE$NOTES[DATAFILE$INST=="usgs and university of miami"]<-"secondary inst: university of miami"
DATAFILE$INST[DATAFILE$INST=="usgs and university of miami"]<-"usgs"
DATAFILE$NOTES[DATAFILE$INST=="usgs and university of california santa barbara"]<-"secondary inst: university of california santa barbara"
DATAFILE$INST[DATAFILE$INST=="usgs and university of california santa barbara"]<-"usgs"
DATAFILE$INST[DATAFILE$INST=="alterra research institute for the green world"]<-"wageningen university and research"
DATAFILE$INST[DATAFILE$INST=="assicuates"]<-"associates"
DATAFILE$INST[DATAFILE$INST=="bangor"]<-"bangor university"
DATAFILE$INST[DATAFILE$INST=="british anarctic society"]<-"british antarctic survey"
DATAFILE$INST[DATAFILE$INST=="british antartic survey"]<-"british antarctic survey"
DATAFILE$INST[DATAFILE$INST=="centro de investigaciones y experiencias forestales (cief)"]<-"centro de investigaciones y experiencias forestales"
DATAFILE$INST[DATAFILE$INST=="chinese academy of sciencies"]<-"chinese academy of sciences"
DATAFILE$INST[DATAFILE$INST=="christian albrechts universitat zu kiel"]<-"christian albrechts universitat kiel"
DATAFILE$INST[DATAFILE$INST=="consejo superior de investigaciones cientificas (csic)"]<-"csic"
DATAFILE$INST[DATAFILE$INST=="csic upv"]<-"ingenio"
DATAFILE$INST[DATAFILE$INST=="durham"]<-"durham university"
DATAFILE$INST[DATAFILE$INST=="eawag/eth"]<-"swiss federal institute of aquatic science and technology"
DATAFILE$INST[DATAFILE$INST=="edinburgh"]<-"university of edinburgh"
DATAFILE$INST[DATAFILE$INST=="freie universitt berlin"]<-"free university of berlin"
DATAFILE$INST[DATAFILE$INST=="freie university of berlin"]<-"free university of berlin"
DATAFILE$INST[DATAFILE$INST=="g√∂ttingen"]<-"university of gottingen"
DATAFILE$INST[DATAFILE$INST=="gatty marine lab (university of saint andrews"]<-"university of st andrews"
DATAFILE$INST[DATAFILE$INST=="georg august universitat gottingen"]<-"university of gottingen"
DATAFILE$INST[DATAFILE$INST=="glasgow"]<-"university of glasgow"
DATAFILE$INST[DATAFILE$INST=="gottingen university"]<-"university of gottingen"
DATAFILE$INST[DATAFILE$INST=="helsinki"]<-"university of helsinki"
DATAFILE$INST[DATAFILE$INST=="humboldt universitat zu berlin"]<-"humboldt university of berlin"
DATAFILE$INST[DATAFILE$INST=="humboldt university"]<-"humboldt university of berlin"
DATAFILE$INST[DATAFILE$INST=="institut mediterrani destudis avencats (csic uib)"]<-"csic institut mediterrani destudis avencats"
DATAFILE$INST[DATAFILE$INST=="instituto de ciencias del mar csic"]<-"csic instituto de ciencias del mar"
DATAFILE$INST[DATAFILE$INST=="izw"]<-"leibniz institute for zoo and wildlife research"
DATAFILE$INST[DATAFILE$INST=="kansas state"]<-"kansas state university"
DATAFILE$INST[DATAFILE$INST=="karlsruhe"]<-"karlsruhe institute of technology"
DATAFILE$INST[DATAFILE$INST=="pontificia universidade catolica de chile"]<-"pontificia universidad catolica de chile"
DATAFILE$INST[DATAFILE$INST=="khorasan agricultural and natural resources res. ctr"]<-"khorasan agricultural and natural resources research center"
DATAFILE$INST[DATAFILE$INST=="kyushu unniversity"]<-"kyushu university"
DATAFILE$INST[DATAFILE$INST=="lehman college cuny"]<-"cuny lehman college"
DATAFILE$INST[DATAFILE$INST=="los alamos national lab"]<-"los alamos national laboratory"
DATAFILE$INST[DATAFILE$INST=="ludwig maximilians universitat munchen"]<-"ludwig maximilian universitat munchen"
DATAFILE$INST[DATAFILE$INST=="lyme regis"]<-""
DATAFILE$INST[DATAFILE$INST=="macaulay¬†institute"]<-"macaulay land use research institute"
DATAFILE$INST[DATAFILE$INST=="max planck inst plant breeding res"]<-"max planck institute for plant breeding research"
DATAFILE$INST[DATAFILE$INST=="max planck institut fur verhaltensphysiologie"]<-"max planck institute for behavioral physiology"
DATAFILE$INST[DATAFILE$INST=="max planck institute jena"]<-"max planck institute for the science of human history"
DATAFILE$INST[DATAFILE$INST=="monterey bay aquarium research institute (mbari)"]<-"monterey bay aquarium research institute"
DATAFILE$INST[DATAFILE$INST=="mt holyoke college"]<-"mount holyoke college"
DATAFILE$INST[DATAFILE$INST=="museum national dhistoire naturelle"]<-"national museum of natural history france"
DATAFILE$INST[DATAFILE$INST=="national institute of water and atmospheric research"]<-"national institute of water and atmospheric research"
DATAFILE$INST[DATAFILE$INST=="nc state university"]<-"north carolina state university"
DATAFILE$INST[DATAFILE$INST=="neri"]<-"neri national environmental research institute"
DATAFILE$INST[DATAFILE$INST=="netherlands institute of ecology; wageningen university and research centre netherlands institute of ecology"]<-"wageningen university and research"
DATAFILE$INST[DATAFILE$INST=="neuchatel"]<-"university of neuchatel"
DATAFILE$INST[DATAFILE$INST=="new college of the university of south florida"]<-"new college of florida"
DATAFILE$INST[DATAFILE$INST=="nioz and groningen"]<-"netherlands institute for sea research"
DATAFILE$INST[DATAFILE$INST=="norwich"]<-"john innes centre"
DATAFILE$INST[DATAFILE$INST=="oklahoma state"]<-"oklahoma state university"
DATAFILE$INST[DATAFILE$INST=="oregan state university"]<-"oregon state university"
DATAFILE$INST[DATAFILE$INST=="ornl"]<-"oak ridge national laboratory"
DATAFILE$INST[DATAFILE$INST=="plymouth"]<-"marine biological association"
DATAFILE$INST[DATAFILE$INST=="plymouth marine lab"]<-"plymouth marine laboratory"
DATAFILE$INST[DATAFILE$INST=="polish academy of sciences"]<-"polish academy of science"
DATAFILE$INST[DATAFILE$INST=="research triangle park"]<-"basf"
DATAFILE$INST[DATAFILE$INST=="royal botanical gardens"]<-"royal botanical gardens kew"
DATAFILE$INST[DATAFILE$INST=="rwth aachen"]<-"rwth aachen university"
DATAFILE$UNIT[DATAFILE$INST=="scripps institute of oceanography"]<-"scripps institution of oceanography"
DATAFILE$INST[DATAFILE$INST=="scripps institute of oceanography"]<-"university of california santa diego"
DATAFILE$UNIT[DATAFILE$INST=="scripps institution  of oceanography"]<-"scripps institution of oceanography"
DATAFILE$INST[DATAFILE$INST=="scripps institution  of oceanography"]<-"university of california santa diego"
DATAFILE$INST[DATAFILE$INST=="sheffield"]<-"university of sheffield"
DATAFILE$INST[DATAFILE$INST=="sheffield"]<-"university of sheffield"
DATAFILE$INST[DATAFILE$INST=="smithsonian trop res inst"]<-"smithsonian tropical research institute"
DATAFILE$INST[DATAFILE$INST=="st. andrews"]<-"university of st andrews"
DATAFILE$INST[DATAFILE$INST=="state university of new york"]<-"suny state university of new york"
DATAFILE$INST[DATAFILE$INST=="suny albany"]<-"suny albany"
DATAFILE$INST[DATAFILE$INST=="york" & (DATAFILE$COUNTRY=="uk" | DATAFILE$COUNTRY=="united kingdom" | DATAFILE$COUNTRY=="england")]<-"university of york"
DATAFILE$INST[DATAFILE$INST=="york" & DATAFILE$COUNTRY=="canada"]<-"york university"
DATAFILE$INST[DATAFILE$INST=="york u"]<-"york university"
DATAFILE$INST[DATAFILE$INST=="suny stonybrook"]<-"suny stony brook"
DATAFILE$INST[DATAFILE$INST=="suny stonybrook"]<-"suny stony brook"
DATAFILE$INST[DATAFILE$INST=="texas a & m university"]<-"texas a and m university"
DATAFILE$INST[DATAFILE$INST=="texas tech"]<-"texas tech university"
DATAFILE$INST[DATAFILE$INST=="u.s.f.w.s. national ecology research center"]<-"usfws national ecology research center"
DATAFILE$INST[DATAFILE$INST=="u.s.f.w.s. northern prairie wildlife research center"]<-"usgs northern prairie wildlife research center"
DATAFILE$INST[DATAFILE$INST=="unam"]<-"universidad nacional autonoma de mexico"
DATAFILE$INST[DATAFILE$INST=="university buffalo"]<-"university of buffalo"
DATAFILE$INST[DATAFILE$INST=="university college of aberystwyth"]<-"aberystwyth university"
DATAFILE$INST[DATAFILE$INST=="university of arkansas little rock"]<-"university of arkansas little rock"
DATAFILE$INST[DATAFILE$INST=="university of colorado"]<-"university of colorado boulder"
DATAFILE$INST[DATAFILE$INST=="university of durham"]<-"durham university"
DATAFILE$INST[DATAFILE$INST=="university of nebranska"]<-"university of nebraska"
DATAFILE$INST[DATAFILE$INST=="university of saint andrews"]<-"university of st andrews"
DATAFILE$INST[DATAFILE$INST=="university of southern florida"]<-"university of south florida"
DATAFILE$INST[DATAFILE$INST=="university of texas at austin"]<-"university of texas austin"
DATAFILE$INST[DATAFILE$INST=="university sheffield"]<-"university of sheffield"
DATAFILE$INST[DATAFILE$INST=="university<ca>of<ca>california<ca>santa<ca>cruz"]<-"university of california santa cruz"
DATAFILE$INST[DATAFILE$INST=="washington university"]<-"washington university in st louis"
DATAFILE$INST[DATAFILE$INST=="washington university of st louis"]<-"washington university in st louis"
DATAFILE$INST[DATAFILE$INST=="western michigan university"]<-"western michigan university"
DATAFILE$INST[DATAFILE$INST=="wisconsin"]<-"university of wisconsin"
DATAFILE$INST[DATAFILE$INST=="woods hole oceanographic institute"]<-"woods hole oceanographic institution"
DATAFILE$INST[DATAFILE$INST=="woods hole research center"]<-"woods hole oceanographic institution"
DATAFILE$NOTES[DATAFILE$INST=="university of natural resources and applied life sciences boku"]<-"institution aka boku"
DATAFILE$INST[DATAFILE$INST=="university of natural resources and applied life sciences boku"]<-"university of natural resources and applied life sciences vienna"

DATAFILE$INST[DATAFILE$INST=="usu"]<-"utah state university"
DATAFILE$INST[DATAFILE$LAST_NAME=="luque"]<-"institut national de recherche en sciences et technologies pour lenvironnement et lagriculture"
# DATAFILE$INST[DATAFILE$INST=="<a0>forestry and forest products research institute"]<-"forestry and forest products research institute"
# DATAFILE$INST[DATAFILE$INST=="<a0>universit<e9> claude bernard lyon 1"]<-"universite claude bernard lyon 1" 
# DATAFILE$INST<-gsub("landscape<a0>ecology","landscape ecology", DATAFILE$INST)
# DATAFILE$INST<-gsub("universit<8a>t","universitat", DATAFILE$INST)
DATAFILE$INST<-gsub("institue",  "institute", DATAFILE$INST)
DATAFILE$INST<-gsub("univerity",  "university", DATAFILE$INST)
DATAFILE$INST[DATAFILE$INST=="canadia forest service"]<-"canadian forest service"
DATAFILE$INST[DATAFILE$INST=="lowa state university"]<-"iowa state university" 
DATAFILE$INST[DATAFILE$INST=="iowa state u"]<-"iowa state university"
DATAFILE$INST[DATAFILE$INST=="iowa state"]<-"iowa state university" 
DATAFILE$INST[DATAFILE$INST=="university of autonoma del estado de hidalgo"]<-"university of autonoma del estado de hidalgo" 
DATAFILE$INST[DATAFILE$INST=="estern illinois  university"]<-"eastern illinois university"
DATAFILE$INST[DATAFILE$INST=="syngenta crop protection"]<-"syngenta crop protection inc"
DATAFILE$INST[DATAFILE$INST=="alterra"]<-"alterra research institute for the green world"
DATAFILE$INST[DATAFILE$INST=="alterra research instituut voor de groene ruimte"]<-"alterra research institute for the green world"
DATAFILE$INST[DATAFILE$INST=="arizona state university west"]<-"arizona state university west campus"
DATAFILE$INST[DATAFILE$INST=="commonwealth scientific and industrial research organisation "]<-"commonwealth scientific and industrial research organisation"
DATAFILE$INST[DATAFILE$INST=="dartmouth university"]<-"dartmouth college"
DATAFILE$INST[DATAFILE$INST=="field museum of natural history"]<-"field museum of natural history"
DATAFILE$INST[DATAFILE$INST=="freie universitôøωt berlin"]<-"freie university of berlin"
DATAFILE$INST[DATAFILE$INST=="jawaharial nehru university"]<-"jawaharlal nehru university"
DATAFILE$INST[DATAFILE$INST=="jawaharial nehruniversity of university"]<-"jawaharlal nehru university"
DATAFILE$INST[DATAFILE$INST=="jawaharlal nehruniversity of university"]<-"jawaharlal nehru university"
DATAFILE$INST[DATAFILE$INST=="royal society for protection of bitds"]<-"royal society for protection of birds"
	
DATAFILE$INST[DATAFILE$INST=="kansas state university "]<-"kansas state university"
DATAFILE$INST[DATAFILE$INST=="landcare research  lincoln"]<-"landcare research ltd"
DATAFILE$INST[DATAFILE$INST=="lehman college"]<-"cuny lehman college"
DATAFILE$INST[DATAFILE$INST=="mississippi state univ."]<-"mississippi state university"
DATAFILE$INST[DATAFILE$INST=="monks wood"]<-"monks wood experiment station"
DATAFILE$INST[DATAFILE$INST=="oklahoma state"]<-"oklahoma state university"
DATAFILE$INST[DATAFILE$INST=="syngenta crop protection inc"]<-"syngenta crop protection inc"
DATAFILE$INST[DATAFILE$INST=="texas tech"]<-"texas tech university"
DATAFILE$INST[DATAFILE$INST=="universidad national autonoma de mexico"]<-"universidad nacional autonoma de mexico"
DATAFILE$INST[DATAFILE$INST=="universidade federal de alagoas"]<-"universidade federal de alagoas"
DATAFILE$INST[DATAFILE$INST=="universit√© de montreal"]<-"universite de montreal"
DATAFILE$INST[DATAFILE$INST=="universit√© joseph fourier"]<-"universite joseph fourier"
DATAFILE$INST[DATAFILE$INST=="universite pierre and marie curie"]<-"universite pierre et marie curie"
DATAFILE$INST[DATAFILE$INST=="uc santa cruz"]<-"university of california santa cruz"
DATAFILE$INST[DATAFILE$INST=="university of edinburhg"]<-"university of edinburgh"
DATAFILE$INST[DATAFILE$INST=="university of leuvenn"]<-"university of leuven"
DATAFILE$INST[DATAFILE$INST=="university of los andes bogota"]<-"universidad de los andes"
DATAFILE$INST[DATAFILE$INST=="university of missisippi"]<-"university of mississippi"
DATAFILE$INST[DATAFILE$INST=="university of montanna"]<-"university of montana"
DATAFILE$INST[DATAFILE$INST=="unsw"]<-"university of new south wales"
DATAFILE$INST[DATAFILE$INST=="mammal research institute (university of pretoria)"]<-"university of pretoria"
DATAFILE$INST[DATAFILE$INST=="gatty marine lab (university of saint andrews)"]<-"university of st andrews"
DATAFILE$INST[DATAFILE$INST=="university of st andrews"]<-"university of st andrews"
DATAFILE$INST[DATAFILE$INST=="university of stirlinng"]<-"university of stirling"
DATAFILE$INST[DATAFILE$INST=="university of tenessee"]<-"university of tennessee"
DATAFILE$INST[DATAFILE$INST=="university of texas at austin"]<-"university of texas austin"
DATAFILE$INST[DATAFILE$INST=="university of wisconsin"]<-"university of wisconsin"
DATAFILE$INST[DATAFILE$INST=="victorian school of forestry"]<-"victorian school of forestry"
DATAFILE$INST[DATAFILE$INST=="vrije universiteit amsterdam"]<-"vrije universiteit amsterdam"
DATAFILE$INST[DATAFILE$INST=="vrije university amsterdam"]<-"vrije universiteit amsterdam"
DATAFILE$INST[DATAFILE$INST=="vrije universiteit"]<-"vrije universiteit amsterdam"
DATAFILE$INST[DATAFILE$INST=="washington u st louis"]<-"washington university in st louis"
DATAFILE$INST[DATAFILE$INST=="western cotton research lab."]<-"usda western cotton research lab"
DATAFILE$INST[DATAFILE$INST=="western michigan university"]<-"western michigan university"
DATAFILE$INST[DATAFILE$INST=="woods hole"]<-"woods hole oceanographic institution"
DATAFILE$INST[DATAFILE$INST=="wwf"]<-"world wildlife fund"
DATAFILE$UNIT[DATAFILE$INST=="university of saint andrews"]<-"gatty marine lab"
DATAFILE$UNIT[DATAFILE$INST=="university of pretoria"]<-"mammal research institute"
DATAFILE$COUNTRY[DATAFILE$INST=="iowa state university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="aarhus university"]<-"denmark"
DATAFILE$COUNTRY[DATAFILE$INST=="southern illinois u"]<-"usa"
DATAFILE$INST[DATAFILE$INST=="southern illinois u"]<-"southern illinois university"

##############################################################
##############################################################
# dividing some names for inst into inst and unit
##############################################################
##############################################################

DATAFILE$UNIT[DATAFILE$INST=="harvard university herbaria"]<-"hu herbaria" 
DATAFILE$INST[DATAFILE$INST=="harvard university herbaria"]<-"harvard university"

DATAFILE$UNIT[DATAFILE$INST=="harvard university medical school"]<-"hu medical school"
DATAFILE$INST[DATAFILE$INST=="harvard university medical school"]<-"harvard university"

DATAFILE$UNIT[DATAFILE$INST=="boston unveristy marine program"]<-"marine program"
DATAFILE$INST[DATAFILE$INST=="boston unveristy marine program"]<-"boston university" 

DATAFILE$UNIT[DATAFILE$INST=="harvard university museum of comparative zoology"]<-"museum of comparative zoology"
DATAFILE$INST[DATAFILE$INST=="harvard university museum of comparative zoology"]<-"harvard university" 

DATAFILE$UNIT[DATAFILE$INST=="harvard forest"]<-"harvard forest"
DATAFILE$INST[DATAFILE$INST=="harvard forest"]<-"harvard university" 

DATAFILE$UNIT[DATAFILE$INST=="harvard university arnold arboretum"]<-"arnold arboretum"
DATAFILE$INST[DATAFILE$INST=="harvard university"]<-"harvard university"
DATAFILE$UNIT[DATAFILE$INST=="harvard university arnold arboretum"]<-"arnold arboretum"
DATAFILE$INST[DATAFILE$INST=="harvard university arnold arboretum"]<-"harvard university"

DATAFILE$UNIT[DATAFILE$INST=="instituto de ecologia unam"]<-"instituto de ecologia"
DATAFILE$INST[DATAFILE$INST=="instituto de ecologia unam"]<-"universidad nacional autonoma de mexico" 

DATAFILE$UNIT[DATAFILE$INST=="harvard university museum of comparative zoology"]<-"museum of comparative zoology"
DATAFILE$INST[DATAFILE$INST=="harvard university museum of comparative zoology"]<-"harvard university" 

DATAFILE$UNIT[DATAFILE$INST=="wageiningen university research center alterra"]<-"research center alterra"
DATAFILE$INST[DATAFILE$INST=="wageiningen university research center alterra"]<-"wageiningen university" 


# DATAFILE$INST<-gsub("l\x9fneburg","lunenburg", DATAFILE$INST)
# DATAFILE$INST<-gsub("universit\x8at","universitat", DATAFILE$INST)
# DATAFILE$INST<-gsub("g\xf6ttingen","gottingen", DATAFILE$INST)
# DATAFILE$INST<-gsub("m\x82xico","mexico", DATAFILE$INST)
# DATAFILE$INST<-gsub("universit\x82","universite", DATAFILE$INST)
# DATAFILE$INST<-gsub("universit\xe9","universite", DATAFILE$INST)
# DATAFILE$INST<-gsub("landscape\xa0ecology","landscape ecology", DATAFILE$INST)
# DATAFILE$INST<-gsub("aut\xfc\xbe\x8c\xa6\x84\xbcnoma","autonoma", DATAFILE$INST)
# DATAFILE$INST<-gsub("montr\xfc\xbe\x8e\x96\x94\xbcal","montreal", DATAFILE$INST)
# DATAFILE$INST<-gsub("<a0>universit<e9>","universite", DATAFILE$INST)
# DATAFILE$INST<-gsub("\xfc\xbe\x8c\x86\x84\xbc","", DATAFILE$INST)
# DATAFILE$INST<-gsub("rwth aachen\xcauniversity","rwth aachen university", DATAFILE$INST)
# DATAFILE$INST<-gsub("macquarie university \xca","macquarie university", DATAFILE$INST)
# DATAFILE$INST<-gsub("leibniz institute for zoo and wildlife research\xfc\xbe\x98\x96\x8c\xbc","leibniz institute for zoo and wildlife research", DATAFILE$INST)


DATAFILE$INST<-as.character(DATAFILE$INST)
DATAFILE$INST<-tolower(DATAFILE$INST)

DATAFILE$INST[grepl("brown univ", DATAFILE$INST)] <- "brown university"
DATAFILE$INST[grepl("cary", DATAFILE$INST)] <- "cary institute of ecosystem studies"
DATAFILE$INST[grepl("ets ingenieros", DATAFILE$INST)] <- "ets ingenieros de montes"
gsub("macaulayooinstitute","istituto per lambiente marino costiero",DATAFILE$INST)
DATAFILE$INST[grepl("macaulay", DATAFILE$INST)] <- "macaulay land use research institute"
DATAFILE$INST[grepl("osna", DATAFILE$INST)] <- "universitat osnabruck"
DATAFILE$INST[grepl("de montreal", DATAFILE$INST)] <- "university of montreal"

DATAFILE$INST<-ifelse(grepl("terrestrial ecology",DATAFILE$INST),"institute of terrestrial ecology",DATAFILE$INST)
# DATAFILE$INST<-ifelse((DATAFILE$COUNTRY="canada" & grepl("queen",DATAFILE$INST)),"queens university",DATAFILE$INST)

DATAFILE$INST<-gsub("(retired) natural history museum london","natural history museum", DATAFILE$INST)
DATAFILE$INST<-gsub("university of aarhus","aarhus university", DATAFILE$INST)

DATAFILE$INST<-gsub("university of aarhus","aarhus university", DATAFILE$INST)
DATAFILE$INST<-gsub("acadia u","aarhus university", DATAFILE$INST)
DATAFILE$INST<-gsub("university of aarhus","aarhus university", DATAFILE$INST)
DATAFILE$INST<-gsub("university of aarhus","aarhus university", DATAFILE$INST)
DATAFILE$INST<-gsub("universityniversity","university", DATAFILE$INST)

DATAFILE$INST[DATAFILE$INST=="university of california" & 
                DATAFILE$CITY=="riverside"]<-"university of california riverside"

DATAFILE$INST[DATAFILE$INST=="university of california" & 
                DATAFILE$CITY=="santa cruz"]<-"university of california santa cruz"

DATAFILE$INST[DATAFILE$INST=="university of california" & 
                DATAFILE$CITY=="oakland"]<-"university of california berkeley"

DATAFILE$INST[DATAFILE$INST=="university of california" & 
                DATAFILE$CITY=="berkeley"]<-"university of california berkeley"

DATAFILE$INST[DATAFILE$INST=="university of california" & 
                DATAFILE$CITY=="los angeles"]<-"university of california los angeles"

DATAFILE$INST[DATAFILE$INST=="university of california" & 
                DATAFILE$CITY=="davis"]<-"university of california davis"

DATAFILE$CITY[DATAFILE$INST=="university of massachusetts amherst"]<-"amherst"

DATAFILE$CITY[DATAFILE$INST=="rutgers university"]<-"new brunswick"
DATAFILE$STATE[DATAFILE$INST=="rutgers university"]<-"nj"

DATAFILE$CITY[DATAFILE$INST=="university of new south wales"]<-NA



DATAFILE$INST[DATAFILE$INST=="queen mary university of london"]<-"queen mary university of london"
DATAFILE$INST[DATAFILE$INST=="institute for terrestrial ecology" & DATAFILE$COUNTRY=="united kingdom"]<-"institute of terrestrial ecology"

DATAFILE$INST[DATAFILE$INST=="queen marys university of london"]<-"queen mary university of london"
DATAFILE$INST[DATAFILE$INST=="university of london queen mary"]<-"queen mary university of london"
DATAFILE$INST[DATAFILE$INST=="queen marys university"]<-"queen mary university of london"
DATAFILE$UNIT[DATAFILE$INST=="acadia u"]<-"aarhus university"
DATAFILE$UNIT[DATAFILE$INST=="aberdeen university"]<-"university of aberdeen"
DATAFILE$UNIT[DATAFILE$INST=="agresearch"]<-"agresearch ltd"

DATAFILE$UNIT[DATAFILE$INST=="agriculture and agri food"]<-"agriculture and agri food canada"
DATAFILE$COUNTRY[DATAFILE$INST=="queens college city university of new york"]<-"usa"
DATAFILE$INST[DATAFILE$INST=="northern prairie wildlife research center"]<-"usgs northern prairie wildlife research center"
DATAFILE$INST[DATAFILE$INST=="usgs prairie wildlife research center"]<-"usgs northern prairie wildlife research center"
DATAFILE$COUNTRY[DATAFILE$INST=="usgs northern prairie wildlife research center"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="ohio northern university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="ohio state university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="ohio wesleyan university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="oklahoma state university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="old dominion university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="oregon state university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="vrije university amsterdam"]<-"netherlands"
DATAFILE$COUNTRY[DATAFILE$INST=="penn state university"]<-"usa"
DATAFILE$INST<-gsub("penn state university","pennsylvaia state university", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="portland state university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="rochester institute of technology"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="rocky mountain research station"]<-"usa"
DATAFILE$INST[DATAFILE$INST=="rocky mountain research station"]<-"usfs rocky mountain research station"
DATAFILE$COUNTRY[DATAFILE$INST=="school of renewable resources louisiana state u"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="school of renewable resources louisiana state university"]<-"usa"
DATAFILE$UNIT[DATAFILE$INST=="school of renewable resources louisiana state u"]<-"school of renewable resources"
DATAFILE$INST[DATAFILE$INST=="school of renewable resources louisiana state u"]<-"louisiana state university"
DATAFILE$UNIT[DATAFILE$INST=="school of renewable resources louisiana state university"]<-"school of renewable resources"
DATAFILE$INST[DATAFILE$INST=="school of renewable resources louisiana state university"]<-"louisiana state university"
DATAFILE$COUNTRY[DATAFILE$INST=="smithsonian institution"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="smithsonian migratory bird center"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="smithsonian national museum of natural history"]<-"usa"
DATAFILE$INST[DATAFILE$INST=="stri"]<-"smithsonian tropical research institute"
DATAFILE$COUNTRY[DATAFILE$INST=="smithsonian tropical research institute"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="southeastern louisiana"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="southern illinois university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="stony brook university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="tall timbers research station"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="texas christian university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="texas tech university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="tulane university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="united state fish and wildlife service"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="universidad de buenos aires"]<-"argentina"
DATAFILE$COUNTRY[DATAFILE$INST=="universidad de los andes"]<-"colombia"
DATAFILE$COUNTRY[DATAFILE$INST=="universidad simon bolivar"]<-"venezuela"
DATAFILE$COUNTRY[DATAFILE$INST=="universidadautonoma del estado de hidalgo"]<-"mexico"
DATAFILE$INST<-gsub("universidadautonoma","universidad autonoma", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="university illinois urbana champaign"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of adelaide"]<-"australia"
DATAFILE$COUNTRY[DATAFILE$INST=="university of alaska"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of alaska fairbanks"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of alaska museum"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of alberta"]<-"canada"
DATAFILE$COUNTRY[DATAFILE$INST=="university of arizona"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of auckland"]<-"new zealand"
DATAFILE$COUNTRY[DATAFILE$INST=="university of bonn"]<-"germany"
DATAFILE$COUNTRY[DATAFILE$INST=="university of british columbia"]<-"canada"
DATAFILE$COUNTRY[DATAFILE$INST=="university of california"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of california berkeley"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of california davis"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of california san diego"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of california riverside"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of california santa cruz"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of cambridge"]<-"united kingdom"
DATAFILE$COUNTRY[DATAFILE$INST=="university of canterbury"]<-"new zealand"
DATAFILE$COUNTRY[DATAFILE$INST=="university of central arkansas"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of chicago"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of colorado boulder"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of colorado denver"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of connecticut"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of copenhagen"]<-"denmark"
DATAFILE$COUNTRY[DATAFILE$INST=="university of dayton"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of delaware"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of exeter"]<-"united kingdom"
DATAFILE$COUNTRY[DATAFILE$INST=="university of georgia"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of hawaii manoa"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of illinois"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of illinois universityrbana champaign"]<-"usa"
DATAFILE$INST<-gsub("universityrbana","university urbana", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="university of illinois urbana champaign"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of kansas"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of london"]<-"united kingdom"
DATAFILE$COUNTRY[DATAFILE$INST=="university of maine"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of manitoba"]<-"canada"
DATAFILE$COUNTRY[DATAFILE$INST=="university of maryland"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of maryland baltimore county"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of massachusetts"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of massachusetts amherst"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of melbourne"]<-"australia"
DATAFILE$COUNTRY[DATAFILE$INST=="university of michigan dearborn"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of minnesota"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of mississippi"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of missouri st louis"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of montana"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of nevada"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of nevada reno"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of new brunswick"]<-"canada"
DATAFILE$COUNTRY[DATAFILE$INST=="university of northern british columbia"]<-"canada"
DATAFILE$COUNTRY[DATAFILE$INST=="university of northern colorado"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of oklahoma"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of pretoria"]<-"south africa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of queensland"]<-"australia"
DATAFILE$COUNTRY[DATAFILE$INST=="university of rhode island"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of st andrews"]<-"scotland"
DATAFILE$COUNTRY[DATAFILE$INST=="university of saskatchewan"]<-"canada"
DATAFILE$COUNTRY[DATAFILE$INST=="university of south dakota"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of southern mississippi"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of tel aviv"]<-"israel"
DATAFILE$COUNTRY[DATAFILE$INST=="university of tennessee"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of victoria"]<-"canada"
DATAFILE$COUNTRY[DATAFILE$INST=="university of virginia"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of washington"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of windsor"]<-"canada"
DATAFILE$COUNTRY[DATAFILE$INST=="university of wisconsin"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of wisconsin milwaukee"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of wisconsin parkside"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of wurzburg"]<-"germany"
DATAFILE$COUNTRY[DATAFILE$INST=="university of wyoming"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="universityautonoma del estado hidalgo"]<-"mexico"
DATAFILE$INST<-gsub("universityautonoma","universidad autonoma", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="universitynacional de misiones conicet"]<-"argentina"
DATAFILE$INST<-gsub("universitynacional","university nacional", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="universitypierre et marie curie"]<-"france"
DATAFILE$INST<-gsub("universitypierre","university pierre", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="uppsala university"]<-"sweden"
DATAFILE$COUNTRY[DATAFILE$INST=="us forest service"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="us geological survey"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="usgs"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="us geological survey alaska science center"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="us geological survey biological resources division"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="us geological survey fort collins science center"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="us geological survey northern prairie wildlife research center"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="usgs northern prairie wildlife research center"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="us geological survey patuxent wildlife research center"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="us geological survey wisconsin cooperative wildlife research unit"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="usda aphis"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="usfs rocky mountain research station"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="usfws alaska region"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="usfws idaho fish and wildlife office"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="utah state university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="vassar college"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="villanova university"]<-"usa"
DATAFILE$INST[DATAFILE$INST=="virginia polytechnic institute state university"]<-"virginia polytechnic institute and state university"
DATAFILE$COUNTRY[DATAFILE$INST=="virginia polytechnic institute and state university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="virginia polytechnic institute state university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="washington state university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="western michigan university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="wichita state university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="york university"]<-"canada"
DATAFILE$COUNTRY[DATAFILE$INST=="zoological society of london"]<-"united kingdom"
DATAFILE$COUNTRY[DATAFILE$INST=="prairie wildlife research center"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="queens university"]<-"canada"
DATAFILE$COUNTRY[DATAFILE$INST=="pacific rim conservation"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="open university"]<-"united kingdom"
DATAFILE$COUNTRY[DATAFILE$INST=="palacky university"]<-"czech republic"
DATAFILE$COUNTRY[DATAFILE$INST=="percy fitzpatrick institute"]<-"south africa"
DATAFILE$COUNTRY[DATAFILE$INST=="point blue conservation"]<-"usa"
DATAFILE$INST[DATAFILE$INST=="queens university of belfast"]<-"queens university belfast"
DATAFILE$COUNTRY[DATAFILE$INST=="queens university of belfast"]<-"northern ireland"
DATAFILE$COUNTRY[DATAFILE$INST=="queens university belfast"]<-"northern ireland"
DATAFILE$COUNTRY[DATAFILE$INST=="rangeland ecosystem science center"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="tabor college"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="snake river field station"]<-"usa" #usgs
DATAFILE$COUNTRY[DATAFILE$INST=="senckenberg research institute frankfurt"]<-"germany"
DATAFILE$COUNTRY[DATAFILE$INST=="simon fraser university"]<-"canada"
DATAFILE$COUNTRY[DATAFILE$INST=="roosevelt universityfield museum"]<-"usa"
DATAFILE$INST[DATAFILE$INST=="roosevelt universityfield museum"]<-"field museum"
DATAFILE$COUNTRY[DATAFILE$INST=="royal ontario museum"]<-"canada"
DATAFILE$COUNTRY[DATAFILE$INST=="ontario ministry of natural resources"]<-"canada"
DATAFILE$COUNTRY[DATAFILE$INST=="stazione zoologica anton dohrn"]<-"italy"
DATAFILE$COUNTRY[DATAFILE$INST=="tetra tech eba"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="tetra tech ec inc"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="tetratech environmental consulting inc"]<-"usa"
DATAFILE$INST[DATAFILE$INST=="tetra tech eba"]<-"tetra tech inc"
DATAFILE$INST[DATAFILE$INST=="tetra tech ec inc"]<-"tetra tech inc"
DATAFILE$INST[DATAFILE$INST=="tetratech environmental consulting inc"]<-"tetra tech inc"
DATAFILE$COUNTRY[DATAFILE$INST=="aarhus university"]<-"denmark"
DATAFILE$INST[DATAFILE$INST=="notre dame"]<-"university of notre dame"
DATAFILE$INST[DATAFILE$INST=="notre dame university"]<-"university of notre dame"
DATAFILE$COUNTRY[DATAFILE$INST=="tokaigakuen university"]<-"japan"
DATAFILE$INST<-gsub("tokaigakuen","tokai gakuen", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="trent university"]<-"canada"
DATAFILE$COUNTRY[DATAFILE$INST=="trinity university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="western foundation of vertebrate zoology"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="western university canada"]<-"canada"
DATAFILE$COUNTRY[DATAFILE$INST=="universidadde antioquia"]<-"colombia"
DATAFILE$COUNTRY[DATAFILE$INST=="universidadde buenos aires"]<-"argentina"
DATAFILE$COUNTRY[DATAFILE$INST=="universidadde los andes"]<-"colombia"
DATAFILE$COUNTRY[DATAFILE$INST=="universidadde vila velha"]<-"brazil"
DATAFILE$INST[DATAFILE$INST=="universidadde vila velha"]<-"universidade vila velha"
DATAFILE$INST<-gsub("universidadde","universidad de", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="universidadnacional autonoma de mexico"]<-"mexico"
DATAFILE$INST<-gsub("universidadnacional","universidad nacional", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="universita del salento"]<-"italy"
DATAFILE$COUNTRY[DATAFILE$INST=="universita di genova"]<-"italy"
DATAFILE$COUNTRY[DATAFILE$INST=="universite pierre et marie curie"]<-"france"

DATAFILE$COUNTRY[DATAFILE$INST=="american museum of natural history"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="free university amsterdam"]<-"netherlands"
DATAFILE$COUNTRY[DATAFILE$INST=="antioch new england graduate school"]<-"usa"
DATAFILE$INST[DATAFILE$INST=="antioch new england graduate school and national university of rwanda"]<-"antioch new england graduate school"
DATAFILE$INST[DATAFILE$INST=="arizona state u"]<-"arizona state university"
DATAFILE$INST[DATAFILE$INST=="asu"]<-"arizona state university"
DATAFILE$COUNTRY[DATAFILE$INST=="arizona state university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="auburn university"]<-"usa"
DATAFILE$INST[DATAFILE$INST=="agresearch ltd."]<-"agresearch"
DATAFILE$INST[DATAFILE$INST=="agresearch ltd"]<-"agresearch"
DATAFILE$INST<-gsub("stationn","station", DATAFILE$INST)
DATAFILE$INST<-gsub("assicuates","associates", DATAFILE$INST)
DATAFILE$INST[DATAFILE$INST=="agriculture and agri food"]<-"agriculture and agri food canada"

DATAFILE$COUNTRY[DATAFILE$INST=="alaska science center"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="amherst college"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="austin peay state university"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="australian commonwealth scientific and research organization"]<-"australia"
DATAFILE$COUNTRY[DATAFILE$INST=="australian museum"]<-"australia"	
DATAFILE$COUNTRY[DATAFILE$INST=="australian national wildlife collection"]<-"australia"	
DATAFILE$COUNTRY[DATAFILE$INST=="aves argentinas"]<-"argentina"	
DATAFILE$COUNTRY[DATAFILE$INST=="bates college"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="beloit college"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="bloomfield college"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="boise state u"]<-"usa"
DATAFILE$INST[DATAFILE$INST=="boise state u"]<-"boise state university"
DATAFILE$COUNTRY[DATAFILE$INST=="boise state university"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="bucknell university"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="california academy of sciences"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="california state university san marcos"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="canadian wildlife service"]<-"canada"	
DATAFILE$COUNTRY[DATAFILE$INST=="catholic university of chile"]<-"chile"	
DATAFILE$COUNTRY[DATAFILE$INST=="centre national de la recherche scientifique"]<-"france"	
DATAFILE$COUNTRY[DATAFILE$INST=="citadel military college of south carolina"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of pittsburgh"]<-"usa"
DATAFILE$CITY[DATAFILE$INST=="university of pittsburgh"]<-"pittsburgh"
DATAFILE$STATE[DATAFILE$INST=="university of pittsburgh"]<-"pa"
DATAFILE$COUNTRY[DATAFILE$INST=="city university of new york"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="college of new jersey"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="college of william and mary"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="colorado state university"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="columbia university"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="commonwealth university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="consejo nacional de investigaciones cientificas y tecnicas"]<-"argentina"
DATAFILE$COUNTRY[DATAFILE$INST=="cornell lab of ornithology"]<-"usa"
DATAFILE$UNIT[DATAFILE$INST=="cornell lab of ornithology"]<-"cornell lab of ornithology"
DATAFILE$INST[DATAFILE$INST=="cornell lab of ornithology"]<-"cornell university"
DATAFILE$COUNTRY[DATAFILE$INST=="cornell u"]<-"usa"	
DATAFILE$INST[DATAFILE$INST=="cornell u"]<-"cornell university"	
DATAFILE$COUNTRY[DATAFILE$INST=="cornell university"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="deakin university australia"]<-"australia"
DATAFILE$COUNTRY[DATAFILE$INST=="delta waterfowl foundation"]<-"canada"
DATAFILE$COUNTRY[DATAFILE$INST=="denver museum of nature e science"]<-"usa"	
DATAFILE$INST[DATAFILE$INST=="denver museum of nature e science"]<-"denver museum of nature and science"	
DATAFILE$COUNTRY[DATAFILE$INST=="disteba universita di lecce"]<-"italy"	
DATAFILE$COUNTRY[DATAFILE$INST=="donana biological station csic"]<-"spain"	
DATAFILE$COUNTRY[DATAFILE$INST=="ducks universitynlimited canada"]<-"canada"
DATAFILE$INST[DATAFILE$INST=="ducks universitynlimited canada"]<-"ducks ulimited canada"
DATAFILE$COUNTRY[DATAFILE$INST=="duke university"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="durham university"]<-"united kingdom"	
DATAFILE$COUNTRY[DATAFILE$INST=="eastern illinois university"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="environment canada"]<-"canada"	
DATAFILE$COUNTRY[DATAFILE$INST=="environment canada national hydrology research center"]<-"canada"	
DATAFILE$COUNTRY[DATAFILE$INST=="environment canada wildlife research division"]<-"canada"	
DATAFILE$COUNTRY[DATAFILE$INST=="environment canada wildlife research east"]<-"canada"	
DATAFILE$COUNTRY[DATAFILE$INST=="estacion biologia chamela"]<-"mexico"	
DATAFILE$COUNTRY[DATAFILE$INST=="estern illinois university"]<-"usa"
DATAFILE$INST[DATAFILE$INST=="estern illinois university"]<-"eastern illinois university"
DATAFILE$COUNTRY[DATAFILE$INST=="field museum of natural history"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="florida institute of technology"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="forschungsinstitut senckenberg"]<-"germany"	
DATAFILE$COUNTRY[DATAFILE$INST=="free university of berlin"]<-"germany"		
DATAFILE$COUNTRY[DATAFILE$INST=="geomar helmholtz center for ocean research kiel"]<-"germany"			
DATAFILE$COUNTRY[DATAFILE$INST=="georgia southern u"]<-"usa"
DATAFILE$INST[DATAFILE$INST=="georgia southern u"]<-"georgia southern university"	
DATAFILE$COUNTRY[DATAFILE$INST=="georgia southern university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="guangxi university"]<-"china"	
DATAFILE$COUNTRY[DATAFILE$INST=="hamilton college"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="harvard university"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="hasting reservation"]<-"usa"
DATAFILE$UNIT[DATAFILE$INST=="hasting reservation"]<-"hastings natural history reservation"
DATAFILE$COUNTRY[DATAFILE$INST=="hawk mountain sanctuary"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="hobart and william smith college"]<-"usa"		
DATAFILE$COUNTRY[DATAFILE$INST=="hofstra university"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="humbolt state university"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="hunter college cuny"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="ibigeo conicet"]<-"argentina"	
DATAFILE$COUNTRY[DATAFILE$INST=="illinois natural history survey"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="indiana state university"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="institut fur ostseeforschungwarnemunde"]<-"germany"		
DATAFILE$INST[DATAFILE$INST=="institut fur ostseeforschungwarnemunde"]<-"leibniz institute for baltic sea research"
DATAFILE$COUNTRY[DATAFILE$INST=="instituto de biologia universitynam"]<-"mexico"			
DATAFILE$UNIT[DATAFILE$INST=="instituto de biologia universitynam"]<-"instituto de biologia"
DATAFILE$INST[DATAFILE$INST=="instituto de biologia universitynam"]<-"universidad nacional autonoma de mexico"
DATAFILE$COUNTRY[DATAFILE$INST=="interuniversity institute for marine sciences"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="james san jacinto mountains reserve"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="kansas state university"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="kruger national park"]<-"south africa"		
DATAFILE$COUNTRY[DATAFILE$INST=="los alamos national laboratory"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="louisiana state university"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="louisiana state university and university federal university of roraima"]<-"usa"
DATAFILE$INST[DATAFILE$INST=="louisiana state university and university federal university of roraima"]<-"louisiana state university"	
DATAFILE$COUNTRY[DATAFILE$INST=="lsa associates"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="lundy environmental consulting"]<-"usa"		
DATAFILE$COUNTRY[DATAFILE$INST=="massey university"]<-"new zealand"			
DATAFILE$COUNTRY[DATAFILE$INST=="max planck institute for ornithology"]<-"germany"			
DATAFILE$COUNTRY[DATAFILE$INST=="mckendree university"]<-"usa"			
DATAFILE$COUNTRY[DATAFILE$INST=="michigan state university"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="minnesota pollution control agency"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="missouri department of conservation"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="museum of new zealand te papa tongarewa"]<-"new zealand"	
DATAFILE$COUNTRY[DATAFILE$INST=="national audubon society"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="national aviary"]<-"usa"		
DATAFILE$COUNTRY[DATAFILE$INST=="national history museum paris"]<-"france"
DATAFILE$COUNTRY[DATAFILE$INST=="national park service inventory e monitoring program"]<-"usa"
DATAFILE$INST[DATAFILE$INST=="national park service inventory e monitoring program"]<-"usnps inventory and monitoring program"
DATAFILE$COUNTRY[DATAFILE$INST=="national university of singapore"]<-"singapore"	
DATAFILE$COUNTRY[DATAFILE$INST=="natural history museum"]<-"united kingdom"
DATAFILE$COUNTRY[DATAFILE$INST=="natural history museum uk"]<-"united kingdom"
DATAFILE$INST[DATAFILE$INST=="natural history museum uk"]<-"natural history museum"
DATAFILE$COUNTRY[DATAFILE$INST=="natural history museum uk"]<-"united kingdom"	
DATAFILE$COUNTRY[DATAFILE$INST=="natural resources institute university of manitoba"]<-"canada"		
DATAFILE$UNIT[DATAFILE$INST=="natural resources institute university of manitoba"]<-"natural resources institute"
DATAFILE$INST[DATAFILE$INST=="natural resources institute university of manitoba"]<-"university of manitoba"
DATAFILE$COUNTRY[DATAFILE$INST=="new mexico state university"]<-"usa"	
DATAFILE$COUNTRY[DATAFILE$INST=="interuniversity institute for marine sciences"]<-"israel"	
# DATAFILE$COUNTRY[DATAFILE$INST=="north central research station"]<-"usa"
DATAFILE$INST[DATAFILE$INST=="north central research station"]<-"usfs north central research station"
DATAFILE$COUNTRY[DATAFILE$INST=="usfs north central research station"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="north dakota state university"]<-"usa"
DATAFILE$COUNTRY[DATAFILE$INST=="northern prairie wildlife research center"]<-"usa"	
DATAFILE$INST[DATAFILE$INST=="northern praire wildlife research center"]<-"usgs northern prairie wildlife research center" 
DATAFILE$COUNTRY[DATAFILE$INST=="rocky mountain research station"]<-"usa"	
DATAFILE$INST[DATAFILE$INST=="us forest service usda"]<-"us forest service"
DATAFILE$COUNTRY[DATAFILE$INST=="university of missouri st. louis"]<-"usa"
DATAFILE$INST[DATAFILE$INST=="university of missouri st. louis"]<-"university of missouri st louis"
DATAFILE$COUNTRY[DATAFILE$INST=="(retired) natural history museum london"]<-"united kingdom"
DATAFILE$INST[DATAFILE$INST=="(retired) natural history museum london"]<-"natural history museum"
# 
DATAFILE$INST[DATAFILE$INST=="cambridge university"]<-"university of cambridge"
DATAFILE$INST[DATAFILE$INST=="universite de sherbrook"]<-"university of sherbrooke"
DATAFILE$INST[DATAFILE$INST=="université joseph fourier"]<-"universite joseph fourier"
DATAFILE$INST[DATAFILE$INST=="royal veterinary andagricultural university"]<-"royal veterinary and agricultural university"
DATAFILE$INST[DATAFILE$INST=="royal society for the protection of bitds"]<-"royal society for the protection of birds"
DATAFILE$INST[DATAFILE$INST=="royal holloway"]<-"royal holloway university of london"

DATAFILE$INST[DATAFILE$INST=="pontifica universidad catolica de chile" ]<-"pontificia universidad catolica de chile"
DATAFILE$INST[DATAFILE$INST== "pennsylvaia state university"]<-"pennsylvania state university"   
DATAFILE$INST[DATAFILE$INST=="phiiipps university marburg"  ]<-"philipps university marburg"
DATAFILE$INST[DATAFILE$INST=="simon fraser university\v" ]<-"simon fraser university"
DATAFILE$INST[DATAFILE$INST== "landcaster university"]<-"lancaster university"
DATAFILE$INST[DATAFILE$INST== "agr univ norway"]<-"agricultural university of norway"
DATAFILE$INST[DATAFILE$INST== "agr university norway"]<-"agricultural university of norway"
DATAFILE$INST[DATAFILE$INST=="albert ludwigs universitatfreiburg" ]<-"university of freiburg"
DATAFILE$INST[DATAFILE$INST== "asa cssa sssa"]<-"american society of agronomy"
DATAFILE$INST[DATAFILE$INST== "austral center for scientific research"  ]<-"conicet austral center for scientific research"  
DATAFILE$INST[DATAFILE$INST== "austrian research centre for forests (bfw)"]<-"austrian research centre for forests"
DATAFILE$INST[DATAFILE$INST== "british antarctic survey cambridge"]<-"british antarctic survey"
DATAFILE$INST[DATAFILE$INST==  "university of oldenberg"]<-"university of oldenburg"
DATAFILE$INST[DATAFILE$INST== "carl von university" | DATAFILE$INST== "carl von university oldenburg"| DATAFILE$INST== "university of oldenburg" ]<-"carl von ossietzky university oldenburg"
DATAFILE$INST[DATAFILE$INST== "carl von ossietzky university"]<-"carl von ossietzky university oldenburg"
DATAFILE$INST[DATAFILE$INST== "centre decologie fonctionnelle et evolutive" ]<-"cnrs centre decologie fonctionnelle et evolutive" 
DATAFILE$INST[DATAFILE$INST== "cnrs centre decologie fonctionnelle et evolutive" ]<-"cnrs centre decologie fonctionnelle et evolutive" 
DATAFILE$INST[DATAFILE$INST== "christ church"]<-"christchurch"
DATAFILE$INST[DATAFILE$INST=="cisro sustainable ecosystems" ]<-"csiro sustainable ecosystems"
DATAFILE$INST[DATAFILE$INST=="columbia universily" ]<-"columbia university"
DATAFILE$INST[DATAFILE$INST== "commonwealth scientific and industrial research organisation" ]<-"csiro commonwealth scientific and industrial research organisation"
DATAFILE$INST[DATAFILE$INST== "csiro" ]<-"csiro commonwealth scientific and industrial research organisation"
DATAFILE$INST[DATAFILE$INST== "commonwealth scientific and industrial research organisation (csiro)"]<-"csiro commonwealth scientific and industrial research organisation"
DATAFILE$INST[DATAFILE$INST== "cisro" & DATAFILE$UNIT=="tropical ecosystems"]<-"csiro tropical ecosystems"
DATAFILE$INST[DATAFILE$INST== "consejo nacl invest cient and tecn"]<-"consejo nacional de investigaciones cientificas y tecnicas" 
DATAFILE$INST[DATAFILE$INST== "davis"]<-"university of california davis"
DATAFILE$INST[DATAFILE$INST== "deakin university australia"]<-"deakin university"
DATAFILE$UNIT[DATAFILE$INST=="division of reptiles and amphibians smithsonian institution washington dc" ]<-"division of reptiles and amphibians" 
DATAFILE$INST[DATAFILE$INST== "division of reptiles and amphibians smithsonian institution washington dc"]<-"national museum of natural history smithsonian institution"
DATAFILE$INST[DATAFILE$INST=="donana biological station" ]<-"csic donana biological station"
DATAFILE$INST[DATAFILE$INST=="donana biological station csic" ]<-"csic donana biological station"
DATAFILE$INST[DATAFILE$INST=="dresden institute of technology" ]<-"dresden university of technology"
DATAFILE$INST[DATAFILE$INST== "eawag/eth zurich"]<- "swiss federal institute of aquatic science and technology"
DATAFILE$INST[DATAFILE$INST== "ecole normale sup<82>rieure"]<-"ecole normale superieure"
DATAFILE$INST[DATAFILE$INST=="edinburgh university" ]<-"university of edinburgh"
DATAFILE$INST[DATAFILE$INST=="eidgenossiche technische hochschule (eth) zurich"]<-"eth zurich"
DATAFILE$INST[DATAFILE$INST== "evolution and marine biology university of california"]<-"university of california santa barbara"
DATAFILE$INST[DATAFILE$INST== "fachstelle fur pflanzenschutz" ]<-"university of bern"
DATAFILE$INST[DATAFILE$INST== "field museum"]<-"field museum of natural history"
DATAFILE$INST[DATAFILE$INST== "florida"]<-"university of florida"
DATAFILE$INST[DATAFILE$INST=="freie universit�t berlin" ]<-"free university of berlin"
DATAFILE$INST[DATAFILE$INST=="griffith sch environm"  ]<-"griffith university" 
DATAFILE$INST[DATAFILE$INST==  "humboldt university berlin"]<-"humboldt university of berlin"
 
  DATAFILE$INST[DATAFILE$INST== "royal veterinary andagricultural university" ]<- "royal veterinary and agricultural university"
  DATAFILE$INST[DATAFILE$INST== "imperial college" ]<- "imperial college london"
  DATAFILE$INST[DATAFILE$INST== "imperial university" ]<- "imperial college london"
  DATAFILE$INST[DATAFILE$INST=="imperial collegesilwood park"  ]<- "imperial college silwood park"
  DATAFILE$INST[DATAFILE$INST== "indian inst science bangalore" ]<- "indian institute of science" 
  DATAFILE$INST[DATAFILE$INST== "instituto de biologia unam" ]<- "universidad nacional autonoma de mexico"
  DATAFILE$INST[DATAFILE$INST=="instituto nacional de pesquisas da amazonia inpa"  ]<- "instituto nacional de pesquisas da amazonia"
  DATAFILE$INST[DATAFILE$INST== "instituto venezolano investigaciones cientificas (ivic)"  ]<- "instituto venezolano investigaciones cientificas" 
  DATAFILE$INST[DATAFILE$INST=="intelligent solutions group (isg)" ]<- "intelligent solutions group"
  DATAFILE$INST[DATAFILE$INST==  "j.w. goethe university frankfurt am main"]<- "goethe university frankfurt"
  DATAFILE$INST[DATAFILE$INST== "jaldin botanique nacional de belgique" ]<- "jardin botanique nacional de belgique"
  DATAFILE$INST[DATAFILE$INST== "john innes center for plant science research"  ]<- "john innes center" 
  DATAFILE$INST[DATAFILE$UNIT=="la selva biological station"  ]<- "la selva biological station"
  DATAFILE$INST[DATAFILE$INST== "la selva biological station" ]<- "organization for tropical studies"
  DATAFILE$INST[DATAFILE$INST== "landcaster university" ]<- "lancaster university"
  DATAFILE$INST[DATAFILE$INST== "ludwig maximilians universit<84>t m<81>nchen" ]<- "ludwig maximilian university of munich"
  DATAFILE$INST[DATAFILE$INST== "ludwig maximilians university of munich" ]<- "ludwig maximilian university of munich"
  DATAFILE$INST[DATAFILE$INST== "university of munich" ]<- "ludwig maximilian university of munich"
  DATAFILE$COUNTRY[DATAFILE$INST== "ludwig maximilian university of munich" ]<- "germany"
  DATAFILE$INST[DATAFILE$INST== "lehrstuhl fur landschaftsokologie der technischen universitat munchen" ]<- "technical university of munich"
  DATAFILE$INST[DATAFILE$INST== "ludwig maximilians universitat munchen" ]<- "ludwig maximilians university of munich"
  DATAFILE$INST[DATAFILE$INST== "macaulay institute" ]<-"macaulay land use research institute"
    DATAFILE$INST[DATAFILE$INST== "macquaire university" ]<-"macquarie university"
    DATAFILE$INST[DATAFILE$INST== "michigan technol university" ]<-"michigan technological university"
    DATAFILE$INST[DATAFILE$INST== "mississippi state" ]<-"mississippi state university"
    DATAFILE$INST[DATAFILE$INST== "museum fu_r naturkunde" ]<-"museum fur naturkunde"
    DATAFILE$INST[DATAFILE$INST== "n/a"   ]<-"missing"  
    DATAFILE$INST[DATAFILE$INST== "national audobon society"  ]<-"national audubon society" 
    DATAFILE$INST[DATAFILE$INST== "natural history museum" & DATAFILE$COUNTRY=="united kingdom" ]<-"natural history museum london"
    DATAFILE$INST[DATAFILE$INST== "national history museum paris" ]<-"national natural history museum france"
    DATAFILE$INST[DATAFILE$INST==  "nioz royal netherlands inst sea res"]<-"royal netherlands institute for sea research"
    DATAFILE$INST[DATAFILE$INST== "noinst" ]<-"missing"
    DATAFILE$INST[DATAFILE$INST==  "north carolina agricultural and technical state university" ]<-"north carolina a and t state university" 
    DATAFILE$INST[DATAFILE$INST==  "ohio university main campus"]<-"ohio university"
    DATAFILE$INST[DATAFILE$INST== "pacific agri food research centre"  ]<-"pacific agrifood research centre" 
    DATAFILE$INST[DATAFILE$INST== "pekinn"   ]<-"peking"  
    DATAFILE$INST[DATAFILE$INST== "pepperdine"  ]<-"pepperdine university" 
    DATAFILE$INST[DATAFILE$INST==  "percy fitzpatrick institute"]<-"percy fitzpatrick institute of african ornithology"
    DATAFILE$INST[DATAFILE$INST== "plant sciences department" & DATAFILE$LAST_NAME=="samples"  ]<-"university of tennessee"
    DATAFILE$INST[DATAFILE$INST==  "queens u" ]<-"queens university" 
    DATAFILE$INST[DATAFILE$INST== "radboud university" ]<-"radboud university nijmegen"
    DATAFILE$INST[DATAFILE$INST== "retired no affiliation listed" ]<-"retired (no affiliation)"
    DATAFILE$INST[DATAFILE$INST== "riverside" ]<- "university of california riverside"
    DATAFILE$INST[DATAFILE$INST== "rose hulman institute of technology\v" ]<-"rose hulman institute of technology"
    DATAFILE$INST[DATAFILE$INST==  "santa cruz"]<-"university of california santa cruz"
    DATAFILE$INST[DATAFILE$INST== "school of biological sciences" & DATAFILE$LAST_NAME=="wardle"  ]<-"university of sydney"
    DATAFILE$INST[DATAFILE$INST==  "school of environment and nat resources"]<-"ohio state university"
    DATAFILE$INST[DATAFILE$INST==  "school of environmental science"    ]<-"murdoch university"
    DATAFILE$INST[DATAFILE$INST==  "school of plant environmental and soil sciences" ]<-"louisiana state university" 
    DATAFILE$INST[DATAFILE$INST== "simon fraser university\v" ]<-"simon fraser university"
    DATAFILE$INST[DATAFILE$INST== "slu" ]<-"swedish university of agricultural sciences"
    DATAFILE$INST[DATAFILE$INST==  "south florida"]<-"university of south florida"
    DATAFILE$INST[DATAFILE$INST== "southeastern louisiana" ]<-"southeastern louisiana university"
    DATAFILE$INST[DATAFILE$INST== "southwest res. ext. center" ]<-"southwest research and extension center"
    DATAFILE$INST[DATAFILE$INST== "st. lawrence centre"  ]<-"st lawrence centre" 
    DATAFILE$INST[DATAFILE$INST== "st. louis university" ]<-"st louis university"
    DATAFILE$INST[DATAFILE$INST== "stazione zoologica di napoli villa acquario" ]<-"stazione zoologica anton dohrn"
    DATAFILE$INST[DATAFILE$INST== "stazione zoologica di napoli villa comunale" ]<-"stazione zoologica anton dohrn"
    DATAFILE$INST[DATAFILE$INST== "stony brook university"	 ]<-"suny stony brook"
    DATAFILE$INST[DATAFILE$INST==  "suny albany"]<-"suny albany"
    DATAFILE$INST[DATAFILE$INST=="suny stony brook" ]<-"suny stony brook"
    DATAFILE$INST[DATAFILE$INST==  "binghamton university state university of new york"]<-"state university of new york binghamton"
    DATAFILE$INST[DATAFILE$INST== "suny" ]<-"state university of new york"
    DATAFILE$INST[DATAFILE$INST==  "swiss federal institute of technology zurich"]<-"swiss federal institute of technology"
    DATAFILE$INST[DATAFILE$INST== "eth zurich" ]<-"swiss federal institute of technology "
    DATAFILE$INST[DATAFILE$INST== "tall timber research station" ]<-"tall timbers research station"
    DATAFILE$INST[DATAFILE$INST== "tech univ darmstadt" ]<-"technische universitat darmstadt"
    DATAFILE$INST[DATAFILE$INST== "tufts" ]<-"tufts university"
    DATAFILE$INST[DATAFILE$INST==  "tuniversity of dresden"]<-"university of dresden"
    DATAFILE$INST[DATAFILE$INST==  "unc chapel hill"]<-"university of north carolina chapel hill"
    DATAFILE$INST[DATAFILE$INST== "universidad autonoma del estado hidalgo" ]<-"universidad autonoma del estado de hidalgo"
    DATAFILE$INST[DATAFILE$INST==  "university of london college imperial college of science"]<-"university of london imperial college of science technology and medicine" 
    DATAFILE$INST[DATAFILE$INST==  "university of london imperial college of science technology"]<-"university of london imperial college of science technology and medicine" 
    DATAFILE$INST[DATAFILE$INST==  "univ london imperial coll sci technol and med"]<-"imperial college of science technology and medicine"
    DATAFILE$INST[DATAFILE$INST== "universidad nacional aut<a2>noma de m<82>xico"  ]<-"universidad nacional autonoma de mexico" 
    DATAFILE$INST[DATAFILE$INST== "universidade de s?o paulo"  ]<-"universidade de sao paulo" 
    DATAFILE$INST[DATAFILE$INST== "universidade estadual paulista (unesp)"  ]<-"universidade estadual paulista" 
    DATAFILE$INST[DATAFILE$INST==  "universidade estadual de campinas (unicamp)"   ]<-"universidade estadual de campinas"   
    DATAFILE$INST[DATAFILE$INST== "universidade federal do rio grande do norte (university of floridarn)"  ]<-"universidade federal do rio grande do norte"
    DATAFILE$INST[DATAFILE$INST== "university fed goias"  ]<-"universidade federal de goias"
    DATAFILE$INST[DATAFILE$INST== "university of brasilia"  ]<-"universidade de brasilia"
    DATAFILE$INST[DATAFILE$INST== "universita die napoli \"frederico ii\""  ]<-"universita die napoli frederico ii" 
    DATAFILE$INST[DATAFILE$INST==  "universite de quebec a monteal" ]<-"universite de quebec a montreal" 
    DATAFILE$INST[DATAFILE$INST==  "university lyon 1 cnrs" ]<-"university lyon 1" 
    DATAFILE$INST[DATAFILE$INST== "universit<82> paris sud" ]<-"universite paris sud"
    DATAFILE$INST[DATAFILE$INST==  "univ. de sao paulo" ]<-"universidade de sao paulo" 
    DATAFILE$INST[DATAFILE$INST==  "university of sao paulo"]<-"universidade de sao paulo"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of notre dame"]<-"usa"
    
    DATAFILE$INST[DATAFILE$INST==  "university of east angolia"]<-"university of east anglia"
    DATAFILE$INST[DATAFILE$INST=="university of california santa diego"  ]<-"university of california san diego"
    DATAFILE$INST[DATAFILE$INST== "cnrs montellier" ]<-"centre decologie fonctionnelle et evolutive cnrs"
    DATAFILE$INST[DATAFILE$INST== "universite claude bernard lyons" ]<-"universite claude bernard lyon 1"
    DATAFILE$INST[DATAFILE$INST==  "royal botanical gardens kew"]<-"royal botanic gardens kew"
    DATAFILE$INST[DATAFILE$INST==  "royal botanic gardens" & DATAFILE$COUNTRY=="united kingdom"]<-"royal botanic gardens kew"
    DATAFILE$INST[DATAFILE$INST== "universite montpellier ii" ]<-"universite montpellier 2"
    DATAFILE$INST[DATAFILE$INST== "university of querzburg"  ]<-"university of wurzburg"
    DATAFILE$INST[DATAFILE$INST== "environmental protection agency" ]<-"us environmental protection agency"
    DATAFILE$INST[DATAFILE$INST== "univesity of sterling" ]<-"university of stirling"
    DATAFILE$INST[DATAFILE$INST== "university of sterling" ]<-"university of stirling"
    DATAFILE$INST[DATAFILE$INST== "universitat pompeuniversity of fabra" ]<-"universitat pompeu fabra"
    DATAFILE$INST[DATAFILE$INST== "university pompeuniversity of fabra" ]<-"universitat pompeu fabra"

    DATAFILE$INST[DATAFILE$INST== "field museuniversity of natural history" ]<-"field museum of natural history"
    DATAFILE$INST[DATAFILE$INST== "filed museuniversity of natural history" ]<-"field museum of natural history"
    DATAFILE$INST[DATAFILE$INST== "boyal botanic gatdens edinburgh" ]<-"royal botanic garden edinburgh"
    DATAFILE$INST[DATAFILE$INST== "university of of minnesota" ]<-"university of minnesota"
    DATAFILE$INST[DATAFILE$INST== "university of leichester" ]<-"university of leicester"
    DATAFILE$INST[DATAFILE$INST== "university pierre et marie curie" ]<-"universite pierre et marie curie"
    DATAFILE$INST[DATAFILE$INST== "instituto venezolano investigaciones cientificas" ]<-"instituto venezolano de investigaciones cientificas"
    DATAFILE$INST[DATAFILE$INST==  "universite de quebec a montreal" ]<-	"university du quebec a montreal"
    DATAFILE$INST[DATAFILE$INST== "istituto per l¬¢ambiente marino costiero" ]<-"instituto per lambiente marino costiero"
    DATAFILE$INST[DATAFILE$INST== "university illinois urbana champaign"	 ]<- "university of illinois urbana champaign"
    DATAFILE$INST[DATAFILE$INST== "universitat pompeu fabra"	 ]<- "university pompeu fabra"
    DATAFILE$INST[DATAFILE$INST== 	"universit√© de montreal" ]<- "universite de montreal"
    DATAFILE$INST[DATAFILE$INST==  "universite de montreal"	]<-"university of montreal"
    DATAFILE$INST[DATAFILE$INST== "universite of montreal" ]<-"university of montreal"
    DATAFILE$INST[DATAFILE$INST== 	"university of fribourg" ]<-"university of freiburg"
    DATAFILE$INST[DATAFILE$INST== "university british columbia"	 ]<-"university of british columbia"
    DATAFILE$INST[DATAFILE$INST== "swiss federal research institute wsl" ]<- "wsl swiss federal research institute"
    DATAFILE$INST[DATAFILE$INST== "swiss federal research institute" ]<- "wsl swiss federal research institute"
    DATAFILE$INST[DATAFILE$INST==  "university buenos aires"	]<- "university of buenos aires"
    DATAFILE$INST[DATAFILE$INST== "john innes centre" ]<-"john innes center"
    DATAFILE$INST[DATAFILE$INST== "universite de sherbrooke"	 ]<- "university of sherbrooke"
    DATAFILE$INST[DATAFILE$INST== "universite de paris sud"	 ]<- "universite paris sud"
    DATAFILE$INST[DATAFILE$INST== "university gottingen" ]<-"university of gottingen"
    DATAFILE$INST[DATAFILE$INST== "university groningen" ]<-"university of groningen"
    DATAFILE$INST[DATAFILE$INST== "university los andes" ]<- "universidad de los andes"
    DATAFILE$INST[DATAFILE$INST== "university of los andes" ]<-"universidad de los andes"
    DATAFILE$INST[DATAFILE$INST== "university tasmania"]<- "university of tasmania"
    DATAFILE$INST[DATAFILE$INST== "versity of tennessee "]<- "university of tennessee"
    DATAFILE$INST[DATAFILE$INST== "university of tennesse"]<- "university of tennessee"
    DATAFILE$INST[DATAFILE$INST== "versity of tennessee"]<- "university of tennessee"
    DATAFILE$INST[DATAFILE$INST== "university tennessee"]<- "university of tennessee"
    DATAFILE$INST[DATAFILE$INST== "netherlands institute for sea research" ]<-	"royal netherlands institute for sea research"
    DATAFILE$INST[DATAFILE$INST==  "university aberdeen" ]<-"university of aberdeen"
    DATAFILE$INST[DATAFILE$INST== "city university of new york" ]<- "cuny city university of new york"
    DATAFILE$INST[DATAFILE$INST== "institute of ecosystem studies"	& DATAFILE$COUNTRY=="usa" ]<- "cary institute of ecosystem studies"
    # DATAFILE$INST[DATAFILE$INST==  "university of college" ]<-university college  2x not colgne
    DATAFILE$INST[DATAFILE$INST== 	"waginen university" ]<-"wageningen university"
    DATAFILE$INST[DATAFILE$INST== "universit√© de montreal"	 ]<-"university of montreal"
    DATAFILE$INST[DATAFILE$INST== "state university of new york" ]<- "suny state university of new york"
    DATAFILE$INST[DATAFILE$INST== "university of nebraska at omaha" ]<-"university of nebraska omaha"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of nebraska omaha"]<-"usa"
    DATAFILE$INST[DATAFILE$INST== "university of nebraska" ]<-"university of nebraska lincoln"
    DATAFILE$INST[DATAFILE$INST== "unl" ]<-"university of nebraska lincoln"
    DATAFILE$INST[DATAFILE$INST=="noura ziadi"]<-"agriculture and agri food canada"
    DATAFILE$INST[DATAFILE$INST=="teagasc"]<-"agriculture and food development authority"
    DATAFILE$INST[DATAFILE$INST=="beaverlodge res farm"]<-"beaverlodge research farm"
    DATAFILE$INST[DATAFILE$INST=="service canadien des forets"]<-"canadian forest service"
    DATAFILE$INST[DATAFILE$INST=="center for international forestry research cifor"]<-"center for international forestry research"
    DATAFILE$COUNTRY[DATAFILE$INST=="shahid beheshti university"]<-"iran"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of hong kong"]<-"hong kong"
    DATAFILE$INST[DATAFILE$INST=="brazilian agricultural research corporation cirad"]<-"cirad-usp-esalq consortium"
    DATAFILE$INST[DATAFILE$INST=="centre decologie des ressources renouvelables"]<-"cnrs centre decologie des ressources renouvelables"
    DATAFILE$INST[DATAFILE$INST=="centre national de la recherche scientifique"]<-"cnrs centre national de la recherche scientifique"
    DATAFILE$INST[DATAFILE$INST=="cnrs"]<-"cnrs centre national de la recherche scientifique"
    DATAFILE$INST[DATAFILE$INST=="centre decologie fonctionnelle et evolutive cnrs"]<-"cnrs institut des sciences de levolution montpellier"
    DATAFILE$INST[DATAFILE$INST=="consejo nacional de investigaciones cientificas y tecnicas"]<-"conicet"
    DATAFILE$INST[DATAFILE$INST=="investigador conicet"]<-"conicet cct mendoza"
    DATAFILE$INST[DATAFILE$INST=="conicet"]<-"conicet consejo nacional de investigaciones cientificas y tecnicas"
    DATAFILE$INST[DATAFILE$INST=="centro nacional patagonico conicet"]<-"conicet centro nacional patagonico"
    DATAFILE$INST[DATAFILE$INST=="ibigeo conicet"]<-"conicet ibigeo"
    DATAFILE$INST[DATAFILE$INST=="copenhagen university"]<-"university of copenhagen"
    DATAFILE$INST[DATAFILE$INST=="university copenhagen"]<-"university of copenhagen"
    DATAFILE$INST[DATAFILE$INST=="university of pk"]<-"university of copenhagen"
    
    DATAFILE$INST[DATAFILE$INST=="cisc"]<-"csic consejo superior de investigaciones cientificas"
    DATAFILE$INST[DATAFILE$INST=="consejo superior de investigaciones cientificas"]<-"csic consejo superior de investigaciones cientificas"
    DATAFILE$INST[DATAFILE$INST=="estacion biologica donana"]<-"csic donana biological station"
    DATAFILE$INST[DATAFILE$INST=="brookhaven national laboratory"]<-"doe brookhaven national laboratory"
    DATAFILE$INST[DATAFILE$INST=="brookhaven national laboratory"]<-"doe brookhaven national laboratory"
    DATAFILE$INST[DATAFILE$INST=="brookhaven national laboratory"]<-"doe brookhaven national laboratory"
    DATAFILE$INST[DATAFILE$INST=="los alamos national laboratory"]<-"doe los alamos national laboratory"
    DATAFILE$INST[DATAFILE$INST=="oak ridge national laboratory"]<-"doe oak ridge national laboratory"
    DATAFILE$INST[DATAFILE$INST=="pacific northwest national laboratory"]<-"doe pacific northwest national laboratory"
    DATAFILE$INST[DATAFILE$INST=="forest research lnstitute malaysia (frim)"]<-"forest research lnstitute malaysia"
    DATAFILE$INST[DATAFILE$INST=="free university amsterdam"]<-"free university amsterdam"
    DATAFILE$INST[DATAFILE$INST=="institute of forestry and nature research"]<-"free university amsterdam"
    DATAFILE$INST[DATAFILE$INST=="french agricultural research centre for international development cirad"]<-"french agricultural research centre for international development"
    DATAFILE$INST[DATAFILE$INST=="national institute for agronomic research"]<-"french national institute for agricultural research"
    DATAFILE$INST[DATAFILE$INST=="helmholtz centre for environmental research ufz university of floridaz"]<-"helmholtz centre for environmental research"
    DATAFILE$INST[DATAFILE$INST=="instituto de recursos naturalesand agrobiologia"]<-"instituto de recursos naturales e agrobiologia"
    DATAFILE$INST[DATAFILE$INST=="imedea"]<-"instituto mediterraneo de estudios avanzados (imedea)"
    DATAFILE$INST[DATAFILE$INST=="istituto per l¬¢ambiente marino costiero"]<-"instituto per lambiente marino costiero"
    DATAFILE$UNIT[DATAFILE$INST=="centro de ecologia"]<-"centro de ecologia"
    DATAFILE$INST[DATAFILE$INST=="centro de ecologia"]<-"instituto venezolano de investigaciones cientificas"
    DATAFILE$INST[DATAFILE$INST=="venezuelan institute for scientific investigation"]<-"instituto venezolano de investigaciones cientificas"
    DATAFILE$INST[DATAFILE$INST=="venezuelan institute for scientific research"]<-"instituto venezolano de investigaciones cientificas"
    DATAFILE$INST[DATAFILE$INST=="international center for research in agroforestry icraf"]<-"international center for research in agroforestry"
    DATAFILE$INST[DATAFILE$INST=="macaulay¬†institute"]<-"macaulay land use research institute"
    DATAFILE$INST[DATAFILE$INST=="memorial uni of newfoundland"]<-"memorial university of newfoundland"
    DATAFILE$INST[DATAFILE$INST=="michigan technol univ"]<-"michigan technological university"
    DATAFILE$INST[DATAFILE$INST=="national natural history museum paris"]<-"national museum of natural history france"
    DATAFILE$INST[DATAFILE$INST=="northeast fisheries science center"]<-"noaa northeast fisheries science center"
    DATAFILE$INST[DATAFILE$INST=="catholic university of chile"]<-"pontificia universidad catolica de chile"
    DATAFILE$INST[DATAFILE$INST=="riso dtuniversity of national laboratory for sustainable energy"]<-"riso dtu national laboratory for sustainable energy"
    DATAFILE$INST[DATAFILE$INST=="rothamsted experimental station"]<-"rothamsted research"
    DATAFILE$INST[DATAFILE$INST=="nederlands institut voor onderzoek der zee"]<-"royal netherlands institute for sea research"
    DATAFILE$INST[DATAFILE$INST=="nederlands institut voor onderzoek der zee"]<-"royal netherlands institute for sea research"
    DATAFILE$INST[DATAFILE$INST=="french national institute for agricultural research"]<-"same as other ag inst?"
    DATAFILE$INST[DATAFILE$INST=="institut national de recherche en sciences et technologies pour lenvironnement et lagriculture"]<-"same as other ag inst?"
    DATAFILE$INST[DATAFILE$INST=="national museum of natural history smithsonian institution"]<-"smithsonian institution national museum of natural history"
    DATAFILE$INST[DATAFILE$INST=="station detudes des gorilles et chimpanzes (segc)"]<-"station detudes des gorilles et chimpanzes"
    DATAFILE$INST[DATAFILE$INST=="swedish agricultural university"]<-"swedish university of agricultural sciences"
    DATAFILE$INST[DATAFILE$INST=="university of tel aviv"]<-"tel aviv university"
    DATAFILE$INST[DATAFILE$INST=="tropical savannas management cooperative research centre and sustainable ecosystems"]<-"tropical savannas management cooperative research centre"
    DATAFILE$INST[DATAFILE$INST=="haus nr.9"]<-"unaffiliated "
    DATAFILE$INST[DATAFILE$INST=="university of buenos aires"]<-"universidad de buenos aires"
    DATAFILE$INST[DATAFILE$INST=="university of buenos aires and ifeva"]<-"universidad de buenos aires"
    DATAFILE$INST[DATAFILE$INST=="university of chile"]<-"universidad de chile"
    DATAFILE$INST[DATAFILE$INST=="university nacl autonoma mexico"]<-"universidad nacional autonoma de mexico"
    DATAFILE$INST[DATAFILE$INST=="instituto de biociencias"]<-"universidade de sao paulo"
    # DATAFILE$INST[DATAFILE$INST=="universidade de sao paulo"]<-"universidade estadual paulista"
    DATAFILE$INST[DATAFILE$INST=="icbs ufal and oxford university"]<-"universidade federal de alagoas"
    DATAFILE$INST[DATAFILE$INST=="universita di napoli"]<-"universita die napoli frederico ii"
    DATAFILE$INST[DATAFILE$INST=="university lyon 1"]<-"universite claude bernard lyon 1"
    DATAFILE$INST[DATAFILE$INST=="university paris sud 11"]<-"universite paris sud"
    DATAFILE$INST[DATAFILE$INST=="aberdeen university"]<-"university of aberdeen"
    DATAFILE$INST[DATAFILE$INST=="university of university amsterdam"]<-"university of amsterdam"
    DATAFILE$UNIT[DATAFILE$INST=="scripps institution of oceanography"]<-"scripps institution of oceanography"
    DATAFILE$INST[DATAFILE$INST=="scripps institution of oceanography"]<-"university of california san diego"
    DATAFILE$UNIT[DATAFILE$INST=="savannah river ecology laboratory"]<-"savannah river ecology laboratory"
    DATAFILE$INST[DATAFILE$INST=="savannah river ecology laboratory"]<-"university of georgia"
    DATAFILE$INST[DATAFILE$INST=="leiden university"]<-"university of leiden"
    DATAFILE$INST[DATAFILE$INST=="universit√© de montreal"]<-"university of montreal"
    DATAFILE$INST[DATAFILE$INST=="universiti sains"]<-"university of sains"
    DATAFILE$INST[DATAFILE$INST=="stellenbosch university"]<-"university of stellenbosch"
    DATAFILE$INST[DATAFILE$INST=="university of t_bingen"]<-"university of tubingen"
    # DATAFILE$INST[DATAFILE$INST=="university of t_bingen"]<-"university of tubingen"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of tubingen"]<-"germany"
    DATAFILE$INST[DATAFILE$INST=="umea university"]<-"university of umea"
    DATAFILE$INST[DATAFILE$INST=="us army"]<-"us army engineer research and development center"
    DATAFILE$INST[DATAFILE$INST=="usda us department of agriculture"]<-"usda"
    DATAFILE$INST[DATAFILE$INST=="usda ars"]<-"usda agricultural research service"
    DATAFILE$INST[DATAFILE$INST=="usda aphis"]<-"usda animal and plant health inspection service"
    DATAFILE$INST[DATAFILE$INST=="us arid land agricultural research center"]<-"usda arid land agricultural research center"
    DATAFILE$INST[DATAFILE$INST=="usda cooperative state research education and extension service (csrees)"]<-"usda cooperative state research education and extension service"
    DATAFILE$INST[DATAFILE$INST=="us environmental protection agency"]<-"usepa environmental protection agency"
    DATAFILE$INST[DATAFILE$INST=="us forest service"]<-"usfs"
    DATAFILE$INST[DATAFILE$INST=="usfs us forest service"]<-"usfs"
    # DATAFILE$INST[DATAFILE$INST=="usfs us forest service"]<-"usfs"
    DATAFILE$INST[DATAFILE$INST=="north central research station"]<-"usfs north central research station"
    DATAFILE$INST[DATAFILE$INST=="us forest service north central research station"]<-"usfs north central research station"
    DATAFILE$INST[DATAFILE$INST=="pacific s.w. research station us forest service"]<-"usfs pacific southest research station"
    DATAFILE$INST[DATAFILE$INST=="usfws alaska region"]<-"usfws alaska region"
    DATAFILE$INST[DATAFILE$INST=="usfws idaho fish and wildlife office"]<-"usfws idaho fish and wildlife office"
    DATAFILE$INST[DATAFILE$INST=="usfws national ecology research center"]<-"usfws national ecology research center"
    DATAFILE$INST[DATAFILE$INST=="usfws national wetlands research center"]<-"usgs national wetlands research center"
    DATAFILE$INST[DATAFILE$INST=="united state fish and wildlife service"]<-"usfws united state fish and wildlife service"
    DATAFILE$INST[DATAFILE$INST=="us geological survey"]<-"usgs"
    DATAFILE$INST[DATAFILE$INST=="us geological survey alaska science center"]<-"usgs alaska science center"
    DATAFILE$INST[DATAFILE$INST=="us geological survey and university of california santa barbara"]<-"usgs and university of california santa barbara"
    DATAFILE$INST[DATAFILE$INST=="us geological survey and university of miami"]<-"usgs and university of miami"
    DATAFILE$INST[DATAFILE$INST=="us geological survey biological resources division"]<-"usgs biological resources division"
    DATAFILE$INST[DATAFILE$INST=="us geological survey forest and rangeland ecosystem science center"]<-"usgs forest and rangeland ecosystem science center"
    DATAFILE$INST[DATAFILE$INST=="us geological survey forest and rangeland ecosystem science center"]<-"usgs forest and rangeland ecosystem science center"
    DATAFILE$INST[DATAFILE$INST=="us geological survey fort collins science center"]<-"usgs fort collins science center"
    DATAFILE$INST[DATAFILE$INST=="us geological survey great lakes science center"]<-"usgs great lakes science center"
    DATAFILE$INST[DATAFILE$INST=="us geological survey/nrii"]<-"usgs national resources inventory"
    DATAFILE$INST[DATAFILE$INST=="us geological survey northern prairie wildlife research center"]<-"usgs northern prairie wildlife research center"
    DATAFILE$INST[DATAFILE$INST=="usgs northern prairie wildlife research center"]<-"usgs northern prairie wildlife research center"
    DATAFILE$INST[DATAFILE$INST=="us geological survey patuxent wildlife research center"]<-"usgs patuxent wildlife research center"
    DATAFILE$INST[DATAFILE$INST=="us geological survey patuzent"]<-"usgs patuxent wildlife research center"
    DATAFILE$INST[DATAFILE$INST=="us geological survey us geological survey"]<-"usgs us geological survey"
    DATAFILE$INST[DATAFILE$INST=="us geological society"]<-"usgs western ecological research center"
    DATAFILE$INST[DATAFILE$INST=="us geological survey western ecological research center"]<-"usgs western ecological research center"
    DATAFILE$INST[DATAFILE$INST=="western fisheries research center"]<-"usgs western fisheries research center"
    DATAFILE$INST[DATAFILE$INST=="us geological survey wisconsin cooperative wildlife research unit"]<-"usgs wisconsin cooperative wildlife research unit"
    DATAFILE$INST[DATAFILE$INST=="usnps inventory and monitoring program"]<-"usnps inventory and monitoring program"
    DATAFILE$INST[DATAFILE$INST=="usonr naval research laboratory"]<-"usonr naval research laboratory"
    DATAFILE$INST[DATAFILE$INST=="virginia commonwealth u"]<-"virginia commonwealth university"
    DATAFILE$INST[DATAFILE$INST=="wageningen university and research centre"]<-"wageningen university and research"
    DATAFILE$INST[DATAFILE$INST=="washington university school of medicine"]<-"washington university in st louis"
    DATAFILE$INST[DATAFILE$INST=="wrcoclaw u"]<-"wrcoclaw university"
    DATAFILE$UNIT[DATAFILE$INST=="yale school of forestry and environmental studies"]<-"school of forestry and environmental studies"
    DATAFILE$INST[DATAFILE$INST=="yale school of forestry and environmental studies"]<-"yale university"
    DATAFILE$INST[DATAFILE$INST=="argonne national laboratory"]<-"doe argonne national laboratory"
    DATAFILE$INST[DATAFILE$INST=="international institute of tropical forestry"]<-"usfs international institute of tropical forestry"
    DATAFILE$INST[DATAFILE$INST=="university connecticut"]<-"university of connecticut"
    DATAFILE$INST[DATAFILE$INST=="universite de lausanne"]<-"university of lausanne"
    DATAFILE$INST[DATAFILE$INST=="nc state university"]<-"north carolina state university"
    DATAFILE$INST[DATAFILE$INST=="alaska science center"]<-"usgs alaska science center"
    DATAFILE$INST[DATAFILE$INST=="uga"]<-"university of georgia"
    DATAFILE$INST[DATAFILE$INST=="uva"]<-"university of virginia"
    
    # DATAFILE$UNIT[DATAFILE$INST=="centro de ecologia"]<-"centro de ecologia"
    # DATAFILE$UNIT[DATAFILE$INST=="yale school of forestry and environmental studies"]<-"school of forestry and environmental studies"
    DATAFILE$UNIT[DATAFILE$INST=="washington university school of medicine"]<-"school of medicine"
    DATAFILE$INST_CHECK[DATAFILE$INST=="us geological survey and university of miami"]<-"2x primary inst"
    DATAFILE$UNIT[DATAFILE$INST=="savannah river ecology laboratory"]<-"savannah river ecology laboratory"
    DATAFILE$NOTES[DATAFILE$INST=="haus nr.9"]<-"max-planck institute for ornithology in a journal article from this time"
    DATAFILE$UNIT[DATAFILE$INST=="scripps institution of oceanography"]<-"scripps institution of oceanography"
    # DATAFILE$UNIT[DATAFILE$INST=="scripps institute of oceanography"]<-"scripps institution of oceanography"
    # DATAFILE$UNIT[DATAFILE$INST=="savannah river ecology laboratory"]<-"savannah river ecology laboratory"
    
    DATAFILE$UNIT[DATAFILE$INST=="univeristy of leiden"]<-"university of leiden"
    
    gsub("us geological survey","usgs",DATAFILE$INST)
    gsub("us forest service","usfs",DATAFILE$INST)
    gsub("us department of agriculture","usda",DATAFILE$INST)

    
    DATAFILE$INST[DATAFILE$INST=="(retired) natural history museum london"]<-"retired"
    DATAFILE$INST[DATAFILE$INST=="agricultural university wageningen"]<-"wageningen agricultural university"
    DATAFILE$INST[DATAFILE$INST=="alberta"]<-"university of alberta"
    DATAFILE$COUNTRY[DATAFILE$INST=="alexandra university"]<-"egypt"
    DATAFILE$COUNTRY[DATAFILE$INST=="alfred wegener institut fur polar und meeresforschung"]<-"germany"
    DATAFILE$COUNTRY[DATAFILE$INST=="arete associates"]<-"usa"
    DATAFILE$UNIT[DATAFILE$INST=="arnold arboretum of harvard university"]<-"arnold arboretum"
    DATAFILE$INST[DATAFILE$INST=="arnold arboretum of harvard university"]<-"harvard university"
    DATAFILE$INST[DATAFILE$INST=="australian commonwealth scientific and research organization"]<-"csiro commonwealth scientific and industrial research organisation"
    DATAFILE$COUNTRY[DATAFILE$INST=="australian national university"]<-"australia"
    DATAFILE$INST[DATAFILE$INST=="basf plant science"]<-"basf"
    DATAFILE$COUNTRY[DATAFILE$INST=="biological centre"]<-"netherlands"
    DATAFILE$COUNTRY[DATAFILE$INST=="biological institute"]<-"yugoslavia"
    DATAFILE$INST[DATAFILE$INST=="bristol university"]<-"university of bristol"
    DATAFILE$COUNTRY[DATAFILE$INST=="british antarctic survey"]<-"united kingdom"
    DATAFILE$INST[DATAFILE$INST=="calyx, inc"]<-"calyx inc"
    DATAFILE$INST[DATAFILE$INST=="university of cardiff"]<-"cardiff university"
    DATAFILE$COUNTRY[DATAFILE$INST=="carl von ossietzky university oldenburg"]<-"germany"
    DATAFILE$INST[DATAFILE$INST=="catholic university of chile"]<-"pontificia universidad catolica de chile"
    DATAFILE$UNIT[DATAFILE$INST=="center of marine sciences of algarve universidade"]<-"center of marine sciences"
    DATAFILE$INST[DATAFILE$INST=="center of marine sciences of algarve universidade"]<-"universidade do algarve"
    DATAFILE$COUNTRY[DATAFILE$INST=="universidade do algarve"]<-"portugal"
    DATAFILE$INST[DATAFILE$INST=="chris britton consultancy balfor beatty"]<-"chris britton consultancy"
    DATAFILE$COUNTRY[DATAFILE$INST=="christian albrechts universitat kiel"]<-"germany"
    DATAFILE$COUNTRY[DATAFILE$INST=="cinvestav"]<-"mexico"
    DATAFILE$INST[DATAFILE$INST=="ciudad universitaria"]<-"ets ingenieros de montes"
    DATAFILE$COUNTRY[DATAFILE$INST=="cnrs centre decologie fonctionnelle et evolutive"]<-"france"
    DATAFILE$INST[DATAFILE$INST=="college of staten island cuny"]<-"cuny college of staten island"
    DATAFILE$INST[DATAFILE$INST=="colorado cooperative fish and wildlife unit"]<-"usgs colorado cooperative fish and wildlife unit"
    DATAFILE$INST[DATAFILE$INST=="commonwealth university"]<-"virginia commonwealth university"
    DATAFILE$INST[DATAFILE$INST=="csic institut mediterrani destudis avencats"]<-"mediterranean institute for advanced studies"
    DATAFILE$INST[DATAFILE$INST=="csic institut mediterrani destudis avencats"]<-"mediterranean institute for advanced studies"
    DATAFILE$INST[DATAFILE$INST=="csic instituto de ciencias del mar"]<-"mediterranean institute for advanced studies"
    DATAFILE$COUNTRY[DATAFILE$INST=="doe brookhaven national laboratory"]<-"usa"
    DATAFILE$COUNTRY[DATAFILE$INST=="doe brookhaven national laboratory"]<-"usa"
    DATAFILE$INST[DATAFILE$INST=="environment canada wildlife research division"]<-"environment canada wildlife research east"
    DATAFILE$INST[DATAFILE$INST=="evolutionary biology centre"]<-"university of uppsala"
    DATAFILE$COUNTRY[DATAFILE$INST=="florida dept of natural resources bureau of marine research"]<-"usa"
    DATAFILE$INST[DATAFILE$INST=="forschungsinstitut senckenberg"]<-"senckenberg research institute"
    DATAFILE$INST[DATAFILE$INST=="gorgan university of agric sci"]<-"gorgan university of agricultural sciences"
    DATAFILE$COUNTRY[DATAFILE$INST=="griffith university"]<-"australia"
    DATAFILE$COUNTRY[DATAFILE$INST=="haus nr9"]<-"germany"
    DATAFILE$COUNTRY[DATAFILE$INST=="hebrew university of jerusalem"]<-"israel"
    DATAFILE$COUNTRY[DATAFILE$INST=="hokkaido university"]<-"japan"
    DATAFILE$COUNTRY[DATAFILE$INST=="humboldt university of berlin"]<-"germany"
    DATAFILE$INST[DATAFILE$INST=="hunter college cuny"]<-"cuny hunter college"
    DATAFILE$COUNTRY[DATAFILE$INST=="icbs ufal and oxford university"]<-"brazil"
    DATAFILE$INST[DATAFILE$INST=="icbs ufal and oxford university"]<-"universidade federal de alagoas"
    DATAFILE$INST[DATAFILE$INST=="inra"]<-"french national institute for agricultural research inra"
    DATAFILE$COUNTRY[DATAFILE$INST=="institut national scientifique et technnique d oceanographie et de peche"]<-"tunisia"
    DATAFILE$COUNTRY[DATAFILE$INST=="institute of marine biology of crete"]<-"greece"
    DATAFILE$INST[DATAFILE$INST=="institute of tropical forestry"]<-"usfs international institute of tropical forestry"
    DATAFILE$INST[DATAFILE$INST=="instituto de biociencias"]<-"universidade de sao paulo"
    DATAFILE$INST[DATAFILE$INST=="instituto mediterraneo de estudios avanzados (imedea)"]<-"mediterranean institute for advanced studies"
    DATAFILE$COUNTRY[DATAFILE$INST=="international institute for applied systems analysis"]<-"austria"
    DATAFILE$COUNTRY[DATAFILE$INST=="international institute of tropical forestry"]<-"puerto rico"
    DATAFILE$INST[DATAFILE$INST=="istituto per l¬¢ambiente marino costiero"]<-"instituto per lambiente marino costiero"
    DATAFILE$INST[DATAFILE$INST=="istituto per l¢ambiente marino costiero"]<-"instituto per lambiente marino costiero"
    DATAFILE$COUNTRY[DATAFILE$INST=="james cook university"]<-"australia"
    DATAFILE$COUNTRY[DATAFILE$INST=="james cook university"]<-"australia"
    DATAFILE$INST[DATAFILE$INST=="kings college"]<-"kings college london"
    DATAFILE$COUNTRY[DATAFILE$INST=="kyushu university"]<-"japan"
    DATAFILE$COUNTRY[DATAFILE$INST=="laboratorio di geologia marina del cnr"]<-"italy"
    DATAFILE$COUNTRY[DATAFILE$INST=="lakehead university"]<-"canada"
    DATAFILE$INST[DATAFILE$INST=="lancaster"]<-"lancaster university"
    DATAFILE$COUNTRY[DATAFILE$INST=="lancaster university"]<-"united kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="leibniz institut fur meereswissenschaften ifmgeomar"]<-"germany"
    # DATAFILE$INST[DATAFILE$INST=="lincoln university"]<-"university of lincoln"
    DATAFILE$INST[DATAFILE$INST=="lincoln college"]<-"lincoln university"
    # DATAFILE$COUNTRY[DATAFILE$INST=="university of lincoln"]<-"united kingdom"
    # DATAFILE$COUNTRY[DATAFILE$INST=="lincoln university"]<-"new zealand"
    DATAFILE$INST[DATAFILE$INST=="ludwig maximilian universitat munchen"]<-"ludwig maximilian university of munich"
    DATAFILE$COUNTRY[DATAFILE$INST=="luminy universite d aix marseille"]<-"france"
    DATAFILE$INST[DATAFILE$INST=="macaulay¬†institute"]<-"macaulay land use research institute"
    DATAFILE$COUNTRY[DATAFILE$INST=="max planck institute for behavioral physiology"]<-"germany"
    DATAFILE$COUNTRY[DATAFILE$INST=="max planck institute for demographic research"]<-"germany"
    DATAFILE$COUNTRY[DATAFILE$INST=="mcgill university"]<-"canada"
    DATAFILE$INST[DATAFILE$INST=="meggers"]<-"smithsonian national museum of natural history"
    DATAFILE$COUNTRY[DATAFILE$INST=="memorial university of newfoundland"]<-"canada"
    DATAFILE$INST[DATAFILE$INST=="millbrook"]<-"cary institute of ecosystem studies"
    DATAFILE$COUNTRY[DATAFILE$INST=="monterey bay aquarium research institute"]<-"usa"
    DATAFILE$INST[DATAFILE$INST=="mpi jena"]<-"max planck institute for human history"
    DATAFILE$INST[DATAFILE$INST=="national institute of food and agriculture"]<-"usda national institute of food and agriculture"
    DATAFILE$COUNTRY[DATAFILE$INST=="national institute of water and atmospheric research"]<-"new zealand"
    DATAFILE$COUNTRY[DATAFILE$INST=="national museum of natural history france"]<-"france"
    DATAFILE$INST[DATAFILE$INST=="national museum of natural history smithsonian institution"]<-"smithsonian national museum of natural history"
    DATAFILE$INST[DATAFILE$INST=="national natural history museum paris"]<-"national museum of natural history france"
    DATAFILE$COUNTRY[DATAFILE$INST=="national university of rosario"]<-"argentina"
    DATAFILE$COUNTRY[DATAFILE$INST=="naturhistorisches museum wien"]<-"austria"
    DATAFILE$INST[DATAFILE$INST=="nederlands institut voor onderzoek der zee"]<-"royal netherlands institute for sea research"
    DATAFILE$INST[DATAFILE$INST=="nederlands institut voor onderzoek der zee"]<-"royal netherlands institute for sea research"
    DATAFILE$COUNTRY[DATAFILE$INST=="new york botanical garden"]<-"usa"
    DATAFILE$INST[DATAFILE$INST=="newfoundland"]<-"missing"
    DATAFILE$INST[DATAFILE$INST=="nioo"]<-"netherlands institute of ecology"
    # DATAFILE$NOTES[DATAFILE$INST=="no one by this name"]<-"2x if on ed board this year"
    DATAFILE$INST[DATAFILE$INST=="northeast fisheries science center"]<-"noaa northeast fisheries science center"
    DATAFILE$INST[DATAFILE$INST=="oklahoma university"]<-"university of oklahoma"
    DATAFILE$COUNTRY[DATAFILE$INST=="oxford university"]<-"united kingdom"
    DATAFILE$INST[DATAFILE$INST=="pacific sw research station us forest service"]<-"usfs pacific sw research station"
    DATAFILE$INST[DATAFILE$INST=="prairie wildlife research center"]<-"usgs prairie wildlife research center"
    DATAFILE$COUNTRY[DATAFILE$STATE=="puerto rico"]<-"puerto rico"
    DATAFILE$INST[DATAFILE$INST=="queens college city university of new york"]<-"cuny queens college"
    DATAFILE$INST[DATAFILE$INST=="sao paulo state university"]<-"universidade estadual paulista"
    DATAFILE$INST[DATAFILE$INST=="retired (no affiliation)"]<-"retired"
    DATAFILE$COUNTRY[DATAFILE$INST=="rice university"]<-"usa"
    DATAFILE$INST[DATAFILE$INST=="riso dtuniversity of national laboratory for sustainable energy"]<-"riso dtu national laboratory for sustainable energy"
    DATAFILE$COUNTRY[DATAFILE$INST=="royal botanic gardens kew"]<-"united kingdom"
    DATAFILE$INST[DATAFILE$INST=="royal botanic gardens melbourne university of melbourne"]<-"royal botanic gardens melbourne"
    DATAFILE$INST[DATAFILE$LAST_NAME=="rutzler"]<-"smithsonian national museum of natural history"
    DATAFILE$INST[DATAFILE$INST=="senckenberg research institute frankfurt"]<-"senckenberg research institute"
    DATAFILE$UNIT[DATAFILE$INST=="southwest research and extension center"]<-"southwest research and extension center"
    DATAFILE$INST[DATAFILE$INST=="southwest research and extension center"]<-"university of arkansas"
    DATAFILE$INST[DATAFILE$INST=="st de pytosociologie fondamental"]<-"centre regional de phytosociologie"
    DATAFILE$INST[DATAFILE$INST=="state university of new york albany"]<-"suny albany"
    DATAFILE$INST[DATAFILE$INST=="state university of new york binghamton"]<-"suny binghamton"
    DATAFILE$INST[DATAFILE$INST=="state university of new york college of environmental science and forestry"]<-"suny college of environmental science and forestry"
    DATAFILE$INST[DATAFILE$INST=="state university of new york state university of new york"]<-"suny state university of new york"
    DATAFILE$INST[DATAFILE$INST=="state university of new york stony brook"]<-"suny stony brook"
    DATAFILE$INST[DATAFILE$INST=="station detudes des gorilles et chimpanzes (segc)"]<-"station detudes des gorilles et chimpanzes"
    DATAFILE$INST[DATAFILE$INST=="swedish agricultural university"]<-"swedish university of agricultural sciences"
    DATAFILE$INST[DATAFILE$INST=="swedish university of agricultural science"]<-"swedish university of agricultural sciences"
    DATAFILE$INST[DATAFILE$INST=="swiss federal institute wsl"]<-"wsl swiss federal institute for forest snow and landscape research"
    DATAFILE$UNIT[DATAFILE$INST=="tidewater agricultural res and ext ctr"]<-"tidewater agricultural res and ext ctr"
    DATAFILE$INST[DATAFILE$INST=="tidewater agricultural res and ext ctr"]<-"virginia polytechnic institute and state university"
    DATAFILE$COUNTRY[DATAFILE$INST=="tijuana estuarine research reserve"]<-"usa"
    DATAFILE$INST[DATAFILE$INST=="tulane"]<-"tulane university"
    DATAFILE$INST[DATAFILE$INST=="ufz center for environmental research"]<-"helmholtz centre for environmental research ufz"
    DATAFILE$INST[DATAFILE$INST=="ufz centre for environmental research leipzig halle"]<-"helmholtz centre for environmental research ufz leipzig halle"
    DATAFILE$INST[DATAFILE$INST=="ufz helmholtz ctr environm res"]<-"helmholtz centre for environmental research ufz"
    DATAFILE$INST[DATAFILE$INST=="universidade federal do rio grande do norte (ufrn)"]<-"universidade federal do rio grande do norte"
    DATAFILE$COUNTRY[DATAFILE$INST=="universita degli studi di padova"]<-"italy"
    DATAFILE$COUNTRY[DATAFILE$INST=="universita di napoli"]<-"italy"
    DATAFILE$COUNTRY[DATAFILE$INST=="universita di pisa"]<-"italy"
    DATAFILE$COUNTRY[DATAFILE$INST=="universita die napoli frederico ii"]<-"italy"
    DATAFILE$COUNTRY[DATAFILE$INST=="universite de lausanne"]<-"switzerland"
    DATAFILE$COUNTRY[DATAFILE$INST=="universite laval quebec"]<-"canada"
    DATAFILE$COUNTRY[DATAFILE$INST=="university college dublin"]<-"ireland"
    DATAFILE$COUNTRY[DATAFILE$INST=="university college london"]<-"united kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="university marine biological station millport"]<-"united kingdom"
    DATAFILE$INST[DATAFILE$INST=="university of illinois university urbana champaign"]<-"university of illinois urbana champaign"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of kentucky"]<-"usa"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of leiden"]<-"netherlands"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of maryland center for environmental science"]<-"usa"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of oregon"]<-"usa"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of oslo"]<-"norway"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of pennsylvania"]<-"usa"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of puerto rico"]<-"puerto rico"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of regina"]<-"canada"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of sheffield"]<-"united kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of sherbrooke"]<-"canada"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of southampton"]<-"united kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of stockholm"]<-"sweden"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of sydney"]<-"australia"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of texas austin"]<-"usa"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of the sunshine"]<-"australia"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of toronto"]<-"canada"
    DATAFILE$INST[DATAFILE$INST=="university of university amsterdam"]<-"university of amsterdam"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of vienna"]<-"austria"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of waikato"]<-"new zealand"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of western ontario"]<-"canada"
    DATAFILE$INST[DATAFILE$INST=="university of zuidema"]<-"university of wageningen"
    DATAFILE$COUNTRY[DATAFILE$INST=="university pompeu fabra"]<-"spain"
    DATAFILE$COUNTRY[DATAFILE$INST=="university system of maryland"]<-"usa"
    DATAFILE$INST[DATAFILE$INST=="uppsala university"]<-"university of uppsala"
    DATAFILE$INST[DATAFILE$INST=="us arid land agricultural research center"]<-"usda arid land agricultural research center"
    DATAFILE$INST[DATAFILE$INST=="us geological society"]<-"us geological survey"
    DATAFILE$INST[DATAFILE$INST=="virginia commonwealth u"]<-"virginia commonwealth university"
    DATAFILE$INST[DATAFILE$INST=="biological station of donana"]<-"csic donana biological station"
    DATAFILE$INST[DATAFILE$INST=="food and agriculture organization fao"]<-"fao food and agriculture organization"
    DATAFILE$INST[DATAFILE$INST=="imperial college of london"]<-"imperial college london"
    DATAFILE$INST[DATAFILE$INST=="institute¬†of terrestrial ecology"]<-"institute of terrestrial ecology"
    DATAFILE$INST[DATAFILE$INST=="joint nature conservation committee¬†"]<-"joint nature conservation committee"
    DATAFILE$INST[DATAFILE$INST=="king‚äôs college"]<-"kings college"
    DATAFILE$INST[DATAFILE$INST=="macaulay¬†institute"]<-"macaulay land use research institute"
    DATAFILE$INST[DATAFILE$INST=="ceh"]<-"nerc centre for ecology and hydrology"
    DATAFILE$INST[DATAFILE$INST=="nerc ctr ecol and hydrol"]<-"nerc centre for ecology and hydrology"
    DATAFILE$INST[DATAFILE$INST=="ceh banchory"]<-"nerc centre for ecology and hydrology banchory"
    DATAFILE$INST[DATAFILE$INST=="ceh monks wood"]<-"nerc centre for ecology and hydrology monks wood"
    DATAFILE$INST[DATAFILE$INST=="netherlands inst ecol nioo knaw"]<-"netherlands institute of ecology nioo knaw"
    DATAFILE$INST[DATAFILE$INST=="northeastern univ"]<-"northeastern university"
    DATAFILE$NOTES[DATAFILE$INST=="western cotton research lab"]<-"formerly the western cotton research lab"
    DATAFILE$INST[DATAFILE$INST=="western cotton research lab"]<-"usda arid land agricultural research center"
    DATAFILE$INST[DATAFILE$INST=="oregon state univ"]<-"oregon state university"
    DATAFILE$INST[DATAFILE$INST=="penn state"]<-"pennsylvania state university"
    DATAFILE$INST[DATAFILE$INST=="sw texas state university"]<-"southwest texas state university"
    DATAFILE$INST[DATAFILE$INST=="tech university darmstadt"]<-"technical university of darmstadt"
    DATAFILE$INST[DATAFILE$INST=="technische universitat darmstadt"]<-"technical university of darmstadt"
    DATAFILE$INST[DATAFILE$INST=="truman state"]<-"truman state university"
    DATAFILE$INST[DATAFILE$INST=="university of alicante"]<-"universidad de alicante"
    DATAFILE$INST[DATAFILE$INST=="university of vigo"]<-"universidad de vigo"
    DATAFILE$INST[DATAFILE$INST=="university of rey juan carlos"]<-"universidad rey juan carlos"
    DATAFILE$INST[DATAFILE$INST=="universitat osnabr√ºck"]<-"universitat osnabruck"
    DATAFILE$INST[DATAFILE$INST=="universidad de costa rica"]<-"university of costa rica"
    DATAFILE$INST[DATAFILE$INST=="university exeter"]<-"university of exeter"
    DATAFILE$INST[DATAFILE$INST=="university glasgow"]<-"university of glasgow"
    DATAFILE$INST[DATAFILE$INST=="university of natal"]<-"university of kwazulu natal"
    DATAFILE$INST[DATAFILE$INST=="university of kwazuluniversity of natal"]<-"university of kwazulu natal"
    DATAFILE$UNIT[DATAFILE$INST=="arnold arboretuniversity of harvard university"]<-"arnold arboretum"
    DATAFILE$INST[DATAFILE$INST=="arnold arboretuniversity of harvard university"]<-"harvard university"
    
    DATAFILE$INST[DATAFILE$INST=="university of london college imperial college of science and technology"]<-"university of london imperial college of science and technology"
    DATAFILE$INST[DATAFILE$INST=="university of london imperial college of science and technology"]<-"university of london imperial college of science and technology"
    DATAFILE$INST[DATAFILE$INST=="universit√© de montreal"]<-"university of montreal"
    DATAFILE$INST[DATAFILE$INST=="university oregon"]<-"university of oregon"
    DATAFILE$INST[DATAFILE$INST=="oxford university"]<-"university of oxford"
    DATAFILE$INST[DATAFILE$INST=="university oxford"]<-"university of oxford"
    DATAFILE$INST[DATAFILE$INST=="universite du quebec"]<-"university of quebec"
    DATAFILE$INST[DATAFILE$INST=="universita del salento"]<-"university of salento"
    DATAFILE$INST[DATAFILE$INST=="umea univ"]<-"university of umea"
    DATAFILE$INST[DATAFILE$INST=="university¬†of¬†wageningen"]<-"university of wageningen"
    DATAFILE$INST[DATAFILE$INST=="waikato university"]<-"university of waikato"
    DATAFILE$INST[DATAFILE$INST=="utrecht"]<-"utrecht university"
    DATAFILE$INST[DATAFILE$INST=="wageningen university research center alterra"]<-"wageningen agricultural university and research center alterra"
    DATAFILE$INST[DATAFILE$INST=="retired"]<-"unaffiliated"
    DATAFILE$INST[DATAFILE$INST=="st andrews"]<-"university of st andrews"
    DATAFILE$INST[DATAFILE$INST=="university of ft hare"]<-"university of fort hare"
   
    
    
    DATAFILE$INST[DATAFILE$INST=="göttingen"]<-"university of gottingen"
    DATAFILE$INST[DATAFILE$INST=="jena"]<-"max planck institute of biogeochemistry"
    DATAFILE$INST[DATAFILE$INST=="peking"]<-"peking university"
    DATAFILE$INST[DATAFILE$INST=="santiago"]<-"pontificia universidad catolica de chile"
    DATAFILE$INST[DATAFILE$INST=="2x university of munster"]<-"university of munster"
    DATAFILE$INST[DATAFILE$INST=="regina"]<-"university of regina"
    DATAFILE$INST[DATAFILE$INST=="guelph"]<-"university of guelph"
    DATAFILE$INST[DATAFILE$INST=="sackville"]<-"mount allison university"
    DATAFILE$INST[DATAFILE$INST=="saskatchewan"]<-"university of saskatchewan"
    DATAFILE$INST[DATAFILE$INST=="toronto"]<-"university of toronto mississauga"
    DATAFILE$INST[DATAFILE$INST=="vancouver"]<-"university of british columbia"
    
    DATAFILE$INST[DATAFILE$INST=="montpellier"]<-"cnrs centre decologie fonctionnelle et evolutive"
    DATAFILE$INST[DATAFILE$INST=="cnrs centre national de la recherche scientifique" & DATAFILE$CITY=="montpellier"]<-"cnrs centre decologie fonctionnelle et evolutive"
    DATAFILE$INST[DATAFILE$INST=="cnrs centre national de la recherche scientifique" & DATAFILE$UNIT=="centre d'ecologie fonctionnelle et evolutive"]<-"cnrs centre decologie fonctionnelle et evolutive"
    DATAFILE$INST[DATAFILE$INST=="cnrs centre national de la recherche scientifique" & DATAFILE$UNIT=="ctr ecol fonct & evolut"]<-"cnrs centre decologie fonctionnelle et evolutive"
    
    
    
    
    DATAFILE$COUNTRY[DATAFILE$INST=="university of aberdeen"]<-"united kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of st andrews"]<-"united kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of freiburg"]<-"germany"
    DATAFILE$COUNTRY[DATAFILE$INST=="texas a and m university"]<-"usa"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of arkansas"]<-"usa"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of arkansas little rock"]<-"usa"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of oxford" & DATAFILE$editor_id==2823]<-"united kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of oxford" & DATAFILE$editor_id==3148]<-"united kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of oxford" & DATAFILE$year==2005 &DATAFILE$journal=="jape" ]<-"united kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of cape town"& DATAFILE$editor_id==2153]<-"south africa"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of innsbruck"]<-"austria"
    DATAFILE$COUNTRY[DATAFILE$INST=="cinvestav irapuato"]<-"mexico"
    DATAFILE$STATE[DATAFILE$INST=="cinvestav irapuato"]<-"guanajuato"
    DATAFILE$CITY[DATAFILE$INST=="cinvestav irapuato"]<-"irapuato"
    DATAFILE$CITY[DATAFILE$CITY=="basal"]<-"basel"
    
    DATAFILE$COUNTRY[DATAFILE$INST=="queen mary university of london"]<-"united kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="swedish university of agricultural sciences"]<-"sweden"
    DATAFILE$COUNTRY[DATAFILE$INST=="vrije universiteit amsterdam"]<-"netherlands"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of western australia"]<-"australia"
    
    DATAFILE$COUNTRY[DATAFILE$COUNTRY=="brazil and uk"]<-"brazil"
    

    DATAFILE$INST[DATAFILE$INST=="innsbruck"]<-"university of innsbruck"
    DATAFILE$INST[DATAFILE$INST=="free university amsterdam"]<-"vrije universiteit amsterdam"
    DATAFILE$INST[DATAFILE$INST=="california state university" & DATAFILE$CITY=="fresno"]<-"california state university fresno"
    DATAFILE$INST[DATAFILE$INST=="california state university" & DATAFILE$CITY=="sacramento" ]<-"california state university sacramento"
    DATAFILE$INST[DATAFILE$INST== "university of floridaz center for environmental research" ]<-"helmholtz centre for environmental research"
    
    
    DATAFILE$INST[DATAFILE$INST== "csiro commonwealth scientific and industrial research organisation" & 
                    DATAFILE$UNIT=="division of plant industry"]<-"csiro plant industry"
    DATAFILE$INST[DATAFILE$INST== "csiro commonwealth scientific and industrial research organisation" & 
                    DATAFILE$UNIT=="division of wildlife and ecology"]<-"csiro wildlife and ecology"
    DATAFILE$INST[DATAFILE$INST== "csiro commonwealth scientific and industrial research organisation" & 
                    DATAFILE$UNIT=="division of forest research"]<-"csiro forest research"
    
    DATAFILE$INST[DATAFILE$INST== "csiro division of wildlife and ecology"]<-"csiro wildlife and ecology"
    
    DATAFILE$COUNTRY[DATAFILE$INST=="usfs"]<-"usa"
    
    DATAFILE$INST[DATAFILE$INST=="usfs"]<-"usfs us forest service"
    
    DATAFILE$INST[DATAFILE$INST=="usgs"]<-"usfs us geological survey"
    DATAFILE$INST[DATAFILE$INST=="usda"]<-"usda us department of agriculture"
    DATAFILE$INST[DATAFILE$INST=="vigo"]<-"university of vigo"
    DATAFILE$INST[DATAFILE$INST=="christchurch"]<-"dsir land resources"
    DATAFILE$INST[DATAFILE$INST=="cork"]<-"university college cork"
    DATAFILE$INST[DATAFILE$INST=="krakow"]<-"jagiellonian university"
    DATAFILE$INST[DATAFILE$INST=="ulster"]<-"university of ulster"
    DATAFILE$INST[DATAFILE$INST=="university of ulm"]<-"ulm university"
    DATAFILE$INST[DATAFILE$INST=="ulm"]<-"ulm university"
    DATAFILE$INST[DATAFILE$INST=="minnesota"]<-"university of st thomas"
    DATAFILE$INST[DATAFILE$INST=="montana"]<-"university of montana"
    DATAFILE$INST[DATAFILE$INST=="maine"]<-"university of southern maine"
    DATAFILE$INST[DATAFILE$INST=="franklin and marshall university"]<-"franklin and marshall"
    DATAFILE$INST[DATAFILE$INST=="pretoria"]<-"university of pretoria"
    DATAFILE$INST[DATAFILE$INST=="tennessee"]<-"university of tennessee"
    DATAFILE$INST[DATAFILE$INST=="salzburg"]<-"university of salzburg"
    DATAFILE$INST[DATAFILE$INST=="sydney"]<-"university of sydney"
    DATAFILE$INST[DATAFILE$INST=="university of western sydney"]<-"western sydney university"
    DATAFILE$INST[DATAFILE$INST=="york"]<-"university of york"
    DATAFILE$INST[DATAFILE$INST=="london"]<-"queen mary university of london"
    DATAFILE$INST[DATAFILE$INST=="queen’s university"]<-"queens university"
    DATAFILE$INST[DATAFILE$INST=="queens university kingston"]<-"queens university"
    DATAFILE$INST[DATAFILE$INST=="zoological society"]<-"zoological society of london"
    
    
    DATAFILE$INST[DATAFILE$INST=="kyoto"]<-"kyoto university"
    DATAFILE$INST[DATAFILE$INST=="hokkaido"]<-"hokkaido university"
    DATAFILE$INST[DATAFILE$INST=="swansea"]<-"swansea university"
    DATAFILE$INST[DATAFILE$INST=="dundee"]<-"university of dundee"
    DATAFILE$INST[DATAFILE$INST=="reading"]<-"university of reading"
    DATAFILE$INST[DATAFILE$INST=="dartmouth"]<-"dartmouth college"
    DATAFILE$INST[DATAFILE$INST=="monash"]<-"monash university"
    DATAFILE$INST[DATAFILE$INST=="syracuse"]<-"suny college of environmental science and forestry"
    
    DATAFILE$INST[DATAFILE$INST=="stockholm university"]<-"university of stockholm"
    DATAFILE$INST[DATAFILE$INST=="university of utrecht"]<-"utrecht university"
    DATAFILE$INST[DATAFILE$INST=="madrid"]<-"csic national museum of natural sciences"
    DATAFILE$INST[DATAFILE$UNIT=="estacion experimental de zonas aridas"]<-"csic estacion experimental de zonas aridas"
    DATAFILE$INST[DATAFILE$INST=="csic"]<-"csic consejo superior de investigaciones cientificas"
    DATAFILE$INST[DATAFILE$INST=="cifor"]<-"cifor center for international forestry research"
    DATAFILE$INST[DATAFILE$INST=="center for international forestry research"]<-"cifor center for international forestry research"
    
    DATAFILE$INST[DATAFILE$INST=="natural environment research council"]<-"nerc natural environment research council"
    DATAFILE$INST[DATAFILE$INST=="nerc"]<-"nerc natural environment research council"
    DATAFILE$INST[DATAFILE$INST=="university of new england armidale"]<-"university of new england"
    DATAFILE$INST[DATAFILE$INST=="imperial college london"]<-"imperial college of science and technology"
    DATAFILE$INST[DATAFILE$INST=="king’s college"]<-"kings college london"
    DATAFILE$INST[DATAFILE$INST=="academy of sciences of the czech republic"]<-"czech academy of sciences"
    DATAFILE$INST[DATAFILE$INST=="academy of sciences of czech republic"]<-"czech academy of sciences"
    DATAFILE$INST[DATAFILE$INST=="neri"]<-"neri national environmental research institute"
    DATAFILE$INST[DATAFILE$INST=="national environmental research institute"]<-"neri national environmental research institute"
    DATAFILE$INST[DATAFILE$INST=="tartu"]<-"university of tartu"
    DATAFILE$INST[DATAFILE$INST=="tartu university"]<-"university of tartu"
    DATAFILE$INST[DATAFILE$INST=="bethesda"]<-"national cancer institute"
    DATAFILE$INST[DATAFILE$INST=="illinois"]<-"university of illinois"
    DATAFILE$INST[DATAFILE$INST=="hawaii"]<-"university of hawaii manoa"
    DATAFILE$INST[DATAFILE$INST=="university of illinois urbana champaign"]<-"university of illinois"
    DATAFILE$INST[DATAFILE$INST=="marine biological association of the united kingdom lab"]<-"marine biological association"
    DATAFILE$INST[DATAFILE$INST=="british antarctic survey"]<-"nerc british antarctic survey"
    DATAFILE$INST[DATAFILE$INST=="university college"]<-"university college london"
    
    DATAFILE$INST[DATAFILE$INST=="university of london imperial college of science and technology"]<-"imperial college london"
    DATAFILE$INST[DATAFILE$INST=="university london imperial coll sci technol and med"]<-"imperial college london"
    DATAFILE$INST[DATAFILE$INST=="imperial college of science and technology"]<-"imperial college london"
    DATAFILE$INST[DATAFILE$INST=="university du quebec a montreal"]<-"university of quebec"
    DATAFILE$INST[DATAFILE$INST=="universite de rennes"]<-"universite de rennes 1"
    
    DATAFILE$NOTES[DATAFILE$INST=="universite montpelier"|DATAFILE$INST=="universite montpelier 2"]<-"um1 and um2 reunited as um in 2015"
    
    DATAFILE$COUNTRY[DATAFILE$INST=="station biologique de la tour du valat"]<-"france"
    
    DATAFILE$INST[DATAFILE$CITY=="reno" & DATAFILE$INST== "university of nevada"]<-"university of nevada reno"
    DATAFILE$INST[DATAFILE$CITY=="hilo" & DATAFILE$INST== "university of hawaii"]<-"university of hawaii hilo"
    DATAFILE$INST[DATAFILE$CITY=="honolulu" & DATAFILE$INST== "university of hawaii"]<-"university of hawaii manoa"
    DATAFILE$INST[DATAFILE$INST== "university of indiana"]<-"indiana university bloomington"
    DATAFILE$INST[DATAFILE$INST== "indiana university"]<-"indiana university bloomington"
    DATAFILE$INST[DATAFILE$INST== "nebraska"]<-"university of nebraska lincoln"
    DATAFILE$INST[DATAFILE$INST== "pittsburgh"]<-"university of pittsburgh"
    DATAFILE$INST[DATAFILE$INST== "national autonomous university mexico"]<-"universidad nacional autonoma de mexico"
    DATAFILE$INST[DATAFILE$INST== "unam"]<-"universidad nacional autonoma de mexico"
    DATAFILE$INST[DATAFILE$INST== "seoul"]<-"seoul national university"
    DATAFILE$INST[DATAFILE$INST== "us fish and wildlife service"]<-"usfws united states fish and wildlife service"
    DATAFILE$INST[DATAFILE$INST== "usfws united state fish and wildlife service"]<-"usfws united states fish and wildlife service"
    DATAFILE$INST[DATAFILE$INST== "fsu"]<-"florida state university"
    DATAFILE$INST[DATAFILE$INST== "usfs us geological survey"]<-"usgs united states geological survey"
    DATAFILE$INST[DATAFILE$INST== "university of lund"]<-"lund university"
    DATAFILE$INST[DATAFILE$INST== "labratory of general and apllied ichthyology"]<-"national museum of natural history france"
    
    
    DATAFILE$INST[DATAFILE$COUNTRY=="sweden" & DATAFILE$INST== "university of agricultural sciences"]<-"swedish university of agricultural sciences"
    
    DATAFILE$INST[DATAFILE$CITY=="chicago" & DATAFILE$INST== "university of illinois"]<-"university of illinois chicago"
    
    
    DATAFILE$INST[DATAFILE$LAST_NAME=="luque" & DATAFILE$INST== "same as other ag inst?"]<-
      "irstea national research institute of science and technology for environment and agriculture"
    
    DATAFILE$INST[DATAFILE$LAST_NAME=="baudry" & DATAFILE$INST== "same as other ag inst?"]<-"inra centre bretagne-normandie"
    
    
    
    DATAFILE$INST[DATAFILE$INST=="university of british columbia" & DATAFILE$CITY=="kelowna"]<-"university of british columbia okanagan"
    DATAFILE$INST[DATAFILE$INST=="university of british columbia" & DATAFILE$CITY=="vancouver"]<-"university of british columbia vancouver"
    
    DATAFILE$INST[DATAFILE$INST=="university of texas" & DATAFILE$CITY=="austin"]<-"university of texas austin"
    DATAFILE$INST[DATAFILE$INST=="university of texas" & DATAFILE$CITY=="austin"]<-"university of texas austin"
    DATAFILE$INST[DATAFILE$INST=="university of massachusetts" & DATAFILE$CITY=="amherst"]<-"university of massachusetts amherst"
    DATAFILE$INST[DATAFILE$INST=="university of north carolina" & DATAFILE$CITY=="chapel hill"]<-"university of north carolina chapel hill"
    DATAFILE$INST[DATAFILE$INST=="university of minnesota" & DATAFILE$CITY=="duluth"]<-"university of minnesota duluth"
    DATAFILE$INST[DATAFILE$INST=="university of arkansas" & DATAFILE$CITY=="fayetteville"]<-"university of arkansas fayetteville"
    DATAFILE$INST[DATAFILE$INST=="university of hawaii" & DATAFILE$CITY=="hilo"]<-"university of hawaii hilo"
    DATAFILE$INST[DATAFILE$INST=="university of hawaii" & DATAFILE$CITY=="honolulu"]<-"university of hawaii manoa"
    DATAFILE$INST[DATAFILE$INST=="university of alaska" & DATAFILE$CITY=="fairbanks"]<-"university of alaska fairbanks"
    DATAFILE$INST[DATAFILE$INST=="university of minnesota" & (DATAFILE$CITY=="minneapolis"|DATAFILE$CITY=="st paul")]<-"university of minnesota twin cities"
    DATAFILE$INST[DATAFILE$INST=="james cook university" & DATAFILE$CITY=="townsville"]<-"james cook university townsville"
    
    DATAFILE$UNIT[DATAFILE$INST=="smithsonian migratory bird center"]<- "smithsonian migratory bird center"
    DATAFILE$INST[DATAFILE$INST=="smithsonian migratory bird center"]<- "smithsonian national zoological park"
    
   
    
    
    
    
    DATAFILE$INST[DATAFILE$INST=="university of wisconsin" & DATAFILE$CITY=="madison"]<-"university of wisconsin madison"
    DATAFILE$INST[DATAFILE$INST=="university of washington" & DATAFILE$CITY=="seattle"]<-"university of washington seattle"
    DATAFILE$INST[DATAFILE$INST=="university of washington" & DATAFILE$CITY=="seattle"]<-"university of washington seattle"
    
    DATAFILE$INST[DATAFILE$INST=="university of toronto" & DATAFILE$CITY=="mississauga"]<-"university of toronto mississauga"
    DATAFILE$INST[DATAFILE$INST=="university of toronto" & DATAFILE$CITY=="scarborough"]<-"university of toronto scarborough"
    
    DATAFILE$COUNTRY[DATAFILE$INST=="usgs forest and rangeland ecosystem science center"]<-"usa"    
    DATAFILE$COUNTRY[DATAFILE$INST=="university of bielefeld"]<-"germany"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of oxford"]<-"united kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of hull"]<-"united kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of edinburgh"]<-"united kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of essex"]<-"united kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of turku"]<-"finland"
    
    DATAFILE$COUNTRY[DATAFILE$INST=="university of washington seattle"]<-"usa"
    DATAFILE$COUNTRY[DATAFILE$INST=="carnegie institution"]<-"usa"
    
    
    DATAFILE$INST[DATAFILE$INST=="wsl swiss federal research institute" & DATAFILE$CITY=="birmensdorf" ]<-
      "wsl swiss federal institute for forest snow and landscape research birmensdorf"
    
    # DATAFILE$INST[DATAFILE$INST=="wsl swiss federal research institute" & DATAFILE$CITY=="davos" ]<-
    #   "wsl swiss federal institute for forest snow and landscape research davos"
    # # OR
    DATAFILE$INST[DATAFILE$INST=="wsl swiss federal research institute" & DATAFILE$CITY=="davos" ]<-
      "wsl swiss federal institute for snow and avalanche research davos"
    
    
    DATAFILE$INST[DATAFILE$INST=="swiss federal institute for forest snow and landscape research"]<-
      "wsl swiss federal institute for forest snow and landscape research birmensdorf"
    
    DATAFILE$INST[DATAFILE$INST=="swiss federal institute for forest snow and landscape research wsl"]<-
      "wsl swiss federal institute for forest snow and landscape research birmensdorf"
    
    DATAFILE$INST[DATAFILE$INST=="wsl swiss federal institute for forest snow and landscape research"]<-
      "wsl swiss federal institute for forest snow and landscape research birmensdorf"
    
    DATAFILE$INST[DATAFILE$INST=="wsl institute for snow and avalanche research sfl"]<-
      "wsl swiss federal institute for snow and avalanche research davos"
    
    DATAFILE$INST[DATAFILE$INST=="wsl swiss federal research institute"]<-
      "wsl swiss federal institute for forest snow and landscape research birmensdorf"
    
    DATAFILE$INST[DATAFILE$INST=="swiss federal institute of technology"]<-
      "eth swiss federal institute of technology zurich"
    
    DATAFILE$INST[DATAFILE$INST=="swiss federal institute of aquatic science and technology"]<-
      "eawag swiss federal institute of aquatic science and technology"
    
  
    
    
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-    
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-    
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-    
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-    
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-    
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    
    
    
    
    
    
    
    
    
    
    DATAFILE$INST<-gsub('[.]','',DATAFILE$INST)  
    # DATAFILE$INST[DATAFILE$INST== "istituto per l¬¢ambiente marino costiero" ]<-"instituto per lambiente marino costiero"
    # DATAFILE$INST[DATAFILE$INST==  ]<-    
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    
    gsub("goottingen","gottingen",DATAFILE$INST)
    gsub("istituto per looambiente marino costiero","istituto per lambiente marino costiero",DATAFILE$INST)
    gsub("macaulayooinstitute","istituto per lambiente marino costiero",DATAFILE$INST)
    gsub("universitoo de montreal","macaulay land use research institute",DATAFILE$INST)
    DATAFILE$ascii<-NULL
    
    # alldata$validenc<-validenc(alldata$country)
    # summary(alldata$validenc<-validenc(alldata$country))
    # foo<-alldata %>% filter(validenc==false)
    # foo<-distinct(foo, country)
    #dont forget - filter will remove all with na in the conditions
    DATAFILE<-DATAFILE %>% replace_na(list(INST = "missing"))
    DATAFILE<-DATAFILE %>% filter(!INST=="no one by this name")
    
    
return(DATAFILE)

}
# FUNCTION TO SYSTEMATIZE UNIVERSITY NAMES AND LOCATIONS
institution_cleaner <- function(datafile) {
  # datafile<-alldata

  # France:
  # 'laboratoire arago' is aka "observatoire océanologique de banyuls-sur-mer" and coadmined by UPMC-Paris 6 (Université Pierre et Marie Curie) and cnrs
  # MIVEGEC (MALADIES INFECTIEUSES ET VECTEURS:ECOLOGIE, GÉNÉTIQUE, EVOLUTION ET CONTRÔLE is coadnibned bvy cnrs and u montpellier
  # centre detudes biologiques de chize (coadmined by cnrs and la rochelle universite)
  # 'centre regional de phytosociologie' is at 'Conservatoire Botanique'
  # roscoff biological station is coadmined by Sorbonne University and CNRS
  # cnrs centre decologie des ressources renouvelables
  # cnrs centre decologie fonctionnelle et evolutive - CEFE
  # cnrs centre delaboration de materiaux et detudes structurales - CEMES
  # cnrs institut des sciences de levolution montpellier - ISEM
  # cnrs institut ecologie et environnement - INEE
  # luca borger (inra and cnrs) was a postdoc funded by both based at chize.
  # Netherlands:
  #
  #   1. 'agricultural university', 'university of wageningen', and 'wageningen agricultural university'
  # are now all 'wageningen university and research'
  #
  # 2. 'netherlands institute of ecology (nioo knaw)' is on the campus of 'wageningen university and research'
  # but part of the Royal Academy.
  #
  # 3. wageningen university and instituto boliviano de investigacion forestal: for paper counted primary address
  #
  # 4. 'alterra' (aka 'alterra research institute for green world'): renamed wageningen environmental research.
  # In database it is 'wageningen environmental research (alterra)'
  #
  # ANUTECH is a spin-off company started by ANU
  # BFH: Fed Research Center for Foresty and Forest Products - Abbreviation
  # BFW: Austrian Research Center for Forests
  # BOKU: university of natural resources and applied life sciences vienna
  # CSIRO: Commonwealth Scientific and Industrial Research Organization
  # IADIZA: CONICET Instituto Argentino de Investigaciones de las Zonas Aridas
  # ICRAF: International Center for Research in Agroforestry
  # IFREMER: Institut Français de Recherche pour l'exploitation de la Mer
  # IMEDEA: Instituto Mediterraneo de Estudios Avanzados
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

  # datafile$country<-as.factor(datafile$country)
  # levels(datafile$country)
  datafile$inst[datafile$inst == ""] <- NA
  datafile$inst[datafile$inst == "N/A"] <- NA
  datafile$inst[datafile$inst == "."] <- NA



  datafile$country <- as.character(datafile$country)


  #

  datafile$country[datafile$inst == "university of new south wales"] <- "australia"
  datafile$country[datafile$inst == "university of guelph"] <- "canada"
  datafile$country[datafile$inst == "instituto mediterraneo de estudios avanzados (imedea)"] <- "spain"
  ##############################################################
  # COrrecting the country in whihc an Editor is based
  ##############################################################
  # datafile$country<-as.factor(datafile$country)
  # datafile<-droplevels(datafile)
  # levels(datafile$country)
  ##############################################################
  # STILL TO DO
  ##############################################################

  # NEED TO CONFIRM WHAT PART OF USSR IN WHICH THE AUTHOR WAS BASED
  levels(datafile$country) <- c(levels(datafile$country), "russia", "scotland", "northern ireland")
  datafile$country[datafile$country == "ussr"] <- "russia"

  # delete these
  which(datafile$country == "")

  ##############################################################
  ##############################################################
  # clean-up of institutions
  ##############################################################
  ##############################################################
  datafile$inst[datafile$inst == "."] <- NA
  datafile <- as.data.frame(datafile)
  # datafile$journal<-as.factor(datafile$journal)
  # correcting the institution where an editor is based
  # levels(datafile$inst) <- c(levels(datafile$inst),"state university of new york college of environmental science and forestry")
  # levels(datafile$inst) <- c(levels(datafile$inst),"forestry and forest products research institute")
  # levels(datafile$inst) <- c(levels(datafile$inst),"university of minnesota duluth")
  # levels(datafile$inst) <- c(levels(datafile$inst),"university of minnesota crookston")
  # # levels(datafile$inst) <- c(levels(datafile$inst),"university of toronto mississauga")
  # levels(datafile$inst) <- c(levels(datafile$inst),"cnrs centre decologie fonctionnelle et evolutive")
  ##############################################################
  ##############################################################
  # correcting or systematizing the name/speclling of an institution
  ##############################################################
  ##############################################################
  # levels(datafile$inst) <- c(levels(datafile$inst),"university of missouri columbia")


  datafile$inst <- as.factor(datafile$inst)
  levels(datafile$inst) <- c(
    levels(datafile$inst), "university of missouri columbia",
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
    "calyx, inc.", "university of north carolina charlotte",
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
    "institute of terrestrial ecology"
  )


  datafile$city[datafile$city == "w<9f>rzburg"] <- "wurzburg"
  datafile$city[datafile$city == "k<9a>ln"] <- "cologne"
  datafile$city[datafile$city == "g<9a>ttingen"] <- "gottingen"
  datafile$city[datafile$city == "z<81>rich"] <- "zurich"
  datafile$city[datafile$city == "m<9f>nchen"] <- "munich"

  datafile$state[datafile$state == "z<81>rich"] <- NA

  # datafile$unit[datafile$unit=="estaci<f3>n biol<f3>gica de do<f1>ana"]<-"estacion biologica de donana"
  # datafile$unit[datafile$unit=="departamento de ecolog<90>a"]<-"departamento de ecologia"
  # datafile$unit[datafile$unit=="fb biologie/chemie/<80>kologie"]<-"fb biologie/chemiekologie"
  # datafile$unit[datafile$unit=="institut f<99>r biologie (ii)"]<-"institut fur biologie (ii)"





  datafile$inst <- as.character(datafile$inst)
  datafile$inst <- trimws(datafile$inst, which = "left")
  datafile$inst <- trimws(datafile$inst, which = "right")
  datafile$inst <- gsub("  ", " ", datafile$inst)
  datafile$inst <- gsub("- ", "-", datafile$inst)
  datafile$inst <- gsub(" -", "-", datafile$inst)
  datafile$inst <- gsub(" - ", "-", datafile$inst)
  datafile$inst <- gsub(" at ", "-", datafile$inst)
  datafile$inst <- gsub("the ", "", datafile$inst)
  datafile$inst <- gsub("univ ", "university ", datafile$inst)
  datafile$inst <- gsub("univ ", "university ", datafile$inst)
  datafile$inst <- gsub("calif ", "california ", datafile$inst)
  datafile$inst <- gsub("calif ", "california ", datafile$inst)
  datafile$inst <- gsub("universtity ", "university ", datafile$inst)
  datafile$inst <- gsub("univesity ", "university ", datafile$inst)
  datafile$inst <- gsub("universityof", "university of ", datafile$inst)
  datafile$inst <- gsub("ofarizona", "of arizona", datafile$inst)
  datafile$inst <- gsub("unviersity", "university", datafile$inst)
  datafile$inst <- gsub("unviversityof", "university of", datafile$inst)
  datafile$inst <- gsub("univesrity", "university", datafile$inst)
  datafile$inst <- gsub("unniversity", "university", datafile$inst)
  datafile$inst <- gsub("museum natl hist nat", "national museum of natural history france", datafile$inst)
  datafile$inst <- gsub("university<ca>of<ca>california<ca>santa<ca>cruz", "university of california santa cruz", datafile$inst)
  datafile$inst <- gsub("uc santa cruz", "university of california santa cruz", datafile$inst)
  # todo: gsub arkansas one is not working inside the function and i have no idea why.
  datafile$inst <- gsub("arkansas", "arkansas", datafile$inst)
  datafile$inst <- gsub("uva", "university of virginia", datafile$inst)
  # datafile$inst<-gsub("us forest serv","us forest service",datafile$inst)

  datafile$inst <- gsub("double check", NA, datafile$inst)
  datafile$inst <- gsub("n.c. state", "north carolina state university", datafile$inst)
  datafile$inst <- gsub("uc davis", "university of california davis", datafile$inst)

  datafile$inst <- gsub("univ connecticut", "university of connecticut", datafile$inst)
  datafile$inst <- gsub("univ los andes", "universidad de los andes", datafile$inst)
  datafile$inst <- gsub("univ nacl autonoma mexico", "universidad nacional autonoma de mexico", datafile$inst)
  datafile$inst <- gsub("ume¥ university", "umea university", datafile$inst)
  # datafile$inst<-gsub("university of arkansas","university of arkansas",datafile$inst)
  datafile$inst <- gsub(" of ", " of ", datafile$inst)
  datafile$inst <- gsub("of california ", "of california-", datafile$inst)
  # datafile$inst<-gsub("u ", "university of ", datafile$inst)
  datafile$inst <- gsub("u t", "university of t", datafile$inst)
  datafile$inst <- gsub("u w", "university of w", datafile$inst)
  datafile$inst <- gsub("u i", "university of i", datafile$inst)
  datafile$inst <- gsub("u s", "university of s", datafile$inst)
  datafile$inst <- gsub("u o", "university of o", datafile$inst)
  datafile$inst <- gsub("u m", "university of m", datafile$inst)
  datafile$inst <- gsub("u k", "university of k", datafile$inst)
  datafile$inst <- gsub("u l", "university of l", datafile$inst)
  datafile$inst <- gsub("u g", "university of g", datafile$inst)
  datafile$inst <- gsub("u f", "university of f", datafile$inst)
  datafile$inst <- gsub("u d", "university of d", datafile$inst)
  datafile$inst <- gsub("u c", "university of c", datafile$inst)
  datafile$inst <- gsub("u b", "university of b", datafile$inst)



  datafile$inst <- gsub("california, ", "california-", datafile$inst)
  datafile$inst <- gsub("u california ", "university of california-", datafile$inst)
  # datafile$inst<-gsub("u. of", "university of", datafile$inst)
  datafile$inst <- gsub("univ. of", "university of", datafile$inst)
  datafile$inst <- gsub("machigan", "michigan", datafile$inst)
  datafile$inst <- gsub("n. prairie", "northern prairie", datafile$inst)
  datafile$inst <- gsub("pacific s.w. research station-us forest service", "usfs-pacific southwest research station", datafile$inst)
  datafile$inst <- gsub("n.b.s. ", "usgs-", datafile$inst)
  datafile$inst <- gsub("u.s. ", "us ", datafile$inst)
  datafile$inst <- gsub("illimois", "illinois", datafile$inst)
  datafile$inst <- gsub("univerisity", "university", datafile$inst)
  datafile$inst <- gsub("univeristy", "university", datafile$inst)
  datafile$inst <- gsub("univerist", "university", datafile$inst)
  datafile$inst <- gsub("universit ", "university", datafile$inst)
  datafile$inst <- gsub(" inpa", "", datafile$inst)
  datafile$inst <- gsub("--", " ", datafile$inst)
  datafile$inst <- gsub("-", " ", datafile$inst)









  datafile$inst <- gsub(" (csiro)", " ", datafile$inst)
  datafile$inst <- gsub(" csiro", " ", datafile$inst)
  datafile$inst <- gsub(",cas", " ", datafile$inst)
  datafile$inst <- gsub("nacaional", "nacional", datafile$inst)
  datafile$inst <- gsub("british colombia", "british columbia", datafile$inst)
  datafile$inst <- gsub("smithosonian", "smithsonian", datafile$inst)
  datafile$inst <- gsub("kansas", "kansas", datafile$inst)
  datafile$inst <- gsub("sao paolo", "sao paulo", datafile$inst)
  datafile$inst <- gsub("illinois", "illinois", datafile$inst)
  datafile$inst <- gsub("wuerzburg", "wurzburg", datafile$inst)
  datafile$inst <- gsub("latrobe", "la trobe", datafile$inst)
  datafile$inst <- gsub("no one by", "no one by", datafile$inst)
  datafile$inst <- gsub("botannical", "botanical", datafile$inst)
  datafile$inst <- gsub("berkely", "berkeley", datafile$inst)
  datafile$inst <- gsub("commonweath", "commonwealth", datafile$inst)
  datafile$inst <- gsub("archibold", "archbold", datafile$inst)
  datafile$inst <- gsub("loisisana", "louisiana", datafile$inst)
  datafile$inst <- gsub("lousiana", "louisiana", datafile$inst)
  datafile$inst <- gsub("bringham", "brigham", datafile$inst)
  datafile$inst <- gsub("connetticut", "connecticut", datafile$inst)
  datafile$inst <- gsub("unversity", "university", datafile$inst)
  datafile$inst <- gsub(",", "", datafile$inst)
  datafile$inst <- gsub("westleyan", "wesleyan", datafile$inst)
  datafile$inst <- gsub("'", "", datafile$inst)
  datafile$inst <- gsub("veterinary&", "veterinary and", datafile$inst)
  datafile$inst <- gsub("&", "and", datafile$inst)
  datafile$inst <- gsub("virginia tech", "virginia polytechnic institute and state university", datafile$inst)
  datafile$inst <- gsub("austrailan", "australian", datafile$inst)
  datafile$inst <- gsub("indian institute of sciences", "indian institute of science", datafile$inst)
  datafile$inst <- gsub("kings", "kings", datafile$inst)
  datafile$inst <- gsub("louisisana", "louisiana", datafile$inst)
  datafile$inst <- gsub("university queensland", "university of queensland", datafile$inst)
  datafile$inst <- gsub("universrity", "university", datafile$inst)
  datafile$inst <- gsub("canadian forestry", "canadian forest", datafile$inst)
  datafile$inst <- gsub("mighican", "michigan", datafile$inst)
  datafile$inst <- gsub("de montpellier ii", "montpellier ii", datafile$inst)
  datafile$inst <- gsub("north arizona", "northern arizona", datafile$inst)
  datafile$inst <- gsub("wsl", "wsl", datafile$inst)
  datafile$inst <- gsub("alabama in", "alabama", datafile$inst)
  datafile$inst <- gsub("fonctionelle", "fonctionnelle", datafile$inst)
  datafile$inst <- gsub("-cnrs", "", datafile$inst)
  # datafile$inst<-gsub("university of sherbooke", "universite de sherbooke", datafile$inst)
  datafile$inst <- gsub("smithsonian institute", "smithsonian institution", datafile$inst)
  datafile$inst <- gsub("minnestoa", "minnesota", datafile$inst)
  datafile$inst <- gsub("university of lausanne", "universite de lausanne", datafile$inst)
  datafile$inst[datafile$inst == "institute of ecosystem studies"] <- "cary institute of ecosystem studies"
  datafile$inst[datafile$inst == "institute of ecosystem studies"] <- "cary institute of ecosystem studies"
  datafile$inst <- gsub("environment bangalore", "environment", datafile$inst)
  datafile$country[datafile$inst == "university of aberdeen"] <- "scotland"
  datafile$inst[datafile$inst == "auburn"] <- "auburn university"
  datafile$inst[datafile$inst == "auburn u"] <- "auburn university"
  datafile$inst[datafile$inst == "michigan state"] <- "michigan state university"
  datafile$inst[datafile$inst == "berkeley"] <- "university of california berkeley"
  datafile$inst[datafile$inst == "berkeley"] <- "university of california berkeley"
  datafile$inst[datafile$inst == "duke"] <- "duke university"
  datafile$inst[datafile$inst == "texas a and m"] <- "texas a and m university"
  datafile$inst[datafile$inst == "purdue"] <- "purdue university"
  datafile$inst[datafile$inst == "princeton"] <- "princeton university"
  datafile$inst[datafile$inst == "oxford"] <- "oxford university"
  datafile$inst[datafile$inst == "los alamos"] <- "los alamos national laboratory"
  datafile$inst[datafile$inst == "stazione zoologica \"anton dohrn\""] <- "stazione zoologica anton dohrn"
  datafile$inst[datafile$inst == "cambridge"] <- "university of cambridge"
  datafile$inst[datafile$inst == "princeton"] <- "princeton university"
  datafile$inst[datafile$inst == "lsu"] <- "louisiana state university"
  # datafile$inst[datafile$inst=="wsl swiss federal research institute"]<-"swiss federal research institute wsl"
  datafile$unit[datafile$inst == "pacific s.w. research station-us forest service"] <- "usfs pacific southwest research station"
  # datafile$inst[datafile$inst=="pacific s.w. research station-us forest service"]<-"us forest service"
  datafile$inst[datafile$inst == "university fo california berkeley"] <- "university of california berkeley"
  datafile$inst[datafile$inst == "us geological survey"] <- "usgs"

  datafile$inst[datafile$inst == "uf"] <- "university of florida"
  datafile$inst[datafile$inst == "cal state bakersfield"] <- "california state university bakersfield"
  datafile$inst[datafile$inst == "universityof wisconsin-milwaukee"] <- "university of wisconsin milwaukee"
  datafile$inst[datafile$inst == "colorado state"] <- "colorado state university"
  datafile$inst[datafile$inst == "cornell"] <- "cornell university"
  datafile$inst[datafile$inst == "east carolina u"] <- "east carolina university"
  datafile$inst[datafile$inst == "florida international u"] <- "florida international university"
  datafile$inst[datafile$inst == "fsu"] <- "florida state university"
  datafile$inst[datafile$inst == "uga"] <- "university of georgia"
  datafile$inst[datafile$inst == "harvard"] <- "harvard university"
  datafile$inst[datafile$inst == "illinois state"] <- "illinois state university"
  datafile$inst[datafile$inst == "indiana u"] <- "indiana university"
  datafile$inst[datafile$inst == "institute of ecosystem"] <- "institute of ecosystem studies"
  datafile$inst[datafile$inst == "iowa state"] <- "iowa state university"
  datafile$inst[datafile$inst == "john carroll u"] <- "john carroll university"
  datafile$inst[datafile$inst == "knoxville"] <- "university of tennessee"
  datafile$inst[datafile$inst == "louisiana state"] <- "louisiana state university"
  datafile$inst[datafile$inst == "marshall unin"] <- "marshall university"
  datafile$inst[datafile$inst == "franklin and farshall"] <- "franklin and marshall university"
  datafile$inst[datafile$inst == "university of bangor"] <- "bangor university"
  datafile$inst[datafile$inst == "miami u"] <- "miami university"
  datafile$inst[datafile$inst == "montana state"] <- "montana state university"
  datafile$inst[datafile$inst == "north dakota state"] <- "north dakota state university"
  datafile$inst[datafile$inst == "ohio state"] <- "ohio state university"
  datafile$inst[datafile$inst == "oregon state"] <- "oregon state university"
  datafile$inst[datafile$inst == "rensselaer poly"] <- "rensselaer polytechnic institute"
  datafile$inst[datafile$inst == "rutgers u"] <- "rutgers university"
  datafile$inst[datafile$inst == "stanford"] <- "stanford university"
  datafile$inst[datafile$inst == "stanford"] <- "stanford university"

  datafile$inst[datafile$inst == "state university of new york at binghamton"] <- "suny binghamton"
  datafile$inst[datafile$inst == "binghamton university-suny"] <- "suny binghamton"
  datafile$inst[datafile$inst == "texas state"] <- "texas state university"
  datafile$inst[datafile$inst == "the university of southwestern louisiana"] <- "university of southwestern louisiana"
  datafile$inst[datafile$inst == "u arizona"] <- "university of arizona"
  datafile$inst[datafile$inst == "university of california irvine"] <- "university of california irvine"
  datafile$inst[datafile$inst == "university of california riverside"] <- "university of california riverside"
  datafile$inst[datafile$inst == "california state university of san marcos"] <- "california state university san marcos"
  datafile$inst[datafile$inst == "university of california at san diego"] <- "university of california san diego"
  datafile$inst[datafile$inst == "university of california santa cruz"] <- "university of california santa cruz"
  datafile$inst[datafile$inst == "uc santa cruz"] <- "university of california santa cruz"
  datafile$inst[datafile$inst == "uc merced, university of california"] <- "university of california merced"
  datafile$inst[datafile$inst == "umsl"] <- "university of missouri st louis"
  datafile$inst[datafile$inst == "university of carolina aiken"] <- "university of south carolina aiken"
  datafile$inst[datafile$inst == "university of massachusetts boston"] <- "university of massachusetts boston"
  # datafile$inst[datafile$inst=="university of nevada las vegas"]<-"university of nevada-las vegas"
  # datafile$inst[datafile$inst=="university of texas austin"]<-"university of texas austin"
  datafile$inst[datafile$inst == "vanderbilt"] <- "vanderbilt university"
  datafile$inst[datafile$inst == "virginia commonweath u"] <- "virginia commonweath university"
  datafile$inst[datafile$inst == "washington state"] <- "washington state university"
  datafile$inst[datafile$inst == "washington u"] <- "washington university"
  datafile$inst[datafile$inst == "willamette u"] <- "willamette university"
  datafile$inst[datafile$inst == "university of pennsylvania, "] <- "university of pennsylvania"
  datafile$inst[datafile$inst == "university of pittsburg"] <- "university of pittsburgh"
  datafile$inst[datafile$inst == "university of tennesee"] <- "university of tennessee"
  datafile$inst[datafile$inst == "university of tennese"] <- "university of tennessee"
  datafile$inst[datafile$inst == "acad natural science philadelphia"] <- "academy of natural science philadelphia"
  datafile$inst[datafile$inst == "am mus nat hist"] <- "american museum of natural history"
  datafile$inst[datafile$inst == "coloradostate univ"] <- "colorado state university"
  datafile$inst[datafile$inst == "evergreen state college washington"] <- "evergreen state college"
  datafile$inst[datafile$inst == "filed museum of natural history"] <- "field museum of natural history"
  datafile$inst[datafile$inst == "george mason university virginia"] <- "george mason university"
  datafile$inst[datafile$inst == "insitute of ecosystem studies"] <- "cary institute of ecosystem studies"
  datafile$inst[datafile$inst == "kenyon college ohio"] <- "kenyon college"
  datafile$inst[datafile$inst == "louisiana state univeristy"] <- "louisiana state university"
  datafile$inst[datafile$inst == "milwaukee public mus"] <- "milwaukee public museum"
  datafile$inst[datafile$inst == "missouri botanical gardens"] <- "missouri botanical garden"
  # datafile$inst[datafile$inst=="oklahoma museum of natural history"]<-"oklahoma mus nat his"
  datafile$inst[datafile$inst == "smithsonian(stri)"] <- "smithsonian tropical research institute"
  datafile$inst[datafile$inst == "stri smithsonian"] <- "smithsonian tropical research institute"
  datafile$inst[datafile$inst == "u connecticut"] <- "university of connecticut"
  datafile$inst[datafile$inst == "university of alabama huntsville"] <- "university of alabama huntsville"
  datafile$inst[datafile$inst == "university of connetticut storrs"] <- "university of connecticut"
  datafile$inst[datafile$inst == "university of michgan"] <- "university of michigan"
  datafile$inst[datafile$inst == "university of missouri st louis"] <- "university of missouri st louis"
  datafile$inst[datafile$inst == "university of missouri_st louis"] <- "university of missouri st louis"
  datafile$inst[datafile$inst == "university of wisconsin milwaukee"] <- "university of wisconsin milwaukee"
  datafile$inst[datafile$inst == "u california irvine"] <- "university of california irvine"
  datafile$inst[datafile$inst == "u california riverside"] <- "university of california riverside"
  datafile$inst[datafile$inst == "university of california at san diego"] <- "university of california san diego"
  datafile$inst[datafile$inst == "u california santa barbara"] <- "university of california santa barbara"
  datafile$inst[datafile$inst == "u california santa cruz"] <- "university of california santa cruz"
  datafile$inst[datafile$inst == "university of california los angeles"] <- "university of california los angeles"
  datafile$inst[datafile$inst == "university of california at los angeles"] <- "university of california los angeles"
  datafile$inst[datafile$inst == "university of california berkeley"] <- "university of california berkeley"
  datafile$inst[datafile$inst == "university of california davis"] <- "university of california davis"
  datafile$inst[datafile$inst == "university of of california davis"] <- "university of california davis"
  datafile$inst[datafile$inst == "university of california santa barbara"] <- "university of california santa barbara"
  datafile$inst[datafile$inst == "university of california san diego"] <- "university of california san diego"
  datafile$inst[datafile$inst == "university of california san diego"] <- "university of california san diego"
  datafile$inst[datafile$inst == "rensselaer poly"] <- "rensselaer polytechnic institute"
  datafile$inst[datafile$inst == "oklahoma mus nat his"] <- "oklahoma museum of natural history"
  datafile$inst[datafile$inst == "us forest service pacific southwest research station"] <- "usfs pacific southwest research station"
  datafile$inst[datafile$inst == "u.s.f.w.s. national wetlands research center"] <- "usfws national wetlands research center"
  datafile$inst[datafile$inst == "arizona state university west"] <- "arizona state university west campus"
  datafile$inst[datafile$inst == "naval research laboratory dc"] <- "usonr naval research laboratory"
  datafile$inst[datafile$inst == "temple-inland forest"] <- "temple-inland forest products"
  datafile$inst[datafile$inst == "usda forest service"] <- "us forest service"
  datafile$inst[datafile$inst == "usda forest service rocky mountain research station"] <- "usfs rocky mountain research station"
  datafile$inst[datafile$inst == "university of newcastle upon tyne"] <- "newcastle university"
  datafile$inst[datafile$inst == "university of newcastle upon tuyne"] <- "newcastle university"
  datafile$inst[datafile$inst == "usda forest service southern research station"] <- "usfs southern research station"
  datafile$inst[datafile$inst == "usfs-pacific southwest research station"] <- "usfs pacific southwest research station"
  datafile$inst[datafile$inst == "usfws-national wetlands research center"] <- "usfs national wetlands research center"
  # datafile$unit[datafile$inst=="usgs forest and rangeland ecosystem science center"]<-"forest and rangeland ecosystem science center"
  # datafile$inst[datafile$inst=="usgs forest and rangeland ecosystem science center"]<-"us geological survey"
  # datafile$unit[datafile$inst=="usgs-northern prairie wildlife research center"]<-"northern prairie wildlife research center"
  datafile$inst[datafile$inst == "usgs-northern prairie wildlife research center"] <- "usgs northern prairie wildlife research center"
  datafile$inst[datafile$inst == "national museum of natural history"] <- "smithsonian national museum of natural history"
  datafile$inst[datafile$inst == "smithsonian"] <- "smithsonian national museum of natural history" # eb verified
  datafile$inst[datafile$inst == "usda"] <- "usda us department of agriculture"
  # datafile$unit[datafile$inst=="usda cooperative state research, education and extension service (csrees)"]<-"usda cooperative state research, education, and extension service"
  datafile$inst[datafile$inst == "usda cooperative state research, education and extension service (csrees)"] <- "usda cooperative state research, education, and extension service"
  datafile$inst[datafile$inst == "virginia tech"] <- "virginia polytechnic institute and state university"
  datafile$inst[datafile$inst == "university of pennsylvania,"] <- "university of pennsylvania"
  datafile$inst[datafile$inst == "division of reptiles and amphibians smithsonian institute washington dc"] <- "smithsonian national museum of natural history"
  datafile$inst[datafile$inst == "university of california-at los angeles"] <- "university of california los angeles"
  datafile$inst[datafile$inst == "national museum of natural history, smithsonian institution"] <- "smithsonian national museum of natural history"

  datafile$unit[datafile$inst == "citrus research and education center"] <- "citrus research and education center"
  datafile$inst[datafile$inst == "citrus research and education center"] <- "university of florida"
  datafile$inst[datafile$inst == "ohio u"] <- "ohio university"
  datafile$inst[datafile$inst == "university of south carolina" & datafile$city == "columbia"] <- "university of south carolina columbia"
  # datafile$unit[datafile$inst=="us department of agriculture cooperative state research, education, and extension service"]<-"cooperative state research, education, and extension service"
  datafile$inst[datafile$inst == "us department of agriculture cooperative state research, education, and extension service"] <- "usda cooperative state research, education, and extension service"
  datafile$inst[datafile$LAST_NAME == "bowers" & datafile$city == "vadnais heights"] <- "calyx inc"
  datafile$unit[datafile$inst == "arnold arboretum of harvard university"] <- "arnold arboretum"
  datafile$inst[datafile$inst == "arnold arboretum of harvard university"] <- "harvard university"
  datafile$unit[datafile$inst == "museum of comparative zoology"] <- "museum of comparative zoology"
  datafile$inst[datafile$inst == "museum of comparative zoology"] <- "harvard university"
  datafile$inst[datafile$inst == "suny"] <- "suny state university of new york"
  datafile$inst[datafile$inst == "state university of new york at stony brook"] <- "suny stony brook"
  datafile$inst[datafile$inst == "state university of new york syracuse"] <- "suny syracuse"
  # datafile$inst[datafile$inst=="arnold arboretum of harvard university"]<-"harvard university arnold arboretum"
  datafile$unit[datafile$inst == "harvard medical school"] <- "medical school"
  datafile$inst[datafile$inst == "harvard medical school"] <- "harvard university"
  datafile$inst[datafile$inst == "australian research center for urban "] <- "smithsonian tropical research institute"
  datafile$inst <- gsub(" catie", "", datafile$inst)
  datafile$inst <- gsub("catie ", "", datafile$inst)
  datafile$inst <- gsub(" (unesp)", "", datafile$inst)
  datafile$inst[datafile$inst == "universidade estadual paulista (unesp)"] <- "universidade estadual paulista"
  datafile$inst <- gsub("darwin university darwin", "darwin university", datafile$inst)
  datafile$inst <- gsub("technion-", "", datafile$inst)
  datafile$inst <- gsub("university california", "university of california", datafile$inst)
  datafile$inst <- gsub("israel institute of technology", "technion israel institute of technology", datafile$inst)
  # datafile$inst<-gsub("ossietzky","", datafile$inst)
  datafile$inst <- gsub(",usda", "", datafile$inst)
  datafile$inst <- gsub("ufz centre", "helmholtz centre", datafile$inst)
  datafile$inst <- gsub(" ufz", "", datafile$inst)
  datafile$inst <- gsub("helmholtz centre for environmental research", "helmholtz centre for environmental research ufz", datafile$inst)
  datafile$inst <- gsub("a& m univ", "a & m university", datafile$inst)
  datafile$inst <- gsub("aandm", "a & m", datafile$inst)
  datafile$inst <- gsub("in cornwall", "cornwall", datafile$inst)
  datafile$inst <- gsub("suny", "state university of new york", datafile$inst)
  # datafile$inst<-gsub("usgs","us geological survey", datafile$inst)
  datafile$inst <- gsub("university of mexico", "universidad nacional autonoma de mexico", datafile$inst)
  datafile$inst <- gsub("nc agricultural", "north carolina agricultural", datafile$inst)
  datafile$inst <- gsub("for forest, snow, and landscape research", "wsl", datafile$inst)
  datafile$inst <- gsub(" company", "", datafile$inst)
  datafile$inst <- gsub("csiro", "csiro", datafile$inst)
  datafile$inst <- gsub("state university university", "state university", datafile$inst)
  datafile$inst <- gsub("north carolina aandt state university", "north carolina a & t state university", datafile$inst)
  datafile$inst <- gsub(" manaaki whenua", "", datafile$inst)
  datafile$inst <- gsub(" headquarters", "", datafile$inst)
  datafile$inst <- gsub("vuniversity", "university", datafile$inst)
  datafile$inst <- gsub(" (imedea)", "", datafile$inst)
  datafile$inst <- gsub(" (unicamp)", "", datafile$inst)
  datafile$inst <- gsub("dhistoire", "dhistoire naturelle", datafile$inst)
  datafile$inst <- gsub("naturelle naturelle", "naturelle", datafile$inst)
  datafile$inst <- gsub("museum national dhistoire naturelle", "national museum of natural history france", datafile$inst)
  datafile$inst <- gsub("oregan", "oregon", datafile$inst)
  datafile$inst <- gsub("philips university", "philips university marburg", datafile$inst)
  # datafile$inst<-gsub("(csic)", "", datafile$inst)
  datafile$inst[datafile$inst == "university of mexico"] <- "universidad nacional autonoma de mexico"
  datafile$inst[datafile$inst == "cary cary institute of ecosystem studies"] <- "cary institute of ecosystem studies"
  datafile$inst[datafile$inst == "university of california nceas"] <- "national center for ecological analysis and synthesis"
  datafile$inst <- gsub("khorasan agricultural and natural resources res ctr", "khorasan agricultural and natural resources research center", datafile$inst)
  datafile$inst[datafile$inst == "texas technical university"] <- "texas tech university"
  datafile$inst[datafile$inst == "mississippi state univ"] <- "mississippi state university"
  datafile$inst[datafile$inst == "empresa brasileira de pesquisa agropecuária"] <- "empresa brasileira de pesquisa agropecuaria"
  datafile$inst[datafile$inst == "technion technion israel institute of technology"] <- "technion israel institute of technology"
  datafile$inst[datafile$inst == "universidad catolica de chile"] <- "pontifica universidad catolica de chile"
  datafile$inst[datafile$inst == "university of marburg"] <- "philipps university marburg"
  datafile$inst[datafile$inst == "norwegian inst nat management"] <- "norwegian institute for nature research"
  datafile$inst[datafile$inst == "philipps university"] <- "philipps university marburg"
  datafile$inst[datafile$inst == "museum dhistoire naturelle"] <- "national museum of natural history france"
  datafile$inst[datafile$inst == "national autonomous university of mexico"] <- "universidad nacional autonoma de mexico"
  datafile$inst[datafile$inst == "landcare research"] <- "manaaki whenua landcare research"
  datafile$unit[datafile$inst == "usgs/nrii"] <- "nrii"
  datafile$inst[datafile$inst == "usgs/nrii"] <- "usgs national resources inventory"
  datafile$unit[datafile$inst == "florida international university and center for tropical plant conservation"] <- "center for tropical plant conservation"
  datafile$inst[datafile$inst == "florida international university and center for tropical plant conservation"] <- "florida international university"
  datafile$inst[datafile$inst == "ecole normale supérieure"] <- "ecole normale superieure"
  datafile$inst[datafile$inst == "université de sherbrooke"] <- "university of sherbrooke"
  datafile$inst[datafile$inst == "pontificia universidad católica de chile"] <- "pontificia universidad catolica de chile"
  datafile$inst[datafile$inst == "university of tromsø"] <- "university of tromso"
  # datafile$inst[datafile$inst=="universit\xfc\xbe\x8c\xa3\xa0\xbc montpellier ii"]<-"universite montpellier ii"
  # datafile$inst[datafile$inst=="universit\xfc\xbe\x8d\x83\xa0\xbct z\xfc\xbe\x8c\x93\xa0\xbcrich irchel"]<-"university of zurich irchel"
  datafile$inst[datafile$inst == "texas a & m univ."] <- "texas a & m university"
  datafile$inst[datafile$inst == "texas a & m"] <- "texas a & m university"
  datafile$inst[datafile$inst == "aberdeen"] <- "university of aberdeen"
  datafile$inst[datafile$inst == "aberdeen"] <- "university of aberdeen"
  datafile$inst[datafile$inst == "cambridge university"] <- "university of cambridge"
  datafile$inst[datafile$inst == "universityof"] <- "university of"
  datafile$inst[datafile$inst == "aberystwyth"] <- "aberystwyth university"
  datafile$inst[datafile$inst == "alabama a and m"] <- "alabama a and m university"
  datafile$inst[datafile$inst == "alterra"] <- "wageningen university and research"
  datafile$inst[datafile$inst == "waginen university"] <- "wageningen university and research"
  datafile$inst[datafile$inst == "agricultural university" & datafile$country == "netherlands"] <- "wageningen university and research"
  datafile$inst[datafile$inst == "wageningen"] <- "wageningen university and research"
  datafile$inst[datafile$inst == "wageningen university"] <- "wageningen university and research"
  datafile$notes[datafile$inst == "usgs and university of miami"] <- "secondary inst: university of miami"
  datafile$inst[datafile$inst == "usgs and university of miami"] <- "usgs"
  datafile$notes[datafile$inst == "usgs and university of california santa barbara"] <- "secondary inst: university of california santa barbara"
  datafile$inst[datafile$inst == "usgs and university of california santa barbara"] <- "usgs"
  datafile$inst[datafile$inst == "alterra research institute for the green world"] <- "wageningen university and research"
  datafile$inst[datafile$inst == "assicuates"] <- "associates"
  datafile$inst[datafile$inst == "bangor"] <- "bangor university"
  datafile$inst[datafile$inst == "british anarctic society"] <- "british antarctic survey"
  datafile$inst[datafile$inst == "british antartic survey"] <- "british antarctic survey"
  datafile$inst[datafile$inst == "centro de investigaciones y experiencias forestales (cief)"] <- "centro de investigaciones y experiencias forestales"
  datafile$inst[datafile$inst == "chinese academy of sciencies"] <- "chinese academy of sciences"
  datafile$inst[datafile$inst == "christian albrechts universitat zu kiel"] <- "christian albrechts universitat kiel"
  datafile$inst[datafile$inst == "consejo superior de investigaciones cientificas (csic)"] <- "csic"
  datafile$inst[datafile$inst == "csic upv"] <- "csic upv ingenio"
  datafile$inst[datafile$inst == "durham"] <- "durham university"
  datafile$inst[datafile$inst == "eawag/eth"] <- "swiss federal institute of aquatic science and technology"
  datafile$inst[datafile$inst == "edinburgh"] <- "university of edinburgh"
  datafile$inst[datafile$inst == "freie universitt berlin"] <- "free university of berlin"
  datafile$inst[datafile$inst == "freie university of berlin"] <- "free university of berlin"
  datafile$inst[datafile$inst == "g√∂ttingen"] <- "university of gottingen"
  datafile$inst[datafile$inst == "gatty marine lab (university of saint andrews"] <- "university of st andrews"
  datafile$inst[datafile$inst == "georg august universitat gottingen"] <- "university of gottingen"
  datafile$inst[datafile$inst == "glasgow"] <- "university of glasgow"
  datafile$inst[datafile$inst == "gottingen university"] <- "university of gottingen"
  datafile$inst[datafile$inst == "helsinki"] <- "university of helsinki"
  datafile$inst[datafile$inst == "humboldt universitat zu berlin"] <- "humboldt university of berlin"
  datafile$inst[datafile$inst == "humboldt university"] <- "humboldt university of berlin"
  datafile$inst[datafile$inst == "institut mediterrani destudis avencats (csic uib)"] <- "csic institut mediterrani destudis avencats"
  datafile$inst[datafile$inst == "instituto de ciencias del mar csic"] <- "csic instituto de ciencias del mar"
  datafile$inst[datafile$inst == "izw"] <- "leibniz institute for zoo and wildlife research"
  datafile$inst[datafile$inst == "kansas state"] <- "kansas state university"
  datafile$inst[datafile$inst == "karlsruhe"] <- "karlsruhe institute of technology"
  datafile$inst[datafile$inst == "pontificia universidade catolica de chile"] <- "pontificia universidad catolica de chile"
  datafile$inst[datafile$inst == "khorasan agricultural and natural resources res. ctr"] <- "khorasan agricultural and natural resources research center"
  datafile$inst[datafile$inst == "kyushu unniversity"] <- "kyushu university"
  datafile$inst[datafile$inst == "lehman college cuny"] <- "cuny lehman college"
  datafile$inst[datafile$inst == "los alamos national lab"] <- "los alamos national laboratory"
  datafile$inst[datafile$inst == "ludwig maximilians universitat munchen"] <- "ludwig maximilian universitat munchen"
  datafile$inst[datafile$inst == "lyme regis"] <- ""
  datafile$inst[datafile$inst == "macaulay¬†institute"] <- "macaulay land use research institute"
  datafile$inst[datafile$inst == "max planck inst plant breeding res"] <- "max planck institute for plant breeding research"
  datafile$inst[datafile$inst == "max planck institut fur verhaltensphysiologie"] <- "max planck institute for behavioral physiology"
  datafile$inst[datafile$inst == "max planck institute jena"] <- "max planck institute for the science of human history"
  datafile$inst[datafile$inst == "monterey bay aquarium research institute (mbari)"] <- "monterey bay aquarium research institute"
  datafile$inst[datafile$inst == "mt holyoke college"] <- "mount holyoke college"
  datafile$inst[datafile$inst == "museum national dhistoire naturelle"] <- "national museum of natural history france"
  datafile$inst[datafile$inst == "national institute of water and atmospheric research"] <- "national institute of water and atmospheric research"
  datafile$inst[datafile$inst == "nc state university"] <- "north carolina state university"
  datafile$inst[datafile$inst == "neri"] <- "neri national environmental research institute"
  datafile$inst[datafile$inst == "netherlands institute of ecology; wageningen university and research centre netherlands institute of ecology"] <- "netherlands institute of ecology nioo knaw"
  datafile$inst[datafile$inst == "neuchatel"] <- "university of neuchatel"
  datafile$inst[datafile$inst == "new college of the university of south florida"] <- "new college of florida"
  datafile$inst[datafile$inst == "nioz and groningen"] <- "netherlands institute for sea research"
  datafile$inst[datafile$inst == "norwich"] <- "john innes centre"
  datafile$inst[datafile$inst == "oklahoma state"] <- "oklahoma state university"
  datafile$inst[datafile$inst == "oregan state university"] <- "oregon state university"
  datafile$inst[datafile$inst == "ornl"] <- "oak ridge national laboratory"
  datafile$inst[datafile$inst == "plymouth"] <- "marine biological association"
  datafile$inst[datafile$inst == "plymouth marine lab"] <- "plymouth marine laboratory"
  datafile$inst[datafile$inst == "polish academy of sciences"] <- "polish academy of science"
  datafile$inst[datafile$inst == "research triangle park"] <- "basf"
  datafile$inst[datafile$inst == "royal botanical gardens"] <- "royal botanical gardens kew"
  datafile$inst[datafile$inst == "rwth aachen"] <- "rwth aachen university"
  datafile$unit[datafile$inst == "scripps institute of oceanography"] <- "scripps institution of oceanography"
  datafile$inst[datafile$inst == "scripps institute of oceanography"] <- "university of california santa diego"
  datafile$unit[datafile$inst == "scripps institution  of oceanography"] <- "scripps institution of oceanography"
  datafile$inst[datafile$inst == "scripps institution  of oceanography"] <- "university of california santa diego"
  datafile$inst[datafile$inst == "sheffield"] <- "university of sheffield"
  datafile$inst[datafile$inst == "sheffield"] <- "university of sheffield"
  datafile$inst[datafile$inst == "smithsonian trop res inst"] <- "smithsonian tropical research institute"
  datafile$inst[datafile$inst == "st. andrews"] <- "university of st andrews"
  datafile$inst[datafile$inst == "state university of new york"] <- "suny state university of new york"
  datafile$inst[datafile$inst == "suny albany"] <- "suny albany"
  datafile$inst[datafile$inst == "york" & (datafile$country == "uk" | datafile$country == "united kingdom" | datafile$country == "england")] <- "university of york"
  datafile$inst[datafile$inst == "york" & datafile$country == "canada"] <- "york university"
  datafile$inst[datafile$inst == "york u"] <- "york university"
  datafile$inst[datafile$inst == "suny stonybrook"] <- "suny stony brook"
  datafile$inst[datafile$inst == "suny stonybrook"] <- "suny stony brook"
  datafile$inst[datafile$inst == "texas a & m university"] <- "texas a and m university"
  datafile$inst[datafile$inst == "texas tech"] <- "texas tech university"
  datafile$inst[datafile$inst == "u.s.f.w.s. national ecology research center"] <- "usfws national ecology research center"
  datafile$inst[datafile$inst == "u.s.f.w.s. northern prairie wildlife research center"] <- "usgs northern prairie wildlife research center"
  datafile$inst[datafile$inst == "unam"] <- "universidad nacional autonoma de mexico"
  datafile$inst[datafile$inst == "university buffalo"] <- "university of buffalo"
  datafile$inst[datafile$inst == "university college of aberystwyth"] <- "aberystwyth university"
  datafile$inst[datafile$inst == "university of arkansas little rock"] <- "university of arkansas little rock"
  datafile$inst[datafile$inst == "university of colorado"] <- "university of colorado boulder"
  datafile$inst[datafile$inst == "university of durham"] <- "durham university"
  datafile$inst[datafile$inst == "university of nebranska"] <- "university of nebraska"
  datafile$inst[datafile$inst == "university of saint andrews"] <- "university of st andrews"
  datafile$inst[datafile$inst == "university of southern florida"] <- "university of south florida"
  datafile$inst[datafile$inst == "university of texas at austin"] <- "university of texas austin"
  datafile$inst[datafile$inst == "university sheffield"] <- "university of sheffield"
  datafile$inst[datafile$inst == "university<ca>of<ca>california<ca>santa<ca>cruz"] <- "university of california santa cruz"
  datafile$inst[datafile$inst == "washington university"] <- "washington university in st louis"
  datafile$inst[datafile$inst == "washington university of st louis"] <- "washington university in st louis"
  datafile$inst[datafile$inst == "western michigan university"] <- "western michigan university"
  datafile$inst[datafile$inst == "wisconsin"] <- "university of wisconsin"
  datafile$inst[datafile$inst == "woods hole oceanographic institute"] <- "woods hole oceanographic institution"
  datafile$inst[datafile$inst == "woods hole research center"] <- "woods hole oceanographic institution"
  datafile$notes[datafile$inst == "university of natural resources and applied life sciences boku"] <- "institution aka boku"
  datafile$inst[datafile$inst == "university of natural resources and applied life sciences boku"] <- "university of natural resources and applied life sciences vienna"

  datafile$inst[datafile$inst == "usu"] <- "utah state university"
  # datafile$inst[datafile$LAST_NAME == "luque"] <- "institut national de recherche en sciences et technologies pour lenvironnement et lagriculture"
  datafile$inst[datafile$LAST_NAME == "luque"] <- "national research institute of science and technology for environment and agriculture (irstea)"
  datafile$inst[datafile$inst == "irstea national research institute of science and technology for environment and agriculture"] <-"national research institute of science and technology for environment and agriculture (irstea)"
  # datafile$inst[datafile$inst=="<a0>forestry and forest products research institute"]<-"forestry and forest products research institute"
  # datafile$inst[datafile$inst=="<a0>universit<e9> claude bernard lyon 1"]<-"universite claude bernard lyon 1"
  # datafile$inst<-gsub("landscape<a0>ecology","landscape ecology", datafile$inst)
  # datafile$inst<-gsub("universit<8a>t","universitat", datafile$inst)
  datafile$inst <- gsub("institue", "institute", datafile$inst)
  datafile$inst <- gsub("univerity", "university", datafile$inst)
  datafile$inst[datafile$inst == "canadia forest service"] <- "canadian forest service"
  datafile$inst[datafile$inst == "lowa state university"] <- "iowa state university"
  datafile$inst[datafile$inst == "iowa state u"] <- "iowa state university"
  datafile$inst[datafile$inst == "iowa state"] <- "iowa state university"
  datafile$inst[datafile$inst == "university of autonoma del estado de hidalgo"] <- "university of autonoma del estado de hidalgo"
  datafile$inst[datafile$inst == "estern illinois  university"] <- "eastern illinois university"
  datafile$inst[datafile$inst == "syngenta crop protection"] <- "syngenta crop protection inc"
  datafile$inst[datafile$inst == "alterra"] <- "alterra research institute for the green world"
  datafile$inst[datafile$inst == "alterra research instituut voor de groene ruimte"] <- "alterra research institute for the green world"
  datafile$inst[datafile$inst == "arizona state university west"] <- "arizona state university west campus"
  datafile$inst[datafile$inst == "commonwealth scientific and industrial research organisation "] <- "commonwealth scientific and industrial research organisation"
  datafile$inst[datafile$inst == "dartmouth university"] <- "dartmouth college"
  datafile$inst[datafile$inst == "field museum of natural history"] <- "field museum of natural history"
  datafile$inst[datafile$inst == "freie universitôøωt berlin"] <- "freie university of berlin"
  datafile$inst[datafile$inst == "jawaharial nehru university"] <- "jawaharlal nehru university"
  datafile$inst[datafile$inst == "jawaharial nehruniversity of university"] <- "jawaharlal nehru university"
  datafile$inst[datafile$inst == "jawaharlal nehruniversity of university"] <- "jawaharlal nehru university"
  datafile$inst[datafile$inst == "royal society for protection of bitds"] <- "royal society for protection of birds"

  datafile$inst[datafile$inst == "kansas state university "] <- "kansas state university"
  datafile$inst[datafile$inst == "landcare research  lincoln"] <- "landcare research ltd"
  datafile$inst[datafile$inst == "lehman college"] <- "cuny lehman college"
  datafile$inst[datafile$inst == "mississippi state univ."] <- "mississippi state university"
  datafile$inst[datafile$inst == "monks wood"] <- "monks wood experiment station"
  datafile$inst[datafile$inst == "oklahoma state"] <- "oklahoma state university"
  datafile$inst[datafile$inst == "syngenta crop protection inc"] <- "syngenta crop protection inc"
  datafile$inst[datafile$inst == "texas tech"] <- "texas tech university"
  datafile$inst[datafile$inst == "universidad national autonoma de mexico"] <- "universidad nacional autonoma de mexico"
  datafile$inst[datafile$inst == "universidade federal de alagoas"] <- "universidade federal de alagoas"
  datafile$inst[datafile$inst == "universit√© de montreal"] <- "universite de montreal"
  datafile$inst[datafile$inst == "universit√© joseph fourier"] <- "universite joseph fourier"
  datafile$inst[datafile$inst == "universite pierre and marie curie"] <- "universite pierre et marie curie"
  datafile$inst[datafile$inst == "uc santa cruz"] <- "university of california santa cruz"
  datafile$inst[datafile$inst == "university of edinburhg"] <- "university of edinburgh"
  datafile$inst[datafile$inst == "university of leuvenn"] <- "university of leuven"
  datafile$inst[datafile$inst == "university of los andes bogota"] <- "universidad de los andes"
  datafile$inst[datafile$inst == "university of missisippi"] <- "university of mississippi"
  datafile$inst[datafile$inst == "university of montanna"] <- "university of montana"
  datafile$inst[datafile$inst == "unsw"] <- "university of new south wales"
  datafile$inst[datafile$inst == "mammal research institute (university of pretoria)"] <- "university of pretoria"
  datafile$inst[datafile$inst == "gatty marine lab (university of saint andrews)"] <- "university of st andrews"
  datafile$inst[datafile$inst == "university of st andrews"] <- "university of st andrews"
  datafile$inst[datafile$inst == "university of stirlinng"] <- "university of stirling"
  datafile$inst[datafile$inst == "university of tenessee"] <- "university of tennessee"
  datafile$inst[datafile$inst == "university of texas at austin"] <- "university of texas austin"
  datafile$inst[datafile$inst == "university of wisconsin"] <- "university of wisconsin"
  datafile$inst[datafile$inst == "victorian school of forestry"] <- "victorian school of forestry"
  datafile$inst[datafile$inst == "vrije universiteit amsterdam"] <- "vrije universiteit amsterdam"
  datafile$inst[datafile$inst == "vrije university amsterdam"] <- "vrije universiteit amsterdam"
  datafile$inst[datafile$inst == "vrije universiteit"] <- "vrije universiteit amsterdam"
  datafile$inst[datafile$inst == "washington u st louis"] <- "washington university in st louis"
  datafile$inst[datafile$inst == "western cotton research lab."] <- "usda western cotton research lab"
  datafile$inst[datafile$inst == "western michigan university"] <- "western michigan university"
  datafile$inst[datafile$inst == "woods hole"] <- "woods hole oceanographic institution"
  datafile$inst[datafile$inst == "wwf"] <- "world wildlife fund"
  datafile$unit[datafile$inst == "university of saint andrews"] <- "gatty marine lab"
  datafile$unit[datafile$inst == "university of pretoria"] <- "mammal research institute"
  datafile$country[datafile$inst == "iowa state university"] <- "usa"
  datafile$country[datafile$inst == "aarhus university"] <- "denmark"
  datafile$country[datafile$inst == "southern illinois u"] <- "usa"
  datafile$inst[datafile$inst == "southern illinois u"] <- "southern illinois university"

  ##############################################################
  ##############################################################
  # dividing some names for inst into inst and unit
  ##############################################################
  ##############################################################

  datafile$unit[datafile$inst == "harvard university herbaria"] <- "hu herbaria"
  datafile$inst[datafile$inst == "harvard university herbaria"] <- "harvard university"

  datafile$unit[datafile$inst == "harvard university medical school"] <- "hu medical school"
  datafile$inst[datafile$inst == "harvard university medical school"] <- "harvard university"

  datafile$unit[datafile$inst == "boston unveristy marine program"] <- "marine program"
  datafile$inst[datafile$inst == "boston unveristy marine program"] <- "boston university"

  datafile$unit[datafile$inst == "harvard university museum of comparative zoology"] <- "museum of comparative zoology"
  datafile$inst[datafile$inst == "harvard university museum of comparative zoology"] <- "harvard university"

  datafile$unit[datafile$inst == "harvard forest"] <- "harvard forest"
  datafile$inst[datafile$inst == "harvard forest"] <- "harvard university"

  datafile$unit[datafile$inst == "harvard university arnold arboretum"] <- "arnold arboretum"
  datafile$inst[datafile$inst == "harvard university"] <- "harvard university"
  datafile$unit[datafile$inst == "harvard university arnold arboretum"] <- "arnold arboretum"
  datafile$inst[datafile$inst == "harvard university arnold arboretum"] <- "harvard university"

  datafile$unit[datafile$inst == "instituto de ecologia unam"] <- "instituto de ecologia"
  datafile$inst[datafile$inst == "instituto de ecologia unam"] <- "universidad nacional autonoma de mexico"

  datafile$unit[datafile$inst == "harvard university museum of comparative zoology"] <- "museum of comparative zoology"
  datafile$inst[datafile$inst == "harvard university museum of comparative zoology"] <- "harvard university"

  datafile$unit[datafile$inst == "wageiningen university research center alterra"] <- "research center alterra"
  datafile$inst[datafile$inst == "wageiningen university research center alterra"] <- "wageiningen university"


  # datafile$inst<-gsub("l\x9fneburg","lunenburg", datafile$inst)
  # datafile$inst<-gsub("universit\x8at","universitat", datafile$inst)
  # datafile$inst<-gsub("g\xf6ttingen","gottingen", datafile$inst)
  # datafile$inst<-gsub("m\x82xico","mexico", datafile$inst)
  # datafile$inst<-gsub("universit\x82","universite", datafile$inst)
  # datafile$inst<-gsub("universit\xe9","universite", datafile$inst)
  # datafile$inst<-gsub("landscape\xa0ecology","landscape ecology", datafile$inst)
  # datafile$inst<-gsub("aut\xfc\xbe\x8c\xa6\x84\xbcnoma","autonoma", datafile$inst)
  # datafile$inst<-gsub("montr\xfc\xbe\x8e\x96\x94\xbcal","montreal", datafile$inst)
  # datafile$inst<-gsub("<a0>universit<e9>","universite", datafile$inst)
  # datafile$inst<-gsub("\xfc\xbe\x8c\x86\x84\xbc","", datafile$inst)
  # datafile$inst<-gsub("rwth aachen\xcauniversity","rwth aachen university", datafile$inst)
  # datafile$inst<-gsub("macquarie university \xca","macquarie university", datafile$inst)
  # datafile$inst<-gsub("leibniz institute for zoo and wildlife research\xfc\xbe\x98\x96\x8c\xbc","leibniz institute for zoo and wildlife research", datafile$inst)


  datafile$inst <- as.character(datafile$inst)
  datafile$inst <- tolower(datafile$inst)

  datafile$inst[grepl("brown univ", datafile$inst)] <- "brown university"
  datafile$inst[grepl("cary", datafile$inst)] <- "cary institute of ecosystem studies"
  datafile$inst[grepl("ets ingenieros", datafile$inst)] <- "ets ingenieros de montes"
  gsub("macaulayooinstitute", "istituto per lambiente marino costiero", datafile$inst)
  datafile$inst[grepl("macaulay", datafile$inst)] <- "macaulay land use research institute"
  datafile$inst[grepl("osna", datafile$inst)] <- "universitat osnabruck"
  datafile$inst[grepl("de montreal", datafile$inst)] <- "university of montreal"

  datafile$inst <- ifelse(grepl("terrestrial ecology", datafile$inst), "institute of terrestrial ecology", datafile$inst)
  # datafile$inst<-ifelse((datafile$country="canada" & grepl("queen",datafile$inst)),"queens university",datafile$inst)

  datafile$inst <- gsub("(retired) natural history museum london", "natural history museum", datafile$inst)
  datafile$inst <- gsub("university of aarhus", "aarhus university", datafile$inst)

  datafile$inst <- gsub("university of aarhus", "aarhus university", datafile$inst)
  datafile$inst <- gsub("acadia u", "aarhus university", datafile$inst)
  datafile$inst <- gsub("university of aarhus", "aarhus university", datafile$inst)
  datafile$inst <- gsub("university of aarhus", "aarhus university", datafile$inst)
  datafile$inst <- gsub("universityniversity", "university", datafile$inst)

  datafile$inst[datafile$inst == "university of california" &
    datafile$city == "riverside"] <- "university of california riverside"

  datafile$inst[datafile$inst == "university of california" &
    datafile$city == "santa cruz"] <- "university of california santa cruz"

  datafile$inst[datafile$inst == "university of california" &
    datafile$city == "oakland"] <- "university of california berkeley"

  datafile$inst[datafile$inst == "university of california" &
    datafile$city == "berkeley"] <- "university of california berkeley"

  datafile$inst[datafile$inst == "university of california" &
    datafile$city == "los angeles"] <- "university of california los angeles"

  datafile$inst[datafile$inst == "university of california" &
    datafile$city == "davis"] <- "university of california davis"

  datafile$city[datafile$inst == "university of massachusetts amherst"] <- "amherst"

  datafile$city[datafile$inst == "rutgers university"] <- "new brunswick"
  datafile$state[datafile$inst == "rutgers university"] <- "nj"

  datafile$city[datafile$inst == "university of new south wales"] <- NA



  datafile$inst[datafile$inst == "queen mary university of london"] <- "queen mary university of london"
  datafile$inst[datafile$inst == "institute for terrestrial ecology" & datafile$country == "united kingdom"] <- "institute of terrestrial ecology"

  datafile$inst[datafile$inst == "queen marys university of london"] <- "queen mary university of london"
  datafile$inst[datafile$inst == "university of london queen mary"] <- "queen mary university of london"
  datafile$inst[datafile$inst == "queen marys university"] <- "queen mary university of london"
  datafile$unit[datafile$inst == "acadia u"] <- "aarhus university"
  datafile$unit[datafile$inst == "aberdeen university"] <- "university of aberdeen"
  datafile$unit[datafile$inst == "agresearch"] <- "agresearch ltd"

  datafile$unit[datafile$inst == "agriculture and agri food"] <- "agriculture and agri food canada"
  datafile$country[datafile$inst == "queens college city university of new york"] <- "usa"
  datafile$inst[datafile$inst == "northern prairie wildlife research center"] <- "usgs northern prairie wildlife research center"
  datafile$inst[datafile$inst == "usgs prairie wildlife research center"] <- "usgs northern prairie wildlife research center"
  datafile$country[datafile$inst == "usgs northern prairie wildlife research center"] <- "usa"
  datafile$country[datafile$inst == "ohio northern university"] <- "usa"
  datafile$country[datafile$inst == "ohio state university"] <- "usa"
  datafile$country[datafile$inst == "ohio wesleyan university"] <- "usa"
  datafile$country[datafile$inst == "oklahoma state university"] <- "usa"
  datafile$country[datafile$inst == "old dominion university"] <- "usa"
  datafile$country[datafile$inst == "oregon state university"] <- "usa"
  datafile$country[datafile$inst == "vrije university amsterdam"] <- "netherlands"
  datafile$country[datafile$inst == "penn state university"] <- "usa"
  datafile$inst <- gsub("penn state university", "pennsylvaia state university", datafile$inst)
  datafile$country[datafile$inst == "portland state university"] <- "usa"
  datafile$country[datafile$inst == "rochester institute of technology"] <- "usa"
  datafile$country[datafile$inst == "rocky mountain research station"] <- "usa"
  datafile$inst[datafile$inst == "rocky mountain research station"] <- "usfs rocky mountain research station"
  datafile$country[datafile$inst == "school of renewable resources louisiana state u"] <- "usa"
  datafile$country[datafile$inst == "school of renewable resources louisiana state university"] <- "usa"
  datafile$unit[datafile$inst == "school of renewable resources louisiana state u"] <- "school of renewable resources"
  datafile$inst[datafile$inst == "school of renewable resources louisiana state u"] <- "louisiana state university"
  datafile$unit[datafile$inst == "school of renewable resources louisiana state university"] <- "school of renewable resources"
  datafile$inst[datafile$inst == "school of renewable resources louisiana state university"] <- "louisiana state university"
  datafile$country[datafile$inst == "smithsonian institution"] <- "usa"
  datafile$country[datafile$inst == "smithsonian migratory bird center"] <- "usa"
  datafile$country[datafile$inst == "smithsonian national museum of natural history"] <- "usa"
  datafile$inst[datafile$inst == "stri"] <- "smithsonian tropical research institute"
  datafile$country[datafile$inst == "smithsonian tropical research institute"] <- "usa"
  datafile$country[datafile$inst == "southeastern louisiana"] <- "usa"
  datafile$country[datafile$inst == "southern illinois university"] <- "usa"
  datafile$country[datafile$inst == "stony brook university"] <- "usa"
  datafile$country[datafile$inst == "tall timbers research station"] <- "usa"
  datafile$country[datafile$inst == "texas christian university"] <- "usa"
  datafile$country[datafile$inst == "texas tech university"] <- "usa"
  datafile$country[datafile$inst == "tulane university"] <- "usa"
  datafile$country[datafile$inst == "united state fish and wildlife service"] <- "usa"
  datafile$country[datafile$inst == "universidad de buenos aires"] <- "argentina"
  datafile$country[datafile$inst == "universidad de los andes"] <- "colombia"
  datafile$country[datafile$inst == "universidad simon bolivar"] <- "venezuela"
  datafile$country[datafile$inst == "universidadautonoma del estado de hidalgo"] <- "mexico"
  datafile$inst <- gsub("universidadautonoma", "universidad autonoma", datafile$inst)
  datafile$country[datafile$inst == "university illinois urbana champaign"] <- "usa"
  datafile$country[datafile$inst == "university of adelaide"] <- "australia"
  datafile$country[datafile$inst == "university of alaska"] <- "usa"
  datafile$country[datafile$inst == "university of alaska fairbanks"] <- "usa"
  datafile$country[datafile$inst == "university of alaska museum"] <- "usa"
  datafile$country[datafile$inst == "university of alberta"] <- "canada"
  datafile$country[datafile$inst == "university of arizona"] <- "usa"
  datafile$country[datafile$inst == "university of auckland"] <- "new zealand"
  datafile$country[datafile$inst == "university of bonn"] <- "germany"
  datafile$country[datafile$inst == "university of british columbia"] <- "canada"
  datafile$country[datafile$inst == "university of california"] <- "usa"
  datafile$country[datafile$inst == "university of california berkeley"] <- "usa"
  datafile$country[datafile$inst == "university of california davis"] <- "usa"
  datafile$country[datafile$inst == "university of california san diego"] <- "usa"
  datafile$country[datafile$inst == "university of california riverside"] <- "usa"
  datafile$country[datafile$inst == "university of california santa cruz"] <- "usa"
  datafile$country[datafile$inst == "university of cambridge"] <- "united kingdom"
  datafile$country[datafile$inst == "university of canterbury"] <- "new zealand"
  datafile$country[datafile$inst == "university of central arkansas"] <- "usa"
  datafile$country[datafile$inst == "university of chicago"] <- "usa"
  datafile$country[datafile$inst == "university of colorado boulder"] <- "usa"
  datafile$country[datafile$inst == "university of colorado denver"] <- "usa"
  datafile$country[datafile$inst == "university of connecticut"] <- "usa"
  datafile$country[datafile$inst == "university of copenhagen"] <- "denmark"
  datafile$country[datafile$inst == "university of dayton"] <- "usa"
  datafile$country[datafile$inst == "university of delaware"] <- "usa"
  datafile$country[datafile$inst == "university of exeter"] <- "united kingdom"
  datafile$country[datafile$inst == "university of georgia"] <- "usa"
  datafile$country[datafile$inst == "university of hawaii manoa"] <- "usa"
  datafile$country[datafile$inst == "university of illinois"] <- "usa"
  datafile$country[datafile$inst == "university of illinois universityrbana champaign"] <- "usa"
  datafile$inst <- gsub("universityrbana", "university urbana", datafile$inst)
  datafile$country[datafile$inst == "university of illinois urbana champaign"] <- "usa"
  datafile$country[datafile$inst == "university of kansas"] <- "usa"
  datafile$country[datafile$inst == "university of london"] <- "united kingdom"
  datafile$country[datafile$inst == "university of maine"] <- "usa"
  datafile$country[datafile$inst == "university of manitoba"] <- "canada"
  datafile$country[datafile$inst == "university of maryland"] <- "usa"
  datafile$country[datafile$inst == "university of maryland baltimore county"] <- "usa"
  datafile$country[datafile$inst == "university of massachusetts"] <- "usa"
  datafile$country[datafile$inst == "university of massachusetts amherst"] <- "usa"
  datafile$country[datafile$inst == "university of melbourne"] <- "australia"
  datafile$country[datafile$inst == "university of michigan dearborn"] <- "usa"
  datafile$country[datafile$inst == "university of minnesota"] <- "usa"
  datafile$country[datafile$inst == "university of mississippi"] <- "usa"
  datafile$country[datafile$inst == "university of missouri st louis"] <- "usa"
  datafile$country[datafile$inst == "university of montana"] <- "usa"
  datafile$country[datafile$inst == "university of nevada"] <- "usa"
  datafile$country[datafile$inst == "university of nevada reno"] <- "usa"
  datafile$country[datafile$inst == "university of new brunswick"] <- "canada"
  datafile$country[datafile$inst == "university of northern british columbia"] <- "canada"
  datafile$country[datafile$inst == "university of northern colorado"] <- "usa"
  datafile$country[datafile$inst == "university of oklahoma"] <- "usa"
  datafile$country[datafile$inst == "university of pretoria"] <- "south africa"
  datafile$country[datafile$inst == "university of queensland"] <- "australia"
  datafile$country[datafile$inst == "university of rhode island"] <- "usa"
  datafile$country[datafile$inst == "university of st andrews"] <- "scotland"
  datafile$country[datafile$inst == "university of saskatchewan"] <- "canada"
  datafile$country[datafile$inst == "university of south dakota"] <- "usa"
  datafile$country[datafile$inst == "university of southern mississippi"] <- "usa"
  datafile$country[datafile$inst == "university of tel aviv"] <- "israel"
  datafile$country[datafile$inst == "university of tennessee"] <- "usa"
  datafile$country[datafile$inst == "university of victoria"] <- "canada"
  datafile$country[datafile$inst == "university of virginia"] <- "usa"
  datafile$country[datafile$inst == "university of washington"] <- "usa"
  datafile$country[datafile$inst == "university of windsor"] <- "canada"
  datafile$country[datafile$inst == "university of wisconsin"] <- "usa"
  datafile$country[datafile$inst == "university of wisconsin milwaukee"] <- "usa"
  datafile$country[datafile$inst == "university of wisconsin parkside"] <- "usa"
  datafile$country[datafile$inst == "university of wurzburg"] <- "germany"
  datafile$country[datafile$inst == "university of wyoming"] <- "usa"
  datafile$country[datafile$inst == "universityautonoma del estado hidalgo"] <- "mexico"
  datafile$inst <- gsub("universityautonoma", "universidad autonoma", datafile$inst)
  datafile$country[datafile$inst == "universitynacional de misiones conicet"] <- "argentina"
  datafile$inst <- gsub("universitynacional", "university nacional", datafile$inst)
  datafile$country[datafile$inst == "universitypierre et marie curie"] <- "france"
  datafile$inst <- gsub("universitypierre", "university pierre", datafile$inst)
  datafile$country[datafile$inst == "uppsala university"] <- "sweden"
  datafile$country[datafile$inst == "us forest service"] <- "usa"
  datafile$country[datafile$inst == "us geological survey"] <- "usa"
  datafile$country[datafile$inst == "usgs"] <- "usa"
  datafile$country[datafile$inst == "us geological survey alaska science center"] <- "usa"
  datafile$country[datafile$inst == "us geological survey biological resources division"] <- "usa"
  datafile$country[datafile$inst == "us geological survey fort collins science center"] <- "usa"
  datafile$country[datafile$inst == "us geological survey northern prairie wildlife research center"] <- "usa"
  datafile$country[datafile$inst == "usgs northern prairie wildlife research center"] <- "usa"
  datafile$country[datafile$inst == "us geological survey patuxent wildlife research center"] <- "usa"
  datafile$country[datafile$inst == "us geological survey wisconsin cooperative wildlife research unit"] <- "usa"
  datafile$country[datafile$inst == "usda aphis"] <- "usa"
  datafile$country[datafile$inst == "usfs rocky mountain research station"] <- "usa"
  datafile$country[datafile$inst == "usfws alaska region"] <- "usa"
  datafile$country[datafile$inst == "usfws idaho fish and wildlife office"] <- "usa"
  datafile$country[datafile$inst == "utah state university"] <- "usa"
  datafile$country[datafile$inst == "vassar college"] <- "usa"
  datafile$country[datafile$inst == "villanova university"] <- "usa"
  datafile$inst[datafile$inst == "virginia polytechnic institute state university"] <- "virginia polytechnic institute and state university"
  datafile$country[datafile$inst == "virginia polytechnic institute and state university"] <- "usa"
  datafile$country[datafile$inst == "virginia polytechnic institute state university"] <- "usa"
  datafile$country[datafile$inst == "washington state university"] <- "usa"
  datafile$country[datafile$inst == "western michigan university"] <- "usa"
  datafile$country[datafile$inst == "wichita state university"] <- "usa"
  datafile$country[datafile$inst == "york university"] <- "canada"
  datafile$country[datafile$inst == "zoological society of london"] <- "united kingdom"
  datafile$country[datafile$inst == "prairie wildlife research center"] <- "usa"
  datafile$country[datafile$inst == "queens university"] <- "canada"
  datafile$country[datafile$inst == "pacific rim conservation"] <- "usa"
  datafile$country[datafile$inst == "open university"] <- "united kingdom"
  datafile$country[datafile$inst == "palacky university"] <- "czech republic"
  datafile$country[datafile$inst == "percy fitzpatrick institute"] <- "south africa"
  datafile$country[datafile$inst == "point blue conservation"] <- "usa"
  datafile$inst[datafile$inst == "queens university of belfast"] <- "queens university belfast"
  datafile$country[datafile$inst == "queens university of belfast"] <- "northern ireland"
  datafile$country[datafile$inst == "queens university belfast"] <- "northern ireland"
  datafile$country[datafile$inst == "rangeland ecosystem science center"] <- "usa"
  datafile$country[datafile$inst == "tabor college"] <- "usa"
  datafile$country[datafile$inst == "snake river field station"] <- "usa" # usgs
  datafile$country[datafile$inst == "senckenberg research institute frankfurt"] <- "germany"
  datafile$country[datafile$inst == "simon fraser university"] <- "canada"
  datafile$country[datafile$inst == "roosevelt universityfield museum"] <- "usa"
  datafile$inst[datafile$inst == "roosevelt universityfield museum"] <- "field museum"
  datafile$country[datafile$inst == "royal ontario museum"] <- "canada"
  datafile$country[datafile$inst == "ontario ministry of natural resources"] <- "canada"
  datafile$country[datafile$inst == "stazione zoologica anton dohrn"] <- "italy"
  datafile$country[datafile$inst == "tetra tech eba"] <- "usa"
  datafile$country[datafile$inst == "tetra tech ec inc"] <- "usa"
  datafile$country[datafile$inst == "tetratech environmental consulting inc"] <- "usa"
  datafile$inst[datafile$inst == "tetra tech eba"] <- "tetra tech inc"
  datafile$inst[datafile$inst == "tetra tech ec inc"] <- "tetra tech inc"
  datafile$inst[datafile$inst == "tetratech environmental consulting inc"] <- "tetra tech inc"
  datafile$country[datafile$inst == "aarhus university"] <- "denmark"
  datafile$inst[datafile$inst == "notre dame"] <- "university of notre dame"
  datafile$inst[datafile$inst == "notre dame university"] <- "university of notre dame"
  datafile$country[datafile$inst == "tokaigakuen university"] <- "japan"
  datafile$inst <- gsub("tokaigakuen", "tokai gakuen", datafile$inst)
  datafile$country[datafile$inst == "trent university"] <- "canada"
  datafile$country[datafile$inst == "trinity university"] <- "usa"
  datafile$country[datafile$inst == "western foundation of vertebrate zoology"] <- "usa"
  datafile$country[datafile$inst == "western university canada"] <- "canada"
  datafile$country[datafile$inst == "universidadde antioquia"] <- "colombia"
  datafile$country[datafile$inst == "universidadde buenos aires"] <- "argentina"
  datafile$country[datafile$inst == "universidadde los andes"] <- "colombia"
  datafile$country[datafile$inst == "universidadde vila velha"] <- "brazil"
  datafile$inst[datafile$inst == "universidadde vila velha"] <- "universidade vila velha"
  datafile$inst <- gsub("universidadde", "universidad de", datafile$inst)
  datafile$country[datafile$inst == "universidadnacional autonoma de mexico"] <- "mexico"
  datafile$inst <- gsub("universidadnacional", "universidad nacional", datafile$inst)
  datafile$country[datafile$inst == "universita del salento"] <- "italy"
  datafile$country[datafile$inst == "universita di genova"] <- "italy"
  datafile$country[datafile$inst == "universite pierre et marie curie"] <- "france"

  datafile$country[datafile$inst == "american museum of natural history"] <- "usa"
  datafile$country[datafile$inst == "free university amsterdam"] <- "netherlands"
  datafile$country[datafile$inst == "antioch new england graduate school"] <- "usa"
  datafile$inst[datafile$inst == "antioch new england graduate school and national university of rwanda"] <- "antioch new england graduate school"
  datafile$inst[datafile$inst == "arizona state u"] <- "arizona state university"
  datafile$inst[datafile$inst == "asu"] <- "arizona state university"
  datafile$country[datafile$inst == "arizona state university"] <- "usa"
  datafile$country[datafile$inst == "auburn university"] <- "usa"
  datafile$inst[datafile$inst == "agresearch ltd."] <- "agresearch"
  datafile$inst[datafile$inst == "agresearch ltd"] <- "agresearch"
  datafile$inst <- gsub("stationn", "station", datafile$inst)
  datafile$inst <- gsub("assicuates", "associates", datafile$inst)
  datafile$inst[datafile$inst == "agriculture and agri food"] <- "agriculture and agri food canada"

  datafile$country[datafile$inst == "alaska science center"] <- "usa"
  datafile$country[datafile$inst == "amherst college"] <- "usa"
  datafile$country[datafile$inst == "austin peay state university"] <- "usa"
  datafile$country[datafile$inst == "australian commonwealth scientific and research organization"] <- "australia"
  datafile$country[datafile$inst == "australian museum"] <- "australia"
  datafile$country[datafile$inst == "australian national wildlife collection"] <- "australia"
  datafile$country[datafile$inst == "aves argentinas"] <- "argentina"
  datafile$country[datafile$inst == "bates college"] <- "usa"
  datafile$country[datafile$inst == "beloit college"] <- "usa"
  datafile$country[datafile$inst == "bloomfield college"] <- "usa"
  datafile$country[datafile$inst == "boise state u"] <- "usa"
  datafile$inst[datafile$inst == "boise state u"] <- "boise state university"
  datafile$country[datafile$inst == "boise state university"] <- "usa"
  datafile$country[datafile$inst == "bucknell university"] <- "usa"
  datafile$country[datafile$inst == "california academy of sciences"] <- "usa"
  datafile$country[datafile$inst == "california state university san marcos"] <- "usa"
  datafile$country[datafile$inst == "canadian wildlife service"] <- "canada"
  datafile$country[datafile$inst == "catholic university of chile"] <- "chile"
  datafile$country[datafile$inst == "centre national de la recherche scientifique"] <- "france"
  datafile$country[datafile$inst == "citadel military college of south carolina"] <- "usa"
  datafile$country[datafile$inst == "university of pittsburgh"] <- "usa"
  datafile$city[datafile$inst == "university of pittsburgh"] <- "pittsburgh"
  datafile$state[datafile$inst == "university of pittsburgh"] <- "pa"
  datafile$country[datafile$inst == "city university of new york"] <- "usa"
  datafile$country[datafile$inst == "college of new jersey"] <- "usa"
  datafile$country[datafile$inst == "college of william and mary"] <- "usa"
  datafile$country[datafile$inst == "colorado state university"] <- "usa"
  datafile$country[datafile$inst == "columbia university"] <- "usa"
  datafile$country[datafile$inst == "commonwealth university"] <- "usa"
  datafile$country[datafile$inst == "consejo nacional de investigaciones cientificas y tecnicas"] <- "argentina"
  datafile$country[datafile$inst == "cornell lab of ornithology"] <- "usa"
  datafile$unit[datafile$inst == "cornell lab of ornithology"] <- "cornell lab of ornithology"
  datafile$inst[datafile$inst == "cornell lab of ornithology"] <- "cornell university"
  datafile$country[datafile$inst == "cornell u"] <- "usa"
  datafile$inst[datafile$inst == "cornell u"] <- "cornell university"
  datafile$country[datafile$inst == "cornell university"] <- "usa"
  datafile$country[datafile$inst == "deakin university australia"] <- "australia"
  datafile$country[datafile$inst == "delta waterfowl foundation"] <- "canada"
  datafile$country[datafile$inst == "denver museum of nature e science"] <- "usa"
  datafile$inst[datafile$inst == "denver museum of nature e science"] <- "denver museum of nature and science"
  datafile$country[datafile$inst == "disteba universita di lecce"] <- "italy"
  datafile$country[datafile$inst == "donana biological station csic"] <- "spain"
  datafile$country[datafile$inst == "ducks universitynlimited canada"] <- "canada"
  datafile$inst[datafile$inst == "ducks universitynlimited canada"] <- "ducks ulimited canada"
  datafile$country[datafile$inst == "duke university"] <- "usa"
  datafile$country[datafile$inst == "durham university"] <- "united kingdom"
  datafile$country[datafile$inst == "eastern illinois university"] <- "usa"
  datafile$country[datafile$inst == "environment canada"] <- "canada"
  datafile$country[datafile$inst == "environment canada national hydrology research center"] <- "canada"
  datafile$country[datafile$inst == "environment canada wildlife research division"] <- "canada"
  datafile$country[datafile$inst == "environment canada wildlife research east"] <- "canada"
  datafile$country[datafile$inst == "estacion biologia chamela"] <- "mexico"
  datafile$country[datafile$inst == "estern illinois university"] <- "usa"
  datafile$inst[datafile$inst == "estern illinois university"] <- "eastern illinois university"
  datafile$country[datafile$inst == "field museum of natural history"] <- "usa"
  datafile$country[datafile$inst == "florida institute of technology"] <- "usa"
  datafile$country[datafile$inst == "forschungsinstitut senckenberg"] <- "germany"
  datafile$country[datafile$inst == "free university of berlin"] <- "germany"
  datafile$country[datafile$inst == "geomar helmholtz center for ocean research kiel"] <- "germany"
  datafile$country[datafile$inst == "georgia southern u"] <- "usa"
  datafile$inst[datafile$inst == "georgia southern u"] <- "georgia southern university"
  datafile$country[datafile$inst == "georgia southern university"] <- "usa"
  datafile$country[datafile$inst == "guangxi university"] <- "china"
  datafile$country[datafile$inst == "hamilton college"] <- "usa"
  datafile$country[datafile$inst == "harvard university"] <- "usa"
  datafile$country[datafile$inst == "hasting reservation"] <- "usa"
  datafile$unit[datafile$inst == "hasting reservation"] <- "hastings natural history reservation"
  datafile$country[datafile$inst == "hawk mountain sanctuary"] <- "usa"
  datafile$country[datafile$inst == "hobart and william smith college"] <- "usa"
  datafile$country[datafile$inst == "hofstra university"] <- "usa"
  datafile$country[datafile$inst == "humbolt state university"] <- "usa"
  datafile$country[datafile$inst == "hunter college cuny"] <- "usa"
  datafile$country[datafile$inst == "ibigeo conicet"] <- "argentina"
  datafile$country[datafile$inst == "illinois natural history survey"] <- "usa"
  datafile$country[datafile$inst == "indiana state university"] <- "usa"
  datafile$country[datafile$inst == "institut fur ostseeforschungwarnemunde"] <- "germany"
  datafile$inst[datafile$inst == "institut fur ostseeforschungwarnemunde"] <- "leibniz institute for baltic sea research"
  datafile$country[datafile$inst == "instituto de biologia universitynam"] <- "mexico"
  datafile$unit[datafile$inst == "instituto de biologia universitynam"] <- "instituto de biologia"
  datafile$inst[datafile$inst == "instituto de biologia universitynam"] <- "universidad nacional autonoma de mexico"
  datafile$country[datafile$inst == "interuniversity institute for marine sciences"] <- "usa"
  datafile$country[datafile$inst == "james san jacinto mountains reserve"] <- "usa"
  datafile$country[datafile$inst == "kansas state university"] <- "usa"
  datafile$country[datafile$inst == "kruger national park"] <- "south africa"
  datafile$country[datafile$inst == "los alamos national laboratory"] <- "usa"
  datafile$country[datafile$inst == "louisiana state university"] <- "usa"
  datafile$country[datafile$inst == "louisiana state university and university federal university of roraima"] <- "usa"
  datafile$inst[datafile$inst == "louisiana state university and university federal university of roraima"] <- "louisiana state university"
  datafile$country[datafile$inst == "lsa associates"] <- "usa"
  datafile$country[datafile$inst == "lundy environmental consulting"] <- "usa"
  datafile$country[datafile$inst == "massey university"] <- "new zealand"
  datafile$country[datafile$inst == "max planck institute for ornithology"] <- "germany"
  datafile$country[datafile$inst == "mckendree university"] <- "usa"
  datafile$country[datafile$inst == "michigan state university"] <- "usa"
  datafile$country[datafile$inst == "minnesota pollution control agency"] <- "usa"
  datafile$country[datafile$inst == "missouri department of conservation"] <- "usa"
  datafile$country[datafile$inst == "museum of new zealand te papa tongarewa"] <- "new zealand"
  datafile$country[datafile$inst == "national audubon society"] <- "usa"
  datafile$country[datafile$inst == "national aviary"] <- "usa"
  datafile$country[datafile$inst == "national history museum paris"] <- "france"
  datafile$country[datafile$inst == "national park service inventory e monitoring program"] <- "usa"
  datafile$inst[datafile$inst == "national park service inventory e monitoring program"] <- "usnps inventory and monitoring program"
  datafile$country[datafile$inst == "national university of singapore"] <- "singapore"
  datafile$country[datafile$inst == "natural history museum"] <- "united kingdom"
  datafile$country[datafile$inst == "natural history museum uk"] <- "united kingdom"
  datafile$inst[datafile$inst == "natural history museum uk"] <- "natural history museum"
  datafile$country[datafile$inst == "natural history museum uk"] <- "united kingdom"
  datafile$country[datafile$inst == "natural resources institute university of manitoba"] <- "canada"
  datafile$unit[datafile$inst == "natural resources institute university of manitoba"] <- "natural resources institute"
  datafile$inst[datafile$inst == "natural resources institute university of manitoba"] <- "university of manitoba"
  datafile$country[datafile$inst == "new mexico state university"] <- "usa"
  datafile$country[datafile$inst == "interuniversity institute for marine sciences"] <- "israel"
  # datafile$country[datafile$inst=="north central research station"]<-"usa"
  datafile$inst[datafile$inst == "north central research station"] <- "usfs north central research station"
  datafile$country[datafile$inst == "usfs north central research station"] <- "usa"
  datafile$country[datafile$inst == "north dakota state university"] <- "usa"
  datafile$country[datafile$inst == "northern prairie wildlife research center"] <- "usa"
  datafile$inst[datafile$inst == "northern praire wildlife research center"] <- "usgs northern prairie wildlife research center"
  datafile$country[datafile$inst == "rocky mountain research station"] <- "usa"
  datafile$inst[datafile$inst == "us forest service usda"] <- "us forest service"
  datafile$country[datafile$inst == "university of missouri st. louis"] <- "usa"
  datafile$inst[datafile$inst == "university of missouri st. louis"] <- "university of missouri st louis"
  datafile$country[datafile$inst == "(retired) natural history museum london"] <- "united kingdom"
  datafile$inst[datafile$inst == "(retired) natural history museum london"] <- "natural history museum"
  #
  datafile$inst[datafile$inst == "cambridge university"] <- "university of cambridge"
  datafile$inst[datafile$inst == "universite de sherbrook"] <- "university of sherbrooke"
  datafile$inst[datafile$inst == "université joseph fourier"] <- "universite joseph fourier"
  datafile$inst[datafile$inst == "royal veterinary andagricultural university"] <- "royal veterinary and agricultural university"
  datafile$inst[datafile$inst == "royal society for the protection of bitds"] <- "royal society for the protection of birds"
  datafile$inst[datafile$inst == "royal holloway"] <- "royal holloway university of london"

  datafile$inst[datafile$inst == "pontifica universidad catolica de chile"] <- "pontificia universidad catolica de chile"
  datafile$inst[datafile$inst == "pennsylvaia state university"] <- "pennsylvania state university"
  datafile$inst[datafile$inst == "phiiipps university marburg"] <- "philipps university marburg"
  datafile$inst[datafile$inst == "simon fraser university\v"] <- "simon fraser university"
  datafile$inst[datafile$inst == "landcaster university"] <- "lancaster university"
  datafile$inst[datafile$inst == "agr univ norway"] <- "agricultural university of norway"
  datafile$inst[datafile$inst == "agr university norway"] <- "agricultural university of norway"
  datafile$inst[datafile$inst == "albert ludwigs universitatfreiburg"] <- "university of freiburg"
  datafile$inst[datafile$inst == "asa cssa sssa"] <- "american society of agronomy"
  datafile$inst[datafile$inst == "austral center for scientific research"] <- "conicet austral center for scientific research"
  datafile$inst[datafile$inst == "austrian research centre for forests (bfw)"] <- "austrian research centre for forests"
  datafile$inst[datafile$inst == "british antarctic survey cambridge"] <- "british antarctic survey"
  datafile$inst[datafile$inst == "university of oldenberg"] <- "university of oldenburg"
  datafile$inst[datafile$inst == "carl von university" | datafile$inst == "carl von university oldenburg" | datafile$inst == "university of oldenburg"] <- "carl von ossietzky university oldenburg"
  datafile$inst[datafile$inst == "carl von ossietzky university"] <- "carl von ossietzky university oldenburg"
  datafile$inst[datafile$inst == "centre decologie fonctionnelle et evolutive"] <- "cnrs centre decologie fonctionnelle et evolutive"
  datafile$inst[datafile$inst == "cnrs centre decologie fonctionnelle et evolutive"] <- "cnrs centre decologie fonctionnelle et evolutive"
  datafile$inst[datafile$inst == "christ church"] <- "christchurch"
  datafile$inst[datafile$inst == "cisro sustainable ecosystems"] <- "csiro sustainable ecosystems"
  datafile$inst[datafile$inst == "columbia universily"] <- "columbia university"
  datafile$inst[datafile$inst == "commonwealth scientific and industrial research organisation"] <- "csiro commonwealth scientific and industrial research organisation"
  datafile$inst[datafile$inst == "csiro"] <- "csiro commonwealth scientific and industrial research organisation"
  datafile$inst[datafile$inst == "commonwealth scientific and industrial research organisation (csiro)"] <- "csiro commonwealth scientific and industrial research organisation"
  datafile$inst[datafile$inst == "cisro" & datafile$unit == "tropical ecosystems"] <- "csiro tropical ecosystems"
  datafile$inst[datafile$inst == "consejo nacl invest cient and tecn"] <- "consejo nacional de investigaciones cientificas y tecnicas"
  datafile$inst[datafile$inst == "davis"] <- "university of california davis"
  datafile$inst[datafile$inst == "deakin university australia"] <- "deakin university"
  datafile$unit[datafile$inst == "division of reptiles and amphibians smithsonian institution washington dc"] <- "division of reptiles and amphibians"
  datafile$inst[datafile$inst == "division of reptiles and amphibians smithsonian institution washington dc"] <- "national museum of natural history smithsonian institution"
  datafile$inst[datafile$inst == "donana biological station"] <- "csic donana biological station"
  datafile$inst[datafile$inst == "donana biological station csic"] <- "csic donana biological station"
  datafile$inst[datafile$inst == "dresden institute of technology"] <- "dresden university of technology"
  datafile$inst[datafile$inst == "eawag/eth zurich"] <- "swiss federal institute of aquatic science and technology"
  datafile$inst[datafile$inst == "ecole normale sup<82>rieure"] <- "ecole normale superieure"
  datafile$inst[datafile$inst == "edinburgh university"] <- "university of edinburgh"
  datafile$inst[datafile$inst == "eidgenossiche technische hochschule (eth) zurich"] <- "eth zurich"
  datafile$inst[datafile$inst == "evolution and marine biology university of california"] <- "university of california santa barbara"
  datafile$inst[datafile$inst == "fachstelle fur pflanzenschutz"] <- "university of bern"
  datafile$inst[datafile$inst == "field museum"] <- "field museum of natural history"
  datafile$inst[datafile$inst == "florida"] <- "university of florida"
  datafile$inst[datafile$inst == "freie universit�t berlin"] <- "free university of berlin"
  datafile$inst[datafile$inst == "griffith sch environm"] <- "griffith university"
  datafile$inst[datafile$inst == "humboldt university berlin"] <- "humboldt university of berlin"

  datafile$inst[datafile$inst == "royal veterinary andagricultural university"] <- "royal veterinary and agricultural university"
  datafile$inst[datafile$inst == "imperial college"] <- "imperial college london"
  datafile$inst[datafile$inst == "imperial university"] <- "imperial college london"
  datafile$inst[datafile$inst == "imperial collegesilwood park"] <- "imperial college silwood park"
  datafile$inst[datafile$inst == "indian inst science bangalore"] <- "indian institute of science"
  datafile$inst[datafile$inst == "instituto de biologia unam"] <- "universidad nacional autonoma de mexico"
  datafile$inst[datafile$inst == "instituto nacional de pesquisas da amazonia inpa"] <- "instituto nacional de pesquisas da amazonia"
  datafile$inst[datafile$inst == "instituto venezolano investigaciones cientificas (ivic)"] <- "instituto venezolano investigaciones cientificas"
  datafile$inst[datafile$inst == "intelligent solutions group (isg)"] <- "intelligent solutions group"
  datafile$inst[datafile$inst == "j.w. goethe university frankfurt am main"] <- "goethe university frankfurt"
  datafile$inst[datafile$inst == "jaldin botanique nacional de belgique"] <- "jardin botanique nacional de belgique"
  datafile$inst[datafile$inst == "john innes center for plant science research"] <- "john innes center"
  datafile$inst[datafile$unit == "la selva biological station"] <- "la selva biological station"
  datafile$inst[datafile$inst == "la selva biological station"] <- "organization for tropical studies"
  datafile$inst[datafile$inst == "landcaster university"] <- "lancaster university"
  datafile$inst[datafile$inst == "ludwig maximilians universit<84>t m<81>nchen"] <- "ludwig maximilian university of munich"
  datafile$inst[datafile$inst == "ludwig maximilians university of munich"] <- "ludwig maximilian university of munich"
  datafile$inst[datafile$inst == "university of munich"] <- "ludwig maximilian university of munich"
  datafile$country[datafile$inst == "ludwig maximilian university of munich"] <- "germany"
  datafile$inst[datafile$inst == "lehrstuhl fur landschaftsokologie der technischen universitat munchen"] <- "technical university of munich"
  datafile$inst[datafile$inst == "ludwig maximilians universitat munchen"] <- "ludwig maximilians university of munich"
  datafile$inst[datafile$inst == "macaulay institute"] <- "macaulay land use research institute"
  datafile$inst[datafile$inst == "macquaire university"] <- "macquarie university"
  datafile$inst[datafile$inst == "michigan technol university"] <- "michigan technological university"
  datafile$inst[datafile$inst == "mississippi state"] <- "mississippi state university"
  datafile$inst[datafile$inst == "museum fu_r naturkunde"] <- "museum fur naturkunde"
  datafile$inst[datafile$inst == "n/a"] <- "missing"
  datafile$inst[datafile$inst == "national audobon society"] <- "national audubon society"
  datafile$inst[datafile$inst == "natural history museum" & datafile$country == "united kingdom"] <- "natural history museum london"
  datafile$inst[datafile$inst == "national history museum paris"] <- "national natural history museum france"
  datafile$inst[datafile$inst == "nioz royal netherlands inst sea res"] <- "royal netherlands institute for sea research"
  datafile$inst[datafile$inst == "noinst"] <- "missing"
  datafile$inst[datafile$inst == "north carolina agricultural and technical state university"] <- "north carolina a and t state university"
  datafile$inst[datafile$inst == "ohio university main campus"] <- "ohio university"
  datafile$inst[datafile$inst == "pacific agri food research centre"] <- "pacific agrifood research centre"
  datafile$inst[datafile$inst == "pekinn"] <- "peking"
  datafile$inst[datafile$inst == "pepperdine"] <- "pepperdine university"
  datafile$inst[datafile$inst == "percy fitzpatrick institute"] <- "percy fitzpatrick institute of african ornithology"
  datafile$inst[datafile$inst == "plant sciences department" & datafile$LAST_NAME == "samples"] <- "university of tennessee"
  datafile$inst[datafile$inst == "queens u"] <- "queens university"
  datafile$inst[datafile$inst == "radboud university"] <- "radboud university nijmegen"
  datafile$inst[datafile$inst == "retired no affiliation listed"] <- "retired (no affiliation)"
  datafile$inst[datafile$inst == "riverside"] <- "university of california riverside"
  datafile$inst[datafile$inst == "rose hulman institute of technology\v"] <- "rose hulman institute of technology"
  datafile$inst[datafile$inst == "santa cruz"] <- "university of california santa cruz"
  datafile$inst[datafile$inst == "school of biological sciences" & datafile$LAST_NAME == "wardle"] <- "university of sydney"
  datafile$inst[datafile$inst == "school of environment and nat resources"] <- "ohio state university"
  datafile$inst[datafile$inst == "school of environmental science"] <- "murdoch university"
  datafile$inst[datafile$inst == "school of plant environmental and soil sciences"] <- "louisiana state university"
  datafile$inst[datafile$inst == "simon fraser university\v"] <- "simon fraser university"
  datafile$inst[datafile$inst == "slu"] <- "swedish university of agricultural sciences"
  datafile$inst[datafile$inst == "south florida"] <- "university of south florida"
  datafile$inst[datafile$inst == "southeastern louisiana"] <- "southeastern louisiana university"
  datafile$inst[datafile$inst == "southwest res. ext. center"] <- "southwest research and extension center"
  datafile$inst[datafile$inst == "st. lawrence centre"] <- "st lawrence centre"
  datafile$inst[datafile$inst == "st. louis university"] <- "st louis university"
  datafile$inst[datafile$inst == "stazione zoologica di napoli villa acquario"] <- "stazione zoologica anton dohrn"
  datafile$inst[datafile$inst == "stazione zoologica di napoli villa comunale"] <- "stazione zoologica anton dohrn"
  datafile$inst[datafile$inst == "stony brook university"] <- "suny stony brook"
  datafile$inst[datafile$inst == "suny albany"] <- "suny albany"
  datafile$inst[datafile$inst == "suny stony brook"] <- "suny stony brook"
  datafile$inst[datafile$inst == "binghamton university state university of new york"] <- "state university of new york binghamton"
  datafile$inst[datafile$inst == "suny"] <- "state university of new york"
  datafile$inst[datafile$inst == "swiss federal institute of technology zurich"] <- "swiss federal institute of technology"
  datafile$inst[datafile$inst == "eth zurich"] <- "swiss federal institute of technology "
  datafile$inst[datafile$inst == "tall timber research station"] <- "tall timbers research station"
  datafile$inst[datafile$inst == "tech univ darmstadt"] <- "technische universitat darmstadt"
  datafile$inst[datafile$inst == "tufts"] <- "tufts university"
  datafile$inst[datafile$inst == "tuniversity of dresden"] <- "university of dresden"
  datafile$inst[datafile$inst == "unc chapel hill"] <- "university of north carolina chapel hill"
  datafile$inst[datafile$inst == "universidad autonoma del estado hidalgo"] <- "universidad autonoma del estado de hidalgo"
  datafile$inst[datafile$inst == "university of london college imperial college of science"] <- "university of london imperial college of science technology and medicine"
  datafile$inst[datafile$inst == "university of london imperial college of science technology"] <- "university of london imperial college of science technology and medicine"
  datafile$inst[datafile$inst == "univ london imperial coll sci technol and med"] <- "imperial college of science technology and medicine"
  datafile$inst[datafile$inst == "universidad nacional aut<a2>noma de m<82>xico"] <- "universidad nacional autonoma de mexico"
  datafile$inst[datafile$inst == "universidade de s?o paulo"] <- "universidade de sao paulo"
  datafile$inst[datafile$inst == "universidade estadual paulista (unesp)"] <- "universidade estadual paulista"
  datafile$inst[datafile$inst == "universidade estadual de campinas (unicamp)"] <- "universidade estadual de campinas"
  datafile$inst[datafile$inst == "universidade federal do rio grande do norte (university of floridarn)"] <- "universidade federal do rio grande do norte"
  datafile$inst[datafile$inst == "university fed goias"] <- "universidade federal de goias"
  datafile$inst[datafile$inst == "university of brasilia"] <- "universidade de brasilia"
  datafile$inst[datafile$inst == "universita die napoli \"frederico ii\""] <- "universita die napoli frederico ii"
  datafile$inst[datafile$inst == "universite de quebec a monteal"] <- "universite de quebec a montreal"
  datafile$inst[datafile$inst == "university lyon 1 cnrs"] <- "university lyon 1"
  datafile$inst[datafile$inst == "universit<82> paris sud"] <- "universite paris sud"
  datafile$inst[datafile$inst == "univ. de sao paulo"] <- "universidade de sao paulo"
  datafile$inst[datafile$inst == "university of sao paulo"] <- "universidade de sao paulo"
  datafile$country[datafile$inst == "university of notre dame"] <- "usa"

  datafile$inst[datafile$inst == "university of east angolia"] <- "university of east anglia"
  datafile$inst[datafile$inst == "university of california santa diego"] <- "university of california san diego"
  datafile$inst[datafile$inst == "cnrs montellier"] <- "centre decologie fonctionnelle et evolutive cnrs"
  datafile$inst[datafile$inst == "universite claude bernard lyons"] <- "universite claude bernard lyon 1"
  datafile$inst[datafile$inst == "royal botanical gardens kew"] <- "royal botanic gardens kew"
  datafile$inst[datafile$inst == "royal botanic gardens" & datafile$country == "united kingdom"] <- "royal botanic gardens kew"
  datafile$inst[datafile$inst == "universite montpellier ii"] <- "universite montpellier 2"
  datafile$inst[datafile$inst == "university of querzburg"] <- "university of wurzburg"
  datafile$inst[datafile$inst == "environmental protection agency"] <- "us environmental protection agency"
  datafile$inst[datafile$inst == "univesity of sterling"] <- "university of stirling"
  datafile$inst[datafile$inst == "university of sterling"] <- "university of stirling"
  datafile$inst[datafile$inst == "universitat pompeuniversity of fabra"] <- "universitat pompeu fabra"
  datafile$inst[datafile$inst == "university pompeuniversity of fabra"] <- "universitat pompeu fabra"

  datafile$inst[datafile$inst == "field museuniversity of natural history"] <- "field museum of natural history"
  datafile$inst[datafile$inst == "filed museuniversity of natural history"] <- "field museum of natural history"
  datafile$inst[datafile$inst == "boyal botanic gatdens edinburgh"] <- "royal botanic garden edinburgh"
  datafile$inst[datafile$inst == "university of of minnesota"] <- "university of minnesota"
  datafile$inst[datafile$inst == "university of leichester"] <- "university of leicester"
  datafile$inst[datafile$inst == "university pierre et marie curie"] <- "universite pierre et marie curie"
  datafile$inst[datafile$inst == "instituto venezolano investigaciones cientificas"] <- "instituto venezolano de investigaciones cientificas"
  datafile$inst[datafile$inst == "universite de quebec a montreal"] <- "university du quebec a montreal"
  datafile$inst[datafile$inst == "istituto per l¬¢ambiente marino costiero"] <- "instituto per lambiente marino costiero"
  datafile$inst[datafile$inst == "university illinois urbana champaign"] <- "university of illinois urbana champaign"
  datafile$inst[datafile$inst == "universitat pompeu fabra"] <- "university pompeu fabra"
  datafile$inst[datafile$inst == "universit√© de montreal"] <- "universite de montreal"
  datafile$inst[datafile$inst == "universite de montreal"] <- "university of montreal"
  datafile$inst[datafile$inst == "universite of montreal"] <- "university of montreal"
  datafile$inst[datafile$inst == "university of fribourg"] <- "university of freiburg"
  datafile$inst[datafile$inst == "university british columbia"] <- "university of british columbia"
  datafile$inst[datafile$inst == "swiss federal research institute wsl"] <- "wsl swiss federal research institute"
  datafile$inst[datafile$inst == "swiss federal research institute"] <- "wsl swiss federal research institute"
  datafile$inst[datafile$inst == "university buenos aires"] <- "university of buenos aires"
  datafile$inst[datafile$inst == "john innes centre"] <- "john innes center"
  datafile$inst[datafile$inst == "universite de sherbrooke"] <- "university of sherbrooke"
  datafile$inst[datafile$inst == "universite de paris sud"] <- "universite paris sud"
  datafile$inst[datafile$inst == "university gottingen"] <- "university of gottingen"
  datafile$inst[datafile$inst == "university groningen"] <- "university of groningen"
  datafile$inst[datafile$inst == "university los andes"] <- "universidad de los andes"
  datafile$inst[datafile$inst == "university of los andes"] <- "universidad de los andes"
  datafile$inst[datafile$inst == "university tasmania"] <- "university of tasmania"
  datafile$inst[datafile$inst == "versity of tennessee "] <- "university of tennessee"
  datafile$inst[datafile$inst == "university of tennesse"] <- "university of tennessee"
  datafile$inst[datafile$inst == "versity of tennessee"] <- "university of tennessee"
  datafile$inst[datafile$inst == "university tennessee"] <- "university of tennessee"
  datafile$inst[datafile$inst == "netherlands institute for sea research"] <- "royal netherlands institute for sea research"
  datafile$inst[datafile$inst == "university aberdeen"] <- "university of aberdeen"
  datafile$inst[datafile$inst == "city university of new york"] <- "cuny city university of new york"
  datafile$inst[datafile$inst == "institute of ecosystem studies" & datafile$country == "usa"] <- "cary institute of ecosystem studies"
  # datafile$inst[datafile$inst==  "university of college" ]<-university college  2x not colgne
  datafile$inst[datafile$inst == "waginen university"] <- "wageningen university"
  datafile$inst[datafile$inst == "universit√© de montreal"] <- "university of montreal"
  datafile$inst[datafile$inst == "state university of new york"] <- "suny state university of new york"
  datafile$inst[datafile$inst == "university of nebraska at omaha"] <- "university of nebraska omaha"
  datafile$country[datafile$inst == "university of nebraska omaha"] <- "usa"
  datafile$inst[datafile$inst == "university of nebraska"] <- "university of nebraska lincoln"
  datafile$inst[datafile$inst == "unl"] <- "university of nebraska lincoln"
  datafile$inst[datafile$inst == "noura ziadi"] <- "agriculture and agri food canada"
  datafile$inst[datafile$inst == "teagasc"] <- "agriculture and food development authority"
  datafile$inst[datafile$inst == "beaverlodge res farm"] <- "beaverlodge research farm"
  datafile$inst[datafile$inst == "service canadien des forets"] <- "canadian forest service"
  datafile$inst[datafile$inst == "center for international forestry research cifor"] <- "center for international forestry research"
  datafile$country[datafile$inst == "shahid beheshti university"] <- "iran"
  datafile$country[datafile$inst == "university of hong kong"] <- "hong kong"
  datafile$inst[datafile$inst == "brazilian agricultural research corporation cirad"] <- "cirad-usp-esalq consortium"
  datafile$inst[datafile$inst == "centre decologie des ressources renouvelables"] <- "cnrs centre decologie des ressources renouvelables"
  datafile$inst[datafile$inst == "centre national de la recherche scientifique"] <- "cnrs centre national de la recherche scientifique"
  datafile$inst[datafile$inst == "cnrs"] <- "cnrs centre national de la recherche scientifique"
  datafile$inst[datafile$inst == "centre decologie fonctionnelle et evolutive cnrs"] <- "cnrs institut des sciences de levolution montpellier"
  datafile$inst[datafile$inst == "consejo nacional de investigaciones cientificas y tecnicas"] <- "conicet"
  datafile$inst[datafile$inst == "investigador conicet"] <- "conicet cct mendoza"
  datafile$inst[datafile$inst == "conicet"] <- "conicet consejo nacional de investigaciones cientificas y tecnicas"
  datafile$inst[datafile$inst == "centro nacional patagonico conicet"] <- "conicet centro nacional patagonico"
  datafile$inst[datafile$inst == "ibigeo conicet"] <- "conicet ibigeo"
  datafile$inst[datafile$inst == "copenhagen university"] <- "university of copenhagen"
  datafile$inst[datafile$inst == "university copenhagen"] <- "university of copenhagen"
  datafile$inst[datafile$inst == "university of pk"] <- "university of copenhagen"

  datafile$inst[datafile$inst == "cisc"] <- "csic consejo superior de investigaciones cientificas"
  datafile$inst[datafile$inst == "consejo superior de investigaciones cientificas"] <- "csic consejo superior de investigaciones cientificas"
  datafile$inst[datafile$inst == "estacion biologica donana"] <- "csic donana biological station"
  datafile$inst[datafile$inst == "brookhaven national laboratory"] <- "doe brookhaven national laboratory"
  datafile$inst[datafile$inst == "brookhaven national laboratory"] <- "doe brookhaven national laboratory"
  datafile$inst[datafile$inst == "brookhaven national laboratory"] <- "doe brookhaven national laboratory"
  datafile$inst[datafile$inst == "los alamos national laboratory"] <- "doe los alamos national laboratory"
  datafile$inst[datafile$inst == "oak ridge national laboratory"] <- "doe oak ridge national laboratory"
  datafile$inst[datafile$inst == "pacific northwest national laboratory"] <- "doe pacific northwest national laboratory"
  datafile$inst[datafile$inst == "forest research lnstitute malaysia (frim)"] <- "forest research lnstitute malaysia"
  datafile$inst[datafile$inst == "free university amsterdam"] <- "free university amsterdam"
  datafile$inst[datafile$inst == "institute of forestry and nature research"] <- "free university amsterdam"
  datafile$inst[datafile$inst == "french agricultural research centre for international development cirad"] <- "french agricultural research centre for international development"
  datafile$inst[datafile$inst == "national institute for agronomic research"] <- "french national institute for agricultural research"
  datafile$inst[datafile$inst == "helmholtz centre for environmental research ufz university of floridaz"] <- "helmholtz centre for environmental research"
  datafile$inst[datafile$inst == "instituto de recursos naturalesand agrobiologia"] <- "instituto de recursos naturales e agrobiologia"
  datafile$inst[datafile$inst == "imedea"] <- "instituto mediterraneo de estudios avanzados (imedea)"
  datafile$inst[datafile$inst == "istituto per l¬¢ambiente marino costiero"] <- "instituto per lambiente marino costiero"
  datafile$unit[datafile$inst == "centro de ecologia"] <- "centro de ecologia"
  datafile$inst[datafile$inst == "centro de ecologia"] <- "instituto venezolano de investigaciones cientificas"
  datafile$inst[datafile$inst == "venezuelan institute for scientific investigation"] <- "instituto venezolano de investigaciones cientificas"
  datafile$inst[datafile$inst == "venezuelan institute for scientific research"] <- "instituto venezolano de investigaciones cientificas"
  datafile$inst[datafile$inst == "international center for research in agroforestry icraf"] <- "international center for research in agroforestry"
  datafile$inst[datafile$inst == "macaulay¬†institute"] <- "macaulay land use research institute"
  datafile$inst[datafile$inst == "memorial uni of newfoundland"] <- "memorial university of newfoundland"
  datafile$inst[datafile$inst == "michigan technol univ"] <- "michigan technological university"
  datafile$inst[datafile$inst == "national natural history museum paris"] <- "national museum of natural history france"
  datafile$inst[datafile$inst == "northeast fisheries science center"] <- "noaa northeast fisheries science center"
  datafile$inst[datafile$inst == "catholic university of chile"] <- "pontificia universidad catolica de chile"
  datafile$inst[datafile$inst == "riso dtuniversity of national laboratory for sustainable energy"] <- "riso dtu national laboratory for sustainable energy"
  datafile$inst[datafile$inst == "rothamsted experimental station"] <- "rothamsted research"
  datafile$inst[datafile$inst == "nederlands institut voor onderzoek der zee"] <- "royal netherlands institute for sea research"
  datafile$inst[datafile$inst == "nederlands institut voor onderzoek der zee"] <- "royal netherlands institute for sea research"
  datafile$inst[datafile$inst == "french national institute for agricultural research"] <- "same as other ag inst?"
  datafile$inst[datafile$inst == "institut national de recherche en sciences et technologies pour lenvironnement et lagriculture"] <- "same as other ag inst?"
  datafile$inst[datafile$inst == "national museum of natural history smithsonian institution"] <- "smithsonian institution national museum of natural history"
  datafile$inst[datafile$inst == "station detudes des gorilles et chimpanzes (segc)"] <- "station detudes des gorilles et chimpanzes"
  datafile$inst[datafile$inst == "swedish agricultural university"] <- "swedish university of agricultural sciences"
  datafile$inst[datafile$inst == "university of tel aviv"] <- "tel aviv university"
  datafile$inst[datafile$inst == "tropical savannas management cooperative research centre and sustainable ecosystems"] <- "tropical savannas management cooperative research centre"
  datafile$inst[datafile$inst == "haus nr.9"] <- "unaffiliated "
  datafile$inst[datafile$inst == "university of buenos aires"] <- "universidad de buenos aires"
  datafile$inst[datafile$inst == "university of buenos aires and ifeva"] <- "universidad de buenos aires"
  datafile$inst[datafile$inst == "university of chile"] <- "universidad de chile"
  datafile$inst[datafile$inst == "university nacl autonoma mexico"] <- "universidad nacional autonoma de mexico"
  datafile$inst[datafile$inst == "instituto de biociencias"] <- "universidade de sao paulo"
  # datafile$inst[datafile$inst=="universidade de sao paulo"]<-"universidade estadual paulista"
  datafile$inst[datafile$inst == "icbs ufal and oxford university"] <- "universidade federal de alagoas"
  datafile$inst[datafile$inst == "universita di napoli"] <- "universita die napoli frederico ii"
  datafile$inst[datafile$inst == "university lyon 1"] <- "universite claude bernard lyon 1"
  datafile$inst[datafile$inst == "university paris sud 11"] <- "universite paris sud"
  datafile$inst[datafile$inst == "aberdeen university"] <- "university of aberdeen"
  datafile$inst[datafile$inst == "university of university amsterdam"] <- "university of amsterdam"
  datafile$unit[datafile$inst == "scripps institution of oceanography"] <- "scripps institution of oceanography"
  datafile$inst[datafile$inst == "scripps institution of oceanography"] <- "university of california san diego"
  datafile$unit[datafile$inst == "savannah river ecology laboratory"] <- "savannah river ecology laboratory"
  datafile$inst[datafile$inst == "savannah river ecology laboratory"] <- "university of georgia"
  datafile$inst[datafile$inst == "leiden university"] <- "university of leiden"
  datafile$inst[datafile$inst == "universit√© de montreal"] <- "university of montreal"
  datafile$inst[datafile$inst == "universiti sains"] <- "university of sains"
  datafile$inst[datafile$inst == "stellenbosch university"] <- "university of stellenbosch"
  datafile$inst[datafile$inst == "university of t_bingen"] <- "university of tubingen"
  # datafile$inst[datafile$inst=="university of t_bingen"]<-"university of tubingen"
  datafile$country[datafile$inst == "university of tubingen"] <- "germany"
  datafile$inst[datafile$inst == "umea university"] <- "university of umea"
  datafile$inst[datafile$inst == "us army"] <- "us army engineer research and development center"
  datafile$inst[datafile$inst == "usda us department of agriculture"] <- "usda"
  datafile$inst[datafile$inst == "usda ars"] <- "usda agricultural research service"
  datafile$inst[datafile$inst == "usda aphis"] <- "usda animal and plant health inspection service"
  datafile$inst[datafile$inst == "us arid land agricultural research center"] <- "usda arid land agricultural research center"
  datafile$inst[datafile$inst == "usda cooperative state research education and extension service (csrees)"] <- "usda cooperative state research education and extension service"
  datafile$inst[datafile$inst == "us environmental protection agency"] <- "usepa environmental protection agency"
  datafile$inst[datafile$inst == "us forest service"] <- "usfs"
  datafile$inst[datafile$inst == "usfs us forest service"] <- "usfs"
  # datafile$inst[datafile$inst=="usfs us forest service"]<-"usfs"
  datafile$inst[datafile$inst == "north central research station"] <- "usfs north central research station"
  datafile$inst[datafile$inst == "us forest service north central research station"] <- "usfs north central research station"
  datafile$inst[datafile$inst == "pacific s.w. research station us forest service"] <- "usfs pacific southest research station"
  datafile$inst[datafile$inst == "usfws alaska region"] <- "usfws alaska region"
  datafile$inst[datafile$inst == "usfws idaho fish and wildlife office"] <- "usfws idaho fish and wildlife office"
  datafile$inst[datafile$inst == "usfws national ecology research center"] <- "usfws national ecology research center"
  datafile$inst[datafile$inst == "usfws national wetlands research center"] <- "usgs national wetlands research center"
  datafile$inst[datafile$inst == "united state fish and wildlife service"] <- "usfws united state fish and wildlife service"
  datafile$inst[datafile$inst == "us geological survey"] <- "usgs"
  datafile$inst[datafile$inst == "us geological survey alaska science center"] <- "usgs alaska science center"
  datafile$inst[datafile$inst == "us geological survey and university of california santa barbara"] <- "usgs and university of california santa barbara"
  datafile$inst[datafile$inst == "us geological survey and university of miami"] <- "usgs and university of miami"
  datafile$inst[datafile$inst == "us geological survey biological resources division"] <- "usgs biological resources division"
  datafile$inst[datafile$inst == "us geological survey forest and rangeland ecosystem science center"] <- "usgs forest and rangeland ecosystem science center"
  datafile$inst[datafile$inst == "us geological survey forest and rangeland ecosystem science center"] <- "usgs forest and rangeland ecosystem science center"
  datafile$inst[datafile$inst == "us geological survey fort collins science center"] <- "usgs fort collins science center"
  datafile$inst[datafile$inst == "us geological survey great lakes science center"] <- "usgs great lakes science center"
  datafile$inst[datafile$inst == "us geological survey/nrii"] <- "usgs national resources inventory"
  datafile$inst[datafile$inst == "us geological survey northern prairie wildlife research center"] <- "usgs northern prairie wildlife research center"
  datafile$inst[datafile$inst == "usgs northern prairie wildlife research center"] <- "usgs northern prairie wildlife research center"
  datafile$inst[datafile$inst == "us geological survey patuxent wildlife research center"] <- "usgs patuxent wildlife research center"
  datafile$inst[datafile$inst == "us geological survey patuzent"] <- "usgs patuxent wildlife research center"
  datafile$inst[datafile$inst == "us geological survey us geological survey"] <- "usgs us geological survey"
  datafile$inst[datafile$inst == "us geological society"] <- "usgs western ecological research center"
  datafile$inst[datafile$inst == "us geological survey western ecological research center"] <- "usgs western ecological research center"
  datafile$inst[datafile$inst == "western fisheries research center"] <- "usgs western fisheries research center"
  datafile$inst[datafile$inst == "us geological survey wisconsin cooperative wildlife research unit"] <- "usgs wisconsin cooperative wildlife research unit"
  datafile$inst[datafile$inst == "usnps inventory and monitoring program"] <- "usnps inventory and monitoring program"
  datafile$inst[datafile$inst == "usonr naval research laboratory"] <- "usonr naval research laboratory"
  datafile$inst[datafile$inst == "virginia commonwealth u"] <- "virginia commonwealth university"
  datafile$inst[datafile$inst == "wageningen university and research centre"] <- "wageningen university and research"
  datafile$inst[datafile$inst == "washington university school of medicine"] <- "washington university in st louis"
  datafile$inst[datafile$inst == "wrcoclaw u"] <- "wrcoclaw university"
  datafile$unit[datafile$inst == "yale school of forestry and environmental studies"] <- "school of forestry and environmental studies"
  datafile$inst[datafile$inst == "yale school of forestry and environmental studies"] <- "yale university"
  datafile$inst[datafile$inst == "argonne national laboratory"] <- "doe argonne national laboratory"
  datafile$inst[datafile$inst == "international institute of tropical forestry"] <- "usfs international institute of tropical forestry"
  datafile$inst[datafile$inst == "university connecticut"] <- "university of connecticut"
  datafile$inst[datafile$inst == "universite de lausanne"] <- "university of lausanne"
  datafile$inst[datafile$inst == "nc state university"] <- "north carolina state university"
  datafile$inst[datafile$inst == "alaska science center"] <- "usgs alaska science center"
  datafile$inst[datafile$inst == "uga"] <- "university of georgia"
  datafile$inst[datafile$inst == "uva"] <- "university of virginia"

  # datafile$unit[datafile$inst=="centro de ecologia"]<-"centro de ecologia"
  # datafile$unit[datafile$inst=="yale school of forestry and environmental studies"]<-"school of forestry and environmental studies"
  datafile$unit[datafile$inst == "washington university school of medicine"] <- "school of medicine"
  datafile$inst_CHECK[datafile$inst == "us geological survey and university of miami"] <- "2x primary inst"
  datafile$unit[datafile$inst == "savannah river ecology laboratory"] <- "savannah river ecology laboratory"
  datafile$notes[datafile$inst == "haus nr.9"] <- "max-planck institute for ornithology in a journal article from this time"
  datafile$unit[datafile$inst == "scripps institution of oceanography"] <- "scripps institution of oceanography"
  # datafile$unit[datafile$inst=="scripps institute of oceanography"]<-"scripps institution of oceanography"
  # datafile$unit[datafile$inst=="savannah river ecology laboratory"]<-"savannah river ecology laboratory"

  datafile$unit[datafile$inst == "univeristy of leiden"] <- "university of leiden"

  gsub("us geological survey", "usgs", datafile$inst)
  gsub("us forest service", "usfs", datafile$inst)
  gsub("us department of agriculture", "usda", datafile$inst)


  datafile$inst[datafile$inst == "(retired) natural history museum london"] <- "retired"
  datafile$inst[datafile$inst == "agricultural university wageningen"] <- "wageningen agricultural university"
  datafile$inst[datafile$inst == "alberta"] <- "university of alberta"
  datafile$country[datafile$inst == "alexandra university"] <- "egypt"
  datafile$country[datafile$inst == "alfred wegener institut fur polar und meeresforschung"] <- "germany"
  datafile$country[datafile$inst == "arete associates"] <- "usa"
  datafile$unit[datafile$inst == "arnold arboretum of harvard university"] <- "arnold arboretum"
  datafile$inst[datafile$inst == "arnold arboretum of harvard university"] <- "harvard university"
  datafile$inst[datafile$inst == "australian commonwealth scientific and research organization"] <- "csiro commonwealth scientific and industrial research organisation"
  datafile$country[datafile$inst == "australian national university"] <- "australia"
  datafile$inst[datafile$inst == "basf plant science"] <- "basf"
  datafile$country[datafile$inst == "biological centre"] <- "netherlands"
  datafile$country[datafile$inst == "biological institute"] <- "yugoslavia"
  datafile$inst[datafile$inst == "bristol university"] <- "university of bristol"
  datafile$country[datafile$inst == "british antarctic survey"] <- "united kingdom"
  datafile$inst[datafile$inst == "calyx, inc"] <- "calyx inc"
  datafile$inst[datafile$inst == "university of cardiff"] <- "cardiff university"
  datafile$country[datafile$inst == "carl von ossietzky university oldenburg"] <- "germany"
  datafile$inst[datafile$inst == "catholic university of chile"] <- "pontificia universidad catolica de chile"
  datafile$unit[datafile$inst == "center of marine sciences of algarve universidade"] <- "center of marine sciences"
  datafile$inst[datafile$inst == "center of marine sciences of algarve universidade"] <- "universidade do algarve"
  datafile$country[datafile$inst == "universidade do algarve"] <- "portugal"
  datafile$inst[datafile$inst == "chris britton consultancy balfor beatty"] <- "chris britton consultancy"
  datafile$country[datafile$inst == "christian albrechts universitat kiel"] <- "germany"
  datafile$country[datafile$inst == "cinvestav"] <- "mexico"
  datafile$inst[datafile$inst == "ciudad universitaria"] <- "ets ingenieros de montes"
  datafile$country[datafile$inst == "cnrs centre decologie fonctionnelle et evolutive"] <- "france"
  datafile$inst[datafile$inst == "college of staten island cuny"] <- "cuny college of staten island"
  datafile$inst[datafile$inst == "colorado cooperative fish and wildlife unit"] <- "usgs colorado cooperative fish and wildlife unit"
  datafile$inst[datafile$inst == "commonwealth university"] <- "virginia commonwealth university"
  datafile$inst[datafile$inst == "mediterranean institute for advanced studies"] <- "csic mediterranean institute for advanced studies"
  datafile$inst[datafile$inst == "csic institut mediterrani destudis avencats"] <- "csic mediterranean institute for advanced studies"
  datafile$inst[datafile$inst == "csic institut mediterrani destudis avencats"] <- "csic mediterranean institute for advanced studies"
  datafile$inst[datafile$inst == "csic instituto de ciencias del mar"] <- "csic mediterranean institute for advanced studies"
  datafile$country[datafile$inst == "doe brookhaven national laboratory"] <- "usa"
  datafile$country[datafile$inst == "doe brookhaven national laboratory"] <- "usa"
  datafile$inst[datafile$inst == "environment canada wildlife research division"] <- "environment canada wildlife research east"
  datafile$inst[datafile$inst == "evolutionary biology centre"] <- "university of uppsala"
  datafile$country[datafile$inst == "florida dept of natural resources bureau of marine research"] <- "usa"
  datafile$inst[datafile$inst == "forschungsinstitut senckenberg"] <- "senckenberg research institute"
  datafile$inst[datafile$inst == "gorgan university of agric sci"] <- "gorgan university of agricultural sciences"
  datafile$country[datafile$inst == "griffith university"] <- "australia"
  datafile$country[datafile$inst == "haus nr9"] <- "germany"
  datafile$country[datafile$inst == "hebrew university of jerusalem"] <- "israel"
  datafile$country[datafile$inst == "hokkaido university"] <- "japan"
  datafile$country[datafile$inst == "humboldt university of berlin"] <- "germany"
  datafile$inst[datafile$inst == "hunter college cuny"] <- "cuny hunter college"
  datafile$country[datafile$inst == "icbs ufal and oxford university"] <- "brazil"
  datafile$inst[datafile$inst == "icbs ufal and oxford university"] <- "universidade federal de alagoas"
  datafile$inst[datafile$inst == "inra"] <- "french national institute for agricultural research inra"
  datafile$country[datafile$inst == "institut national scientifique et technnique d oceanographie et de peche"] <- "tunisia"
  datafile$country[datafile$inst == "institute of marine biology of crete"] <- "greece"
  datafile$inst[datafile$inst == "institute of tropical forestry"] <- "usfs international institute of tropical forestry"
  datafile$inst[datafile$inst == "instituto de biociencias"] <- "universidade de sao paulo"
  datafile$inst[datafile$inst == "instituto mediterraneo de estudios avanzados (imedea)"] <- "csic mediterranean institute for advanced studies"
  datafile$country[datafile$inst == "international institute for applied systems analysis"] <- "austria"
  datafile$country[datafile$inst == "international institute of tropical forestry"] <- "puerto rico"
  datafile$inst[datafile$inst == "istituto per l¬¢ambiente marino costiero"] <- "instituto per lambiente marino costiero"
  datafile$inst[datafile$inst == "istituto per l¢ambiente marino costiero"] <- "instituto per lambiente marino costiero"
  datafile$country[datafile$inst == "james cook university"] <- "australia"
  datafile$country[datafile$inst == "james cook university"] <- "australia"
  datafile$inst[datafile$inst == "kings college"] <- "kings college london"
  datafile$country[datafile$inst == "kyushu university"] <- "japan"
  datafile$country[datafile$inst == "laboratorio di geologia marina del cnr"] <- "italy"
  datafile$country[datafile$inst == "lakehead university"] <- "canada"
  datafile$inst[datafile$inst == "lancaster"] <- "lancaster university"
  datafile$country[datafile$inst == "lancaster university"] <- "united kingdom"
  datafile$country[datafile$inst == "leibniz institut fur meereswissenschaften ifmgeomar"] <- "germany"
  # datafile$inst[datafile$inst=="lincoln university"]<-"university of lincoln"
  datafile$inst[datafile$inst == "lincoln college"] <- "lincoln university"
  # datafile$country[datafile$inst=="university of lincoln"]<-"united kingdom"
  # datafile$country[datafile$inst=="lincoln university"]<-"new zealand"
  datafile$inst[datafile$inst == "ludwig maximilian universitat munchen"] <- "ludwig maximilian university of munich"
  datafile$inst[datafile$inst == "naturalis biodivers ctr"] <- "naturalis biodiversity center"
  datafile$country[datafile$inst == "luminy universite d aix marseille"] <- "france"
  datafile$inst[datafile$inst == "macaulay¬†institute"] <- "macaulay land use research institute"
  datafile$country[datafile$inst == "max planck institute for behavioral physiology"] <- "germany"
  datafile$country[datafile$inst == "max planck institute for demographic research"] <- "germany"
  datafile$country[datafile$inst == "mcgill university"] <- "canada"
  datafile$inst[datafile$inst == "meggers"] <- "smithsonian national museum of natural history"
  datafile$country[datafile$inst == "memorial university of newfoundland"] <- "canada"
  datafile$inst[datafile$inst == "millbrook"] <- "cary institute of ecosystem studies"
  datafile$country[datafile$inst == "monterey bay aquarium research institute"] <- "usa"
  datafile$inst[datafile$inst == "mpi jena"] <- "max planck institute for human history"
  datafile$inst[datafile$inst == "national institute of food and agriculture"] <- "usda national institute of food and agriculture"
  datafile$country[datafile$inst == "national institute of water and atmospheric research"] <- "new zealand"
  datafile$country[datafile$inst == "national museum of natural history france"] <- "france"
  datafile$inst[datafile$inst == "national museum of natural history smithsonian institution"] <- "smithsonian national museum of natural history"
  datafile$inst[datafile$inst == "national natural history museum paris"] <- "national museum of natural history france"
  datafile$country[datafile$inst == "national university of rosario"] <- "argentina"
  datafile$country[datafile$inst == "naturhistorisches museum wien"] <- "austria"
  datafile$inst[datafile$inst == "nederlands institut voor onderzoek der zee"] <- "royal netherlands institute for sea research"
  datafile$inst[datafile$inst == "nederlands institut voor onderzoek der zee"] <- "royal netherlands institute for sea research"
  datafile$country[datafile$inst == "new york botanical garden"] <- "usa"
  datafile$inst[datafile$inst == "newfoundland"] <- "missing"
  datafile$inst[datafile$inst == "nioo"] <- "netherlands institute of ecology nioo knaw"
  datafile$inst[datafile$inst == "netherlands institute of ecology"] <- "netherlands institute of ecology nioo knaw"
  datafile$inst[datafile$inst == "netherlands institute of ecology; wageningen university and research centre"] <- "netherlands institute of ecology nioo knaw"

  # datafile$notes[datafile$inst=="no one by this name"]<-"2x if on ed board this year"
  datafile$inst[datafile$inst == "northeast fisheries science center"] <- "noaa northeast fisheries science center"
  datafile$inst[datafile$inst == "oklahoma university"] <- "university of oklahoma"
  datafile$country[datafile$inst == "oxford university"] <- "united kingdom"
  datafile$inst[datafile$inst == "pacific sw research station us forest service"] <- "usfs pacific sw research station"
  datafile$inst[datafile$inst == "prairie wildlife research center"] <- "usgs prairie wildlife research center"
  datafile$country[datafile$state == "puerto rico"] <- "puerto rico"
  datafile$inst[datafile$inst == "queens college city university of new york"] <- "cuny queens college"
  datafile$inst[datafile$inst == "sao paulo state university"] <- "universidade estadual paulista"
  datafile$inst[datafile$inst == "retired (no affiliation)"] <- "retired"
  datafile$country[datafile$inst == "rice university"] <- "usa"
  datafile$inst[datafile$inst == "riso dtuniversity of national laboratory for sustainable energy"] <- "riso dtu national laboratory for sustainable energy"
  datafile$country[datafile$inst == "royal botanic gardens kew"] <- "united kingdom"
  datafile$inst[datafile$inst == "royal botanic gardens melbourne university of melbourne"] <- "royal botanic gardens melbourne"
  datafile$inst[datafile$LAST_NAME == "rutzler"] <- "smithsonian national museum of natural history"
  datafile$inst[datafile$inst == "senckenberg research institute frankfurt"] <- "senckenberg research institute"
  datafile$unit[datafile$inst == "southwest research and extension center"] <- "southwest research and extension center"
  datafile$inst[datafile$inst == "southwest research and extension center"] <- "university of arkansas"
  datafile$inst[datafile$inst == "st de pytosociologie fondamental"] <- "centre regional de phytosociologie"
  datafile$inst[datafile$inst == "state university of new york albany"] <- "suny albany"
  datafile$inst[datafile$inst == "state university of new york binghamton"] <- "suny binghamton"
  datafile$inst[datafile$inst == "state university of new york college of environmental science and forestry"] <- "suny college of environmental science and forestry"
  datafile$inst[datafile$inst == "state university of new york state university of new york"] <- "suny state university of new york"
  datafile$inst[datafile$inst == "state university of new york stony brook"] <- "suny stony brook"
  datafile$inst[datafile$inst == "station detudes des gorilles et chimpanzes (segc)"] <- "station detudes des gorilles et chimpanzes"
  datafile$inst[datafile$inst == "swedish agricultural university"] <- "swedish university of agricultural sciences"
  datafile$inst[datafile$inst == "swedish university of agricultural science"] <- "swedish university of agricultural sciences"
  datafile$inst[datafile$inst == "swiss federal institute wsl"] <- "wsl swiss federal institute for forest snow and landscape research"
  datafile$unit[datafile$inst == "tidewater agricultural res and ext ctr"] <- "tidewater agricultural res and ext ctr"
  datafile$inst[datafile$inst == "tidewater agricultural res and ext ctr"] <- "virginia polytechnic institute and state university"
  datafile$country[datafile$inst == "tijuana estuarine research reserve"] <- "usa"
  datafile$inst[datafile$inst == "tulane"] <- "tulane university"
  datafile$inst[datafile$inst == "ufz center for environmental research"] <- "helmholtz centre for environmental research ufz"
  datafile$inst[datafile$inst == "ufz centre for environmental research leipzig halle"] <- "helmholtz centre for environmental research ufz leipzig halle"
  datafile$inst[datafile$inst == "ufz helmholtz ctr environm res"] <- "helmholtz centre for environmental research ufz"
  datafile$inst[datafile$inst == "universidade federal do rio grande do norte (ufrn)"] <- "universidade federal do rio grande do norte"
  datafile$country[datafile$inst == "universita degli studi di padova"] <- "italy"
  datafile$country[datafile$inst == "universita di napoli"] <- "italy"
  datafile$country[datafile$inst == "universita di pisa"] <- "italy"
  datafile$country[datafile$inst == "universita die napoli frederico ii"] <- "italy"
  datafile$country[datafile$inst == "universite de lausanne"] <- "switzerland"
  datafile$country[datafile$inst == "universite laval quebec"] <- "canada"
  datafile$country[datafile$inst == "university college dublin"] <- "ireland"
  datafile$country[datafile$inst == "university college london"] <- "united kingdom"
  datafile$country[datafile$inst == "university marine biological station millport"] <- "united kingdom"
  datafile$inst[datafile$inst == "university of illinois university urbana champaign"] <- "university of illinois urbana champaign"
  datafile$country[datafile$inst == "university of kentucky"] <- "usa"
  datafile$country[datafile$inst == "university of leiden"] <- "netherlands"
  datafile$country[datafile$inst == "university of maryland center for environmental science"] <- "usa"
  datafile$country[datafile$inst == "university of oregon"] <- "usa"
  datafile$country[datafile$inst == "university of oslo"] <- "norway"
  datafile$country[datafile$inst == "university of pennsylvania"] <- "usa"
  datafile$country[datafile$inst == "university of puerto rico"] <- "puerto rico"
  datafile$country[datafile$inst == "university of regina"] <- "canada"
  datafile$country[datafile$inst == "university of sheffield"] <- "united kingdom"
  datafile$country[datafile$inst == "university of sherbrooke"] <- "canada"
  datafile$country[datafile$inst == "university of southampton"] <- "united kingdom"
  datafile$country[datafile$inst == "university of stockholm"] <- "sweden"
  datafile$country[datafile$inst == "university of sydney"] <- "australia"
  datafile$country[datafile$inst == "university of texas austin"] <- "usa"
  datafile$country[datafile$inst == "university of the sunshine"] <- "australia"
  datafile$country[datafile$inst == "university of toronto"] <- "canada"
  datafile$inst[datafile$inst == "university of university amsterdam"] <- "university of amsterdam"
  datafile$country[datafile$inst == "university of vienna"] <- "austria"
  datafile$country[datafile$inst == "university of waikato"] <- "new zealand"
  datafile$country[datafile$inst == "university of western ontario"] <- "canada"
  datafile$inst[datafile$inst == "university of zuidema"] <- "university of wageningen"
  datafile$country[datafile$inst == "university pompeu fabra"] <- "spain"
  datafile$country[datafile$inst == "university system of maryland"] <- "usa"
  datafile$inst[datafile$inst == "uppsala university"] <- "university of uppsala"
  datafile$inst[datafile$inst == "us arid land agricultural research center"] <- "usda arid land agricultural research center"
  datafile$inst[datafile$inst == "us geological society"] <- "us geological survey"
  datafile$inst[datafile$inst == "virginia commonwealth u"] <- "virginia commonwealth university"
  datafile$inst[datafile$inst == "biological station of donana"] <- "csic donana biological station"
  datafile$inst[datafile$inst == "food and agriculture organization fao"] <- "fao food and agriculture organization"
  datafile$inst[datafile$inst == "imperial college of london"] <- "imperial college london"
  datafile$inst[datafile$inst == "institute¬†of terrestrial ecology"] <- "institute of terrestrial ecology"
  datafile$inst[datafile$inst == "joint nature conservation committee¬†"] <- "joint nature conservation committee"
  datafile$inst[datafile$inst == "king‚äôs college"] <- "kings college"
  datafile$inst[datafile$inst == "macaulay¬†institute"] <- "macaulay land use research institute"
  datafile$inst[datafile$inst == "ceh"] <- "nerc centre for ecology and hydrology"
  datafile$inst[datafile$inst == "nerc ctr ecol and hydrol"] <- "nerc centre for ecology and hydrology"
  datafile$inst[datafile$inst == "ceh banchory"] <- "nerc centre for ecology and hydrology banchory"
  datafile$inst[datafile$inst == "ceh monks wood"] <- "nerc centre for ecology and hydrology monks wood"
  datafile$inst[datafile$inst == "netherlands inst ecol nioo knaw"] <- "netherlands institute of ecology nioo knaw"
  datafile$inst[datafile$inst == "northeastern univ"] <- "northeastern university"
  datafile$notes[datafile$inst == "western cotton research lab"] <- "formerly the western cotton research lab"
  datafile$inst[datafile$inst == "western cotton research lab"] <- "usda arid land agricultural research center"
  datafile$inst[datafile$inst == "oregon state univ"] <- "oregon state university"
  datafile$inst[datafile$inst == "penn state"] <- "pennsylvania state university"
  datafile$inst[datafile$inst == "sw texas state university"] <- "southwest texas state university"
  datafile$inst[datafile$inst == "tech university darmstadt"] <- "technical university of darmstadt"
  datafile$inst[datafile$inst == "technische universitat darmstadt"] <- "technical university of darmstadt"
  datafile$inst[datafile$inst == "truman state"] <- "truman state university"
  datafile$inst[datafile$inst == "university of alicante"] <- "universidad de alicante"
  datafile$inst[datafile$inst == "university of vigo"] <- "universidad de vigo"
  datafile$inst[datafile$inst == "university of rey juan carlos"] <- "universidad rey juan carlos"
  datafile$inst[datafile$inst == "universitat osnabr√ºck"] <- "universitat osnabruck"
  datafile$inst[datafile$inst == "universidad de costa rica"] <- "university of costa rica"
  datafile$inst[datafile$inst == "university exeter"] <- "university of exeter"
  datafile$inst[datafile$inst == "university glasgow"] <- "university of glasgow"
  datafile$inst[datafile$inst == "university of natal"] <- "university of kwazulu natal"
  datafile$inst[datafile$inst == "university of kwazuluniversity of natal"] <- "university of kwazulu natal"
  datafile$unit[datafile$inst == "arnold arboretuniversity of harvard university"] <- "arnold arboretum"
  datafile$inst[datafile$inst == "arnold arboretuniversity of harvard university"] <- "harvard university"

  datafile$inst[datafile$inst == "university of london college imperial college of science and technology"] <- "university of london imperial college of science and technology"
  datafile$inst[datafile$inst == "university of london imperial college of science and technology"] <- "university of london imperial college of science and technology"
  datafile$inst[datafile$inst == "universit√© de montreal"] <- "university of montreal"
  datafile$inst[datafile$inst == "university oregon"] <- "university of oregon"
  datafile$inst[datafile$inst == "oxford university"] <- "university of oxford"
  datafile$inst[datafile$inst == "university oxford"] <- "university of oxford"
  datafile$inst[datafile$inst == "universite du quebec"] <- "university of quebec"
  datafile$inst[datafile$inst == "universita del salento"] <- "university of salento"
  datafile$inst[datafile$inst == "umea univ"] <- "university of umea"
  datafile$inst[datafile$inst == "university¬†of¬†wageningen"] <- "wageningen university and research"
  datafile$inst[datafile$inst == "waikato university"] <- "university of waikato"
  datafile$inst[datafile$inst == "utrecht"] <- "utrecht university"
  datafile$inst[datafile$inst == "alterra research institute for green world"] <- "wageningen environmental research (alterra)"
  datafile$inst[datafile$inst == "wageningen university research center alterra"] <- "wageningen environmental research (alterra)"
  datafile$inst[datafile$inst == "wageningen agricultural university and research center alterra"] <- "wageningen environmental research (alterra)"

  datafile$inst[datafile$inst == "'agricultural university"] <- "wageningen university and research"
  datafile$inst[datafile$inst == "university of wageningen"] <- "wageningen university and research"
  datafile$inst[datafile$inst == "wageningen agricultural university"] <- "wageningen university and research"


  datafile$inst[datafile$inst == "retired"] <- "unaffiliated"
  datafile$inst[datafile$inst == "st andrews"] <- "university of st andrews"
  datafile$inst[datafile$inst == "university of ft hare"] <- "university of fort hare"



  datafile$inst[datafile$inst == "göttingen"] <- "university of gottingen"
  datafile$inst[datafile$inst == "jena"] <- "max planck institute of biogeochemistry"
  datafile$inst[datafile$inst == "peking"] <- "peking university"
  datafile$inst[datafile$inst == "santiago"] <- "pontificia universidad catolica de chile"
  datafile$inst[datafile$inst == "2x university of munster"] <- "university of munster"
  datafile$inst[datafile$inst == "regina"] <- "university of regina"
  datafile$inst[datafile$inst == "guelph"] <- "university of guelph"
  datafile$inst[datafile$inst == "sackville"] <- "mount allison university"
  datafile$inst[datafile$inst == "saskatchewan"] <- "university of saskatchewan"
  datafile$inst[datafile$inst == "toronto"] <- "university of toronto mississauga"
  datafile$inst[datafile$inst == "vancouver"] <- "university of british columbia"

  datafile$inst[datafile$inst == "montpellier"] <- "cnrs centre decologie fonctionnelle et evolutive"
  datafile$inst[datafile$inst == "cnrs centre national de la recherche scientifique" & datafile$city == "montpellier"] <- "cnrs centre decologie fonctionnelle et evolutive"
  datafile$inst[datafile$inst == "cnrs centre national de la recherche scientifique" & datafile$unit == "centre d'ecologie fonctionnelle et evolutive"] <- "cnrs centre decologie fonctionnelle et evolutive"
  datafile$inst[datafile$inst == "cnrs centre national de la recherche scientifique" & datafile$unit == "ctr ecol fonct & evolut"] <- "cnrs centre decologie fonctionnelle et evolutive"




  datafile$country[datafile$inst == "university of aberdeen"] <- "united kingdom"
  datafile$country[datafile$inst == "university of st andrews"] <- "united kingdom"
  datafile$country[datafile$inst == "university of freiburg"] <- "germany"
  datafile$country[datafile$inst == "texas a and m university"] <- "usa"
  datafile$country[datafile$inst == "university of arkansas"] <- "usa"
  datafile$country[datafile$inst == "university of arkansas little rock"] <- "usa"
  datafile$country[datafile$inst == "university of oxford" & datafile$editor_id == 2823] <- "united kingdom"
  datafile$country[datafile$inst == "university of oxford" & datafile$editor_id == 3148] <- "united kingdom"
  datafile$country[datafile$inst == "university of oxford" & datafile$year == 2005 & datafile$journal == "jape"] <- "united kingdom"
  datafile$country[datafile$inst == "university of cape town" & datafile$editor_id == 2153] <- "south africa"
  datafile$country[datafile$inst == "university of innsbruck"] <- "austria"
  datafile$country[datafile$inst == "cinvestav irapuato"] <- "mexico"
  datafile$state[datafile$inst == "cinvestav irapuato"] <- "guanajuato"
  datafile$city[datafile$inst == "cinvestav irapuato"] <- "irapuato"
  datafile$city[datafile$city == "basal"] <- "basel"

  datafile$country[datafile$inst == "queen mary university of london"] <- "united kingdom"
  datafile$country[datafile$inst == "swedish university of agricultural sciences"] <- "sweden"
  datafile$country[datafile$inst == "vrije universiteit amsterdam"] <- "netherlands"
  datafile$country[datafile$inst == "university of western australia"] <- "australia"

  datafile$country[datafile$country == "brazil and uk"] <- "brazil"


  datafile$inst[datafile$inst == "innsbruck"] <- "university of innsbruck"
  datafile$inst[datafile$inst == "free university amsterdam"] <- "vrije universiteit amsterdam"
  datafile$inst[datafile$inst == "california state university" & datafile$city == "fresno"] <- "california state university fresno"
  datafile$inst[datafile$inst == "california state university" & datafile$city == "sacramento"] <- "california state university sacramento"
  datafile$inst[datafile$inst == "university of floridaz center for environmental research"] <- "helmholtz centre for environmental research"


  datafile$inst[datafile$inst == "csiro commonwealth scientific and industrial research organisation" &
    datafile$unit == "division of plant industry"] <- "csiro plant industry"
  datafile$inst[datafile$inst == "csiro commonwealth scientific and industrial research organisation" &
    datafile$unit == "division of wildlife and ecology"] <- "csiro wildlife and ecology"
  datafile$inst[datafile$inst == "csiro commonwealth scientific and industrial research organisation" &
    datafile$unit == "division of forest research"] <- "csiro forest research"

  datafile$inst[datafile$inst == "csiro division of wildlife and ecology"] <- "csiro wildlife and ecology"

  datafile$country[datafile$inst == "usfs"] <- "usa"

  datafile$inst[datafile$inst == "usfs"] <- "usfs us forest service"

  datafile$inst[datafile$inst == "usgs"] <- "usfs us geological survey"
  datafile$inst[datafile$inst == "usda"] <- "usda us department of agriculture"
  datafile$inst[datafile$inst == "vigo"] <- "university of vigo"
  datafile$inst[datafile$inst == "christchurch"] <- "dsir land resources"
  datafile$inst[datafile$inst == "cork"] <- "university college cork"
  datafile$inst[datafile$inst == "krakow"] <- "jagiellonian university"
  datafile$inst[datafile$inst == "ulster"] <- "university of ulster"
  datafile$inst[datafile$inst == "university of ulm"] <- "ulm university"
  datafile$inst[datafile$inst == "ulm"] <- "ulm university"
  datafile$inst[datafile$inst == "minnesota"] <- "university of st thomas"
  datafile$inst[datafile$inst == "montana"] <- "university of montana"
  datafile$inst[datafile$inst == "maine"] <- "university of southern maine"
  datafile$inst[datafile$inst == "franklin and marshall university"] <- "franklin and marshall"
  datafile$inst[datafile$inst == "pretoria"] <- "university of pretoria"
  datafile$inst[datafile$inst == "tennessee"] <- "university of tennessee"
  datafile$inst[datafile$inst == "salzburg"] <- "university of salzburg"
  datafile$inst[datafile$inst == "sydney"] <- "university of sydney"
  datafile$inst[datafile$inst == "university of western sydney"] <- "western sydney university"
  datafile$inst[datafile$inst == "york"] <- "university of york"
  datafile$inst[datafile$inst == "london"] <- "queen mary university of london"
  datafile$inst[datafile$inst == "queen’s university"] <- "queens university"
  datafile$inst[datafile$inst == "queens university kingston"] <- "queens university"
  datafile$inst[datafile$inst == "zoological society"] <- "zoological society of london"


  datafile$inst[datafile$inst == "kyoto"] <- "kyoto university"
  datafile$inst[datafile$inst == "hokkaido"] <- "hokkaido university"
  datafile$inst[datafile$inst == "swansea"] <- "swansea university"
  datafile$inst[datafile$inst == "dundee"] <- "university of dundee"
  datafile$inst[datafile$inst == "reading"] <- "university of reading"
  datafile$inst[datafile$inst == "dartmouth"] <- "dartmouth college"
  datafile$inst[datafile$inst == "monash"] <- "monash university"
  datafile$inst[datafile$inst == "syracuse"] <- "suny college of environmental science and forestry"

  datafile$inst[datafile$inst == "stockholm university"] <- "university of stockholm"
  datafile$inst[datafile$inst == "university of utrecht"] <- "utrecht university"
  datafile$inst[datafile$inst == "madrid"] <- "csic national museum of natural sciences"
  datafile$inst[datafile$unit == "estacion experimental de zonas aridas"] <- "csic estacion experimental de zonas aridas"
  datafile$inst[datafile$inst == "csic"] <- "csic consejo superior de investigaciones cientificas"
  datafile$inst[datafile$inst == "cifor"] <- "cifor center for international forestry research"
  datafile$inst[datafile$inst == "center for international forestry research"] <- "cifor center for international forestry research"

  datafile$inst[datafile$inst == "natural environment research council"] <- "nerc natural environment research council"
  datafile$inst[datafile$inst == "nerc"] <- "nerc natural environment research council"
  datafile$inst[datafile$inst == "university of new england armidale"] <- "university of new england"
  datafile$inst[datafile$inst == "imperial college london"] <- "imperial college of science and technology"
  datafile$inst[datafile$inst == "king’s college"] <- "kings college london"
  datafile$inst[datafile$inst == "academy of sciences of the czech republic"] <- "czech academy of sciences"
  datafile$inst[datafile$inst == "academy of sciences of czech republic"] <- "czech academy of sciences"
  datafile$inst[datafile$inst == "neri"] <- "neri national environmental research institute"
  datafile$inst[datafile$inst == "national environmental research institute"] <- "neri national environmental research institute"
  datafile$inst[datafile$inst == "tartu"] <- "university of tartu"
  datafile$inst[datafile$inst == "tartu university"] <- "university of tartu"
  datafile$inst[datafile$inst == "bethesda"] <- "national cancer institute"
  datafile$inst[datafile$inst == "illinois"] <- "university of illinois"
  datafile$inst[datafile$inst == "hawaii"] <- "university of hawaii manoa"
  datafile$inst[datafile$inst == "university of illinois urbana champaign"] <- "university of illinois"
  datafile$inst[datafile$inst == "marine biological association of the united kingdom lab"] <- "marine biological association"
  datafile$inst[datafile$inst == "british antarctic survey"] <- "nerc british antarctic survey"
  datafile$inst[datafile$inst == "university college"] <- "university college london"

  datafile$inst[datafile$inst == "university of london imperial college of science and technology"] <- "imperial college london"
  datafile$inst[datafile$inst == "university london imperial coll sci technol and med"] <- "imperial college london"
  datafile$inst[datafile$inst == "imperial college of science and technology"] <- "imperial college london"
  datafile$inst[datafile$inst == "university du quebec a montreal"] <- "university of quebec"
  datafile$inst[datafile$inst == "universite de rennes"] <- "universite de rennes 1"

  datafile$notes[datafile$inst == "universite montpelier" | datafile$inst == "universite montpelier 2"] <- "um1 and um2 reunited as um in 2015"

  datafile$country[datafile$inst == "station biologique de la tour du valat"] <- "france"

  datafile$inst[datafile$city == "reno" & datafile$inst == "university of nevada"] <- "university of nevada reno"
  datafile$inst[datafile$city == "hilo" & datafile$inst == "university of hawaii"] <- "university of hawaii hilo"
  datafile$inst[datafile$city == "honolulu" & datafile$inst == "university of hawaii"] <- "university of hawaii manoa"
  datafile$inst[datafile$inst == "university of indiana"] <- "indiana university bloomington"
  datafile$inst[datafile$inst == "indiana university"] <- "indiana university bloomington"
  datafile$inst[datafile$inst == "nebraska"] <- "university of nebraska lincoln"
  datafile$inst[datafile$inst == "pittsburgh"] <- "university of pittsburgh"
  datafile$inst[datafile$inst == "national autonomous university mexico"] <- "universidad nacional autonoma de mexico"
  datafile$inst[datafile$inst == "unam"] <- "universidad nacional autonoma de mexico"
  datafile$inst[datafile$inst == "seoul"] <- "seoul national university"
  datafile$inst[datafile$inst == "us fish and wildlife service"] <- "usfws united states fish and wildlife service"
  datafile$inst[datafile$inst == "usfws united state fish and wildlife service"] <- "usfws united states fish and wildlife service"
  datafile$inst[datafile$inst == "fsu"] <- "florida state university"
  datafile$inst[datafile$inst == "usfs us geological survey"] <- "usgs united states geological survey"
  datafile$inst[datafile$inst == "university of lund"] <- "lund university"
  datafile$inst[datafile$inst == "labratory of general and apllied ichthyology"] <- "national museum of natural history france"


  datafile$inst[datafile$country == "sweden" & datafile$inst == "university of agricultural sciences"] <- "swedish university of agricultural sciences"

  datafile$inst[datafile$city == "chicago" & datafile$inst == "university of illinois"] <- "university of illinois chicago"


  datafile$inst[datafile$LAST_NAME == "luque" & datafile$inst == "same as other ag inst?"] <-
    "national research institute of science and technology for environment and agriculture (irstea)"

  
  
  
  
  datafile$inst[datafile$LAST_NAME == "baudry" & datafile$inst == "same as other ag inst?"] <- "inra centre bretagne-normandie"



  datafile$inst[datafile$inst == "university of british columbia" & datafile$city == "kelowna"] <- "university of british columbia okanagan"
  datafile$inst[datafile$inst == "university of british columbia" & datafile$city == "vancouver"] <- "university of british columbia vancouver"

  datafile$inst[datafile$inst == "university of texas" & datafile$city == "austin"] <- "university of texas austin"
  datafile$inst[datafile$inst == "university of texas" & datafile$city == "austin"] <- "university of texas austin"
  datafile$inst[datafile$inst == "university of massachusetts" & datafile$city == "amherst"] <- "university of massachusetts amherst"
  datafile$inst[datafile$inst == "university of north carolina" & datafile$city == "chapel hill"] <- "university of north carolina chapel hill"
  datafile$inst[datafile$inst == "university of minnesota" & datafile$city == "duluth"] <- "university of minnesota duluth"
  datafile$inst[datafile$inst == "university of arkansas" & datafile$city == "fayetteville"] <- "university of arkansas fayetteville"
  datafile$inst[datafile$inst == "university of hawaii" & datafile$city == "hilo"] <- "university of hawaii hilo"
  datafile$inst[datafile$inst == "university of hawaii" & datafile$city == "honolulu"] <- "university of hawaii manoa"
  datafile$inst[datafile$inst == "university of alaska" & datafile$city == "fairbanks"] <- "university of alaska fairbanks"
  datafile$inst[datafile$inst == "university of minnesota" & (datafile$city == "minneapolis" | datafile$city == "st paul")] <- "university of minnesota twin cities"
  datafile$inst[datafile$inst == "james cook university" & datafile$city == "townsville"] <- "james cook university townsville"

  datafile$unit[datafile$inst == "smithsonian migratory bird center"] <- "smithsonian migratory bird center"
  datafile$inst[datafile$inst == "smithsonian migratory bird center"] <- "smithsonian national zoological park"






  datafile$inst[datafile$inst == "university of wisconsin" & datafile$city == "madison"] <- "university of wisconsin madison"
  datafile$inst[datafile$inst == "university of washington" & datafile$city == "seattle"] <- "university of washington seattle"
  datafile$inst[datafile$inst == "university of washington" & datafile$city == "seattle"] <- "university of washington seattle"

  datafile$inst[datafile$inst == "university of toronto" & datafile$city == "mississauga"] <- "university of toronto mississauga"
  datafile$inst[datafile$inst == "university of toronto" & datafile$city == "scarborough"] <- "university of toronto scarborough"

  datafile$country[datafile$inst == "usgs forest and rangeland ecosystem science center"] <- "usa"
  datafile$country[datafile$inst == "university of bielefeld"] <- "germany"
  datafile$country[datafile$inst == "university of oxford"] <- "united kingdom"
  datafile$country[datafile$inst == "university of hull"] <- "united kingdom"
  datafile$country[datafile$inst == "university of edinburgh"] <- "united kingdom"
  datafile$country[datafile$inst == "university of essex"] <- "united kingdom"
  datafile$country[datafile$inst == "university of turku"] <- "finland"

  datafile$country[datafile$inst == "university of washington seattle"] <- "usa"
  datafile$country[datafile$inst == "carnegie institution"] <- "usa"


  datafile$inst[datafile$inst == "wsl swiss federal research institute" & datafile$city == "birmensdorf"] <-
    "wsl swiss federal institute for forest snow and landscape research birmensdorf"

  # datafile$inst[datafile$inst=="wsl swiss federal research institute" & datafile$city=="davos" ]<-
  #   "wsl swiss federal institute for forest snow and landscape research davos"
  # # OR
  datafile$inst[datafile$inst == "wsl swiss federal research institute" & datafile$city == "davos"] <-
    "wsl swiss federal institute for snow and avalanche research davos"


  datafile$inst[datafile$inst == "swiss federal institute for forest snow and landscape research"] <-
    "wsl swiss federal institute for forest snow and landscape research birmensdorf"

  datafile$inst[datafile$inst == "swiss federal institute for forest snow and landscape research wsl"] <-
    "wsl swiss federal institute for forest snow and landscape research birmensdorf"

  datafile$inst[datafile$inst == "wsl swiss federal institute for forest snow and landscape research"] <-
    "wsl swiss federal institute for forest snow and landscape research birmensdorf"

  datafile$inst[datafile$inst == "wsl institute for snow and avalanche research sfl"] <-
    "wsl swiss federal institute for snow and avalanche research davos"

  datafile$inst[datafile$inst == "wsl swiss federal research institute"] <-
    "wsl swiss federal institute for forest snow and landscape research birmensdorf"

  datafile$inst[datafile$inst == "swiss federal institute of technology"] <-
    "eth swiss federal institute of technology zurich"

  datafile$inst[datafile$inst == "swiss federal institute of aquatic science and technology"] <-
    "eawag swiss federal institute of aquatic science and technology"


  datafile$inst[datafile$inst == "vu university amsterdam"] <-
    "vrije universiteit amsterdam"

  datafile$inst[datafile$inst == "cnrs iri montpellier"] <- "MIVEGEC"


  datafile$inst[datafile$inst == "french national institute for agricultural research inra"] <- "inra national institute for agricultural research"
  datafile$inst[datafile$inst == "french national institute for agricultural research centre de nancy"] <- "inra national institute for agricultural research centre de nancy"
  datafile$inst[datafile$inst == "french agricultural research centre for international development"] <- "cirad agricultural research centre for international development"

  datafile$inst[datafile$inst == "french national school of forestry engref"] <- "engref french national school of forestry"
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-




# spelling corrections states
  
  
  

  
    
  
  datafile$state[datafile$state == "massachussetts"] <-
    "ma"
  
  datafile$state[datafile$state == "missisippi"] <-
    "ms"
  
  datafile$state[datafile$state == "alaksa"] <-
    "ak"
  
  datafile$state[datafile$state == "lousiana"] <-
    "LA"
  
  datafile$state[datafile$state == "washington dc"] <-
  "district of columbia"
  
  datafile$state[datafile$state == "west va"] <-
    "wv"
  
  datafile$state[datafile$state == "south carolin"] <-
    "sc"
  
datafile$city[datafile$state == "east lansing"] <-
  "east lansing"
datafile$state[datafile$state == "east lansing"] <-
  "mi"

  datafile$inst <- gsub("[.]", "", datafile$inst)
  # datafile$inst[datafile$inst== "istituto per l¬¢ambiente marino costiero" ]<-"instituto per lambiente marino costiero"
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-
  # datafile$inst[datafile$inst==  ]<-

  gsub("goottingen", "gottingen", datafile$inst)
  gsub("istituto per looambiente marino costiero", "istituto per lambiente marino costiero", datafile$inst)
  gsub("macaulayooinstitute", "istituto per lambiente marino costiero", datafile$inst)
  gsub("universitoo de montreal", "macaulay land use research institute", datafile$inst)
  datafile$ascii <- NULL

  # alldata$validenc<-validenc(alldata$country)
  # summary(alldata$validenc<-validenc(alldata$country))
  # foo<-alldata %>% filter(validenc==false)
  # foo<-distinct(foo, country)
  # dont forget - filter will remove all with na in the conditions
  datafile <- datafile %>% replace_na(list(inst = "missing"))
  datafile <- datafile %>% filter(!inst == "no one by this name")


  return(datafile)
}

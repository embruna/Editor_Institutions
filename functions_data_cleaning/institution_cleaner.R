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




DATAFILE$COUNTRY[DATAFILE$INST=="University of New South Wales"]<-"Australia"
DATAFILE$COUNTRY[DATAFILE$INST=="University of Guelph"]<-"Canada"
DATAFILE$COUNTRY[DATAFILE$INST=="Instituto Mediterraneo de Estudios Avanzados (IMEDEA)"]<-"Spain"
##############################################################
# COrrecting the country in whihc an Editor is based
##############################################################
DATAFILE$COUNTRY<-as.factor(DATAFILE$COUNTRY)
DATAFILE<-droplevels(DATAFILE)
levels(DATAFILE$COUNTRY)
##############################################################
# STILL TO DO 
##############################################################

# NEED TO CONFIRM WHAT PART OF USSR IN WHICH THE AUTHOR WAS BASED
levels(DATAFILE$COUNTRY) <- c(levels(DATAFILE$COUNTRY),"Russia","Scotland","Northern Ireland")
DATAFILE$COUNTRY[DATAFILE$COUNTRY=="USSR"]<-"Russia"

# TODO: NEED DO DELETE THESE
which(DATAFILE$COUNTRY=="")

##############################################################
##############################################################
# CLEAN-UP OF INSTITUTIONS  
##############################################################
##############################################################
DATAFILE$INST[DATAFILE$INST=="."]<-NA
DATAFILE<-as.data.frame(DATAFILE)
DATAFILE$JOURNAL<-as.factor(DATAFILE$JOURNAL)
# Correcting the Institution where an Editor is based
# levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"State University of New York College of Environmental Science and Forestry")
# levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"Forestry and Forest Products Research Institute")
# levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"University of Minnesota Duluth")
# levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"University of Minnesota Crookston")
# # levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"University of Toronto Mississauga")
# levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"CNRS Centre dEcologie Fonctionnelle et Evolutive")
##############################################################
##############################################################
# Correcting or systematizing the name/speclling of an institution
##############################################################
##############################################################
# levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"University of Missouri Columbia")


DATAFILE$INST<-as.factor(DATAFILE$INST)
levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"University of Missouri Columbia",
                           "lancaster university",
                           "landcaster university",
                           "lancaster",
                           "csiro",
                           "australian commonwealth scientific and research organization",
                           "CNRS Centre dEcologie Fonctionnelle et Evolutive",
                           "Forestry and Forest Products Research Institute",
                           "University of Minnesota Duluth",
                           "University of Minnesota Crookston",
                           "University of Toronto Mississauga",
                           "State University of New York College of Environmental Science and Forestry",
                           "Calyx, Inc.","University of North Carolina Charlotte",
                           "Smithsonian National Museum of Natural History",
                           "Smithsonian National Zoological Park",
                           "Laboratoire Associe de Modelisation des Plantes",
                           "conicet consejo nacional de investigaciones cientificas y tecnicas",
                           "fao food and agriculture organization",
                           "Southern Illinois University",
                           "universite libre de bruxelles",
                           "southern illinois u",
                           "Aarhus University",
                           "franklin and marshall university",
                           "University of St. Andrews",
                           "university of st andrews",
                           "university of pittsburgh",
                           "university of uppsala",
                           "NERC Centre for Population Biology",
                           "Massey University",
                           "University of Bialystok",
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


DATAFILE$CITY[DATAFILE$CITY=="W<9f>rzburg"]<-"Wurzburg"
DATAFILE$CITY[DATAFILE$CITY=="K<9a>ln"]<-"Cologne"
DATAFILE$CITY[DATAFILE$CITY=="G<9a>ttingen"]<-"Gottingen"
DATAFILE$CITY[DATAFILE$CITY=="Z<81>rich"]<-"Zurich"
DATAFILE$CITY[DATAFILE$CITY=="M<9f>nchen"]<-"Munich"

DATAFILE$STATE[DATAFILE$STATE=="Z<81>rich"]<-"Zurich"

DATAFILE$UNIT[DATAFILE$UNIT=="Estaci<f3>n Biol<f3>gica de Do<f1>ana"]<-"Estacion Biologica de Donana"
DATAFILE$UNIT[DATAFILE$UNIT=="Departamento de Ecolog<90>a"]<-"Departamento de Ecologia"
DATAFILE$UNIT[DATAFILE$UNIT=="FB Biologie/Chemie/<80>kologie"]<-"FB Biologie/Chemiekologie"
DATAFILE$UNIT[DATAFILE$UNIT=="Institut f<99>r Biologie (II)"]<-"Institut fur Biologie (II)"





DATAFILE$INST<-as.character(DATAFILE$INST)
DATAFILE$INST<-trimws(DATAFILE$INST, which = "left")
DATAFILE$INST<-trimws(DATAFILE$INST, which = "right")
DATAFILE$INST<-gsub("  ", " ", DATAFILE$INST)
DATAFILE$INST<-gsub("- ", "-", DATAFILE$INST)
DATAFILE$INST<-gsub(" -", "-", DATAFILE$INST)
DATAFILE$INST<-gsub(" - ", "-", DATAFILE$INST)
DATAFILE$INST<-gsub(" at ", "-", DATAFILE$INST)
DATAFILE$INST<-gsub("The ", "", DATAFILE$INST)
DATAFILE$INST<-gsub("univ ", "university ", DATAFILE$INST)
DATAFILE$INST<-gsub("Univ ", "University ", DATAFILE$INST)
DATAFILE$INST<-gsub("Calif ", "California ", DATAFILE$INST)
DATAFILE$INST<-gsub("calif ", "california ", DATAFILE$INST)
DATAFILE$INST<-gsub("universtity ", "university ", DATAFILE$INST)
DATAFILE$INST<-gsub("univesity ", "university ", DATAFILE$INST)
DATAFILE$INST<-gsub("universityof", "university of ", DATAFILE$INST)
DATAFILE$INST<-gsub("ofarizona", "of arizona", DATAFILE$INST)
DATAFILE$INST<-gsub("unviersity", "university", DATAFILE$INST)
DATAFILE$INST<-gsub("unviversityof", "university of", DATAFILE$INST)
DATAFILE$INST<-gsub("univesrity", "university", DATAFILE$INST)
DATAFILE$INST<-gsub("Unniversity","University",DATAFILE$INST)
DATAFILE$INST<-gsub("Museum Natl Hist Nat","national museum of natural history france",DATAFILE$INST)
DATAFILE$INST<-gsub("University<ca>of<ca>California<ca>Santa<ca>Cruz","University of California Santa Cruz",DATAFILE$INST)
DATAFILE$INST<-gsub("Uc Santa Cruz","University of California Santa Cruz",DATAFILE$INST)
# TODO: gsub ArKansas one is not working inside the function and I have no idea why.
DATAFILE$INST<-gsub("Arkansas","Arkansas", DATAFILE$INST,ignore.case=TRUE)
DATAFILE$INST<-gsub("UVA","University of Virginia",DATAFILE$INST)
DATAFILE$INST<-gsub("US FOREST SERV","US Forest Service",DATAFILE$INST)
DATAFILE$INST<-gsub("LSU","Louisiana State University",DATAFILE$INST)
DATAFILE$INST<-gsub("double check",NA,DATAFILE$INST)
DATAFILE$INST<-gsub("N.C. State","North Carolina State University",DATAFILE$INST)
DATAFILE$INST<-gsub("UC Davis","University of California Davis",DATAFILE$INST)
DATAFILE$INST<-gsub("UGA","University of Georgia",DATAFILE$INST)
DATAFILE$INST<-gsub("UNIV CONNECTICUT","University of Connecticut",DATAFILE$INST)
DATAFILE$INST<-gsub("Univ Los Andes","Universidad de los Andes",DATAFILE$INST)
DATAFILE$INST<-gsub("Univ Nacl Autonoma Mexico","Universidad Nacional Autonoma de Mexico",DATAFILE$INST)
DATAFILE$INST<-gsub("Ume¥ University","Umea University",DATAFILE$INST)
# DATAFILE$INST<-gsub("University of ArKansas","University of Arkansas",DATAFILE$INST)
DATAFILE$INST<-gsub(" Of ", " of ", DATAFILE$INST)
DATAFILE$INST<-gsub("of California ", "of California-", DATAFILE$INST)
DATAFILE$INST<-gsub("U ", "University of ", DATAFILE$INST)
DATAFILE$INST<-gsub("California, ", "California-", DATAFILE$INST)
DATAFILE$INST<-gsub("U California ", "University of California-", DATAFILE$INST)
DATAFILE$INST<-gsub("U. of", "University of", DATAFILE$INST)
DATAFILE$INST<-gsub("Univ. of", "University of", DATAFILE$INST)
DATAFILE$INST<-gsub("Machigan", "Michigan", DATAFILE$INST)
DATAFILE$INST<-gsub("N. Prairie", "Northern Prairie", DATAFILE$INST)
DATAFILE$INST<-gsub("Pacific S.W. Research Station-US Forest Service", "USFS-Pacific Southwest Research Station", DATAFILE$INST)
DATAFILE$INST<-gsub("N.B.S. ", "USGS-", DATAFILE$INST)
DATAFILE$INST<-gsub("U.S. ", "US ", DATAFILE$INST)
DATAFILE$INST<-gsub("Illimois", "Illinois", DATAFILE$INST)
DATAFILE$INST<-gsub("Univerisity", "University", DATAFILE$INST)
DATAFILE$INST<-gsub("Univeristy", "University", DATAFILE$INST)
DATAFILE$INST<-gsub("Univerist", "University", DATAFILE$INST)
DATAFILE$INST<-gsub("Universit ", "University", DATAFILE$INST)
DATAFILE$INST<-gsub(" INPA", "", DATAFILE$INST)
DATAFILE$INST<-gsub("--", " ", DATAFILE$INST)
DATAFILE$INST<-gsub("-", " ", DATAFILE$INST)
DATAFILE$INST<-gsub(" (CSIRO)", " ", DATAFILE$INST)
DATAFILE$INST<-gsub(" CSIRO", " ", DATAFILE$INST)
DATAFILE$INST<-gsub(",CAS", " ", DATAFILE$INST)
DATAFILE$INST<-gsub("Nacaional", "Nacional", DATAFILE$INST)
DATAFILE$INST<-gsub("British Colombia", "British Columbia", DATAFILE$INST)
DATAFILE$INST<-gsub("Smithosonian", "Smithsonian", DATAFILE$INST)
DATAFILE$INST<-gsub("kansas", "Kansas", DATAFILE$INST)
DATAFILE$INST<-gsub("Sao Paolo", "Sao Paulo", DATAFILE$INST)
DATAFILE$INST<-gsub("illinois", "Illinois", DATAFILE$INST)
DATAFILE$INST<-gsub("Wuerzburg", "Wurzburg", DATAFILE$INST)
DATAFILE$INST<-gsub("LaTrobe", "La Trobe", DATAFILE$INST)
DATAFILE$INST<-gsub("No one by", "no one by", DATAFILE$INST)
DATAFILE$INST<-gsub("Botannical", "Botanical", DATAFILE$INST)
DATAFILE$INST<-gsub("Berkely", "Berkeley", DATAFILE$INST)
DATAFILE$INST<-gsub("Commonweath", "Commonwealth", DATAFILE$INST)
DATAFILE$INST<-gsub("Archibold", "Archbold", DATAFILE$INST)
DATAFILE$INST<-gsub("Loisisana", "Louisiana", DATAFILE$INST)
DATAFILE$INST<-gsub("Lousiana", "Louisiana", DATAFILE$INST)
DATAFILE$INST<-gsub("Bringham", "Brigham", DATAFILE$INST)
DATAFILE$INST<-gsub("Connetticut", "Connecticut", DATAFILE$INST)
DATAFILE$INST<-gsub("Unversity", "University", DATAFILE$INST)
DATAFILE$INST<-gsub(",", "", DATAFILE$INST)
DATAFILE$INST<-gsub("Westleyan", "Wesleyan", DATAFILE$INST)
DATAFILE$INST<-gsub("'", "", DATAFILE$INST)
DATAFILE$INST<-gsub("Veterinary&", "Veterinary and", DATAFILE$INST)
DATAFILE$INST<-gsub("&", "and", DATAFILE$INST)
DATAFILE$INST<-gsub("Virginia Tech", "Virginia Polytechnic Institute and State University", DATAFILE$INST)
DATAFILE$INST<-gsub("Austrailan", "Australian", DATAFILE$INST)
DATAFILE$INST<-gsub("Indian Institute of Sciences", "Indian Institute of Science", DATAFILE$INST)
DATAFILE$INST<-gsub("KingS", "Kings", DATAFILE$INST)
DATAFILE$INST<-gsub("Louisisana", "Louisiana", DATAFILE$INST)
DATAFILE$INST<-gsub("university queensland", "university of queensland", DATAFILE$INST)
DATAFILE$INST<-gsub("Universrity", "University", DATAFILE$INST)
DATAFILE$INST<-gsub("Canadian Forestry", "Canadian Forest", DATAFILE$INST)
DATAFILE$INST<-gsub("Mighican", "Michigan", DATAFILE$INST)
DATAFILE$INST<-gsub("de Montpellier II", "Montpellier II", DATAFILE$INST)
DATAFILE$INST<-gsub("North Arizona", "Northern Arizona", DATAFILE$INST)
DATAFILE$INST<-gsub("Wsl", "WSL", DATAFILE$INST)
DATAFILE$INST<-gsub("Alabama in", "Alabama", DATAFILE$INST)
DATAFILE$INST<-gsub("Fonctionelle", "Fonctionnelle", DATAFILE$INST)
DATAFILE$INST<-gsub("-CNRS", "", DATAFILE$INST)
# DATAFILE$INST<-gsub("University of Sherbooke", "Universite de Sherbooke", DATAFILE$INST)
DATAFILE$INST<-gsub("Smithsonian Institute", "Smithsonian Institution", DATAFILE$INST)
DATAFILE$INST<-gsub("Minnestoa", "Minnesota", DATAFILE$INST)
DATAFILE$INST<-gsub("University of Lausanne", "Universite de Lausanne", DATAFILE$INST)
DATAFILE$INST[DATAFILE$INST=="Institute of Ecosystem Studies"]<-"Cary Institute of Ecosystem Studies"
DATAFILE$INST[DATAFILE$INST=="institute of ecosystem studies"]<-"cary institute of ecosystem studies"
DATAFILE$INST<-gsub("Environment Bangalore", "Environment", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="University of Aberdeen"  ]<-"Scotland"
DATAFILE$INST[DATAFILE$INST=="Auburn"] <- "Auburn University"
DATAFILE$INST[DATAFILE$INST=="Auburn U"] <- "Auburn University"
DATAFILE$INST[DATAFILE$INST=="Michigan State"] <- "Michigan State University"
DATAFILE$INST[DATAFILE$INST=="Berkeley"] <- "University of California Berkeley"
DATAFILE$INST[DATAFILE$INST=="Duke"] <- "Duke University"
DATAFILE$INST[DATAFILE$INST=="Texas A and M"] <- "Texas A and M University"
DATAFILE$INST[DATAFILE$INST=="Purdue"] <- "Purdue University"
DATAFILE$INST[DATAFILE$INST=="Princeton"] <- "Princeton University"
DATAFILE$INST[DATAFILE$INST=="Oxford"] <- "Oxford University"
DATAFILE$INST[DATAFILE$INST=="Los Alamos"] <- "Los Alamos National Laboratory"
DATAFILE$INST[DATAFILE$INST=="Stazione Zoologica \"Anton Dohrn\""] <- "Stazione Zoologica Anton Dohrn"
DATAFILE$INST[DATAFILE$INST=="Cambridge"] <- "University of Cambridge"
DATAFILE$INST[DATAFILE$INST=="Princeton"]<-"Princeton University"
# DATAFILE$INST[DATAFILE$INST=="WSL Swiss Federal Research Institute"]<-"Swiss Federal Research Institute WSL"
DATAFILE$UNIT[DATAFILE$INST=="Pacific S.W. Research Station-US Forest Service"]<-"USFS Pacific Southwest Research Station"
# DATAFILE$INST[DATAFILE$INST=="Pacific S.W. Research Station-US Forest Service"]<-"US Forest Service"
DATAFILE$INST[DATAFILE$INST=="University fo California Berkeley"]<-"University of California Berkeley"
DATAFILE$INST[DATAFILE$INST=="US Geological survey"]<-"USGS"
DATAFILE$INST[DATAFILE$INST=="UF"]<-"University of Florida"
DATAFILE$INST[DATAFILE$INST=="Cal State Bakersfield"]<-"California State University Bakersfield"
DATAFILE$INST[DATAFILE$INST=="Universityof Wisconsin-Milwaukee"]<-"University of Wisconsin Milwaukee"
DATAFILE$INST[DATAFILE$INST=="Colorado State"]<-"Colorado State University"
DATAFILE$INST[DATAFILE$INST=="Cornell"]<-"Cornell University"
DATAFILE$INST[DATAFILE$INST=="East Carolina U"]<-"East Carolina University"
DATAFILE$INST[DATAFILE$INST=="Florida International U"]<-"Florida International University"
DATAFILE$INST[DATAFILE$INST=="FSU"]<-"Florida State University"
DATAFILE$INST[DATAFILE$INST=="Harvard"]<-"Harvard University"
DATAFILE$INST[DATAFILE$INST=="Illinois State"]<-"Illinois State University"
DATAFILE$INST[DATAFILE$INST=="Indiana U"]<-"Indiana University"
DATAFILE$INST[DATAFILE$INST=="Institute of Ecosystem"]<-"Institute of Ecosystem Studies"
DATAFILE$INST[DATAFILE$INST=="Iowa State"]<-"Iowa State University"
DATAFILE$INST[DATAFILE$INST=="John Carroll U"]<-"John Carroll University"
DATAFILE$INST[DATAFILE$INST=="Knoxville"]<-"University of Tennessee"
DATAFILE$INST[DATAFILE$INST=="Louisiana State"]<-"Louisiana State University"
DATAFILE$INST[DATAFILE$INST=="Marshall Unin"]<-"Marshall University"
DATAFILE$INST[DATAFILE$INST=="franklin and farshall"]<-"franklin and marshall university"
DATAFILE$INST[DATAFILE$INST=="university of bangor"]<-"bangor university"
DATAFILE$INST[DATAFILE$INST=="Miami U"]<-"Miami University"
DATAFILE$INST[DATAFILE$INST=="Montana State"]<-"Montana State University"
DATAFILE$INST[DATAFILE$INST=="North Dakota State"]<-"North Dakota State University"
DATAFILE$INST[DATAFILE$INST=="Ohio State"]<-"Ohio State University"
DATAFILE$INST[DATAFILE$INST=="Oregon State"]<-"Oregon State University"
DATAFILE$INST[DATAFILE$INST=="Rensselaer Poly"]<-"Rensselaer Polytechnic Institute"
DATAFILE$INST[DATAFILE$INST=="Rutgers U"]<-"Rutgers University"
DATAFILE$INST[DATAFILE$INST=="Stanford"]<-"Stanford University"
DATAFILE$INST[DATAFILE$INST=="State University of New York at Binghamton"]<-"SUNY Binghamton"
DATAFILE$INST[DATAFILE$INST=="Binghamton University-SUNY"]<-"SUNY Binghamton"
DATAFILE$INST[DATAFILE$INST=="Texas State"]<-"Texas State University"
DATAFILE$INST[DATAFILE$INST=="The University of Southwestern Louisiana"]<-"University of Southwestern Louisiana"
DATAFILE$INST[DATAFILE$INST=="U Arizona"]<-"University of Arizona"
DATAFILE$INST[DATAFILE$INST=="University of California Irvine"]<-"University of California Irvine"
DATAFILE$INST[DATAFILE$INST=="University of California Riverside"]<-"University of California Riverside"
DATAFILE$INST[DATAFILE$INST=="California State University of San Marcos"]<-"California State University San Marcos"
DATAFILE$INST[DATAFILE$INST=="University of California at San Diego"]<-"University of California San Diego"
DATAFILE$INST[DATAFILE$INST=="University of California Santa Cruz"]<-"University of California Santa Cruz"
DATAFILE$INST[DATAFILE$INST=="UC Santa Cruz"]<-"University of California Santa Cruz"
DATAFILE$INST[DATAFILE$INST=="Uc Merced, University of California"]<-"University of California Merced"
DATAFILE$INST[DATAFILE$INST=="UMSL"]<-"University of Missouri St Louis"
DATAFILE$INST[DATAFILE$INST=="University of Carolina Aiken"]<-"University of South Carolina Aiken"
DATAFILE$INST[DATAFILE$INST=="University of Massachusetts Boston"]<-"University of Massachusetts Boston"
# DATAFILE$INST[DATAFILE$INST=="University of Nevada Las Vegas"]<-"University of Nevada-Las Vegas"
# DATAFILE$INST[DATAFILE$INST=="University of Texas Austin"]<-"University of Texas Austin"
DATAFILE$INST[DATAFILE$INST=="Vanderbilt"]<-"Vanderbilt University"
DATAFILE$INST[DATAFILE$INST=="Virginia Commonweath U"]<-"Virginia Commonweath University"
DATAFILE$INST[DATAFILE$INST=="Washington State"]<-"Washington State University"
DATAFILE$INST[DATAFILE$INST=="Washington U"]<-"Washington University"
DATAFILE$INST[DATAFILE$INST=="Willamette U"]<-"Willamette University"
DATAFILE$INST[DATAFILE$INST=="University of Pennsylvania, "]<-"University of Pennsylvania"
DATAFILE$INST[DATAFILE$INST=="University of Pittsburg"]<-"University of Pittsburgh"
DATAFILE$INST[DATAFILE$INST=="University of Tennesee"]<-"University of Tennessee"
DATAFILE$INST[DATAFILE$INST=="University of Tennese"]<-"University of Tennessee"
DATAFILE$INST[DATAFILE$INST=="Acad Natural Science Philadelphia"]<-"Academy of Natural Science Philadelphia"
DATAFILE$INST[DATAFILE$INST=="Am Mus Nat Hist"]<-"American Museum of Natural History"
DATAFILE$INST[DATAFILE$INST=="ColoradoState Univ"]<-"Colorado State University"
DATAFILE$INST[DATAFILE$INST=="Evergreen State College Washington"]<-"Evergreen State College"
DATAFILE$INST[DATAFILE$INST=="Filed Museum of Natural History"]<-"Field Museum Of Natural History"
DATAFILE$INST[DATAFILE$INST=="George Mason University Virginia"]<-"George Mason University"
DATAFILE$INST[DATAFILE$INST=="Insitute of Ecosystem Studies"]<-"Cary Institute of Ecosystem Studies"
DATAFILE$INST[DATAFILE$INST=="Kenyon College Ohio"]<-"Kenyon College"
DATAFILE$INST[DATAFILE$INST=="Louisiana State Univeristy"]<-"Louisiana State University"
DATAFILE$INST[DATAFILE$INST=="Milwaukee Public Mus"]<-"Milwaukee Public Museum"
DATAFILE$INST[DATAFILE$INST=="Missouri Botanical Gardens"]<-"Missouri Botanical Garden"
# DATAFILE$INST[DATAFILE$INST=="Oklahoma Museum of Natural History"]<-"Oklahoma Mus Nat His"
DATAFILE$INST[DATAFILE$INST=="Smithsonian(STRI)"]<-"Smithsonian Tropical Research Institute"
DATAFILE$INST[DATAFILE$INST=="STRI Smithsonian"]<-"Smithsonian Tropical Research Institute"
DATAFILE$INST[DATAFILE$INST=="U Connecticut"]<-"University of Connecticut"
DATAFILE$INST[DATAFILE$INST=="University of Alabama Huntsville"]<-"University of Alabama Huntsville"
DATAFILE$INST[DATAFILE$INST=="University of Connetticut Storrs"]<-"University of Connecticut"
DATAFILE$INST[DATAFILE$INST=="University of Michgan"]<-"University of Michigan"
DATAFILE$INST[DATAFILE$INST=="University of Missouri St Louis"]<-"University of Missouri St Louis"
DATAFILE$INST[DATAFILE$INST=="University of Missouri_St Louis"]<-"University of Missouri St Louis"
DATAFILE$INST[DATAFILE$INST=="University of Wisconsin Milwaukee"]<-"University of Wisconsin Milwaukee"
DATAFILE$INST[DATAFILE$INST=="U California Irvine"]<-"University of California Irvine"
DATAFILE$INST[DATAFILE$INST=="U California Riverside"]<-"University of California Riverside"
DATAFILE$INST[DATAFILE$INST=="University of California at San Diego"]<-"University of California San Diego"
DATAFILE$INST[DATAFILE$INST=="U California Santa Barbara"]<-"University of California Santa Barbara"
DATAFILE$INST[DATAFILE$INST=="U California Santa Cruz"]<-"University of California Santa Cruz"
DATAFILE$INST[DATAFILE$INST=="University of California Los Angeles"]<-"University of California Los Angeles"
DATAFILE$INST[DATAFILE$INST=="University of California Berkeley"]<-"University of California Berkeley"
DATAFILE$INST[DATAFILE$INST=="University of California Davis"]<-"University of California Davis"
DATAFILE$INST[DATAFILE$INST=="University of California Santa Barbara"]<-"University of California Santa Barbara"
DATAFILE$INST[DATAFILE$INST=="University of California San Diego"]<-"University of California San Diego"
DATAFILE$INST[DATAFILE$INST=="University of California San Diego"]<-"University of California San Diego"
DATAFILE$INST[DATAFILE$INST=="Rensselaer Poly"]<-"Rensselaer Polytechnic Institute"
DATAFILE$INST[DATAFILE$INST=="Oklahoma Mus Nat His"]<-"Oklahoma Museum of Natural History"
DATAFILE$INST[DATAFILE$INST=="US Forest Service Pacific Southwest Research Station"]<-"USFS Pacific Southwest Research Station"
DATAFILE$INST[DATAFILE$INST=="U.S.F.W.S. National Wetlands Research Center"]<-"USFWS National Wetlands Research Center"
DATAFILE$INST[DATAFILE$INST=="Arizona State University West"]<-"Arizona State University West Campus"
DATAFILE$INST[DATAFILE$INST=="Naval Research Laboratory DC"]<-"USONR Naval Research Laboratory"
DATAFILE$INST[DATAFILE$INST=="Temple-Inland Forest"]<-"Temple-Inland Forest Products"
DATAFILE$INST[DATAFILE$INST=="USDA Forest Service"]<-"USFS"
DATAFILE$INST[DATAFILE$INST=="USDA Forest Service Rocky Mountain Research Station"]<-"USFS Rocky Mountain Research Station"
DATAFILE$INST[DATAFILE$INST=="university of newcastle upon tyne"]<-"newcastle university"
DATAFILE$INST[DATAFILE$INST=="university of newcastle upon tuyne"]<-"newcastle university"
DATAFILE$INST[DATAFILE$INST=="USDA Forest Service Southern Research Station"]<-"USFS Southern Research Station"
DATAFILE$INST[DATAFILE$INST=="USFS-Pacific Southwest Research Station"]<-"USFS Pacific Southwest Research Station"
DATAFILE$INST[DATAFILE$INST=="USFWS-National Wetlands Research Center"]<-"USFS National Wetlands Research Center"
# DATAFILE$UNIT[DATAFILE$INST=="USGS Forest and Rangeland Ecosystem Science Center"]<-"Forest and Rangeland Ecosystem Science Center"
# DATAFILE$INST[DATAFILE$INST=="USGS Forest and Rangeland Ecosystem Science Center"]<-"US Geological Survey"
# DATAFILE$UNIT[DATAFILE$INST=="USGS-Northern Prairie Wildlife Research Center"]<-"Northern Prairie Wildlife Research Center"
DATAFILE$INST[DATAFILE$INST=="USGS-Northern Prairie Wildlife Research Center"]<-"USGS Northern Prairie Wildlife Research Center"
DATAFILE$INST[DATAFILE$INST=="National Museum of Natural History"]<-"Smithsonian National Museum of Natural History"
DATAFILE$INST[DATAFILE$INST=="Smithsonian"]<-"Smithsonian National Museum of Natural History" #EB Verified
DATAFILE$INST[DATAFILE$INST=="USDA"]<-"USDA US Department of Agriculture"
# DATAFILE$UNIT[DATAFILE$INST=="USDA Cooperative State Research, Education and Extension Service (CSREES)"]<-"USDA Cooperative State Research, Education, and Extension Service"
DATAFILE$INST[DATAFILE$INST=="USDA Cooperative State Research, Education and Extension Service (CSREES)"]<-"USDA Cooperative State Research, Education, and Extension Service"
DATAFILE$INST[DATAFILE$INST=="Virginia Tech"]<-"Virginia Polytechnic Institute and State University"
DATAFILE$INST[DATAFILE$INST=="University of Pennsylvania,"]<-"University of Pennsylvania"
DATAFILE$INST[DATAFILE$INST=="Division of Reptiles and Amphibians Smithsonian Institute Washington DC"]<-"Smithsonian National Museum of Natural History"
DATAFILE$INST[DATAFILE$INST=="University of California-At Los Angeles"]<-"University of California Los Angeles"
DATAFILE$INST[DATAFILE$INST=="National Museum of Natural History, Smithsonian Institution"]<-"Smithsonian National Museum of Natural History"
DATAFILE$UNIT[DATAFILE$INST=="Citrus Research and Education Center"]<-"Citrus Research and Education Center"
DATAFILE$INST[DATAFILE$INST=="Citrus Research and Education Center"]<-"University of Florida"
DATAFILE$INST[DATAFILE$INST=="Ohio u"]<-"Ohio University"
DATAFILE$INST[DATAFILE$INST=="University of South Carolina" & DATAFILE$CITY=="Columbia"]<-"University of South Carolina Columbia"
# DATAFILE$UNIT[DATAFILE$INST=="US Department of Agriculture Cooperative State Research, Education, and Extension Service"]<-"Cooperative State Research, Education, and Extension Service"
DATAFILE$INST[DATAFILE$INST=="US Department of Agriculture Cooperative State Research, Education, and Extension Service"]<-"USDA Cooperative State Research, Education, and Extension Service"
DATAFILE$INST[DATAFILE$LAST_NAME=="Bowers" & DATAFILE$CITY=="Vadnais Heights"]<-"Calyx, Inc."
DATAFILE$UNIT[DATAFILE$INST=="Arnold Arboretum Of Harvard University"]<-"Arnold Arboretum"
DATAFILE$INST[DATAFILE$INST=="Arnold Arboretum Of Harvard University"]<-"Harvard University"
DATAFILE$UNIT[DATAFILE$INST=="Museum of Comparative Zoology"]<-"Museum of Comparative Zoology"
DATAFILE$INST[DATAFILE$INST=="Museum of Comparative Zoology"]<-"Harvard University"
DATAFILE$INST[DATAFILE$INST=="SUNY"]<-"SUNY State University of New York"
DATAFILE$INST[DATAFILE$INST=="State University of New York at Stony Brook"]<-"SUNY Stony Brook"
DATAFILE$INST[DATAFILE$INST=="State University of New York Syracuse"]<-"SUNY Syracuse"
# DATAFILE$INST[DATAFILE$INST=="Arnold Arboretum of Harvard University"]<-"Harvard University Arnold Arboretum"
DATAFILE$UNIT[DATAFILE$INST=="Harvard Medical School"]<-"Medical School"
DATAFILE$INST[DATAFILE$INST=="Harvard Medical School"]<-"Harvard University"
DATAFILE$INST[DATAFILE$INST=="Australian Research Center for Urban "]<-"Smithsonian Tropical Research Institute"
DATAFILE$INST<-gsub(" CATIE","", DATAFILE$INST)
DATAFILE$INST<-gsub("CATIE ","", DATAFILE$INST)
DATAFILE$INST<-gsub(" (UNESP)","", DATAFILE$INST)
DATAFILE$INST[DATAFILE$INST=="Universidade Estadual Paulista (UNESP)"]<-"Universidade Estadual Paulista"
DATAFILE$INST<-gsub("Darwin University Darwin","Darwin University", DATAFILE$INST)
DATAFILE$INST<-gsub("Technion-","", DATAFILE$INST)
DATAFILE$INST<-gsub("university california","university of california", DATAFILE$INST)
DATAFILE$INST<-gsub("Israel Institute of Technology","Technion Israel Institute of Technology", DATAFILE$INST)
DATAFILE$INST<-gsub("Ossietzky","", DATAFILE$INST)
DATAFILE$INST<-gsub(",USDA","", DATAFILE$INST)
DATAFILE$INST<-gsub("UFZ Centre","Helmholtz Centre", DATAFILE$INST)
DATAFILE$INST<-gsub(" UFZ","", DATAFILE$INST)
DATAFILE$INST<-gsub("Helmholtz Centre for Environmental Research","Helmholtz Centre for Environmental Research UFZ", DATAFILE$INST)
DATAFILE$INST<-gsub("A& M Univ","A & M University", DATAFILE$INST)
DATAFILE$INST<-gsub("AandM","A & M", DATAFILE$INST)
DATAFILE$INST<-gsub("in Cornwall","Cornwall", DATAFILE$INST)
DATAFILE$INST<-gsub("SUNY","State University of New York", DATAFILE$INST)
DATAFILE$INST<-gsub("USGS","US Geological Survey", DATAFILE$INST)
DATAFILE$INST<-gsub("University of Mexico","Universidad Nacional Autonoma de Mexico", DATAFILE$INST)
DATAFILE$INST<-gsub("NC Agricultural","North Carolina Agricultural", DATAFILE$INST)
DATAFILE$INST<-gsub("for Forest, Snow, and Landscape Research","WSL", DATAFILE$INST)
DATAFILE$INST<-gsub(" Company","", DATAFILE$INST)
DATAFILE$INST<-gsub("Csiro","CSIRO", DATAFILE$INST)
DATAFILE$INST<-gsub("State University University","State University", DATAFILE$INST)
DATAFILE$INST<-gsub("North Carolina AandT State University","North Carolina A & T State University", DATAFILE$INST)
DATAFILE$INST<-gsub(" Manaaki Whenua","", DATAFILE$INST)
DATAFILE$INST<-gsub(" Headquarters","", DATAFILE$INST)
DATAFILE$INST<-gsub("VUniversity","University", DATAFILE$INST)
DATAFILE$INST<-gsub(" (IMEDEA)","", DATAFILE$INST)
DATAFILE$INST<-gsub(" (UNICAMP)","", DATAFILE$INST)
DATAFILE$INST<-gsub("dHistoire","dHistoire Naturelle", DATAFILE$INST)
DATAFILE$INST<-gsub("Naturelle Naturelle","Naturelle", DATAFILE$INST)
DATAFILE$INST<-gsub("Museum National dHistoire Naturelle", "national museum of natural history france", DATAFILE$INST)
DATAFILE$INST<-gsub("Oregan", "Oregon", DATAFILE$INST)
DATAFILE$INST<-gsub("Philips University","Philips University Marburg", DATAFILE$INST)
# DATAFILE$INST<-gsub("(CSIC)", "", DATAFILE$INST)
DATAFILE$INST[DATAFILE$INST=="Cary Cary Institute of Ecosystem Studies"]<-"Cary Institute of Ecosystem Studies"
DATAFILE$INST[DATAFILE$INST=="University of California NCEAS"]<-"National Center for Ecological Analysis and Synthesis"
DATAFILE$INST<-gsub("Khorasan Agricultural and Natural Resources Res Ctr","Khorasan Agricultural and Natural Resources Research Center", DATAFILE$INST)
DATAFILE$INST[DATAFILE$INST=="Texas Technical University"]<-"Texas Tech University" 
DATAFILE$INST[DATAFILE$INST=="Mississippi State Univ"]<-"Mississippi State University" 
DATAFILE$INST[DATAFILE$INST=="Empresa Brasileira de Pesquisa Agropecuária"]<-"Empresa Brasileira de Pesquisa Agropecuaria" 
DATAFILE$INST[DATAFILE$INST=="Technion Technion Israel Institute of Technology"]<-"Technion Israel Institute of Technology"
DATAFILE$INST[DATAFILE$INST=="Universidad Catolica de Chile"]<-"Pontifica Universidad Catolica de Chile"
DATAFILE$INST[DATAFILE$INST=="University of Marburg"]<-"Philipps University Marburg"
DATAFILE$INST[DATAFILE$INST=="Philipps University"]<-"Philipps University Marburg"
DATAFILE$INST[DATAFILE$INST=="Museum dHistoire Naturelle"]<-"National Museum of Natural History France"
DATAFILE$INST[DATAFILE$INST=="National Autonomous university of Mexico"]<-"Universidad Nacional Autonoma de Mexico"
DATAFILE$INST[DATAFILE$INST=="Landcare Research"]<-"Manaaki Whenua Landcare Research"
DATAFILE$UNIT[DATAFILE$INST=="USGS/NRII"]<-"NRII" 
DATAFILE$INST[DATAFILE$INST=="USGS/NRII"]<-"usgs national resources inventory"
DATAFILE$UNIT[DATAFILE$INST=="Florida International University and Center for Tropical Plant Conservation"]<-"Center for Tropical Plant Conservation" 
DATAFILE$INST[DATAFILE$INST=="Florida International University and Center for Tropical Plant Conservation"]<-"Florida International University" 
DATAFILE$INST[DATAFILE$INST=="Ecole Normale Supérieure"]<-"Ecole Normale Superieure" 
DATAFILE$INST[DATAFILE$INST=="Université de Sherbrooke"]<-"University of Sherbrooke"
DATAFILE$INST[DATAFILE$INST=="Pontificia Universidad Católica de Chile"]<-"Pontificia Universidad Catolica de Chile" 
DATAFILE$INST[DATAFILE$INST=="University of Tromsø"]<-"University of Tromso" 
DATAFILE$INST[DATAFILE$INST=="Universit\xfc\xbe\x8c\xa3\xa0\xbc Montpellier II"]<-"Universite Montpellier II" 
DATAFILE$INST[DATAFILE$INST=="Universit\xfc\xbe\x8d\x83\xa0\xbct Z\xfc\xbe\x8c\x93\xa0\xbcrich Irchel"]<-"University of Zurich Irchel" 
DATAFILE$INST[DATAFILE$INST=="Texas A & M Univ."]<-"Texas A & M University"
DATAFILE$INST[DATAFILE$INST=="Texas A & M"]<-"Texas A & M University"
DATAFILE$INST[DATAFILE$INST=="Aberdeen"]<-"University of Aberdeen"
DATAFILE$INST[DATAFILE$INST=="Cambridge University"]<-"University of Cambridge"
DATAFILE$INST[DATAFILE$INST=="Universityof"]<-"University of"
DATAFILE$INST[DATAFILE$INST=="Aberystwyth"]<-"Aberystwyth University"
DATAFILE$INST[DATAFILE$INST=="Alabama A and M"]<-"Alabama A and M University"
DATAFILE$INST[DATAFILE$INST=="Alterra"]<-"Wageningen University and Research"
DATAFILE$INST[DATAFILE$INST=="Waginen University"]<-"Wageningen University and Research"
DATAFILE$INST[DATAFILE$INST=="Wageningen"]<-"Wageningen University and Research"
DATAFILE$INST[DATAFILE$INST=="Wageningen University"]<-"Wageningen University and Research"
DATAFILE$NOTES[DATAFILE$INST=="usgs and university of miami"]<-"secondary inst: university of miami"
DATAFILE$INST[DATAFILE$INST=="usgs and university of miami"]<-"usgs"
DATAFILE$NOTES[DATAFILE$INST=="usgs and university of california santa barbara"]<-"secondary inst: university of california santa barbara"
DATAFILE$INST[DATAFILE$INST=="usgs and university of california santa barbara"]<-"usgs"
DATAFILE$INST[DATAFILE$INST=="ALTERRA Research Institute for the Green World"]<-"Wageningen University and Research"
DATAFILE$INST[DATAFILE$INST=="Assicuates"]<-"Associates"
DATAFILE$INST[DATAFILE$INST=="Bangor"]<-"Bangor University"
DATAFILE$INST[DATAFILE$INST=="British Anarctic Society"]<-"British Antarctic Survey"
DATAFILE$INST[DATAFILE$INST=="British Antartic Survey"]<-"British Antarctic Survey"
DATAFILE$INST[DATAFILE$INST=="Centro de Investigaciones y Experiencias Forestales (CIEF)"]<-"Centro de Investigaciones y Experiencias Forestales"
DATAFILE$INST[DATAFILE$INST=="Chinese Academy of Sciencies"]<-"Chinese Academy of Sciences"
DATAFILE$INST[DATAFILE$INST=="Christian Albrechts Universitat zu Kiel"]<-"Christian Albrechts Universitat Kiel"
DATAFILE$INST[DATAFILE$INST=="Consejo Superior de Investigaciones Cientificas (CSIC)"]<-"CSIC"
DATAFILE$INST[DATAFILE$INST=="CSIC UPV"]<-"INGENIO"
DATAFILE$INST[DATAFILE$INST=="Durham"]<-"Durham University"
DATAFILE$INST[DATAFILE$INST=="EAWAG/ETH"]<-"Swiss Federal Institute of Aquatic Science and Technology"
DATAFILE$INST[DATAFILE$INST=="Edinburgh"]<-"University of Edinburgh"
DATAFILE$INST[DATAFILE$INST=="Freie Universitt Berlin"]<-"Free University of Berlin"
DATAFILE$INST[DATAFILE$INST=="Freie University of Berlin"]<-"Free University of Berlin"
DATAFILE$INST[DATAFILE$INST=="G√∂ttingen"]<-"University of Gottingen"
DATAFILE$INST[DATAFILE$INST=="Gatty Marine Lab (University of Saint Andrews"]<-"University of St Andrews"
DATAFILE$INST[DATAFILE$INST=="Georg August Universitat Gottingen"]<-"University of Gottingen"
DATAFILE$INST[DATAFILE$INST=="Glasgow"]<-"University of Glasgow"
DATAFILE$INST[DATAFILE$INST=="Gottingen University"]<-"University of Gottingen"
DATAFILE$INST[DATAFILE$INST=="Helsinki"]<-"University of Helsinki"
DATAFILE$INST[DATAFILE$INST=="Humboldt Universitat zu Berlin"]<-"Humboldt University of Berlin"
DATAFILE$INST[DATAFILE$INST=="Humboldt University"]<-"Humboldt University of Berlin"
DATAFILE$INST[DATAFILE$INST=="Institut Mediterrani dEstudis Avencats (CSIC UIB)"]<-"CSIC Institut Mediterrani dEstudis Avencats"
DATAFILE$INST[DATAFILE$INST=="Instituto de Ciencias del Mar CSIC"]<-"CSIC Instituto de Ciencias del Mar"
DATAFILE$INST[DATAFILE$INST=="IZW"]<-"Leibniz Institute for Zoo and Wildlife Research"
DATAFILE$INST[DATAFILE$INST=="Kansas State"]<-"Kansas State University"
DATAFILE$INST[DATAFILE$INST=="Karlsruhe"]<-"Karlsruhe Institute of Technology"
DATAFILE$INST[DATAFILE$INST=="Khorasan Agricultural and Natural Resources Res. Ctr"]<-"Khorasan Agricultural and Natural Resources Research Center"
DATAFILE$INST[DATAFILE$INST=="Kyushu Unniversity"]<-"Kyushu University"
DATAFILE$INST[DATAFILE$INST=="Lehman College CUNY"]<-"CUNY Lehman College"
DATAFILE$INST[DATAFILE$INST=="Los Alamos National Lab"]<-"Los Alamos National Laboratory"
DATAFILE$INST[DATAFILE$INST=="Ludwig Maximilians Universitat Munchen"]<-"Ludwig Maximilian Universitat Munchen"
DATAFILE$INST[DATAFILE$INST=="Lyme Regis"]<-""
DATAFILE$INST[DATAFILE$INST=="Macaulay¬†Institute"]<-"Macaulay Land Use Research Institute"
DATAFILE$INST[DATAFILE$INST=="Max Planck Inst Plant Breeding Res"]<-"Max Planck Institute for Plant Breeding Research"
DATAFILE$INST[DATAFILE$INST=="Max Planck Institut fur Verhaltensphysiologie"]<-"Max Planck Institute for Behavioral Physiology"
DATAFILE$INST[DATAFILE$INST=="Max Planck Institute Jena"]<-"Max Planck Institute for the Science of Human History"
DATAFILE$INST[DATAFILE$INST=="Monterey Bay Aquarium Research Institute (MBARI)"]<-"Monterey Bay Aquarium Research Institute"
DATAFILE$INST[DATAFILE$INST=="Mt Holyoke College"]<-"Mount Holyoke College"
DATAFILE$INST[DATAFILE$INST=="Museum National dHistoire Naturelle"]<-"national museum of natural history france"
DATAFILE$INST[DATAFILE$INST=="National Institute of Water And Atmospheric Research"]<-"National Institute of Water and Atmospheric Research"
DATAFILE$INST[DATAFILE$INST=="NC State University"]<-"North Carolina State University"
DATAFILE$INST[DATAFILE$INST=="NERI"]<-""
DATAFILE$INST[DATAFILE$INST=="Netherlands Institute of Ecology; Wageningen University and Research Centre Netherlands Institute of Ecology"]<-"Wageningen University and Research"
DATAFILE$INST[DATAFILE$INST=="Neuchatel"]<-"University of Neuchatel"
DATAFILE$INST[DATAFILE$INST=="New College of the University of South Florida"]<-"New College of Florida"
DATAFILE$INST[DATAFILE$INST=="NIOZ and Groningen"]<-"Netherlands Institute for Sea Research"
DATAFILE$INST[DATAFILE$INST=="Norwich"]<-"John Innes Centre"
DATAFILE$INST[DATAFILE$INST=="Oklahoma State"]<-"Oklahoma State University"
DATAFILE$INST[DATAFILE$INST=="Oregan State University"]<-"Oregon State University"
DATAFILE$INST[DATAFILE$INST=="ORNL"]<-"Oak Ridge National Laboratory"
DATAFILE$INST[DATAFILE$INST=="Plymouth"]<-"Marine Biological Association"
DATAFILE$INST[DATAFILE$INST=="Plymouth Marine Lab"]<-"Plymouth Marine Laboratory"
DATAFILE$INST[DATAFILE$INST=="Polish Academy of Sciences"]<-"Polish Academy of Science"
DATAFILE$INST[DATAFILE$INST=="Research Triangle Park"]<-"BASF"
DATAFILE$INST[DATAFILE$INST=="Royal Botanical Gardens"]<-"Royal Botanical Gardens Kew"
DATAFILE$INST[DATAFILE$INST=="RWTH Aachen"]<-"RWTH Aachen University"
DATAFILE$UNIT[DATAFILE$INST=="Scripps Institute of Oceanography"]<-"Scripps Institution of Oceanography"
DATAFILE$INST[DATAFILE$INST=="Scripps Institute of Oceanography"]<-"University of California Santa Diego"
DATAFILE$UNIT[DATAFILE$INST=="Scripps Institution  of Oceanography"]<-"Scripps Institution of Oceanography"
DATAFILE$INST[DATAFILE$INST=="Scripps Institution  of Oceanography"]<-"University of California Santa Diego"
DATAFILE$INST[DATAFILE$INST=="Sheffield"]<-"University of Sheffield"
DATAFILE$INST[DATAFILE$INST=="SMITHSONIAN TROP RES INST"]<-"Smithsonian Tropical Research Institute"
DATAFILE$INST[DATAFILE$INST=="St. Andrews"]<-"University of St Andrews"
DATAFILE$INST[DATAFILE$INST=="State University of New York"]<-"SUNY State University of New York"
DATAFILE$INST[DATAFILE$INST=="Suny Albany"]<-"SUNY Albany"
DATAFILE$INST[DATAFILE$INST=="York" & (DATAFILE$COUNTRY=="UK" | DATAFILE$COUNTRY=="United Kingdom" | DATAFILE$COUNTRY=="ENGLAND")]<-"University of York"
DATAFILE$INST[DATAFILE$INST=="York" & DATAFILE$COUNTRY=="Canada"]<-"York University"
DATAFILE$INST[DATAFILE$INST=="York U"]<-"York University"
DATAFILE$INST[DATAFILE$INST=="Suny Stonybrook"]<-"SUNY Stony Brook"
DATAFILE$INST[DATAFILE$INST=="Texas A & M University"]<-"Texas A and M University"
DATAFILE$INST[DATAFILE$INST=="Texas Tech"]<-"Texas Tech University"
DATAFILE$INST[DATAFILE$INST=="U.S.F.W.S. National Ecology Research Center"]<-"USFWS National Ecology Research Center"
DATAFILE$INST[DATAFILE$INST=="U.S.F.W.S. Northern Prairie Wildlife Research Center"]<-"USGS Northern Prairie Wildlife Research Center"
DATAFILE$INST[DATAFILE$INST=="UNAM"]<-"Universidad Nacional Autonoma de Mexico"
DATAFILE$INST[DATAFILE$INST=="University Buffalo"]<-"University of Buffalo"
DATAFILE$INST[DATAFILE$INST=="University College of ABERYSTWYTH"]<-"Aberystwyth University"
DATAFILE$INST[DATAFILE$INST=="University of ArKansas Little Rock"]<-"University of Arkansas Little Rock"
DATAFILE$INST[DATAFILE$INST=="University of Colorado"]<-"University of Colorado Boulder"
DATAFILE$INST[DATAFILE$INST=="University of Durham"]<-"Durham University"
DATAFILE$INST[DATAFILE$INST=="University of Nebranska"]<-"University of Nebraska"
DATAFILE$INST[DATAFILE$INST=="University of Saint Andrews"]<-"University of St Andrews"
DATAFILE$INST[DATAFILE$INST=="University of Southern Florida"]<-"University of South Florida"
DATAFILE$INST[DATAFILE$INST=="University of Texas at Austin"]<-"University of Texas Austin"
DATAFILE$INST[DATAFILE$INST=="University Sheffield"]<-"University of Sheffield"
DATAFILE$INST[DATAFILE$INST=="University<ca>of<ca>California<ca>Santa<ca>Cruz"]<-"University of California Santa Cruz"
DATAFILE$INST[DATAFILE$INST=="Washington University"]<-"Washington University in St Louis"
DATAFILE$INST[DATAFILE$INST=="Washington University of St Louis"]<-"Washington University in St Louis"
DATAFILE$INST[DATAFILE$INST=="Western Michigan University"]<-"Western Michigan University"
DATAFILE$INST[DATAFILE$INST=="Wisconsin"]<-"University of Wisconsin"
DATAFILE$INST[DATAFILE$INST=="Woods Hole Oceanographic Institute"]<-"Woods Hole Oceanographic Institution"
DATAFILE$INST[DATAFILE$INST=="Woods Hole Research Center"]<-"Woods Hole Oceanographic Institution"
DATAFILE$NOTES[DATAFILE$INST=="university of natural resources and applied life sciences boku"]<-"institution aka BOKU"
DATAFILE$INST[DATAFILE$INST=="university of natural resources and applied life sciences boku"]<-"university of natural resources and applied life sciences vienna"

DATAFILE$INST[DATAFILE$INST=="USU"]<-"Utah State University"
DATAFILE$INST[DATAFILE$LAST_NAME=="Luque"]<-"Institut national de recherche en sciences et technologies pour lenvironnement et lagriculture"
DATAFILE$INST[DATAFILE$INST=="<a0>Forestry and Forest Products Research Institute"]<-"Forestry and Forest Products Research Institute"
DATAFILE$INST[DATAFILE$INST=="<a0>Universit<e9> Claude Bernard Lyon 1"]<-"Universite Claude Bernard Lyon 1" 
DATAFILE$INST<-gsub("Landscape<a0>Ecology","Landscape Ecology", DATAFILE$INST)
DATAFILE$INST<-gsub("Universit<8a>t","Universitat", DATAFILE$INST)
DATAFILE$INST<-gsub("Institue",  "Institute", DATAFILE$INST)
DATAFILE$INST<-gsub("Univerity",  "University", DATAFILE$INST)
DATAFILE$INST[DATAFILE$INST=="Canadia Forest Service"]<-"Canadian Forest Service"
DATAFILE$INST[DATAFILE$INST=="lowa State University"]<-"Iowa State University" 
DATAFILE$INST[DATAFILE$INST=="Iowa State U"]<-"Iowa State University"
DATAFILE$INST[DATAFILE$INST=="Iowa State"]<-"Iowa State University" 
DATAFILE$INST[DATAFILE$INST=="University of Autonoma del Estado de HIdalgo"]<-"University of Autonoma del Estado de Hidalgo" 
DATAFILE$INST[DATAFILE$INST=="Estern Illinois  University"]<-"Eastern Illinois University"
DATAFILE$INST[DATAFILE$INST=="Syngenta Crop Protection"]<-"Syngenta Crop Protection Inc"
DATAFILE$INST[DATAFILE$INST=="Alterra"]<-"ALTERRA Research Institute for the Green World"
DATAFILE$INST[DATAFILE$INST=="ALTERRA Research Instituut voor de groene ruimte"]<-"ALTERRA Research Institute for the Green World"
DATAFILE$INST[DATAFILE$INST=="Arizona State University west"]<-"Arizona State University West Campus"
DATAFILE$INST[DATAFILE$INST=="Commonwealth Scientific and Industrial Research Organisation "]<-"Commonwealth Scientific and Industrial Research Organisation"
DATAFILE$INST[DATAFILE$INST=="Dartmouth University"]<-"Dartmouth College"
DATAFILE$INST[DATAFILE$INST=="Field Museum Of Natural History"]<-"Field Museum of Natural History"
DATAFILE$INST[DATAFILE$INST=="Freie UniversitÔøΩt Berlin"]<-"Freie University of Berlin"
DATAFILE$INST[DATAFILE$INST=="Jawaharial Nehru University"]<-"Jawaharlal Nehru University"
DATAFILE$INST[DATAFILE$INST=="Kansas State University "]<-"Kansas State University"
DATAFILE$INST[DATAFILE$INST=="Landcare Research  Lincoln"]<-"Landcare Research Ltd"
DATAFILE$INST[DATAFILE$INST=="Lehman College"]<-"CUNY Lehman College"
DATAFILE$INST[DATAFILE$INST=="Mississippi State Univ."]<-"Mississippi State University"
DATAFILE$INST[DATAFILE$INST=="Monks Wood"]<-"Monks Wood Experiment Station"
DATAFILE$INST[DATAFILE$INST=="Oklahoma State"]<-"Oklahoma State University"
DATAFILE$INST[DATAFILE$INST=="Syngenta Crop Protection Inc"]<-"Syngenta Crop Protection Inc"
DATAFILE$INST[DATAFILE$INST=="Texas Tech"]<-"Texas Tech University"
DATAFILE$INST[DATAFILE$INST=="Universidad National Autonoma de Mexico"]<-"Universidad Nacional Autonoma de Mexico"
DATAFILE$INST[DATAFILE$INST=="Universidade Federal De Alagoas"]<-"Universidade Federal de Alagoas"
DATAFILE$INST[DATAFILE$INST=="Universit√© de Montreal"]<-"Universite de Montreal"
DATAFILE$INST[DATAFILE$INST=="Universit√© Joseph Fourier"]<-"Universite Joseph Fourier"
DATAFILE$INST[DATAFILE$INST=="Universite Pierre and Marie Curie"]<-"Universite Pierre Et Marie Curie"
DATAFILE$INST[DATAFILE$INST=="Uc Santa Cruz"]<-"University of California Santa Cruz"
DATAFILE$INST[DATAFILE$INST=="University of Edinburhg"]<-"University of Edinburgh"
DATAFILE$INST[DATAFILE$INST=="University of Leuvenn"]<-"University of Leuven"
DATAFILE$INST[DATAFILE$INST=="University of Los Andes Bogota"]<-"Universidad de Los Andes"
DATAFILE$INST[DATAFILE$INST=="University of Missisippi"]<-"University of Mississippi"
DATAFILE$INST[DATAFILE$INST=="University of Montanna"]<-"University of Montana"
DATAFILE$INST[DATAFILE$INST=="Unsw"]<-"University of New South Wales"
DATAFILE$INST[DATAFILE$INST=="Mammal Research Institute (University of Pretoria)"]<-"University of Pretoria"
DATAFILE$INST[DATAFILE$INST=="Gatty Marine Lab (University of Saint Andrews)"]<-"University of St Andrews"
DATAFILE$INST[DATAFILE$INST=="University of St Andrews"]<-"University of St Andrews"
DATAFILE$INST[DATAFILE$INST=="University of Stirlinng"]<-"University of Stirling"
DATAFILE$INST[DATAFILE$INST=="University of Tenessee"]<-"University of Tennessee"
DATAFILE$INST[DATAFILE$INST=="University of Texas At Austin"]<-"University of Texas Austin"
DATAFILE$INST[DATAFILE$INST=="University of Wisconsin"]<-"University of Wisconsin"
DATAFILE$INST[DATAFILE$INST=="Victorian School of forestry"]<-"Victorian School of Forestry"
DATAFILE$INST[DATAFILE$INST=="VRije Universiteit Amsterdam"]<-"Vrije Universiteit Amsterdam"
DATAFILE$INST[DATAFILE$INST=="vrije university amsterdam"]<-"vrije universiteit amsterdam"
DATAFILE$INST[DATAFILE$INST=="vrije universiteit"]<-"vrije universiteit amsterdam"
DATAFILE$INST[DATAFILE$INST=="Washington U St Louis"]<-"Washington University in St Louis"
DATAFILE$INST[DATAFILE$INST=="Western Cotton Research Lab."]<-"USDA Western Cotton Research Lab"
DATAFILE$INST[DATAFILE$INST=="Western Michigan University"]<-"Western Michigan University"
DATAFILE$INST[DATAFILE$INST=="Woods Hole"]<-"Woods Hole Oceanographic Institution"
DATAFILE$INST[DATAFILE$INST=="WWF"]<-"World Wildlife Fund"
DATAFILE$UNIT[DATAFILE$INST=="University of Saint Andrews"]<-"Gatty Marine Lab"
DATAFILE$UNIT[DATAFILE$INST=="University of Pretoria"]<-"Mammal Research Institute"
DATAFILE$COUNTRY[DATAFILE$INST=="Iowa State University"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="Aarhus University"]<-"Denmark"
DATAFILE$COUNTRY[DATAFILE$INST=="Southern Illinois U"]<-"USA"
DATAFILE$INST[DATAFILE$INST=="Southern Illinois U"]<-"Southern Illinois University"

##############################################################
##############################################################
# Dividing Some Names for INST into INST and UNIT
##############################################################
##############################################################

DATAFILE$UNIT[DATAFILE$INST=="Harvard University Herbaria"]<-"HU Herbaria" 
DATAFILE$INST[DATAFILE$INST=="Harvard University Herbaria"]<-"Harvard University"

DATAFILE$UNIT[DATAFILE$INST=="Harvard University Medical School"]<-"HU Medical School"
DATAFILE$INST[DATAFILE$INST=="Harvard University Medical School"]<-"Harvard University"

DATAFILE$UNIT[DATAFILE$INST=="Boston Unveristy Marine Program"]<-"Marine Program"
DATAFILE$INST[DATAFILE$INST=="Boston Unveristy Marine Program"]<-"Boston University" 

DATAFILE$UNIT[DATAFILE$INST=="Harvard University Museum of Comparative Zoology"]<-"Museum of Comparative Zoology"
DATAFILE$INST[DATAFILE$INST=="Harvard University Museum of Comparative Zoology"]<-"Harvard University" 

DATAFILE$UNIT[DATAFILE$INST=="Harvard Forest"]<-"Harvard Forest"
DATAFILE$INST[DATAFILE$INST=="Harvard Forest"]<-"Harvard University" 

DATAFILE$UNIT[DATAFILE$INST=="Harvard University Arnold Arboretum"]<-"Arnold Arboretum"
DATAFILE$INST[DATAFILE$INST=="Harvard University"]<-"Harvard University"
DATAFILE$UNIT[DATAFILE$INST=="harvard university arnold arboretum"]<-"arnold arboretum"
DATAFILE$INST[DATAFILE$INST=="harvard university arnold arboretum"]<-"harvard university"

DATAFILE$UNIT[DATAFILE$INST=="Instituto de Ecologia UNAM"]<-"Instituto de Ecologia"
DATAFILE$INST[DATAFILE$INST=="Instituto de Ecologia UNAM"]<-"Universidad Nacional Autonoma de Mexico" 

DATAFILE$UNIT[DATAFILE$INST=="Harvard University Museum of Comparative Zoology"]<-"Museum of Comparative Zoology"
DATAFILE$INST[DATAFILE$INST=="Harvard University Museum of Comparative Zoology"]<-"Harvard University" 

DATAFILE$UNIT[DATAFILE$INST=="Wageiningen University Research Center Alterra"]<-"Research Center Alterra"
DATAFILE$INST[DATAFILE$INST=="Wageiningen University Research Center Alterra"]<-"Wageiningen University" 

 
DATAFILE$INST<-gsub("L\x9fneburg","Lunenburg", DATAFILE$INST)
DATAFILE$INST<-gsub("Universit\x8at","Universitat", DATAFILE$INST)
DATAFILE$INST<-gsub("G\xf6ttingen","Gottingen", DATAFILE$INST)
DATAFILE$INST<-gsub("M\x82xico","Mexico", DATAFILE$INST)
DATAFILE$INST<-gsub("Universit\x82","Universite", DATAFILE$INST)
DATAFILE$INST<-gsub("Universit\xe9","Universite", DATAFILE$INST)
DATAFILE$INST<-gsub("Landscape\xa0Ecology","Landscape Ecology", DATAFILE$INST)
DATAFILE$INST<-gsub("Aut\xfc\xbe\x8c\xa6\x84\xbcnoma","Autonoma", DATAFILE$INST)
DATAFILE$INST<-gsub("Montr\xfc\xbe\x8e\x96\x94\xbcal","Montreal", DATAFILE$INST)
DATAFILE$INST<-gsub("<a0>Universit<e9>","Universite", DATAFILE$INST)
DATAFILE$INST<-gsub("\xfc\xbe\x8c\x86\x84\xbc","", DATAFILE$INST)
DATAFILE$INST<-gsub("RWTH Aachen\xcaUniversity","RWTH Aachen University", DATAFILE$INST)
DATAFILE$INST<-gsub("Macquarie University \xca","Macquarie University", DATAFILE$INST)
DATAFILE$INST<-gsub("leibniz institute for zoo and wildlife research\xfc\xbe\x98\x96\x8c\xbc","leibniz institute for zoo and wildlife research", DATAFILE$INST)


DATAFILE$INST<-as.character(DATAFILE$INST)
DATAFILE$INST<-tolower(DATAFILE$INST)

DATAFILE$INST[grepl("Brown Univ", DATAFILE$INST, ignore.case=TRUE)] <- "brown university"
DATAFILE$INST[grepl("Cary", DATAFILE$INST, ignore.case=TRUE)] <- "cary institute of ecosystem studies"
DATAFILE$INST[grepl("ets ingenieros", DATAFILE$INST, ignore.case=TRUE)] <- "ets ingenieros de montes"
gsub("macaulayooinstitute","istituto per lambiente marino costiero",DATAFILE$INST)
DATAFILE$INST[grepl("macaulay", DATAFILE$INST, ignore.case=TRUE)] <- "macaulay land use research institute"
DATAFILE$INST[grepl("osna", DATAFILE$INST, ignore.case=TRUE)] <- "universitat osnabruck"
DATAFILE$INST[grepl("de montreal", DATAFILE$INST, ignore.case=TRUE)] <- "university of montreal"

DATAFILE$INST<-ifelse(grepl("terrestrial ecology",DATAFILE$INST),"institute of terrestrial ecology",DATAFILE$INST)
# DATAFILE$INST<-ifelse((DATAFILE$COUNTRY="Canada" & grepl("queen",DATAFILE$INST)),"queens university",DATAFILE$INST)

DATAFILE$INST<-gsub("(retired) natural history museum london","natural history museum", DATAFILE$INST)
DATAFILE$INST<-gsub("university of aarhus","aarhus university", DATAFILE$INST)

DATAFILE$INST<-gsub("university of aarhus","aarhus university", DATAFILE$INST)
DATAFILE$INST<-gsub("acadia u","aarhus university", DATAFILE$INST)
DATAFILE$INST<-gsub("university of aarhus","aarhus university", DATAFILE$INST)
DATAFILE$INST<-gsub("university of aarhus","aarhus university", DATAFILE$INST)
DATAFILE$INST<-gsub("universityniversity","university", DATAFILE$INST)
DATAFILE$INST[DATAFILE$INST=="queen mary university of london"]<-"queen mary university of london"
DATAFILE$INST[DATAFILE$INST=="institute for terrestrial ecology" & DATAFILE$COUNTRY=="United Kingdom"]<-"institute of terrestrial ecology"

DATAFILE$INST[DATAFILE$INST=="queen marys university of london"]<-"queen mary university of london"
DATAFILE$INST[DATAFILE$INST=="university of london queen mary"]<-"queen mary university of london"
DATAFILE$INST[DATAFILE$INST=="queen marys university"]<-"queen mary university of london"
DATAFILE$UNIT[DATAFILE$INST=="acadia u"]<-"aarhus university"
DATAFILE$UNIT[DATAFILE$INST=="aberdeen university"]<-"university of aberdeen"
DATAFILE$UNIT[DATAFILE$INST=="agresearch"]<-"agresearch ltd"

DATAFILE$UNIT[DATAFILE$INST=="agriculture and agri food"]<-"agriculture and agri food canada"
DATAFILE$COUNTRY[DATAFILE$INST=="queens college city university of new york"]<-"USA"
DATAFILE$INST[DATAFILE$INST=="northern prairie wildlife research center"]<-"usgs northern prairie wildlife research center"
DATAFILE$INST[DATAFILE$INST=="usgs prairie wildlife research center"]<-"usgs northern prairie wildlife research center"
DATAFILE$COUNTRY[DATAFILE$INST=="usgs northern prairie wildlife research center"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="ohio northern university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="ohio state university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="ohio wesleyan university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="oklahoma state university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="old dominion university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="oregon state university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="vrije university amsterdam"]<-"Netherlands"
DATAFILE$COUNTRY[DATAFILE$INST=="penn state university"]<-"USA"
DATAFILE$INST<-gsub("penn state university","pennsylvaia state university", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="portland state university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="rochester institute of technology"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="rocky mountain research station"]<-"USA"
DATAFILE$INST[DATAFILE$INST=="rocky mountain research station"]<-"usfs rocky mountain research station"
DATAFILE$COUNTRY[DATAFILE$INST=="school of renewable resources louisiana state u"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="school of renewable resources louisiana state university"]<-"USA"
DATAFILE$UNIT[DATAFILE$INST=="school of renewable resources louisiana state u"]<-"school of renewable resources"
DATAFILE$INST[DATAFILE$INST=="school of renewable resources louisiana state u"]<-"louisiana state university"
DATAFILE$UNIT[DATAFILE$INST=="school of renewable resources louisiana state university"]<-"school of renewable resources"
DATAFILE$INST[DATAFILE$INST=="school of renewable resources louisiana state university"]<-"louisiana state university"
DATAFILE$COUNTRY[DATAFILE$INST=="smithsonian institution"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="smithsonian migratory bird center"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="smithsonian national museum of natural history"]<-"USA"
DATAFILE$INST[DATAFILE$INST=="stri"]<-"smithsonian tropical research institute"
DATAFILE$COUNTRY[DATAFILE$INST=="smithsonian tropical research institute"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="southeastern louisiana"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="southern illinois university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="stony brook university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="tall timbers research station"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="texas christian university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="texas tech university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="tulane university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="united state fish and wildlife service"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="universidad de buenos aires"]<-"Argentina"
DATAFILE$COUNTRY[DATAFILE$INST=="universidad de los andes"]<-"Colombia"
DATAFILE$COUNTRY[DATAFILE$INST=="universidad simon bolivar"]<-"Venezuela"
DATAFILE$COUNTRY[DATAFILE$INST=="universidadautonoma del estado de hidalgo"]<-"Mexico"
DATAFILE$INST<-gsub("universidadautonoma","universidad autonoma", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="university illinois urbana champaign"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of adelaide"]<-"Australia"
DATAFILE$COUNTRY[DATAFILE$INST=="university of alaska"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of alaska fairbanks"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of alaska museum"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of alberta"]<-"Canada"
DATAFILE$COUNTRY[DATAFILE$INST=="university of arizona"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of auckland"]<-"New Zealand"
DATAFILE$COUNTRY[DATAFILE$INST=="university of bonn"]<-"Germany"
DATAFILE$COUNTRY[DATAFILE$INST=="university of british columbia"]<-"Canada"
DATAFILE$COUNTRY[DATAFILE$INST=="university of california"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of california berkeley"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of california davis"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of california san diego"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of california riverside"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of california santa cruz"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of cambridge"]<-"United Kingdom"
DATAFILE$COUNTRY[DATAFILE$INST=="university of canterbury"]<-"New Zealand"
DATAFILE$COUNTRY[DATAFILE$INST=="university of central arkansas"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of chicago"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of colorado boulder"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of colorado denver"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of connecticut"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of copenhagen"]<-"Denmark"
DATAFILE$COUNTRY[DATAFILE$INST=="university of dayton"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of delaware"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of exeter"]<-"United Kingdom"
DATAFILE$COUNTRY[DATAFILE$INST=="university of georgia"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of hawaii manoa"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of illinois"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of illinois universityrbana champaign"]<-"USA"
DATAFILE$INST<-gsub("universityrbana","university urbana", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="university of illinois urbana champaign"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of kansas"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of london"]<-"United Kingdom"
DATAFILE$COUNTRY[DATAFILE$INST=="university of maine"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of manitoba"]<-"Canada"
DATAFILE$COUNTRY[DATAFILE$INST=="university of maryland"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of maryland baltimore county"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of massachusetts"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of massachusetts amherst"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of melbourne"]<-"Australia"
DATAFILE$COUNTRY[DATAFILE$INST=="university of michigan dearborn"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of minnesota"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of mississippi"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of missouri st louis"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of montana"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of nevada"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of nevada reno"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of new brunswick"]<-"Canada"
DATAFILE$COUNTRY[DATAFILE$INST=="university of northern british columbia"]<-"Canada"
DATAFILE$COUNTRY[DATAFILE$INST=="university of northern colorado"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of oklahoma"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of pretoria"]<-"South Africa"
DATAFILE$COUNTRY[DATAFILE$INST=="university of queensland"]<-"Australia"
DATAFILE$COUNTRY[DATAFILE$INST=="university of rhode island"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of st andrews"]<-"Scotland"
DATAFILE$COUNTRY[DATAFILE$INST=="university of saskatchewan"]<-"Canada"
DATAFILE$COUNTRY[DATAFILE$INST=="university of south dakota"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of southern mississippi"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of tel aviv"]<-"Israel"
DATAFILE$COUNTRY[DATAFILE$INST=="university of tennessee"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of victoria"]<-"Canada"
DATAFILE$COUNTRY[DATAFILE$INST=="university of virginia"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of washington"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of windsor"]<-"Canada"
DATAFILE$COUNTRY[DATAFILE$INST=="university of wisconsin"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of wisconsin milwaukee"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of wisconsin parkside"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of wurzburg"]<-"Germany"
DATAFILE$COUNTRY[DATAFILE$INST=="university of wyoming"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="universityautonoma del estado hidalgo"]<-"Mexico"
DATAFILE$INST<-gsub("universityautonoma","universidad autonoma", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="universitynacional de misiones conicet"]<-"Argentina"
DATAFILE$INST<-gsub("universitynacional","university nacional", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="universitypierre et marie curie"]<-"France"
DATAFILE$INST<-gsub("universitypierre","university pierre", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="uppsala university"]<-"Sweden"
DATAFILE$COUNTRY[DATAFILE$INST=="us forest service"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="us geological survey"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="us geological survey alaska science center"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="us geological survey biological resources division"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="us geological survey fort collins science center"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="us geological survey northern prairie wildlife research center"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="usgs northern prairie wildlife research center"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="us geological survey patuxent wildlife research center"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="us geological survey wisconsin cooperative wildlife research unit"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="usda aphis"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="usfs rocky mountain research station"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="usfws alaska region"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="usfws idaho fish and wildlife office"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="utah state university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="vassar college"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="villanova university"]<-"USA"
DATAFILE$INST[DATAFILE$INST=="virginia polytechnic institute state university"]<-"virginia polytechnic institute and state university"
DATAFILE$COUNTRY[DATAFILE$INST=="virginia polytechnic institute and state university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="virginia polytechnic institute state university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="washington state university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="western michigan university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="wichita state university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="york university"]<-"Canada"
DATAFILE$COUNTRY[DATAFILE$INST=="zoological society of london"]<-"United Kingdom"
DATAFILE$COUNTRY[DATAFILE$INST=="prairie wildlife research center"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="queens university"]<-"Canada"
DATAFILE$COUNTRY[DATAFILE$INST=="pacific rim conservation"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="open university"]<-"United Kingdom"
DATAFILE$COUNTRY[DATAFILE$INST=="palacky university"]<-"Czech Republic"
DATAFILE$COUNTRY[DATAFILE$INST=="percy fitzpatrick institute"]<-"South Africa"
DATAFILE$COUNTRY[DATAFILE$INST=="point blue conservation"]<-"USA"
DATAFILE$INST[DATAFILE$INST=="queens university of belfast"]<-"queens university belfast"
DATAFILE$COUNTRY[DATAFILE$INST=="queens university of belfast"]<-"Northern Ireland"
DATAFILE$COUNTRY[DATAFILE$INST=="queens university belfast"]<-"Northern Ireland"
DATAFILE$COUNTRY[DATAFILE$INST=="rangeland ecosystem science center"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="tabor college"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="snake river field station"]<-"USA" #USGS
DATAFILE$COUNTRY[DATAFILE$INST=="senckenberg research institute frankfurt"]<-"Germany"
DATAFILE$COUNTRY[DATAFILE$INST=="simon fraser university"]<-"Canada"
DATAFILE$COUNTRY[DATAFILE$INST=="roosevelt universityfield museum"]<-"USA"
DATAFILE$INST[DATAFILE$INST=="roosevelt universityfield museum"]<-"field museum"
DATAFILE$COUNTRY[DATAFILE$INST=="royal ontario museum"]<-"Canada"
DATAFILE$COUNTRY[DATAFILE$INST=="ontario ministry of natural resources"]<-"Canada"
DATAFILE$COUNTRY[DATAFILE$INST=="stazione zoologica anton dohrn"]<-"Italy"
DATAFILE$COUNTRY[DATAFILE$INST=="tetra tech eba"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="tetra tech ec inc"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="tetratech environmental consulting inc"]<-"USA"
DATAFILE$INST[DATAFILE$INST=="tetra tech eba"]<-"tetra tech inc"
DATAFILE$INST[DATAFILE$INST=="tetra tech ec inc"]<-"tetra tech inc"
DATAFILE$INST[DATAFILE$INST=="tetratech environmental consulting inc"]<-"tetra tech inc"
DATAFILE$COUNTRY[DATAFILE$INST=="aarhus university"]<-"Denmark"
DATAFILE$INST[DATAFILE$INST=="notre dame"]<-"university of notre dame"
DATAFILE$INST[DATAFILE$INST=="notre dame university"]<-"university of notre dame"
DATAFILE$COUNTRY[DATAFILE$INST=="tokaigakuen university"]<-"Japan"
DATAFILE$INST<-gsub("tokaigakuen","tokai gakuen", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="trent university"]<-"Canada"
DATAFILE$COUNTRY[DATAFILE$INST=="trinity university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="western foundation of vertebrate zoology"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="western university canada"]<-"Canada"
DATAFILE$COUNTRY[DATAFILE$INST=="universidadde antioquia"]<-"Colombia"
DATAFILE$COUNTRY[DATAFILE$INST=="universidadde buenos aires"]<-"Argentina"
DATAFILE$COUNTRY[DATAFILE$INST=="universidadde los andes"]<-"Colombia"
DATAFILE$COUNTRY[DATAFILE$INST=="universidadde vila velha"]<-"Brazil"
DATAFILE$INST[DATAFILE$INST=="universidadde vila velha"]<-"universidade vila velha"
DATAFILE$INST<-gsub("universidadde","universidad de", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="universidadnacional autonoma de mexico"]<-"Mexico"
DATAFILE$INST<-gsub("universidadnacional","universidad nacional", DATAFILE$INST)
DATAFILE$COUNTRY[DATAFILE$INST=="universita del salento"]<-"Italy"
DATAFILE$COUNTRY[DATAFILE$INST=="universita di genova"]<-"Italy"
DATAFILE$COUNTRY[DATAFILE$INST=="universite pierre et marie curie"]<-"France"

DATAFILE$COUNTRY[DATAFILE$INST=="american museum of natural history"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="free university amsterdam"]<-"Netherlands"
DATAFILE$COUNTRY[DATAFILE$INST=="antioch new england graduate school"]<-"USA"
DATAFILE$INST[DATAFILE$INST=="antioch new england graduate school and national university of rwanda"]<-"antioch new england graduate school"
DATAFILE$INST[DATAFILE$INST=="arizona state u"]<-"arizona state university"
DATAFILE$INST[DATAFILE$INST=="asu"]<-"arizona state university"
DATAFILE$COUNTRY[DATAFILE$INST=="arizona state university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="auburn university"]<-"USA"
DATAFILE$INST[DATAFILE$INST=="agresearch ltd."]<-"agresearch"
DATAFILE$INST[DATAFILE$INST=="agresearch ltd"]<-"agresearch"
DATAFILE$INST<-gsub("stationn","station", DATAFILE$INST)
DATAFILE$INST<-gsub("assicuates","associates", DATAFILE$INST)
DATAFILE$INST[DATAFILE$INST=="agriculture and agri food"]<-"agriculture and agri food canada"

DATAFILE$COUNTRY[DATAFILE$INST=="alaska science center"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="amherst college"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="austin peay state university"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="australian commonwealth scientific and research organization"]<-"Australia"
DATAFILE$COUNTRY[DATAFILE$INST=="australian museum"]<-"Australia"	
DATAFILE$COUNTRY[DATAFILE$INST=="australian national wildlife collection"]<-"Australia"	
DATAFILE$COUNTRY[DATAFILE$INST=="aves argentinas"]<-"Argentina"	
DATAFILE$COUNTRY[DATAFILE$INST=="bates college"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="beloit college"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="bloomfield college"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="boise state u"]<-"USA"
DATAFILE$INST[DATAFILE$INST=="boise state u"]<-"boise state university"
DATAFILE$COUNTRY[DATAFILE$INST=="boise state university"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="bucknell university"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="california academy of sciences"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="california state university san marcos"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="canadian wildlife service"]<-"Canada"	
DATAFILE$COUNTRY[DATAFILE$INST=="catholic university of chile"]<-"Chile"	
DATAFILE$COUNTRY[DATAFILE$INST=="centre national de la recherche scientifique"]<-"France"	
DATAFILE$COUNTRY[DATAFILE$INST=="citadel military college of south carolina"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="university of pittsburgh"]<-"USA"
DATAFILE$CITY[DATAFILE$INST=="university of pittsburgh"]<-"Pittsburgh"
DATAFILE$STATE[DATAFILE$INST=="university of pittsburgh"]<-"PA"
DATAFILE$COUNTRY[DATAFILE$INST=="city university of new york"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="college of new jersey"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="college of william and mary"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="colorado state university"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="columbia university"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="commonwealth university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="consejo nacional de investigaciones cientificas y tecnicas"]<-"Argentina"
DATAFILE$COUNTRY[DATAFILE$INST=="cornell lab of ornithology"]<-"USA"
DATAFILE$UNIT[DATAFILE$INST=="cornell lab of ornithology"]<-"cornell lab of ornithology"
DATAFILE$INST[DATAFILE$INST=="cornell lab of ornithology"]<-"cornell university"
DATAFILE$COUNTRY[DATAFILE$INST=="cornell u"]<-"USA"	
DATAFILE$INST[DATAFILE$INST=="cornell u"]<-"cornell university"	
DATAFILE$COUNTRY[DATAFILE$INST=="cornell university"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="deakin university australia"]<-"Australia"
DATAFILE$COUNTRY[DATAFILE$INST=="delta waterfowl foundation"]<-"Canada"
DATAFILE$COUNTRY[DATAFILE$INST=="denver museum of nature e science"]<-"USA"	
DATAFILE$INST[DATAFILE$INST=="denver museum of nature e science"]<-"denver museum of nature and science"	
DATAFILE$COUNTRY[DATAFILE$INST=="disteba universita di lecce"]<-"Italy"	
DATAFILE$COUNTRY[DATAFILE$INST=="donana biological station csic"]<-"Spain"	
DATAFILE$COUNTRY[DATAFILE$INST=="ducks universitynlimited canada"]<-"Canada"
DATAFILE$INST[DATAFILE$INST=="ducks universitynlimited canada"]<-"ducks ulimited canada"
DATAFILE$COUNTRY[DATAFILE$INST=="duke university"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="durham university"]<-"United Kingdom"	
DATAFILE$COUNTRY[DATAFILE$INST=="eastern illinois university"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="environment canada"]<-"Canada"	
DATAFILE$COUNTRY[DATAFILE$INST=="environment canada national hydrology research center"]<-"Canada"	
DATAFILE$COUNTRY[DATAFILE$INST=="environment canada wildlife research division"]<-"Canada"	
DATAFILE$COUNTRY[DATAFILE$INST=="environment canada wildlife research east"]<-"Canada"	
DATAFILE$COUNTRY[DATAFILE$INST=="estacion biologia chamela"]<-"Mexico"	
DATAFILE$COUNTRY[DATAFILE$INST=="estern illinois university"]<-"USA"
DATAFILE$INST[DATAFILE$INST=="estern illinois university"]<-"eastern illinois university"
DATAFILE$COUNTRY[DATAFILE$INST=="field museum of natural history"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="florida institute of technology"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="forschungsinstitut senckenberg"]<-"Germany"	
DATAFILE$COUNTRY[DATAFILE$INST=="free university of berlin"]<-"Germany"		
DATAFILE$COUNTRY[DATAFILE$INST=="geomar helmholtz center for ocean research kiel"]<-"Germany"			
DATAFILE$COUNTRY[DATAFILE$INST=="georgia southern u"]<-"USA"
DATAFILE$INST[DATAFILE$INST=="georgia southern u"]<-"georgia southern university"	
DATAFILE$COUNTRY[DATAFILE$INST=="georgia southern university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="guangxi university"]<-"China"	
DATAFILE$COUNTRY[DATAFILE$INST=="hamilton college"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="harvard university"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="hasting reservation"]<-"USA"
DATAFILE$UNIT[DATAFILE$INST=="hasting reservation"]<-"hastings natural history reservation"
DATAFILE$COUNTRY[DATAFILE$INST=="hawk mountain sanctuary"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="hobart and william smith college"]<-"USA"		
DATAFILE$COUNTRY[DATAFILE$INST=="hofstra university"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="humbolt state university"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="hunter college cuny"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="ibigeo conicet"]<-"Argentina"	
DATAFILE$COUNTRY[DATAFILE$INST=="illinois natural history survey"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="indiana state university"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="institut fur ostseeforschungwarnemunde"]<-"Germany"		
DATAFILE$INST[DATAFILE$INST=="institut fur ostseeforschungwarnemunde"]<-"Leibniz Institute for Baltic Sea Research"
DATAFILE$COUNTRY[DATAFILE$INST=="instituto de biologia universitynam"]<-"Mexico"			
DATAFILE$UNIT[DATAFILE$INST=="instituto de biologia universitynam"]<-"instituto de biologia"
DATAFILE$INST[DATAFILE$INST=="instituto de biologia universitynam"]<-"universidad nacional autonoma de mexico"
DATAFILE$COUNTRY[DATAFILE$INST=="interuniversity institute for marine sciences"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="james san jacinto mountains reserve"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="kansas state university"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="kruger national park"]<-"South Africa"		
DATAFILE$COUNTRY[DATAFILE$INST=="los alamos national laboratory"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="louisiana state university"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="louisiana state university and university federal university of roraima"]<-"USA"
DATAFILE$INST[DATAFILE$INST=="louisiana state university and university federal university of roraima"]<-"louisiana state university"	
DATAFILE$COUNTRY[DATAFILE$INST=="lsa associates"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="lundy environmental consulting"]<-"USA"		
DATAFILE$COUNTRY[DATAFILE$INST=="massey university"]<-"New Zealand"			
DATAFILE$COUNTRY[DATAFILE$INST=="max planck institute for ornithology"]<-"Germany"			
DATAFILE$COUNTRY[DATAFILE$INST=="mckendree university"]<-"USA"			
DATAFILE$COUNTRY[DATAFILE$INST=="michigan state university"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="minnesota pollution control agency"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="missouri department of conservation"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="museum of new zealand te papa tongarewa"]<-"New Zealand"	
DATAFILE$COUNTRY[DATAFILE$INST=="national audubon society"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="national aviary"]<-"USA"		
DATAFILE$COUNTRY[DATAFILE$INST=="national history museum paris"]<-"France"
DATAFILE$COUNTRY[DATAFILE$INST=="national park service inventory e monitoring program"]<-"USA"
DATAFILE$INST[DATAFILE$INST=="national park service inventory e monitoring program"]<-"usnps inventory and monitoring program"
DATAFILE$COUNTRY[DATAFILE$INST=="national university of singapore"]<-"Singapore"	
DATAFILE$COUNTRY[DATAFILE$INST=="natural history museum"]<-"United Kingdom"
DATAFILE$COUNTRY[DATAFILE$INST=="natural history museum uk"]<-"United Kingdom"
DATAFILE$INST[DATAFILE$INST=="natural history museum uk"]<-"natural history museum"
DATAFILE$COUNTRY[DATAFILE$INST=="natural history museum uk"]<-"United Kingdom"	
DATAFILE$COUNTRY[DATAFILE$INST=="natural resources institute university of manitoba"]<-"Canada"		
DATAFILE$UNIT[DATAFILE$INST=="natural resources institute university of manitoba"]<-"natural resources institute"
DATAFILE$INST[DATAFILE$INST=="natural resources institute university of manitoba"]<-"university of manitoba"
DATAFILE$COUNTRY[DATAFILE$INST=="new mexico state university"]<-"USA"	
DATAFILE$COUNTRY[DATAFILE$INST=="Interuniversity Institute for Marine Sciences"]<-"Israel"	
# DATAFILE$COUNTRY[DATAFILE$INST=="north central research station"]<-"USA"
DATAFILE$INST[DATAFILE$INST=="north central research station"]<-"usfs north central research station"
DATAFILE$COUNTRY[DATAFILE$INST=="usfs north central research station"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="north dakota state university"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$INST=="northern prairie wildlife research center"]<-"USA"	
DATAFILE$INST[DATAFILE$INST=="northern praire wildlife research center"]<-"usgs northern prairie wildlife research center" 
DATAFILE$COUNTRY[DATAFILE$INST=="rocky mountain research station"]<-"USA"	
DATAFILE$INST[DATAFILE$INST=="us forest service usda"]<-"us forest service"
DATAFILE$COUNTRY[DATAFILE$INST=="university of missouri st. louis"]<-"USA"
DATAFILE$INST[DATAFILE$INST=="university of missouri st. louis"]<-"university of missouri st louis"
DATAFILE$COUNTRY[DATAFILE$INST=="(retired) natural history museum london"]<-"United Kingdom"
# DATAFILE$INST[DATAFILE$INST=="(retired) natural history museum london"]<-"natural history museum"
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
DATAFILE$INST[DATAFILE$INST== "centre decologie fonctionnelle et evolutive" ]<-"cnrs centre decologie fonctionnelle et evolutive" 
DATAFILE$INST[DATAFILE$INST== "cnrs centre decologie fonctionnelle et evolutive" ]<-"cnrs centre decologie fonctionnelle et evolutive" 
DATAFILE$INST[DATAFILE$INST== "christ church"]<-"christchurch"
DATAFILE$INST[DATAFILE$INST=="cisro sustainable ecosystems" ]<-"csiro sustainable ecosystems"
DATAFILE$INST[DATAFILE$INST=="columbia universily" ]<-"columbia university"
DATAFILE$INST[DATAFILE$INST== "commonwealth scientific and industrial research organisation" ]<-"csiro commonwealth scientific and industrial research organisation"
DATAFILE$INST[DATAFILE$INST== "csiro" ]<-"csiro commonwealth scientific and industrial research organisation"
DATAFILE$INST[DATAFILE$INST== "commonwealth scientific and industrial research organisation (csiro)"]<-"csiro commonwealth scientific and industrial research organisation"
DATAFILE$INST[DATAFILE$INST== "cisro" & DATAFILE$UNIT=="Tropical Ecosystems"]<-"csiro tropical ecosystems"
DATAFILE$INST[DATAFILE$INST== "consejo nacl invest cient and tecn"]<-"consejo nacional de investigaciones cientificas y tecnicas" 
DATAFILE$INST[DATAFILE$INST== "davis"]<-"university of california davis"
DATAFILE$INST[DATAFILE$INST== "deakin university australia"]<-"deakin university"
DATAFILE$UNIT[DATAFILE$INST=="division of reptiles and amphibians smithsonian institution washington dc" ]<-"division of reptiles and amphibians" 
DATAFILE$INST[DATAFILE$INST== "division of reptiles and amphibians smithsonian institution washington dc"]<-"national museum of natural history smithsonian institution"
DATAFILE$INST[DATAFILE$INST=="donana biological station" ]<-"csic donana biological station"
DATAFILE$INST[DATAFILE$INST=="donana biological station csic" ]<-"csic donana biological station"
DATAFILE$INST[DATAFILE$INST=="dresden institute of technology" ]<-"dresden university of technology"
DATAFILE$INST[DATAFILE$INST== "eawag/eth zurich"]<- "Swiss Federal Institute of Aquatic Science and Technology"
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
  DATAFILE$INST[DATAFILE$INST==  "j.w. goethe university frankfurt am main"]<- "Goethe University Frankfurt"
  DATAFILE$INST[DATAFILE$INST== "jaldin botanique nacional de belgique" ]<- "jardin botanique nacional de belgique"
  DATAFILE$INST[DATAFILE$INST== "john innes center for plant science research"  ]<- "john innes center" 
  DATAFILE$INST[DATAFILE$UNIT=="la selva biological station"  ]<- "la selva biological station"
  DATAFILE$INST[DATAFILE$INST== "la selva biological station" ]<- "organization for tropical studies"
  DATAFILE$INST[DATAFILE$INST== "landcaster university" ]<- "lancaster university"
  DATAFILE$INST[DATAFILE$INST== "ludwig maximilians universit<84>t m<81>nchen" ]<- "ludwig maximilian university of munich"
  DATAFILE$INST[DATAFILE$INST== "ludwig maximilians university of munich" ]<- "ludwig maximilian university of munich"
  DATAFILE$INST[DATAFILE$INST== "university of munich" ]<- "ludwig maximilian university of munich"
  DATAFILE$COUNTRY[DATAFILE$INST== "ludwig maximilian university of munich" ]<- "Germany"
  DATAFILE$INST[DATAFILE$INST== "lehrstuhl fur landschaftsokologie der technischen universitat munchen" ]<- "technical university of munich"
  DATAFILE$INST[DATAFILE$INST== "ludwig maximilians universitat munchen" ]<- "ludwig maximilians university of munich"
  DATAFILE$INST[DATAFILE$INST== "macaulay institute" ]<-"macaulay land use research institute"
    DATAFILE$INST[DATAFILE$INST== "macquaire university" ]<-"macquarie university"
    DATAFILE$INST[DATAFILE$INST== "michigan technol university" ]<-"michigan technological university"
    DATAFILE$INST[DATAFILE$INST== "mississippi state" ]<-"mississippi state university"
    DATAFILE$INST[DATAFILE$INST== "museum fu_r naturkunde" ]<-"museum fur naturkunde"
    DATAFILE$INST[DATAFILE$INST== "n/a"   ]<-"missing"  
    DATAFILE$INST[DATAFILE$INST== "national audobon society"  ]<-"national audubon society" 
    DATAFILE$INST[DATAFILE$INST== "natural history museum" & DATAFILE$COUNTRY=="United Kingdom" ]<-"natural history museum london"
    DATAFILE$INST[DATAFILE$INST== "national history museum paris" ]<-"national natural history museum france"
    DATAFILE$INST[DATAFILE$INST==  "nioz royal netherlands inst sea res"]<-"royal netherlands institute for sea research"
    DATAFILE$INST[DATAFILE$INST== "noinst" ]<-"missing"
    DATAFILE$INST[DATAFILE$INST==  "north carolina agricultural and technical state university" ]<-"north carolina a and t state university" 
    DATAFILE$INST[DATAFILE$INST==  "ohio university main campus"]<-"ohio university"
    DATAFILE$INST[DATAFILE$INST== "pacific agri food research centre"  ]<-"pacific agrifood research centre" 
    DATAFILE$INST[DATAFILE$INST== "pekinn"   ]<-"peking"  
    DATAFILE$INST[DATAFILE$INST== "pepperdine"  ]<-"pepperdine university" 
    DATAFILE$INST[DATAFILE$INST==  "percy fitzpatrick institute"]<-"percy fitzpatrick institute of african ornithology"
    DATAFILE$INST[DATAFILE$INST== "plant sciences department" & DATAFILE$LAST_NAME=="Samples"  ]<-"university of tennessee"
    DATAFILE$INST[DATAFILE$INST==  "queens u" ]<-"queens university" 
    DATAFILE$INST[DATAFILE$INST== "radboud university" ]<-"radboud university nijmegen"
    DATAFILE$INST[DATAFILE$INST== "retired no affiliation listed" ]<-"retired (no affiliation)"
    DATAFILE$INST[DATAFILE$INST== "riverside" ]<- "university of california riverside"
    DATAFILE$INST[DATAFILE$INST== "rose hulman institute of technology\v" ]<-"rose hulman institute of technology"
    DATAFILE$INST[DATAFILE$INST==  "santa cruz"]<-"university of california santa cruz"
    DATAFILE$INST[DATAFILE$INST== "school of biological sciences" & DATAFILE$LAST_NAME=="Wardle"  ]<-"university of sydney"
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
    DATAFILE$INST[DATAFILE$INST== "stony brook university"	 ]<-"state university of new york stony brook"
    DATAFILE$INST[DATAFILE$INST==  "suny albany"]<-"state university of new york albany"
    DATAFILE$INST[DATAFILE$INST=="suny stony brook" ]<-"state university of new york stony brook"
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
    DATAFILE$COUNTRY[DATAFILE$INST=="university of notre dame"]<-"USA"
    
    DATAFILE$INST[DATAFILE$INST==  "university of east angolia"]<-"university of east anglia"
    DATAFILE$INST[DATAFILE$INST=="university of california santa diego"  ]<-"university of california san diego"
    DATAFILE$INST[DATAFILE$INST== "cnrs montellier" ]<-"centre decologie fonctionnelle et evolutive cnrs"
    DATAFILE$INST[DATAFILE$INST== "universite claude bernard lyons" ]<-"universite claude bernard lyon 1"
    DATAFILE$INST[DATAFILE$INST==  "royal botanical gardens kew"]<-"royal botanic gardens kew"
    DATAFILE$INST[DATAFILE$INST==  "royal botanic gardens" & DATAFILE$COUNTRY=="United Kingdom"]<-"royal botanic gardens kew"
    DATAFILE$INST[DATAFILE$INST== "universite montpellier ii" ]<-"universite montpellier 2"
    DATAFILE$INST[DATAFILE$INST== "university of querzburg"  ]<-"university of wurzburg"
    DATAFILE$INST[DATAFILE$INST== "environmental protection agency" ]<-"us environmental protection agency"
    DATAFILE$INST[DATAFILE$INST== "univesity of sterling" ]<-"univesity of stirling"
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
    DATAFILE$INST[DATAFILE$INST== "institute of ecosystem studies"	& DATAFILE$COUNtRY=="USA" ]<- "cary institute of ecosystem studies"
    # DATAFILE$INST[DATAFILE$INST==  "university of college" ]<-university college  2x not colgne
    DATAFILE$INST[DATAFILE$INST== 	"waginen university" ]<-"wageningen university"
    DATAFILE$INST[DATAFILE$INST== "universit√© de montreal"	 ]<-"university of montreal"
    DATAFILE$INST[DATAFILE$INST== "state university of new york" ]<- "suny state university of new york"
    DATAFILE$INST[DATAFILE$INST== "university of nebraska at omaha" ]<-"university of nebraska omaha"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of nebraska omaha"]<-"USA"
    DATAFILE$INST[DATAFILE$INST== "university of nebraska" ]<-"university of nebraska lincoln"
    DATAFILE$INST[DATAFILE$INST== "unl" ]<-"university of nebraska lincoln"
    DATAFILE$INST[DATAFILE$INST=="noura ziadi"]<-"agriculture and agri food canada"
    DATAFILE$INST[DATAFILE$INST=="teagasc"]<-"Agriculture and Food Development Authority"
    DATAFILE$INST[DATAFILE$INST=="beaverlodge res farm"]<-"beaverlodge research farm"
    DATAFILE$INST[DATAFILE$INST=="service canadien des forets"]<-"canadian forest service"
    DATAFILE$INST[DATAFILE$INST=="center for international forestry research cifor"]<-"center for international forestry research"
    DATAFILE$COUNTRY[DATAFILE$INST=="shahid beheshti university"]<-"Iran"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of hong kong"]<-"Hong Kong"
    DATAFILE$INST[DATAFILE$INST=="brazilian agricultural research corporation cirad"]<-"cirad-usp-esalq consortium"
    DATAFILE$INST[DATAFILE$INST=="centre decologie des ressources renouvelables"]<-"cnrs centre decologie des ressources renouvelables"
    DATAFILE$INST[DATAFILE$INST=="centre national de la recherche scientifique"]<-"cnrs centre national de la recherche scientifique"
    DATAFILE$INST[DATAFILE$INST=="cnrs"]<-"cnrs centre national de la recherche scientifique"
    DATAFILE$INST[DATAFILE$INST=="centre decologie fonctionnelle et evolutive cnrs"]<-"cnrs institut des sciences de levolution montpellier"
    DATAFILE$INST[DATAFILE$INST=="consejo nacional de investigaciones cientificas y tecnicas"]<-"conicet"
    DATAFILE$INST[DATAFILE$INST=="investigador conicet"]<-"CONICET CCT Mendoza"
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
    DATAFILE$INST[DATAFILE$INST=="universidade de sao paulo"]<-"universidade estadual paulista"
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
    DATAFILE$COUNTRY[DATAFILE$INST=="university of tubingen"]<-"Germany"
    DATAFILE$INST[DATAFILE$INST=="umea university"]<-"university of umea"
    DATAFILE$INST[DATAFILE$INST=="us army"]<-"US Army Engineer Research and Development Center"
    DATAFILE$INST[DATAFILE$INST=="usda us department of agriculture"]<-"usda"
    DATAFILE$INST[DATAFILE$INST=="usda ars"]<-"usda agricultural research service"
    DATAFILE$INST[DATAFILE$INST=="usda aphis"]<-"usda Animal and Plant Health Inspection Service"
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
    DATAFILE$INST[DATAFILE$INST=="usfws national wetlands research center"]<-"usfws national wetlands research center"
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
    # DATAFILE$UNIT[DATAFILE$INST=="centro de ecologia"]<-"centro de ecologia"
    # DATAFILE$UNIT[DATAFILE$INST=="yale school of forestry and environmental studies"]<-"school of forestry and environmental studies"
    DATAFILE$UNIT[DATAFILE$INST=="washington university school of medicine"]<-"school of medicine"
    DATAFILE$INST_CHECK[DATAFILE$INST=="us geological survey and university of miami"]<-"2x primary inst"
    DATAFILE$UNIT[DATAFILE$INST=="savannah river ecology laboratory"]<-"savannah river ecology laboratory"
    DATAFILE$NOTES[DATAFILE$INST=="haus nr.9"]<-"Max-Planck Institute for ornithology in a journal article from this time"
    DATAFILE$UNIT[DATAFILE$INST=="scripps institution of oceanography"]<-"scripps institution of oceanography"
    # DATAFILE$UNIT[DATAFILE$INST=="scripps institute of oceanography"]<-"scripps institution of oceanography"
    # DATAFILE$UNIT[DATAFILE$INST=="savannah river ecology laboratory"]<-"savannah river ecology laboratory"
    
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
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    gsub("us geological survey","usgs",DATAFILE$INST)
    gsub("us forest service","usfs",DATAFILE$INST)
    gsub("us department of agriculture","usda",DATAFILE$INST)

    
    DATAFILE$INST[DATAFILE$INST=="(retired) natural history museum london"]<-"retired"
    DATAFILE$INST[DATAFILE$INST=="agricultural university wageningen"]<-"wageningen agricultural university"
    DATAFILE$INST[DATAFILE$INST=="alberta"]<-"university of alberta"
    DATAFILE$COUNTRY[DATAFILE$INST=="alexandra university"]<-"Egypt"
    DATAFILE$COUNTRY[DATAFILE$INST=="alfred wegener institut fur polar und meeresforschung"]<-"Germany"
    DATAFILE$COUNTRY[DATAFILE$INST=="arete associates"]<-"USA"
    DATAFILE$UNIT[DATAFILE$INST=="arnold arboretum of harvard university"]<-"arnold arboretum"
    DATAFILE$INST[DATAFILE$INST=="arnold arboretum of harvard university"]<-"harvard university"
    DATAFILE$INST[DATAFILE$INST=="australian commonwealth scientific and research organization"]<-"csiro commonwealth scientific and industrial research organisation"
    DATAFILE$COUNTRY[DATAFILE$INST=="australian national university"]<-"Australia"
    DATAFILE$INST[DATAFILE$INST=="basf plant science"]<-"basf"
    DATAFILE$COUNTRY[DATAFILE$INST=="biological centre"]<-"Netherlands"
    DATAFILE$COUNTRY[DATAFILE$INST=="biological institute"]<-"Yugoslavia"
    DATAFILE$INST[DATAFILE$INST=="bristol university"]<-"university of bristol"
    DATAFILE$COUNTRY[DATAFILE$INST=="british antarctic survey"]<-"United Kingdom"
    DATAFILE$INST[DATAFILE$INST=="calyx, inc"]<-"calyx inc"
    DATAFILE$INST[DATAFILE$INST=="university of cardiff"]<-"cardiff university"
    DATAFILE$COUNTRY[DATAFILE$INST=="carl von ossietzky university oldenburg"]<-"Germany"
    DATAFILE$INST[DATAFILE$INST=="catholic university of chile"]<-"pontificia universidad catolica de chile"
    DATAFILE$UNIT[DATAFILE$INST=="center of marine sciences of algarve universidade"]<-"center of marine sciences"
    DATAFILE$INST[DATAFILE$INST=="center of marine sciences of algarve universidade"]<-"universidade do algarve"
    DATAFILE$COUNTRY[DATAFILE$INST=="universidade do algarve"]<-"Portugal"
    DATAFILE$INST[DATAFILE$INST=="chris britton consultancy balfor beatty"]<-"chris britton consultancy"
    DATAFILE$COUNTRY[DATAFILE$INST=="christian albrechts universitat kiel"]<-"Germany"
    DATAFILE$COUNTRY[DATAFILE$INST=="cinvestav"]<-"Mexico"
    DATAFILE$INST[DATAFILE$INST=="ciudad universitaria"]<-"ets ingenieros de montes"
    DATAFILE$COUNTRY[DATAFILE$INST=="cnrs centre decologie fonctionnelle et evolutive"]<-"France"
    DATAFILE$INST[DATAFILE$INST=="college of staten island cuny"]<-"cuny college of staten island"
    DATAFILE$INST[DATAFILE$INST=="colorado cooperative fish and wildlife unit"]<-"usgs colorado cooperative fish and wildlife unit"
    DATAFILE$INST[DATAFILE$INST=="commonwealth university"]<-"virginia commonwealth university"
    DATAFILE$INST[DATAFILE$INST=="csic institut mediterrani destudis avencats"]<-"Mediterranean Institute for Advanced Studies"
    DATAFILE$INST[DATAFILE$INST=="csic institut mediterrani destudis avencats"]<-"Mediterranean Institute for Advanced Studies"
    DATAFILE$INST[DATAFILE$INST=="csic instituto de ciencias del mar"]<-"Mediterranean Institute for Advanced Studies"
    DATAFILE$COUNTRY[DATAFILE$INST=="doe brookhaven national laboratory"]<-"USA"
    DATAFILE$COUNTRY[DATAFILE$INST=="doe brookhaven national laboratory"]<-"USA"
    DATAFILE$INST[DATAFILE$INST=="environment canada wildlife research division"]<-"environment canada wildlife research east"
    DATAFILE$INST[DATAFILE$INST=="evolutionary biology centre"]<-"university of uppsala"
    DATAFILE$COUNTRY[DATAFILE$INST=="florida dept of natural resources bureau of marine research"]<-"USA"
    DATAFILE$INST[DATAFILE$INST=="forschungsinstitut senckenberg"]<-"senckenberg research institute"
    DATAFILE$INST[DATAFILE$INST=="gorgan university of agric sci"]<-"gorgan university of agricultural sciences"
    DATAFILE$COUNTRY[DATAFILE$INST=="griffith university"]<-"Australia"
    DATAFILE$COUNTRY[DATAFILE$INST=="haus nr9"]<-"Germany"
    DATAFILE$COUNTRY[DATAFILE$INST=="hebrew university of jerusalem"]<-"Israel"
    DATAFILE$COUNTRY[DATAFILE$INST=="hokkaido university"]<-"Japan"
    DATAFILE$COUNTRY[DATAFILE$INST=="humboldt university of berlin"]<-"Germany"
    DATAFILE$INST[DATAFILE$INST=="hunter college cuny"]<-"cuny hunter college"
    DATAFILE$COUNTRY[DATAFILE$INST=="icbs ufal and oxford university"]<-"Brazil"
    DATAFILE$INST[DATAFILE$INST=="icbs ufal and oxford university"]<-"universidade federal de alagoas"
    DATAFILE$INST[DATAFILE$INST=="inra"]<-"french national institute for agricultural research inra"
    DATAFILE$COUNTRY[DATAFILE$INST=="institut national scientifique et technnique d oceanographie et de peche"]<-"Tunisia"
    DATAFILE$COUNTRY[DATAFILE$INST=="institute of marine biology of crete"]<-"Greece"
    DATAFILE$INST[DATAFILE$INST=="institute of tropical forestry"]<-"international institute of tropical forestry"
    DATAFILE$INST[DATAFILE$INST=="instituto de biociencias"]<-"universidade de sao paulo"
    DATAFILE$INST[DATAFILE$INST=="instituto mediterraneo de estudios avanzados (imedea)"]<-"Mediterranean Institute for Advanced Studies"
    DATAFILE$COUNTRY[DATAFILE$INST=="international institute for applied systems analysis"]<-"Austria"
    DATAFILE$COUNTRY[DATAFILE$INST=="international institute of tropical forestry"]<-"Puerto Rico"
    DATAFILE$INST[DATAFILE$INST=="istituto per l¬¢ambiente marino costiero"]<-"instituto per lambiente marino costiero"
    DATAFILE$INST[DATAFILE$INST=="istituto per l¢ambiente marino costiero"]<-"instituto per lambiente marino costiero"
    DATAFILE$COUNTRY[DATAFILE$INST=="james cook university"]<-"Australia"
    DATAFILE$COUNTRY[DATAFILE$INST=="james cook university"]<-"Australia"
    DATAFILE$INST[DATAFILE$INST=="kings college"]<-"kings college london"
    DATAFILE$COUNTRY[DATAFILE$INST=="kyushu university"]<-"Japan"
    DATAFILE$COUNTRY[DATAFILE$INST=="laboratorio di geologia marina del cnr"]<-"Italy"
    DATAFILE$COUNTRY[DATAFILE$INST=="lakehead university"]<-"Canada"
    DATAFILE$INST[DATAFILE$INST=="lancaster"]<-"lancaster university"
    DATAFILE$COUNTRY[DATAFILE$INST=="lancaster university"]<-"United Kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="leibniz institut fur meereswissenschaften ifmgeomar"]<-"Germany"
    # DATAFILE$INST[DATAFILE$INST=="lincoln university"]<-"university of lincoln"
    DATAFILE$INST[DATAFILE$INST=="lincoln college"]<-"lincoln university"
    # DATAFILE$COUNTRY[DATAFILE$INST=="university of lincoln"]<-"United Kingdom"
    # DATAFILE$COUNTRY[DATAFILE$INST=="lincoln university"]<-"New Zealand"
    DATAFILE$INST[DATAFILE$INST=="ludwig maximilian universitat munchen"]<-"ludwig maximilian university of munich"
    DATAFILE$COUNTRY[DATAFILE$INST=="luminy universite d aix marseille"]<-"France"
    DATAFILE$INST[DATAFILE$INST=="macaulay¬†institute"]<-"macaulay land use research institute"
    DATAFILE$COUNTRY[DATAFILE$INST=="max planck institute for behavioral physiology"]<-"Germany"
    DATAFILE$COUNTRY[DATAFILE$INST=="max planck institute for demographic research"]<-"Germany"
    DATAFILE$COUNTRY[DATAFILE$INST=="mcgill university"]<-"Canada"
    DATAFILE$INST[DATAFILE$INST=="Meggers"]<-"smithsonian national museum of natural history"
    DATAFILE$COUNTRY[DATAFILE$INST=="memorial university of newfoundland"]<-"Canada"
    DATAFILE$INST[DATAFILE$INST=="millbrook"]<-"cary institute of ecosystem studies"
    DATAFILE$COUNTRY[DATAFILE$INST=="monterey bay aquarium research institute"]<-"USA"
    DATAFILE$INST[DATAFILE$INST=="mpi jena"]<-"max planck institute for human history"
    DATAFILE$INST[DATAFILE$INST=="national institute of food and agriculture"]<-"usda national institute of food and agriculture"
    DATAFILE$COUNTRY[DATAFILE$INST=="national institute of water and atmospheric research"]<-"New Zealand"
    DATAFILE$COUNTRY[DATAFILE$INST=="national museum of natural history france"]<-"France"
    DATAFILE$INST[DATAFILE$INST=="national museum of natural history smithsonian institution"]<-"smithsonian national museum of natural history"
    DATAFILE$INST[DATAFILE$INST=="national natural history museum paris"]<-"national museum of natural history france"
    DATAFILE$COUNTRY[DATAFILE$INST=="national university of rosario"]<-"Argentina"
    DATAFILE$COUNTRY[DATAFILE$INST=="naturhistorisches museum wien"]<-"Austria"
    DATAFILE$INST[DATAFILE$INST=="nederlands institut voor onderzoek der zee"]<-"royal netherlands institute for sea research"
    DATAFILE$INST[DATAFILE$INST=="nederlands institut voor onderzoek der zee"]<-"royal netherlands institute for sea research"
    DATAFILE$COUNTRY[DATAFILE$INST=="new york botanical garden"]<-"USA"
    DATAFILE$INST[DATAFILE$INST=="newfoundland"]<-"missing"
    DATAFILE$INST[DATAFILE$INST=="nioo"]<-"netherlands institute of ecology"
    # DATAFILE$NOTES[DATAFILE$INST=="no one by this name"]<-"2x if on ed board this year"
    DATAFILE$INST[DATAFILE$INST=="northeast fisheries science center"]<-"noaa northeast fisheries science center"
    DATAFILE$INST[DATAFILE$INST=="oklahoma university"]<-"university of oklahoma"
    DATAFILE$COUNTRY[DATAFILE$INST=="oxford university"]<-"United Kingdom"
    DATAFILE$INST[DATAFILE$INST=="pacific sw research station us forest service"]<-"usfs pacific sw research station"
    DATAFILE$INST[DATAFILE$INST=="prairie wildlife research center"]<-"usgs prairie wildlife research center"
    DATAFILE$COUNTRY[DATAFILE$STATE=="Puerto Rico"]<-"Puerto Rico"
    DATAFILE$INST[DATAFILE$INST=="queens college city university of new york"]<-"cuny queens college"
    DATAFILE$INST[DATAFILE$INST=="sao paulo state university"]<-"universidade estadual paulista"
    DATAFILE$INST[DATAFILE$INST=="retired (no affiliation)"]<-"retired"
    DATAFILE$COUNTRY[DATAFILE$INST=="rice university"]<-"USA"
    DATAFILE$INST[DATAFILE$INST=="riso dtuniversity of national laboratory for sustainable energy"]<-"riso dtu national laboratory for sustainable energy"
    DATAFILE$COUNTRY[DATAFILE$INST=="royal botanic gardens kew"]<-"United Kingdom"
    DATAFILE$INST[DATAFILE$INST=="royal botanic gardens melbourne university of melbourne"]<-"royal botanic gardens melbourne"
    DATAFILE$INST[DATAFILE$LAST_NAME=="Rutzler"]<-"smithsonian national museum of natural history"
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
    DATAFILE$INST[DATAFILE$INST=="swiss federal institute wsl"]<-"swiss federal institute for forest snow and landscape research"
    DATAFILE$UNIT[DATAFILE$INST=="tidewater agricultural res and ext ctr"]<-"tidewater agricultural res and ext ctr"
    DATAFILE$INST[DATAFILE$INST=="tidewater agricultural res and ext ctr"]<-"virginia polytechnic institute and state university"
    DATAFILE$COUNTRY[DATAFILE$INST=="tijuana estuarine research reserve"]<-"USA"
    DATAFILE$INST[DATAFILE$INST=="tulane"]<-"tulane university"
    DATAFILE$INST[DATAFILE$INST=="ufz center for environmental research"]<-"helmholtz centre for environmental research ufz"
    DATAFILE$INST[DATAFILE$INST=="ufz centre for environmental research leipzig halle"]<-"helmholtz centre for environmental research ufz leipzig halle"
    DATAFILE$INST[DATAFILE$INST=="ufz helmholtz ctr environm res"]<-"helmholtz centre for environmental research ufz"
    DATAFILE$INST[DATAFILE$INST=="universidade federal do rio grande do norte (ufrn)"]<-"universidade federal do rio grande do norte"
    DATAFILE$COUNTRY[DATAFILE$INST=="universita degli studi di padova"]<-"Italy"
    DATAFILE$COUNTRY[DATAFILE$INST=="universita di napoli"]<-"Italy"
    DATAFILE$COUNTRY[DATAFILE$INST=="universita di pisa"]<-"Italy"
    DATAFILE$COUNTRY[DATAFILE$INST=="universita die napoli frederico ii"]<-"Italy"
    DATAFILE$COUNTRY[DATAFILE$INST=="universite de lausanne"]<-"Switzerland"
    DATAFILE$COUNTRY[DATAFILE$INST=="universite laval quebec"]<-"Canada"
    DATAFILE$COUNTRY[DATAFILE$INST=="university college dublin"]<-"Ireland"
    DATAFILE$COUNTRY[DATAFILE$INST=="university college london"]<-"United Kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="university marine biological station millport"]<-"United Kingdom"
    DATAFILE$INST[DATAFILE$INST=="university of illinois university urbana champaign"]<-"university of illinois urbana champaign"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of kentucky"]<-"USA"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of leiden"]<-"Netherlands"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of maryland center for environmental science"]<-"USA"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of oregon"]<-"USA"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of oslo"]<-"Norway"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of pennsylvania"]<-"USA"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of puerto rico"]<-"Puerto Rico"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of regina"]<-"Canada"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of sheffield"]<-"United Kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of sherbrooke"]<-"Canada"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of southampton"]<-"United Kingdom"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of stockholm"]<-"Sweden"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of sydney"]<-"Australia"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of texas austin"]<-"USA"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of the sunshine"]<-"Australia"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of toronto"]<-"Canada"
    DATAFILE$INST[DATAFILE$INST=="university of university amsterdam"]<-"university of amsterdam"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of vienna"]<-"Austria"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of waikato"]<-"New Zealand"
    DATAFILE$COUNTRY[DATAFILE$INST=="university of western ontario"]<-"Canada"
    DATAFILE$INST[DATAFILE$INST=="university of zuidema"]<-"university of wageningen"
    DATAFILE$COUNTRY[DATAFILE$INST=="university pompeu fabra"]<-"Spain"
    DATAFILE$COUNTRY[DATAFILE$INST=="university system of maryland"]<-"USA"
    DATAFILE$INST[DATAFILE$INST=="uppsala university"]<-"university of uppsala"
    DATAFILE$INST[DATAFILE$INST=="us arid land agricultural research center"]<-"usda arid land agricultural research center"
    DATAFILE$INST[DATAFILE$INST=="us geological society"]<-"us geological survey"
    DATAFILE$INST[DATAFILE$INST=="virginia commonwealth u"]<-"virginia commonwealth university"
    DATAFILE$INST[DATAFILE$INST=="biological station of donana"]<-"csic donana biological station"
    DATAFILE$INST[DATAFILE$INST=="food and agriculture organization fao"]<-"fao food and agriculture organization"
    DATAFILE$INST[DATAFILE$INST=="imperial college of london"]<-"imperial college london"
    DATAFILE$INST[DATAFILE$INST=="institute¬†of terrestrial ecology"]<-"institute of terrestrial ecology"
    DATAFILE$INST[DATAFILE$INST=="joint nature conservation committee¬†"]<-"joint nature conservation committee"
    DATAFILE$INST[DATAFILE$INST=="king‚Äôs college"]<-"kings college"
    DATAFILE$INST[DATAFILE$INST=="macaulay¬†institute"]<-"macaulay land use research institute"
    DATAFILE$INST[DATAFILE$INST=="ceh"]<-"nerc centre for ecology and hydrology"
    DATAFILE$INST[DATAFILE$INST=="nerc ctr ecol and hydrol"]<-"nerc centre for ecology and hydrology"
    DATAFILE$INST[DATAFILE$INST=="ceh banchory"]<-"nerc centre for ecology and hydrology banchory"
    DATAFILE$INST[DATAFILE$INST=="ceh monks wood"]<-"nerc centre for ecology and hydrology monks wood"
    DATAFILE$INST[DATAFILE$INST=="netherlands inst ecol nioo knaw"]<-"netherlands institute of ecology nioo knaw"
    DATAFILE$INST[DATAFILE$INST=="northeastern univ"]<-"northeastern university"
    DATAFILE$INST[DATAFILE$INST=="western cotton research lab"]<-"now the usda arid land agricultural research center"
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
    
    # TODO: check to see if these are working because it may be an ascii problem
    # university of st andrews	university of st. andrews
    DATAFILE$INST[DATAFILE$INST== "university of floridaz center for environmental research" ]<-"helmholtz centre for environmental research"
    
    DATAFILE$INST<-gsub('[.]','',DATAFILE$INST)  
    # DATAFILE$INST[DATAFILE$INST== "istituto per l¬¢ambiente marino costiero" ]<-"instituto per lambiente marino costiero"
    # DATAFILE$INST[DATAFILE$INST==  ]<-    
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    # DATAFILE$INST[DATAFILE$INST==  ]<-
    DATAFILE$INST_CHECK[is.na(DATAFILE$INST)]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="ulm"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="ulster"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="sydney"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="syracuse"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="tartu"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="tennessee"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="swansea"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="seoul"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="sackville"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="salzburg"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="santiago"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="queensland"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="pretoria"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="peking"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="ohio"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="north carolina"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="montpellier"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="monash"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="montana"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="madrid"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="maine"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="massey"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="london"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="krakow"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="kyoto"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="jena"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="illinois"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="indiana"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="hokkaido"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="guelph"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="dundee"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="dartmouth"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="copenhagen"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="cork"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="pennsylvania"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="philadelphia"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="pittsburgh"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="reading"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="regina"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="saskatchewan"]<-"2x Inst"
    DATAFILE$INST_CHECK[DATAFILE$INST=="seattle wa"]<-"2x Inst"
	
    gsub("goottingen","gottingen",DATAFILE$INST)
    gsub("istituto per looambiente marino costiero","istituto per lambiente marino costiero",DATAFILE$INST)
    gsub("macaulayooinstitute","istituto per lambiente marino costiero",DATAFILE$INST)
    gsub("universitoo de montreal","macaulay land use research institute",DATAFILE$INST)
    DATAFILE$ascii<-NULL
    
    # ALLDATA$validEnc<-validEnc(ALLDATA$COUNTRY)
    # summary(ALLDATA$validEnc<-validEnc(ALLDATA$COUNTRY))
    # foo<-ALLDATA %>% filter(validEnc==FALSE)
    # foo<-distinct(foo, COUNTRY)
    #DONT FORGET - FILTER WILL REMOVE ALL WITH NA in the conditions
    DATAFILE<-DATAFILE %>% replace_na(list(INST = "missing"))
    DATAFILE<-DATAFILE %>% filter(!INST=="no one by this name")
    
    # DATAFILE<-DATAFILE %>% filter(INST==!"no one by this name")
    # DATAFILE<-DATAFILE[!DATAFILE$INST == "no one by this name", ]
    
    
    
return(DATAFILE)

}
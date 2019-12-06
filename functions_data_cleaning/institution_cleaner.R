#FUNCTION TO SYSTEMATIZE UNIVERSITY AND PLACE NAMES
institution_cleaner <- function(DATAFILE) {
  # DATAFILE<-ALLDATA
  # SOME NOTES:
  
  # 
  # ANUTECH is a spin-off company started by ANU
  # BFH: Fed Research Center for Foresty and Forest Products - Abbreviation 
  # BFW: Austrian Research Center for Forests
  # BOKU: University of Natural Resources and Life LifeCycleSavings
  #CSIRO: Commonwealth Scientific and Industrial Research Organization
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
  
DATAFILE$COUNTRY<-as.factor(DATAFILE$COUNTRY)
levels(DATAFILE$COUNTRY)
DATAFILE$COUNTRY<-as.character(DATAFILE$COUNTRY)
DATAFILE$COUNTRY[DATAFILE$COUNTRY=="Australiatralia"]<-"Australia"
DATAFILE$COUNTRY[DATAFILE$COUNTRY=="MEXICO"]<-"Mexico"
DATAFILE$COUNTRY[DATAFILE$COUNTRY=="NewZealand"]<-"New Zealand"
DATAFILE$COUNTRY[DATAFILE$COUNTRY=="PuertoRico"]<-"Puerto Rico"
DATAFILE$COUNTRY[DATAFILE$COUNTRY=="The Netherlands"]<-"Netherlands"
DATAFILE$COUNTRY[DATAFILE$COUNTRY=="UK"]<-"United Kingdom"
DATAFILE$COUNTRY[DATAFILE$COUNTRY=="US"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$COUNTRY=="Usa"]<-"USA"
DATAFILE$COUNTRY[DATAFILE$COUNTRY=="United States"]<-"USA"
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
levels(DATAFILE$COUNTRY) <- c(levels(DATAFILE$COUNTRY),"Russia")
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
DATAFILE$INST<-as.character(DATAFILE$INST)
DATAFILE$INST<-trimws(DATAFILE$INST, which = "left")
DATAFILE$INST<-trimws(DATAFILE$INST, which = "right")
DATAFILE$INST<-gsub("- ", "-", DATAFILE$INST)
DATAFILE$INST<-gsub(" -", "-", DATAFILE$INST)
DATAFILE$INST<-gsub(" - ", "-", DATAFILE$INST)
DATAFILE$INST<-gsub(" at ", "-", DATAFILE$INST)
DATAFILE$INST<-gsub("The ", "", DATAFILE$INST)
DATAFILE$INST<-gsub("Unniversity","University",DATAFILE$INST)
DATAFILE$INST<-gsub("Museum Natl Hist Nat","National History Museum Paris",DATAFILE$INST)
DATAFILE$INST<-gsub("University<ca>of<ca>California<ca>Santa<ca>Cruz","University of California Santa Cruz",DATAFILE$INST)
DATAFILE$INST<-gsub("Uc Santa Cruz","University of California Santa Cruz",DATAFILE$INST)
# TODO: gsub ArKansas one is not working inside the function and I have no idea why.
DATAFILE$INST<-gsub("Arkansas","Arkansas", DATAFILE$INST,ignore.case=TRUE)
DATAFILE$INST<-gsub("UVA","University of Virginia",DATAFILE$INST)
DATAFILE$INST<-gsub("US FOREST SERV","US Forest Service",DATAFILE$INST)
DATAFILE$INST<-gsub("LSU","Louisiana State University",DATAFILE$INST)
DATAFILE$INST<-gsub("double check","DOUBLE CHECK",DATAFILE$INST)
DATAFILE$INST<-gsub("N.C. State","North Carolina State University",DATAFILE$INST)
DATAFILE$INST<-gsub("UC Davis","University of California Davis",DATAFILE$INST)
DATAFILE$INST<-gsub("UF","University of Florida",DATAFILE$INST)
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
DATAFILE$INST<-gsub("Universrity", "University", DATAFILE$INST)
DATAFILE$INST<-gsub("Canadian Forestry", "Canadian Forest", DATAFILE$INST)
DATAFILE$INST<-gsub("Mighican", "Michigan", DATAFILE$INST)
DATAFILE$INST<-gsub("de Montpellier II", "Montpellier II", DATAFILE$INST)
DATAFILE$INST<-gsub("North Arizona", "Northern Arizona", DATAFILE$INST)
DATAFILE$INST<-gsub("Wsl", "WSL", DATAFILE$INST)
DATAFILE$INST<-gsub("Alabama in", "Alabama", DATAFILE$INST)
DATAFILE$INST<-gsub("Fonctionelle", "Fonctionnelle", DATAFILE$INST)
DATAFILE$INST<-gsub("-CNRS", "", DATAFILE$INST)
DATAFILE$INST<-gsub("University of Sherbooke", "Universite de Sherbooke", DATAFILE$INST)
DATAFILE$INST<-gsub("Smithsonian Institute", "Smithsonian Institution", DATAFILE$INST)
DATAFILE$INST<-gsub("Minnestoa", "Minnesota", DATAFILE$INST)
DATAFILE$INST<-gsub("University of Lausanne", "Universite de Lausanne", DATAFILE$INST)
DATAFILE$INST<-gsub("Institute of Ecosystem Studies", "Cary Institute of Ecosystem Studies", DATAFILE$INST)
DATAFILE$INST<-gsub("Environment Bangalore", "Environment", DATAFILE$INST)

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
DATAFILE$INST[DATAFILE$INST=="WSL Swiss Federal Research Institute"]<-"Swiss Federal Research Institute WSL"
DATAFILE$UNIT[DATAFILE$INST=="Pacific S.W. Research Station-US Forest Service"]<-"Pacific S.W. Research Station"
DATAFILE$INST[DATAFILE$INST=="Pacific S.W. Research Station-US Forest Service"]<-"US Forest Service"
DATAFILE$INST[DATAFILE$INST=="University fo California Berkeley"]<-"University of California-Berkeley"
DATAFILE$INST[DATAFILE$INST=="US Geological survey"]<-"US Geological Survey"
DATAFILE$INST[DATAFILE$INST=="Cal State Bakersfield"]<-"California State University-Bakersfield"
DATAFILE$INST[DATAFILE$INST=="Universityof Wisconsin-Milwaukee"]<-"University of Wisconsin-Milwaukee"
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
DATAFILE$INST[DATAFILE$INST=="Miami U"]<-"Miami University"
DATAFILE$INST[DATAFILE$INST=="Montana State"]<-"Montana State University"
DATAFILE$INST[DATAFILE$INST=="North Dakota State"]<-"North Dakota State University"
DATAFILE$INST[DATAFILE$INST=="Ohio State"]<-"Ohio State University"
DATAFILE$INST[DATAFILE$INST=="Oregon State"]<-"Oregon State University"
DATAFILE$INST[DATAFILE$INST=="Rensselaer Poly"]<-"Rensselaer Polytechnic Institute"
DATAFILE$INST[DATAFILE$INST=="Rutgers U"]<-"Rutgers University"
DATAFILE$INST[DATAFILE$INST=="Stanford"]<-"Stanford University"
DATAFILE$INST[DATAFILE$INST=="State University of New York at Binghamton"]<-"SUNY at Binghamton"
DATAFILE$INST[DATAFILE$INST=="Binghamton University-SUNY"]<-"SUNY at Binghamton"
DATAFILE$INST[DATAFILE$INST=="Texas State"]<-"Texas State University"
DATAFILE$INST[DATAFILE$INST=="The University of Southwestern Louisiana"]<-"University of Southwestern Louisiana"
DATAFILE$INST[DATAFILE$INST=="U Arizona"]<-"University of Arizona"
DATAFILE$INST[DATAFILE$INST=="University of California Irvine"]<-"University of California-Irvine"
DATAFILE$INST[DATAFILE$INST=="University of California Riverside"]<-"University of California-Riverside"
DATAFILE$INST[DATAFILE$INST=="California State University of San Marcos"]<-"California State University-San Marcos"
DATAFILE$INST[DATAFILE$INST=="University of California at San Diego"]<-"University of California-San Diego"
DATAFILE$INST[DATAFILE$INST=="University of California Santa Cruz"]<-"University of California-Santa Cruz"
DATAFILE$INST[DATAFILE$INST=="UC Santa Cruz"]<-"University of California-Santa Cruz"
DATAFILE$INST[DATAFILE$INST=="Uc Merced, University of California"]<-"University of California-Merced"
DATAFILE$INST[DATAFILE$INST=="UMSL"]<-"University of Missouri-St. Louis"
DATAFILE$INST[DATAFILE$INST=="University of Carolina Aiken"]<-"University of South Carolina-Aiken"
DATAFILE$INST[DATAFILE$INST=="University of Massachusetts Boston"]<-"University of Massachusetts-Boston"
DATAFILE$INST[DATAFILE$INST=="University of Nevada Las Vegas"]<-"University of Nevada-Las Vegas"
DATAFILE$INST[DATAFILE$INST=="University of Texas Austin"]<-"University of Texas at Austin"
DATAFILE$INST[DATAFILE$INST=="Vanderbilt"]<-"Vanderbilt University"
DATAFILE$INST[DATAFILE$INST=="Virginia Commonweath U"]<-"Virginia Commonweath University"
DATAFILE$INST[DATAFILE$INST=="Washington State"]<-"Washington State University"
DATAFILE$INST[DATAFILE$INST=="Washington U"]<-"Washington University"
DATAFILE$INST[DATAFILE$INST=="Willamette U"]<-"Willamette University"
DATAFILE$INST[DATAFILE$INST=="University of Pennsylvania, "]<-"University of Pennsylvania"
DATAFILE$INST[DATAFILE$INST=="University of Pittsburg"]<-"University of Pittsburgh"
DATAFILE$INST[DATAFILE$INST=="University of Tennesee"]<-"University of Tennessee"
DATAFILE$INST[DATAFILE$INST=="Acad Natural Science Philadelphia"]<-"Academy of Natural Science Philadelphia"
DATAFILE$INST[DATAFILE$INST=="Am Mus Nat Hist"]<-"American Museum of Natural History"
DATAFILE$INST[DATAFILE$INST=="ColoradoState Univ"]<-"Colorado State University"
DATAFILE$INST[DATAFILE$INST=="Evergreen State College Washington"]<-"Evergreen State College"
DATAFILE$INST[DATAFILE$INST=="Filed Museum of Natural History"]<-"Field Museum Of Natural History"
DATAFILE$INST[DATAFILE$INST=="George Mason University Virginia"]<-"George Mason University"
DATAFILE$INST[DATAFILE$INST=="Insitute of Ecosystem Studies"]<-"Institute of Ecosystem Studies"
DATAFILE$INST[DATAFILE$INST=="Kenyon College Ohio"]<-"Kenyon College"
DATAFILE$INST[DATAFILE$INST=="Louisiana State Univeristy"]<-"Louisiana State University"
DATAFILE$INST[DATAFILE$INST=="Milwaukee Public Mus"]<-"Milwaukee Public Museum"
DATAFILE$INST[DATAFILE$INST=="Missouri Botanical Gardens"]<-"Missouri Botanical Garden"
DATAFILE$INST[DATAFILE$INST=="Oklahoma Museum of Natural History"]<-"Oklahoma Mus Nat His"
DATAFILE$INST[DATAFILE$INST=="Smithsonian(STRI)"]<-"Smithsonian Tropical Research Institute"
DATAFILE$INST[DATAFILE$INST=="STRI Smithsonian"]<-"Smithsonian Tropical Research Institute"
DATAFILE$INST[DATAFILE$INST=="U Connecticut"]<-"University of Connecticut"
DATAFILE$INST[DATAFILE$INST=="University of Alabama Huntsville"]<-"University of Alabama in Huntsville"
DATAFILE$INST[DATAFILE$INST=="University of Connetticut Storrs"]<-"University of Connecticut"
DATAFILE$INST[DATAFILE$INST=="University of Michgan"]<-"University of Michigan"
DATAFILE$INST[DATAFILE$INST=="University of Missouri St Louis"]<-"University of Missouri-St. Louis"
DATAFILE$INST[DATAFILE$INST=="University of Missouri_St Louis"]<-"University of Missouri-St. Louis"
DATAFILE$INST[DATAFILE$INST=="University of Wisconsin Milwaukee"]<-"University of Wisconsin-Milwaukee"
DATAFILE$INST[DATAFILE$INST=="U California Irvine"]<-"University of California-Irvine"
DATAFILE$INST[DATAFILE$INST=="U California Riverside"]<-"University of California-Riverside"
DATAFILE$INST[DATAFILE$INST=="University of California at San Diego"]<-"University of California-San Diego"
DATAFILE$INST[DATAFILE$INST=="U California Santa Barbara"]<-"University of California-Santa Barbara"
DATAFILE$INST[DATAFILE$INST=="U California Santa Cruz"]<-"University of California-Santa Cruz"
DATAFILE$INST[DATAFILE$INST=="University of California Los Angeles"]<-"University of California-Los Angeles"
DATAFILE$INST[DATAFILE$INST=="University of California Berkeley"]<-"University of California-Berkeley"
DATAFILE$INST[DATAFILE$INST=="University of California Davis"]<-"University of California-Davis"
DATAFILE$INST[DATAFILE$INST=="University of California Santa Barbara"]<-"University of California-Santa Barbara"
DATAFILE$INST[DATAFILE$INST=="University of California San Diego"]<-"University of California-San Diego"
DATAFILE$INST[DATAFILE$INST=="University of California San Diego"]<-"University of California-San Diego"
DATAFILE$INST[DATAFILE$INST=="Rensselaer Poly"]<-"Rensselaer Polytechnic Institute"
DATAFILE$INST[DATAFILE$INST=="Oklahoma Mus Nat His"]<-"Oklahoma Museum of Natural History"
DATAFILE$INST[DATAFILE$INST=="US Forest Service Pacific Southwest Research Station"]<-"USFS-Pacific Southwest Research Station"
DATAFILE$INST[DATAFILE$INST=="U.S.F.W.S. National Wetlands Research Center"]<-"USFWS-National Wetlands Research Center"
DATAFILE$INST[DATAFILE$INST=="Arizona State University West"]<-"Arizona State University-West Campus"
DATAFILE$INST[DATAFILE$INST=="Naval Research Laboratory DC"]<-"US Naval Research Laboratory"
DATAFILE$INST[DATAFILE$INST=="Temple-Inland Forest"]<-"Temple-Inland Forest Products"
DATAFILE$INST[DATAFILE$INST=="USDA Forest Service"]<-"US Forest Service"
DATAFILE$INST[DATAFILE$INST=="USDA Forest Service Rocky Mountain Research Station"]<-"US Forest Service-Rocky Mountain Research Station"
DATAFILE$INST[DATAFILE$INST=="USDA Forest Service Southern Research Station"]<-"US Forest Service-Southern Research Station"
DATAFILE$INST[DATAFILE$INST=="USFS-Pacific Southwest Research Station"]<-"US Forest Service-Pacific Southwest Research Station"
DATAFILE$INST[DATAFILE$INST=="USFWS-National Wetlands Research Center"]<-"US Fish & Wildlife Service-National Wetlands Research Center"
DATAFILE$UNIT[DATAFILE$INST=="USGS Forest and Rangeland Ecosystem Science Center"]<-"Forest and Rangeland Ecosystem Science Center"
DATAFILE$INST[DATAFILE$INST=="USGS Forest and Rangeland Ecosystem Science Center"]<-"US Geological Survey"
DATAFILE$UNIT[DATAFILE$INST=="USGS-Northern Prairie Wildlife Research Center"]<-"Northern Prairie Wildlife Research Center"
DATAFILE$INST[DATAFILE$INST=="USGS-Northern Prairie Wildlife Research Center"]<-"US Geological Survey"
DATAFILE$INST[DATAFILE$INST=="National Museum of Natural History"]<-"Smithsonian National Museum of Natural History"
DATAFILE$INST[DATAFILE$INST=="Smithsonian"]<-"Smithsonian National Museum of Natural History" #EB Verified
DATAFILE$INST[DATAFILE$INST=="USDA"]<-"US Department of Agriculture"
DATAFILE$UNIT[DATAFILE$INST=="USDA Cooperative State Research, Education and Extension Service (CSREES)"]<-"Cooperative State Research, Education, and Extension Service"
DATAFILE$INST[DATAFILE$INST=="USDA Cooperative State Research, Education and Extension Service (CSREES)"]<-"US Department of Agriculture"
DATAFILE$INST[DATAFILE$INST=="Virginia Tech"]<-"Virginia Polytechnic Institute and State University"
DATAFILE$INST[DATAFILE$INST=="University of Pennsylvania,"]<-"University of Pennsylvania"
DATAFILE$INST[DATAFILE$INST=="Division of Reptiles and Amphibians Smithsonian Institute Washington DC"]<-"Smithsonian National Museum of Natural History"
DATAFILE$INST[DATAFILE$INST=="University of California-At Los Angeles"]<-"University of California-Los Angeles"
DATAFILE$INST[DATAFILE$INST=="National Museum of Natural History, Smithsonian Institution"]<-"Smithsonian National Museum of Natural History"
DATAFILE$UNIT[DATAFILE$INST=="Citrus Research and Education Center"]<-"Citrus Research and Education Center"
DATAFILE$INST[DATAFILE$INST=="Citrus Research and Education Center"]<-"University of Florida"
DATAFILE$INST[DATAFILE$INST=="Ohio u"]<-"Ohio University-Main Campus"
DATAFILE$INST[DATAFILE$INST=="University of South Carolina" & DATAFILE$CITY=="Columbia"]<-"University of South Carolina-Columbia"
DATAFILE$UNIT[DATAFILE$INST=="US Department of Agriculture Cooperative State Research, Education, and Extension Service"]<-"Cooperative State Research, Education, and Extension Service"
DATAFILE$INST[DATAFILE$INST=="US Department of Agriculture Cooperative State Research, Education, and Extension Service"]<-"US Department of Agriculture"
DATAFILE$INST[DATAFILE$LAST_NAME=="Bowers" & DATAFILE$CITY=="Vadnais Heights"]<-"Calyx, Inc."
DATAFILE$INST[DATAFILE$INST=="Arnold Arboretum Of Harvard University"]<-"Harvard University Arnold Arboretum"
DATAFILE$INST[DATAFILE$INST=="Museum of Comparative Zoology"]<-"Harvard University Museum of Comparative Zoology"
DATAFILE$INST[DATAFILE$INST=="SUNY"]<-"State University of New York"
DATAFILE$INST[DATAFILE$INST=="State University of New York at Stony Brook"]<-"State University of New York-Stony Brook"
DATAFILE$INST[DATAFILE$INST=="State University of New York Syracuse"]<-"State University of New York-Syracuse"
DATAFILE$INST[DATAFILE$INST=="Arnold Arboretum of Harvard University"]<-"Harvard University Arnold Arboretum"
DATAFILE$INST[DATAFILE$INST=="Harvard Medical School"]<-"Harvard University Medical School"
DATAFILE$INST[DATAFILE$INST=="Australian Research Center for Urban "]<-"Smithsonian Tropical Research Institute"
DATAFILE$INST<-gsub(" CATIE","", DATAFILE$INST)
DATAFILE$INST<-gsub("CATIE ","", DATAFILE$INST)
DATAFILE$INST<-gsub(" (UNESP)","", DATAFILE$INST)
DATAFILE$INST<-gsub("Darwin University Darwin","Darwin University", DATAFILE$INST)
DATAFILE$INST<-gsub("Technion-","", DATAFILE$INST)
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
DATAFILE$INST<-gsub("National Museum of Natural History Paris","Museum National dHistoire Naturelle", DATAFILE$INST)
DATAFILE$INST<-gsub("Naturelle Naturelle","Naturelle", DATAFILE$INST)
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
DATAFILE$INST[DATAFILE$INST=="Museum dHistoire Naturelle"]<-"Museum National dHistoire Naturelle"
DATAFILE$INST[DATAFILE$INST=="National Autonomous university of Mexico"]<-"Universidad Nacional Autonoma de Mexico"
DATAFILE$INST[DATAFILE$INST=="Landcare Research"]<-"Manaaki Whenua Landcare Research"
DATAFILE$UNIT[DATAFILE$INST=="USGS/NRII"]<-"NRII" 
DATAFILE$INST[DATAFILE$INST=="USGS/NRII"]<-"US Geological Survey"
DATAFILE$UNIT[DATAFILE$INST=="Florida International University and Center for Tropical Plant Conservation"]<-"Center for Tropical Plant Conservation" 
DATAFILE$INST[DATAFILE$INST=="Florida International University and Center for Tropical Plant Conservation"]<-"Florida International University" 
DATAFILE$INST[DATAFILE$INST=="Ecole Normale Supérieure"]<-"Ecole Normale Superieure" 
DATAFILE$INST[DATAFILE$INST=="Université de Sherbrooke"]<-"Universite de Sherbrooke"
DATAFILE$INST[DATAFILE$INST=="Pontificia Universidad Católica de Chile"]<-"Pontificia Universidad Catolica de Chile" 
DATAFILE$INST[DATAFILE$INST=="University of Tromsø"]<-"University of Tromso" 
DATAFILE$INST[DATAFILE$INST=="Universit\xfc\xbe\x8c\xa3\xa0\xbc Montpellier II"]<-"Universite Montpellier II" 
DATAFILE$INST[DATAFILE$INST=="Universit\xfc\xbe\x8d\x83\xa0\xbct Z\xfc\xbe\x8c\x93\xa0\xbcrich Irchel"]<-"University of Zurich Irchel" 
DATAFILE$INST[DATAFILE$INST=="Texas A & M Univ."]<-"Texas A & M University"
DATAFILE$INST[DATAFILE$INST=="Texas A & M"]<-"Texas A & M University"
DATAFILE$INST[DATAFILE$INST=="Aberdeen"]<-"University of Aberdeen"
DATAFILE$INST[DATAFILE$INST=="Universityof"]<-"University of"
DATAFILE$INST[DATAFILE$INST=="Aberystwyth"]<-"Aberystwyth University"
DATAFILE$INST[DATAFILE$INST=="Alabama A and M"]<-"Alabama A and M University"
DATAFILE$INST[DATAFILE$INST=="Alterra"]<-"Wageningen University and Research"
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
DATAFILE$INST[DATAFILE$INST=="Gatty Marine Lab (University of Saint Andrews"]<-"University of Saint Andrews"
DATAFILE$INST[DATAFILE$INST=="Georg August Universitat Gottingen"]<-"University of Gottingen"
DATAFILE$INST[DATAFILE$INST=="Glasgow"]<-"University of Glasgow"
DATAFILE$INST[DATAFILE$INST=="Gottingen University"]<-"University of Gottingen"
DATAFILE$INST[DATAFILE$INST=="Helsinki"]<-"University of Helsinki"
DATAFILE$INST[DATAFILE$INST=="Humboldt Universitat zu Berlin"]<-"Humboldt University of Berlin"
DATAFILE$INST[DATAFILE$INST=="Humboldt University"]<-"Humboldt University of Berlin"
DATAFILE$INST[DATAFILE$INST=="Institut Mediterrani dEstudis Avencats (CSIC UIB)"]<-"CSIC Institut Mediterrani dEstudis Avencats"
DATAFILE$INST[DATAFILE$INST=="Instituto de Ciencias del Mar CSIC"]<-"Instituto de Ciencias del Mar CSIC"
DATAFILE$INST[DATAFILE$INST=="Istituto per l¬¢Ambiente Marino Costiero"]<-"Istituto per lAmbiente Marino Costiero"
DATAFILE$INST[DATAFILE$INST=="IZW"]<-"Leibniz Institute for Zoo and Wildlife Research"
DATAFILE$INST[DATAFILE$INST=="Kansas State"]<-"Kansas State University"
DATAFILE$INST[DATAFILE$INST=="Karlsruhe"]<-"Karlsruhe Institute of Technology"
DATAFILE$INST[DATAFILE$INST=="Khorasan Agricultural and Natural Resources Res. Ctr"]<-"Khorasan Agricultural and Natural Resources Research Center"
DATAFILE$INST[DATAFILE$INST=="Kyushu Unniversity"]<-"Kyushu University"
DATAFILE$INST[DATAFILE$INST=="Lehman College CUNY"]<-"CUNY Lehman College"
DATAFILE$INST[DATAFILE$INST=="Los Alamos National Lab"]<-"Los Alamos National Laboratory"
DATAFILE$INST[DATAFILE$INST=="Ludwig Maximilians Universitat Munchen"]<-"Ludwig Maximilians Universitat Munchen"
DATAFILE$INST[DATAFILE$INST=="Lyme Regis"]<-""
DATAFILE$INST[DATAFILE$INST=="Macaulay¬†Institute"]<-"Macaulay Land Use Research Institute"
DATAFILE$INST[DATAFILE$INST=="Max Planck Inst Plant Breeding Res"]<-"Max Planck Institute for Plant Breeding Research"
DATAFILE$INST[DATAFILE$INST=="Max Planck Institut fur Verhaltensphysiologie"]<-"Max Planck Institute for Behavioral Physiology"
DATAFILE$INST[DATAFILE$INST=="Max Planck Institute Jena"]<-"Max Planck Institute for the Science of Human History"
DATAFILE$INST[DATAFILE$INST=="Monterey Bay Aquarium Research Institute (MBARI)"]<-"Monterey Bay Aquarium Research Institute"
DATAFILE$INST[DATAFILE$INST=="Mt Holyoke College"]<-"Mount Holyoke College"
DATAFILE$INST[DATAFILE$INST=="Museum National dHistoire Naturelle"]<-"National History Museum Paris"
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
DATAFILE$INST[DATAFILE$INST=="Scripps Institute of Oceanography"]<-"University of California Santa Diego"
DATAFILE$INST[DATAFILE$INST=="Scripps Institution  of Oceanography"]<-"University of California Santa Diego"
DATAFILE$INST[DATAFILE$INST=="Sheffield"]<-"University of Sheffield"
DATAFILE$INST[DATAFILE$INST=="SMITHSONIAN TROP RES INST"]<-"Smithsonian Tropical Research Institute"
DATAFILE$INST[DATAFILE$INST=="St. Andrews"]<-"University of St. Andrews"
DATAFILE$INST[DATAFILE$INST=="State University of New York"]<-"SUNY"
DATAFILE$INST[DATAFILE$INST=="Suny Albany"]<-"SUNY Albany"
DATAFILE$INST[DATAFILE$INST=="York" & (DATAFILE$COUNTRY=="UK" | DATAFILE$COUNTRY=="United Kingdom" | DATAFILE$COUNTRY=="ENGLAND")]<-"University of York"
DATAFILE$INST[DATAFILE$INST=="York" & DATAFILE$COUNTRY=="Canada"]<-"York University"
DATAFILE$INST[DATAFILE$INST=="York U"]<-"York University"
DATAFILE$INST[DATAFILE$INST=="Suny Stonybrook"]<-"SUNY Stony Brook"
DATAFILE$INST[DATAFILE$INST=="Texas A & M University"]<-"Texas A and M University"
DATAFILE$INST[DATAFILE$INST=="Texas Tech"]<-"Texas Tech University"
DATAFILE$INST[DATAFILE$INST=="U.S.F.W.S. National Ecology Research Center"]<-"USFWS National Ecology Research Center"
DATAFILE$INST[DATAFILE$INST=="U.S.F.W.S. Northern Prairie Wildlife Research Center"]<-"USFWS Northern Prairie Wildlife Research Center"
DATAFILE$INST[DATAFILE$INST=="UNAM"]<-"Universidad Nacional Autonoma de Mexico"
DATAFILE$INST[DATAFILE$INST=="University Buffalo"]<-"University of Buffalo"
DATAFILE$INST[DATAFILE$INST=="University College of ABERYSTWYTH"]<-"Aberystwyth University"
DATAFILE$INST[DATAFILE$INST=="University of ArKansas Little Rock"]<-"University of Arkansas Little Rock"
DATAFILE$INST[DATAFILE$INST=="University of Colorado"]<-"University of Colorado Boulder"
DATAFILE$INST[DATAFILE$INST=="University of Durham"]<-"Durham University"
DATAFILE$INST[DATAFILE$INST=="University of Nebranska"]<-"University of Nebraska"
DATAFILE$INST[DATAFILE$INST=="University of Saint Andrews"]<-"University of St. Andrews"
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
DATAFILE$INST[DATAFILE$INST=="Lehman College"]<-"Lehman College CUNY"
DATAFILE$INST[DATAFILE$INST=="Mississippi State Univ."]<-"Mississippi State University"
DATAFILE$INST[DATAFILE$INST=="Monks Wood"]<-"Monks Wood Experiment Station"
DATAFILE$INST[DATAFILE$INST=="Oklahoma State"]<-"Oklahoma State University"
DATAFILE$INST[DATAFILE$INST=="Syngenta Crop Protection Inc"]<-"Syngenta Crop Protection Inc."
DATAFILE$INST[DATAFILE$INST=="Texas Tech"]<-"Texas Tech University"
DATAFILE$INST[DATAFILE$INST=="Universidad National Autonoma de Mexico"]<-"Universidad Nacional Autonoma de Mexico"
DATAFILE$INST[DATAFILE$INST=="Universidade Federal De Alagoas"]<-"Universidade Federal de Alagoas"
DATAFILE$INST[DATAFILE$INST=="Universit√© de Montreal"]<-"Universite de Montreal"
DATAFILE$INST[DATAFILE$INST=="Universit√© Joseph Fourier"]<-"Universite Joseph Fourier"
DATAFILE$INST[DATAFILE$INST=="Universite Pierre and Marie Curie"]<-"Universite Pierre Et Marie Curie"
DATAFILE$INST[DATAFILE$INST=="Uc Santa Cruz"]<-"University of California Santa Cruz"
DATAFILE$INST[DATAFILE$INST=="University of Edinburhg"]<-"University of Edinburgh"
DATAFILE$INST[DATAFILE$INST=="University of Leuvenn"]<-"University of Leuven"
DATAFILE$INST[DATAFILE$INST=="University of Los Andes Bogota"]<-"University of Los Andes"
DATAFILE$INST[DATAFILE$INST=="University of Missisippi"]<-"University of Mississippi"
DATAFILE$INST[DATAFILE$INST=="University of Montanna"]<-"University of Montana"
DATAFILE$INST[DATAFILE$INST=="Unsw"]<-"University of New South Wales"
DATAFILE$INST[DATAFILE$INST=="Mammal Research Institute (University of Pretoria)"]<-"University of Pretoria"
DATAFILE$INST[DATAFILE$INST=="Gatty Marine Lab (University of Saint Andrews)"]<-"University of Saint Andrews"
DATAFILE$INST[DATAFILE$INST=="University of St Andrews"]<-"University of St. Andrews"
DATAFILE$INST[DATAFILE$INST=="University of Stirlinng"]<-"University of Stirling"
DATAFILE$INST[DATAFILE$INST=="University of Tenessee"]<-"University of Tennessee"
DATAFILE$INST[DATAFILE$INST=="University of Texas At Austin"]<-"University of Texas Austin"
DATAFILE$INST[DATAFILE$INST=="University of Wisconsin"]<-"University of Wisconsin"
DATAFILE$INST[DATAFILE$INST=="Victorian School of forestry"]<-"Victorian School of Forestry"
DATAFILE$INST[DATAFILE$INST=="VRije Universiteit Amsterdam"]<-"Vrije Universiteit Amsterdam"
DATAFILE$INST[DATAFILE$INST=="Washington U St Louis"]<-"Washington University"
DATAFILE$INST[DATAFILE$INST=="Western Cotton Research Lab."]<-"Western Cotton Research Lab"
DATAFILE$INST[DATAFILE$INST=="Western Michigan University"]<-"Western Michigan University"
DATAFILE$INST[DATAFILE$INST=="Woods Hole"]<-"Woods Hole Oceanographic Institution"
DATAFILE$INST[DATAFILE$INST=="WWF"]<-"World Wildlife Fund"
DATAFILE$UNIT[DATAFILE$INST=="University of Saint Andrews"]<-"Gatty Marine Lab"
DATAFILE$UNIT[DATAFILE$INST=="University of Pretoria"]<-"Mammal Research Institute"
DATAFILE$COUNTRY[DATAFILE$INST=="Iowa State University"]<-"USA"
##############################################################
##############################################################
# Dividing Some Names for INST into INST and UNIT
##############################################################
##############################################################

DATAFILE$UNIT[DATAFILE$INST=="Harvard University Herbaria"]<-"Herbaria" 
DATAFILE$INST[DATAFILE$INST=="Harvard University Herbaria"]<-"Harvard University"

DATAFILE$UNIT[DATAFILE$INST=="Harvard University Medical School"]<-"Medical School"
DATAFILE$INST[DATAFILE$INST=="Harvard University Medical School"]<-"Harvard University"

DATAFILE$UNIT[DATAFILE$INST=="Boston Unveristy Marine Program"]<-"Marine Program"
DATAFILE$INST[DATAFILE$INST=="Boston Unveristy Marine Program"]<-"Boston Unversity" 

DATAFILE$UNIT[DATAFILE$INST=="Harvard University Museum of Comparative Zoology"]<-"Museum of Comparative Zoology"
DATAFILE$INST[DATAFILE$INST=="Harvard University Museum of Comparative Zoology"]<-"Harvard University" 

DATAFILE$UNIT[DATAFILE$INST=="Harvard Forest"]<-"Harvard Forest"
DATAFILE$INST[DATAFILE$INST=="Harvard Forest"]<-"Harvard University" 

DATAFILE$UNIT[DATAFILE$INST=="Harvard University Arnold Arboretum"]<-"Arnold Arboretum"
DATAFILE$INST[DATAFILE$INST=="Harvard University"]<-"Harvard University" 

DATAFILE$UNIT[DATAFILE$INST=="Instituto de Ecologia UNAM"]<-"Instituto de Ecologia"
DATAFILE$INST[DATAFILE$INST=="Instituto de Ecologia UNAM"]<-"Universidad Nacional Autonoma de Mexico" 

DATAFILE$UNIT[DATAFILE$INST=="Harvard University Museum of Comparative Zoology"]<-"Museum of Comparative Zoology"
DATAFILE$INST[DATAFILE$INST=="Harvard University Museum of Comparative Zoology"]<-"Harvard University" 

DATAFILE$UNIT[DATAFILE$INST=="Wageiningen University Research Center Alterra"]<-"Research Center Alterra"
DATAFILE$INST[DATAFILE$INST=="Wageiningen University Research Center Alterra"]<-"Wageiningen University" 
return(DATAFILE)

}
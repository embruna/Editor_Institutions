#FUNCTION TO SYSTEMATIZE UNIVERSITY AND PLACE NAMES
country_cleaner <- function(DATAFILE) {


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
DATAFILE$COUNTRY[DATAFILE$FIRST_NAME=="Jeannine" & DATAFILE$LAST_NAME=="Cavender-Bares"]<-"USA"

##############################################################
# COrrecting the country in whihc an Editor is based
##############################################################

DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="Bieber"]<-"Austria"


DATAFILE$COUNTRY<-as.factor(DATAFILE$COUNTRY)
DATAFILE<-droplevels(DATAFILE)
levels(DATAFILE$COUNTRY)


##############################################################
# STILL TO DO 
##############################################################

# NEED TO CONFIRM WHAT PART OF USSR IN WHICH THE AUTHOR WAS BASED
levels(DATAFILE$COUNTRY) <- c(levels(DATAFILE$COUNTRY),"Russia")
DATAFILE$COUNTRY[DATAFILE$COUNTRY=="USSR"]<-"Russia"

# NEED DO DELETE THESE
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

DATAFILE$INST[DATAFILE$JOURNAL=="AMNAT" & DATAFILE$LAST_NAME=="Case"]<-"University of California San Diego"
DATAFILE$INST[DATAFILE$LAST_NAME=="Noon"]<-"Colorado State University"
DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="VanDerHeijden"]<-"Switzerland"

levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"State University of New York College of Environmental Science and Forestry")
DATAFILE$INST[DATAFILE$LAST_NAME=="Burgess"]<-"State University of New York College of Environmental Science and Forestry"
DATAFILE$INST[DATAFILE$LAST_NAME=="Fragoso"]<-"State University of New York College of Environmental Science and Forestry"
DATAFILE$INST[DATAFILE$LAST_NAME=="Yanai"]<-"State University of New York College of Environmental Science and Forestry"
DATAFILE$INST[DATAFILE$LAST_NAME=="Hall" & DATAFILE$FIRST_NAME=="Charles" & DATAFILE$JOURNAL=="CONBIO"]<-"State University of New York College of Environmental Science and Forestry"

levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"Forestry and Forest Products Research Institute","University of Minnesota Duluth","University of Minnesota Crookston")
DATAFILE$INST[DATAFILE$LAST_NAME=="Fujimori"]<-"Forestry and Forest Products Research Institute"
DATAFILE$INST[DATAFILE$LAST_NAME=="Johnson" & DATAFILE$FIRST_NAME=="Lucinda"]<-"University of Minnesota Duluth"
DATAFILE$INST[DATAFILE$LAST_NAME=="Moen" & DATAFILE$FIRST_NAME=="Ron"]<-"University of Minnesota Duluth"
DATAFILE$INST[DATAFILE$LAST_NAME=="Sterner" & DATAFILE$FIRST_NAME=="Robert"]<-"University of Minnesota Duluth"
DATAFILE$INST[DATAFILE$LAST_NAME=="Wiersma" & DATAFILE$FIRST_NAME=="Jochum"]<-"University of Minnesota Crookston"
DATAFILE$INST[DATAFILE$LAST_NAME=="Smith" & DATAFILE$FIRST_NAME=="Madeleine"]<-"University of Minnesota Crookston"
DATAFILE$INST[DATAFILE$LAST_NAME=="Sims" & DATAFILE$FIRST_NAME=="Albert"]<-"University of Minnesota Crookston"

levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"University of Toronto Mississauga")
DATAFILE$INST[DATAFILE$LAST_NAME=="Sprules" & DATAFILE$FIRST_NAME=="Gary"]<-"University of Toronto Mississauga"
DATAFILE$INST[DATAFILE$LAST_NAME=="Wagner" & DATAFILE$FIRST_NAME=="Helene"]<-"University of Toronto Mississauga"
DATAFILE$INST[DATAFILE$LAST_NAME=="Kotanen" & DATAFILE$FIRST_NAME=="Peter"]<-"University of Toronto Mississauga"
DATAFILE$INST[DATAFILE$LAST_NAME=="Loiselle" & DATAFILE$FIRST_NAME=="Bette"]<-"University of Missouri St Louis"
DATAFILE$INST[DATAFILE$LAST_NAME=="Ricklefs" & DATAFILE$FIRST_NAME=="Robert"]<-"University of Missouri St Louis"
DATAFILE$INST[DATAFILE$LAST_NAME=="Renner" & DATAFILE$FIRST_NAME=="Susanne"]<-"University of Missouri St Louis"
DATAFILE$INST[DATAFILE$LAST_NAME=="Sork" & DATAFILE$FIRST_NAME=="Victoria"]<-"University of Missouri St Louis"

levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"Universite Libre de Bruxelles","CNRS Centre dEcologie Fonctionnelle et Evolutive")
DATAFILE$INST[DATAFILE$LAST_NAME=="Parmentier"]<-"Universite Libre de Bruxelles"
DATAFILE$INST[DATAFILE$LAST_NAME=="Debussche"]<-"CNRS Centre dEcologie Fonctionnelle et Evolutive"


# DATAFILE$INST<-as.factor(DATAFILE$INST)
# DATAFILE$STATE<-as.factor(DATAFILE$STATE)
# DATAFILE$COUNTRY<-as.factor(DATAFILE$COUNTRY)
# DATAFILE$Inst_Prior_Class<-as.factor(DATAFILE$Inst_Prior_Class)
# DATAFILE<-DATAFILE %>% select(-DATA)

##############################################################
##############################################################
# Correcting or systematizing the name/speclling of an institution
##############################################################
##############################################################




levels(DATAFILE$INST) <- c(levels(DATAFILE$INST),"University of Missouri Columbia")
DATAFILE$INST[DATAFILE$INST=="University of Missouri"]<-"University of Missouri Columbia"

DATAFILE$INST<-as.character(DATAFILE$INST)
DATAFILE$INST<-trimws(DATAFILE$INST, which = "left")
DATAFILE$INST<-trimws(DATAFILE$INST, which = "right")
DATAFILE$INST<-gsub("- ", "-", DATAFILE$INST)
DATAFILE$INST<-gsub(" -", "-", DATAFILE$INST)
DATAFILE$INST<-gsub(" - ", "-", DATAFILE$INST)
DATAFILE$INST<-gsub(" at ", "-", DATAFILE$INST)
DATAFILE$INST<-gsub("The ", "", DATAFILE$INST)
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
DATAFILE$INST[DATAFILE$INST=="WSL Swiss Federal Research Institute"]<-"Swiss Federal Research Institute WSL"
# DATAFILE$INST<-gsub("(CSIC)", "", DATAFILE$INST)
DATAFILE$INST<-gsub("Smithsonian Institute", "Smithsonian Institution", DATAFILE$INST)
DATAFILE$INST<-gsub("Minnestoa", "Minnesota", DATAFILE$INST)
DATAFILE$INST<-gsub("University of Lausanne", "Universite de Lausanne", DATAFILE$INST)
DATAFILE$INST<-gsub("Institute of Ecosystem Studies", "Cary Institute of Ecosystem Studies", DATAFILE$INST)
DATAFILE$INST<-gsub("Environment Bangalore", "Environment", DATAFILE$INST)
DATAFILE$INST[DATAFILE$INST=="Cary Cary Institute of Ecosystem Studies"]<-"Cary Institute of Ecosystem Studies"
DATAFILE$INST<-gsub(" CATIE","", DATAFILE$INST)
DATAFILE$INST<-gsub("CATIE ","", DATAFILE$INST)
DATAFILE$INST[DATAFILE$INST=="University of California NCEAS"]<-"National Center for Ecological Analysis and Synthesis"
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
DATAFILE$INST<-gsub("Khorasan Agricultural and Natural Resources Res Ctr","Khorasan Agricultural and Natural Resources Research Center", DATAFILE$INST)
DATAFILE$INST[DATAFILE$INST=="Texas Technical University"]<-"Texas Tech University" 
DATAFILE$INST[DATAFILE$INST=="Mississippi State Univ"]<-"Mississippi State University" 
DATAFILE$INST[DATAFILE$INST=="Empresa Brasileira de Pesquisa Agropecuária"]<-"Empresa Brasileira de Pesquisa Agropecuaria" 
DATAFILE$INST[DATAFILE$INST=="Technion Technion Israel Institute of Technology"]<-"Technion Israel Institute of Technology"
DATAFILE$INST[DATAFILE$INST=="Universidad Catolica de Chile"]<-"Pontifica Universidad Catolica de Chile"
DATAFILE$INST<-gsub("Philips University","Philips University Marburg", DATAFILE$INST)
DATAFILE$INST[DATAFILE$INST=="University of Marburg"]<-"Philipps University Marburg"
DATAFILE$INST[DATAFILE$INST=="Philipps University"]<-"Philipps University Marburg"
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
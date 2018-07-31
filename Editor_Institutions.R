
# R CODE FOR IMPORTING, MANIPULATING, AND ANALYZING DATA ON THE INSTITUTIONS OF JOURNAL EDITORS 


# Changes to be made: 
# 1) If possible pull the csv file directly uploaded as "global_uni1" directly from the repo 
# 2) If possible download "world_university_names.sql" directly from the repo
# 3) I did a bunch of hacking around to get the countries and uni names, when should be able to get 
#    much more easily by making quesries using SQL tools. 
# 4) WHY IS IT PUTTING AMERICAN UNIVERSITY - DUBAI in INDIA? Change country code un unis.df

# R & Package versions:
# R version = 3.3.1 (2016-06-21)
# tidyverse = 1.1.1


# Load required libraries
library(tidyverse)
library(stringr)
library(stringi)
library(stringdist)
library(RecordLinkage)
# 
# library(vegan)
# library(nlme)
# library(MuMIn)
# library(directlabels)
# library(grid)
# library(gridExtra)
# library(RColorBrewer)




# Clear the environment 
rm(list=ls())


##############################################################
##############################################################
#
# UPLOAD & STANDARDIZE DATA ON CARNEGIE CLASSIFICATIONS
#
##############################################################
##############################################################

CarnCats<-read_csv("./Data/CarnegCategories_2015.csv", col_names = TRUE)
CarnCats<-CarnCats %>% select(Value,Label_1) %>% rename(BASIC2015=Value, Classification=Label_1)
str(CarnCats)
CarnCats<-as.data.frame(CarnCats)

CarnRanks<-read_csv("./Data/CarnegRankings_2015_Reduced.csv", col_names = TRUE)
# CarnRanks<-CarnRanks %>% select(UNITID, NAME, CITY, STABBR,BASIC2015)
str(CarnRanks)
CarnRanks<-as.data.frame(CarnRanks)

CarnData<-left_join(CarnCats,CarnRanks,by="BASIC2015")
# grouping different subcategories
CarnData$Category[(CarnData$BASIC2015>0 & CarnData$BASIC2015<10)|CarnData$BASIC2015==14]<-"Associates"
CarnData$Category[CarnData$BASIC2015>=10 & CarnData$BASIC2015<=13]<-"SpecialFocus(2yr)"
CarnData$Category[CarnData$BASIC2015>=15 & CarnData$BASIC2015<=17]<-"Doctoral"
CarnData$Category[CarnData$BASIC2015>=18 & CarnData$BASIC2015<=20]<-"Masters"
CarnData$Category[CarnData$BASIC2015>=21 & CarnData$BASIC2015<=23]<-"Baccalaureate"
CarnData$Category[CarnData$BASIC2015==24]<-"Faith-Related"
CarnData$Category[CarnData$BASIC2015>=25 & CarnData$BASIC2015<=26]<-"Medical/Health"
CarnData$Category[CarnData$BASIC2015==27]<-"SpecialFocus-4yr-Engineering"
CarnData$Category[CarnData$BASIC2015==28]<-"SpecialFocus-4yr-Tech"
CarnData$Category[CarnData$BASIC2015==29]<-"SpecialFocus-4yr-Business"
CarnData$Category[CarnData$BASIC2015==30]<-"SpecialFocus-4yr-Art-Music-Design"
CarnData$Category[CarnData$BASIC2015==31]<-"Law"
CarnData$Category[CarnData$BASIC2015==32]<-"Other"
CarnData$Category[CarnData$BASIC2015==33]<-"Tribal"
CarnData$Category<-as.factor(CarnData$Category)
levels(CarnData$Category)





##############################################################
##############################################################
#
# UPLOAD & STANDARDIZE DATA ON EDITORS
#
##############################################################
##############################################################

folder <- "./Data/"      # path to folder that holds multiple .csv files
file_list <- list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder

# read in each .csv file in file_list and create a data frame with the same name as the .csv file
for (i in 1:length(file_list)){
  assign(file_list[i], 
         read.csv(paste(folder, file_list[i], sep=''),na.strings = c("","NA"), encoding = "ASCII")
  )}


# data <- 
#   do.call("bind_rows", 
#           lapply(file_list, 
#                  function(x) 
#                    read.csv(paste(folder, x, sep=''), 
#                             stringsAsFactors = FALSE)))


AG<-AGRONOMY_data_11.02.2017.csv %>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
AJB<-AJB.csv %>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)                      
AMNAT<-AMNAT.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
AREES<-AREES.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
BIOCON<-BIOCON_TS.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
CONBIO<-CONBIO_EKB.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
ECOG<-ECOGRAPHY_5112017.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
ECOL<-ECOLOGY.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
EVOL<-EVOL.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
FEM<-FEM_7112017.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
FUNECOL<-FUNECOLdata_Allen_EB_1dec.csv%>% select(JOURNAL,YEAR, FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,COUNTRY) #NO UNIT, CITY, STATE
JANE<-JANE.csv  %>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
JAPE<-JAPE_new.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
JBIOG<-JBIOG.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
JECOL<-JECOL_new.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
JTE<-JTE_2015.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
OECOL<-OECOL.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)                   
PLANTECOL<-PLANTECOL_new.csv %>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)

# Need to be completed
LECO<-LECO_2017.csv %>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# JZOOL NEEDS TO HAVE THE EDITOR IDS ADDED IN
JZOOL<-JZOOL17nov.csv %>% select(JOURNAL,YEAR, INST,COUNTRY)
GCB<-GCBdata.csv%>% select(JOURNAL,YEAR,INST,COUNTRY)
BITR<-BITR.csv %>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# OIKOS (JOAN)
OIKOS<-OIKOS_21july2018.csv %>% select(JOURNAL,YEAR, INST,COUNTRY)
# NEWPHYT (MAIRA)
NEWPHYT<-NEWPHYT_21july2018.csv %>% select(JOURNAL,YEAR, INST,COUNTRY)
# NAJFM (MAIRA)
NAJFM<-NAJFM_21july2018.csv %>% select(JOURNAL,YEAR, INST,COUNTRY)
# MARECOL
MARECOL<-MARECOL_21July2018.csv %>% select(JOURNAL,YEAR, INST,COUNTRY)
# MEPS: NOT EVEN AN EXCEL SHEET
# NEED TO SEE WHICH ONES ARE MISSING INSTITUTIONS


#### CORRECTIONS


## NOT CORRECT EITHER INST OR CITY STATE!!!
# FEM 1994 Colorado State University     USA       775    Douglas           A   Maguire      Seattle Washington              <NA>   <NA>


JANE$UNIT<-as.character(JANE$UNIT)
JANE$INST<-as.character(JANE$INST)
JANE$UNIT[JANE$editor_id==1702]<-"Deptartment of Biology"
JANE$INST[JANE$editor_id==1702]<-"University of York"
JANE$COUNTRY[JANE$editor_id==1702]<-"UK"

JBIOG<-JBIOG.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,CITY,STATE,COUNTRY)
JBIOG$INST<-as.character(JBIOG$INST)
JBIOG<-JBIOG %>% separate(INST, c("UNIT", "INST"),",",extra="merge",fill="left",remove=TRUE)
JBIOG$INST[JBIOG$UNIT=="Uc Merced"]<-"University of California-Merced"
JBIOG$UNIT[JBIOG$UNIT=="Uc Merced"]<-NA
JBIOG$INST[JBIOG$INST==" School Of Life Sciences, Arizona State University"]<-"Arizona State University"
JBIOG$UNIT[JBIOG$UNIT=="Institute For Species Exploration"]<-"Institute For Species Exploration,School of Life Sciences"
JBIOG$INST[JBIOG$UNIT=="Evolution And Marine Biology, University of California"]<-"University of California-Santa Barbara"
JBIOG$UNIT[JBIOG$FIRST_NAME=="Dov"]<-"Department of Ecology, Evolution, and Marine Biology"

PLANTECOL$INST<-as.character(PLANTECOL$INST)
PLANTECOL$INST[PLANTECOL$LAST_NAME=="Veblen"]<-"University of Colorado-Boulder"
PLANTECOL$INST[PLANTECOL$LAST_NAME=="Picket"]<-"New York Botanical Garden"
PLANTECOL$INST[PLANTECOL$LAST_NAME=="Peet"]<-"University of North Carolina-Chapel Hill"
PLANTECOL$INST[PLANTECOL$LAST_NAME=="Damman"]<-"University of Connecticut"
PLANTECOL$INST[PLANTECOL$LAST_NAME=="Pickett" & PLANTECOL$FIRST_NAME=="Steward"]<-"New York Botanical Garden"

PLANTECOL$INST<-as.factor(PLANTECOL$INST)

AG$INST<-as.character(AG$INST)
AG$STATE<-as.character(AG$STATE)
AG$COUNTRY<-as.character(AG$COUNTRY)
AG$INST[AG$LAST_NAME=="Benbi" & AG$FIRST_NAME=="Dinesh"]<-"Punjab Agricultural University"
AG$STATE[AG$LAST_NAME=="Benbi" & AG$FIRST_NAME=="Dinesh"]<-"Punjab"
AG$COUNTRY[AG$LAST_NAME=="Benbi" & AG$FIRST_NAME=="Dinesh"]<-"India"
AG$COUNTRY[AG$LAST_NAME=="Pedreira" & AG$FIRST_NAME=="Carlos"]<-"Brazil"


EVOL$COUNTRY<-as.character(EVOL$COUNTRY)
EVOL$STATE<-as.character(EVOL$STATE)
EVOL$COUNTRY[EVOL$LAST_NAME=="Knowlton" & EVOL$FIRST_NAME=="Nancy"]<-"Panama"
EVOL$STATE[EVOL$LAST_NAME=="Knowlton" & EVOL$FIRST_NAME=="Nancy"]<-NA

LECO$INST<-as.character(LECO$INST)
LECO$CITY[LECO$INST=="University of Nevada" & LECO$LAST_NAME=="Walker"]<-"Las Vegas"
LECO$STATE[LECO$INST=="University of Nevada" & LECO$LAST_NAME=="Walker"]<-"NV"
LECO$INST[LECO$INST=="University of Nevada" & LECO$LAST_NAME=="Walker"]<-"University of Nevada-Las Vegas"

OECOL$INST<-as.character(OECOL$INST)
OECOL$CITY[OECOL$INST=="University of Nevada" & OECOL$LAST_NAME=="Walker"]<-"Las Vegas"
OECOL$STATE[OECOL$INST=="University of Nevada" & OECOL$LAST_NAME=="Walker"]<-"NV"
OECOL$INST[OECOL$INST=="University of Nevada" & OECOL$LAST_NAME=="Hayes"]<-"University of Nevada-Reno"



AMNAT$INST<-as.character(AMNAT$INST)
AMNAT$CITY[AMNAT$INST=="University of Hawaii" & AMNAT$LAST_NAME=="Palumbi"]<-"Honolulu"
AMNAT$STATE[AMNAT$INST=="University of Hawaii" & AMNAT$LAST_NAME=="Palumbi"]<-"HI"
AMNAT$INST[AMNAT$INST=="University of Hawaii" & AMNAT$LAST_NAME=="Palumbi"]<-"University of Hawaii at Manoa"


####################
# BIND THEM UP 
####################

ALLDATA<-bind_rows(FUNECOL,MARECOL,NAJFM,NEWPHYT,OIKOS,JZOOL, GCB,BITR,LECO,PLANTECOL,OECOL,JTE,JECOL,JBIOG,JAPE,JANE,FUNECOL,FEM,EVOL,ECOL,ECOG,CONBIO,BIOCON,AREES,AMNAT,AJB,AG)

ALLDATA$JOURNAL<-as.factor(ALLDATA$JOURNAL)
ALLDATA<-droplevels(ALLDATA)
ALLDATA$Inst_Prior_Class
ALLDATA$INST<-as.factor(ALLDATA$INST)
levels(ALLDATA$INST)
####################
# Clean Up
####################

# Remove duplicate rws of each editor for a journal if they have same Inst 
ALLDATA <- ALLDATA %>% group_by(editor_id,JOURNAL) %>% filter(row_number(INST) == 1)
str(ALLDATA)
ALLDATA$INST<-as.character(ALLDATA$INST)
ALLDATA$INST[ALLDATA$INST == ""]  <- NA
ALLDATA<-as.data.frame(ALLDATA)

###############
## CLEAN UP THE COUNTRIES
###############

ALLDATA$COUNTRY<-as.factor(ALLDATA$COUNTRY)
levels(ALLDATA$COUNTRY)
ALLDATA$COUNTRY<-as.character(ALLDATA$COUNTRY)
ALLDATA$COUNTRY[ALLDATA$COUNTRY=="Australiatralia"]<-"Australia"
ALLDATA$COUNTRY[ALLDATA$COUNTRY=="MEXICO"]<-"Mexico"
ALLDATA$COUNTRY[ALLDATA$COUNTRY=="NewZealand"]<-"New Zealand"
ALLDATA$COUNTRY[ALLDATA$COUNTRY=="PuertoRico"]<-"Puerto Rico"
ALLDATA$COUNTRY[ALLDATA$COUNTRY=="The Netherlands"]<-"Netherlands"
ALLDATA$COUNTRY[ALLDATA$COUNTRY=="UK"]<-"United Kingdom"
ALLDATA$COUNTRY[ALLDATA$COUNTRY=="US"]<-"USA"
ALLDATA$COUNTRY[ALLDATA$COUNTRY=="Usa"]<-"USA"
ALLDATA$COUNTRY[ALLDATA$COUNTRY=="United States"]<-"USA"
ALLDATA$COUNTRY[ALLDATA$INST=="University of New South Wales"]<-"Australia"
ALLDATA$COUNTRY[ALLDATA$INST=="University of Guelph"]<-"Canada"
ALLDATA$COUNTRY[ALLDATA$INST=="Instituto Mediterraneo de Estudios Avanzados (IMEDEA)"]<-"Spain"



#######
####### NEED TO CONFIRM WHAT PART OF USSR IN WHICH THE AUTHOR WAS BASED
#######
ALLDATA$COUNTRY[ALLDATA$COUNTRY=="USSR"]<-"Russia"
#######
#######
#######
ALLDATA$COUNTRY[ALLDATA$FIRST_NAME=="Jeannine" & ALLDATA$LAST_NAME=="Cavender-Bares"]<-"USA"


ALLDATA$COUNTRY<-as.factor(ALLDATA$COUNTRY)
ALLDATA<-droplevels(ALLDATA)
levels(ALLDATA$COUNTRY)

########
########NEED DO DELETE THESE
########
which(ALLDATA$COUNTRY=="")

###############
###############
# CLEAN UP THE INSTITUTIONS
###############
###############

write.csv(ALLDATA,file="SciWri17_Ed_Inst.csv")
#USE open refine to clean up countries and Inst

# ALLDATA<-read_csv("./Data/SciWri17_Ed_Inst_OR_Clean.csv", col_names=TRUE)
ALLDATA<-as.data.frame(ALLDATA)
str(ALLDATA)
ALLDATA$JOURNAL<-as.factor(ALLDATA$JOURNAL)

ALLDATA$INST[ALLDATA$Column==1149]<-"Forestry and Forest Products Research Institute"

ALLDATA$INST<-as.factor(ALLDATA$INST)
ALLDATA$STATE<-as.factor(ALLDATA$STATE)
ALLDATA$COUNTRY<-as.factor(ALLDATA$COUNTRY)
ALLDATA$Inst_Prior_Class<-as.factor(ALLDATA$Inst_Prior_Class)
ALLDATA<-ALLDATA %>% select(-DATA)

ALLDATA$INST<-as.character(ALLDATA$INST)

ALLDATA$INST<-trimws(ALLDATA$INST, which = "left")
ALLDATA$INST<-trimws(ALLDATA$INST, which = "right")
ALLDATA$INST<-gsub("- ", "-", ALLDATA$INST)
ALLDATA$INST<-gsub(" -", "-", ALLDATA$INST)
ALLDATA$INST<-gsub(" - ", "-", ALLDATA$INST)
ALLDATA$INST<-gsub(" at ", "-", ALLDATA$INST)
ALLDATA$INST<-gsub("The ", "", ALLDATA$INST)
ALLDATA$INST<-gsub(" Of ", " of ", ALLDATA$INST)
ALLDATA$INST<-gsub("of California ", "of California-", ALLDATA$INST)
ALLDATA$INST<-gsub("U ", "University of ", ALLDATA$INST)
ALLDATA$INST<-gsub("California, ", "California-", ALLDATA$INST)
ALLDATA$INST<-gsub("U California ", "University of California-", ALLDATA$INST)
ALLDATA$INST<-gsub("U. of", "University of", ALLDATA$INST)
ALLDATA$INST<-gsub("Univ. of", "University of", ALLDATA$INST)
ALLDATA$INST<-gsub("Machigan", "Michigan", ALLDATA$INST)
ALLDATA$INST<-gsub("N. Prairie", "Northern Prairie", ALLDATA$INST)
ALLDATA$INST<-gsub("Pacific S.W. Research Station-US Forest Service", "USFS-Pacific Southwest Research Station", ALLDATA$INST)
ALLDATA$INST<-gsub("N.B.S. ", "USGS-", ALLDATA$INST)
ALLDATA$INST<-gsub("U.S. ", "US ", ALLDATA$INST)
ALLDATA$INST<-gsub("Illimois", "Illinois", ALLDATA$INST)
ALLDATA$INST<-gsub("Univerisity", "University", ALLDATA$INST)
ALLDATA$INST<-gsub("Univeristy", "University", ALLDATA$INST)
ALLDATA$INST<-gsub("Univerist", "University", ALLDATA$INST)
ALLDATA$INST<-gsub("Universit ", "University", ALLDATA$INST)

ALLDATA$UNIT[ALLDATA$INST=="Pacific S.W. Research Station-US Forest Service"]<-"Pacific S.W. Research Station"
ALLDATA$INST[ALLDATA$INST=="Pacific S.W. Research Station-US Forest Service"]<-"US Forest Service"
# ALLDATA$INST[ALLDATA$INST=="Pacific S.W. Research Station-US Forest Service"]<-"USFS-Pacific Southwest Research Station"
ALLDATA$INST[ALLDATA$INST=="University fo California Berkeley"]<-"University of California-Berkeley"
ALLDATA$INST[ALLDATA$INST=="US Geological survey"]<-"US Geological Survey"
ALLDATA$INST[ALLDATA$INST=="Cal State Bakersfield"]<-"California State University-Bakersfield"
ALLDATA$INST[ALLDATA$INST=="Universityof Wisconsin-Milwaukee"]<-"University of Wisconsin-Milwaukee"
ALLDATA$INST[ALLDATA$INST=="Colorado State"]<-"Colorado State University"
ALLDATA$INST[ALLDATA$INST=="Cornell"]<-"Cornell University"
ALLDATA$INST[ALLDATA$INST=="East Carolina U"]<-"East Carolina University"
ALLDATA$INST[ALLDATA$INST=="Florida International U"]<-"Florida International University"
ALLDATA$INST[ALLDATA$INST=="FSU"]<-"Florida State University"
ALLDATA$INST[ALLDATA$INST=="Harvard"]<-"Harvard University"
ALLDATA$INST[ALLDATA$INST=="Illinois State"]<-"Illinois State University"
ALLDATA$INST[ALLDATA$INST=="Indiana U"]<-"Indiana University"
ALLDATA$INST[ALLDATA$INST=="Institute of Ecosystem"]<-"Institute of Ecosystem Studies"
ALLDATA$INST[ALLDATA$INST=="Iowa State"]<-"Iowa State University"
ALLDATA$INST[ALLDATA$INST=="John Carroll U"]<-"John Carroll University"
ALLDATA$INST[ALLDATA$INST=="Knoxville"]<-"University of Tennessee"
ALLDATA$INST[ALLDATA$INST=="Louisiana State"]<-"Louisiana State University"
ALLDATA$INST[ALLDATA$INST=="Marshall Unin"]<-"Marshall University"
ALLDATA$INST[ALLDATA$INST=="Miami U"]<-"Miami University"
ALLDATA$INST[ALLDATA$INST=="Montana State"]<-"Montana State University"
ALLDATA$INST[ALLDATA$INST=="North Dakota State"]<-"North Dakota State University"
ALLDATA$INST[ALLDATA$INST=="Ohio State"]<-"Ohio State University"
ALLDATA$INST[ALLDATA$INST=="Oregon State"]<-"Oregon State University"
ALLDATA$INST[ALLDATA$INST=="Rensselaer Poly"]<-"Rensselaer Polytechnic Institute"
ALLDATA$INST[ALLDATA$INST=="Rutgers U"]<-"Rutgers University"
ALLDATA$INST[ALLDATA$INST=="Stanford"]<-"Stanford University"
# ALLDATA$INST[ALLDATA$INST=="State University of New York at Binghamton"]<-"State University of New York-Binghamton"
ALLDATA$INST[ALLDATA$INST=="State University of New York at Binghamton"]<-"SUNY at Binghamton"
# ALLDATA$INST[ALLDATA$INST=="Binghamton University-SUNY"]<-"State University of New York-Binghamton"
ALLDATA$INST[ALLDATA$INST=="Binghamton University-SUNY"]<-"SUNY at Binghamton"
ALLDATA$INST[ALLDATA$INST=="Texas State"]<-"Texas State University"
ALLDATA$INST[ALLDATA$INST=="The University of Southwestern Louisiana"]<-"University of Southwestern Louisiana"
ALLDATA$INST[ALLDATA$INST=="U Arizona"]<-"University of Arizona"
ALLDATA$INST[ALLDATA$INST=="University of California Irvine"]<-"University of California-Irvine"
ALLDATA$INST[ALLDATA$INST=="University of California Riverside"]<-"University of California-Riverside"
ALLDATA$INST[ALLDATA$INST=="University of California at San Diego"]<-"University of California-San Diego"
ALLDATA$INST[ALLDATA$INST=="University of California Santa Cruz"]<-"University of California-Santa Cruz"
ALLDATA$INST[ALLDATA$INST=="Uc Merced, University of California"]<-"University of California-Merced"
ALLDATA$INST[ALLDATA$INST=="UMSL"]<-"University of Missouri-St. Louis"
ALLDATA$INST[ALLDATA$INST=="University of Carolina Aiken"]<-"University of South Carolina-Aiken"
# ALLDATA$INST[ALLDATA$INST=="University of Illinois at Urbana-Champaign"]<-"University of Illinois"
ALLDATA$INST[ALLDATA$INST=="University of Massachusetts Boston"]<-"University of Massachusetts-Boston"
ALLDATA$INST[ALLDATA$INST=="University of Nevada Las Vegas"]<-"University of Nevada-Las Vegas"
# ALLDATA$INST[ALLDATA$INST=="University of North Carolina at Chapel Hill"]<-"University of North Carolina-Chapel Hill"
# ALLDATA$INST[ALLDATA$INST=="University of Texas at Austin"]<-"University of Texas-Austin"
ALLDATA$INST[ALLDATA$INST=="University of Texas Austin"]<-"University of Texas at Austin"
ALLDATA$INST[ALLDATA$INST=="Vanderbilt"]<-"Vanderbilt University"
ALLDATA$INST[ALLDATA$INST=="Virginia Commonweath U"]<-"Virginia Commonweath University"
ALLDATA$INST[ALLDATA$INST=="Washington State"]<-"Washington State University"
ALLDATA$INST[ALLDATA$INST=="Washington U"]<-"Washington University"
ALLDATA$INST[ALLDATA$INST=="Willamette U"]<-"Willamette University"
# ALLDATA$INST[ALLDATA$INST=="University of Colorado Boulder"]<-"University of Colorado-Boulder"
ALLDATA$INST[ALLDATA$INST=="University of Pennsylvania, "]<-"University of Pennsylvania"
ALLDATA$INST[ALLDATA$INST=="University of Pittsburg"]<-"University of Pittsburgh"
ALLDATA$INST[ALLDATA$INST=="University of Tennesee"]<-"University of Tennessee"
ALLDATA$INST[ALLDATA$INST=="Acad Natural Science Philadelphia"]<-"Academy of Natural Science Philadelphia"
ALLDATA$INST[ALLDATA$INST=="Am Mus Nat Hist"]<-"American Museum of Natural History"
ALLDATA$INST[ALLDATA$INST=="ColoradoState Univ"]<-"Colorado State University"
ALLDATA$INST[ALLDATA$INST=="Evergreen State College Washington"]<-"Evergreen State College"
ALLDATA$INST[ALLDATA$INST=="Filed Museum of Natural History"]<-"Field Museum Of Natural History"
ALLDATA$INST[ALLDATA$INST=="George Mason University Virginia"]<-"George Mason University"
ALLDATA$INST[ALLDATA$INST=="Insitute of Ecosystem Studies"]<-"Institute of Ecosystem Studies"
ALLDATA$INST[ALLDATA$INST=="Kenyon College Ohio"]<-"Kenyon College"
ALLDATA$INST[ALLDATA$INST=="Louisiana State Univeristy"]<-"Louisiana State University"
ALLDATA$INST[ALLDATA$INST=="Milwaukee Public Mus"]<-"Milwaukee Public Museum"
ALLDATA$INST[ALLDATA$INST=="Missouri Botanical Gardens"]<-"Missouri Botanical Garden"
ALLDATA$INST[ALLDATA$INST=="Oklahoma Museum of Natural History"]<-"Oklahoma Mus Nat His"
ALLDATA$INST[ALLDATA$INST=="Smithsonian(STRI)"]<-"Smithsonian Tropical Research Institute"
ALLDATA$INST[ALLDATA$INST=="STRI Smithsonian"]<-"Smithsonian Tropical Research Institute"
ALLDATA$INST[ALLDATA$INST=="U Connecticut"]<-"University of Connecticut"
ALLDATA$INST[ALLDATA$INST=="University of Alabama Huntsville"]<-"University of Alabama in Huntsville"
ALLDATA$INST[ALLDATA$INST=="University of Connetticut Storrs"]<-"University of Connecticut"
ALLDATA$INST[ALLDATA$INST=="University of Michgan"]<-"University of Michigan"
ALLDATA$INST[ALLDATA$INST=="University of Missouri St Louis"]<-"University of Missouri-St. Louis"
ALLDATA$INST[ALLDATA$INST=="University of Missouri_St Louis"]<-"University of Missouri-St. Louis"
ALLDATA$INST[ALLDATA$INST=="University of Wisconsin Milwaukee"]<-"University of Wisconsin-Milwaukee"
ALLDATA$INST[ALLDATA$INST=="U California Irvine"]<-"University of California-Irvine"
ALLDATA$INST[ALLDATA$INST=="U California Riverside"]<-"University of California-Riverside"
ALLDATA$INST[ALLDATA$INST=="University of California at San Diego"]<-"University of California-San Diego"
ALLDATA$INST[ALLDATA$INST=="U California Santa Barbara"]<-"University of California-Santa Barbara"
ALLDATA$INST[ALLDATA$INST=="U California Santa Cruz"]<-"University of California-Santa Cruz"
ALLDATA$INST[ALLDATA$INST=="University of California Los Angeles"]<-"University of California-Los Angeles"
ALLDATA$INST[ALLDATA$INST=="University of California Berkeley"]<-"University of California-Berkeley"
ALLDATA$INST[ALLDATA$INST=="University of California Davis"]<-"University of California-Davis"
ALLDATA$INST[ALLDATA$INST=="University of California Santa Barbara"]<-"University of California-Santa Barbara"
ALLDATA$INST[ALLDATA$INST=="University of California San Diego"]<-"University of California-San Diego"
ALLDATA$INST[ALLDATA$INST=="University of California San Diego"]<-"University of California-San Diego"
ALLDATA$INST[ALLDATA$INST=="Rensselaer Poly"]<-"Rensselaer Polytechnic Institute"
ALLDATA$INST[ALLDATA$INST=="Oklahoma Mus Nat His"]<-"Oklahoma Museum of Natural History"
ALLDATA$INST[ALLDATA$INST=="US Forest Service Pacific Southwest Research Station"]<-"USFS-Pacific Southwest Research Station"
ALLDATA$INST[ALLDATA$INST=="U.S.F.W.S. National Wetlands Research Center"]<-"USFWS-National Wetlands Research Center"
ALLDATA$INST[ALLDATA$INST=="Arizona State University West"]<-"Arizona State University-West Campus"
ALLDATA$INST[ALLDATA$INST=="Naval Research Laboratory DC"]<-"US Naval Research Laboratory"
ALLDATA$INST[ALLDATA$INST=="Temple-Inland Forest"]<-"Temple-Inland Forest Products"
ALLDATA$INST[ALLDATA$INST=="USDA Forest Service"]<-"US Forest Service"
ALLDATA$INST[ALLDATA$INST=="USDA Forest Service Rocky Mountain Research Station"]<-"US Forest Service-Rocky Mountain Research Station"
ALLDATA$INST[ALLDATA$INST=="USDA Forest Service Southern Research Station"]<-"US Forest Service-Southern Research Station"
ALLDATA$INST[ALLDATA$INST=="USFS-Pacific Southwest Research Station"]<-"US Forest Service-Pacific Southwest Research Station"
ALLDATA$INST[ALLDATA$INST=="USFWS-National Wetlands Research Center"]<-"US Fish & Wildlife Service-National Wetlands Research Center"
ALLDATA$UNIT[ALLDATA$INST=="USGS Forest and Rangeland Ecosystem Science Center"]<-"Forest and Rangeland Ecosystem Science Center"
ALLDATA$INST[ALLDATA$INST=="USGS Forest and Rangeland Ecosystem Science Center"]<-"US Geological Survey"
# ALLDATA$INST[ALLDATA$INST=="USGS Western Ecological Research Center"]<-"US Geological Survey-Western Ecological Research Center"
ALLDATA$UNIT[ALLDATA$INST=="USGS-Northern Prairie Wildlife Research Center"]<-"Northern Prairie Wildlife Research Center"
ALLDATA$INST[ALLDATA$INST=="USGS-Northern Prairie Wildlife Research Center"]<-"US Geological Survey"
# ALLDATA$INST[ALLDATA$INST=="USGS-Northern Prairie Wildlife Research Center"]<-"US Geological Survey-Northern Prairie Wildlife Research Center"
ALLDATA$INST[ALLDATA$INST=="National Museum of Natural History"]<-"Smithsonian National Museum of Natural History"
ALLDATA$INST[ALLDATA$INST=="Smithsonian"]<-"Smithsonian National Museum of Natural History" #EB Verified
ALLDATA$INST[ALLDATA$INST=="USDA"]<-"US Department of Agriculture"
ALLDATA$UNIT[ALLDATA$INST=="USDA Cooperative State Research, Education and Extension Service (CSREES)"]<-"Cooperative State Research, Education, and Extension Service"
ALLDATA$INST[ALLDATA$INST=="USDA Cooperative State Research, Education and Extension Service (CSREES)"]<-"US Department of Agriculture"
# ALLDATA$INST[ALLDATA$INST=="USDA Cooperative State Research, Education and Extension Service (CSREES)"]<-"US Department of Agriculture Cooperative State Research, Education, and Extension Service"
ALLDATA$INST[ALLDATA$INST=="Virginia Tech"]<-"Virginia Polytechnic Institute and State University"
ALLDATA$INST[ALLDATA$INST=="University of Pennsylvania,"]<-"University of Pennsylvania"
ALLDATA$INST[ALLDATA$INST=="Division of Reptiles and Amphibians Smithsonian Institute Washington DC"]<-"Smithsonian National Museum of Natural History"
ALLDATA$INST[ALLDATA$INST=="University of California-At Los Angeles"]<-"University of California-Los Angeles"
ALLDATA$INST[ALLDATA$INST=="National Museum of Natural History, Smithsonian Institution"]<-"Smithsonian National Museum of Natural History"

ALLDATA$UNIT[ALLDATA$INST=="Citrus Research and Education Center"]<-"Citrus Research and Education Center"
ALLDATA$INST[ALLDATA$INST=="Citrus Research and Education Center"]<-"University of Florida"

ALLDATA$INST[ALLDATA$INST=="Ohio u"]<-"Ohio University-Main Campus"
ALLDATA$INST[ALLDATA$INST=="University of South Carolina" & ALLDATA$CITY=="Columbia"]<-"University of South Carolina-Columbia"


ALLDATA$UNIT[ALLDATA$INST=="US Department of Agriculture Cooperative State Research, Education, and Extension Service"]<-"Cooperative State Research, Education, and Extension Service"
ALLDATA$INST[ALLDATA$INST=="US Department of Agriculture Cooperative State Research, Education, and Extension Service"]<-"US Department of Agriculture"

##NEED to 2X 
ALLDATA$INST[ALLDATA$LAST_NAME=="Bowers" & ALLDATA$CITY=="Vadnais Heights"]<-"Calyx, Inc."
ALLDATA$INST[ALLDATA$INST=="Arnold Arboretum Of Harvard University"]<-"Harvard University Arnold Arboretum"
ALLDATA$INST[ALLDATA$INST=="Museum of Comparative Zoology"]<-"Harvard University Museum of Comparative Zoology"
ALLDATA$INST[ALLDATA$INST=="SUNY"]<-"State University of New York"
ALLDATA$INST[ALLDATA$INST=="State University of New York at Stony Brook"]<-"State University of New York-Stony Brook"
ALLDATA$INST[ALLDATA$INST=="State University of New York Syracuse"]<-"State University of New York-Syracuse"
ALLDATA$INST[ALLDATA$INST=="Arnold Arboretum of Harvard University"]<-"Harvard University Arnold Arboretum"
ALLDATA$INST[ALLDATA$INST=="Harvard Medical School"]<-"Harvard University Medical School"
ALLDATA$INST[ALLDATA$INST=="Australian Research Center for Urban "]<-"Smithsonian Tropical Research Institute"


# Spell checking, inadvertent apostrophes, etc.
ALLDATA$INST<-gsub(" INPA", "", ALLDATA$INST)
ALLDATA$INST<-gsub("--", " ", ALLDATA$INST)
ALLDATA$INST<-gsub("-", " ", ALLDATA$INST)
ALLDATA$INST<-gsub(" (CSIRO)", " ", ALLDATA$INST)
ALLDATA$INST<-gsub(" CSIRO", " ", ALLDATA$INST)
ALLDATA$INST<-gsub(",CAS", " ", ALLDATA$INST)


# ALLDATA$INST[ALLDATA$INST==" INPA"]<-""
# ALLDATA$INST[ALLDATA$INST==" (CSIRO)"]<-""
# ALLDATA$INST[ALLDATA$INST==" CSIRO"]<-""
# ALLDATA$INST[ALLDATA$INST==",CAS"]<-""

ALLDATA$INST<-gsub("Nacaional", "Nacional", ALLDATA$INST)
ALLDATA$INST<-gsub("British Colombia", "British Columbia", ALLDATA$INST)
ALLDATA$INST<-gsub("Smithosonian", "Smithsonian", ALLDATA$INST)
ALLDATA$INST<-gsub("kansas", "Kansas", ALLDATA$INST)
ALLDATA$INST<-gsub("Sao Paolo", "Sao Paulo", ALLDATA$INST)
ALLDATA$INST<-gsub("illinois", "Illinois", ALLDATA$INST)

ALLDATA$INST<-gsub("Wuerzburg", "Wurzburg", ALLDATA$INST)
ALLDATA$INST<-gsub("LaTrobe", "La Trobe", ALLDATA$INST)
ALLDATA$INST<-gsub("No one by", "no one by", ALLDATA$INST)
ALLDATA$INST<-gsub("Botannical", "Botanical", ALLDATA$INST)
ALLDATA$INST<-gsub("Berkely", "Berkeley", ALLDATA$INST)
ALLDATA$INST<-gsub("Commonweath", "Commonwealth", ALLDATA$INST)
ALLDATA$INST<-gsub("Archibold", "Archbold", ALLDATA$INST)

# ALLDATA$INST[ALLDATA$INST=="Nacaional"]<-"Nacional"
# ALLDATA$INST[ALLDATA$INST=="British Colombia"]<-"British Columbia"
# ALLDATA$INST[ALLDATA$INST=="Smithosonian"]<-"Smithsonian"
# ALLDATA$INST[ALLDATA$INST=="kansas"]<-"Kansas"
# ALLDATA$INST[ALLDATA$INST=="Sao Paolo"]<-"Sao Paulo"
# ALLDATA$INST[ALLDATA$INST=="illinois"]<-"Illinois"
# ALLDATA$INST[ALLDATA$INST=="Wuerzburg"]<-"Wurzburg"
# ALLDATA$INST[ALLDATA$INST=="LaTrobe"]<-"La Trobe"
# ALLDATA$INST[ALLDATA$INST=="no one by"]<-"No one by"
# ALLDATA$INST[ALLDATA$INST=="Botannical"]<-"Botanical"
# ALLDATA$INST[ALLDATA$INST=="Berkely"]<-"Berkeley"
# ALLDATA$INST[ALLDATA$INST=="Commonweath"]<-"Commonwealth"
# ALLDATA$INST[ALLDATA$INST=="Archibold"]<-"Archbold"


ALLDATA$INST<-gsub("Loisisana", "Louisiana", ALLDATA$INST)
ALLDATA$INST<-gsub("Lousiana", "Louisiana", ALLDATA$INST)
ALLDATA$INST<-gsub("Bringham", "Brigham", ALLDATA$INST)
ALLDATA$INST<-gsub("Connetticut", "Connecticut", ALLDATA$INST)
ALLDATA$INST<-gsub("Unversity", "University", ALLDATA$INST)
ALLDATA$INST<-gsub(",", "", ALLDATA$INST)
ALLDATA$INST<-gsub("Westleyan", "Wesleyan", ALLDATA$INST)
ALLDATA$INST<-gsub("'", "", ALLDATA$INST)

# ALLDATA$INST[ALLDATA$INST=="Loisisana"]<-"Louisiana"
# ALLDATA$INST[ALLDATA$INST=="Lousiana"]<-""
# ALLDATA$INST[ALLDATA$INST=="Bringham"]<-"Brigham"
# ALLDATA$INST[ALLDATA$INST=="Connetticut"]<-"Connecticut"
# ALLDATA$INST[ALLDATA$INST=="Unversity"]<-"University"
# ALLDATA$INST[ALLDATA$INST==","]<-""
# ALLDATA$INST[ALLDATA$INST=="Westleyan"]<-"Wesleyan"
# ALLDATA$INST[ALLDATA$INST=="'"]<-""


ALLDATA$INST<-gsub("Veterinary&", "Veterinary and", ALLDATA$INST)
ALLDATA$INST<-gsub("&", "and", ALLDATA$INST)
ALLDATA$INST<-gsub("Virginia Tech", "Virginia Polytechnic Institute and State University", ALLDATA$INST)
ALLDATA$INST<-gsub("Austrailan", "Australian", ALLDATA$INST)
ALLDATA$INST<-gsub("Indian Institute of Sciences", "Indian Institute of Science", ALLDATA$INST)
ALLDATA$INST<-gsub("KingS", "Kings", ALLDATA$INST)
ALLDATA$INST<-gsub("Louisisana", "Louisiana", ALLDATA$INST)
ALLDATA$INST<-gsub("Universrity", "University", ALLDATA$INST)

# ALLDATA$INST[ALLDATA$INST=="Veterinary&"]<-"Veterinary and"
# ALLDATA$INST[ALLDATA$INST=="&"]<-"and"
# ALLDATA$INST[ALLDATA$INST=="Virginia Tech"]<-"Virginia Polytechnic Institute and State University"
# ALLDATA$INST[ALLDATA$INST=="Austrailan"]<-"Australian"
# ALLDATA$INST[ALLDATA$INST=="Indian Institute of Sciences"]<-"Indian Institute of Science"
# ALLDATA$INST[ALLDATA$INST=="KingS"]<-"Kings"
# ALLDATA$INST[ALLDATA$INST=="Louisisana"]<-"Louisiana"
# ALLDATA$INST[ALLDATA$INST=="Universrity"]<-"University"


ALLDATA$INST<-gsub("Canadian Forestry", "Canadian Forest", ALLDATA$INST)
ALLDATA$INST<-gsub("Mighican", "Michigan", ALLDATA$INST)
ALLDATA$INST<-gsub("de Montpellier II", "Montpellier II", ALLDATA$INST)
ALLDATA$INST<-gsub("North Arizona", "Northern Arizona", ALLDATA$INST)
ALLDATA$INST<-gsub("Wsl", "WSL", ALLDATA$INST)
ALLDATA$INST<-gsub("Alabama in", "Alabama", ALLDATA$INST)
ALLDATA$INST<-gsub("Fonctionelle", "Fonctionnelle", ALLDATA$INST)
ALLDATA$INST<-gsub("-CNRS", "", ALLDATA$INST)
ALLDATA$INST<-gsub("University of Sherbooke", "Universite de Sherbooke", ALLDATA$INST)

# ALLDATA$INST[ALLDATA$INST=="Canadian Forestry"]<-"Canadian Forest"
# ALLDATA$INST[ALLDATA$INST=="Mighican"]<-"Michigan"
# ALLDATA$INST[ALLDATA$INST=="de Montpellier II"]<-"Montpellier II"
# ALLDATA$INST[ALLDATA$INST=="North Arizona"]<-"Northern Arizona"
# ALLDATA$INST[ALLDATA$INST==" Wsl"]<-"WSL"
ALLDATA$INST[ALLDATA$INST=="WSL Swiss Federal Research Institute"]<-"Swiss Federal Research Institute WSL"
# ALLDATA$INST[ALLDATA$INST=="Alabama in"]<-"Alabama"
# ALLDATA$INST[ALLDATA$INST=="Fonctionelle"]<-"Fonctionnelle"
# ALLDATA$INST[ALLDATA$INST=="-CNRS"]<-""
# ALLDATA$INST[ALLDATA$INST=="University of Sherbooke"]<-"Universite de Sherbooke"


ALLDATA$INST<-gsub("(CSIC)", "", ALLDATA$INST)
ALLDATA$INST<-gsub("Smithsonian Institute", "Smithsonian Institution", ALLDATA$INST)
ALLDATA$INST<-gsub("Minnestoa", "Minnesota", ALLDATA$INST)
ALLDATA$INST<-gsub("University of Lausanne", "Universite de Lausanne", ALLDATA$INST)
ALLDATA$INST<-gsub("Institute of Ecosystem Studies", "Cary Institute of Ecosystem Studies", ALLDATA$INST)
ALLDATA$INST<-gsub("Environment Bangalore", "Environment", ALLDATA$INST)

# ALLDATA$INST[ALLDATA$INST=="(CSIC)"]<-""
# ALLDATA$INST[ALLDATA$INST=="Smithsonian Institute"]<-"Smithsonian Institution"
# ALLDATA$INST[ALLDATA$INST=="Minnestoa"]<-"Minnesota"
# ALLDATA$INST[ALLDATA$INST=="University of Lausanne"]<-"Universite de Lausanne"
# ALLDATA$INST[ALLDATA$INST=="Institute of Ecosystem Studies"]<-"Cary Institute of Ecosystem Studies"
ALLDATA$INST[ALLDATA$INST=="Cary Cary Institute of Ecosystem Studies"]<-"Cary Institute of Ecosystem Studies"
# ALLDATA$INST[ALLDATA$INST=="Environment Bangalore"]<-"Environment"



ALLDATA$INST<-gsub(" CATIE","", ALLDATA$INST)
ALLDATA$INST<-gsub("CATIE ","", ALLDATA$INST)
# 
# ALLDATA$INST[ALLDATA$INST==" CATIE"]<-""
# ALLDATA$INST[ALLDATA$INST=="CATIE "]<-""
ALLDATA$INST[ALLDATA$INST=="University of California NCEAS"]<-"National Center for Ecological Analysis and Synthesis"


ALLDATA$INST<-gsub(" (UNESP)","", ALLDATA$INST)
ALLDATA$INST<-gsub("Darwin University Darwin","Darwin University", ALLDATA$INST)
ALLDATA$INST<-gsub("Technion-","", ALLDATA$INST)
ALLDATA$INST<-gsub("Israel Institute of Technology","Technion Israel Institute of Technology", ALLDATA$INST)
ALLDATA$INST<-gsub("Ossietzky","", ALLDATA$INST)
ALLDATA$INST<-gsub(",USDA","", ALLDATA$INST)
ALLDATA$INST<-gsub("UFZ Centre","Helmholtz Centre", ALLDATA$INST)
ALLDATA$INST<-gsub(" UFZ","", ALLDATA$INST)


ALLDATA$INST<-gsub("Helmholtz Centre for Environmental Research","Helmholtz Centre for Environmental Research UFZ", ALLDATA$INST)



ALLDATA$INST<-gsub("A& M Univ","A & M University", ALLDATA$INST)
ALLDATA$INST<-gsub("AandM","A & M", ALLDATA$INST)
ALLDATA$INST<-gsub("Khorasan Agricultural and Natural Resources Res Ctr","Khorasan Agricultural and Natural Resources Research Center", ALLDATA$INST)
ALLDATA$INST[ALLDATA$INST=="Texas Technical University"]<-"Texas Tech University" 
ALLDATA$INST[ALLDATA$INST=="Mississippi State Univ"]<-"Mississippi State University" 
ALLDATA$INST[ALLDATA$INST=="Empresa Brasileira de Pesquisa Agropecuária"]<-"Empresa Brasileira de Pesquisa Agropecuaria" 

# ALLDATA$INST[ALLDATA$INST==" (UNESP)"]<-""
# ALLDATA$INST[ALLDATA$INST=="Darwin University Darwin"]<-"Darwin University"
# ALLDATA$INST[ALLDATA$INST=="Technion-"]<-""
ALLDATA$INST[ALLDATA$INST=="Technion Technion Israel Institute of Technology"]<-"Technion Israel Institute of Technology"
# ALLDATA$INST[ALLDATA$INST=="Ossietzky"]<-""
# ALLDATA$INST[ALLDATA$INST==",USDA"]<-""
ALLDATA$INST[ALLDATA$INST=="Universidad Catolica de Chile"]<-"Pontifica Universidad Catolica de Chile"
# ALLDATA$INST[ALLDATA$INST=="Pontifica Pontifica Universidad Catolica de Chile"]<-"Pontifica Universidad Catolica de Chile"
# ALLDATA$INST[ALLDATA$INST=="UFZ Centre"]<-"Helmholtz Centre"
# ALLDATA$INST[ALLDATA$INST==" UFZ"]<-""
# ALLDATA$INST[ALLDATA$INST=="Helmholtz Centre for Environmental Research"]<-"Helmholtz Centre for Environmental Research UFZ"
# ALLDATA$INST[ALLDATA$INST=="dHistoire"]<-"dHistoire Naturelle"
# ALLDATA$INST[ALLDATA$INST=="A& M Univ"]<-"A & M University"


ALLDATA$INST<-gsub("Philips University","Philips University Marburg", ALLDATA$INST)
ALLDATA$INST[ALLDATA$INST=="University of Marburg"]<-"Philipps University Marburg"
ALLDATA$INST[ALLDATA$INST=="Philipps University"]<-"Philipps University Marburg"



ALLDATA$INST<-gsub("in Cornwall","Cornwall", ALLDATA$INST)
ALLDATA$INST<-gsub("SUNY","State University of New York", ALLDATA$INST)
ALLDATA$INST<-gsub("USGS","US Geological Survey", ALLDATA$INST)
ALLDATA$INST<-gsub("University of Mexico","Universidad Nacional Autonoma de Mexico", ALLDATA$INST)
ALLDATA$INST<-gsub("NC Agricultural","North Carolina Agricultural", ALLDATA$INST)
ALLDATA$INST<-gsub("for Forest, Snow, and Landscape Research","WSL", ALLDATA$INST)
ALLDATA$INST<-gsub(" Company","", ALLDATA$INST)
ALLDATA$INST<-gsub("CSIRO Plant Industry","", ALLDATA$INST)
ALLDATA$INST<-gsub("State University University","State University", ALLDATA$INST)
ALLDATA$INST<-gsub("North Carolina AandT State University","North Carolina A & T State University", ALLDATA$INST)
ALLDATA$INST<-gsub(" Manaaki Whenua","", ALLDATA$INST)
ALLDATA$INST<-gsub(" Headquarters","", ALLDATA$INST)
ALLDATA$INST<-gsub("VUniversity","University", ALLDATA$INST)
ALLDATA$INST<-gsub(" (IMEDEA)","", ALLDATA$INST)
ALLDATA$INST<-gsub(" (UNICAMP)","", ALLDATA$INST)



ALLDATA$INST<-gsub("dHistoire","dHistoire Naturelle", ALLDATA$INST)
ALLDATA$INST<-gsub("National Museum of Natural History Paris","Museum National dHistoire Naturelle", ALLDATA$INST)
ALLDATA$INST<-gsub("Naturelle Naturelle","Naturelle", ALLDATA$INST)

# ALLDATA$INST[ALLDATA$INST=="in Cornwall"]<-"Cornwall"
# ALLDATA$INST[ALLDATA$INST=="SUNY"]<-"State University of New York"
ALLDATA$INST[ALLDATA$INST=="Museum dHistoire Naturelle"]<-"Museum National dHistoire Naturelle"
# ALLDATA$INST[ALLDATA$INST=="USGS"]<-"US Geological Survey"
ALLDATA$INST[ALLDATA$INST=="National Autonomous university of Mexico"]<-"Universidad Nacional Autonoma de Mexico"
# ALLDATA$INST[ALLDATA$INST=="NC Agricultural"]<-"North Carolina Agricultural"
# ALLDATA$INST[ALLDATA$INST=="for Forest, Snow, and Landscape Research"]<-"WSL"
# ALLDATA$INST[ALLDATA$INST==" Company"]<-""
# ALLDATA$INST[ALLDATA$INST=="CSIRO Plant Industry"]<-""
# ALLDATA$INST[ALLDATA$INST==" Manaaki Whenua"]<-""
ALLDATA$INST[ALLDATA$INST=="Landcare Research"]<-"Manaaki Whenua Landcare Research"
# ALLDATA$INST[ALLDATA$INST==" Headquarters"]<-""
# ALLDATA$INST[ALLDATA$INST=="VUniversity"]<-"VUniversity"
# ALLDATA$INST[ALLDATA$INST==" (IMEDEA)"]<-""
# ALLDATA$INST[ALLDATA$INST==" (UNICAMP)"]<-""


ALLDATA$UNIT[ALLDATA$INST=="USGS/NRII"]<-"NRII" 
ALLDATA$INST[ALLDATA$INST=="USGS/NRII"]<-"US Geological Survey"

ALLDATA$UNIT[ALLDATA$INST=="Florida International University and Center for Tropical Plant Conservation"]<-"Center for Tropical Plant Conservation" 
ALLDATA$INST[ALLDATA$INST=="Florida International University and Center for Tropical Plant Conservation"]<-"Florida International University" 

ALLDATA$UNIT[ALLDATA$INST=="Harvard University Herbaria"]<-"Herbaria" 
ALLDATA$INST[ALLDATA$INST=="Harvard University Herbaria"]<-"Harvard University"

ALLDATA$UNIT[ALLDATA$INST=="Harvard University Medical School"]<-"Medical School"
ALLDATA$INST[ALLDATA$INST=="Harvard University Medical School"]<-"Harvard University"

ALLDATA$UNIT[ALLDATA$INST=="Boston Unveristy Marine Program"]<-"Marine Program"
ALLDATA$INST[ALLDATA$INST=="Boston Unveristy Marine Program"]<-"Boston Unversity" 

ALLDATA$UNIT[ALLDATA$INST=="Harvard University Museum of Comparative Zoology"]<-"Museum of Comparative Zoology"
ALLDATA$INST[ALLDATA$INST=="Harvard University Museum of Comparative Zoology"]<-"Harvard University" 

ALLDATA$UNIT[ALLDATA$INST=="Harvard Forest"]<-"Harvard Forest"
ALLDATA$INST[ALLDATA$INST=="Harvard Forest"]<-"Harvard University" 

ALLDATA$UNIT[ALLDATA$INST=="Harvard University Arnold Arboretum"]<-"Arnold Arboretum"
ALLDATA$INST[ALLDATA$INST=="Harvard University"]<-"Harvard University" 

ALLDATA$UNIT[ALLDATA$INST=="Instituto de Ecologia UNAM"]<-"Instituto de Ecologia"
ALLDATA$INST[ALLDATA$INST=="Instituto de Ecologia UNAM"]<-"Universidad Nacional Autonoma de Mexico" 

ALLDATA$UNIT[ALLDATA$INST=="Harvard University Museum of Comparative Zoology"]<-"Museum of Comparative Zoology"
ALLDATA$INST[ALLDATA$INST=="Harvard University Museum of Comparative Zoology"]<-"Harvard University" 

ALLDATA$UNIT[ALLDATA$INST=="Wageiningen University Research Center Alterra"]<-"Research Center Alterra"
ALLDATA$INST[ALLDATA$INST=="Wageiningen University Research Center Alterra"]<-"Wageiningen University" 

ALLDATA$INST[ALLDATA$INST=="Ecole Normale Supérieure"]<-"Ecole Normale Superieure" 
ALLDATA$INST[ALLDATA$INST=="Université de Sherbrooke"]<-"Universite de Sherbrooke"
ALLDATA$INST[ALLDATA$INST=="Pontificia Universidad Católica de Chile"]<-"Pontificia Universidad Catolica de Chile" 
ALLDATA$INST[ALLDATA$INST=="University of Tromsø"]<-"University of Tromso" 
ALLDATA$INST[ALLDATA$INST=="Universit\xfc\xbe\x8c\xa3\xa0\xbc Montpellier II"]<-"Universite Montpellier II" 
ALLDATA$INST[ALLDATA$INST=="Universit\xfc\xbe\x8d\x83\xa0\xbct Z\xfc\xbe\x8c\x93\xa0\xbcrich Irchel"]<-"University of Zurich Irchel" 
ALLDATA$INST[ALLDATA$INST=="Texas A & M Univ."]<-"Texas A & M University"
ALLDATA$INST[ALLDATA$INST=="Texas A & M"]<-"Texas A & M University"


corrections1<-read.csv("./Data/uniNameList_corrections_only.csv",encoding = "ASCII",stringsAsFactors = FALSE)
str(corrections1)
#THIS CHANGES THE UNITS AND INST BASED ON THE CORRECTION IN THE DATAFRAME
ALLDATA<-left_join(ALLDATA, corrections1,by="INST",copy=TRUE) %>% mutate(INST = ifelse(is.na(INST_CORR), INST, INST_CORR)) %>% mutate(UNIT = ifelse(is.na(UNIT_CORR), UNIT, UNIT_CORR))
ALLDATA<-ALLDATA %>% select(-INST_CORR,-UNIT_CORR)

# ALLDATA$INST[ALLDATA$INST==" UPV"]<-"Universitat Politecnica de Valencia" 
ALLDATA$INST[ALLDATA$INST=="USU"]<-"Utah State University"
ALLDATA$INST[ALLDATA$LAST_NAME=="Luque"]<-"Institut national de recherche en sciences et technologies pour lenvironnement et lagriculture"
ALLDATA$INST[ALLDATA$INST=="<a0>Forestry and Forest Products Research Institute"]<-"Forestry and Forest Products Research Institute"
ALLDATA$INST[ALLDATA$INST=="<a0>Universit<e9> Claude Bernard Lyon 1"]<-"Universite Claude Bernard Lyon 1" 
ALLDATA$INST<-gsub("Landscape<a0>Ecology","Landscape Ecology", ALLDATA$INST)
ALLDATA$INST<-gsub("Universit<8a>t","Universitat", ALLDATA$INST)


ALLDATA$INST<-gsub("Institue",  "Institute", ALLDATA$INST)
ALLDATA$INST<-gsub("Univerity",  "University", ALLDATA$INST)
ALLDATA$INST[ALLDATA$INST=="Canadia Forest Service"]<-"Canadian Forest Service"
ALLDATA$INST[ALLDATA$INST=="lowa State University"]<-"Iowa State University"

# This will check if any are still in UTF8 with accents instead of ascii
asciitest<-stri_enc_mark(ALLDATA$INST)
asciitest<-as.factor(asciitest)
summary(asciitest)

# ALLDATA_native<-ALLDATA$INST[asciitest]
# ALLDATA_native
# corrections<-NULL
# original<-c('"Ecole Normale Supérieure"',  
#   '"Pontificia Universidad Católica de Chile"',   
#   '"Université de Sherbrooke"',
#   '"University of Tromsø"',
#   '"Universit\xfc\xbe\x8c\xa3\xa0\xbc Montpellier II"',
#   '"Universit\xfc\xbe\x8d\x83\xa0\xbct Z\xfc\xbe\x8c\x93\xa0\xbcrich Irchel"')
# # 
# 
# corrections$name<-c('Ecole Normale Superieure', 
# 'Pontificia Universidad Catolica de Chile', 
# 'Universite de Sherbrooke', 
# 'University of Tromso',
# 'Universite Montpellier II',  
# 'University of Zurich Irchel')

# 
# asciitest<-as.data.frame(asciitest)
# asciitest<-asciitest %>% rename("name"="asciitest")
# corrections2<-bind_cols(asciitest,corrections)
# 
# ALLDATA_native
# ALLDATA$INST[corrections2$name] <- corrections2$name1
# 
# 



# ALLDATA$INST<-as.character(ALLDATA$INST)
# ALLDATA$INST<-str_replace_all(ALLDATA$INST,"\\xa","")
# 
# head(ALLDATA$INST,10)
ALLDATA$INST
ALLDATA$INST<-as_factor(ALLDATA$INST)
str(ALLDATA$INST)
# foo<-tbl_df(foo)
# summary(foo)
ALLDATA$INST<-na.omit(ALLDATA$INST)
ALLDATA$INST[] <- lapply(ALLDATA$INST, as.character)
names(ALLDATA)
trim.trailing <- function (x) sub("\\s+$", "", x)
ALLDATA$INST<-trim.trailing(ALLDATA$INST)
trim.leading <- function (x) sub("^\\s+", "", x)
ALLDATA$INST<-trim.leading(ALLDATA$INST)
head(ALLDATA$INST)

ALLDATA<-ALLDATA %>% arrange(INST)
nchar(ALLDATA$INST[1])
ALLDATA$INST<-gsub("[.]","",ALLDATA$INST)
# foo$value<-gsub("[\\]","",foo$value)
stri_unescape_unicode(ALLDATA$INST)
iconv(ALLDATA$INST, to = "ASCII//TRANSLIT")

# foo$value<-gsub("[']","",foo$value)

# ALLDATA$INST[ALLDATA$INST==""]<-NA
summary(ALLDATA$INST)
ALLDATA<-ALLDATA %>% filter(ALLDATA$INST!="")

# corrections2$name
# x <- rbind(foo[1680,],foo[1679,])
# x <- rbind(foo[4,],foo[3,])
# <a0>Forestry and Forest Products Research Institute
# <a0>Universit<e9> Claude Bernard Lyon
# Encoding(x) <- "UTF-8"
# iconv(x$value, "UTF-8", "ASCII",sub='') ## replace any non UTF-8 by ''
# "faile"
# 
# foo$value<-iconv(foo$value, "ASCII", "UTF-8")
# 
# x_2_rep <- stri_replace_all_regex(x_2, parse(text = '\U00000420'), "P")
# 
# which(foo$value==".")
 
# foo$value<-gsub("[\xa0]","",foo$value)

# foo$value<-gsub("\\\\", "", foo$value, fixed=TRUE)

UNI_LIST<-ALLDATA$INST
summary(UNI_LIST)
levels(UNI_LIST)
UNI_LIST<-as.data.frame(UNI_LIST)
summary(UNI_LIST)
str(UNI_LIST)
UNI_LIST<-distinct(UNI_LIST)
head(UNI_LIST)

# foo<-slice(foo,3:n())
# foo2<-foo
str(UNI_LIST)
# 
# foo2col<-NULL
# foo2col$one<-foo[1:(nrow(foo)/2),]
# foo2col$two<-foo[(nrow(foo)/2+1):nrow(foo),]
write.csv(UNI_LIST, file="uniNameList.csv", row.names = T) #export it as a csv file
# UNI_LIST<-unlist(UNI_LIST)
UNI_LIST<-as.factor(UNI_LIST)
UNI_LIST[] <- lapply(UNI_LIST, as.character)
str(UNI_LIST)
NamesList<-sapply(UNI_LIST$UNI_LIST,agrep,UNI_LIST$UNI_LIST, value=TRUE)
# foo[1343,]









# 
# 
# 
# jarowinkler(foo,foo)
# # ALLDATA$INST<-as.factor(ALLDATA$INST)
# # levels(ALLDATA$INST)
# foo<-as.data.frame(levels(as.factor(foo)))
# str(foo)
# 
# foo<-na.omit(foo)
# foo<-droplevels(foo)
# foo[] <- lapply(foo, as.character)
# 
# head(foo)# foo$V1<-as.character(foo$V1)
# # foo$V2<-as.character(foo$V2)

# foo2<-agrep(foo$V1,foo$V2,value=T)
library(stringdist)
# # foo3<-as.data.frame(stringsim(foo$V1, foo$V2, method = "jaccard"))
# foo<-foo %>% slice(-1)
# head(foo)
# str(foo)
# foo[] <- lapply(foo, as.character)
# foo  <-tbl_df(foo)
# # str_replace_all(foo, "[^[:alnum:]]", "")
# str_replace_all(FIXINST, "[[:punct:]]", "")
# head(FIXINST)
# FIXINST  <- gsub("[^0-9A-Za-z///' ]","" , FIXINST ,ignore.case = TRUE)
# str_replace_all(foo, "[[:punct:]]", " ")
# head(foo2)
# NamesList<-sapply(FIXINST,agrep,FIXINST, value=TRUE)

 NamesDF<-data.frame(
   Name1 = rep(names(NamesList), lapply(NamesList, length)),
   Name2 = unlist(NamesList))
#
 summary(NamesDF)
 str(NamesDF)
#
# Create a column to which you will add a logical condition telling you if the names are an EXACT match
 NamesDF$match<-NA
 NamesDF$match<-NamesDF$Name1==NamesDF$Name2
 match2<-ifelse(NamesDF$match=="TRUE",1,0) #convert TRUE/FALSEto 0/1
 NamesDF<-cbind(NamesDF,match2)
 head(NamesDF,40)
str(NamesDF)
NamesDF<-arrange(NamesDF,Name1,Name2) #organize in alphabetica order
NamesDF<-filter(NamesDF, match==FALSE)  # THIS DELETES ALL NAMES THAT ARE 100% MATCH
head(NamesDF)
# # Convert to chr
NamesDF$Name1<-as.character(NamesDF$Name1)
NamesDF$Name2<-as.character(NamesDF$Name2)
str(NamesDF)
library(RecordLinkage)
# # Calculate the proportional similarity and # changes required to go from one name to another. Package RecordLinkage
NamesDF$Name_sim<-levenshteinSim(NamesDF$Name1, NamesDF$Name2)
NamesDF$Name_dist<-levenshteinDist(NamesDF$Name1, NamesDF$Name2)
#
# # Because this does all pairwise comparisons, it results in redundancy: "e bruna vs emilio bruna" and "emilio bruna vs e bruna"
# # are in different rows, even though they are the same "comparison". This deletes one of the two
NamesDF<-NamesDF[!duplicated(t(apply(NamesDF, 1, sort))),]
# # this arranges them in order from most similar (1 change required) to least similar.
# # look carefully at those with a few changes, as they are likely to be a tiny spelling mistake or difference in intials

#
NamesDF$index<-seq.int(nrow(NamesDF)) #adds a column with an index to make it easier to id which row you need'
NamesDF <- NamesDF %>% select(index, Name1, Name2, Name_sim,Name_dist) #It's kinda ugly, but this rearranges columns (and dumps the "FALSE")
NamesDF <- arrange(NamesDF, desc(Name_sim))
head(NamesDF)
write.csv(NamesDF, file="uniNameCheck.csv", row.names = T) #export it as a csv file
#
#
library(fuzzyjoin)
library<-c("acuminata", "cetosa","vidris","psittacorum","guianensis")
data<-c("acuminate","cetosa","acumminata","acuminatadu","tomentosa")

words <- tbl_df(library)
data <- tbl_df(data)
str(data)
str(words)
str(library)
joined <- data %>%
  stringdist_left_join(words, max_dist = 3)
joined

load("./Data/Consolidated_uni_df.RData")
str(Consolidated.uni.df)
words<-Consolidated.uni.df$uni.name
words<-as.factor(words)
words<-na.omit(words)
words<-droplevels(words)
words[] <- lapply(words, as.character)
words<-tbl_df(words)

FIXINST<-ALLDATA$INST
FIXINST<-na.omit(FIXINST)
head(FIXINST)
# str_replace_all(FIXINST, "[^[:alnum:]]", " ")
# str_replace_all(FIXINST, "[[:punct:]]", " ")
# 
FIXINST  <- gsub("[^0-9A-Za-z///' ]","" , FIXINST ,ignore.case = TRUE)

# gsub("9", "", FIXINST, fixed = TRUE)
# FIXINST[56]
str(FIXINST)
INST<-as.factor(FIXINST)
INST<-as.dataframe(levels(INST))
head(INST)
summary(INST)
INST.na=is.na(INST)
summary(INST.na)
str(INST)
# INST<-as.factor(INST)
INST<-INST %>% slice(-1)

INST<-na.omit(INST)
INST<-droplevels(INST)
str(INST)
summary(INST)
INST[] <- lapply(INST, as.character)
data <- tbl_df(INST)

data<-data %>% arrange (value)
data<-data %>% slice(-1)

words<-words %>% arrange (value)
words<-words %>% slice(-1)

names(data)
names(words)
data<-rename(data,"value"="levels(INST)")
str(words)
str(data)
# 

words[] <- lapply(words, as.character)

data[] <- lapply(data, as.character)
# https://cran.r-project.org/web/packages/fuzzyjoin/vignettes/stringdist_join.html
joined <- data %>%
  stringdist_left_join(words, max_dist = 5)
joined

# data<-as.vector(data)
# words<-as.vector(words)
words2 <- words %>%
  sample_n(10000)

data2 <- data %>%
  sample_n(800)



joined <- data2 %>%
  stringdist_left_join(words2, max_dist = 2)
joined




NameSimilarityDF<-Name.check(foo$V1,foo$V2)
# WHICH ROWS ARE MISSING INST?
MISSING_INST<-ALLDATA %>% select(JOURNAL,YEAR,FIRST_NAME,MIDDLE_NAME, LAST_NAME,INST,COUNTRY) %>% filter(is.na(INST)|INST=="")
MISSING_INST_tbl<-table(MISSING_INST$JOURNAL,MISSING_INST$YEAR)

write.csv(MISSING_INST_tbl,"MISSING_INST_tbl.csv")

write.csv(MISSING_INST,"MISSING_INST.csv")

















  
  
# ALLDATA$INST<-gsub("Univ.", "University", ALLDATA$INST)

# US INSTITUTIONS ONLY

USA_INST<-ALLDATA %>% filter(COUNTRY=="USA")
USA_INST$INST<-as.factor(USA_INST$INST)
USA_INST<-droplevels(USA_INST)
levels(USA_INST$INST)
USA_INST$STATE<-as.character(USA_INST$STATE)
levels(USA_INST$STATE)
USA_INST$STATE
# USA_INST$STATE<-state.abb[grep(foo, state.name)]
# USA_INST$STATE
USA_INST$STATE[USA_INST$STATE=="Alabama"]<-"AL"
USA_INST$STATE[USA_INST$STATE=="Arizona"]<-"AZ"
USA_INST$STATE[USA_INST$STATE=="California"]<-"CA"
USA_INST$STATE[USA_INST$STATE=="Colorado"]<-"CO"
USA_INST$STATE[USA_INST$STATE=="Connecticut"]<-"CT"
USA_INST$STATE[USA_INST$STATE=="DC"]<-"Washington DC"
USA_INST$STATE[USA_INST$STATE=="Florida"]<-"FL"
USA_INST$STATE[USA_INST$STATE=="Idaho"]<-"ID"
USA_INST$STATE[USA_INST$STATE=="Illinois"]<-"IL"
USA_INST$STATE[USA_INST$STATE=="Kentucky"]<-"KY"
USA_INST$STATE[USA_INST$STATE=="Louisiana"]<-"LA"
USA_INST$STATE[USA_INST$STATE=="Lousiana"]<-"LA"
USA_INST$STATE[USA_INST$STATE=="South Dakota"]<-"SD"
USA_INST$STATE[USA_INST$STATE=="Michigan"]<-"MI"
USA_INST$STATE[USA_INST$STATE=="Virginia"]<-"VA"
USA_INST$STATE[USA_INST$STATE=="Utah"]<-"UT"
USA_INST$STATE[USA_INST$STATE=="Texas"]<-"TX"
USA_INST$STATE[USA_INST$STATE=="Tennessee"]<-"TN"
USA_INST$STATE[USA_INST$STATE=="Wisconsin"]<-"WI"
USA_INST$STATE[USA_INST$STATE=="West Virginia"]<-"WV"
USA_INST$STATE[USA_INST$STATE=="VI"]<-"VA"
USA_INST$STATE[USA_INST$STATE=="West Virgina"]<-"WV"
USA_INST$STATE[USA_INST$STATE=="South Carolina"]<-"SC"
USA_INST$STATE[USA_INST$STATE=="Washington"]<-"WA"
USA_INST$STATE[USA_INST$STATE=="Washington "]<-"WA"
USA_INST$STATE[USA_INST$STATE=="Wyoming"]<-"WY"
USA_INST$STATE[USA_INST$STATE=="Maryland"]<-"MD"
USA_INST$STATE[USA_INST$STATE=="Massachusetts"]<-"MA"
USA_INST$STATE[USA_INST$STATE=="Mississippi"]<-"MS"
USA_INST$STATE[USA_INST$STATE=="PE"]<-"PA"
USA_INST$STATE[USA_INST$STATE=="Minnesota"]<-"MN"
USA_INST$STATE[USA_INST$STATE=="North Dakota"]<-"ND"
USA_INST$STATE[USA_INST$STATE=="North Carolina"]<-"NC"
USA_INST$STATE[USA_INST$STATE=="Nevada"]<-"NV"
USA_INST$STATE[USA_INST$STATE=="New Hampshire"]<-"NH"
USA_INST$STATE[USA_INST$STATE=="New Mexico"]<-"NM"
USA_INST$STATE[USA_INST$STATE=="New York"]<-"NY"
USA_INST$STATE[USA_INST$STATE=="Nebraska"]<-"NE"
USA_INST$STATE[USA_INST$STATE=="Montana"]<-"MT"
USA_INST$STATE[USA_INST$STATE=="Missouri"]<-"MO"
USA_INST$STATE[USA_INST$STATE=="Oregon"]<-"AL"
USA_INST$STATE[USA_INST$STATE=="NoState"]<-NA
USA_INST$STATE[USA_INST$STATE=="Pennsylvania"]<-"PA"
USA_INST$STATE[USA_INST$STATE=="Puerto Rico"]<-"PR"

USA_INST$STATE[USA_INST$STATE=="Iowa"]<-"IA"
USA_INST$STATE[USA_INST$STATE=="Vermont"]<-"VT"
USA_INST$STATE[USA_INST$STATE=="Arkansa"]<-"AR"
USA_INST$STATE[USA_INST$STATE=="Arkansas"]<-"AR"
USA_INST$STATE[USA_INST$STATE=="Kansas"]<-"KS"
USA_INST$STATE[USA_INST$STATE=="Georgia"]<-"GA"
USA_INST$STATE[USA_INST$STATE=="Hawaii"]<-"HI"
USA_INST$STATE[USA_INST$STATE=="Oklahoma"]<-"OK"
USA_INST$STATE[USA_INST$STATE=="Indiana"]<-"IN"
USA_INST$STATE[USA_INST$STATE=="Ohio"]<-"OH"
USA_INST$STATE[USA_INST$STATE==""]<-NA
USA_INST$STATE<-as.factor(USA_INST$STATE)
USA_INST$STATE<-droplevels(USA_INST$STATE)
levels(USA_INST$STATE)
summary(USA_INST$STATE)
# nlevels(USA_INST$STATE)

#Need to match up the names used in Carengie Classification with names used in ALLDATA
str(CarnData)
CarnData_names<-CarnData %>% select(NAME,CITY,STABBR,Category)  
CarnData_names<-CarnData_names %>% filter(Category=="Doctoral"|Category=="Masters"|Category=="Baccalaureate"|Category=="Tribal")
CarnData_names$Category<-droplevels(CarnData_names$Category)
summary(CarnData_names)

str(USA_INST)
USA_INST_names<-USA_INST %>% select(INST,CITY,STATE, Inst_Prior_Class)  
USA_INST_names$STATE<-as.character(USA_INST_names$STATE)
USA_INST_names$Inst_Prior_Class<-as.character(USA_INST_names$Inst_Prior_Class)
str(USA_INST_names)
USA_INST_names<-distinct(USA_INST_names)
foo<-USA_INST_names$INST
foo2<-USA_INST_names$INST
foo3<- cbind.data.frame(foo,foo2)
str(foo3)
names(foo3)[1] <- "Name1"
names(foo3)[2] <- "Name2"
foo3$Name1<-as.character(foo3$Name1)
foo3$Name2<-as.character(foo3$Name2)
NamesList<-sapply(foo3$Name1,agrep,foo3$Name2, value=TRUE) 
NamesDF<-data.frame(
  Name1 = rep(names(NamesList), lapply(NamesList, length)),
  Name2 = unlist(NamesList))
NamesDF<-na.omit(NamesDF)
NamesDF<-distinct(NamesDF)
str(NamesList)

NamesDF$match<-NA
NamesDF$match<-NamesDF$Name1==NamesDF$Name2
# match2<-ifelse(NamesDF$match=="TRUE",1,0) #convert TRUE/FALSEto 0/1
# NamesDF<-cbind(NamesDF,match2) 
# head(NamesDF,40)
# str(NamesDF)
NamesDF<-arrange(NamesDF,Name1,Name2) #organize in alphabetica order
NamesDF<-filter(NamesDF, match==FALSE)  # THIS DELETES ALL NAMES THAT ARE 100% MATCH 
head(NamesDF)
# Convert to chr
NamesDF$Name1<-as.character(NamesDF$Name1)
NamesDF$Name2<-as.character(NamesDF$Name2)
str(NamesDF)

NamesDF$Name_sim<-levenshteinSim(NamesDF$Name1, NamesDF$Name2)
NamesDF$Name_dist<-levenshteinDist(NamesDF$Name1, NamesDF$Name2)


# Because this does all pairwise comparisons, it results in redundancy: "e bruna vs emilio bruna" and "emilio bruna vs e bruna"
# are in different rows, even though they are the same "comparison". This deletes one of the two 
NamesDF<-NamesDF[!duplicated(t(apply(NamesDF, 1, sort))),]
# this arranges them in order from most similar (1 change required) to least similar.
# look carefully at those with a few changes, as they are likely to be a tiny spelling mistake or difference in intials


NamesDF$index<-seq.int(nrow(NamesDF)) #adds a column with an index to make it easier to id which row you need'
NamesDF <- NamesDF %>% select(index, Name1, Name2, Name_sim,Name_dist) #It's kinda ugly, but this rearranges columns (and dumps the "FALSE")
NamesDF <- arrange(NamesDF, desc(Name_sim))
head(NamesDF)
#return(NamesDF)
write.csv(NamesDF, file="./Data/InstNameCheck_USA.csv", row.names = T) #export it as a csv file


# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # How similar are things in one and the other 
# # https://www.r-bloggers.com/fuzzy-string-matching-a-survival-skill-to-tackle-unstructured-information/
# # Method 1: using the native R adist
# # It creates a matrix with the Standard Levenshtein distance between the name fields of both sources
# dist.name<-adist(USA_INST_names$INST,CarnData_names$NAME, partial = TRUE, ignore.case = TRUE)
# # We now take the pairs with the minimum distance
# min.name<-apply(dist.name, 1, min)
# match.s1.s2<-NULL  
# for(i in 1:nrow(dist.name))
# {
#   s2.i<-match(min.name[i],dist.name[i,])
#   s1.i<-i
#   match.s1.s2<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=CarnData_names[s2.i,]$NAME, s1name=USA_INST_names[s1.i,]$INST, adist=min.name[i]),match.s1.s2)
# }
# # and we then can have a look at the results
# View(match.s1.s2)
# 
# 
# # Now let’s make use of all meaningful implementations of string distance metrics in the stringdist package:
# # Method 2: applying different string matching methods
# #osa Optimal string aligment, (restricted Damerau-Levenshtein distance).
# #lv Levenshtein distance (as in R’s native adist).
# #dl Full Damerau-Levenshtein distance.
# #hamming Hamming distance (a and b must have same nr of characters).
# #lcs Longest common substring distance.
# #qgram q-gram distance.
# #cosine cosine distance between q-gram profiles
# #jaccard Jaccard distance between q-gram profiles
# #jw Jaro, or Jaro-Winker distance.
# 
# #install.packages('stringdist')
# distance.methods<-c('osa','lv','dl','hamming','lcs','qgram','cosine','jaccard','jw')
# dist.methods<-list()
# for(m in 1:length(distance.methods))
# {
#   dist.name.enh<-matrix(NA, ncol = length(CarnData_names$NAME),nrow = length(USA_INST_names$INST))
#   for(i in 1:length(CarnData_names$NAME)) {
#     for(j in 1:length(USA_INST_names$INST)) { 
#       dist.name.enh[j,i]<-stringdist(tolower(CarnData_names[i,]$NAME),tolower(USA_INST_names[j,]$INST),method = distance.methods[m])      
#       #adist.enhance(source2.devices[i,]$name,source1.devices[j,]$name)
#     }  
#   }
#   dist.methods[[distance.methods[m]]]<-dist.name.enh
# }
# 
# match.s1.s2.enh<-NULL
# for(m in 1:length(dist.methods))
# {
#   
#   dist.matrix<-as.matrix(dist.methods[[distance.methods[m]]])
#   min.name.enh<-apply(dist.matrix, 1, base::min)
#   for(i in 1:nrow(dist.matrix))
#   {
#     s2.i<-match(min.name.enh[i],dist.matrix[i,])
#     s1.i<-i
#     match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=CarnData_names[s2.i,]$NAME, s1name=USA_INST_names[s1.i,]$INST, adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
#   }
# }
# # Let's have a look at the results
# library(reshape2)
# matched.names.matrix<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
# View(matched.names.matrix)
# 



USA_INST$editor_id<-as.factor(USA_INST$editor_id)
summary(USA_INST$editor_id)
USA_INST$INST<-as.character(USA_INST$INST)
# foo<-USA_INST$INST
foo<-USA_INST

foo<-as.data.frame(foo)
foo<-foo %>% rename(NAME=INST)
foo$NAME<-as.character(foo$NAME)
foo$source<-"class"
str(foo)
# foo<-foo %>% filter(NAME != "NA")
# foo <- foo %>% group_by(NAME) %>% filter(row_number(source) == 1) %>% arrange(NAME)
foo <- foo %>% group_by(editor_id) %>% filter(row_number(source) == 1) %>% arrange(NAME)

# foo<-as.data.frame(foo)

CarnData$NAME<-as.character(CarnData$NAME)
foo2<-CarnData
# foo2<-na.omit(as.data.frame(foo2))
# foo2<-foo2 %>% rename(NAME=foo2)
foo2$source<-"carnegie"
# foo2<-unlist(foo2)
# foo2<-as.data.frame(foo2)
str(foo2)
foo3<-full_join(foo,foo2,by="NAME", "Source")
foo3$editor_id<-as.numeric(foo3$editor_id)
foo3<-filter(foo3,editor_id>0)
write.csv(foo3,file="./Data/SciWri17_USA_Ed_Inst_Carneg.csv")



USA_ED_clean<-read_csv("./Data/SciWri17_USA_Ed_Inst_Carneg.csv", col_names=TRUE)

USA_ED_clean<-USA_ED_clean %>% filter(Classification != "NA")
USA_ED_clean<-USA_ED_clean %>% filter(Classification != "(Not classified)")
USA_ED_clean$NAME<-as.factor(USA_ED_clean$NAME)
USA_ED_clean$Classification<-as.factor(USA_ED_clean$Classification)
USA_ED_clean<-droplevels(USA_ED_clean)
levels(USA_ED_clean$NAME)
levels(USA_ED_clean$Classification)
summary(USA_ED_clean$Classification)
summary<-USA_ED_clean$Classification
summary(summary)
foo4<-foo3
str(foo4)
foo5<-stringdistmatrix(foo3$NAME,method="dl")


INST<-na.omit(INST)
INST<-as.data.frame(INST)
summary(INST)
str(INST)
INST$INST<-as.factor(INST$INST)

inst_names <- INST %>% group_by(INST) %>% filter(row_number(COUNTRY) == 1) %>% arrange(INST)
write.csv(inst_names,file="INST_names_class.csv")



percents<-c(87.4,5.2,3.2,2.1,1.4,1.1,0.1)
lbls <- c("1-Doctoral Univ", "2-Fed Agency", "3-Garden/Museums/NGO/Private Inst.", "4-Baccalaureate Colleges", "5-Smithsonian", "6-Master's Colleges", "7-Industry")
data_barplot<-as.data.frame(cbind(percents,lbls))
data_barplot$percents<-as.numeric(as.character(data_barplot$percents))
# geom_bar is designed to make it easy to create bar charts that show
# counts (or sums of weights)
g <- ggplot(data_barplot, aes(lbls))+theme_bw()+theme(axis.text.x=element_text(angle = -45, hjust = 0))
plot<-g + geom_bar(aes(weight = percents), fill="darkblue",color="darkblue")

str(data_barplot)
# Clear the environment 
rm(list=ls())

# Import list of world universities downloaded from this GitHub Repo: https://github.com/endSly/world-universities-csv
global_uni1 = read_csv("https://raw.githubusercontent.com/endSly/world-universities-csv/master/world-universities.csv", col_names=FALSE)
global_uni1<-global_uni1 %>% rename(ISO2=X1,uni.name=X2,website=X3)
global_uni1$uni.name<-as.factor(global_uni1$uni.name)
global_uni1$ISO2<-as.factor(global_uni1$ISO2)
global_uni1$website<-as.factor(global_uni1$website)
str(global_uni1)
summary(global_uni1)


# library("RSQLite")
# library("R.utils")
#download this sql: https://github.com/gedex/World-University-Names-Database/blob/master/world_university_names.sql
numLines <- R.utils::countLines("./Data/world_university_names.sql")
FullUniDB <- readLines("./Data/world_university_names.sql",n=numLines)


# CREATE THE DATABASE OF COUNTRY CODES & NAMES
countries.df<-FullUniDB[34:279]

countries.df<-gsub("(", "", countries.df, fixed=TRUE)
countries.df<-gsub("'),", "", countries.df, fixed=TRUE)
countries.df<-gsub("'", "", countries.df, fixed=TRUE)
countries.df<-gsub(";", "", countries.df, fixed=TRUE)
countries.df<-gsub(")", "", countries.df, fixed=TRUE)
countries.df<-as.data.frame(countries.df)
countries.df$countries.df<-as.character(countries.df$countries.df)
countries.df<-separate(countries.df, countries.df, c("country.ID", "ISO2","ISO3","country","modifier"), sep = ", ", remove = FALSE, convert = FALSE, extra = "merge", fill = "right")
countries.df$country[countries.df$ISO3 == "VGB"]  <- "British Virgin Islands"
countries.df$country[countries.df$ISO3 == "PRK"]  <- "Democratic Peoples Republic of Korea (NK)"
countries.df$country[countries.df$ISO3 == "COD"]  <- "Democratic Republic of the Congo"
countries.df$country[countries.df$ISO3 == "FSM"]  <- "Federated States of Micronesia"
countries.df$country[countries.df$ISO3 == "VIR"]  <- "US Virgin Islands"
countries.df$country[countries.df$ISO3 == "KOR"]  <- "Republic of Korea (SK)"
countries.df$country[countries.df$ISO3 == "PSE"]  <- "Palestinian Territory, Occupied"
countries.df$country[countries.df$ISO3 == "IRN"]  <- "Iran, Islamic Republic of"
countries.df$country[countries.df$ISO3 == "TZA"]  <- "Tanzania, United Republic of"
countries.df<-countries.df %>% select(-countries.df,-modifier)
countries.df$country.ID<-as.factor(countries.df$country.ID)
countries.df<-countries.df %>% rename(ISO2=ISO2 ,ISO3=ISO3 ,country=country )
countries.df$ISO2<-as.factor(countries.df$ISO2)
countries.df$ISO3<-as.factor(countries.df$ISO3)
countries.df$country<-as.factor(countries.df$country)



# CREATE THE UNIVERSITY DATABASE #1
unis.df <-FullUniDB[300:9537]
# NB better to go in 1 comma, 1in one comma, in from last comma (URL and then the rest is inst name)

# Change to Split at first comma, then at second, then done.
unis.df<-gsub("(", "", unis.df, fixed=TRUE)
unis.df<-gsub("(", "", unis.df, fixed=TRUE)
unis.df<-gsub(")", "", unis.df, fixed=TRUE)
unis.df <-gsub(", '", ",", unis.df , fixed=TRUE)
unis.df <-gsub("''", "'", unis.df , fixed=TRUE)
unis.df <-gsub("',", ",", unis.df , fixed=TRUE)
unis.df <-gsub("'", "", unis.df , fixed=TRUE)
unis.df <-unis.df %>% str_split(",",simplify=TRUE) 
unis.df <-as.data.frame(unis.df)
unis.df <- sapply(unis.df[1:ncol(unis.df)],as.character)
unis.df <-as.data.frame(unis.df)

str(unis.df)
unis.df$V3<-trimws(unis.df$V3)
unis.df$V4<-trimws(unis.df$V4)
unis.df$V5<-trimws(unis.df$V5)
unis.df$V6<-trimws(unis.df$V6)

unis.df<-unis.df%>% unite_("uni.name", c("V3","V4","V5","V6"), sep=" ", remove=TRUE)
unis.df<-unis.df %>% rename(uni.ID=V1,country.ID=V2) %>% select(-uni.ID,-V7)

#Remove some extra spaces
unis.df$uni.name <-trimws(unis.df$uni.name, which = "right")
unis.df$uni.name<-gsub("  ", " ", unis.df$uni.name, fixed=TRUE)
#delete some prolematic rows
unis.df<-unis.df[-c(1024, 2061, 2958, 3953,5084,6156,7144,7895,8884),]
unis.df$country.ID<-droplevels(unis.df$country.ID)
unis.df$country.ID<-trimws(unis.df$country.ID)

###################################################################
# CREATE THE UNIVERSITY DATABASE #2 (this one has the web addresses)
unis.web.df <-FullUniDB[9559:26356]
unis.web.df <-gsub("(", "", unis.web.df , fixed=TRUE)
unis.web.df <-gsub(", '", ",", unis.web.df , fixed=TRUE)
unis.web.df <-gsub("',", ",", unis.web.df , fixed=TRUE)
unis.web.df <-gsub("/'),", "", unis.web.df , fixed=TRUE)
unis.web.df <-gsub("''", "'", unis.web.df , fixed=TRUE)
unis.web.df <-gsub("  ", " ", unis.web.df , fixed=TRUE)

unis.web.df <-unis.web.df %>% str_split(",",n=3,simplify=TRUE) 
unis.web.df<-as.data.frame(unis.web.df)
unis.web.df$V3<-as.character(unis.web.df$V3)
str(unis.web.df)
unis.web.df[1:10,]
# https://stackoverflow.com/questions/24938616/string-split-on-last-comma-in-r
uniweb<-str_split(unis.web.df$V3, ",\\s*(?=[^,]+$)", simplify=TRUE)
uniweb<-as.data.frame(uniweb)

unis.web.df<-bind_cols(unis.web.df, uniweb)
unis.web.df<-unis.web.df %>% rename(uni.ID=V1,country.ID=V2,original=V3,uni.name=V11,website=V21)
unis.web.df<-unis.web.df %>% select(-original, -uni.ID)
rm(uniweb)
#Remove asterisk and some extra spaces
unis.web.df$uni.name <-gsub("*", "", unis.web.df$uni.name , fixed=TRUE)
unis.web.df$uni.name <-trimws(unis.web.df$uni.name, which = "right")
# add slash to match website in other df 
unis.web.df$website <- paste(unis.web.df$website, "/", sep="")

unis.web.df$country.ID<-as.factor(unis.web.df$country.ID)


unis.web.df<-unis.web.df[-c(682,1307, 1964, 2648, 3353, 4009, 4673, 5234, 5878, 6524, 7202, 7870, 8598, 9264,9900,10558,11144,11738,12379,13066,13770,14497,15216,15922,16617),]
unis.web.df$country.ID<-droplevels(unis.web.df$country.ID)
unis.web.df$country.ID<-trimws(unis.web.df$country.ID)

## NOTE THAT SOME OF THE UNIVERSITIES HAVE WEBSOTES BUT NO UNINAMES!!!



###################################
#add ISO2 and ISO3 to the uni databases
####################################
# for global_uni1
global_uni1<-left_join(global_uni1,countries.df, by="ISO2")
global_uni1$ISO2<-as.factor(global_uni1$ISO2)
global_uni1<-global_uni1 %>% select(country.ID,country,ISO2,ISO3,uni.name,website)

# for unis.df
unis.df<-left_join(unis.df,countries.df, by="country.ID")

# for unis.web.df
unis.web.df<-left_join(unis.web.df,countries.df, by="country.ID")
unis.web.df<-unis.web.df %>% select(country.ID,country,ISO2,ISO3,uni.name,website)

# Consolidate the three
# First might be easiest to convert the diacritical marks / accents
# use stri_trans_general() from stringi library
unis.web.df$uni.name2<-stri_trans_general(unis.web.df$uni.name, "Latin-ASCII")
unis.df$uni.name2<-stri_trans_general(unis.df$uni.name, "Latin-ASCII")
global_uni1$uni.name2<-stri_trans_general(global_uni1$uni.name, "Latin-ASCII")

# The remove any hyphens or commas
unis.web.df$uni.name2<-gsub(", ", " ", unis.web.df$uni.name2, fixed=TRUE)
unis.df$uni.name2<-gsub(", ", " ", unis.df$uni.name2 , fixed=TRUE)
global_uni1$uni.name2<-gsub(", ", " ", global_uni1$uni.name2 , fixed=TRUE)

# Are there any in unis.df NOT in unis.web.df when searching by uni.name?
foo1<-anti_join(unis.df,unis.web.df,by="uni.name2") #6523 of the 9229 
#do similarity analyses of these with unis.web.df$names2 and after removing any similar add to the master list

# Are there any in global_uni1 NOT in unis.web.df when searching by uni.name?
foo2<-anti_join(global_uni1,unis.web.df,by="website")  
foo3<-anti_join(global_uni1,unis.web.df,by="uni.name2") 
foo4<-bind_rows(foo2,foo3) #put them together
foo5<-unique(foo4,by="uni.name2") #remove the duplicates

#put the two datafiles together 
foo6<-bind_rows(foo1,foo5)
foo7<-unique(foo6)

# Consolidate them with unis.web.df
Consolidated.uni.df<-rbind(foo7,unis.web.df)
Consolidated.uni.df$uni.ID<-1:nrow(Consolidated.uni.df)
rm(foo1,foo2,foo3,foo4,foo5,foo6,foo7,global_uni1,unis.df,unis.web.df)

Consolidated.uni.df<-Consolidated.uni.df %>% select(uni.ID,country.ID,ISO2,ISO3,country,uni.name,uni.name2,website)
# to get the initials of each uni I just deleted lower-case letters http://r.789695.n4.nabble.com/Extract-upper-case-letters-td4634664.html
Consolidated.uni.df$initials<-gsub("[^::A-Z::]","", Consolidated.uni.df$uni.name2)
Consolidated.uni.df$uni.code <- paste(Consolidated.uni.df$ISO3,"-",Consolidated.uni.df$initials,  sep="")

# Now do a similarity analysis of name remove the duplicates
# DO SOME ERROR CORRECTION:
  # 1) SOME ARE MISSING UNI NAMES, EG, INST NAME IS "UNIVERSITY": COUNT CHARACTERS,SORT, AND LOOK AT ONES WITH LEAST CAHARACTERS

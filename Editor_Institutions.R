
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
# rm(list=ls())


##############################################################
##############################################################
#
# UPLOAD & STANDARDIZE DATA ON CARNEGIE CLASSIFICATIONS
#
##############################################################
##############################################################

CarnCats<-read_csv("./Data/carnegie/CarnegCategories_2015.csv", col_names = TRUE)
CarnCats<-CarnCats %>% select(Value,Label_1) %>% rename(BASIC2015=Value, Classification=Label_1)
str(CarnCats)
CarnCats<-as.data.frame(CarnCats)

CarnRanks<-read_csv("./Data/carnegie/CarnegRankings_2015_Reduced.csv", col_names = TRUE)
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
summary(CarnData)

rm(CarnCats,CarnRanks)

##############################################################
##############################################################
#
# UPLOAD AND CLEAN DATA FROM DRYAD
#
##############################################################
##############################################################

Cho<-read.csv("./Data/dryad/Dryad_Cho_V2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
# minor corrections to Cho_V2 Caught by Bruna's 2017 SciWri Class
Cho$COUNTRY[Cho$LAST_NAME=="Pedreira" & Cho$FIRST_NAME=="Carlos"] <- "Brazil"
Cho$COUNTRY[Cho$LAST_NAME=="Benbi" & Cho$FIRST_NAME=="Dinesh"] <- "India"
Cho$COUNTRY[Cho$LAST_NAME=="Borras" & Cho$FIRST_NAME=="Lucas"] <- "Argentina"
Cho$COUNTRY[Cho$LAST_NAME=="Esker" & Cho$FIRST_NAME=="Paul"] <- "Costa Rica"
Cho$COUNTRY[Cho$LAST_NAME=="Buresh" & Cho$FIRST_NAME=="Roland"] <- "USA"

Cho$COUNTRY<-as.character(Cho$COUNTRY)
Cho$COUNTRY[Cho$COUNTRY=="UK"] <- "United Kingdom"
Cho$COUNTRY[Cho$COUNTRY=="USSR"] <- "Russia"
Cho$COUNTRY[Cho$COUNTRY=="ITALY"] <- "Italy"
Cho$COUNTRY[Cho$COUNTRY=="United States"] <- "USA"
Cho$COUNTRY[Cho$COUNTRY=="Usa"] <- "USA"
Cho$COUNTRY[Cho$COUNTRY=="UsA"] <- "USA"
# Convert Country back to factor
Cho$COUNTRY<-as.factor(Cho$COUNTRY)
droplevels(Cho$COUNTRY)

# Data on Editors from Espin et al. 2017 
Espin<-read.csv("./Data/dryad/Dryad.Espin.v1.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )


#Add a tag
Espin$source<-"EspinDryad"
Cho$source<-"ChoDryad"
#Bind the datasets together
DRYADDATA<-bind_rows(Cho,Espin, id=NULL)
#Delete these columns
DRYADDATA<-DRYADDATA %>% select(-geo.code,-INCOME_LEVEL,-REGION)
levels(as.factor(DRYADDATA$JOURNAL))
rm(Cho,Espin)

# TODO: NEED TO USE THE ALLDATA FILE, WHEN CORRECTED, TO MAKE ANY NECESSARY CORRECTIONS TO THE 
# ALREADY ARCHIVED CHO AND ESPIN DATA.  THIS WILL ALSO GIVE YOU THE MOST UP TO DATE ARCHIVE 

##############################################################
##############################################################
#
# UPLOAD RAW EDITOR DATA FROM INDIVIDUAL JOURNALS
#
##############################################################
##############################################################

folder <- "./Data/sciwri17_raw_data/"      # path to folder that holds multiple .csv files
file_list <- list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder

# read in each .csv file in file_list and create a data frame with the same name as the .csv file
# Note the ASCII encoding to try and deal with the UTF-8 characters
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

################################################
# UPLOAD AND STANDARDIZE AUK AND CONDOR
# THESE WILL *NOT* BE IN THE ED INST PAPER
################################################

folder <- "./Data/hurtado_data/"      # path to folder that holds multiple .csv files
file_list <- list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder

# read in each .csv file in file_list and create a data frame with the same name as the .csv file
# Note the ASCII encoding to try and deal with the UTF-8 characters
for (i in 1:length(file_list)){
  assign(file_list[i], 
         read.csv(paste(folder, file_list[i], sep=''),na.strings = c("","NA"), encoding = "ASCII")
  )}




##############################################################
##############################################################
#
# CLEAN-UP BY JOURNAL
#
##############################################################
############################################################### 

# ##################################
# AUK
# ##################################
AUK_raw<-AUK.csv
source("clean_AUK.R")
AUK<-clean_AUK(AUK_raw)
rm(AUK_raw,AUK.csv)

# ##################################
# CONDOR
# ##################################
CONDOR_raw<-CONDOR.csv
source("clean_CONDOR.R")
CONDOR<-clean_CONDOR(CONDOR_raw)
rm(CONDOR_raw,CONDOR.csv)
# 
# AMNAT<-AMNAT.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# AG<-AGRONOMY_data_11.02.2017.csv %>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# AJB<-AJB.csv %>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)                      
# AMNAT<-AMNAT.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# AREES<-AREES.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# BIOCON<-BIOCON_TS.csv
# # BIOCON<-BIOCON_TS.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# BITR<-BITR.csv %>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# CONBIO<-CONBIO_EKB.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# ECOG<-ECOGRAPHY_5112017.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# ECOL<-ECOLOGY.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# EVOL<-EVOL.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# FEM<-FEM_7112017.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# GCB<-GCBdata.csv
# JANE<-JANE.csv  %>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# JAPE<-JAPE_new.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# JBIOG<-JBIOG.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# JECOL<-JECOL_new.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# JTE<-JTE_2015.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# # JZOOL HAS MULTIPLE DATASETS TO UPLOAD
# JZOOL1<-JZOOL17nov.csv
# JZOOL2<-JZOOL.csv
# LECO<-LECO_2017.csv %>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# MARECOL<-MARECOL_21July2018.csv %>% select(JOURNAL,YEAR, NAME,INST,UNIT,CITY,STATE,COUNTRY)
# NAJFM<-NAJFM_21july2018.csv %>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# NEWPHYT<-NEWPHYT_21july2018.csv %>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# OECOL<-OECOL.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)                   
# OIKOS<-OIKOS_21july2018.csv %>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# PLANTECOL<-PLANTECOL_new.csv %>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY)
# # FUNECOL HAS MULTIPLE DATASETS TO UPLOAD
# FUNECOL1<-FUNECOLdata_Allen_EB_1dec.csv%>% select(JOURNAL,YEAR, FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,COUNTRY) #NO UNIT, CITY, STATE
# FUNECOL2<-FUNECOL_data_11.03.2017.csv%>% select(JOURNAL,YEAR, FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY,editor_id) #NO UNIT, CITY, STATE



##############################################################
# FUNCTIONAL ECOLOGY
##############################################################


# FUNECOL HAS MULTIPLE DATASETS TO UPLOAD
FUNECOL1<-FUNECOLdata_Allen_EB_1dec.csv
FUNECOL2<-FUNECOL_data_11.03.2017.csv

# REVIEWING AND CORRECTING
FUNECOL<-full_join(FUNECOL1,FUNECOL2,by=c("YEAR","FIRST_NAME","LAST_NAME"))
FUNECOL$COUNTRY.x<-gsub("New Zeland","New Zealand", FUNECOL$COUNTRY.x)

rm(FUNECOL1,FUNECOL2)
colnames(FUNECOL)
summary(FUNECOL$ISSUE.x==FUNECOL$ISSUE.y)

#######
# TODO: NEED TO CORRECT THE ONES BELOW WHERE CATEGORY X AND Y DON'T MATCH.
summary((as.character(FUNECOL$CATEGORY.x)==as.character(FUNECOL$CATEGORY.y)))
which((as.character(FUNECOL$CATEGORY.x)==as.character(FUNECOL$CATEGORY.y))==FALSE)
which(is.na((as.character(FUNECOL$CATEGORY.x))==(as.character(FUNECOL$CATEGORY.y))))

FUNECOL[203,]
FUNECOL[583,]
FUNECOL[584,]
FUNECOL[582,]
FUNECOL[581,]
FUNECOL[580,]
FUNECOL[579,]
FUNECOL[202,]
FUNECOL[201,]
FUNECOL[167,]
FUNECOL[162,]

####
# FUNECOL$TITLE.y<-NULL
# FUNECOL<-FUNECOL %>% rename("TITLE"="TITLE.x")


# FUNECOL$ISSUE.y<-NULL
# FUNECOL<-FUNECOL %>% rename("ISSUE"="ISSUE.x")
summary(FUNECOL$NAME.x==FUNECOL$NAME.y)
FUNECOL$NAME.y<-NULL
FUNECOL<-FUNECOL %>% rename("NAME"="NAME.x")



summary(FUNECOL$JOURNAL.x==FUNECOL$JOURNAL.y)
summary(FUNECOL$MIDDLE_NAME.x==FUNECOL$MIDDLE_NAME.y)
FUNECOL$COUNTRY.x<-as.character(FUNECOL$COUNTRY.x)
FUNECOL$COUNTRY.y<-as.character(FUNECOL$COUNTRY.y)
summary(FUNECOL$COUNTRY.x==FUNECOL$COUNTRY.y)
country_fix<-which((FUNECOL$COUNTRY.x==FUNECOL$COUNTRY.y)=="FALSE")
country_fix.df<-FUNECOL[country_fix,]  #KEEP COUNTRIES IN UK AS ORIGINAL NAMES< CONVERT TO UK LATER
summary(FUNECOL$JOURNAL.x==FUNECOL$JOURNAL.y)
FUNECOL$INST.x<-as.character(FUNECOL$INST.x)
FUNECOL$INST.y<-as.character(FUNECOL$INST.y)
INST_fix<-which((FUNECOL$INST.x==FUNECOL$INST.y)=="FALSE")
INST_fix.df<-FUNECOL[INST_fix,]  
INST_fix.df<-filter(INST_fix.df,INST.y!="Noinst")
INST_fix.df<-filter(INST_fix.df,INST.y!="NoInst")
FUNECOL$INST.x<-gsub("Colorado State","Colorado State University", FUNECOL$INST.x)
FUNECOL$INST.x<-gsub("Kentucky","University of Kentucky", FUNECOL$INST.x)
FUNECOL$INST.x<-gsub("Aberdeen","University of Aberdeen", FUNECOL$INST.x)
FUNECOL$INST.x<-gsub("Massachusetts","University of Massachusetts at Amherst", FUNECOL$INST.x)
FUNECOL$INST.x<-gsub("Utah State","Utah State University", FUNECOL$INST.x)
FUNECOL$INST.x<-gsub("Exeter","University of Exeter", FUNECOL$INST.x)
FUNECOL$INST.x<-gsub("Edinburgh","University of Edinburgh", FUNECOL$INST.x)
FUNECOL$INST.x<-gsub("Sheffield","University of Sheffield", FUNECOL$INST.x)
FUNECOL$INST.x<-gsub("Stellenbosch","University of Stellenbosch", FUNECOL$INST.x)
summary(FUNECOL$INST.x==FUNECOL$INST.y)
INST_fix<-which((FUNECOL$INST.x==FUNECOL$INST.y)=="FALSE")
INST_fix.df<-FUNECOL[INST_fix,]  
FUNECOL<-FUNECOL %>% select(-JOURNAL.y,-INST.y,-MIDDLE_NAME.y,-COUNTRY.y,-VOLUME.y,-ISSUE.y) %>% rename("JOURNAL"="JOURNAL.x","INST"="INST.x","VOLUME"="VOLUME.x","ISSUE"="ISSUE.x","MIDDLE_NAME"="MIDDLE_NAME.x","COUNTRY"="COUNTRY.x")
rm(INST_fix.df,INST_fix,country_fix,country_fix.df)

FUNECOL$INST[FUNECOL$editor_id==1878 & FUNECOL$LAST_NAME=="Soler"]<-"CSIC"

levels(FUNECOL$UNIT)
FUNECOL$UNIT<-as.character(FUNECOL$UNIT) # change to 'character'
FUNECOL$UNIT[FUNECOL$editor_id==1878 & FUNECOL$LAST_NAME=="Soler"]<-"Estacion Experimental de Zonas Aridas"
FUNECOL$UNIT<-as.factor(FUNECOL$UNIT) # change back to factor 
FUNECOL$UNIT<-droplevels(FUNECOL$UNIT) 

levels(FUNECOL$CATEGORY.x)
levels(FUNECOL$CATEGORY.y)
summary(FUNECOL$CATEGORY.x==FUNECOL$CATEGORY.y)
head(FUNECOL,10)
FUNECOL<-FUNECOL %>% rename("TITLE"="TITLE.x","CATEGORY"="CATEGORY.y")


rm(FUNECOLdata_Allen_EB_1dec.csv,FUNECOL_data_11.03.2017.csv,FUNECOLdata_Allen.csv)

##############################################################
# LANDSCAPE ECOLOGY
##############################################################
LECO_raw<-LECO_2017.csv 
source("clean_LECO.R")
LECO<-clean_LECO(LECO_raw)
rm(LECO_2017.csv,LECO_raw)

##############################################################
# CONBIO
# 
##############################################################
CONBIO_raw<-CONBIO_EKB.csv
source("clean_CONBIO.R")
CONBIO<-clean_CONBIO(CONBIO_raw)
rm(CONBIO_EKB.csv,CONBIO_raw)


##############################################################
# JZOOLOGY
##############################################################

# JZOOL HAS MULTIPLE DATASETS TO UPLOAD
JZOOL1<-JZOOL17nov.csv
JZOOL2<-JZOOL.csv
source("clean_JZOOL.R")
JZOOL<-clean_JZOOL(JZOOL1,JZOOL2)
rm(JZOOL.csv,JZOOL17nov.csv,JZOOL1,JZOOL2)
##############################################################
# NEW PHYT
##############################################################

NEWPHYT_raw<-NEWPHYT_21july2018.csv
source("clean_NEWPHYT.R")
NEWPHYT<-clean_NEWPHYT(NEWPHYT_raw)
rm(NEWPHYT_21july2018.csv,NEWPHYT_raw)

##############################################################
# BIOLOGICAL CONSERVATION
##############################################################
# TODO: STILL NEEED TO need to add the 2x names, fill in any Missing for INST, UNIT, etc, and the FILL the columns
BIOCON_raw<-BIOCON_TS.csv
source("clean_BIOCON.R")
BIOCON<-clean_BIOCON(BIOCON_raw)
rm(BIOCON_TS.csv,BIOCON_raw)

##############################################################
# AGRONOMY
##############################################################

AG_raw<-AGRONOMY_data_11.02.2017.csv 
source("clean_AGRON.R")
AG<-clean_AGRON(AG_raw)
rm(AGRONOMY_data_11.02.2017.csv,AG_raw)

##############################################################
# FEM
##############################################################

FEM_raw<-FEM_7112017.csv
source("clean_FEM.R")
# TODO: ## NOT CORRECT EITHER INST OR CITY STATE!!!
# FEM 1994 Colorado State University     USA       775    Douglas           A   Maguire      Seattle Washington              <NA>   <NA>

FEM<-clean_FEM(FEM_raw)
rm(FEM_7112017.csv)



##############################################################
# J ANIMAL ECOLOGY
##############################################################

JANE_raw<-JANE.csv
source("clean_JANE.R")
JANE<-clean_JANE(JANE_raw)
rm(JANE.csv,JANE_raw)

##############################################################
# J BIOGEOGRAPHY
##############################################################
JBIOG_raw<-JBIOG.csv
source("clean_JBIOG.R")
JBIOG<-clean_JBIOG(JBIOG_raw)
rm(JBIOG_raw,JBIOG.csv)
##############################################################
# PLANT ECOLOGY
##############################################################
PLANTECOL_raw<-PLANTECOL_new.csv 
source("clean_PLANTECOL.R")
PLANTECOL<-clean_PLANTECOL(PLANTECOL_raw)
rm(PLANTECOL_raw,PLANTECOL_new.csv)
##############################################################
# EVOLUTION
##############################################################
EVOL_raw<-EVOL.csv
source("clean_EVOL.R")
EVOL<-clean_EVOL(EVOL_raw)
rm(EVOL.csv,EVOL_raw)
##############################################################
# AMERICAN NATURALIST
##############################################################

summary(AMNAT.csv)
summary(AmNat0614.csv)


# TODO: FIGURE OUT WTF AM NAT
# TODO: AM NAT Needs institutions from 2006-2014 put in
# NEED TO FIGURE OUT IF AMNAT, AMNAT0614,orAMNATEKB!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
# AMNATpre2005<-filter(AMNAT.csv, YEAR<2006)
# summary(AMNATpre2005)
# AMNATpost2005<-filter(AMNAT.csv, YEAR>2005)
# summary(AMNATpost2005)


# FIND THE DUPLICATES, DELETE THE ONE THAT DOESN'T HAVE THE INST
ALL_AMNAT<-bind_rows(AMNAT.csv,AmNat0614.csv) %>% arrange(YEAR,LAST_NAME,FIRST_NAME,INST)
head(ALL_AMNAT,20)
summary(ALL_AMNAT)


foo<-ALL_AMNAT %>% 
  group_by(YEAR,LAST_NAME,FIRST_NAME) %>% 
  filter(n()>1) %>% 
  slice(1) %>% arrange(YEAR)



ALL_AMNAT %>% group_by(YEAR,LAST_NAME,FIRST_NAME) %>% filter(n()>1) %>% summarize(n=n())


# AmNat0614<-AmNat0614.csv %>% select(LAST_NAME,FIRST_NAME,YEAR,INSTITUTION,COUNTRY,GENDER)
# AMNAT_left<-left_join(AMNAT2006, AmNat0614, by="LAST_NAME", "YEAR")
# 

# 
# str(AmNat0614)
# str(AMNAT2006)
# antijoiun1_AMNAT<-anti_join(AMNAT2006, AmNat0614, by="YEAR","editor_id")
# 
AMNAT<-AMNAT.csv

AMNAT$INST<-as.character(AMNAT$INST)
AMNAT$CITY[AMNAT$INST=="University of Hawaii" & AMNAT$LAST_NAME=="Palumbi"]<-"Honolulu"
AMNAT$STATE[AMNAT$INST=="University of Hawaii" & AMNAT$LAST_NAME=="Palumbi"]<-"HI"
AMNAT$INST[AMNAT$INST=="University of Hawaii" & AMNAT$LAST_NAME=="Palumbi"]<-"University of Hawaii at Manoa"
AMNAT<-AMNAT %>% rename("TITLE"="TITLE.x")
head(AMNAT,10)
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(AMNAT)
# Then need to add a function to upload the corrections/additions
##

rm(AMNAT_EKB.csv,AMNAT.csv,AmNat0614.csv)

##############################################################
# JAPE
##############################################################
JAPE_raw<-JAPE_new.csv
source("clean_JAPE.R")
JAPE<-clean_JAPE(JAPE_raw)
rm(JAPE_new.csv)
##############################################################
# OECOLOGIA
##############################################################
OECOL_raw<-OECOL.csv
source("clean_OECOL.R")
OECOL<-clean_OECOL(OECOL_raw)
rm(OECOL.csv,OECOL_raw)
##############################################################
# GCB
##############################################################
GCB_raw<-GCBdata.csv
# TODO: 

# 2) get title abbreviations 
# 3) Make consistent 
# 4) only have data 1995-2007 (1995 was vol 1), 2008-2015 
# 5) disambiguate editors and add editor id numbers

source("clean_GCB.R")
GCB<-clean_GCB(GCB_raw)
rm(GCBdata.csv,GCB_raw)
##############################################################
# NAJFM
##############################################################
NAJFM_raw<-NAJFM_21july2018.csv 
source("clean_NAJFM.R")
NAJFM<-clean_NAJFM(NAJFM_raw)
rm(NAJFM_21july2018.csv,NAJFM_raw)
##############################################################
# MARECOL
##############################################################

# TODO: MARECOL
# 1) SPLIT name
# 2) Disambiguate and add editor ID
# 3) NEEDS INSTITIONS

MARECOL_raw<-MARECOL_21July2018.csv 
source("clean_MARECOL.R")
MARECOL<-clean_MARECOL(MARECOL_raw)
rm(MARECOL_21July2018.csv,MARECOL_raw)

##############################################################
# AJB
##############################################################

AJB_raw<-AJB.csv
source("clean_AJB.R")
AJB<-clean_AJB(AJB_raw)
rm(AJB.csv,AJB_raw)
##############################################################
# OIKOS
##############################################################
OIKOS_raw<-OIKOS_21july2018.csv
source("clean_OIKOS.R")
OIKOS<-clean_OIKOS(OIKOS_raw)
rm(OIKOS_21july2018.csv,OIKOS_raw)
##############################################################
# ECOGRAPHY
##############################################################

ECOG_raw<-ECOGRAPHY_5112017.csv
source("clean_ECOG.R")
ECOG<-clean_ECOG(ECOG_raw)
rm(ECOGRAPHY_5112017.csv,ECOG_raw)
##############################################################
# BITR
##############################################################

BITR<-BITR.csv 
head(BITR,10)
BITR<-rename(BITR,"TITLE"="TITLE.x")
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(MEPS)
# Then need to add a function to upload the corrections/additions
##

rm(BITR.csv)
##############################################################
# JECOL
##############################################################

JECOL<-JECOL_new.csv

head(JECOL,10)
JECOL<-rename(JECOL,"TITLE"="TITLE.x")
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(MEPS)
# Then need to add a function to upload the corrections/additions
##


rm(JECOL_new.csv)
##############################################################
# AREES
##############################################################
AREES<-AREES.csv

head(AREES,10)
AREES<-rename(AREES,"TITLE"="TITLE.x")
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(MEPS)
# Then need to add a function to upload the corrections/additions
##

rm(AREES.csv)
##############################################################
# JTE
##############################################################

JTE<-JTE_2015.csv

head(JTE,10)
JTE<-rename(JTE,"TITLE"="TITLE.x")
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(MEPS)
# Then need to add a function to upload the corrections/additions
##
rm(JTE_2015.csv)

######################
# MEEPS
######################
# TODO: MEPS: NOT EVEN AN EXCEL SHEET,NEED TO SEE WHICH ONES ARE MISSING INSTITUTIONS

##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(MEPS)
# Then need to add a function to upload the corrections/additions
##

##############################################################
# ECOLOGY
##############################################################

ECOL<-ECOLOGY.csv
head(ECOL,10)
rm(ECOLOGY.csv)

##############################################################
##############################################################
#
# BIND THEM ALL UP INTO ONE FILE  
#
##############################################################
##############################################################
# with ALL
ALLDATA<-bind_rows(MARECOL,NAJFM,NEWPHYT,JZOOL,GCB,
                   OIKOS,BITR,LECO,PLANTECOL,OECOL,
                   JTE,JECOL,JBIOG,JAPE,JANE,FUNECOL,
                   FEM,EVOL,ECOL,ECOG,CONBIO,BIOCON,
                   AREES,AMNAT,AJB,AG)


# No Agronomy, MARECOL, NAJFM, JZOOL, OECOL, JANE
ALLDATA<-bind_rows(NEWPHYT,GCB,
                   OIKOS,BITR,LECO,PLANTECOL,
                   JTE,JECOL,JBIOG,JAPE,FUNECOL,
                   FEM,EVOL,ECOL,ECOG,CONBIO,BIOCON,
                   AREES,AMNAT,AJB)

head(ALLDATA,10)
str(ALLDATA)
ALLDATA <- ALLDATA %>% select(-NAME) %>% select(JOURNAL,YEAR,VOLUME,ISSUE,editor_id,FIRST_NAME,MIDDLE_NAME,
                                                LAST_NAME,TITLE,CATEGORY,CATEGORY.x,INST,OLD_INST,INST.2,
                                                UNIT,CITY,STATE,COUNTRY,COUNTRY_Prior_Class,geo.code,
                                                 geo.code_Prior_Class,NOTES,GENDER,TITLE)
# ############
# # check to see what is diff between category and category.x
# ALLDATA$check<-ALLDATA$CATEGORY==ALLDATA$CATEGORY.x
# # those with FALSE are the discrepancy. Turns out it is three where SPECIAL EIDTOR was coded as SE in category.x. 
# # SPecial is correct, so delete CATEGORY.x and check
# ALLDATA<-ALLDATA %>% select(-CATEGORY.x,-check)
# ##########

############
# # THIS LOOKS FOR DIFF BTWN INST2 and INST
# # TURNS OUT INST is correction of INST2, so delete INST2 and check
# ALLDATA$check<-ALLDATA$INST.2==ALLDATA$INST
# ALLDATA<-ALLDATA %>% select(-INST.2,-check)
############

############
# OLDINST is only a few. delete and recheck
# ALLDATA<-ALLDATA %>% select(-OLD_INST)
############

head(ALLDATA,10)
#
source("institution_cleaner.R")
ALLDATA<-institution_cleaner(ALLDATA)

# TODO: ADD AN EDITOR_ID to GCB and MARECOL
missing_edID<-filter(ALLDATA,is.na(ALLDATA$editor_id))
summary(missing_edID)



ALLDATA$JOURNAL<-as.factor(ALLDATA$JOURNAL)
ALLDATA<-droplevels(ALLDATA)
ALLDATA$Inst_Prior_Class
ALLDATA$INST<-as.factor(ALLDATA$INST)
levels(ALLDATA$INST)
levels(ALLDATA$JOURNAL)
ALLDATA$editor_id<-as.factor(ALLDATA$editor_id)
levels(ALLDATA$editor_id)
summary(ALLDATA)


# HOW MANY YEARS OF EACH JOURNAL?
str(ALLDATA)
JrnlYrs_10<-ALLDATA %>% filter(YEAR>=1985,YEAR<=1994) %>% group_by(JOURNAL) %>% summarise(yrs_per_jrnl=n_distinct(JOURNAL,YEAR)) %>% arrange(yrs_per_jrnl)
JrnlYrs_20<-ALLDATA %>% filter(YEAR>=1985,YEAR<=2004) %>% group_by(JOURNAL) %>% summarise(yrs_per_jrnl=n_distinct(JOURNAL,YEAR)) %>% arrange(yrs_per_jrnl)
JrnlYrs_30<-ALLDATA %>% filter(YEAR>=1985,YEAR<=2014) %>% group_by(JOURNAL) %>% summarise(yrs_per_jrnl=n_distinct(JOURNAL,YEAR)) %>% arrange(yrs_per_jrnl)
JrnlYrs_35<-ALLDATA %>% filter(YEAR>=1985,YEAR<=2019) %>% group_by(JOURNAL) %>% summarise(yrs_per_jrnl=n_distinct(JOURNAL,YEAR)) %>% arrange(yrs_per_jrnl)
JrnlYrs_30
# foo<-ALLDATA %>% filter(YEAR>"1984") %>% filter(YEAR<"2015") %>% filter(JOURNAL=="MARECOL") %>% group_by(YEAR) %>% summarize(n())
# foo


# rm(AG,AJB,AMNAT,AREES,BIOCON,BITR,CONBIO,DRYADDATA,ECOG,ECOL,EVOL,FEM,FUNECOL,GCB,GCB_inst,JANE,JAPE,JBIOG,JECOL,JTE,JZOOL,JZOOL_inst,LECO,MARECOL,MARECOL_inst,NAJFM,NAJFM_inst,NEWPHYT,NEWPHYT_inst,OECOL,OIKOS,PLANTECOL)
##############################################################
##############################################################
#
# CONTINUED CLEAN UP OF MASTER FILE  
#
##############################################################
##############################################################


# # Remove duplicate rws of each editor for a journal if they have same Inst 
# ALLDATA <- ALLDATA %>% group_by(editor_id,JOURNAL) %>% filter(row_number(INST) == 1)
# str(ALLDATA)
# ALLDATA$INST<-as.character(ALLDATA$INST)
# ALLDATA$INST[ALLDATA$INST == ""]  <- NA
# ALLDATA<-as.data.frame(ALLDATA)

###############
## CLEAN UP
###############
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



##############################################################
# COrrecting the country and institition where an  Editor is based
##############################################################

ALLDATA$COUNTRY[ALLDATA$LAST_NAME=="Bieber"]<-"Austria"
ALLDATA$COUNTRY[ALLDATA$FIRST_NAME=="Jeannine" & ALLDATA$LAST_NAME=="Cavender-Bares"]<-"USA"

ALLDATA$INST[ALLDATA$JOURNAL=="AMNAT" & ALLDATA$LAST_NAME=="Case"]<-"University of California San Diego"
ALLDATA$INST[ALLDATA$LAST_NAME=="Noon"]<-"Colorado State University"
ALLDATA$COUNTRY[ALLDATA$LAST_NAME=="VanDerHeijden"]<-"Switzerland"



ALLDATA$COUNTRY<-as.factor(ALLDATA$COUNTRY)
ALLDATA<-droplevels(ALLDATA)
levels(ALLDATA$COUNTRY)

##############################################################
# CLEAN-UP OF COUNTRIES  / COUNTRY COLUMN
##############################################################

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

##############################################################
# STILL TO DO 
##############################################################

# NEED TO CONFIRM WHAT PART OF USSR IN WHICH THE AUTHOR WAS BASED
ALLDATA$COUNTRY[ALLDATA$COUNTRY=="USSR"]<-"Russia"

# NEED DO DELETE THESE
which(ALLDATA$COUNTRY=="")

##############################################################
##############################################################
# CLEAN-UP OF INSTITUTIONS  
##############################################################
##############################################################
ALLDATA$INST[ALLDATA$INST=="."]<-NA

ALLDATA<-as.data.frame(ALLDATA)
ALLDATA$JOURNAL<-as.factor(ALLDATA$JOURNAL)

# Correcting the Institution where an Editor is based
ALLDATA$INST<-as.character(ALLDATA$INST)

ALLDATA$INST[ALLDATA$JOURNAL=="AMNAT" & ALLDATA$LAST_NAME=="Case"]<-"University of California San Diego"
ALLDATA$INST[ALLDATA$LAST_NAME=="Noon"]<-"Colorado State University"
ALLDATA$COUNTRY[ALLDATA$LAST_NAME=="VanDerHeijden"]<-"Switzerland"

levels(ALLDATA$INST) <- c(levels(ALLDATA$INST),"State University of New York College of Environmental Science and Forestry")
ALLDATA$INST[ALLDATA$LAST_NAME=="Burgess"]<-"State University of New York College of Environmental Science and Forestry"
ALLDATA$INST[ALLDATA$LAST_NAME=="Fragoso"]<-"State University of New York College of Environmental Science and Forestry"
ALLDATA$INST[ALLDATA$LAST_NAME=="Yanai"]<-"State University of New York College of Environmental Science and Forestry"
ALLDATA$INST[ALLDATA$LAST_NAME=="Hall" & ALLDATA$FIRST_NAME=="Charles" & ALLDATA$JOURNAL=="CONBIO"]<-"State University of New York College of Environmental Science and Forestry"

levels(ALLDATA$INST) <- c(levels(ALLDATA$INST),"Forestry and Forest Products Research Institute","University of Minnesota Duluth","University of Minnesota Duluth","University of Minnesota Crookston")
ALLDATA$INST[ALLDATA$LAST_NAME=="Fujimori"]<-"Forestry and Forest Products Research Institute"
ALLDATA$INST[ALLDATA$LAST_NAME=="Johnson" & ALLDATA$FIRST_NAME=="Lucinda"]<-"University of Minnesota Duluth"
ALLDATA$INST[ALLDATA$LAST_NAME=="Moen" & ALLDATA$FIRST_NAME=="Ron"]<-"University of Minnesota Duluth"
ALLDATA$INST[ALLDATA$LAST_NAME=="Sterner" & ALLDATA$FIRST_NAME=="Robert"]<-"University of Minnesota Duluth"
ALLDATA$INST[ALLDATA$LAST_NAME=="Wiersma" & ALLDATA$FIRST_NAME=="Jochum"]<-"University of Minnesota Crookston"
ALLDATA$INST[ALLDATA$LAST_NAME=="Smith" & ALLDATA$FIRST_NAME=="Madeleine"]<-"University of Minnesota Crookston"
ALLDATA$INST[ALLDATA$LAST_NAME=="Sims" & ALLDATA$FIRST_NAME=="Albert"]<-"University of Minnesota Crookston"

ALLDATA$INST[ALLDATA$LAST_NAME=="Sprules" & ALLDATA$FIRST_NAME=="Gary"]<-"University of Toronto Mississauga"
ALLDATA$INST[ALLDATA$LAST_NAME=="Wagner" & ALLDATA$FIRST_NAME=="Helene"]<-"University of Toronto Mississauga"
ALLDATA$INST[ALLDATA$LAST_NAME=="Kotanen" & ALLDATA$FIRST_NAME=="Peter"]<-"University of Toronto Mississauga"
ALLDATA$INST[ALLDATA$LAST_NAME=="Loiselle" & ALLDATA$FIRST_NAME=="Bette"]<-"University of Missouri St Louis"
ALLDATA$INST[ALLDATA$LAST_NAME=="Ricklefs" & ALLDATA$FIRST_NAME=="Robert"]<-"University of Missouri St Louis"
ALLDATA$INST[ALLDATA$LAST_NAME=="Renner" & ALLDATA$FIRST_NAME=="Susanne"]<-"University of Missouri St Louis"
ALLDATA$INST[ALLDATA$LAST_NAME=="Sork" & ALLDATA$FIRST_NAME=="Victoria"]<-"University of Missouri St Louis"

# levels(ALLDATA$INST) <- c(levels(ALLDATA$INST),"Universite Libre de Bruxelles","CNRS Centre dEcologie Fonctionnelle et Evolutive")
ALLDATA$INST[ALLDATA$LAST_NAME=="Parmentier"]<-"Universite Libre de Bruxelles"
ALLDATA$INST[ALLDATA$LAST_NAME=="Debussche"]<-"CNRS Centre dEcologie Fonctionnelle et Evolutive"


# ALLDATA$INST<-as.factor(ALLDATA$INST)
# ALLDATA$STATE<-as.factor(ALLDATA$STATE)
# ALLDATA$COUNTRY<-as.factor(ALLDATA$COUNTRY)
# ALLDATA$Inst_Prior_Class<-as.factor(ALLDATA$Inst_Prior_Class)
# ALLDATA<-ALLDATA %>% select(-DATA)

##############################################################
##############################################################
# Correcting or systematizing the name/speclling of an institution
##############################################################
##############################################################




# levels(ALLDATA$INST) <- c(levels(ALLDATA$INST),"University of Missouri Columbia")
ALLDATA$INST[ALLDATA$INST=="University of Missouri"]<-"University of Missouri Columbia"

# ALLDATA$INST<-as.character(ALLDATA$INST)
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
ALLDATA$INST[ALLDATA$INST=="State University of New York at Binghamton"]<-"SUNY at Binghamton"
ALLDATA$INST[ALLDATA$INST=="Binghamton University-SUNY"]<-"SUNY at Binghamton"
ALLDATA$INST[ALLDATA$INST=="Texas State"]<-"Texas State University"
ALLDATA$INST[ALLDATA$INST=="The University of Southwestern Louisiana"]<-"University of Southwestern Louisiana"
ALLDATA$INST[ALLDATA$INST=="U Arizona"]<-"University of Arizona"
ALLDATA$INST[ALLDATA$INST=="University of California Irvine"]<-"University of California-Irvine"
ALLDATA$INST[ALLDATA$INST=="University of California Riverside"]<-"University of California-Riverside"
ALLDATA$INST[ALLDATA$INST=="University of California at San Diego"]<-"University of California-San Diego"
ALLDATA$INST[ALLDATA$INST=="University of California Santa Cruz"]<-"University of California-Santa Cruz"
ALLDATA$INST[ALLDATA$INST=="UC Santa Cruz"]<-"University of California-Santa Cruz"
ALLDATA$INST[ALLDATA$INST=="Uc Merced, University of California"]<-"University of California-Merced"
ALLDATA$INST[ALLDATA$INST=="UMSL"]<-"University of Missouri-St. Louis"
ALLDATA$INST[ALLDATA$INST=="University of Carolina Aiken"]<-"University of South Carolina-Aiken"
ALLDATA$INST[ALLDATA$INST=="University of Massachusetts Boston"]<-"University of Massachusetts-Boston"
ALLDATA$INST[ALLDATA$INST=="University of Nevada Las Vegas"]<-"University of Nevada-Las Vegas"
ALLDATA$INST[ALLDATA$INST=="University of Texas Austin"]<-"University of Texas at Austin"
ALLDATA$INST[ALLDATA$INST=="Vanderbilt"]<-"Vanderbilt University"
ALLDATA$INST[ALLDATA$INST=="Virginia Commonweath U"]<-"Virginia Commonweath University"
ALLDATA$INST[ALLDATA$INST=="Washington State"]<-"Washington State University"
ALLDATA$INST[ALLDATA$INST=="Washington U"]<-"Washington University"
ALLDATA$INST[ALLDATA$INST=="Willamette U"]<-"Willamette University"
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
ALLDATA$UNIT[ALLDATA$INST=="USGS-Northern Prairie Wildlife Research Center"]<-"Northern Prairie Wildlife Research Center"
ALLDATA$INST[ALLDATA$INST=="USGS-Northern Prairie Wildlife Research Center"]<-"US Geological Survey"
ALLDATA$INST[ALLDATA$INST=="National Museum of Natural History"]<-"Smithsonian National Museum of Natural History"
ALLDATA$INST[ALLDATA$INST=="Smithsonian"]<-"Smithsonian National Museum of Natural History" #EB Verified
ALLDATA$INST[ALLDATA$INST=="USDA"]<-"US Department of Agriculture"
ALLDATA$UNIT[ALLDATA$INST=="USDA Cooperative State Research, Education and Extension Service (CSREES)"]<-"Cooperative State Research, Education, and Extension Service"
ALLDATA$INST[ALLDATA$INST=="USDA Cooperative State Research, Education and Extension Service (CSREES)"]<-"US Department of Agriculture"
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
ALLDATA$INST[ALLDATA$LAST_NAME=="Bowers" & ALLDATA$CITY=="Vadnais Heights"]<-"Calyx, Inc."
ALLDATA$INST[ALLDATA$INST=="Arnold Arboretum Of Harvard University"]<-"Harvard University Arnold Arboretum"
ALLDATA$INST[ALLDATA$INST=="Museum of Comparative Zoology"]<-"Harvard University Museum of Comparative Zoology"
ALLDATA$INST[ALLDATA$INST=="SUNY"]<-"State University of New York"
ALLDATA$INST[ALLDATA$INST=="State University of New York at Stony Brook"]<-"State University of New York-Stony Brook"
ALLDATA$INST[ALLDATA$INST=="State University of New York Syracuse"]<-"State University of New York-Syracuse"
ALLDATA$INST[ALLDATA$INST=="Arnold Arboretum of Harvard University"]<-"Harvard University Arnold Arboretum"
ALLDATA$INST[ALLDATA$INST=="Harvard Medical School"]<-"Harvard University Medical School"
ALLDATA$INST[ALLDATA$INST=="Australian Research Center for Urban "]<-"Smithsonian Tropical Research Institute"
ALLDATA$INST<-gsub(" INPA", "", ALLDATA$INST)
ALLDATA$INST<-gsub("--", " ", ALLDATA$INST)
ALLDATA$INST<-gsub("-", " ", ALLDATA$INST)
ALLDATA$INST<-gsub(" (CSIRO)", " ", ALLDATA$INST)
ALLDATA$INST<-gsub(" CSIRO", " ", ALLDATA$INST)
ALLDATA$INST<-gsub(",CAS", " ", ALLDATA$INST)
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
ALLDATA$INST<-gsub("Loisisana", "Louisiana", ALLDATA$INST)
ALLDATA$INST<-gsub("Lousiana", "Louisiana", ALLDATA$INST)
ALLDATA$INST<-gsub("Bringham", "Brigham", ALLDATA$INST)
ALLDATA$INST<-gsub("Connetticut", "Connecticut", ALLDATA$INST)
ALLDATA$INST<-gsub("Unversity", "University", ALLDATA$INST)
ALLDATA$INST<-gsub(",", "", ALLDATA$INST)
ALLDATA$INST<-gsub("Westleyan", "Wesleyan", ALLDATA$INST)
ALLDATA$INST<-gsub("'", "", ALLDATA$INST)
ALLDATA$INST<-gsub("Veterinary&", "Veterinary and", ALLDATA$INST)
ALLDATA$INST<-gsub("&", "and", ALLDATA$INST)
ALLDATA$INST<-gsub("Virginia Tech", "Virginia Polytechnic Institute and State University", ALLDATA$INST)
ALLDATA$INST<-gsub("Austrailan", "Australian", ALLDATA$INST)
ALLDATA$INST<-gsub("Indian Institute of Sciences", "Indian Institute of Science", ALLDATA$INST)
ALLDATA$INST<-gsub("KingS", "Kings", ALLDATA$INST)
ALLDATA$INST<-gsub("Louisisana", "Louisiana", ALLDATA$INST)
ALLDATA$INST<-gsub("Universrity", "University", ALLDATA$INST)
ALLDATA$INST<-gsub("Canadian Forestry", "Canadian Forest", ALLDATA$INST)
ALLDATA$INST<-gsub("Mighican", "Michigan", ALLDATA$INST)
ALLDATA$INST<-gsub("de Montpellier II", "Montpellier II", ALLDATA$INST)
ALLDATA$INST<-gsub("North Arizona", "Northern Arizona", ALLDATA$INST)
ALLDATA$INST<-gsub("Wsl", "WSL", ALLDATA$INST)
ALLDATA$INST<-gsub("Alabama in", "Alabama", ALLDATA$INST)
ALLDATA$INST<-gsub("Fonctionelle", "Fonctionnelle", ALLDATA$INST)
ALLDATA$INST<-gsub("-CNRS", "", ALLDATA$INST)
ALLDATA$INST<-gsub("University of Sherbooke", "Universite de Sherbooke", ALLDATA$INST)
ALLDATA$INST[ALLDATA$INST=="WSL Swiss Federal Research Institute"]<-"Swiss Federal Research Institute WSL"
# ALLDATA$INST<-gsub("(CSIC)", "", ALLDATA$INST)
ALLDATA$INST<-gsub("Smithsonian Institute", "Smithsonian Institution", ALLDATA$INST)
ALLDATA$INST<-gsub("Minnestoa", "Minnesota", ALLDATA$INST)
ALLDATA$INST<-gsub("University of Lausanne", "Universite de Lausanne", ALLDATA$INST)
ALLDATA$INST<-gsub("Institute of Ecosystem Studies", "Cary Institute of Ecosystem Studies", ALLDATA$INST)
ALLDATA$INST<-gsub("Environment Bangalore", "Environment", ALLDATA$INST)
ALLDATA$INST[ALLDATA$INST=="Cary Cary Institute of Ecosystem Studies"]<-"Cary Institute of Ecosystem Studies"
ALLDATA$INST<-gsub(" CATIE","", ALLDATA$INST)
ALLDATA$INST<-gsub("CATIE ","", ALLDATA$INST)
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
ALLDATA$INST[ALLDATA$INST=="Technion Technion Israel Institute of Technology"]<-"Technion Israel Institute of Technology"
ALLDATA$INST[ALLDATA$INST=="Universidad Catolica de Chile"]<-"Pontifica Universidad Catolica de Chile"
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
ALLDATA$INST[ALLDATA$INST=="Museum dHistoire Naturelle"]<-"Museum National dHistoire Naturelle"
ALLDATA$INST[ALLDATA$INST=="National Autonomous university of Mexico"]<-"Universidad Nacional Autonoma de Mexico"
ALLDATA$INST[ALLDATA$INST=="Landcare Research"]<-"Manaaki Whenua Landcare Research"
ALLDATA$UNIT[ALLDATA$INST=="USGS/NRII"]<-"NRII" 
ALLDATA$INST[ALLDATA$INST=="USGS/NRII"]<-"US Geological Survey"
ALLDATA$UNIT[ALLDATA$INST=="Florida International University and Center for Tropical Plant Conservation"]<-"Center for Tropical Plant Conservation" 
ALLDATA$INST[ALLDATA$INST=="Florida International University and Center for Tropical Plant Conservation"]<-"Florida International University" 
ALLDATA$INST[ALLDATA$INST=="Ecole Normale Supérieure"]<-"Ecole Normale Superieure" 
ALLDATA$INST[ALLDATA$INST=="Université de Sherbrooke"]<-"Universite de Sherbrooke"
ALLDATA$INST[ALLDATA$INST=="Pontificia Universidad Católica de Chile"]<-"Pontificia Universidad Catolica de Chile" 
ALLDATA$INST[ALLDATA$INST=="University of Tromsø"]<-"University of Tromso" 
ALLDATA$INST[ALLDATA$INST=="Universit\xfc\xbe\x8c\xa3\xa0\xbc Montpellier II"]<-"Universite Montpellier II" 
ALLDATA$INST[ALLDATA$INST=="Universit\xfc\xbe\x8d\x83\xa0\xbct Z\xfc\xbe\x8c\x93\xa0\xbcrich Irchel"]<-"University of Zurich Irchel" 
ALLDATA$INST[ALLDATA$INST=="Texas A & M Univ."]<-"Texas A & M University"
ALLDATA$INST[ALLDATA$INST=="Texas A & M"]<-"Texas A & M University"


##############################################################
##############################################################
# Dividing Some Names for INST into INST and UNIT
##############################################################
##############################################################

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



##############################################################
# SAVE THE FILE AS A CSV FOR MANUAL REVIEW
##############################################################

UNI_LIST<-ALLDATA %>% select(INST,COUNTRY) %>% arrange(COUNTRY,INST)
UNI_LIST<-unique(UNI_LIST)

head(UNI_LIST,100)
write.csv(UNI_LIST, file="./output_review/uniNameList.csv", row.names = T) #export it as a csv file



##############################################################
# AFTER MANUAL REVIEW, UPLOAD THE CORRECTIONS AND INCORPORATE THEM
##############################################################

corrections1<-read.csv("./output_review/uniNameList_corrections_only.csv",encoding = "ASCII",stringsAsFactors = FALSE)

str(corrections1)
#THIS CHANGES THE UNITS AND INST BASED ON THE CORRECTION IN THE DATAFRAME
ALLDATA<-left_join(ALLDATA, corrections1,by="INST",copy=TRUE) %>% mutate(INST = ifelse(is.na(INST_CORR), INST, INST_CORR)) %>% mutate(UNIT = ifelse(is.na(UNIT_CORR), UNIT, UNIT_CORR))
ALLDATA<-ALLDATA %>% select(-INST_CORR,-UNIT_CORR)


##############################################################
##############################################################
# FOR SOME REASON SOME DIDN"T CHANGE< SO NEED TO DO MANUALLY
##############################################################
##############################################################

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
ALLDATA$INST[ALLDATA$LAST_NAME=="Debussche"]<-"CNRS Centre dEcologie Fonctionnelle et Evolutive"
ALLDATA$COUNTRY[ALLDATA$LAST_NAME=="Gandon"]<-"France"
ALLDATA$FIRST_NAME[ALLDATA$LAST_NAME=="MONOD"]<-"Theodore"
ALLDATA$LAST_NAME[ALLDATA$LAST_NAME=="MONOD"]<-"Monod"
ALLDATA$INST[ALLDATA$LAST_NAME=="Whitmore"]<-"University of Oxford"
ALLDATA$INST[ALLDATA$LAST_NAME=="Hails"]<-"University of Oxford"
ALLDATA$UNIT[ALLDATA$LAST_NAME=="Whitmore"]<-"Oxford Forestry Institute"
ALLDATA$INST[ALLDATA$LAST_NAME=="Leamy"]<-"University of North Carolina Charlotte"
ALLDATA$INST[ALLDATA$LAST_NAME=="Gustafson"& ALLDATA$FIRST_NAME=="E"]<-"US Forest Service"
ALLDATA$UNIT[ALLDATA$LAST_NAME=="Gustafson"& ALLDATA$FIRST_NAME=="E"]<-"North Central Research Station"
ALLDATA$COUNTRY[ALLDATA$LAST_NAME=="Westing"&ALLDATA$FIRST_NAME=="Arthur"]<-"Sweden"
ALLDATA$COUNTRY[ALLDATA$LAST_NAME=="Galdon"&ALLDATA$FIRST_NAME=="Luis"]<-"Spain"
ALLDATA$INST[ALLDATA$LAST_NAME=="Labandeira"& ALLDATA$FIRST_NAME=="Conrad"]<-"Smithsonian National Museum of Natural History"
ALLDATA$INST[ALLDATA$LAST_NAME=="SEIDENSTZICKER"& ALLDATA$FIRST_NAME=="JOHN"]<-"Smithsonian National Zoological Park"
ALLDATA$INST[ALLDATA$LAST_NAME=="Kleiman"& ALLDATA$FIRST_NAME=="Devra"]<-"Smithsonian National Zoological Park"
ALLDATA$INST[ALLDATA$INST=="Syngenta Crop Protection"]<-"Syngenta Crop Protection Inc"

##############################################################
# CHANGE THOSE WITH BLANK INST to NA
##############################################################

# ALLDATA$INST[ALLDATA$LAST_NAME=="Angeler" & ALLDATA$YEAR>"2011" & ALLDATA$JOURNAL=="JAPE"]<-NA
ALLDATA$INST[ALLDATA$INST==""]<-NA
ALLDATA$INST[ALLDATA$INST=="N/A"]<-NA
ALLDATA$INST[ALLDATA$INST=="."]<-NA




##############################################################
# SAVE THE FILE AS A CSV FOR MANUAL REVIEW AGAIN
##############################################################
UNI_LIST2<-ALLDATA %>% select(INST,COUNTRY) %>% arrange(COUNTRY,INST)
UNI_LIST2<-UNI_LIST2[!is.na(UNI_LIST2$INST),]
head(UNI_LIST2,20)
levels(UNI_LIST2$COUNTRY)<-c(levels(UNI_LIST2$COUNTRY),"missing")
UNI_LIST2$COUNTRY[is.na(UNI_LIST2$COUNTRY)]<-"missing"
UNI_LIST2<-UNI_LIST2 %>% group_by(INST,COUNTRY) %>% summarize(count=n())

write.csv(UNI_LIST2, file="./output_review/uniNameList2.csv", row.names = T) #export it as a csv file



##############################################################
# NOW UPLOAD CORRECTIONS
##############################################################

corrections2<-read.csv("./output_review/uniNameList_corrections2.csv",encoding = "ASCII",stringsAsFactors = FALSE)
str(corrections2)
#THIS CHANGES THE UNITS AND INST BASED ON THE CORRECTION IN THE DATAFRAME
ALLDATA2<-left_join(ALLDATA, corrections2,by=c("INST","COUNTRY"),copy=TRUE) %>% mutate(INST = ifelse(is.na(INST_CORR), INST, INST_CORR)) %>% mutate(UNIT = ifelse(is.na(UNIT_CORR), UNIT, UNIT_CORR)) %>% mutate(COUNTRY = ifelse(is.na(COUNTRY_CORR), COUNTRY, COUNTRY_CORR))
ALLDATA<-ALLDATA2 %>% select(-INST_CORR,-UNIT_CORR,-COUNTRY_CORR)
rm(ALLDATA2)


##############################################################
##############################################################
# DATA CHECK: FILLING IN MISSING INST, CHECKING EDITORS WITH DUPLICATE INST
##############################################################
##############################################################
# write.csv(ALLDATA,"./output_review/ALLDATA_add_PJ_data.csv",row.names=FALSE)
# # THIS IS IN Author_and_Inst_to_check.R 
# ################



##############################
##############################
# ADD AND PATRICK JAMES CORRECTIONS
##############################
##############################








































##############################################################
# This will check if any are still in UTF8 with accents instead of ascii
##############################################################

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
# ALLDATA$INST<-as.character(ALLDATA$INST)
# ALLDATA$INST<-str_replace_all(ALLDATA$INST,"\\xa","")
# head(ALLDATA$INST,10)

ALLDATA$INST<-as_factor(ALLDATA$INST)
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
stri_unescape_unicode(ALLDATA$INST)
iconv(ALLDATA$INST, to = "ASCII//TRANSLIT")
# summary(ALLDATA$INST)
ALLDATA<-ALLDATA %>% filter(ALLDATA$INST!="")




####################################

# GO WITH ALLDATA TO INST_corrections.R
# this is where any corrections are put in'
# when done, take the resulting df and fo to EditorINst_analyses.R

####################################






























##############################################################
# THIS WILL ALLOW YOU DO TO COMPARE ALL NAMES
# TO ALL NAMES TO SEE IF THERE ARE ANY THAT ARE 
# SIMILAR AND SHOULD BE POOLED
##############################################################

# 
# UNI_LIST<-ALLDATA$INST
# summary(UNI_LIST)
# levels(UNI_LIST)
# UNI_LIST<-as.data.frame(UNI_LIST)
# summary(UNI_LIST)
# str(UNI_LIST)
# UNI_LIST<-distinct(UNI_LIST)
# head(UNI_LIST)



























# 
# # TODO: FIND THE DUPLICATED ONES AND CORRECT THEM
# 
# DISTINCT<-ALLDATA %>% distinct(JOURNAL, YEAR, editor_id, .keep_all = TRUE)
# 
# alldata_dupes<-ALLDATA %>% group_by(JOURNAL,LAST_NAME,FIRST_NAME,YEAR) %>% filter(n()>1)
# alldata_dupes<-droplevels(alldata_dupes)
# levels(alldata_dupes$JOURNAL)






# ONE LAST CHECK OF THE NAMES
x <- ALLDATA$LAST_NAME
y<-as.data.frame(x)
# str(y)
y<-distinct(y)
# head(y)
# 
y[] <- lapply(y, as.character)
NamesList<-sapply(y$x,agrep,y$x, value=TRUE)
NamesDF<-data.frame(
  Name1 = rep(names(NamesList), lapply(NamesList, length)),
  Name2 = unlist(NamesList))
# Create a column to which you will add a logical condition telling you if the names are an EXACT match
NamesDF$match<-NA
NamesDF$match<-NamesDF$Name1==NamesDF$Name2
match2<-ifelse(NamesDF$match=="TRUE",1,0) #convert TRUE/FALSEto 0/1
NamesDF<-cbind(NamesDF,match2)
NamesDF<-arrange(NamesDF,Name1,Name2) #organize in alphabetica order
NamesDF<-filter(NamesDF, match==FALSE)  # THIS DELETES ALL NAMES THAT ARE 100% MATCH
# # Convert to chr
NamesDF$Name1<-as.character(NamesDF$Name1)
NamesDF$Name2<-as.character(NamesDF$Name2)
# # Calculate the proportional similarity and # changes required to go from one name to another. Package RecordLinkage
NamesDF$Name_sim<-levenshteinSim(NamesDF$Name1, NamesDF$Name2)
NamesDF$Name_dist<-levenshteinDist(NamesDF$Name1, NamesDF$Name2)
# # Because this does all pairwise comparisons, it results in redundancy: "e bruna vs emilio bruna" and "emilio bruna vs e bruna"
# # are in different rows, even though they are the same "comparison". This deletes one of the two
NamesDF<-NamesDF[!duplicated(t(apply(NamesDF, 1, sort))),]
# # this arranges them in order from most similar (1 change required) to least similar.
# # look carefully at those with a few changes, as they are likely to be a tiny spelling mistake or difference in intials
NamesDF$index<-seq.int(nrow(NamesDF)) #adds a column with an index to make it easier to id which row you need'
NamesDF <- NamesDF %>% select(index, Name1, Name2, Name_sim,Name_dist) #It's kinda ugly, but this rearranges columns (and dumps the "FALSE")
NamesDF <- arrange(NamesDF, desc(Name_sim))
head(NamesDF)
# invoking the function
source("./functions/namecompare.R")
x <- ALLDATA$LAST_NAME
y <- namecompare(x)

write.csv(UNI_LIST, file="uniNameList_post_review.csv", row.names = T) #export it as a csv file

UNI_LIST<-as.factor(UNI_LIST)
UNI_LIST[] <- lapply(UNI_LIST, as.character)
str(UNI_LIST)
NamesList<-sapply(UNI_LIST$UNI_LIST,agrep,UNI_LIST$UNI_LIST, value=TRUE)

 NamesDF<-data.frame(
   Name1 = rep(names(NamesList), lapply(NamesList, length)),
   Name2 = unlist(NamesList))

summary(NamesDF)
str(NamesDF)

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
# # Calculate the proportional similarity and # changes required to go from one name to another. Package RecordLinkage
NamesDF$Name_sim<-levenshteinSim(NamesDF$Name1, NamesDF$Name2)
NamesDF$Name_dist<-levenshteinDist(NamesDF$Name1, NamesDF$Name2)
#
# # Because this does all pairwise comparisons, it results in redundancy: "e bruna vs emilio bruna" and "emilio bruna vs e bruna"
# # are in different rows, even though they are the same "comparison". This deletes one of the two
NamesDF<-NamesDF[!duplicated(t(apply(NamesDF, 1, sort))),]
# # this arranges them in order from most similar (1 change required) to least similar.
# # look carefully at those with a few changes, as they are likely to be a tiny spelling mistake or difference in intials
NamesDF$index<-seq.int(nrow(NamesDF)) #adds a column with an index to make it easier to id which row you need'
NamesDF <- NamesDF %>% select(index, Name1, Name2, Name_sim,Name_dist) #It's kinda ugly, but this rearranges columns (and dumps the "FALSE")
NamesDF <- arrange(NamesDF, desc(Name_sim))
head(NamesDF)
write.csv(NamesDF, file="uniNameCheck_each_vs_each.csv", row.names = T) #export it as a csv file
#
#

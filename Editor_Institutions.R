
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
# library(vegan)
# library(nlme)
# library(MuMIn)
# library(directlabels)
# library(grid)
# library(gridExtra)
# library(RColorBrewer)

##############################################################
##############################################################
# UPLOAD AND CLEAN DATA FROM DRYAD
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
# UPLOAD RAW EDITOR DATA FROM INDIVIDUAL JOURNALS
# COLLECTED BY SCIWRI17
##############################################################
##############################################################
# path to folder that holds multiple .csv files
folder <- "./Data/sciwri17_raw_data/"      
# create list of all .csv files in folder
file_list <- list.files(path=folder, pattern="*.csv") 
# read in each .csv file in file_list and create a data frame 
# with the same name as the .csv file
# Note the ASCII encoding to try and deal with the UTF-8 characters
for (i in 1:length(file_list)){
  assign(file_list[i], 
         read.csv(paste(folder, file_list[i], sep=''),
                  na.strings = c("","NA"), encoding = "ASCII")
  )}
rm(folder,file_list,i)
################################################
# UPLOAD AND STANDARDIZE AUK AND CONDOR COLLECTED BY Hurtado (MALAS RA) 
################################################
folder <- "./Data/hurtado_data/"      # path to folder that holds multiple .csv files
file_list <- list.files(path=folder, pattern="*.csv") # create list of all .csv files in folder
# read in each .csv file in file_list and create a data frame with the same name as the .csv file
# Note the ASCII encoding to try and deal with the UTF-8 characters
for (i in 1:length(file_list)){
  assign(file_list[i], 
         read.csv(paste(folder, file_list[i], sep=''),
                  na.strings = c("","NA"), encoding = "ASCII")
  )}
rm(folder,file_list,i)
##############################################################
##############################################################
# CLEAN-UP BY JOURNAL
##############################################################
############################################################### 

##############################################################
# AUK
##############################################################
AUK_raw<-AUK.csv
source("functions_data_import/clean_AUK.R")
AUK<-clean_AUK(AUK_raw)
rm(AUK_raw,AUK.csv)
##############################################################
# CONDOR
##############################################################
CONDOR_raw<-CONDOR.csv
source("functions_data_import/clean_CONDOR.R")
CONDOR<-clean_CONDOR(CONDOR_raw)
rm(CONDOR_raw,CONDOR.csv)
##############################################################
# FUNCTIONAL ECOLOGY
##############################################################
# FUNECOL HAS MULTIPLE DATASETS TO UPLOAD
FUNECOL1_raw<-FUNECOLdata_Allen_EB_1dec.csv
FUNECOL2_raw<-FUNECOL_data_11.03.2017.csv
# TODO: NEED TO CORRECT THE ONES BELOW WHERE CATEGORY X AND Y DON'T MATCH.
# TODO: OPEN THE FUNCTION TO SEE WHICH ONES THEY ARE
source("functions_data_import/clean_FUNECOL.R")
FUNECOL<-clean_FUNECOL(FUNECOL1_raw,FUNECOL2_raw)
rm(FUNECOLdata_Allen_EB_1dec.csv,FUNECOL_data_11.03.2017.csv,
   FUNECOL1_raw,FUNECOL2_raw)
##############################################################
# LANDSCAPE ECOLOGY
##############################################################
LECO_raw<-LECO_2017.csv 
source("functions_data_import/clean_LECO.R")
LECO<-clean_LECO(LECO_raw)
rm(LECO_2017.csv,LECO_raw)
##############################################################
# CONBIO
##############################################################
CONBIO_raw<-CONBIO_EKB.csv
source("functions_data_import/clean_CONBIO.R")
CONBIO<-clean_CONBIO(CONBIO_raw)
rm(CONBIO_EKB.csv,CONBIO_raw)
##############################################################
# JZOOLOGY
##############################################################
# JZOOL HAS MULTIPLE DATASETS TO UPLOAD
JZOOL1<-JZOOL17nov.csv
JZOOL2<-JZOOL.csv
# TODO: a bunch of institutions still missing
source("functions_data_import/clean_JZOOL.R")
JZOOL<-clean_JZOOL(JZOOL1,JZOOL2)
rm(JZOOL.csv,JZOOL17nov.csv,JZOOL1,JZOOL2)
##############################################################
# NEW PHYT
##############################################################
NEWPHYT_raw<-NEWPHYT_21july2018.csv
source("functions_data_import/clean_NEWPHYT.R")
NEWPHYT<-clean_NEWPHYT(NEWPHYT_raw)
rm(NEWPHYT_21july2018.csv,NEWPHYT_raw)
##############################################################
# BIOLOGICAL CONSERVATION
##############################################################
BIOCON_raw<-BIOCON_TS.csv
# TODO: STILL NEEED TO need to add the 2x names, fill in any Missing 
# for INST, UNIT, etc, and the FILL the columns
source("functions_data_import/clean_BIOCON.R")
BIOCON<-clean_BIOCON(BIOCON_raw)
rm(BIOCON_TS.csv,BIOCON_raw)
##############################################################
# AGRONOMY
##############################################################
AG_raw<-AGRONOMY_data_11.02.2017.csv 
source("functions_data_import/clean_AGRON.R")
AG<-clean_AGRON(AG_raw)
rm(AGRONOMY_data_11.02.2017.csv,AG_raw)
##############################################################
# FEM
##############################################################
FEM_raw<-FEM_7112017.csv
source("functions_data_import/clean_FEM.R")
FEM<-clean_FEM(FEM_raw)
rm(FEM_7112017.csv)
##############################################################
# J ANIMAL ECOLOGY
##############################################################
JANE_raw<-JANE.csv
source("functions_data_import/clean_JANE.R")
JANE<-clean_JANE(JANE_raw)
rm(JANE.csv,JANE_raw)
##############################################################
# J BIOGEOGRAPHY
##############################################################
JBIOG_raw<-JBIOG.csv
source("functions_data_import/clean_JBIOG.R")
JBIOG<-clean_JBIOG(JBIOG_raw)
rm(JBIOG_raw,JBIOG.csv)
##############################################################
# PLANT ECOLOGY
##############################################################
PLANTECOL_raw<-PLANTECOL_new.csv 
source("functions_data_import/clean_PLANTECOL.R")
PLANTECOL<-clean_PLANTECOL(PLANTECOL_raw)
rm(PLANTECOL_raw,PLANTECOL_new.csv)
##############################################################
# EVOLUTION
##############################################################
EVOL_raw<-EVOL.csv
source("functions_data_import/clean_EVOL.R")
EVOL<-clean_EVOL(EVOL_raw)
rm(EVOL.csv,EVOL_raw)
##############################################################
# AMERICAN NATURALIST
##############################################################
AMNAT1_raw<-AMNAT.csv
AMNAT2_raw<-AmNat0614.csv
# TODO: FIGURE OUT WTF AM NAT
# TODO: AM NAT Needs institutions from 2006-2014 put in
# NEED TO FIGURE OUT IF AMNAT, AMNAT0614,orAMNATEKB!!!!!!!!!!!!!!
source("functions_data_import/clean_AMNAT.R")
AMNAT<-clean_AMNAT(AMNAT1_raw,AMNAT2_raw)
rm(AMNAT.csv,AmNat0614.csv,AMNAT1_raw,AMNAT2_raw)
rm(AMNAT_EKB.csv)
##############################################################
# JAPE
##############################################################
JAPE_raw<-JAPE_new.csv
source("functions_data_import/clean_JAPE.R")
JAPE<-clean_JAPE(JAPE_raw)
rm(JAPE_new.csv)
##############################################################
# OECOLOGIA
##############################################################
OECOL_raw<-OECOL.csv
source("functions_data_import/clean_OECOL.R")
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
source("functions_data_import/clean_GCB.R")
GCB<-clean_GCB(GCB_raw)
rm(GCBdata.csv,GCB_raw)
##############################################################
# NAJFM
##############################################################
NAJFM_raw<-NAJFM_21july2018.csv 
source("functions_data_import/clean_NAJFM.R")
NAJFM<-clean_NAJFM(NAJFM_raw)
rm(NAJFM_21july2018.csv,NAJFM_raw)
##############################################################
# MARECOL
##############################################################
# TODO: MARECOL
# 2) Disambiguate and add editor ID
# 3) NEEDS INSTITIONS ADDED OR FILLED
MARECOL_raw<-MARECOL_21July2018.csv 
source("functions_data_import/clean_MARECOL.R")
MARECOL<-clean_MARECOL(MARECOL_raw)
rm(MARECOL_21July2018.csv,MARECOL_raw)
##############################################################
# AJB
##############################################################
AJB_raw<-AJB.csv
source("functions_data_import/clean_AJB.R")
AJB<-clean_AJB(AJB_raw)
rm(AJB.csv,AJB_raw)
##############################################################
# OIKOS
##############################################################
OIKOS_raw<-OIKOS_21july2018.csv
source("functions_data_import/clean_OIKOS.R")
OIKOS<-clean_OIKOS(OIKOS_raw)
rm(OIKOS_21july2018.csv,OIKOS_raw)
##############################################################
# ECOGRAPHY
##############################################################
ECOG_raw<-ECOGRAPHY_5112017.csv
source("functions_data_import/clean_ECOG.R")
ECOG<-clean_ECOG(ECOG_raw)
rm(ECOGRAPHY_5112017.csv,ECOG_raw)
##############################################################
# BITR
##############################################################
BITR_raw<-BITR.csv 
source("functions_data_import/clean_BITR.R")
BITR<-clean_BITR(BITR_raw)
rm(BITR.csv, BITR_raw)
##############################################################
# JECOL
##############################################################
JECOL_raw<-JECOL_new.csv
source("functions_data_import/clean_JECOL.R")
JECOL<-clean_JECOL(JECOL_raw)
rm(JECOL_new.csv,JECOL_raw)
##############################################################
# AREES
##############################################################
AREES_raw<-AREES.csv
source("functions_data_import/clean_AREES.R")
AREES<-clean_AREES(AREES_raw)
rm(AREES.csv,AREES_raw)
##############################################################
# JTE
##############################################################
JTE_raw<-JTE_2015.csv
source("functions_data_import/clean_JTE.R")
JTE<-clean_JTE(JTE_raw)
rm(JTE_2015.csv,JTE_raw)
######################
# MEEPS
######################
# TODO: EVERYTHING. NOT EVEN AN EXCEL SHEET,NEED TO SEE WHICH 
# ONES ARE MISSING INSTITUTIONS
##############################################################
# ECOLOGY
##############################################################
ECOL_raw<-ECOLOGY.csv
source("functions_data_import/clean_ECOL.R")
ECOL<-clean_ECOL(ECOL_raw)
rm(ECOLOGY.csv, ECOL_raw)
##############################################################
##############################################################
##############################################################
# BIND THEM ALL UP INTO ONE FILE  
##############################################################
##############################################################
# with ALL


ALLDATA<-bind_rows(CONBIO,
                   MARECOL,
                   NAJFM,
                   NEWPHYT,
                   JZOOL,
                   GCB,
                   OIKOS,
                   BITR,
                   LECO,
                   PLANTECOL,
                   OECOL,
                   JTE,
                   JECOL,
                   JBIOG,
                   JAPE,
                   JANE,
                   FUNECOL,
                   FEM,
                   EVOL,
                   ECOL,
                   ECOG,
                   AREES,
                   AMNAT,
                   AJB,
                   AG,
                   CONDOR,
                   AUK,
                   BIOCON)
levels(as.factor(ALLDATA$JOURNAL))
head(ALLDATA,10)
str(ALLDATA)
colnames(ALLDATA)
summary(ALLDATA)


ALLDATA <- ALLDATA %>% 
  select(JOURNAL,YEAR,VOLUME,ISSUE,editor_id,    # deleting "NAME"
         FIRST_NAME,MIDDLE_NAME,
         LAST_NAME,TITLE,CATEGORY,CATEGORY.x,INST,OLD_INST,INST.2,
         UNIT,CITY,STATE,COUNTRY,COUNTRY_Prior_Class,geo.code,
         geo.code_Prior_Class,NOTES,GENDER,TITLE)
# ############
# # check to see what is diff between category and category.x
ALLDATA$check<-ALLDATA$CATEGORY==ALLDATA$CATEGORY.x
summary(ALLDATA$check)
# those with FALSE are the discrepancy. Turns out it is three where
# SPECIAL EIDTOR was coded as SE in category.x.
# Special is correct, so delete CATEGORY.x and check
ALLDATA<-ALLDATA %>% select(-CATEGORY.x,-check)
# ##########

############
# THIS LOOKS FOR DIFF BTWN INST2 and INST
# TURNS OUT INST is correction of INST2, so delete INST2 and check
ALLDATA$check<-ALLDATA$INST.2==ALLDATA$INST
summary(ALLDATA$check)
# LOOKED OVER, they all check out
ALLDATA<-ALLDATA %>% select(-INST.2,-check)
############

############
# OLDINST is only a few. delete and recheck
ALLDATA<-ALLDATA %>% select(-OLD_INST)
############

##############################################################
# COrrecting the country and institition where Editors are based
##############################################################

ALLDATA$INST<-as.factor(ALLDATA$INST)
levels(ALLDATA$INST) <- c(levels(ALLDATA$INST),"University of Missouri Columbia",
                          "CNRS Centre dEcologie Fonctionnelle et Evolutive",
                          "Forestry and Forest Products Research Institute",
                          "University of Minnesota Duluth",
                          "University of Minnesota Crookston",
                          "State University of New York College of Environmental Science and Forestry",
                          "Calyx, Inc.","University of North Carolina Charlotte",
                          "Smithsonian National Museum of Natural History",
                          "Smithsonian National Zoological Park",
                          "Laboratoire Associe de Modelisation des Plantes",
                          "Southern Illinois University",
                          "southern illinois u",
                          "Aarhus University",
                          "University of St. Andrews",
                          "university of st andrews")



levels(ALLDATA$INST) <- c(levels(ALLDATA$INST),"University of Toronto Mississauga","Universite Libre de Bruxelles")
ALLDATA$INST[ALLDATA$LAST_NAME=="Sprules" & ALLDATA$FIRST_NAME=="Gary"]<-"University of Toronto Mississauga"
ALLDATA$INST[ALLDATA$LAST_NAME=="Wagner" & ALLDATA$FIRST_NAME=="Helene"]<-"University of Toronto Mississauga"
ALLDATA$INST[ALLDATA$LAST_NAME=="Kotanen" & ALLDATA$FIRST_NAME=="Peter"]<-"University of Toronto Mississauga"
ALLDATA$INST[ALLDATA$LAST_NAME=="Loiselle" & ALLDATA$FIRST_NAME=="Bette"]<-"University of Missouri St Louis"
ALLDATA$INST[ALLDATA$LAST_NAME=="Ricklefs" & ALLDATA$FIRST_NAME=="Robert"]<-"University of Missouri St Louis"
ALLDATA$INST[ALLDATA$LAST_NAME=="Renner" & ALLDATA$FIRST_NAME=="Susanne"]<-"University of Missouri St Louis"
ALLDATA$INST[ALLDATA$LAST_NAME=="Sork" & ALLDATA$FIRST_NAME=="Victoria"]<-"University of Missouri St Louis"
ALLDATA$INST[ALLDATA$LAST_NAME=="Parmentier"]<-"Universite Libre de Bruxelles"
ALLDATA$INST[ALLDATA$LAST_NAME=="Debussche"]<-"CNRS Centre dEcologie Fonctionnelle et Evolutive"
# levels(ALLDATA$INST) <- c(levels(ALLDATA$INST),"Forestry and Forest Products Research Institute","University of Minnesota Duluth","University of Minnesota Crookston")
ALLDATA$INST[ALLDATA$LAST_NAME=="Fujimori"]<-"Forestry and Forest Products Research Institute"
ALLDATA$INST[ALLDATA$LAST_NAME=="Johnson" & ALLDATA$FIRST_NAME=="Lucinda"]<-"University of Minnesota Duluth"
ALLDATA$INST[ALLDATA$LAST_NAME=="Moen" & ALLDATA$FIRST_NAME=="Ron"]<-"University of Minnesota Duluth"
ALLDATA$INST[ALLDATA$LAST_NAME=="Sterner" & ALLDATA$FIRST_NAME=="Robert"]<-"University of Minnesota Duluth"
ALLDATA$INST[ALLDATA$LAST_NAME=="Wiersma" & ALLDATA$FIRST_NAME=="Jochum"]<-"University of Minnesota Crookston"
ALLDATA$INST[ALLDATA$LAST_NAME=="Smith" & ALLDATA$FIRST_NAME=="Madeleine"]<-"University of Minnesota Crookston"
ALLDATA$INST[ALLDATA$LAST_NAME=="Sims" & ALLDATA$FIRST_NAME=="Albert"]<-"University of Minnesota Crookston"
ALLDATA$INST[ALLDATA$JOURNAL=="AMNAT" & ALLDATA$LAST_NAME=="Case"]<-"University of California San Diego"
ALLDATA$INST[ALLDATA$LAST_NAME=="Noon"]<-"Colorado State University"
ALLDATA$COUNTRY[ALLDATA$LAST_NAME=="VanDerHeijden"]<-"Switzerland"
# levels(ALLDATA$INST) <- c(levels(ALLDATA$INST),"State University of New York College of Environmental Science and Forestry")
ALLDATA$INST[ALLDATA$LAST_NAME=="Burgess"]<-"State University of New York College of Environmental Science and Forestry"
ALLDATA$INST[ALLDATA$LAST_NAME=="Fragoso"]<-"State University of New York College of Environmental Science and Forestry"
ALLDATA$INST[ALLDATA$LAST_NAME=="Yanai"]<-"State University of New York College of Environmental Science and Forestry"
ALLDATA$INST[ALLDATA$LAST_NAME=="Hall" & ALLDATA$FIRST_NAME=="Charles" & ALLDATA$JOURNAL=="CONBIO"]<-
  "State University of New York College of Environmental Science and Forestry"
ALLDATA$INST[ALLDATA$INST=="University of Missouri"]<-"University of Missouri Columbia" 
ALLDATA$COUNTRY[ALLDATA$LAST_NAME=="Bieber"]<-"Austria"
ALLDATA$COUNTRY[ALLDATA$FIRST_NAME=="Jeannine" & ALLDATA$LAST_NAME=="Cavender-Bares"]<-"USA"
ALLDATA$INST[ALLDATA$JOURNAL=="AMNAT" & ALLDATA$LAST_NAME=="Case"]<-"University of California San Diego"
ALLDATA$INST[ALLDATA$LAST_NAME=="Noon"]<-"Colorado State University"
ALLDATA$COUNTRY[ALLDATA$LAST_NAME=="VanDerHeijden"]<-"Switzerland"
ALLDATA$INST[ALLDATA$LAST_NAME=="Bowers" & ALLDATA$CITY=="Vadnais Heights"]<-"Calyx, Inc."
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
ALLDATA$INST[ALLDATA$LAST_NAME=="Houllier" & (ALLDATA$YEAR>1998 & ALLDATA$YEAR<2003)]<-NA
ALLDATA$INST[ALLDATA$LAST_NAME=="Houllier" & (ALLDATA$YEAR>1998 & ALLDATA$YEAR<2003)]<-"Laboratoire Associe de Modelisation des Plantes"
ALLDATA$INST[ALLDATA$LAST_NAME=="Watling"& ALLDATA$INST=="Adelaide"]<-"University of Adelaide"
ALLDATA$INST[ALLDATA$LAST_NAME=="Boutin" & ALLDATA$INST=="Alberta"]<-"University of Alberta"
ALLDATA$INST[ALLDATA$LAST_NAME=="Fox" & ALLDATA$INST=="Alberta"]<-"University of Calgary"
ALLDATA$INST[ALLDATA$LAST_NAME=="Patek" & ALLDATA$INST=="Amherst"]<-"University of Massachusetts at Amherst"
ALLDATA$INST[ALLDATA$LAST_NAME=="Mcgraw" & ALLDATA$INST=="Arizona"]<-"Arizona State University"
ALLDATA$COUNTRY[ALLDATA$LAST_NAME=="Vanderheijden" & ALLDATA$INST=="Amsterdam"]<-"Amsterdam"

##########################
# Clean Up Institutions and countries
##########################
# head(ALLDATA,10)
source("functions_data_cleaning/institution_cleaner.R")
ALLDATA<-institution_cleaner(ALLDATA)


##########################
# TODO: ADD AN EDITOR_ID to GCB and MARECOL
missing_edID<-filter(ALLDATA,is.na(ALLDATA$editor_id))
missing_edID$JOURNAL<-as.factor(missing_edID$JOURNAL)
ALLDATA$JOURNAL<-as.factor(ALLDATA$JOURNAL)
summary(missing_edID)
ALLDATA<-droplevels(ALLDATA)
ALLDATA$Inst_Prior_Class
ALLDATA$INST<-as.factor(ALLDATA$INST)
levels(ALLDATA$INST)
levels(ALLDATA$JOURNAL)
ALLDATA$editor_id<-as.factor(ALLDATA$editor_id)
levels(ALLDATA$editor_id)
summary(ALLDATA)
##########################

# HOW MANY YEARS OF EACH JOURNAL?
str(ALLDATA)
JrnlYrs_10<-ALLDATA %>% filter(YEAR>=1985,YEAR<=1994) %>% group_by(JOURNAL) %>% summarise(yrs_per_jrnl=n_distinct(JOURNAL,YEAR)) %>% arrange(yrs_per_jrnl)
JrnlYrs_20<-ALLDATA %>% filter(YEAR>=1985,YEAR<=2004) %>% group_by(JOURNAL) %>% summarise(yrs_per_jrnl=n_distinct(JOURNAL,YEAR)) %>% arrange(yrs_per_jrnl)
JrnlYrs_30<-ALLDATA %>% filter(YEAR>=1985,YEAR<=2014) %>% group_by(JOURNAL) %>% summarise(yrs_per_jrnl=n_distinct(JOURNAL,YEAR)) %>% arrange(yrs_per_jrnl)
JrnlYrs_35<-ALLDATA %>% filter(YEAR>=1985,YEAR<=2019) %>% group_by(JOURNAL) %>% summarise(yrs_per_jrnl=n_distinct(JOURNAL,YEAR)) %>% arrange(yrs_per_jrnl)
head(JrnlYrs_30)
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


ALLDATA$COUNTRY<-as.factor(ALLDATA$COUNTRY)
ALLDATA<-droplevels(ALLDATA)
levels(ALLDATA$COUNTRY)

##############################################################
# TODO: instituion_cleaner
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

##############################################################
##############################################################
# Correcting or systematizing the name/speclling of an institution
##############################################################
##############################################################




# FOR SOME REASON SOME DIDN"T CHANGE< SO NEED TO DO MANUALLY
##############################################################
##############################################################

##############################################################
# CHANGE THOSE WITH BLANK INST to NA
##############################################################
# ALLDATA$INST[ALLDATA$LAST_NAME=="Angeler" & ALLDATA$YEAR>"2011" & ALLDATA$JOURNAL=="JAPE"]<-NA
ALLDATA$INST[ALLDATA$INST==""]<-NA
ALLDATA$INST[ALLDATA$INST=="N/A"]<-NA
ALLDATA$INST[ALLDATA$INST=="."]<-NA

# ##############################################################
# # SAVE THE FILE AS A CSV FOR MANUAL REVIEW AGAIN
# ##############################################################
# UNI_LIST2<-ALLDATA %>% select(INST,COUNTRY) %>% arrange(COUNTRY,INST)
# UNI_LIST2<-UNI_LIST2[!is.na(UNI_LIST2$INST),]
# head(UNI_LIST2,20)
# levels(UNI_LIST2$COUNTRY)<-c(levels(UNI_LIST2$COUNTRY),"missing")
# UNI_LIST2$COUNTRY[is.na(UNI_LIST2$COUNTRY)]<-"missing"
# UNI_LIST2<-UNI_LIST2 %>% group_by(INST,COUNTRY) %>% summarize(count=n())
# 
# write.csv(UNI_LIST2, file="./output_review/uniNameList2.csv", row.names = T) #export it as a csv file



##############################################################
# NOW UPLOAD CORRECTIONS
##############################################################
# 
# corrections2<-read.csv("./output_review/uniNameList_corrections2.csv",encoding = "ASCII",stringsAsFactors = FALSE)
# str(corrections2)
# #THIS CHANGES THE UNITS AND INST BASED ON THE CORRECTION IN THE DATAFRAME
# ALLDATA2<-left_join(ALLDATA, corrections2,by=c("INST","COUNTRY"),copy=TRUE) %>% mutate(INST = ifelse(is.na(INST_CORR), INST, INST_CORR)) %>% mutate(UNIT = ifelse(is.na(UNIT_CORR), UNIT, UNIT_CORR)) %>% mutate(COUNTRY = ifelse(is.na(COUNTRY_CORR), COUNTRY, COUNTRY_CORR))
# ALLDATA<-ALLDATA2 %>% select(-INST_CORR,-UNIT_CORR,-COUNTRY_CORR)
# rm(ALLDATA2)


##############################################################
##############################################################
# DATA CHECK: FILLING IN MISSING INST, CHECKING EDITORS WITH DUPLICATE INST
##############################################################
##############################################################

##############################
##############################
# ADD AND PATRICK JAMES CORRECTIONS
##############################
##############################
source("functions_data_cleaning/JamesCorrections.R")
ALLDATA<-JamesCorrections(ALLDATA)


source("functions_data_cleaning/institution_cleaner.R")
ALLDATA<-institution_cleaner(ALLDATA)



###############################
# REMOVE ANY ROWS WITH NO DATA
ALLDATA<-ALLDATA %>% drop_na(LAST_NAME,FIRST_NAME)
###############################

###############################
# DELETE DUPLICATE ROWS
# TODO: SEE WHICH ONES ARE BIENBG DROPPED
ALLDATA<-distinct(ALLDATA,LAST_NAME,FIRST_NAME,YEAR,TITLE,.keep_all = TRUE)
###############################



###########################
# TODO: FINAL REVIEW OF DATA
############################
############################
# TODO: Review the insitutions for any duplicates, spelling errors, etc

# LIST OF ALL INSTITIONS
ALLDATA_inst_check<-ALLDATA %>%
  select(INST) %>% 
  distinct(INST) %>% 
  arrange(INST)
write_csv(ALLDATA_inst_check,"./output_review/ALLDATA_inst_check.csv")

# LIST OF ALL INSTITIONS BY COUNTRY (easier to see if any were assigned wrong country code)
ALLDATA_inst_check_by_country<-ALLDATA %>%
  select(INST,COUNTRY) %>% 
  distinct(INST,COUNTRY) %>% 
  arrange(COUNTRY,INST)
write_csv(ALLDATA_inst_check_by_country,"./output_review/ALLDATA_inst_check_by_country.csv")

checkINST<-ALLDATA %>% 
  select(INST) %>% 
  distinct(INST) %>% 
  arrange(INST)
# similarity index to to find mispellings etc.
source("./functions_data_cleaning/name.check.R")
NameSimilarityDF<-name.check(checkINST$INST)
write.csv(NameSimilarityDF, file="./output_review/NameSimilarityDF_check.csv", row.names = F) #export it as a csv file
############################

############################
# TODO: FIND and correct any editors with both an editor_id and NA for editor_id
ALLDATA$LAST_NAME<-stri_trans_totitle(ALLDATA$LAST_NAME)
ALLDATA$FIRST_NAME<-stri_trans_totitle(ALLDATA$FIRST_NAME)

ALLDATA$editor_id.y<-NULL
ALLDATA<-ALLDATA %>% rename("editor_id"="editor_id.x")
ALLDATA$editor_id<-as.character(ALLDATA$editor_id)
ALLDATA<-ALLDATA %>% replace_na(list(editor_id = "TBD", editor_id.y = "TBD"))
dup_edID<-ALLDATA %>% 
  select(LAST_NAME,FIRST_NAME,editor_id) %>% 
  group_by(LAST_NAME,FIRST_NAME) %>% 
  mutate(n_id=n_distinct(editor_id)) %>% 
  filter(n_id>1) %>% 
  distinct(LAST_NAME,FIRST_NAME,editor_id,.keep_all=TRUE) %>% 
  arrange(LAST_NAME,FIRST_NAME) 
dup_edID
write.csv(dup_edID, file="./output_review/dup_edID.csv", row.names = F) #export it as a csv file
############################

############################
# TODO: Same as above based on last name (in case the first name is different)
dup_edID2<-ALLDATA %>% 
  select(LAST_NAME,FIRST_NAME,editor_id) %>% 
  group_by(LAST_NAME) %>% 
  mutate(n_id=n_distinct(editor_id)) %>% 
  filter(n_id>1) %>% 
  distinct(LAST_NAME,editor_id,.keep_all=TRUE) %>% 
  arrange(LAST_NAME) 
dup_edID2
write.csv(dup_edID2, file="./output_review/dup_edID2.csv", row.names = F) #export it as a csv file
############################

############################
# TODO: find cases where the same editor has >1 editor_id
dup_edID3<-ALLDATA %>% 
  select(LAST_NAME,FIRST_NAME,editor_id) %>% 
  filter(editor_id!="TBD") %>% 
  group_by(LAST_NAME,FIRST_NAME) %>% 
  mutate(n_names=n_distinct(editor_id)) %>% 
  distinct(LAST_NAME,FIRST_NAME,editor_id,.keep_all=TRUE) %>%
  arrange(desc(LAST_NAME,FIRST_NAME,editor_id)) %>% 
  filter(n_names>1) 
dup_edID3 
# dup_edID3 <-dup_edID3 %>% group_by(FIRST_NAME,LAST_NAME) %>%  arrange(editor_id) %>% slice(2)
write.csv(dup_edID3, file="./output_review/dup_edID3.csv", row.names = F) #export it as a csv file
############################

############################
# TODO: ID ANY EDITORS WITH NO INST LISTED
# could be na or missing
missing_INST<-ALLDATA %>% filter(is.na(INST) | INST=="missing") %>% group_by(JOURNAL) %>% distinct(LAST_NAME,FIRST_NAME) %>% summarize(n=n()) %>% arrange(desc(n))

write.csv(missing_INST, file="./output_review/missing_INST.csv", row.names = F) #export it as a csv file
############################

############################
# TODO: Some of the editors are in multiple times because they have multiple jobs. ID and fix
# ie they could be listed in seperate rows as EIC and SE

############################

############################
# TODO: Still left to fix and 2x
##########################################
# 2x claudia bieber. CVM is in austria but country is australia
# BIOCON: editor_id 2874 and 2875 are the same person!			
# BIOCON: editor_id 3024 country should be Singapore in 2009
# JECOL: editor_id 1279 in 2013: country and state mismatch 
# JECOL editor_ID 703 in 2014: remove zip from state
# JECOL editor_ID 2408 in 2010-2013 should country be MEX or GER? Apparently MEX (mex in state, not country)
# JECOL: several have country listed in state column  
# 2x ythat Vegetation Survey of Western Australia shouldn’t be Univ of Western Australia
# ingrid parmentier should be Universite Libre de Bruxelles 2x journal
# James Cook University ONE OF THESE IS JAMES COOK UNIVERSITY TOWNSVILLE
# Natiral History Museum need to 2x each some are us some are UK
# NEED TO GET PEOPLE BY CAMPUS UNAM
# no one by this name
# NoInst
# Oregon Trail
# Pfenning does he have two researcher iD's? 
# Stephen Simpson Ecology 2002 2x if Oxford, UK, Australia 
# 1 Traveser  Anna       217             2
# 2 Traveset  Anna       217             2
# Troy Day not on Amnat board in 12-14, listed as in Australia
# FUNECOL Data import function has some that still need to be corrected
# COuntries: California, Germanny
##########################################



######################################
#TODO: Correct accents etc into ASCII
######################################
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

###############################



############################
# SUMMARY OF HOW MANY MISSING INST BY JOURNAL

checkINST2<-ALLDATA %>% 
  filter(is.na(INST)|INST=="missing") %>% 
  distinct(JOURNAL,LAST_NAME,COUNTRY) %>% 
  group_by(JOURNAL) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

checkINST2
############################

############################
# Summary of how many editors
ALLDATA %>% 
  distinct(LAST_NAME,FIRST_NAME) %>% 
  summarise(n())
############################



























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

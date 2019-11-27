
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
AUK_raw$JOURNAL<-"AUK"
AUK_raw$editor_id<-NA
AUK<-AUK_raw%>% select(JOURNAL,YEAR, editor_id,EDITOR_TITLE,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY,NOTES)

AUK$INST<-gsub("None given",NA, AUK$INST)
AUK$UNIT<-gsub("None given",NA, AUK$UNIT)
AUK$CITY<-gsub("None given",NA, AUK$CITY)
AUK$STATE<-gsub("None given",NA, AUK$STATE)
AUK$COUNTRY<-gsub("None given",NA, AUK$COUNTRY)

rm(AUK_raw,AUK.csv)

# ##################################
# CONDOR
# ##################################

CONDOR_raw<-CONDOR.csv
CONDOR_raw$JOURNAL<-"CONDOR"
CONDOR_raw$editor_id<-NA

CONDOR<-CONDOR_raw %>% select(JOURNAL,YEAR, editor_id,EDITOR_TITLE,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY,NOTES)
CONDOR$INST<-gsub("None given",NA, CONDOR$INST)
CONDOR$UNIT<-gsub("None given",NA, CONDOR$UNIT)
CONDOR$CITY<-gsub("None given",NA, CONDOR$CITY)
CONDOR$STATE<-gsub("None given",NA, CONDOR$STATE)
CONDOR$COUNTRY<-gsub("None given",NA, CONDOR$COUNTRY)

rm(CONDOR.csv,CONDOR_raw)



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
# # FUNECOL: REVIEW AND DELETE COLS AS NEEDED
# FUNECOL1<-FUNECOLdata_Allen_EB_1dec.csv%>% select(JOURNAL,YEAR, FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,COUNTRY) #NO UNIT, CITY, STATE
# FUNECOL2<-FUNECOL_data_11.03.2017.csv%>% select(JOURNAL,YEAR, FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY,editor_id) #NO UNIT, CITY, STATE

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

LECO<-LECO_2017.csv 

LECO$INST<-as.character(LECO$INST)
LECO$CITY[LECO$INST=="University of Nevada" & LECO$LAST_NAME=="Walker"]<-"Las Vegas"

levels(LECO$STATE) <- c(levels(LECO$STATE),"NV")
LECO$STATE[LECO$INST=="University of Nevada" & LECO$LAST_NAME=="Walker"]<-"NV"
LECO$INST[LECO$INST=="University of Nevada" & LECO$LAST_NAME=="Walker"]<-"University of Nevada-Las Vegas"

LECO$INST[LECO$YEAR==1987 & LECO$LAST_NAME=="Ramos"]<-"missing"
# spot checks of leco

LECO$INST<-trimws(LECO$INST)
LECO$UNIT<-trimws(LECO$UNIT)
LECO$CITY <-trimws(LECO$CITY)
LECO$STATE<-trimws(LECO$STATE)
LECO$COUNTRY<-trimws(LECO$COUNTRY)
LECO$INST[LECO$INST==""]<-NA
LECO$UNIT[LECO$UNIT==""]<-NA
LECO$CITY[LECO$CITY==""]<-NA
LECO$STATE[LECO$STATE==""]<-NA
LECO$COUNTRY[LECO$COUNTRY==""]<-NA
LECO<-LECO %>% arrange(editor_id,YEAR,INST)

# fill in the institutions in subsequent years (only 1st year recorded) and then look for any thiat might need
# to be double checked
head(LECO,10)
LECO<-LECO %>% fill(INST)
LECO<-LECO %>% select(-X,-X.1,-X.2,-X.3,-X.4)
LECO<-rename(LECO,"TITLE"="TITLE.x")
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(LECO)
# Then need to add a function to upload the corrections/additions
##
rm(LECO_2017.csv)

##############################################################
# CONBIO
# 
##############################################################

CONBIO<-CONBIO_EKB.csv

CONBIO$INST<-trimws(CONBIO$INST)
CONBIO$UNIT<-trimws(CONBIO$UNIT)
CONBIO$CITY <-trimws(CONBIO$CITY)
CONBIO$STATE<-trimws(CONBIO$STATE)
CONBIO$COUNTRY<-trimws(CONBIO$COUNTRY)
CONBIO$INST[CONBIO$INST==""]<-NA
CONBIO$UNIT[CONBIO$UNIT==""]<-NA
CONBIO$CITY[CONBIO$CITY==""]<-NA
CONBIO$STATE[CONBIO$STATE==""]<-NA
CONBIO$COUNTRY[CONBIO$COUNTRY==""]<-NA
CONBIO<-CONBIO %>% arrange(editor_id,YEAR,INST)

# fill in the institutions in subsequent years (only 1st year recorded) and then look for any thiat might need
# to be double checked
# CONBIO_fixes<-CONBIO_fixes %>% group_by(editor_id,INST) %>% distinct(editor_id,INST) %>% distinct(editor_id)

head(CONBIO,10)
CONBIO<-CONBIO %>% fill(INST)
CONBIO<-rename(CONBIO,"TITLE"="TITLE.x")

##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(CONBIO)
# Then need to add a function to upload the corrections/additions
##
rm(CONBIO_EKB.csv)
##############################################################
# JZOOLOGY
##############################################################

# JZOOL HAS MULTIPLE DATASETS TO UPLOAD
JZOOL1<-JZOOL17nov.csv
JZOOL2<-JZOOL.csv



JZOOL1$JOURNAL<-as.factor("JZOOL")
summary(JZOOL1$JOURNAL)
summary(JZOOL2$JOURNAL)
JZOOL<-full_join(JZOOL1,JZOOL2,by=c("YEAR","LAST_NAME","FIRST_NAME","MIDDLE_NAME"))
rm(JZOOL1,JZOOL2)
str(JZOOL)
colnames(JZOOL)
JZOOL$JOURNAL.x<-as.character(JZOOL$JOURNAL.x)
JZOOL$JOURNAL.y<-as.character(JZOOL$JOURNAL.y)
summary(JZOOL$JOURNAL.x==JZOOL$JOURNAL.y)
JZOOL$JOURNAL<-"JZOOL" 

JZOOL$COUNTRY.x<-as.character(JZOOL$COUNTRY.x)
JZOOL$COUNTRY.y<-as.character(JZOOL$COUNTRY.y)
summary(JZOOL$COUNTRY.x==JZOOL$COUNTRY.y)
country_fix<-which((JZOOL$COUNTRY.x==JZOOL$COUNTRY.y)=="FALSE")
country_fix.df<-JZOOL[country_fix,]  #KEEP COUNTRIES IN UK AS ORIGINAL NAMES< CONVERT TO UK LATER

JZOOL$NAME.x<-as.character(JZOOL$NAME.x)
JZOOL$NAME.y<-as.character(JZOOL$NAME.y)
summary(JZOOL$NAME.x==JZOOL$NAME.y)
NAME_fix<-which((JZOOL$NAME.x==JZOOL$NAME.y)=="FALSE")
NAME_fix.df<-JZOOL[NAME_fix,]  
# the different ones are due to periods after initials

JZOOL$INST.x<-as.character(JZOOL$INST.x)
JZOOL$INST.y<-as.character(JZOOL$INST.y)
summary(JZOOL$INST.x==JZOOL$INST.y)
INST_fix<-which((JZOOL$INST.x==JZOOL$INST.y)=="FALSE")
INST_fix.df<-JZOOL[INST_fix,]  

JZOOL$VOLUME.x<-as.character(JZOOL$VOLUME.x)
JZOOL$VOLUME.y<-as.character(JZOOL$VOLUME.y)
summary(JZOOL$VOLUME.x==JZOOL$VOLUME.y)
vol_fix<-which((JZOOL$VOLUME.x==JZOOL$VOLUME.y)=="FALSE")

JZOOL$TITLE.x<-as.character(JZOOL$TITLE.x)
JZOOL$TITLE<-as.character(JZOOL$TITLE)
summary(JZOOL$TITLE.x==JZOOL$TITLE)
title_fix<-which((JZOOL$TITLE.x==JZOOL$TITLE)=="FALSE")
title_fix.df<-JZOOL[title_fix,]  
title_fix.df<-select(title_fix.df,TITLE,TITLE.x)

JZOOL$ISSUE.x<-as.character(JZOOL$ISSUE.x)
JZOOL$ISSUE.y<-as.character(JZOOL$ISSUE.y)
summary(JZOOL$ISSUE.x==JZOOL$ISSUE.y)
issue_fix<-which((JZOOL$ISSUE.x==JZOOL$ISSUE.y)=="FALSE")
issue_fix.df<-JZOOL[issue_fix,]

JZOOL[55,]$ISSUE.x<-JZOOL[55,]$ISSUE.y
JZOOL[56,]$ISSUE.x<-JZOOL[56,]$ISSUE.y
JZOOL[57,]$ISSUE.x<-JZOOL[57,]$ISSUE.y

JZOOL<-JZOOL %>% select(-JOURNAL.x,-VOLUME.y,-JOURNAL.y,-ISSUE.y,-NAME.x,-NAME.y,-INST.y,-COUNTRY.y,-CATEGORY.y,-TITLE.x) %>% 
  rename("VOLUME"="VOLUME.x","ISSUE"="ISSUE.x","INST"="INST.x","CATEGORY"="CATEGORY.x","COUNTRY"="COUNTRY.x")
rm(INST_fix.df,INST_fix,country_fix,country_fix.df,vol_fix,issue_fix,issue_fix.df,NAME_fix,NAME_fix.df,title_fix,title_fix.df)
# str(JZOOL)
JZOOL<-JZOOL%>%select(JOURNAL,YEAR, VOLUME,ISSUE,editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,TITLE,CATEGORY,INST,UNIT,CITY,STATE,COUNTRY,geo.code,NOTES,GENDER)
# str(JZOOL)
JZOOL$JOURNAL<-as.character(JZOOL$JOURNAL)
JZOOL$JOURNAL[JZOOL$JOURNAL=="JZ"]<-"JZOOL"
JZOOL$JOURNAL<-as.factor(JZOOL$JOURNAL)

JZOOL$INST<-trimws(JZOOL$INST)
JZOOL$UNIT<-trimws(JZOOL$UNIT)
JZOOL$CITY <-trimws(JZOOL$CITY)
JZOOL$STATE<-trimws(JZOOL$STATE)
JZOOL$COUNTRY<-trimws(JZOOL$COUNTRY)
JZOOL$INST[JZOOL$INST==""]<-NA
JZOOL$UNIT[JZOOL$UNIT==""]<-NA
JZOOL$CITY[JZOOL$CITY==""]<-NA
JZOOL$STATE[JZOOL$STATE==""]<-NA
JZOOL$COUNTRY[JZOOL$COUNTRY==""]<-NA
JZOOL<-JZOOL %>% arrange(editor_id,YEAR,INST) %>% select(-VOLUME, -ISSUE, -TITLE,-CATEGORY,-geo.code)
str(JZOOL)
# fill in the institutions in subsequent years (only 1st year recorded) and then look for any thiat might need
# to be double checked
head(JZOOL,10)
# NEED TO ADD "missing" to 1st line of group by edito where NA
JZOOL_1row<-JZOOL %>% group_by(editor_id) %>% 
  arrange(editor_id,YEAR) %>% 
  filter(row_number()==1)
levels(JZOOL_1row$INST)<-c(levels(JZOOL_1row$INST),"missing")
JZOOL_1row$INST<-replace(JZOOL_1row$INST, is.na(JZOOL_1row$INST), "missing")

JZOOL_remainder<-JZOOL %>% group_by(editor_id) %>% 
  arrange(editor_id,YEAR) %>% 
  filter(row_number()>1)

JZOOL<-bind_rows(JZOOL_remainder,JZOOL_1row) 
head(JZOOL,10)
JZOOL<-JZOOL %>% arrange(editor_id,YEAR) %>% fill(INST)
head(JZOOL,10)

##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(JZOOL)
# Then need to add a function to upload the corrections/additions
##

rm(JZOOL_1row,JZOOL_remainder,JZOOL.csv,JZOOL17nov.csv)
##############################################################
# NEW PHYT
##############################################################
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(NEWPHYTE)
# Then need to add a function to upload the corrections/additions
##

NEWPHYT<-NEWPHYT_21july2018.csv

head(NEWPHYT,10)
NEWPHYT<-NEWPHYT %>% rename("TITLE"="TITLE.x")
rm(NEWPHYT_21july2018.csv)
##############################################################
# BIOLOGICAL CONSERVATION
##############################################################
# BIOCON_corrections
# These are in "./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_7_JAPE.xlsx"

BIOCON<-BIOCON_TS.csv


# DELETE THEIR RECORD FOR GIVEN YEAR (not on board)
BIOCON<-BIOCON[!(BIOCON$editor_id==2399 & BIOCON$YEAR==1997),]
BIOCON<-BIOCON[!(BIOCON$editor_id==2830 & BIOCON$YEAR==1995),]
BIOCON<-BIOCON[!(BIOCON$editor_id==2830 & BIOCON$YEAR==1996),]
BIOCON<-BIOCON[!(BIOCON$editor_id==566 & BIOCON$YEAR==1988),]

# CORRECT RECORDS
BIOCON$INST[BIOCON$LAST_NAME=="ATKINSON" & BIOCON$editor_id==1388 & BIOCON$YEAR==1997]<-"Ecological Research Associates of New Zealand"
BIOCON$INST[BIOCON$LAST_NAME=="DIRZO" & BIOCON$YEAR==1998]<-"Northern Arizona University"
BIOCON$CITY[BIOCON$LAST_NAME=="DIRZO" & BIOCON$YEAR==1998]<-"Flagstaff"

BIOCON$INST[BIOCON$LAST_NAME=="Guyer" & BIOCON$YEAR==1998]<-"Auburn University"
BIOCON$UNIT[BIOCON$LAST_NAME=="Guyer" & BIOCON$YEAR==1998]<-"Zoology"
BIOCON$STATE[BIOCON$LAST_NAME=="Guyer" & BIOCON$YEAR==1998]<-"Alabama"
BIOCON$CITY[BIOCON$LAST_NAME=="Guyer" & BIOCON$YEAR==1998]<-"Auburn"
BIOCON$COUNTRY[BIOCON$LAST_NAME=="Guyer" & BIOCON$YEAR==1998]<-"USA"

BIOCON$INST[BIOCON$editor_id==2399 & BIOCON$YEAR==1998]<-BIOCON$INST[BIOCON$editor_id==2399 & BIOCON$YEAR==1999]
BIOCON$UNIT[BIOCON$editor_id==2399 & BIOCON$YEAR==1998]<-NA
BIOCON$CITY[BIOCON$editor_id==2399 & BIOCON$YEAR==1998]<-"Penicuik"
BIOCON$STATE[BIOCON$editor_id==2399 & BIOCON$YEAR==1998]<-NA
BIOCON$COUNTRY[BIOCON$editor_id==2399 & BIOCON$YEAR==1998]<-"UK"

BIOCON$INST[BIOCON$editor_id==3143 & BIOCON$YEAR==1998]<-BIOCON$INST[BIOCON$editor_id==3143 & BIOCON$YEAR==1999]

BIOCON$INST[BIOCON$editor_id==3108 & BIOCON$YEAR==1997]<-"University of Toronto"
BIOCON$CITY[BIOCON$editor_id==3108 & BIOCON$YEAR==1997]<-"Toronto"
BIOCON$STATE[BIOCON$editor_id==3108 & BIOCON$YEAR==1997]<-"Ontario"

levels(BIOCON$INST)<-c(levels(BIOCON$INST),"University of Capetown")
BIOCON$INST[BIOCON$editor_id==2830 & BIOCON$YEAR==1996]<-"University of Capetown"
levels(BIOCON$CITY)<-c(levels(BIOCON$CITY),"Capetown")
BIOCON$CITY[BIOCON$editor_id==2830 & BIOCON$YEAR==1996]<-"Capetown"


BIOCON$YEAR[BIOCON$editor_id==3269 & BIOCON$VOL==121]<-2006
BIOCON$INST[BIOCON$editor_id==3269 & BIOCON$YEAR==2005]<-BIOCON$INST[BIOCON$editor_id==3269 & BIOCON$YEAR==2006]
BIOCON$UNIT[BIOCON$editor_id==3269 & BIOCON$YEAR==2005]<-BIOCON$UNIT[BIOCON$editor_id==3269 & BIOCON$YEAR==2006]
BIOCON$STATE[BIOCON$editor_id==3269 & BIOCON$YEAR==2005]<-BIOCON$STATE[BIOCON$editor_id==3269 & BIOCON$YEAR==2006]
BIOCON$CITY[BIOCON$editor_id==3269 & BIOCON$YEAR==2005]<-"Santa Barbara"
BIOCON$COUNTRY[BIOCON$editor_id==3269 & BIOCON$YEAR==2005]<-"USA"
BIOCON$NOTES[BIOCON$editor_id==3269 & BIOCON$YEAR==2005]<-NA

BIOCON$CATEGORY[BIOCON$editor_id==3269 & BIOCON$YEAR==2005]<-"AE"



# ADD RECORDS (Not included for some years)
Pressey_2005<-data.frame(editor_id=342, FIRST_NAME="Bob",LAST_NAME="Pressey",NAME="Bob Pressey", INST="New South Wales Evironment and Conservation",CITY="Kensington",STATE="NSW",COUNTRY="Australia") 
Wright_1998<-data.frame(editor_id=3255, FIRST_NAME="R",MIDDLE_NAME="Gerald",LAST_NAME="Wright",NAME="R Gerald Wright", INST="University of Idaho",UNIT="Wildlife Resources",CITY="Moscow",STATE="Idaho",COUNTRY="USA") 
Kirby_2005<-BIOCON[BIOCON$editor_id==2030 & BIOCON$YEAR==2006,]
Kirby_2005$YEAR<-2005
Lind_2005<-BIOCON[BIOCON$editor_id==765 & BIOCON$YEAR==2006,]
Lind_2005$YEAR<-2005
Lipps_2005<-BIOCON[BIOCON$editor_id==2043 & BIOCON$YEAR==2006,]
Lipps_2005$YEAR<-2005
JPM_2006<-BIOCON[BIOCON$editor_id==1763 & BIOCON$YEAR==2007,]
JPM_2006$YEAR<-2006
WFL_2005<-BIOCON[BIOCON$editor_id==3763 & BIOCON$YEAR==2006,]
WFL_2005$YEAR<-2005
WALDREN_2005<-BIOCON[BIOCON$editor_id==3496 & BIOCON$YEAR==2006,]
WALDREN_2005$YEAR<-2005


BIOCON_ADDS<-bind_rows(Pressey_2005,Wright_1998,Kirby_2005,Lind_2005,JPM_2006,WFL_2005,Lipps_2005,WALDREN_2005)
BIOCON<-bind_rows(BIOCON,BIOCON_ADDS)
rm(Pressey_2005,Wright_1998,Kirby_2005,Lind_2005,JPM_2006,WFL_2005,Lipps_2005,WALDREN_2005)

head(BIOCON,10)
BIOCON<-rename(BIOCON,"TITLE"="TITLE.x")


rm(BIOCON_TS.csv,BIOCON_ADDS,BIOCON_TS.csv)

##############################################################
# AGRONOMY
##############################################################



AG<-AGRONOMY_data_11.02.2017.csv 

# AGRONOMY MISSING INST
AG$INST<-as.factor(AG$INST)

AG$INST<-as.character(AG$INST)
AG$STATE<-as.character(AG$STATE)
AG$COUNTRY<-as.character(AG$COUNTRY)
AG$INST[AG$LAST_NAME=="Benbi" & AG$FIRST_NAME=="Dinesh"]<-"Punjab Agricultural University"
AG$STATE[AG$LAST_NAME=="Benbi" & AG$FIRST_NAME=="Dinesh"]<-"Punjab"
AG$COUNTRY[AG$LAST_NAME=="Benbi" & AG$FIRST_NAME=="Dinesh"]<-"India"
AG$COUNTRY[AG$LAST_NAME=="Pedreira" & AG$FIRST_NAME=="Carlos"]<-"Brazil"
AG<-AG %>% rename("TITLE"="TITLE.x")
head(AG,10)

##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(AGRONOMY)
# Then need to add a function to upload the corrections/additions
##

rm(AGRONOMY_data_11.02.2017.csv)
##############################################################
# FEM
##############################################################

FEM<-FEM_7112017.csv


FEM<-FEM %>% rename("TITLE"="TITLE.x")
head(FEM,10)
## NOT CORRECT EITHER INST OR CITY STATE!!!
# FEM 1994 Colorado State University     USA       775    Douglas           A   Maguire      Seattle Washington              <NA>   <NA>
rm(FEM_7112017.csv)
##############################################################
# J ANIMAL ECOLOGY
##############################################################

JANE<-JANE.csv

JANE$UNIT<-as.character(JANE$UNIT)
JANE$INST<-as.character(JANE$INST)
JANE$UNIT[JANE$editor_id==1702]<-"Deptartment of Biology"
JANE$INST[JANE$editor_id==1702]<-"University of York"
JANE$COUNTRY[JANE$editor_id==1702]<-"UK"
head(JANE,10)
JANE<-JANE %>% rename("TITLE"="TITLE.x")
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(JANE)
# Then need to add a function to upload the corrections/additions
##
rm(JANE.csv)

##############################################################
# J BIOGEOGRAPHY
##############################################################

JBIOG<-JBIOG.csv

# JBIOG<-JBIOG.csv%>% select(JOURNAL,YEAR, editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,CITY,STATE,COUNTRY)
JBIOG$INST<-as.character(JBIOG$INST)
JBIOG<-JBIOG %>% separate(INST, c("UNIT", "INST"),",",extra="merge",fill="left",remove=TRUE)
JBIOG$INST[JBIOG$UNIT=="Uc Merced"]<-"University of California-Merced"
JBIOG$UNIT[JBIOG$UNIT=="Uc Merced"]<-NA
JBIOG$INST[JBIOG$INST==" School Of Life Sciences, Arizona State University"]<-"Arizona State University"
JBIOG$UNIT[JBIOG$UNIT=="Institute For Species Exploration"]<-"Institute For Species Exploration,School of Life Sciences"
JBIOG$INST[JBIOG$UNIT=="Evolution And Marine Biology, University of California"]<-"University of California-Santa Barbara"
JBIOG$UNIT[JBIOG$FIRST_NAME=="Dov"]<-"Department of Ecology, Evolution, and Marine Biology"

# spot checks of JBIOG
JBIOG$INST<-trimws(JBIOG$INST)
JBIOG$UNIT<-trimws(JBIOG$UNIT)
JBIOG$CITY <-trimws(JBIOG$CITY)
JBIOG$STATE<-trimws(JBIOG$STATE)
JBIOG$COUNTRY<-trimws(JBIOG$COUNTRY)
JBIOG$INST[JBIOG$INST==""]<-NA
JBIOG$UNIT[JBIOG$UNIT==""]<-NA
JBIOG$CITY[JBIOG$CITY==""]<-NA
JBIOG$STATE[JBIOG$STATE==""]<-NA
JBIOG$COUNTRY[JBIOG$COUNTRY==""]<-NA
JBIOG<-JBIOG %>% arrange(editor_id,YEAR,INST)

# fill in the institutions in subsequent years (only 1st year recorded) and then look for any thiat might need

# NEED TO ADD "missing" to 1st line of group by edito where NA
JBIOG_1row<-JBIOG %>% group_by(editor_id) %>% 
  arrange(editor_id,YEAR) %>% 
  filter(row_number()==1)
levels(JBIOG_1row$INST)<-c(levels(JBIOG_1row$INST),"missing")
JBIOG_1row$INST<-replace(JBIOG_1row$INST, is.na(JBIOG_1row$INST), "missing")

JBIOG_remainder<-JBIOG %>% group_by(editor_id) %>% 
  arrange(editor_id,YEAR) %>% 
  filter(row_number()>1)

JBIOG<-bind_rows(JBIOG_remainder,JBIOG_1row)
head(JBIOG,70)
JBIOG<-JBIOG %>% arrange(editor_id,YEAR) %>% fill(INST) %>% rename("TITLE"="TITLE.x")
head(JBIOG,10)

rm(JBIOG_1row,JBIOG_remainder,JBIOG.csv)
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(JBIOG)
# Then need to add a function to upload the corrections/additions
##


##############################################################
# PLANT ECOLOGY
##############################################################

PLANTECOL<-PLANTECOL_new.csv 


# some editing
PLANTECOL$INST<-as.character(PLANTECOL$INST)
PLANTECOL$INST[PLANTECOL$LAST_NAME=="Veblen"]<-"University of Colorado-Boulder"
PLANTECOL$INST[PLANTECOL$LAST_NAME=="Picket"]<-"New York Botanical Garden"
PLANTECOL$INST[PLANTECOL$LAST_NAME=="Peet"]<-"University of North Carolina-Chapel Hill"
PLANTECOL$INST[PLANTECOL$LAST_NAME=="Damman"]<-"University of Connecticut"
PLANTECOL$INST[PLANTECOL$LAST_NAME=="Pickett" & PLANTECOL$FIRST_NAME=="Steward"]<-"New York Botanical Garden"

PLANTECOL$INST<-trimws(PLANTECOL$INST)
PLANTECOL$UNIT<-trimws(PLANTECOL$UNIT)
PLANTECOL$CITY <-trimws(PLANTECOL$CITY)
PLANTECOL$STATE<-trimws(PLANTECOL$STATE)
PLANTECOL$COUNTRY<-trimws(PLANTECOL$COUNTRY)
PLANTECOL$INST[PLANTECOL$INST==""]<-NA
PLANTECOL$UNIT[PLANTECOL$UNIT==""]<-NA
PLANTECOL$CITY[PLANTECOL$CITY==""]<-NA
PLANTECOL$STATE[PLANTECOL$STATE==""]<-NA
PLANTECOL$COUNTRY[PLANTECOL$COUNTRY==""]<-NA
PLANTECOL<-PLANTECOL %>% arrange(editor_id,YEAR,INST)

# fill in the institutions in subsequent years (only 1st year recorded) and then look for any thiat might need
# to be double checked
PLANTECOL_fixes<-PLANTECOL_fixes %>% group_by(editor_id,INST) %>% distinct(editor_id,INST) %>% distinct(editor_id)
head(PLANTECOL,10)
PLANTECOL<-PLANTECOL %>% fill(INST) %>% rename("TITLE"="TITLE.x")
head(PLANTECOL,10)
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(PLANTECOL)
# Then need to add a function to upload the corrections/additions
##
rm(PLANTECOL_fixes,PLANTECOL_new.csv)
##############################################################
# EVOLUTION
##############################################################

EVOL<-EVOL.csv

EVOL$COUNTRY<-as.character(EVOL$COUNTRY)
EVOL$STATE<-as.character(EVOL$STATE)
EVOL$COUNTRY[EVOL$LAST_NAME=="Knowlton" & EVOL$FIRST_NAME=="Nancy"]<-"Panama"
EVOL$STATE[EVOL$LAST_NAME=="Knowlton" & EVOL$FIRST_NAME=="Nancy"]<-NA
EVOL<-EVOL %>% rename("TITLE"="TITLE.x")
head(EVOL,10)
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(EVOL)
# Then need to add a function to upload the corrections/additions
##
rm(EVOL.csv)
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


JAPE<-JAPE_new.csv


head(JAPE,10)
JAPE<-JAPE %>% rename("TITLE"="TITLE.x")
head(JAPE,10)
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(AMNAT)
# Then need to add a function to upload the corrections/additions
##
rm(JAPE_new.csv)
##############################################################
# OECOLOGIA
##############################################################

OECOL<-OECOL.csv

OECOL$INST<-as.character(OECOL$INST)
OECOL$CITY[OECOL$INST=="University of Nevada" & OECOL$LAST_NAME=="Walker"]<-"Las Vegas"
OECOL$STATE[OECOL$INST=="University of Nevada" & OECOL$LAST_NAME=="Walker"]<-"NV"
OECOL$INST[OECOL$INST=="University of Nevada" & OECOL$LAST_NAME=="Hayes"]<-"University of Nevada-Reno"
OECOL<-OECOL %>% rename("TITLE"="TITLE.x")
head(OECOL,10)
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(OECOLOGIA)
# Then need to add a function to upload the corrections/additions
##
rm(OECOL.csv)
##############################################################
# GCB
##############################################################

GCB<-GCBdata.csv

# TODO: GCB Need to 
GCB<- GCB %>% extract(NAME, c("FIRST_NAME","LAST_NAME"), "([^ ]+) (.*)")
GCB<-GCB %>% separate(LAST_NAME, c("MIDDLE_NAME", "LAST_NAME"),sep = " ", fill = "left",REMOVE=FALSE)
GCB$editor_id<-NA

head(GCB,10)
# 2) get title abbreviations 
# 3) Make consistent 
# 4) only have data 1995-2007 (1995 was vol 1), 2008-2015 
# 5) disambiguate editors and add editor id numbers

rm(GCBdata.csv)
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(GCB)
# Then need to add a function to upload the corrections/additions
##

##############################################################
# NAJFM
##############################################################
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(NAJFM)
# Then need to add a function to upload the corrections/additions
##

NAJFM<-NAJFM_21july2018.csv 

head(NAJFM,10)
NAJFM<-NAJFM %>% rename("TITLE"="TITLE.x")
rm(NAJFM_21july2018.csv)
##############################################################
# MARECOL
##############################################################

# TODO: MARECOL
# 1) SPLIT name
# 2) Disambiguate and add editor ID
# 3) NEEDS INSTITIONS

MARECOL<-MARECOL_21July2018.csv 

MARECOL$INST<-as.factor(MARECOL$INST)
MARECOL<-rename(MARECOL,"VOLUME"="Volume","ISSUE"="Issue")
str(MARECOL)
MARECOL$no<-NULL

MARECOL$FIRST_NAME<-NULL
MARECOL$MIDDLE.NAME<-NULL
MARECOL$LAST_NAME<-NULL
MARECOL$editor_id<-NA
MARECOL<- MARECOL %>% extract(NAME, c("FIRST_NAME","LAST_NAME"), "([^ ]+) (.*)")
MARECOL$FIRST_NAME<-gsub("[.]","",MARECOL$FIRST_NAME)
MARECOL<-MARECOL %>% separate(LAST_NAME, c("MIDDLE_NAME", "LAST_NAME"),sep = ". ", extra="merge",fill = "left",remove=FALSE)


head(MARECOL,10)
MARECOL<-MARECOL %>% rename("TITLE"="Title") %>% select(JOURNAL,YEAR,VOLUME,ISSUE,TITLE,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY,editor_id)
head(MARECOL,10)
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(MARECOL)
# Then need to add a function to upload the corrections/additions
##
rm(MARECOL_21July2018.csv)

##############################################################
# AJB
##############################################################

AJB<-AJB.csv
head(AJB,10)
AJB<-rename(AJB,"TITLE"="TITLE.x")
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(MEPS)
# Then need to add a function to upload the corrections/additions
##

rm(AJB.csv)
##############################################################
# OIKOS
##############################################################

OIKOS<-OIKOS_21july2018.csv

head(OIKOS,10)
OIKOS<-rename(OIKOS,"TITLE"="TITLE.x")
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(MEPS)
# Then need to add a function to upload the corrections/additions
##

rm(OIKOS_21july2018.csv)


##############################################################
# ECOGRAPHY
##############################################################

ECOG<-ECOGRAPHY_5112017.csv

head(ECOG,10)
ECOG<-rename(ECOG,"TITLE"="TITLE.x")
##
# code to generate the list of which ones need to be 2x or have missing names is in 
# Author_and_Inst_to_Check.R  
# Need to convert that to a function, e.g., check(MEPS)
# Then need to add a function to upload the corrections/additions
##
rm(ECOGRAPHY_5112017.csv)
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






































##########################################
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
# need to 2x University of Illinois Urbana Champaign
# need to 2x UT Austin
# NEED TO GET PEOPLE BY CAMPUS UNAM
# no one by this name
# NoInst
# Oregon Trail
# Pfenning does he have two researcher iD's? 
# Stephen Simpson Ecology 2002 2x if Oxford, UK, Australia 
# U of California MANY TO SORT OUT
# what is NRII?
##########################################


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



# TODO: FIND THE DUPLICATED ONES AND CORRECT THEM

DISTINCT<-ALLDATA %>% distinct(JOURNAL, YEAR, editor_id, .keep_all = TRUE)

alldata_dupes<-ALLDATA %>% group_by(JOURNAL,LAST_NAME,FIRST_NAME,YEAR) %>% filter(n()>1)
alldata_dupes<-droplevels(alldata_dupes)
levels(alldata_dupes$JOURNAL)






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

# This will load and the corrected files and make the required changes.
JamesCorrections <- function(ORIGINAL_DATA) {
  # ORIGINAL_DATA<-ALLDATA  
library(tidyverse)

  # source("functions_data_cleaning/institution_cleaner.R")
##############################################################
# NO INST 2x NEEDED
# AJB 
# ECOLOGY
# FEM
# FUNECOL
# ECOGRAPHY
# JTE
##############################################################

##############################################################
# checked by Patrick, need to upload corrections
# Round 1: AGRON, ARES, EVOL (DONE)
# Round 2a: CONBIO,NEW PHYT (DONE)
# Round 2b: NEW PHYT (DONE)
# Round 3: BITR (DONE)
# Round 4: AMNAT (DONE, EB 2x)
# Round 6: JECOL (DONE)
# Round 7: JAPE (DONE)
# Round 5: BIOCON (DONE, need to upload and integrate the corrections)
# Round 8: PLANTECOL (DONE, to be emailed)
# Round 9: JBIOG (2x in progress, almost done - need to add institutions that were "missing")
# Round 10: LECO (DONE, 2x the ones "incorrect" to see if the INST value in the table is the corrected or original")
# JANE: Needs extensive data fill
# JZOOL: Needs extensive data fill
# OECOL: Needs extensive data fill
# OIKOS: (DONE)
# AUK: not for this paper
# CONDOR: not for this paper
##############################################################

##########
multi1<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_1.csv", col_names = TRUE)
multi1[multi1=="missing"]<-NA
multi1<-multi1 %>%
  group_by(LAST_NAME,FIRST_NAME) %>% 
  arrange(YEAR) %>% 
  fill(INST,UNIT,CITY,STATE,.direction="down")
##########

##########
multi2a<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_2a.csv", col_names = TRUE)
multi2a[multi2a=="missing"]<-NA
multi2a<-multi2a %>% filter(JOURNAL=="CONBIO"|JOURNAL=="NEWPHYT") %>% 
  group_by(LAST_NAME,FIRST_NAME) %>% 
  arrange(YEAR) %>% 
  fill(INST,UNIT,CITY,STATE,.direction="down") #delete out other journals this is conbio and new phyt
##########

##########
multi2b<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_2b.csv", col_names = TRUE)
multi2b[multi2b=="missing"]<-NA
multi2b<-multi2b %>% 
  filter(JOURNAL=="CONBIO"|JOURNAL=="NEWPHYT") %>% 
  group_by(LAST_NAME,FIRST_NAME) %>% 
  arrange(YEAR) %>% 
  fill(INST,UNIT,CITY,STATE,.direction="down") #delete out other journals this is conbio and new phyt
##########

##########
BITR_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_3_BITR.csv", col_names = TRUE)
BITR_inst[BITR_inst=="missing"]<-NA
BITR_inst<-BITR_inst %>% 
  group_by(LAST_NAME,FIRST_NAME) %>% 
  arrange(YEAR) %>% 
  fill(INST,UNIT,CITY,STATE,.direction="down")

##########

##########
AMNAT_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_4_AMNAT.csv", col_names = TRUE,na = c("", "N/A", "NA"), trim_ws = TRUE)
AMNAT_inst[AMNAT_inst=="missing"]<-NA
AMNAT_inst<-AMNAT_inst %>% 
  group_by(LAST_NAME,FIRST_NAME) %>% 
  arrange(YEAR) %>% 
  fill(INST,UNIT,CITY,STATE,.direction="down") %>% 
  filter(JOURNAL=="AMNAT")
##########

##########
JECOL_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_6_JEcol.csv", col_names = TRUE)
JECOL_inst[JECOL_inst=="missing"]<-NA
JECOL_inst<-JECOL_inst %>% 
  group_by(LAST_NAME,FIRST_NAME) %>%
  arrange(YEAR) %>% 
  fill(INST,UNIT,CITY,STATE,.direction="down") %>% 
  filter(JOURNAL=="JECOL")
##########

##########
JAPE_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_7_JAPE.csv", col_names = TRUE)
JAPE_inst[JAPE_inst=="missing"]<-NA
JAPE_inst[JAPE_inst=="unknown"]<-NA
JAPE_inst<-JAPE_inst %>% 
  rename("FIRST_NAME"="FIRST_NA", "MIDDLE_NAME"="MIDDLE_","LAST_NAME"="LAST_NA") %>% 
  group_by(LAST_NAME,FIRST_NAME) %>% 
  arrange(YEAR) %>% 
  fill(INST,UNIT,CITY,STATE,COUNTRY,NOTES,.direction="down") %>% 
  filter(JOURNAL=="JAPE")
JAPE_inst$editor_id<-NULL
# TODO: something is going on with editot IDS
##########

##########
# SEPARATE FUNCTION
# JBIOG LECO PLANTECOL OIKOS  OECOLOGIA JANE 
###########


#############
# INST_fix<-bind_rows(multi2b,multi2a,multi1,BITR_inst,JECOL_inst,AMNAT_inst,JAPE_inst,JBIOG_inst,OIKOS_inst) %>% 
#   distinct(editor_id,JOURNAL,YEAR,.keep_all= TRUE) %>%     #there are some duplicates, best to remove them
#   arrange(JOURNAL,editor_id,YEAR)
# rm(multi2b,multi2a,multi1,BITR_inst,JECOL_inst,AMNAT_inst,JAPE_inst,JBIOG_inst,OIKOS_inst)

# NO OIKOS
INST_fix<-bind_rows(multi2b,multi2a,multi1,BITR_inst,JECOL_inst,AMNAT_inst,JAPE_inst) %>% 
  distinct(LAST_NAME,JOURNAL,YEAR,.keep_all= TRUE) %>%     #there are some duplicates, best to remove them
  arrange(JOURNAL,editor_id,YEAR)
rm(multi2b,multi2a,multi1,BITR_inst,JECOL_inst,AMNAT_inst,JAPE_inst)


# colnames(INST_fix)
# colnames(OECOL_inst)

INST_fix$INST[INST_fix$INST=="UNKNOWN"]<-NA
INST_fix$INST[INST_fix$INST=="unknown"]<-NA
INST_fix$UNIT[INST_fix$UNIT=="UNKNOWN"]<-NA
INST_fix$CITY[INST_fix$CITY=="UNKNOWN"]<-NA
INST_fix$STATE[INST_fix$STATE=="UNKNOWN"]<-NA
INST_fix$STATE[INST_fix$STATE=="unknown"]<-NA


# INST_fix<-institution_cleaner(INST_fix)
INST_fix<-INST_fix %>% select(-X1)

head(INST_fix,20)
#
# 
# # ORIGINAL_DATA$editor_id<-as.numeric(ORIGINAL_DATA$editor_id)
# # ARE THERE ANY IN CORRECT that *ARENT* in ORIGINAL_DATA?
# # THESE NEED TO BE ADDED TO ORIGINAL_DATA
# # C_but_not_O<-anti_join(INST_fix,ORIGINAL_DATA,by=c("editor_id","JOURNAL","YEAR")) %>% arrange(JOURNAL,YEAR,editor_id)  #in correct but not orig 24
# C_but_not_O<-anti_join(INST_fix,ORIGINAL_DATA,by=c("JOURNAL","YEAR","LAST_NAME")) %>% arrange(JOURNAL,YEAR,editor_id)  #in correct but not orig 24
# nrow(C_but_not_O)
# summary(C_but_not_O)
# 
# #THESE ARE THE ONES IN ORIGINAL_DATA but not CORRECTED 
# # O_butnot_C<-anti_join(ORIGINAL_DATA,INST_fix,by=c("editor_id","JOURNAL","YEAR")) # in orig but not correct 21221
# O_butnot_C<-anti_join(ORIGINAL_DATA,INST_fix,by=c("JOURNAL","YEAR","LAST_NAME")) # in orig but not correct 21221
# nrow(O_butnot_C)
# O_butnot_C
# 
# # THESE ARE THE ONES IN BOTH CORRECTED AND ORIGINAL_DATA
# # O_and_C<-inner_join(ORIGINAL_DATA,INST_fix,by=c("editor_id","JOURNAL","YEAR")) #in correct but not orig 4381
# O_and_C<-inner_join(ORIGINAL_DATA,INST_fix,by=c("LAST_NAME","JOURNAL","YEAR")) #in correct but not orig 4381
# nrow(O_and_C)
# 
# colnames(ORIGINAL_DATA)
# colnames(INST_fix)
# str(ORIGINAL_DATA)
# str(INST_fix)



# both<-full_join(ORIGINAL_DATA,INST_fix,by=c("editor_id","JOURNAL","YEAR")) 

INST_fix$editor_id<-as.character(INST_fix$editor_id)
str(INST_fix$editor_id)
str(ORIGINAL_DATA$editor_id)
ORIGINAL_DATA$editor_id<-as.factor(ORIGINAL_DATA$editor_id)
INST_fix$editor_id<-as.factor(INST_fix$editor_id)

both<-full_join(ORIGINAL_DATA,INST_fix,by=c("JOURNAL","YEAR","LAST_NAME","FIRST_NAME")) 
nrow(both)
nrow(ORIGINAL_DATA)
nrow(INST_fix)

str(both)

# nrow(C_but_not_O)
# nrow(O_butnot_C)
# nrow(O_and_C)
# nrow(C_but_not_O)+nrow(O_butnot_C)+nrow(O_and_C)

head(both)
str(both)

colnames(both)
both<-select(both,JOURNAL,YEAR,VOLUME,ISSUE,editor_id.x,editor_id.y,
             FIRST_NAME,MIDDLE_NAME.x,MIDDLE_NAME.y,LAST_NAME,
             TITLE,CATEGORY,INST.x,INST.y,UNIT.x,UNIT.y,CITY.x,CITY.y,STATE.x,STATE.y,
             COUNTRY.x,COUNTRY.y,COUNTRY_Prior_Class,geo.code,geo.code_Prior_Class,
             NOTES.x,NOTES.y,GENDER)

colnames(both)
both[both=="missing"]<-NA
both[both=="unknown"]<-NA


# both<-both %>% mutate(VOLUME.x = replace(VOLUME.x, is.na(VOLUME.x),VOLUME.y[is.na(VOLUME.x)]))
# both$VOLUME.y<-NULL
# both<-both %>% rename("VOLUME"="VOLUME.x")


# 
# both<-both %>% mutate(ISSUE.x = replace(ISSUE.x, is.na(ISSUE.x),ISSUE.y[is.na(ISSUE.x)]))
# both$ISSUE.y<-NULL
# both<-both %>% rename("ISSUE"="ISSUE.x")

# 
# both<-both %>% mutate(TITLE.x = replace(TITLE.x, is.na(TITLE.x),TITLE.y[is.na(TITLE.x)]))
# both$TITLE.y<-NULL
# both<-both %>% rename("TITLE"="TITLE.x")
# 



# #CAN QUICKLY ID WHAT NEEDS TO BE FIXED AS FOLLOWS
# str(both)
# # FIRST NAME DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
# summary(both$FIRST_NAME.x==both$FIRST_NAME.y) # 10 false
# both$FIRST_check<-both$FIRST_NAME.x==both$FIRST_NAME.y
# #spelling mistake in NAMES.y, so delete that column and the checm column
# both$FIRST_check<-NULL
# both$FIRST_NAME.y<-NULL
# both<-both %>% rename("FIRST_NAME"="FIRST_NAME.x")

# MIDDLE NAME DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
summary(both$MIDDLE_NAME.x==both$MIDDLE_NAME.y)  # NO FALSE
both$MIDDLE_check<-both$MIDDLE_NAME.x==both$MIDDLE_NAME.y
both$MIDDLE_NAME.y<-NULL
both$MIDDLE_check<-NULL
both<-both %>% rename("MIDDLE_NAME"="MIDDLE_NAME.x")

# LAST NAME IFFERENCES BETWEEN ALL DATA AND CHECKED FILE
# summary(both$LAST_NAME.x==both$LAST_NAME.y) # 21 FALSE
# both$LAST_check<-both$LAST_NAME.x==both$LAST_NAME.y
# # This identifies one mistake in thge original (ORIGINAL_DATA) that needs to be corrected
# str(both)
both$LAST_NAME[both$editor_id.x==1355 & both$FIRST_NAME=="Holmes"]<-"Rolston"
both<-both[!(is.na(both$editor_id.x) & both$FIRST_NAME=="Holmes"),]


# both$LAST_NAME.y<-NULL
# both$LAST_check<-NULL
# both<-both %>% rename("LAST_NAME"="LAST_NAME.x")


# STATE DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
summary(both$STATE.x==both$STATE.y) # 19 FALSE
both$STATE_check<-both$STATE.x==both$STATE.y
both$STATE.x[both$STATE.x=="missing"]<-NA
both$STATE.x[both$YEAR!=1992 & both$FIRST_NAME=="David" & both$LAST_NAME=="Gibson" & both$CITY.x=="Carbondale"]<-"IL"
both$STATE.x[both$YEAR==1992 & both$CITY.x=="Pensacola" & both$LAST_NAME=="Gibson"]<-"FL"
both$STATE.x[both$STATE.x=="England"]<-NA
both$STATE.x[both$LAST_NAME=="Moss"]<-NA
both$STATE.x[both$LAST_NAME=="Usher"]<-NA
both$STATE.x[both$LAST_NAME=="Milner-Gulland"]<-NA
both$INST.x[both$LAST_NAME=="Belovsky" & both$INST.y=="Notre Dame"]<-"university of notre dame"
both$CITY.x[both$LAST_NAME=="Belovsky" & both$INST.y=="Notre Dame"]<-"Notre Dame"
both$COUNTRY.x[both$LAST_NAME=="Belovsky" & both$INST.x=="university of notre dame"]<-"USA"
both$STATE.x[both$LAST_NAME=="Belovsky" & both$INST.x=="Notre Dame University"]<-"IN"
both$STATE.x[both$FIRST_NAME=="James" & both$LAST_NAME=="Carlton"]<-"CT"
both$STATE.x[both$FIRST_NAME=="Jon" & both$LAST_NAME=="Rodriguez"]<-NA
both$STATE.x[both$editor_id.x==455 & both$FIRST_NAME=="Christopher" & both$LAST_NAME=="Frissell"]<-"MT"
both$INST.x[both$editor_id.x==455 & both$FIRST_NAME=="Christopher" & both$LAST_NAME=="Frissell"]<-"university of montana"
both$UNIT.x[both$editor_id.x==455 & both$FIRST_NAME=="Christopher" & both$LAST_NAME=="Frissell"]<-"Flathead Lake Biological Station"
both$CITY.x[both$editor_id.x==455 & both$FIRST_NAME=="Christopher" & both$LAST_NAME=="Frissell"]<-"Polson"
both$CITY.x[both$editor_id.x==1511 & both$LAST_NAME=="Cinner" & both$YEAR>2009 & both$YEAR<2015 ]<-"Townsville"
both$STATE.x[both$editor_id.x==1511 & both$LAST_NAME=="Cinner" & both$YEAR>2009 & both$YEAR<2015 ]<-"Queensland"
both$UNIT.x[both$editor_id.x==1511 & both$LAST_NAME=="Cinner"]<-"ARC Centre of Excellence for Coral Reef Studies"
both$INST.x[both$editor_id.x==1511 & both$LAST_NAME=="Cinner"]<-"james cook university"
both$NOTES.y[both$editor_id.x==1511 & both$LAST_NAME=="Cinner" & both$YEAR==2013]<-"journal front matter has INST=Columbia Univ, but his CV makes no mention of this"

# This will replace all the "NA" in STATE.x (origianlly no info) with the value from STATE.y (Patrick's data collection), if there is one
both<-both %>% mutate(STATE.x = replace(STATE.x, is.na(STATE.x), STATE.y[is.na(STATE.x)]))


both$STATE.y<-NULL
both$STATE_check<-NULL
both<-both %>% rename("STATE"="STATE.x")

# This identifies one mistake in thge original (ORIGINAL_DATA) that needs to be corrected
both$UNIT.x[both$UNIT.x=="Estaci<f3>n Biol<f3>gica de Do<f1>ana"]<-"estacion biologica donana"
both$UNIT.y[both$UNIT.y=="Estaci<f3>n Biol<f3>gica de Do<f1>ana"]<-"estacion biologica donana"
both$UNIT.x<-gsub("Estaci\xf3n Biol\xf3gica de Do\xf1ana","estacion biologica donana",both$UNIT.x)
both$UNIT.x<-gsub("Biologie/Chemie/\x80kologie","biochemical ecology",both$UNIT.x)
both$UNIT.x<-gsub("f\x99r","fur",both$UNIT.x)
both$UNIT.x<-gsub("Ecolog\x90a","Ecologia",both$UNIT.x)

# INST DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
both$INST.x<-as.character(both$INST.x)
both$INST.x<-tolower(both$INST.x)
both$INST.y<-tolower(both$INST.y)
both$UNIT.x<-tolower(both$UNIT.x)
both$UNIT.y<-tolower(both$UNIT.y)


# This will replace all the "NA" and "" in INST.x (origianlly no info) with the value from INST.y (Patrick's data collection), if there is one
both<-both %>% mutate(INST.x = replace(INST.x, is.na(INST.x),INST.y[is.na(INST.x)]))
both <- both %>% mutate(INST.x = replace(INST.x, INST.x == "", NA))
summary(both$INST.x==both$INST.y) # 235 FALSE
both$INST_check<-both$INST.x==both$INST.y

INST_check<-filter(both,INST_check=="FALSE")
INST_check_ok<-filter(both,INST_check==TRUE |is.na(INST_check))
#
# write.csv(INST_check, file="./Data/Patrick_James_Data_Corrections/Complete/INST_corrections_2x.csv", row.names = F) #export it as a csv file


both$INST.x[both$editor_id.x==1248 & both$JOURNAL=="BITR" & both$YEAR==1987 ]<-"new york botanical garden"
both$INST.x[both$editor_id.x==1704 & both$JOURNAL=="JECOL" & both$YEAR==2009 ]<-"university of sheffield"
# both$INST.x[both$INST.x=="Unniversity of Stirling" ]<-"University of Stirling"

both$UNIT.x[both$editor_id.x==1229 & both$INST.x=="university of montana"]<-"savannah river ecology laboratory"
both$INST.x[both$editor_id.x==1229 & both$INST.x=="university of montana"]<-"university of georgia"

both$UNIT.x[both$INST.x=="alterra research institute for the green world" ]<-"alterra research institute for the green world"
both$UNIT.x[both$INST.x=="gatty marine lab (university of saint andrews)" ]<-"gatty marine lab"
both$UNIT.x[both$INST.x=="netherlands institute of ecology; wageningen university and research centre netherlands institute of ecology" ]<-"Netherlands Institute of Ecology"
both$UNIT.x[both$INST.x=="norwich" ]<-"norwich research park industrial biotechnology and bioenergy alliance"
both$UNIT.x[both$INST.x=="scripps institute of oceanography" ]<-"scripps institution of oceanography"
both$UNIT.x[both$INST.x=="scripps institution  of oceanography" ]<-"scripps institution of oceanography"
both$UNIT.x[both$INST.x=="scripps institute of oceanography" ]<-"gatty marine lab"



both$UNIT.x[both$INST.x=="savannah river ecology laboratory" ]<-"savannah river ecology laboratory"
both$INST.x[both$INST.x=="savannah river ecology laboratory" ]<-"university of georgia"
both$UNIT.x[both$INST.y=="university of california santa cruz extension"]<-"ucsc extension"
both$INST.x[both$INST.x=="university of california" & both$INST.y=="university of california davis" ]<-"university of california davis"
both$INST.x[both$INST.x=="university of california" & both$INST.y=="university of california berkeley" ]<-"university of california berkeley"
both$INST.x[both$INST.x=="university of california" & both$INST.y=="university of california riverside" ]<-"university of california riverside"
# both$INST.x[both$editor_id.x==1839 & both$JOURNAL=="CONBIO" ]<-"Venezuelan Institute For Scientific Investigation"
both$INST.x[both$editor_id.x==1218 & both$JOURNAL=="CONBIO" & both$YEAR>1986 & both$YEAR<1992 ]<-"royal botanic gardens kew"
# both$INST.x[both$INST.x=="Museum Natl Hist Nat"]<-"National History Museum Paris"
# both$INST.x[both$INST.x=="University<ca>of<ca>California<ca>Santa<ca>Cruz"]<-"University of California Santa Cruz"
# both$INST.x[both$INST.y=="Uc Santa Cruz"]<-"University of California Santa Cruz"

both$INST.x[both$LAST_NAME=="Streeter" & both$JOURNAL=="JECOL" & both$YEAR==2009 ]<-"university of sussex"
both$INST.x[both$LAST_NAME=="Grover" & both$JOURNAL=="AMNAT"]<-"university of texas arlington"
both$INST.x[both$LAST_NAME=="Noss" & both$JOURNAL=="CONBIO" & both$YEAR==1998 ]<-"conservation biology institute"

both$UNIT.x[both$LAST_NAME=="Dratch" & both$JOURNAL=="CONBIO" & both$INST.x=="national fish and wildlife forensics laboratory" ]<-"national fish and wildlife forensics laboratory"
both$INST.x[both$LAST_NAME=="Dratch" & both$JOURNAL=="CONBIO" & both$INST.x=="national fish and wildlife forensics laboratory" ]<-"us fish and wildlife service"
both$INST.x[both$LAST_NAME=="Daszak" & both$JOURNAL=="CONBIO" & both$INST.x=="university of nevada reno" ]<-"consortium for conservation medicine"

both$UNIT.x[both$LAST_NAME=="Meffe" & both$JOURNAL=="CONBIO" & both$INST.x=="university of montana" ]<-"savanna riverl ecological laboratory"
both$UNIT.x[both$LAST_NAME=="Meffe" & both$JOURNAL=="CONBIO" & both$UNIT.x=="savanna riverl ecological laboratory" ]<-"university of georgia"
both$UNIT.x[both$JOURNAL=="LECO" & both$INST.x=="institute of landscape ecology of slovak academy of sciences" ]<-"institute of landscape ecology"
both$INST.x[both$JOURNAL=="LECO" & both$INST.x=="institute of landscape ecology of slovak academy of sciences" ]<-"slovak academy of sciences"

summary(both$INST.x==both$INST.y)
both$INST_check<-both$INST.x==both$INST.y
both$INST.y<-NULL
both$INST_check<-NULL
both<-both %>% rename("INST"="INST.x")

# CITY DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
summary(both$CITY.x==both$CITY.y) # 52 FALSE
both$CITY_check<-both$CITY.x==both$CITY.y
CITY_check<-filter(both,CITY_check=="FALSE")
CITY_check_ok<-filter(both,CITY_check==TRUE |is.na(CITY_check))
write.csv(CITY_check, file="./Data/Patrick_James_Data_Corrections/Complete/CITY_corrections_2x.csv", row.names = F) #export it as a csv file
both$STATE[both$CITY.x=="New Mexico" & both$CITY.y=="Las Cruces"]<-"NM"
both$CITY.x[both$CITY.x=="New Mexico" & both$CITY.y=="Las Cruces"]<-"Las Cruces"
both$CITY.x[is.na(both$CITY.x) & both$CITY.y=="Las Cruces"]<-"Las Cruces"
both$NOTES.y[both$CITY.x=="Basel" & both$CITY.y=="Lausanne"]<-"2x city"
both$NOTES.y[both$CITY.x=="Brighton" & both$CITY.y=="Toronto"]<-"2x city"
both$NOTES.y[both$CITY.x=="Zurich" & both$CITY.y=="Basel"]<-"2x city"
both$NOTES.y[both$CITY.x=="Canberra" & both$CITY.y=="Lyneham"]<-"2x city"

both$CITY.x[both$CITY.x=="Stanford" & both$CITY.y=="Pacific Grove"]<-"Pacific Grove"
both$CITY.x[both$CITY.x=="Manhattan" & both$CITY.y=="New York"]<-"New York"
both$CITY.x[both$CITY.x=="East Lansging" & both$CITY.y=="East Lansing"]<-"East Lansing"
both$CITY.x[both$CITY.x=="New Yor City" & both$CITY.y=="New York City"]<-"New York"
both$CITY.x[both$CITY.x=="Manhattan" & both$CITY.y=="New York"]<-"New York"
both$CITY.x[both$CITY.x=="Los Angeles" & both$CITY.y=="Malibu"]<-"Malibu"
both$CITY.x[both$CITY.x=="Manhattan" & both$CITY.y=="New York"]<-"New York"
both$CITY.x[both$CITY.x=="Manhattan" & both$CITY.y=="New York"]<-"New York"
both$CITY.x[both$CITY.x=="Sheffield S10 2TN"]<-"Sheffield S10 2TN"

both$UNIT.x[both$CITY.x=="\xcaDepartment of Animal Ecology and Tropical Biology (Zoology III)"]<-"Department of Animal Ecology and Tropical Biology (Zoology III)"
both$CITY.x[both$CITY.x=="\xcaDepartment of Animal Ecology and Tropical Biology (Zoology III)"]<-NA

both$NOTES.y[both$CITY.x=="Aberdeen" & both$CITY.y=="Cragiebuckler"]<-"2x city"
both$CITY.x[both$CITY.x=="Invergowric" & both$CITY.y=="Invergowrie"]<-"Invergowrie"
both$NOTES.y[both$CITY.x=="Brisbane" & both$CITY.y=="St Lucia"]<-"2x city"
both$NOTES.y[both$CITY.x=="London" & both$CITY.y=="Ascot"]<-"2x city"
both$NOTES.y[both$CITY.x=="Williams" & both$CITY.y=="Mystic"]<-"2x city"
both$NOTES.y[both$CITY.x=="Melbourne" & both$CITY.y=="Parkville"]<-"2x city"
both$NOTES.y[both$CITY.x=="New Brunswick" & both$CITY.y=="Polson"]<-"2x city"
both$NOTES.y[both$CITY.x=="London" & both$CITY.y=="Notre Dame"]<-"Notre Dame"
both$NOTES.y[both$CITY.x=="Canberra" & both$CITY.y=="Lyneham"]<-"2x city"
both$NOTES.y[both$CITY.x=="Canberra" & both$CITY.y=="Lyneham"]<-"2x city"
both$NOTES.y[both$CITY.x=="Canberra" & both$CITY.y=="Lyneham"]<-"2x city"

summary(both$CITY.x==both$CITY.y)
both$CITY_check<-both$CITY.x==both$CITY.y
# write.csv(CITY_check, file="./Data/Patrick_James_Data_Corrections/Complete/CITY_corrections_2x.csv", row.names = F) #export it as a csv file

# This will replace all the "NA" in CITY.x (origianlly no info) with the value from CITY.y (Patrick's data collection), if there is one
both<-both %>% mutate(CITY.x = replace(CITY.x, is.na(CITY.x), CITY.y[is.na(CITY.x)]))

both$CITY.y<-NULL
both$CITY_check<-NULL
both<-both %>% rename("CITY"="CITY.x")
# str(both)

# TODO UNIT DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
# both$UNIT.x<-as.character(both$UNIT.x)
# both$UNIT.x<-gsub("",NA,both$UNIT.x)
both$UNIT.x[both$UNIT.x == ""] <- NA
summary(both$UNIT.x==both$UNIT.y) # 7 FALSE
both$UNIT_check<-both$UNIT.x==both$UNIT.y

# No probs, just differences in words ("the, and", etc)
# This will replace all the "NA" in UNIT.x (origianlly no info) with the value from UNIT.y (Patrick's data collection), if there is one
both<-both %>% mutate(UNIT.x = replace(UNIT.x, is.na(UNIT.x), UNIT.y[is.na(UNIT.x)]))

both$UNIT.x[both$UNIT.y=="savannah river ecology laboratory"]<-"savannah river ecology laboratory"
both$UNIT.y<-NULL
both$UNIT_check<-NULL
both<-both %>% rename("UNIT"="UNIT.x")

# TODO COUNTRY DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE

both <- both %>% mutate(COUNTRY.x = replace(COUNTRY.x, COUNTRY.x == "", NA))
both$COUNTRY.x<-as.factor(both$COUNTRY.x)
both$COUNTRY.y<-as.factor(both$COUNTRY.y)
country_levels<-(c(levels(both$COUNTRY.x),levels(both$COUNTRY.y)))
levels(both$COUNTRY.x)<-c(levels(both$COUNTRY.x),country_levels,NA)
levels(both$COUNTRY.y)<-c(levels(both$COUNTRY.y),country_levels,NA)

# This will replace all the "NA" in CITY.x (origianlly no info) with the value from CITY.y (Patrick's data collection), if there is one

levels(both$COUNTRY.x)
str(both$COUNTRY.x)
str(both$COUNTRY.y)

both<-both %>% mutate(COUNTRY.x = replace(COUNTRY.x, is.na(COUNTRY.x), COUNTRY.y[is.na(COUNTRY.x)]))
summary(both$COUNTRY.x==both$COUNTRY.y) # 3552 FALSE
both$country_check<-both$COUNTRY.x==both$COUNTRY.y

country_check<-filter(both,country_check=="FALSE")
# write.csv(country_check, file="./Data/Patrick_James_Data_Corrections/Complete/COUNTRY_corrections_2x.csv", row.names = F) #export it as a csv file


both$FIRST_NAME[both$LAST_NAME=="Weiher" & both$JOURNAL=="PLANTECOL"]<-"Ewan"
both$FIRST_NAME[both$LAST_NAME=="Olsvig-Whittaker" & both$JOURNAL=="PLANTECOL"]<-"D"
both$MIDDLE_NAME[both$LAST_NAME=="Olsvig-Whittaker" & both$JOURNAL=="PLANTECOL"]<-"L"

both$COUNTRY.x[both$LAST_NAME=="Tjoelker" & both$JOURNAL=="NEWPHYT" & both$INST=="texas a & m university"]<-"USA"
both$COUNTRY.x[both$LAST_NAME=="Atkin" & both$JOURNAL=="NEWPHYT" & both$INST=="university of york"]<-"United Kingdom"
both$COUNTRY.x[both$LAST_NAME=="Long" & both$JOURNAL=="JECOL" & both$INST=="university of essex"]<-"United Kingdom"
both$COUNTRY.x[both$LAST_NAME=="Westing" & both$JOURNAL=="CONBIO" & both$INST=="stockholm international peace research institute"]<-"Sweden"
both$CITY[both$LAST_NAME=="Westing" & both$JOURNAL=="CONBIO" & both$INST=="stockholm international peace research institute"]<-"Stockholm"
both$STATE[both$LAST_NAME=="Westing" & both$JOURNAL=="CONBIO" & both$INST=="stockholm international peace research institute"]<-NA
both$COUNTRY.x[both$LAST_NAME=="Westing" & both$JOURNAL=="CONBIO" & both$INST=="westing associates"]<-"USA"
both$COUNTRY.x[both$LAST_NAME=="Belovsky" & both$JOURNAL=="CONBIO" & both$INST=="utah state university"]<-"USA"
both$UNIT[both$LAST_NAME=="Bolker" & both$JOURNAL=="AMNAT" & both$INST=="mcmaster university"]<-NA
both$COUNTRY.x[both$LAST_NAME=="Bolker" & both$JOURNAL=="AMNAT" & both$INST=="mcmaster university"]<-"Canada"

both$UNIT[both$LAST_NAME=="Krivan" & both$JOURNAL=="AMNAT" ]<-"biology centre"
both$INST[both$LAST_NAME=="Krivan" & both$JOURNAL=="AMNAT" ]<-"academy of sciences of the czech republic"
both$CITY[both$LAST_NAME=="Krivan" & both$JOURNAL=="AMNAT" ]<-"Ceske Budejovice"
both$STATE[both$LAST_NAME=="Krivan" & both$JOURNAL=="AMNAT"]<-"South Bohemia"
both$COUNTRY.x[both$LAST_NAME=="Krivan" & both$JOURNAL=="AMNAT"]<-"Czech Republic"

both$NOTES.y[both$COUNTRY.y == "Wales"] <- "Wales"
both$NOTES.y[both$COUNTRY.y == "Scotland"] <- "Scotland"
both$NOTES.y[both$COUNTRY.y == "England"] <- "England"
both$NOTES.y[both$COUNTRY.x == "Wales"] <- "Wales"
both$NOTES.y[both$COUNTRY.x == "Scotland"] <- "Scotland"
both$NOTES.y[both$COUNTRY.x == "England"] <- "England"
both$COUNTRY.x[both$COUNTRY.x == "Wales"] <- "United Kingdom"
both$COUNTRY.x[both$COUNTRY.x == "Scotland"] <- "United Kingdom"
both$COUNTRY.x[both$COUNTRY.x == "England"] <- "United Kingdom"


both$COUNTRY.y<-NULL
both$country_check<-NULL
both<-both %>% rename("COUNTRY"="COUNTRY.x")

# TODO NOTES DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE

str(both$NOTES.x)
both$NOTES <-paste(both$NOTES.x,both$NOTES.y, sep= " / ")
both$NOTES[both$NOTES == "NA / NA"] <- NA
both$NOTES <-gsub(" / NA","",both$NOTES)
both$NOTES <-gsub("NA / ","",both$NOTES)
both$NOTES.x<-NULL
both$NOTES.y<-NULL
both$NOTES[both$INST=="Lyme Regis"]<-"is this ghillean prance unattached?"
both$NOTES[both$INST=="NERI"]<-"no longer exists: https://tethys.pnnl.gov/institution/national-environmental-research-institute-neri"
both$NOTES[both$LAST_NAME=="Boggs"]<-"2x carol boggs has inst colorado but should be stanford"
both$NOTES[both$INST == "biological centre"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "biological institute"] <- "DOUBLE CHECK INST"
# both$NOTES[both$INST == "Bogota"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "disteba university of salento"] <- "DOUBLE CHECK INST -  / disteba universita di lecce"
both$NOTES[both$INST == "haus nr.9"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "james cook university townsville"] <- "DOUBLE CHECK INST"
# both$NOTES[both$INST == "lancaster"] <- "DOUBLE CHECK INST"
# both$NOTES[both$INST == "London"] <- "DOUBLE CHECK INST"
# both$NOTES[both$INST == "Madrid"] <- "DOUBLE CHECK INST"
# both$NOTES[both$INST == "Maine"] <- "DOUBLE CHECK INST"
both$NOTES[is.na(both$INST)] <- "DOUBLE CHECK INST"
# both$NOTES[both$INST == "Royal Botanic Gardens Melbourne University of Melbourne"] <- "DOUBLE CHECK INST"
# both$NOTES[both$INST == "Salzburg"] <- "DOUBLE CHECK INST"
# both$NOTES[both$INST == "Swansea"] <- "DOUBLE CHECK INST"
# both$NOTES[both$INST == "Sydney"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "seidenstzicker& kleiman = smithsonian national zoological park, labandeira: smithsonian national museum of natural history"] <- "DOUBLE CHECK INST"
# both$NOTES[both$INST == "Montpellier"] <- "DOUBLE CHECK INST"
# both$NOTES[both$INST == "DOUBLE CHECK"] <- "DOUBLE CHECK INST"
# both$NOTES[both$INST == "CNRS"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
# both$NOTES[both$INST == "CSIRO"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
# both$NOTES[both$INST == "Smithsonian Institution"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
# both$NOTES[both$INST == "University of Arkansas"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
# both$NOTES[both$INST == "University of British Columbia"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
# both$NOTES[both$INST == "University of California"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
# both$NOTES[both$INST == "University of Exeter"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
# both$NOTES[both$INST == "University of Georgia"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
# both$NOTES[both$INST == "University of Illinois"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
# both$NOTES[both$INST == "University of Massachusetts"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
# both$NOTES[both$INST == "University of Minnesota"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
# both$NOTES[both$INST == "University of South Carolina"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
# both$NOTES[both$INST == "University of Texas"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
# both$NOTES[both$INST == "University of Toronto"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
# both$NOTES[both$INST == "University of Wisconsin"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
# both$NOTES[both$INST == "US Geological Survey"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"



# both<-institution_cleaner(both)


# NO IDEA WHY THIS ISN'T WORKING INSIDE FUNCTION, SO DOING HERE
both$INST<-gsub("ArKansas","Arkansas", both$INST)


# TODO: some error checking of states:
# canada instead of Canada
# British Columbia in USA
# Lower Austria     USA
# Galicia     USA
# England     USA
# Uppland     USA
# ONtario in USA
# Gelderland

both$COUNTRY[both$STATE == "British Columbia" & both$COUNTRY == "USA"] <- "Canada"
both$COUNTRY[both$STATE == "Lower Austria" & both$COUNTRY == "USA"] <- "Canada"
both$COUNTRY[both$STATE == "Galicia" & both$COUNTRY == "USA"] <- "Spain"
both$COUNTRY[both$STATE == "England" & both$COUNTRY == "USA"] <- "United Kingdom"
both$COUNTRY[both$STATE == "Uppland" & both$COUNTRY == "USA"] <- "Sweden"
both$COUNTRY[both$STATE == "Ontario"] <- "Canada"
both$COUNTRY[both$STATE == "Gelderland"] <- "Netherlands"
levels(both$INST)




colnames(both)
both$editor_id.y<-NULL
both<-both %>% rename("editor_id"="editor_id.x")

both<-both[!(is.na(both$JOURNAL) & is.na(both$YEAR)),]

# 
# both<-both %>%
#   group_by(JOURNAL,LAST_NAME,FIRST_NAME) %>%
#   mutate(INST = ifelse((row_number()==1 & is.na(INST)), "missing", INST))
# 
# 
# both<-both %>%
#   group_by(JOURNAL,LAST_NAME,FIRST_NAME) %>%
#   mutate(UNIT = ifelse((row_number()==1 & is.na(UNIT)), "missing", UNIT))
# 
# both<-both %>%
#   group_by(JOURNAL,LAST_NAME,FIRST_NAME) %>%
#   mutate(STATE = ifelse((row_number()==1 & is.na(STATE)), "missing", STATE))
# 
# 
# both<-both %>%
#   group_by(JOURNAL,LAST_NAME,FIRST_NAME) %>%
#   mutate(CITY = ifelse((row_number()==1 & is.na(CITY)), "missing", CITY))
# 
new_row<-both %>% 
  filter(LAST_NAME=="Willis" & both$JOURNAL=="JECOL" & both$YEAR==1994)
new_row$YEAR<-1992
both<-rbind(new_row,both)
both$INST[both$LAST_NAME=="Willis"& both$JOURNAL=="JECOL" &
                (both$YEAR>1990|both$YEAR<2007)]<-"university of sheffield"
both$CITY[both$LAST_NAME=="Willis"& both$JOURNAL=="JECOL" &
            (both$YEAR>1990|both$YEAR<2007)]<-"Sheffield"
both$STATE[both$LAST_NAME=="Willis"& both$JOURNAL=="JECOL" &
            (both$YEAR>1990|both$YEAR<2007)]<-"S Yorkshire"
both$COUNTRY[both$LAST_NAME=="Willis"& both$JOURNAL=="JECOL" &
             (both$YEAR>1990|both$YEAR<2007)]<-"United Kingdom"

both<-both %>% arrange(JOURNAL,LAST_NAME,FIRST_NAME,YEAR)

# colnames(both)
# str(as.data.frame(both))
# str(ALLDATA)
# colnames(ALLDATA)
# colnames(both)==colnames(ALLDATA)





# ADDED EB 11 october 2020
both$FIRST_NAME<-tolower(both$FIRST_NAME)
both$LAST_NAME<-tolower(both$LAST_NAME)
both$MIDDLE_NAME<-tolower(both$MIDDLE_NAME)

both$INST[both$INST=="double check"]<-NA

both$INST[both$LAST_NAME=="charlesworth" & both$YEAR==1991 & both$JOURNAL=="AMNAT"]<-"university of chicago"
both$INST[both$LAST_NAME=="chesson" & both$YEAR==1991 & both$JOURNAL=="AMNAT"]<-"australian national university"
both$INST[both$LAST_NAME=="chesson" & both$YEAR==1995 & both$JOURNAL=="AMNAT"]<-"australian national university"
both$INST[both$LAST_NAME=="duffy" & both$YEAR==2015 & both$JOURNAL=="AMNAT"]<-"georgia institute of technology"
both$INST[both$LAST_NAME=="dworkin" & both$YEAR==2015 & both$JOURNAL=="AMNAT"]<-"mcmaster university"
both$INST[both$LAST_NAME=="frederickson" & both$YEAR==2015 & both$JOURNAL=="AMNAT"]<-"university of toronto"
both$INST[both$LAST_NAME=="fuller" & both$YEAR==2015 & both$JOURNAL=="AMNAT"]<-"university of illinois"
both$INST[both$LAST_NAME=="kirkpatrick" & both$YEAR==1991 & both$JOURNAL=="AMNAT"]<-"university of texas austin"
both$INST[both$LAST_NAME=="leips" & both$YEAR==2015 & both$JOURNAL=="AMNAT"]<-"university of maryland baltimore county"
both$INST[both$LAST_NAME=="meagher" & both$YEAR==1991 & both$JOURNAL=="AMNAT"]<-"university of st andrews"
both$INST[both$LAST_NAME=="mooney" & both$YEAR==1985 & both$JOURNAL=="AMNAT"]<-"stanford university"
both$INST[both$LAST_NAME=="mooney" & both$YEAR==1990 & both$JOURNAL=="AMNAT"]<-"stanford university"
both$INST[both$LAST_NAME=="pagel" & both$YEAR==1997 & both$JOURNAL=="AMNAT"]<-"university of oxford"
both$INST[both$LAST_NAME=="pastor" & both$YEAR==1991 & both$JOURNAL=="AMNAT"]<-"university of minnesota duluth"
both$INST[both$LAST_NAME=="real" & both$YEAR==1991 & both$JOURNAL=="AMNAT"]<-"indiana university"
both$INST[both$LAST_NAME=="real" & both$YEAR==1993 & both$JOURNAL=="AMNAT"]<-"indiana university"
both$INST[both$LAST_NAME=="roth" & both$YEAR==1991 & both$JOURNAL=="AMNAT"]<-"duke university"
both$INST[both$LAST_NAME=="roughgarden" & both$YEAR==1985 & both$JOURNAL=="AMNAT"]<-"stanford university"
both$INST[both$LAST_NAME=="roughgarden" & both$YEAR==1990 & both$JOURNAL=="AMNAT"]<-"stanford university"
both$INST[both$LAST_NAME=="seger" & both$YEAR==1991 & both$JOURNAL=="AMNAT"]<-"university of utah"
both$INST[both$LAST_NAME=="tilma" & both$YEAR==1991 & both$JOURNAL=="AMNAT"]<-"university of minnesota"
both$INST[both$LAST_NAME=="tilma" & both$YEAR==1994 & both$JOURNAL=="AMNAT"]<-"university of minnesota"
both$INST[both$LAST_NAME=="whitlock" & both$YEAR==2005 & both$JOURNAL=="AMNAT"]<-"university of british columbia"
both$INST[both$LAST_NAME=="wu" & both$YEAR==1991 & both$JOURNAL=="AMNAT"]<-"university of chicago"
both$INST[both$LAST_NAME=="foote" & both$YEAR==2004 & both$JOURNAL=="AREES"]<-"university of chicago"
both$INST[both$LAST_NAME=="werner" & both$YEAR==1985 & both$JOURNAL=="AREES"]<-"michigan state university"
both$INST[both$LAST_NAME=="wing" & both$YEAR==1999 & both$JOURNAL=="AREES"]<-"smithsonian national museum of natural history"
both$INST[both$LAST_NAME=="wing" & both$YEAR==2003 & both$JOURNAL=="AREES"]<-"smithsonian national museum of natural history"
both$INST[both$LAST_NAME=="andelman" & both$YEAR==2006 & both$JOURNAL=="BIOCON"]<-"university of california santa barbara"
both$INST[both$LAST_NAME=="bourliere" & both$YEAR==1986 & both$JOURNAL=="BIOCON"]<-"university of paris"
both$INST[both$LAST_NAME=="dirzo" & both$YEAR==1998 & both$JOURNAL=="BIOCON"]<-"northern arizona university"
both$INST[both$LAST_NAME=="duffey" & both$YEAR==1989 & both$JOURNAL=="BIOCON"]<-"unaffiliated"
both$INST[both$LAST_NAME=="duffey" & both$YEAR==2014 & both$JOURNAL=="BIOCON"]<-"unaffiliated"
both$INST[both$LAST_NAME=="hawkins" & both$YEAR==2008 & both$JOURNAL=="BIOCON"]<-"university of southampton"
both$INST[both$LAST_NAME=="hawkins" & both$YEAR==2014 & both$JOURNAL=="BIOCON"]<-"university of southampton"
both$INST[both$LAST_NAME=="hockey" & both$YEAR==1997 & both$JOURNAL=="BIOCON"]<-"university of cape town"
both$INST[both$LAST_NAME=="hockey" & both$YEAR==2007 & both$JOURNAL=="BIOCON"]<-"university of cape town"
both$INST[both$LAST_NAME=="hockey" & both$YEAR==2008 & both$JOURNAL=="BIOCON"]<-"university of cape town"
both$INST[both$LAST_NAME=="hockey" & both$YEAR==2009 & both$JOURNAL=="BIOCON"]<-"university of cape town"
both$INST[both$LAST_NAME=="johnsingh" & both$YEAR==2006 & both$JOURNAL=="BIOCON"]<-"wildlife institute of india"
both$INST[both$LAST_NAME=="krishnaswamy" & both$YEAR==2014 & both$JOURNAL=="BIOCON"]<-"ashoka trust for research in ecology and the environment"
both$INST[both$LAST_NAME=="kuenen" & both$YEAR==1986 & both$JOURNAL=="BIOCON"]<-"university of leiden"
both$INST[both$LAST_NAME=="lomolino" & both$YEAR==1998 & both$JOURNAL=="BIOCON"]<-"university of oklahoma"
both$INST[both$LAST_NAME=="marrs" & both$YEAR==2014 & both$JOURNAL=="BIOCON"]<-"university of liverpool"
both$INST[both$LAST_NAME=="mcnicholl" & both$YEAR==1990 & both$JOURNAL=="BIOCON"]<-"unaffiliated"
both$INST[both$LAST_NAME=="mcnicholl" & both$YEAR==1993 & both$JOURNAL=="BIOCON"]<-"unaffiliated"
both$INST[both$LAST_NAME=="morgan" & both$YEAR==1987 & both$JOURNAL=="BIOCON"]<-"station biologique de la tour du valat"
both$INST[both$LAST_NAME=="morgan" & both$YEAR==2004 & both$JOURNAL=="BIOCON"]<-"station biologique de la tour du valat"
both$INST[both$LAST_NAME=="peterken" & both$YEAR==1998 & both$JOURNAL=="BIOCON"]<-"unaffiliated"
both$INST[both$LAST_NAME=="peterken" & both$YEAR==2009 & both$JOURNAL=="BIOCON"]<-"unaffiliated"
both$INST[both$LAST_NAME=="scott" & both$YEAR==1986 & both$JOURNAL=="BIOCON"]<-"unaffiliated"
both$INST[both$LAST_NAME=="westhoff" & both$YEAR==1985 & both$JOURNAL=="BIOCON"]<-"unaffiliated"
both$INST[both$LAST_NAME=="westhoff" & both$YEAR==1988 & both$JOURNAL=="BIOCON"]<-"unaffiliated"
both$INST[both$LAST_NAME=="anderson" & both$YEAR==1993 & both$JOURNAL=="BITR"]<-"australian national university"
both$INST[both$LAST_NAME=="anderson" & both$YEAR==1997 & both$JOURNAL=="BITR"]<-"australian national university"
both$INST[both$LAST_NAME=="benson" & both$YEAR==1993 & both$JOURNAL=="BITR"]<-"universidade estadual de campinas"
both$INST[both$LAST_NAME=="benson" & both$YEAR==1996 & both$JOURNAL=="BITR"]<-"universidade estadual de campinas"
both$INST[both$LAST_NAME=="berry" & both$YEAR==1996 & both$JOURNAL=="BITR"]<-"missouri botanical garden"
both$INST[both$LAST_NAME=="berry" & both$YEAR==1997 & both$JOURNAL=="BITR"]<-"missouri botanical garden"
both$INST[both$LAST_NAME=="brown" & both$YEAR==1993 & both$JOURNAL=="BITR"]<-"university of illinois urbana champaign"
both$INST[both$LAST_NAME=="brown" & both$YEAR==1996 & both$JOURNAL=="BITR"]<-"university of illinois urbana champaign"
both$INST[both$LAST_NAME=="foster" & both$YEAR==1996 & both$JOURNAL=="BITR"]<-"usgs patuxent wildlife research center"
both$INST[both$LAST_NAME=="fox" & both$YEAR==1993 & both$JOURNAL=="BITR"]<-"curtin university of technology"
both$INST[both$LAST_NAME=="fox" & both$YEAR==1997 & both$JOURNAL=="BITR"]<-"curtin university of technology"
both$INST[both$LAST_NAME=="west-eberhard" & both$YEAR==1993 & both$JOURNAL=="BITR"]<-"smithsonian tropical research institute"
both$INST[both$LAST_NAME=="west-eberhard" & both$YEAR==1996 & both$JOURNAL=="BITR"]<-"smithsonian tropical research institute"
both$INST[both$LAST_NAME=="angert" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of british columbia"
both$INST[both$LAST_NAME=="azevedo" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of houston"
both$INST[both$LAST_NAME=="case" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"kent state university"
both$INST[both$LAST_NAME=="dean" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of minnesota"
both$INST[both$LAST_NAME=="dworkin" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"michigan state university"
both$INST[both$LAST_NAME=="edmands" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of southern california"
both$INST[both$LAST_NAME=="engelstadter" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of queensland"
both$INST[both$LAST_NAME=="evans" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"monash university"
both$INST[both$LAST_NAME=="friedman" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of oxford"
both$INST[both$LAST_NAME=="hadfield" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of edinburgh"
both$INST[both$LAST_NAME=="hahn" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"indiana university"
both$INST[both$LAST_NAME=="hall" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of georgia"
both$INST[both$LAST_NAME=="kisdi" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of helsinki"
both$INST[both$LAST_NAME=="laine" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of helsinki"
both$INST[both$LAST_NAME=="marshall" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"monash university"
both$INST[both$LAST_NAME=="masel" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of arizona"
both$INST[both$LAST_NAME=="mcadam" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of guelph"
both$INST[both$LAST_NAME=="neiman" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of iowa"
both$INST[both$LAST_NAME=="redfield" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of british columbia"
both$INST[both$LAST_NAME=="robosky" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of michigan"
both$INST[both$LAST_NAME=="roze" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"cnrs roscoff biological station"
both$INST[both$LAST_NAME=="rozen" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of leiden"
both$INST[both$LAST_NAME=="sweigart" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of georgia"
both$INST[both$LAST_NAME=="tobias" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"university of oxford"
both$INST[both$LAST_NAME=="valenzuela" & both$YEAR==2015 & both$JOURNAL=="EVOL"]<-"iowa state university"
both$INST[both$LAST_NAME=="bearhop" & both$YEAR==2008 & both$JOURNAL=="JANE"]<-"university of exeter"
both$INST[both$LAST_NAME=="bearhop" & both$YEAR==2014 & both$JOURNAL=="JANE"]<-"university of exeter"
both$INST[both$LAST_NAME=="gurney" & both$YEAR==1999 & both$JOURNAL=="JANE"]<-"university of strathclyde"
both$INST[both$LAST_NAME=="gurney" & both$YEAR==2014 & both$JOURNAL=="JANE"]<-"university of strathclyde"
both$INST[both$LAST_NAME=="hall" & both$YEAR==1999 & both$JOURNAL=="JANE"]<-"flinders university"
both$INST[both$LAST_NAME=="hall" & both$YEAR==2009 & both$JOURNAL=="JANE"]<-"worldfish centre"
both$INST[both$LAST_NAME=="lessells" & both$YEAR==1994 & both$JOURNAL=="JANE"]<-"netherlands institute of ecology"
both$INST[both$LAST_NAME=="lessells" & both$YEAR==2014 & both$JOURNAL=="JANE"]<-"netherlands institute of ecology"
both$INST[both$LAST_NAME=="manly" & both$YEAR==2005 & both$JOURNAL=="JANE"]<-"western ecosystem technology"
both$INST[both$LAST_NAME=="manly" & both$YEAR==2013 & both$JOURNAL=="JANE"]<-"western ecosystem technology"
both$INST[both$LAST_NAME=="may" & both$YEAR==1991 & both$JOURNAL=="JANE"]<-"university of oxford"
both$INST[both$LAST_NAME=="may" & both$YEAR==1996 & both$JOURNAL=="JANE"]<-"university of oxford"
both$INST[both$LAST_NAME=="mccann" & both$YEAR==2005 & both$JOURNAL=="JANE"]<-"university of guelph"
both$INST[both$LAST_NAME=="mccleery" & both$YEAR==2008 & both$JOURNAL=="JANE"]<-"university of oxford"
both$INST[both$LAST_NAME=="mcintyre" & both$YEAR==1985 & both$JOURNAL=="JANE"]<-"university of aberdeen"
both$INST[both$LAST_NAME=="mcintyre" & both$YEAR==1990 & both$JOURNAL=="JANE"]<-"university of aberdeen"
both$INST[both$LAST_NAME=="meiri" & both$YEAR==2011 & both$JOURNAL=="JANE"]<-"tel aviv university"
both$INST[both$LAST_NAME=="meiri" & both$YEAR==2014 & both$JOURNAL=="JANE"]<-"tel aviv university"
both$INST[both$LAST_NAME=="o'gorman" & both$YEAR==2013 & both$JOURNAL=="JANE"]<-"queen mary university of london"
both$INST[both$LAST_NAME=="o'gorman" & both$YEAR==2014 & both$JOURNAL=="JANE"]<-"imperial college london"
both$INST[both$LAST_NAME=="rogers" & both$YEAR==1994 & both$JOURNAL=="JANE"]<-"university of oxford"
both$INST[both$LAST_NAME=="rogers" & both$YEAR==1996 & both$JOURNAL=="JANE"]<-"university of oxford"
both$INST[both$LAST_NAME=="stouffer" & both$YEAR==2013 & both$JOURNAL=="JANE"]<-"university of canterbury"
both$INST[both$LAST_NAME=="stouffer" & both$YEAR==2014 & both$JOURNAL=="JANE"]<-"university of canterbury"
both$INST[both$LAST_NAME=="thorpe" & both$YEAR==1994 & both$JOURNAL=="JANE"]<-"soafd fisheries laboratory"
both$INST[both$LAST_NAME=="thorpe" & both$YEAR==1997 & both$JOURNAL=="JANE"]<-"university of glasgow "
both$INST[both$LAST_NAME=="vanderpol" & both$YEAR==2013 & both$JOURNAL=="JANE"]<-"australian national university"
both$INST[both$LAST_NAME=="vanderpol" & both$YEAR==2014 & both$JOURNAL=="JANE"]<-"australian national university"
both$INST[both$LAST_NAME=="block" & both$YEAR==1985 & both$JOURNAL=="JAPE"]<-"liverpool john moores university"
both$INST[both$LAST_NAME=="block" & both$YEAR==1989 & both$JOURNAL=="JAPE"]<-"liverpool john moores university"
both$INST[both$LAST_NAME=="bullock" & both$YEAR==2000 & both$JOURNAL=="JAPE"]<-"institute of terrestrial ecology"
both$INST[both$LAST_NAME=="bullock" & both$YEAR==2003 & both$JOURNAL=="JAPE"]<-"institute of terrestrial ecology"
both$INST[both$LAST_NAME=="clarke" & both$YEAR==1989 & both$JOURNAL=="JAPE"]<-"institute of terrestrial ecology"
both$INST[both$LAST_NAME=="day" & both$YEAR==1988 & both$JOURNAL=="JAPE"]<-"silsoe research institute"
both$INST[both$LAST_NAME=="day" & both$YEAR==1996 & both$JOURNAL=="JAPE"]<-"silsoe research institute"
both$INST[both$LAST_NAME=="fernande-juricic" & both$YEAR==2010 & both$JOURNAL=="JAPE"]<-"purdue university"
both$INST[both$LAST_NAME=="freckleto" & both$YEAR==2005 & both$JOURNAL=="JAPE"]<-"university of oxford"
both$INST[both$LAST_NAME=="hill" & both$YEAR==1993 & both$JOURNAL=="JAPE"]<-"game conservancy"
both$INST[both$LAST_NAME=="hill" & both$YEAR==1999 & both$JOURNAL=="JAPE"]<-"university of cambridge"
both$INST[both$LAST_NAME=="mason" & both$YEAR==1989 & both$JOURNAL=="JAPE"]<-"university of essex"
both$INST[both$LAST_NAME=="mason" & both$YEAR==1996 & both$JOURNAL=="JAPE"]<-"university of essex"
both$INST[both$LAST_NAME=="mead" & both$YEAR==1985 & both$JOURNAL=="JAPE"]<-"university of reading"
both$INST[both$LAST_NAME=="mead" & both$YEAR==1987 & both$JOURNAL=="JAPE"]<-"university of reading"
both$INST[both$LAST_NAME=="miles" & both$YEAR==1988 & both$JOURNAL=="JAPE"]<-"institute of terrestrial ecology"
both$INST[both$LAST_NAME=="miles" & both$YEAR==1991 & both$JOURNAL=="JAPE"]<-"institute of terrestrial ecology"
both$INST[both$LAST_NAME=="monteith" & both$YEAR==1985 & both$JOURNAL=="JAPE"]<-"university of nottingham"
both$INST[both$LAST_NAME=="monteith" & both$YEAR==1987 & both$JOURNAL=="JAPE"]<-"university of nottingham"
both$INST[both$LAST_NAME=="roberts" & both$YEAR==1985 & both$JOURNAL=="JAPE"]<-"central electricity research laboratories"
both$INST[both$LAST_NAME=="roberts" & both$YEAR==1987 & both$JOURNAL=="JAPE"]<-"central electricity research laboratories"
both$INST[both$LAST_NAME=="rutter" & both$YEAR==1985 & both$JOURNAL=="JAPE"]<-"imperial college london"
both$INST[both$LAST_NAME=="rutter" & both$YEAR==1988 & both$JOURNAL=="JAPE"]<-"imperial college london"
both$INST[both$LAST_NAME=="smith" & both$YEAR==2005 & both$JOURNAL=="JAPE"]<-"central science laboratory"
both$INST[both$LAST_NAME=="smith" & both$YEAR==2006 & both$JOURNAL=="JAPE"]<-"central science laboratory"
both$INST[both$LAST_NAME=="thomas" & both$YEAR==1989 & both$JOURNAL=="JAPE"]<-"afrc institute for grassland and animal production"
both$INST[both$LAST_NAME=="thomas" & both$YEAR==1997 & both$JOURNAL=="JAPE"]<-"afrc institute for grassland and animal production"
both$INST[both$LAST_NAME=="walpole" & both$YEAR==2006 & both$JOURNAL=="JAPE"]<-"fauna and flora international"
both$INST[both$LAST_NAME=="walpole" & both$YEAR==2009 & both$JOURNAL=="JAPE"]<-"fauna and flora international"
both$INST[both$LAST_NAME=="welch" & both$YEAR==1988 & both$JOURNAL=="JAPE"]<-"institute of terrestrial ecology"
both$INST[both$LAST_NAME=="welch" & both$YEAR==1996 & both$JOURNAL=="JAPE"]<-"institute of terrestrial ecology"
both$INST[both$LAST_NAME=="ali" & both$YEAR==2013 & both$JOURNAL=="JBIOG"]<-"university of hong kong"
both$INST[both$LAST_NAME=="ali" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"university of hong kong"
both$INST[both$LAST_NAME=="bryson" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"university of washington"
both$INST[both$LAST_NAME=="carine" & both$YEAR==2010 & both$JOURNAL=="JBIOG"]<-"natural history museum london"
both$INST[both$LAST_NAME=="carine" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"natural history museum london"
both$INST[both$LAST_NAME=="cavers" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"nerc centre for ecology and hydrology"
both$INST[both$LAST_NAME=="chapman" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"nerc centre for ecology and hydrology"
both$INST[both$LAST_NAME=="diniz-filho" & both$YEAR==2005 & both$JOURNAL=="JBIOG"]<-"universidade federal de goias"
both$INST[both$LAST_NAME=="diniz-filho" & both$YEAR==2006 & both$JOURNAL=="JBIOG"]<-"universidade federal de goias"
both$INST[both$LAST_NAME=="emerson" & both$YEAR==2013 & both$JOURNAL=="JBIOG"]<-"csic - ipna"
both$INST[both$LAST_NAME=="emerson" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"csic - ipna"
both$INST[both$LAST_NAME=="gaither" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"durham university"
both$INST[both$LAST_NAME=="gilman" & both$YEAR==2010 & both$JOURNAL=="JBIOG"]<-"auckland university of technology"
both$INST[both$LAST_NAME=="gilman" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"auckland university of technology"
both$INST[both$LAST_NAME=="guilhaumon" & both$YEAR==2013 & both$JOURNAL=="JBIOG"]<-"university of evora"
both$INST[both$LAST_NAME=="guilhaumon" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"universite de montpellier"
both$INST[both$LAST_NAME=="hansen" & both$YEAR==2011 & both$JOURNAL=="JBIOG"]<-"university of kwazulu natal"
both$INST[both$LAST_NAME=="hansen" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"university of zurich"
both$INST[both$LAST_NAME=="harrison" & both$YEAR==1985 & both$JOURNAL=="JBIOG"]<-"university college london"
both$INST[both$LAST_NAME=="harrison" & both$YEAR==2004 & both$JOURNAL=="JBIOG"]<-"university college london"
both$INST[both$LAST_NAME=="higgins" & both$YEAR==2013 & both$JOURNAL=="JBIOG"]<-"goethe university frankfurt"
both$INST[both$LAST_NAME=="higgins" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"university of otago"
both$INST[both$LAST_NAME=="holger" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"university of gottingen"
both$INST[both$LAST_NAME=="kreft" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"university of gottingen"
both$INST[both$LAST_NAME=="holland" & both$YEAR==1986 & both$JOURNAL=="JBIOG"]<-"university of otago"
both$INST[both$LAST_NAME=="holland" & both$YEAR==2003 & both$JOURNAL=="JBIOG"]<-"university of otago"
both$INST[both$LAST_NAME=="jetz" & both$YEAR==2006 & both$JOURNAL=="JBIOG"]<-"university of california san diego"
both$INST[both$LAST_NAME=="jetz" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"yale university"
both$INST[both$LAST_NAME=="katinas" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"museo de la plata"
both$INST[both$LAST_NAME=="kullman" & both$YEAR==1991 & both$JOURNAL=="JBIOG"]<-"university of umea"
both$INST[both$LAST_NAME=="kullman" & both$YEAR==2004 & both$JOURNAL=="JBIOG"]<-"university of umea"
both$INST[both$LAST_NAME=="lei" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"chinese academy of sciences"
both$INST[both$LAST_NAME=="maggs" & both$YEAR==2008 & both$JOURNAL=="JBIOG"]<-"queens university belfast"
both$INST[both$LAST_NAME=="maggs" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"queens university belfast"
both$INST[both$LAST_NAME=="masters" & both$YEAR==2011 & both$JOURNAL=="JBIOG"]<-"university of kwazulu natal"
both$INST[both$LAST_NAME=="masters" & both$YEAR==2012 & both$JOURNAL=="JBIOG"]<-"university of ft hare"
both$INST[both$LAST_NAME=="parmakelis" & both$YEAR==2013 & both$JOURNAL=="JBIOG"]<-"university of athens"
both$INST[both$LAST_NAME=="parmakelis" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"university of athens"
both$INST[both$LAST_NAME=="paulay" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"university of florida"
both$INST[both$LAST_NAME=="phillimore" & both$YEAR==2013 & both$JOURNAL=="JBIOG"]<-"university of edinburgh"
both$INST[both$LAST_NAME=="phillimore" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"university of edinburgh"
both$INST[both$LAST_NAME=="prance" & both$YEAR==2000 & both$JOURNAL=="JBIOG"]<-"royal botanic gardens kew"
both$INST[both$LAST_NAME=="prance" & both$YEAR==2004 & both$JOURNAL=="JBIOG"]<-"royal botanic gardens kew"
both$INST[both$LAST_NAME=="richardson" & both$YEAR==2011 & both$JOURNAL=="JBIOG"]<-"royal botanic garden edinburgh"
both$INST[both$LAST_NAME=="richardson" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"royal botanic garden edinburgh"
both$INST[both$LAST_NAME=="schaefer" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"technical university of munich"
both$INST[both$LAST_NAME=="stearns" & both$YEAR==1985 & both$JOURNAL=="JBIOG"]<-"university of wisconsin"
both$INST[both$LAST_NAME=="stearns" & both$YEAR==1990 & both$JOURNAL=="JBIOG"]<-"university of wisconsin"
both$INST[both$LAST_NAME=="triantis" & both$YEAR==2010 & both$JOURNAL=="JBIOG"]<-"university of azores"
both$INST[both$LAST_NAME=="triantis" & both$YEAR==2014 & both$JOURNAL=="JBIOG"]<-"national & kapodistrian university"
both$INST[both$LAST_NAME=="vanderhammen" & both$YEAR==1985 & both$JOURNAL=="JBIOG"]<-"university of amsterdam"
both$INST[both$LAST_NAME=="vanderhammen" & both$YEAR==1990 & both$JOURNAL=="JBIOG"]<-"university of amsterdam"
both$INST[both$LAST_NAME=="watts" & both$YEAR==1985 & both$JOURNAL=="JBIOG"]<-"university of hull"
both$INST[both$LAST_NAME=="watts" & both$YEAR==1995 & both$JOURNAL=="JBIOG"]<-"university of hull"
both$INST[both$LAST_NAME=="dekroon" & both$YEAR==2002 & both$JOURNAL=="JECOL"]<-"radboud university nijmegen"
both$INST[both$LAST_NAME=="dekroon" & both$YEAR==2004 & both$JOURNAL=="JECOL"]<-"radboud university nijmegen"
both$INST[both$LAST_NAME=="etherington" & both$YEAR==1985 & both$JOURNAL=="JECOL"]<-"cardiff university"
both$INST[both$LAST_NAME=="etherington" & both$YEAR==1990 & both$JOURNAL=="JECOL"]<-"university of wales"
both$INST[both$LAST_NAME=="gray" & both$YEAR==1991 & both$JOURNAL=="JECOL"]<-"cardiff university"
both$INST[both$LAST_NAME=="hopkins" & both$YEAR==1985 & both$JOURNAL=="JECOL"]<-"university of sheffield"
both$INST[both$LAST_NAME=="huntely" & both$YEAR==1987 & both$JOURNAL=="JECOL"]<-"cambridge university"
both$INST[both$LAST_NAME=="huntley" & both$YEAR==1985 & both$JOURNAL=="JECOL"]<-"cambridge university"
both$INST[both$LAST_NAME=="huntley" & both$YEAR==1988 & both$JOURNAL=="JECOL"]<-"cambridge university"
both$INST[both$LAST_NAME=="lack" & both$YEAR==1987 & both$JOURNAL=="JECOL"]<-"swansea university"
both$INST[both$LAST_NAME=="lack" & both$YEAR==1990 & both$JOURNAL=="JECOL"]<-"oxford brookes university"
both$INST[both$LAST_NAME=="long" & both$YEAR==1986 & both$JOURNAL=="JECOL"]<-"university of essex"
both$INST[both$LAST_NAME=="ouborg" & both$YEAR==2003 & both$JOURNAL=="JECOL"]<-"radboud university nijmegen"
both$INST[both$LAST_NAME=="ouborg" & both$YEAR==2004 & both$JOURNAL=="JECOL"]<-"radboud university nijmegen"
both$INST[both$LAST_NAME=="vandermeijden" & both$YEAR==1989 & both$JOURNAL=="JECOL"]<-"leiden university"
both$INST[both$LAST_NAME=="vandermeijden" & both$YEAR==1993 & both$JOURNAL=="JECOL"]<-"leiden university"
both$INST[both$LAST_NAME=="vangroenendael" & both$YEAR==1992 & both$JOURNAL=="JECOL"]<-"wageningen university and research"
both$INST[both$LAST_NAME=="white" & both$YEAR==1986 & both$JOURNAL=="JECOL"]<-"university college dublin"
both$INST[both$LAST_NAME=="albon" & both$YEAR==1995 & both$JOURNAL=="JZOOL"]<-"zoological society of london"
both$INST[both$LAST_NAME=="ayers" & both$YEAR==1985 & both$JOURNAL=="NEWPHYT"]<-"lancaster university"
both$INST[both$LAST_NAME=="ayers" & both$YEAR==2002 & both$JOURNAL=="NEWPHYT"]<-"lancaster university"
both$INST[both$LAST_NAME=="barbour" & both$YEAR==2015 & both$JOURNAL=="NEWPHYT"]<-"university of sydney"
both$INST[both$LAST_NAME=="graham" & both$YEAR==2015 & both$JOURNAL=="NEWPHYT"]<-"university of florida"
both$INST[both$LAST_NAME=="schat" & both$YEAR==2015 & both$JOURNAL=="NEWPHYT"]<-"vrije universiteit amsterdam"
both$INST[both$LAST_NAME=="smith" & both$YEAR==2015 & both$JOURNAL=="NEWPHYT"]<-"university of cambridge"
both$INST[both$LAST_NAME=="stinchcombe" & both$YEAR==2014 & both$JOURNAL=="NEWPHYT"]<-"university of toronto"
both$INST[both$LAST_NAME=="sultan" & both$YEAR==2004 & both$JOURNAL=="NEWPHYT"]<-"wesleyan university"
both$INST[both$LAST_NAME=="sultan" & both$YEAR==2009 & both$JOURNAL=="NEWPHYT"]<-"wesleyan university"
both$INST[both$LAST_NAME=="syrett" & both$YEAR==1985 & both$JOURNAL=="NEWPHYT"]<-"university of wales"
both$INST[both$LAST_NAME=="syrett" & both$YEAR==1989 & both$JOURNAL=="NEWPHYT"]<-"university of wales"
both$INST[both$LAST_NAME=="tjoelker" & both$YEAR==2014 & both$JOURNAL=="NEWPHYT"]<-"western sydney university"
both$INST[both$LAST_NAME=="vamosi" & both$YEAR==2015 & both$JOURNAL=="NEWPHYT"]<-"university of calgary"
both$INST[both$LAST_NAME=="west" & both$YEAR==1994 & both$JOURNAL=="NEWPHYT"]<-"university of cambridge"
both$INST[both$LAST_NAME=="wolfenden" & both$YEAR==1990 & both$JOURNAL=="NEWPHYT"]<-"lancaster university"
both$INST[both$LAST_NAME=="wolfenden" & both$YEAR==1991 & both$JOURNAL=="NEWPHYT"]<-"lancaster university"
both$INST[both$LAST_NAME=="wolfenden" & both$YEAR==1996 & both$JOURNAL=="NEWPHYT"]<-"lancaster university"
both$INST[both$LAST_NAME=="bever" & both$YEAR==2013 & both$JOURNAL=="OECOL"]<-"university of indiana"
both$INST[both$LAST_NAME=="buchmann" & both$YEAR==2004 & both$JOURNAL=="OECOL"]<-"swiss federal institute of technology"
both$INST[both$LAST_NAME=="buchmann" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"swiss federal institute of technology"
both$INST[both$LAST_NAME=="diehl" & both$YEAR==2011 & both$JOURNAL=="OECOL"]<-"university of umea"
both$INST[both$LAST_NAME=="elle" & both$YEAR==2013 & both$JOURNAL=="OECOL"]<-"simon fraser university"
both$INST[both$LAST_NAME=="fiedler" & both$YEAR==2005 & both$JOURNAL=="OECOL"]<-"university of vienna"
both$INST[both$LAST_NAME=="fiedler" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"university of vienna"
both$INST[both$LAST_NAME=="gough" & both$YEAR==2013 & both$JOURNAL=="OECOL"]<-"university of texas arlington"
both$INST[both$LAST_NAME=="gough" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"university of texas arlington"
both$INST[both$LAST_NAME=="heimpel" & both$YEAR==2013 & both$JOURNAL=="OECOL"]<-"university of minnesota"
both$INST[both$LAST_NAME=="heimpel" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"university of minnesota"
both$INST[both$LAST_NAME=="herre" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"smithsonian tropical research institute"
both$INST[both$LAST_NAME=="ibanez" & both$YEAR==2013 & both$JOURNAL=="OECOL"]<-"university of michigan"
both$INST[both$LAST_NAME=="ibanez" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"university of michigan"
both$INST[both$LAST_NAME=="koricheva" & both$YEAR==2006 & both$JOURNAL=="OECOL"]<-"royal holloway university of london"
both$INST[both$LAST_NAME=="koricheva" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"royal holloway university of london"
both$INST[both$LAST_NAME=="korner" & both$YEAR==1992 & both$JOURNAL=="OECOL"]<-"university of basel"
both$INST[both$LAST_NAME=="korner" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"university of basel"
both$INST[both$LAST_NAME=="laaksonen" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"university of turku "
both$INST[both$LAST_NAME=="layman" & both$YEAR==2013 & both$JOURNAL=="OECOL"]<-"florida international university"
both$INST[both$LAST_NAME=="layman" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"florida international university"
both$INST[both$LAST_NAME=="legalliard" & both$YEAR==2012 & both$JOURNAL=="OECOL"]<-"cnrs institut ecologie et environnement"
both$INST[both$LAST_NAME=="legalliard" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"cnrs institut ecologie et environnement"
both$INST[both$LAST_NAME=="lichstein" & both$YEAR==2013 & both$JOURNAL=="OECOL"]<-"university of florida"
both$INST[both$LAST_NAME=="lichstein" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"university of florida"
both$INST[both$LAST_NAME=="lill" & both$YEAR==2012 & both$JOURNAL=="OECOL"]<-"george washington university"
both$INST[both$LAST_NAME=="lill" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"george washington university"
both$INST[both$LAST_NAME=="muller" & both$YEAR==2011 & both$JOURNAL=="OECOL"]<-"university of bielefeld"
both$INST[both$LAST_NAME=="muller" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"university of bielefeld"
both$INST[both$LAST_NAME=="niinemets" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"estonian university of life sciences"
both$INST[both$LAST_NAME=="prinzing" & both$YEAR==2013 & both$JOURNAL=="OECOL"]<-"universite de rennes 1"
both$INST[both$LAST_NAME=="schaller" & both$YEAR==1985 & both$JOURNAL=="OECOL"]<-"university of vienna"
both$INST[both$LAST_NAME=="schaller" & both$YEAR==1986 & both$JOURNAL=="OECOL"]<-"university of vienna"
both$INST[both$LAST_NAME=="shurin" & both$YEAR==2011 & both$JOURNAL=="OECOL"]<-"university of california san diego"
both$INST[both$LAST_NAME=="shurin" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"university of california san diego"
both$INST[both$LAST_NAME=="siemann" & both$YEAR==2012 & both$JOURNAL=="OECOL"]<-"rice university"
both$INST[both$LAST_NAME=="siemann" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"rice university"
both$INST[both$LAST_NAME=="stark" & both$YEAR==2012 & both$JOURNAL=="OECOL"]<-"utah state university"
both$INST[both$LAST_NAME=="stark" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"utah state university"
both$INST[both$LAST_NAME=="stewart" & both$YEAR==1998 & both$JOURNAL=="OECOL"]<-"university of queensland"
both$INST[both$LAST_NAME=="ward" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"university of kansas"
both$INST[both$LAST_NAME=="weisser" & both$YEAR==1985 & both$JOURNAL=="OECOL"]<-"university of innsbruck "
both$INST[both$LAST_NAME=="weisser" & both$YEAR==2014 & both$JOURNAL=="OECOL"]<-"technical university of munich"
both$INST[both$LAST_NAME=="ziegler" & both$YEAR==1985 & both$JOURNAL=="OECOL"]<-"technical university of munich"
both$INST[both$LAST_NAME=="ziegler" & both$YEAR==2007 & both$JOURNAL=="OECOL"]<-"technical university of munich"
both$INST[both$LAST_NAME=="aarsen" & both$YEAR==2007 & both$JOURNAL=="OIKOS"]<-"queens university"
both$INST[both$LAST_NAME=="aarsen" & both$YEAR==2014 & both$JOURNAL=="OIKOS"]<-"queens university"
both$INST[both$LAST_NAME=="abbot" & both$YEAR==2013 & both$JOURNAL=="OIKOS"]<-"lund university"
both$INST[both$LAST_NAME=="abbot" & both$YEAR==2014 & both$JOURNAL=="OIKOS"]<-"lund university"
both$INST[both$LAST_NAME=="andersen" & both$YEAR==1985 & both$JOURNAL=="OIKOS"]<-"geological survey of denmark"
both$INST[both$LAST_NAME=="andersen" & both$YEAR==1989 & both$JOURNAL=="OIKOS"]<-"geological survey of denmark"
both$INST[both$LAST_NAME=="andersson" & both$YEAR==1985 & both$JOURNAL=="OIKOS"]<-"swedish university of agricultural sciences"
both$INST[both$LAST_NAME=="andersson" & both$YEAR==1989 & both$JOURNAL=="OIKOS"]<-"swedish university of agricultural sciences"
both$INST[both$LAST_NAME=="boomsma" & both$YEAR==2011 & both$JOURNAL=="OIKOS"]<-"university of copenhagen"
both$INST[both$LAST_NAME=="dahl" & both$YEAR==1985 & both$JOURNAL=="OIKOS"]<-"agricultural university of norway"
both$INST[both$LAST_NAME=="dahl" & both$YEAR==1989 & both$JOURNAL=="OIKOS"]<-"agricultural university of norway"
both$INST[both$LAST_NAME=="grae" & both$YEAR==2012 & both$JOURNAL=="OIKOS"]<-"norwegian university of science and technology"
both$INST[both$LAST_NAME=="grae" & both$YEAR==2014 & both$JOURNAL=="OIKOS"]<-"norwegian university of science and technology"
both$INST[both$LAST_NAME=="jarvinen" & both$YEAR==1989 & both$JOURNAL=="OIKOS"]<-"university of helsinki"
both$INST[both$LAST_NAME=="malian" & both$YEAR==2012 & both$JOURNAL=="OIKOS"]<-"swiss federal institute of aquatic science and technology"
both$INST[both$LAST_NAME=="malian" & both$YEAR==2014 & both$JOURNAL=="OIKOS"]<-"swiss federal institute of aquatic science and technology"
both$INST[both$LAST_NAME=="moore" & both$YEAR==2011 & both$JOURNAL=="OIKOS"]<-"university of california santa cruz"
both$INST[both$LAST_NAME=="moore" & both$YEAR==2014 & both$JOURNAL=="OIKOS"]<-"university of california santa cruz"
both$INST[both$LAST_NAME=="robinson" & both$YEAR==2011 & both$JOURNAL=="OIKOS"]<-"british trust for ornithology"
both$INST[both$LAST_NAME=="robinson" & both$YEAR==2014 & both$JOURNAL=="OIKOS"]<-"british trust for ornithology"
both$INST[both$LAST_NAME=="roy" & both$YEAR==2012 & both$JOURNAL=="OIKOS"]<-"evergreen state college"
both$INST[both$LAST_NAME=="roy" & both$YEAR==2014 & both$JOURNAL=="OIKOS"]<-"evergreen state college"
both$INST[both$LAST_NAME=="scherer_lorenzon" & both$YEAR==2014 & both$JOURNAL=="OIKOS"]<-"university of freiburg"
both$INST[both$LAST_NAME=="solbrek" & both$YEAR==2011 & both$JOURNAL=="OIKOS"]<-"swedish university of agricultural sciences"
both$INST[both$LAST_NAME=="sun" & both$YEAR==2014 & both$JOURNAL=="OIKOS"]<-"university of hong kong"
both$INST[both$LAST_NAME=="traveser" & both$YEAR==2011 & both$JOURNAL=="OIKOS"]<-"mediterranean institute for advanced studies"
both$INST[both$LAST_NAME=="traveser" & both$YEAR==2014 & both$JOURNAL=="OIKOS"]<-"mediterranean institute for advanced studies"
both$INST[both$LAST_NAME=="bornkamm" & both$YEAR==1985 & both$JOURNAL=="PLANTECOL"]<-"technical university of berlin"
both$INST[both$LAST_NAME=="bornkamm" & both$YEAR==1989 & both$JOURNAL=="PLANTECOL"]<-"technical university of berlin"
both$INST[both$LAST_NAME=="epstein" & both$YEAR==2012 & both$JOURNAL=="PLANTECOL"]<-"university of virginia"
both$INST[both$LAST_NAME=="gimingham" & both$YEAR==1985 & both$JOURNAL=="PLANTECOL"]<-"university of aberdeen"
both$INST[both$LAST_NAME=="gimingham" & both$YEAR==1989 & both$JOURNAL=="PLANTECOL"]<-"university of aberdeen"
both$INST[both$LAST_NAME=="grubb" & both$YEAR==1985 & both$JOURNAL=="PLANTECOL"]<-"university of cambridge"
both$INST[both$LAST_NAME=="grubb" & both$YEAR==1986 & both$JOURNAL=="PLANTECOL"]<-"university of cambridge"
both$INST[both$LAST_NAME=="ne'eman" & both$YEAR==2012 & both$JOURNAL=="PLANTECOL"]<-"university of haifa-oranim"
both$INST[both$LAST_NAME=="oksanen" & both$YEAR==1989 & both$JOURNAL=="PLANTECOL"]<-"university of eastern finland"
both$INST[both$LAST_NAME=="rozema" & both$YEAR==2012 & both$JOURNAL=="PLANTECOL"]<-"vrije universiteit amsterdam"
both$INST[both$LAST_NAME=="walsh" & both$YEAR==2012 & both$JOURNAL=="PLANTECOL"]<-"university of north carolina chapel hill"
both$INST[both$LAST_NAME=="white" & both$YEAR==1985 & both$JOURNAL=="PLANTECOL"]<-"university college dublin"
both$INST[both$LAST_NAME=="white" & both$YEAR==1989 & both$JOURNAL=="PLANTECOL"]<-"university college dublin"
both$INST[both$editor_id==1747 & both$YEAR==2011 & both$JOURNAL=="JBIOG"]<-"university of fort hare"

both$INST[both$editor_id==3892 & both$YEAR==1985]<-"louisiana state university"

both$INST[both$editor_id==716 & both$LAST_NAME=="hansen" & both$JOURNAL=="JBIOG"]<-"university of zurich"
both$COUNTRY[both$editor_id==716 & both$LAST_NAME=="hansen" & both$JOURNAL=="JBIOG"]<-"Switzerland"
both$INST[both$editor_id==1849 & both$INST=="university of stirling"]<-"vrije universiteit amsterdam"
# both$INST[both$editor_id==105 & both$JOURNAL=="AGRONOMY"]<-"california state university fresno"
# both$INST[both$editor_id==1673 & both$YEAR==2014]<-"california state university sacramento"

both$editor_id[both$JOURNAL=="GCB" & both$LAST_NAME=="korner" & both$FIRST_NAME=="christian"]<-499
both$INST[both$editor_id==499 & both$YEAR>1989]<-"university of basel"
both$CITY[both$CITY=="turtu"& both$editor_id==3587]<-"turku"

both$CITY[both$CITY=="universidade estadual paulista"& both$editor_id==2383]<-"Rio Claro"
both$CITY[both$CITY=="universidade estadual paulista"& both$editor_id==1225]<-"Sao Paulo"

both$INST[both$editor_id==2358 & both$JOURNAL=="JANE"]<-"university college cork"
both$CITY[both$editor_id==2358 & both$JOURNAL=="JANE"]<-"Cork"

both$INST[both$editor_id==1417 & both$YEAR=="1992"]<-"usfs pacific southwest research station"

both$INST[both$editor_id==190]<-"kansas state university"

both$INST[both$editor_id==1763]<-"universidade de sao paulo"
both$INST[both$editor_id==2819]<-"universidade de sao paulo"

both$STATE[both$CITY=="Para"]<-"Para"
both$CITY[both$CITY=="Para"]<-NA

both$CITY[both$CITY=="Washington"]<-"Washington DC"
both$CITY[both$CITY=="Washington, DC"]<-"Washington DC"
both$CITY[both$CITY=="DC"]<-"Washington DC"
both$CITY[both$CITY=="District of Columbia"]<-"Washington DC"
both$STATE[both$CITY=="Washington DC"]<-"Washington DC"

both$CITY[both$CITY=="St louis"]<-"St Louis"

both$CITY[both$CITY=="Fredricton"]<-"Fredericton"


both$COUNTRY[both$editor_id==3359 & both$YEAR==2014]<-"New Zealand"


both$COUNTRY[both$COUNTRY=="West Germany"]<-"Germany"


colnames(both)
both<-both %>% group_by(JOURNAL,editor_id,LAST_NAME,FIRST_NAME) %>% 
  arrange(YEAR) %>% 
  fill(INST,.direction="down")


both$LAST_NAME[both$LAST_NAME=="aarsen" & both$editor_id=="2119"]<-"aarssen"
both$LAST_NAME[both$LAST_NAME=="tilma" & both$editor_id=="891"]<-"tilman"
both$FIRST_NAME[both$editor_id=="570"]<-"christer"
both$LAST_NAME[both$editor_id=="570"]<-"solbreck"
both$UNIT[both$editor_id=="1605"]<-"Citrus Res & Educ Ctr"
both$LAST_NAME[both$editor_id=="2760"]<-"ayres"
both$LAST_NAME[both$LAST_NAME=="freckleto" & both$JOURNAL=="JAPE"]<-"freckelton"
both$editor_id[both$LAST_NAME=="freckleto" & both$JOURNAL=="JAPE"]<-"3063"
both$FIRST_NAME[both$LAST_NAME=="boomsma" & both$JOURNAL=="OIKOS"]<-"jacobus"

both$FIRST_NAME[both$LAST_NAME=="ouborg" & both$JOURNAL=="JECOL"]<-"n"
both$MIDDLE_NAME[both$LAST_NAME=="ouborg" & both$JOURNAL=="JECOL"]<-"joop"
both$NOTES[both$LAST_NAME=="block" & both$editor_id=="3714"]<-"from 1993 record"
both$LAST_NAME[both$editor_id=="283"]<-"graae"
both$FIRST_NAME[both$LAST_NAME=="ward" & both$JOURNAL=="OECOL"]<-"joy"
both$editor_id[both$LAST_NAME=="ward" & both$JOURNAL=="OECOL"]<-"1938"
both$CITY[both$LAST_NAME=="ward" & both$JOURNAL=="OECOL"]<-"lawrence"
both$LAST_NAME[both$editor_id=="283"]<-"graae"
both$FIRST_NAME[both$LAST_NAME=="ward" & both$JOURNAL=="OECOL"]<-"joy"

both$LAST_NAME[both$editor_id=="564"]<-"leroy"
both$FIRST_NAME[both$editor_id=="564"]<-"carri"
both$MIDDLE_NAME[both$editor_id=="564"]<-NA
both$LAST_NAME[both$editor_id=="2552"]<-"scherer-lorenzen"
both$LAST_NAME[both$editor_id=="519"]<-"melian"
both$FIRST_NAME[both$editor_id=="1323"]<-"holger"
both$LAST_NAME[both$editor_id=="1323"]<-"kreft"
both$LAST_NAME[both$editor_id=="217"]<-"traveset"
both$NOTES[both$editor_id=="102"& both$JOURNAL=="OECOL"]<-"city listed as washington DC in frontmatter"
both$FIRST_NAME[both$editor_id=="2672" & both$JOURNAL=="BIOCON"]<-"n"
both$MIDDLE_NAME[both$editor_id=="2672" & both$JOURNAL=="BIOCON"]<-"c"

both$FIRST_NAME[both$editor_id=="753" & both$JOURNAL=="BIOCON"]<-"d"
both$MIDDLE_NAME[both$editor_id=="753" & both$JOURNAL=="BIOCON"]<-"j"

both$LAST_NAME[both$editor_id=="2154" & both$JOURNAL=="JBIOG"]<-"gillman" 
both$MIDDLE_NAME[both$FIRST_NAME == "par" & both$LAST_NAME=="hockey"]<-"ar" 
both$FIRST_NAME[both$FIRST_NAME == "par" & both$LAST_NAME=="hockey"]<-"p"

both$INST[(both$editor_id=="3252" |both$editor_id=="453"|both$editor_id=="1973") & both$INST=="queensland"]<-"university of queensland"
both$INST[both$LAST_NAME=="vellend" & both$JOURNAL=="OIKOS"]<-"cornell university"
both$INST[both$INST=="wyoming" & both$LAST_NAME=="benkman"]<-"university of wyoming"
both$INST[both$JOURNAL=="FUNECOL" & both$LAST_NAME=="blanckenhorn"]<-"university of zurich irchel"
both$CITY[both$LAST_NAME=="hixon" & both$INST=="university of hawaii"]<-"honolulu"
both$INST[both$INST=="zurich" & both$LAST_NAME=="tschirren"]<-"university of zurich"
both$INST[both$INST=="vermont" & both$LAST_NAME=="brody"]<-"university of vermont"
both$INST[both$INST=="utah" & both$LAST_NAME=="ehleringer"]<-"university of utah"
both$INST[both$INST=="utah" & both$LAST_NAME=="caldwell"]<-"utah state university"
both$INST[both$LAST_NAME=="herrera" & both$INST=="csic consejo superior de investigaciones cientificas"]<-"csic donana biological station"
both$INST[both$LAST_NAME=="herrera" & both$FIRST_NAME=="carlos"]<-"csic donana biological station"
both$INST[both$LAST_NAME=="herrera" & both$FIRST_NAME=="carlos"]<-"csic donana biological station"
both$INST[both$LAST_NAME=="jordano"]<-"csic donana biological station"
both$UNIT[both$LAST_NAME=="jordano"& both$INST=="csic donana biological station"]<-"csic donana biological station"

both$COUNTRY[both$LAST_NAME=="laaksonen" & both$JOURNAL=="OECOL"]<-"Finland"
both$INST[both$LAST_NAME=="angilletta" & both$INST=="indiana"]<-"indiana state university"
both$INST[both$LAST_NAME=="reynolds" & both$INST=="indiana"]<-"indiana university bloomington"
both$INST[both$LAST_NAME=="pienkowski" & both$COUNTRY=="United Kingdom"]<-"joint nature conservation committee"

both$INST[both$LAST_NAME=="cavers" & both$INST=="nerc centre for ecology and hydrology"]<-"nerc centre for ecology and hydrology edinburgh"
both$INST[both$LAST_NAME=="chapman" & both$INST=="nerc centre for ecology and hydrology"]<-"nerc centre for ecology and hydrology edinburgh"
both$INST[both$CITY=="Wallingford" & both$INST=="nerc centre for ecology and hydrology"]<-"nerc centre for ecology and hydrology wallingford"
both$INST[both$CITY=="Bailrigg" & both$INST=="nerc centre for ecology and hydrology"]<-"nerc centre for ecology and hydrology bailrigg"


both$INST[both$LAST_NAME=="croxall"]<-"nerc british antarctic survey"
both$INST[both$LAST_NAME=="pywell" & both$UNIT=="Center for Ecology and Hydrology"]<-"nerc centre for ecology and hydrology wallingford"


both$INST[both$LAST_NAME=="harwood" & both$INST=="nerc natural environment research council"]<-"nerc sea mammal research unit"

both$INST[both$LAST_NAME=="montevecchi"]<-"memorial university of newfoundland"
both$INST[both$editor_id==3772]<-"memorial university of newfoundland"

both$INST[both$editor_id==1158]<-"university of georgia caes griffin campus"
both$UNIT[both$editor_id==1158]<-"caes griffin campus-ag experiment station"


both$INST[both$editor_id==1410]<-"universite montpellier 2"

both$INST[both$LAST_NAME=="hauber" & both$YEAR==2017]<-"cuny hunter college"

both$INST[both$editor_id==1972 & both$YEAR==2004]<-"kansas state university"
both$LAST_NAME[both$editor_id==1972 & both$YEAR==2004]<-"with"
both$MIDDLE_NAME[both$editor_id==1972 & both$YEAR==2004]<-"a"




both$LAST_NAME[both$LAST_NAME=="van der maarel" & both$JOURNAL=="LANDSCAPEECO"]<-"vandermaarel"
both$LAST_NAME[both$LAST_NAME=="vander" & both$JOURNAL=="LANDSCAPEECO"]<-"vandermaarel"
both$INST[both$LAST_NAME=="vandermaarel" & both$JOURNAL=="LANDSCAPEECO"]<-"university of uppsala"
	
both$INST[both$LAST_NAME=="cymerman" & both$JOURNAL=="LANDSCAPEECO"]<-"university of georgia athens"
both$LAST_NAME[both$LAST_NAME=="cymerman" & both$JOURNAL=="LANDSCAPEECO"]<-"hepinstall-cymerman"


both$INST[both$editor_id==381 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2015]<-"leibniz university hannover"
both$INST[both$editor_id==1480 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2004]<-"roskilde university"
both$INST[both$editor_id==2316 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2015]<-"swiss federal institute for forest snow and landscape research wsl"
both$INST[both$editor_id==1990 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2015]<-"michigan state university"
both$INST[both$editor_id==1520 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2015]<-"north carolina state university"
both$INST[both$editor_id==448 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2004]<-"usfs rocky mountain research station"
both$INST[both$editor_id==3067 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==1997]<-"appalachian environmental laboratory"
both$INST[both$editor_id==2387 & both$JOURNAL=="LANDSCAPEECO" & (both$YEAR>=1987 & both$YEAR<=1992)]<-"institut botanique"
# both$INST[both$editor_id==2387 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==1992]<-"institut botanique"
both$INST[both$editor_id==1192 & both$JOURNAL=="LANDSCAPEECO" & (both$YEAR>=1987 & both$YEAR<=1996)]<-"institute geography and geoecology"
# both$INST[both$editor_id==1192 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==1996]<-"institute geography and geoecology"
both$INST[both$editor_id==3744 & both$JOURNAL=="LANDSCAPEECO" & (both$YEAR>=1987 & both$YEAR<=1997)]<-"technical university of munich"
# both$INST[both$editor_id==3744 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==1997]<-"technical university of munich"
both$INST[both$editor_id==1080 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2015]<-"agroscope"
both$COUNTRY[both$editor_id==1080 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2015]<-"Netherlands"
both$INST[both$editor_id==492 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==1997]<-"south dakota state university"
both$INST[both$editor_id==1089 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2004]<-"swiss federal institute for forest snow and landscape research wsl"
both$INST[both$editor_id==3126 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2015]<-"university of bari"
both$INST[both$editor_id==3592 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2015]<-"university of richmond"
both$LAST_NAME[both$editor_id==3592 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2015]<-"lookingbill"
both$INST[both$editor_id==2674 & both$JOURNAL=="LANDSCAPEECO" & (both$YEAR>=1993 & both$YEAR<=1997)]<-"hiroshima university"
# both$INST[both$editor_id==2674 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==1997]<-"hiroshima university"
both$INST[both$editor_id==1781 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2004]<-"university of michigan"
both$INST[both$editor_id==2885 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==1993]<-"Institute for Forestry and Nature Research"
both$INST[both$editor_id==2885 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==1997]<-"Institute for Forestry and Nature Research"
both$INST[both$editor_id==2885 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2004]<-"alterra research institute for the green world"
both$INST[both$editor_id==177 & both$JOURNAL=="LANDSCAPEECO" & (both$YEAR>=1987 & both$YEAR<=1997)]<-"ets ingenieros de montes"
# both$INST[both$editor_id==177 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==1997]<-"ets ingenieros de montes"
both$INST[both$editor_id==2907 & both$JOURNAL=="LANDSCAPEECO" & (both$YEAR>=1987 & both$YEAR<=1992)]<-"university of new mexico"
# both$INST[both$editor_id==2907 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==1992]<-"university of new mexico"
both$INST[both$editor_id==3791 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2004]<-"colorado state university"
both$INST[both$editor_id==2546 & both$JOURNAL=="LANDSCAPEECO" & (both$YEAR>=1987 & both$YEAR<=1997)]<-"center of biological and ecological sciences"
# both$INST[both$editor_id==2546 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==1997]<-"center of biological and ecological sciences"
both$INST[both$editor_id==571 & both$JOURNAL=="LANDSCAPEECO" & (both$YEAR>=1987 & both$YEAR<=1992)]<-"harvard university"
# both$INST[both$editor_id==571 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==1992]<-"harvard university"
both$INST[both$editor_id==1906 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2015]<-"harvard university"
both$INST[both$editor_id==371 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2013]<-"arizona state university"
both$INST[both$editor_id==2580 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2004]<-"university of wisconsin madison"
both$INST[both$editor_id==3912 & both$JOURNAL=="LANDSCAPEECO" & (both$YEAR>=1993 & both$YEAR<=1997)]<-"oregon state university"
# both$INST[both$editor_id==3912 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==1997]<-"oregon state university"
both$INST[both$editor_id==602 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2015]<-"university of wolverhampton"
both$INST[both$editor_id==3824 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2015]<-"chinese academy of sciences"
both$INST[both$editor_id==1428 & both$JOURNAL=="LANDSCAPEECO" & (both$YEAR>=1987 & both$YEAR<=1992)]<-"international institute for aerospace survey and earth sciences"
# both$INST[both$editor_id==1428 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==1992]<-"international institute for aerospace survey and earth sciences"
both$INST[both$editor_id==1045 & both$JOURNAL=="LANDSCAPEECO" & (both$YEAR>=1987 & both$YEAR<=1992)]<-"university of arizona"
# both$INST[both$editor_id==1045 & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==1992]<-"university of arizona"
both$INST[both$editor_id==3669 & both$JOURNAL=="OECOL" & both$YEAR==2013]<-"estonian university of life sciences"









both$INST[both$editor_id==898 & both$INST=="conicet consejo nacional de investigaciones cientificas y tecnicas"]<-"conicet cct mendoza"
both$INST[both$LAST_NAME=="areta" & both$INST=="conicet consejo nacional de investigaciones cientificas y tecnicas"]<-"conicet ibigeo"
both$INST[both$editor_id==2340 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro sustainable ecosystems"
both$INST[both$editor_id==1693 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro forest research"
both$INST[both$editor_id==196 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro entomology"
both$INST[both$LAST_NAME=="thrall" & both$YEAR==2010]<-"csiro plant industry"
both$INST[both$LAST_NAME=="rothstein" & both$INST=="university of california"]<-"university of california santa barbara"
both$INST[both$LAST_NAME=="powell" & both$INST=="university of alaska"]<-"university of alaska fairbanks"
both$INST[both$editor_id==2281 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro land and water"
# both$INST[both$editor_id==2281 & both$JOURNAL=="JECOL" & (both$YEAR>1987 & both$YEAR<1994)]<-"csiro land and water"
both$INST[both$editor_id==2281 & both$UNIT=="division of wildlife and ecology" & both$CITY=="lyneham"]<-"csiro wildlife and ecology"
both$INST[both$editor_id==2281 & both$CITY=="lyneham"]<-"csiro wildlife and ecology"
both$INST[both$LAST_NAME=="austin" & both$CITY=="lyneham"]<-"csiro wildlife and ecology"
both$INST[both$LAST_NAME=="austin" & both$JOURNAL=="JECOL"& (both$YEAR>1987 & both$YEAR<1994)]<-"csiro wildlife and ecology"
# both$INST[both$editor_id==2281 & (both$YEAR==1988 | both$YEAR==1989) & both$JOURNAL=="JECOL"]<-"csiro wildlife and ecology"
both$INST[both$editor_id==2868 & both$INST=="csiro commonwealth scientific and industrial research organisation" & both$CITY=="Canberra"]<-"csiro land and water"
both$INST[both$editor_id==2281 & both$CITY=="Canberra"]<-"csiro land and water"
both$INST[both$editor_id==2281 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro land and water"
both$INST[both$editor_id==3598 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro sustainable ecosystems"
both$INST[both$editor_id==392 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro forest research"
both$INST[both$editor_id==1160 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro wildlife and ecology"
both$INST[both$editor_id==3091 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro wildlife and ecology"
both$INST[both$editor_id==911 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro sustainable ecosystems"
both$INST[both$editor_id==1094 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro ecosystem sciences"
both$INST[both$LAST_NAME=="doerr" & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro ecosystem sciences"
both$INST[both$editor_id==1493 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro forest research"
both$INST[both$editor_id==2867 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"university of adelaide"
both$INST[both$editor_id==3091 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro wildlife and ecology"
both$INST[both$editor_id==2340 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro computing research"
both$INST[both$editor_id==2699 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro plant industry"
both$INST[both$LAST_NAME=="thrall" & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro plant industry"
both$INST[both$editor_id==342 & both$INST=="james cook university"]<-"james cook university brisbane"
both$INST[both$editor_id==1561 & both$INST=="james cook university"]<-"james cook university townsville"
both$INST[both$editor_id==3027 & both$INST=="james cook university"]<-"james cook university townsville"
both$INST[both$LAST_NAME=="crozier" & both$INST=="james cook university"]<-"james cook university townsville"
both$INST[both$editor_id==3305 & both$INST=="james cook university"]<-"james cook university townsville"
both$INST[both$editor_id==2745 & both$INST=="james cook university"]<-"james cook university brisbane"
both$INST[both$editor_id==621 & both$INST=="james cook university"]<-"james cook university townsville"
both$INST[both$editor_id==2971 & both$INST=="james cook university"]<-"james cook university townsville"
both$INST[both$editor_id==3857 & both$INST=="james cook university"]<-"james cook university townsville"
both$INST[both$editor_id==2095 & both$INST=="north carolina"]<-"university of north carolina chapel hill"
both$INST[both$LAST_NAME=="petit" & both$INST=="smithsonian institution"]<-"smithsonian national zoological park"
both$INST[both$LAST_NAME=="fleischer" & both$INST=="smithsonian institution"]<-"smithsonian national zoological park"
both$INST[both$editor_id==323 & both$INST=="smithsonian institute"]<-"smithsonian institution national museum of natural history"
both$INST[both$editor_id==3593 & both$INST=="smithsonian institute"]<-"smithsonian institution"
both$INST[both$LAST_NAME=="sedinger" & both$INST=="university of alaska"]<-"university of alaska fairbanks"
both$INST[both$editor_id==3207 & both$INST=="university of alaska"]<-"university of alaska fairbanks"
both$INST[both$editor_id==1062 & both$INST=="university of alaska"]<-"university of alaska fairbanks"
both$INST[both$editor_id==1062 & both$INST=="university of california"]<-"university of california berkeley"
both$INST[both$editor_id==1561 & both$INST=="university of california"]<-"university of california santa barbara"
both$INST[both$editor_id==1871 & both$INST=="university of california"]<-"university of california los angeles"
both$INST[both$editor_id==1886 & both$INST=="university of california"]<-"university of california davis"
both$INST[both$editor_id==3330 & both$INST=="university of california"]<-"university of california irvine"
both$INST[both$editor_id==3333 & both$INST=="university of california"]<-"university of california santa barbara"
both$INST[both$editor_id==3444 & both$INST=="university of california"]<-"university of california santa barbara"
both$INST[both$editor_id==3786 & both$INST=="university of california"]<-"university of california santa barbara"
both$INST[both$editor_id==3804 & both$INST=="university of california"]<-"university of california berkeley"
both$INST[both$LAST_NAME=="eadie" & both$INST=="university of california"]<-"university of california davis"
both$INST[both$editor_id==566 & both$INST=="university of california"]<-"university of california davis"
both$INST[both$editor_id==1655 & both$INST=="university of california"]<-"university of california danr"
both$INST[both$editor_id==1220 & both$INST=="university of california"]<-"university of california los angeles"
both$INST[both$editor_id==2058 & both$INST=="university of california"]<-"university of california los angeles"
both$INST[both$editor_id==2525 & both$INST=="university of california"]<-"university of california riverside"
both$INST[both$editor_id==2411 & both$INST=="university of hawaii"]<-"university of hawaii manoa"
both$INST[both$editor_id==672 & both$INST=="university of hawaii"]<-"university of hawaii manoa"
both$INST[both$editor_id==2097 & both$INST=="university of massachusetts"]<-"university of massachusetts amherst"
both$INST[both$LAST_NAME=="kroodsma" & both$INST=="university of massachusetts"]<-"university of massachusetts amherst"
both$INST[both$LAST_NAME=="byers" & both$INST=="university of massachusetts"]<-"university of massachusetts amherst"
both$INST[both$editor_id==1435 & both$INST=="university of massachusetts"]<-"university of massachusetts amherst"
both$INST[both$editor_id==1025 & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==1799 & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==2035 & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==3218 & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==972 & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==891 & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==3360 & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$LAST_NAME=="arnold" & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==1817 & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==48 & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==936 & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==1506 & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==52 & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==792 & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==1781 & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==1200 & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==2872 & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==2905 & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$LAST_NAME=="klicka" & both$INST=="university of nevada"]<-"university of nevada las vegas"
both$INST[both$editor_id==2253 & both$INST=="university of nevada"]<-"university of nevada las vegas"
both$INST[both$editor_id==2352 & both$INST=="university of north carolina"]<-"university of north carolina chapel hill"
both$INST[both$editor_id==820 & both$INST=="university of north carolina"]<-"university of north carolina chapel hill"
both$INST[both$editor_id==3200 & both$INST=="university of south carolina"]<-"university of south carolina columbia"
both$INST[both$editor_id==1749 & both$INST=="university of texas"]<-"university of texas austin"
both$INST[both$editor_id==3137 & both$INST=="university of texas"]<-"university of texas austin"
both$INST[both$editor_id==3535 & both$INST=="university of texas"]<-"university of texas austin"
both$INST[both$editor_id==723 & both$INST=="university of texas"]<-"university of texas austin"
both$INST[both$editor_id==1691 & both$INST=="university of texas"]<-"university of texas san antonio"
both$INST[both$editor_id==3229 & both$INST=="university of washington"]<-"university of washington seattle"
both$INST[both$editor_id==3179 & both$INST=="university of washington"]<-"university of washington seattle"
both$INST[both$editor_id==3487 & both$INST=="university of washington"]<-"university of washington seattle"
both$INST[both$editor_id==858 & both$INST=="university of washington"]<-"university of washington seattle"
both$INST[both$editor_id==3183 & both$INST=="university of washington"]<-"university of washington seattle"
both$INST[both$editor_id==3001 & both$INST=="university of washington"]<-"university of washington seattle"
both$INST[both$editor_id==1213 & both$INST=="university of washington"]<-"university of washington seattle"
both$INST[both$editor_id==1668 & both$INST=="university of washington"]<-"university of washington seattle"
both$INST[both$editor_id==590 & both$INST=="usda us department of agriculture"]<-"usda ars"
both$INST[both$editor_id==1799 & both$INST=="usfs us forest service"]<-"usfs research and development"
both$INST[both$editor_id==467 & both$INST=="usfs us forest service"]<-"usfs southern research station"
both$INST[both$editor_id==1327 & both$INST=="usfs us forest service"]<-"usfs southern research station"
both$INST[both$editor_id==446 & both$INST=="usfs us forest service"]<-"usfs pacific southwest research station"
both$INST[both$editor_id==3341 & both$INST=="usfs us forest service"]<-"usfs pacific southwest research station"
both$INST[both$editor_id==2178 & both$INST=="usfs us forest service"]<-"usfs northern research station"
both$INST[both$editor_id==2170 & both$INST=="usfs us forest service"]<-"usfs northern research station"
both$INST[both$editor_id==724 & both$INST=="usfs us forest service"]<-"usfs northern research station"
both$INST[both$editor_id==798 & both$INST=="usfs us forest service"]<-"usfs rocky mountain research station"
both$INST[both$editor_id==3310 & both$INST=="usfs us forest service"]<-"usfs rocky mountain research station"
both$INST[both$editor_id==1783 & both$INST=="usfs us forest service"]<-"usfs rocky mountain research station"
both$INST[both$editor_id==448 & both$INST=="usfs us forest service"]<-"usfs rocky mountain research station"
both$INST[both$editor_id==2613 & both$INST=="usfs us forest service"]<-"usfs pacific northwest research station"
both$INST[both$editor_id==2555 & both$INST=="usfs us forest service"]<-"usfs rocky mountain research station"
both$INST[both$editor_id==3310 & both$INST=="usfs us forest service"]<-"usfs rocky mountain research station"
both$INST[both$editor_id==135 & both$INST=="usfs us forest service"]<-"usfs northern research station"
both$INST[both$editor_id==448 & both$INST=="usfs us forest service"]<-"usfs rocky mountain research station"
both$INST[both$LAST_NAME=="monahan" & both$INST=="usfs us forest service"]<-"usfs rocky mountain research station"
both$INST[both$editor_id==766 & both$INST=="usfs us forest service"]<-"usfs northern research station"
both$INST[both$editor_id==1566 & both$INST=="usfs us forest service"]<-"usfs institute of pacific islands dorestry"
both$INST[both$editor_id==1107 & both$INST=="usfs us forest service"]<-"usfs pacific northwest research station"
both$INST[both$editor_id==2178 & both$INST=="usfs us forest service"]<-"usfs northern research station"
both$INST[both$editor_id==2075 & both$INST=="usfs us forest service"]<-"usfs southern research station"
both$INST[both$editor_id==671 & both$INST=="usfs us forest service"]<-"usfs northern research station"
both$INST[both$editor_id==975 & both$INST=="usfs us forest service"]<-"usfs northern research station"
both$INST[both$editor_id==1127 & both$INST=="us forest serv"]<-"usfs international institute of tropical forestry"
both$INST[both$editor_id==139 & both$INST=="usfs us forest service"]<-"usfs international institute of tropical forestry"
both$INST[both$editor_id==909 & both$INST=="usfs us forest service"]<-"usfs pacific southwest research station"
both$INST[both$editor_id==139 & both$INST=="usfs us forest service"]<-"usfs international institute of tropical forestry"
both$INST[both$editor_id==1580 & both$INST=="usfs us forest service"]<-"usfs pacific northwest research station"
both$INST[both$editor_id==341 & both$INST=="usfs us forest service"]<-"usfs northern research station"
both$INST[both$editor_id==1255 & both$INST=="usfs us forest service"]<-"usfs southern research station"
both$INST[both$editor_id==1909 & both$INST=="usfs us forest service"]<-"usfs southern research station"
both$INST[both$editor_id==3383 & both$INST=="usfs us forest service"]<-"usfs southern research station"
both$INST[both$editor_id==1604 & both$INST=="usgs united states geological survey"]<-"usgs national wetlands research center"
both$INST[both$editor_id==1786 & both$INST=="usgs united states geological survey"]<-"usgs patuxent wildlife research center"
both$INST[both$editor_id==663 & both$INST=="usgs united states geological survey"]<-"usgs national wetlands research center"
both$INST[both$editor_id==663 & both$JOURNAL=="AMNAT" & (both$YEAR>2005  &  both$YEAR<2010)]<-"usgs national wetlands research center"
both$INST[both$editor_id==1604 & both$INST=="usgs united states geological survey"]<-"usgs national wetlands research center"
both$INST[both$LAST_NAME=="piatt" & both$INST=="usgs united states geological survey"]<-"usgs alaska science center"
both$INST[both$LAST_NAME=="weathers" & both$INST=="university of california"]<-"university of california davis"
both$INST[both$LAST_NAME=="gutierrez" & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==1854 & both$INST=="usgs united states geological survey"]<-"usgs patuxent wildlife research center"
both$INST[both$editor_id==2037 & both$INST=="usgs united states geological survey"]<-"usgs western ecological research center"
both$INST[both$editor_id==2037 & both$JOURNAL=="ECOLOGY"& (both$YEAR>2005 & both$YEAR<2016)]<-"usgs western ecological research center"
both$INST[both$LAST_NAME=="lafferty" & both$FIRST_NAME=="kevin" & both$YEAR==2005]<-"usgs western ecological research center"
both$INST[both$editor_id==3643 & both$INST=="usgs united states geological survey"]<-"usgs fort collins science center"
both$INST[both$editor_id==1848 & both$INST=="usgs united states geological survey"]<-"usgs patuxent wildlife research center"
both$INST[both$editor_id==486 & both$INST=="usgs united states geological survey"]<-"usgs florence bascom geoscience center"


both$INST[both$LAST_NAME=="niewiarowski" & both$INST=="ohio"]<-"university of akron"
both$INST[both$editor_id==1254 & both$INST=="ohio"]<-"ohio university"
both$INST[both$editor_id==2835]<-"pennsylvania state university"
both$INST[both$editor_id==3101]<-"university of washington seattle"




both$INST[(both$editor_id==500|both$editor_id==876|both$editor_id==1125|
             both$editor_id==1779|both$editor_id==2295|both$editor_id==2607|
             both$editor_id==2841|both$editor_id==3202|both$editor_id==3234|
             both$editor_id==3342|both$editor_id==3425|both$editor_id==7) &
            both$INST=="university of british columbia"]<-"university of british columbia vancouver"


both$INST[both$LAST_NAME=="martin" & 
            both$FIRST_NAME=="kathy" & 
            both$INST=="university of british columbia"]<-"university of british columbia vancouver"











both$FIRST_NAME[both$editor_id==3378]<-"s"



# Weisser vs Weiser


both$INST[both$LAST_NAME == "weisser" & both$JOURNAL=="OECOL" &
            (both$YEAR>2006 & both$YEAR<2014)]<-"university of jena"
both$editor_id[both$LAST_NAME == "weisser" & both$JOURNAL=="OECOL" &
                 (both$YEAR>2006 & both$YEAR<2015)]<-NA

both$LAST_NAME[both$LAST_NAME == "weisser" & both$JOURNAL=="OECOL" & (both$YEAR>1983 & both$YEAR<2007)]<-"weiser"
both$MIDDLE_NAME[both$LAST_NAME == "weiser" & both$JOURNAL=="OECOL" &
                   both$YEAR<2007]<-NA

both<-both[!(both$editor_id=="3798" & both$JOURNAL=="OECOL" & both$LAST_NAME=="weiser" & both$YEAR>2006),]




both<-both[!(both$LAST_NAME=="fernande-juricic" & both$JOURNAL=="JAPE" & both$YEAR==2010),]
both<-both[!(both$editor_id=="1261" & both$JOURNAL=="OECOL" & both$YEAR==1999),]
both<-both[!(both$editor_id=="3669" & both$JOURNAL=="OECOL" & both$YEAR==2012),]
both<-both[!(both$editor_id=="2069" & both$JOURNAL=="PLANTECOL" & both$YEAR==2007),]
both<-both[!(both$editor_id=="1079" & both$JOURNAL=="PLANTECOL" & both$YEAR==2007),]
both<-both[!(both$editor_id=="100" & both$JOURNAL=="PLANTECOL" & both$YEAR==1991),]
both<-both[!(both$editor_id=="2896" & both$JOURNAL=="PLANTECOL" & both$YEAR==2004),]
both<-both[!(both$editor_id=="1057" & both$JOURNAL=="BIOCON" & both$YEAR==1985),]
both<-both[!(both$editor_id=="56" & both$JOURNAL=="BIOCON" & both$YEAR==1985),]
both<-both[!(both$editor_id=="2914" & both$JOURNAL=="BIOCON" & both$YEAR==1985),]
both<-both[!(both$editor_id=="753" & both$JOURNAL=="BIOCON" & both$YEAR==1985),]
both<-both[!(both$editor_id=="56" & both$JOURNAL=="BIOCON" & both$YEAR==1986),]
both<-both[!(both$editor_id=="3355" & both$JOURNAL=="BIOCON" & both$YEAR==1998),]
both<-both[!(both$editor_id=="1402" & both$JOURNAL=="OECOL" & both$YEAR==2012),]
both<-both[!(both$editor_id=="3944" & both$JOURNAL=="OECOL" & both$YEAR==1992),]
both<-both[!(both$editor_id=="1819" & both$JOURNAL=="BITR" & both$YEAR==1997),]
both<-both[!(both$editor_id=="591" & both$JOURNAL=="EVOL" & both$YEAR==2015),]
both<-both[!(both$editor_id=="2047" & both$JOURNAL=="JANE" & both$YEAR==2012),]
both<-both[!(is.na(both$editor_id) & both$JOURNAL=="JAPE" & both$LAST_NAME=="croxall"),]
both<-both[!(both$editor_id=="3584" & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2015),]
# ON ADVISORY BOARD BUT NOT ED BOARD
both<-both[!(both$editor_id=="371" & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2012),]
both<-both[!(both$editor_id=="371" & both$JOURNAL=="LANDSCAPEECO" & both$YEAR==2013),]


# COUNTRY CITY UNIT CORRECTIONS
both$CITY[both$UNIT=="Ontario Canada"]<-"Gainesville"
both$STATE[both$UNIT=="Ontario Canada"]<-"FL"
both$UNIT[both$UNIT=="Ontario Canada"]<-"Dept. of Biology"
both$COUNTRY[both$INST=="university of florida"]<-"USA"
both$COUNTRY[both$CITY=="Latvia"]<-"Latvia"
both$CITY[both$CITY=="Latvia"]<-NA
both$CITY[both$CITY=="Ann Arbon"]<-"Ann Arbor"

both$CITY<-gsub("St. ","St ",both$CITY)
both$CITY<-gsub("Saint ","St ",both$CITY)
both$INST<-gsub("St. ","St ",both$INST)
both$INST<-gsub("saint ","st ",both$INST)
both$CITY<-gsub("saint ","st ",both$CITY)
both$CITY<-gsub("Ft. ","Fort ",both$CITY)
both$CITY<-gsub("Ft C","Fort C ",both$CITY)

both$CITY<-gsub("W\x9frzburg","Wurzburg",both$CITY)
both$CITY<-gsub("K\x9aln","cologne",both$CITY)
both$CITY<-gsub("M\x9fnchen","Munich",both$CITY)
both$CITY<-gsub("G\x9attingen","Gottingen",both$CITY)
both$CITY<-gsub("Z\x81rich","Zurich",both$CITY)

# both$CITY<-tolower(both$CITY)

both$CITY<-gsub("Edinburgh EH9 3JZ","edinburgh",both$CITY)
both$CITY<-gsub("Quebec City","Quebec",both$CITY)
both$CITY<-gsub("Fuzerbrook","Furzebrook",both$CITY)
both$CITY<-gsub("E Lansing","East Lansing",both$CITY)
both$CITY<-gsub("Hickory Coners","Hickory Corners",both$CITY)
both$CITY<-gsub("Rodenbosch","Rondebosch",both$CITY)
both$CITY<-gsub("Rodenbosch","Rondebosch",both$CITY)
both$CITY<-gsub("Santa Cruz, California, USA","Santa Cruz",both$CITY)
both$CITY<-gsub("Auburn, Alabama, USA","Auburn",both$CITY)
both$CITY<-gsub("Cambridge, Massachusetts, USA","Cambridge",both$CITY)
both$CITY<-gsub("Sheffield S10 2TN","Sheffield",both$CITY)
both$CITY<-gsub("Mississippi State","",both$CITY)
both$CITY<-gsub("STORRS","storrs",both$CITY)
both$CITY<-gsub("starrs","storrs",both$CITY)
both$CITY<-gsub("Champaign","Urbana-Champaign",both$CITY)
both$CITY<-gsub("Urbana","Urbana-Champaign",both$CITY)
both$CITY<-gsub("Guleph","Guelph",both$CITY)
both$CITY<-gsub("Goettingen","Gottingen",both$CITY)
both$CITY<-gsub("Osnabrück","Osnabruck",both$CITY)
both$CITY<-gsub("Montana","",both$CITY)
both$CITY<-gsub("Antwerpen","Antwerp",both$CITY)
both$CITY<-gsub("Brasília","Brasilia",both$CITY)
both$CITY<-gsub("P.R.","",both$CITY)
both$CITY<-gsub("Denmark","",both$CITY)
both$CITY<-gsub("Mexico D.F.","Mexico City",both$CITY)
both$CITY<-gsub("Stockolm","Stockholm",both$CITY)
both$CITY<-gsub("NottinghamUK","Nottingham",both$CITY)
both$CITY<-gsub("Stonybrook","Stony Brook",both$CITY)
both$CITY<-gsub("Tufts","Medford",both$CITY)
both$CITY<-gsub("Tuscon","Tucson",both$CITY)
both$CITY<-gsub("New York City","New York",both$CITY)
both$CITY<-gsub("Notre Dame","South Bend",both$CITY)

both$CITY<-gsub("St Martin d'Heres Cedex","St Martin dHeres Cedex",both$CITY)
both$CITY<-gsub("St-Jean-sur-Richelieu","St Jean sur Richelieu",both$CITY)

# NOTES
both$NOTES[both$CITY=="Latvia"]<-NA
both$NOTES[both$editor_id==130& both$YEAR==1990]<-"INST was formerly oxford polytechnic"
both$NOTES[both$editor_id==3395 & both$YEAR==1986]<-"not 100% regarding INST"
both$NOTES[both$editor_id==289 & both$YEAR==1985]<-"not 100% regarding INST"
both$NOTES[both$editor_id==722 & both$YEAR==1985]<-"not 100% regarding INST; city in 1993 paper & jrnl front mattter don't match"



return(both)
}
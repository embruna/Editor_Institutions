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
# TODO: something is going on with editor IDS
##########

##########
# SEPARATE FUNCTION
# JBIOG
# LECO
# PLANTECOL
# OIKOS
# OECOLOGIA
# JANE 
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
colnames(INST_fix)



INST_fix$INST<-tolower(INST_fix$INST)
INST_fix$UNIT<-tolower(INST_fix$UNIT)
INST_fix$CITY<-tolower(INST_fix$CITY)
INST_fix$STATE<-tolower(INST_fix$STATE)
INST_fix$COUNTRY<-tolower(INST_fix$COUNTRY)
INST_fix$NOTES<-tolower(INST_fix$NOTES)
INST_fix$JOURNAL<-tolower(INST_fix$JOURNAL)
INST_fix$FIRST_NAME<-tolower(INST_fix$FIRST_NAME)
INST_fix$MIDDLE_NAME<-tolower(INST_fix$MIDDLE_NAME)
INST_fix$LAST_NAME<-tolower(INST_fix$LAST_NAME)

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
colnames(both)
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
both$LAST_NAME[both$editor_id.x==1355 & both$FIRST_NAME=="holmes"]<-"rolston"
both<-both[!(is.na(both$editor_id.x) & both$FIRST_NAME=="holmes"),]


# both$LAST_NAME.y<-NULL
# both$LAST_check<-NULL
# both<-both %>% rename("LAST_NAME"="LAST_NAME.x")


# state differences between all data and checked file
summary(both$STATE.x==both$STATE.y) # 19 false
both$STATE_check<-both$STATE.x==both$STATE.y
both$STATE.x[both$STATE.x=="missing"]<-NA
both$STATE.x[both$YEAR!=1992 & both$FIRST_NAME=="david" & both$LAST_NAME=="gibson" & both$CITY.x=="carbondale"]<-"il"
both$STATE.x[both$YEAR==1992 & both$CITY.x=="pensacola" & both$LAST_NAME=="gibson"]<-"fl"
both$STATE.x[both$STATE.x=="england"]<-NA
both$STATE.x[both$LAST_NAME=="moss"]<-NA
both$STATE.x[both$LAST_NAME=="usher"]<-NA
both$STATE.x[both$LAST_NAME=="milner-gulland"]<-NA
both$INST.y[both$LAST_NAME=="belovsky" & both$INST.y=="notre dame"]<-"university of notre dame"
both$CITY.x[both$LAST_NAME=="belovsky" & both$INST.y=="notre dame"]<-"notre dame"
both$COUNTRY.x[both$LAST_NAME=="belovsky" & both$INST.y=="university of notre dame"]<-"usa"
both$STATE.x[both$LAST_NAME=="belovsky" & both$INST.y=="notre dame university"]<-"in"
both$STATE.x[both$FIRST_NAME=="james" & both$LAST_NAME=="carlton"]<-"ct"
both$STATE.x[both$FIRST_NAME=="jon" & both$LAST_NAME=="rodriguez"]<-NA
both$STATE.x[both$editor_id.x==455 & both$FIRST_NAME=="christopher" & both$LAST_NAME=="frissell"]<-"mt"
both$INST.y[both$editor_id.x==455 & both$FIRST_NAME=="christopher" & both$LAST_NAME=="frissell"]<-"university of montana"
both$UNIT.x[both$editor_id.x==455 & both$FIRST_NAME=="christopher" & both$LAST_NAME=="frissell"]<-"flathead lake biological station"
both$CITY.x[both$editor_id.x==455 & both$FIRST_NAME=="christopher" & both$LAST_NAME=="frissell"]<-"polson"
both$CITY.x[both$editor_id.x==1511 & both$LAST_NAME=="cinner" & both$YEAR>2009 & both$YEAR<2015 ]<-"townsville"
both$STATE.x[both$editor_id.x==1511 & both$LAST_NAME=="cinner" & both$YEAR>2009 & both$YEAR<2015 ]<-"queensland"
both$UNIT.x[both$editor_id.x==1511 & both$LAST_NAME=="cinner"]<-"arc centre of excellence for coral reef studies"
both$INST.y[both$editor_id.x==1511 & both$LAST_NAME=="cinner"]<-"james cook university"
both$NOTES.y[both$editor_id.x==1511 & both$LAST_NAME=="cinner" & both$YEAR==2013]<-"journal front matter has inst=columbia univ, but his cv makes no mention of this"

# this will replace all the "na" in STATE.x (origianlly no info) with the value from STATE.y (patrick's data collection), if there is one
both<-both %>% mutate(STATE.x = replace(STATE.x, is.na(STATE.x), STATE.y[is.na(STATE.x)]))


both$STATE.y<-NULL
both$STATE_check<-NULL
both<-both %>% rename("STATE"="STATE.x")

# this identifies one mistake in thge original (original_data) that needs to be corrected
both$UNIT.x[both$UNIT.x=="estaci<f3>n biol<f3>gica de do<f1>ana"]<-"estacion biologica donana"
both$UNIT.y[both$UNIT.y=="estaci<f3>n biol<f3>gica de do<f1>ana"]<-"estacion biologica donana"
both$UNIT.x<-gsub("estaci\xf3n biol\xf3gica de do\xf1ana","estacion biologica donana",both$UNIT.x)
both$UNIT.x<-gsub("biologie/chemie/\x80kologie","biochemical ecology",both$UNIT.x)
both$UNIT.x<-gsub("f\x99r","fur",both$UNIT.x)
both$UNIT.x<-gsub("ecolog\x90a","ecologia",both$UNIT.x)

# inst differences between all data and checked file
both$INST.y<-as.character(both$INST.y)
both$INST.y<-tolower(both$INST.y)
both$INST.y<-tolower(both$INST.y)
both$UNIT.x<-tolower(both$UNIT.x)
both$UNIT.y<-tolower(both$UNIT.y)


# this will replace all the "na" and "" in INST.x (origianlly no info) with the value from INST.y (patrick's data collection), if there is one
both<-both %>% mutate(INST.x = replace(INST.x, is.na(INST.x),INST.y[is.na(INST.x)]))
both <- both %>% mutate(INST.x = replace(INST.x, INST.x == "", NA))
summary(both$INST.y==both$INST.y) # 235 false
both$inst_check<-both$INST.y==both$INST.y

inst_check<-filter(both,inst_check=="FALSE")
inst_check_ok<-filter(both,inst_check==TRUE |is.na(inst_check))
#
# write.csv(inst_check, file="./data/patrick_james_data_corrections/complete/inst_corrections_2x.csv", row.names = f) #export it as a csv file


both$INST.y[both$editor_id.x==1248 & both$JOURNAL=="bitr" & both$YEAR==1987 ]<-"new york botanical garden"
both$INST.y[both$editor_id.x==1704 & both$JOURNAL=="jecol" & both$YEAR==2009 ]<-"university of sheffield"
# both$INST.y[both$INST.y=="unniversity of stirling" ]<-"university of stirling"

both$UNIT.x[both$editor_id.x==1229 & both$INST.y=="university of montana"]<-"savannah river ecology laboratory"
both$INST.y[both$editor_id.x==1229 & both$INST.y=="university of montana"]<-"university of georgia"

both$UNIT.x[both$INST.y=="alterra research institute for the green world" ]<-"alterra research institute for the green world"
both$UNIT.x[both$INST.y=="gatty marine lab (university of saint andrews)" ]<-"gatty marine lab"
both$UNIT.x[both$INST.y=="netherlands institute of ecology; wageningen university and research centre netherlands institute of ecology" ]<-"netherlands institute of ecology"
both$UNIT.x[both$INST.y=="norwich" ]<-"norwich research park industrial biotechnology and bioenergy alliance"
both$UNIT.x[both$INST.y=="scripps institute of oceanography" ]<-"scripps institution of oceanography"
both$UNIT.x[both$INST.y=="scripps institution  of oceanography" ]<-"scripps institution of oceanography"
both$UNIT.x[both$INST.y=="scripps institute of oceanography" ]<-"gatty marine lab"



both$UNIT.x[both$INST.y=="savannah river ecology laboratory" ]<-"savannah river ecology laboratory"
both$INST.y[both$INST.y=="savannah river ecology laboratory" ]<-"university of georgia"
both$UNIT.x[both$INST.y=="university of california santa cruz extension"]<-"ucsc extension"
both$INST.y[both$INST.y=="university of california" & both$INST.y=="university of california davis" ]<-"university of california davis"
both$INST.y[both$INST.y=="university of california" & both$INST.y=="university of california berkeley" ]<-"university of california berkeley"
both$INST.y[both$INST.y=="university of california" & both$INST.y=="university of california riverside" ]<-"university of california riverside"
# both$INST.y[both$editor_id.x==1839 & both$JOURNAL=="conbio" ]<-"venezuelan institute for scientific investigation"
both$INST.y[both$editor_id.x==1218 & both$JOURNAL=="conbio" & both$YEAR>1986 & both$YEAR<1992 ]<-"royal botanic gardens kew"
# both$INST.y[both$INST.y=="museum natl hist nat"]<-"national history museum paris"
# both$INST.y[both$INST.y=="university<ca>of<ca>california<ca>santa<ca>cruz"]<-"university of california santa cruz"
# both$INST.y[both$INST.y=="uc santa cruz"]<-"university of california santa cruz"

both$INST.y[both$LAST_NAME=="streeter" & both$JOURNAL=="jecol" & both$YEAR==2009 ]<-"university of sussex"
both$INST.y[both$LAST_NAME=="grover" & both$JOURNAL=="amnat"]<-"university of texas arlington"
both$INST.y[both$LAST_NAME=="noss" & both$JOURNAL=="conbio" & both$YEAR==1998 ]<-"conservation biology institute"

both$UNIT.x[both$LAST_NAME=="dratch" & both$JOURNAL=="conbio" & both$INST.y=="national fish and wildlife forensics laboratory" ]<-"national fish and wildlife forensics laboratory"
both$INST.y[both$LAST_NAME=="dratch" & both$JOURNAL=="conbio" & both$INST.y=="national fish and wildlife forensics laboratory" ]<-"us fish and wildlife service"
both$INST.y[both$LAST_NAME=="daszak" & both$JOURNAL=="conbio" & both$INST.y=="university of nevada reno" ]<-"consortium for conservation medicine"

both$UNIT.x[both$LAST_NAME=="meffe" & both$JOURNAL=="conbio" & both$INST.y=="university of montana" ]<-"savanna riverl ecological laboratory"
both$UNIT.x[both$LAST_NAME=="meffe" & both$JOURNAL=="conbio" & both$UNIT.x=="savanna riverl ecological laboratory" ]<-"university of georgia"
both$UNIT.x[both$JOURNAL=="leco" & both$INST.y=="institute of landscape ecology of slovak academy of sciences" ]<-"institute of landscape ecology"
both$INST.y[both$JOURNAL=="leco" & both$INST.y=="institute of landscape ecology of slovak academy of sciences" ]<-"slovak academy of sciences"

summary(both$INST.y==both$INST.y)
both$inst_check<-both$INST.y==both$INST.y
both$INST.y<-NULL
both$inst_check<-NULL
both<-both %>% rename("INST"="INST.x")

# city differences between all data and checked file
summary(both$CITY.x==both$CITY.y) # 52 false
both$city_check<-both$CITY.x==both$CITY.y
city_check<-filter(both,city_check=="false")
city_check_ok<-filter(both,city_check==TRUE |is.na(city_check))
write.csv(city_check, file="./data/patrick_james_data_corrections/complete/city_corrections_2x.csv", row.names = FALSE) #export it as a csv file
both$STATE[both$CITY.x=="new mexico" & both$CITY.y=="las cruces"]<-"nm"
both$CITY.x[both$CITY.x=="new mexico" & both$CITY.y=="las cruces"]<-"las cruces"
both$CITY.x[is.na(both$CITY.x) & both$CITY.y=="las cruces"]<-"las cruces"
both$NOTES.y[both$CITY.x=="basel" & both$CITY.y=="lausanne"]<-"2x city"
both$NOTES.y[both$CITY.x=="brighton" & both$CITY.y=="toronto"]<-"2x city"
both$NOTES.y[both$CITY.x=="zurich" & both$CITY.y=="basel"]<-"2x city"
both$NOTES.y[both$CITY.x=="canberra" & both$CITY.y=="lyneham"]<-"2x city"

both$CITY.x[both$CITY.x=="stanford" & both$CITY.y=="pacific grove"]<-"pacific grove"
both$CITY.x[both$CITY.x=="manhattan" & both$CITY.y=="new york"]<-"new york"
both$CITY.x[both$CITY.x=="east lansging" & both$CITY.y=="east lansing"]<-"east lansing"
both$CITY.x[both$CITY.x=="new yor city" & both$CITY.y=="new york city"]<-"new york"
both$CITY.x[both$CITY.x=="manhattan" & both$CITY.y=="new york"]<-"new york"
both$CITY.x[both$CITY.x=="los angeles" & both$CITY.y=="malibu"]<-"malibu"
both$CITY.x[both$CITY.x=="manhattan" & both$CITY.y=="new york"]<-"new york"
both$CITY.x[both$CITY.x=="manhattan" & both$CITY.y=="new york"]<-"new york"
both$CITY.x[both$CITY.x=="sheffield s10 2tn"]<-"sheffield s10 2tn"

both$UNIT.x[both$CITY.x=="\xcadepartment of animal ecology and tropical biology (zoology iii)"]<-"department of animal ecology and tropical biology (zoology iii)"
both$CITY.x[both$CITY.x=="\xcadepartment of animal ecology and tropical biology (zoology iii)"]<-NA

both$NOTES.y[both$CITY.x=="aberdeen" & both$CITY.y=="cragiebuckler"]<-"2x city"
both$CITY.x[both$CITY.x=="invergowric" & both$CITY.y=="invergowrie"]<-"invergowrie"
both$NOTES.y[both$CITY.x=="brisbane" & both$CITY.y=="st lucia"]<-"2x city"
both$NOTES.y[both$CITY.x=="london" & both$CITY.y=="ascot"]<-"2x city"
both$NOTES.y[both$CITY.x=="williams" & both$CITY.y=="mystic"]<-"2x city"
both$NOTES.y[both$CITY.x=="melbourne" & both$CITY.y=="parkville"]<-"2x city"
both$NOTES.y[both$CITY.x=="new brunswick" & both$CITY.y=="polson"]<-"2x city"
both$NOTES.y[both$CITY.x=="london" & both$CITY.y=="notre dame"]<-"notre dame"
both$NOTES.y[both$CITY.x=="canberra" & both$CITY.y=="lyneham"]<-"2x city"
both$NOTES.y[both$CITY.x=="canberra" & both$CITY.y=="lyneham"]<-"2x city"
both$NOTES.y[both$CITY.x=="canberra" & both$CITY.y=="lyneham"]<-"2x city"

summary(both$CITY.x==both$CITY.y)
both$city_check<-both$CITY.x==both$CITY.y
# write.csv(city_check, file="./data/patrick_james_data_corrections/complete/city_corrections_2x.csv", row.names = f) #export it as a csv file

# this will replace all the "na" in city.x (origianlly no info) with the value from city.y (patrick's data collection), if there is one
both<-both %>% mutate(CITY.x = replace(CITY.x, is.na(CITY.x), CITY.y[is.na(CITY.x)]))

both$CITY.y<-NULL
both$city_check<-NULL
both<-both %>% rename("CITY"="CITY.x")
# str(both)

# todo unit differences between all data and checked file
# both$UNIT.x<-as.character(both$UNIT.x)
# both$UNIT.x<-gsub("",na,both$UNIT.x)
both$UNIT.x[both$UNIT.x == ""] <- NA
summary(both$UNIT.x==both$UNIT.y) # 7 false
both$unit_check<-both$UNIT.x==both$UNIT.y

# no probs, just differences in words ("the, and", etc)
# this will replace all the "na" in unit.x (origianlly no info) with the value from unit.y (patrick's data collection), if there is one
both<-both %>% mutate(UNIT.x = replace(UNIT.x, is.na(UNIT.x), UNIT.y[is.na(UNIT.x)]))

both$UNIT.x[both$UNIT.y=="savannah river ecology laboratory"]<-"savannah river ecology laboratory"
both$UNIT.y<-NULL
both$unit_check<-NULL
both<-both %>% rename("UNIT"="UNIT.x")

# todo country differences between all data and checked file

both <- both %>% mutate(COUNTRY.x = replace(COUNTRY.x, COUNTRY.x == "", NA))
both$COUNTRY.x<-as.factor(both$COUNTRY.x)
both$COUNTRY.y<-as.factor(both$COUNTRY.y)
country_levels<-(c(levels(both$COUNTRY.x),levels(both$COUNTRY.y)))
levels(both$COUNTRY.x)<-c(levels(both$COUNTRY.x),country_levels,NA)
levels(both$COUNTRY.y)<-c(levels(both$COUNTRY.y),country_levels,NA)

# this will replace all the "na" in city.x (origianlly no info) with the value from city.y (patrick's data collection), if there is one

levels(both$COUNTRY.x)
str(both$COUNTRY.x)
str(both$COUNTRY.y)

both<-both %>% mutate(COUNTRY.x = replace(COUNTRY.x, is.na(COUNTRY.x), COUNTRY.y[is.na(COUNTRY.x)]))
summary(both$COUNTRY.x==both$COUNTRY.y) # 3552 false
both$country_check<-both$COUNTRY.x==both$COUNTRY.y

country_check<-filter(both,country_check=="FALSE")
# write.csv(country_check, file="./data/patrick_james_data_corrections/complete/country_corrections_2x.csv", row.names = f) #export it as a csv file


both$FIRST_NAME[both$LAST_NAME=="weiher" & both$JOURNAL=="plantecol"]<-"ewan"
both$MIDDLE_NAME[both$LAST_NAME=="long" & both$FIRST_NAME=="steve" & both$JOURNAL=="gcb"]<-"p"
both$FIRST_NAME[both$LAST_NAME=="long" & both$FIRST_NAME=="steve" & both$JOURNAL=="gcb"]<-"stephen"
both$INST[both$LAST_NAME=="long" & both$FIRST_NAME=="stephen" &
            both$JOURNAL=="gcb" & both$YEAR>1999]<-"university of illinois"

both$FIRST_NAME[both$LAST_NAME=="olsvig-whittaker" & both$JOURNAL=="plantecol"]<-"d"
both$MIDDLE_NAME[both$LAST_NAME=="olsvig-whittaker" & both$JOURNAL=="plantecol"]<-"l"

both$COUNTRY.x[both$LAST_NAME=="tjoelker" & both$JOURNAL=="newphyt" & both$INST=="texas a & m university"]<-"usa"
both$COUNTRY.x[both$LAST_NAME=="atkin" & both$JOURNAL=="newphyt" & both$INST=="university of york"]<-"united kingdom"
both$COUNTRY.x[both$LAST_NAME=="long" & both$JOURNAL=="jecol" & both$INST=="university of essex"]<-"united kingdom"
both$COUNTRY.x[both$LAST_NAME=="westing" & both$JOURNAL=="conbio" & both$INST=="stockholm international peace research institute"]<-"sweden"
both$CITY[both$LAST_NAME=="westing" & both$JOURNAL=="conbio" & both$INST=="stockholm international peace research institute"]<-"stockholm"
both$STATE[both$LAST_NAME=="westing" & both$JOURNAL=="conbio" & both$INST=="stockholm international peace research institute"]<-NA
both$COUNTRY.x[both$LAST_NAME=="westing" & both$JOURNAL=="conbio" & both$INST=="westing associates"]<-"usa"
both$COUNTRY.x[both$LAST_NAME=="belovsky" & both$JOURNAL=="conbio" & both$INST=="utah state university"]<-"usa"
both$UNIT[both$LAST_NAME=="bolker" & both$JOURNAL=="amnat" & both$INST=="mcmaster university"]<-NA
both$COUNTRY.x[both$LAST_NAME=="bolker" & both$JOURNAL=="amnat" & both$INST=="mcmaster university"]<-"canada"

both$UNIT[both$LAST_NAME=="krivan" & both$JOURNAL=="amnat" ]<-"biology centre"
both$INST[both$LAST_NAME=="krivan" & both$JOURNAL=="amnat" ]<-"academy of sciences of the czech republic"
both$CITY[both$LAST_NAME=="krivan" & both$JOURNAL=="amnat" ]<-"ceske budejovice"
both$STATE[both$LAST_NAME=="krivan" & both$JOURNAL=="amnat"]<-"south bohemia"
both$COUNTRY.x[both$LAST_NAME=="krivan" & both$JOURNAL=="amnat"]<-"czech republic"

both$NOTES.y[both$COUNTRY.y == "wales"] <- "wales"
both$NOTES.y[both$COUNTRY.y == "scotland"] <- "scotland"
both$NOTES.y[both$COUNTRY.y == "england"] <- "england"
both$NOTES.y[both$COUNTRY.x == "wales"] <- "wales"
both$NOTES.y[both$COUNTRY.x == "scotland"] <- "scotland"
both$NOTES.y[both$COUNTRY.x == "england"] <- "england"
both$COUNTRY.x[both$COUNTRY.x == "wales"] <- "united kingdom"
both$COUNTRY.x[both$COUNTRY.x == "scotland"] <- "united kingdom"
both$COUNTRY.x[both$COUNTRY.x == "england"] <- "united kingdom"


both$COUNTRY.y<-NULL
both$COUNTRY_check<-NULL
both<-both %>% rename("COUNTRY"="COUNTRY.x")

# todo notes differences between all data and checked file

str(both$NOTES.x)
both$NOTES <-paste(both$NOTES.x,both$NOTES.y, sep= " / ")
both$NOTES[both$NOTES == "na / na"] <- NA
both$NOTES <-gsub(" / na","",both$NOTES)
both$NOTES <-gsub("na / ","",both$NOTES)
both$NOTES.x<-NULL
both$NOTES.y<-NULL
both$NOTES[both$INST=="lyme regis"]<-"is this ghillean prance unattached?"
both$NOTES[both$INST=="neri"]<-"no longer exists: https://tethys.pnnl.gov/institution/national-environmental-research-institute-neri"
both$NOTES[both$LAST_NAME=="boggs"]<-"2x carol boggs has inst colorado but should be stanford"
# both$NOTES[both$INST == "biological centre"] <- "double check inst"
# both$NOTES[both$INST == "biological institute"] <- "double check inst"
# both$NOTES[both$INST == "bogota"] <- "double check inst"
# both$NOTES[both$INST == "disteba university of salento"] <- "double check inst -  / disteba universita di lecce"
# both$NOTES[both$INST == "haus nr.9"] <- "double check inst"
# both$NOTES[both$INST == "james cook university townsville"] <- "double check inst"
# both$NOTES[both$INST == "lancaster"] <- "double check inst"
# both$NOTES[both$INST == "london"] <- "double check inst"
# both$NOTES[both$INST == "madrid"] <- "double check inst"
# both$NOTES[both$INST == "maine"] <- "double check inst"
# both$NOTES[is.na(both$INST)] <- "double check inst"
# both$NOTES[both$INST == "royal botanic gardens melbourne university of melbourne"] <- "double check inst"
# both$NOTES[both$INST == "salzburg"] <- "double check inst"
# both$NOTES[both$INST == "swansea"] <- "double check inst"
# both$NOTES[both$INST == "sydney"] <- "double check inst"
# both$NOTES[both$INST == "seidenstzicker& kleiman = smithsonian national zoological park, labandeira: smithsonian national museum of natural history"] <- "double check inst"
# both$NOTES[both$INST == "montpellier"] <- "double check inst"
# both$NOTES[both$INST == "double check"] <- "double check inst"
# both$NOTES[both$INST == "cnrs"] <- "double check what campus/unit"
# both$NOTES[both$INST == "csiro"] <- "double check what campus/unit"
# both$NOTES[both$INST == "smithsonian institution"] <- "double check what campus/unit"
# both$NOTES[both$INST == "university of arkansas"] <- "double check what campus/unit"
# both$NOTES[both$INST == "university of british columbia"] <- "double check what campus/unit"
# both$NOTES[both$INST == "university of california"] <- "double check what campus/unit"
# both$NOTES[both$INST == "university of exeter"] <- "double check what campus/unit"
# both$NOTES[both$INST == "university of georgia"] <- "double check what campus/unit"
# both$NOTES[both$INST == "university of illinois"] <- "double check what campus/unit"
# both$NOTES[both$INST == "university of massachusetts"] <- "double check what campus/unit"
# both$NOTES[both$INST == "university of minnesota"] <- "double check what campus/unit"
# both$NOTES[both$INST == "university of south carolina"] <- "double check what campus/unit"
# both$NOTES[both$INST == "university of texas"] <- "double check what campus/unit"
# both$NOTES[both$INST == "university of toronto"] <- "double check what campus/unit"
# both$NOTES[both$INST == "university of wisconsin"] <- "double check what campus/unit"
# both$NOTES[both$INST == "us geological survey"] <- "double check what campus/unit"



# both<-institution_cleaner(both)


# no idea why this isn't working inside function, so doing here
both$INST<-gsub("arkansas","arkansas", both$INST)


# todo: some error checking of states:
# canada instead of canada
# british columbia in usa
# lower austria     usa
# galicia     usa
# england     usa
# uppland     usa
# ontario in usa
# gelderland

both$COUNTRY[both$STATE == "british columbia" & both$COUNTRY == "usa"] <- "canada"
both$COUNTRY[both$STATE == "lower austria" & both$COUNTRY == "usa"] <- "canada"
both$COUNTRY[both$STATE == "galicia" & both$COUNTRY == "usa"] <- "spain"
both$COUNTRY[both$STATE == "england" & both$COUNTRY == "usa"] <- "united kingdom"
both$COUNTRY[both$STATE == "uppland" & both$COUNTRY == "usa"] <- "sweden"
both$COUNTRY[both$STATE == "ontario"] <- "canada"
both$COUNTRY[both$STATE == "gelderland"] <- "netherlands"
levels(both$INST)




colnames(both)
both$editor_id.y<-NULL
both<-both %>% rename("editor_id"="editor_id.x")

both<-both[!(is.na(both$JOURNAL) & is.na(both$YEAR)),]

# 
# both<-both %>%
#   group_by(journal,LAST_NAME,first_name) %>%
#   mutate(inst = ifelse((row_number()==1 & is.na(inst)), "missing", inst))
# 
# 
# both<-both %>%
#   group_by(journal,LAST_NAME,first_name) %>%
#   mutate(unit = ifelse((row_number()==1 & is.na(unit)), "missing", unit))
# 
# both<-both %>%
#   group_by(journal,LAST_NAME,first_name) %>%
#   mutate(state = ifelse((row_number()==1 & is.na(state)), "missing", state))
# 
# 
# both<-both %>%
#   group_by(journal,LAST_NAME,first_name) %>%
#   mutate(city = ifelse((row_number()==1 & is.na(city)), "missing", city))
# 
new_row<-both %>% 
  filter(LAST_NAME=="willis" & JOURNAL=="jecol" & YEAR==1994)
new_row$YEAR<-1992
both<-rbind(new_row,both)
both$INST[both$LAST_NAME=="willis"& both$JOURNAL=="jecol" &
                (both$YEAR>1990|both$YEAR<2007)]<-"university of sheffield"
both$CITY[both$LAST_NAME=="willis"& both$JOURNAL=="jecol" &
            (both$YEAR>1990|both$YEAR<2007)]<-"sheffield"
both$STATE[both$LAST_NAME=="willis"& both$JOURNAL=="jecol" &
            (both$YEAR>1990|both$YEAR<2007)]<-"s yorkshire"
both$COUNTRY[both$LAST_NAME=="willis"& both$JOURNAL=="jecol" &
             (both$YEAR>1990|both$YEAR<2007)]<-"united kingdom"

both<-both %>% arrange(JOURNAL,LAST_NAME,FIRST_NAME,YEAR)

# colnames(both)
# str(as.data.frame(both))
# str(alldata)
# colnames(alldata)
# colnames(both)==colnames(alldata)





# added eb 11 october 2020
both$FIRST_NAME<-tolower(both$FIRST_NAME)
both$LAST_NAME<-tolower(both$LAST_NAME)
both$MIDDLE_NAME<-tolower(both$MIDDLE_NAME)

both$INST[both$INST=="double check"]<-NA

both$INST[both$LAST_NAME=="charlesworth" & both$YEAR==1991 & both$JOURNAL=="amnat"]<-"university of chicago"
both$INST[both$LAST_NAME=="chesson" & both$YEAR==1991 & both$JOURNAL=="amnat"]<-"australian national university"
both$INST[both$LAST_NAME=="chesson" & both$YEAR==1995 & both$JOURNAL=="amnat"]<-"australian national university"
both$INST[both$LAST_NAME=="duffy" & both$YEAR==2015 & both$JOURNAL=="amnat"]<-"georgia institute of technology"
both$INST[both$LAST_NAME=="dworkin" & both$YEAR==2015 & both$JOURNAL=="amnat"]<-"mcmaster university"
both$INST[both$LAST_NAME=="frederickson" & both$YEAR==2015 & both$JOURNAL=="amnat"]<-"university of toronto"
both$INST[both$LAST_NAME=="fuller" & both$YEAR==2015 & both$JOURNAL=="amnat"]<-"university of illinois"
both$INST[both$LAST_NAME=="kirkpatrick" & both$YEAR==1991 & both$JOURNAL=="amnat"]<-"university of texas austin"
both$INST[both$LAST_NAME=="leips" & both$YEAR==2015 & both$JOURNAL=="amnat"]<-"university of maryland baltimore county"
both$INST[both$LAST_NAME=="meagher" & both$YEAR==1991 & both$JOURNAL=="amnat"]<-"university of st andrews"
both$INST[both$LAST_NAME=="mooney" & both$YEAR==1985 & both$JOURNAL=="amnat"]<-"stanford university"
both$INST[both$LAST_NAME=="mooney" & both$YEAR==1990 & both$JOURNAL=="amnat"]<-"stanford university"
both$INST[both$LAST_NAME=="pagel" & both$YEAR==1997 & both$JOURNAL=="amnat"]<-"university of oxford"
both$INST[both$LAST_NAME=="pastor" & both$YEAR==1991 & both$JOURNAL=="amnat"]<-"university of minnesota duluth"
both$INST[both$LAST_NAME=="real" & both$YEAR==1991 & both$JOURNAL=="amnat"]<-"indiana university"
both$INST[both$LAST_NAME=="real" & both$YEAR==1993 & both$JOURNAL=="amnat"]<-"indiana university"
both$INST[both$LAST_NAME=="roth" & both$YEAR==1991 & both$JOURNAL=="amnat"]<-"duke university"
both$INST[both$LAST_NAME=="roughgarden" & both$YEAR==1985 & both$JOURNAL=="amnat"]<-"stanford university"
both$INST[both$LAST_NAME=="roughgarden" & both$YEAR==1990 & both$JOURNAL=="amnat"]<-"stanford university"
both$INST[both$LAST_NAME=="seger" & both$YEAR==1991 & both$JOURNAL=="amnat"]<-"university of utah"
both$INST[both$LAST_NAME=="tilma" & both$YEAR==1991 & both$JOURNAL=="amnat"]<-"university of minnesota"
both$INST[both$LAST_NAME=="tilma" & both$YEAR==1994 & both$JOURNAL=="amnat"]<-"university of minnesota"
both$INST[both$LAST_NAME=="whitlock" & both$YEAR==2005 & both$JOURNAL=="amnat"]<-"university of british columbia"
both$INST[both$LAST_NAME=="wu" & both$YEAR==1991 & both$JOURNAL=="amnat"]<-"university of chicago"
both$INST[both$LAST_NAME=="foote" & both$YEAR==2004 & both$JOURNAL=="arees"]<-"university of chicago"
both$INST[both$LAST_NAME=="werner" & both$YEAR==1985 & both$JOURNAL=="arees"]<-"michigan state university"
both$INST[both$LAST_NAME=="wing" & both$YEAR==1999 & both$JOURNAL=="arees"]<-"smithsonian national museum of natural history"
both$INST[both$LAST_NAME=="wing" & both$YEAR==2003 & both$JOURNAL=="arees"]<-"smithsonian national museum of natural history"
both$INST[both$LAST_NAME=="andelman" & both$YEAR==2006 & both$JOURNAL=="biocon"]<-"university of california santa barbara"
both$INST[both$LAST_NAME=="bourliere" & both$YEAR==1986 & both$JOURNAL=="biocon"]<-"university of paris"
both$INST[both$LAST_NAME=="dirzo" & both$YEAR==1998 & both$JOURNAL=="biocon"]<-"northern arizona university"
both$INST[both$LAST_NAME=="duffey" & both$YEAR==1989 & both$JOURNAL=="biocon"]<-"unaffiliated"
both$INST[both$LAST_NAME=="duffey" & both$YEAR==2014 & both$JOURNAL=="biocon"]<-"unaffiliated"
both$INST[both$LAST_NAME=="hawkins" & both$YEAR==2008 & both$JOURNAL=="biocon"]<-"university of southampton"
both$INST[both$LAST_NAME=="hawkins" & both$YEAR==2014 & both$JOURNAL=="biocon"]<-"university of southampton"
both$INST[both$LAST_NAME=="hockey" & both$YEAR==1997 & both$JOURNAL=="biocon"]<-"university of cape town"
both$INST[both$LAST_NAME=="hockey" & both$YEAR==2007 & both$JOURNAL=="biocon"]<-"university of cape town"
both$INST[both$LAST_NAME=="hockey" & both$YEAR==2008 & both$JOURNAL=="biocon"]<-"university of cape town"
both$INST[both$LAST_NAME=="hockey" & both$YEAR==2009 & both$JOURNAL=="biocon"]<-"university of cape town"
both$INST[both$LAST_NAME=="johnsingh" & both$YEAR==2006 & both$JOURNAL=="biocon"]<-"wildlife institute of india"
both$INST[both$LAST_NAME=="krishnaswamy" & both$YEAR==2014 & both$JOURNAL=="biocon"]<-"ashoka trust for research in ecology and the environment"
both$INST[both$LAST_NAME=="kuenen" & both$YEAR==1986 & both$JOURNAL=="biocon"]<-"university of leiden"
both$INST[both$LAST_NAME=="lomolino" & both$YEAR==1998 & both$JOURNAL=="biocon"]<-"university of oklahoma"
both$INST[both$LAST_NAME=="marrs" & both$YEAR==2014 & both$JOURNAL=="biocon"]<-"university of liverpool"
both$INST[both$LAST_NAME=="mcnicholl" & both$YEAR==1990 & both$JOURNAL=="biocon"]<-"unaffiliated"
both$INST[both$LAST_NAME=="mcnicholl" & both$YEAR==1993 & both$JOURNAL=="biocon"]<-"unaffiliated"
both$INST[both$LAST_NAME=="morgan" & both$YEAR==1987 & both$JOURNAL=="biocon"]<-"station biologique de la tour du valat"
both$INST[both$LAST_NAME=="morgan" & both$YEAR==2004 & both$JOURNAL=="biocon"]<-"station biologique de la tour du valat"
both$INST[both$LAST_NAME=="peterken" & both$YEAR==1998 & both$JOURNAL=="biocon"]<-"unaffiliated"
both$INST[both$LAST_NAME=="peterken" & both$YEAR==2009 & both$JOURNAL=="biocon"]<-"unaffiliated"
both$INST[both$LAST_NAME=="scott" & both$YEAR==1986 & both$JOURNAL=="biocon"]<-"unaffiliated"
both$INST[both$LAST_NAME=="westhoff" & both$YEAR==1985 & both$JOURNAL=="biocon"]<-"unaffiliated"
both$INST[both$LAST_NAME=="westhoff" & both$YEAR==1988 & both$JOURNAL=="biocon"]<-"unaffiliated"
both$INST[both$LAST_NAME=="anderson" & both$YEAR==1993 & both$JOURNAL=="bitr"]<-"australian national university"
both$INST[both$LAST_NAME=="anderson" & both$YEAR==1997 & both$JOURNAL=="bitr"]<-"australian national university"
both$INST[both$LAST_NAME=="benson" & both$YEAR==1993 & both$JOURNAL=="bitr"]<-"universidade estadual de campinas"
both$INST[both$LAST_NAME=="benson" & both$YEAR==1996 & both$JOURNAL=="bitr"]<-"universidade estadual de campinas"
both$INST[both$LAST_NAME=="berry" & both$YEAR==1996 & both$JOURNAL=="bitr"]<-"missouri botanical garden"
both$INST[both$LAST_NAME=="berry" & both$YEAR==1997 & both$JOURNAL=="bitr"]<-"missouri botanical garden"
both$INST[both$LAST_NAME=="brown" & both$YEAR==1993 & both$JOURNAL=="bitr"]<-"university of illinois urbana champaign"
both$INST[both$LAST_NAME=="brown" & both$YEAR==1996 & both$JOURNAL=="bitr"]<-"university of illinois urbana champaign"
both$INST[both$LAST_NAME=="foster" & both$YEAR==1996 & both$JOURNAL=="bitr"]<-"usgs patuxent wildlife research center"
both$INST[both$LAST_NAME=="fox" & both$YEAR==1993 & both$JOURNAL=="bitr"]<-"curtin university of technology"
both$INST[both$LAST_NAME=="fox" & both$YEAR==1997 & both$JOURNAL=="bitr"]<-"curtin university of technology"
both$INST[both$LAST_NAME=="west-eberhard" & both$YEAR==1993 & both$JOURNAL=="bitr"]<-"smithsonian tropical research institute"
both$INST[both$LAST_NAME=="west-eberhard" & both$YEAR==1996 & both$JOURNAL=="bitr"]<-"smithsonian tropical research institute"
both$INST[both$LAST_NAME=="angert" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of british columbia"
both$INST[both$LAST_NAME=="azevedo" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of houston"
both$INST[both$LAST_NAME=="case" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"kent state university"
both$INST[both$LAST_NAME=="dean" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of minnesota"
both$INST[both$LAST_NAME=="dworkin" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"michigan state university"
both$INST[both$LAST_NAME=="edmands" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of southern california"
both$INST[both$LAST_NAME=="engelstadter" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of queensland"
both$INST[both$LAST_NAME=="evans" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"monash university"
both$INST[both$LAST_NAME=="friedman" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of oxford"
both$INST[both$LAST_NAME=="hadfield" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of edinburgh"
both$INST[both$LAST_NAME=="hahn" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"indiana university"
both$INST[both$LAST_NAME=="hall" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of georgia"
both$INST[both$LAST_NAME=="kisdi" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of helsinki"
both$INST[both$LAST_NAME=="laine" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of helsinki"
both$INST[both$LAST_NAME=="marshall" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"monash university"
both$INST[both$LAST_NAME=="masel" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of arizona"
both$INST[both$LAST_NAME=="mcadam" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of guelph"
both$INST[both$LAST_NAME=="neiman" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of iowa"
both$INST[both$LAST_NAME=="redfield" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of british columbia"
both$INST[both$LAST_NAME=="robosky" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of michigan"
both$INST[both$LAST_NAME=="roze" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"cnrs roscoff biological station"
both$INST[both$LAST_NAME=="rozen" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of leiden"
both$INST[both$LAST_NAME=="sweigart" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of georgia"
both$INST[both$LAST_NAME=="tobias" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"university of oxford"
both$INST[both$LAST_NAME=="valenzuela" & both$YEAR==2015 & both$JOURNAL=="evol"]<-"iowa state university"
both$INST[both$LAST_NAME=="bearhop" & both$YEAR==2008 & both$JOURNAL=="jane"]<-"university of exeter"
both$INST[both$LAST_NAME=="bearhop" & both$YEAR==2014 & both$JOURNAL=="jane"]<-"university of exeter"
both$INST[both$LAST_NAME=="gurney" & both$YEAR==1999 & both$JOURNAL=="jane"]<-"university of strathclyde"
both$INST[both$LAST_NAME=="gurney" & both$YEAR==2014 & both$JOURNAL=="jane"]<-"university of strathclyde"
both$INST[both$LAST_NAME=="hall" & both$YEAR==1999 & both$JOURNAL=="jane"]<-"flinders university"
both$INST[both$LAST_NAME=="hall" & both$YEAR==2009 & both$JOURNAL=="jane"]<-"worldfish centre"
both$INST[both$LAST_NAME=="lessells" & both$YEAR==1994 & both$JOURNAL=="jane"]<-"netherlands institute of ecology"
both$INST[both$LAST_NAME=="lessells" & both$YEAR==2014 & both$JOURNAL=="jane"]<-"netherlands institute of ecology"
both$INST[both$LAST_NAME=="manly" & both$YEAR==2005 & both$JOURNAL=="jane"]<-"western ecosystem technology"
both$INST[both$LAST_NAME=="manly" & both$YEAR==2013 & both$JOURNAL=="jane"]<-"western ecosystem technology"
both$INST[both$LAST_NAME=="may" & both$YEAR==1991 & both$JOURNAL=="jane"]<-"university of oxford"
both$INST[both$LAST_NAME=="may" & both$YEAR==1996 & both$JOURNAL=="jane"]<-"university of oxford"
both$INST[both$LAST_NAME=="mccann" & both$YEAR==2005 & both$JOURNAL=="jane"]<-"university of guelph"
both$INST[both$LAST_NAME=="mccleery" & both$YEAR==2008 & both$JOURNAL=="jane"]<-"university of oxford"
both$INST[both$LAST_NAME=="mcintyre" & both$YEAR==1985 & both$JOURNAL=="jane"]<-"university of aberdeen"
both$INST[both$LAST_NAME=="mcintyre" & both$YEAR==1990 & both$JOURNAL=="jane"]<-"university of aberdeen"
both$INST[both$LAST_NAME=="meiri" & both$YEAR==2011 & both$JOURNAL=="jane"]<-"tel aviv university"
both$INST[both$LAST_NAME=="meiri" & both$YEAR==2014 & both$JOURNAL=="jane"]<-"tel aviv university"
both$INST[both$LAST_NAME=="o'gorman" & both$YEAR==2013 & both$JOURNAL=="jane"]<-"queen mary university of london"
both$INST[both$LAST_NAME=="o'gorman" & both$YEAR==2014 & both$JOURNAL=="jane"]<-"imperial college london"
both$INST[both$LAST_NAME=="rogers" & both$YEAR==1994 & both$JOURNAL=="jane"]<-"university of oxford"
both$INST[both$LAST_NAME=="rogers" & both$YEAR==1996 & both$JOURNAL=="jane"]<-"university of oxford"
both$INST[both$LAST_NAME=="stouffer" & both$YEAR==2013 & both$JOURNAL=="jane"]<-"university of canterbury"
both$INST[both$LAST_NAME=="stouffer" & both$YEAR==2014 & both$JOURNAL=="jane"]<-"university of canterbury"
both$INST[both$LAST_NAME=="thorpe" & both$YEAR==1994 & both$JOURNAL=="jane"]<-"soafd fisheries laboratory"
both$INST[both$LAST_NAME=="thorpe" & both$YEAR==1997 & both$JOURNAL=="jane"]<-"university of glasgow "
both$INST[both$LAST_NAME=="vanderpol" & both$YEAR==2013 & both$JOURNAL=="jane"]<-"australian national university"
both$INST[both$LAST_NAME=="vanderpol" & both$YEAR==2014 & both$JOURNAL=="jane"]<-"australian national university"
both$INST[both$LAST_NAME=="block" & both$YEAR==1985 & both$JOURNAL=="jape"]<-"liverpool john moores university"
both$INST[both$LAST_NAME=="block" & both$YEAR==1989 & both$JOURNAL=="jape"]<-"liverpool john moores university"
both$INST[both$LAST_NAME=="bullock" & both$YEAR==2000 & both$JOURNAL=="jape"]<-"institute of terrestrial ecology"
both$INST[both$LAST_NAME=="bullock" & both$YEAR==2003 & both$JOURNAL=="jape"]<-"institute of terrestrial ecology"
both$INST[both$LAST_NAME=="clarke" & both$YEAR==1989 & both$JOURNAL=="jape"]<-"institute of terrestrial ecology"
both$INST[both$LAST_NAME=="day" & both$YEAR==1988 & both$JOURNAL=="jape"]<-"silsoe research institute"
both$INST[both$LAST_NAME=="day" & both$YEAR==1996 & both$JOURNAL=="jape"]<-"silsoe research institute"
both$INST[both$LAST_NAME=="fernande-juricic" & both$YEAR==2010 & both$JOURNAL=="jape"]<-"purdue university"
both$INST[both$LAST_NAME=="freckleto" & both$YEAR==2005 & both$JOURNAL=="jape"]<-"university of oxford"
both$INST[both$LAST_NAME=="hill" & both$YEAR==1993 & both$JOURNAL=="jape"]<-"game conservancy"
both$INST[both$LAST_NAME=="hill" & both$YEAR==1999 & both$JOURNAL=="jape"]<-"university of cambridge"
both$INST[both$LAST_NAME=="mason" & both$YEAR==1989 & both$JOURNAL=="jape"]<-"university of essex"
both$INST[both$LAST_NAME=="mason" & both$YEAR==1996 & both$JOURNAL=="jape"]<-"university of essex"
both$INST[both$LAST_NAME=="mead" & both$YEAR==1985 & both$JOURNAL=="jape"]<-"university of reading"
both$INST[both$LAST_NAME=="mead" & both$YEAR==1987 & both$JOURNAL=="jape"]<-"university of reading"
both$INST[both$LAST_NAME=="miles" & both$YEAR==1988 & both$JOURNAL=="jape"]<-"institute of terrestrial ecology"
both$INST[both$LAST_NAME=="miles" & both$YEAR==1991 & both$JOURNAL=="jape"]<-"institute of terrestrial ecology"
both$INST[both$LAST_NAME=="monteith" & both$YEAR==1985 & both$JOURNAL=="jape"]<-"university of nottingham"
both$INST[both$LAST_NAME=="monteith" & both$YEAR==1987 & both$JOURNAL=="jape"]<-"university of nottingham"
both$INST[both$LAST_NAME=="roberts" & both$YEAR==1985 & both$JOURNAL=="jape"]<-"central electricity research laboratories"
both$INST[both$LAST_NAME=="roberts" & both$YEAR==1987 & both$JOURNAL=="jape"]<-"central electricity research laboratories"
both$INST[both$LAST_NAME=="rutter" & both$YEAR==1985 & both$JOURNAL=="jape"]<-"imperial college london"
both$INST[both$LAST_NAME=="rutter" & both$YEAR==1988 & both$JOURNAL=="jape"]<-"imperial college london"
both$INST[both$LAST_NAME=="smith" & both$YEAR==2005 & both$JOURNAL=="jape"]<-"central science laboratory"
both$INST[both$LAST_NAME=="smith" & both$YEAR==2006 & both$JOURNAL=="jape"]<-"central science laboratory"
both$INST[both$LAST_NAME=="thomas" & both$YEAR==1989 & both$JOURNAL=="jape"]<-"afrc institute for grassland and animal production"
both$INST[both$LAST_NAME=="thomas" & both$YEAR==1997 & both$JOURNAL=="jape"]<-"afrc institute for grassland and animal production"
both$INST[both$LAST_NAME=="walpole" & both$YEAR==2006 & both$JOURNAL=="jape"]<-"fauna and flora international"
both$INST[both$LAST_NAME=="walpole" & both$YEAR==2009 & both$JOURNAL=="jape"]<-"fauna and flora international"
both$INST[both$LAST_NAME=="welch" & both$YEAR==1988 & both$JOURNAL=="jape"]<-"institute of terrestrial ecology"
both$INST[both$LAST_NAME=="welch" & both$YEAR==1996 & both$JOURNAL=="jape"]<-"institute of terrestrial ecology"
both$INST[both$LAST_NAME=="ali" & both$YEAR==2013 & both$JOURNAL=="jbiog"]<-"university of hong kong"
both$INST[both$LAST_NAME=="ali" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"university of hong kong"
both$INST[both$LAST_NAME=="bryson" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"university of washington"
both$INST[both$LAST_NAME=="carine" & both$YEAR==2010 & both$JOURNAL=="jbiog"]<-"natural history museum london"
both$INST[both$LAST_NAME=="carine" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"natural history museum london"
both$INST[both$LAST_NAME=="cavers" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"nerc centre for ecology and hydrology"
both$INST[both$LAST_NAME=="chapman" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"nerc centre for ecology and hydrology"
both$INST[both$LAST_NAME=="diniz-filho" & both$YEAR==2005 & both$JOURNAL=="jbiog"]<-"universidade federal de goias"
both$INST[both$LAST_NAME=="diniz-filho" & both$YEAR==2006 & both$JOURNAL=="jbiog"]<-"universidade federal de goias"
both$INST[both$LAST_NAME=="emerson" & both$YEAR==2013 & both$JOURNAL=="jbiog"]<-"csic - ipna"
both$INST[both$LAST_NAME=="emerson" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"csic - ipna"
both$INST[both$LAST_NAME=="gaither" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"durham university"
both$INST[both$LAST_NAME=="gilman" & both$YEAR==2010 & both$JOURNAL=="jbiog"]<-"auckland university of technology"
both$INST[both$LAST_NAME=="gilman" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"auckland university of technology"
both$INST[both$LAST_NAME=="guilhaumon" & both$YEAR==2013 & both$JOURNAL=="jbiog"]<-"university of evora"
both$INST[both$LAST_NAME=="guilhaumon" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"universite de montpellier"
both$INST[both$LAST_NAME=="hansen" & both$YEAR==2011 & both$JOURNAL=="jbiog"]<-"university of kwazulu natal"
both$INST[both$LAST_NAME=="hansen" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"university of zurich"
both$INST[both$LAST_NAME=="harrison" & both$YEAR==1985 & both$JOURNAL=="jbiog"]<-"university college london"
both$INST[both$LAST_NAME=="harrison" & both$YEAR==2004 & both$JOURNAL=="jbiog"]<-"university college london"
both$INST[both$LAST_NAME=="higgins" & both$YEAR==2013 & both$JOURNAL=="jbiog"]<-"goethe university frankfurt"
both$INST[both$LAST_NAME=="higgins" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"university of otago"
both$INST[both$LAST_NAME=="holger" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"university of gottingen"
both$INST[both$LAST_NAME=="kreft" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"university of gottingen"
both$INST[both$LAST_NAME=="holland" & both$YEAR==1986 & both$JOURNAL=="jbiog"]<-"university of otago"
both$INST[both$LAST_NAME=="holland" & both$YEAR==2003 & both$JOURNAL=="jbiog"]<-"university of otago"
both$INST[both$LAST_NAME=="jetz" & both$YEAR==2006 & both$JOURNAL=="jbiog"]<-"university of california san diego"
both$INST[both$LAST_NAME=="jetz" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"yale university"
both$INST[both$LAST_NAME=="katinas" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"museo de la plata"
both$INST[both$LAST_NAME=="kullman" & both$YEAR==1991 & both$JOURNAL=="jbiog"]<-"university of umea"
both$INST[both$LAST_NAME=="kullman" & both$YEAR==2004 & both$JOURNAL=="jbiog"]<-"university of umea"
both$INST[both$LAST_NAME=="lei" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"chinese academy of sciences"
both$INST[both$LAST_NAME=="maggs" & both$YEAR==2008 & both$JOURNAL=="jbiog"]<-"queens university belfast"
both$INST[both$LAST_NAME=="maggs" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"queens university belfast"
both$INST[both$LAST_NAME=="masters" & both$YEAR==2011 & both$JOURNAL=="jbiog"]<-"university of kwazulu natal"
both$INST[both$LAST_NAME=="masters" & both$YEAR==2012 & both$JOURNAL=="jbiog"]<-"university of ft hare"
both$INST[both$LAST_NAME=="parmakelis" & both$YEAR==2013 & both$JOURNAL=="jbiog"]<-"university of athens"
both$INST[both$LAST_NAME=="parmakelis" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"university of athens"
both$INST[both$LAST_NAME=="paulay" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"university of florida"
both$INST[both$LAST_NAME=="phillimore" & both$YEAR==2013 & both$JOURNAL=="jbiog"]<-"university of edinburgh"
both$INST[both$LAST_NAME=="phillimore" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"university of edinburgh"
both$INST[both$LAST_NAME=="prance" & both$YEAR==2000 & both$JOURNAL=="jbiog"]<-"royal botanic gardens kew"
both$INST[both$LAST_NAME=="prance" & both$YEAR==2004 & both$JOURNAL=="jbiog"]<-"royal botanic gardens kew"
both$INST[both$LAST_NAME=="richardson" & both$YEAR==2011 & both$JOURNAL=="jbiog"]<-"royal botanic garden edinburgh"
both$INST[both$LAST_NAME=="richardson" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"royal botanic garden edinburgh"
both$INST[both$LAST_NAME=="schaefer" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"technical university of munich"
both$INST[both$LAST_NAME=="stearns" & both$YEAR==1985 & both$JOURNAL=="jbiog"]<-"university of wisconsin"
both$INST[both$LAST_NAME=="stearns" & both$YEAR==1990 & both$JOURNAL=="jbiog"]<-"university of wisconsin"
both$INST[both$LAST_NAME=="triantis" & both$YEAR==2010 & both$JOURNAL=="jbiog"]<-"university of azores"
both$INST[both$LAST_NAME=="triantis" & both$YEAR==2014 & both$JOURNAL=="jbiog"]<-"national & kapodistrian university"
both$INST[both$LAST_NAME=="vanderhammen" & both$YEAR==1985 & both$JOURNAL=="jbiog"]<-"university of amsterdam"
both$INST[both$LAST_NAME=="vanderhammen" & both$YEAR==1990 & both$JOURNAL=="jbiog"]<-"university of amsterdam"
both$INST[both$LAST_NAME=="watts" & both$YEAR==1985 & both$JOURNAL=="jbiog"]<-"university of hull"
both$INST[both$LAST_NAME=="watts" & both$YEAR==1995 & both$JOURNAL=="jbiog"]<-"university of hull"
both$INST[both$LAST_NAME=="dekroon" & both$YEAR==2002 & both$JOURNAL=="jecol"]<-"radboud university nijmegen"
both$INST[both$LAST_NAME=="dekroon" & both$YEAR==2004 & both$JOURNAL=="jecol"]<-"radboud university nijmegen"
both$INST[both$LAST_NAME=="etherington" & both$YEAR==1985 & both$JOURNAL=="jecol"]<-"cardiff university"
both$INST[both$LAST_NAME=="etherington" & both$YEAR==1990 & both$JOURNAL=="jecol"]<-"university of wales"
both$INST[both$LAST_NAME=="gray" & both$YEAR==1991 & both$JOURNAL=="jecol"]<-"cardiff university"
both$INST[both$LAST_NAME=="hopkins" & both$YEAR==1985 & both$JOURNAL=="jecol"]<-"university of sheffield"
both$INST[both$LAST_NAME=="huntely" & both$YEAR==1987 & both$JOURNAL=="jecol"]<-"cambridge university"
both$INST[both$LAST_NAME=="huntley" & both$YEAR==1985 & both$JOURNAL=="jecol"]<-"cambridge university"
both$INST[both$LAST_NAME=="huntley" & both$YEAR==1988 & both$JOURNAL=="jecol"]<-"cambridge university"
both$INST[both$LAST_NAME=="lack" & both$YEAR==1987 & both$JOURNAL=="jecol"]<-"swansea university"
both$INST[both$LAST_NAME=="lack" & both$YEAR==1990 & both$JOURNAL=="jecol"]<-"oxford brookes university"
both$INST[both$LAST_NAME=="long" & both$YEAR==1986 & both$JOURNAL=="jecol"]<-"university of essex"
both$INST[both$LAST_NAME=="ouborg" & both$YEAR==2003 & both$JOURNAL=="jecol"]<-"radboud university nijmegen"
both$INST[both$LAST_NAME=="ouborg" & both$YEAR==2004 & both$JOURNAL=="jecol"]<-"radboud university nijmegen"
both$INST[both$LAST_NAME=="vandermeijden" & both$YEAR==1989 & both$JOURNAL=="jecol"]<-"leiden university"
both$INST[both$LAST_NAME=="vandermeijden" & both$YEAR==1993 & both$JOURNAL=="jecol"]<-"leiden university"
both$INST[both$LAST_NAME=="vangroenendael" & both$YEAR==1992 & both$JOURNAL=="jecol"]<-"wageningen university and research"
both$INST[both$LAST_NAME=="white" & both$YEAR==1986 & both$JOURNAL=="jecol"]<-"university college dublin"
both$INST[both$LAST_NAME=="albon" & both$YEAR==1995 & both$JOURNAL=="jzool"]<-"zoological society of london"
both$INST[both$LAST_NAME=="ayers" & both$YEAR==1985 & both$JOURNAL=="newphyt"]<-"lancaster university"
both$INST[both$LAST_NAME=="ayers" & both$YEAR==2002 & both$JOURNAL=="newphyt"]<-"lancaster university"
both$INST[both$LAST_NAME=="barbour" & both$YEAR==2015 & both$JOURNAL=="newphyt"]<-"university of sydney"
both$INST[both$LAST_NAME=="graham" & both$YEAR==2015 & both$JOURNAL=="newphyt"]<-"university of florida"
both$INST[both$LAST_NAME=="schat" & both$YEAR==2015 & both$JOURNAL=="newphyt"]<-"vrije universiteit amsterdam"
both$INST[both$LAST_NAME=="smith" & both$YEAR==2015 & both$JOURNAL=="newphyt"]<-"university of cambridge"
both$INST[both$LAST_NAME=="stinchcombe" & both$YEAR==2014 & both$JOURNAL=="newphyt"]<-"university of toronto"
both$INST[both$LAST_NAME=="sultan" & both$YEAR==2004 & both$JOURNAL=="newphyt"]<-"wesleyan university"
both$INST[both$LAST_NAME=="sultan" & both$YEAR==2009 & both$JOURNAL=="newphyt"]<-"wesleyan university"
both$INST[both$LAST_NAME=="syrett" & both$YEAR==1985 & both$JOURNAL=="newphyt"]<-"university of wales"
both$INST[both$LAST_NAME=="syrett" & both$YEAR==1989 & both$JOURNAL=="newphyt"]<-"university of wales"
both$INST[both$LAST_NAME=="tjoelker" & both$YEAR==2014 & both$JOURNAL=="newphyt"]<-"western sydney university"
both$INST[both$LAST_NAME=="vamosi" & both$YEAR==2015 & both$JOURNAL=="newphyt"]<-"university of calgary"
both$INST[both$LAST_NAME=="west" & both$YEAR==1994 & both$JOURNAL=="newphyt"]<-"university of cambridge"
both$INST[both$LAST_NAME=="wolfenden" & both$YEAR==1990 & both$JOURNAL=="newphyt"]<-"lancaster university"
both$INST[both$LAST_NAME=="wolfenden" & both$YEAR==1991 & both$JOURNAL=="newphyt"]<-"lancaster university"
both$INST[both$LAST_NAME=="wolfenden" & both$YEAR==1996 & both$JOURNAL=="newphyt"]<-"lancaster university"
both$INST[both$LAST_NAME=="bever" & both$YEAR==2013 & both$JOURNAL=="oecol"]<-"university of indiana"
both$INST[both$LAST_NAME=="buchmann" & both$YEAR==2004 & both$JOURNAL=="oecol"]<-"swiss federal institute of technology"
both$INST[both$LAST_NAME=="buchmann" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"swiss federal institute of technology"
both$INST[both$LAST_NAME=="diehl" & both$YEAR==2011 & both$JOURNAL=="oecol"]<-"university of umea"
both$INST[both$LAST_NAME=="elle" & both$YEAR==2013 & both$JOURNAL=="oecol"]<-"simon fraser university"
both$INST[both$LAST_NAME=="fiedler" & both$YEAR==2005 & both$JOURNAL=="oecol"]<-"university of vienna"
both$INST[both$LAST_NAME=="fiedler" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"university of vienna"
both$INST[both$LAST_NAME=="gough" & both$YEAR==2013 & both$JOURNAL=="oecol"]<-"university of texas arlington"
both$INST[both$LAST_NAME=="gough" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"university of texas arlington"
both$INST[both$LAST_NAME=="heimpel" & both$YEAR==2013 & both$JOURNAL=="oecol"]<-"university of minnesota"
both$INST[both$LAST_NAME=="heimpel" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"university of minnesota"
both$INST[both$LAST_NAME=="herre" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"smithsonian tropical research institute"
both$INST[both$LAST_NAME=="ibanez" & both$YEAR==2013 & both$JOURNAL=="oecol"]<-"university of michigan"
both$INST[both$LAST_NAME=="ibanez" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"university of michigan"
both$INST[both$LAST_NAME=="koricheva" & both$YEAR==2006 & both$JOURNAL=="oecol"]<-"royal holloway university of london"
both$INST[both$LAST_NAME=="koricheva" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"royal holloway university of london"
both$INST[both$LAST_NAME=="korner" & both$YEAR==1992 & both$JOURNAL=="oecol"]<-"university of basel"
both$INST[both$LAST_NAME=="korner" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"university of basel"
both$INST[both$LAST_NAME=="laaksonen" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"university of turku "
both$INST[both$LAST_NAME=="layman" & both$YEAR==2013 & both$JOURNAL=="oecol"]<-"florida international university"
both$INST[both$LAST_NAME=="layman" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"florida international university"
both$INST[both$LAST_NAME=="legalliard" & both$YEAR==2012 & both$JOURNAL=="oecol"]<-"cnrs institut ecologie et environnement"
both$INST[both$LAST_NAME=="legalliard" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"cnrs institut ecologie et environnement"
both$INST[both$LAST_NAME=="lichstein" & both$YEAR==2013 & both$JOURNAL=="oecol"]<-"university of florida"
both$INST[both$LAST_NAME=="lichstein" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"university of florida"
both$INST[both$LAST_NAME=="lill" & both$YEAR==2012 & both$JOURNAL=="oecol"]<-"george washington university"
both$INST[both$LAST_NAME=="lill" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"george washington university"
both$INST[both$LAST_NAME=="muller" & both$YEAR==2011 & both$JOURNAL=="oecol"]<-"university of bielefeld"
both$INST[both$LAST_NAME=="muller" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"university of bielefeld"
both$INST[both$LAST_NAME=="niinemets" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"estonian university of life sciences"
both$INST[both$LAST_NAME=="prinzing" & both$YEAR==2013 & both$JOURNAL=="oecol"]<-"universite de rennes 1"
both$INST[both$LAST_NAME=="schaller" & both$YEAR==1985 & both$JOURNAL=="oecol"]<-"university of vienna"
both$INST[both$LAST_NAME=="schaller" & both$YEAR==1986 & both$JOURNAL=="oecol"]<-"university of vienna"
both$INST[both$LAST_NAME=="shurin" & both$YEAR==2011 & both$JOURNAL=="oecol"]<-"university of california san diego"
both$INST[both$LAST_NAME=="shurin" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"university of california san diego"
both$INST[both$LAST_NAME=="siemann" & both$YEAR==2012 & both$JOURNAL=="oecol"]<-"rice university"
both$INST[both$LAST_NAME=="siemann" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"rice university"
both$INST[both$LAST_NAME=="stark" & both$YEAR==2012 & both$JOURNAL=="oecol"]<-"utah state university"
both$INST[both$LAST_NAME=="stark" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"utah state university"
both$INST[both$LAST_NAME=="stewart" & both$YEAR==1998 & both$JOURNAL=="oecol"]<-"university of queensland"
both$INST[both$LAST_NAME=="ward" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"university of kansas"
both$INST[both$LAST_NAME=="weisser" & both$YEAR==1985 & both$JOURNAL=="oecol"]<-"university of innsbruck "
both$INST[both$LAST_NAME=="weisser" & both$YEAR==2014 & both$JOURNAL=="oecol"]<-"technical university of munich"
both$INST[both$LAST_NAME=="ziegler" & both$YEAR==1985 & both$JOURNAL=="oecol"]<-"technical university of munich"
both$INST[both$LAST_NAME=="ziegler" & both$YEAR==2007 & both$JOURNAL=="oecol"]<-"technical university of munich"
both$INST[both$LAST_NAME=="aarsen" & both$YEAR==2007 & both$JOURNAL=="oikos"]<-"queens university"
both$INST[both$LAST_NAME=="aarsen" & both$YEAR==2014 & both$JOURNAL=="oikos"]<-"queens university"
both$INST[both$LAST_NAME=="abbot" & both$YEAR==2013 & both$JOURNAL=="oikos"]<-"lund university"
both$INST[both$LAST_NAME=="abbot" & both$YEAR==2014 & both$JOURNAL=="oikos"]<-"lund university"
both$INST[both$LAST_NAME=="andersen" & both$YEAR==1985 & both$JOURNAL=="oikos"]<-"geological survey of denmark"
both$INST[both$LAST_NAME=="andersen" & both$YEAR==1989 & both$JOURNAL=="oikos"]<-"geological survey of denmark"
both$INST[both$LAST_NAME=="andersson" & both$YEAR==1985 & both$JOURNAL=="oikos"]<-"swedish university of agricultural sciences"
both$INST[both$LAST_NAME=="andersson" & both$YEAR==1989 & both$JOURNAL=="oikos"]<-"swedish university of agricultural sciences"
both$INST[both$LAST_NAME=="boomsma" & both$YEAR==2011 & both$JOURNAL=="oikos"]<-"university of copenhagen"
both$INST[both$LAST_NAME=="dahl" & both$YEAR==1985 & both$JOURNAL=="oikos"]<-"agricultural university of norway"
both$INST[both$LAST_NAME=="dahl" & both$YEAR==1989 & both$JOURNAL=="oikos"]<-"agricultural university of norway"
both$INST[both$LAST_NAME=="grae" & both$YEAR==2012 & both$JOURNAL=="oikos"]<-"norwegian university of science and technology"
both$INST[both$LAST_NAME=="grae" & both$YEAR==2014 & both$JOURNAL=="oikos"]<-"norwegian university of science and technology"
both$INST[both$LAST_NAME=="jarvinen" & both$YEAR==1989 & both$JOURNAL=="oikos"]<-"university of helsinki"
both$INST[both$LAST_NAME=="malian" & both$YEAR==2012 & both$JOURNAL=="oikos"]<-"swiss federal institute of aquatic science and technology"
both$INST[both$LAST_NAME=="malian" & both$YEAR==2014 & both$JOURNAL=="oikos"]<-"swiss federal institute of aquatic science and technology"
both$INST[both$LAST_NAME=="moore" & both$YEAR==2011 & both$JOURNAL=="oikos"]<-"university of california santa cruz"
both$INST[both$LAST_NAME=="moore" & both$YEAR==2014 & both$JOURNAL=="oikos"]<-"university of california santa cruz"
both$INST[both$LAST_NAME=="robinson" & both$YEAR==2011 & both$JOURNAL=="oikos"]<-"british trust for ornithology"
both$INST[both$LAST_NAME=="robinson" & both$YEAR==2014 & both$JOURNAL=="oikos"]<-"british trust for ornithology"
both$INST[both$LAST_NAME=="roy" & both$YEAR==2012 & both$JOURNAL=="oikos"]<-"evergreen state college"
both$INST[both$LAST_NAME=="roy" & both$YEAR==2014 & both$JOURNAL=="oikos"]<-"evergreen state college"
both$INST[both$LAST_NAME=="scherer_lorenzon" & both$YEAR==2014 & both$JOURNAL=="oikos"]<-"university of freiburg"
both$INST[both$LAST_NAME=="solbrek" & both$YEAR==2011 & both$JOURNAL=="oikos"]<-"swedish university of agricultural sciences"
both$INST[both$LAST_NAME=="sun" & both$YEAR==2014 & both$JOURNAL=="oikos"]<-"university of hong kong"
both$INST[both$LAST_NAME=="traveser" & both$YEAR==2011 & both$JOURNAL=="oikos"]<-"mediterranean institute for advanced studies"
both$INST[both$LAST_NAME=="traveser" & both$YEAR==2014 & both$JOURNAL=="oikos"]<-"mediterranean institute for advanced studies"
both$INST[both$LAST_NAME=="bornkamm" & both$YEAR==1985 & both$JOURNAL=="plantecol"]<-"technical university of berlin"
both$INST[both$LAST_NAME=="bornkamm" & both$YEAR==1989 & both$JOURNAL=="plantecol"]<-"technical university of berlin"
both$INST[both$LAST_NAME=="epstein" & both$YEAR==2012 & both$JOURNAL=="plantecol"]<-"university of virginia"
both$INST[both$LAST_NAME=="gimingham" & both$YEAR==1985 & both$JOURNAL=="plantecol"]<-"university of aberdeen"
both$INST[both$LAST_NAME=="gimingham" & both$YEAR==1989 & both$JOURNAL=="plantecol"]<-"university of aberdeen"
both$INST[both$LAST_NAME=="grubb" & both$YEAR==1985 & both$JOURNAL=="plantecol"]<-"university of cambridge"
both$INST[both$LAST_NAME=="grubb" & both$YEAR==1986 & both$JOURNAL=="plantecol"]<-"university of cambridge"
both$INST[both$LAST_NAME=="ne'eman" & both$YEAR==2012 & both$JOURNAL=="plantecol"]<-"university of haifa-oranim"
both$INST[both$LAST_NAME=="oksanen" & both$YEAR==1989 & both$JOURNAL=="plantecol"]<-"university of eastern finland"
both$INST[both$LAST_NAME=="rozema" & both$YEAR==2012 & both$JOURNAL=="plantecol"]<-"vrije universiteit amsterdam"
both$INST[both$LAST_NAME=="walsh" & both$YEAR==2012 & both$JOURNAL=="plantecol"]<-"university of north carolina chapel hill"
both$INST[both$LAST_NAME=="white" & both$YEAR==1985 & both$JOURNAL=="plantecol"]<-"university college dublin"
both$INST[both$LAST_NAME=="white" & both$YEAR==1989 & both$JOURNAL=="plantecol"]<-"university college dublin"

both$INST[both$editor_id==1747 & both$YEAR==2011 & both$JOURNAL=="jbiog"]<-"university of oxford"

both$INST[both$editor_id==3892 & both$YEAR==1985]<-"louisiana state university"

both$INST[both$editor_id==716 & both$LAST_NAME=="hansen" & both$JOURNAL=="jbiog"]<-"university of zurich"
both$COUNTRY[both$editor_id==716 & both$LAST_NAME=="hansen" & both$JOURNAL=="jbiog"]<-"switzerland"
both$INST[both$editor_id==1849 & both$INST=="university of stirling"]<-"vrije universiteit amsterdam"
# both$INST[both$editor_id==105 & both$JOURNAL=="agronomy"]<-"california state university fresno"
# both$INST[both$editor_id==1673 & both$YEAR==2014]<-"california state university sacramento"

both$editor_id[both$JOURNAL=="gcb" & both$LAST_NAME=="korner" & both$FIRST_NAME=="christian"]<-499
both$INST[both$editor_id==499 & both$YEAR>1989]<-"university of basel"
both$CITY[both$CITY=="turtu"& both$editor_id==3587]<-"turku"

both$CITY[both$CITY=="universidade estadual paulista"& both$editor_id==2383]<-"rio claro"
both$CITY[both$CITY=="universidade estadual paulista"& both$editor_id==1225]<-"sao paulo"

both$INST[both$editor_id==2358 & both$JOURNAL=="jane"]<-"university college cork"
both$CITY[both$editor_id==2358 & both$JOURNAL=="jane"]<-"cork"

both$INST[both$editor_id==1417 & both$YEAR=="1992"]<-"usfs pacific southwest research station"

both$INST[both$editor_id==190]<-"kansas state university"

both$INST[both$editor_id==1763]<-"universidade de sao paulo"
both$INST[both$editor_id==2819]<-"universidade de sao paulo"

both$STATE[both$CITY=="para"]<-"para"
both$CITY[both$CITY=="para"]<-NA

both$CITY[both$CITY=="washington"]<-"washington dc"
both$CITY[both$CITY=="washington, dc"]<-"washington dc"
both$CITY[both$CITY=="dc"]<-"washington dc"
both$CITY[both$CITY=="district of columbia"]<-"washington dc"
both$STATE[both$CITY=="washington dc"]<-"washington dc"

both$CITY[both$CITY=="st louis"]<-"st louis"

both$CITY[both$CITY=="fredricton"]<-"fredericton"


both$COUNTRY[both$editor_id==3359 & both$YEAR==2014]<-"new zealand"


both$COUNTRY[both$COUNTRY=="west germany"]<-"germany"


colnames(both)
both<-both %>% group_by(JOURNAL,editor_id,LAST_NAME,FIRST_NAME) %>% 
  arrange(YEAR) %>% 
  fill(INST,.direction="down")


both$FIRST_NAME[both$LAST_NAME=="lack" & both$FIRST_NAME=="alan" &
                  both$MIDDLE_NAME=="j"]<-"andrew"


both$FIRST_NAME[both$LAST_NAME=="lomolino" & both$FIRST_NAME=="m" &
                  both$INST=="university of oklahoma"]<-"mark"

both$MIDDLE_NAME[both$LAST_NAME=="lomolino" & both$FIRST_NAME=="mark" &
                   both$INST=="university of oklahoma"]<-"v"


both$FIRST_NAME[both$LAST_NAME=="houck" & both$FIRST_NAME=="lynn" &
                  both$JOURNAL=="amnat"]<-"lynne"


both$FIRST_NAME[both$LAST_NAME=="dustin" & both$FIRST_NAME=="marshall" &
                  both$JOURNAL=="evol" & both$YEAR==2014]<-"dustin"


both$LAST_NAME[both$LAST_NAME=="hannson" & both$FIRST_NAME=="l" & both$JOURNAL=="leco"]<-"hansson"
both$FIRST_NAME[both$LAST_NAME=="hansson" & both$FIRST_NAME=="l" & both$JOURNAL=="leco"]<-"lennart"
both$FIRST_NAME[both$LAST_NAME=="wagner" & both$FIRST_NAME=="h" & both$JOURNAL=="leco"]<-"helene"
both$LAST_NAME[both$LAST_NAME=="aarsen" & both$editor_id=="2119"]<-"aarssen"
both$LAST_NAME[both$LAST_NAME=="tilma" & both$editor_id=="891"]<-"tilman"
both$FIRST_NAME[both$editor_id=="570"]<-"christer"
both$LAST_NAME[both$editor_id=="570"]<-"solbreck"
both$UNIT[both$editor_id=="1605"]<-"citrus res & educ ctr"
both$LAST_NAME[both$editor_id=="2760"]<-"ayres"
both$LAST_NAME[both$LAST_NAME=="freckleto" & both$JOURNAL=="jape"]<-"freckelton"
both$editor_id[both$LAST_NAME=="freckleto" & both$JOURNAL=="jape"]<-"3063"
both$FIRST_NAME[both$LAST_NAME=="boomsma" & both$JOURNAL=="oikos"]<-"jacobus"

both$FIRST_NAME[both$LAST_NAME=="ouborg" & both$JOURNAL=="jecol"]<-"n"
both$MIDDLE_NAME[both$LAST_NAME=="ouborg" & both$JOURNAL=="jecol"]<-"joop"
both$NOTES[both$LAST_NAME=="block" & both$editor_id=="3714"]<-"from 1993 record"
both$LAST_NAME[both$editor_id=="283"]<-"graae"
both$FIRST_NAME[both$LAST_NAME=="ward" & both$JOURNAL=="oecol"]<-"joy"
both$editor_id[both$LAST_NAME=="ward" & both$JOURNAL=="oecol"]<-"1938"
both$CITY[both$LAST_NAME=="ward" & both$JOURNAL=="oecol"]<-"lawrence"
both$LAST_NAME[both$editor_id=="283"]<-"graae"
both$FIRST_NAME[both$LAST_NAME=="ward" & both$JOURNAL=="oecol"]<-"joy"

both$LAST_NAME[both$editor_id=="564"]<-"leroy"
both$FIRST_NAME[both$editor_id=="564"]<-"carri"
both$MIDDLE_NAME[both$editor_id=="564"]<-NA
both$LAST_NAME[both$editor_id=="2552"]<-"scherer-lorenzen"
both$LAST_NAME[both$editor_id=="519"]<-"melian"
both$FIRST_NAME[both$editor_id=="1323"]<-"holger"
both$LAST_NAME[both$editor_id=="1323"]<-"kreft"
both$LAST_NAME[both$editor_id=="217"]<-"traveset"
both$NOTES[both$editor_id=="102"& both$JOURNAL=="oecol"]<-"city listed as washington dc in frontmatter"
both$FIRST_NAME[both$editor_id=="2672" & both$JOURNAL=="biocon"]<-"n"
both$MIDDLE_NAME[both$editor_id=="2672" & both$JOURNAL=="biocon"]<-"c"

both$FIRST_NAME[both$editor_id=="753" & both$JOURNAL=="biocon"]<-"d"
both$MIDDLE_NAME[both$editor_id=="753" & both$JOURNAL=="biocon"]<-"j"

both$LAST_NAME[both$editor_id=="2154" & both$JOURNAL=="jbiog"]<-"gillman" 
both$MIDDLE_NAME[both$FIRST_NAME == "par" & both$LAST_NAME=="hockey"]<-"ar" 
both$FIRST_NAME[both$FIRST_NAME == "par" & both$LAST_NAME=="hockey"]<-"p"

both$INST[(both$editor_id=="3252" |both$editor_id=="453"|both$editor_id=="1973") & both$INST=="queensland"]<-"university of queensland"
both$INST[both$LAST_NAME=="vellend" & both$JOURNAL=="oikos"]<-"cornell university"
both$INST[both$INST=="wyoming" & both$LAST_NAME=="benkman"]<-"university of wyoming"
both$INST[both$JOURNAL=="funecol" & both$LAST_NAME=="blanckenhorn"]<-"university of zurich irchel"
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

both$COUNTRY[both$LAST_NAME=="laaksonen" & both$JOURNAL=="oecol"]<-"finland"
both$INST[both$LAST_NAME=="angilletta" & both$INST=="indiana"]<-"indiana state university"
both$INST[both$LAST_NAME=="reynolds" & both$INST=="indiana"]<-"indiana university bloomington"
both$INST[both$LAST_NAME=="pienkowski" & both$COUNTRY=="united kingdom"]<-"joint nature conservation committee"

both$INST[both$LAST_NAME=="cavers" & both$INST=="nerc centre for ecology and hydrology"]<-"nerc centre for ecology and hydrology edinburgh"
both$INST[both$LAST_NAME=="chapman" & both$INST=="nerc centre for ecology and hydrology"]<-"nerc centre for ecology and hydrology edinburgh"
both$INST[both$CITY=="wallingford" & both$INST=="nerc centre for ecology and hydrology"]<-"nerc centre for ecology and hydrology wallingford"
both$INST[both$CITY=="bailrigg" & both$INST=="nerc centre for ecology and hydrology"]<-"nerc centre for ecology and hydrology bailrigg"


both$INST[both$LAST_NAME=="croxall"]<-"nerc british antarctic survey"
both$INST[both$LAST_NAME=="pywell" & both$UNIT=="center for ecology and hydrology"]<-"nerc centre for ecology and hydrology wallingford"


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




both$LAST_NAME[both$LAST_NAME=="van der maarel" & both$JOURNAL=="leco"]<-"vandermaarel"
both$LAST_NAME[both$LAST_NAME=="vander" & both$JOURNAL=="leco"]<-"vandermaarel"
both$INST[both$LAST_NAME=="vandermaarel" & both$JOURNAL=="leco"]<-"university of uppsala"
	
both$INST[both$LAST_NAME=="cymerman" & both$JOURNAL=="leco"]<-"university of georgia"
both$CITY[both$LAST_NAME=="cymerman" & both$JOURNAL=="leco"]<-"athens"
both$LAST_NAME[both$LAST_NAME=="cymerman" & both$JOURNAL=="leco"]<-"hepinstall-cymerman"


both$INST[both$editor_id==381 & both$JOURNAL=="leco" & both$YEAR==2015]<-"leibniz university hannover"
both$INST[both$editor_id==1480 & both$JOURNAL=="leco" & both$YEAR==2004]<-"roskilde university"
both$INST[both$editor_id==2316 & both$JOURNAL=="leco" & both$YEAR==2015]<-"swiss federal institute for forest snow and landscape research wsl"
both$INST[both$editor_id==1990 & both$JOURNAL=="leco" & both$YEAR==2015]<-"michigan state university"
both$INST[both$editor_id==1520 & both$JOURNAL=="leco" & both$YEAR==2015]<-"north carolina state university"
both$INST[both$editor_id==448 & both$JOURNAL=="leco" & both$YEAR==2004]<-"usfs rocky mountain research station"
both$INST[both$editor_id==3067 & both$JOURNAL=="leco" & both$YEAR==1997]<-"appalachian environmental laboratory"
both$INST[both$editor_id==2387 & both$JOURNAL=="leco" & (both$YEAR>=1987 & both$YEAR<=1992)]<-"institut botanique"
# both$INST[both$editor_id==2387 & both$JOURNAL=="leco" & both$YEAR==1992]<-"institut botanique"
both$INST[both$editor_id==1192 & both$JOURNAL=="leco" & (both$YEAR>=1987 & both$YEAR<=1996)]<-"institute geography and geoecology"
# both$INST[both$editor_id==1192 & both$JOURNAL=="leco" & both$YEAR==1996]<-"institute geography and geoecology"
both$INST[both$editor_id==3744 & both$JOURNAL=="leco" & (both$YEAR>=1987 & both$YEAR<=1997)]<-"technical university of munich"
# both$INST[both$editor_id==3744 & both$JOURNAL=="leco" & both$YEAR==1997]<-"technical university of munich"
both$INST[both$editor_id==1080 & both$JOURNAL=="leco" & both$YEAR==2015]<-"agroscope"
both$COUNTRY[both$editor_id==1080 & both$JOURNAL=="leco" & both$YEAR==2015]<-"netherlands"
both$INST[both$editor_id==492 & both$JOURNAL=="leco" & both$YEAR==1997]<-"south dakota state university"
both$INST[both$editor_id==1089 & both$JOURNAL=="leco" & both$YEAR==2004]<-"swiss federal institute for forest snow and landscape research wsl"
both$INST[both$editor_id==3126 & both$JOURNAL=="leco" & both$YEAR==2015]<-"university of bari"
both$INST[both$editor_id==3592 & both$JOURNAL=="leco" & both$YEAR==2015]<-"university of richmond"
both$LAST_NAME[both$editor_id==3592 & both$JOURNAL=="leco" & both$YEAR==2015]<-"lookingbill"
both$INST[both$editor_id==2674 & both$JOURNAL=="leco" & (both$YEAR>=1993 & both$YEAR<=1997)]<-"hiroshima university"
# both$INST[both$editor_id==2674 & both$JOURNAL=="leco" & both$YEAR==1997]<-"hiroshima university"
both$INST[both$editor_id==1781 & both$JOURNAL=="leco" & both$YEAR==2004]<-"university of michigan"
both$INST[both$editor_id==2885 & both$JOURNAL=="leco" & both$YEAR==1993]<-"institute for forestry and nature research"
both$INST[both$editor_id==2885 & both$JOURNAL=="leco" & both$YEAR==1997]<-"institute for forestry and nature research"
both$INST[both$editor_id==2885 & both$JOURNAL=="leco" & both$YEAR==2004]<-"alterra research institute for the green world"
both$INST[both$editor_id==177 & both$JOURNAL=="leco" & (both$YEAR>=1987 & both$YEAR<=1997)]<-"ets ingenieros de montes"
# both$INST[both$editor_id==177 & both$JOURNAL=="leco" & both$YEAR==1997]<-"ets ingenieros de montes"
both$INST[both$editor_id==2907 & both$JOURNAL=="leco" & (both$YEAR>=1987 & both$YEAR<=1992)]<-"university of new mexico"
# both$INST[both$editor_id==2907 & both$JOURNAL=="leco" & both$YEAR==1992]<-"university of new mexico"
both$INST[both$editor_id==3791 & both$JOURNAL=="leco" & both$YEAR==2004]<-"colorado state university"
both$INST[both$editor_id==2546 & both$JOURNAL=="leco" & (both$YEAR>=1987 & both$YEAR<=1997)]<-"center of biological and ecological sciences"
# both$INST[both$editor_id==2546 & both$JOURNAL=="leco" & both$YEAR==1997]<-"center of biological and ecological sciences"
both$INST[both$editor_id==571 & both$JOURNAL=="leco" & (both$YEAR>=1987 & both$YEAR<=1992)]<-"harvard university"
# both$INST[both$editor_id==571 & both$JOURNAL=="leco" & both$YEAR==1992]<-"harvard university"
both$INST[both$editor_id==1906 & both$JOURNAL=="leco" & both$YEAR==2015]<-"harvard university"
both$INST[both$editor_id==371 & both$JOURNAL=="leco" & both$YEAR==2013]<-"arizona state university"
both$INST[both$editor_id==2580 & both$JOURNAL=="leco"]<-"university of wisconsin madison"
both$INST[both$editor_id==3756 & both$JOURNAL=="funecol"]<-"university of wisconsin madison"
both$INST[both$editor_id==111 & both$JOURNAL=="ecology"]<-"university of wisconsin madison"
both$INST[both$editor_id==111 & both$JOURNAL=="amnat"]<-"university of wisconsin madison"
both$INST[both$editor_id==2159 & both$JOURNAL=="ajb"]<-"university of wisconsin madison"
both$INST[both$editor_id==955 & both$JOURNAL=="jecol"]<-"university of wisconsin madison"
both$INST[both$editor_id==3294 & both$JOURNAL=="amnat"]<-"university of wisconsin madison"
both$INST[both$editor_id==1661 & both$JOURNAL=="oecol"]<-"university of wisconsin madison"
both$INST[both$editor_id==1815 & both$JOURNAL=="funecol"]<-"university of wisconsin madison"
both$INST[both$editor_id==874 & both$JOURNAL=="ajb"]<-"university of wisconsin madison"
both$INST[both$editor_id==1134 & both$JOURNAL=="jbiog"]<-"university of wisconsin madison"
both$INST[both$editor_id==901 & both$JOURNAL=="oecol"]<-"university of wisconsin madison"

both$INST[both$editor_id==3912 & both$JOURNAL=="leco" & (both$YEAR>=1993 & both$YEAR<=1997)]<-"oregon state university"
# both$INST[both$editor_id==3912 & both$JOURNAL=="leco" & both$YEAR==1997]<-"oregon state university"
both$INST[both$editor_id==602 & both$JOURNAL=="leco" & both$YEAR==2015]<-"university of wolverhampton"
both$INST[both$editor_id==3824 & both$JOURNAL=="leco" & both$YEAR==2015]<-"chinese academy of sciences"
both$INST[both$editor_id==1428 & both$JOURNAL=="leco" & (both$YEAR>=1987 & both$YEAR<=1992)]<-"international institute for aerospace survey and earth sciences"
# both$INST[both$editor_id==1428 & both$JOURNAL=="leco" & both$YEAR==1992]<-"international institute for aerospace survey and earth sciences"
both$INST[both$editor_id==1045 & both$JOURNAL=="leco" & (both$YEAR>=1987 & both$YEAR<=1992)]<-"university of arizona"
# both$INST[both$editor_id==1045 & both$JOURNAL=="leco" & both$YEAR==1992]<-"university of arizona"
both$INST[both$editor_id==3669 & both$JOURNAL=="oecol" & both$YEAR==2013]<-"estonian university of life sciences"

both$FIRST_NAME[both$LAST_NAME=="barry" & both$JOURNAL=="marecol" & (both$YEAR==2006|both$YEAR==2005)]<-"j"
both$MIDDLE_NAME[both$LAST_NAME=="barry" & both$JOURNAL=="marecol" & (both$YEAR==2006|both$YEAR==2005)]<-"p"


both$FIRST_NAME[both$LAST_NAME=="cadena" & both$JOURNAL=="condor"]<-"c"
both$MIDDLE_NAME[both$LAST_NAME=="cadena" & both$JOURNAL=="condor"]<-"daniel"
both$LAST_NAME[both$LAST_NAME=="cadenaordonez" & both$JOURNAL=="auk"]<-"cadena"
both$FIRST_NAME[both$LAST_NAME=="cadena" & both$JOURNAL=="auk"]<-"c"
both$MIDDLE_NAME[both$LAST_NAME=="cadena" & both$JOURNAL=="auk"]<-"daniel"

both$FIRST_NAME[both$LAST_NAME=="arnold" & both$FIRST_NAME=="w" & both$JOURNAL=="auk"]<-"todd"
both$MIDDLE_NAME[both$LAST_NAME=="arnold" & both$FIRST_NAME=="todd" & both$JOURNAL=="auk"]<-"w"

both$FIRST_NAME[both$LAST_NAME=="dumbacher" & both$JOURNAL=="auk"]<-"john"


both$INST[both$editor_id==898 & both$INST=="conicet consejo nacional de investigaciones cientificas y tecnicas"]<-"conicet cct mendoza"
both$INST[both$LAST_NAME=="areta" & both$INST=="conicet consejo nacional de investigaciones cientificas y tecnicas"]<-"conicet ibigeo"
both$INST[both$editor_id==2340 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro sustainable ecosystems"
both$INST[both$editor_id==1693 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro forest research"
both$INST[both$editor_id==196 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro entomology"
both$INST[both$LAST_NAME=="thrall" & both$YEAR==2010]<-"csiro plant industry"
both$INST[both$LAST_NAME=="rothstein" & both$INST=="university of california"]<-"university of california santa barbara"
both$INST[both$LAST_NAME=="powell" & both$INST=="university of alaska"]<-"university of alaska fairbanks"
both$INST[both$editor_id==2281 & both$INST=="csiro commonwealth scientific and industrial research organisation"]<-"csiro land and water"
# both$INST[both$editor_id==2281 & both$JOURNAL=="jecol" & (both$YEAR>1987 & both$YEAR<1994)]<-"csiro land and water"
both$INST[both$editor_id==2281 & both$UNIT=="division of wildlife and ecology" & both$CITY=="lyneham"]<-"csiro wildlife and ecology"
both$INST[both$editor_id==2281 & both$CITY=="lyneham"]<-"csiro wildlife and ecology"
both$INST[both$LAST_NAME=="austin" & both$CITY=="lyneham"]<-"csiro wildlife and ecology"
both$INST[both$LAST_NAME=="austin" & both$JOURNAL=="jecol"& (both$YEAR>1987 & both$YEAR<1994)]<-"csiro wildlife and ecology"
# both$INST[both$editor_id==2281 & (both$YEAR==1988 | both$YEAR==1989) & both$JOURNAL=="jecol"]<-"csiro wildlife and ecology"
both$INST[both$editor_id==2868 & both$INST=="csiro commonwealth scientific and industrial research organisation" & both$CITY=="canberra"]<-"csiro land and water"
both$INST[both$editor_id==2281 & both$CITY=="canberra"]<-"csiro land and water"
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
both$INST[both$editor_id==663 & both$JOURNAL=="amnat" & (both$YEAR>2005  &  both$YEAR<2010)]<-"usgs national wetlands research center"
both$INST[both$editor_id==1604 & both$INST=="usgs united states geological survey"]<-"usgs national wetlands research center"
both$INST[both$LAST_NAME=="piatt" & both$INST=="usgs united states geological survey"]<-"usgs alaska science center"
both$INST[both$LAST_NAME=="weathers" & both$INST=="university of california"]<-"university of california davis"
both$INST[both$LAST_NAME=="gutierrez" & both$INST=="university of minnesota"]<-"university of minnesota twin cities"
both$INST[both$editor_id==1854 & both$INST=="usgs united states geological survey"]<-"usgs patuxent wildlife research center"
both$INST[both$editor_id==2037 & both$INST=="usgs united states geological survey"]<-"usgs western ecological research center"
both$INST[both$editor_id==2037 & both$JOURNAL=="ecology"& (both$YEAR>2005 & both$YEAR<2016)]<-"usgs western ecological research center"
both$INST[both$LAST_NAME=="lafferty" & both$FIRST_NAME=="kevin" & both$YEAR==2005]<-"usgs western ecological research center"
both$INST[both$editor_id==3643 & both$INST=="usgs united states geological survey"]<-"usgs fort collins science center"
both$INST[both$editor_id==1848 & both$INST=="usgs united states geological survey"]<-"usgs patuxent wildlife research center"
both$INST[both$editor_id==486 & both$INST=="usgs united states geological survey"]<-"usgs florence bascom geoscience center"


both$INST[both$LAST_NAME=="niewiarowski" & both$INST=="ohio"]<-"university of akron"
both$INST[both$editor_id==1254 & both$INST=="ohio"]<-"ohio university"
both$INST[both$editor_id==2835]<-"pennsylvania state university"
both$INST[both$editor_id==3101]<-"university of washington seattle"
both$INST[both$editor_id==1612]<-"university of texas arlington"
both$INST[both$editor_id==697]<-"suny stony brook"
both$INST[both$editor_id==3069]<-"suny stony brook"
both$INST[both$editor_id==1364 & both$YEAR==2000]<-"university of freiburg"
both$FIRST_NAME[both$editor_id==1364]<-"hanns-christof"

both$INST[both$editor_id==1278]<-"suny stony brook"
both$INST[both$editor_id==3807]<-"suny binghamton"
both$INST[both$editor_id==1404]<-"finnish game and fisheries research institute"
both$UNIT[both$editor_id==1404]<-"oulu game and fisheries research"




both$INST[both$LAST_NAME=="hepinstall-cymerman" & both$JOURNAL=="leco"]<-"university of georgia"
both$INST[both$LAST_NAME=="cymerman" & both$JOURNAL=="leco"]<-"university of georgia"
both$CITY[both$LAST_NAME=="cymerman" & both$JOURNAL=="leco"]<-"athens"
both$INST[both$LAST_NAME=="tjoelker" & both$JOURNAL=="funecol" & both$YEAR>2011]<-"western sydney university"
both$INST[both$INST=="carnegie institution" & both$JOURNAL=="gcb"]<-"carnegie institution of washington"


both$INST[both$INST=="national autonomous universidad nacional autonoma de mexico"]<-"universidad nacional autonoma de mexico"


both$INST[(both$editor_id==500|both$editor_id==876|both$editor_id==1125|
             both$editor_id==1779|both$editor_id==2295|both$editor_id==2607|
             both$editor_id==2841|both$editor_id==3202|both$editor_id==3234|
             both$editor_id==3342|both$editor_id==3425|both$editor_id==7) &
            both$INST=="university of british columbia"]<-"university of british columbia vancouver"


both$INST[both$LAST_NAME=="martin" & 
            both$FIRST_NAME=="kathy" & 
            both$INST=="university of british columbia"]<-"university of british columbia vancouver"











both$FIRST_NAME[both$editor_id==3378]<-"s"



# weisser vs weiser


both$INST[both$LAST_NAME == "weisser" & both$JOURNAL=="oecol" &
            (both$YEAR>2006 & both$YEAR<2014)]<-"university of jena"
both$editor_id[both$LAST_NAME == "weisser" & both$JOURNAL=="oecol" &
                 (both$YEAR>2006 & both$YEAR<2015)]<-NA
# 
# both$LAST_NAME[both$LAST_NAME=="de kroon" & both$FIRST_NAME=="hans"]<-"hans"
# 
# both$LAST_NAME[both$LAST_NAME=="van groenendael" & both$FIRST_NAME=="jan"]<-"vangroenendael"
# 
# both$LAST_NAME[both$LAST_NAME=="van der meijden" & both$FIRST_NAME=="eddy"]<-"vandermeijden"
# 
# both$LAST_NAME[both$LAST_NAME=="van der meijden" & both$FIRST_NAME=="eddy"]<-"vandermeijden"
# 
# both$LAST_NAME[both$LAST_NAME=="van der meijden" & both$FIRST_NAME=="eddy"]<-"vandermeijden"

both$LAST_NAME<-gsub("[[:space:]]", "", both$LAST_NAME)





both$LAST_NAME[both$LAST_NAME == "weisser" & both$JOURNAL=="oecol" & (both$YEAR>1983 & both$YEAR<2007)]<-"weiser"
both$MIDDLE_NAME[both$LAST_NAME == "weiser" & both$JOURNAL=="oecol" &
                   both$YEAR<2007]<-NA

both<-both[!(both$editor_id=="3798" & both$JOURNAL=="oecol" & both$LAST_NAME=="weiser" & both$YEAR>2006),]
both<-both[!(both$LAST_NAME=="mason" & both$FIRST_NAME=="christoph" & both$JOURNAL=="jape"),]
both<-both[!(both$LAST_NAME=="fernande-juricic" & both$JOURNAL=="jape" & both$YEAR==2010),]
both<-both[!(both$editor_id=="1261" & both$JOURNAL=="oecol" & both$YEAR==1999),]
both<-both[!(both$editor_id=="3669" & both$JOURNAL=="oecol" & both$YEAR==2012),]
both<-both[!(both$editor_id=="2069" & both$JOURNAL=="plantecol" & both$YEAR==2007),]
both<-both[!(both$editor_id=="1079" & both$JOURNAL=="plantecol" & both$YEAR==2007),]
both<-both[!(both$editor_id=="100" & both$JOURNAL=="plantecol" & both$YEAR==1991),]
both<-both[!(both$editor_id=="2896" & both$JOURNAL=="plantecol" & both$YEAR==2004),]
both<-both[!(both$editor_id=="1057" & both$JOURNAL=="biocon" & both$YEAR==1985),]
both<-both[!(both$editor_id=="56" & both$JOURNAL=="biocon" & both$YEAR==1985),]
both<-both[!(both$editor_id=="2914" & both$JOURNAL=="biocon" & both$YEAR==1985),]
both<-both[!(both$editor_id=="753" & both$JOURNAL=="biocon" & both$YEAR==1985),]
both<-both[!(both$editor_id=="56" & both$JOURNAL=="biocon" & both$YEAR==1986),]
both<-both[!(both$editor_id=="3355" & both$JOURNAL=="biocon" & both$YEAR==1998),]
both<-both[!(both$editor_id=="1402" & both$JOURNAL=="oecol" & both$YEAR==2012),]
both<-both[!(both$editor_id=="3944" & both$JOURNAL=="oecol" & both$YEAR==1992),]
both<-both[!(both$editor_id=="1819" & both$JOURNAL=="bitr" & both$YEAR==1997),]
both<-both[!(both$editor_id=="591" & both$JOURNAL=="evol" & both$YEAR==2015),]
both<-both[!(both$editor_id=="2047" & both$JOURNAL=="jane" & both$YEAR==2012),]
both<-both[!(is.na(both$editor_id) & both$JOURNAL=="jape" & both$LAST_NAME=="croxall"),]
both<-both[!(both$editor_id=="3584" & both$JOURNAL=="leco" & both$YEAR==2015),]
# on advisory board but not ed board
both<-both[!(both$editor_id=="371" & both$JOURNAL=="leco" & both$YEAR==2012),]
both<-both[!(both$editor_id=="371" & both$JOURNAL=="leco" & both$YEAR==2013),]
both<-both[!(both$editor_id=="23" & both$JOURNAL=="oecol" & both$YEAR==2011),]
both<-both[!(both$LAST_NAME=="banks-lei" & both$JOURNAL=="jape" & both$YEAR==2013),]



# country city unit corrections
both$CITY[both$UNIT=="ontario canada"]<-"gainesville"
both$STATE[both$UNIT=="ontario canada"]<-"fl"
both$UNIT[both$UNIT=="ontario canada"]<-"dept. of biology"
both$COUNTRY[both$INST=="university of florida"]<-"usa"
both$COUNTRY[both$CITY=="latvia"]<-"latvia"
both$CITY[both$CITY=="latvia"]<-NA
both$CITY[both$CITY=="ann arbon"]<-"ann arbor"

both$CITY<-gsub("st. ","st ",both$CITY)
both$CITY<-gsub("saint ","st ",both$CITY)
both$INST<-gsub("st. ","st ",both$INST)
both$INST<-gsub("saint ","st ",both$INST)
both$CITY<-gsub("saint ","st ",both$CITY)
both$CITY<-gsub("ft. ","fort ",both$CITY)
both$CITY<-gsub("ft c","fort c ",both$CITY)

# both$CITY<-gsub("w\x9frzburg","wurzburg",both$CITY)
# both$CITY<-gsub("k\x9aln","cologne",both$CITY)
# both$CITY<-gsub("m\x9fnchen","munich",both$CITY)
# both$CITY<-gsub("g\x9attingen","gottingen",both$CITY)
# both$CITY<-gsub("z\x81rich","zurich",both$CITY)

# both$CITY<-tolower(both$CITY)

both$CITY<-gsub("edinburgh eh9 3jz","edinburgh",both$CITY)
both$CITY<-gsub("quebec city","quebec",both$CITY)
both$CITY<-gsub("fuzerbrook","furzebrook",both$CITY)
both$CITY<-gsub("e lansing","east lansing",both$CITY)
both$CITY<-gsub("hickory coners","hickory corners",both$CITY)
both$CITY<-gsub("rodenbosch","rondebosch",both$CITY)
both$CITY<-gsub("rodenbosch","rondebosch",both$CITY)
both$CITY<-gsub("santa cruz, california, usa","santa cruz",both$CITY)
both$CITY<-gsub("auburn, alabama, usa","auburn",both$CITY)
both$CITY<-gsub("cambridge, massachusetts, usa","cambridge",both$CITY)
both$CITY<-gsub("sheffield s10 2tn","sheffield",both$CITY)
both$CITY<-gsub("mississippi state","",both$CITY)
both$CITY<-gsub("storrs","storrs",both$CITY)
both$CITY<-gsub("starrs","storrs",both$CITY)
both$CITY<-gsub("champaign","urbana-champaign",both$CITY)
both$CITY<-gsub("urbana","urbana-champaign",both$CITY)
both$CITY<-gsub("guleph","guelph",both$CITY)
both$CITY<-gsub("goettingen","gottingen",both$CITY)
both$CITY<-gsub("osnabrück","osnabruck",both$CITY)
both$CITY<-gsub("montana","",both$CITY)
both$CITY<-gsub("antwerpen","antwerp",both$CITY)
both$CITY<-gsub("brasília","brasilia",both$CITY)
both$CITY<-gsub("p.r.","",both$CITY)
both$CITY<-gsub("denmark","",both$CITY)
both$CITY<-gsub("mexico d.f.","mexico city",both$CITY)
both$CITY<-gsub("stockolm","stockholm",both$CITY)
both$CITY<-gsub("nottinghamuk","nottingham",both$CITY)
both$CITY<-gsub("stonybrook","stony brook",both$CITY)
both$CITY<-gsub("tufts","medford",both$CITY)
both$CITY<-gsub("tuscon","tucson",both$CITY)
both$CITY<-gsub("new york city","new york",both$CITY)
both$CITY<-gsub("notre dame","south bend",both$CITY)

both$CITY<-gsub("st martin d'heres cedex","st martin dheres cedex",both$CITY)
both$CITY<-gsub("st-jean-sur-richelieu","st jean sur richelieu",both$CITY)

# notes
both$NOTES[both$CITY=="latvia"]<-NA
both$NOTES[both$editor_id==130 & both$YEAR==1990]<-"inst was formerly oxford polytechnic"
both$NOTES[both$editor_id==3395 & both$YEAR==1986]<-"not 100% regarding inst"
both$NOTES[both$editor_id==289 & both$YEAR==1985]<-"not 100% regarding inst"
both$NOTES[both$editor_id==722 & both$YEAR==1985]<-"not 100% regarding inst; city in 1993 paper & jrnl front mattter don't match"


both$editor_id[both$LAST_NAME=="fedler" & both$FIRST_NAME=="anthony" & both$JOURNAL=="najfm"]<-NA
both$INST[both$INST=="university of wisconsin eauniversity of claire"]<-"university of wisconsin eau claire"

return(both)
}
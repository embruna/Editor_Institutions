# This will load and the corrected files and make the required changes.

library(tidyverse)
source("institution_cleaner.R")
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
# Round 3: BITR
# Round 4: AMNAT
# Round 6: JECOL
# Round 7: JAPE
# Round 5: BIOCON
# Round 8: PLANTECOL
# Round 9: JBIOG
# Round 10: LECO
# JANE
# JZOOL: will need extensive loopkup
# OECOL: will need extensive loopkup
# OIKOS: will need extensive loopkup
# AUK: not for this paper
# CONDOR: not for this paper
##############################################################


multi1<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_1.csv", col_names = TRUE)
multi1<-multi1 %>% fill(INST,UNIT,CITY,STATE)

multi2a<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_2a.csv", col_names = TRUE)
multi2a<-multi2a %>% filter(JOURNAL=="CONBIO"|JOURNAL=="NEWPHYT") %>% fill(INST,UNIT,CITY,STATE) #delete out other journals this is conbio and new phyt

multi2b<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_2b.csv", col_names = TRUE)
multi2b<-multi2b %>% filter(JOURNAL=="CONBIO"|JOURNAL=="NEWPHYT") %>% fill(INST,UNIT,CITY,STATE) #delete out other journals this is conbio and new phyt

BITR_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_3_BITR.csv", col_names = TRUE)
BITR_inst<-BITR_inst %>% fill(INST,UNIT,CITY,STATE)

AMNAT_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_4_AMNAT.csv", col_names = TRUE)
AMNAT_inst<-AMNAT_inst %>% fill(INST,UNIT,CITY,STATE)%>% filter(JOURNAL=="AMNAT")

JECOL_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_6_JEcol.csv", col_names = TRUE)
JECOL_inst<-JECOL_inst %>% fill(INST,UNIT,CITY,STATE)%>% filter(JOURNAL=="JECOL")

JAPE_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_7_JAPE.csv", col_names = TRUE)
JAPE_inst<-JAPE_inst %>% 
  rename("FIRST_NAME"="FIRST_NA", "MIDDLE_NAME"="MIDDLE_","LAST_NAME"="LAST_NA") %>% 
  fill(INST,UNIT,CITY,STATE,NOTES)%>% filter(JOURNAL=="JAPE")
JAPE_inst$editor_id<-NULL
# TODO: something is going on with editot IDS


JBIOG_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/newPJCorrectionsDONE_JBIOG.csv", col_names = TRUE)
JBIOG_inst<-JBIOG_inst %>% select(-X1,-check) %>% rename("NOTES"=`please note here if INCORRECT`)
# This will keep only the ones we 2x / spotchecked that are correct OR are still missing the INST 
JBIOG_inst<-JBIOG_inst[(!is.na(JBIOG_inst$NOTES)|
                          (JBIOG_inst$INST=="missing")),]


LECO_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/newPJCorrections_10_LECO.csv", col_names = TRUE)
names(LECO_inst)
LECO_inst<-LECO_inst %>%  select(-X1,-check) %>% rename("NOTES"=`please note if INCORRECT`)
LECO_inst<-LECO_inst[(!is.na(LECO_inst$NOTES)|
                          (LECO_inst$INST=="missing")),]

# 
# PLANTECOL_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_8_PLANTECOL.csv", col_names = TRUE)
# PLANTECOL_inst<-PLANTECOL_inst %>%  select(-X1,-check) %>% rename("NOTES"=`Please note here if INCORRECT`)
# PLANTECOL_inst<-PLANTECOL_inst[(!is.na(PLANTECOL_inst$NOTES)|
#                         (PLANTECOL_inst$INST=="missing")),]
# 

#############
INST_fix<-bind_rows(multi2b,multi2a,multi1,BITR_inst,JECOL_inst,AMNAT_inst,JAPE_inst,JBIOG_inst,LECO_inst) %>% 
  distinct(editor_id,JOURNAL,YEAR,.keep_all= TRUE) %>%     #there are some duplicates, best to remove them
  arrange(JOURNAL,editor_id,YEAR)


INST_fix$INST[INST_fix$INST=="UNKNOWN"]<-NA
INST_fix$INST[INST_fix$INST=="unknown"]<-NA
INST_fix$UNIT[INST_fix$UNIT=="UNKNOWN"]<-NA
INST_fix$CITY[INST_fix$CITY=="UNKNOWN"]<-NA
INST_fix$STATE[INST_fix$STATE=="UNKNOWN"]<-NA
INST_fix$STATE[INST_fix$STATE=="unknown"]<-NA

source("institution_cleaner.R")
INST_fix<-institution_cleaner(INST_fix)
INST_fix<-INST_fix %>% select(-X1)
head(INST_fix,20)
#

# ALLDATA$editor_id<-as.numeric(ALLDATA$editor_id)
# ARE THERE ANY IN CORRECT that *ARENT* in ALLDATA?
# THESE NEED TO BE ADDED TO ALLDATA
# C_but_not_O<-anti_join(INST_fix,ALLDATA,by=c("editor_id","JOURNAL","YEAR")) %>% arrange(JOURNAL,YEAR,editor_id)  #in correct but not orig 24
C_but_not_O<-anti_join(INST_fix,ALLDATA,by=c("JOURNAL","YEAR","LAST_NAME")) %>% arrange(JOURNAL,YEAR,editor_id)  #in correct but not orig 24
nrow(C_but_not_O)
summary(C_but_not_O)

#THESE ARE THE ONES IN ALLDATA but not CORRECTED 
# O_butnot_C<-anti_join(ALLDATA,INST_fix,by=c("editor_id","JOURNAL","YEAR")) # in orig but not correct 21221
O_butnot_C<-anti_join(ALLDATA,INST_fix,by=c("JOURNAL","YEAR","LAST_NAME")) # in orig but not correct 21221
nrow(O_butnot_C)
O_butnot_C

# THESE ARE THE ONES IN BOTH CORRECTED AND ALLDATA
# O_and_C<-inner_join(ALLDATA,INST_fix,by=c("editor_id","JOURNAL","YEAR")) #in correct but not orig 4381
O_and_C<-inner_join(ALLDATA,INST_fix,by=c("LAST_NAME","JOURNAL","YEAR")) #in correct but not orig 4381
nrow(O_and_C)


# both<-full_join(ALLDATA,INST_fix,by=c("editor_id","JOURNAL","YEAR")) 
both<-full_join(ALLDATA,INST_fix,by=c("JOURNAL","YEAR","LAST_NAME","FIRST_NAME")) 
nrow(both)
nrow(ALLDATA)
nrow(INST_fix)
nrow(C_but_not_O)
nrow(O_butnot_C)
nrow(O_and_C)
nrow(C_but_not_O)+nrow(O_butnot_C)+nrow(O_and_C)

head(both)
str(both)
colnames(both)
both<-select(both,JOURNAL,YEAR,VOLUME,ISSUE,editor_id.x,editor_id.y,
             FIRST_NAME,MIDDLE_NAME.x,MIDDLE_NAME.y,LAST_NAME,
             TITLE,CATEGORY,CATEGORY.x,INST.x,INST.y,UNIT.x,UNIT.y,CITY.x,CITY.y,STATE.x,STATE.y,
             COUNTRY.x,COUNTRY.y,COUNTRY_Prior_Class,geo.code,geo.code_Prior_Class,
             GENDER,NOTES.x,NOTES.y)

#CAN QUICKLY ID WHAT NEEDS TO BE FIXED AS FOLLOWS

# FIRST NAME DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
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
# # This identifies one mistake in thge original (ALLDATA) that needs to be corrected 
# str(both)
both$LAST_NAME[both$editor_id.x==1355 & both$FIRST_NAME=="Holmes"]<-"Rolston"
both<-both[!(is.na(both$editor_id.x) & both$FIRST_NAME=="Holmes"),]


# both$LAST_NAME.y<-NULL
# both$LAST_check<-NULL
# both<-both %>% rename("LAST_NAME"="LAST_NAME.x")


# STATE DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
summary(both$STATE.x==both$STATE.y) # 19 FALSE
both$STATE_check<-both$STATE.x==both$STATE.y

both$STATE.x[both$YEAR!=1992 & both$FIRST_NAME=="David" & both$LAST_NAME=="Gibson" & both$CITY.x=="Carbondale"]<-"IL"
both$STATE.x[both$YEAR==1992 & both$CITY.x=="Pensacola" & both$LAST_NAME=="Gibson"]<-"FL"
both$STATE.x[both$LAST_NAME=="Moss"]<-NA
both$STATE.x[both$LAST_NAME=="Usher"]<-NA
both$STATE.x[both$LAST_NAME=="Milner-Gulland"]<-NA
both$INST.x[both$LAST_NAME=="Belovsky" & both$INST.y=="Notre Dame"]<-"Notre Dame University"
both$CITY.x[both$LAST_NAME=="Belovsky" & both$INST.y=="Notre Dame"]<-"Notre Dame"
both$STATE.x[both$LAST_NAME=="Belovsky" & both$INST.x=="Notre Dame University"]<-"IN"
both$STATE.x[both$FIRST_NAME=="James" & both$LAST_NAME=="Carlton"]<-"CT"
both$STATE.x[both$FIRST_NAME=="Jon" & both$LAST_NAME=="Rodriguez"]<-NA
both$STATE.x[both$editor_id.x==455 & both$FIRST_NAME=="Christopher" & both$LAST_NAME=="Frissell"]<-"MT"
both$INST.x[both$editor_id.x==455 & both$FIRST_NAME=="Christopher" & both$LAST_NAME=="Frissell"]<-"University of Montana"
both$UNIT.x[both$editor_id.x==455 & both$FIRST_NAME=="Christopher" & both$LAST_NAME=="Frissell"]<-"Flathead Lake Biological Station"
both$CITY.x[both$editor_id.x==455 & both$FIRST_NAME=="Christopher" & both$LAST_NAME=="Frissell"]<-"Polson"
both$CITY.x[both$editor_id.x==1511 & both$LAST_NAME=="Cinner" & both$YEAR>2009 & both$YEAR<2015 ]<-"Townsville"
both$STATE.x[both$editor_id.x==1511 & both$LAST_NAME=="Cinner" & both$YEAR>2009 & both$YEAR<2015 ]<-"Queensland"
both$UNIT.x[both$editor_id.x==1511 & both$LAST_NAME=="Cinner"]<-"ARC Centre of Excellence for Coral Reef Studies"
both$INST.x[both$editor_id.x==1511 & both$LAST_NAME=="Cinner"]<-"James Cook University"
both$NOTES.y[both$editor_id.x==1511 & both$LAST_NAME=="Cinner" & both$YEAR==2013]<-"journal front matter has INST=Columbia Univ, but his CV makes no mention of this"

# This will replace all the "NA" in STATE.x (origianlly no info) with the value from STATE.y (Patrick's data collection), if there is one 
both<-both %>% mutate(STATE.x = replace(STATE.x, is.na(STATE.x), STATE.y[is.na(STATE.x)]))

both$STATE.y<-NULL
both$STATE_check<-NULL
both<-both %>% rename("STATE"="STATE.x")

# This identifies one mistake in thge original (ALLDATA) that needs to be corrected 


# INST DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE

# This will replace all the "NA" and "" in INST.x (origianlly no info) with the value from INST.y (Patrick's data collection), if there is one 
both<-both %>% mutate(INST.x = replace(INST.x, is.na(INST.x), INST.y[is.na(INST.x)]))
both <- both %>% mutate(INST.x = replace(INST.x, INST.x == "", NA))
summary(both$INST.x==both$INST.y) # 235 FALSE
both$INST_check<-both$INST.x==both$INST.y

INST_check<-filter(both,INST_check=="FALSE")
INST_check_ok<-filter(both,INST_check==TRUE |is.na(INST_check))

write.csv(INST_check, file="./Data/Patrick_James_Data_Corrections/Complete/INST_corrections_2x.csv", row.names = F) #export it as a csv file


both$INST.x[both$editor_id.x==1248 & both$JOURNAL=="BITR" & both$YEAR==1987 ]<-"New York Botanical Garden"
both$INST.x[both$editor_id.x==1704 & both$JOURNAL=="JECOL" & both$YEAR==2009 ]<-"University of Sheffield"
# both$INST.x[both$INST.x=="Unniversity of Stirling" ]<-"University of Stirling"

both$UNIT.x[both$editor_id.x=1229 & both$INST.x=="University of Montana"]<-"Savannah River Ecology Laboratory"
both$INST.x[both$editor_id.x==1229 & both$INST.x=="University of Montana"]<-"University of Georgia"

both$UNIT.x[both$INST.x=="ALTERRA Research Institute for the Green World" ]<-"ALTERRA Research Institute for the Green World"
both$UNIT.x[both$INST.x=="Gatty Marine Lab (University of Saint Andrews)" ]<-"Gatty Marine Lab"
both$UNIT.x[both$INST.x=="Netherlands Institute of Ecology; Wageningen University and Research Centre Netherlands Institute of Ecology" ]<-"Netherlands Institute of Ecology"
both$UNIT.x[both$INST.x=="Norwich" ]<-"Norwich Research Park Industrial Biotechnology and Bioenergy Alliance"
both$UNIT.x[both$INST.x=="Scripps Institute of Oceanography" ]<-"Scripps Institution of Oceanography"
both$UNIT.x[both$INST.x=="Scripps Institution  of Oceanography" ]<-"Scripps Institution of Oceanography"
both$UNIT.x[both$INST.x=="Scripps Institute of Oceanography" ]<-"Gatty Marine Lab"



both$UNIT.x[both$INST.x=="Savannah River Ecology Laboratory" ]<-"Savannah River Ecology Laboratory"
both$INST.x[both$INST.x=="Savannah River Ecology Laboratory" ]<-"University of Georgia"
both$UNIT.x[both$INST.y=="University of California Santa Cruz Extension"]<-"UCSC Extension"
both$INST.x[both$INST.x=="University of California" & both$INST.y=="University of California Davis" ]<-"University of California Davis"
both$INST.x[both$INST.x=="University of California" & both$INST.y=="University of California Berkeley" ]<-"University of California Berkeley"
both$INST.x[both$INST.x=="University of California" & both$INST.y=="University of California Riverside" ]<-"University of California Riverside"
# both$INST.x[both$editor_id.x==1839 & both$JOURNAL=="CONBIO" ]<-"Venezuelan Institute For Scientific Investigation"
both$INST.x[both$editor_id.x==1218 & both$JOURNAL=="CONBIO" & both$YEAR>1986 & both$YEAR<1992 ]<-"Royal Botanic Gardens Kew"
# both$INST.x[both$INST.x=="Museum Natl Hist Nat"]<-"National History Museum Paris"
# both$INST.x[both$INST.x=="University<ca>of<ca>California<ca>Santa<ca>Cruz"]<-"University of California Santa Cruz"
# both$INST.x[both$INST.y=="Uc Santa Cruz"]<-"University of California Santa Cruz"

both$INST.x[both$LAST_NAME=="Streeter" & both$JOURNAL=="JECOL" & both$YEAR==2009 ]<-"University of Sussex"
both$INST.x[both$LAST_NAME=="Grover" & both$JOURNAL=="AMNAT"]<-"University of Texas Arlington"
both$INST.x[both$LAST_NAME=="Noss" & both$JOURNAL=="CONBIO" & both$YEAR==1998 ]<-"Conservation Biology Institute"

both$UNIT.x[both$LAST_NAME=="Dratch" & both$JOURNAL=="CONBIO" & both$INST.x=="National Fish and Wildlife Forensics Laboratory" ]<-"National Fish and Wildlife Forensics Laboratory"
both$INST.x[both$LAST_NAME=="Dratch" & both$JOURNAL=="CONBIO" & both$INST.x=="National Fish and Wildlife Forensics Laboratory" ]<-"US Fish and Wildlife Service"
both$INST.x[both$LAST_NAME=="Daszak" & both$JOURNAL=="CONBIO" & both$INST.x=="University of Nevada Reno" ]<-"Consortium for Conservation Medicine"

both$UNIT.x[both$LAST_NAME=="Meffe" & both$JOURNAL=="CONBIO" & both$INST.x=="University of Montana" ]<-"Savanna Riverl Ecological Laboratory"
both$UNIT.x[both$LAST_NAME=="Meffe" & both$JOURNAL=="CONBIO" & both$UNIT.x=="Savanna Riverl Ecological Laboratory" ]<-"University of Georgia"
both$UNIT.x[both$JOURNAL=="LECO" & both$INST.x=="Institute of Landscape Ecology of Slovak Academy of Sciences" ]<-"Institute of Landscape Ecology"
both$INST.x[both$JOURNAL=="LECO" & both$INST.x=="Institute of Landscape Ecology of Slovak Academy of Sciences" ]<-"Slovak Academy of Sciences"

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
write.csv(CITY_check, file="./Data/Patrick_James_Data_Corrections/Complete/CITY_corrections_2x.csv", row.names = F) #export it as a csv file

# This will replace all the "NA" in CITY.x (origianlly no info) with the value from CITY.y (Patrick's data collection), if there is one 
both<-both %>% mutate(CITY.x = replace(CITY.x, is.na(CITY.x), CITY.y[is.na(CITY.x)]))

both$CITY.y<-NULL
both$CITY_check<-NULL
both<-both %>% rename("CITY"="CITY.x")

# TODO UNIT DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
# both$UNIT.x<-as.character(both$UNIT.x)
# both$UNIT.x<-gsub("",NA,both$UNIT.x)
both$UNIT.x[both$UNIT.x == ""] <- NA
summary(both$UNIT.x==both$UNIT.y) # 7 FALSE
both$UNIT_check<-both$UNIT.x==both$UNIT.y

# No probs, just differences in words ("the, and", etc)
# This will replace all the "NA" in UNIT.x (origianlly no info) with the value from UNIT.y (Patrick's data collection), if there is one 
both<-both %>% mutate(UNIT.x = replace(UNIT.x, is.na(UNIT.x), UNIT.y[is.na(UNIT.x)]))

both$UNIT.x[both$UNIT.y=="Savannah River Ecology Laboratory"]<-"Savannah River Ecology Laboratory"
both$UNIT.y<-NULL
both$UNIT_check<-NULL
both<-both %>% rename("UNIT"="UNIT.x")

# TODO COUNTRY DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE

both <- both %>% mutate(COUNTRY.x = replace(COUNTRY.x, COUNTRY.x == "", NA))
both$COUNTRY.x<-as.factor(both$COUNTRY.x)
country_levels<-(c(levels(both$COUNTRY.x),levels(both$COUNTRY.y)))
levels(both$COUNTRY.x)<-c(levels(both$COUNTRY.x),country_levels)
levels(both$COUNTRY.y)<-c(levels(both$COUNTRY.y),country_levels)

# This will replace all the "NA" in CITY.x (origianlly no info) with the value from CITY.y (Patrick's data collection), if there is one 

levels(both$COUNTRY.x)
str(both$COUNTRY.x)
str(both$COUNTRY.y)

both<-both %>% mutate(COUNTRY.x = replace(COUNTRY.x, is.na(COUNTRY.x), COUNTRY.y[is.na(COUNTRY.x)]))
summary(both$COUNTRY.x==both$COUNTRY.y) # 3552 FALSE
both$country_check<-both$COUNTRY.x==both$COUNTRY.y

country_check<-filter(both,country_check=="FALSE")
write.csv(country_check, file="./Data/Patrick_James_Data_Corrections/Complete/COUNTRY_corrections_2x.csv", row.names = F) #export it as a csv file

both$COUNTRY.x[both$LAST_NAME=="Tjoelker" & both$JOURNAL=="NEWPHYT" & both$INST=="Texas A & M University"]<-"USA"
both$COUNTRY.x[both$LAST_NAME=="Atkin" & both$JOURNAL=="NEWPHYT" & both$INST=="University of York"]<-"United Kingdom"
both$COUNTRY.x[both$LAST_NAME=="Long" & both$JOURNAL=="JECOL" & both$INST=="University of Essex"]<-"United Kingdom"
both$COUNTRY.x[both$LAST_NAME=="Westing" & both$JOURNAL=="CONBIO" & both$INST=="Stockholm International Peace Research Institute"]<-"Sweden"
both$CITY[both$LAST_NAME=="Westing" & both$JOURNAL=="CONBIO" & both$INST=="Stockholm International Peace Research Institute"]<-"Stockholm"
both$STATE[both$LAST_NAME=="Westing" & both$JOURNAL=="CONBIO" & both$INST=="Stockholm International Peace Research Institute"]<-NA
both$COUNTRY.x[both$LAST_NAME=="Westing" & both$JOURNAL=="CONBIO" & both$INST=="Westing Associates"]<-"USA"
both$COUNTRY.x[both$LAST_NAME=="Belovsky" & both$JOURNAL=="CONBIO" & both$INST=="Utah State University"]<-"USA"
both$UNIT[both$LAST_NAME=="Bolker" & both$JOURNAL=="AMNAT" & both$INST=="McMaster University"]<-NA
both$COUNTRY.x[both$LAST_NAME=="Bolker" & both$JOURNAL=="AMNAT" & both$INST=="McMaster University"]<-"Canada"

both$UNIT[both$LAST_NAME=="Krivan" & both$JOURNAL=="AMNAT" ]<-"Biology Centre"
both$INST[both$LAST_NAME=="Krivan" & both$JOURNAL=="AMNAT" ]<-"Academy of Sciences of the Czech Republic"
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
both$NOTES[both$INST == "Biological Centre"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "Biological Institute"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "Bogota"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "DiSTEBA University of Salento"] <- "DOUBLE CHECK INST -  / DiSTEBA Universita di Lecce"
both$NOTES[both$INST == "Haus Nr.9"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "James Cook University Townsville"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "Lancaster"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "London"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "Madrid"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "Maine"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "missing"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "Royal Botanic Gardens Melbourne University of Melbourne"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "Salzburg"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "Swansea"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "Sydney"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "SEIDENSTZICKER& Kleiman = Smithsonian National Zoological Park, Labandeira: Smithsonian National Museum of Natural History"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "Montpellier"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "DOUBLE CHECK"] <- "DOUBLE CHECK INST"
both$NOTES[both$INST == "CNRS"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
both$NOTES[both$INST == "CSIRO"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
both$NOTES[both$INST == "Smithsonian Institution"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
both$NOTES[both$INST == "University of Arkansas"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
both$NOTES[both$INST == "University of British Columbia"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
both$NOTES[both$INST == "University of California"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
both$NOTES[both$INST == "University of Exeter"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
both$NOTES[both$INST == "University of Georgia"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
both$NOTES[both$INST == "University of Illinois"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
both$NOTES[both$INST == "University of Massachusetts"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
both$NOTES[both$INST == "University of Minnesota"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
both$NOTES[both$INST == "University of South Carolina"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
both$NOTES[both$INST == "University of Texas"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
both$NOTES[both$INST == "University of Toronto"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
both$NOTES[both$INST == "University of Wisconsin"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"
both$NOTES[both$INST == "US Geological Survey"] <- "DOUBLE CHECK WHAT CAMPUS/UNIT"


# NO IDEA WHY THIS ISN'T WORKING INSIDE FUNCTION, SO DOING HERE
both$INST[both$INST == "University of ArKansas"] <- "University of Arkansas"




both<-institution_cleaner(both)




both$INST<-gsub("ArKansas","Arkansas", both$INST)


# both$INST[both$INST == "UC Davis"] <- "University of California Davis"
# both$INST[both$INST == "UF"] <- "University of Florida"
# both$INST[both$INST == "UGA"] <- "University of Georgia"
# both$INST[both$INST == "UNIV CONNECTICUT"] <- "University of Connecticut"
# both$INST[both$INST == "Univ Los Andes"] <- "Universidad de los Andes"
# both$INST[both$INST == "Univ Nacl Autonoma Mexico"] <- "Universidad Nacional Autonoma de Mexico"
# both$INST[both$INST == "Ume¥ University"] <- "Umea University"
# both$INST[both$INST == "Texas A and M"] <- "Texas A and M University"
# both$INST[both$INST == "Purdue"] <- "Purdue University"
# both$INST[both$INST == "Princeton"] <- "Princeton University"
# both$INST[both$INST == "Oxford"] <- "Oxford University"
# both$INST[both$INST == "Cambridge"] <- "University of Cambridge"
# both$INST[both$INST == "N.C. State"] <- "North Carolina State University"
# both$INST[both$INST == "LSU"] <- "Louisiana State University"
# both$INST[both$INST == "double check"] <- "DOUBLE CHECK"
# both$INST[both$INST == "Auburn"] <- "Auburn University"
# both$INST[both$INST == "Michigan State"] <- "Michigan State University"
# both$INST[both$INST == "Berkeley"] <- "University of California Berkeley"
# both$INST[both$INST == "Duke"] <- "Duke University"

# both$INST[both$INST == "Los Alamos"] <- "Los Alamos National Laboratory"
# both$INST[both$INST == "Stazione Zoologica \"Anton Dohrn\""] <- "Stazione Zoologica Anton Dohrn"
# both$INST[both$INST == "Cambridge"] <- "University of Cambridge"
# both$INST[both$INST == "Cambridge"] <- "University of Cambridge"
# both$INST[both$INST == "UVA"] <- "University of Virginia"
# both$INST[both$INST == "US FOREST SERV"] <- "US Forest Service"

levels(as.factor(both$INST))

both$INST<-as.factor(both$INST)
both$INST<-droplevels(both$INST)
both_inst_check<-as.data.frame(levels(both$INST))
write_csv(both_inst_check,"./data/both_inst_check.csv")
# "St. Andrews"      
# "Salzburg"  
# "Plymouth" 
# "ORNL" 
# "Aberdeen"                                                                                                                   
# "Aberystwyth"   
# "Bangor" 
# "Michigan State" 
# "Bogota" 
# "London"           
# "Duram"
# "Glasgow"                                                                                                                    
# "Göttingen"
# "Madrid"                                                                                                                     
# "Maine" 
# "Norwich"   
# "SEIDENSTZICKERand Kleiman = Smithsonian National Zoological Park Labandeira: Smithsonian National Museum of Natural History"
# "Stazione Zoologica \"Anton Dohrn\""  







# 
# 
# colnames(both)
# anti_join(both,ALLDATA)
# 
# nrow(O_butnot_C)+nrow(C_but_not_O)+nrow(O_and_C)
# nrow(ALLDATA)
# 
# levels(as.factor(Obut_notC$JOURNAL))
# ALLDATA
# foo<-inner_join(CORRECT,ORIG,by=c("editor_id","JOURNAL","YEAR")) #in correct but not orig 24
# foo$check<-(foo$LAST_NAME.x==foo$LAST_NAME.y)
# 
# foo2<-anti_join(CORRECT,ORIG,by=c("editor_id","JOURNAL","YEAR")) # in orig but not correct 21085
# nrow(ORIG)
# nrow(CORRECT)
# nrow(foo)
# nrow(foo2)
# 
# duplicated_ALLDATA<-ALLDATA %>% 
#   group_by(editor_id,JOURNAL,YEAR) %>% 
#   filter(n()>1)
# 
# 
# nrow(ORIG)+nrow(foo)
# nrow(CORRECT)+nrow(foo2)+nrow(foo)
# not_in_ALLDATA<-anti_join(INST_fix,ALLDATA,by="editor_id","JOURNAL")
# not_in_ALLDATA<-anti_join(ALLDATA,INST_fix)
# nrow(ALLDATA)
# nrow(INST_fix)
# nrow(not_in_ALLDATA)
# nrow(not_in_ALLDATA)+nrow(INST_fix)
# 
# 
# str(ALLDATA)
# str(INST_fix)
# ALLDATA$editor_id<-as.numeric(ALLDATA$editor_id)
# levels(as.factor(ALLDATA$JOURNAL))
# levels(INST_fix$JOURNAL)
# ALLDATA_FIXES<-inner_join(ALLDATA,INST_fix,by=c("JOURNAL","editor_id","YEAR"))
# 
# 
#   not_in_ALLDATA<-droplevels(not_in_ALLDATA)
# ###########
# 
# # ROADMAP FOR INCLUDING CORRECTIONS
# CONBIO_ALLDATA<-ALLDATA %>% filter(JOURNAL=="CONBIO")
# CONBIO_PJ<-read.csv("./Data/PJ_INST_FIX/CONBIO_INST_FIX.csv")
# str(CONBIO_ALLDATA)
# str(CONBIO_PJ)
# CONBIO_PJ<-select(CONBIO_INST_FIX,-JOURNAL,-X1,-FIRST_NA,-MIDDLE_,-LAST_NA)
# # any that are ONLY in CONBIO_ALLDATA and NOT in PJ
# CB_noPJ<-anti_join(CONBIO_ALLDATA,CONBIO_PJ,by=c("editor_id","YEAR"))
# 
# # those from ALLData also in PJ
# CB_and_PJ<-semi_join(CONBIO_ALLDATA,CONBIO_PJ,by=c("editor_id","YEAR"))
# 
# 
# # Any in PJ but not in all data
# CB_PJnoALLDATA<-anti_join(CONBIO_PJ,CONBIO_ALLDATA,by=c("editor_id","YEAR"))
# # those from PJ also in ALLData PJ
# CB_PJand_ALLDATA<-semi_join(CONBIO_PJ,CONBIO_ALLDATA,by=c("editor_id","YEAR"))
# 
# #now the ones in both (meaning they had to be 2x for some reason)
# nrow(CB_and_PJ)
# nrow(CB_PJand_ALLDATA)
# CB_2check<-full_join(CB_and_PJ,CB_PJand_ALLDATA,by=c("editor_id","YEAR"))
# str(CB_2check)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# CONBIO<-droplevels(CONBIO)
# # CONBIO<-select(CONBIO,-X)
# CONBIO_PJ<-select(CONBIO_INST_FIX,-X)
# CONBIO_INST_FIX$editor_id<-as.integer(CONBIO_INST_FIX$editor_id)
# CB_2check<-full_join(CONBIO,CONBIO_INST_FIX,by=c("editor_id","YEAR"))
# str(CB_2check)
# names(CB_2check)
# 
# 
# 
# which(is.na(CB_2check$JOURNAL.x==CB_2check$JOURNAL.y))
# CB_2check$INST.CB_2check<-CB_2check$INST.x==CB_2check$INST.y
# levels(as.factor(CB_2check$INST.CB_2check))
# CONBIO_INST_fix<-which((CB_2check$INST.x==CB_2check$INST.y)=="FALSE")
# 
# CB_2check$UNIT.CB_2check<-CB_2check$UNIT.x==CB_2check$UNIT.y
# levels(as.factor(CB_2check$UNIT.CB_2check))
# 
# CB_2check$CITY.CB_2check<-CB_2check$CITY.x==CB_2check$CITY.y
# levels(as.factor(CB_2check$CITY.CB_2check))
# 
# CB_2check$STATE.CB_2check<-CB_2check$STATE.x==CB_2check$STATE.y
# levels(as.factor(CB_2check$STATE.CB_2check))
# 
# CB_2check$COUNTRY.CB_2check<-CB_2check$COUNTRY.x==CB_2check$COUNTRY.y                               
# levels(as.factor(CB_2check$COUNTRY.CB_2check))
# 
# CONBIO_INST_fix<-which((CB_2check$INST.x==CB_2check$INST.y)=="FALSE"|is.na(CB_2check$INST.x==CB_2check$INST.y))
# CONBIO_UNIT_fix<-which((CB_2check$UNIT.x==CB_2check$UNIT.y)=="FALSE"|is.na(CB_2check$UNIT.x==CB_2check$UNIT.y))
# CONBIO_CITY_fix<-which((CB_2check$CITY.x==CB_2check$CITY.y)=="FALSE"|is.na(CB_2check$CITY.x==CB_2check$CITY.y))
# CONBIO_STATE_fix<-which((CB_2check$STATE.x==CB_2check$STATE.y)=="FALSE"|is.na(CB_2check$STATE.x==CB_2check$STATE.y))
# CONBIO_COUNTRY_fix<-which((CB_2check$COUNTRY.x==CB_2check$COUNTRY.y)=="FALSE"|is.na(CB_2check$COUNTRY.x==CB_2check$COUNTRY.y))
# 
# CB_2check$COUNTRY.x[CB_2check$editor_id==11]<-"England"
# 
# # 
# # 
# # levels(CB_2check$FIRST_NAME.x)<-c(levels(as.factor(CB_2check$FIRST_NAME.x)),levels(as.factor(CB_2check$FIRST_NAME.y))) # need to have the same levels of factor for x and y
# # levels(CB_2check$FIRST_NAME.y)<-c(levels(as.factor(CB_2check$FIRST_NAME.x)),levels(as.factor(CB_2check$FIRST_NAME.y)))# need to have the same levels of factor for x and y
# # CB_2check$FIRST_NAME.CB_2check<-as.factor(CB_2check$FIRST_NAME.x)==as.factor(CB_2check$FIRST_NAME.y)
# # 
# # levels(CB_2check$MIDDLE_NAME.x)<-c(levels(as.factor(CB_2check$MIDDLE_NAME.x)),levels(as.factor(CB_2check$MIDDLE_NAME.y))) # need to have the same levels of factor for x and y
# # levels(CB_2check$MIDDLE_NAME.y)<-c(levels(as.factor(CB_2check$MIDDLE_NAME.x)),levels(as.factor(CB_2check$MIDDLE_NAME.y)))# need to have the same levels of factor for x and y
# # CB_2check$MIDDLE_NAME.CB_2check<-as.factor(CB_2check$MIDDLE_NAME.x)==as.factor(CB_2check$MIDDLE_NAME.y)
# # 
# # levels(CB_2check$LAST_NAME.x)<-c(levels(as.factor(CB_2check$LAST_NAME.x)),levels(as.factor(CB_2check$LAST_NAME.y))) # need to have the same levels of factor for x and y
# # levels(CB_2check$LAST_NAME.y)<-c(levels(as.factor(CB_2check$LAST_NAME.x)),levels(as.factor(CB_2check$LAST_NAME.y)))# need to have the same levels of factor for x and y
# CB_2check$FIRST_NAME.test<-CB_2check$FIRST_NAME.x==CB_2check$FIRST_NAME.y
# CB_2check$MIDDLE_NAME.test<-CB_2check$MIDDLE_NAME.x==CB_2check$MIDDLE_NAME.y
# CB_2check$LAST_NAME.test<-CB_2check$LAST_NAME.x==CB_2check$LAST_NAME.y
# 
# names(CB_2check)
# 
#              
#                
# 
# 
# CB_2check<-CB_2check %>% select("JOURNAL.x","JOURNAL.y","YEAR","editor_id","FIRST_NAME.x","FIRST_NAME.y","FIRST_NAME.test",
#                       "MIDDLE_NAME.x","MIDDLE_NAME.y","MIDDLE_NAME.test","LAST_NAME.x","LAST_NAME.y","LAST_NAME.test",
#                       "INST.x","INST.y","INST.test","UNIT.x","UNIT.y","UNIT.CB_2check","CITY.x","CITY.y","CITY.CB_2check",
#                       "STATE.x","STATE.y","STATE.test","COUNTRY.x","COUNTRY.y","COUNTRY.test","NOTES.x","NOTES.y")
# 
# 
# 
# CONBIO_INST_checks<-CB_2check[CONBIO_INST_fix,]
# CONBIO_INST_checks<-arrange(CONBIO_INST_checks,INST.CB_2check)
# 
# CONBIO_UNIT_checks<-CB_2check[CONBIO_UNIT_fix,]
# CONBIO_UNIT_checks<-arrange(CONBIO_UNIT_checks,UNIT.CB_2check)
# 
# CONBIO_CITY_checks<-CB_2check[CONBIO_CITY_fix,]
# CONBIO_CITY_checks<-arrange(CONBIO_CITY_checks,CITY.CB_2check)
# 
# CONBIO_STATE_checks<-CB_2check[CONBIO_STATE_fix,]
# CONBIO_STATE_checks<-arrange(CONBIO_STATE_checks,STATE.CB_2check)
# 
# CONBIO_COUNTRY_checks<-CB_2check[CONBIO_COUNTRY_fix,]
# CONBIO_COUNTRY_checks<-arrange(CONBIO_COUNTRY_checks,COUNTRY.CB_2check)


# This will load and the corrected files and make the required changes.

library(tidyverse)

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




INST_fix<-bind_rows(multi2b,multi2a,multi1,BITR_inst,JECOL_inst,AMNAT_inst,JAPE_inst,JBIOG_inst,LECO_inst) %>% 
  distinct(editor_id,JOURNAL,YEAR,.keep_all= TRUE) %>%     #there are some duplicates, best to remove them
  arrange(JOURNAL,editor_id,YEAR)
INST_fix$INST<-gsub("UNKNOWN","missing",INST_fix$INST)  #Replace "unknwon with NA 
INST_fix$UNIT<-gsub("UNKNOWN","missing",INST_fix$UNIT)
INST_fix$CITY<-gsub("UNKNOWN","missing",INST_fix$CITY)
INST_fix$STATE<-gsub("UNKNOWN","missing",INST_fix$STATE)

source("institution_cleaner.R")
INST_fix<-institution_cleaner(INST_fix)

levels(as.factor(INST_fix$JOURNAL))

AG_INST_FIX<-INST_fix %>% filter(JOURNAL=="AGRONOMY") %>% 
  arrange(editor_id,YEAR) %>% select(-"X1")
write.csv(AG_INST_FIX, file="./Data/PJ_INST_FIX/AG_INST_FIX.csv", row.names = T) #export it as a csv file

AG


AMNAT_INST_FIX<-INST_fix %>% filter(JOURNAL=="AMNAT") %>% 
  arrange(editor_id,YEAR) %>% select(-"X1")


write.csv(AMNAT_INST_FIX, file="./Data/PJ_INST_FIX/AMNAT_INST_FIX.csv", row.names = T) #export it as a csv file

AREES_INST_FIX<-INST_fix %>% filter(JOURNAL=="AREES") %>% 
  arrange(editor_id,YEAR) %>% select(-"X1")



write.csv(AREES_INST_FIX, file="./Data/PJ_INST_FIX/AREES_INST_FIX.csv", row.names = T) #export it as a csv file

BITR_INST_FIX<-INST_fix %>% filter(JOURNAL=="BITR") %>% 
  arrange(editor_id,YEAR) %>% select(-"X1")

write.csv(BITR_INST_FIX, file="./Data/PJ_INST_FIX/BITR_INST_FIX.csv", row.names = T) #export it as a csv file

CONBIO_INST_FIX<-INST_fix %>% filter(JOURNAL=="CONBIO") %>% 
  arrange(editor_id,YEAR) %>% select(-"X1")

write.csv(CONBIO_INST_FIX, file="./Data/PJ_INST_FIX/CONBIO_INST_FIX.csv", row.names = T) #export it as a csv file

EVOL_INST_FIX<-INST_fix %>% filter(JOURNAL=="EVOL") %>% 
  arrange(editor_id,YEAR) %>% select(-"X1")

write.csv(EVOL_INST_FIX, file="./Data/PJ_INST_FIX/EVOL_INST_FIX.csv", row.names = T) #export it as a csv file

FEM_INST_FIX<-INST_fix %>% filter(JOURNAL=="FEM") %>% 
  arrange(editor_id,YEAR) %>% select(-"X1")

  write.csv(FEM_INST_FIX, file="./Data/PJ_INST_FIX/FEM_INST_FIX.csv", row.names = T) #export it as a csv file

NEWPHYT_INST_FIX<-INST_fix %>% filter(JOURNAL=="NEWPHYT") %>% 
  arrange(editor_id,YEAR) %>% select(-"X1")

write.csv(NEWPHYT_INST_FIX, file="./Data/PJ_INST_FIX/NEWPHYT_INST_FIX.csv", row.names = T) #export it as a csv file

JECOL_INST_FIX<-INST_fix %>% filter(JOURNAL=="JECOL") %>% 
  arrange(editor_id,YEAR) %>% select(-"X1")

write.csv(JECOL_INST_FIX, file="./Data/PJ_INST_FIX/JECOL_INST_FIX.csv", row.names = T) #export it as a csv file

JAPE_INST_FIX<-INST_fix %>% filter(JOURNAL=="JAPE") %>% 
  arrange(editor_id,YEAR) %>% select(-"X1")

write.csv(JAPE_INST_FIX, file="./Data/PJ_INST_FIX/JAPE_INST_FIX.csv", row.names = T) #export it as a csv file




LECO_INST_FIX<-INST_fix %>% filter(JOURNAL=="LECO") %>% 
  arrange(editor_id,YEAR) %>% select(-"X1")


write.csv(LECO_INST_FIX, file="./Data/PJ_INST_FIX/LECO_INST_FIX.csv", row.names = T) #export it as a csv file

JBIOG_INST_FIX<-INST_fix %>% filter(JOURNAL=="JBIOG") %>% 
  arrange(editor_id,YEAR) %>% select(-"X1")
write.csv(JBIOG_INST_FIX, file="./Data/PJ_INST_FIX/JBIOG_INST_FIX.csv", row.names = T) #export it as a csv file

###########

CORRECT<-INST_fix %>% select(editor_id,JOURNAL,YEAR,LAST_NAME) # 4374
CORRECT$source<-"correct"
ORIG<-ALLDATA%>% select(editor_id,JOURNAL,YEAR,LAST_NAME)  # 25602
ORIG$source<-"orig"
foo<-inner_join(CORRECT,ORIG,by=c("editor_id","JOURNAL","YEAR")) #in correct but not orig 24
foo$check<-(foo$LAST_NAME.x==foo$LAST_NAME.y)
foo<-inner_join(ORIG,CORRECT,by=c("editor_id","JOURNAL","YEAR")) #in correct but not orig 24
foo2<-anti_join(CORRECT,ORIG,by=c("editor_id","JOURNAL","YEAR")) # in orig but not correct 21085
nrow(ORIG)
nrow(CORRECT)
nrow(foo)
nrow(foo2)

duplicated_ALLDATA<-ALLDATA %>% 
  group_by(editor_id,JOURNAL,YEAR) %>% 
  filter(n()>1)


nrow(ORIG)+nrow(foo)
nrow(CORRECT)+nrow(foo2)+nrow(foo)
not_in_ALLDATA<-anti_join(INST_fix,ALLDATA,by="editor_id","JOURNAL")
not_in_ALLDATA<-anti_join(ALLDATA,INST_fix)
nrow(ALLDATA)
nrow(INST_fix)
nrow(not_in_ALLDATA)
nrow(not_in_ALLDATA)+nrow(INST_fix)


str(ALLDATA)
str(INST_fix)
ALLDATA$editor_id<-as.numeric(ALLDATA$editor_id)
levels(as.factor(ALLDATA$JOURNAL))
levels(INST_fix$JOURNAL)
ALLDATA_FIXES<-inner_join(ALLDATA,INST_fix,by=c("JOURNAL","editor_id","YEAR"))


  not_in_ALLDATA<-droplevels(not_in_ALLDATA)
###########

# ROADMAP FOR INCLUDING CORRECTIONS
CONBIO_ALLDATA<-ALLDATA %>% filter(JOURNAL=="CONBIO")
CONBIO_PJ<-read.csv("./Data/PJ_INST_FIX/CONBIO_INST_FIX.csv")
str(CONBIO_ALLDATA)
str(CONBIO_PJ)
CONBIO_PJ<-select(CONBIO_INST_FIX,-JOURNAL,-X1,-FIRST_NA,-MIDDLE_,-LAST_NA)
# any that are ONLY in CONBIO_ALLDATA and NOT in PJ
CB_noPJ<-anti_join(CONBIO_ALLDATA,CONBIO_PJ,by=c("editor_id","YEAR"))

# those from ALLData also in PJ
CB_and_PJ<-semi_join(CONBIO_ALLDATA,CONBIO_PJ,by=c("editor_id","YEAR"))


# Any in PJ but not in all data
CB_PJnoALLDATA<-anti_join(CONBIO_PJ,CONBIO_ALLDATA,by=c("editor_id","YEAR"))
# those from PJ also in ALLData PJ
CB_PJand_ALLDATA<-semi_join(CONBIO_PJ,CONBIO_ALLDATA,by=c("editor_id","YEAR"))

#now the ones in both (meaning they had to be 2x for some reason)
nrow(CB_and_PJ)
nrow(CB_PJand_ALLDATA)
CB_2check<-full_join(CB_and_PJ,CB_PJand_ALLDATA,by=c("editor_id","YEAR"))
str(CB_2check)









CONBIO<-droplevels(CONBIO)
# CONBIO<-select(CONBIO,-X)
CONBIO_PJ<-select(CONBIO_INST_FIX,-X)
CONBIO_INST_FIX$editor_id<-as.integer(CONBIO_INST_FIX$editor_id)
CB_2check<-full_join(CONBIO,CONBIO_INST_FIX,by=c("editor_id","YEAR"))
str(CB_2check)
names(CB_2check)



which(is.na(CB_2check$JOURNAL.x==CB_2check$JOURNAL.y))
CB_2check$INST.CB_2check<-CB_2check$INST.x==CB_2check$INST.y
levels(as.factor(CB_2check$INST.CB_2check))
CONBIO_INST_fix<-which((CB_2check$INST.x==CB_2check$INST.y)=="FALSE")

CB_2check$UNIT.CB_2check<-CB_2check$UNIT.x==CB_2check$UNIT.y
levels(as.factor(CB_2check$UNIT.CB_2check))

CB_2check$CITY.CB_2check<-CB_2check$CITY.x==CB_2check$CITY.y
levels(as.factor(CB_2check$CITY.CB_2check))

CB_2check$STATE.CB_2check<-CB_2check$STATE.x==CB_2check$STATE.y
levels(as.factor(CB_2check$STATE.CB_2check))

CB_2check$COUNTRY.CB_2check<-CB_2check$COUNTRY.x==CB_2check$COUNTRY.y                               
levels(as.factor(CB_2check$COUNTRY.CB_2check))

CONBIO_INST_fix<-which((CB_2check$INST.x==CB_2check$INST.y)=="FALSE"|is.na(CB_2check$INST.x==CB_2check$INST.y))
CONBIO_UNIT_fix<-which((CB_2check$UNIT.x==CB_2check$UNIT.y)=="FALSE"|is.na(CB_2check$UNIT.x==CB_2check$UNIT.y))
CONBIO_CITY_fix<-which((CB_2check$CITY.x==CB_2check$CITY.y)=="FALSE"|is.na(CB_2check$CITY.x==CB_2check$CITY.y))
CONBIO_STATE_fix<-which((CB_2check$STATE.x==CB_2check$STATE.y)=="FALSE"|is.na(CB_2check$STATE.x==CB_2check$STATE.y))
CONBIO_COUNTRY_fix<-which((CB_2check$COUNTRY.x==CB_2check$COUNTRY.y)=="FALSE"|is.na(CB_2check$COUNTRY.x==CB_2check$COUNTRY.y))

CB_2check$COUNTRY.x[CB_2check$editor_id==11]<-"England"

# 
# 
# levels(CB_2check$FIRST_NAME.x)<-c(levels(as.factor(CB_2check$FIRST_NAME.x)),levels(as.factor(CB_2check$FIRST_NAME.y))) # need to have the same levels of factor for x and y
# levels(CB_2check$FIRST_NAME.y)<-c(levels(as.factor(CB_2check$FIRST_NAME.x)),levels(as.factor(CB_2check$FIRST_NAME.y)))# need to have the same levels of factor for x and y
# CB_2check$FIRST_NAME.CB_2check<-as.factor(CB_2check$FIRST_NAME.x)==as.factor(CB_2check$FIRST_NAME.y)
# 
# levels(CB_2check$MIDDLE_NAME.x)<-c(levels(as.factor(CB_2check$MIDDLE_NAME.x)),levels(as.factor(CB_2check$MIDDLE_NAME.y))) # need to have the same levels of factor for x and y
# levels(CB_2check$MIDDLE_NAME.y)<-c(levels(as.factor(CB_2check$MIDDLE_NAME.x)),levels(as.factor(CB_2check$MIDDLE_NAME.y)))# need to have the same levels of factor for x and y
# CB_2check$MIDDLE_NAME.CB_2check<-as.factor(CB_2check$MIDDLE_NAME.x)==as.factor(CB_2check$MIDDLE_NAME.y)
# 
# levels(CB_2check$LAST_NAME.x)<-c(levels(as.factor(CB_2check$LAST_NAME.x)),levels(as.factor(CB_2check$LAST_NAME.y))) # need to have the same levels of factor for x and y
# levels(CB_2check$LAST_NAME.y)<-c(levels(as.factor(CB_2check$LAST_NAME.x)),levels(as.factor(CB_2check$LAST_NAME.y)))# need to have the same levels of factor for x and y
CB_2check$FIRST_NAME.test<-CB_2check$FIRST_NAME.x==CB_2check$FIRST_NAME.y
CB_2check$MIDDLE_NAME.test<-CB_2check$MIDDLE_NAME.x==CB_2check$MIDDLE_NAME.y
CB_2check$LAST_NAME.test<-CB_2check$LAST_NAME.x==CB_2check$LAST_NAME.y

names(CB_2check)

             
               


CB_2check<-CB_2check %>% select("JOURNAL.x","JOURNAL.y","YEAR","editor_id","FIRST_NAME.x","FIRST_NAME.y","FIRST_NAME.test",
                      "MIDDLE_NAME.x","MIDDLE_NAME.y","MIDDLE_NAME.test","LAST_NAME.x","LAST_NAME.y","LAST_NAME.test",
                      "INST.x","INST.y","INST.test","UNIT.x","UNIT.y","UNIT.CB_2check","CITY.x","CITY.y","CITY.CB_2check",
                      "STATE.x","STATE.y","STATE.test","COUNTRY.x","COUNTRY.y","COUNTRY.test","NOTES.x","NOTES.y")



CONBIO_INST_checks<-CB_2check[CONBIO_INST_fix,]
CONBIO_INST_checks<-arrange(CONBIO_INST_checks,INST.CB_2check)

CONBIO_UNIT_checks<-CB_2check[CONBIO_UNIT_fix,]
CONBIO_UNIT_checks<-arrange(CONBIO_UNIT_checks,UNIT.CB_2check)

CONBIO_CITY_checks<-CB_2check[CONBIO_CITY_fix,]
CONBIO_CITY_checks<-arrange(CONBIO_CITY_checks,CITY.CB_2check)

CONBIO_STATE_checks<-CB_2check[CONBIO_STATE_fix,]
CONBIO_STATE_checks<-arrange(CONBIO_STATE_checks,STATE.CB_2check)

CONBIO_COUNTRY_checks<-CB_2check[CONBIO_COUNTRY_fix,]
CONBIO_COUNTRY_checks<-arrange(CONBIO_COUNTRY_checks,COUNTRY.CB_2check)


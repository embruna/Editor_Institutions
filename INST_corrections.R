# This will load and the corrected files and make the required changes.

library(tidyverse)


multi1<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_1.csv", col_names = TRUE)
multi1<-multi1 %>% fill(INST,UNIT,CITY,STATE)

multi2a<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_2a.csv", col_names = TRUE)
multi2a<-filter(multi2a,JOURNAL=="CONBIO"|JOURNAL=="NEWPHYT")#delete out other journals this is conbio and new phyt

multi2b<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_2b.csv", col_names = TRUE)
multi2b<-multi2b %>% fill(INST,UNIT,CITY,STATE) %>% filter(JOURNAL=="CONBIO"|JOURNAL=="NEWPHYT") #delete out other journals this is conbio and new phyt

BITR_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_3_BITR.csv", col_names = TRUE)
BITR_inst<-BITR_inst %>% fill(INST,UNIT,CITY,STATE)

AMNAT_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_4_AMNAT.csv", col_names = TRUE)
AMNAT_inst<-AMNAT_inst %>% fill(INST,UNIT,CITY,STATE)%>% filter(JOURNAL=="AMNAT")


JECOL_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_6_JEcol.csv", col_names = TRUE)
JECOL_inst<-JECOL_inst %>% fill(INST,UNIT,CITY,STATE)%>% filter(JOURNAL=="JECOL")

JAPE_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_7_JAPE.csv", col_names = TRUE)
JAPE_inst<-JAPE_inst %>% fill(INST,UNIT,CITY,STATE,NOTES)%>% filter(JOURNAL=="JAPE")



# Round 7: JAPE

INST_fix<-bind_rows(multi2b,multi2a,multi1,BITR_inst,JECOL_inst,AMNAT_inst,JAPE_inst) %>% 
  distinct(editor_id,JOURNAL,YEAR,.keep_all= TRUE) %>%     #there are some duplicates, best to remove them
  arrange(JOURNAL,editor_id,YEAR)
INST_fix$INST<-gsub("UNKNOWN","missing",INST_fix$INST)  #Replace "unknwon with NA 
INST_fix$UNIT<-gsub("UNKNOWN","missing",INST_fix$UNIT)
INST_fix$CITY<-gsub("UNKNOWN","missing",INST_fix$CITY)
INST_fix$STATE<-gsub("UNKNOWN","missing",INST_fix$STATE)

levels(as.factor(INST_fix$JOURNAL))

AG_INST_FIX<-INST_fix %>% filter(JOURNAL=="AGRONOMY")
write.csv(AG_INST_FIX, file="./Data/PJ_INST_FIX/AG_INST_FIX.csv", row.names = T) #export it as a csv file

AMNAT_INST_FIX<-INST_fix %>% filter(JOURNAL=="AMNAT")
write.csv(AMNAT_INST_FIX, file="./Data/PJ_INST_FIX/AMNAT_INST_FIX.csv", row.names = T) #export it as a csv file

AREES_INST_FIX<-INST_fix %>% filter(JOURNAL=="AREES")
write.csv(AREES_INST_FIX, file="./Data/PJ_INST_FIX/AREES_INST_FIX.csv", row.names = T) #export it as a csv file

BITR_INST_FIX<-INST_fix %>% filter(JOURNAL=="BITR")
write.csv(BITR_INST_FIX, file="./Data/PJ_INST_FIX/BITR_INST_FIX.csv", row.names = T) #export it as a csv file

CONBIO_INST_FIX<-INST_fix %>% filter(JOURNAL=="CONBIO")
write.csv(CONBIO_INST_FIX, file="./Data/PJ_INST_FIX/CONBIO_INST_FIX.csv", row.names = T) #export it as a csv file

EVOL_INST_FIX<-INST_fix %>% filter(JOURNAL=="EVOL")
write.csv(EVOL_INST_FIX, file="./Data/PJ_INST_FIX/EVOL_INST_FIX.csv", row.names = T) #export it as a csv file

FEM_INST_FIX<-INST_fix %>% filter(JOURNAL=="FEM")
write.csv(FEM_INST_FIX, file="./Data/PJ_INST_FIX/FEM_INST_FIX.csv", row.names = T) #export it as a csv file

NEWPHYT_INST_FIX<-INST_fix %>% filter(JOURNAL=="NEWPHYT")
write.csv(NEWPHYT_INST_FIX, file="./Data/PJ_INST_FIX/NEWPHYT_INST_FIX.csv", row.names = T) #export it as a csv file

JECOL_INST_FIX<-INST_fix %>% filter(JOURNAL=="JECOL")
write.csv(JECOL_INST_FIX, file="./Data/PJ_INST_FIX/JECOL_INST_FIX.csv", row.names = T) #export it as a csv file

JAPE_INST_FIX<-INST_fix %>% filter(JOURNAL=="JAPE")
write.csv(JAPE_INST_FIX, file="./Data/PJ_INST_FIX/JAPE_INST_FIX.csv", row.names = T) #export it as a csv file







###########

# ROADMAP FOR INCLUDING CORRECTIONS

CONBIO_INST_FIX<-read.csv("./Data/PJ_INST_FIX/CONBIO_INST_FIX.csv")


str(CONBIO)
str(CONBIO_INST_FIX)
CONBIO<-droplevels(CONBIO)
CONBIO<-select(CONBIO,-X)
CONBIO_INST_FIX<-select(CONBIO_INST_FIX,-X)


CONBIO_INST_FIX$editor_id<-as.factor(CONBIO_INST_FIX$editor_id)
test<-full_join(CONBIO,CONBIO_INST_FIX,by=c("editor_id","YEAR"))
str(test)
names(test)



which(is.na(test$JOURNAL.x==test$JOURNAL.y))

test$INST.test<-test$INST.x==test$INST.y
levels(as.factor(test$INST.test))
CONBIO_INST_fix<-which((test$INST.x==test$INST.y)=="FALSE")

test$UNIT.test<-test$UNIT.x==test$UNIT.y
levels(as.factor(test$UNIT.test))

test$CITY.test<-test$CITY.x==test$CITY.y
levels(as.factor(test$CITY.test))

test$STATE.test<-test$STATE.x==test$STATE.y
levels(as.factor(test$STATE.test))

test$COUNTRY.test<-test$COUNTRY.x==test$COUNTRY.y                               
levels(as.factor(test$COUNTRY.test))

CONBIO_INST_fix<-which((test$INST.x==test$INST.y)=="FALSE"|is.na(test$INST.x==test$INST.y))
CONBIO_UNIT_fix<-which((test$UNIT.x==test$UNIT.y)=="FALSE"|is.na(test$UNIT.x==test$UNIT.y))
CONBIO_CITY_fix<-which((test$CITY.x==test$CITY.y)=="FALSE"|is.na(test$CITY.x==test$CITY.y))
CONBIO_STATE_fix<-which((test$STATE.x==test$STATE.y)=="FALSE"|is.na(test$STATE.x==test$STATE.y))
CONBIO_COUNTRY_fix<-which((test$COUNTRY.x==test$COUNTRY.y)=="FALSE"|is.na(test$COUNTRY.x==test$COUNTRY.y))

test$COUNTRY.x[test$editor_id==11]<-"England"

# 
# 
# levels(test$FIRST_NAME.x)<-c(levels(as.factor(test$FIRST_NAME.x)),levels(as.factor(test$FIRST_NAME.y))) # need to have the same levels of factor for x and y
levels(test$FIRST_NAME.y)<-c(levels(as.factor(test$FIRST_NAME.x)),levels(as.factor(test$FIRST_NAME.y)))# need to have the same levels of factor for x and y
test$FIRST_NAME.test<-as.factor(test$FIRST_NAME.x)==as.factor(test$FIRST_NAME.y)

levels(test$MIDDLE_NAME.x)<-c(levels(as.factor(test$MIDDLE_NAME.x)),levels(as.factor(test$MIDDLE_NAME.y))) # need to have the same levels of factor for x and y
levels(test$MIDDLE_NAME.y)<-c(levels(as.factor(test$MIDDLE_NAME.x)),levels(as.factor(test$MIDDLE_NAME.y)))# need to have the same levels of factor for x and y
test$MIDDLE_NAME.test<-as.factor(test$MIDDLE_NAME.x)==as.factor(test$MIDDLE_NAME.y)

levels(test$LAST_NAME.x)<-c(levels(as.factor(test$LAST_NAME.x)),levels(as.factor(test$LAST_NAME.y))) # need to have the same levels of factor for x and y
levels(test$LAST_NAME.y)<-c(levels(as.factor(test$LAST_NAME.x)),levels(as.factor(test$LAST_NAME.y)))# need to have the same levels of factor for x and y

test$LAST_NAME.test<-test$LAST_NAME.x==test$LAST_NAME.y

names(test)
test<-test %>% select("JOURNAL.x","JOURNAL.y","YEAR","VOLUME","ISSUE","editor_id","FIRST_NAME.x","FIRST_NAME.y","FIRST_NAME.test",
                      "MIDDLE_NAME.x","MIDDLE_NAME.y","MIDDLE_NAME.test","LAST_NAME.x","LAST_NAME.y","LAST_NAME.test",
                      "INST.x","INST.y","INST.test","UNIT.x","UNIT.y","UNIT.test","CITY.x","CITY.y","CITY.test",
                      "STATE.x","STATE.y","STATE.test","COUNTRY.x","COUNTRY.y","COUNTRY.test","NOTES.x","NOTES.y","geo.code","GENDER")



CONBIO_INST_checks<-test[CONBIO_INST_fix,]
CONBIO_INST_checks<-arrange(CONBIO_INST_checks,INST.test)

CONBIO_UNIT_checks<-test[CONBIO_UNIT_fix,]
CONBIO_UNIT_checks<-arrange(CONBIO_UNIT_checks,UNIT.test)

CONBIO_CITY_checks<-test[CONBIO_CITY_fix,]
CONBIO_CITY_checks<-arrange(CONBIO_CITY_checks,CITY.test)

CONBIO_STATE_checks<-test[CONBIO_STATE_fix,]
CONBIO_STATE_checks<-arrange(CONBIO_STATE_checks,STATE.test)

CONBIO_COUNTRY_checks<-test[CONBIO_COUNTRY_fix,]
CONBIO_COUNTRY_checks<-arrange(CONBIO_COUNTRY_checks,COUNTRY.test)


PJ_JBIOG_corrections <- function(ORIGINAL_DATA) {
  # ORIGINAL_DATA<-ALLDATA  
  library(tidyverse)

  
JBIOG_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_JBIOG.csv", col_names = TRUE)
  
names(JBIOG_inst)

JBIOG_inst<-JBIOG_inst %>%  select(-X1) 
JBIOG_inst<-JBIOG_inst %>% select(JOURNAL, YEAR,editor_id, FIRST_NAME,
                                MIDDLE_NAME, LAST_NAME, INST,CITY,NOTES,
                                correct_INST,correct_CITY,correct_STATE,correct_COUNTRY)

# ORIGINAL_DATA<-ALLDATA
JBIOG<-filter(ORIGINAL_DATA,JOURNAL=="JBIOG")

colnames(JBIOG)
colnames(ORIGINAL_DATA)


# remove OECOL FROM THE COMPLETE DATASET, WILL REBIND AFTER ADDING CORRECTIONS
ORIGINAL_DATA<-ORIGINAL_DATA %>% filter(JOURNAL!="JBIOG")

# INSERT THE CORRECTIONS TO OECOL AND FILL
JBIOG<-JBIOG %>% na_if("missing")
JBIOG_inst<-JBIOG_inst %>% na_if("missing")


JBIOG$INST<-as.character(JBIOG$INST)

JBIOG_inst$editor_id<-as.factor(JBIOG_inst$editor_id)
JBIOG$editor_id<-as.factor(JBIOG$editor_id)
colnames(JBIOG)
colnames(JBIOG_inst)
JBIOG<-full_join(JBIOG, JBIOG_inst, by = c("LAST_NAME","FIRST_NAME","YEAR"),all = T)
colnames(JBIOG)
JBIOG <- JBIOG %>%
  mutate(CITY.x = ifelse((is.na(CITY.x)|CITY.x=="missing"), CITY.y, CITY.x)) %>%
  select(-CITY.y) %>%
  rename("CITY"="CITY.x") %>%
  mutate(CITY = ifelse(is.na(correct_CITY), CITY, correct_CITY)) %>%
  select(-correct_CITY)
JBIOG <- JBIOG %>%
  mutate(INST.x = ifelse((is.na(INST.x)|INST.x=="missing"), INST.y, INST.x)) %>%
  select(-INST.y) %>%
  rename("INST"="INST.x") %>% 
  mutate(INST = ifelse(is.na(correct_INST), INST, correct_INST)) %>%
  select(-correct_INST)
JBIOG <- JBIOG %>%
  select(-JOURNAL.y) %>%
  rename("JOURNAL"="JOURNAL.x")
JBIOG <- JBIOG %>%
  mutate(MIDDLE_NAME.x = ifelse((is.na(MIDDLE_NAME.x)|MIDDLE_NAME.x=="missing"), MIDDLE_NAME.y, MIDDLE_NAME.x)) %>%
  select(-MIDDLE_NAME.y) %>%
  rename("MIDDLE_NAME"="MIDDLE_NAME.x")

JBIOG <- JBIOG %>%
  mutate(NOTES.x = ifelse((is.na(NOTES.x)|NOTES.x=="missing"), NOTES.y, NOTES.x)) %>%
  select(-NOTES.y) %>%
  rename("NOTES"="NOTES.x")
# JBIOG <- JBIOG %>%
#   mutate(UNIT.x = ifelse((is.na(UNIT.x)|UNIT.x=="missing"), UNIT.y, UNIT.x)) %>%
#   select(-UNIT.y) %>%
#   rename("UNIT"="UNIT.x")
JBIOG <- JBIOG %>%
  mutate(STATE.x = ifelse((is.na(STATE.x)|STATE.x=="missing"), STATE.y, STATE.x)) %>%
  select(-STATE.y) %>%
  rename("STATE"="STATE.x") %>% 
  mutate(STATE = ifelse(is.na(correct_STATE), STATE, correct_STATE)) %>%
  select(-correct_STATE)
JBIOG <- JBIOG %>%
  select(-COUNTRY.y) %>%
  rename("COUNTRY"="COUNTRY.x") %>% 
  mutate(COUNTRY = ifelse(is.na(correct_COUNTRY), COUNTRY, correct_COUNTRY)) %>%
  select(-correct_COUNTRY)
JBIOG <- JBIOG %>%
  mutate(editor_id.x = ifelse((is.na(editor_id.x)|editor_id.x=="missing"), editor_id.y, editor_id.x)) %>%
  select(-editor_id.y) %>%
  rename("editor_id"="editor_id.x")
# 
JBIOG$JOURNAL<-"JBIOG"
# 
# JBIOG$LAST_NAME[JBIOG$LAST_NAME=="Lookinbill"]<-"Lookingbill"
# JBIOG$CITY[JBIOG$LAST_NAME=="Overton"]<-NA
# JBIOG$geo.code[JBIOG$LAST_NAME=="Betts"]<-"CAN"

JBIOG<-JBIOG %>% group_by(LAST_NAME,FIRST_NAME) %>% 
  fill(INST,CITY,.direction="down")
# 
# JBIOG$editor_id<-as.factor(JBIOG$editor_id)
# # 
# # Rebind the ORIGINAL DATA AND NOW CORRECTED JBIOG
# 
# str(ORIGINAL_DATA)
# str(JBIOG)
# JBIOG$editor_id<-as.factor(JBIOG$editor_id)
# str(JBIOG_inst)
# ORIGINAL_DATA<-bind_rows(ORIGINAL_DATA,JBIOG)
# colnames(ORIGINAL_DATA)

# rm(JBIOG,JBIOG_inst)
JBIOG$editor_id<-as.character(JBIOG$editor_id)
ORIGINAL_DATA$editor_id<-as.character(ORIGINAL_DATA$editor_id)
return_list <- list(ORIGINAL_DATA,JBIOG)
return(return_list)
}
PJ_LECO_corrections <- function(ORIGINAL_DATA) {
  # ORIGINAL_DATA<-ALLDATA  
  library(tidyverse)
  
LECO_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/newPJCorrections_SHORT_10_LECO.csv", col_names = TRUE)
names(LECO_inst)
LECO_inst<-LECO_inst %>%  select(-X1,-NOTES)
LECO_inst<-LECO_inst %>% select(JOURNAL, YEAR,editor_id, FIRST_NAME,
                                MIDDLE_NAME, LAST_NAME, INST,CITY,
                                correct_INST,correct_CITY,correct_STATE,
                                correct_COUNTRY,correct_NAME_FIRST,
                                correct_NAME_LAST)

# ORIGINAL_DATA<-ALLDATA
LANDSCAPEECO<-filter(ORIGINAL_DATA,JOURNAL=="LECO")

colnames(LANDSCAPEECO)
colnames(ORIGINAL_DATA)


# remove OECOL FROM THE COMPLETE DATASET, WILL REBIND AFTER ADDING CORRECTIONS
ORIGINAL_DATA<-ORIGINAL_DATA %>% filter(JOURNAL!="LECO")

# INSERT THE CORRECTIONS TO OECOL AND FILL
LANDSCAPEECO<-LANDSCAPEECO %>% na_if("missing")
LECO_inst<-LECO_inst %>% na_if("missing")


LANDSCAPEECO$INST<-as.character(LANDSCAPEECO$INST)
LECO_inst$editor_id<-as.character(LECO_inst$editor_id)
LANDSCAPEECO$editor_id<-as.character(LANDSCAPEECO$editor_id)
# 
LANDSCAPEECO<-full_join(LANDSCAPEECO, LECO_inst, by = c("LAST_NAME","FIRST_NAME","YEAR"),all = T)
colnames(LANDSCAPEECO)

LANDSCAPEECO <- LANDSCAPEECO %>%
  mutate(CITY.x = ifelse((is.na(CITY.x)|CITY.x=="missing"), CITY.y, CITY.x)) %>%
  select(-CITY.y) %>%
  rename("CITY"="CITY.x") %>%
  mutate(CITY = ifelse(is.na(correct_CITY), CITY, correct_CITY)) %>%
  select(-correct_CITY)
LANDSCAPEECO <- LANDSCAPEECO %>%
  mutate(INST.x = ifelse((is.na(INST.x)|INST.x=="missing"), INST.y, INST.x)) %>%
  select(-INST.y) %>%
  rename("INST"="INST.x") %>% 
  mutate(INST = ifelse(is.na(correct_INST), INST, correct_INST)) %>%
  select(-correct_INST)
LANDSCAPEECO <- LANDSCAPEECO %>%
  select(-JOURNAL.y) %>%
  rename("JOURNAL"="JOURNAL.x")
LANDSCAPEECO <- LANDSCAPEECO %>%
  mutate(MIDDLE_NAME.x = ifelse((is.na(MIDDLE_NAME.x)|MIDDLE_NAME.x=="missing"), MIDDLE_NAME.y, MIDDLE_NAME.x)) %>%
  select(-MIDDLE_NAME.y) %>%
  rename("MIDDLE_NAME"="MIDDLE_NAME.x")
# 
# LANDSCAPEECO <- LANDSCAPEECO %>%
#   mutate(NOTES.x = ifelse((is.na(NOTES.x)|NOTES.x=="missing"), NOTES.y, NOTES.x)) %>%
#   select(-NOTES.y) %>%
#   rename("NOTES"="NOTES.x")
# LANDSCAPEECO <- LANDSCAPEECO %>%
#   mutate(UNIT.x = ifelse((is.na(UNIT.x)|UNIT.x=="missing"), UNIT.y, UNIT.x)) %>%
#   select(-UNIT.y) %>%
#   rename("UNIT"="UNIT.x")
LANDSCAPEECO <- LANDSCAPEECO %>%
  mutate(STATE = ifelse(is.na(correct_STATE), STATE, correct_STATE)) %>%
  select(-correct_STATE)
LANDSCAPEECO <- LANDSCAPEECO %>%
  mutate(COUNTRY = ifelse(is.na(correct_COUNTRY), COUNTRY, correct_COUNTRY)) %>%
  select(-correct_COUNTRY)
LANDSCAPEECO <- LANDSCAPEECO %>%
  mutate(editor_id.x = ifelse((is.na(editor_id.x)|editor_id.x=="missing"), editor_id.y, editor_id.x)) %>%
  select(-editor_id.y) %>%
  rename("editor_id"="editor_id.x")
# 
LANDSCAPEECO$JOURNAL<-"LANDSCAPEECO"
# 
# LANDSCAPEECO$LAST_NAME[LANDSCAPEECO$LAST_NAME=="Lookinbill"]<-"Lookingbill"
# LANDSCAPEECO$CITY[LANDSCAPEECO$LAST_NAME=="Overton"]<-NA
# LANDSCAPEECO$geo.code[LANDSCAPEECO$LAST_NAME=="Betts"]<-"CAN"

LANDSCAPEECO<-LANDSCAPEECO %>% group_by(LAST_NAME,FIRST_NAME) %>% 
  fill(INST,CITY,.direction="down")



# 
# LANDSCAPEECO <- LANDSCAPEECO %>%
#   mutate(CITY.x = ifelse((is.na(CITY.x)|CITY.x=="missing"), CITY.y, CITY.x)) %>%
#   select(-CITY.y) %>%
#   rename("CITY"="CITY.x")
# LANDSCAPEECO <- LANDSCAPEECO %>%
#   mutate(INST.x = ifelse((is.na(INST.x)|INST.x=="missing"), INST.y, INST.x)) %>%
#   select(-INST.y) %>%
#   rename("INST"="INST.x")
# LANDSCAPEECO <- LANDSCAPEECO %>%
#   select(-JOURNAL.y) %>%
#   rename("JOURNAL"="JOURNAL.x")
# LANDSCAPEECO <- LANDSCAPEECO %>%
#   mutate(MIDDLE_NAME.x = ifelse((is.na(MIDDLE_NAME.x)|MIDDLE_NAME.x=="missing"), MIDDLE_NAME.y, MIDDLE_NAME.x)) %>%
#   select(-MIDDLE_NAME.y) %>%
#   rename("MIDDLE_NAME"="MIDDLE_NAME.x")
# LANDSCAPEECO <- LANDSCAPEECO %>%
#   mutate(NOTES.x = ifelse((is.na(NOTES.x)|NOTES.x=="missing"), NOTES.y, NOTES.x)) %>%
#   select(-NOTES.y) %>%
#   rename("NOTES"="NOTES.x")
# # LANDSCAPEECO <- LANDSCAPEECO %>%
# #   mutate(UNIT.x = ifelse((is.na(UNIT.x)|UNIT.x=="missing"), UNIT.y, UNIT.x)) %>%
# #   select(-UNIT.y) %>%
# #   rename("UNIT"="UNIT.x")
# # LANDSCAPEECO <- LANDSCAPEECO %>%
# #   mutate(STATE.x = ifelse((is.na(STATE.x)|STATE.x=="missing"), STATE.y, STATE.x)) %>%
# #   select(-STATE.y) %>%
# #   rename("STATE"="STATE.x")
# # LANDSCAPEECO <- LANDSCAPEECO %>%
# #   select(-COUNTRY.y) %>%
# #   rename("COUNTRY"="COUNTRY.x")
# # str(LANDSCAPEECO$editor_id.x)
# # str(LANDSCAPEECO$editor_id.y)
# 
# LANDSCAPEECO<- LANDSCAPEECO %>%
#   mutate(editor_id.x = ifelse((is.na(editor_id.x)|editor_id.x=="missing"), editor_id.y, editor_id.x)) %>%
#   select(-editor_id.y) %>%
#   rename("editor_id"="editor_id.x")
# # # 
# LANDSCAPEECO$JOURNAL<-"LECO"
# # 
# # LANDSCAPEECO$LAST_NAME[LANDSCAPEECO$LAST_NAME=="Lookinbill"]<-"Lookingbill"
# # LANDSCAPEECO$CITY[LANDSCAPEECO$LAST_NAME=="Overton"]<-NA
# # LANDSCAPEECO$geo.code[LANDSCAPEECO$LAST_NAME=="Betts"]<-"CAN"
# 
# LANDSCAPEECO<-LANDSCAPEECO %>% group_by(LAST_NAME,FIRST_NAME) %>% 
#   fill(INST,CITY,.direction="down")
# 
# LANDSCAPEECO$editor_id<-as.factor(LANDSCAPEECO$editor_id)
# # 
# # Rebind the ORIGINAL DATA AND NOW CORRECTED LANDSCAPEECO
# 
# str(ORIGINAL_DATA)
# str(LANDSCAPEECO)
# LANDSCAPEECO$editor_id<-as.factor(LANDSCAPEECO$editor_id)
# str(LECO_inst)
# ORIGINAL_DATA<-bind_rows(ORIGINAL_DATA,LANDSCAPEECO)
# colnames(ORIGINAL_DATA)

# rm(LANDSCAPEECO,LECO_inst)
LANDSCAPEECO$editor_id<-as.character(LANDSCAPEECO$editor_id)
LANDSCAPEECO$JOURNAL<-"LECO"
ORIGINAL_DATA$editor_id<-as.character(ORIGINAL_DATA$editor_id)
return_list <- list(ORIGINAL_DATA,LANDSCAPEECO)
return(return_list)
}
PJ_PLANTECOL_corrections <- function(ORIGINAL_DATA) {
  # ORIGINAL_DATA<-ALLDATA  
  library(tidyverse)

  
PLANTECOL_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_PLANTECOL.csv", col_names = TRUE)
  
names(PLANTECOL_inst)

PLANTECOL_inst<-PLANTECOL_inst %>%  select(-X1) 
PLANTECOL_inst<-PLANTECOL_inst %>% select(JOURNAL, YEAR,editor_id, FIRST_NAME,
                                MIDDLE_NAME, LAST_NAME, INST,CITY,NOTES,
                                correct_INST,correct_CITY,correct_STATE,correct_FIRST_NAME,correct_MIDDLE_NAME)

# ORIGINAL_DATA<-ALLDATA
PLANTECOL<-filter(ORIGINAL_DATA,JOURNAL=="PLANTECOL")

colnames(PLANTECOL)
colnames(ORIGINAL_DATA)


# remove OECOL FROM THE COMPLETE DATASET, WILL REBIND AFTER ADDING CORRECTIONS
ORIGINAL_DATA<-ORIGINAL_DATA %>% filter(JOURNAL!="PLANTECOL")

# INSERT THE CORRECTIONS TO OECOL AND FILL
PLANTECOL<-PLANTECOL %>% na_if("missing")
PLANTECOL_inst<-PLANTECOL_inst %>% na_if("missing")


PLANTECOL$INST<-as.character(PLANTECOL$INST)

PLANTECOL_inst$editor_id<-as.factor(PLANTECOL_inst$editor_id)
PLANTECOL$editor_id<-as.factor(PLANTECOL$editor_id)
colnames(PLANTECOL)
colnames(PLANTECOL_inst)
PLANTECOL<-full_join(PLANTECOL, PLANTECOL_inst, by = c("LAST_NAME","FIRST_NAME","YEAR"),all = T)
colnames(PLANTECOL)
PLANTECOL <- PLANTECOL %>%
  mutate(CITY.x = ifelse((is.na(CITY.x)|CITY.x=="missing"), CITY.y, CITY.x)) %>%
  select(-CITY.y) %>%
  rename("CITY"="CITY.x") %>%
  mutate(CITY = ifelse(is.na(correct_CITY), CITY, correct_CITY)) %>%
  select(-correct_CITY)
PLANTECOL <- PLANTECOL %>%
  mutate(INST.x = ifelse((is.na(INST.x)|INST.x=="missing"), INST.y, INST.x)) %>%
  select(-INST.y) %>%
  rename("INST"="INST.x") %>% 
  mutate(INST = ifelse(is.na(correct_INST), INST, correct_INST)) %>%
  select(-correct_INST)
PLANTECOL <- PLANTECOL %>%
  select(-JOURNAL.y) %>%
  rename("JOURNAL"="JOURNAL.x")
PLANTECOL <- PLANTECOL %>%
  mutate(MIDDLE_NAME.x = ifelse((is.na(MIDDLE_NAME.x)|MIDDLE_NAME.x=="missing"), MIDDLE_NAME.y, MIDDLE_NAME.x)) %>%
  select(-MIDDLE_NAME.y) %>%
  rename("MIDDLE_NAME"="MIDDLE_NAME.x") %>% 
  mutate(MIDDLE_NAME = ifelse(is.na(correct_MIDDLE_NAME), MIDDLE_NAME, correct_MIDDLE_NAME)) %>%
  select(-correct_MIDDLE_NAME)
PLANTECOL <- PLANTECOL %>%
  mutate(FIRST_NAME = ifelse(is.na(correct_FIRST_NAME), FIRST_NAME, correct_FIRST_NAME)) %>%
  select(-correct_FIRST_NAME)
PLANTECOL <- PLANTECOL %>%
  mutate(NOTES.x = ifelse((is.na(NOTES.x)|NOTES.x=="missing"), NOTES.y, NOTES.x)) %>%
  select(-NOTES.y) %>%
  rename("NOTES"="NOTES.x")
# PLANTECOL <- PLANTECOL %>%
#   mutate(UNIT.x = ifelse((is.na(UNIT.x)|UNIT.x=="missing"), UNIT.y, UNIT.x)) %>%
#   select(-UNIT.y) %>%
#   rename("UNIT"="UNIT.x")
PLANTECOL <- PLANTECOL %>%
  mutate(STATE = ifelse(is.na(correct_STATE), STATE, correct_STATE)) %>%
  select(-correct_STATE)
# PLANTECOL <- PLANTECOL %>%
#   select(-COUNTRY.y) %>%
#   rename("COUNTRY"="COUNTRY.x") %>% 
#   mutate(COUNTRY = ifelse(is.na(correct_COUNTRY), COUNTRY, correct_COUNTRY)) %>%
#   select(-correct_COUNTRY)
PLANTECOL <- PLANTECOL %>%
  mutate(editor_id.x = ifelse((is.na(editor_id.x)|editor_id.x=="missing"), editor_id.y, editor_id.x)) %>%
  select(-editor_id.y) %>%
  rename("editor_id"="editor_id.x")
# 
PLANTECOL$JOURNAL<-"PLANTECOL"
# 
# PLANTECOL$LAST_NAME[PLANTECOL$LAST_NAME=="Lookinbill"]<-"Lookingbill"
# PLANTECOL$CITY[PLANTECOL$LAST_NAME=="Overton"]<-NA
# PLANTECOL$geo.code[PLANTECOL$LAST_NAME=="Betts"]<-"CAN"

PLANTECOL<-PLANTECOL %>% group_by(LAST_NAME,FIRST_NAME) %>% 
  fill(INST,CITY,.direction="down")
# 
# PLANTECOL$editor_id<-as.factor(PLANTECOL$editor_id)
# # 
# # Rebind the ORIGINAL DATA AND NOW CORRECTED PLANTECOL
# 
# str(ORIGINAL_DATA)
# str(PLANTECOL)
# PLANTECOL$editor_id<-as.factor(PLANTECOL$editor_id)
# str(PLANTECOL_inst)
# ORIGINAL_DATA<-bind_rows(ORIGINAL_DATA,PLANTECOL)
# colnames(ORIGINAL_DATA)

# rm(PLANTECOL,PLANTECOL_inst)
PLANTECOL$editor_id<-as.character(PLANTECOL$editor_id)
ORIGINAL_DATA$editor_id<-as.character(ORIGINAL_DATA$editor_id)
return_list <- list(ORIGINAL_DATA,PLANTECOL)
return(return_list)
}
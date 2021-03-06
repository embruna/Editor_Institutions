PJ_OIKOS_corrections <- function(ORIGINAL_DATA) {
  # ORIGINAL_DATA<-ALLDATA  
  library(tidyverse)

  
OIKOS_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_OIKOS.csv", col_names = TRUE)
  
names(OIKOS_inst)

OIKOS_inst<-OIKOS_inst %>%  select(-X1) 
OIKOS_inst<-OIKOS_inst %>% select(JOURNAL, YEAR,editor_id, FIRST_NAME,
                                MIDDLE_NAME, LAST_NAME, INST,CITY,NOTES)

# ORIGINAL_DATA<-ALLDATA
OIKOS<-filter(ORIGINAL_DATA,JOURNAL=="OIKOS")

colnames(OIKOS)
colnames(ORIGINAL_DATA)


# remove OECOL FROM THE COMPLETE DATASET, WILL REBIND AFTER ADDING CORRECTIONS
ORIGINAL_DATA<-ORIGINAL_DATA %>% filter(JOURNAL!="OIKOS")

# INSERT THE CORRECTIONS TO OECOL AND FILL
OIKOS<-OIKOS %>% na_if("missing")
OIKOS_inst<-OIKOS_inst %>% na_if("missing")


OIKOS$INST<-as.character(OIKOS$INST)

OIKOS_inst$editor_id<-as.character(OIKOS_inst$editor_id)
OIKOS$editor_id<-as.character(OIKOS$editor_id)
# 
OIKOS<-full_join(OIKOS, OIKOS_inst, by = c("LAST_NAME","FIRST_NAME","YEAR"),all = T)

OIKOS <- OIKOS %>%
  mutate(CITY.x = ifelse((is.na(CITY.x)|CITY.x=="missing"), CITY.y, CITY.x)) %>%
  select(-CITY.y) %>%
  rename("CITY"="CITY.x")
OIKOS <- OIKOS %>%
  mutate(INST.x = ifelse((is.na(INST.x)|INST.x=="missing"), INST.y, INST.x)) %>%
  select(-INST.y) %>%
  rename("INST"="INST.x")
OIKOS <- OIKOS %>%
  select(-JOURNAL.y) %>%
  rename("JOURNAL"="JOURNAL.x")
OIKOS <- OIKOS %>%
  mutate(MIDDLE_NAME.x = ifelse((is.na(MIDDLE_NAME.x)|MIDDLE_NAME.x=="missing"), MIDDLE_NAME.y, MIDDLE_NAME.x)) %>%
  select(-MIDDLE_NAME.y) %>%
  rename("MIDDLE_NAME"="MIDDLE_NAME.x")
OIKOS <- OIKOS %>%
  mutate(NOTES.x = ifelse((is.na(NOTES.x)|NOTES.x=="missing"), NOTES.y, NOTES.x)) %>%
  select(-NOTES.y) %>%
  rename("NOTES"="NOTES.x")
# OIKOS <- OIKOS %>%
#   mutate(UNIT.x = ifelse((is.na(UNIT.x)|UNIT.x=="missing"), UNIT.y, UNIT.x)) %>%
#   select(-UNIT.y) %>%
#   rename("UNIT"="UNIT.x")
# OIKOS <- OIKOS %>%
#   mutate(STATE.x = ifelse((is.na(STATE.x)|STATE.x=="missing"), STATE.y, STATE.x)) %>%
#   select(-STATE.y) %>%
#   rename("STATE"="STATE.x")
# OIKOS <- OIKOS %>%
#   select(-COUNTRY.y) %>%
#   rename("COUNTRY"="COUNTRY.x")
OIKOS <- OIKOS %>%
  mutate(editor_id.x = ifelse((is.na(editor_id.x)|editor_id.x=="missing"), editor_id.y, editor_id.x)) %>%
  select(-editor_id.y) %>%
  rename("editor_id"="editor_id.x")
# 
OIKOS$JOURNAL<-"OIKOS"
# 
# OIKOS$LAST_NAME[OIKOS$LAST_NAME=="Lookinbill"]<-"Lookingbill"
# OIKOS$CITY[OIKOS$LAST_NAME=="Overton"]<-NA
# OIKOS$geo.code[OIKOS$LAST_NAME=="Betts"]<-"CAN"

OIKOS<-OIKOS %>% group_by(LAST_NAME,FIRST_NAME) %>% 
  fill(INST,CITY,.direction="down")
# 
# OIKOS$editor_id<-as.factor(OIKOS$editor_id)
# # 
# # Rebind the ORIGINAL DATA AND NOW CORRECTED OIKOS
# 
# str(ORIGINAL_DATA)
# str(OIKOS)
# OIKOS$editor_id<-as.factor(OIKOS$editor_id)
# str(OIKOS_inst)
# ORIGINAL_DATA<-bind_rows(ORIGINAL_DATA,OIKOS)
# colnames(ORIGINAL_DATA)

# rm(OIKOS,OIKOS_inst)
OIKOS$editor_id<-as.character(OIKOS$editor_id)
ORIGINAL_DATA$editor_id<-as.character(ORIGINAL_DATA$editor_id)
return_list <- list(ORIGINAL_DATA,OIKOS)
return(return_list)
}
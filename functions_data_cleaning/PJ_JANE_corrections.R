PJ_JANE_corrections <- function(ORIGINAL_DATA) {
  # ORIGINAL_DATA<-ALLDATA  
  library(tidyverse)
  
JANE_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_JANE.csv", col_names = TRUE)
names(JANE_inst)
JANE_inst<-JANE_inst %>%  select(-X1)
JANE_inst<-JANE_inst %>% select(JOURNAL, YEAR,editor_id, FIRST_NAME,
                                MIDDLE_NAME, LAST_NAME, INST,CITY,NOTES)

JANE_inst<-JANE_inst %>% filter_all(any_vars(!is.na(.)))

# ORIGINAL_DATA<-ALLDATA
JANE<-filter(ORIGINAL_DATA,JOURNAL=="JANE")

colnames(JANE)
colnames(ORIGINAL_DATA)


# remove JANE FROM THE COMPLETE DATASET, WILL REBIND AFTER ADDING CORRECTIONS
ORIGINAL_DATA<-ORIGINAL_DATA %>% filter(JOURNAL!="JANE")

# INSERT THE CORRECTIONS TO JANE AND FILL
JANE<-JANE %>% na_if("missing")
JANE_inst<-JANE_inst %>% na_if("missing")


JANE$INST<-as.character(JANE$INST)

JANE_inst$editor_id<-as.factor(JANE_inst$editor_id)
JANE$editor_id<-as.factor(JANE$editor_id)
# 
JANE<-full_join(JANE, JANE_inst, by = c("LAST_NAME","FIRST_NAME","YEAR"),all = T)

JANE <- JANE %>%
  mutate(CITY.x = ifelse((is.na(CITY.x)|CITY.x=="missing"), CITY.y, CITY.x)) %>%
  select(-CITY.y) %>%
  rename("CITY"="CITY.x")
JANE <- JANE %>%
  mutate(INST.x = ifelse((is.na(INST.x)|INST.x=="missing"), INST.y, INST.x)) %>%
  select(-INST.y) %>%
  rename("INST"="INST.x")
JANE <- JANE %>%
  select(-JOURNAL.y) %>%
  rename("JOURNAL"="JOURNAL.x")
JANE <- JANE %>%
  mutate(MIDDLE_NAME.x = ifelse((is.na(MIDDLE_NAME.x)|MIDDLE_NAME.x=="missing"), MIDDLE_NAME.y, MIDDLE_NAME.x)) %>%
  select(-MIDDLE_NAME.y) %>%
  rename("MIDDLE_NAME"="MIDDLE_NAME.x")
JANE <- JANE %>%
  mutate(NOTES.x = ifelse((is.na(NOTES.x)|NOTES.x=="missing"), NOTES.y, NOTES.x)) %>%
  select(-NOTES.y) %>%
  rename("NOTES"="NOTES.x")
# JANE <- JANE %>%
#   mutate(UNIT.x = ifelse((is.na(UNIT.x)|UNIT.x=="missing"), UNIT.y, UNIT.x)) %>%
#   select(-UNIT.y) %>%
#   rename("UNIT"="UNIT.x")
# JANE <- JANE %>%
#   mutate(STATE.x = ifelse((is.na(STATE.x)|STATE.x=="missing"), STATE.y, STATE.x)) %>%
#   select(-STATE.y) %>%
#   rename("STATE"="STATE.x")
# JANE <- JANE %>%
#   select(-COUNTRY.y) %>%
#   rename("COUNTRY"="COUNTRY.x")
JANE <- JANE %>%
  mutate(editor_id.x = ifelse((is.na(editor_id.x)|editor_id.x=="missing"), editor_id.y, editor_id.x)) %>%
  select(-editor_id.y) %>%
  rename("editor_id"="editor_id.x")
# 
JANE$JOURNAL<-"JANE"

# 
# JANE$LAST_NAME[JANE$LAST_NAME=="Lookinbill"]<-"Lookingbill"
# JANE$CITY[JANE$LAST_NAME=="Overton"]<-NA
# JANE$geo.code[JANE$LAST_NAME=="Betts"]<-"CAN"
colnames(JANE)
JANE<-JANE %>% group_by(LAST_NAME,FIRST_NAME) %>% 
  fill(INST,CITY,.direction="down")
# 
# JANE$editor_id<-as.factor(JANE$editor_id)
# # 
# # Rebind the ORIGINAL DATA AND NOW CORRECTED JANE
# 
# str(ORIGINAL_DATA)
# str(JANE)
# JANE$editor_id<-as.factor(JANE$editor_id)
# str(JANE_inst)
# ORIGINAL_DATA<-bind_rows(ORIGINAL_DATA,JANE)
# colnames(ORIGINAL_DATA)

# rm(JANE,JANE_inst)
JANE$editor_id<-as.character(JANE$editor_id)
ORIGINAL_DATA$editor_id<-as.character(ORIGINAL_DATA$editor_id)
return_list <- list(ORIGINAL_DATA,JANE)
return(return_list)
}
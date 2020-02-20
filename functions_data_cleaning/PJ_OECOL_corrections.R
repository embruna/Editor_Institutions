# This will load and the corrected files and make the required changes.
PJ_OECOL_corrections <- function(ORIGINAL_DATA) {
  # ORIGINAL_DATA<-ALLDATA  
  library(tidyverse)
  
OECOL_inst<-read.csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_OECOL.csv", encoding="ascii")
names(OECOL_inst)
OECOL_inst<-OECOL_inst %>%  select(-X,-editor_id.y) %>%
  rename("editor_id"="editor_id.x")
names(OECOL_inst)
OECOL_inst<-OECOL_inst %>% select(JOURNAL, YEAR,editor_id, FIRST_NAME,
                                  MIDDLE_NAME, LAST_NAME, INST,CITY,NOTES)


# NEW df with OECOLOGIA
OECOLOGIA<-filter(ORIGINAL_DATA,JOURNAL=="OECOL")

# remove OECOL FROM THE COMPLETE DATASET, WILL REBIND AFTER ADDING CORRECTIONS
ORIGINAL_DATA<-ORIGINAL_DATA %>% filter(JOURNAL!="OECOL")

# INSERT THE CORRECTIONS TO OECOL AND FILL
OECOLOGIA<-OECOLOGIA %>% na_if("missing")
OECOL_inst<-OECOL_inst %>% na_if("missing")
# colnames(OECOLOGIA)
# colnames(OECOL_inst)
# str(OECOLOGIA)
# str(OECOL_inst)
OECOL_inst$editor_id<-as.factor(OECOL_inst$editor_id)
OECOL_inst$INST<-as.character(OECOL_inst$INST)
OECOLOGIA$INST<-as.character(OECOLOGIA$INST)

OECOL_inst$MIDDLE_NAME<-as.character(OECOL_inst$MIDDLE_NAME)
OECOLOGIA$MIDDLE_NAME<-as.character(OECOLOGIA$MIDDLE_NAME)

OECOL_inst$LAST_NAME<-as.character(OECOL_inst$LAST_NAME)
OECOLOGIA$LAST_NAME<-as.character(OECOLOGIA$LAST_NAME)

OECOL_inst$FIRST_NAME<-as.character(OECOL_inst$FIRST_NAME)
OECOLOGIA$FIRST_NAME<-as.character(OECOLOGIA$FIRST_NAME)

OECOL_inst$CITY<-as.character(OECOL_inst$CITY)
OECOLOGIA$CITY<-as.character(OECOLOGIA$CITY)

OECOL_inst$NOTES<-as.character(OECOL_inst$NOTES)
OECOLOGIA$NOTES<-as.character(OECOLOGIA$NOTES)

OECOLOGIA<-full_join(OECOLOGIA, OECOL_inst, by = c("LAST_NAME","FIRST_NAME","YEAR"),all = T) %>%
  group_by(LAST_NAME,FIRST_NAME) %>%
  mutate(CITY.x = ifelse((is.na(CITY.x)|CITY.x=="missing"), CITY.y, CITY.x)) %>%
  select(-CITY.y) %>%
  rename("CITY"="CITY.x") %>%
  mutate(INST.x = ifelse((is.na(INST.x)|INST.x=="missing"), INST.y, INST.x)) %>%
  select(-INST.y) %>%
  rename("INST"="INST.x") %>%
  select(-JOURNAL.y) %>%
  rename("JOURNAL"="JOURNAL.x") %>%
  mutate(MIDDLE_NAME.x = ifelse((is.na(MIDDLE_NAME.x)|MIDDLE_NAME.x=="missing"), MIDDLE_NAME.y, MIDDLE_NAME.x)) %>%
  select(-MIDDLE_NAME.y) %>%
  rename("MIDDLE_NAME"="MIDDLE_NAME.x") %>%
  mutate(NOTES.x = ifelse((is.na(NOTES.x)|NOTES.x=="missing"), NOTES.y, NOTES.x)) %>%
  select(-NOTES.y) %>%
  rename("NOTES"="NOTES.x") %>%
  mutate(editor_id.x = ifelse((is.na(editor_id.x)|editor_id.x=="missing"), editor_id.y, editor_id.x)) %>%
  select(-editor_id.y) %>%
  rename("editor_id"="editor_id.x")

OECOLOGIA$JOURNAL<-"OECOL"

OECOLOGIA<-foo<-OECOLOGIA %>% group_by(LAST_NAME,FIRST_NAME,COUNTRY) %>%
  fill(INST,CITY,.direction="down")

OECOLOGIA$editor_id<-as.factor(OECOLOGIA$editor_id)

# Rebind the ORIGINAL DATA AND NOW CORRECTED OECOLOGIA
str(ORIGINAL_DATA)
str(OECOLOGIA)

# ORIGINAL_DATA<-bind_rows(ORIGINAL_DATA,OECOLOGIA)
# rm(OECOLOGIA,OECOL_inst)
OECOLOGIA$editor_id<-as.character(OECOLOGIA$editor_id)
ORIGINAL_DATA$editor_id<-as.character(ORIGINAL_DATA$editor_id)
return_list <- list(ORIGINAL_DATA,OECOLOGIA)
return(return_list)
}
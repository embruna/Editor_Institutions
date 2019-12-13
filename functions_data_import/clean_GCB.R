#FUNCTION TO CLEAN AND PROCESS GCB
clean_GCB <- function(DATAFILE) {
  library(tidyverse)
  DATAFILE<- DATAFILE %>% extract(NAME, c("FIRST_NAME","LAST_NAME"), "([^ ]+) (.*)")
  DATAFILE<-DATAFILE %>% separate(LAST_NAME, c("MIDDLE_NAME", "LAST_NAME"),sep = " ", extra="merge", fill = "left",remove=FALSE)
  DATAFILE$editor_id<-NA
  # head(DATAFILE,10)
  DATAFILE<-DATAFILE %>% select(JOURNAL,YEAR,VOLUME,ISSUE,TITLE,editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,COUNTRY,GENDER,CATEGORY,NOTES)  
  
  DATAFILE$INST<-as.character(DATAFILE$INST)
  DATAFILE$STATE<-NA
  DATAFILE$STATE<-as.character(DATAFILE$STATE)
  DATAFILE$UNIT<-NA
  DATAFILE$UNIT<-as.character(DATAFILE$UNIT)
  DATAFILE$CITY<-NA
  DATAFILE$CITY<-as.character(DATAFILE$CITY)
  
  # This will add "missing" to the first row of a group if the first INST is NA
  DATAFILE$INST<-trimws(DATAFILE$INST)
  DATAFILE$UNIT<-trimws(DATAFILE$UNIT)
  DATAFILE$CITY<-trimws(DATAFILE$CITY)
  DATAFILE$STATE<-trimws(DATAFILE$STATE)
  
  DATAFILE<-DATAFILE %>% 
    group_by(LAST_NAME,FIRST_NAME) %>% 
    mutate(INST = ifelse((row_number()==1 & is.na(INST)), "missing", INST))
  
  
  DATAFILE<-DATAFILE %>% 
    group_by(LAST_NAME,FIRST_NAME) %>% 
    mutate(UNIT = ifelse((row_number()==1 & is.na(UNIT)), "missing", UNIT))
  
  DATAFILE<-DATAFILE %>% 
    group_by(LAST_NAME,FIRST_NAME) %>% 
    mutate(STATE = ifelse((row_number()==1 & is.na(STATE)), "missing", STATE))
  
  
  DATAFILE<-DATAFILE %>% 
    group_by(LAST_NAME,FIRST_NAME) %>% 
    mutate(CITY = ifelse((row_number()==1 & is.na(CITY)), "missing", CITY))
  
  
  
  DATAFILE<-DATAFILE %>% arrange(LAST_NAME,FIRST_NAME,YEAR) 
  DATAFILE<-DATAFILE %>% fill(INST,UNIT,STATE,CITY,.direction="down")
  # 
  DATAFILE<-DATAFILE %>% 
    group_by(LAST_NAME,FIRST_NAME) %>% 
    mutate(INST = ifelse((row_number()>1 & INST=="missing"),NA, INST))
  
  
  
  DATAFILE<-DATAFILE %>% 
    group_by(LAST_NAME,FIRST_NAME) %>% 
    mutate(UNIT = ifelse((row_number()>1 & UNIT=="missing"),NA, UNIT))
  
  
  
  DATAFILE<-DATAFILE %>% 
    group_by(LAST_NAME,FIRST_NAME) %>% 
    mutate(CITY = ifelse((row_number()>1 & CITY=="missing"),NA, CITY))
  
  
  
  DATAFILE<-DATAFILE %>% 
    group_by(LAST_NAME,FIRST_NAME) %>% 
    mutate(STATE = ifelse((row_number()>1 & STATE=="missing"),NA, STATE))
  
  
  

  return(DATAFILE)
}



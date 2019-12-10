#FUNCTION TO CLEAN AND PROCESS JAPE
clean_JAPE <- function(DATAFILE) {
  # DATAFILE<-JAPE_raw
  library(tidyverse)
  head(DATAFILE,10)
  DATAFILE<-DATAFILE %>% rename("TITLE"="TITLE.x")
  head(DATAFILE,10)
  
  # str(DATAFILE)
  DATAFILE$INST<-as.character(DATAFILE$INST)
  DATAFILE<-DATAFILE %>% 
    group_by(LAST_NAME,FIRST_NAME) %>% 
    mutate(INST = ifelse((row_number()==1 & is.na(INST)), "missing", INST))
  
  DATAFILE$UNIT<-as.character(DATAFILE$UNIT)
  DATAFILE<-DATAFILE %>% 
    group_by(LAST_NAME,FIRST_NAME) %>% 
    mutate(UNIT = ifelse((row_number()==1 & is.na(UNIT)), "missing", UNIT))
  
  DATAFILE$STATE<-as.character(DATAFILE$STATE)
  DATAFILE<-DATAFILE %>% 
    group_by(LAST_NAME,FIRST_NAME) %>% 
    mutate(STATE = ifelse((row_number()==1 & is.na(STATE)), "missing", STATE))
  
  DATAFILE$CITY<-as.character(DATAFILE$CITY)
  DATAFILE<-DATAFILE %>% 
    group_by(LAST_NAME,FIRST_NAME) %>% 
    mutate(CITY = ifelse((row_number()==1 & is.na(CITY)), "missing", CITY))
  
  DATAFILE$INST<-trimws(DATAFILE$INST)
  DATAFILE$UNIT<-trimws(DATAFILE$UNIT)
  DATAFILE$CITY<-trimws(DATAFILE$CITY)
  DATAFILE$STATE<-trimws(DATAFILE$STATE)
  # 
  # DATAFILE$INST<-as.character(DATAFILE$INST)
  # DATAFILE<-DATAFILE %>% 
  #   group_by(LAST_NAME,FIRST_NAME) %>% 
  #   mutate(INST = ifelse((row_number()==1 & INST=="missing"), "missing", (fill(INST,.direction="down"))))
  # 
  
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
  
  
  ##
  # code to generate the list of which ones need to be 2x or have missing names is in 
  # Author_and_Inst_to_Check.R  
  # Need to convert that to a function, e.g., check(AMNAT)
  # Then need to add a function to upload the corrections/additions
  ##
  return(DATAFILE)
}



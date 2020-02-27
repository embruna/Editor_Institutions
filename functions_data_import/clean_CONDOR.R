#FUNCTION TO CLEAN AND PROCESS CONDOR
clean_CONDOR <- function(DATAFILE) {
  
  DATAFILE$JOURNAL<-"CONDOR"
  DATAFILE$editor_id<-NA
  DATAFILE<-DATAFILE %>% select(JOURNAL,YEAR, editor_id,EDITOR_TITLE,
                                    FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,
                                    UNIT,CITY,STATE,COUNTRY,NOTES)
  DATAFILE<-DATAFILE %>% rename("TITLE"="EDITOR_TITLE") 
  DATAFILE$INST<-gsub("None given",NA, DATAFILE$INST)
  DATAFILE$UNIT<-gsub("None given",NA, DATAFILE$UNIT)
  DATAFILE$CITY<-gsub("None given",NA, DATAFILE$CITY)
  DATAFILE$STATE<-gsub("None given",NA, DATAFILE$STATE)
  DATAFILE$COUNTRY<-gsub("None given",NA, DATAFILE$COUNTRY)
  
  
  
  # This will add "missing" to the first row of a group if the first INST is NA
  
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
  
  DATAFILE$INST<-trimws(DATAFILE$INST)
  DATAFILE$UNIT<-trimws(DATAFILE$UNIT)
  DATAFILE$CITY<-trimws(DATAFILE$CITY)
  DATAFILE$STATE<-trimws(DATAFILE$STATE)
  
  
  
  
  # DATAFILE<-DATAFILE %>% fill(INST,UNIT,CITY,STATE,.direction="down")
  
  
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



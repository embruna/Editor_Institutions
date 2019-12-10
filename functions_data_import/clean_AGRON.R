#FUNCTION TO CLEAN AND PROCESS AGRON
clean_AGRON <- function(DATAFILE) {
  
  # AGRONOMY MISSING INST
  # DATAFILE$INST<-as.factor(DATAFILE$INST)
  
  DATAFILE$INST<-as.character(DATAFILE$INST)
  DATAFILE$STATE<-as.character(DATAFILE$STATE)
  DATAFILE$COUNTRY<-as.character(DATAFILE$COUNTRY)
  DATAFILE$UNIT<-as.character(DATAFILE$UNIT)
  DATAFILE$CITY<-as.character(DATAFILE$CITY)
  DATAFILE$INST<-trimws(DATAFILE$INST)
  DATAFILE$UNIT<-trimws(DATAFILE$UNIT)
  DATAFILE$CITY<-trimws(DATAFILE$CITY)
  DATAFILE$STATE<-trimws(DATAFILE$STATE)
  
  
  DATAFILE$INST[DATAFILE$LAST_NAME=="Benbi" & DATAFILE$FIRST_NAME=="Dinesh"]<-"Punjab Agricultural University"
  DATAFILE$STATE[DATAFILE$LAST_NAME=="Benbi" & DATAFILE$FIRST_NAME=="Dinesh"]<-"Punjab"
  DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="Benbi" & DATAFILE$FIRST_NAME=="Dinesh"]<-"India"
  DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="Pedreira" & DATAFILE$FIRST_NAME=="Carlos"]<-"Brazil"
  # DATAFILE<-DATAFILE %>% rename("TITLE"="TITLE.x")

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



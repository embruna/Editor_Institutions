#FUNCTION TO CLEAN AND PROCESS FEM
clean_FEM <- function(DATAFILE) {
  levels(DATAFILE$INST)<-c(levels(DATAFILE$INST),"University of Washington Seattle")
  DATAFILE$INST[DATAFILE$FIRST_NAME=="Douglas" & DATAFILE$LAST_NAME=="Maguire" & (DATAFILE$YEAR==1993 | DATAFILE$YEAR==1994)]<-"University of Washington Seattle"
  levels(DATAFILE$UNIT)<-c(DATAFILE$UNIT,"College of Forest Resources")
  DATAFILE$UNIT[DATAFILE$FIRST_NAME=="Douglas" & DATAFILE$LAST_NAME=="Maguire" & (DATAFILE$YEAR==1993 | DATAFILE$YEAR==1994)]<-"College of Forest Resources"
  DATAFILE$NOTES[DATAFILE$FIRST_NAME=="Douglas" & DATAFILE$LAST_NAME=="Maguire" & (DATAFILE$YEAR==1993 | DATAFILE$YEAR==1994)]<-"jrl lists 'seattle wa' as address, CV says that he left UW in 92 and in 93/94 he was at U Maine"
  
  DATAFILE<-DATAFILE %>% rename("TITLE"="TITLE.x")
  
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



#FUNCTION TO CLEAN AND PROCESS PLANTECOL
clean_PLANTECOL <- function(DATAFILE) {
  
  # DATAFILE<-PLANTECOL_raw
  
  # some editing
  DATAFILE$INST<-as.character(DATAFILE$INST)
  DATAFILE$INST[DATAFILE$LAST_NAME=="Veblen"]<-"University of Colorado-Boulder"
  DATAFILE$INST[DATAFILE$LAST_NAME=="Picket"]<-"New York Botanical Garden"
  DATAFILE$INST[DATAFILE$LAST_NAME=="Peet"]<-"University of North Carolina-Chapel Hill"
  DATAFILE$INST[DATAFILE$LAST_NAME=="Damman"]<-"University of Connecticut"
  DATAFILE$INST[DATAFILE$LAST_NAME=="Pickett" & DATAFILE$FIRST_NAME=="Steward"]<-"New York Botanical Garden"
  
  DATAFILE<-DATAFILE %>% 
    mutate(UNIT=case_when(
      UNIT == "Institut f\x99r Biologie (II)"~"Institut for Biologie (II)",
      .default = as.character(INST)
    )
    )
  
  
  
  DATAFILE$INST<-trimws(DATAFILE$INST)
  DATAFILE$UNIT<-trimws(DATAFILE$UNIT)
  DATAFILE$CITY <-trimws(DATAFILE$CITY)
  DATAFILE$STATE<-trimws(DATAFILE$STATE)
  DATAFILE$COUNTRY<-trimws(DATAFILE$COUNTRY)
  DATAFILE$INST[DATAFILE$INST==""]<-NA
  DATAFILE$UNIT[DATAFILE$UNIT==""]<-NA
  DATAFILE$CITY[DATAFILE$CITY==""]<-NA
  DATAFILE$STATE[DATAFILE$STATE==""]<-NA
  DATAFILE$COUNTRY[DATAFILE$COUNTRY==""]<-NA
  DATAFILE<-DATAFILE %>% arrange(editor_id,YEAR,INST)
  
  # fill in the institutions in subsequent years (only 1st year recorded) and then look for any thiat might need
  # to be double checked
  # DATAFILE_fixes<-DATAFILE %>% group_by(editor_id,INST) %>% distinct(editor_id,INST) %>% distinct(editor_id)
  head(DATAFILE,10)
  # DATAFILE<-DATAFILE %>% fill(INST,.direction="down") %>% rename("TITLE"="TITLE.x")
  DATAFILE<-DATAFILE %>% rename("TITLE"="TITLE.x")
  head(DATAFILE,10)
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
  
  
  DATAFILE<-DATAFILE %>%
    mutate(across(everything(), as.character))
  
  
  
  return(DATAFILE)
}



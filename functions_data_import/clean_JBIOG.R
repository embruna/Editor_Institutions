#FUNCTION TO CLEAN AND PROCESS JBIOG
clean_JBIOG <- function(DATAFILE) {
  DATAFILE$INST<-as.character(DATAFILE$INST)
  DATAFILE<-DATAFILE %>% separate(INST, c("UNIT", "INST"),",",extra="merge",fill="left",remove=TRUE)
  DATAFILE$INST[DATAFILE$UNIT=="Uc Merced"]<-"University of California-Merced"
  DATAFILE$UNIT[DATAFILE$UNIT=="Uc Merced"]<-NA
  DATAFILE$INST[DATAFILE$INST==" School Of Life Sciences, Arizona State University"]<-"Arizona State University"
  DATAFILE$UNIT[DATAFILE$UNIT=="Institute For Species Exploration"]<-"Institute For Species Exploration,School of Life Sciences"
  DATAFILE$INST[DATAFILE$UNIT=="Evolution And Marine Biology, University of California"]<-"University of California-Santa Barbara"
  DATAFILE$UNIT[DATAFILE$FIRST_NAME=="Dov"]<-"Department of Ecology, Evolution, and Marine Biology"
  
  DATAFILE$INST<-as.character(DATAFILE$INST)
  DATAFILE$STATE<-as.character(DATAFILE$STATE)
  DATAFILE$COUNTRY<-as.character(DATAFILE$COUNTRY)
  DATAFILE$UNIT<-as.character(DATAFILE$UNIT)
  DATAFILE$CITY<-as.character(DATAFILE$CITY)
  
  # spot checks of DATAFILE
  DATAFILE$INST<-trimws(DATAFILE$INST)
  DATAFILE$UNIT<-trimws(DATAFILE$UNIT)
  DATAFILE$CITY <-trimws(DATAFILE$CITY)
  DATAFILE$STATE<-trimws(DATAFILE$STATE)
  DATAFILE$COUNTRY<-trimws(DATAFILE$COUNTRY)
  # DATAFILE$INST[DATAFILE$INST==""]<-NA
  # DATAFILE$UNIT[DATAFILE$UNIT==""]<-NA
  # DATAFILE$CITY[DATAFILE$CITY==""]<-NA
  # DATAFILE$STATE[DATAFILE$STATE==""]<-NA
  # DATAFILE$COUNTRY[DATAFILE$COUNTRY==""]<-NA
  # DATAFILE<-DATAFILE %>% arrange(editor_id,YEAR,INST)
  
  # fill in the institutions in subsequent years (only 1st year recorded) and then look for any thiat might need
  
  # # NEED TO ADD "missing" to 1st line of group by edito where NA
  # DATAFILE_1row<-DATAFILE %>% group_by(editor_id) %>% 
  #   arrange(editor_id,YEAR) %>% 
  #   filter(row_number()==1)
  # levels(DATAFILE_1row$INST)<-c(levels(DATAFILE_1row$INST),"missing")
  # DATAFILE_1row$INST<-replace(DATAFILE_1row$INST, is.na(DATAFILE_1row$INST), "missing")
  # 
  # DATAFILE_remainder<-DATAFILE %>% group_by(editor_id) %>% 
  #   arrange(editor_id,YEAR) %>% 
  #   filter(row_number()>1)
  # 
  # DATAFILE<-bind_rows(DATAFILE_remainder,DATAFILE_1row)
  # head(DATAFILE,70)
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
  # 
  # 
  # DATAFILE<-DATAFILE %>% arrange(LAST_NAME,FIRST_NAME,YEAR) 
  # DATAFILE<-DATAFILE %>% fill(INST,UNIT,STATE,CITY,.direction="down")
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
  
  
  
  
  
  return(DATAFILE)
}



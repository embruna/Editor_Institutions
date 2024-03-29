#FUNCTION TO CLEAN AND PROCESS MARECOL
# DATAFILE<-MARECOL_raw
clean_MARECOL <- function(DATAFILE) {
  # DATAFILE<-MARECOL_21July2018.csv
  DATAFILE<-rename(DATAFILE,"VOLUME"="Volume","ISSUE"="Issue")
  # str(DATAFILE)
  DATAFILE$no<-NULL
  DATAFILE$editor_id<-NA
  # DATAFILE<- DATAFILE %>% extract(NAME, c("FIRST_NAME","LAST_NAME"), "([^ ]+) (.*)")
  
  DATAFILE$NAME<-as.character(DATAFILE$NAME)
  DATAFILE<- DATAFILE %>% separate(NAME, c("FIRST_NAME","LAST_NAME"),sep = " ", extra="merge", fill = "left",remove=FALSE)
  DATAFILE<-DATAFILE %>% separate(LAST_NAME, c("MIDDLE_NAME", "LAST_NAME"),sep = " ", extra="merge", fill = "left",remove=FALSE)
  DATAFILE<-DATAFILE %>% separate(LAST_NAME, c("MIDDLE_NAME", "LAST_NAME"),sep = ". ", extra="merge",fill = "left",remove=FALSE)
  DATAFILE$FIRST_NAME<-gsub("[.]","",DATAFILE$FIRST_NAME)
  
  # head(DATAFILE,10)
  DATAFILE<-DATAFILE %>% rename("TITLE"="Title") %>% select(JOURNAL,YEAR,VOLUME,ISSUE,TITLE,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY,editor_id)
  # head(DATAFILE,10)
  # This will add "missing" to the first row of a group if the first INST is NA
  
  
  DATAFILE$INST<-as.character(DATAFILE$INST)
  DATAFILE$UNIT<-as.character(DATAFILE$UNIT)
  DATAFILE$CITY<-as.character(DATAFILE$CITY)
  DATAFILE$STATE<-as.character(DATAFILE$STATE)
  DATAFILE$COUNTRY<-as.character(DATAFILE$COUNTRY)
  
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
  
  DATAFILE<-DATAFILE %>% 
    group_by(LAST_NAME,FIRST_NAME) %>% 
    mutate(COUNTRY = ifelse((row_number()==1 & is.na(COUNTRY)), "missing", COUNTRY))
  
  
  # DATAFILE<-DATAFILE %>% fill(INST,UNIT,CITY,STATE,.direction="down")
  
  
  DATAFILE<-DATAFILE %>% arrange(LAST_NAME,FIRST_NAME,YEAR) 
  DATAFILE<-DATAFILE %>% fill(INST,UNIT,STATE,CITY,COUNTRY,.direction="down")
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
    group_by(LAST_NAME,FIRST_NAME) %>% 
    mutate(COUNTRY = ifelse((row_number()>1 & COUNTRY=="missing"),NA, COUNTRY))
  
  DATAFILE<-DATAFILE %>%
    mutate(across(everything(), as.character))
  
  return(DATAFILE)
}



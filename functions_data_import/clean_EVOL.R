#FUNCTION TO CLEAN AND PROCESS EVOL

# DATAFILE<-EVOL_raw
clean_EVOL <- function(DATAFILE) {
  
  
  DATAFILE$INST<-as.character(DATAFILE$INST)
  DATAFILE$STATE<-as.character(DATAFILE$STATE)
  DATAFILE$UNIT<-as.character(DATAFILE$UNIT)
  DATAFILE$CITY<-as.character(DATAFILE$CITY)
  
  
  
  DATAFILE<-DATAFILE %>% 
    mutate(INST=case_when(
      INST == " Universidad Nacional Autonoma de M\x82xico"~"Universidad Nacional Autonoma de Mexico",
      INST == " Universidad Nacional Aut\xa2noma de M\x82xico"~"Universidad Nacional Autonoma de Mexico",
      INST == "Universidad Nacional Aut\xa2noma de M\x82xico"~"Universidad Nacional Autonoma de Mexico",
      INST == "Universidad Nacional Autonoma de M\x82xico"~"Universidad Nacional Autonoma de Mexico",
      INST == "Universit\x82 de Montpellier II"~"Universite de Montpellier II",
      INST == "Universit\x82 Montpellier II"~"Universite de Montpellier II",
      INST == "Universit\x82 de Lausanne"~"Universite de Lausanne",
      INST == "Ludwig-Maximilians-Universit\x84t M\x81nchen"~"Ludwig-Maximilians-Universitat Munchen",
      INST == "Ecole Normale Sup\x82rieure"~"Ecole Normale Superieure",
      INST == "Universit\x82 Paris-Sud"~"Universite Paris-Sud",
      INST == "Universit\x84t Z\x81rich-Irchel"~"Universitat Zurich-Irchel",
      .default = as.character(INST)
    )
    )
  
  DATAFILE<-DATAFILE %>% 
    mutate(CITY=case_when(
      CITY == "Z\x81rich"~"Zurich",
      .default = as.character(CITY)
    )
    )
  
  
  DATAFILE<-DATAFILE %>% 
    mutate(STATE=case_when(
      STATE == "Z\x81rich"~"Zurich",
      .default = as.character(STATE)
    )
    )
  
  
  
  
  DATAFILE$COUNTRY<-as.character(DATAFILE$COUNTRY)
  DATAFILE$STATE<-as.character(DATAFILE$STATE)
  DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="Knowlton" & DATAFILE$FIRST_NAME=="Nancy"]<-"Panama"
  DATAFILE$STATE[DATAFILE$LAST_NAME=="Knowlton" & DATAFILE$FIRST_NAME=="Nancy"]<-NA
  # DATAFILE<-DATAFILE %>% rename("TITLE"="TITLE.x")
  head(DATAFILE,10)
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



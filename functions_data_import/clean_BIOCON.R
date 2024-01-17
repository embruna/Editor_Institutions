#FUNCTION TO CLEAN AND PROCESS BIOCON
# DATAFILE<-BIOCON_raw
clean_BIOCON <- function(DATAFILE) {
    # DELETE THEIR RECORD FOR GIVEN YEAR (not on board)
  DATAFILE<-DATAFILE[!(DATAFILE$editor_id==2399 & DATAFILE$YEAR==1997),]
  DATAFILE<-DATAFILE[!(DATAFILE$editor_id==2830 & DATAFILE$YEAR==1995),]
  DATAFILE<-DATAFILE[!(DATAFILE$editor_id==2830 & DATAFILE$YEAR==1996),]
  DATAFILE<-DATAFILE[!(DATAFILE$editor_id==566 & DATAFILE$YEAR==1988),]
    # CORRECT RECORDS
  DATAFILE$INST[DATAFILE$LAST_NAME=="ATKINSON" & DATAFILE$editor_id==1388 & DATAFILE$YEAR==1997]<-"Ecological Research Associates of New Zealand"
  DATAFILE$INST[DATAFILE$LAST_NAME=="DIRZO" & DATAFILE$YEAR==1998]<-"Northern Arizona University"
  DATAFILE$CITY[DATAFILE$LAST_NAME=="DIRZO" & DATAFILE$YEAR==1998]<-"Flagstaff"
  DATAFILE$INST[DATAFILE$LAST_NAME=="Guyer" & DATAFILE$YEAR==1998]<-"Auburn University"
  DATAFILE$UNIT[DATAFILE$LAST_NAME=="Guyer" & DATAFILE$YEAR==1998]<-"Zoology"
  DATAFILE$STATE[DATAFILE$LAST_NAME=="Guyer" & DATAFILE$YEAR==1998]<-"Alabama"
  DATAFILE$CITY[DATAFILE$LAST_NAME=="Guyer" & DATAFILE$YEAR==1998]<-"Auburn"
  DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="Guyer" & DATAFILE$YEAR==1998]<-"USA"
  DATAFILE$INST[DATAFILE$editor_id==2399 & DATAFILE$YEAR==1998]<-DATAFILE$INST[DATAFILE$editor_id==2399 & DATAFILE$YEAR==1999]
  DATAFILE$UNIT[DATAFILE$editor_id==2399 & DATAFILE$YEAR==1998]<-NA
  DATAFILE$CITY[DATAFILE$editor_id==2399 & DATAFILE$YEAR==1998]<-"Penicuik"
  DATAFILE$STATE[DATAFILE$editor_id==2399 & DATAFILE$YEAR==1998]<-NA
  DATAFILE$COUNTRY[DATAFILE$editor_id==2399 & DATAFILE$YEAR==1998]<-"UK"
  DATAFILE$INST[DATAFILE$editor_id==3143 & DATAFILE$YEAR==1998]<-DATAFILE$INST[DATAFILE$editor_id==3143 & DATAFILE$YEAR==1999]
  DATAFILE$INST[DATAFILE$editor_id==3108 & DATAFILE$YEAR==1997]<-"University of Toronto"
  DATAFILE$CITY[DATAFILE$editor_id==3108 & DATAFILE$YEAR==1997]<-"Toronto"
  DATAFILE$STATE[DATAFILE$editor_id==3108 & DATAFILE$YEAR==1997]<-"Ontario"
  # levels(DATAFILE$INST)<-c(levels(DATAFILE$INST),"University of Capetown")
  DATAFILE$INST[DATAFILE$editor_id==2830 & DATAFILE$YEAR==1996]<-"University of Capetown"
  # levels(DATAFILE$CITY)<-c(levels(DATAFILE$CITY),"Capetown")
  DATAFILE$CITY[DATAFILE$editor_id==2830 & DATAFILE$YEAR==1996]<-"Capetown"
  DATAFILE$YEAR[DATAFILE$editor_id==3269 & DATAFILE$VOLUME==121]<-2006
  DATAFILE$INST[DATAFILE$editor_id==3269 & DATAFILE$YEAR==2005]<-DATAFILE$INST[DATAFILE$editor_id==3269 & DATAFILE$YEAR==2006]
  DATAFILE$UNIT[DATAFILE$editor_id==3269 & DATAFILE$YEAR==2005]<-DATAFILE$UNIT[DATAFILE$editor_id==3269 & DATAFILE$YEAR==2006]
  DATAFILE$STATE[DATAFILE$editor_id==3269 & DATAFILE$YEAR==2005]<-DATAFILE$STATE[DATAFILE$editor_id==3269 & DATAFILE$YEAR==2006]
  DATAFILE$CITY[DATAFILE$editor_id==3269 & DATAFILE$YEAR==2005]<-"Santa Barbara"
  DATAFILE$COUNTRY[DATAFILE$editor_id==3269 & DATAFILE$YEAR==2005]<-"USA"
  DATAFILE$NOTES[DATAFILE$editor_id==3269 & DATAFILE$YEAR==2005]<-NA
  DATAFILE$CATEGORY[DATAFILE$editor_id==3269 & DATAFILE$YEAR==2005]<-"AE"
  DATAFILE<-DATAFILE %>% 
    mutate(INST=case_when(
      INST == "Federal Research Center for Nature Conservation and Landscape\xa0Ecology"~"Federal Research Center for Nature Conservation and Landscape Ecology",
      .default = as.character(INST)
      )
      )
    
  DATAFILE$INST[DATAFILE$editor_id==3269 & DATAFILE$YEAR==2005]<-"Federal Research Center for Nature Conservation and Landscape\xa0Ecology"
  
  
  # ADD RECORDS (Not included for some years)
  Pressey_2005<-data.frame(editor_id=342, FIRST_NAME="Bob",LAST_NAME="Pressey",NAME="Bob Pressey", INST="New South Wales Evironment and Conservation",CITY="Kensington",STATE="NSW",COUNTRY="Australia", UNIT=NA) 
  Wright_1998<-data.frame(editor_id=3255, FIRST_NAME="R",MIDDLE_NAME="Gerald",LAST_NAME="Wright",NAME="R Gerald Wright", INST="University of Idaho",UNIT="Wildlife Resources",CITY="Moscow",STATE="Idaho",COUNTRY="USA") 
  Kirby_2005<-DATAFILE[DATAFILE$editor_id==2030 & DATAFILE$YEAR==2006,]
  Kirby_2005$YEAR<-2005
  Lind_2005<-DATAFILE[DATAFILE$editor_id==765 & DATAFILE$YEAR==2006,]
  Lind_2005$YEAR<-2005
  Lipps_2005<-DATAFILE[DATAFILE$editor_id==2043 & DATAFILE$YEAR==2006,]
  Lipps_2005$YEAR<-2005
  JPM_2006<-DATAFILE[DATAFILE$editor_id==1763 & DATAFILE$YEAR==2007,]
  JPM_2006$YEAR<-2006
  WFL_2005<-DATAFILE[DATAFILE$editor_id==3763 & DATAFILE$YEAR==2006,]
  WFL_2005$YEAR<-2005
  WALDREN_2005<-DATAFILE[DATAFILE$editor_id==3496 & DATAFILE$YEAR==2006,]
  WALDREN_2005$YEAR<-2005
  
  DATAFILE$editor_id<-as.character(DATAFILE$editor_id)
  
  DATAFILE<-DATAFILE %>% mutate_all(as.character)
  Pressey_2005<-Pressey_2005 %>% mutate_all(as.character)
  Wright_1998<-Wright_1998 %>% mutate_all(as.character)
  Kirby_2005<-Kirby_2005 %>% mutate_all(as.character)
  Lind_2005<-Lind_2005 %>% mutate_all(as.character)
  JPM_2006<-JPM_2006 %>% mutate_all(as.character)
  WFL_2005<-WFL_2005 %>% mutate_all(as.character)
  Lipps_2005<-Lipps_2005 %>% mutate_all(as.character)
  WALDREN_2005<-WALDREN_2005 %>% mutate_all(as.character)
  
  # 
  # Pressey_2005$editor_id<-as.character(Pressey_2005$editor_id)
  # Wright_1998$editor_id<-as.character(Wright_1998$editor_id)
  # Kirby_2005$editor_id<-as.character(Kirby_2005$editor_id)
  
  DATAFILE_ADDS<-bind_rows(Pressey_2005,Wright_1998,Kirby_2005,Lind_2005,JPM_2006,WFL_2005,Lipps_2005,WALDREN_2005)
  
  DATAFILE_ADDS<-DATAFILE_ADDS %>% mutate_all(as.character)
  DATAFILE<-DATAFILE %>% mutate_all(as.character)
  # DATAFILE_ADDS$YEAR<-as.character(DATAFILE_ADDS$YEAR)
  # DATAFILE$YEAR<-as.character(DATAFILE$YEAR)
  # DATAFILE$editor_id<-as.character(DATAFILE$editor_id)
  DATAFILE<-bind_rows(DATAFILE,DATAFILE_ADDS)
  rm(Pressey_2005,Wright_1998,Kirby_2005,Lind_2005,JPM_2006,WFL_2005,Lipps_2005,WALDREN_2005)
  
  head(DATAFILE,10)
  # DATAFILE<-rename(DATAFILE,"TITLE"="TITLE.x")
  # DATAFILE<-DATAFILE %>% fill(INST,UNIT,CITY,STATE,COUNTRY,.direction="down")
  
  # This will add "missing" to the first row of a group if the first INST is NA
  str(DATAFILE$LAST_NAME)
  DATAFILE$LAST_NAME<-as.character(DATAFILE$LAST_NAME)
  DATAFILE$FIRST_NAME<-as.character(DATAFILE$FIRST_NAME)
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
  
  
  DATAFILE<-DATAFILE %>% 
    mutate(INST=case_when(
      INST == "Federal Research Center for Nature Conservation and Landscape\xa0Ecology"~"Federal Research Center for Nature Conservation and Landscape Ecology",
      .default = as.character(INST)
    )
    )
  
  
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



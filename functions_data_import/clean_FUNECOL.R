#FUNCTION TO CLEAN AND PROCESS FUNECOL
clean_FUNECOL <- function(DATAFILE1,DATAFILE2) {
  # REVIEWING AND CORRECTING
  DATAFILE<-full_join(DATAFILE1,DATAFILE2,by=c("YEAR","FIRST_NAME","LAST_NAME"))
  DATAFILE$COUNTRY.x<-gsub("New Zeland","New Zealand", DATAFILE$COUNTRY.x)
  # colnames(FUNECOL)
  # summary(FUNECOL$ISSUE.x==FUNECOL$ISSUE.y)
  #######
  # TODO: NEED TO CORRECT THE ONES BELOW WHERE CATEGORY X AND Y DON'T MATCH.
  # summary((as.character(FUNECOL$CATEGORY.x)==as.character(FUNECOL$CATEGORY.y)))
  # which((as.character(FUNECOL$CATEGORY.x)==as.character(FUNECOL$CATEGORY.y))==FALSE)
  # which(is.na((as.character(FUNECOL$CATEGORY.x))==(as.character(FUNECOL$CATEGORY.y))))
  # FUNECOL[203,]
  # FUNECOL[583,]
  # FUNECOL[584,]
  # FUNECOL[582,]
  # FUNECOL[581,]
  # FUNECOL[580,]
  # FUNECOL[579,]
  # FUNECOL[202,]
  # FUNECOL[201,]
  # FUNECOL[167,]
  # FUNECOL[162,]
  ####
  # summary(FUNECOL$NAME.x==FUNECOL$NAME.y)
  DATAFILE$NAME.y<-NULL
  DATAFILE<-DATAFILE %>% rename("NAME"="NAME.x")
  # summary(DATAFILE$JOURNAL.x==DATAFILE$JOURNAL.y)
  # summary(DATAFILE$MIDDLE_NAME.x==DATAFILE$MIDDLE_NAME.y)
  DATAFILE$COUNTRY.x<-as.character(DATAFILE$COUNTRY.x)
  DATAFILE$COUNTRY.y<-as.character(DATAFILE$COUNTRY.y)
  # summary(DATAFILE$COUNTRY.x==DATAFILE$COUNTRY.y)
  country_fix<-which((DATAFILE$COUNTRY.x==DATAFILE$COUNTRY.y)=="FALSE")
  country_fix.df<-DATAFILE[country_fix,]  #KEEP COUNTRIES IN UK AS ORIGINAL NAMES< CONVERT TO UK LATER
  # summary(DATAFILE$JOURNAL.x==DATAFILE$JOURNAL.y)
  DATAFILE$INST.x<-as.character(DATAFILE$INST.x)
  DATAFILE$INST.y<-as.character(DATAFILE$INST.y)
  INST_fix<-which((DATAFILE$INST.x==DATAFILE$INST.y)=="FALSE")
  INST_fix.df<-DATAFILE[INST_fix,]  
  INST_fix.df<-filter(INST_fix.df,INST.y!="Noinst")
  INST_fix.df<-filter(INST_fix.df,INST.y!="NoInst")
  DATAFILE$INST.x<-gsub("Colorado State","Colorado State University", DATAFILE$INST.x)
  DATAFILE$INST.x<-gsub("Kentucky","University of Kentucky", DATAFILE$INST.x)
  DATAFILE$INST.x<-gsub("Aberdeen","University of Aberdeen", DATAFILE$INST.x)
  DATAFILE$INST.x<-gsub("Massachusetts","University of Massachusetts at Amherst", DATAFILE$INST.x)
  DATAFILE$INST.x<-gsub("Utah State","Utah State University", DATAFILE$INST.x)
  DATAFILE$INST.x<-gsub("Exeter","University of Exeter", DATAFILE$INST.x)
  DATAFILE$INST.x<-gsub("Edinburgh","University of Edinburgh", DATAFILE$INST.x)
  DATAFILE$INST.x<-gsub("Sheffield","University of Sheffield", DATAFILE$INST.x)
  DATAFILE$INST.x<-gsub("Stellenbosch","University of Stellenbosch", DATAFILE$INST.x)
  # summary(DATAFILE$INST.x==DATAFILE$INST.y)
  INST_fix<-which((DATAFILE$INST.x==DATAFILE$INST.y)=="FALSE")
  INST_fix.df<-DATAFILE[INST_fix,]  
  DATAFILE<-DATAFILE %>% select(-JOURNAL.y,-INST.y,-MIDDLE_NAME.y,-COUNTRY.y,-VOLUME.y,-ISSUE.y) %>% rename("JOURNAL"="JOURNAL.x","INST"="INST.x","VOLUME"="VOLUME.x","ISSUE"="ISSUE.x","MIDDLE_NAME"="MIDDLE_NAME.x","COUNTRY"="COUNTRY.x")
  # rm(INST_fix.df,INST_fix,country_fix,country_fix.df)
  rm(INST_fix.df,INST_fix,country_fix)
  
  DATAFILE$INST[DATAFILE$editor_id==1878 & DATAFILE$LAST_NAME=="Soler"]<-"CSIC"
  # levels(DATAFILE$UNIT)
  DATAFILE$UNIT<-as.character(DATAFILE$UNIT) # change to 'character'
  DATAFILE$UNIT[DATAFILE$editor_id==1878 & DATAFILE$LAST_NAME=="Soler"]<-"Estacion Experimental de Zonas Aridas"
  DATAFILE$UNIT<-as.factor(DATAFILE$UNIT) # change back to factor 
  DATAFILE$UNIT<-droplevels(DATAFILE$UNIT) 
  
  # levels(DATAFILE$CATEGORY.x)
  # levels(DATAFILE$CATEGORY.y)
  # summary(DATAFILE$CATEGORY.x==DATAFILE$CATEGORY.y)
  # head(DATAFILE,10)
  DATAFILE$TITLE.x<-NULL
  DATAFILE<-DATAFILE %>% rename("CATEGORY"="CATEGORY.y")
  
  # This will add "missing" to the first row of a group if the first INST is NA
  
  
  DATAFILE$INST<-as.character(DATAFILE$INST)
  DATAFILE$UNIT<-as.character(DATAFILE$UNIT)
  DATAFILE$CITY<-as.character(DATAFILE$CITY)
  DATAFILE$STATE<-as.character(DATAFILE$STATE)
  
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



#FUNCTION TO CLEAN AND PROCESS AMNAT
clean_AMNAT <- function(DATAFILE1,DATAFILE2) {
  
  # TODO: FIGURE OUT WTF AM NAT
  # TODO: AM NAT Needs institutions from 2006-2014 put in
  # NEED TO FIGURE OUT IF AMNAT, AMNAT0614,orAMNATEKB!!!!!!!!!!!!!!
  # AMNATpre2005<-filter(AMNAT.csv, YEAR<2006)
  # summary(AMNATpre2005)
  # AMNATpost2005<-filter(AMNAT.csv, YEAR>2005)
  # summary(AMNATpost2005)
  
  # FIND THE DUPLICATES, DELETE THE ONE THAT DOESN'T HAVE THE INST
  DATAFILE<-bind_rows(DATAFILE1,DATAFILE2) %>% 
    arrange(YEAR,LAST_NAME,FIRST_NAME,INST)
  # head(DATAFILE,20)
  # summary(DATAFILE)
  
  foo<-DATAFILE %>% 
    group_by(YEAR,LAST_NAME,FIRST_NAME) %>% 
    filter(n()>1) %>% 
    slice(1) %>% arrange(YEAR)
  
  DATAFILE %>% group_by(YEAR,LAST_NAME,FIRST_NAME) %>% 
    filter(n()>1) %>% summarize(n=n())
  
  # AmNat0614<-AmNat0614.csv %>% select(LAST_NAME,FIRST_NAME,
  # YEAR,INSTITUTION,COUNTRY,GENDER)
  # AMNAT_left<-left_join(AMNAT2006, AmNat0614, by="LAST_NAME", "YEAR")
  #   # str(AmNat0614)
  # str(AMNAT2006)
  # antijoiun1_AMNAT<-anti_join(AMNAT2006, AmNat0614, by="YEAR","editor_id")
  
  DATAFILE<-DATAFILE1
  
  DATAFILE$INST<-as.character(DATAFILE$INST)
  DATAFILE$CITY[DATAFILE$INST=="University of Hawaii" &
               DATAFILE$LAST_NAME=="Palumbi"]<-"Honolulu"
  DATAFILE$STATE[DATAFILE$INST=="University of Hawaii" &
                DATAFILE$LAST_NAME=="Palumbi"]<-"HI"
  DATAFILE$INST[DATAFILE$INST=="University of Hawaii" &
               DATAFILE$LAST_NAME=="Palumbi"]<-"University of Hawaii at Manoa"
  DATAFILE<-DATAFILE %>% rename("TITLE"="TITLE.x")
  # head(DATAFILE,10)
  return(DATAFILE)
}



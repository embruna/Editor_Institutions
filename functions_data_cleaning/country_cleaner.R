#FUNCTION TO SYSTEMATIZE and CORRECT COUNTRY NAMES
country_cleaner <- function(DATAFILE) {
  
  
  # DATAFILE<-ALLDATA
  # SOME NOTES:
  
  # DATAFILE$COUNTRY<-as.factor(DATAFILE$COUNTRY)
  # levels(DATAFILE$COUNTRY)
  DATAFILE$COUNTRY<-as.character(DATAFILE$COUNTRY)
  DATAFILE$COUNTRY[DATAFILE$COUNTRY=="australiatralia"]<-"australia"
  # DATAFILE$COUNTRY[DATAFILE$COUNTRY=="italy"]<-"italy"
  DATAFILE$COUNTRY[DATAFILE$COUNTRY=="p. r. china"]<-"china"
  # DATAFILE$COUNTRY[DATAFILE$COUNTRY=="mexico"]<-"mexico"
  DATAFILE$COUNTRY[DATAFILE$COUNTRY=="newzealand"]<-"new zealand"
  DATAFILE$COUNTRY[DATAFILE$COUNTRY=="n. ireland"]<-"northern ireland"
  DATAFILE$COUNTRY[DATAFILE$COUNTRY=="puertorico"]<-"puerto rico"
  DATAFILE$COUNTRY[DATAFILE$COUNTRY=="the netherlands"]<-"netherlands"
  DATAFILE$COUNTRY[DATAFILE$COUNTRY=="uk"]<-"united kingdom"
  DATAFILE$COUNTRY[DATAFILE$COUNTRY=="us"]<-"usa"
  # DATAFILE$COUNTRY[DATAFILE$COUNTRY=="usa"]<-"usa"
  # DATAFILE$COUNTRY[DATAFILE$COUNTRY=="usa"]<-"usa"
  DATAFILE$COUNTRY[DATAFILE$COUNTRY=="united states"]<-"usa"
  DATAFILE$COUNTRY[DATAFILE$COUNTRY=="missing"]<-NA
  # DATAFILE$COUNTRY<-gsub("iran", "iran", DATAFILE$COUNTRY)
  DATAFILE$COUNTRY<-gsub("california", "usa", DATAFILE$COUNTRY)
  DATAFILE$COUNTRY<-gsub("germanny", "germany", DATAFILE$COUNTRY)
  DATAFILE$COUNTRY<-gsub("p.r. china", "china", DATAFILE$COUNTRY)
  
  
  return(DATAFILE)
  
}
#FUNCTION TO CLEAN AND PROCESS LECO
clean_LECO <- function(DATAFILE) {
  
  DATAFILE$INST<-as.character(DATAFILE$INST)
  DATAFILE$CITY[DATAFILE$INST=="University of Nevada" & DATAFILE$LAST_NAME=="Walker"]<-"Las Vegas"
  
  levels(DATAFILE$STATE) <- c(levels(DATAFILE$STATE),"NV")
  DATAFILE$STATE[DATAFILE$INST=="University of Nevada" & DATAFILE$LAST_NAME=="Walker"]<-"NV"
  DATAFILE$INST[DATAFILE$INST=="University of Nevada" & DATAFILE$LAST_NAME=="Walker"]<-"University of Nevada-Las Vegas"
  
  DATAFILE$INST[DATAFILE$YEAR==1987 & DATAFILE$LAST_NAME=="Ramos"]<-"missing"
  # spot checks of DATAFILE
  
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
  head(DATAFILE,10)
  DATAFILE<-DATAFILE %>% fill(INST,.direction="down")
  DATAFILE<-DATAFILE %>% select(-X,-X.1,-X.2,-X.3,-X.4)
  DATAFILE<-rename(DATAFILE,"TITLE"="TITLE.x")
  ##
  # code to generate the list of which ones need to be 2x or have missing names is in 
  # Author_and_Inst_to_Check.R  
  # Need to convert that to a function, e.g., check(LECO)
  # Then need to add a function to upload the corrections/additions
  ##
  return(DATAFILE)
}



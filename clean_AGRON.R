#FUNCTION TO CLEAN AND PROCESS AGRON
clean_AGRON <- function(DATAFILE) {
  
  # AGRONOMY MISSING INST
  DATAFILE$INST<-as.factor(DATAFILE$INST)
  
  DATAFILE$INST<-as.character(DATAFILE$INST)
  DATAFILE$STATE<-as.character(DATAFILE$STATE)
  DATAFILE$COUNTRY<-as.character(DATAFILE$COUNTRY)
  DATAFILE$INST[DATAFILE$LAST_NAME=="Benbi" & DATAFILE$FIRST_NAME=="Dinesh"]<-"Punjab Agricultural University"
  DATAFILE$STATE[DATAFILE$LAST_NAME=="Benbi" & DATAFILE$FIRST_NAME=="Dinesh"]<-"Punjab"
  DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="Benbi" & DATAFILE$FIRST_NAME=="Dinesh"]<-"India"
  DATAFILE$COUNTRY[DATAFILE$LAST_NAME=="Pedreira" & DATAFILE$FIRST_NAME=="Carlos"]<-"Brazil"
  DATAFILE<-DATAFILE %>% rename("TITLE"="TITLE.x")

  
  ##
  # code to generate the list of which ones need to be 2x or have missing names is in 
  # Author_and_Inst_to_Check.R  
  # Need to convert that to a function, e.g., check(AGRONOMY)
  # Then need to add a function to upload the corrections/additions
  ##
  
  return(DATAFILE)
}



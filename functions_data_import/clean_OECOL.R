#FUNCTION TO CLEAN AND PROCESS OECOL
clean_OECOL <- function(DATAFILE) {
  
  
  DATAFILE$INST<-as.character(DATAFILE$INST)
  DATAFILE$CITY[DATAFILE$INST=="University of Nevada" & DATAFILE$LAST_NAME=="Walker"]<-"Las Vegas"
  DATAFILE$STATE[DATAFILE$INST=="University of Nevada" & DATAFILE$LAST_NAME=="Walker"]<-"NV"
  DATAFILE$INST[DATAFILE$INST=="University of Nevada" & DATAFILE$LAST_NAME=="Hayes"]<-"University of Nevada-Reno"
  DATAFILE<-DATAFILE %>% rename("TITLE"="TITLE.x")
  head(DATAFILE,10)
  ##
  # code to generate the list of which ones need to be 2x or have missing names is in 
  # Author_and_Inst_to_Check.R  
  # Need to convert that to a function, e.g., check(DATAFILEOGIA)
  # Then need to add a function to upload the corrections/additions
  ##
  return(DATAFILE)
}



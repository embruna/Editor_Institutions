#FUNCTION TO CLEAN AND PROCESS JANE
clean_JANE <- function(DATAFILE) {
  
  DATAFILE$UNIT<-as.character(DATAFILE$UNIT)
  DATAFILE$INST<-as.character(DATAFILE$INST)
  DATAFILE$UNIT[DATAFILE$editor_id==1702]<-"Deptartment of Biology"
  DATAFILE$INST[DATAFILE$editor_id==1702]<-"University of York"
  DATAFILE$COUNTRY[DATAFILE$editor_id==1702]<-"UK"
  head(DATAFILE,10)
  DATAFILE<-DATAFILE %>% rename("TITLE"="TITLE.x")
  ##
  # code to generate the list of which ones need to be 2x or have missing names is in 
  # Author_and_Inst_to_Check.R  
  # Need to convert that to a function, e.g., check(DATAFILE)
  # Then need to add a function to upload the corrections/additions
  ##
  return(DATAFILE)
}



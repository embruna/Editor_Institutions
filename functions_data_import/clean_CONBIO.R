#FUNCTION TO CLEAN AND PROCESS CONBIO
clean_CONBIO <- function(DATAFILE) {
  
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
  
  # fill in the institutions in subsequent years 
  # (only 1st year recorded) and then look for any thiat might need
  # to be double checked
  # DATAFILE_fixes<-DATAFILE_fixes %>% group_by(editor_id,INST) %>% 
  # distinct(editor_id,INST) %>% distinct(editor_id)
  
  head(DATAFILE,10)
  DATAFILE<-DATAFILE %>% fill(INST,.direction="down")
  DATAFILE<-rename(DATAFILE,"TITLE"="TITLE.x")
  
  ##
  # code to generate the list of which ones need to be 2x or have missing names is in 
  # Author_and_Inst_to_Check.R  
  # Need to convert that to a function, e.g., check(DATAFILE)
  # Then need to add a function to upload the corrections/additions
  ##
  return(DATAFILE)
}



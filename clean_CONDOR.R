#FUNCTION TO CLEAN AND PROCESS CONDOR
clean_CONDOR <- function(DATAFILE) {
  
  DATAFILE_raw$JOURNAL<-"CONDOR"
  DATAFILE_raw$editor_id<-NA
  DATAFILE<-DATAFILE_raw %>% select(JOURNAL,YEAR, editor_id,EDITOR_TITLE,
                                    FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,
                                    UNIT,CITY,STATE,COUNTRY,NOTES)
  DATAFILE$INST<-gsub("None given",NA, DATAFILE$INST)
  DATAFILE$UNIT<-gsub("None given",NA, DATAFILE$UNIT)
  DATAFILE$CITY<-gsub("None given",NA, DATAFILE$CITY)
  DATAFILE$STATE<-gsub("None given",NA, DATAFILE$STATE)
  DATAFILE$COUNTRY<-gsub("None given",NA, DATAFILE$COUNTRY)
  
  return(DATAFILE)
}



#FUNCTION TO CLEAN AND PROCESS MARECOL
clean_MARECOL <- function(DATAFILE) {
  DATAFILE$INST<-as.factor(DATAFILE$INST)
  DATAFILE<-rename(DATAFILE,"VOLUME"="Volume","ISSUE"="Issue")
  # str(DATAFILE)
  DATAFILE$no<-NULL
  
  # DATAFILE$FIRST_NAME<-NULL
  # DATAFILE$MIDDLE.NAME<-NULL
  # DATAFILE$LAST_NAME<-NULL
  DATAFILE$editor_id<-NA
  # DATAFILE<- DATAFILE %>% extract(NAME, c("FIRST_NAME","LAST_NAME"), "([^ ]+) (.*)")
  
  
  DATAFILE<- DATAFILE %>% extract(NAME, c("FIRST_NAME","LAST_NAME"), "([^ ]+) (.*)")
  DATAFILE<-DATAFILE %>% separate(LAST_NAME, c("MIDDLE_NAME", "LAST_NAME"),sep = " ", extra="merge", fill = "left",remove=FALSE)
  DATAFILE<-DATAFILE %>% separate(LAST_NAME, c("MIDDLE_NAME", "LAST_NAME"),sep = ". ", extra="merge",fill = "left",remove=FALSE)
  DATAFILE$FIRST_NAME<-gsub("[.]","",DATAFILE$FIRST_NAME)
  
  # head(DATAFILE,10)
  DATAFILE<-DATAFILE %>% rename("TITLE"="Title") %>% select(JOURNAL,YEAR,VOLUME,ISSUE,TITLE,FIRST_NAME,MIDDLE_NAME,LAST_NAME,INST,UNIT,CITY,STATE,COUNTRY,editor_id)
  # head(DATAFILE,10)
  
  
  return(DATAFILE)
}



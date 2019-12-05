#FUNCTION TO CLEAN AND PROCESS GCB
clean_GCB <- function(DATAFILE) {

  DATAFILE<- DATAFILE %>% extract(NAME, c("FIRST_NAME","LAST_NAME"), "([^ ]+) (.*)")
  DATAFILE<-DATAFILE %>% separate(LAST_NAME, c("MIDDLE_NAME", "LAST_NAME"),sep = " ", extra="merge", fill = "left",remove=FALSE)
  DATAFILE$editor_id<-NA

  return(DATAFILE)
}



#FUNCTION TO CLEAN AND PROCESS FEM
clean_FEM <- function(DATAFILE) {
  levels(DATAFILE$INST)<-c(levels(DATAFILE$INST),"University of Washington Seattle")
  DATAFILE$INST[DATAFILE$FIRST_NAME=="Douglas" & DATAFILE$LAST_NAME=="Maguire" & (DATAFILE$YEAR==1993 | DATAFILE$YEAR==1994)]<-"University of Washington Seattle"
  levels(DATAFILE$UNIT)<-c(DATAFILE$UNIT,"College of Forest Resources")
  DATAFILE$UNIT[DATAFILE$FIRST_NAME=="Douglas" & DATAFILE$LAST_NAME=="Maguire" & (DATAFILE$YEAR==1993 | DATAFILE$YEAR==1994)]<-"College of Forest Resources"
  DATAFILE$NOTES[DATAFILE$FIRST_NAME=="Douglas" & DATAFILE$LAST_NAME=="Maguire" & (DATAFILE$YEAR==1993 | DATAFILE$YEAR==1994)]<-"jrl lists 'seattle wa' as address, CV says that he left UW in 92 and in 93/94 he was at U Maine"
  
  DATAFILE<-DATAFILE %>% rename("TITLE"="TITLE.x")
  
  
  return(DATAFILE)
}



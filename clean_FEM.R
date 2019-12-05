#FUNCTION TO CLEAN AND PROCESS FEM
clean_FEM <- function(DATAFILE) {
  DATAFILE<-DATAFILE %>% rename("TITLE"="TITLE.x")
  
  
  return(DATAFILE)
}



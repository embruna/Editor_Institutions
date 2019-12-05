#FUNCTION TO CLEAN AND PROCESS JAPE
clean_JAPE <- function(DATAFILE) {
  
  head(DATAFILE,10)
  DATAFILE<-DATAFILE %>% rename("TITLE"="TITLE.x")
  head(DATAFILE,10)
  ##
  # code to generate the list of which ones need to be 2x or have missing names is in 
  # Author_and_Inst_to_Check.R  
  # Need to convert that to a function, e.g., check(AMNAT)
  # Then need to add a function to upload the corrections/additions
  ##
  return(DATAFILE)
}



# FUNCTION TO SYSTEMATIZE and CORRECT country NAMES
country_cleaner <- function(datafile) {
  # datafile<-alldata
  
  datafile$country <- as.character(datafile$country)
  datafile$country[datafile$country == "australiatralia"] <- "australia"
  datafile$country[datafile$country == "p. r. china"] <- "china"
  datafile$country[datafile$country == "newzealand"] <- "new zealand"
  datafile$country[datafile$country == "n. ireland"] <- "northern ireland"
  datafile$country[datafile$country == "puertorico"] <- "puerto rico"
  datafile$country[datafile$country == "the netherlands"] <- "netherlands"
  datafile$country[datafile$country == "uk"] <- "united kingdom"
  datafile$country[datafile$country == "us"] <- "usa"
  datafile$country[datafile$country == "united states"] <- "usa"
  datafile$country[datafile$country == "missing"] <- NA
  datafile$country <- gsub("california", "usa", datafile$country)
  datafile$country <- gsub("germanny", "germany", datafile$country)
  datafile$country <- gsub("p.r. china", "china", datafile$country)


  return(datafile)
}

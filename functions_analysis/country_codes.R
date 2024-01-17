country_codes <- function(dataset) {

  # data cleanup
  
  #remove (trim) the leading and trailing white spaces (not can do with one command as per: http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r)
  trim.trailing <- function (x) sub("\\s+$", "", x)
  dataset$country<-trim.trailing(dataset$country)
  trim.leading <- function (x)  sub("^\\s+", "", x)
  dataset$country<-trim.leading(dataset$country)
  
  # remove any double spaces
  dataset$country<-gsub("  ", " ", dataset$country, fixed=TRUE)
  
  # using country names and package "countrycode" to add a column with the 3 digit code 
  
  library(countrycode)
  
  # eliminate any rows with blanks in the 
  # dataset=author.geo for debugging purposes only
  dataset<-subset(dataset, country!="unknown")
  
  # chnage countries as needed, either because country names have changed or to reflect political organization
  
  dataset$country[dataset$country == "scotland"]  <- "united kingdom" 
  dataset$country[dataset$country == "scotland"]  <- "united kingdom"  
  dataset$country[dataset$country == "wales"]  <- "united kingdom"
  dataset$country[dataset$country == "wales"]  <- "united kingdom"
  dataset$country[dataset$country == "england"]  <- "united kingdom"
  dataset$country[dataset$country == "england"]  <- "united kingdom"
  dataset$country[dataset$country == "north ireland"]  <- "united kingdom" 
  dataset$country[dataset$country == "northern ireland"]  <- "united kingdom"  
  dataset$country[dataset$country == "n. ireland"]  <- "united kingdom"  
  dataset$country[dataset$country == "northern ireland"]  <- "united kingdom"  
  dataset$country[dataset$country == "german democratic republic"]  <- "germany" #removing old names
  dataset$country[dataset$country == "us"]  <- "usa" #in case any snuck in
  dataset$country[dataset$country == "yugoslavia"]  <- "croatia" #authors from pretinac
  # dataset$country[dataset$country == "french guiana"]  <- "france" 
  
  #wos data come capitalized, this corrects dsome of the odd ones
  dataset$country[dataset$country == "bophuthatswana"]  <- "south africa"
  dataset$country[dataset$country == "byelarus"]  <- "belarus"
  dataset$country[dataset$country == "ciskei"]  <- "south africa"
  dataset$country[dataset$country == "england"]  <- "england"
  dataset$country[dataset$country == "yemen arab rep"]  <- "yemen"
  dataset$country[dataset$country == "wales"]  <- "wales"
  dataset$country[dataset$country == "transkei"]  <- "south africa"
  dataset$country[dataset$country == "neth antilles"]  <- "netherland antilles"
  dataset$country[dataset$country == "mongol peo rep"]  <- "mongolia"
  dataset$country[dataset$country == "fr polynesia"]  <- "french polynesia"
  dataset$country[dataset$country == "fed rep ger"]  <- "germany"
  dataset$country[dataset$country == "ger dem rep"]  <- "germany"
  dataset$country[dataset$country == "guinea bissau"]  <- "guinea-bissau"
  dataset$country[dataset$country == "papua n guinea"]  <- "papua new guinea"  
  dataset$country[dataset$country == "w ind assoc st"]  <- NA
  dataset$country[dataset$country == "micronesia"]  <- "federated states of micronesia"    
  dataset$country[dataset$country == "cent afr republ"]  <- "central african republic"    
  
  # west indies associated states: collective name for a number of islands in
  # eastern caribbean whose status changed from being british colonies to states 
  # in free association with the united kingdom in 1967. 
  # included antigua, dominica, grenada, saint christopher-nevis-anguilla, 
  # saint lucia, and saint vincent.
  # treat as na for now because would need to check each authors home island
  
  #step3: change all the country names to the codes used in mapping
  #add a column with the 3 letter country codes to be consistent with the other datasets
  #maptools uses the iso 3166 three letter codes: https://en.wikipedia.org/wiki/iso_3166-1_alpha-3
  #the packahge countrycode will take your column of country names and convert them to iso3166-3 codes
  #i began by checking the values of country to see if there are any mistakes. to do so i just created a vector 
  
  #called codecheck
  
  dataset$geo.code<-countrycode(dataset$country, "country.name", "iso3c", warn = TRUE)
  #by setting "warn=true" it will tell you which ones it couldn't convert. because of spelling mistakes, etc.
  
  dataset$geo.code<-as.factor(dataset$geo.code)
  
  #deleting rows without country
  # dataset <- dataset[!is.na(dataset$geo.code),] 
  
  # you can correct these as follows in the dataframe with all the data, then add a 
  # new column to the dataframe with the country codes
  
  return(dataset)
  levels(as.factor(dataset$country))
}


  
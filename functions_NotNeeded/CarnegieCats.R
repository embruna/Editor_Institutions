#FUNCTION TO Organize and Summarize the Carnegie CLassifications
CarnegieCats <- function(carnegie_raw) {
  # carnegie_raw<-read_csv("./data_raw/carnegie/CarnegCategories_2015.csv")
  carnegie_raw<-carnegie_raw %>% select(Value,Label...4) %>% rename("basic2015"="Value", "classification"='Label...4')
  str(carnegie_raw)
  carnegie_raw<-as.data.frame(carnegie_raw)
  CarnRanks<-read_csv("./data_raw/carnegie/CarnegRankings_2015_Reduced.csv") %>% rename("basic2015"="BASIC2015")
  # CarnRanks<-CarnRanks %>% select(UNITID, NAME, CITY, STABBR,BASIC2015)
  str(CarnRanks)
  CarnRanks<-as.data.frame(CarnRanks)
  names(carnegie_raw)
  carnegie_raw<-left_join(carnegie_raw,CarnRanks,by="basic2015")
  # grouping different subcategories
  carnegie_raw$Category[(carnegie_raw$basic2015>0 & carnegie_raw$basic2015<10)|carnegie_raw$basic2015==14]<-"Associates"
  carnegie_raw$Category[carnegie_raw$basic2015>=10 & carnegie_raw$basic2015<=13]<-"SpecialFocus(2yr)"
  carnegie_raw$Category[carnegie_raw$basic2015>=15 & carnegie_raw$basic2015<=17]<-"Doctoral"
  carnegie_raw$Category[carnegie_raw$basic2015>=18 & carnegie_raw$basic2015<=20]<-"Masters"
  carnegie_raw$Category[carnegie_raw$basic2015>=21 & carnegie_raw$basic2015<=23]<-"Baccalaureate"
  carnegie_raw$Category[carnegie_raw$basic2015==24]<-"Faith-Related"
  carnegie_raw$Category[carnegie_raw$basic2015>=25 & carnegie_raw$basic2015<=26]<-"Medical/Health"
  carnegie_raw$Category[carnegie_raw$basic2015==27]<-"SpecialFocus-4yr-Engineering"
  carnegie_raw$Category[carnegie_raw$basic2015==28]<-"SpecialFocus-4yr-Tech"
  carnegie_raw$Category[carnegie_raw$basic2015==29]<-"SpecialFocus-4yr-Business"
  carnegie_raw$Category[carnegie_raw$basic2015==30]<-"SpecialFocus-4yr-Art-Music-Design"
  carnegie_raw$Category[carnegie_raw$basic2015==31]<-"Law"
  carnegie_raw$Category[carnegie_raw$basic2015==32]<-"Other"
  carnegie_raw$Category[carnegie_raw$basic2015==33]<-"Tribal"
  carnegie_raw$Category<-as.factor(carnegie_raw$Category)
  # levels(carnegie_raw$Category)
  # summary(carnegie_raw)
  rm(CarnRanks)
  names(carnegie_raw)<-tolower(names(carnegie_raw))
  return(carnegie_raw)
}



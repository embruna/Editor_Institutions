#FUNCTION TO Organize and Summarize the Carnegie CLassifications
CarnegieCats <- function(DATAFILE) {
  
  DATAFILE<-DATAFILE %>% select(Value,Label_1) %>% rename(BASIC2015=Value, Classification=Label_1)
  str(DATAFILE)
  DATAFILE<-as.data.frame(DATAFILE)
  CarnRanks<-read_csv("./Data/carnegie/CarnegRankings_2015_Reduced.csv", col_names = TRUE)
  # CarnRanks<-CarnRanks %>% select(UNITID, NAME, CITY, STABBR,BASIC2015)
  str(CarnRanks)
  CarnRanks<-as.data.frame(CarnRanks)
  DATAFILE<-left_join(DATAFILE,CarnRanks,by="BASIC2015")
  # grouping different subcategories
  DATAFILE$Category[(DATAFILE$BASIC2015>0 & DATAFILE$BASIC2015<10)|DATAFILE$BASIC2015==14]<-"Associates"
  DATAFILE$Category[DATAFILE$BASIC2015>=10 & DATAFILE$BASIC2015<=13]<-"SpecialFocus(2yr)"
  DATAFILE$Category[DATAFILE$BASIC2015>=15 & DATAFILE$BASIC2015<=17]<-"Doctoral"
  DATAFILE$Category[DATAFILE$BASIC2015>=18 & DATAFILE$BASIC2015<=20]<-"Masters"
  DATAFILE$Category[DATAFILE$BASIC2015>=21 & DATAFILE$BASIC2015<=23]<-"Baccalaureate"
  DATAFILE$Category[DATAFILE$BASIC2015==24]<-"Faith-Related"
  DATAFILE$Category[DATAFILE$BASIC2015>=25 & DATAFILE$BASIC2015<=26]<-"Medical/Health"
  DATAFILE$Category[DATAFILE$BASIC2015==27]<-"SpecialFocus-4yr-Engineering"
  DATAFILE$Category[DATAFILE$BASIC2015==28]<-"SpecialFocus-4yr-Tech"
  DATAFILE$Category[DATAFILE$BASIC2015==29]<-"SpecialFocus-4yr-Business"
  DATAFILE$Category[DATAFILE$BASIC2015==30]<-"SpecialFocus-4yr-Art-Music-Design"
  DATAFILE$Category[DATAFILE$BASIC2015==31]<-"Law"
  DATAFILE$Category[DATAFILE$BASIC2015==32]<-"Other"
  DATAFILE$Category[DATAFILE$BASIC2015==33]<-"Tribal"
  DATAFILE$Category<-as.factor(DATAFILE$Category)
  # levels(DATAFILE$Category)
  # summary(DATAFILE)
  rm(CarnRanks)
  return(DATAFILE)
}




ALLDATA
DRYADDATA
str(DRYADDATA)
str(ALLDATA)
ALLDATA$JOURNAL<-as.factor(ALLDATA$JOURNAL)
DRYADDATA$JOURNAL<-as.factor(DRYADDATA$JOURNAL)

# summary_DY<-DRYADDATA %>%  group_by(JOURNAL, YEAR)
# summary_DY_tbl<-summarize(summary_DY, countDY = n())
# summary_DY_tbl
# 
# 
# ALLDATA<-ALLDATA %>% select(JOURNAL, YEAR, editor_id, INST,UNIT,CITY,STATE,COUNTRY, FIRST_NAME,MIDDLE_NAME,LAST_NAME)
# summary_AD <-ALLDATA %>%  group_by(JOURNAL, YEAR)
# summary_AD_tbl<-summarize(summary_AD, countAD = n())
# summary_AD_tbl
# 
# joinedAD<-full_join(summary_AD_tbl,summary_DY_tbl)
# joinedAD$diff<-joinedAD$countAD-joinedAD$countDY
DRYADDATA$editor_id<-as.factor(DRYADDATA$editor_id)
ALLDATA2<-ALLDATA %>% select(JOURNAL,YEAR,FIRST_NAME,MIDDLE_NAME,LAST_NAME,editor_id,INST,UNIT,CITY,STATE,COUNTRY)
ALLDATA2$source<-"SciWri17"
DATA_JOIN<-full_join(DRYADDATA,ALLDATA2,by=c("JOURNAL","YEAR","editor_id"))
DATA_JOIN$COUNTRY.CHK<-DATA_JOIN$COUNTRY.x==DATA_JOIN$COUNTRY.y
DATA_JOIN$FIRST_NAME.CHK<-DATA_JOIN$FIRST_NAME.x==DATA_JOIN$FIRST_NAME.y
DATA_JOIN$MIDDLE_NAME.CHK<-DATA_JOIN$MIDDLE_NAME.x==DATA_JOIN$MIDDLE_NAME.y
DATA_JOIN$LAST_NAME.CHK<-DATA_JOIN$LAST_NAME.x==DATA_JOIN$LAST_NAME.y

FirstCheck<-DATA_JOIN[DATA_JOIN$FIRST_NAME.CHK=="FALSE",]
FirstCheck<-FirstCheck[complete.cases(FirstCheck$FIRST_NAME.CHK), ]
FirstCheck$double.check<-"first_name"

LastCheck<-DATA_JOIN[DATA_JOIN$LAST_NAME.CHK=="FALSE",]
LastCheck<-LastCheck[complete.cases(LastCheck$LAST_NAME.CHK), ]
LastCheck$double.check<-"last_name"

MidCheck<-DATA_JOIN[DATA_JOIN$MIDDLE_NAME.CHK=="FALSE",]
MidCheck<-MidCheck[complete.cases(MidCheck$MIDDLE_NAME.CHK), ]
MidCheck$double.check<-"middle_name"

CountryCheck<-DATA_JOIN[DATA_JOIN$COUNTRY.CHK=="FALSE",]
CountryCheck<-CountryCheck[complete.cases(CountryCheck$COUNTRY.CHK), ]
CountryCheck$double.check<-"country"

error_check_joined<-bind_rows(CountryCheck,MidCheck,LastCheck,FirstCheck)
write.csv(error_check_joined, file="error_check_joined.csv", row.names = T) #export it as a csv file

# DATA_JOIN$editor_id.CHK<-DATA_JOIN$editor_id.x==DATA_JOIN$editor_id.y
str(DATA_JOIN)

# first name check false
# last name check false
# middle name check false
# country check false
# VanDerMaarel 1033 or 1035 editor ID?


# TO FIND those that were DRYAD in BUT NOT SciWri

  DRYAD_butnot_SCIWRI<-anti_join(DRYADDATA,ALLDATA2,by=c("JOURNAL","YEAR","editor_id"))
  DRYAD_butnot_SCIWRI$code<-"DRYAD_butnot_SCIWRI"
    #lots because of the weirdness with funecol and jzool  
  DRYAD_butnot_SCIWRI<-DRYAD_butnot_SCIWRI %>% filter(JOURNAL!="JZOOL"&JOURNAL!="FUNECOL")
  write.csv(DRYAD_butnot_SCIWRI, file="DRYAD_butnot_SCIWRI.csv", row.names = T) #export it as a csv file
  

  
  # TO FIND those that were SCI WRI in BUT NOT DRYAD
  
  SCIWRI_butnot_DRYAD<-anti_join(ALLDATA2,DRYADDATA,by=c("JOURNAL","YEAR","editor_id"))
  SCIWRI_butnot_DRYAD$code<-"SCIWRI_butnot_DRYAD"
  
  #lots because of the weirdness with funecol and jzool  
  
  SCIWRI_butnot_DRYAD<-SCIWRI_butnot_DRYAD %>% filter(JOURNAL!="JZOOL"&JOURNAL!="FUNECOL"&JOURNAL!="MARECOL"&JOURNAL!="GCB")
  
  write.csv(SCIWRI_butnot_DRYAD, file="SCIWRI_butnot_DRYAD.csv", row.names = T) #export it as a csv file

  
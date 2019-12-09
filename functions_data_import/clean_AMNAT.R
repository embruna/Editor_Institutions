#FUNCTION TO CLEAN AND PROCESS AMNAT
clean_AMNAT <- function(DATAFILE1,DATAFILE2) {
  # DATAFILE1<-AMNAT1_raw
  # DATAFILE2<-AMNAT2_raw
  # # TODO: FIGURE OUT WTF AM NAT
  # TODO: AM NAT Needs institutions from 2006-2014 put in
  # NEED TO FIGURE OUT IF AMNAT, AMNAT0614,orAMNATEKB!!!!!!!!!!!!!!
  # AMNATpre2005<-filter(AMNAT.csv, YEAR<2006)
  # summary(AMNATpre2005)
  # AMNATpost2005<-filter(AMNAT.csv, YEAR>2005)
  # summary(AMNATpost2005)
  str(DATAFILE1)
  colnames(DATAFILE)
  # FIND THE DUPLICATES, DELETE THE ONE THAT DOESN'T HAVE THE INST
  DATAFILE<-full_join(DATAFILE1,DATAFILE2,by=c("LAST_NAME","FIRST_NAME","YEAR")) %>% 
    arrange(LAST_NAME,FIRST_NAME,YEAR) %>% select(JOURNAL.x,JOURNAL.y,YEAR,VOLUME.x,VOLUME.y,
                                                  ISSUE.x,ISSUE.y,editor_id,FIRST_NAME, 
                                                  MIDDLE_NAME.x,MIDDLE_NAME.y,LAST_NAME,SUFFIX,NAME,
                                                  TITLE.x.x,TITLE.x.y,CATEGORY.x,CATEGORY.y,INST.x,INST.y,
                                                  UNIT, CITY,STATE,COUNTRY.x,COUNTRY.y,geo.code,GENDER)
  
  DATAFILE$MIDDLE_NAME.y<-gsub("[.]","",DATAFILE$MIDDLE_NAME.y)
  
  summary(DATAFILE$MIDDLE_NAME.y==DATAFILE$MIDDLE_NAME.x)
  DATAFILE$MID_check<-DATAFILE$MIDDLE_NAME.y==DATAFILE$MIDDLE_NAME.x
  # # This identifies one mistake in thge original (ORIGINAL_DATA) that needs to be corrected 
  # str(both)
  DATAFILE$MIDDLE_NAME.y<-NULL
  DATAFILE$MID_check<-NULL
  DATAFILE<-DATAFILE %>% rename("MIDDLE_NAME"="MIDDLE_NAME.x")
  
  DATAFILE$ISSUE.y<-NULL
  DATAFILE<-DATAFILE %>% rename("ISSUE"="ISSUE.x")
  
  DATAFILE$JOURNAL.x<-"AMNAT"
  DATAFILE$JOURNAL.y<-NULL
  DATAFILE<-DATAFILE %>% rename("JOURNAL"="JOURNAL.x")
  
  
  DATAFILE$VOLUME.y<-NULL
  DATAFILE<-DATAFILE %>% rename("VOLUME"="VOLUME.x")
  
  levels(DATAFILE$TITLE.x.x)<-c(levels(DATAFILE$TITLE.x.x),levels(DATAFILE$TITLE.x.y))
  DATAFILE<-DATAFILE %>% mutate(TITLE.x.x = replace(TITLE.x.x, is.na(TITLE.x.x), TITLE.x.y[is.na(TITLE.x.x)]))
  DATAFILE$TITLE.x.y<-NULL
  DATAFILE<-DATAFILE %>% rename("TITLE"="TITLE.x.x")
  
  levels(DATAFILE$CATEGORY.x)<-c(levels(DATAFILE$CATEGORY.x),levels(DATAFILE$CATEGORY.y))
  DATAFILE<-DATAFILE %>% mutate(CATEGORY.x = replace(CATEGORY.x, is.na(CATEGORY.x), CATEGORY.y[is.na(CATEGORY.x)]))
  DATAFILE$CATEGORY.y<-NULL
  DATAFILE<-DATAFILE %>% rename("CATEGORY"="CATEGORY.x")
  
  

  DATAFILE$COUNTRY.x<-as.factor(DATAFILE$COUNTRY.x)
  country_levels<-(c(levels(DATAFILE$COUNTRY.x),levels(DATAFILE$COUNTRY.y)))
  levels(DATAFILE$COUNTRY.x)<-c(levels(DATAFILE$COUNTRY.x),country_levels)
  levels(DATAFILE$COUNTRY.y)<-c(levels(DATAFILE$COUNTRY.y),country_levels)
  
  summary(DATAFILE$COUNTRY.y==DATAFILE$COUNTRY.x)
  DATAFILE$COUNTRY_check<-DATAFILE$COUNTRY.y==DATAFILE$COUNTRY.x
  DATAFILE<-DATAFILE %>% mutate(COUNTRY.x = replace(COUNTRY.x, is.na(COUNTRY.x), COUNTRY.y[is.na(COUNTRY.x)]))
  DATAFILE$COUNTRY.y<-NULL
  DATAFILE<-DATAFILE %>% rename("COUNTRY"="COUNTRY.x")
  
  
  INST_levels<-(c(levels(DATAFILE$INST.x),levels(DATAFILE$INST.y)))
  levels(DATAFILE$INST.x)<-c(levels(DATAFILE$INST.x),INST_levels,"missing")
  levels(DATAFILE$INST.y)<-c(levels(DATAFILE$INST.y),INST_levels,"missing")
  summary(DATAFILE$INST.y==DATAFILE$INST.x)
  DATAFILE$INST_check<-DATAFILE$INST.y==DATAFILE$INST.x
  DATAFILE<-DATAFILE %>% mutate(INST.x = replace(INST.x, is.na(INST.x), INST.y[is.na(INST.x)]))
  DATAFILE$INST.y<-NULL
  DATAFILE<-DATAFILE %>% rename("INST"="INST.x")
  
  DATAFILE<-DATAFILE %>% 
    distinct(YEAR,LAST_NAME,FIRST_NAME,INST,.keep_all = TRUE) %>% 
    arrange (LAST_NAME,FIRST_NAME,YEAR)

  
  
  
  # foo<-DATAFILE %>% 
  #   group_by(YEAR,LAST_NAME,FIRST_NAME) %>% 
  #   filter(n()>1) %>% 
  #   slice(1) %>% arrange(YEAR)
  # 
  # DATAFILE %>% group_by(YEAR,LAST_NAME,FIRST_NAME) %>% 
  #   filter(n()>1) %>% summarize(n=n())
  # 
  # # AmNat0614<-AmNat0614.csv %>% select(LAST_NAME,FIRST_NAME,
  # YEAR,INSTITUTION,COUNTRY,GENDER)
  # AMNAT_left<-left_join(AMNAT2006, AmNat0614, by="LAST_NAME", "YEAR")
  #   # str(AmNat0614)
  # str(AMNAT2006)
  # antijoiun1_AMNAT<-anti_join(AMNAT2006, AmNat0614, by="YEAR","editor_id")
  
  # DATAFILE<-DATAFILE1
  
  DATAFILE$INST<-as.character(DATAFILE$INST)
  DATAFILE$CITY[DATAFILE$INST=="University of Hawaii" &
               DATAFILE$LAST_NAME=="Palumbi"]<-"Honolulu"
  DATAFILE$STATE[DATAFILE$INST=="University of Hawaii" &
                DATAFILE$LAST_NAME=="Palumbi"]<-"HI"
  DATAFILE$INST[DATAFILE$INST=="University of Hawaii" &
               DATAFILE$LAST_NAME=="Palumbi"]<-"University of Hawaii at Manoa"
  
  DATAFILE$INST[is.na(DATAFILE$INST)]<-"missing"
  DATAFILE$UNIT[is.na(DATAFILE$UNIT)]<-"missing"
  DATAFILE$CITY[is.na(DATAFILE$CITY)]<-"missing"
  DATAFILE$STATE[is.na(DATAFILE$STATE)]<-"missing"
  
  DATAFILE$INST<-trimws(DATAFILE$INST)
  DATAFILE$UNIT<-trimws(DATAFILE$UNIT)
  DATAFILE$CITY<-trimws(DATAFILE$CITY)
  DATAFILE$STATE<-trimws(DATAFILE$STATE)
  
  DATAFILE<-DATAFILE %>% arrange(LAST_NAME,FIRST_NAME,YEAR) 
  return(DATAFILE)
}



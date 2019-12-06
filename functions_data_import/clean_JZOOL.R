#FUNCTION TO CLEAN AND PROCESS JZOOL
clean_JZOOL <- function(DATAFILE1,DATAFILE2) {
  
  DATAFILE1$JOURNAL<-as.factor("JZOOL")
  summary(DATAFILE1$JOURNAL)
  summary(DATAFILE2$JOURNAL)
  DATAFILE<-full_join(DATAFILE1,DATAFILE2,by=c("YEAR","LAST_NAME","FIRST_NAME","MIDDLE_NAME"))
  rm(DATAFILE1,DATAFILE2)
  str(DATAFILE)
  colnames(DATAFILE)
  DATAFILE$JOURNAL.x<-as.character(DATAFILE$JOURNAL.x)
  DATAFILE$JOURNAL.y<-as.character(DATAFILE$JOURNAL.y)
  summary(DATAFILE$JOURNAL.x==DATAFILE$JOURNAL.y)
  DATAFILE$JOURNAL<-"JZOOL" 
  
  DATAFILE$COUNTRY.x<-as.character(DATAFILE$COUNTRY.x)
  DATAFILE$COUNTRY.y<-as.character(DATAFILE$COUNTRY.y)
  summary(DATAFILE$COUNTRY.x==DATAFILE$COUNTRY.y)
  country_fix<-which((DATAFILE$COUNTRY.x==DATAFILE$COUNTRY.y)=="FALSE")
  country_fix.df<-DATAFILE[country_fix,]  #KEEP COUNTRIES IN UK AS ORIGINAL NAMES< CONVERT TO UK LATER
  
  DATAFILE$NAME.x<-as.character(DATAFILE$NAME.x)
  DATAFILE$NAME.y<-as.character(DATAFILE$NAME.y)
  summary(DATAFILE$NAME.x==DATAFILE$NAME.y)
  NAME_fix<-which((DATAFILE$NAME.x==DATAFILE$NAME.y)=="FALSE")
  NAME_fix.df<-DATAFILE[NAME_fix,]  
  # the different ones are due to periods after initials
  
  DATAFILE$INST.x<-as.character(DATAFILE$INST.x)
  DATAFILE$INST.y<-as.character(DATAFILE$INST.y)
  summary(DATAFILE$INST.x==DATAFILE$INST.y)
  INST_fix<-which((DATAFILE$INST.x==DATAFILE$INST.y)=="FALSE")
  INST_fix.df<-DATAFILE[INST_fix,]  
  
  DATAFILE$VOLUME.x<-as.character(DATAFILE$VOLUME.x)
  DATAFILE$VOLUME.y<-as.character(DATAFILE$VOLUME.y)
  summary(DATAFILE$VOLUME.x==DATAFILE$VOLUME.y)
  vol_fix<-which((DATAFILE$VOLUME.x==DATAFILE$VOLUME.y)=="FALSE")
  
  DATAFILE$TITLE.x<-as.character(DATAFILE$TITLE.x)
  DATAFILE$TITLE<-as.character(DATAFILE$TITLE)
  summary(DATAFILE$TITLE.x==DATAFILE$TITLE)
  title_fix<-which((DATAFILE$TITLE.x==DATAFILE$TITLE)=="FALSE")
  title_fix.df<-DATAFILE[title_fix,]  
  title_fix.df<-select(title_fix.df,TITLE,TITLE.x)
  
  DATAFILE$ISSUE.x<-as.character(DATAFILE$ISSUE.x)
  DATAFILE$ISSUE.y<-as.character(DATAFILE$ISSUE.y)
  summary(DATAFILE$ISSUE.x==DATAFILE$ISSUE.y)
  issue_fix<-which((DATAFILE$ISSUE.x==DATAFILE$ISSUE.y)=="FALSE")
  issue_fix.df<-DATAFILE[issue_fix,]
  
  DATAFILE[55,]$ISSUE.x<-DATAFILE[55,]$ISSUE.y
  DATAFILE[56,]$ISSUE.x<-DATAFILE[56,]$ISSUE.y
  DATAFILE[57,]$ISSUE.x<-DATAFILE[57,]$ISSUE.y
  
  DATAFILE<-DATAFILE %>% select(-JOURNAL.x,-VOLUME.y,-JOURNAL.y,-ISSUE.y,-NAME.x,-NAME.y,-INST.y,-COUNTRY.y,-CATEGORY.y,-TITLE.x) %>% 
    rename("VOLUME"="VOLUME.x","ISSUE"="ISSUE.x","INST"="INST.x","CATEGORY"="CATEGORY.x","COUNTRY"="COUNTRY.x")
  rm(INST_fix.df,INST_fix,country_fix,country_fix.df,vol_fix,issue_fix,issue_fix.df,NAME_fix,NAME_fix.df,title_fix,title_fix.df)
  # str(DATAFILE)
  DATAFILE<-DATAFILE%>%select(JOURNAL,YEAR, VOLUME,ISSUE,editor_id,FIRST_NAME,MIDDLE_NAME,LAST_NAME,TITLE,CATEGORY,INST,UNIT,CITY,STATE,COUNTRY,geo.code,NOTES,GENDER)
  # str(DATAFILE)
  DATAFILE$JOURNAL<-as.character(DATAFILE$JOURNAL)
  DATAFILE$JOURNAL[DATAFILE$JOURNAL=="JZ"]<-"JZOOL"
  DATAFILE$JOURNAL<-as.factor(DATAFILE$JOURNAL)
  
  DATAFILE$INST<-trimws(DATAFILE$INST)
  DATAFILE$UNIT<-trimws(DATAFILE$UNIT)
  DATAFILE$CITY <-trimws(DATAFILE$CITY)
  DATAFILE$STATE<-trimws(DATAFILE$STATE)
  DATAFILE$COUNTRY<-trimws(DATAFILE$COUNTRY)
  DATAFILE$INST[DATAFILE$INST==""]<-NA
  DATAFILE$UNIT[DATAFILE$UNIT==""]<-NA
  DATAFILE$CITY[DATAFILE$CITY==""]<-NA
  DATAFILE$STATE[DATAFILE$STATE==""]<-NA
  DATAFILE$COUNTRY[DATAFILE$COUNTRY==""]<-NA
  DATAFILE<-DATAFILE %>% arrange(editor_id,YEAR,INST) %>% select(-VOLUME, -ISSUE, -TITLE,-CATEGORY,-geo.code)
  str(DATAFILE)
  # fill in the institutions in subsequent years (only 1st year recorded) and then look for any thiat might need
  # to be double checked
  head(DATAFILE,10)
  # NEED TO ADD "missing" to 1st line of group by edito where NA
  DATAFILE_1row<-DATAFILE %>% group_by(editor_id) %>% 
    arrange(editor_id,YEAR) %>% 
    filter(row_number()==1)
  levels(DATAFILE_1row$INST)<-c(levels(DATAFILE_1row$INST),"missing")
  DATAFILE_1row$INST<-replace(DATAFILE_1row$INST, is.na(DATAFILE_1row$INST), "missing")
  
  DATAFILE_remainder<-DATAFILE %>% group_by(editor_id) %>% 
    arrange(editor_id,YEAR) %>% 
    filter(row_number()>1)
  
  DATAFILE<-bind_rows(DATAFILE_remainder,DATAFILE_1row) 
  head(DATAFILE,10)
  DATAFILE<-DATAFILE %>% arrange(editor_id,YEAR) %>% fill(INST)
  head(DATAFILE,10)
  
  ##
  # code to generate the list of which ones need to be 2x or have missing names is in 
  # Author_and_Inst_to_Check.R  
  # Need to convert that to a function, e.g., check(DATAFILE)
  # Then need to add a function to upload the corrections/additions
  ##
  
  return(DATAFILE)
}



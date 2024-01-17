# This will load and the corrected files and make the required changes.
JamesCorrections1 <- function(ORIGINAL_DATA) {
  # ORIGINAL_DATA<-alldata  
  library(tidyverse)
  
  
  ##########
  multi1<-read_csv("./data_raw/Patrick_James_Data_Corrections/Complete/PJCorrections_1.csv", col_names = TRUE)
  multi1<-multi1 %>% fill(inst, unit,city,state,.direction="down")
  # str(multi1)
  ##########
  
  ##########
  multi2a<-read_csv("./data_raw/Patrick_James_Data_Corrections/Complete/PJCorrections_2a.csv", col_names = TRUE)
  multi2a<-multi2a %>% filter(journal=="CONBIO"|journal=="NEWPHYT") %>% fill(inst, unit,city,state,.direction="down") #delete out other journals this is conbio and new phyt
  # str(multi2a)
  ##########
  
  ##########
  multi2b<-read_csv("./data_raw/Patrick_James_Data_Corrections/Complete/PJCorrections_2b.csv", col_names = TRUE)
  multi2b<-multi2b %>% filter(journal=="CONBIO"|journal=="NEWPHYT") %>% fill(inst, unit,city,state,.direction="down") #delete out other journals this is conbio and new phyt
  # str(multi2b)
  ##########
  
  ##########
  BITR_inst<-read_csv("./data_raw/Patrick_James_Data_Corrections/Complete/PJCorrections_3_BITR.csv", col_names = TRUE)
  BITR_inst<-BITR_inst %>% fill(inst, unit,city,state,.direction="down")
  # str(BITR_inst)
  ##########
  
  ##########
  AMNAT_inst<-read_csv("./data_raw/Patrick_James_Data_Corrections/Complete/PJCorrections_4_AMNAT.csv", col_names = TRUE,na = c("", "N/A", "NA"), trim_ws = TRUE)
  AMNAT_inst<-AMNAT_inst %>% fill(inst, unit,city,state,.direction="down")%>% filter(journal=="AMNAT")
  # str(AMNAT_inst)
  ##########
  
  ##########
  JECOL_inst<-read_csv("./data_raw/Patrick_James_Data_Corrections/Complete/PJCorrections_6_JEcol.csv", col_names = TRUE)
  JECOL_inst<-JECOL_inst %>% fill(inst, unit,city,state,.direction="down")%>% filter(journal=="JECOL")
  # str(JECOL_inst)
  ##########
  
  ##########
  JAPE_inst<-read_csv("./data_raw/Patrick_James_Data_Corrections/Complete/PJCorrections_7_JAPE.csv", col_names = TRUE)
  JAPE_inst<-JAPE_inst %>% 
    rename("first_name"="first_na", "middle_name"="middle_","last_name"="last_na") %>% 
    fill(inst, unit,city,state,notes,.direction="down")%>% filter(journal=="JAPE")
  # JAPE_inst$editor_id<-NULL
  JAPE_inst$X1<-NULL
  # str(JAPE_inst)
  # TODO: something is going on with editot IDS
  ##########
  
  ##########
  
  # JBIOG_inst<-read_csv("./data_raw/Patrick_James_Data_Corrections/Complete/newPJCorrectionsDONE_JBIOG.csv", col_names = TRUE)
  # originally was the one above, changed to one above on 16 jan 2023. which is it???
  
  JBIOG_inst<-read_csv("./data_raw/Patrick_James_Data_Corrections/Complete/PJCorrections_JBIOG.csv", col_names = TRUE)
  # JBIOG_inst<-JBIOG_inst %>% select(-X1,-check) %>% rename("notes"=`please note here if INCORRECT`)
  JBIOG_inst<-JBIOG_inst %>% select(-`...1`,-check) 
  # This will keep only the ones we 2x / spotchecked that are correct OR are still missing the INST 
  JBIOG_inst<-JBIOG_inst[(!is.na(JBIOG_inst$notes)|
                            (JBIOG_inst$inst=="missing")),]
  JBIOG_inst<-JBIOG_inst %>% select(-city,-state)
  # str(JBIOG_inst)
  
  #########
  
  ##########
  # 
  
  
  
   LECO_inst<-read_csv("./data_raw/Patrick_James_Data_Corrections/Complete/newPJCorrections_10_LECO.csv", col_names = TRUE)
   #
    LECO_inst<-LECO_inst %>%  select(-`...1`,-check)
    # %>% rename("notes"=`please note if INCORRECT`)
    LECO_inst<-LECO_inst %>% select(journal, year,editor_id, first_name,
                                     middle_name, last_name, inst,city,notes)
   #  names(LECO_inst)
   #  nrow(LECO_inst)
   #  LANDSCAPEECO<-filter(ORIGINAL_DATA,journal=="LECO")
   #  subset<-right_join(LANDSCAPEECO, LECO_inst, by = c("editor_id","year"))
   #
   #  subset[subset == "missing"] <- NA
   #  colnames(subset)
   #  a <-subset$INST.x
   #  b <-tolower(subset$INST.y)
   #  c<-data.frame(a,b,a==b)
   #
   # both<-bind_rows(both,subset)
   # colnames(both)
  ##########
  
  ##########
  # PLANTECOL_inst<-read_csv("./data_raw/Patrick_James_Data_Corrections/Complete/PJCorrections_8_PLANTECOL.csv", col_names = TRUE)
  # PLANTECOL_inst<-PLANTECOL_inst %>%  select(-X1,-check) %>% rename("notes"=`Please note here if INCORRECT`)
  # PLANTECOL_inst<-PLANTECOL_inst[(!is.na(PLANTECOL_inst$notes)|
  #                         (PLANTECOL_inst$INST=="missing")),]
  ##########
  
  ##########
  # OIKOS
  ##########
  OIKOS_inst<-read_csv("./data_raw/Patrick_James_Data_Corrections/Complete/PJCorrections_OIKOS.csv", col_names = TRUE)
  OIKOS_inst<-OIKOS_inst %>% select(-`...1`)
  OIKOS_inst$volume<-as.character(OIKOS_inst$volume)
  OIKOS_inst$issue<-as.character(OIKOS_inst$issue)
  str(OIKOS_inst)
  
  
  ##########
  # OECOLOGIA
  ###########
  OECOL_inst<-read_csv("./data_raw/Patrick_James_Data_Corrections/Complete/PJCorrections_OECOL.csv", col_names = TRUE)
  names(OECOL_inst)
  OECOL_inst<-OECOL_inst %>%  
    select(-`...1`,-editor_id.y) %>% 
    rename("editor_id"="editor_id.x")
  OECOL_inst<-OECOL_inst[!(OECOL_inst$last_name=="Gough" & OECOL_inst$year==2012),]
  OECOL_inst<-OECOL_inst[!(OECOL_inst$last_name=="Heimpel" & OECOL_inst$year==2012),]
  OECOL_inst<-OECOL_inst[!(OECOL_inst$last_name=="Ibanez" & OECOL_inst$year==2012),]
  OECOL_inst<-OECOL_inst[!(OECOL_inst$last_name=="Layman" & OECOL_inst$year==2012),]
  OECOL_inst<-OECOL_inst[!(OECOL_inst$last_name=="Le Galliard" & OECOL_inst$year==2012),]
  
  OECOL_inst<-OECOL_inst %>% select(journal, year,editor_id, first_name,
                                    middle_name, last_name, inst,city,notes)
  
  
  
  
  
  ##########
  # JANE
  ##########
  
  
  
  
  
  
  
  
  #############
  INST_fix<-bind_rows(multi2b,multi2a,multi1,BITR_inst,JECOL_inst,AMNAT_inst,JAPE_inst,JBIOG_inst,OIKOS_inst,LECO_inst,OECOL_inst) %>% 
    distinct(editor_id,journal,year,.keep_all= TRUE) %>%     #there are some duplicates, best to remove them
    arrange(journal,editor_id,year)
  str(INST_fix)
  
  
  
  rm(multi2b,multi2a,multi1,BITR_inst,JECOL_inst,AMNAT_inst,JAPE_inst,JBIOG_inst,OIKOS_inst,LECO_inst,OECOL_inst)

  INST_fix<-INST_fix %>% select(journal, year, volume, issue, editor_id, first_name, middle_name,last_name,title,inst, unit, city, state, country, notes) 
  
  
  #   
#   # colnames(INST_fix)
#   # colnames(OECOL_inst)
#   
#   INST_fix$INST[INST_fix$INST=="UNKNOWN"]<-NA
#   INST_fix$INST[INST_fix$INST=="unknown"]<-NA
#   INST_fix$UNIT[INST_fix$UNIT=="UNKNOWN"]<-NA
#   INST_fix$city[INST_fix$city=="UNKNOWN"]<-NA
#   INST_fix$state[INST_fix$state=="UNKNOWN"]<-NA
#   INST_fix$state[INST_fix$state=="unknown"]<-NA
#   
#   
#   # INST_fix<-institution_cleaner(INST_fix)
#   # INST_fix<-INST_fix %>% select(-X1)
#   # 
#   head(INST_fix,20)
#   str(ORIGINAL_DATA$editor_id)
#   INST_fix$editor_id<-as.factor(INST_fix$editor_id)
#   str(INST_fix$editor_id)
#   ORIGINAL_DATA$editor_id<-as.factor(ORIGINAL_DATA$editor_id)
# 
#   both<-full_join(ORIGINAL_DATA,INST_fix,by=c("journal","year","last_name","first_name")) 
#   nrow(both)
#   nrow(ORIGINAL_DATA)
#   nrow(INST_fix)
#   
#   str(both)
#   
#   head(both)
#   str(both)
#   
#   colnames(both)
#   both<-select(both,journal,year,volume.x,volume.y,issue.x,issue.y,editor_id.x,editor_id.y,
#                first_name,middle_name.x,middle_name.y,last_name,
#                TITLE.x,TITLE.y,CATEGORY,INST.x,INST.y,UNIT.x,UNIT.y,city.x,city.y,state.x,state.y,
#                COUNTRY.x,COUNTRY.y,COUNTRY_Prior_Class,geo.code,geo.code_Prior_Class,
#                notes.x,notes.y,GENDER)
#   
#   
#   
#   # 
#   # 
#   # 
#   # 
#   # colnames(both)
#   # 
#   # 
#   # 
#   # both<-both %>% mutate(volume.x = replace(volume.x, is.na(volume.x),volume.y[is.na(volume.x)]))
#   # both$volume.y<-NULL
#   # both<-both %>% rename("volume"="volume.x")
#   # 
#   # both<-both %>% mutate(issue.x = replace(issue.x, is.na(issue.x),issue.y[is.na(issue.x)]))
#   # both$issue.y<-NULL
#   # both<-both %>% rename("issue"="issue.x")
#   # 
#   # both<-both %>% mutate(TITLE.x = replace(TITLE.x, is.na(TITLE.x),TITLE.y[is.na(TITLE.x)]))
#   # both$TITLE.y<-NULL
#   # both<-both %>% rename("TITLE"="TITLE.x")
#   # 
#   # # #CAN QUICKLY ID WHAT NEEDS TO BE FIXED AS FOLLOWS
#   # # str(both)
#   # # # FIRST NAME DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
#   # # summary(both$first_name.x==both$first_name.y) # 10 false
#   # # both$FIRST_check<-both$first_name.x==both$first_name.y
#   # # #spelling mistake in NAMES.y, so delete that column and the checm column
#   # # both$FIRST_check<-NULL
#   # # both$first_name.y<-NULL
#   # # both<-both %>% rename("first_name"="first_name.x")
#   # 
#   # # MIDDLE NAME DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
#   # summary(both$middle_name.x==both$middle_name.y)  # NO FALSE
#   # both$middle_check<-both$middle_name.x==both$middle_name.y
#   # both$middle_name.y<-NULL
#   # both$middle_check<-NULL
#   # both<-both %>% rename("middle_name"="middle_name.x")
#   # 
#   # # LAST NAME IFFERENCES BETWEEN ALL DATA AND CHECKED FILE
#   # # summary(both$last_name.x==both$last_name.y) # 21 FALSE
#   # # both$last_check<-both$last_name.x==both$last_name.y
#   # # # This identifies one mistake in thge original (ORIGINAL_DATA) that needs to be corrected 
#   # # str(both)
#   # 
#   # # both$last_name.y<-NULL
#   # # both$last_check<-NULL
#   # # both<-both %>% rename("last_name"="last_name.x")
#   # 
#   # # state DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
#   # both$state.x<-gsub("missing",NA,both$state.x)
#   #   # This will replace all the "NA" in state.x (origianlly no info) with the value from state.y (Patrick's data collection), if there is one 
#   # both<-both %>% mutate(state.x = replace(state.x, is.na(state.x), state.y[is.na(state.x)]))
#   # both$state_check<-both$state.x==both$state.y
#   # summary(both$state.x==both$state.y) # 94 FALSE
#   # # REPLACE THE ONES FALSE with y into x
#   # # both<-both %>% mutate(state.x = replace(state.x, state_check==FALSE, state.y))
#   # both$state.x <- ifelse(both$state_check==FALSE, both$state.y, both$state.x)
#   # 
#   # both$state.y<-NULL
#   # both$state_check<-NULL
#   # both<-both %>% rename("state"="state.x")
#   # 
#   # # This identifies one mistake in thge original (ORIGINAL_DATA) that needs to be corrected 
#   # 
#   # # INST DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
#   # both$INST.x<-as.character(both$INST.x)
#   # both$INST.x<-gsub("missing",NA,both$INST.x)
#   # # This will replace all the "NA" and "" in INST.x (origianlly no info) with the value from INST.y (Patrick's data collection), if there is one 
#   # both<-both %>% mutate(INST.x = replace(INST.x, is.na(INST.x),INST.y[is.na(INST.x)]))
#   # both <- both %>% mutate(INST.x = replace(INST.x, INST.x == "", NA))
#   # summary(both$INST.x==both$INST.y) # 235 FALSE
#   # both$INST_check<-both$INST.x==both$INST.y
#   # 
#   # INST_check<-filter(both,INST_check=="FALSE")
#   # INST_check_ok<-filter(both,INST_check==TRUE |is.na(INST_check))
#   # # 
#   # # write.csv(INST_check, file="./data_raw/Patrick_James_Data_Corrections/Complete/INST_corrections_2x.csv", row.names = F) #export it as a csv file
#   # 
#   # summary(both$INST.x==both$INST.y)
#   # both$INST_check<-both$INST.x==both$INST.y
#   # both$INST.y<-NULL
#   # both$INST_check<-NULL
#   # both<-both %>% rename("INST"="INST.x")
#   # 
#   # # city DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
#   # summary(both$city.x==both$city.y) # 52 FALSE
#   # both$city_check<-both$city.x==both$city.y
#   # city_check<-filter(both,city_check=="FALSE")
#   # city_check_ok<-filter(both,city_check==TRUE |is.na(city_check))
#   # write.csv(city_check, file="./data_raw/Patrick_James_Data_Corrections/Complete/city_corrections_2x.csv", row.names = F) #export it as a csv file
#   # 
#   # summary(both$city.x==both$city.y)
#   # both$city_check<-both$city.x==both$city.y
#   # # write.csv(city_check, file="./data_raw/Patrick_James_Data_Corrections/Complete/city_corrections_2x.csv", row.names = F) #export it as a csv file
#   # 
#   # # This will replace all the "NA" in city.x (origianlly no info) with the value from city.y (Patrick's data collection), if there is one 
#   # both<-both %>% mutate(city.x = replace(city.x, is.na(city.x), city.y[is.na(city.x)]))
#   # 
#   # both$city.y<-NULL
#   # both$city_check<-NULL
#   # both<-both %>% rename("city"="city.x")
#   # # str(both)
#   # 
#   # # TODO UNIT DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
#   # # both$UNIT.x<-as.character(both$UNIT.x)
#   # # both$UNIT.x<-gsub("",NA,both$UNIT.x)
#   # both$UNIT.x[both$UNIT.x == ""] <- NA
#   # summary(both$UNIT.x==both$UNIT.y) # 7 FALSE
#   # both$UNIT_check<-both$UNIT.x==both$UNIT.y
#   # 
#   # # No probs, just differences in words ("the, and", etc)
#   # # This will replace all the "NA" in UNIT.x (origianlly no info) with the value from UNIT.y (Patrick's data collection), if there is one 
#   # both<-both %>% mutate(UNIT.x = replace(UNIT.x, is.na(UNIT.x), UNIT.y[is.na(UNIT.x)]))
#   # 
#   # 
#   # both$UNIT.y<-NULL
#   # both$UNIT_check<-NULL
#   # both<-both %>% rename("UNIT"="UNIT.x")
#   # 
#   # # TODO COUNTRY DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
#   # 
#   # both <- both %>% mutate(COUNTRY.x = replace(COUNTRY.x, COUNTRY.x == "", NA))
#   # both$COUNTRY.x<-as.factor(both$COUNTRY.x)
#   # both$COUNTRY.y<-as.factor(both$COUNTRY.y)
#   # country_levels<-(c(levels(both$COUNTRY.x),levels(both$COUNTRY.y)))
#   # levels(both$COUNTRY.x)<-c(levels(both$COUNTRY.x),country_levels,NA)
#   # levels(both$COUNTRY.y)<-c(levels(both$COUNTRY.y),country_levels,NA)
#   # 
#   # # This will replace all the "NA" in city.x (origianlly no info) with the value from city.y (Patrick's data collection), if there is one 
#   # 
#   # levels(both$COUNTRY.x)
#   # str(both$COUNTRY.x)
#   # str(both$COUNTRY.y)
#   # 
#   # both<-both %>% mutate(COUNTRY.x = replace(COUNTRY.x, is.na(COUNTRY.x), COUNTRY.y[is.na(COUNTRY.x)]))
#   # summary(both$COUNTRY.x==both$COUNTRY.y) # 3552 FALSE
#   # both$country_check<-both$COUNTRY.x==both$COUNTRY.y
#   # 
#   # country_check<-filter(both,country_check=="FALSE")
#   # # write.csv(country_check, file="./data_raw/Patrick_James_Data_Corrections/Complete/COUNTRY_corrections_2x.csv", row.names = F) #export it as a csv file
#   # 
#   # 
#   # 
#   # 
#   # both$COUNTRY.y<-NULL
#   # both$country_check<-NULL
#   # both<-both %>% rename("COUNTRY"="COUNTRY.x")
#   # 
#   # # TODO notes DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
#   # 
#   # str(both$notes.x)
#   # both$notes <-paste(both$notes.x,both$notes.y, sep= " / ")
#   # both$notes[both$notes == "NA / NA"] <- NA
#   # both$notes <-gsub(" / NA","",both$notes)
#   # both$notes <-gsub("NA / ","",both$notes)
#   # both$notes.x<-NULL
#   # both$notes.y<-NULL
#   # 
#   # # both<-institution_cleaner(both)
#   # 
#   # 
#   # 
#   # # TODO: some error checking of states: 
#   # # canada instead of Canada
#   # # British Columbia in USA
#   # # Lower Austria     USA
#   # # Galicia     USA
#   # # England     USA
#   # # Uppland     USA
#   # # ONtario in USA
#   # # Gelderland
#   # 
#   # 
#   # # both$INST<-tolower(both$INST)
#   # 
#   # colnames(both)
#   # both$editor_id.y<-NULL
#   # both<-both %>% rename("editor_id"="editor_id.x")
#   # 
#   # both<-both[!(is.na(both$journal) & is.na(both$year)),]
#   # 
#   # 
#   # both<-both %>% 
#   #   group_by(journal,last_name,first_name) %>% 
#   #   mutate(INST = ifelse((row_number()==1 & is.na(INST)), "missing", INST))
#   # 
#   # 
#   # both<-both %>% 
#   #   group_by(journal,last_name,first_name) %>% 
#   #   mutate(UNIT = ifelse((row_number()==1 & is.na(UNIT)), "missing", UNIT))
#   # 
#   # both<-both %>% 
#   #   group_by(journal,last_name,first_name) %>% 
#   #   mutate(state = ifelse((row_number()==1 & is.na(state)), "missing", state))
#   # 
#   # 
#   # both<-both %>% 
#   #   group_by(journal,last_name,first_name) %>% 
#   #   mutate(city = ifelse((row_number()==1 & is.na(city)), "missing", city))
#   # 
#   # 
#   # both<-both %>% arrange(journal,last_name,first_name,year)
#   # 
#   # colnames(both)
#   # str(as.data.frame(both))
#   # str(ALLDATA)
#   # colnames(ALLDATA)
#   # colnames(both)==colnames(ALLDATA)
#   # 
#   # 
#   # 
#   # 
#   
#  #  
#  #  
#  # 
#   # 
  return(INST_fix)
}
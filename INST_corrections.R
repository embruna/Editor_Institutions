# This will load and the corrected files and make the required changes.

library(tidyverse)

##############################################################
# NO INST 2x NEEDED
# AJB 
# ECOLOGY
# FEM
# FUNECOL
# ECOGRAPHY
# JTE
##############################################################

##############################################################
# checked by Patrick, need to upload corrections
# Round 1: AGRON, ARES, EVOL (DONE)
# Round 2a: CONBIO,NEW PHYT (DONE)
# Round 2b: NEW PHYT (DONE)
# Round 3: BITR
# Round 4: AMNAT
# Round 6: JECOL
# Round 7: JAPE
# Round 5: BIOCON
# Round 8: PLANTECOL
# Round 9: JBIOG
# Round 10: LECO
# JANE
# JZOOL: will need extensive loopkup
# OECOL: will need extensive loopkup
# OIKOS: will need extensive loopkup
# AUK: not for this paper
# CONDOR: not for this paper
##############################################################


multi1<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_1.csv", col_names = TRUE)
multi1<-multi1 %>% fill(INST,UNIT,CITY,STATE)

multi2a<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_2a.csv", col_names = TRUE)
multi2a<-multi2a %>% filter(JOURNAL=="CONBIO"|JOURNAL=="NEWPHYT") %>% fill(INST,UNIT,CITY,STATE) #delete out other journals this is conbio and new phyt

multi2b<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_2b.csv", col_names = TRUE)
multi2b<-multi2b %>% filter(JOURNAL=="CONBIO"|JOURNAL=="NEWPHYT") %>% fill(INST,UNIT,CITY,STATE) #delete out other journals this is conbio and new phyt

BITR_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_3_BITR.csv", col_names = TRUE)
BITR_inst<-BITR_inst %>% fill(INST,UNIT,CITY,STATE)

AMNAT_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_4_AMNAT.csv", col_names = TRUE)
AMNAT_inst<-AMNAT_inst %>% fill(INST,UNIT,CITY,STATE)%>% filter(JOURNAL=="AMNAT")

JECOL_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_6_JEcol.csv", col_names = TRUE)
JECOL_inst<-JECOL_inst %>% fill(INST,UNIT,CITY,STATE)%>% filter(JOURNAL=="JECOL")

JAPE_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_7_JAPE.csv", col_names = TRUE)
JAPE_inst<-JAPE_inst %>% 
  rename("FIRST_NAME"="FIRST_NA", "MIDDLE_NAME"="MIDDLE_","LAST_NAME"="LAST_NA") %>% 
  fill(INST,UNIT,CITY,STATE,NOTES)%>% filter(JOURNAL=="JAPE")


JBIOG_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/newPJCorrectionsDONE_JBIOG.csv", col_names = TRUE)
JBIOG_inst<-JBIOG_inst %>% select(-X1,-check) %>% rename("NOTES"=`please note here if INCORRECT`)
# This will keep only the ones we 2x / spotchecked that are correct OR are still missing the INST 
JBIOG_inst<-JBIOG_inst[(!is.na(JBIOG_inst$NOTES)|
                          (JBIOG_inst$INST=="missing")),]


LECO_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/newPJCorrections_10_LECO.csv", col_names = TRUE)
names(LECO_inst)
LECO_inst<-LECO_inst %>%  select(-X1,-check) %>% rename("NOTES"=`please note if INCORRECT`)
LECO_inst<-LECO_inst[(!is.na(LECO_inst$NOTES)|
                          (LECO_inst$INST=="missing")),]

# 
# PLANTECOL_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_8_PLANTECOL.csv", col_names = TRUE)
# PLANTECOL_inst<-PLANTECOL_inst %>%  select(-X1,-check) %>% rename("NOTES"=`Please note here if INCORRECT`)
# PLANTECOL_inst<-PLANTECOL_inst[(!is.na(PLANTECOL_inst$NOTES)|
#                         (PLANTECOL_inst$INST=="missing")),]
# 





#############
INST_fix<-bind_rows(multi2b,multi2a,multi1,BITR_inst,JECOL_inst,AMNAT_inst,JAPE_inst,JBIOG_inst,LECO_inst) %>% 
  distinct(editor_id,JOURNAL,YEAR,.keep_all= TRUE) %>%     #there are some duplicates, best to remove them
  arrange(JOURNAL,editor_id,YEAR)


INST_fix$INST[INST_fix$INST=="UNKNOWN"]<-NA
INST_fix$INST[INST_fix$INST=="unknown"]<-NA
INST_fix$UNIT[INST_fix$UNIT=="UNKNOWN"]<-NA
INST_fix$CITY[INST_fix$CITY=="UNKNOWN"]<-NA
INST_fix$STATE[INST_fix$STATE=="UNKNOWN"]<-NA
INST_fix$STATE[INST_fix$STATE=="unknown"]<-NA


source("institution_cleaner.R")
INST_fix<-institution_cleaner(INST_fix)
INST_fix<-INST_fix %>% select(-X1)
head(INST_fix,20)
# 
# levels(as.factor(INST_fix$JOURNAL))
# 
# AG_INST_FIX<-INST_fix %>% filter(JOURNAL=="AGRONOMY") %>% 
#   arrange(editor_id,YEAR) %>% select(-"X1")
# write.csv(AG_INST_FIX, file="./Data/PJ_INST_FIX/AG_INST_FIX.csv", row.names = T) #export it as a csv file
# 
# AG
# 
# 
# AMNAT_INST_FIX<-INST_fix %>% filter(JOURNAL=="AMNAT") %>% 
#   arrange(editor_id,YEAR) %>% select(-"X1")
# 
# 
# write.csv(AMNAT_INST_FIX, file="./Data/PJ_INST_FIX/AMNAT_INST_FIX.csv", row.names = T) #export it as a csv file
# 
# AREES_INST_FIX<-INST_fix %>% filter(JOURNAL=="AREES") %>% 
#   arrange(editor_id,YEAR) %>% select(-"X1")
# 
# 
# 
# write.csv(AREES_INST_FIX, file="./Data/PJ_INST_FIX/AREES_INST_FIX.csv", row.names = T) #export it as a csv file
# 
# BITR_INST_FIX<-INST_fix %>% filter(JOURNAL=="BITR") %>% 
#   arrange(editor_id,YEAR) %>% select(-"X1")
# 
# write.csv(BITR_INST_FIX, file="./Data/PJ_INST_FIX/BITR_INST_FIX.csv", row.names = T) #export it as a csv file
# 
# CONBIO_INST_FIX<-INST_fix %>% filter(JOURNAL=="CONBIO") %>% 
#   arrange(editor_id,YEAR) %>% select(-"X1")
# 
# write.csv(CONBIO_INST_FIX, file="./Data/PJ_INST_FIX/CONBIO_INST_FIX.csv", row.names = T) #export it as a csv file
# 
# EVOL_INST_FIX<-INST_fix %>% filter(JOURNAL=="EVOL") %>% 
#   arrange(editor_id,YEAR) %>% select(-"X1")
# 
# write.csv(EVOL_INST_FIX, file="./Data/PJ_INST_FIX/EVOL_INST_FIX.csv", row.names = T) #export it as a csv file
# 
# FEM_INST_FIX<-INST_fix %>% filter(JOURNAL=="FEM") %>% 
#   arrange(editor_id,YEAR) %>% select(-"X1")
# 
#   write.csv(FEM_INST_FIX, file="./Data/PJ_INST_FIX/FEM_INST_FIX.csv", row.names = T) #export it as a csv file
# 
# NEWPHYT_INST_FIX<-INST_fix %>% filter(JOURNAL=="NEWPHYT") %>% 
#   arrange(editor_id,YEAR) %>% select(-"X1")
# 
# write.csv(NEWPHYT_INST_FIX, file="./Data/PJ_INST_FIX/NEWPHYT_INST_FIX.csv", row.names = T) #export it as a csv file
# 
# JECOL_INST_FIX<-INST_fix %>% filter(JOURNAL=="JECOL") %>% 
#   arrange(editor_id,YEAR) %>% select(-"X1")
# 
# write.csv(JECOL_INST_FIX, file="./Data/PJ_INST_FIX/JECOL_INST_FIX.csv", row.names = T) #export it as a csv file
# 
# JAPE_INST_FIX<-INST_fix %>% filter(JOURNAL=="JAPE") %>% 
#   arrange(editor_id,YEAR) %>% select(-"X1")
# 
# write.csv(JAPE_INST_FIX, file="./Data/PJ_INST_FIX/JAPE_INST_FIX.csv", row.names = T) #export it as a csv file
# 
# 
# 
# 
# LECO_INST_FIX<-INST_fix %>% filter(JOURNAL=="LECO") %>% 
#   arrange(editor_id,YEAR) %>% select(-"X1")
# 
# 
# write.csv(LECO_INST_FIX, file="./Data/PJ_INST_FIX/LECO_INST_FIX.csv", row.names = T) #export it as a csv file
# 
# JBIOG_INST_FIX<-INST_fix %>% filter(JOURNAL=="JBIOG") %>% 
#   arrange(editor_id,YEAR) %>% select(-"X1")
# write.csv(JBIOG_INST_FIX, file="./Data/PJ_INST_FIX/JBIOG_INST_FIX.csv", row.names = T) #export it as a csv file
# 
# ###########
# 
# CORRECT<-INST_fix %>% select(editor_id,JOURNAL,YEAR,LAST_NAME) # 4374
# CORRECT$source<-"correct"
# nrow(CORRECT)
# ORIG<-ALLDATA%>% select(editor_id,JOURNAL,YEAR,LAST_NAME)  # 25602
# ORIG$source<-"orig"
# nrow(ORIG)

#ARE THERE ANY IN CORRECT that *ARENT* in ALLDATA?
#THESE NEED TO BE ADDED TO ALLDATA
C_but_not_O<-anti_join(INST_fix,ALLDATA,by=c("editor_id","JOURNAL","YEAR"),keep=TRUE) #in correct but not orig 24
nrow(C_but_not_O)
C_but_not_O

#THESE ARE THE ONES IN ALLDATA but not CORRECTED 
O_butnot_C<-anti_join(ALLDATA,INST_fix,by=c("editor_id","JOURNAL","YEAR")) # in orig but not correct 21221
nrow(O_butnot_C)
O_butnot_C

# THESE ARE THE ONES IN BOTH CORRECTED AND ALLDATA
O_and_C<-inner_join(ALLDATA,INST_fix,by=c("editor_id","JOURNAL","YEAR")) #in correct but not orig 4381
nrow(O_and_C)


both<-full_join(ALLDATA,INST_fix,by=c("editor_id","JOURNAL","YEAR")) #in correct but not orig 4381
nrow(both)

head(both)

colnames(both)
both<-select(both,JOURNAL,YEAR,VOLUME,ISSUE,editor_id,
             FIRST_NAME.x,FIRST_NAME.y,MIDDLE_NAME.x,MIDDLE_NAME.y,LAST_NAME.x,LAST_NAME.y,
             TITLE,CATEGORY,INST.x,INST.y,UNIT.x,UNIT.y,CITY.x,CITY.y,STATE.x,STATE.y,
             COUNTRY.x,COUNTRY.y,COUNTRY_Prior_Class,geo.code,geo.code_Prior_Class,
             GENDER,dupe,NOTES.y)

#CAN QUICKLY ID WHAT NEEDS TO BE FIXED AS FOLLOWS

# FIRST NAME DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
summary(both$FIRST_NAME.x==both$FIRST_NAME.y) # 10 false
both$FIRST_check<-both$FIRST_NAME.x==both$FIRST_NAME.y
#spelling mistake in NAMES.y, so delete that column and the checm column
both$FIRST_check<-NULL
both$FIRST_NAME.y<-NULL
both<-both %>% rename("FIRST_NAME"="FIRST_NAME.x")

# MIDDLE NAME DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
summary(both$MIDDLE_NAME.x==both$MIDDLE_NAME.y)  # NO FALSE
both$MIDDLE_check<-both$MIDDLE_NAME.x==both$MIDDLE_NAME.y
both$MIDDLE_NAME.y<-NULL
both$MIDDLE_check<-NULL
both<-both %>% rename("MIDDLE_NAME"="MIDDLE_NAME.x")

# LAST NAME IFFERENCES BETWEEN ALL DATA AND CHECKED FILE
summary(both$LAST_NAME.x==both$LAST_NAME.y) # 21 FALSE
both$LAST_check<-both$LAST_NAME.x==both$LAST_NAME.y
# This identifies one mistake in thge original (ALLDATA) that needs to be corrected 
both$LAST_NAME.x[both$editor_id==1355 & both$FIRST_NAME.x=="Holmes"]<-"Rolston"
both$LAST_NAME.y<-NULL
both$LAST_check<-NULL
both<-both %>% rename("LAST_NAME"="LAST_NAME.x")

# STATE DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
summary(both$STATE.x==both$STATE.y) # 19 FALSE
both$STATE_check<-both$STATE.x==both$STATE.y

both$STATE.x[both$editor_id==703 & both$LAST_NAME=="Gibson" & both$CITY.x=="Carbondale"]<-"IL"
both$STATE.x[both$editor_id==330 & both$LAST_NAME=="Moss"]<-NA
both$STATE.x[both$editor_id==2583 & both$LAST_NAME=="Usher"]<-NA
both$STATE.x[both$editor_id==1004 & both$LAST_NAME=="Milner-Gulland"]<-NA
both$STATE.x[both$editor_id==1150 & both$LAST_NAME=="Belovsky" & both$INST.x=="Notre Dame University"]<-NA
both$STATE.x[both$editor_id==1501 & both$LAST_NAME=="Carlton"]<-"CT"
both$STATE.x[both$editor_id==1839 & both$LAST_NAME=="Rodriguez"]<-NA

both$STATE.y<-NULL
both$STATE_check<-NULL
both<-both %>% rename("STATE"="STATE.x")

# This identifies one mistake in thge original (ALLDATA) that needs to be corrected 
both$LAST_NAME.x[both$editor_id==1355 & both$FIRST_NAME.x=="Holmes"]<-"Rolston"
both$LAST_NAME.y<-NULL
both$LAST_check<-NULL
both<-both %>% rename("LAST_NAME"="LAST_NAME.x")


# UNIT DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
summary(both$UNIT.x==both$UNIT.y) # 7 FALSE
both$UNIT_check<-both$UNIT.x==both$UNIT.y
# No probs, just differences in words ("the, and", etc)
both$UNIT.y<-NULL
both$UNIT_check<-NULL
both<-both %>% rename("UNIT"="UNIT.x")

# INST DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
summary(both$INST.x==both$INST.y) # 235 FALSE
both$INST_check<-both$INST.x==both$INST.y

INST_corrections<-both %>% filter(INST_check==FALSE)
write.csv(INST_corrections, file="./Data/INST_corrections_2x.csv", row.names = T) #export it as a csv file

# CITY DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
summary(both$CITY.x==both$CITY.y) # 52 FALSE


# COUNTRY DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
country_levels<-(c(levels(both$COUNTRY.x),levels(both$COUNTRY.y)))
levels(both$COUNTRY.x)<-c(country_levels,levels(both$COUNTRY.x))
levels(both$COUNTRY.y)<-c(country_levels,levels(both$COUNTRY.y))
summary(both$COUNTRY.x==both$COUNTRY.y) # 3552 FALSE
both$country_check<-both$COUNTRY.x==both$COUNTRY.y


both$notes_check<-(both$NOTES.x==both$NOTES.y) # 1 FALSE












colnames(both)
anti_join(both,ALLDATA)

nrow(O_butnot_C)+nrow(C_but_not_O)+nrow(O_and_C)
nrow(ALLDATA)

levels(as.factor(Obut_notC$JOURNAL))
ALLDATA
foo<-inner_join(CORRECT,ORIG,by=c("editor_id","JOURNAL","YEAR")) #in correct but not orig 24
foo$check<-(foo$LAST_NAME.x==foo$LAST_NAME.y)

foo2<-anti_join(CORRECT,ORIG,by=c("editor_id","JOURNAL","YEAR")) # in orig but not correct 21085
nrow(ORIG)
nrow(CORRECT)
nrow(foo)
nrow(foo2)

duplicated_ALLDATA<-ALLDATA %>% 
  group_by(editor_id,JOURNAL,YEAR) %>% 
  filter(n()>1)


nrow(ORIG)+nrow(foo)
nrow(CORRECT)+nrow(foo2)+nrow(foo)
not_in_ALLDATA<-anti_join(INST_fix,ALLDATA,by="editor_id","JOURNAL")
not_in_ALLDATA<-anti_join(ALLDATA,INST_fix)
nrow(ALLDATA)
nrow(INST_fix)
nrow(not_in_ALLDATA)
nrow(not_in_ALLDATA)+nrow(INST_fix)


str(ALLDATA)
str(INST_fix)
ALLDATA$editor_id<-as.numeric(ALLDATA$editor_id)
levels(as.factor(ALLDATA$JOURNAL))
levels(INST_fix$JOURNAL)
ALLDATA_FIXES<-inner_join(ALLDATA,INST_fix,by=c("JOURNAL","editor_id","YEAR"))


  not_in_ALLDATA<-droplevels(not_in_ALLDATA)
###########

# ROADMAP FOR INCLUDING CORRECTIONS
CONBIO_ALLDATA<-ALLDATA %>% filter(JOURNAL=="CONBIO")
CONBIO_PJ<-read.csv("./Data/PJ_INST_FIX/CONBIO_INST_FIX.csv")
str(CONBIO_ALLDATA)
str(CONBIO_PJ)
CONBIO_PJ<-select(CONBIO_INST_FIX,-JOURNAL,-X1,-FIRST_NA,-MIDDLE_,-LAST_NA)
# any that are ONLY in CONBIO_ALLDATA and NOT in PJ
CB_noPJ<-anti_join(CONBIO_ALLDATA,CONBIO_PJ,by=c("editor_id","YEAR"))

# those from ALLData also in PJ
CB_and_PJ<-semi_join(CONBIO_ALLDATA,CONBIO_PJ,by=c("editor_id","YEAR"))


# Any in PJ but not in all data
CB_PJnoALLDATA<-anti_join(CONBIO_PJ,CONBIO_ALLDATA,by=c("editor_id","YEAR"))
# those from PJ also in ALLData PJ
CB_PJand_ALLDATA<-semi_join(CONBIO_PJ,CONBIO_ALLDATA,by=c("editor_id","YEAR"))

#now the ones in both (meaning they had to be 2x for some reason)
nrow(CB_and_PJ)
nrow(CB_PJand_ALLDATA)
CB_2check<-full_join(CB_and_PJ,CB_PJand_ALLDATA,by=c("editor_id","YEAR"))
str(CB_2check)









CONBIO<-droplevels(CONBIO)
# CONBIO<-select(CONBIO,-X)
CONBIO_PJ<-select(CONBIO_INST_FIX,-X)
CONBIO_INST_FIX$editor_id<-as.integer(CONBIO_INST_FIX$editor_id)
CB_2check<-full_join(CONBIO,CONBIO_INST_FIX,by=c("editor_id","YEAR"))
str(CB_2check)
names(CB_2check)



which(is.na(CB_2check$JOURNAL.x==CB_2check$JOURNAL.y))
CB_2check$INST.CB_2check<-CB_2check$INST.x==CB_2check$INST.y
levels(as.factor(CB_2check$INST.CB_2check))
CONBIO_INST_fix<-which((CB_2check$INST.x==CB_2check$INST.y)=="FALSE")

CB_2check$UNIT.CB_2check<-CB_2check$UNIT.x==CB_2check$UNIT.y
levels(as.factor(CB_2check$UNIT.CB_2check))

CB_2check$CITY.CB_2check<-CB_2check$CITY.x==CB_2check$CITY.y
levels(as.factor(CB_2check$CITY.CB_2check))

CB_2check$STATE.CB_2check<-CB_2check$STATE.x==CB_2check$STATE.y
levels(as.factor(CB_2check$STATE.CB_2check))

CB_2check$COUNTRY.CB_2check<-CB_2check$COUNTRY.x==CB_2check$COUNTRY.y                               
levels(as.factor(CB_2check$COUNTRY.CB_2check))

CONBIO_INST_fix<-which((CB_2check$INST.x==CB_2check$INST.y)=="FALSE"|is.na(CB_2check$INST.x==CB_2check$INST.y))
CONBIO_UNIT_fix<-which((CB_2check$UNIT.x==CB_2check$UNIT.y)=="FALSE"|is.na(CB_2check$UNIT.x==CB_2check$UNIT.y))
CONBIO_CITY_fix<-which((CB_2check$CITY.x==CB_2check$CITY.y)=="FALSE"|is.na(CB_2check$CITY.x==CB_2check$CITY.y))
CONBIO_STATE_fix<-which((CB_2check$STATE.x==CB_2check$STATE.y)=="FALSE"|is.na(CB_2check$STATE.x==CB_2check$STATE.y))
CONBIO_COUNTRY_fix<-which((CB_2check$COUNTRY.x==CB_2check$COUNTRY.y)=="FALSE"|is.na(CB_2check$COUNTRY.x==CB_2check$COUNTRY.y))

CB_2check$COUNTRY.x[CB_2check$editor_id==11]<-"England"

# 
# 
# levels(CB_2check$FIRST_NAME.x)<-c(levels(as.factor(CB_2check$FIRST_NAME.x)),levels(as.factor(CB_2check$FIRST_NAME.y))) # need to have the same levels of factor for x and y
# levels(CB_2check$FIRST_NAME.y)<-c(levels(as.factor(CB_2check$FIRST_NAME.x)),levels(as.factor(CB_2check$FIRST_NAME.y)))# need to have the same levels of factor for x and y
# CB_2check$FIRST_NAME.CB_2check<-as.factor(CB_2check$FIRST_NAME.x)==as.factor(CB_2check$FIRST_NAME.y)
# 
# levels(CB_2check$MIDDLE_NAME.x)<-c(levels(as.factor(CB_2check$MIDDLE_NAME.x)),levels(as.factor(CB_2check$MIDDLE_NAME.y))) # need to have the same levels of factor for x and y
# levels(CB_2check$MIDDLE_NAME.y)<-c(levels(as.factor(CB_2check$MIDDLE_NAME.x)),levels(as.factor(CB_2check$MIDDLE_NAME.y)))# need to have the same levels of factor for x and y
# CB_2check$MIDDLE_NAME.CB_2check<-as.factor(CB_2check$MIDDLE_NAME.x)==as.factor(CB_2check$MIDDLE_NAME.y)
# 
# levels(CB_2check$LAST_NAME.x)<-c(levels(as.factor(CB_2check$LAST_NAME.x)),levels(as.factor(CB_2check$LAST_NAME.y))) # need to have the same levels of factor for x and y
# levels(CB_2check$LAST_NAME.y)<-c(levels(as.factor(CB_2check$LAST_NAME.x)),levels(as.factor(CB_2check$LAST_NAME.y)))# need to have the same levels of factor for x and y
CB_2check$FIRST_NAME.test<-CB_2check$FIRST_NAME.x==CB_2check$FIRST_NAME.y
CB_2check$MIDDLE_NAME.test<-CB_2check$MIDDLE_NAME.x==CB_2check$MIDDLE_NAME.y
CB_2check$LAST_NAME.test<-CB_2check$LAST_NAME.x==CB_2check$LAST_NAME.y

names(CB_2check)

             
               


CB_2check<-CB_2check %>% select("JOURNAL.x","JOURNAL.y","YEAR","editor_id","FIRST_NAME.x","FIRST_NAME.y","FIRST_NAME.test",
                      "MIDDLE_NAME.x","MIDDLE_NAME.y","MIDDLE_NAME.test","LAST_NAME.x","LAST_NAME.y","LAST_NAME.test",
                      "INST.x","INST.y","INST.test","UNIT.x","UNIT.y","UNIT.CB_2check","CITY.x","CITY.y","CITY.CB_2check",
                      "STATE.x","STATE.y","STATE.test","COUNTRY.x","COUNTRY.y","COUNTRY.test","NOTES.x","NOTES.y")



CONBIO_INST_checks<-CB_2check[CONBIO_INST_fix,]
CONBIO_INST_checks<-arrange(CONBIO_INST_checks,INST.CB_2check)

CONBIO_UNIT_checks<-CB_2check[CONBIO_UNIT_fix,]
CONBIO_UNIT_checks<-arrange(CONBIO_UNIT_checks,UNIT.CB_2check)

CONBIO_CITY_checks<-CB_2check[CONBIO_CITY_fix,]
CONBIO_CITY_checks<-arrange(CONBIO_CITY_checks,CITY.CB_2check)

CONBIO_STATE_checks<-CB_2check[CONBIO_STATE_fix,]
CONBIO_STATE_checks<-arrange(CONBIO_STATE_checks,STATE.CB_2check)

CONBIO_COUNTRY_checks<-CB_2check[CONBIO_COUNTRY_fix,]
CONBIO_COUNTRY_checks<-arrange(CONBIO_COUNTRY_checks,COUNTRY.CB_2check)


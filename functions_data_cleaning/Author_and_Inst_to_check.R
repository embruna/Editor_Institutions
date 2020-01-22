# This generates the list of names and institutions that need to be double checked.

library(tidyverse)
ALLDATA<-read.csv("./output_review/ALLDATA_add_PJ_data.csv")
str(ALLDATA)
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
##############################################################


##############################################################
# WITH PATRICK to ADD INST
# Round 5: BIOCON
# Round 8: PLANTECOL
# Round 9: JBIOG
# Round 10: LECO
##############################################################


##############################################################
# STILL NEED TO ADD INST
# JANE
# JZOOL: will need extensive loopkup
# OECOL: will need extensive loopkup
# OIKOS: will need extensive loopkup
# AUK: not for this paper
# CONDOR: not for this paper
##############################################################



ALLDATA$INST[ALLDATA$INST==""]<-NA
ALLDATA$UNIT[ALLDATA$UNIT==""]<-NA
ALLDATA$COUNTRY[ALLDATA$COUNTRY==""]<-NA



# Starting what year? 
# ALLDATA_filtered<-filter(ALLDATA, YEAR>1984)
ALLDATA_filtered<-ALLDATA

summary(ALLDATA_filtered)
#These are all unique editor x inst combinations (ie. appear once for every inst they have including NA)
eds_all_inst<-ALLDATA_filtered %>% distinct(LAST_NAME,FIRST_NAME,INST, .keep_all= TRUE) %>% arrange(LAST_NAME,FIRST_NAME,INST,JOURNAL)
head(eds_all_inst,20)
# only the eds with multiple inst, including NA NOTE THIS DOES NOT INCLUDE ALL YEARS, ONLY ONE!
eds_multi_inst<-ALLDATA_filtered %>% distinct(LAST_NAME,FIRST_NAME,INST, .keep_all= TRUE) %>% group_by(LAST_NAME,FIRST_NAME) %>% filter(n()>1) %>% arrange(LAST_NAME,FIRST_NAME,INST,JOURNAL)
head(eds_multi_inst,23)


# Editors for which there are NAs, ALL YEARS THEY HAVE NA
eds_inst_NAs<-ALLDATA_filtered[is.na(ALLDATA_filtered$INST),]
eds_inst_NAs<-eds_inst_NAs %>% arrange(LAST_NAME,FIRST_NAME,JOURNAL,YEAR) 
head(eds_inst_NAs,75)
summary(eds_inst_NAs)

eds_inst_NAs %>% group_by(JOURNAL) %>% summarize(count=n()) %>% arrange(desc(count))

distinct(eds_inst_NAs,editor_id,.keep_all= TRUE) %>% group_by(JOURNAL) %>% summarize(count=n()) %>% arrange(desc(count))
# 
# # eds_inst_NAs_round1_fixes<-eds_inst_NAs %>% filter(JOURNAL=="BITR"|JOURNAL=="JZOOL"|JOURNAL=="EVOL"|JOURNAL=="AREES") %>% arrange(JOURNAL,YEAR,LAST_NAME)
# # write.csv(eds_inst_NAs_round1_fixes, file="eds_inst_NAs_round1_fixes.csv", row.names = T) #export it as a csv file
# ########
# head(eds_inst_NAs_fixes,40)
# eds_inst_NAs_fixes$editor_id<-droplevels(eds_inst_NAs_fixes$editor_id)
# 
# header_for_eds_inst_NAs_fixes<-eds_multi_inst %>% filter(editor_id %in% eds_inst_NAs_fixes$editor_id) %>% arrange(editor_id,INST,JOURNAL,LAST_NAME,FIRST_NAME)
# header_for_eds_inst_NAs_fixes<-header_for_eds_inst_NAs_fixes %>% distinct(LAST_NAME,FIRST_NAME,INST, .keep_all= TRUE) %>% group_by(LAST_NAME,FIRST_NAME) %>% filter(n()>1) %>% arrange(LAST_NAME,FIRST_NAME,INST,JOURNAL)
# header_for_eds_inst_NAs_fixes$INST <-as.factor(header_for_eds_inst_NAs_fixes$INST)
# header_for_eds_inst_NAs_fixes <-header_for_eds_inst_NAs_fixes %>% drop_na(INST)
#  
# eds_inst_NAs_fixes<-bind_rows(eds_inst_NAs_fixes,header_for_eds_inst_NAs_fixes) %>% arrange(LAST_NAME,FIRST_NAME,INST,JOURNAL,YEAR)
# head(eds_inst_NAs_fixes,60)
# # eds_inst_NAs_fixes<-split(eds_inst_NAs_fixes, eds_inst_NAs_fixes$editor_id)
# # write.csv(eds_inst_NAs_fixes, file="./Data/Patrick_James_Data_Corrections/eds_inst_NAs_fixes_round3_BITR.csv", row.names = T) #export it as a csv file
# 
# 
# #THis will cross with ALLDATA to include any that have the inst Name.
# 
# 
# ALLDATA_filtered %>%
#   filter(editor_id %in% eds_inst_NAs_round2_fixes$editor_id)
# 
# journal_count<-select(eds_inst_NAs,LAST_NAME,FIRST_NAME,JOURNAL) %>% # select variables to summarise
#   group_by(JOURNAL) %>% summarise(count=n()) %>% arrange(count)
# journal_count
# # Editors for which there are NAs, ONLY FIRST YEAR OF NA
# eds_inst_NAs_first<-eds_inst_NAs %>% group_by(LAST_NAME,FIRST_NAME,JOURNAL) %>% slice(n=1) %>% arrange(LAST_NAME,FIRST_NAME,JOURNAL,YEAR)
# eds_inst_NAs_first %>% group_by(JOURNAL) %>% summarize(n())
# # The editors may have an INST in another year, add it to compare
# eds_inst_NAs_plus<-drop_na(eds_all_inst,INST) 
# eds_inst_NAs_plus<-bind_rows(eds_inst_NAs_first,eds_inst_NAs_plus) %>% group_by(LAST_NAME,FIRST_NAME) %>% filter(n()>1) %>% arrange(LAST_NAME,FIRST_NAME,JOURNAL,YEAR)
# 
# 
# 
# write.csv(eds_inst_NAs_plus, file="eds_inst_NA.csv", row.names = T) #export it as a csv file
# 
# # These are the Editors for whom there is no institution in any year
# 
# 
# 




#######################
# JZOOL
#######################

JZOOL_checks<-ALLDATA %>% filter(JOURNAL=="JZOOL")
JZOOL_checks<-JZOOL %>% arrange(editor_id,YEAR,INST)
head(JZOOL_checks,10)
JZOOL_checks<-JZOOL_checks %>% group_by(editor_id,INST) %>% distinct(editor_id,INST)


# editors with >1 inst : 2x
JZOOL_ed_checks<-JZOOL_checks %>%  distinct(editor_id,INST) %>% group_by(editor_id) %>% filter(n()>1)

# Inst with >1 editors : 2x
JZOOL_inst_checks<-JZOOL_checks %>%  distinct(editor_id,INST) %>% group_by(INST) %>% filter(n()>1)

JZOOL_checks<-bind_rows(JZOOL_inst_checks,JZOOL_ed_checks) %>% distinct(editor_id,INST)

JZOOL_checks<-inner_join(JZOOL,JZOOL_checks) %>% distinct(editor_id,INST,.keep_all = TRUE)

JZOOL_checks$check<-"2x"

sub1<-JZOOL %>%
  group_by(editor_id) %>%
  do(sample_n(.,1))

sub2<-JZOOL %>%
  group_by(editor_id) %>%
  do(sample_n(.,1))


sub3<-JZOOL %>%
  group_by(editor_id) %>%
  do(sample_n(.,1))

# JZOOL_spotchecks<-sub1
JZOOL_spotchecks<-unique(bind_rows(sub1,sub2,sub3)) %>% arrange(YEAR,LAST_NAME)

JZOOL_spotchecks$check<-"spotcheck"

JZOOL_spotchecks<-JZOOL_spotchecks[sample(nrow(JZOOL_spotchecks), 50), ]
JZOOL_checks<-bind_rows(JZOOL_spotchecks,JZOOL_checks) %>%arrange(YEAR,LAST_NAME)
JZOOL_checks<-JZOOL_checks %>% distinct(editor_id, INST,.keep_all = TRUE)

write.csv(JZOOL_checks, file="./Data/Patrick_James_Data_Corrections/JZOOL_checks.csv", row.names = T) #export it as a csv file

rm(JZOOL_checks,sub1,sub2,sub3,JZOOL_ed_checks,JZOOL_inst_checks)





#######################
# OIKOS
#######################

OIKOS_checks<-ALLDATA %>% filter(JOURNAL=="OIKOS")
OIKOS_checks<-OIKOS_checks %>% arrange(editor_id,YEAR,INST)
head(OIKOS_checks,10)
# OIKOS_checks<-OIKOS_checks %>% group_by(editor_id,INST) %>% distinct(editor_id,INST,.keep_all=TRUE)



# editors with >1 inst : 2x

OIKOS_INST_NA<-OIKOS_checks %>%  filter(is.na(INST))  %>% group_by(editor_id) %>% distinct(editor_id,CITY,.keep_all=TRUE)
# 
# OIKOS_ed_checks<-OIKOS_checks %>%  distinct(editor_id,INST) %>% group_by(editor_id) %>% filter(n()>1) 
# 
# # Inst with >1 editors : 2x
# OIKOS_inst_checks<-OIKOS_checks %>%  distinct(editor_id,INST) %>% group_by(INST) %>% filter(n()>1) 
# 
# OIKOS_checks<-bind_rows(OIKOS_inst_checks,OIKOS_ed_checks) %>% distinct(editor_id,INST)
# 
# OIKOS_checks<-inner_join(OIKOS,OIKOS_checks) %>% distinct(editor_id,INST,.keep_all = TRUE)
# 
# OIKOS_checks$check<-"2x"
# 
# sub1<-OIKOS %>% 
#   group_by(editor_id) %>%
#   do(sample_n(.,1))
# 
# sub2<-OIKOS %>% 
#   group_by(editor_id) %>%
#   do(sample_n(.,1))
# 
# 
# sub3<-OIKOS %>% 
#   group_by(editor_id) %>%
#   do(sample_n(.,1))
# 
# # OIKOS_spotchecks<-sub1
# OIKOS_spotchecks<-unique(bind_rows(sub1,sub2,sub3)) %>% arrange(YEAR,LAST_NAME)
# 
# OIKOS_spotchecks$check<-"spotcheck"
# 
# OIKOS_checks<-bind_rows(OIKOS_spotchecks,OIKOS_checks) %>%arrange(YEAR,LAST_NAME)
# OIKOS_checks<-OIKOS_checks %>% distinct(editor_id, INST,.keep_all = TRUE)

write.csv(OIKOS_INST_NA, file="./Data/Patrick_James_Data_Corrections/OIKOS_checks.csv", row.names = T) #export it as a csv file

rm(OIKOS_checks,sub1,sub2,sub3,OIKOS_ed_checks,OIKOS_inst_checks)



#######################
# JANE
#######################

JANE_checks<-ALLDATA %>% filter(JOURNAL=="JANE")
JANE_checks<-JANE_checks %>% arrange(editor_id,YEAR,INST)
head(JANE_checks,10)
# JANE_checks<-JANE_checks %>% group_by(editor_id,INST) %>% distinct(editor_id,INST,.keep_all=TRUE)



# editors with >1 inst : 2x
JANE_checks$INST[is.na(JANE_checks$INST)]<-"missing"
JANE_checks$CITY[is.na(JANE_checks$CITY)]<-"missing"
JANE_checks$STATE[is.na(JANE_checks$STATE)]<-"missing"
JANE_checks$UNIT[is.na(JANE_checks$UNIT)]<-"missing"
JANE_INST_NA<-JANE_checks %>%  filter(is.na(INST)| INST=="missing")  %>% group_by(editor_id) %>% distinct(editor_id,CITY,.keep_all=TRUE)
# 
# JANE_ed_checks<-JANE_checks %>%  distinct(editor_id,INST) %>% group_by(editor_id) %>% filter(n()>1) 
# 
# # Inst with >1 editors : 2x
# JANE_inst_checks<-JANE_checks %>%  distinct(editor_id,INST) %>% group_by(INST) %>% filter(n()>1) 
# 
# JANE_checks<-bind_rows(JANE_inst_checks,JANE_ed_checks) %>% distinct(editor_id,INST)
# 
# JANE_checks<-inner_join(JANE,JANE_checks) %>% distinct(editor_id,INST,.keep_all = TRUE)
# 
# JANE_checks$check<-"2x"
# 
# sub1<-JANE %>% 
#   group_by(editor_id) %>%
#   do(sample_n(.,1))
# 
# sub2<-JANE %>% 
#   group_by(editor_id) %>%
#   do(sample_n(.,1))
# 
# 
# sub3<-JANE %>% 
#   group_by(editor_id) %>%
#   do(sample_n(.,1))
# 
# # JANE_spotchecks<-sub1
# JANE_spotchecks<-unique(bind_rows(sub1,sub2,sub3)) %>% arrange(YEAR,LAST_NAME)
# 
# JANE_spotchecks$check<-"spotcheck"
# 
# JANE_checks<-bind_rows(JANE_spotchecks,JANE_checks) %>%arrange(YEAR,LAST_NAME)
# JANE_checks<-JANE_checks %>% distinct(editor_id, INST,.keep_all = TRUE)

write.csv(JANE_INST_NA, file="./Data/Patrick_James_Data_Corrections/JANE_checks.csv", row.names = T) #export it as a csv file

rm(JANE_checks,sub1,sub2,sub3,JANE_ed_checks,JANE_inst_checks)


#######################
# LECO
#######################
LECO_checks<-ALLDATA %>% filter(JOURNAL=="LECO")
LECO_checks<-LECO %>% arrange(editor_id,YEAR,INST)
head(LECO_checks,10)
LECO_checks<-LECO_checks %>% group_by(editor_id,INST) %>% distinct(editor_id,INST)

# editors with >1 inst : 2x
LECO_ed_checks<-LECO_checks %>%  distinct(editor_id,INST) %>% group_by(editor_id) %>% filter(n()>1) 

# Inst with >1 editors : 2x
LECO_inst_checks<-LECO_checks %>%  distinct(editor_id,INST) %>% group_by(INST) %>% filter(n()>1) 

LECO_checks<-bind_rows(LECO_inst_checks,LECO_ed_checks) %>% distinct(editor_id,INST)

LECO_checks<-inner_join(LECO,LECO_checks) %>% distinct(editor_id,INST,.keep_all = TRUE)

LECO_checks$check<-"2x"

sub1<-LECO %>% 
  group_by(editor_id) %>%
  do(sample_n(.,1))

sub2<-LECO %>% 
  group_by(editor_id) %>%
  do(sample_n(.,1))


sub3<-LECO %>% 
  group_by(editor_id) %>%
  do(sample_n(.,1))

# LECO_spotchecks<-sub1
LECO_spotchecks<-unique(bind_rows(sub1,sub2,sub3)) %>% arrange(YEAR,LAST_NAME)

LECO_spotchecks$check<-"spotcheck"

LECO_checks<-bind_rows(LECO_spotchecks,LECO_checks) %>%arrange(YEAR,LAST_NAME)
LECO_checks<-LECO_checks %>% distinct(editor_id, INST,.keep_all = TRUE)

# write.csv(LECO_checks, file="./Data/Patrick_James_Data_Corrections/leco_checks.csv", row.names = T) #export it as a csv file

rm(LECO_checks,sub1,sub2,sub3,LECO_ed_checks,LECO_inst_checks)

#######################
# CONBIO
#######################

CONBIO_checks<-ALLDATA %>% filter(JOURNAL=="CONBIO")

CONBIO_checks<-CONBIO %>% arrange(editor_id,YEAR,INST)
head(CONBIO_checks,10)
CONBIO_checks<-CONBIO_checks %>% group_by(editor_id,INST) %>% distinct(editor_id,INST)

# editors with >1 inst : 2x
CONBIO_ed_checks<-CONBIO_checks %>%  distinct(editor_id,INST) %>% group_by(editor_id) %>% filter(n()>1)

# Inst with >1 editors : 2x
CONBIO_inst_checks<-CONBIO_checks %>%  distinct(editor_id,INST) %>% group_by(INST) %>% filter(n()>1)

CONBIO_checks<-bind_rows(CONBIO_inst_checks,CONBIO_ed_checks) %>% distinct(editor_id,INST)

CONBIO_checks<-inner_join(CONBIO,CONBIO_checks) %>% distinct(editor_id,INST,.keep_all = TRUE)

CONBIO_checks$check<-"2x"

sub1<-CONBIO %>%
  group_by(editor_id) %>%
  do(sample_n(.,1))

sub2<-CONBIO %>%
  group_by(editor_id) %>%
  do(sample_n(.,1))


sub3<-CONBIO %>%
  group_by(editor_id) %>%
  do(sample_n(.,1))

# CONBIO_spotchecks<-sub1
CONBIO_spotchecks<-unique(bind_rows(sub1,sub2,sub3)) %>% arrange(YEAR,LAST_NAME)

CONBIO_spotchecks$check<-"spotcheck"

CONBIO_spotchecks<-CONBIO_spotchecks[sample(nrow(CONBIO_spotchecks), 50), ]
CONBIO_checks<-bind_rows(CONBIO_spotchecks,CONBIO_checks) %>%arrange(YEAR,LAST_NAME)
CONBIO_checks<-CONBIO_checks %>% distinct(editor_id, INST,.keep_all = TRUE)

# write.csv(CONBIO_checks, file="./Data/Patrick_James_Data_Corrections/CONBIO_checks.csv", row.names = T) #export it as a csv file

rm(CONBIO_checks,sub1,sub2,sub3,CONBIO_ed_checks,CONBIO_inst_checks)

#######################
# AGrONOMY
#######################
AG_check<-ALLDATA %>% filter(JOURNAL=="AGRONOMY")
# AG_inst<-AG %>% group_by(LAST_NAME,FIRST_NAME,MIDDLE_NAME,YEAR,INST) %>% summarise(n()) %>% arrange(LAST_NAME,FIRST_NAME,YEAR)
# write.csv(AG_inst, file="AG_missing_inst.csv", row.names = T) #export it as a csv file

#######################
# JBIOG
#######################
JBIOG_check<-ALLDATA %>% filter(JOURNAL=="JBIOG")
JBIOG_checks<-JBIOG %>% arrange(editor_id,YEAR,INST)
head(JBIOG_checks,10)
JBIOG_checks<-JBIOG_checks %>% group_by(editor_id,INST) %>% distinct(editor_id,INST)

# editors with >1 inst : 2x
JBIOG_ed_checks<-JBIOG_checks %>%  distinct(editor_id,INST) %>% group_by(editor_id) %>% filter(n()>1) 

# Inst with >1 editors : 2x
JBIOG_inst_checks<-JBIOG_checks %>%  distinct(editor_id,INST) %>% group_by(INST) %>% filter(n()>1) 

JBIOG_checks<-bind_rows(JBIOG_inst_checks,JBIOG_ed_checks) %>% distinct(editor_id,INST)

JBIOG_checks<-inner_join(JBIOG,JBIOG_checks) %>% distinct(editor_id,INST,.keep_all = TRUE)

JBIOG_checks$check<-"2x"

sub1<-JBIOG %>% 
  group_by(editor_id) %>%
  do(sample_n(.,1))

sub2<-JBIOG %>% 
  group_by(editor_id) %>%
  do(sample_n(.,1))


sub3<-JBIOG %>% 
  group_by(editor_id) %>%
  do(sample_n(.,1))

JBIOG_spotchecks<-unique(bind_rows(sub1,sub2,sub3)) %>% arrange(YEAR,LAST_NAME)

JBIOG_spotchecks$check<-"spotcheck"

JBIOG_checks<-bind_rows(JBIOG_spotchecks,JBIOG_checks) %>%arrange(YEAR,LAST_NAME)
JBIOG_checks<-JBIOG_checks %>% distinct(editor_id, INST,.keep_all = TRUE)

write.csv(JBIOG_checks, file="./Data/Patrick_James_Data_Corrections/JBIOG_checks.csv", row.names = T) #export it as a csv file

rm(JBIOG_checks,sub1,sub2,sub3,JBIOG_ed_checks,JBIOG_inst_checks,JBIOG_1row,JBIOG_remainder)



#######################
# NEWPHYT
#######################
NEWPHYT_check<-ALLDATA %>% filter(JOURNAL=="NEWPHYT")
# NEWPHYT (MAIRA): 
# 1) NEEDS INST
NEWPHYT$INST<-as.factor(NEWPHYT$INST)
NEWPHYT_inst<-NEWPHYT %>% group_by(LAST_NAME,FIRST_NAME,MIDDLE_NAME,YEAR,INST) %>% summarise(n()) %>% arrange(LAST_NAME,FIRST_NAME,YEAR)
# write.csv(NEWPHYT_inst, file="NEWPHYT_missing_inst.csv", row.names = T) #export it as a csv file


#######################
# NAJFM
#######################

NAJFM_check<-ALLDATA %>% filter(JOURNAL=="NAJFM")
# NAJFM (MAIRA): 
# 1) NEEDS INST
NAJFM$INST<-as.factor(NAJFM$INST)
NAJFM_inst<-NAJFM %>% group_by(LAST_NAME,FIRST_NAME,MIDDLE_NAME,YEAR,INST) %>% summarise(n()) %>% arrange(LAST_NAME,FIRST_NAME,YEAR)
write.csv(NAJFM_inst, file="NAJFM_missing_inst.csv", row.names = T) #export it as a csv file

#######################
# GCB
#######################
GCB_check<-ALLDATA %>% filter(JOURNAL=="GCB")
#TO FILL OUT MISSING INST 
GCB$INST<-as.factor(GCB$INST)
GCB_inst<-GCB %>% group_by(NAME,YEAR,INST) %>% summarise(n()) %>% arrange(NAME,YEAR)
write.csv(GCB_inst, file="GCB_missing_inst.csv", row.names = T) #export it as a csv file


#######################
# PLANTECOL
#######################
PLANTECOL_checks<-ALLDATA %>% filter(JOURNAL=="PLANTECOL") %>% arrange(editor_id,YEAR,INST)
head(PLANTECOL_checks,10)
PLANTECOL_checks<-PLANTECOL_checks %>% group_by(editor_id,INST) %>% distinct(editor_id,INST)

# editors with >1 inst : 2x
PLANTECOL_ed_checks<-PLANTECOL_checks %>%  distinct(editor_id,INST) %>% group_by(editor_id) %>% filter(n()>1) 

# Inst with >1 editors : 2x
PLANTECOL_inst_checks<-PLANTECOL_checks %>%  distinct(editor_id,INST) %>% group_by(INST) %>% filter(n()>1) 

PLANTECOL_checks<-bind_rows(PLANTECOL_inst_checks,PLANTECOL_ed_checks) %>% distinct(editor_id,INST)

PLANTECOL_checks<-inner_join(PLANTECOL,PLANTECOL_checks) %>% distinct(editor_id,INST,.keep_all = TRUE)

PLANTECOL_checks$check<-"2x"

sub1<-PLANTECOL %>% 
  group_by(editor_id) %>%
  do(sample_n(.,1))

sub2<-PLANTECOL %>% 
  group_by(editor_id) %>%
  do(sample_n(.,1))


sub3<-PLANTECOL %>% 
  group_by(editor_id) %>%
  do(sample_n(.,1))

plantecol_spotchecks<-unique(bind_rows(sub1,sub2,sub3)) %>% arrange(YEAR,LAST_NAME)

plantecol_spotchecks$check<-"spotcheck"

PLANTECOL_checks<-bind_rows(plantecol_spotchecks,PLANTECOL_checks) %>%arrange(YEAR,LAST_NAME)
PLANTECOL_checks<-PLANTECOL_checks %>% distinct(editor_id, INST,.keep_all = TRUE)

write.csv(PLANTECOL_checks, file="./Data/Patrick_James_Data_Corrections/plantecol_checks.csv", row.names = T) #export it as a csv file


rm(PLANTECOL_checks,sub1,sub2,sub3,PLANTECOL_ed_checks,PLANTECOL_inst_checks) 




##############

#######################
# AGrONOMY
#######################
# all with NA
# OECOL_check<-ALLDATA %>% filter(JOURNAL=="OECOL") %>% arrange(LAST_NAME,YEAR) %>% filter(is.na(INST)) %>% distinct(LAST_NAME,FIRST_NAME,.keep_all = TRUE)
OECOL_check<-ALLDATA %>% filter(JOURNAL=="OECOL") %>% arrange(LAST_NAME,YEAR)
OECOL_check$INST[is.na(OECOL_check$INST)]<-"missing"
OECOL_check$UNIT[is.na(OECOL_check$UNIT)]<-"missing"
OECOL_check$CITY[is.na(OECOL_check$CITY)]<-"missing"
OECOL_check$STATE[is.na(OECOL_check$STATE)]<-"missing"
OECOL_check<-OECOL_check %>% filter(INST=="missing" |INST== "DOUBLE CHECK INST") %>% arrange(LAST_NAME,YEAR) %>% distinct(LAST_NAME,FIRST_NAME,.keep_all = TRUE)
write.csv(OECOL_check, file="./Data/Patrick_James_Data_Corrections/OECOL_checks.csv", row.names = T) #export it as a csv file
# 
# OECOL_check2<-filter(ALLDATA,JOURNAL=="OECOL") %>% filter(is.na(INST)==FALSE) %>% distinct(LAST_NAME,FIRST_NAME,.keep_all = TRUE)
# 
# OECOL_check3<-left_join(OECOL_check,OECOL_check2,by="editor_id.x") %>% select(INST.x,INST.y)
# # AG_inst<-AG %>% group_by(LAST_NAME,FIRST_NAME,MIDDLE_NAME,YEAR,INST) %>% summarise(n()) %>% arrange(LAST_NAME,FIRST_NAME,YEAR)
# # write.csv(AG_inst, file="AG_missing_inst.csv", row.names = T) #export it as a csv file
# OECOL_inst<-OECOL_check %>% distinct(LAST_NAME,FIRST_NAME,INST) %>% group_by(LAST_NAME,FIRST_NAME) %>% summarize(n_distinct(INST))

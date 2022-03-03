# This generates the list of names and institutions that need to be double checked.

library(tidyverse)
alldata<-read.csv("./output_review/alldata_add_PJ_data.csv")
str(alldata)
##############################################################
# NO inst 2x NEEDED
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
# WITH PATRICK to ADD inst
# Round 5: BIOCON
# Round 8: PLANTECOL
# Round 9: JBIOG
# Round 10: LECO
##############################################################


##############################################################
# STILL NEED TO ADD inst
# JANE
# JZOOL: will need extensive loopkup
# OECOL: will need extensive loopkup
# OIKOS: will need extensive loopkup
# AUK: not for this paper
# CONDOR: not for this paper
##############################################################



alldata$inst[alldata$inst==""]<-NA
alldata$unit[alldata$unit==""]<-NA
alldata$country[alldata$country==""]<-NA



# Starting what year? 
# alldata_filtered<-filter(alldata, year>1984)
alldata_filtered<-alldata

summary(alldata_filtered)
#These are all unique editor x inst combinations (ie. appear once for every inst they have including NA)
eds_all_inst<-alldata_filtered %>% 
  distinct(last_name,first_name,inst, .keep_all= TRUE) %>% 
  arrange(last_name,first_name,inst,journal)
head(eds_all_inst,20)
# only the eds with multiple inst, including NA NOTE THIS DOES NOT INCLUDE ALL yearS, ONLY ONE!
eds_multi_inst<-alldata_filtered %>%
  distinct(last_name,first_name,inst, .keep_all= TRUE) %>%
  group_by(last_name,first_name) %>% filter(n()>1) %>% 
  arrange(last_name,first_name,inst,journal)
head(eds_multi_inst,23)


# Editors for which there are NAs, ALL yearS THEY HAVE NA
eds_inst_NAs<-alldata_filtered[is.na(alldata_filtered$inst),]
eds_inst_NAs<-eds_inst_NAs %>% arrange(last_name,first_name,journal,year) 
head(eds_inst_NAs,75)
summary(eds_inst_NAs)

eds_inst_NAs %>% 
  group_by(journal) %>% 
  summarize(count=n()) %>%
  arrange(desc(count))

distinct(eds_inst_NAs,editor_id,.keep_all= TRUE) %>%
  group_by(journal) %>%
  summarize(count=n()) %>%
  arrange(desc(count))
# 
# # eds_inst_NAs_round1_fixes<-eds_inst_NAs %>% filter(journal=="BITR"|journal=="JZOOL"|journal=="EVOL"|journal=="AREES") %>% arrange(journal,year,last_name)
# # write.csv(eds_inst_NAs_round1_fixes, file="eds_inst_NAs_round1_fixes.csv", row.names = T) #export it as a csv file
# ########
# head(eds_inst_NAs_fixes,40)
# eds_inst_NAs_fixes$editor_id<-droplevels(eds_inst_NAs_fixes$editor_id)
# 
# header_for_eds_inst_NAs_fixes<-eds_multi_inst %>% filter(editor_id %in% eds_inst_NAs_fixes$editor_id) %>% arrange(editor_id,inst,journal,last_name,first_name)
# header_for_eds_inst_NAs_fixes<-header_for_eds_inst_NAs_fixes %>% distinct(last_name,first_name,inst, .keep_all= TRUE) %>% group_by(last_name,first_name) %>% filter(n()>1) %>% arrange(last_name,first_name,inst,journal)
# header_for_eds_inst_NAs_fixes$inst <-as.factor(header_for_eds_inst_NAs_fixes$inst)
# header_for_eds_inst_NAs_fixes <-header_for_eds_inst_NAs_fixes %>% drop_na(inst)
#  
# eds_inst_NAs_fixes<-bind_rows(eds_inst_NAs_fixes,header_for_eds_inst_NAs_fixes) %>% arrange(last_name,first_name,inst,journal,year)
# head(eds_inst_NAs_fixes,60)
# # eds_inst_NAs_fixes<-split(eds_inst_NAs_fixes, eds_inst_NAs_fixes$editor_id)
# # write.csv(eds_inst_NAs_fixes, file="./Data/Patrick_James_Data_Corrections/eds_inst_NAs_fixes_round3_BITR.csv", row.names = T) #export it as a csv file
# 
# 
# #THis will cross with alldata to include any that have the inst Name.
# 
# 
# alldata_filtered %>%
#   filter(editor_id %in% eds_inst_NAs_round2_fixes$editor_id)
# 
# journal_count<-select(eds_inst_NAs,last_name,first_name,journal) %>% # select variables to summarise
#   group_by(journal) %>% summarise(count=n()) %>% arrange(count)
# journal_count
# # Editors for which there are NAs, ONLY FIRST year OF NA
# eds_inst_NAs_first<-eds_inst_NAs %>% group_by(last_name,first_name,journal) %>% slice(n=1) %>% arrange(last_name,first_name,journal,year)
# eds_inst_NAs_first %>% group_by(journal) %>% summarize(n())
# # The editors may have an inst in another year, add it to compare
# eds_inst_NAs_plus<-drop_na(eds_all_inst,inst) 
# eds_inst_NAs_plus<-bind_rows(eds_inst_NAs_first,eds_inst_NAs_plus) %>% group_by(last_name,first_name) %>% filter(n()>1) %>% arrange(last_name,first_name,journal,year)
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

JZOOL_checks<-alldata %>% filter(journal=="JZOOL")
JZOOL_checks<-JZOOL %>% arrange(editor_id,year,inst)
head(JZOOL_checks,10)
JZOOL_checks<-JZOOL_checks %>% group_by(editor_id,inst) %>% distinct(editor_id,inst)


# editors with >1 inst : 2x
JZOOL_ed_checks<-JZOOL_checks %>%  distinct(editor_id,inst) %>% group_by(editor_id) %>% filter(n()>1)

# Inst with >1 editors : 2x
JZOOL_inst_checks<-JZOOL_checks %>%  distinct(editor_id,inst) %>% group_by(inst) %>% filter(n()>1)

JZOOL_checks<-bind_rows(JZOOL_inst_checks,JZOOL_ed_checks) %>% distinct(editor_id,inst)

JZOOL_checks<-inner_join(JZOOL,JZOOL_checks) %>% distinct(editor_id,inst,.keep_all = TRUE)

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
JZOOL_spotchecks<-unique(bind_rows(sub1,sub2,sub3)) %>% arrange(year,last_name)

JZOOL_spotchecks$check<-"spotcheck"

JZOOL_spotchecks<-JZOOL_spotchecks[sample(nrow(JZOOL_spotchecks), 50), ]
JZOOL_checks<-bind_rows(JZOOL_spotchecks,JZOOL_checks) %>%arrange(year,last_name)
JZOOL_checks<-JZOOL_checks %>% distinct(editor_id, inst,.keep_all = TRUE)

write.csv(JZOOL_checks, file="./Data/Patrick_James_Data_Corrections/JZOOL_checks.csv", row.names = T) #export it as a csv file

rm(JZOOL_checks,sub1,sub2,sub3,JZOOL_ed_checks,JZOOL_inst_checks)





#######################
# OIKOS
#######################

OIKOS_checks<-alldata %>% filter(journal=="OIKOS")
OIKOS_checks<-OIKOS_checks %>% arrange(editor_id,year,inst)
head(OIKOS_checks,10)
# OIKOS_checks<-OIKOS_checks %>% group_by(editor_id,inst) %>% distinct(editor_id,inst,.keep_all=TRUE)



# editors with >1 inst : 2x

OIKOS_inst_NA<-OIKOS_checks %>%
  filter(is.na(inst))  %>%
  group_by(editor_id) %>%
  distinct(editor_id,city,.keep_all=TRUE)
# 
# OIKOS_ed_checks<-OIKOS_checks %>%  distinct(editor_id,inst) %>% group_by(editor_id) %>% filter(n()>1) 
# 
# # Inst with >1 editors : 2x
# OIKOS_inst_checks<-OIKOS_checks %>%  distinct(editor_id,inst) %>% group_by(inst) %>% filter(n()>1) 
# 
# OIKOS_checks<-bind_rows(OIKOS_inst_checks,OIKOS_ed_checks) %>% distinct(editor_id,inst)
# 
# OIKOS_checks<-inner_join(OIKOS,OIKOS_checks) %>% distinct(editor_id,inst,.keep_all = TRUE)
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
# OIKOS_spotchecks<-unique(bind_rows(sub1,sub2,sub3)) %>% arrange(year,last_name)
# 
# OIKOS_spotchecks$check<-"spotcheck"
# 
# OIKOS_checks<-bind_rows(OIKOS_spotchecks,OIKOS_checks) %>%arrange(year,last_name)
# OIKOS_checks<-OIKOS_checks %>% distinct(editor_id, inst,.keep_all = TRUE)

write.csv(OIKOS_inst_NA, file="./Data/Patrick_James_Data_Corrections/OIKOS_checks.csv", row.names = T) #export it as a csv file

rm(OIKOS_checks,sub1,sub2,sub3,OIKOS_ed_checks,OIKOS_inst_checks)



#######################
# JANE
#######################

JANE_checks<-alldata %>% filter(journal=="JANE")
JANE_checks<-JANE_checks %>% arrange(editor_id,year,inst)
head(JANE_checks,10)
# JANE_checks<-JANE_checks %>% group_by(editor_id,inst) %>% distinct(editor_id,inst,.keep_all=TRUE)



# editors with >1 inst : 2x
JANE_checks$inst[is.na(JANE_checks$inst)]<-"missing"
JANE_checks$city[is.na(JANE_checks$city)]<-"missing"
JANE_checks$state[is.na(JANE_checks$state)]<-"missing"
JANE_checks$unit[is.na(JANE_checks$unit)]<-"missing"
JANE_inst_NA<-JANE_checks %>%
  filter(is.na(inst)| inst=="missing")  %>%
  group_by(editor_id) %>% 
  distinct(editor_id,city,.keep_all=TRUE)
# 
# JANE_ed_checks<-JANE_checks %>%  distinct(editor_id,inst) %>% group_by(editor_id) %>% filter(n()>1) 
# 
# # Inst with >1 editors : 2x
# JANE_inst_checks<-JANE_checks %>%  distinct(editor_id,inst) %>% group_by(inst) %>% filter(n()>1) 
# 
# JANE_checks<-bind_rows(JANE_inst_checks,JANE_ed_checks) %>% distinct(editor_id,inst)
# 
# JANE_checks<-inner_join(JANE,JANE_checks) %>% distinct(editor_id,inst,.keep_all = TRUE)
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
# JANE_spotchecks<-unique(bind_rows(sub1,sub2,sub3)) %>% arrange(year,last_name)
# 
# JANE_spotchecks$check<-"spotcheck"
# 
# JANE_checks<-bind_rows(JANE_spotchecks,JANE_checks) %>%arrange(year,last_name)
# JANE_checks<-JANE_checks %>% distinct(editor_id, inst,.keep_all = TRUE)

write.csv(JANE_inst_NA, file="./Data/Patrick_James_Data_Corrections/JANE_checks.csv", row.names = T) #export it as a csv file

rm(JANE_checks,sub1,sub2,sub3,JANE_ed_checks,JANE_inst_checks)


#######################
# LECO
#######################
LECO_checks<-alldata %>% filter(journal=="LECO")
LECO_checks<-LECO %>% arrange(editor_id,year,inst)
head(LECO_checks,10)
LECO_checks<-LECO_checks %>% group_by(editor_id,inst) %>% distinct(editor_id,inst)

# editors with >1 inst : 2x
LECO_ed_checks<-LECO_checks %>%  distinct(editor_id,inst) %>% group_by(editor_id) %>% filter(n()>1) 

# Inst with >1 editors : 2x
LECO_inst_checks<-LECO_checks %>%  distinct(editor_id,inst) %>% group_by(inst) %>% filter(n()>1) 

LECO_checks<-bind_rows(LECO_inst_checks,LECO_ed_checks) %>% distinct(editor_id,inst)

LECO_checks<-inner_join(LECO,LECO_checks) %>% distinct(editor_id,inst,.keep_all = TRUE)

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
LECO_spotchecks<-unique(bind_rows(sub1,sub2,sub3)) %>% arrange(year,last_name)

LECO_spotchecks$check<-"spotcheck"

LECO_checks<-bind_rows(LECO_spotchecks,LECO_checks) %>%arrange(year,last_name)
LECO_checks<-LECO_checks %>% distinct(editor_id, inst,.keep_all = TRUE)

# write.csv(LECO_checks, file="./Data/Patrick_James_Data_Corrections/leco_checks.csv", row.names = T) #export it as a csv file

rm(LECO_checks,sub1,sub2,sub3,LECO_ed_checks,LECO_inst_checks)

#######################
# CONBIO
#######################

CONBIO_checks<-alldata %>% filter(journal=="CONBIO")

CONBIO_checks<-CONBIO %>% arrange(editor_id,year,inst)
head(CONBIO_checks,10)
CONBIO_checks<-CONBIO_checks %>% group_by(editor_id,inst) %>% distinct(editor_id,inst)

# editors with >1 inst : 2x
CONBIO_ed_checks<-CONBIO_checks %>%
  distinct(editor_id,inst) %>%
  group_by(editor_id) %>%
  filter(n()>1)

# Inst with >1 editors : 2x
CONBIO_inst_checks<-CONBIO_checks %>%
  distinct(editor_id,inst) %>%
  group_by(inst) %>%
  filter(n()>1)

CONBIO_checks<-bind_rows(CONBIO_inst_checks,CONBIO_ed_checks) %>% distinct(editor_id,inst)

CONBIO_checks<-inner_join(CONBIO,CONBIO_checks) %>% distinct(editor_id,inst,.keep_all = TRUE)

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
CONBIO_spotchecks<-unique(bind_rows(sub1,sub2,sub3)) %>% arrange(year,last_name)

CONBIO_spotchecks$check<-"spotcheck"

CONBIO_spotchecks<-CONBIO_spotchecks[sample(nrow(CONBIO_spotchecks), 50), ]
CONBIO_checks<-bind_rows(CONBIO_spotchecks,CONBIO_checks) %>%arrange(year,last_name)
CONBIO_checks<-CONBIO_checks %>% distinct(editor_id, inst,.keep_all = TRUE)

# write.csv(CONBIO_checks, file="./Data/Patrick_James_Data_Corrections/CONBIO_checks.csv", row.names = T) #export it as a csv file

rm(CONBIO_checks,sub1,sub2,sub3,CONBIO_ed_checks,CONBIO_inst_checks)

#######################
# AGrONOMY
#######################
AG_check<-alldata %>% filter(journal=="AGRONOMY")
# AG_inst<-AG %>% group_by(last_name,first_name,MIDDLE_name,year,inst) %>% summarise(n()) %>% arrange(last_name,first_name,year)
# write.csv(AG_inst, file="AG_missing_inst.csv", row.names = T) #export it as a csv file

#######################
# JBIOG
#######################
JBIOG_check<-alldata %>% filter(journal=="JBIOG")
JBIOG_checks<-JBIOG %>% arrange(editor_id,year,inst)
head(JBIOG_checks,10)
JBIOG_checks<-JBIOG_checks %>% group_by(editor_id,inst) %>% distinct(editor_id,inst)

# editors with >1 inst : 2x
JBIOG_ed_checks<-JBIOG_checks %>%  distinct(editor_id,inst) %>% group_by(editor_id) %>% filter(n()>1) 

# Inst with >1 editors : 2x
JBIOG_inst_checks<-JBIOG_checks %>%  distinct(editor_id,inst) %>% group_by(inst) %>% filter(n()>1) 

JBIOG_checks<-bind_rows(JBIOG_inst_checks,JBIOG_ed_checks) %>% distinct(editor_id,inst)

JBIOG_checks<-inner_join(JBIOG,JBIOG_checks) %>% distinct(editor_id,inst,.keep_all = TRUE)

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

JBIOG_spotchecks<-unique(bind_rows(sub1,sub2,sub3)) %>% arrange(year,last_name)

JBIOG_spotchecks$check<-"spotcheck"

JBIOG_checks<-bind_rows(JBIOG_spotchecks,JBIOG_checks) %>%arrange(year,last_name)
JBIOG_checks<-JBIOG_checks %>% distinct(editor_id, inst,.keep_all = TRUE)

write.csv(JBIOG_checks, file="./Data/Patrick_James_Data_Corrections/JBIOG_checks.csv", row.names = T) #export it as a csv file

rm(JBIOG_checks,sub1,sub2,sub3,JBIOG_ed_checks,JBIOG_inst_checks,JBIOG_1row,JBIOG_remainder)



#######################
# NEWPHYT
#######################
NEWPHYT_check<-alldata %>% filter(journal=="NEWPHYT")
# NEWPHYT (MAIRA): 
# 1) NEEDS inst
NEWPHYT$inst<-as.factor(NEWPHYT$inst)
NEWPHYT_inst<-NEWPHYT %>% group_by(last_name,first_name,MIDDLE_name,year,inst) %>% summarise(n()) %>% arrange(last_name,first_name,year)
# write.csv(NEWPHYT_inst, file="NEWPHYT_missing_inst.csv", row.names = T) #export it as a csv file


#######################
# NAJFM
#######################

NAJFM_check<-alldata %>% filter(journal=="NAJFM")
# NAJFM (MAIRA): 
# 1) NEEDS inst
NAJFM$inst<-as.factor(NAJFM$inst)
NAJFM_inst<-NAJFM %>%
  group_by(last_name,first_name,MIDDLE_name,year,inst) %>% summarise(n()) %>% arrange(last_name,first_name,year)
write.csv(NAJFM_inst, file="NAJFM_missing_inst.csv", row.names = T) #export it as a csv file

#######################
# GCB
#######################
GCB_check<-alldata %>% filter(journal=="GCB")
#TO FILL OUT MISSING inst 
GCB$inst<-as.factor(GCB$inst)
GCB_inst<-GCB %>% group_by(name,year,inst) %>% summarise(n()) %>% arrange(name,year)
write.csv(GCB_inst, file="GCB_missing_inst.csv", row.names = T) #export it as a csv file


#######################
# PLANTECOL
#######################
PLANTECOL_checks<-alldata %>% filter(journal=="PLANTECOL") %>% arrange(editor_id,year,inst)
head(PLANTECOL_checks,10)
PLANTECOL_checks<-PLANTECOL_checks %>% group_by(editor_id,inst) %>% distinct(editor_id,inst)

# editors with >1 inst : 2x
PLANTECOL_ed_checks<-PLANTECOL_checks %>%  distinct(editor_id,inst) %>% group_by(editor_id) %>% filter(n()>1) 

# Inst with >1 editors : 2x
PLANTECOL_inst_checks<-PLANTECOL_checks %>%  distinct(editor_id,inst) %>% group_by(inst) %>% filter(n()>1) 

PLANTECOL_checks<-bind_rows(PLANTECOL_inst_checks,PLANTECOL_ed_checks) %>% distinct(editor_id,inst)

PLANTECOL_checks<-inner_join(PLANTECOL,PLANTECOL_checks) %>% distinct(editor_id,inst,.keep_all = TRUE)

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

plantecol_spotchecks<-unique(bind_rows(sub1,sub2,sub3)) %>% arrange(year,last_name)

plantecol_spotchecks$check<-"spotcheck"

PLANTECOL_checks<-bind_rows(plantecol_spotchecks,PLANTECOL_checks) %>%arrange(year,last_name)
PLANTECOL_checks<-PLANTECOL_checks %>% distinct(editor_id, inst,.keep_all = TRUE)

write.csv(PLANTECOL_checks, file="./Data/Patrick_James_Data_Corrections/plantecol_checks.csv", row.names = T) #export it as a csv file


rm(PLANTECOL_checks,sub1,sub2,sub3,PLANTECOL_ed_checks,PLANTECOL_inst_checks) 




##############

#######################
# AGrONOMY
#######################
# all with NA
# OECOL_check<-alldata %>% filter(journal=="OECOL") %>% arrange(last_name,year) %>% filter(is.na(inst)) %>% distinct(last_name,first_name,.keep_all = TRUE)
OECOL_check<-alldata %>% filter(journal=="OECOL") %>% arrange(last_name,year)
OECOL_check$inst[is.na(OECOL_check$inst)]<-"missing"
OECOL_check$unit[is.na(OECOL_check$unit)]<-"missing"
OECOL_check$city[is.na(OECOL_check$city)]<-"missing"
OECOL_check$state[is.na(OECOL_check$state)]<-"missing"
OECOL_check<-OECOL_check %>% filter(inst=="missing" |inst== "DOUBLE CHECK inst") %>% arrange(last_name,year) %>% distinct(last_name,first_name,.keep_all = TRUE)
write.csv(OECOL_check, file="./Data/Patrick_James_Data_Corrections/OECOL_checks.csv", row.names = T) #export it as a csv file
# 
# OECOL_check2<-filter(alldata,journal=="OECOL") %>% filter(is.na(inst)==FALSE) %>% distinct(last_name,first_name,.keep_all = TRUE)
# 
# OECOL_check3<-left_join(OECOL_check,OECOL_check2,by="editor_id.x") %>% select(inst.x,inst.y)
# # AG_inst<-AG %>% group_by(last_name,first_name,MIDDLE_name,year,inst) %>% summarise(n()) %>% arrange(last_name,first_name,year)
# # write.csv(AG_inst, file="AG_missing_inst.csv", row.names = T) #export it as a csv file
# OECOL_inst<-OECOL_check %>% distinct(last_name,first_name,inst) %>% group_by(last_name,first_name) %>% summarize(n_distinct(inst))

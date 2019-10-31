# This generates the list of names and institutions that need to be double checked.

library(tidyverse)
#######################
# JZOOL
#######################


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
# LECO
#######################

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

# AG_inst<-AG %>% group_by(LAST_NAME,FIRST_NAME,MIDDLE_NAME,YEAR,INST) %>% summarise(n()) %>% arrange(LAST_NAME,FIRST_NAME,YEAR)
# write.csv(AG_inst, file="AG_missing_inst.csv", row.names = T) #export it as a csv file

#######################
# JBIOG
#######################
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

# NEWPHYT (MAIRA): 
# 1) NEEDS INST
NEWPHYT$INST<-as.factor(NEWPHYT$INST)
NEWPHYT_inst<-NEWPHYT %>% group_by(LAST_NAME,FIRST_NAME,MIDDLE_NAME,YEAR,INST) %>% summarise(n()) %>% arrange(LAST_NAME,FIRST_NAME,YEAR)
# write.csv(NEWPHYT_inst, file="NEWPHYT_missing_inst.csv", row.names = T) #export it as a csv file


#######################
# NAJFM
#######################


# NAJFM (MAIRA): 
# 1) NEEDS INST
NAJFM$INST<-as.factor(NAJFM$INST)
NAJFM_inst<-NAJFM %>% group_by(LAST_NAME,FIRST_NAME,MIDDLE_NAME,YEAR,INST) %>% summarise(n()) %>% arrange(LAST_NAME,FIRST_NAME,YEAR)
write.csv(NAJFM_inst, file="NAJFM_missing_inst.csv", row.names = T) #export it as a csv file

#######################
# GCB
#######################

#TO FILL OUT MISSING INST 
GCB$INST<-as.factor(GCB$INST)
GCB_inst<-GCB %>% group_by(NAME,YEAR,INST) %>% summarise(n()) %>% arrange(NAME,YEAR)
write.csv(GCB_inst, file="GCB_missing_inst.csv", row.names = T) #export it as a csv file


#######################
# PLANTECOL
#######################
PLANTECOL_checks<-PLANTECOL %>% arrange(editor_id,YEAR,INST)
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


library(tidyverse)
JECOL_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_6_JEcol.csv", col_names = TRUE)
JECOL_inst<-JECOL_inst %>% fill(INST,UNIT,CITY,STATE)%>% filter(JOURNAL=="JECOL")

AMNAT_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_4_AMNAT.csv", col_names = TRUE)
AMNAT_inst<-AMNAT_inst %>% fill(INST,UNIT,CITY,STATE)%>% filter(JOURNAL=="AMNAT")


BITR_inst<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_3_BITR.csv", col_names = TRUE)
BITR_inst<-BITR_inst %>% fill(INST,UNIT,CITY,STATE)

multi1<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_1.csv", col_names = TRUE)
multi1<-multi1 %>% fill(INST,UNIT,CITY,STATE)

multi2a<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_2a.csv", col_names = TRUE)
multi2a<-filter(multi2a,JOURNAL=="CONBIO"|JOURNAL=="NEWPHYT")#delete out other journals this is conbio and new phyt


multi2b<-read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_2b.csv", col_names = TRUE)
multi2b<-multi2b %>% fill(INST,UNIT,CITY,STATE) %>% filter(JOURNAL=="CONBIO"|JOURNAL=="NEWPHYT")
#delete out other journals this is conbio and new phyt


INST_fix<-bind_rows(multi2b,multi2a,multi1,BITR_inst,JECOL_inst,AMNAT_inst) %>% 
  distinct(editor_id,JOURNAL,YEAR,.keep_all= TRUE) %>%     #there are some duplicates, best to remove them
  arrange(JOURNAL,editor_id,YEAR)
INST_fix$INST<-gsub("UNKNOWN",NA,INST_fix$INST)  #Replace "unknwon with NA 
INST_fix$UNIT<-gsub("UNKNOWN",NA,INST_fix$UNIT)
INST_fix$CITY<-gsub("UNKNOWN",NA,INST_fix$CITY)
INST_fix$STATE<-gsub("UNKNOWN",NA,INST_fix$STATE)




write.csv(INST_fix, file="./Data/INST_fix.csv", row.names = T) #export it as a csv file

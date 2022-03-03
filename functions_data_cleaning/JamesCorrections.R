# This will load and the corrected files and make the required changes.
JamesCorrections <- function(original_data) {
  # original_data<-alldata
  # colnames(alldata)
  library(tidyverse)

  # source("functions_data_cleaning/institution_cleaner.R")
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
  # Round 3: BITR (DONE)
  # Round 4: AMNAT (DONE, EB 2x)
  # Round 6: JECOL (DONE)
  # Round 7: JAPE (DONE)
  # Round 5: BIOCON (DONE, need to upload and integrate the corrections)
  # Round 8: PLANTECOL (DONE, to be emailed)
  # Round 9: JBIOG (2x in progress, almost done - need to add institutions that were "missing")
  # Round 10: LECO (DONE, 2x the ones "incorrect" to see if the inst value in the table is the corrected or original")
  # JANE: Needs extensive data fill
  # JZOOL: Needs extensive data fill
  # OECOL: Needs extensive data fill
  # OIKOS: (DONE)
  # AUK: not for this paper
  # CONDOR: not for this paper
  ##############################################################

  ##########
  multi1 <- read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_1.csv", col_names = TRUE)
  multi1[multi1 == "missing"] <- NA
  multi1 <- multi1 %>%
    group_by(last_name, first_name) %>%
    arrange(year) %>%
    fill(inst, unit, city, state, .direction = "down")
  ##########

  ##########
  multi2a <- read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_2a.csv", col_names = TRUE)
  multi2a[multi2a == "missing"] <- NA
  multi2a <- multi2a %>%
    filter(journal == "CONBIO" | journal == "NEWPHYT") %>%
    group_by(last_name, first_name) %>%
    arrange(year) %>%
    fill(inst, unit, city, state, .direction = "down") # delete out other journals this is conbio and new phyt
  ##########

  ##########
  multi2b <- read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_2b.csv", col_names = TRUE)
  multi2b[multi2b == "missing"] <- NA
  multi2b <- multi2b %>%
    filter(journal == "CONBIO" | journal == "NEWPHYT") %>%
    group_by(last_name, first_name) %>%
    arrange(year) %>%
    fill(inst, unit, city, state, .direction = "down") # delete out other journals this is conbio and new phyt
  ##########

  ##########
  BITR_inst <- read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_3_BITR.csv", col_names = TRUE)
  BITR_inst[BITR_inst == "missing"] <- NA
  BITR_inst <- BITR_inst %>%
    group_by(last_name, first_name) %>%
    arrange(year) %>%
    fill(inst, unit, city, state, .direction = "down")

  ##########

  ##########
  AMNAT_inst <- read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_4_AMNAT.csv", col_names = TRUE, na = c("", "N/A", "NA"), trim_ws = TRUE)
  AMNAT_inst[AMNAT_inst == "missing"] <- NA
  AMNAT_inst <- AMNAT_inst %>%
    group_by(last_name, first_name) %>%
    arrange(year) %>%
    fill(inst, unit, city, state, .direction = "down") %>%
    filter(journal == "AMNAT")
  ##########

  ##########
  JECOL_inst <- read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_6_JEcol.csv", col_names = TRUE)
  JECOL_inst[JECOL_inst == "missing"] <- NA
  JECOL_inst <- JECOL_inst %>%
    group_by(last_name, first_name) %>%
    arrange(year) %>%
    fill(inst, unit, city, state, .direction = "down") %>%
    filter(journal == "JECOL")
  ##########

  ##########
  JAPE_inst <- read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_7_JAPE.csv", col_names = TRUE)
  JAPE_inst[JAPE_inst == "missing"] <- NA
  JAPE_inst[JAPE_inst == "unknown"] <- NA
  JAPE_inst <- JAPE_inst %>%
    rename("first_name" = "first_na", "middle_name" = "middle_", "last_name" = "last_na") %>%
    group_by(last_name, first_name) %>%
    arrange(year) %>%
    fill(inst, unit, city, state, country, notes, .direction = "down") %>%
    filter(journal == "JAPE")
  JAPE_inst$editor_id <- NULL
  # TODO: something is going on with editor IDS
  ##########

  ##########
  # SEPARATE FUNCTION
  # JBIOG
  # LECO
  # PLANTECOL
  # OIKOS
  # OECOLOGIA
  # JANE
  ###########


  #############
  # inst_fix<-bind_rows(multi2b,multi2a,multi1,BITR_inst,JECOL_inst,AMNAT_inst,JAPE_inst,JBIOG_inst,OIKOS_inst) %>%
  #   distinct(editor_id,journal,year,.keep_all= TRUE) %>%     #there are some duplicates, best to remove them
  #   arrange(journal,editor_id,year)
  # rm(multi2b,multi2a,multi1,BITR_inst,JECOL_inst,AMNAT_inst,JAPE_inst,JBIOG_inst,OIKOS_inst)

  # NO OIKOS
  inst_fix <- bind_rows(multi2b, multi2a, multi1, BITR_inst, JECOL_inst, AMNAT_inst, JAPE_inst) %>%
    distinct(last_name, journal, year, .keep_all = TRUE) %>% # there are some duplicates, best to remove them
    arrange(journal, editor_id, year)
  rm(multi2b, multi2a, multi1, BITR_inst, JECOL_inst, AMNAT_inst, JAPE_inst)

  
  inst_fix<-inst_fix %>%
    mutate(across(everything(), as.character))
  
  
  
  # colnames(inst_fix)
  # colnames(OECOL_inst)

  inst_fix$inst[inst_fix$inst == "UNKNOWN"] <- NA
  inst_fix$inst[inst_fix$inst == "unknown"] <- NA
  inst_fix$unit[inst_fix$unit == "UNKNOWN"] <- NA
  inst_fix$city[inst_fix$city == "UNKNOWN"] <- NA
  inst_fix$state[inst_fix$state == "UNKNOWN"] <- NA
  inst_fix$state[inst_fix$state == "unknown"] <- NA


  # inst_fix<-institution_cleaner(inst_fix)
  inst_fix <- inst_fix %>% select(-"...1")

  head(inst_fix, 20)
  colnames(inst_fix)



  inst_fix$inst <- tolower(inst_fix$inst)
  inst_fix$unit <- tolower(inst_fix$unit)
  inst_fix$city <- tolower(inst_fix$city)
  inst_fix$state <- tolower(inst_fix$state)
  inst_fix$country <- tolower(inst_fix$country)
  inst_fix$notes <- tolower(inst_fix$notes)
  inst_fix$journal <- tolower(inst_fix$journal)
  inst_fix$first_name <- tolower(inst_fix$first_name)
  inst_fix$middle_name <- tolower(inst_fix$middle_name)
  inst_fix$last_name <- tolower(inst_fix$last_name)

  #
  #
  # # original_data$editor_id<-as.numeric(original_data$editor_id)
  # # ARE THERE ANY IN CORRECT that *ARENT* in original_data?
  # # THESE NEED TO BE ADDED TO original_data
  # # C_but_not_O<-anti_join(inst_fix,original_data,by=c("editor_id","journal","year")) %>% arrange(journal,year,editor_id)  #in correct but not orig 24
  # C_but_not_O<-anti_join(inst_fix,original_data,by=c("journal","year","last_name")) %>% arrange(journal,year,editor_id)  #in correct but not orig 24
  # nrow(C_but_not_O)
  # summary(C_but_not_O)
  #
  # #THESE ARE THE ONES IN original_data but not CORRECTED
  # # O_butnot_C<-anti_join(original_data,inst_fix,by=c("editor_id","journal","year")) # in orig but not correct 21221
  # O_butnot_C<-anti_join(original_data,inst_fix,by=c("journal","year","last_name")) # in orig but not correct 21221
  # nrow(O_butnot_C)
  # O_butnot_C
  #
  # # THESE ARE THE ONES IN BOTH CORRECTED AND original_data
  # # O_and_C<-inner_join(original_data,inst_fix,by=c("editor_id","journal","year")) #in correct but not orig 4381
  # O_and_C<-inner_join(original_data,inst_fix,by=c("last_name","journal","year")) #in correct but not orig 4381
  # nrow(O_and_C)
  #
  # colnames(original_data)
  # colnames(inst_fix)
  # str(original_data)
  # str(inst_fix)



  # both<-full_join(original_data,inst_fix,by=c("editor_id","journal","year"))

  inst_fix$editor_id <- as.character(inst_fix$editor_id)
  str(inst_fix$editor_id)
  str(original_data$editor_id)
  original_data$editor_id <- as.factor(original_data$editor_id)
  inst_fix$editor_id <- as.factor(inst_fix$editor_id)

  both <- full_join(original_data, inst_fix, by = c("journal", "year", "last_name", "first_name"))
  nrow(both)
  nrow(original_data)
  nrow(inst_fix)
  colnames(both)
  str(both)

  # nrow(C_but_not_O)
  # nrow(O_butnot_C)
  # nrow(O_and_C)
  # nrow(C_but_not_O)+nrow(O_butnot_C)+nrow(O_and_C)

  head(both)
  str(both)

  colnames(both)
  both <- select(
    both, journal, year, volume, issue, editor_id.x, editor_id.y,
    first_name, middle_name.x, middle_name.y, last_name,
    title, category, inst.x, inst.y, unit.x, unit.y, city.x, city.y, state.x, state.y,
    country.x, country.y, country_prior_class, geo.code, geo.code_prior_class,
    notes.x, notes.y, gender
  )

  colnames(both)
  both[both == "missing"] <- NA
  both[both == "unknown"] <- NA


  # both<-both %>% mutate(volume.x = replace(volume.x, is.na(volume.x),volume.y[is.na(volume.x)]))
  # both$volume.y<-NULL
  # both<-both %>% rename("volume"="volume.x")


  #
  # both<-both %>% mutate(issue.x = replace(issue.x, is.na(issue.x),issue.y[is.na(issue.x)]))
  # both$issue.y<-NULL
  # both<-both %>% rename("issue"="issue.x")

  #
  # both<-both %>% mutate(title.x = replace(title.x, is.na(title.x),title.y[is.na(title.x)]))
  # both$title.y<-NULL
  # both<-both %>% rename("title"="title.x")
  #
 


  # #CAN QUICKLY ID WHAT NEEDS TO BE FIXED AS FOLLOWS
  # str(both)
  # # FIRST NAME DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
  # summary(both$first_name.x==both$first_name.y) # 10 false
  # both$FIRST_check<-both$first_name.x==both$first_name.y
  # #spelling mistake in NAMES.y, so delete that column and the checm column
  # both$FIRST_check<-NULL
  # both$first_name.y<-NULL
  # both<-both %>% rename("first_name"="first_name.x")

  # MIDDLE NAME DIFFERENCES BETWEEN ALL DATA AND CHECKED FILE
  summary(both$middle_name.x == both$middle_name.y) # NO FALSE
  both$middle_check <- both$middle_name.x == both$middle_name.y
  both$middle_name.y <- NULL
  both$middle_check <- NULL
  both <- both %>% rename("middle_name" = "middle_name.x")

  # LAST NAME IFFERENCES BETWEEN ALL DATA AND CHECKED FILE
  # summary(both$last_name.x==both$last_name.y) # 21 FALSE
  # both$LAST_check<-both$last_name.x==both$last_name.y
  # # This identifies one mistake in thge original (original_data) that needs to be corrected
  # str(both)
  both$last_name[both$editor_id.x == 1355 & both$first_name == "holmes"] <- "rolston"
  both <- both[!(is.na(both$editor_id.x) & both$first_name == "holmes"), ]


  # both$last_name.y<-NULL
  # both$LAST_check<-NULL
  # both<-both %>% rename("last_name"="last_name.x")


  # state differences between all data and checked file
  summary(both$state.x == both$state.y) # 19 false
  both$state_check <- both$state.x == both$state.y
  both$state.x[both$state.x == "missing"] <- NA
  both$state.x[both$year != 1992 & both$first_name == "david" & both$last_name == "gibson" & both$city.x == "carbondale"] <- "il"
  both$state.x[both$year == 1992 & both$city.x == "pensacola" & both$last_name == "gibson"] <- "fl"
  both$state.x[both$state.x == "england"] <- NA
  both$state.x[both$last_name == "moss"] <- NA
  both$state.x[both$last_name == "usher"] <- NA
  both$state.x[both$last_name == "milner-gulland"] <- NA
  both$inst.y[both$last_name == "belovsky" & both$inst.y == "notre dame"] <- "university of notre dame"
  both$city.x[both$last_name == "belovsky" & both$inst.y == "notre dame"] <- "notre dame"
  both$country.x[both$last_name == "belovsky" & both$inst.y == "university of notre dame"] <- "usa"
  both$state.x[both$last_name == "belovsky" & both$inst.y == "notre dame university"] <- "in"
  both$state.x[both$first_name == "james" & both$last_name == "carlton"] <- "ct"
  both$state.x[both$first_name == "jon" & both$last_name == "rodriguez"] <- NA
  both$state.x[both$editor_id.x == 455 & both$first_name == "christopher" & both$last_name == "frissell"] <- "mt"
  both$inst.y[both$editor_id.x == 455 & both$first_name == "christopher" & both$last_name == "frissell"] <- "university of montana"
  both$unit.x[both$editor_id.x == 455 & both$first_name == "christopher" & both$last_name == "frissell"] <- "flathead lake biological station"
  both$city.x[both$editor_id.x == 455 & both$first_name == "christopher" & both$last_name == "frissell"] <- "polson"
  both$city.x[both$editor_id.x == 1511 & both$last_name == "cinner" & both$year > 2009 & both$year < 2015] <- "townsville"
  both$state.x[both$editor_id.x == 1511 & both$last_name == "cinner" & both$year > 2009 & both$year < 2015] <- "queensland"
  both$unit.x[both$editor_id.x == 1511 & both$last_name == "cinner"] <- "arc centre of excellence for coral reef studies"
  both$inst.y[both$editor_id.x == 1511 & both$last_name == "cinner"] <- "james cook university"
  both$notes.y[both$editor_id.x == 1511 & both$last_name == "cinner" & both$year == 2013] <- "journal front matter has inst=columbia univ, but his cv makes no mention of this"

  # this will replace all the "na" in state.x (origianlly no info) with the value from state.y (patrick's data collection), if there is one
  both <- both %>% mutate(state.x = replace(state.x, is.na(state.x), state.y[is.na(state.x)]))


  both$state.y <- NULL
  both$state_check <- NULL
  both <- both %>% rename("state" = "state.x")

  # this identifies one mistake in thge original (original_data) that needs to be corrected
  both$unit.x[both$unit.x == "estaci<f3>n biol<f3>gica de do<f1>ana"] <- "estacion biologica donana"
  both$unit.y[both$unit.y == "estaci<f3>n biol<f3>gica de do<f1>ana"] <- "estacion biologica donana"
  both$unit.x <- gsub("estaci\xf3n biol\xf3gica de do\xf1ana", "estacion biologica donana", both$unit.x)
  both$unit.x <- gsub("biologie/chemie/\x80kologie", "biochemical ecology", both$unit.x)
  both$unit.x <- gsub("f\x99r", "fur", both$unit.x)
  both$unit.x <- gsub("ecolog\x90a", "ecologia", both$unit.x)

  # inst differences between all data and checked file
  both$inst.y <- as.character(both$inst.y)
  both$inst.y <- tolower(both$inst.y)
  both$inst.y <- tolower(both$inst.y)
  both$unit.x <- tolower(both$unit.x)
  both$unit.y <- tolower(both$unit.y)
# both[7358,14]

  # this will replace all the "na" and "" in inst.x (origianlly no info) with the value from inst.y (patrick's data collection), if there is one
  both <- both %>% mutate(inst.x = replace(inst.x, is.na(inst.x), inst.y[is.na(inst.x)]))
  both <- both %>% mutate(inst.x = replace(inst.x, inst.x == "", NA))
  summary(both$inst.y == both$inst.y) # 235 false
  both$inst_check <- both$inst.y == both$inst.y

  inst_check <- filter(both, inst_check == "FALSE")
  inst_check_ok <- filter(both, inst_check == TRUE | is.na(inst_check))
  #
  # write.csv(inst_check, file="./data/patrick_james_data_corrections/complete/inst_corrections_2x.csv", row.names = f) #export it as a csv file


  both$inst.y[both$editor_id.x == 1248 & both$journal == "bitr" & both$year == 1987] <- "new york botanical garden"
  both$inst.y[both$editor_id.x == 1704 & both$journal == "jecol" & both$year == 2009] <- "university of sheffield"
  # both$inst.y[both$inst.y=="unniversity of stirling" ]<-"university of stirling"

  both$unit.x[both$editor_id.x == 1229 & both$inst.y == "university of montana"] <- "savannah river ecology laboratory"
  both$inst.y[both$editor_id.x == 1229 & both$inst.y == "university of montana"] <- "university of georgia"

  both$unit.x[both$inst.y == "alterra research institute for the green world"] <- "alterra research institute for the green world"
  both$unit.x[both$inst.y == "gatty marine lab (university of saint andrews)"] <- "gatty marine lab"
  both$unit.x[both$inst.y == "netherlands institute of ecology; wageningen university and research centre netherlands institute of ecology"] <- "netherlands institute of ecology nioo knaw"
  both$unit.x[both$inst.y == "norwich"] <- "norwich research park industrial biotechnology and bioenergy alliance"
  both$unit.x[both$inst.y == "scripps institute of oceanography"] <- "scripps institution of oceanography"
  both$unit.x[both$inst.y == "scripps institution  of oceanography"] <- "scripps institution of oceanography"
  both$unit.x[both$inst.y == "scripps institute of oceanography"] <- "gatty marine lab"



  both$unit.x[both$inst.y == "savannah river ecology laboratory"] <- "savannah river ecology laboratory"
  both$inst.y[both$inst.y == "savannah river ecology laboratory"] <- "university of georgia"
  both$unit.x[both$inst.y == "university of california santa cruz extension"] <- "ucsc extension"
  both$inst.y[both$inst.y == "university of california" & both$inst.y == "university of california davis"] <- "university of california davis"
  both$inst.y[both$inst.y == "university of california" & both$inst.y == "university of california berkeley"] <- "university of california berkeley"
  both$inst.y[both$inst.y == "university of california" & both$inst.y == "university of california riverside"] <- "university of california riverside"
  # both$inst.y[both$editor_id.x==1839 & both$journal=="conbio" ]<-"venezuelan institute for scientific investigation"
  both$inst.y[both$editor_id.x == 1218 & both$journal == "conbio" & both$year > 1986 & both$year < 1992] <- "royal botanic gardens kew"
  # both$inst.y[both$inst.y=="museum natl hist nat"]<-"national history museum paris"
  # both$inst.y[both$inst.y=="university<ca>of<ca>california<ca>santa<ca>cruz"]<-"university of california santa cruz"
  # both$inst.y[both$inst.y=="uc santa cruz"]<-"university of california santa cruz"

  both$inst.y[both$last_name == "streeter" & both$journal == "jecol" & both$year == 2009] <- "university of sussex"
  both$inst.y[both$last_name == "grover" & both$journal == "amnat"] <- "university of texas arlington"
  both$inst.y[both$last_name == "noss" & both$journal == "conbio" & both$year == 1998] <- "conservation biology institute"

  both$unit.x[both$last_name == "dratch" & both$journal == "conbio" & both$inst.y == "national fish and wildlife forensics laboratory"] <- "national fish and wildlife forensics laboratory"
  both$inst.y[both$last_name == "dratch" & both$journal == "conbio" & both$inst.y == "national fish and wildlife forensics laboratory"] <- "us fish and wildlife service"
  both$inst.y[both$last_name == "daszak" & both$journal == "conbio" & both$inst.y == "university of nevada reno"] <- "consortium for conservation medicine"

  both$unit.x[both$last_name == "meffe" & both$journal == "conbio" & both$inst.y == "university of montana"] <- "savanna riverl ecological laboratory"
  both$unit.x[both$last_name == "meffe" & both$journal == "conbio" & both$unit.x == "savanna riverl ecological laboratory"] <- "university of georgia"
  both$unit.x[both$journal == "leco" & both$inst.y == "institute of landscape ecology of slovak academy of sciences"] <- "institute of landscape ecology"
  both$inst.y[both$journal == "leco" & both$inst.y == "institute of landscape ecology of slovak academy of sciences"] <- "slovak academy of sciences"

  summary(both$inst.y == both$inst.y)
  both$inst_check <- both$inst.y == both$inst.y
  both$inst.y <- NULL
  both$inst_check <- NULL
  both <- both %>% rename("inst" = "inst.x")

  # city differences between all data and checked file
  summary(both$city.x == both$city.y) # 52 false
  both$city_check <- both$city.x == both$city.y
  city_check <- filter(both, city_check == "false")
  city_check_ok <- filter(both, city_check == TRUE | is.na(city_check))
  write.csv(city_check, file = "./data/patrick_james_data_corrections/complete/city_corrections_2x.csv", row.names = FALSE) # export it as a csv file
  both$state[both$city.x == "new mexico" & both$city.y == "las cruces"] <- "nm"
  both$city.x[both$city.x == "new mexico" & both$city.y == "las cruces"] <- "las cruces"
  both$city.x[is.na(both$city.x) & both$city.y == "las cruces"] <- "las cruces"
  both$notes.y[both$city.x == "basel" & both$city.y == "lausanne"] <- "2x city"
  both$notes.y[both$city.x == "brighton" & both$city.y == "toronto"] <- "2x city"
  both$notes.y[both$city.x == "zurich" & both$city.y == "basel"] <- "2x city"
  both$notes.y[both$city.x == "canberra" & both$city.y == "lyneham"] <- "2x city"

  both$city.x[both$city.x == "stanford" & both$city.y == "pacific grove"] <- "pacific grove"
  both$city.x[both$city.x == "manhattan" & both$city.y == "new york"] <- "new york"
  both$city.x[both$city.x == "east lansging" & both$city.y == "east lansing"] <- "east lansing"
  both$city.x[both$city.x == "new yor city" & both$city.y == "new york city"] <- "new york"
  both$city.x[both$city.x == "manhattan" & both$city.y == "new york"] <- "new york"
  both$city.x[both$city.x == "los angeles" & both$city.y == "malibu"] <- "malibu"
  both$city.x[both$city.x == "manhattan" & both$city.y == "new york"] <- "new york"
  both$city.x[both$city.x == "manhattan" & both$city.y == "new york"] <- "new york"
  both$city.x[both$city.x == "sheffield s10 2tn"] <- "sheffield s10 2tn"

  both$unit.x[both$city.x == "\xcadepartment of animal ecology and tropical biology (zoology iii)"] <- "department of animal ecology and tropical biology (zoology iii)"
  both$city.x[both$city.x == "\xcadepartment of animal ecology and tropical biology (zoology iii)"] <- NA

  both$notes.y[both$city.x == "aberdeen" & both$city.y == "cragiebuckler"] <- "2x city"
  both$city.x[both$city.x == "invergowric" & both$city.y == "invergowrie"] <- "invergowrie"
  both$notes.y[both$city.x == "brisbane" & both$city.y == "st lucia"] <- "2x city"
  both$notes.y[both$city.x == "london" & both$city.y == "ascot"] <- "2x city"
  both$notes.y[both$city.x == "williams" & both$city.y == "mystic"] <- "2x city"
  both$notes.y[both$city.x == "melbourne" & both$city.y == "parkville"] <- "2x city"
  both$notes.y[both$city.x == "new brunswick" & both$city.y == "polson"] <- "2x city"
  both$notes.y[both$city.x == "london" & both$city.y == "notre dame"] <- "notre dame"
  both$notes.y[both$city.x == "canberra" & both$city.y == "lyneham"] <- "2x city"
  both$notes.y[both$city.x == "canberra" & both$city.y == "lyneham"] <- "2x city"
  both$notes.y[both$city.x == "canberra" & both$city.y == "lyneham"] <- "2x city"

  summary(both$city.x == both$city.y)
  both$city_check <- both$city.x == both$city.y
  # write.csv(city_check, file="./data/patrick_james_data_corrections/complete/city_corrections_2x.csv", row.names = f) #export it as a csv file

  # this will replace all the "na" in city.x (origianlly no info) with the value from city.y (patrick's data collection), if there is one
  both <- both %>% mutate(city.x = replace(city.x, is.na(city.x), city.y[is.na(city.x)]))

  both$city.y <- NULL
  both$city_check <- NULL
  both <- both %>% rename("city" = "city.x")
  # str(both)

  # todo unit differences between all data and checked file
  # both$unit.x<-as.character(both$unit.x)
  # both$unit.x<-gsub("",na,both$unit.x)
  both$unit.x[both$unit.x == ""] <- NA
  summary(both$unit.x == both$unit.y) # 7 false
  both$unit_check <- both$unit.x == both$unit.y

  # no probs, just differences in words ("the, and", etc)
  # this will replace all the "na" in unit.x (origianlly no info) with the value from unit.y (patrick's data collection), if there is one
  both <- both %>% mutate(unit.x = replace(unit.x, is.na(unit.x), unit.y[is.na(unit.x)]))

  both$unit.x[both$unit.y == "savannah river ecology laboratory"] <- "savannah river ecology laboratory"
  both$unit.y <- NULL
  both$unit_check <- NULL
  both <- both %>% rename("unit" = "unit.x")

  # todo country differences between all data and checked file

  both <- both %>% mutate(country.x = replace(country.x, country.x == "", NA))
  both$country.x <- as.factor(both$country.x)
  both$country.y <- as.factor(both$country.y)
  country_levels <- (c(levels(both$country.x), levels(both$country.y)))
  levels(both$country.x) <- c(levels(both$country.x), country_levels, NA)
  levels(both$country.y) <- c(levels(both$country.y), country_levels, NA)

  # this will replace all the "na" in city.x (origianlly no info) with the value from city.y (patrick's data collection), if there is one

  levels(both$country.x)
  str(both$country.x)
  str(both$country.y)

  both <- both %>% mutate(country.x = replace(country.x, is.na(country.x), country.y[is.na(country.x)]))
  summary(both$country.x == both$country.y) # 3552 false
  both$country_check <- both$country.x == both$country.y

  country_check <- filter(both, country_check == "FALSE")
  # write.csv(country_check, file="./data/patrick_james_data_corrections/complete/country_corrections_2x.csv", row.names = f) #export it as a csv file


  both$first_name[both$last_name == "weiher" & both$journal == "plantecol"] <- "ewan"
  both$middle_name[both$last_name == "long" & both$first_name == "steve" & both$journal == "gcb"] <- "p"
  both$first_name[both$last_name == "long" & both$first_name == "steve" & both$journal == "gcb"] <- "stephen"
  both$inst[both$last_name == "long" & both$first_name == "stephen" &
    both$journal == "gcb" & both$year > 1999] <- "university of illinois"

  both$first_name[both$last_name == "olsvig-whittaker" & both$journal == "plantecol"] <- "d"
  both$middle_name[both$last_name == "olsvig-whittaker" & both$journal == "plantecol"] <- "l"

  both$country.x[both$last_name == "tjoelker" & both$journal == "newphyt" & both$inst == "texas a & m university"] <- "usa"
  both$country.x[both$last_name == "atkin" & both$journal == "newphyt" & both$inst == "university of york"] <- "united kingdom"
  both$country.x[both$last_name == "long" & both$journal == "jecol" & both$inst == "university of essex"] <- "united kingdom"
  both$country.x[both$last_name == "westing" & both$journal == "conbio" & both$inst == "stockholm international peace research institute"] <- "sweden"
  both$city[both$last_name == "westing" & both$journal == "conbio" & both$inst == "stockholm international peace research institute"] <- "stockholm"
  both$state[both$last_name == "westing" & both$journal == "conbio" & both$inst == "stockholm international peace research institute"] <- NA
  both$country.x[both$last_name == "westing" & both$journal == "conbio" & both$inst == "westing associates"] <- "usa"
  both$country.x[both$last_name == "belovsky" & both$journal == "conbio" & both$inst == "utah state university"] <- "usa"
  both$unit[both$last_name == "bolker" & both$journal == "amnat" & both$inst == "mcmaster university"] <- NA
  both$country.x[both$last_name == "bolker" & both$journal == "amnat" & both$inst == "mcmaster university"] <- "canada"

  both$unit[both$last_name == "krivan" & both$journal == "amnat"] <- "biology centre"
  both$inst[both$last_name == "krivan" & both$journal == "amnat"] <- "academy of sciences of the czech republic"
  both$city[both$last_name == "krivan" & both$journal == "amnat"] <- "ceske budejovice"
  both$state[both$last_name == "krivan" & both$journal == "amnat"] <- "south bohemia"
  both$country.x[both$last_name == "krivan" & both$journal == "amnat"] <- "czech republic"

  both$notes.y[both$country.y == "wales"] <- "wales"
  both$notes.y[both$country.y == "scotland"] <- "scotland"
  both$notes.y[both$country.y == "england"] <- "england"
  both$notes.y[both$country.x == "wales"] <- "wales"
  both$notes.y[both$country.x == "scotland"] <- "scotland"
  both$notes.y[both$country.x == "england"] <- "england"
  both$country.x[both$country.x == "wales"] <- "united kingdom"
  both$country.x[both$country.x == "scotland"] <- "united kingdom"
  both$country.x[both$country.x == "england"] <- "united kingdom"


  both$country.y <- NULL
  both$country_check <- NULL
  both <- both %>% rename("country" = "country.x")

  # todo notes differences between all data and checked file

  str(both$notes.x)
  both$notes <- paste(both$notes.x, both$notes.y, sep = " / ")
  both$notes[both$notes == "na / na"] <- NA
  both$notes <- gsub(" / na", "", both$notes)
  both$notes <- gsub("na / ", "", both$notes)
  both$notes.x <- NULL
  both$notes.y <- NULL
  both$notes[both$inst == "lyme regis"] <- "is this ghillean prance unattached?"
  both$notes[both$inst == "neri"] <- "no longer exists: https://tethys.pnnl.gov/institution/national-environmental-research-institute-neri"
  both$notes[both$last_name == "boggs"] <- "2x carol boggs has inst colorado but should be stanford"
  # both$notes[both$inst == "biological centre"] <- "double check inst"
  # both$notes[both$inst == "biological institute"] <- "double check inst"
  # both$notes[both$inst == "bogota"] <- "double check inst"
  # both$notes[both$inst == "disteba university of salento"] <- "double check inst -  / disteba universita di lecce"
  # both$notes[both$inst == "haus nr.9"] <- "double check inst"
  # both$notes[both$inst == "james cook university townsville"] <- "double check inst"
  # both$notes[both$inst == "lancaster"] <- "double check inst"
  # both$notes[both$inst == "london"] <- "double check inst"
  # both$notes[both$inst == "madrid"] <- "double check inst"
  # both$notes[both$inst == "maine"] <- "double check inst"
  # both$notes[is.na(both$inst)] <- "double check inst"
  # both$notes[both$inst == "royal botanic gardens melbourne university of melbourne"] <- "double check inst"
  # both$notes[both$inst == "salzburg"] <- "double check inst"
  # both$notes[both$inst == "swansea"] <- "double check inst"
  # both$notes[both$inst == "sydney"] <- "double check inst"
  # both$notes[both$inst == "seidenstzicker& kleiman = smithsonian national zoological park, labandeira: smithsonian national museum of natural history"] <- "double check inst"
  # both$notes[both$inst == "montpellier"] <- "double check inst"
  # both$notes[both$inst == "double check"] <- "double check inst"
  # both$notes[both$inst == "cnrs"] <- "double check what campus/unit"
  # both$notes[both$inst == "csiro"] <- "double check what campus/unit"
  # both$notes[both$inst == "smithsonian institution"] <- "double check what campus/unit"
  # both$notes[both$inst == "university of arkansas"] <- "double check what campus/unit"
  # both$notes[both$inst == "university of british columbia"] <- "double check what campus/unit"
  # both$notes[both$inst == "university of california"] <- "double check what campus/unit"
  # both$notes[both$inst == "university of exeter"] <- "double check what campus/unit"
  # both$notes[both$inst == "university of georgia"] <- "double check what campus/unit"
  # both$notes[both$inst == "university of illinois"] <- "double check what campus/unit"
  # both$notes[both$inst == "university of massachusetts"] <- "double check what campus/unit"
  # both$notes[both$inst == "university of minnesota"] <- "double check what campus/unit"
  # both$notes[both$inst == "university of south carolina"] <- "double check what campus/unit"
  # both$notes[both$inst == "university of texas"] <- "double check what campus/unit"
  # both$notes[both$inst == "university of toronto"] <- "double check what campus/unit"
  # both$notes[both$inst == "university of wisconsin"] <- "double check what campus/unit"
  # both$notes[both$inst == "us geological survey"] <- "double check what campus/unit"



  # both<-institution_cleaner(both)


  # no idea why this isn't working inside function, so doing here
  both$inst <- gsub("arkansas", "arkansas", both$inst)


  # todo: some error checking of states:
  # canada instead of canada
  # british columbia in usa
  # lower austria     usa
  # galicia     usa
  # england     usa
  # uppland     usa
  # ontario in usa
  # gelderland

  both$country[both$state == "british columbia" & both$country == "usa"] <- "canada"
  both$country[both$state == "lower austria" & both$country == "usa"] <- "canada"
  both$country[both$state == "galicia" & both$country == "usa"] <- "spain"
  both$country[both$state == "england" & both$country == "usa"] <- "united kingdom"
  both$country[both$state == "uppland" & both$country == "usa"] <- "sweden"
  both$country[both$state == "ontario"] <- "canada"
  both$country[both$state == "gelderland"] <- "netherlands"
  levels(both$inst)




  colnames(both)
  both$editor_id.y <- NULL
  both <- both %>% rename("editor_id" = "editor_id.x")

  both <- both[!(is.na(both$journal) & is.na(both$year)), ]

  #
  # both<-both %>%
  #   group_by(journal,last_name,first_name) %>%
  #   mutate(inst = ifelse((row_number()==1 & is.na(inst)), "missing", inst))
  #
  #
  # both<-both %>%
  #   group_by(journal,last_name,first_name) %>%
  #   mutate(unit = ifelse((row_number()==1 & is.na(unit)), "missing", unit))
  #
  # both<-both %>%
  #   group_by(journal,last_name,first_name) %>%
  #   mutate(state = ifelse((row_number()==1 & is.na(state)), "missing", state))
  #
  #
  # both<-both %>%
  #   group_by(journal,last_name,first_name) %>%
  #   mutate(city = ifelse((row_number()==1 & is.na(city)), "missing", city))
  #
  new_row <- both %>%
    filter(last_name == "willis" & journal == "jecol" & year == 1994)
  new_row$year <- 1992
  both <- rbind(new_row, both)
  both$inst[both$last_name == "willis" & both$journal == "jecol" &
    (both$year > 1990 | both$year < 2007)] <- "university of sheffield"
  both$city[both$last_name == "willis" & both$journal == "jecol" &
    (both$year > 1990 | both$year < 2007)] <- "sheffield"
  both$state[both$last_name == "willis" & both$journal == "jecol" &
    (both$year > 1990 | both$year < 2007)] <- "s yorkshire"
  both$country[both$last_name == "willis" & both$journal == "jecol" &
    (both$year > 1990 | both$year < 2007)] <- "united kingdom"

  both <- both %>% arrange(journal, last_name, first_name, year)

  # colnames(both)
  # str(as.data.frame(both))
  # str(alldata)
  # colnames(alldata)
  # colnames(both)==colnames(alldata)





  # added eb 11 october 2020
  both$first_name <- tolower(both$first_name)
  both$last_name <- tolower(both$last_name)
  both$middle_name <- tolower(both$middle_name)

  both$inst[both$inst == "double check"] <- NA

  both$inst[both$last_name == "charlesworth" & both$year == 1991 & both$journal == "amnat"] <- "university of chicago"
  both$inst[both$last_name == "chesson" & both$year == 1991 & both$journal == "amnat"] <- "australian national university"
  both$inst[both$last_name == "chesson" & both$year == 1995 & both$journal == "amnat"] <- "australian national university"
  both$inst[both$last_name == "duffy" & both$year == 2015 & both$journal == "amnat"] <- "georgia institute of technology"
  both$inst[both$last_name == "dworkin" & both$year == 2015 & both$journal == "amnat"] <- "mcmaster university"
  both$inst[both$last_name == "frederickson" & both$year == 2015 & both$journal == "amnat"] <- "university of toronto"
  both$inst[both$last_name == "fuller" & both$year == 2015 & both$journal == "amnat"] <- "university of illinois"
  both$inst[both$last_name == "kirkpatrick" & both$year == 1991 & both$journal == "amnat"] <- "university of texas austin"
  both$inst[both$last_name == "leips" & both$year == 2015 & both$journal == "amnat"] <- "university of maryland baltimore county"
  both$inst[both$last_name == "meagher" & both$year == 1991 & both$journal == "amnat"] <- "university of st andrews"
  both$inst[both$last_name == "mooney" & both$year == 1985 & both$journal == "amnat"] <- "stanford university"
  both$inst[both$last_name == "mooney" & both$year == 1990 & both$journal == "amnat"] <- "stanford university"
  both$inst[both$last_name == "pagel" & both$year == 1997 & both$journal == "amnat"] <- "university of oxford"
  both$inst[both$last_name == "pastor" & both$year == 1991 & both$journal == "amnat"] <- "university of minnesota duluth"
  both$inst[both$last_name == "real" & both$year == 1991 & both$journal == "amnat"] <- "indiana university"
  both$inst[both$last_name == "real" & both$year == 1993 & both$journal == "amnat"] <- "indiana university"
  both$inst[both$last_name == "roth" & both$year == 1991 & both$journal == "amnat"] <- "duke university"
  both$inst[both$last_name == "roughgarden" & both$year == 1985 & both$journal == "amnat"] <- "stanford university"
  both$inst[both$last_name == "roughgarden" & both$year == 1990 & both$journal == "amnat"] <- "stanford university"
  both$inst[both$last_name == "seger" & both$year == 1991 & both$journal == "amnat"] <- "university of utah"
  both$inst[both$last_name == "tilma" & both$year == 1991 & both$journal == "amnat"] <- "university of minnesota"
  both$inst[both$last_name == "tilma" & both$year == 1994 & both$journal == "amnat"] <- "university of minnesota"
  both$inst[both$last_name == "whitlock" & both$year == 2005 & both$journal == "amnat"] <- "university of british columbia"
  both$inst[both$last_name == "wu" & both$year == 1991 & both$journal == "amnat"] <- "university of chicago"
  both$inst[both$last_name == "foote" & both$year == 2004 & both$journal == "arees"] <- "university of chicago"
  both$inst[both$last_name == "werner" & both$year == 1985 & both$journal == "arees"] <- "michigan state university"
  both$inst[both$last_name == "wing" & both$year == 1999 & both$journal == "arees"] <- "smithsonian national museum of natural history"
  both$inst[both$last_name == "wing" & both$year == 2003 & both$journal == "arees"] <- "smithsonian national museum of natural history"
  both$inst[both$last_name == "andelman" & both$year == 2006 & both$journal == "biocon"] <- "university of california santa barbara"
  both$inst[both$last_name == "bourliere" & both$year == 1986 & both$journal == "biocon"] <- "university of paris"
  both$inst[both$last_name == "dirzo" & both$year == 1998 & both$journal == "biocon"] <- "northern arizona university"
  both$inst[both$last_name == "duffey" & both$year == 1989 & both$journal == "biocon"] <- "unaffiliated"
  both$inst[both$last_name == "duffey" & both$year == 2014 & both$journal == "biocon"] <- "unaffiliated"
  both$inst[both$last_name == "hawkins" & both$year == 2008 & both$journal == "biocon"] <- "university of southampton"
  both$inst[both$last_name == "hawkins" & both$year == 2014 & both$journal == "biocon"] <- "university of southampton"
  both$inst[both$last_name == "hockey" & both$year == 1997 & both$journal == "biocon"] <- "university of cape town"
  both$inst[both$last_name == "hockey" & both$year == 2007 & both$journal == "biocon"] <- "university of cape town"
  both$inst[both$last_name == "hockey" & both$year == 2008 & both$journal == "biocon"] <- "university of cape town"
  both$inst[both$last_name == "hockey" & both$year == 2009 & both$journal == "biocon"] <- "university of cape town"
  both$inst[both$last_name == "johnsingh" & both$year == 2006 & both$journal == "biocon"] <- "wildlife institute of india"
  both$inst[both$last_name == "krishnaswamy" & both$year == 2014 & both$journal == "biocon"] <- "ashoka trust for research in ecology and the environment"
  both$inst[both$last_name == "kuenen" & both$year == 1986 & both$journal == "biocon"] <- "university of leiden"
  both$inst[both$last_name == "lomolino" & both$year == 1998 & both$journal == "biocon"] <- "university of oklahoma"
  both$inst[both$last_name == "marrs" & both$year == 2014 & both$journal == "biocon"] <- "university of liverpool"
  both$inst[both$last_name == "mcnicholl" & both$year == 1990 & both$journal == "biocon"] <- "unaffiliated"
  both$inst[both$last_name == "mcnicholl" & both$year == 1993 & both$journal == "biocon"] <- "unaffiliated"
  both$inst[both$last_name == "morgan" & both$year == 1987 & both$journal == "biocon"] <- "station biologique de la tour du valat"
  both$inst[both$last_name == "morgan" & both$year == 2004 & both$journal == "biocon"] <- "station biologique de la tour du valat"
  both$inst[both$last_name == "peterken" & both$year == 1998 & both$journal == "biocon"] <- "unaffiliated"
  both$inst[both$last_name == "peterken" & both$year == 2009 & both$journal == "biocon"] <- "unaffiliated"
  both$inst[both$last_name == "scott" & both$year == 1986 & both$journal == "biocon"] <- "unaffiliated"
  both$inst[both$last_name == "westhoff" & both$year == 1985 & both$journal == "biocon"] <- "unaffiliated"
  both$inst[both$last_name == "westhoff" & both$year == 1988 & both$journal == "biocon"] <- "unaffiliated"
  both$inst[both$last_name == "anderson" & both$year == 1993 & both$journal == "bitr"] <- "australian national university"
  both$inst[both$last_name == "anderson" & both$year == 1997 & both$journal == "bitr"] <- "australian national university"
  both$inst[both$last_name == "benson" & both$year == 1993 & both$journal == "bitr"] <- "universidade estadual de campinas"
  both$inst[both$last_name == "benson" & both$year == 1996 & both$journal == "bitr"] <- "universidade estadual de campinas"
  both$inst[both$last_name == "berry" & both$year == 1996 & both$journal == "bitr"] <- "missouri botanical garden"
  both$inst[both$last_name == "berry" & both$year == 1997 & both$journal == "bitr"] <- "missouri botanical garden"
  both$inst[both$last_name == "brown" & both$year == 1993 & both$journal == "bitr"] <- "university of illinois urbana champaign"
  both$inst[both$last_name == "brown" & both$year == 1996 & both$journal == "bitr"] <- "university of illinois urbana champaign"
  both$inst[both$last_name == "foster" & both$year == 1996 & both$journal == "bitr"] <- "usgs patuxent wildlife research center"
  both$inst[both$last_name == "fox" & both$year == 1993 & both$journal == "bitr"] <- "curtin university of technology"
  both$inst[both$last_name == "fox" & both$year == 1997 & both$journal == "bitr"] <- "curtin university of technology"
  both$inst[both$last_name == "west-eberhard" & both$year == 1993 & both$journal == "bitr"] <- "smithsonian tropical research institute"
  both$inst[both$last_name == "west-eberhard" & both$year == 1996 & both$journal == "bitr"] <- "smithsonian tropical research institute"
  both$inst[both$last_name == "angert" & both$year == 2015 & both$journal == "evol"] <- "university of british columbia"
  both$inst[both$last_name == "azevedo" & both$year == 2015 & both$journal == "evol"] <- "university of houston"
  both$inst[both$last_name == "case" & both$year == 2015 & both$journal == "evol"] <- "kent state university"
  both$inst[both$last_name == "dean" & both$year == 2015 & both$journal == "evol"] <- "university of minnesota"
  both$inst[both$last_name == "dworkin" & both$year == 2015 & both$journal == "evol"] <- "michigan state university"
  both$inst[both$last_name == "edmands" & both$year == 2015 & both$journal == "evol"] <- "university of southern california"
  both$inst[both$last_name == "engelstadter" & both$year == 2015 & both$journal == "evol"] <- "university of queensland"
  both$inst[both$last_name == "evans" & both$year == 2015 & both$journal == "evol"] <- "monash university"
  both$inst[both$last_name == "friedman" & both$year == 2015 & both$journal == "evol"] <- "university of oxford"
  both$inst[both$last_name == "hadfield" & both$year == 2015 & both$journal == "evol"] <- "university of edinburgh"
  both$inst[both$last_name == "hahn" & both$year == 2015 & both$journal == "evol"] <- "indiana university"
  both$inst[both$last_name == "hall" & both$year == 2015 & both$journal == "evol"] <- "university of georgia"
  both$inst[both$last_name == "kisdi" & both$year == 2015 & both$journal == "evol"] <- "university of helsinki"
  both$inst[both$last_name == "laine" & both$year == 2015 & both$journal == "evol"] <- "university of helsinki"
  both$inst[both$last_name == "marshall" & both$year == 2015 & both$journal == "evol"] <- "monash university"
  both$inst[both$last_name == "masel" & both$year == 2015 & both$journal == "evol"] <- "university of arizona"
  both$inst[both$last_name == "mcadam" & both$year == 2015 & both$journal == "evol"] <- "university of guelph"
  both$inst[both$last_name == "neiman" & both$year == 2015 & both$journal == "evol"] <- "university of iowa"
  both$inst[both$last_name == "redfield" & both$year == 2015 & both$journal == "evol"] <- "university of british columbia"
  both$inst[both$last_name == "robosky" & both$year == 2015 & both$journal == "evol"] <- "university of michigan"
  both$inst[both$last_name == "roze" & both$year == 2015 & both$journal == "evol"] <- "roscoff biological station"
  both$inst[both$last_name == "decamps" & both$journal == "leco"] <- "cnrs centre delaboration de materiaux et detudes structurales"
  both$inst[both$last_name == "weimerskirch" & both$journal == "jane"] <- "centre detudes biologiques de chize"
  both$inst[both$last_name == "rozen" & both$year == 2015 & both$journal == "evol"] <- "university of leiden"
  both$inst[both$last_name == "sweigart" & both$year == 2015 & both$journal == "evol"] <- "university of georgia"
  both$inst[both$last_name == "tobias" & both$year == 2015 & both$journal == "evol"] <- "university of oxford"
  both$inst[both$last_name == "valenzuela" & both$year == 2015 & both$journal == "evol"] <- "iowa state university"
  both$inst[both$last_name == "bearhop" & both$year == 2008 & both$journal == "jane"] <- "university of exeter"
  both$inst[both$last_name == "bearhop" & both$year == 2014 & both$journal == "jane"] <- "university of exeter"
  both$inst[both$last_name == "gurney" & both$year == 1999 & both$journal == "jane"] <- "university of strathclyde"
  both$inst[both$last_name == "gurney" & both$year == 2014 & both$journal == "jane"] <- "university of strathclyde"
  both$inst[both$last_name == "hall" & both$year == 1999 & both$journal == "jane"] <- "flinders university"
  both$inst[both$last_name == "hall" & both$year == 2009 & both$journal == "jane"] <- "worldfish centre"
  both$inst[both$last_name == "lessells" & both$year == 1994 & both$journal == "jane"] <- "netherlands institute of ecology nioo knaw"
  both$inst[both$last_name == "lessells" & both$year == 2014 & both$journal == "jane"] <- "netherlands institute of ecology nioo knaw"
  both$inst[both$last_name == "manly" & both$year == 2005 & both$journal == "jane"] <- "western ecosystem technology"
  both$inst[both$last_name == "manly" & both$year == 2013 & both$journal == "jane"] <- "western ecosystem technology"
  both$inst[both$last_name == "may" & both$year == 1991 & both$journal == "jane"] <- "university of oxford"
  both$inst[both$last_name == "may" & both$year == 1996 & both$journal == "jane"] <- "university of oxford"
  both$inst[both$last_name == "mccann" & both$year == 2005 & both$journal == "jane"] <- "university of guelph"
  both$inst[both$last_name == "mccleery" & both$year == 2008 & both$journal == "jane"] <- "university of oxford"
  both$inst[both$last_name == "mcintyre" & both$year == 1985 & both$journal == "jane"] <- "university of aberdeen"
  both$inst[both$last_name == "mcintyre" & both$year == 1990 & both$journal == "jane"] <- "university of aberdeen"
  both$inst[both$last_name == "meiri" & both$year == 2011 & both$journal == "jane"] <- "tel aviv university"
  both$inst[both$last_name == "meiri" & both$year == 2014 & both$journal == "jane"] <- "tel aviv university"
  both$inst[both$last_name == "o'gorman" & both$year == 2013 & both$journal == "jane"] <- "queen mary university of london"
  both$inst[both$last_name == "o'gorman" & both$year == 2014 & both$journal == "jane"] <- "imperial college london"
  both$inst[both$last_name == "rogers" & both$year == 1994 & both$journal == "jane"] <- "university of oxford"
  both$inst[both$last_name == "rogers" & both$year == 1996 & both$journal == "jane"] <- "university of oxford"
  both$inst[both$last_name == "stouffer" & both$year == 2013 & both$journal == "jane"] <- "university of canterbury"
  both$inst[both$last_name == "stouffer" & both$year == 2014 & both$journal == "jane"] <- "university of canterbury"
  both$inst[both$last_name == "thorpe" & both$year == 1994 & both$journal == "jane"] <- "soafd fisheries laboratory"
  both$inst[both$last_name == "thorpe" & both$year == 1997 & both$journal == "jane"] <- "university of glasgow "
  both$inst[both$last_name == "vanderpol" & both$year == 2013 & both$journal == "jane"] <- "australian national university"
  both$inst[both$last_name == "vanderpol" & both$year == 2014 & both$journal == "jane"] <- "australian national university"
  both$inst[both$last_name == "block" & both$year == 1985 & both$journal == "jape"] <- "liverpool john moores university"
  both$inst[both$last_name == "block" & both$year == 1989 & both$journal == "jape"] <- "liverpool john moores university"
  both$inst[both$last_name == "bullock" & both$year == 2000 & both$journal == "jape"] <- "institute of terrestrial ecology"
  both$inst[both$last_name == "bullock" & both$year == 2003 & both$journal == "jape"] <- "institute of terrestrial ecology"
  both$inst[both$last_name == "clarke" & both$year == 1989 & both$journal == "jape"] <- "institute of terrestrial ecology"
  both$inst[both$last_name == "day" & both$year == 1988 & both$journal == "jape"] <- "silsoe research institute"
  both$inst[both$last_name == "day" & both$year == 1996 & both$journal == "jape"] <- "silsoe research institute"
  both$inst[both$last_name == "fernande-juricic" & both$year == 2010 & both$journal == "jape"] <- "purdue university"
  both$inst[both$last_name == "freckleto" & both$year == 2005 & both$journal == "jape"] <- "university of oxford"
  both$inst[both$last_name == "hill" & both$year == 1993 & both$journal == "jape"] <- "game conservancy"
  both$inst[both$last_name == "hill" & both$year == 1999 & both$journal == "jape"] <- "university of cambridge"
  both$inst[both$last_name == "mason" & both$year == 1989 & both$journal == "jape"] <- "university of essex"
  both$inst[both$last_name == "mason" & both$year == 1996 & both$journal == "jape"] <- "university of essex"
  both$inst[both$last_name == "mead" & both$year == 1985 & both$journal == "jape"] <- "university of reading"
  both$inst[both$last_name == "mead" & both$year == 1987 & both$journal == "jape"] <- "university of reading"
  both$inst[both$last_name == "miles" & both$year == 1988 & both$journal == "jape"] <- "institute of terrestrial ecology"
  both$inst[both$last_name == "miles" & both$year == 1991 & both$journal == "jape"] <- "institute of terrestrial ecology"
  both$inst[both$last_name == "monteith" & both$year == 1985 & both$journal == "jape"] <- "university of nottingham"
  both$inst[both$last_name == "monteith" & both$year == 1987 & both$journal == "jape"] <- "university of nottingham"
  both$inst[both$last_name == "roberts" & both$year == 1985 & both$journal == "jape"] <- "central electricity research laboratories"
  both$inst[both$last_name == "roberts" & both$year == 1987 & both$journal == "jape"] <- "central electricity research laboratories"
  both$inst[both$last_name == "rutter" & both$year == 1985 & both$journal == "jape"] <- "imperial college london"
  both$inst[both$last_name == "rutter" & both$year == 1988 & both$journal == "jape"] <- "imperial college london"
  both$inst[both$last_name == "smith" & both$year == 2005 & both$journal == "jape"] <- "central science laboratory"
  both$inst[both$last_name == "smith" & both$year == 2006 & both$journal == "jape"] <- "central science laboratory"
  both$inst[both$last_name == "thomas" & both$year == 1989 & both$journal == "jape"] <- "afrc institute for grassland and animal production"
  both$inst[both$last_name == "thomas" & both$year == 1997 & both$journal == "jape"] <- "afrc institute for grassland and animal production"
  both$inst[both$last_name == "walpole" & both$year == 2006 & both$journal == "jape"] <- "fauna and flora international"
  both$inst[both$last_name == "walpole" & both$year == 2009 & both$journal == "jape"] <- "fauna and flora international"
  both$inst[both$last_name == "welch" & both$year == 1988 & both$journal == "jape"] <- "institute of terrestrial ecology"
  both$inst[both$last_name == "welch" & both$year == 1996 & both$journal == "jape"] <- "institute of terrestrial ecology"
  both$inst[both$last_name == "ali" & both$year == 2013 & both$journal == "jbiog"] <- "university of hong kong"
  both$inst[both$last_name == "ali" & both$year == 2014 & both$journal == "jbiog"] <- "university of hong kong"
  both$inst[both$last_name == "bryson" & both$year == 2014 & both$journal == "jbiog"] <- "university of washington"
  both$inst[both$last_name == "carine" & both$year == 2010 & both$journal == "jbiog"] <- "natural history museum london"
  both$inst[both$last_name == "carine" & both$year == 2014 & both$journal == "jbiog"] <- "natural history museum london"
  both$inst[both$last_name == "cavers" & both$year == 2014 & both$journal == "jbiog"] <- "nerc centre for ecology and hydrology"
  both$inst[both$last_name == "chapman" & both$year == 2014 & both$journal == "jbiog"] <- "nerc centre for ecology and hydrology"
  both$inst[both$last_name == "diniz-filho" & both$year == 2005 & both$journal == "jbiog"] <- "universidade federal de goias"
  both$inst[both$last_name == "diniz-filho" & both$year == 2006 & both$journal == "jbiog"] <- "universidade federal de goias"
  both$inst[both$last_name == "emerson" & both$year == 2013 & both$journal == "jbiog"] <- "csic - ipna"
  both$inst[both$last_name == "emerson" & both$year == 2014 & both$journal == "jbiog"] <- "csic - ipna"
  both$inst[both$last_name == "gaither" & both$year == 2014 & both$journal == "jbiog"] <- "durham university"
  both$inst[both$last_name == "gilman" & both$year == 2010 & both$journal == "jbiog"] <- "auckland university of technology"
  both$inst[both$last_name == "gilman" & both$year == 2014 & both$journal == "jbiog"] <- "auckland university of technology"
  both$inst[both$last_name == "guilhaumon" & both$year == 2013 & both$journal == "jbiog"] <- "university of evora"
  both$inst[both$last_name == "guilhaumon" & both$year == 2014 & both$journal == "jbiog"] <- "universite de montpellier"
  both$inst[both$last_name == "hansen" & both$year == 2011 & both$journal == "jbiog"] <- "university of kwazulu natal"
  both$inst[both$last_name == "hansen" & both$year == 2014 & both$journal == "jbiog"] <- "university of zurich"
  both$inst[both$last_name == "harrison" & both$year == 1985 & both$journal == "jbiog"] <- "university college london"
  both$inst[both$last_name == "harrison" & both$year == 2004 & both$journal == "jbiog"] <- "university college london"
  both$inst[both$last_name == "higgins" & both$year == 2013 & both$journal == "jbiog"] <- "goethe university frankfurt"
  both$inst[both$last_name == "higgins" & both$year == 2014 & both$journal == "jbiog"] <- "university of otago"
  both$inst[both$last_name == "holger" & both$year == 2014 & both$journal == "jbiog"] <- "university of gottingen"
  both$inst[both$last_name == "kreft" & both$year == 2014 & both$journal == "jbiog"] <- "university of gottingen"
  both$inst[both$last_name == "holland" & both$year == 1986 & both$journal == "jbiog"] <- "university of otago"
  both$inst[both$last_name == "holland" & both$year == 2003 & both$journal == "jbiog"] <- "university of otago"
  both$inst[both$last_name == "jetz" & both$year == 2006 & both$journal == "jbiog"] <- "university of california san diego"
  both$inst[both$last_name == "jetz" & both$year == 2014 & both$journal == "jbiog"] <- "yale university"
  both$inst[both$last_name == "katinas" & both$year == 2014 & both$journal == "jbiog"] <- "museo de la plata"
  both$inst[both$last_name == "kullman" & both$year == 1991 & both$journal == "jbiog"] <- "university of umea"
  both$inst[both$last_name == "kullman" & both$year == 2004 & both$journal == "jbiog"] <- "university of umea"
  both$inst[both$last_name == "lei" & both$year == 2014 & both$journal == "jbiog"] <- "chinese academy of sciences"
  both$inst[both$last_name == "maggs" & both$year == 2008 & both$journal == "jbiog"] <- "queens university belfast"
  both$inst[both$last_name == "maggs" & both$year == 2014 & both$journal == "jbiog"] <- "queens university belfast"
  both$inst[both$last_name == "masters" & both$year == 2011 & both$journal == "jbiog"] <- "university of kwazulu natal"
  both$inst[both$last_name == "masters" & both$year == 2012 & both$journal == "jbiog"] <- "university of ft hare"
  both$inst[both$last_name == "parmakelis" & both$year == 2013 & both$journal == "jbiog"] <- "university of athens"
  both$inst[both$last_name == "parmakelis" & both$year == 2014 & both$journal == "jbiog"] <- "university of athens"
  both$inst[both$last_name == "paulay" & both$year == 2014 & both$journal == "jbiog"] <- "university of florida"
  both$inst[both$last_name == "phillimore" & both$year == 2013 & both$journal == "jbiog"] <- "university of edinburgh"
  both$inst[both$last_name == "phillimore" & both$year == 2014 & both$journal == "jbiog"] <- "university of edinburgh"
  both$inst[both$last_name == "prance" & both$year == 2000 & both$journal == "jbiog"] <- "royal botanic gardens kew"
  both$inst[both$last_name == "prance" & both$year == 2004 & both$journal == "jbiog"] <- "royal botanic gardens kew"
  both$inst[both$last_name == "richardson" & both$year == 2011 & both$journal == "jbiog"] <- "royal botanic garden edinburgh"
  both$inst[both$last_name == "richardson" & both$year == 2014 & both$journal == "jbiog"] <- "royal botanic garden edinburgh"
  both$inst[both$last_name == "schaefer" & both$year == 2014 & both$journal == "jbiog"] <- "technical university of munich"
  both$inst[both$last_name == "stearns" & both$year == 1985 & both$journal == "jbiog"] <- "university of wisconsin"
  both$inst[both$last_name == "stearns" & both$year == 1990 & both$journal == "jbiog"] <- "university of wisconsin"
  both$inst[both$last_name == "triantis" & both$year == 2010 & both$journal == "jbiog"] <- "university of azores"
  both$inst[both$last_name == "triantis" & both$year == 2014 & both$journal == "jbiog"] <- "national & kapodistrian university"
  both$inst[both$last_name == "vanderhammen" & both$year == 1985 & both$journal == "jbiog"] <- "university of amsterdam"
  both$inst[both$last_name == "vanderhammen" & both$year == 1990 & both$journal == "jbiog"] <- "university of amsterdam"
  both$inst[both$last_name == "watts" & both$year == 1985 & both$journal == "jbiog"] <- "university of hull"
  both$inst[both$last_name == "watts" & both$year == 1995 & both$journal == "jbiog"] <- "university of hull"
  both$inst[both$last_name == "dekroon" & both$year == 2002 & both$journal == "jecol"] <- "radboud university nijmegen"
  both$inst[both$last_name == "dekroon" & both$year == 2004 & both$journal == "jecol"] <- "radboud university nijmegen"
  both$inst[both$last_name == "etherington" & both$year == 1985 & both$journal == "jecol"] <- "cardiff university"
  both$inst[both$last_name == "etherington" & both$year == 1990 & both$journal == "jecol"] <- "university of wales"
  both$inst[both$last_name == "gray" & both$year == 1991 & both$journal == "jecol"] <- "cardiff university"
  both$inst[both$last_name == "hopkins" & both$year == 1985 & both$journal == "jecol"] <- "university of sheffield"
  both$inst[both$last_name == "huntely" & both$year == 1987 & both$journal == "jecol"] <- "cambridge university"
  both$inst[both$last_name == "huntley" & both$year == 1985 & both$journal == "jecol"] <- "cambridge university"
  both$inst[both$last_name == "huntley" & both$year == 1988 & both$journal == "jecol"] <- "cambridge university"
  both$inst[both$last_name == "lack" & both$year == 1987 & both$journal == "jecol"] <- "swansea university"
  both$inst[both$last_name == "lack" & both$year == 1990 & both$journal == "jecol"] <- "oxford brookes university"
  both$inst[both$last_name == "long" & both$year == 1986 & both$journal == "jecol"] <- "university of essex"
  both$inst[both$last_name == "ouborg" & both$year == 2003 & both$journal == "jecol"] <- "radboud university nijmegen"
  both$inst[both$last_name == "ouborg" & both$year == 2004 & both$journal == "jecol"] <- "radboud university nijmegen"
  both$inst[both$last_name == "vandermeijden" & both$year == 1989 & both$journal == "jecol"] <- "leiden university"
  both$inst[both$last_name == "vandermeijden" & both$year == 1993 & both$journal == "jecol"] <- "leiden university"
  both$inst[both$last_name == "vangroenendael" & both$year == 1992 & both$journal == "jecol"] <- "wageningen university and research"
  both$inst[both$last_name == "white" & both$year == 1986 & both$journal == "jecol"] <- "university college dublin"
  both$inst[both$last_name == "albon" & both$year == 1995 & both$journal == "jzool"] <- "zoological society of london"
  both$inst[both$last_name == "ayers" & both$year == 1985 & both$journal == "newphyt"] <- "lancaster university"
  both$inst[both$last_name == "ayers" & both$year == 2002 & both$journal == "newphyt"] <- "lancaster university"
  both$inst[both$last_name == "barbour" & both$year == 2015 & both$journal == "newphyt"] <- "university of sydney"
  both$inst[both$last_name == "graham" & both$year == 2015 & both$journal == "newphyt"] <- "university of florida"
  both$inst[both$last_name == "schat" & both$year == 2015 & both$journal == "newphyt"] <- "vrije universiteit amsterdam"
  both$inst[both$last_name == "smith" & both$year == 2015 & both$journal == "newphyt"] <- "university of cambridge"
  both$inst[both$last_name == "stinchcombe" & both$year == 2014 & both$journal == "newphyt"] <- "university of toronto"
  both$inst[both$last_name == "sultan" & both$year == 2004 & both$journal == "newphyt"] <- "wesleyan university"
  both$inst[both$last_name == "sultan" & both$year == 2009 & both$journal == "newphyt"] <- "wesleyan university"
  both$inst[both$last_name == "syrett" & both$year == 1985 & both$journal == "newphyt"] <- "university of wales"
  both$inst[both$last_name == "syrett" & both$year == 1989 & both$journal == "newphyt"] <- "university of wales"
  both$inst[both$last_name == "tjoelker" & both$year == 2014 & both$journal == "newphyt"] <- "western sydney university"
  both$inst[both$last_name == "vamosi" & both$year == 2015 & both$journal == "newphyt"] <- "university of calgary"
  both$inst[both$last_name == "west" & both$year == 1994 & both$journal == "newphyt"] <- "university of cambridge"
  both$inst[both$last_name == "wolfenden" & both$year == 1990 & both$journal == "newphyt"] <- "lancaster university"
  both$inst[both$last_name == "wolfenden" & both$year == 1991 & both$journal == "newphyt"] <- "lancaster university"
  both$inst[both$last_name == "wolfenden" & both$year == 1996 & both$journal == "newphyt"] <- "lancaster university"
  both$inst[both$last_name == "bever" & both$year == 2013 & both$journal == "oecol"] <- "university of indiana"
  both$inst[both$last_name == "buchmann" & both$year == 2004 & both$journal == "oecol"] <- "swiss federal institute of technology"
  both$inst[both$last_name == "buchmann" & both$year == 2014 & both$journal == "oecol"] <- "swiss federal institute of technology"
  both$inst[both$last_name == "diehl" & both$year == 2011 & both$journal == "oecol"] <- "university of umea"
  both$inst[both$last_name == "elle" & both$year == 2013 & both$journal == "oecol"] <- "simon fraser university"
  both$inst[both$last_name == "fiedler" & both$year == 2005 & both$journal == "oecol"] <- "university of vienna"
  both$inst[both$last_name == "fiedler" & both$year == 2014 & both$journal == "oecol"] <- "university of vienna"
  both$inst[both$last_name == "gough" & both$year == 2013 & both$journal == "oecol"] <- "university of texas arlington"
  both$inst[both$last_name == "gough" & both$year == 2014 & both$journal == "oecol"] <- "university of texas arlington"
  both$inst[both$last_name == "heimpel" & both$year == 2013 & both$journal == "oecol"] <- "university of minnesota"
  both$inst[both$last_name == "heimpel" & both$year == 2014 & both$journal == "oecol"] <- "university of minnesota"
  both$inst[both$last_name == "herre" & both$year == 2014 & both$journal == "oecol"] <- "smithsonian tropical research institute"
  both$inst[both$last_name == "ibanez" & both$year == 2013 & both$journal == "oecol"] <- "university of michigan"
  both$inst[both$last_name == "ibanez" & both$year == 2014 & both$journal == "oecol"] <- "university of michigan"
  both$inst[both$last_name == "koricheva" & both$year == 2006 & both$journal == "oecol"] <- "royal holloway university of london"
  both$inst[both$last_name == "koricheva" & both$year == 2014 & both$journal == "oecol"] <- "royal holloway university of london"
  both$inst[both$last_name == "korner" & both$year == 1992 & both$journal == "oecol"] <- "university of basel"
  both$inst[both$last_name == "korner" & both$year == 2014 & both$journal == "oecol"] <- "university of basel"
  both$inst[both$last_name == "laaksonen" & both$year == 2014 & both$journal == "oecol"] <- "university of turku "
  both$inst[both$last_name == "layman" & both$year == 2013 & both$journal == "oecol"] <- "florida international university"
  both$inst[both$last_name == "layman" & both$year == 2014 & both$journal == "oecol"] <- "florida international university"
  both$inst[both$last_name == "legalliard" & both$year == 2012 & both$journal == "oecol"] <- "cnrs institut ecologie et environnement"
  both$inst[both$last_name == "legalliard" & both$year == 2014 & both$journal == "oecol"] <- "cnrs institut ecologie et environnement"
  both$inst[both$last_name == "lichstein" & both$year == 2013 & both$journal == "oecol"] <- "university of florida"
  both$inst[both$last_name == "lichstein" & both$year == 2014 & both$journal == "oecol"] <- "university of florida"
  both$inst[both$last_name == "lill" & both$year == 2012 & both$journal == "oecol"] <- "george washington university"
  both$inst[both$last_name == "lill" & both$year == 2014 & both$journal == "oecol"] <- "george washington university"
  both$inst[both$last_name == "muller" & both$year == 2011 & both$journal == "oecol"] <- "university of bielefeld"
  both$inst[both$last_name == "muller" & both$year == 2014 & both$journal == "oecol"] <- "university of bielefeld"
  both$inst[both$last_name == "niinemets" & both$year == 2014 & both$journal == "oecol"] <- "estonian university of life sciences"
  both$inst[both$last_name == "prinzing" & both$year == 2013 & both$journal == "oecol"] <- "universite de rennes 1"
  both$inst[both$last_name == "schaller" & both$year == 1985 & both$journal == "oecol"] <- "university of vienna"
  both$inst[both$last_name == "schaller" & both$year == 1986 & both$journal == "oecol"] <- "university of vienna"
  both$inst[both$last_name == "shurin" & both$year == 2011 & both$journal == "oecol"] <- "university of california san diego"
  both$inst[both$last_name == "shurin" & both$year == 2014 & both$journal == "oecol"] <- "university of california san diego"
  both$inst[both$last_name == "siemann" & both$year == 2012 & both$journal == "oecol"] <- "rice university"
  both$inst[both$last_name == "siemann" & both$year == 2014 & both$journal == "oecol"] <- "rice university"
  both$inst[both$last_name == "stark" & both$year == 2012 & both$journal == "oecol"] <- "utah state university"
  both$inst[both$last_name == "stark" & both$year == 2014 & both$journal == "oecol"] <- "utah state university"
  both$inst[both$last_name == "stewart" & both$year == 1998 & both$journal == "oecol"] <- "university of queensland"
  both$inst[both$last_name == "ward" & both$year == 2014 & both$journal == "oecol"] <- "university of kansas"
  both$inst[both$last_name == "weisser" & both$year == 1985 & both$journal == "oecol"] <- "university of innsbruck "
  both$inst[both$last_name == "weisser" & both$year == 2014 & both$journal == "oecol"] <- "technical university of munich"
  both$inst[both$last_name == "ziegler" & both$year == 1985 & both$journal == "oecol"] <- "technical university of munich"
  both$inst[both$last_name == "ziegler" & both$year == 2007 & both$journal == "oecol"] <- "technical university of munich"
  both$inst[both$last_name == "aarsen" & both$year == 2007 & both$journal == "oikos"] <- "queens university"
  both$inst[both$last_name == "aarsen" & both$year == 2014 & both$journal == "oikos"] <- "queens university"
  both$inst[both$last_name == "abbot" & both$year == 2013 & both$journal == "oikos"] <- "lund university"
  both$inst[both$last_name == "abbot" & both$year == 2014 & both$journal == "oikos"] <- "lund university"
  both$inst[both$last_name == "andersen" & both$year == 1985 & both$journal == "oikos"] <- "geological survey of denmark"
  both$inst[both$last_name == "andersen" & both$year == 1989 & both$journal == "oikos"] <- "geological survey of denmark"
  both$inst[both$last_name == "andersson" & both$year == 1985 & both$journal == "oikos"] <- "swedish university of agricultural sciences"
  both$inst[both$last_name == "andersson" & both$year == 1989 & both$journal == "oikos"] <- "swedish university of agricultural sciences"
  both$inst[both$last_name == "boomsma" & both$year == 2011 & both$journal == "oikos"] <- "university of copenhagen"
  both$inst[both$last_name == "dahl" & both$year == 1985 & both$journal == "oikos"] <- "agricultural university of norway"
  both$inst[both$last_name == "dahl" & both$year == 1989 & both$journal == "oikos"] <- "agricultural university of norway"
  both$inst[both$last_name == "grae" & both$year == 2012 & both$journal == "oikos"] <- "norwegian university of science and technology"
  both$inst[both$last_name == "grae" & both$year == 2014 & both$journal == "oikos"] <- "norwegian university of science and technology"
  both$inst[both$last_name == "jarvinen" & both$year == 1989 & both$journal == "oikos"] <- "university of helsinki"
  both$inst[both$last_name == "malian" & both$year == 2012 & both$journal == "oikos"] <- "swiss federal institute of aquatic science and technology"
  both$inst[both$last_name == "malian" & both$year == 2014 & both$journal == "oikos"] <- "swiss federal institute of aquatic science and technology"
  both$inst[both$last_name == "moore" & both$year == 2011 & both$journal == "oikos"] <- "university of california santa cruz"
  both$inst[both$last_name == "moore" & both$year == 2014 & both$journal == "oikos"] <- "university of california santa cruz"
  both$inst[both$last_name == "robinson" & both$year == 2011 & both$journal == "oikos"] <- "british trust for ornithology"
  both$inst[both$last_name == "robinson" & both$year == 2014 & both$journal == "oikos"] <- "british trust for ornithology"
  both$inst[both$last_name == "roy" & both$year == 2012 & both$journal == "oikos"] <- "evergreen state college"
  both$inst[both$last_name == "roy" & both$year == 2014 & both$journal == "oikos"] <- "evergreen state college"
  both$inst[both$last_name == "scherer_lorenzon" & both$year == 2014 & both$journal == "oikos"] <- "university of freiburg"
  both$inst[both$last_name == "solbrek" & both$year == 2011 & both$journal == "oikos"] <- "swedish university of agricultural sciences"
  both$inst[both$last_name == "sun" & both$year == 2014 & both$journal == "oikos"] <- "university of hong kong"
  both$inst[both$last_name == "traveser" & both$year == 2011 & both$journal == "oikos"] <- "mediterranean institute for advanced studies"
  both$inst[both$last_name == "traveser" & both$year == 2014 & both$journal == "oikos"] <- "mediterranean institute for advanced studies"
  both$inst[both$last_name == "bornkamm" & both$year == 1985 & both$journal == "plantecol"] <- "technical university of berlin"
  both$inst[both$last_name == "bornkamm" & both$year == 1989 & both$journal == "plantecol"] <- "technical university of berlin"
  both$inst[both$last_name == "epstein" & both$year == 2012 & both$journal == "plantecol"] <- "university of virginia"
  both$inst[both$last_name == "gimingham" & both$year == 1985 & both$journal == "plantecol"] <- "university of aberdeen"
  both$inst[both$last_name == "gimingham" & both$year == 1989 & both$journal == "plantecol"] <- "university of aberdeen"
  both$inst[both$last_name == "grubb" & both$year == 1985 & both$journal == "plantecol"] <- "university of cambridge"
  both$inst[both$last_name == "grubb" & both$year == 1986 & both$journal == "plantecol"] <- "university of cambridge"
  both$inst[both$last_name == "ne'eman" & both$year == 2012 & both$journal == "plantecol"] <- "university of haifa-oranim"
  both$inst[both$last_name == "oksanen" & both$year == 1989 & both$journal == "plantecol"] <- "university of eastern finland"
  both$inst[both$last_name == "rozema" & both$year == 2012 & both$journal == "plantecol"] <- "vrije universiteit amsterdam"
  both$inst[both$last_name == "walsh" & both$year == 2012 & both$journal == "plantecol"] <- "university of north carolina chapel hill"
  both$inst[both$last_name == "white" & both$year == 1985 & both$journal == "plantecol"] <- "university college dublin"
  both$inst[both$last_name == "white" & both$year == 1989 & both$journal == "plantecol"] <- "university college dublin"

  both$inst[both$editor_id == 1747 & both$year == 2011 & both$journal == "jbiog"] <- "university of oxford"

  both$inst[both$editor_id == 3892 & both$year == 1985] <- "louisiana state university"

  both$inst[both$editor_id == 716 & both$last_name == "hansen" & both$journal == "jbiog"] <- "university of zurich"
  both$country[both$editor_id == 716 & both$last_name == "hansen" & both$journal == "jbiog"] <- "switzerland"
  both$inst[both$editor_id == 1849 & both$inst == "university of stirling"] <- "vrije universiteit amsterdam"
  # both$inst[both$editor_id==105 & both$journal=="agronomy"]<-"california state university fresno"
  # both$inst[both$editor_id==1673 & both$year==2014]<-"california state university sacramento"

  both$editor_id[both$journal == "gcb" & both$last_name == "korner" & both$first_name == "christian"] <- 499
  both$inst[both$editor_id == 499 & both$year > 1989] <- "university of basel"
  both$city[both$city == "turtu" & both$editor_id == 3587] <- "turku"

  both$city[both$city == "universidade estadual paulista" & both$editor_id == 2383] <- "rio claro"
  both$city[both$city == "universidade estadual paulista" & both$editor_id == 1225] <- "sao paulo"

  both$inst[both$editor_id == 2358 & both$journal == "jane"] <- "university college cork"
  both$city[both$editor_id == 2358 & both$journal == "jane"] <- "cork"

  both$inst[both$editor_id == 1417 & both$year == "1992"] <- "usfs pacific southwest research station"

  both$inst[both$editor_id == 190] <- "kansas state university"

  both$inst[both$editor_id == 1763] <- "universidade de sao paulo"
  both$inst[both$editor_id == 2819] <- "universidade de sao paulo"

  both$state[both$city == "para"] <- "para"
  both$city[both$city == "para"] <- NA

  both$city[both$city == "washington"] <- "washington dc"
  both$city[both$city == "washington, dc"] <- "washington dc"
  both$city[both$city == "dc"] <- "washington dc"
  both$city[both$city == "district of columbia"] <- "washington dc"
  both$state[both$city == "washington dc"] <- "washington dc"

  both$city[both$city == "st louis"] <- "st louis"

  both$city[both$city == "fredricton"] <- "fredericton"


  both$country[both$editor_id == 3359 & both$year == 2014] <- "new zealand"


  both$country[both$country == "west germany"] <- "germany"


  colnames(both)
  both <- both %>%
    group_by(journal, editor_id, last_name, first_name) %>%
    arrange(year) %>%
    fill(inst, .direction = "down")


  both$first_name[both$last_name == "lack" & both$first_name == "alan" &
    both$middle_name == "j"] <- "andrew"


  both$first_name[both$last_name == "lomolino" & both$first_name == "m" &
    both$inst == "university of oklahoma"] <- "mark"

  both$middle_name[both$last_name == "lomolino" & both$first_name == "mark" &
    both$inst == "university of oklahoma"] <- "v"


  both$first_name[both$last_name == "houck" & both$first_name == "lynn" &
    both$journal == "amnat"] <- "lynne"


  both$first_name[both$last_name == "dustin" & both$first_name == "marshall" &
    both$journal == "evol" & both$year == 2014] <- "dustin"


  both$last_name[both$last_name == "hannson" & both$first_name == "l" & both$journal == "leco"] <- "hansson"
  both$first_name[both$last_name == "hansson" & both$first_name == "l" & both$journal == "leco"] <- "lennart"
  both$first_name[both$last_name == "wagner" & both$first_name == "h" & both$journal == "leco"] <- "helene"
  both$last_name[both$last_name == "aarsen" & both$editor_id == "2119"] <- "aarssen"
  both$last_name[both$last_name == "tilma" & both$editor_id == "891"] <- "tilman"
  both$first_name[both$editor_id == "570"] <- "christer"
  both$last_name[both$editor_id == "570"] <- "solbreck"
  both$unit[both$editor_id == "1605"] <- "citrus res & educ ctr"
  both$last_name[both$editor_id == "2760"] <- "ayres"
  both$last_name[both$last_name == "freckleto" & both$journal == "jape"] <- "freckelton"
  both$editor_id[both$last_name == "freckleto" & both$journal == "jape"] <- "3063"
  both$first_name[both$last_name == "boomsma" & both$journal == "oikos"] <- "jacobus"

  both$first_name[both$last_name == "ouborg" & both$journal == "jecol"] <- "n"
  both$middle_name[both$last_name == "ouborg" & both$journal == "jecol"] <- "joop"
  both$notes[both$last_name == "block" & both$editor_id == "3714"] <- "from 1993 record"
  both$last_name[both$editor_id == "283"] <- "graae"
  both$first_name[both$last_name == "ward" & both$journal == "oecol"] <- "joy"
  both$editor_id[both$last_name == "ward" & both$journal == "oecol"] <- "1938"
  both$city[both$last_name == "ward" & both$journal == "oecol"] <- "lawrence"
  both$last_name[both$editor_id == "283"] <- "graae"
  both$first_name[both$last_name == "ward" & both$journal == "oecol"] <- "joy"

  both$last_name[both$editor_id == "564"] <- "leroy"
  both$first_name[both$editor_id == "564"] <- "carri"
  both$middle_name[both$editor_id == "564"] <- NA
  both$last_name[both$editor_id == "2552"] <- "scherer-lorenzen"
  both$last_name[both$editor_id == "519"] <- "melian"
  both$first_name[both$editor_id == "1323"] <- "holger"
  both$last_name[both$editor_id == "1323"] <- "kreft"
  both$last_name[both$editor_id == "217"] <- "traveset"
  both$notes[both$editor_id == "102" & both$journal == "oecol"] <- "city listed as washington dc in frontmatter"
  both$first_name[both$editor_id == "2672" & both$journal == "biocon"] <- "n"
  both$middle_name[both$editor_id == "2672" & both$journal == "biocon"] <- "c"

  both$first_name[both$editor_id == "753" & both$journal == "biocon"] <- "d"
  both$middle_name[both$editor_id == "753" & both$journal == "biocon"] <- "j"

  both$last_name[both$editor_id == "2154" & both$journal == "jbiog"] <- "gillman"
  both$middle_name[both$first_name == "par" & both$last_name == "hockey"] <- "ar"
  both$first_name[both$first_name == "par" & both$last_name == "hockey"] <- "p"

  both$inst[(both$editor_id == "3252" | both$editor_id == "453" | both$editor_id == "1973") & both$inst == "queensland"] <- "university of queensland"
  both$inst[both$last_name == "vellend" & both$journal == "oikos"] <- "cornell university"
  both$inst[both$inst == "wyoming" & both$last_name == "benkman"] <- "university of wyoming"
  both$inst[both$journal == "funecol" & both$last_name == "blanckenhorn"] <- "university of zurich irchel"
  both$city[both$last_name == "hixon" & both$inst == "university of hawaii"] <- "honolulu"
  both$inst[both$inst == "zurich" & both$last_name == "tschirren"] <- "university of zurich"
  both$inst[both$inst == "vermont" & both$last_name == "brody"] <- "university of vermont"
  both$inst[both$inst == "utah" & both$last_name == "ehleringer"] <- "university of utah"
  both$inst[both$inst == "utah" & both$last_name == "caldwell"] <- "utah state university"
  both$inst[both$last_name == "herrera" & both$inst == "csic consejo superior de investigaciones cientificas"] <- "csic donana biological station"
  both$inst[both$last_name == "herrera" & both$first_name == "carlos"] <- "csic donana biological station"
  both$inst[both$last_name == "bascompte" & both$first_name == "jordi"] <- "csic donana biological station"
  both$inst[both$last_name == "herrera" & both$first_name == "carlos"] <- "csic donana biological station"
  both$inst[both$editor_id == 878 & both$inst == "csic consejo superior de investigaciones cientificas"] <- "csic donana biological station"
  both$inst[both$editor_id == 1494 & both$journal == "jape"] <- "csic donana biological station"
  both$inst[both$editor_id == 3320 & both$journal == "amnat"] <- "csic uv instituto de biología integrativa de sistemas"
  both$inst[both$editor_id == 2522 & both$journal == "oecol"] <- "csic zaidin experimental station"
  both$inst[both$last_name == "jordano"] <- "csic donana biological station"
  both$unit[both$last_name == "jordano" & both$inst == "csic donana biological station"] <- "csic donana biological station"

  both$country[both$last_name == "laaksonen" & both$journal == "oecol"] <- "finland"
  both$inst[both$last_name == "angilletta" & both$inst == "indiana"] <- "indiana state university"
  both$inst[both$last_name == "reynolds" & both$inst == "indiana"] <- "indiana university bloomington"
  both$inst[both$last_name == "pienkowski" & both$country == "united kingdom"] <- "joint nature conservation committee"

  both$inst[both$last_name == "cavers" & both$inst == "nerc centre for ecology and hydrology"] <- "nerc centre for ecology and hydrology edinburgh"
  both$inst[both$last_name == "chapman" & both$inst == "nerc centre for ecology and hydrology"] <- "nerc centre for ecology and hydrology edinburgh"
  both$inst[both$city == "wallingford" & both$inst == "nerc centre for ecology and hydrology"] <- "nerc centre for ecology and hydrology wallingford"
  both$inst[both$city == "bailrigg" & both$inst == "nerc centre for ecology and hydrology"] <- "nerc centre for ecology and hydrology bailrigg"


  both$inst[both$last_name == "croxall"] <- "nerc british antarctic survey"
  both$inst[both$last_name == "pywell" & both$unit == "center for ecology and hydrology"] <- "nerc centre for ecology and hydrology wallingford"


  both$inst[both$last_name == "harwood" & both$inst == "nerc natural environment research council"] <- "nerc sea mammal research unit"

  both$inst[both$last_name == "montevecchi"] <- "memorial university of newfoundland"
  both$inst[both$editor_id == 3772] <- "memorial university of newfoundland"

  both$inst[both$editor_id == 1158] <- "university of georgia caes griffin campus"
  both$unit[both$editor_id == 1158] <- "caes griffin campus-ag experiment station"


  both$inst[both$editor_id == 1410] <- "universite montpellier 2"

  both$inst[both$last_name == "hauber" & both$year == 2017] <- "cuny hunter college"

  both$inst[both$editor_id == 1972 & both$year == 2004] <- "kansas state university"
  both$last_name[both$editor_id == 1972 & both$year == 2004] <- "with"
  both$middle_name[both$editor_id == 1972 & both$year == 2004] <- "a"




  both$last_name[both$last_name == "van der maarel" & both$journal == "leco"] <- "vandermaarel"
  both$last_name[both$last_name == "vander" & both$journal == "leco"] <- "vandermaarel"
  both$inst[both$last_name == "vandermaarel" & both$journal == "leco"] <- "university of uppsala"

  both$inst[both$last_name == "cymerman" & both$journal == "leco"] <- "university of georgia"
  both$city[both$last_name == "cymerman" & both$journal == "leco"] <- "athens"
  both$last_name[both$last_name == "cymerman" & both$journal == "leco"] <- "hepinstall-cymerman"


  both$inst[both$editor_id == 381 & both$journal == "leco" & both$year == 2015] <- "leibniz university hannover"
  both$inst[both$editor_id == 1480 & both$journal == "leco" & both$year == 2004] <- "roskilde university"
  both$inst[both$editor_id == 2316 & both$journal == "leco" & both$year == 2015] <- "swiss federal institute for forest snow and landscape research wsl"
  both$inst[both$editor_id == 1990 & both$journal == "leco" & both$year == 2015] <- "michigan state university"
  both$inst[both$editor_id == 1520 & both$journal == "leco" & both$year == 2015] <- "north carolina state university"
  both$inst[both$editor_id == 448 & both$journal == "leco" & both$year == 2004] <- "usfs rocky mountain research station"
  both$inst[both$editor_id == 3067 & both$journal == "leco" & both$year == 1997] <- "appalachian environmental laboratory"
  both$inst[both$editor_id == 2387 & both$journal == "leco" & (both$year >= 1987 & both$year <= 1992)] <- "institut botanique"
  # both$inst[both$editor_id==2387 & both$journal=="leco" & both$year==1992]<-"institut botanique"
  both$inst[both$editor_id == 1192 & both$journal == "leco" & (both$year >= 1987 & both$year <= 1996)] <- "institute geography and geoecology"
  # both$inst[both$editor_id==1192 & both$journal=="leco" & both$year==1996]<-"institute geography and geoecology"
  both$inst[both$editor_id == 3744 & both$journal == "leco" & (both$year >= 1987 & both$year <= 1997)] <- "technical university of munich"
  # both$inst[both$editor_id==3744 & both$journal=="leco" & both$year==1997]<-"technical university of munich"
  both$inst[both$editor_id == 1080 & both$journal == "leco" & both$year == 2015] <- "agroscope"
  both$country[both$editor_id == 1080 & both$journal == "leco" & both$year == 2015] <- "netherlands"
  both$inst[both$editor_id == 492 & both$journal == "leco" & both$year == 1997] <- "south dakota state university"
  both$inst[both$editor_id == 1089 & both$journal == "leco" & both$year == 2004] <- "swiss federal institute for forest snow and landscape research wsl"
  both$inst[both$editor_id == 3126 & both$journal == "leco" & both$year == 2015] <- "university of bari"
  both$inst[both$editor_id == 3592 & both$journal == "leco" & both$year == 2015] <- "university of richmond"
  both$last_name[both$editor_id == 3592 & both$journal == "leco" & both$year == 2015] <- "lookingbill"
  both$inst[both$editor_id == 2674 & both$journal == "leco" & (both$year >= 1993 & both$year <= 1997)] <- "hiroshima university"
  # both$inst[both$editor_id==2674 & both$journal=="leco" & both$year==1997]<-"hiroshima university"
  both$inst[both$editor_id == 1781 & both$journal == "leco" & both$year == 2004] <- "university of michigan"
  both$inst[both$editor_id == 2885 & both$journal == "leco" & (both$year >= 1993 & both$year <= 1997)] <- "institute for forestry and nature research"
  # both$inst[both$editor_id==2885 & both$journal=="leco" & both$year<=1997]<-"institute for forestry and nature research"
  both$inst[both$editor_id == 2885 & both$journal == "leco" & both$year == 2004] <- "alterra research institute for the green world"
  both$inst[both$editor_id == 177 & both$journal == "leco" & (both$year >= 1987 & both$year <= 1997)] <- "ets ingenieros de montes"
  # both$inst[both$editor_id==177 & both$journal=="leco" & both$year==1997]<-"ets ingenieros de montes"
  both$inst[both$editor_id == 2907 & both$journal == "leco" & (both$year >= 1987 & both$year <= 1992)] <- "university of new mexico"
  # both$inst[both$editor_id==2907 & both$journal=="leco" & both$year==1992]<-"university of new mexico"
  both$inst[both$editor_id == 3791 & both$journal == "leco" & both$year == 2004] <- "colorado state university"
  both$inst[both$editor_id == 2546 & both$journal == "leco" & (both$year >= 1987 & both$year <= 1997)] <- "center of biological and ecological sciences"
  # both$inst[both$editor_id==2546 & both$journal=="leco" & both$year==1997]<-"center of biological and ecological sciences"
  both$inst[both$editor_id == 571 & both$journal == "leco" & (both$year >= 1987 & both$year <= 1992)] <- "harvard university"
  # both$inst[both$editor_id==571 & both$journal=="leco" & both$year==1992]<-"harvard university"
  both$inst[both$editor_id == 1906 & both$journal == "leco" & both$year == 2015] <- "harvard university"
  both$inst[both$editor_id == 371 & both$journal == "leco" & both$year == 2013] <- "arizona state university"
  both$inst[both$editor_id == 2580 & both$journal == "leco"] <- "university of wisconsin madison"
  both$inst[both$editor_id == 3756 & both$journal == "funecol"] <- "university of wisconsin madison"
  both$inst[both$editor_id == 111 & both$journal == "ecology"] <- "university of wisconsin madison"
  both$inst[both$editor_id == 111 & both$journal == "amnat"] <- "university of wisconsin madison"
  both$inst[both$editor_id == 2159 & both$journal == "ajb"] <- "university of wisconsin madison"
  both$inst[both$editor_id == 955 & both$journal == "jecol"] <- "university of wisconsin madison"
  both$inst[both$editor_id == 3294 & both$journal == "amnat"] <- "university of wisconsin madison"
  both$inst[both$editor_id == 1661 & both$journal == "oecol"] <- "university of wisconsin madison"
  both$inst[both$editor_id == 1815 & both$journal == "funecol"] <- "university of wisconsin madison"
  both$inst[both$editor_id == 874 & both$journal == "ajb"] <- "university of wisconsin madison"
  both$inst[both$editor_id == 1134 & both$journal == "jbiog"] <- "university of wisconsin madison"
  both$inst[both$editor_id == 901 & both$journal == "oecol"] <- "university of wisconsin madison"

  both$inst[both$editor_id == 3912 & both$journal == "leco" & (both$year >= 1993 & both$year <= 1997)] <- "oregon state university"
  # both$inst[both$editor_id==3912 & both$journal=="leco" & both$year==1997]<-"oregon state university"
  both$inst[both$editor_id == 602 & both$journal == "leco" & both$year == 2015] <- "university of wolverhampton"
  both$inst[both$editor_id == 3824 & both$journal == "leco" & both$year == 2015] <- "chinese academy of sciences"
  both$inst[both$editor_id == 1428 & both$journal == "leco" & (both$year >= 1987 & both$year <= 1992)] <- "international institute for aerospace survey and earth sciences"
  # both$inst[both$editor_id==1428 & both$journal=="leco" & both$year==1992]<-"international institute for aerospace survey and earth sciences"
  both$inst[both$editor_id == 1045 & both$journal == "leco" & (both$year >= 1987 & both$year <= 1992)] <- "university of arizona"
  # both$inst[both$editor_id==1045 & both$journal=="leco" & both$year==1992]<-"university of arizona"
  both$inst[both$editor_id == 3669 & both$journal == "oecol" & both$year == 2013] <- "estonian university of life sciences"

  both$first_name[both$last_name == "barry" & both$journal == "marecol" & (both$year == 2006 | both$year == 2005)] <- "j"
  both$middle_name[both$last_name == "barry" & both$journal == "marecol" & (both$year == 2006 | both$year == 2005)] <- "p"


  both$first_name[both$last_name == "cadena" & both$journal == "condor"] <- "c"
  both$middle_name[both$last_name == "cadena" & both$journal == "condor"] <- "daniel"
  both$last_name[both$last_name == "cadenaordonez" & both$journal == "auk"] <- "cadena"
  both$first_name[both$last_name == "cadena" & both$journal == "auk"] <- "c"
  both$middle_name[both$last_name == "cadena" & both$journal == "auk"] <- "daniel"

  both$first_name[both$last_name == "arnold" & both$first_name == "w" & both$journal == "auk"] <- "todd"
  both$middle_name[both$last_name == "arnold" & both$first_name == "todd" & both$journal == "auk"] <- "w"

  both$first_name[both$last_name == "dumbacher" & both$journal == "auk"] <- "john"


  both$inst[both$editor_id == 898 & both$inst == "conicet consejo nacional de investigaciones cientificas y tecnicas"] <- "conicet cct mendoza"
  both$inst[both$last_name == "areta" & both$inst == "conicet consejo nacional de investigaciones cientificas y tecnicas"] <- "conicet ibigeo"
  both$inst[both$editor_id == 2340 & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "csiro sustainable ecosystems"
  both$inst[both$editor_id == 1693 & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "csiro forest research"
  both$inst[both$editor_id == 196 & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "csiro entomology"
  both$inst[both$last_name == "thrall" & both$year == 2010] <- "csiro plant industry"
  both$inst[both$last_name == "rothstein" & both$inst == "university of california"] <- "university of california santa barbara"
  both$inst[both$last_name == "powell" & both$inst == "university of alaska"] <- "university of alaska fairbanks"
  both$inst[both$editor_id == 2281 & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "csiro land and water"
  # both$inst[both$editor_id==2281 & both$journal=="jecol" & (both$year>1987 & both$year<1994)]<-"csiro land and water"
  both$inst[both$editor_id == 2281 & both$unit == "division of wildlife and ecology" & both$city == "lyneham"] <- "csiro wildlife and ecology"
  both$inst[both$editor_id == 2281 & both$city == "lyneham"] <- "csiro wildlife and ecology"
  both$inst[both$last_name == "austin" & both$city == "lyneham"] <- "csiro wildlife and ecology"
  both$inst[both$last_name == "austin" & both$journal == "jecol" & (both$year > 1987 & both$year < 1994)] <- "csiro wildlife and ecology"
  # both$inst[both$editor_id==2281 & (both$year==1988 | both$year==1989) & both$journal=="jecol"]<-"csiro wildlife and ecology"
  both$inst[both$editor_id == 2868 & both$inst == "csiro commonwealth scientific and industrial research organisation" & both$city == "canberra"] <- "csiro land and water"
  both$inst[both$editor_id == 2281 & both$city == "canberra"] <- "csiro land and water"
  both$inst[both$editor_id == 2281 & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "csiro land and water"
  both$inst[both$editor_id == 3598 & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "csiro sustainable ecosystems"
  both$inst[both$editor_id == 392 & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "csiro forest research"
  both$inst[both$editor_id == 1160 & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "csiro wildlife and ecology"
  both$inst[both$editor_id == 3091 & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "csiro wildlife and ecology"
  both$inst[both$editor_id == 911 & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "csiro sustainable ecosystems"
  both$inst[both$editor_id == 1094 & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "csiro ecosystem sciences"
  both$inst[both$last_name == "doerr" & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "csiro ecosystem sciences"
  both$inst[both$editor_id == 1493 & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "csiro forest research"
  both$inst[both$editor_id == 2867 & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "university of adelaide"
  both$inst[both$editor_id == 3091 & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "csiro wildlife and ecology"
  both$inst[both$editor_id == 2340 & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "csiro computing research"
  both$inst[both$editor_id == 2699 & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "csiro plant industry"
  both$inst[both$last_name == "thrall" & both$inst == "csiro commonwealth scientific and industrial research organisation"] <- "csiro plant industry"
  both$inst[both$editor_id == 342 & both$inst == "james cook university"] <- "james cook university brisbane"
  both$inst[both$editor_id == 1561 & both$inst == "james cook university"] <- "james cook university townsville"
  both$inst[both$editor_id == 3027 & both$inst == "james cook university"] <- "james cook university townsville"
  both$inst[both$last_name == "crozier" & both$inst == "james cook university"] <- "james cook university townsville"
  both$inst[both$editor_id == 3305 & both$inst == "james cook university"] <- "james cook university townsville"
  both$inst[both$editor_id == 2745 & both$inst == "james cook university"] <- "james cook university brisbane"
  both$inst[both$editor_id == 621 & both$inst == "james cook university"] <- "james cook university townsville"
  both$inst[both$editor_id == 2971 & both$inst == "james cook university"] <- "james cook university townsville"
  both$inst[both$editor_id == 3857 & both$inst == "james cook university"] <- "james cook university townsville"
  both$inst[both$editor_id == 2095 & both$inst == "north carolina"] <- "university of north carolina chapel hill"
  both$inst[both$last_name == "petit" & both$inst == "smithsonian institution"] <- "smithsonian national zoological park"
  both$inst[both$last_name == "fleischer" & both$inst == "smithsonian institution"] <- "smithsonian national zoological park"
  both$inst[both$editor_id == 323 & both$inst == "smithsonian institute"] <- "smithsonian institution national museum of natural history"
  both$inst[both$editor_id == 3593 & both$inst == "smithsonian institute"] <- "smithsonian institution"
  both$inst[both$last_name == "sedinger" & both$inst == "university of alaska"] <- "university of alaska fairbanks"
  both$inst[both$editor_id == 3207 & both$inst == "university of alaska"] <- "university of alaska fairbanks"
  both$inst[both$editor_id == 1062 & both$inst == "university of alaska"] <- "university of alaska fairbanks"
  both$inst[both$editor_id == 1062 & both$inst == "university of california"] <- "university of california berkeley"
  both$inst[both$editor_id == 1561 & both$inst == "university of california"] <- "university of california santa barbara"
  both$inst[both$editor_id == 1871 & both$inst == "university of california"] <- "university of california los angeles"
  both$inst[both$editor_id == 1886 & both$inst == "university of california"] <- "university of california davis"
  both$inst[both$editor_id == 3330 & both$inst == "university of california"] <- "university of california irvine"
  both$inst[both$editor_id == 3333 & both$inst == "university of california"] <- "university of california santa barbara"
  both$inst[both$editor_id == 3444 & both$inst == "university of california"] <- "university of california santa barbara"
  both$inst[both$editor_id == 3786 & both$inst == "university of california"] <- "university of california santa barbara"
  both$inst[both$editor_id == 3804 & both$inst == "university of california"] <- "university of california berkeley"
  both$inst[both$last_name == "eadie" & both$inst == "university of california"] <- "university of california davis"
  both$inst[both$editor_id == 566 & both$inst == "university of california"] <- "university of california davis"
  both$inst[both$editor_id == 1655 & both$inst == "university of california"] <- "university of california danr"
  both$inst[both$editor_id == 1220 & both$inst == "university of california"] <- "university of california los angeles"
  both$inst[both$editor_id == 2058 & both$inst == "university of california"] <- "university of california los angeles"
  both$inst[both$editor_id == 2525 & both$inst == "university of california"] <- "university of california riverside"
  both$inst[both$editor_id == 2411 & both$inst == "university of hawaii"] <- "university of hawaii manoa"
  both$inst[both$editor_id == 672 & both$inst == "university of hawaii"] <- "university of hawaii manoa"
  both$inst[both$editor_id == 2097 & both$inst == "university of massachusetts"] <- "university of massachusetts amherst"
  both$inst[both$last_name == "kroodsma" & both$inst == "university of massachusetts"] <- "university of massachusetts amherst"
  both$inst[both$last_name == "byers" & both$inst == "university of massachusetts"] <- "university of massachusetts amherst"
  both$inst[both$editor_id == 1435 & both$inst == "university of massachusetts"] <- "university of massachusetts amherst"
  both$inst[both$editor_id == 1025 & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$editor_id == 1799 & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$editor_id == 2035 & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$editor_id == 3218 & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$editor_id == 972 & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$editor_id == 891 & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$editor_id == 3360 & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$last_name == "arnold" & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$editor_id == 1817 & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$editor_id == 48 & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$editor_id == 936 & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$editor_id == 1506 & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$editor_id == 52 & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$editor_id == 792 & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$editor_id == 1781 & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$editor_id == 1200 & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$editor_id == 2872 & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$editor_id == 2905 & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$last_name == "klicka" & both$inst == "university of nevada"] <- "university of nevada las vegas"
  both$inst[both$editor_id == 2253 & both$inst == "university of nevada"] <- "university of nevada las vegas"
  both$inst[both$editor_id == 2352 & both$inst == "university of north carolina"] <- "university of north carolina chapel hill"
  both$inst[both$editor_id == 820 & both$inst == "university of north carolina"] <- "university of north carolina chapel hill"
  both$inst[both$editor_id == 3200 & both$inst == "university of south carolina"] <- "university of south carolina columbia"
  both$inst[both$editor_id == 1749 & both$inst == "university of texas"] <- "university of texas austin"
  both$inst[both$editor_id == 3137 & both$inst == "university of texas"] <- "university of texas austin"
  both$inst[both$editor_id == 3535 & both$inst == "university of texas"] <- "university of texas austin"
  both$inst[both$editor_id == 723 & both$inst == "university of texas"] <- "university of texas austin"
  both$inst[both$editor_id == 1691 & both$inst == "university of texas"] <- "university of texas san antonio"
  both$inst[both$editor_id == 3229 & both$inst == "university of washington"] <- "university of washington seattle"
  both$inst[both$editor_id == 3179 & both$inst == "university of washington"] <- "university of washington seattle"
  both$inst[both$editor_id == 3487 & both$inst == "university of washington"] <- "university of washington seattle"
  both$inst[both$editor_id == 858 & both$inst == "university of washington"] <- "university of washington seattle"
  both$inst[both$editor_id == 3183 & both$inst == "university of washington"] <- "university of washington seattle"
  both$inst[both$editor_id == 3001 & both$inst == "university of washington"] <- "university of washington seattle"
  both$inst[both$editor_id == 1213 & both$inst == "university of washington"] <- "university of washington seattle"
  both$inst[both$editor_id == 1668 & both$inst == "university of washington"] <- "university of washington seattle"
  both$inst[both$editor_id == 590 & both$inst == "usda us department of agriculture"] <- "usda ars"
  both$inst[both$editor_id == 1799 & both$inst == "usfs us forest service"] <- "usfs research and development"
  both$inst[both$editor_id == 467 & both$inst == "usfs us forest service"] <- "usfs southern research station"
  both$inst[both$editor_id == 1327 & both$inst == "usfs us forest service"] <- "usfs southern research station"
  both$inst[both$editor_id == 446 & both$inst == "usfs us forest service"] <- "usfs pacific southwest research station"
  both$inst[both$editor_id == 3341 & both$inst == "usfs us forest service"] <- "usfs pacific southwest research station"
  both$inst[both$editor_id == 2178 & both$inst == "usfs us forest service"] <- "usfs northern research station"
  both$inst[both$editor_id == 2170 & both$inst == "usfs us forest service"] <- "usfs northern research station"
  both$inst[both$editor_id == 724 & both$inst == "usfs us forest service"] <- "usfs northern research station"
  both$inst[both$editor_id == 798 & both$inst == "usfs us forest service"] <- "usfs rocky mountain research station"
  both$inst[both$editor_id == 3310 & both$inst == "usfs us forest service"] <- "usfs rocky mountain research station"
  both$inst[both$editor_id == 1783 & both$inst == "usfs us forest service"] <- "usfs rocky mountain research station"
  both$inst[both$editor_id == 448 & both$inst == "usfs us forest service"] <- "usfs rocky mountain research station"
  both$inst[both$editor_id == 2613 & both$inst == "usfs us forest service"] <- "usfs pacific northwest research station"
  both$inst[both$editor_id == 2555 & both$inst == "usfs us forest service"] <- "usfs rocky mountain research station"
  both$inst[both$editor_id == 3310 & both$inst == "usfs us forest service"] <- "usfs rocky mountain research station"
  both$inst[both$editor_id == 135 & both$inst == "usfs us forest service"] <- "usfs northern research station"
  both$inst[both$editor_id == 448 & both$inst == "usfs us forest service"] <- "usfs rocky mountain research station"
  both$inst[both$last_name == "monahan" & both$inst == "usfs us forest service"] <- "usfs rocky mountain research station"
  both$inst[both$editor_id == 766 & both$inst == "usfs us forest service"] <- "usfs northern research station"
  both$inst[both$editor_id == 1566 & both$inst == "usfs us forest service"] <- "usfs institute of pacific islands dorestry"
  both$inst[both$editor_id == 1107 & both$inst == "usfs us forest service"] <- "usfs pacific northwest research station"
  both$inst[both$editor_id == 2178 & both$inst == "usfs us forest service"] <- "usfs northern research station"
  both$inst[both$editor_id == 2075 & both$inst == "usfs us forest service"] <- "usfs southern research station"
  both$inst[both$editor_id == 671 & both$inst == "usfs us forest service"] <- "usfs northern research station"
  both$inst[both$editor_id == 975 & both$inst == "usfs us forest service"] <- "usfs northern research station"
  both$inst[both$editor_id == 1127 & both$inst == "us forest serv"] <- "usfs international institute of tropical forestry"
  both$inst[both$editor_id == 139 & both$inst == "usfs us forest service"] <- "usfs international institute of tropical forestry"
  both$inst[both$editor_id == 909 & both$inst == "usfs us forest service"] <- "usfs pacific southwest research station"
  both$inst[both$editor_id == 139 & both$inst == "usfs us forest service"] <- "usfs international institute of tropical forestry"
  both$inst[both$editor_id == 1580 & both$inst == "usfs us forest service"] <- "usfs pacific northwest research station"
  both$inst[both$editor_id == 341 & both$inst == "usfs us forest service"] <- "usfs northern research station"
  both$inst[both$editor_id == 1255 & both$inst == "usfs us forest service"] <- "usfs southern research station"
  both$inst[both$editor_id == 1909 & both$inst == "usfs us forest service"] <- "usfs southern research station"
  both$inst[both$editor_id == 3383 & both$inst == "usfs us forest service"] <- "usfs southern research station"
  both$inst[both$editor_id == 1604 & both$inst == "usgs united states geological survey"] <- "usgs national wetlands research center"
  both$inst[both$editor_id == 1786 & both$inst == "usgs united states geological survey"] <- "usgs patuxent wildlife research center"
  both$inst[both$editor_id == 663 & both$inst == "usgs united states geological survey"] <- "usgs national wetlands research center"
  both$inst[both$editor_id == 663 & both$journal == "amnat" & (both$year > 2005 & both$year < 2010)] <- "usgs national wetlands research center"
  both$inst[both$editor_id == 1604 & both$inst == "usgs united states geological survey"] <- "usgs national wetlands research center"
  both$inst[both$last_name == "piatt" & both$inst == "usgs united states geological survey"] <- "usgs alaska science center"
  both$inst[both$last_name == "weathers" & both$inst == "university of california"] <- "university of california davis"
  both$inst[both$last_name == "gutierrez" & both$inst == "university of minnesota"] <- "university of minnesota twin cities"
  both$inst[both$editor_id == 1854 & both$inst == "usgs united states geological survey"] <- "usgs patuxent wildlife research center"
  both$inst[both$editor_id == 2037 & both$inst == "usgs united states geological survey"] <- "usgs western ecological research center"
  both$inst[both$editor_id == 2037 & both$journal == "ecology" & (both$year > 2005 & both$year < 2016)] <- "usgs western ecological research center"
  both$inst[both$last_name == "lafferty" & both$first_name == "kevin" & both$year == 2005] <- "usgs western ecological research center"
  both$inst[both$editor_id == 3643 & both$inst == "usgs united states geological survey"] <- "usgs fort collins science center"
  both$inst[both$editor_id == 1848 & both$inst == "usgs united states geological survey"] <- "usgs patuxent wildlife research center"
  both$inst[both$editor_id == 486 & both$inst == "usgs united states geological survey"] <- "usgs florence bascom geoscience center"


  both$inst[both$last_name == "niewiarowski" & both$inst == "ohio"] <- "university of akron"
  both$inst[both$editor_id == 1254 & both$inst == "ohio"] <- "ohio university"
  both$inst[both$editor_id == 2835] <- "pennsylvania state university"
  both$inst[both$editor_id == 3101] <- "university of washington seattle"
  both$inst[both$editor_id == 1612] <- "university of texas arlington"
  both$inst[both$editor_id == 697] <- "suny stony brook"
  both$inst[both$editor_id == 3069] <- "suny stony brook"
  both$inst[both$editor_id == 1364 & both$year == 2000] <- "university of freiburg"
  both$first_name[both$editor_id == 1364] <- "hanns-christof"

  both$inst[both$editor_id == 1278] <- "suny stony brook"
  both$inst[both$editor_id == 3807] <- "suny binghamton"
  both$inst[both$editor_id == 1404] <- "finnish game and fisheries research institute"
  both$unit[both$editor_id == 1404] <- "oulu game and fisheries research"




  both$inst[both$last_name == "hepinstall-cymerman" & both$journal == "leco"] <- "university of georgia"
  both$inst[both$last_name == "cymerman" & both$journal == "leco"] <- "university of georgia"
  both$city[both$last_name == "cymerman" & both$journal == "leco"] <- "athens"
  both$inst[both$last_name == "tjoelker" & both$journal == "funecol" & both$year > 2011] <- "western sydney university"
  both$inst[both$inst == "carnegie institution" & both$journal == "gcb"] <- "carnegie institution of washington"


  both$inst[both$inst == "national autonomous universidad nacional autonoma de mexico"] <- "universidad nacional autonoma de mexico"


  both$inst[(both$editor_id == 500 | both$editor_id == 876 | both$editor_id == 1125 |
    both$editor_id == 1779 | both$editor_id == 2295 | both$editor_id == 2607 |
    both$editor_id == 2841 | both$editor_id == 3202 | both$editor_id == 3234 |
    both$editor_id == 3342 | both$editor_id == 3425 | both$editor_id == 7) &
    both$inst == "university of british columbia"] <- "university of british columbia vancouver"


  both$inst[both$last_name == "martin" &
    both$first_name == "kathy" &
    both$inst == "university of british columbia"] <- "university of british columbia vancouver"











  both$first_name[both$editor_id == 3378] <- "s"



  # weisser vs weiser


  both$inst[both$last_name == "weisser" & both$journal == "oecol" &
    (both$year > 2006 & both$year < 2014)] <- "university of jena"
  both$editor_id[both$last_name == "weisser" & both$journal == "oecol" &
    (both$year > 2006 & both$year < 2015)] <- NA
  #
  # both$last_name[both$last_name=="de kroon" & both$first_name=="hans"]<-"hans"
  #
  # both$last_name[both$last_name=="van groenendael" & both$first_name=="jan"]<-"vangroenendael"
  #
  # both$last_name[both$last_name=="van der meijden" & both$first_name=="eddy"]<-"vandermeijden"
  #
  # both$last_name[both$last_name=="van der meijden" & both$first_name=="eddy"]<-"vandermeijden"
  #
  # both$last_name[both$last_name=="van der meijden" & both$first_name=="eddy"]<-"vandermeijden"

  both$last_name <- gsub("[[:space:]]", "", both$last_name)





  both$last_name[both$last_name == "weisser" & both$journal == "oecol" & (both$year > 1983 & both$year < 2007)] <- "weiser"
  both$middle_name[both$last_name == "weiser" & both$journal == "oecol" &
    both$year < 2007] <- NA

  both <- both[!(both$editor_id == "3798" & both$journal == "oecol" & both$last_name == "weiser" & both$year > 2006), ]
  both <- both[!(both$last_name == "mason" & both$first_name == "christoph" & both$journal == "jape"), ]
  both <- both[!(both$last_name == "fernande-juricic" & both$journal == "jape" & both$year == 2010), ]
  both <- both[!(both$editor_id == "1261" & both$journal == "oecol" & both$year == 1999), ]
  both <- both[!(both$editor_id == "3669" & both$journal == "oecol" & both$year == 2012), ]
  both <- both[!(both$editor_id == "2069" & both$journal == "plantecol" & both$year == 2007), ]
  both <- both[!(both$editor_id == "1079" & both$journal == "plantecol" & both$year == 2007), ]
  both <- both[!(both$editor_id == "100" & both$journal == "plantecol" & both$year == 1991), ]
  both <- both[!(both$editor_id == "2896" & both$journal == "plantecol" & both$year == 2004), ]
  both <- both[!(both$editor_id == "1057" & both$journal == "biocon" & both$year == 1985), ]
  both <- both[!(both$editor_id == "56" & both$journal == "biocon" & both$year == 1985), ]
  both <- both[!(both$editor_id == "2914" & both$journal == "biocon" & both$year == 1985), ]
  both <- both[!(both$editor_id == "753" & both$journal == "biocon" & both$year == 1985), ]
  both <- both[!(both$editor_id == "56" & both$journal == "biocon" & both$year == 1986), ]
  both <- both[!(both$editor_id == "3355" & both$journal == "biocon" & both$year == 1998), ]
  both <- both[!(both$editor_id == "1402" & both$journal == "oecol" & both$year == 2012), ]
  both <- both[!(both$editor_id == "3944" & both$journal == "oecol" & both$year == 1992), ]
  both <- both[!(both$editor_id == "1819" & both$journal == "bitr" & both$year == 1997), ]
  both <- both[!(both$editor_id == "591" & both$journal == "evol" & both$year == 2015), ]
  both <- both[!(both$editor_id == "2047" & both$journal == "jane" & both$year == 2012), ]
  both <- both[!(is.na(both$editor_id) & both$journal == "jape" & both$last_name == "croxall"), ]
  both <- both[!(both$editor_id == "3584" & both$journal == "leco" & both$year == 2015), ]
  # on advisory board but not ed board
  both <- both[!(both$editor_id == "371" & both$journal == "leco" & both$year == 2012), ]
  both <- both[!(both$editor_id == "371" & both$journal == "leco" & both$year == 2013), ]
  both <- both[!(both$editor_id == "23" & both$journal == "oecol" & both$year == 2011), ]
  both <- both[!(both$last_name == "banks-lei" & both$journal == "jape" & both$year == 2013), ]



  # country city unit corrections
  both$city[both$unit == "ontario canada"] <- "gainesville"
  both$state[both$unit == "ontario canada"] <- "fl"
  both$unit[both$unit == "ontario canada"] <- "dept. of biology"
  both$country[both$inst == "university of florida"] <- "usa"
  both$country[both$city == "latvia"] <- "latvia"
  both$city[both$city == "latvia"] <- NA
  both$city[both$city == "ann arbon"] <- "ann arbor"

  both$city <- gsub("st. ", "st ", both$city)
  both$city <- gsub("saint ", "st ", both$city)
  both$inst <- gsub("st. ", "st ", both$inst)
  both$inst <- gsub("saint ", "st ", both$inst)
  both$city <- gsub("saint ", "st ", both$city)
  both$city <- gsub("ft. ", "fort ", both$city)
  both$city <- gsub("ft c", "fort c ", both$city)

  # both$city<-gsub("w\x9frzburg","wurzburg",both$city)
  # both$city<-gsub("k\x9aln","cologne",both$city)
  # both$city<-gsub("m\x9fnchen","munich",both$city)
  # both$city<-gsub("g\x9attingen","gottingen",both$city)
  # both$city<-gsub("z\x81rich","zurich",both$city)

  # both$city<-tolower(both$city)

  both$city <- gsub("edinburgh eh9 3jz", "edinburgh", both$city)
  both$city <- gsub("quebec city", "quebec", both$city)
  both$city <- gsub("fuzerbrook", "furzebrook", both$city)
  both$city <- gsub("e lansing", "east lansing", both$city)
  both$city <- gsub("hickory coners", "hickory corners", both$city)
  both$city <- gsub("rodenbosch", "rondebosch", both$city)
  both$city <- gsub("rodenbosch", "rondebosch", both$city)
  both$city <- gsub("santa cruz, california, usa", "santa cruz", both$city)
  both$city <- gsub("auburn, alabama, usa", "auburn", both$city)
  both$city <- gsub("cambridge, massachusetts, usa", "cambridge", both$city)
  both$city <- gsub("sheffield s10 2tn", "sheffield", both$city)
  both$city <- gsub("mississippi state", "", both$city)
  both$city <- gsub("storrs", "storrs", both$city)
  both$city <- gsub("starrs", "storrs", both$city)
  both$city <- gsub("champaign", "urbana-champaign", both$city)
  both$city <- gsub("urbana", "urbana-champaign", both$city)
  both$city <- gsub("guleph", "guelph", both$city)
  both$city <- gsub("goettingen", "gottingen", both$city)
  both$city <- gsub("osnabrück", "osnabruck", both$city)
  both$city <- gsub("montana", "", both$city)
  both$city <- gsub("antwerpen", "antwerp", both$city)
  both$city <- gsub("brasília", "brasilia", both$city)
  both$city <- gsub("p.r.", "", both$city)
  both$city <- gsub("denmark", "", both$city)
  both$city <- gsub("mexico d.f.", "mexico city", both$city)
  both$city <- gsub("stockolm", "stockholm", both$city)
  both$city <- gsub("nottinghamuk", "nottingham", both$city)
  both$city <- gsub("stonybrook", "stony brook", both$city)
  both$city <- gsub("tufts", "medford", both$city)
  both$city <- gsub("tuscon", "tucson", both$city)
  both$city <- gsub("new york city", "new york", both$city)
  both$city <- gsub("notre dame", "south bend", both$city)

  both$city <- gsub("st martin d'heres cedex", "st martin dheres cedex", both$city)
  both$city <- gsub("st-jean-sur-richelieu", "st jean sur richelieu", both$city)

  # notes
  both$notes[both$city == "latvia"] <- NA
  both$notes[both$editor_id == 130 & both$year == 1990] <- "inst was formerly oxford polytechnic"
  both$notes[both$editor_id == 3395 & both$year == 1986] <- "not 100% regarding inst"
  both$notes[both$editor_id == 289 & both$year == 1985] <- "not 100% regarding inst"
  both$notes[both$editor_id == 722 & both$year == 1985] <- "not 100% regarding inst; city in 1993 paper & jrnl front mattter don't match"


  both$editor_id[both$last_name == "fedler" & both$first_name == "anthony" & both$journal == "najfm"] <- NA
  both$inst[both$inst == "university of wisconsin eauniversity of claire"] <- "university of wisconsin eau claire"

  return(both)
}

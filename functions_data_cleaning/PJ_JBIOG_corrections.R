pj_jbiog_corrections <- function(original_data) {
  # original_data<-alldata
  library(tidyverse)


  jbiog_inst <- read_csv("./Data/Patrick_James_Data_Corrections/Complete/PJCorrections_JBIOG.csv", col_names = TRUE)
  
  jbiog_inst<-jbiog_inst %>%
    mutate(across(everything(), as.character))
  
  names(jbiog_inst)

  jbiog_inst <- jbiog_inst %>% select(-"...1")
  jbiog_inst <- jbiog_inst %>% select(
    journal, year, editor_id, first_name,
    middle_name, last_name, inst, city, notes,
    correct_inst, correct_city, correct_state, correct_country
  )

  # original_data<-ALLDATA
  JBIOG <- filter(original_data, journal == "JBIOG")

  colnames(JBIOG)
  colnames(original_data)


  # remove OECOL FROM THE COMPLETE DATASET, WILL REBIND AFTER ADDING CORRECTIONS
  original_data <- original_data %>% filter(journal != "JBIOG")

  # INSERT THE CORRECTIONS TO OECOL AND FILL
  JBIOG <- JBIOG %>% na_if("missing")
  jbiog_inst <- jbiog_inst %>% na_if("missing")


  JBIOG$inst <- as.character(JBIOG$inst)

  jbiog_inst$editor_id <- as.character(jbiog_inst$editor_id)
  JBIOG$editor_id <- as.character(JBIOG$editor_id)
  colnames(JBIOG)
  colnames(jbiog_inst)
  JBIOG <- full_join(JBIOG, jbiog_inst, by = c("last_name", "first_name", "year"), all = T)
  colnames(JBIOG)
  JBIOG <- JBIOG %>%
    mutate(city.x = ifelse((is.na(city.x) | city.x == "missing"), city.y, city.x)) %>%
    select(-city.y) %>%
    rename("city" = "city.x") %>%
    mutate(city = ifelse(is.na(correct_city), city, correct_city)) %>%
    select(-correct_city)
  JBIOG <- JBIOG %>%
    mutate(inst.x = ifelse((is.na(inst.x) | inst.x == "missing"), inst.y, inst.x)) %>%
    select(-inst.y) %>%
    rename("inst" = "inst.x") %>%
    mutate(inst = ifelse(is.na(correct_inst), inst, correct_inst)) %>%
    select(-correct_inst)
  JBIOG <- JBIOG %>%
    select(-journal.y) %>%
    rename("journal" = "journal.x")
  JBIOG <- JBIOG %>%
    mutate(middle_name.x = ifelse((is.na(middle_name.x) | middle_name.x == "missing"), middle_name.y, middle_name.x)) %>%
    select(-middle_name.y) %>%
    rename("middle_name" = "middle_name.x")

  JBIOG <- JBIOG %>%
    mutate(notes.x = ifelse((is.na(notes.x) | notes.x == "missing"), notes.y, notes.x)) %>%
    select(-notes.y) %>%
    rename("notes" = "notes.x")
  # JBIOG <- JBIOG %>%
  #   mutate(UNIT.x = ifelse((is.na(UNIT.x)|UNIT.x=="missing"), UNIT.y, UNIT.x)) %>%
  #   select(-UNIT.y) %>%
  #   rename("UNIT"="UNIT.x")
  JBIOG <- JBIOG %>%
    mutate(state = ifelse(is.na(correct_state), state, correct_state)) %>%
    select(-correct_state)
  JBIOG <- JBIOG %>%
    mutate(country = ifelse(is.na(correct_country), country, correct_country)) %>%
    select(-correct_country)
  JBIOG <- JBIOG %>%
    mutate(editor_id.x = ifelse((is.na(editor_id.x) | editor_id.x == "missing"), editor_id.y, editor_id.x)) %>%
    select(-editor_id.y) %>%
    rename("editor_id" = "editor_id.x")
  #
  JBIOG$journal <- "JBIOG"
  #
  # JBIOG$last_name[JBIOG$last_name=="Lookinbill"]<-"Lookingbill"
  # JBIOG$city[JBIOG$last_name=="Overton"]<-NA
  # JBIOG$geo.code[JBIOG$last_name=="Betts"]<-"CAN"

  JBIOG <- JBIOG %>%
    group_by(last_name, first_name) %>%
    fill(inst, city, .direction = "down")
  #
  # JBIOG$editor_id<-as.factor(JBIOG$editor_id)
  # #
  # # Rebind the ORIGINAL DATA AND NOW CORRECTED JBIOG
  #
  # str(original_data)
  # str(JBIOG)
  # JBIOG$editor_id<-as.factor(JBIOG$editor_id)
  # str(jbiog_inst)
  # original_data<-bind_rows(original_data,JBIOG)
  # colnames(original_data)

  # rm(JBIOG,jbiog_inst)
  JBIOG$editor_id <- as.character(JBIOG$editor_id)
  
  JBIOG<-JBIOG %>%
    mutate(across(everything(), as.character))
  
  original_data$editor_id <- as.character(original_data$editor_id)
  return_list <- list(original_data, JBIOG)
  return(return_list)
}

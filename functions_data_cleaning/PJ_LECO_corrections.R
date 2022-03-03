PJ_LECO_corrections <- function(original_data) {
  # original_data<-alldata
  library(tidyverse)

  leco_inst <- read_csv("./Data/Patrick_James_Data_Corrections/Complete/newPJCorrections_SHORT_10_LECO.csv", col_names = TRUE)
  
  leco_inst<-leco_inst %>%
    mutate(across(everything(), as.character))
  
  names(leco_inst)
  leco_inst <- leco_inst %>% select(-"...1", -notes)
  leco_inst <- leco_inst %>% select(
    journal, year, editor_id, first_name,
    middle_name, last_name, inst, city,
    correct_inst, correct_city, correct_state,
    correct_country, correct_name_first,
    correct_name_last
  )

  # original_data<-ALLDATA
  landscapeeco <- filter(original_data, journal == "LECO")

  colnames(landscapeeco)
  colnames(original_data)


  # remove OECOL FROM THE COMPLETE DATASET, WILL REBIND AFTER ADDING CORRECTIONS
  original_data <- original_data %>% filter(journal != "LECO")

  # INSERT THE CORRECTIONS TO OECOL AND FILL
  landscapeeco <- landscapeeco %>% na_if("missing")
  leco_inst <- leco_inst %>% na_if("missing")


  landscapeeco$inst <- as.character(landscapeeco$inst)
  leco_inst$editor_id <- as.character(leco_inst$editor_id)
  landscapeeco$editor_id <- as.character(landscapeeco$editor_id)
  #
  landscapeeco <- full_join(landscapeeco, leco_inst, by = c("last_name", "first_name", "year"), all = T)
  colnames(landscapeeco)

  landscapeeco <- landscapeeco %>%
    mutate(city.x = ifelse((is.na(city.x) | city.x == "missing"), city.y, city.x)) %>%
    select(-city.y) %>%
    rename("city" = "city.x") %>%
    mutate(city = ifelse(is.na(correct_city), city, correct_city)) %>%
    select(-correct_city)
  landscapeeco <- landscapeeco %>%
    mutate(inst.x = ifelse((is.na(inst.x) | inst.x == "missing"), inst.y, inst.x)) %>%
    select(-inst.y) %>%
    rename("inst" = "inst.x") %>%
    mutate(inst = ifelse(is.na(correct_inst), inst, correct_inst)) %>%
    select(-correct_inst)
  landscapeeco <- landscapeeco %>%
    select(-journal.y) %>%
    rename("journal" = "journal.x")
  landscapeeco <- landscapeeco %>%
    mutate(middle_name.x = ifelse((is.na(middle_name.x) | middle_name.x == "missing"), middle_name.y, middle_name.x)) %>%
    select(-middle_name.y) %>%
    rename("middle_name" = "middle_name.x")
  #
  # landscapeeco <- landscapeeco %>%
  #   mutate(notes.x = ifelse((is.na(notes.x)|notes.x=="missing"), notes.y, notes.x)) %>%
  #   select(-notes.y) %>%
  #   rename("notes"="notes.x")
  # landscapeeco <- landscapeeco %>%
  #   mutate(UNIT.x = ifelse((is.na(UNIT.x)|UNIT.x=="missing"), UNIT.y, UNIT.x)) %>%
  #   select(-UNIT.y) %>%
  #   rename("UNIT"="UNIT.x")
  landscapeeco <- landscapeeco %>%
    mutate(state = ifelse(is.na(correct_state), state, correct_state)) %>%
    select(-correct_state)
  landscapeeco <- landscapeeco %>%
    mutate(country = ifelse(is.na(correct_country), country, correct_country)) %>%
    select(-correct_country)
  landscapeeco <- landscapeeco %>%
    mutate(editor_id.x = ifelse((is.na(editor_id.x) | editor_id.x == "missing"), editor_id.y, editor_id.x)) %>%
    select(-editor_id.y) %>%
    rename("editor_id" = "editor_id.x")
  #
  landscapeeco$journal <- "landscapeeco"
  #
  # landscapeeco$last_name[landscapeeco$last_name=="Lookinbill"]<-"Lookingbill"
  # landscapeeco$city[landscapeeco$last_name=="Overton"]<-NA
  # landscapeeco$geo.code[landscapeeco$last_name=="Betts"]<-"CAN"

  landscapeeco <- landscapeeco %>%
    group_by(last_name, first_name) %>%
    fill(inst, city, .direction = "down")



  #
  # landscapeeco <- landscapeeco %>%
  #   mutate(city.x = ifelse((is.na(city.x)|city.x=="missing"), city.y, city.x)) %>%
  #   select(-city.y) %>%
  #   rename("city"="city.x")
  # landscapeeco <- landscapeeco %>%
  #   mutate(inst.x = ifelse((is.na(inst.x)|inst.x=="missing"), inst.y, inst.x)) %>%
  #   select(-inst.y) %>%
  #   rename("inst"="inst.x")
  # landscapeeco <- landscapeeco %>%
  #   select(-journal.y) %>%
  #   rename("journal"="journal.x")
  # landscapeeco <- landscapeeco %>%
  #   mutate(middle_name.x = ifelse((is.na(middle_name.x)|middle_name.x=="missing"), middle_name.y, middle_name.x)) %>%
  #   select(-middle_name.y) %>%
  #   rename("middle_name"="middle_name.x")
  # landscapeeco <- landscapeeco %>%
  #   mutate(notes.x = ifelse((is.na(notes.x)|notes.x=="missing"), notes.y, notes.x)) %>%
  #   select(-notes.y) %>%
  #   rename("notes"="notes.x")
  # # landscapeeco <- landscapeeco %>%
  # #   mutate(UNIT.x = ifelse((is.na(UNIT.x)|UNIT.x=="missing"), UNIT.y, UNIT.x)) %>%
  # #   select(-UNIT.y) %>%
  # #   rename("UNIT"="UNIT.x")
  # # landscapeeco <- landscapeeco %>%
  # #   mutate(state.x = ifelse((is.na(state.x)|state.x=="missing"), state.y, state.x)) %>%
  # #   select(-state.y) %>%
  # #   rename("state"="state.x")
  # # landscapeeco <- landscapeeco %>%
  # #   select(-country.y) %>%
  # #   rename("country"="country.x")
  # # str(landscapeeco$editor_id.x)
  # # str(landscapeeco$editor_id.y)
  #
  # landscapeeco<- landscapeeco %>%
  #   mutate(editor_id.x = ifelse((is.na(editor_id.x)|editor_id.x=="missing"), editor_id.y, editor_id.x)) %>%
  #   select(-editor_id.y) %>%
  #   rename("editor_id"="editor_id.x")
  # # #
  # landscapeeco$journal<-"LECO"
  # #
  # # landscapeeco$last_name[landscapeeco$last_name=="Lookinbill"]<-"Lookingbill"
  # # landscapeeco$city[landscapeeco$last_name=="Overton"]<-NA
  # # landscapeeco$geo.code[landscapeeco$last_name=="Betts"]<-"CAN"
  #
  # landscapeeco<-landscapeeco %>% group_by(last_name,first_name) %>%
  #   fill(inst,city,.direction="down")
  #
  # landscapeeco$editor_id<-as.factor(landscapeeco$editor_id)
  # #
  # # Rebind the ORIGINAL DATA AND NOW CORRECTED landscapeeco
  #
  # str(original_data)
  # str(landscapeeco)
  # landscapeeco$editor_id<-as.factor(landscapeeco$editor_id)
  # str(leco_inst)
  # original_data<-bind_rows(original_data,landscapeeco)
  # colnames(original_data)

  # rm(landscapeeco,leco_inst)
  landscapeeco$editor_id <- as.character(landscapeeco$editor_id)
  landscapeeco$journal <- "LECO"
  
  
  landscapeeco<-landscapeeco %>%
    mutate(across(everything(), as.character))
  
  original_data$editor_id <- as.character(original_data$editor_id)
  return_list <- list(original_data, landscapeeco)
  return(return_list)
}

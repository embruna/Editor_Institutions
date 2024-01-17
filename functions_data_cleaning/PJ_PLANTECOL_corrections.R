PJ_PLANTECOL_corrections <- function(original_data) {
  # original_data<-alldata
  library(tidyverse)


  plantecol_inst <- read_csv("./data_raw/Patrick_James_Data_Corrections/Complete/PJCorrections_PLANTECOL.csv", col_names = TRUE)
  
  plantecol_inst<-plantecol_inst %>%
    mutate(across(everything(), as.character))
  
  names(plantecol_inst)

  plantecol_inst <- plantecol_inst %>% select(-"...1")
  plantecol_inst <- plantecol_inst %>% select(
    journal, year, editor_id, first_name,
    middle_name, last_name, inst, city, notes,
    correct_inst, correct_city, correct_state, correct_first_name, correct_middle_name
  )

  # original_data<-ALLDATA
  PLANTECOL <- filter(original_data, journal == "PLANTECOL")

  colnames(PLANTECOL)
  colnames(original_data)


  # remove PLANTECOL FROM THE COMPLETE DATASET, WILL REBIND AFTER ADDING CORRECTIONS
  original_data <- original_data %>% filter(journal != "PLANTECOL")

  # INSERT THE CORRECTIONS TO OECOL AND FILL

  
  PLANTECOL<-PLANTECOL %>%
    mutate(across(where(is.character), ~na_if(., "missing")))
  
  plantecol_inst<-plantecol_inst %>%
    mutate(across(where(is.character), ~na_if(., "missing")))
  
  
  # 
  #   PLANTECOL <- PLANTECOL %>% na_if("missing")
  # plantecol_inst <- plantecol_inst %>% na_if("missing")


  PLANTECOL$inst <- as.character(PLANTECOL$inst)

  plantecol_inst$editor_id <- as.character(plantecol_inst$editor_id)
  PLANTECOL$editor_id <- as.character(PLANTECOL$editor_id)
  colnames(PLANTECOL)
  colnames(plantecol_inst)
  PLANTECOL <- full_join(PLANTECOL, plantecol_inst, by = c("last_name", "first_name", "year"))

  PLANTECOL <- PLANTECOL %>%
    group_by(last_name, first_name, country) %>%
    fill(correct_inst, correct_city, correct_state, correct_first_name, correct_middle_name, .direction = "down")



  colnames(PLANTECOL)
  PLANTECOL <- PLANTECOL %>%
    mutate(city.x = ifelse((is.na(city.x) | city.x == "missing"), city.y, city.x)) %>%
    select(-city.y) %>%
    rename("city" = "city.x") %>%
    mutate(city = ifelse(is.na(correct_city) == FALSE, correct_city, city)) %>%
    select(-correct_city)
  PLANTECOL <- PLANTECOL %>%
    mutate(inst.x = ifelse((is.na(inst.x) | inst.x == "missing"), inst.y, inst.x)) %>%
    select(-inst.y) %>%
    rename("inst" = "inst.x") %>%
    mutate(inst = ifelse(!is.na(correct_inst), correct_inst, inst)) %>%
    select(-correct_inst)
  PLANTECOL <- PLANTECOL %>%
    select(-journal.y) %>%
    rename("journal" = "journal.x")
  PLANTECOL <- PLANTECOL %>%
    mutate(middle_name.x = ifelse((is.na(middle_name.x) | middle_name.x == "missing"), middle_name.y, middle_name.x)) %>%
    select(-middle_name.y) %>%
    rename("middle_name" = "middle_name.x") %>%
    mutate(middle_name = ifelse(!is.na(correct_middle_name), correct_middle_name, middle_name)) %>%
    select(-correct_middle_name)
  # PLANTECOL <- PLANTECOL %>%
  #   mutate(first_name = ifelse(!is.na(correct_first_name), correct_first_name,first_name)) %>%
  #   select(-correct_first_name)
  PLANTECOL <- PLANTECOL %>%
    mutate(notes.x = ifelse((!is.na(notes.x) | notes.x == "missing"), notes.y, notes.x)) %>%
    select(-notes.y) %>%
    rename("notes" = "notes.x")
  # PLANTECOL <- PLANTECOL %>%
  #   mutate(UNIT.x = ifelse((is.na(UNIT.x)|UNIT.x=="missing"), UNIT.y, UNIT.x)) %>%
  #   select(-UNIT.y) %>%
  #   rename("UNIT"="UNIT.x")
  PLANTECOL <- PLANTECOL %>%
    mutate(state = ifelse(!is.na(correct_state), correct_state, state)) %>%
    select(-correct_state)
  # PLANTECOL <- PLANTECOL %>%
  #   select(-country.y) %>%
  #   rename("country"="country.x") %>%
  #   mutate(country = ifelse(is.na(correct_country), country, correct_country)) %>%
  #   select(-correct_country)
  PLANTECOL <- PLANTECOL %>%
    mutate(editor_id.x = ifelse((is.na(editor_id.x) | editor_id.x == "missing"), editor_id.y, editor_id.x)) %>%
    select(-editor_id.y) %>%
    rename("editor_id" = "editor_id.x")
  #
  PLANTECOL$journal <- "PLANTECOL"
  #
  # PLANTECOL$last_name[PLANTECOL$last_name=="Lookinbill"]<-"Lookingbill"
  # PLANTECOL$city[PLANTECOL$last_name=="Overton"]<-NA
  # PLANTECOL$geo.code[PLANTECOL$last_name=="Betts"]<-"CAN"

  PLANTECOL <- PLANTECOL %>%
    group_by(last_name, first_name, country) %>%
    fill(inst, city, .direction = "down")
  #
  # PLANTECOL$editor_id<-as.factor(PLANTECOL$editor_id)
  # #
  # # Rebind the ORIGINAL DATA AND NOW CORRECTED PLANTECOL
  #
  # str(original_data)
  # str(PLANTECOL)
  # PLANTECOL$editor_id<-as.factor(PLANTECOL$editor_id)
  # str(plantecol_inst)
  # original_data<-bind_rows(original_data,PLANTECOL)
  # colnames(original_data)

  # rm(PLANTECOL,plantecol_inst)
  PLANTECOL$editor_id <- as.character(PLANTECOL$editor_id)
  
  
  PLANTECOL<-PLANTECOL %>%
    mutate(across(everything(), as.character))
  
  original_data$editor_id <- as.character(original_data$editor_id)
  return_list <- list(original_data, PLANTECOL)
  
  
  
  return(return_list)
}

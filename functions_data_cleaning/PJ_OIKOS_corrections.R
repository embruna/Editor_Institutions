PJ_OIKOS_corrections <- function(original_data) {
  # original_data<-alldata
  library(tidyverse)


  oikos_inst <- read_csv("./data_raw/Patrick_James_Data_Corrections/Complete/PJCorrections_OIKOS.csv", col_names = TRUE)
  oikos_inst<-oikos_inst %>%
    mutate(across(everything(), as.character))
  names(oikos_inst)

  oikos_inst <- oikos_inst %>% select(-"...1")
  oikos_inst <- oikos_inst %>% select(
    journal, year, editor_id, first_name,
    middle_name, last_name, inst, city, notes
  )

  # original_data<-ALLDATA
  OIKOS <- filter(original_data, journal == "OIKOS")

  colnames(OIKOS)
  colnames(original_data)


  # remove OECOL FROM THE COMPLETE DATASET, WILL REBIND AFTER ADDING CORRECTIONS
  original_data <- original_data %>% filter(journal != "OIKOS")

  # INSERT THE CORRECTIONS TO OECOL AND FILL
  
  
  
  OIKOS<-OIKOS %>%
    mutate(across(where(is.character), ~na_if(., "missing")))
  
  oikos_inst<-oikos_inst %>%
    mutate(across(where(is.character), ~na_if(., "missing")))
  
  


  OIKOS$inst <- as.character(OIKOS$inst)

  oikos_inst$editor_id <- as.character(oikos_inst$editor_id)
  OIKOS$editor_id <- as.character(OIKOS$editor_id)
  #
  OIKOS <- full_join(OIKOS, oikos_inst, by = c("last_name", "first_name", "year"))

  OIKOS <- OIKOS %>%
    mutate(city.x = ifelse((is.na(city.x) | city.x == "missing"), city.y, city.x)) %>%
    select(-city.y) %>%
    rename("city" = "city.x")
  OIKOS <- OIKOS %>%
    mutate(inst.x = ifelse((is.na(inst.x) | inst.x == "missing"), inst.y, inst.x)) %>%
    select(-inst.y) %>%
    rename("inst" = "inst.x")
  OIKOS <- OIKOS %>%
    select(-journal.y) %>%
    rename("journal" = "journal.x")
  OIKOS <- OIKOS %>%
    mutate(middle_name.x = ifelse((is.na(middle_name.x) | middle_name.x == "missing"), middle_name.y, middle_name.x)) %>%
    select(-middle_name.y) %>%
    rename("middle_name" = "middle_name.x")
  OIKOS <- OIKOS %>%
    mutate(notes.x = ifelse((is.na(notes.x) | notes.x == "missing"), notes.y, notes.x)) %>%
    select(-notes.y) %>%
    rename("notes" = "notes.x")
  # OIKOS <- OIKOS %>%
  #   mutate(UNIT.x = ifelse((is.na(UNIT.x)|UNIT.x=="missing"), UNIT.y, UNIT.x)) %>%
  #   select(-UNIT.y) %>%
  #   rename("UNIT"="UNIT.x")
  # OIKOS <- OIKOS %>%
  #   mutate(state.x = ifelse((is.na(state.x)|state.x=="missing"), state.y, state.x)) %>%
  #   select(-state.y) %>%
  #   rename("state"="state.x")
  # OIKOS <- OIKOS %>%
  #   select(-COUNTRY.y) %>%
  #   rename("COUNTRY"="COUNTRY.x")
  OIKOS <- OIKOS %>%
    mutate(editor_id.x = ifelse((is.na(editor_id.x) | editor_id.x == "missing"), editor_id.y, editor_id.x)) %>%
    select(-editor_id.y) %>%
    rename("editor_id" = "editor_id.x")
  #
  OIKOS$journal <- "OIKOS"
  #
  # OIKOS$last_name[OIKOS$last_name=="Lookinbill"]<-"Lookingbill"
  # OIKOS$city[OIKOS$last_name=="Overton"]<-NA
  # OIKOS$geo.code[OIKOS$last_name=="Betts"]<-"CAN"

  OIKOS <- OIKOS %>%
    group_by(last_name, first_name) %>%
    fill(inst, city, .direction = "down")
  #
  # OIKOS$editor_id<-as.factor(OIKOS$editor_id)
  # #
  # # Rebind the ORIGINAL DATA AND NOW CORRECTED OIKOS
  #
  # str(original_data)
  # str(OIKOS)
  # OIKOS$editor_id<-as.factor(OIKOS$editor_id)
  # str(oikos_inst)
  # original_data<-bind_rows(original_data,OIKOS)
  # colnames(original_data)

  # rm(OIKOS,oikos_inst)
  OIKOS$editor_id <- as.character(OIKOS$editor_id)
  
  
  
  OIKOS<-OIKOS %>%
    mutate(across(everything(), as.character))
  
  original_data$editor_id <- as.character(original_data$editor_id)
  return_list <- list(original_data, OIKOS)
  return(return_list)
}

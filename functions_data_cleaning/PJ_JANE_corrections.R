PJ_JANE_corrections <- function(original_data) {
  # original_data<-alldata
  library(tidyverse)

  jane_inst <- read_csv("./data_raw/Patrick_James_Data_Corrections/Complete/PJCorrections_JANE.csv", col_names = TRUE)
  names(jane_inst)
  jane_inst<-jane_inst %>%
    mutate(across(everything(), as.character))
  
  jane_inst <- jane_inst %>% select(-"...1")
  jane_inst <- jane_inst %>% select(
    journal, year, editor_id, first_name,
    middle_name, last_name, inst, city, notes
  )

  jane_inst <- jane_inst %>% filter_all(any_vars(!is.na(.)))

  # original_data<-ALLDATA
  JANE <- filter(original_data, journal == "JANE")

  colnames(JANE)
  colnames(original_data)


  # remove JANE FROM THE COMPLETE DATASET, WILL REBIND AFTER ADDING CORRECTIONS
  original_data <- original_data %>% filter(journal != "JANE")

  # INSERT THE CORRECTIONS TO JANE AND FILL
  
  
  JANE<-JANE %>%
    mutate(across(where(is.character), ~na_if(., "missing")))
  
  jane_inst<-jane_inst %>%
    mutate(across(where(is.character), ~na_if(., "missing")))
  
  
  

  JANE$inst <- as.character(JANE$inst)

  jane_inst$editor_id <- as.character(jane_inst$editor_id)
  JANE$editor_id <- as.character(JANE$editor_id)
  #
  JANE <- full_join(JANE, jane_inst, by = c("last_name", "first_name", "year"))

  JANE <- JANE %>%
    mutate(city.x = ifelse((is.na(city.x) | city.x == "missing"), city.y, city.x)) %>%
    select(-city.y) %>%
    rename("city" = "city.x")
  JANE <- JANE %>%
    mutate(inst.x = ifelse((is.na(inst.x) | inst.x == "missing"), inst.y, inst.x)) %>%
    select(-inst.y) %>%
    rename("inst" = "inst.x")
  JANE <- JANE %>%
    select(-journal.y) %>%
    rename("journal" = "journal.x")
  JANE <- JANE %>%
    mutate(middle_name.x = ifelse((is.na(middle_name.x) | middle_name.x == "missing"), middle_name.y, middle_name.x)) %>%
    select(-middle_name.y) %>%
    rename("middle_name" = "middle_name.x")
  JANE <- JANE %>%
    mutate(notes.x = ifelse((is.na(notes.x) | notes.x == "missing"), notes.y, notes.x)) %>%
    select(-notes.y) %>%
    rename("notes" = "notes.x")
  # JANE <- JANE %>%
  #   mutate(UNIT.x = ifelse((is.na(UNIT.x)|UNIT.x=="missing"), UNIT.y, UNIT.x)) %>%
  #   select(-UNIT.y) %>%
  #   rename("UNIT"="UNIT.x")
  # JANE <- JANE %>%
  #   mutate(STATE.x = ifelse((is.na(STATE.x)|STATE.x=="missing"), STATE.y, STATE.x)) %>%
  #   select(-STATE.y) %>%
  #   rename("STATE"="STATE.x")
  # JANE <- JANE %>%
  #   select(-country.y) %>%
  #   rename("country"="country.x")
  JANE <- JANE %>%
    mutate(editor_id.x = ifelse((is.na(editor_id.x) | editor_id.x == "missing"), editor_id.y, editor_id.x)) %>%
    select(-editor_id.y) %>%
    rename("editor_id" = "editor_id.x")
  #
  JANE$journal <- "JANE"

  #
  # JANE$last_name[JANE$last_name=="Lookinbill"]<-"Lookingbill"
  # JANE$city[JANE$last_name=="Overton"]<-NA
  # JANE$geo.code[JANE$last_name=="Betts"]<-"CAN"
  colnames(JANE)
  JANE <- JANE %>%
    group_by(last_name, first_name, country) %>%
    fill(inst, city, .direction = "down")
  #
  # JANE$editor_id<-as.factor(JANE$editor_id)
  # #
  # # Rebind the ORIGINAL DATA AND NOW CORRECTED JANE
  #
  # str(original_data)
  # str(JANE)
  # JANE$editor_id<-as.factor(JANE$editor_id)
  # str(jane_inst)
  # original_data<-bind_rows(original_data,JANE)
  # colnames(original_data)

  # rm(JANE,jane_inst)
  JANE$editor_id <- as.character(JANE$editor_id)
  
  
  JANE<-JANE %>%
    mutate(across(everything(), as.character))
  
  original_data$editor_id <- as.character(original_data$editor_id)
  return_list <- list(original_data, JANE)
  return(return_list)
}

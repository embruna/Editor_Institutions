# This will load and the corrected files and make the required changes.
PJ_OECOL_corrections <- function(original_data) {
  # original_data<-alldata
  library(tidyverse)

  oecol_inst <- read.csv("./data_raw/Patrick_James_Data_Corrections/Complete/PJCorrections_OECOL.csv", encoding = "ascii")
  
  oecol_inst<-oecol_inst %>%
    mutate(across(everything(), as.character))
  
  
  names(oecol_inst)
  oecol_inst <- oecol_inst %>%
    select(-X, -editor_id.y) %>%
    rename("editor_id" = "editor_id.x")
  names(oecol_inst)
  oecol_inst <- oecol_inst %>% select(
    journal, year, editor_id, first_name,
    middle_name, last_name, inst, city, notes
  )


  # NEW df with oecologia
  oecologia <- filter(original_data, journal == "OECOL")

  # remove OECOL FROM THE COMPLETE DATASET, WILL REBIND AFTER ADDING CORRECTIONS
  original_data <- original_data %>% filter(journal != "OECOL")

  # INSERT THE CORRECTIONS TO OECOL AND FILL
  
  oecologia<-oecologia %>%
    mutate(across(where(is.character), ~na_if(., "missing")))
  
  oecol_inst<-oecol_inst %>%
    mutate(across(where(is.character), ~na_if(., "missing")))
  
  # oecologia <- oecologia %>% na_if("missing")
  # oecol_inst <- oecol_inst %>% na_if("missing")
  # 
  
  # colnames(oecologia)
  # colnames(oecol_inst)
  # str(oecologia)
  # str(oecol_inst)
  oecol_inst$editor_id <- as.factor(oecol_inst$editor_id)
  oecol_inst$inst <- as.character(oecol_inst$inst)
  oecologia$inst <- as.character(oecologia$inst)

  oecol_inst$middle_name <- as.character(oecol_inst$middle_name)
  oecologia$middle_name <- as.character(oecologia$middle_name)

  oecol_inst$last_name <- as.character(oecol_inst$last_name)
  oecologia$last_name <- as.character(oecologia$last_name)

  oecol_inst$first_name <- as.character(oecol_inst$first_name)
  oecologia$first_name <- as.character(oecologia$first_name)

  oecol_inst$city <- as.character(oecol_inst$city)
  oecologia$city <- as.character(oecologia$city)

  oecol_inst$notes <- as.character(oecol_inst$notes)
  oecologia$notes <- as.character(oecologia$notes)
  
  # oecologia <- full_join(oecologia, oecol_inst, by = c("last_name", "first_name", "year"), all = T) %>%
  oecologia <- full_join(oecologia, oecol_inst, by = c("last_name", "first_name", "year")) %>%
    group_by(last_name, first_name) %>%
    mutate(city.x = ifelse((is.na(city.x) | city.x == "missing"), city.y, city.x)) %>%
    select(-city.y) %>%
    rename("city" = "city.x") %>%
    mutate(inst.x = ifelse((is.na(inst.x) | inst.x == "missing"), inst.y, inst.x)) %>%
    select(-inst.y) %>%
    rename("inst" = "inst.x") %>%
    select(-journal.y) %>%
    rename("journal" = "journal.x") %>%
    mutate(middle_name.x = ifelse((is.na(middle_name.x) | middle_name.x == "missing"), middle_name.y, middle_name.x)) %>%
    select(-middle_name.y) %>%
    rename("middle_name" = "middle_name.x") %>%
    mutate(notes.x = ifelse((is.na(notes.x) | notes.x == "missing"), notes.y, notes.x)) %>%
    select(-notes.y) %>%
    rename("notes" = "notes.x") 
  # %>%
  #   mutate(editor_id.x = ifelse((is.na(editor_id.x) | editor_id.x == "missing"), editor_id.y, editor_id.x)) %>%
  #   select(-editor_id.y) %>%
  #   rename("editor_id" = "editor_id.x")

  oecologia$journal <- "OECOL"
  
  oecologia <- oecologia %>%
    group_by(last_name, first_name, country) %>%
    fill(inst, city, .direction = "down")

  
  
  oecologia<-oecologia %>%
    mutate(across(everything(), as.character))
  
  
  oecologia <- oecologia %>%
     mutate(editor_id.x = ifelse((is.na(editor_id.x) | editor_id.x == "missing"), editor_id.y, editor_id.x)) %>%
     select(-editor_id.y) %>%
     rename("editor_id" = "editor_id.x")
  
  
  oecologia$editor_id <- as.factor(oecologia$editor_id)

  # Rebind the ORIGINAL DATA AND NOW CORRECTED oecologia
  str(original_data)
  str(oecologia)

  # original_data<-bind_rows(original_data,oecologia)
  # rm(oecologia,oecol_inst)
  oecologia$editor_id <- as.character(oecologia$editor_id)
  
  
  oecologia<-oecologia %>%
    mutate(across(everything(), as.character))
  
  original_data$editor_id <- as.character(original_data$editor_id)
  return_list <- list(original_data, oecologia)
  return(return_list)
}

# Introduction ------------------------------------------------------------

# CODE FOR IMPORTING, MANIPULATING, AND ANALYZING DATA ON THE 
# INSTITUTIONS AT WHICH JOURNAL EDITORS ARE BASED

# load libraries ----------------------------------------------------------

library(tidyverse)
library(stringr)
library(stringi)
library(stringdist)
library(RecordLinkage)
library(janitor)
library(openalexR)
library(rjson)
library(jsonlite)

if (FALSE) { # \dontrun{
  
  # Query to search information about all Italian educational institutions
  
  query_inst <- oa_query(
    entity = "institutions",
    country_code = "us",
    options=list(select="id,display_name,ror"),
    type = "education"
  )
  
  res <- oa_request(
    query_url = query_inst,
    count_only = FALSE,
    verbose = FALSE
  )
  
  res_df<-oa2df(res, entity = "institutions")
} # }


unis<-editors %>% 
  filter(country == "usa") %>% 
  select(inst) %>% 
  distinct()

res_df<-res_df %>% 
  mutate(inst=tolower(display_name)) %>% 
  relocate(inst,.before=1) %>% 
  mutate(inst=gsub('[[:punct:] ]+',' ',inst)) %>% 
  mutate(inst=gsub("\\.","",inst)) %>%   
  mutate(inst=gsub("the ","",inst)) %>%   
  mutate(inst=gsub(" at "," ",inst))
foo<-all_join(unis,res_df,by="inst")
#          
#   mutate(inst=gsub(",","",inst)) %>% 
#   mutate(inst=gsub(" - "," ",inst)) %>% 
#   mutate(inst=gsub(" – "," ",inst)) %>% 
# mutate(inst=gsub("-"," ",inst)) %>% 
#   mutate(inst=gsub("–"," ",inst)) %>% 
#   mutate(inst=gsub(","," ",inst))





json_file <- "https://api.openalex.org/institutions"
json_data <- fromJSON(file=json_file)  
foo<-json_data$results 
foo<-as.data.frame(do.call(cbind, foo))
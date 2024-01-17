#FUNCTION TO Organize and Summarize the Carnegie CLassifications
CarnegieCats_2021 <- function(cc_year_choice) {
  cc_year_choice<-"basic2021"
  # cc_year_choice<-as.character(cc_year_choice)
  library(readxl)
  
  
  
  carnegie_raw<-read_excel("./data_raw/carnegie/CCIHE2021-PublicData.xlsx", sheet = "Data") %>% 
    select("unitid":"basic2021") %>% 
    mutate(across(everything(), tolower))
  names(carnegie_raw)<-tolower(names(carnegie_raw))
  
  
  carnegie_ranks<-read_excel("./data_raw/carnegie/CCIHE2021-PublicData.xlsx", sheet = "Values") %>% 
    mutate(across(everything(), tolower)) %>% 
    rename('cc_year'='Variable',
           "cc_def"="Label...2",
           "classification"='Label...4') %>% 
    drop_na(any_of('classification')) %>% 
    fill(c(cc_year, cc_def), .direction="down") %>% 
    filter(cc_year==cc_year_choice) %>%
    select(Value, classification)
    names(carnegie_ranks)<-c(cc_year_choice, "classification")
      # names(carnegie_ranks)<-tolower(names(carnegie_ranks))
  
  
  
  carnegie_raw<-left_join(carnegie_raw,carnegie_ranks) %>% select("unitid", 
                                                                  "name",
                                                                  "city",
                                                                  "stabbr", 
                                                                  'category'=cc_year_choice,
                                                                  "classification")
    
  carnegie_raw<-carnegie_raw %>% 
  mutate(classification = replace(classification, str_detect(classification, "associate's"), "associates")) %>% 
    mutate(classification = replace(classification, str_detect(classification, "special focus two-year"), "special-focus-2-yr")) %>% 
    mutate(classification = replace(classification, str_detect(classification, "doctoral"), "doctoral")) %>% 
    mutate(classification = replace(classification, str_detect(classification, "master's"), "masters")) %>% 
    mutate(classification = replace(classification, str_detect(classification, "baccalaureate"), "baccalaureate")) %>% 
    mutate(classification = replace(classification, str_detect(classification, "medical"), "medical-health")) %>% 
    mutate(classification = replace(classification, str_detect(classification, "faith-related"), "faith")) %>% 
    mutate(classification = replace(classification, str_detect(classification, "arts, music & design schools"), "art-music-design")) %>% 
    mutate(classification = replace(classification, str_detect(classification, "engineering and other technology"), "engineering-tech")) %>% 
    mutate(classification = replace(classification, str_detect(classification, "special focus"), "other-special-focus")) %>% 
    mutate(classification = replace(classification, str_detect(classification, "law"), "law")) %>% 
    mutate(classification = replace(classification, str_detect(classification, "tribal"), "tribal")) 
    carnegie_raw$classification<-as.factor(carnegie_raw$classification)
  
  
# 
#   
#   # carnegie_raw<-read_csv("./data_raw/carnegie/CarnegCategories_2015.csv")
#   carnegie_raw<-carnegie_raw %>% select(Value,Label...4) %>% rename("basic2015"="Value", "classification"='Label...4')
#   str(carnegie_raw)
#   carnegie_raw<-as.data.frame(carnegie_raw)
#   CarnRanks<-read_csv("./data_raw/carnegie/CarnegRankings_2015_Reduced.csv") %>% rename("basic2015"="BASIC2015")
#   # CarnRanks<-CarnRanks %>% select(UNITID, NAME, CITY, STABBR,BASIC2015)
#   str(CarnRanks)
#   CarnRanks<-as.data.frame(CarnRanks)
#   names(carnegie_raw)
#   carnegie_raw<-left_join(carnegie_raw,CarnRanks,by="basic2015")
#   # grouping different subcategories
#   carnegie_raw$Category[(carnegie_raw$basic2015>0 & carnegie_raw$basic2015<10)|carnegie_raw$basic2015==14]<-"Associates"
#   carnegie_raw$Category[carnegie_raw$basic2015>=10 & carnegie_raw$basic2015<=13]<-"SpecialFocus(2yr)"
#   carnegie_raw$Category[carnegie_raw$basic2015>=15 & carnegie_raw$basic2015<=17]<-"Doctoral"
#   carnegie_raw$Category[carnegie_raw$basic2015>=18 & carnegie_raw$basic2015<=20]<-"Masters"
#   carnegie_raw$Category[carnegie_raw$basic2015>=21 & carnegie_raw$basic2015<=23]<-"Baccalaureate"
#   carnegie_raw$Category[carnegie_raw$basic2015==24]<-"Faith-Related"
#   carnegie_raw$Category[carnegie_raw$basic2015>=25 & carnegie_raw$basic2015<=26]<-"Medical/Health"
#   carnegie_raw$Category[carnegie_raw$basic2015==27]<-"SpecialFocus-4yr-Engineering"
#   carnegie_raw$Category[carnegie_raw$basic2015==28]<-"SpecialFocus-4yr-Tech"
#   carnegie_raw$Category[carnegie_raw$basic2015==29]<-"SpecialFocus-4yr-Business"
#   carnegie_raw$Category[carnegie_raw$basic2015==30]<-"SpecialFocus-4yr-Art-Music-Design"
#   carnegie_raw$Category[carnegie_raw$basic2015==31]<-"Law"
#   carnegie_raw$Category[carnegie_raw$basic2015==32]<-"Other"
#   carnegie_raw$Category[carnegie_raw$basic2015==33]<-"Tribal"
#   carnegie_raw$Category<-as.factor(carnegie_raw$Category)
#   # levels(carnegie_raw$Category)
#   # summary(carnegie_raw)
  rm(carnegie_ranks)
  # names(carnegie_raw)<-tolower(names(carnegie_raw))
  return(carnegie_raw)
}



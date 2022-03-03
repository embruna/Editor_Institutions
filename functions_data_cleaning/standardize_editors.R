standardize_editors <- function(alldata) {
  
  # convert to lower case
  alldata$category <- tolower(alldata$category)
  alldata$title <- tolower(alldata$title)
  
  
  # alldata$category<-as.factor(alldata$category)
  alldata$title[is.na(alldata$title) & alldata$category == "se"] <- "se"
  alldata$title[is.na(alldata$title) & alldata$category == "ae"] <- "ae"
  alldata$title[is.na(alldata$title) & alldata$category == "eic"] <- "eic"
  alldata$title[is.na(alldata$title) & alldata$category == "special"] <- "special"
# 
#   alldata$title[is.na(alldata$title) & is.na(alldata$category)] <- "TBD"
#   alldata$title[alldata$title == "missing" & is.na(alldata$category)] <- "TBD"
#   alldata$category[is.na(alldata$category)] <- "TBD"
  
  alldata$title <- trimws(alldata$title, which = "both")
  alldata$title <- tolower(alldata$title)
  levels(as.factor(alldata$title))
  levels(as.factor(alldata$category))
  need_category <- filter(alldata, category == "bissing")
  levels(as.factor(need_category$title))
  
  alldata$category[alldata$title == "associate editor"] <- "ae"
  alldata$category[alldata$title == "associate editors"] <- "ae"
  
  alldata$category[alldata$title == "board of reviewing editors"] <- "se"
  alldata$category[alldata$title == "editorial board"] <- "se"
  alldata$category[alldata$title == "early career board mentor"] <- "se"
  
  alldata$category[alldata$title == "chief editor"] <- "eic"
  alldata$category[alldata$title == "editor in chief"] <- "eic"
  alldata$category[alldata$title == "editors in chief"] <- "eic"
  
  alldata$category[alldata$title == "co-founding editor"] <- "oversight"
  
  alldata$category[alldata$title == "abstract translators"] <- "service"
  alldata$category[alldata$title == "associate book review editor"] <- "special"
  alldata$category[alldata$title == "book review editor"] <- "special"
  alldata$category[alldata$title == "book review editorial"] <- "special"
  alldata$category[alldata$title == "early career editorial board"] <- "special"
  alldata$category[alldata$title == "in memoriam editor"] <- "special"
  alldata$category[alldata$title == "in memoriam editor"] <- "special"
  alldata$category[alldata$title == "latin american editorial board"] <- "special"
  alldata$category[alldata$title == "in memoriam editor"] <- "special"
  alldata$category[alldata$title == "memorials editor"] <- "special"
  alldata$category[alldata$title == "in memoriam editor"] <- "special"
  
  # Standardize production staff --------------------------------------------
  
  
  alldata$category[alldata$title == "journal director"] <- "production"
  alldata$category[alldata$title == "journals director"] <- "production"
  alldata$category[alldata$title == "translator"] <- "production"
  alldata$category[alldata$title == "spanish abstract translations"] <- "production"
  alldata$category[alldata$title == "assistant editors"] <- "production"
  alldata$category[alldata$title == "central ornithology publications office coordinator"] <- "production"
  alldata$category[alldata$title == "copy editor"] <- "production"
  alldata$category[alldata$title == "copyeditor"] <- "production"
  
  # DOUBLE CHECK!!!!
  alldata$category[alldata$title == "manager"] <- "production"
  alldata$category[alldata$title == "office coordinator"] <- "production"
  alldata$category[alldata$title == "proof editor"] <- "production"
  alldata$category[alldata$title == "project manager"] <- "production"
  alldata$category[alldata$title == "review editor"] <- "production"
  alldata$category[alldata$title == "review manager"] <- "production"
  alldata$category[alldata$title == "editorial advisory board"] <- "oversight"
  alldata$category[alldata$title == "editor"] <- "se" # OIKOS
  alldata$category[alldata$title == "editors"] <- "se"
  alldata$category[alldata$title == "interim editor"] <- "eic"
  alldata$category[alldata$title == "missing"] <- "TBD"
  alldata$category[alldata$title == "outgoing editor"] <- "eic"
  alldata$category[alldata$title == "editor elect"] <- "se"
  
  levels(as.factor(alldata$title))
  levels(as.factor(alldata$category))
  
  ### RECATOEGORIZE THE PRODUCTION STAFF
  alldata$category[alldata$title == "editorial assistants"] <- "production"
  alldata$category[alldata$title == "me"] <- "Production" # ME = Managing Editor
  alldata$category[alldata$title == "editorial assistant"] <- "production"
  alldata$category[alldata$title == "assistant managing editor"] <- "production"
  alldata$category[alldata$title == "managing editor"] <- "production"
  alldata$category[alldata$title == "production editor"] <- "production"
  alldata$category[alldata$title == "publications committee"] <- "oversight" # JZOOL
  alldata$category[alldata$title == "assistant editor"] <- "production" # JZOOL
  
  # 
  # alldata <- alldata %>% 
  #   filter(category != "production" | category != "oversight")
  # 
  # alldata <- alldata %>% 
  #   filter(category == "se" | category == "eic" | category == "ae" |
  #            category == "special" | category == "TBD")
  # alldata<-alldata %>% filter(category!="Production"|category!="Oversight")
  
  return(alldata)
  
}
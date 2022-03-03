name.check <- function(ColToClean) {
  
  # Trying to find names that are mispelled or close to correct close
  #   http://stackoverflow.com/questions/6683380/techniques-for-finding-near-duplicate-records
  # # https://cran.r-project.org/web/packages/RecordLinkage/index.html AND
  # # https://cran.r-project.org/web/packages/stringdist/stringdist.pdf
  # # https://cran.r-project.org/web/packages/RecordLinkage/RecordLinkage.pdf
  # https://journal.r-project.org/archive/2010-2/RJournal_2010-2_Sariyar+Borg.pdf
  # http://stackoverflow.com/questions/11535625/similarity-scores-based-on-string-comparison-in-r-edit-distance
  # http://stackoverflow.com/questions/28952034/finding-partial-matches-on-strings-in-r
  # CHECKFILE<-DataToClean
  #  # This uses agrep to check similarity, then outputs a list of all names in your file compared to 
  # all other names. This is what will help find spelling mistakes, eg. "abrams" and "abrasm"  will be counted as unique, as will 
  # "e bruna" and "emilio bruna". You can use this info to error correct or make changes to correctly pool the people with multiple names
  # ColToClean<-checkINST$INST
  library(tidyverse)
  ColToClean<-levels(as.factor(ColToClean))
  NamesList<-sapply(ColToClean,agrep,ColToClean, value=TRUE) 
  
  # Convert this list to a dataframe (with help from this post:   
  # https://aurelienmadouasse.wordpress.com/2012/05/22/r-code-how-to-convert-a-list-to-a-data-frame/)
  
  NamesDF<-data.frame(
    Name1 = rep(names(NamesList), lapply(NamesList, length)),
    Name2 = unlist(NamesList))
  
  # summary(NamesDF)
  # str(NamesDF)
  
  # Create a column to which you will add a logical condition telling you if the names are an EXACT match
  NamesDF$match<-NA
  NamesDF$match<-NamesDF$Name1==NamesDF$Name2
  # match2<-ifelse(NamesDF$match=="TRUE",1,0) #convert TRUE/FALSEto 0/1
  # NamesDF<-cbind(NamesDF,match2) 
  # head(NamesDF,40)
  # str(NamesDF)
  NamesDF<-arrange(NamesDF,Name1,Name2) #organize in alphabetica order
  NamesDF<-filter(NamesDF, match==FALSE)  # THIS DELETES ALL NAMES THAT ARE 100% MATCH 
  head(NamesDF)
  # Convert to chr
  NamesDF$Name1<-as.character(NamesDF$Name1)
  NamesDF$Name2<-as.character(NamesDF$Name2)
  str(NamesDF)
  
  # Calclulate the proportional similarity and # changes required to go from one name to another. Package RecordLinkage
  NamesDF$Name_sim<-levenshteinSim(NamesDF$Name1, NamesDF$Name2)
  NamesDF$Name_dist<-levenshteinDist(NamesDF$Name1, NamesDF$Name2)
  
  # Because this does all pairwise comparisons, it results in redundancy: "e bruna vs emilio bruna" and "emilio bruna vs e bruna"
  # are in different rows, even though they are the same "comparison". This deletes one of the two 
  NamesDF<-NamesDF[!duplicated(t(apply(NamesDF, 1, sort))),]
  # this arranges them in order from most similar (1 change required) to least similar.
  # look carefully at those with a few changes, as they are likely to be a tiny spelling mistake or difference in intials
  
  
  NamesDF$index<-seq.int(nrow(NamesDF)) #adds a column with an index to make it easier to id which row you need'
  NamesDF <- NamesDF %>% select(index, Name1, Name2, Name_sim,Name_dist) #It's kinda ugly, but this rearranges columns (and dumps the "FALSE")
  NamesDF <- arrange(NamesDF, desc(Name_sim))
  # head(NamesDF)
  # write.csv(NamesDF, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/NameCheck_ALLDATA_2.csv", row.names = T) #export it as a csv file
  
  
  return(NamesDF)
  
}
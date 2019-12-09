
##############################################################
##############################################################
# UPLOAD & STANDARDIZE DATA ON CARNEGIE CLASSIFICATIONS
##############################################################
##############################################################
Carnegie_raw<-read_csv("./Data/carnegie/CarnegCategories_2015.csv", col_names = TRUE)
source("CarnegieCats.R")
Carnegie<-CarnegieCats(Carnegie_raw)
rm(Carnegie_raw)

##############################################################
##############################################################
#
# ANALYSIS - GLOBAL
#
##############################################################
##############################################################
# ALLDATA<-both
ALLDATA<-read.csv("./output/ALLDATA.csv",stringsAsFactors = FALSE)

AnalysisData<-filter(ALLDATA,YEAR>"1984")

eds<-AnalysisData %>% summarise(n_distinct(editor_id))
eds


No_Inst<-AnalysisData %>% summarise(n_distinct(INST))
No_Inst

# Total Number of Inst (all jrnls pooled)  vs. Year

InstPerYr<-AnalysisData %>% group_by(YEAR) %>% summarize(InstPerYear = n_distinct(INST))
InstPerYr

EdsPerYr<-AnalysisData %>% group_by(YEAR) %>% summarize(EdsPerYear = n_distinct(editor_id))
EdsPerYr


# Total Editors from each Inst
EdsByInst<-AnalysisData %>% select(INST,editor_id)
EdsByInst<-EdsByInst %>% count(INST) 
EdsByInst<-EdsByInst %>% arrange(desc(n))

EdsByInst_20<-EdsByInst %>% slice(1:20)
sum(EdsByInst_20$n)/sum(EdsByInst$n)

##############################################################
##############################################################
#
# ANALYSIS - US INST ONLY
#
##############################################################
##############################################################

summary(ALLDATA$INST)

USA_INST<-AnalysisData %>% filter(COUNTRY=="USA")
USA_INST$INST<-as.factor(USA_INST$INST)
USA_INST<-droplevels(USA_INST)
levels(USA_INST$INST)
USA_INST$STATE<-as.character(USA_INST$STATE)
levels(USA_INST$STATE)
USA_INST$STATE
# USA_INST$STATE<-state.abb[grep(foo, state.name)]
# USA_INST$STATE
USA_INST$STATE[USA_INST$STATE=="Alabama"]<-"AL"
USA_INST$STATE[USA_INST$STATE=="Arizona"]<-"AZ"
USA_INST$STATE[USA_INST$STATE=="California"]<-"CA"
USA_INST$STATE[USA_INST$STATE=="Colorado"]<-"CO"
USA_INST$STATE[USA_INST$STATE=="Connecticut"]<-"CT"
USA_INST$STATE[USA_INST$STATE=="Washington DC"]<-"DC"
USA_INST$STATE[USA_INST$STATE=="Florida"]<-"FL"
USA_INST$STATE[USA_INST$STATE=="Idaho"]<-"ID"
USA_INST$STATE[USA_INST$STATE=="Illinois"]<-"IL"
USA_INST$STATE[USA_INST$STATE=="Kentucky"]<-"KY"
USA_INST$STATE[USA_INST$STATE=="Louisiana"]<-"LA"
USA_INST$STATE[USA_INST$STATE=="Lousiana"]<-"LA"
USA_INST$STATE[USA_INST$STATE=="South Dakota"]<-"SD"
USA_INST$STATE[USA_INST$STATE=="Michigan"]<-"MI"
USA_INST$STATE[USA_INST$STATE=="Maine"]<-"ME"
USA_INST$STATE[USA_INST$STATE=="Virginia"]<-"VA"
USA_INST$STATE[USA_INST$STATE=="New Jersey"]<-"NJ"
USA_INST$STATE[USA_INST$STATE=="Rhode Island"]<-"RI"

USA_INST$STATE[USA_INST$STATE=="Utah"]<-"UT"
USA_INST$STATE[USA_INST$STATE=="Texas"]<-"TX"
USA_INST$STATE[USA_INST$STATE=="Tennessee"]<-"TN"
USA_INST$STATE[USA_INST$STATE=="Wisconsin"]<-"WI"
USA_INST$STATE[USA_INST$STATE=="West Virginia"]<-"WV"
USA_INST$STATE[USA_INST$STATE=="VI"]<-"VA"
USA_INST$STATE[USA_INST$STATE=="West Virgina"]<-"WV"
USA_INST$STATE[USA_INST$STATE=="South Carolina"]<-"SC"
USA_INST$STATE[USA_INST$STATE=="Washington"]<-"WA"
USA_INST$STATE[USA_INST$STATE=="Washington "]<-"WA"
USA_INST$STATE[USA_INST$STATE=="Wyoming"]<-"WY"
USA_INST$STATE[USA_INST$STATE=="Maryland"]<-"MD"
USA_INST$STATE[USA_INST$STATE=="Massachusetts"]<-"MA"
USA_INST$STATE[USA_INST$STATE=="Mississippi"]<-"MS"
USA_INST$STATE[USA_INST$STATE=="PE"]<-"PA"
USA_INST$STATE[USA_INST$STATE=="Minnesota"]<-"MN"
USA_INST$STATE[USA_INST$STATE=="North Dakota"]<-"ND"
USA_INST$STATE[USA_INST$STATE=="North Carolina"]<-"NC"
USA_INST$STATE[USA_INST$STATE=="Nevada"]<-"NV"
USA_INST$STATE[USA_INST$STATE=="New Hampshire"]<-"NH"
USA_INST$STATE[USA_INST$STATE=="New Mexico"]<-"NM"
USA_INST$STATE[USA_INST$STATE=="New York"]<-"NY"
USA_INST$STATE[USA_INST$STATE=="Nebraska"]<-"NE"
USA_INST$STATE[USA_INST$STATE=="Montana"]<-"MT"
USA_INST$STATE[USA_INST$STATE=="Missouri"]<-"MO"
USA_INST$STATE[USA_INST$STATE=="Oregon"]<-"AL"
USA_INST$STATE[USA_INST$STATE=="NoState"]<-NA
USA_INST$STATE[USA_INST$STATE=="Pennsylvania"]<-"PA"
USA_INST$STATE[USA_INST$STATE=="Puerto Rico"]<-"PR"
USA_INST$STATE[USA_INST$STATE=="Alaksa"]<-"AK"
USA_INST$STATE[USA_INST$STATE=="Alaska"]<-"AK"
USA_INST$STATE[USA_INST$STATE=="District of Columbia"]<-"DC"
USA_INST$STATE[USA_INST$STATE=="Iowa"]<-"IA"
USA_INST$STATE[USA_INST$STATE=="Vermont"]<-"VT"
USA_INST$STATE[USA_INST$STATE=="Arkansa"]<-"AR"
USA_INST$STATE[USA_INST$STATE=="Arkansas"]<-"AR"
USA_INST$STATE[USA_INST$STATE=="Kansas"]<-"KS"
USA_INST$STATE[USA_INST$STATE=="Georgia"]<-"GA"
USA_INST$STATE[USA_INST$STATE=="Hawaii"]<-"HI"
USA_INST$STATE[USA_INST$STATE=="Oklahoma"]<-"OK"
USA_INST$STATE[USA_INST$STATE=="Indiana"]<-"IN"
USA_INST$STATE[USA_INST$STATE=="Ohio"]<-"OH"
USA_INST$STATE[USA_INST$STATE==""]<-NA
USA_INST$STATE<-as.factor(USA_INST$STATE)
USA_INST$STATE<-droplevels(USA_INST$STATE)
levels(USA_INST$STATE)
summary(USA_INST$STATE)
# nlevels(USA_INST$STATE)

#Need to match up the names used in Carengie Classification with names used in ALLDATA
str(CarnData)
CarnData_names<-CarnData %>% select(NAME,CITY,STABBR,Category)  
CarnData_names<-CarnData_names %>% filter(Category=="Doctoral"|Category=="Masters"|Category=="Baccalaureate"|Category=="Tribal")
CarnData_names$Category<-droplevels(CarnData_names$Category)
summary(CarnData_names)

str(USA_INST)
USA_INST_names<-USA_INST %>% select(INST,CITY,STATE)  
USA_INST_names$STATE<-as.character(USA_INST_names$STATE)
str(USA_INST_names)
USA_INST_names<-distinct(USA_INST_names)
foo<-USA_INST_names$INST
foo2<-USA_INST_names$INST
foo3<- cbind.data.frame(foo,foo2)
str(foo3)
names(foo3)[1] <- "Name1"
names(foo3)[2] <- "Name2"
foo3$Name1<-as.character(foo3$Name1)
foo3$Name2<-as.character(foo3$Name2)
NamesList<-sapply(foo3$Name1,agrep,foo3$Name2, value=TRUE) 
NamesDF<-data.frame(
  Name1 = rep(names(NamesList), lapply(NamesList, length)),
  Name2 = unlist(NamesList))
NamesDF<-na.omit(NamesDF)
NamesDF<-distinct(NamesDF)
str(NamesList)

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
head(NamesDF)
#return(NamesDF)
write.csv(NamesDF, file="./Data/InstNameCheck_USA.csv", row.names = T) #export it as a csv file



USA_INST$editor_id<-as.factor(USA_INST$editor_id)
summary(USA_INST$editor_id)
USA_INST$INST<-as.character(USA_INST$INST)
# foo<-USA_INST$INST
foo<-USA_INST

foo<-as.data.frame(foo)
foo<-foo %>% rename(NAME=INST)
foo$NAME<-as.character(foo$NAME)
foo$source<-"class"
str(foo)
# foo<-foo %>% filter(NAME != "NA")
# foo <- foo %>% group_by(NAME) %>% filter(row_number(source) == 1) %>% arrange(NAME)
foo <- foo %>% group_by(editor_id) %>% filter(row_number(source) == 1) %>% arrange(NAME)

# foo<-as.data.frame(foo)

CarnData$NAME<-as.character(CarnData$NAME)
foo2<-CarnData
# foo2<-na.omit(as.data.frame(foo2))
# foo2<-foo2 %>% rename(NAME=foo2)
foo2$source<-"carnegie"
# foo2<-unlist(foo2)
# foo2<-as.data.frame(foo2)
str(foo2)
foo3<-full_join(foo,foo2,by="NAME", "Source")
foo3$editor_id<-as.numeric(foo3$editor_id)
foo3<-filter(foo3,editor_id>0)
write.csv(foo3,file="./Data/ESA2018_USA_Ed_Inst_Carneg.csv")


USA_ED_clean<-read_csv("./Data/ESA2018_USA_Ed_Inst_Carneg.csv", col_names=TRUE)

USA_ED_clean<-USA_ED_clean %>% filter(Classification != "NA")
USA_ED_clean<-USA_ED_clean %>% filter(Classification != "(Not classified)")
USA_ED_clean$NAME<-as.factor(USA_ED_clean$NAME)
USA_ED_clean$Classification<-as.factor(USA_ED_clean$Classification)
USA_ED_clean<-droplevels(USA_ED_clean)
levels(USA_ED_clean$NAME)
levels(USA_ED_clean$Classification)
summary(USA_ED_clean$Classification)
summary<-USA_ED_clean$Classification
summary(summary)
foo4<-foo3
str(foo4)
foo5<-stringdistmatrix(foo3$NAME,method="dl")


INST<-na.omit(INST)
INST<-as.data.frame(INST)
summary(INST)
str(INST)
INST$INST<-as.factor(INST$INST)

inst_names <- INST %>% group_by(INST) %>% filter(row_number(COUNTRY) == 1) %>% arrange(INST)
write.csv(inst_names,file="INST_names_class.csv")

summary(foo4)

percents<-c(87.4,5.2,3.2,2.1,1.4,1.1,0.1)
lbls <- c("1-Doctoral Univ", "2-Fed Agency", "3-Garden/Museums/NGO/Private Inst.", "4-Baccalaureate Colleges", "5-Smithsonian", "6-Master's Colleges", "7-Industry")
data_barplot<-as.data.frame(cbind(percents,lbls))
data_barplot$percents<-as.numeric(as.character(data_barplot$percents))
# geom_bar is designed to make it easy to create bar charts that show
# counts (or sums of weights)
g <- ggplot(data_barplot, aes(lbls))+theme_bw()+theme(axis.text.x=element_text(angle = -45, hjust = 0))
plot<-g + geom_bar(aes(weight = percents), fill="darkblue",color="darkblue")
plot
str(data_barplot)
# Clear the environment 
rm(list=ls())

# Import list of world universities downloaded from this GitHub Repo: https://github.com/endSly/world-universities-csv
global_uni1 = read_csv("https://raw.githubusercontent.com/endSly/world-universities-csv/master/world-universities.csv", col_names=FALSE)
global_uni1<-global_uni1 %>% rename(ISO2=X1,uni.name=X2,website=X3)
global_uni1$uni.name<-as.factor(global_uni1$uni.name)
global_uni1$ISO2<-as.factor(global_uni1$ISO2)
global_uni1$website<-as.factor(global_uni1$website)
str(global_uni1)
summary(global_uni1)


# library("RSQLite")
# library("R.utils")
#download this sql: https://github.com/gedex/World-University-Names-Database/blob/master/world_university_names.sql
numLines <- R.utils::countLines("./Data/world_university_names.sql")
FullUniDB <- readLines("./Data/world_university_names.sql",n=numLines)


# CREATE THE DATABASE OF COUNTRY CODES & NAMES
countries.df<-FullUniDB[34:279]

countries.df<-gsub("(", "", countries.df, fixed=TRUE)
countries.df<-gsub("'),", "", countries.df, fixed=TRUE)
countries.df<-gsub("'", "", countries.df, fixed=TRUE)
countries.df<-gsub(";", "", countries.df, fixed=TRUE)
countries.df<-gsub(")", "", countries.df, fixed=TRUE)
countries.df<-as.data.frame(countries.df)
countries.df$countries.df<-as.character(countries.df$countries.df)
countries.df<-separate(countries.df, countries.df, c("country.ID", "ISO2","ISO3","country","modifier"), sep = ", ", remove = FALSE, convert = FALSE, extra = "merge", fill = "right")
countries.df$country[countries.df$ISO3 == "VGB"]  <- "British Virgin Islands"
countries.df$country[countries.df$ISO3 == "PRK"]  <- "Democratic Peoples Republic of Korea (NK)"
countries.df$country[countries.df$ISO3 == "COD"]  <- "Democratic Republic of the Congo"
countries.df$country[countries.df$ISO3 == "FSM"]  <- "Federated States of Micronesia"
countries.df$country[countries.df$ISO3 == "VIR"]  <- "US Virgin Islands"
countries.df$country[countries.df$ISO3 == "KOR"]  <- "Republic of Korea (SK)"
countries.df$country[countries.df$ISO3 == "PSE"]  <- "Palestinian Territory, Occupied"
countries.df$country[countries.df$ISO3 == "IRN"]  <- "Iran, Islamic Republic of"
countries.df$country[countries.df$ISO3 == "TZA"]  <- "Tanzania, United Republic of"
countries.df<-countries.df %>% select(-countries.df,-modifier)
countries.df$country.ID<-as.factor(countries.df$country.ID)
countries.df<-countries.df %>% rename(ISO2=ISO2 ,ISO3=ISO3 ,country=country )
countries.df$ISO2<-as.factor(countries.df$ISO2)
countries.df$ISO3<-as.factor(countries.df$ISO3)
countries.df$country<-as.factor(countries.df$country)



# CREATE THE UNIVERSITY DATABASE #1
unis.df <-FullUniDB[300:9537]
# NB better to go in 1 comma, 1in one comma, in from last comma (URL and then the rest is inst name)

# Change to Split at first comma, then at second, then done.
unis.df<-gsub("(", "", unis.df, fixed=TRUE)
unis.df<-gsub("(", "", unis.df, fixed=TRUE)
unis.df<-gsub(")", "", unis.df, fixed=TRUE)
unis.df <-gsub(", '", ",", unis.df , fixed=TRUE)
unis.df <-gsub("''", "'", unis.df , fixed=TRUE)
unis.df <-gsub("',", ",", unis.df , fixed=TRUE)
unis.df <-gsub("'", "", unis.df , fixed=TRUE)
unis.df <-unis.df %>% str_split(",",simplify=TRUE) 
unis.df <-as.data.frame(unis.df)
unis.df <- sapply(unis.df[1:ncol(unis.df)],as.character)
unis.df <-as.data.frame(unis.df)

str(unis.df)
unis.df$V3<-trimws(unis.df$V3)
unis.df$V4<-trimws(unis.df$V4)
unis.df$V5<-trimws(unis.df$V5)
unis.df$V6<-trimws(unis.df$V6)

unis.df<-unis.df%>% unite_("uni.name", c("V3","V4","V5","V6"), sep=" ", remove=TRUE)
unis.df<-unis.df %>% rename(uni.ID=V1,country.ID=V2) %>% select(-uni.ID,-V7)

#Remove some extra spaces
unis.df$uni.name <-trimws(unis.df$uni.name, which = "right")
unis.df$uni.name<-gsub("  ", " ", unis.df$uni.name, fixed=TRUE)
#delete some prolematic rows
unis.df<-unis.df[-c(1024, 2061, 2958, 3953,5084,6156,7144,7895,8884),]
unis.df$country.ID<-droplevels(unis.df$country.ID)
unis.df$country.ID<-trimws(unis.df$country.ID)

###################################################################
# CREATE THE UNIVERSITY DATABASE #2 (this one has the web addresses)
unis.web.df <-FullUniDB[9559:26356]
unis.web.df <-gsub("(", "", unis.web.df , fixed=TRUE)
unis.web.df <-gsub(", '", ",", unis.web.df , fixed=TRUE)
unis.web.df <-gsub("',", ",", unis.web.df , fixed=TRUE)
unis.web.df <-gsub("/'),", "", unis.web.df , fixed=TRUE)
unis.web.df <-gsub("''", "'", unis.web.df , fixed=TRUE)
unis.web.df <-gsub("  ", " ", unis.web.df , fixed=TRUE)

unis.web.df <-unis.web.df %>% str_split(",",n=3,simplify=TRUE) 
unis.web.df<-as.data.frame(unis.web.df)
unis.web.df$V3<-as.character(unis.web.df$V3)
str(unis.web.df)
unis.web.df[1:10,]
# https://stackoverflow.com/questions/24938616/string-split-on-last-comma-in-r
uniweb<-str_split(unis.web.df$V3, ",\\s*(?=[^,]+$)", simplify=TRUE)
uniweb<-as.data.frame(uniweb)

unis.web.df<-bind_cols(unis.web.df, uniweb)
unis.web.df<-unis.web.df %>% rename(uni.ID=V1,country.ID=V2,original=V3,uni.name=V11,website=V21)
unis.web.df<-unis.web.df %>% select(-original, -uni.ID)
rm(uniweb)
#Remove asterisk and some extra spaces
unis.web.df$uni.name <-gsub("*", "", unis.web.df$uni.name , fixed=TRUE)
unis.web.df$uni.name <-trimws(unis.web.df$uni.name, which = "right")
# add slash to match website in other df 
unis.web.df$website <- paste(unis.web.df$website, "/", sep="")

unis.web.df$country.ID<-as.factor(unis.web.df$country.ID)


unis.web.df<-unis.web.df[-c(682,1307, 1964, 2648, 3353, 4009, 4673, 5234, 5878, 6524, 7202, 7870, 8598, 9264,9900,10558,11144,11738,12379,13066,13770,14497,15216,15922,16617),]
unis.web.df$country.ID<-droplevels(unis.web.df$country.ID)
unis.web.df$country.ID<-trimws(unis.web.df$country.ID)

## NOTE THAT SOME OF THE UNIVERSITIES HAVE WEBSOTES BUT NO UNINAMES!!!



###################################
#add ISO2 and ISO3 to the uni databases
####################################
# for global_uni1
global_uni1<-left_join(global_uni1,countries.df, by="ISO2")
global_uni1$ISO2<-as.factor(global_uni1$ISO2)
global_uni1<-global_uni1 %>% select(country.ID,country,ISO2,ISO3,uni.name,website)

# for unis.df
unis.df<-left_join(unis.df,countries.df, by="country.ID")

# for unis.web.df
unis.web.df<-left_join(unis.web.df,countries.df, by="country.ID")
unis.web.df<-unis.web.df %>% select(country.ID,country,ISO2,ISO3,uni.name,website)

# Consolidate the three
# First might be easiest to convert the diacritical marks / accents
# use stri_trans_general() from stringi library
unis.web.df$uni.name2<-stri_trans_general(unis.web.df$uni.name, "Latin-ASCII")
unis.df$uni.name2<-stri_trans_general(unis.df$uni.name, "Latin-ASCII")
global_uni1$uni.name2<-stri_trans_general(global_uni1$uni.name, "Latin-ASCII")

# The remove any hyphens or commas
unis.web.df$uni.name2<-gsub(", ", " ", unis.web.df$uni.name2, fixed=TRUE)
unis.df$uni.name2<-gsub(", ", " ", unis.df$uni.name2 , fixed=TRUE)
global_uni1$uni.name2<-gsub(", ", " ", global_uni1$uni.name2 , fixed=TRUE)

# Are there any in unis.df NOT in unis.web.df when searching by uni.name?
foo1<-anti_join(unis.df,unis.web.df,by="uni.name2") #6523 of the 9229 
#do similarity analyses of these with unis.web.df$names2 and after removing any similar add to the master list

# Are there any in global_uni1 NOT in unis.web.df when searching by uni.name?
foo2<-anti_join(global_uni1,unis.web.df,by="website")  
foo3<-anti_join(global_uni1,unis.web.df,by="uni.name2") 
foo4<-bind_rows(foo2,foo3) #put them together
foo5<-unique(foo4,by="uni.name2") #remove the duplicates

#put the two datafiles together 
foo6<-bind_rows(foo1,foo5)
foo7<-unique(foo6)

# Consolidate them with unis.web.df
Consolidated.uni.df<-rbind(foo7,unis.web.df)
Consolidated.uni.df$uni.ID<-1:nrow(Consolidated.uni.df)
rm(foo1,foo2,foo3,foo4,foo5,foo6,foo7,global_uni1,unis.df,unis.web.df)

Consolidated.uni.df<-Consolidated.uni.df %>% select(uni.ID,country.ID,ISO2,ISO3,country,uni.name,uni.name2,website)
# to get the initials of each uni I just deleted lower-case letters http://r.789695.n4.nabble.com/Extract-upper-case-letters-td4634664.html
Consolidated.uni.df$initials<-gsub("[^::A-Z::]","", Consolidated.uni.df$uni.name2)
Consolidated.uni.df$uni.code <- paste(Consolidated.uni.df$ISO3,"-",Consolidated.uni.df$initials,  sep="")

# Now do a similarity analysis of name remove the duplicates
# DO SOME ERROR CORRECTION:
# 1) SOME ARE MISSING UNI NAMES, EG, INST NAME IS "UNIVERSITY": COUNT CHARACTERS,SORT, AND LOOK AT ONES WITH LEAST CAHARACTERS
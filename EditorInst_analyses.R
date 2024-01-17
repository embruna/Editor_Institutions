

library(tidyverse)
analysis_data<-read_csv("./data_clean/InstitutionData_clean.csv")
colnames(analysis_data)<-tolower(colnames(analysis_data))

analysis_data$journal<-as.factor(analysis_data$journal)
analysis_data$inst<-as.factor(analysis_data$inst)
analysis_data<-analysis_data %>%    
  filter(!journal %in% c("najfm","agronomy","marecol","auk","condor")) %>%
  filter(!inst=="missing") %>% 
  drop_na(inst) %>% 
  select(-country_prior_class,-geo.code,
         # -geo.code_Prior_Class,
         -gender, -notes,-inst_check)


analysis_data<-droplevels(analysis_data)
##############################################################
##############################################################
# UPLOAD & STANDARDIZE DATA ON CARNEGIE CLASSIFICATIONS
##############################################################
##############################################################
# carnegie_raw<-read_csv("./Data/carnegie/CarnegCategories_2015.csv")
source("./functions_analysis/CarnegieCats_2021.R")
carnegie<-CarnegieCats_2021("basic2021")


##############################################################
##############################################################
#
# ANALYSIS - GLOBAL
#
##############################################################
##############################################################

str(analysis_data)
analysis_data$first_name<-as.factor(analysis_data$first_name)
analysis_data$middle_name<-as.factor(analysis_data$middle_name)
analysis_data$last_name<-as.factor(analysis_data$last_name)
analysis_data$title<-as.factor(analysis_data$title)
analysis_data$category<-as.factor(analysis_data$category)


# data overview -----------------------------------------------------------

range(analysis_data$year)
unique(analysis_data$journal)

##############################################################
first_year=1985
last_year=2014
analysis_data <- analysis_data %>% filter(year>0|is.na(year)==FALSE) %>% filter(year>=first_year) %>% filter(year<=last_year)
summary(analysis_data$year)

# HOW MANY journalS AND WHAT ARE THEY
jrnls<-analysis_data %>% summarise(n_distinct(journal))
jrnls_list<-analysis_data %>% 
  group_by(journal) %>% 
  summarize(n_distinct(journal))
jrnls_list

# EDS - HOW MANY AND HOW MANY PER journal
eds<-analysis_data %>% summarise(n_distinct(editor_id))
eds

eds_per_jrnl<-analysis_data %>% 
  group_by(journal) %>% 
  summarize(n_eds=n_distinct(editor_id)) %>% 
  arrange(desc(n_eds))
eds_per_jrnl

# inst and No PER journal
No_Inst<-analysis_data %>% summarise(n_distinct(inst))
No_Inst

inst_per_jrnl<-analysis_data %>% 
  group_by(journal) %>% 
  summarize(n_inst=n_distinct(inst)) %>% 
  arrange(desc(n_inst))
inst_per_jrnl


# inst PER EDITOR
inst_per_ed<-analysis_data %>% 
  group_by(journal) %>% 
  summarize(n_eds=n_distinct(editor_id),n_inst=n_distinct(inst)) %>% 
  mutate(inst_per_ed=n_inst/n_eds) %>% 
  arrange((inst_per_ed))
inst_per_ed



# Total Number of Inst (all jrnls pooled)  vs. Year
InstPerYr<-analysis_data %>% 
  group_by(year) %>% 
  summarize(InstPerYear = n_distinct(inst))
InstPerYr

EdsPerYr<-analysis_data %>% 
  group_by(year) %>% 
  summarize(EdsPerYear = n_distinct(editor_id))
EdsPerYr


# Total "Editorial Years" for each Inst
EdsByInst<-analysis_data %>% 
  select(inst,editor_id) %>% 
  count(inst) %>% 
  arrange(desc(n)) %>% 
  mutate(perc=n/sum(n)*100)
EdsByInst


topX<-10
EdsByInst_topX<-EdsByInst %>% 
  slice(1:topX)
EdsByInst_topX


# Inst of individual editors (accross all years)
Inst_by_Ed<-analysis_data %>% 
  select(inst,editor_id,country) %>% 
  group_by(inst,country) %>% 
  summarize(n=n_distinct(editor_id)) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  mutate(perc=n/sum(n)*100) %>% 
  mutate(cumPerc=cumsum(perc))
Inst_by_Ed
sum(Inst_by_Ed$n)

topY<-200
Inst_by_Ed_topY<-Inst_by_Ed %>% 
  slice(1:topY)
Inst_by_Ed_topY


Ed_by_name<-analysis_data %>% 
  group_by(first_name) %>% 
  summarize(n=n_distinct(editor_id)) %>% 
  arrange(desc(n)) %>% 
  mutate(cumulative=cumsum(n),cumper=cumulative/sum(n)*100) 
Ed_by_name


Ed_by_name_LAST<-analysis_data %>% 
  group_by(last_name) %>% 
  summarize(n=n_distinct(editor_id)) %>% 
  arrange(desc(n)) %>% 
  mutate(cumulative=cumsum(n),cumper=cumulative/sum(n)*100) 
Ed_by_name_LAST



editors <- analysis_data %>% select(first_name,last_name,middle_name,editor_id)
years_per_person<-analysis_data %>% 
  group_by(editor_id) %>% 
  summarize(yrs=n()) %>% 
  left_join(editors) %>% 
  group_by(editor_id) %>%
  slice(1) %>% 
  arrange(desc(yrs))





Ed_by_country<-analysis_data %>% 
  group_by(country) %>% 
  summarize(n=n_distinct(editor_id)) %>% 
  arrange(desc(n)) %>% 
  mutate(cumulative=cumsum(n),cumper=cumulative/sum(n)*100) 
Ed_by_country

##############################################################
##############################################################

# ANALYSIS - ADD country CODES AND WORLD BANK CATEGORIES

##############################################################
##############################################################
names(analysis_data)

source("./functions_analysis/country_codes.R")
analysis_data<-country_codes(analysis_data)

analysis_data<-analysis_data 
source("./functions_analysis/AddIncomeRegion.R")
analysis_data<-AddIncomeRegion(analysis_data)




##############################################################
##############################################################
#
# ANALYSIS - RICHNESS AND DIVERSITY 


##############################################################
##############################################################
# Cumulative Institutional Richness: Cumulative Number of countries represented through year X
# Used in Figure 1A
# Use Rarefaction curves generated by vegan then convert back to tibble
library(vegan)

InstAcum<-analysis_data %>% group_by(year,inst) %>% summarize(yr_tot = n_distinct(inst))
InstAcum<-spread(InstAcum, inst,yr_tot) 
InstAcum[is.na(InstAcum)] <- 0
InstAcum<-ungroup(InstAcum)
InstAcum<-select(InstAcum,-year)
InstAcum<-specaccum(InstAcum, "collector")
InstAcum<-as_tibble(InstAcum$richness)
names(InstAcum)[1] <- "CumulativeRichness"
InstAcum$year<-seq(first_year,last_year,1)
InstAcum

##############################################################
# Annual Institutional Richness: Number of countries represented in year X 
# Used in Figure ---
instperYR<-analysis_data %>% group_by(year) %>% summarize(AnnualRichness = n_distinct(inst)) 
instperYR
##############################################################

##############################################################

##############################################################
# Institutional Diversity (all journals pooled) 
# Used in Figure --
DivDataPooled<-analysis_data %>% group_by(year, inst) %>% summarize(Total = n_distinct(editor_id)) 
# DivDataPooled<-as.data.frame(EdsPerCountryPerJrnlPerYr.LONG)
DivDataPooled<-DivDataPooled %>% group_by(year, inst) %>% summarise(Total_Eds=sum(Total))
DivDataPooled<-spread(DivDataPooled, inst, Total_Eds) 
DivDataPooled[is.na(DivDataPooled)] <- 0
DivDataPooled<-ungroup(DivDataPooled)
# 4: Geo Diverisity using Inverse Simpson's Index (expressed as 1/D)
IsimpDivTable <- diversity((DivDataPooled %>% select(-year)), index="invsimpson") #Need to strip away the journal and year columns for vegan to do the analysis
# Table DIVERSITY with Results and Journals
IsimpDivTable <- data.frame(IsimpDivTable)
IsimpDivTable$year <-DivDataPooled$year #Add year as a column
IsimpDivTable<-rename(IsimpDivTable, InvSimpson=IsimpDivTable) #rename the columns
IsimpDivTable <- IsimpDivTable[c("year","InvSimpson")] #reorder the columns
IsimpDivTable<-as_tibble(IsimpDivTable)

# THIS CALCLULATES THE SIMPSONS INDEX (expressed as 1-D)
simpDivTable <- diversity((DivDataPooled %>% select(-year)), index="simpson") #Need to strip away the journal and year columns for vegan to do the analysis
# Table DIVERSITY with Results and Journals
simpDivTable <- data.frame(simpDivTable)
simpDivTable$year <-DivDataPooled$year #Add year as a column
simpDivTable<-rename(simpDivTable, Simpson=simpDivTable) #rename the columns
simpDivTable <- simpDivTable[c("year","Simpson")] #reorder the columns
simpDivTable<-as_tibble(simpDivTable)

IsimpDivTable<-full_join(instperYR,IsimpDivTable, by="year")
IsimpDivTable<-full_join(IsimpDivTable,simpDivTable, by="year")
rm(simpDivTable)



# 4: Institutional Evenness (all journals pooled). We do not present results 
# for "Evenness" of the Institutions but it is straightforward to calculate.
IsimpDivTable<-mutate(IsimpDivTable, Inst.Evenness = InvSimpson/AnnualRichness)
IsimpDivTable


##############################################################
# Fig 1A: Total Number of Editors (all journals pooled)  vs. Year
##############################################################

plotTOTALedsYear<-ggplot(EdsPerYr, aes(x=year, y=EdsPerYear)) +
  xlab("Year")+
  ylab("N")+
  geom_line(size=1, color="blue")+
  ggtitle('(A) Number of Editors') + 
  geom_point(color="black", shape=1)+
  scale_y_continuous(breaks=seq(0, 1500, 150))+
  scale_x_continuous(breaks=seq(first_year, last_year+1, 5))

plotTOTALedsYear<-plotTOTALedsYear+theme_classic()+
  theme( axis.text=element_text(colour="black", size = 10),  
         legend.text = element_text(color="black", size=10),  
         legend.position = c(0.9,0.8),
         legend.title = element_blank(),   #Removes the Legend title
         plot.margin=unit(c(1,5,1,1),"lines"),
         legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
plotTOTALedsYear



##############################################################
# Fig. 1B: Cumulative Inst Richness
##############################################################
# PUT THE NECESSARY DATA IN ONE DATAFRAME 
jointinstperYR<-instperYR
jointedAccDF<-InstAcum

jointInstperYR<-rename(jointinstperYR,Countries=AnnualRichness)
jointRichness<-full_join(jointinstperYR, jointedAccDF, by = "year")
jointRichness<-gather(jointRichness, "Richness","N", 2:3)
jointRichness[jointRichness=="Countries"]<-"Annual"
jointRichness[jointRichness=="CumulativeRichness"]<-"Cumulative"
rm(jointInstperYR,jointedAccDF)

#plot cumulative and annual richness same plot
jointRichnessPlot<-ggplot(jointRichness, aes(x=year, y=N, group = Richness, colour = Richness)) +
  geom_line(size=1) +
  scale_color_manual(values=c("blue", "red"))+
  geom_text(data = jointRichness[jointRichness$year=="2012" & jointRichness$Richness=="Annual",], aes(label = Richness), hjust = 1.0, vjust = 2.5, size=3.5) +
  geom_text(data = jointRichness[jointRichness$year=="2012" & jointRichness$Richness=="Cumulative",], aes(label = Richness), hjust = 2.0, vjust = 1.35, size=3.5) +
  xlab("Year")+
  ylab("Richness")+
  ggtitle('(B) Editor Instgraphic Richness')+
  geom_point(color="black", shape=1)+
  scale_y_continuous(limits = c(25, 1500))+
  scale_x_continuous(breaks=seq(first_year, last_year+1, 5))

jointRichnessPlot<-jointRichnessPlot+theme_classic()+
  theme(axis.text=element_text(colour="black", size = 10),         #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        plot.margin=unit(c(1,5,1,1),"lines"),
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10),  
        legend.position = ("none"),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
jointRichnessPlot

##############################################################
# Plot 1C: COMMunitY (POOLED journalS) LEVEL DIVERSITY
##############################################################
IsimpDivTable$year
IsimpDivTable$InvSimpson
plotPOOLinstimpdiv<-ggplot(IsimpDivTable, aes(x=year, y=InvSimpson)) +
  geom_line(size=1, color="blue") + # Use hollow circles
  xlab("Year")+
  ylab(bquote('D'[2]))+
  ggtitle('(C) Editor Institutional Diversity')+
  geom_point(color="black", shape=1)+
  scale_y_continuous(limits = c(0, 250))+
  scale_x_continuous(breaks=seq(first_year, last_year+1, 5))

plotPOOLinstimpdiv<-plotPOOLinstimpdiv+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 10),
        plot.margin=unit(c(1,5,1,1),"lines"),
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10),  
        legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
plotPOOLinstimpdiv



######################################################
# Binding these up to make Fig. 1
######################################################



##############################################################
# Fig 2A: bar chart of countries with the most unique Institutions  
##############################################################

##############################################################
# Number and Pcnt of Editors from Each Country (all journals and years pooled)
# Used for Fig 2A
Editor.Inst<-analysis_data %>%  group_by(inst) %>% 
  summarize(N_Inst = n_distinct(editor_id)) %>% 
  mutate(Pcnt_Inst= (N_Inst/sum(N_Inst)*100)) %>% 
  arrange(desc(Pcnt_Inst))
Editor.Inst


cutoff = 50 # This is how many countries you want on the chart, all the rest will be in "OTHER"
editor.inst<-arrange(Editor.Inst, desc(Pcnt_Inst)) %>% select(inst,N_Inst,Pcnt_Inst)
most.common.Institutions<-slice(Editor.Inst, 1:cutoff)
least.common.Institutions<-slice(Editor.Inst, (cutoff+1):nrow(Editor.Inst)) 
least.common.Institutions$geo.code<-"OTHER"
least.common.Institutions<-least.common.Institutions %>% 
  mutate(sum(N_Inst)) %>%
  mutate(sum(Pcnt_Inst)) %>% 
  select(-N_Inst) %>% 
  select(-Pcnt_Inst) %>% 
  rename(N_Inst = `sum(N_Inst)`) %>% 
  rename(Pcnt_Inst = `sum(Pcnt_Inst)`) %>% 
  slice(1:1)
most.common.Institutions<-bind_rows(most.common.Institutions, least.common.Institutions)
# most.common.Institutions$inst<-as.factor(most.common.Institutions$inst)
most.common.Institutions

# This is needed to put them in order in the plot with OTHER at the end of the graph
order<-seq(1:nrow(most.common.Institutions))
most.common.Institutions$inst<- factor(most.common.Institutions$inst,most.common.Institutions$inst[levels = order])
# levels(most.common.Institutions$geo.code)
rm(order,editor.inst,least.common.Institutions)

InstED<-arrange(most.common.Institutions) %>%  ggplot(aes(x=inst, y=Pcnt_Inst)) +
  geom_bar(colour="black", stat="identity")+
  coord_flip()+
  ylab("Percent") +
  xlab("Institution")+
  ggtitle('(A) Editors by Institution')+
  scale_y_continuous(breaks=seq(0, 70, 5))
InstED<-InstED+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 10),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10),  
        legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
InstED


###########################3

# Second question
years_with_an_editor<-analysis_data %>% select(year,inst) %>% 
  distinct(year,inst) %>%  
  # Count how many observations had counts > 0 for each site
  group_by(inst) %>%
  summarize(years=n()) %>% 
  arrange(desc(years))
years_with_an_editor
##############################################################
##############################################################
#
# ANALYSIS - US inst ONLY
#
##############################################################
##############################################################



##############################################################
##############################################################
#
# ANALYSIS - US inst ONLY
#
##############################################################
##############################################################




summary(analysis_data$inst)

USA_inst<-analysis_data %>% filter(geo.code=="USA")
USA_inst$inst<-as.factor(USA_inst$inst)
USA_inst<-droplevels(USA_inst)
levels(USA_inst$inst)
USA_inst$state<-as.character(USA_inst$state)
levels(USA_inst$state)
USA_inst$state
# USA_inst$state<-state.abb[grep(foo, state.name)]
# USA_inst$state
USA_inst$state[USA_inst$state=="alabama"]<-"AL"
USA_inst$state[USA_inst$state=="arizona"]<-"AZ"
USA_inst$state[USA_inst$state=="california"]<-"CA"
USA_inst$state[USA_inst$state=="colorado"]<-"CO"
USA_inst$state[USA_inst$state=="connecticut"]<-"CT"
USA_inst$state[USA_inst$state=="washington dc"]<-"DC"
USA_inst$state[USA_inst$state=="florida"]<-"FL"
USA_inst$state[USA_inst$state=="idaho"]<-"ID"
USA_inst$state[USA_inst$state=="illinois"]<-"IL"
USA_inst$state[USA_inst$state=="kentucky"]<-"KY"
USA_inst$state[USA_inst$state=="louisiana"]<-"LA"
USA_inst$state[USA_inst$state=="lousiana"]<-"LA"
USA_inst$state[USA_inst$state=="south dakota"]<-"SD"
USA_inst$state[USA_inst$state=="michigan"]<-"MI"
USA_inst$state[USA_inst$state=="maine"]<-"ME"
USA_inst$state[USA_inst$state=="virginia"]<-"VA"
USA_inst$state[USA_inst$state=="new jersey"]<-"NJ"
USA_inst$state[USA_inst$state=="rhode island"]<-"RI"

USA_inst$state[USA_inst$state=="utah"]<-"UT"
USA_inst$state[USA_inst$state=="texas"]<-"TX"
USA_inst$state[USA_inst$state=="tennessee"]<-"TN"
USA_inst$state[USA_inst$state=="wisconsin"]<-"WI"
USA_inst$state[USA_inst$state=="west virginia"]<-"WV"
USA_inst$state[USA_inst$state=="vi"]<-"VA"
USA_inst$state[USA_inst$state=="west virgina"]<-"WV"
USA_inst$state[USA_inst$state=="south carolina"]<-"SC"
USA_inst$state[USA_inst$state=="washington"]<-"WA"
USA_inst$state[USA_inst$state=="washington "]<-"WA"
USA_inst$state[USA_inst$state=="wyoming"]<-"WY"
USA_inst$state[USA_inst$state=="maryland"]<-"MD"
USA_inst$state[USA_inst$state=="massachusetts"]<-"MA"
USA_inst$state[USA_inst$state=="mississippi"]<-"MS"
USA_inst$state[USA_inst$state=="pe"]<-"PA"
USA_inst$state[USA_inst$state=="minnesota"]<-"MN"
USA_inst$state[USA_inst$state=="north dakota"]<-"ND"
USA_inst$state[USA_inst$state=="north carolina"]<-"NC"
USA_inst$state[USA_inst$state=="nevada"]<-"NV"
USA_inst$state[USA_inst$state=="new hampshire"]<-"NH"
USA_inst$state[USA_inst$state=="new mexico"]<-"NM"
USA_inst$state[USA_inst$state=="new york"]<-"NY"
USA_inst$state[USA_inst$state=="nebraska"]<-"NE"
USA_inst$state[USA_inst$state=="montana"]<-"MT"
USA_inst$state[USA_inst$state=="missouri"]<-"MO"
USA_inst$state[USA_inst$state=="oregon"]<-"AL"
USA_inst$state[USA_inst$state=="nostate"]<-NA
USA_inst$state[USA_inst$state=="pennsylvania"]<-"PA"
USA_inst$state[USA_inst$state=="puerto rico"]<-"PR"
USA_inst$state[USA_inst$state=="alaksa"]<-"AK"
USA_inst$state[USA_inst$state=="alaska"]<-"AK"
USA_inst$state[USA_inst$state=="district of columbia"]<-"DC"
USA_inst$state[USA_inst$state=="iowa"]<-"IA"
USA_inst$state[USA_inst$state=="vermont"]<-"VT"
USA_inst$state[USA_inst$state=="arkansa"]<-"AR"
USA_inst$state[USA_inst$state=="arkansas"]<-"AR"
USA_inst$state[USA_inst$state=="kansas"]<-"KS"
USA_inst$state[USA_inst$state=="georgia"]<-"GA"
USA_inst$state[USA_inst$state=="hawaii"]<-"HI"
USA_inst$state[USA_inst$state=="oklahoma"]<-"OK"
USA_inst$state[USA_inst$state=="indiana"]<-"IN"
USA_inst$state[USA_inst$state=="ohio"]<-"OH"
USA_inst$state[USA_inst$state==""]<-NA
USA_inst<-USA_inst %>% 
  mutate(state=tolower(state))

USA_inst$state<-as.factor(USA_inst$state)
USA_inst$state<-droplevels(USA_inst$state)
levels(USA_inst$state)
summary(USA_inst$state)
# nlevels(USA_inst$state)

#Need to match up the names used in Carengie Classification with names used in ALLDATA
str(CarnData)
CarnData_names<-CarnData %>% select(NAME,city,STABBR,Category)  
CarnData_names<-CarnData_names %>% filter(Category=="Doctoral"|Category=="Masters"|Category=="Baccalaureate"|Category=="Tribal")
CarnData_names$Category<-droplevels(CarnData_names$Category)
summary(CarnData_names)

str(USA_inst)
USA_inst_names<-USA_inst %>% select(inst,city,state)  
USA_inst_names$state<-as.character(USA_inst_names$state)
str(USA_inst_names)
USA_inst_names<-distinct(USA_inst_names)
foo<-USA_inst_names$inst
foo2<-USA_inst_names$inst
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



USA_inst$editor_id<-as.factor(USA_inst$editor_id)
summary(USA_inst$editor_id)
USA_inst$inst<-as.character(USA_inst$inst)
# foo<-USA_inst$inst
foo<-USA_inst

foo<-as.data.frame(foo)
foo<-foo %>% rename(NAME=inst)
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


inst<-na.omit(inst)
inst<-as.data.frame(inst)
summary(inst)
str(inst)
inst$inst<-as.factor(inst$inst)

inst_names <- inst %>% group_by(inst) %>% filter(row_number(country) == 1) %>% arrange(inst)
write.csv(inst_names,file="inst_names_class.csv")

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


# CREATE THE DATABASE OF country CODES & NAMES
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
# 1) SOME ARE MISSING UNI NAMES, EG, inst NAME IS "UNIVERSITY": COUNT CHARACTERS,SORT, AND LOOK AT ONES WITH LEAST CAHARACTERS

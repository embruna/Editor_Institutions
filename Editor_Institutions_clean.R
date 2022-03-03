 # Introduction ------------------------------------------------------------

# CODE FOR IMPORTING, MANIPULATING, AND ANALYZING DATA ON THE 
# INSTITUTIONS AT WHICH JOURNAL EDITORS ARE BASED

# load libraries ----------------------------------------------------------

library(tidyverse)
library(stringr)
library(stringi)
library(stringdist)
library(RecordLinkage)
# library(vegan)
# library(nlme)
# library(MuMIn)
# library(directlabels)
# library(grid)
# library(gridExtra)
# library(RColorBrewer)


# load & clean data from dryad ---------------------------------------------

cho_data <- read.csv("./Data/dryad/Dryad_Cho_V2.csv", dec = ".", header = TRUE, sep = ",", check.names = FALSE)
colnames(cho_data)<-tolower(colnames(cho_data))

# minor corrections to cho_data_V2 Caught by Bruna's 2017 SciWri Class
cho_data$country[cho_data$last_name == "Pedreira" & cho_data$first_name == "Carlos"] <- "Brazil"
cho_data$country[cho_data$last_name == "Benbi" & cho_data$first_name == "Dinesh"] <- "India"
cho_data$country[cho_data$last_name == "Borras" & cho_data$first_name == "Lucas"] <- "Argentina"
cho_data$country[cho_data$last_name == "Esker" & cho_data$first_name == "Paul"] <- "Costa Rica"
cho_data$country[cho_data$last_name == "Buresh" & cho_data$first_name == "Roland"] <- "USA"

cho_data$country <- as.character(cho_data$country)
cho_data$country[cho_data$country == "UK"] <- "United Kingdom"
cho_data$country[cho_data$country == "USSR"] <- "Russia"
cho_data$country[cho_data$country == "ITALY"] <- "Italy"
cho_data$country[cho_data$country == "United States"] <- "USA"
cho_data$country[cho_data$country == "Usa"] <- "USA"
cho_data$country[cho_data$country == "UsA"] <- "USA"
# Convert Country back to factor
cho_data$country <- as.factor(cho_data$country)
droplevels(cho_data$country)
# Add a tag to ID the data source
cho_data$source <- "cho_dryad"

# load data from Espin et al. 2017 ----------------------------------------

espin_data <- read.csv("./Data/dryad/Dryad.Espin.v1.csv", dec = ".", header = TRUE, sep = ",", check.names = FALSE)
colnames(espin_data)<-tolower(colnames(espin_data))
# Add a tag to ID the data source
espin_data$source <- "espin_dryad"


# bind together espin and dryad data ---------------------------------------

dryad_data <- bind_rows(cho_data, espin_data, id = NULL)
# Delete these columns
dryad_data <- dryad_data %>% select(-geo.code, -income_level, -region)
levels(as.factor(dryad_data$journal))
rm(cho_data, espin_data)


# load raw data collected by sciwri 17 ------------------------------------
# path to folder that holds multiple .csv files
folder <- "./Data/sciwri17_raw_data/"
# create list of all .csv files in folder
file_list <- list.files(path = folder, pattern = "*.csv")
# read in each .csv file in file_list and create a data frame
# with the same name as the .csv file
# Note the ASCII encoding to try and deal with the UTF-8 characters
for (i in 1:length(file_list)) {
  assign(
    file_list[i],
    read.csv(paste(folder, file_list[i], sep = ""),
      na.strings = c("", "NA"), encoding = "ASCII"
    )
  )
}
rm(folder, file_list, i)


# load & standardize AUK, CONDOR collected by Hurtado (MALAS GA) ----------

folder <- "./Data/hurtado_data/" # path to folder that holds multiple .csv files
# create list of all .csv files in folder
file_list <- list.files(path = folder, pattern = "*.csv") 
# read each .csv in file_list & create data frame with same name as .csv file
# Note the ASCII encoding to try and deal with the UTF-8 characters
for (i in 1:length(file_list)) {
  assign(
    file_list[i],
    read.csv(paste(folder, file_list[i], sep = ""),
      na.strings = c("", "NA"), encoding = "ASCII"
    )
  )
}
rm(folder, file_list, i)



# clean up data for each journal ------------------------------------------


# Auk ---------------------------------------------------------------------

AUK_raw <- AUK.csv
source("functions_data_import/clean_AUK.R")
AUK <- clean_AUK(AUK_raw)
rm(AUK_raw, AUK.csv, clean_AUK)

# Condor ------------------------------------------------------------------

CONDOR_raw <- CONDOR.csv
source("functions_data_import/clean_CONDOR.R")
CONDOR <- clean_CONDOR(CONDOR_raw)
rm(CONDOR_raw, CONDOR.csv)


# Functional Ecol ---------------------------------------------------------

# FUNECOL HAS MULTIPLE DATASETS TO UPLOAD
FUNECOL1_raw <- FUNECOLdata_Allen_EB_1dec.csv
FUNECOL2_raw <- FUNECOL_data_11.03.2017.csv
# TODO: NEED TO CORRECT THE ONES BELOW WHERE category X AND Y DON'T MATCH.
# TODO: OPEN THE FUNCTION TO SEE WHICH ONES THEY ARE
source("functions_data_import/clean_FUNECOL.R")
FUNECOL <- clean_FUNECOL(FUNECOL1_raw, FUNECOL2_raw)
rm(
  FUNECOLdata_Allen_EB_1dec.csv, FUNECOL_data_11.03.2017.csv,
  FUNECOL1_raw, FUNECOL2_raw
)


# Lanscape Ecol -----------------------------------------------------------

LECO_raw <- LECO_2017.csv
source("functions_data_import/clean_LECO.R")
LECO <- clean_LECO(LECO_raw)
rm(LECO_2017.csv, LECO_raw)


# ConBio ------------------------------------------------------------------


CONBIO_raw <- CONBIO_EKB.csv
source("functions_data_import/clean_CONBIO.R")
CONBIO <- clean_CONBIO(CONBIO_raw)
rm(CONBIO_EKB.csv, CONBIO_raw)


# J Zoology ---------------------------------------------------------------

# JZOOL HAS MULTIPLE DATASETS TO UPLOAD
JZOOL1 <- JZOOL17nov.csv
JZOOL2 <- JZOOL.csv
# TODO: a bunch of institutions still missing
source("functions_data_import/clean_JZOOL.R")
JZOOL <- clean_JZOOL(JZOOL1, JZOOL2)
rm(JZOOL.csv, JZOOL17nov.csv, JZOOL1, JZOOL2)

# New Phytologist ---------------------------------------------------------

NEWPHYT_raw <- NEWPHYT_21july2018.csv
source("functions_data_import/clean_NEWPHYT.R")
NEWPHYT <- clean_NEWPHYT(NEWPHYT_raw)
rm(NEWPHYT_21july2018.csv, NEWPHYT_raw)


# Biological Conservation -------------------------------------------------

BIOCON_raw <- BIOCON_TS.csv
# TODO: STILL NEEED TO need to add the 2x names, fill in any Missing
# for inst, unit, etc, and the FILL the columns
source("functions_data_import/clean_BIOCON.R")
BIOCON <- clean_BIOCON(BIOCON_raw)
rm(BIOCON_TS.csv, BIOCON_raw)


# Agronomy ----------------------------------------------------------------

AG_raw <- AGRONOMY_data_11.02.2017.csv
source("functions_data_import/clean_AGRON.R")
AG <- clean_AGRON(AG_raw)
rm(AGRONOMY_data_11.02.2017.csv, AG_raw)

# Forest Ecol & Manag -----------------------------------------------------

FEM_raw <- FEM_7112017.csv
source("functions_data_import/clean_FEM.R")
FEM <- clean_FEM(FEM_raw)
rm(FEM_7112017.csv, FEM_raw)

# J Animal Ecol -----------------------------------------------------------

JANE_raw <- JANE.csv
source("functions_data_import/clean_JANE.R")
JANE <- clean_JANE(JANE_raw)
rm(JANE.csv, JANE_raw)


# J Biogeography ----------------------------------------------------------

JBIOG_raw <- JBIOG.csv
source("functions_data_import/clean_JBIOG.R")
JBIOG <- clean_JBIOG(JBIOG_raw)
rm(JBIOG_raw, JBIOG.csv)


# Plant Ecology -----------------------------------------------------------


PLANTECOL_raw <- PLANTECOL_new.csv
source("functions_data_import/clean_PLANTECOL.R")
PLANTECOL <- clean_PLANTECOL(PLANTECOL_raw)
rm(PLANTECOL_raw, PLANTECOL_new.csv)

# Evolution ---------------------------------------------------------------
EVOL_raw <- EVOL.csv
source("functions_data_import/clean_EVOL.R")
EVOL <- clean_EVOL(EVOL_raw)
rm(EVOL.csv, EVOL_raw)


# AmNat -------------------------------------------------------------------

AMNAT1_raw <- AMNAT.csv
AMNAT2_raw <- AmNat0614.csv
# TODO: FIGURE OUT WTF AM NAT
# TODO: AM NAT Needs institutions from 2006-2014 put in
# NEED TO FIGURE OUT IF AMNAT, AMNAT0614,orAMNATEKB!!!!!!!!!!!!!!
source("functions_data_import/clean_AMNAT.R")
AMNAT <- clean_AMNAT(AMNAT1_raw, AMNAT2_raw)
rm(AMNAT.csv, AmNat0614.csv, AMNAT1_raw, AMNAT2_raw)
rm(AMNAT_EKB.csv)


# J Applied Ecol ----------------------------------------------------------

JAPE_raw <- JAPE_new.csv
source("functions_data_import/clean_JAPE.R")
JAPE <- clean_JAPE(JAPE_raw)
rm(JAPE_new.csv, JAPE_raw)

# Oecologia ---------------------------------------------------------------

OECOL_raw <- OECOL.csv
source("functions_data_import/clean_OECOL.R")
OECOL <- clean_OECOL(OECOL_raw)
rm(OECOL.csv, OECOL_raw)

# Global Change Bio -------------------------------------------------------

GCB_raw <- GCBdata.csv
# TODO:
# 2) get title abbreviations
# 3) Make consistent
# 4) only have data 1995-2007 (1995 was vol 1), 2008-2015
# 5) disambiguate editors and add editor id numbers
source("functions_data_import/clean_GCB.R")
GCB <- clean_GCB(GCB_raw)
rm(GCBdata.csv, GCB_raw)


# North Am J Fisheries Manag ----------------------------------------------

NAJFM_raw <- NAJFM_21july2018.csv
source("functions_data_import/clean_NAJFM.R")
NAJFM <- clean_NAJFM(NAJFM_raw)
rm(NAJFM_21july2018.csv, NAJFM_raw)

# Marine Ecology ----------------------------------------------------------

# TODO: MARECOL
# 2) Disambiguate and add editor ID
# 3) NEEDS instITIONS ADDED OR FILLED
MARECOL_raw <- MARECOL_21July2018.csv
source("functions_data_import/clean_MARECOL.R")
MARECOL <- clean_MARECOL(MARECOL_raw)
rm(MARECOL_21July2018.csv, MARECOL_raw)


# Am J Botany -------------------------------------------------------------

AJB_raw <- AJB.csv
source("functions_data_import/clean_AJB.R")
AJB <- clean_AJB(AJB_raw)
rm(AJB.csv, AJB_raw)


# Oikos -------------------------------------------------------------------

OIKOS_raw <- OIKOS_21july2018.csv
source("functions_data_import/clean_OIKOS.R")
OIKOS <- clean_OIKOS(OIKOS_raw)
rm(OIKOS_21july2018.csv, OIKOS_raw)

# Ecography ---------------------------------------------------------------

ECOG_raw <- ECOGRAPHY_5112017.csv
source("functions_data_import/clean_ECOG.R")
ECOG <- clean_ECOG(ECOG_raw)
rm(ECOGRAPHY_5112017.csv, ECOG_raw)

# Biotropica --------------------------------------------------------------

BITR_raw <- BITR.csv
source("functions_data_import/clean_BITR.R")
BITR <- clean_BITR(BITR_raw)
rm(BITR.csv, BITR_raw)


# J Ecology ---------------------------------------------------------------

JECOL_raw <- JECOL_new.csv
source("functions_data_import/clean_JECOL.R")
JECOL <- clean_JECOL(JECOL_raw)
rm(JECOL_new.csv, JECOL_raw)


# Annual Rev Ecology Evol Syst --------------------------------------------

AREES_raw <- AREES.csv
source("functions_data_import/clean_AREES.R")
AREES <- clean_AREES(AREES_raw)
rm(AREES.csv, AREES_raw)

# J Tropical Ecology ------------------------------------------------------

JTE_raw <- JTE_2015.csv
source("functions_data_import/clean_JTE.R")
JTE <- clean_JTE(JTE_raw)
rm(JTE_2015.csv, JTE_raw)


# Marine Ecol Prog Series -------------------------------------------------

# TODO: EVERYTHING. NOT EVEN AN EXCEL SHEET,NEED TO SEE WHICH
# ONES ARE MISSING instITUTIONS


# Ecology -----------------------------------------------------------------

ECOL_raw <- ECOLOGY.csv
source("functions_data_import/clean_ECOL.R")
ECOL <- clean_ECOL(ECOL_raw)
rm(ECOLOGY.csv, ECOL_raw)


# Bind journals into a single dataframe & standardize ----------------------

alldata <- bind_rows(
  CONBIO,
  MARECOL,
  NAJFM,
  NEWPHYT,
  JZOOL,
  GCB,
  OIKOS,
  BITR,
  LECO,
  PLANTECOL,
  OECOL,
  JTE,
  JECOL,
  JBIOG,
  JAPE,
  JANE,
  FUNECOL,
  FEM,
  EVOL,
  ECOL,
  ECOG,
  AREES,
  AMNAT,
  AJB,
  AG,
  CONDOR,
  AUK,
  BIOCON
)

colnames(alldata)<-tolower(colnames(alldata))
alldata$journal<-as.factor(alldata$journal)

rm(
  CONBIO,
  MARECOL,
  NAJFM,
  NEWPHYT,
  JZOOL,
  GCB,
  OIKOS,
  BITR,
  LECO,
  PLANTECOL,
  OECOL,
  JTE,
  JECOL,
  JBIOG,
  JAPE,
  JANE,
  FUNECOL,
  FEM,
  EVOL,
  ECOL,
  ECOG,
  AREES,
  AMNAT,
  AJB,
  AG,
  CONDOR,
  AUK,
  BIOCON
)


alldata <- alldata %>%
  select(
    journal, year, volume, issue, editor_id, # deleting "name"
    first_name, middle_name,
    last_name, title, category, category.x, inst, old_inst, inst.2,
    unit, city, state, country, country_prior_class, geo.code,
    geo.code_prior_class, notes, gender, title
  )

# clean-up ----------------------------------------------------------------
# check to see what is diff between category and category.x

alldata$check <- alldata$category == alldata$category.x
summary(alldata$check)
# those with FALSE are the discrepancy. Turns out it is three where
# SPECIAL EIDTOR was coded as SE in category.x.
# Special is correct, so delete category.x and check
alldata <- alldata %>% select(-category.x, -check)

# Look for diff btwn inst2 and inst ---------------------------------------

# TURNS OUT inst is correction of inst2, so delete inst2 and check
alldata$check <- alldata$inst.2 == alldata$inst
summary(alldata$check)
# LOOKED OVER, they all check out
alldata <- alldata %>% select(-inst.2, -check)

# OLDinst is only a few. delete and recheck
alldata <- alldata %>% select(-old_inst)

# alldata_2<-alldata to avoid having to redo the whole thing to error check below
# alldata<-alldata_2

alldata[alldata == "missing"] <- NA
alldata$inst[alldata$inst == "double check"] <- NA

# Oecologia Corrections by Patrick James ----------------------------------

source("functions_data_cleaning/PJ_OECOL_corrections.R")
data_list <- PJ_OECOL_corrections(alldata)
alldata <- as_tibble(data_list[[1]])
OECOLOGIA <- as_tibble(data_list[[2]])
alldata <- bind_rows(alldata, OECOLOGIA)
rm(data_list, OECOLOGIA)

# Landscape Eco Corrections by Patrick James ------------------------------
# TODO: LECO NEEDS CORRECTION!! James indicates mistakes, but not what they are.
source("functions_data_cleaning/PJ_LECO_corrections.R")
data_list <- PJ_LECO_corrections(alldata)
alldata <- as_tibble(data_list[[1]])
LECO <- as_tibble(data_list[[2]])
alldata <- bind_rows(alldata, LECO)
rm(data_list, LECO)


# Oikos Corrections by Patrick James ------------------------------
source("functions_data_cleaning/PJ_OIKOS_corrections.R")
data_list <- PJ_OIKOS_corrections(alldata)
alldata <- as_tibble(data_list[[1]])
OIKOS <- as_tibble(data_list[[2]])
alldata <- bind_rows(alldata, OIKOS)
rm(data_list, OIKOS)
# J Animal Ecol Corrections by Patrick James ------------------------------
source("functions_data_cleaning/PJ_JANE_corrections.R")
data_list <- PJ_JANE_corrections(alldata)
alldata <- as_tibble(data_list[[1]])
JANE <- as_tibble(data_list[[2]])
alldata <- bind_rows(alldata, JANE)
rm(data_list, JANE)
# J Biogeography Corrections by Patrick James ------------------------------
source("functions_data_cleaning/pj_jbiog_corrections.R")
data_list <- pj_jbiog_corrections(alldata)
alldata <- as_tibble(data_list[[1]])
JBIOG <- as_tibble(data_list[[2]])
alldata <- bind_rows(alldata, JBIOG)
rm(data_list, JBIOG)
# Plant Ecology Corrections by Patrick James ------------------------------
source("functions_data_cleaning/PJ_PLANTECOL_corrections.R")
data_list <- PJ_PLANTECOL_corrections(alldata)
alldata <- as_tibble(data_list[[1]])
PLANTECOL <- as_tibble(data_list[[2]])
alldata <- bind_rows(alldata, PLANTECOL)
rm(data_list, PLANTECOL)


alldata_ORIG_FOR_TESTING <- alldata
# alldata<-alldata_ORIG_FOR_TESTING

# this adds a column to flag all those needing an inst check
alldata$inst_CHECK <- NA

colnames(alldata)


# Clean Editors  ----------------------------------------------------------
source("functions_data_cleaning/editor_cleaner.R")
alldata <- editor_cleaner(alldata)

# Clean Countries ---------------------------------------------------------
source("functions_data_cleaning/country_cleaner.R")
alldata <- country_cleaner(alldata)

# Clean Institutions ------------------------------------------------------
source("functions_data_cleaning/institution_cleaner.R")
alldata <- institution_cleaner(alldata)





# Insert Corrections by Patrick James -------------------------------------

str(alldata)

# alldata$VOLUME.check<-alldata$VOLUME.x==alldata$VOLUME.y
# summary(alldata$VOLUME.x==alldata$VOLUME.y)
# source("functions_data_cleaning/institution_cleaner.R")
# alldata<-institution_cleaner(alldata)
source("functions_data_cleaning/JamesCorrections.R")
alldata <- JamesCorrections(alldata)

source("functions_data_cleaning/country_cleaner.R")
alldata <- country_cleaner(alldata)

source("functions_data_cleaning/institution_cleaner.R")
alldata <- institution_cleaner(alldata)

source("functions_data_cleaning/editor_ID_corrections.R")
alldata <- editor_ID_corrections(alldata)

alldata <- as_tibble(alldata)
alldata$city <- tolower(alldata$city)
alldata$first_name <- tolower(alldata$first_name)
alldata$last_name <- tolower(alldata$last_name)
alldata$middle_name <- tolower(alldata$middle_name)



# Add mising Editor IDs ---------------------------------------------------

alldata$editor_id <- as.numeric(as.character(alldata$editor_id))
max(na.omit(alldata$editor_id)) + 1

missing_edID <- alldata %>%
  filter(is.na(editor_id)) %>%
  select(last_name, first_name) %>%
  group_by(last_name, first_name) %>%
  slice(n = 1)

no_edID <- inner_join(alldata, missing_edID) %>%
  filter(is.na(editor_id)) %>%
  select(journal, year, editor_id, first_name, middle_name, last_name, title, inst) %>%
  group_by(first_name, middle_name, last_name) %>%
  slice(n = 1) %>%
  arrange(last_name, first_name, middle_name) %>%
  ungroup() %>%
  select(last_name, first_name)
start_no <- max(na.omit(alldata$editor_id)) + 1
end_no <- max(start_no + nrow(no_edID)) - 1
no_edID$editor_id <- seq(
  from = start_no,
  to = end_no,
  by = 1
)

alldata <- full_join(alldata, no_edID, by = c("last_name", "first_name")) %>%
  mutate(editor_id.x = ifelse(is.na(editor_id.x), editor_id.y, editor_id.x)) %>%
  rename(editor_id = editor_id.x) %>%
  select(-editor_id.y)


##########################
# # TODO: ADD AN EDITOR_ID to GCB and MARECOL
# summary(alldata$editor_id)
# missing_edID<-filter(alldata,is.na(editor_id))
# missing_edID$journal<-as.factor(missing_edID$journal)
# alldata$journal<-as.factor(alldata$journal)
# summary(missing_edID)
# alldata<-droplevels(alldata)
# alldata$Inst_Prior_Class
# alldata$inst<-as.factor(alldata$inst)
# levels(alldata$inst)
# levels(alldata$journal)
# alldata$editor_id<-as.factor(alldata$editor_id)
# levels(alldata$editor_id)
# summary(alldata)



# Standardize Editor Categories ------------------------------------------
# alldata_original<-alldata
# alldata<-alldata_original
source("functions_data_cleaning/standardize_editors.R")
alldata <- standardize_editors(alldata)


# get missing editor categories from dryad data ---------------------------

alldata$category<-as.factor(alldata$category)
levels(alldata$category)
summary(alldata$category)
alldata$title<-as.factor(alldata$title)
levels(alldata$title)

# DF of all the ones WITHOUT editor category (category == NA)
alldata_na_cat<-alldata %>% 
  filter(is.na(category)) %>% 
  mutate(across(everything(), as.character)) %>% 
  mutate(across(everything(), tolower)) %>% 
  mutate(across(everything(), trimws))

# DF of all the ones WITH editor category
alldata_with_cat<-alldata %>% 
  filter(!is.na(category)) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), tolower)) %>% 
  mutate(across(everything(), trimws))

# trim down the dryad data to the columns you need to match up with the ones
# in alldata missing editor cat
dryad_data_slim<-dryad_data %>% 
  select(journal, year,first_name, last_name, category, editor_id) %>%
  mutate(across(everything(), tolower)) %>% 
  mutate(across(everything(), as.character)) %>% 
  mutate(across(everything(), trimws))

# left_join the trimmed dryad data and all_data_na 
# then replace the value in the .x column (missing) with that from .y (dryad),
# rename the .x column, then delete the .y column
alldata_na_cat<-left_join(alldata_na_cat,dryad_data_slim, 
                          by=c("editor_id",
                               "first_name", 
                               "last_name",
                               "journal",
                               "year")) %>% 
  mutate(category.x = ifelse(is.na(category.x),category.y, category.x)) %>% 
  select(-category.y) %>% 
  rename("category"="category.x")

# Bind the two subsets up and assign them to name alldata
alldata<-bind_rows(alldata_with_cat,alldata_na_cat)
levels(as.factor(alldata$category))

# Looks ok, so remove the ones you used to do this correction
rm(alldata_with_cat,
   alldata_na_cat,
   dryad_data_slim,
   dryad_data)

# final cleanup of missing, blanks, etc
which(alldata$country == "")
alldata$inst[alldata$inst == "missing"] <- NA
# REMOVE ANY ROWS WITH NO DATA
alldata <- as_tibble(alldata)
alldata <- alldata %>% drop_na(last_name, first_name)


# save alldata df ---------------------------------------------------------


write_csv(alldata, "./data_clean/alldata.csv")



# reduce to editors only --------------------------------------------------


# See what's left
levels(as.factor(alldata$category))
# what are the "service" editor categories?
# TODO: 2x if auk and conder's abstract tramnslators also handled MSS. until then, exclude
alldata %>% filter(category=="service")

# Oversight = publications committee, advisory board, etc. 
# These should be excluded.
alldata %>% filter(category=="oversight")

# Exclude service and oversight and name the new df "editors"
nrow(alldata)
 editors <- alldata %>% 
   filter(category == "se" | 
            category == "eic" | 
            category == "ae" | 
            category == "special")
 
 levels(as.factor(alldata$category))
 
####################################
# TODO: NEED TO CONVERT THE DIFFERENT CATEGORIES TO SHORTCUT titleS
levels(as.factor(editors$title))
levels(as.factor(editors$category))

# Summarize No. or Years for each journal ---------------------------------

str(editors)
JrnlYrs_10 <- editors %>%
  filter(year >= 1985, year <= 1994) %>%
  group_by(journal) %>%
  summarise(yrs_per_jrnl = n_distinct(journal, year)) %>%
  arrange(yrs_per_jrnl)
JrnlYrs_20 <- editors %>%
  filter(year >= 1985, year <= 2004) %>%
  group_by(journal) %>%
  summarise(yrs_per_jrnl = n_distinct(journal, year)) %>%
  arrange(yrs_per_jrnl)
JrnlYrs_30 <- editors %>%
  filter(year >= 1985, year <= 2014) %>%
  group_by(journal) %>%
  summarise(yrs_per_jrnl = n_distinct(journal, year)) %>%
  arrange(yrs_per_jrnl)
JrnlYrs_35 <- editors %>%
  filter(year >= 1985, year <= 2019) %>%
  group_by(journal) %>%
  summarise(yrs_per_jrnl = n_distinct(journal, year)) %>%
  arrange(yrs_per_jrnl)
head(JrnlYrs_30)

# Continued Cleanup -------------------------------------------------------
editors$country <- as.factor(editors$country)
editors <- droplevels(editors)
levels(editors$country)

##############################
# TODO: instituion_cleaner
# NEED TO CONFIRM WHAT PART OF USSR IN WHICH THE AUTHOR WAS BASED
# editors$country[editors$country=="USSR"]<-"Russia"

# NEED DO DELETE THESE
which(editors$country == "")

# institutions - cleanup --------------------------------------------------
# editors$inst[editors$inst=="."]<-NA
editors$journal <- as.factor(editors$journal)
summary(editors$journal)


# Correcting or systematizing the name/speclling of an institution
##############################################################




# FOR SOME REASON SOME DIDN"T CHANGE< SO NEED TO DO MANUALLY

##############################################################


###############################
# DELETE DUPLICATE ROWS
# TODO: SEE WHICH ONES ARE BIENBG DROPPED

foo <- select(editors, journal, year, first_name, last_name)
foo<-foo[duplicated(foo[, 1:4]), ]
editors <- distinct(editors, journal, last_name, first_name, year, title, .keep_all = TRUE)
###############################

############################
# TODO: missing an editor_ID

editors$editor_id <- as.character(editors$editor_id)
editors <- editors %>% replace_na(list(editor_id = "TBD"))
no_edID <- editors %>%
  filter(editor_id == "TBD" | editor_id == "missing" | is.na(editor_id)) %>%
  select(last_name, first_name, middle_name) %>%
  group_by(last_name, first_name, middle_name) %>%
  slice(n = 1)
no_edID

no_edID <- inner_join(editors, no_edID) %>%
  select(journal, year, editor_id, first_name, middle_name, last_name, title, inst) %>%
  group_by(first_name, middle_name, last_name, editor_id) %>%
  slice(n = 1) %>%
  arrange(last_name, first_name, middle_name, editor_id)

write_csv(no_edID, file = "./output_review/no_editor_ID.csv") # export it as a csv file
############################

write_csv(editors, "./data_clean/InstitutionData_clean.csv")
# 
# library(tidyverse)
# editors <- read_csv("./data_clean/InstitutionData_clean.csv")
#
editors_ORIG_FOR_TESTING <- editors

editors <- editors %>% 
  mutate(across(everything(), as.factor))
summary(editors)

inst_missing<-editors %>% filter(is.na(inst)) %>% group_by(journal) %>% summarize(n=n())
sum(inst_missing$n)
# editors<-editors_ORIG_FOR_TESTING
# editors<-editors %>% filter(journal!="CONDOR"&journal!="AUK"&journal!="AGRONOMY"&
#                               journal!="NAJFM"&journal!="MARECOL"&journal!="GCB")
editors <- editors %>% filter(journal != "agronomy")
editors <- editors %>% filter(journal != "najfm")
editors <- editors %>% filter(journal != "auk")
editors <- editors %>% filter(journal != "marecol")
editors <- editors %>% filter(journal != "condor")

###########################
# TODO: FINAL REVIEW OF DATA
############################

# # TODO: Check for people at institutions with > 1 campus
# colnames(editors)
filter(editors,inst == "university of california")
MultiCampus <- editors %>%
  filter(inst == "university of arkansas" |
    inst == "university of alaska" |
    inst == "university of california" |
    inst == "university of hawaii" |
    inst == "university of massachusetts" |
    inst == "university of minnesota" |
    inst == "university of nevada" |
    inst == "university of north carolina" |
    inst == "university of south carolina" |
    inst == "university of texas" |
    inst == "university of washington" |
    inst == "usgs united states geological survey" |
    inst == "smithsonian institution" |
    inst == "csiro commonwealth scientific and industrial research organisation" |
    inst == "conicet consejo nacional de investigaciones cientificas y tecnicas" |
    inst == "james cook university" |
    inst == "north carolina" |
    inst == "usfs us forest service" |
    inst == "usda us department of agriculture" |
    inst == "university of north carolina") %>%
  distinct(editor_id, inst, city, .keep_all = TRUE) %>%
  select(journal, year, , editor_id, first_name, middle_name, last_name, inst, unit, city, country) %>% 
  arrange(editor_id,last_name)

MultiCampus %>% group_by(journal) %>% summarize(n())

write_csv(MultiCampus, "./output_review/MultiCampus.csv")


# TODO: Review the insitutions for any duplicates, spelling errors, etc

# LIST OF ALL instITIONS
editors_inst_check <- editors %>%
  select(inst, country) %>%
  distinct(inst, country) %>%
  arrange(inst)
write_csv(editors_inst_check, "./output_review/editors_inst_check.csv")

# LIST OF ALL instITIONS BY country (easier to see if any were assigned wrong country code)
editors_inst_check_by_country <- editors %>%
  select(inst, country) %>%
  distinct(inst, country) %>%
  arrange(country, inst)
write_csv(editors_inst_check_by_country, "./output_review/editors_inst_check_by_country.csv")



checkinst <- editors %>%
  select(inst) %>%
  distinct(inst) %>%
  arrange(inst)


# similarity index to to find mispellings etc.
source("./functions_data_cleaning/name.check.R")
checkinst$inst <- as.character(checkinst$inst)
checkinst$inst[checkinst$inst == ""] <- NA
str(checkinst$inst)
NameSimilarityDF <- name.check(checkinst$inst)
write_csv(NameSimilarityDF, file = "./output_review/NameSimilarityDF_check.csv") # export it as a csv file
############################


# LIST OF ALL instITIONS with city (to tell apart U of C, U Texas, etc)
editors_inst_city_check_by_country <- editors %>%
  select(inst, city, country) %>%
  distinct(inst, city, country) %>%
  arrange(country, inst, city) %>%
  group_by(inst) %>%
  mutate(n = n()) %>%
  arrange(desc(n)) %>%
  filter(n > 1)
write_csv(editors_inst_city_check_by_country, "./output_review/editors_inst_city_check_by_country.csv")

checkinst <- editors %>%
  select(inst) %>%
  distinct(inst) %>%
  arrange(inst)
# similarity index to to find mispellings etc.
source("./functions_data_cleaning/name.check.R")
checkinst$inst <- as.character(checkinst$inst)
checkinst$inst[checkinst$inst == ""] <- NA
str(checkinst$inst)
NameSimilarityDF <- name.check(checkinst$inst)
write_csv(NameSimilarityDF, file = "./output_review/NameSimilarityDF_check.csv") # export it as a csv file
############################






############################
# TODO: FIND and correct any editors with both an editor_id and NA for editor_id
# editors$last_name<-stri_trans_totitle(editors$last_name)
# editors$first_name<-stri_trans_totitle(editors$first_name)

# editors$editor_id.y<-NULL
# editors<-editors %>% rename("editor_id"="editor_id.x")
editors$editor_id <- as.character(editors$editor_id)
editors <- editors %>% replace_na(list(editor_id = "TBD", editor_id.y = "TBD"))
dup_edID <- editors %>%
  select(last_name, first_name, editor_id) %>%
  group_by(last_name, first_name) %>%
  mutate(n_id = n_distinct(editor_id)) %>%
  filter(n_id > 1) %>%
  distinct(last_name, first_name, editor_id, .keep_all = TRUE) %>%
  arrange(last_name, first_name)
dup_edID
write_csv(dup_edID, file = "./output_review/dup_edID.csv") # export it as a csv file
############################

############################
# TODO: Same as above based on last name (in case the first name is different)
dup_edID2 <- editors %>%
  select(last_name, first_name, editor_id) %>%
  group_by(last_name) %>%
  mutate(n_id = n_distinct(editor_id)) %>%
  filter(n_id > 1) %>%
  distinct(last_name, editor_id, .keep_all = TRUE) %>%
  arrange(last_name)
dup_edID2
write_csv(dup_edID2, file = "./output_review/dup_edID2.csv") # export it as a csv file
############################

############################
# TODO: find cases where the same editor has >1 editor_id
dup_edID3 <- editors %>%
  select(last_name, first_name, editor_id) %>%
  filter(editor_id != "TBD") %>%
  group_by(last_name, first_name) %>%
  mutate(n_names = n_distinct(editor_id)) %>%
  distinct(last_name, first_name, editor_id, .keep_all = TRUE) %>%
  arrange(desc(last_name, first_name, editor_id)) %>%
  filter(n_names > 1)
dup_edID3
# dup_edID3 <-dup_edID3 %>% group_by(first_name,last_name) %>%  arrange(editor_id) %>% slice(2)
write_csv(dup_edID3, file = "./output_review/dup_edID3.csv") # export it as a csv file
############################

############################
# TODO: find cases where the diff editors has same editor_id
dup_edIDx <- editors %>%
  select(journal, last_name, editor_id) %>%
  filter(editor_id != "TBD") %>%
  group_by(editor_id) %>%
  mutate(n_eds = n_distinct(last_name)) %>%
  distinct(last_name, editor_id, .keep_all = TRUE) %>%
  arrange(desc(editor_id, last_name, first_name)) %>%
  filter(n_eds > 1)
dup_edIDx
# dup_edID3 <-dup_edID3 %>% group_by(first_name,last_name) %>%  arrange(editor_id) %>% slice(2)
write_csv(dup_edIDx, file = "./output_review/dup_edIDx.csv") # export it as a csv file
############################

############################
# TODO: find cases where the all potential cases of editor with >1 editor_id, including missing
dup_edID4 <- editors %>%
  select(last_name, first_name, editor_id, journal) %>%
  # filter(editor_id!="TBD") %>%
  group_by(last_name)
dup_edID4$first_init <- str_sub(dup_edID4$first_name, start = 1, end = 1)
dup_edID4 <- dup_edID4 %>%
  group_by(last_name, first_init) %>%
  distinct(last_name, first_init, editor_id, .keep_all = TRUE) %>%
  mutate(n_names = n_distinct(editor_id)) %>%
  arrange(last_name, first_init, first_name, editor_id) %>%
  filter(n_names > 1)
dup_edID4


# dup_edID3 <-dup_edID3 %>% group_by(first_name,last_name) %>%  arrange(editor_id) %>% slice(2)
write_csv(dup_edID4, file = "./output_review/dup_edID4.csv") # export it as a csv file
############################


############################
# summary of editors per journal with no inst
# could be na or missing
missing_inst <- editors %>%
  filter(is.na(inst) | inst == "missing") %>%
  group_by(journal) %>%
  distinct(last_name, first_name) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

write_csv(missing_inst, file = "./output_review/missing_inst.csv") # export it as a csv file

############################
# peoplw with NA inst
missing_inst_names <- editors %>%
  select(journal, year, editor_id, first_name, middle_name, last_name, inst, notes) %>%
  filter(is.na(inst) | inst == "missing") %>%
  group_by(journal, last_name, first_name) %>%
  filter(row_number() == 1 | row_number() == n()) %>%
  arrange(journal, last_name, first_name, year)
write_csv(missing_inst_names, file = "./output_review/missing_inst_editor_names.csv") # export it as a csv file
############################

############################
# Cases of editors with >1 inst (including NA, TBD, errors of spelling)
multiple_inst <- editors %>%
  select(last_name, first_name, journal, inst, year) %>%
  distinct(last_name, first_name, inst, .keep_all = TRUE) %>%
  # filter(editor_id!="TBD") %>%
  group_by(last_name, first_name) %>%
  # dup_inst$first_init<-str_sub(dup_edID4$first_name, start = 1, end = 1)
  # dup_inst<-dup_inst %>%
  # group_by(last_name,first_init) %>%
  mutate(n_inst = n_distinct(inst)) %>%
  filter(n_inst > 1) %>%
  select(last_name, first_name, inst, year, journal) %>%
  arrange(last_name, first_name, year)

multiple_inst
write_csv(multiple_inst, file = "./output_review/eds_multiple_inst.csv") # export it as a csv file

############################
# SUMMARY OF HOW MANY MISSING inst BY journal

checkinst2 <- editors %>%
  filter(is.na(inst) | inst == "missing") %>%
  distinct(journal, last_name, country) %>%
  group_by(journal) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

checkinst2
sum(checkinst2$n)
############################


############################
# SUMMARY OF HOW MANY DIFFERENT RECORDS MISSING inst

checkinst3 <- editors %>%
  filter(is.na(inst) | inst == "missing") %>%
  group_by(journal) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

checkinst3
sum(checkinst3$n)
############################

















editors %>%
  filter(country=="china") %>% 
  filter(inst!="academica sinica") %>% 
  group_by(inst) %>% 
  summarize(n=n_distinct(editor_id)) %>% 
  arrange(desc(n))


foo<- editors %>%
  group_by(inst) %>% 
  summarize(n=n_distinct(editor_id)) %>% 
  arrange(desc(n))










############################
# Summary of how many editors
foo <- editors %>%
  filter(journal != "CONDOR") %>%
  filter(journal != "AUK") %>%
  filter(journal != "AGRONOMY") %>%
  filter(journal != "NAJFM") %>%
  filter(journal != "MARECOL") %>%
  filter(journal != "GCB") %>%
  distinct(last_name, first_name, inst) %>%
  group_by(inst) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
foo$counter <- (seq(1:nrow(foo)) - 1)
foo$cumsum <- (cumsum(foo$n) - 307)
foo$prop <- (foo$cumsum / (sum(foo$n) - 307)) * 100
############################


editors$journal<-droplevels(editors$journal)
levels(editors$journal)

























##############################################################
# THIS WILL ALLOW YOU DO TO COMPARE ALL nameS
# TO ALL nameS TO SEE IF THERE ARE ANY THAT ARE
# SIMILAR AND SHOULD BE POOLED
##############################################################

#
# UNI_LIST<-editors$inst
# summary(UNI_LIST)
# levels(UNI_LIST)
# UNI_LIST<-as.data.frame(UNI_LIST)
# summary(UNI_LIST)
# str(UNI_LIST)
# UNI_LIST<-distinct(UNI_LIST)
# head(UNI_LIST)



# ONE LAST CHECK OF THE nameS
x <- editors$last_name
y <- as.data.frame(x)
# str(y)
y <- distinct(y)
# head(y)
#
y[] <- lapply(y, as.character)
NamesList <- sapply(y$x, agrep, y$x, value = TRUE)
NamesDF <- data.frame(
  Name1 = rep(names(NamesList), lapply(NamesList, length)),
  Name2 = unlist(NamesList)
)
# Create a column to which you will add a logical condition telling you if the names are an EXACT match
NamesDF$match <- NA
NamesDF$match <- NamesDF$Name1 == NamesDF$Name2
match2 <- ifelse(NamesDF$match == "TRUE", 1, 0) # convert TRUE/FALSEto 0/1
NamesDF <- cbind(NamesDF, match2)
NamesDF <- arrange(NamesDF, Name1, Name2) # organize in alphabetica order
NamesDF <- filter(NamesDF, match == FALSE) # THIS DELETES ALL nameS THAT ARE 100% MATCH
# # Convert to chr
NamesDF$Name1 <- as.character(NamesDF$Name1)
NamesDF$Name2 <- as.character(NamesDF$Name2)
# # Calculate the proportional similarity and # changes required to go from one name to another. Package RecordLinkage
NamesDF$Name_sim <- levenshteinSim(NamesDF$Name1, NamesDF$Name2)
NamesDF$Name_dist <- levenshteinDist(NamesDF$Name1, NamesDF$Name2)
# # Because this does all pairwise comparisons, it results in redundancy: "e bruna vs emilio bruna" and "emilio bruna vs e bruna"
# # are in different rows, even though they are the same "comparison". This deletes one of the two
NamesDF <- NamesDF[!duplicated(t(apply(NamesDF, 1, sort))), ]
# # this arranges them in order from most similar (1 change required) to least similar.
# # look carefully at those with a few changes, as they are likely to be a tiny spelling mistake or difference in intials
NamesDF$index <- seq.int(nrow(NamesDF)) # adds a column with an index to make it easier to id which row you need'
NamesDF <- NamesDF %>% select(index, Name1, Name2, Name_sim, Name_dist) # It's kinda ugly, but this rearranges columns (and dumps the "FALSE")
NamesDF <- arrange(NamesDF, desc(Name_sim))
head(NamesDF)
# invoking the function
source("./functions/namecompare.R")
x <- editors$last_name
y <- namecompare(x)

write_csv(UNI_LIST, file = "uniNameList_post_review.csv", row.names = T) # export it as a csv file

UNI_LIST <- as.factor(UNI_LIST)
UNI_LIST[] <- lapply(UNI_LIST, as.character)
str(UNI_LIST)
NamesList <- sapply(UNI_LIST$UNI_LIST, agrep, UNI_LIST$UNI_LIST, value = TRUE)

NamesDF <- data.frame(
  Name1 = rep(names(NamesList), lapply(NamesList, length)),
  Name2 = unlist(NamesList)
)

summary(NamesDF)
str(NamesDF)

# Create a column to which you will add a logical condition telling you if the names are an EXACT match
NamesDF$match <- NA
NamesDF$match <- NamesDF$Name1 == NamesDF$Name2
match2 <- ifelse(NamesDF$match == "TRUE", 1, 0) # convert TRUE/FALSEto 0/1
NamesDF <- cbind(NamesDF, match2)
head(NamesDF, 40)
str(NamesDF)
NamesDF <- arrange(NamesDF, Name1, Name2) # organize in alphabetica order
NamesDF <- filter(NamesDF, match == FALSE) # THIS DELETES ALL nameS THAT ARE 100% MATCH
head(NamesDF)
# # Convert to chr
NamesDF$Name1 <- as.character(NamesDF$Name1)
NamesDF$Name2 <- as.character(NamesDF$Name2)
str(NamesDF)
# # Calculate the proportional similarity and # changes required to go from one name to another. Package RecordLinkage
NamesDF$Name_sim <- levenshteinSim(NamesDF$Name1, NamesDF$Name2)
NamesDF$Name_dist <- levenshteinDist(NamesDF$Name1, NamesDF$Name2)
#
# # Because this does all pairwise comparisons, it results in redundancy: "e bruna vs emilio bruna" and "emilio bruna vs e bruna"
# # are in different rows, even though they are the same "comparison". This deletes one of the two
NamesDF <- NamesDF[!duplicated(t(apply(NamesDF, 1, sort))), ]
# # this arranges them in order from most similar (1 change required) to least similar.
# # look carefully at those with a few changes, as they are likely to be a tiny spelling mistake or difference in intials
NamesDF$index <- seq.int(nrow(NamesDF)) # adds a column with an index to make it easier to id which row you need'
NamesDF <- NamesDF %>% select(index, Name1, Name2, Name_sim, Name_dist) # It's kinda ugly, but this rearranges columns (and dumps the "FALSE")
NamesDF <- arrange(NamesDF, desc(Name_sim))
head(NamesDF)
write_csv(NamesDF, file = "./output_review/uniNameCheck_each_vs_each.csv") # export it as a csv file
#
#

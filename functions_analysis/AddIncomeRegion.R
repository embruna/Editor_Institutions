AddIncomeRegion <- function(DATASET) {
  
  # IMPORT World Bank Data on country Income and Region
  # Espin et al used the version of the data downloaded 2/Dec/2015 from https://data.worldbank.org/data-catalog/world-development-indicators
  WDI_data<-read.csv("./data_raw/WDI_data.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  row.names(WDI_data) <- WDI_data$iso3c     #Assigning row names in table for later search
  
  #These lines add the income level and region level based on the editor country
  DATASET$INCOME_LEVEL <- WDI_data[as.character(DATASET$geo.code), 'income']  #Making a new column of income level by country
  DATASET$REGION <- WDI_data[as.character(DATASET$geo.code), 'region']  #Making a new column of income level by country
  
  #step 4: Changing the order of CATEGORY, INCOME_LEVEL, REGION and JOURNAL factors.
  #This is then used to have always the same order of the lines in future plots and tables
  INCOMES.ORDERED.LIST <- c(  'High income: OECD', 'High income: nonOECD',
                              'Upper middle income','Lower middle income','Low income')
  
  #list of geographical regions, useful for analysis and to give them an order in plots
  REGIONS.ORDERED.LIST <- c('North America', 'Europe & Central Asia','East Asia & Pacific',
                            'Latin America & Caribbean', 'Sub-Saharan Africa',
                            'South Asia','Middle East & North Africa')
  
  DATASET$INCOME_LEVEL <-  factor(x =  DATASET$INCOME_LEVEL, levels = INCOMES.ORDERED.LIST)
  DATASET$REGION <-  factor(x =  DATASET$REGION, levels = REGIONS.ORDERED.LIST)
  
  # Northern Ireland is incorrectly coded as IRL instead of GBR
  DATASET$geo.code[DATASET$country == "NORTH IRELAND"]  <- "GBR"    
  DATASET$geo.code[DATASET$country == "NORTHERN IRELAND"]  <- "GBR" 
  DATASET$geo.code[DATASET$country == "NORTH IRELAND"]  <- "GBR"
  DATASET$geo.code[DATASET$country == "N. Ireland"]  <- "GBR" 
  DATASET$geo.code[DATASET$country == "Northern Ireland"]  <- "GBR" 
  
  # rm(WDI_data,REGIONS.ORDERED.LIST,INCOMES.ORDERED.LIST)
  
  
  return(DATASET)
  
}

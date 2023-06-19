#################### CODE SUMMARY ##########################################
# Description: Function to create an inflation adjustment multiplier using
# Bureau of Labor Labor Stats Producer Price Index. Reads in Monthly data
# for years selected, converts monthly to annual average, then creates
# annual multipliers by dividing annual data by the index year (selected
# by user). Adapted from script by Geret DePiper (NEFSC)
#
# Input: BLS Producer Price index monthly data for processed and frozen foods,
# from: "http://download.bls.gov/pub/time.series/wp/wp.data.3.ProcessedFoods"
#
# Output: Data table with annual average PPI data and multiplier (adj) based
# on user selected index year
#
# Programmer: Howard Townsend (NOAA/NMFS/OST/ Marine Ecosystems Division) 
# Date: November 17, 2021
#
# Modified: 
###

############### Set up #############################################
# setwd("C:/Users/Howard.Townsend/Documents/My manuscripts/Portfolio/Analysis")
# paste("Today is", date())
# 
# library(tidyverse)
# library(data.table)
#

# INDEX_YR <-2020
# FIRST_YEAR <-1988
# LAST_YEAR <-2020

############# Create Inflation Adjuster ######################################
Create_infl_adj2 <- function(FIRST_YEAR, LAST_YEAR,INDEX_YR){
  temp <- data.table(read.delim("wp.data.3.ProcessedFoods.txt", fill=FALSE, stringsAsFactors=FALSE))
  temp$series_id <- str_replace_all(temp$series_id,fixed(" "),"")
  
  #Creating price deflator
  DEFLATOR <- temp[temp$series_id == "WPU0223",]
  DEFLATOR <- DEFLATOR[which(DEFLATOR$year>=FIRST_YEAR & DEFLATOR$year<=LAST_YEAR),]
  DEFLATOR$period <- substr(DEFLATOR$period,2,3 )
  DEFLATOR$period <- as.numeric(DEFLATOR$period)
  setnames(DEFLATOR,
           c('year', 'period', 'value'),
           c('YEAR', 'MONTH', 'PPI'))
  DEFLATOR <- subset(DEFLATOR, select=c("YEAR","MONTH","PPI"))
  MDEFLATOR <- data.frame(DEFLATOR)
  test.yr <- (FIRST_YEAR-LAST_YEAR+1)*13
  print(test.yr)
  NROW(MDEFLATOR)
 
  #*** fix this month 13 is the is Annual PPI 
  # Ann_PPI <- data.frame(MDEFLATOR %>% group_by(YEAR) %>% summarize(PPI_yr = mean(PPI)))
  # Ann_PPI <- data.frame(MDEFLATOR %>% group_by(YEAR) %>% filter(MONTH == 13)) #filter cause problems in functions
  Ann_PPI <- MDEFLATOR[MDEFLATOR$MONTH==13,]
  
  
  IND = Ann_PPI %>% filter(YEAR==INDEX_YR) %>%  dplyr::select(PPI) # MASS package masks dplyr select
  INDEX=IND[1,1]
  
  Ann_PPI = transform(Ann_PPI, adj=INDEX/PPI)
  return(Ann_PPI)
}

#### Testing ####
# # Test inputs
# FIRST_YEAR = 1988
# LAST_YEAR = 2020
# INDEX_YR=2020
# 
# Adjust_2020 <- Create_infl_adj(FIRST_YEAR, LAST_YEAR,INDEX_YR)



#################### CODE SUMMARY ##########################################
# Description: Function to process NMFS Fisheries Landings and price data, 
# subsets by region, summarizes data, filters data for analysis criteria, 
# outputs REVENUE and LANDINGS data for portfolio analysis
#
# Input: Annual commercial landings data (1950-2017) in csv file,
# All Species by NMFS Region - Totals By YEAR/REGION/SPECIES
# Data from NMFS Fisheries One Stop Shop (FOSS) Landing database:
# https://foss.nmfs.noaa.gov/apexfoss/f?p=215:200::::::
# csv file should be read in as an R data file
#
# Output: List with two data frames - LANDINGS and REVENUE-
# summarized by year for high-value species - i.e., top X landings

#
# Programmer: Howard Townsend (NOAA/NMFS/OST/ Marine Ecosystems Division) 
# Date: September 1, 2021
#
# Modified: 
############################################################################

############### Set up #############################################
# setwd("C:/Users/Howard.Townsend/Documents/My manuscripts/Portfolio/Analysis")
# paste("Today is", date())
# 
# library(tidyverse)

# Region="Alaska"
# Region="Gulf"
# Region="Hawaii"
# 
# a_check <- Process_FOSS_topR (Recent,Region,topR)
# 
# plot(a_check$ANNUAL_L,a_check$ANNUAL_R)
# cor(a_check$ANNUAL_L,a_check$ANNUAL_R)
############# Process FOSS ######################################
Process_FOSS2_topR <- function(Recent,Region,topR){
  ###### Subset data by Region ####
  RegRecentAll <- Recent %>% dplyr::group_by(Year) %>% 
    filter(Region.Name == Region)
  # Remove cases with incomplete catch or revenue data
  RegRecent <- RegRecentAll %>% filter(!is.na(Pounds) & !is.na(Dollars))
  # Recent_keep <- RegRecentAll %>% filter(!is.na(Pounds),!is.na(Dollars))
  # Species_keep <- unique(Recent_keep$NMFS.Name)
  # unique(RegRecentAll$NMFS.Name)
  # RegRecent <- RegRecentAll %>% filter(NMFS.Name %in% Species_keep)
  #Create Initial Data Summaries to make sure everything's cool
  # PriceSumm <- RegRecent  %>% group_by(NMFS.Name) %>% summarize( N = length(Dollars),
  #                                                                meanPrice = mean(Dollars), sd   = sd(Dollars), min = min(Dollars), max = max(Dollars))
  # LandingsSumm <- RegRecent  %>% group_by(NMFS.Name) %>% summarize( N = length(Pounds),
  #                                                                   meanPrice = mean(Pounds), sd   = sd(Pounds), min = min(Pounds), max = max(Pounds))
  # 
  
  ###### Create analysis dataframes- for Landings and Value (Pounds and Dollars) 

  
  #Reformat Price/Revenue data - Sum up dollar values by year for a species and create spec X yr table
  AnnualRevenue <- RegRecent %>% dplyr::group_by(Year) %>% dplyr::summarize(Dollars=sum(Dollars, na.rm = TRUE))
  SpecAnnualRevenue <- RegRecent %>% dplyr::group_by(Year,NMFS.Name) %>% dplyr::summarize(Dollars=sum(Dollars, na.rm = TRUE))
  SpecRevTable <- SpecAnnualRevenue %>% spread(key=NMFS.Name, value=Dollars)
  
  #Final Analysis dataframe - Remove any species that do not have landings for all years
  SpecRev0 <- SpecRevTable %>%  select_if(~ !any(is.na(.)))
  SpecRev <- SpecRev0 %>%  select_if(~ !any(.== 0))
  
  
  #Report out on species not included in the analysis because of incomplete Rev data
  kept_Spec_R <- names(SpecRev)
  drop_Spec_R <- setdiff(names(SpecRevTable),names(SpecRev))
  print(paste("For the", Region, "Region the following species were dropped"))
  # print(drop_Spec_R)
  print(paste(as.character(length(drop_Spec_R)), "species were dropped because of incomplete data for some years."))
  print(paste(as.character(length(kept_Spec_R)), "species were kept."))
  print(paste0("The dropped species accounts for ",
               100*round(sum(SpecRevTable[, names(SpecRevTable) %in% drop_Spec_R],na.rm=T)/sum(SpecRevTable, na.rm=T),2),
               "% of total revenues."))
  
  
  #Reformat Landings data  - Sum up pound values by year for a species and create spec X yr table
  AnnualCatch <- RegRecent %>% dplyr::group_by(Year) %>% dplyr::summarize(Pounds=sum(Pounds, na.rm = TRUE))
  SpecAnnualCatch <- RegRecent %>% dplyr::group_by(Year,NMFS.Name) %>% dplyr::summarize(Pounds=sum(Pounds, na.rm = TRUE))
  SpecCatchTable <- SpecAnnualCatch%>% spread(key=NMFS.Name, value=Pounds)
  
  #Final Analysis dataframe - Remove any species that do not have landings for all year
  SpecCatch0 <- SpecCatchTable %>%  select_if(~ !any(is.na(.)))
  SpecCatch <- SpecCatch0 %>%  select_if(~ !any(.== 0))
  
  #Report out on species not included in the analysis because of incomplete Rev data
  kept_Spec_C <- names(SpecCatch)
  drop_Spec_C <- setdiff(names(SpecCatchTable),names(SpecRev))
  
  int_Spec <- intersect(kept_Spec_C,kept_Spec_R)
  
  SpecRev <- dplyr::select (SpecRev, c(int_Spec)) # MASS package masks dplyr select
  SpecCatch <- dplyr::select (SpecCatch, c(int_Spec))
  
  # kept_all <- as.numeric(setdiff(kept_Spec_C,kept_Spec_C))
  # if(kept_all !=0){
  #   SpecRev <- select (SpecRev, c(uni_Spec))
  #   SpecCatch <- select (SpecCatch, c(uni_Spec))
  # }
  # if(length(kept_Spec_R)>length(kept_Spec_C)){
  #   SpecRev <- select (SpecRev,-c(drop_Spec_C))
  # }
  # if(length(kept_Spec_C)>length(kept_Spec_R)){
  #   SpecCatch <- select (SpecRev,-c(drop_Spec_R))
  # }
  
  #Check to make sure Catch and Revenue tables are the same dimension and same species listed
  if(!identical(dim(SpecCatch),dim(SpecRev))){
    warning("******WARNING****** Revenue and Catch Matrices are different sizes. Fix before proceeding")
  }
  if(!identical(names(SpecCatch),names(SpecRev))){
    warning("******WARNING****** Revenue and Catch Matrices list different species. Fix before proceeding")
  }
  
  # subset data set for species where mean landings is in top X revenue 
  spec_ct <- ncol(SpecRev)
  MeanRev<- colMeans(SpecRev[sapply(SpecRev, is.numeric)],na.rm=TRUE)
  MeanRev <-as.data.frame(MeanRev)
  MeanRev <- tibble::rownames_to_column(MeanRev,"NMFS.Name")
  MeanRev <- MeanRev[-1,] #drop the year from the vector
   # HiCatch<- MeanCatch[MeanCatch>=quantile(MeanCatch,Top_pctl,na.rm=TRUE)]
  if(topR >spec_ct){topR ==spec_ct}
  HiRev <- MeanRev %>% arrange(desc(MeanRev)) %>% slice(1:topR)
  HiRevSpecies <- HiRev$NMFS.Name
  
  HiRevSpecRev <- SpecRev[, c("Year",HiRevSpecies)]
  HiRevSpecCatch <- SpecCatch[, c("Year",HiRevSpecies)]
  
  SpecSumm_R <- describe(HiRevSpecRev[-1])
  SpecSumm_L <- describe(HiRevSpecCatch[-1])
  
  Tot_R <- rowSums(HiRevSpecRev[-1])
  Tot_L <- rowSums(HiRevSpecCatch[-1])
  P_Selection <- RegRecent %>% filter(NMFS.Name %in% c(HiRevSpecies))
  
  summary(P_Selection)
  dim(P_Selection)
  unique(P_Selection$NMFS.Name)
  
  HiRevSpec <- list("REVENUE" = HiRevSpecRev,"LANDINGS" =HiRevSpecCatch, 
                    "REV_SPEC_SUMM"=SpecSumm_R, "LAND_SPEC_SUM" = SpecSumm_L,
                    "ANNUAL_R"= Tot_R, "ANNUAL_L"= Tot_L, 
                    "P_Selection"=P_Selection)
}
############# Read in Data and test function ######################################
# RawData <- as.data.frame(read.csv2(file="foss_landings.csv",
#                                    header=TRUE, sep=",",stringsAsFactors = FALSE))
# RawData$Pounds <-as.numeric(gsub("," ,"",RawData$Pounds))
# RawData$Dollars <-as.numeric(gsub("," ,"",RawData$Dollars))
# str(RawData)
# 
# n_rawObs <- nrow(RawData)
# paste("The landings file has", n_rawObs, "observations")
# 
# 
# #Use data from 1985 (or later) to present, earlier data is less complete, so # stocks analyzed will
# # decrease if an earlier date is set
# Recent <- RawData[ which(RawData$Year > 1984), ]
# n_recentObs <- nrow(Recent)
# paste(n_rawObs-n_recentObs," observations were removed")
# min(Recent$Year)
# 
# 
# Region="Pacific Coast"
# a <- Process_FOSS2_topR (Recent,Region,topR =25)
# 
# plot(a$ANNUAL_L,a$ANNUAL_R)
# cor(a$ANNUAL_L,a$ANNUAL_R)

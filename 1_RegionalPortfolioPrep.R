##### CODE SUMMARY #####
# Description: Reads in landings data, processes its, and applies
# inflation multiplier to create regional data sets (portfolios) for
# mean-variance optimization.
#
# Input: 1) Data from NMFS Fisheries One Stop Shop (FOSS) Landing database:
# https://foss.nmfs.noaa.gov/apexfoss/f?p=215:200::::
# Saved as csv file ("foss_landings.csv")
# 2) "RegionsTable.csv" used to index regions from FOSS Landings
# Used in Process_FOSS2 and ProcessFOSS2_topl functions
# 3) Data from BLS Producer Price index monthly data for processed 
# and frozen foods,
# "http://download.bls.gov/pub/time.series/wp/wp.data.3.ProcessedFoods"
# saved as file: "wp.data.3.ProcessedFoods.txt"
# Used in Create_infl_adj2 function
#
# Output: List objects for each region ([Region Abbreviation]_DAT). Each list
# contains data frames of revenue, landings, inflation-adjusted revenue, and 
# other data frames and info needed for analysis, plots and tables. Ten objects
# in each list.
#
# Pre-check plots (optional) used to inspect the selected portfolios from each
# region, to allow the analyst to determine if patterns in the data may cause 
# issues with optimization algorithm used in "2_RegionalPortfolioOpt.R". For 
# if all species exhibit similar revenue trends or if there are very few 
# negative correlations among species revenues in a portfolio.
#
# Programmer: Howard Townsend (NOAA/NMFS/OST/ Marine Ecosystems Division) 
# adapted from code developed by 
# Geret DePiper (NOAA/NMFS/NEFSC/READ/Social Sciences Branch)
# and Lauran Brewster (UMASS-Dartmouth/SMAST)
# Date: May 31,2023
#
# Modified: 
##

##### SET UP #####
setwd("C:/Users/Howard.Townsend/Documents/My manuscripts/Portfolio/Analysis")
paste("Today is", date())
# save.image(file='Regional_EFs.RData')
# load(file='Regional_EFs.RData')
###### Package dependencies#####
# R does not come out of the box with everything you need
# Lines below check to see whether the needed packages are installed, and installs and loads if not
# ***** Run the snippet below to ensure all necessary packages are available

PKG <- c('priceR', 'RColorBrewer', 'viridis', 'eeptools', 'openxlsx',
         'grDevices',  'extrafont', 'boot', 'kernlab', 'reshape', 
         'matrixcalc', 'corrplot', 'corrr', 'tidyverse', 'foreign', 
         'ggplot2', 'micEcon', 'matrixStats', 'matrixcalc', 'kernlab', 
         'scales', 'reshape2', 'data.table', 'corrplot', 'psych', 
         'ggforce', 'PerformanceAnalytics')

for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p)
    require(p,character.only = TRUE)}
}

###### Source user-defined functions for analysis#####
source("C:/Users/Howard.Townsend/Documents/My manuscripts/Portfolio/Analysis/Process_FOSS2.R")
source("C:/Users/Howard.Townsend/Documents/My manuscripts/Portfolio/Analysis/Process_FOSS2_topl.R")
source("C:/Users/Howard.Townsend/Documents/My manuscripts/Portfolio/Analysis/Process_FOSS2_topR.R")
source("C:/Users/Howard.Townsend/Documents/My manuscripts/Portfolio/Analysis/Exec_string.R")
source("C:/Users/Howard.Townsend/Documents/My manuscripts/Portfolio/Analysis/Create_infl_adj2.R")
source("C:/Users/Howard.Townsend/Documents/My manuscripts/Portfolio/Analysis/portfolio_risk.R")


##### READ IN AND PREP DATA ####
###### Settings - time frame and other parameters for analysis ----
FIRST_YEAR = 1990
LAST_YEAR = 2020
# Top_pctl=0.50 # percentile for hi-value species (top 75% = 25 percentile), Process_FOSS2
# Topl=25 # Top X for average landings, selection criterion for Process_FOSS2_topl
TopR=25 # Top X for average revenue, selection criterion for Process_FOSS2_topl
INDEX_YR=2020 # year to index $ value for inflation adjustment, Create_infl_adj



###### Read in FOSS Data ----
# Data from NMFS Fisheries One Stop Shop (FOSS) Landing database:
# https://foss.nmfs.noaa.gov/apexfoss/f?p=215:200::::::
# has been downloaded to a local directory

#*** Consider reading in directly from FOSS website
RawData <- as.data.frame(read.csv2(file="foss_landings.csv",
                                   header=TRUE, sep=",",stringsAsFactors = FALSE))
RawData$Pounds <-as.numeric(gsub("," ,"",RawData$Pounds))
RawData$Dollars <-as.numeric(gsub("," ,"",RawData$Dollars))
str(RawData)

n_rawObs <- nrow(RawData)
paste("The landings file has", n_rawObs, "observations")


#Use data from 1985 (or later) to present, earlier data is less complete, so num stocks analyzed will
# decrease if an earlier date is used
# Recent <- RawData[ which(RawData$Year >= FIRST_YEAR), ]
Recent <- RawData  %>%  filter(Year >= FIRST_YEAR & Year <= LAST_YEAR)
n_recentObs <- nrow(Recent)
paste(n_rawObs-n_recentObs," observations were removed")
min(Recent$Year)
max(Recent$Year)
###### Read in Regions Table#####
# Removed Great Lakes because of data issues
Reg <- read.csv2(file="RegionsTable2.csv",
                 header=TRUE, sep=",",stringsAsFactors = FALSE)

###### Create inflation multiplier (adj) #####
# Creates the inflation adjuster using BLS PPI for processed foods
PPI <- Create_infl_adj2(FIRST_YEAR, LAST_YEAR,INDEX_YR)

# Converts the inflation adjuster to a diagonal matrix for multiplying adjuster
# by revenue row (i.e., so each PPI year applies to appropriate revenue year)
adj_mult <- as.matrix (diag(PPI$adj))

###### Process_FOSS to all regions#####
#Select one option below based on analysis parameters used in settings
for (i in 1:nrow(Reg)){
  assign((paste(Reg$RegBrev[i],"_DAT", sep = "")), Process_FOSS2_topR(Recent,Reg$Region[i],TopR))
}
# for (i in 1:nrow(Reg)){
#   assign((paste(Reg$RegBrev[i],"_DAT", sep = "")), Process_FOSS2_topl(Recent,Reg$Region[i],Topl))
# }
# for (i in 1:nrow(Reg)){
#   assign((paste(Reg$RegBrev[i],"_DAT", sep = "")), Process_FOSS2(Recent,Reg$Region[i],Top_pctl))
# }

###### Inflation multiplier applied to to revenue data#####
i=1
for (i in 1:nrow(Reg)){
  RegRevTxt <- paste0(Reg$RegBrev[i],"_DAT$REVENUE", sep = "")
  RegRev <- execute.string(RegRevTxt)
  Year <- getElement(RegRev, "Year")
  numstk <- ncol(RegRev)
  rev <- as.matrix(RegRev[2:numstk])
  REV_ADJ <- adj_mult %*% rev
  REV_ADJ <- as.data.frame(cbind(Year,REV_ADJ))
  tmp.txt<- paste0(Reg$RegBrev[i],"_DAT$REV_ADJ <- REV_ADJ", sep = "")
  # eval(parse(text=tmp.txt)) # does the same thing as execute.string function
  execute.string(tmp.txt)
}

##### MAKING INPUT DATA SET FOR PORTFOLIO OPTIMIZATION FUNCTION ----
###### P_Selection Inflation multiplier and formatting #####
# P_Selection is species and years selected for analysis
PPI_adj <- PPI %>% select(YEAR,adj) %>% rename(Year = YEAR)

for (i in 1:nrow(Reg)){
  P_SelectionTxt <- paste0(Reg$RegBrev[i],"_DAT$P_Selection", sep = "")
  P_Selection <- execute.string(P_SelectionTxt)
  # P_Selection <- AK_DAT$P_Selection
  P_Selection <- P_Selection %>%
    left_join(PPI_adj, by="Year")
  P_Selection$adjDollars <- P_Selection$Dollars*P_Selection$adj
  RegP_Selection <- paste0(Reg$RegBrev[i],"_DAT$P_Selection <- P_Selection", sep="")
  execute.string(RegP_Selection)
}

###### P_Selection  Formatting #####
#### formatting data for mean-var optimization function
for (i in 1:nrow(Reg)){
  P_SelectionTxt <- paste0(Reg$RegBrev[i],"_DAT$P_Selection", sep = "")
  P_Selection <- execute.string(P_SelectionTxt)
  
  P_Selection$Year <-as.numeric(P_Selection$Year)
  P_Selection$NMFS.Namev2 <- sub(" **","",as.character(P_Selection$NMFS.Name),fixed=TRUE)
  P_Selection$NMFS.Namev2[1:12]
  P_Selection <- P_Selection %>% arrange(Year)
  P_Selection <- P_Selection %>%
    dplyr::rename("IYear"="Year",   #rename to match Geret's code
                  "Catch"="Pounds",
                  "Taxonkey"="Tsn") %>%
    as.data.frame()
  
  SCALING_ORDER <- floor(log10(max(P_Selection$adjDollars)))
  # SCALING_F <- 10^SCALING_ORDER
  SCALING_F=max(P_Selection$adjDollars)
  
  SCAL <- as.data.frame(cbind(SCALING_ORDER, SCALING_F ))
  
  SCAL.txt<- paste0(Reg$RegBrev[i],"_DAT$SCAL <- SCAL", sep = "")
  # eval(parse(text=tmp.txt))
  execute.string(SCAL.txt)
  
  P_Selection<-P_Selection %>%
    mutate(Value=adjDollars/SCALING_F) %>%
    droplevels()
  
  LB_SUM<-aggregate(cbind(Catch, Value)~IYear, P_Selection, sum) %>%
    dplyr::rename(SumCT=Catch,
                  SumVAL=Value)
  data<-merge(P_Selection, LB_SUM, by=c("IYear")) %>%
    mutate(Price=Value/Catch,
           PortRevShare = Value / SumVAL) 
  # print(Reg$RegBrev[i])
  # paste0("Scaling order is: ", SCALING_ORDER)
  # range(data$value)
  # summary(data)
  tmp.txt<- paste0(Reg$RegBrev[i],"_DAT$DATA <- data", sep = "")
  # eval(parse(text=tmp.txt))
  execute.string(tmp.txt)
}


#### PRE-CHECKS PLOTS ----
# Select a region for plots using data assignment below (i.e,. change prefix in _DAT)
# data <- GoM_DAT$DATA
# summary(data)
# head(data)
###### 1. Timeseries gaps ----
#CHECK FOR GAPS IN TIMESERIES
ggplot(data %>%
         filter(!Value==0), aes(x=NMFS.Namev2, y=IYear)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust=1),
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_y_continuous(minor_breaks = seq(1950, 2021, 5))

#Use this to check that missing years have been filled otherwise correlation matrix won't run
ggplot(data, aes(x=as.factor(Taxonkey), y=IYear)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust=1),
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_y_continuous(minor_breaks = seq(1950, 2021, 5))

###### 2. Correlation matrix ----
# CHECK THERE IS A GOOD MIX OF COVARIANCE/CORRELATION IN YOUR SELECTED PORTFOLIO
summary(data)
dataCOR<- data %>%
  select(NMFS.Namev2, IYear, Value) %>%
  pivot_wider(names_from = NMFS.Namev2, values_from = Value) %>%
  unnest() 
dataCOR=cor(as.matrix(dataCOR[,-1]))
corrplot.mixed(dataCOR, order = 'AOE', addCoef.col = 1,    # Change font size of text labels
               tl.cex = 0.5)

###### 3. Combined Revenue timeseries ----
ggplot(data) +
  geom_point(aes(x=IYear, y=SumVAL)) +
  labs(y =paste0("Revenue (10^", SCALING_ORDER ," Dollars)"))

###### 4. Species Revenue timeseries ----
#CHECK THE TIME SERIES TRENDS BETWEEN SPECIES
#(might need to use representative species if multiple species follow the same trend)
ggplot(data) +
  geom_line(aes(x=IYear, y=Value, colour=NMFS.Namev2), size=1.5) +
  scale_color_viridis(discrete=TRUE) +
  labs(y =paste0("Revenue (10^", SCALING_ORDER ," Dollars)"))+
  theme(legend.position="none")

ggplot(data) +
  geom_line(aes(x=IYear, y=log(Value), colour=NMFS.Namev2), size=2) +
  geom_text(aes(x=IYear, y=log(Value), label = NMFS.Namev2),
            data = data %>% filter(IYear == max(IYear)),
            nudge_x = 0.35,
            size = 2) +
  scale_color_viridis(discrete=TRUE) +
  labs(y =paste0("log(Revenue, 10^", SCALING_ORDER ," Dollars)")) +
  theme(legend.position="none")


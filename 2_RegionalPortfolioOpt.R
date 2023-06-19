##### CODE SUMMARY #####
# Description: Mean-variance optimization for regional data sets (portfolios)
# created in "1_RegionalPortfolioPrep". Creates data frames for efficient 
# frontiers (Dyanmic - Jin et al and Static - Sanchirico et al), calculates
# realized mean-variance for a given year and calculates risk gaps 
# from dynamic efficient frontiers.
#
# Input: 1) List objects for each region ([Region Abbreviation]_DAT) created
# in "1_RegionalPortfolioPrep".
#
# Output: List objects for each region ([Region Abbreviation]_DAT). Each list
# contains data frames of revenue, landings, inflation-adjusted revenue, and 
# other data frames and info needed for analysis, plots and tables. Thirteen
# objects in each list.
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
source("C:/Users/Howard.Townsend/Documents/My manuscripts/Portfolio/Analysis/Exec_string.R")
source("C:/Users/Howard.Townsend/Documents/My manuscripts/Portfolio/Analysis/Create_infl_adj2.R")
source("C:/Users/Howard.Townsend/Documents/My manuscripts/Portfolio/Analysis/portfolio_risk.R")

# REGIONAL PORTFOLIOS ##########  
# Select a LAMBDA - decay rate for Value-at-Risk methodology,
# LAMBDA=1 # all years equally weighted
# LAMDA=0.549 #5% of the weight remains after 5 years)
LAMBDA=0.741 # 5% of the weight remains after 10 years)

#Select Region index, region indices - uncomment and run function below
# view(Reg)
i=1 
#NOTE: With Topl=25 FOR HI (3), MA (4), NE(5),  margins in portfolio_risk were set to 10^-3 in optimizer,
# for all other regions margins were set to 10^-4

#NOTE: With TopR=25 FOR HI (3), and PC(6),  margins in portfolio_risk were set to 10^-3 in optimizer,
# for all other regions margins were set to 10^-4

dataTxt <- paste0(Reg$RegBrev[i],"_DAT$DATA", sep = "")
data <- as.data.frame(execute.string(dataTxt))
ScalingTxt <- paste0(Reg$RegBrev[i],"_DAT$SCAL", sep = "")
Scaling <- as.data.frame(execute.string(ScalingTxt))

###### Dynamic efficient frontiers   #####
#Loop through the years so the frontier is going up until time t.
#Gives actual mean-var for the same portfolio year as the frontier
YEAR=2020
YEARS<-unique(data$IYear)
# YEARS = 2010:202
FrontierResults <-NULL
ImpResults <-NULL
Portfolio <-NULL
Taxon_Opt_Weight <- NULL
Half_Max_Rev <- NULL

for (k in YEARS) {
  
  yr = min(YEARS):k
  N = as.numeric(length(yr))
  j = N
  LAST_YEAR = max(yr)
  
  c<-portfolio_risk(data=data,YEAR=LAST_YEAR,YEARS=yr,TAXA=Taxonkey,N=N,j=j, P_LAMBDA=LAMBDA, EBFM = TRUE)
  d<-portfolio_risk(data=data,YEAR=LAST_YEAR,YEARS=yr,TAXA=Taxonkey,N=N,j=j, P_LAMBDA=LAMBDA, EBFM = FALSE)
  
  
  EBFM_EF_Risk <- c$MEAN_VAR
  Type <- rep("EBFM",nrow(c$MEAN_VAR))
  EBFM_EF_Risk <- cbind(Type, EBFM_EF_Risk)
  
  SS_EF_Risk <- d$MEAN_VAR
  Type <- rep("SS",nrow(d$MEAN_VAR))
  SS_EF_Risk <- cbind(Type, SS_EF_Risk)
  
  All_EF_Risk <- rbind(EBFM_EF_Risk, SS_EF_Risk)
  # All_EF <- rbind(All_EF, SS_EF)
  All_EF_Risk$Iteration<-LAST_YEAR #!!
  Iteration<-LAST_YEAR
  
  FrontierResults <- rbind(All_EF_Risk, FrontierResults)
  
  TImpWeights <- c$T_IMPLICIT_WEIGHTS
  Iteration<-LAST_YEAR
  ImpWeights <- cbind(Iteration, TImpWeights)
  ImpWeights<-as.data.frame(ImpWeights) #implicit weights for the terminal year
  
  denom<-t(ImpWeights$TImpWeight)%*%ImpWeights$WVal #(denominator in Eqn. 6)
  
  ImpResults <- rbind(ImpWeights, ImpResults)
  ImpResults <-as.data.frame(ImpResults) 
  
  Front_Portfolio<- c$FRONTIER_PORTFOLIO
  Act_Portfolio_T <-c$ACTUAL_PORTFOLIO_T
  AF<-cbind(Iteration, Act_Portfolio_T, Front_Portfolio)
  AF<-cbind(AF,denom = denom)
  
  Portfolio<-rbind(AF, Portfolio)
  
  # Create a dataframe which pulls the optimal weights for each taxon key, for each target revenue run, for each year  
  alex <- c$TAXON_OPTIMAL_WEIGHTS
  alex$Iteration<-LAST_YEAR
  Taxon_Opt_Weight <- rbind(Taxon_Opt_Weight, alex)
  # Taxon_Opt_Weight<-as.data.frame(Taxon_Opt_Weight) 
  
  #Create a dataframe which shows half of the maximum revenue up until time t
  HalfMaxRev <- c$HALF_MAX_REV
  HalfMaxRev <- cbind(HalfMaxRev, Iteration)
  Half_Max_Rev <- as.data.frame(rbind(Half_Max_Rev, HalfMaxRev)) 
  
  print(Iteration)
  
}

###### Static efficient frontiers #####
# Creates frontier for terminal year, creates actual man-var for all portfolio years
StaticPlot<-NULL
ITERATION<-unique(ImpResults$Iteration)

COVAR_MAT<-as.matrix(c$COVARIANCE)

for (j in ITERATION) {
  
  Link<-ImpResults %>%
    filter(Iteration==j)
  
  boo<-matrix(t(Link$TImpWeight)%*%COVAR_MAT%*%Link$TImpWeight)
  
  TARGET<-Portfolio %>%
    filter(Iteration==j) %>%
    select(TARGET)
  
  boo <- cbind(boo, j, TARGET)
  colnames(boo) <- c('ActualP_T', "Iteration", "TARGET")
  
  StaticPlot<-rbind(StaticPlot, boo)
  StaticPlot_filt <- StaticPlot %>% filter(Iteration >2005)
}

Portfolio<-Portfolio %>%
  select(-c(3:4)) %>% #drop duplicated columns including TARGET
  mutate(RiskgapNum=sqrt(ActualP_T)-sqrt(FrontierP),
         RiskgapDollar=RiskgapNum/denom) %>%
  filter(!duplicated(.))

###### Append data frames to ([Region Abbreviation]_DAT) #####
tmp.txt<- paste0(Reg$RegBrev[i],"_DAT$Portfolio <- Portfolio", sep = "")
# eval(parse(text=tmp.txt))
execute.string(tmp.txt)

tmp.txt<- paste0(Reg$RegBrev[i],"_DAT$FrontierResults <- FrontierResults", sep = "")
# eval(parse(text=tmp.txt))
execute.string(tmp.txt)

tmp.txt<- paste0(Reg$RegBrev[i],"_DAT$StaticPlot_filt <- StaticPlot_filt", sep = "")
# eval(parse(text=tmp.txt))
execute.string(tmp.txt)

# CHECK PLOTS ########## 
rescl <- as.numeric(Scaling[2]/10^Scaling[1])
p <- ggplot() +
  geom_line(data =FrontierResults %>%
              filter(Iteration == 2020), aes(x= OptimizedStDev*rescl, y=OptimizedRevenue*rescl, color=Type),lwd=1) +
  geom_point(data=StaticPlot_filt %>% filter(!Iteration==min(Iteration)), aes(x= sqrt(ActualP_T)*rescl, y= TARGET*rescl), color="red") + 
  geom_text(data=StaticPlot_filt %>% filter(!Iteration==min(Iteration)), aes(x= sqrt(ActualP_T)*rescl, y= TARGET*rescl, label=Iteration), hjust=1.2) +
  # geom_text(data=Portfolio, aes(x= sqrt(ActualP_T), y= round(TARGET, digits = 4), label=round(sqrt(ActualP_T), digits = 4)), hjust=0, vjust=1) +
  labs(x=paste0("Risk (10^", Scaling[1] ," Dollars)"), y=paste0("Revenue (10^", Scaling[1] ," Dollars)"))
p + ggtitle(paste(Reg$Region[i])) + theme(legend.position="bottom")




# save.image(file='Regional_EFs_Top25L.RData')
# save.image(file='Regional_EFs_Top25R.RData')

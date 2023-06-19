##### CODE SUMMARY #####
# Description:  Use outputs created in "1_RegionalPortfolioOpt.R" to create
# selected graphs and tables
#
# Input: List objects for each region ([Region Abbreviation]_DAT) created
# in "1_RegionalPortfolioOpt.R"
# 
#
# Output: Selected plots grouped by all regions
#
# Programmer: Howard Townsend (NOAA/NMFS/OST/ Marine Ecosystems Division) 
# and Lauran Brewer (UMASS-Dartmouth-SMAST) 
# Date: May 31, 2023
#
# Modified: 
##

##### SET UP #####
setwd("C:/Users/Howard.Townsend/Documents/My manuscripts/Portfolio/Analysis")
paste("Today is", date())
# load(file='Regional_EFs_Top25L.RData')
# load(file='Regional_EFs_Top25R.RData')

###### Package dependencies#####

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
source("C:/Users/Howard.Townsend/Documents/My manuscripts/Portfolio/Analysis/Exec_string.R")
source("C:/Users/Howard.Townsend/Documents/My manuscripts/Portfolio/Analysis/fancy_scientific.R")

#### PLOTS ----
# Correlation Matrix of Revenue Plot----
i=1
pdf("CorrMatPlot_All.pdf", width=8.5, height=11) #print to pdf
for(i in 1:nrow(Reg)){
REV_ADJTxt <- paste0(Reg$RegBrev[i],"_DAT$REV_ADJ", sep = "")
REV_ADJ <- as.data.frame(execute.string(REV_ADJTxt))
dataCOR=cor(as.matrix(REV_ADJ[,-1]))
# corrplot.mixed(dataCOR, order = 'AOE', addCoef.col =2,    # Change font size of text labels
#                tl.cex = 0.1, title =paste0(Reg$RegBrev[i]), mar = c(0,0,0.5,0))
corrplot(dataCOR, method='shade',  
               tl.cex = .6, title =paste0(Reg$RegBrev[i]), mar = c(0,0,1,0))

Sys.sleep(0) # allows multiple pages to be printed to pdf
}
dev.off() #turns off pdf printing


# AdjustedRevenue over time by species Stacked Bar Plots ----
i=1
pdf("AdjRev_All.pdf", width=11, height=8.5) #print to pdf
for(i in 1:nrow(Reg)){
  REV_ADJTxt <- paste0(Reg$RegBrev[i],"_DAT$REV_ADJ", sep = "")
  REV_ADJ <- as.data.frame(execute.string(REV_ADJTxt))
  
REV_ADJ_Long <- REV_ADJ %>%
  pivot_longer(!Year, names_to = "Species", values_to = "AdjRev")

REV_ADJ_Long$Species<-gsub('[^[:alnum:] ]','',as.character(REV_ADJ_Long$Species))

width_scale = 5
RevPlot <- ggplot(REV_ADJ_Long, aes(fill=Species, y=AdjRev, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  labs(y= "Revenue (in 2020 $)") +
  theme(legend.text=ggplot2::element_text(size=8),
        legend.box.margin = margin(0,0, 0.5, 0),
        legend.position="bottom",
        legend.key.size = grid::unit(width_scale/50, "inch"),
        legend.key.width = grid::unit(width_scale/50, "inch"),
        legend.box.just = "right")
print(RevPlot+ ggtitle(Reg$RegBrev[i]))
Sys.sleep(0) # allows multiple pages to be printed to pdf
}
dev.off() #turns off pdf printing

# Landings over time by species Stacked Bar Plots ----
i=1
pdf("Landings_All.pdf", width=11, height=8.5) #print to pdf
for(i in 1:nrow(Reg)){
  LANDINGSTxt <- paste0(Reg$RegBrev[i],"_DAT$LANDINGS", sep = "")
  print(LANDINGSTxt)
  LANDINGS <- as.data.frame(execute.string(LANDINGSTxt))
  
LAND_Long <- LANDINGS %>%
  pivot_longer(!Year, names_to = "Species", values_to = "Landing")

LAND_Long$Species<-gsub('[^[:alnum:] ]','',as.character(LAND_Long$Species))
LandPlot <- ggplot(LAND_Long, aes(fill=Species, y=Landing, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  labs(y= "Landings (in lbs)") +
  theme(legend.text=ggplot2::element_text(size=8),
        legend.box.margin = margin(0,0, 0.5, 0),
        legend.position="bottom",
        legend.key.size = grid::unit(width_scale/50, "inch"),
        legend.key.width = grid::unit(width_scale/50, "inch"),
        legend.box.just = "right")
print(LandPlot+ ggtitle(Reg$RegBrev[i]))
Sys.sleep(0) # allows multiple pages to be printed to pdf
}
dev.off() #turns off pdf printing

# Static Frontier Regular Plot (a la Sanchirico, Efficient Frontier with past actuals) ----
i=1
pdf("San_EF_All.pdf", width=11, height=8.5) #print to pdf

for(i in 1:nrow(Reg)){
  Frontier <- paste0(Reg$RegBrev[i],"_DAT$FrontierResults", sep = "")
  FrontierResults <- execute.string(Frontier)
  StatTxt <- paste0(Reg$RegBrev[i],"_DAT$StaticPlot_filt", sep = "")
  StaticPlot_filt <- as.data.frame(execute.string(StatTxt))
  ScalingTxt <- paste0(Reg$RegBrev[i],"_DAT$SCAL", sep = "")
  Scaling <- as.data.frame(execute.string(ScalingTxt))
  Scaling$rescl <- as.numeric(Scaling[2]/10^Scaling[1])
  # rescl <- as.numeric(Scaling[2]/10^Scaling[1])
  San_EFF <- ggplot() +
    geom_line(data =FrontierResults %>%
                filter(Iteration == 2020), aes(x= OptimizedStDev*Scaling$rescl, y=OptimizedRevenue*Scaling$rescl, color=Type),lwd=1) +
    geom_point(data=StaticPlot_filt %>% filter(!Iteration==min(Iteration)), aes(x= sqrt(ActualP_T)*Scaling$rescl, y= TARGET*Scaling$rescl), color="red") + 
    geom_text(data=StaticPlot_filt %>% filter(!Iteration==min(Iteration)), aes(x= sqrt(ActualP_T)*Scaling$rescl, y= TARGET*Scaling$rescl, label=Iteration), hjust=1.2) +
    # geom_text(data=Portfolio, aes(x= sqrt(ActualP_T), y= round(TARGET, digits = 4), label=round(sqrt(ActualP_T), digits = 4)), hjust=0, vjust=1) +
    labs(x=paste0("Risk (10^", Scaling$SCALING_ORDER ," Dollars)"), y=paste0("Revenue (10^", Scaling$SCALING_ORDER ," Dollars)"))
  print(San_EFF + ggtitle(paste(Reg$Region[i])) + theme(legend.position="bottom"))
  Sys.sleep(0) # allows multiple pages to be printed to pdf
}

dev.off() #turns off pdf printing

# Static Frontier Panel Plot (a la Sanchirico, Efficient Frontier with past actuals) ----
# Initialize the data frame
i=1
FrontTxt <- paste0(Reg$RegBrev[i],"_DAT$FrontierResults", sep = "")
Front<- as.data.frame(execute.string(FrontTxt))
All_Front <- subset(Front,FALSE)
All_Front$RegBrev <- character()
All_Front$ScalF <- double()
All_Front$ScalOrd <- double()
All_Front$rescl <- double()

StatTxt <- paste0(Reg$RegBrev[i],"_DAT$StaticPlot_filt", sep = "")
Stat<- as.data.frame(execute.string(StatTxt))
All_Stat <- subset(Stat,FALSE)
All_Stat$RegBrev <- character()
All_Stat$Rescale <- double()
All_Stat$ScalOrd <- double()
All_Stat$rescl <- double()

# fill in the data frame for all years and regions
for(i in 1:nrow(Reg)){
  FrontTxt <- paste0(Reg$RegBrev[i],"_DAT$FrontierResults", sep = "")
  Reg_Front<- as.data.frame(execute.string(FrontTxt))
  ScalingTxt <- paste0(Reg$RegBrev[i],"_DAT$SCAL", sep = "")
  Scaling <- as.data.frame(execute.string(ScalingTxt))
  Reg_Front$ScalF <- as.numeric(Scaling[2])
  Reg_Front$ScalOrd <- as.numeric(Scaling[1])
  # Reg_Front$rescl <- as.numeric(Scaling[2]/10^Scaling[1])
  Reg_Front$rescl <- as.numeric(Scaling[2]/10^7)
  Reg_Front$RegBrev  <- Reg$RegBrev[i]
  
  Reg_Front <- Reg_Front %>% rename("Variance" = "OptimizedVariance",
           "Revenue" = "OptimizedRevenue")
  Reg_Front<- Reg_Front %>% arrange(Type, Iteration)
  Reg_Front <- Reg_Front %>% 
    mutate(Variance.next=c(Variance[-1],NA),Revenue.next=c(Revenue[-1],NA))
  
  All_Front <- rbind(All_Front,  Reg_Front)
  
  StatTxt <- paste0(Reg$RegBrev[i],"_DAT$StaticPlot_filt", sep = "")
  Reg_Stat<- as.data.frame(execute.string(StatTxt))
  ScalingTxt <- paste0(Reg$RegBrev[i],"_DAT$SCAL", sep = "")
  Scaling <- as.data.frame(execute.string(ScalingTxt))
  Reg_Stat$ScalF <- as.numeric(Scaling[2])
  Reg_Stat$ScalOrd <- as.numeric(Scaling[1])
  # Reg_Stat$rescl <- as.numeric(Scaling[2]/10^Scaling[1])
  Reg_Stat$rescl <- as.numeric(Scaling[2]/10^7)
  Reg_Stat$RegBrev  <- Reg$RegBrev[i]
  
  Reg_Stat <-Reg_Stat %>% distinct()
  Reg_Stat <- Reg_Stat %>% rename("Variance" = "ActualP_T",
                                    "Revenue" = "TARGET")
  Reg_Stat<- Reg_Stat %>% arrange(Iteration)
  Reg_Stat <- Reg_Stat %>% 
    mutate(Variance.next=c(Variance[-1],NA),Revenue.next=c(Revenue[-1],NA))
  
  All_Stat <- rbind(All_Stat,  Reg_Stat)
}
# All_Stat <-All_Stat %>% distinct()
All_Stat$Type <- "Actual"
All_Front <- All_Front %>% select(-c("TARGET", "OptimizedStDev"))
# 
# All_Stat <- All_Stat %>% arrange(Iteration, RegBrev)
# 
# All_Stat$Type <- "Actual"
# All_Stat <- All_Stat %>% rename("Variance" = "ActualP_T",
#                                "Revenue" = "TARGET")
# All_Front <- All_Front %>%
#   rename("Variance" = "OptimizedVariance",
#          "Revenue" = "OptimizedRevenue")
# All_Front <- All_Front %>% select(-c("TARGET", "OptimizedStDev"))
Front <- All_Front %>% filter(Iteration == 2020)

dim(All_Front)
dim(All_Stat)
names(All_Front)
names(All_Stat)

All_Opt_Act <- rbind(Front, All_Stat)
# All_Opt_Act<- All_Opt_Act %>% arrange(Type,RegBrev,Iteration)
# 
# All_Opt_Act <- All_Opt_Act %>% mutate(Variance.next=c(Variance[-1],NA),Revenue.next=c(Revenue[-1],NA))

keep_yrs <- c(2006, 2009, 2012, 2016, 2020)


San_EFF_pan <- ggplot() +
  geom_line(data = All_Opt_Act   %>% filter(Type!="Actual"),
            aes(x= sqrt(Variance)*rescl, y=Revenue*rescl,
                color=Type),lwd=1) +

  geom_point(data=All_Opt_Act  %>% filter(Type=="Actual"),
             aes(x= sqrt(Variance)*rescl, y= Revenue*rescl),
             color="grey50") +

  geom_segment(data=All_Opt_Act  %>% filter(Type=="Actual"),
    color="grey",
    aes(x= sqrt(Variance)*rescl, y= Revenue*rescl,
        xend=sqrt(Variance.next)*rescl,
        yend=Revenue.next*rescl),
    arrow=arrow(length=unit(0.3,"cm"), ends ="last")) +

  geom_text(data=All_Opt_Act  %>% filter(Type=="Actual") %>%
              filter(Iteration %in% keep_yrs),
            aes(x= sqrt(Variance)*rescl, y= Revenue*rescl,
                label=Iteration), size=2, hjust=1.2) +
  
 
  # labs(x=paste0("Risk (10^7 Dollars)"), y=paste0("Revenue (10^7 Dollars)")) +
  labs(x=expression(Risk~(10^7~Dollars)), y=expression(Revenue~(10^7~Dollars))) +
  facet_wrap(~RegBrev, scales="free")

San_EFF_pan

pdf("San_EF_Panel.pdf", width=11, height=8.5) #print to pdf
print(San_EFF_pan)
dev.off() #turns off pdf printing


# Dynamic Frontier plot (a la Jin) ----
i=1
pdf("Jin_EF_All.pdf", width=11, height=8.5) #print to pdf
for(i in 1:nrow(Reg)){
  Frontier <- paste0(Reg$RegBrev[i],"_DAT$FrontierResults", sep = "")
  FrontierResults <- execute.string(Frontier)
  PortfolioTxt <- paste0(Reg$RegBrev[i],"_DAT$Portfolio", sep = "")
  Portfolio <- as.data.frame(execute.string(PortfolioTxt))
  ScalingTxt <- paste0(Reg$RegBrev[i],"_DAT$SCAL", sep = "")
  Scaling <- as.data.frame(execute.string(ScalingTxt))
  
  rescl <- as.numeric(Scaling[2]/10^Scaling[1])
### Removes the first year (min(Iteration))
Jin_EFF <- ggplot() +
  geom_line(data =FrontierResults %>%
              # filter(Type=='EBFM') %>% #Use this line if you want to drop the SS blue line.
              filter(!Iteration==min(Iteration)), 
            aes(x= OptimizedStDev*rescl, y=OptimizedRevenue*rescl, color=Type),lwd=1)+
  geom_point(data =Portfolio %>%
               filter(!Iteration==min(Iteration)), aes(x = sqrt(ActualP_T)*rescl, y = TARGET*rescl))+
  labs(x=paste0("Risk (10^", Scaling$SCALING_ORDER ," Dollars)"), y=paste0("Revenue (10^", Scaling$SCALING_ORDER ," Dollars)")) +
  facet_wrap(~Iteration, scales="fixed")
print(Jin_EFF + ggtitle(Reg$RegBrev[i]))
Sys.sleep(0) # allows multiple pages to be printed to pdf
}
dev.off() #turns off pdf printing

# Barplot of risk gap vs riskgap in dollars ----
# Initialize the data frame
i=1
PortfolioTxt <- paste0(Reg$RegBrev[i],"_DAT$Portfolio", sep = "")
Portfolio <- as.data.frame(execute.string(PortfolioTxt))
All_Portfolio <- subset(Portfolio,FALSE)
All_Portfolio$RegBrev <- character()
All_Portfolio$Rescale <- double()

# fill in the data frame for all years
for(i in 1:nrow(Reg)){
  PortfolioTxt <- paste0(Reg$RegBrev[i],"_DAT$Portfolio", sep = "")
  Reg_Portfolio <- as.data.frame(execute.string(PortfolioTxt))
  ScalingTxt <- paste0(Reg$RegBrev[i],"_DAT$SCAL", sep = "")
  Scaling <- as.data.frame(execute.string(ScalingTxt))
  rescl <- as.numeric(Scaling[2])
  Reg_Portfolio$Rescale <- rescl
  Reg_Portfolio$RegBrev <- Reg$RegBrev[i]
  
  All_Portfolio <- rbind(All_Portfolio,  Reg_Portfolio)
}

coeff <- 5
RG <- ggplot(All_Portfolio, aes(x=Iteration)) +
  geom_bar( aes(y=RiskgapDollar), stat="identity", size=.1, fill="black", color="black", alpha=.4) +
  geom_line( aes(y=RiskgapNum*Rescale/10^8 / coeff), size=1, color="red") +
  geom_point( aes(y=RiskgapNum*Rescale/10^8 / coeff), size=2, color="red") +
  scale_y_continuous(
    # Features of the first axis
    name = "Risk Gap Per Dollar",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name=expression(Risk~Gap~(10^8~Dollars)))) +
  theme(axis.title.y = element_text(color = "black", size=15),
        axis.title.y.right = element_text(color = "red", size=15)) +
  xlab("Year") +
  facet_wrap(~RegBrev, scales="free_y")
  # ggtitle(Reg$RegBrev[i])

pdf("RiskGap_All.pdf", width=11, height=8.5) #print to pdf
print(RG)
dev.off() #turns off pdf printing



# SETTING UP THEME ----
theme_set(theme_bw() + 
            theme(text = element_text(size = 20),
                  axis.title = element_text(size = 20),
                  axis.text.x = element_text(angle = 60, vjust = 0.7, size=12),
                  legend.position="top",
                  legend.title=element_blank(),
                  legend.text=element_text(size=10),
                  panel.background = element_rect(fill='transparent'), #transparent panel bg
                  plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
                  # panel.grid.major = element_blank(), #remove major gridlines
                  # panel.grid.minor = element_blank(), #remove minor gridlines
                  legend.background = element_rect(fill='transparent'), #transparent legend bg
                  legend.box.background = element_rect(fill='transparent', color='NA'), #transparent legend panel
                  strip.background = element_blank(),
                  strip.text = element_text(colour = 'black', size=10)
            ))

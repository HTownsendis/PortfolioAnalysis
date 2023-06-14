# PortfolioAnalysis
Portfolio optimization accounts for species interactions (through correlations in revenue time series) and can be used to inform a broader set of living marine resource management concerns and put species interactions and economic considerations in the forefront of resource management decision-making. 

The approach can use readily available data on landings and revenue from a given management region to generate an easily digestible indicators of risk associated with historical resource use patterns, risk gap. For demonstration purposes, we calculate efficient portfolio frontiers (minimizing risk for desired levels of revenue outcomes) and calculate risk gaps based on historical revenues and standard deviations (risks) of fishing portfolios from seven US fisheries regions. 

The code is broken into 3 sections - 
1) RegionalPortfolioPrep.R - Reads in landings data, processes its, and applies inflation multiplier to create regional data sets (portfolios) for mean-variance optimization.
2) RegionalPortfolioPrep.R - Mean-variance optimization for regional data sets (portfolios) created in "1_RegionalPortfolioPrep". Creates data frames for efficient frontiers (Dyanmic - Jin et al and Static - Sanchirico et al), calculates realized mean-variance for a given year and calculates risk gaps from dynamic efficient frontiers.
3) RegionalPlots_Group.R - Use outputs created in "1_RegionalPortfolioOpt.R" to create # selected graphs and tables

These scripts use various user-sourced functions included in the repository.

These scrips and functions were developed in collaboration with Geret DePiper (NOAA/NMFS/NEFSC/READ/Social Sciences Branch)
and Lauran Brewster (UMASS-Dartmouth/SMAST)

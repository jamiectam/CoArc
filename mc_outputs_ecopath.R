rm(list = ls())
###############
# Script Info #
###############
# PURPOSE: Synthesize Monte-Carlo results from EwE outputs
# AUTHOR: Jamie Tam 
# REVIEWED BY:
# VERSION: 0.1
#

######################
# CHANGES/ ADDITIONS #
######################

# Need to add: 

# Done:

############
# PACKAGES #
############
require(data.table)
require(matrixStats)
#
##################
# Set up folders #
##################

#set working directory to the folder with the results from MC
setwd("~/EwE output/mc_MC_01")

#loop through subfolders to extract csv files
files <- list.files(full.names = T, recursive = T, pattern = "biomass_annual.csv")
subfiles<-files[c(T,F)]

nfiles<-length(subfiles)
#create empty dataframe with rows 1:49 (change if using a different model with differnt number of FGs)
biomass_mc<-data.frame()[1:49,]
for(i in 1:nfiles){
biomass<-read.csv(paste(subfiles[[i]]),skip=9,header=TRUE)
#pulls data from year 1, change by adjusting row number
bio_y<-biomass[2,-1]
rownames(bio_y)<-(paste0("X",i))
biomass_t<-t(bio_y)
biomass_mc<-cbind(biomass_mc, biomass_t)
}
  save(biomass_mc, file="biomass_mc.RData")
  write.csv(biomass_mc, file="biomass_mc.csv")


  
biomass_mc_mat<-as.matrix(biomass_mc)

#make melted table of MCoutputs to make plots
NL_df<-melt(NL_mat, variable.name="group", value.name="biomass")

#to make table
#set quantiles
probs<-c(0.05,0.5,0.95)
biomass_mc_mean<-rowMeans2(biomass_mc_mat)
biomass_mc_quant<-rowQuantiles(biomass_mc_mat, probs=probs)
biomass_mc_stats<-cbind(biomass_mc_mean, biomass_mc_quant)

  save(biomass_mc_stats, file="biomass_mc_stats.RData")
   write.csv(biomass_mc_stats, file="biomass_mc_stats.csv")



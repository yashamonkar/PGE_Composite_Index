#______________________________________________________________________________#
###-----------Estimate the prevented pollution damages-------###

#Formula for each generator
# (No_Tax_Production - Tax_Production)*Pollution Tax


#Point towards the working directory. 
setwd("~/GitHub/PGE_Composite_Index") 


#______________________________________________________________________________#
###Load the libraries###
library(dplyr)
library(corrplot)


#______________________________________________________________________________#
###Model Hyper-parameters###
N_years <- 500

#Tax on Thermal Plants Source Zeighami et al 
thermal_tax <- c(7.911343, 34.68308, 34.68308, 1.383465)


#______________________________________________________________________________#
#Compute the prevented pollution damages
prevented_damages <- list()


for(yr in 1:N_years) {
  print(yr)
  
  
  #----------------------------------------------------------------------#
  ####------Generation -- No Tax --------###
  #Generation Amount
  name <- paste0('data/no_tax/PGE_GEN_', yr-1, '.csv')
  no_tax_gen <- read.csv(name, sep=",", header = FALSE)
  no_tax_gen <- no_tax_gen[, 1:4] #First four rows are thermal plants
  
  
  ####------Generation -- Pollution Tax --------###
  #Generation Amount
  name <- paste0('data/tax/PGE_GEN_', yr-1, '.csv')
  tax_gen <- read.csv(name, sep=",", header = FALSE)
  tax_gen <- tax_gen[, 1:4] #First four rows are thermal plants
  
  #Subract the difference
  reduced_gen <- no_tax_gen - tax_gen
  reduced_pollution <- reduced_gen * thermal_tax
  prevented_damages[[yr]] <- sum(reduced_pollution[,4])/10^6

  
}



prevented_damages <- unlist(prevented_damages)
plot(density(prevented_damages))

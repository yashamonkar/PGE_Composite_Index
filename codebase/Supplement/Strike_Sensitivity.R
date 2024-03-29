#______________________________________________________________________#
###Code for implementing the composite index insurance##



#Point towards the working directory. 
setwd("~/GitHub/PGE_Composite_Index")

#______________________________________________________________________#
#Load data and dependencies

#Library
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggpattern)


#______________________________________________________________________#
#Initial Data Wrangling
net_revenue <- read.csv("sims/Net_Revenue_no_tax.csv", header = TRUE, sep=",")
streamflow <- read.csv("data/Streamflow.csv")
CDD <- read.csv("data/CDD.csv")
Yearly_gas <- read.csv("data/Yearly_gas.csv", header = FALSE)


#Streamflow
sites = c('ORO_fnf', 'SHA_fnf', 'FOL_fnf', 'PAR_fnf', 'NML_fnf', 'MIL_fnf', 'PFT_fnf')
streamflow = streamflow[,sites]
streamflow = rowMeans(streamflow)

###CDD
pge_cities = c('FRESNO_T', 'SACRAMENTO_T','SAN.JOSE_T', 'SAN.FRANCISCO_T')
CDD = CDD[,pge_cities]
CDD = rowMeans(CDD)


#Set-up regression dataset
reg_dataset <- data.frame(streamflow = scale(log(streamflow)), 
                          CDD = scale(CDD),
                          Natural_Gas = scale(Yearly_gas$V1),
                          Net_revenue = net_revenue$Net_revenue)
reg_dataset <- reg_dataset[-which.min(reg_dataset$Net_revenue), ]



#______________________________________________________________________#
#Divide into training and testing. 
set.seed(1)
train_frac <- 0.8

#Seperate into Testing and Training Datasets
train_years <- sample(1:nrow(reg_dataset), 
                      round(500*train_frac), 
                      replace = FALSE)
train_dataset <- reg_dataset[train_years,]
test_dataset <- reg_dataset[-train_years,]



#______________________________________________________________________#
###--------------------Set up regressions----------------------------###

###------Composite Index----------###
reg_composite = lm(Net_revenue ~ streamflow + CDD + Natural_Gas,
                   train_dataset)

newdata = data.frame(streamflow = test_dataset$streamflow,
                     CDD = test_dataset$CDD,
                     Natural_Gas=test_dataset$Natural_Gas)
train_cor <- round(cor(train_dataset$Net_revenue, reg_composite$fitted.values),3)
adj_r <- round(summary(reg_composite)$adj.r.squared,3)



plot(train_dataset$Net_revenue, reg_composite$fitted.values, 
     main = "Composite Index Regression ",
     pch = 19, xlab = "Net Revenues", ylab = "Fitted Revenues",
     xlim = c(min(train_dataset$Net_revenue),max(train_dataset$Net_revenue)),
     ylim = c(min(train_dataset$Net_revenue),max(train_dataset$Net_revenue)))
points(test_dataset$Net_revenue, predict(reg_composite, newdata), 
       col='red', pch = 19)
abline(0,1, lty = 2)
mtext(paste0("Training Correlation (R) is ", train_cor, ". The Adjusted R-sq is ", adj_r), 
      side=3)


paste0("The mean training error is ", mean((train_dataset$Net_revenue-reg_composite$fitted.values)^2))
paste0("The mean testing error is ", mean((test_dataset$Net_revenue-predict(reg_composite, newdata))^2))




#______________________________________________________________________#
###--------------Set-up the insurance contract-----------------------###
###Select the percentile
strike_percentile <- 0.2


#Set-up the Values
revenues <- c(train_dataset$Net_revenue, test_dataset$Net_revenue)
comp_index <- c(reg_composite$fitted.values, predict(reg_composite, newdata))


#Compute the strike
strike <- quantile(comp_index, strike_percentile)

#Compute the payout
payout <- strike - comp_index
payout[payout < 0] = 0


#Compute the premiums 
percent_expected_payout = mean(payout)/max(payout) * 100.0
premium_basis_points = 221.04 * percent_expected_payout + 304.97
braun_premium = premium_basis_points / 10000.0 * max(payout)


#Compute the hedged revenue
hedged_revenues = revenues + payout - braun_premium


#Start with the values
paste0("The premium is ", round(braun_premium*1000,2), " millions")
paste0("The expected payout is ", round(mean(payout)*1000,2), " millions")
paste0("The loading is ", round((-mean(payout)+braun_premium)*1000,2), " millions")
paste0("The mean hedged revenue is ", round(mean(hedged_revenues)*1000,0), " millions")
paste0("The variance hedged revenue is ", round(var(hedged_revenues)*1000*1000,0), " millions-sq")
paste0("The 95%var  hedged revenue is ", round(quantile(hedged_revenues, 0.05)*1000,0), " millions")
paste0("The minimum  hedged revenue is ", round(min(hedged_revenues)*1000,0), " millions")

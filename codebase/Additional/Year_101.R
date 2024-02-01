#Point towards the working directory. 
setwd("~/GitHub/PGE_Composite_Index")



#______________________________________________________________________#
#Load data and dependencies

#Library
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggpattern)
library(dplyr)
library(zoo)


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


#Max Temperautre -- Daily
Temp <- read.csv("data/temp_annual.csv")
pge_cities = c('FRESNO_T', 'SACRAMENTO_T','SAN.JOSE_T', 'SAN.FRANCISCO_T')
Temp = Temp[,pge_cities]
Temp = data.frame(Daily_Temp = rowMeans(Temp),
                  Year = rep(1:500, each = 365))


#Hourly Prices
Price <- read.csv("data/no_tax/Simulated_Price_Variable_NG.csv", header = FALSE)
count_exceed <- list()
for(i in 1:500){
  count_exceed[[i]] <- sum(Price[,i] > 1000)
}
count_exceed <- unlist(count_exceed)
Price <- NULL


###Deamnd Data###
Demand <- read.csv("data/Simulated_Demand.csv", header = FALSE)
Demand <- matrix(Demand$V1, nrow = 8760, ncol = 500)
daily_demand <- sapply(as.data.frame(Demand), function(column) rollapply(column, width = 24, by = 24, FUN = mean, align = "left", partial = TRUE))
daily_demand <- unlist(daily_demand)


#____________________________________________________________________#
#Maximum Temp
max_temp <- Temp %>% group_by(Year) %>% summarize_all(max)




#_________________________________________________________________#
###Demand and Temperature
plot(Temp$Daily_Temp , daily_demand,
     xlab = "Daily Temperature", ylab = "Daily Demand",
     pch=19)
points(Temp$Daily_Temp[36501:36865] , daily_demand[36501:36865], col='red', pch = 19)





par(mfrow=c(1,1))
#Max Demand and Max Temperature 
demand_quants <- apply(Demand, 2, quantile, probs = 1)
plot(max_temp$Daily_Temp, demand_quants,
     xlab = "Max Temperature", ylab = "Max Hourly Demand",
     main = paste0("Max Temperature vs Max Demand"),
     pch=19)
points(max_temp$Daily_Temp[101], demand_quants[101], pch = 19, 
       col='red', cex = 1.5)

max_temp$CDD <- CDD
plot(max_temp$CDD, max_temp$Daily_Temp, xlab = "Summer CDD", ylab = "Max Temperature",
     main = "Maximum Daily Temperature vs Summer CDD", pch = 19)
points(max_temp$CDD[101], max_temp$Daily_Temp[101], pch = 19, 
       col='red', cex = 1.5)


#____________________________________________________________________#
#Moving Average
windows <- c(2,3,5,90)

par(mfrow=c(2,2))

for(i in 1:length(windows)) {
k=windows[i]

#Compute the rolling averages
roll_temp <- Temp %>%
  group_by(Year) %>%
  mutate(rolling_avg = rollapply(Daily_Temp, width = k, FUN = mean, align = "right", fill = NA))

#Compute the maximum rolling mean per year
max_values <- roll_temp %>%
  group_by(Year) %>%
  summarize(max_rolling_avg = max(rolling_avg, na.rm = TRUE))

#Add the CDD
max_values$CDD <- CDD
max_values$Exceed <- count_exceed

Exceedances <- max_values[max_values$Exceed > 0,]


plot(max_values$CDD, max_values$max_rolling_avg, 
     xlab = "Summer CDD", ylab = "Maximum Rolling Temperature",
     main = paste0("Window Size - ",k, " Days"),  
     pch=19)
#points(Exceedances$CDD, Exceedances$max_rolling_avg, pch = 19, 
#       col='blue', cex = 1.5)
points(max_values$CDD[101], max_values$max_rolling_avg[101], pch = 19, 
       col='red', cex = 1.5)

}



#____________________________________________________________________#
###Demand-Temperature correlatio
quants <- c(0.5,0.75,0.9,0.95,0.97, 0.99,0.995, 0.999, 1)

par(mfrow=c(3,3))

for(i in 1:9) {
  k=quants[i]
  
  #Demand Quantile
  demand_quants <- apply(Demand, 2, quantile, probs = k)
  
  plot(CDD, demand_quants,
       xlab = "Summer CDD", ylab = "Demand Quantile",
       main = paste0("Quantile - ",k*100, "th"),  
       pch=19)
  points(CDD[101], demand_quants[101], pch = 19, 
         col='red', cex = 1.5)
  
}

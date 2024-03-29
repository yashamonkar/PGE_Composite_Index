#______________________________________________________________________________#
#This script includes the PGE Finanical Model -- No Tax Scenario -- Variable NG
#Output is the net revenue across the 500 simulated CAPOW years. 
#Output also includes some model diagnostics. 

#Point towards the working directory. 
setwd("~/GitHub/PGE_Composite_Index")



#______________________________________________________________________________#
###Load the libraries###
library(dplyr)
library(corrplot)


#Load Functions
source("functions/get_validation_plots.R")
source("functions/get_revenue_dependence.R")
source("functions/get_corr_plot.R")


#Setup the PDF for the figures
pdf("figures/financial_model_no_tax.pdf")

#______________________________________________________________________________#
###Reading the input data###

#Read the Demand, Market Price and Qualifying Generators (500 Runs)
Demand <- read.csv("data/Simulated_Demand.csv", header = FALSE)
Price <- read.csv("data/no_tax/Simulated_Price_Variable_NG.csv", header = FALSE)
Qualifying_Generators <- read.csv("data/no_tax/qf_generation.csv", header = FALSE)

#Additional CAPOW outputs
streamflow <- read.csv("data/Streamflow.csv")
CDD <- read.csv("data/CDD.csv")
Yearly_gas <- read.csv("data/Yearly_gas.csv", header = FALSE)

#Load Profile 
Load_profile <- read.csv("data/Load_Profile.csv", header = FALSE)



#______________________________________________________________________________#
###Model Hyper-parameters###
N_years <- 500


#------------------------------------------------------------------------------#
###--------Capacity data--------------###

#Capacity associated with solar
Solar_PPA = 3395  
Solar_cap_in_region = 2777
PGE_owned_solar_capacity_MW = 152  


#Capacity associated with wind
Wind_PPA = 1780 
Wind_cap_in_region = 950
#They do not own any wind of their own. (Page 17)


#Capacity associated with hydro
hydro_total = 5844  
owned_hydro = 2655
Hydro_fraction = owned_hydro / hydro_total
percent_hydro_ppa = 0.5 
percent_hydo_owned = 9.7
Hydro_PPA = owned_hydro * (percent_hydro_ppa / percent_hydo_owned)

# Nuclear
pge_nuclear_capacity_MW = 2323.0  
pge_nuclear_capacity_factor = 0.8950  
nuclear_fuel_costs = 7.45 # $/MWh.


#------------------------------------------------------------------------------#
###--------Cost data--------------###

#PPA and Qualifying Generators Costs
PPA_cost_per_MWh = 106.2  
QF_cost_per_MWh = 33.1553 


#Delivery Costs
Delivery_charge = 0.046


#Seasonal Rates
Res_rate_summer = 0.2097163 #cents/kwhr
Res_rate_winter = 0.1578837 #cents/kwhr
Com_rate_summer = 0.1865078 #cents/kwhr
Com_rate_winter = 0.1388922 #cents/kwhr
Ind_rate = 0.1010 #cents/kwhr
Ag_rate_summer = 0.219343 #cents/kwhr
Ag_rate_winter = 0.174257 #cents/kwhr

#Percent of sectoral load met by PG&E
pct_now = c(0.811384629, 0.75790101, 0.850178588, 0.757734342)

#Model Generation Costs
Model_gen_cost = 0  #Yash - This is useless. 



#Season Indicator
Summer = c(5, 6, 7, 8, 9, 10)
Winter = c(1, 2, 3, 4, 11, 12)

st_date <- as.POSIXct("01-01-2023 00:00", format="%m-%d-%Y %H:%M")
end_date <- as.POSIXct("12-31-2023 23:00", format="%m-%d-%Y %H:%M")
months <- seq(st_date, end_date, by ="hours")
months <- as.numeric(format(months, "%m"))


#______________________________________________________________________________#
#Run the financial model.

financial_results <- Sectoral_Revenue <- Total_Deliveries <-  list()
#N_years <- 100

for(yr in 1:N_years) {
  print(yr)
  
  #Hours in the year
  hr <- 8712
  
  #----------------------------------------------------------------------#
  ####------Demand for the current run. --------###
  year_demand = Demand[(1+(yr-1)*8760):(yr*8760),]
  
  #PGE DElivery portion
  PGE_delivery_load <- year_demand*0.82
  
  
  #----------------------------------------------------------------------#
  ####------CAISO Price --------###
  Wholesale_price <- Price[,yr]
  average_wholesale_price = mean(Wholesale_price) #---Output
  max_yearly_wholesale_price = max(Wholesale_price) #---Output
  
  
  #----------------------------------------------------------------------#
  ####------Generation --------###
  #Generation Amount
  name <- paste0('data/no_tax/PGE_GEN_', yr-1, '.csv')
  PGE_gen <- read.csv(name, sep=",", header = FALSE)
  
  
  #Generation Cost
  name <- paste0('data/no_tax/PGE_GEN_cost_', yr-1, '.csv')
  PGE_gen_cost <- read.csv(name, sep=",", header = FALSE)
  
  
  #Thermal Generation
  Thermal_gen = PGE_gen[, 1:4] #First four rows are thermal plants
  Thermal_cost = PGE_gen_cost[,1:4]
  Total_Thermal_cost = Thermal_gen*Thermal_cost
  
  
  #Hydro
  name2 = 'data/CA_hydro_mins.csv'
  Min_hydro = read.csv(name2, sep=",", header = TRUE)
  Min_hydro = Min_hydro$PGE_valley
  PGE_hydro = (PGE_gen[, 6] + Min_hydro[1:hr]) * Hydro_fraction
  
  
  #Solar
  name3 = paste0("data/no_tax/PGE_Solar_",yr-1,".csv")
  PGE_solar = read.csv(name3, sep = ",", header = FALSE)
  PGE_owned_solar = PGE_solar * (PGE_owned_solar_capacity_MW / Solar_cap_in_region) 
  PGE_total_annual_solar = sum(PGE_solar)
  
  #Wind
  name4 = paste0("data/no_tax/PGE_Wind_",yr-1,".csv")
  PGE_wind = read.csv(name4, sep=",", header = FALSE)
  PGE_total_annual_wind = sum(PGE_wind)
  
  #Wind, Solar, Hydro PPA
  PGE_PPA_solar = PGE_solar * (Solar_PPA / Solar_cap_in_region)
  PGE_PPA_wind = PGE_wind * (Wind_PPA / Wind_cap_in_region)
  for(i in 1:nrow(PGE_PPA_solar)){
    if(PGE_PPA_solar[i,] > Solar_PPA) {PGE_PPA_solar[i,] > Solar_PPA}
    if(PGE_PPA_wind[i,] > Wind_PPA) {PGE_PPA_wind[i,] > Wind_PPA}
  }
  PGE_PPA_hydro = ((PGE_gen[, 6] + Min_hydro[1:hr]) * (Hydro_PPA / hydro_total))
  
  #Qualifying facilities
  PGE_QF = Qualifying_Generators[1:hr, yr]
  
  #Nuclear
  hours_in_year = 8760.0  # Change to d
  pge_nuclear_generation = pge_nuclear_capacity_MW * pge_nuclear_capacity_factor * hours_in_year
  PGE_nuc = rep(pge_nuclear_capacity_MW*pge_nuclear_capacity_factor, 8712)
  
  
  #------------------Total Generation Cost----------------------------------#
  #------Total Generation Cost = Thermal + Nuclear Cost------#
  Total_cost_of_generation = sum(Total_Thermal_cost) + pge_nuclear_generation * nuclear_fuel_costs
  hourly_cost_of_generation = rowSums(Total_Thermal_cost) + (pge_nuclear_generation * nuclear_fuel_costs)/hours_in_year
  
  #Total PPA and Qualifying Gens Cost
  Total_PPAs = PGE_PPA_hydro + PGE_PPA_solar[,1] + PGE_PPA_wind[,1]
  hourly_PPA_cost = Total_PPAs * PPA_cost_per_MWh + PGE_QF * QF_cost_per_MWh   
  Total_PPAs_cost = sum(Total_PPAs * PPA_cost_per_MWh) + sum(PGE_QF * QF_cost_per_MWh)
  
  #----------------------------------------------------------------------------------------#
  #--------------------PG&E Financials-----------------------------#
  # all of these (excluding ROE) are in units of billions of $
  # From the 2020 10-K report and their Presentation and Complete Earnings
  Rate_base = 29.5  # Yufei originally had 30
  ROE = 0.1025  # Yufei originally had 0.092
  OM_cost = 6.51594  # Yufei originally had 5
  Depreciation = 2.60217  # Yufei originally had 2
  
  
  #--------------------Sectoral Load----------------------------------------#
  #Load_total = PGE_delivery_load #80% of demand from CAPOW.
  Load_total = year_demand #Yash's potential addition given that 80% of the total demand in the PGE zones are delivered by PG&E but you have another part where the sectoral demand met by PG%E is also added. 
  Ag_load = Load_profile[, 1] / rowSums(Load_profile) * Load_total
  Com_load = Load_profile[, 2] / rowSums(Load_profile) * Load_total
  Ind_load = Load_profile[, 3] / rowSums(Load_profile) * Load_total
  Res_load = Load_profile[, 4] / rowSums(Load_profile) * Load_total
  
  #Load percentages met by PG&E by sector
  pct_list = pct_now
  Res_pct = pct_list[1]
  Ind_pct = pct_list[2]
  Com_pct = pct_list[3]
  Ag_pct = pct_list[4]
  
  #Sectoral Load in MWhr met by PG&E
  PGE_res_load = Res_load * Res_pct
  PGE_com_load = Com_load * Com_pct
  PGE_ind_load = Ind_load * Ind_pct
  PGE_ag_load = Ag_load * Ag_pct
  
  #PG&E Load Base
  Load_base = PGE_ag_load + PGE_com_load + PGE_ind_load + PGE_res_load
  
  
  #--------------------CAISO Exchange---------------------------------------#
  #Total Generation and revenue of selling selling total generation 
  Market_sales = rowSums(Thermal_gen) + PGE_hydro + PGE_nuc + PGE_PPA_hydro + PGE_owned_solar$V1 + PGE_PPA_solar$V1 + PGE_PPA_wind$V1 + PGE_QF
  Total_market_sales = sum(Market_sales) #Total Annual Generated Power
  Market_sell_revenue = Market_sales * Wholesale_price
  Total_market_sales_money = sum(Market_sell_revenue)
  
  
  #Toal Demand -- for 8712 hours and Cost of meeting total demand (across 4 sectors at wholesale price)
  Market_buy = Load_base[1:hr]
  Total_market_buy = sum(Market_buy) #Total Annual Purchased Power
  Market_buy_cost = Market_buy * Wholesale_price
  Total_market_buy_money = sum(Market_buy_cost)
  
  
  #Revenue from individual generation components
  Market_thermal = rowSums(Thermal_gen) * Wholesale_price
  Market_hydro = PGE_hydro * Wholesale_price
  annual_hydro = sum(PGE_hydro)  #REMOVE This is actually not needed.
  Market_nuc = PGE_nuc * Wholesale_price
  Market_PPA_hydro = PGE_PPA_hydro * Wholesale_price
  Market_PPA_solar = PGE_PPA_solar * Wholesale_price
  Market_PPA_wind = PGE_PPA_wind * Wholesale_price
  Market_QF = PGE_QF * Wholesale_price
  
  
  #Fraction use of Natural Gas
  fraction_NG_generation = mean(rowSums(Thermal_gen) / (rowSums(Thermal_gen) + PGE_hydro + PGE_nuc + PGE_owned_solar$V1))
  
  
  #CAISO Exchage (Total Hourly Sectoral Demand - Total Hourly Generation)
  CAISO_Exchange = Market_buy - Market_sales
  CAISO_money_exchange = CAISO_Exchange * - Wholesale_price  ###CONSIDER THE NEGATIVE SIGN
  Total_market_exchange = sum(CAISO_money_exchange)
  
  
  #--------------------Total PG&E Costs-------------------------------------#
  Total_cost = (Total_PPAs_cost + Total_cost_of_generation) - Total_market_exchange #The other parts of the costs which are debt, loan payments these are missing. Not 100% missing they are in the code
  Average_rate = Total_cost / (sum(Load_base) * 1000)
  pct_sale_to_CAISO = sum(CAISO_Exchange) / sum(Load_base)
  
  
  #------------------Revenue by Demand Sectors----------------------------#
  #Residential Rates
  Res_generation_rate_sum = Res_rate_summer 
  Res_generation_rate_win = Res_rate_winter 
  Res_gen_modifier_sum = Res_generation_rate_sum 
  Res_gen_modifier_Win = Res_generation_rate_win 
  
  #Commercial Rates
  Com_generation_rate_sum = Com_rate_summer 
  Com_generation_rate_win = Com_rate_winter 
  Com_gen_modifier_sum = Com_generation_rate_sum
  Com_gen_modifier_Win = Com_generation_rate_win 
  
  #Industrial Rates
  Ind_generation_rate = Ind_rate 
  Ind_gen_modifier_Win = Ind_generation_rate 
  
  #Agricultural Rates
  Ag_generation_rate_sum = Ag_rate_summer 
  Ag_generation_rate_win = Ag_rate_winter
  Ag_gen_modifier_sum = Ag_generation_rate_sum 
  Ag_gen_modifier_Win = Ag_generation_rate_win 
  
  # Store revenue results
  PGE_res_revenue = rep(NA, hr)
  PGE_com_revenue = rep(NA, hr)
  PGE_ind_revenue = rep(NA, hr)
  PGE_ag_revenue = rep(NA, hr)
  
  for(i in 1:hr){
    if(months[[i]] %in% Summer){
      PGE_res_revenue[i] = PGE_res_load[i] * (Model_gen_cost + Res_gen_modifier_sum) * 1000  # load is in MWh
      PGE_com_revenue[i] = PGE_com_load[i] * (Model_gen_cost + Com_gen_modifier_sum) * 1000
      PGE_ind_revenue[i] = PGE_ind_load[i] * (Model_gen_cost + Ind_gen_modifier_Win) * 1000
      PGE_ag_revenue[i] = PGE_ag_load[i] * (Model_gen_cost + Ag_gen_modifier_sum) * 1000
    } else {
      PGE_res_revenue[i] = PGE_res_load[i] * (Model_gen_cost + Res_gen_modifier_Win) * 1000
      PGE_com_revenue[i] = PGE_com_load[i] * (Model_gen_cost + Com_gen_modifier_Win) * 1000
      PGE_ind_revenue[i] = PGE_ind_load[i] * (Model_gen_cost + Ind_gen_modifier_Win) * 1000
      PGE_ag_revenue[i] = PGE_ag_load[i] * (Model_gen_cost + Ag_gen_modifier_Win) * 1000
    }
  }
  
  #Compute total revenue
  PGE_total_serving_revenue = PGE_res_revenue + PGE_com_revenue + PGE_ind_revenue + PGE_ag_revenue
  PGE_delivery_revenue = Load_total * Delivery_charge * 1000
  
  
  #----------------------------Net Revenue---------------------------------#
  #Daily Time Scale
  
  days_in_model_year = hr/24
  hourly_cost = hourly_PPA_cost + hourly_cost_of_generation - CAISO_money_exchange
  daily_net_revenue = rep(NA,days_in_model_year)
  for (i in 1:days_in_model_year) {
    daily_net_revenue[i] = (sum(PGE_total_serving_revenue[((i-1)*24+1):(i*24)]) + sum(PGE_delivery_revenue[((i-1)*24+1):(i*24)])) - OM_cost/365 - (Rate_base * ROE)/365 - sum(hourly_cost[((i-1)*24+1):(i*24)]) }
  
  #Annual Time Scale
  PGE_total_serving_revenue_total = sum(PGE_total_serving_revenue)
  PGE_delivery_revenue_total = sum(PGE_delivery_revenue)
  #Total_revenue = sum(PGE_total_serving_revenue) + sum(PGE_delivery_revenue)
  Total_revenue = sum(PGE_total_serving_revenue) + sum(PGE_delivery_revenue) - sum(hourly_cost)
  Total_revenue_in_Billion = Total_revenue / 1000000000
  Net_revenue = Total_revenue_in_Billion - (Total_cost / 1000000000)
  
  
  #-------------------------------------------------------------------------#
  ###Data Required for validation
  
  ###Sectoral Revenues
  Res_Revenue <- (sum(PGE_res_revenue))/10^6
  Com_Revenue <- (sum(PGE_com_revenue))/10^6
  Ind_Revenue <- (sum(PGE_ind_revenue))/10^6
  Ag_Revenue <- (sum(PGE_ag_revenue))/10^6
  Sectoral_Rev <- c(Res_Revenue, Com_Revenue, Ind_Revenue, Ag_Revenue)
  
  
  ###Total Deliveries
  Total_Deliveries[[yr]] <- sum(PGE_delivery_load)/1000 #MWhr
  
  
  #-------------------------------------------------------------------------#
  financial_results[[yr]] <- c(Net_revenue, average_wholesale_price)
  Sectoral_Revenue[[yr]] <- Sectoral_Rev
  
  
  
}



#_____________________________________________________________________________________#

#Convert to data frame
fin_results <- bind_cols(lapply(financial_results,data.frame))
fin_results <- data.frame(t(fin_results))
colnames(fin_results) = c("Net_Revenue", "Market_Price")




#______________________________________________________________________________#
###Plotting correlations within the PG&E region.

#Streamflow
sites = c('ORO_fnf', 'SHA_fnf', 'FOL_fnf', 'PAR_fnf', 'NML_fnf', 'MIL_fnf', 'PFT_fnf')
streamflow = streamflow[,sites]
streamflow = rowMeans(streamflow)

###CDD
pge_cities = c('FRESNO_T', 'SACRAMENTO_T','SAN.JOSE_T', 'SAN.FRANCISCO_T')
CDD = CDD[,pge_cities]
CDD = rowMeans(CDD)


#Save the net-revenues
plt_dataset <- data.frame(Market_Price = fin_results$Market_Price,
                          Net_revenue = fin_results$Net_Revenue)
write.table(plt_dataset, "sims/Net_Revenue_no_tax.csv", sep=",")


#______________________________________________________________________________#
###Plot1
plt_dataset <- data.frame(streamflow = streamflow, 
                          CDD = CDD,
                          Market_Price = fin_results$Market_Price,
                          NG_Price = Yearly_gas$V1,
                          Net_revenue = fin_results$Net_Revenue)

plt_dataset = scale(plt_dataset)


# Customize Lower Panel
lower.panel<-function(x, y){
  points(x,y, pch=19)
  abline(lm(y~x),col='red', lwd = 2, lty = 2)
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt, cex = 2, col = 'red')
}

#Customize Diagonal Panel
panel.hist = function(x, ...) {
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 0.75))
  hist(x, freq = FALSE, col="cyan", add=TRUE) 
  lines(density(x))
}

pairs(plt_dataset, lower.panel = lower.panel, 
      upper.panel = NULL, diag.panel = panel.hist)


#______________________________________________________________________________#
###Yash's Plot for Sectoral Revenue Validation. 

#Convert to data frame
Sectoral_Rev <- bind_cols(lapply(Sectoral_Revenue,data.frame))
Sectoral_Rev <- data.frame(t(Sectoral_Rev))
colnames(Sectoral_Rev) = c("Residential", "Commercial", "Industrial", "Agriculture")

#Get the Validated Values


par(mfrow=c(2,2))


hist(Sectoral_Rev$Residential, col ='grey', ylab = "All Years", xlab = "$ Millions",
     freq = TRUE, main = "Residential Sector",
     cex.lab = 1.5)

hist(Sectoral_Rev$Commercial, col ='grey', ylab = "All Years", xlab = "$ Millions",
     freq = TRUE, main = "Commercial Sector",
     cex.lab = 1.5)

hist(Sectoral_Rev$Industrial, col ='grey', ylab = "All Years", xlab = "$ Millions",
     freq = TRUE, main = "Industrial Sector",
     cex.lab = 1.5)

hist(Sectoral_Rev$Agriculture, col ='grey', ylab = "All Years", xlab = "$ Millions",
     freq = TRUE, main = "Agricultural Sector",
     cex.lab = 1.5)

par(mfrow=c(1,1))

###Fractional Variation
Sectoral_Fractions <- 100*Sectoral_Rev/rowSums(Sectoral_Rev)

#Validation Plots -- 
res_frac <- c(0.42, 0.42, 0.3986)
com_frac <- c(0.4, 0.4, 0.4)
ind_frac <- c(0.1268, 0.118, 0.112)
ag_frac <- c(0.1, 0.08, 0.09)

par(mfrow=c(2,2))

hist(Sectoral_Fractions$Residential, col ='grey', ylab = "All Years", xlab = "% Revenue", 
     freq = TRUE, main = "Residential Sector", 
     cex.lab = 1.5)
points(100*res_frac, rep(20,3), pch = 19, col ='black', cex = 1.2)

hist(Sectoral_Fractions$Commercial, col ='grey', ylab = "All Years", xlab = "% Revenue",
     freq = TRUE, main = "Commercial Sector",
     cex.lab = 1.5)
points(100*com_frac, rep(20,3), pch = 19, col ='black', cex = 1.2)

hist(Sectoral_Fractions$Industrial, col ='grey', ylab = "All Years", xlab = "% Revenue", 
     freq = TRUE, main = "Industrial Sector",
     cex.lab = 1.5)
points(100*ind_frac, rep(20,3), pch = 19, col ='black', cex = 1.2)

hist(Sectoral_Fractions$Agriculture, col ='grey', ylab = "All Years", xlab = "% Revenue", 
     freq = TRUE, main = "Agricultural Sector",
     cex.lab = 1.5)
points(100*ag_frac, rep(20,3), pch = 19, col ='black', cex = 1.2)

par(mfrow=c(1,1))


##PLOT OF TOTAL REVENUES
pge_deliveries <- c(79774, 82226, 83017)

hist(unlist(Total_Deliveries), col ='grey', ylab = "All Years", xlab = "GWhr", 
     freq = TRUE, main = "Deliveries in GWhr",
     cex.lab = 1.5)
points(pge_deliveries, rep(20,3), pch = 19, col ='black', cex = 1.2)




#______________________________________________________________________________#
###Plot2 


#Form10-K Revenues (pg 1010)
pge_10_k_rev <- c(12.261, 12.083, 12.3)

#Dry Years Revenue 
dry_threshold <- quantile(streamflow, .25)
dry_year_revenue <- fin_results$Net_Revenue[which(streamflow < dry_threshold)]


#Cool Year Revenue
cool_threshold <- quantile(CDD, .25)
cool_year_revenue <- fin_results$Net_Revenue[which(CDD < cool_threshold)]


#High Cost Revenue
high_cost_threshold <- quantile(fin_results$Market_Price, .75)
high_cost_year_revenue <- fin_results$Net_Revenue[which(fin_results$Market_Price > high_cost_threshold)]


# Plot the histograms in r
par(mfrow = c(4,1))

hist(fin_results$Net_Revenue, col ='grey', ylab = "Count", xlab = "",
     freq = TRUE, main = "All Years", 
     xlim = c(min(fin_results$Net_Revenue), max(fin_results$Net_Revenue)),
     cex.lab = 1.5)
points(pge_10_k_rev, rep(20,3), pch = 19, col ='black', cex = 1.2)
abline(v=mean(fin_results$Net_Revenue),lwd = 2, lty = 2)


hist(dry_year_revenue, col ='blue', ylab = "Count", xlab = "",
     freq = TRUE, main = "Dry Years", 
     xlim = c(min(fin_results$Net_Revenue), max(fin_results$Net_Revenue)),
     cex.lab = 1.5)
abline(v=mean(dry_year_revenue),lwd = 2, lty = 2)


hist(cool_year_revenue, col ='yellow', ylab = "Count", xlab = "",
     freq = TRUE, main = "Cool Summers ", 
     xlim = c(min(fin_results$Net_Revenue), max(fin_results$Net_Revenue)),
     cex.lab = 1.5)
abline(v=mean(cool_year_revenue),lwd = 2, lty = 2)


hist(high_cost_year_revenue , col ='green', ylab = "Count", xlab = "Unhedged Net Revenue ($B)",
     freq = TRUE, main = " High Electricity Prices", 
     xlim = c(min(fin_results$Net_Revenue), max(fin_results$Net_Revenue)),
     cex.lab = 1.5)
abline(v=mean(high_cost_year_revenue),lwd = 2, lty = 2)

par(mfrow = c(1,1))



#------------------------------------------------------------------#
#Presentation Plot
# Plot the histograms in r
par(mfrow = c(4,1),  mar = c(3, 3, 3, 2))

hist(fin_results$Net_Revenue, col ='grey', ylab = "Count", xlab = "",
     freq = TRUE, main = "All Years", 
     xlim = c(min(fin_results$Net_Revenue), max(fin_results$Net_Revenue)),
     cex.lab = 1.5, yaxt="n", breaks = 30, cex.axis=1.5, cex.main = 2)
abline(v=mean(fin_results$Net_Revenue),lwd = 2, lty = 2)


hist(dry_year_revenue, col ='blue', ylab = "Count", xlab = "",
     freq = TRUE, main = "Dry Years", 
     xlim = c(min(fin_results$Net_Revenue), max(fin_results$Net_Revenue)),
     cex.lab = 1.5, yaxt="n", breaks = 30, cex.axis=1.5, cex.main = 2)
abline(v=mean(dry_year_revenue),lwd = 2, lty = 2)


hist(cool_year_revenue, col ='yellow', ylab = "Count", xlab = "",
     freq = TRUE, main = "Cool Summers ", 
     xlim = c(min(fin_results$Net_Revenue), max(fin_results$Net_Revenue)),
     cex.lab = 1.5, yaxt="n", breaks = 30, cex.axis=1.5, cex.main = 2)
abline(v=mean(cool_year_revenue),lwd = 2, lty = 2)


hist(high_cost_year_revenue , col ='green', ylab = "Count", xlab = "Unhedged Net Revenue ($B)",
     freq = TRUE, main = " High Electricity Prices", 
     xlim = c(min(fin_results$Net_Revenue), max(fin_results$Net_Revenue)),
     cex.lab = 1.5, yaxt="n", breaks = 30, cex.axis=1.5, cex.main = 2)
abline(v=mean(high_cost_year_revenue),lwd = 2, lty = 2)

par(mfrow = c(1,1),  mar = c(4, 4, 4, 4))


#______________________________________________________________________________#
###Composite Index

reg_dataset <- data.frame(streamflow = streamflow, 
                          CDD = CDD,
                          Natural_Gas = Yearly_gas$V1,
                          Net_revenue = fin_results$Net_Revenue)

#Regression to compute the fitted values
composite_index = lm(Net_revenue~ streamflow + CDD + Natural_Gas,
                     reg_dataset)
plot(reg_dataset$Net_revenue,composite_index$fitted.values, pch = 19)
abline(0,1, col='red', lwd = 2)


#Add the composite values
plt_dataset <- data.frame(Streamflow = streamflow, 
                          CDD = CDD,
                          Natural_Gas = Yearly_gas$V1,
                          Composite_Index = composite_index$fitted.values,
                          Net_Revenue = fin_results$Net_Revenue)

pairs(plt_dataset, lower.panel = lower.panel, 
      upper.panel = NULL, diag.panel = panel.hist,cex.labels = 1.75, 
      main = "Correlation of Indices", cex.main = 2)


#_________________________________________________________________________#
#Plot for presentation.

# Customize Lower Panel
lower.panel<-function(x, y){
  points(x,y, pch=19)
  abline(lm(y~x),col='red', lwd = 3, lty = 2)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
}

#Customize Diagonal Panel
panel.hist = function(x, ...) {
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 0.75))
  hist(x, freq = FALSE, col="cyan", add=TRUE) 
  lines(density(x))
}



pairs(plt_dataset, lower.panel = lower.panel, 
      upper.panel = NULL, 
      cex.labels = 2.5,
      labels = c("Streamflow", "CDD", "Natural Gas", "Composite \nIndex", "Net Revenue"),
      cex.main = 2)


dev.off()

#__________________________________________________________________________________#
#Final Plots for paper

#Validation
pdf("figures/paper/Validation.pdf",height=7, width=14)
get_validation(Deliveries = unlist(Total_Deliveries),
               Net_revenue = fin_results$Net_Revenue)
dev.off()

pdf("figures/paper/Unmanaged.pdf",height=15, width=15)
get_revenue_dependence(Net_revenue = fin_results$Net_Revenue,
                       streamflow = streamflow, 
                       CDD = CDD,
                       Market_Price = fin_results$Market_Price,
                       NG_Price = Yearly_gas$V1)
dev.off()


pdf("figures/paper/Corr_Plot.pdf",height=15, width=15)
get_corr_plot(Net_revenue = reg_dataset$Net_revenue,
              streamflow = streamflow,
              CDD = CDD,
              NG_Price = Yearly_gas$V1,
              Fitted_Values = composite_index$fitted.values)
dev.off()


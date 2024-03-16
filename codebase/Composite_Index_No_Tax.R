#______________________________________________________________________#
###Code for implementing the composite index insurance###
### Main criteria the variance of the revenues match ### 
### Step 1:- Compute the hedged revenues for the 3 indices. 
### Step 2:- Select the strike for the composite index 
###   s.t. the variance of the composite index and portfolio is the same. 


###For each regression, the data are divided into training and testing
###The model fit is assessed. 
### For computing the strike, all the data (predicted values) are used.

#Comment on removing the last point:- The outlier reduces the ability to increase the peak. 


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


pdf("figures/Risk_mitigation_no_tax.pdf", height=3700/600, width=6500/600)

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



#Global Hyper-parameters. This is the 95% VaR 
ned_var <- 0.05 


#______________________________________________________________________#
###--------------------Set up regressions----------------------------###


###------Streamflow----------###
reg_streamflow = lm(Net_revenue ~ streamflow,
                    train_dataset)

newdata = data.frame(streamflow=test_dataset$streamflow)
train_cor <- round(cor(train_dataset$Net_revenue, reg_streamflow$fitted.values),3)
adj_r <- round(summary(reg_streamflow)$adj.r.squared,3)

plot(train_dataset$Net_revenue, reg_streamflow$fitted.values, 
     main = "Streamflow Regression ",
     pch = 19, xlab = "Net Revenues", ylab = "Fitted Revenues",
     xlim = c(min(train_dataset$Net_revenue),max(train_dataset$Net_revenue)),
     ylim = c(min(train_dataset$Net_revenue),max(train_dataset$Net_revenue)))
points(test_dataset$Net_revenue, predict(reg_streamflow, newdata), 
       col='red', pch = 19)
abline(0,1, lty = 2)
mtext(paste0("Training Correlation (R) is ", train_cor, ". The Adjusted R-sq is ", adj_r), 
      side=3)


###------CDD----------###
reg_CDD = lm(Net_revenue ~ CDD,
             train_dataset)

newdata = data.frame(CDD=test_dataset$CDD)
train_cor <- round(cor(train_dataset$Net_revenue, reg_CDD$fitted.values),3)
adj_r <- round(summary(reg_CDD)$adj.r.squared,3)

plot(train_dataset$Net_revenue, reg_CDD$fitted.values, 
     main = "CDD Regression ",
     pch = 19, xlab = "Net Revenues", ylab = "Fitted Revenues",
     xlim = c(min(train_dataset$Net_revenue),max(train_dataset$Net_revenue)),
     ylim = c(min(train_dataset$Net_revenue),max(train_dataset$Net_revenue)))
points(test_dataset$Net_revenue, predict(reg_CDD, newdata), 
       col='red', pch = 19)
abline(0,1, lty = 2)
mtext(paste0("Training Correlation (R) is ", train_cor, ". The Adjusted R-sq is ", adj_r), 
      side=3)


###------NG----------###
reg_price = lm(Net_revenue ~ Natural_Gas,
               train_dataset)
train_cor <- round(cor(train_dataset$Net_revenue, reg_price$fitted.values),3)
adj_r <- round(summary(reg_price)$adj.r.squared,3)
newdata = data.frame(Natural_Gas=test_dataset$Natural_Gas)


plot(train_dataset$Net_revenue, reg_price$fitted.values, 
     main = "Natural Gas Regression ",
     pch = 19, xlab = "Net Revenues", ylab = "Fitted Revenues",
     xlim = c(min(train_dataset$Net_revenue),max(train_dataset$Net_revenue)),
     ylim = c(min(train_dataset$Net_revenue),max(train_dataset$Net_revenue)))
points(test_dataset$Net_revenue, predict(reg_price, newdata), 
       col='red', pch = 19)
abline(0,1, lty = 2)
mtext(paste0("Training Correlation (R) is ", train_cor, ". The Adjusted R-sq is ", adj_r), 
      side=3)



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


plt_dataset <- data.frame(Revenues = c(train_dataset$Net_revenue, test_dataset$Net_revenue),
                          Fitted = c(reg_composite$fitted.values, predict(reg_composite, newdata)),
                          Type = c(rep("Training",400), rep("Testing",99) ))

pdf("figures/supplement/Testing_Training_Error.pdf", 
    height=7, width=9)
ggplot(plt_dataset) +
  geom_point(aes(x=Revenues, y = Fitted, color = Type), size = 2) +
  xlab("Net Revenues ($B)") +  
  xlim(c(11.5,13)) +
  ylim(c(11.5,13)) +
  ylab("Fitted Values ($B)") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +  
  scale_color_manual(values = c("Testing" = "red", "Training" = "black")) +  # Setting manual colors
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14))
dev.off()








#_________________________________________________________________________#
####Functions 

#INPUTS
#1. Upper Bound of Search. upper = 0.3
#2. Lower Bound of Search. lower = 0.01
#3. The fitted Regression. reg = reg_streamflow
#4. Newdata (Testing and Training Data). newdata = data.frame(streamflow=reg_dataset$streamflow)
#5. Value at Risk Threshold (var_thresh) var_thresh = 0.05
#6. Unhedged Revenue. unhedged = reg_dataset$Net_revenue 

###Output
#1. Hedged Revenues
#2. Payout
#2. Strike Value in percentile
#3. Premium Costs

get_strike <- function(upper, lower, reg, newdata, var_thresh, unhedged){
  
  #Compute the predicted revenue
  pred_rev <- predict(reg , newdata)
  
  #Create sequences of strikes -- Percentiles
  strike_seq <- seq(from = lower, to = upper, length = 100) 
  strike <- quantile(pred_rev, strike_seq)
  
  #Payout Matrix
  payout <- matrix(NA, nrow = length(pred_rev), ncol = length(strike))
  
  #Positive Correlation
  if(upper < 0.5) {
    for(i in 1:length(strike)){payout[,i] <- strike[i] - pred_rev}
    payout[payout < 0] = 0
  } else {
    for(i in 1:length(strike)){payout[,i] <- pred_rev- strike[i]}
    payout[payout < 0] = 0
  }
  
  #Compute the premiums 
  percent_expected_payout = apply(payout,2,mean)/apply(payout,2,max) * 100.0
  premium_basis_points = 221.04 * percent_expected_payout + 304.97
  braun_premium = premium_basis_points / 10000.0 * apply(payout,2,max)
  
  #Compute the hedged revenue
  hedged_revenues = matrix(NA, nrow = nrow(payout), ncol = length(strike))
  for(i in 1:length(strike)){
    hedged_revenues[,i] = unhedged + payout[,i] - braun_premium[i]
  }
  
  #Optimizing criteria
  revenue_var <- apply(hedged_revenues,2,function(x) quantile(x, probs=var_thresh))
  val_indx <- which.max(revenue_var)
  
  #Ouput the results
  return_list <- list(hedged_revenue = hedged_revenues[, val_indx],
                      Payout = payout[, val_indx],
                      strike = strike_seq[val_indx],
                      premium = braun_premium[val_indx]) 
  
  return(return_list)
  
}



#FUNCTION TO MATCH VARIANCE
match_var <- function(upper, lower, reg, newdata, portfolio_var, unhedged){
  
  #Compute the predicted revenue
  pred_rev <- predict(reg , newdata)
  
  #Create sequences of strikes -- Percentiles
  strike_seq <- seq(from = lower, to = upper, length = 400) 
  strike <- quantile(pred_rev, strike_seq)
  
  #Payout Matrix
  payout <- matrix(NA, nrow = length(pred_rev), ncol = length(strike))
  
  #Positive Correlation
  if(upper < 0.5) {
    for(i in 1:length(strike)){payout[,i] <- strike[i] - pred_rev}
    payout[payout < 0] = 0
  } else {
    for(i in 1:length(strike)){payout[,i] <- pred_rev- strike[i]}
    payout[payout < 0] = 0
  }
  
  #Compute the premiums 
  percent_expected_payout = apply(payout,2,mean)/apply(payout,2,max) * 100.0
  premium_basis_points = 221.04 * percent_expected_payout + 304.97
  braun_premium = premium_basis_points / 10000.0 * apply(payout,2,max)
  
  #Compute the hedged revenue
  hedged_revenues = matrix(NA, nrow = nrow(payout), ncol = length(strike))
  for(i in 1:length(strike)){
    hedged_revenues[,i] = unhedged + payout[,i] - braun_premium[i]
  }
  
  #Optimizing criteria
  revenue_var <- apply(hedged_revenues,2,var)
  val_indx <- which.min(abs(revenue_var-portfolio_var))
  
  #Ouput the results
  return_list <- list(hedged_revenue = hedged_revenues[, val_indx],
                      Payout = payout[, val_indx],
                      strike = strike_seq[val_indx],
                      premium = braun_premium[val_indx]) 
  
  return(return_list)
  
}



#_____________________________________________________________________#

###-------------Streamflow-----------------#
sf_index <- get_strike(upper=0.3,
                       lower=0.1,
                       reg=reg_streamflow,
                       newdata = data.frame(streamflow=reg_dataset$streamflow),
                       var_thresh = 0.05,
                       unhedged = reg_dataset$Net_revenue )

#SF plot
plot(reg_dataset$Net_revenue, type='l', 
     main = paste0("Streamflow Index \n Strike is ", 
                   round(100*sf_index$strike,2), 
                   "th percentile. Premium is ", 
                   round(1000*sf_index$premium,2), "$ Mil"),
     xlab = "Years", ylab = "Net Revenue")
lines(1:nrow(reg_dataset), sf_index$hedged_revenue, col='red')
abline(h = mean(reg_dataset$Net_revenue), lwd = 2 , lty = 2)
abline(h = mean(sf_index$hedged_revenue), lwd = 2 , lty = 2, col='red')
abline(h = quantile(reg_dataset$Net_revenue, ned_var), lwd = 2 , lty = 2)
abline(h = quantile(sf_index$hedged_revenue, ned_var), lwd = 2 , lty = 2, col='red')
abline(h = min(reg_dataset$Net_revenue), lwd = 2 )
abline(h = min(sf_index$hedged_revenue), lwd = 2, col='red')
legend("topleft", legend=c("Unmanaged", "Streamflow Index"),
       col=c("black", "red"), lty=1, lwd = 2, cex=0.95,
       title="Net Revenue", text.font=4, bg='lightblue')



###-------------CDD-------------------#
cdd_index <- get_strike(upper=0.3,
                        lower=0.1,
                        reg=reg_CDD,
                        newdata = data.frame(CDD=reg_dataset$CDD),
                        var_thresh = 0.05,
                        unhedged = reg_dataset$Net_revenue)

#CDD plot
plot(reg_dataset$Net_revenue, type='l', 
     main = paste0("CDD Index \n Strike is ", 
                   round(100*cdd_index$strike,2), 
                   "th percentile. Premium is ", 
                   round(1000*cdd_index$premium,2), "$ Mil"),
     xlab = "Years", ylab = "Net Revenue")
lines(1:nrow(reg_dataset), cdd_index$hedged_revenue, col='red')
abline(h = mean(reg_dataset$Net_revenue), lwd = 2 , lty = 2)
abline(h = mean(cdd_index$hedged_revenue), lwd = 2 , lty = 2, col='red')
abline(h = quantile(reg_dataset$Net_revenue, ned_var), lwd = 2 , lty = 2)
abline(h = quantile(cdd_index$hedged_revenue, ned_var), lwd = 2 , lty = 2, col='red')
abline(h = min(reg_dataset$Net_revenue), lwd = 2 )
abline(h = min(cdd_index$hedged_revenue), lwd = 2, col='red')
legend("topleft", legend=c("Unmanaged", "CDD Index"),
       col=c("black", "red"), lty=1, lwd = 2, cex=0.95,
       title="Net Revenue", text.font=4, bg='lightblue')



###-------------Natural Gas Price-------------------#
NG_index <- get_strike(upper=0.3,
                       lower=0.1,
                       reg=reg_price,
                       newdata = data.frame(Natural_Gas=reg_dataset$Natural_Gas),
                       var_thresh = 0.05,
                       unhedged = reg_dataset$Net_revenue)

#Natural Gas plot
plot(reg_dataset$Net_revenue, type='l', 
     main = paste0("Natural Gas Index \n Strike is ", 
                   round(100*NG_index$strike,2), 
                   "th percentile. Premium is ", 
                   round(1000*NG_index$premium,2), "$ Mil"),
     xlab = "Years", ylab = "Net Revenue")
lines(1:nrow(reg_dataset), NG_index$hedged_revenue, col='red')
abline(h = mean(reg_dataset$Net_revenue), lwd = 2 , lty = 2)
abline(h = mean(NG_index$hedged_revenue), lwd = 2 , lty = 2, col='red')
abline(h = quantile(reg_dataset$Net_revenue, ned_var), lwd = 2 , lty = 2)
abline(h = quantile(NG_index$hedged_revenue, ned_var), lwd = 2 , lty = 2, col='red')
abline(h = min(reg_dataset$Net_revenue), lwd = 2 )
abline(h = min(NG_index$hedged_revenue), lwd = 2, col='red')
legend("topleft", legend=c("Unmanaged", "Natural Gas Index"),
       col=c("black", "red"), lty=1, lwd = 2, cex=0.95,
       title="Net Revenue", text.font=4, bg='lightblue')




###-------------------Overall portfolio of insurance products-------------------------###

#Total Payout Portfolio
payout_portfolio <- cdd_index$Payout + sf_index$Payout + NG_index$Payout

#Total Premium  Portfolio
premium_portfolio <- cdd_index$premium + sf_index$premium + NG_index$premium

#Hedged Revenue Portfolio
hedged_net_revenues_portfolio <- reg_dataset$Net_revenue + payout_portfolio - premium_portfolio

#Create the plot
plot(reg_dataset$Net_revenue, type='l', 
     main = paste0("Portfolio of Contracts \n Premium is ", 
                   round(1000*premium_portfolio,2), "$ Mil"),
     xlab = "Years", ylab = "Net Revenue")
lines(1:nrow(reg_dataset), hedged_net_revenues_portfolio, col='red')
abline(h = mean(reg_dataset$Net_revenue), lwd = 2 , lty = 2)
abline(h = mean(hedged_net_revenues_portfolio), lwd = 2 , lty = 2, col='red')
abline(h = quantile(reg_dataset$Net_revenue, ned_var), lwd = 2 , lty = 2)
abline(h = quantile(hedged_net_revenues_portfolio, ned_var), lwd = 2 , lty = 2, col='red')
abline(h = min(reg_dataset$Net_revenue), lwd = 2)
abline(h = min(hedged_net_revenues_portfolio), lwd = 2 , col='red')
legend("topleft", legend=c("Unmanaged", "Portfolio of Contracts"),
       col=c("black", "red"), lty=1, lwd = 2, cex=0.95,
       title="Net Revenue", text.font=4, bg='lightblue')



###-------------Composite Index-------------------#
Composite_index <- get_strike(upper=0.3,
                              lower=0.01,
                              reg=reg_composite,
                              newdata = data.frame(streamflow = reg_dataset$streamflow,
                                                   CDD=reg_dataset$CDD,
                                                   Natural_Gas=reg_dataset$Natural_Gas),
                              var_thresh = 0.05,
                              unhedged = reg_dataset$Net_revenue)

#Composite Index plot
plot(reg_dataset$Net_revenue, type='l', 
     main = paste0("Composite Index - Maximize 5th Percentile \n Strike is ", 
                   round(100*Composite_index$strike,2), 
                   "th percentile. Premium is ", 
                   round(1000*Composite_index$premium,2), "$ Mil"),
     xlab = "Years", ylab = "Net Revenue")
lines(1:nrow(reg_dataset), Composite_index$hedged_revenue, col='red')
abline(h = mean(reg_dataset$Net_revenue), lwd = 2 , lty = 2)
abline(h = mean(Composite_index$hedged_revenue), lwd = 2 , lty = 2, col='red')
abline(h = quantile(reg_dataset$Net_revenue, ned_var), lwd = 2 , lty = 2)
abline(h = quantile(Composite_index$hedged_revenue, ned_var), lwd = 2 , lty = 2, col='red')
abline(h = min(reg_dataset$Net_revenue), lwd = 2 )
abline(h = min(Composite_index$hedged_revenue), lwd = 2, col='red')
legend("topleft", legend=c("Unmanaged", "Composite Index"),
       col=c("black", "red"), lty=1, lwd = 2, cex=0.95,
       title="Net Revenue", text.font=4, bg='lightblue')





###-------------Composite Index-------------------#
Composite_index <- match_var(upper=0.3,
                             lower=0.01,
                             reg=reg_composite,
                             newdata = data.frame(streamflow = reg_dataset$streamflow,
                                                  CDD=reg_dataset$CDD,
                                                  Natural_Gas=reg_dataset$Natural_Gas),
                             portfolio_var = var(hedged_net_revenues_portfolio),
                             unhedged = reg_dataset$Net_revenue)

#Composite Index plot
plot(reg_dataset$Net_revenue, type='l', 
     main = paste0("Composite Index - Same Variance \n Strike is ", 
                   round(100*Composite_index$strike,2), 
                   "th percentile. Premium is ", 
                   round(1000*Composite_index$premium,2), "$ Mil"),
     xlab = "Years", ylab = "Net Revenue")
lines(1:nrow(reg_dataset), Composite_index$hedged_revenue, col='red')
abline(h = mean(reg_dataset$Net_revenue), lwd = 2 , lty = 2)
abline(h = mean(Composite_index$hedged_revenue), lwd = 2 , lty = 2, col='red')
abline(h = quantile(reg_dataset$Net_revenue, ned_var), lwd = 2 , lty = 2)
abline(h = quantile(Composite_index$hedged_revenue, ned_var), lwd = 2 , lty = 2, col='red')
abline(h = min(reg_dataset$Net_revenue), lwd = 2 )
abline(h = min(Composite_index$hedged_revenue), lwd = 2, col='red')
legend("topleft", legend=c("Unmanaged", "Composite Index"),
       col=c("black", "red"), lty=1, lwd = 2, cex=0.95,
       title="Net Revenue", text.font=4, bg='lightblue')


#Ggpplot = Composite Index
plt_dataset <- data.frame(Year = rep(1:nrow(reg_dataset),2),
                          Revenue = c(reg_dataset$Net_revenue,Composite_index$hedged_revenue),
                          Type = rep(c("Unmanaged Revenues", "Managed Revenues"), each=499))

pdf("figures/paper/Composite_Revenues.pdf",height=8, width=12)
ggplot(plt_dataset) +
  geom_line(aes(x=Year, y=Revenue, color = Type), size = 1.15) +
  geom_hline(yintercept = min(reg_dataset$Net_revenue), 
             col = "#000000", linetype ='dashed', size = 1.05) +
  geom_hline(yintercept = min(Composite_index$hedged_revenue), 
             col = "#FF0000", linetype ='dashed', size = 1.05) +
  scale_x_continuous(name = "Years", limits = c(70,170)) +
  scale_y_continuous(name = "Net Revenue ($B)", limits = c(11.4,12.75)) +
  geom_hline(yintercept = mean(reg_dataset$Net_revenue), 
             col = "#000000", linetype ='dashed', size = 1) +
  geom_hline(yintercept = mean(Composite_index$hedged_revenue), 
             col = "#FF0000", linetype ='dashed', size = 1) +
  geom_segment(aes(x = 75, y = min(reg_dataset$Net_revenue), 
                   xend = 75, yend = min(Composite_index$hedged_revenue)),
               arrow = arrow(length = unit(0.5, "cm")), 
               color ='blue', size = 1.15) +
  geom_segment(aes(x = 70, y = mean(reg_dataset$Net_revenue), 
                   xend = 70, yend = mean(Composite_index$hedged_revenue)),
               arrow = arrow(length = unit(0.15, "cm")), 
               color ='blue', size = 1.15) +
  scale_x_continuous(name = "Years", limits = c(70,170)) +
  geom_text(aes(x = 85, y = 11.6, 
                label = "Net Revenue \n Floor Improvement"), 
            size = 5) +
  geom_text(aes(x = 70, y = 12.32, label = "Mean"), size = 5) +
  scale_color_manual(values = c("Unmanaged Revenues" = "#000000", 
                                "Managed Revenues" = "#FF0000")) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.08),
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14),
        axis.text.x = element_text(size = rel(1.75)),  
        axis.text.y = element_text(size = rel(1.75)),
        axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18))
dev.off()



pdf("figures/supplement/Composite_Revenues_all.pdf",height=8, width=12)
ggplot(plt_dataset) +
  geom_line(aes(x=Year, y=Revenue, color = Type)) +
  geom_hline(yintercept = min(reg_dataset$Net_revenue), 
             col = "#000000", linetype ='dashed', size = 1.05) +
  geom_hline(yintercept = min(Composite_index$hedged_revenue), 
             col = "#FF0000", linetype ='dashed', size = 1.05) +
  scale_x_continuous(name = "Years") +
  scale_y_continuous(name = "Net Revenue ($B)", limits = c(11.4,13)) +
  geom_hline(yintercept = mean(reg_dataset$Net_revenue), 
             col = "#000000", linetype ='dashed', size = 1) +
  geom_hline(yintercept = mean(Composite_index$hedged_revenue), 
             col = "#FF0000", linetype ='dashed', size = 1) +
  geom_segment(aes(x = 5, y = min(reg_dataset$Net_revenue), 
                   xend = 5, yend = min(Composite_index$hedged_revenue)),
               arrow = arrow(length = unit(0.5, "cm")), 
               color ='blue', size = 1.15) +
  geom_segment(aes(x = 0, y = mean(reg_dataset$Net_revenue), 
                   xend = 0, yend = mean(Composite_index$hedged_revenue)),
               arrow = arrow(length = unit(0.15, "cm")), 
               color ='blue', size = 1.15) +
  scale_x_continuous(name = "Years") +
  geom_text(aes(x = 30, y = 11.6, 
                label = "Net Revenue \n Floor Improvement"), 
            size = 5) +
  geom_text(aes(x = 0, y = 12.32, label = "Mean"), size = 5) +
  scale_color_manual(values = c("Unmanaged Revenues" = "#000000", 
                                "Managed Revenues" = "#FF0000")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14),
        axis.text.x = element_text(size = rel(1.75)),  
        axis.text.y = element_text(size = rel(1.75)),
        axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18))
dev.off()


#______________________________________________________________________#
###Additional Plots and Checks
print(paste0("The Variance of Unmanaged net revenues is ", var(1000*reg_dataset$Net_revenue)))
print(paste0("The Variance of portfolio index is ", var(1000*hedged_net_revenues_portfolio)))
print(paste0("The Variance of composite index is ", var(1000*Composite_index$hedged_revenue)))


print(paste0("The 5th percentile of Unmanaged net revenues is ", 1000*quantile(reg_dataset$Net_revenue, 0.05)))
print(paste0("The 5th percentile of portfolio index is ", 1000*quantile(hedged_net_revenues_portfolio, 0.05)))
print(paste0("The 5th percentile  of composite index is ", 1000*quantile(Composite_index$hedged_revenue, 0.05)))

#Measure of basis risk
basis_risk <- data.frame(Unhedhged = reg_dataset$Net_revenue,
                         Composite = Composite_index$Payout,
                         Portfolio = payout_portfolio)
cond <- basis_risk$Unhedhged < quantile(reg_dataset$Net_revenue, Composite_index$strike)
basis_risk$Unhedhged <- as.numeric(cond)
basis_risk$Composite[basis_risk$Composite > 0] <- 1
basis_risk$Portfolio[basis_risk$Portfolio > 0] <- 1

#Overpayments of the composite index
table(basis_risk$Unhedhged, basis_risk$Composite)/sum(basis_risk$Composite)
table(basis_risk$Unhedhged, basis_risk$Portfolio)/sum(basis_risk$Portfolio)


#Save the Datasets
revenues <- data.frame(Unmanaged = reg_dataset$Net_revenue,
                       Portfolio = hedged_net_revenues_portfolio,
                       Composite_index = Composite_index$hedged_revenue)
write.table(revenues, "sims/All_revenues_no_tax.csv", sep=",")

Unmanaged_Losses <- quantile(reg_dataset$Net_revenue, Composite_index$strike) - reg_dataset$Net_revenue
Unmanaged_Losses[Unmanaged_Losses < 0] = 0
payouts <- data.frame(Unmanaged = Unmanaged_Losses,
                      Portfolio = payout_portfolio,
                      Composite_index = Composite_index$Payout,
                      Streamflow = sf_index$Payout,
                      CDD = cdd_index$Payout,
                      NG = NG_index$Payout)
write.table(payouts, "sims/All_payouts_no_tax.csv", sep=",")


#----------------------------------------------------------------------#
#Create the plot
plot(reg_dataset$Net_revenue, type='l', main = "Annual Net Revenue",
     xlab = "Years", ylab = "Net Revenue")
lines(1:nrow(reg_dataset), hedged_net_revenues_portfolio, col='red')
lines(1:nrow(reg_dataset), Composite_index$hedged_revenue, col='blue')
abline(h = mean(reg_dataset$Net_revenue), lwd = 2 , lty = 2)
abline(h = mean(hedged_net_revenues_portfolio), lwd = 2 , lty = 2, col='red')
abline(h = mean(Composite_index$hedged_revenue), lwd = 2 , lty = 2, col='blue')
abline(h = min(reg_dataset$Net_revenue), lwd = 2)
abline(h = min(hedged_net_revenues_portfolio), lwd = 2 , col='red')
abline(h = min(Composite_index$hedged_revenue), lwd = 2 , col='blue')
abline(h = max(reg_dataset$Net_revenue), lwd = 2 , lty = 2)
abline(h = max(hedged_net_revenues_portfolio), lwd = 2 , lty = 2, col='red')
abline(h = max(Composite_index$hedged_revenue), lwd = 2 , lty = 2, col='blue')
abline(h = quantile(reg_dataset$Net_revenue, ned_var), lwd = 2,  lty = 2)
abline(h = quantile(hedged_net_revenues_portfolio, ned_var), lwd = 2 , lty = 2 , col='red')
abline(h = quantile(Composite_index$hedged_revenue, ned_var), lwd = 2 , lty = 2 , col='blue')
legend("topleft", legend=c("Unmanaged", "Composite Index", "Portfolio"),
       col=c("black", "blue", "red"), lty=1, lwd = 2, cex=0.95,
       title="Net Revenue", text.font=4, bg='lightblue')

par(mfrow=c(1,1))



#----------------------------------------------------------------------#
Net_Revenue <- reg_dataset$Net_revenue
Hedged_Revenue <- hedged_net_revenues_portfolio
Composite_Revene <- Composite_index$hedged_revenue


plt_dataset <- data.frame(Revenue = c(Net_Revenue, Hedged_Revenue),
                          Type = c(rep("Unmanaged", length(Net_Revenue)),
                                   rep("Portfolio", length(Hedged_Revenue))))

p1 <- ggplot(plt_dataset, aes(x=Revenue, fill = Type, pattern = Type)) +
  geom_histogram_pattern(
    pattern_angle = 135,
    pattern_density = 0.1,
    pattern_spacing = 0.025,
    color = 'black',
    position = 'identity', 
    alpha = 0.5, bins=30) +
  geom_vline(xintercept = min(reg_dataset$Net_revenue),
             color = "#E69F00", size = 1) +
  geom_vline(xintercept = min(hedged_net_revenues_portfolio),
             color = "#000000", size = 1) +
  #geom_vline(xintercept = quantile(reg_dataset$Net_revenue, ned_var),
  #           color = "#E69F00", size = 1, linetype = "dashed") +
  #geom_vline(xintercept = quantile(hedged_net_revenues_portfolio, ned_var),
  #           color = "#000000", size = 1, linetype = "dashed") +
  scale_fill_manual(values = c("#FFFFFF", "#E69F00")) +
  xlab("Revenue ($B)") +
  ylab("Count (No. of Years)") +
  scale_pattern_manual(values = c(Portfolio = "stripe", Unmanaged = "none")) +
  #ggtitle(paste0("Portfolio of Contracts \nCost - Annual Premium - ", 1000*round(premium_portfolio, 3), "$ Million")) +
  xlim(c(min(Net_Revenue),max(Net_Revenue))) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 14),
        axis.text.x = element_text(size = rel(1.75)),  
        axis.text.y = element_text(size = rel(1.75)),
        axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18))






plt_dataset <- data.frame(Revenue = c(reg_dataset$Net_revenue,
                                      Composite_index$hedged_revenue),
                          Type = c(rep("Unmanaged", nrow(reg_dataset)),
                                   rep("Composite_Index", nrow(reg_dataset))))


p2 <- ggplot(plt_dataset, aes(x=Revenue, fill = Type, pattern = Type)) +
  geom_histogram_pattern(
    pattern_angle = 135,
    pattern_density = 0.1,
    pattern_spacing = 0.025,
    color = 'black',
    position = 'identity', 
    alpha = 0.5, bins=30) +
  geom_vline(xintercept = min(reg_dataset$Net_revenue),
             color = "#E69F00", size = 1) +
  geom_vline(xintercept = min(Composite_index$hedged_revenue),
             color = "#000000", size = 1) +
  #geom_vline(xintercept = quantile(reg_dataset$Net_revenue, ned_var),
  #           color = "#E69F00", size = 1, linetype = "dashed") +
  #geom_vline(xintercept = quantile(composite_index$hedged_revenue, ned_var),
  #           color = "#000000", size = 1, linetype = "dashed") +
  scale_fill_manual(values = c("#FFFFFF", "#E69F00")) +
  scale_pattern_manual(values = c(Composite_Index = "wave", Unmanaged = "none")) +
  xlab("Revenue ($B)") +
  ylab("Count (No. of Years)") +
  #ggtitle(paste0("Composite Index Contract \nCost - Annual Premium - ", 1000*round(Composite_index$premium, 3), "$ Million")) +
  xlim(c(min(Net_Revenue),max(Net_Revenue))) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 14),
        axis.text.x = element_text(size = rel(1.75)),  
        axis.text.y = element_text(size = rel(1.75)),
        axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18))

pdf("figures/paper/Histogram_Revenues.pdf",height=7, width=14)
plot_grid(p2,p1,
          nrow =1,
          labels = c('A', 'B'), 
          label_size = 22)
dev.off()


#------------------------------------------------------------------------------------------#
#Histograms of Payouts 

#Unmanaged Losses
Unmanaged_Losses <- quantile(reg_dataset$Net_revenue, Composite_index$strike) - reg_dataset$Net_revenue
Unmanaged_Losses[Unmanaged_Losses < 0] = 0 


plt_dataset <- data.frame(Revenue = c(Unmanaged_Losses, 
                                      payout_portfolio),
                          Type = c(rep("Unmanaged", nrow(reg_dataset)),
                                   rep("Portfolio", nrow(reg_dataset))))  


p1 <- ggplot(plt_dataset, aes(x=Revenue, fill = Type, pattern = Type)) +
  geom_histogram_pattern(
    pattern_angle = 135,
    pattern_density = 0.1,
    pattern_spacing = 0.025,
    color = 'black',
    position = 'identity', 
    alpha = 0.5, bins=30) +
  scale_fill_manual(values = c("#FFFFFF", "#E69F00")) +
  scale_pattern_manual(values = c(Portfolio = "stripe", Unmanaged = "none")) +
  xlim(c(-0.1,0.35)) +
  scale_y_log(name = "Count (No. of Years)", 
                breaks=c(5, 10, 50, 100, 200, 300, 400, 500),
                limits = c(1,499)) +
  xlab("Revenue ($B)") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size=20)) 


plt_dataset <- data.frame(Revenue = c(Unmanaged_Losses, 
                                      Composite_index$Payout),
                          Type = c(rep("Unmanaged", nrow(reg_dataset)),
                                   rep("Composite_Index", nrow(reg_dataset))))  


p2 <- ggplot(plt_dataset, aes(x=Revenue, fill = Type, pattern = Type)) +
  geom_histogram_pattern(
    pattern_angle = 135,
    pattern_density = 0.1,
    pattern_spacing = 0.025,
    color = 'black',
    position = 'identity', 
    alpha = 0.5, bins=30) +
  scale_fill_manual(values = c("#FFFFFF", "#E69F00")) +
  scale_pattern_manual(values = c(Composite_Index = "wave", Unmanaged = "none")) +
  ggtitle(paste0("Composite Index Contract - Payouts \nAnnual Premium - ", 1000*round(Composite_index$premium, 3), "$ Million")) +
  xlim(c(-0.05,0.35)) +
  ylim(c(0,500)) +
  xlab("Revenue ($B)") +
  ylab("Count (No. of Years)") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size=20)) 

plot_grid(p1,p2,
          nrow =1)



#------------------------------------------------------------------------------------------#
# Figure 11 --> Add the boxplots for both 

plt_dataset <- data.frame(Revenue = c(Unmanaged_Losses, 
                                      payout_portfolio),
                          Type = c(rep("Unmanaged", nrow(reg_dataset)),
                                   rep("Portfolio", nrow(reg_dataset))))  



p1 <- ggplot(plt_dataset, aes(x=Revenue, fill = Type, pattern = Type)) +
  geom_histogram_pattern(
    pattern_angle = 135,
    pattern_density = 0.1,
    pattern_spacing = 0.025,
    color = 'black',
    position = 'identity', 
    alpha = 0.5, bins=30) +
  scale_fill_manual(values = c("#FFFFFF", "#E69F00")) +
  scale_pattern_manual(values = c(Portfolio = "stripe", Unmanaged = "none")) +
  ggtitle(paste0("Portfolio of Contracts - Payouts \nCost - Annual Premium - ", 1000*round(premium_portfolio, 3), "$ Million")) +
  xlim(c(-0.1,0.35)) +
  ylim(c(0,100)) +
  xlab("Revenue ($B)") +
  ylab("Count (No. of Years)") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size=20)) 


plt_dataset <- data.frame(Revenue = c(Unmanaged_Losses, 
                                      Composite_index$payout),
                          Type = c(rep("Unmanaged", nrow(reg_dataset)),
                                   rep("Composite_Index", nrow(reg_dataset))))  


p2 <- ggplot(plt_dataset, aes(x=Revenue, fill = Type, pattern = Type)) +
  geom_histogram_pattern(
    pattern_angle = 135,
    pattern_density = 0.1,
    pattern_spacing = 0.025,
    color = 'black',
    position = 'identity', 
    alpha = 0.5, bins=30) +
  scale_fill_manual(values = c("#FFFFFF", "#E69F00")) +
  scale_pattern_manual(values = c(Composite_Index = "wave", Unmanaged = "none")) +
  ggtitle(paste0("Composite Index Contract - Payouts \nCost - Annual Premium - ", 1000*round(Composite_index$premium, 3), "$ Million")) +
  xlim(c(-0.05,0.35)) +
  ylim(c(0,100)) +
  xlab("Revenue ($B)") +
  ylab("Count (No. of Years)") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size=20)) 

plot_grid(p1,p2,
          nrow =1)


#------------------------------------------------------------------------------------------#
# Figure 10 --> Add the boxplots for both 

#Unmanaged Losses
Unmanaged_Losses <- quantile(reg_dataset$Net_revenue, Composite_index$strike) - reg_dataset$Net_revenue
Unmanaged_Losses[Unmanaged_Losses < 0] = 0


plt_dataset <- data.frame(Unmanaged_Loss = -Unmanaged_Losses*1000,
                          Composite = Composite_index$Payout*1000,
                          Streamflow = sf_index$Payout*1000,
                          CDD = cdd_index$Payout*1000,
                          Natural_Gas = NG_index$Payout*1000)
plt_dataset <- t(plt_dataset)



#ns <- sample(1:470, 1) #ns <- 108, 392 106,316 #55,321, 58, 393, 322
#plt_dataset <- plt_dataset[,ns:(ns+20)]
#colnames(plt_dataset) <- c(1:21)

ns <- c( 222,  21, 80, 188, 401, 261,  213, 296, 109, 497, 225, 263, 155, 176,  17, 290, 285, 209, 436, 172) #sample(1:ncol(plt_dataset), 20)
#ns <- c( 222,  21, 80, 188, 401, 261,  213, 296, 109, 497, 225, 445, 155, 176,  17, 56, 473, 209, 120, 436) #sample(1:ncol(plt_dataset), 20)
plt_dataset <- plt_dataset[,ns]
colnames(plt_dataset) <- c(1:20)


# Example of how your data should be structured:
data <- data.frame(
  Year = rep(1:20, times = 5),
  Type = rep(c("Unmanaged Loss", "Composite Index Payout", "Streamflow Payout", "CDD Payout", "Natural Gas Payout"), each = 20),
  Value = c(plt_dataset[1,], plt_dataset[2,], plt_dataset[3,], plt_dataset[4,], plt_dataset[5,]) # This should be your actual data values
)


data$Type <- factor(data$Type, 
                    levels = c("Composite Index Payout", "Streamflow Payout", "CDD Payout", "Natural Gas Payout", "Unmanaged Loss"))


p <- ggplot(data, aes(x = Year, y = Value, fill = Type, color = Type)) +
  geom_bar(data = subset(data, Type != "Unmanaged Loss"), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Composite Index Payout" = "orange", 
                               "Streamflow Payout" = "brown", 
                               "CDD Payout" = "green", 
                               "Natural Gas Payout" = "black")) +
  scale_color_manual(values = c("Unmanaged Loss" = "blue")) +
  labs(fill = "", color = "", x = "Years", y = "($ Million)") +
  ylim(c(-300,300)) +
  theme_bw() +
  theme(legend.text = element_text(size = 22), 
        legend.title = element_blank(),
        axis.text.x = element_text(size = rel(1.75)),  
        axis.text.y = element_text(size = rel(1.75)),
        axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18)) 



legend_b <- get_legend(
  p + 
    guides(color = guide_legend(nrow = 4, override.aes = list(size=7)),
           shape = guide_legend(override.aes = list(size = 5)))
)


p <- p + 
  geom_line(data = subset(data, Type == "Unmanaged Loss"), 
            size = 1, linetype = "dashed", color = 'blue') +
  geom_point(data = subset(data, Type == "Unmanaged Loss"), 
             size = 2.5, color = 'blue')

for(i in 1:21){
  p <- p +  geom_segment(x = i-0.5, y = 0, xend = i-0.5, yend = 300, color = "black", linetype = 'dashed')
}

p <- p + theme(legend.position="none")


pdf("figures/paper/Payouts.pdf",height=7, width=14)
p + inset_element(legend_b, left = 0.05, bottom = 0.05, right = 0.25, top = 0.25)
dev.off()


dev.off()
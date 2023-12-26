#______________________________________________________________________________#
#This script includes the PGE Finanical Model -- Pollution Tax Scenario
#This script implements the risk mitigation strategies. 
### Main criteria the 95% VaR matches for the portfolio of contracts and composite index. 


#Point towards the working directory. 
setwd("~/GitHub/PGE_Composite_Index")

#______________________________________________________________________#
#Load data and dependencies

#Library
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggpattern)

#Load Functions
source("functions/get_composite_strike.R")
source("functions/get_strike.R")

pdf("figures/Risk_mitigation_no_tax.pdf", height=3700/600, width=6500/600)

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
pge_cities = c('FRESNO_T', 'SACRAMENTO_T','SAN.JOSE_T')
CDD = CDD[,pge_cities]
CDD = rowMeans(CDD)



#Global Hyper-parameters. (Interpret as 100-sel_var*100)
sel_var <- 0.15
ned_var <- 0.05


#Set-up regression dataset
reg_dataset <- data.frame(streamflow = log(streamflow), 
                          CDD = CDD,
                          Natural_Gas = Yearly_gas$V1,
                          Net_revenue = net_revenue$Net_revenue)

reg_dataset <- reg_dataset[-which.min(reg_dataset$Net_revenue), ]


#----------------------------------------------------------------------#
###Portfolio of Contracts

###------Streamflow----------###
reg_streamflow = lm(Net_revenue ~ streamflow,
                    reg_dataset)

#Compute the best strike 
streamflow_index <- get_strike(lower = 0.1, upper = 0.3,
                               regression = reg_streamflow,
                               var_thresh = sel_var)



#Create the plot
plot(reg_dataset$Net_revenue, type='l', 
     main = paste0("Streamflow Index \n Strike is ", 
                   round(100*streamflow_index$strike,2), 
                   "th percentile. Premium is ", 
                   round(1000*streamflow_index$premium,2), "$ Mil"),
     xlab = "Years", ylab = "Net Revenue")
lines(1:nrow(reg_dataset), streamflow_index$hedged_revenue, col='red')
abline(h = mean(reg_dataset$Net_revenue), lwd = 2 , lty = 2)
abline(h = mean(streamflow_index$hedged_revenue), lwd = 2 , lty = 2, col='red')
abline(h = quantile(reg_dataset$Net_revenue, ned_var), lwd = 2 , lty = 2)
abline(h = quantile(streamflow_index$hedged_revenue, ned_var), lwd = 2 , lty = 2, col='red')
abline(h = min(reg_dataset$Net_revenue), lwd = 2 )
abline(h = min(streamflow_index$hedged_revenue), lwd = 2, col='red')
legend("topleft", legend=c("Unmanaged", "Streamflow Index"),
       col=c("black", "red"), lty=1, lwd = 2, cex=0.95,
       title="Net Revenue", text.font=4, bg='lightblue')


###---------------------------Cooling Degree Days---------------------------###
reg_CDD = lm(Net_revenue ~ CDD,
             reg_dataset)

#Compute the best strike 
CDD_index <- get_strike(lower = 0.1, upper = 0.3,
                        regression = reg_CDD,
                        var_thresh = sel_var)

#Create the plot
plot(reg_dataset$Net_revenue, type='l',  
     main = paste0("CDD Index \n Strike is ", 
                   round(100*CDD_index$strike,2), 
                   "th percentile. Premium is ", 
                   round(1000*CDD_index$premium,2), "$ Mil"),
     xlab = "Years", ylab = "Net Revenue")
lines(1:nrow(reg_dataset), CDD_index$hedged_revenue, col='red')
abline(h = mean(reg_dataset$Net_revenue), lwd = 2 , lty = 2)
abline(h = mean(CDD_index$hedged_revenue), lwd = 2 , lty = 2, col='red')
abline(h = quantile(reg_dataset$Net_revenue, ned_var), lwd = 2 , lty = 2)
abline(h = quantile(CDD_index$hedged_revenue, ned_var), lwd = 2 , lty = 2, col='red')
abline(h = min(reg_dataset$Net_revenue), lwd = 2)
abline(h = min(CDD_index$hedged_revenue), lwd = 2 , col='red')
legend("topleft", legend=c("Unmanaged", "CDD Index"),
       col=c("black", "red"), lty=1, lwd = 2, cex=0.95,
       title="Net Revenue", text.font=4, bg='lightblue')



###-------------------------------Natural Gas-------------------------------###
reg_dataset$NP <- -reg_dataset$Natural_Gas
reg_price = lm(Net_revenue ~ NP,
               reg_dataset)

#Compute the best strike 
Price_index <- get_strike(lower = 0.1, upper = 0.3,
                          regression = reg_price,
                          var_thresh = sel_var)

#Create the plot
plot(reg_dataset$Net_revenue, type='l', 
     main = paste0("Natural Gas Index \n Strike is ", 
                   round(100*Price_index$strike,2), 
                   "th percentile. Premium is ", 
                   round(1000*Price_index$premium,2), "$ Mil"),
     xlab = "Years", ylab = "Net Revenue")
lines(1:nrow(reg_dataset), Price_index$hedged_revenue, col='red')
abline(h = mean(reg_dataset$Net_revenue), lwd = 2 , lty = 2)
abline(h = mean(Price_index$hedged_revenue), lwd = 2 , lty = 2, col='red')
abline(h = quantile(reg_dataset$Net_revenue, ned_var), lwd = 2 , lty = 2)
abline(h = quantile(Price_index$hedged_revenue, ned_var), lwd = 2 , lty = 2, col='red')
abline(h = min(reg_dataset$Net_revenue), lwd = 2)
abline(h = min(Price_index$hedged_revenue), lwd = 2, col='red')
legend("topleft", legend=c("Unmanaged", "Natural Gas Index"),
       col=c("black", "red"), lty=1, lwd = 2, cex=0.95,
       title="Net Revenue", text.font=4, bg='lightblue')


###-------------------Overall portfolio of insurance products-------------------------###

#Total Payout Portfolio
payout_portfolio <- CDD_index$Payout + streamflow_index$Payout + Price_index$Payout

#Total Premium  Portfolio
premium_portfolio <- CDD_index$premium + streamflow_index$premium + Price_index$premium

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


#________________________________________________________________________________#
#Compute Composite index with the same variance as the portfolio of indices



###Composite Index
reg_composite = lm(Net_revenue ~ streamflow + CDD + Natural_Gas,
                   reg_dataset)

composite_index <- get_composite_strike(lower = 0.05,
                                        upper = 0.2,
                                        composite_regression = reg_composite,
                                        portfolio_rev = reg_composite$model[,1],
                                        method = "same_thresh",
                                        thresh = 0.15)


#Create the plot
plot(reg_dataset$Net_revenue, type='l', 
     main = paste0("Composite Index Insurance \n Strike is ", 
                   round(100*composite_index$strike,2), 
                   "th percentile. Premium is ", 
                   round(1000*composite_index$premium,2), "$ Mil"),
     xlab = "Years", ylab = "Net Revenue")
lines(1:nrow(reg_dataset), composite_index$hedged_revenue, col='blue')
abline(h = mean(reg_dataset$Net_revenue), lwd = 2 , lty = 2)
abline(h = mean(composite_index$hedged_revenue), lwd = 2 , lty = 2, col='blue')
abline(h = min(reg_dataset$Net_revenue), lwd = 2)
abline(h = min(composite_index$hedged_revenue), lwd = 2 , col='blue')
abline(h = max(reg_dataset$Net_revenue), lwd = 2 , lty = 2)
abline(h = max(composite_index$hedged_revenue), lwd = 2 , lty = 2, col='blue')
abline(h = quantile(reg_dataset$Net_revenue, ned_var), lwd = 2,  lty = 2)
abline(h = quantile(composite_index$hedged_revenue, ned_var), lwd = 2 , lty = 2 , col='blue')
legend("topleft", legend=c("Unmanaged", "Composite Index"),
       col=c("black", "blue"), lty=1, lwd = 2, cex=0.95,
       title="Net Revenue", text.font=4, bg='lightblue')

par(mfrow=c(1,1))



#Create the plot
plot(reg_dataset$Net_revenue, type='l', main = "Annual Net Revenue",
     xlab = "Years", ylab = "Net Revenue")
lines(1:nrow(reg_dataset), hedged_net_revenues_portfolio, col='red')
lines(1:nrow(reg_dataset), composite_index$hedged_revenue, col='blue')
abline(h = mean(reg_dataset$Net_revenue), lwd = 2 , lty = 2)
abline(h = mean(hedged_net_revenues_portfolio), lwd = 2 , lty = 2, col='red')
abline(h = mean(composite_index$hedged_revenue), lwd = 2 , lty = 2, col='blue')
abline(h = min(reg_dataset$Net_revenue), lwd = 2)
abline(h = min(hedged_net_revenues_portfolio), lwd = 2 , col='red')
abline(h = min(composite_index$hedged_revenue), lwd = 2 , col='blue')
abline(h = max(reg_dataset$Net_revenue), lwd = 2 , lty = 2)
abline(h = max(hedged_net_revenues_portfolio), lwd = 2 , lty = 2, col='red')
abline(h = max(composite_index$hedged_revenue), lwd = 2 , lty = 2, col='blue')
abline(h = quantile(reg_dataset$Net_revenue, ned_var), lwd = 2,  lty = 2)
abline(h = quantile(hedged_net_revenues_portfolio, ned_var), lwd = 2 , lty = 2 , col='red')
abline(h = quantile(composite_index$hedged_revenue, ned_var), lwd = 2 , lty = 2 , col='blue')
legend("topleft", legend=c("Unmanaged", "Composite Index", "Portfolio"),
       col=c("black", "blue", "red"), lty=1, lwd = 2, cex=0.95,
       title="Net Revenue", text.font=4, bg='lightblue')

par(mfrow=c(1,1))



print(paste0("The Variance of Unmanaged net revenues is ", var(1000*reg_dataset$Net_revenue)))
print(paste0("The Variance of portfolio index is ", var(1000*hedged_net_revenues_portfolio)))
print(paste0("The Variance of composite index is ", var(1000*composite_index$hedged_revenue)))


print(paste0("The 5th percentile of Unmanaged net revenues is ", 1000*quantile(reg_dataset$Net_revenue, 0.05)))
print(paste0("The 5th percentile of portfolio index is ", 1000*quantile(hedged_net_revenues_portfolio, 0.05)))
print(paste0("The 5th percentile  of composite index is ", 1000*quantile(composite_index$hedged_revenue, 0.05)))


#------------------------------------------------------------------------------------------#
# Figure 9 --> Add the boxplots for both 

plt_dataset <- data.frame(Revenue = c(reg_dataset$Net_revenue, 
                                      hedged_net_revenues_portfolio),
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
  ggtitle(paste0("Portfolio of Contracts \nCost - Annual Premium - ", 1000*round(premium_portfolio, 3), "$ Million")) +
  xlim(c(min(reg_dataset$Net_revenue),max(reg_dataset$Net_revenue))) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size=20)) 






plt_dataset <- data.frame(Revenue = c(reg_dataset$Net_revenue,
                                      composite_index$hedged_revenue),
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
  geom_vline(xintercept = min(composite_index$hedged_revenue),
             color = "#000000", size = 1) +
  #geom_vline(xintercept = quantile(reg_dataset$Net_revenue, ned_var),
  #           color = "#E69F00", size = 1, linetype = "dashed") +
  #geom_vline(xintercept = quantile(composite_index$hedged_revenue, ned_var),
  #           color = "#000000", size = 1, linetype = "dashed") +
  scale_fill_manual(values = c("#FFFFFF", "#E69F00")) +
  scale_pattern_manual(values = c(Composite_Index = "wave", Unmanaged = "none")) +
  xlab("Revenue ($B)") +
  ylab("Count (No. of Years)") +
  ggtitle(paste0("Composite Index Contract \nCost - Annual Premium - ", 1000*round(composite_index$premium, 3), "$ Million")) +
  xlim(c(min(reg_dataset$Net_revenue),max(reg_dataset$Net_revenue))) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size=20)) 


plot_grid(p1,p2,
          nrow =1)


#------------------------------------------------------------------------------------------#
# Figure 10 --> Add the boxplots for both 

#Unmanaged Losses
Unmanaged_Losses <- quantile(reg_dataset$Net_revenue, composite_index$strike) - reg_dataset$Net_revenue
Unmanaged_Losses[Unmanaged_Losses < 0] = 0


plt_dataset <- data.frame(Unmanaged_Loss = -Unmanaged_Losses,
                          Composite = composite_index$payout,
                          Streamflow = streamflow_index$Payout,
                          CDD = CDD_index$Payout,
                          Natural_Gas = Price_index$Payout)
plt_dataset <- t(plt_dataset)

#ns <- sample(1:470, 1) #ns <- 108, 392 106,316 #55,321, 58, 393, 322
#plt_dataset <- plt_dataset[,ns:(ns+20)]
#colnames(plt_dataset) <- c(1:21)

ns <- c( 222,  21, 209, 188, 401, 261,  213, 296, 109, 497, 225, 263, 155, 176,  17, 290, 285, 80, 436,   172) #sample(1:ncol(plt_dataset), 20)
plt_dataset <- plt_dataset[,ns]
colnames(plt_dataset) <- c(1:20)


plt2 <- plt_dataset[2:5,]

x <- 1:100
y <- rep(plt_dataset[1,], each = 5)



par(mfrow = c(1,1),  mar = c(5, 5, 3, 2))

barplot(plt2,
        col = c("#0096FF", "#DCDCDC", "#808080", "#B2BEB5"),
        beside = TRUE,
        ylim = c(-0.35, 0.35), border = TRUE,
        ylab = "($ Billion)", 
        xlab = "Years", cex.lab=1.5, cex.legend = 1.75)
lines(x,y, lwd = 3, lty = 2 , col ='red')
for(i in 1:20){
  lines(x= c(0.5+5*i, 0.5+5*i), y=c(0,0.28), lwd = 0.05, lty =2)}
legend('topright', 
       legend  = c("Unmanaged Loss",  "Composite Payout", "Streamflow Payout", "Temperature Payout", "Natural Gas Payout"),
       col = c("red","#0096FF", "#DCDCDC", "#808080", "#B2BEB5"),
       pch = 15, cex = 0.9, ncol=5, 
       pt.cex=c(0,2.5,2.5,2.5,2.5,2.5),
       lwd=c(2,NA,NA,NA,NA,NA),
       lty=c(2,NA,NA,NA,NA,NA))








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
  ggtitle(paste0("Portfolio of Contracts - Payouts \nAnnual Premium - ", 1000*round(premium_portfolio, 3), "$ Million")) +
  xlim(c(-0.1,0.35)) +
  ylim(c(0,500)) +
  xlab("Revenue ($B)") +
  ylab("Count (No. of Years)") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size=20)) 


plt_dataset <- data.frame(Revenue = c(Unmanaged_Losses, 
                                      composite_index$payout),
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
  ggtitle(paste0("Composite Index Contract - Payouts \nAnnual Premium - ", 1000*round(composite_index$premium, 3), "$ Million")) +
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
                                      composite_index$payout),
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
  ggtitle(paste0("Composite Index Contract - Payouts \nCost - Annual Premium - ", 1000*round(composite_index$premium, 3), "$ Million")) +
  xlim(c(-0.05,0.35)) +
  ylim(c(0,100)) +
  xlab("Revenue ($B)") +
  ylab("Count (No. of Years)") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size=20)) 

plot_grid(p1,p2,
          nrow =1)

dev.off()

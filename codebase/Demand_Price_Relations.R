
#Point towards the working directory. 
setwd("~/GitHub/PGE_Composite_Index")

library(ggplot2)
library(corrplot)
library(zoo)


pdf("figures/Demand_Price_CDD_Diagnostics_Annual.pdf")

###Deamnd Data###
Demand <- read.csv("data/Simulated_Demand.csv", header = FALSE)
Demand <- matrix(Demand$V1, nrow = 8760, ncol = 500)
Demand <- as.data.frame(Demand)
#Daily Steps
daily_demand <- sapply(Demand, function(column) rollapply(column, width = 24, by = 24, FUN = mean, align = "left", partial = TRUE))
daily_demand <- daily_demand[1:363,]
daily_demand <- as.numeric(unlist(daily_demand))
#Annual Time Steps
yearly_demand <- colMeans(Demand)
Demand <- NULL



#Price Daata
Price <- read.csv("data/no_tax/Simulated_Price_Variable_NG.csv", header = FALSE)
daily_price <- sapply(Price, function(column) rollapply(column, width = 24, by = 24, FUN = mean, align = "left", partial = TRUE))
daily_price <- as.numeric(unlist(daily_price))
annual_price <- colMeans(Price)
Price <- NULL


#CDD
CDD <- read.csv("data/CDD.csv")
pge_cities = c('FRESNO_T', 'SACRAMENTO_T','SAN.JOSE_T', 'SAN.FRANCISCO_T')
CDD = CDD[,pge_cities]
CDD = rowMeans(CDD)


#Streamflow
streamflow <- read.csv("data/Streamflow.csv")
sites = c('ORO_fnf', 'SHA_fnf', 'FOL_fnf', 'PAR_fnf', 'NML_fnf', 'MIL_fnf', 'PFT_fnf')
streamflow = streamflow[,sites]
streamflow = rowMeans(streamflow)


#Net Revenue
net_revenue <- read.csv("sims/Net_Revenue_no_tax.csv", header = TRUE, sep=",")


#Naural Gas
Yearly_gas <- read.csv("data/Yearly_gas.csv", header = FALSE)




#-------------Annual Time Scale-----------------------------#
plt_dataset <- data.frame(Demand = yearly_demand, 
                          streamflow = streamflow,
                          CDD = CDD,
                          Price = annual_price,
                          NG = Yearly_gas$V1,
                          Revenue = net_revenue$Net_revenue)

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




ggplot(plt_dataset) +
  geom_point(aes(x=Demand, y=CDD, col = Price)) +
  scale_color_gradient2(midpoint=mean(plt_dataset$Price),
                       low="blue", mid="white",high="red") +
  ggtitle("CDD vs Demand") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 8))


ggplot(plt_dataset) +
  geom_point(aes(x=Demand, y=streamflow, col = Price)) +
  scale_color_gradient2(midpoint=mean(plt_dataset$Price),
                        low="blue", mid="white",high="red") +
  ggtitle("Streamflow vs Demand") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 8))


ggplot(plt_dataset) +
  geom_point(aes(x=Demand, y=Price, col = CDD)) +
  scale_color_gradient2(midpoint=mean(plt_dataset$CDD),
                        low="blue", mid="white",high="red") +
  ggtitle("Price vs Demand") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 8))


ggplot(plt_dataset) +
  geom_point(aes(x=Demand, y=Price, col = streamflow)) +
  scale_color_gradient2(midpoint=mean(plt_dataset$streamflow),
                        low="blue", mid="white",high="red") +
  ggtitle("Price vs Demand") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 8))


quants <- quantile(plt_dataset$streamflow, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
plt_dataset$discrete_streamflow <- cut(plt_dataset$streamflow, breaks = 4, labels = c("Low", "Medium", "High" , " Very High"))

plt_dataset$SF_Quant <- rank(plt_dataset$streamflow)/nrow(plt_dataset)

ggplot(plt_dataset) +
  geom_point(aes(x=Demand, y=Price, col = CDD, shape = factor(discrete_streamflow))) +
  scale_color_gradient2(midpoint=median(plt_dataset$CDD),
                        low="blue", mid="white",high="red") +
  ggtitle("Price vs Demand") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 6))


ggplot(plt_dataset) +
  geom_point(aes(x=Demand, y=Revenue, 
                 col = CDD, 
                 shape = factor(discrete_streamflow),
                 size = Price)) +
  scale_color_gradient2(midpoint=median(plt_dataset$CDD),
                        low="blue", mid="white",high="red") +
  scale_size(range = c(1, 3)) +
  ggtitle("Revenue vs Demand") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 6))

#Select values to highlight
top_values <- sort(plt_dataset$Price, decreasing = TRUE)[1:3]
high_price <- plt_dataset[plt_dataset$Price %in% top_values,]

top_values <- sort(plt_dataset$CDD, decreasing = TRUE)[1:3]
high_cdd <- plt_dataset[plt_dataset$CDD %in% top_values,]

top_values <- sort(plt_dataset$Demand, decreasing = TRUE)[1:3]
high_demand <- plt_dataset[plt_dataset$Demand %in% top_values,]

top_values <- sort(plt_dataset$NG, decreasing = TRUE)[1:3]
high_gas <- plt_dataset[plt_dataset$NG %in% top_values,]



ggplot(plt_dataset) +
  geom_point(aes(y=Revenue, x=CDD, 
                 shape = factor(discrete_streamflow),
                 col = Price)) +
  geom_point(data = high_price, 
             mapping = aes(x=CDD,y=Revenue), 
             shape = 21, color = "red", fill = NA, size = 5) +
  geom_point(data = high_cdd, 
             mapping = aes(x=CDD,y=Revenue), 
             shape = 24, color = "blue", fill = NA, size = 5) +
  scale_color_gradient2(midpoint=median(plt_dataset$Price),
                        low="blue", mid="green",high="red") +
  geom_hline(yintercept = quantile(plt_dataset$Revenue, 0.15), 
             col = "black", linetype ='dashed', fill = NA) +
  scale_size(range = c(1, 3)) +
  ggtitle("Revenue vs CDD") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 6))



ggplot(plt_dataset) +
  geom_point(aes(y=Revenue, x=Price, 
                 shape = factor(discrete_streamflow),
                 col = CDD)) +
  geom_point(data = high_cdd, 
             mapping = aes(x=Price,y=Revenue), 
             shape = 24, color = "blue", fill = NA, size = 5) +
  geom_point(data = high_price, 
             mapping = aes(x=Price,y=Revenue), 
             shape = 21, color = "red", fill = NA, size = 5) +
  scale_color_gradient2(midpoint=median(plt_dataset$CDD),
                        low="blue", mid="green",high="red") +
  geom_hline(yintercept = quantile(plt_dataset$Revenue, 0.20), 
             col = "black", linetype ='dashed') +
  scale_size(range = c(1, 3)) +
  ggtitle("Revenue vs Price") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 6))


ggplot(plt_dataset) +
  geom_point(aes(y=Revenue, x=NG, 
                 shape = factor(discrete_streamflow),
                 col = CDD)) +
  geom_point(data = high_cdd, 
             mapping = aes(x=NG,y=Revenue), 
             shape = 24, color = "blue", fill = NA, size = 5) +
  geom_point(data = high_price, 
             mapping = aes(x=NG,y=Revenue), 
             shape = 21, color = "red", fill = NA, size = 5) +
  scale_color_gradient2(midpoint=median(plt_dataset$CDD),
                        low="blue", mid="green",high="red") +
  geom_hline(yintercept = quantile(plt_dataset$Revenue, 0.20), 
             col = "black", linetype ='dashed') +
  scale_size(range = c(1, 3)) +
  ggtitle("Revenue vs Natural Gas Price") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 6))

ggplot(plt_dataset) +
  geom_point(aes(y=Price, x=NG))

dev.off()

pdf("figures/paper/Revenue_Correlation.pdf", 
    height=7, width=8)

ggplot(plt_dataset) +
  geom_point(aes(y=Revenue, x=NG, 
                 shape = factor(discrete_streamflow),
                 col = CDD), size = 2) +
  geom_point(data = high_cdd, 
             mapping = aes(x=NG,y=Revenue), 
             shape = 24, color = "blue", fill = NA, size = 5) +
  geom_point(data = high_price, 
             mapping = aes(x=NG,y=Revenue), 
             shape = 21, color = "red", fill = NA, size = 7) +
  scale_color_gradient2(midpoint=median(plt_dataset$CDD),
                        low="blue", mid="green",high="red") +
  geom_hline(yintercept = quantile(plt_dataset$Revenue, 0.20), 
             col = "black", linetype ='dashed') +
  scale_size(range = c(1, 3)) +
  ylab("Net Revenue ($B)") +  
  xlab("Natural Gas Price ($/Million Btu)") +
  ggtitle("Net Revenue and influence of indices") +
  guides(color = guide_colourbar(barwidth = rel(10), barheight = rel(1))) +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)) +
  labs(shape = "Streamflow") 

dev.off()

#-------------Daily Time Scale-----------------------------#

#pdf("figures/Demand_Price_CDD_Diagnostics_Hourly.pdf")

#plt_dataset <- data.frame(Demand = daily_demand,
#                          Price = daily_price)
#

#ggplot(plt_dataset) +
#  geom_point(mapping = aes(x=Demand, y=Price), alpha = 1/20) +
#  ggtitle("Price vs Demand") +
#  theme_bw()


#ggplot(plt_dataset) +
#  geom_point(mapping = aes(x=log(Demand), y=log(Price)), alpha = 1/20) +
#ggtitle("Price vs Demand") +
#  theme_bw()





#dev.off()






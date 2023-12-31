
#Point towards the working directory. 
setwd("~/GitHub/PGE_Composite_Index")

library(ggplot2)


pdf("figures/Demand_Price_CDD_Diagnostics_Annual.pdf")

###Deamnd Data###
Demand <- read.csv("data/Simulated_Demand.csv", header = FALSE)
Demand <- matrix(Demand$V1, nrow = 8760, ncol = 500)
Demand <- as.data.frame(Demand)
#Daily Steps
daily_demand <- sapply(Demand, function(column) rollapply(column, width = 24, by = 24, FUN = mean, align = "left", partial = TRUE))
daily_demand <- daily_demand[1:363,]
daily_demand <- as.numeric(unlist(daily_demand))
#Annual Time Steos
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




#-------------Annual Time Scale-----------------------------#
plt_dataset <- data.frame(Demand = yearly_demand, 
                          CDD = CDD,
                          Price = annual_price)
ggplot(plt_dataset) +
  geom_point(aes(y=Demand, x=CDD, col = Price)) +
  scale_color_gradient2(midpoint=mean(plt_dataset$Price),
                       low="blue", mid="white",high="red") +
  ggtitle("CDD vs Demand") +
  theme_bw()


ggplot(plt_dataset) +
  geom_point(aes(x=Demand, y=Price, col = CDD)) +
  scale_color_gradient2(midpoint=mean(plt_dataset$CDD),
                        low="blue", mid="white",high="red") +
  ggtitle("Price vs Demand") +
  theme_bw()

dev.off()


#-------------Daily Time Scale-----------------------------#

pdf("figures/Demand_Price_CDD_Diagnostics_Hourly.pdf")

plt_dataset <- data.frame(Demand = daily_demand,
                          Price = daily_price)


ggplot(plt_dataset) +
  geom_point(mapping = aes(x=Demand, y=Price), alpha = 1/20) +
  ggtitle("Price vs Demand") +
  theme_bw()


ggplot(plt_dataset) +
  geom_point(mapping = aes(x=log(Demand), y=log(Price)), alpha = 1/20) +
  ggtitle("Price vs Demand") +
  theme_bw()


dev.off()






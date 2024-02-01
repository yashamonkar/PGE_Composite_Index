
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
max_demand <- apply(Demand, 2, max)
#Daily Steps
daily_demand <- sapply(Demand, function(column) rollapply(column, width = 24, by = 24, FUN = mean, align = "left", partial = TRUE))
daily_demand <- daily_demand[1:363,]
daily_demand <- as.numeric(unlist(daily_demand))
#Annual Time Steps
yearly_demand <- colMeans(Demand)
Demand <- NULL



#Price Daata
Price <- read.csv("data/no_tax/Simulated_Price_Variable_NG.csv", header = FALSE)
max_price <- apply(Price, 2, max)
daily_price <- sapply(Price, function(column) rollapply(column, width = 24, by = 24, FUN = mean, align = "left", partial = TRUE))
daily_price <- as.numeric(unlist(daily_price))
annual_price <- colMeans(Price)

count_exceed <- list()
for(i in 1:500){
  count_exceed[[i]] <- sum(Price[,i] > 1000)
}
count_exceed <- unlist(count_exceed)

Price <- NULL

#Max Temperautre -- Daily
Temp <- read.csv("data/temp_annual.csv")
pge_cities = c('FRESNO_T', 'SACRAMENTO_T','SAN.JOSE_T', 'SAN.FRANCISCO_T')
Temp = Temp[,pge_cities]
Temp = data.frame(Temp = rowMeans(Temp),
                  Year = rep(1:500, each = 365))
Temp = Temp %>% group_by(Year) %>% summarize(max(Temp))
max_temp = Temp$`max(Temp)`
Temp = NULL

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



plt_dataset <- data.frame(Demand = yearly_demand, 
                          streamflow = streamflow,
                          CDD = CDD,
                          Price = annual_price,
                          NG = Yearly_gas$V1,
                          Revenue = net_revenue$Net_revenue)


pairs(plt_dataset, lower.panel = lower.panel, 
      upper.panel = NULL, diag.panel = panel.hist)


#Add the count exceedances
plt_dataset$Exceed <- count_exceed
plt_dataset <- plt_dataset[-which.min(plt_dataset$Revenue), ]

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


extreme_price <- plt_dataset[plt_dataset$Exceed > 0,]



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
  geom_point(aes(y=Revenue, x=NG, 
                 shape = factor(discrete_streamflow),
                 col = CDD)) +
  geom_point(data = high_cdd, 
             mapping = aes(x=NG,y=Revenue), 
             shape = 24, color = "blue", fill = NA, size = 5) +
  geom_point(data = high_price, 
             mapping = aes(x=NG,y=Revenue), 
             shape = 21, color = "red", fill = NA, size = 5) +
  geom_point(data = extreme_price, 
             mapping = aes(x=NG,y=Revenue), 
             shape = 21, color = "black", fill = NA, size = 5) +
  scale_color_gradient2(midpoint=median(plt_dataset$CDD),
                        low="blue", mid="green",high="red") +
  geom_hline(yintercept = quantile(plt_dataset$Revenue, 0.20), 
             col = "black", linetype ='dashed') +
  scale_size(range = c(1, 3)) +
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
  geom_point(data = extreme_price, 
             mapping = aes(x=Price,y=Revenue), 
             shape = 21, color = "black", fill = NA, size = 5) +
  scale_color_gradient2(midpoint=median(plt_dataset$CDD),
                        low="blue", mid="green",high="red") +
  geom_hline(yintercept = quantile(plt_dataset$Revenue, 0.20), 
             col = "black", linetype ='dashed') +
  scale_size(range = c(1, 3)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6), 
        legend.title = element_text(size = 6))


ggplot(plt_dataset) +
  geom_point(aes(y=Price, x=NG))

dev.off()

pdf("figures/paper/Revenue_Correlation.pdf", 
    height=7, width=8)


high_cdd$Labels <- c("B", "A", "C")
high_price$Labels <- c("E", "D", "C")

ggplot(plt_dataset) +
  geom_point(aes(y=Revenue, x=NG, 
                 shape = factor(discrete_streamflow),
                 col = CDD), size = 2) +
  geom_point(data = high_cdd, 
             mapping = aes(x=NG,y=Revenue), 
             shape = 24, color = "blue", fill = NA, size = 5) + 
  geom_text(data = high_cdd, 
            mapping = aes(x=NG,y=Revenue, label = Labels), 
            nudge_y = 0.05, vjust = 0) +
  geom_point(data = high_price, 
             mapping = aes(x=NG,y=Revenue), 
             shape = 21, color = "red", fill = NA, size = 7) +
  geom_text(data = high_price, 
            mapping = aes(x=NG,y=Revenue, label = Labels), 
            nudge_y = 0.05, vjust = 0) +
  scale_color_gradient2(midpoint=median(plt_dataset$CDD),
                        low="blue", mid="green",high="red") +
  geom_hline(yintercept = quantile(plt_dataset$Revenue, 0.15), 
             col = "black", linetype ='dashed') +
  scale_size(range = c(1, 3)) +
  ylab("Net Revenue ($B)") +  
  xlab("Natural Gas Price ($/Million Btu)") +
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



#________________________________________________________________________#
###Does not work


#Creating the plot in ggplot2
quant50 <- apply(Price, 1, function(x) quantile(x, probs = 0.5))
quant90 <- apply(Price, 1, function(x) quantile(x, probs = 0.9))
quant95 <- apply(Price, 1, function(x) quantile(x, probs = 0.95))
quant99 <- apply(Price, 1, function(x) quantile(x, probs = 0.99))
quantmax <- apply(Price, 1, max)

st_date <- as.POSIXct("01-01-2019 00:00", format="%m-%d-%Y %H:%M")
time_stamps <- seq(st_date, by = "hour", length.out = 8712)




plt_dataset <- data.frame(DOY = rep(time_stamps, 5),
                          Price = c(quant50, quant90, quant95, quant99, quantmax), 
                          Type = rep(c("Median", "90th quantile", 
                                       "95th quantile", "99th quantile", 
                                       "max"), each = 8712))
plt_dataset$Type <- factor(plt_dataset$Type, levels = c("Median", "90th quantile", "95th quantile", "99th quantile", "max"))


ggplot(plt_dataset) +
  geom_line(aes(x=DOY, y= log10(Price), color = Type)) +
  scale_y_continuous(name = "Market Price ($/Mwhr)", limits = c(1.4,3.1), 
                     labels =  function(x) signif(10^x, digits = 2)) +
  scale_color_manual(values = c("Median" = "#0000FF", 
                                "90th quantile" = "#660099", 
                                "95th quantile" = "pink", 
                                "99th quantile" = "#CC0033", 
                                "max" = "#FF0000")) +
  xlab("Day of the Year (Representative Year)") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)) +
  guides(color = guide_legend(title = "Type", 
                              order = 1,
                              override.aes = list(size = 3)))



plt_dataset <- data.frame(DOY = rep(time_stamps, 6),
                          Price = c(quant50, quant90, quant95, quant99, quantmax, Price[,101]), 
                          Type = rep(c("Median", "90th quantile", 
                                       "95th quantile", "99th quantile", 
                                       "max", "Year-101"), each = 8712))
plt_dataset$Type <- factor(plt_dataset$Type, levels = c("Median", "90th quantile", "95th quantile", "99th quantile", "max", "Year-101"))


ggplot(plt_dataset) +
  geom_line(aes(x=DOY, y= log10(Price), color = Type)) +
  scale_y_continuous(name = "Market Price ($/Mwhr)", limits = c(1.4,3.1),
                     labels =  function(x) signif(10^x, digits = 2)) +
  scale_color_manual(values = c("Median" = "#0000FF", 
                                "90th quantile" = "#660099", 
                                "95th quantile" = "pink", 
                                "99th quantile" = "#CC0033", 
                                "max" = "#FF0000",
                                "Year-101" = "black")) +
  xlab("Day of the Year (Representative Year)") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 14),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14))










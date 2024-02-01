Price <- read.csv("data/no_tax/Simulated_Price_Variable_NG.csv", header = FALSE)


#Creating the plot in ggplot2
quant50 <- apply(Price, 1, function(x) quantile(x, probs = 0.5))
quant90 <- apply(Price, 1, function(x) quantile(x, probs = 0.9))
quant95 <- apply(Price, 1, function(x) quantile(x, probs = 0.95))
quant99 <- apply(Price, 1, function(x) quantile(x, probs = 0.99))
quantmax <- apply(Price, 1, max)

st_date <- as.POSIXct("01-01-2019 00:00", format="%m-%d-%Y %H:%M")
time_stamps <- seq(st_date, by = "hour", length.out = 8712)

count_exceed <- list()
for(i in 1:500){
  count_exceed[[i]] <- sum(Price[,i] > 500)
}
count_exceed <- unlist(count_exceed)



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
Price <- NULL




###------------Deamnd Data---------------###
Price <- read.csv("data/Simulated_Demand.csv", header = FALSE)
Price <- matrix(Price$V1, nrow = 8760, ncol = 500)

#Creating the plot in ggplot2
quant50 <- apply(Price, 1, function(x) quantile(x, probs = 0.5))
quant90 <- apply(Price, 1, function(x) quantile(x, probs = 0.9))
quant95 <- apply(Price, 1, function(x) quantile(x, probs = 0.95))
quant99 <- apply(Price, 1, function(x) quantile(x, probs = 0.99))
quantmax <- apply(Price, 1, max)

st_date <- as.POSIXct("01-01-2019 00:00", format="%m-%d-%Y %H:%M")
time_stamps <- seq(st_date, by = "hour", length.out = 8760)




plt_dataset <- data.frame(DOY = rep(time_stamps, 2),
                          Price = c(quant50, quantmax), 
                          Type = rep(c("Median", 
                                       "max"), each = 8760))
plt_dataset$Type <- factor(plt_dataset$Type, levels = c("Median",  "max"))


ggplot(plt_dataset) +
  geom_line(aes(x=DOY, y= Price, color = Type)) +
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



plt_dataset <- data.frame(DOY = rep(time_stamps, 3),
                          Price = c(quant50, quantmax, Price[,101]), 
                          Type = rep(c("Median", 
                                       "max", "Year-101"), each = 8760))
plt_dataset$Type <- factor(plt_dataset$Type, levels = c("Median", "max", "Year-101"))


ggplot(plt_dataset) +
  geom_line(aes(x=DOY, y= Price, color = Type)) +
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

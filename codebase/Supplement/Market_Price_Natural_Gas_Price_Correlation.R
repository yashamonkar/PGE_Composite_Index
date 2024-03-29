#______________________________________________________#
###Natural Gas and Market Price Plot###



#Point towards the working directory. 
setwd("~/GitHub/PGE_Composite_Index")



#______________________________________________________________________________#
###Load the libraries###
library(dplyr)
library(ggplot2)




#______________________________________________________________________________#
###Reading the input data###
Price <- read.csv("data/no_tax/Simulated_Price_Variable_NG.csv", header = FALSE)
Yearly_gas <- read.csv("data/Yearly_gas.csv", header = FALSE)




#_________________________________________________________________________#
###generate the plot

plt_dataset <- data.frame(Price = colMeans(Price),
                          Gas = Yearly_gas$V1)
plt_dataset <- plt_dataset[-101,]


pdf("figures/supplement/Market_Price_Gas_Price_Corr.pdf", 
    height=7, width=9)
ggplot(plt_dataset) +
  geom_point(aes(x=Gas, y = Price), size = 1.75) +
  ylab("Market Price ($/MWhr)") +  
  xlab("Natural Gas Price ($/MMBtu)") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14))
dev.off()
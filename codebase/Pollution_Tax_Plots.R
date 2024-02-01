#Point towards the working directory. 
setwd("~/GitHub/PGE_Composite_Index")

#______________________________________________________________________#
#Load data and dependencies

#Library
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggpattern)

#Load the libraries
source("functions/get_revenue_comp.R")
source("functions/generic/generic_corr_plot.R")


#______________________________________________________________________#
#Reading the Input Data Files

#Revenues (Unmanaged and Adjusted)
tax_revenue <- read.csv("sims/Net_Revenue_Pollution_tax.csv", 
                        header = TRUE, sep=",")
composite_index_revenue_tax <- read.csv("sims/All_revenues_pollution_tax.csv")
no_tax_rev <- read.csv("sims/Net_Revenue_no_tax.csv", header = TRUE, sep=",")

#Hydrometerological and Market Conditions
streamflow <- read.csv("data/Streamflow.csv")
CDD <- read.csv("data/CDD.csv")
Yearly_gas <- read.csv("data/Yearly_gas.csv", header = FALSE)

#Pollution Damages
Pollution_Damages <- read.csv("sims/Prevented_Pollution_Damages.csv", 
                              header = TRUE, sep=",")



#Streamflow
sites = c('ORO_fnf', 'SHA_fnf', 'FOL_fnf', 'PAR_fnf', 'NML_fnf', 'MIL_fnf', 'PFT_fnf')
streamflow = streamflow[,sites]
streamflow = rowMeans(streamflow)

###CDD
pge_cities = c('FRESNO_T', 'SACRAMENTO_T','SAN.JOSE_T', 'SAN.FRANCISCO_T')
CDD = CDD[,pge_cities]
CDD = rowMeans(CDD)




#______________________________________________________________________________#
#Final Plots for the paper

####-----------------Changes in the Revenue--------------------------###
pdf("figures/paper/Revenue_Comparision.pdf",height=7, width=9)
get_revenue_comp(tax = tax_revenue$Net_revenue[-101], 
                 no_tax = no_tax_rev$Net_revenue[-101])
dev.off()

1000*(min(no_tax_rev$Net_revenue[-101]) - min(tax_revenue$Net_revenue[-101]))
1000*(mean(no_tax_rev$Net_revenue[-101]) - mean(tax_revenue$Net_revenue[-101]))
1000*(quantile(no_tax_rev$Net_revenue[-101], 0.05) - quantile(tax_revenue$Net_revenue[-101], 0.05))


####--------------Composite Index-Pollution Tax---------------------###
plt_dataset <- data.frame(Damages = Pollution_Damages$CAISO[-101],
                          Unmanaged = tax_revenue$Net_revenue[-101],
                          Index = composite_index_revenue_tax$Composite_index)

p1 <- ggplot(plt_dataset) +
  geom_point(aes(x=Damages, y = Unmanaged), color='blue') +
  geom_hline(aes(yintercept = mean(plt_dataset$Unmanaged)), size = 1.15) +
  geom_hline(aes(yintercept = quantile(plt_dataset$Unmanaged, 0.05)), 
             linetype = "dashed", size = 1.15) +
  geom_hline(aes(yintercept = quantile(plt_dataset$Unmanaged, 0.95)), 
             linetype = "dashed", size = 1.15) +
  xlab("Prevented Pollution Damages ($ Million)") +
  ylab("Net Revenue ($ Billion)") +
  ylim(c(11,12.7)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14))


p2 <- ggplot(plt_dataset) +
  geom_point(aes(x=Damages, y = Index), color='red') +
  geom_hline(aes(yintercept = mean(plt_dataset$Index)), size = 1.15) +
  geom_hline(aes(yintercept = quantile(plt_dataset$Index, 0.05)), 
             linetype = "dashed", size = 1.15) +
  geom_hline(aes(yintercept = quantile(plt_dataset$Index, 0.95)), 
             linetype = "dashed", size = 1.15) +
  xlab("Prevented Pollution Damages ($ Million)") +
  ylab("Net Revenue ($ Billion)") +
  ylim(c(11,12.7)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14))

pdf("figures/paper/Pollution_Tax.pdf",height=7, width=18)
plot_grid(p1,p2,
          nrow =1,
          labels = c('A', 'B'), 
          label_size = 18)
dev.off()



####--------------Prevented Pollution Damages---------------------###
plt_dataset <- data.frame(Streamflow = streamflow[-101],
                          CDD = CDD[-101],
                          NG = Yearly_gas$V1[-101],
                          Damages = Pollution_Damages$CAISO[-101], 
                          Unmanaged = tax_revenue$Net_revenue[-101])

generic_corr_plot(Plotting_Dataset = plt_dataset,
                  Field_Names = c("Streamflow", "CDD", "NG", "Prevented\nDamages", "Net\nRevenue"))



plt_dataset <- data.frame(Streamflow = streamflow[-101],
                          CDD = CDD[-101],
                          NG = Yearly_gas$V1[-101],
                          Damages = Pollution_Damages$CAISO[-101],
                          Tax = no_tax_rev$Net_revenue[-101] - tax_revenue$Net_revenue[-101], 
                          Unmanaged = tax_revenue$Net_revenue[-101])

generic_corr_plot(Plotting_Dataset = plt_dataset,
                  Field_Names = c("Streamflow", "CDD", "NG", 
                                  "Prevented\nDamages\n(CAISO)", 
                                  "\u0394 Revenue\n Tax Burden\n(PGE)" ,
                                  "Net\nRevenue"))




####--------------Composite Index-Pollution Tax---------------------###
plt_dataset <- data.frame(Damages = Pollution_Damages$CAISO[-101],
                          Unmanaged = tax_revenue$Net_revenue[-101],
                          Index = composite_index_revenue_tax$Composite_index,
                          Tax = no_tax_rev$Net_revenue[-101] - tax_revenue$Net_revenue[-101])

p1 <- ggplot(plt_dataset) +
  geom_point(aes(x=1000*Tax, y = Unmanaged), color='blue') +
  geom_hline(aes(yintercept = mean(plt_dataset$Unmanaged)), size = 1.15) +
  geom_hline(aes(yintercept = quantile(plt_dataset$Unmanaged, 0.05)), 
             linetype = "dashed", size = 1.15) +
  geom_hline(aes(yintercept = quantile(plt_dataset$Unmanaged, 0.95)), 
             linetype = "dashed", size = 1.15) +
  xlab("Tax Burden ($ Million)") +
  ylab("Net Revenue ($ Billion)") +
  ylim(c(11,12.7)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14))


p2 <- ggplot(plt_dataset) +
  geom_point(aes(x=1000*Tax, y = Index), color='red') +
  geom_hline(aes(yintercept = mean(plt_dataset$Index)), size = 1.15) +
  geom_hline(aes(yintercept = quantile(plt_dataset$Index, 0.05)), 
             linetype = "dashed", size = 1.15) +
  geom_hline(aes(yintercept = quantile(plt_dataset$Index, 0.95)), 
             linetype = "dashed", size = 1.15) +
  xlab("Tax Burden ($ Million)") +
  ylab("Net Revenue ($ Billion)") +
  ylim(c(11,12.7)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14))

pdf("figures/paper/Pollution_Tax.pdf",height=7, width=14)
plot_grid(p1,p2,
          nrow =1,
          labels = c('A', 'B'), 
          label_size = 18)
dev.off()
#_________________________________________________________________#



plt_dataset <- data.frame(Damages = Pollution_Damages$CAISO[-101],
                          Unmanaged = tax_revenue$Net_revenue[-101],
                          Index = composite_index_revenue_tax$Composite_index,
                          Tax = no_tax_rev$Net_revenue[-101] - tax_revenue$Net_revenue[-101],
                          Streamflow = streamflow[-101],
                          CDD = CDD[-101],
                          NG = Yearly_gas$V1[-101])


ggplot(plt_dataset) +
  geom_point(aes(x=Tax, y = Unmanaged, color=CDD)) +
  geom_hline(aes(yintercept = mean(plt_dataset$Unmanaged)), size = 1.15) +
  geom_hline(aes(yintercept = quantile(plt_dataset$Unmanaged, 0.05)), 
             linetype = "dashed", size = 1.15) +
  geom_hline(aes(yintercept = quantile(plt_dataset$Unmanaged, 0.95)), 
             linetype = "dashed", size = 1.15) +
  scale_color_gradient2(midpoint=median(plt_dataset$CDD),
                        low="blue", mid="green",high="red") +
  xlab("Tax Burden ($ Million)") +
  ylab("Net Revenue ($ Billion)") +
  ylim(c(11,12.7)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14))


ggplot(plt_dataset) +
  geom_point(aes(x=Tax, y = Unmanaged, color=Streamflow)) +
  geom_hline(aes(yintercept = mean(plt_dataset$Unmanaged)), size = 1.15) +
  geom_hline(aes(yintercept = quantile(plt_dataset$Unmanaged, 0.05)), 
             linetype = "dashed", size = 1.15) +
  geom_hline(aes(yintercept = quantile(plt_dataset$Unmanaged, 0.95)), 
             linetype = "dashed", size = 1.15) +
  scale_color_gradient2(midpoint=median(plt_dataset$Streamflow),
                        low="blue", mid="green",high="red") +
  xlab("Tax Burden ($ Million)") +
  ylab("Net Revenue ($ Billion)") +
  ylim(c(11,12.7)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14))






#____________________________________________________________________#
plt_dataset <- data.frame(Damages = Pollution_Damages$CAISO[-101], 
                          Streamflow = streamflow[-101],
                          CDD = CDD[-101],
                          NG = Yearly_gas$V1[-101],
                          Tax_Rev = tax_revenue$Net_revenue[-101],
                          No_Tax_Rev = no_tax_rev$Net_revenue[-101],
                          Tax = no_tax_rev$Net_revenue[-101] - tax_revenue$Net_revenue[-101])


ggplot(plt_dataset) +
  geom_point(aes(y=Tax_Rev, x = No_Tax_Rev, color = NG)) +
  ylab("Pollution Tax Revenue ($ Billions)") +  
  xlab("No Tax Revenue ($ Billions)") +
  scale_color_gradient2(midpoint=mean(plt_dataset$NG),
                        low="blue", mid="yellow",high="red") +
  geom_abline(intercept = 0, slope = 1, color ='black') +
  xlim(c(11,13))+
  ylim(c(11,13))+
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14))



ggplot(plt_dataset) +
  geom_point(aes(x=Tax_Rev, y = Tax)) +
  ylab("Pollution Tax ($ Billions)") +  
  xlab("Tax Revenue ($ Billions)") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14))



ggplot(plt_dataset) +
  geom_point(aes(x=Streamflow, y = Tax)) +
  ylab("Pollution Tax ($ Billions)") +  
  xlab("Streamflow") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14))



ggplot(plt_dataset) +
  geom_point(aes(x=CDD, y = Tax)) +
  ylab("Pollution Tax ($ Billions)") +  
  xlab("CDD") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14))




ggplot(plt_dataset) +
  geom_point(aes(x=NG, y = Tax)) +
  ylab("Pollution Tax ($ Billions)") +  
  xlab("NG") +
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14))








plt_dataset <- data.frame(Damages = Pollution_Damages$CAISO[-101], 
                          Streamflow = streamflow[-101],
                          CDD = CDD[-101],
                          Payouts =  tax_revenue$Net_revenue[-101] - composite_index_revenue_tax$Composite_index)
plt_dataset$Unmanaged = tax_revenue$Net_revenue[-101]
plt_dataset$Index = composite_index_revenue_tax$Composite_index

payouts_dataset <- plt_dataset[plt_dataset$Payouts < median(plt_dataset$Payouts),]



ggplot(plt_dataset) +
  geom_point(aes(x=Streamflow, y = Damages, color = CDD), size = 1.75) +
  scale_color_gradient2(midpoint=mean(plt_dataset$CDD),
                        low="blue", mid="yellow",high="red") +
  ylab("Prevented Pollution Damages ($ Millions)") +  
  xlab("Annual Average Streamflow (cfs)") +
  labs(color = "Annual CDD") + 
  xlim(c(0,32000))+
  ylim(c(600,750))+
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)) +
  guides(color = guide_colourbar(title.hjust = 0.5,
                                 barwidth = unit(4, "in"), barheight = unit(0.3, "in")))

ggplot(plt_dataset) +
  geom_point(aes(x=Streamflow, y = Damages, color = CDD), size = 1.75) +
  geom_point(payouts_dataset, mapping = aes(x=Streamflow, y = Damages), size = 1.75) +
  scale_color_gradient2(midpoint=mean(plt_dataset$CDD),
                        low="blue", mid="yellow",high="red") +
  ylab("Prevented Pollution Damages ($ Millions)") +  
  xlab("Annual Average Streamflow (cfs)") +
  labs(color = "Annual CDD") + 
  xlim(c(0,32000))+
  ylim(c(600,750))+
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)) +
  guides(color = guide_colourbar(title.hjust = 0.5,
                                 barwidth = unit(4, "in"), barheight = unit(0.3, "in")))


ggplot(plt_dataset) +
  geom_point(aes(x=CDD, y = Unmanaged, color = Damages), size = 1.75) +
  scale_color_gradient2(midpoint=mean(plt_dataset$Damages),
                        low="blue", mid="yellow",high="red") +
  xlab("CDD ") +  
  ylab("Unmanaged Revenue ($B)") +
  labs(color = "Prevented Damages ($ M)") + 
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)) +
  guides(color = guide_colourbar(title.hjust = 0.5,
                                 barwidth = unit(4, "in"), barheight = unit(0.3, "in")))




ggplot(plt_dataset) +
  geom_point(aes(x=Streamflow, y = Unmanaged, color = Damages), size = 1.75) +
  scale_color_gradient2(midpoint=mean(plt_dataset$Damages),
                        low="blue", mid="yellow",high="red") +
  xlab("Streamflow ") +  
  ylab("Unmanaged Revenue ($B)") +
  labs(color = "Prevented Damages ($ M)") + 
  theme_bw() +
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 12),
        axis.text.x = element_text(size = rel(1.5)),  
        axis.text.y = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14)) +
  guides(color = guide_colourbar(title.hjust = 0.5,
                                 barwidth = unit(4, "in"), barheight = unit(0.3, "in")))


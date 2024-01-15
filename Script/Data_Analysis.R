 # R version 4.2.2 (2022-10-31 ucrt) 

# All the packages used in this Rscript are listed below

install.packages("ggplot2")
install.packages("dplyr")
install.packages("agricolae")
install.packages("egg")
install.packages("tidyverse")
install.packages("viridis")

library(ggplot2)
library(dplyr)
library(agricolae)
library(egg)
library(tidyverse)
library(viridis)

# All the dataset used in this Rscript can be found in the "Data" subfolder 

setwd(dir="C:/Users/Nans/Desktop/Data_PVC_FI_Fragmentation/Data")

################################## Hypothesis 1 : Abundance of formed MPs depending on the FI treatments

Abundance<-read.csv("H1_abundance.csv", header = T, sep = ";")

# Data representation

UVC_abundance_plot<-ggplot(data=Abundance, aes(x=Treatment, y=MPs3, fill=Treatment))+
  ggtitle("Mean abundance of formed MPs <1mm") + 
  xlab("Flow Intermittence treatment") + ylab("Average abundance of formed MPs")  + geom_boxplot()+theme_article()

UVC_abundance_plot<-UVC_abundance_plot+scale_fill_manual(values=c("#f2f0f7", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba","#6a51a3","#54278f","#9ecae1","#6baed6"))
UVC_abundance_plot

# Data analysis 

shapiro.test(Abundance$MPs3)
bartlett.test(MPs3~Treatment, data=Abundance)
kruskal.test(MPs3~Treatment, data=Abundance)
kruskal(Abundance$MPs3, Abundance$Treatment, group=TRUE)$groups 

################################## Hypothesis 2 : Mass loss of the PVC film depending on the FI treatments

Mass_loss<-read.csv("H2_mass_loss.csv", header = T, sep = ";")

# Data representation

UVC_Mass_loss_plot<-ggplot(data=Mass_loss, aes(x=Treatment, y=Fraction2, fill=Treatment))+ theme(plot.title = element_text(hjust = 2)) +
  ggtitle("Proportion of the estimated initial disk mass present in the 20µm sieve") + 
  xlab("Flow Intermittence treatment") + ylab("Proportion of the estimated mass")+ theme_article() +
  geom_boxplot(notch = FALSE)

UVC_Mass_loss_plot<-UVC_Mass_loss_plot+scale_fill_manual(values=c("#f2f0f7", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba","#6a51a3","#54278f","#9ecae1","#6baed6","#4292c6"))+
  theme(plot.title = element_text(hjust = 0.5))

UVC_Mass_loss_plot

# Data analysis

shapiro.test(Mass_loss$Fraction2)
bartlett.test(Fraction2~Treatment, data=Mass_loss)
kruskal.test(Fraction2~Treatment, data=Mass_loss)
kruskal(Mass_loss$Fraction2,Mass_loss$Treatment, group=TRUE)$groups


################################## Hypothesis 3 : Average size of the form MPs depending on the FI treatments

Size<-read.csv("H3_size.csv", header = T, sep = ";")

# Data representation

UVC_size_plot<-ggplot(data=Size, aes(x=Treatment, y=Size, fill=Treatment)) + geom_boxplot()+ theme(plot.title = element_text(hjust = 2)) +
  ggtitle("Mean size of the formed MPs depending on the FI treatment") + 
  xlab("Flow Intermittence treatment") + ylab("Mean size of formed MPs(µm)")+ theme_article() +
  geom_boxplot(notch = FALSE)
UVC_size_plot<-UVC_size_plot+scale_fill_manual(values=c("#f2f0f7", "#dadaeb", "#bcbddc", "#9e9ac8", "#807dba","#6a51a3","#54278f","#9ecae1","#6baed6","#4292c6"))+
  theme(plot.title = element_text(hjust = 0.5))

UVC_size_plot

# Data analysis

shapiro.test(Size$Size)
bartlett.test(Size~Treatment, data=Size)
kruskal.test(Size~Treatment, data = Size)
kruskal(Size$Size, Size$Treatment, group=TRUE)$groups

################################## UV-C and Sunlight exposure comparison

SUN<-read.csv("SUN_abundance_size.csv", header = T, sep = ";")
SUN$Treatment <- fct_relevel(SUN$Treatment, c("C-UV","1W", "2W", "3W","4W","5W","6W", "7W", "8W", "9W", "10W", "11W","12W","6D0W"))

# Data representation of the average abundance of formed MPs

SUN_abundance_plot<-ggplot(data=SUN, aes(x=Treatment, y=MPs3, fill=Treatment))+  theme(plot.title = element_text(hjust = 2)) +
  ggtitle("Mean abundance of the formed MPs depending on sunlight exposure duration") + 
  xlab("Number of exposure week") + ylab("Average abundance of formed MPs per PVC disk")+ theme_article() +
  geom_boxplot(notch = FALSE) + theme_article()

SUN_abundance_plot<- SUN_abundance_plot + scale_fill_manual(values=c("#9ecae1", "#fff5eb", "#fee6ce", "#fdd0a2", "#fdae6b", "#fd8d3c","#f16913","#d94801","#e31a1c","#bd0026","#a63603","#7f2704","#7f0000","#756bb1"))
SUN_abundance_plot

# Data analysis of the average abundance of formed MPs

shapiro.test(SUN$MPs3)
bartlett.test(MPs3~Treatment, data=SUN)
kruskal.test(MPs3 ~ Treatment, data = SUN)
kruskal(SUN$MPs3, SUN$Treatment, group=TRUE)$groups

# Data representation of the average size of formed MPs

SUN_size_plot<-ggplot(data=SUN, aes(x=Treatment, y=Size, fill=Treatment))+  theme(plot.title = element_text(hjust = 2)) +
  ggtitle("Mean size of the formed MPs depending on sunlight exposure duration") + 
  xlab("Number of exposure week") + ylab("Mean size of the formed MPs (µm)")+ theme_article() +
  geom_boxplot(notch = FALSE) + theme_article ()

SUN_size_plot<- SUN_size_plot + scale_fill_manual(values=c("#9ecae1", "#fff5eb", "#fee6ce", "#fdd0a2", "#fdae6b", "#fd8d3c","#f16913","#d94801","#e31a1c","#bd0026","#a63603","#7f2704","#7f0000","#756bb1"))
SUN_size_plot

# Data analysis of the average size of formed MPs

shapiro.test(SUN$Size)
bartlett.test(Size~Treatment, data=SUN)
kruskal.test(Size~Treatment, data = SUN)
kruskal(SUN$Size, SUN$Treatment, group=TRUE)$groups

################################## Graphical representation of the PFI depending on the models output

PFIs<-read.csv("PFI.csv", header = T, sep = ";")

PFI_plot <- ggplot(PFIs, aes(x=" ", y=PFI))+
  geom_point(aes(color=model, size=model))+
  theme_article()+
  facet_grid(. ~ period)+
  ylab("Plastic Fragmentation Index")+
  xlab("")+
  ylim(415,460)+
  scale_color_manual(values = c("black",viridis(5)))+
  scale_size_manual(values = c(5,3,3,3,3,3))+
  theme(legend.title=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
PFI_plot


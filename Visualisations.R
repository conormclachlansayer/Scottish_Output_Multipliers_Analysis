library(tidyverse)
library(readxl)
library(ggrepel)
library(RColorBrewer)
library(wesanderson)
library(ggsci)
library(BBmisc)
library(scales)
library(directlabels)

### Need to first open and run Fiscal Multipliers R Script (for data)
##############################################################################################################################################################
##############################################################################################################################################################
######################################################################## VISUALISATIONS ######################################################################
##############################################################################################################################################################
##############################################################################################################################################################

# ###Graphing
# top25 <- sigchangelong$Industry[perc75] 
# top10 <- sigchangelong$Industry[perc90]
# top5 <- sigchangelong$Industry[perc95]
# topbot6 <- sigchangelong$Industry[topbot33]
#   
# subset25 <- filter(timemultiplierslong, Industry %in% top25)
# subset10 <- filter(timemultiplierslong, Industry %in% top10)
# subset5 <- filter(timemultiplierslong, Industry %in% top5)
# subsettop6 <- filter(timemultiplierslong, Industry %in% topbot6)

##############################################################################################################################################################
############################################################# Creating graph of Multipliers in 2016 ##########################################################

#Graph 1 in Presentation
################################################ How output multipliers varied across Scottish Industry Multipliers (with Order as x axis and Output.2016 as size)
# Graphing Multiplier Sizes (with Order as x axis and Industry Output (2016) as size)
x.axis <- c(98,91,81,71,61,51,41,31,21,11,1) 
ggplot(multipliers_2016, aes(x=Order, y=Output2Mult, size= Output, col = IndustryType))+   
  geom_point(alpha=0.3)+
  #ggtitle("Scottish Industry Multipliers")+
  labs(x="<------------------------ Lowest Multipliers, Highest Multipliers ------------------------>",y="Size of Output Multiplier", 
       #subtitle="Percentage impact on y axis, nominal impact on economy represented by size of bubble",
       size = "Total Industry Output in 2016", col = "Industry Classification")+
  guides(size = guide_legend(order=1),
         col = guide_legend(order=2))+
  scale_x_reverse(limits=c(98,1), breaks=(x.axis))+
  scale_y_continuous(limits=c(1,2), breaks=seq(1,2,0.1))+
  scale_size_continuous(labels=dollar_format(prefix="�", suffix="m"), range = c(1, 10))+
  #scale_color_gradient2(midpoint=mean(Multipliers_2016$Output.2016), low="#6f2cdb", mid="#009af9", high="#00b594", space ="Lab") +
  #scale_color_continuous("#6f2cdb")+
  geom_text_repel(aes(label = IndustryName.Every4), force=1, size=3, arrow = arrow(length = unit(0.00001, "npc")), box.padding = 1) +
  theme(legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "cm"), 
        axis.line = element_line(size=1, colour = "black"),
        plot.title = element_text(size = 20, face = "bold")) +
  theme_classic() +
  scale_color_manual(values=c("#333333", "#CC3300","#000066"))


##############################################################################################################################################################
############################################ Graphing Indirect & Induced Effects across Industries ############################################################

#Graph 2 in Presentation
#################################################################################### Relationship between Output multipliers & Indirect effects
# Graphing Multiplier Sizes (with Order as x axis and Industry Output (2016) as size)
x.axis <- c(98,91,81,71,61,51,41,31,21,11,1) 
ggplot(multipliers_2016, aes(x=Order, y=IndirectEffect, size=Output, col=IndustryType))+   
  geom_point(alpha=0.3)+
  #ggtitle("Scottish Industry Multipliers")+
  labs(x="<------------------------ Lowest Multipliers, Highest Multipliers ------------------------>",y="Size of Indirect Effect", 
       #subtitle="Percentage impact on y axis, nominal impact on economy represented by size of bubble",
       size = "Total Industry Output in 2016", col = "Industry Classification")+
  guides(size = guide_legend(order=1),
         fill = guide_legend(order=2))+
  scale_x_reverse(limits=c(98,1), breaks=(x.axis))+
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1))+
  scale_size_continuous(labels=dollar_format(prefix="£", suffix="m"), range = c(1, 10))+
  #scale_color_gradient2(midpoint=mean(Multipliers_2016$Output.2016), low="#6f2cdb", mid="#009af9", high="#00b594", space ="Lab") +
  #scale_color_continuous("#6f2cdb")+
  geom_text_repel(aes(label = IndustryName.Significant.Indirect), force=2, size=3, arrow = arrow(length = unit(0.00001, "npc")), box.padding = 1) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "cm"), 
        axis.line = element_line(size=1, colour = "black"),
        plot.title = element_text(size = 20, face = "bold")) +
  theme_classic()+
  scale_color_manual(values=c("#333333", "#CC3300","#000066"))

#Graph 3 in Presentation
#################################################################################### Relationship between Indirect effects & domestic usage

ggplot(multipliers_2016, aes(x=Domestic_Proportion, y=IndirectEffect, size=Output, col=IndustryType))+   
  geom_point(alpha=0.3)+
  #ggtitle("Scottish Industry Multipliers")+
  labs(x="Proportion of Inputs sourced Domestically",y="Size of Indirect Effect", 
       #subtitle="Percentage impact on y axis, nominal impact on economy represented by size of bubble",
       size = "Total Industry Output in 2016", col = "Industry Classification")+
  guides(size = guide_legend(order=1),
         fill = guide_legend(order=2))+
  scale_x_continuous(labels=scales::percent, limits=c(0.3,0.81), breaks=seq(0.3, 0.8, 0.1)) + 
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1))+
  scale_size_continuous(labels=dollar_format(prefix="?", suffix="m"), range = c(1, 10))+
  #scale_color_gradient2(midpoint=mean(Multipliers_2016$Output.2016), low="#6f2cdb", mid="#009af9", high="#00b594", space ="Lab") +
  #scale_color_continuous("#6f2cdb")+
  geom_text_repel(aes(label = IndustryName.Significant.Indirect), force=16, size=3, arrow = arrow(length = unit(0.00001, "npc")), box.padding = 1) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "cm"), 
        axis.line = element_line(size=1, colour = "black"),
        plot.title = element_text(size = 20, face = "bold")) +
  theme_classic()+
  scale_color_manual(values=c("#333333", "#CC3300","#000066"))


#Graph 3b in Presentation
############################################################# Relationship between Indirect effects & Proportion of Domestic Supply used by others

# ggplot(multipliers_2016, aes(x=1-Prop_Internal_Use, y=IndirectEffect, size=IndustryOutput, col=IndustryType))+   
#   geom_point(alpha=0.3)+
#   #ggtitle("Scottish Industry Multipliers")+
#   labs(x="Proportion of Domestic Supply used by others",y="Size of Indirect Effect", 
#        #subtitle="Percentage impact on y axis, nominal impact on economy represented by size of bubble",
#        size = "Total Industry Output in 2016")+
#   guides(size = guide_legend(order=1),
#          fill = guide_legend(order=2))+
#   scale_x_continuous(labels=scales::percent, limits=c(0.2,1), breaks=seq(0.2,1, 0.1)) + 
#   scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1))+
#   scale_size_continuous(labels=dollar_format(prefix="£", suffix="m"), range = c(1, 10))+
#   #scale_color_gradient2(midpoint=mean(Multipliers_2016$Output.2016), low="#6f2cdb", mid="#009af9", high="#00b594", space ="Lab") +
#   #scale_color_continuous("#6f2cdb")+
#   geom_text_repel(aes(label = IndustryName.Significant.Indirect), force=16, size=3, arrow = arrow(length = unit(0.00001, "npc")), box.padding = 1) +
#   theme(legend.position = "bottom", legend.direction = "horizontal",
#         legend.box = "horizontal",
#         legend.key.size = unit(0.5, "cm"), 
#         axis.line = element_line(size=1, colour = "black"),
#         plot.title = element_text(size = 20, face = "bold")) +
#   theme_classic()+
#   scale_color_manual(values=c("#333333", "#CC3300","#000066"))


#Graph 4 in Presentation
#################################################################################### Relationship between Output Multipliers and Induced effects
# Graphing Multiplier Sizes (with Order as x axis and Output.2016 as size)
x.axis <- c(98,91,81,71,61,51,41,31,21,11,1) 
ggplot(multipliers_2016, aes(x=Order, y=InducedEffect, size=Output, col= IndustryType))+   
  geom_point(alpha=0.3)+
  #ggtitle("Scottish Industry Multipliers")+
  labs(x="<------------------------ Lowest Multipliers, Highest Multipliers ------------------------>",y="Size of Induced Effect", 
       #subtitle="Percentage impact on y axis, nominal impact on economy represented by size of bubble",
       size = "Total Industry Output in 2016", col = "Industry Classification")+
  guides(size = guide_legend(order=1),
         fill = guide_legend(order=2))+
  scale_x_reverse(limits=c(98,1), breaks=(x.axis))+
  scale_y_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.1))+
  scale_size_continuous(labels=dollar_format(prefix="£", suffix="m"), range = c(1, 10))+
  #scale_color_gradient2(midpoint=mean(Multipliers_2016$Output.2016), low="#6f2cdb", mid="#009af9", high="#00b594", space ="Lab") +
  #scale_color_continuous("#6f2cdb")+
  geom_text_repel(aes(label = IndustryName.Significant.Induced), force=17, size=3, arrow = arrow(length = unit(0.00001, "npc")), box.padding = 1) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "cm"), 
        axis.line = element_line(size=1, colour = "black"),
        plot.title = element_text(size = 20, face = "bold")) +
  theme_classic()+
  scale_color_manual(values=c("#333333", "#CC3300","#000066"))

#Graph 5 in Presentation
#################################################################################### Relationship between Induced effects & Employee Compensation
ggplot(multipliers_2016, aes(x=EmployeeIntensity, y=InducedEffect, size=Output, col= IndustryType))+   
  geom_point(alpha=0.3)+
  #ggtitle("Scottish Industry Multipliers")+
  labs(x="Proportion of GVA attributed to Employee Compensation",y="Size of Induced Effect", 
       #subtitle="Percentage impact on y axis, nominal impact on economy represented by size of bubble",
       size = "Total Industry Output in 2016", col = "Industry Classification")+
  guides(size = guide_legend(order=1),
         fill = guide_legend(order=2))+
  scale_x_continuous(labels=scales::percent, limits=c(0,1), breaks=seq(0, 1, 0.1)) + 
  scale_y_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.1))+
  scale_size_continuous(labels=dollar_format(prefix="?", suffix="m"), range = c(1, 10))+
  #scale_color_gradient2(midpoint=mean(Multipliers_2016$Output.2016), low="#6f2cdb", mid="#009af9", high="#00b594", space ="Lab") +
  #scale_color_continuous("#6f2cdb")+
  geom_text_repel(aes(label = IndustryName.Significant.Induced), force=10, size=3, arrow = arrow(length = unit(0.00001, "npc")), box.padding = 1) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "cm"), 
        axis.line = element_line(size=1, colour = "black"),
        plot.title = element_text(size = 20, face = "bold")) +
  theme_classic()+
  scale_color_manual(values=c("#333333", "#CC3300","#000066"))



##############################################################################################################################################################
#################################################################### OVER TIME ###############################################################################
##############################################################################################################################################################


#Graph 6 in Presentation
########################################################################### All Output Multipliers over Time [Select 9 industries] 
ggplot(data = multipliers_select10, aes(x=Year, y=Output2Mult))+   
  geom_line(data = multipliers_notselect10, size=0.4, alpha=0.1, show.legend = FALSE, aes(x=Year, y=Output2Mult, group = Industry))+
  geom_line(data = multipliers_select10, size=1.2, alpha=1, show.legend = FALSE, aes(x=Year, y=Output2Mult, colour=Industry, group = Industry))+
  #ggtitle("The relationship between Multipliers & Domestic Usage")+
  labs(x="Year",y="Output Multiplier (Direct + Indirect + Induced Effect)") + 
  #subtitle="Percentage impact on y axis, nominal impact on economy represented by size of bubble",
  #fill = "Type of Industry", size = "Output in 2016")+
  scale_x_continuous(limits=c(1998,2021), breaks=seq(1998,2016,2)) + 
  scale_y_continuous(limits=c(1,2.6), breaks=seq(1,2.6,0.2))+
  #scale_colour_distiller(palette="RdBu") +
  #scale_color_manual(values=wes_palette("Cavalcanti1"))+
  scale_color_jco()+
  theme(legend.position = "right", legend.direction = "vertical",
        legend.box = "vertical",
        legend.key.size = unit(0.5, "cm"), 
        axis.line = element_line(size=1, colour = "black"),
        plot.title = element_text(size = 20, face = "bold")) +
  theme_classic()+
  geom_dl(aes(label=Industry, color=Industry), method = list(dl.trans(x = x + .1), "last.bumpup"))


#Graph 7 in Presentation
########################################################################### All Industry Domestic Usage over time [Select 9 industries] 

ggplot(data = multipliers_select10, aes(x=Year, y=Domestic_Proportion))+   
  geom_line(data = multipliers_notselect10, size=0.4, alpha=0.1, show.legend = FALSE, aes(x=Year, y=Domestic_Proportion, group = Industry))+
  geom_line(data = multipliers_select10, size=1.2, alpha=1, show.legend = FALSE, aes(x=Year, y=Domestic_Proportion, colour=Industry, group = Industry))+
  #ggtitle("The relationship between Multipliers & Domestic Usage")+
  labs(x="Year",y="Proportion of Inputs sourced Domestically") + 
  #subtitle="Percentage impact on y axis, nominal impact on economy represented by size of bubble",
  #fill = "Type of Industry", size = "Output in 2016")+
  scale_x_continuous(limits=c(1998,2021), breaks=seq(1998,2016,2)) + 
  scale_y_continuous(limits=c(0.19,0.9), breaks=seq(0.2,0.9,0.1))+
  #scale_colour_distiller(palette="RdBu") +
  #scale_color_manual(values=wes_palette("Cavalcanti1"))+
  scale_color_jco()+
  theme(legend.position = "right", legend.direction = "vertical",
        legend.box = "vertical",
        legend.key.size = unit(0.5, "cm"), 
        axis.line = element_line(size=1, colour = "black"),
        plot.title = element_text(size = 20, face = "bold")) +
  theme_classic()+
  geom_dl(aes(label=Industry, color=Industry), method = list(dl.trans(x = x + .1), "last.bumpup"))

#Graph 8 in Presentation
########################################################################### All Industry Employee Compensation Usage over time [Select 9 industries] 
temp_multipliers_notselect10 <- multipliers_notselect10 %>% select(Year, Industry, EmployeeIntensity) %>% spread(key= Year, EmployeeIntensity)
row = 1
while(row < nrow(temp_multipliers_notselect10)){
  print(min(temp_multipliers_notselect10[row, -1]) < 0.15 | max(temp_multipliers_notselect10[row, -1]) > 1.15)
  if(min(temp_multipliers_notselect10[row, -1]) < 0.15 | max(temp_multipliers_notselect10[row, -1]) > 1.15){
    temp_multipliers_notselect10 <- temp_multipliers_notselect10[-row,]
  }
  
  row = row + 1
}
temp_multipliers_notselect10 <- temp_multipliers_notselect10 %>% slice(seq(1, nrow(temp_multipliers_notselect10), 2)) %>% gather(key = Year, value = "EmployeeIntensity", -Industry)
temp_multipliers_notselect10$Year <- as.numeric(temp_multipliers_notselect10$Year)

ggplot(data = multipliers_select10, aes(x=Year, y=EmployeeIntensity))+   
  geom_line(data = temp_multipliers_notselect10, size=0.4, alpha=0.1, show.legend = FALSE, aes(x=Year, y=EmployeeIntensity, group = Industry))+
  geom_line(data = multipliers_select10, size=1.2, alpha=1, show.legend = FALSE, aes(x=Year, y=EmployeeIntensity, colour=Industry, group = Industry))+
  #ggtitle("The relationship between Multipliers & Domestic Usage")+
  labs(x="Year",y="Proportion of GVA attributed to Employee Compensation") + 
  #subtitle="Percentage impact on y axis, nominal impact on economy represented by size of bubble",
  #fill = "Type of Industry", size = "Output in 2016")+
  scale_x_continuous(limits=c(1998,2021), breaks=seq(1998,2016,2)) + 
  scale_y_continuous(limits=c(0.14,1.15), breaks=seq(0.15,1.15,0.2))+
  #scale_colour_distiller(palette="RdBu") +
  #scale_color_manual(values=wes_palette("Cavalcanti1"))+
  scale_color_jco()+
  theme(legend.position = "right", legend.direction = "vertical",
        legend.box = "vertical",
        legend.key.size = unit(0.5, "cm"), 
        axis.line = element_line(size=1, colour = "black"),
        plot.title = element_text(size = 20, face = "bold")) +
  theme_classic()+
  geom_dl(aes(label=Industry, color=Industry), method = list(dl.trans(x = x + .1), "last.bumpup"))

remove(temp_multipliers_notselect10)

##############################################################################################################################################################
##############################################################################################################################################################
################################################################# LITERATURE REVIEW PRESENTATION #############################################################
##############################################################################################################################################################
##############################################################################################################################################################

##############################################################################################################################################################
################################################################# Graphing Business Cycle ####################################################################

ggplot()+   
  geom_point(data = multiplier.determinants.businesscycle, alpha=0.7, size=5, aes(x=Time,y=Multiplier_Estimate, col=Multiplier_Type))+
  #scale_color_jco()+
  labs(x="Point in Business Cycle",y="Fiscal Multiplier Estimate", col="Multiplier Type") + 
  #subtitle="Percentage impact on y axis, nominal impact on economy represented by size of bubble", col = "Type of Industry", size = "Output in 2016")+
  scale_y_continuous(limits=c(-0.1,3.5), breaks=c(0,0.5,1,1.5,2,2.5,3,3.5))+
  scale_color_manual(values=c("#6699FF", "#FFCC00","#666666"))+
  #geom_text_repel(aes(label = type2_multipliers_yearly_largest_75_long$Industry), force=3, size=3, arrow = arrow(length = unit(0.00001, "npc")), box.padding = 1) +
  theme(legend.position = "right", legend.direction = "vertical",
        legend.box = "vertical",
        legend.key.size = unit(0.5, "cm"), 
        axis.line = element_line(size=1, colour = "black"),
        plot.title = element_text(size = 20, face = "bold")) +
  theme_classic()


##############################################################################################################################################################
################################################################# Graphing Monetary Cycle ####################################################################

ggplot()+   
  geom_point(data = multiplier.determinants.monetarycycle, alpha=0.7, size=5, aes(x=Time,y=Multiplier_Estimate, col=Multiplier_Type))+
  scale_color_manual(values=c("#666666"))+
  labs(x="Point in Monetary Cycle",y="Fiscal Multiplier Estimate", col="Multiplier Type") + 
  #subtitle="Percentage impact on y axis, nominal impact on economy represented by size of bubble", col = "Type of Industry", size = "Output in 2016")+
  scale_y_continuous(limits=c(0.5,4), breaks=c(0.5,1,1.5,2,2.5,3,3.5,4))+
  #geom_text_repel(aes(label = type2_multipliers_yearly_largest_75_long$Industry), force=3, size=3, arrow = arrow(length = unit(0.00001, "npc")), box.padding = 1) +
  theme(legend.position = "right", legend.direction = "vertical",
        legend.box = "vertical",
        legend.key.size = unit(0.5, "cm"), 
        axis.line = element_line(size=1, colour = "black"),
        plot.title = element_text(size = 20, face = "bold")) +
  theme_classic()
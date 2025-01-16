#Plots_BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS
library("readxl")
library("ggplot2")
library("ggpubr")
library("ggExtra")
library(tidyr)

#BLUP_Vr_Oscheius_mod3_scaled_dataScale_SSSS  from O2-Vr_Oscheius_BLUP_SSSS.R
#BLUP_Vr_Caenorhabditis_mod3_scaled_dataScale from C2-Vr_Caenorhabditis_BLUP.R


BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale <- rbind.data.frame(BLUP_Vr_Caenorhabditis_mod3_scaled_dataScale,BLUP_Vr_Oscheius_mod3_scaled_dataScale_SSSS)
View(BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale)
BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale$Species <- gsub("Species_phylo\\.", "", BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale$Species)

write_xlsx(BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale, "BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale.xlsx")


factor(BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale$Genus)

Plot_BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_P3pP4p <- ggplot(BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale, aes(x = P3p_BLUP, y = P4p_BLUP)) + 
  geom_point(aes(colour = Genus), shape=19, alpha=0.9)+  scale_colour_manual(values = c("salmon","royalblue"))+  
  xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))


pdf("Plot_BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_P3pP4p_boxplot.pdf")
ggMarginal(Plot_BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
dev.off() 

#BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_all_Pnp  ----                                                  


#reshape dataset

BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_2 <- BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale %>%
  gather(key= "Pn.p", value="Trait_Frequency", 2:7)
View(BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_2)  
BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_2$Pn.p <- gsub("_BLUP", "", BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_2$Pn.p)
BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_2 <- BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_2[,-3]

BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_2$Pn.p_fate <- c(rep("SS", 53), rep("SSSS",24) , rep("SS", 53) , rep("SSSS",24), rep("wt",231),rep("SS", 53), rep("SSSS",24))
BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_2$data_type <- rep("BLUP",462)

write_xlsx(BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_2, "BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_2.xlsx")

head(BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_2)


Plot_BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_all_Pnp <- ggplot(BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_2, aes( x= Pn.p ,y= Trait_Frequency)) +
  geom_point(aes(colour = Genus), size=0.7) + scale_colour_manual(values = c("salmon","royalblue"))+  
  facet_grid(rows =vars(Genus))+ theme_bw() +
  labs(title = "BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_all_Pnp", y ="Trait frequency") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 20)), legend.position = "bottom",axis.text.x = element_text(size = 10),axis.title = element_text(size = 15))


pdf("Plot_BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_all_Pnp.pdf")
Plot_BLUP_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_all_Pnp
dev.off()



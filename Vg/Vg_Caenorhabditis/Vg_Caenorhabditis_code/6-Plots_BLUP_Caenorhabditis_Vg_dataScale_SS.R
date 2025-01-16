#Plots_BLUP_Caenorhabditis_Vg_dataScale_SS
library("readxl")
library("ggplot2")
library("ggpubr")
library("ggExtra")


#BLUP_Caenorhabditis_Vg_dataScale_SS from 2-BLUP_Caenorhabditis_Vg_SS.R

---- #BLUP_Caenorhabditis----

table(BLUP_Caenorhabditis_Vg_dataScale_SS$Treatment)
names(BLUP_Caenorhabditis_Vg_dataScale_SS)

BLUP_Caenorhabditis_Vg_dataScale_SS_WILD <- subset(BLUP_Caenorhabditis_Vg_dataScale_SS, Treatment =="WILD")
table(BLUP_Caenorhabditis_Vg_dataScale_SS_WILD$Treatment)
table(BLUP_Caenorhabditis_Vg_dataScale_SS_WILD$Species)

BLUP_Caenorhabditis_Vg_dataScale_SS_WILD$Species <- factor(BLUP_Caenorhabditis_Vg_dataScale_SS_WILD$Species, levels = c("C.elegans", "C.briggsae") )
factor(BLUP_Caenorhabditis_Vg_dataScale_SS_WILD$Species)

Plot_BLUP_Caenorhabditis_Vg_dataScale_SS_P3pP4p  <- ggplot(BLUP_Caenorhabditis_Vg_dataScale_SS_WILD, aes(x = P3p_SS_BLUP, y = P4p_SS_BLUP)) + 
  geom_point(aes(colour = Species), shape=19) + scale_colour_manual(values = c("red","deeppink"))+  
  facet_grid( rows =vars(Species))  + xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))

pdf("Plot_BLUP_Caenorhabditis_Vg_dataScale_SS_P3pP4p.pdf")
Plot_BLUP_Caenorhabditis_Vg_dataScale_SS_P3pP4p
dev.off()


Plot_BLUP_Caenorhabditis_Vg_dataScale_SS_P3pP4p_v2 <- ggplot(BLUP_Caenorhabditis_Vg_dataScale_SS_WILD, aes(x = P3p_SS_BLUP, y = P4p_SS_BLUP)) + 
  geom_point(aes(colour = Species), shape=19, alpha=0.8) + scale_colour_manual(values = c("red","deeppink"))+  
  xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))

pdf("Plot_BLUP_Caenorhabditis_Vg_dataScale_SS_P3pP4p_v2_boxplot.pdf")
ggMarginal(Plot_BLUP_Caenorhabditis_Vg_dataScale_SS_P3pP4p_v2, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
dev.off()



---- ##BLUP_Celegans----

BLUP_Celegans_Vg_2_dataScale_WILD <- subset(BLUP_Caenorhabditis_Vg_dataScale_SS_WILD, Species =="C.elegans")

Plot_BLUP_Celegans_Vg_dataScale_P3pP4p <- ggplot(BLUP_Celegans_Vg_2_dataScale_WILD, aes(x = P3p_SS_BLUP, y = P4p_SS_BLUP)) + 
  geom_point(aes(colour = Species), shape=19, alpha=0.8) +scale_colour_manual(values = c("red"))+
  xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))

pdf("Plot_BLUP_Celegans_Vg_dataScale_P3pP4p_boxplot.pdf")
ggMarginal(Plot_BLUP_Celegans_Vg_dataScale_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
dev.off()


---- ##BLUP_Cbriggsae----

BLUP_Cbriggsae_Vg_2_dataScale_WILD <- subset(BLUP_Caenorhabditis_Vg_dataScale_SS_WILD, Species =="C.briggsae")

Plot_BLUP_Cbriggsae_Vg_dataScale_P3pP4p <- ggplot(BLUP_Cbriggsae_Vg_2_dataScale_WILD, aes(x = P3p_SS_BLUP, y = P4p_SS_BLUP)) + 
  geom_point(aes(colour = Species), shape=19, alpha=0.8) +scale_colour_manual(values = c("deeppink"))+
  xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))

pdf("Plot_BLUP_Cbriggsae_Vg_dataScale_P3pP4p_boxplot.pdf")
ggMarginal(Plot_BLUP_Cbriggsae_Vg_dataScale_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
dev.off()


#BLUP_Caenorhabditis_Vm_mod_dataScale_all_Pnp  ----                                                  

#reshape data to have one column with Pnp_fate and another with the frequency 

head(BLUP_Caenorhabditis_Vg_dataScale_SS_WILD)

BLUP_Caenorhabditis_Vg_dataScale_SS_WILD_1 <- BLUP_Caenorhabditis_Vg_dataScale_SS_WILD %>%
  gather(key = "trait", value = "Trait_Frequency", 2:7)
View(BLUP_Caenorhabditis_Vg_dataScale_SS_WILD_v2)

BLUP_Caenorhabditis_Vg_dataScale_SS_WILD_v2 <- BLUP_Caenorhabditis_Vg_dataScale_SS_WILD_1 %>%
  separate(trait, into = c("Pn.p", "Pn.p_fate"), sep = "_")



write_xlsx(BLUP_Caenorhabditis_Vg_dataScale_SS_WILD_v2, "BLUP_Caenorhabditis_Vg_dataScale_SS_WILD_v2.xlsx")

BLUP_Caenorhabditis_Vg_dataScale_SS_WILD_v3$Species <- factor(BLUP_Caenorhabditis_Vg_dataScale_SS_WILD_v3$Species, levels = c("C.elegans", "C.briggsae") )
factor(BLUP_Caenorhabditis_Vg_dataScale_SS_WILD_v3$Species)


Plot_BLUP_Caenorhabditis_Vg_dataScale_all_Pnp <- ggplot(BLUP_Caenorhabditis_Vg_dataScale_SS_WILD_v3, aes( x= Pn.p ,y= Trait_Frequency)) +
  geom_point(aes(colour = Species), size=0.7) + scale_colour_manual(values = c("red","deeppink"))+  
  facet_grid(rows =vars(Species))+ theme_bw() +
  labs(title = "BLUP_Caenorhabditis_Vm_mod_dataScale_all_Pnp", y ="Trait frequency") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 20)), legend.position = "bottom",axis.text.x = element_text(size = 10),axis.title = element_text(size = 15))


pdf("Plot_BLUP_Caenorhabditis_Vg_dataScale_all_Pnp.pdf")
Plot_BLUP_Caenorhabditis_Vg_dataScale_all_Pnp
dev.off()


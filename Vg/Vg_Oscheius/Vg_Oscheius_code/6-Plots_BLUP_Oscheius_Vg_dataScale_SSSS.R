#Plots_BLUP_Oscheius_Vg_dataScale_SSSS
library("readxl")
library("ggplot2")
library("ggpubr")
library("ggExtra")
library(tidyr)


#BLUP_Oscheius_Vg_2_dataScale from 2-BLUP_Otipulae_Vg_SSSSS.R

---- #BLUP_Oscheius----

s
table(BLUP_Oscheius_Vg_2_dataScale$Treatment)
names(BLUP_Oscheius_Vg_2_dataScale)

BLUP_Oscheius_Vg_2_dataScale_WILD <- subset(BLUP_Oscheius_Vg_2_dataScale, Treatment =="WILD")
table(BLUP_Oscheius_Vg_2_dataScale_WILD$Treatment)
table(BLUP_Oscheius_Vg_2_dataScale_WILD$Species)

BLUP_Oscheius_Vg_2_dataScale_WILD$Species <- factor(BLUP_Oscheius_Vg_2_dataScale_WILD$Species, levels = c("O.tipulae", "O.onirici") )
factor(BLUP_Oscheius_Vg_2_dataScale_WILD$Species)

Plot_BLUP_Oscheius_Vg_dataScale_P3pP4p  <- ggplot(BLUP_Oscheius_Vg_2_dataScale_WILD, aes(x = P3p_SSSS_BLUP, y = P4p_SSSS_BLUP)) + 
  geom_point(aes(colour = Species), shape=19) + scale_colour_manual(values = c("dodgerblue","turquoise"))+  
  facet_grid( rows =vars(Species))  + xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))

pdf("Plot_BLUP_Oscheius_Vg_dataScale_P3pP4p.pdf")
Plot_BLUP_Oscheius_Vg_dataScale_P3pP4p
dev.off()


Plot_BLUP_Oscheius_Vg_dataScale_P3pP4p_v2 <- ggplot(BLUP_Oscheius_Vg_2_dataScale_WILD, aes(x = P3p_SSSS_BLUP, y = P4p_SSSS_BLUP)) + 
  geom_point(aes(colour = Species), shape=19, alpha=0.8) + scale_colour_manual(values = c("dodgerblue","turquoise"))+  
  xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))

pdf("Plot_BLUP_Oscheius_Vg_dataScale_P3pP4p_v2_boxplot.pdf")
ggMarginal(Plot_BLUP_Oscheius_Vg_dataScale_P3pP4p_v2, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
dev.off()





---- ##BLUP_Otipulae----
s
BLUP_Otipulae_Vg_2_dataScale_WILD <- subset(BLUP_Oscheius_Vg_2_dataScale_WILD, Species =="O.tipulae")

Plot_BLUP_Otipulae_Vg_dataScale_P3pP4p <- ggplot(BLUP_Otipulae_Vg_2_dataScale_WILD, aes(x = P3p_SSSS_BLUP, y = P4p_SSSS_BLUP)) + 
  geom_point(aes(colour = Species), shape=19, alpha=0.8) +scale_colour_manual(values = c("dodgerblue"))+
  xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))

pdf("Plot_BLUP_Otipulae_Vg_dataScale_P3pP4p_boxplot.pdf")
ggMarginal(Plot_BLUP_Otipulae_Vg_dataScale_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
dev.off()



---- ##BLUP_Oonirici----
s
BLUP_Oonirici_Vg_2_dataScale_WILD <- subset(BLUP_Oscheius_Vg_2_dataScale_WILD, Species =="O.onirici")

Plot_BLUP_Oonirici_Vg_dataScale_P3pP4p <- ggplot(BLUP_Oonirici_Vg_2_dataScale_WILD, aes(x = P3p_SSSS_BLUP, y = P4p_SSSS_BLUP)) + 
  geom_point(aes(colour = Species), shape=19, alpha=0.8) +scale_colour_manual(values = c("turquoise"))+
  xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))

pdf("Plot_BLUP_Oonirici_Vg_dataScale_P3pP4p_boxplot.pdf")
ggMarginal(Plot_BLUP_Oonirici_Vg_dataScale_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
dev.off()




#BLUP_Oscheius_Vm_mod_dataScale_all_Pnp  ----   
#reshape data to have one column with Pnp_fate and another with the frequency 

BLUP_Oscheius_Vg_2_dataScale_WILD_1 <- BLUP_Oscheius_Vg_2_dataScale_WILD%>%
  gather(key = "trait", value = “Trait_frequency”, 2:7)

head(BLUP_Oscheius_Vg_2_dataScale_WILD_1)
View(BLUP_Oscheius_Vg_2_dataScale_WILD_1)


BLUP_Oscheius_Vg_2_dataScale_WILD_2 <- BLUP_Oscheius_Vg_2_dataScale_WILD_1 %>%
  separate(trait, into = c("Pn.p", "Pn.p_fate"), sep = "_")

View(BLUP_Oscheius_Vg_2_dataScale_WILD_2)

write_xlsx(BLUP_Oscheius_Vg_2_dataScale_WILD_2, "BLUP_Oscheius_Vg_2_dataScale_WILD_2.xlsx")


Plot_BLUP_Oscheius_Vg_dataScale_all_Pnp <- ggplot(BLUP_Oscheius_Vg_2_dataScale_WILD_2, aes( x= Pn.p ,y= Trait_Frequency)) +
  geom_point(aes(colour = Species), size=0.7) + scale_colour_manual(values = c("dodgerblue","turquoise"))+  
  facet_grid(rows =vars(Species))+ theme_bw() +
  labs(title = "BLUP_Oscheius_Vm_mod_dataScale_all_Pnp", y ="Trait frequency") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 20)), legend.position = "bottom",axis.text.x = element_text(size = 10),axis.title = element_text(size = 15))


pdf("Plot_BLUP_Oscheius_Vg_dataScale_all_Pnp.pdf")
Plot_BLUP_Oscheius_Vg_dataScale_all_Pnp
dev.off()


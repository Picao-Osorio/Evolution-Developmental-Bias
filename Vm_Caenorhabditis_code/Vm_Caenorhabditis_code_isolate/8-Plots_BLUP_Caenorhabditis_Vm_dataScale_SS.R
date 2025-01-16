#Plots_BLUP_Caenorhabditis_Vm_dataScale_SS
library("readxl")
library("ggplot2")
library("ggpubr")
library("ggExtra")



#BLUP_Caenorhabditis_Vm_dataScale_SS) from 3-Vm_Caenorhabditis_isolate_BLUP_Lines_SS.R

factor(BLUP_Caenorhabditis_Vm_dataScale_SS$Treatment)

---- #BLUP_Celegans----
#
BLUP_Celegans_Vm_dataScale_SS <- subset(BLUP_Caenorhabditis_Vm_dataScale_SS, Species =="C.elegans")

Plot_BLUP_Celegans_Vm_dataScale_P3pP4p <- ggplot(BLUP_Celegans_Vm_dataScale_SS, aes(x = P3p_SS_BLUP, y = P4p_SS_BLUP)) + 
  geom_point(aes(colour = Treatment, shape=Treatment,alpha = Treatment, size=Treatment))  + geom_point(data=subset(BLUP_Celegans_Vm_dataScale_SS, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment,alpha = Treatment)) +  scale_alpha_discrete( range = c(0.8, 1)) +scale_size_manual(values = c(1.5 , 2.5))+    scale_shape_manual(values = c(16 , 19))+
  scale_colour_manual(values = c("red4","red"))+
  facet_grid( rows =vars(Ancestor))  + xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))


pdf("Plot_BLUP_Celegans_Vm_dataScale_P3pP4p.pdf")
Plot_BLUP_Celegans_Vm_dataScale_P3pP4p
dev.off()




#BLUP_JU1200_Vm
{
  

  BLUP_JU1200_Vm_dataScale_SS <- subset(BLUP_Caenorhabditis_Vm_dataScale_SS, Ancestor =="JU1200")
  
  
  Plot_BLUP_JU1200_Vm_dataScale_SS_P3pP4p <- ggplot(BLUP_JU1200_Vm_dataScale_SS, aes(x = P3p_SS_BLUP, y = P4p_SS_BLUP)) + 
    geom_point(aes(colour = Treatment, shape=Treatment, size=Treatment))+ geom_point(data=subset(BLUP_JU1200_Vm_dataScale_SS, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment), alpha=0.8) +scale_size_manual(values = c(1.5 , 3))+ scale_shape_manual(values = c(16 , 19))+
    scale_colour_manual(values = c("red4","red")) +
    xlim(0, 1) + ylim(0,1)+
    theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))
  
    
  
  pdf("Plot_BLUP_JU1200_Vm_dataScale_SS_P3pP4p_boxplot.pdf")
  ggMarginal(Plot_BLUP_JU1200_Vm_dataScale_SS_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
  dev.off() 
  
  
}

#BLUP_PB306_Vm
{
  table(BLUP_PB306_Vm_dataScale_SS$Treatment)
  
  BLUP_PB306_Vm_dataScale_SS <- subset(BLUP_Caenorhabditis_Vm_dataScale_SS, Ancestor =="PB306")
  
  
  Plot_BLUP_PB306_Vm_dataScale_SS_P3pP4p <- ggplot(BLUP_PB306_Vm_dataScale_SS, aes(x = P3p_SS_BLUP, y = P4p_SS_BLUP)) + 
    geom_point(aes(colour = Treatment, shape=Treatment, size=Treatment))+ geom_point(data=subset(BLUP_PB306_Vm_dataScale_SS, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment), alpha=0.8) +scale_size_manual(values = c(1.5 , 3))+ scale_shape_manual(values = c(16 , 19))+
    scale_colour_manual(values = c("red4","red")) +
    xlim(0, 1) + ylim(0,1)+
    theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))
  
 
  
  pdf("Plot_BLUP_PB306_Vm_dataScale_SS_P3pP4p_boxplot.pdf")
  ggMarginal(Plot_BLUP_PB306_Vm_dataScale_SS_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
  dev.off() 
  
  
}    


---- #BLUP_Cbriggsae----
#
BLUP_Cbriggsae_Vm_dataScale <- subset(BLUP_Caenorhabditis_Vm_dataScale_SS, Species =="C.briggsae")

Plot_BLUP_Cbriggsae_Vm_dataScale_P3pP4p <- ggplot(BLUP_Cbriggsae_Vm_dataScale, aes(x = P3p_SS_BLUP, y = P4p_SS_BLUP)) + 
  geom_point(aes(colour = Treatment, shape=Treatment,alpha = Treatment, size=Treatment))  + geom_point(data=subset(BLUP_Cbriggsae_Vm_dataScale, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment,alpha = Treatment)) +  scale_alpha_discrete( range = c(0.8, 1)) +scale_size_manual(values = c(1.5 , 2.5))+    scale_shape_manual(values = c(16 , 19))+
  scale_colour_manual(values = c("pink4","deeppink"))+
  facet_grid( rows =vars(Ancestor))  + xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))


pdf("Plot_BLUP_Cbriggsae_Vm_dataScale_P3pP4p.pdf")
Plot_BLUP_Cbriggsae_Vm_dataScale_P3pP4p
dev.off()



#BLUP_AF16_Vm
{
  BLUP_AF16_Vm_dataScale_SS <- subset(BLUP_Caenorhabditis_Vm_dataScale_SS, Ancestor =="AF16")
  
  table(BLUP_AF16_Vm_dataScale_SS$Treatment)
  
  Plot_BLUP_AF16_Vm_dataScale_SS_P3pP4p <- ggplot(BLUP_AF16_Vm_dataScale_SS, aes(x = P3p_SS_BLUP, y = P4p_SS_BLUP)) + 
    geom_point(aes(colour = Treatment, shape=Treatment, size=Treatment))+ geom_point(data=subset(BLUP_AF16_Vm_dataScale_SS, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment), alpha=0.8) +scale_size_manual(values = c(1.5 , 3))+ scale_shape_manual(values = c(16 , 19))+
    scale_colour_manual(values = c("pink4","deeppink")) +
    xlim(0, 1) + ylim(0,1)+
    theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))
  
   
  
  pdf("Plot_BLUP_AF16_Vm_dataScale_SS_P3pP4p_boxplot.pdf")
  ggMarginal(Plot_BLUP_AF16_Vm_dataScale_SS_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
  dev.off() 
  
  
}

#BLUP_PB800_Vm
{
  BLUP_PB800_Vm_dataScale_SS <- subset(BLUP_Caenorhabditis_Vm_dataScale_SS, Ancestor =="PB800")
  table(BLUP_PB800_Vm_dataScale_SS$Treatment)
  
  Plot_BLUP_PB800_Vm_dataScale_SS_P3pP4p <- ggplot(BLUP_PB800_Vm_dataScale_SS, aes(x = P3p_SS_BLUP, y = P4p_SS_BLUP)) + 
    geom_point(aes(colour = Treatment, shape=Treatment, size=Treatment))+ geom_point(data=subset(BLUP_PB800_Vm_dataScale_SS, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment), alpha=0.8) +scale_size_manual(values = c(1.5 , 3))+ scale_shape_manual(values = c(16 , 19))+
    scale_colour_manual(values = c("pink4","deeppink")) +
    xlim(0, 1) + ylim(0,1)+
    theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))
  
  
  
  pdf("Plot_BLUP_PB800_Vm_dataScale_SS_P3pP4p_boxplot.pdf")
  ggMarginal(Plot_BLUP_PB800_Vm_dataScale_SS_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
  dev.off() 
  
  
}    

#BLUP_Caenorhabditis_Vm_mod_dataScale_all_Pnp  ----                                                  

#reshape data to have one column with Pnp_fate and another with the frequency 

BLUP_Caenorhabditis_Vm_mod_dataScale_SS_1 <- BLUP_Caenorhabditis_Vm_mod_dataScale_SS%>%
  gather(key = "trait", value = “Trait_frequency”, 2:7)

head(BLUP_Caenorhabditis_Vm_mod_dataScale_SS_1)
View(BLUP_Caenorhabditis_Vm_mod_dataScale_SS_1)


BLUP_Caenorhabditis_Vm_mod_dataScale_SS_2 <- BLUP_Caenorhabditis_Vm_mod_dataScale_SS_1 %>%
  separate(trait, into = c("Pn.p", "Pn.p_fate"), sep = "_")

View(BLUP_Caenorhabditis_Vm_mod_dataScale_SS_2)

write_xlsx(BLUP_Caenorhabditis_Vm_mod_dataScale_SS_2, "BLUP_Caenorhabditis_Vm_mod_dataScale_SS_2.xlsx")



BLUP_Caenorhabditis_Vm_mod_dataScale_SS_2$Ancestor <- factor(BLUP_Caenorhabditis_Vm_mod_dataScale_SS_2$Ancestor, levels = c("PB306", "JU1200","AF16","PB800") )
factor(BLUP_Caenorhabditis_Vm_mod_dataScale_SS_2$Ancestor)


Plot_BLUP_Caenorhabditis_Vm_dataScale_all_Pnp <- ggplot(BLUP_Caenorhabditis_Vm_mod_dataScale_SS_2, aes( x= Treatment ,y= Trait_Frequency)) +
  geom_point(aes(colour=interaction(Treatment, Species, sep=':')), size=0.7)+  scale_colour_manual(values = c("pink4","deeppink","red4","red"))+
  facet_grid(Ancestor~Pn.p)+ theme_bw() +
  labs(title = "BLUP_Caenorhabditis_Vm_mod_dataScale_all_Pnp", y ="Trait frequency") + theme( axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))

pdf("Plot_BLUP_Caenorhabditis_Vm_dataScale_all_Pnp.pdf")
Plot_BLUP_Caenorhabditis_Vm_dataScale_all_Pnp
dev.off()



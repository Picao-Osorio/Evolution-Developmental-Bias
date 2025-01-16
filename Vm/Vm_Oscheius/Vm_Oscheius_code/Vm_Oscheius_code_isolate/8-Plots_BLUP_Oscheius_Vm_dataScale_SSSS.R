#Plots_BLUP_Oscheius_Vm_dataScale_SSSS
library("readxl")
library("ggplot2")
library("ggpubr")
library("ggExtra")

# BLUP_Oscheius_Vm_mod_dataScale_SSSS from 3-Vm_BLUP_Oscheius_Lines_SSSS.R

head(BLUP_Oscheius_Vm_mod_dataScale_SSSS)
View(BLUP_Oscheius_Vm_mod_dataScale_SSSS)



---- #BLUP_Otipulae----
#
BLUP_Otipulae_Vm_mod_dataScale_SSSS <- subset(BLUP_Oscheius_Vm_mod_dataScale_SSSS, Species =="O.tipulae")

Plot_BLUP_Otipulae_Vm_dataScale_P3pP4p_SSSS <- ggplot(BLUP_Otipulae_Vm_mod_dataScale_SSSS, aes(x = P3p_SSSS_BLUP, y = P4p_SSSS_BLUP)) + 
  geom_point(aes(colour = Treatment, shape=Treatment,alpha = Treatment, size=Treatment))  + geom_point(data=subset(BLUP_Otipulae_Vm_mod_dataScale_SSSS, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment,alpha = Treatment)) +  scale_alpha_discrete( range = c(0.8, 1)) +scale_size_manual(values = c(1.2 , 2.5))+    scale_shape_manual(values = c(16 , 19))+
  scale_colour_manual(values = c("royalblue4","deepskyblue"))+
  facet_grid( rows =vars(Ancestor))  + xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))


pdf("Plot_BLUP_Otipulae_Vm_dataScale_P3pP4p_SSSS.pdf")
Plot_BLUP_Otipulae_Vm_dataScale_P3pP4p_SSSS
dev.off()




#BLUP_CEW1_Vm
{
  
  
  
  BLUP_CEW1_Vm_mod_dataScale_SSSS <- subset(BLUP_Oscheius_Vm_mod_dataScale_SSSS, Ancestor =="CEW1")
  table(BLUP_CEW1_Vm_mod_dataScale_SSSS$Treatment)
  
  Plot_BLUP_CEW1_Vm_dataScale_P3pP4p_SSSS <- ggplot(BLUP_CEW1_Vm_mod_dataScale_SSSS, aes(x = P3p_SSSS_BLUP, y = P4p_SSSS_BLUP)) + 
    geom_point(aes(colour = Treatment, shape=Treatment, size=Treatment))+ geom_point(data=subset(BLUP_CEW1_Vm_mod_dataScale_SSSS, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment), alpha=0.8) +scale_size_manual(values = c(1.2 , 3))+ scale_shape_manual(values = c(16 , 19))+
    scale_colour_manual(values = c("royalblue4","deepskyblue")) +
    xlim(0, 1) + ylim(0,1)+
    theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))
  
  
  
  pdf("Plot_BLUP_CEW1_Vm_dataScale_P3pP4p_SSSS_boxplot.pdf")
  ggMarginal(Plot_BLUP_CEW1_Vm_dataScale_P3pP4p_SSSS, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
  dev.off() 
  
  
}

#BLUP_JU178_Vm
{
  
  BLUP_JU178_Vm_mod_dataScale_SSSS <- subset(BLUP_Oscheius_Vm_mod_dataScale_SSSS, Ancestor =="JU178")
  table(BLUP_JU178_Vm_mod_dataScale_SSSS$Treatment)
  
  
  Plot_BLUP_JU178_Vm_dataScale_P3pP4p_SSSS <- ggplot(BLUP_JU178_Vm_mod_dataScale_SSSS, aes(x = P3p_SSSS_BLUP, y = P4p_SSSS_BLUP)) + 
    geom_point(aes(colour = Treatment, shape=Treatment, size=Treatment))+ geom_point(data=subset(BLUP_JU178_Vm_mod_dataScale_SSSS, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment), alpha=0.8) +scale_size_manual(values = c(1.2 , 3))+ scale_shape_manual(values = c(16 , 19))+
    scale_colour_manual(values = c("royalblue4","deepskyblue")) +
    xlim(0, 1) + ylim(0,1)+
    theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))
  
 
  pdf("Plot_BLUP_JU178_Vm_dataScale_P3pP4p_SSSS_boxplot.pdf")
  ggMarginal(Plot_BLUP_JU178_Vm_dataScale_P3pP4p_SSSS, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
  dev.off() 
  
  
}    


---- #BLUP_Oonirici----
#
BLUP_Oonirici_Vm_mod_dataScale_SSSS <- subset(BLUP_Oscheius_Vm_mod_dataScale_SSSS, Species =="O.onirici")

Plot_BLUP_Oonirici_Vm_dataScale_P3pP4p_SSSS <- ggplot(BLUP_Oonirici_Vm_mod_dataScale_SSSS, aes(x = P3p_SSSS_BLUP, y = P4p_SSSS_BLUP)) + 
  geom_point(aes(colour = Treatment, shape=Treatment,alpha = Treatment, size=Treatment))  + geom_point(data=subset(BLUP_Oonirici_Vm_mod_dataScale_SSSS, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment,alpha = Treatment)) +  scale_alpha_discrete( range = c(0.8, 1)) +scale_size_manual(values = c(1.2 , 2.5))+    scale_shape_manual(values = c(16 , 19))+
  scale_colour_manual(values = c("skyblue4","turquoise"))+
  facet_grid( rows =vars(Ancestor))  + xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))


pdf("Plot_BLUP_Oonirici_Vm_dataScale_P3pP4p_SSSS.pdf")
Plot_BLUP_Oonirici_Vm_dataScale_P3pP4p_SSSS
dev.off()



#BLUP_PS2068_Vm
{
  
  BLUP_PS2068_Vm_mod_dataScale_SSSS <- subset(BLUP_Oscheius_Vm_mod_dataScale_SSSS, Ancestor =="PS2068")
  
  table(BLUP_PS2068_Vm_mod_dataScale_SSSS$Treatment)
  
  Plot_BLUP_PS2068_Vm_dataScale_P3pP4p_SSSS <- ggplot(BLUP_PS2068_Vm_mod_dataScale_SSSS, aes(x = P3p_SSSS_BLUP, y = P4p_SSSS_BLUP)) + 
    geom_point(aes(colour = Treatment, shape=Treatment, size=Treatment))+ geom_point(data=subset(BLUP_PS2068_Vm_mod_dataScale_SSSS, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment), alpha=0.8) +scale_size_manual(values = c(1.2 , 3))+ scale_shape_manual(values = c(16 , 19))+
    scale_colour_manual(values = c("skyblue4","turquoise")) +
    xlim(0, 1) + ylim(0,1)+
    theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))
  
  
 
  pdf("Plot_BLUP_PS2068_Vm_dataScale_P3pP4p_SSSS_boxplot.pdf")
  ggMarginal(Plot_BLUP_PS2068_Vm_dataScale_P3pP4p_SSSS, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
  dev.off() 
  
  
}

#BLUP_JU77_Vm
{
  BLUP_JU77_Vm_mod_dataScale_SSSS <- subset(BLUP_Oscheius_Vm_mod_dataScale_SSSS, Ancestor =="JU77")
  table(BLUP_JU77_Vm_mod_dataScale_SSSS$Treatment)
  
  Plot_BLUP_JU77_Vm_dataScale_P3pP4p_SSSS <- ggplot(BLUP_JU77_Vm_mod_dataScale_SSSS, aes(x = P3p_SSSS_BLUP, y = P4p_SSSS_BLUP)) + 
    geom_point(aes(colour = Treatment, shape=Treatment, size=Treatment))+ geom_point(data=subset(BLUP_JU77_Vm_mod_dataScale_SSSS, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment), alpha=0.8) +scale_size_manual(values = c(1.2 , 3))+ scale_shape_manual(values = c(16 , 19))+
    scale_colour_manual(values = c("skyblue4","turquoise")) +
    xlim(0, 1) + ylim(0,1)+
    theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))
  
  
  
  pdf("Plot_BLUP_JU77_Vm_dataScale_P3pP4p_SSSS_boxplot.pdf")
  ggMarginal(Plot_BLUP_JU77_Vm_dataScale_P3pP4p_SSSS, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
  dev.off() 
  
  
}    

#BLUP_Oscheius_Vm_mod_dataScale_all_Pnp  ----                                                  

#reshape data to have one column with Pnp_fate and another with the frequency 

BLUP_Oscheius_Vm_mod_dataScale_SSSS_1 <- BLUP_Oscheius_Vm_mod_dataScale_SSSS%>%
  gather(key = "trait", value = “Trait_frequency”, 2:7)

head(BLUP_Oscheius_Vm_mod_dataScale_SSSS_1)
View(BLUP_Oscheius_Vm_mod_dataScale_SSSS_1)


BLUP_Oscheius_Vm_mod_dataScale_SSSS_2 <- BLUP_Oscheius_Vm_mod_dataScale_SSSS_1 %>%
  separate(trait, into = c("Pn.p", "Pn.p_fate"), sep = "_")

View(BLUP_Oscheius_Vm_mod_dataScale_SSSS_2)

write_xlsx(BLUP_Oscheius_Vm_mod_dataScale_SSSS_2, "BLUP_Oscheius_Vm_mod_dataScale_SSSS_2.xlsx")



Plot_BLUP_Oscheius_Vm_dataScale_all_Pnp <- ggplot(BLUP_Oscheius_Vm_mod_dataScale_SSSS_2, aes( x= Treatment ,y= Trait_Frequency)) +
  geom_point(aes(colour=interaction(Treatment, Species, sep=':')), size=0.7)+  scale_colour_manual(values = c("skyblue4","turquoise","royalblue4","deepskyblue"))+
  facet_grid(Ancestor~Pn.p)+ theme_bw() +
  labs(title = "BLUP_Oscheius_Vm_mod_dataScale_all_Pnp", y ="Trait frequency") + theme( axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))

pdf("Plot_BLUP_Oscheius_Vm_dataScale_all_Pnp.pdf")
Plot_BLUP_Oscheius_Vm_dataScale_all_Pnp
dev.off()


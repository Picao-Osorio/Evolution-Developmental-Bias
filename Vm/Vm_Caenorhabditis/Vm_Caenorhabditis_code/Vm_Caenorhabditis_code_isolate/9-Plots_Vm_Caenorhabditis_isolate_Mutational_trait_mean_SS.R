#Plots_Vm_Caenorhabditis_isolate_Mutational_trait_mean_SS

library(readxl)
library(ggplot2)
library(ggdark)
library(ggtext) 

#Caenorhabditis_Vm_mutational_median_effects_SS_summary from 5-Vm_Caenorhabditis_Mutational_Median_Effects_SS
head(Caenorhabditis_Vm_mutational_median_effects_SS_summary)

Caenorhabditis_Vm_mutational_median_effects_SS_summary$Ancestor <- factor(Caenorhabditis_Vm_mutational_median_effects_SS_summary$Ancestor, levels = c("PB306", "JU1200","AF16","PB800") )
factor(Caenorhabditis_Vm_mutational_median_effects_SS_summary$Ancestor)

Caenorhabditis_Vm_mutational_median_effects_SS_summary$Species <- factor(Caenorhabditis_Vm_mutational_median_effects_SS_summary$Species, levels = c("C.elegans","C.briggsae") )
factor(Caenorhabditis_Vm_mutational_median_effects_SS_summary$Species)

Caenorhabditis_Vm_mutational_median_effects_SS_summary_dataScale <- subset(Caenorhabditis_Vm_mutational_median_effects_SS_summary, Scale=="Data")
table(Caenorhabditis_Vm_mutational_median_effects_SS_summary$Species)
head(Caenorhabditis_Vm_mutational_median_effects_SS_summary_dataScale)

#Caenorhabditis_Vm_mutational_median_effects_SS_summary: all Pn.p  ----                                                  
Plot_Vm_Caenorhabditis_isolate_Mutational_trait_mean_dataScale <- ggplot(Caenorhabditis_Vm_mutational_median_effects_SS_summary_dataScale, aes( x=Treatment ,y= median)) +
  geom_errorbar(aes(ymin=lower.HPD_0.95, ymax=upper.HPD_0.95), width=0.1) + 
  geom_linerange(aes(ymin=lower.HPD_0.83, ymax=upper.HPD_0.83, color = interaction(Treatment, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red","pink4","deeppink"))+
  geom_point(aes(fill=interaction(Treatment, Species, sep=':')), shape=23,size=0.8)+  scale_fill_manual(values = c("red4","red","pink4","deeppink"))+
  facet_grid(Ancestor~Pnp)+ theme_bw() +
  labs(title = "Vm_Caenorhabditis_isolate_Mutational-Bias_trait_mean", y ="Trait Mean (z)") + theme( axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))

pdf("Plot_Vm_Caenorhabditis_isolate_Mutational_trait_mean_dataScale.pdf")
Plot_Vm_Caenorhabditis_isolate_Mutational_trait_mean_dataScale
dev.off()

#Caenorhabditis_Vm_mutational_median_effects_SS_summary:only P3.p----

Caenorhabditis_Vm_mutational_median_effects_SS_summary_dataScale_onlyP3p <- subset(Caenorhabditis_Vm_mutational_median_effects_SS_summary_dataScale, Pnp =="P3.p")

Plot_Vm_Caenorhabditis_isolate_Mutational_trait_mean_dataScale_onlyP3p <- ggplot(Caenorhabditis_Vm_mutational_median_effects_SS_summary_dataScale_onlyP3p, aes( x=Treatment ,y= median)) +
  geom_errorbar(aes(ymin=lower.HPD_0.95, ymax=upper.HPD_0.95), width=0.1) + 
  geom_linerange(aes(ymin=lower.HPD_0.83, ymax=upper.HPD_0.83, color = interaction(Treatment, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red","pink4","deeppink"))+
  geom_point(aes(fill=interaction(Treatment, Species, sep=':')), shape=23,size=0.8)+  scale_fill_manual(values = c("red4","red","pink4","deeppink"))+
  facet_grid(Ancestor~Pnp)+ theme_bw() + ylim(0, 0.45) + 
  labs(title = "Vm_Caenorhabditis_isolate_Mutational-Bias_trait_mean_onlyP3p", y ="Trait Mean (z)") + theme(  aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))


pdf("Plot_Vm_Caenorhabditis_isolate_Mutational_trait_mean_dataScale_onlyP3p.pdf")
Plot_Vm_Caenorhabditis_isolate_Mutational_trait_mean_dataScale_onlyP3p
dev.off()


Plot_Vm_Caenorhabditis_isolate_Mutational_trait_mean_dataScale_onlyP3p_2 <- ggplot(Caenorhabditis_Vm_mutational_median_effects_SS_summary_dataScale_onlyP3p, aes( x=Treatment ,y= median)) +
  geom_errorbar(aes(ymin=lower.HPD_0.95, ymax=upper.HPD_0.95), width=0.1) + 
  geom_linerange(aes(ymin=lower.HPD_0.83, ymax=upper.HPD_0.83, color = interaction(Treatment, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red","pink4","deeppink"))+
  geom_point(aes(fill=interaction(Treatment, Species, sep=':')), shape=23,size=0.8)+  scale_fill_manual(values = c("red4","red","pink4","deeppink"))+
  facet_grid(~Ancestor)+ theme_bw() + ylim(0, 0.45) + 
  labs(title = "Vm_Caenorhabditis_isolate_Mutational-Bias_trait_mean_P3.p", y ="Trait Mean (z)") + theme( aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))

pdf("Plot_Vm_Caenorhabditis_isolate_Mutational_trait_mean_dataScale_onlyP3p_2.pdf")
Plot_Vm_Caenorhabditis_isolate_Mutational_trait_mean_dataScale_onlyP3p_2
dev.off()

#Caenorhabditis_Vm_mutational_median_effects_SS_summary:without P3.p----
Caenorhabditis_Vm_mutational_median_effects_SS_summary_dataScale_woutP3p <- subset(Caenorhabditis_Vm_mutational_median_effects_SS_summary_dataScale, Pnp !="P3.p")

Plot_Vm_Caenorhabditis_isolate_Mutational_trait_mean_dataScale_woutP3p <- ggplot(Caenorhabditis_Vm_mutational_median_effects_SS_summary_dataScale_woutP3p, aes( x=Treatment ,y= median)) +
  geom_errorbar(aes(ymin=lower.HPD_0.95, ymax=upper.HPD_0.95), width=0.1) + 
  geom_linerange(aes(ymin=lower.HPD_0.83, ymax=upper.HPD_0.83, color = interaction(Treatment, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red","pink4","deeppink"))+
  geom_point(aes(fill=interaction(Treatment, Species, sep=':')), shape=23,size=0.8)+  scale_fill_manual(values = c("red4","red","pink4","deeppink"))+
  facet_grid(Ancestor~Pnp)+ theme_bw()  + 
  labs(title = "Vm_Caenorhabditis_isolate_Mutational-Bias_trait_mean_woutP3.p", y ="Trait Mean (z)") + theme( aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))

pdf("Plot_Vm_Caenorhabditis_isolate_Mutational_trait_mean_dataScale_woutP3p.pdf")
Plot_Vm_Caenorhabditis_isolate_Mutational_trait_mean_dataScale_woutP3p
dev.off()

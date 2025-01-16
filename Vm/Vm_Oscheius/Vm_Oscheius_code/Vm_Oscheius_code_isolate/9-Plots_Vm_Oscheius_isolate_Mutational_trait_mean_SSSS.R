#Plots_Vm_Oscheius_isolate_Mutational_trait_mean_SSSS
library(readxl)
library(ggplot2)
library(ggdark)
library(ggtext) 

#Oscheius_Vm_mutational_median_effects_SSSS_Summary from 5-Vm_Oscheius_Mutational_Median_Effects_SSSS

table(Oscheius_Vm_mutational_median_effects_SSSS_Summary$Scale)
Oscheius_Vm_Mutational_Median_effects_data_scale_SSSS <- subset(Oscheius_Vm_mutational_median_effects_SSSS_Summary, Scale=="Data")

Oscheius_Vm_Mutational_Median_effects_data_scale_SSSS$Species <- factor(Oscheius_Vm_Mutational_Median_effects_data_scale_SSSS$Species, levels = c("O.tipulae","O.onirici") )
factor(Oscheius_Vm_Mutational_Median_effects_data_scale_SSSS$Species)

table(Oscheius_Vm_Mutational_Median_effects_data_scale_SSSS$Species)
table(Oscheius_Vm_Mutational_Median_effects_data_scale_SSSS$Treatment)

#Oscheius_Vm_Mutational_Median_effects_data_scale_SSSS: all Pn.p  ----                                                  
Plot_Vm_Oscheius_isolate_Mutational_trait_mean_dataScale <- ggplot(Oscheius_Vm_Mutational_Median_effects_data_scale_SSSS, aes( x=Treatment ,y= median)) +
  geom_errorbar(aes(ymin=lower.HPD_0.95, ymax=upper.HPD_0.95), width=0.1) + 
  geom_linerange(aes(ymin=lower.HPD_0.83, ymax=upper.HPD_0.83, color = interaction(Treatment, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue","skyblue4","turquoise"))+
  geom_point(aes(fill=interaction(Treatment, Species, sep=':')), shape=23,size=0.8)+  scale_fill_manual(values = c("royalblue4","deepskyblue","skyblue4","turquoise"))+
  facet_grid(Ancestor~Pnp)+ theme_bw() +
  labs(title = "Vm_Oscheius_isolate_Mutational-Bias_trait_mean", y ="Frequency of trait Mean (z)") + theme( axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))

pdf("Plot_Vm_Oscheius_isolate_Mutational_trait_mean_dataScale.pdf")
Plot_Vm_Oscheius_isolate_Mutational_trait_mean_dataScale
dev.off()

#Oscheius_Vm_Mutational_Median_effects_data_scale_SSSS:only P4.p----
Oscheius_Vm_Mutational_Median_effects_data_scale_SSSS_onlyP4p <- subset(Oscheius_Vm_Mutational_Median_effects_data_scale_SSSS, Pnp =="P4.p")

Plot_Vm_Oscheius_isolate_Mutational_trait_mean_dataScale_onlyP4p <- ggplot(Oscheius_Vm_Mutational_Median_effects_data_scale_SSSS_onlyP4p, aes( x=Treatment ,y= median)) +
  geom_errorbar(aes(ymin=lower.HPD_0.95, ymax=upper.HPD_0.95), width=0.1) + 
  geom_linerange(aes(ymin=lower.HPD_0.83, ymax=upper.HPD_0.83, color = interaction(Treatment, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue","skyblue4","turquoise"))+
  geom_point(aes(fill=interaction(Treatment, Species, sep=':')), shape=23,size=0.8)+  scale_fill_manual(values = c("royalblue4","deepskyblue","skyblue4","turquoise"))+
  facet_grid(Ancestor~Pnp)+ theme_bw() + ylim(0.15, 1) + 
  labs(title = "Vm_Oscheius_isolate_Mutational-Bias_trait_mean_onlyP4p", y ="Frequency of trait Mean (z)") + theme(  aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))


pdf("Plot_Vm_Oscheius_isolate_Mutational_trait_mean_dataScale_onlyP4p.pdf")
Plot_Vm_Oscheius_isolate_Mutational_trait_mean_dataScale_onlyP4p
dev.off()



#Oscheius_Vm_Mutational_Median_effects_data_scale_SSSS:without P3.p_P4p----
Oscheius_Vm_Mutational_Median_effects_data_scale_SSSS_woutP3p_P4p <- subset(Oscheius_Vm_Mutational_Median_effects_data_scale_SSSS, Pnp !="P4.p" & Pnp !="P3.p")
table(Oscheius_Vm_Mutational_Median_effects_data_scale_SSSS_woutP3p_P4p$Pnp)

Plot_Vm_Oscheius_isolate_Mutational_trait_mean_dataScale_woutP3p_P4p <- ggplot(Oscheius_Vm_Mutational_Median_effects_data_scale_SSSS_woutP3p_P4p, aes( x=Treatment ,y= median)) +
  geom_errorbar(aes(ymin=lower.HPD_0.95, ymax=upper.HPD_0.95), width=0.1) + 
  geom_linerange(aes(ymin=lower.HPD_0.83, ymax=upper.HPD_0.83, color = interaction(Treatment, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue","skyblue4","turquoise"))+
  geom_point(aes(fill=interaction(Treatment, Species, sep=':')), shape=23,size=0.8)+  scale_fill_manual(values = c("royalblue4","deepskyblue","skyblue4","turquoise"))+
  facet_grid(Ancestor~Pnp)+ theme_bw() + 
  labs(title = "Vm_Oscheius_isolate_Mutational-Bias_trait_mean_woutP3p_P4.p", y ="Frequency of trait Mean (z)") + theme( aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))

pdf("Plot_Vm_Oscheius_isolate_Mutational_trait_mean_dataScale_woutP3p_P4p.pdf")
Plot_Vm_Oscheius_isolate_Mutational_trait_mean_dataScale_woutP3p_P4p
dev.off()

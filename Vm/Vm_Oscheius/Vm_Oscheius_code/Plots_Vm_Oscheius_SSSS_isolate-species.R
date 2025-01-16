#Plots_Vm_Oscheius_SSSS_isolate-species
library(readxl)
library(ggplot2)
library(ggdark)
library(ggtext) 
library(ggrepel)


#Vm_Oscheius_species_summary_SSSS_vulva from 2-Vm_Oscheius_species_summary_SSSS_vulva.R
#Vm_Oscheius_isolate_SSSS_Summary		from 6-Vm_Oscheius_isolate_SSSS_vulva_Summary.R

#Plots_Va_H2_Vm_Oscheius_isolate_species----

{
  ## Plots_Va_H2_Vm_Oscheius_isolate_dataScale ----
  
  ###Plots_Va_Vm_Oscheius_isolate_dataScale----
  
  Va_Vm_Oscheius_isolate_dataScale <- subset(Vm_Oscheius_isolate_SSSS_Summary, Scale =="data"& Measure=="Va")
  View(Va_Vm_Oscheius_isolate_dataScale)
  
  Va_Vm_Oscheius_isolate_dataScale$Ancestral <- factor(Va_Vm_Oscheius_isolate_dataScale$Ancestral, levels = c("CEW1","JU178", "PS2068","JU77") )
  factor(Va_Vm_Oscheius_isolate_dataScale$Ancestral)
  
  Va_Vm_Oscheius_isolate_dataScale$Species <- factor(Va_Vm_Oscheius_isolate_dataScale$Species, levels = c("O.tipulae","O.onirici") )
  factor(Va_Vm_Oscheius_isolate_dataScale$Species)
  
  factor(Va_Vm_Oscheius_isolate_dataScale$Model_Set)
  
  #Va_Vm_Oscheius_isolate_dataScale: all Pn.p                                                    
  Plot_Va_Vm_Oscheius_isolate_dataScale <- ggplot(Va_Vm_Oscheius_isolate_dataScale, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise3","turquoise"))+
    facet_grid(Ancestral~Pnp, scales="free")+ theme_bw() +
    labs(title = "Oscheius_isolate_Vm_SSSS", y=expression(paste("Mutational Variance (",italic(V[M]),")")), x="Models")+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Oscheius_isolate_dataScale.pdf")
  Plot_Va_Vm_Oscheius_isolate_dataScale
  dev.off()
  
  #Va_Vm_Oscheius_isolate_dataScale: without P4.p
  Va_Vm_Oscheius_isolate_dataScale_woutP4p <- subset(Va_Vm_Oscheius_isolate_dataScale, Pnp !="P4.p")
  View(Va_Vm_Oscheius_isolate_dataScale_woutP4p)
  
  Plot_Va_Vm_Oscheius_isolate_dataScale_woutP4p <- ggplot(Va_Vm_Oscheius_isolate_dataScale_woutP4p, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise3","turquoise"))+
    facet_grid(Ancestral~Pnp, scales="free")+ theme_bw() +
    labs(title = "Oscheius_isolate_Vm_woutP4p", y =expression(paste("Mutational Variance (",italic(V[M]),")")), x="Models")+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vm_Oscheius_isolate_dataScale_woutP4p.pdf")
  Plot_Va_Vm_Oscheius_isolate_dataScale_woutP4p
  dev.off()               
  
  
  #Va_Vm_Oscheius_isolate_dataScale: all Pn.p in log10 scale
  Plot_Va_Vm_Oscheius_isolate_dataScale_log10 <-  ggplot(Va_Vm_Oscheius_isolate_dataScale, aes( x=Model_Set ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    facet_grid(Ancestral~Pnp)+ theme_bw() +
    labs(title = "Oscheius_isolate_Vm log10 scale", y =expression(paste(log[10] (italic(V[M])))), x="Models")+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vm_Oscheius_isolate_dataScale_log10.pdf")
  Plot_Va_Vm_Oscheius_isolate_dataScale_log10
  dev.off()
  
  #Va_Vm_Oscheius_isolate_dataScale: all Pn.p in log10 scale only Vm
  Va_Vm_Oscheius_isolate_dataScale_Vm <- subset(Va_Vm_Oscheius_isolate_dataScale, Model_Set =="Vm")
  
  Plot_Va_Vm_Oscheius_isolate_dataScale_Vm_log10 <- ggplot(Va_Vm_Oscheius_isolate_dataScale_Vm, aes( x=Pnp ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("deepskyblue","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("deepskyblue","turquoise"))+
    facet_grid(rows=vars(Ancestral))+ theme_bw() +
    labs(title = "Oscheius_isolate_Vm log10 scale", y =expression(paste(log[10] (italic(V[M])))),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Oscheius_isolate_dataScale_Vm_log10.pdf")
  Plot_Va_Vm_Oscheius_isolate_dataScale_Vm_log10
  dev.off()
  
  #Va_Vm_Oscheius_isolate_dataScale: all Pn.p  only Vm
  
  Plot_Va_Vm_Oscheius_isolate_dataScale_Vm <- ggplot(Va_Vm_Oscheius_isolate_dataScale_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("deepskyblue","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("deepskyblue","turquoise"))+
    facet_grid(rows=vars(Ancestral), scales="free")+ theme_bw() +
    labs(title = "Oscheius_isolate_Vm", y=expression(paste("Mutational Variance (",italic(V[M]),")")), x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Oscheius_isolate_dataScale_Vm.pdf")
  Plot_Va_Vm_Oscheius_isolate_dataScale_Vm
  dev.off()
  
  
  #Va_Vm_Oscheius_isolate_dataScale_CEW1: all Pn.p                                                    
  
  Va_Vm_Oscheius_isolate_dataScale_CEW1 <- subset(Va_Vm_Oscheius_isolate_dataScale, Ancestral =="CEW1")
  
  Plot_Va_Vm_Oscheius_isolate_dataScale_CEW1 <- ggplot(Va_Vm_Oscheius_isolate_dataScale_CEW1, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_Set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue"))+
    geom_point(aes(fill=Model_Set), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "CEW1_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")), x="Models")+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vm_Oscheius_isolate_dataScale_CEW1.pdf")
  Plot_Va_Vm_Oscheius_isolate_dataScale_CEW1
  dev.off()
  
  Va_Vm_Oscheius_isolate_dataScale_CEW1_Vm <- subset(Va_Vm_Oscheius_isolate_dataScale_CEW1, Model_Set =="Vm")
  
  Plot_Va_Vm_Oscheius_isolate_dataScale_CEW1_Vm <- ggplot(Va_Vm_Oscheius_isolate_dataScale_CEW1_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="deepskyblue", size=1.5, alpha=0.9) + 
    geom_point(fill="deepskyblue", shape=21,size=2)+
    theme_bw() +
    labs(title = "CEW1_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vm_Oscheius_isolate_dataScale_CEW1_Vm.pdf")
  Plot_Va_Vm_Oscheius_isolate_dataScale_CEW1_Vm
  dev.off()
  
  
  #Va_Vm_Oscheius_isolate_dataScale_JU178: all Pn.p                                                    
  
  Va_Vm_Oscheius_isolate_dataScale_JU178 <- subset(Va_Vm_Oscheius_isolate_dataScale, Ancestral =="JU178")
  
  Plot_Va_Vm_Oscheius_isolate_dataScale_JU178 <- ggplot(Va_Vm_Oscheius_isolate_dataScale_JU178, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_Set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue"))+
    geom_point(aes(fill=Model_Set), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "JU178_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")), x="Models")+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Oscheius_isolate_dataScale_JU178.pdf")
  Plot_Va_Vm_Oscheius_isolate_dataScale_JU178
  dev.off()
  
  Va_Vm_Oscheius_isolate_dataScale_JU178_Vm <- subset(Va_Vm_Oscheius_isolate_dataScale_JU178, Model_Set =="Vm")
  
  Plot_Va_Vm_Oscheius_isolate_dataScale_JU178_Vm <- ggplot(Va_Vm_Oscheius_isolate_dataScale_JU178_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="deepskyblue", size=1.5, alpha=0.9) + 
    geom_point(fill="deepskyblue", shape=21,size=2)+
    theme_bw() +
    labs(title = "JU178_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vm_Oscheius_isolate_dataScale_JU178_Vm.pdf")
  Plot_Va_Vm_Oscheius_isolate_dataScale_JU178_Vm
  dev.off()
  
  #Va_Vm_Oscheius_isolate_dataScale_JU77: all Pn.p                                                    
  
  Va_Vm_Oscheius_isolate_dataScale_JU77 <- subset(Va_Vm_Oscheius_isolate_dataScale, Ancestral =="JU77")
  
  Plot_Va_Vm_Oscheius_isolate_dataScale_JU77 <- ggplot(Va_Vm_Oscheius_isolate_dataScale_JU77, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_Set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("skyblue4","turquoise3","turquoise"))+
    geom_point(aes(fill=Model_Set), shape=21,size=2)+ scale_fill_manual(values = c("skyblue4","turquoise3","turquoise"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "JU77_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")), x="Models")+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Oscheius_isolate_dataScale_JU77.pdf")
  Plot_Va_Vm_Oscheius_isolate_dataScale_JU77
  dev.off()
  
  Va_Vm_Oscheius_isolate_dataScale_JU77_Vm <- subset(Va_Vm_Oscheius_isolate_dataScale_JU77, Model_Set =="Vm")
  
  Plot_Va_Vm_Oscheius_isolate_dataScale_JU77_Vm <- ggplot(Va_Vm_Oscheius_isolate_dataScale_JU77_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="turquoise", size=1.5, alpha=0.9) + 
    geom_point(fill="turquoise", shape=21,size=2)+
    theme_bw() +
    labs(title = "JU77_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vm_Oscheius_isolate_dataScale_JU77_Vm.pdf")
  Plot_Va_Vm_Oscheius_isolate_dataScale_JU77_Vm
  dev.off()
  
  #Va_Vm_Oscheius_isolate_dataScale_PS2068: all Pn.p                                                    
  
  Va_Vm_Oscheius_isolate_dataScale_PS2068 <- subset(Va_Vm_Oscheius_isolate_dataScale, Ancestral =="PS2068")
  
  Plot_Va_Vm_Oscheius_isolate_dataScale_PS2068 <- ggplot(Va_Vm_Oscheius_isolate_dataScale_PS2068, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_Set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("skyblue4","turquoise3","turquoise"))+
    geom_point(aes(fill=Model_Set), shape=21,size=2)+ scale_fill_manual(values = c("skyblue4","turquoise3","turquoise"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "PS2068_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")), x="Models")+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Oscheius_isolate_dataScale_PS2068.pdf")
  Plot_Va_Vm_Oscheius_isolate_dataScale_PS2068
  dev.off()
  
  Va_Vm_Oscheius_isolate_dataScale_PS2068_Vm <- subset(Va_Vm_Oscheius_isolate_dataScale_PS2068, Model_Set =="Vm")
  
  Plot_Va_Vm_Oscheius_isolate_dataScale_PS2068_Vm <- ggplot(Va_Vm_Oscheius_isolate_dataScale_PS2068_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="turquoise", size=1.5, alpha=0.9) + 
    geom_point(fill="turquoise", shape=21,size=2)+
    theme_bw() +
    labs(title = "PS2068_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vm_Oscheius_isolate_dataScale_PS2068_Vm.pdf")
  Plot_Va_Vm_Oscheius_isolate_dataScale_PS2068_Vm
  dev.off()
  
  
  ###Plots_H2_Vm_Oscheius_isolate_dataScale----
  
  H2_Vm_Oscheius_isolate_dataScale <- subset(Vm_Oscheius_isolate_SSSS_Summary, Scale =="data"& Measure=="H2")
  View(H2_Vm_Oscheius_isolate_dataScale)
  
  H2_Vm_Oscheius_isolate_dataScale$Ancestral <- factor(H2_Vm_Oscheius_isolate_dataScale$Ancestral, levels = c("CEW1","JU178", "PS2068","JU77") )
  factor(H2_Vm_Oscheius_isolate_dataScale$Ancestral)
  
  H2_Vm_Oscheius_isolate_dataScale$Species <- factor(H2_Vm_Oscheius_isolate_dataScale$Species, levels = c("O.tipulae","O.onirici") )
  factor(H2_Vm_Oscheius_isolate_dataScale$Species)
  
  factor(H2_Vm_Oscheius_isolate_dataScale$Model_Set)
  
  #H2_Vm_Oscheius_isolate_dataScale: all Pn.p                                                    
  Plot_H2_Vm_Oscheius_isolate_dataScale <- ggplot(H2_Vm_Oscheius_isolate_dataScale, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise3","turquoise"))+
    facet_grid(Ancestral~Pnp, scales="free")+ theme_bw() +
    labs(title = "Oscheius_isolate_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")), x="Models")+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Oscheius_isolate_dataScale.pdf")
  Plot_H2_Vm_Oscheius_isolate_dataScale
  dev.off()
  
  #H2_Vm_Oscheius_isolate_dataScale: without P4.p
  H2_Vm_Oscheius_isolate_dataScale_woutP4p <- subset(H2_Vm_Oscheius_isolate_dataScale, Pnp !="P4.p")
  View(H2_Vm_Oscheius_isolate_dataScale_woutP4p)
  
  Plot_H2_Vm_Oscheius_isolate_dataScale_woutP4p <- ggplot(H2_Vm_Oscheius_isolate_dataScale_woutP4p, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise3","turquoise"))+
    facet_grid(Ancestral~Pnp, scales="free")+ theme_bw() +
    labs(title = "Oscheius_isolate_mutational heritability_woutP4p", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")), x="Models")+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Oscheius_isolate_dataScale_woutP4p.pdf")
  Plot_H2_Vm_Oscheius_isolate_dataScale_woutP4p
  dev.off()               
  
  
  #H2_Vm_Oscheius_isolate_dataScale: all Pn.p in log10 scale
  Plot_H2_Vm_Oscheius_isolate_dataScale_log10 <- ggplot(H2_Vm_Oscheius_isolate_dataScale, aes( x=Model_Set ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    facet_grid(Ancestral~Pnp)+ theme_bw() +
    labs(title = "Oscheius_isolate_mutational heritability log10 scale", y =expression(paste(log[10] (italic(H[M]^{2})))), x="Models")+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Oscheius_isolate_dataScale_log10.pdf")
  Plot_H2_Vm_Oscheius_isolate_dataScale_log10
  dev.off()
  
  #H2_Vm_Oscheius_isolate_dataScale: all Pn.p in log10 scale only Vm
  H2_Vm_Oscheius_isolate_dataScale_Vm <- subset(H2_Vm_Oscheius_isolate_dataScale, Model_Set =="Vm")
  
  Plot_H2_Vm_Oscheius_isolate_dataScale_Vm_log10 <- ggplot(H2_Vm_Oscheius_isolate_dataScale_Vm, aes( x=Pnp ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("deepskyblue","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("deepskyblue","turquoise"))+
    facet_grid(rows=vars(Ancestral))+ theme_bw() +
    labs(title = "Oscheius_isolate_mutational heritability log10 scale", y =expression(paste(log[10] (italic(H[M]^{2})))),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Oscheius_isolate_dataScale_Vm_log10.pdf")
  Plot_H2_Vm_Oscheius_isolate_dataScale_Vm_log10
  dev.off()
  
  #H2_Vm_Oscheius_isolate_dataScale: all Pn.p only Vm
  
  Plot_H2_Vm_Oscheius_isolate_dataScale_Vm <- ggplot(H2_Vm_Oscheius_isolate_dataScale_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("deepskyblue","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("deepskyblue","turquoise"))+
    facet_grid(rows=vars(Ancestral), scales="free")+ theme_bw() +
    labs(title = "Oscheius_isolate_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Oscheius_isolate_dataScale_Vm.pdf")
  Plot_H2_Vm_Oscheius_isolate_dataScale_Vm
  dev.off()
  
  
  
  #H2_Vm_Oscheius_isolate_dataScale_CEW1: all Pn.p                                                    
  
  H2_Vm_Oscheius_isolate_dataScale_CEW1 <- subset(H2_Vm_Oscheius_isolate_dataScale, Ancestral =="CEW1")
  
  Plot_H2_Vm_Oscheius_isolate_dataScale_CEW1 <- ggplot(H2_Vm_Oscheius_isolate_dataScale_CEW1, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_Set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue"))+
    geom_point(aes(fill=Model_Set), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "CEW1_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")), x="Models")+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Oscheius_isolate_dataScale_CEW1.pdf")
  Plot_H2_Vm_Oscheius_isolate_dataScale_CEW1
  dev.off()
  
  H2_Vm_Oscheius_isolate_dataScale_CEW1_Vm <- subset(H2_Vm_Oscheius_isolate_dataScale_CEW1, Model_Set =="Vm")
  
  Plot_H2_Vm_Oscheius_isolate_dataScale_CEW1_Vm <- ggplot(H2_Vm_Oscheius_isolate_dataScale_CEW1_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="deepskyblue", size=1.5, alpha=0.9) + 
    geom_point(fill="deepskyblue", shape=21,size=2)+
    theme_bw() +
    labs(title = "CEW1_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Oscheius_isolate_dataScale_CEW1_Vm.pdf")
  Plot_H2_Vm_Oscheius_isolate_dataScale_CEW1_Vm
  dev.off()
  
  
  #H2_Vm_Oscheius_isolate_dataScale_JU178: all Pn.p                                                    
  
  H2_Vm_Oscheius_isolate_dataScale_JU178 <- subset(H2_Vm_Oscheius_isolate_dataScale, Ancestral =="JU178")
  
  Plot_H2_Vm_Oscheius_isolate_dataScale_JU178 <- ggplot(H2_Vm_Oscheius_isolate_dataScale_JU178, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_Set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue"))+
    geom_point(aes(fill=Model_Set), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "JU178_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")), x="Models")+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Oscheius_isolate_dataScale_JU178.pdf")
  Plot_H2_Vm_Oscheius_isolate_dataScale_JU178
  dev.off()
  
  H2_Vm_Oscheius_isolate_dataScale_JU178_Vm <- subset(H2_Vm_Oscheius_isolate_dataScale_JU178, Model_Set =="Vm")
  
  Plot_H2_Vm_Oscheius_isolate_dataScale_JU178_Vm <- ggplot(H2_Vm_Oscheius_isolate_dataScale_JU178_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="deepskyblue", size=1.5, alpha=0.9) + 
    geom_point(fill="deepskyblue", shape=21,size=2)+
    theme_bw() +
    labs(title = "JU178_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Oscheius_isolate_dataScale_JU178_Vm.pdf")
  Plot_H2_Vm_Oscheius_isolate_dataScale_JU178_Vm
  dev.off()
  
  #H2_Vm_Oscheius_isolate_dataScale_JU77: all Pn.p                                                    
  
  H2_Vm_Oscheius_isolate_dataScale_JU77 <- subset(H2_Vm_Oscheius_isolate_dataScale, Ancestral =="JU77")
  
  Plot_H2_Vm_Oscheius_isolate_dataScale_JU77 <- ggplot(H2_Vm_Oscheius_isolate_dataScale_JU77, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_Set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("skyblue4","turquoise3","turquoise"))+
    geom_point(aes(fill=Model_Set), shape=21,size=2)+ scale_fill_manual(values = c("skyblue4","turquoise3","turquoise"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "JU77_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")), x="Models")+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Oscheius_isolate_dataScale_JU77.pdf")
  Plot_H2_Vm_Oscheius_isolate_dataScale_JU77
  dev.off()
  
  H2_Vm_Oscheius_isolate_dataScale_JU77_Vm <- subset(H2_Vm_Oscheius_isolate_dataScale_JU77, Model_Set =="Vm")
  
  Plot_H2_Vm_Oscheius_isolate_dataScale_JU77_Vm <- ggplot(H2_Vm_Oscheius_isolate_dataScale_JU77_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="turquoise", size=1.5, alpha=0.9) + 
    geom_point(fill="turquoise", shape=21,size=2)+
    theme_bw() +
    labs(title = "JU77_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Oscheius_isolate_dataScale_JU77_Vm.pdf")
  Plot_H2_Vm_Oscheius_isolate_dataScale_JU77_Vm
  dev.off()
  
  #H2_Vm_Oscheius_isolate_dataScale_PS2068: all Pn.p                                                    
  
  H2_Vm_Oscheius_isolate_dataScale_PS2068 <- subset(H2_Vm_Oscheius_isolate_dataScale, Ancestral =="PS2068")
  
  Plot_H2_Vm_Oscheius_isolate_dataScale_PS2068 <- ggplot(H2_Vm_Oscheius_isolate_dataScale_PS2068, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_Set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("skyblue4","turquoise3","turquoise"))+
    geom_point(aes(fill=Model_Set), shape=21,size=2)+ scale_fill_manual(values = c("skyblue4","turquoise3","turquoise"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "PS2068_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")), x="Models")+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Oscheius_isolate_dataScale_PS2068.pdf")
  Plot_H2_Vm_Oscheius_isolate_dataScale_PS2068
  dev.off()
  
  H2_Vm_Oscheius_isolate_dataScale_PS2068_Vm <- subset(H2_Vm_Oscheius_isolate_dataScale_PS2068, Model_Set =="Vm")
  
  Plot_H2_Vm_Oscheius_isolate_dataScale_PS2068_Vm <- ggplot(H2_Vm_Oscheius_isolate_dataScale_PS2068_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="turquoise", size=1.5, alpha=0.9) + 
    geom_point(fill="turquoise", shape=21,size=2)+
    theme_bw() +
    labs(title = "PS2068_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Oscheius_isolate_dataScale_PS2068_Vm.pdf")
  Plot_H2_Vm_Oscheius_isolate_dataScale_PS2068_Vm
  dev.off()
  
  
  
   
  
  ##Plots_Va_H2_Vm_Oscheius_species_dataScale_SSSS ----
  
  ###Plots_Va_Vm_Oscheius_species_dataScale_SSSS----
  ##
  Va_Vm_Oscheius_species_dataScale_SSSS <- subset(Vm_Oscheius_species_summary_SSSS_vulva, Scale =="data"& Measure=="Va")
  names(Va_Vm_Oscheius_species_dataScale_SSSS)
  table(Va_Vm_Oscheius_species_dataScale_SSSS$Species)
  
  Va_Vm_Oscheius_species_dataScale_SSSS$Species <- factor(Va_Vm_Oscheius_species_dataScale_SSSS$Species, levels = c("O.tipulae","O.onirici" ) )
  factor(Va_Vm_Oscheius_species_dataScale_SSSS$Species)
  
  #Va_Vm_Oscheius_species_dataScale_SSSS: all Pn.p                                                    
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS <- ggplot(Va_Vm_Oscheius_species_dataScale_SSSS, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise3","turquoise"))+
    facet_grid(Species~Pnp, scales="free")+ theme_bw() +
    labs(title = "Oscheius_species_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")"))) + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Oscheius_species_dataScale_SSSS.pdf")
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS
  dev.off()
  
  #Va_Vm_Oscheius_species_dataScale_SSSS: all Pn.p in log10 scale
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS_log10 <- ggplot(Va_Vm_Oscheius_species_dataScale_SSSS, aes( x=Model_Set ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise3","turquoise"))+
    facet_grid(Species~Pnp)+ theme_bw() +
    labs(title = "Oscheius_species_Vm log10 scale", y =expression(paste(log[10] (italic(V[M]))))) + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Oscheius_species_dataScale_SSSS_log10.pdf")
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS_log10
  dev.off()
  
  #Va_Vm_Oscheius_species_dataScale_SSSS: all Pn.p in log10 scale only Vm
  
  Va_Vm_Oscheius_species_dataScale_SSSS_Vm <- subset(Va_Vm_Oscheius_species_dataScale_SSSS, Model_Set =="Vm")
  
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Vm_log10 <- ggplot(Va_Vm_Oscheius_species_dataScale_SSSS_Vm, aes( x=Pnp ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("deepskyblue","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("deepskyblue","turquoise"))+
    facet_grid(rows=vars(Species))+ theme_bw() +
    labs(title = "Oscheius_species_Vm log10 scale_Vm", y =expression(paste(log[10] (italic(V[M])))),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Vm_log10.pdf")
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Vm_log10
  dev.off()
  
  #Va_Vm_Oscheius_species_dataScale_SSSS: all Pn.p  only Vm
  
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Vm <- ggplot(Va_Vm_Oscheius_species_dataScale_SSSS_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Species), size=1.5, alpha=0.9) + scale_colour_manual(values = c("deepskyblue","turquoise"))+
    geom_point(aes(fill=Species), shape=21,size=1.5)+ scale_fill_manual(values = c("deepskyblue","turquoise"))+
    facet_grid(rows=vars(Species), scales="free")+ theme_bw() +
    labs(title = "Oscheius_species_Vm_scale_Vm",y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Vm.pdf")
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Vm
  dev.off()
  
  
  #Va_Vm_Oscheius_species_dataScale_SSSS: without P4.p
  Va_Vm_Oscheius_species_dataScale_SSSS_woutP4p <- subset(Va_Vm_Oscheius_species_dataScale_SSSS, Pnp !="P4.p")
  View(Va_Vm_Oscheius_species_dataScale_SSSS_woutP4p)
  
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS_woutP4p <- ggplot(Va_Vm_Oscheius_species_dataScale_SSSS_woutP4p, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise3","turquoise"))+
    facet_grid(Species~Pnp)+ theme_bw() +
    labs(title = "Oscheius_species_Vm_woutP4p", y =expression(paste("Mutational Variance (",italic(V[M]),")"))) + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vm_Oscheius_species_dataScale_SSSS_woutP4p.pdf")
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS_woutP4p
  dev.off()
  
  
  #Va_Vm_Oscheius_species_dataScale_SSSS_Otipulae: all Pn.p                                                    
  
  Va_Vm_Oscheius_species_dataScale_SSSS_Otipulae <- subset(Va_Vm_Oscheius_species_dataScale_SSSS, Species =="O.tipulae")
  
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Otipulae <- ggplot(Va_Vm_Oscheius_species_dataScale_SSSS_Otipulae, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_Set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue"))+
    geom_point(aes(fill=Model_Set), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "Otipulae_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")"))) + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Otipulae.pdf")
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Otipulae
  dev.off()
  
  Va_Vm_Oscheius_species_dataScale_SSSS_Otipulae_Vm <- subset( Va_Vm_Oscheius_species_dataScale_SSSS_Otipulae, Model_Set =="Vm")
  
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Otipulae_Vm <- ggplot(Va_Vm_Oscheius_species_dataScale_SSSS_Otipulae_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="deepskyblue", size=1.5, alpha=0.9) +
    geom_point(fill="deepskyblue", shape=21,size=2)+
    theme_bw() +
    labs(title = "Otipulae_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +   theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Otipulae_Vm.pdf")
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Otipulae_Vm
  dev.off()
  
  
  
  #Va_Vm_Oscheius_species_dataScale_SSSS_Oonirici: all Pn.p                                                    
  
  Va_Vm_Oscheius_species_dataScale_SSSS_Oonirici <- subset(Va_Vm_Oscheius_species_dataScale_SSSS, Species =="O.onirici")
  
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Oonirici <- ggplot(Va_Vm_Oscheius_species_dataScale_SSSS_Oonirici, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_Set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("skyblue4","turquoise3","turquoise"))+
    geom_point(aes(fill=Model_Set), shape=21,size=2)+ scale_fill_manual(values = c("skyblue4","turquoise3","turquoise"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "Oonirici_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")"))) + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) +  theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Oonirici.pdf")
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Oonirici
  dev.off()
  
  Va_Vm_Oscheius_species_dataScale_SSSS_Oonirici_Vm <- subset( Va_Vm_Oscheius_species_dataScale_SSSS_Oonirici, Model_Set =="Vm")
  
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Oonirici_Vm <- ggplot(Va_Vm_Oscheius_species_dataScale_SSSS_Oonirici_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="turquoise", size=1.5, alpha=0.9) +
    geom_point(fill="turquoise", shape=21,size=2)+
    theme_bw() +
    labs(title = "Oonirici_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +   theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Oonirici_Vm.pdf")
  Plot_Va_Vm_Oscheius_species_dataScale_SSSS_Oonirici_Vm
  dev.off()
  
  ###Plots_H2_Vm_Oscheius_species_dataScale_SSSS----
  ##
  H2_Vm_Oscheius_species_dataScale_SSSS <- subset(Vm_Oscheius_species_summary_SSSS_vulva, Scale =="data"& Measure=="H2")
  names(H2_Vm_Oscheius_species_dataScale_SSSS)
  table(H2_Vm_Oscheius_species_dataScale_SSSS$Species)
  
  H2_Vm_Oscheius_species_dataScale_SSSS$Species <- factor(H2_Vm_Oscheius_species_dataScale_SSSS$Species, levels = c("O.tipulae","O.onirici" ) )
  factor(H2_Vm_Oscheius_species_dataScale_SSSS$Species)
  
  #H2_Vm_Oscheius_species_dataScale_SSSS: all Pn.p                                                    
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS <- ggplot(H2_Vm_Oscheius_species_dataScale_SSSS, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise3","turquoise"))+
    facet_grid(Species~Pnp, scales="free")+ theme_bw() +
    labs(title = "Oscheius_species_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")), x="Models") + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Oscheius_species_dataScale_SSSS.pdf")
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS
  dev.off()
  
  #H2_Vm_Oscheius_species_dataScale_SSSS: all Pn.p in log10 scale
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS_log10 <- ggplot(H2_Vm_Oscheius_species_dataScale_SSSS, aes( x=Model_Set ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise3","turquoise"))+
    facet_grid(Species~Pnp)+ theme_bw() +
    labs(title = "Oscheius_species_mutational heritability log10 scale", y =expression(paste(log[10] (italic(H[M]^{2})))), x="Models") + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Oscheius_species_dataScale_SSSS_log10.pdf")
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS_log10
  dev.off()
  
  #H2_Vm_Oscheius_species_dataScale_SSSS: all Pn.p in log10 scale only Vm
  
  H2_Vm_Oscheius_species_dataScale_SSSS_Vm <- subset(H2_Vm_Oscheius_species_dataScale_SSSS, Model_Set =="Vm")
  
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Vm_log10 <- ggplot(H2_Vm_Oscheius_species_dataScale_SSSS_Vm, aes( x=Pnp ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("deepskyblue","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("deepskyblue","turquoise"))+
    facet_grid(rows=vars(Species))+ theme_bw() +
    labs(title = "Oscheius_species_mutational heritability log10 scale_mutational heritability", y =expression(paste(log[10] (italic(H[M]^{2})))),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text = element_text(size=10),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Vm_log10.pdf")
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Vm_log10
  dev.off()
  
  #H2_Vm_Oscheius_species_dataScale_SSSS: all Pn.p only Vm                                                 
  
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Vm <- ggplot(H2_Vm_Oscheius_species_dataScale_SSSS_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Species), size=1.5, alpha=0.9) + scale_colour_manual(values = c("deepskyblue","turquoise"))+
    geom_point(aes(fill=Species), shape=21,size=2)+ scale_fill_manual(values = c("deepskyblue","turquoise"))+
    facet_grid(rows=vars(Species), scales="free")+ theme_bw() +
    labs(title = "Oscheius_species_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Vm.pdf")
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Vm
  dev.off()
  
  #H2_Vm_Oscheius_species_dataScale_SSSS: without P4.p
  H2_Vm_Oscheius_species_dataScale_SSSS_woutP4p <- subset(H2_Vm_Oscheius_species_dataScale_SSSS, Pnp !="P4.p")
  View(H2_Vm_Oscheius_species_dataScale_SSSS_woutP4p)
  
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS_woutP4p <- ggplot(H2_Vm_Oscheius_species_dataScale_SSSS_woutP4p, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_Set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=interaction(Model_Set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise3","turquoise"))+
    facet_grid(Species~Pnp, scales="free")+ theme_bw() +
    labs(title = "Oscheius_species_mutational heritability_woutP4p", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")"))) + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Oscheius_species_dataScale_SSSS_woutP4p.pdf")
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS_woutP4p
  dev.off()
  
  
  #H2_Vm_Oscheius_species_dataScale_SSSS_Otipulae: all Pn.p                                                    
  
  H2_Vm_Oscheius_species_dataScale_SSSS_Otipulae <- subset(H2_Vm_Oscheius_species_dataScale_SSSS, Species =="O.tipulae")
  
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Otipulae <- ggplot(H2_Vm_Oscheius_species_dataScale_SSSS_Otipulae, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_Set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue"))+
    geom_point(aes(fill=Model_Set), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "Otipulae_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")"))) + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Otipulae.pdf")
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Otipulae
  dev.off()
  
  H2_Vm_Oscheius_species_dataScale_SSSS_Otipulae_Vm <- subset(H2_Vm_Oscheius_species_dataScale_SSSS_Otipulae, Model_Set =="Vm")
  
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Otipulae_Vm <- ggplot(H2_Vm_Oscheius_species_dataScale_SSSS_Otipulae_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="deepskyblue", size=1.5, alpha=0.9) +
    geom_point(fill="deepskyblue", shape=21,size=2)+ 
    theme_bw() +
    labs(title = "Otipulae_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Otipulae_Vm.pdf")
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Otipulae_Vm
  dev.off() 
  
  #H2_Vm_Oscheius_species_dataScale_SSSS_Oonirici: all Pn.p                                                    
  
  H2_Vm_Oscheius_species_dataScale_SSSS_Oonirici <- subset(H2_Vm_Oscheius_species_dataScale_SSSS, Species =="O.onirici")
  
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Oonirici <- ggplot(H2_Vm_Oscheius_species_dataScale_SSSS_Oonirici, aes( x=Model_Set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_Set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("skyblue4","turquoise3","turquoise"))+
    geom_point(aes(fill=Model_Set), shape=21,size=2)+ scale_fill_manual(values = c("skyblue4","turquoise3","turquoise"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "Oonirici_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")"))) + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Oonirici.pdf")
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Oonirici
  dev.off()
  
  H2_Vm_Oscheius_species_dataScale_SSSS_Oonirici_Vm <- subset(H2_Vm_Oscheius_species_dataScale_SSSS_Oonirici, Model_Set =="Vm")
  
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Oonirici_Vm <- ggplot(H2_Vm_Oscheius_species_dataScale_SSSS_Oonirici_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="turquoise", size=1.5, alpha=0.9) +
    geom_point(fill="turquoise", shape=21,size=2)+ 
    theme_bw() +
    labs(title = "Oonirici_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Oonirici_Vm.pdf")
  Plot_H2_Vm_Oscheius_species_dataScale_SSSS_Oonirici_Vm
  dev.off()

 






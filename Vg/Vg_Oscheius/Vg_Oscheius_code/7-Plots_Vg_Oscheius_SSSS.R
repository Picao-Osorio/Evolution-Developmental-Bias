#Plots_Vg_Oscheius_SSSS
library(readxl)
library(ggplot2)
library(ggdark)
library(ggtext) 
library(ggrepel)


View(Vg_Oscheius_summary_2) #from 4-Vg_Oscheius_summary_SSSS

Vg_Oscheius_summary_2$Species <- factor(Vg_Oscheius_summary_2$Species, levels = c("O.tipulae","O.onirici") )
factor(Vg_Oscheius_summary_2$Species)

Vg_Oscheius_summary_2$Model_set <- factor(Vg_Oscheius_summary_2$Model_set, levels = c("Control","WILD","Vg") )
factor(Vg_Oscheius_summary_2$Model_set)

#Plots_Va_H2_Evol_Vg_Oscheius_isolate_species----

{
  
  ##Plots_Va_H2_Evol_Vg_Oscheius_dataScale ----
  
  ###Plots_Va_Vg_Oscheius_dataScale_SSSS----
  ##
  Va_Vg_Oscheius_dataScale_SSSS <- subset(Vg_Oscheius_summary_2, Scale =="data"& Measure=="Va")
  names(Va_Vg_Oscheius_dataScale_SSSS)
  table(Va_Vg_Oscheius_dataScale_SSSS$Species)
  
  Va_Vg_Oscheius_dataScale_SSSS$Species <- factor(Va_Vg_Oscheius_dataScale_SSSS$Species, levels = c("O.tipulae","O.onirici" ) )
  factor(Va_Vg_Oscheius_dataScale_SSSS$Species)
  
  #Va_Vg_Oscheius_dataScale_SSSS: all Pn.p                                                    
  Plot_Va_Vg_Oscheius_dataScale_SSSS <- ggplot(Va_Vg_Oscheius_dataScale_SSSS, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    facet_grid(Species~Pnp, scales="free")+ theme_bw() +
    labs(title = "Oscheius_Vg", y =expression(paste("Standing Genetic Variance (",italic(V[G]),")")), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vg_Oscheius_dataScale_SSSS.pdf")
  Plot_Va_Vg_Oscheius_dataScale_SSSS
  dev.off()
  
  #Va_Vg_Oscheius_dataScale_SSSS: all Pn.p in log10 scale
  Plot_Va_Vg_Oscheius_dataScale_SSSS_log10 <- ggplot(Va_Vg_Oscheius_dataScale_SSSS, aes( x=Model_set ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    facet_grid(Species~Pnp)+ theme_bw() +
    labs(title = "Oscheius_Vg log10 scale", y =expression(paste("Standing Genetic Variance (",italic(V[G]),")")), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Oscheius_dataScale_SSSS_log10.pdf")
  Plot_Va_Vg_Oscheius_dataScale_SSSS_log10
  dev.off()
  
  
  
  #Va_Vg_Oscheius_dataScale_SSSS: all Pn.p  only Vg
  
  Va_Vg_Oscheius_dataScale_SSSS_Vg <- subset(Va_Vg_Oscheius_dataScale_SSSS, Model_set =="Vg")
  
  
  Plot_Va_Vg_Oscheius_dataScale_SSSS_Vg <- ggplot(Va_Vg_Oscheius_dataScale_SSSS_Vg, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Species), size=1.5, alpha=0.9) + scale_colour_manual(values = c("deepskyblue","turquoise"))+
    geom_point(aes(fill=Species), shape=21,size=1.5)+ scale_fill_manual(values = c("deepskyblue","turquoise"))+
    facet_grid(rows=vars(Species))+ theme_bw() +
    labs(title = "Oscheius_Vg log10 scale_Vg",y =expression(paste("Standing Genetic Variance (",italic(V[G]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Oscheius_dataScale_SSSS_Vg.pdf")
  Plot_Va_Vg_Oscheius_dataScale_SSSS_Vg
  dev.off()
  
  #Va_Vg_Oscheius_dataScale_SSSS: all Pn.p in log10 scale only Vg
  
  
  Plot_Va_Vg_Oscheius_dataScale_SSSS_Vg_log10 <- ggplot(Va_Vg_Oscheius_dataScale_SSSS_Vg, aes( x=Pnp ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("deepskyblue","turquoise"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("deepskyblue","turquoise"))+
    facet_grid(rows=vars(Species))+ theme_bw() +
    labs(title = "Oscheius_Vg log10 scale_Vg", y =expression(paste(log[10] (italic(V[G])))),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Oscheius_dataScale_SSSS_Vg_log10.pdf")
  Plot_Va_Vg_Oscheius_dataScale_SSSS_Vg_log10
  dev.off()
  
  
  #Va_Vg_Oscheius_dataScale_SSSS_Otipulae: all Pn.p                                                    
  
  Va_Vg_Oscheius_dataScale_SSSS_Otipulae <- subset(Va_Vg_Oscheius_dataScale_SSSS, Species =="O.tipulae")
  
  Plot_Va_Vg_Oscheius_dataScale_SSSS_Otipulae <- ggplot(Va_Vg_Oscheius_dataScale_SSSS_Otipulae, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "Otipulae_Vg", y =expression(paste("Standing Genetic Variance (",italic(V[G]),")")), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G])))  + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Oscheius_dataScale_SSSS_Otipulae.pdf")
  Plot_Va_Vg_Oscheius_dataScale_SSSS_Otipulae
  dev.off()
  
  Va_Vg_Oscheius_dataScale_SSSS_Otipulae_Vg <- subset( Va_Vg_Oscheius_dataScale_SSSS_Otipulae, Model_set =="Vg")
  
  Plot_Va_Vg_Oscheius_dataScale_SSSS_Otipulae_Vg <- ggplot(Va_Vg_Oscheius_dataScale_SSSS_Otipulae_Vg, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="deepskyblue", size=1.5, alpha=0.9) +
    geom_point(fill="deepskyblue", shape=21,size=2)+
    theme_bw() +
    labs(title = "Otipulae_Vg", y =expression(paste("Standing Genetic Variance (",italic(V[G]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +   theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Oscheius_dataScale_SSSS_Otipulae_Vg.pdf")
  Plot_Va_Vg_Oscheius_dataScale_SSSS_Otipulae_Vg
  dev.off()
  
  
  Plot_Va_Vg_Oscheius_dataScale_SSSS_Otipulae_Vg_log10 <- ggplot(Va_Vg_Oscheius_dataScale_SSSS_Otipulae_Vg, aes( x=Pnp ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83)), color="deepskyblue", size=1.5, alpha=0.9) +
    geom_point(fill="deepskyblue", shape=21,size=2)+
    theme_bw() +
    labs(title = "Otipulae_Vg log10", y =expression(paste(log[10] (italic(V[G])))),x="Pn.p")  + scale_x_discrete(labels=c("3","4","5","6","7","8")) +   theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Oscheius_dataScale_SSSS_Otipulae_Vg_log10.pdf")
  Plot_Va_Vg_Oscheius_dataScale_SSSS_Otipulae_Vg_log10
  dev.off()
  
  
  
  
  #Va_Vg_Oscheius_dataScale_SSSS_Oonirici: all Pn.p                                                    
  
  Va_Vg_Oscheius_dataScale_SSSS_Oonirici <- subset(Va_Vg_Oscheius_dataScale_SSSS, Species =="O.onirici")
  
  Plot_Va_Vg_Oscheius_dataScale_SSSS_Oonirici <- ggplot(Va_Vg_Oscheius_dataScale_SSSS_Oonirici, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("skyblue4","turquoise4","turquoise"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "Oonirici_Vg", y =expression(paste("Standing Genetic Variance (",italic(V[G]),")")), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Oscheius_dataScale_SSSS_Oonirici.pdf")
  Plot_Va_Vg_Oscheius_dataScale_SSSS_Oonirici
  dev.off()
  
  Va_Vg_Oscheius_dataScale_SSSS_Oonirici_Vg <- subset( Va_Vg_Oscheius_dataScale_SSSS_Oonirici, Model_set =="Vg")
  
  Plot_Va_Vg_Oscheius_dataScale_SSSS_Oonirici_Vg <- ggplot(Va_Vg_Oscheius_dataScale_SSSS_Oonirici_Vg, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="turquoise", size=1.5, alpha=0.9) +
    geom_point(fill="turquoise", shape=21,size=2)+
    theme_bw() +
    labs(title = "Oonirici_Vg", y =expression(paste("Standing Genetic Variance (",italic(V[G]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +   theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Oscheius_dataScale_SSSS_Oonirici_Vg.pdf")
  Plot_Va_Vg_Oscheius_dataScale_SSSS_Oonirici_Vg
  dev.off()
  
  Plot_Va_Vg_Oscheius_dataScale_SSSS_Oonirici_Vg_log10 <- ggplot(Va_Vg_Oscheius_dataScale_SSSS_Oonirici_Vg, aes( x=Pnp ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83)), color="turquoise", size=1.5, alpha=0.9) +
    geom_point(fill="turquoise", shape=21,size=2)+
    theme_bw() +
    labs(title = "Oonirici_Vg log10", y =expression(paste(log[10] (italic(V[G])))),x="Pn.p")  + scale_x_discrete(labels=c("3","4","5","6","7","8")) +   theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Oscheius_dataScale_SSSS_Oonirici_Vg_log10.pdf")
  Plot_Va_Vg_Oscheius_dataScale_SSSS_Oonirici_Vg_log10
  dev.off()
  
  
  ###Plots_H2_Vg_Oscheius_dataScale----
  ##
  H2_Vg_Oscheius_dataScale <- subset(Vg_Oscheius_summary_2, Scale =="data"& Measure=="H2")
  names(H2_Vg_Oscheius_dataScale)
  table(H2_Vg_Oscheius_dataScale$Species)
  
  H2_Vg_Oscheius_dataScale$Species <- factor(H2_Vg_Oscheius_dataScale$Species, levels = c("O.tipulae","O.onirici" ) )
  factor(H2_Vg_Oscheius_dataScale$Species)
  
  #H2_Vg_Oscheius_dataScale: all Pn.p                                                    
  Plot_H2_Vg_Oscheius_dataScale <- ggplot(H2_Vg_Oscheius_dataScale, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    facet_grid(Species~Pnp, scales="free")+ theme_bw() +
    labs(title = "Oscheius_heritability", y =expression(paste("Heritability (",italic(H[G]^{2}),")")), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vg_Oscheius_dataScale_SSSS.pdf")
  Plot_H2_Vg_Oscheius_dataScale
  dev.off()
  
  #H2_Vg_Oscheius_dataScale: all Pn.p in log10 scale
  Plot_H2_Vg_Oscheius_dataScale_log10 <- ggplot(H2_Vg_Oscheius_dataScale, aes( x=Model_set ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue","skyblue4","turquoise4","turquoise"))+
    facet_grid(Species~Pnp)+ theme_bw() +
    labs(title = "Oscheius_heritability log10 scale", y =expression(paste(log[10] (italic(H[M]^{2})))), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vg_Oscheius_dataScale_log10.pdf")
  Plot_H2_Vg_Oscheius_dataScale_log10
  dev.off()
  
  #H2_Vg_Oscheius_dataScale: all Pn.p in log10 scale only Vg
  
  H2_Vg_Oscheius_dataScale_Vg <- subset(H2_Vg_Oscheius_dataScale, Model_set =="Vg")
  
  Plot_H2_Vg_Oscheius_dataScale_Vg_log10 <- ggplot(H2_Vg_Oscheius_dataScale_Vg, aes( x=Pnp ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("deepskyblue","turquoise"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("deepskyblue","turquoise"))+
    facet_grid(rows=vars(Species))+ theme_bw() +
    labs(title = "Oscheius_heritability log10 scale_heritability", y =expression(paste(log[10] (italic(H[M]^{2})))),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text = element_text(size=10),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vg_Oscheius_dataScale_Vg_log10.pdf")
  Plot_H2_Vg_Oscheius_dataScale_Vg_log10
  dev.off()
  
  #H2_Vg_Oscheius_dataScale: all Pn.p only Vg                                                 
  
  Plot_H2_Vg_Oscheius_dataScale_Vg <- ggplot(H2_Vg_Oscheius_dataScale_Vg, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Species), size=1.5, alpha=0.9) + scale_colour_manual(values = c("deepskyblue","turquoise"))+
    geom_point(aes(fill=Species), shape=21,size=2)+ scale_fill_manual(values = c("deepskyblue","turquoise"))+
    facet_grid(rows=vars(Species), scales="free")+ theme_bw() +
    labs(title = "Oscheius_heritability", y =expression(paste("Heritability (",italic(H[G]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vg_Oscheius_dataScale_Vg.pdf")
  Plot_H2_Vg_Oscheius_dataScale_Vg
  dev.off()
  
 
  
  
  #H2_Vg_Oscheius_dataScale_Otipulae: all Pn.p                                                    
  
  H2_Vg_Oscheius_dataScale_Otipulae <- subset(H2_Vg_Oscheius_dataScale, Species =="O.tipulae")
  
  Plot_H2_Vg_Oscheius_dataScale_Otipulae <- ggplot(H2_Vg_Oscheius_dataScale_Otipulae, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("royalblue4","deepskyblue4","deepskyblue"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("royalblue4","deepskyblue4","deepskyblue"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "Otipulae_heritability", y =expression(paste("Heritability (",italic(H[G]^{2}),")")), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vg_Oscheius_dataScale_Otipulae.pdf")
  Plot_H2_Vg_Oscheius_dataScale_Otipulae
  dev.off()
  
  H2_Vg_Oscheius_dataScale_Otipulae_Vg <- subset(H2_Vg_Oscheius_dataScale_Otipulae, Model_set =="Vg")
  
  Plot_H2_Vg_Oscheius_dataScale_Otipulae_Vg <- ggplot(H2_Vg_Oscheius_dataScale_Otipulae_Vg, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="deepskyblue", size=1.5, alpha=0.9) +
    geom_point(fill="deepskyblue", shape=21,size=2)+ 
    theme_bw() +
    labs(title = "Otipulae_heritability", y =expression(paste("Heritability (",italic(H[G]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vg_Oscheius_dataScale_Otipulae_Vg.pdf")
  Plot_H2_Vg_Oscheius_dataScale_Otipulae_Vg
  dev.off() 
  
  #H2_Vg_Oscheius_dataScale_Oonirici: all Pn.p                                                    
  
  H2_Vg_Oscheius_dataScale_Oonirici <- subset(H2_Vg_Oscheius_dataScale, Species =="O.onirici")
  
  Plot_H2_Vg_Oscheius_dataScale_Oonirici <- ggplot(H2_Vg_Oscheius_dataScale_Oonirici, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("skyblue4","turquoise4","turquoise"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("skyblue4","turquoise4","turquoise"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "Oonirici_heritability", y =expression(paste("Heritability (",italic(H[G]^{2}),")")), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vg_Oscheius_dataScale_Oonirici.pdf")
  Plot_H2_Vg_Oscheius_dataScale_Oonirici
  dev.off()
  
  H2_Vg_Oscheius_dataScale_Oonirici_Vg <- subset(H2_Vg_Oscheius_dataScale_Oonirici, Model_set =="Vg")
  
  Plot_H2_Vg_Oscheius_dataScale_Oonirici_Vg <- ggplot(H2_Vg_Oscheius_dataScale_Oonirici_Vg, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="turquoise", size=1.5, alpha=0.9) +
    geom_point(fill="turquoise", shape=21,size=2)+ 
    theme_bw() +
    labs(title = "Oonirici_heritability", y =expression(paste("Heritability (",italic(H[G]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vg_Oscheius_dataScale_Oonirici_Vg.pdf")
  Plot_H2_Vg_Oscheius_dataScale_Oonirici_Vg
  dev.off()
  
  





#Plots_Vg_Caenorhabditis_SS
library(readxl)
library(ggplot2)
library(ggdark)
library(ggtext) 
library(ggrepel)


View(Vg_Caenorhabditis_summary_SS_vulva) #from 4-Vg_Caenorhabditis_summary_SS_vulva.R

Vg_Caenorhabditis_summary_SS_vulva$Species <- factor(Vg_Caenorhabditis_summary_SS_vulva$Species, levels = c("C.elegans","C.briggsae") )
factor(Vg_Caenorhabditis_summary_SS_vulva$Species)

Vg_Caenorhabditis_summary_SS_vulva$Model_set <- factor(Vg_Caenorhabditis_summary_SS_vulva$Model_set, levels = c("Control","WILD","Vg") )
factor(Vg_Caenorhabditis_summary_SS_vulva$Model_set)

#Plots_Va_H2_Evol_Vg_Caenorhabditis_isolate_species----

{
  
  ##Plots_Va_H2_Evol_Vg_Caenorhabditis_dataScale ----
  
  ###Plots_Va_Vg_Caenorhabditis_dataScale_SS----
  ##
  Va_Vg_Caenorhabditis_dataScale_SS <- subset(Vg_Caenorhabditis_summary_SS_vulva, Scale =="data"& Measure=="Va")
  names(Va_Vg_Caenorhabditis_dataScale_SS)
  table(Va_Vg_Caenorhabditis_dataScale_SS$Species)
  
  Va_Vg_Caenorhabditis_dataScale_SS$Species <- factor(Va_Vg_Caenorhabditis_dataScale_SS$Species, levels = c("C.elegans","C.briggsae" ) )
  factor(Va_Vg_Caenorhabditis_dataScale_SS$Species)
  
  #Va_Vg_Caenorhabditis_dataScale_SS: all Pn.p                                                    
  Plot_Va_Vg_Caenorhabditis_dataScale_SS <- ggplot(Va_Vg_Caenorhabditis_dataScale_SS, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink3","deeppink"))+
    facet_grid(Species~Pnp, scales="free")+ theme_bw() +
    labs(title = "Caenorhabditis_Vg", y =expression(paste("Standing Genetic Variance (",italic(V[G]),")")), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vg_Caenorhabditis_dataScale_SS.pdf")
  Plot_Va_Vg_Caenorhabditis_dataScale_SS
  dev.off()
  
  #Va_Vg_Caenorhabditis_dataScale_SS: all Pn.p in log10 scale
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_log10 <- ggplot(Va_Vg_Caenorhabditis_dataScale_SS, aes( x=Model_set ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink3","deeppink"))+
    facet_grid(Species~Pnp)+ theme_bw() +
    labs(title = "Caenorhabditis_Vg log10 scale", y =expression(paste("Standing Genetic Variance (",italic(V[G]),")")), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Caenorhabditis_dataScale_SS_log10.pdf")
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_log10
  dev.off()
  
  
  
  #Va_Vg_Caenorhabditis_dataScale_SS: all Pn.p  only Vg
  
  Va_Vg_Caenorhabditis_dataScale_SS_Vg <- subset(Va_Vg_Caenorhabditis_dataScale_SS, Model_set =="Vg")
  
  
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_Vg <- ggplot(Va_Vg_Caenorhabditis_dataScale_SS_Vg, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Species), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red","deeppink"))+
    geom_point(aes(fill=Species), shape=21,size=1.5)+ scale_fill_manual(values = c("red","deeppink"))+
    facet_grid(rows=vars(Species))+ theme_bw() +
    labs(title = "Caenorhabditis_Vg log10 scale_Vg",y =expression(paste("Standing Genetic Variance (",italic(V[G]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Caenorhabditis_dataScale_SS_Vg.pdf")
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_Vg
  dev.off()
  
  #Va_Vg_Caenorhabditis_dataScale_SS: all Pn.p in log10 scale only Vg
  
  
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_Vg_log10 <- ggplot(Va_Vg_Caenorhabditis_dataScale_SS_Vg, aes( x=Pnp ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("red","deeppink"))+
    facet_grid(rows=vars(Species))+ theme_bw() +
    labs(title = "Caenorhabditis_Vg log10 scale_Vg", y =expression(paste(log[10] (italic(V[G])))),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Caenorhabditis_dataScale_SS_Vg_log10.pdf")
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_Vg_log10
  dev.off()
  
  #Va_Vg_Caenorhabditis_dataScale_SS: without P3.p
  Va_Vg_Caenorhabditis_dataScale_SS_woutP3p <- subset(Va_Vg_Caenorhabditis_dataScale_SS, Pnp !="P3.p")
  View(Va_Vg_Caenorhabditis_dataScale_SS_woutP3p)
  
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_woutP3p <- ggplot(Va_Vg_Caenorhabditis_dataScale_SS_woutP3p, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink3","deeppink"))+
    facet_grid(Species~Pnp, scales = "free")+ theme_bw() +
    labs(title = "Caenorhabditis_Vg_woutP3p", y =expression(paste("Standing Genetic Variance (",italic(V[G]),")")), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G])))  + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vg_Caenorhabditis_dataScale_SS_woutP3p.pdf")
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_woutP3p
  dev.off()
  
  
  #Va_Vg_Caenorhabditis_dataScale_SS_Celegans: all Pn.p                                                    
  
  Va_Vg_Caenorhabditis_dataScale_SS_Celegans <- subset(Va_Vg_Caenorhabditis_dataScale_SS, Species =="C.elegans")
  
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_Celegans <- ggplot(Va_Vg_Caenorhabditis_dataScale_SS_Celegans, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "Celegans_Vg", y =expression(paste("Standing Genetic Variance (",italic(V[G]),")")), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G])))  + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Caenorhabditis_dataScale_SS_Celegans.pdf")
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_Celegans
  dev.off()
  
  Va_Vg_Caenorhabditis_dataScale_SS_Celegans_Vg <- subset( Va_Vg_Caenorhabditis_dataScale_SS_Celegans, Model_set =="Vg")
  
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_Celegans_Vg <- ggplot(Va_Vg_Caenorhabditis_dataScale_SS_Celegans_Vg, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="red", size=1.5, alpha=0.9) +
    geom_point(fill="red", shape=21,size=2)+
    theme_bw() +
    labs(title = "Celegans_Vg", y =expression(paste("Standing Genetic Variance (",italic(V[G]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +   theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Caenorhabditis_dataScale_SS_Celegans_Vg.pdf")
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_Celegans_Vg
  dev.off()
  
  
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_Celegans_Vg_log10 <- ggplot(Va_Vg_Caenorhabditis_dataScale_SS_Celegans_Vg, aes( x=Pnp ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83)), color="red", size=1.5, alpha=0.9) +
    geom_point(fill="red", shape=21,size=2)+
    theme_bw() +
    labs(title = "Celegans_Vg log10", y =expression(paste(log[10] (italic(V[G])))),x="Pn.p")  + scale_x_discrete(labels=c("3","4","5","6","7","8")) +   theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Caenorhabditis_dataScale_SS_Celegans_Vg_log10.pdf")
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_Celegans_Vg_log10
  dev.off()
  
  
  
 
  #Va_Vg_Caenorhabditis_dataScale_SS_Cbriggsae: all Pn.p                                                    
  
  Va_Vg_Caenorhabditis_dataScale_SS_Cbriggsae <- subset(Va_Vg_Caenorhabditis_dataScale_SS, Species =="C.briggsae")
  
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_Cbriggsae <- ggplot(Va_Vg_Caenorhabditis_dataScale_SS_Cbriggsae, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("pink4","deeppink3","deeppink"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("pink4","deeppink3","deeppink"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "Cbriggsae_Vg", y =expression(paste("Standing Genetic Variance (",italic(V[G]),")")), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Caenorhabditis_dataScale_SS_Cbriggsae.pdf")
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_Cbriggsae
  dev.off()
  
  Va_Vg_Caenorhabditis_dataScale_SS_Cbriggsae_Vg <- subset( Va_Vg_Caenorhabditis_dataScale_SS_Cbriggsae, Model_set =="Vg")
  
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_Cbriggsae_Vg <- ggplot(Va_Vg_Caenorhabditis_dataScale_SS_Cbriggsae_Vg, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="deeppink", size=1.5, alpha=0.9) +
    geom_point(fill="deeppink", shape=21,size=2)+
    theme_bw() +
    labs(title = "Cbriggsae_Vg", y =expression(paste("Standing Genetic Variance (",italic(V[G]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +   theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Caenorhabditis_dataScale_SS_Cbriggsae_Vg.pdf")
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_Cbriggsae_Vg
  dev.off()
  
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_Cbriggsae_Vg_log10 <- ggplot(Va_Vg_Caenorhabditis_dataScale_SS_Cbriggsae_Vg, aes( x=Pnp ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83)), color="deeppink", size=1.5, alpha=0.9) +
    geom_point(fill="deeppink", shape=21,size=2)+
    theme_bw() +
    labs(title = "Cbriggsae_Vg log10", y =expression(paste(log[10] (italic(V[G])))),x="Pn.p")  + scale_x_discrete(labels=c("3","4","5","6","7","8")) +   theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vg_Caenorhabditis_dataScale_SS_Cbriggsae_Vg_log10.pdf")
  Plot_Va_Vg_Caenorhabditis_dataScale_SS_Cbriggsae_Vg_log10
  dev.off()
  
  
  ###Plots_H2_Vg_Caenorhabditis_dataScale----
  ##
  H2_Vg_Caenorhabditis_dataScale <- subset(Vg_Caenorhabditis_summary_SS_vulva, Scale =="data"& Measure=="H2")
  names(H2_Vg_Caenorhabditis_dataScale)
  table(H2_Vg_Caenorhabditis_dataScale$Species)
  
  H2_Vg_Caenorhabditis_dataScale$Species <- factor(H2_Vg_Caenorhabditis_dataScale$Species, levels = c("C.elegans","C.briggsae" ) )
  factor(H2_Vg_Caenorhabditis_dataScale$Species)
  
  #H2_Vg_Caenorhabditis_dataScale: all Pn.p                                                    
  Plot_H2_Vg_Caenorhabditis_dataScale <- ggplot(H2_Vg_Caenorhabditis_dataScale, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink3","deeppink"))+
    facet_grid(Species~Pnp, scales="free")+ theme_bw() +
    labs(title = "Caenorhabditis_heritability", y =expression(paste("Heritability (",italic(H[G]^{2}),")")), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vg_Caenorhabditis_dataScale.pdf")
  Plot_H2_Vg_Caenorhabditis_dataScale
  dev.off()
  
  #H2_Vg_Caenorhabditis_dataScale: all Pn.p in log10 scale
  Plot_H2_Vg_Caenorhabditis_dataScale_log10 <- ggplot(H2_Vg_Caenorhabditis_dataScale, aes( x=Model_set ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink3","deeppink"))+
    facet_grid(Species~Pnp)+ theme_bw() +
    labs(title = "Caenorhabditis_heritability log10 scale", y =expression(paste(log[10] (italic(H[M]^{2})))), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vg_Caenorhabditis_dataScale_log10.pdf")
  Plot_H2_Vg_Caenorhabditis_dataScale_log10
  dev.off()
  
  #H2_Vg_Caenorhabditis_dataScale: all Pn.p in log10 scale only Vg
  
  H2_Vg_Caenorhabditis_dataScale_Vg <- subset(H2_Vg_Caenorhabditis_dataScale, Model_set =="Vg")
  
  Plot_H2_Vg_Caenorhabditis_dataScale_Vg_log10 <- ggplot(H2_Vg_Caenorhabditis_dataScale_Vg, aes( x=Pnp ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("red","deeppink"))+
    facet_grid(rows=vars(Species))+ theme_bw() +
    labs(title = "Caenorhabditis_heritability log10 scale_heritability", y =expression(paste(log[10] (italic(H[M]^{2})))),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text = element_text(size=10),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vg_Caenorhabditis_dataScale_Vg_log10.pdf")
  Plot_H2_Vg_Caenorhabditis_dataScale_Vg_log10
  dev.off()
  
  #H2_Vg_Caenorhabditis_dataScale: all Pn.p only Vg                                                 
  
  Plot_H2_Vg_Caenorhabditis_dataScale_Vg <- ggplot(H2_Vg_Caenorhabditis_dataScale_Vg, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Species), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red","deeppink"))+
    geom_point(aes(fill=Species), shape=21,size=2)+ scale_fill_manual(values = c("red","deeppink"))+
    facet_grid(rows=vars(Species), scales="free")+ theme_bw() +
    labs(title = "Caenorhabditis_heritability", y =expression(paste("Heritability (",italic(H[G]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vg_Caenorhabditis_dataScale_Vg.pdf")
  Plot_H2_Vg_Caenorhabditis_dataScale_Vg
  dev.off()
  
  #H2_Vg_Caenorhabditis_dataScale: without P3.p
  H2_Vg_Caenorhabditis_dataScale_woutP3p <- subset(H2_Vg_Caenorhabditis_dataScale, Pnp !="P3.p")
  View(H2_Vg_Caenorhabditis_dataScale_woutP3p)
  
  Plot_H2_Vg_Caenorhabditis_dataScale_woutP3p <- ggplot(H2_Vg_Caenorhabditis_dataScale_woutP3p, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink3","deeppink"))+
    facet_grid(Species~Pnp, scales="free")+ theme_bw() +
    labs(title = "Caenorhabditis_heritability_woutP3p", y =expression(paste("Heritability (",italic(H[G]^{2}),")")), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vg_Caenorhabditis_dataScale_woutP3p.pdf")
  Plot_H2_Vg_Caenorhabditis_dataScale_woutP3p
  dev.off()
  
  
  #H2_Vg_Caenorhabditis_dataScale_Celegans: all Pn.p                                                    
  
  H2_Vg_Caenorhabditis_dataScale_Celegans <- subset(H2_Vg_Caenorhabditis_dataScale, Species =="C.elegans")
  
  Plot_H2_Vg_Caenorhabditis_dataScale_Celegans <- ggplot(H2_Vg_Caenorhabditis_dataScale_Celegans, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "Celegans_heritability", y =expression(paste("Heritability (",italic(H[G]^{2}),")")), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vg_Caenorhabditis_dataScale_Celegans.pdf")
  Plot_H2_Vg_Caenorhabditis_dataScale_Celegans
  dev.off()
  
  H2_Vg_Caenorhabditis_dataScale_Celegans_Vg <- subset(H2_Vg_Caenorhabditis_dataScale_Celegans, Model_set =="Vg")
  
  Plot_H2_Vg_Caenorhabditis_dataScale_Celegans_Vg <- ggplot(H2_Vg_Caenorhabditis_dataScale_Celegans_Vg, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="red", size=1.5, alpha=0.9) +
    geom_point(fill="red", shape=21,size=2)+ 
    theme_bw() +
    labs(title = "Celegans_heritability", y =expression(paste("Heritability (",italic(H[G]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vg_Caenorhabditis_dataScale_Celegans_Vg.pdf")
  Plot_H2_Vg_Caenorhabditis_dataScale_Celegans_Vg
  dev.off() 
  
  #H2_Vg_Caenorhabditis_dataScale_Cbriggsae: all Pn.p                                                    
  
  H2_Vg_Caenorhabditis_dataScale_Cbriggsae <- subset(H2_Vg_Caenorhabditis_dataScale, Species =="C.briggsae")
  
  Plot_H2_Vg_Caenorhabditis_dataScale_Cbriggsae <- ggplot(H2_Vg_Caenorhabditis_dataScale_Cbriggsae, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("pink4","deeppink3","deeppink"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("pink4","deeppink3","deeppink"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "Cbriggsae_heritability", y =expression(paste("Heritability (",italic(H[G]^{2}),")")), x="Models") + scale_x_discrete(labels=c('Controls', 'Wild Isolates', expression(V[G]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vg_Caenorhabditis_dataScale_Cbriggsae.pdf")
  Plot_H2_Vg_Caenorhabditis_dataScale_Cbriggsae
  dev.off()
  
  H2_Vg_Caenorhabditis_dataScale_Cbriggsae_Vg <- subset(H2_Vg_Caenorhabditis_dataScale_Cbriggsae, Model_set =="Vg")
  
  Plot_H2_Vg_Caenorhabditis_dataScale_Cbriggsae_Vg <- ggplot(H2_Vg_Caenorhabditis_dataScale_Cbriggsae_Vg, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="deeppink", size=1.5, alpha=0.9) +
    geom_point(fill="deeppink", shape=21,size=2)+ 
    theme_bw() +
    labs(title = "Cbriggsae_heritability", y =expression(paste("Heritability (",italic(H[G]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vg_Caenorhabditis_dataScale_Cbriggsae_Vg.pdf")
  Plot_H2_Vg_Caenorhabditis_dataScale_Cbriggsae_Vg
  dev.off()
  
  



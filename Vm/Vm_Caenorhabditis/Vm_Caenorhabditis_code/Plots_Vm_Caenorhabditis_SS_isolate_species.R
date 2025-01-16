#Plots_Vm_Caenorhabditis_SS
library(readxl)
library(ggplot2)
library(ggdark)
library(ggtext) 
library(ggrepel)

# Vm_Caenorhabditis_isolate_summary 	 	  from 6-Vm_Caenorhabditis_isolate_SS_vulva_summary.R
# Vm_Caenorhabditis_species_summary_SS_vulva  from 2-Vm_Caenorhabditis_species_summary_SS_vulva
View(Vm_Caenorhabditis_isolate_summary)

#Plots_Va_H2_Vm_Caenorhabditis_isolate_species----

{
  ## Plots_Va_H2_Vm_Caenorhabditis_isolate_dataScale_SS ----
  
  ###Plots_Va_Vm_Caenorhabditis_isolate_dataScale_SS----
  
  Va_Vm_Caenorhabditis_isolate_dataScale_SS <- subset(Vm_Caenorhabditis_isolate_summary, Scale =="data"& Measure=="Va")
  View(Va_Vm_Caenorhabditis_isolate_dataScale_SS)
  
  Va_Vm_Caenorhabditis_isolate_dataScale_SS$Ancestral <- factor(Va_Vm_Caenorhabditis_isolate_dataScale_SS$Ancestral, levels = c("PB306", "JU1200","AF16","PB800") )
  factor(Va_Vm_Caenorhabditis_isolate_dataScale_SS$Ancestral)
  
  Va_Vm_Caenorhabditis_isolate_dataScale_SS$Species <- factor(Va_Vm_Caenorhabditis_isolate_dataScale_SS$Species, levels = c("C.elegans","C.briggsae") )
  factor(Va_Vm_Caenorhabditis_isolate_dataScale_SS$Species)
  
  factor(Va_Vm_Caenorhabditis_isolate_dataScale_SS$Model_set)
  
  #Va_Vm_Caenorhabditis_isolate_dataScale_SS: all Pn.p                                                    
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS <- ggplot(Va_Vm_Caenorhabditis_isolate_dataScale_SS, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink3","deeppink"))+
    facet_grid(Ancestral~Pnp, scales="free")+ theme_bw() +
    labs(title = "Caenorhabditis_isolate_Vm", y=expression(paste("Mutational Variance (",italic(V[M]),")")), x="Models")   + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS.pdf")
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS
  dev.off()
  

  #Va_Vm_Caenorhabditis_isolate_dataScale_SS: without P3.p
  Va_Vm_Caenorhabditis_isolate_dataScale_SS_woutP3p <- subset(Va_Vm_Caenorhabditis_isolate_dataScale_SS, Pnp !="P3.p")
  View(Va_Vm_Caenorhabditis_isolate_dataScale_SS_woutP3p)
  
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_woutP3p <- ggplot(Va_Vm_Caenorhabditis_isolate_dataScale_SS_woutP3p, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink3","deeppink"))+
    facet_grid(Ancestral~Pnp, scales="free")+ theme_bw() +
    labs(title = "Caenorhabditis_isolate_Vm_woutP3p", y =expression(paste("Mutational Variance (",italic(V[M]),")")), x="Models")   + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_woutP3p.pdf")
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_woutP3p
  dev.off()               
  
  
  #Va_Vm_Caenorhabditis_isolate_dataScale_SS: all Pn.p in log10 scale
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_log10 <-  ggplot(Va_Vm_Caenorhabditis_isolate_dataScale_SS, aes( x=Model_set ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    facet_grid(Ancestral~Pnp)+ theme_bw() +
    labs(title = "Caenorhabditis_isolate_Vm log10 scale", y =expression(paste(log[10] (italic(V[M])))),x="Models") + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_log10.pdf")
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_log10
  dev.off()
  
  #Va_Vm_Caenorhabditis_isolate_dataScale_SS: all Pn.p in log10 scale only Vm
  Va_Vm_Caenorhabditis_isolate_dataScale_SS_Vm <- subset(Va_Vm_Caenorhabditis_isolate_dataScale_SS, Model_set =="Vm")
  
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_Vm_log10 <- ggplot(Va_Vm_Caenorhabditis_isolate_dataScale_SS_Vm, aes( x=Pnp ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("red","deeppink"))+
    facet_grid(rows=vars(Ancestral), scales="free")+ theme_bw() +
    labs(title = "Caenorhabditis_isolate_Vm log10 scale", y =expression(paste(log[10] (italic(V[M])))),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_Vm_log10.pdf")
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_Vm_log10
  dev.off()
  
  #Va_Vm_Caenorhabditis_isolate_dataScale_SS: all Pn.p  only Vm
  
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_Vm <- ggplot(Va_Vm_Caenorhabditis_isolate_dataScale_SS_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("red","deeppink"))+
    facet_grid(rows=vars(Ancestral), scales="free")+ theme_bw() +
    labs(title = "Caenorhabditis_isolate_Vm", y=expression(paste("Mutational Variance (",italic(V[M]),")")), x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_Vm.pdf")
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_Vm
  dev.off()
  
  
  #Va_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200: all Pn.p                                                    
  
  Va_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200 <- subset(Va_Vm_Caenorhabditis_isolate_dataScale_SS, Ancestral =="JU1200")
  
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200 <- ggplot(Va_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "JU1200_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Models") + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200.pdf")
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200
  dev.off()
  
  Va_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200_Vm <- subset(Va_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200, Model_set =="Vm")
  
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200_Vm <- ggplot(Va_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="red", size=1.5, alpha=0.9) + 
    geom_point(fill="red", shape=21,size=2)+
    theme_bw() +
    labs(title = "JU1200_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200_Vm.pdf")
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200_Vm
  dev.off()
  
  
  #Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB306: all Pn.p                                                    
  
  Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB306 <- subset(Va_Vm_Caenorhabditis_isolate_dataScale_SS, Ancestral =="PB306")
  
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB306 <- ggplot(Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB306, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "PB306_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Models") + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB306.pdf")
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB306
  dev.off()
  
  Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB306_Vm <- subset(Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB306, Model_set =="Vm")
  
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB306_Vm <- ggplot(Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB306_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="red", size=1.5, alpha=0.9) + 
    geom_point(fill="red", shape=21,size=2)+
    theme_bw() +
    labs(title = "PB306_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB306_Vm.pdf")
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB306_Vm
  dev.off()
  
  #Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB800: all Pn.p                                                    
  
  Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB800 <- subset(Va_Vm_Caenorhabditis_isolate_dataScale_SS, Ancestral =="PB800")
  
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB800 <- ggplot(Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB800, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("pink4","deeppink3","deeppink"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("pink4","deeppink3","deeppink"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "PB800_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Models") + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB800.pdf")
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB800
  dev.off()
  
  Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB800_Vm <- subset(Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB800, Model_set =="Vm")
  
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB800_Vm <- ggplot(Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB800_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="deeppink", size=1.5, alpha=0.9) + 
    geom_point(fill="deeppink", shape=21,size=2)+
    theme_bw() +
    labs(title = "PB800_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB800_Vm.pdf")
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_PB800_Vm
  dev.off()
  
  #Va_Vm_Caenorhabditis_isolate_dataScale_SS_AF16: all Pn.p                                                    
  
  Va_Vm_Caenorhabditis_isolate_dataScale_SS_AF16 <- subset(Va_Vm_Caenorhabditis_isolate_dataScale_SS, Ancestral =="AF16")
  
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_AF16 <- ggplot(Va_Vm_Caenorhabditis_isolate_dataScale_SS_AF16, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("pink4","deeppink3","deeppink"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("pink4","deeppink3","deeppink"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "AF16_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Models") + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_AF16.pdf")
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_AF16
  dev.off()
  
  Va_Vm_Caenorhabditis_isolate_dataScale_SS_AF16_Vm <- subset(Va_Vm_Caenorhabditis_isolate_dataScale_SS_AF16, Model_set =="Vm")
  
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_AF16_Vm <- ggplot(Va_Vm_Caenorhabditis_isolate_dataScale_SS_AF16_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="deeppink", size=1.5, alpha=0.9) + 
    geom_point(fill="deeppink", shape=21,size=2)+
    theme_bw() +
    labs(title = "AF16_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_AF16_Vm.pdf")
  Plot_Va_Vm_Caenorhabditis_isolate_dataScale_SS_AF16_Vm
  dev.off()
  
  
  ###Plots_H2_Vm_Caenorhabditis_isolate_dataScale_SS----
  
  H2_Vm_Caenorhabditis_isolate_dataScale_SS <- subset(Vm_Caenorhabditis_isolate_summary, Scale =="data"& Measure=="H2")
  View(H2_Vm_Caenorhabditis_isolate_dataScale_SS)
  
  H2_Vm_Caenorhabditis_isolate_dataScale_SS$Ancestral <- factor(H2_Vm_Caenorhabditis_isolate_dataScale_SS$Ancestral, levels = c("PB306", "JU1200","AF16","PB800") )
  factor(H2_Vm_Caenorhabditis_isolate_dataScale_SS$Ancestral)
  
  H2_Vm_Caenorhabditis_isolate_dataScale_SS$Species <- factor(H2_Vm_Caenorhabditis_isolate_dataScale_SS$Species, levels = c("C.elegans","C.briggsae") )
  factor(H2_Vm_Caenorhabditis_isolate_dataScale_SS$Species)
  
  factor(H2_Vm_Caenorhabditis_isolate_dataScale_SS$Model_set)
  
  #H2_Vm_Caenorhabditis_isolate_dataScale_SS: all Pn.p                                                    
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS <- ggplot(H2_Vm_Caenorhabditis_isolate_dataScale_SS, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink3","deeppink"))+
    facet_grid(Ancestral~Pnp, scales="free")+ theme_bw() +
    labs(title = "Caenorhabditis_isolate_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")), x="Models")   + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS.pdf")
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS
  dev.off()
  
  #H2_Vm_Caenorhabditis_isolate_dataScale_SS: without P3.p
  H2_Vm_Caenorhabditis_isolate_dataScale_SS_woutP3p <- subset(H2_Vm_Caenorhabditis_isolate_dataScale_SS, Pnp !="P3.p")
  View(H2_Vm_Caenorhabditis_isolate_dataScale_SS_woutP3p)
  
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_woutP3p <- ggplot(H2_Vm_Caenorhabditis_isolate_dataScale_SS_woutP3p, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink3","deeppink"))+
    facet_grid(Ancestral~Pnp, scales="free")+ theme_bw() +
    labs(title = "Caenorhabditis_isolate_mutational heritability_woutP3p", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")), x="Models")   + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_woutP3p.pdf")
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_woutP3p
  dev.off()               
  
  
  #H2_Vm_Caenorhabditis_isolate_dataScale_SS: all Pn.p in log10 scale
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_log10 <- ggplot(H2_Vm_Caenorhabditis_isolate_dataScale_SS, aes( x=Model_set ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    facet_grid(Ancestral~Pnp)+ theme_bw() +
    labs(title = "Caenorhabditis_isolate_mutational heritability log10 scale", y =expression(paste(log[10] (italic(H[M]^{2})))),x="Models") + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_log10.pdf")
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_log10
  dev.off()
  
  #H2_Vm_Caenorhabditis_isolate_dataScale_SS: all Pn.p in log10 scale only Vm
  H2_Vm_Caenorhabditis_isolate_dataScale_SS_Vm <- subset(H2_Vm_Caenorhabditis_isolate_dataScale_SS, Model_set =="Vm")
  
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_Vm_log10 <- ggplot(H2_Vm_Caenorhabditis_isolate_dataScale_SS_Vm, aes( x=Pnp ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("red","deeppink"))+
    facet_grid(rows=vars(Ancestral))+ theme_bw() +
    labs(title = "Caenorhabditis_isolate_mutational heritability log10 scale", y =expression(paste(log[10] (italic(H[M]^{2})))),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_Vm_log10.pdf")
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_Vm_log10
  dev.off()
  
  #H2_Vm_Caenorhabditis_isolate_dataScale_SS: all Pn.p only Vm
  
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_Vm <- ggplot(H2_Vm_Caenorhabditis_isolate_dataScale_SS_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("red","deeppink"))+
    facet_grid(rows=vars(Ancestral), scales="free")+ theme_bw() +
    labs(title = "Caenorhabditis_isolate_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_Vm.pdf")
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_Vm
  dev.off()
  
  
  
  #H2_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200: all Pn.p                                                    
  
  H2_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200 <- subset(H2_Vm_Caenorhabditis_isolate_dataScale_SS, Ancestral =="JU1200")
  
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200 <- ggplot(H2_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "JU1200_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Models") + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200.pdf")
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200
  dev.off()
  
  H2_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200_Vm <- subset(H2_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200, Model_set =="Vm")
  
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200_Vm <- ggplot(H2_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="red", size=1.5, alpha=0.9) + 
    geom_point(fill="red", shape=21,size=2)+
    theme_bw() +
    labs(title = "JU1200_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200_Vm.pdf")
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_JU1200_Vm
  dev.off()
  
  
  #H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB306: all Pn.p                                                    
  
  H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB306 <- subset(H2_Vm_Caenorhabditis_isolate_dataScale_SS, Ancestral =="PB306")
  
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB306 <- ggplot(H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB306, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "PB306_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Models") + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB306.pdf")
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB306
  dev.off()
  
  H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB306_Vm <- subset(H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB306, Model_set =="Vm")
  
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB306_Vm <- ggplot(H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB306_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="red", size=1.5, alpha=0.9) + 
    geom_point(fill="red", shape=21,size=2)+
    theme_bw() +
    labs(title = "PB306_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB306_Vm.pdf")
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB306_Vm
  dev.off()
  
  #H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB800: all Pn.p                                                    
  
  H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB800 <- subset(H2_Vm_Caenorhabditis_isolate_dataScale_SS, Ancestral =="PB800")
  
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB800 <- ggplot(H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB800, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("pink4","deeppink3","deeppink"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("pink4","deeppink3","deeppink"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "PB800_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Models") + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB800.pdf")
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB800
  dev.off()
  
  H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB800_Vm <- subset(H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB800, Model_set =="Vm")
  
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB800_Vm <- ggplot(H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB800_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="deeppink", size=1.5, alpha=0.9) + 
    geom_point(fill="deeppink", shape=21,size=2)+
    theme_bw() +
    labs(title = "PB800_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB800_Vm.pdf")
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_PB800_Vm
  dev.off()
  
  #H2_Vm_Caenorhabditis_isolate_dataScale_SS_AF16: all Pn.p                                                    
  
  H2_Vm_Caenorhabditis_isolate_dataScale_SS_AF16 <- subset(H2_Vm_Caenorhabditis_isolate_dataScale_SS, Ancestral =="AF16")
  
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_AF16 <- ggplot(H2_Vm_Caenorhabditis_isolate_dataScale_SS_AF16, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("pink4","deeppink3","deeppink"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("pink4","deeppink3","deeppink"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "AF16_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Models") + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_AF16.pdf")
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_AF16
  dev.off()
  
  H2_Vm_Caenorhabditis_isolate_dataScale_SS_AF16_Vm <- subset(H2_Vm_Caenorhabditis_isolate_dataScale_SS_AF16, Model_set =="Vm")
  
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_AF16_Vm <- ggplot(H2_Vm_Caenorhabditis_isolate_dataScale_SS_AF16_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="deeppink", size=1.5, alpha=0.9) + 
    geom_point(fill="deeppink", shape=21,size=2)+
    theme_bw() +
    labs(title = "AF16_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_AF16_Vm.pdf")
  Plot_H2_Vm_Caenorhabditis_isolate_dataScale_SS_AF16_Vm
  dev.off()
  
  
  
  
  
  
  
  
  
  
  ##Plots_Va_H2_Vm_Caenorhabditis_species_dataScale_SS ----
  
  ###Plots_Va_Vm_Caenorhabditis_species_dataScale_SS----
  ##
  Va_Vm_Caenorhabditis_species_dataScale_SS <- subset(Vm_Caenorhabditis_species_summary_SS_vulva, Scale =="data"& Measure=="Va")
  names(Va_Vm_Caenorhabditis_species_dataScale_SS)
  table(Va_Vm_Caenorhabditis_species_dataScale_SS$Species)
  
  Va_Vm_Caenorhabditis_species_dataScale_SS$Species <- factor(Va_Vm_Caenorhabditis_species_dataScale_SS$Species, levels = c("C.elegans","C.briggsae" ) )
  factor(Va_Vm_Caenorhabditis_species_dataScale_SS$Species)
  
  #Va_Vm_Caenorhabditis_species_dataScale_SS: all Pn.p                                                    
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS <- ggplot(Va_Vm_Caenorhabditis_species_dataScale_SS, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink3","deeppink"))+
    facet_grid(Species~Pnp, scales="free")+ theme_bw() +
    labs(title = "Caenorhabditis_species_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")"))) + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Caenorhabditis_species_dataScale_SS.pdf")
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS
  dev.off()
  
  #Va_Vm_Caenorhabditis_species_dataScale_SS: all Pn.p in log10 scale
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_log10 <- ggplot(Va_Vm_Caenorhabditis_species_dataScale_SS, aes( x=Model_set ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink3","deeppink"))+
    facet_grid(Species~Pnp)+ theme_bw() +
    labs(title = "Caenorhabditis_species_Vm log10 scale", y =expression(paste(log[10] (italic(V[M])))))+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_log10.pdf")
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_log10
  dev.off()
  
  #Va_Vm_Caenorhabditis_species_dataScale_SS: all Pn.p in log10 scale only Vm
  
  Va_Vm_Caenorhabditis_species_dataScale_SS_Vm <- subset(Va_Vm_Caenorhabditis_species_dataScale_SS, Model_set =="Vm")
  
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Vm_log10 <- ggplot(Va_Vm_Caenorhabditis_species_dataScale_SS_Vm, aes( x=Pnp ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("red","deeppink"))+
    facet_grid(rows=vars(Species))+ theme_bw() +
    labs(title = "Caenorhabditis_species_Vm log10 scale_Vm", y =expression(paste(log[10] (italic(V[M])))),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Vm_log10.pdf")
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Vm_log10
  dev.off()
  
  #Va_Vm_Caenorhabditis_species_dataScale_SS: all Pn.p  only Vm
  
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Vm <- ggplot(Va_Vm_Caenorhabditis_species_dataScale_SS_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Species), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red","deeppink"))+
    geom_point(aes(fill=Species), shape=21,size=1.5)+ scale_fill_manual(values = c("red","deeppink"))+
    facet_grid(rows=vars(Species), scales="free")+ theme_bw() +
    labs(title = "Caenorhabditis_species_Vm log10 scale_Vm",y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Vm.pdf")
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Vm
  dev.off()
  
  
  #Va_Vm_Caenorhabditis_species_dataScale_SS: without P3.p
  Va_Vm_Caenorhabditis_species_dataScale_SS_woutP3p <- subset(Va_Vm_Caenorhabditis_species_dataScale_SS, Pnp !="P3.p")
  View(Va_Vm_Caenorhabditis_species_dataScale_SS_woutP3p)
  
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_woutP3p <- ggplot(Va_Vm_Caenorhabditis_species_dataScale_SS_woutP3p, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink3","deeppink"))+
    facet_grid(Species~Pnp, scales = "free")+ theme_bw() +
    labs(title = "Caenorhabditis_species_Vm_woutP3p", y =expression(paste("Mutational Variance (",italic(V[M]),")"))) + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_woutP3p.pdf")
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_woutP3p
  dev.off()
  
  
  #Va_Vm_Caenorhabditis_species_dataScale_SS_Celegans: all Pn.p                                                    
  
  Va_Vm_Caenorhabditis_species_dataScale_SS_Celegans <- subset(Va_Vm_Caenorhabditis_species_dataScale_SS, Species =="C.elegans")
  
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Celegans <- ggplot(Va_Vm_Caenorhabditis_species_dataScale_SS_Celegans, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "Celegans_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")))+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Celegans.pdf")
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Celegans
  dev.off()
  
  Va_Vm_Caenorhabditis_species_dataScale_SS_Celegans_Vm <- subset( Va_Vm_Caenorhabditis_species_dataScale_SS_Celegans, Model_set =="Vm")
  
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Celegans_Vm <- ggplot(Va_Vm_Caenorhabditis_species_dataScale_SS_Celegans_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="red", size=1.5, alpha=0.9) +
    geom_point(fill="red", shape=21,size=2)+
    theme_bw() +
    labs(title = "Celegans_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +   theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Celegans_Vm.pdf")
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Celegans_Vm
  dev.off()
  
  
  
  #Va_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae: all Pn.p                                                    
  
  Va_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae <- subset(Va_Vm_Caenorhabditis_species_dataScale_SS, Species =="C.briggsae")
  
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae <- ggplot(Va_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("pink4","deeppink3","deeppink"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("pink4","deeppink3","deeppink"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "Cbriggsae_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")"))) + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M])))+ theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae.pdf")
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae
  dev.off()
  
  Va_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae_Vm <- subset( Va_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae, Model_set =="Vm")
  
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae_Vm <- ggplot(Va_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="deeppink", size=1.5, alpha=0.9) +
    geom_point(fill="deeppink", shape=21,size=2)+
    theme_bw() +
    labs(title = "Cbriggsae_Vm", y =expression(paste("Mutational Variance (",italic(V[M]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +   theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae_Vm.pdf")
  Plot_Va_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae_Vm
  dev.off()
  
  ###Plots_H2_Vm_Caenorhabditis_species_dataScale_SS----
  ##
  H2_Vm_Caenorhabditis_species_dataScale_SS <- subset(Vm_Caenorhabditis_species_summary_SS_vulva, Scale =="data"& Measure=="H2")
  names(H2_Vm_Caenorhabditis_species_dataScale_SS)
  table(H2_Vm_Caenorhabditis_species_dataScale_SS$Species)
  
  H2_Vm_Caenorhabditis_species_dataScale_SS$Species <- factor(H2_Vm_Caenorhabditis_species_dataScale_SS$Species, levels = c("C.elegans","C.briggsae" ) )
  factor(H2_Vm_Caenorhabditis_species_dataScale_SS$Species)
  
  #H2_Vm_Caenorhabditis_species_dataScale_SS: all Pn.p                                                    
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS <- ggplot(H2_Vm_Caenorhabditis_species_dataScale_SS, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink3","deeppink"))+
    facet_grid(Species~Pnp, scales="free")+ theme_bw() +
    labs(title = "Caenorhabditis_species_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")"))) + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M])))+ theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Caenorhabditis_species_dataScale_SS.pdf")
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS
  dev.off()
  
  #H2_Vm_Caenorhabditis_species_dataScale_SS: all Pn.p in log10 scale
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_log10 <- ggplot(H2_Vm_Caenorhabditis_species_dataScale_SS, aes( x=Model_set ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink3","deeppink"))+
    facet_grid(Species~Pnp)+ theme_bw() +
    labs(title = "Caenorhabditis_species_mutational heritability log10 scale", y =expression(paste(log[10] (italic(H[M]^{2})))))+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_log10.pdf")
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_log10
  dev.off()
  
  #H2_Vm_Caenorhabditis_species_dataScale_SS: all Pn.p in log10 scale only Vm
  
  H2_Vm_Caenorhabditis_species_dataScale_SS_Vm <- subset(H2_Vm_Caenorhabditis_species_dataScale_SS, Model_set =="Vm")
  
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Vm_log10 <- ggplot(H2_Vm_Caenorhabditis_species_dataScale_SS_Vm, aes( x=Pnp ,y= log10(median))) +
    geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2) + 
    geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=1.5)+ scale_fill_manual(values = c("red","deeppink"))+
    facet_grid(rows=vars(Species))+ theme_bw() +
    labs(title = "Caenorhabditis_species_mutational heritability log10 scale_mutational heritability", y =expression(paste(log[10] (italic(H[M]^{2})))),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text = element_text(size=10),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Vm_log10.pdf")
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Vm_log10
  dev.off()
  
  #H2_Vm_Caenorhabditis_species_dataScale_SS: all Pn.p only Vm                                                 
  
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Vm <- ggplot(H2_Vm_Caenorhabditis_species_dataScale_SS_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Species), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red","deeppink"))+
    geom_point(aes(fill=Species), shape=21,size=2)+ scale_fill_manual(values = c("red","deeppink"))+
    facet_grid(rows=vars(Species), scales="free")+ theme_bw() +
    labs(title = "Caenorhabditis_species_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Vm.pdf")
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Vm
  dev.off()
  
  #H2_Vm_Caenorhabditis_species_dataScale_SS: without P3.p
  H2_Vm_Caenorhabditis_species_dataScale_SS_woutP3p <- subset(H2_Vm_Caenorhabditis_species_dataScale_SS, Pnp !="P3.p")
  View(H2_Vm_Caenorhabditis_species_dataScale_SS_woutP3p)
  
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_woutP3p <- ggplot(H2_Vm_Caenorhabditis_species_dataScale_SS_woutP3p, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color= interaction(Model_set, Species, sep=':')), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red","pink4","deeppink4","deeppink"))+
    geom_point(aes(fill=interaction(Model_set, Species, sep=':')), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red","pink4","deeppink3","deeppink"))+
    facet_grid(Species~Pnp, scales="free")+ theme_bw() +
    labs(title = "Caenorhabditis_species_mutational heritability_woutP3p", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")))+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  
  pdf("Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_woutP3p.pdf")
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_woutP3p
  dev.off()
  
  
  #H2_Vm_Caenorhabditis_species_dataScale_SS_Celegans: all Pn.p                                                    
  
  H2_Vm_Caenorhabditis_species_dataScale_SS_Celegans <- subset(H2_Vm_Caenorhabditis_species_dataScale_SS, Species =="C.elegans")
  
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Celegans <- ggplot(H2_Vm_Caenorhabditis_species_dataScale_SS_Celegans, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("red4","red3","red"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("red4","red3","red"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "Celegans_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")"))) + scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M])))+ theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Celegans.pdf")
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Celegans
  dev.off()
  
  H2_Vm_Caenorhabditis_species_dataScale_SS_Celegans_Vm <- subset(H2_Vm_Caenorhabditis_species_dataScale_SS_Celegans, Model_set =="Vm")
  
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Celegans_Vm <- ggplot(H2_Vm_Caenorhabditis_species_dataScale_SS_Celegans_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="red", size=1.5, alpha=0.9) +
    geom_point(fill="red", shape=21,size=2)+ 
    theme_bw() +
    labs(title = "Celegans_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Celegans_Vm.pdf")
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Celegans_Vm
  dev.off() 
  
  #H2_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae: all Pn.p                                                    
  
  H2_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae <- subset(H2_Vm_Caenorhabditis_species_dataScale_SS, Species =="C.briggsae")
  
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae <- ggplot(H2_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae, aes( x=Model_set ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Model_set), size=1.5, alpha=0.9) + scale_colour_manual(values = c("pink4","deeppink3","deeppink"))+
    geom_point(aes(fill=Model_set), shape=21,size=2)+ scale_fill_manual(values = c("pink4","deeppink3","deeppink"))+
    facet_grid(~Pnp)+ theme_bw() +
    labs(title = "Cbriggsae_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")))+ scale_x_discrete(labels=c('Ancestral', 'ML', expression(V[M]))) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae.pdf")
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae
  dev.off()
  
  H2_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae_Vm <- subset(H2_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae, Model_set =="Vm")
  
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae_Vm <- ggplot(H2_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae_Vm, aes( x=Pnp ,y= median)) +
    geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2) + 
    geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83), color="deeppink", size=1.5, alpha=0.9) +
    geom_point(fill="deeppink", shape=21,size=2)+ 
    theme_bw() +
    labs(title = "Cbriggsae_mutational heritability", y =expression(paste("Mutational heritability (",italic(H[M]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))
  
  pdf("Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae_Vm.pdf")
  Plot_H2_Vm_Caenorhabditis_species_dataScale_SS_Cbriggsae_Vm
  dev.off()
  



#Plots_Vr_Caenorhabditis_SS_Oscheius_SSSS
library(scales)
library(readxl)
library(ggplot2)
library(ggtext) 
library(ggExtra)

#Vr_Caenorhabditis_summary from C3-Vr_Caenorhabditis_summary_SS.R
#Vr_Oscheius_summary	   from O3-Vr_Oscheius_summary_SSSS.R

Vr_Caenorhabditis_SS_Oscheius_SSSS_summary <- rbind.data.frame(Vr_Caenorhabditis_summary, Vr_Oscheius_summary)
names(Vr_Caenorhabditis_SS_Oscheius_SSSS_summary)
head(Vr_Caenorhabditis_SS_Oscheius_SSSS_summary)

write_xlsx(Vr_Caenorhabditis_SS_Oscheius_SSSS_summary, "Vr_Caenorhabditis_SS_Oscheius_SSSS_summary.xlsx")

#Plots_Va_H2_Evol_Vr_Caenorhabditis_SS_Oscheius_SSSSs----
##Plots_Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale----
##
Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale <- subset(Vr_Caenorhabditis_SS_Oscheius_SSSS_summary, Scale =="data"& Measure=="Va_Phylo")
names(Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale)
head(Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale)
table(Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale$Genus)

factor(Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale$Genus)

#Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale: all Pn.p                                                    
Plot_Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale <- ggplot(Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale, aes( x=Pnp ,y= median)) +
  geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2, color="black") + 
  geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Genus), linewidth=1.5, alpha=0.9) + scale_colour_manual(values = c("salmon","royalblue"))+
  geom_point(aes(fill=Genus), shape=21,size=1.5, color="black")+ scale_fill_manual(values = c("salmon","royalblue"))+
  facet_grid(rows=vars(Genus))+ theme_bw() +
  labs(title = "Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS",y =expression(paste("Rates among species Genetic Variance (",italic(V[R]),")")),x="Pn.p") +scale_y_continuous(labels = scales::number_format()) + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))


pdf("Plot_Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale.pdf")
Plot_Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale
dev.off()


Plot_Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_2 <- ggplot(Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale, aes( x=Pnp ,y= median)) +
  geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2, color="black", size=1) + 
  geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Genus), linewidth=2.5, alpha=0.9) + scale_colour_manual(values = c("salmon","royalblue"))+
  geom_point(aes(fill=Genus), shape=21,size=1.5, stroke=1, color="black")+ scale_fill_manual(values = c("salmon","royalblue"))+
  facet_grid(cols=vars(Genus))+ theme_bw() +
  labs(title = "Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS",y =expression(paste("Rates among species Genetic Variance (",italic(V[R]),")")),x="Pn.p") +scale_y_continuous(labels = scales::number_format()) + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))


pdf("Plot_Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_2.pdf")
Plot_Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_2
dev.off()



#Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale: all Pn.p    in log10 scale                                                
Plot_Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_log10 <- ggplot(Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale, aes( x=Pnp ,y= log10(median))) +
  geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2, color="black") + 
  geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=Genus), size=1.5, alpha=0.9) + scale_colour_manual(values = c("salmon","royalblue"))+
  geom_point(aes(fill=Genus), shape=21,size=1.5, color="black")+ scale_fill_manual(values = c("salmon","royalblue"))+
  facet_grid(rows=vars(Genus))+ theme_bw() +
  labs(title = "Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS log10",y =expression(paste("Log10 (",italic(V[R]),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))


pdf("Plot_Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_log10.pdf")
Plot_Va_Phylo_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_log10
dev.off()



##Plots_H2_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale----
##
H2_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale <- subset(Vr_Caenorhabditis_SS_Oscheius_SSSS_summary, Scale =="data"& Measure=="H2")
names(H2_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale)
head(H2_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale)
table(H2_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale$Genus)

factor(H2_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale$Genus)

#H2_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale: all Pn.p                                                    
Plot_H2_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale <- ggplot(H2_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale, aes( x=Pnp ,y= median)) +
  geom_errorbar(aes(ymin=lowerHPD_0.95, ymax=upperHPD_0.95), width=0.2, color="black") + 
  geom_linerange(aes(ymin=lowerHPD_0.83, ymax=upperHPD_0.83, color=Genus), size=1.5, alpha=0.9) + scale_colour_manual(values = c("salmon","royalblue"))+
  geom_point(aes(fill=Genus), shape=21,size=1.5, color="black")+ scale_fill_manual(values = c("salmon","royalblue"))+
  facet_grid(rows=vars(Genus))+ theme_bw() +
  labs(title = "H2_Vr_Caenorhabditis_SS_Oscheius_SSSS",y =expression(paste("Phylogenetic Heritability (",italic(H[R]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))


pdf("Plot_H2_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale.pdf")
Plot_H2_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale
dev.off()



#H2_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale: all Pn.p    in log10 scale                                                
Plot_H2_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_log10 <- ggplot(H2_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale, aes( x=Pnp ,y= log10(median))) +
  geom_errorbar(aes(ymin=log10(lowerHPD_0.95), ymax=log10(upperHPD_0.95)), width=0.2, color="black") + 
  geom_linerange(aes(ymin=log10(lowerHPD_0.83), ymax=log10(upperHPD_0.83), color=Genus), size=1.5, alpha=0.9) + scale_colour_manual(values = c("salmon","royalblue"))+
  geom_point(aes(fill=Genus), shape=21,size=1.5, color="black")+ scale_fill_manual(values = c("salmon","royalblue"))+
  facet_grid(rows=vars(Genus))+ theme_bw() +
  labs(title = "H2_Vr_Caenorhabditis_SS_Oscheius_SSSS log10",y =expression(paste("Log10 (",italic(H[R]^{2}),")")),x="Pn.p") + scale_x_discrete(labels=c("3","4","5","6","7","8")) +  theme(aspect.ratio=1,axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)),axis.text = element_text(size =10), legend.position = "bottom",axis.title = element_text(size = 15))


pdf("Plot_H2_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_log10.pdf")
Plot_H2_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_log10
dev.off()


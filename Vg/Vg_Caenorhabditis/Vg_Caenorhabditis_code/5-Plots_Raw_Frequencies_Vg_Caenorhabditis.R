#Plots_Raw_Frequencies_Vg_Caenorhabditis
library(readxl)
library(ggplot2)
library(ggdark)
library(ggtext) 
library(ggExtra)
library(tidyr)

#Data----
Vm_Vg_data<- as.data.frame(read_xlsx("Data S6 phenotyping for VG VM.xlsx"))
View(Vm_Vg_data)
Vm_Vg_data <- Vm_Vg_data[,-c(49,50)]
Vg_data <- subset(Vm_Vg_data, Treatment !="MA" )
Vg_Caenorhabditis_data <- subset(Vg_data, Genus == "Caenorhabditis" )
Vg_Caenorhabditis_data$LineB <- ifelse(Vg_Caenorhabditis_data$Treatment == "CONTROL" ,paste(Vg_Caenorhabditis_data$Line,Vg_Caenorhabditis_data$Block), paste(Vg_Caenorhabditis_data$Line))
Vg_Caenorhabditis_data$BlockRep <- paste(Vg_Caenorhabditis_data$Block,Vg_Caenorhabditis_data$Replicate)
Vg_Caenorhabditis_data$Treatment <- as.factor(Vg_Caenorhabditis_data$Treatment)
Vg_Caenorhabditis_data$Observer <- as.factor(Vg_Caenorhabditis_data$Observer)

Vg_Caenorhabditis_data$P3.p_total <- (Vg_Caenorhabditis_data$P3.p_S + Vg_Caenorhabditis_data$P3.p_SS + Vg_Caenorhabditis_data$P3.p_SSS + Vg_Caenorhabditis_data$P3.p_SSSS + Vg_Caenorhabditis_data$P3.p_Other)
Vg_Caenorhabditis_data$P4.p_total <- (Vg_Caenorhabditis_data$P4.p_S + Vg_Caenorhabditis_data$P4.p_SS + Vg_Caenorhabditis_data$P4.p_SSS + Vg_Caenorhabditis_data$P4.p_SSSS + Vg_Caenorhabditis_data$P4.p_Other)
Vg_Caenorhabditis_data$P8.p_total <- (Vg_Caenorhabditis_data$P8.p_S + Vg_Caenorhabditis_data$P8.p_SS + Vg_Caenorhabditis_data$P8.p_SSS + Vg_Caenorhabditis_data$P8.p_SSSS + Vg_Caenorhabditis_data$P8.p_Other)
Vg_Caenorhabditis_data$P5.p_total <- (Vg_Caenorhabditis_data$P5.p_wt+ Vg_Caenorhabditis_data$P5.p_Other) 
Vg_Caenorhabditis_data$P6.p_total <- (Vg_Caenorhabditis_data$P6.p_wt+ Vg_Caenorhabditis_data$P6.p_Other) 
Vg_Caenorhabditis_data$P7.p_total <- (Vg_Caenorhabditis_data$P7.p_wt+ Vg_Caenorhabditis_data$P7.p_Other) 
View(Vg_Caenorhabditis_data)

table(Vg_Caenorhabditis_data$P3.p_total )
table(Vg_Caenorhabditis_data$P4.p_total )
table(Vg_Caenorhabditis_data$P8.p_total )
table(Vg_Caenorhabditis_data$P5.p_total )
table(Vg_Caenorhabditis_data$P6.p_total )
table(Vg_Caenorhabditis_data$P7.p_total )
View(Vg_Caenorhabditis_data)



#Sum and aggregate data per line
{
  
  sum_Vg_Caenorhabditis_data <- aggregate(cbind(Vg_Caenorhabditis_data$P3.p_S,Vg_Caenorhabditis_data$P3.p_SS,Vg_Caenorhabditis_data$P3.p_SSS,Vg_Caenorhabditis_data$P3.p_SSSS,Vg_Caenorhabditis_data$P3.p_Other,Vg_Caenorhabditis_data$P3.p_total,Vg_Caenorhabditis_data$P4.p_S,Vg_Caenorhabditis_data$P4.p_SS,Vg_Caenorhabditis_data$P4.p_SSS,Vg_Caenorhabditis_data$P4.p_SSSS,Vg_Caenorhabditis_data$P4.p_Other,Vg_Caenorhabditis_data$P4.p_total,Vg_Caenorhabditis_data$P5.p_wt, Vg_Caenorhabditis_data$P5.p_Other,Vg_Caenorhabditis_data$P5.p_total,Vg_Caenorhabditis_data$P6.p_wt, Vg_Caenorhabditis_data$P6.p_Other,Vg_Caenorhabditis_data$P6.p_total, Vg_Caenorhabditis_data$P7.p_wt, Vg_Caenorhabditis_data$P7.p_Other,Vg_Caenorhabditis_data$P7.p_total, Vg_Caenorhabditis_data$P8.p_S,Vg_Caenorhabditis_data$P8.p_SS,Vg_Caenorhabditis_data$P8.p_SSS,Vg_Caenorhabditis_data$P8.p_SSSS,Vg_Caenorhabditis_data$P8.p_Other,Vg_Caenorhabditis_data$P8.p_total), by=list(LineB=Vg_Caenorhabditis_data$LineB), FUN=sum)
  View(sum_Vg_Caenorhabditis_data)
  colnames(sum_Vg_Caenorhabditis_data) <- c("LineB","P3.p_S_sum","P3.p_SS_sum","P3.p_SSS_sum","P3.p_SSSS_sum","P3.p_Other_sum","P3.p_total_sum","P4.p_S_sum", "P4.p_SS_sum","P4.p_SSS_sum","P4.p_SSSS_sum", "P4.p_Other_sum","P4.p_total_sum","P5.p_wt_sum","P5.p_Other_sum","P5.p_total_sum","P6.p_wt_sum","P6.p_Other_sum","P6.p_total_sum","P7.p_wt_sum","P7.p_Other_sum","P7.p_total_sum", "P8.p_S_sum", "P8.p_SS_sum","P8.p_SSS_sum","P8.p_SSSS_sum", "P8.p_Other_sum","P8.p_total_sum")
  names(Vg_Caenorhabditis_data)
  
  Vg_Caenorhabditis_data_sum <- merge(unique(Vg_Caenorhabditis_data[,c("Variance","Genus","Species","Ancestral","Treatment","LineB" )]), sum_Vg_Caenorhabditis_data)         
  View(Vg_Caenorhabditis_data_sum)
  table(Vg_Caenorhabditis_data_sum$LineB)
  
}  

#frequency
{
  Vg_Caenorhabditis_data_sum$P3.p_S_freq <-  c((Vg_Caenorhabditis_data_sum$P3.p_S_sum/Vg_Caenorhabditis_data_sum$P3.p_total_sum))  
  Vg_Caenorhabditis_data_sum$P3.p_SS_freq <-  c((Vg_Caenorhabditis_data_sum$P3.p_SS_sum/Vg_Caenorhabditis_data_sum$P3.p_total_sum))  
  Vg_Caenorhabditis_data_sum$P3.p_SSS_freq <-  c((Vg_Caenorhabditis_data_sum$P3.p_SSS_sum/Vg_Caenorhabditis_data_sum$P3.p_total_sum))  
  Vg_Caenorhabditis_data_sum$P3.p_SSSS_freq <-  c((Vg_Caenorhabditis_data_sum$P3.p_SSSS_sum/Vg_Caenorhabditis_data_sum$P3.p_total_sum))  
  Vg_Caenorhabditis_data_sum$P3.p_Other_freq <-  c((Vg_Caenorhabditis_data_sum$P3.p_Other_sum/Vg_Caenorhabditis_data_sum$P3.p_total_sum))  
  
  Vg_Caenorhabditis_data_sum$P4.p_S_freq <-  c((Vg_Caenorhabditis_data_sum$P4.p_S_sum/Vg_Caenorhabditis_data_sum$P4.p_total_sum))  
  Vg_Caenorhabditis_data_sum$P4.p_SS_freq <-  c((Vg_Caenorhabditis_data_sum$P4.p_SS_sum/Vg_Caenorhabditis_data_sum$P4.p_total_sum))  
  Vg_Caenorhabditis_data_sum$P4.p_SSS_freq <-  c((Vg_Caenorhabditis_data_sum$P4.p_SSS_sum/Vg_Caenorhabditis_data_sum$P4.p_total_sum))  
  Vg_Caenorhabditis_data_sum$P4.p_SSSS_freq <-  c((Vg_Caenorhabditis_data_sum$P4.p_SSSS_sum/Vg_Caenorhabditis_data_sum$P4.p_total_sum))  
  Vg_Caenorhabditis_data_sum$P4.p_Other_freq <-  c((Vg_Caenorhabditis_data_sum$P4.p_Other_sum/Vg_Caenorhabditis_data_sum$P4.p_total_sum))  
  
  Vg_Caenorhabditis_data_sum$P5.p_wt_freq <-  c((Vg_Caenorhabditis_data_sum$P5.p_wt_sum/Vg_Caenorhabditis_data_sum$P5.p_total_sum))  
  Vg_Caenorhabditis_data_sum$P5.p_Other_freq <-  c((Vg_Caenorhabditis_data_sum$P5.p_Other_sum/Vg_Caenorhabditis_data_sum$P5.p_total_sum))
  
  Vg_Caenorhabditis_data_sum$P6.p_wt_freq <-  c((Vg_Caenorhabditis_data_sum$P6.p_wt_sum/Vg_Caenorhabditis_data_sum$P6.p_total_sum))  
  Vg_Caenorhabditis_data_sum$P6.p_Other_freq <-  c((Vg_Caenorhabditis_data_sum$P6.p_Other_sum/Vg_Caenorhabditis_data_sum$P6.p_total_sum))
  
  Vg_Caenorhabditis_data_sum$P7.p_wt_freq <-  c((Vg_Caenorhabditis_data_sum$P7.p_wt_sum/Vg_Caenorhabditis_data_sum$P7.p_total_sum))  
  Vg_Caenorhabditis_data_sum$P7.p_Other_freq <-  c((Vg_Caenorhabditis_data_sum$P7.p_Other_sum/Vg_Caenorhabditis_data_sum$P7.p_total_sum))
  
  Vg_Caenorhabditis_data_sum$P8.p_S_freq <-  c((Vg_Caenorhabditis_data_sum$P8.p_S_sum/Vg_Caenorhabditis_data_sum$P8.p_total_sum))  
  Vg_Caenorhabditis_data_sum$P8.p_SS_freq <-  c((Vg_Caenorhabditis_data_sum$P8.p_SS_sum/Vg_Caenorhabditis_data_sum$P8.p_total_sum))  
  Vg_Caenorhabditis_data_sum$P8.p_SSS_freq <-  c((Vg_Caenorhabditis_data_sum$P8.p_SSS_sum/Vg_Caenorhabditis_data_sum$P8.p_total_sum))  
  Vg_Caenorhabditis_data_sum$P8.p_SSSS_freq <-  c((Vg_Caenorhabditis_data_sum$P8.p_SSSS_sum/Vg_Caenorhabditis_data_sum$P8.p_total_sum))  
  Vg_Caenorhabditis_data_sum$P8.p_Other_freq <-  c((Vg_Caenorhabditis_data_sum$P8.p_Other_sum/Vg_Caenorhabditis_data_sum$P8.p_total_sum))  
  View(Vg_Caenorhabditis_data_sum)
}



table(Vg_Caenorhabditis_data_sum$Species)
write_xlsx(Vg_Caenorhabditis_data_sum, "Vg_Caenorhabditis_data_sum-and-frequencies.xlsx")



#PLots----

---- #Raw_Frequencies_Caenorhabditis----

Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale <- subset(Vg_Caenorhabditis_data_sum, Treatment =="WILD")
table(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale$Species)

Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale$Species <- factor(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale$Species, levels = c("C.elegans", "C.briggsae") )
factor(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale$Species)

Plot_Raw_Frequencies_Caenorhabditis_Vg_dataScale_P3pP4p  <- ggplot(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale, aes(x = P3.p_SS_freq, y = P4.p_SS_freq)) + 
  geom_point(aes(colour = Species), shape=19) + scale_colour_manual(values = c("red","deeppink"))+  
  facet_grid( rows =vars(Species))  + xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequecy of P3.p division", y = "Frequecy of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))

pdf("Plot_Raw_Frequencies_Caenorhabditis_Vg_dataScale_P3pP4p.pdf")
Plot_Raw_Frequencies_Caenorhabditis_Vg_dataScale_P3pP4p
dev.off()


Plot_Raw_Frequencies_Caenorhabditis_Vg_dataScale_P3pP4p_v2 <- ggplot(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale, aes(x = P3.p_SS_freq, y = P4.p_SS_freq)) + 
  geom_point(aes(colour = Species), shape=19, alpha=0.8) + scale_colour_manual(values = c("red","deeppink"))+  
  xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequecy of P3.p division", y = "Frequecy of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))

pdf("Plot_Raw_Frequencies_Caenorhabditis_Vg_dataScale_P3pP4p_v2_boxplot.pdf")
ggMarginal(Plot_Raw_Frequencies_Caenorhabditis_Vg_dataScale_P3pP4p_v2, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
dev.off()



---- ##Raw_Frequencies_Celegans----

Raw_Frequencies_Celegans_Vg_mod_dataScale <- subset(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale, Species =="C.elegans")

Plot_Raw_Frequencies_Celegans_Vg_dataScale_P3pP4p <- ggplot(Raw_Frequencies_Celegans_Vg_mod_dataScale, aes(x = P3.p_SS_freq, y = P4.p_SS_freq)) + 
  geom_point(aes(colour = Species), shape=19, alpha=0.8) +scale_colour_manual(values = c("red"))+
  xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequecy of P3.p division", y = "Frequecy of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))

pdf("Plot_Raw_Frequencies_Celegans_Vg_dataScale_P3pP4p_boxplot.pdf")
ggMarginal(Plot_Raw_Frequencies_Celegans_Vg_dataScale_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
dev.off()



---- ##Raw_Frequencies_Cbriggsae----

Raw_Frequencies_Cbriggsae_Vg_mod_dataScale <- subset(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale, Species =="C.briggsae")

Plot_Raw_Frequencies_Cbriggsae_Vg_dataScale_P3pP4p <- ggplot(Raw_Frequencies_Cbriggsae_Vg_mod_dataScale, aes(x = P3.p_SS_freq, y = P4.p_SS_freq)) + 
  geom_point(aes(colour = Species), shape=19, alpha=0.8) +scale_colour_manual(values = c("deeppink"))+
  xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequecy of P3.p division", y = "Frequecy of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))

pdf("Plot_Raw_Frequencies_Cbriggsae_Vg_dataScale_P3pP4p_boxplot.pdf")
ggMarginal(Plot_Raw_Frequencies_Cbriggsae_Vg_dataScale_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
dev.off()



#Raw_Frequencies__Caenorhabditis_Vg_mod_dataScale_all_Pnp  ----                                                  

Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale

#reshape data to have one column with Pnp_fate and another with the frequency 

names(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale)

Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild <- Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale[,-c(7:33)]
head(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild)
names(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild)


Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild_v2 <- Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild %>%
  gather(key = "trait", value = "Trait_Frequency", 7:27)
View(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild_v2)

Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild_v3 <- Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild_v2 %>%
  separate(trait, into = c("Pn.p", "Pn.p_fate"), sep = "_")

View(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild_v3)

write_xlsx(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild_v3, "Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild_v3.xlsx")



Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild_v3_SS <- subset(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild_v3, Pn.p_fate=="SS" | Pn.p_fate=="wt")
View(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild_v3_SS)

Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild_v3_SS$Species <- factor(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild_v3_SS$Species, levels = c("C.elegans", "C.briggsae") )
factor(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild_v3_SS$Species)


Plot_Raw_Frequencies_Caenorhabditis_Vg_dataScale_all_Pnp_SS <- ggplot(Raw_Frequencies_Caenorhabditis_Vg_mod_dataScale_wild_v3_SS, aes( x= Pn.p ,y= Trait_Frequency)) +
  geom_point(aes(colour = Species), size=0.7) + scale_colour_manual(values = c("red","deeppink")) +  
  facet_grid(rows =vars(Species)) + theme_bw() +
  labs(title = "Raw_Frequencies_Caenorhabditis_Vg_dataScale_all_Pnp_SS", y ="Trait frequency") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 20)), legend.position = "bottom",axis.text.x = element_text(size = 10),axis.title = element_text(size = 15))

pdf("Plot_Raw_Frequencies_Caenorhabditis_Vg_dataScale_all_Pnp_SS.pdf")
Plot_Raw_Frequencies_Caenorhabditis_Vg_dataScale_all_Pnp_SS
dev.off()



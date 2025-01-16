#Plots_Raw_Frequencies_Vg_Oscheius_SSSS
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
Vg_Oscheius_data <- subset(Vg_data, Genus == "Oscheius" )
Vg_Oscheius_data$LineB <- ifelse(Vg_Oscheius_data$Treatment == "CONTROL" ,paste(Vg_Oscheius_data$Line,Vg_Oscheius_data$Block), paste(Vg_Oscheius_data$Line))
Vg_Oscheius_data$BlockRep <- paste(Vg_Oscheius_data$Block,Vg_Oscheius_data$Replicate)
Vg_Oscheius_data$Treatment <- as.factor(Vg_Oscheius_data$Treatment)
Vg_Oscheius_data$Observer <- as.factor(Vg_Oscheius_data$Observer)

Vg_Oscheius_data_woutCB <- subset(Vg_Oscheius_data, Observer !="CB")

Vg_Oscheius_data_woutCB$P3.p_total <- (Vg_Oscheius_data_woutCB$P3.p_S + Vg_Oscheius_data_woutCB$P3.p_SS + Vg_Oscheius_data_woutCB$P3.p_SSS + Vg_Oscheius_data_woutCB$P3.p_SSSS + Vg_Oscheius_data_woutCB$P3.p_Other)
Vg_Oscheius_data_woutCB$P4.p_total <- (Vg_Oscheius_data_woutCB$P4.p_S + Vg_Oscheius_data_woutCB$P4.p_SS + Vg_Oscheius_data_woutCB$P4.p_SSS + Vg_Oscheius_data_woutCB$P4.p_SSSS + Vg_Oscheius_data_woutCB$P4.p_Other)
Vg_Oscheius_data_woutCB$P8.p_total <- (Vg_Oscheius_data_woutCB$P8.p_S + Vg_Oscheius_data_woutCB$P8.p_SS + Vg_Oscheius_data_woutCB$P8.p_SSS + Vg_Oscheius_data_woutCB$P8.p_SSSS + Vg_Oscheius_data_woutCB$P8.p_Other)
Vg_Oscheius_data_woutCB$P5.p_total <- (Vg_Oscheius_data_woutCB$P5.p_wt+ Vg_Oscheius_data_woutCB$P5.p_Other) 
Vg_Oscheius_data_woutCB$P6.p_total <- (Vg_Oscheius_data_woutCB$P6.p_wt+ Vg_Oscheius_data_woutCB$P6.p_Other) 
Vg_Oscheius_data_woutCB$P7.p_total <- (Vg_Oscheius_data_woutCB$P7.p_wt+ Vg_Oscheius_data_woutCB$P7.p_Other) 
View(Vg_Oscheius_data_woutCB)

table(Vg_Oscheius_data_woutCB$P3.p_total )
table(Vg_Oscheius_data_woutCB$P4.p_total )
table(Vg_Oscheius_data_woutCB$P8.p_total )
table(Vg_Oscheius_data_woutCB$P5.p_total )
table(Vg_Oscheius_data_woutCB$P6.p_total )
table(Vg_Oscheius_data_woutCB$P7.p_total )
View(Vg_Oscheius_data_woutCB)



#Sum and aggregate data per line
{
  
  sum_Vg_Oscheius_data_woutCB <- aggregate(cbind(Vg_Oscheius_data_woutCB$P3.p_S,Vg_Oscheius_data_woutCB$P3.p_SS,Vg_Oscheius_data_woutCB$P3.p_SSS,Vg_Oscheius_data_woutCB$P3.p_SSSS,Vg_Oscheius_data_woutCB$P3.p_Other,Vg_Oscheius_data_woutCB$P3.p_total,Vg_Oscheius_data_woutCB$P4.p_S,Vg_Oscheius_data_woutCB$P4.p_SS,Vg_Oscheius_data_woutCB$P4.p_SSS,Vg_Oscheius_data_woutCB$P4.p_SSSS,Vg_Oscheius_data_woutCB$P4.p_Other,Vg_Oscheius_data_woutCB$P4.p_total,Vg_Oscheius_data_woutCB$P5.p_wt, Vg_Oscheius_data_woutCB$P5.p_Other,Vg_Oscheius_data_woutCB$P5.p_total,Vg_Oscheius_data_woutCB$P6.p_wt, Vg_Oscheius_data_woutCB$P6.p_Other,Vg_Oscheius_data_woutCB$P6.p_total, Vg_Oscheius_data_woutCB$P7.p_wt, Vg_Oscheius_data_woutCB$P7.p_Other,Vg_Oscheius_data_woutCB$P7.p_total, Vg_Oscheius_data_woutCB$P8.p_S,Vg_Oscheius_data_woutCB$P8.p_SS,Vg_Oscheius_data_woutCB$P8.p_SSS,Vg_Oscheius_data_woutCB$P8.p_SSSS,Vg_Oscheius_data_woutCB$P8.p_Other,Vg_Oscheius_data_woutCB$P8.p_total), by=list(LineB=Vg_Oscheius_data_woutCB$LineB), FUN=sum)
  View(sum_Vg_Oscheius_data_woutCB)
  colnames(sum_Vg_Oscheius_data_woutCB) <- c("LineB","P3.p_S_sum","P3.p_SS_sum","P3.p_SSS_sum","P3.p_SSSS_sum","P3.p_Other_sum","P3.p_total_sum","P4.p_S_sum", "P4.p_SS_sum","P4.p_SSS_sum","P4.p_SSSS_sum", "P4.p_Other_sum","P4.p_total_sum","P5.p_wt_sum","P5.p_Other_sum","P5.p_total_sum","P6.p_wt_sum","P6.p_Other_sum","P6.p_total_sum","P7.p_wt_sum","P7.p_Other_sum","P7.p_total_sum", "P8.p_S_sum", "P8.p_SS_sum","P8.p_SSS_sum","P8.p_SSSS_sum", "P8.p_Other_sum","P8.p_total_sum")
  names(Vg_Oscheius_data_woutCB)
  
  Vg_Oscheius_data_woutCB_sum <- merge(unique(Vg_Oscheius_data_woutCB[,c("Variance","Genus","Species","Ancestral","Treatment","LineB" )]), sum_Vg_Oscheius_data_woutCB)         
  View(Vg_Oscheius_data_woutCB_sum)
  table(Vg_Oscheius_data_woutCB_sum$LineB)
  
}  

#frequency
{
  Vg_Oscheius_data_woutCB_sum$P3.p_S_freq <-  c((Vg_Oscheius_data_woutCB_sum$P3.p_S_sum/Vg_Oscheius_data_woutCB_sum$P3.p_total_sum))  
  Vg_Oscheius_data_woutCB_sum$P3.p_SS_freq <-  c((Vg_Oscheius_data_woutCB_sum$P3.p_SS_sum/Vg_Oscheius_data_woutCB_sum$P3.p_total_sum))  
  Vg_Oscheius_data_woutCB_sum$P3.p_SSS_freq <-  c((Vg_Oscheius_data_woutCB_sum$P3.p_SSS_sum/Vg_Oscheius_data_woutCB_sum$P3.p_total_sum))  
  Vg_Oscheius_data_woutCB_sum$P3.p_SSSS_freq <-  c((Vg_Oscheius_data_woutCB_sum$P3.p_SSSS_sum/Vg_Oscheius_data_woutCB_sum$P3.p_total_sum))  
  Vg_Oscheius_data_woutCB_sum$P3.p_Other_freq <-  c((Vg_Oscheius_data_woutCB_sum$P3.p_Other_sum/Vg_Oscheius_data_woutCB_sum$P3.p_total_sum))  
  
  Vg_Oscheius_data_woutCB_sum$P4.p_S_freq <-  c((Vg_Oscheius_data_woutCB_sum$P4.p_S_sum/Vg_Oscheius_data_woutCB_sum$P4.p_total_sum))  
  Vg_Oscheius_data_woutCB_sum$P4.p_SS_freq <-  c((Vg_Oscheius_data_woutCB_sum$P4.p_SS_sum/Vg_Oscheius_data_woutCB_sum$P4.p_total_sum))  
  Vg_Oscheius_data_woutCB_sum$P4.p_SSS_freq <-  c((Vg_Oscheius_data_woutCB_sum$P4.p_SSS_sum/Vg_Oscheius_data_woutCB_sum$P4.p_total_sum))  
  Vg_Oscheius_data_woutCB_sum$P4.p_SSSS_freq <-  c((Vg_Oscheius_data_woutCB_sum$P4.p_SSSS_sum/Vg_Oscheius_data_woutCB_sum$P4.p_total_sum))  
  Vg_Oscheius_data_woutCB_sum$P4.p_Other_freq <-  c((Vg_Oscheius_data_woutCB_sum$P4.p_Other_sum/Vg_Oscheius_data_woutCB_sum$P4.p_total_sum))  
  
  Vg_Oscheius_data_woutCB_sum$P5.p_wt_freq <-  c((Vg_Oscheius_data_woutCB_sum$P5.p_wt_sum/Vg_Oscheius_data_woutCB_sum$P5.p_total_sum))  
  Vg_Oscheius_data_woutCB_sum$P5.p_Other_freq <-  c((Vg_Oscheius_data_woutCB_sum$P5.p_Other_sum/Vg_Oscheius_data_woutCB_sum$P5.p_total_sum))
  
  Vg_Oscheius_data_woutCB_sum$P6.p_wt_freq <-  c((Vg_Oscheius_data_woutCB_sum$P6.p_wt_sum/Vg_Oscheius_data_woutCB_sum$P6.p_total_sum))  
  Vg_Oscheius_data_woutCB_sum$P6.p_Other_freq <-  c((Vg_Oscheius_data_woutCB_sum$P6.p_Other_sum/Vg_Oscheius_data_woutCB_sum$P6.p_total_sum))
  
  Vg_Oscheius_data_woutCB_sum$P7.p_wt_freq <-  c((Vg_Oscheius_data_woutCB_sum$P7.p_wt_sum/Vg_Oscheius_data_woutCB_sum$P7.p_total_sum))  
  Vg_Oscheius_data_woutCB_sum$P7.p_Other_freq <-  c((Vg_Oscheius_data_woutCB_sum$P7.p_Other_sum/Vg_Oscheius_data_woutCB_sum$P7.p_total_sum))
  
  Vg_Oscheius_data_woutCB_sum$P8.p_S_freq <-  c((Vg_Oscheius_data_woutCB_sum$P8.p_S_sum/Vg_Oscheius_data_woutCB_sum$P8.p_total_sum))  
  Vg_Oscheius_data_woutCB_sum$P8.p_SS_freq <-  c((Vg_Oscheius_data_woutCB_sum$P8.p_SS_sum/Vg_Oscheius_data_woutCB_sum$P8.p_total_sum))  
  Vg_Oscheius_data_woutCB_sum$P8.p_SSS_freq <-  c((Vg_Oscheius_data_woutCB_sum$P8.p_SSS_sum/Vg_Oscheius_data_woutCB_sum$P8.p_total_sum))  
  Vg_Oscheius_data_woutCB_sum$P8.p_SSSS_freq <-  c((Vg_Oscheius_data_woutCB_sum$P8.p_SSSS_sum/Vg_Oscheius_data_woutCB_sum$P8.p_total_sum))  
  Vg_Oscheius_data_woutCB_sum$P8.p_Other_freq <-  c((Vg_Oscheius_data_woutCB_sum$P8.p_Other_sum/Vg_Oscheius_data_woutCB_sum$P8.p_total_sum))  
  View(Vg_Oscheius_data_woutCB_sum)
}



table(Vg_Oscheius_data_woutCB_sum$Species)
write_xlsx(Vg_Oscheius_data_woutCB_sum, "Vg_Oscheius_data_woutCB_sum-and-frequencies.xlsx")



#PLots----

---- #Raw_Frequencies_Oscheius----

Raw_Frequencies_Oscheius_Vg_mod_dataScale <- subset(Vg_Oscheius_data_woutCB_sum, Treatment =="WILD")
table(Raw_Frequencies_Oscheius_Vg_mod_dataScale$Species)

Raw_Frequencies_Oscheius_Vg_mod_dataScale$Species <- factor(Raw_Frequencies_Oscheius_Vg_mod_dataScale$Species, levels = c("O.tipulae", "O.onirici") )
factor(Raw_Frequencies_Oscheius_Vg_mod_dataScale$Species)

Plot_Raw_Frequencies_Oscheius_Vg_dataScale_P3pP4p_SSSS  <- ggplot(Raw_Frequencies_Oscheius_Vg_mod_dataScale, aes(x = P3.p_SSSS_freq, y = P4.p_SSSS_freq)) + 
  geom_point(aes(colour = Species), shape=19) + scale_colour_manual(values = c("dodgerblue","turquoise"))+  
  facet_grid( rows =vars(Species))  + xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))

pdf("Plot_Raw_Frequencies_Oscheius_Vg_dataScale_P3pP4p_SSSS.pdf")
Plot_Raw_Frequencies_Oscheius_Vg_dataScale_P3pP4p_SSSS
dev.off()


Plot_Raw_Frequencies_Oscheius_Vg_dataScale_P3pP4p_SSSS_v2 <- ggplot(Raw_Frequencies_Oscheius_Vg_mod_dataScale, aes(x = P3.p_SSSS_freq, y = P4.p_SSSS_freq)) + 
  geom_point(aes(colour = Species), shape=19, alpha=0.8) + scale_colour_manual(values = c("dodgerblue","turquoise"))+  
  xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))

pdf("Plot_Raw_Frequencies_Oscheius_Vg_dataScale_P3pP4p_SSSS_v2_boxplot.pdf")
ggMarginal(Plot_Raw_Frequencies_Oscheius_Vg_dataScale_P3pP4p_SSSS_v2, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=20)
dev.off()




---- ##Raw_Frequencies_Otipulae----

Raw_Frequencies_Otipulae_Vg_mod_dataScale <- subset(Raw_Frequencies_Oscheius_Vg_mod_dataScale, Species =="O.tipulae")

Plot_Raw_Frequencies_Otipulae_Vg_dataScale_P3pP4p_SSSS <- ggplot(Raw_Frequencies_Otipulae_Vg_mod_dataScale, aes(x = P3.p_SSSS_freq, y = P4.p_SSSS_freq)) + 
  geom_point(aes(colour = Species), shape=19, alpha=0.8) +scale_colour_manual(values = c("dodgerblue"))+
  xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))

pdf("Plot_Raw_Frequencies_Otipulae_Vg_dataScale_P3pP4p_SSSS_boxplot.pdf")
ggMarginal(Plot_Raw_Frequencies_Otipulae_Vg_dataScale_P3pP4p_SSSS, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=20)
dev.off()


---- ##Raw_Frequencies_Oonirici----

Raw_Frequencies_Oonirici_Vg_mod_dataScale <- subset(Raw_Frequencies_Oscheius_Vg_mod_dataScale, Species =="O.onirici")

Plot_Raw_Frequencies_Oonirici_Vg_dataScale_P3pP4p_SSSS <- ggplot(Raw_Frequencies_Oonirici_Vg_mod_dataScale, aes(x = P3.p_SSSS_freq, y = P4.p_SSSS_freq)) + 
  geom_point(aes(colour = Species), shape=19, alpha=0.8) +scale_colour_manual(values = c("turquoise"))+
  xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))

pdf("Plot_Raw_Frequencies_Oonirici_Vg_dataScale_P3pP4p_SSSS_boxplot.pdf")
ggMarginal(Plot_Raw_Frequencies_Oonirici_Vg_dataScale_P3pP4p_SSSS, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=20)
dev.off()


#Raw_Frequencies_Oscheius_Vg_mod_dataScale_all_Pnp  ---- 
#reshape data to have one column with Pnp_fate and another with the frequency 

Raw_Frequencies_Oscheius_Vg_mod_dataScale_0 <-Raw_Frequencies_Oscheius_Vg_mod_dataScale[,-c(7,33)]

Raw_Frequencies_Oscheius_Vg_mod_dataScale_1 <- Raw_Frequencies_Oscheius_Vg_mod_dataScale_0 %>%
  gather(key = "trait", value = “Trait_frequency”, 7:27)

head(Raw_Frequencies_Oscheius_Vg_mod_dataScale_1)
View(Raw_Frequencies_Oscheius_Vg_mod_dataScale_1)


Raw_Frequencies_Oscheius_Vg_mod_dataScale_2 <- Raw_Frequencies_Oscheius_Vg_mod_dataScale_1 %>%
  separate(trait, into = c("Pn.p", "Pn.p_fate"), sep = "_")

View(Raw_Frequencies_Oscheius_Vg_mod_dataScale_2)

write_xlsx(Raw_Frequencies_Oscheius_Vg_mod_dataScale_2, "Raw_Frequencies_Oscheius_Vg_mod_dataScale_2.xlsx")                                                 

Raw_Frequencies_Oscheius_Vg_mod_dataScale_2_WILD <- subset(Raw_Frequencies_Oscheius_Vg_mod_dataScale_v2, Treatment =="WILD")
head(Raw_Frequencies_Oscheius_Vg_mod_dataScale_2_WILD)

Raw_Frequencies_Oscheius_Vg_mod_dataScale_2_WILD_SS <- subset(Raw_Frequencies_Oscheius_Vg_mod_dataScale_2_WILD, Pn.p_fate=="SS" | Pn.p_fate=="wt")
View(Raw_Frequencies_Oscheius_Vg_mod_dataScale_2_WILD_SS)

Plot_Raw_Frequencies_Oscheius_Vm_dataScale_all_Pnp <- ggplot(Raw_Frequencies_Oscheius_Vg_mod_dataScale_2_WILD_SS, aes( x= Pn.p ,y= Trait_frequency)) +
  geom_point(aes(colour = Species), size=0.7) + scale_colour_manual(values = c("dodgerblue","turquoise")) +  
  facet_grid(rows =vars(Species)) + theme_bw() +
  labs(title = "Raw_Frequencies_Oscheius_Vm_mod_dataScale_all_Pnp", y ="Trait frequency") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 20)), legend.position = "bottom",axis.text.x = element_text(size = 10),axis.title = element_text(size = 15))

pdf("Plot_Raw_Frequencies_Oscheius_Vm_dataScale_all_Pnp.pdf")
Plot_Raw_Frequencies_Oscheius_Vm_dataScale_all_Pnp
dev.off()


#Plots_Raw_Frequencies_Vm_Caenorhabditis
library(readxl)
library(ggplot2)
library(ggdark)
library(ggtext) 
library("ggpubr")
library("ggExtra")


#Data----
Vm_Vg_data<- as.data.frame(read_xlsx("Data S6 phenotyping for VG VM.xlsx"))
View(Vm_Vg_data)
Vm_Vg_data <- Vm_Vg_data[,-c(49,50)]
Vm_data <- subset(Vm_Vg_data, Variance == "Vm" )
Vm_Caenorhabditis_data <- subset(Vm_data, Genus == "Caenorhabditis" )
Vm_Caenorhabditis_data$LineB <- ifelse(Vm_Caenorhabditis_data$Treatment == "CONTROL" ,paste(Vm_Caenorhabditis_data$Line,Vm_Caenorhabditis_data$Block), paste(Vm_Caenorhabditis_data$Line))
Vm_Caenorhabditis_data$BlockRep <- paste(Vm_Caenorhabditis_data$Block,Vm_Caenorhabditis_data$Replicate)
Vm_Caenorhabditis_data$Treatment <- as.factor(Vm_Caenorhabditis_data$Treatment)
Vm_Caenorhabditis_data$Observer <- as.factor(Vm_Caenorhabditis_data$Observer)
View(Vm_Caenorhabditis_data)

Vm_Caenorhabditis_data$P3.p_total <- (Vm_Caenorhabditis_data$P3.p_S + Vm_Caenorhabditis_data$P3.p_SS + Vm_Caenorhabditis_data$P3.p_SSS + Vm_Caenorhabditis_data$P3.p_SSSS + Vm_Caenorhabditis_data$P3.p_Other)
Vm_Caenorhabditis_data$P4.p_total <- (Vm_Caenorhabditis_data$P4.p_S + Vm_Caenorhabditis_data$P4.p_SS + Vm_Caenorhabditis_data$P4.p_SSS + Vm_Caenorhabditis_data$P4.p_SSSS + Vm_Caenorhabditis_data$P4.p_Other)
Vm_Caenorhabditis_data$P8.p_total <- (Vm_Caenorhabditis_data$P8.p_S + Vm_Caenorhabditis_data$P8.p_SS + Vm_Caenorhabditis_data$P8.p_SSS + Vm_Caenorhabditis_data$P8.p_SSSS + Vm_Caenorhabditis_data$P8.p_Other)
Vm_Caenorhabditis_data$P5.p_total <- (Vm_Caenorhabditis_data$P5.p_wt+ Vm_Caenorhabditis_data$P5.p_Other) 
Vm_Caenorhabditis_data$P6.p_total <- (Vm_Caenorhabditis_data$P6.p_wt+ Vm_Caenorhabditis_data$P6.p_Other) 
Vm_Caenorhabditis_data$P7.p_total <- (Vm_Caenorhabditis_data$P7.p_wt+ Vm_Caenorhabditis_data$P7.p_Other) 
View(Vm_Caenorhabditis_data)

table(Vm_Caenorhabditis_data$P3.p_total )
table(Vm_Caenorhabditis_data$P4.p_total )
table(Vm_Caenorhabditis_data$P8.p_total )
table(Vm_Caenorhabditis_data$P5.p_total )
table(Vm_Caenorhabditis_data$P6.p_total )
table(Vm_Caenorhabditis_data$P7.p_total )




#Sum and aggregate data per line
{
  
  sum_Vm_Caenorhabditis_data <- aggregate(cbind(Vm_Caenorhabditis_data$P3.p_S,Vm_Caenorhabditis_data$P3.p_SS,Vm_Caenorhabditis_data$P3.p_SSS,Vm_Caenorhabditis_data$P3.p_SSSS,Vm_Caenorhabditis_data$P3.p_Other,Vm_Caenorhabditis_data$P3.p_total,Vm_Caenorhabditis_data$P4.p_S,Vm_Caenorhabditis_data$P4.p_SS,Vm_Caenorhabditis_data$P4.p_SSS,Vm_Caenorhabditis_data$P4.p_SSSS,Vm_Caenorhabditis_data$P4.p_Other,Vm_Caenorhabditis_data$P4.p_total,Vm_Caenorhabditis_data$P5.p_wt, Vm_Caenorhabditis_data$P5.p_Other,Vm_Caenorhabditis_data$P5.p_total,Vm_Caenorhabditis_data$P6.p_wt, Vm_Caenorhabditis_data$P6.p_Other,Vm_Caenorhabditis_data$P6.p_total, Vm_Caenorhabditis_data$P7.p_wt, Vm_Caenorhabditis_data$P7.p_Other,Vm_Caenorhabditis_data$P7.p_total, Vm_Caenorhabditis_data$P8.p_S,Vm_Caenorhabditis_data$P8.p_SS,Vm_Caenorhabditis_data$P8.p_SSS,Vm_Caenorhabditis_data$P8.p_SSSS,Vm_Caenorhabditis_data$P8.p_Other,Vm_Caenorhabditis_data$P8.p_total), by=list(LineB=Vm_Caenorhabditis_data$LineB), FUN=sum)
  View(sum_Vm_Caenorhabditis_data)
  colnames(sum_Vm_Caenorhabditis_data) <- c("LineB","P3.p_S_sum","P3.p_SS_sum","P3.p_SSS_sum","P3.p_SSSS_sum","P3.p_Other_sum","P3.p_total_sum","P4.p_S_sum", "P4.p_SS_sum","P4.p_SSS_sum","P4.p_SSSS_sum", "P4.p_Other_sum","P4.p_total_sum","P5.p_wt_sum","P5.p_Other_sum","P5.p_total_sum","P6.p_wt_sum","P6.p_Other_sum","P6.p_total_sum","P7.p_wt_sum","P7.p_Other_sum","P7.p_total_sum", "P8.p_S_sum", "P8.p_SS_sum","P8.p_SSS_sum","P8.p_SSSS_sum", "P8.p_Other_sum","P8.p_total_sum")
 names(Vm_Caenorhabditis_data)
  
  
  Vm_Caenorhabditis_data_sum <- merge(unique(Vm_Caenorhabditis_data[,c("Variance","Genus","Species","Ancestral","Treatment","LineB" )]), sum_Vm_Caenorhabditis_data)         
  View(Vm_Caenorhabditis_data_sum)
  
  
}  

#frequency
{
 Vm_Caenorhabditis_data_sum$P3.p_S_freq <-  c((Vm_Caenorhabditis_data_sum$P3.p_S_sum/Vm_Caenorhabditis_data_sum$P3.p_total_sum))  
 Vm_Caenorhabditis_data_sum$P3.p_SS_freq <-  c((Vm_Caenorhabditis_data_sum$P3.p_SS_sum/Vm_Caenorhabditis_data_sum$P3.p_total_sum))  
 Vm_Caenorhabditis_data_sum$P3.p_SSS_freq <-  c((Vm_Caenorhabditis_data_sum$P3.p_SSS_sum/Vm_Caenorhabditis_data_sum$P3.p_total_sum))  
 Vm_Caenorhabditis_data_sum$P3.p_SSSS_freq <-  c((Vm_Caenorhabditis_data_sum$P3.p_SSSS_sum/Vm_Caenorhabditis_data_sum$P3.p_total_sum))  
 Vm_Caenorhabditis_data_sum$P3.p_Other_freq <-  c((Vm_Caenorhabditis_data_sum$P3.p_Other_sum/Vm_Caenorhabditis_data_sum$P3.p_total_sum))  
  
 Vm_Caenorhabditis_data_sum$P4.p_S_freq <-  c((Vm_Caenorhabditis_data_sum$P4.p_S_sum/Vm_Caenorhabditis_data_sum$P4.p_total_sum))  
 Vm_Caenorhabditis_data_sum$P4.p_SS_freq <-  c((Vm_Caenorhabditis_data_sum$P4.p_SS_sum/Vm_Caenorhabditis_data_sum$P4.p_total_sum))  
 Vm_Caenorhabditis_data_sum$P4.p_SSS_freq <-  c((Vm_Caenorhabditis_data_sum$P4.p_SSS_sum/Vm_Caenorhabditis_data_sum$P4.p_total_sum))  
 Vm_Caenorhabditis_data_sum$P4.p_SSSS_freq <-  c((Vm_Caenorhabditis_data_sum$P4.p_SSSS_sum/Vm_Caenorhabditis_data_sum$P4.p_total_sum))  
 Vm_Caenorhabditis_data_sum$P4.p_Other_freq <-  c((Vm_Caenorhabditis_data_sum$P4.p_Other_sum/Vm_Caenorhabditis_data_sum$P4.p_total_sum))  
 
 Vm_Caenorhabditis_data_sum$P5.p_wt_freq <-  c((Vm_Caenorhabditis_data_sum$P5.p_wt_sum/Vm_Caenorhabditis_data_sum$P5.p_total_sum))  
 Vm_Caenorhabditis_data_sum$P5.p_Other_freq <-  c((Vm_Caenorhabditis_data_sum$P5.p_Other_sum/Vm_Caenorhabditis_data_sum$P5.p_total_sum))
 
 Vm_Caenorhabditis_data_sum$P6.p_wt_freq <-  c((Vm_Caenorhabditis_data_sum$P6.p_wt_sum/Vm_Caenorhabditis_data_sum$P6.p_total_sum))  
 Vm_Caenorhabditis_data_sum$P6.p_Other_freq <-  c((Vm_Caenorhabditis_data_sum$P6.p_Other_sum/Vm_Caenorhabditis_data_sum$P6.p_total_sum))
 
 Vm_Caenorhabditis_data_sum$P7.p_wt_freq <-  c((Vm_Caenorhabditis_data_sum$P7.p_wt_sum/Vm_Caenorhabditis_data_sum$P7.p_total_sum))  
 Vm_Caenorhabditis_data_sum$P7.p_Other_freq <-  c((Vm_Caenorhabditis_data_sum$P7.p_Other_sum/Vm_Caenorhabditis_data_sum$P7.p_total_sum))
  
 Vm_Caenorhabditis_data_sum$P8.p_S_freq <-  c((Vm_Caenorhabditis_data_sum$P8.p_S_sum/Vm_Caenorhabditis_data_sum$P8.p_total_sum))  
 Vm_Caenorhabditis_data_sum$P8.p_SS_freq <-  c((Vm_Caenorhabditis_data_sum$P8.p_SS_sum/Vm_Caenorhabditis_data_sum$P8.p_total_sum))  
 Vm_Caenorhabditis_data_sum$P8.p_SSS_freq <-  c((Vm_Caenorhabditis_data_sum$P8.p_SSS_sum/Vm_Caenorhabditis_data_sum$P8.p_total_sum))  
 Vm_Caenorhabditis_data_sum$P8.p_SSSS_freq <-  c((Vm_Caenorhabditis_data_sum$P8.p_SSSS_sum/Vm_Caenorhabditis_data_sum$P8.p_total_sum))  
 Vm_Caenorhabditis_data_sum$P8.p_Other_freq <-  c((Vm_Caenorhabditis_data_sum$P8.p_Other_sum/Vm_Caenorhabditis_data_sum$P8.p_total_sum))  
  View(Vm_Caenorhabditis_data_sum)
}

Vm_Caenorhabditis_data_sum <- Vm_Caenorhabditis_data_sum %>% mutate(Treatment= str_replace(Treatment,"CONTROL", 'Ancestral'))
Vm_Caenorhabditis_data_sum <- Vm_Caenorhabditis_data_sum %>% mutate(Treatment= str_replace(Treatment,'MA', 'ML'))
table(Vm_Caenorhabditis_data_sum$Treatment)
factor(Vm_Caenorhabditis_data_sum$Treatment)


write_xlsx(Vm_Caenorhabditis_data_sum, "Vm_Caenorhabditis_sum-and-frequencies.xlsx")


#PLots----

---- #Raw_Frequencies_Celegans----
s
Raw_Frequencies_Celegans_Vm_mod_dataScale <- subset(Vm_Caenorhabditis_data_sum, Species =="C.elegans")

Plot_Raw_Frequencies_Celegans_Vm_dataScale_P3pP4p <- ggplot(Raw_Frequencies_Celegans_Vm_mod_dataScale, aes(x = 1- P3.p_S_freq, y = 1- P4.p_S_freq)) + 
  geom_point(aes(colour = Treatment, shape=Treatment,alpha = Treatment, size=Treatment))  + geom_point(data=subset(Raw_Frequencies_Celegans_Vm_mod_dataScale, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment,alpha = Treatment)) +  scale_alpha_discrete( range = c(0.8, 1)) +scale_size_manual(values = c(1.5 , 2.5))+    scale_shape_manual(values = c(16 , 19))+
  scale_colour_manual(values = c("red4","red"))+
  facet_grid( rows =vars(Ancestral))  + xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))


pdf("Plot_Raw_Frequencies_Celegans_Vm_dataScale_P3pP4p.pdf")
Plot_Raw_Frequencies_Celegans_Vm_dataScale_P3pP4p
dev.off()



#Raw_Frequencies_JU1200_Vm
{
  
  
  Raw_Frequencies_JU1200_Vm_mod_dataScale <- subset(Vm_Caenorhabditis_data_sum, Ancestral =="JU1200")
  table(Raw_Frequencies_JU1200_Vm_mod_dataScale$Treatment)
  
  
  Plot_Raw_Frequencies_JU1200_Vm_dataScale_P3pP4p <- ggplot(Raw_Frequencies_JU1200_Vm_mod_dataScale, aes(x = 1- P3.p_S_freq, y = 1- P4.p_S_freq)) + 
    geom_point(aes(colour = Treatment, shape=Treatment, size=Treatment))+ geom_point(data=subset(Raw_Frequencies_JU1200_Vm_mod_dataScale, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment), alpha=0.8) +scale_size_manual(values = c(1.5 , 3))+ scale_shape_manual(values = c(16 , 19))+
    scale_colour_manual(values = c("red4","red")) +
    xlim(0, 1) + ylim(0,1)+
    theme_bw() + labs( x = "Frequency of P3.p division Frequency", y = "Frequency of P4.p division Frequency") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))
  
    
  
  pdf("Plot_Raw_Frequencies_JU1200_Vm_dataScale_P3pP4p_boxplot.pdf")
  ggMarginal(Plot_Raw_Frequencies_JU1200_Vm_dataScale_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=20)
  dev.off() 
  
  
}

#Raw_Frequencies_PB306_Vm
{
  
  Raw_Frequencies_PB306_Vm_mod_dataScale <- subset(Vm_Caenorhabditis_data_sum, Ancestral =="PB306")
  table(Raw_Frequencies_PB306_Vm_mod_dataScale$Treatment)
  
  
  Plot_Raw_Frequencies_PB306_Vm_dataScale_P3pP4p <- ggplot(Raw_Frequencies_PB306_Vm_mod_dataScale, aes(x = 1- P3.p_S_freq, y = 1- P4.p_S_freq)) + 
    geom_point(aes(colour = Treatment, shape=Treatment, size=Treatment))+ geom_point(data=subset(Raw_Frequencies_PB306_Vm_mod_dataScale, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment), alpha=0.8) +scale_size_manual(values = c(1.5 , 3))+ scale_shape_manual(values = c(16 , 19))+
    scale_colour_manual(values = c("red4","red")) +
    xlim(0, 1) + ylim(0,1)+
    theme_bw() + labs( x = "Frequency of P3.p division Frequency", y = "Frequency of P4.p division Frequency") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))
  
  
  pdf("Plot_Raw_Frequencies_PB306_Vm_dataScale_P3pP4p_boxplot.pdf")
  ggMarginal(Plot_Raw_Frequencies_PB306_Vm_dataScale_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=20)
  dev.off() 
  
  
}    

---- #Raw_Frequencies_Cbriggsae----
s
Raw_Frequencies_Cbriggsae_Vm_mod_dataScale <- subset(Vm_Caenorhabditis_data_sum, Species =="C.briggsae")

Plot_Raw_Frequencies_Cbriggsae_Vm_dataScale_P3pP4p <- ggplot(Raw_Frequencies_Cbriggsae_Vm_mod_dataScale, aes(x = 1- P3.p_S_freq, y = 1- P4.p_S_freq)) + 
  geom_point(aes(colour = Treatment, shape=Treatment,alpha = Treatment, size=Treatment))  + geom_point(data=subset(Raw_Frequencies_Cbriggsae_Vm_mod_dataScale, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment,alpha = Treatment)) +  scale_alpha_discrete( range = c(0.8, 1)) +scale_size_manual(values = c(1.5 , 2.5))+    scale_shape_manual(values = c(16 , 19))+
  scale_colour_manual(values = c("pink4","deeppink"))+
  facet_grid( rows =vars(Ancestral))  + xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))


pdf("Plot_Raw_Frequencies_Cbriggsae_Vm_dataScale_P3pP4p.pdf")
Plot_Raw_Frequencies_Cbriggsae_Vm_dataScale_P3pP4p
dev.off()




#Raw_Frequencies_AF16_Vm
{
  Raw_Frequencies_AF16_Vm_mod_dataScale <- subset(Vm_Caenorhabditis_data_sum, Ancestral =="AF16")
  
  table(Raw_Frequencies_AF16_Vm_mod_dataScale$Treatment)
  
  Plot_Raw_Frequencies_AF16_Vm_dataScale_P3pP4p <- ggplot(Raw_Frequencies_AF16_Vm_mod_dataScale, aes(x = 1- P3.p_S_freq, y = 1- P4.p_S_freq)) + 
    geom_point(aes(colour = Treatment, shape=Treatment, size=Treatment))+ geom_point(data=subset(Raw_Frequencies_AF16_Vm_mod_dataScale, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment), alpha=0.8) +scale_size_manual(values = c(1.5 , 3))+ scale_shape_manual(values = c(16 , 19))+
    scale_colour_manual(values = c("pink4","deeppink")) +
    xlim(0, 1) + ylim(0,1)+
    theme_bw() + labs( x = "Frequency of P3.p division Frequency", y = "Frequency of P4.p division Frequency") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))
  
 
  
  pdf("Plot_Raw_Frequencies_AF16_Vm_dataScale_P3pP4p_boxplot.pdf")
  ggMarginal(Plot_Raw_Frequencies_AF16_Vm_dataScale_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=20)
  dev.off() 
  
  
}

#Raw_Frequencies_PB800_Vm
{
  Raw_Frequencies_PB800_Vm_mod_dataScale <- subset(Vm_Caenorhabditis_data_sum, Ancestral =="PB800")
  table(Raw_Frequencies_PB800_Vm_mod_dataScale$Treatment)
  
  Plot_Raw_Frequencies_PB800_Vm_dataScale_P3pP4p <- ggplot(Raw_Frequencies_PB800_Vm_mod_dataScale, aes(x = 1- P3.p_S_freq, y = 1- P4.p_S_freq)) + 
    geom_point(aes(colour = Treatment, shape=Treatment, size=Treatment))+ geom_point(data=subset(Raw_Frequencies_PB800_Vm_mod_dataScale, Treatment == 'Ancestral'), aes(colour = Treatment, size=Treatment), alpha=0.8) +scale_size_manual(values = c(1.5 , 3))+ scale_shape_manual(values = c(16 , 19))+
    scale_colour_manual(values = c("pink4","deeppink")) +
    xlim(0, 1) + ylim(0,1)+
    theme_bw() + labs( x = "Frequency of P3.p division Frequency", y = "Frequency of P4.p division Frequency") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))
  
 
  pdf("Plot_Raw_Frequencies_PB800_Vm_dataScale_P3pP4p_boxplot.pdf")
  ggMarginal(Plot_Raw_Frequencies_PB800_Vm_dataScale_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=20)
  dev.off() 
  
  
}    

#Raw_Frequencies_Caenorhabditis_Vm_mod_dataScale_all_Pnp  ----                                                  
Vm_Caenorhabditis_data_sum

#reshape data to have one column with Pnp_fate and another with the frequency 

Vm_Caenorhabditis_frequencies_0 <-Vm_Caenorhabditis_data_sum[,-c(7,33)]

Vm_Caenorhabditis_frequencies_1 <- Vm_Caenorhabditis_frequencies_0 %>%
  gather(key = "trait", value = “Trait_frequency”, 7:27)

head(Vm_Caenorhabditis_frequencies_1)
View(Vm_Caenorhabditis_frequencies_1)


Vm_Caenorhabditis_frequencies_2 <- Vm_Caenorhabditis_frequencies_1 %>%
  separate(trait, into = c("Pn.p", "Pn.p_fate"), sep = "_")

View(Vm_Caenorhabditis_frequencies_2)

write_xlsx(Vm_Caenorhabditis_frequencies_2, "Vm_Caenorhabditis_frequencies_2.xlsx")



Vm_Caenorhabditis_data_frequencies_2$Ancestor <- factor(Vm_Caenorhabditis_data_frequencies_2$Ancestor, levels = c("PB306", "JU1200","AF16","PB800") )
factor(Vm_Caenorhabditis_data_frequencies_2$Ancestor)

Plot_Raw_Frequencies_Caenorhabditis_Vm_dataScale_all_Pnp <- ggplot(Vm_Caenorhabditis_data_frequencies_2, aes( x= Treatment ,y= Trait_frequency)) +
  geom_point(aes(colour=interaction(Treatment, Species, sep=':')), size=0.7)+  scale_colour_manual(values = c("pink4","deeppink","red4","red"))+
  facet_grid(Ancestor~Pn.p)+ theme_bw() +
  labs(title = "Raw_Frequencies_Caenorhabditis_Vm_mod_dataScale_all_Pnp", y ="Trait frequency") + theme( axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))

pdf("Plot_Raw_Frequencies_Caenorhabditis_Vm_dataScale_all_Pnp.pdf")
Plot_Raw_Frequencies_Caenorhabditis_Vm_dataScale_all_Pnp
dev.off()






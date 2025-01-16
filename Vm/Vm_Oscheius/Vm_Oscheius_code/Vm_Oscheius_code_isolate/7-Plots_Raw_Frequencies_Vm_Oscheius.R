#Plots_Raw_Frequencies_Vm_Oscheius
library(readxl)
library(ggplot2)
library(ggdark)
library(ggtext) 

#Data----
Vm_Vg_data<- as.data.frame(read_xlsx("Data S6 phenotyping for VG VM.xlsx"))
View(Vm_Vg_data)
Vm_Vg_data <- Vm_Vg_data[,-c(49,50)]
Vm_data_v2 <- subset(Vm_Vg_data, Variance == "Vm" )
Vm_Oscheius_data <- subset(Vm_data_v2, Genus == "Oscheius" )
Vm_Oscheius_data$LineB <- ifelse(Vm_Oscheius_data$Treatment == "CONTROL" ,paste(Vm_Oscheius_data$Line,Vm_Oscheius_data$Block), paste(Vm_Oscheius_data$Line))
Vm_Oscheius_data$BlockRep <- paste(Vm_Oscheius_data$Block,Vm_Oscheius_data$Replicate)
Vm_Oscheius_data$Treatment <- as.factor(Vm_Oscheius_data$Treatment)
Vm_Oscheius_data$Observer <- as.factor(Vm_Oscheius_data$Observer)
View(Vm_Oscheius_data)

Vm_Oscheius_data$P3.p_total <- (Vm_Oscheius_data$P3.p_S + Vm_Oscheius_data$P3.p_SS + Vm_Oscheius_data$P3.p_SSS + Vm_Oscheius_data$P3.p_SSSS + Vm_Oscheius_data$P3.p_Other)
Vm_Oscheius_data$P4.p_total <- (Vm_Oscheius_data$P4.p_S + Vm_Oscheius_data$P4.p_SS + Vm_Oscheius_data$P4.p_SSS + Vm_Oscheius_data$P4.p_SSSS + Vm_Oscheius_data$P4.p_Other)
Vm_Oscheius_data$P8.p_total <- (Vm_Oscheius_data$P8.p_S + Vm_Oscheius_data$P8.p_SS + Vm_Oscheius_data$P8.p_SSS + Vm_Oscheius_data$P8.p_SSSS + Vm_Oscheius_data$P8.p_Other)
Vm_Oscheius_data$P5.p_total <- (Vm_Oscheius_data$P5.p_wt+ Vm_Oscheius_data$P5.p_Other) 
Vm_Oscheius_data$P6.p_total <- (Vm_Oscheius_data$P6.p_wt+ Vm_Oscheius_data$P6.p_Other) 
Vm_Oscheius_data$P7.p_total <- (Vm_Oscheius_data$P7.p_wt+ Vm_Oscheius_data$P7.p_Other) 
View(Vm_Oscheius_data)

table(Vm_Oscheius_data$P3.p_total )
table(Vm_Oscheius_data$P4.p_total )
table(Vm_Oscheius_data$P8.p_total )
table(Vm_Oscheius_data$P5.p_total )
table(Vm_Oscheius_data$P6.p_total )
table(Vm_Oscheius_data$P7.p_total )




#Sum and aggregate data per line
{
  
  sum_Vm_Oscheius_data <- aggregate(cbind(Vm_Oscheius_data$P3.p_S,Vm_Oscheius_data$P3.p_SS,Vm_Oscheius_data$P3.p_SSS,Vm_Oscheius_data$P3.p_SSSS,Vm_Oscheius_data$P3.p_Other,Vm_Oscheius_data$P3.p_total,Vm_Oscheius_data$P4.p_S,Vm_Oscheius_data$P4.p_SS,Vm_Oscheius_data$P4.p_SSS,Vm_Oscheius_data$P4.p_SSSS,Vm_Oscheius_data$P4.p_Other,Vm_Oscheius_data$P4.p_total,Vm_Oscheius_data$P5.p_wt, Vm_Oscheius_data$P5.p_Other,Vm_Oscheius_data$P5.p_total,Vm_Oscheius_data$P6.p_wt, Vm_Oscheius_data$P6.p_Other,Vm_Oscheius_data$P6.p_total, Vm_Oscheius_data$P7.p_wt, Vm_Oscheius_data$P7.p_Other,Vm_Oscheius_data$P7.p_total, Vm_Oscheius_data$P8.p_S,Vm_Oscheius_data$P8.p_SS,Vm_Oscheius_data$P8.p_SSS,Vm_Oscheius_data$P8.p_SSSS,Vm_Oscheius_data$P8.p_Other,Vm_Oscheius_data$P8.p_total), by=list(LineB=Vm_Oscheius_data$LineB), FUN=sum)
  View(sum_Vm_Oscheius_data)
  colnames(sum_Vm_Oscheius_data) <- c("LineB","P3.p_S_sum","P3.p_SS_sum","P3.p_SSS_sum","P3.p_SSSS_sum","P3.p_Other_sum","P3.p_total_sum","P4.p_S_sum", "P4.p_SS_sum","P4.p_SSS_sum","P4.p_SSSS_sum", "P4.p_Other_sum","P4.p_total_sum","P5.p_wt_sum","P5.p_Other_sum","P5.p_total_sum","P6.p_wt_sum","P6.p_Other_sum","P6.p_total_sum","P7.p_wt_sum","P7.p_Other_sum","P7.p_total_sum", "P8.p_S_sum", "P8.p_SS_sum","P8.p_SSS_sum","P8.p_SSSS_sum", "P8.p_Other_sum","P8.p_total_sum")
  names(Vm_Oscheius_data)
  
  Vm_Oscheius_data_sum <- merge(unique(Vm_Oscheius_data[,c("Variance","Genus","Species","Ancestral","Treatment","LineB" )]), sum_Vm_Oscheius_data)         
  View(Vm_Oscheius_data_sum)
  
  
}  

#frequency
{
  Vm_Oscheius_data_sum$P3.p_S_freq <-  c((Vm_Oscheius_data_sum$P3.p_S_sum/Vm_Oscheius_data_sum$P3.p_total_sum))  
  Vm_Oscheius_data_sum$P3.p_SS_freq <-  c((Vm_Oscheius_data_sum$P3.p_SS_sum/Vm_Oscheius_data_sum$P3.p_total_sum))  
  Vm_Oscheius_data_sum$P3.p_SSS_freq <-  c((Vm_Oscheius_data_sum$P3.p_SSS_sum/Vm_Oscheius_data_sum$P3.p_total_sum))  
  Vm_Oscheius_data_sum$P3.p_SSSS_freq <-  c((Vm_Oscheius_data_sum$P3.p_SSSS_sum/Vm_Oscheius_data_sum$P3.p_total_sum))  
  Vm_Oscheius_data_sum$P3.p_Other_freq <-  c((Vm_Oscheius_data_sum$P3.p_Other_sum/Vm_Oscheius_data_sum$P3.p_total_sum))  
  
  Vm_Oscheius_data_sum$P4.p_S_freq <-  c((Vm_Oscheius_data_sum$P4.p_S_sum/Vm_Oscheius_data_sum$P4.p_total_sum))  
  Vm_Oscheius_data_sum$P4.p_SS_freq <-  c((Vm_Oscheius_data_sum$P4.p_SS_sum/Vm_Oscheius_data_sum$P4.p_total_sum))  
  Vm_Oscheius_data_sum$P4.p_SSS_freq <-  c((Vm_Oscheius_data_sum$P4.p_SSS_sum/Vm_Oscheius_data_sum$P4.p_total_sum))  
  Vm_Oscheius_data_sum$P4.p_SSSS_freq <-  c((Vm_Oscheius_data_sum$P4.p_SSSS_sum/Vm_Oscheius_data_sum$P4.p_total_sum))  
  Vm_Oscheius_data_sum$P4.p_Other_freq <-  c((Vm_Oscheius_data_sum$P4.p_Other_sum/Vm_Oscheius_data_sum$P4.p_total_sum))  
  
  Vm_Oscheius_data_sum$P5.p_wt_freq <-  c((Vm_Oscheius_data_sum$P5.p_wt_sum/Vm_Oscheius_data_sum$P5.p_total_sum))  
  Vm_Oscheius_data_sum$P5.p_Other_freq <-  c((Vm_Oscheius_data_sum$P5.p_Other_sum/Vm_Oscheius_data_sum$P5.p_total_sum))
  
  Vm_Oscheius_data_sum$P6.p_wt_freq <-  c((Vm_Oscheius_data_sum$P6.p_wt_sum/Vm_Oscheius_data_sum$P6.p_total_sum))  
  Vm_Oscheius_data_sum$P6.p_Other_freq <-  c((Vm_Oscheius_data_sum$P6.p_Other_sum/Vm_Oscheius_data_sum$P6.p_total_sum))
  
  Vm_Oscheius_data_sum$P7.p_wt_freq <-  c((Vm_Oscheius_data_sum$P7.p_wt_sum/Vm_Oscheius_data_sum$P7.p_total_sum))  
  Vm_Oscheius_data_sum$P7.p_Other_freq <-  c((Vm_Oscheius_data_sum$P7.p_Other_sum/Vm_Oscheius_data_sum$P7.p_total_sum))
  
  Vm_Oscheius_data_sum$P8.p_S_freq <-  c((Vm_Oscheius_data_sum$P8.p_S_sum/Vm_Oscheius_data_sum$P8.p_total_sum))  
  Vm_Oscheius_data_sum$P8.p_SS_freq <-  c((Vm_Oscheius_data_sum$P8.p_SS_sum/Vm_Oscheius_data_sum$P8.p_total_sum))  
  Vm_Oscheius_data_sum$P8.p_SSS_freq <-  c((Vm_Oscheius_data_sum$P8.p_SSS_sum/Vm_Oscheius_data_sum$P8.p_total_sum))  
  Vm_Oscheius_data_sum$P8.p_SSSS_freq <-  c((Vm_Oscheius_data_sum$P8.p_SSSS_sum/Vm_Oscheius_data_sum$P8.p_total_sum))  
  Vm_Oscheius_data_sum$P8.p_Other_freq <-  c((Vm_Oscheius_data_sum$P8.p_Other_sum/Vm_Oscheius_data_sum$P8.p_total_sum))  
  View(Vm_Oscheius_data_sum)
}

Vm_Oscheius_data_sum <- Vm_Oscheius_data_sum %>% mutate(Treatment= str_replace(Treatment,"Ancestral", 'Ancestral'))
Vm_Oscheius_data_sum <- Vm_Oscheius_data_sum %>% mutate(Treatment= str_replace(Treatment,'MA', 'ML'))
table(Vm_Oscheius_data_sum$Treatment)
factor(Vm_Oscheius_data_sum$Treatment)

#Vm_Oscheius_data_sum$Treatment <- factor(Vm_Oscheius_data_sum$Treatment, levels = c("CONTROL", "MA") )

write_xlsx(Vm_Oscheius_data_sum, "Vm_Oscheius_sum-and-frequencies.xlsx")


#PLots----

---- #Raw_Frequencies_Otipulae----
s
Raw_Frequencies_Otipulae_Vm_mod_dataScale <- subset(Vm_Oscheius_data_sum, Species =="O.tipulae")

Plot_Raw_Frequencies_Otipulae_Vm_dataScale_P3pP4p <- ggplot(Raw_Frequencies_Otipulae_Vm_mod_dataScale, aes(x = P3.p_SSSS_freq, y = P4.p_SSSS_freq)) + 
  geom_point(aes(colour = Treatment, shape=Treatment,alpha = Treatment, size=Treatment))  + geom_point(data=subset(Raw_Frequencies_Otipulae_Vm_mod_dataScale, Treatment == "Ancestral"), aes(colour = Treatment, size=Treatment,alpha = Treatment)) +  scale_alpha_discrete( range = c(0.8, 1)) +scale_size_manual(values = c(1.5 , 2.5))+    scale_shape_manual(values = c(16 , 19))+
  scale_colour_manual(values = c("royalblue4","deepskyblue"))+
  facet_grid( rows =vars(Ancestral))  + xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))

pdf("Plot_Raw_Frequencies_Otipulae_Vm_dataScale_P3pP4p.pdf")
Plot_Raw_Frequencies_Otipulae_Vm_dataScale_P3pP4p
dev.off()




#Raw_Frequencies_CEW1_Vm
{
  
  
  Raw_Frequencies_CEW1_Vm_mod_dataScale <- subset(Vm_Oscheius_data_sum, Ancestral =="CEW1")
  table(Raw_Frequencies_CEW1_Vm_mod_dataScale$Treatment)
  
  
  Plot_Raw_Frequencies_CEW1_Vm_dataScale_P3pP4p <- ggplot(Raw_Frequencies_CEW1_Vm_mod_dataScale, aes(x = P3.p_SSSS_freq, y = P4.p_SSSS_freq)) + 
    geom_point(aes(colour = Treatment, shape=Treatment, size=Treatment))+ geom_point(data=subset(Raw_Frequencies_CEW1_Vm_mod_dataScale, Treatment == "Ancestral"), aes(colour = Treatment, size=Treatment), alpha=0.8) +scale_size_manual(values = c(1.5 , 3))+ scale_shape_manual(values = c(16 , 19))+
    scale_colour_manual(values = c("royalblue4","deepskyblue")) +
    xlim(0, 1) + ylim(0,1)+
    theme_bw() + labs( x = "Frequency of P3.p division Frequency", y = "Frequency of P4.p division Frequency") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))
  
  
  
  pdf("Plot_Raw_Frequencies_CEW1_Vm_dataScale_P3pP4p_boxplot.pdf")
  ggMarginal(Plot_Raw_Frequencies_CEW1_Vm_dataScale_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
  dev.off() 
  
  
}

#Raw_Frequencies_JU178_Vm
{
  
  Raw_Frequencies_JU178_Vm_mod_dataScale <- subset(Vm_Oscheius_data_sum, Ancestral =="JU178")
  table(Raw_Frequencies_JU178_Vm_mod_dataScale$Treatment)
  
  
  Plot_Raw_Frequencies_JU178_Vm_dataScale_P3pP4p <- ggplot(Raw_Frequencies_JU178_Vm_mod_dataScale, aes(x = P3.p_SSSS_freq, y = P4.p_SSSS_freq)) + 
    geom_point(aes(colour = Treatment, shape=Treatment, size=Treatment))+ geom_point(data=subset(Raw_Frequencies_JU178_Vm_mod_dataScale, Treatment == "Ancestral"), aes(colour = Treatment, size=Treatment), alpha=0.8) +scale_size_manual(values = c(1.5 , 3))+ scale_shape_manual(values = c(16 , 19))+
    scale_colour_manual(values = c("royalblue4","deepskyblue")) +
    xlim(0, 1) + ylim(0,1)+
    theme_bw() + labs( x = "Frequency of P3.p division Frequency", y = "Frequency of P4.p division Frequency") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))
  
  
  pdf("Plot_Raw_Frequencies_JU178_Vm_dataScale_P3pP4p_boxplot.pdf")
  ggMarginal(Plot_Raw_Frequencies_JU178_Vm_dataScale_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
  dev.off() 
  
  
}    

---- #Raw_Frequencies_Oonirici----
s
Raw_Frequencies_Oonirici_Vm_mod_dataScale <- subset(Vm_Oscheius_data_sum, Species =="O.onirici")

Plot_Raw_Frequencies_Oonirici_Vm_dataScale_P3pP4p <- ggplot(Raw_Frequencies_Oonirici_Vm_mod_dataScale, aes(x = P3.p_SSSS_freq, y = P4.p_SSSS_freq)) + 
  geom_point(aes(colour = Treatment, shape=Treatment,alpha = Treatment, size=Treatment))  + geom_point(data=subset(Raw_Frequencies_Oonirici_Vm_mod_dataScale, Treatment == "Ancestral"), aes(colour = Treatment, size=Treatment,alpha = Treatment)) +  scale_alpha_discrete( range = c(0.8, 1)) +scale_size_manual(values = c(1.5 , 2.5))+    scale_shape_manual(values = c(16 , 19))+
  scale_colour_manual(values = c("skyblue4","turquoise"))+
  facet_grid( rows =vars(Ancestral))  + xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))


pdf("Plot_Raw_Frequencies_Oonirici_Vm_dataScale_P3pP4p.pdf")
Plot_Raw_Frequencies_Oonirici_Vm_dataScale_P3pP4p
dev.off()



#Raw_Frequencies_PS2068_Vm
{
  Raw_Frequencies_PS2068_Vm_mod_dataScale <- subset(Vm_Oscheius_data_sum, Ancestral =="PS2068")
  
  table(Raw_Frequencies_PS2068_Vm_mod_dataScale$Treatment)
  
  Plot_Raw_Frequencies_PS2068_Vm_dataScale_P3pP4p <- ggplot(Raw_Frequencies_PS2068_Vm_mod_dataScale, aes(x = P3.p_SSSS_freq, y = P4.p_SSSS_freq)) + 
    geom_point(aes(colour = Treatment, shape=Treatment, size=Treatment))+ geom_point(data=subset(Raw_Frequencies_PS2068_Vm_mod_dataScale, Treatment == "Ancestral"), aes(colour = Treatment, size=Treatment), alpha=0.8) +scale_size_manual(values = c(1.5 , 3))+ scale_shape_manual(values = c(16 , 19))+
    scale_colour_manual(values = c("skyblue4","turquoise")) +
    xlim(0, 1) + ylim(0,1)+
    theme_bw() + labs( x = "Frequency of P3.p division Frequency", y = "Frequency of P4.p division Frequency") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))
  
  
  pdf("Plot_Raw_Frequencies_PS2068_Vm_dataScale_P3pP4p_boxplot.pdf")
  ggMarginal(Plot_Raw_Frequencies_PS2068_Vm_dataScale_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
  dev.off() 
  
  
}

#Raw_Frequencies_JU77_Vm
{
  Raw_Frequencies_JU77_Vm_mod_dataScale <- subset(Vm_Oscheius_data_sum, Ancestral =="JU77")
  table(Raw_Frequencies_JU77_Vm_mod_dataScale$Treatment)
  
  Plot_Raw_Frequencies_JU77_Vm_dataScale_P3pP4p <- ggplot(Raw_Frequencies_JU77_Vm_mod_dataScale, aes(x = P3.p_SSSS_freq, y = P4.p_SSSS_freq)) + 
    geom_point(aes(colour = Treatment, shape=Treatment, size=Treatment))+ geom_point(data=subset(Raw_Frequencies_JU77_Vm_mod_dataScale, Treatment == "Ancestral"), aes(colour = Treatment, size=Treatment), alpha=0.8) +scale_size_manual(values = c(1.5 , 3))+ scale_shape_manual(values = c(16 , 19))+
    scale_colour_manual(values = c("skyblue4","turquoise")) +
    xlim(0, 1) + ylim(0,1)+
    theme_bw() + labs( x = "Frequency of P3.p division Frequency", y = "Frequency of P4.p division Frequency") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))
  
 
  pdf("Plot_Raw_Frequencies_JU77_Vm_dataScale_P3pP4p_boxplot.pdf")
  ggMarginal(Plot_Raw_Frequencies_JU77_Vm_dataScale_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
  dev.off() 
  
  
}    

#BLUP_Oscheius_Vm_mod_dataScale_all_Pnp  ----                                                  


#reshape data to have one column with Pnp_fate and another with the frequency 

Vm_Oscheius_frequencies_0 <-Vm_Oscheius_data_sum[,-c(7,33)]

Vm_Oscheius_frequencies_1 <- Vm_Oscheius_frequencies_0 %>%
  gather(key = "trait", value = “Trait_frequency”, 7:27)

head(Vm_Oscheius_frequencies_1)
View(Vm_Oscheius_frequencies_1)


Vm_Oscheius_frequencies_2 <- Vm_Oscheius_frequencies_1 %>%
  separate(trait, into = c("Pn.p", "Pn.p_fate"), sep = "_")

View(Vm_Oscheius_frequencies_2)

write_xlsx(Vm_Oscheius_frequencies_2, "Vm_Oscheius_frequencies_2.xlsx")



Plot_Raw_Frequencies_Oscheius_Vm_dataScale_all_Pnp <- ggplot(Vm_Oscheius_frequencies_2, aes( x= Treatment ,y= Trait_frequency)) +
  geom_point(aes(colour=interaction(Treatment, Species, sep=':')), size=0.7)+  scale_colour_manual(values = c("royalblue4","deepskyblue","skyblue4","turquoise"))+
  facet_grid(Ancestral~Pn.p)+ theme_bw() +
  labs(title = "Raw_Frequencies_Oscheius_Vm_mod_dataScale_all_Pnp", y ="Trait frequency") + theme( axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom",axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),axis.title = element_text(size = 15))

pdf("Plot_Raw_Frequencies_Oscheius_Vm_dataScale_all_Pnp.pdf")
Plot_Raw_Frequencies_Oscheius_Vm_dataScale_all_Pnp
dev.off()




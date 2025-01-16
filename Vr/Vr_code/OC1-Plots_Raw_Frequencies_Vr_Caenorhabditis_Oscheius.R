#Plots_Raw_Frequencies_Vr_Caenorhabditis_Oscheius
library(readxl)
library(ggplot2)
library(ggtext) 
library(ggExtra)


#Data----
Vr_data <- as.data.frame(read_xlsx("Data S1 phenotyping species diversity.xlsx"))
View(Vr_data)
dim(Vr_data)
Vr_data$Species_replicate <- paste(Vr_data$Species,Vr_data$Replicate)
Vr_data$Species_phylo <- paste(Vr_data$Species)



Vr_Caenorhabditis_Oscheius_data$P3.p_total <- (Vr_Caenorhabditis_Oscheius_data$P3.p_S + Vr_Caenorhabditis_Oscheius_data$P3.p_SS + Vr_Caenorhabditis_Oscheius_data$P3.p_SSS + Vr_Caenorhabditis_Oscheius_data$P3.p_SSSS + Vr_Caenorhabditis_Oscheius_data$P3.p_Other)
Vr_Caenorhabditis_Oscheius_data$P4.p_total <- (Vr_Caenorhabditis_Oscheius_data$P4.p_S + Vr_Caenorhabditis_Oscheius_data$P4.p_SS + Vr_Caenorhabditis_Oscheius_data$P4.p_SSS + Vr_Caenorhabditis_Oscheius_data$P4.p_SSSS + Vr_Caenorhabditis_Oscheius_data$P4.p_Other)
Vr_Caenorhabditis_Oscheius_data$P8.p_total <- (Vr_Caenorhabditis_Oscheius_data$P8.p_S + Vr_Caenorhabditis_Oscheius_data$P8.p_SS + Vr_Caenorhabditis_Oscheius_data$P8.p_SSS + Vr_Caenorhabditis_Oscheius_data$P8.p_SSSS + Vr_Caenorhabditis_Oscheius_data$P8.p_Other)
Vr_Caenorhabditis_Oscheius_data$P5.p_total <- (Vr_Caenorhabditis_Oscheius_data$P5.p_wt+ Vr_Caenorhabditis_Oscheius_data$P5.p_Other) 
Vr_Caenorhabditis_Oscheius_data$P6.p_total <- (Vr_Caenorhabditis_Oscheius_data$P6.p_wt+ Vr_Caenorhabditis_Oscheius_data$P6.p_Other) 
Vr_Caenorhabditis_Oscheius_data$P7.p_total <- (Vr_Caenorhabditis_Oscheius_data$P7.p_wt+ Vr_Caenorhabditis_Oscheius_data$P7.p_Other) 
View(Vr_Caenorhabditis_Oscheius_data)

table(Vr_Caenorhabditis_Oscheius_data$P3.p_total )
table(Vr_Caenorhabditis_Oscheius_data$P4.p_total )
table(Vr_Caenorhabditis_Oscheius_data$P8.p_total )
table(Vr_Caenorhabditis_Oscheius_data$P5.p_total )
table(Vr_Caenorhabditis_Oscheius_data$P6.p_total )
table(Vr_Caenorhabditis_Oscheius_data$P7.p_total )
View(Vr_Caenorhabditis_Oscheius_data)



#Sum and aggregate data per line
{
  
  sum_Vr_Caenorhabditis_Oscheius_data <- aggregate(cbind(Vr_Caenorhabditis_Oscheius_data$P3.p_S,Vr_Caenorhabditis_Oscheius_data$P3.p_SS,Vr_Caenorhabditis_Oscheius_data$P3.p_SSS,Vr_Caenorhabditis_Oscheius_data$P3.p_SSSS,Vr_Caenorhabditis_Oscheius_data$P3.p_Other,Vr_Caenorhabditis_Oscheius_data$P3.p_total,Vr_Caenorhabditis_Oscheius_data$P4.p_S,Vr_Caenorhabditis_Oscheius_data$P4.p_SS,Vr_Caenorhabditis_Oscheius_data$P4.p_SSS,Vr_Caenorhabditis_Oscheius_data$P4.p_SSSS,Vr_Caenorhabditis_Oscheius_data$P4.p_Other,Vr_Caenorhabditis_Oscheius_data$P4.p_total,Vr_Caenorhabditis_Oscheius_data$P5.p_wt, Vr_Caenorhabditis_Oscheius_data$P5.p_Other,Vr_Caenorhabditis_Oscheius_data$P5.p_total,Vr_Caenorhabditis_Oscheius_data$P6.p_wt, Vr_Caenorhabditis_Oscheius_data$P6.p_Other,Vr_Caenorhabditis_Oscheius_data$P6.p_total, Vr_Caenorhabditis_Oscheius_data$P7.p_wt, Vr_Caenorhabditis_Oscheius_data$P7.p_Other,Vr_Caenorhabditis_Oscheius_data$P7.p_total, Vr_Caenorhabditis_Oscheius_data$P8.p_S,Vr_Caenorhabditis_Oscheius_data$P8.p_SS,Vr_Caenorhabditis_Oscheius_data$P8.p_SSS,Vr_Caenorhabditis_Oscheius_data$P8.p_SSSS,Vr_Caenorhabditis_Oscheius_data$P8.p_Other,Vr_Caenorhabditis_Oscheius_data$P8.p_total), by=list(Line=Vr_Caenorhabditis_Oscheius_data$Line), FUN=sum)
  View(sum_Vr_Caenorhabditis_Oscheius_data)
  colnames(sum_Vr_Caenorhabditis_Oscheius_data) <- c("Line","P3.p_S_sum","P3.p_SS_sum","P3.p_SSS_sum","P3.p_SSSS_sum","P3.p_Other_sum","P3.p_total_sum","P4.p_S_sum", "P4.p_SS_sum","P4.p_SSS_sum","P4.p_SSSS_sum", "P4.p_Other_sum","P4.p_total_sum","P5.p_wt_sum","P5.p_Other_sum","P5.p_total_sum","P6.p_wt_sum","P6.p_Other_sum","P6.p_total_sum","P7.p_wt_sum","P7.p_Other_sum","P7.p_total_sum", "P8.p_S_sum", "P8.p_SS_sum","P8.p_SSS_sum","P8.p_SSSS_sum", "P8.p_Other_sum","P8.p_total_sum")
  names(Vr_Caenorhabditis_Oscheius_data)
  
  Vr_Caenorhabditis_Oscheius_data_sum <- merge(unique(Vr_Caenorhabditis_Oscheius_data[,c("Variance","Genus","Species","Treatment","Line" )]), sum_Vr_Caenorhabditis_Oscheius_data)         
  View(Vr_Caenorhabditis_Oscheius_data_sum)
  table(Vr_Caenorhabditis_Oscheius_data_sum$Line)
  
}  

#frequency
{
  Vr_Caenorhabditis_Oscheius_data_sum$P3.p_S_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P3.p_S_sum/Vr_Caenorhabditis_Oscheius_data_sum$P3.p_total_sum))  
  Vr_Caenorhabditis_Oscheius_data_sum$P3.p_SS_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P3.p_SS_sum/Vr_Caenorhabditis_Oscheius_data_sum$P3.p_total_sum))  
  Vr_Caenorhabditis_Oscheius_data_sum$P3.p_SSS_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P3.p_SSS_sum/Vr_Caenorhabditis_Oscheius_data_sum$P3.p_total_sum))  
  Vr_Caenorhabditis_Oscheius_data_sum$P3.p_SSSS_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P3.p_SSSS_sum/Vr_Caenorhabditis_Oscheius_data_sum$P3.p_total_sum))  
  Vr_Caenorhabditis_Oscheius_data_sum$P3.p_Other_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P3.p_Other_sum/Vr_Caenorhabditis_Oscheius_data_sum$P3.p_total_sum))  
  
  Vr_Caenorhabditis_Oscheius_data_sum$P4.p_S_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P4.p_S_sum/Vr_Caenorhabditis_Oscheius_data_sum$P4.p_total_sum))  
  Vr_Caenorhabditis_Oscheius_data_sum$P4.p_SS_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P4.p_SS_sum/Vr_Caenorhabditis_Oscheius_data_sum$P4.p_total_sum))  
  Vr_Caenorhabditis_Oscheius_data_sum$P4.p_SSS_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P4.p_SSS_sum/Vr_Caenorhabditis_Oscheius_data_sum$P4.p_total_sum))  
  Vr_Caenorhabditis_Oscheius_data_sum$P4.p_SSSS_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P4.p_SSSS_sum/Vr_Caenorhabditis_Oscheius_data_sum$P4.p_total_sum))  
  Vr_Caenorhabditis_Oscheius_data_sum$P4.p_Other_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P4.p_Other_sum/Vr_Caenorhabditis_Oscheius_data_sum$P4.p_total_sum))  
  
  Vr_Caenorhabditis_Oscheius_data_sum$P5.p_wt_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P5.p_wt_sum/Vr_Caenorhabditis_Oscheius_data_sum$P5.p_total_sum))  
  Vr_Caenorhabditis_Oscheius_data_sum$P5.p_Other_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P5.p_Other_sum/Vr_Caenorhabditis_Oscheius_data_sum$P5.p_total_sum))
  
  Vr_Caenorhabditis_Oscheius_data_sum$P6.p_wt_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P6.p_wt_sum/Vr_Caenorhabditis_Oscheius_data_sum$P6.p_total_sum))  
  Vr_Caenorhabditis_Oscheius_data_sum$P6.p_Other_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P6.p_Other_sum/Vr_Caenorhabditis_Oscheius_data_sum$P6.p_total_sum))
  
  Vr_Caenorhabditis_Oscheius_data_sum$P7.p_wt_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P7.p_wt_sum/Vr_Caenorhabditis_Oscheius_data_sum$P7.p_total_sum))  
  Vr_Caenorhabditis_Oscheius_data_sum$P7.p_Other_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P7.p_Other_sum/Vr_Caenorhabditis_Oscheius_data_sum$P7.p_total_sum))
  
  Vr_Caenorhabditis_Oscheius_data_sum$P8.p_S_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P8.p_S_sum/Vr_Caenorhabditis_Oscheius_data_sum$P8.p_total_sum))  
  Vr_Caenorhabditis_Oscheius_data_sum$P8.p_SS_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P8.p_SS_sum/Vr_Caenorhabditis_Oscheius_data_sum$P8.p_total_sum))  
  Vr_Caenorhabditis_Oscheius_data_sum$P8.p_SSS_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P8.p_SSS_sum/Vr_Caenorhabditis_Oscheius_data_sum$P8.p_total_sum))  
  Vr_Caenorhabditis_Oscheius_data_sum$P8.p_SSSS_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P8.p_SSSS_sum/Vr_Caenorhabditis_Oscheius_data_sum$P8.p_total_sum))  
  Vr_Caenorhabditis_Oscheius_data_sum$P8.p_Other_freq <-  c((Vr_Caenorhabditis_Oscheius_data_sum$P8.p_Other_sum/Vr_Caenorhabditis_Oscheius_data_sum$P8.p_total_sum))  
  View(Vr_Caenorhabditis_Oscheius_data_sum)
}



table(Vr_Caenorhabditis_Oscheius_data_sum$Species)
write_xlsx(Vr_Caenorhabditis_Oscheius_data_sum, "Vr_Caenorhabditis_Oscheius_data_sum-and-frequencies.xlsx")

#Vr_Caenorhabditis_Oscheius_data_frequencies_SS_SSSS.xlsx : manually curation of P3.p, P4.p and P8.p to keep the frequencies of 'SS' fate in Caenorhabditis and 'SSSS' in Oscheius

Vr_Caenorhabditis_Oscheius_data_frequencies_SS_SSSS <- as.data.frame(read_xlsx("Vr_Caenorhabditis_Oscheius_data_frequencies_SS_SSSS.xlsx"))






#PLots----

---- #Raw_Frequencies_Vr_Caenorhabditis_Oscheius----
s
factor(Vr_Caenorhabditis_Oscheius_data_frequencies_SS_SSSS$Genus)
names(Vr_Caenorhabditis_Oscheius_data_frequencies_SS_SSSS)

Plot_Raw_Frequencies_Vr_Caenorhabditis_SS_Oscheius_SSSS_P3pP4p <- ggplot(Vr_Caenorhabditis_Oscheius_data_frequencies_SS_SSSS, aes(x = P3.p_freq, y = P4.p_freq)) + 
  geom_point(aes(colour = Genus), shape=19, alpha=0.9)+  scale_colour_manual(values = c("salmon","royalblue"))+  
  xlim(0, 1) + ylim(0,1)+
  theme_bw() + labs( x = "Frequency of P3.p division", y = "Frequency of P4.p division") + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 15)),axis.title.x = element_text(margin = margin(t = 15)), legend.position = "bottom", text = element_text(size = 15))


pdf("Plot_Raw_Frequencies_Vr_Caenorhabditis_SS_Oscheius_SSSS_P3pP4p_boxplot.pdf")
ggMarginal(Plot_Raw_Frequencies_Vr_Caenorhabditis_SS_Oscheius_SSSS_P3pP4p, type = "boxplot",  groupColour = TRUE, groupFill = TRUE, alpha = 0.2, size=10)
dev.off() 




#Raw_Frequencies_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_all_Pnp  ----                                                  
Raw_Frequencies_Vr_Caenorhabditis_SS_Oscheius_SSSS_v2 <- as.data.frame(read_xlsx("2024.10.08-Vr_Caenorhabditis_Oscheius_data_frequencies_SS_SSSS_v2.xlsx"))

head(Raw_Frequencies_Vr_Caenorhabditis_SS_Oscheius_SSSS_v2)
View(Raw_Frequencies_Vr_Caenorhabditis_SS_Oscheius_SSSS_v2)


Plot_Raw_Frequencies_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_all_Pnp <- ggplot(Raw_Frequencies_Vr_Caenorhabditis_SS_Oscheius_SSSS_v2, aes( x= Pn.p ,y= Trait_Frequency)) +
  geom_point(aes(colour = Genus), size=0.7) + scale_colour_manual(values = c("salmon","royalblue"))+  
  facet_grid(rows =vars(Genus))+ theme_bw() +
  labs(title = "Raw_Frequencies_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_all_Pnp", y ="Trait frequency") + scale_x_discrete(labels=c("3","4","5","6","7","8")) + theme(aspect.ratio=1, axis.title.y = element_text(margin = margin(r = 20)),axis.title.x = element_text(margin = margin(t = 20)), legend.position = "bottom",axis.text.x = element_text(size = 10),axis.title = element_text(size = 15))


pdf("Plot_Raw_Frequencies_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_all_Pnp.pdf")
Plot_Raw_Frequencies_Vr_Caenorhabditis_SS_Oscheius_SSSS_dataScale_all_Pnp
dev.off()



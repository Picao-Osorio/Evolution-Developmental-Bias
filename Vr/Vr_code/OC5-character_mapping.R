library(phytools)
library(readxl)
library(Matrix)
library(ggplot2)
library(ape)
library(ggdark)
library(writexl)
library(tidyr)


# frequency data from Plots_Raw_Frequencies_Vr_Caenorhabditis_Oscheius
View(Vr_Caenorhabditis_Oscheius_data_reduced_sum)
names(Vr_Caenorhabditis_Oscheius_data_reduced_sum)

##reduce to only frequency
Vr_Caenorhabditis_Oscheius_data_reduced_freq <- Vr_Caenorhabditis_Oscheius_data_reduced_sum[,-c(6:32)]
View(Vr_Caenorhabditis_Oscheius_data_reduced_freq)
names(Vr_Caenorhabditis_Oscheius_data_reduced_freq)



# Caenorhabditis_spp----

Vr_Caenorhabditis_data_reduced_freq <- subset(Vr_Caenorhabditis_Oscheius_data_reduced_freq, Genus =="Caenorhabditis")
View(Vr_Caenorhabditis_data_reduced_freq)

#Caenorhabditis_phylogeny: 
plot(Caenorhabditis_chronogram_ultra)

#Caenorhabditis_spp_P3p_S_freq_phylo
{
  Caenorhabditis_spp_P3.p_S_freq <- as.vector(Vr_Caenorhabditis_data_reduced_freq$P3.p_S_freq)
  names(Caenorhabditis_spp_P3.p_S_freq) <- Vr_Caenorhabditis_data_reduced_freq$Species
  Caenorhabditis_spp_P3p_S_freq_phylo <- contMap(Caenorhabditis_chronogram_ultra,Caenorhabditis_spp_P3.p_S_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
  plot(Caenorhabditis_spp_P3p_S_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
  
  pdf("Caenorhabditis_spp_P3p_S_freq_phylo.pdf")
  plot(Caenorhabditis_spp_P3p_S_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
  dev.off() 
}

#Caenorhabditis_spp_P3p_SS_freq_phylo
{
  Caenorhabditis_spp_P3.p_SS_freq <- as.vector(Vr_Caenorhabditis_data_reduced_freq$P3.p_SS_freq)
  names(Caenorhabditis_spp_P3.p_SS_freq) <- Vr_Caenorhabditis_data_reduced_freq$Species
  Caenorhabditis_spp_P3p_SS_freq_phylo <- contMap(Caenorhabditis_chronogram_ultra,Caenorhabditis_spp_P3.p_SS_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
  plot(Caenorhabditis_spp_P3p_SS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
  
  pdf("Caenorhabditis_spp_P3p_SS_freq_phylo.pdf")
  plot(Caenorhabditis_spp_P3p_SS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
  dev.off() 
}

#Caenorhabditis_spp_P4p_SS_freq_phylo
{
  Caenorhabditis_spp_P4.p_SS_freq <- as.vector(Vr_Caenorhabditis_data_reduced_freq$P4.p_SS_freq)
  names(Caenorhabditis_spp_P4.p_SS_freq) <- Vr_Caenorhabditis_data_reduced_freq$Species
  Caenorhabditis_spp_P4p_SS_freq_phylo <- contMap(Caenorhabditis_chronogram_ultra,Caenorhabditis_spp_P4.p_SS_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
  plot(Caenorhabditis_spp_P4p_SS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
  
  pdf("Caenorhabditis_spp_P4p_SS_freq_phylo.pdf")
  plot(Caenorhabditis_spp_P4p_SS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
  dev.off() 
}

#Caenorhabditis_spp_P8p_SS_freq_phylo
{
  Caenorhabditis_spp_P8.p_SS_freq <- as.vector(Vr_Caenorhabditis_data_reduced_freq$P8.p_SS_freq)
  names(Caenorhabditis_spp_P8.p_SS_freq) <- Vr_Caenorhabditis_data_reduced_freq$Species
  Caenorhabditis_spp_P8p_SS_freq_phylo <- contMap(Caenorhabditis_chronogram_ultra,Caenorhabditis_spp_P8.p_SS_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
  plot(Caenorhabditis_spp_P8p_SS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
  
  pdf("Caenorhabditis_spp_P8p_SS_freq_phylo.pdf")
  plot(Caenorhabditis_spp_P8p_SS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
  dev.off() 
}

#Caenorhabditis_spp_P3_4_8.p_SS_freq:“phylogenetic scatterplot matrix” continuous character color mappings of each trait on the diagonal, and bivariate phylomorphospaces in off-diagonal positions
{
  Caenorhabditis_spp_P3_4_8.p_SS_freq <- cbind(Vr_Caenorhabditis_data_reduced_freq$P3.p_SS_freq, Vr_Caenorhabditis_data_reduced_freq$P4.p_SS_freq,Vr_Caenorhabditis_data_reduced_freq$P8.p_SS_freq)
  rownames(Caenorhabditis_spp_P3_4_8.p_SS_freq) <- Vr_Caenorhabditis_data_reduced_freq$Species
  Caenorhabditis_spp_P3_4_8.p_SS_freq
  Caenorhabditis_spp_P3_4_8.p_SS_freq_fancyTree <- fancyTree(Caenorhabditis_chronogram_ultra,type="scattergram",X=Caenorhabditis_spp_P3_4_8.p_SS_freq)
  plot(Caenorhabditis_spp_P3_4_8.p_SS_freq_fancyTree, fsize=c(0.2),legend=10,label=FALSE)
  
  
  
  
  pdf("Caenorhabditis_spp_P3_4_8.p_SS_freq_fancyTree.pdf")
  plot(Caenorhabditis_spp_P3_4_8.p_SS_freq_fancyTree, fsize=c(0.2),label=FALSE)
  dev.off() 
  
}


#Caenorhabditis_spp_P3_4_traitgram
{
  Caenorhabditis_spp_P3_4.p_SS_freq <- cbind(Vr_Caenorhabditis_data_reduced_freq$P3.p_SS_freq, Vr_Caenorhabditis_data_reduced_freq$P4.p_SS_freq)
  rownames(Caenorhabditis_spp_P3_4.p_SS_freq) <- Vr_Caenorhabditis_data_reduced_freq$Species
  fancyTree(Caenorhabditis_chronogram_ultra,type="traitgram3d",X=Caenorhabditis_spp_P3_4.p_SS_freq,method="static",angle=-45,maxit=2000000000,label=FALSE)
  
  pdf("Caenorhabditis_spp_P3_4_traitgram.pdf")
  fancyTree(Caenorhabditis_chronogram_ultra,type="traitgram3d",X=Caenorhabditis_spp_P3_4.p_SS_freq,method="static",angle=-45,maxit=2000000000)
  dev.off() 
  par(mfrow = c(1,1))
}

#Caenorhabditis_spp_phenograms
{
  pdf("Caenorhabditis_spp_P3p_SS_phenogram.pdf")
  phenogram(Caenorhabditis_chronogram_ultra,Caenorhabditis_spp_P3_4_8.p_SS_freq[,1],spread.labels=TRUE, ftype="i",fsize=0.4)
  dev.off()
  
  pdf("Caenorhabditis_spp_P4p_SS_phenogram.pdf")
  phenogram(Caenorhabditis_chronogram_ultra,Caenorhabditis_spp_P3_4_8.p_SS_freq[,2],spread.labels=TRUE, ftype="i",fsize=0.4, ylim=c(0,1))
  dev.off()
  
  pdf("Caenorhabditis_spp_P8p_SS_phenogram.pdf")
  phenogram(Caenorhabditis_chronogram_ultra,Caenorhabditis_spp_P3_4_8.p_SS_freq[,3],spread.labels=TRUE, ftype="i",fsize=0.4, ylim=c(0,1))
  dev.off()
}


----#Oscheius_spp----
{
  
  Vr_Oscheius_data_reduced_freq <- subset(Vr_Caenorhabditis_Oscheius_data_reduced_freq, Genus =="Oscheius")
  View(Vr_Oscheius_data_reduced_freq)
  
  #Oscheius_phylogeny: 
  plot(Oscheius_chronogram_ultra)
  
  
  Oscheius_chronogram_ultra #Oscheius_phylogeny
  #Character Trait evolution along the phylogeny - Oscheius
  {
    #Oscheius_spp_P3p_S_freq_phylo
    {
      
      
      Oscheius_spp_P3.p_S_freq <- as.vector(Vr_Oscheius_data_reduced_freq$P3.p_S_freq)
      names(Oscheius_spp_P3.p_S_freq) <- Vr_Oscheius_data_reduced_freq$Species
      Oscheius_spp_P3p_S_freq_phylo <- contMap(Oscheius_chronogram_ultra,Oscheius_spp_P3.p_S_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
      plot(Oscheius_spp_P3p_S_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
      
      pdf("Oscheius_spp_P3p_S_freq_phylo.pdf")
      plot(Oscheius_spp_P3p_S_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
      dev.off() 
    }
    
    #Oscheius_spp_P3p_SSSS_freq_phylo
    {
      Oscheius_spp_P3.p_SSSS_freq <- as.vector(Vr_Oscheius_data_reduced_freq$P3.p_SSSS_freq)
      names(Oscheius_spp_P3.p_SSSS_freq) <- Vr_Oscheius_data_reduced_freq$Species
      Oscheius_spp_P3p_SSSS_freq_phylo <- contMap(Oscheius_chronogram_ultra,Oscheius_spp_P3.p_SSSS_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
      plot(Oscheius_spp_P3p_SSSS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
      
      pdf("Oscheius_spp_P3p_SSSS_freq_phylo.pdf")
      plot(Oscheius_spp_P3p_SSSS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
      dev.off() 
    }
    
    #Oscheius_spp_P4p_S_freq_phylo
    {
      Oscheius_spp_P4.p_S_freq <- as.vector(Vr_Oscheius_data_reduced_freq$P4.p_S_freq)
      names(Oscheius_spp_P4.p_S_freq) <- Vr_Oscheius_data_reduced_freq$Species
      Oscheius_spp_P4p_S_freq_phylo <- contMap(Oscheius_chronogram_ultra,Oscheius_spp_P4.p_S_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
      plot(Oscheius_spp_P4p_S_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
      
      pdf("Oscheius_spp_P4p_S_freq_phylo.pdf")
      plot(Oscheius_spp_P4p_S_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
      dev.off() 
    }
    #Oscheius_spp_P4p_SS_freq_phylo
    {
      Oscheius_spp_P4.p_SS_freq <- as.vector(Vr_Oscheius_data_reduced_freq$P4.p_SS_freq)
      names(Oscheius_spp_P4.p_SS_freq) <- Vr_Oscheius_data_reduced_freq$Species
      Oscheius_spp_P4p_SS_freq_phylo <- contMap(Oscheius_chronogram_ultra,Oscheius_spp_P4.p_SS_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
      plot(Oscheius_spp_P4p_SS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
      
      pdf("Oscheius_spp_P4p_SS_freq_phylo.pdf")
      plot(Oscheius_spp_P4p_SS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
      dev.off() 
    }
    
    #Oscheius_spp_P4p_SSSS_freq_phylo
    {
      Oscheius_spp_P4.p_SSSS_freq <- as.vector(Vr_Oscheius_data_reduced_freq$P4.p_SSSS_freq)
      names(Oscheius_spp_P4.p_SSSS_freq) <- Vr_Oscheius_data_reduced_freq$Species
      Oscheius_spp_P4p_SSSS_freq_phylo <- contMap(Oscheius_chronogram_ultra,Oscheius_spp_P4.p_SSSS_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
      plot(Oscheius_spp_P4p_SSSS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
      
      pdf("Oscheius_spp_P4p_SSSS_freq_phylo.pdf")
      plot(Oscheius_spp_P4p_SSSS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
      dev.off() 
    }
    
    #Oscheius_spp_P8p_S_freq_phylo
    {
      Oscheius_spp_P8.p_S_freq <- as.vector(Vr_Oscheius_data_reduced_freq$P8.p_S_freq)
      names(Oscheius_spp_P8.p_S_freq) <- Vr_Oscheius_data_reduced_freq$Species
      Oscheius_spp_P8p_S_freq_phylo <- contMap(Oscheius_chronogram_ultra,Oscheius_spp_P8.p_S_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
      plot(Oscheius_spp_P8p_S_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
      
      pdf("Oscheius_spp_P8p_S_freq_phylo.pdf")
      plot(Oscheius_spp_P8p_S_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
      dev.off() 
    }
    
    #Oscheius_spp_P8p_SS_freq_phylo
    {
      Oscheius_spp_P8.p_SS_freq <- as.vector(Vr_Oscheius_data_reduced_freq$P8.p_SS_freq)
      names(Oscheius_spp_P8.p_SS_freq) <- Vr_Oscheius_data_reduced_freq$Species
      Oscheius_spp_P8p_SS_freq_phylo <- contMap(Oscheius_chronogram_ultra,Oscheius_spp_P8.p_SS_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
      plot(Oscheius_spp_P8p_SS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
      
      pdf("Oscheius_spp_P8p_SS_freq_phylo.pdf")
      plot(Oscheius_spp_P8p_SS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
      dev.off() 
    }
    
    #Oscheius_spp_P8p_SSSS_freq_phylo
    {
      Oscheius_spp_P8.p_SSSS_freq <- as.vector(Vr_Oscheius_data_reduced_freq$P8.p_SSSS_freq)
      names(Oscheius_spp_P8.p_SSSS_freq) <- Vr_Oscheius_data_reduced_freq$Species
      Oscheius_spp_P8p_SSSS_freq_phylo <- contMap(Oscheius_chronogram_ultra,Oscheius_spp_P8.p_SSSS_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
      plot(Oscheius_spp_P8p_SSSS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
      
      pdf("Oscheius_spp_P8p_SSSS_freq_phylo.pdf")
      plot(Oscheius_spp_P8p_SSSS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
      dev.off() 
    }
  }
  
  #Oscheius_spp_P3_4_8.p_SSSS_freq:“phylogenetic scatterplot matrix” continuous character color mappings of each trait on the diagonal, and bivariate phylomorphospaces in off-diagonal positions
  {
    Oscheius_spp_P3_4_8.p_SSSS_freq <- cbind(Vr_Oscheius_data_reduced_freq$P3.p_SSSS_freq, Vr_Oscheius_data_reduced_freq$P4.p_SSSS_freq,Vr_Oscheius_data_reduced_freq$P8.p_SSSS_freq)
    rownames(Oscheius_spp_P3_4_8.p_SSSS_freq) <- Vr_Oscheius_data_reduced_freq$Species
    Oscheius_spp_P3_4_8.p_SSSS_freq
    Oscheius_spp_P3_4_8.p_SSSS_freq_fancyTree <- fancyTree(Oscheius_chronogram_ultra,type="scattergram",X=Oscheius_spp_P3_4_8.p_SSSS_freq)
    plot(Oscheius_spp_P3_4_8.p_SSSS_freq_fancyTree, fsize=c(0.2),legend=10,label=FALSE,xlim=c(0,1), ylim=c(0,1))
    
    
    
    
    pdf("Oscheius_spp_P3_4_8.p_SSSS_freq_fancyTree.pdf")
    plot(Oscheius_spp_P3_4_8.p_SSSS_freq_fancyTree, fsize=c(0.4),label=FALSE,xlim=c(0,1), ylim=c(0,1))
    dev.off() 
    
    
  }
  
  #Oscheius_spp_SSSS_phylomorphospaces
  {
    Oscheius_spp_P3_4.p_SSSS_freq <- cbind(Vr_Oscheius_data_reduced_freq$P3.p_SSSS_freq, Vr_Oscheius_data_reduced_freq$P4.p_SSSS_freq)
    rownames(Oscheius_spp_P3_4.p_SSSS_freq) <- Vr_Oscheius_data_reduced_freq$Species
    pdf("Oscheius_spp_P3_4.p_SSSS_phylomorphospace.pdf")
    phylomorphospace(Oscheius_chronogram_ultra,X=Oscheius_spp_P3_4.p_SSSS_freq,xlim=c(0,1), ylim=c(0,1), label=FALSE)
    dev.off() 
    
    Oscheius_spp_P3_8.p_SSSS_freq <- cbind(Vr_Oscheius_data_reduced_freq$P3.p_SSSS_freq, Vr_Oscheius_data_reduced_freq$P8.p_SSSS_freq)
    rownames(Oscheius_spp_P3_8.p_SSSS_freq) <- Vr_Oscheius_data_reduced_freq$Species
    pdf("Oscheius_spp_P3_8.p_SSSS_phylomorphospace.pdf")
    phylomorphospace(Oscheius_chronogram_ultra,X=Oscheius_spp_P3_8.p_SSSS_freq,xlim=c(0,1), ylim=c(0,1), label=FALSE)
    dev.off() 
    
    Oscheius_spp_P4_8.p_SSSS_freq <- cbind(Vr_Oscheius_data_reduced_freq$P4.p_SSSS_freq, Vr_Oscheius_data_reduced_freq$P8.p_SSSS_freq)
    rownames(Oscheius_spp_P4_8.p_SSSS_freq) <- Vr_Oscheius_data_reduced_freq$Species
    pdf("Oscheius_spp_P4_8.p_SSSS_phylomorphospace.pdf")
    phylomorphospace(Oscheius_chronogram_ultra,X=Oscheius_spp_P4_8.p_SSSS_freq,xlim=c(0,1), ylim=c(0,1), label=FALSE)
    dev.off() 
    
  }
  
  #Oscheius_spp_P3_4_traitgram
  {
    Oscheius_spp_P3_4.p_SSSS_freq <- cbind(Vr_Oscheius_data_reduced_freq$P3.p_SSSS_freq, Vr_Oscheius_data_reduced_freq$P4.p_SSSS_freq)
    rownames(Oscheius_spp_P3_4.p_SSSS_freq) <- Vr_Oscheius_data_reduced_freq$Species
    fancyTree(Oscheius_chronogram_ultra,type="traitgram3d",X=Oscheius_spp_P3_4.p_SSSS_freq,method="static",angle=-45,maxit=2000000000,label=FALSE,xlim=c(0,1), ylim=c(0,1))
    
    pdf("Oscheius_spp_P3_4_traitgram.pdf")
    fancyTree(Oscheius_chronogram_ultra,type="traitgram3d",X=Oscheius_spp_P3_4.p_SSSS_freq,method="static",angle=-45,maxit=2000000000)
    dev.off() 
    
  }
  
  #Oscheius_spp_phenograms
  {
    pdf("Oscheius_spp_P3p_SSSS_phenogram.pdf")
    phenogram(Oscheius_chronogram_ultra,Oscheius_spp_P3_4_8.p_SSSS_freq[,1],spread.labels=TRUE, ftype="i",fsize=0.4, ylim=c(0,1))
    dev.off()
    
    pdf("Oscheius_spp_P4p_SSSS_phenogram.pdf")
    phenogram(Oscheius_chronogram_ultra,Oscheius_spp_P3_4_8.p_SSSS_freq[,2],spread.labels=TRUE, ftype="i",fsize=0.4)
    dev.off()
    
    pdf("Oscheius_spp_P8p_SSSS_phenogram.pdf")
    phenogram(Oscheius_chronogram_ultra,Oscheius_spp_P3_4_8.p_SSSS_freq[,3],spread.labels=TRUE, ftype="i",fsize=0.4)
    dev.off()
  }
  
}




#----Caenorhabditis_Oscheius_spp----

#reshape data to have one column with Pnp_fate and another with the frequency 
##add the 'phylogenetic position' of each species
Vr_Caenorhabditis_Oscheius_data_reduced_freq$Phylo_position <- c(	"05",	"09",	"01",	"12",	"11",	"30",	"16",	"10",	"54",	"65",	"73",	"66",	"44",	"43",	"32",	"58",	"74",	"63",	"45",	"20",	"47",	"06",	"31",	"23",	"77",	"69",	"27",	"15",	"13",	"60",	"42",	"59",	"46",	"22",	"26",	"14",	"68",	"03",	"35",	"40",	"02",	"07",	"55",	"04",	"75",	"17",	"36",	"48",	"33",	"39",	"25",	"34",	"18",	"53",	"08",	"41",	"67",	"56",	"19",	"51",	"52",	"37",	"24",	"21",	"38",	"49",	"62",	"76",	"71",	"64",	"50",	"70",	"28",	"61",	"57",	"72",	"29")
View(Vr_Caenorhabditis_Oscheius_data_reduced_freq)

Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi <- Vr_Caenorhabditis_Oscheius_data_reduced_freq %>%
  gather(key = "trait", value = "trait_value", 6:26)

head(Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi)
View(Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi)

write_xlsx(Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi, "Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi.xlsx")

Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi_v2 <- Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi %>%
  separate(trait, into = c("Pn.p", "Pn.p_fate"), sep = "_")

View(Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi_v2)

write_xlsx(Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi_v2, "Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi_v2.xlsx")


Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi_v2$Phylo_Species_line <- paste(Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi_v2$Phylo_position, Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi_v2$Species,Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi_v2$Line)


Plot_Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi_v2_circle <-  ggplot(Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi_v2, aes(x="", y=trait_value, group=Pn.p_fate, colour=Pn.p_fate, fill=Pn.p_fate)) +
  geom_bar(width = .1, stat = "identity", color="black") +
  coord_polar("y", start=0) + 
  facet_grid(Phylo_Species_line ~ Pn.p) +theme_void() +
  scale_fill_manual(values=c("lightgreen", "darkgrey","yellow","brown4","orange","black"))  + theme(legend.position = "bottom")

pdf("Plot_Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi_v2_circle.pdf",  width=12, height=17)
Plot_Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi_v2_circle
dev.off() 

Plot_Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi_v2_bar <- ggplot(Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi_v2, aes(x="", y=trait_value, group=Pn.p_fate, colour=Pn.p_fate, fill=Pn.p_fate)) +
  geom_col(aes(fill = Pn.p_fate), width = 1, color="black", linewidth=0.12) +
  facet_grid(Phylo_Species_line ~ Pn.p) +theme_void() +
  scale_fill_manual(values=c("lightgreen", "darkgrey","yellow","brown4","orange","black"))  + theme(legend.position = "bottom") + coord_flip()

pdf("Plot_Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi_v2_bar.pdf",  width=12, height=17)
Plot_Vr_Caenorhabditis_Oscheius_data_reduced_freq_multi_v2_bar
dev.off() 




#Character Trait evolution along the phylogeny - Caenorhabditis_Oscheius
{
  
  Vr_Caenorhabditis_Oscheius_data_reduced_freq
  
  Caenorhabditis_Oscheius_phylo<-read.nexus("Chronogram-Caenorhabditis_Oscheius.nex")
  plot(Caenorhabditis_Oscheius_phylo)
  is.ultrametric(Caenorhabditis_Oscheius_phylo)
  
  #Caenorhabditis_Oscheius_spp_P3p_S_freq_phylo
  {
    Caenorhabditis_Oscheius_spp_P3.p_S_freq <- as.vector(Vr_Caenorhabditis_Oscheius_data_reduced_freq$P3.p_S_freq)
    names(Caenorhabditis_Oscheius_spp_P3.p_S_freq) <- Vr_Caenorhabditis_Oscheius_data_reduced_freq$Species
    Caenorhabditis_Oscheius_spp_P3p_S_freq_phylo <- contMap(Caenorhabditis_Oscheius_phylo,Caenorhabditis_Oscheius_spp_P3.p_S_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
    plot(Caenorhabditis_Oscheius_spp_P3p_S_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    
    pdf("Caenorhabditis_Oscheius_spp_P3p_S_freq_phylo.pdf")
    plot(Caenorhabditis_Oscheius_spp_P3p_S_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    dev.off() 
  }
  
  #Caenorhabditis_Oscheius_spp_P3p_SS_freq_phylo
  {
    Caenorhabditis_Oscheius_spp_P3.p_SS_freq <- as.vector(Vr_Caenorhabditis_Oscheius_data_reduced_freq$P3.p_SS_freq)
    names(Caenorhabditis_Oscheius_spp_P3.p_SS_freq) <- Vr_Caenorhabditis_Oscheius_data_reduced_freq$Species
    Caenorhabditis_Oscheius_spp_P3p_SS_freq_phylo <- contMap(Caenorhabditis_Oscheius_phylo,Caenorhabditis_Oscheius_spp_P3.p_SS_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
    plot(Caenorhabditis_Oscheius_spp_P3p_SS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    
    pdf("Caenorhabditis_Oscheius_spp_P3p_SS_freq_phylo.pdf")
    plot(Caenorhabditis_Oscheius_spp_P3p_SS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    dev.off() 
  }
  
  #Caenorhabditis_Oscheius_spp_P3p_SSSS_freq_phylo
  {
    Caenorhabditis_Oscheius_spp_P3.p_SSSS_freq <- as.vector(Vr_Caenorhabditis_Oscheius_data_reduced_freq$P3.p_SSSS_freq)
    names(Caenorhabditis_Oscheius_spp_P3.p_SSSS_freq) <- Vr_Caenorhabditis_Oscheius_data_reduced_freq$Species
    Caenorhabditis_Oscheius_spp_P3p_SSSS_freq_phylo <- contMap(Caenorhabditis_Oscheius_phylo,Caenorhabditis_Oscheius_spp_P3.p_SSSS_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
    plot(Caenorhabditis_Oscheius_spp_P3p_SSSS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    
    pdf("Caenorhabditis_Oscheius_spp_P3p_SSSS_freq_phylo.pdf")
    plot(Caenorhabditis_Oscheius_spp_P3p_SSSS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    dev.off() 
  }
  
  #Caenorhabditis_Oscheius_spp_P4p_S_freq_phylo
  {
    Caenorhabditis_Oscheius_spp_P4.p_S_freq <- as.vector(Vr_Caenorhabditis_Oscheius_data_reduced_freq$P4.p_S_freq)
    names(Caenorhabditis_Oscheius_spp_P4.p_S_freq) <- Vr_Caenorhabditis_Oscheius_data_reduced_freq$Species
    Caenorhabditis_Oscheius_spp_P4p_S_freq_phylo <- contMap(Caenorhabditis_Oscheius_phylo,Caenorhabditis_Oscheius_spp_P4.p_S_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
    plot(Caenorhabditis_Oscheius_spp_P4p_S_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    
    pdf("Caenorhabditis_Oscheius_spp_P4p_S_freq_phylo.pdf")
    plot(Caenorhabditis_Oscheius_spp_P4p_S_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    dev.off() 
  }
  
  
  #Caenorhabditis_Oscheius_spp_P4p_SS_freq_phylo
  {
    Caenorhabditis_Oscheius_spp_P4.p_SS_freq <- as.vector(Vr_Caenorhabditis_Oscheius_data_reduced_freq$P4.p_SS_freq)
    names(Caenorhabditis_Oscheius_spp_P4.p_SS_freq) <- Vr_Caenorhabditis_Oscheius_data_reduced_freq$Species
    Caenorhabditis_Oscheius_spp_P4p_SS_freq_phylo <- contMap(Caenorhabditis_Oscheius_phylo,Caenorhabditis_Oscheius_spp_P4.p_SS_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
    plot(Caenorhabditis_Oscheius_spp_P4p_SS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    
    pdf("Caenorhabditis_Oscheius_spp_P4p_SS_freq_phylo.pdf")
    plot(Caenorhabditis_Oscheius_spp_P4p_SS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    dev.off() 
  }
  
  #Caenorhabditis_Oscheius_spp_P4p_SSSS_freq_phylo
  {
    Caenorhabditis_Oscheius_spp_P4.p_SSSS_freq <- as.vector(Vr_Caenorhabditis_Oscheius_data_reduced_freq$P4.p_SSSS_freq)
    names(Caenorhabditis_Oscheius_spp_P4.p_SSSS_freq) <- Vr_Caenorhabditis_Oscheius_data_reduced_freq$Species
    Caenorhabditis_Oscheius_spp_P4p_SSSS_freq_phylo <- contMap(Caenorhabditis_Oscheius_phylo,Caenorhabditis_Oscheius_spp_P4.p_SSSS_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
    plot(Caenorhabditis_Oscheius_spp_P4p_SSSS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    
    pdf("Caenorhabditis_Oscheius_spp_P4p_SSSS_freq_phylo.pdf")
    plot(Caenorhabditis_Oscheius_spp_P4p_SSSS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    dev.off() 
  }
  
  #Caenorhabditis_SS_Oscheius_SSSS_spp_P3p_SS_freq_phylo
  {
    Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P3p <- subset(Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P3p_P4p, Pn.p=="P3.p" & Pn.p_fate2=="division")
    View(Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P3p)
    table(Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P3p$Pn.p)
    table(Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P3p$Species)
    names(Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P3p)
    
    Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P3p_vector <- as.vector(Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P3p$Frequency)
    names(Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P3p_vector) <- Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P3p$Species
    Caenorhabditis_SS_Oscheius_SSSS_spp_P3p_freq_phylo <- contMap(Caenorhabditis_Oscheius_phylo,Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P3p_vector,plot=FALSE, res=100, lims=c(min(0),max(1)))
    plot(Caenorhabditis_SS_Oscheius_SSSS_spp_P3p_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    
    pdf("Caenorhabditis_SS_Oscheius_SSSS_spp_P3p_freq_phylo.pdf")
    plot(Caenorhabditis_SS_Oscheius_SSSS_spp_P3p_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    dev.off() 
    
  }
  
  #Caenorhabditis_SS_Oscheius_SSSS_spp_P4p_SS_freq_phylo
  {
    Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P4p <- subset(Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P3p_P4p, Pn.p=="P4.p" & Pn.p_fate2=="division")
    View(Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P4p)
    table(Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P4p$Pn.p)
    table(Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P4p$Species)
    names(Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P4p)
    
    Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P4p_vector <- as.vector(Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P4p$Frequency)
    names(Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P4p_vector) <- Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P4p$Species
    Caenorhabditis_SS_Oscheius_SSSS_spp_P4p_freq_phylo <- contMap(Caenorhabditis_Oscheius_phylo,Vr_Caenorhabditis_SS_Oscheius_SSSS_data_reduced_freq_P4p_vector,plot=FALSE, res=100, lims=c(min(0),max(1)))
    plot(Caenorhabditis_SS_Oscheius_SSSS_spp_P4p_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    
    pdf("Caenorhabditis_SS_Oscheius_SSSS_spp_P4p_freq_phylo.pdf")
    plot(Caenorhabditis_SS_Oscheius_SSSS_spp_P4p_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    dev.off() 
    
  }
  
  
  
  
 

  
  
  #Caenorhabditis_Oscheius_spp_P8p_S_freq_phylo
  {
    Caenorhabditis_Oscheius_spp_P8.p_S_freq <- as.vector(Vr_Caenorhabditis_Oscheius_data_reduced_freq$P8.p_S_freq)
    names(Caenorhabditis_Oscheius_spp_P8.p_S_freq) <- Vr_Caenorhabditis_Oscheius_data_reduced_freq$Species
    Caenorhabditis_Oscheius_spp_P8p_S_freq_phylo <- contMap(Caenorhabditis_Oscheius_phylo,Caenorhabditis_Oscheius_spp_P8.p_S_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
    plot(Caenorhabditis_Oscheius_spp_P8p_S_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    
    pdf("Caenorhabditis_Oscheius_spp_P8p_S_freq_phylo.pdf")
    plot(Caenorhabditis_Oscheius_spp_P8p_S_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    dev.off() 
  }
  
  
  
  #Caenorhabditis_Oscheius_spp_P8p_SS_freq_phylo
  {
    Caenorhabditis_Oscheius_spp_P8.p_SS_freq <- as.vector(Vr_Caenorhabditis_Oscheius_data_reduced_freq$P8.p_SS_freq)
    names(Caenorhabditis_Oscheius_spp_P8.p_SS_freq) <- Vr_Caenorhabditis_Oscheius_data_reduced_freq$Species
    Caenorhabditis_Oscheius_spp_P8p_SS_freq_phylo <- contMap(Caenorhabditis_Oscheius_phylo,Caenorhabditis_Oscheius_spp_P8.p_SS_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
    plot(Caenorhabditis_Oscheius_spp_P8p_SS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    
    pdf("Caenorhabditis_Oscheius_spp_P8p_SS_freq_phylo.pdf")
    plot(Caenorhabditis_Oscheius_spp_P8p_SS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    dev.off() 
  }
  
  #Caenorhabditis_Oscheius_spp_P8p_SSSS_freq_phylo
  {
    Caenorhabditis_Oscheius_spp_P8.p_SSSS_freq <- as.vector(Vr_Caenorhabditis_Oscheius_data_reduced_freq$P8.p_SSSS_freq)
    names(Caenorhabditis_Oscheius_spp_P8.p_SSSS_freq) <- Vr_Caenorhabditis_Oscheius_data_reduced_freq$Species
    Caenorhabditis_Oscheius_spp_P8p_SSSS_freq_phylo <- contMap(Caenorhabditis_Oscheius_phylo,Caenorhabditis_Oscheius_spp_P8.p_SSSS_freq,plot=FALSE, res=100, lims=c(min(0),max(1)))
    plot(Caenorhabditis_Oscheius_spp_P8p_SSSS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    
    pdf("Caenorhabditis_Oscheius_spp_P8p_SSSS_freq_phylo.pdf")
    plot(Caenorhabditis_Oscheius_spp_P8p_SSSS_freq_phylo,type="phylogram",legend=10, fsize=c(0.7))
    dev.off() 
  }
}




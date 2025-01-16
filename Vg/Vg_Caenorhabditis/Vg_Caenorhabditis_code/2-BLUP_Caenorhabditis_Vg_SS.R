#Vg_Caenorhabditis_BLUP_Lines_SS
library(plotMCMC)
library(postMCMCglmm) 
library(tidyverse)

----#BLUP_Celegans_Vg----
{
  #Extract BLUP of Lines Celegans_Vg_P3p_SS
  {
    
    mean_random_Celegans_Vg_P3p_SS_mod <- ranef(Celegans_Vg_P3p_SS_mod, use = "mean") + mean(Celegans_Vg_P3p_SS_mod[["Sol"]][,c(1:4)])
    #View(mean_random_Celegans_Vg_P3p_SS_mod)
    mean_random_Celegans_Vg_P3p_SS_mod_dataScale <- pnorm(mean_random_Celegans_Vg_P3p_SS_mod)
    View(mean_random_Celegans_Vg_P3p_SS_mod_dataScale)
    mean_random_Celegans_Vg_P3p_SS_mod_dataScale_Line <- as.data.frame(mean_random_Celegans_Vg_P3p_SS_mod_dataScale[c(1:100),])
    colnames(mean_random_Celegans_Vg_P3p_SS_mod_dataScale_Line) <- c("P3p_SS_BLUP")
    View(mean_random_Celegans_Vg_P3p_SS_mod_dataScale_Line)
    
    
  }
  
  #Extract BLUP of Lines Celegans_Vg_P4p_SS
  {
    
    mean_random_Celegans_Vg_P4p_SS_mod <- ranef(Celegans_Vg_P4p_SS_mod, use = "mean") + mean(Celegans_Vg_P4p_SS_mod[["Sol"]][,c(1:4)])
    #View(mean_random_Celegans_Vg_P4p_SS_mod)
    mean_random_Celegans_Vg_P4p_SS_mod_dataScale <- pnorm(mean_random_Celegans_Vg_P4p_SS_mod)
    #View(mean_random_Celegans_Vg_P4p_SS_mod_dataScale)
    mean_random_Celegans_Vg_P4p_SS_mod_dataScale_Line <- as.data.frame(mean_random_Celegans_Vg_P4p_SS_mod_dataScale[c(1:100),])
    colnames(mean_random_Celegans_Vg_P4p_SS_mod_dataScale_Line) <- c("P4p_SS_BLUP")
    #View(mean_random_Celegans_Vg_P4p_SS_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines Celegans_Vg_P8p_SS
  {
    
    mean_random_Celegans_Vg_P8p_SS_mod <- ranef(Celegans_Vg_P8p_SS_mod, use = "mean") + mean(Celegans_Vg_P8p_SS_mod[["Sol"]][,c(1:4)])
    #View(mean_random_Celegans_Vg_P8p_SS_mod)
    mean_random_Celegans_Vg_P8p_SS_mod_dataScale <- pnorm(mean_random_Celegans_Vg_P8p_SS_mod)
    #View(mean_random_Celegans_Vg_P8p_SS_mod_dataScale)
    mean_random_Celegans_Vg_P8p_SS_mod_dataScale_Line <- as.data.frame(mean_random_Celegans_Vg_P8p_SS_mod_dataScale[c(1:100),])
    colnames(mean_random_Celegans_Vg_P8p_SS_mod_dataScale_Line) <- c("P8p_SS_BLUP")
    #View(mean_random_Celegans_Vg_P8p_SS_mod_dataScale_Line)
    
  }
  
  #Extract BLUP of Lines Celegans_Vg_P5p_wt
  {
    
    mean_random_Celegans_Vg_P5p_wt_mod <- ranef(Celegans_Vg_P5p_wt_mod, use = "mean") + mean(Celegans_Vg_P5p_wt_mod[["Sol"]][,c(1:4)])
    #View(mean_random_Celegans_Vg_P5p_wt_mod)
    mean_random_Celegans_Vg_P5p_wt_mod_dataScale <- pnorm(mean_random_Celegans_Vg_P5p_wt_mod)
    #View(mean_random_Celegans_Vg_P5p_wt_mod_dataScale)
    mean_random_Celegans_Vg_P5p_wt_mod_dataScale_Line <- as.data.frame(mean_random_Celegans_Vg_P5p_wt_mod_dataScale[c(1:100),])
    colnames(mean_random_Celegans_Vg_P5p_wt_mod_dataScale_Line) <- c("P5p_BLUP")
    #View(mean_random_Celegans_Vg_P5p_wt_mod_dataScale_Line)
    
    
    
  }
  
  #Extract BLUP of Lines Celegans_Vg_P6p_wt
  {
    
    mean_random_Celegans_Vg_P6p_wt_mod <- ranef(Celegans_Vg_P6p_wt_mod, use = "mean") + mean(Celegans_Vg_P6p_wt_mod[["Sol"]][,c(1:4)])
    #View(mean_random_Celegans_Vg_P6p_wt_mod)
    mean_random_Celegans_Vg_P6p_wt_mod_dataScale <- pnorm(mean_random_Celegans_Vg_P6p_wt_mod)
    #View(mean_random_Celegans_Vg_P6p_wt_mod_dataScale)
    mean_random_Celegans_Vg_P6p_wt_mod_dataScale_Line <- as.data.frame(mean_random_Celegans_Vg_P6p_wt_mod_dataScale[c(1:100),])
    colnames(mean_random_Celegans_Vg_P6p_wt_mod_dataScale_Line) <- c("P6p_BLUP")
    #View(mean_random_Celegans_Vg_P6p_wt_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines Celegans_Vg_P7p_wt
  {
    
    mean_random_Celegans_Vg_P7p_wt_mod <- ranef(Celegans_Vg_P7p_wt_mod, use = "mean") + mean(Celegans_Vg_P7p_wt_mod[["Sol"]][,c(1:4)])
    #View(mean_random_Celegans_Vg_P7p_wt_mod)
    mean_random_Celegans_Vg_P7p_wt_mod_dataScale <- pnorm(mean_random_Celegans_Vg_P7p_wt_mod)
    #View(mean_random_Celegans_Vg_P7p_wt_mod_dataScale)
    mean_random_Celegans_Vg_P7p_wt_mod_dataScale_Line <- as.data.frame(mean_random_Celegans_Vg_P7p_wt_mod_dataScale[c(1:100),])
    colnames(mean_random_Celegans_Vg_P7p_wt_mod_dataScale_Line) <- c("P7p_BLUP")
    #View(mean_random_Celegans_Vg_P7p_wt_mod_dataScale_Line)
    
  }
  
  BLUP_Celegans_Vg_mod_dataScale_SS <- cbind.data.frame(mean_random_Celegans_Vg_P3p_SS_mod_dataScale_Line,mean_random_Celegans_Vg_P4p_SS_mod_dataScale_Line,mean_random_Celegans_Vg_P5p_wt_mod_dataScale_Line,mean_random_Celegans_Vg_P6p_wt_mod_dataScale_Line,mean_random_Celegans_Vg_P7p_wt_mod_dataScale_Line,mean_random_Celegans_Vg_P8p_SS_mod_dataScale_Line)
  BLUP_Celegans_Vg_mod_dataScale_SS$Species <- rep("C.elegans", 100)
  #View(BLUP_Celegans_Vg_mod_dataScale_SS)
  
  
}

----#BLUP_Cbriggsae_Vg----
{
  #Extract BLUP of Lines Cbriggsae_Vg_P3p_SS
  {
    
    mean_random_Cbriggsae_Vg_P3p_SS_mod <- ranef(Cbriggsae_Vg_P3p_SS_mod, use = "mean") + mean(Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,c(1:4)])
    #View(mean_random_Cbriggsae_Vg_P3p_SS_mod)
    mean_random_Cbriggsae_Vg_P3p_SS_mod_dataScale <- pnorm(mean_random_Cbriggsae_Vg_P3p_SS_mod)
    View(mean_random_Cbriggsae_Vg_P3p_SS_mod_dataScale)
    mean_random_Cbriggsae_Vg_P3p_SS_mod_dataScale_Line <- as.data.frame(mean_random_Cbriggsae_Vg_P3p_SS_mod_dataScale[c(1:96),])
    colnames(mean_random_Cbriggsae_Vg_P3p_SS_mod_dataScale_Line) <- c("P3p_SS_BLUP")
    #View(mean_random_Cbriggsae_Vg_P3p_SS_mod_dataScale_Line)
    
    
  }
  
  #Extract BLUP of Lines Cbriggsae_Vg_P4p_SS
  {
    
    mean_random_Cbriggsae_Vg_P4p_SS_mod <- ranef(Cbriggsae_Vg_P4p_SS_mod, use = "mean") + mean(Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,c(1:4)])
    #View(mean_random_Cbriggsae_Vg_P4p_SS_mod)
    mean_random_Cbriggsae_Vg_P4p_SS_mod_dataScale <- pnorm(mean_random_Cbriggsae_Vg_P4p_SS_mod)
    #View(mean_random_Cbriggsae_Vg_P4p_SS_mod_dataScale)
    mean_random_Cbriggsae_Vg_P4p_SS_mod_dataScale_Line <- as.data.frame(mean_random_Cbriggsae_Vg_P4p_SS_mod_dataScale[c(1:96),])
    colnames(mean_random_Cbriggsae_Vg_P4p_SS_mod_dataScale_Line) <- c("P4p_SS_BLUP")
    #View(mean_random_Cbriggsae_Vg_P4p_SS_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines Cbriggsae_Vg_P8p_SS
  {
    
    mean_random_Cbriggsae_Vg_P8p_SS_mod <- ranef(Cbriggsae_Vg_P8p_SS_mod, use = "mean") + mean(Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,c(1:4)])
    #View(mean_random_Cbriggsae_Vg_P8p_SS_mod)
    mean_random_Cbriggsae_Vg_P8p_SS_mod_dataScale <- pnorm(mean_random_Cbriggsae_Vg_P8p_SS_mod)
    #View(mean_random_Cbriggsae_Vg_P8p_SS_mod_dataScale)
    mean_random_Cbriggsae_Vg_P8p_SS_mod_dataScale_Line <- as.data.frame(mean_random_Cbriggsae_Vg_P8p_SS_mod_dataScale[c(1:96),])
    colnames(mean_random_Cbriggsae_Vg_P8p_SS_mod_dataScale_Line) <- c("P8p_SS_BLUP")
    #View(mean_random_Cbriggsae_Vg_P8p_SS_mod_dataScale_Line)
    
  }
  
  #Extract BLUP of Lines Cbriggsae_Vg_P5p_wt
  {
    
    mean_random_Cbriggsae_Vg_P5p_wt_mod <- ranef(Cbriggsae_Vg_P5p_wt_mod, use = "mean") + mean(Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,c(1:4)])
    #View(mean_random_Cbriggsae_Vg_P5p_wt_mod)
    mean_random_Cbriggsae_Vg_P5p_wt_mod_dataScale <- pnorm(mean_random_Cbriggsae_Vg_P5p_wt_mod)
    #View(mean_random_Cbriggsae_Vg_P5p_wt_mod_dataScale)
    mean_random_Cbriggsae_Vg_P5p_wt_mod_dataScale_Line <- as.data.frame(mean_random_Cbriggsae_Vg_P5p_wt_mod_dataScale[c(1:96),])
    colnames(mean_random_Cbriggsae_Vg_P5p_wt_mod_dataScale_Line) <- c("P5p_BLUP")
    #View(mean_random_Cbriggsae_Vg_P5p_wt_mod_dataScale_Line)
    
    
    
  }
  
  #Extract BLUP of Lines Cbriggsae_Vg_P6p_wt
  {
    
    mean_random_Cbriggsae_Vg_P6p_wt_mod <- ranef(Cbriggsae_Vg_P6p_wt_mod, use = "mean") + mean(Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,c(1:4)])
    #View(mean_random_Cbriggsae_Vg_P6p_wt_mod)
    mean_random_Cbriggsae_Vg_P6p_wt_mod_dataScale <- pnorm(mean_random_Cbriggsae_Vg_P6p_wt_mod)
    #View(mean_random_Cbriggsae_Vg_P6p_wt_mod_dataScale)
    mean_random_Cbriggsae_Vg_P6p_wt_mod_dataScale_Line <- as.data.frame(mean_random_Cbriggsae_Vg_P6p_wt_mod_dataScale[c(1:96),])
    colnames(mean_random_Cbriggsae_Vg_P6p_wt_mod_dataScale_Line) <- c("P6p_BLUP")
    #View(mean_random_Cbriggsae_Vg_P6p_wt_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines Cbriggsae_Vg_P7p_wt
  {
    
    mean_random_Cbriggsae_Vg_P7p_wt_mod <- ranef(Cbriggsae_Vg_P7p_wt_mod, use = "mean") + mean(Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,c(1:4)])
    #View(mean_random_Cbriggsae_Vg_P7p_wt_mod)
    mean_random_Cbriggsae_Vg_P7p_wt_mod_dataScale <- pnorm(mean_random_Cbriggsae_Vg_P7p_wt_mod)
    #View(mean_random_Cbriggsae_Vg_P7p_wt_mod_dataScale)
    mean_random_Cbriggsae_Vg_P7p_wt_mod_dataScale_Line <- as.data.frame(mean_random_Cbriggsae_Vg_P7p_wt_mod_dataScale[c(1:96),])
    colnames(mean_random_Cbriggsae_Vg_P7p_wt_mod_dataScale_Line) <- c("P7p_BLUP")
    #View(mean_random_Cbriggsae_Vg_P7p_wt_mod_dataScale_Line)
    
  }
  
  BLUP_Cbriggsae_Vg_mod_dataScale_SS <- cbind.data.frame(mean_random_Cbriggsae_Vg_P3p_SS_mod_dataScale_Line,mean_random_Cbriggsae_Vg_P4p_SS_mod_dataScale_Line,mean_random_Cbriggsae_Vg_P5p_wt_mod_dataScale_Line,mean_random_Cbriggsae_Vg_P6p_wt_mod_dataScale_Line,mean_random_Cbriggsae_Vg_P7p_wt_mod_dataScale_Line,mean_random_Cbriggsae_Vg_P8p_SS_mod_dataScale_Line)
  BLUP_Cbriggsae_Vg_mod_dataScale_SS$Species <- rep("C.briggsae", 96)
  View(BLUP_Cbriggsae_Vg_mod_dataScale_SS)
  
  
}

BLUP_Caenorhabditis_Vg_dataScale_SS <- rbind.data.frame(BLUP_Celegans_Vg_mod_dataScale_SS,BLUP_Cbriggsae_Vg_mod_dataScale_SS)
BLUP_Caenorhabditis_Vg_dataScale_SS <- cbind(Lines = rownames(BLUP_Caenorhabditis_Vg_dataScale_SS),BLUP_Caenorhabditis_Vg_dataScale_SS)
rownames(BLUP_Caenorhabditis_Vg_dataScale_SS) <- NULL
View(BLUP_Caenorhabditis_Vg_dataScale_SS)
BLUP_Caenorhabditis_Vg_dataScale_SS$Treatment <- c(rep("WILD", 20),rep("CONTROL", 24), rep("WILD", 25) ,rep("CONTROL", 26), rep("WILD", 5) ,rep("CONTROL",24), rep("WILD",44 ) ,rep("CONTROL", 22), rep("WILD", 6))


write_xlsx(BLUP_Caenorhabditis_Vg_dataScale_SS, "BLUP_Caenorhabditis_Vg_dataScale_SS.xlsx")



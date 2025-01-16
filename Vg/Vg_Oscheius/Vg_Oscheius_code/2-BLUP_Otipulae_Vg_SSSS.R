#BLUP_Otipulae_Vg_SSSSS_2

library(emmeans)
library(plotMCMC)
library(postMCMCglmm) 

#BLUP_Otipulae_Vg----
{
  ##Extract BLUP of Lines Otipulae_Vg_P3p_SSSS----
  {
    
    mean_random_Otipulae_Vg_P3p_SSSS_mod_2 <- ranef(Otipulae_Vg_P3p_SSSS_mod_2, use = "mean") + mean(Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:3)])
    View(mean_random_Otipulae_Vg_P3p_SSSS_mod_2)
    mean_random_Otipulae_Vg_P3p_SSSS_mod_2_dataScale <- pnorm(mean_random_Otipulae_Vg_P3p_SSSS_mod_2)
    #View(mean_random_Otipulae_Vg_P3p_SSSS_mod_2_dataScale)
    mean_random_Otipulae_Vg_P3p_SSSS_mod_2_dataScale_Line <- as.data.frame(mean_random_Otipulae_Vg_P3p_SSSS_mod_2_dataScale[c(1:83),])
    colnames(mean_random_Otipulae_Vg_P3p_SSSS_mod_2_dataScale_Line) <- c("P3p_SSSS_BLUP")
    View(mean_random_Otipulae_Vg_P3p_SSSS_mod_2_dataScale_Line)
    
    
  }
  
  ##Extract BLUP of Lines Otipulae_Vg_P4p_SSSS----
  {
    
    mean_random_Otipulae_Vg_P4p_SSSS_mod_2 <- ranef(Otipulae_Vg_P4p_SSSS_mod_2, use = "mean") + mean(Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:3)])
    #View(mean_random_Otipulae_Vg_P4p_SSSS_mod_2)
    mean_random_Otipulae_Vg_P4p_SSSS_mod_2_dataScale <- pnorm(mean_random_Otipulae_Vg_P4p_SSSS_mod_2)
    #View(mean_random_Otipulae_Vg_P4p_SSSS_mod_2_dataScale)
    mean_random_Otipulae_Vg_P4p_SSSS_mod_2_dataScale_Line <- as.data.frame(mean_random_Otipulae_Vg_P4p_SSSS_mod_2_dataScale[c(1:83),])
    colnames(mean_random_Otipulae_Vg_P4p_SSSS_mod_2_dataScale_Line) <- c("P4p_SSSS_BLUP")
    #View(mean_random_Otipulae_Vg_P4p_SSSS_mod_2_dataScale_Line)
  }
  
  
  ##Extract BLUP of Lines Otipulae_Vg_P8p_SSSS----
  {
    
    mean_random_Otipulae_Vg_P8p_SSSS_mod_2 <- ranef(Otipulae_Vg_P8p_SSSS_mod_2, use = "mean") + mean(Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:3)])
    #View(mean_random_Otipulae_Vg_P8p_SSSS_mod_2)
    mean_random_Otipulae_Vg_P8p_SSSS_mod_2_dataScale <- pnorm(mean_random_Otipulae_Vg_P8p_SSSS_mod_2)
    #View(mean_random_Otipulae_Vg_P8p_SSSS_mod_2_dataScale)
    mean_random_Otipulae_Vg_P8p_SSSS_mod_2_dataScale_Line <- as.data.frame(mean_random_Otipulae_Vg_P8p_SSSS_mod_2_dataScale[c(1:83),])
    colnames(mean_random_Otipulae_Vg_P8p_SSSS_mod_2_dataScale_Line) <- c("P8p_SSSS_BLUP")
    #View(mean_random_Otipulae_Vg_P8p_SSSS_mod_2_dataScale_Line)
    
  }
  
  ##Extract BLUP of Lines Otipulae_Vg_P5p_wt----
  {
    
    mean_random_Otipulae_Vg_P5p_wt_mod_2 <- ranef(Otipulae_Vg_P5p_wt_mod_2, use = "mean") + mean(Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,c(1:3)])
    #View(mean_random_Otipulae_Vg_P5p_wt_mod_2)
    mean_random_Otipulae_Vg_P5p_wt_mod_2_dataScale <- pnorm(mean_random_Otipulae_Vg_P5p_wt_mod_2)
    #View(mean_random_Otipulae_Vg_P5p_wt_mod_2_dataScale)
    mean_random_Otipulae_Vg_P5p_wt_mod_2_dataScale_Line <- as.data.frame(mean_random_Otipulae_Vg_P5p_wt_mod_2_dataScale[c(1:83),])
    colnames(mean_random_Otipulae_Vg_P5p_wt_mod_2_dataScale_Line) <- c("P5p_wt_BLUP")
    #View(mean_random_Otipulae_Vg_P5p_wt_mod_2_dataScale_Line)
    
    
    
  }
  
  ##Extract BLUP of Lines Otipulae_Vg_P6p_wt----
  {
    
    mean_random_Otipulae_Vg_P6p_wt_mod_2 <- ranef(Otipulae_Vg_P6p_wt_mod_2, use = "mean") + mean(Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,c(1:3)])
    #View(mean_random_Otipulae_Vg_P6p_wt_mod_2)
    mean_random_Otipulae_Vg_P6p_wt_mod_2_dataScale <- pnorm(mean_random_Otipulae_Vg_P6p_wt_mod_2)
    #View(mean_random_Otipulae_Vg_P6p_wt_mod_2_dataScale)
    mean_random_Otipulae_Vg_P6p_wt_mod_2_dataScale_Line <- as.data.frame(mean_random_Otipulae_Vg_P6p_wt_mod_2_dataScale[c(1:83),])
    colnames(mean_random_Otipulae_Vg_P6p_wt_mod_2_dataScale_Line) <- c("P6p_wt_BLUP")
    #View(mean_random_Otipulae_Vg_P6p_wt_mod_2_dataScale_Line)
  }
  
  
  ##Extract BLUP of Lines Otipulae_Vg_P7p_wt----
  {
    
    mean_random_Otipulae_Vg_P7p_wt_mod_2 <- ranef(Otipulae_Vg_P7p_wt_mod_2, use = "mean") + mean(Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,c(1:3)])
    #View(mean_random_Otipulae_Vg_P7p_wt_mod_2)
    mean_random_Otipulae_Vg_P7p_wt_mod_2_dataScale <- pnorm(mean_random_Otipulae_Vg_P7p_wt_mod_2)
    #View(mean_random_Otipulae_Vg_P7p_wt_mod_2_dataScale)
    mean_random_Otipulae_Vg_P7p_wt_mod_2_dataScale_Line <- as.data.frame(mean_random_Otipulae_Vg_P7p_wt_mod_2_dataScale[c(1:83),])
    colnames(mean_random_Otipulae_Vg_P7p_wt_mod_2_dataScale_Line) <- c("P7p_wt_BLUP")
    #View(mean_random_Otipulae_Vg_P7p_wt_mod_2_dataScale_Line)
    
  }
  
  BLUP_Otipulae_Vg_mod_2_dataScale <- cbind.data.frame(mean_random_Otipulae_Vg_P3p_SSSS_mod_2_dataScale_Line,mean_random_Otipulae_Vg_P4p_SSSS_mod_2_dataScale_Line,mean_random_Otipulae_Vg_P5p_wt_mod_2_dataScale_Line,mean_random_Otipulae_Vg_P6p_wt_mod_2_dataScale_Line,mean_random_Otipulae_Vg_P7p_wt_mod_2_dataScale_Line,mean_random_Otipulae_Vg_P8p_SSSS_mod_2_dataScale_Line)
  BLUP_Otipulae_Vg_mod_2_dataScale$Species <- rep("O.tipulae", 83)
  View(BLUP_Otipulae_Vg_mod_2_dataScale)
  
  
}

#BLUP_Oonirici_Vg----
{
  ##Extract BLUP of Lines Oonirici_Vg_P3p_SSSS----
  {
    
    mean_random_Oonirici_Vg_P3p_SSSS_mod_2 <- ranef(Oonirici_Vg_P3p_SSSS_mod_2, use = "mean") + mean(Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:3)])
    #View(mean_random_Oonirici_Vg_P3p_SSSS_mod_2)
    mean_random_Oonirici_Vg_P3p_SSSS_mod_2_dataScale <- pnorm(mean_random_Oonirici_Vg_P3p_SSSS_mod_2)
    #View(mean_random_Oonirici_Vg_P3p_SSSS_mod_2_dataScale)
    mean_random_Oonirici_Vg_P3p_SSSS_mod_2_dataScale_Line <- as.data.frame(mean_random_Oonirici_Vg_P3p_SSSS_mod_2_dataScale[c(1:32),])
    colnames(mean_random_Oonirici_Vg_P3p_SSSS_mod_2_dataScale_Line) <- c("P3p_SSSS_BLUP")
    #View(mean_random_Oonirici_Vg_P3p_SSSS_mod_2_dataScale_Line)
    
    
  }
  
  ##Extract BLUP of Lines Oonirici_Vg_P4p_SSSS----
  {
    
    mean_random_Oonirici_Vg_P4p_SSSS_mod_2 <- ranef(Oonirici_Vg_P4p_SSSS_mod_2, use = "mean") + mean(Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:3)])
    #View(mean_random_Oonirici_Vg_P4p_SSSS_mod_2)
    mean_random_Oonirici_Vg_P4p_SSSS_mod_2_dataScale <- pnorm(mean_random_Oonirici_Vg_P4p_SSSS_mod_2)
    #View(mean_random_Oonirici_Vg_P4p_SSSS_mod_2_dataScale)
    mean_random_Oonirici_Vg_P4p_SSSS_mod_2_dataScale_Line <- as.data.frame(mean_random_Oonirici_Vg_P4p_SSSS_mod_2_dataScale[c(1:32),])
    colnames(mean_random_Oonirici_Vg_P4p_SSSS_mod_2_dataScale_Line) <- c("P4p_SSSS_BLUP")
    #View(mean_random_Oonirici_Vg_P4p_SSSS_mod_2_dataScale_Line)
  }
  
  
  ##Extract BLUP of Lines Oonirici_Vg_P8p_SSSS----
  {
    
    mean_random_Oonirici_Vg_P8p_SSSS_mod_2 <- ranef(Oonirici_Vg_P8p_SSSS_mod_2, use = "mean") + mean(Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:3)])
    #View(mean_random_Oonirici_Vg_P8p_SSSS_mod_2)
    mean_random_Oonirici_Vg_P8p_SSSS_mod_2_dataScale <- pnorm(mean_random_Oonirici_Vg_P8p_SSSS_mod_2)
    #View(mean_random_Oonirici_Vg_P8p_SSSS_mod_2_dataScale)
    mean_random_Oonirici_Vg_P8p_SSSS_mod_2_dataScale_Line <- as.data.frame(mean_random_Oonirici_Vg_P8p_SSSS_mod_2_dataScale[c(1:32),])
    colnames(mean_random_Oonirici_Vg_P8p_SSSS_mod_2_dataScale_Line) <- c("P8p_SSSS_BLUP")
    #View(mean_random_Oonirici_Vg_P8p_SSSS_mod_2_dataScale_Line)
    
  }
  
  ##Extract BLUP of Lines Oonirici_Vg_P5p_wt----
  {
    
    mean_random_Oonirici_Vg_P5p_wt_mod_2 <- ranef(Oonirici_Vg_P5p_wt_mod_2, use = "mean") + mean(Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,c(1:3)])
    #View(mean_random_Oonirici_Vg_P5p_wt_mod_2)
    mean_random_Oonirici_Vg_P5p_wt_mod_2_dataScale <- pnorm(mean_random_Oonirici_Vg_P5p_wt_mod_2)
    #View(mean_random_Oonirici_Vg_P5p_wt_mod_2_dataScale)
    mean_random_Oonirici_Vg_P5p_wt_mod_2_dataScale_Line <- as.data.frame(mean_random_Oonirici_Vg_P5p_wt_mod_2_dataScale[c(1:32),])
    colnames(mean_random_Oonirici_Vg_P5p_wt_mod_2_dataScale_Line) <- c("P5p_wt_BLUP")
    #View(mean_random_Oonirici_Vg_P5p_wt_mod_2_dataScale_Line)
    
    
    
  }
  
  ##Extract BLUP of Lines Oonirici_Vg_P6p_wt----
  {
    
    mean_random_Oonirici_Vg_P6p_wt_mod_2 <- ranef(Oonirici_Vg_P6p_wt_mod_2, use = "mean") + mean(Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,c(1:3)])
    #View(mean_random_Oonirici_Vg_P6p_wt_mod_2)
    mean_random_Oonirici_Vg_P6p_wt_mod_2_dataScale <- pnorm(mean_random_Oonirici_Vg_P6p_wt_mod_2)
    #View(mean_random_Oonirici_Vg_P6p_wt_mod_2_dataScale)
    mean_random_Oonirici_Vg_P6p_wt_mod_2_dataScale_Line <- as.data.frame(mean_random_Oonirici_Vg_P6p_wt_mod_2_dataScale[c(1:32),])
    colnames(mean_random_Oonirici_Vg_P6p_wt_mod_2_dataScale_Line) <- c("P6p_wt_BLUP")
    #View(mean_random_Oonirici_Vg_P6p_wt_mod_2_dataScale_Line)
  }
  
  
  ##Extract BLUP of Lines Oonirici_Vg_P7p_wt----
  {
    
    mean_random_Oonirici_Vg_P7p_wt_mod_2 <- ranef(Oonirici_Vg_P7p_wt_mod_2, use = "mean") + mean(Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,c(1:3)])
    #View(mean_random_Oonirici_Vg_P7p_wt_mod_2)
    mean_random_Oonirici_Vg_P7p_wt_mod_2_dataScale <- pnorm(mean_random_Oonirici_Vg_P7p_wt_mod_2)
    #View(mean_random_Oonirici_Vg_P7p_wt_mod_2_dataScale)
    mean_random_Oonirici_Vg_P7p_wt_mod_2_dataScale_Line <- as.data.frame(mean_random_Oonirici_Vg_P7p_wt_mod_2_dataScale[c(1:32),])
    colnames(mean_random_Oonirici_Vg_P7p_wt_mod_2_dataScale_Line) <- c("P7p_wt_BLUP")
    #View(mean_random_Oonirici_Vg_P7p_wt_mod_2_dataScale_Line)
    
  }
  
  BLUP_Oonirici_Vg_mod_2_dataScale <- cbind.data.frame(mean_random_Oonirici_Vg_P3p_SSSS_mod_2_dataScale_Line,mean_random_Oonirici_Vg_P4p_SSSS_mod_2_dataScale_Line,mean_random_Oonirici_Vg_P5p_wt_mod_2_dataScale_Line,mean_random_Oonirici_Vg_P6p_wt_mod_2_dataScale_Line,mean_random_Oonirici_Vg_P7p_wt_mod_2_dataScale_Line,mean_random_Oonirici_Vg_P8p_SSSS_mod_2_dataScale_Line)
  BLUP_Oonirici_Vg_mod_2_dataScale$Species <- rep("O.onirici", 32)
  View(BLUP_Oonirici_Vg_mod_2_dataScale)
  
  
}

BLUP_Oscheius_Vg_2_dataScale <- rbind.data.frame(BLUP_Otipulae_Vg_mod_2_dataScale,BLUP_Oonirici_Vg_mod_2_dataScale)
BLUP_Oscheius_Vg_2_dataScale <- cbind(Lines = rownames(BLUP_Oscheius_Vg_2_dataScale),BLUP_Oscheius_Vg_2_dataScale)
rownames(BLUP_Oscheius_Vg_2_dataScale) <- NULL
BLUP_Oscheius_Vg_2_dataScale$Treatment <- c(rep("CONTROL", 9), rep("WILD", 33),rep("CONTROL", 12), rep("WILD", 37),rep("CONTROL", 6), rep("WILD", 3),rep("CONTROL", 10), rep("WILD", 5))
View(BLUP_Oscheius_Vg_2_dataScale)
write_xlsx(BLUP_Oscheius_Vg_2_dataScale, "BLUP_Oscheius_Vg_SSSS_2_dataScale.xlsx")


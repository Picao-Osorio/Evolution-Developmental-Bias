#Vr_Oscheius_BLUP_SSSS
library(emmeans)
library(plotMCMC)
library(postMCMCglmm) 




#BLUP_Vr_Oscheius_P3_4_8p_SSSS_P5_6_7_wt
{
  #Extract BLUP of Lines Vr_Oscheius_P3p_SSSS
  {
    
    mean_random_Vr_Oscheius_P3p_SSSS_mod3_scaled <- ranef(Vr_Oscheius_P3p_SSSS_mod3_scaled, use = "mean") + mean(Vr_Oscheius_P3p_SSSS_mod3_scaled[["Sol"]][,1])
    View(mean_random_Vr_Oscheius_P3p_SSSS_mod3_scaled)
    mean_random_Vr_Oscheius_P3p_SSSS_mod3_scaled_dataScale <- pnorm(mean_random_Vr_Oscheius_P3p_SSSS_mod3_scaled)
    #View(mean_random_Vr_Oscheius_P3p_SSSS_mod3_scaled_dataScale)
    mean_random_Vr_Oscheius_P3p_SSSS_mod3_scaled_dataScale_Line <- as.data.frame(mean_random_Vr_Oscheius_P3p_SSSS_mod3_scaled_dataScale[c(24:47),])
    colnames(mean_random_Vr_Oscheius_P3p_SSSS_mod3_scaled_dataScale_Line) <- c("P3p_BLUP")
    View(mean_random_Vr_Oscheius_P3p_SSSS_mod3_scaled_dataScale_Line)
  }
  
  #Extract BLUP of Lines Vr_Oscheius_P4p_SSSS
  {
    mean_random_Vr_Oscheius_P4p_SSSS_mod3_scaled <- ranef(Vr_Oscheius_P4p_SSSS_mod3_scaled, use = "mean") + mean(Vr_Oscheius_P4p_SSSS_mod3_scaled[["Sol"]][,1])
    #View(mean_random_Vr_Oscheius_P4p_SSSS_mod3_scaled)
    mean_random_Vr_Oscheius_P4p_SSSS_mod3_scaled_dataScale <- pnorm(mean_random_Vr_Oscheius_P4p_SSSS_mod3_scaled)
    #View(mean_random_Vr_Oscheius_P4p_SSSS_mod3_scaled_dataScale)
    mean_random_Vr_Oscheius_P4p_SSSS_mod3_scaled_dataScale_Line <- as.data.frame(mean_random_Vr_Oscheius_P4p_SSSS_mod3_scaled_dataScale[c(24:47),])
    colnames(mean_random_Vr_Oscheius_P4p_SSSS_mod3_scaled_dataScale_Line) <- c("P4p_BLUP")
    #View(mean_random_Vr_Oscheius_P4p_SSSS_mod3_scaled_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines Vr_Oscheius_P8p_SSSS
  {
    mean_random_Vr_Oscheius_P8p_SSSS_mod3_scaled <- ranef(Vr_Oscheius_P8p_SSSS_mod3_scaled, use = "mean") + mean(Vr_Oscheius_P8p_SSSS_mod3_scaled[["Sol"]][,1])
    #View(mean_random_Vr_Oscheius_P8p_SSSS_mod3_scaled)
    mean_random_Vr_Oscheius_P8p_SSSS_mod3_scaled_dataScale <- pnorm(mean_random_Vr_Oscheius_P8p_SSSS_mod3_scaled)
    #View(mean_random_Vr_Oscheius_P8p_SSSS_mod3_scaled_dataScale)
    mean_random_Vr_Oscheius_P8p_SSSS_mod3_scaled_dataScale_Line <- as.data.frame(mean_random_Vr_Oscheius_P8p_SSSS_mod3_scaled_dataScale[c(24:47),])
    colnames(mean_random_Vr_Oscheius_P8p_SSSS_mod3_scaled_dataScale_Line) <- c("P8p_BLUP")
    #View(mean_random_Vr_Oscheius_P8p_SSSS_mod3_scaled_dataScale_Line)
    
  }
  
  #Extract BLUP of Lines Vr_Oscheius_P5p_wt
  {
    
    mean_random_Vr_Oscheius_P5p_wt_mod3_scaled <- ranef(Vr_Oscheius_P5p_wt_mod3_scaled, use = "mean") + mean(Vr_Oscheius_P5p_wt_mod3_scaled[["Sol"]][,1])
    #View(mean_random_Vr_Oscheius_P5p_wt_mod3_scaled)
    mean_random_Vr_Oscheius_P5p_wt_mod3_scaled_dataScale <- pnorm(mean_random_Vr_Oscheius_P5p_wt_mod3_scaled)
    #View(mean_random_Vr_Oscheius_P5p_wt_mod3_scaled_dataScale)
    mean_random_Vr_Oscheius_P5p_wt_mod3_scaled_dataScale_Line <- as.data.frame(mean_random_Vr_Oscheius_P5p_wt_mod3_scaled_dataScale[c(24:47),])
    colnames(mean_random_Vr_Oscheius_P5p_wt_mod3_scaled_dataScale_Line) <- c("P5p_BLUP")
    #View(mean_random_Vr_Oscheius_P5p_wt_mod3_scaled_dataScale_Line)
  }
  
  #Extract BLUP of Lines Vr_Oscheius_P6p_wt
  {
    mean_random_Vr_Oscheius_P6p_wt_mod3_scaled <- ranef(Vr_Oscheius_P6p_wt_mod3_scaled, use = "mean") + mean(Vr_Oscheius_P6p_wt_mod3_scaled[["Sol"]][,1])
    #View(mean_random_Vr_Oscheius_P6p_wt_mod3_scaled)
    mean_random_Vr_Oscheius_P6p_wt_mod3_scaled_dataScale <- pnorm(mean_random_Vr_Oscheius_P6p_wt_mod3_scaled)
    #View(mean_random_Vr_Oscheius_P6p_wt_mod3_scaled_dataScale)
    mean_random_Vr_Oscheius_P6p_wt_mod3_scaled_dataScale_Line <- as.data.frame(mean_random_Vr_Oscheius_P6p_wt_mod3_scaled_dataScale[c(24:47),])
    colnames(mean_random_Vr_Oscheius_P6p_wt_mod3_scaled_dataScale_Line) <- c("P6p_BLUP")
    #View(mean_random_Vr_Oscheius_P6p_wt_mod3_scaled_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines Vr_Oscheius_P7p_wt
  {
    mean_random_Vr_Oscheius_P7p_wt_mod3_scaled <- ranef(Vr_Oscheius_P7p_wt_mod3_scaled, use = "mean") + mean(Vr_Oscheius_P7p_wt_mod3_scaled[["Sol"]][,1])
    #View(mean_random_Vr_Oscheius_P7p_wt_mod3_scaled)
    mean_random_Vr_Oscheius_P7p_wt_mod3_scaled_dataScale <- pnorm(mean_random_Vr_Oscheius_P7p_wt_mod3_scaled)
    #View(mean_random_Vr_Oscheius_P7p_wt_mod3_scaled_dataScale)
    mean_random_Vr_Oscheius_P7p_wt_mod3_scaled_dataScale_Line <- as.data.frame(mean_random_Vr_Oscheius_P7p_wt_mod3_scaled_dataScale[c(24:47),])
    colnames(mean_random_Vr_Oscheius_P7p_wt_mod3_scaled_dataScale_Line) <- c("P7p_BLUP")
    #View(mean_random_Vr_Oscheius_P7p_wt_mod3_scaled_dataScale_Line)
    
  }
  
  
  BLUP_Vr_Oscheius_mod3_scaled_dataScale_SSSS <- cbind.data.frame(mean_random_Vr_Oscheius_P3p_SSSS_mod3_scaled_dataScale_Line,mean_random_Vr_Oscheius_P4p_SSSS_mod3_scaled_dataScale_Line,mean_random_Vr_Oscheius_P5p_wt_mod3_scaled_dataScale_Line,mean_random_Vr_Oscheius_P6p_wt_mod3_scaled_dataScale_Line,mean_random_Vr_Oscheius_P7p_wt_mod3_scaled_dataScale_Line,mean_random_Vr_Oscheius_P8p_SSSS_mod3_scaled_dataScale_Line)
  BLUP_Vr_Oscheius_mod3_scaled_dataScale_SSSS <- cbind(Species = rownames(BLUP_Vr_Oscheius_mod3_scaled_dataScale_SSSS),BLUP_Vr_Oscheius_mod3_scaled_dataScale_SSSS)
  rownames(BLUP_Vr_Oscheius_mod3_scaled_dataScale_SSSS) <- NULL
  
  BLUP_Vr_Oscheius_mod3_scaled_dataScale_SSSS$Genus <- rep("Oscheius", 24)
  BLUP_Vr_Oscheius_mod3_scaled_dataScale_SSSS$Pnp_fate <- rep("SSSS_wt", 24)
  
  BLUP_Vr_Oscheius_mod3_scaled_dataScale_SSSS$Variance <- rep("Vr", 24)
  
  View(BLUP_Vr_Oscheius_mod3_scaled_dataScale_SSSS)
  
  write_xlsx(BLUP_Vr_Oscheius_mod3_scaled_dataScale_SSSS, "Vr_Oscheius_BLUP_SSSS.xlsx")
  
}


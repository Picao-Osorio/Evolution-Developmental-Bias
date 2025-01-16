
#Vr_Caenorhabditis_BLUP_Lines
library(emmeans)
library(plotMCMC)
library(postMCMCglmm) 

#BLUP_Vr_Caenorhabditis_P3_4_8p_SS_P5_6_7_wt
{
  #Extract BLUP of Lines Vr_Caenorhabditis_P3p_SS
  {
    
    mean_random_Vr_Caenorhabditis_P3p_SS_mod3_scaled <- ranef(Vr_Caenorhabditis_P3p_SS_mod3_scaled, use = "mean") + mean(Vr_Caenorhabditis_P3p_SS_mod3_scaled[["Sol"]][,1])
    View(mean_random_Vr_Caenorhabditis_P3p_SS_mod3_scaled)
    mean_random_Vr_Caenorhabditis_P3p_SS_mod3_scaled_dataScale <- pnorm(mean_random_Vr_Caenorhabditis_P3p_SS_mod3_scaled)
    View(mean_random_Vr_Caenorhabditis_P3p_SS_mod3_scaled_dataScale)
    mean_random_Vr_Caenorhabditis_P3p_SS_mod3_scaled_dataScale_Line <- as.data.frame(mean_random_Vr_Caenorhabditis_P3p_SS_mod3_scaled_dataScale[c(55:107),])
    colnames(mean_random_Vr_Caenorhabditis_P3p_SS_mod3_scaled_dataScale_Line) <- c("P3p_BLUP")
    View(mean_random_Vr_Caenorhabditis_P3p_SS_mod3_scaled_dataScale_Line)
  }
  
  #Extract BLUP of Lines Vr_Caenorhabditis_P4p_SS
  {
    mean_random_Vr_Caenorhabditis_P4p_SS_mod3_scaled <- ranef(Vr_Caenorhabditis_P4p_SS_mod3_scaled, use = "mean") + mean(Vr_Caenorhabditis_P4p_SS_mod3_scaled[["Sol"]][,1])
    #View(mean_random_Vr_Caenorhabditis_P4p_SS_mod3_scaled)
    mean_random_Vr_Caenorhabditis_P4p_SS_mod3_scaled_dataScale <- pnorm(mean_random_Vr_Caenorhabditis_P4p_SS_mod3_scaled)
    #View(mean_random_Vr_Caenorhabditis_P4p_SS_mod3_scaled_dataScale)
    mean_random_Vr_Caenorhabditis_P4p_SS_mod3_scaled_dataScale_Line <- as.data.frame(mean_random_Vr_Caenorhabditis_P4p_SS_mod3_scaled_dataScale[c(55:107),])
    colnames(mean_random_Vr_Caenorhabditis_P4p_SS_mod3_scaled_dataScale_Line) <- c("P4p_BLUP")
    View(mean_random_Vr_Caenorhabditis_P4p_SS_mod3_scaled_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines Vr_Caenorhabditis_P8p_SS
  {
    mean_random_Vr_Caenorhabditis_P8p_SS_mod3_scaled <- ranef(Vr_Caenorhabditis_P8p_SS_mod3_scaled, use = "mean") + mean(Vr_Caenorhabditis_P8p_SS_mod3_scaled[["Sol"]][,1])
    #View(mean_random_Vr_Caenorhabditis_P8p_SS_mod3_scaled)
    mean_random_Vr_Caenorhabditis_P8p_SS_mod3_scaled_dataScale <- pnorm(mean_random_Vr_Caenorhabditis_P8p_SS_mod3_scaled)
    #View(mean_random_Vr_Caenorhabditis_P8p_SS_mod3_scaled_dataScale)
    mean_random_Vr_Caenorhabditis_P8p_SS_mod3_scaled_dataScale_Line <- as.data.frame(mean_random_Vr_Caenorhabditis_P8p_SS_mod3_scaled_dataScale[c(55:107),])
    colnames(mean_random_Vr_Caenorhabditis_P8p_SS_mod3_scaled_dataScale_Line) <- c("P8p_BLUP")
    #View(mean_random_Vr_Caenorhabditis_P8p_SS_mod3_scaled_dataScale_Line)
    
  }
  
  #Extract BLUP of Lines Vr_Caenorhabditis_P5p_wt
  {
    
    mean_random_Vr_Caenorhabditis_P5p_wt_mod3_scaled <- ranef(Vr_Caenorhabditis_P5p_wt_mod3_scaled, use = "mean") + mean(Vr_Caenorhabditis_P5p_wt_mod3_scaled[["Sol"]][,1])
    #View(mean_random_Vr_Caenorhabditis_P5p_wt_mod3_scaled)
    mean_random_Vr_Caenorhabditis_P5p_wt_mod3_scaled_dataScale <- pnorm(mean_random_Vr_Caenorhabditis_P5p_wt_mod3_scaled)
    #View(mean_random_Vr_Caenorhabditis_P5p_wt_mod3_scaled_dataScale)
    mean_random_Vr_Caenorhabditis_P5p_wt_mod3_scaled_dataScale_Line <- as.data.frame(mean_random_Vr_Caenorhabditis_P5p_wt_mod3_scaled_dataScale[c(55:107),])
    colnames(mean_random_Vr_Caenorhabditis_P5p_wt_mod3_scaled_dataScale_Line) <- c("P5p_BLUP")
    #View(mean_random_Vr_Caenorhabditis_P5p_wt_mod3_scaled_dataScale_Line)
  }
  
  #Extract BLUP of Lines Vr_Caenorhabditis_P6p_wt
  {
    mean_random_Vr_Caenorhabditis_P6p_wt_mod3_scaled <- ranef(Vr_Caenorhabditis_P6p_wt_mod3_scaled, use = "mean") + mean(Vr_Caenorhabditis_P6p_wt_mod3_scaled[["Sol"]][,1])
    #View(mean_random_Vr_Caenorhabditis_P6p_wt_mod3_scaled)
    mean_random_Vr_Caenorhabditis_P6p_wt_mod3_scaled_dataScale <- pnorm(mean_random_Vr_Caenorhabditis_P6p_wt_mod3_scaled)
    #View(mean_random_Vr_Caenorhabditis_P6p_wt_mod3_scaled_dataScale)
    mean_random_Vr_Caenorhabditis_P6p_wt_mod3_scaled_dataScale_Line <- as.data.frame(mean_random_Vr_Caenorhabditis_P6p_wt_mod3_scaled_dataScale[c(55:107),])
    colnames(mean_random_Vr_Caenorhabditis_P6p_wt_mod3_scaled_dataScale_Line) <- c("P6p_BLUP")
    #View(mean_random_Vr_Caenorhabditis_P6p_wt_mod3_scaled_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines Vr_Caenorhabditis_P7p_wt
  {
    mean_random_Vr_Caenorhabditis_P7p_wt_mod3_scaled <- ranef(Vr_Caenorhabditis_P7p_wt_mod3_scaled, use = "mean") + mean(Vr_Caenorhabditis_P7p_wt_mod3_scaled[["Sol"]][,1])
    #View(mean_random_Vr_Caenorhabditis_P7p_wt_mod3_scaled)
    mean_random_Vr_Caenorhabditis_P7p_wt_mod3_scaled_dataScale <- pnorm(mean_random_Vr_Caenorhabditis_P7p_wt_mod3_scaled)
    #View(mean_random_Vr_Caenorhabditis_P7p_wt_mod3_scaled_dataScale)
    mean_random_Vr_Caenorhabditis_P7p_wt_mod3_scaled_dataScale_Line <- as.data.frame(mean_random_Vr_Caenorhabditis_P7p_wt_mod3_scaled_dataScale[c(55:107),])
    colnames(mean_random_Vr_Caenorhabditis_P7p_wt_mod3_scaled_dataScale_Line) <- c("P7p_BLUP")
    #View(mean_random_Vr_Caenorhabditis_P7p_wt_mod3_scaled_dataScale_Line)
    
  }
  
  
  BLUP_Vr_Caenorhabditis_mod3_scaled_dataScale <- cbind.data.frame(mean_random_Vr_Caenorhabditis_P3p_SS_mod3_scaled_dataScale_Line,mean_random_Vr_Caenorhabditis_P4p_SS_mod3_scaled_dataScale_Line,mean_random_Vr_Caenorhabditis_P5p_wt_mod3_scaled_dataScale_Line,mean_random_Vr_Caenorhabditis_P6p_wt_mod3_scaled_dataScale_Line,mean_random_Vr_Caenorhabditis_P7p_wt_mod3_scaled_dataScale_Line,mean_random_Vr_Caenorhabditis_P8p_SS_mod3_scaled_dataScale_Line)
  BLUP_Vr_Caenorhabditis_mod3_scaled_dataScale <- cbind(Species = rownames(BLUP_Vr_Caenorhabditis_mod3_scaled_dataScale),BLUP_Vr_Caenorhabditis_mod3_scaled_dataScale)
  rownames(BLUP_Vr_Caenorhabditis_mod3_scaled_dataScale) <- NULL
  
  BLUP_Vr_Caenorhabditis_mod3_scaled_dataScale$Genus <- rep("Caenorhabditis", 53)
  BLUP_Vr_Caenorhabditis_mod3_scaled_dataScale$Pnp_fate <- rep("SS_wt", 53)
  BLUP_Vr_Caenorhabditis_mod3_scaled_dataScale$Variance <- rep("Vr", 53)
  
  View(BLUP_Vr_Caenorhabditis_mod3_scaled_dataScale)
  
  write_xlsx(BLUP_Vr_Caenorhabditis_mod3_scaled_dataScale, "Vr_Caenorhabditis_SS_BLUP.xlsx")
  
}




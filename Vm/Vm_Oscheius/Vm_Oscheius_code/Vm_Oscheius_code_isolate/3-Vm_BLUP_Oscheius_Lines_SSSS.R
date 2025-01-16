#BLUP_Oscheius_Lines_SSSS

library(plotMCMC)
library(postMCMCglmm) 
library(tidyverse)

#BLUP_CEW1_Vm----
{
  #Extract BLUP of Lines CEW1_Vm_P3p_SSSS
  {
    
    mean_random_CEW1_Vm_P3p_SSSS_mod <- ranef(CEW1_Vm_P3p_SSSS_mod, use = "mean") + mean(CEW1_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    View(mean_random_CEW1_Vm_P3p_SSSS_mod)
    mean_random_CEW1_Vm_P3p_SSSS_mod_dataScale <- pnorm(mean_random_CEW1_Vm_P3p_SSSS_mod)
    #View(mean_random_CEW1_Vm_P3p_SSSS_mod_dataScale)
    mean_random_CEW1_Vm_P3p_SSSS_mod_dataScale_Line <- as.data.frame(mean_random_CEW1_Vm_P3p_SSSS_mod_dataScale[c(1:68),])
    colnames(mean_random_CEW1_Vm_P3p_SSSS_mod_dataScale_Line) <- c("P3p_SSSS_BLUP")
    View(mean_random_CEW1_Vm_P3p_SSSS_mod_dataScale_Line)
  }
  
  #Extract BLUP of Lines CEW1_Vm_P4p_SSSS
  {
    
    mean_random_CEW1_Vm_P4p_SSSS_mod <- ranef(CEW1_Vm_P4p_SSSS_mod, use = "mean") + mean(CEW1_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_CEW1_Vm_P4p_SSSS_mod)
    mean_random_CEW1_Vm_P4p_SSSS_mod_dataScale <- pnorm(mean_random_CEW1_Vm_P4p_SSSS_mod)
    ##View(mean_random_CEW1_Vm_P4p_SSSS_mod_dataScale)
    mean_random_CEW1_Vm_P4p_SSSS_mod_dataScale_Line <- as.data.frame(mean_random_CEW1_Vm_P4p_SSSS_mod_dataScale[c(1:68),])
    colnames(mean_random_CEW1_Vm_P4p_SSSS_mod_dataScale_Line) <- c("P4p_SSSS_BLUP")
    View(mean_random_CEW1_Vm_P4p_SSSS_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines CEW1_Vm_P8p_SSSS
  {
    
    mean_random_CEW1_Vm_P8p_SSSS_mod <- ranef(CEW1_Vm_P8p_SSSS_mod, use = "mean") + mean(CEW1_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_CEW1_Vm_P8p_SSSS_mod)
    mean_random_CEW1_Vm_P8p_SSSS_mod_dataScale <- pnorm(mean_random_CEW1_Vm_P8p_SSSS_mod)
    ##View(mean_random_CEW1_Vm_P8p_SSSS_mod_dataScale)
    mean_random_CEW1_Vm_P8p_SSSS_mod_dataScale_Line <- as.data.frame(mean_random_CEW1_Vm_P8p_SSSS_mod_dataScale[c(1:68),])
    colnames(mean_random_CEW1_Vm_P8p_SSSS_mod_dataScale_Line) <- c("P8p_SSSS_BLUP")
    #View(mean_random_CEW1_Vm_P8p_SSSS_mod_dataScale_Line)
    
  }
  
  #Extract BLUP of Lines CEW1_Vm_P5p_wt
  {
    
    mean_random_CEW1_Vm_P5p_wt_mod <- ranef(CEW1_Vm_P5p_wt_mod, use = "mean") + mean(CEW1_Vm_P5p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_CEW1_Vm_P5p_wt_mod)
    mean_random_CEW1_Vm_P5p_wt_mod_dataScale <- pnorm(mean_random_CEW1_Vm_P5p_wt_mod)
    ##View(mean_random_CEW1_Vm_P5p_wt_mod_dataScale)
    mean_random_CEW1_Vm_P5p_wt_mod_dataScale_Line <- as.data.frame(mean_random_CEW1_Vm_P5p_wt_mod_dataScale[c(1:68),])
    colnames(mean_random_CEW1_Vm_P5p_wt_mod_dataScale_Line) <- c("P5p_wt_BLUP")
    #View(mean_random_CEW1_Vm_P5p_wt_mod_dataScale_Line)
  }
  
  #Extract BLUP of Lines CEW1_Vm_P6p_wt
  {
    
    mean_random_CEW1_Vm_P6p_wt_mod <- ranef(CEW1_Vm_P6p_wt_mod, use = "mean") + mean(CEW1_Vm_P6p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_CEW1_Vm_P6p_wt_mod)
    mean_random_CEW1_Vm_P6p_wt_mod_dataScale <- pnorm(mean_random_CEW1_Vm_P6p_wt_mod)
    ##View(mean_random_CEW1_Vm_P6p_wt_mod_dataScale)
    mean_random_CEW1_Vm_P6p_wt_mod_dataScale_Line <- as.data.frame(mean_random_CEW1_Vm_P6p_wt_mod_dataScale[c(1:68),])
    colnames(mean_random_CEW1_Vm_P6p_wt_mod_dataScale_Line) <- c("P6p_wt_BLUP")
    #View(mean_random_CEW1_Vm_P6p_wt_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines CEW1_Vm_P7p_wt
  {
    
    mean_random_CEW1_Vm_P7p_wt_mod <- ranef(CEW1_Vm_P7p_wt_mod, use = "mean") + mean(CEW1_Vm_P7p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_CEW1_Vm_P7p_wt_mod)
    mean_random_CEW1_Vm_P7p_wt_mod_dataScale <- pnorm(mean_random_CEW1_Vm_P7p_wt_mod)
    ##View(mean_random_CEW1_Vm_P7p_wt_mod_dataScale)
    mean_random_CEW1_Vm_P7p_wt_mod_dataScale_Line <- as.data.frame(mean_random_CEW1_Vm_P7p_wt_mod_dataScale[c(1:68),])
    colnames(mean_random_CEW1_Vm_P7p_wt_mod_dataScale_Line) <- c("P7p_wt_BLUP")
    #View(mean_random_CEW1_Vm_P7p_wt_mod_dataScale_Line)
    
  }
  
  
  BLUP_CEW1_Vm_P3_4_8p_SSSS_mod_dataScale <- cbind.data.frame(mean_random_CEW1_Vm_P3p_SSSS_mod_dataScale_Line,mean_random_CEW1_Vm_P4p_SSSS_mod_dataScale_Line,mean_random_CEW1_Vm_P5p_wt_mod_dataScale_Line,mean_random_CEW1_Vm_P6p_wt_mod_dataScale_Line,mean_random_CEW1_Vm_P7p_wt_mod_dataScale_Line,mean_random_CEW1_Vm_P8p_SSSS_mod_dataScale_Line)
  BLUP_CEW1_Vm_P3_4_8p_SSSS_mod_dataScale$Ancestor <- rep("CEW1", 68)
  BLUP_CEW1_Vm_P3_4_8p_SSSS_mod_dataScale$Species <- rep("O.tipulae", 68)
  BLUP_CEW1_Vm_P3_4_8p_SSSS_mod_dataScale$Treatment <- c(rep("CONTROL", 18), rep("ML", 50))
  View(BLUP_CEW1_Vm_P3_4_8p_SSSS_mod_dataScale)
  
  
}

#BLUP_JU178_Vm----
{
  #Extract BLUP of Lines JU178_Vm_P3p_SSSS
  {
    
    mean_random_JU178_Vm_P3p_SSSS_mod <- ranef(JU178_Vm_P3p_SSSS_mod, use = "mean") + mean(JU178_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    #View(mean_random_JU178_Vm_P3p_SSSS_mod)
    mean_random_JU178_Vm_P3p_SSSS_mod_dataScale <- pnorm(mean_random_JU178_Vm_P3p_SSSS_mod)
    View(mean_random_JU178_Vm_P3p_SSSS_mod_dataScale)
    mean_random_JU178_Vm_P3p_SSSS_mod_dataScale_Line <- as.data.frame(mean_random_JU178_Vm_P3p_SSSS_mod_dataScale[c(1:70),])
    colnames(mean_random_JU178_Vm_P3p_SSSS_mod_dataScale_Line) <- c("P3p_SSSS_BLUP")
    View(mean_random_JU178_Vm_P3p_SSSS_mod_dataScale_Line)
  }
  
  #Extract BLUP of Lines JU178_Vm_P4p_SSSS
  {
    
    mean_random_JU178_Vm_P4p_SSSS_mod <- ranef(JU178_Vm_P4p_SSSS_mod, use = "mean") + mean(JU178_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_JU178_Vm_P4p_SSSS_mod)
    mean_random_JU178_Vm_P4p_SSSS_mod_dataScale <- pnorm(mean_random_JU178_Vm_P4p_SSSS_mod)
    ##View(mean_random_JU178_Vm_P4p_SSSS_mod_dataScale)
    mean_random_JU178_Vm_P4p_SSSS_mod_dataScale_Line <- as.data.frame(mean_random_JU178_Vm_P4p_SSSS_mod_dataScale[c(1:70),])
    colnames(mean_random_JU178_Vm_P4p_SSSS_mod_dataScale_Line) <- c("P4p_SSSS_BLUP")
    #View(mean_random_JU178_Vm_P4p_SSSS_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines JU178_Vm_P8p_SSSS
  {
    
    mean_random_JU178_Vm_P8p_SSSS_mod <- ranef(JU178_Vm_P8p_SSSS_mod, use = "mean") + mean(JU178_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_JU178_Vm_P8p_SSSS_mod)
    mean_random_JU178_Vm_P8p_SSSS_mod_dataScale <- pnorm(mean_random_JU178_Vm_P8p_SSSS_mod)
    ##View(mean_random_JU178_Vm_P8p_SSSS_mod_dataScale)
    mean_random_JU178_Vm_P8p_SSSS_mod_dataScale_Line <- as.data.frame(mean_random_JU178_Vm_P8p_SSSS_mod_dataScale[c(1:70),])
    colnames(mean_random_JU178_Vm_P8p_SSSS_mod_dataScale_Line) <- c("P8p_SSSS_BLUP")
    #View(mean_random_JU178_Vm_P8p_SSSS_mod_dataScale_Line)
    
  }
  
  #Extract BLUP of Lines JU178_Vm_P5p_wt
  {
    
    mean_random_JU178_Vm_P5p_wt_mod <- ranef(JU178_Vm_P5p_wt_mod, use = "mean") + mean(JU178_Vm_P5p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_JU178_Vm_P5p_wt_mod)
    mean_random_JU178_Vm_P5p_wt_mod_dataScale <- pnorm(mean_random_JU178_Vm_P5p_wt_mod)
    ##View(mean_random_JU178_Vm_P5p_wt_mod_dataScale)
    mean_random_JU178_Vm_P5p_wt_mod_dataScale_Line <- as.data.frame(mean_random_JU178_Vm_P5p_wt_mod_dataScale[c(1:70),])
    colnames(mean_random_JU178_Vm_P5p_wt_mod_dataScale_Line) <- c("P5p_wt_BLUP")
    #View(mean_random_JU178_Vm_P5p_wt_mod_dataScale_Line)
  }
  
  #Extract BLUP of Lines JU178_Vm_P6p_wt
  {
    
    mean_random_JU178_Vm_P6p_wt_mod <- ranef(JU178_Vm_P6p_wt_mod, use = "mean") + mean(JU178_Vm_P6p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_JU178_Vm_P6p_wt_mod)
    mean_random_JU178_Vm_P6p_wt_mod_dataScale <- pnorm(mean_random_JU178_Vm_P6p_wt_mod)
    ##View(mean_random_JU178_Vm_P6p_wt_mod_dataScale)
    mean_random_JU178_Vm_P6p_wt_mod_dataScale_Line <- as.data.frame(mean_random_JU178_Vm_P6p_wt_mod_dataScale[c(1:70),])
    colnames(mean_random_JU178_Vm_P6p_wt_mod_dataScale_Line) <- c("P6p_wt_BLUP")
    #View(mean_random_JU178_Vm_P6p_wt_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines JU178_Vm_P7p_wt
  {
    
    mean_random_JU178_Vm_P7p_wt_mod <- ranef(JU178_Vm_P7p_wt_mod, use = "mean") + mean(JU178_Vm_P7p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_JU178_Vm_P7p_wt_mod)
    mean_random_JU178_Vm_P7p_wt_mod_dataScale <- pnorm(mean_random_JU178_Vm_P7p_wt_mod)
    ##View(mean_random_JU178_Vm_P7p_wt_mod_dataScale)
    mean_random_JU178_Vm_P7p_wt_mod_dataScale_Line <- as.data.frame(mean_random_JU178_Vm_P7p_wt_mod_dataScale[c(1:70),])
    colnames(mean_random_JU178_Vm_P7p_wt_mod_dataScale_Line) <- c("P7p_wt_BLUP")
    #View(mean_random_JU178_Vm_P7p_wt_mod_dataScale_Line)
    
  }
  
  
  BLUP_JU178_Vm_P3_4_8p_SSSS_mod_dataScale <- cbind.data.frame(mean_random_JU178_Vm_P3p_SSSS_mod_dataScale_Line,mean_random_JU178_Vm_P4p_SSSS_mod_dataScale_Line,mean_random_JU178_Vm_P5p_wt_mod_dataScale_Line,mean_random_JU178_Vm_P6p_wt_mod_dataScale_Line,mean_random_JU178_Vm_P7p_wt_mod_dataScale_Line,mean_random_JU178_Vm_P8p_SSSS_mod_dataScale_Line)
  BLUP_JU178_Vm_P3_4_8p_SSSS_mod_dataScale$Ancestor <- rep("JU178", 70)
  BLUP_JU178_Vm_P3_4_8p_SSSS_mod_dataScale$Species <- rep("O.tipulae", 70)
  BLUP_JU178_Vm_P3_4_8p_SSSS_mod_dataScale$Treatment <- c(rep("CONTROL", 20),rep("ML", 50))
  View(BLUP_JU178_Vm_P3_4_8p_SSSS_mod_dataScale)
  
  
}


#BLUP_PS2068_Vm ----
{
  #Extract BLUP of Lines PS2068_Vm_P3p_SSSS
  {
    
    mean_random_PS2068_Vm_P3p_SSSS_mod <- ranef(PS2068_Vm_P3p_SSSS_mod, use = "mean") + mean(PS2068_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    #View(mean_random_PS2068_Vm_P3p_SSSS_mod)
    mean_random_PS2068_Vm_P3p_SSSS_mod_dataScale <- pnorm(mean_random_PS2068_Vm_P3p_SSSS_mod)
    View(mean_random_PS2068_Vm_P3p_SSSS_mod_dataScale)
    mean_random_PS2068_Vm_P3p_SSSS_mod_dataScale_Line <- as.data.frame(mean_random_PS2068_Vm_P3p_SSSS_mod_dataScale[c(1:72),])
    colnames(mean_random_PS2068_Vm_P3p_SSSS_mod_dataScale_Line) <- c("P3p_SSSS_BLUP")
    #View(mean_random_PS2068_Vm_P3p_SSSS_mod_dataScale_Line)
  }
  
  #Extract BLUP of Lines PS2068_Vm_P4p_SSSS
  {
    
    mean_random_PS2068_Vm_P4p_SSSS_mod <- ranef(PS2068_Vm_P4p_SSSS_mod, use = "mean") + mean(PS2068_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_PS2068_Vm_P4p_SSSS_mod)
    mean_random_PS2068_Vm_P4p_SSSS_mod_dataScale <- pnorm(mean_random_PS2068_Vm_P4p_SSSS_mod)
    ##View(mean_random_PS2068_Vm_P4p_SSSS_mod_dataScale)
    mean_random_PS2068_Vm_P4p_SSSS_mod_dataScale_Line <- as.data.frame(mean_random_PS2068_Vm_P4p_SSSS_mod_dataScale[c(1:72),])
    colnames(mean_random_PS2068_Vm_P4p_SSSS_mod_dataScale_Line) <- c("P4p_SSSS_BLUP")
    #View(mean_random_PS2068_Vm_P4p_SSSS_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines PS2068_Vm_P8p_SSSS
  {
    
    mean_random_PS2068_Vm_P8p_SSSS_mod <- ranef(PS2068_Vm_P8p_SSSS_mod, use = "mean") + mean(PS2068_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_PS2068_Vm_P8p_SSSS_mod)
    mean_random_PS2068_Vm_P8p_SSSS_mod_dataScale <- pnorm(mean_random_PS2068_Vm_P8p_SSSS_mod)
    ##View(mean_random_PS2068_Vm_P8p_SSSS_mod_dataScale)
    mean_random_PS2068_Vm_P8p_SSSS_mod_dataScale_Line <- as.data.frame(mean_random_PS2068_Vm_P8p_SSSS_mod_dataScale[c(1:72),])
    colnames(mean_random_PS2068_Vm_P8p_SSSS_mod_dataScale_Line) <- c("P8p_SSSS_BLUP")
    #View(mean_random_PS2068_Vm_P8p_SSSS_mod_dataScale_Line)
    
  }
  
  #Extract BLUP of Lines PS2068_Vm_P5p_wt
  {
    
    mean_random_PS2068_Vm_P5p_wt_mod <- ranef(PS2068_Vm_P5p_wt_mod, use = "mean") + mean(PS2068_Vm_P5p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_PS2068_Vm_P5p_wt_mod)
    mean_random_PS2068_Vm_P5p_wt_mod_dataScale <- pnorm(mean_random_PS2068_Vm_P5p_wt_mod)
    ##View(mean_random_PS2068_Vm_P5p_wt_mod_dataScale)
    mean_random_PS2068_Vm_P5p_wt_mod_dataScale_Line <- as.data.frame(mean_random_PS2068_Vm_P5p_wt_mod_dataScale[c(1:72),])
    colnames(mean_random_PS2068_Vm_P5p_wt_mod_dataScale_Line) <- c("P5p_wt_BLUP")
    #View(mean_random_PS2068_Vm_P5p_wt_mod_dataScale_Line)
  }
  
  #Extract BLUP of Lines PS2068_Vm_P6p_wt
  {
    
    mean_random_PS2068_Vm_P6p_wt_mod <- ranef(PS2068_Vm_P6p_wt_mod, use = "mean") + mean(PS2068_Vm_P6p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_PS2068_Vm_P6p_wt_mod)
    mean_random_PS2068_Vm_P6p_wt_mod_dataScale <- pnorm(mean_random_PS2068_Vm_P6p_wt_mod)
    ##View(mean_random_PS2068_Vm_P6p_wt_mod_dataScale)
    mean_random_PS2068_Vm_P6p_wt_mod_dataScale_Line <- as.data.frame(mean_random_PS2068_Vm_P6p_wt_mod_dataScale[c(1:72),])
    colnames(mean_random_PS2068_Vm_P6p_wt_mod_dataScale_Line) <- c("P6p_wt_BLUP")
    #View(mean_random_PS2068_Vm_P6p_wt_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines PS2068_Vm_P7p_wt
  {
    
    mean_random_PS2068_Vm_P7p_wt_mod <- ranef(PS2068_Vm_P7p_wt_mod, use = "mean") + mean(PS2068_Vm_P7p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_PS2068_Vm_P7p_wt_mod)
    mean_random_PS2068_Vm_P7p_wt_mod_dataScale <- pnorm(mean_random_PS2068_Vm_P7p_wt_mod)
    ##View(mean_random_PS2068_Vm_P7p_wt_mod_dataScale)
    mean_random_PS2068_Vm_P7p_wt_mod_dataScale_Line <- as.data.frame(mean_random_PS2068_Vm_P7p_wt_mod_dataScale[c(1:72),])
    colnames(mean_random_PS2068_Vm_P7p_wt_mod_dataScale_Line) <- c("P7p_wt_BLUP")
    #View(mean_random_PS2068_Vm_P7p_wt_mod_dataScale_Line)
    
  }
  
  
  BLUP_PS2068_Vm_P3_4_8p_SSSS_mod_dataScale <- cbind.data.frame(mean_random_PS2068_Vm_P3p_SSSS_mod_dataScale_Line,mean_random_PS2068_Vm_P4p_SSSS_mod_dataScale_Line,mean_random_PS2068_Vm_P5p_wt_mod_dataScale_Line,mean_random_PS2068_Vm_P6p_wt_mod_dataScale_Line,mean_random_PS2068_Vm_P7p_wt_mod_dataScale_Line,mean_random_PS2068_Vm_P8p_SSSS_mod_dataScale_Line)
  BLUP_PS2068_Vm_P3_4_8p_SSSS_mod_dataScale$Ancestor <- rep("PS2068", 72)
  BLUP_PS2068_Vm_P3_4_8p_SSSS_mod_dataScale$Species <- rep("O.onirici", 72)
  BLUP_PS2068_Vm_P3_4_8p_SSSS_mod_dataScale$Treatment <- c(rep("ML", 50), rep("CONTROL", 22))
  View(BLUP_PS2068_Vm_P3_4_8p_SSSS_mod_dataScale)
  
  
}


#BLUP_JU77_Vm ----
{
  #Extract BLUP of Lines JU77_Vm_P3p_SSSS
  {
    
    mean_random_JU77_Vm_P3p_SSSS_mod <- ranef(JU77_Vm_P3p_SSSS_mod, use = "mean") + mean(JU77_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    #View(mean_random_JU77_Vm_P3p_SSSS_mod)
    mean_random_JU77_Vm_P3p_SSSS_mod_dataScale <- pnorm(mean_random_JU77_Vm_P3p_SSSS_mod)
    View(mean_random_JU77_Vm_P3p_SSSS_mod_dataScale)
    mean_random_JU77_Vm_P3p_SSSS_mod_dataScale_Line <- as.data.frame(mean_random_JU77_Vm_P3p_SSSS_mod_dataScale[c(1:66),])
    colnames(mean_random_JU77_Vm_P3p_SSSS_mod_dataScale_Line) <- c("P3p_SSSS_BLUP")
    #View(mean_random_JU77_Vm_P3p_SSSS_mod_dataScale_Line)
  }
  
  #Extract BLUP of Lines JU77_Vm_P4p_SSSS
  {
    
    mean_random_JU77_Vm_P4p_SSSS_mod <- ranef(JU77_Vm_P4p_SSSS_mod, use = "mean") + mean(JU77_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_JU77_Vm_P4p_SSSS_mod)
    mean_random_JU77_Vm_P4p_SSSS_mod_dataScale <- pnorm(mean_random_JU77_Vm_P4p_SSSS_mod)
    ##View(mean_random_JU77_Vm_P4p_SSSS_mod_dataScale)
    mean_random_JU77_Vm_P4p_SSSS_mod_dataScale_Line <- as.data.frame(mean_random_JU77_Vm_P4p_SSSS_mod_dataScale[c(1:66),])
    colnames(mean_random_JU77_Vm_P4p_SSSS_mod_dataScale_Line) <- c("P4p_SSSS_BLUP")
    #View(mean_random_JU77_Vm_P4p_SSSS_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines JU77_Vm_P8p_SSSS
  {
    
    mean_random_JU77_Vm_P8p_SSSS_mod <- ranef(JU77_Vm_P8p_SSSS_mod, use = "mean") + mean(JU77_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_JU77_Vm_P8p_SSSS_mod)
    mean_random_JU77_Vm_P8p_SSSS_mod_dataScale <- pnorm(mean_random_JU77_Vm_P8p_SSSS_mod)
    ##View(mean_random_JU77_Vm_P8p_SSSS_mod_dataScale)
    mean_random_JU77_Vm_P8p_SSSS_mod_dataScale_Line <- as.data.frame(mean_random_JU77_Vm_P8p_SSSS_mod_dataScale[c(1:66),])
    colnames(mean_random_JU77_Vm_P8p_SSSS_mod_dataScale_Line) <- c("P8p_SSSS_BLUP")
    #View(mean_random_JU77_Vm_P8p_SSSS_mod_dataScale_Line)
    
  }
  
  #Extract BLUP of Lines JU77_Vm_P5p_wt
  {
    
    mean_random_JU77_Vm_P5p_wt_mod <- ranef(JU77_Vm_P5p_wt_mod, use = "mean") + mean(JU77_Vm_P5p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_JU77_Vm_P5p_wt_mod)
    mean_random_JU77_Vm_P5p_wt_mod_dataScale <- pnorm(mean_random_JU77_Vm_P5p_wt_mod)
    ##View(mean_random_JU77_Vm_P5p_wt_mod_dataScale)
    mean_random_JU77_Vm_P5p_wt_mod_dataScale_Line <- as.data.frame(mean_random_JU77_Vm_P5p_wt_mod_dataScale[c(1:66),])
    colnames(mean_random_JU77_Vm_P5p_wt_mod_dataScale_Line) <- c("P5p_wt_BLUP")
    #View(mean_random_JU77_Vm_P5p_wt_mod_dataScale_Line)
  }
  
  #Extract BLUP of Lines JU77_Vm_P6p_wt
  {
    
    mean_random_JU77_Vm_P6p_wt_mod <- ranef(JU77_Vm_P6p_wt_mod, use = "mean") + mean(JU77_Vm_P6p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_JU77_Vm_P6p_wt_mod)
    mean_random_JU77_Vm_P6p_wt_mod_dataScale <- pnorm(mean_random_JU77_Vm_P6p_wt_mod)
    ##View(mean_random_JU77_Vm_P6p_wt_mod_dataScale)
    mean_random_JU77_Vm_P6p_wt_mod_dataScale_Line <- as.data.frame(mean_random_JU77_Vm_P6p_wt_mod_dataScale[c(1:66),])
    colnames(mean_random_JU77_Vm_P6p_wt_mod_dataScale_Line) <- c("P6p_wt_BLUP")
    #View(mean_random_JU77_Vm_P6p_wt_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines JU77_Vm_P7p_wt
  {
    
    mean_random_JU77_Vm_P7p_wt_mod <- ranef(JU77_Vm_P7p_wt_mod, use = "mean") + mean(JU77_Vm_P7p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_JU77_Vm_P7p_wt_mod)
    mean_random_JU77_Vm_P7p_wt_mod_dataScale <- pnorm(mean_random_JU77_Vm_P7p_wt_mod)
    ##View(mean_random_JU77_Vm_P7p_wt_mod_dataScale)
    mean_random_JU77_Vm_P7p_wt_mod_dataScale_Line <- as.data.frame(mean_random_JU77_Vm_P7p_wt_mod_dataScale[c(1:66),])
    colnames(mean_random_JU77_Vm_P7p_wt_mod_dataScale_Line) <- c("P7p_wt_BLUP")
    #View(mean_random_JU77_Vm_P7p_wt_mod_dataScale_Line)
    
  }
  
  
  BLUP_JU77_Vm_P3_4_8p_SSSS_mod_dataScale <- cbind.data.frame(mean_random_JU77_Vm_P3p_SSSS_mod_dataScale_Line,mean_random_JU77_Vm_P4p_SSSS_mod_dataScale_Line,mean_random_JU77_Vm_P5p_wt_mod_dataScale_Line,mean_random_JU77_Vm_P6p_wt_mod_dataScale_Line,mean_random_JU77_Vm_P7p_wt_mod_dataScale_Line,mean_random_JU77_Vm_P8p_SSSS_mod_dataScale_Line)
  BLUP_JU77_Vm_P3_4_8p_SSSS_mod_dataScale$Ancestor <- rep("JU77", 66)
  BLUP_JU77_Vm_P3_4_8p_SSSS_mod_dataScale$Species <- rep("O.onirici", 66)
  BLUP_JU77_Vm_P3_4_8p_SSSS_mod_dataScale$Treatment <- c(rep("ML", 50), rep("CONTROL", 16))
  
  View(BLUP_JU77_Vm_P3_4_8p_SSSS_mod_dataScale)
  
  
}

##----
BLUP_Oscheius_Vm_mod_dataScale_SSSS <- rbind.data.frame(BLUP_CEW1_Vm_P3_4_8p_SSSS_mod_dataScale, BLUP_JU178_Vm_P3_4_8p_SSSS_mod_dataScale,BLUP_PS2068_Vm_P3_4_8p_SSSS_mod_dataScale,BLUP_JU77_Vm_P3_4_8p_SSSS_mod_dataScale)
BLUP_Oscheius_Vm_mod_dataScale_SSSS <- cbind(Lines = rownames(BLUP_Oscheius_Vm_mod_dataScale_SSSS),BLUP_Oscheius_Vm_mod_dataScale_SSSS)
BLUP_Oscheius_Vm_mod_dataScale_SSSS <- BLUP_Oscheius_Vm_mod_dataScale_SSSS %>%  mutate(Lines = str_remove_all(Lines, "LineB."))
BLUP_Oscheius_Vm_mod_dataScale_SSSS <- BLUP_Oscheius_Vm_mod_dataScale_SSSS %>% mutate(Treatment= str_replace(Treatment,'CONTROL', 'Ancestral'))

rownames(BLUP_Oscheius_Vm_mod_dataScale_SSSS) <- NULL
View(BLUP_Oscheius_Vm_mod_dataScale_SSSS)
write_xlsx(BLUP_Oscheius_Vm_mod_dataScale_SSSS, "BLUP_Oscheius_Vm_SSSS_wt_mod_dataScale.xlsx")



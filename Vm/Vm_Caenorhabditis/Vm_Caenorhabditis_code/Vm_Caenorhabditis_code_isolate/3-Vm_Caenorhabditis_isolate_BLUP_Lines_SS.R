#Vm_Caenorhabditis_isolate_BLUP_Lines_SS
library(plotMCMC)
library(postMCMCglmm) 
library(tidyverse)


#BLUP_JU1200_Vm----
{
  #Extract BLUP of Lines JU1200_Vm_P3p_SS
  {
    
    
    mean_random_JU1200_Vm_P3p_SS_mod <- ranef(JU1200_Vm_P3p_SS_mod, use = "mean") + mean(JU1200_Vm_P3p_SS_mod[["Sol"]][,c(1:4)])
    #View(mean_random_JU1200_Vm_P3p_SS_mod)
    mean_random_JU1200_Vm_P3p_SS_mod_dataScale <- pnorm(mean_random_JU1200_Vm_P3p_SS_mod)
    #View(mean_random_JU1200_Vm_P3p_SS_mod_dataScale)
    mean_random_JU1200_Vm_P3p_SS_mod_dataScale_Line <- as.data.frame(mean_random_JU1200_Vm_P3p_SS_mod_dataScale[c(1:74),])
    colnames(mean_random_JU1200_Vm_P3p_SS_mod_dataScale_Line) <- c("P3p_SS_BLUP")
    # View(mean_random_JU1200_Vm_P3p_SS_mod_dataScale_Line)
  }
  
  #Extract BLUP of Lines JU1200_Vm_P4p_SS
  {
    
    mean_random_JU1200_Vm_P4p_SS_mod <- ranef(JU1200_Vm_P4p_SS_mod, use = "mean") + mean(JU1200_Vm_P4p_SS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_JU1200_Vm_P4p_SS_mod)
    mean_random_JU1200_Vm_P4p_SS_mod_dataScale <- pnorm(mean_random_JU1200_Vm_P4p_SS_mod)
    ##View(mean_random_JU1200_Vm_P4p_SS_mod_dataScale)
    mean_random_JU1200_Vm_P4p_SS_mod_dataScale_Line <- as.data.frame(mean_random_JU1200_Vm_P4p_SS_mod_dataScale[c(1:74),])
    colnames(mean_random_JU1200_Vm_P4p_SS_mod_dataScale_Line) <- c("P4p_SS_BLUP")
    #View(mean_random_JU1200_Vm_P4p_SS_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines JU1200_Vm_P8p_SS
  {
    
    mean_random_JU1200_Vm_P8p_SS_mod <- ranef(JU1200_Vm_P8p_SS_mod, use = "mean") + mean(JU1200_Vm_P8p_SS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_JU1200_Vm_P8p_SS_mod)
    mean_random_JU1200_Vm_P8p_SS_mod_dataScale <- pnorm(mean_random_JU1200_Vm_P8p_SS_mod)
    ##View(mean_random_JU1200_Vm_P8p_SS_mod_dataScale)
    mean_random_JU1200_Vm_P8p_SS_mod_dataScale_Line <- as.data.frame(mean_random_JU1200_Vm_P8p_SS_mod_dataScale[c(1:74),])
    colnames(mean_random_JU1200_Vm_P8p_SS_mod_dataScale_Line) <- c("P8p_SS_BLUP")
    #View(mean_random_JU1200_Vm_P8p_SS_mod_dataScale_Line)
    
  }
  
  #Extract BLUP of Lines JU1200_Vm_P5p_wt
  {
    
    mean_random_JU1200_Vm_P5p_wt_mod <- ranef(JU1200_Vm_P5p_wt_mod, use = "mean") + mean(JU1200_Vm_P5p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_JU1200_Vm_P5p_wt_mod)
    mean_random_JU1200_Vm_P5p_wt_mod_dataScale <- pnorm(mean_random_JU1200_Vm_P5p_wt_mod)
    ##View(mean_random_JU1200_Vm_P5p_wt_mod_dataScale)
    mean_random_JU1200_Vm_P5p_wt_mod_dataScale_Line <- as.data.frame(mean_random_JU1200_Vm_P5p_wt_mod_dataScale[c(1:74),])
    colnames(mean_random_JU1200_Vm_P5p_wt_mod_dataScale_Line) <- c("P5p_wt_BLUP")
    #View(mean_random_JU1200_Vm_P5p_wt_mod_dataScale_Line)
  }
  
  #Extract BLUP of Lines JU1200_Vm_P6p_wt
  {
    
    mean_random_JU1200_Vm_P6p_wt_mod <- ranef(JU1200_Vm_P6p_wt_mod, use = "mean") + mean(JU1200_Vm_P6p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_JU1200_Vm_P6p_wt_mod)
    mean_random_JU1200_Vm_P6p_wt_mod_dataScale <- pnorm(mean_random_JU1200_Vm_P6p_wt_mod)
    ##View(mean_random_JU1200_Vm_P6p_wt_mod_dataScale)
    mean_random_JU1200_Vm_P6p_wt_mod_dataScale_Line <- as.data.frame(mean_random_JU1200_Vm_P6p_wt_mod_dataScale[c(1:74),])
    colnames(mean_random_JU1200_Vm_P6p_wt_mod_dataScale_Line) <- c("P6p_wt_BLUP")
    #View(mean_random_JU1200_Vm_P6p_wt_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines JU1200_Vm_P7p_wt
  {
    
    mean_random_JU1200_Vm_P7p_wt_mod <- ranef(JU1200_Vm_P7p_wt_mod, use = "mean") + mean(JU1200_Vm_P7p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_JU1200_Vm_P7p_wt_mod)
    mean_random_JU1200_Vm_P7p_wt_mod_dataScale <- pnorm(mean_random_JU1200_Vm_P7p_wt_mod)
    ##View(mean_random_JU1200_Vm_P7p_wt_mod_dataScale)
    mean_random_JU1200_Vm_P7p_wt_mod_dataScale_Line <- as.data.frame(mean_random_JU1200_Vm_P7p_wt_mod_dataScale[c(1:74),])
    colnames(mean_random_JU1200_Vm_P7p_wt_mod_dataScale_Line) <- c("P7p_wt_BLUP")
    #View(mean_random_JU1200_Vm_P7p_wt_mod_dataScale_Line)
    
  }
  
  
  BLUP_JU1200_Vm_P3_4_8p_SS_mod_dataScale <- cbind.data.frame(mean_random_JU1200_Vm_P3p_SS_mod_dataScale_Line,mean_random_JU1200_Vm_P4p_SS_mod_dataScale_Line,mean_random_JU1200_Vm_P5p_wt_mod_dataScale_Line,mean_random_JU1200_Vm_P6p_wt_mod_dataScale_Line,mean_random_JU1200_Vm_P7p_wt_mod_dataScale_Line,mean_random_JU1200_Vm_P8p_SS_mod_dataScale_Line)
  BLUP_JU1200_Vm_P3_4_8p_SS_mod_dataScale$Ancestor <- rep("JU1200", 74)
  BLUP_JU1200_Vm_P3_4_8p_SS_mod_dataScale$Species <- rep("C.elegans", 74)
  BLUP_JU1200_Vm_P3_4_8p_SS_mod_dataScale$Treatment <- c(rep("CONTROL", 24), rep("MA", 50))
  View(BLUP_JU1200_Vm_P3_4_8p_SS_mod_dataScale)
  
  
}

#BLUP_PB306_Vm----
{
  #Extract BLUP of Lines PB306_Vm_P3p_SS
  {
    
    
    mean_random_PB306_Vm_P3p_SS_mod <- ranef(PB306_Vm_P3p_SS_mod, use = "mean") + mean(PB306_Vm_P3p_SS_mod[["Sol"]][,c(1:4)])
    #View(mean_random_PB306_Vm_P3p_SS_mod)
    mean_random_PB306_Vm_P3p_SS_mod_dataScale <- pnorm(mean_random_PB306_Vm_P3p_SS_mod)
    #View(mean_random_PB306_Vm_P3p_SS_mod_dataScale)
    mean_random_PB306_Vm_P3p_SS_mod_dataScale_Line <- as.data.frame(mean_random_PB306_Vm_P3p_SS_mod_dataScale[c(1:75),])
    colnames(mean_random_PB306_Vm_P3p_SS_mod_dataScale_Line) <- c("P3p_SS_BLUP")
    #View(mean_random_PB306_Vm_P3p_SS_mod_dataScale_Line)
  }
  
  #Extract BLUP of Lines PB306_Vm_P4p_SS
  {
    
    
    mean_random_PB306_Vm_P4p_SS_mod <- ranef(PB306_Vm_P4p_SS_mod, use = "mean") + mean(PB306_Vm_P4p_SS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_PB306_Vm_P4p_SS_mod)
    mean_random_PB306_Vm_P4p_SS_mod_dataScale <- pnorm(mean_random_PB306_Vm_P4p_SS_mod)
    ##View(mean_random_PB306_Vm_P4p_SS_mod_dataScale)
    mean_random_PB306_Vm_P4p_SS_mod_dataScale_Line <- as.data.frame(mean_random_PB306_Vm_P4p_SS_mod_dataScale[c(1:75),])
    colnames(mean_random_PB306_Vm_P4p_SS_mod_dataScale_Line) <- c("P4p_SS_BLUP")
    #View(mean_random_PB306_Vm_P4p_SS_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines PB306_Vm_P8p_SS
  {
    
    mean_random_PB306_Vm_P8p_SS_mod <- ranef(PB306_Vm_P8p_SS_mod, use = "mean") + mean(PB306_Vm_P8p_SS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_PB306_Vm_P8p_SS_mod)
    mean_random_PB306_Vm_P8p_SS_mod_dataScale <- pnorm(mean_random_PB306_Vm_P8p_SS_mod)
    ##View(mean_random_PB306_Vm_P8p_SS_mod_dataScale)
    mean_random_PB306_Vm_P8p_SS_mod_dataScale_Line <- as.data.frame(mean_random_PB306_Vm_P8p_SS_mod_dataScale[c(1:75),])
    colnames(mean_random_PB306_Vm_P8p_SS_mod_dataScale_Line) <- c("P8p_SS_BLUP")
    #View(mean_random_PB306_Vm_P8p_SS_mod_dataScale_Line)
    
  }
  
  #Extract BLUP of Lines PB306_Vm_P5p_wt
  {
    
    mean_random_PB306_Vm_P5p_wt_mod <- ranef(PB306_Vm_P5p_wt_mod, use = "mean") + mean(PB306_Vm_P5p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_PB306_Vm_P5p_wt_mod)
    mean_random_PB306_Vm_P5p_wt_mod_dataScale <- pnorm(mean_random_PB306_Vm_P5p_wt_mod)
    ##View(mean_random_PB306_Vm_P5p_wt_mod_dataScale)
    mean_random_PB306_Vm_P5p_wt_mod_dataScale_Line <- as.data.frame(mean_random_PB306_Vm_P5p_wt_mod_dataScale[c(1:75),])
    colnames(mean_random_PB306_Vm_P5p_wt_mod_dataScale_Line) <- c("P5p_wt_BLUP")
    #View(mean_random_PB306_Vm_P5p_wt_mod_dataScale_Line)
  }
  
  #Extract BLUP of Lines PB306_Vm_P6p_wt
  {
    
    mean_random_PB306_Vm_P6p_wt_mod <- ranef(PB306_Vm_P6p_wt_mod, use = "mean") + mean(PB306_Vm_P6p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_PB306_Vm_P6p_wt_mod)
    mean_random_PB306_Vm_P6p_wt_mod_dataScale <- pnorm(mean_random_PB306_Vm_P6p_wt_mod)
    ##View(mean_random_PB306_Vm_P6p_wt_mod_dataScale)
    mean_random_PB306_Vm_P6p_wt_mod_dataScale_Line <- as.data.frame(mean_random_PB306_Vm_P6p_wt_mod_dataScale[c(1:75),])
    colnames(mean_random_PB306_Vm_P6p_wt_mod_dataScale_Line) <- c("P6p_wt_BLUP")
    #View(mean_random_PB306_Vm_P6p_wt_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines PB306_Vm_P7p_wt
  {
    
    mean_random_PB306_Vm_P7p_wt_mod <- ranef(PB306_Vm_P7p_wt_mod, use = "mean") + mean(PB306_Vm_P7p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_PB306_Vm_P7p_wt_mod)
    mean_random_PB306_Vm_P7p_wt_mod_dataScale <- pnorm(mean_random_PB306_Vm_P7p_wt_mod)
    ##View(mean_random_PB306_Vm_P7p_wt_mod_dataScale)
    mean_random_PB306_Vm_P7p_wt_mod_dataScale_Line <- as.data.frame(mean_random_PB306_Vm_P7p_wt_mod_dataScale[c(1:75),])
    colnames(mean_random_PB306_Vm_P7p_wt_mod_dataScale_Line) <- c("P7p_wt_BLUP")
    #View(mean_random_PB306_Vm_P7p_wt_mod_dataScale_Line)
    
  }
  
  
  BLUP_PB306_Vm_P3_4_8p_SS_mod_dataScale <- cbind.data.frame(mean_random_PB306_Vm_P3p_SS_mod_dataScale_Line,mean_random_PB306_Vm_P4p_SS_mod_dataScale_Line,mean_random_PB306_Vm_P5p_wt_mod_dataScale_Line,mean_random_PB306_Vm_P6p_wt_mod_dataScale_Line,mean_random_PB306_Vm_P7p_wt_mod_dataScale_Line,mean_random_PB306_Vm_P8p_SS_mod_dataScale_Line)
  BLUP_PB306_Vm_P3_4_8p_SS_mod_dataScale$Ancestor <- rep("PB306", 75)
  BLUP_PB306_Vm_P3_4_8p_SS_mod_dataScale$Species <- rep("C.elegans", 75)
  BLUP_PB306_Vm_P3_4_8p_SS_mod_dataScale$Treatment <- c(rep("MA", 49), rep("CONTROL", 26))
  View(BLUP_PB306_Vm_P3_4_8p_SS_mod_dataScale)
  
  
}

#BLUP_AF16_Vm----
{
  #Extract BLUP of Lines AF16_Vm_P3p_SS
  {
    
    mean_random_AF16_Vm_P3p_SS_mod <- ranef(AF16_Vm_P3p_SS_mod, use = "mean") + mean(AF16_Vm_P3p_SS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_AF16_Vm_P3p_SS_mod)
    mean_random_AF16_Vm_P3p_SS_mod_dataScale <- pnorm(mean_random_AF16_Vm_P3p_SS_mod)
    #View(mean_random_AF16_Vm_P3p_SS_mod_dataScale)
    mean_random_AF16_Vm_P3p_SS_mod_dataScale_Line <- as.data.frame(mean_random_AF16_Vm_P3p_SS_mod_dataScale[c(1:74),])
    colnames(mean_random_AF16_Vm_P3p_SS_mod_dataScale_Line) <- c("P3p_SS_BLUP")
    #View(mean_random_AF16_Vm_P3p_SS_mod_dataScale_Line)
  }
  
  #Extract BLUP of Lines AF16_Vm_P4p_SS
  {
    
    mean_random_AF16_Vm_P4p_SS_mod <- ranef(AF16_Vm_P4p_SS_mod, use = "mean") + mean(AF16_Vm_P4p_SS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_AF16_Vm_P4p_SS_mod)
    mean_random_AF16_Vm_P4p_SS_mod_dataScale <- pnorm(mean_random_AF16_Vm_P4p_SS_mod)
    ##View(mean_random_AF16_Vm_P4p_SS_mod_dataScale)
    mean_random_AF16_Vm_P4p_SS_mod_dataScale_Line <- as.data.frame(mean_random_AF16_Vm_P4p_SS_mod_dataScale[c(1:74),])
    colnames(mean_random_AF16_Vm_P4p_SS_mod_dataScale_Line) <- c("P4p_SS_BLUP")
    #View(mean_random_AF16_Vm_P4p_SS_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines AF16_Vm_P8p_SS
  {
    
    mean_random_AF16_Vm_P8p_SS_mod <- ranef(AF16_Vm_P8p_SS_mod, use = "mean") + mean(AF16_Vm_P8p_SS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_AF16_Vm_P8p_SS_mod)
    mean_random_AF16_Vm_P8p_SS_mod_dataScale <- pnorm(mean_random_AF16_Vm_P8p_SS_mod)
    ##View(mean_random_AF16_Vm_P8p_SS_mod_dataScale)
    mean_random_AF16_Vm_P8p_SS_mod_dataScale_Line <- as.data.frame(mean_random_AF16_Vm_P8p_SS_mod_dataScale[c(1:74),])
    colnames(mean_random_AF16_Vm_P8p_SS_mod_dataScale_Line) <- c("P8p_SS_BLUP")
    #View(mean_random_AF16_Vm_P8p_SS_mod_dataScale_Line)
    
  }
  
  #Extract BLUP of Lines AF16_Vm_P5p_wt
  {
    
    
    mean_random_AF16_Vm_P5p_wt_mod <- ranef(AF16_Vm_P5p_wt_mod, use = "mean") + mean(AF16_Vm_P5p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_AF16_Vm_P5p_wt_mod)
    mean_random_AF16_Vm_P5p_wt_mod_dataScale <- pnorm(mean_random_AF16_Vm_P5p_wt_mod)
    ##View(mean_random_AF16_Vm_P5p_wt_mod_dataScale)
    mean_random_AF16_Vm_P5p_wt_mod_dataScale_Line <- as.data.frame(mean_random_AF16_Vm_P5p_wt_mod_dataScale[c(1:74),])
    colnames(mean_random_AF16_Vm_P5p_wt_mod_dataScale_Line) <- c("P5p_wt_BLUP")
    #View(mean_random_AF16_Vm_P5p_wt_mod_dataScale_Line)
  }
  
  #Extract BLUP of Lines AF16_Vm_P6p_wt
  {
    
    mean_random_AF16_Vm_P6p_wt_mod <- ranef(AF16_Vm_P6p_wt_mod, use = "mean") + mean(AF16_Vm_P6p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_AF16_Vm_P6p_wt_mod)
    mean_random_AF16_Vm_P6p_wt_mod_dataScale <- pnorm(mean_random_AF16_Vm_P6p_wt_mod)
    ##View(mean_random_AF16_Vm_P6p_wt_mod_dataScale)
    mean_random_AF16_Vm_P6p_wt_mod_dataScale_Line <- as.data.frame(mean_random_AF16_Vm_P6p_wt_mod_dataScale[c(1:74),])
    colnames(mean_random_AF16_Vm_P6p_wt_mod_dataScale_Line) <- c("P6p_wt_BLUP")
    #View(mean_random_AF16_Vm_P6p_wt_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines AF16_Vm_P7p_wt
  {
    
    mean_random_AF16_Vm_P7p_wt_mod <- ranef(AF16_Vm_P7p_wt_mod, use = "mean") + mean(AF16_Vm_P7p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_AF16_Vm_P7p_wt_mod)
    mean_random_AF16_Vm_P7p_wt_mod_dataScale <- pnorm(mean_random_AF16_Vm_P7p_wt_mod)
    ##View(mean_random_AF16_Vm_P7p_wt_mod_dataScale)
    mean_random_AF16_Vm_P7p_wt_mod_dataScale_Line <- as.data.frame(mean_random_AF16_Vm_P7p_wt_mod_dataScale[c(1:74),])
    colnames(mean_random_AF16_Vm_P7p_wt_mod_dataScale_Line) <- c("P7p_wt_BLUP")
    #View(mean_random_AF16_Vm_P7p_wt_mod_dataScale_Line)
    
  }
  
  
  BLUP_AF16_Vm_P3_4_8p_SS_mod_dataScale <- cbind.data.frame(mean_random_AF16_Vm_P3p_SS_mod_dataScale_Line,mean_random_AF16_Vm_P4p_SS_mod_dataScale_Line,mean_random_AF16_Vm_P5p_wt_mod_dataScale_Line,mean_random_AF16_Vm_P6p_wt_mod_dataScale_Line,mean_random_AF16_Vm_P7p_wt_mod_dataScale_Line,mean_random_AF16_Vm_P8p_SS_mod_dataScale_Line)
  BLUP_AF16_Vm_P3_4_8p_SS_mod_dataScale$Ancestor <- rep("AF16", 74)
  BLUP_AF16_Vm_P3_4_8p_SS_mod_dataScale$Species <- rep("C.briggsae", 74)
  BLUP_AF16_Vm_P3_4_8p_SS_mod_dataScale$Treatment <- c(rep("CONTROL", 24), rep("MA", 50))
  View(BLUP_AF16_Vm_P3_4_8p_SS_mod_dataScale)
  
  
}

#BLUP_PB800_Vm----
{
  #Extract BLUP of Lines PB800_Vm_P3p_SS
  {
    
    mean_random_PB800_Vm_P3p_SS_mod <- ranef(PB800_Vm_P3p_SS_mod, use = "mean") + mean(PB800_Vm_P3p_SS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_PB800_Vm_P3p_SS_mod)
    mean_random_PB800_Vm_P3p_SS_mod_dataScale <- pnorm(mean_random_PB800_Vm_P3p_SS_mod)
    #View(mean_random_PB800_Vm_P3p_SS_mod_dataScale)
    mean_random_PB800_Vm_P3p_SS_mod_dataScale_Line <- as.data.frame(mean_random_PB800_Vm_P3p_SS_mod_dataScale[c(1:72),])
    colnames(mean_random_PB800_Vm_P3p_SS_mod_dataScale_Line) <- c("P3p_SS_BLUP")
    View(mean_random_PB800_Vm_P3p_SS_mod_dataScale_Line)
  }
  
  #Extract BLUP of Lines PB800_Vm_P4p_SS
  {
    
    mean_random_PB800_Vm_P4p_SS_mod <- ranef(PB800_Vm_P4p_SS_mod, use = "mean") + mean(PB800_Vm_P4p_SS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_PB800_Vm_P4p_SS_mod)
    mean_random_PB800_Vm_P4p_SS_mod_dataScale <- pnorm(mean_random_PB800_Vm_P4p_SS_mod)
    ##View(mean_random_PB800_Vm_P4p_SS_mod_dataScale)
    mean_random_PB800_Vm_P4p_SS_mod_dataScale_Line <- as.data.frame(mean_random_PB800_Vm_P4p_SS_mod_dataScale[c(1:72),])
    colnames(mean_random_PB800_Vm_P4p_SS_mod_dataScale_Line) <- c("P4p_SS_BLUP")
    #View(mean_random_PB800_Vm_P4p_SS_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines PB800_Vm_P8p_SS
  {
    
    mean_random_PB800_Vm_P8p_SS_mod <- ranef(PB800_Vm_P8p_SS_mod, use = "mean") + mean(PB800_Vm_P8p_SS_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_PB800_Vm_P8p_SS_mod)
    mean_random_PB800_Vm_P8p_SS_mod_dataScale <- pnorm(mean_random_PB800_Vm_P8p_SS_mod)
    ##View(mean_random_PB800_Vm_P8p_SS_mod_dataScale)
    mean_random_PB800_Vm_P8p_SS_mod_dataScale_Line <- as.data.frame(mean_random_PB800_Vm_P8p_SS_mod_dataScale[c(1:72),])
    colnames(mean_random_PB800_Vm_P8p_SS_mod_dataScale_Line) <- c("P8p_SS_BLUP")
    #View(mean_random_PB800_Vm_P8p_SS_mod_dataScale_Line)
    
  }
  
  #Extract BLUP of Lines PB800_Vm_P5p_wt
  {
    
    mean_random_PB800_Vm_P5p_wt_mod <- ranef(PB800_Vm_P5p_wt_mod, use = "mean") + mean(PB800_Vm_P5p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_PB800_Vm_P5p_wt_mod)
    mean_random_PB800_Vm_P5p_wt_mod_dataScale <- pnorm(mean_random_PB800_Vm_P5p_wt_mod)
    ##View(mean_random_PB800_Vm_P5p_wt_mod_dataScale)
    mean_random_PB800_Vm_P5p_wt_mod_dataScale_Line <- as.data.frame(mean_random_PB800_Vm_P5p_wt_mod_dataScale[c(1:72),])
    colnames(mean_random_PB800_Vm_P5p_wt_mod_dataScale_Line) <- c("P5p_wt_BLUP")
    #View(mean_random_PB800_Vm_P5p_wt_mod_dataScale_Line)
  }
  
  #Extract BLUP of Lines PB800_Vm_P6p_wt
  {
    
    mean_random_PB800_Vm_P6p_wt_mod <- ranef(PB800_Vm_P6p_wt_mod, use = "mean") + mean(PB800_Vm_P6p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_PB800_Vm_P6p_wt_mod)
    mean_random_PB800_Vm_P6p_wt_mod_dataScale <- pnorm(mean_random_PB800_Vm_P6p_wt_mod)
    ##View(mean_random_PB800_Vm_P6p_wt_mod_dataScale)
    mean_random_PB800_Vm_P6p_wt_mod_dataScale_Line <- as.data.frame(mean_random_PB800_Vm_P6p_wt_mod_dataScale[c(1:72),])
    colnames(mean_random_PB800_Vm_P6p_wt_mod_dataScale_Line) <- c("P6p_wt_BLUP")
    #View(mean_random_PB800_Vm_P6p_wt_mod_dataScale_Line)
  }
  
  
  #Extract BLUP of Lines PB800_Vm_P7p_wt
  {
    
    mean_random_PB800_Vm_P7p_wt_mod <- ranef(PB800_Vm_P7p_wt_mod, use = "mean") + mean(PB800_Vm_P7p_wt_mod[["Sol"]][,c(1:4)])
    ##View(mean_random_PB800_Vm_P7p_wt_mod)
    mean_random_PB800_Vm_P7p_wt_mod_dataScale <- pnorm(mean_random_PB800_Vm_P7p_wt_mod)
    ##View(mean_random_PB800_Vm_P7p_wt_mod_dataScale)
    mean_random_PB800_Vm_P7p_wt_mod_dataScale_Line <- as.data.frame(mean_random_PB800_Vm_P7p_wt_mod_dataScale[c(1:72),])
    colnames(mean_random_PB800_Vm_P7p_wt_mod_dataScale_Line) <- c("P7p_wt_BLUP")
    #View(mean_random_PB800_Vm_P7p_wt_mod_dataScale_Line)
    
  }
  
  
  BLUP_PB800_Vm_P3_4_8p_SS_mod_dataScale <- cbind.data.frame(mean_random_PB800_Vm_P3p_SS_mod_dataScale_Line,mean_random_PB800_Vm_P4p_SS_mod_dataScale_Line,mean_random_PB800_Vm_P5p_wt_mod_dataScale_Line,mean_random_PB800_Vm_P6p_wt_mod_dataScale_Line,mean_random_PB800_Vm_P7p_wt_mod_dataScale_Line,mean_random_PB800_Vm_P8p_SS_mod_dataScale_Line)
  BLUP_PB800_Vm_P3_4_8p_SS_mod_dataScale$Ancestor <- rep("PB800", 72)
  BLUP_PB800_Vm_P3_4_8p_SS_mod_dataScale$Species <- rep("C.briggsae", 72)
  BLUP_PB800_Vm_P3_4_8p_SS_mod_dataScale$Treatment <- c(rep("MA", 50), rep("CONTROL", 22))
  
  View(BLUP_PB800_Vm_P3_4_8p_SS_mod_dataScale)
  
  
}


BLUP_Caenorhabditis_Vm_dataScale_SS <- rbind.data.frame(BLUP_JU1200_Vm_P3_4_8p_SS_mod_dataScale, BLUP_PB306_Vm_P3_4_8p_SS_mod_dataScale,BLUP_AF16_Vm_P3_4_8p_SS_mod_dataScale,BLUP_PB800_Vm_P3_4_8p_SS_mod_dataScale)
BLUP_Caenorhabditis_Vm_dataScale_SS <- cbind(Lines = rownames(BLUP_Caenorhabditis_Vm_mod_dataScale_SS),BLUP_Caenorhabditis_Vm_mod_dataScale_SS)
BLUP_Caenorhabditis_Vm_dataScale_SS <- BLUP_Caenorhabditis_Vm_mod_dataScale_SS %>%  mutate(Lines = str_remove_all(Lines, "LineB."))
BLUP_Caenorhabditis_Vm_dataScale_SS <- BLUP_Caenorhabditis_Vm_mod_dataScale_SS %>% mutate(Treatment= str_replace(Treatment,'CONTROL', 'Ancestral'))
rownames(BLUP_Caenorhabditis_Vm_dataScale_SS) <- NULL
View(BLUP_Caenorhabditis_Vm_dataScale_SS)
write_xlsx(BLUP_Caenorhabditis_Vm_dataScale_SS, "BLUP_Caenorhabditis_Vm_SS_wt_mod_dataScale.xlsx")



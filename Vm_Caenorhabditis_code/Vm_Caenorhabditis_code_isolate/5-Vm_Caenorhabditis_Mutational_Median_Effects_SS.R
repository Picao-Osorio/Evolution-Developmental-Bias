#Mutational Median effects - Mutational Bias_SS
library(emmeans)

#JU1200_Vm_ Mutational Median effects - Mutational Bias ----
{
  ##JU1200_Vm_P3p_SS Mutation Median effects - Mutational Bias----
  {
    JU1200_Vm_P3p_SS_mod_e_Treatment<- emmeans(JU1200_Vm_P3p_SS_mod, ~Treatment, data=Vm_JU1200_data)
    JU1200_Vm_P3p_SS_mod_e_Treatment_hpd0.95 <- as.data.frame(JU1200_Vm_P3p_SS_mod_e_Treatment)
    colnames(JU1200_Vm_P3p_SS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU1200_Vm_P3p_SS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU1200_Vm_P3p_SS_mod_e_Treatment, prob = 0.83))
    colnames(JU1200_Vm_P3p_SS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU1200_Vm_P3p_SS_mod_Treatment_liab <- cbind.data.frame(JU1200_Vm_P3p_SS_mod_e_Treatment_hpd0.95,JU1200_Vm_P3p_SS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU1200_Vm_P3p_SS_mod_Treatment_liab$Ancestor <- c("JU1200","JU1200")
    JU1200_Vm_P3p_SS_mod_Treatment_liab$Pnp <- c("P3.p","P3.p")
    JU1200_Vm_P3p_SS_mod_Treatment_liab$Pnp_fate <- c("SS","SS")
    JU1200_Vm_P3p_SS_mod_Treatment_liab$Scale <- c("liab","liab")
    JU1200_Vm_P3p_SS_mod_Treatment_liab
    
    JU1200_Vm_P3p_SS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU1200_Vm_P3p_SS_mod_Treatment_liab[,2]))
    colnames(JU1200_Vm_P3p_SS_mod_Treatment_DataScale_median) <- c("median")
    JU1200_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU1200_Vm_P3p_SS_mod_Treatment_liab[,3]))
    colnames(JU1200_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU1200_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU1200_Vm_P3p_SS_mod_Treatment_liab[,4]))
    colnames(JU1200_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU1200_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU1200_Vm_P3p_SS_mod_Treatment_liab[,5]))
    colnames(JU1200_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU1200_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU1200_Vm_P3p_SS_mod_Treatment_liab[,6]))
    colnames(JU1200_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU1200_Vm_P3p_SS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU1200_Vm_P3p_SS_mod_Treatment_DataScale) <- c("Treatment")
    JU1200_Vm_P3p_SS_mod_Treatment_DataScale <- cbind.data.frame(JU1200_Vm_P3p_SS_mod_Treatment_DataScale,JU1200_Vm_P3p_SS_mod_Treatment_DataScale_median,JU1200_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.95,JU1200_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.95,JU1200_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.83,JU1200_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU1200_Vm_P3p_SS_mod_Treatment_DataScale$Ancestor <- c("JU1200","JU1200")
    JU1200_Vm_P3p_SS_mod_Treatment_DataScale$Pnp <- c("P3.p","P3.p")
    JU1200_Vm_P3p_SS_mod_Treatment_DataScale$Pnp_fate <- c("SS","SS")
    JU1200_Vm_P3p_SS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU1200_Vm_P3p_SS_mod_Treatment_liab_DataScale <- rbind.data.frame(JU1200_Vm_P3p_SS_mod_Treatment_liab, JU1200_Vm_P3p_SS_mod_Treatment_DataScale)
    JU1200_Vm_P3p_SS_mod_Treatment_liab_DataScale
    
  }
  
  ##JU1200_Vm_P4p_SS Mutation Median effects - Mutational Bias----
  {
    JU1200_Vm_P4p_SS_mod_e_Treatment<- emmeans(JU1200_Vm_P4p_SS_mod, ~Treatment, data=Vm_JU1200_data)
    JU1200_Vm_P4p_SS_mod_e_Treatment_hpd0.95 <- as.data.frame(JU1200_Vm_P4p_SS_mod_e_Treatment)
    colnames(JU1200_Vm_P4p_SS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU1200_Vm_P4p_SS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU1200_Vm_P4p_SS_mod_e_Treatment, prob = 0.83))
    colnames(JU1200_Vm_P4p_SS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU1200_Vm_P4p_SS_mod_Treatment_liab <- cbind.data.frame(JU1200_Vm_P4p_SS_mod_e_Treatment_hpd0.95,JU1200_Vm_P4p_SS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU1200_Vm_P4p_SS_mod_Treatment_liab$Ancestor <- c("JU1200","JU1200")
    JU1200_Vm_P4p_SS_mod_Treatment_liab$Pnp <- c("P4.p","P4.p")
    JU1200_Vm_P4p_SS_mod_Treatment_liab$Pnp_fate <- c("SS","SS")
    JU1200_Vm_P4p_SS_mod_Treatment_liab$Scale <- c("liab","liab")
    JU1200_Vm_P4p_SS_mod_Treatment_liab
    
    JU1200_Vm_P4p_SS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU1200_Vm_P4p_SS_mod_Treatment_liab[,2]))
    colnames(JU1200_Vm_P4p_SS_mod_Treatment_DataScale_median) <- c("median")
    JU1200_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU1200_Vm_P4p_SS_mod_Treatment_liab[,3]))
    colnames(JU1200_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU1200_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU1200_Vm_P4p_SS_mod_Treatment_liab[,4]))
    colnames(JU1200_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU1200_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU1200_Vm_P4p_SS_mod_Treatment_liab[,5]))
    colnames(JU1200_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU1200_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU1200_Vm_P4p_SS_mod_Treatment_liab[,6]))
    colnames(JU1200_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU1200_Vm_P4p_SS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU1200_Vm_P4p_SS_mod_Treatment_DataScale) <- c("Treatment")
    JU1200_Vm_P4p_SS_mod_Treatment_DataScale <- cbind.data.frame(JU1200_Vm_P4p_SS_mod_Treatment_DataScale,JU1200_Vm_P4p_SS_mod_Treatment_DataScale_median,JU1200_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.95,JU1200_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.95,JU1200_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.83,JU1200_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU1200_Vm_P4p_SS_mod_Treatment_DataScale$Ancestor <- c("JU1200","JU1200")
    JU1200_Vm_P4p_SS_mod_Treatment_DataScale$Pnp <- c("P4.p","P4.p")
    JU1200_Vm_P4p_SS_mod_Treatment_DataScale$Pnp_fate <- c("SS","SS")
    JU1200_Vm_P4p_SS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU1200_Vm_P4p_SS_mod_Treatment_liab_DataScale <- rbind.data.frame(JU1200_Vm_P4p_SS_mod_Treatment_liab, JU1200_Vm_P4p_SS_mod_Treatment_DataScale)
    JU1200_Vm_P4p_SS_mod_Treatment_liab_DataScale
    
  }
  
  ##JU1200_Vm_P8p_SS Mutation Median effects - Mutational Bias----
  {
    JU1200_Vm_P8p_SS_mod_e_Treatment<- emmeans(JU1200_Vm_P8p_SS_mod, ~Treatment, data=Vm_JU1200_data)
    JU1200_Vm_P8p_SS_mod_e_Treatment_hpd0.95 <- as.data.frame(JU1200_Vm_P8p_SS_mod_e_Treatment)
    colnames(JU1200_Vm_P8p_SS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU1200_Vm_P8p_SS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU1200_Vm_P8p_SS_mod_e_Treatment, prob = 0.83))
    colnames(JU1200_Vm_P8p_SS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU1200_Vm_P8p_SS_mod_Treatment_liab <- cbind.data.frame(JU1200_Vm_P8p_SS_mod_e_Treatment_hpd0.95,JU1200_Vm_P8p_SS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU1200_Vm_P8p_SS_mod_Treatment_liab$Ancestor <- c("JU1200","JU1200")
    JU1200_Vm_P8p_SS_mod_Treatment_liab$Pnp <- c("P8.p","P8.p")
    JU1200_Vm_P8p_SS_mod_Treatment_liab$Pnp_fate <- c("SS","SS")
    JU1200_Vm_P8p_SS_mod_Treatment_liab$Scale <- c("liab","liab")
    JU1200_Vm_P8p_SS_mod_Treatment_liab
    
    JU1200_Vm_P8p_SS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU1200_Vm_P8p_SS_mod_Treatment_liab[,2]))
    colnames(JU1200_Vm_P8p_SS_mod_Treatment_DataScale_median) <- c("median")
    JU1200_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU1200_Vm_P8p_SS_mod_Treatment_liab[,3]))
    colnames(JU1200_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU1200_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU1200_Vm_P8p_SS_mod_Treatment_liab[,4]))
    colnames(JU1200_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU1200_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU1200_Vm_P8p_SS_mod_Treatment_liab[,5]))
    colnames(JU1200_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU1200_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU1200_Vm_P8p_SS_mod_Treatment_liab[,6]))
    colnames(JU1200_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU1200_Vm_P8p_SS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU1200_Vm_P8p_SS_mod_Treatment_DataScale) <- c("Treatment")
    JU1200_Vm_P8p_SS_mod_Treatment_DataScale <- cbind.data.frame(JU1200_Vm_P8p_SS_mod_Treatment_DataScale,JU1200_Vm_P8p_SS_mod_Treatment_DataScale_median,JU1200_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.95,JU1200_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.95,JU1200_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.83,JU1200_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU1200_Vm_P8p_SS_mod_Treatment_DataScale$Ancestor <- c("JU1200","JU1200")
    JU1200_Vm_P8p_SS_mod_Treatment_DataScale$Pnp <- c("P8.p","P8.p")
    JU1200_Vm_P8p_SS_mod_Treatment_DataScale$Pnp_fate <- c("SS","SS")
    JU1200_Vm_P8p_SS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU1200_Vm_P8p_SS_mod_Treatment_liab_DataScale <- rbind.data.frame(JU1200_Vm_P8p_SS_mod_Treatment_liab, JU1200_Vm_P8p_SS_mod_Treatment_DataScale)
    JU1200_Vm_P8p_SS_mod_Treatment_liab_DataScale
    
  }
  
  ##JU1200_Vm_P5p_wt Mutation Median effects - Mutational Bias----
  {
    JU1200_Vm_P5p_wt_mod_e_Treatment<- emmeans(JU1200_Vm_P5p_wt_mod, ~Treatment, data=Vm_JU1200_data)
    JU1200_Vm_P5p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(JU1200_Vm_P5p_wt_mod_e_Treatment)
    colnames(JU1200_Vm_P5p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU1200_Vm_P5p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU1200_Vm_P5p_wt_mod_e_Treatment, prob = 0.83))
    colnames(JU1200_Vm_P5p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU1200_Vm_P5p_wt_mod_Treatment_liab <- cbind.data.frame(JU1200_Vm_P5p_wt_mod_e_Treatment_hpd0.95,JU1200_Vm_P5p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU1200_Vm_P5p_wt_mod_Treatment_liab$Ancestor <- c("JU1200","JU1200")
    JU1200_Vm_P5p_wt_mod_Treatment_liab$Pnp <- c("P5.p","P5.p")
    JU1200_Vm_P5p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    JU1200_Vm_P5p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    JU1200_Vm_P5p_wt_mod_Treatment_liab
    
    JU1200_Vm_P5p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU1200_Vm_P5p_wt_mod_Treatment_liab[,2]))
    colnames(JU1200_Vm_P5p_wt_mod_Treatment_DataScale_median) <- c("median")
    JU1200_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU1200_Vm_P5p_wt_mod_Treatment_liab[,3]))
    colnames(JU1200_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU1200_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU1200_Vm_P5p_wt_mod_Treatment_liab[,4]))
    colnames(JU1200_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU1200_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU1200_Vm_P5p_wt_mod_Treatment_liab[,5]))
    colnames(JU1200_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU1200_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU1200_Vm_P5p_wt_mod_Treatment_liab[,6]))
    colnames(JU1200_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU1200_Vm_P5p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU1200_Vm_P5p_wt_mod_Treatment_DataScale) <- c("Treatment")
    JU1200_Vm_P5p_wt_mod_Treatment_DataScale <- cbind.data.frame(JU1200_Vm_P5p_wt_mod_Treatment_DataScale,JU1200_Vm_P5p_wt_mod_Treatment_DataScale_median,JU1200_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95,JU1200_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95,JU1200_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83,JU1200_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU1200_Vm_P5p_wt_mod_Treatment_DataScale$Ancestor <- c("JU1200","JU1200")
    JU1200_Vm_P5p_wt_mod_Treatment_DataScale$Pnp <- c("P5.p","P5.p")
    JU1200_Vm_P5p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    JU1200_Vm_P5p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU1200_Vm_P5p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(JU1200_Vm_P5p_wt_mod_Treatment_liab, JU1200_Vm_P5p_wt_mod_Treatment_DataScale)
    JU1200_Vm_P5p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##JU1200_Vm_P6p_wt Mutation Median effects - Mutational Bias----
  {
    JU1200_Vm_P6p_wt_mod_e_Treatment<- emmeans(JU1200_Vm_P6p_wt_mod, ~Treatment, data=Vm_JU1200_data)
    JU1200_Vm_P6p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(JU1200_Vm_P6p_wt_mod_e_Treatment)
    colnames(JU1200_Vm_P6p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU1200_Vm_P6p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU1200_Vm_P6p_wt_mod_e_Treatment, prob = 0.83))
    colnames(JU1200_Vm_P6p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU1200_Vm_P6p_wt_mod_Treatment_liab <- cbind.data.frame(JU1200_Vm_P6p_wt_mod_e_Treatment_hpd0.95,JU1200_Vm_P6p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU1200_Vm_P6p_wt_mod_Treatment_liab$Ancestor <- c("JU1200","JU1200")
    JU1200_Vm_P6p_wt_mod_Treatment_liab$Pnp <- c("P6.p","P6.p")
    JU1200_Vm_P6p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    JU1200_Vm_P6p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    JU1200_Vm_P6p_wt_mod_Treatment_liab
    
    JU1200_Vm_P6p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU1200_Vm_P6p_wt_mod_Treatment_liab[,2]))
    colnames(JU1200_Vm_P6p_wt_mod_Treatment_DataScale_median) <- c("median")
    JU1200_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU1200_Vm_P6p_wt_mod_Treatment_liab[,3]))
    colnames(JU1200_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU1200_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU1200_Vm_P6p_wt_mod_Treatment_liab[,4]))
    colnames(JU1200_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU1200_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU1200_Vm_P6p_wt_mod_Treatment_liab[,5]))
    colnames(JU1200_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU1200_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU1200_Vm_P6p_wt_mod_Treatment_liab[,6]))
    colnames(JU1200_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU1200_Vm_P6p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU1200_Vm_P6p_wt_mod_Treatment_DataScale) <- c("Treatment")
    JU1200_Vm_P6p_wt_mod_Treatment_DataScale <- cbind.data.frame(JU1200_Vm_P6p_wt_mod_Treatment_DataScale,JU1200_Vm_P6p_wt_mod_Treatment_DataScale_median,JU1200_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95,JU1200_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95,JU1200_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83,JU1200_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU1200_Vm_P6p_wt_mod_Treatment_DataScale$Ancestor <- c("JU1200","JU1200")
    JU1200_Vm_P6p_wt_mod_Treatment_DataScale$Pnp <- c("P6.p","P6.p")
    JU1200_Vm_P6p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    JU1200_Vm_P6p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU1200_Vm_P6p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(JU1200_Vm_P6p_wt_mod_Treatment_liab, JU1200_Vm_P6p_wt_mod_Treatment_DataScale)
    JU1200_Vm_P6p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##JU1200_Vm_P7p_wt Mutation Median effects - Mutational Bias----
  {
    JU1200_Vm_P7p_wt_mod_e_Treatment<- emmeans(JU1200_Vm_P7p_wt_mod, ~Treatment, data=Vm_JU1200_data)
    JU1200_Vm_P7p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(JU1200_Vm_P7p_wt_mod_e_Treatment)
    colnames(JU1200_Vm_P7p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU1200_Vm_P7p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU1200_Vm_P7p_wt_mod_e_Treatment, prob = 0.83))
    colnames(JU1200_Vm_P7p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU1200_Vm_P7p_wt_mod_Treatment_liab <- cbind.data.frame(JU1200_Vm_P7p_wt_mod_e_Treatment_hpd0.95,JU1200_Vm_P7p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU1200_Vm_P7p_wt_mod_Treatment_liab$Ancestor <- c("JU1200","JU1200")
    JU1200_Vm_P7p_wt_mod_Treatment_liab$Pnp <- c("P7.p","P7.p")
    JU1200_Vm_P7p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    JU1200_Vm_P7p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    JU1200_Vm_P7p_wt_mod_Treatment_liab
    
    JU1200_Vm_P7p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU1200_Vm_P7p_wt_mod_Treatment_liab[,2]))
    colnames(JU1200_Vm_P7p_wt_mod_Treatment_DataScale_median) <- c("median")
    JU1200_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU1200_Vm_P7p_wt_mod_Treatment_liab[,3]))
    colnames(JU1200_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU1200_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU1200_Vm_P7p_wt_mod_Treatment_liab[,4]))
    colnames(JU1200_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU1200_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU1200_Vm_P7p_wt_mod_Treatment_liab[,5]))
    colnames(JU1200_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU1200_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU1200_Vm_P7p_wt_mod_Treatment_liab[,6]))
    colnames(JU1200_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU1200_Vm_P7p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU1200_Vm_P7p_wt_mod_Treatment_DataScale) <- c("Treatment")
    JU1200_Vm_P7p_wt_mod_Treatment_DataScale <- cbind.data.frame(JU1200_Vm_P7p_wt_mod_Treatment_DataScale,JU1200_Vm_P7p_wt_mod_Treatment_DataScale_median,JU1200_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95,JU1200_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95,JU1200_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83,JU1200_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU1200_Vm_P7p_wt_mod_Treatment_DataScale$Ancestor <- c("JU1200","JU1200")
    JU1200_Vm_P7p_wt_mod_Treatment_DataScale$Pnp <- c("P7.p","P7.p")
    JU1200_Vm_P7p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    JU1200_Vm_P7p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU1200_Vm_P7p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(JU1200_Vm_P7p_wt_mod_Treatment_liab, JU1200_Vm_P7p_wt_mod_Treatment_DataScale)
    JU1200_Vm_P7p_wt_mod_Treatment_liab_DataScale
    
  }
  ##----
  JU1200_Vm_mod_Treatment_liab_DataScale <- rbind.data.frame(JU1200_Vm_P3p_SS_mod_Treatment_liab_DataScale,JU1200_Vm_P4p_SS_mod_Treatment_liab_DataScale,JU1200_Vm_P5p_wt_mod_Treatment_liab_DataScale,JU1200_Vm_P6p_wt_mod_Treatment_liab_DataScale,JU1200_Vm_P7p_wt_mod_Treatment_liab_DataScale, JU1200_Vm_P8p_SS_mod_Treatment_liab_DataScale)
  JU1200_Vm_mod_Treatment_liab_DataScale$Species <- rep("C.elegans",24)
  JU1200_Vm_mod_Treatment_liab_DataScale$Genus <- rep("Caenorhabditis",24)
  
  View(JU1200_Vm_mod_Treatment_liab_DataScale)
  
}


#PB306_Vm_ Mutational Median effects - Mutational Bias ----
{
  ##PB306_Vm_P3p_SS Mutation Median effects - Mutational Bias----
  {
    PB306_Vm_P3p_SS_mod_e_Treatment<- emmeans(PB306_Vm_P3p_SS_mod, ~Treatment, data=Vm_PB306_data)
    PB306_Vm_P3p_SS_mod_e_Treatment_hpd0.95 <- as.data.frame(PB306_Vm_P3p_SS_mod_e_Treatment)
    colnames(PB306_Vm_P3p_SS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PB306_Vm_P3p_SS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PB306_Vm_P3p_SS_mod_e_Treatment, prob = 0.83))
    colnames(PB306_Vm_P3p_SS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PB306_Vm_P3p_SS_mod_Treatment_liab <- cbind.data.frame(PB306_Vm_P3p_SS_mod_e_Treatment_hpd0.95,PB306_Vm_P3p_SS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PB306_Vm_P3p_SS_mod_Treatment_liab$Ancestor <- c("PB306","PB306")
    PB306_Vm_P3p_SS_mod_Treatment_liab$Pnp <- c("P3.p","P3.p")
    PB306_Vm_P3p_SS_mod_Treatment_liab$Pnp_fate <- c("SS","SS")
    PB306_Vm_P3p_SS_mod_Treatment_liab$Scale <- c("liab","liab")
    PB306_Vm_P3p_SS_mod_Treatment_liab
    
    PB306_Vm_P3p_SS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PB306_Vm_P3p_SS_mod_Treatment_liab[,2]))
    colnames(PB306_Vm_P3p_SS_mod_Treatment_DataScale_median) <- c("median")
    PB306_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PB306_Vm_P3p_SS_mod_Treatment_liab[,3]))
    colnames(PB306_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PB306_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PB306_Vm_P3p_SS_mod_Treatment_liab[,4]))
    colnames(PB306_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PB306_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PB306_Vm_P3p_SS_mod_Treatment_liab[,5]))
    colnames(PB306_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PB306_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PB306_Vm_P3p_SS_mod_Treatment_liab[,6]))
    colnames(PB306_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PB306_Vm_P3p_SS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PB306_Vm_P3p_SS_mod_Treatment_DataScale) <- c("Treatment")
    PB306_Vm_P3p_SS_mod_Treatment_DataScale <- cbind.data.frame(PB306_Vm_P3p_SS_mod_Treatment_DataScale,PB306_Vm_P3p_SS_mod_Treatment_DataScale_median,PB306_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.95,PB306_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.95,PB306_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.83,PB306_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.83 )
    PB306_Vm_P3p_SS_mod_Treatment_DataScale$Ancestor <- c("PB306","PB306")
    PB306_Vm_P3p_SS_mod_Treatment_DataScale$Pnp <- c("P3.p","P3.p")
    PB306_Vm_P3p_SS_mod_Treatment_DataScale$Pnp_fate <- c("SS","SS")
    PB306_Vm_P3p_SS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PB306_Vm_P3p_SS_mod_Treatment_liab_DataScale <- rbind.data.frame(PB306_Vm_P3p_SS_mod_Treatment_liab, PB306_Vm_P3p_SS_mod_Treatment_DataScale)
    PB306_Vm_P3p_SS_mod_Treatment_liab_DataScale
    
  }
  
  ##PB306_Vm_P4p_SS Mutation Median effects - Mutational Bias----
  {
    PB306_Vm_P4p_SS_mod_e_Treatment<- emmeans(PB306_Vm_P4p_SS_mod, ~Treatment, data=Vm_PB306_data)
    PB306_Vm_P4p_SS_mod_e_Treatment_hpd0.95 <- as.data.frame(PB306_Vm_P4p_SS_mod_e_Treatment)
    colnames(PB306_Vm_P4p_SS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PB306_Vm_P4p_SS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PB306_Vm_P4p_SS_mod_e_Treatment, prob = 0.83))
    colnames(PB306_Vm_P4p_SS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PB306_Vm_P4p_SS_mod_Treatment_liab <- cbind.data.frame(PB306_Vm_P4p_SS_mod_e_Treatment_hpd0.95,PB306_Vm_P4p_SS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PB306_Vm_P4p_SS_mod_Treatment_liab$Ancestor <- c("PB306","PB306")
    PB306_Vm_P4p_SS_mod_Treatment_liab$Pnp <- c("P4.p","P4.p")
    PB306_Vm_P4p_SS_mod_Treatment_liab$Pnp_fate <- c("SS","SS")
    PB306_Vm_P4p_SS_mod_Treatment_liab$Scale <- c("liab","liab")
    PB306_Vm_P4p_SS_mod_Treatment_liab
    
    PB306_Vm_P4p_SS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PB306_Vm_P4p_SS_mod_Treatment_liab[,2]))
    colnames(PB306_Vm_P4p_SS_mod_Treatment_DataScale_median) <- c("median")
    PB306_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PB306_Vm_P4p_SS_mod_Treatment_liab[,3]))
    colnames(PB306_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PB306_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PB306_Vm_P4p_SS_mod_Treatment_liab[,4]))
    colnames(PB306_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PB306_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PB306_Vm_P4p_SS_mod_Treatment_liab[,5]))
    colnames(PB306_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PB306_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PB306_Vm_P4p_SS_mod_Treatment_liab[,6]))
    colnames(PB306_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PB306_Vm_P4p_SS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PB306_Vm_P4p_SS_mod_Treatment_DataScale) <- c("Treatment")
    PB306_Vm_P4p_SS_mod_Treatment_DataScale <- cbind.data.frame(PB306_Vm_P4p_SS_mod_Treatment_DataScale,PB306_Vm_P4p_SS_mod_Treatment_DataScale_median,PB306_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.95,PB306_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.95,PB306_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.83,PB306_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.83 )
    PB306_Vm_P4p_SS_mod_Treatment_DataScale$Ancestor <- c("PB306","PB306")
    PB306_Vm_P4p_SS_mod_Treatment_DataScale$Pnp <- c("P4.p","P4.p")
    PB306_Vm_P4p_SS_mod_Treatment_DataScale$Pnp_fate <- c("SS","SS")
    PB306_Vm_P4p_SS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PB306_Vm_P4p_SS_mod_Treatment_liab_DataScale <- rbind.data.frame(PB306_Vm_P4p_SS_mod_Treatment_liab, PB306_Vm_P4p_SS_mod_Treatment_DataScale)
    PB306_Vm_P4p_SS_mod_Treatment_liab_DataScale
    
  }
  
  ##PB306_Vm_P8p_SS Mutation Median effects - Mutational Bias----
  {
    PB306_Vm_P8p_SS_mod_e_Treatment<- emmeans(PB306_Vm_P8p_SS_mod, ~Treatment, data=Vm_PB306_data)
    PB306_Vm_P8p_SS_mod_e_Treatment_hpd0.95 <- as.data.frame(PB306_Vm_P8p_SS_mod_e_Treatment)
    colnames(PB306_Vm_P8p_SS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PB306_Vm_P8p_SS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PB306_Vm_P8p_SS_mod_e_Treatment, prob = 0.83))
    colnames(PB306_Vm_P8p_SS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PB306_Vm_P8p_SS_mod_Treatment_liab <- cbind.data.frame(PB306_Vm_P8p_SS_mod_e_Treatment_hpd0.95,PB306_Vm_P8p_SS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PB306_Vm_P8p_SS_mod_Treatment_liab$Ancestor <- c("PB306","PB306")
    PB306_Vm_P8p_SS_mod_Treatment_liab$Pnp <- c("P8.p","P8.p")
    PB306_Vm_P8p_SS_mod_Treatment_liab$Pnp_fate <- c("SS","SS")
    PB306_Vm_P8p_SS_mod_Treatment_liab$Scale <- c("liab","liab")
    PB306_Vm_P8p_SS_mod_Treatment_liab
    
    PB306_Vm_P8p_SS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PB306_Vm_P8p_SS_mod_Treatment_liab[,2]))
    colnames(PB306_Vm_P8p_SS_mod_Treatment_DataScale_median) <- c("median")
    PB306_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PB306_Vm_P8p_SS_mod_Treatment_liab[,3]))
    colnames(PB306_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PB306_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PB306_Vm_P8p_SS_mod_Treatment_liab[,4]))
    colnames(PB306_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PB306_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PB306_Vm_P8p_SS_mod_Treatment_liab[,5]))
    colnames(PB306_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PB306_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PB306_Vm_P8p_SS_mod_Treatment_liab[,6]))
    colnames(PB306_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PB306_Vm_P8p_SS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PB306_Vm_P8p_SS_mod_Treatment_DataScale) <- c("Treatment")
    PB306_Vm_P8p_SS_mod_Treatment_DataScale <- cbind.data.frame(PB306_Vm_P8p_SS_mod_Treatment_DataScale,PB306_Vm_P8p_SS_mod_Treatment_DataScale_median,PB306_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.95,PB306_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.95,PB306_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.83,PB306_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.83 )
    PB306_Vm_P8p_SS_mod_Treatment_DataScale$Ancestor <- c("PB306","PB306")
    PB306_Vm_P8p_SS_mod_Treatment_DataScale$Pnp <- c("P8.p","P8.p")
    PB306_Vm_P8p_SS_mod_Treatment_DataScale$Pnp_fate <- c("SS","SS")
    PB306_Vm_P8p_SS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PB306_Vm_P8p_SS_mod_Treatment_liab_DataScale <- rbind.data.frame(PB306_Vm_P8p_SS_mod_Treatment_liab, PB306_Vm_P8p_SS_mod_Treatment_DataScale)
    PB306_Vm_P8p_SS_mod_Treatment_liab_DataScale
    
  }
  
  ##PB306_Vm_P5p_wt Mutation Median effects - Mutational Bias----
  {
    PB306_Vm_P5p_wt_mod_e_Treatment<- emmeans(PB306_Vm_P5p_wt_mod, ~Treatment, data=Vm_PB306_data)
    PB306_Vm_P5p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(PB306_Vm_P5p_wt_mod_e_Treatment)
    colnames(PB306_Vm_P5p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PB306_Vm_P5p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PB306_Vm_P5p_wt_mod_e_Treatment, prob = 0.83))
    colnames(PB306_Vm_P5p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PB306_Vm_P5p_wt_mod_Treatment_liab <- cbind.data.frame(PB306_Vm_P5p_wt_mod_e_Treatment_hpd0.95,PB306_Vm_P5p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PB306_Vm_P5p_wt_mod_Treatment_liab$Ancestor <- c("PB306","PB306")
    PB306_Vm_P5p_wt_mod_Treatment_liab$Pnp <- c("P5.p","P5.p")
    PB306_Vm_P5p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    PB306_Vm_P5p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    PB306_Vm_P5p_wt_mod_Treatment_liab
    
    PB306_Vm_P5p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PB306_Vm_P5p_wt_mod_Treatment_liab[,2]))
    colnames(PB306_Vm_P5p_wt_mod_Treatment_DataScale_median) <- c("median")
    PB306_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PB306_Vm_P5p_wt_mod_Treatment_liab[,3]))
    colnames(PB306_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PB306_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PB306_Vm_P5p_wt_mod_Treatment_liab[,4]))
    colnames(PB306_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PB306_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PB306_Vm_P5p_wt_mod_Treatment_liab[,5]))
    colnames(PB306_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PB306_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PB306_Vm_P5p_wt_mod_Treatment_liab[,6]))
    colnames(PB306_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PB306_Vm_P5p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PB306_Vm_P5p_wt_mod_Treatment_DataScale) <- c("Treatment")
    PB306_Vm_P5p_wt_mod_Treatment_DataScale <- cbind.data.frame(PB306_Vm_P5p_wt_mod_Treatment_DataScale,PB306_Vm_P5p_wt_mod_Treatment_DataScale_median,PB306_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95,PB306_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95,PB306_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83,PB306_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    PB306_Vm_P5p_wt_mod_Treatment_DataScale$Ancestor <- c("PB306","PB306")
    PB306_Vm_P5p_wt_mod_Treatment_DataScale$Pnp <- c("P5.p","P5.p")
    PB306_Vm_P5p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    PB306_Vm_P5p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PB306_Vm_P5p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(PB306_Vm_P5p_wt_mod_Treatment_liab, PB306_Vm_P5p_wt_mod_Treatment_DataScale)
    PB306_Vm_P5p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##PB306_Vm_P6p_wt Mutation Median effects - Mutational Bias----
  {
    PB306_Vm_P6p_wt_mod_e_Treatment<- emmeans(PB306_Vm_P6p_wt_mod, ~Treatment, data=Vm_PB306_data)
    PB306_Vm_P6p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(PB306_Vm_P6p_wt_mod_e_Treatment)
    colnames(PB306_Vm_P6p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PB306_Vm_P6p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PB306_Vm_P6p_wt_mod_e_Treatment, prob = 0.83))
    colnames(PB306_Vm_P6p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PB306_Vm_P6p_wt_mod_Treatment_liab <- cbind.data.frame(PB306_Vm_P6p_wt_mod_e_Treatment_hpd0.95,PB306_Vm_P6p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PB306_Vm_P6p_wt_mod_Treatment_liab$Ancestor <- c("PB306","PB306")
    PB306_Vm_P6p_wt_mod_Treatment_liab$Pnp <- c("P6.p","P6.p")
    PB306_Vm_P6p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    PB306_Vm_P6p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    PB306_Vm_P6p_wt_mod_Treatment_liab
    
    PB306_Vm_P6p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PB306_Vm_P6p_wt_mod_Treatment_liab[,2]))
    colnames(PB306_Vm_P6p_wt_mod_Treatment_DataScale_median) <- c("median")
    PB306_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PB306_Vm_P6p_wt_mod_Treatment_liab[,3]))
    colnames(PB306_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PB306_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PB306_Vm_P6p_wt_mod_Treatment_liab[,4]))
    colnames(PB306_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PB306_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PB306_Vm_P6p_wt_mod_Treatment_liab[,5]))
    colnames(PB306_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PB306_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PB306_Vm_P6p_wt_mod_Treatment_liab[,6]))
    colnames(PB306_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PB306_Vm_P6p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PB306_Vm_P6p_wt_mod_Treatment_DataScale) <- c("Treatment")
    PB306_Vm_P6p_wt_mod_Treatment_DataScale <- cbind.data.frame(PB306_Vm_P6p_wt_mod_Treatment_DataScale,PB306_Vm_P6p_wt_mod_Treatment_DataScale_median,PB306_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95,PB306_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95,PB306_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83,PB306_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    PB306_Vm_P6p_wt_mod_Treatment_DataScale$Ancestor <- c("PB306","PB306")
    PB306_Vm_P6p_wt_mod_Treatment_DataScale$Pnp <- c("P6.p","P6.p")
    PB306_Vm_P6p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    PB306_Vm_P6p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PB306_Vm_P6p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(PB306_Vm_P6p_wt_mod_Treatment_liab, PB306_Vm_P6p_wt_mod_Treatment_DataScale)
    PB306_Vm_P6p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##PB306_Vm_P7p_wt Mutation Median effects - Mutational Bias----
  {
    PB306_Vm_P7p_wt_mod_e_Treatment<- emmeans(PB306_Vm_P7p_wt_mod, ~Treatment, data=Vm_PB306_data)
    PB306_Vm_P7p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(PB306_Vm_P7p_wt_mod_e_Treatment)
    colnames(PB306_Vm_P7p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PB306_Vm_P7p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PB306_Vm_P7p_wt_mod_e_Treatment, prob = 0.83))
    colnames(PB306_Vm_P7p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PB306_Vm_P7p_wt_mod_Treatment_liab <- cbind.data.frame(PB306_Vm_P7p_wt_mod_e_Treatment_hpd0.95,PB306_Vm_P7p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PB306_Vm_P7p_wt_mod_Treatment_liab$Ancestor <- c("PB306","PB306")
    PB306_Vm_P7p_wt_mod_Treatment_liab$Pnp <- c("P7.p","P7.p")
    PB306_Vm_P7p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    PB306_Vm_P7p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    PB306_Vm_P7p_wt_mod_Treatment_liab
    
    PB306_Vm_P7p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PB306_Vm_P7p_wt_mod_Treatment_liab[,2]))
    colnames(PB306_Vm_P7p_wt_mod_Treatment_DataScale_median) <- c("median")
    PB306_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PB306_Vm_P7p_wt_mod_Treatment_liab[,3]))
    colnames(PB306_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PB306_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PB306_Vm_P7p_wt_mod_Treatment_liab[,4]))
    colnames(PB306_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PB306_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PB306_Vm_P7p_wt_mod_Treatment_liab[,5]))
    colnames(PB306_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PB306_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PB306_Vm_P7p_wt_mod_Treatment_liab[,6]))
    colnames(PB306_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PB306_Vm_P7p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PB306_Vm_P7p_wt_mod_Treatment_DataScale) <- c("Treatment")
    PB306_Vm_P7p_wt_mod_Treatment_DataScale <- cbind.data.frame(PB306_Vm_P7p_wt_mod_Treatment_DataScale,PB306_Vm_P7p_wt_mod_Treatment_DataScale_median,PB306_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95,PB306_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95,PB306_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83,PB306_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    PB306_Vm_P7p_wt_mod_Treatment_DataScale$Ancestor <- c("PB306","PB306")
    PB306_Vm_P7p_wt_mod_Treatment_DataScale$Pnp <- c("P7.p","P7.p")
    PB306_Vm_P7p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    PB306_Vm_P7p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PB306_Vm_P7p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(PB306_Vm_P7p_wt_mod_Treatment_liab, PB306_Vm_P7p_wt_mod_Treatment_DataScale)
    PB306_Vm_P7p_wt_mod_Treatment_liab_DataScale
    
  }
  ##----
  PB306_Vm_mod_Treatment_liab_DataScale <- rbind.data.frame(PB306_Vm_P3p_SS_mod_Treatment_liab_DataScale,PB306_Vm_P4p_SS_mod_Treatment_liab_DataScale,PB306_Vm_P5p_wt_mod_Treatment_liab_DataScale,PB306_Vm_P6p_wt_mod_Treatment_liab_DataScale,PB306_Vm_P7p_wt_mod_Treatment_liab_DataScale, PB306_Vm_P8p_SS_mod_Treatment_liab_DataScale)
  PB306_Vm_mod_Treatment_liab_DataScale$Species <- rep("C.elegans",24)
  PB306_Vm_mod_Treatment_liab_DataScale$Genus <- rep("Caenorhabditis",24)
  
  View(PB306_Vm_mod_Treatment_liab_DataScale)
  
}

#AF16_Vm_ Mutational Median effects - Mutational Bias ----
{
  ##AF16_Vm_P3p_SS Mutation Median effects - Mutational Bias----
  {
    AF16_Vm_P3p_SS_mod_e_Treatment<- emmeans(AF16_Vm_P3p_SS_mod, ~Treatment, data=Vm_AF16_data)
    AF16_Vm_P3p_SS_mod_e_Treatment_hpd0.95 <- as.data.frame(AF16_Vm_P3p_SS_mod_e_Treatment)
    colnames(AF16_Vm_P3p_SS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    AF16_Vm_P3p_SS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(AF16_Vm_P3p_SS_mod_e_Treatment, prob = 0.83))
    colnames(AF16_Vm_P3p_SS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    AF16_Vm_P3p_SS_mod_Treatment_liab <- cbind.data.frame(AF16_Vm_P3p_SS_mod_e_Treatment_hpd0.95,AF16_Vm_P3p_SS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    AF16_Vm_P3p_SS_mod_Treatment_liab$Ancestor <- c("AF16","AF16")
    AF16_Vm_P3p_SS_mod_Treatment_liab$Pnp <- c("P3.p","P3.p")
    AF16_Vm_P3p_SS_mod_Treatment_liab$Pnp_fate <- c("SS","SS")
    AF16_Vm_P3p_SS_mod_Treatment_liab$Scale <- c("liab","liab")
    AF16_Vm_P3p_SS_mod_Treatment_liab
    
    AF16_Vm_P3p_SS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(AF16_Vm_P3p_SS_mod_Treatment_liab[,2]))
    colnames(AF16_Vm_P3p_SS_mod_Treatment_DataScale_median) <- c("median")
    AF16_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(AF16_Vm_P3p_SS_mod_Treatment_liab[,3]))
    colnames(AF16_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    AF16_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(AF16_Vm_P3p_SS_mod_Treatment_liab[,4]))
    colnames(AF16_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    AF16_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(AF16_Vm_P3p_SS_mod_Treatment_liab[,5]))
    colnames(AF16_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    AF16_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(AF16_Vm_P3p_SS_mod_Treatment_liab[,6]))
    colnames(AF16_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    AF16_Vm_P3p_SS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(AF16_Vm_P3p_SS_mod_Treatment_DataScale) <- c("Treatment")
    AF16_Vm_P3p_SS_mod_Treatment_DataScale <- cbind.data.frame(AF16_Vm_P3p_SS_mod_Treatment_DataScale,AF16_Vm_P3p_SS_mod_Treatment_DataScale_median,AF16_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.95,AF16_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.95,AF16_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.83,AF16_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.83 )
    AF16_Vm_P3p_SS_mod_Treatment_DataScale$Ancestor <- c("AF16","AF16")
    AF16_Vm_P3p_SS_mod_Treatment_DataScale$Pnp <- c("P3.p","P3.p")
    AF16_Vm_P3p_SS_mod_Treatment_DataScale$Pnp_fate <- c("SS","SS")
    AF16_Vm_P3p_SS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    AF16_Vm_P3p_SS_mod_Treatment_liab_DataScale <- rbind.data.frame(AF16_Vm_P3p_SS_mod_Treatment_liab, AF16_Vm_P3p_SS_mod_Treatment_DataScale)
    AF16_Vm_P3p_SS_mod_Treatment_liab_DataScale
    
  }
  
  ##AF16_Vm_P4p_SS Mutation Median effects - Mutational Bias----
  {
    AF16_Vm_P4p_SS_mod_e_Treatment<- emmeans(AF16_Vm_P4p_SS_mod, ~Treatment, data=Vm_AF16_data)
    AF16_Vm_P4p_SS_mod_e_Treatment_hpd0.95 <- as.data.frame(AF16_Vm_P4p_SS_mod_e_Treatment)
    colnames(AF16_Vm_P4p_SS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    AF16_Vm_P4p_SS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(AF16_Vm_P4p_SS_mod_e_Treatment, prob = 0.83))
    colnames(AF16_Vm_P4p_SS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    AF16_Vm_P4p_SS_mod_Treatment_liab <- cbind.data.frame(AF16_Vm_P4p_SS_mod_e_Treatment_hpd0.95,AF16_Vm_P4p_SS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    AF16_Vm_P4p_SS_mod_Treatment_liab$Ancestor <- c("AF16","AF16")
    AF16_Vm_P4p_SS_mod_Treatment_liab$Pnp <- c("P4.p","P4.p")
    AF16_Vm_P4p_SS_mod_Treatment_liab$Pnp_fate <- c("SS","SS")
    AF16_Vm_P4p_SS_mod_Treatment_liab$Scale <- c("liab","liab")
    AF16_Vm_P4p_SS_mod_Treatment_liab
    
    AF16_Vm_P4p_SS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(AF16_Vm_P4p_SS_mod_Treatment_liab[,2]))
    colnames(AF16_Vm_P4p_SS_mod_Treatment_DataScale_median) <- c("median")
    AF16_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(AF16_Vm_P4p_SS_mod_Treatment_liab[,3]))
    colnames(AF16_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    AF16_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(AF16_Vm_P4p_SS_mod_Treatment_liab[,4]))
    colnames(AF16_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    AF16_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(AF16_Vm_P4p_SS_mod_Treatment_liab[,5]))
    colnames(AF16_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    AF16_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(AF16_Vm_P4p_SS_mod_Treatment_liab[,6]))
    colnames(AF16_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    AF16_Vm_P4p_SS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(AF16_Vm_P4p_SS_mod_Treatment_DataScale) <- c("Treatment")
    AF16_Vm_P4p_SS_mod_Treatment_DataScale <- cbind.data.frame(AF16_Vm_P4p_SS_mod_Treatment_DataScale,AF16_Vm_P4p_SS_mod_Treatment_DataScale_median,AF16_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.95,AF16_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.95,AF16_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.83,AF16_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.83 )
    AF16_Vm_P4p_SS_mod_Treatment_DataScale$Ancestor <- c("AF16","AF16")
    AF16_Vm_P4p_SS_mod_Treatment_DataScale$Pnp <- c("P4.p","P4.p")
    AF16_Vm_P4p_SS_mod_Treatment_DataScale$Pnp_fate <- c("SS","SS")
    AF16_Vm_P4p_SS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    AF16_Vm_P4p_SS_mod_Treatment_liab_DataScale <- rbind.data.frame(AF16_Vm_P4p_SS_mod_Treatment_liab, AF16_Vm_P4p_SS_mod_Treatment_DataScale)
    AF16_Vm_P4p_SS_mod_Treatment_liab_DataScale
    
  }
  
  ##AF16_Vm_P8p_SS Mutation Median effects - Mutational Bias----
  {
    AF16_Vm_P8p_SS_mod_e_Treatment<- emmeans(AF16_Vm_P8p_SS_mod, ~Treatment, data=Vm_AF16_data)
    AF16_Vm_P8p_SS_mod_e_Treatment_hpd0.95 <- as.data.frame(AF16_Vm_P8p_SS_mod_e_Treatment)
    colnames(AF16_Vm_P8p_SS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    AF16_Vm_P8p_SS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(AF16_Vm_P8p_SS_mod_e_Treatment, prob = 0.83))
    colnames(AF16_Vm_P8p_SS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    AF16_Vm_P8p_SS_mod_Treatment_liab <- cbind.data.frame(AF16_Vm_P8p_SS_mod_e_Treatment_hpd0.95,AF16_Vm_P8p_SS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    AF16_Vm_P8p_SS_mod_Treatment_liab$Ancestor <- c("AF16","AF16")
    AF16_Vm_P8p_SS_mod_Treatment_liab$Pnp <- c("P8.p","P8.p")
    AF16_Vm_P8p_SS_mod_Treatment_liab$Pnp_fate <- c("SS","SS")
    AF16_Vm_P8p_SS_mod_Treatment_liab$Scale <- c("liab","liab")
    AF16_Vm_P8p_SS_mod_Treatment_liab
    
    AF16_Vm_P8p_SS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(AF16_Vm_P8p_SS_mod_Treatment_liab[,2]))
    colnames(AF16_Vm_P8p_SS_mod_Treatment_DataScale_median) <- c("median")
    AF16_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(AF16_Vm_P8p_SS_mod_Treatment_liab[,3]))
    colnames(AF16_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    AF16_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(AF16_Vm_P8p_SS_mod_Treatment_liab[,4]))
    colnames(AF16_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    AF16_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(AF16_Vm_P8p_SS_mod_Treatment_liab[,5]))
    colnames(AF16_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    AF16_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(AF16_Vm_P8p_SS_mod_Treatment_liab[,6]))
    colnames(AF16_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    AF16_Vm_P8p_SS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(AF16_Vm_P8p_SS_mod_Treatment_DataScale) <- c("Treatment")
    AF16_Vm_P8p_SS_mod_Treatment_DataScale <- cbind.data.frame(AF16_Vm_P8p_SS_mod_Treatment_DataScale,AF16_Vm_P8p_SS_mod_Treatment_DataScale_median,AF16_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.95,AF16_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.95,AF16_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.83,AF16_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.83 )
    AF16_Vm_P8p_SS_mod_Treatment_DataScale$Ancestor <- c("AF16","AF16")
    AF16_Vm_P8p_SS_mod_Treatment_DataScale$Pnp <- c("P8.p","P8.p")
    AF16_Vm_P8p_SS_mod_Treatment_DataScale$Pnp_fate <- c("SS","SS")
    AF16_Vm_P8p_SS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    AF16_Vm_P8p_SS_mod_Treatment_liab_DataScale <- rbind.data.frame(AF16_Vm_P8p_SS_mod_Treatment_liab, AF16_Vm_P8p_SS_mod_Treatment_DataScale)
    AF16_Vm_P8p_SS_mod_Treatment_liab_DataScale
    
  }
  
  ##AF16_Vm_P5p_wt Mutation Median effects - Mutational Bias----
  {
    AF16_Vm_P5p_wt_mod_e_Treatment<- emmeans(AF16_Vm_P5p_wt_mod, ~Treatment, data=Vm_AF16_data)
    AF16_Vm_P5p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(AF16_Vm_P5p_wt_mod_e_Treatment)
    colnames(AF16_Vm_P5p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    AF16_Vm_P5p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(AF16_Vm_P5p_wt_mod_e_Treatment, prob = 0.83))
    colnames(AF16_Vm_P5p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    AF16_Vm_P5p_wt_mod_Treatment_liab <- cbind.data.frame(AF16_Vm_P5p_wt_mod_e_Treatment_hpd0.95,AF16_Vm_P5p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    AF16_Vm_P5p_wt_mod_Treatment_liab$Ancestor <- c("AF16","AF16")
    AF16_Vm_P5p_wt_mod_Treatment_liab$Pnp <- c("P5.p","P5.p")
    AF16_Vm_P5p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    AF16_Vm_P5p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    AF16_Vm_P5p_wt_mod_Treatment_liab
    
    AF16_Vm_P5p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(AF16_Vm_P5p_wt_mod_Treatment_liab[,2]))
    colnames(AF16_Vm_P5p_wt_mod_Treatment_DataScale_median) <- c("median")
    AF16_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(AF16_Vm_P5p_wt_mod_Treatment_liab[,3]))
    colnames(AF16_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    AF16_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(AF16_Vm_P5p_wt_mod_Treatment_liab[,4]))
    colnames(AF16_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    AF16_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(AF16_Vm_P5p_wt_mod_Treatment_liab[,5]))
    colnames(AF16_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    AF16_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(AF16_Vm_P5p_wt_mod_Treatment_liab[,6]))
    colnames(AF16_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    AF16_Vm_P5p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(AF16_Vm_P5p_wt_mod_Treatment_DataScale) <- c("Treatment")
    AF16_Vm_P5p_wt_mod_Treatment_DataScale <- cbind.data.frame(AF16_Vm_P5p_wt_mod_Treatment_DataScale,AF16_Vm_P5p_wt_mod_Treatment_DataScale_median,AF16_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95,AF16_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95,AF16_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83,AF16_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    AF16_Vm_P5p_wt_mod_Treatment_DataScale$Ancestor <- c("AF16","AF16")
    AF16_Vm_P5p_wt_mod_Treatment_DataScale$Pnp <- c("P5.p","P5.p")
    AF16_Vm_P5p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    AF16_Vm_P5p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    AF16_Vm_P5p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(AF16_Vm_P5p_wt_mod_Treatment_liab, AF16_Vm_P5p_wt_mod_Treatment_DataScale)
    AF16_Vm_P5p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##AF16_Vm_P6p_wt Mutation Median effects - Mutational Bias----
  {
    AF16_Vm_P6p_wt_mod_e_Treatment<- emmeans(AF16_Vm_P6p_wt_mod, ~Treatment, data=Vm_AF16_data)
    AF16_Vm_P6p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(AF16_Vm_P6p_wt_mod_e_Treatment)
    colnames(AF16_Vm_P6p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    AF16_Vm_P6p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(AF16_Vm_P6p_wt_mod_e_Treatment, prob = 0.83))
    colnames(AF16_Vm_P6p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    AF16_Vm_P6p_wt_mod_Treatment_liab <- cbind.data.frame(AF16_Vm_P6p_wt_mod_e_Treatment_hpd0.95,AF16_Vm_P6p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    AF16_Vm_P6p_wt_mod_Treatment_liab$Ancestor <- c("AF16","AF16")
    AF16_Vm_P6p_wt_mod_Treatment_liab$Pnp <- c("P6.p","P6.p")
    AF16_Vm_P6p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    AF16_Vm_P6p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    AF16_Vm_P6p_wt_mod_Treatment_liab
    
    AF16_Vm_P6p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(AF16_Vm_P6p_wt_mod_Treatment_liab[,2]))
    colnames(AF16_Vm_P6p_wt_mod_Treatment_DataScale_median) <- c("median")
    AF16_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(AF16_Vm_P6p_wt_mod_Treatment_liab[,3]))
    colnames(AF16_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    AF16_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(AF16_Vm_P6p_wt_mod_Treatment_liab[,4]))
    colnames(AF16_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    AF16_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(AF16_Vm_P6p_wt_mod_Treatment_liab[,5]))
    colnames(AF16_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    AF16_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(AF16_Vm_P6p_wt_mod_Treatment_liab[,6]))
    colnames(AF16_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    AF16_Vm_P6p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(AF16_Vm_P6p_wt_mod_Treatment_DataScale) <- c("Treatment")
    AF16_Vm_P6p_wt_mod_Treatment_DataScale <- cbind.data.frame(AF16_Vm_P6p_wt_mod_Treatment_DataScale,AF16_Vm_P6p_wt_mod_Treatment_DataScale_median,AF16_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95,AF16_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95,AF16_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83,AF16_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    AF16_Vm_P6p_wt_mod_Treatment_DataScale$Ancestor <- c("AF16","AF16")
    AF16_Vm_P6p_wt_mod_Treatment_DataScale$Pnp <- c("P6.p","P6.p")
    AF16_Vm_P6p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    AF16_Vm_P6p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    AF16_Vm_P6p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(AF16_Vm_P6p_wt_mod_Treatment_liab, AF16_Vm_P6p_wt_mod_Treatment_DataScale)
    AF16_Vm_P6p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##AF16_Vm_P7p_wt Mutation Median effects - Mutational Bias----
  {
    AF16_Vm_P7p_wt_mod_e_Treatment<- emmeans(AF16_Vm_P7p_wt_mod, ~Treatment, data=Vm_AF16_data)
    AF16_Vm_P7p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(AF16_Vm_P7p_wt_mod_e_Treatment)
    colnames(AF16_Vm_P7p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    AF16_Vm_P7p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(AF16_Vm_P7p_wt_mod_e_Treatment, prob = 0.83))
    colnames(AF16_Vm_P7p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    AF16_Vm_P7p_wt_mod_Treatment_liab <- cbind.data.frame(AF16_Vm_P7p_wt_mod_e_Treatment_hpd0.95,AF16_Vm_P7p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    AF16_Vm_P7p_wt_mod_Treatment_liab$Ancestor <- c("AF16","AF16")
    AF16_Vm_P7p_wt_mod_Treatment_liab$Pnp <- c("P7.p","P7.p")
    AF16_Vm_P7p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    AF16_Vm_P7p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    AF16_Vm_P7p_wt_mod_Treatment_liab
    
    AF16_Vm_P7p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(AF16_Vm_P7p_wt_mod_Treatment_liab[,2]))
    colnames(AF16_Vm_P7p_wt_mod_Treatment_DataScale_median) <- c("median")
    AF16_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(AF16_Vm_P7p_wt_mod_Treatment_liab[,3]))
    colnames(AF16_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    AF16_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(AF16_Vm_P7p_wt_mod_Treatment_liab[,4]))
    colnames(AF16_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    AF16_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(AF16_Vm_P7p_wt_mod_Treatment_liab[,5]))
    colnames(AF16_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    AF16_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(AF16_Vm_P7p_wt_mod_Treatment_liab[,6]))
    colnames(AF16_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    AF16_Vm_P7p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(AF16_Vm_P7p_wt_mod_Treatment_DataScale) <- c("Treatment")
    AF16_Vm_P7p_wt_mod_Treatment_DataScale <- cbind.data.frame(AF16_Vm_P7p_wt_mod_Treatment_DataScale,AF16_Vm_P7p_wt_mod_Treatment_DataScale_median,AF16_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95,AF16_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95,AF16_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83,AF16_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    AF16_Vm_P7p_wt_mod_Treatment_DataScale$Ancestor <- c("AF16","AF16")
    AF16_Vm_P7p_wt_mod_Treatment_DataScale$Pnp <- c("P7.p","P7.p")
    AF16_Vm_P7p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    AF16_Vm_P7p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    AF16_Vm_P7p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(AF16_Vm_P7p_wt_mod_Treatment_liab, AF16_Vm_P7p_wt_mod_Treatment_DataScale)
    AF16_Vm_P7p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##----
  AF16_Vm_mod_Treatment_liab_DataScale <- rbind.data.frame(AF16_Vm_P3p_SS_mod_Treatment_liab_DataScale,AF16_Vm_P4p_SS_mod_Treatment_liab_DataScale,AF16_Vm_P5p_wt_mod_Treatment_liab_DataScale,AF16_Vm_P6p_wt_mod_Treatment_liab_DataScale,AF16_Vm_P7p_wt_mod_Treatment_liab_DataScale, AF16_Vm_P8p_SS_mod_Treatment_liab_DataScale)
  AF16_Vm_mod_Treatment_liab_DataScale$Species <- rep("C.briggsae",24)
  AF16_Vm_mod_Treatment_liab_DataScale$Genus <- rep("Caenorhabditis",24)
  
  View(AF16_Vm_mod_Treatment_liab_DataScale)
  
}

#PB800_Vm_ Mutational Median effects - Mutational Bias ----
{
  ##PB800_Vm_P3p_SS Mutation Median effects - Mutational Bias----
  {
    PB800_Vm_P3p_SS_mod_e_Treatment<- emmeans(PB800_Vm_P3p_SS_mod, ~Treatment, data=Vm_PB800_data)
    PB800_Vm_P3p_SS_mod_e_Treatment_hpd0.95 <- as.data.frame(PB800_Vm_P3p_SS_mod_e_Treatment)
    colnames(PB800_Vm_P3p_SS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PB800_Vm_P3p_SS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PB800_Vm_P3p_SS_mod_e_Treatment, prob = 0.83))
    colnames(PB800_Vm_P3p_SS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PB800_Vm_P3p_SS_mod_Treatment_liab <- cbind.data.frame(PB800_Vm_P3p_SS_mod_e_Treatment_hpd0.95,PB800_Vm_P3p_SS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PB800_Vm_P3p_SS_mod_Treatment_liab$Ancestor <- c("PB800","PB800")
    PB800_Vm_P3p_SS_mod_Treatment_liab$Pnp <- c("P3.p","P3.p")
    PB800_Vm_P3p_SS_mod_Treatment_liab$Pnp_fate <- c("SS","SS")
    PB800_Vm_P3p_SS_mod_Treatment_liab$Scale <- c("liab","liab")
    PB800_Vm_P3p_SS_mod_Treatment_liab
    
    PB800_Vm_P3p_SS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PB800_Vm_P3p_SS_mod_Treatment_liab[,2]))
    colnames(PB800_Vm_P3p_SS_mod_Treatment_DataScale_median) <- c("median")
    PB800_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PB800_Vm_P3p_SS_mod_Treatment_liab[,3]))
    colnames(PB800_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PB800_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PB800_Vm_P3p_SS_mod_Treatment_liab[,4]))
    colnames(PB800_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PB800_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PB800_Vm_P3p_SS_mod_Treatment_liab[,5]))
    colnames(PB800_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PB800_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PB800_Vm_P3p_SS_mod_Treatment_liab[,6]))
    colnames(PB800_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PB800_Vm_P3p_SS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PB800_Vm_P3p_SS_mod_Treatment_DataScale) <- c("Treatment")
    PB800_Vm_P3p_SS_mod_Treatment_DataScale <- cbind.data.frame(PB800_Vm_P3p_SS_mod_Treatment_DataScale,PB800_Vm_P3p_SS_mod_Treatment_DataScale_median,PB800_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.95,PB800_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.95,PB800_Vm_P3p_SS_mod_Treatment_DataScale_lower.HPD_0.83,PB800_Vm_P3p_SS_mod_Treatment_DataScale_upper.HPD_0.83 )
    PB800_Vm_P3p_SS_mod_Treatment_DataScale$Ancestor <- c("PB800","PB800")
    PB800_Vm_P3p_SS_mod_Treatment_DataScale$Pnp <- c("P3.p","P3.p")
    PB800_Vm_P3p_SS_mod_Treatment_DataScale$Pnp_fate <- c("SS","SS")
    PB800_Vm_P3p_SS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PB800_Vm_P3p_SS_mod_Treatment_liab_DataScale <- rbind.data.frame(PB800_Vm_P3p_SS_mod_Treatment_liab, PB800_Vm_P3p_SS_mod_Treatment_DataScale)
    PB800_Vm_P3p_SS_mod_Treatment_liab_DataScale
    
  }
  
  ##PB800_Vm_P4p_SS Mutation Median effects - Mutational Bias----
  {
    PB800_Vm_P4p_SS_mod_e_Treatment<- emmeans(PB800_Vm_P4p_SS_mod, ~Treatment, data=Vm_PB800_data)
    PB800_Vm_P4p_SS_mod_e_Treatment_hpd0.95 <- as.data.frame(PB800_Vm_P4p_SS_mod_e_Treatment)
    colnames(PB800_Vm_P4p_SS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PB800_Vm_P4p_SS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PB800_Vm_P4p_SS_mod_e_Treatment, prob = 0.83))
    colnames(PB800_Vm_P4p_SS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PB800_Vm_P4p_SS_mod_Treatment_liab <- cbind.data.frame(PB800_Vm_P4p_SS_mod_e_Treatment_hpd0.95,PB800_Vm_P4p_SS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PB800_Vm_P4p_SS_mod_Treatment_liab$Ancestor <- c("PB800","PB800")
    PB800_Vm_P4p_SS_mod_Treatment_liab$Pnp <- c("P4.p","P4.p")
    PB800_Vm_P4p_SS_mod_Treatment_liab$Pnp_fate <- c("SS","SS")
    PB800_Vm_P4p_SS_mod_Treatment_liab$Scale <- c("liab","liab")
    PB800_Vm_P4p_SS_mod_Treatment_liab
    
    PB800_Vm_P4p_SS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PB800_Vm_P4p_SS_mod_Treatment_liab[,2]))
    colnames(PB800_Vm_P4p_SS_mod_Treatment_DataScale_median) <- c("median")
    PB800_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PB800_Vm_P4p_SS_mod_Treatment_liab[,3]))
    colnames(PB800_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PB800_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PB800_Vm_P4p_SS_mod_Treatment_liab[,4]))
    colnames(PB800_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PB800_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PB800_Vm_P4p_SS_mod_Treatment_liab[,5]))
    colnames(PB800_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PB800_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PB800_Vm_P4p_SS_mod_Treatment_liab[,6]))
    colnames(PB800_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PB800_Vm_P4p_SS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PB800_Vm_P4p_SS_mod_Treatment_DataScale) <- c("Treatment")
    PB800_Vm_P4p_SS_mod_Treatment_DataScale <- cbind.data.frame(PB800_Vm_P4p_SS_mod_Treatment_DataScale,PB800_Vm_P4p_SS_mod_Treatment_DataScale_median,PB800_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.95,PB800_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.95,PB800_Vm_P4p_SS_mod_Treatment_DataScale_lower.HPD_0.83,PB800_Vm_P4p_SS_mod_Treatment_DataScale_upper.HPD_0.83 )
    PB800_Vm_P4p_SS_mod_Treatment_DataScale$Ancestor <- c("PB800","PB800")
    PB800_Vm_P4p_SS_mod_Treatment_DataScale$Pnp <- c("P4.p","P4.p")
    PB800_Vm_P4p_SS_mod_Treatment_DataScale$Pnp_fate <- c("SS","SS")
    PB800_Vm_P4p_SS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PB800_Vm_P4p_SS_mod_Treatment_liab_DataScale <- rbind.data.frame(PB800_Vm_P4p_SS_mod_Treatment_liab, PB800_Vm_P4p_SS_mod_Treatment_DataScale)
    PB800_Vm_P4p_SS_mod_Treatment_liab_DataScale
    
  }
  
  ##PB800_Vm_P8p_SS Mutation Median effects - Mutational Bias----
  {
    PB800_Vm_P8p_SS_mod_e_Treatment<- emmeans(PB800_Vm_P8p_SS_mod, ~Treatment, data=Vm_PB800_data)
    PB800_Vm_P8p_SS_mod_e_Treatment_hpd0.95 <- as.data.frame(PB800_Vm_P8p_SS_mod_e_Treatment)
    colnames(PB800_Vm_P8p_SS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PB800_Vm_P8p_SS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PB800_Vm_P8p_SS_mod_e_Treatment, prob = 0.83))
    colnames(PB800_Vm_P8p_SS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PB800_Vm_P8p_SS_mod_Treatment_liab <- cbind.data.frame(PB800_Vm_P8p_SS_mod_e_Treatment_hpd0.95,PB800_Vm_P8p_SS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PB800_Vm_P8p_SS_mod_Treatment_liab$Ancestor <- c("PB800","PB800")
    PB800_Vm_P8p_SS_mod_Treatment_liab$Pnp <- c("P8.p","P8.p")
    PB800_Vm_P8p_SS_mod_Treatment_liab$Pnp_fate <- c("SS","SS")
    PB800_Vm_P8p_SS_mod_Treatment_liab$Scale <- c("liab","liab")
    PB800_Vm_P8p_SS_mod_Treatment_liab
    
    PB800_Vm_P8p_SS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PB800_Vm_P8p_SS_mod_Treatment_liab[,2]))
    colnames(PB800_Vm_P8p_SS_mod_Treatment_DataScale_median) <- c("median")
    PB800_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PB800_Vm_P8p_SS_mod_Treatment_liab[,3]))
    colnames(PB800_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PB800_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PB800_Vm_P8p_SS_mod_Treatment_liab[,4]))
    colnames(PB800_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PB800_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PB800_Vm_P8p_SS_mod_Treatment_liab[,5]))
    colnames(PB800_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PB800_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PB800_Vm_P8p_SS_mod_Treatment_liab[,6]))
    colnames(PB800_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PB800_Vm_P8p_SS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PB800_Vm_P8p_SS_mod_Treatment_DataScale) <- c("Treatment")
    PB800_Vm_P8p_SS_mod_Treatment_DataScale <- cbind.data.frame(PB800_Vm_P8p_SS_mod_Treatment_DataScale,PB800_Vm_P8p_SS_mod_Treatment_DataScale_median,PB800_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.95,PB800_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.95,PB800_Vm_P8p_SS_mod_Treatment_DataScale_lower.HPD_0.83,PB800_Vm_P8p_SS_mod_Treatment_DataScale_upper.HPD_0.83 )
    PB800_Vm_P8p_SS_mod_Treatment_DataScale$Ancestor <- c("PB800","PB800")
    PB800_Vm_P8p_SS_mod_Treatment_DataScale$Pnp <- c("P8.p","P8.p")
    PB800_Vm_P8p_SS_mod_Treatment_DataScale$Pnp_fate <- c("SS","SS")
    PB800_Vm_P8p_SS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PB800_Vm_P8p_SS_mod_Treatment_liab_DataScale <- rbind.data.frame(PB800_Vm_P8p_SS_mod_Treatment_liab, PB800_Vm_P8p_SS_mod_Treatment_DataScale)
    PB800_Vm_P8p_SS_mod_Treatment_liab_DataScale
    
  }
  
  ##PB800_Vm_P5p_wt Mutation Median effects - Mutational Bias----
  {
    PB800_Vm_P5p_wt_mod_e_Treatment<- emmeans(PB800_Vm_P5p_wt_mod, ~Treatment, data=Vm_PB800_data)
    PB800_Vm_P5p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(PB800_Vm_P5p_wt_mod_e_Treatment)
    colnames(PB800_Vm_P5p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PB800_Vm_P5p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PB800_Vm_P5p_wt_mod_e_Treatment, prob = 0.83))
    colnames(PB800_Vm_P5p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PB800_Vm_P5p_wt_mod_Treatment_liab <- cbind.data.frame(PB800_Vm_P5p_wt_mod_e_Treatment_hpd0.95,PB800_Vm_P5p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PB800_Vm_P5p_wt_mod_Treatment_liab$Ancestor <- c("PB800","PB800")
    PB800_Vm_P5p_wt_mod_Treatment_liab$Pnp <- c("P5.p","P5.p")
    PB800_Vm_P5p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    PB800_Vm_P5p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    PB800_Vm_P5p_wt_mod_Treatment_liab
    
    PB800_Vm_P5p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PB800_Vm_P5p_wt_mod_Treatment_liab[,2]))
    colnames(PB800_Vm_P5p_wt_mod_Treatment_DataScale_median) <- c("median")
    PB800_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PB800_Vm_P5p_wt_mod_Treatment_liab[,3]))
    colnames(PB800_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PB800_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PB800_Vm_P5p_wt_mod_Treatment_liab[,4]))
    colnames(PB800_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PB800_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PB800_Vm_P5p_wt_mod_Treatment_liab[,5]))
    colnames(PB800_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PB800_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PB800_Vm_P5p_wt_mod_Treatment_liab[,6]))
    colnames(PB800_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PB800_Vm_P5p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PB800_Vm_P5p_wt_mod_Treatment_DataScale) <- c("Treatment")
    PB800_Vm_P5p_wt_mod_Treatment_DataScale <- cbind.data.frame(PB800_Vm_P5p_wt_mod_Treatment_DataScale,PB800_Vm_P5p_wt_mod_Treatment_DataScale_median,PB800_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95,PB800_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95,PB800_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83,PB800_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    PB800_Vm_P5p_wt_mod_Treatment_DataScale$Ancestor <- c("PB800","PB800")
    PB800_Vm_P5p_wt_mod_Treatment_DataScale$Pnp <- c("P5.p","P5.p")
    PB800_Vm_P5p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    PB800_Vm_P5p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PB800_Vm_P5p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(PB800_Vm_P5p_wt_mod_Treatment_liab, PB800_Vm_P5p_wt_mod_Treatment_DataScale)
    PB800_Vm_P5p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##PB800_Vm_P6p_wt Mutation Median effects - Mutational Bias----
  {
    PB800_Vm_P6p_wt_mod_e_Treatment<- emmeans(PB800_Vm_P6p_wt_mod, ~Treatment, data=Vm_PB800_data)
    PB800_Vm_P6p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(PB800_Vm_P6p_wt_mod_e_Treatment)
    colnames(PB800_Vm_P6p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PB800_Vm_P6p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PB800_Vm_P6p_wt_mod_e_Treatment, prob = 0.83))
    colnames(PB800_Vm_P6p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PB800_Vm_P6p_wt_mod_Treatment_liab <- cbind.data.frame(PB800_Vm_P6p_wt_mod_e_Treatment_hpd0.95,PB800_Vm_P6p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PB800_Vm_P6p_wt_mod_Treatment_liab$Ancestor <- c("PB800","PB800")
    PB800_Vm_P6p_wt_mod_Treatment_liab$Pnp <- c("P6.p","P6.p")
    PB800_Vm_P6p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    PB800_Vm_P6p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    PB800_Vm_P6p_wt_mod_Treatment_liab
    
    PB800_Vm_P6p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PB800_Vm_P6p_wt_mod_Treatment_liab[,2]))
    colnames(PB800_Vm_P6p_wt_mod_Treatment_DataScale_median) <- c("median")
    PB800_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PB800_Vm_P6p_wt_mod_Treatment_liab[,3]))
    colnames(PB800_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PB800_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PB800_Vm_P6p_wt_mod_Treatment_liab[,4]))
    colnames(PB800_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PB800_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PB800_Vm_P6p_wt_mod_Treatment_liab[,5]))
    colnames(PB800_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PB800_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PB800_Vm_P6p_wt_mod_Treatment_liab[,6]))
    colnames(PB800_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PB800_Vm_P6p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PB800_Vm_P6p_wt_mod_Treatment_DataScale) <- c("Treatment")
    PB800_Vm_P6p_wt_mod_Treatment_DataScale <- cbind.data.frame(PB800_Vm_P6p_wt_mod_Treatment_DataScale,PB800_Vm_P6p_wt_mod_Treatment_DataScale_median,PB800_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95,PB800_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95,PB800_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83,PB800_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    PB800_Vm_P6p_wt_mod_Treatment_DataScale$Ancestor <- c("PB800","PB800")
    PB800_Vm_P6p_wt_mod_Treatment_DataScale$Pnp <- c("P6.p","P6.p")
    PB800_Vm_P6p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    PB800_Vm_P6p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PB800_Vm_P6p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(PB800_Vm_P6p_wt_mod_Treatment_liab, PB800_Vm_P6p_wt_mod_Treatment_DataScale)
    PB800_Vm_P6p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##PB800_Vm_P7p_wt Mutation Median effects - Mutational Bias----
  {
    PB800_Vm_P7p_wt_mod_e_Treatment<- emmeans(PB800_Vm_P7p_wt_mod, ~Treatment, data=Vm_PB800_data)
    PB800_Vm_P7p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(PB800_Vm_P7p_wt_mod_e_Treatment)
    colnames(PB800_Vm_P7p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PB800_Vm_P7p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PB800_Vm_P7p_wt_mod_e_Treatment, prob = 0.83))
    colnames(PB800_Vm_P7p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PB800_Vm_P7p_wt_mod_Treatment_liab <- cbind.data.frame(PB800_Vm_P7p_wt_mod_e_Treatment_hpd0.95,PB800_Vm_P7p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PB800_Vm_P7p_wt_mod_Treatment_liab$Ancestor <- c("PB800","PB800")
    PB800_Vm_P7p_wt_mod_Treatment_liab$Pnp <- c("P7.p","P7.p")
    PB800_Vm_P7p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    PB800_Vm_P7p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    PB800_Vm_P7p_wt_mod_Treatment_liab
    
    PB800_Vm_P7p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PB800_Vm_P7p_wt_mod_Treatment_liab[,2]))
    colnames(PB800_Vm_P7p_wt_mod_Treatment_DataScale_median) <- c("median")
    PB800_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PB800_Vm_P7p_wt_mod_Treatment_liab[,3]))
    colnames(PB800_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PB800_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PB800_Vm_P7p_wt_mod_Treatment_liab[,4]))
    colnames(PB800_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PB800_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PB800_Vm_P7p_wt_mod_Treatment_liab[,5]))
    colnames(PB800_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PB800_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PB800_Vm_P7p_wt_mod_Treatment_liab[,6]))
    colnames(PB800_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PB800_Vm_P7p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PB800_Vm_P7p_wt_mod_Treatment_DataScale) <- c("Treatment")
    PB800_Vm_P7p_wt_mod_Treatment_DataScale <- cbind.data.frame(PB800_Vm_P7p_wt_mod_Treatment_DataScale,PB800_Vm_P7p_wt_mod_Treatment_DataScale_median,PB800_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95,PB800_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95,PB800_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83,PB800_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    PB800_Vm_P7p_wt_mod_Treatment_DataScale$Ancestor <- c("PB800","PB800")
    PB800_Vm_P7p_wt_mod_Treatment_DataScale$Pnp <- c("P7.p","P7.p")
    PB800_Vm_P7p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    PB800_Vm_P7p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PB800_Vm_P7p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(PB800_Vm_P7p_wt_mod_Treatment_liab, PB800_Vm_P7p_wt_mod_Treatment_DataScale)
    PB800_Vm_P7p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##----
  PB800_Vm_mod_Treatment_liab_DataScale <- rbind.data.frame(PB800_Vm_P3p_SS_mod_Treatment_liab_DataScale,PB800_Vm_P4p_SS_mod_Treatment_liab_DataScale,PB800_Vm_P5p_wt_mod_Treatment_liab_DataScale,PB800_Vm_P6p_wt_mod_Treatment_liab_DataScale,PB800_Vm_P7p_wt_mod_Treatment_liab_DataScale, PB800_Vm_P8p_SS_mod_Treatment_liab_DataScale)
  PB800_Vm_mod_Treatment_liab_DataScale$Species <- rep("C.briggsae",24)
  PB800_Vm_mod_Treatment_liab_DataScale$Genus <- rep("Caenorhabditis",24)
  
  View(PB800_Vm_mod_Treatment_liab_DataScale)
  
}

#Caenorhabditis_Vm_mutational_median_effects_summary----
Caenorhabditis_Vm_mutational_median_effects_SS_summary <- rbind.data.frame(JU1200_Vm_mod_Treatment_liab_DataScale, PB306_Vm_mod_Treatment_liab_DataScale, AF16_Vm_mod_Treatment_liab_DataScale, PB800_Vm_mod_Treatment_liab_DataScale)
Caenorhabditis_Vm_mutational_median_effects_SS_summary <- Caenorhabditis_Vm_mutational_median_effects_SS_summary %>% mutate(Treatment= str_replace(Treatment,'MA', 'ML'))
Caenorhabditis_Vm_mutational_median_effects_SS_summary <- Caenorhabditis_Vm_mutational_median_effects_SS_summary %>% mutate(Treatment= str_replace(Treatment,'CONTROL', 'Ancestral'))

View(Caenorhabditis_Vm_mutational_median_effects_SS_summary)

write_xlsx(Caenorhabditis_Vm_mutational_median_effects_SS_summary, "Caenorhabditis_Vm_Mutational_Median_effects-Mutational_Bias_SS_summary.xlsx")


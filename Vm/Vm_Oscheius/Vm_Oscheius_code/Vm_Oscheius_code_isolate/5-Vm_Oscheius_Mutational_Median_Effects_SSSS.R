#Mutational Median effects - Mutational Bias_SSSS
library(emmeans)

#CEW1_Vm_ Mutational Median effects - Mutational Bias ----
{
  ##CEW1_Vm_P3p_SSSS Mutation Median effects - Mutational Bias----
  {
    CEW1_Vm_P3p_SSSS_mod_e_Treatment<- emmeans(CEW1_Vm_P3p_SSSS_mod, ~Treatment, data=Vm_CEW1_data)
    CEW1_Vm_P3p_SSSS_mod_e_Treatment_hpd0.95 <- as.data.frame(CEW1_Vm_P3p_SSSS_mod_e_Treatment)
    colnames(CEW1_Vm_P3p_SSSS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    CEW1_Vm_P3p_SSSS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(CEW1_Vm_P3p_SSSS_mod_e_Treatment, prob = 0.83))
    colnames(CEW1_Vm_P3p_SSSS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    CEW1_Vm_P3p_SSSS_mod_Treatment_liab <- cbind.data.frame(CEW1_Vm_P3p_SSSS_mod_e_Treatment_hpd0.95,CEW1_Vm_P3p_SSSS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    CEW1_Vm_P3p_SSSS_mod_Treatment_liab$Ancestor <- c("CEW1","CEW1")
    CEW1_Vm_P3p_SSSS_mod_Treatment_liab$Pnp <- c("P3.p","P3.p")
    CEW1_Vm_P3p_SSSS_mod_Treatment_liab$Pnp_fate <- c("SSSS","SSSS")
    CEW1_Vm_P3p_SSSS_mod_Treatment_liab$Scale <- c("liab","liab")
    CEW1_Vm_P3p_SSSS_mod_Treatment_liab
    
    CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(CEW1_Vm_P3p_SSSS_mod_Treatment_liab[,2]))
    colnames(CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale_median) <- c("median")
    CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(CEW1_Vm_P3p_SSSS_mod_Treatment_liab[,3]))
    colnames(CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(CEW1_Vm_P3p_SSSS_mod_Treatment_liab[,4]))
    colnames(CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(CEW1_Vm_P3p_SSSS_mod_Treatment_liab[,5]))
    colnames(CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(CEW1_Vm_P3p_SSSS_mod_Treatment_liab[,6]))
    colnames(CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale) <- c("Treatment")
    CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale <- cbind.data.frame(CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale,CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale_median,CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95,CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95,CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83,CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 )
    CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale$Ancestor <- c("CEW1","CEW1")
    CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale$Pnp <- c("P3.p","P3.p")
    CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale$Pnp_fate <- c("SSSS","SSSS")
    CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    CEW1_Vm_P3p_SSSS_mod_Treatment_liab_DataScale <- rbind.data.frame(CEW1_Vm_P3p_SSSS_mod_Treatment_liab, CEW1_Vm_P3p_SSSS_mod_Treatment_DataScale)
    CEW1_Vm_P3p_SSSS_mod_Treatment_liab_DataScale
    
  }
  
  ##CEW1_Vm_P4p_SSSS Mutation Median effects - Mutational Bias----
  {
    CEW1_Vm_P4p_SSSS_mod_e_Treatment<- emmeans(CEW1_Vm_P4p_SSSS_mod, ~Treatment, data=Vm_CEW1_data)
    CEW1_Vm_P4p_SSSS_mod_e_Treatment_hpd0.95 <- as.data.frame(CEW1_Vm_P4p_SSSS_mod_e_Treatment)
    colnames(CEW1_Vm_P4p_SSSS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    CEW1_Vm_P4p_SSSS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(CEW1_Vm_P4p_SSSS_mod_e_Treatment, prob = 0.83))
    colnames(CEW1_Vm_P4p_SSSS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    CEW1_Vm_P4p_SSSS_mod_Treatment_liab <- cbind.data.frame(CEW1_Vm_P4p_SSSS_mod_e_Treatment_hpd0.95,CEW1_Vm_P4p_SSSS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    CEW1_Vm_P4p_SSSS_mod_Treatment_liab$Ancestor <- c("CEW1","CEW1")
    CEW1_Vm_P4p_SSSS_mod_Treatment_liab$Pnp <- c("P4.p","P4.p")
    CEW1_Vm_P4p_SSSS_mod_Treatment_liab$Pnp_fate <- c("SSSS","SSSS")
    CEW1_Vm_P4p_SSSS_mod_Treatment_liab$Scale <- c("liab","liab")
    CEW1_Vm_P4p_SSSS_mod_Treatment_liab
    
    CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(CEW1_Vm_P4p_SSSS_mod_Treatment_liab[,2]))
    colnames(CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale_median) <- c("median")
    CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(CEW1_Vm_P4p_SSSS_mod_Treatment_liab[,3]))
    colnames(CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(CEW1_Vm_P4p_SSSS_mod_Treatment_liab[,4]))
    colnames(CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(CEW1_Vm_P4p_SSSS_mod_Treatment_liab[,5]))
    colnames(CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(CEW1_Vm_P4p_SSSS_mod_Treatment_liab[,6]))
    colnames(CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale) <- c("Treatment")
    CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale <- cbind.data.frame(CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale,CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale_median,CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95,CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95,CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83,CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 )
    CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale$Ancestor <- c("CEW1","CEW1")
    CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale$Pnp <- c("P4.p","P4.p")
    CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale$Pnp_fate <- c("SSSS","SSSS")
    CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    CEW1_Vm_P4p_SSSS_mod_Treatment_liab_DataScale <- rbind.data.frame(CEW1_Vm_P4p_SSSS_mod_Treatment_liab, CEW1_Vm_P4p_SSSS_mod_Treatment_DataScale)
    CEW1_Vm_P4p_SSSS_mod_Treatment_liab_DataScale
    
  }
  
  ##CEW1_Vm_P8p_SSSS Mutation Median effects - Mutational Bias----
  {
    CEW1_Vm_P8p_SSSS_mod_e_Treatment<- emmeans(CEW1_Vm_P8p_SSSS_mod, ~Treatment, data=Vm_CEW1_data)
    CEW1_Vm_P8p_SSSS_mod_e_Treatment_hpd0.95 <- as.data.frame(CEW1_Vm_P8p_SSSS_mod_e_Treatment)
    colnames(CEW1_Vm_P8p_SSSS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    CEW1_Vm_P8p_SSSS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(CEW1_Vm_P8p_SSSS_mod_e_Treatment, prob = 0.83))
    colnames(CEW1_Vm_P8p_SSSS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    CEW1_Vm_P8p_SSSS_mod_Treatment_liab <- cbind.data.frame(CEW1_Vm_P8p_SSSS_mod_e_Treatment_hpd0.95,CEW1_Vm_P8p_SSSS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    CEW1_Vm_P8p_SSSS_mod_Treatment_liab$Ancestor <- c("CEW1","CEW1")
    CEW1_Vm_P8p_SSSS_mod_Treatment_liab$Pnp <- c("P8.p","P8.p")
    CEW1_Vm_P8p_SSSS_mod_Treatment_liab$Pnp_fate <- c("SSSS","SSSS")
    CEW1_Vm_P8p_SSSS_mod_Treatment_liab$Scale <- c("liab","liab")
    CEW1_Vm_P8p_SSSS_mod_Treatment_liab
    
    CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(CEW1_Vm_P8p_SSSS_mod_Treatment_liab[,2]))
    colnames(CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale_median) <- c("median")
    CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(CEW1_Vm_P8p_SSSS_mod_Treatment_liab[,3]))
    colnames(CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(CEW1_Vm_P8p_SSSS_mod_Treatment_liab[,4]))
    colnames(CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(CEW1_Vm_P8p_SSSS_mod_Treatment_liab[,5]))
    colnames(CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(CEW1_Vm_P8p_SSSS_mod_Treatment_liab[,6]))
    colnames(CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale) <- c("Treatment")
    CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale <- cbind.data.frame(CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale,CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale_median,CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95,CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95,CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83,CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 )
    CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale$Ancestor <- c("CEW1","CEW1")
    CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale$Pnp <- c("P8.p","P8.p")
    CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale$Pnp_fate <- c("SSSS","SSSS")
    CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    CEW1_Vm_P8p_SSSS_mod_Treatment_liab_DataScale <- rbind.data.frame(CEW1_Vm_P8p_SSSS_mod_Treatment_liab, CEW1_Vm_P8p_SSSS_mod_Treatment_DataScale)
    CEW1_Vm_P8p_SSSS_mod_Treatment_liab_DataScale
    
  }
  
  ##CEW1_Vm_P5p_wt Mutation Median effects - Mutational Bias----
  {
    CEW1_Vm_P5p_wt_mod_e_Treatment<- emmeans(CEW1_Vm_P5p_wt_mod, ~Treatment, data=Vm_CEW1_data)
    CEW1_Vm_P5p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(CEW1_Vm_P5p_wt_mod_e_Treatment)
    colnames(CEW1_Vm_P5p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    CEW1_Vm_P5p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(CEW1_Vm_P5p_wt_mod_e_Treatment, prob = 0.83))
    colnames(CEW1_Vm_P5p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    CEW1_Vm_P5p_wt_mod_Treatment_liab <- cbind.data.frame(CEW1_Vm_P5p_wt_mod_e_Treatment_hpd0.95,CEW1_Vm_P5p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    CEW1_Vm_P5p_wt_mod_Treatment_liab$Ancestor <- c("CEW1","CEW1")
    CEW1_Vm_P5p_wt_mod_Treatment_liab$Pnp <- c("P5.p","P5.p")
    CEW1_Vm_P5p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    CEW1_Vm_P5p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    CEW1_Vm_P5p_wt_mod_Treatment_liab
    
    CEW1_Vm_P5p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(CEW1_Vm_P5p_wt_mod_Treatment_liab[,2]))
    colnames(CEW1_Vm_P5p_wt_mod_Treatment_DataScale_median) <- c("median")
    CEW1_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(CEW1_Vm_P5p_wt_mod_Treatment_liab[,3]))
    colnames(CEW1_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    CEW1_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(CEW1_Vm_P5p_wt_mod_Treatment_liab[,4]))
    colnames(CEW1_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    CEW1_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(CEW1_Vm_P5p_wt_mod_Treatment_liab[,5]))
    colnames(CEW1_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    CEW1_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(CEW1_Vm_P5p_wt_mod_Treatment_liab[,6]))
    colnames(CEW1_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    CEW1_Vm_P5p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(CEW1_Vm_P5p_wt_mod_Treatment_DataScale) <- c("Treatment")
    CEW1_Vm_P5p_wt_mod_Treatment_DataScale <- cbind.data.frame(CEW1_Vm_P5p_wt_mod_Treatment_DataScale,CEW1_Vm_P5p_wt_mod_Treatment_DataScale_median,CEW1_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95,CEW1_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95,CEW1_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83,CEW1_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    CEW1_Vm_P5p_wt_mod_Treatment_DataScale$Ancestor <- c("CEW1","CEW1")
    CEW1_Vm_P5p_wt_mod_Treatment_DataScale$Pnp <- c("P5.p","P5.p")
    CEW1_Vm_P5p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    CEW1_Vm_P5p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    CEW1_Vm_P5p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(CEW1_Vm_P5p_wt_mod_Treatment_liab, CEW1_Vm_P5p_wt_mod_Treatment_DataScale)
    CEW1_Vm_P5p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##CEW1_Vm_P6p_wt Mutation Median effects - Mutational Bias----
  {
    CEW1_Vm_P6p_wt_mod_e_Treatment<- emmeans(CEW1_Vm_P6p_wt_mod, ~Treatment, data=Vm_CEW1_data)
    CEW1_Vm_P6p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(CEW1_Vm_P6p_wt_mod_e_Treatment)
    colnames(CEW1_Vm_P6p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    CEW1_Vm_P6p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(CEW1_Vm_P6p_wt_mod_e_Treatment, prob = 0.83))
    colnames(CEW1_Vm_P6p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    CEW1_Vm_P6p_wt_mod_Treatment_liab <- cbind.data.frame(CEW1_Vm_P6p_wt_mod_e_Treatment_hpd0.95,CEW1_Vm_P6p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    CEW1_Vm_P6p_wt_mod_Treatment_liab$Ancestor <- c("CEW1","CEW1")
    CEW1_Vm_P6p_wt_mod_Treatment_liab$Pnp <- c("P6.p","P6.p")
    CEW1_Vm_P6p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    CEW1_Vm_P6p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    CEW1_Vm_P6p_wt_mod_Treatment_liab
    
    CEW1_Vm_P6p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(CEW1_Vm_P6p_wt_mod_Treatment_liab[,2]))
    colnames(CEW1_Vm_P6p_wt_mod_Treatment_DataScale_median) <- c("median")
    CEW1_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(CEW1_Vm_P6p_wt_mod_Treatment_liab[,3]))
    colnames(CEW1_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    CEW1_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(CEW1_Vm_P6p_wt_mod_Treatment_liab[,4]))
    colnames(CEW1_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    CEW1_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(CEW1_Vm_P6p_wt_mod_Treatment_liab[,5]))
    colnames(CEW1_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    CEW1_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(CEW1_Vm_P6p_wt_mod_Treatment_liab[,6]))
    colnames(CEW1_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    CEW1_Vm_P6p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(CEW1_Vm_P6p_wt_mod_Treatment_DataScale) <- c("Treatment")
    CEW1_Vm_P6p_wt_mod_Treatment_DataScale <- cbind.data.frame(CEW1_Vm_P6p_wt_mod_Treatment_DataScale,CEW1_Vm_P6p_wt_mod_Treatment_DataScale_median,CEW1_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95,CEW1_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95,CEW1_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83,CEW1_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    CEW1_Vm_P6p_wt_mod_Treatment_DataScale$Ancestor <- c("CEW1","CEW1")
    CEW1_Vm_P6p_wt_mod_Treatment_DataScale$Pnp <- c("P6.p","P6.p")
    CEW1_Vm_P6p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    CEW1_Vm_P6p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    CEW1_Vm_P6p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(CEW1_Vm_P6p_wt_mod_Treatment_liab, CEW1_Vm_P6p_wt_mod_Treatment_DataScale)
    CEW1_Vm_P6p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##CEW1_Vm_P7p_wt Mutation Median effects - Mutational Bias----
  {
    CEW1_Vm_P7p_wt_mod_e_Treatment<- emmeans(CEW1_Vm_P7p_wt_mod, ~Treatment, data=Vm_CEW1_data)
    CEW1_Vm_P7p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(CEW1_Vm_P7p_wt_mod_e_Treatment)
    colnames(CEW1_Vm_P7p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    CEW1_Vm_P7p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(CEW1_Vm_P7p_wt_mod_e_Treatment, prob = 0.83))
    colnames(CEW1_Vm_P7p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    CEW1_Vm_P7p_wt_mod_Treatment_liab <- cbind.data.frame(CEW1_Vm_P7p_wt_mod_e_Treatment_hpd0.95,CEW1_Vm_P7p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    CEW1_Vm_P7p_wt_mod_Treatment_liab$Ancestor <- c("CEW1","CEW1")
    CEW1_Vm_P7p_wt_mod_Treatment_liab$Pnp <- c("P7.p","P7.p")
    CEW1_Vm_P7p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    CEW1_Vm_P7p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    CEW1_Vm_P7p_wt_mod_Treatment_liab
    
    CEW1_Vm_P7p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(CEW1_Vm_P7p_wt_mod_Treatment_liab[,2]))
    colnames(CEW1_Vm_P7p_wt_mod_Treatment_DataScale_median) <- c("median")
    CEW1_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(CEW1_Vm_P7p_wt_mod_Treatment_liab[,3]))
    colnames(CEW1_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    CEW1_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(CEW1_Vm_P7p_wt_mod_Treatment_liab[,4]))
    colnames(CEW1_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    CEW1_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(CEW1_Vm_P7p_wt_mod_Treatment_liab[,5]))
    colnames(CEW1_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    CEW1_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(CEW1_Vm_P7p_wt_mod_Treatment_liab[,6]))
    colnames(CEW1_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    CEW1_Vm_P7p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(CEW1_Vm_P7p_wt_mod_Treatment_DataScale) <- c("Treatment")
    CEW1_Vm_P7p_wt_mod_Treatment_DataScale <- cbind.data.frame(CEW1_Vm_P7p_wt_mod_Treatment_DataScale,CEW1_Vm_P7p_wt_mod_Treatment_DataScale_median,CEW1_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95,CEW1_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95,CEW1_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83,CEW1_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    CEW1_Vm_P7p_wt_mod_Treatment_DataScale$Ancestor <- c("CEW1","CEW1")
    CEW1_Vm_P7p_wt_mod_Treatment_DataScale$Pnp <- c("P7.p","P7.p")
    CEW1_Vm_P7p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    CEW1_Vm_P7p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    CEW1_Vm_P7p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(CEW1_Vm_P7p_wt_mod_Treatment_liab, CEW1_Vm_P7p_wt_mod_Treatment_DataScale)
    CEW1_Vm_P7p_wt_mod_Treatment_liab_DataScale
    
  }
  ##----
  CEW1_Vm_mod_Treatment_liab_DataScale_SSSS <- rbind.data.frame(CEW1_Vm_P3p_SSSS_mod_Treatment_liab_DataScale,CEW1_Vm_P4p_SSSS_mod_Treatment_liab_DataScale,CEW1_Vm_P5p_wt_mod_Treatment_liab_DataScale,CEW1_Vm_P6p_wt_mod_Treatment_liab_DataScale,CEW1_Vm_P7p_wt_mod_Treatment_liab_DataScale, CEW1_Vm_P8p_SSSS_mod_Treatment_liab_DataScale)
  CEW1_Vm_mod_Treatment_liab_DataScale_SSSS$Species <- rep("O.tipulae",24)
  CEW1_Vm_mod_Treatment_liab_DataScale_SSSS$Genus <- rep("Oscheius",24)
  
  View(CEW1_Vm_mod_Treatment_liab_DataScale_SSSS)
  
}


#JU178_Vm_ Mutational Median effects - Mutational Bias ----
{
  ##JU178_Vm_P3p_SSSS Mutation Median effects - Mutational Bias----
  {
    JU178_Vm_P3p_SSSS_mod_e_Treatment<- emmeans(JU178_Vm_P3p_SSSS_mod, ~Treatment, data=Vm_JU178_data)
    JU178_Vm_P3p_SSSS_mod_e_Treatment_hpd0.95 <- as.data.frame(JU178_Vm_P3p_SSSS_mod_e_Treatment)
    colnames(JU178_Vm_P3p_SSSS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU178_Vm_P3p_SSSS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU178_Vm_P3p_SSSS_mod_e_Treatment, prob = 0.83))
    colnames(JU178_Vm_P3p_SSSS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU178_Vm_P3p_SSSS_mod_Treatment_liab <- cbind.data.frame(JU178_Vm_P3p_SSSS_mod_e_Treatment_hpd0.95,JU178_Vm_P3p_SSSS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU178_Vm_P3p_SSSS_mod_Treatment_liab$Ancestor <- c("JU178","JU178")
    JU178_Vm_P3p_SSSS_mod_Treatment_liab$Pnp <- c("P3.p","P3.p")
    JU178_Vm_P3p_SSSS_mod_Treatment_liab$Pnp_fate <- c("SSSS","SSSS")
    JU178_Vm_P3p_SSSS_mod_Treatment_liab$Scale <- c("liab","liab")
    JU178_Vm_P3p_SSSS_mod_Treatment_liab
    
    JU178_Vm_P3p_SSSS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU178_Vm_P3p_SSSS_mod_Treatment_liab[,2]))
    colnames(JU178_Vm_P3p_SSSS_mod_Treatment_DataScale_median) <- c("median")
    JU178_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU178_Vm_P3p_SSSS_mod_Treatment_liab[,3]))
    colnames(JU178_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU178_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU178_Vm_P3p_SSSS_mod_Treatment_liab[,4]))
    colnames(JU178_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU178_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU178_Vm_P3p_SSSS_mod_Treatment_liab[,5]))
    colnames(JU178_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU178_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU178_Vm_P3p_SSSS_mod_Treatment_liab[,6]))
    colnames(JU178_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU178_Vm_P3p_SSSS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU178_Vm_P3p_SSSS_mod_Treatment_DataScale) <- c("Treatment")
    JU178_Vm_P3p_SSSS_mod_Treatment_DataScale <- cbind.data.frame(JU178_Vm_P3p_SSSS_mod_Treatment_DataScale,JU178_Vm_P3p_SSSS_mod_Treatment_DataScale_median,JU178_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95,JU178_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95,JU178_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83,JU178_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU178_Vm_P3p_SSSS_mod_Treatment_DataScale$Ancestor <- c("JU178","JU178")
    JU178_Vm_P3p_SSSS_mod_Treatment_DataScale$Pnp <- c("P3.p","P3.p")
    JU178_Vm_P3p_SSSS_mod_Treatment_DataScale$Pnp_fate <- c("SSSS","SSSS")
    JU178_Vm_P3p_SSSS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU178_Vm_P3p_SSSS_mod_Treatment_liab_DataScale <- rbind.data.frame(JU178_Vm_P3p_SSSS_mod_Treatment_liab, JU178_Vm_P3p_SSSS_mod_Treatment_DataScale)
    JU178_Vm_P3p_SSSS_mod_Treatment_liab_DataScale
    
  }
  
  ##JU178_Vm_P4p_SSSS Mutation Median effects - Mutational Bias----
  {
    JU178_Vm_P4p_SSSS_mod_e_Treatment<- emmeans(JU178_Vm_P4p_SSSS_mod, ~Treatment, data=Vm_JU178_data)
    JU178_Vm_P4p_SSSS_mod_e_Treatment_hpd0.95 <- as.data.frame(JU178_Vm_P4p_SSSS_mod_e_Treatment)
    colnames(JU178_Vm_P4p_SSSS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU178_Vm_P4p_SSSS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU178_Vm_P4p_SSSS_mod_e_Treatment, prob = 0.83))
    colnames(JU178_Vm_P4p_SSSS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU178_Vm_P4p_SSSS_mod_Treatment_liab <- cbind.data.frame(JU178_Vm_P4p_SSSS_mod_e_Treatment_hpd0.95,JU178_Vm_P4p_SSSS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU178_Vm_P4p_SSSS_mod_Treatment_liab$Ancestor <- c("JU178","JU178")
    JU178_Vm_P4p_SSSS_mod_Treatment_liab$Pnp <- c("P4.p","P4.p")
    JU178_Vm_P4p_SSSS_mod_Treatment_liab$Pnp_fate <- c("SSSS","SSSS")
    JU178_Vm_P4p_SSSS_mod_Treatment_liab$Scale <- c("liab","liab")
    JU178_Vm_P4p_SSSS_mod_Treatment_liab
    
    JU178_Vm_P4p_SSSS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU178_Vm_P4p_SSSS_mod_Treatment_liab[,2]))
    colnames(JU178_Vm_P4p_SSSS_mod_Treatment_DataScale_median) <- c("median")
    JU178_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU178_Vm_P4p_SSSS_mod_Treatment_liab[,3]))
    colnames(JU178_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU178_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU178_Vm_P4p_SSSS_mod_Treatment_liab[,4]))
    colnames(JU178_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU178_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU178_Vm_P4p_SSSS_mod_Treatment_liab[,5]))
    colnames(JU178_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU178_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU178_Vm_P4p_SSSS_mod_Treatment_liab[,6]))
    colnames(JU178_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU178_Vm_P4p_SSSS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU178_Vm_P4p_SSSS_mod_Treatment_DataScale) <- c("Treatment")
    JU178_Vm_P4p_SSSS_mod_Treatment_DataScale <- cbind.data.frame(JU178_Vm_P4p_SSSS_mod_Treatment_DataScale,JU178_Vm_P4p_SSSS_mod_Treatment_DataScale_median,JU178_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95,JU178_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95,JU178_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83,JU178_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU178_Vm_P4p_SSSS_mod_Treatment_DataScale$Ancestor <- c("JU178","JU178")
    JU178_Vm_P4p_SSSS_mod_Treatment_DataScale$Pnp <- c("P4.p","P4.p")
    JU178_Vm_P4p_SSSS_mod_Treatment_DataScale$Pnp_fate <- c("SSSS","SSSS")
    JU178_Vm_P4p_SSSS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU178_Vm_P4p_SSSS_mod_Treatment_liab_DataScale <- rbind.data.frame(JU178_Vm_P4p_SSSS_mod_Treatment_liab, JU178_Vm_P4p_SSSS_mod_Treatment_DataScale)
    JU178_Vm_P4p_SSSS_mod_Treatment_liab_DataScale
    
  }
  
  ##JU178_Vm_P8p_SSSS Mutation Median effects - Mutational Bias----
  {
    JU178_Vm_P8p_SSSS_mod_e_Treatment<- emmeans(JU178_Vm_P8p_SSSS_mod, ~Treatment, data=Vm_JU178_data)
    JU178_Vm_P8p_SSSS_mod_e_Treatment_hpd0.95 <- as.data.frame(JU178_Vm_P8p_SSSS_mod_e_Treatment)
    colnames(JU178_Vm_P8p_SSSS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU178_Vm_P8p_SSSS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU178_Vm_P8p_SSSS_mod_e_Treatment, prob = 0.83))
    colnames(JU178_Vm_P8p_SSSS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU178_Vm_P8p_SSSS_mod_Treatment_liab <- cbind.data.frame(JU178_Vm_P8p_SSSS_mod_e_Treatment_hpd0.95,JU178_Vm_P8p_SSSS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU178_Vm_P8p_SSSS_mod_Treatment_liab$Ancestor <- c("JU178","JU178")
    JU178_Vm_P8p_SSSS_mod_Treatment_liab$Pnp <- c("P8.p","P8.p")
    JU178_Vm_P8p_SSSS_mod_Treatment_liab$Pnp_fate <- c("SSSS","SSSS")
    JU178_Vm_P8p_SSSS_mod_Treatment_liab$Scale <- c("liab","liab")
    JU178_Vm_P8p_SSSS_mod_Treatment_liab
    
    JU178_Vm_P8p_SSSS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU178_Vm_P8p_SSSS_mod_Treatment_liab[,2]))
    colnames(JU178_Vm_P8p_SSSS_mod_Treatment_DataScale_median) <- c("median")
    JU178_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU178_Vm_P8p_SSSS_mod_Treatment_liab[,3]))
    colnames(JU178_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU178_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU178_Vm_P8p_SSSS_mod_Treatment_liab[,4]))
    colnames(JU178_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU178_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU178_Vm_P8p_SSSS_mod_Treatment_liab[,5]))
    colnames(JU178_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU178_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU178_Vm_P8p_SSSS_mod_Treatment_liab[,6]))
    colnames(JU178_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU178_Vm_P8p_SSSS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU178_Vm_P8p_SSSS_mod_Treatment_DataScale) <- c("Treatment")
    JU178_Vm_P8p_SSSS_mod_Treatment_DataScale <- cbind.data.frame(JU178_Vm_P8p_SSSS_mod_Treatment_DataScale,JU178_Vm_P8p_SSSS_mod_Treatment_DataScale_median,JU178_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95,JU178_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95,JU178_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83,JU178_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU178_Vm_P8p_SSSS_mod_Treatment_DataScale$Ancestor <- c("JU178","JU178")
    JU178_Vm_P8p_SSSS_mod_Treatment_DataScale$Pnp <- c("P8.p","P8.p")
    JU178_Vm_P8p_SSSS_mod_Treatment_DataScale$Pnp_fate <- c("SSSS","SSSS")
    JU178_Vm_P8p_SSSS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU178_Vm_P8p_SSSS_mod_Treatment_liab_DataScale <- rbind.data.frame(JU178_Vm_P8p_SSSS_mod_Treatment_liab, JU178_Vm_P8p_SSSS_mod_Treatment_DataScale)
    JU178_Vm_P8p_SSSS_mod_Treatment_liab_DataScale
    
  }
  
  ##JU178_Vm_P5p_wt Mutation Median effects - Mutational Bias----
  {
    JU178_Vm_P5p_wt_mod_e_Treatment<- emmeans(JU178_Vm_P5p_wt_mod, ~Treatment, data=Vm_JU178_data)
    JU178_Vm_P5p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(JU178_Vm_P5p_wt_mod_e_Treatment)
    colnames(JU178_Vm_P5p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU178_Vm_P5p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU178_Vm_P5p_wt_mod_e_Treatment, prob = 0.83))
    colnames(JU178_Vm_P5p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU178_Vm_P5p_wt_mod_Treatment_liab <- cbind.data.frame(JU178_Vm_P5p_wt_mod_e_Treatment_hpd0.95,JU178_Vm_P5p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU178_Vm_P5p_wt_mod_Treatment_liab$Ancestor <- c("JU178","JU178")
    JU178_Vm_P5p_wt_mod_Treatment_liab$Pnp <- c("P5.p","P5.p")
    JU178_Vm_P5p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    JU178_Vm_P5p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    JU178_Vm_P5p_wt_mod_Treatment_liab
    
    JU178_Vm_P5p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU178_Vm_P5p_wt_mod_Treatment_liab[,2]))
    colnames(JU178_Vm_P5p_wt_mod_Treatment_DataScale_median) <- c("median")
    JU178_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU178_Vm_P5p_wt_mod_Treatment_liab[,3]))
    colnames(JU178_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU178_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU178_Vm_P5p_wt_mod_Treatment_liab[,4]))
    colnames(JU178_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU178_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU178_Vm_P5p_wt_mod_Treatment_liab[,5]))
    colnames(JU178_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU178_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU178_Vm_P5p_wt_mod_Treatment_liab[,6]))
    colnames(JU178_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU178_Vm_P5p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU178_Vm_P5p_wt_mod_Treatment_DataScale) <- c("Treatment")
    JU178_Vm_P5p_wt_mod_Treatment_DataScale <- cbind.data.frame(JU178_Vm_P5p_wt_mod_Treatment_DataScale,JU178_Vm_P5p_wt_mod_Treatment_DataScale_median,JU178_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95,JU178_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95,JU178_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83,JU178_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU178_Vm_P5p_wt_mod_Treatment_DataScale$Ancestor <- c("JU178","JU178")
    JU178_Vm_P5p_wt_mod_Treatment_DataScale$Pnp <- c("P5.p","P5.p")
    JU178_Vm_P5p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    JU178_Vm_P5p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU178_Vm_P5p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(JU178_Vm_P5p_wt_mod_Treatment_liab, JU178_Vm_P5p_wt_mod_Treatment_DataScale)
    JU178_Vm_P5p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##JU178_Vm_P6p_wt Mutation Median effects - Mutational Bias----
  {
    JU178_Vm_P6p_wt_mod_e_Treatment<- emmeans(JU178_Vm_P6p_wt_mod, ~Treatment, data=Vm_JU178_data)
    JU178_Vm_P6p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(JU178_Vm_P6p_wt_mod_e_Treatment)
    colnames(JU178_Vm_P6p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU178_Vm_P6p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU178_Vm_P6p_wt_mod_e_Treatment, prob = 0.83))
    colnames(JU178_Vm_P6p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU178_Vm_P6p_wt_mod_Treatment_liab <- cbind.data.frame(JU178_Vm_P6p_wt_mod_e_Treatment_hpd0.95,JU178_Vm_P6p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU178_Vm_P6p_wt_mod_Treatment_liab$Ancestor <- c("JU178","JU178")
    JU178_Vm_P6p_wt_mod_Treatment_liab$Pnp <- c("P6.p","P6.p")
    JU178_Vm_P6p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    JU178_Vm_P6p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    JU178_Vm_P6p_wt_mod_Treatment_liab
    
    JU178_Vm_P6p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU178_Vm_P6p_wt_mod_Treatment_liab[,2]))
    colnames(JU178_Vm_P6p_wt_mod_Treatment_DataScale_median) <- c("median")
    JU178_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU178_Vm_P6p_wt_mod_Treatment_liab[,3]))
    colnames(JU178_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU178_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU178_Vm_P6p_wt_mod_Treatment_liab[,4]))
    colnames(JU178_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU178_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU178_Vm_P6p_wt_mod_Treatment_liab[,5]))
    colnames(JU178_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU178_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU178_Vm_P6p_wt_mod_Treatment_liab[,6]))
    colnames(JU178_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU178_Vm_P6p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU178_Vm_P6p_wt_mod_Treatment_DataScale) <- c("Treatment")
    JU178_Vm_P6p_wt_mod_Treatment_DataScale <- cbind.data.frame(JU178_Vm_P6p_wt_mod_Treatment_DataScale,JU178_Vm_P6p_wt_mod_Treatment_DataScale_median,JU178_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95,JU178_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95,JU178_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83,JU178_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU178_Vm_P6p_wt_mod_Treatment_DataScale$Ancestor <- c("JU178","JU178")
    JU178_Vm_P6p_wt_mod_Treatment_DataScale$Pnp <- c("P6.p","P6.p")
    JU178_Vm_P6p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    JU178_Vm_P6p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU178_Vm_P6p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(JU178_Vm_P6p_wt_mod_Treatment_liab, JU178_Vm_P6p_wt_mod_Treatment_DataScale)
    JU178_Vm_P6p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##JU178_Vm_P7p_wt Mutation Median effects - Mutational Bias----
  {
    JU178_Vm_P7p_wt_mod_e_Treatment<- emmeans(JU178_Vm_P7p_wt_mod, ~Treatment, data=Vm_JU178_data)
    JU178_Vm_P7p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(JU178_Vm_P7p_wt_mod_e_Treatment)
    colnames(JU178_Vm_P7p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU178_Vm_P7p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU178_Vm_P7p_wt_mod_e_Treatment, prob = 0.83))
    colnames(JU178_Vm_P7p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU178_Vm_P7p_wt_mod_Treatment_liab <- cbind.data.frame(JU178_Vm_P7p_wt_mod_e_Treatment_hpd0.95,JU178_Vm_P7p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU178_Vm_P7p_wt_mod_Treatment_liab$Ancestor <- c("JU178","JU178")
    JU178_Vm_P7p_wt_mod_Treatment_liab$Pnp <- c("P7.p","P7.p")
    JU178_Vm_P7p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    JU178_Vm_P7p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    JU178_Vm_P7p_wt_mod_Treatment_liab
    
    JU178_Vm_P7p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU178_Vm_P7p_wt_mod_Treatment_liab[,2]))
    colnames(JU178_Vm_P7p_wt_mod_Treatment_DataScale_median) <- c("median")
    JU178_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU178_Vm_P7p_wt_mod_Treatment_liab[,3]))
    colnames(JU178_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU178_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU178_Vm_P7p_wt_mod_Treatment_liab[,4]))
    colnames(JU178_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU178_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU178_Vm_P7p_wt_mod_Treatment_liab[,5]))
    colnames(JU178_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU178_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU178_Vm_P7p_wt_mod_Treatment_liab[,6]))
    colnames(JU178_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU178_Vm_P7p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU178_Vm_P7p_wt_mod_Treatment_DataScale) <- c("Treatment")
    JU178_Vm_P7p_wt_mod_Treatment_DataScale <- cbind.data.frame(JU178_Vm_P7p_wt_mod_Treatment_DataScale,JU178_Vm_P7p_wt_mod_Treatment_DataScale_median,JU178_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95,JU178_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95,JU178_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83,JU178_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU178_Vm_P7p_wt_mod_Treatment_DataScale$Ancestor <- c("JU178","JU178")
    JU178_Vm_P7p_wt_mod_Treatment_DataScale$Pnp <- c("P7.p","P7.p")
    JU178_Vm_P7p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    JU178_Vm_P7p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU178_Vm_P7p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(JU178_Vm_P7p_wt_mod_Treatment_liab, JU178_Vm_P7p_wt_mod_Treatment_DataScale)
    JU178_Vm_P7p_wt_mod_Treatment_liab_DataScale
    
  }
  ##----
  JU178_Vm_mod_Treatment_liab_DataScale_SSSS <- rbind.data.frame(JU178_Vm_P3p_SSSS_mod_Treatment_liab_DataScale,JU178_Vm_P4p_SSSS_mod_Treatment_liab_DataScale,JU178_Vm_P5p_wt_mod_Treatment_liab_DataScale,JU178_Vm_P6p_wt_mod_Treatment_liab_DataScale,JU178_Vm_P7p_wt_mod_Treatment_liab_DataScale, JU178_Vm_P8p_SSSS_mod_Treatment_liab_DataScale)
  JU178_Vm_mod_Treatment_liab_DataScale_SSSS$Species <- rep("O.tipulae",24)
  JU178_Vm_mod_Treatment_liab_DataScale_SSSS$Genus <- rep("Oscheius",24)
  
  View(JU178_Vm_mod_Treatment_liab_DataScale_SSSS)
  
}

#PS2068_Vm_ Mutational Median effects - Mutational Bias ----
{
  ##PS2068_Vm_P3p_SSSS Mutation Median effects - Mutational Bias----
  {
    PS2068_Vm_P3p_SSSS_mod_e_Treatment<- emmeans(PS2068_Vm_P3p_SSSS_mod, ~Treatment, data=Vm_PS2068_data)
    PS2068_Vm_P3p_SSSS_mod_e_Treatment_hpd0.95 <- as.data.frame(PS2068_Vm_P3p_SSSS_mod_e_Treatment)
    colnames(PS2068_Vm_P3p_SSSS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PS2068_Vm_P3p_SSSS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PS2068_Vm_P3p_SSSS_mod_e_Treatment, prob = 0.83))
    colnames(PS2068_Vm_P3p_SSSS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PS2068_Vm_P3p_SSSS_mod_Treatment_liab <- cbind.data.frame(PS2068_Vm_P3p_SSSS_mod_e_Treatment_hpd0.95,PS2068_Vm_P3p_SSSS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PS2068_Vm_P3p_SSSS_mod_Treatment_liab$Ancestor <- c("PS2068","PS2068")
    PS2068_Vm_P3p_SSSS_mod_Treatment_liab$Pnp <- c("P3.p","P3.p")
    PS2068_Vm_P3p_SSSS_mod_Treatment_liab$Pnp_fate <- c("SSSS","SSSS")
    PS2068_Vm_P3p_SSSS_mod_Treatment_liab$Scale <- c("liab","liab")
    PS2068_Vm_P3p_SSSS_mod_Treatment_liab
    
    PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PS2068_Vm_P3p_SSSS_mod_Treatment_liab[,2]))
    colnames(PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale_median) <- c("median")
    PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PS2068_Vm_P3p_SSSS_mod_Treatment_liab[,3]))
    colnames(PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PS2068_Vm_P3p_SSSS_mod_Treatment_liab[,4]))
    colnames(PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PS2068_Vm_P3p_SSSS_mod_Treatment_liab[,5]))
    colnames(PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PS2068_Vm_P3p_SSSS_mod_Treatment_liab[,6]))
    colnames(PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale) <- c("Treatment")
    PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale <- cbind.data.frame(PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale,PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale_median,PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95,PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95,PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83,PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 )
    PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale$Ancestor <- c("PS2068","PS2068")
    PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale$Pnp <- c("P3.p","P3.p")
    PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale$Pnp_fate <- c("SSSS","SSSS")
    PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PS2068_Vm_P3p_SSSS_mod_Treatment_liab_DataScale <- rbind.data.frame(PS2068_Vm_P3p_SSSS_mod_Treatment_liab, PS2068_Vm_P3p_SSSS_mod_Treatment_DataScale)
    PS2068_Vm_P3p_SSSS_mod_Treatment_liab_DataScale
    
  }
  
  ##PS2068_Vm_P4p_SSSS Mutation Median effects - Mutational Bias----
  {
    PS2068_Vm_P4p_SSSS_mod_e_Treatment<- emmeans(PS2068_Vm_P4p_SSSS_mod, ~Treatment, data=Vm_PS2068_data)
    PS2068_Vm_P4p_SSSS_mod_e_Treatment_hpd0.95 <- as.data.frame(PS2068_Vm_P4p_SSSS_mod_e_Treatment)
    colnames(PS2068_Vm_P4p_SSSS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PS2068_Vm_P4p_SSSS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PS2068_Vm_P4p_SSSS_mod_e_Treatment, prob = 0.83))
    colnames(PS2068_Vm_P4p_SSSS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PS2068_Vm_P4p_SSSS_mod_Treatment_liab <- cbind.data.frame(PS2068_Vm_P4p_SSSS_mod_e_Treatment_hpd0.95,PS2068_Vm_P4p_SSSS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PS2068_Vm_P4p_SSSS_mod_Treatment_liab$Ancestor <- c("PS2068","PS2068")
    PS2068_Vm_P4p_SSSS_mod_Treatment_liab$Pnp <- c("P4.p","P4.p")
    PS2068_Vm_P4p_SSSS_mod_Treatment_liab$Pnp_fate <- c("SSSS","SSSS")
    PS2068_Vm_P4p_SSSS_mod_Treatment_liab$Scale <- c("liab","liab")
    PS2068_Vm_P4p_SSSS_mod_Treatment_liab
    
    PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PS2068_Vm_P4p_SSSS_mod_Treatment_liab[,2]))
    colnames(PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale_median) <- c("median")
    PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PS2068_Vm_P4p_SSSS_mod_Treatment_liab[,3]))
    colnames(PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PS2068_Vm_P4p_SSSS_mod_Treatment_liab[,4]))
    colnames(PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PS2068_Vm_P4p_SSSS_mod_Treatment_liab[,5]))
    colnames(PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PS2068_Vm_P4p_SSSS_mod_Treatment_liab[,6]))
    colnames(PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale) <- c("Treatment")
    PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale <- cbind.data.frame(PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale,PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale_median,PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95,PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95,PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83,PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 )
    PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale$Ancestor <- c("PS2068","PS2068")
    PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale$Pnp <- c("P4.p","P4.p")
    PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale$Pnp_fate <- c("SSSS","SSSS")
    PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PS2068_Vm_P4p_SSSS_mod_Treatment_liab_DataScale <- rbind.data.frame(PS2068_Vm_P4p_SSSS_mod_Treatment_liab, PS2068_Vm_P4p_SSSS_mod_Treatment_DataScale)
    PS2068_Vm_P4p_SSSS_mod_Treatment_liab_DataScale
    
  }
  
  ##PS2068_Vm_P8p_SSSS Mutation Median effects - Mutational Bias----
  {
    PS2068_Vm_P8p_SSSS_mod_e_Treatment<- emmeans(PS2068_Vm_P8p_SSSS_mod, ~Treatment, data=Vm_PS2068_data)
    PS2068_Vm_P8p_SSSS_mod_e_Treatment_hpd0.95 <- as.data.frame(PS2068_Vm_P8p_SSSS_mod_e_Treatment)
    colnames(PS2068_Vm_P8p_SSSS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PS2068_Vm_P8p_SSSS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PS2068_Vm_P8p_SSSS_mod_e_Treatment, prob = 0.83))
    colnames(PS2068_Vm_P8p_SSSS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PS2068_Vm_P8p_SSSS_mod_Treatment_liab <- cbind.data.frame(PS2068_Vm_P8p_SSSS_mod_e_Treatment_hpd0.95,PS2068_Vm_P8p_SSSS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PS2068_Vm_P8p_SSSS_mod_Treatment_liab$Ancestor <- c("PS2068","PS2068")
    PS2068_Vm_P8p_SSSS_mod_Treatment_liab$Pnp <- c("P8.p","P8.p")
    PS2068_Vm_P8p_SSSS_mod_Treatment_liab$Pnp_fate <- c("SSSS","SSSS")
    PS2068_Vm_P8p_SSSS_mod_Treatment_liab$Scale <- c("liab","liab")
    PS2068_Vm_P8p_SSSS_mod_Treatment_liab
    
    PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PS2068_Vm_P8p_SSSS_mod_Treatment_liab[,2]))
    colnames(PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale_median) <- c("median")
    PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PS2068_Vm_P8p_SSSS_mod_Treatment_liab[,3]))
    colnames(PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PS2068_Vm_P8p_SSSS_mod_Treatment_liab[,4]))
    colnames(PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PS2068_Vm_P8p_SSSS_mod_Treatment_liab[,5]))
    colnames(PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PS2068_Vm_P8p_SSSS_mod_Treatment_liab[,6]))
    colnames(PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale) <- c("Treatment")
    PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale <- cbind.data.frame(PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale,PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale_median,PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95,PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95,PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83,PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 )
    PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale$Ancestor <- c("PS2068","PS2068")
    PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale$Pnp <- c("P8.p","P8.p")
    PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale$Pnp_fate <- c("SSSS","SSSS")
    PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PS2068_Vm_P8p_SSSS_mod_Treatment_liab_DataScale <- rbind.data.frame(PS2068_Vm_P8p_SSSS_mod_Treatment_liab, PS2068_Vm_P8p_SSSS_mod_Treatment_DataScale)
    PS2068_Vm_P8p_SSSS_mod_Treatment_liab_DataScale
    
  }
  
  ##PS2068_Vm_P5p_wt Mutation Median effects - Mutational Bias----
  {
    PS2068_Vm_P5p_wt_mod_e_Treatment<- emmeans(PS2068_Vm_P5p_wt_mod, ~Treatment, data=Vm_PS2068_data)
    PS2068_Vm_P5p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(PS2068_Vm_P5p_wt_mod_e_Treatment)
    colnames(PS2068_Vm_P5p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PS2068_Vm_P5p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PS2068_Vm_P5p_wt_mod_e_Treatment, prob = 0.83))
    colnames(PS2068_Vm_P5p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PS2068_Vm_P5p_wt_mod_Treatment_liab <- cbind.data.frame(PS2068_Vm_P5p_wt_mod_e_Treatment_hpd0.95,PS2068_Vm_P5p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PS2068_Vm_P5p_wt_mod_Treatment_liab$Ancestor <- c("PS2068","PS2068")
    PS2068_Vm_P5p_wt_mod_Treatment_liab$Pnp <- c("P5.p","P5.p")
    PS2068_Vm_P5p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    PS2068_Vm_P5p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    PS2068_Vm_P5p_wt_mod_Treatment_liab
    
    PS2068_Vm_P5p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PS2068_Vm_P5p_wt_mod_Treatment_liab[,2]))
    colnames(PS2068_Vm_P5p_wt_mod_Treatment_DataScale_median) <- c("median")
    PS2068_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PS2068_Vm_P5p_wt_mod_Treatment_liab[,3]))
    colnames(PS2068_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PS2068_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PS2068_Vm_P5p_wt_mod_Treatment_liab[,4]))
    colnames(PS2068_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PS2068_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PS2068_Vm_P5p_wt_mod_Treatment_liab[,5]))
    colnames(PS2068_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PS2068_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PS2068_Vm_P5p_wt_mod_Treatment_liab[,6]))
    colnames(PS2068_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PS2068_Vm_P5p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PS2068_Vm_P5p_wt_mod_Treatment_DataScale) <- c("Treatment")
    PS2068_Vm_P5p_wt_mod_Treatment_DataScale <- cbind.data.frame(PS2068_Vm_P5p_wt_mod_Treatment_DataScale,PS2068_Vm_P5p_wt_mod_Treatment_DataScale_median,PS2068_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95,PS2068_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95,PS2068_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83,PS2068_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    PS2068_Vm_P5p_wt_mod_Treatment_DataScale$Ancestor <- c("PS2068","PS2068")
    PS2068_Vm_P5p_wt_mod_Treatment_DataScale$Pnp <- c("P5.p","P5.p")
    PS2068_Vm_P5p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    PS2068_Vm_P5p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PS2068_Vm_P5p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(PS2068_Vm_P5p_wt_mod_Treatment_liab, PS2068_Vm_P5p_wt_mod_Treatment_DataScale)
    PS2068_Vm_P5p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##PS2068_Vm_P6p_wt Mutation Median effects - Mutational Bias----
  {
    PS2068_Vm_P6p_wt_mod_e_Treatment<- emmeans(PS2068_Vm_P6p_wt_mod, ~Treatment, data=Vm_PS2068_data)
    PS2068_Vm_P6p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(PS2068_Vm_P6p_wt_mod_e_Treatment)
    colnames(PS2068_Vm_P6p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PS2068_Vm_P6p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PS2068_Vm_P6p_wt_mod_e_Treatment, prob = 0.83))
    colnames(PS2068_Vm_P6p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PS2068_Vm_P6p_wt_mod_Treatment_liab <- cbind.data.frame(PS2068_Vm_P6p_wt_mod_e_Treatment_hpd0.95,PS2068_Vm_P6p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PS2068_Vm_P6p_wt_mod_Treatment_liab$Ancestor <- c("PS2068","PS2068")
    PS2068_Vm_P6p_wt_mod_Treatment_liab$Pnp <- c("P6.p","P6.p")
    PS2068_Vm_P6p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    PS2068_Vm_P6p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    PS2068_Vm_P6p_wt_mod_Treatment_liab
    
    PS2068_Vm_P6p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PS2068_Vm_P6p_wt_mod_Treatment_liab[,2]))
    colnames(PS2068_Vm_P6p_wt_mod_Treatment_DataScale_median) <- c("median")
    PS2068_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PS2068_Vm_P6p_wt_mod_Treatment_liab[,3]))
    colnames(PS2068_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PS2068_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PS2068_Vm_P6p_wt_mod_Treatment_liab[,4]))
    colnames(PS2068_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PS2068_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PS2068_Vm_P6p_wt_mod_Treatment_liab[,5]))
    colnames(PS2068_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PS2068_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PS2068_Vm_P6p_wt_mod_Treatment_liab[,6]))
    colnames(PS2068_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PS2068_Vm_P6p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PS2068_Vm_P6p_wt_mod_Treatment_DataScale) <- c("Treatment")
    PS2068_Vm_P6p_wt_mod_Treatment_DataScale <- cbind.data.frame(PS2068_Vm_P6p_wt_mod_Treatment_DataScale,PS2068_Vm_P6p_wt_mod_Treatment_DataScale_median,PS2068_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95,PS2068_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95,PS2068_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83,PS2068_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    PS2068_Vm_P6p_wt_mod_Treatment_DataScale$Ancestor <- c("PS2068","PS2068")
    PS2068_Vm_P6p_wt_mod_Treatment_DataScale$Pnp <- c("P6.p","P6.p")
    PS2068_Vm_P6p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    PS2068_Vm_P6p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PS2068_Vm_P6p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(PS2068_Vm_P6p_wt_mod_Treatment_liab, PS2068_Vm_P6p_wt_mod_Treatment_DataScale)
    PS2068_Vm_P6p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##PS2068_Vm_P7p_wt Mutation Median effects - Mutational Bias----
  {
    PS2068_Vm_P7p_wt_mod_e_Treatment<- emmeans(PS2068_Vm_P7p_wt_mod, ~Treatment, data=Vm_PS2068_data)
    PS2068_Vm_P7p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(PS2068_Vm_P7p_wt_mod_e_Treatment)
    colnames(PS2068_Vm_P7p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    PS2068_Vm_P7p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(PS2068_Vm_P7p_wt_mod_e_Treatment, prob = 0.83))
    colnames(PS2068_Vm_P7p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    PS2068_Vm_P7p_wt_mod_Treatment_liab <- cbind.data.frame(PS2068_Vm_P7p_wt_mod_e_Treatment_hpd0.95,PS2068_Vm_P7p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    PS2068_Vm_P7p_wt_mod_Treatment_liab$Ancestor <- c("PS2068","PS2068")
    PS2068_Vm_P7p_wt_mod_Treatment_liab$Pnp <- c("P7.p","P7.p")
    PS2068_Vm_P7p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    PS2068_Vm_P7p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    PS2068_Vm_P7p_wt_mod_Treatment_liab
    
    PS2068_Vm_P7p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(PS2068_Vm_P7p_wt_mod_Treatment_liab[,2]))
    colnames(PS2068_Vm_P7p_wt_mod_Treatment_DataScale_median) <- c("median")
    PS2068_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(PS2068_Vm_P7p_wt_mod_Treatment_liab[,3]))
    colnames(PS2068_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    PS2068_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(PS2068_Vm_P7p_wt_mod_Treatment_liab[,4]))
    colnames(PS2068_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    PS2068_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(PS2068_Vm_P7p_wt_mod_Treatment_liab[,5]))
    colnames(PS2068_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    PS2068_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(PS2068_Vm_P7p_wt_mod_Treatment_liab[,6]))
    colnames(PS2068_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    PS2068_Vm_P7p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(PS2068_Vm_P7p_wt_mod_Treatment_DataScale) <- c("Treatment")
    PS2068_Vm_P7p_wt_mod_Treatment_DataScale <- cbind.data.frame(PS2068_Vm_P7p_wt_mod_Treatment_DataScale,PS2068_Vm_P7p_wt_mod_Treatment_DataScale_median,PS2068_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95,PS2068_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95,PS2068_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83,PS2068_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    PS2068_Vm_P7p_wt_mod_Treatment_DataScale$Ancestor <- c("PS2068","PS2068")
    PS2068_Vm_P7p_wt_mod_Treatment_DataScale$Pnp <- c("P7.p","P7.p")
    PS2068_Vm_P7p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    PS2068_Vm_P7p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    PS2068_Vm_P7p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(PS2068_Vm_P7p_wt_mod_Treatment_liab, PS2068_Vm_P7p_wt_mod_Treatment_DataScale)
    PS2068_Vm_P7p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##----
  PS2068_Vm_mod_Treatment_liab_DataScale_SSSS <- rbind.data.frame(PS2068_Vm_P3p_SSSS_mod_Treatment_liab_DataScale,PS2068_Vm_P4p_SSSS_mod_Treatment_liab_DataScale,PS2068_Vm_P5p_wt_mod_Treatment_liab_DataScale,PS2068_Vm_P6p_wt_mod_Treatment_liab_DataScale,PS2068_Vm_P7p_wt_mod_Treatment_liab_DataScale, PS2068_Vm_P8p_SSSS_mod_Treatment_liab_DataScale)
  PS2068_Vm_mod_Treatment_liab_DataScale_SSSS$Species <- rep("O.onirici",24)
  PS2068_Vm_mod_Treatment_liab_DataScale_SSSS$Genus <- rep("Oscheius",24)
  
  View(PS2068_Vm_mod_Treatment_liab_DataScale_SSSS)
  
}

#JU77_Vm_ Mutational Median effects - Mutational Bias ----
{
  ##JU77_Vm_P3p_SSSS Mutation Median effects - Mutational Bias----
  {
    JU77_Vm_P3p_SSSS_mod_e_Treatment<- emmeans(JU77_Vm_P3p_SSSS_mod, ~Treatment, data=Vm_JU77_data)
    JU77_Vm_P3p_SSSS_mod_e_Treatment_hpd0.95 <- as.data.frame(JU77_Vm_P3p_SSSS_mod_e_Treatment)
    colnames(JU77_Vm_P3p_SSSS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU77_Vm_P3p_SSSS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU77_Vm_P3p_SSSS_mod_e_Treatment, prob = 0.83))
    colnames(JU77_Vm_P3p_SSSS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU77_Vm_P3p_SSSS_mod_Treatment_liab <- cbind.data.frame(JU77_Vm_P3p_SSSS_mod_e_Treatment_hpd0.95,JU77_Vm_P3p_SSSS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU77_Vm_P3p_SSSS_mod_Treatment_liab$Ancestor <- c("JU77","JU77")
    JU77_Vm_P3p_SSSS_mod_Treatment_liab$Pnp <- c("P3.p","P3.p")
    JU77_Vm_P3p_SSSS_mod_Treatment_liab$Pnp_fate <- c("SSSS","SSSS")
    JU77_Vm_P3p_SSSS_mod_Treatment_liab$Scale <- c("liab","liab")
    JU77_Vm_P3p_SSSS_mod_Treatment_liab
    
    JU77_Vm_P3p_SSSS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU77_Vm_P3p_SSSS_mod_Treatment_liab[,2]))
    colnames(JU77_Vm_P3p_SSSS_mod_Treatment_DataScale_median) <- c("median")
    JU77_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU77_Vm_P3p_SSSS_mod_Treatment_liab[,3]))
    colnames(JU77_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU77_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU77_Vm_P3p_SSSS_mod_Treatment_liab[,4]))
    colnames(JU77_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU77_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU77_Vm_P3p_SSSS_mod_Treatment_liab[,5]))
    colnames(JU77_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU77_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU77_Vm_P3p_SSSS_mod_Treatment_liab[,6]))
    colnames(JU77_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU77_Vm_P3p_SSSS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU77_Vm_P3p_SSSS_mod_Treatment_DataScale) <- c("Treatment")
    JU77_Vm_P3p_SSSS_mod_Treatment_DataScale <- cbind.data.frame(JU77_Vm_P3p_SSSS_mod_Treatment_DataScale,JU77_Vm_P3p_SSSS_mod_Treatment_DataScale_median,JU77_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95,JU77_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95,JU77_Vm_P3p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83,JU77_Vm_P3p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU77_Vm_P3p_SSSS_mod_Treatment_DataScale$Ancestor <- c("JU77","JU77")
    JU77_Vm_P3p_SSSS_mod_Treatment_DataScale$Pnp <- c("P3.p","P3.p")
    JU77_Vm_P3p_SSSS_mod_Treatment_DataScale$Pnp_fate <- c("SSSS","SSSS")
    JU77_Vm_P3p_SSSS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU77_Vm_P3p_SSSS_mod_Treatment_liab_DataScale <- rbind.data.frame(JU77_Vm_P3p_SSSS_mod_Treatment_liab, JU77_Vm_P3p_SSSS_mod_Treatment_DataScale)
    JU77_Vm_P3p_SSSS_mod_Treatment_liab_DataScale
    
  }
  
  ##JU77_Vm_P4p_SSSS Mutation Median effects - Mutational Bias----
  {
    JU77_Vm_P4p_SSSS_mod_e_Treatment<- emmeans(JU77_Vm_P4p_SSSS_mod, ~Treatment, data=Vm_JU77_data)
    JU77_Vm_P4p_SSSS_mod_e_Treatment_hpd0.95 <- as.data.frame(JU77_Vm_P4p_SSSS_mod_e_Treatment)
    colnames(JU77_Vm_P4p_SSSS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU77_Vm_P4p_SSSS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU77_Vm_P4p_SSSS_mod_e_Treatment, prob = 0.83))
    colnames(JU77_Vm_P4p_SSSS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU77_Vm_P4p_SSSS_mod_Treatment_liab <- cbind.data.frame(JU77_Vm_P4p_SSSS_mod_e_Treatment_hpd0.95,JU77_Vm_P4p_SSSS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU77_Vm_P4p_SSSS_mod_Treatment_liab$Ancestor <- c("JU77","JU77")
    JU77_Vm_P4p_SSSS_mod_Treatment_liab$Pnp <- c("P4.p","P4.p")
    JU77_Vm_P4p_SSSS_mod_Treatment_liab$Pnp_fate <- c("SSSS","SSSS")
    JU77_Vm_P4p_SSSS_mod_Treatment_liab$Scale <- c("liab","liab")
    JU77_Vm_P4p_SSSS_mod_Treatment_liab
    
    JU77_Vm_P4p_SSSS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU77_Vm_P4p_SSSS_mod_Treatment_liab[,2]))
    colnames(JU77_Vm_P4p_SSSS_mod_Treatment_DataScale_median) <- c("median")
    JU77_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU77_Vm_P4p_SSSS_mod_Treatment_liab[,3]))
    colnames(JU77_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU77_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU77_Vm_P4p_SSSS_mod_Treatment_liab[,4]))
    colnames(JU77_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU77_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU77_Vm_P4p_SSSS_mod_Treatment_liab[,5]))
    colnames(JU77_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU77_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU77_Vm_P4p_SSSS_mod_Treatment_liab[,6]))
    colnames(JU77_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU77_Vm_P4p_SSSS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU77_Vm_P4p_SSSS_mod_Treatment_DataScale) <- c("Treatment")
    JU77_Vm_P4p_SSSS_mod_Treatment_DataScale <- cbind.data.frame(JU77_Vm_P4p_SSSS_mod_Treatment_DataScale,JU77_Vm_P4p_SSSS_mod_Treatment_DataScale_median,JU77_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95,JU77_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95,JU77_Vm_P4p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83,JU77_Vm_P4p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU77_Vm_P4p_SSSS_mod_Treatment_DataScale$Ancestor <- c("JU77","JU77")
    JU77_Vm_P4p_SSSS_mod_Treatment_DataScale$Pnp <- c("P4.p","P4.p")
    JU77_Vm_P4p_SSSS_mod_Treatment_DataScale$Pnp_fate <- c("SSSS","SSSS")
    JU77_Vm_P4p_SSSS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU77_Vm_P4p_SSSS_mod_Treatment_liab_DataScale <- rbind.data.frame(JU77_Vm_P4p_SSSS_mod_Treatment_liab, JU77_Vm_P4p_SSSS_mod_Treatment_DataScale)
    JU77_Vm_P4p_SSSS_mod_Treatment_liab_DataScale
    
  }
  
  ##JU77_Vm_P8p_SSSS Mutation Median effects - Mutational Bias----
  {
    JU77_Vm_P8p_SSSS_mod_e_Treatment<- emmeans(JU77_Vm_P8p_SSSS_mod, ~Treatment, data=Vm_JU77_data)
    JU77_Vm_P8p_SSSS_mod_e_Treatment_hpd0.95 <- as.data.frame(JU77_Vm_P8p_SSSS_mod_e_Treatment)
    colnames(JU77_Vm_P8p_SSSS_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU77_Vm_P8p_SSSS_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU77_Vm_P8p_SSSS_mod_e_Treatment, prob = 0.83))
    colnames(JU77_Vm_P8p_SSSS_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU77_Vm_P8p_SSSS_mod_Treatment_liab <- cbind.data.frame(JU77_Vm_P8p_SSSS_mod_e_Treatment_hpd0.95,JU77_Vm_P8p_SSSS_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU77_Vm_P8p_SSSS_mod_Treatment_liab$Ancestor <- c("JU77","JU77")
    JU77_Vm_P8p_SSSS_mod_Treatment_liab$Pnp <- c("P8.p","P8.p")
    JU77_Vm_P8p_SSSS_mod_Treatment_liab$Pnp_fate <- c("SSSS","SSSS")
    JU77_Vm_P8p_SSSS_mod_Treatment_liab$Scale <- c("liab","liab")
    JU77_Vm_P8p_SSSS_mod_Treatment_liab
    
    JU77_Vm_P8p_SSSS_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU77_Vm_P8p_SSSS_mod_Treatment_liab[,2]))
    colnames(JU77_Vm_P8p_SSSS_mod_Treatment_DataScale_median) <- c("median")
    JU77_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU77_Vm_P8p_SSSS_mod_Treatment_liab[,3]))
    colnames(JU77_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU77_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU77_Vm_P8p_SSSS_mod_Treatment_liab[,4]))
    colnames(JU77_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU77_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU77_Vm_P8p_SSSS_mod_Treatment_liab[,5]))
    colnames(JU77_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU77_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU77_Vm_P8p_SSSS_mod_Treatment_liab[,6]))
    colnames(JU77_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU77_Vm_P8p_SSSS_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU77_Vm_P8p_SSSS_mod_Treatment_DataScale) <- c("Treatment")
    JU77_Vm_P8p_SSSS_mod_Treatment_DataScale <- cbind.data.frame(JU77_Vm_P8p_SSSS_mod_Treatment_DataScale,JU77_Vm_P8p_SSSS_mod_Treatment_DataScale_median,JU77_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.95,JU77_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.95,JU77_Vm_P8p_SSSS_mod_Treatment_DataScale_lower.HPD_0.83,JU77_Vm_P8p_SSSS_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU77_Vm_P8p_SSSS_mod_Treatment_DataScale$Ancestor <- c("JU77","JU77")
    JU77_Vm_P8p_SSSS_mod_Treatment_DataScale$Pnp <- c("P8.p","P8.p")
    JU77_Vm_P8p_SSSS_mod_Treatment_DataScale$Pnp_fate <- c("SSSS","SSSS")
    JU77_Vm_P8p_SSSS_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU77_Vm_P8p_SSSS_mod_Treatment_liab_DataScale <- rbind.data.frame(JU77_Vm_P8p_SSSS_mod_Treatment_liab, JU77_Vm_P8p_SSSS_mod_Treatment_DataScale)
    JU77_Vm_P8p_SSSS_mod_Treatment_liab_DataScale
    
  }
  
  ##JU77_Vm_P5p_wt Mutation Median effects - Mutational Bias----
  {
    JU77_Vm_P5p_wt_mod_e_Treatment<- emmeans(JU77_Vm_P5p_wt_mod, ~Treatment, data=Vm_JU77_data)
    JU77_Vm_P5p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(JU77_Vm_P5p_wt_mod_e_Treatment)
    colnames(JU77_Vm_P5p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU77_Vm_P5p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU77_Vm_P5p_wt_mod_e_Treatment, prob = 0.83))
    colnames(JU77_Vm_P5p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU77_Vm_P5p_wt_mod_Treatment_liab <- cbind.data.frame(JU77_Vm_P5p_wt_mod_e_Treatment_hpd0.95,JU77_Vm_P5p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU77_Vm_P5p_wt_mod_Treatment_liab$Ancestor <- c("JU77","JU77")
    JU77_Vm_P5p_wt_mod_Treatment_liab$Pnp <- c("P5.p","P5.p")
    JU77_Vm_P5p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    JU77_Vm_P5p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    JU77_Vm_P5p_wt_mod_Treatment_liab
    
    JU77_Vm_P5p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU77_Vm_P5p_wt_mod_Treatment_liab[,2]))
    colnames(JU77_Vm_P5p_wt_mod_Treatment_DataScale_median) <- c("median")
    JU77_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU77_Vm_P5p_wt_mod_Treatment_liab[,3]))
    colnames(JU77_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU77_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU77_Vm_P5p_wt_mod_Treatment_liab[,4]))
    colnames(JU77_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU77_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU77_Vm_P5p_wt_mod_Treatment_liab[,5]))
    colnames(JU77_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU77_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU77_Vm_P5p_wt_mod_Treatment_liab[,6]))
    colnames(JU77_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU77_Vm_P5p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU77_Vm_P5p_wt_mod_Treatment_DataScale) <- c("Treatment")
    JU77_Vm_P5p_wt_mod_Treatment_DataScale <- cbind.data.frame(JU77_Vm_P5p_wt_mod_Treatment_DataScale,JU77_Vm_P5p_wt_mod_Treatment_DataScale_median,JU77_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.95,JU77_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.95,JU77_Vm_P5p_wt_mod_Treatment_DataScale_lower.HPD_0.83,JU77_Vm_P5p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU77_Vm_P5p_wt_mod_Treatment_DataScale$Ancestor <- c("JU77","JU77")
    JU77_Vm_P5p_wt_mod_Treatment_DataScale$Pnp <- c("P5.p","P5.p")
    JU77_Vm_P5p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    JU77_Vm_P5p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU77_Vm_P5p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(JU77_Vm_P5p_wt_mod_Treatment_liab, JU77_Vm_P5p_wt_mod_Treatment_DataScale)
    JU77_Vm_P5p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##JU77_Vm_P6p_wt Mutation Median effects - Mutational Bias----
  {
    JU77_Vm_P6p_wt_mod_e_Treatment<- emmeans(JU77_Vm_P6p_wt_mod, ~Treatment, data=Vm_JU77_data)
    JU77_Vm_P6p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(JU77_Vm_P6p_wt_mod_e_Treatment)
    colnames(JU77_Vm_P6p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU77_Vm_P6p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU77_Vm_P6p_wt_mod_e_Treatment, prob = 0.83))
    colnames(JU77_Vm_P6p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU77_Vm_P6p_wt_mod_Treatment_liab <- cbind.data.frame(JU77_Vm_P6p_wt_mod_e_Treatment_hpd0.95,JU77_Vm_P6p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU77_Vm_P6p_wt_mod_Treatment_liab$Ancestor <- c("JU77","JU77")
    JU77_Vm_P6p_wt_mod_Treatment_liab$Pnp <- c("P6.p","P6.p")
    JU77_Vm_P6p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    JU77_Vm_P6p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    JU77_Vm_P6p_wt_mod_Treatment_liab
    
    JU77_Vm_P6p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU77_Vm_P6p_wt_mod_Treatment_liab[,2]))
    colnames(JU77_Vm_P6p_wt_mod_Treatment_DataScale_median) <- c("median")
    JU77_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU77_Vm_P6p_wt_mod_Treatment_liab[,3]))
    colnames(JU77_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU77_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU77_Vm_P6p_wt_mod_Treatment_liab[,4]))
    colnames(JU77_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU77_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU77_Vm_P6p_wt_mod_Treatment_liab[,5]))
    colnames(JU77_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU77_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU77_Vm_P6p_wt_mod_Treatment_liab[,6]))
    colnames(JU77_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU77_Vm_P6p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU77_Vm_P6p_wt_mod_Treatment_DataScale) <- c("Treatment")
    JU77_Vm_P6p_wt_mod_Treatment_DataScale <- cbind.data.frame(JU77_Vm_P6p_wt_mod_Treatment_DataScale,JU77_Vm_P6p_wt_mod_Treatment_DataScale_median,JU77_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.95,JU77_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.95,JU77_Vm_P6p_wt_mod_Treatment_DataScale_lower.HPD_0.83,JU77_Vm_P6p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU77_Vm_P6p_wt_mod_Treatment_DataScale$Ancestor <- c("JU77","JU77")
    JU77_Vm_P6p_wt_mod_Treatment_DataScale$Pnp <- c("P6.p","P6.p")
    JU77_Vm_P6p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    JU77_Vm_P6p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU77_Vm_P6p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(JU77_Vm_P6p_wt_mod_Treatment_liab, JU77_Vm_P6p_wt_mod_Treatment_DataScale)
    JU77_Vm_P6p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##JU77_Vm_P7p_wt Mutation Median effects - Mutational Bias----
  {
    JU77_Vm_P7p_wt_mod_e_Treatment<- emmeans(JU77_Vm_P7p_wt_mod, ~Treatment, data=Vm_JU77_data)
    JU77_Vm_P7p_wt_mod_e_Treatment_hpd0.95 <- as.data.frame(JU77_Vm_P7p_wt_mod_e_Treatment)
    colnames(JU77_Vm_P7p_wt_mod_e_Treatment_hpd0.95) <- c("Treatment","median", "lower.HPD_0.95","upper.HPD_0.95")
    
    JU77_Vm_P7p_wt_mod_e_Treatment_hpd0.83 <- as.data.frame(hpd.summary(JU77_Vm_P7p_wt_mod_e_Treatment, prob = 0.83))
    colnames(JU77_Vm_P7p_wt_mod_e_Treatment_hpd0.83) <- c("Treatment","median", "lower.HPD_0.83","upper.HPD_0.83")
    JU77_Vm_P7p_wt_mod_Treatment_liab <- cbind.data.frame(JU77_Vm_P7p_wt_mod_e_Treatment_hpd0.95,JU77_Vm_P7p_wt_mod_e_Treatment_hpd0.83)[,-c(5:6)]
    
    JU77_Vm_P7p_wt_mod_Treatment_liab$Ancestor <- c("JU77","JU77")
    JU77_Vm_P7p_wt_mod_Treatment_liab$Pnp <- c("P7.p","P7.p")
    JU77_Vm_P7p_wt_mod_Treatment_liab$Pnp_fate <- c("wt","wt")
    JU77_Vm_P7p_wt_mod_Treatment_liab$Scale <- c("liab","liab")
    JU77_Vm_P7p_wt_mod_Treatment_liab
    
    JU77_Vm_P7p_wt_mod_Treatment_DataScale_median <- as.data.frame(pnorm(JU77_Vm_P7p_wt_mod_Treatment_liab[,2]))
    colnames(JU77_Vm_P7p_wt_mod_Treatment_DataScale_median) <- c("median")
    JU77_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95 <- as.data.frame(pnorm(JU77_Vm_P7p_wt_mod_Treatment_liab[,3]))
    colnames(JU77_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95) <- c("lower.HPD_0.95")
    JU77_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95 <- as.data.frame(pnorm(JU77_Vm_P7p_wt_mod_Treatment_liab[,4]))
    colnames(JU77_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95) <- c("upper.HPD_0.95")
    JU77_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83 <- as.data.frame(pnorm(JU77_Vm_P7p_wt_mod_Treatment_liab[,5]))
    colnames(JU77_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83) <- c("lower.HPD_0.83")
    JU77_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83 <- as.data.frame(pnorm(JU77_Vm_P7p_wt_mod_Treatment_liab[,6]))
    colnames(JU77_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83) <- c("upper.HPD_0.83")
    
    
    JU77_Vm_P7p_wt_mod_Treatment_DataScale <- as.data.frame(c("CONTROL","MA"))
    colnames(JU77_Vm_P7p_wt_mod_Treatment_DataScale) <- c("Treatment")
    JU77_Vm_P7p_wt_mod_Treatment_DataScale <- cbind.data.frame(JU77_Vm_P7p_wt_mod_Treatment_DataScale,JU77_Vm_P7p_wt_mod_Treatment_DataScale_median,JU77_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.95,JU77_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.95,JU77_Vm_P7p_wt_mod_Treatment_DataScale_lower.HPD_0.83,JU77_Vm_P7p_wt_mod_Treatment_DataScale_upper.HPD_0.83 )
    JU77_Vm_P7p_wt_mod_Treatment_DataScale$Ancestor <- c("JU77","JU77")
    JU77_Vm_P7p_wt_mod_Treatment_DataScale$Pnp <- c("P7.p","P7.p")
    JU77_Vm_P7p_wt_mod_Treatment_DataScale$Pnp_fate <- c("wt","wt")
    JU77_Vm_P7p_wt_mod_Treatment_DataScale$Scale <- c("Data","Data")
    
    JU77_Vm_P7p_wt_mod_Treatment_liab_DataScale <- rbind.data.frame(JU77_Vm_P7p_wt_mod_Treatment_liab, JU77_Vm_P7p_wt_mod_Treatment_DataScale)
    JU77_Vm_P7p_wt_mod_Treatment_liab_DataScale
    
  }
  
  ##----
  JU77_Vm_mod_Treatment_liab_DataScale_SSSS <- rbind.data.frame(JU77_Vm_P3p_SSSS_mod_Treatment_liab_DataScale,JU77_Vm_P4p_SSSS_mod_Treatment_liab_DataScale,JU77_Vm_P5p_wt_mod_Treatment_liab_DataScale,JU77_Vm_P6p_wt_mod_Treatment_liab_DataScale,JU77_Vm_P7p_wt_mod_Treatment_liab_DataScale, JU77_Vm_P8p_SSSS_mod_Treatment_liab_DataScale)
  JU77_Vm_mod_Treatment_liab_DataScale_SSSS$Species <- rep("O.onirici",24)
  JU77_Vm_mod_Treatment_liab_DataScale_SSSS$Genus <- rep("Oscheius",24)
  
  View(JU77_Vm_mod_Treatment_liab_DataScale_SSSS)
  
}

#Oscheius_Vm_mutational_median_effects_SSSS_Summary----
Oscheius_Vm_mutational_median_effects_SSSS_Summary <- rbind.data.frame(CEW1_Vm_mod_Treatment_liab_DataScale_SSSS, JU178_Vm_mod_Treatment_liab_DataScale_SSSS, PS2068_Vm_mod_Treatment_liab_DataScale_SSSS, JU77_Vm_mod_Treatment_liab_DataScale_SSSS)
Oscheius_Vm_mutational_median_effects_SSSS_Summary <- Oscheius_Vm_mutational_median_effects_SSSS_Summary %>% mutate(Treatment= str_replace(Treatment,'MA', 'ML'))
Oscheius_Vm_mutational_median_effects_SSSS_Summary <- Oscheius_Vm_mutational_median_effects_SSSS_Summary %>% mutate(Treatment= str_replace(Treatment,'CONTROL', 'Ancestral'))

View(Oscheius_Vm_mutational_median_effects_SSSS_Summary)

write_xlsx(Oscheius_Vm_mutational_median_effects_SSSS_Summary, "Oscheius_Vm_Mutational_Median_effects-Mutational_Bias_SSSS_Summary.xlsx")

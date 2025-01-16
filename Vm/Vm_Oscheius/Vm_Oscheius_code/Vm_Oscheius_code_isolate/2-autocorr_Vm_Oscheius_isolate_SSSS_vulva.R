#autocorr_Vm_Oscheius_isolate_SSSS_vulva

#CEW1
{
#CEW1_P3p_autocorr
autocorr_CEW1_CONTROL_P3p_SSSS_mod <- t(as.data.frame(autocorr.diag(CEW1_CONTROL_P3p_SSSS_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_CONTROL_P3p_SSSS_mod) <- c("CEW1_CONTROL_P3p_SSSS_mod")

autocorr_CEW1_MA_P3p_SSSS_mod <- t(as.data.frame(autocorr.diag(CEW1_MA_P3p_SSSS_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_MA_P3p_SSSS_mod) <- c("CEW1_MA_P3p_SSSS_mod")

autocorr_CEW1_Vm_P3p_SSSS_mod <- t(as.data.frame(autocorr.diag(CEW1_Vm_P3p_SSSS_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_Vm_P3p_SSSS_mod) <- c("CEW1_Vm_P3p_SSSS_mod")

autocorr_CEW1_P3p_SSSS_mod <- rbind.data.frame(autocorr_CEW1_CONTROL_P3p_SSSS_mod,autocorr_CEW1_MA_P3p_SSSS_mod,autocorr_CEW1_Vm_P3p_SSSS_mod)

#CEW1_P4p_autocorr
autocorr_CEW1_CONTROL_P4p_SSSS_mod <- t(as.data.frame(autocorr.diag(CEW1_CONTROL_P4p_SSSS_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_CONTROL_P4p_SSSS_mod) <- c("CEW1_CONTROL_P4p_SSSS_mod")

autocorr_CEW1_MA_P4p_SSSS_mod <- t(as.data.frame(autocorr.diag(CEW1_MA_P4p_SSSS_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_MA_P4p_SSSS_mod) <- c("CEW1_MA_P4p_SSSS_mod")

autocorr_CEW1_Vm_P4p_SSSS_mod <- t(as.data.frame(autocorr.diag(CEW1_Vm_P4p_SSSS_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_Vm_P4p_SSSS_mod) <- c("CEW1_Vm_P4p_SSSS_mod")

autocorr_CEW1_P4p_SSSS_mod <- rbind.data.frame(autocorr_CEW1_CONTROL_P4p_SSSS_mod,autocorr_CEW1_MA_P4p_SSSS_mod,autocorr_CEW1_Vm_P4p_SSSS_mod)

#CEW1_P8p_autocorr
autocorr_CEW1_CONTROL_P8p_SSSS_mod <- t(as.data.frame(autocorr.diag(CEW1_CONTROL_P8p_SSSS_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_CONTROL_P8p_SSSS_mod) <- c("CEW1_CONTROL_P8p_SSSS_mod")

autocorr_CEW1_MA_P8p_SSSS_mod <- t(as.data.frame(autocorr.diag(CEW1_MA_P8p_SSSS_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_MA_P8p_SSSS_mod) <- c("CEW1_MA_P8p_SSSS_mod")

autocorr_CEW1_Vm_P8p_SSSS_mod <- t(as.data.frame(autocorr.diag(CEW1_Vm_P8p_SSSS_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_Vm_P8p_SSSS_mod) <- c("CEW1_Vm_P8p_SSSS_mod")


autocorr_CEW1_P8p_SSSS_mod <- rbind.data.frame(autocorr_CEW1_CONTROL_P8p_SSSS_mod,autocorr_CEW1_MA_P8p_SSSS_mod,autocorr_CEW1_Vm_P8p_SSSS_mod)

#CEW1_P5p_autocorr
autocorr_CEW1_CONTROL_P5p_wt_mod <- t(as.data.frame(autocorr.diag(CEW1_CONTROL_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_CONTROL_P5p_wt_mod) <- c("CEW1_CONTROL_P5p_wt_mod")

autocorr_CEW1_MA_P5p_wt_mod <- t(as.data.frame(autocorr.diag(CEW1_MA_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_MA_P5p_wt_mod) <- c("CEW1_MA_P5p_wt_mod")

autocorr_CEW1_Vm_P5p_wt_mod <- t(as.data.frame(autocorr.diag(CEW1_Vm_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_Vm_P5p_wt_mod) <- c("CEW1_Vm_P5p_wt_mod")

autocorr_CEW1_P5p_wt_mod <- rbind.data.frame(autocorr_CEW1_CONTROL_P5p_wt_mod,autocorr_CEW1_MA_P5p_wt_mod,autocorr_CEW1_Vm_P5p_wt_mod)


#CEW1_P6p_autocorr
autocorr_CEW1_CONTROL_P6p_wt_mod <- t(as.data.frame(autocorr.diag(CEW1_CONTROL_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_CONTROL_P6p_wt_mod) <- c("CEW1_CONTROL_P6p_wt_mod")

autocorr_CEW1_MA_P6p_wt_mod <- t(as.data.frame(autocorr.diag(CEW1_MA_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_MA_P6p_wt_mod) <- c("CEW1_MA_P6p_wt_mod")

autocorr_CEW1_Vm_P6p_wt_mod <- t(as.data.frame(autocorr.diag(CEW1_Vm_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_Vm_P6p_wt_mod) <- c("CEW1_Vm_P6p_wt_mod")


autocorr_CEW1_P6p_wt_mod <- rbind.data.frame(autocorr_CEW1_CONTROL_P6p_wt_mod,autocorr_CEW1_MA_P6p_wt_mod,autocorr_CEW1_Vm_P6p_wt_mod)

#CEW1_P7p_autocorr
autocorr_CEW1_CONTROL_P7p_wt_mod <- t(as.data.frame(autocorr.diag(CEW1_CONTROL_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_CONTROL_P7p_wt_mod) <- c("CEW1_CONTROL_P7p_wt_mod")

autocorr_CEW1_MA_P7p_wt_mod <- t(as.data.frame(autocorr.diag(CEW1_MA_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_MA_P7p_wt_mod) <- c("CEW1_MA_P7p_wt_mod")

autocorr_CEW1_Vm_P7p_wt_mod <- t(as.data.frame(autocorr.diag(CEW1_Vm_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
rownames(autocorr_CEW1_Vm_P7p_wt_mod) <- c("CEW1_Vm_P7p_wt_mod")

autocorr_CEW1_P7p_wt_mod <- rbind.data.frame(autocorr_CEW1_CONTROL_P7p_wt_mod,autocorr_CEW1_MA_P7p_wt_mod,autocorr_CEW1_Vm_P7p_wt_mod)


#CEW1_autocorr_SSSS_vulva
autocorr_CEW1_SSSS_vulva <- rbind.data.frame(autocorr_CEW1_P3p_SSSS_mod,autocorr_CEW1_P4p_SSSS_mod,autocorr_CEW1_P5p_wt_mod,autocorr_CEW1_P6p_wt_mod,autocorr_CEW1_P7p_wt_mod,autocorr_CEW1_P8p_SSSS_mod)

}

#JU178
{
  #JU178_P3p_autocorr
  autocorr_JU178_CONTROL_P3p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU178_CONTROL_P3p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_CONTROL_P3p_SSSS_mod) <- c("JU178_CONTROL_P3p_SSSS_mod")
  
  autocorr_JU178_MA_P3p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU178_MA_P3p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_MA_P3p_SSSS_mod) <- c("JU178_MA_P3p_SSSS_mod")
  
  autocorr_JU178_Vm_P3p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU178_Vm_P3p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_Vm_P3p_SSSS_mod) <- c("JU178_Vm_P3p_SSSS_mod")
  
  autocorr_JU178_P3p_SSSS_mod <- rbind.data.frame(autocorr_JU178_CONTROL_P3p_SSSS_mod,autocorr_JU178_MA_P3p_SSSS_mod,autocorr_JU178_Vm_P3p_SSSS_mod)
  
  #JU178_P4p_autocorr
  autocorr_JU178_CONTROL_P4p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU178_CONTROL_P4p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_CONTROL_P4p_SSSS_mod) <- c("JU178_CONTROL_P4p_SSSS_mod")
  
  autocorr_JU178_MA_P4p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU178_MA_P4p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_MA_P4p_SSSS_mod) <- c("JU178_MA_P4p_SSSS_mod")
  
  autocorr_JU178_Vm_P4p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU178_Vm_P4p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_Vm_P4p_SSSS_mod) <- c("JU178_Vm_P4p_SSSS_mod")
  
  autocorr_JU178_P4p_SSSS_mod <- rbind.data.frame(autocorr_JU178_CONTROL_P4p_SSSS_mod,autocorr_JU178_MA_P4p_SSSS_mod,autocorr_JU178_Vm_P4p_SSSS_mod)
  
  #JU178_P8p_autocorr
  autocorr_JU178_CONTROL_P8p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU178_CONTROL_P8p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_CONTROL_P8p_SSSS_mod) <- c("JU178_CONTROL_P8p_SSSS_mod")
  
  autocorr_JU178_MA_P8p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU178_MA_P8p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_MA_P8p_SSSS_mod) <- c("JU178_MA_P8p_SSSS_mod")
  
  autocorr_JU178_Vm_P8p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU178_Vm_P8p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_Vm_P8p_SSSS_mod) <- c("JU178_Vm_P8p_SSSS_mod")
  
  
  autocorr_JU178_P8p_SSSS_mod <- rbind.data.frame(autocorr_JU178_CONTROL_P8p_SSSS_mod,autocorr_JU178_MA_P8p_SSSS_mod,autocorr_JU178_Vm_P8p_SSSS_mod)
  
  #JU178_P5p_autocorr
  autocorr_JU178_CONTROL_P5p_wt_mod <- t(as.data.frame(autocorr.diag(JU178_CONTROL_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_CONTROL_P5p_wt_mod) <- c("JU178_CONTROL_P5p_wt_mod")
  
  autocorr_JU178_MA_P5p_wt_mod <- t(as.data.frame(autocorr.diag(JU178_MA_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_MA_P5p_wt_mod) <- c("JU178_MA_P5p_wt_mod")
  
  autocorr_JU178_Vm_P5p_wt_mod <- t(as.data.frame(autocorr.diag(JU178_Vm_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_Vm_P5p_wt_mod) <- c("JU178_Vm_P5p_wt_mod")
  
  autocorr_JU178_P5p_wt_mod <- rbind.data.frame(autocorr_JU178_CONTROL_P5p_wt_mod,autocorr_JU178_MA_P5p_wt_mod,autocorr_JU178_Vm_P5p_wt_mod)
  
  
  #JU178_P6p_autocorr
  autocorr_JU178_CONTROL_P6p_wt_mod <- t(as.data.frame(autocorr.diag(JU178_CONTROL_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_CONTROL_P6p_wt_mod) <- c("JU178_CONTROL_P6p_wt_mod")
  
  autocorr_JU178_MA_P6p_wt_mod <- t(as.data.frame(autocorr.diag(JU178_MA_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_MA_P6p_wt_mod) <- c("JU178_MA_P6p_wt_mod")
  
  autocorr_JU178_Vm_P6p_wt_mod <- t(as.data.frame(autocorr.diag(JU178_Vm_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_Vm_P6p_wt_mod) <- c("JU178_Vm_P6p_wt_mod")
  
  
  autocorr_JU178_P6p_wt_mod <- rbind.data.frame(autocorr_JU178_CONTROL_P6p_wt_mod,autocorr_JU178_MA_P6p_wt_mod,autocorr_JU178_Vm_P6p_wt_mod)
  
  #JU178_P7p_autocorr
  autocorr_JU178_CONTROL_P7p_wt_mod <- t(as.data.frame(autocorr.diag(JU178_CONTROL_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_CONTROL_P7p_wt_mod) <- c("JU178_CONTROL_P7p_wt_mod")
  
  autocorr_JU178_MA_P7p_wt_mod <- t(as.data.frame(autocorr.diag(JU178_MA_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_MA_P7p_wt_mod) <- c("JU178_MA_P7p_wt_mod")
  
  autocorr_JU178_Vm_P7p_wt_mod <- t(as.data.frame(autocorr.diag(JU178_Vm_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU178_Vm_P7p_wt_mod) <- c("JU178_Vm_P7p_wt_mod")
  
  autocorr_JU178_P7p_wt_mod <- rbind.data.frame(autocorr_JU178_CONTROL_P7p_wt_mod,autocorr_JU178_MA_P7p_wt_mod,autocorr_JU178_Vm_P7p_wt_mod)
  
  
  #JU178_autocorr_SSSS_vulva
  autocorr_JU178_SSSS_vulva <- rbind.data.frame(autocorr_JU178_P3p_SSSS_mod,autocorr_JU178_P4p_SSSS_mod,autocorr_JU178_P5p_wt_mod,autocorr_JU178_P6p_wt_mod,autocorr_JU178_P7p_wt_mod,autocorr_JU178_P8p_SSSS_mod)
  autocorr_JU178_SSSS_vulva
}

#PS2068
{
  #PS2068_P3p_autocorr
  autocorr_PS2068_CONTROL_P3p_SSSS_mod <- t(as.data.frame(autocorr.diag(PS2068_CONTROL_P3p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_CONTROL_P3p_SSSS_mod) <- c("PS2068_CONTROL_P3p_SSSS_mod")
  
  autocorr_PS2068_MA_P3p_SSSS_mod <- t(as.data.frame(autocorr.diag(PS2068_MA_P3p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_MA_P3p_SSSS_mod) <- c("PS2068_MA_P3p_SSSS_mod")
  
  autocorr_PS2068_Vm_P3p_SSSS_mod <- t(as.data.frame(autocorr.diag(PS2068_Vm_P3p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_Vm_P3p_SSSS_mod) <- c("PS2068_Vm_P3p_SSSS_mod")
  
  autocorr_PS2068_P3p_SSSS_mod <- rbind.data.frame(autocorr_PS2068_CONTROL_P3p_SSSS_mod,autocorr_PS2068_MA_P3p_SSSS_mod,autocorr_PS2068_Vm_P3p_SSSS_mod)
  
  #PS2068_P4p_autocorr
  autocorr_PS2068_CONTROL_P4p_SSSS_mod <- t(as.data.frame(autocorr.diag(PS2068_CONTROL_P4p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_CONTROL_P4p_SSSS_mod) <- c("PS2068_CONTROL_P4p_SSSS_mod")
  
  autocorr_PS2068_MA_P4p_SSSS_mod <- t(as.data.frame(autocorr.diag(PS2068_MA_P4p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_MA_P4p_SSSS_mod) <- c("PS2068_MA_P4p_SSSS_mod")
  
  autocorr_PS2068_Vm_P4p_SSSS_mod <- t(as.data.frame(autocorr.diag(PS2068_Vm_P4p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_Vm_P4p_SSSS_mod) <- c("PS2068_Vm_P4p_SSSS_mod")
  
  autocorr_PS2068_P4p_SSSS_mod <- rbind.data.frame(autocorr_PS2068_CONTROL_P4p_SSSS_mod,autocorr_PS2068_MA_P4p_SSSS_mod,autocorr_PS2068_Vm_P4p_SSSS_mod)
  
  #PS2068_P8p_autocorr
  autocorr_PS2068_CONTROL_P8p_SSSS_mod <- t(as.data.frame(autocorr.diag(PS2068_CONTROL_P8p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_CONTROL_P8p_SSSS_mod) <- c("PS2068_CONTROL_P8p_SSSS_mod")
  
  autocorr_PS2068_MA_P8p_SSSS_mod <- t(as.data.frame(autocorr.diag(PS2068_MA_P8p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_MA_P8p_SSSS_mod) <- c("PS2068_MA_P8p_SSSS_mod")
  
  autocorr_PS2068_Vm_P8p_SSSS_mod <- t(as.data.frame(autocorr.diag(PS2068_Vm_P8p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_Vm_P8p_SSSS_mod) <- c("PS2068_Vm_P8p_SSSS_mod")
  
  
  autocorr_PS2068_P8p_SSSS_mod <- rbind.data.frame(autocorr_PS2068_CONTROL_P8p_SSSS_mod,autocorr_PS2068_MA_P8p_SSSS_mod,autocorr_PS2068_Vm_P8p_SSSS_mod)
  
  #PS2068_P5p_autocorr
  autocorr_PS2068_CONTROL_P5p_wt_mod <- t(as.data.frame(autocorr.diag(PS2068_CONTROL_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_CONTROL_P5p_wt_mod) <- c("PS2068_CONTROL_P5p_wt_mod")
  
  autocorr_PS2068_MA_P5p_wt_mod <- t(as.data.frame(autocorr.diag(PS2068_MA_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_MA_P5p_wt_mod) <- c("PS2068_MA_P5p_wt_mod")
  
  autocorr_PS2068_Vm_P5p_wt_mod <- t(as.data.frame(autocorr.diag(PS2068_Vm_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_Vm_P5p_wt_mod) <- c("PS2068_Vm_P5p_wt_mod")
  
  autocorr_PS2068_P5p_wt_mod <- rbind.data.frame(autocorr_PS2068_CONTROL_P5p_wt_mod,autocorr_PS2068_MA_P5p_wt_mod,autocorr_PS2068_Vm_P5p_wt_mod)
  
  
  #PS2068_P6p_autocorr
  autocorr_PS2068_CONTROL_P6p_wt_mod <- t(as.data.frame(autocorr.diag(PS2068_CONTROL_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_CONTROL_P6p_wt_mod) <- c("PS2068_CONTROL_P6p_wt_mod")
  
  autocorr_PS2068_MA_P6p_wt_mod <- t(as.data.frame(autocorr.diag(PS2068_MA_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_MA_P6p_wt_mod) <- c("PS2068_MA_P6p_wt_mod")
  
  autocorr_PS2068_Vm_P6p_wt_mod <- t(as.data.frame(autocorr.diag(PS2068_Vm_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_Vm_P6p_wt_mod) <- c("PS2068_Vm_P6p_wt_mod")
  
  
  autocorr_PS2068_P6p_wt_mod <- rbind.data.frame(autocorr_PS2068_CONTROL_P6p_wt_mod,autocorr_PS2068_MA_P6p_wt_mod,autocorr_PS2068_Vm_P6p_wt_mod)
  
  #PS2068_P7p_autocorr
  autocorr_PS2068_CONTROL_P7p_wt_mod <- t(as.data.frame(autocorr.diag(PS2068_CONTROL_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_CONTROL_P7p_wt_mod) <- c("PS2068_CONTROL_P7p_wt_mod")
  
  autocorr_PS2068_MA_P7p_wt_mod <- t(as.data.frame(autocorr.diag(PS2068_MA_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_MA_P7p_wt_mod) <- c("PS2068_MA_P7p_wt_mod")
  
  autocorr_PS2068_Vm_P7p_wt_mod <- t(as.data.frame(autocorr.diag(PS2068_Vm_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PS2068_Vm_P7p_wt_mod) <- c("PS2068_Vm_P7p_wt_mod")
  
  autocorr_PS2068_P7p_wt_mod <- rbind.data.frame(autocorr_PS2068_CONTROL_P7p_wt_mod,autocorr_PS2068_MA_P7p_wt_mod,autocorr_PS2068_Vm_P7p_wt_mod)
  
  
  #PS2068_autocorr_SSSS_vulva
  autocorr_PS2068_SSSS_vulva <- rbind.data.frame(autocorr_PS2068_P3p_SSSS_mod,autocorr_PS2068_P4p_SSSS_mod,autocorr_PS2068_P5p_wt_mod,autocorr_PS2068_P6p_wt_mod,autocorr_PS2068_P7p_wt_mod,autocorr_PS2068_P8p_SSSS_mod)
  autocorr_PS2068_SSSS_vulva
}

#JU77
{
  #JU77_P3p_autocorr
  autocorr_JU77_CONTROL_P3p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU77_CONTROL_P3p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_CONTROL_P3p_SSSS_mod) <- c("JU77_CONTROL_P3p_SSSS_mod")
  
  autocorr_JU77_MA_P3p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU77_MA_P3p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_MA_P3p_SSSS_mod) <- c("JU77_MA_P3p_SSSS_mod")
  
  autocorr_JU77_Vm_P3p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU77_Vm_P3p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_Vm_P3p_SSSS_mod) <- c("JU77_Vm_P3p_SSSS_mod")
  
  autocorr_JU77_P3p_SSSS_mod <- rbind.data.frame(autocorr_JU77_CONTROL_P3p_SSSS_mod,autocorr_JU77_MA_P3p_SSSS_mod,autocorr_JU77_Vm_P3p_SSSS_mod)
  
  #JU77_P4p_autocorr
  autocorr_JU77_CONTROL_P4p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU77_CONTROL_P4p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_CONTROL_P4p_SSSS_mod) <- c("JU77_CONTROL_P4p_SSSS_mod")
  
  autocorr_JU77_MA_P4p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU77_MA_P4p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_MA_P4p_SSSS_mod) <- c("JU77_MA_P4p_SSSS_mod")
  
  autocorr_JU77_Vm_P4p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU77_Vm_P4p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_Vm_P4p_SSSS_mod) <- c("JU77_Vm_P4p_SSSS_mod")
  
  autocorr_JU77_P4p_SSSS_mod <- rbind.data.frame(autocorr_JU77_CONTROL_P4p_SSSS_mod,autocorr_JU77_MA_P4p_SSSS_mod,autocorr_JU77_Vm_P4p_SSSS_mod)
  
  #JU77_P8p_autocorr
  autocorr_JU77_CONTROL_P8p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU77_CONTROL_P8p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_CONTROL_P8p_SSSS_mod) <- c("JU77_CONTROL_P8p_SSSS_mod")
  
  autocorr_JU77_MA_P8p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU77_MA_P8p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_MA_P8p_SSSS_mod) <- c("JU77_MA_P8p_SSSS_mod")
  
  autocorr_JU77_Vm_P8p_SSSS_mod <- t(as.data.frame(autocorr.diag(JU77_Vm_P8p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_Vm_P8p_SSSS_mod) <- c("JU77_Vm_P8p_SSSS_mod")
  
  
  autocorr_JU77_P8p_SSSS_mod <- rbind.data.frame(autocorr_JU77_CONTROL_P8p_SSSS_mod,autocorr_JU77_MA_P8p_SSSS_mod,autocorr_JU77_Vm_P8p_SSSS_mod)
  
  #JU77_P5p_autocorr
  autocorr_JU77_CONTROL_P5p_wt_mod <- t(as.data.frame(autocorr.diag(JU77_CONTROL_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_CONTROL_P5p_wt_mod) <- c("JU77_CONTROL_P5p_wt_mod")
  
  autocorr_JU77_MA_P5p_wt_mod <- t(as.data.frame(autocorr.diag(JU77_MA_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_MA_P5p_wt_mod) <- c("JU77_MA_P5p_wt_mod")
  
  autocorr_JU77_Vm_P5p_wt_mod <- t(as.data.frame(autocorr.diag(JU77_Vm_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_Vm_P5p_wt_mod) <- c("JU77_Vm_P5p_wt_mod")
  
  autocorr_JU77_P5p_wt_mod <- rbind.data.frame(autocorr_JU77_CONTROL_P5p_wt_mod,autocorr_JU77_MA_P5p_wt_mod,autocorr_JU77_Vm_P5p_wt_mod)
  
  
  #JU77_P6p_autocorr
  autocorr_JU77_CONTROL_P6p_wt_mod <- t(as.data.frame(autocorr.diag(JU77_CONTROL_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_CONTROL_P6p_wt_mod) <- c("JU77_CONTROL_P6p_wt_mod")
  
  autocorr_JU77_MA_P6p_wt_mod <- t(as.data.frame(autocorr.diag(JU77_MA_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_MA_P6p_wt_mod) <- c("JU77_MA_P6p_wt_mod")
  
  autocorr_JU77_Vm_P6p_wt_mod <- t(as.data.frame(autocorr.diag(JU77_Vm_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_Vm_P6p_wt_mod) <- c("JU77_Vm_P6p_wt_mod")
  
  
  autocorr_JU77_P6p_wt_mod <- rbind.data.frame(autocorr_JU77_CONTROL_P6p_wt_mod,autocorr_JU77_MA_P6p_wt_mod,autocorr_JU77_Vm_P6p_wt_mod)
  
  #JU77_P7p_autocorr
  autocorr_JU77_CONTROL_P7p_wt_mod <- t(as.data.frame(autocorr.diag(JU77_CONTROL_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_CONTROL_P7p_wt_mod) <- c("JU77_CONTROL_P7p_wt_mod")
  
  autocorr_JU77_MA_P7p_wt_mod <- t(as.data.frame(autocorr.diag(JU77_MA_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_MA_P7p_wt_mod) <- c("JU77_MA_P7p_wt_mod")
  
  autocorr_JU77_Vm_P7p_wt_mod <- t(as.data.frame(autocorr.diag(JU77_Vm_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU77_Vm_P7p_wt_mod) <- c("JU77_Vm_P7p_wt_mod")
  
  autocorr_JU77_P7p_wt_mod <- rbind.data.frame(autocorr_JU77_CONTROL_P7p_wt_mod,autocorr_JU77_MA_P7p_wt_mod,autocorr_JU77_Vm_P7p_wt_mod)
  
  
  #JU77_autocorr_SSSS_vulva
  autocorr_JU77_SSSS_vulva <- rbind.data.frame(autocorr_JU77_P3p_SSSS_mod,autocorr_JU77_P4p_SSSS_mod,autocorr_JU77_P5p_wt_mod,autocorr_JU77_P6p_wt_mod,autocorr_JU77_P7p_wt_mod,autocorr_JU77_P8p_SSSS_mod)
  autocorr_JU77_SSSS_vulva
}

Vm_Oscheius_isolate_SSSS_autocorr <-  rbind.data.frame(autocorr_CEW1_SSSS_vulva,autocorr_JU178_SSSS_vulva,autocorr_PS2068_SSSS_vulva,autocorr_JU77_SSSS_vulva)

Vm_Oscheius_isolate_SSSS_autocorr <- cbind(Models = rownames(Vm_Oscheius_isolate_SSSS_autocorr),Vm_Oscheius_isolate_SSSS_autocorr)

View(Vm_Oscheius_isolate_SSSS_autocorr)

write_xlsx(Vm_Oscheius_isolate_SSSS_autocorr, "Vm_Oscheius_isolate_SSSS_autocorr.xlsx")


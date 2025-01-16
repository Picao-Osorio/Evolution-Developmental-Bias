#autocorr_Vm_Caenorhabditis_isolate_SS_vulva

#JU1200
{
  #JU1200_P3p_autocorr
  autocorr_JU1200_CONTROL_P3p_SS_mod <- t(as.data.frame(autocorr.diag(JU1200_CONTROL_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_CONTROL_P3p_SS_mod) <- c("JU1200_CONTROL_P3p_SS_mod")
  
  autocorr_JU1200_MA_P3p_SS_mod <- t(as.data.frame(autocorr.diag(JU1200_MA_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_MA_P3p_SS_mod) <- c("JU1200_MA_P3p_SS_mod")
  
  autocorr_JU1200_Vm_P3p_SS_mod <- t(as.data.frame(autocorr.diag(JU1200_Vm_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_Vm_P3p_SS_mod) <- c("JU1200_Vm_P3p_SS_mod")
  
  autocorr_JU1200_P3p_SS_mod <- rbind.data.frame(autocorr_JU1200_CONTROL_P3p_SS_mod,autocorr_JU1200_MA_P3p_SS_mod,autocorr_JU1200_Vm_P3p_SS_mod)
  
  #JU1200_P4p_autocorr
  autocorr_JU1200_CONTROL_P4p_SS_mod <- t(as.data.frame(autocorr.diag(JU1200_CONTROL_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_CONTROL_P4p_SS_mod) <- c("JU1200_CONTROL_P4p_SS_mod")
  
  autocorr_JU1200_MA_P4p_SS_mod <- t(as.data.frame(autocorr.diag(JU1200_MA_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_MA_P4p_SS_mod) <- c("JU1200_MA_P4p_SS_mod")
  
  autocorr_JU1200_Vm_P4p_SS_mod <- t(as.data.frame(autocorr.diag(JU1200_Vm_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_Vm_P4p_SS_mod) <- c("JU1200_Vm_P4p_SS_mod")
  
  autocorr_JU1200_P4p_SS_mod <- rbind.data.frame(autocorr_JU1200_CONTROL_P4p_SS_mod,autocorr_JU1200_MA_P4p_SS_mod,autocorr_JU1200_Vm_P4p_SS_mod)
  
  #JU1200_P8p_autocorr
  autocorr_JU1200_CONTROL_P8p_SS_mod <- t(as.data.frame(autocorr.diag(JU1200_CONTROL_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_CONTROL_P8p_SS_mod) <- c("JU1200_CONTROL_P8p_SS_mod")
  
  autocorr_JU1200_MA_P8p_SS_mod <- t(as.data.frame(autocorr.diag(JU1200_MA_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_MA_P8p_SS_mod) <- c("JU1200_MA_P8p_SS_mod")
  
  autocorr_JU1200_Vm_P8p_SS_mod <- t(as.data.frame(autocorr.diag(JU1200_Vm_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_Vm_P8p_SS_mod) <- c("JU1200_Vm_P8p_SS_mod")
  
  
  autocorr_JU1200_P8p_SS_mod <- rbind.data.frame(autocorr_JU1200_CONTROL_P8p_SS_mod,autocorr_JU1200_MA_P8p_SS_mod,autocorr_JU1200_Vm_P8p_SS_mod)
  
  #JU1200_P5p_autocorr
  autocorr_JU1200_CONTROL_P5p_wt_mod <- t(as.data.frame(autocorr.diag(JU1200_CONTROL_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_CONTROL_P5p_wt_mod) <- c("JU1200_CONTROL_P5p_wt_mod")
  
  autocorr_JU1200_MA_P5p_wt_mod <- t(as.data.frame(autocorr.diag(JU1200_MA_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_MA_P5p_wt_mod) <- c("JU1200_MA_P5p_wt_mod")
  
  autocorr_JU1200_Vm_P5p_wt_mod <- t(as.data.frame(autocorr.diag(JU1200_Vm_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_Vm_P5p_wt_mod) <- c("JU1200_Vm_P5p_wt_mod")
  
  autocorr_JU1200_P5p_wt_mod <- rbind.data.frame(autocorr_JU1200_CONTROL_P5p_wt_mod,autocorr_JU1200_MA_P5p_wt_mod,autocorr_JU1200_Vm_P5p_wt_mod)
  
  
  #JU1200_P6p_autocorr
  autocorr_JU1200_CONTROL_P6p_wt_mod <- t(as.data.frame(autocorr.diag(JU1200_CONTROL_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_CONTROL_P6p_wt_mod) <- c("JU1200_CONTROL_P6p_wt_mod")
  
  autocorr_JU1200_MA_P6p_wt_mod <- t(as.data.frame(autocorr.diag(JU1200_MA_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_MA_P6p_wt_mod) <- c("JU1200_MA_P6p_wt_mod")
  
  autocorr_JU1200_Vm_P6p_wt_mod <- t(as.data.frame(autocorr.diag(JU1200_Vm_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_Vm_P6p_wt_mod) <- c("JU1200_Vm_P6p_wt_mod")
  
  
  autocorr_JU1200_P6p_wt_mod <- rbind.data.frame(autocorr_JU1200_CONTROL_P6p_wt_mod,autocorr_JU1200_MA_P6p_wt_mod,autocorr_JU1200_Vm_P6p_wt_mod)
  
  #JU1200_P7p_autocorr
  autocorr_JU1200_CONTROL_P7p_wt_mod <- t(as.data.frame(autocorr.diag(JU1200_CONTROL_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_CONTROL_P7p_wt_mod) <- c("JU1200_CONTROL_P7p_wt_mod")
  
  autocorr_JU1200_MA_P7p_wt_mod <- t(as.data.frame(autocorr.diag(JU1200_MA_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_MA_P7p_wt_mod) <- c("JU1200_MA_P7p_wt_mod")
  
  autocorr_JU1200_Vm_P7p_wt_mod <- t(as.data.frame(autocorr.diag(JU1200_Vm_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_JU1200_Vm_P7p_wt_mod) <- c("JU1200_Vm_P7p_wt_mod")
  
  autocorr_JU1200_P7p_wt_mod <- rbind.data.frame(autocorr_JU1200_CONTROL_P7p_wt_mod,autocorr_JU1200_MA_P7p_wt_mod,autocorr_JU1200_Vm_P7p_wt_mod)
  
  
  #JU1200_autocorr_SS_vulva
  autocorr_JU1200_SS_vulva <- rbind.data.frame(autocorr_JU1200_P3p_SS_mod,autocorr_JU1200_P4p_SS_mod,autocorr_JU1200_P5p_wt_mod,autocorr_JU1200_P6p_wt_mod,autocorr_JU1200_P7p_wt_mod,autocorr_JU1200_P8p_SS_mod)
  
}

#PB306
{
  #PB306_P3p_autocorr
  autocorr_PB306_CONTROL_P3p_SS_mod <- t(as.data.frame(autocorr.diag(PB306_CONTROL_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_CONTROL_P3p_SS_mod) <- c("PB306_CONTROL_P3p_SS_mod")
  
  autocorr_PB306_MA_P3p_SS_mod <- t(as.data.frame(autocorr.diag(PB306_MA_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_MA_P3p_SS_mod) <- c("PB306_MA_P3p_SS_mod")
  
  autocorr_PB306_Vm_P3p_SS_mod <- t(as.data.frame(autocorr.diag(PB306_Vm_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_Vm_P3p_SS_mod) <- c("PB306_Vm_P3p_SS_mod")
  
  autocorr_PB306_P3p_SS_mod <- rbind.data.frame(autocorr_PB306_CONTROL_P3p_SS_mod,autocorr_PB306_MA_P3p_SS_mod,autocorr_PB306_Vm_P3p_SS_mod)
  
  #PB306_P4p_autocorr
  autocorr_PB306_CONTROL_P4p_SS_mod <- t(as.data.frame(autocorr.diag(PB306_CONTROL_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_CONTROL_P4p_SS_mod) <- c("PB306_CONTROL_P4p_SS_mod")
  
  autocorr_PB306_MA_P4p_SS_mod <- t(as.data.frame(autocorr.diag(PB306_MA_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_MA_P4p_SS_mod) <- c("PB306_MA_P4p_SS_mod")
  
  autocorr_PB306_Vm_P4p_SS_mod <- t(as.data.frame(autocorr.diag(PB306_Vm_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_Vm_P4p_SS_mod) <- c("PB306_Vm_P4p_SS_mod")
  
  autocorr_PB306_P4p_SS_mod <- rbind.data.frame(autocorr_PB306_CONTROL_P4p_SS_mod,autocorr_PB306_MA_P4p_SS_mod,autocorr_PB306_Vm_P4p_SS_mod)
  
  #PB306_P8p_autocorr
  autocorr_PB306_CONTROL_P8p_SS_mod <- t(as.data.frame(autocorr.diag(PB306_CONTROL_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_CONTROL_P8p_SS_mod) <- c("PB306_CONTROL_P8p_SS_mod")
  
  autocorr_PB306_MA_P8p_SS_mod <- t(as.data.frame(autocorr.diag(PB306_MA_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_MA_P8p_SS_mod) <- c("PB306_MA_P8p_SS_mod")
  
  autocorr_PB306_Vm_P8p_SS_mod <- t(as.data.frame(autocorr.diag(PB306_Vm_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_Vm_P8p_SS_mod) <- c("PB306_Vm_P8p_SS_mod")
  
  
  autocorr_PB306_P8p_SS_mod <- rbind.data.frame(autocorr_PB306_CONTROL_P8p_SS_mod,autocorr_PB306_MA_P8p_SS_mod,autocorr_PB306_Vm_P8p_SS_mod)
  
  #PB306_P5p_autocorr
  autocorr_PB306_CONTROL_P5p_wt_mod <- t(as.data.frame(autocorr.diag(PB306_CONTROL_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_CONTROL_P5p_wt_mod) <- c("PB306_CONTROL_P5p_wt_mod")
  
  autocorr_PB306_MA_P5p_wt_mod <- t(as.data.frame(autocorr.diag(PB306_MA_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_MA_P5p_wt_mod) <- c("PB306_MA_P5p_wt_mod")
  
  autocorr_PB306_Vm_P5p_wt_mod <- t(as.data.frame(autocorr.diag(PB306_Vm_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_Vm_P5p_wt_mod) <- c("PB306_Vm_P5p_wt_mod")
  
  autocorr_PB306_P5p_wt_mod <- rbind.data.frame(autocorr_PB306_CONTROL_P5p_wt_mod,autocorr_PB306_MA_P5p_wt_mod,autocorr_PB306_Vm_P5p_wt_mod)
  
  
  #PB306_P6p_autocorr
  autocorr_PB306_CONTROL_P6p_wt_mod <- t(as.data.frame(autocorr.diag(PB306_CONTROL_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_CONTROL_P6p_wt_mod) <- c("PB306_CONTROL_P6p_wt_mod")
  
  autocorr_PB306_MA_P6p_wt_mod <- t(as.data.frame(autocorr.diag(PB306_MA_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_MA_P6p_wt_mod) <- c("PB306_MA_P6p_wt_mod")
  
  autocorr_PB306_Vm_P6p_wt_mod <- t(as.data.frame(autocorr.diag(PB306_Vm_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_Vm_P6p_wt_mod) <- c("PB306_Vm_P6p_wt_mod")
  
  
  autocorr_PB306_P6p_wt_mod <- rbind.data.frame(autocorr_PB306_CONTROL_P6p_wt_mod,autocorr_PB306_MA_P6p_wt_mod,autocorr_PB306_Vm_P6p_wt_mod)
  
  #PB306_P7p_autocorr
  autocorr_PB306_CONTROL_P7p_wt_mod <- t(as.data.frame(autocorr.diag(PB306_CONTROL_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_CONTROL_P7p_wt_mod) <- c("PB306_CONTROL_P7p_wt_mod")
  
  autocorr_PB306_MA_P7p_wt_mod <- t(as.data.frame(autocorr.diag(PB306_MA_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_MA_P7p_wt_mod) <- c("PB306_MA_P7p_wt_mod")
  
  autocorr_PB306_Vm_P7p_wt_mod <- t(as.data.frame(autocorr.diag(PB306_Vm_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB306_Vm_P7p_wt_mod) <- c("PB306_Vm_P7p_wt_mod")
  
  autocorr_PB306_P7p_wt_mod <- rbind.data.frame(autocorr_PB306_CONTROL_P7p_wt_mod,autocorr_PB306_MA_P7p_wt_mod,autocorr_PB306_Vm_P7p_wt_mod)
  
  
  #PB306_autocorr_SS_vulva
  autocorr_PB306_SS_vulva <- rbind.data.frame(autocorr_PB306_P3p_SS_mod,autocorr_PB306_P4p_SS_mod,autocorr_PB306_P5p_wt_mod,autocorr_PB306_P6p_wt_mod,autocorr_PB306_P7p_wt_mod,autocorr_PB306_P8p_SS_mod)
  autocorr_PB306_SS_vulva
}

#AF16
{
  #AF16_P3p_autocorr
  autocorr_AF16_CONTROL_P3p_SS_mod <- t(as.data.frame(autocorr.diag(AF16_CONTROL_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_CONTROL_P3p_SS_mod) <- c("AF16_CONTROL_P3p_SS_mod")
  
  autocorr_AF16_MA_P3p_SS_mod <- t(as.data.frame(autocorr.diag(AF16_MA_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_MA_P3p_SS_mod) <- c("AF16_MA_P3p_SS_mod")
  
  autocorr_AF16_Vm_P3p_SS_mod <- t(as.data.frame(autocorr.diag(AF16_Vm_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_Vm_P3p_SS_mod) <- c("AF16_Vm_P3p_SS_mod")
  
  autocorr_AF16_P3p_SS_mod <- rbind.data.frame(autocorr_AF16_CONTROL_P3p_SS_mod,autocorr_AF16_MA_P3p_SS_mod,autocorr_AF16_Vm_P3p_SS_mod)
  
  #AF16_P4p_autocorr
  autocorr_AF16_CONTROL_P4p_SS_mod <- t(as.data.frame(autocorr.diag(AF16_CONTROL_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_CONTROL_P4p_SS_mod) <- c("AF16_CONTROL_P4p_SS_mod")
  
  autocorr_AF16_MA_P4p_SS_mod <- t(as.data.frame(autocorr.diag(AF16_MA_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_MA_P4p_SS_mod) <- c("AF16_MA_P4p_SS_mod")
  
  autocorr_AF16_Vm_P4p_SS_mod <- t(as.data.frame(autocorr.diag(AF16_Vm_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_Vm_P4p_SS_mod) <- c("AF16_Vm_P4p_SS_mod")
  
  autocorr_AF16_P4p_SS_mod <- rbind.data.frame(autocorr_AF16_CONTROL_P4p_SS_mod,autocorr_AF16_MA_P4p_SS_mod,autocorr_AF16_Vm_P4p_SS_mod)
  
  #AF16_P8p_autocorr
  autocorr_AF16_CONTROL_P8p_SS_mod <- t(as.data.frame(autocorr.diag(AF16_CONTROL_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_CONTROL_P8p_SS_mod) <- c("AF16_CONTROL_P8p_SS_mod")
  
  autocorr_AF16_MA_P8p_SS_mod <- t(as.data.frame(autocorr.diag(AF16_MA_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_MA_P8p_SS_mod) <- c("AF16_MA_P8p_SS_mod")
  
  autocorr_AF16_Vm_P8p_SS_mod <- t(as.data.frame(autocorr.diag(AF16_Vm_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_Vm_P8p_SS_mod) <- c("AF16_Vm_P8p_SS_mod")
  
  
  autocorr_AF16_P8p_SS_mod <- rbind.data.frame(autocorr_AF16_CONTROL_P8p_SS_mod,autocorr_AF16_MA_P8p_SS_mod,autocorr_AF16_Vm_P8p_SS_mod)
  
  #AF16_P5p_autocorr
  autocorr_AF16_CONTROL_P5p_wt_mod <- t(as.data.frame(autocorr.diag(AF16_CONTROL_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_CONTROL_P5p_wt_mod) <- c("AF16_CONTROL_P5p_wt_mod")
  
  autocorr_AF16_MA_P5p_wt_mod <- t(as.data.frame(autocorr.diag(AF16_MA_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_MA_P5p_wt_mod) <- c("AF16_MA_P5p_wt_mod")
  
  autocorr_AF16_Vm_P5p_wt_mod <- t(as.data.frame(autocorr.diag(AF16_Vm_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_Vm_P5p_wt_mod) <- c("AF16_Vm_P5p_wt_mod")
  
  autocorr_AF16_P5p_wt_mod <- rbind.data.frame(autocorr_AF16_CONTROL_P5p_wt_mod,autocorr_AF16_MA_P5p_wt_mod,autocorr_AF16_Vm_P5p_wt_mod)
  
  
  #AF16_P6p_autocorr
  autocorr_AF16_CONTROL_P6p_wt_mod <- t(as.data.frame(autocorr.diag(AF16_CONTROL_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_CONTROL_P6p_wt_mod) <- c("AF16_CONTROL_P6p_wt_mod")
  
  autocorr_AF16_MA_P6p_wt_mod <- t(as.data.frame(autocorr.diag(AF16_MA_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_MA_P6p_wt_mod) <- c("AF16_MA_P6p_wt_mod")
  
  autocorr_AF16_Vm_P6p_wt_mod <- t(as.data.frame(autocorr.diag(AF16_Vm_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_Vm_P6p_wt_mod) <- c("AF16_Vm_P6p_wt_mod")
  
  
  autocorr_AF16_P6p_wt_mod <- rbind.data.frame(autocorr_AF16_CONTROL_P6p_wt_mod,autocorr_AF16_MA_P6p_wt_mod,autocorr_AF16_Vm_P6p_wt_mod)
  
  #AF16_P7p_autocorr
  autocorr_AF16_CONTROL_P7p_wt_mod <- t(as.data.frame(autocorr.diag(AF16_CONTROL_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_CONTROL_P7p_wt_mod) <- c("AF16_CONTROL_P7p_wt_mod")
  
  autocorr_AF16_MA_P7p_wt_mod <- t(as.data.frame(autocorr.diag(AF16_MA_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_MA_P7p_wt_mod) <- c("AF16_MA_P7p_wt_mod")
  
  autocorr_AF16_Vm_P7p_wt_mod <- t(as.data.frame(autocorr.diag(AF16_Vm_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_AF16_Vm_P7p_wt_mod) <- c("AF16_Vm_P7p_wt_mod")
  
  autocorr_AF16_P7p_wt_mod <- rbind.data.frame(autocorr_AF16_CONTROL_P7p_wt_mod,autocorr_AF16_MA_P7p_wt_mod,autocorr_AF16_Vm_P7p_wt_mod)
  
  
  #AF16_autocorr_SS_vulva
  autocorr_AF16_SS_vulva <- rbind.data.frame(autocorr_AF16_P3p_SS_mod,autocorr_AF16_P4p_SS_mod,autocorr_AF16_P5p_wt_mod,autocorr_AF16_P6p_wt_mod,autocorr_AF16_P7p_wt_mod,autocorr_AF16_P8p_SS_mod)
  autocorr_AF16_SS_vulva
}

#PB800
{
  #PB800_P3p_autocorr
  autocorr_PB800_CONTROL_P3p_SS_mod <- t(as.data.frame(autocorr.diag(PB800_CONTROL_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_CONTROL_P3p_SS_mod) <- c("PB800_CONTROL_P3p_SS_mod")
  
  autocorr_PB800_MA_P3p_SS_mod <- t(as.data.frame(autocorr.diag(PB800_MA_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_MA_P3p_SS_mod) <- c("PB800_MA_P3p_SS_mod")
  
  autocorr_PB800_Vm_P3p_SS_mod <- t(as.data.frame(autocorr.diag(PB800_Vm_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_Vm_P3p_SS_mod) <- c("PB800_Vm_P3p_SS_mod")
  
  autocorr_PB800_P3p_SS_mod <- rbind.data.frame(autocorr_PB800_CONTROL_P3p_SS_mod,autocorr_PB800_MA_P3p_SS_mod,autocorr_PB800_Vm_P3p_SS_mod)
  
  #PB800_P4p_autocorr
  autocorr_PB800_CONTROL_P4p_SS_mod <- t(as.data.frame(autocorr.diag(PB800_CONTROL_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_CONTROL_P4p_SS_mod) <- c("PB800_CONTROL_P4p_SS_mod")
  
  autocorr_PB800_MA_P4p_SS_mod <- t(as.data.frame(autocorr.diag(PB800_MA_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_MA_P4p_SS_mod) <- c("PB800_MA_P4p_SS_mod")
  
  autocorr_PB800_Vm_P4p_SS_mod <- t(as.data.frame(autocorr.diag(PB800_Vm_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_Vm_P4p_SS_mod) <- c("PB800_Vm_P4p_SS_mod")
  
  autocorr_PB800_P4p_SS_mod <- rbind.data.frame(autocorr_PB800_CONTROL_P4p_SS_mod,autocorr_PB800_MA_P4p_SS_mod,autocorr_PB800_Vm_P4p_SS_mod)
  
  #PB800_P8p_autocorr
  autocorr_PB800_CONTROL_P8p_SS_mod <- t(as.data.frame(autocorr.diag(PB800_CONTROL_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_CONTROL_P8p_SS_mod) <- c("PB800_CONTROL_P8p_SS_mod")
  
  autocorr_PB800_MA_P8p_SS_mod <- t(as.data.frame(autocorr.diag(PB800_MA_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_MA_P8p_SS_mod) <- c("PB800_MA_P8p_SS_mod")
  
  autocorr_PB800_Vm_P8p_SS_mod <- t(as.data.frame(autocorr.diag(PB800_Vm_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_Vm_P8p_SS_mod) <- c("PB800_Vm_P8p_SS_mod")
  
  
  autocorr_PB800_P8p_SS_mod <- rbind.data.frame(autocorr_PB800_CONTROL_P8p_SS_mod,autocorr_PB800_MA_P8p_SS_mod,autocorr_PB800_Vm_P8p_SS_mod)
  
  #PB800_P5p_autocorr
  autocorr_PB800_CONTROL_P5p_wt_mod <- t(as.data.frame(autocorr.diag(PB800_CONTROL_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_CONTROL_P5p_wt_mod) <- c("PB800_CONTROL_P5p_wt_mod")
  
  autocorr_PB800_MA_P5p_wt_mod <- t(as.data.frame(autocorr.diag(PB800_MA_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_MA_P5p_wt_mod) <- c("PB800_MA_P5p_wt_mod")
  
  autocorr_PB800_Vm_P5p_wt_mod <- t(as.data.frame(autocorr.diag(PB800_Vm_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_Vm_P5p_wt_mod) <- c("PB800_Vm_P5p_wt_mod")
  
  autocorr_PB800_P5p_wt_mod <- rbind.data.frame(autocorr_PB800_CONTROL_P5p_wt_mod,autocorr_PB800_MA_P5p_wt_mod,autocorr_PB800_Vm_P5p_wt_mod)
  
  
  #PB800_P6p_autocorr
  autocorr_PB800_CONTROL_P6p_wt_mod <- t(as.data.frame(autocorr.diag(PB800_CONTROL_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_CONTROL_P6p_wt_mod) <- c("PB800_CONTROL_P6p_wt_mod")
  
  autocorr_PB800_MA_P6p_wt_mod <- t(as.data.frame(autocorr.diag(PB800_MA_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_MA_P6p_wt_mod) <- c("PB800_MA_P6p_wt_mod")
  
  autocorr_PB800_Vm_P6p_wt_mod <- t(as.data.frame(autocorr.diag(PB800_Vm_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_Vm_P6p_wt_mod) <- c("PB800_Vm_P6p_wt_mod")
  
  
  autocorr_PB800_P6p_wt_mod <- rbind.data.frame(autocorr_PB800_CONTROL_P6p_wt_mod,autocorr_PB800_MA_P6p_wt_mod,autocorr_PB800_Vm_P6p_wt_mod)
  
  #PB800_P7p_autocorr
  autocorr_PB800_CONTROL_P7p_wt_mod <- t(as.data.frame(autocorr.diag(PB800_CONTROL_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_CONTROL_P7p_wt_mod) <- c("PB800_CONTROL_P7p_wt_mod")
  
  autocorr_PB800_MA_P7p_wt_mod <- t(as.data.frame(autocorr.diag(PB800_MA_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_MA_P7p_wt_mod) <- c("PB800_MA_P7p_wt_mod")
  
  autocorr_PB800_Vm_P7p_wt_mod <- t(as.data.frame(autocorr.diag(PB800_Vm_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_PB800_Vm_P7p_wt_mod) <- c("PB800_Vm_P7p_wt_mod")
  
  autocorr_PB800_P7p_wt_mod <- rbind.data.frame(autocorr_PB800_CONTROL_P7p_wt_mod,autocorr_PB800_MA_P7p_wt_mod,autocorr_PB800_Vm_P7p_wt_mod)
  
  
  #PB800_autocorr_SS_vulva
  autocorr_PB800_SS_vulva <- rbind.data.frame(autocorr_PB800_P3p_SS_mod,autocorr_PB800_P4p_SS_mod,autocorr_PB800_P5p_wt_mod,autocorr_PB800_P6p_wt_mod,autocorr_PB800_P7p_wt_mod,autocorr_PB800_P8p_SS_mod)
  autocorr_PB800_SS_vulva
}


Vm_Caenorhabditis_isolate_SS_autocorr <-  rbind.data.frame(autocorr_JU1200_SS_vulva,autocorr_PB306_SS_vulva,autocorr_AF16_SS_vulva,autocorr_PB800_SS_vulva)
View(Vm_Caenorhabditis_isolate_SS_autocorr)
Vm_Caenorhabditis_isolate_SS_autocorr <- cbind(Models = rownames(Vm_Caenorhabditis_isolate_SS_autocorr),Vm_Caenorhabditis_isolate_SS_autocorr)


write_xlsx(Vm_Caenorhabditis_isolate_SS_autocorr, "Vm_Caenorhabditis_isolate_SS_autocorr.xlsx")

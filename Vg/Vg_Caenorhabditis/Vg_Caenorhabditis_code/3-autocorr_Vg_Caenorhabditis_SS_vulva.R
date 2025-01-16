#autocorr_Vg_Caenorhabditis_SS_vulva


Celegans_CONTROL_P3p_SS_mod
#Celegans
{
  #Celegans_P3p_autocorr
  autocorr_Celegans_CONTROL_P3p_SS_mod <- t(as.data.frame(autocorr.diag(Celegans_CONTROL_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_CONTROL_P3p_SS_mod) <- c("Celegans_CONTROL_P3p_SS_mod")
  
  autocorr_Celegans_WILD_P3p_SS_mod <- t(as.data.frame(autocorr.diag(Celegans_WILD_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_WILD_P3p_SS_mod) <- c("Celegans_WILD_P3p_SS_mod")
  
  autocorr_Celegans_Vg_P3p_SS_mod <- t(as.data.frame(autocorr.diag(Celegans_Vg_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_Vg_P3p_SS_mod) <- c("Celegans_Vg_P3p_SS_mod")
  
  autocorr_Celegans_P3p_SS_mod <- rbind.data.frame(autocorr_Celegans_CONTROL_P3p_SS_mod,autocorr_Celegans_WILD_P3p_SS_mod,autocorr_Celegans_Vg_P3p_SS_mod)
  
  #Celegans_P4p_autocorr
  autocorr_Celegans_CONTROL_P4p_SS_mod <- t(as.data.frame(autocorr.diag(Celegans_CONTROL_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_CONTROL_P4p_SS_mod) <- c("Celegans_CONTROL_P4p_SS_mod")
  
  autocorr_Celegans_WILD_P4p_SS_mod <- t(as.data.frame(autocorr.diag(Celegans_WILD_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_WILD_P4p_SS_mod) <- c("Celegans_WILD_P4p_SS_mod")
  
  autocorr_Celegans_Vg_P4p_SS_mod <- t(as.data.frame(autocorr.diag(Celegans_Vg_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_Vg_P4p_SS_mod) <- c("Celegans_Vg_P4p_SS_mod")
  
  autocorr_Celegans_P4p_SS_mod <- rbind.data.frame(autocorr_Celegans_CONTROL_P4p_SS_mod,autocorr_Celegans_WILD_P4p_SS_mod,autocorr_Celegans_Vg_P4p_SS_mod)
  
  #Celegans_P8p_autocorr
  autocorr_Celegans_CONTROL_P8p_SS_mod <- t(as.data.frame(autocorr.diag(Celegans_CONTROL_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_CONTROL_P8p_SS_mod) <- c("Celegans_CONTROL_P8p_SS_mod")
  
  autocorr_Celegans_WILD_P8p_SS_mod <- t(as.data.frame(autocorr.diag(Celegans_WILD_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_WILD_P8p_SS_mod) <- c("Celegans_WILD_P8p_SS_mod")
  
  autocorr_Celegans_Vg_P8p_SS_mod <- t(as.data.frame(autocorr.diag(Celegans_Vg_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_Vg_P8p_SS_mod) <- c("Celegans_Vg_P8p_SS_mod")
  
  
  autocorr_Celegans_P8p_SS_mod <- rbind.data.frame(autocorr_Celegans_CONTROL_P8p_SS_mod,autocorr_Celegans_WILD_P8p_SS_mod,autocorr_Celegans_Vg_P8p_SS_mod)
  
  #Celegans_P5p_autocorr
  autocorr_Celegans_CONTROL_P5p_wt_mod <- t(as.data.frame(autocorr.diag(Celegans_CONTROL_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_CONTROL_P5p_wt_mod) <- c("Celegans_CONTROL_P5p_wt_mod")
  
  autocorr_Celegans_WILD_P5p_wt_mod <- t(as.data.frame(autocorr.diag(Celegans_WILD_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_WILD_P5p_wt_mod) <- c("Celegans_WILD_P5p_wt_mod")
  
  autocorr_Celegans_Vg_P5p_wt_mod <- t(as.data.frame(autocorr.diag(Celegans_Vg_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_Vg_P5p_wt_mod) <- c("Celegans_Vg_P5p_wt_mod")
  
  autocorr_Celegans_P5p_wt_mod <- rbind.data.frame(autocorr_Celegans_CONTROL_P5p_wt_mod,autocorr_Celegans_WILD_P5p_wt_mod,autocorr_Celegans_Vg_P5p_wt_mod)
  
  
  #Celegans_P6p_autocorr
  autocorr_Celegans_CONTROL_P6p_wt_mod <- t(as.data.frame(autocorr.diag(Celegans_CONTROL_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_CONTROL_P6p_wt_mod) <- c("Celegans_CONTROL_P6p_wt_mod")
  
  autocorr_Celegans_WILD_P6p_wt_mod <- t(as.data.frame(autocorr.diag(Celegans_WILD_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_WILD_P6p_wt_mod) <- c("Celegans_WILD_P6p_wt_mod")
  
  autocorr_Celegans_Vg_P6p_wt_mod <- t(as.data.frame(autocorr.diag(Celegans_Vg_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_Vg_P6p_wt_mod) <- c("Celegans_Vg_P6p_wt_mod")
  
  
  autocorr_Celegans_P6p_wt_mod <- rbind.data.frame(autocorr_Celegans_CONTROL_P6p_wt_mod,autocorr_Celegans_WILD_P6p_wt_mod,autocorr_Celegans_Vg_P6p_wt_mod)
  
  #Celegans_P7p_autocorr
  autocorr_Celegans_CONTROL_P7p_wt_mod <- t(as.data.frame(autocorr.diag(Celegans_CONTROL_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_CONTROL_P7p_wt_mod) <- c("Celegans_CONTROL_P7p_wt_mod")
  
  autocorr_Celegans_WILD_P7p_wt_mod <- t(as.data.frame(autocorr.diag(Celegans_WILD_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_WILD_P7p_wt_mod) <- c("Celegans_WILD_P7p_wt_mod")
  
  autocorr_Celegans_Vg_P7p_wt_mod <- t(as.data.frame(autocorr.diag(Celegans_Vg_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Celegans_Vg_P7p_wt_mod) <- c("Celegans_Vg_P7p_wt_mod")
  
  autocorr_Celegans_P7p_wt_mod <- rbind.data.frame(autocorr_Celegans_CONTROL_P7p_wt_mod,autocorr_Celegans_WILD_P7p_wt_mod,autocorr_Celegans_Vg_P7p_wt_mod)
  
  
  #Celegans_autocorr_SS_vulva
  autocorr_Celegans_SS_vulva <- rbind.data.frame(autocorr_Celegans_P3p_SS_mod,autocorr_Celegans_P4p_SS_mod,autocorr_Celegans_P5p_wt_mod,autocorr_Celegans_P6p_wt_mod,autocorr_Celegans_P7p_wt_mod,autocorr_Celegans_P8p_SS_mod)
  
}



#Cbriggsae
{
  #Cbriggsae_P3p_autocorr
  autocorr_Cbriggsae_CONTROL_P3p_SS_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_CONTROL_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_CONTROL_P3p_SS_mod) <- c("Cbriggsae_CONTROL_P3p_SS_mod")
  
  autocorr_Cbriggsae_WILD_P3p_SS_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_WILD_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_WILD_P3p_SS_mod) <- c("Cbriggsae_WILD_P3p_SS_mod")
  
  autocorr_Cbriggsae_Vg_P3p_SS_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_Vg_P3p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_Vg_P3p_SS_mod) <- c("Cbriggsae_Vg_P3p_SS_mod")
  
  autocorr_Cbriggsae_P3p_SS_mod <- rbind.data.frame(autocorr_Cbriggsae_CONTROL_P3p_SS_mod,autocorr_Cbriggsae_WILD_P3p_SS_mod,autocorr_Cbriggsae_Vg_P3p_SS_mod)
  
  #Cbriggsae_P4p_autocorr
  autocorr_Cbriggsae_CONTROL_P4p_SS_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_CONTROL_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_CONTROL_P4p_SS_mod) <- c("Cbriggsae_CONTROL_P4p_SS_mod")
  
  autocorr_Cbriggsae_WILD_P4p_SS_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_WILD_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_WILD_P4p_SS_mod) <- c("Cbriggsae_WILD_P4p_SS_mod")
  
  autocorr_Cbriggsae_Vg_P4p_SS_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_Vg_P4p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_Vg_P4p_SS_mod) <- c("Cbriggsae_Vg_P4p_SS_mod")
  
  autocorr_Cbriggsae_P4p_SS_mod <- rbind.data.frame(autocorr_Cbriggsae_CONTROL_P4p_SS_mod,autocorr_Cbriggsae_WILD_P4p_SS_mod,autocorr_Cbriggsae_Vg_P4p_SS_mod)
  
  #Cbriggsae_P8p_autocorr
  autocorr_Cbriggsae_CONTROL_P8p_SS_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_CONTROL_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_CONTROL_P8p_SS_mod) <- c("Cbriggsae_CONTROL_P8p_SS_mod")
  
  autocorr_Cbriggsae_WILD_P8p_SS_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_WILD_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_WILD_P8p_SS_mod) <- c("Cbriggsae_WILD_P8p_SS_mod")
  
  autocorr_Cbriggsae_Vg_P8p_SS_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_Vg_P8p_SS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_Vg_P8p_SS_mod) <- c("Cbriggsae_Vg_P8p_SS_mod")
  
  
  autocorr_Cbriggsae_P8p_SS_mod <- rbind.data.frame(autocorr_Cbriggsae_CONTROL_P8p_SS_mod,autocorr_Cbriggsae_WILD_P8p_SS_mod,autocorr_Cbriggsae_Vg_P8p_SS_mod)
  
  #Cbriggsae_P5p_autocorr
  autocorr_Cbriggsae_CONTROL_P5p_wt_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_CONTROL_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_CONTROL_P5p_wt_mod) <- c("Cbriggsae_CONTROL_P5p_wt_mod")
  
  autocorr_Cbriggsae_WILD_P5p_wt_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_WILD_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_WILD_P5p_wt_mod) <- c("Cbriggsae_WILD_P5p_wt_mod")
  
  autocorr_Cbriggsae_Vg_P5p_wt_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_Vg_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_Vg_P5p_wt_mod) <- c("Cbriggsae_Vg_P5p_wt_mod")
  
  autocorr_Cbriggsae_P5p_wt_mod <- rbind.data.frame(autocorr_Cbriggsae_CONTROL_P5p_wt_mod,autocorr_Cbriggsae_WILD_P5p_wt_mod,autocorr_Cbriggsae_Vg_P5p_wt_mod)
  
  
  #Cbriggsae_P6p_autocorr
  autocorr_Cbriggsae_CONTROL_P6p_wt_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_CONTROL_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_CONTROL_P6p_wt_mod) <- c("Cbriggsae_CONTROL_P6p_wt_mod")
  
  autocorr_Cbriggsae_WILD_P6p_wt_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_WILD_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_WILD_P6p_wt_mod) <- c("Cbriggsae_WILD_P6p_wt_mod")
  
  autocorr_Cbriggsae_Vg_P6p_wt_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_Vg_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_Vg_P6p_wt_mod) <- c("Cbriggsae_Vg_P6p_wt_mod")
  
  
  autocorr_Cbriggsae_P6p_wt_mod <- rbind.data.frame(autocorr_Cbriggsae_CONTROL_P6p_wt_mod,autocorr_Cbriggsae_WILD_P6p_wt_mod,autocorr_Cbriggsae_Vg_P6p_wt_mod)
  
  #Cbriggsae_P7p_autocorr
  autocorr_Cbriggsae_CONTROL_P7p_wt_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_CONTROL_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_CONTROL_P7p_wt_mod) <- c("Cbriggsae_CONTROL_P7p_wt_mod")
  
  autocorr_Cbriggsae_WILD_P7p_wt_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_WILD_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_WILD_P7p_wt_mod) <- c("Cbriggsae_WILD_P7p_wt_mod")
  
  autocorr_Cbriggsae_Vg_P7p_wt_mod <- t(as.data.frame(autocorr.diag(Cbriggsae_Vg_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Cbriggsae_Vg_P7p_wt_mod) <- c("Cbriggsae_Vg_P7p_wt_mod")
  
  autocorr_Cbriggsae_P7p_wt_mod <- rbind.data.frame(autocorr_Cbriggsae_CONTROL_P7p_wt_mod,autocorr_Cbriggsae_WILD_P7p_wt_mod,autocorr_Cbriggsae_Vg_P7p_wt_mod)
  
  
  #Cbriggsae_autocorr_SS_vulva
  autocorr_Cbriggsae_SS_vulva <- rbind.data.frame(autocorr_Cbriggsae_P3p_SS_mod,autocorr_Cbriggsae_P4p_SS_mod,autocorr_Cbriggsae_P5p_wt_mod,autocorr_Cbriggsae_P6p_wt_mod,autocorr_Cbriggsae_P7p_wt_mod,autocorr_Cbriggsae_P8p_SS_mod)
  autocorr_Cbriggsae_SS_vulva
}




Vg_Caenorhabditis_isolate_SS_autocorr <-  rbind.data.frame(autocorr_Celegans_SS_vulva,autocorr_Cbriggsae_SS_vulva)
View(Vg_Caenorhabditis_isolate_SS_autocorr)
Vg_Caenorhabditis_isolate_SS_autocorr <- cbind(Models = rownames(Vg_Caenorhabditis_isolate_SS_autocorr),Vg_Caenorhabditis_isolate_SS_autocorr)


write_xlsx(Vg_Caenorhabditis_isolate_SS_autocorr, "Vg_Caenorhabditis_isolate_SS_autocorr.xlsx")



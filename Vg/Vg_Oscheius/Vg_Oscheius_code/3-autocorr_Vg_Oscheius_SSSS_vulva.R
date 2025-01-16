#autocorr_Vg_Oscheius_SSSS_vulva



#Otipulae
{
  #Otipulae_P3p_autocorr
  autocorr_Otipulae_CONTROL_P3p_SSSS_mod_2 <- t(as.data.frame(autocorr.diag(Otipulae_CONTROL_P3p_SSSS_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_CONTROL_P3p_SSSS_mod_2) <- c("Otipulae_CONTROL_P3p_SSSS_mod_2")
  
  autocorr_Otipulae_WILD_P3p_SSSS_mod <- t(as.data.frame(autocorr.diag(Otipulae_WILD_P3p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_WILD_P3p_SSSS_mod) <- c("Otipulae_WILD_P3p_SSSS_mod")
  
  autocorr_Otipulae_Vg_P3p_SSSS_mod_2 <- t(as.data.frame(autocorr.diag(Otipulae_Vg_P3p_SSSS_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_Vg_P3p_SSSS_mod_2) <- c("Otipulae_Vg_P3p_SSSS_mod_2")
  
  autocorr_Otipulae_P3p_SSSS_mod <- rbind.data.frame(autocorr_Otipulae_CONTROL_P3p_SSSS_mod_2,autocorr_Otipulae_WILD_P3p_SSSS_mod,autocorr_Otipulae_Vg_P3p_SSSS_mod_2)
  
  #Otipulae_P4p_autocorr
  autocorr_Otipulae_CONTROL_P4p_SSSS_mod_2 <- t(as.data.frame(autocorr.diag(Otipulae_CONTROL_P4p_SSSS_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_CONTROL_P4p_SSSS_mod_2) <- c("Otipulae_CONTROL_P4p_SSSS_mod_2")
  
  autocorr_Otipulae_WILD_P4p_SSSS_mod <- t(as.data.frame(autocorr.diag(Otipulae_WILD_P4p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_WILD_P4p_SSSS_mod) <- c("Otipulae_WILD_P4p_SSSS_mod")
  
  autocorr_Otipulae_Vg_P4p_SSSS_mod_2 <- t(as.data.frame(autocorr.diag(Otipulae_Vg_P4p_SSSS_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_Vg_P4p_SSSS_mod_2) <- c("Otipulae_Vg_P4p_SSSS_mod_2")
  
  autocorr_Otipulae_P4p_SSSS_mod <- rbind.data.frame(autocorr_Otipulae_CONTROL_P4p_SSSS_mod_2,autocorr_Otipulae_WILD_P4p_SSSS_mod,autocorr_Otipulae_Vg_P4p_SSSS_mod_2)
  
  #Otipulae_P8p_autocorr
  autocorr_Otipulae_CONTROL_P8p_SSSS_mod_2 <- t(as.data.frame(autocorr.diag(Otipulae_CONTROL_P8p_SSSS_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_CONTROL_P8p_SSSS_mod_2) <- c("Otipulae_CONTROL_P8p_SSSS_mod_2")
  
  autocorr_Otipulae_WILD_P8p_SSSS_mod <- t(as.data.frame(autocorr.diag(Otipulae_WILD_P8p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_WILD_P8p_SSSS_mod) <- c("Otipulae_WILD_P8p_SSSS_mod")
  
  autocorr_Otipulae_Vg_P8p_SSSS_mod_2 <- t(as.data.frame(autocorr.diag(Otipulae_Vg_P8p_SSSS_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_Vg_P8p_SSSS_mod_2) <- c("Otipulae_Vg_P8p_SSSS_mod_2")
  
  
  autocorr_Otipulae_P8p_SSSS_mod <- rbind.data.frame(autocorr_Otipulae_CONTROL_P8p_SSSS_mod_2,autocorr_Otipulae_WILD_P8p_SSSS_mod,autocorr_Otipulae_Vg_P8p_SSSS_mod_2)
  
  #Otipulae_P5p_autocorr
  autocorr_Otipulae_CONTROL_P5p_wt_mod_2 <- t(as.data.frame(autocorr.diag(Otipulae_CONTROL_P5p_wt_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_CONTROL_P5p_wt_mod_2) <- c("Otipulae_CONTROL_P5p_wt_mod_2")
  
  autocorr_Otipulae_WILD_P5p_wt_mod <- t(as.data.frame(autocorr.diag(Otipulae_WILD_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_WILD_P5p_wt_mod) <- c("Otipulae_WILD_P5p_wt_mod")
  
  autocorr_Otipulae_Vg_P5p_wt_mod_2 <- t(as.data.frame(autocorr.diag(Otipulae_Vg_P5p_wt_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_Vg_P5p_wt_mod_2) <- c("Otipulae_Vg_P5p_wt_mod_2")
  
  autocorr_Otipulae_P5p_wt_mod <- rbind.data.frame(autocorr_Otipulae_CONTROL_P5p_wt_mod_2,autocorr_Otipulae_WILD_P5p_wt_mod,autocorr_Otipulae_Vg_P5p_wt_mod_2)
  
  
  #Otipulae_P6p_autocorr
  autocorr_Otipulae_CONTROL_P6p_wt_mod_2 <- t(as.data.frame(autocorr.diag(Otipulae_CONTROL_P6p_wt_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_CONTROL_P6p_wt_mod_2) <- c("Otipulae_CONTROL_P6p_wt_mod_2")
  
  autocorr_Otipulae_WILD_P6p_wt_mod <- t(as.data.frame(autocorr.diag(Otipulae_WILD_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_WILD_P6p_wt_mod) <- c("Otipulae_WILD_P6p_wt_mod")
  
  autocorr_Otipulae_Vg_P6p_wt_mod_2 <- t(as.data.frame(autocorr.diag(Otipulae_Vg_P6p_wt_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_Vg_P6p_wt_mod_2) <- c("Otipulae_Vg_P6p_wt_mod_2")
  
  
  autocorr_Otipulae_P6p_wt_mod <- rbind.data.frame(autocorr_Otipulae_CONTROL_P6p_wt_mod_2,autocorr_Otipulae_WILD_P6p_wt_mod,autocorr_Otipulae_Vg_P6p_wt_mod_2)
  
  #Otipulae_P7p_autocorr
  autocorr_Otipulae_CONTROL_P7p_wt_mod_2 <- t(as.data.frame(autocorr.diag(Otipulae_CONTROL_P7p_wt_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_CONTROL_P7p_wt_mod_2) <- c("Otipulae_CONTROL_P7p_wt_mod_2")
  
  autocorr_Otipulae_WILD_P7p_wt_mod <- t(as.data.frame(autocorr.diag(Otipulae_WILD_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_WILD_P7p_wt_mod) <- c("Otipulae_WILD_P7p_wt_mod")
  
  autocorr_Otipulae_Vg_P7p_wt_mod_2 <- t(as.data.frame(autocorr.diag(Otipulae_Vg_P7p_wt_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Otipulae_Vg_P7p_wt_mod_2) <- c("Otipulae_Vg_P7p_wt_mod_2")
  
  autocorr_Otipulae_P7p_wt_mod <- rbind.data.frame(autocorr_Otipulae_CONTROL_P7p_wt_mod_2,autocorr_Otipulae_WILD_P7p_wt_mod,autocorr_Otipulae_Vg_P7p_wt_mod_2)
  
  
  #Otipulae_autocorr_SSSS_vulva
  autocorr_Otipulae_SSSS_vulva <- rbind.data.frame(autocorr_Otipulae_P3p_SSSS_mod,autocorr_Otipulae_P4p_SSSS_mod,autocorr_Otipulae_P5p_wt_mod,autocorr_Otipulae_P6p_wt_mod,autocorr_Otipulae_P7p_wt_mod,autocorr_Otipulae_P8p_SSSS_mod)
  
}



#Oonirici
{
  #Oonirici_P3p_autocorr
  autocorr_Oonirici_CONTROL_P3p_SSSS_mod_2 <- t(as.data.frame(autocorr.diag(Oonirici_CONTROL_P3p_SSSS_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_CONTROL_P3p_SSSS_mod_2) <- c("Oonirici_CONTROL_P3p_SSSS_mod_2")
  
  autocorr_Oonirici_WILD_P3p_SSSS_mod <- t(as.data.frame(autocorr.diag(Oonirici_WILD_P3p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_WILD_P3p_SSSS_mod) <- c("Oonirici_WILD_P3p_SSSS_mod")
  
  autocorr_Oonirici_Vg_P3p_SSSS_mod_2 <- t(as.data.frame(autocorr.diag(Oonirici_Vg_P3p_SSSS_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_Vg_P3p_SSSS_mod_2) <- c("Oonirici_Vg_P3p_SSSS_mod_2")
  
  autocorr_Oonirici_P3p_SSSS_mod <- rbind.data.frame(autocorr_Oonirici_CONTROL_P3p_SSSS_mod_2,autocorr_Oonirici_WILD_P3p_SSSS_mod,autocorr_Oonirici_Vg_P3p_SSSS_mod_2)
  
  #Oonirici_P4p_autocorr
  autocorr_Oonirici_CONTROL_P4p_SSSS_mod_2 <- t(as.data.frame(autocorr.diag(Oonirici_CONTROL_P4p_SSSS_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_CONTROL_P4p_SSSS_mod_2) <- c("Oonirici_CONTROL_P4p_SSSS_mod_2")
  
  autocorr_Oonirici_WILD_P4p_SSSS_mod <- t(as.data.frame(autocorr.diag(Oonirici_WILD_P4p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_WILD_P4p_SSSS_mod) <- c("Oonirici_WILD_P4p_SSSS_mod")
  
  autocorr_Oonirici_Vg_P4p_SSSS_mod_2 <- t(as.data.frame(autocorr.diag(Oonirici_Vg_P4p_SSSS_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_Vg_P4p_SSSS_mod_2) <- c("Oonirici_Vg_P4p_SSSS_mod_2")
  
  autocorr_Oonirici_P4p_SSSS_mod <- rbind.data.frame(autocorr_Oonirici_CONTROL_P4p_SSSS_mod_2,autocorr_Oonirici_WILD_P4p_SSSS_mod,autocorr_Oonirici_Vg_P4p_SSSS_mod_2)
  
  #Oonirici_P8p_autocorr
  autocorr_Oonirici_CONTROL_P8p_SSSS_mod_2 <- t(as.data.frame(autocorr.diag(Oonirici_CONTROL_P8p_SSSS_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_CONTROL_P8p_SSSS_mod_2) <- c("Oonirici_CONTROL_P8p_SSSS_mod_2")
  
  autocorr_Oonirici_WILD_P8p_SSSS_mod <- t(as.data.frame(autocorr.diag(Oonirici_WILD_P8p_SSSS_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_WILD_P8p_SSSS_mod) <- c("Oonirici_WILD_P8p_SSSS_mod")
  
  autocorr_Oonirici_Vg_P8p_SSSS_mod_2 <- t(as.data.frame(autocorr.diag(Oonirici_Vg_P8p_SSSS_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_Vg_P8p_SSSS_mod_2) <- c("Oonirici_Vg_P8p_SSSS_mod_2")
  
  
  autocorr_Oonirici_P8p_SSSS_mod <- rbind.data.frame(autocorr_Oonirici_CONTROL_P8p_SSSS_mod_2,autocorr_Oonirici_WILD_P8p_SSSS_mod,autocorr_Oonirici_Vg_P8p_SSSS_mod_2)
  
  #Oonirici_P5p_autocorr
  autocorr_Oonirici_CONTROL_P5p_wt_mod_2 <- t(as.data.frame(autocorr.diag(Oonirici_CONTROL_P5p_wt_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_CONTROL_P5p_wt_mod_2) <- c("Oonirici_CONTROL_P5p_wt_mod_2")
  
  autocorr_Oonirici_WILD_P5p_wt_mod <- t(as.data.frame(autocorr.diag(Oonirici_WILD_P5p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_WILD_P5p_wt_mod) <- c("Oonirici_WILD_P5p_wt_mod")
  
  autocorr_Oonirici_Vg_P5p_wt_mod_2 <- t(as.data.frame(autocorr.diag(Oonirici_Vg_P5p_wt_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_Vg_P5p_wt_mod_2) <- c("Oonirici_Vg_P5p_wt_mod_2")
  
  autocorr_Oonirici_P5p_wt_mod <- rbind.data.frame(autocorr_Oonirici_CONTROL_P5p_wt_mod_2,autocorr_Oonirici_WILD_P5p_wt_mod,autocorr_Oonirici_Vg_P5p_wt_mod_2)
  
  
  #Oonirici_P6p_autocorr
  autocorr_Oonirici_CONTROL_P6p_wt_mod_2 <- t(as.data.frame(autocorr.diag(Oonirici_CONTROL_P6p_wt_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_CONTROL_P6p_wt_mod_2) <- c("Oonirici_CONTROL_P6p_wt_mod_2")
  
  autocorr_Oonirici_WILD_P6p_wt_mod <- t(as.data.frame(autocorr.diag(Oonirici_WILD_P6p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_WILD_P6p_wt_mod) <- c("Oonirici_WILD_P6p_wt_mod")
  
  autocorr_Oonirici_Vg_P6p_wt_mod_2 <- t(as.data.frame(autocorr.diag(Oonirici_Vg_P6p_wt_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_Vg_P6p_wt_mod_2) <- c("Oonirici_Vg_P6p_wt_mod_2")
  
  
  autocorr_Oonirici_P6p_wt_mod <- rbind.data.frame(autocorr_Oonirici_CONTROL_P6p_wt_mod_2,autocorr_Oonirici_WILD_P6p_wt_mod,autocorr_Oonirici_Vg_P6p_wt_mod_2)
  
  #Oonirici_P7p_autocorr
  autocorr_Oonirici_CONTROL_P7p_wt_mod_2 <- t(as.data.frame(autocorr.diag(Oonirici_CONTROL_P7p_wt_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_CONTROL_P7p_wt_mod_2) <- c("Oonirici_CONTROL_P7p_wt_mod_2")
  
  autocorr_Oonirici_WILD_P7p_wt_mod <- t(as.data.frame(autocorr.diag(Oonirici_WILD_P7p_wt_mod[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_WILD_P7p_wt_mod) <- c("Oonirici_WILD_P7p_wt_mod")
  
  autocorr_Oonirici_Vg_P7p_wt_mod_2 <- t(as.data.frame(autocorr.diag(Oonirici_Vg_P7p_wt_mod_2[["VCV"]])[2,c(1:2)]))
  rownames(autocorr_Oonirici_Vg_P7p_wt_mod_2) <- c("Oonirici_Vg_P7p_wt_mod_2")
  
  autocorr_Oonirici_P7p_wt_mod <- rbind.data.frame(autocorr_Oonirici_CONTROL_P7p_wt_mod_2,autocorr_Oonirici_WILD_P7p_wt_mod,autocorr_Oonirici_Vg_P7p_wt_mod_2)
  
  
  #Oonirici_autocorr_SSSS_vulva
  autocorr_Oonirici_SSSS_vulva <- rbind.data.frame(autocorr_Oonirici_P3p_SSSS_mod,autocorr_Oonirici_P4p_SSSS_mod,autocorr_Oonirici_P5p_wt_mod,autocorr_Oonirici_P6p_wt_mod,autocorr_Oonirici_P7p_wt_mod,autocorr_Oonirici_P8p_SSSS_mod)
  autocorr_Oonirici_SSSS_vulva
}




Vg_Oscheius_isolate_SSSS_autocorr <-  rbind.data.frame(autocorr_Otipulae_SSSS_vulva,autocorr_Oonirici_SSSS_vulva)
View(Vg_Oscheius_isolate_SSSS_autocorr)
Vg_Oscheius_isolate_SSSS_autocorr <- cbind(Models = rownames(Vg_Oscheius_isolate_SSSS_autocorr),Vg_Oscheius_isolate_SSSS_autocorr)


write_xlsx(Vg_Oscheius_isolate_SSSS_autocorr, "Vg_Oscheius_isolate_SSSS_autocorr.xlsx")



#Vm_MCMCglmm_Oscheius_isolate_SSSS_vulva

library(readxl)
library(Matrix)
library(rlang)
library(dplyr)
library(ggplot2)
library(coda)       
library(ape)
library(MCMCglmm)
library(QGglmm)
library(purrr)
library(writexl)
library(plotMCMC)
library(postMCMCglmm) 
library(emmeans)

Vm_Vg_data<- as.data.frame(read_xlsx("Data S6 phenotyping for VG VM.xlsx"))
View(Vm_Vg_data)
Vm_Vg_data <- Vm_Vg_data[,-c(49,50)]
Vm_data <- subset(Vm_Vg_data, Variance == "Vm" )
Vm_Oscheius_data <- subset(Vm_data, Genus == "Oscheius" )
Vm_Oscheius_data$LineB <- ifelse(Vm_Oscheius_data$Treatment == "CONTROL" ,paste(Vm_Oscheius_data$Line,Vm_Oscheius_data$Block), paste(Vm_Oscheius_data$Line))
Vm_Oscheius_data$BlockRep <- paste(Vm_Oscheius_data$Block,Vm_Oscheius_data$Replicate)
Vm_Oscheius_data$Treatment <- as.factor(Vm_Oscheius_data$Treatment)
Vm_Oscheius_data$Observer <- as.factor(Vm_Oscheius_data$Observer)

View(Vm_Oscheius_data)
table(Vm_Oscheius_data$Species)
table(Vm_Oscheius_data$Ancestral)

Vm_CEW1_data <- subset(Vm_Oscheius_data, Ancestral =="CEW1")
Vm_JU178_data <- subset(Vm_Oscheius_data, Ancestral =="JU178")
Vm_PS2068_data <- subset(Vm_Oscheius_data, Ancestral =="PS2068")
Vm_JU77_data <- subset(Vm_Oscheius_data, Ancestral =="JU77")


table(Vm_Oscheius_data$P3.p_multinomial)
table(Vm_Oscheius_data$P4.p_multinomial)
table(Vm_Oscheius_data$P8.p_multinomial)





prior_bi_block <-
  list( R = list(V = 1, fix = 1),      # Fixing the "residual" variance to 1 because it is not identifiable in binary responses 
        G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1),
                 G2 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1)))

#CEW1----
{
  Vm_CEW1_bi_CONTROL <- subset(Vm_CEW1_data, Treatment =="CONTROL")
  Vm_CEW1_bi_MA <- subset(Vm_CEW1_data, Treatment =="MA")
  
  
  ## CEW1 P3p ----
  
  
  #CEW1_CONTROL_P3p_SSSS_mod 
  {
    ## Total separation!
    CEW1_CONTROL_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_CEW1_bi_CONTROL,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)            
    
    saveRDS(CEW1_CONTROL_P3p_SSSS_mod, file = "CEW1_CONTROL_P3p_SSSS_mod.rds")
    CEW1_CONTROL_P3p_SSSS_mod <- readRDS("CEW1_CONTROL_P3p_SSSS_mod.rds")
    
    summary(CEW1_CONTROL_P3p_SSSS_mod) 
    plotTrace(CEW1_CONTROL_P3p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("CEW1_CONTROL_P3p_SSSS_mod.pdf")
    plot(CEW1_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(CEW1_CONTROL_P3p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(CEW1_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(CEW1_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(CEW1_CONTROL_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(CEW1_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(CEW1_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_CONTROL_P3p_SSSS_mod <- CEW1_CONTROL_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_CEW1_CONTROL_P3p_SSSS_mod <- CEW1_CONTROL_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_CEW1_CONTROL_P3p_SSSS_mod <- rowSums(CEW1_CONTROL_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_CEW1_CONTROL_P3p_SSSS_mod) 
      HPDinterval(va_liab_CEW1_CONTROL_P3p_SSSS_mod) 
      
      mean(vlat_CEW1_CONTROL_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_CEW1_CONTROL_P3p_SSSS_mod <- CEW1_CONTROL_P3p_SSSS_mod[["X"]]
      beta_CEW1_CONTROL_P3p_SSSS_mod <- CEW1_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_CEW1_CONTROL_P3p_SSSS_mod   <- apply(beta_CEW1_CONTROL_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_CEW1_CONTROL_P3p_SSSS_mod %*% b))}) 
      mean(vf_CEW1_CONTROL_P3p_SSSS_mod) 
      
      
      h2_liab_CEW1_CONTROL_P3p_SSSS_mod <- va_liab_CEW1_CONTROL_P3p_SSSS_mod / (vlat_CEW1_CONTROL_P3p_SSSS_mod + vf_CEW1_CONTROL_P3p_SSSS_mod)
      mean(h2_liab_CEW1_CONTROL_P3p_SSSS_mod) 
      posterior.mode(h2_liab_CEW1_CONTROL_P3p_SSSS_mod)	
      median(h2_liab_CEW1_CONTROL_P3p_SSSS_mod)		
      HPDinterval(h2_liab_CEW1_CONTROL_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_CEW1_CONTROL_P3p_SSSS_mod <- rowMeans(CEW1_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_CEW1_CONTROL_P3p_SSSS_mod <- (va_liab_CEW1_CONTROL_P3p_SSSS_mod/2) / (trait_mean_liab_CEW1_CONTROL_P3p_SSSS_mod)^2
    mean(Evol_liab_CEW1_CONTROL_P3p_SSSS_mod)
    
    
    #CEW1_CONTROL_P3p_SSSS_mod data scale
    {
      
      predict_CEW1_CONTROL_P3p_SSSS_mod <- map(1:nrow(CEW1_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_CEW1_CONTROL_P3p_SSSS_mod %*% CEW1_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_CEW1_CONTROL_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_CEW1_CONTROL_P3p_SSSS_mod,
                      var.a = CEW1_CONTROL_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_CONTROL_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_CONTROL_P3p_SSSS_mod <- data_CEW1_CONTROL_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_CEW1_CONTROL_P3p_SSSS_mod <- data_CEW1_CONTROL_P3p_SSSS_mod[["mean.obs"]]
      va_data_CEW1_CONTROL_P3p_SSSS_mod <- data_CEW1_CONTROL_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_CEW1_CONTROL_P3p_SSSS_mod <- data_CEW1_CONTROL_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_CEW1_CONTROL_P3p_SSSS_mod <- (va_data_CEW1_CONTROL_P3p_SSSS_mod/2) / (trait_mean_data_CEW1_CONTROL_P3p_SSSS_mod)^2
      
      mean(h2_data_CEW1_CONTROL_P3p_SSSS_mod) 
      mean(trait_mean_data_CEW1_CONTROL_P3p_SSSS_mod)
      mean(va_data_CEW1_CONTROL_P3p_SSSS_mod) 
      mean(vp_data_CEW1_CONTROL_P3p_SSSS_mod)
      mean(Evol_data_CEW1_CONTROL_P3p_SSSS_mod)
      
    }
    
  }
  
  #CEW1_MA_P3p_SSSS_mod 
  {
    
    CEW1_MA_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_CEW1_bi_MA,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)         
    
    saveRDS(CEW1_MA_P3p_SSSS_mod, file = "CEW1_MA_P3p_SSSS_mod.rds")
    CEW1_MA_P3p_SSSS_mod <- readRDS("CEW1_MA_P3p_SSSS_mod.rds")
    
    summary(CEW1_MA_P3p_SSSS_mod) 
    #plot(CEW1_MA_P3p_SSSS_mod)
    
    # traces and posterior densities
    pdf("CEW1_MA_P3p_SSSS_mod.pdf")
    plot(CEW1_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(CEW1_MA_P3p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(CEW1_MA_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(CEW1_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(CEW1_MA_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(CEW1_MA_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(CEW1_MA_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_MA_P3p_SSSS_mod <- CEW1_MA_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_CEW1_MA_P3p_SSSS_mod <- CEW1_MA_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_CEW1_MA_P3p_SSSS_mod <- rowSums(CEW1_MA_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_CEW1_MA_P3p_SSSS_mod) 
      HPDinterval(va_liab_CEW1_MA_P3p_SSSS_mod) 
      
      mean(vlat_CEW1_MA_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_CEW1_MA_P3p_SSSS_mod <- CEW1_MA_P3p_SSSS_mod[["X"]]
      beta_CEW1_MA_P3p_SSSS_mod <- CEW1_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_CEW1_MA_P3p_SSSS_mod   <- apply(beta_CEW1_MA_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_CEW1_MA_P3p_SSSS_mod %*% b))}) 
      mean(vf_CEW1_MA_P3p_SSSS_mod) 
      
      h2_liab_CEW1_MA_P3p_SSSS_mod <- va_liab_CEW1_MA_P3p_SSSS_mod / (vlat_CEW1_MA_P3p_SSSS_mod + vf_CEW1_MA_P3p_SSSS_mod)
      mean(h2_liab_CEW1_MA_P3p_SSSS_mod) 
      posterior.mode(h2_liab_CEW1_MA_P3p_SSSS_mod)	
      median(h2_liab_CEW1_MA_P3p_SSSS_mod)		
      HPDinterval(h2_liab_CEW1_MA_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_CEW1_MA_P3p_SSSS_mod <- rowMeans(CEW1_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_CEW1_MA_P3p_SSSS_mod <- (va_liab_CEW1_MA_P3p_SSSS_mod/2) / (trait_mean_liab_CEW1_MA_P3p_SSSS_mod)^2
    mean(Evol_liab_CEW1_MA_P3p_SSSS_mod)
    
    #CEW1_MA_P3p_SSSS_mod data scale
    {
      
      predict_CEW1_MA_P3p_SSSS_mod <- map(1:nrow(CEW1_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_CEW1_MA_P3p_SSSS_mod %*% CEW1_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_CEW1_MA_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_CEW1_MA_P3p_SSSS_mod,
                      var.a = CEW1_MA_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_MA_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_MA_P3p_SSSS_mod <- data_CEW1_MA_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_CEW1_MA_P3p_SSSS_mod <- data_CEW1_MA_P3p_SSSS_mod[["mean.obs"]]
      va_data_CEW1_MA_P3p_SSSS_mod <- data_CEW1_MA_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_CEW1_MA_P3p_SSSS_mod <- data_CEW1_MA_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_CEW1_MA_P3p_SSSS_mod <- (va_data_CEW1_MA_P3p_SSSS_mod/2) / (trait_mean_data_CEW1_MA_P3p_SSSS_mod)^2
      
      mean(h2_data_CEW1_MA_P3p_SSSS_mod)
      mean(trait_mean_data_CEW1_MA_P3p_SSSS_mod)
      mean(va_data_CEW1_MA_P3p_SSSS_mod)
      mean(vp_data_CEW1_MA_P3p_SSSS_mod)
      mean(Evol_data_CEW1_MA_P3p_SSSS_mod)
      
    }
    
  }
  
  #CEW1_Vm_P3p_SSSS_mod 
  {
    
    CEW1_Vm_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer + Treatment -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_CEW1_data,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)            
    
    saveRDS(CEW1_Vm_P3p_SSSS_mod, file = "CEW1_Vm_P3p_SSSS_mod.rds")
    CEW1_Vm_P3p_SSSS_mod <- readRDS("CEW1_Vm_P3p_SSSS_mod.rds")
    
    summary(CEW1_Vm_P3p_SSSS_mod) 
    plotTrace(CEW1_Vm_P3p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("CEW1_Vm_P3p_SSSS_mod.pdf")
    plot(CEW1_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(CEW1_Vm_P3p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(CEW1_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(CEW1_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(CEW1_Vm_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(CEW1_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(CEW1_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_Vm_P3p_SSSS_mod <- CEW1_Vm_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_CEW1_Vm_P3p_SSSS_mod <- CEW1_Vm_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_CEW1_Vm_P3p_SSSS_mod <- rowSums(CEW1_Vm_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_CEW1_Vm_P3p_SSSS_mod) 
      HPDinterval(va_liab_CEW1_Vm_P3p_SSSS_mod) 
      
      mean(vlat_CEW1_Vm_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_CEW1_Vm_P3p_SSSS_mod <- CEW1_Vm_P3p_SSSS_mod[["X"]]
      beta_CEW1_Vm_P3p_SSSS_mod <- CEW1_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_CEW1_Vm_P3p_SSSS_mod   <- apply(beta_CEW1_Vm_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_CEW1_Vm_P3p_SSSS_mod %*% b))}) 
      mean(vf_CEW1_Vm_P3p_SSSS_mod) 
      
      
      h2_liab_CEW1_Vm_P3p_SSSS_mod <- va_liab_CEW1_Vm_P3p_SSSS_mod / (vlat_CEW1_Vm_P3p_SSSS_mod + vf_CEW1_Vm_P3p_SSSS_mod)
      mean(h2_liab_CEW1_Vm_P3p_SSSS_mod) 
      posterior.mode(h2_liab_CEW1_Vm_P3p_SSSS_mod)	
      median(h2_liab_CEW1_Vm_P3p_SSSS_mod)		
      HPDinterval(h2_liab_CEW1_Vm_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_CEW1_Vm_P3p_SSSS_mod <- ((rowMeans(CEW1_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(CEW1_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + CEW1_Vm_P3p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_CEW1_Vm_P3p_SSSS_mod <- (va_liab_CEW1_Vm_P3p_SSSS_mod/2) / (trait_mean_liab_CEW1_Vm_P3p_SSSS_mod)^2
    mean(Evol_liab_CEW1_Vm_P3p_SSSS_mod)
    
    
    #CEW1_Vm_P3p_SSSS_mod data scale
    {
      
      predict_CEW1_Vm_P3p_SSSS_mod <- map(1:nrow(CEW1_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_CEW1_Vm_P3p_SSSS_mod %*% CEW1_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_CEW1_Vm_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_CEW1_Vm_P3p_SSSS_mod,
                      var.a = CEW1_Vm_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_Vm_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_Vm_P3p_SSSS_mod <- data_CEW1_Vm_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_CEW1_Vm_P3p_SSSS_mod <- data_CEW1_Vm_P3p_SSSS_mod[["mean.obs"]]
      va_data_CEW1_Vm_P3p_SSSS_mod <- data_CEW1_Vm_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_CEW1_Vm_P3p_SSSS_mod <- data_CEW1_Vm_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_CEW1_Vm_P3p_SSSS_mod <- (va_data_CEW1_Vm_P3p_SSSS_mod/2) / (trait_mean_data_CEW1_Vm_P3p_SSSS_mod)^2
      
      mean(h2_data_CEW1_Vm_P3p_SSSS_mod)
      mean(trait_mean_data_CEW1_Vm_P3p_SSSS_mod)
      mean(va_data_CEW1_Vm_P3p_SSSS_mod)
      mean(vp_data_CEW1_Vm_P3p_SSSS_mod)
      mean(Evol_data_CEW1_Vm_P3p_SSSS_mod)
      
    }
    
  }
  
  
 
  
  
  ## CEW1 P4p ----
  
  
  #CEW1_CONTROL_P4p_SSSS_mod 
  {
    
    CEW1_CONTROL_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_CEW1_bi_CONTROL,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)            
    
    saveRDS(CEW1_CONTROL_P4p_SSSS_mod, file = "CEW1_CONTROL_P4p_SSSS_mod.rds")
    CEW1_CONTROL_P4p_SSSS_mod <- readRDS("CEW1_CONTROL_P4p_SSSS_mod.rds")
    
    summary(CEW1_CONTROL_P4p_SSSS_mod) 
    plotTrace(CEW1_CONTROL_P4p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("CEW1_CONTROL_P4p_SSSS_mod.pdf")
    plot(CEW1_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(CEW1_CONTROL_P4p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(CEW1_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(CEW1_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(CEW1_CONTROL_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(CEW1_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(CEW1_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_CONTROL_P4p_SSSS_mod <- CEW1_CONTROL_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_CEW1_CONTROL_P4p_SSSS_mod <- CEW1_CONTROL_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_CEW1_CONTROL_P4p_SSSS_mod <- rowSums(CEW1_CONTROL_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_CEW1_CONTROL_P4p_SSSS_mod) 
      HPDinterval(va_liab_CEW1_CONTROL_P4p_SSSS_mod) 
      
      mean(vlat_CEW1_CONTROL_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_CEW1_CONTROL_P4p_SSSS_mod <- CEW1_CONTROL_P4p_SSSS_mod[["X"]]
      beta_CEW1_CONTROL_P4p_SSSS_mod <- CEW1_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_CEW1_CONTROL_P4p_SSSS_mod   <- apply(beta_CEW1_CONTROL_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_CEW1_CONTROL_P4p_SSSS_mod %*% b))}) 
      mean(vf_CEW1_CONTROL_P4p_SSSS_mod) 
      
      
      h2_liab_CEW1_CONTROL_P4p_SSSS_mod <- va_liab_CEW1_CONTROL_P4p_SSSS_mod / (vlat_CEW1_CONTROL_P4p_SSSS_mod + vf_CEW1_CONTROL_P4p_SSSS_mod)
      mean(h2_liab_CEW1_CONTROL_P4p_SSSS_mod) 
      posterior.mode(h2_liab_CEW1_CONTROL_P4p_SSSS_mod)	
      median(h2_liab_CEW1_CONTROL_P4p_SSSS_mod)		
      HPDinterval(h2_liab_CEW1_CONTROL_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_CEW1_CONTROL_P4p_SSSS_mod <- rowMeans(CEW1_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_CEW1_CONTROL_P4p_SSSS_mod <- (va_liab_CEW1_CONTROL_P4p_SSSS_mod/2) / (trait_mean_liab_CEW1_CONTROL_P4p_SSSS_mod)^2
    mean(Evol_liab_CEW1_CONTROL_P4p_SSSS_mod)
    
    
    #CEW1_CONTROL_P4p_SSSS_mod data scale
    {
      
      predict_CEW1_CONTROL_P4p_SSSS_mod <- map(1:nrow(CEW1_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_CEW1_CONTROL_P4p_SSSS_mod %*% CEW1_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_CEW1_CONTROL_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_CEW1_CONTROL_P4p_SSSS_mod,
                      var.a = CEW1_CONTROL_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_CONTROL_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_CONTROL_P4p_SSSS_mod <- data_CEW1_CONTROL_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_CEW1_CONTROL_P4p_SSSS_mod <- data_CEW1_CONTROL_P4p_SSSS_mod[["mean.obs"]]
      va_data_CEW1_CONTROL_P4p_SSSS_mod <- data_CEW1_CONTROL_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_CEW1_CONTROL_P4p_SSSS_mod <- data_CEW1_CONTROL_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_CEW1_CONTROL_P4p_SSSS_mod <- (va_data_CEW1_CONTROL_P4p_SSSS_mod/2) / (trait_mean_data_CEW1_CONTROL_P4p_SSSS_mod)^2
      
      mean(h2_data_CEW1_CONTROL_P4p_SSSS_mod) 
      mean(trait_mean_data_CEW1_CONTROL_P4p_SSSS_mod)
      mean(va_data_CEW1_CONTROL_P4p_SSSS_mod) 
      mean(vp_data_CEW1_CONTROL_P4p_SSSS_mod)
      mean(Evol_data_CEW1_CONTROL_P4p_SSSS_mod)
      
    }
    
  }
  
  #CEW1_MA_P4p_SSSS_mod 
  {
    
    CEW1_MA_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_CEW1_bi_MA,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)         
    
    saveRDS(CEW1_MA_P4p_SSSS_mod, file = "CEW1_MA_P4p_SSSS_mod.rds")
    CEW1_MA_P4p_SSSS_mod <- readRDS("CEW1_MA_P4p_SSSS_mod.rds")
    
    summary(CEW1_MA_P4p_SSSS_mod) 
    #plot(CEW1_MA_P4p_SSSS_mod)
    
    # traces and posterior densities
    pdf("CEW1_MA_P4p_SSSS_mod.pdf")
    plot(CEW1_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(CEW1_MA_P4p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(CEW1_MA_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(CEW1_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(CEW1_MA_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(CEW1_MA_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(CEW1_MA_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_MA_P4p_SSSS_mod <- CEW1_MA_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_CEW1_MA_P4p_SSSS_mod <- CEW1_MA_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_CEW1_MA_P4p_SSSS_mod <- rowSums(CEW1_MA_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_CEW1_MA_P4p_SSSS_mod) 
      HPDinterval(va_liab_CEW1_MA_P4p_SSSS_mod) 
      
      mean(vlat_CEW1_MA_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_CEW1_MA_P4p_SSSS_mod <- CEW1_MA_P4p_SSSS_mod[["X"]]
      beta_CEW1_MA_P4p_SSSS_mod <- CEW1_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_CEW1_MA_P4p_SSSS_mod   <- apply(beta_CEW1_MA_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_CEW1_MA_P4p_SSSS_mod %*% b))}) 
      mean(vf_CEW1_MA_P4p_SSSS_mod) 
      
      h2_liab_CEW1_MA_P4p_SSSS_mod <- va_liab_CEW1_MA_P4p_SSSS_mod / (vlat_CEW1_MA_P4p_SSSS_mod + vf_CEW1_MA_P4p_SSSS_mod)
      mean(h2_liab_CEW1_MA_P4p_SSSS_mod) 
      posterior.mode(h2_liab_CEW1_MA_P4p_SSSS_mod)	
      median(h2_liab_CEW1_MA_P4p_SSSS_mod)		
      HPDinterval(h2_liab_CEW1_MA_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_CEW1_MA_P4p_SSSS_mod <- rowMeans(CEW1_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_CEW1_MA_P4p_SSSS_mod <- (va_liab_CEW1_MA_P4p_SSSS_mod/2) / (trait_mean_liab_CEW1_MA_P4p_SSSS_mod)^2
    mean(Evol_liab_CEW1_MA_P4p_SSSS_mod)
    
    #CEW1_MA_P4p_SSSS_mod data scale
    {
      
      predict_CEW1_MA_P4p_SSSS_mod <- map(1:nrow(CEW1_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_CEW1_MA_P4p_SSSS_mod %*% CEW1_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_CEW1_MA_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_CEW1_MA_P4p_SSSS_mod,
                      var.a = CEW1_MA_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_MA_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_MA_P4p_SSSS_mod <- data_CEW1_MA_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_CEW1_MA_P4p_SSSS_mod <- data_CEW1_MA_P4p_SSSS_mod[["mean.obs"]]
      va_data_CEW1_MA_P4p_SSSS_mod <- data_CEW1_MA_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_CEW1_MA_P4p_SSSS_mod <- data_CEW1_MA_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_CEW1_MA_P4p_SSSS_mod <- (va_data_CEW1_MA_P4p_SSSS_mod/2) / (trait_mean_data_CEW1_MA_P4p_SSSS_mod)^2
      
      mean(h2_data_CEW1_MA_P4p_SSSS_mod)
      mean(trait_mean_data_CEW1_MA_P4p_SSSS_mod)
      mean(va_data_CEW1_MA_P4p_SSSS_mod)
      mean(vp_data_CEW1_MA_P4p_SSSS_mod)
      mean(Evol_data_CEW1_MA_P4p_SSSS_mod)
      
    }
    
  }
  
  #CEW1_Vm_P4p_SSSS_mod 
  {
    
    CEW1_Vm_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer + Treatment -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_CEW1_data,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)            
    
    saveRDS(CEW1_Vm_P4p_SSSS_mod, file = "CEW1_Vm_P4p_SSSS_mod.rds")
    CEW1_Vm_P4p_SSSS_mod <- readRDS("CEW1_Vm_P4p_SSSS_mod.rds")
    
    summary(CEW1_Vm_P4p_SSSS_mod) 
    plotTrace(CEW1_Vm_P4p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("CEW1_Vm_P4p_SSSS_mod.pdf")
    plot(CEW1_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(CEW1_Vm_P4p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(CEW1_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(CEW1_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(CEW1_Vm_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(CEW1_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(CEW1_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_Vm_P4p_SSSS_mod <- CEW1_Vm_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_CEW1_Vm_P4p_SSSS_mod <- CEW1_Vm_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_CEW1_Vm_P4p_SSSS_mod <- rowSums(CEW1_Vm_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_CEW1_Vm_P4p_SSSS_mod) 
      HPDinterval(va_liab_CEW1_Vm_P4p_SSSS_mod) 
      
      mean(vlat_CEW1_Vm_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_CEW1_Vm_P4p_SSSS_mod <- CEW1_Vm_P4p_SSSS_mod[["X"]]
      beta_CEW1_Vm_P4p_SSSS_mod <- CEW1_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_CEW1_Vm_P4p_SSSS_mod   <- apply(beta_CEW1_Vm_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_CEW1_Vm_P4p_SSSS_mod %*% b))}) 
      mean(vf_CEW1_Vm_P4p_SSSS_mod) 
      
      
      h2_liab_CEW1_Vm_P4p_SSSS_mod <- va_liab_CEW1_Vm_P4p_SSSS_mod / (vlat_CEW1_Vm_P4p_SSSS_mod + vf_CEW1_Vm_P4p_SSSS_mod)
      mean(h2_liab_CEW1_Vm_P4p_SSSS_mod) 
      posterior.mode(h2_liab_CEW1_Vm_P4p_SSSS_mod)	
      median(h2_liab_CEW1_Vm_P4p_SSSS_mod)		
      HPDinterval(h2_liab_CEW1_Vm_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_CEW1_Vm_P4p_SSSS_mod <- ((rowMeans(CEW1_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(CEW1_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + CEW1_Vm_P4p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_CEW1_Vm_P4p_SSSS_mod <- (va_liab_CEW1_Vm_P4p_SSSS_mod/2) / (trait_mean_liab_CEW1_Vm_P4p_SSSS_mod)^2
    mean(Evol_liab_CEW1_Vm_P4p_SSSS_mod)
    
    
    #CEW1_Vm_P4p_SSSS_mod data scale
    {
      
      predict_CEW1_Vm_P4p_SSSS_mod <- map(1:nrow(CEW1_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_CEW1_Vm_P4p_SSSS_mod %*% CEW1_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_CEW1_Vm_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_CEW1_Vm_P4p_SSSS_mod,
                      var.a = CEW1_Vm_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_Vm_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_Vm_P4p_SSSS_mod <- data_CEW1_Vm_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_CEW1_Vm_P4p_SSSS_mod <- data_CEW1_Vm_P4p_SSSS_mod[["mean.obs"]]
      va_data_CEW1_Vm_P4p_SSSS_mod <- data_CEW1_Vm_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_CEW1_Vm_P4p_SSSS_mod <- data_CEW1_Vm_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_CEW1_Vm_P4p_SSSS_mod <- (va_data_CEW1_Vm_P4p_SSSS_mod/2) / (trait_mean_data_CEW1_Vm_P4p_SSSS_mod)^2
      
      mean(h2_data_CEW1_Vm_P4p_SSSS_mod)
      mean(trait_mean_data_CEW1_Vm_P4p_SSSS_mod)
      mean(va_data_CEW1_Vm_P4p_SSSS_mod)
      mean(vp_data_CEW1_Vm_P4p_SSSS_mod)
      mean(Evol_data_CEW1_Vm_P4p_SSSS_mod)
      
    }
    
  }
  
  
  
  
  
  ## CEW1 P8p ----
  
  
  #CEW1_CONTROL_P8p_SSSS_mod 
  {
    
    CEW1_CONTROL_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_CEW1_bi_CONTROL,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)            
    
    saveRDS(CEW1_CONTROL_P8p_SSSS_mod, file = "CEW1_CONTROL_P8p_SSSS_mod.rds")
    CEW1_CONTROL_P8p_SSSS_mod <- readRDS("CEW1_CONTROL_P8p_SSSS_mod.rds")
    
    summary(CEW1_CONTROL_P8p_SSSS_mod) 
    plotTrace(CEW1_CONTROL_P8p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("CEW1_CONTROL_P8p_SSSS_mod.pdf")
    plot(CEW1_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(CEW1_CONTROL_P8p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(CEW1_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(CEW1_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(CEW1_CONTROL_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(CEW1_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(CEW1_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_CONTROL_P8p_SSSS_mod <- CEW1_CONTROL_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_CEW1_CONTROL_P8p_SSSS_mod <- CEW1_CONTROL_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_CEW1_CONTROL_P8p_SSSS_mod <- rowSums(CEW1_CONTROL_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_CEW1_CONTROL_P8p_SSSS_mod) 
      HPDinterval(va_liab_CEW1_CONTROL_P8p_SSSS_mod) 
      
      mean(vlat_CEW1_CONTROL_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_CEW1_CONTROL_P8p_SSSS_mod <- CEW1_CONTROL_P8p_SSSS_mod[["X"]]
      beta_CEW1_CONTROL_P8p_SSSS_mod <- CEW1_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_CEW1_CONTROL_P8p_SSSS_mod   <- apply(beta_CEW1_CONTROL_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_CEW1_CONTROL_P8p_SSSS_mod %*% b))}) 
      mean(vf_CEW1_CONTROL_P8p_SSSS_mod) 
      
      
      h2_liab_CEW1_CONTROL_P8p_SSSS_mod <- va_liab_CEW1_CONTROL_P8p_SSSS_mod / (vlat_CEW1_CONTROL_P8p_SSSS_mod + vf_CEW1_CONTROL_P8p_SSSS_mod)
      mean(h2_liab_CEW1_CONTROL_P8p_SSSS_mod) 
      posterior.mode(h2_liab_CEW1_CONTROL_P8p_SSSS_mod)	
      median(h2_liab_CEW1_CONTROL_P8p_SSSS_mod)		
      HPDinterval(h2_liab_CEW1_CONTROL_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_CEW1_CONTROL_P8p_SSSS_mod <- rowMeans(CEW1_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_CEW1_CONTROL_P8p_SSSS_mod <- (va_liab_CEW1_CONTROL_P8p_SSSS_mod/2) / (trait_mean_liab_CEW1_CONTROL_P8p_SSSS_mod)^2
    mean(Evol_liab_CEW1_CONTROL_P8p_SSSS_mod)
    
    
    #CEW1_CONTROL_P8p_SSSS_mod data scale
    {
      
      predict_CEW1_CONTROL_P8p_SSSS_mod <- map(1:nrow(CEW1_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_CEW1_CONTROL_P8p_SSSS_mod %*% CEW1_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_CEW1_CONTROL_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_CEW1_CONTROL_P8p_SSSS_mod,
                      var.a = CEW1_CONTROL_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_CONTROL_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_CONTROL_P8p_SSSS_mod <- data_CEW1_CONTROL_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_CEW1_CONTROL_P8p_SSSS_mod <- data_CEW1_CONTROL_P8p_SSSS_mod[["mean.obs"]]
      va_data_CEW1_CONTROL_P8p_SSSS_mod <- data_CEW1_CONTROL_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_CEW1_CONTROL_P8p_SSSS_mod <- data_CEW1_CONTROL_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_CEW1_CONTROL_P8p_SSSS_mod <- (va_data_CEW1_CONTROL_P8p_SSSS_mod/2) / (trait_mean_data_CEW1_CONTROL_P8p_SSSS_mod)^2
      
      mean(h2_data_CEW1_CONTROL_P8p_SSSS_mod) 
      mean(trait_mean_data_CEW1_CONTROL_P8p_SSSS_mod)
      mean(va_data_CEW1_CONTROL_P8p_SSSS_mod) 
      mean(vp_data_CEW1_CONTROL_P8p_SSSS_mod)
      mean(Evol_data_CEW1_CONTROL_P8p_SSSS_mod)
      
    }
    
  }
  
  #CEW1_MA_P8p_SSSS_mod 
  {
    
    CEW1_MA_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_CEW1_bi_MA,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)         
    
    saveRDS(CEW1_MA_P8p_SSSS_mod, file = "CEW1_MA_P8p_SSSS_mod.rds")
    CEW1_MA_P8p_SSSS_mod <- readRDS("CEW1_MA_P8p_SSSS_mod.rds")
    
    summary(CEW1_MA_P8p_SSSS_mod) 
    #plot(CEW1_MA_P8p_SSSS_mod)
    
    # traces and posterior densities
    pdf("CEW1_MA_P8p_SSSS_mod.pdf")
    plot(CEW1_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(CEW1_MA_P8p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(CEW1_MA_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(CEW1_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(CEW1_MA_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(CEW1_MA_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(CEW1_MA_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_MA_P8p_SSSS_mod <- CEW1_MA_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_CEW1_MA_P8p_SSSS_mod <- CEW1_MA_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_CEW1_MA_P8p_SSSS_mod <- rowSums(CEW1_MA_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_CEW1_MA_P8p_SSSS_mod) 
      HPDinterval(va_liab_CEW1_MA_P8p_SSSS_mod) 
      
      mean(vlat_CEW1_MA_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_CEW1_MA_P8p_SSSS_mod <- CEW1_MA_P8p_SSSS_mod[["X"]]
      beta_CEW1_MA_P8p_SSSS_mod <- CEW1_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_CEW1_MA_P8p_SSSS_mod   <- apply(beta_CEW1_MA_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_CEW1_MA_P8p_SSSS_mod %*% b))}) 
      mean(vf_CEW1_MA_P8p_SSSS_mod) 
      
      h2_liab_CEW1_MA_P8p_SSSS_mod <- va_liab_CEW1_MA_P8p_SSSS_mod / (vlat_CEW1_MA_P8p_SSSS_mod + vf_CEW1_MA_P8p_SSSS_mod)
      mean(h2_liab_CEW1_MA_P8p_SSSS_mod) 
      posterior.mode(h2_liab_CEW1_MA_P8p_SSSS_mod)	
      median(h2_liab_CEW1_MA_P8p_SSSS_mod)		
      HPDinterval(h2_liab_CEW1_MA_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_CEW1_MA_P8p_SSSS_mod <- rowMeans(CEW1_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_CEW1_MA_P8p_SSSS_mod <- (va_liab_CEW1_MA_P8p_SSSS_mod/2) / (trait_mean_liab_CEW1_MA_P8p_SSSS_mod)^2
    mean(Evol_liab_CEW1_MA_P8p_SSSS_mod)
    
    #CEW1_MA_P8p_SSSS_mod data scale
    {
      
      predict_CEW1_MA_P8p_SSSS_mod <- map(1:nrow(CEW1_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_CEW1_MA_P8p_SSSS_mod %*% CEW1_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_CEW1_MA_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_CEW1_MA_P8p_SSSS_mod,
                      var.a = CEW1_MA_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_MA_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_MA_P8p_SSSS_mod <- data_CEW1_MA_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_CEW1_MA_P8p_SSSS_mod <- data_CEW1_MA_P8p_SSSS_mod[["mean.obs"]]
      va_data_CEW1_MA_P8p_SSSS_mod <- data_CEW1_MA_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_CEW1_MA_P8p_SSSS_mod <- data_CEW1_MA_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_CEW1_MA_P8p_SSSS_mod <- (va_data_CEW1_MA_P8p_SSSS_mod/2) / (trait_mean_data_CEW1_MA_P8p_SSSS_mod)^2
      
      mean(h2_data_CEW1_MA_P8p_SSSS_mod)
      mean(trait_mean_data_CEW1_MA_P8p_SSSS_mod)
      mean(va_data_CEW1_MA_P8p_SSSS_mod)
      mean(vp_data_CEW1_MA_P8p_SSSS_mod)
      mean(Evol_data_CEW1_MA_P8p_SSSS_mod)
      
    }
    
  }
  
  #CEW1_Vm_P8p_SSSS_mod 
  {
    
    CEW1_Vm_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer + Treatment -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_CEW1_data,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)            
    
    saveRDS(CEW1_Vm_P8p_SSSS_mod, file = "CEW1_Vm_P8p_SSSS_mod.rds")
    CEW1_Vm_P8p_SSSS_mod <- readRDS("CEW1_Vm_P8p_SSSS_mod.rds")
    
    summary(CEW1_Vm_P8p_SSSS_mod) 
    plotTrace(CEW1_Vm_P8p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("CEW1_Vm_P8p_SSSS_mod.pdf")
    plot(CEW1_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(CEW1_Vm_P8p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(CEW1_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(CEW1_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(CEW1_Vm_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(CEW1_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(CEW1_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_Vm_P8p_SSSS_mod <- CEW1_Vm_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_CEW1_Vm_P8p_SSSS_mod <- CEW1_Vm_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_CEW1_Vm_P8p_SSSS_mod <- rowSums(CEW1_Vm_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_CEW1_Vm_P8p_SSSS_mod) 
      HPDinterval(va_liab_CEW1_Vm_P8p_SSSS_mod) 
      
      mean(vlat_CEW1_Vm_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_CEW1_Vm_P8p_SSSS_mod <- CEW1_Vm_P8p_SSSS_mod[["X"]]
      beta_CEW1_Vm_P8p_SSSS_mod <- CEW1_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_CEW1_Vm_P8p_SSSS_mod   <- apply(beta_CEW1_Vm_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_CEW1_Vm_P8p_SSSS_mod %*% b))}) 
      mean(vf_CEW1_Vm_P8p_SSSS_mod) 
      
      
      h2_liab_CEW1_Vm_P8p_SSSS_mod <- va_liab_CEW1_Vm_P8p_SSSS_mod / (vlat_CEW1_Vm_P8p_SSSS_mod + vf_CEW1_Vm_P8p_SSSS_mod)
      mean(h2_liab_CEW1_Vm_P8p_SSSS_mod) 
      posterior.mode(h2_liab_CEW1_Vm_P8p_SSSS_mod)	
      median(h2_liab_CEW1_Vm_P8p_SSSS_mod)		
      HPDinterval(h2_liab_CEW1_Vm_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_CEW1_Vm_P8p_SSSS_mod <- ((rowMeans(CEW1_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(CEW1_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + CEW1_Vm_P8p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_CEW1_Vm_P8p_SSSS_mod <- (va_liab_CEW1_Vm_P8p_SSSS_mod/2) / (trait_mean_liab_CEW1_Vm_P8p_SSSS_mod)^2
    mean(Evol_liab_CEW1_Vm_P8p_SSSS_mod)
    
    
    #CEW1_Vm_P8p_SSSS_mod data scale
    {
      
      predict_CEW1_Vm_P8p_SSSS_mod <- map(1:nrow(CEW1_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_CEW1_Vm_P8p_SSSS_mod %*% CEW1_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_CEW1_Vm_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_CEW1_Vm_P8p_SSSS_mod,
                      var.a = CEW1_Vm_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_Vm_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_Vm_P8p_SSSS_mod <- data_CEW1_Vm_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_CEW1_Vm_P8p_SSSS_mod <- data_CEW1_Vm_P8p_SSSS_mod[["mean.obs"]]
      va_data_CEW1_Vm_P8p_SSSS_mod <- data_CEW1_Vm_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_CEW1_Vm_P8p_SSSS_mod <- data_CEW1_Vm_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_CEW1_Vm_P8p_SSSS_mod <- (va_data_CEW1_Vm_P8p_SSSS_mod/2) / (trait_mean_data_CEW1_Vm_P8p_SSSS_mod)^2
      
      mean(h2_data_CEW1_Vm_P8p_SSSS_mod)
      mean(trait_mean_data_CEW1_Vm_P8p_SSSS_mod)
      mean(va_data_CEW1_Vm_P8p_SSSS_mod)
      mean(vp_data_CEW1_Vm_P8p_SSSS_mod)
      mean(Evol_data_CEW1_Vm_P8p_SSSS_mod)
      
    }
    
  }
  
  
 ## CEW1 P5p ----
  
  
  #CEW1_CONTROL_P5p_wt_mod 
  {
    
    CEW1_CONTROL_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_CEW1_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,data scale
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(CEW1_CONTROL_P5p_wt_mod, file = "CEW1_CONTROL_P5p_wt_mod.rds")
    CEW1_CONTROL_P5p_wt_mod <- readRDS("CEW1_CONTROL_P5p_wt_mod.rds")
    
    summary(CEW1_CONTROL_P5p_wt_mod) 
    plotTrace(CEW1_CONTROL_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("CEW1_CONTROL_P5p_wt_mod.pdf")
    plot(CEW1_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(CEW1_CONTROL_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(CEW1_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(CEW1_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(CEW1_CONTROL_P5p_wt_mod[["VCV"]])
    #autocorr.plot(CEW1_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(CEW1_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_CONTROL_P5p_wt_mod <- CEW1_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_CEW1_CONTROL_P5p_wt_mod <- CEW1_CONTROL_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_CEW1_CONTROL_P5p_wt_mod <- rowSums(CEW1_CONTROL_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_CEW1_CONTROL_P5p_wt_mod) 
      HPDinterval(va_liab_CEW1_CONTROL_P5p_wt_mod) 
      
      mean(vlat_CEW1_CONTROL_P5p_wt_mod) 
      
      #variance of fixed effects
      X_CEW1_CONTROL_P5p_wt_mod <- CEW1_CONTROL_P5p_wt_mod[["X"]]
      beta_CEW1_CONTROL_P5p_wt_mod <- CEW1_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_CEW1_CONTROL_P5p_wt_mod   <- apply(beta_CEW1_CONTROL_P5p_wt_mod, 1, function(b) {var(as.vector(X_CEW1_CONTROL_P5p_wt_mod %*% b))}) 
      mean(vf_CEW1_CONTROL_P5p_wt_mod) 
      
      
      h2_liab_CEW1_CONTROL_P5p_wt_mod <- va_liab_CEW1_CONTROL_P5p_wt_mod / (vlat_CEW1_CONTROL_P5p_wt_mod + vf_CEW1_CONTROL_P5p_wt_mod)
      mean(h2_liab_CEW1_CONTROL_P5p_wt_mod) 
      posterior.mode(h2_liab_CEW1_CONTROL_P5p_wt_mod)	
      median(h2_liab_CEW1_CONTROL_P5p_wt_mod)		
      HPDinterval(h2_liab_CEW1_CONTROL_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_CEW1_CONTROL_P5p_wt_mod <- rowMeans(CEW1_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_CEW1_CONTROL_P5p_wt_mod <- (va_liab_CEW1_CONTROL_P5p_wt_mod/2) / (trait_mean_liab_CEW1_CONTROL_P5p_wt_mod)^2
    mean(Evol_liab_CEW1_CONTROL_P5p_wt_mod)
    
    
    #CEW1_CONTROL_P5p_wt_mod data scale
    {
      
      predict_CEW1_CONTROL_P5p_wt_mod <- map(1:nrow(CEW1_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_CEW1_CONTROL_P5p_wt_mod %*% CEW1_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_CEW1_CONTROL_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_CEW1_CONTROL_P5p_wt_mod,
                      var.a = CEW1_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_CONTROL_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_CONTROL_P5p_wt_mod <- data_CEW1_CONTROL_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_CEW1_CONTROL_P5p_wt_mod <- data_CEW1_CONTROL_P5p_wt_mod[["mean.obs"]]
      va_data_CEW1_CONTROL_P5p_wt_mod <- data_CEW1_CONTROL_P5p_wt_mod[["var.a.obs"]]
      vp_data_CEW1_CONTROL_P5p_wt_mod <- data_CEW1_CONTROL_P5p_wt_mod[["var.obs"]]
      
      Evol_data_CEW1_CONTROL_P5p_wt_mod <- (va_data_CEW1_CONTROL_P5p_wt_mod/2) / (trait_mean_data_CEW1_CONTROL_P5p_wt_mod)^2
      
      mean(h2_data_CEW1_CONTROL_P5p_wt_mod) 
      mean(trait_mean_data_CEW1_CONTROL_P5p_wt_mod)
      mean(va_data_CEW1_CONTROL_P5p_wt_mod) 
      mean(vp_data_CEW1_CONTROL_P5p_wt_mod)
      mean(Evol_data_CEW1_CONTROL_P5p_wt_mod)
      
    }
    
  }
  
  #CEW1_MA_P5p_wt_mod 
  {
    
    CEW1_MA_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_CEW1_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(CEW1_MA_P5p_wt_mod, file = "CEW1_MA_P5p_wt_mod.rds")
    CEW1_MA_P5p_wt_mod <- readRDS("CEW1_MA_P5p_wt_mod.rds")
    
    summary(CEW1_MA_P5p_wt_mod) 
    #plot(CEW1_MA_P5p_wt_mod)
    
    # traces and posterior densities
    pdf("CEW1_MA_P5p_wt_mod.pdf")
    plot(CEW1_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(CEW1_MA_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(CEW1_MA_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_MA_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(CEW1_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(CEW1_MA_P5p_wt_mod[["VCV"]])
    #autocorr.plot(CEW1_MA_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(CEW1_MA_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_MA_P5p_wt_mod <- CEW1_MA_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_CEW1_MA_P5p_wt_mod <- CEW1_MA_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_CEW1_MA_P5p_wt_mod <- rowSums(CEW1_MA_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_CEW1_MA_P5p_wt_mod) 
      HPDinterval(va_liab_CEW1_MA_P5p_wt_mod) 
      
      mean(vlat_CEW1_MA_P5p_wt_mod) 
      
      #variance of fixed effects
      X_CEW1_MA_P5p_wt_mod <- CEW1_MA_P5p_wt_mod[["X"]]
      beta_CEW1_MA_P5p_wt_mod <- CEW1_MA_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_CEW1_MA_P5p_wt_mod   <- apply(beta_CEW1_MA_P5p_wt_mod, 1, function(b) {var(as.vector(X_CEW1_MA_P5p_wt_mod %*% b))}) 
      mean(vf_CEW1_MA_P5p_wt_mod) 
      
      h2_liab_CEW1_MA_P5p_wt_mod <- va_liab_CEW1_MA_P5p_wt_mod / (vlat_CEW1_MA_P5p_wt_mod + vf_CEW1_MA_P5p_wt_mod)
      mean(h2_liab_CEW1_MA_P5p_wt_mod) 
      posterior.mode(h2_liab_CEW1_MA_P5p_wt_mod)	
      median(h2_liab_CEW1_MA_P5p_wt_mod)		
      HPDinterval(h2_liab_CEW1_MA_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_CEW1_MA_P5p_wt_mod <- rowMeans(CEW1_MA_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_CEW1_MA_P5p_wt_mod <- (va_liab_CEW1_MA_P5p_wt_mod/2) / (trait_mean_liab_CEW1_MA_P5p_wt_mod)^2
    mean(Evol_liab_CEW1_MA_P5p_wt_mod)
    
    #CEW1_MA_P5p_wt_mod data scale
    {
      
      predict_CEW1_MA_P5p_wt_mod <- map(1:nrow(CEW1_MA_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_CEW1_MA_P5p_wt_mod %*% CEW1_MA_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_CEW1_MA_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_CEW1_MA_P5p_wt_mod,
                      var.a = CEW1_MA_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_MA_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_MA_P5p_wt_mod <- data_CEW1_MA_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_CEW1_MA_P5p_wt_mod <- data_CEW1_MA_P5p_wt_mod[["mean.obs"]]
      va_data_CEW1_MA_P5p_wt_mod <- data_CEW1_MA_P5p_wt_mod[["var.a.obs"]]
      vp_data_CEW1_MA_P5p_wt_mod <- data_CEW1_MA_P5p_wt_mod[["var.obs"]]
      
      Evol_data_CEW1_MA_P5p_wt_mod <- (va_data_CEW1_MA_P5p_wt_mod/2) / (trait_mean_data_CEW1_MA_P5p_wt_mod)^2
      
      mean(h2_data_CEW1_MA_P5p_wt_mod)
      mean(trait_mean_data_CEW1_MA_P5p_wt_mod)
      mean(va_data_CEW1_MA_P5p_wt_mod)
      mean(vp_data_CEW1_MA_P5p_wt_mod)
      mean(Evol_data_CEW1_MA_P5p_wt_mod)
      
    }
    
  }
  
  #CEW1_Vm_P5p_wt_mod 
  {
    
    CEW1_Vm_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_CEW1_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(CEW1_Vm_P5p_wt_mod, file = "CEW1_Vm_P5p_wt_mod.rds")
    CEW1_Vm_P5p_wt_mod <- readRDS("CEW1_Vm_P5p_wt_mod.rds")
    
    summary(CEW1_Vm_P5p_wt_mod) 
    plotTrace(CEW1_Vm_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("CEW1_Vm_P5p_wt_mod.pdf")
    plot(CEW1_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(CEW1_Vm_P5p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(CEW1_Vm_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_Vm_P5p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(CEW1_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(CEW1_Vm_P5p_wt_mod[["VCV"]])
    #autocorr.plot(CEW1_Vm_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(CEW1_Vm_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_Vm_P5p_wt_mod <- CEW1_Vm_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_CEW1_Vm_P5p_wt_mod <- CEW1_Vm_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_CEW1_Vm_P5p_wt_mod <- rowSums(CEW1_Vm_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_CEW1_Vm_P5p_wt_mod) 
      HPDinterval(va_liab_CEW1_Vm_P5p_wt_mod) 
      
      mean(vlat_CEW1_Vm_P5p_wt_mod) 
      
      #variance of fixed effects
      X_CEW1_Vm_P5p_wt_mod <- CEW1_Vm_P5p_wt_mod[["X"]]
      beta_CEW1_Vm_P5p_wt_mod <- CEW1_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]
      vf_CEW1_Vm_P5p_wt_mod   <- apply(beta_CEW1_Vm_P5p_wt_mod, 1, function(b) {var(as.vector(X_CEW1_Vm_P5p_wt_mod %*% b))}) 
      mean(vf_CEW1_Vm_P5p_wt_mod) 
      
      
      h2_liab_CEW1_Vm_P5p_wt_mod <- va_liab_CEW1_Vm_P5p_wt_mod / (vlat_CEW1_Vm_P5p_wt_mod + vf_CEW1_Vm_P5p_wt_mod)
      mean(h2_liab_CEW1_Vm_P5p_wt_mod) 
      posterior.mode(h2_liab_CEW1_Vm_P5p_wt_mod)	
      median(h2_liab_CEW1_Vm_P5p_wt_mod)		
      HPDinterval(h2_liab_CEW1_Vm_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_CEW1_Vm_P5p_wt_mod <- ((rowMeans(CEW1_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(CEW1_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + CEW1_Vm_P5p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_CEW1_Vm_P5p_wt_mod <- (va_liab_CEW1_Vm_P5p_wt_mod/2) / (trait_mean_liab_CEW1_Vm_P5p_wt_mod)^2
    mean(Evol_liab_CEW1_Vm_P5p_wt_mod)
    
    
    #CEW1_Vm_P5p_wt_mod data scale
    {
      
      predict_CEW1_Vm_P5p_wt_mod <- map(1:nrow(CEW1_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_CEW1_Vm_P5p_wt_mod %*% CEW1_Vm_P5p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_CEW1_Vm_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_CEW1_Vm_P5p_wt_mod,
                      var.a = CEW1_Vm_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_Vm_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_Vm_P5p_wt_mod <- data_CEW1_Vm_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_CEW1_Vm_P5p_wt_mod <- data_CEW1_Vm_P5p_wt_mod[["mean.obs"]]
      va_data_CEW1_Vm_P5p_wt_mod <- data_CEW1_Vm_P5p_wt_mod[["var.a.obs"]]
      vp_data_CEW1_Vm_P5p_wt_mod <- data_CEW1_Vm_P5p_wt_mod[["var.obs"]]
      
      Evol_data_CEW1_Vm_P5p_wt_mod <- (va_data_CEW1_Vm_P5p_wt_mod/2) / (trait_mean_data_CEW1_Vm_P5p_wt_mod)^2
      
      mean(h2_data_CEW1_Vm_P5p_wt_mod)
      mean(trait_mean_data_CEW1_Vm_P5p_wt_mod)
      mean(va_data_CEW1_Vm_P5p_wt_mod)
      mean(vp_data_CEW1_Vm_P5p_wt_mod)
      mean(Evol_data_CEW1_Vm_P5p_wt_mod)
      
    }
    
  }
  
  
  
  
  
  ## CEW1 P6p ----
  
  
  #CEW1_CONTROL_P6p_wt_mod 
  {
    
    CEW1_CONTROL_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_CEW1_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(CEW1_CONTROL_P6p_wt_mod, file = "CEW1_CONTROL_P6p_wt_mod.rds")
    CEW1_CONTROL_P6p_wt_mod <- readRDS("CEW1_CONTROL_P6p_wt_mod.rds")
    
    summary(CEW1_CONTROL_P6p_wt_mod) 
    plotTrace(CEW1_CONTROL_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("CEW1_CONTROL_P6p_wt_mod.pdf")
    plot(CEW1_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(CEW1_CONTROL_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(CEW1_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(CEW1_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(CEW1_CONTROL_P6p_wt_mod[["VCV"]])
    #autocorr.plot(CEW1_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(CEW1_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_CONTROL_P6p_wt_mod <- CEW1_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_CEW1_CONTROL_P6p_wt_mod <- CEW1_CONTROL_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_CEW1_CONTROL_P6p_wt_mod <- rowSums(CEW1_CONTROL_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_CEW1_CONTROL_P6p_wt_mod) 
      HPDinterval(va_liab_CEW1_CONTROL_P6p_wt_mod) 
      
      mean(vlat_CEW1_CONTROL_P6p_wt_mod) 
      
      #variance of fixed effects
      X_CEW1_CONTROL_P6p_wt_mod <- CEW1_CONTROL_P6p_wt_mod[["X"]]
      beta_CEW1_CONTROL_P6p_wt_mod <- CEW1_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_CEW1_CONTROL_P6p_wt_mod   <- apply(beta_CEW1_CONTROL_P6p_wt_mod, 1, function(b) {var(as.vector(X_CEW1_CONTROL_P6p_wt_mod %*% b))}) 
      mean(vf_CEW1_CONTROL_P6p_wt_mod) 
      
      
      h2_liab_CEW1_CONTROL_P6p_wt_mod <- va_liab_CEW1_CONTROL_P6p_wt_mod / (vlat_CEW1_CONTROL_P6p_wt_mod + vf_CEW1_CONTROL_P6p_wt_mod)
      mean(h2_liab_CEW1_CONTROL_P6p_wt_mod) 
      posterior.mode(h2_liab_CEW1_CONTROL_P6p_wt_mod)	
      median(h2_liab_CEW1_CONTROL_P6p_wt_mod)		
      HPDinterval(h2_liab_CEW1_CONTROL_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_CEW1_CONTROL_P6p_wt_mod <- rowMeans(CEW1_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_CEW1_CONTROL_P6p_wt_mod <- (va_liab_CEW1_CONTROL_P6p_wt_mod/2) / (trait_mean_liab_CEW1_CONTROL_P6p_wt_mod)^2
    mean(Evol_liab_CEW1_CONTROL_P6p_wt_mod)
    
    
    #CEW1_CONTROL_P6p_wt_mod data scale
    {
      
      predict_CEW1_CONTROL_P6p_wt_mod <- map(1:nrow(CEW1_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_CEW1_CONTROL_P6p_wt_mod %*% CEW1_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_CEW1_CONTROL_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_CEW1_CONTROL_P6p_wt_mod,
                      var.a = CEW1_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_CONTROL_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_CONTROL_P6p_wt_mod <- data_CEW1_CONTROL_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_CEW1_CONTROL_P6p_wt_mod <- data_CEW1_CONTROL_P6p_wt_mod[["mean.obs"]]
      va_data_CEW1_CONTROL_P6p_wt_mod <- data_CEW1_CONTROL_P6p_wt_mod[["var.a.obs"]]
      vp_data_CEW1_CONTROL_P6p_wt_mod <- data_CEW1_CONTROL_P6p_wt_mod[["var.obs"]]
      
      Evol_data_CEW1_CONTROL_P6p_wt_mod <- (va_data_CEW1_CONTROL_P6p_wt_mod/2) / (trait_mean_data_CEW1_CONTROL_P6p_wt_mod)^2
      
      mean(h2_data_CEW1_CONTROL_P6p_wt_mod) 
      mean(trait_mean_data_CEW1_CONTROL_P6p_wt_mod)
      mean(va_data_CEW1_CONTROL_P6p_wt_mod) 
      mean(vp_data_CEW1_CONTROL_P6p_wt_mod)
      mean(Evol_data_CEW1_CONTROL_P6p_wt_mod)
      
    }
    
  }
  
  #CEW1_MA_P6p_wt_mod 
  {
    
    CEW1_MA_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_CEW1_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(CEW1_MA_P6p_wt_mod, file = "CEW1_MA_P6p_wt_mod.rds")
    CEW1_MA_P6p_wt_mod <- readRDS("CEW1_MA_P6p_wt_mod.rds")
    
    summary(CEW1_MA_P6p_wt_mod) 
    #plot(CEW1_MA_P6p_wt_mod)
    
    # traces and posterior densities
    pdf("CEW1_MA_P6p_wt_mod.pdf")
    plot(CEW1_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(CEW1_MA_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(CEW1_MA_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_MA_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(CEW1_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(CEW1_MA_P6p_wt_mod[["VCV"]])
    #autocorr.plot(CEW1_MA_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(CEW1_MA_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_MA_P6p_wt_mod <- CEW1_MA_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_CEW1_MA_P6p_wt_mod <- CEW1_MA_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_CEW1_MA_P6p_wt_mod <- rowSums(CEW1_MA_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_CEW1_MA_P6p_wt_mod) 
      HPDinterval(va_liab_CEW1_MA_P6p_wt_mod) 
      
      mean(vlat_CEW1_MA_P6p_wt_mod) 
      
      #variance of fixed effects
      X_CEW1_MA_P6p_wt_mod <- CEW1_MA_P6p_wt_mod[["X"]]
      beta_CEW1_MA_P6p_wt_mod <- CEW1_MA_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_CEW1_MA_P6p_wt_mod   <- apply(beta_CEW1_MA_P6p_wt_mod, 1, function(b) {var(as.vector(X_CEW1_MA_P6p_wt_mod %*% b))}) 
      mean(vf_CEW1_MA_P6p_wt_mod) 
      
      h2_liab_CEW1_MA_P6p_wt_mod <- va_liab_CEW1_MA_P6p_wt_mod / (vlat_CEW1_MA_P6p_wt_mod + vf_CEW1_MA_P6p_wt_mod)
      mean(h2_liab_CEW1_MA_P6p_wt_mod) 
      posterior.mode(h2_liab_CEW1_MA_P6p_wt_mod)	
      median(h2_liab_CEW1_MA_P6p_wt_mod)		
      HPDinterval(h2_liab_CEW1_MA_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_CEW1_MA_P6p_wt_mod <- rowMeans(CEW1_MA_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_CEW1_MA_P6p_wt_mod <- (va_liab_CEW1_MA_P6p_wt_mod/2) / (trait_mean_liab_CEW1_MA_P6p_wt_mod)^2
    mean(Evol_liab_CEW1_MA_P6p_wt_mod)
    
    #CEW1_MA_P6p_wt_mod data scale
    {
      
      predict_CEW1_MA_P6p_wt_mod <- map(1:nrow(CEW1_MA_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_CEW1_MA_P6p_wt_mod %*% CEW1_MA_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_CEW1_MA_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_CEW1_MA_P6p_wt_mod,
                      var.a = CEW1_MA_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_MA_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_MA_P6p_wt_mod <- data_CEW1_MA_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_CEW1_MA_P6p_wt_mod <- data_CEW1_MA_P6p_wt_mod[["mean.obs"]]
      va_data_CEW1_MA_P6p_wt_mod <- data_CEW1_MA_P6p_wt_mod[["var.a.obs"]]
      vp_data_CEW1_MA_P6p_wt_mod <- data_CEW1_MA_P6p_wt_mod[["var.obs"]]
      
      Evol_data_CEW1_MA_P6p_wt_mod <- (va_data_CEW1_MA_P6p_wt_mod/2) / (trait_mean_data_CEW1_MA_P6p_wt_mod)^2
      
      mean(h2_data_CEW1_MA_P6p_wt_mod)
      mean(trait_mean_data_CEW1_MA_P6p_wt_mod)
      mean(va_data_CEW1_MA_P6p_wt_mod)
      mean(vp_data_CEW1_MA_P6p_wt_mod)
      mean(Evol_data_CEW1_MA_P6p_wt_mod)
      
    }
    
  }
  
  #CEW1_Vm_P6p_wt_mod 
  {
    
    CEW1_Vm_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_CEW1_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(CEW1_Vm_P6p_wt_mod, file = "CEW1_Vm_P6p_wt_mod.rds")
    CEW1_Vm_P6p_wt_mod <- readRDS("CEW1_Vm_P6p_wt_mod.rds")
    
    summary(CEW1_Vm_P6p_wt_mod) 
    plotTrace(CEW1_Vm_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("CEW1_Vm_P6p_wt_mod.pdf")
    plot(CEW1_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(CEW1_Vm_P6p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(CEW1_Vm_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_Vm_P6p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(CEW1_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(CEW1_Vm_P6p_wt_mod[["VCV"]])
    #autocorr.plot(CEW1_Vm_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(CEW1_Vm_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_Vm_P6p_wt_mod <- CEW1_Vm_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_CEW1_Vm_P6p_wt_mod <- CEW1_Vm_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_CEW1_Vm_P6p_wt_mod <- rowSums(CEW1_Vm_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_CEW1_Vm_P6p_wt_mod) 
      HPDinterval(va_liab_CEW1_Vm_P6p_wt_mod) 
      
      mean(vlat_CEW1_Vm_P6p_wt_mod) 
      
      #variance of fixed effects
      X_CEW1_Vm_P6p_wt_mod <- CEW1_Vm_P6p_wt_mod[["X"]]
      beta_CEW1_Vm_P6p_wt_mod <- CEW1_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]
      vf_CEW1_Vm_P6p_wt_mod   <- apply(beta_CEW1_Vm_P6p_wt_mod, 1, function(b) {var(as.vector(X_CEW1_Vm_P6p_wt_mod %*% b))}) 
      mean(vf_CEW1_Vm_P6p_wt_mod) 
      
      
      h2_liab_CEW1_Vm_P6p_wt_mod <- va_liab_CEW1_Vm_P6p_wt_mod / (vlat_CEW1_Vm_P6p_wt_mod + vf_CEW1_Vm_P6p_wt_mod)
      mean(h2_liab_CEW1_Vm_P6p_wt_mod) 
      posterior.mode(h2_liab_CEW1_Vm_P6p_wt_mod)	
      median(h2_liab_CEW1_Vm_P6p_wt_mod)		
      HPDinterval(h2_liab_CEW1_Vm_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_CEW1_Vm_P6p_wt_mod <- ((rowMeans(CEW1_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(CEW1_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + CEW1_Vm_P6p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_CEW1_Vm_P6p_wt_mod <- (va_liab_CEW1_Vm_P6p_wt_mod/2) / (trait_mean_liab_CEW1_Vm_P6p_wt_mod)^2
    mean(Evol_liab_CEW1_Vm_P6p_wt_mod)
    
    
    #CEW1_Vm_P6p_wt_mod data scale
    {
      
      predict_CEW1_Vm_P6p_wt_mod <- map(1:nrow(CEW1_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_CEW1_Vm_P6p_wt_mod %*% CEW1_Vm_P6p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_CEW1_Vm_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_CEW1_Vm_P6p_wt_mod,
                      var.a = CEW1_Vm_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_Vm_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_Vm_P6p_wt_mod <- data_CEW1_Vm_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_CEW1_Vm_P6p_wt_mod <- data_CEW1_Vm_P6p_wt_mod[["mean.obs"]]
      va_data_CEW1_Vm_P6p_wt_mod <- data_CEW1_Vm_P6p_wt_mod[["var.a.obs"]]
      vp_data_CEW1_Vm_P6p_wt_mod <- data_CEW1_Vm_P6p_wt_mod[["var.obs"]]
      
      Evol_data_CEW1_Vm_P6p_wt_mod <- (va_data_CEW1_Vm_P6p_wt_mod/2) / (trait_mean_data_CEW1_Vm_P6p_wt_mod)^2
      
      mean(h2_data_CEW1_Vm_P6p_wt_mod)
      mean(trait_mean_data_CEW1_Vm_P6p_wt_mod)
      mean(va_data_CEW1_Vm_P6p_wt_mod)
      mean(vp_data_CEW1_Vm_P6p_wt_mod)
      mean(Evol_data_CEW1_Vm_P6p_wt_mod)
      
    }
    
  }
  
  
  
  
  ## CEW1 P7p ----
  
  
  #CEW1_CONTROL_P7p_wt_mod 
  {
    
    CEW1_CONTROL_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_CEW1_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(CEW1_CONTROL_P7p_wt_mod, file = "CEW1_CONTROL_P7p_wt_mod.rds")
    CEW1_CONTROL_P7p_wt_mod <- readRDS("CEW1_CONTROL_P7p_wt_mod.rds")
    
    summary(CEW1_CONTROL_P7p_wt_mod) 
    plotTrace(CEW1_CONTROL_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("CEW1_CONTROL_P7p_wt_mod.pdf")
    plot(CEW1_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(CEW1_CONTROL_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(CEW1_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(CEW1_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(CEW1_CONTROL_P7p_wt_mod[["VCV"]])
    #autocorr.plot(CEW1_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(CEW1_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_CONTROL_P7p_wt_mod <- CEW1_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_CEW1_CONTROL_P7p_wt_mod <- CEW1_CONTROL_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_CEW1_CONTROL_P7p_wt_mod <- rowSums(CEW1_CONTROL_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_CEW1_CONTROL_P7p_wt_mod) 
      HPDinterval(va_liab_CEW1_CONTROL_P7p_wt_mod) 
      
      mean(vlat_CEW1_CONTROL_P7p_wt_mod) 
      
      #variance of fixed effects
      X_CEW1_CONTROL_P7p_wt_mod <- CEW1_CONTROL_P7p_wt_mod[["X"]]
      beta_CEW1_CONTROL_P7p_wt_mod <- CEW1_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_CEW1_CONTROL_P7p_wt_mod   <- apply(beta_CEW1_CONTROL_P7p_wt_mod, 1, function(b) {var(as.vector(X_CEW1_CONTROL_P7p_wt_mod %*% b))}) 
      mean(vf_CEW1_CONTROL_P7p_wt_mod) 
      
      
      h2_liab_CEW1_CONTROL_P7p_wt_mod <- va_liab_CEW1_CONTROL_P7p_wt_mod / (vlat_CEW1_CONTROL_P7p_wt_mod + vf_CEW1_CONTROL_P7p_wt_mod)
      mean(h2_liab_CEW1_CONTROL_P7p_wt_mod) 
      posterior.mode(h2_liab_CEW1_CONTROL_P7p_wt_mod)	
      median(h2_liab_CEW1_CONTROL_P7p_wt_mod)		
      HPDinterval(h2_liab_CEW1_CONTROL_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_CEW1_CONTROL_P7p_wt_mod <- rowMeans(CEW1_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_CEW1_CONTROL_P7p_wt_mod <- (va_liab_CEW1_CONTROL_P7p_wt_mod/2) / (trait_mean_liab_CEW1_CONTROL_P7p_wt_mod)^2
    mean(Evol_liab_CEW1_CONTROL_P7p_wt_mod)
    
    
    #CEW1_CONTROL_P7p_wt_mod data scale
    {
      
      predict_CEW1_CONTROL_P7p_wt_mod <- map(1:nrow(CEW1_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_CEW1_CONTROL_P7p_wt_mod %*% CEW1_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_CEW1_CONTROL_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_CEW1_CONTROL_P7p_wt_mod,
                      var.a = CEW1_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_CONTROL_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_CONTROL_P7p_wt_mod <- data_CEW1_CONTROL_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_CEW1_CONTROL_P7p_wt_mod <- data_CEW1_CONTROL_P7p_wt_mod[["mean.obs"]]
      va_data_CEW1_CONTROL_P7p_wt_mod <- data_CEW1_CONTROL_P7p_wt_mod[["var.a.obs"]]
      vp_data_CEW1_CONTROL_P7p_wt_mod <- data_CEW1_CONTROL_P7p_wt_mod[["var.obs"]]
      
      Evol_data_CEW1_CONTROL_P7p_wt_mod <- (va_data_CEW1_CONTROL_P7p_wt_mod/2) / (trait_mean_data_CEW1_CONTROL_P7p_wt_mod)^2
      
      mean(h2_data_CEW1_CONTROL_P7p_wt_mod) 
      mean(trait_mean_data_CEW1_CONTROL_P7p_wt_mod)
      mean(va_data_CEW1_CONTROL_P7p_wt_mod) 
      mean(vp_data_CEW1_CONTROL_P7p_wt_mod)
      mean(Evol_data_CEW1_CONTROL_P7p_wt_mod)
      
    }
    
  }
  
  #CEW1_MA_P7p_wt_mod 
  {
    
    CEW1_MA_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_CEW1_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(CEW1_MA_P7p_wt_mod, file = "CEW1_MA_P7p_wt_mod.rds")
    CEW1_MA_P7p_wt_mod <- readRDS("CEW1_MA_P7p_wt_mod.rds")
    
    summary(CEW1_MA_P7p_wt_mod) 
    #plot(CEW1_MA_P7p_wt_mod)
    
    # traces and posterior densities
    pdf("CEW1_MA_P7p_wt_mod.pdf")
    plot(CEW1_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(CEW1_MA_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(CEW1_MA_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_MA_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(CEW1_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(CEW1_MA_P7p_wt_mod[["VCV"]])
    #autocorr.plot(CEW1_MA_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(CEW1_MA_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_MA_P7p_wt_mod <- CEW1_MA_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_CEW1_MA_P7p_wt_mod <- CEW1_MA_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_CEW1_MA_P7p_wt_mod <- rowSums(CEW1_MA_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_CEW1_MA_P7p_wt_mod) 
      HPDinterval(va_liab_CEW1_MA_P7p_wt_mod) 
      
      mean(vlat_CEW1_MA_P7p_wt_mod) 
      
      #variance of fixed effects
      X_CEW1_MA_P7p_wt_mod <- CEW1_MA_P7p_wt_mod[["X"]]
      beta_CEW1_MA_P7p_wt_mod <- CEW1_MA_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_CEW1_MA_P7p_wt_mod   <- apply(beta_CEW1_MA_P7p_wt_mod, 1, function(b) {var(as.vector(X_CEW1_MA_P7p_wt_mod %*% b))}) 
      mean(vf_CEW1_MA_P7p_wt_mod) 
      
      h2_liab_CEW1_MA_P7p_wt_mod <- va_liab_CEW1_MA_P7p_wt_mod / (vlat_CEW1_MA_P7p_wt_mod + vf_CEW1_MA_P7p_wt_mod)
      mean(h2_liab_CEW1_MA_P7p_wt_mod) 
      posterior.mode(h2_liab_CEW1_MA_P7p_wt_mod)	
      median(h2_liab_CEW1_MA_P7p_wt_mod)		
      HPDinterval(h2_liab_CEW1_MA_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_CEW1_MA_P7p_wt_mod <- rowMeans(CEW1_MA_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_CEW1_MA_P7p_wt_mod <- (va_liab_CEW1_MA_P7p_wt_mod/2) / (trait_mean_liab_CEW1_MA_P7p_wt_mod)^2
    mean(Evol_liab_CEW1_MA_P7p_wt_mod)
    
    #CEW1_MA_P7p_wt_mod data scale
    {
      
      predict_CEW1_MA_P7p_wt_mod <- map(1:nrow(CEW1_MA_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_CEW1_MA_P7p_wt_mod %*% CEW1_MA_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_CEW1_MA_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_CEW1_MA_P7p_wt_mod,
                      var.a = CEW1_MA_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_MA_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_MA_P7p_wt_mod <- data_CEW1_MA_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_CEW1_MA_P7p_wt_mod <- data_CEW1_MA_P7p_wt_mod[["mean.obs"]]
      va_data_CEW1_MA_P7p_wt_mod <- data_CEW1_MA_P7p_wt_mod[["var.a.obs"]]
      vp_data_CEW1_MA_P7p_wt_mod <- data_CEW1_MA_P7p_wt_mod[["var.obs"]]
      
      Evol_data_CEW1_MA_P7p_wt_mod <- (va_data_CEW1_MA_P7p_wt_mod/2) / (trait_mean_data_CEW1_MA_P7p_wt_mod)^2
      
      mean(h2_data_CEW1_MA_P7p_wt_mod)
      mean(trait_mean_data_CEW1_MA_P7p_wt_mod)
      mean(va_data_CEW1_MA_P7p_wt_mod)
      mean(vp_data_CEW1_MA_P7p_wt_mod)
      mean(Evol_data_CEW1_MA_P7p_wt_mod)
      
    }
    
  }
  
  #CEW1_Vm_P7p_wt_mod 
  {
    
    CEW1_Vm_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_CEW1_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(CEW1_Vm_P7p_wt_mod, file = "CEW1_Vm_P7p_wt_mod.rds")
    CEW1_Vm_P7p_wt_mod <- readRDS("CEW1_Vm_P7p_wt_mod.rds")
    
    summary(CEW1_Vm_P7p_wt_mod) 
    plotTrace(CEW1_Vm_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("CEW1_Vm_P7p_wt_mod.pdf")
    plot(CEW1_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(CEW1_Vm_P7p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(CEW1_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(CEW1_Vm_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(CEW1_Vm_P7p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(CEW1_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(CEW1_Vm_P7p_wt_mod[["VCV"]])
    #autocorr.plot(CEW1_Vm_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(CEW1_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(CEW1_Vm_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_CEW1_Vm_P7p_wt_mod <- CEW1_Vm_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_CEW1_Vm_P7p_wt_mod <- CEW1_Vm_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_CEW1_Vm_P7p_wt_mod <- rowSums(CEW1_Vm_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_CEW1_Vm_P7p_wt_mod) 
      HPDinterval(va_liab_CEW1_Vm_P7p_wt_mod) 
      
      mean(vlat_CEW1_Vm_P7p_wt_mod) 
      
      #variance of fixed effects
      X_CEW1_Vm_P7p_wt_mod <- CEW1_Vm_P7p_wt_mod[["X"]]
      beta_CEW1_Vm_P7p_wt_mod <- CEW1_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]
      vf_CEW1_Vm_P7p_wt_mod   <- apply(beta_CEW1_Vm_P7p_wt_mod, 1, function(b) {var(as.vector(X_CEW1_Vm_P7p_wt_mod %*% b))}) 
      mean(vf_CEW1_Vm_P7p_wt_mod) 
      
      
      h2_liab_CEW1_Vm_P7p_wt_mod <- va_liab_CEW1_Vm_P7p_wt_mod / (vlat_CEW1_Vm_P7p_wt_mod + vf_CEW1_Vm_P7p_wt_mod)
      mean(h2_liab_CEW1_Vm_P7p_wt_mod) 
      posterior.mode(h2_liab_CEW1_Vm_P7p_wt_mod)	
      median(h2_liab_CEW1_Vm_P7p_wt_mod)		
      HPDinterval(h2_liab_CEW1_Vm_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_CEW1_Vm_P7p_wt_mod <- ((rowMeans(CEW1_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(CEW1_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + CEW1_Vm_P7p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_CEW1_Vm_P7p_wt_mod <- (va_liab_CEW1_Vm_P7p_wt_mod/2) / (trait_mean_liab_CEW1_Vm_P7p_wt_mod)^2
    mean(Evol_liab_CEW1_Vm_P7p_wt_mod)
    
    
    #CEW1_Vm_P7p_wt_mod data scale
    {
      
      predict_CEW1_Vm_P7p_wt_mod <- map(1:nrow(CEW1_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_CEW1_Vm_P7p_wt_mod %*% CEW1_Vm_P7p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_CEW1_Vm_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_CEW1_Vm_P7p_wt_mod,
                      var.a = CEW1_Vm_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(CEW1_Vm_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_CEW1_Vm_P7p_wt_mod <- data_CEW1_Vm_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_CEW1_Vm_P7p_wt_mod <- data_CEW1_Vm_P7p_wt_mod[["mean.obs"]]
      va_data_CEW1_Vm_P7p_wt_mod <- data_CEW1_Vm_P7p_wt_mod[["var.a.obs"]]
      vp_data_CEW1_Vm_P7p_wt_mod <- data_CEW1_Vm_P7p_wt_mod[["var.obs"]]
      
      Evol_data_CEW1_Vm_P7p_wt_mod <- (va_data_CEW1_Vm_P7p_wt_mod/2) / (trait_mean_data_CEW1_Vm_P7p_wt_mod)^2
      
      mean(h2_data_CEW1_Vm_P7p_wt_mod)
      mean(trait_mean_data_CEW1_Vm_P7p_wt_mod)
      mean(va_data_CEW1_Vm_P7p_wt_mod)
      mean(vp_data_CEW1_Vm_P7p_wt_mod)
      mean(Evol_data_CEW1_Vm_P7p_wt_mod)
      
    }
    
  }
  
}
  
  
  


#JU178----
{
  Vm_JU178_bi_CONTROL <- subset(Vm_JU178_data, Treatment =="CONTROL")
  Vm_JU178_bi_MA <- subset(Vm_JU178_data, Treatment =="MA")
  
  
  ##---- JU178 P3p ----
  
  
  #JU178_CONTROL_P3p_SSSS_mod 
  {
    
    JU178_CONTROL_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_JU178_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(JU178_CONTROL_P3p_SSSS_mod, file = "JU178_CONTROL_P3p_SSSS_mod.rds")
    JU178_CONTROL_P3p_SSSS_mod <- readRDS("JU178_CONTROL_P3p_SSSS_mod.rds")
    
    summary(JU178_CONTROL_P3p_SSSS_mod) 
    plotTrace(JU178_CONTROL_P3p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU178_CONTROL_P3p_SSSS_mod.pdf")
    plot(JU178_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(JU178_CONTROL_P3p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU178_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU178_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU178_CONTROL_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU178_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU178_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_CONTROL_P3p_SSSS_mod <- JU178_CONTROL_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU178_CONTROL_P3p_SSSS_mod <- JU178_CONTROL_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU178_CONTROL_P3p_SSSS_mod <- rowSums(JU178_CONTROL_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU178_CONTROL_P3p_SSSS_mod) 
      HPDinterval(va_liab_JU178_CONTROL_P3p_SSSS_mod) 
      
      mean(vlat_JU178_CONTROL_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU178_CONTROL_P3p_SSSS_mod <- JU178_CONTROL_P3p_SSSS_mod[["X"]]
      beta_JU178_CONTROL_P3p_SSSS_mod <- JU178_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_JU178_CONTROL_P3p_SSSS_mod   <- apply(beta_JU178_CONTROL_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_JU178_CONTROL_P3p_SSSS_mod %*% b))}) 
      mean(vf_JU178_CONTROL_P3p_SSSS_mod) 
      
      
      h2_liab_JU178_CONTROL_P3p_SSSS_mod <- va_liab_JU178_CONTROL_P3p_SSSS_mod / (vlat_JU178_CONTROL_P3p_SSSS_mod + vf_JU178_CONTROL_P3p_SSSS_mod)
      mean(h2_liab_JU178_CONTROL_P3p_SSSS_mod) 
      posterior.mode(h2_liab_JU178_CONTROL_P3p_SSSS_mod)	
      median(h2_liab_JU178_CONTROL_P3p_SSSS_mod)		
      HPDinterval(h2_liab_JU178_CONTROL_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU178_CONTROL_P3p_SSSS_mod <- rowMeans(JU178_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU178_CONTROL_P3p_SSSS_mod <- (va_liab_JU178_CONTROL_P3p_SSSS_mod/2) / (trait_mean_liab_JU178_CONTROL_P3p_SSSS_mod)^2
    mean(Evol_liab_JU178_CONTROL_P3p_SSSS_mod)
    
    
    #JU178_CONTROL_P3p_SSSS_mod data scale
    {
      
      predict_JU178_CONTROL_P3p_SSSS_mod <- map(1:nrow(JU178_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU178_CONTROL_P3p_SSSS_mod %*% JU178_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU178_CONTROL_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU178_CONTROL_P3p_SSSS_mod,
                      var.a = JU178_CONTROL_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_CONTROL_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_CONTROL_P3p_SSSS_mod <- data_JU178_CONTROL_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU178_CONTROL_P3p_SSSS_mod <- data_JU178_CONTROL_P3p_SSSS_mod[["mean.obs"]]
      va_data_JU178_CONTROL_P3p_SSSS_mod <- data_JU178_CONTROL_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_JU178_CONTROL_P3p_SSSS_mod <- data_JU178_CONTROL_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU178_CONTROL_P3p_SSSS_mod <- (va_data_JU178_CONTROL_P3p_SSSS_mod/2) / (trait_mean_data_JU178_CONTROL_P3p_SSSS_mod)^2
      
      mean(h2_data_JU178_CONTROL_P3p_SSSS_mod) 
      mean(trait_mean_data_JU178_CONTROL_P3p_SSSS_mod)
      mean(va_data_JU178_CONTROL_P3p_SSSS_mod) 
      mean(vp_data_JU178_CONTROL_P3p_SSSS_mod)
      mean(Evol_data_JU178_CONTROL_P3p_SSSS_mod)
      
    }
    
  }
  
  #JU178_MA_P3p_SSSS_mod 
  {
    
    JU178_MA_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_JU178_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(JU178_MA_P3p_SSSS_mod, file = "JU178_MA_P3p_SSSS_mod.rds")
    JU178_MA_P3p_SSSS_mod <- readRDS("JU178_MA_P3p_SSSS_mod.rds")
    
    summary(JU178_MA_P3p_SSSS_mod) 
    #plot(JU178_MA_P3p_SSSS_mod)
    
    # traces and posterior densities
    pdf("JU178_MA_P3p_SSSS_mod.pdf")
    plot(JU178_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(JU178_MA_P3p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU178_MA_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU178_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU178_MA_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU178_MA_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU178_MA_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_MA_P3p_SSSS_mod <- JU178_MA_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU178_MA_P3p_SSSS_mod <- JU178_MA_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU178_MA_P3p_SSSS_mod <- rowSums(JU178_MA_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU178_MA_P3p_SSSS_mod) 
      HPDinterval(va_liab_JU178_MA_P3p_SSSS_mod) 
      
      mean(vlat_JU178_MA_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU178_MA_P3p_SSSS_mod <- JU178_MA_P3p_SSSS_mod[["X"]]
      beta_JU178_MA_P3p_SSSS_mod <- JU178_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_JU178_MA_P3p_SSSS_mod   <- apply(beta_JU178_MA_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_JU178_MA_P3p_SSSS_mod %*% b))}) 
      mean(vf_JU178_MA_P3p_SSSS_mod) 
      
      h2_liab_JU178_MA_P3p_SSSS_mod <- va_liab_JU178_MA_P3p_SSSS_mod / (vlat_JU178_MA_P3p_SSSS_mod + vf_JU178_MA_P3p_SSSS_mod)
      mean(h2_liab_JU178_MA_P3p_SSSS_mod) 
      posterior.mode(h2_liab_JU178_MA_P3p_SSSS_mod)	
      median(h2_liab_JU178_MA_P3p_SSSS_mod)		
      HPDinterval(h2_liab_JU178_MA_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU178_MA_P3p_SSSS_mod <- rowMeans(JU178_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU178_MA_P3p_SSSS_mod <- (va_liab_JU178_MA_P3p_SSSS_mod/2) / (trait_mean_liab_JU178_MA_P3p_SSSS_mod)^2
    mean(Evol_liab_JU178_MA_P3p_SSSS_mod)
    
    #JU178_MA_P3p_SSSS_mod data scale
    {
      
      predict_JU178_MA_P3p_SSSS_mod <- map(1:nrow(JU178_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU178_MA_P3p_SSSS_mod %*% JU178_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU178_MA_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU178_MA_P3p_SSSS_mod,
                      var.a = JU178_MA_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_MA_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_MA_P3p_SSSS_mod <- data_JU178_MA_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU178_MA_P3p_SSSS_mod <- data_JU178_MA_P3p_SSSS_mod[["mean.obs"]]
      va_data_JU178_MA_P3p_SSSS_mod <- data_JU178_MA_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_JU178_MA_P3p_SSSS_mod <- data_JU178_MA_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU178_MA_P3p_SSSS_mod <- (va_data_JU178_MA_P3p_SSSS_mod/2) / (trait_mean_data_JU178_MA_P3p_SSSS_mod)^2
      
      mean(h2_data_JU178_MA_P3p_SSSS_mod)
      mean(trait_mean_data_JU178_MA_P3p_SSSS_mod)
      mean(va_data_JU178_MA_P3p_SSSS_mod)
      mean(vp_data_JU178_MA_P3p_SSSS_mod)
      mean(Evol_data_JU178_MA_P3p_SSSS_mod)
      
    }
    
  }
  
  #JU178_Vm_P3p_SSSS_mod 
  {
    
    JU178_Vm_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_JU178_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(JU178_Vm_P3p_SSSS_mod, file = "JU178_Vm_P3p_SSSS_mod.rds")
    JU178_Vm_P3p_SSSS_mod <- readRDS("JU178_Vm_P3p_SSSS_mod.rds")
    
    summary(JU178_Vm_P3p_SSSS_mod) 
    plotTrace(JU178_Vm_P3p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("JU178_Vm_P3p_SSSS_mod.pdf")
    plot(JU178_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(JU178_Vm_P3p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU178_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU178_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU178_Vm_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU178_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU178_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_Vm_P3p_SSSS_mod <- JU178_Vm_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU178_Vm_P3p_SSSS_mod <- JU178_Vm_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU178_Vm_P3p_SSSS_mod <- rowSums(JU178_Vm_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU178_Vm_P3p_SSSS_mod) 
      HPDinterval(va_liab_JU178_Vm_P3p_SSSS_mod) 
      
      mean(vlat_JU178_Vm_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU178_Vm_P3p_SSSS_mod <- JU178_Vm_P3p_SSSS_mod[["X"]]
      beta_JU178_Vm_P3p_SSSS_mod <- JU178_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_JU178_Vm_P3p_SSSS_mod   <- apply(beta_JU178_Vm_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_JU178_Vm_P3p_SSSS_mod %*% b))}) 
      mean(vf_JU178_Vm_P3p_SSSS_mod) 
      
      
      h2_liab_JU178_Vm_P3p_SSSS_mod <- va_liab_JU178_Vm_P3p_SSSS_mod / (vlat_JU178_Vm_P3p_SSSS_mod + vf_JU178_Vm_P3p_SSSS_mod)
      mean(h2_liab_JU178_Vm_P3p_SSSS_mod) 
      posterior.mode(h2_liab_JU178_Vm_P3p_SSSS_mod)	
      median(h2_liab_JU178_Vm_P3p_SSSS_mod)		
      HPDinterval(h2_liab_JU178_Vm_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU178_Vm_P3p_SSSS_mod <- ((rowMeans(JU178_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU178_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + JU178_Vm_P3p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU178_Vm_P3p_SSSS_mod <- (va_liab_JU178_Vm_P3p_SSSS_mod/2) / (trait_mean_liab_JU178_Vm_P3p_SSSS_mod)^2
    mean(Evol_liab_JU178_Vm_P3p_SSSS_mod)
    
    
    #JU178_Vm_P3p_SSSS_mod data scale
    {
      
      predict_JU178_Vm_P3p_SSSS_mod <- map(1:nrow(JU178_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU178_Vm_P3p_SSSS_mod %*% JU178_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU178_Vm_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU178_Vm_P3p_SSSS_mod,
                      var.a = JU178_Vm_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_Vm_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_Vm_P3p_SSSS_mod <- data_JU178_Vm_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU178_Vm_P3p_SSSS_mod <- data_JU178_Vm_P3p_SSSS_mod[["mean.obs"]]
      va_data_JU178_Vm_P3p_SSSS_mod <- data_JU178_Vm_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_JU178_Vm_P3p_SSSS_mod <- data_JU178_Vm_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU178_Vm_P3p_SSSS_mod <- (va_data_JU178_Vm_P3p_SSSS_mod/2) / (trait_mean_data_JU178_Vm_P3p_SSSS_mod)^2
      
      mean(h2_data_JU178_Vm_P3p_SSSS_mod)
      mean(trait_mean_data_JU178_Vm_P3p_SSSS_mod)
      mean(va_data_JU178_Vm_P3p_SSSS_mod)
      mean(vp_data_JU178_Vm_P3p_SSSS_mod)
      mean(Evol_data_JU178_Vm_P3p_SSSS_mod)
      
    }
    
  }
  
  
  
  
  ##---- JU178 P4p ----
  
  
  #JU178_CONTROL_P4p_SSSS_mod 
  {
    
    JU178_CONTROL_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_JU178_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(JU178_CONTROL_P4p_SSSS_mod, file = "JU178_CONTROL_P4p_SSSS_mod.rds")
    JU178_CONTROL_P4p_SSSS_mod <- readRDS("JU178_CONTROL_P4p_SSSS_mod.rds")
    
    summary(JU178_CONTROL_P4p_SSSS_mod) 
    plotTrace(JU178_CONTROL_P4p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU178_CONTROL_P4p_SSSS_mod.pdf")
    plot(JU178_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(JU178_CONTROL_P4p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU178_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU178_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU178_CONTROL_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU178_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU178_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_CONTROL_P4p_SSSS_mod <- JU178_CONTROL_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU178_CONTROL_P4p_SSSS_mod <- JU178_CONTROL_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU178_CONTROL_P4p_SSSS_mod <- rowSums(JU178_CONTROL_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU178_CONTROL_P4p_SSSS_mod) 
      HPDinterval(va_liab_JU178_CONTROL_P4p_SSSS_mod) 
      
      mean(vlat_JU178_CONTROL_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU178_CONTROL_P4p_SSSS_mod <- JU178_CONTROL_P4p_SSSS_mod[["X"]]
      beta_JU178_CONTROL_P4p_SSSS_mod <- JU178_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_JU178_CONTROL_P4p_SSSS_mod   <- apply(beta_JU178_CONTROL_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_JU178_CONTROL_P4p_SSSS_mod %*% b))}) 
      mean(vf_JU178_CONTROL_P4p_SSSS_mod) 
      
      
      h2_liab_JU178_CONTROL_P4p_SSSS_mod <- va_liab_JU178_CONTROL_P4p_SSSS_mod / (vlat_JU178_CONTROL_P4p_SSSS_mod + vf_JU178_CONTROL_P4p_SSSS_mod)
      mean(h2_liab_JU178_CONTROL_P4p_SSSS_mod) 
      posterior.mode(h2_liab_JU178_CONTROL_P4p_SSSS_mod)	
      median(h2_liab_JU178_CONTROL_P4p_SSSS_mod)		
      HPDinterval(h2_liab_JU178_CONTROL_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU178_CONTROL_P4p_SSSS_mod <- rowMeans(JU178_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU178_CONTROL_P4p_SSSS_mod <- (va_liab_JU178_CONTROL_P4p_SSSS_mod/2) / (trait_mean_liab_JU178_CONTROL_P4p_SSSS_mod)^2
    mean(Evol_liab_JU178_CONTROL_P4p_SSSS_mod)
    
    
    #JU178_CONTROL_P4p_SSSS_mod data scale
    {
      
      predict_JU178_CONTROL_P4p_SSSS_mod <- map(1:nrow(JU178_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU178_CONTROL_P4p_SSSS_mod %*% JU178_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU178_CONTROL_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU178_CONTROL_P4p_SSSS_mod,
                      var.a = JU178_CONTROL_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_CONTROL_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_CONTROL_P4p_SSSS_mod <- data_JU178_CONTROL_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU178_CONTROL_P4p_SSSS_mod <- data_JU178_CONTROL_P4p_SSSS_mod[["mean.obs"]]
      va_data_JU178_CONTROL_P4p_SSSS_mod <- data_JU178_CONTROL_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_JU178_CONTROL_P4p_SSSS_mod <- data_JU178_CONTROL_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU178_CONTROL_P4p_SSSS_mod <- (va_data_JU178_CONTROL_P4p_SSSS_mod/2) / (trait_mean_data_JU178_CONTROL_P4p_SSSS_mod)^2
      
      mean(h2_data_JU178_CONTROL_P4p_SSSS_mod) 
      mean(trait_mean_data_JU178_CONTROL_P4p_SSSS_mod)
      mean(va_data_JU178_CONTROL_P4p_SSSS_mod) 
      mean(vp_data_JU178_CONTROL_P4p_SSSS_mod)
      mean(Evol_data_JU178_CONTROL_P4p_SSSS_mod)
      
    }
    
  }
  
  #JU178_MA_P4p_SSSS_mod 
  {
    
    JU178_MA_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_JU178_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(JU178_MA_P4p_SSSS_mod, file = "JU178_MA_P4p_SSSS_mod.rds")
    JU178_MA_P4p_SSSS_mod <- readRDS("JU178_MA_P4p_SSSS_mod.rds")
    
    summary(JU178_MA_P4p_SSSS_mod) 
    #plot(JU178_MA_P4p_SSSS_mod)
    
    # traces and posterior densities
    pdf("JU178_MA_P4p_SSSS_mod.pdf")
    plot(JU178_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(JU178_MA_P4p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU178_MA_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU178_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU178_MA_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU178_MA_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU178_MA_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_MA_P4p_SSSS_mod <- JU178_MA_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU178_MA_P4p_SSSS_mod <- JU178_MA_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU178_MA_P4p_SSSS_mod <- rowSums(JU178_MA_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU178_MA_P4p_SSSS_mod) 
      HPDinterval(va_liab_JU178_MA_P4p_SSSS_mod) 
      
      mean(vlat_JU178_MA_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU178_MA_P4p_SSSS_mod <- JU178_MA_P4p_SSSS_mod[["X"]]
      beta_JU178_MA_P4p_SSSS_mod <- JU178_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_JU178_MA_P4p_SSSS_mod   <- apply(beta_JU178_MA_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_JU178_MA_P4p_SSSS_mod %*% b))}) 
      mean(vf_JU178_MA_P4p_SSSS_mod) 
      
      h2_liab_JU178_MA_P4p_SSSS_mod <- va_liab_JU178_MA_P4p_SSSS_mod / (vlat_JU178_MA_P4p_SSSS_mod + vf_JU178_MA_P4p_SSSS_mod)
      mean(h2_liab_JU178_MA_P4p_SSSS_mod) 
      posterior.mode(h2_liab_JU178_MA_P4p_SSSS_mod)	
      median(h2_liab_JU178_MA_P4p_SSSS_mod)		
      HPDinterval(h2_liab_JU178_MA_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU178_MA_P4p_SSSS_mod <- rowMeans(JU178_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU178_MA_P4p_SSSS_mod <- (va_liab_JU178_MA_P4p_SSSS_mod/2) / (trait_mean_liab_JU178_MA_P4p_SSSS_mod)^2
    mean(Evol_liab_JU178_MA_P4p_SSSS_mod)
    
    #JU178_MA_P4p_SSSS_mod data scale
    {
      
      predict_JU178_MA_P4p_SSSS_mod <- map(1:nrow(JU178_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU178_MA_P4p_SSSS_mod %*% JU178_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU178_MA_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU178_MA_P4p_SSSS_mod,
                      var.a = JU178_MA_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_MA_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_MA_P4p_SSSS_mod <- data_JU178_MA_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU178_MA_P4p_SSSS_mod <- data_JU178_MA_P4p_SSSS_mod[["mean.obs"]]
      va_data_JU178_MA_P4p_SSSS_mod <- data_JU178_MA_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_JU178_MA_P4p_SSSS_mod <- data_JU178_MA_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU178_MA_P4p_SSSS_mod <- (va_data_JU178_MA_P4p_SSSS_mod/2) / (trait_mean_data_JU178_MA_P4p_SSSS_mod)^2
      
      mean(h2_data_JU178_MA_P4p_SSSS_mod)
      mean(trait_mean_data_JU178_MA_P4p_SSSS_mod)
      mean(va_data_JU178_MA_P4p_SSSS_mod)
      mean(vp_data_JU178_MA_P4p_SSSS_mod)
      mean(Evol_data_JU178_MA_P4p_SSSS_mod)
      
    }
    
  }
  
  #JU178_Vm_P4p_SSSS_mod 
  {
    
    JU178_Vm_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_JU178_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(JU178_Vm_P4p_SSSS_mod, file = "JU178_Vm_P4p_SSSS_mod.rds")
    JU178_Vm_P4p_SSSS_mod <- readRDS("JU178_Vm_P4p_SSSS_mod.rds")
    
    summary(JU178_Vm_P4p_SSSS_mod) 
    plotTrace(JU178_Vm_P4p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("JU178_Vm_P4p_SSSS_mod.pdf")
    plot(JU178_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(JU178_Vm_P4p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU178_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU178_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU178_Vm_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU178_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU178_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_Vm_P4p_SSSS_mod <- JU178_Vm_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU178_Vm_P4p_SSSS_mod <- JU178_Vm_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU178_Vm_P4p_SSSS_mod <- rowSums(JU178_Vm_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU178_Vm_P4p_SSSS_mod) 
      HPDinterval(va_liab_JU178_Vm_P4p_SSSS_mod) 
      
      mean(vlat_JU178_Vm_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU178_Vm_P4p_SSSS_mod <- JU178_Vm_P4p_SSSS_mod[["X"]]
      beta_JU178_Vm_P4p_SSSS_mod <- JU178_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_JU178_Vm_P4p_SSSS_mod   <- apply(beta_JU178_Vm_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_JU178_Vm_P4p_SSSS_mod %*% b))}) 
      mean(vf_JU178_Vm_P4p_SSSS_mod) 
      
      
      h2_liab_JU178_Vm_P4p_SSSS_mod <- va_liab_JU178_Vm_P4p_SSSS_mod / (vlat_JU178_Vm_P4p_SSSS_mod + vf_JU178_Vm_P4p_SSSS_mod)
      mean(h2_liab_JU178_Vm_P4p_SSSS_mod) 
      posterior.mode(h2_liab_JU178_Vm_P4p_SSSS_mod)	
      median(h2_liab_JU178_Vm_P4p_SSSS_mod)		
      HPDinterval(h2_liab_JU178_Vm_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU178_Vm_P4p_SSSS_mod <- ((rowMeans(JU178_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU178_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + JU178_Vm_P4p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU178_Vm_P4p_SSSS_mod <- (va_liab_JU178_Vm_P4p_SSSS_mod/2) / (trait_mean_liab_JU178_Vm_P4p_SSSS_mod)^2
    mean(Evol_liab_JU178_Vm_P4p_SSSS_mod)
    
    
    #JU178_Vm_P4p_SSSS_mod data scale
    {
      
      predict_JU178_Vm_P4p_SSSS_mod <- map(1:nrow(JU178_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU178_Vm_P4p_SSSS_mod %*% JU178_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU178_Vm_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU178_Vm_P4p_SSSS_mod,
                      var.a = JU178_Vm_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_Vm_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_Vm_P4p_SSSS_mod <- data_JU178_Vm_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU178_Vm_P4p_SSSS_mod <- data_JU178_Vm_P4p_SSSS_mod[["mean.obs"]]
      va_data_JU178_Vm_P4p_SSSS_mod <- data_JU178_Vm_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_JU178_Vm_P4p_SSSS_mod <- data_JU178_Vm_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU178_Vm_P4p_SSSS_mod <- (va_data_JU178_Vm_P4p_SSSS_mod/2) / (trait_mean_data_JU178_Vm_P4p_SSSS_mod)^2
      
      mean(h2_data_JU178_Vm_P4p_SSSS_mod)
      mean(trait_mean_data_JU178_Vm_P4p_SSSS_mod)
      mean(va_data_JU178_Vm_P4p_SSSS_mod)
      mean(vp_data_JU178_Vm_P4p_SSSS_mod)
      mean(Evol_data_JU178_Vm_P4p_SSSS_mod)
      
    }
    
  }
  
  
  
  
  
  ##---- JU178 P8p ----
  
  
  #JU178_CONTROL_P8p_SSSS_mod 
  {
    
    JU178_CONTROL_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_JU178_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(JU178_CONTROL_P8p_SSSS_mod, file = "JU178_CONTROL_P8p_SSSS_mod.rds")
    JU178_CONTROL_P8p_SSSS_mod <- readRDS("JU178_CONTROL_P8p_SSSS_mod.rds")
    
    summary(JU178_CONTROL_P8p_SSSS_mod) 
    plotTrace(JU178_CONTROL_P8p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU178_CONTROL_P8p_SSSS_mod.pdf")
    plot(JU178_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(JU178_CONTROL_P8p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU178_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU178_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU178_CONTROL_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU178_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU178_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_CONTROL_P8p_SSSS_mod <- JU178_CONTROL_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU178_CONTROL_P8p_SSSS_mod <- JU178_CONTROL_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU178_CONTROL_P8p_SSSS_mod <- rowSums(JU178_CONTROL_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU178_CONTROL_P8p_SSSS_mod) 
      HPDinterval(va_liab_JU178_CONTROL_P8p_SSSS_mod) 
      
      mean(vlat_JU178_CONTROL_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU178_CONTROL_P8p_SSSS_mod <- JU178_CONTROL_P8p_SSSS_mod[["X"]]
      beta_JU178_CONTROL_P8p_SSSS_mod <- JU178_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_JU178_CONTROL_P8p_SSSS_mod   <- apply(beta_JU178_CONTROL_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_JU178_CONTROL_P8p_SSSS_mod %*% b))}) 
      mean(vf_JU178_CONTROL_P8p_SSSS_mod) 
      
      
      h2_liab_JU178_CONTROL_P8p_SSSS_mod <- va_liab_JU178_CONTROL_P8p_SSSS_mod / (vlat_JU178_CONTROL_P8p_SSSS_mod + vf_JU178_CONTROL_P8p_SSSS_mod)
      mean(h2_liab_JU178_CONTROL_P8p_SSSS_mod) 
      posterior.mode(h2_liab_JU178_CONTROL_P8p_SSSS_mod)	
      median(h2_liab_JU178_CONTROL_P8p_SSSS_mod)		
      HPDinterval(h2_liab_JU178_CONTROL_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU178_CONTROL_P8p_SSSS_mod <- rowMeans(JU178_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU178_CONTROL_P8p_SSSS_mod <- (va_liab_JU178_CONTROL_P8p_SSSS_mod/2) / (trait_mean_liab_JU178_CONTROL_P8p_SSSS_mod)^2
    mean(Evol_liab_JU178_CONTROL_P8p_SSSS_mod)
    
    
    #JU178_CONTROL_P8p_SSSS_mod data scale
    {
      
      predict_JU178_CONTROL_P8p_SSSS_mod <- map(1:nrow(JU178_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU178_CONTROL_P8p_SSSS_mod %*% JU178_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU178_CONTROL_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU178_CONTROL_P8p_SSSS_mod,
                      var.a = JU178_CONTROL_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_CONTROL_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_CONTROL_P8p_SSSS_mod <- data_JU178_CONTROL_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU178_CONTROL_P8p_SSSS_mod <- data_JU178_CONTROL_P8p_SSSS_mod[["mean.obs"]]
      va_data_JU178_CONTROL_P8p_SSSS_mod <- data_JU178_CONTROL_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_JU178_CONTROL_P8p_SSSS_mod <- data_JU178_CONTROL_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU178_CONTROL_P8p_SSSS_mod <- (va_data_JU178_CONTROL_P8p_SSSS_mod/2) / (trait_mean_data_JU178_CONTROL_P8p_SSSS_mod)^2
      
      mean(h2_data_JU178_CONTROL_P8p_SSSS_mod) 
      mean(trait_mean_data_JU178_CONTROL_P8p_SSSS_mod)
      mean(va_data_JU178_CONTROL_P8p_SSSS_mod) 
      mean(vp_data_JU178_CONTROL_P8p_SSSS_mod)
      mean(Evol_data_JU178_CONTROL_P8p_SSSS_mod)
      
    }
    
  }
  
  #JU178_MA_P8p_SSSS_mod 
  {
    
    JU178_MA_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_JU178_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(JU178_MA_P8p_SSSS_mod, file = "JU178_MA_P8p_SSSS_mod.rds")
    JU178_MA_P8p_SSSS_mod <- readRDS("JU178_MA_P8p_SSSS_mod.rds")
    
    summary(JU178_MA_P8p_SSSS_mod) 
    #plot(JU178_MA_P8p_SSSS_mod)
    
    # traces and posterior densities
    pdf("JU178_MA_P8p_SSSS_mod.pdf")
    plot(JU178_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(JU178_MA_P8p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU178_MA_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU178_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU178_MA_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU178_MA_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU178_MA_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_MA_P8p_SSSS_mod <- JU178_MA_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU178_MA_P8p_SSSS_mod <- JU178_MA_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU178_MA_P8p_SSSS_mod <- rowSums(JU178_MA_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU178_MA_P8p_SSSS_mod) 
      HPDinterval(va_liab_JU178_MA_P8p_SSSS_mod) 
      
      mean(vlat_JU178_MA_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU178_MA_P8p_SSSS_mod <- JU178_MA_P8p_SSSS_mod[["X"]]
      beta_JU178_MA_P8p_SSSS_mod <- JU178_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_JU178_MA_P8p_SSSS_mod   <- apply(beta_JU178_MA_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_JU178_MA_P8p_SSSS_mod %*% b))}) 
      mean(vf_JU178_MA_P8p_SSSS_mod) 
      
      h2_liab_JU178_MA_P8p_SSSS_mod <- va_liab_JU178_MA_P8p_SSSS_mod / (vlat_JU178_MA_P8p_SSSS_mod + vf_JU178_MA_P8p_SSSS_mod)
      mean(h2_liab_JU178_MA_P8p_SSSS_mod) 
      posterior.mode(h2_liab_JU178_MA_P8p_SSSS_mod)	
      median(h2_liab_JU178_MA_P8p_SSSS_mod)		
      HPDinterval(h2_liab_JU178_MA_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU178_MA_P8p_SSSS_mod <- rowMeans(JU178_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU178_MA_P8p_SSSS_mod <- (va_liab_JU178_MA_P8p_SSSS_mod/2) / (trait_mean_liab_JU178_MA_P8p_SSSS_mod)^2
    mean(Evol_liab_JU178_MA_P8p_SSSS_mod)
    
    #JU178_MA_P8p_SSSS_mod data scale
    {
      
      predict_JU178_MA_P8p_SSSS_mod <- map(1:nrow(JU178_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU178_MA_P8p_SSSS_mod %*% JU178_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU178_MA_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU178_MA_P8p_SSSS_mod,
                      var.a = JU178_MA_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_MA_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_MA_P8p_SSSS_mod <- data_JU178_MA_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU178_MA_P8p_SSSS_mod <- data_JU178_MA_P8p_SSSS_mod[["mean.obs"]]
      va_data_JU178_MA_P8p_SSSS_mod <- data_JU178_MA_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_JU178_MA_P8p_SSSS_mod <- data_JU178_MA_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU178_MA_P8p_SSSS_mod <- (va_data_JU178_MA_P8p_SSSS_mod/2) / (trait_mean_data_JU178_MA_P8p_SSSS_mod)^2
      
      mean(h2_data_JU178_MA_P8p_SSSS_mod)
      mean(trait_mean_data_JU178_MA_P8p_SSSS_mod)
      mean(va_data_JU178_MA_P8p_SSSS_mod)
      mean(vp_data_JU178_MA_P8p_SSSS_mod)
      mean(Evol_data_JU178_MA_P8p_SSSS_mod)
      
    }
    
  }
  
  #JU178_Vm_P8p_SSSS_mod 
  {
    
    JU178_Vm_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_JU178_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(JU178_Vm_P8p_SSSS_mod, file = "JU178_Vm_P8p_SSSS_mod.rds")
    JU178_Vm_P8p_SSSS_mod <- readRDS("JU178_Vm_P8p_SSSS_mod.rds")
    
    summary(JU178_Vm_P8p_SSSS_mod) 
    plotTrace(JU178_Vm_P8p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("JU178_Vm_P8p_SSSS_mod.pdf")
    plot(JU178_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(JU178_Vm_P8p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU178_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU178_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU178_Vm_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU178_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU178_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_Vm_P8p_SSSS_mod <- JU178_Vm_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU178_Vm_P8p_SSSS_mod <- JU178_Vm_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU178_Vm_P8p_SSSS_mod <- rowSums(JU178_Vm_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU178_Vm_P8p_SSSS_mod) 
      HPDinterval(va_liab_JU178_Vm_P8p_SSSS_mod) 
      
      mean(vlat_JU178_Vm_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU178_Vm_P8p_SSSS_mod <- JU178_Vm_P8p_SSSS_mod[["X"]]
      beta_JU178_Vm_P8p_SSSS_mod <- JU178_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_JU178_Vm_P8p_SSSS_mod   <- apply(beta_JU178_Vm_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_JU178_Vm_P8p_SSSS_mod %*% b))}) 
      mean(vf_JU178_Vm_P8p_SSSS_mod) 
      
      
      h2_liab_JU178_Vm_P8p_SSSS_mod <- va_liab_JU178_Vm_P8p_SSSS_mod / (vlat_JU178_Vm_P8p_SSSS_mod + vf_JU178_Vm_P8p_SSSS_mod)
      mean(h2_liab_JU178_Vm_P8p_SSSS_mod) 
      posterior.mode(h2_liab_JU178_Vm_P8p_SSSS_mod)	
      median(h2_liab_JU178_Vm_P8p_SSSS_mod)		
      HPDinterval(h2_liab_JU178_Vm_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU178_Vm_P8p_SSSS_mod <- ((rowMeans(JU178_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU178_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + JU178_Vm_P8p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU178_Vm_P8p_SSSS_mod <- (va_liab_JU178_Vm_P8p_SSSS_mod/2) / (trait_mean_liab_JU178_Vm_P8p_SSSS_mod)^2
    mean(Evol_liab_JU178_Vm_P8p_SSSS_mod)
    
    
    #JU178_Vm_P8p_SSSS_mod data scale
    {
      
      predict_JU178_Vm_P8p_SSSS_mod <- map(1:nrow(JU178_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU178_Vm_P8p_SSSS_mod %*% JU178_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU178_Vm_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU178_Vm_P8p_SSSS_mod,
                      var.a = JU178_Vm_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_Vm_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_Vm_P8p_SSSS_mod <- data_JU178_Vm_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU178_Vm_P8p_SSSS_mod <- data_JU178_Vm_P8p_SSSS_mod[["mean.obs"]]
      va_data_JU178_Vm_P8p_SSSS_mod <- data_JU178_Vm_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_JU178_Vm_P8p_SSSS_mod <- data_JU178_Vm_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU178_Vm_P8p_SSSS_mod <- (va_data_JU178_Vm_P8p_SSSS_mod/2) / (trait_mean_data_JU178_Vm_P8p_SSSS_mod)^2
      
      mean(h2_data_JU178_Vm_P8p_SSSS_mod)
      mean(trait_mean_data_JU178_Vm_P8p_SSSS_mod)
      mean(va_data_JU178_Vm_P8p_SSSS_mod)
      mean(vp_data_JU178_Vm_P8p_SSSS_mod)
      mean(Evol_data_JU178_Vm_P8p_SSSS_mod)
      
    }
    
  }
  
  
 ## JU178 P5p ----
  
  
  #JU178_CONTROL_P5p_wt_mod 
  {
    
    JU178_CONTROL_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vm_JU178_bi_CONTROL,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)            
    
    saveRDS(JU178_CONTROL_P5p_wt_mod, file = "JU178_CONTROL_P5p_wt_mod.rds")
    JU178_CONTROL_P5p_wt_mod <- readRDS("JU178_CONTROL_P5p_wt_mod.rds")
    
    summary(JU178_CONTROL_P5p_wt_mod) 
    plotTrace(JU178_CONTROL_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU178_CONTROL_P5p_wt_mod.pdf")
    plot(JU178_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU178_CONTROL_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU178_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU178_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU178_CONTROL_P5p_wt_mod[["VCV"]])
    #autocorr.plot(JU178_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU178_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_CONTROL_P5p_wt_mod <- JU178_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU178_CONTROL_P5p_wt_mod <- JU178_CONTROL_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_JU178_CONTROL_P5p_wt_mod <- rowSums(JU178_CONTROL_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_JU178_CONTROL_P5p_wt_mod) 
      HPDinterval(va_liab_JU178_CONTROL_P5p_wt_mod) 
      
      mean(vlat_JU178_CONTROL_P5p_wt_mod) 
      
      #variance of fixed effects
      X_JU178_CONTROL_P5p_wt_mod <- JU178_CONTROL_P5p_wt_mod[["X"]]
      beta_JU178_CONTROL_P5p_wt_mod <- JU178_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU178_CONTROL_P5p_wt_mod   <- apply(beta_JU178_CONTROL_P5p_wt_mod, 1, function(b) {var(as.vector(X_JU178_CONTROL_P5p_wt_mod %*% b))}) 
      mean(vf_JU178_CONTROL_P5p_wt_mod) 
      
      
      h2_liab_JU178_CONTROL_P5p_wt_mod <- va_liab_JU178_CONTROL_P5p_wt_mod / (vlat_JU178_CONTROL_P5p_wt_mod + vf_JU178_CONTROL_P5p_wt_mod)
      mean(h2_liab_JU178_CONTROL_P5p_wt_mod) 
      posterior.mode(h2_liab_JU178_CONTROL_P5p_wt_mod)	
      median(h2_liab_JU178_CONTROL_P5p_wt_mod)		
      HPDinterval(h2_liab_JU178_CONTROL_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU178_CONTROL_P5p_wt_mod <- rowMeans(JU178_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU178_CONTROL_P5p_wt_mod <- (va_liab_JU178_CONTROL_P5p_wt_mod/2) / (trait_mean_liab_JU178_CONTROL_P5p_wt_mod)^2
    mean(Evol_liab_JU178_CONTROL_P5p_wt_mod)
    
    
    #JU178_CONTROL_P5p_wt_mod data scale
    {
      
      predict_JU178_CONTROL_P5p_wt_mod <- map(1:nrow(JU178_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU178_CONTROL_P5p_wt_mod %*% JU178_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU178_CONTROL_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_JU178_CONTROL_P5p_wt_mod,
                      var.a = JU178_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_CONTROL_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_CONTROL_P5p_wt_mod <- data_JU178_CONTROL_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_JU178_CONTROL_P5p_wt_mod <- data_JU178_CONTROL_P5p_wt_mod[["mean.obs"]]
      va_data_JU178_CONTROL_P5p_wt_mod <- data_JU178_CONTROL_P5p_wt_mod[["var.a.obs"]]
      vp_data_JU178_CONTROL_P5p_wt_mod <- data_JU178_CONTROL_P5p_wt_mod[["var.obs"]]
      
      Evol_data_JU178_CONTROL_P5p_wt_mod <- (va_data_JU178_CONTROL_P5p_wt_mod/2) / (trait_mean_data_JU178_CONTROL_P5p_wt_mod)^2
      
      mean(h2_data_JU178_CONTROL_P5p_wt_mod) 
      mean(trait_mean_data_JU178_CONTROL_P5p_wt_mod)
      mean(va_data_JU178_CONTROL_P5p_wt_mod) 
      mean(vp_data_JU178_CONTROL_P5p_wt_mod)
      mean(Evol_data_JU178_CONTROL_P5p_wt_mod)
      
    }
    
  }
  
  #JU178_MA_P5p_wt_mod 
  {
    
    JU178_MA_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_JU178_bi_MA,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)         
    
    saveRDS(JU178_MA_P5p_wt_mod, file = "JU178_MA_P5p_wt_mod.rds")
    JU178_MA_P5p_wt_mod <- readRDS("JU178_MA_P5p_wt_mod.rds")
    
    summary(JU178_MA_P5p_wt_mod) 
    #plot(JU178_MA_P5p_wt_mod)
    
    # traces and posterior densities
    pdf("JU178_MA_P5p_wt_mod.pdf")
    plot(JU178_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU178_MA_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU178_MA_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_MA_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU178_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU178_MA_P5p_wt_mod[["VCV"]])
    #autocorr.plot(JU178_MA_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU178_MA_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_MA_P5p_wt_mod <- JU178_MA_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU178_MA_P5p_wt_mod <- JU178_MA_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_JU178_MA_P5p_wt_mod <- rowSums(JU178_MA_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_JU178_MA_P5p_wt_mod) 
      HPDinterval(va_liab_JU178_MA_P5p_wt_mod) 
      
      mean(vlat_JU178_MA_P5p_wt_mod) 
      
      #variance of fixed effects
      X_JU178_MA_P5p_wt_mod <- JU178_MA_P5p_wt_mod[["X"]]
      beta_JU178_MA_P5p_wt_mod <- JU178_MA_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU178_MA_P5p_wt_mod   <- apply(beta_JU178_MA_P5p_wt_mod, 1, function(b) {var(as.vector(X_JU178_MA_P5p_wt_mod %*% b))}) 
      mean(vf_JU178_MA_P5p_wt_mod) 
      
      h2_liab_JU178_MA_P5p_wt_mod <- va_liab_JU178_MA_P5p_wt_mod / (vlat_JU178_MA_P5p_wt_mod + vf_JU178_MA_P5p_wt_mod)
      mean(h2_liab_JU178_MA_P5p_wt_mod) 
      posterior.mode(h2_liab_JU178_MA_P5p_wt_mod)	
      median(h2_liab_JU178_MA_P5p_wt_mod)		
      HPDinterval(h2_liab_JU178_MA_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU178_MA_P5p_wt_mod <- rowMeans(JU178_MA_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU178_MA_P5p_wt_mod <- (va_liab_JU178_MA_P5p_wt_mod/2) / (trait_mean_liab_JU178_MA_P5p_wt_mod)^2
    mean(Evol_liab_JU178_MA_P5p_wt_mod)
    
    #JU178_MA_P5p_wt_mod data scale
    {
      
      predict_JU178_MA_P5p_wt_mod <- map(1:nrow(JU178_MA_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU178_MA_P5p_wt_mod %*% JU178_MA_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU178_MA_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_JU178_MA_P5p_wt_mod,
                      var.a = JU178_MA_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_MA_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_MA_P5p_wt_mod <- data_JU178_MA_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_JU178_MA_P5p_wt_mod <- data_JU178_MA_P5p_wt_mod[["mean.obs"]]
      va_data_JU178_MA_P5p_wt_mod <- data_JU178_MA_P5p_wt_mod[["var.a.obs"]]
      vp_data_JU178_MA_P5p_wt_mod <- data_JU178_MA_P5p_wt_mod[["var.obs"]]
      
      Evol_data_JU178_MA_P5p_wt_mod <- (va_data_JU178_MA_P5p_wt_mod/2) / (trait_mean_data_JU178_MA_P5p_wt_mod)^2
      
      mean(h2_data_JU178_MA_P5p_wt_mod)
      mean(trait_mean_data_JU178_MA_P5p_wt_mod)
      mean(va_data_JU178_MA_P5p_wt_mod)
      mean(vp_data_JU178_MA_P5p_wt_mod)
      mean(Evol_data_JU178_MA_P5p_wt_mod)
      
    }
    
  }
  
  #JU178_Vm_P5p_wt_mod 
  {
    
    JU178_Vm_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Treatment -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_JU178_data,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)            
    
    saveRDS(JU178_Vm_P5p_wt_mod, file = "JU178_Vm_P5p_wt_mod.rds")
    JU178_Vm_P5p_wt_mod <- readRDS("JU178_Vm_P5p_wt_mod.rds")
    
    summary(JU178_Vm_P5p_wt_mod) 
    plotTrace(JU178_Vm_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("JU178_Vm_P5p_wt_mod.pdf")
    plot(JU178_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(JU178_Vm_P5p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU178_Vm_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_Vm_P5p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU178_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU178_Vm_P5p_wt_mod[["VCV"]])
    #autocorr.plot(JU178_Vm_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU178_Vm_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_Vm_P5p_wt_mod <- JU178_Vm_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU178_Vm_P5p_wt_mod <- JU178_Vm_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_JU178_Vm_P5p_wt_mod <- rowSums(JU178_Vm_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_JU178_Vm_P5p_wt_mod) 
      HPDinterval(va_liab_JU178_Vm_P5p_wt_mod) 
      
      mean(vlat_JU178_Vm_P5p_wt_mod) 
      
      #variance of fixed effects
      X_JU178_Vm_P5p_wt_mod <- JU178_Vm_P5p_wt_mod[["X"]]
      beta_JU178_Vm_P5p_wt_mod <- JU178_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]
      vf_JU178_Vm_P5p_wt_mod   <- apply(beta_JU178_Vm_P5p_wt_mod, 1, function(b) {var(as.vector(X_JU178_Vm_P5p_wt_mod %*% b))}) 
      mean(vf_JU178_Vm_P5p_wt_mod) 
      
      
      h2_liab_JU178_Vm_P5p_wt_mod <- va_liab_JU178_Vm_P5p_wt_mod / (vlat_JU178_Vm_P5p_wt_mod + vf_JU178_Vm_P5p_wt_mod)
      mean(h2_liab_JU178_Vm_P5p_wt_mod) 
      posterior.mode(h2_liab_JU178_Vm_P5p_wt_mod)	
      median(h2_liab_JU178_Vm_P5p_wt_mod)		
      HPDinterval(h2_liab_JU178_Vm_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU178_Vm_P5p_wt_mod <- ((rowMeans(JU178_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU178_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + JU178_Vm_P5p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU178_Vm_P5p_wt_mod <- (va_liab_JU178_Vm_P5p_wt_mod/2) / (trait_mean_liab_JU178_Vm_P5p_wt_mod)^2
    mean(Evol_liab_JU178_Vm_P5p_wt_mod)
    
    
    #JU178_Vm_P5p_wt_mod data scale
    {
      
      predict_JU178_Vm_P5p_wt_mod <- map(1:nrow(JU178_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU178_Vm_P5p_wt_mod %*% JU178_Vm_P5p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU178_Vm_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_JU178_Vm_P5p_wt_mod,
                      var.a = JU178_Vm_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_Vm_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_Vm_P5p_wt_mod <- data_JU178_Vm_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_JU178_Vm_P5p_wt_mod <- data_JU178_Vm_P5p_wt_mod[["mean.obs"]]
      va_data_JU178_Vm_P5p_wt_mod <- data_JU178_Vm_P5p_wt_mod[["var.a.obs"]]
      vp_data_JU178_Vm_P5p_wt_mod <- data_JU178_Vm_P5p_wt_mod[["var.obs"]]
      
      Evol_data_JU178_Vm_P5p_wt_mod <- (va_data_JU178_Vm_P5p_wt_mod/2) / (trait_mean_data_JU178_Vm_P5p_wt_mod)^2
      
      mean(h2_data_JU178_Vm_P5p_wt_mod)
      mean(trait_mean_data_JU178_Vm_P5p_wt_mod)
      mean(va_data_JU178_Vm_P5p_wt_mod)
      mean(vp_data_JU178_Vm_P5p_wt_mod)
      mean(Evol_data_JU178_Vm_P5p_wt_mod)
      
    }
    
  }
  
  
 
  
  
  ## JU178 P6p ----
  
  
  #JU178_CONTROL_P6p_wt_mod 
  {
    
    JU178_CONTROL_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vm_JU178_bi_CONTROL,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)            
    
    saveRDS(JU178_CONTROL_P6p_wt_mod, file = "JU178_CONTROL_P6p_wt_mod.rds")
    JU178_CONTROL_P6p_wt_mod <- readRDS("JU178_CONTROL_P6p_wt_mod.rds")
    
    summary(JU178_CONTROL_P6p_wt_mod) 
    plotTrace(JU178_CONTROL_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU178_CONTROL_P6p_wt_mod.pdf")
    plot(JU178_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU178_CONTROL_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU178_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU178_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU178_CONTROL_P6p_wt_mod[["VCV"]])
    #autocorr.plot(JU178_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU178_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_CONTROL_P6p_wt_mod <- JU178_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU178_CONTROL_P6p_wt_mod <- JU178_CONTROL_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_JU178_CONTROL_P6p_wt_mod <- rowSums(JU178_CONTROL_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_JU178_CONTROL_P6p_wt_mod) 
      HPDinterval(va_liab_JU178_CONTROL_P6p_wt_mod) 
      
      mean(vlat_JU178_CONTROL_P6p_wt_mod) 
      
      #variance of fixed effects
      X_JU178_CONTROL_P6p_wt_mod <- JU178_CONTROL_P6p_wt_mod[["X"]]
      beta_JU178_CONTROL_P6p_wt_mod <- JU178_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU178_CONTROL_P6p_wt_mod   <- apply(beta_JU178_CONTROL_P6p_wt_mod, 1, function(b) {var(as.vector(X_JU178_CONTROL_P6p_wt_mod %*% b))}) 
      mean(vf_JU178_CONTROL_P6p_wt_mod) 
      
      
      h2_liab_JU178_CONTROL_P6p_wt_mod <- va_liab_JU178_CONTROL_P6p_wt_mod / (vlat_JU178_CONTROL_P6p_wt_mod + vf_JU178_CONTROL_P6p_wt_mod)
      mean(h2_liab_JU178_CONTROL_P6p_wt_mod) 
      posterior.mode(h2_liab_JU178_CONTROL_P6p_wt_mod)	
      median(h2_liab_JU178_CONTROL_P6p_wt_mod)		
      HPDinterval(h2_liab_JU178_CONTROL_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU178_CONTROL_P6p_wt_mod <- rowMeans(JU178_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU178_CONTROL_P6p_wt_mod <- (va_liab_JU178_CONTROL_P6p_wt_mod/2) / (trait_mean_liab_JU178_CONTROL_P6p_wt_mod)^2
    mean(Evol_liab_JU178_CONTROL_P6p_wt_mod)
    
    
    #JU178_CONTROL_P6p_wt_mod data scale
    {
      
      predict_JU178_CONTROL_P6p_wt_mod <- map(1:nrow(JU178_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU178_CONTROL_P6p_wt_mod %*% JU178_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU178_CONTROL_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_JU178_CONTROL_P6p_wt_mod,
                      var.a = JU178_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_CONTROL_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_CONTROL_P6p_wt_mod <- data_JU178_CONTROL_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_JU178_CONTROL_P6p_wt_mod <- data_JU178_CONTROL_P6p_wt_mod[["mean.obs"]]
      va_data_JU178_CONTROL_P6p_wt_mod <- data_JU178_CONTROL_P6p_wt_mod[["var.a.obs"]]
      vp_data_JU178_CONTROL_P6p_wt_mod <- data_JU178_CONTROL_P6p_wt_mod[["var.obs"]]
      
      Evol_data_JU178_CONTROL_P6p_wt_mod <- (va_data_JU178_CONTROL_P6p_wt_mod/2) / (trait_mean_data_JU178_CONTROL_P6p_wt_mod)^2
      
      mean(h2_data_JU178_CONTROL_P6p_wt_mod) 
      mean(trait_mean_data_JU178_CONTROL_P6p_wt_mod)
      mean(va_data_JU178_CONTROL_P6p_wt_mod) 
      mean(vp_data_JU178_CONTROL_P6p_wt_mod)
      mean(Evol_data_JU178_CONTROL_P6p_wt_mod)
      
    }
    
  }
  
  #JU178_MA_P6p_wt_mod 
  {
    
    JU178_MA_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_JU178_bi_MA,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)         
    
    saveRDS(JU178_MA_P6p_wt_mod, file = "JU178_MA_P6p_wt_mod.rds")
    JU178_MA_P6p_wt_mod <- readRDS("JU178_MA_P6p_wt_mod.rds")
    
    summary(JU178_MA_P6p_wt_mod) 
    #plot(JU178_MA_P6p_wt_mod)
    
    # traces and posterior densities
    pdf("JU178_MA_P6p_wt_mod.pdf")
    plot(JU178_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU178_MA_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU178_MA_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_MA_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU178_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU178_MA_P6p_wt_mod[["VCV"]])
    #autocorr.plot(JU178_MA_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU178_MA_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_MA_P6p_wt_mod <- JU178_MA_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU178_MA_P6p_wt_mod <- JU178_MA_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_JU178_MA_P6p_wt_mod <- rowSums(JU178_MA_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_JU178_MA_P6p_wt_mod) 
      HPDinterval(va_liab_JU178_MA_P6p_wt_mod) 
      
      mean(vlat_JU178_MA_P6p_wt_mod) 
      
      #variance of fixed effects
      X_JU178_MA_P6p_wt_mod <- JU178_MA_P6p_wt_mod[["X"]]
      beta_JU178_MA_P6p_wt_mod <- JU178_MA_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU178_MA_P6p_wt_mod   <- apply(beta_JU178_MA_P6p_wt_mod, 1, function(b) {var(as.vector(X_JU178_MA_P6p_wt_mod %*% b))}) 
      mean(vf_JU178_MA_P6p_wt_mod) 
      
      h2_liab_JU178_MA_P6p_wt_mod <- va_liab_JU178_MA_P6p_wt_mod / (vlat_JU178_MA_P6p_wt_mod + vf_JU178_MA_P6p_wt_mod)
      mean(h2_liab_JU178_MA_P6p_wt_mod) 
      posterior.mode(h2_liab_JU178_MA_P6p_wt_mod)	
      median(h2_liab_JU178_MA_P6p_wt_mod)		
      HPDinterval(h2_liab_JU178_MA_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU178_MA_P6p_wt_mod <- rowMeans(JU178_MA_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU178_MA_P6p_wt_mod <- (va_liab_JU178_MA_P6p_wt_mod/2) / (trait_mean_liab_JU178_MA_P6p_wt_mod)^2
    mean(Evol_liab_JU178_MA_P6p_wt_mod)
    
    #JU178_MA_P6p_wt_mod data scale
    {
      
      predict_JU178_MA_P6p_wt_mod <- map(1:nrow(JU178_MA_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU178_MA_P6p_wt_mod %*% JU178_MA_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU178_MA_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_JU178_MA_P6p_wt_mod,
                      var.a = JU178_MA_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_MA_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_MA_P6p_wt_mod <- data_JU178_MA_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_JU178_MA_P6p_wt_mod <- data_JU178_MA_P6p_wt_mod[["mean.obs"]]
      va_data_JU178_MA_P6p_wt_mod <- data_JU178_MA_P6p_wt_mod[["var.a.obs"]]
      vp_data_JU178_MA_P6p_wt_mod <- data_JU178_MA_P6p_wt_mod[["var.obs"]]
      
      Evol_data_JU178_MA_P6p_wt_mod <- (va_data_JU178_MA_P6p_wt_mod/2) / (trait_mean_data_JU178_MA_P6p_wt_mod)^2
      
      mean(h2_data_JU178_MA_P6p_wt_mod)
      mean(trait_mean_data_JU178_MA_P6p_wt_mod)
      mean(va_data_JU178_MA_P6p_wt_mod)
      mean(vp_data_JU178_MA_P6p_wt_mod)
      mean(Evol_data_JU178_MA_P6p_wt_mod)
      
    }
    
  }
  
  #JU178_Vm_P6p_wt_mod 
  {
    
    JU178_Vm_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Treatment -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_JU178_data,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)            
    
    saveRDS(JU178_Vm_P6p_wt_mod, file = "JU178_Vm_P6p_wt_mod.rds")
    JU178_Vm_P6p_wt_mod <- readRDS("JU178_Vm_P6p_wt_mod.rds")
    
    summary(JU178_Vm_P6p_wt_mod) 
    plotTrace(JU178_Vm_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("JU178_Vm_P6p_wt_mod.pdf")
    plot(JU178_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(JU178_Vm_P6p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU178_Vm_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_Vm_P6p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU178_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU178_Vm_P6p_wt_mod[["VCV"]])
    #autocorr.plot(JU178_Vm_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU178_Vm_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_Vm_P6p_wt_mod <- JU178_Vm_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU178_Vm_P6p_wt_mod <- JU178_Vm_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_JU178_Vm_P6p_wt_mod <- rowSums(JU178_Vm_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_JU178_Vm_P6p_wt_mod) 
      HPDinterval(va_liab_JU178_Vm_P6p_wt_mod) 
      
      mean(vlat_JU178_Vm_P6p_wt_mod) 
      
      #variance of fixed effects
      X_JU178_Vm_P6p_wt_mod <- JU178_Vm_P6p_wt_mod[["X"]]
      beta_JU178_Vm_P6p_wt_mod <- JU178_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]
      vf_JU178_Vm_P6p_wt_mod   <- apply(beta_JU178_Vm_P6p_wt_mod, 1, function(b) {var(as.vector(X_JU178_Vm_P6p_wt_mod %*% b))}) 
      mean(vf_JU178_Vm_P6p_wt_mod) 
      
      
      h2_liab_JU178_Vm_P6p_wt_mod <- va_liab_JU178_Vm_P6p_wt_mod / (vlat_JU178_Vm_P6p_wt_mod + vf_JU178_Vm_P6p_wt_mod)
      mean(h2_liab_JU178_Vm_P6p_wt_mod) 
      posterior.mode(h2_liab_JU178_Vm_P6p_wt_mod)	
      median(h2_liab_JU178_Vm_P6p_wt_mod)		
      HPDinterval(h2_liab_JU178_Vm_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU178_Vm_P6p_wt_mod <- ((rowMeans(JU178_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU178_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + JU178_Vm_P6p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU178_Vm_P6p_wt_mod <- (va_liab_JU178_Vm_P6p_wt_mod/2) / (trait_mean_liab_JU178_Vm_P6p_wt_mod)^2
    mean(Evol_liab_JU178_Vm_P6p_wt_mod)
    
    
    #JU178_Vm_P6p_wt_mod data scale
    {
      
      predict_JU178_Vm_P6p_wt_mod <- map(1:nrow(JU178_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU178_Vm_P6p_wt_mod %*% JU178_Vm_P6p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU178_Vm_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_JU178_Vm_P6p_wt_mod,
                      var.a = JU178_Vm_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_Vm_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_Vm_P6p_wt_mod <- data_JU178_Vm_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_JU178_Vm_P6p_wt_mod <- data_JU178_Vm_P6p_wt_mod[["mean.obs"]]
      va_data_JU178_Vm_P6p_wt_mod <- data_JU178_Vm_P6p_wt_mod[["var.a.obs"]]
      vp_data_JU178_Vm_P6p_wt_mod <- data_JU178_Vm_P6p_wt_mod[["var.obs"]]
      
      Evol_data_JU178_Vm_P6p_wt_mod <- (va_data_JU178_Vm_P6p_wt_mod/2) / (trait_mean_data_JU178_Vm_P6p_wt_mod)^2
      
      mean(h2_data_JU178_Vm_P6p_wt_mod)
      mean(trait_mean_data_JU178_Vm_P6p_wt_mod)
      mean(va_data_JU178_Vm_P6p_wt_mod)
      mean(vp_data_JU178_Vm_P6p_wt_mod)
      mean(Evol_data_JU178_Vm_P6p_wt_mod)
      
    }
    
  }
  
  
  
  
  
  ## JU178 P7p ----
  
  
  #JU178_CONTROL_P7p_wt_mod 
  {
    
    JU178_CONTROL_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vm_JU178_bi_CONTROL,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)            
    
    saveRDS(JU178_CONTROL_P7p_wt_mod, file = "JU178_CONTROL_P7p_wt_mod.rds")
    JU178_CONTROL_P7p_wt_mod <- readRDS("JU178_CONTROL_P7p_wt_mod.rds")
    
    summary(JU178_CONTROL_P7p_wt_mod) 
    plotTrace(JU178_CONTROL_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU178_CONTROL_P7p_wt_mod.pdf")
    plot(JU178_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU178_CONTROL_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU178_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU178_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU178_CONTROL_P7p_wt_mod[["VCV"]])
    #autocorr.plot(JU178_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU178_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_CONTROL_P7p_wt_mod <- JU178_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU178_CONTROL_P7p_wt_mod <- JU178_CONTROL_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_JU178_CONTROL_P7p_wt_mod <- rowSums(JU178_CONTROL_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_JU178_CONTROL_P7p_wt_mod) 
      HPDinterval(va_liab_JU178_CONTROL_P7p_wt_mod) 
      
      mean(vlat_JU178_CONTROL_P7p_wt_mod) 
      
      #variance of fixed effects
      X_JU178_CONTROL_P7p_wt_mod <- JU178_CONTROL_P7p_wt_mod[["X"]]
      beta_JU178_CONTROL_P7p_wt_mod <- JU178_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU178_CONTROL_P7p_wt_mod   <- apply(beta_JU178_CONTROL_P7p_wt_mod, 1, function(b) {var(as.vector(X_JU178_CONTROL_P7p_wt_mod %*% b))}) 
      mean(vf_JU178_CONTROL_P7p_wt_mod) 
      
      
      h2_liab_JU178_CONTROL_P7p_wt_mod <- va_liab_JU178_CONTROL_P7p_wt_mod / (vlat_JU178_CONTROL_P7p_wt_mod + vf_JU178_CONTROL_P7p_wt_mod)
      mean(h2_liab_JU178_CONTROL_P7p_wt_mod) 
      posterior.mode(h2_liab_JU178_CONTROL_P7p_wt_mod)	
      median(h2_liab_JU178_CONTROL_P7p_wt_mod)		
      HPDinterval(h2_liab_JU178_CONTROL_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU178_CONTROL_P7p_wt_mod <- rowMeans(JU178_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU178_CONTROL_P7p_wt_mod <- (va_liab_JU178_CONTROL_P7p_wt_mod/2) / (trait_mean_liab_JU178_CONTROL_P7p_wt_mod)^2
    mean(Evol_liab_JU178_CONTROL_P7p_wt_mod)
    
    
    #JU178_CONTROL_P7p_wt_mod data scale
    {
      
      predict_JU178_CONTROL_P7p_wt_mod <- map(1:nrow(JU178_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU178_CONTROL_P7p_wt_mod %*% JU178_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU178_CONTROL_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_JU178_CONTROL_P7p_wt_mod,
                      var.a = JU178_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_CONTROL_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_CONTROL_P7p_wt_mod <- data_JU178_CONTROL_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_JU178_CONTROL_P7p_wt_mod <- data_JU178_CONTROL_P7p_wt_mod[["mean.obs"]]
      va_data_JU178_CONTROL_P7p_wt_mod <- data_JU178_CONTROL_P7p_wt_mod[["var.a.obs"]]
      vp_data_JU178_CONTROL_P7p_wt_mod <- data_JU178_CONTROL_P7p_wt_mod[["var.obs"]]
      
      Evol_data_JU178_CONTROL_P7p_wt_mod <- (va_data_JU178_CONTROL_P7p_wt_mod/2) / (trait_mean_data_JU178_CONTROL_P7p_wt_mod)^2
      
      mean(h2_data_JU178_CONTROL_P7p_wt_mod) 
      mean(trait_mean_data_JU178_CONTROL_P7p_wt_mod)
      mean(va_data_JU178_CONTROL_P7p_wt_mod) 
      mean(vp_data_JU178_CONTROL_P7p_wt_mod)
      mean(Evol_data_JU178_CONTROL_P7p_wt_mod)
      
    }
    
  }
  
  #JU178_MA_P7p_wt_mod 
  {
    
    JU178_MA_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_JU178_bi_MA,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)         
    
    saveRDS(JU178_MA_P7p_wt_mod, file = "JU178_MA_P7p_wt_mod.rds")
    JU178_MA_P7p_wt_mod <- readRDS("JU178_MA_P7p_wt_mod.rds")
    
    summary(JU178_MA_P7p_wt_mod) 
    #plot(JU178_MA_P7p_wt_mod)
    
    # traces and posterior densities
    pdf("JU178_MA_P7p_wt_mod.pdf")
    plot(JU178_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU178_MA_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU178_MA_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_MA_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU178_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU178_MA_P7p_wt_mod[["VCV"]])
    #autocorr.plot(JU178_MA_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU178_MA_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_MA_P7p_wt_mod <- JU178_MA_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU178_MA_P7p_wt_mod <- JU178_MA_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_JU178_MA_P7p_wt_mod <- rowSums(JU178_MA_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_JU178_MA_P7p_wt_mod) 
      HPDinterval(va_liab_JU178_MA_P7p_wt_mod) 
      
      mean(vlat_JU178_MA_P7p_wt_mod) 
      
      #variance of fixed effects
      X_JU178_MA_P7p_wt_mod <- JU178_MA_P7p_wt_mod[["X"]]
      beta_JU178_MA_P7p_wt_mod <- JU178_MA_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU178_MA_P7p_wt_mod   <- apply(beta_JU178_MA_P7p_wt_mod, 1, function(b) {var(as.vector(X_JU178_MA_P7p_wt_mod %*% b))}) 
      mean(vf_JU178_MA_P7p_wt_mod) 
      
      h2_liab_JU178_MA_P7p_wt_mod <- va_liab_JU178_MA_P7p_wt_mod / (vlat_JU178_MA_P7p_wt_mod + vf_JU178_MA_P7p_wt_mod)
      mean(h2_liab_JU178_MA_P7p_wt_mod) 
      posterior.mode(h2_liab_JU178_MA_P7p_wt_mod)	
      median(h2_liab_JU178_MA_P7p_wt_mod)		
      HPDinterval(h2_liab_JU178_MA_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU178_MA_P7p_wt_mod <- rowMeans(JU178_MA_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU178_MA_P7p_wt_mod <- (va_liab_JU178_MA_P7p_wt_mod/2) / (trait_mean_liab_JU178_MA_P7p_wt_mod)^2
    mean(Evol_liab_JU178_MA_P7p_wt_mod)
    
    #JU178_MA_P7p_wt_mod data scale
    {
      
      predict_JU178_MA_P7p_wt_mod <- map(1:nrow(JU178_MA_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU178_MA_P7p_wt_mod %*% JU178_MA_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU178_MA_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_JU178_MA_P7p_wt_mod,
                      var.a = JU178_MA_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_MA_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_MA_P7p_wt_mod <- data_JU178_MA_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_JU178_MA_P7p_wt_mod <- data_JU178_MA_P7p_wt_mod[["mean.obs"]]
      va_data_JU178_MA_P7p_wt_mod <- data_JU178_MA_P7p_wt_mod[["var.a.obs"]]
      vp_data_JU178_MA_P7p_wt_mod <- data_JU178_MA_P7p_wt_mod[["var.obs"]]
      
      Evol_data_JU178_MA_P7p_wt_mod <- (va_data_JU178_MA_P7p_wt_mod/2) / (trait_mean_data_JU178_MA_P7p_wt_mod)^2
      
      mean(h2_data_JU178_MA_P7p_wt_mod)
      mean(trait_mean_data_JU178_MA_P7p_wt_mod)
      mean(va_data_JU178_MA_P7p_wt_mod)
      mean(vp_data_JU178_MA_P7p_wt_mod)
      mean(Evol_data_JU178_MA_P7p_wt_mod)
      
    }
    
  }
  
  #JU178_Vm_P7p_wt_mod 
  {
    
    JU178_Vm_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Treatment -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_JU178_data,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)            
    
    saveRDS(JU178_Vm_P7p_wt_mod, file = "JU178_Vm_P7p_wt_mod.rds")
    JU178_Vm_P7p_wt_mod <- readRDS("JU178_Vm_P7p_wt_mod.rds")
    
    summary(JU178_Vm_P7p_wt_mod) 
    plotTrace(JU178_Vm_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("JU178_Vm_P7p_wt_mod.pdf")
    plot(JU178_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(JU178_Vm_P7p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU178_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU178_Vm_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU178_Vm_P7p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU178_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU178_Vm_P7p_wt_mod[["VCV"]])
    #autocorr.plot(JU178_Vm_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU178_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU178_Vm_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU178_Vm_P7p_wt_mod <- JU178_Vm_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU178_Vm_P7p_wt_mod <- JU178_Vm_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_JU178_Vm_P7p_wt_mod <- rowSums(JU178_Vm_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_JU178_Vm_P7p_wt_mod) 
      HPDinterval(va_liab_JU178_Vm_P7p_wt_mod) 
      
      mean(vlat_JU178_Vm_P7p_wt_mod) 
      
      #variance of fixed effects
      X_JU178_Vm_P7p_wt_mod <- JU178_Vm_P7p_wt_mod[["X"]]
      beta_JU178_Vm_P7p_wt_mod <- JU178_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]
      vf_JU178_Vm_P7p_wt_mod   <- apply(beta_JU178_Vm_P7p_wt_mod, 1, function(b) {var(as.vector(X_JU178_Vm_P7p_wt_mod %*% b))}) 
      mean(vf_JU178_Vm_P7p_wt_mod) 
      
      
      h2_liab_JU178_Vm_P7p_wt_mod <- va_liab_JU178_Vm_P7p_wt_mod / (vlat_JU178_Vm_P7p_wt_mod + vf_JU178_Vm_P7p_wt_mod)
      mean(h2_liab_JU178_Vm_P7p_wt_mod) 
      posterior.mode(h2_liab_JU178_Vm_P7p_wt_mod)	
      median(h2_liab_JU178_Vm_P7p_wt_mod)		
      HPDinterval(h2_liab_JU178_Vm_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU178_Vm_P7p_wt_mod <- ((rowMeans(JU178_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU178_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + JU178_Vm_P7p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU178_Vm_P7p_wt_mod <- (va_liab_JU178_Vm_P7p_wt_mod/2) / (trait_mean_liab_JU178_Vm_P7p_wt_mod)^2
    mean(Evol_liab_JU178_Vm_P7p_wt_mod)
    
    
    #JU178_Vm_P7p_wt_mod data scale
    {
      
      predict_JU178_Vm_P7p_wt_mod <- map(1:nrow(JU178_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU178_Vm_P7p_wt_mod %*% JU178_Vm_P7p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU178_Vm_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_JU178_Vm_P7p_wt_mod,
                      var.a = JU178_Vm_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU178_Vm_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU178_Vm_P7p_wt_mod <- data_JU178_Vm_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_JU178_Vm_P7p_wt_mod <- data_JU178_Vm_P7p_wt_mod[["mean.obs"]]
      va_data_JU178_Vm_P7p_wt_mod <- data_JU178_Vm_P7p_wt_mod[["var.a.obs"]]
      vp_data_JU178_Vm_P7p_wt_mod <- data_JU178_Vm_P7p_wt_mod[["var.obs"]]
      
      Evol_data_JU178_Vm_P7p_wt_mod <- (va_data_JU178_Vm_P7p_wt_mod/2) / (trait_mean_data_JU178_Vm_P7p_wt_mod)^2
      
      mean(h2_data_JU178_Vm_P7p_wt_mod)
      mean(trait_mean_data_JU178_Vm_P7p_wt_mod)
      mean(va_data_JU178_Vm_P7p_wt_mod)
      mean(vp_data_JU178_Vm_P7p_wt_mod)
      mean(Evol_data_JU178_Vm_P7p_wt_mod)
      
    }
    
  }
}

#PS2068----
{
  Vm_PS2068_bi_CONTROL <- subset(Vm_PS2068_data, Treatment =="CONTROL")
  Vm_PS2068_bi_MA <- subset(Vm_PS2068_data, Treatment =="MA")
  
  
  ##---- PS2068 P3p ----
  
  
  #PS2068_CONTROL_P3p_SSSS_mod 
  {
    
    PS2068_CONTROL_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vm_PS2068_bi_CONTROL,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)            
    
    saveRDS(PS2068_CONTROL_P3p_SSSS_mod, file = "PS2068_CONTROL_P3p_SSSS_mod.rds")
    PS2068_CONTROL_P3p_SSSS_mod <- readRDS("PS2068_CONTROL_P3p_SSSS_mod.rds")
    
    summary(PS2068_CONTROL_P3p_SSSS_mod) 
    plotTrace(PS2068_CONTROL_P3p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PS2068_CONTROL_P3p_SSSS_mod.pdf")
    plot(PS2068_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(PS2068_CONTROL_P3p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PS2068_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PS2068_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PS2068_CONTROL_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(PS2068_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PS2068_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_CONTROL_P3p_SSSS_mod <- PS2068_CONTROL_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_PS2068_CONTROL_P3p_SSSS_mod <- PS2068_CONTROL_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_PS2068_CONTROL_P3p_SSSS_mod <- rowSums(PS2068_CONTROL_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_PS2068_CONTROL_P3p_SSSS_mod) 
      HPDinterval(va_liab_PS2068_CONTROL_P3p_SSSS_mod) 
      
      mean(vlat_PS2068_CONTROL_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_PS2068_CONTROL_P3p_SSSS_mod <- PS2068_CONTROL_P3p_SSSS_mod[["X"]]
      beta_PS2068_CONTROL_P3p_SSSS_mod <- PS2068_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_PS2068_CONTROL_P3p_SSSS_mod   <- apply(beta_PS2068_CONTROL_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_PS2068_CONTROL_P3p_SSSS_mod %*% b))}) 
      mean(vf_PS2068_CONTROL_P3p_SSSS_mod) 
      
      
      h2_liab_PS2068_CONTROL_P3p_SSSS_mod <- va_liab_PS2068_CONTROL_P3p_SSSS_mod / (vlat_PS2068_CONTROL_P3p_SSSS_mod + vf_PS2068_CONTROL_P3p_SSSS_mod)
      mean(h2_liab_PS2068_CONTROL_P3p_SSSS_mod) 
      posterior.mode(h2_liab_PS2068_CONTROL_P3p_SSSS_mod)	
      median(h2_liab_PS2068_CONTROL_P3p_SSSS_mod)		
      HPDinterval(h2_liab_PS2068_CONTROL_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PS2068_CONTROL_P3p_SSSS_mod <- rowMeans(PS2068_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PS2068_CONTROL_P3p_SSSS_mod <- (va_liab_PS2068_CONTROL_P3p_SSSS_mod/2) / (trait_mean_liab_PS2068_CONTROL_P3p_SSSS_mod)^2
    mean(Evol_liab_PS2068_CONTROL_P3p_SSSS_mod)
    
    
    #PS2068_CONTROL_P3p_SSSS_mod data scale
    {
      
      predict_PS2068_CONTROL_P3p_SSSS_mod <- map(1:nrow(PS2068_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PS2068_CONTROL_P3p_SSSS_mod %*% PS2068_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PS2068_CONTROL_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_PS2068_CONTROL_P3p_SSSS_mod,
                      var.a = PS2068_CONTROL_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_CONTROL_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_CONTROL_P3p_SSSS_mod <- data_PS2068_CONTROL_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_PS2068_CONTROL_P3p_SSSS_mod <- data_PS2068_CONTROL_P3p_SSSS_mod[["mean.obs"]]
      va_data_PS2068_CONTROL_P3p_SSSS_mod <- data_PS2068_CONTROL_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_PS2068_CONTROL_P3p_SSSS_mod <- data_PS2068_CONTROL_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_PS2068_CONTROL_P3p_SSSS_mod <- (va_data_PS2068_CONTROL_P3p_SSSS_mod/2) / (trait_mean_data_PS2068_CONTROL_P3p_SSSS_mod)^2
      
      mean(h2_data_PS2068_CONTROL_P3p_SSSS_mod) 
      mean(trait_mean_data_PS2068_CONTROL_P3p_SSSS_mod)
      mean(va_data_PS2068_CONTROL_P3p_SSSS_mod) 
      mean(vp_data_PS2068_CONTROL_P3p_SSSS_mod)
      mean(Evol_data_PS2068_CONTROL_P3p_SSSS_mod)
      
    }
    
  }
  
  #PS2068_MA_P3p_SSSS_mod 
  {
    
    PS2068_MA_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PS2068_bi_MA,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)         
    
    saveRDS(PS2068_MA_P3p_SSSS_mod, file = "PS2068_MA_P3p_SSSS_mod.rds")
    PS2068_MA_P3p_SSSS_mod <- readRDS("PS2068_MA_P3p_SSSS_mod.rds")
    
    summary(PS2068_MA_P3p_SSSS_mod) 
    #plot(PS2068_MA_P3p_SSSS_mod)
    
    # traces and posterior densities
    pdf("PS2068_MA_P3p_SSSS_mod.pdf")
    plot(PS2068_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(PS2068_MA_P3p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PS2068_MA_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PS2068_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PS2068_MA_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(PS2068_MA_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PS2068_MA_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_MA_P3p_SSSS_mod <- PS2068_MA_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_PS2068_MA_P3p_SSSS_mod <- PS2068_MA_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_PS2068_MA_P3p_SSSS_mod <- rowSums(PS2068_MA_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_PS2068_MA_P3p_SSSS_mod) 
      HPDinterval(va_liab_PS2068_MA_P3p_SSSS_mod) 
      
      mean(vlat_PS2068_MA_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_PS2068_MA_P3p_SSSS_mod <- PS2068_MA_P3p_SSSS_mod[["X"]]
      beta_PS2068_MA_P3p_SSSS_mod <- PS2068_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_PS2068_MA_P3p_SSSS_mod   <- apply(beta_PS2068_MA_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_PS2068_MA_P3p_SSSS_mod %*% b))}) 
      mean(vf_PS2068_MA_P3p_SSSS_mod) 
      
      h2_liab_PS2068_MA_P3p_SSSS_mod <- va_liab_PS2068_MA_P3p_SSSS_mod / (vlat_PS2068_MA_P3p_SSSS_mod + vf_PS2068_MA_P3p_SSSS_mod)
      mean(h2_liab_PS2068_MA_P3p_SSSS_mod) 
      posterior.mode(h2_liab_PS2068_MA_P3p_SSSS_mod)	
      median(h2_liab_PS2068_MA_P3p_SSSS_mod)		
      HPDinterval(h2_liab_PS2068_MA_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PS2068_MA_P3p_SSSS_mod <- rowMeans(PS2068_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PS2068_MA_P3p_SSSS_mod <- (va_liab_PS2068_MA_P3p_SSSS_mod/2) / (trait_mean_liab_PS2068_MA_P3p_SSSS_mod)^2
    mean(Evol_liab_PS2068_MA_P3p_SSSS_mod)
    
    #PS2068_MA_P3p_SSSS_mod data scale
    {
      
      predict_PS2068_MA_P3p_SSSS_mod <- map(1:nrow(PS2068_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PS2068_MA_P3p_SSSS_mod %*% PS2068_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PS2068_MA_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_PS2068_MA_P3p_SSSS_mod,
                      var.a = PS2068_MA_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_MA_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_MA_P3p_SSSS_mod <- data_PS2068_MA_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_PS2068_MA_P3p_SSSS_mod <- data_PS2068_MA_P3p_SSSS_mod[["mean.obs"]]
      va_data_PS2068_MA_P3p_SSSS_mod <- data_PS2068_MA_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_PS2068_MA_P3p_SSSS_mod <- data_PS2068_MA_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_PS2068_MA_P3p_SSSS_mod <- (va_data_PS2068_MA_P3p_SSSS_mod/2) / (trait_mean_data_PS2068_MA_P3p_SSSS_mod)^2
      
      mean(h2_data_PS2068_MA_P3p_SSSS_mod)
      mean(trait_mean_data_PS2068_MA_P3p_SSSS_mod)
      mean(va_data_PS2068_MA_P3p_SSSS_mod)
      mean(vp_data_PS2068_MA_P3p_SSSS_mod)
      mean(Evol_data_PS2068_MA_P3p_SSSS_mod)
      
    }
    
  }
  
  #PS2068_Vm_P3p_SSSS_mod 
  {
    
    PS2068_Vm_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer + Treatment -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PS2068_data,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)            
    
    saveRDS(PS2068_Vm_P3p_SSSS_mod, file = "PS2068_Vm_P3p_SSSS_mod.rds")
    PS2068_Vm_P3p_SSSS_mod <- readRDS("PS2068_Vm_P3p_SSSS_mod.rds")
    
    summary(PS2068_Vm_P3p_SSSS_mod) 
    plotTrace(PS2068_Vm_P3p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("PS2068_Vm_P3p_SSSS_mod.pdf")
    plot(PS2068_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(PS2068_Vm_P3p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PS2068_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PS2068_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PS2068_Vm_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(PS2068_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PS2068_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_Vm_P3p_SSSS_mod <- PS2068_Vm_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_PS2068_Vm_P3p_SSSS_mod <- PS2068_Vm_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_PS2068_Vm_P3p_SSSS_mod <- rowSums(PS2068_Vm_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_PS2068_Vm_P3p_SSSS_mod) 
      HPDinterval(va_liab_PS2068_Vm_P3p_SSSS_mod) 
      
      mean(vlat_PS2068_Vm_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_PS2068_Vm_P3p_SSSS_mod <- PS2068_Vm_P3p_SSSS_mod[["X"]]
      beta_PS2068_Vm_P3p_SSSS_mod <- PS2068_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_PS2068_Vm_P3p_SSSS_mod   <- apply(beta_PS2068_Vm_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_PS2068_Vm_P3p_SSSS_mod %*% b))}) 
      mean(vf_PS2068_Vm_P3p_SSSS_mod) 
      
      
      h2_liab_PS2068_Vm_P3p_SSSS_mod <- va_liab_PS2068_Vm_P3p_SSSS_mod / (vlat_PS2068_Vm_P3p_SSSS_mod + vf_PS2068_Vm_P3p_SSSS_mod)
      mean(h2_liab_PS2068_Vm_P3p_SSSS_mod) 
      posterior.mode(h2_liab_PS2068_Vm_P3p_SSSS_mod)	
      median(h2_liab_PS2068_Vm_P3p_SSSS_mod)		
      HPDinterval(h2_liab_PS2068_Vm_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PS2068_Vm_P3p_SSSS_mod <- ((rowMeans(PS2068_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(PS2068_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + PS2068_Vm_P3p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PS2068_Vm_P3p_SSSS_mod <- (va_liab_PS2068_Vm_P3p_SSSS_mod/2) / (trait_mean_liab_PS2068_Vm_P3p_SSSS_mod)^2
    mean(Evol_liab_PS2068_Vm_P3p_SSSS_mod)
    
    
    #PS2068_Vm_P3p_SSSS_mod data scale
    {
      
      predict_PS2068_Vm_P3p_SSSS_mod <- map(1:nrow(PS2068_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PS2068_Vm_P3p_SSSS_mod %*% PS2068_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PS2068_Vm_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_PS2068_Vm_P3p_SSSS_mod,
                      var.a = PS2068_Vm_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_Vm_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_Vm_P3p_SSSS_mod <- data_PS2068_Vm_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_PS2068_Vm_P3p_SSSS_mod <- data_PS2068_Vm_P3p_SSSS_mod[["mean.obs"]]
      va_data_PS2068_Vm_P3p_SSSS_mod <- data_PS2068_Vm_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_PS2068_Vm_P3p_SSSS_mod <- data_PS2068_Vm_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_PS2068_Vm_P3p_SSSS_mod <- (va_data_PS2068_Vm_P3p_SSSS_mod/2) / (trait_mean_data_PS2068_Vm_P3p_SSSS_mod)^2
      
      mean(h2_data_PS2068_Vm_P3p_SSSS_mod)
      mean(trait_mean_data_PS2068_Vm_P3p_SSSS_mod)
      mean(va_data_PS2068_Vm_P3p_SSSS_mod)
      mean(vp_data_PS2068_Vm_P3p_SSSS_mod)
      mean(Evol_data_PS2068_Vm_P3p_SSSS_mod)
      
    }
    
  }
  
  
 
  
  ##---- PS2068 P4p ----
  
  
  #PS2068_CONTROL_P4p_SSSS_mod 
  {
    
    PS2068_CONTROL_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vm_PS2068_bi_CONTROL,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)            
    
    saveRDS(PS2068_CONTROL_P4p_SSSS_mod, file = "PS2068_CONTROL_P4p_SSSS_mod.rds")
    PS2068_CONTROL_P4p_SSSS_mod <- readRDS("PS2068_CONTROL_P4p_SSSS_mod.rds")
    
    summary(PS2068_CONTROL_P4p_SSSS_mod) 
    plotTrace(PS2068_CONTROL_P4p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PS2068_CONTROL_P4p_SSSS_mod.pdf")
    plot(PS2068_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(PS2068_CONTROL_P4p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PS2068_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PS2068_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PS2068_CONTROL_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(PS2068_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PS2068_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_CONTROL_P4p_SSSS_mod <- PS2068_CONTROL_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_PS2068_CONTROL_P4p_SSSS_mod <- PS2068_CONTROL_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_PS2068_CONTROL_P4p_SSSS_mod <- rowSums(PS2068_CONTROL_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_PS2068_CONTROL_P4p_SSSS_mod) 
      HPDinterval(va_liab_PS2068_CONTROL_P4p_SSSS_mod) 
      
      mean(vlat_PS2068_CONTROL_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_PS2068_CONTROL_P4p_SSSS_mod <- PS2068_CONTROL_P4p_SSSS_mod[["X"]]
      beta_PS2068_CONTROL_P4p_SSSS_mod <- PS2068_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_PS2068_CONTROL_P4p_SSSS_mod   <- apply(beta_PS2068_CONTROL_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_PS2068_CONTROL_P4p_SSSS_mod %*% b))}) 
      mean(vf_PS2068_CONTROL_P4p_SSSS_mod) 
      
      
      h2_liab_PS2068_CONTROL_P4p_SSSS_mod <- va_liab_PS2068_CONTROL_P4p_SSSS_mod / (vlat_PS2068_CONTROL_P4p_SSSS_mod + vf_PS2068_CONTROL_P4p_SSSS_mod)
      mean(h2_liab_PS2068_CONTROL_P4p_SSSS_mod) 
      posterior.mode(h2_liab_PS2068_CONTROL_P4p_SSSS_mod)	
      median(h2_liab_PS2068_CONTROL_P4p_SSSS_mod)		
      HPDinterval(h2_liab_PS2068_CONTROL_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PS2068_CONTROL_P4p_SSSS_mod <- rowMeans(PS2068_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PS2068_CONTROL_P4p_SSSS_mod <- (va_liab_PS2068_CONTROL_P4p_SSSS_mod/2) / (trait_mean_liab_PS2068_CONTROL_P4p_SSSS_mod)^2
    mean(Evol_liab_PS2068_CONTROL_P4p_SSSS_mod)
    
    
    #PS2068_CONTROL_P4p_SSSS_mod data scale
    {
      
      predict_PS2068_CONTROL_P4p_SSSS_mod <- map(1:nrow(PS2068_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PS2068_CONTROL_P4p_SSSS_mod %*% PS2068_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PS2068_CONTROL_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_PS2068_CONTROL_P4p_SSSS_mod,
                      var.a = PS2068_CONTROL_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_CONTROL_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_CONTROL_P4p_SSSS_mod <- data_PS2068_CONTROL_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_PS2068_CONTROL_P4p_SSSS_mod <- data_PS2068_CONTROL_P4p_SSSS_mod[["mean.obs"]]
      va_data_PS2068_CONTROL_P4p_SSSS_mod <- data_PS2068_CONTROL_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_PS2068_CONTROL_P4p_SSSS_mod <- data_PS2068_CONTROL_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_PS2068_CONTROL_P4p_SSSS_mod <- (va_data_PS2068_CONTROL_P4p_SSSS_mod/2) / (trait_mean_data_PS2068_CONTROL_P4p_SSSS_mod)^2
      
      mean(h2_data_PS2068_CONTROL_P4p_SSSS_mod) 
      mean(trait_mean_data_PS2068_CONTROL_P4p_SSSS_mod)
      mean(va_data_PS2068_CONTROL_P4p_SSSS_mod) 
      mean(vp_data_PS2068_CONTROL_P4p_SSSS_mod)
      mean(Evol_data_PS2068_CONTROL_P4p_SSSS_mod)
      
    }
    
  }
  
  #PS2068_MA_P4p_SSSS_mod 
  {
    
    PS2068_MA_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PS2068_bi_MA,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)         
    
    saveRDS(PS2068_MA_P4p_SSSS_mod, file = "PS2068_MA_P4p_SSSS_mod.rds")
    PS2068_MA_P4p_SSSS_mod <- readRDS("PS2068_MA_P4p_SSSS_mod.rds")
    
    summary(PS2068_MA_P4p_SSSS_mod) 
    #plot(PS2068_MA_P4p_SSSS_mod)
    
    # traces and posterior densities
    pdf("PS2068_MA_P4p_SSSS_mod.pdf")
    plot(PS2068_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(PS2068_MA_P4p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PS2068_MA_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PS2068_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PS2068_MA_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(PS2068_MA_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PS2068_MA_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_MA_P4p_SSSS_mod <- PS2068_MA_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_PS2068_MA_P4p_SSSS_mod <- PS2068_MA_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_PS2068_MA_P4p_SSSS_mod <- rowSums(PS2068_MA_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_PS2068_MA_P4p_SSSS_mod) 
      HPDinterval(va_liab_PS2068_MA_P4p_SSSS_mod) 
      
      mean(vlat_PS2068_MA_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_PS2068_MA_P4p_SSSS_mod <- PS2068_MA_P4p_SSSS_mod[["X"]]
      beta_PS2068_MA_P4p_SSSS_mod <- PS2068_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_PS2068_MA_P4p_SSSS_mod   <- apply(beta_PS2068_MA_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_PS2068_MA_P4p_SSSS_mod %*% b))}) 
      mean(vf_PS2068_MA_P4p_SSSS_mod) 
      
      h2_liab_PS2068_MA_P4p_SSSS_mod <- va_liab_PS2068_MA_P4p_SSSS_mod / (vlat_PS2068_MA_P4p_SSSS_mod + vf_PS2068_MA_P4p_SSSS_mod)
      mean(h2_liab_PS2068_MA_P4p_SSSS_mod) 
      posterior.mode(h2_liab_PS2068_MA_P4p_SSSS_mod)	
      median(h2_liab_PS2068_MA_P4p_SSSS_mod)		
      HPDinterval(h2_liab_PS2068_MA_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PS2068_MA_P4p_SSSS_mod <- rowMeans(PS2068_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PS2068_MA_P4p_SSSS_mod <- (va_liab_PS2068_MA_P4p_SSSS_mod/2) / (trait_mean_liab_PS2068_MA_P4p_SSSS_mod)^2
    mean(Evol_liab_PS2068_MA_P4p_SSSS_mod)
    
    #PS2068_MA_P4p_SSSS_mod data scale
    {
      
      predict_PS2068_MA_P4p_SSSS_mod <- map(1:nrow(PS2068_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PS2068_MA_P4p_SSSS_mod %*% PS2068_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PS2068_MA_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_PS2068_MA_P4p_SSSS_mod,
                      var.a = PS2068_MA_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_MA_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_MA_P4p_SSSS_mod <- data_PS2068_MA_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_PS2068_MA_P4p_SSSS_mod <- data_PS2068_MA_P4p_SSSS_mod[["mean.obs"]]
      va_data_PS2068_MA_P4p_SSSS_mod <- data_PS2068_MA_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_PS2068_MA_P4p_SSSS_mod <- data_PS2068_MA_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_PS2068_MA_P4p_SSSS_mod <- (va_data_PS2068_MA_P4p_SSSS_mod/2) / (trait_mean_data_PS2068_MA_P4p_SSSS_mod)^2
      
      mean(h2_data_PS2068_MA_P4p_SSSS_mod)
      mean(trait_mean_data_PS2068_MA_P4p_SSSS_mod)
      mean(va_data_PS2068_MA_P4p_SSSS_mod)
      mean(vp_data_PS2068_MA_P4p_SSSS_mod)
      mean(Evol_data_PS2068_MA_P4p_SSSS_mod)
      
    }
    
  }
  
  #PS2068_Vm_P4p_SSSS_mod 
  {
    
    PS2068_Vm_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer + Treatment -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PS2068_data,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)            
    
    saveRDS(PS2068_Vm_P4p_SSSS_mod, file = "PS2068_Vm_P4p_SSSS_mod.rds")
    PS2068_Vm_P4p_SSSS_mod <- readRDS("PS2068_Vm_P4p_SSSS_mod.rds")
    
    summary(PS2068_Vm_P4p_SSSS_mod) 
    plotTrace(PS2068_Vm_P4p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("PS2068_Vm_P4p_SSSS_mod.pdf")
    plot(PS2068_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(PS2068_Vm_P4p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PS2068_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PS2068_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PS2068_Vm_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(PS2068_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PS2068_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_Vm_P4p_SSSS_mod <- PS2068_Vm_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_PS2068_Vm_P4p_SSSS_mod <- PS2068_Vm_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_PS2068_Vm_P4p_SSSS_mod <- rowSums(PS2068_Vm_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_PS2068_Vm_P4p_SSSS_mod) 
      HPDinterval(va_liab_PS2068_Vm_P4p_SSSS_mod) 
      
      mean(vlat_PS2068_Vm_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_PS2068_Vm_P4p_SSSS_mod <- PS2068_Vm_P4p_SSSS_mod[["X"]]
      beta_PS2068_Vm_P4p_SSSS_mod <- PS2068_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_PS2068_Vm_P4p_SSSS_mod   <- apply(beta_PS2068_Vm_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_PS2068_Vm_P4p_SSSS_mod %*% b))}) 
      mean(vf_PS2068_Vm_P4p_SSSS_mod) 
      
      
      h2_liab_PS2068_Vm_P4p_SSSS_mod <- va_liab_PS2068_Vm_P4p_SSSS_mod / (vlat_PS2068_Vm_P4p_SSSS_mod + vf_PS2068_Vm_P4p_SSSS_mod)
      mean(h2_liab_PS2068_Vm_P4p_SSSS_mod) 
      posterior.mode(h2_liab_PS2068_Vm_P4p_SSSS_mod)	
      median(h2_liab_PS2068_Vm_P4p_SSSS_mod)		
      HPDinterval(h2_liab_PS2068_Vm_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PS2068_Vm_P4p_SSSS_mod <- ((rowMeans(PS2068_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(PS2068_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + PS2068_Vm_P4p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PS2068_Vm_P4p_SSSS_mod <- (va_liab_PS2068_Vm_P4p_SSSS_mod/2) / (trait_mean_liab_PS2068_Vm_P4p_SSSS_mod)^2
    mean(Evol_liab_PS2068_Vm_P4p_SSSS_mod)
    
    
    #PS2068_Vm_P4p_SSSS_mod data scale
    {
      
      predict_PS2068_Vm_P4p_SSSS_mod <- map(1:nrow(PS2068_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PS2068_Vm_P4p_SSSS_mod %*% PS2068_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PS2068_Vm_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_PS2068_Vm_P4p_SSSS_mod,
                      var.a = PS2068_Vm_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_Vm_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_Vm_P4p_SSSS_mod <- data_PS2068_Vm_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_PS2068_Vm_P4p_SSSS_mod <- data_PS2068_Vm_P4p_SSSS_mod[["mean.obs"]]
      va_data_PS2068_Vm_P4p_SSSS_mod <- data_PS2068_Vm_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_PS2068_Vm_P4p_SSSS_mod <- data_PS2068_Vm_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_PS2068_Vm_P4p_SSSS_mod <- (va_data_PS2068_Vm_P4p_SSSS_mod/2) / (trait_mean_data_PS2068_Vm_P4p_SSSS_mod)^2
      
      mean(h2_data_PS2068_Vm_P4p_SSSS_mod)
      mean(trait_mean_data_PS2068_Vm_P4p_SSSS_mod)
      median(va_data_PS2068_Vm_P4p_SSSS_mod)/2
      mean(vp_data_PS2068_Vm_P4p_SSSS_mod)
      mean(Evol_data_PS2068_Vm_P4p_SSSS_mod)
      
    }
    
  }
  
  
 
  
  
  ##---- PS2068 P8p ----
  
  
  #PS2068_CONTROL_P8p_SSSS_mod 
  {
    
    PS2068_CONTROL_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vm_PS2068_bi_CONTROL,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)            
    
    saveRDS(PS2068_CONTROL_P8p_SSSS_mod, file = "PS2068_CONTROL_P8p_SSSS_mod.rds")
    PS2068_CONTROL_P8p_SSSS_mod <- readRDS("PS2068_CONTROL_P8p_SSSS_mod.rds")
    
    summary(PS2068_CONTROL_P8p_SSSS_mod) 
    plotTrace(PS2068_CONTROL_P8p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PS2068_CONTROL_P8p_SSSS_mod.pdf")
    plot(PS2068_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(PS2068_CONTROL_P8p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PS2068_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PS2068_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PS2068_CONTROL_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(PS2068_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PS2068_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_CONTROL_P8p_SSSS_mod <- PS2068_CONTROL_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_PS2068_CONTROL_P8p_SSSS_mod <- PS2068_CONTROL_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_PS2068_CONTROL_P8p_SSSS_mod <- rowSums(PS2068_CONTROL_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_PS2068_CONTROL_P8p_SSSS_mod) 
      HPDinterval(va_liab_PS2068_CONTROL_P8p_SSSS_mod) 
      
      mean(vlat_PS2068_CONTROL_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_PS2068_CONTROL_P8p_SSSS_mod <- PS2068_CONTROL_P8p_SSSS_mod[["X"]]
      beta_PS2068_CONTROL_P8p_SSSS_mod <- PS2068_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_PS2068_CONTROL_P8p_SSSS_mod   <- apply(beta_PS2068_CONTROL_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_PS2068_CONTROL_P8p_SSSS_mod %*% b))}) 
      mean(vf_PS2068_CONTROL_P8p_SSSS_mod) 
      
      
      h2_liab_PS2068_CONTROL_P8p_SSSS_mod <- va_liab_PS2068_CONTROL_P8p_SSSS_mod / (vlat_PS2068_CONTROL_P8p_SSSS_mod + vf_PS2068_CONTROL_P8p_SSSS_mod)
      mean(h2_liab_PS2068_CONTROL_P8p_SSSS_mod) 
      posterior.mode(h2_liab_PS2068_CONTROL_P8p_SSSS_mod)	
      median(h2_liab_PS2068_CONTROL_P8p_SSSS_mod)		
      HPDinterval(h2_liab_PS2068_CONTROL_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PS2068_CONTROL_P8p_SSSS_mod <- rowMeans(PS2068_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PS2068_CONTROL_P8p_SSSS_mod <- (va_liab_PS2068_CONTROL_P8p_SSSS_mod/2) / (trait_mean_liab_PS2068_CONTROL_P8p_SSSS_mod)^2
    mean(Evol_liab_PS2068_CONTROL_P8p_SSSS_mod)
    
    
    #PS2068_CONTROL_P8p_SSSS_mod data scale
    {
      
      predict_PS2068_CONTROL_P8p_SSSS_mod <- map(1:nrow(PS2068_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PS2068_CONTROL_P8p_SSSS_mod %*% PS2068_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PS2068_CONTROL_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_PS2068_CONTROL_P8p_SSSS_mod,
                      var.a = PS2068_CONTROL_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_CONTROL_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_CONTROL_P8p_SSSS_mod <- data_PS2068_CONTROL_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_PS2068_CONTROL_P8p_SSSS_mod <- data_PS2068_CONTROL_P8p_SSSS_mod[["mean.obs"]]
      va_data_PS2068_CONTROL_P8p_SSSS_mod <- data_PS2068_CONTROL_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_PS2068_CONTROL_P8p_SSSS_mod <- data_PS2068_CONTROL_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_PS2068_CONTROL_P8p_SSSS_mod <- (va_data_PS2068_CONTROL_P8p_SSSS_mod/2) / (trait_mean_data_PS2068_CONTROL_P8p_SSSS_mod)^2
      
      mean(h2_data_PS2068_CONTROL_P8p_SSSS_mod) 
      mean(trait_mean_data_PS2068_CONTROL_P8p_SSSS_mod)
      mean(va_data_PS2068_CONTROL_P8p_SSSS_mod) 
      mean(vp_data_PS2068_CONTROL_P8p_SSSS_mod)
      mean(Evol_data_PS2068_CONTROL_P8p_SSSS_mod)
      
    }
    
  }
  
  #PS2068_MA_P8p_SSSS_mod 
  {
    
    PS2068_MA_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PS2068_bi_MA,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)         
    
    saveRDS(PS2068_MA_P8p_SSSS_mod, file = "PS2068_MA_P8p_SSSS_mod.rds")
    PS2068_MA_P8p_SSSS_mod <- readRDS("PS2068_MA_P8p_SSSS_mod.rds")
    
    summary(PS2068_MA_P8p_SSSS_mod) 
    #plot(PS2068_MA_P8p_SSSS_mod)
    
    # traces and posterior densities
    pdf("PS2068_MA_P8p_SSSS_mod.pdf")
    plot(PS2068_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(PS2068_MA_P8p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PS2068_MA_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PS2068_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PS2068_MA_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(PS2068_MA_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PS2068_MA_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_MA_P8p_SSSS_mod <- PS2068_MA_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_PS2068_MA_P8p_SSSS_mod <- PS2068_MA_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_PS2068_MA_P8p_SSSS_mod <- rowSums(PS2068_MA_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_PS2068_MA_P8p_SSSS_mod) 
      HPDinterval(va_liab_PS2068_MA_P8p_SSSS_mod) 
      
      mean(vlat_PS2068_MA_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_PS2068_MA_P8p_SSSS_mod <- PS2068_MA_P8p_SSSS_mod[["X"]]
      beta_PS2068_MA_P8p_SSSS_mod <- PS2068_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_PS2068_MA_P8p_SSSS_mod   <- apply(beta_PS2068_MA_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_PS2068_MA_P8p_SSSS_mod %*% b))}) 
      mean(vf_PS2068_MA_P8p_SSSS_mod) 
      
      h2_liab_PS2068_MA_P8p_SSSS_mod <- va_liab_PS2068_MA_P8p_SSSS_mod / (vlat_PS2068_MA_P8p_SSSS_mod + vf_PS2068_MA_P8p_SSSS_mod)
      mean(h2_liab_PS2068_MA_P8p_SSSS_mod) 
      posterior.mode(h2_liab_PS2068_MA_P8p_SSSS_mod)	
      median(h2_liab_PS2068_MA_P8p_SSSS_mod)		
      HPDinterval(h2_liab_PS2068_MA_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PS2068_MA_P8p_SSSS_mod <- rowMeans(PS2068_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PS2068_MA_P8p_SSSS_mod <- (va_liab_PS2068_MA_P8p_SSSS_mod/2) / (trait_mean_liab_PS2068_MA_P8p_SSSS_mod)^2
    mean(Evol_liab_PS2068_MA_P8p_SSSS_mod)
    
    #PS2068_MA_P8p_SSSS_mod data scale
    {
      
      predict_PS2068_MA_P8p_SSSS_mod <- map(1:nrow(PS2068_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PS2068_MA_P8p_SSSS_mod %*% PS2068_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PS2068_MA_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_PS2068_MA_P8p_SSSS_mod,
                      var.a = PS2068_MA_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_MA_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_MA_P8p_SSSS_mod <- data_PS2068_MA_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_PS2068_MA_P8p_SSSS_mod <- data_PS2068_MA_P8p_SSSS_mod[["mean.obs"]]
      va_data_PS2068_MA_P8p_SSSS_mod <- data_PS2068_MA_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_PS2068_MA_P8p_SSSS_mod <- data_PS2068_MA_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_PS2068_MA_P8p_SSSS_mod <- (va_data_PS2068_MA_P8p_SSSS_mod/2) / (trait_mean_data_PS2068_MA_P8p_SSSS_mod)^2
      
      mean(h2_data_PS2068_MA_P8p_SSSS_mod)
      mean(trait_mean_data_PS2068_MA_P8p_SSSS_mod)
      mean(va_data_PS2068_MA_P8p_SSSS_mod)
      mean(vp_data_PS2068_MA_P8p_SSSS_mod)
      mean(Evol_data_PS2068_MA_P8p_SSSS_mod)
      
    }
    
  }
  
  #PS2068_Vm_P8p_SSSS_mod 
  {
    
    PS2068_Vm_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer + Treatment -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PS2068_data,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)            
    
    saveRDS(PS2068_Vm_P8p_SSSS_mod, file = "PS2068_Vm_P8p_SSSS_mod.rds")
    PS2068_Vm_P8p_SSSS_mod <- readRDS("PS2068_Vm_P8p_SSSS_mod.rds")
    
    summary(PS2068_Vm_P8p_SSSS_mod) 
    plotTrace(PS2068_Vm_P8p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("PS2068_Vm_P8p_SSSS_mod.pdf")
    plot(PS2068_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(PS2068_Vm_P8p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PS2068_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PS2068_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PS2068_Vm_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(PS2068_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PS2068_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_Vm_P8p_SSSS_mod <- PS2068_Vm_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_PS2068_Vm_P8p_SSSS_mod <- PS2068_Vm_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_PS2068_Vm_P8p_SSSS_mod <- rowSums(PS2068_Vm_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_PS2068_Vm_P8p_SSSS_mod) 
      HPDinterval(va_liab_PS2068_Vm_P8p_SSSS_mod) 
      
      mean(vlat_PS2068_Vm_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_PS2068_Vm_P8p_SSSS_mod <- PS2068_Vm_P8p_SSSS_mod[["X"]]
      beta_PS2068_Vm_P8p_SSSS_mod <- PS2068_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_PS2068_Vm_P8p_SSSS_mod   <- apply(beta_PS2068_Vm_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_PS2068_Vm_P8p_SSSS_mod %*% b))}) 
      mean(vf_PS2068_Vm_P8p_SSSS_mod) 
      
      
      h2_liab_PS2068_Vm_P8p_SSSS_mod <- va_liab_PS2068_Vm_P8p_SSSS_mod / (vlat_PS2068_Vm_P8p_SSSS_mod + vf_PS2068_Vm_P8p_SSSS_mod)
      mean(h2_liab_PS2068_Vm_P8p_SSSS_mod) 
      posterior.mode(h2_liab_PS2068_Vm_P8p_SSSS_mod)	
      median(h2_liab_PS2068_Vm_P8p_SSSS_mod)		
      HPDinterval(h2_liab_PS2068_Vm_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PS2068_Vm_P8p_SSSS_mod <- ((rowMeans(PS2068_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(PS2068_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + PS2068_Vm_P8p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PS2068_Vm_P8p_SSSS_mod <- (va_liab_PS2068_Vm_P8p_SSSS_mod/2) / (trait_mean_liab_PS2068_Vm_P8p_SSSS_mod)^2
    mean(Evol_liab_PS2068_Vm_P8p_SSSS_mod)
    
    
    #PS2068_Vm_P8p_SSSS_mod data scale
    {
      
      predict_PS2068_Vm_P8p_SSSS_mod <- map(1:nrow(PS2068_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PS2068_Vm_P8p_SSSS_mod %*% PS2068_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PS2068_Vm_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_PS2068_Vm_P8p_SSSS_mod,
                      var.a = PS2068_Vm_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_Vm_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_Vm_P8p_SSSS_mod <- data_PS2068_Vm_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_PS2068_Vm_P8p_SSSS_mod <- data_PS2068_Vm_P8p_SSSS_mod[["mean.obs"]]
      va_data_PS2068_Vm_P8p_SSSS_mod <- data_PS2068_Vm_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_PS2068_Vm_P8p_SSSS_mod <- data_PS2068_Vm_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_PS2068_Vm_P8p_SSSS_mod <- (va_data_PS2068_Vm_P8p_SSSS_mod/2) / (trait_mean_data_PS2068_Vm_P8p_SSSS_mod)^2
      
      mean(h2_data_PS2068_Vm_P8p_SSSS_mod)
      mean(trait_mean_data_PS2068_Vm_P8p_SSSS_mod)
      mean(va_data_PS2068_Vm_P8p_SSSS_mod)
      mean(vp_data_PS2068_Vm_P8p_SSSS_mod)
      mean(Evol_data_PS2068_Vm_P8p_SSSS_mod)
      
    }
    
  }
  
  
 ## PS2068 P5p ----
  
  
  #PS2068_CONTROL_P5p_wt_mod 
  {
    
    PS2068_CONTROL_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                          random      = ~ LineB + BlockRep,
                                          family      = "threshold",
                                          data        = Vm_PS2068_bi_CONTROL,
                                          prior       = prior_bi_block,
                                          nitt        = 1260000,       
                                          thin        = 500,           
                                          burnin      = 10000,
                                          trunc       = TRUE,
                                          pr          = TRUE,
                                          pl          = TRUE)            
    
    saveRDS(PS2068_CONTROL_P5p_wt_mod, file = "PS2068_CONTROL_P5p_wt_mod.rds")
    PS2068_CONTROL_P5p_wt_mod <- readRDS("PS2068_CONTROL_P5p_wt_mod.rds")
    
    summary(PS2068_CONTROL_P5p_wt_mod) 
    plotTrace(PS2068_CONTROL_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PS2068_CONTROL_P5p_wt_mod.pdf")
    plot(PS2068_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PS2068_CONTROL_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PS2068_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PS2068_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PS2068_CONTROL_P5p_wt_mod[["VCV"]])
    #autocorr.plot(PS2068_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PS2068_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_CONTROL_P5p_wt_mod <- PS2068_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_PS2068_CONTROL_P5p_wt_mod <- PS2068_CONTROL_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_PS2068_CONTROL_P5p_wt_mod <- rowSums(PS2068_CONTROL_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_PS2068_CONTROL_P5p_wt_mod) 
      HPDinterval(va_liab_PS2068_CONTROL_P5p_wt_mod) 
      
      mean(vlat_PS2068_CONTROL_P5p_wt_mod) 
      
      #variance of fixed effects
      X_PS2068_CONTROL_P5p_wt_mod <- PS2068_CONTROL_P5p_wt_mod[["X"]]
      beta_PS2068_CONTROL_P5p_wt_mod <- PS2068_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_PS2068_CONTROL_P5p_wt_mod   <- apply(beta_PS2068_CONTROL_P5p_wt_mod, 1, function(b) {var(as.vector(X_PS2068_CONTROL_P5p_wt_mod %*% b))}) 
      mean(vf_PS2068_CONTROL_P5p_wt_mod) 
      
      
      h2_liab_PS2068_CONTROL_P5p_wt_mod <- va_liab_PS2068_CONTROL_P5p_wt_mod / (vlat_PS2068_CONTROL_P5p_wt_mod + vf_PS2068_CONTROL_P5p_wt_mod)
      mean(h2_liab_PS2068_CONTROL_P5p_wt_mod) 
      posterior.mode(h2_liab_PS2068_CONTROL_P5p_wt_mod)	
      median(h2_liab_PS2068_CONTROL_P5p_wt_mod)		
      HPDinterval(h2_liab_PS2068_CONTROL_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PS2068_CONTROL_P5p_wt_mod <- rowMeans(PS2068_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PS2068_CONTROL_P5p_wt_mod <- (va_liab_PS2068_CONTROL_P5p_wt_mod/2) / (trait_mean_liab_PS2068_CONTROL_P5p_wt_mod)^2
    mean(Evol_liab_PS2068_CONTROL_P5p_wt_mod)
    
    
    #PS2068_CONTROL_P5p_wt_mod data scale
    {
      
      predict_PS2068_CONTROL_P5p_wt_mod <- map(1:nrow(PS2068_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PS2068_CONTROL_P5p_wt_mod %*% PS2068_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PS2068_CONTROL_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_PS2068_CONTROL_P5p_wt_mod,
                      var.a = PS2068_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_CONTROL_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_CONTROL_P5p_wt_mod <- data_PS2068_CONTROL_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_PS2068_CONTROL_P5p_wt_mod <- data_PS2068_CONTROL_P5p_wt_mod[["mean.obs"]]
      va_data_PS2068_CONTROL_P5p_wt_mod <- data_PS2068_CONTROL_P5p_wt_mod[["var.a.obs"]]
      vp_data_PS2068_CONTROL_P5p_wt_mod <- data_PS2068_CONTROL_P5p_wt_mod[["var.obs"]]
      
      Evol_data_PS2068_CONTROL_P5p_wt_mod <- (va_data_PS2068_CONTROL_P5p_wt_mod/2) / (trait_mean_data_PS2068_CONTROL_P5p_wt_mod)^2
      
      mean(h2_data_PS2068_CONTROL_P5p_wt_mod) 
      mean(trait_mean_data_PS2068_CONTROL_P5p_wt_mod)
      mean(va_data_PS2068_CONTROL_P5p_wt_mod) 
      mean(vp_data_PS2068_CONTROL_P5p_wt_mod)
      mean(Evol_data_PS2068_CONTROL_P5p_wt_mod)
      
    }
    
  }
  
  #PS2068_MA_P5p_wt_mod 
  {
    
    PS2068_MA_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                     random      = ~ LineB + BlockRep,
                                     family      = "threshold",
                                     data        = Vm_PS2068_bi_MA,
                                     prior       = prior_bi_block,
                                     nitt        = 1260000,       
                                     thin        = 500,           
                                     burnin      = 10000,
                                     trunc       = TRUE,
                                     pr          = TRUE,
                                     pl          = TRUE)         
    
    saveRDS(PS2068_MA_P5p_wt_mod, file = "PS2068_MA_P5p_wt_mod.rds")
    PS2068_MA_P5p_wt_mod <- readRDS("PS2068_MA_P5p_wt_mod.rds")
    
    summary(PS2068_MA_P5p_wt_mod) 
    #plot(PS2068_MA_P5p_wt_mod)
    
    # traces and posterior densities
    pdf("PS2068_MA_P5p_wt_mod.pdf")
    plot(PS2068_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PS2068_MA_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PS2068_MA_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_MA_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PS2068_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PS2068_MA_P5p_wt_mod[["VCV"]])
    #autocorr.plot(PS2068_MA_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PS2068_MA_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_MA_P5p_wt_mod <- PS2068_MA_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_PS2068_MA_P5p_wt_mod <- PS2068_MA_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_PS2068_MA_P5p_wt_mod <- rowSums(PS2068_MA_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_PS2068_MA_P5p_wt_mod) 
      HPDinterval(va_liab_PS2068_MA_P5p_wt_mod) 
      
      mean(vlat_PS2068_MA_P5p_wt_mod) 
      
      #variance of fixed effects
      X_PS2068_MA_P5p_wt_mod <- PS2068_MA_P5p_wt_mod[["X"]]
      beta_PS2068_MA_P5p_wt_mod <- PS2068_MA_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_PS2068_MA_P5p_wt_mod   <- apply(beta_PS2068_MA_P5p_wt_mod, 1, function(b) {var(as.vector(X_PS2068_MA_P5p_wt_mod %*% b))}) 
      mean(vf_PS2068_MA_P5p_wt_mod) 
      
      h2_liab_PS2068_MA_P5p_wt_mod <- va_liab_PS2068_MA_P5p_wt_mod / (vlat_PS2068_MA_P5p_wt_mod + vf_PS2068_MA_P5p_wt_mod)
      mean(h2_liab_PS2068_MA_P5p_wt_mod) 
      posterior.mode(h2_liab_PS2068_MA_P5p_wt_mod)	
      median(h2_liab_PS2068_MA_P5p_wt_mod)		
      HPDinterval(h2_liab_PS2068_MA_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PS2068_MA_P5p_wt_mod <- rowMeans(PS2068_MA_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PS2068_MA_P5p_wt_mod <- (va_liab_PS2068_MA_P5p_wt_mod/2) / (trait_mean_liab_PS2068_MA_P5p_wt_mod)^2
    mean(Evol_liab_PS2068_MA_P5p_wt_mod)
    
    #PS2068_MA_P5p_wt_mod data scale
    {
      
      predict_PS2068_MA_P5p_wt_mod <- map(1:nrow(PS2068_MA_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PS2068_MA_P5p_wt_mod %*% PS2068_MA_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PS2068_MA_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_PS2068_MA_P5p_wt_mod,
                      var.a = PS2068_MA_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_MA_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_MA_P5p_wt_mod <- data_PS2068_MA_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_PS2068_MA_P5p_wt_mod <- data_PS2068_MA_P5p_wt_mod[["mean.obs"]]
      va_data_PS2068_MA_P5p_wt_mod <- data_PS2068_MA_P5p_wt_mod[["var.a.obs"]]
      vp_data_PS2068_MA_P5p_wt_mod <- data_PS2068_MA_P5p_wt_mod[["var.obs"]]
      
      Evol_data_PS2068_MA_P5p_wt_mod <- (va_data_PS2068_MA_P5p_wt_mod/2) / (trait_mean_data_PS2068_MA_P5p_wt_mod)^2
      
      mean(h2_data_PS2068_MA_P5p_wt_mod)
      mean(trait_mean_data_PS2068_MA_P5p_wt_mod)
      mean(va_data_PS2068_MA_P5p_wt_mod)
      mean(vp_data_PS2068_MA_P5p_wt_mod)
      mean(Evol_data_PS2068_MA_P5p_wt_mod)
      
    }
    
  }
  
  #PS2068_Vm_P5p_wt_mod 
  {
    
    PS2068_Vm_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Treatment -1,
                                     random      = ~ LineB + BlockRep,
                                     family      = "threshold",
                                     data        = Vm_PS2068_data,
                                     prior       = prior_bi_block,
                                     nitt        = 1260000,       
                                     thin        = 500,           
                                     burnin      = 10000,
                                     trunc       = TRUE,
                                     pr          = TRUE,
                                     pl          = TRUE)            
    
    saveRDS(PS2068_Vm_P5p_wt_mod, file = "PS2068_Vm_P5p_wt_mod.rds")
    PS2068_Vm_P5p_wt_mod <- readRDS("PS2068_Vm_P5p_wt_mod.rds")
    
    summary(PS2068_Vm_P5p_wt_mod) 
    plotTrace(PS2068_Vm_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("PS2068_Vm_P5p_wt_mod.pdf")
    plot(PS2068_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(PS2068_Vm_P5p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PS2068_Vm_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_Vm_P5p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PS2068_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PS2068_Vm_P5p_wt_mod[["VCV"]])
    #autocorr.plot(PS2068_Vm_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PS2068_Vm_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_Vm_P5p_wt_mod <- PS2068_Vm_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_PS2068_Vm_P5p_wt_mod <- PS2068_Vm_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_PS2068_Vm_P5p_wt_mod <- rowSums(PS2068_Vm_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_PS2068_Vm_P5p_wt_mod) 
      HPDinterval(va_liab_PS2068_Vm_P5p_wt_mod) 
      
      mean(vlat_PS2068_Vm_P5p_wt_mod) 
      
      #variance of fixed effects
      X_PS2068_Vm_P5p_wt_mod <- PS2068_Vm_P5p_wt_mod[["X"]]
      beta_PS2068_Vm_P5p_wt_mod <- PS2068_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]
      vf_PS2068_Vm_P5p_wt_mod   <- apply(beta_PS2068_Vm_P5p_wt_mod, 1, function(b) {var(as.vector(X_PS2068_Vm_P5p_wt_mod %*% b))}) 
      mean(vf_PS2068_Vm_P5p_wt_mod) 
      
      
      h2_liab_PS2068_Vm_P5p_wt_mod <- va_liab_PS2068_Vm_P5p_wt_mod / (vlat_PS2068_Vm_P5p_wt_mod + vf_PS2068_Vm_P5p_wt_mod)
      mean(h2_liab_PS2068_Vm_P5p_wt_mod) 
      posterior.mode(h2_liab_PS2068_Vm_P5p_wt_mod)	
      median(h2_liab_PS2068_Vm_P5p_wt_mod)		
      HPDinterval(h2_liab_PS2068_Vm_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PS2068_Vm_P5p_wt_mod <- ((rowMeans(PS2068_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(PS2068_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + PS2068_Vm_P5p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PS2068_Vm_P5p_wt_mod <- (va_liab_PS2068_Vm_P5p_wt_mod/2) / (trait_mean_liab_PS2068_Vm_P5p_wt_mod)^2
    mean(Evol_liab_PS2068_Vm_P5p_wt_mod)
    
    
    #PS2068_Vm_P5p_wt_mod data scale
    {
      
      predict_PS2068_Vm_P5p_wt_mod <- map(1:nrow(PS2068_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PS2068_Vm_P5p_wt_mod %*% PS2068_Vm_P5p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PS2068_Vm_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_PS2068_Vm_P5p_wt_mod,
                      var.a = PS2068_Vm_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_Vm_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_Vm_P5p_wt_mod <- data_PS2068_Vm_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_PS2068_Vm_P5p_wt_mod <- data_PS2068_Vm_P5p_wt_mod[["mean.obs"]]
      va_data_PS2068_Vm_P5p_wt_mod <- data_PS2068_Vm_P5p_wt_mod[["var.a.obs"]]
      vp_data_PS2068_Vm_P5p_wt_mod <- data_PS2068_Vm_P5p_wt_mod[["var.obs"]]
      
      Evol_data_PS2068_Vm_P5p_wt_mod <- (va_data_PS2068_Vm_P5p_wt_mod/2) / (trait_mean_data_PS2068_Vm_P5p_wt_mod)^2
      
      mean(h2_data_PS2068_Vm_P5p_wt_mod)
      mean(trait_mean_data_PS2068_Vm_P5p_wt_mod)
      mean(va_data_PS2068_Vm_P5p_wt_mod)
      mean(vp_data_PS2068_Vm_P5p_wt_mod)
      mean(Evol_data_PS2068_Vm_P5p_wt_mod)
      
    }
    
  }
  
  
 
  
  ## PS2068 P6p ----
  
  
  #PS2068_CONTROL_P6p_wt_mod 
  {
    
    PS2068_CONTROL_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                          random      = ~ LineB + BlockRep,
                                          family      = "threshold",
                                          data        = Vm_PS2068_bi_CONTROL,
                                          prior       = prior_bi_block,
                                          nitt        = 1260000,       
                                          thin        = 500,           
                                          burnin      = 10000,
                                          trunc       = TRUE,
                                          pr          = TRUE,
                                          pl          = TRUE)            
    
    saveRDS(PS2068_CONTROL_P6p_wt_mod, file = "PS2068_CONTROL_P6p_wt_mod.rds")
    PS2068_CONTROL_P6p_wt_mod <- readRDS("PS2068_CONTROL_P6p_wt_mod.rds")
    
    summary(PS2068_CONTROL_P6p_wt_mod) 
    plotTrace(PS2068_CONTROL_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PS2068_CONTROL_P6p_wt_mod.pdf")
    plot(PS2068_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PS2068_CONTROL_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PS2068_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PS2068_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PS2068_CONTROL_P6p_wt_mod[["VCV"]])
    #autocorr.plot(PS2068_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PS2068_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_CONTROL_P6p_wt_mod <- PS2068_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_PS2068_CONTROL_P6p_wt_mod <- PS2068_CONTROL_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_PS2068_CONTROL_P6p_wt_mod <- rowSums(PS2068_CONTROL_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_PS2068_CONTROL_P6p_wt_mod) 
      HPDinterval(va_liab_PS2068_CONTROL_P6p_wt_mod) 
      
      mean(vlat_PS2068_CONTROL_P6p_wt_mod) 
      
      #variance of fixed effects
      X_PS2068_CONTROL_P6p_wt_mod <- PS2068_CONTROL_P6p_wt_mod[["X"]]
      beta_PS2068_CONTROL_P6p_wt_mod <- PS2068_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_PS2068_CONTROL_P6p_wt_mod   <- apply(beta_PS2068_CONTROL_P6p_wt_mod, 1, function(b) {var(as.vector(X_PS2068_CONTROL_P6p_wt_mod %*% b))}) 
      mean(vf_PS2068_CONTROL_P6p_wt_mod) 
      
      
      h2_liab_PS2068_CONTROL_P6p_wt_mod <- va_liab_PS2068_CONTROL_P6p_wt_mod / (vlat_PS2068_CONTROL_P6p_wt_mod + vf_PS2068_CONTROL_P6p_wt_mod)
      mean(h2_liab_PS2068_CONTROL_P6p_wt_mod) 
      posterior.mode(h2_liab_PS2068_CONTROL_P6p_wt_mod)	
      median(h2_liab_PS2068_CONTROL_P6p_wt_mod)		
      HPDinterval(h2_liab_PS2068_CONTROL_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PS2068_CONTROL_P6p_wt_mod <- rowMeans(PS2068_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PS2068_CONTROL_P6p_wt_mod <- (va_liab_PS2068_CONTROL_P6p_wt_mod/2) / (trait_mean_liab_PS2068_CONTROL_P6p_wt_mod)^2
    mean(Evol_liab_PS2068_CONTROL_P6p_wt_mod)
    
    
    #PS2068_CONTROL_P6p_wt_mod data scale
    {
      
      predict_PS2068_CONTROL_P6p_wt_mod <- map(1:nrow(PS2068_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PS2068_CONTROL_P6p_wt_mod %*% PS2068_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PS2068_CONTROL_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_PS2068_CONTROL_P6p_wt_mod,
                      var.a = PS2068_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_CONTROL_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_CONTROL_P6p_wt_mod <- data_PS2068_CONTROL_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_PS2068_CONTROL_P6p_wt_mod <- data_PS2068_CONTROL_P6p_wt_mod[["mean.obs"]]
      va_data_PS2068_CONTROL_P6p_wt_mod <- data_PS2068_CONTROL_P6p_wt_mod[["var.a.obs"]]
      vp_data_PS2068_CONTROL_P6p_wt_mod <- data_PS2068_CONTROL_P6p_wt_mod[["var.obs"]]
      
      Evol_data_PS2068_CONTROL_P6p_wt_mod <- (va_data_PS2068_CONTROL_P6p_wt_mod/2) / (trait_mean_data_PS2068_CONTROL_P6p_wt_mod)^2
      
      mean(h2_data_PS2068_CONTROL_P6p_wt_mod) 
      mean(trait_mean_data_PS2068_CONTROL_P6p_wt_mod)
      mean(va_data_PS2068_CONTROL_P6p_wt_mod) 
      mean(vp_data_PS2068_CONTROL_P6p_wt_mod)
      mean(Evol_data_PS2068_CONTROL_P6p_wt_mod)
      
    }
    
  }
  
  #PS2068_MA_P6p_wt_mod 
  {
    
    PS2068_MA_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                     random      = ~ LineB + BlockRep,
                                     family      = "threshold",
                                     data        = Vm_PS2068_bi_MA,
                                     prior       = prior_bi_block,
                                     nitt        = 1260000,       
                                     thin        = 500,           
                                     burnin      = 10000,
                                     trunc       = TRUE,
                                     pr          = TRUE,
                                     pl          = TRUE)         
    
    saveRDS(PS2068_MA_P6p_wt_mod, file = "PS2068_MA_P6p_wt_mod.rds")
    PS2068_MA_P6p_wt_mod <- readRDS("PS2068_MA_P6p_wt_mod.rds")
    
    summary(PS2068_MA_P6p_wt_mod) 
    #plot(PS2068_MA_P6p_wt_mod)
    
    # traces and posterior densities
    pdf("PS2068_MA_P6p_wt_mod.pdf")
    plot(PS2068_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PS2068_MA_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PS2068_MA_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_MA_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PS2068_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PS2068_MA_P6p_wt_mod[["VCV"]])
    #autocorr.plot(PS2068_MA_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PS2068_MA_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_MA_P6p_wt_mod <- PS2068_MA_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_PS2068_MA_P6p_wt_mod <- PS2068_MA_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_PS2068_MA_P6p_wt_mod <- rowSums(PS2068_MA_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_PS2068_MA_P6p_wt_mod) 
      HPDinterval(va_liab_PS2068_MA_P6p_wt_mod) 
      
      mean(vlat_PS2068_MA_P6p_wt_mod) 
      
      #variance of fixed effects
      X_PS2068_MA_P6p_wt_mod <- PS2068_MA_P6p_wt_mod[["X"]]
      beta_PS2068_MA_P6p_wt_mod <- PS2068_MA_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_PS2068_MA_P6p_wt_mod   <- apply(beta_PS2068_MA_P6p_wt_mod, 1, function(b) {var(as.vector(X_PS2068_MA_P6p_wt_mod %*% b))}) 
      mean(vf_PS2068_MA_P6p_wt_mod) 
      
      h2_liab_PS2068_MA_P6p_wt_mod <- va_liab_PS2068_MA_P6p_wt_mod / (vlat_PS2068_MA_P6p_wt_mod + vf_PS2068_MA_P6p_wt_mod)
      mean(h2_liab_PS2068_MA_P6p_wt_mod) 
      posterior.mode(h2_liab_PS2068_MA_P6p_wt_mod)	
      median(h2_liab_PS2068_MA_P6p_wt_mod)		
      HPDinterval(h2_liab_PS2068_MA_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PS2068_MA_P6p_wt_mod <- rowMeans(PS2068_MA_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PS2068_MA_P6p_wt_mod <- (va_liab_PS2068_MA_P6p_wt_mod/2) / (trait_mean_liab_PS2068_MA_P6p_wt_mod)^2
    mean(Evol_liab_PS2068_MA_P6p_wt_mod)
    
    #PS2068_MA_P6p_wt_mod data scale
    {
      
      predict_PS2068_MA_P6p_wt_mod <- map(1:nrow(PS2068_MA_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PS2068_MA_P6p_wt_mod %*% PS2068_MA_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PS2068_MA_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_PS2068_MA_P6p_wt_mod,
                      var.a = PS2068_MA_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_MA_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_MA_P6p_wt_mod <- data_PS2068_MA_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_PS2068_MA_P6p_wt_mod <- data_PS2068_MA_P6p_wt_mod[["mean.obs"]]
      va_data_PS2068_MA_P6p_wt_mod <- data_PS2068_MA_P6p_wt_mod[["var.a.obs"]]
      vp_data_PS2068_MA_P6p_wt_mod <- data_PS2068_MA_P6p_wt_mod[["var.obs"]]
      
      Evol_data_PS2068_MA_P6p_wt_mod <- (va_data_PS2068_MA_P6p_wt_mod/2) / (trait_mean_data_PS2068_MA_P6p_wt_mod)^2
      
      mean(h2_data_PS2068_MA_P6p_wt_mod)
      mean(trait_mean_data_PS2068_MA_P6p_wt_mod)
      mean(va_data_PS2068_MA_P6p_wt_mod)
      mean(vp_data_PS2068_MA_P6p_wt_mod)
      mean(Evol_data_PS2068_MA_P6p_wt_mod)
      
    }
    
  }
  
  #PS2068_Vm_P6p_wt_mod 
  {
    
    PS2068_Vm_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Treatment -1,
                                     random      = ~ LineB + BlockRep,
                                     family      = "threshold",
                                     data        = Vm_PS2068_data,
                                     prior       = prior_bi_block,
                                     nitt        = 1260000,       
                                     thin        = 500,           
                                     burnin      = 10000,
                                     trunc       = TRUE,
                                     pr          = TRUE,
                                     pl          = TRUE)            
    
    saveRDS(PS2068_Vm_P6p_wt_mod, file = "PS2068_Vm_P6p_wt_mod.rds")
    PS2068_Vm_P6p_wt_mod <- readRDS("PS2068_Vm_P6p_wt_mod.rds")
    
    summary(PS2068_Vm_P6p_wt_mod) 
    plotTrace(PS2068_Vm_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("PS2068_Vm_P6p_wt_mod.pdf")
    plot(PS2068_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(PS2068_Vm_P6p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PS2068_Vm_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_Vm_P6p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PS2068_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PS2068_Vm_P6p_wt_mod[["VCV"]])
    #autocorr.plot(PS2068_Vm_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PS2068_Vm_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_Vm_P6p_wt_mod <- PS2068_Vm_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_PS2068_Vm_P6p_wt_mod <- PS2068_Vm_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_PS2068_Vm_P6p_wt_mod <- rowSums(PS2068_Vm_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_PS2068_Vm_P6p_wt_mod) 
      HPDinterval(va_liab_PS2068_Vm_P6p_wt_mod) 
      
      mean(vlat_PS2068_Vm_P6p_wt_mod) 
      
      #variance of fixed effects
      X_PS2068_Vm_P6p_wt_mod <- PS2068_Vm_P6p_wt_mod[["X"]]
      beta_PS2068_Vm_P6p_wt_mod <- PS2068_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]
      vf_PS2068_Vm_P6p_wt_mod   <- apply(beta_PS2068_Vm_P6p_wt_mod, 1, function(b) {var(as.vector(X_PS2068_Vm_P6p_wt_mod %*% b))}) 
      mean(vf_PS2068_Vm_P6p_wt_mod) 
      
      
      h2_liab_PS2068_Vm_P6p_wt_mod <- va_liab_PS2068_Vm_P6p_wt_mod / (vlat_PS2068_Vm_P6p_wt_mod + vf_PS2068_Vm_P6p_wt_mod)
      mean(h2_liab_PS2068_Vm_P6p_wt_mod) 
      posterior.mode(h2_liab_PS2068_Vm_P6p_wt_mod)	
      median(h2_liab_PS2068_Vm_P6p_wt_mod)		
      HPDinterval(h2_liab_PS2068_Vm_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PS2068_Vm_P6p_wt_mod <- ((rowMeans(PS2068_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(PS2068_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + PS2068_Vm_P6p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PS2068_Vm_P6p_wt_mod <- (va_liab_PS2068_Vm_P6p_wt_mod/2) / (trait_mean_liab_PS2068_Vm_P6p_wt_mod)^2
    mean(Evol_liab_PS2068_Vm_P6p_wt_mod)
    
    
    #PS2068_Vm_P6p_wt_mod data scale
    {
      
      predict_PS2068_Vm_P6p_wt_mod <- map(1:nrow(PS2068_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PS2068_Vm_P6p_wt_mod %*% PS2068_Vm_P6p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PS2068_Vm_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_PS2068_Vm_P6p_wt_mod,
                      var.a = PS2068_Vm_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_Vm_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_Vm_P6p_wt_mod <- data_PS2068_Vm_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_PS2068_Vm_P6p_wt_mod <- data_PS2068_Vm_P6p_wt_mod[["mean.obs"]]
      va_data_PS2068_Vm_P6p_wt_mod <- data_PS2068_Vm_P6p_wt_mod[["var.a.obs"]]
      vp_data_PS2068_Vm_P6p_wt_mod <- data_PS2068_Vm_P6p_wt_mod[["var.obs"]]
      
      Evol_data_PS2068_Vm_P6p_wt_mod <- (va_data_PS2068_Vm_P6p_wt_mod/2) / (trait_mean_data_PS2068_Vm_P6p_wt_mod)^2
      
      mean(h2_data_PS2068_Vm_P6p_wt_mod)
      mean(trait_mean_data_PS2068_Vm_P6p_wt_mod)
      mean(va_data_PS2068_Vm_P6p_wt_mod)
      mean(vp_data_PS2068_Vm_P6p_wt_mod)
      mean(Evol_data_PS2068_Vm_P6p_wt_mod)
      
    }
    
  }
  
  
  
  
  ## PS2068 P7p ----
  
  
  #PS2068_CONTROL_P7p_wt_mod 
  {
    
    PS2068_CONTROL_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                          random      = ~ LineB + BlockRep,
                                          family      = "threshold",
                                          data        = Vm_PS2068_bi_CONTROL,
                                          prior       = prior_bi_block,
                                          nitt        = 1260000,       
                                          thin        = 500,           
                                          burnin      = 10000,
                                          trunc       = TRUE,
                                          pr          = TRUE,
                                          pl          = TRUE)            
    
    saveRDS(PS2068_CONTROL_P7p_wt_mod, file = "PS2068_CONTROL_P7p_wt_mod.rds")
    PS2068_CONTROL_P7p_wt_mod <- readRDS("PS2068_CONTROL_P7p_wt_mod.rds")
    
    summary(PS2068_CONTROL_P7p_wt_mod) 
    plotTrace(PS2068_CONTROL_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PS2068_CONTROL_P7p_wt_mod.pdf")
    plot(PS2068_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PS2068_CONTROL_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PS2068_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PS2068_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PS2068_CONTROL_P7p_wt_mod[["VCV"]])
    #autocorr.plot(PS2068_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PS2068_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_CONTROL_P7p_wt_mod <- PS2068_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_PS2068_CONTROL_P7p_wt_mod <- PS2068_CONTROL_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_PS2068_CONTROL_P7p_wt_mod <- rowSums(PS2068_CONTROL_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_PS2068_CONTROL_P7p_wt_mod) 
      HPDinterval(va_liab_PS2068_CONTROL_P7p_wt_mod) 
      
      mean(vlat_PS2068_CONTROL_P7p_wt_mod) 
      
      #variance of fixed effects
      X_PS2068_CONTROL_P7p_wt_mod <- PS2068_CONTROL_P7p_wt_mod[["X"]]
      beta_PS2068_CONTROL_P7p_wt_mod <- PS2068_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_PS2068_CONTROL_P7p_wt_mod   <- apply(beta_PS2068_CONTROL_P7p_wt_mod, 1, function(b) {var(as.vector(X_PS2068_CONTROL_P7p_wt_mod %*% b))}) 
      mean(vf_PS2068_CONTROL_P7p_wt_mod) 
      
      
      h2_liab_PS2068_CONTROL_P7p_wt_mod <- va_liab_PS2068_CONTROL_P7p_wt_mod / (vlat_PS2068_CONTROL_P7p_wt_mod + vf_PS2068_CONTROL_P7p_wt_mod)
      mean(h2_liab_PS2068_CONTROL_P7p_wt_mod) 
      posterior.mode(h2_liab_PS2068_CONTROL_P7p_wt_mod)	
      median(h2_liab_PS2068_CONTROL_P7p_wt_mod)		
      HPDinterval(h2_liab_PS2068_CONTROL_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PS2068_CONTROL_P7p_wt_mod <- rowMeans(PS2068_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PS2068_CONTROL_P7p_wt_mod <- (va_liab_PS2068_CONTROL_P7p_wt_mod/2) / (trait_mean_liab_PS2068_CONTROL_P7p_wt_mod)^2
    mean(Evol_liab_PS2068_CONTROL_P7p_wt_mod)
    
    
    #PS2068_CONTROL_P7p_wt_mod data scale
    {
      
      predict_PS2068_CONTROL_P7p_wt_mod <- map(1:nrow(PS2068_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PS2068_CONTROL_P7p_wt_mod %*% PS2068_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PS2068_CONTROL_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_PS2068_CONTROL_P7p_wt_mod,
                      var.a = PS2068_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_CONTROL_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_CONTROL_P7p_wt_mod <- data_PS2068_CONTROL_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_PS2068_CONTROL_P7p_wt_mod <- data_PS2068_CONTROL_P7p_wt_mod[["mean.obs"]]
      va_data_PS2068_CONTROL_P7p_wt_mod <- data_PS2068_CONTROL_P7p_wt_mod[["var.a.obs"]]
      vp_data_PS2068_CONTROL_P7p_wt_mod <- data_PS2068_CONTROL_P7p_wt_mod[["var.obs"]]
      
      Evol_data_PS2068_CONTROL_P7p_wt_mod <- (va_data_PS2068_CONTROL_P7p_wt_mod/2) / (trait_mean_data_PS2068_CONTROL_P7p_wt_mod)^2
      
      mean(h2_data_PS2068_CONTROL_P7p_wt_mod) 
      mean(trait_mean_data_PS2068_CONTROL_P7p_wt_mod)
      mean(va_data_PS2068_CONTROL_P7p_wt_mod) 
      mean(vp_data_PS2068_CONTROL_P7p_wt_mod)
      mean(Evol_data_PS2068_CONTROL_P7p_wt_mod)
      
    }
    
  }
  
  #PS2068_MA_P7p_wt_mod 
  {
    
    PS2068_MA_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                     random      = ~ LineB + BlockRep,
                                     family      = "threshold",
                                     data        = Vm_PS2068_bi_MA,
                                     prior       = prior_bi_block,
                                     nitt        = 1260000,       
                                     thin        = 500,           
                                     burnin      = 10000,
                                     trunc       = TRUE,
                                     pr          = TRUE,
                                     pl          = TRUE)         
    
    saveRDS(PS2068_MA_P7p_wt_mod, file = "PS2068_MA_P7p_wt_mod.rds")
    PS2068_MA_P7p_wt_mod <- readRDS("PS2068_MA_P7p_wt_mod.rds")
    
    summary(PS2068_MA_P7p_wt_mod) 
    #plot(PS2068_MA_P7p_wt_mod)
    
    # traces and posterior densities
    pdf("PS2068_MA_P7p_wt_mod.pdf")
    plot(PS2068_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PS2068_MA_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PS2068_MA_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_MA_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PS2068_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PS2068_MA_P7p_wt_mod[["VCV"]])
    #autocorr.plot(PS2068_MA_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PS2068_MA_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_MA_P7p_wt_mod <- PS2068_MA_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_PS2068_MA_P7p_wt_mod <- PS2068_MA_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_PS2068_MA_P7p_wt_mod <- rowSums(PS2068_MA_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_PS2068_MA_P7p_wt_mod) 
      HPDinterval(va_liab_PS2068_MA_P7p_wt_mod) 
      
      mean(vlat_PS2068_MA_P7p_wt_mod) 
      
      #variance of fixed effects
      X_PS2068_MA_P7p_wt_mod <- PS2068_MA_P7p_wt_mod[["X"]]
      beta_PS2068_MA_P7p_wt_mod <- PS2068_MA_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_PS2068_MA_P7p_wt_mod   <- apply(beta_PS2068_MA_P7p_wt_mod, 1, function(b) {var(as.vector(X_PS2068_MA_P7p_wt_mod %*% b))}) 
      mean(vf_PS2068_MA_P7p_wt_mod) 
      
      h2_liab_PS2068_MA_P7p_wt_mod <- va_liab_PS2068_MA_P7p_wt_mod / (vlat_PS2068_MA_P7p_wt_mod + vf_PS2068_MA_P7p_wt_mod)
      mean(h2_liab_PS2068_MA_P7p_wt_mod) 
      posterior.mode(h2_liab_PS2068_MA_P7p_wt_mod)	
      median(h2_liab_PS2068_MA_P7p_wt_mod)		
      HPDinterval(h2_liab_PS2068_MA_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PS2068_MA_P7p_wt_mod <- rowMeans(PS2068_MA_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PS2068_MA_P7p_wt_mod <- (va_liab_PS2068_MA_P7p_wt_mod/2) / (trait_mean_liab_PS2068_MA_P7p_wt_mod)^2
    mean(Evol_liab_PS2068_MA_P7p_wt_mod)
    
    #PS2068_MA_P7p_wt_mod data scale
    {
      
      predict_PS2068_MA_P7p_wt_mod <- map(1:nrow(PS2068_MA_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PS2068_MA_P7p_wt_mod %*% PS2068_MA_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PS2068_MA_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_PS2068_MA_P7p_wt_mod,
                      var.a = PS2068_MA_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_MA_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_MA_P7p_wt_mod <- data_PS2068_MA_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_PS2068_MA_P7p_wt_mod <- data_PS2068_MA_P7p_wt_mod[["mean.obs"]]
      va_data_PS2068_MA_P7p_wt_mod <- data_PS2068_MA_P7p_wt_mod[["var.a.obs"]]
      vp_data_PS2068_MA_P7p_wt_mod <- data_PS2068_MA_P7p_wt_mod[["var.obs"]]
      
      Evol_data_PS2068_MA_P7p_wt_mod <- (va_data_PS2068_MA_P7p_wt_mod/2) / (trait_mean_data_PS2068_MA_P7p_wt_mod)^2
      
      mean(h2_data_PS2068_MA_P7p_wt_mod)
      mean(trait_mean_data_PS2068_MA_P7p_wt_mod)
      mean(va_data_PS2068_MA_P7p_wt_mod)
      mean(vp_data_PS2068_MA_P7p_wt_mod)
      mean(Evol_data_PS2068_MA_P7p_wt_mod)
      
    }
    
  }
  
  #PS2068_Vm_P7p_wt_mod 
  {
    
    PS2068_Vm_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Treatment -1,
                                     random      = ~ LineB + BlockRep,
                                     family      = "threshold",
                                     data        = Vm_PS2068_data,
                                     prior       = prior_bi_block,
                                     nitt        = 1260000,       
                                     thin        = 500,           
                                     burnin      = 10000,
                                     trunc       = TRUE,
                                     pr          = TRUE,
                                     pl          = TRUE)            
    
    saveRDS(PS2068_Vm_P7p_wt_mod, file = "PS2068_Vm_P7p_wt_mod.rds")
    PS2068_Vm_P7p_wt_mod <- readRDS("PS2068_Vm_P7p_wt_mod.rds")
    
    summary(PS2068_Vm_P7p_wt_mod) 
    plotTrace(PS2068_Vm_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("PS2068_Vm_P7p_wt_mod.pdf")
    plot(PS2068_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(PS2068_Vm_P7p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PS2068_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PS2068_Vm_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PS2068_Vm_P7p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PS2068_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PS2068_Vm_P7p_wt_mod[["VCV"]])
    #autocorr.plot(PS2068_Vm_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PS2068_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PS2068_Vm_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PS2068_Vm_P7p_wt_mod <- PS2068_Vm_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_PS2068_Vm_P7p_wt_mod <- PS2068_Vm_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_PS2068_Vm_P7p_wt_mod <- rowSums(PS2068_Vm_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_PS2068_Vm_P7p_wt_mod) 
      HPDinterval(va_liab_PS2068_Vm_P7p_wt_mod) 
      
      mean(vlat_PS2068_Vm_P7p_wt_mod) 
      
      #variance of fixed effects
      X_PS2068_Vm_P7p_wt_mod <- PS2068_Vm_P7p_wt_mod[["X"]]
      beta_PS2068_Vm_P7p_wt_mod <- PS2068_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]
      vf_PS2068_Vm_P7p_wt_mod   <- apply(beta_PS2068_Vm_P7p_wt_mod, 1, function(b) {var(as.vector(X_PS2068_Vm_P7p_wt_mod %*% b))}) 
      mean(vf_PS2068_Vm_P7p_wt_mod) 
      
      
      h2_liab_PS2068_Vm_P7p_wt_mod <- va_liab_PS2068_Vm_P7p_wt_mod / (vlat_PS2068_Vm_P7p_wt_mod + vf_PS2068_Vm_P7p_wt_mod)
      mean(h2_liab_PS2068_Vm_P7p_wt_mod) 
      posterior.mode(h2_liab_PS2068_Vm_P7p_wt_mod)	
      median(h2_liab_PS2068_Vm_P7p_wt_mod)		
      HPDinterval(h2_liab_PS2068_Vm_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PS2068_Vm_P7p_wt_mod <- ((rowMeans(PS2068_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(PS2068_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + PS2068_Vm_P7p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PS2068_Vm_P7p_wt_mod <- (va_liab_PS2068_Vm_P7p_wt_mod/2) / (trait_mean_liab_PS2068_Vm_P7p_wt_mod)^2
    mean(Evol_liab_PS2068_Vm_P7p_wt_mod)
    
    
    #PS2068_Vm_P7p_wt_mod data scale
    {
      
      predict_PS2068_Vm_P7p_wt_mod <- map(1:nrow(PS2068_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PS2068_Vm_P7p_wt_mod %*% PS2068_Vm_P7p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PS2068_Vm_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_PS2068_Vm_P7p_wt_mod,
                      var.a = PS2068_Vm_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PS2068_Vm_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PS2068_Vm_P7p_wt_mod <- data_PS2068_Vm_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_PS2068_Vm_P7p_wt_mod <- data_PS2068_Vm_P7p_wt_mod[["mean.obs"]]
      va_data_PS2068_Vm_P7p_wt_mod <- data_PS2068_Vm_P7p_wt_mod[["var.a.obs"]]
      vp_data_PS2068_Vm_P7p_wt_mod <- data_PS2068_Vm_P7p_wt_mod[["var.obs"]]
      
      Evol_data_PS2068_Vm_P7p_wt_mod <- (va_data_PS2068_Vm_P7p_wt_mod/2) / (trait_mean_data_PS2068_Vm_P7p_wt_mod)^2
      
      mean(h2_data_PS2068_Vm_P7p_wt_mod)
      mean(trait_mean_data_PS2068_Vm_P7p_wt_mod)
      mean(va_data_PS2068_Vm_P7p_wt_mod)
      mean(vp_data_PS2068_Vm_P7p_wt_mod)
      mean(Evol_data_PS2068_Vm_P7p_wt_mod)
      
    }
    
  }
  
  
}

#JU77----
{
  Vm_JU77_bi_CONTROL <- subset(Vm_JU77_data, Treatment =="CONTROL")
  Vm_JU77_bi_MA <- subset(Vm_JU77_data, Treatment =="MA")
  
  
  ##---- JU77 P3p ----
  
  
  #JU77_CONTROL_P3p_SSSS_mod 
  {
    
    JU77_CONTROL_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_JU77_bi_CONTROL,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)            
    
    saveRDS(JU77_CONTROL_P3p_SSSS_mod, file = "JU77_CONTROL_P3p_SSSS_mod.rds")
    JU77_CONTROL_P3p_SSSS_mod <- readRDS("JU77_CONTROL_P3p_SSSS_mod.rds")
    
    summary(JU77_CONTROL_P3p_SSSS_mod) 
    plotTrace(JU77_CONTROL_P3p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU77_CONTROL_P3p_SSSS_mod.pdf")
    plot(JU77_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(JU77_CONTROL_P3p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU77_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU77_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU77_CONTROL_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU77_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU77_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_CONTROL_P3p_SSSS_mod <- JU77_CONTROL_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU77_CONTROL_P3p_SSSS_mod <- JU77_CONTROL_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU77_CONTROL_P3p_SSSS_mod <- rowSums(JU77_CONTROL_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU77_CONTROL_P3p_SSSS_mod) 
      HPDinterval(va_liab_JU77_CONTROL_P3p_SSSS_mod) 
      
      mean(vlat_JU77_CONTROL_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU77_CONTROL_P3p_SSSS_mod <- JU77_CONTROL_P3p_SSSS_mod[["X"]]
      beta_JU77_CONTROL_P3p_SSSS_mod <- JU77_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_JU77_CONTROL_P3p_SSSS_mod   <- apply(beta_JU77_CONTROL_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_JU77_CONTROL_P3p_SSSS_mod %*% b))}) 
      mean(vf_JU77_CONTROL_P3p_SSSS_mod) 
      
      
      h2_liab_JU77_CONTROL_P3p_SSSS_mod <- va_liab_JU77_CONTROL_P3p_SSSS_mod / (vlat_JU77_CONTROL_P3p_SSSS_mod + vf_JU77_CONTROL_P3p_SSSS_mod)
      mean(h2_liab_JU77_CONTROL_P3p_SSSS_mod) 
      posterior.mode(h2_liab_JU77_CONTROL_P3p_SSSS_mod)	
      median(h2_liab_JU77_CONTROL_P3p_SSSS_mod)		
      HPDinterval(h2_liab_JU77_CONTROL_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU77_CONTROL_P3p_SSSS_mod <- rowMeans(JU77_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU77_CONTROL_P3p_SSSS_mod <- (va_liab_JU77_CONTROL_P3p_SSSS_mod/2) / (trait_mean_liab_JU77_CONTROL_P3p_SSSS_mod)^2
    mean(Evol_liab_JU77_CONTROL_P3p_SSSS_mod)
    
    
    #JU77_CONTROL_P3p_SSSS_mod data scale
    {
      
      predict_JU77_CONTROL_P3p_SSSS_mod <- map(1:nrow(JU77_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU77_CONTROL_P3p_SSSS_mod %*% JU77_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU77_CONTROL_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU77_CONTROL_P3p_SSSS_mod,
                      var.a = JU77_CONTROL_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_CONTROL_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_CONTROL_P3p_SSSS_mod <- data_JU77_CONTROL_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU77_CONTROL_P3p_SSSS_mod <- data_JU77_CONTROL_P3p_SSSS_mod[["mean.obs"]]
      va_data_JU77_CONTROL_P3p_SSSS_mod <- data_JU77_CONTROL_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_JU77_CONTROL_P3p_SSSS_mod <- data_JU77_CONTROL_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU77_CONTROL_P3p_SSSS_mod <- (va_data_JU77_CONTROL_P3p_SSSS_mod/2) / (trait_mean_data_JU77_CONTROL_P3p_SSSS_mod)^2
      
      mean(h2_data_JU77_CONTROL_P3p_SSSS_mod) 
      mean(trait_mean_data_JU77_CONTROL_P3p_SSSS_mod)
      mean(va_data_JU77_CONTROL_P3p_SSSS_mod) 
      mean(vp_data_JU77_CONTROL_P3p_SSSS_mod)
      mean(Evol_data_JU77_CONTROL_P3p_SSSS_mod)
      
    }
    
  }
  
  #JU77_MA_P3p_SSSS_mod 
  {
    
    JU77_MA_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_JU77_bi_MA,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)         
    
    saveRDS(JU77_MA_P3p_SSSS_mod, file = "JU77_MA_P3p_SSSS_mod.rds")
    JU77_MA_P3p_SSSS_mod <- readRDS("JU77_MA_P3p_SSSS_mod.rds")
    
    summary(JU77_MA_P3p_SSSS_mod) 
    #plot(JU77_MA_P3p_SSSS_mod)
    
    # traces and posterior densities
    pdf("JU77_MA_P3p_SSSS_mod.pdf")
    plot(JU77_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(JU77_MA_P3p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU77_MA_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU77_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU77_MA_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU77_MA_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU77_MA_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_MA_P3p_SSSS_mod <- JU77_MA_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU77_MA_P3p_SSSS_mod <- JU77_MA_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU77_MA_P3p_SSSS_mod <- rowSums(JU77_MA_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU77_MA_P3p_SSSS_mod) 
      HPDinterval(va_liab_JU77_MA_P3p_SSSS_mod) 
      
      mean(vlat_JU77_MA_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU77_MA_P3p_SSSS_mod <- JU77_MA_P3p_SSSS_mod[["X"]]
      beta_JU77_MA_P3p_SSSS_mod <- JU77_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_JU77_MA_P3p_SSSS_mod   <- apply(beta_JU77_MA_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_JU77_MA_P3p_SSSS_mod %*% b))}) 
      mean(vf_JU77_MA_P3p_SSSS_mod) 
      
      h2_liab_JU77_MA_P3p_SSSS_mod <- va_liab_JU77_MA_P3p_SSSS_mod / (vlat_JU77_MA_P3p_SSSS_mod + vf_JU77_MA_P3p_SSSS_mod)
      mean(h2_liab_JU77_MA_P3p_SSSS_mod) 
      posterior.mode(h2_liab_JU77_MA_P3p_SSSS_mod)	
      median(h2_liab_JU77_MA_P3p_SSSS_mod)		
      HPDinterval(h2_liab_JU77_MA_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU77_MA_P3p_SSSS_mod <- rowMeans(JU77_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU77_MA_P3p_SSSS_mod <- (va_liab_JU77_MA_P3p_SSSS_mod/2) / (trait_mean_liab_JU77_MA_P3p_SSSS_mod)^2
    mean(Evol_liab_JU77_MA_P3p_SSSS_mod)
    
    #JU77_MA_P3p_SSSS_mod data scale
    {
      
      predict_JU77_MA_P3p_SSSS_mod <- map(1:nrow(JU77_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU77_MA_P3p_SSSS_mod %*% JU77_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU77_MA_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU77_MA_P3p_SSSS_mod,
                      var.a = JU77_MA_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_MA_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_MA_P3p_SSSS_mod <- data_JU77_MA_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU77_MA_P3p_SSSS_mod <- data_JU77_MA_P3p_SSSS_mod[["mean.obs"]]
      va_data_JU77_MA_P3p_SSSS_mod <- data_JU77_MA_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_JU77_MA_P3p_SSSS_mod <- data_JU77_MA_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU77_MA_P3p_SSSS_mod <- (va_data_JU77_MA_P3p_SSSS_mod/2) / (trait_mean_data_JU77_MA_P3p_SSSS_mod)^2
      
      mean(h2_data_JU77_MA_P3p_SSSS_mod)
      mean(trait_mean_data_JU77_MA_P3p_SSSS_mod)
      mean(va_data_JU77_MA_P3p_SSSS_mod)
      mean(vp_data_JU77_MA_P3p_SSSS_mod)
      mean(Evol_data_JU77_MA_P3p_SSSS_mod)
      
    }
    
  }
  
  #JU77_Vm_P3p_SSSS_mod 
  {
    
    JU77_Vm_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer + Treatment -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_JU77_data,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)            
    
    saveRDS(JU77_Vm_P3p_SSSS_mod, file = "JU77_Vm_P3p_SSSS_mod.rds")
    JU77_Vm_P3p_SSSS_mod <- readRDS("JU77_Vm_P3p_SSSS_mod.rds")
    
    summary(JU77_Vm_P3p_SSSS_mod) 
    plotTrace(JU77_Vm_P3p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("JU77_Vm_P3p_SSSS_mod.pdf")
    plot(JU77_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(JU77_Vm_P3p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU77_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU77_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU77_Vm_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU77_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU77_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_Vm_P3p_SSSS_mod <- JU77_Vm_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU77_Vm_P3p_SSSS_mod <- JU77_Vm_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU77_Vm_P3p_SSSS_mod <- rowSums(JU77_Vm_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU77_Vm_P3p_SSSS_mod) 
      HPDinterval(va_liab_JU77_Vm_P3p_SSSS_mod) 
      
      mean(vlat_JU77_Vm_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU77_Vm_P3p_SSSS_mod <- JU77_Vm_P3p_SSSS_mod[["X"]]
      beta_JU77_Vm_P3p_SSSS_mod <- JU77_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_JU77_Vm_P3p_SSSS_mod   <- apply(beta_JU77_Vm_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_JU77_Vm_P3p_SSSS_mod %*% b))}) 
      mean(vf_JU77_Vm_P3p_SSSS_mod) 
      
      
      h2_liab_JU77_Vm_P3p_SSSS_mod <- va_liab_JU77_Vm_P3p_SSSS_mod / (vlat_JU77_Vm_P3p_SSSS_mod + vf_JU77_Vm_P3p_SSSS_mod)
      mean(h2_liab_JU77_Vm_P3p_SSSS_mod) 
      posterior.mode(h2_liab_JU77_Vm_P3p_SSSS_mod)	
      median(h2_liab_JU77_Vm_P3p_SSSS_mod)		
      HPDinterval(h2_liab_JU77_Vm_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU77_Vm_P3p_SSSS_mod <- ((rowMeans(JU77_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU77_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + JU77_Vm_P3p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU77_Vm_P3p_SSSS_mod <- (va_liab_JU77_Vm_P3p_SSSS_mod/2) / (trait_mean_liab_JU77_Vm_P3p_SSSS_mod)^2
    mean(Evol_liab_JU77_Vm_P3p_SSSS_mod)
    
    
    #JU77_Vm_P3p_SSSS_mod data scale
    {
      
      predict_JU77_Vm_P3p_SSSS_mod <- map(1:nrow(JU77_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU77_Vm_P3p_SSSS_mod %*% JU77_Vm_P3p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU77_Vm_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU77_Vm_P3p_SSSS_mod,
                      var.a = JU77_Vm_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_Vm_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_Vm_P3p_SSSS_mod <- data_JU77_Vm_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU77_Vm_P3p_SSSS_mod <- data_JU77_Vm_P3p_SSSS_mod[["mean.obs"]]
      va_data_JU77_Vm_P3p_SSSS_mod <- data_JU77_Vm_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_JU77_Vm_P3p_SSSS_mod <- data_JU77_Vm_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU77_Vm_P3p_SSSS_mod <- (va_data_JU77_Vm_P3p_SSSS_mod/2) / (trait_mean_data_JU77_Vm_P3p_SSSS_mod)^2
      
      mean(h2_data_JU77_Vm_P3p_SSSS_mod)
      mean(trait_mean_data_JU77_Vm_P3p_SSSS_mod)
      mean(va_data_JU77_Vm_P3p_SSSS_mod)
      mean(vp_data_JU77_Vm_P3p_SSSS_mod)
      mean(Evol_data_JU77_Vm_P3p_SSSS_mod)
      
    }
    
  }
  
  
  
  
  
  ##---- JU77 P4p ----
  
  
  #JU77_CONTROL_P4p_SSSS_mod 
  {
    
    JU77_CONTROL_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_JU77_bi_CONTROL,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)            
    
    saveRDS(JU77_CONTROL_P4p_SSSS_mod, file = "JU77_CONTROL_P4p_SSSS_mod.rds")
    JU77_CONTROL_P4p_SSSS_mod <- readRDS("JU77_CONTROL_P4p_SSSS_mod.rds")
    
    summary(JU77_CONTROL_P4p_SSSS_mod) 
    plotTrace(JU77_CONTROL_P4p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU77_CONTROL_P4p_SSSS_mod.pdf")
    plot(JU77_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(JU77_CONTROL_P4p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU77_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU77_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU77_CONTROL_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU77_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU77_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_CONTROL_P4p_SSSS_mod <- JU77_CONTROL_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU77_CONTROL_P4p_SSSS_mod <- JU77_CONTROL_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU77_CONTROL_P4p_SSSS_mod <- rowSums(JU77_CONTROL_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU77_CONTROL_P4p_SSSS_mod) 
      HPDinterval(va_liab_JU77_CONTROL_P4p_SSSS_mod) 
      
      mean(vlat_JU77_CONTROL_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU77_CONTROL_P4p_SSSS_mod <- JU77_CONTROL_P4p_SSSS_mod[["X"]]
      beta_JU77_CONTROL_P4p_SSSS_mod <- JU77_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_JU77_CONTROL_P4p_SSSS_mod   <- apply(beta_JU77_CONTROL_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_JU77_CONTROL_P4p_SSSS_mod %*% b))}) 
      mean(vf_JU77_CONTROL_P4p_SSSS_mod) 
      
      
      h2_liab_JU77_CONTROL_P4p_SSSS_mod <- va_liab_JU77_CONTROL_P4p_SSSS_mod / (vlat_JU77_CONTROL_P4p_SSSS_mod + vf_JU77_CONTROL_P4p_SSSS_mod)
      mean(h2_liab_JU77_CONTROL_P4p_SSSS_mod) 
      posterior.mode(h2_liab_JU77_CONTROL_P4p_SSSS_mod)	
      median(h2_liab_JU77_CONTROL_P4p_SSSS_mod)		
      HPDinterval(h2_liab_JU77_CONTROL_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU77_CONTROL_P4p_SSSS_mod <- rowMeans(JU77_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU77_CONTROL_P4p_SSSS_mod <- (va_liab_JU77_CONTROL_P4p_SSSS_mod/2) / (trait_mean_liab_JU77_CONTROL_P4p_SSSS_mod)^2
    mean(Evol_liab_JU77_CONTROL_P4p_SSSS_mod)
    
    
    #JU77_CONTROL_P4p_SSSS_mod data scale
    {
      
      predict_JU77_CONTROL_P4p_SSSS_mod <- map(1:nrow(JU77_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU77_CONTROL_P4p_SSSS_mod %*% JU77_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU77_CONTROL_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU77_CONTROL_P4p_SSSS_mod,
                      var.a = JU77_CONTROL_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_CONTROL_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_CONTROL_P4p_SSSS_mod <- data_JU77_CONTROL_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU77_CONTROL_P4p_SSSS_mod <- data_JU77_CONTROL_P4p_SSSS_mod[["mean.obs"]]
      va_data_JU77_CONTROL_P4p_SSSS_mod <- data_JU77_CONTROL_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_JU77_CONTROL_P4p_SSSS_mod <- data_JU77_CONTROL_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU77_CONTROL_P4p_SSSS_mod <- (va_data_JU77_CONTROL_P4p_SSSS_mod/2) / (trait_mean_data_JU77_CONTROL_P4p_SSSS_mod)^2
      
      mean(h2_data_JU77_CONTROL_P4p_SSSS_mod) 
      mean(trait_mean_data_JU77_CONTROL_P4p_SSSS_mod)
      mean(va_data_JU77_CONTROL_P4p_SSSS_mod) 
      mean(vp_data_JU77_CONTROL_P4p_SSSS_mod)
      mean(Evol_data_JU77_CONTROL_P4p_SSSS_mod)
      
    }
    
  }
  
  #JU77_MA_P4p_SSSS_mod 
  {
    
    JU77_MA_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_JU77_bi_MA,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)         
    
    saveRDS(JU77_MA_P4p_SSSS_mod, file = "JU77_MA_P4p_SSSS_mod.rds")
    JU77_MA_P4p_SSSS_mod <- readRDS("JU77_MA_P4p_SSSS_mod.rds")
    
    summary(JU77_MA_P4p_SSSS_mod) 
    #plot(JU77_MA_P4p_SSSS_mod)
    
    # traces and posterior densities
    pdf("JU77_MA_P4p_SSSS_mod.pdf")
    plot(JU77_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(JU77_MA_P4p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU77_MA_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU77_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU77_MA_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU77_MA_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU77_MA_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_MA_P4p_SSSS_mod <- JU77_MA_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU77_MA_P4p_SSSS_mod <- JU77_MA_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU77_MA_P4p_SSSS_mod <- rowSums(JU77_MA_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU77_MA_P4p_SSSS_mod) 
      HPDinterval(va_liab_JU77_MA_P4p_SSSS_mod) 
      
      mean(vlat_JU77_MA_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU77_MA_P4p_SSSS_mod <- JU77_MA_P4p_SSSS_mod[["X"]]
      beta_JU77_MA_P4p_SSSS_mod <- JU77_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_JU77_MA_P4p_SSSS_mod   <- apply(beta_JU77_MA_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_JU77_MA_P4p_SSSS_mod %*% b))}) 
      mean(vf_JU77_MA_P4p_SSSS_mod) 
      
      h2_liab_JU77_MA_P4p_SSSS_mod <- va_liab_JU77_MA_P4p_SSSS_mod / (vlat_JU77_MA_P4p_SSSS_mod + vf_JU77_MA_P4p_SSSS_mod)
      mean(h2_liab_JU77_MA_P4p_SSSS_mod) 
      posterior.mode(h2_liab_JU77_MA_P4p_SSSS_mod)	
      median(h2_liab_JU77_MA_P4p_SSSS_mod)		
      HPDinterval(h2_liab_JU77_MA_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU77_MA_P4p_SSSS_mod <- rowMeans(JU77_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU77_MA_P4p_SSSS_mod <- (va_liab_JU77_MA_P4p_SSSS_mod/2) / (trait_mean_liab_JU77_MA_P4p_SSSS_mod)^2
    mean(Evol_liab_JU77_MA_P4p_SSSS_mod)
    
    #JU77_MA_P4p_SSSS_mod data scale
    {
      
      predict_JU77_MA_P4p_SSSS_mod <- map(1:nrow(JU77_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU77_MA_P4p_SSSS_mod %*% JU77_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU77_MA_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU77_MA_P4p_SSSS_mod,
                      var.a = JU77_MA_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_MA_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_MA_P4p_SSSS_mod <- data_JU77_MA_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU77_MA_P4p_SSSS_mod <- data_JU77_MA_P4p_SSSS_mod[["mean.obs"]]
      va_data_JU77_MA_P4p_SSSS_mod <- data_JU77_MA_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_JU77_MA_P4p_SSSS_mod <- data_JU77_MA_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU77_MA_P4p_SSSS_mod <- (va_data_JU77_MA_P4p_SSSS_mod/2) / (trait_mean_data_JU77_MA_P4p_SSSS_mod)^2
      
      mean(h2_data_JU77_MA_P4p_SSSS_mod)
      mean(trait_mean_data_JU77_MA_P4p_SSSS_mod)
      mean(va_data_JU77_MA_P4p_SSSS_mod)
      mean(vp_data_JU77_MA_P4p_SSSS_mod)
      mean(Evol_data_JU77_MA_P4p_SSSS_mod)
      
    }
    
  }
  
  #JU77_Vm_P4p_SSSS_mod 
  {
    
    JU77_Vm_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer + Treatment -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_JU77_data,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)            
    
    saveRDS(JU77_Vm_P4p_SSSS_mod, file = "JU77_Vm_P4p_SSSS_mod.rds")
    JU77_Vm_P4p_SSSS_mod <- readRDS("JU77_Vm_P4p_SSSS_mod.rds")
    
    summary(JU77_Vm_P4p_SSSS_mod) 
    plotTrace(JU77_Vm_P4p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("JU77_Vm_P4p_SSSS_mod.pdf")
    plot(JU77_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(JU77_Vm_P4p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU77_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU77_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU77_Vm_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU77_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU77_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_Vm_P4p_SSSS_mod <- JU77_Vm_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU77_Vm_P4p_SSSS_mod <- JU77_Vm_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU77_Vm_P4p_SSSS_mod <- rowSums(JU77_Vm_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU77_Vm_P4p_SSSS_mod) 
      HPDinterval(va_liab_JU77_Vm_P4p_SSSS_mod) 
      
      mean(vlat_JU77_Vm_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU77_Vm_P4p_SSSS_mod <- JU77_Vm_P4p_SSSS_mod[["X"]]
      beta_JU77_Vm_P4p_SSSS_mod <- JU77_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_JU77_Vm_P4p_SSSS_mod   <- apply(beta_JU77_Vm_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_JU77_Vm_P4p_SSSS_mod %*% b))}) 
      mean(vf_JU77_Vm_P4p_SSSS_mod) 
      
      
      h2_liab_JU77_Vm_P4p_SSSS_mod <- va_liab_JU77_Vm_P4p_SSSS_mod / (vlat_JU77_Vm_P4p_SSSS_mod + vf_JU77_Vm_P4p_SSSS_mod)
      mean(h2_liab_JU77_Vm_P4p_SSSS_mod) 
      posterior.mode(h2_liab_JU77_Vm_P4p_SSSS_mod)	
      median(h2_liab_JU77_Vm_P4p_SSSS_mod)		
      HPDinterval(h2_liab_JU77_Vm_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU77_Vm_P4p_SSSS_mod <- ((rowMeans(JU77_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU77_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + JU77_Vm_P4p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU77_Vm_P4p_SSSS_mod <- (va_liab_JU77_Vm_P4p_SSSS_mod/2) / (trait_mean_liab_JU77_Vm_P4p_SSSS_mod)^2
    mean(Evol_liab_JU77_Vm_P4p_SSSS_mod)
    
    
    #JU77_Vm_P4p_SSSS_mod data scale
    {
      
      predict_JU77_Vm_P4p_SSSS_mod <- map(1:nrow(JU77_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU77_Vm_P4p_SSSS_mod %*% JU77_Vm_P4p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU77_Vm_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU77_Vm_P4p_SSSS_mod,
                      var.a = JU77_Vm_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_Vm_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_Vm_P4p_SSSS_mod <- data_JU77_Vm_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU77_Vm_P4p_SSSS_mod <- data_JU77_Vm_P4p_SSSS_mod[["mean.obs"]]
      va_data_JU77_Vm_P4p_SSSS_mod <- data_JU77_Vm_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_JU77_Vm_P4p_SSSS_mod <- data_JU77_Vm_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU77_Vm_P4p_SSSS_mod <- (va_data_JU77_Vm_P4p_SSSS_mod/2) / (trait_mean_data_JU77_Vm_P4p_SSSS_mod)^2
      
      mean(h2_data_JU77_Vm_P4p_SSSS_mod)
      mean(trait_mean_data_JU77_Vm_P4p_SSSS_mod)
      mean(va_data_JU77_Vm_P4p_SSSS_mod)
      mean(vp_data_JU77_Vm_P4p_SSSS_mod)
      mean(Evol_data_JU77_Vm_P4p_SSSS_mod)
      
    }
    
  }
  
  
 
  
  
  ##---- JU77 P8p ----
  
  
  #JU77_CONTROL_P8p_SSSS_mod 
  {
    
    JU77_CONTROL_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_JU77_bi_CONTROL,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)            
    
    saveRDS(JU77_CONTROL_P8p_SSSS_mod, file = "JU77_CONTROL_P8p_SSSS_mod.rds")
    JU77_CONTROL_P8p_SSSS_mod <- readRDS("JU77_CONTROL_P8p_SSSS_mod.rds")
    
    summary(JU77_CONTROL_P8p_SSSS_mod) 
    plotTrace(JU77_CONTROL_P8p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU77_CONTROL_P8p_SSSS_mod.pdf")
    plot(JU77_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(JU77_CONTROL_P8p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU77_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU77_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU77_CONTROL_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU77_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU77_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_CONTROL_P8p_SSSS_mod <- JU77_CONTROL_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU77_CONTROL_P8p_SSSS_mod <- JU77_CONTROL_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU77_CONTROL_P8p_SSSS_mod <- rowSums(JU77_CONTROL_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU77_CONTROL_P8p_SSSS_mod) 
      HPDinterval(va_liab_JU77_CONTROL_P8p_SSSS_mod) 
      
      mean(vlat_JU77_CONTROL_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU77_CONTROL_P8p_SSSS_mod <- JU77_CONTROL_P8p_SSSS_mod[["X"]]
      beta_JU77_CONTROL_P8p_SSSS_mod <- JU77_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_JU77_CONTROL_P8p_SSSS_mod   <- apply(beta_JU77_CONTROL_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_JU77_CONTROL_P8p_SSSS_mod %*% b))}) 
      mean(vf_JU77_CONTROL_P8p_SSSS_mod) 
      
      
      h2_liab_JU77_CONTROL_P8p_SSSS_mod <- va_liab_JU77_CONTROL_P8p_SSSS_mod / (vlat_JU77_CONTROL_P8p_SSSS_mod + vf_JU77_CONTROL_P8p_SSSS_mod)
      mean(h2_liab_JU77_CONTROL_P8p_SSSS_mod) 
      posterior.mode(h2_liab_JU77_CONTROL_P8p_SSSS_mod)	
      median(h2_liab_JU77_CONTROL_P8p_SSSS_mod)		
      HPDinterval(h2_liab_JU77_CONTROL_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU77_CONTROL_P8p_SSSS_mod <- rowMeans(JU77_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU77_CONTROL_P8p_SSSS_mod <- (va_liab_JU77_CONTROL_P8p_SSSS_mod/2) / (trait_mean_liab_JU77_CONTROL_P8p_SSSS_mod)^2
    mean(Evol_liab_JU77_CONTROL_P8p_SSSS_mod)
    
    
    #JU77_CONTROL_P8p_SSSS_mod data scale
    {
      
      predict_JU77_CONTROL_P8p_SSSS_mod <- map(1:nrow(JU77_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU77_CONTROL_P8p_SSSS_mod %*% JU77_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU77_CONTROL_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU77_CONTROL_P8p_SSSS_mod,
                      var.a = JU77_CONTROL_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_CONTROL_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_CONTROL_P8p_SSSS_mod <- data_JU77_CONTROL_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU77_CONTROL_P8p_SSSS_mod <- data_JU77_CONTROL_P8p_SSSS_mod[["mean.obs"]]
      va_data_JU77_CONTROL_P8p_SSSS_mod <- data_JU77_CONTROL_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_JU77_CONTROL_P8p_SSSS_mod <- data_JU77_CONTROL_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU77_CONTROL_P8p_SSSS_mod <- (va_data_JU77_CONTROL_P8p_SSSS_mod/2) / (trait_mean_data_JU77_CONTROL_P8p_SSSS_mod)^2
      
      mean(h2_data_JU77_CONTROL_P8p_SSSS_mod) 
      mean(trait_mean_data_JU77_CONTROL_P8p_SSSS_mod)
      mean(va_data_JU77_CONTROL_P8p_SSSS_mod) 
      mean(vp_data_JU77_CONTROL_P8p_SSSS_mod)
      mean(Evol_data_JU77_CONTROL_P8p_SSSS_mod)
      
    }
    
  }
  
  #JU77_MA_P8p_SSSS_mod 
  {
    
    JU77_MA_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_JU77_bi_MA,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)         
    
    saveRDS(JU77_MA_P8p_SSSS_mod, file = "JU77_MA_P8p_SSSS_mod.rds")
    JU77_MA_P8p_SSSS_mod <- readRDS("JU77_MA_P8p_SSSS_mod.rds")
    
    summary(JU77_MA_P8p_SSSS_mod) 
    #plot(JU77_MA_P8p_SSSS_mod)
    
    # traces and posterior densities
    pdf("JU77_MA_P8p_SSSS_mod.pdf")
    plot(JU77_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) 
    plot(JU77_MA_P8p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU77_MA_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU77_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU77_MA_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU77_MA_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU77_MA_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_MA_P8p_SSSS_mod <- JU77_MA_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU77_MA_P8p_SSSS_mod <- JU77_MA_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU77_MA_P8p_SSSS_mod <- rowSums(JU77_MA_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU77_MA_P8p_SSSS_mod) 
      HPDinterval(va_liab_JU77_MA_P8p_SSSS_mod) 
      
      mean(vlat_JU77_MA_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU77_MA_P8p_SSSS_mod <- JU77_MA_P8p_SSSS_mod[["X"]]
      beta_JU77_MA_P8p_SSSS_mod <- JU77_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]
      vf_JU77_MA_P8p_SSSS_mod   <- apply(beta_JU77_MA_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_JU77_MA_P8p_SSSS_mod %*% b))}) 
      mean(vf_JU77_MA_P8p_SSSS_mod) 
      
      h2_liab_JU77_MA_P8p_SSSS_mod <- va_liab_JU77_MA_P8p_SSSS_mod / (vlat_JU77_MA_P8p_SSSS_mod + vf_JU77_MA_P8p_SSSS_mod)
      mean(h2_liab_JU77_MA_P8p_SSSS_mod) 
      posterior.mode(h2_liab_JU77_MA_P8p_SSSS_mod)	
      median(h2_liab_JU77_MA_P8p_SSSS_mod)		
      HPDinterval(h2_liab_JU77_MA_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU77_MA_P8p_SSSS_mod <- rowMeans(JU77_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU77_MA_P8p_SSSS_mod <- (va_liab_JU77_MA_P8p_SSSS_mod/2) / (trait_mean_liab_JU77_MA_P8p_SSSS_mod)^2
    mean(Evol_liab_JU77_MA_P8p_SSSS_mod)
    
    #JU77_MA_P8p_SSSS_mod data scale
    {
      
      predict_JU77_MA_P8p_SSSS_mod <- map(1:nrow(JU77_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU77_MA_P8p_SSSS_mod %*% JU77_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU77_MA_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU77_MA_P8p_SSSS_mod,
                      var.a = JU77_MA_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_MA_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_MA_P8p_SSSS_mod <- data_JU77_MA_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU77_MA_P8p_SSSS_mod <- data_JU77_MA_P8p_SSSS_mod[["mean.obs"]]
      va_data_JU77_MA_P8p_SSSS_mod <- data_JU77_MA_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_JU77_MA_P8p_SSSS_mod <- data_JU77_MA_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU77_MA_P8p_SSSS_mod <- (va_data_JU77_MA_P8p_SSSS_mod/2) / (trait_mean_data_JU77_MA_P8p_SSSS_mod)^2
      
      mean(h2_data_JU77_MA_P8p_SSSS_mod)
      mean(trait_mean_data_JU77_MA_P8p_SSSS_mod)
      mean(va_data_JU77_MA_P8p_SSSS_mod)
      mean(vp_data_JU77_MA_P8p_SSSS_mod)
      mean(Evol_data_JU77_MA_P8p_SSSS_mod)
      
    }
    
  }
  
  #JU77_Vm_P8p_SSSS_mod 
  {
    
    JU77_Vm_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer + Treatment -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_JU77_data,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)            
    
    saveRDS(JU77_Vm_P8p_SSSS_mod, file = "JU77_Vm_P8p_SSSS_mod.rds")
    JU77_Vm_P8p_SSSS_mod <- readRDS("JU77_Vm_P8p_SSSS_mod.rds")
    
    summary(JU77_Vm_P8p_SSSS_mod) 
    plotTrace(JU77_Vm_P8p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("JU77_Vm_P8p_SSSS_mod.pdf")
    plot(JU77_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(JU77_Vm_P8p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU77_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU77_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU77_Vm_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(JU77_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU77_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_Vm_P8p_SSSS_mod <- JU77_Vm_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_JU77_Vm_P8p_SSSS_mod <- JU77_Vm_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_JU77_Vm_P8p_SSSS_mod <- rowSums(JU77_Vm_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_JU77_Vm_P8p_SSSS_mod) 
      HPDinterval(va_liab_JU77_Vm_P8p_SSSS_mod) 
      
      mean(vlat_JU77_Vm_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_JU77_Vm_P8p_SSSS_mod <- JU77_Vm_P8p_SSSS_mod[["X"]]
      beta_JU77_Vm_P8p_SSSS_mod <- JU77_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_JU77_Vm_P8p_SSSS_mod   <- apply(beta_JU77_Vm_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_JU77_Vm_P8p_SSSS_mod %*% b))}) 
      mean(vf_JU77_Vm_P8p_SSSS_mod) 
      
      
      h2_liab_JU77_Vm_P8p_SSSS_mod <- va_liab_JU77_Vm_P8p_SSSS_mod / (vlat_JU77_Vm_P8p_SSSS_mod + vf_JU77_Vm_P8p_SSSS_mod)
      mean(h2_liab_JU77_Vm_P8p_SSSS_mod) 
      posterior.mode(h2_liab_JU77_Vm_P8p_SSSS_mod)	
      median(h2_liab_JU77_Vm_P8p_SSSS_mod)		
      HPDinterval(h2_liab_JU77_Vm_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU77_Vm_P8p_SSSS_mod <- ((rowMeans(JU77_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU77_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + JU77_Vm_P8p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU77_Vm_P8p_SSSS_mod <- (va_liab_JU77_Vm_P8p_SSSS_mod/2) / (trait_mean_liab_JU77_Vm_P8p_SSSS_mod)^2
    mean(Evol_liab_JU77_Vm_P8p_SSSS_mod)
    
    
    #JU77_Vm_P8p_SSSS_mod data scale
    {
      
      predict_JU77_Vm_P8p_SSSS_mod <- map(1:nrow(JU77_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU77_Vm_P8p_SSSS_mod %*% JU77_Vm_P8p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU77_Vm_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_JU77_Vm_P8p_SSSS_mod,
                      var.a = JU77_Vm_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_Vm_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_Vm_P8p_SSSS_mod <- data_JU77_Vm_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_JU77_Vm_P8p_SSSS_mod <- data_JU77_Vm_P8p_SSSS_mod[["mean.obs"]]
      va_data_JU77_Vm_P8p_SSSS_mod <- data_JU77_Vm_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_JU77_Vm_P8p_SSSS_mod <- data_JU77_Vm_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_JU77_Vm_P8p_SSSS_mod <- (va_data_JU77_Vm_P8p_SSSS_mod/2) / (trait_mean_data_JU77_Vm_P8p_SSSS_mod)^2
      
      mean(h2_data_JU77_Vm_P8p_SSSS_mod)
      mean(trait_mean_data_JU77_Vm_P8p_SSSS_mod)
      mean(va_data_JU77_Vm_P8p_SSSS_mod)
      mean(vp_data_JU77_Vm_P8p_SSSS_mod)
      mean(Evol_data_JU77_Vm_P8p_SSSS_mod)
      
    }
    
  }
  
  
   ## JU77 P5p ----
  
  
  #JU77_CONTROL_P5p_wt_mod 
  {
    
    JU77_CONTROL_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_JU77_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(JU77_CONTROL_P5p_wt_mod, file = "JU77_CONTROL_P5p_wt_mod.rds")
    JU77_CONTROL_P5p_wt_mod <- readRDS("JU77_CONTROL_P5p_wt_mod.rds")
    
    summary(JU77_CONTROL_P5p_wt_mod) 
    plotTrace(JU77_CONTROL_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU77_CONTROL_P5p_wt_mod.pdf")
    plot(JU77_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU77_CONTROL_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU77_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU77_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU77_CONTROL_P5p_wt_mod[["VCV"]])
    #autocorr.plot(JU77_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU77_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_CONTROL_P5p_wt_mod <- JU77_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU77_CONTROL_P5p_wt_mod <- JU77_CONTROL_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_JU77_CONTROL_P5p_wt_mod <- rowSums(JU77_CONTROL_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_JU77_CONTROL_P5p_wt_mod) 
      HPDinterval(va_liab_JU77_CONTROL_P5p_wt_mod) 
      
      mean(vlat_JU77_CONTROL_P5p_wt_mod) 
      
      #variance of fixed effects
      X_JU77_CONTROL_P5p_wt_mod <- JU77_CONTROL_P5p_wt_mod[["X"]]
      beta_JU77_CONTROL_P5p_wt_mod <- JU77_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU77_CONTROL_P5p_wt_mod   <- apply(beta_JU77_CONTROL_P5p_wt_mod, 1, function(b) {var(as.vector(X_JU77_CONTROL_P5p_wt_mod %*% b))}) 
      mean(vf_JU77_CONTROL_P5p_wt_mod) 
      
      
      h2_liab_JU77_CONTROL_P5p_wt_mod <- va_liab_JU77_CONTROL_P5p_wt_mod / (vlat_JU77_CONTROL_P5p_wt_mod + vf_JU77_CONTROL_P5p_wt_mod)
      mean(h2_liab_JU77_CONTROL_P5p_wt_mod) 
      posterior.mode(h2_liab_JU77_CONTROL_P5p_wt_mod)	
      median(h2_liab_JU77_CONTROL_P5p_wt_mod)		
      HPDinterval(h2_liab_JU77_CONTROL_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU77_CONTROL_P5p_wt_mod <- rowMeans(JU77_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU77_CONTROL_P5p_wt_mod <- (va_liab_JU77_CONTROL_P5p_wt_mod/2) / (trait_mean_liab_JU77_CONTROL_P5p_wt_mod)^2
    mean(Evol_liab_JU77_CONTROL_P5p_wt_mod)
    
    
    #JU77_CONTROL_P5p_wt_mod data scale
    {
      
      predict_JU77_CONTROL_P5p_wt_mod <- map(1:nrow(JU77_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU77_CONTROL_P5p_wt_mod %*% JU77_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU77_CONTROL_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_JU77_CONTROL_P5p_wt_mod,
                      var.a = JU77_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_CONTROL_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_CONTROL_P5p_wt_mod <- data_JU77_CONTROL_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_JU77_CONTROL_P5p_wt_mod <- data_JU77_CONTROL_P5p_wt_mod[["mean.obs"]]
      va_data_JU77_CONTROL_P5p_wt_mod <- data_JU77_CONTROL_P5p_wt_mod[["var.a.obs"]]
      vp_data_JU77_CONTROL_P5p_wt_mod <- data_JU77_CONTROL_P5p_wt_mod[["var.obs"]]
      
      Evol_data_JU77_CONTROL_P5p_wt_mod <- (va_data_JU77_CONTROL_P5p_wt_mod/2) / (trait_mean_data_JU77_CONTROL_P5p_wt_mod)^2
      
      mean(h2_data_JU77_CONTROL_P5p_wt_mod) 
      mean(trait_mean_data_JU77_CONTROL_P5p_wt_mod)
      mean(va_data_JU77_CONTROL_P5p_wt_mod) 
      mean(vp_data_JU77_CONTROL_P5p_wt_mod)
      mean(Evol_data_JU77_CONTROL_P5p_wt_mod)
      
    }
    
  }
  
  #JU77_MA_P5p_wt_mod 
  {
    
    JU77_MA_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_JU77_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(JU77_MA_P5p_wt_mod, file = "JU77_MA_P5p_wt_mod.rds")
    JU77_MA_P5p_wt_mod <- readRDS("JU77_MA_P5p_wt_mod.rds")
    
    summary(JU77_MA_P5p_wt_mod) 
    #plot(JU77_MA_P5p_wt_mod)
    
    # traces and posterior densities
    pdf("JU77_MA_P5p_wt_mod.pdf")
    plot(JU77_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU77_MA_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU77_MA_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_MA_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU77_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU77_MA_P5p_wt_mod[["VCV"]])
    #autocorr.plot(JU77_MA_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU77_MA_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_MA_P5p_wt_mod <- JU77_MA_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU77_MA_P5p_wt_mod <- JU77_MA_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_JU77_MA_P5p_wt_mod <- rowSums(JU77_MA_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_JU77_MA_P5p_wt_mod) 
      HPDinterval(va_liab_JU77_MA_P5p_wt_mod) 
      
      mean(vlat_JU77_MA_P5p_wt_mod) 
      
      #variance of fixed effects
      X_JU77_MA_P5p_wt_mod <- JU77_MA_P5p_wt_mod[["X"]]
      beta_JU77_MA_P5p_wt_mod <- JU77_MA_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU77_MA_P5p_wt_mod   <- apply(beta_JU77_MA_P5p_wt_mod, 1, function(b) {var(as.vector(X_JU77_MA_P5p_wt_mod %*% b))}) 
      mean(vf_JU77_MA_P5p_wt_mod) 
      
      h2_liab_JU77_MA_P5p_wt_mod <- va_liab_JU77_MA_P5p_wt_mod / (vlat_JU77_MA_P5p_wt_mod + vf_JU77_MA_P5p_wt_mod)
      mean(h2_liab_JU77_MA_P5p_wt_mod) 
      posterior.mode(h2_liab_JU77_MA_P5p_wt_mod)	
      median(h2_liab_JU77_MA_P5p_wt_mod)		
      HPDinterval(h2_liab_JU77_MA_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU77_MA_P5p_wt_mod <- rowMeans(JU77_MA_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU77_MA_P5p_wt_mod <- (va_liab_JU77_MA_P5p_wt_mod/2) / (trait_mean_liab_JU77_MA_P5p_wt_mod)^2
    mean(Evol_liab_JU77_MA_P5p_wt_mod)
    
    #JU77_MA_P5p_wt_mod data scale
    {
      
      predict_JU77_MA_P5p_wt_mod <- map(1:nrow(JU77_MA_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU77_MA_P5p_wt_mod %*% JU77_MA_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU77_MA_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_JU77_MA_P5p_wt_mod,
                      var.a = JU77_MA_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_MA_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_MA_P5p_wt_mod <- data_JU77_MA_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_JU77_MA_P5p_wt_mod <- data_JU77_MA_P5p_wt_mod[["mean.obs"]]
      va_data_JU77_MA_P5p_wt_mod <- data_JU77_MA_P5p_wt_mod[["var.a.obs"]]
      vp_data_JU77_MA_P5p_wt_mod <- data_JU77_MA_P5p_wt_mod[["var.obs"]]
      
      Evol_data_JU77_MA_P5p_wt_mod <- (va_data_JU77_MA_P5p_wt_mod/2) / (trait_mean_data_JU77_MA_P5p_wt_mod)^2
      
      mean(h2_data_JU77_MA_P5p_wt_mod)
      mean(trait_mean_data_JU77_MA_P5p_wt_mod)
      mean(va_data_JU77_MA_P5p_wt_mod)
      mean(vp_data_JU77_MA_P5p_wt_mod)
      mean(Evol_data_JU77_MA_P5p_wt_mod)
      
    }
    
  }
  
  #JU77_Vm_P5p_wt_mod 
  {
    
    JU77_Vm_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_JU77_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(JU77_Vm_P5p_wt_mod, file = "JU77_Vm_P5p_wt_mod.rds")
    JU77_Vm_P5p_wt_mod <- readRDS("JU77_Vm_P5p_wt_mod.rds")
    
    summary(JU77_Vm_P5p_wt_mod) 
    plotTrace(JU77_Vm_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("JU77_Vm_P5p_wt_mod.pdf")
    plot(JU77_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(JU77_Vm_P5p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU77_Vm_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_Vm_P5p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU77_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU77_Vm_P5p_wt_mod[["VCV"]])
    #autocorr.plot(JU77_Vm_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU77_Vm_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_Vm_P5p_wt_mod <- JU77_Vm_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU77_Vm_P5p_wt_mod <- JU77_Vm_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_JU77_Vm_P5p_wt_mod <- rowSums(JU77_Vm_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_JU77_Vm_P5p_wt_mod) 
      HPDinterval(va_liab_JU77_Vm_P5p_wt_mod) 
      
      mean(vlat_JU77_Vm_P5p_wt_mod) 
      
      #variance of fixed effects
      X_JU77_Vm_P5p_wt_mod <- JU77_Vm_P5p_wt_mod[["X"]]
      beta_JU77_Vm_P5p_wt_mod <- JU77_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]
      vf_JU77_Vm_P5p_wt_mod   <- apply(beta_JU77_Vm_P5p_wt_mod, 1, function(b) {var(as.vector(X_JU77_Vm_P5p_wt_mod %*% b))}) 
      mean(vf_JU77_Vm_P5p_wt_mod) 
      
      
      h2_liab_JU77_Vm_P5p_wt_mod <- va_liab_JU77_Vm_P5p_wt_mod / (vlat_JU77_Vm_P5p_wt_mod + vf_JU77_Vm_P5p_wt_mod)
      mean(h2_liab_JU77_Vm_P5p_wt_mod) 
      posterior.mode(h2_liab_JU77_Vm_P5p_wt_mod)	
      median(h2_liab_JU77_Vm_P5p_wt_mod)		
      HPDinterval(h2_liab_JU77_Vm_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU77_Vm_P5p_wt_mod <- ((rowMeans(JU77_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU77_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + JU77_Vm_P5p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU77_Vm_P5p_wt_mod <- (va_liab_JU77_Vm_P5p_wt_mod/2) / (trait_mean_liab_JU77_Vm_P5p_wt_mod)^2
    mean(Evol_liab_JU77_Vm_P5p_wt_mod)
    
    
    #JU77_Vm_P5p_wt_mod data scale
    {
      
      predict_JU77_Vm_P5p_wt_mod <- map(1:nrow(JU77_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU77_Vm_P5p_wt_mod %*% JU77_Vm_P5p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU77_Vm_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_JU77_Vm_P5p_wt_mod,
                      var.a = JU77_Vm_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_Vm_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_Vm_P5p_wt_mod <- data_JU77_Vm_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_JU77_Vm_P5p_wt_mod <- data_JU77_Vm_P5p_wt_mod[["mean.obs"]]
      va_data_JU77_Vm_P5p_wt_mod <- data_JU77_Vm_P5p_wt_mod[["var.a.obs"]]
      vp_data_JU77_Vm_P5p_wt_mod <- data_JU77_Vm_P5p_wt_mod[["var.obs"]]
      
      Evol_data_JU77_Vm_P5p_wt_mod <- (va_data_JU77_Vm_P5p_wt_mod/2) / (trait_mean_data_JU77_Vm_P5p_wt_mod)^2
      
      mean(h2_data_JU77_Vm_P5p_wt_mod)
      mean(trait_mean_data_JU77_Vm_P5p_wt_mod)
      mean(va_data_JU77_Vm_P5p_wt_mod)
      mean(vp_data_JU77_Vm_P5p_wt_mod)
      mean(Evol_data_JU77_Vm_P5p_wt_mod)
      
    }
    
  }
  
  
  
  
  
  ## JU77 P6p ----
  
  
  #JU77_CONTROL_P6p_wt_mod 
  {
    
    JU77_CONTROL_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_JU77_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(JU77_CONTROL_P6p_wt_mod, file = "JU77_CONTROL_P6p_wt_mod.rds")
    JU77_CONTROL_P6p_wt_mod <- readRDS("JU77_CONTROL_P6p_wt_mod.rds")
    
    summary(JU77_CONTROL_P6p_wt_mod) 
    plotTrace(JU77_CONTROL_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU77_CONTROL_P6p_wt_mod.pdf")
    plot(JU77_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU77_CONTROL_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU77_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU77_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU77_CONTROL_P6p_wt_mod[["VCV"]])
    #autocorr.plot(JU77_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU77_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_CONTROL_P6p_wt_mod <- JU77_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU77_CONTROL_P6p_wt_mod <- JU77_CONTROL_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_JU77_CONTROL_P6p_wt_mod <- rowSums(JU77_CONTROL_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_JU77_CONTROL_P6p_wt_mod) 
      HPDinterval(va_liab_JU77_CONTROL_P6p_wt_mod) 
      
      mean(vlat_JU77_CONTROL_P6p_wt_mod) 
      
      #variance of fixed effects
      X_JU77_CONTROL_P6p_wt_mod <- JU77_CONTROL_P6p_wt_mod[["X"]]
      beta_JU77_CONTROL_P6p_wt_mod <- JU77_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU77_CONTROL_P6p_wt_mod   <- apply(beta_JU77_CONTROL_P6p_wt_mod, 1, function(b) {var(as.vector(X_JU77_CONTROL_P6p_wt_mod %*% b))}) 
      mean(vf_JU77_CONTROL_P6p_wt_mod) 
      
      
      h2_liab_JU77_CONTROL_P6p_wt_mod <- va_liab_JU77_CONTROL_P6p_wt_mod / (vlat_JU77_CONTROL_P6p_wt_mod + vf_JU77_CONTROL_P6p_wt_mod)
      mean(h2_liab_JU77_CONTROL_P6p_wt_mod) 
      posterior.mode(h2_liab_JU77_CONTROL_P6p_wt_mod)	
      median(h2_liab_JU77_CONTROL_P6p_wt_mod)		
      HPDinterval(h2_liab_JU77_CONTROL_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU77_CONTROL_P6p_wt_mod <- rowMeans(JU77_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU77_CONTROL_P6p_wt_mod <- (va_liab_JU77_CONTROL_P6p_wt_mod/2) / (trait_mean_liab_JU77_CONTROL_P6p_wt_mod)^2
    mean(Evol_liab_JU77_CONTROL_P6p_wt_mod)
    
    
    #JU77_CONTROL_P6p_wt_mod data scale
    {
      
      predict_JU77_CONTROL_P6p_wt_mod <- map(1:nrow(JU77_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU77_CONTROL_P6p_wt_mod %*% JU77_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU77_CONTROL_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_JU77_CONTROL_P6p_wt_mod,
                      var.a = JU77_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_CONTROL_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_CONTROL_P6p_wt_mod <- data_JU77_CONTROL_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_JU77_CONTROL_P6p_wt_mod <- data_JU77_CONTROL_P6p_wt_mod[["mean.obs"]]
      va_data_JU77_CONTROL_P6p_wt_mod <- data_JU77_CONTROL_P6p_wt_mod[["var.a.obs"]]
      vp_data_JU77_CONTROL_P6p_wt_mod <- data_JU77_CONTROL_P6p_wt_mod[["var.obs"]]
      
      Evol_data_JU77_CONTROL_P6p_wt_mod <- (va_data_JU77_CONTROL_P6p_wt_mod/2) / (trait_mean_data_JU77_CONTROL_P6p_wt_mod)^2
      
      mean(h2_data_JU77_CONTROL_P6p_wt_mod) 
      mean(trait_mean_data_JU77_CONTROL_P6p_wt_mod)
      mean(va_data_JU77_CONTROL_P6p_wt_mod) 
      mean(vp_data_JU77_CONTROL_P6p_wt_mod)
      mean(Evol_data_JU77_CONTROL_P6p_wt_mod)
      
    }
    
  }
  
  #JU77_MA_P6p_wt_mod 
  {
    
    JU77_MA_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_JU77_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(JU77_MA_P6p_wt_mod, file = "JU77_MA_P6p_wt_mod.rds")
    JU77_MA_P6p_wt_mod <- readRDS("JU77_MA_P6p_wt_mod.rds")
    
    summary(JU77_MA_P6p_wt_mod) 
    #plot(JU77_MA_P6p_wt_mod)
    
    # traces and posterior densities
    pdf("JU77_MA_P6p_wt_mod.pdf")
    plot(JU77_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU77_MA_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU77_MA_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_MA_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU77_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU77_MA_P6p_wt_mod[["VCV"]])
    #autocorr.plot(JU77_MA_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU77_MA_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_MA_P6p_wt_mod <- JU77_MA_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU77_MA_P6p_wt_mod <- JU77_MA_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_JU77_MA_P6p_wt_mod <- rowSums(JU77_MA_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_JU77_MA_P6p_wt_mod) 
      HPDinterval(va_liab_JU77_MA_P6p_wt_mod) 
      
      mean(vlat_JU77_MA_P6p_wt_mod) 
      
      #variance of fixed effects
      X_JU77_MA_P6p_wt_mod <- JU77_MA_P6p_wt_mod[["X"]]
      beta_JU77_MA_P6p_wt_mod <- JU77_MA_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU77_MA_P6p_wt_mod   <- apply(beta_JU77_MA_P6p_wt_mod, 1, function(b) {var(as.vector(X_JU77_MA_P6p_wt_mod %*% b))}) 
      mean(vf_JU77_MA_P6p_wt_mod) 
      
      h2_liab_JU77_MA_P6p_wt_mod <- va_liab_JU77_MA_P6p_wt_mod / (vlat_JU77_MA_P6p_wt_mod + vf_JU77_MA_P6p_wt_mod)
      mean(h2_liab_JU77_MA_P6p_wt_mod) 
      posterior.mode(h2_liab_JU77_MA_P6p_wt_mod)	
      median(h2_liab_JU77_MA_P6p_wt_mod)		
      HPDinterval(h2_liab_JU77_MA_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU77_MA_P6p_wt_mod <- rowMeans(JU77_MA_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU77_MA_P6p_wt_mod <- (va_liab_JU77_MA_P6p_wt_mod/2) / (trait_mean_liab_JU77_MA_P6p_wt_mod)^2
    mean(Evol_liab_JU77_MA_P6p_wt_mod)
    
    #JU77_MA_P6p_wt_mod data scale
    {
      
      predict_JU77_MA_P6p_wt_mod <- map(1:nrow(JU77_MA_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU77_MA_P6p_wt_mod %*% JU77_MA_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU77_MA_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_JU77_MA_P6p_wt_mod,
                      var.a = JU77_MA_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_MA_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_MA_P6p_wt_mod <- data_JU77_MA_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_JU77_MA_P6p_wt_mod <- data_JU77_MA_P6p_wt_mod[["mean.obs"]]
      va_data_JU77_MA_P6p_wt_mod <- data_JU77_MA_P6p_wt_mod[["var.a.obs"]]
      vp_data_JU77_MA_P6p_wt_mod <- data_JU77_MA_P6p_wt_mod[["var.obs"]]
      
      Evol_data_JU77_MA_P6p_wt_mod <- (va_data_JU77_MA_P6p_wt_mod/2) / (trait_mean_data_JU77_MA_P6p_wt_mod)^2
      
      mean(h2_data_JU77_MA_P6p_wt_mod)
      mean(trait_mean_data_JU77_MA_P6p_wt_mod)
      mean(va_data_JU77_MA_P6p_wt_mod)
      mean(vp_data_JU77_MA_P6p_wt_mod)
      mean(Evol_data_JU77_MA_P6p_wt_mod)
      
    }
    
  }
  
  #JU77_Vm_P6p_wt_mod 
  {
    
    JU77_Vm_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_JU77_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(JU77_Vm_P6p_wt_mod, file = "JU77_Vm_P6p_wt_mod.rds")
    JU77_Vm_P6p_wt_mod <- readRDS("JU77_Vm_P6p_wt_mod.rds")
    
    summary(JU77_Vm_P6p_wt_mod) 
    plotTrace(JU77_Vm_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("JU77_Vm_P6p_wt_mod.pdf")
    plot(JU77_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(JU77_Vm_P6p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU77_Vm_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_Vm_P6p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU77_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU77_Vm_P6p_wt_mod[["VCV"]])
    #autocorr.plot(JU77_Vm_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU77_Vm_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_Vm_P6p_wt_mod <- JU77_Vm_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU77_Vm_P6p_wt_mod <- JU77_Vm_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_JU77_Vm_P6p_wt_mod <- rowSums(JU77_Vm_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_JU77_Vm_P6p_wt_mod) 
      HPDinterval(va_liab_JU77_Vm_P6p_wt_mod) 
      
      mean(vlat_JU77_Vm_P6p_wt_mod) 
      
      #variance of fixed effects
      X_JU77_Vm_P6p_wt_mod <- JU77_Vm_P6p_wt_mod[["X"]]
      beta_JU77_Vm_P6p_wt_mod <- JU77_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]
      vf_JU77_Vm_P6p_wt_mod   <- apply(beta_JU77_Vm_P6p_wt_mod, 1, function(b) {var(as.vector(X_JU77_Vm_P6p_wt_mod %*% b))}) 
      mean(vf_JU77_Vm_P6p_wt_mod) 
      
      
      h2_liab_JU77_Vm_P6p_wt_mod <- va_liab_JU77_Vm_P6p_wt_mod / (vlat_JU77_Vm_P6p_wt_mod + vf_JU77_Vm_P6p_wt_mod)
      mean(h2_liab_JU77_Vm_P6p_wt_mod) 
      posterior.mode(h2_liab_JU77_Vm_P6p_wt_mod)	
      median(h2_liab_JU77_Vm_P6p_wt_mod)		
      HPDinterval(h2_liab_JU77_Vm_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU77_Vm_P6p_wt_mod <- ((rowMeans(JU77_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU77_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + JU77_Vm_P6p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU77_Vm_P6p_wt_mod <- (va_liab_JU77_Vm_P6p_wt_mod/2) / (trait_mean_liab_JU77_Vm_P6p_wt_mod)^2
    mean(Evol_liab_JU77_Vm_P6p_wt_mod)
    
    
    #JU77_Vm_P6p_wt_mod data scale
    {
      
      predict_JU77_Vm_P6p_wt_mod <- map(1:nrow(JU77_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU77_Vm_P6p_wt_mod %*% JU77_Vm_P6p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU77_Vm_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_JU77_Vm_P6p_wt_mod,
                      var.a = JU77_Vm_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_Vm_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_Vm_P6p_wt_mod <- data_JU77_Vm_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_JU77_Vm_P6p_wt_mod <- data_JU77_Vm_P6p_wt_mod[["mean.obs"]]
      va_data_JU77_Vm_P6p_wt_mod <- data_JU77_Vm_P6p_wt_mod[["var.a.obs"]]
      vp_data_JU77_Vm_P6p_wt_mod <- data_JU77_Vm_P6p_wt_mod[["var.obs"]]
      
      Evol_data_JU77_Vm_P6p_wt_mod <- (va_data_JU77_Vm_P6p_wt_mod/2) / (trait_mean_data_JU77_Vm_P6p_wt_mod)^2
      
      mean(h2_data_JU77_Vm_P6p_wt_mod)
      mean(trait_mean_data_JU77_Vm_P6p_wt_mod)
      mean(va_data_JU77_Vm_P6p_wt_mod)
      mean(vp_data_JU77_Vm_P6p_wt_mod)
      mean(Evol_data_JU77_Vm_P6p_wt_mod)
      
    }
    
  }
  
  
  
  
  ## JU77 P7p ----
  
  
  #JU77_CONTROL_P7p_wt_mod 
  {
    
    JU77_CONTROL_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_JU77_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(JU77_CONTROL_P7p_wt_mod, file = "JU77_CONTROL_P7p_wt_mod.rds")
    JU77_CONTROL_P7p_wt_mod <- readRDS("JU77_CONTROL_P7p_wt_mod.rds")
    
    summary(JU77_CONTROL_P7p_wt_mod) 
    plotTrace(JU77_CONTROL_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU77_CONTROL_P7p_wt_mod.pdf")
    plot(JU77_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU77_CONTROL_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU77_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU77_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU77_CONTROL_P7p_wt_mod[["VCV"]])
    #autocorr.plot(JU77_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU77_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_CONTROL_P7p_wt_mod <- JU77_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU77_CONTROL_P7p_wt_mod <- JU77_CONTROL_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_JU77_CONTROL_P7p_wt_mod <- rowSums(JU77_CONTROL_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_JU77_CONTROL_P7p_wt_mod) 
      HPDinterval(va_liab_JU77_CONTROL_P7p_wt_mod) 
      
      mean(vlat_JU77_CONTROL_P7p_wt_mod) 
      
      #variance of fixed effects
      X_JU77_CONTROL_P7p_wt_mod <- JU77_CONTROL_P7p_wt_mod[["X"]]
      beta_JU77_CONTROL_P7p_wt_mod <- JU77_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU77_CONTROL_P7p_wt_mod   <- apply(beta_JU77_CONTROL_P7p_wt_mod, 1, function(b) {var(as.vector(X_JU77_CONTROL_P7p_wt_mod %*% b))}) 
      mean(vf_JU77_CONTROL_P7p_wt_mod) 
      
      
      h2_liab_JU77_CONTROL_P7p_wt_mod <- va_liab_JU77_CONTROL_P7p_wt_mod / (vlat_JU77_CONTROL_P7p_wt_mod + vf_JU77_CONTROL_P7p_wt_mod)
      mean(h2_liab_JU77_CONTROL_P7p_wt_mod) 
      posterior.mode(h2_liab_JU77_CONTROL_P7p_wt_mod)	
      median(h2_liab_JU77_CONTROL_P7p_wt_mod)		
      HPDinterval(h2_liab_JU77_CONTROL_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU77_CONTROL_P7p_wt_mod <- rowMeans(JU77_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU77_CONTROL_P7p_wt_mod <- (va_liab_JU77_CONTROL_P7p_wt_mod/2) / (trait_mean_liab_JU77_CONTROL_P7p_wt_mod)^2
    mean(Evol_liab_JU77_CONTROL_P7p_wt_mod)
    
    
    #JU77_CONTROL_P7p_wt_mod data scale
    {
      
      predict_JU77_CONTROL_P7p_wt_mod <- map(1:nrow(JU77_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU77_CONTROL_P7p_wt_mod %*% JU77_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU77_CONTROL_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_JU77_CONTROL_P7p_wt_mod,
                      var.a = JU77_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_CONTROL_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_CONTROL_P7p_wt_mod <- data_JU77_CONTROL_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_JU77_CONTROL_P7p_wt_mod <- data_JU77_CONTROL_P7p_wt_mod[["mean.obs"]]
      va_data_JU77_CONTROL_P7p_wt_mod <- data_JU77_CONTROL_P7p_wt_mod[["var.a.obs"]]
      vp_data_JU77_CONTROL_P7p_wt_mod <- data_JU77_CONTROL_P7p_wt_mod[["var.obs"]]
      
      Evol_data_JU77_CONTROL_P7p_wt_mod <- (va_data_JU77_CONTROL_P7p_wt_mod/2) / (trait_mean_data_JU77_CONTROL_P7p_wt_mod)^2
      
      mean(h2_data_JU77_CONTROL_P7p_wt_mod) 
      mean(trait_mean_data_JU77_CONTROL_P7p_wt_mod)
      mean(va_data_JU77_CONTROL_P7p_wt_mod) 
      mean(vp_data_JU77_CONTROL_P7p_wt_mod)
      mean(Evol_data_JU77_CONTROL_P7p_wt_mod)
      
    }
    
  }
  
  #JU77_MA_P7p_wt_mod 
  {
    
    JU77_MA_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_JU77_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(JU77_MA_P7p_wt_mod, file = "JU77_MA_P7p_wt_mod.rds")
    JU77_MA_P7p_wt_mod <- readRDS("JU77_MA_P7p_wt_mod.rds")
    
    summary(JU77_MA_P7p_wt_mod) 
    #plot(JU77_MA_P7p_wt_mod)
    
    # traces and posterior densities
    pdf("JU77_MA_P7p_wt_mod.pdf")
    plot(JU77_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU77_MA_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU77_MA_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_MA_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU77_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU77_MA_P7p_wt_mod[["VCV"]])
    #autocorr.plot(JU77_MA_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU77_MA_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_MA_P7p_wt_mod <- JU77_MA_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU77_MA_P7p_wt_mod <- JU77_MA_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_JU77_MA_P7p_wt_mod <- rowSums(JU77_MA_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_JU77_MA_P7p_wt_mod) 
      HPDinterval(va_liab_JU77_MA_P7p_wt_mod) 
      
      mean(vlat_JU77_MA_P7p_wt_mod) 
      
      #variance of fixed effects
      X_JU77_MA_P7p_wt_mod <- JU77_MA_P7p_wt_mod[["X"]]
      beta_JU77_MA_P7p_wt_mod <- JU77_MA_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU77_MA_P7p_wt_mod   <- apply(beta_JU77_MA_P7p_wt_mod, 1, function(b) {var(as.vector(X_JU77_MA_P7p_wt_mod %*% b))}) 
      mean(vf_JU77_MA_P7p_wt_mod) 
      
      h2_liab_JU77_MA_P7p_wt_mod <- va_liab_JU77_MA_P7p_wt_mod / (vlat_JU77_MA_P7p_wt_mod + vf_JU77_MA_P7p_wt_mod)
      mean(h2_liab_JU77_MA_P7p_wt_mod) 
      posterior.mode(h2_liab_JU77_MA_P7p_wt_mod)	
      median(h2_liab_JU77_MA_P7p_wt_mod)		
      HPDinterval(h2_liab_JU77_MA_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU77_MA_P7p_wt_mod <- rowMeans(JU77_MA_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU77_MA_P7p_wt_mod <- (va_liab_JU77_MA_P7p_wt_mod/2) / (trait_mean_liab_JU77_MA_P7p_wt_mod)^2
    mean(Evol_liab_JU77_MA_P7p_wt_mod)
    
    #JU77_MA_P7p_wt_mod data scale
    {
      
      predict_JU77_MA_P7p_wt_mod <- map(1:nrow(JU77_MA_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU77_MA_P7p_wt_mod %*% JU77_MA_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU77_MA_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_JU77_MA_P7p_wt_mod,
                      var.a = JU77_MA_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_MA_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_MA_P7p_wt_mod <- data_JU77_MA_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_JU77_MA_P7p_wt_mod <- data_JU77_MA_P7p_wt_mod[["mean.obs"]]
      va_data_JU77_MA_P7p_wt_mod <- data_JU77_MA_P7p_wt_mod[["var.a.obs"]]
      vp_data_JU77_MA_P7p_wt_mod <- data_JU77_MA_P7p_wt_mod[["var.obs"]]
      
      Evol_data_JU77_MA_P7p_wt_mod <- (va_data_JU77_MA_P7p_wt_mod/2) / (trait_mean_data_JU77_MA_P7p_wt_mod)^2
      
      mean(h2_data_JU77_MA_P7p_wt_mod)
      mean(trait_mean_data_JU77_MA_P7p_wt_mod)
      mean(va_data_JU77_MA_P7p_wt_mod)
      mean(vp_data_JU77_MA_P7p_wt_mod)
      mean(Evol_data_JU77_MA_P7p_wt_mod)
      
    }
    
  }
  
  #JU77_Vm_P7p_wt_mod 
  {
    
    JU77_Vm_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_JU77_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(JU77_Vm_P7p_wt_mod, file = "JU77_Vm_P7p_wt_mod.rds")
    JU77_Vm_P7p_wt_mod <- readRDS("JU77_Vm_P7p_wt_mod.rds")
    
    summary(JU77_Vm_P7p_wt_mod) 
    plotTrace(JU77_Vm_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("JU77_Vm_P7p_wt_mod.pdf")
    plot(JU77_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(JU77_Vm_P7p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU77_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU77_Vm_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU77_Vm_P7p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU77_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU77_Vm_P7p_wt_mod[["VCV"]])
    #autocorr.plot(JU77_Vm_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU77_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU77_Vm_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU77_Vm_P7p_wt_mod <- JU77_Vm_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU77_Vm_P7p_wt_mod <- JU77_Vm_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_JU77_Vm_P7p_wt_mod <- rowSums(JU77_Vm_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_JU77_Vm_P7p_wt_mod) 
      HPDinterval(va_liab_JU77_Vm_P7p_wt_mod) 
      
      mean(vlat_JU77_Vm_P7p_wt_mod) 
      
      #variance of fixed effects
      X_JU77_Vm_P7p_wt_mod <- JU77_Vm_P7p_wt_mod[["X"]]
      beta_JU77_Vm_P7p_wt_mod <- JU77_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]
      vf_JU77_Vm_P7p_wt_mod   <- apply(beta_JU77_Vm_P7p_wt_mod, 1, function(b) {var(as.vector(X_JU77_Vm_P7p_wt_mod %*% b))}) 
      mean(vf_JU77_Vm_P7p_wt_mod) 
      
      
      h2_liab_JU77_Vm_P7p_wt_mod <- va_liab_JU77_Vm_P7p_wt_mod / (vlat_JU77_Vm_P7p_wt_mod + vf_JU77_Vm_P7p_wt_mod)
      mean(h2_liab_JU77_Vm_P7p_wt_mod) 
      posterior.mode(h2_liab_JU77_Vm_P7p_wt_mod)	
      median(h2_liab_JU77_Vm_P7p_wt_mod)		
      HPDinterval(h2_liab_JU77_Vm_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU77_Vm_P7p_wt_mod <- ((rowMeans(JU77_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU77_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + JU77_Vm_P7p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU77_Vm_P7p_wt_mod <- (va_liab_JU77_Vm_P7p_wt_mod/2) / (trait_mean_liab_JU77_Vm_P7p_wt_mod)^2
    mean(Evol_liab_JU77_Vm_P7p_wt_mod)
    
    
    #JU77_Vm_P7p_wt_mod data scale
    {
      
      predict_JU77_Vm_P7p_wt_mod <- map(1:nrow(JU77_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU77_Vm_P7p_wt_mod %*% JU77_Vm_P7p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU77_Vm_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_JU77_Vm_P7p_wt_mod,
                      var.a = JU77_Vm_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU77_Vm_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU77_Vm_P7p_wt_mod <- data_JU77_Vm_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_JU77_Vm_P7p_wt_mod <- data_JU77_Vm_P7p_wt_mod[["mean.obs"]]
      va_data_JU77_Vm_P7p_wt_mod <- data_JU77_Vm_P7p_wt_mod[["var.a.obs"]]
      vp_data_JU77_Vm_P7p_wt_mod <- data_JU77_Vm_P7p_wt_mod[["var.obs"]]
      
      Evol_data_JU77_Vm_P7p_wt_mod <- (va_data_JU77_Vm_P7p_wt_mod/2) / (trait_mean_data_JU77_Vm_P7p_wt_mod)^2
      
      mean(h2_data_JU77_Vm_P7p_wt_mod)
      mean(trait_mean_data_JU77_Vm_P7p_wt_mod)
      mean(va_data_JU77_Vm_P7p_wt_mod)
      mean(vp_data_JU77_Vm_P7p_wt_mod)
      mean(Evol_data_JU77_Vm_P7p_wt_mod)
      
    }
    
  }
  
  
 



  
  
}



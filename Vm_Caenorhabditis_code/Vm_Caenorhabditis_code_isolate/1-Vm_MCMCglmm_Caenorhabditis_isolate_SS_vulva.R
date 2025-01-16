#1-Vm_MCMCglmm_Caenorhabditis_isolate_SS_vulva.R
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
Vm_Caenorhabditis_data <- subset(Vm_data, Genus == "Caenorhabditis" )
Vm_Caenorhabditis_data$LineB <- ifelse(Vm_Caenorhabditis_data$Treatment == "CONTROL" ,paste(Vm_Caenorhabditis_data$Line,Vm_Caenorhabditis_data$Block), paste(Vm_Caenorhabditis_data$Line))
Vm_Caenorhabditis_data$BlockRep <- paste(Vm_Caenorhabditis_data$Block,Vm_Caenorhabditis_data$Replicate)
Vm_Caenorhabditis_data$Treatment <- as.factor(Vm_Caenorhabditis_data$Treatment)
Vm_Caenorhabditis_data$Observer <- as.factor(Vm_Caenorhabditis_data$Observer)

View(Vm_Caenorhabditis_data)
table(Vm_Caenorhabditis_data$Species)
table(Vm_Caenorhabditis_data$Ancestral)

Vm_JU1200_data <- subset(Vm_Caenorhabditis_data, Ancestral =="JU1200")
Vm_PB306_data <- subset(Vm_Caenorhabditis_data, Ancestral =="PB306")
Vm_AF16_data <- subset(Vm_Caenorhabditis_data, Ancestral =="AF16")
Vm_PB800_data <- subset(Vm_Caenorhabditis_data, Ancestral =="PB800")


table(Vm_Caenorhabditis_data$P3.p_multinomial)
table(Vm_Caenorhabditis_data$P4.p_multinomial)
table(Vm_Caenorhabditis_data$P8.p_multinomial)





prior_bi_block <-
  list( R = list(V = 1, fix = 1),      # Fixing the "residual" variance to 1 because it is not identifiable in binary responses 
        G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1),
                 G2 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1)))

#JU1200----
{
  Vm_JU1200_bi_CONTROL <- subset(Vm_JU1200_data, Treatment =="CONTROL")
  Vm_JU1200_bi_MA <- subset(Vm_JU1200_data, Treatment =="MA")
  
  
  ##---- JU1200 P3p ----
  
  
  #JU1200_CONTROL_P3p_SS_mod 
  {
    
    JU1200_CONTROL_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vm_JU1200_bi_CONTROL,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)            
    
    saveRDS(JU1200_CONTROL_P3p_SS_mod, file = "JU1200_CONTROL_P3p_SS_mod.rds")
    JU1200_CONTROL_P3p_SS_mod <- readRDS("JU1200_CONTROL_P3p_SS_mod.rds")
    
    summary(JU1200_CONTROL_P3p_SS_mod) 
    plotTrace(JU1200_CONTROL_P3p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU1200_CONTROL_P3p_SS_mod.pdf")
    plot(JU1200_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(JU1200_CONTROL_P3p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU1200_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU1200_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU1200_CONTROL_P3p_SS_mod[["VCV"]])
    #autocorr.plot(JU1200_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU1200_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_CONTROL_P3p_SS_mod <- JU1200_CONTROL_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_JU1200_CONTROL_P3p_SS_mod <- JU1200_CONTROL_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_JU1200_CONTROL_P3p_SS_mod <- rowSums(JU1200_CONTROL_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_JU1200_CONTROL_P3p_SS_mod) 
      HPDinterval(va_liab_JU1200_CONTROL_P3p_SS_mod) 
      
      mean(vlat_JU1200_CONTROL_P3p_SS_mod) 
      
      #variance of fixed effects
      X_JU1200_CONTROL_P3p_SS_mod <- JU1200_CONTROL_P3p_SS_mod[["X"]]
      beta_JU1200_CONTROL_P3p_SS_mod <- JU1200_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]
      vf_JU1200_CONTROL_P3p_SS_mod   <- apply(beta_JU1200_CONTROL_P3p_SS_mod, 1, function(b) {var(as.vector(X_JU1200_CONTROL_P3p_SS_mod %*% b))}) 
      mean(vf_JU1200_CONTROL_P3p_SS_mod) 
      
      
      h2_liab_JU1200_CONTROL_P3p_SS_mod <- va_liab_JU1200_CONTROL_P3p_SS_mod / (vlat_JU1200_CONTROL_P3p_SS_mod + vf_JU1200_CONTROL_P3p_SS_mod)
      mean(h2_liab_JU1200_CONTROL_P3p_SS_mod) 
      posterior.mode(h2_liab_JU1200_CONTROL_P3p_SS_mod)	
      median(h2_liab_JU1200_CONTROL_P3p_SS_mod)		
      HPDinterval(h2_liab_JU1200_CONTROL_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU1200_CONTROL_P3p_SS_mod <- rowMeans(JU1200_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU1200_CONTROL_P3p_SS_mod <- (va_liab_JU1200_CONTROL_P3p_SS_mod/2) / (trait_mean_liab_JU1200_CONTROL_P3p_SS_mod)^2
    mean(Evol_liab_JU1200_CONTROL_P3p_SS_mod)
    
    
    #JU1200_CONTROL_P3p_SS_mod data scale
    {
      
      predict_JU1200_CONTROL_P3p_SS_mod <- map(1:nrow(JU1200_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU1200_CONTROL_P3p_SS_mod %*% JU1200_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU1200_CONTROL_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_JU1200_CONTROL_P3p_SS_mod,
                      var.a = JU1200_CONTROL_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_CONTROL_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_CONTROL_P3p_SS_mod <- data_JU1200_CONTROL_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_JU1200_CONTROL_P3p_SS_mod <- data_JU1200_CONTROL_P3p_SS_mod[["mean.obs"]]
      va_data_JU1200_CONTROL_P3p_SS_mod <- data_JU1200_CONTROL_P3p_SS_mod[["var.a.obs"]]
      vp_data_JU1200_CONTROL_P3p_SS_mod <- data_JU1200_CONTROL_P3p_SS_mod[["var.obs"]]
      
      Evol_data_JU1200_CONTROL_P3p_SS_mod <- (va_data_JU1200_CONTROL_P3p_SS_mod/2) / (trait_mean_data_JU1200_CONTROL_P3p_SS_mod)^2
      
      mean(h2_data_JU1200_CONTROL_P3p_SS_mod) 
      mean(trait_mean_data_JU1200_CONTROL_P3p_SS_mod)
      mean(va_data_JU1200_CONTROL_P3p_SS_mod) 
      mean(vp_data_JU1200_CONTROL_P3p_SS_mod)
      mean(Evol_data_JU1200_CONTROL_P3p_SS_mod)
      
    }
    
  }
  
  #JU1200_MA_P3p_SS_mod 
  {
    
    JU1200_MA_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_JU1200_bi_MA,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)         
    
    saveRDS(JU1200_MA_P3p_SS_mod, file = "JU1200_MA_P3p_SS_mod.rds")
    JU1200_MA_P3p_SS_mod <- readRDS("JU1200_MA_P3p_SS_mod.rds")
    
    summary(JU1200_MA_P3p_SS_mod) 
    #plot(JU1200_MA_P3p_SS_mod)
    
    # traces and posterior densities
    pdf("JU1200_MA_P3p_SS_mod.pdf")
    plot(JU1200_MA_P3p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(JU1200_MA_P3p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_MA_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU1200_MA_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_MA_P3p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU1200_MA_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU1200_MA_P3p_SS_mod[["VCV"]])
    #autocorr.plot(JU1200_MA_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_MA_P3p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU1200_MA_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_MA_P3p_SS_mod <- JU1200_MA_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_JU1200_MA_P3p_SS_mod <- JU1200_MA_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_JU1200_MA_P3p_SS_mod <- rowSums(JU1200_MA_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_JU1200_MA_P3p_SS_mod) 
      HPDinterval(va_liab_JU1200_MA_P3p_SS_mod) 
      
      mean(vlat_JU1200_MA_P3p_SS_mod) 
      
      #variance of fixed effects
      X_JU1200_MA_P3p_SS_mod <- JU1200_MA_P3p_SS_mod[["X"]]
      beta_JU1200_MA_P3p_SS_mod <- JU1200_MA_P3p_SS_mod[["Sol"]][,c(1:4)]
      vf_JU1200_MA_P3p_SS_mod   <- apply(beta_JU1200_MA_P3p_SS_mod, 1, function(b) {var(as.vector(X_JU1200_MA_P3p_SS_mod %*% b))}) 
      mean(vf_JU1200_MA_P3p_SS_mod) 
      
      h2_liab_JU1200_MA_P3p_SS_mod <- va_liab_JU1200_MA_P3p_SS_mod / (vlat_JU1200_MA_P3p_SS_mod + vf_JU1200_MA_P3p_SS_mod)
      mean(h2_liab_JU1200_MA_P3p_SS_mod) 
      posterior.mode(h2_liab_JU1200_MA_P3p_SS_mod)	
      median(h2_liab_JU1200_MA_P3p_SS_mod)		
      HPDinterval(h2_liab_JU1200_MA_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU1200_MA_P3p_SS_mod <- rowMeans(JU1200_MA_P3p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU1200_MA_P3p_SS_mod <- (va_liab_JU1200_MA_P3p_SS_mod/2) / (trait_mean_liab_JU1200_MA_P3p_SS_mod)^2
    mean(Evol_liab_JU1200_MA_P3p_SS_mod)
    
    #JU1200_MA_P3p_SS_mod data scale
    {
      
      predict_JU1200_MA_P3p_SS_mod <- map(1:nrow(JU1200_MA_P3p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU1200_MA_P3p_SS_mod %*% JU1200_MA_P3p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU1200_MA_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_JU1200_MA_P3p_SS_mod,
                      var.a = JU1200_MA_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_MA_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_MA_P3p_SS_mod <- data_JU1200_MA_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_JU1200_MA_P3p_SS_mod <- data_JU1200_MA_P3p_SS_mod[["mean.obs"]]
      va_data_JU1200_MA_P3p_SS_mod <- data_JU1200_MA_P3p_SS_mod[["var.a.obs"]]
      vp_data_JU1200_MA_P3p_SS_mod <- data_JU1200_MA_P3p_SS_mod[["var.obs"]]
      
      Evol_data_JU1200_MA_P3p_SS_mod <- (va_data_JU1200_MA_P3p_SS_mod/2) / (trait_mean_data_JU1200_MA_P3p_SS_mod)^2
      
      mean(h2_data_JU1200_MA_P3p_SS_mod)
      mean(trait_mean_data_JU1200_MA_P3p_SS_mod)
      mean(va_data_JU1200_MA_P3p_SS_mod)
      mean(vp_data_JU1200_MA_P3p_SS_mod)
      mean(Evol_data_JU1200_MA_P3p_SS_mod)
      
    }
    
  }
  
  #JU1200_Vm_P3p_SS_mod 
  {
    
    JU1200_Vm_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer + Treatment -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_JU1200_data,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)            
    
    saveRDS(JU1200_Vm_P3p_SS_mod, file = "JU1200_Vm_P3p_SS_mod.rds")
    JU1200_Vm_P3p_SS_mod <- readRDS("JU1200_Vm_P3p_SS_mod.rds")
    
    summary(JU1200_Vm_P3p_SS_mod) 
    plotTrace(JU1200_Vm_P3p_SS_mod$Sol)
    
    # traces and posterior densities
    pdf("JU1200_Vm_P3p_SS_mod.pdf")
    plot(JU1200_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(JU1200_Vm_P3p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU1200_Vm_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_Vm_P3p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU1200_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU1200_Vm_P3p_SS_mod[["VCV"]])
    #autocorr.plot(JU1200_Vm_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU1200_Vm_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_Vm_P3p_SS_mod <- JU1200_Vm_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_JU1200_Vm_P3p_SS_mod <- JU1200_Vm_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_JU1200_Vm_P3p_SS_mod <- rowSums(JU1200_Vm_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_JU1200_Vm_P3p_SS_mod) 
      HPDinterval(va_liab_JU1200_Vm_P3p_SS_mod) 
      
      mean(vlat_JU1200_Vm_P3p_SS_mod) 
      
      #variance of fixed effects
      X_JU1200_Vm_P3p_SS_mod <- JU1200_Vm_P3p_SS_mod[["X"]]
      beta_JU1200_Vm_P3p_SS_mod <- JU1200_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]
      vf_JU1200_Vm_P3p_SS_mod   <- apply(beta_JU1200_Vm_P3p_SS_mod, 1, function(b) {var(as.vector(X_JU1200_Vm_P3p_SS_mod %*% b))}) 
      mean(vf_JU1200_Vm_P3p_SS_mod) 
      
      
      h2_liab_JU1200_Vm_P3p_SS_mod <- va_liab_JU1200_Vm_P3p_SS_mod / (vlat_JU1200_Vm_P3p_SS_mod + vf_JU1200_Vm_P3p_SS_mod)
      mean(h2_liab_JU1200_Vm_P3p_SS_mod) 
      posterior.mode(h2_liab_JU1200_Vm_P3p_SS_mod)	
      median(h2_liab_JU1200_Vm_P3p_SS_mod)		
      HPDinterval(h2_liab_JU1200_Vm_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU1200_Vm_P3p_SS_mod <- ((rowMeans(JU1200_Vm_P3p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU1200_Vm_P3p_SS_mod[["Sol"]][,c(1:4)]) + JU1200_Vm_P3p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU1200_Vm_P3p_SS_mod <- (va_liab_JU1200_Vm_P3p_SS_mod/2) / (trait_mean_liab_JU1200_Vm_P3p_SS_mod)^2
    mean(Evol_liab_JU1200_Vm_P3p_SS_mod)
    
    
    #JU1200_Vm_P3p_SS_mod data scale
    {
      
      predict_JU1200_Vm_P3p_SS_mod <- map(1:nrow(JU1200_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU1200_Vm_P3p_SS_mod %*% JU1200_Vm_P3p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU1200_Vm_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_JU1200_Vm_P3p_SS_mod,
                      var.a = JU1200_Vm_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_Vm_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_Vm_P3p_SS_mod <- data_JU1200_Vm_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_JU1200_Vm_P3p_SS_mod <- data_JU1200_Vm_P3p_SS_mod[["mean.obs"]]
      va_data_JU1200_Vm_P3p_SS_mod <- data_JU1200_Vm_P3p_SS_mod[["var.a.obs"]]
      vp_data_JU1200_Vm_P3p_SS_mod <- data_JU1200_Vm_P3p_SS_mod[["var.obs"]]
      
      Evol_data_JU1200_Vm_P3p_SS_mod <- (va_data_JU1200_Vm_P3p_SS_mod/2) / (trait_mean_data_JU1200_Vm_P3p_SS_mod)^2
      
      mean(h2_data_JU1200_Vm_P3p_SS_mod)
      mean(trait_mean_data_JU1200_Vm_P3p_SS_mod)
      mean(va_data_JU1200_Vm_P3p_SS_mod)
      mean(vp_data_JU1200_Vm_P3p_SS_mod)
      mean(Evol_data_JU1200_Vm_P3p_SS_mod)
      
    }
    
  }
  
  
  
  
  
  ##---- JU1200 P4p ----
  
  
  #JU1200_CONTROL_P4p_SS_mod 
  {
    
    JU1200_CONTROL_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vm_JU1200_bi_CONTROL,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)            
    
    saveRDS(JU1200_CONTROL_P4p_SS_mod, file = "JU1200_CONTROL_P4p_SS_mod.rds")
    JU1200_CONTROL_P4p_SS_mod <- readRDS("JU1200_CONTROL_P4p_SS_mod.rds")
    
    summary(JU1200_CONTROL_P4p_SS_mod) 
    plotTrace(JU1200_CONTROL_P4p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU1200_CONTROL_P4p_SS_mod.pdf")
    plot(JU1200_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(JU1200_CONTROL_P4p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU1200_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU1200_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU1200_CONTROL_P4p_SS_mod[["VCV"]])
    #autocorr.plot(JU1200_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU1200_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_CONTROL_P4p_SS_mod <- JU1200_CONTROL_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_JU1200_CONTROL_P4p_SS_mod <- JU1200_CONTROL_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_JU1200_CONTROL_P4p_SS_mod <- rowSums(JU1200_CONTROL_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_JU1200_CONTROL_P4p_SS_mod) 
      HPDinterval(va_liab_JU1200_CONTROL_P4p_SS_mod) 
      
      mean(vlat_JU1200_CONTROL_P4p_SS_mod) 
      
      #variance of fixed effects
      X_JU1200_CONTROL_P4p_SS_mod <- JU1200_CONTROL_P4p_SS_mod[["X"]]
      beta_JU1200_CONTROL_P4p_SS_mod <- JU1200_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]
      vf_JU1200_CONTROL_P4p_SS_mod   <- apply(beta_JU1200_CONTROL_P4p_SS_mod, 1, function(b) {var(as.vector(X_JU1200_CONTROL_P4p_SS_mod %*% b))}) 
      mean(vf_JU1200_CONTROL_P4p_SS_mod) 
      
      
      h2_liab_JU1200_CONTROL_P4p_SS_mod <- va_liab_JU1200_CONTROL_P4p_SS_mod / (vlat_JU1200_CONTROL_P4p_SS_mod + vf_JU1200_CONTROL_P4p_SS_mod)
      mean(h2_liab_JU1200_CONTROL_P4p_SS_mod) 
      posterior.mode(h2_liab_JU1200_CONTROL_P4p_SS_mod)	
      median(h2_liab_JU1200_CONTROL_P4p_SS_mod)		
      HPDinterval(h2_liab_JU1200_CONTROL_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU1200_CONTROL_P4p_SS_mod <- rowMeans(JU1200_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU1200_CONTROL_P4p_SS_mod <- (va_liab_JU1200_CONTROL_P4p_SS_mod/2) / (trait_mean_liab_JU1200_CONTROL_P4p_SS_mod)^2
    mean(Evol_liab_JU1200_CONTROL_P4p_SS_mod)
    
    
    #JU1200_CONTROL_P4p_SS_mod data scale
    {
      
      predict_JU1200_CONTROL_P4p_SS_mod <- map(1:nrow(JU1200_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU1200_CONTROL_P4p_SS_mod %*% JU1200_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU1200_CONTROL_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_JU1200_CONTROL_P4p_SS_mod,
                      var.a = JU1200_CONTROL_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_CONTROL_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_CONTROL_P4p_SS_mod <- data_JU1200_CONTROL_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_JU1200_CONTROL_P4p_SS_mod <- data_JU1200_CONTROL_P4p_SS_mod[["mean.obs"]]
      va_data_JU1200_CONTROL_P4p_SS_mod <- data_JU1200_CONTROL_P4p_SS_mod[["var.a.obs"]]
      vp_data_JU1200_CONTROL_P4p_SS_mod <- data_JU1200_CONTROL_P4p_SS_mod[["var.obs"]]
      
      Evol_data_JU1200_CONTROL_P4p_SS_mod <- (va_data_JU1200_CONTROL_P4p_SS_mod/2) / (trait_mean_data_JU1200_CONTROL_P4p_SS_mod)^2
      
      mean(h2_data_JU1200_CONTROL_P4p_SS_mod) 
      mean(trait_mean_data_JU1200_CONTROL_P4p_SS_mod)
      mean(va_data_JU1200_CONTROL_P4p_SS_mod) 
      mean(vp_data_JU1200_CONTROL_P4p_SS_mod)
      mean(Evol_data_JU1200_CONTROL_P4p_SS_mod)
      
    }
    
  }
  
  #JU1200_MA_P4p_SS_mod 
  {
    
    JU1200_MA_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_JU1200_bi_MA,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)         
    
    saveRDS(JU1200_MA_P4p_SS_mod, file = "JU1200_MA_P4p_SS_mod.rds")
    JU1200_MA_P4p_SS_mod <- readRDS("JU1200_MA_P4p_SS_mod.rds")
    
    summary(JU1200_MA_P4p_SS_mod) 
    #plot(JU1200_MA_P4p_SS_mod)
    
    # traces and posterior densities
    pdf("JU1200_MA_P4p_SS_mod.pdf")
    plot(JU1200_MA_P4p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(JU1200_MA_P4p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_MA_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU1200_MA_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_MA_P4p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU1200_MA_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU1200_MA_P4p_SS_mod[["VCV"]])
    #autocorr.plot(JU1200_MA_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_MA_P4p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU1200_MA_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_MA_P4p_SS_mod <- JU1200_MA_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_JU1200_MA_P4p_SS_mod <- JU1200_MA_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_JU1200_MA_P4p_SS_mod <- rowSums(JU1200_MA_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_JU1200_MA_P4p_SS_mod) 
      HPDinterval(va_liab_JU1200_MA_P4p_SS_mod) 
      
      mean(vlat_JU1200_MA_P4p_SS_mod) 
      
      #variance of fixed effects
      X_JU1200_MA_P4p_SS_mod <- JU1200_MA_P4p_SS_mod[["X"]]
      beta_JU1200_MA_P4p_SS_mod <- JU1200_MA_P4p_SS_mod[["Sol"]][,c(1:4)]
      vf_JU1200_MA_P4p_SS_mod   <- apply(beta_JU1200_MA_P4p_SS_mod, 1, function(b) {var(as.vector(X_JU1200_MA_P4p_SS_mod %*% b))}) 
      mean(vf_JU1200_MA_P4p_SS_mod) 
      
      h2_liab_JU1200_MA_P4p_SS_mod <- va_liab_JU1200_MA_P4p_SS_mod / (vlat_JU1200_MA_P4p_SS_mod + vf_JU1200_MA_P4p_SS_mod)
      mean(h2_liab_JU1200_MA_P4p_SS_mod) 
      posterior.mode(h2_liab_JU1200_MA_P4p_SS_mod)	
      median(h2_liab_JU1200_MA_P4p_SS_mod)		
      HPDinterval(h2_liab_JU1200_MA_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU1200_MA_P4p_SS_mod <- rowMeans(JU1200_MA_P4p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU1200_MA_P4p_SS_mod <- (va_liab_JU1200_MA_P4p_SS_mod/2) / (trait_mean_liab_JU1200_MA_P4p_SS_mod)^2
    mean(Evol_liab_JU1200_MA_P4p_SS_mod)
    
    #JU1200_MA_P4p_SS_mod data scale
    {
      
      predict_JU1200_MA_P4p_SS_mod <- map(1:nrow(JU1200_MA_P4p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU1200_MA_P4p_SS_mod %*% JU1200_MA_P4p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU1200_MA_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_JU1200_MA_P4p_SS_mod,
                      var.a = JU1200_MA_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_MA_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_MA_P4p_SS_mod <- data_JU1200_MA_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_JU1200_MA_P4p_SS_mod <- data_JU1200_MA_P4p_SS_mod[["mean.obs"]]
      va_data_JU1200_MA_P4p_SS_mod <- data_JU1200_MA_P4p_SS_mod[["var.a.obs"]]
      vp_data_JU1200_MA_P4p_SS_mod <- data_JU1200_MA_P4p_SS_mod[["var.obs"]]
      
      Evol_data_JU1200_MA_P4p_SS_mod <- (va_data_JU1200_MA_P4p_SS_mod/2) / (trait_mean_data_JU1200_MA_P4p_SS_mod)^2
      
      mean(h2_data_JU1200_MA_P4p_SS_mod)
      mean(trait_mean_data_JU1200_MA_P4p_SS_mod)
      mean(va_data_JU1200_MA_P4p_SS_mod)
      mean(vp_data_JU1200_MA_P4p_SS_mod)
      mean(Evol_data_JU1200_MA_P4p_SS_mod)
      
    }
    
  }
  
  #JU1200_Vm_P4p_SS_mod 
  {
    
    JU1200_Vm_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer + Treatment -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_JU1200_data,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)            
    
    saveRDS(JU1200_Vm_P4p_SS_mod, file = "JU1200_Vm_P4p_SS_mod.rds")
    JU1200_Vm_P4p_SS_mod <- readRDS("JU1200_Vm_P4p_SS_mod.rds")
    
    summary(JU1200_Vm_P4p_SS_mod) 
    plotTrace(JU1200_Vm_P4p_SS_mod$Sol)
    
    # traces and posterior densities
    pdf("JU1200_Vm_P4p_SS_mod.pdf")
    plot(JU1200_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(JU1200_Vm_P4p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU1200_Vm_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_Vm_P4p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU1200_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU1200_Vm_P4p_SS_mod[["VCV"]])
    #autocorr.plot(JU1200_Vm_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU1200_Vm_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_Vm_P4p_SS_mod <- JU1200_Vm_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_JU1200_Vm_P4p_SS_mod <- JU1200_Vm_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_JU1200_Vm_P4p_SS_mod <- rowSums(JU1200_Vm_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_JU1200_Vm_P4p_SS_mod) 
      HPDinterval(va_liab_JU1200_Vm_P4p_SS_mod) 
      
      mean(vlat_JU1200_Vm_P4p_SS_mod) 
      
      #variance of fixed effects
      X_JU1200_Vm_P4p_SS_mod <- JU1200_Vm_P4p_SS_mod[["X"]]
      beta_JU1200_Vm_P4p_SS_mod <- JU1200_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]
      vf_JU1200_Vm_P4p_SS_mod   <- apply(beta_JU1200_Vm_P4p_SS_mod, 1, function(b) {var(as.vector(X_JU1200_Vm_P4p_SS_mod %*% b))}) 
      mean(vf_JU1200_Vm_P4p_SS_mod) 
      
      
      h2_liab_JU1200_Vm_P4p_SS_mod <- va_liab_JU1200_Vm_P4p_SS_mod / (vlat_JU1200_Vm_P4p_SS_mod + vf_JU1200_Vm_P4p_SS_mod)
      mean(h2_liab_JU1200_Vm_P4p_SS_mod) 
      posterior.mode(h2_liab_JU1200_Vm_P4p_SS_mod)	
      median(h2_liab_JU1200_Vm_P4p_SS_mod)		
      HPDinterval(h2_liab_JU1200_Vm_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU1200_Vm_P4p_SS_mod <- ((rowMeans(JU1200_Vm_P4p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU1200_Vm_P4p_SS_mod[["Sol"]][,c(1:4)]) + JU1200_Vm_P4p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU1200_Vm_P4p_SS_mod <- (va_liab_JU1200_Vm_P4p_SS_mod/2) / (trait_mean_liab_JU1200_Vm_P4p_SS_mod)^2
    mean(Evol_liab_JU1200_Vm_P4p_SS_mod)
    
    
    #JU1200_Vm_P4p_SS_mod data scale
    {
      
      predict_JU1200_Vm_P4p_SS_mod <- map(1:nrow(JU1200_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU1200_Vm_P4p_SS_mod %*% JU1200_Vm_P4p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU1200_Vm_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_JU1200_Vm_P4p_SS_mod,
                      var.a = JU1200_Vm_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_Vm_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_Vm_P4p_SS_mod <- data_JU1200_Vm_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_JU1200_Vm_P4p_SS_mod <- data_JU1200_Vm_P4p_SS_mod[["mean.obs"]]
      va_data_JU1200_Vm_P4p_SS_mod <- data_JU1200_Vm_P4p_SS_mod[["var.a.obs"]]
      vp_data_JU1200_Vm_P4p_SS_mod <- data_JU1200_Vm_P4p_SS_mod[["var.obs"]]
      
      Evol_data_JU1200_Vm_P4p_SS_mod <- (va_data_JU1200_Vm_P4p_SS_mod/2) / (trait_mean_data_JU1200_Vm_P4p_SS_mod)^2
      
      mean(h2_data_JU1200_Vm_P4p_SS_mod)
      mean(trait_mean_data_JU1200_Vm_P4p_SS_mod)
      mean(va_data_JU1200_Vm_P4p_SS_mod)
      mean(vp_data_JU1200_Vm_P4p_SS_mod)
      mean(Evol_data_JU1200_Vm_P4p_SS_mod)
      
    }
    
  }
  
  
  
  
  
  ##---- JU1200 P8p ----
  
  
  #JU1200_CONTROL_P8p_SS_mod 
  {
    
    JU1200_CONTROL_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vm_JU1200_bi_CONTROL,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)            
    
    saveRDS(JU1200_CONTROL_P8p_SS_mod, file = "JU1200_CONTROL_P8p_SS_mod.rds")
    JU1200_CONTROL_P8p_SS_mod <- readRDS("JU1200_CONTROL_P8p_SS_mod.rds")
    
    summary(JU1200_CONTROL_P8p_SS_mod) 
    plotTrace(JU1200_CONTROL_P8p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU1200_CONTROL_P8p_SS_mod.pdf")
    plot(JU1200_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(JU1200_CONTROL_P8p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU1200_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU1200_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU1200_CONTROL_P8p_SS_mod[["VCV"]])
    #autocorr.plot(JU1200_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU1200_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_CONTROL_P8p_SS_mod <- JU1200_CONTROL_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_JU1200_CONTROL_P8p_SS_mod <- JU1200_CONTROL_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_JU1200_CONTROL_P8p_SS_mod <- rowSums(JU1200_CONTROL_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_JU1200_CONTROL_P8p_SS_mod) 
      HPDinterval(va_liab_JU1200_CONTROL_P8p_SS_mod) 
      
      mean(vlat_JU1200_CONTROL_P8p_SS_mod) 
      
      #variance of fixed effects
      X_JU1200_CONTROL_P8p_SS_mod <- JU1200_CONTROL_P8p_SS_mod[["X"]]
      beta_JU1200_CONTROL_P8p_SS_mod <- JU1200_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]
      vf_JU1200_CONTROL_P8p_SS_mod   <- apply(beta_JU1200_CONTROL_P8p_SS_mod, 1, function(b) {var(as.vector(X_JU1200_CONTROL_P8p_SS_mod %*% b))}) 
      mean(vf_JU1200_CONTROL_P8p_SS_mod) 
      
      
      h2_liab_JU1200_CONTROL_P8p_SS_mod <- va_liab_JU1200_CONTROL_P8p_SS_mod / (vlat_JU1200_CONTROL_P8p_SS_mod + vf_JU1200_CONTROL_P8p_SS_mod)
      mean(h2_liab_JU1200_CONTROL_P8p_SS_mod) 
      posterior.mode(h2_liab_JU1200_CONTROL_P8p_SS_mod)	
      median(h2_liab_JU1200_CONTROL_P8p_SS_mod)		
      HPDinterval(h2_liab_JU1200_CONTROL_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU1200_CONTROL_P8p_SS_mod <- rowMeans(JU1200_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU1200_CONTROL_P8p_SS_mod <- (va_liab_JU1200_CONTROL_P8p_SS_mod/2) / (trait_mean_liab_JU1200_CONTROL_P8p_SS_mod)^2
    mean(Evol_liab_JU1200_CONTROL_P8p_SS_mod)
    
    
    #JU1200_CONTROL_P8p_SS_mod data scale
    {
      
      predict_JU1200_CONTROL_P8p_SS_mod <- map(1:nrow(JU1200_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU1200_CONTROL_P8p_SS_mod %*% JU1200_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU1200_CONTROL_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_JU1200_CONTROL_P8p_SS_mod,
                      var.a = JU1200_CONTROL_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_CONTROL_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_CONTROL_P8p_SS_mod <- data_JU1200_CONTROL_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_JU1200_CONTROL_P8p_SS_mod <- data_JU1200_CONTROL_P8p_SS_mod[["mean.obs"]]
      va_data_JU1200_CONTROL_P8p_SS_mod <- data_JU1200_CONTROL_P8p_SS_mod[["var.a.obs"]]
      vp_data_JU1200_CONTROL_P8p_SS_mod <- data_JU1200_CONTROL_P8p_SS_mod[["var.obs"]]
      
      Evol_data_JU1200_CONTROL_P8p_SS_mod <- (va_data_JU1200_CONTROL_P8p_SS_mod/2) / (trait_mean_data_JU1200_CONTROL_P8p_SS_mod)^2
      
      mean(h2_data_JU1200_CONTROL_P8p_SS_mod) 
      mean(trait_mean_data_JU1200_CONTROL_P8p_SS_mod)
      mean(va_data_JU1200_CONTROL_P8p_SS_mod) 
      mean(vp_data_JU1200_CONTROL_P8p_SS_mod)
      mean(Evol_data_JU1200_CONTROL_P8p_SS_mod)
      
    }
    
  }
  
  #JU1200_MA_P8p_SS_mod 
  {
    
    JU1200_MA_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_JU1200_bi_MA,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)         
    
    saveRDS(JU1200_MA_P8p_SS_mod, file = "JU1200_MA_P8p_SS_mod.rds")
    JU1200_MA_P8p_SS_mod <- readRDS("JU1200_MA_P8p_SS_mod.rds")
    
    summary(JU1200_MA_P8p_SS_mod) 
    #plot(JU1200_MA_P8p_SS_mod)
    
    # traces and posterior densities
    pdf("JU1200_MA_P8p_SS_mod.pdf")
    plot(JU1200_MA_P8p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(JU1200_MA_P8p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_MA_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU1200_MA_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_MA_P8p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU1200_MA_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU1200_MA_P8p_SS_mod[["VCV"]])
    #autocorr.plot(JU1200_MA_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_MA_P8p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU1200_MA_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_MA_P8p_SS_mod <- JU1200_MA_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_JU1200_MA_P8p_SS_mod <- JU1200_MA_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_JU1200_MA_P8p_SS_mod <- rowSums(JU1200_MA_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_JU1200_MA_P8p_SS_mod) 
      HPDinterval(va_liab_JU1200_MA_P8p_SS_mod) 
      
      mean(vlat_JU1200_MA_P8p_SS_mod) 
      
      #variance of fixed effects
      X_JU1200_MA_P8p_SS_mod <- JU1200_MA_P8p_SS_mod[["X"]]
      beta_JU1200_MA_P8p_SS_mod <- JU1200_MA_P8p_SS_mod[["Sol"]][,c(1:4)]
      vf_JU1200_MA_P8p_SS_mod   <- apply(beta_JU1200_MA_P8p_SS_mod, 1, function(b) {var(as.vector(X_JU1200_MA_P8p_SS_mod %*% b))}) 
      mean(vf_JU1200_MA_P8p_SS_mod) 
      
      h2_liab_JU1200_MA_P8p_SS_mod <- va_liab_JU1200_MA_P8p_SS_mod / (vlat_JU1200_MA_P8p_SS_mod + vf_JU1200_MA_P8p_SS_mod)
      mean(h2_liab_JU1200_MA_P8p_SS_mod) 
      posterior.mode(h2_liab_JU1200_MA_P8p_SS_mod)	
      median(h2_liab_JU1200_MA_P8p_SS_mod)		
      HPDinterval(h2_liab_JU1200_MA_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU1200_MA_P8p_SS_mod <- rowMeans(JU1200_MA_P8p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU1200_MA_P8p_SS_mod <- (va_liab_JU1200_MA_P8p_SS_mod/2) / (trait_mean_liab_JU1200_MA_P8p_SS_mod)^2
    mean(Evol_liab_JU1200_MA_P8p_SS_mod)
    
    #JU1200_MA_P8p_SS_mod data scale
    {
      
      predict_JU1200_MA_P8p_SS_mod <- map(1:nrow(JU1200_MA_P8p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU1200_MA_P8p_SS_mod %*% JU1200_MA_P8p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU1200_MA_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_JU1200_MA_P8p_SS_mod,
                      var.a = JU1200_MA_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_MA_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_MA_P8p_SS_mod <- data_JU1200_MA_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_JU1200_MA_P8p_SS_mod <- data_JU1200_MA_P8p_SS_mod[["mean.obs"]]
      va_data_JU1200_MA_P8p_SS_mod <- data_JU1200_MA_P8p_SS_mod[["var.a.obs"]]
      vp_data_JU1200_MA_P8p_SS_mod <- data_JU1200_MA_P8p_SS_mod[["var.obs"]]
      
      Evol_data_JU1200_MA_P8p_SS_mod <- (va_data_JU1200_MA_P8p_SS_mod/2) / (trait_mean_data_JU1200_MA_P8p_SS_mod)^2
      
      mean(h2_data_JU1200_MA_P8p_SS_mod)
      mean(trait_mean_data_JU1200_MA_P8p_SS_mod)
      mean(va_data_JU1200_MA_P8p_SS_mod)
      mean(vp_data_JU1200_MA_P8p_SS_mod)
      mean(Evol_data_JU1200_MA_P8p_SS_mod)
      
    }
    
  }
  
  #JU1200_Vm_P8p_SS_mod 
  {
    
    JU1200_Vm_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer + Treatment -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_JU1200_data,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)            
    
    saveRDS(JU1200_Vm_P8p_SS_mod, file = "JU1200_Vm_P8p_SS_mod.rds")
    JU1200_Vm_P8p_SS_mod <- readRDS("JU1200_Vm_P8p_SS_mod.rds")
    
    summary(JU1200_Vm_P8p_SS_mod) 
    plotTrace(JU1200_Vm_P8p_SS_mod$Sol)
    
    # traces and posterior densities
    pdf("JU1200_Vm_P8p_SS_mod.pdf")
    plot(JU1200_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(JU1200_Vm_P8p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU1200_Vm_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_Vm_P8p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU1200_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU1200_Vm_P8p_SS_mod[["VCV"]])
    #autocorr.plot(JU1200_Vm_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU1200_Vm_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_Vm_P8p_SS_mod <- JU1200_Vm_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_JU1200_Vm_P8p_SS_mod <- JU1200_Vm_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_JU1200_Vm_P8p_SS_mod <- rowSums(JU1200_Vm_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_JU1200_Vm_P8p_SS_mod) 
      HPDinterval(va_liab_JU1200_Vm_P8p_SS_mod) 
      
      mean(vlat_JU1200_Vm_P8p_SS_mod) 
      
      #variance of fixed effects
      X_JU1200_Vm_P8p_SS_mod <- JU1200_Vm_P8p_SS_mod[["X"]]
      beta_JU1200_Vm_P8p_SS_mod <- JU1200_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]
      vf_JU1200_Vm_P8p_SS_mod   <- apply(beta_JU1200_Vm_P8p_SS_mod, 1, function(b) {var(as.vector(X_JU1200_Vm_P8p_SS_mod %*% b))}) 
      mean(vf_JU1200_Vm_P8p_SS_mod) 
      
      
      h2_liab_JU1200_Vm_P8p_SS_mod <- va_liab_JU1200_Vm_P8p_SS_mod / (vlat_JU1200_Vm_P8p_SS_mod + vf_JU1200_Vm_P8p_SS_mod)
      mean(h2_liab_JU1200_Vm_P8p_SS_mod) 
      posterior.mode(h2_liab_JU1200_Vm_P8p_SS_mod)	
      median(h2_liab_JU1200_Vm_P8p_SS_mod)		
      HPDinterval(h2_liab_JU1200_Vm_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU1200_Vm_P8p_SS_mod <- ((rowMeans(JU1200_Vm_P8p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU1200_Vm_P8p_SS_mod[["Sol"]][,c(1:4)]) + JU1200_Vm_P8p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU1200_Vm_P8p_SS_mod <- (va_liab_JU1200_Vm_P8p_SS_mod/2) / (trait_mean_liab_JU1200_Vm_P8p_SS_mod)^2
    mean(Evol_liab_JU1200_Vm_P8p_SS_mod)
    
    
    #JU1200_Vm_P8p_SS_mod data scale
    {
      
      predict_JU1200_Vm_P8p_SS_mod <- map(1:nrow(JU1200_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU1200_Vm_P8p_SS_mod %*% JU1200_Vm_P8p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU1200_Vm_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_JU1200_Vm_P8p_SS_mod,
                      var.a = JU1200_Vm_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_Vm_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_Vm_P8p_SS_mod <- data_JU1200_Vm_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_JU1200_Vm_P8p_SS_mod <- data_JU1200_Vm_P8p_SS_mod[["mean.obs"]]
      va_data_JU1200_Vm_P8p_SS_mod <- data_JU1200_Vm_P8p_SS_mod[["var.a.obs"]]
      vp_data_JU1200_Vm_P8p_SS_mod <- data_JU1200_Vm_P8p_SS_mod[["var.obs"]]
      
      Evol_data_JU1200_Vm_P8p_SS_mod <- (va_data_JU1200_Vm_P8p_SS_mod/2) / (trait_mean_data_JU1200_Vm_P8p_SS_mod)^2
      
      mean(h2_data_JU1200_Vm_P8p_SS_mod)
      mean(trait_mean_data_JU1200_Vm_P8p_SS_mod)
      mean(va_data_JU1200_Vm_P8p_SS_mod)
      mean(vp_data_JU1200_Vm_P8p_SS_mod)
      mean(Evol_data_JU1200_Vm_P8p_SS_mod)
      
    }
    
  }
  
##---- JU1200 P5p ----
  
  
  #JU1200_CONTROL_P5p_wt_mod 
  {
    
    JU1200_CONTROL_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                          random      = ~ LineB + BlockRep,
                                          family      = "threshold",
                                          data        = Vm_JU1200_bi_CONTROL,
                                          prior       = prior_bi_block,
                                          nitt        = 1260000,       
                                          thin        = 500,           
                                          burnin      = 10000,
                                          trunc       = TRUE,
                                          pr          = TRUE,
                                          pl          = TRUE)            
    
    saveRDS(JU1200_CONTROL_P5p_wt_mod, file = "JU1200_CONTROL_P5p_wt_mod.rds")
    JU1200_CONTROL_P5p_wt_mod <- readRDS("JU1200_CONTROL_P5p_wt_mod.rds")
    
    summary(JU1200_CONTROL_P5p_wt_mod) 
    plotTrace(JU1200_CONTROL_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU1200_CONTROL_P5p_wt_mod.pdf")
    plot(JU1200_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU1200_CONTROL_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU1200_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU1200_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU1200_CONTROL_P5p_wt_mod[["VCV"]])
    #autocorr.plot(JU1200_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU1200_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_CONTROL_P5p_wt_mod <- JU1200_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU1200_CONTROL_P5p_wt_mod <- JU1200_CONTROL_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_JU1200_CONTROL_P5p_wt_mod <- rowSums(JU1200_CONTROL_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_JU1200_CONTROL_P5p_wt_mod) 
      HPDinterval(va_liab_JU1200_CONTROL_P5p_wt_mod) 
      
      mean(vlat_JU1200_CONTROL_P5p_wt_mod) 
      
      #variance of fixed effects
      X_JU1200_CONTROL_P5p_wt_mod <- JU1200_CONTROL_P5p_wt_mod[["X"]]
      beta_JU1200_CONTROL_P5p_wt_mod <- JU1200_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU1200_CONTROL_P5p_wt_mod   <- apply(beta_JU1200_CONTROL_P5p_wt_mod, 1, function(b) {var(as.vector(X_JU1200_CONTROL_P5p_wt_mod %*% b))}) 
      mean(vf_JU1200_CONTROL_P5p_wt_mod) 
      
      
      h2_liab_JU1200_CONTROL_P5p_wt_mod <- va_liab_JU1200_CONTROL_P5p_wt_mod / (vlat_JU1200_CONTROL_P5p_wt_mod + vf_JU1200_CONTROL_P5p_wt_mod)
      mean(h2_liab_JU1200_CONTROL_P5p_wt_mod) 
      posterior.mode(h2_liab_JU1200_CONTROL_P5p_wt_mod)	
      median(h2_liab_JU1200_CONTROL_P5p_wt_mod)		
      HPDinterval(h2_liab_JU1200_CONTROL_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU1200_CONTROL_P5p_wt_mod <- rowMeans(JU1200_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU1200_CONTROL_P5p_wt_mod <- (va_liab_JU1200_CONTROL_P5p_wt_mod/2) / (trait_mean_liab_JU1200_CONTROL_P5p_wt_mod)^2
    mean(Evol_liab_JU1200_CONTROL_P5p_wt_mod)
    
    
    #JU1200_CONTROL_P5p_wt_mod data scale
    {
      
      predict_JU1200_CONTROL_P5p_wt_mod <- map(1:nrow(JU1200_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU1200_CONTROL_P5p_wt_mod %*% JU1200_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU1200_CONTROL_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_JU1200_CONTROL_P5p_wt_mod,
                      var.a = JU1200_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_CONTROL_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_CONTROL_P5p_wt_mod <- data_JU1200_CONTROL_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_JU1200_CONTROL_P5p_wt_mod <- data_JU1200_CONTROL_P5p_wt_mod[["mean.obs"]]
      va_data_JU1200_CONTROL_P5p_wt_mod <- data_JU1200_CONTROL_P5p_wt_mod[["var.a.obs"]]
      vp_data_JU1200_CONTROL_P5p_wt_mod <- data_JU1200_CONTROL_P5p_wt_mod[["var.obs"]]
      
      Evol_data_JU1200_CONTROL_P5p_wt_mod <- (va_data_JU1200_CONTROL_P5p_wt_mod/2) / (trait_mean_data_JU1200_CONTROL_P5p_wt_mod)^2
      
      mean(h2_data_JU1200_CONTROL_P5p_wt_mod) 
      mean(trait_mean_data_JU1200_CONTROL_P5p_wt_mod)
      mean(va_data_JU1200_CONTROL_P5p_wt_mod) 
      mean(vp_data_JU1200_CONTROL_P5p_wt_mod)
      mean(Evol_data_JU1200_CONTROL_P5p_wt_mod)
      
    }
    
  }
  
  #JU1200_MA_P5p_wt_mod 
  {
    
    JU1200_MA_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                     random      = ~ LineB + BlockRep,
                                     family      = "threshold",
                                     data        = Vm_JU1200_bi_MA,
                                     prior       = prior_bi_block,
                                     nitt        = 1260000,       
                                     thin        = 500,           
                                     burnin      = 10000,
                                     trunc       = TRUE,
                                     pr          = TRUE,
                                     pl          = TRUE)         
    
    saveRDS(JU1200_MA_P5p_wt_mod, file = "JU1200_MA_P5p_wt_mod.rds")
    JU1200_MA_P5p_wt_mod <- readRDS("JU1200_MA_P5p_wt_mod.rds")
    
    summary(JU1200_MA_P5p_wt_mod) 
    #plot(JU1200_MA_P5p_wt_mod)
    
    # traces and posterior densities
    pdf("JU1200_MA_P5p_wt_mod.pdf")
    plot(JU1200_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU1200_MA_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU1200_MA_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_MA_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU1200_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU1200_MA_P5p_wt_mod[["VCV"]])
    #autocorr.plot(JU1200_MA_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU1200_MA_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_MA_P5p_wt_mod <- JU1200_MA_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU1200_MA_P5p_wt_mod <- JU1200_MA_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_JU1200_MA_P5p_wt_mod <- rowSums(JU1200_MA_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_JU1200_MA_P5p_wt_mod) 
      HPDinterval(va_liab_JU1200_MA_P5p_wt_mod) 
      
      mean(vlat_JU1200_MA_P5p_wt_mod) 
      
      #variance of fixed effects
      X_JU1200_MA_P5p_wt_mod <- JU1200_MA_P5p_wt_mod[["X"]]
      beta_JU1200_MA_P5p_wt_mod <- JU1200_MA_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU1200_MA_P5p_wt_mod   <- apply(beta_JU1200_MA_P5p_wt_mod, 1, function(b) {var(as.vector(X_JU1200_MA_P5p_wt_mod %*% b))}) 
      mean(vf_JU1200_MA_P5p_wt_mod) 
      
      h2_liab_JU1200_MA_P5p_wt_mod <- va_liab_JU1200_MA_P5p_wt_mod / (vlat_JU1200_MA_P5p_wt_mod + vf_JU1200_MA_P5p_wt_mod)
      mean(h2_liab_JU1200_MA_P5p_wt_mod) 
      posterior.mode(h2_liab_JU1200_MA_P5p_wt_mod)	
      median(h2_liab_JU1200_MA_P5p_wt_mod)		
      HPDinterval(h2_liab_JU1200_MA_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU1200_MA_P5p_wt_mod <- rowMeans(JU1200_MA_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU1200_MA_P5p_wt_mod <- (va_liab_JU1200_MA_P5p_wt_mod/2) / (trait_mean_liab_JU1200_MA_P5p_wt_mod)^2
    mean(Evol_liab_JU1200_MA_P5p_wt_mod)
    
    #JU1200_MA_P5p_wt_mod data scale
    {
      
      predict_JU1200_MA_P5p_wt_mod <- map(1:nrow(JU1200_MA_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU1200_MA_P5p_wt_mod %*% JU1200_MA_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU1200_MA_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_JU1200_MA_P5p_wt_mod,
                      var.a = JU1200_MA_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_MA_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_MA_P5p_wt_mod <- data_JU1200_MA_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_JU1200_MA_P5p_wt_mod <- data_JU1200_MA_P5p_wt_mod[["mean.obs"]]
      va_data_JU1200_MA_P5p_wt_mod <- data_JU1200_MA_P5p_wt_mod[["var.a.obs"]]
      vp_data_JU1200_MA_P5p_wt_mod <- data_JU1200_MA_P5p_wt_mod[["var.obs"]]
      
      Evol_data_JU1200_MA_P5p_wt_mod <- (va_data_JU1200_MA_P5p_wt_mod/2) / (trait_mean_data_JU1200_MA_P5p_wt_mod)^2
      
      mean(h2_data_JU1200_MA_P5p_wt_mod)
      mean(trait_mean_data_JU1200_MA_P5p_wt_mod)
      mean(va_data_JU1200_MA_P5p_wt_mod)
      mean(vp_data_JU1200_MA_P5p_wt_mod)
      mean(Evol_data_JU1200_MA_P5p_wt_mod)
      
    }
    
  }
  
  #JU1200_Vm_P5p_wt_mod 
  {
    
    JU1200_Vm_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Treatment -1,
                                     random      = ~ LineB + BlockRep,
                                     family      = "threshold",
                                     data        = Vm_JU1200_data,
                                     prior       = prior_bi_block,
                                     nitt        = 1260000,       
                                     thin        = 500,           
                                     burnin      = 10000,
                                     trunc       = TRUE,
                                     pr          = TRUE,
                                     pl          = TRUE)            
    
    saveRDS(JU1200_Vm_P5p_wt_mod, file = "JU1200_Vm_P5p_wt_mod.rds")
    JU1200_Vm_P5p_wt_mod <- readRDS("JU1200_Vm_P5p_wt_mod.rds")
    
    summary(JU1200_Vm_P5p_wt_mod) 
    plotTrace(JU1200_Vm_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("JU1200_Vm_P5p_wt_mod.pdf")
    plot(JU1200_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(JU1200_Vm_P5p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU1200_Vm_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_Vm_P5p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU1200_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU1200_Vm_P5p_wt_mod[["VCV"]])
    #autocorr.plot(JU1200_Vm_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU1200_Vm_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_Vm_P5p_wt_mod <- JU1200_Vm_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU1200_Vm_P5p_wt_mod <- JU1200_Vm_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_JU1200_Vm_P5p_wt_mod <- rowSums(JU1200_Vm_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_JU1200_Vm_P5p_wt_mod) 
      HPDinterval(va_liab_JU1200_Vm_P5p_wt_mod) 
      
      mean(vlat_JU1200_Vm_P5p_wt_mod) 
      
      #variance of fixed effects
      X_JU1200_Vm_P5p_wt_mod <- JU1200_Vm_P5p_wt_mod[["X"]]
      beta_JU1200_Vm_P5p_wt_mod <- JU1200_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]
      vf_JU1200_Vm_P5p_wt_mod   <- apply(beta_JU1200_Vm_P5p_wt_mod, 1, function(b) {var(as.vector(X_JU1200_Vm_P5p_wt_mod %*% b))}) 
      mean(vf_JU1200_Vm_P5p_wt_mod) 
      
      
      h2_liab_JU1200_Vm_P5p_wt_mod <- va_liab_JU1200_Vm_P5p_wt_mod / (vlat_JU1200_Vm_P5p_wt_mod + vf_JU1200_Vm_P5p_wt_mod)
      mean(h2_liab_JU1200_Vm_P5p_wt_mod) 
      posterior.mode(h2_liab_JU1200_Vm_P5p_wt_mod)	
      median(h2_liab_JU1200_Vm_P5p_wt_mod)		
      HPDinterval(h2_liab_JU1200_Vm_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU1200_Vm_P5p_wt_mod <- ((rowMeans(JU1200_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU1200_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + JU1200_Vm_P5p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU1200_Vm_P5p_wt_mod <- (va_liab_JU1200_Vm_P5p_wt_mod/2) / (trait_mean_liab_JU1200_Vm_P5p_wt_mod)^2
    mean(Evol_liab_JU1200_Vm_P5p_wt_mod)
    
    
    #JU1200_Vm_P5p_wt_mod data scale
    {
      
      predict_JU1200_Vm_P5p_wt_mod <- map(1:nrow(JU1200_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU1200_Vm_P5p_wt_mod %*% JU1200_Vm_P5p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU1200_Vm_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_JU1200_Vm_P5p_wt_mod,
                      var.a = JU1200_Vm_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_Vm_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_Vm_P5p_wt_mod <- data_JU1200_Vm_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_JU1200_Vm_P5p_wt_mod <- data_JU1200_Vm_P5p_wt_mod[["mean.obs"]]
      va_data_JU1200_Vm_P5p_wt_mod <- data_JU1200_Vm_P5p_wt_mod[["var.a.obs"]]
      vp_data_JU1200_Vm_P5p_wt_mod <- data_JU1200_Vm_P5p_wt_mod[["var.obs"]]
      
      Evol_data_JU1200_Vm_P5p_wt_mod <- (va_data_JU1200_Vm_P5p_wt_mod/2) / (trait_mean_data_JU1200_Vm_P5p_wt_mod)^2
      
      mean(h2_data_JU1200_Vm_P5p_wt_mod)
      mean(trait_mean_data_JU1200_Vm_P5p_wt_mod)
      mean(va_data_JU1200_Vm_P5p_wt_mod)
      mean(vp_data_JU1200_Vm_P5p_wt_mod)
      mean(Evol_data_JU1200_Vm_P5p_wt_mod)
      
    }
    
  }
  
  
 
  
  
  ##---- JU1200 P6p ----
  
  
  #JU1200_CONTROL_P6p_wt_mod 
  {
    
    JU1200_CONTROL_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                          random      = ~ LineB + BlockRep,
                                          family      = "threshold",
                                          data        = Vm_JU1200_bi_CONTROL,
                                          prior       = prior_bi_block,
                                          nitt        = 1260000,       
                                          thin        = 500,           
                                          burnin      = 10000,
                                          trunc       = TRUE,
                                          pr          = TRUE,
                                          pl          = TRUE)            
    
    saveRDS(JU1200_CONTROL_P6p_wt_mod, file = "JU1200_CONTROL_P6p_wt_mod.rds")
    JU1200_CONTROL_P6p_wt_mod <- readRDS("JU1200_CONTROL_P6p_wt_mod.rds")
    
    summary(JU1200_CONTROL_P6p_wt_mod) 
    plotTrace(JU1200_CONTROL_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU1200_CONTROL_P6p_wt_mod.pdf")
    plot(JU1200_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU1200_CONTROL_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU1200_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU1200_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU1200_CONTROL_P6p_wt_mod[["VCV"]])
    #autocorr.plot(JU1200_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU1200_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_CONTROL_P6p_wt_mod <- JU1200_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU1200_CONTROL_P6p_wt_mod <- JU1200_CONTROL_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_JU1200_CONTROL_P6p_wt_mod <- rowSums(JU1200_CONTROL_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_JU1200_CONTROL_P6p_wt_mod) 
      HPDinterval(va_liab_JU1200_CONTROL_P6p_wt_mod) 
      
      mean(vlat_JU1200_CONTROL_P6p_wt_mod) 
      
      #variance of fixed effects
      X_JU1200_CONTROL_P6p_wt_mod <- JU1200_CONTROL_P6p_wt_mod[["X"]]
      beta_JU1200_CONTROL_P6p_wt_mod <- JU1200_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU1200_CONTROL_P6p_wt_mod   <- apply(beta_JU1200_CONTROL_P6p_wt_mod, 1, function(b) {var(as.vector(X_JU1200_CONTROL_P6p_wt_mod %*% b))}) 
      mean(vf_JU1200_CONTROL_P6p_wt_mod) 
      
      
      h2_liab_JU1200_CONTROL_P6p_wt_mod <- va_liab_JU1200_CONTROL_P6p_wt_mod / (vlat_JU1200_CONTROL_P6p_wt_mod + vf_JU1200_CONTROL_P6p_wt_mod)
      mean(h2_liab_JU1200_CONTROL_P6p_wt_mod) 
      posterior.mode(h2_liab_JU1200_CONTROL_P6p_wt_mod)	
      median(h2_liab_JU1200_CONTROL_P6p_wt_mod)		
      HPDinterval(h2_liab_JU1200_CONTROL_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU1200_CONTROL_P6p_wt_mod <- rowMeans(JU1200_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU1200_CONTROL_P6p_wt_mod <- (va_liab_JU1200_CONTROL_P6p_wt_mod/2) / (trait_mean_liab_JU1200_CONTROL_P6p_wt_mod)^2
    mean(Evol_liab_JU1200_CONTROL_P6p_wt_mod)
    
    
    #JU1200_CONTROL_P6p_wt_mod data scale
    {
      
      predict_JU1200_CONTROL_P6p_wt_mod <- map(1:nrow(JU1200_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU1200_CONTROL_P6p_wt_mod %*% JU1200_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU1200_CONTROL_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_JU1200_CONTROL_P6p_wt_mod,
                      var.a = JU1200_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_CONTROL_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_CONTROL_P6p_wt_mod <- data_JU1200_CONTROL_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_JU1200_CONTROL_P6p_wt_mod <- data_JU1200_CONTROL_P6p_wt_mod[["mean.obs"]]
      va_data_JU1200_CONTROL_P6p_wt_mod <- data_JU1200_CONTROL_P6p_wt_mod[["var.a.obs"]]
      vp_data_JU1200_CONTROL_P6p_wt_mod <- data_JU1200_CONTROL_P6p_wt_mod[["var.obs"]]
      
      Evol_data_JU1200_CONTROL_P6p_wt_mod <- (va_data_JU1200_CONTROL_P6p_wt_mod/2) / (trait_mean_data_JU1200_CONTROL_P6p_wt_mod)^2
      
      mean(h2_data_JU1200_CONTROL_P6p_wt_mod) 
      mean(trait_mean_data_JU1200_CONTROL_P6p_wt_mod)
      mean(va_data_JU1200_CONTROL_P6p_wt_mod) 
      mean(vp_data_JU1200_CONTROL_P6p_wt_mod)
      mean(Evol_data_JU1200_CONTROL_P6p_wt_mod)
      
    }
    
  }
  
  #JU1200_MA_P6p_wt_mod 
  {
    
    JU1200_MA_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                     random      = ~ LineB + BlockRep,
                                     family      = "threshold",
                                     data        = Vm_JU1200_bi_MA,
                                     prior       = prior_bi_block,
                                     nitt        = 1260000,       
                                     thin        = 500,           
                                     burnin      = 10000,
                                     trunc       = TRUE,
                                     pr          = TRUE,
                                     pl          = TRUE)         
    
    saveRDS(JU1200_MA_P6p_wt_mod, file = "JU1200_MA_P6p_wt_mod.rds")
    JU1200_MA_P6p_wt_mod <- readRDS("JU1200_MA_P6p_wt_mod.rds")
    
    summary(JU1200_MA_P6p_wt_mod) 
    #plot(JU1200_MA_P6p_wt_mod)
    
    # traces and posterior densities
    pdf("JU1200_MA_P6p_wt_mod.pdf")
    plot(JU1200_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU1200_MA_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU1200_MA_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_MA_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU1200_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU1200_MA_P6p_wt_mod[["VCV"]])
    #autocorr.plot(JU1200_MA_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU1200_MA_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_MA_P6p_wt_mod <- JU1200_MA_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU1200_MA_P6p_wt_mod <- JU1200_MA_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_JU1200_MA_P6p_wt_mod <- rowSums(JU1200_MA_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_JU1200_MA_P6p_wt_mod) 
      HPDinterval(va_liab_JU1200_MA_P6p_wt_mod) 
      
      mean(vlat_JU1200_MA_P6p_wt_mod) 
      
      #variance of fixed effects
      X_JU1200_MA_P6p_wt_mod <- JU1200_MA_P6p_wt_mod[["X"]]
      beta_JU1200_MA_P6p_wt_mod <- JU1200_MA_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU1200_MA_P6p_wt_mod   <- apply(beta_JU1200_MA_P6p_wt_mod, 1, function(b) {var(as.vector(X_JU1200_MA_P6p_wt_mod %*% b))}) 
      mean(vf_JU1200_MA_P6p_wt_mod) 
      
      h2_liab_JU1200_MA_P6p_wt_mod <- va_liab_JU1200_MA_P6p_wt_mod / (vlat_JU1200_MA_P6p_wt_mod + vf_JU1200_MA_P6p_wt_mod)
      mean(h2_liab_JU1200_MA_P6p_wt_mod) 
      posterior.mode(h2_liab_JU1200_MA_P6p_wt_mod)	
      median(h2_liab_JU1200_MA_P6p_wt_mod)		
      HPDinterval(h2_liab_JU1200_MA_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU1200_MA_P6p_wt_mod <- rowMeans(JU1200_MA_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU1200_MA_P6p_wt_mod <- (va_liab_JU1200_MA_P6p_wt_mod/2) / (trait_mean_liab_JU1200_MA_P6p_wt_mod)^2
    mean(Evol_liab_JU1200_MA_P6p_wt_mod)
    
    #JU1200_MA_P6p_wt_mod data scale
    {
      
      predict_JU1200_MA_P6p_wt_mod <- map(1:nrow(JU1200_MA_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU1200_MA_P6p_wt_mod %*% JU1200_MA_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU1200_MA_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_JU1200_MA_P6p_wt_mod,
                      var.a = JU1200_MA_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_MA_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_MA_P6p_wt_mod <- data_JU1200_MA_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_JU1200_MA_P6p_wt_mod <- data_JU1200_MA_P6p_wt_mod[["mean.obs"]]
      va_data_JU1200_MA_P6p_wt_mod <- data_JU1200_MA_P6p_wt_mod[["var.a.obs"]]
      vp_data_JU1200_MA_P6p_wt_mod <- data_JU1200_MA_P6p_wt_mod[["var.obs"]]
      
      Evol_data_JU1200_MA_P6p_wt_mod <- (va_data_JU1200_MA_P6p_wt_mod/2) / (trait_mean_data_JU1200_MA_P6p_wt_mod)^2
      
      mean(h2_data_JU1200_MA_P6p_wt_mod)
      mean(trait_mean_data_JU1200_MA_P6p_wt_mod)
      mean(va_data_JU1200_MA_P6p_wt_mod)
      mean(vp_data_JU1200_MA_P6p_wt_mod)
      mean(Evol_data_JU1200_MA_P6p_wt_mod)
      
    }
    
  }
  
  #JU1200_Vm_P6p_wt_mod 
  {
    
    JU1200_Vm_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Treatment -1,
                                     random      = ~ LineB + BlockRep,
                                     family      = "threshold",
                                     data        = Vm_JU1200_data,
                                     prior       = prior_bi_block,
                                     nitt        = 1260000,       
                                     thin        = 500,           
                                     burnin      = 10000,
                                     trunc       = TRUE,
                                     pr          = TRUE,
                                     pl          = TRUE)            
    
    saveRDS(JU1200_Vm_P6p_wt_mod, file = "JU1200_Vm_P6p_wt_mod.rds")
    JU1200_Vm_P6p_wt_mod <- readRDS("JU1200_Vm_P6p_wt_mod.rds")
    
    summary(JU1200_Vm_P6p_wt_mod) 
    plotTrace(JU1200_Vm_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("JU1200_Vm_P6p_wt_mod.pdf")
    plot(JU1200_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(JU1200_Vm_P6p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU1200_Vm_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_Vm_P6p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU1200_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU1200_Vm_P6p_wt_mod[["VCV"]])
    #autocorr.plot(JU1200_Vm_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU1200_Vm_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_Vm_P6p_wt_mod <- JU1200_Vm_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU1200_Vm_P6p_wt_mod <- JU1200_Vm_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_JU1200_Vm_P6p_wt_mod <- rowSums(JU1200_Vm_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_JU1200_Vm_P6p_wt_mod) 
      HPDinterval(va_liab_JU1200_Vm_P6p_wt_mod) 
      
      mean(vlat_JU1200_Vm_P6p_wt_mod) 
      
      #variance of fixed effects
      X_JU1200_Vm_P6p_wt_mod <- JU1200_Vm_P6p_wt_mod[["X"]]
      beta_JU1200_Vm_P6p_wt_mod <- JU1200_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]
      vf_JU1200_Vm_P6p_wt_mod   <- apply(beta_JU1200_Vm_P6p_wt_mod, 1, function(b) {var(as.vector(X_JU1200_Vm_P6p_wt_mod %*% b))}) 
      mean(vf_JU1200_Vm_P6p_wt_mod) 
      
      
      h2_liab_JU1200_Vm_P6p_wt_mod <- va_liab_JU1200_Vm_P6p_wt_mod / (vlat_JU1200_Vm_P6p_wt_mod + vf_JU1200_Vm_P6p_wt_mod)
      mean(h2_liab_JU1200_Vm_P6p_wt_mod) 
      posterior.mode(h2_liab_JU1200_Vm_P6p_wt_mod)	
      median(h2_liab_JU1200_Vm_P6p_wt_mod)		
      HPDinterval(h2_liab_JU1200_Vm_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU1200_Vm_P6p_wt_mod <- ((rowMeans(JU1200_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU1200_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + JU1200_Vm_P6p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU1200_Vm_P6p_wt_mod <- (va_liab_JU1200_Vm_P6p_wt_mod/2) / (trait_mean_liab_JU1200_Vm_P6p_wt_mod)^2
    mean(Evol_liab_JU1200_Vm_P6p_wt_mod)
    
    
    #JU1200_Vm_P6p_wt_mod data scale
    {
      
      predict_JU1200_Vm_P6p_wt_mod <- map(1:nrow(JU1200_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU1200_Vm_P6p_wt_mod %*% JU1200_Vm_P6p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU1200_Vm_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_JU1200_Vm_P6p_wt_mod,
                      var.a = JU1200_Vm_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_Vm_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_Vm_P6p_wt_mod <- data_JU1200_Vm_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_JU1200_Vm_P6p_wt_mod <- data_JU1200_Vm_P6p_wt_mod[["mean.obs"]]
      va_data_JU1200_Vm_P6p_wt_mod <- data_JU1200_Vm_P6p_wt_mod[["var.a.obs"]]
      vp_data_JU1200_Vm_P6p_wt_mod <- data_JU1200_Vm_P6p_wt_mod[["var.obs"]]
      
      Evol_data_JU1200_Vm_P6p_wt_mod <- (va_data_JU1200_Vm_P6p_wt_mod/2) / (trait_mean_data_JU1200_Vm_P6p_wt_mod)^2
      
      mean(h2_data_JU1200_Vm_P6p_wt_mod)
      mean(trait_mean_data_JU1200_Vm_P6p_wt_mod)
      mean(va_data_JU1200_Vm_P6p_wt_mod)
      mean(vp_data_JU1200_Vm_P6p_wt_mod)
      mean(Evol_data_JU1200_Vm_P6p_wt_mod)
      
    }
    
  }
  
  
  
  
  
  ##---- JU1200 P7p ----
  
  
  #JU1200_CONTROL_P7p_wt_mod 
  {
    
    JU1200_CONTROL_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                          random      = ~ LineB + BlockRep,
                                          family      = "threshold",
                                          data        = Vm_JU1200_bi_CONTROL,
                                          prior       = prior_bi_block,
                                          nitt        = 1260000,       
                                          thin        = 500,           
                                          burnin      = 10000,
                                          trunc       = TRUE,
                                          pr          = TRUE,
                                          pl          = TRUE)            
    
    saveRDS(JU1200_CONTROL_P7p_wt_mod, file = "JU1200_CONTROL_P7p_wt_mod.rds")
    JU1200_CONTROL_P7p_wt_mod <- readRDS("JU1200_CONTROL_P7p_wt_mod.rds")
    
    summary(JU1200_CONTROL_P7p_wt_mod) 
    plotTrace(JU1200_CONTROL_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("JU1200_CONTROL_P7p_wt_mod.pdf")
    plot(JU1200_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU1200_CONTROL_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU1200_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU1200_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU1200_CONTROL_P7p_wt_mod[["VCV"]])
    #autocorr.plot(JU1200_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU1200_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_CONTROL_P7p_wt_mod <- JU1200_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU1200_CONTROL_P7p_wt_mod <- JU1200_CONTROL_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_JU1200_CONTROL_P7p_wt_mod <- rowSums(JU1200_CONTROL_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_JU1200_CONTROL_P7p_wt_mod) 
      HPDinterval(va_liab_JU1200_CONTROL_P7p_wt_mod) 
      
      mean(vlat_JU1200_CONTROL_P7p_wt_mod) 
      
      #variance of fixed effects
      X_JU1200_CONTROL_P7p_wt_mod <- JU1200_CONTROL_P7p_wt_mod[["X"]]
      beta_JU1200_CONTROL_P7p_wt_mod <- JU1200_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU1200_CONTROL_P7p_wt_mod   <- apply(beta_JU1200_CONTROL_P7p_wt_mod, 1, function(b) {var(as.vector(X_JU1200_CONTROL_P7p_wt_mod %*% b))}) 
      mean(vf_JU1200_CONTROL_P7p_wt_mod) 
      
      
      h2_liab_JU1200_CONTROL_P7p_wt_mod <- va_liab_JU1200_CONTROL_P7p_wt_mod / (vlat_JU1200_CONTROL_P7p_wt_mod + vf_JU1200_CONTROL_P7p_wt_mod)
      mean(h2_liab_JU1200_CONTROL_P7p_wt_mod) 
      posterior.mode(h2_liab_JU1200_CONTROL_P7p_wt_mod)	
      median(h2_liab_JU1200_CONTROL_P7p_wt_mod)		
      HPDinterval(h2_liab_JU1200_CONTROL_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU1200_CONTROL_P7p_wt_mod <- rowMeans(JU1200_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU1200_CONTROL_P7p_wt_mod <- (va_liab_JU1200_CONTROL_P7p_wt_mod/2) / (trait_mean_liab_JU1200_CONTROL_P7p_wt_mod)^2
    mean(Evol_liab_JU1200_CONTROL_P7p_wt_mod)
    
    
    #JU1200_CONTROL_P7p_wt_mod data scale
    {
      
      predict_JU1200_CONTROL_P7p_wt_mod <- map(1:nrow(JU1200_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU1200_CONTROL_P7p_wt_mod %*% JU1200_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_JU1200_CONTROL_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_JU1200_CONTROL_P7p_wt_mod,
                      var.a = JU1200_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_CONTROL_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_CONTROL_P7p_wt_mod <- data_JU1200_CONTROL_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_JU1200_CONTROL_P7p_wt_mod <- data_JU1200_CONTROL_P7p_wt_mod[["mean.obs"]]
      va_data_JU1200_CONTROL_P7p_wt_mod <- data_JU1200_CONTROL_P7p_wt_mod[["var.a.obs"]]
      vp_data_JU1200_CONTROL_P7p_wt_mod <- data_JU1200_CONTROL_P7p_wt_mod[["var.obs"]]
      
      Evol_data_JU1200_CONTROL_P7p_wt_mod <- (va_data_JU1200_CONTROL_P7p_wt_mod/2) / (trait_mean_data_JU1200_CONTROL_P7p_wt_mod)^2
      
      mean(h2_data_JU1200_CONTROL_P7p_wt_mod) 
      mean(trait_mean_data_JU1200_CONTROL_P7p_wt_mod)
      mean(va_data_JU1200_CONTROL_P7p_wt_mod) 
      mean(vp_data_JU1200_CONTROL_P7p_wt_mod)
      mean(Evol_data_JU1200_CONTROL_P7p_wt_mod)
      
    }
    
  }
  
  #JU1200_MA_P7p_wt_mod 
  {
    
    JU1200_MA_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                     random      = ~ LineB + BlockRep,
                                     family      = "threshold",
                                     data        = Vm_JU1200_bi_MA,
                                     prior       = prior_bi_block,
                                     nitt        = 1260000,       
                                     thin        = 500,           
                                     burnin      = 10000,
                                     trunc       = TRUE,
                                     pr          = TRUE,
                                     pl          = TRUE)         
    
    saveRDS(JU1200_MA_P7p_wt_mod, file = "JU1200_MA_P7p_wt_mod.rds")
    JU1200_MA_P7p_wt_mod <- readRDS("JU1200_MA_P7p_wt_mod.rds")
    
    summary(JU1200_MA_P7p_wt_mod) 
    #plot(JU1200_MA_P7p_wt_mod)
    
    # traces and posterior densities
    pdf("JU1200_MA_P7p_wt_mod.pdf")
    plot(JU1200_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(JU1200_MA_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(JU1200_MA_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_MA_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(JU1200_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(JU1200_MA_P7p_wt_mod[["VCV"]])
    #autocorr.plot(JU1200_MA_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(JU1200_MA_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_MA_P7p_wt_mod <- JU1200_MA_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU1200_MA_P7p_wt_mod <- JU1200_MA_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_JU1200_MA_P7p_wt_mod <- rowSums(JU1200_MA_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_JU1200_MA_P7p_wt_mod) 
      HPDinterval(va_liab_JU1200_MA_P7p_wt_mod) 
      
      mean(vlat_JU1200_MA_P7p_wt_mod) 
      
      #variance of fixed effects
      X_JU1200_MA_P7p_wt_mod <- JU1200_MA_P7p_wt_mod[["X"]]
      beta_JU1200_MA_P7p_wt_mod <- JU1200_MA_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_JU1200_MA_P7p_wt_mod   <- apply(beta_JU1200_MA_P7p_wt_mod, 1, function(b) {var(as.vector(X_JU1200_MA_P7p_wt_mod %*% b))}) 
      mean(vf_JU1200_MA_P7p_wt_mod) 
      
      h2_liab_JU1200_MA_P7p_wt_mod <- va_liab_JU1200_MA_P7p_wt_mod / (vlat_JU1200_MA_P7p_wt_mod + vf_JU1200_MA_P7p_wt_mod)
      mean(h2_liab_JU1200_MA_P7p_wt_mod) 
      posterior.mode(h2_liab_JU1200_MA_P7p_wt_mod)	
      median(h2_liab_JU1200_MA_P7p_wt_mod)		
      HPDinterval(h2_liab_JU1200_MA_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_JU1200_MA_P7p_wt_mod <- rowMeans(JU1200_MA_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_JU1200_MA_P7p_wt_mod <- (va_liab_JU1200_MA_P7p_wt_mod/2) / (trait_mean_liab_JU1200_MA_P7p_wt_mod)^2
    mean(Evol_liab_JU1200_MA_P7p_wt_mod)
    
    #JU1200_MA_P7p_wt_mod data scale
    {
      
      predict_JU1200_MA_P7p_wt_mod <- map(1:nrow(JU1200_MA_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_JU1200_MA_P7p_wt_mod %*% JU1200_MA_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_JU1200_MA_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_JU1200_MA_P7p_wt_mod,
                      var.a = JU1200_MA_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_MA_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_MA_P7p_wt_mod <- data_JU1200_MA_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_JU1200_MA_P7p_wt_mod <- data_JU1200_MA_P7p_wt_mod[["mean.obs"]]
      va_data_JU1200_MA_P7p_wt_mod <- data_JU1200_MA_P7p_wt_mod[["var.a.obs"]]
      vp_data_JU1200_MA_P7p_wt_mod <- data_JU1200_MA_P7p_wt_mod[["var.obs"]]
      
      Evol_data_JU1200_MA_P7p_wt_mod <- (va_data_JU1200_MA_P7p_wt_mod/2) / (trait_mean_data_JU1200_MA_P7p_wt_mod)^2
      
      mean(h2_data_JU1200_MA_P7p_wt_mod)
      mean(trait_mean_data_JU1200_MA_P7p_wt_mod)
      mean(va_data_JU1200_MA_P7p_wt_mod)
      mean(vp_data_JU1200_MA_P7p_wt_mod)
      mean(Evol_data_JU1200_MA_P7p_wt_mod)
      
    }
    
  }
  
  #JU1200_Vm_P7p_wt_mod 
  {
    
    JU1200_Vm_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Treatment -1,
                                     random      = ~ LineB + BlockRep,
                                     family      = "threshold",
                                     data        = Vm_JU1200_data,
                                     prior       = prior_bi_block,
                                     nitt        = 1260000,       
                                     thin        = 500,           
                                     burnin      = 10000,
                                     trunc       = TRUE,
                                     pr          = TRUE,
                                     pl          = TRUE)            
    
    saveRDS(JU1200_Vm_P7p_wt_mod, file = "JU1200_Vm_P7p_wt_mod.rds")
    JU1200_Vm_P7p_wt_mod <- readRDS("JU1200_Vm_P7p_wt_mod.rds")
    
    summary(JU1200_Vm_P7p_wt_mod) 
    plotTrace(JU1200_Vm_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("JU1200_Vm_P7p_wt_mod.pdf")
    plot(JU1200_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(JU1200_Vm_P7p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(JU1200_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(JU1200_Vm_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(JU1200_Vm_P7p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(JU1200_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(JU1200_Vm_P7p_wt_mod[["VCV"]])
    #autocorr.plot(JU1200_Vm_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(JU1200_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(JU1200_Vm_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_JU1200_Vm_P7p_wt_mod <- JU1200_Vm_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_JU1200_Vm_P7p_wt_mod <- JU1200_Vm_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_JU1200_Vm_P7p_wt_mod <- rowSums(JU1200_Vm_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_JU1200_Vm_P7p_wt_mod) 
      HPDinterval(va_liab_JU1200_Vm_P7p_wt_mod) 
      
      mean(vlat_JU1200_Vm_P7p_wt_mod) 
      
      #variance of fixed effects
      X_JU1200_Vm_P7p_wt_mod <- JU1200_Vm_P7p_wt_mod[["X"]]
      beta_JU1200_Vm_P7p_wt_mod <- JU1200_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]
      vf_JU1200_Vm_P7p_wt_mod   <- apply(beta_JU1200_Vm_P7p_wt_mod, 1, function(b) {var(as.vector(X_JU1200_Vm_P7p_wt_mod %*% b))}) 
      mean(vf_JU1200_Vm_P7p_wt_mod) 
      
      
      h2_liab_JU1200_Vm_P7p_wt_mod <- va_liab_JU1200_Vm_P7p_wt_mod / (vlat_JU1200_Vm_P7p_wt_mod + vf_JU1200_Vm_P7p_wt_mod)
      mean(h2_liab_JU1200_Vm_P7p_wt_mod) 
      posterior.mode(h2_liab_JU1200_Vm_P7p_wt_mod)	
      median(h2_liab_JU1200_Vm_P7p_wt_mod)		
      HPDinterval(h2_liab_JU1200_Vm_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_JU1200_Vm_P7p_wt_mod <- ((rowMeans(JU1200_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(JU1200_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + JU1200_Vm_P7p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_JU1200_Vm_P7p_wt_mod <- (va_liab_JU1200_Vm_P7p_wt_mod/2) / (trait_mean_liab_JU1200_Vm_P7p_wt_mod)^2
    mean(Evol_liab_JU1200_Vm_P7p_wt_mod)
    
    
    #JU1200_Vm_P7p_wt_mod data scale
    {
      
      predict_JU1200_Vm_P7p_wt_mod <- map(1:nrow(JU1200_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_JU1200_Vm_P7p_wt_mod %*% JU1200_Vm_P7p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_JU1200_Vm_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_JU1200_Vm_P7p_wt_mod,
                      var.a = JU1200_Vm_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(JU1200_Vm_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_JU1200_Vm_P7p_wt_mod <- data_JU1200_Vm_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_JU1200_Vm_P7p_wt_mod <- data_JU1200_Vm_P7p_wt_mod[["mean.obs"]]
      va_data_JU1200_Vm_P7p_wt_mod <- data_JU1200_Vm_P7p_wt_mod[["var.a.obs"]]
      vp_data_JU1200_Vm_P7p_wt_mod <- data_JU1200_Vm_P7p_wt_mod[["var.obs"]]
      
      Evol_data_JU1200_Vm_P7p_wt_mod <- (va_data_JU1200_Vm_P7p_wt_mod/2) / (trait_mean_data_JU1200_Vm_P7p_wt_mod)^2
      
      mean(h2_data_JU1200_Vm_P7p_wt_mod)
      mean(trait_mean_data_JU1200_Vm_P7p_wt_mod)
      mean(va_data_JU1200_Vm_P7p_wt_mod)
      mean(vp_data_JU1200_Vm_P7p_wt_mod)
      mean(Evol_data_JU1200_Vm_P7p_wt_mod)
      
    }
    
  }
  
  
  
  
  }  
  

#PB306----
{
  Vm_PB306_bi_CONTROL <- subset(Vm_PB306_data, Treatment =="CONTROL")
  Vm_PB306_bi_MA <- subset(Vm_PB306_data, Treatment =="MA")
  
  
  ##---- PB306 P3p ----
  
  
  #PB306_CONTROL_P3p_SS_mod 
  {
    
    PB306_CONTROL_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_PB306_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(PB306_CONTROL_P3p_SS_mod, file = "PB306_CONTROL_P3p_SS_mod.rds")
    PB306_CONTROL_P3p_SS_mod <- readRDS("PB306_CONTROL_P3p_SS_mod.rds")
    
    summary(PB306_CONTROL_P3p_SS_mod) 
    plotTrace(PB306_CONTROL_P3p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PB306_CONTROL_P3p_SS_mod.pdf")
    plot(PB306_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(PB306_CONTROL_P3p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB306_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB306_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB306_CONTROL_P3p_SS_mod[["VCV"]])
    #autocorr.plot(PB306_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB306_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_CONTROL_P3p_SS_mod <- PB306_CONTROL_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB306_CONTROL_P3p_SS_mod <- PB306_CONTROL_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_PB306_CONTROL_P3p_SS_mod <- rowSums(PB306_CONTROL_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_PB306_CONTROL_P3p_SS_mod) 
      HPDinterval(va_liab_PB306_CONTROL_P3p_SS_mod) 
      
      mean(vlat_PB306_CONTROL_P3p_SS_mod) 
      
      #variance of fixed effects
      X_PB306_CONTROL_P3p_SS_mod <- PB306_CONTROL_P3p_SS_mod[["X"]]
      beta_PB306_CONTROL_P3p_SS_mod <- PB306_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]
      vf_PB306_CONTROL_P3p_SS_mod   <- apply(beta_PB306_CONTROL_P3p_SS_mod, 1, function(b) {var(as.vector(X_PB306_CONTROL_P3p_SS_mod %*% b))}) 
      mean(vf_PB306_CONTROL_P3p_SS_mod) 
      
      
      h2_liab_PB306_CONTROL_P3p_SS_mod <- va_liab_PB306_CONTROL_P3p_SS_mod / (vlat_PB306_CONTROL_P3p_SS_mod + vf_PB306_CONTROL_P3p_SS_mod)
      mean(h2_liab_PB306_CONTROL_P3p_SS_mod) 
      posterior.mode(h2_liab_PB306_CONTROL_P3p_SS_mod)	
      median(h2_liab_PB306_CONTROL_P3p_SS_mod)		
      HPDinterval(h2_liab_PB306_CONTROL_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB306_CONTROL_P3p_SS_mod <- rowMeans(PB306_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB306_CONTROL_P3p_SS_mod <- (va_liab_PB306_CONTROL_P3p_SS_mod/2) / (trait_mean_liab_PB306_CONTROL_P3p_SS_mod)^2
    mean(Evol_liab_PB306_CONTROL_P3p_SS_mod)
    
    
    #PB306_CONTROL_P3p_SS_mod data scale
    {
      
      predict_PB306_CONTROL_P3p_SS_mod <- map(1:nrow(PB306_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB306_CONTROL_P3p_SS_mod %*% PB306_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PB306_CONTROL_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_PB306_CONTROL_P3p_SS_mod,
                      var.a = PB306_CONTROL_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_CONTROL_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_CONTROL_P3p_SS_mod <- data_PB306_CONTROL_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_PB306_CONTROL_P3p_SS_mod <- data_PB306_CONTROL_P3p_SS_mod[["mean.obs"]]
      va_data_PB306_CONTROL_P3p_SS_mod <- data_PB306_CONTROL_P3p_SS_mod[["var.a.obs"]]
      vp_data_PB306_CONTROL_P3p_SS_mod <- data_PB306_CONTROL_P3p_SS_mod[["var.obs"]]
      
      Evol_data_PB306_CONTROL_P3p_SS_mod <- (va_data_PB306_CONTROL_P3p_SS_mod/2) / (trait_mean_data_PB306_CONTROL_P3p_SS_mod)^2
      
      mean(h2_data_PB306_CONTROL_P3p_SS_mod) 
      mean(trait_mean_data_PB306_CONTROL_P3p_SS_mod)
      mean(va_data_PB306_CONTROL_P3p_SS_mod) 
      mean(vp_data_PB306_CONTROL_P3p_SS_mod)
      mean(Evol_data_PB306_CONTROL_P3p_SS_mod)
      
    }
    
  }
  
  #PB306_MA_P3p_SS_mod 
  {
    
    PB306_MA_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_PB306_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(PB306_MA_P3p_SS_mod, file = "PB306_MA_P3p_SS_mod.rds")
    PB306_MA_P3p_SS_mod <- readRDS("PB306_MA_P3p_SS_mod.rds")
    
    summary(PB306_MA_P3p_SS_mod) 
    #plot(PB306_MA_P3p_SS_mod)
    
    # traces and posterior densities
    pdf("PB306_MA_P3p_SS_mod.pdf")
    plot(PB306_MA_P3p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(PB306_MA_P3p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_MA_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB306_MA_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_MA_P3p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB306_MA_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB306_MA_P3p_SS_mod[["VCV"]])
    #autocorr.plot(PB306_MA_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_MA_P3p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB306_MA_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_MA_P3p_SS_mod <- PB306_MA_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB306_MA_P3p_SS_mod <- PB306_MA_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_PB306_MA_P3p_SS_mod <- rowSums(PB306_MA_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_PB306_MA_P3p_SS_mod) 
      HPDinterval(va_liab_PB306_MA_P3p_SS_mod) 
      
      mean(vlat_PB306_MA_P3p_SS_mod) 
      
      #variance of fixed effects
      X_PB306_MA_P3p_SS_mod <- PB306_MA_P3p_SS_mod[["X"]]
      beta_PB306_MA_P3p_SS_mod <- PB306_MA_P3p_SS_mod[["Sol"]][,c(1:4)]
      vf_PB306_MA_P3p_SS_mod   <- apply(beta_PB306_MA_P3p_SS_mod, 1, function(b) {var(as.vector(X_PB306_MA_P3p_SS_mod %*% b))}) 
      mean(vf_PB306_MA_P3p_SS_mod) 
      
      h2_liab_PB306_MA_P3p_SS_mod <- va_liab_PB306_MA_P3p_SS_mod / (vlat_PB306_MA_P3p_SS_mod + vf_PB306_MA_P3p_SS_mod)
      mean(h2_liab_PB306_MA_P3p_SS_mod) 
      posterior.mode(h2_liab_PB306_MA_P3p_SS_mod)	
      median(h2_liab_PB306_MA_P3p_SS_mod)		
      HPDinterval(h2_liab_PB306_MA_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB306_MA_P3p_SS_mod <- rowMeans(PB306_MA_P3p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB306_MA_P3p_SS_mod <- (va_liab_PB306_MA_P3p_SS_mod/2) / (trait_mean_liab_PB306_MA_P3p_SS_mod)^2
    mean(Evol_liab_PB306_MA_P3p_SS_mod)
    
    #PB306_MA_P3p_SS_mod data scale
    {
      
      predict_PB306_MA_P3p_SS_mod <- map(1:nrow(PB306_MA_P3p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB306_MA_P3p_SS_mod %*% PB306_MA_P3p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PB306_MA_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_PB306_MA_P3p_SS_mod,
                      var.a = PB306_MA_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_MA_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_MA_P3p_SS_mod <- data_PB306_MA_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_PB306_MA_P3p_SS_mod <- data_PB306_MA_P3p_SS_mod[["mean.obs"]]
      va_data_PB306_MA_P3p_SS_mod <- data_PB306_MA_P3p_SS_mod[["var.a.obs"]]
      vp_data_PB306_MA_P3p_SS_mod <- data_PB306_MA_P3p_SS_mod[["var.obs"]]
      
      Evol_data_PB306_MA_P3p_SS_mod <- (va_data_PB306_MA_P3p_SS_mod/2) / (trait_mean_data_PB306_MA_P3p_SS_mod)^2
      
      mean(h2_data_PB306_MA_P3p_SS_mod)
      mean(trait_mean_data_PB306_MA_P3p_SS_mod)
      mean(va_data_PB306_MA_P3p_SS_mod)
      mean(vp_data_PB306_MA_P3p_SS_mod)
      mean(Evol_data_PB306_MA_P3p_SS_mod)
      
    }
    
  }
  
  #PB306_Vm_P3p_SS_mod 
  {
    
    PB306_Vm_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_PB306_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(PB306_Vm_P3p_SS_mod, file = "PB306_Vm_P3p_SS_mod.rds")
    PB306_Vm_P3p_SS_mod <- readRDS("PB306_Vm_P3p_SS_mod.rds")
    
    summary(PB306_Vm_P3p_SS_mod) 
    plotTrace(PB306_Vm_P3p_SS_mod$Sol)
    
    # traces and posterior densities
    pdf("PB306_Vm_P3p_SS_mod.pdf")
    plot(PB306_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(PB306_Vm_P3p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PB306_Vm_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_Vm_P3p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PB306_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PB306_Vm_P3p_SS_mod[["VCV"]])
    #autocorr.plot(PB306_Vm_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PB306_Vm_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_Vm_P3p_SS_mod <- PB306_Vm_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB306_Vm_P3p_SS_mod <- PB306_Vm_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_PB306_Vm_P3p_SS_mod <- rowSums(PB306_Vm_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_PB306_Vm_P3p_SS_mod) 
      HPDinterval(va_liab_PB306_Vm_P3p_SS_mod) 
      
      mean(vlat_PB306_Vm_P3p_SS_mod) 
      
      #variance of fixed effects
      X_PB306_Vm_P3p_SS_mod <- PB306_Vm_P3p_SS_mod[["X"]]
      beta_PB306_Vm_P3p_SS_mod <- PB306_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]
      vf_PB306_Vm_P3p_SS_mod   <- apply(beta_PB306_Vm_P3p_SS_mod, 1, function(b) {var(as.vector(X_PB306_Vm_P3p_SS_mod %*% b))}) 
      mean(vf_PB306_Vm_P3p_SS_mod) 
      
      
      h2_liab_PB306_Vm_P3p_SS_mod <- va_liab_PB306_Vm_P3p_SS_mod / (vlat_PB306_Vm_P3p_SS_mod + vf_PB306_Vm_P3p_SS_mod)
      mean(h2_liab_PB306_Vm_P3p_SS_mod) 
      posterior.mode(h2_liab_PB306_Vm_P3p_SS_mod)	
      median(h2_liab_PB306_Vm_P3p_SS_mod)		
      HPDinterval(h2_liab_PB306_Vm_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PB306_Vm_P3p_SS_mod <- ((rowMeans(PB306_Vm_P3p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(PB306_Vm_P3p_SS_mod[["Sol"]][,c(1:4)]) + PB306_Vm_P3p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PB306_Vm_P3p_SS_mod <- (va_liab_PB306_Vm_P3p_SS_mod/2) / (trait_mean_liab_PB306_Vm_P3p_SS_mod)^2
    mean(Evol_liab_PB306_Vm_P3p_SS_mod)
    
    
    #PB306_Vm_P3p_SS_mod data scale
    {
      
      predict_PB306_Vm_P3p_SS_mod <- map(1:nrow(PB306_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PB306_Vm_P3p_SS_mod %*% PB306_Vm_P3p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PB306_Vm_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_PB306_Vm_P3p_SS_mod,
                      var.a = PB306_Vm_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_Vm_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_Vm_P3p_SS_mod <- data_PB306_Vm_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_PB306_Vm_P3p_SS_mod <- data_PB306_Vm_P3p_SS_mod[["mean.obs"]]
      va_data_PB306_Vm_P3p_SS_mod <- data_PB306_Vm_P3p_SS_mod[["var.a.obs"]]
      vp_data_PB306_Vm_P3p_SS_mod <- data_PB306_Vm_P3p_SS_mod[["var.obs"]]
      
      Evol_data_PB306_Vm_P3p_SS_mod <- (va_data_PB306_Vm_P3p_SS_mod/2) / (trait_mean_data_PB306_Vm_P3p_SS_mod)^2
      
      mean(h2_data_PB306_Vm_P3p_SS_mod)
      mean(trait_mean_data_PB306_Vm_P3p_SS_mod)
      mean(va_data_PB306_Vm_P3p_SS_mod)
      mean(vp_data_PB306_Vm_P3p_SS_mod)
      mean(Evol_data_PB306_Vm_P3p_SS_mod)
      
    }
    
  }
  
  
 
  
  
  ##---- PB306 P4p ----
  
  
  #PB306_CONTROL_P4p_SS_mod 
  {
    
    PB306_CONTROL_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_PB306_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(PB306_CONTROL_P4p_SS_mod, file = "PB306_CONTROL_P4p_SS_mod.rds")
    PB306_CONTROL_P4p_SS_mod <- readRDS("PB306_CONTROL_P4p_SS_mod.rds")
    
    summary(PB306_CONTROL_P4p_SS_mod) 
    plotTrace(PB306_CONTROL_P4p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PB306_CONTROL_P4p_SS_mod.pdf")
    plot(PB306_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(PB306_CONTROL_P4p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB306_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB306_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB306_CONTROL_P4p_SS_mod[["VCV"]])
    #autocorr.plot(PB306_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB306_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_CONTROL_P4p_SS_mod <- PB306_CONTROL_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB306_CONTROL_P4p_SS_mod <- PB306_CONTROL_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_PB306_CONTROL_P4p_SS_mod <- rowSums(PB306_CONTROL_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_PB306_CONTROL_P4p_SS_mod) 
      HPDinterval(va_liab_PB306_CONTROL_P4p_SS_mod) 
      
      mean(vlat_PB306_CONTROL_P4p_SS_mod) 
      
      #variance of fixed effects
      X_PB306_CONTROL_P4p_SS_mod <- PB306_CONTROL_P4p_SS_mod[["X"]]
      beta_PB306_CONTROL_P4p_SS_mod <- PB306_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]
      vf_PB306_CONTROL_P4p_SS_mod   <- apply(beta_PB306_CONTROL_P4p_SS_mod, 1, function(b) {var(as.vector(X_PB306_CONTROL_P4p_SS_mod %*% b))}) 
      mean(vf_PB306_CONTROL_P4p_SS_mod) 
      
      
      h2_liab_PB306_CONTROL_P4p_SS_mod <- va_liab_PB306_CONTROL_P4p_SS_mod / (vlat_PB306_CONTROL_P4p_SS_mod + vf_PB306_CONTROL_P4p_SS_mod)
      mean(h2_liab_PB306_CONTROL_P4p_SS_mod) 
      posterior.mode(h2_liab_PB306_CONTROL_P4p_SS_mod)	
      median(h2_liab_PB306_CONTROL_P4p_SS_mod)		
      HPDinterval(h2_liab_PB306_CONTROL_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB306_CONTROL_P4p_SS_mod <- rowMeans(PB306_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB306_CONTROL_P4p_SS_mod <- (va_liab_PB306_CONTROL_P4p_SS_mod/2) / (trait_mean_liab_PB306_CONTROL_P4p_SS_mod)^2
    mean(Evol_liab_PB306_CONTROL_P4p_SS_mod)
    
    
    #PB306_CONTROL_P4p_SS_mod data scale
    {
      
      predict_PB306_CONTROL_P4p_SS_mod <- map(1:nrow(PB306_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB306_CONTROL_P4p_SS_mod %*% PB306_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PB306_CONTROL_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_PB306_CONTROL_P4p_SS_mod,
                      var.a = PB306_CONTROL_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_CONTROL_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_CONTROL_P4p_SS_mod <- data_PB306_CONTROL_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_PB306_CONTROL_P4p_SS_mod <- data_PB306_CONTROL_P4p_SS_mod[["mean.obs"]]
      va_data_PB306_CONTROL_P4p_SS_mod <- data_PB306_CONTROL_P4p_SS_mod[["var.a.obs"]]
      vp_data_PB306_CONTROL_P4p_SS_mod <- data_PB306_CONTROL_P4p_SS_mod[["var.obs"]]
      
      Evol_data_PB306_CONTROL_P4p_SS_mod <- (va_data_PB306_CONTROL_P4p_SS_mod/2) / (trait_mean_data_PB306_CONTROL_P4p_SS_mod)^2
      
      mean(h2_data_PB306_CONTROL_P4p_SS_mod) 
      mean(trait_mean_data_PB306_CONTROL_P4p_SS_mod)
      mean(va_data_PB306_CONTROL_P4p_SS_mod) 
      mean(vp_data_PB306_CONTROL_P4p_SS_mod)
      mean(Evol_data_PB306_CONTROL_P4p_SS_mod)
      
    }
    
  }
  
  #PB306_MA_P4p_SS_mod 
  {
    
    PB306_MA_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_PB306_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(PB306_MA_P4p_SS_mod, file = "PB306_MA_P4p_SS_mod.rds")
    PB306_MA_P4p_SS_mod <- readRDS("PB306_MA_P4p_SS_mod.rds")
    
    summary(PB306_MA_P4p_SS_mod) 
    #plot(PB306_MA_P4p_SS_mod)
    
    # traces and posterior densities
    pdf("PB306_MA_P4p_SS_mod.pdf")
    plot(PB306_MA_P4p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(PB306_MA_P4p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_MA_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB306_MA_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_MA_P4p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB306_MA_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB306_MA_P4p_SS_mod[["VCV"]])
    #autocorr.plot(PB306_MA_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_MA_P4p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB306_MA_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_MA_P4p_SS_mod <- PB306_MA_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB306_MA_P4p_SS_mod <- PB306_MA_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_PB306_MA_P4p_SS_mod <- rowSums(PB306_MA_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_PB306_MA_P4p_SS_mod) 
      HPDinterval(va_liab_PB306_MA_P4p_SS_mod) 
      
      mean(vlat_PB306_MA_P4p_SS_mod) 
      
      #variance of fixed effects
      X_PB306_MA_P4p_SS_mod <- PB306_MA_P4p_SS_mod[["X"]]
      beta_PB306_MA_P4p_SS_mod <- PB306_MA_P4p_SS_mod[["Sol"]][,c(1:4)]
      vf_PB306_MA_P4p_SS_mod   <- apply(beta_PB306_MA_P4p_SS_mod, 1, function(b) {var(as.vector(X_PB306_MA_P4p_SS_mod %*% b))}) 
      mean(vf_PB306_MA_P4p_SS_mod) 
      
      h2_liab_PB306_MA_P4p_SS_mod <- va_liab_PB306_MA_P4p_SS_mod / (vlat_PB306_MA_P4p_SS_mod + vf_PB306_MA_P4p_SS_mod)
      mean(h2_liab_PB306_MA_P4p_SS_mod) 
      posterior.mode(h2_liab_PB306_MA_P4p_SS_mod)	
      median(h2_liab_PB306_MA_P4p_SS_mod)		
      HPDinterval(h2_liab_PB306_MA_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB306_MA_P4p_SS_mod <- rowMeans(PB306_MA_P4p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB306_MA_P4p_SS_mod <- (va_liab_PB306_MA_P4p_SS_mod/2) / (trait_mean_liab_PB306_MA_P4p_SS_mod)^2
    mean(Evol_liab_PB306_MA_P4p_SS_mod)
    
    #PB306_MA_P4p_SS_mod data scale
    {
      
      predict_PB306_MA_P4p_SS_mod <- map(1:nrow(PB306_MA_P4p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB306_MA_P4p_SS_mod %*% PB306_MA_P4p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PB306_MA_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_PB306_MA_P4p_SS_mod,
                      var.a = PB306_MA_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_MA_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_MA_P4p_SS_mod <- data_PB306_MA_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_PB306_MA_P4p_SS_mod <- data_PB306_MA_P4p_SS_mod[["mean.obs"]]
      va_data_PB306_MA_P4p_SS_mod <- data_PB306_MA_P4p_SS_mod[["var.a.obs"]]
      vp_data_PB306_MA_P4p_SS_mod <- data_PB306_MA_P4p_SS_mod[["var.obs"]]
      
      Evol_data_PB306_MA_P4p_SS_mod <- (va_data_PB306_MA_P4p_SS_mod/2) / (trait_mean_data_PB306_MA_P4p_SS_mod)^2
      
      mean(h2_data_PB306_MA_P4p_SS_mod)
      mean(trait_mean_data_PB306_MA_P4p_SS_mod)
      mean(va_data_PB306_MA_P4p_SS_mod)
      mean(vp_data_PB306_MA_P4p_SS_mod)
      mean(Evol_data_PB306_MA_P4p_SS_mod)
      
    }
    
  }
  
  #PB306_Vm_P4p_SS_mod 
  {
    
    PB306_Vm_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_PB306_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(PB306_Vm_P4p_SS_mod, file = "PB306_Vm_P4p_SS_mod.rds")
    PB306_Vm_P4p_SS_mod <- readRDS("PB306_Vm_P4p_SS_mod.rds")
    
    summary(PB306_Vm_P4p_SS_mod) 
    plotTrace(PB306_Vm_P4p_SS_mod$Sol)
    
    # traces and posterior densities
    pdf("PB306_Vm_P4p_SS_mod.pdf")
    plot(PB306_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(PB306_Vm_P4p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PB306_Vm_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_Vm_P4p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PB306_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PB306_Vm_P4p_SS_mod[["VCV"]])
    #autocorr.plot(PB306_Vm_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PB306_Vm_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_Vm_P4p_SS_mod <- PB306_Vm_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB306_Vm_P4p_SS_mod <- PB306_Vm_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_PB306_Vm_P4p_SS_mod <- rowSums(PB306_Vm_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_PB306_Vm_P4p_SS_mod) 
      HPDinterval(va_liab_PB306_Vm_P4p_SS_mod) 
      
      mean(vlat_PB306_Vm_P4p_SS_mod) 
      
      #variance of fixed effects
      X_PB306_Vm_P4p_SS_mod <- PB306_Vm_P4p_SS_mod[["X"]]
      beta_PB306_Vm_P4p_SS_mod <- PB306_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]
      vf_PB306_Vm_P4p_SS_mod   <- apply(beta_PB306_Vm_P4p_SS_mod, 1, function(b) {var(as.vector(X_PB306_Vm_P4p_SS_mod %*% b))}) 
      mean(vf_PB306_Vm_P4p_SS_mod) 
      
      
      h2_liab_PB306_Vm_P4p_SS_mod <- va_liab_PB306_Vm_P4p_SS_mod / (vlat_PB306_Vm_P4p_SS_mod + vf_PB306_Vm_P4p_SS_mod)
      mean(h2_liab_PB306_Vm_P4p_SS_mod) 
      posterior.mode(h2_liab_PB306_Vm_P4p_SS_mod)	
      median(h2_liab_PB306_Vm_P4p_SS_mod)		
      HPDinterval(h2_liab_PB306_Vm_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PB306_Vm_P4p_SS_mod <- ((rowMeans(PB306_Vm_P4p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(PB306_Vm_P4p_SS_mod[["Sol"]][,c(1:4)]) + PB306_Vm_P4p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PB306_Vm_P4p_SS_mod <- (va_liab_PB306_Vm_P4p_SS_mod/2) / (trait_mean_liab_PB306_Vm_P4p_SS_mod)^2
    mean(Evol_liab_PB306_Vm_P4p_SS_mod)
    
    
    #PB306_Vm_P4p_SS_mod data scale
    {
      
      predict_PB306_Vm_P4p_SS_mod <- map(1:nrow(PB306_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PB306_Vm_P4p_SS_mod %*% PB306_Vm_P4p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PB306_Vm_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_PB306_Vm_P4p_SS_mod,
                      var.a = PB306_Vm_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_Vm_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_Vm_P4p_SS_mod <- data_PB306_Vm_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_PB306_Vm_P4p_SS_mod <- data_PB306_Vm_P4p_SS_mod[["mean.obs"]]
      va_data_PB306_Vm_P4p_SS_mod <- data_PB306_Vm_P4p_SS_mod[["var.a.obs"]]
      vp_data_PB306_Vm_P4p_SS_mod <- data_PB306_Vm_P4p_SS_mod[["var.obs"]]
      
      Evol_data_PB306_Vm_P4p_SS_mod <- (va_data_PB306_Vm_P4p_SS_mod/2) / (trait_mean_data_PB306_Vm_P4p_SS_mod)^2
      
      mean(h2_data_PB306_Vm_P4p_SS_mod)
      mean(trait_mean_data_PB306_Vm_P4p_SS_mod)
      mean(va_data_PB306_Vm_P4p_SS_mod)
      mean(vp_data_PB306_Vm_P4p_SS_mod)
      mean(Evol_data_PB306_Vm_P4p_SS_mod)
      
    }
    
  }
  
  
  
  
  
  ##---- PB306 P8p ----
  
  
  #PB306_CONTROL_P8p_SS_mod 
  {
    
    PB306_CONTROL_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_PB306_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(PB306_CONTROL_P8p_SS_mod, file = "PB306_CONTROL_P8p_SS_mod.rds")
    PB306_CONTROL_P8p_SS_mod <- readRDS("PB306_CONTROL_P8p_SS_mod.rds")
    
    summary(PB306_CONTROL_P8p_SS_mod) 
    plotTrace(PB306_CONTROL_P8p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PB306_CONTROL_P8p_SS_mod.pdf")
    plot(PB306_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(PB306_CONTROL_P8p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB306_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB306_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB306_CONTROL_P8p_SS_mod[["VCV"]])
    #autocorr.plot(PB306_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB306_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_CONTROL_P8p_SS_mod <- PB306_CONTROL_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB306_CONTROL_P8p_SS_mod <- PB306_CONTROL_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_PB306_CONTROL_P8p_SS_mod <- rowSums(PB306_CONTROL_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_PB306_CONTROL_P8p_SS_mod) 
      HPDinterval(va_liab_PB306_CONTROL_P8p_SS_mod) 
      
      mean(vlat_PB306_CONTROL_P8p_SS_mod) 
      
      #variance of fixed effects
      X_PB306_CONTROL_P8p_SS_mod <- PB306_CONTROL_P8p_SS_mod[["X"]]
      beta_PB306_CONTROL_P8p_SS_mod <- PB306_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]
      vf_PB306_CONTROL_P8p_SS_mod   <- apply(beta_PB306_CONTROL_P8p_SS_mod, 1, function(b) {var(as.vector(X_PB306_CONTROL_P8p_SS_mod %*% b))}) 
      mean(vf_PB306_CONTROL_P8p_SS_mod) 
      
      
      h2_liab_PB306_CONTROL_P8p_SS_mod <- va_liab_PB306_CONTROL_P8p_SS_mod / (vlat_PB306_CONTROL_P8p_SS_mod + vf_PB306_CONTROL_P8p_SS_mod)
      mean(h2_liab_PB306_CONTROL_P8p_SS_mod) 
      posterior.mode(h2_liab_PB306_CONTROL_P8p_SS_mod)	
      median(h2_liab_PB306_CONTROL_P8p_SS_mod)		
      HPDinterval(h2_liab_PB306_CONTROL_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB306_CONTROL_P8p_SS_mod <- rowMeans(PB306_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB306_CONTROL_P8p_SS_mod <- (va_liab_PB306_CONTROL_P8p_SS_mod/2) / (trait_mean_liab_PB306_CONTROL_P8p_SS_mod)^2
    mean(Evol_liab_PB306_CONTROL_P8p_SS_mod)
    
    
    #PB306_CONTROL_P8p_SS_mod data scale
    {
      
      predict_PB306_CONTROL_P8p_SS_mod <- map(1:nrow(PB306_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB306_CONTROL_P8p_SS_mod %*% PB306_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PB306_CONTROL_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_PB306_CONTROL_P8p_SS_mod,
                      var.a = PB306_CONTROL_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_CONTROL_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_CONTROL_P8p_SS_mod <- data_PB306_CONTROL_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_PB306_CONTROL_P8p_SS_mod <- data_PB306_CONTROL_P8p_SS_mod[["mean.obs"]]
      va_data_PB306_CONTROL_P8p_SS_mod <- data_PB306_CONTROL_P8p_SS_mod[["var.a.obs"]]
      vp_data_PB306_CONTROL_P8p_SS_mod <- data_PB306_CONTROL_P8p_SS_mod[["var.obs"]]
      
      Evol_data_PB306_CONTROL_P8p_SS_mod <- (va_data_PB306_CONTROL_P8p_SS_mod/2) / (trait_mean_data_PB306_CONTROL_P8p_SS_mod)^2
      
      mean(h2_data_PB306_CONTROL_P8p_SS_mod) 
      mean(trait_mean_data_PB306_CONTROL_P8p_SS_mod)
      mean(va_data_PB306_CONTROL_P8p_SS_mod) 
      mean(vp_data_PB306_CONTROL_P8p_SS_mod)
      mean(Evol_data_PB306_CONTROL_P8p_SS_mod)
      
    }
    
  }
  
  #PB306_MA_P8p_SS_mod 
  {
    
    PB306_MA_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_PB306_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(PB306_MA_P8p_SS_mod, file = "PB306_MA_P8p_SS_mod.rds")
    PB306_MA_P8p_SS_mod <- readRDS("PB306_MA_P8p_SS_mod.rds")
    
    summary(PB306_MA_P8p_SS_mod) 
    #plot(PB306_MA_P8p_SS_mod)
    
    # traces and posterior densities
    pdf("PB306_MA_P8p_SS_mod.pdf")
    plot(PB306_MA_P8p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(PB306_MA_P8p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_MA_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB306_MA_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_MA_P8p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB306_MA_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB306_MA_P8p_SS_mod[["VCV"]])
    #autocorr.plot(PB306_MA_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_MA_P8p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB306_MA_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_MA_P8p_SS_mod <- PB306_MA_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB306_MA_P8p_SS_mod <- PB306_MA_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_PB306_MA_P8p_SS_mod <- rowSums(PB306_MA_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_PB306_MA_P8p_SS_mod) 
      HPDinterval(va_liab_PB306_MA_P8p_SS_mod) 
      
      mean(vlat_PB306_MA_P8p_SS_mod) 
      
      #variance of fixed effects
      X_PB306_MA_P8p_SS_mod <- PB306_MA_P8p_SS_mod[["X"]]
      beta_PB306_MA_P8p_SS_mod <- PB306_MA_P8p_SS_mod[["Sol"]][,c(1:4)]
      vf_PB306_MA_P8p_SS_mod   <- apply(beta_PB306_MA_P8p_SS_mod, 1, function(b) {var(as.vector(X_PB306_MA_P8p_SS_mod %*% b))}) 
      mean(vf_PB306_MA_P8p_SS_mod) 
      
      h2_liab_PB306_MA_P8p_SS_mod <- va_liab_PB306_MA_P8p_SS_mod / (vlat_PB306_MA_P8p_SS_mod + vf_PB306_MA_P8p_SS_mod)
      mean(h2_liab_PB306_MA_P8p_SS_mod) 
      posterior.mode(h2_liab_PB306_MA_P8p_SS_mod)	
      median(h2_liab_PB306_MA_P8p_SS_mod)		
      HPDinterval(h2_liab_PB306_MA_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB306_MA_P8p_SS_mod <- rowMeans(PB306_MA_P8p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB306_MA_P8p_SS_mod <- (va_liab_PB306_MA_P8p_SS_mod/2) / (trait_mean_liab_PB306_MA_P8p_SS_mod)^2
    mean(Evol_liab_PB306_MA_P8p_SS_mod)
    
    #PB306_MA_P8p_SS_mod data scale
    {
      
      predict_PB306_MA_P8p_SS_mod <- map(1:nrow(PB306_MA_P8p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB306_MA_P8p_SS_mod %*% PB306_MA_P8p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PB306_MA_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_PB306_MA_P8p_SS_mod,
                      var.a = PB306_MA_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_MA_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_MA_P8p_SS_mod <- data_PB306_MA_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_PB306_MA_P8p_SS_mod <- data_PB306_MA_P8p_SS_mod[["mean.obs"]]
      va_data_PB306_MA_P8p_SS_mod <- data_PB306_MA_P8p_SS_mod[["var.a.obs"]]
      vp_data_PB306_MA_P8p_SS_mod <- data_PB306_MA_P8p_SS_mod[["var.obs"]]
      
      Evol_data_PB306_MA_P8p_SS_mod <- (va_data_PB306_MA_P8p_SS_mod/2) / (trait_mean_data_PB306_MA_P8p_SS_mod)^2
      
      mean(h2_data_PB306_MA_P8p_SS_mod)
      mean(trait_mean_data_PB306_MA_P8p_SS_mod)
      mean(va_data_PB306_MA_P8p_SS_mod)
      mean(vp_data_PB306_MA_P8p_SS_mod)
      mean(Evol_data_PB306_MA_P8p_SS_mod)
      
    }
    
  }
  
  #PB306_Vm_P8p_SS_mod 
  {
    
    PB306_Vm_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_PB306_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(PB306_Vm_P8p_SS_mod, file = "PB306_Vm_P8p_SS_mod.rds")
    PB306_Vm_P8p_SS_mod <- readRDS("PB306_Vm_P8p_SS_mod.rds")
    
    summary(PB306_Vm_P8p_SS_mod) 
    plotTrace(PB306_Vm_P8p_SS_mod$Sol)
    
    # traces and posterior densities
    pdf("PB306_Vm_P8p_SS_mod.pdf")
    plot(PB306_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(PB306_Vm_P8p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PB306_Vm_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_Vm_P8p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PB306_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PB306_Vm_P8p_SS_mod[["VCV"]])
    #autocorr.plot(PB306_Vm_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PB306_Vm_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_Vm_P8p_SS_mod <- PB306_Vm_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB306_Vm_P8p_SS_mod <- PB306_Vm_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_PB306_Vm_P8p_SS_mod <- rowSums(PB306_Vm_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_PB306_Vm_P8p_SS_mod) 
      HPDinterval(va_liab_PB306_Vm_P8p_SS_mod) 
      
      mean(vlat_PB306_Vm_P8p_SS_mod) 
      
      #variance of fixed effects
      X_PB306_Vm_P8p_SS_mod <- PB306_Vm_P8p_SS_mod[["X"]]
      beta_PB306_Vm_P8p_SS_mod <- PB306_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]
      vf_PB306_Vm_P8p_SS_mod   <- apply(beta_PB306_Vm_P8p_SS_mod, 1, function(b) {var(as.vector(X_PB306_Vm_P8p_SS_mod %*% b))}) 
      mean(vf_PB306_Vm_P8p_SS_mod) 
      
      
      h2_liab_PB306_Vm_P8p_SS_mod <- va_liab_PB306_Vm_P8p_SS_mod / (vlat_PB306_Vm_P8p_SS_mod + vf_PB306_Vm_P8p_SS_mod)
      mean(h2_liab_PB306_Vm_P8p_SS_mod) 
      posterior.mode(h2_liab_PB306_Vm_P8p_SS_mod)	
      median(h2_liab_PB306_Vm_P8p_SS_mod)		
      HPDinterval(h2_liab_PB306_Vm_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PB306_Vm_P8p_SS_mod <- ((rowMeans(PB306_Vm_P8p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(PB306_Vm_P8p_SS_mod[["Sol"]][,c(1:4)]) + PB306_Vm_P8p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PB306_Vm_P8p_SS_mod <- (va_liab_PB306_Vm_P8p_SS_mod/2) / (trait_mean_liab_PB306_Vm_P8p_SS_mod)^2
    mean(Evol_liab_PB306_Vm_P8p_SS_mod)
    
    
    #PB306_Vm_P8p_SS_mod data scale
    {
      
      predict_PB306_Vm_P8p_SS_mod <- map(1:nrow(PB306_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PB306_Vm_P8p_SS_mod %*% PB306_Vm_P8p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PB306_Vm_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_PB306_Vm_P8p_SS_mod,
                      var.a = PB306_Vm_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_Vm_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_Vm_P8p_SS_mod <- data_PB306_Vm_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_PB306_Vm_P8p_SS_mod <- data_PB306_Vm_P8p_SS_mod[["mean.obs"]]
      va_data_PB306_Vm_P8p_SS_mod <- data_PB306_Vm_P8p_SS_mod[["var.a.obs"]]
      vp_data_PB306_Vm_P8p_SS_mod <- data_PB306_Vm_P8p_SS_mod[["var.obs"]]
      
      Evol_data_PB306_Vm_P8p_SS_mod <- (va_data_PB306_Vm_P8p_SS_mod/2) / (trait_mean_data_PB306_Vm_P8p_SS_mod)^2
      
      mean(h2_data_PB306_Vm_P8p_SS_mod)
      mean(trait_mean_data_PB306_Vm_P8p_SS_mod)
      mean(va_data_PB306_Vm_P8p_SS_mod)
      mean(vp_data_PB306_Vm_P8p_SS_mod)
      mean(Evol_data_PB306_Vm_P8p_SS_mod)
      
    }
    
  }
 
  ##---- PB306 P5p ----
  
  
  #PB306_CONTROL_P5p_wt_mod 
  {
    
    PB306_CONTROL_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vm_PB306_bi_CONTROL,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)            
    
    saveRDS(PB306_CONTROL_P5p_wt_mod, file = "PB306_CONTROL_P5p_wt_mod.rds")
    PB306_CONTROL_P5p_wt_mod <- readRDS("PB306_CONTROL_P5p_wt_mod.rds")
    
    summary(PB306_CONTROL_P5p_wt_mod) 
    plotTrace(PB306_CONTROL_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PB306_CONTROL_P5p_wt_mod.pdf")
    plot(PB306_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PB306_CONTROL_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB306_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB306_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB306_CONTROL_P5p_wt_mod[["VCV"]])
    #autocorr.plot(PB306_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB306_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_CONTROL_P5p_wt_mod <- PB306_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB306_CONTROL_P5p_wt_mod <- PB306_CONTROL_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_PB306_CONTROL_P5p_wt_mod <- rowSums(PB306_CONTROL_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_PB306_CONTROL_P5p_wt_mod) 
      HPDinterval(va_liab_PB306_CONTROL_P5p_wt_mod) 
      
      mean(vlat_PB306_CONTROL_P5p_wt_mod) 
      
      #variance of fixed effects
      X_PB306_CONTROL_P5p_wt_mod <- PB306_CONTROL_P5p_wt_mod[["X"]]
      beta_PB306_CONTROL_P5p_wt_mod <- PB306_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_PB306_CONTROL_P5p_wt_mod   <- apply(beta_PB306_CONTROL_P5p_wt_mod, 1, function(b) {var(as.vector(X_PB306_CONTROL_P5p_wt_mod %*% b))}) 
      mean(vf_PB306_CONTROL_P5p_wt_mod) 
      
      
      h2_liab_PB306_CONTROL_P5p_wt_mod <- va_liab_PB306_CONTROL_P5p_wt_mod / (vlat_PB306_CONTROL_P5p_wt_mod + vf_PB306_CONTROL_P5p_wt_mod)
      mean(h2_liab_PB306_CONTROL_P5p_wt_mod) 
      posterior.mode(h2_liab_PB306_CONTROL_P5p_wt_mod)	
      median(h2_liab_PB306_CONTROL_P5p_wt_mod)		
      HPDinterval(h2_liab_PB306_CONTROL_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB306_CONTROL_P5p_wt_mod <- rowMeans(PB306_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB306_CONTROL_P5p_wt_mod <- (va_liab_PB306_CONTROL_P5p_wt_mod/2) / (trait_mean_liab_PB306_CONTROL_P5p_wt_mod)^2
    mean(Evol_liab_PB306_CONTROL_P5p_wt_mod)
    
    
    #PB306_CONTROL_P5p_wt_mod data scale
    {
      
      predict_PB306_CONTROL_P5p_wt_mod <- map(1:nrow(PB306_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB306_CONTROL_P5p_wt_mod %*% PB306_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PB306_CONTROL_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_PB306_CONTROL_P5p_wt_mod,
                      var.a = PB306_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_CONTROL_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_CONTROL_P5p_wt_mod <- data_PB306_CONTROL_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_PB306_CONTROL_P5p_wt_mod <- data_PB306_CONTROL_P5p_wt_mod[["mean.obs"]]
      va_data_PB306_CONTROL_P5p_wt_mod <- data_PB306_CONTROL_P5p_wt_mod[["var.a.obs"]]
      vp_data_PB306_CONTROL_P5p_wt_mod <- data_PB306_CONTROL_P5p_wt_mod[["var.obs"]]
      
      Evol_data_PB306_CONTROL_P5p_wt_mod <- (va_data_PB306_CONTROL_P5p_wt_mod/2) / (trait_mean_data_PB306_CONTROL_P5p_wt_mod)^2
      
      mean(h2_data_PB306_CONTROL_P5p_wt_mod) 
      mean(trait_mean_data_PB306_CONTROL_P5p_wt_mod)
      mean(va_data_PB306_CONTROL_P5p_wt_mod) 
      mean(vp_data_PB306_CONTROL_P5p_wt_mod)
      mean(Evol_data_PB306_CONTROL_P5p_wt_mod)
      
    }
    
  }
  
  #PB306_MA_P5p_wt_mod 
  {
    
    PB306_MA_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PB306_bi_MA,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)         
    
    saveRDS(PB306_MA_P5p_wt_mod, file = "PB306_MA_P5p_wt_mod.rds")
    PB306_MA_P5p_wt_mod <- readRDS("PB306_MA_P5p_wt_mod.rds")
    
    summary(PB306_MA_P5p_wt_mod) 
    #plot(PB306_MA_P5p_wt_mod)
    
    # traces and posterior densities
    pdf("PB306_MA_P5p_wt_mod.pdf")
    plot(PB306_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PB306_MA_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB306_MA_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_MA_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB306_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB306_MA_P5p_wt_mod[["VCV"]])
    #autocorr.plot(PB306_MA_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB306_MA_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_MA_P5p_wt_mod <- PB306_MA_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB306_MA_P5p_wt_mod <- PB306_MA_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_PB306_MA_P5p_wt_mod <- rowSums(PB306_MA_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_PB306_MA_P5p_wt_mod) 
      HPDinterval(va_liab_PB306_MA_P5p_wt_mod) 
      
      mean(vlat_PB306_MA_P5p_wt_mod) 
      
      #variance of fixed effects
      X_PB306_MA_P5p_wt_mod <- PB306_MA_P5p_wt_mod[["X"]]
      beta_PB306_MA_P5p_wt_mod <- PB306_MA_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_PB306_MA_P5p_wt_mod   <- apply(beta_PB306_MA_P5p_wt_mod, 1, function(b) {var(as.vector(X_PB306_MA_P5p_wt_mod %*% b))}) 
      mean(vf_PB306_MA_P5p_wt_mod) 
      
      h2_liab_PB306_MA_P5p_wt_mod <- va_liab_PB306_MA_P5p_wt_mod / (vlat_PB306_MA_P5p_wt_mod + vf_PB306_MA_P5p_wt_mod)
      mean(h2_liab_PB306_MA_P5p_wt_mod) 
      posterior.mode(h2_liab_PB306_MA_P5p_wt_mod)	
      median(h2_liab_PB306_MA_P5p_wt_mod)		
      HPDinterval(h2_liab_PB306_MA_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB306_MA_P5p_wt_mod <- rowMeans(PB306_MA_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB306_MA_P5p_wt_mod <- (va_liab_PB306_MA_P5p_wt_mod/2) / (trait_mean_liab_PB306_MA_P5p_wt_mod)^2
    mean(Evol_liab_PB306_MA_P5p_wt_mod)
    
    #PB306_MA_P5p_wt_mod data scale
    {
      
      predict_PB306_MA_P5p_wt_mod <- map(1:nrow(PB306_MA_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB306_MA_P5p_wt_mod %*% PB306_MA_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PB306_MA_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_PB306_MA_P5p_wt_mod,
                      var.a = PB306_MA_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_MA_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_MA_P5p_wt_mod <- data_PB306_MA_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_PB306_MA_P5p_wt_mod <- data_PB306_MA_P5p_wt_mod[["mean.obs"]]
      va_data_PB306_MA_P5p_wt_mod <- data_PB306_MA_P5p_wt_mod[["var.a.obs"]]
      vp_data_PB306_MA_P5p_wt_mod <- data_PB306_MA_P5p_wt_mod[["var.obs"]]
      
      Evol_data_PB306_MA_P5p_wt_mod <- (va_data_PB306_MA_P5p_wt_mod/2) / (trait_mean_data_PB306_MA_P5p_wt_mod)^2
      
      mean(h2_data_PB306_MA_P5p_wt_mod)
      mean(trait_mean_data_PB306_MA_P5p_wt_mod)
      mean(va_data_PB306_MA_P5p_wt_mod)
      mean(vp_data_PB306_MA_P5p_wt_mod)
      mean(Evol_data_PB306_MA_P5p_wt_mod)
      
    }
    
  }
  
  #PB306_Vm_P5p_wt_mod 
  {
    
    PB306_Vm_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Treatment -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PB306_data,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)            
    
    saveRDS(PB306_Vm_P5p_wt_mod, file = "PB306_Vm_P5p_wt_mod.rds")
    PB306_Vm_P5p_wt_mod <- readRDS("PB306_Vm_P5p_wt_mod.rds")
    
    summary(PB306_Vm_P5p_wt_mod) 
    plotTrace(PB306_Vm_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("PB306_Vm_P5p_wt_mod.pdf")
    plot(PB306_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(PB306_Vm_P5p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PB306_Vm_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_Vm_P5p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PB306_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PB306_Vm_P5p_wt_mod[["VCV"]])
    #autocorr.plot(PB306_Vm_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PB306_Vm_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_Vm_P5p_wt_mod <- PB306_Vm_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB306_Vm_P5p_wt_mod <- PB306_Vm_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_PB306_Vm_P5p_wt_mod <- rowSums(PB306_Vm_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_PB306_Vm_P5p_wt_mod) 
      HPDinterval(va_liab_PB306_Vm_P5p_wt_mod) 
      
      mean(vlat_PB306_Vm_P5p_wt_mod) 
      
      #variance of fixed effects
      X_PB306_Vm_P5p_wt_mod <- PB306_Vm_P5p_wt_mod[["X"]]
      beta_PB306_Vm_P5p_wt_mod <- PB306_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]
      vf_PB306_Vm_P5p_wt_mod   <- apply(beta_PB306_Vm_P5p_wt_mod, 1, function(b) {var(as.vector(X_PB306_Vm_P5p_wt_mod %*% b))}) 
      mean(vf_PB306_Vm_P5p_wt_mod) 
      
      
      h2_liab_PB306_Vm_P5p_wt_mod <- va_liab_PB306_Vm_P5p_wt_mod / (vlat_PB306_Vm_P5p_wt_mod + vf_PB306_Vm_P5p_wt_mod)
      mean(h2_liab_PB306_Vm_P5p_wt_mod) 
      posterior.mode(h2_liab_PB306_Vm_P5p_wt_mod)	
      median(h2_liab_PB306_Vm_P5p_wt_mod)		
      HPDinterval(h2_liab_PB306_Vm_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PB306_Vm_P5p_wt_mod <- ((rowMeans(PB306_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(PB306_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + PB306_Vm_P5p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PB306_Vm_P5p_wt_mod <- (va_liab_PB306_Vm_P5p_wt_mod/2) / (trait_mean_liab_PB306_Vm_P5p_wt_mod)^2
    mean(Evol_liab_PB306_Vm_P5p_wt_mod)
    
    
    #PB306_Vm_P5p_wt_mod data scale
    {
      
      predict_PB306_Vm_P5p_wt_mod <- map(1:nrow(PB306_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PB306_Vm_P5p_wt_mod %*% PB306_Vm_P5p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PB306_Vm_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_PB306_Vm_P5p_wt_mod,
                      var.a = PB306_Vm_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_Vm_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_Vm_P5p_wt_mod <- data_PB306_Vm_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_PB306_Vm_P5p_wt_mod <- data_PB306_Vm_P5p_wt_mod[["mean.obs"]]
      va_data_PB306_Vm_P5p_wt_mod <- data_PB306_Vm_P5p_wt_mod[["var.a.obs"]]
      vp_data_PB306_Vm_P5p_wt_mod <- data_PB306_Vm_P5p_wt_mod[["var.obs"]]
      
      Evol_data_PB306_Vm_P5p_wt_mod <- (va_data_PB306_Vm_P5p_wt_mod/2) / (trait_mean_data_PB306_Vm_P5p_wt_mod)^2
      
      mean(h2_data_PB306_Vm_P5p_wt_mod)
      mean(trait_mean_data_PB306_Vm_P5p_wt_mod)
      mean(va_data_PB306_Vm_P5p_wt_mod)
      mean(vp_data_PB306_Vm_P5p_wt_mod)
      mean(Evol_data_PB306_Vm_P5p_wt_mod)
      
    }
    
  }
  
  
  
  
  
  ##---- PB306 P6p ----
  
  
  #PB306_CONTROL_P6p_wt_mod 
  {
    
    PB306_CONTROL_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vm_PB306_bi_CONTROL,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)            
    
    saveRDS(PB306_CONTROL_P6p_wt_mod, file = "PB306_CONTROL_P6p_wt_mod.rds")
    PB306_CONTROL_P6p_wt_mod <- readRDS("PB306_CONTROL_P6p_wt_mod.rds")
    
    summary(PB306_CONTROL_P6p_wt_mod) 
    plotTrace(PB306_CONTROL_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PB306_CONTROL_P6p_wt_mod.pdf")
    plot(PB306_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PB306_CONTROL_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB306_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB306_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB306_CONTROL_P6p_wt_mod[["VCV"]])
    #autocorr.plot(PB306_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB306_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_CONTROL_P6p_wt_mod <- PB306_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB306_CONTROL_P6p_wt_mod <- PB306_CONTROL_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_PB306_CONTROL_P6p_wt_mod <- rowSums(PB306_CONTROL_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_PB306_CONTROL_P6p_wt_mod) 
      HPDinterval(va_liab_PB306_CONTROL_P6p_wt_mod) 
      
      mean(vlat_PB306_CONTROL_P6p_wt_mod) 
      
      #variance of fixed effects
      X_PB306_CONTROL_P6p_wt_mod <- PB306_CONTROL_P6p_wt_mod[["X"]]
      beta_PB306_CONTROL_P6p_wt_mod <- PB306_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_PB306_CONTROL_P6p_wt_mod   <- apply(beta_PB306_CONTROL_P6p_wt_mod, 1, function(b) {var(as.vector(X_PB306_CONTROL_P6p_wt_mod %*% b))}) 
      mean(vf_PB306_CONTROL_P6p_wt_mod) 
      
      
      h2_liab_PB306_CONTROL_P6p_wt_mod <- va_liab_PB306_CONTROL_P6p_wt_mod / (vlat_PB306_CONTROL_P6p_wt_mod + vf_PB306_CONTROL_P6p_wt_mod)
      mean(h2_liab_PB306_CONTROL_P6p_wt_mod) 
      posterior.mode(h2_liab_PB306_CONTROL_P6p_wt_mod)	
      median(h2_liab_PB306_CONTROL_P6p_wt_mod)		
      HPDinterval(h2_liab_PB306_CONTROL_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB306_CONTROL_P6p_wt_mod <- rowMeans(PB306_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB306_CONTROL_P6p_wt_mod <- (va_liab_PB306_CONTROL_P6p_wt_mod/2) / (trait_mean_liab_PB306_CONTROL_P6p_wt_mod)^2
    mean(Evol_liab_PB306_CONTROL_P6p_wt_mod)
    
    
    #PB306_CONTROL_P6p_wt_mod data scale
    {
      
      predict_PB306_CONTROL_P6p_wt_mod <- map(1:nrow(PB306_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB306_CONTROL_P6p_wt_mod %*% PB306_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PB306_CONTROL_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_PB306_CONTROL_P6p_wt_mod,
                      var.a = PB306_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_CONTROL_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_CONTROL_P6p_wt_mod <- data_PB306_CONTROL_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_PB306_CONTROL_P6p_wt_mod <- data_PB306_CONTROL_P6p_wt_mod[["mean.obs"]]
      va_data_PB306_CONTROL_P6p_wt_mod <- data_PB306_CONTROL_P6p_wt_mod[["var.a.obs"]]
      vp_data_PB306_CONTROL_P6p_wt_mod <- data_PB306_CONTROL_P6p_wt_mod[["var.obs"]]
      
      Evol_data_PB306_CONTROL_P6p_wt_mod <- (va_data_PB306_CONTROL_P6p_wt_mod/2) / (trait_mean_data_PB306_CONTROL_P6p_wt_mod)^2
      
      mean(h2_data_PB306_CONTROL_P6p_wt_mod) 
      mean(trait_mean_data_PB306_CONTROL_P6p_wt_mod)
      mean(va_data_PB306_CONTROL_P6p_wt_mod) 
      mean(vp_data_PB306_CONTROL_P6p_wt_mod)
      mean(Evol_data_PB306_CONTROL_P6p_wt_mod)
      
    }
    
  }
  
  #PB306_MA_P6p_wt_mod 
  {
    
    PB306_MA_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PB306_bi_MA,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)         
    
    saveRDS(PB306_MA_P6p_wt_mod, file = "PB306_MA_P6p_wt_mod.rds")
    PB306_MA_P6p_wt_mod <- readRDS("PB306_MA_P6p_wt_mod.rds")
    
    summary(PB306_MA_P6p_wt_mod) 
    #plot(PB306_MA_P6p_wt_mod)
    
    # traces and posterior densities
    pdf("PB306_MA_P6p_wt_mod.pdf")
    plot(PB306_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PB306_MA_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB306_MA_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_MA_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB306_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB306_MA_P6p_wt_mod[["VCV"]])
    #autocorr.plot(PB306_MA_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB306_MA_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_MA_P6p_wt_mod <- PB306_MA_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB306_MA_P6p_wt_mod <- PB306_MA_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_PB306_MA_P6p_wt_mod <- rowSums(PB306_MA_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_PB306_MA_P6p_wt_mod) 
      HPDinterval(va_liab_PB306_MA_P6p_wt_mod) 
      
      mean(vlat_PB306_MA_P6p_wt_mod) 
      
      #variance of fixed effects
      X_PB306_MA_P6p_wt_mod <- PB306_MA_P6p_wt_mod[["X"]]
      beta_PB306_MA_P6p_wt_mod <- PB306_MA_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_PB306_MA_P6p_wt_mod   <- apply(beta_PB306_MA_P6p_wt_mod, 1, function(b) {var(as.vector(X_PB306_MA_P6p_wt_mod %*% b))}) 
      mean(vf_PB306_MA_P6p_wt_mod) 
      
      h2_liab_PB306_MA_P6p_wt_mod <- va_liab_PB306_MA_P6p_wt_mod / (vlat_PB306_MA_P6p_wt_mod + vf_PB306_MA_P6p_wt_mod)
      mean(h2_liab_PB306_MA_P6p_wt_mod) 
      posterior.mode(h2_liab_PB306_MA_P6p_wt_mod)	
      median(h2_liab_PB306_MA_P6p_wt_mod)		
      HPDinterval(h2_liab_PB306_MA_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB306_MA_P6p_wt_mod <- rowMeans(PB306_MA_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB306_MA_P6p_wt_mod <- (va_liab_PB306_MA_P6p_wt_mod/2) / (trait_mean_liab_PB306_MA_P6p_wt_mod)^2
    mean(Evol_liab_PB306_MA_P6p_wt_mod)
    
    #PB306_MA_P6p_wt_mod data scale
    {
      
      predict_PB306_MA_P6p_wt_mod <- map(1:nrow(PB306_MA_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB306_MA_P6p_wt_mod %*% PB306_MA_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PB306_MA_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_PB306_MA_P6p_wt_mod,
                      var.a = PB306_MA_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_MA_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_MA_P6p_wt_mod <- data_PB306_MA_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_PB306_MA_P6p_wt_mod <- data_PB306_MA_P6p_wt_mod[["mean.obs"]]
      va_data_PB306_MA_P6p_wt_mod <- data_PB306_MA_P6p_wt_mod[["var.a.obs"]]
      vp_data_PB306_MA_P6p_wt_mod <- data_PB306_MA_P6p_wt_mod[["var.obs"]]
      
      Evol_data_PB306_MA_P6p_wt_mod <- (va_data_PB306_MA_P6p_wt_mod/2) / (trait_mean_data_PB306_MA_P6p_wt_mod)^2
      
      mean(h2_data_PB306_MA_P6p_wt_mod)
      mean(trait_mean_data_PB306_MA_P6p_wt_mod)
      mean(va_data_PB306_MA_P6p_wt_mod)
      mean(vp_data_PB306_MA_P6p_wt_mod)
      mean(Evol_data_PB306_MA_P6p_wt_mod)
      
    }
    
  }
  
  #PB306_Vm_P6p_wt_mod 
  {
    
    PB306_Vm_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Treatment -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PB306_data,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)            
    
    saveRDS(PB306_Vm_P6p_wt_mod, file = "PB306_Vm_P6p_wt_mod.rds")
    PB306_Vm_P6p_wt_mod <- readRDS("PB306_Vm_P6p_wt_mod.rds")
    
    summary(PB306_Vm_P6p_wt_mod) 
    plotTrace(PB306_Vm_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("PB306_Vm_P6p_wt_mod.pdf")
    plot(PB306_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(PB306_Vm_P6p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PB306_Vm_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_Vm_P6p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PB306_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PB306_Vm_P6p_wt_mod[["VCV"]])
    #autocorr.plot(PB306_Vm_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PB306_Vm_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_Vm_P6p_wt_mod <- PB306_Vm_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB306_Vm_P6p_wt_mod <- PB306_Vm_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_PB306_Vm_P6p_wt_mod <- rowSums(PB306_Vm_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_PB306_Vm_P6p_wt_mod) 
      HPDinterval(va_liab_PB306_Vm_P6p_wt_mod) 
      
      mean(vlat_PB306_Vm_P6p_wt_mod) 
      
      #variance of fixed effects
      X_PB306_Vm_P6p_wt_mod <- PB306_Vm_P6p_wt_mod[["X"]]
      beta_PB306_Vm_P6p_wt_mod <- PB306_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]
      vf_PB306_Vm_P6p_wt_mod   <- apply(beta_PB306_Vm_P6p_wt_mod, 1, function(b) {var(as.vector(X_PB306_Vm_P6p_wt_mod %*% b))}) 
      mean(vf_PB306_Vm_P6p_wt_mod) 
      
      
      h2_liab_PB306_Vm_P6p_wt_mod <- va_liab_PB306_Vm_P6p_wt_mod / (vlat_PB306_Vm_P6p_wt_mod + vf_PB306_Vm_P6p_wt_mod)
      mean(h2_liab_PB306_Vm_P6p_wt_mod) 
      posterior.mode(h2_liab_PB306_Vm_P6p_wt_mod)	
      median(h2_liab_PB306_Vm_P6p_wt_mod)		
      HPDinterval(h2_liab_PB306_Vm_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PB306_Vm_P6p_wt_mod <- ((rowMeans(PB306_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(PB306_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + PB306_Vm_P6p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PB306_Vm_P6p_wt_mod <- (va_liab_PB306_Vm_P6p_wt_mod/2) / (trait_mean_liab_PB306_Vm_P6p_wt_mod)^2
    mean(Evol_liab_PB306_Vm_P6p_wt_mod)
    
    
    #PB306_Vm_P6p_wt_mod data scale
    {
      
      predict_PB306_Vm_P6p_wt_mod <- map(1:nrow(PB306_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PB306_Vm_P6p_wt_mod %*% PB306_Vm_P6p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PB306_Vm_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_PB306_Vm_P6p_wt_mod,
                      var.a = PB306_Vm_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_Vm_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_Vm_P6p_wt_mod <- data_PB306_Vm_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_PB306_Vm_P6p_wt_mod <- data_PB306_Vm_P6p_wt_mod[["mean.obs"]]
      va_data_PB306_Vm_P6p_wt_mod <- data_PB306_Vm_P6p_wt_mod[["var.a.obs"]]
      vp_data_PB306_Vm_P6p_wt_mod <- data_PB306_Vm_P6p_wt_mod[["var.obs"]]
      
      Evol_data_PB306_Vm_P6p_wt_mod <- (va_data_PB306_Vm_P6p_wt_mod/2) / (trait_mean_data_PB306_Vm_P6p_wt_mod)^2
      
      mean(h2_data_PB306_Vm_P6p_wt_mod)
      mean(trait_mean_data_PB306_Vm_P6p_wt_mod)
      mean(va_data_PB306_Vm_P6p_wt_mod)
      mean(vp_data_PB306_Vm_P6p_wt_mod)
      mean(Evol_data_PB306_Vm_P6p_wt_mod)
      
    }
    
  }
  
  
  
  
  ##---- PB306 P7p ----
  
  
  #PB306_CONTROL_P7p_wt_mod 
  {
    
    PB306_CONTROL_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vm_PB306_bi_CONTROL,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)            
    
    saveRDS(PB306_CONTROL_P7p_wt_mod, file = "PB306_CONTROL_P7p_wt_mod.rds")
    PB306_CONTROL_P7p_wt_mod <- readRDS("PB306_CONTROL_P7p_wt_mod.rds")
    
    summary(PB306_CONTROL_P7p_wt_mod) 
    plotTrace(PB306_CONTROL_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PB306_CONTROL_P7p_wt_mod.pdf")
    plot(PB306_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PB306_CONTROL_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB306_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB306_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB306_CONTROL_P7p_wt_mod[["VCV"]])
    #autocorr.plot(PB306_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB306_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_CONTROL_P7p_wt_mod <- PB306_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB306_CONTROL_P7p_wt_mod <- PB306_CONTROL_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_PB306_CONTROL_P7p_wt_mod <- rowSums(PB306_CONTROL_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_PB306_CONTROL_P7p_wt_mod) 
      HPDinterval(va_liab_PB306_CONTROL_P7p_wt_mod) 
      
      mean(vlat_PB306_CONTROL_P7p_wt_mod) 
      
      #variance of fixed effects
      X_PB306_CONTROL_P7p_wt_mod <- PB306_CONTROL_P7p_wt_mod[["X"]]
      beta_PB306_CONTROL_P7p_wt_mod <- PB306_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_PB306_CONTROL_P7p_wt_mod   <- apply(beta_PB306_CONTROL_P7p_wt_mod, 1, function(b) {var(as.vector(X_PB306_CONTROL_P7p_wt_mod %*% b))}) 
      mean(vf_PB306_CONTROL_P7p_wt_mod) 
      
      
      h2_liab_PB306_CONTROL_P7p_wt_mod <- va_liab_PB306_CONTROL_P7p_wt_mod / (vlat_PB306_CONTROL_P7p_wt_mod + vf_PB306_CONTROL_P7p_wt_mod)
      mean(h2_liab_PB306_CONTROL_P7p_wt_mod) 
      posterior.mode(h2_liab_PB306_CONTROL_P7p_wt_mod)	
      median(h2_liab_PB306_CONTROL_P7p_wt_mod)		
      HPDinterval(h2_liab_PB306_CONTROL_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB306_CONTROL_P7p_wt_mod <- rowMeans(PB306_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB306_CONTROL_P7p_wt_mod <- (va_liab_PB306_CONTROL_P7p_wt_mod/2) / (trait_mean_liab_PB306_CONTROL_P7p_wt_mod)^2
    mean(Evol_liab_PB306_CONTROL_P7p_wt_mod)
    
    
    #PB306_CONTROL_P7p_wt_mod data scale
    {
      
      predict_PB306_CONTROL_P7p_wt_mod <- map(1:nrow(PB306_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB306_CONTROL_P7p_wt_mod %*% PB306_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PB306_CONTROL_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_PB306_CONTROL_P7p_wt_mod,
                      var.a = PB306_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_CONTROL_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_CONTROL_P7p_wt_mod <- data_PB306_CONTROL_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_PB306_CONTROL_P7p_wt_mod <- data_PB306_CONTROL_P7p_wt_mod[["mean.obs"]]
      va_data_PB306_CONTROL_P7p_wt_mod <- data_PB306_CONTROL_P7p_wt_mod[["var.a.obs"]]
      vp_data_PB306_CONTROL_P7p_wt_mod <- data_PB306_CONTROL_P7p_wt_mod[["var.obs"]]
      
      Evol_data_PB306_CONTROL_P7p_wt_mod <- (va_data_PB306_CONTROL_P7p_wt_mod/2) / (trait_mean_data_PB306_CONTROL_P7p_wt_mod)^2
      
      mean(h2_data_PB306_CONTROL_P7p_wt_mod) 
      mean(trait_mean_data_PB306_CONTROL_P7p_wt_mod)
      mean(va_data_PB306_CONTROL_P7p_wt_mod) 
      mean(vp_data_PB306_CONTROL_P7p_wt_mod)
      mean(Evol_data_PB306_CONTROL_P7p_wt_mod)
      
    }
    
  }
  
  #PB306_MA_P7p_wt_mod 
  {
    
    PB306_MA_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PB306_bi_MA,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)         
    
    saveRDS(PB306_MA_P7p_wt_mod, file = "PB306_MA_P7p_wt_mod.rds")
    PB306_MA_P7p_wt_mod <- readRDS("PB306_MA_P7p_wt_mod.rds")
    
    summary(PB306_MA_P7p_wt_mod) 
    #plot(PB306_MA_P7p_wt_mod)
    
    # traces and posterior densities
    pdf("PB306_MA_P7p_wt_mod.pdf")
    plot(PB306_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PB306_MA_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB306_MA_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_MA_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB306_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB306_MA_P7p_wt_mod[["VCV"]])
    #autocorr.plot(PB306_MA_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB306_MA_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_MA_P7p_wt_mod <- PB306_MA_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB306_MA_P7p_wt_mod <- PB306_MA_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_PB306_MA_P7p_wt_mod <- rowSums(PB306_MA_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_PB306_MA_P7p_wt_mod) 
      HPDinterval(va_liab_PB306_MA_P7p_wt_mod) 
      
      mean(vlat_PB306_MA_P7p_wt_mod) 
      
      #variance of fixed effects
      X_PB306_MA_P7p_wt_mod <- PB306_MA_P7p_wt_mod[["X"]]
      beta_PB306_MA_P7p_wt_mod <- PB306_MA_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_PB306_MA_P7p_wt_mod   <- apply(beta_PB306_MA_P7p_wt_mod, 1, function(b) {var(as.vector(X_PB306_MA_P7p_wt_mod %*% b))}) 
      mean(vf_PB306_MA_P7p_wt_mod) 
      
      h2_liab_PB306_MA_P7p_wt_mod <- va_liab_PB306_MA_P7p_wt_mod / (vlat_PB306_MA_P7p_wt_mod + vf_PB306_MA_P7p_wt_mod)
      mean(h2_liab_PB306_MA_P7p_wt_mod) 
      posterior.mode(h2_liab_PB306_MA_P7p_wt_mod)	
      median(h2_liab_PB306_MA_P7p_wt_mod)		
      HPDinterval(h2_liab_PB306_MA_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB306_MA_P7p_wt_mod <- rowMeans(PB306_MA_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB306_MA_P7p_wt_mod <- (va_liab_PB306_MA_P7p_wt_mod/2) / (trait_mean_liab_PB306_MA_P7p_wt_mod)^2
    mean(Evol_liab_PB306_MA_P7p_wt_mod)
    
    #PB306_MA_P7p_wt_mod data scale
    {
      
      predict_PB306_MA_P7p_wt_mod <- map(1:nrow(PB306_MA_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB306_MA_P7p_wt_mod %*% PB306_MA_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PB306_MA_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_PB306_MA_P7p_wt_mod,
                      var.a = PB306_MA_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_MA_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_MA_P7p_wt_mod <- data_PB306_MA_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_PB306_MA_P7p_wt_mod <- data_PB306_MA_P7p_wt_mod[["mean.obs"]]
      va_data_PB306_MA_P7p_wt_mod <- data_PB306_MA_P7p_wt_mod[["var.a.obs"]]
      vp_data_PB306_MA_P7p_wt_mod <- data_PB306_MA_P7p_wt_mod[["var.obs"]]
      
      Evol_data_PB306_MA_P7p_wt_mod <- (va_data_PB306_MA_P7p_wt_mod/2) / (trait_mean_data_PB306_MA_P7p_wt_mod)^2
      
      mean(h2_data_PB306_MA_P7p_wt_mod)
      mean(trait_mean_data_PB306_MA_P7p_wt_mod)
      mean(va_data_PB306_MA_P7p_wt_mod)
      mean(vp_data_PB306_MA_P7p_wt_mod)
      mean(Evol_data_PB306_MA_P7p_wt_mod)
      
    }
    
  }
  
  #PB306_Vm_P7p_wt_mod 
  {
    
    PB306_Vm_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Treatment -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PB306_data,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)            
    
    saveRDS(PB306_Vm_P7p_wt_mod, file = "PB306_Vm_P7p_wt_mod.rds")
    PB306_Vm_P7p_wt_mod <- readRDS("PB306_Vm_P7p_wt_mod.rds")
    
    summary(PB306_Vm_P7p_wt_mod) 
    plotTrace(PB306_Vm_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("PB306_Vm_P7p_wt_mod.pdf")
    plot(PB306_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(PB306_Vm_P7p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB306_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PB306_Vm_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB306_Vm_P7p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PB306_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PB306_Vm_P7p_wt_mod[["VCV"]])
    #autocorr.plot(PB306_Vm_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB306_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PB306_Vm_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB306_Vm_P7p_wt_mod <- PB306_Vm_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB306_Vm_P7p_wt_mod <- PB306_Vm_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_PB306_Vm_P7p_wt_mod <- rowSums(PB306_Vm_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_PB306_Vm_P7p_wt_mod) 
      HPDinterval(va_liab_PB306_Vm_P7p_wt_mod) 
      
      mean(vlat_PB306_Vm_P7p_wt_mod) 
      
      #variance of fixed effects
      X_PB306_Vm_P7p_wt_mod <- PB306_Vm_P7p_wt_mod[["X"]]
      beta_PB306_Vm_P7p_wt_mod <- PB306_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]
      vf_PB306_Vm_P7p_wt_mod   <- apply(beta_PB306_Vm_P7p_wt_mod, 1, function(b) {var(as.vector(X_PB306_Vm_P7p_wt_mod %*% b))}) 
      mean(vf_PB306_Vm_P7p_wt_mod) 
      
      
      h2_liab_PB306_Vm_P7p_wt_mod <- va_liab_PB306_Vm_P7p_wt_mod / (vlat_PB306_Vm_P7p_wt_mod + vf_PB306_Vm_P7p_wt_mod)
      mean(h2_liab_PB306_Vm_P7p_wt_mod) 
      posterior.mode(h2_liab_PB306_Vm_P7p_wt_mod)	
      median(h2_liab_PB306_Vm_P7p_wt_mod)		
      HPDinterval(h2_liab_PB306_Vm_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PB306_Vm_P7p_wt_mod <- ((rowMeans(PB306_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(PB306_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + PB306_Vm_P7p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PB306_Vm_P7p_wt_mod <- (va_liab_PB306_Vm_P7p_wt_mod/2) / (trait_mean_liab_PB306_Vm_P7p_wt_mod)^2
    mean(Evol_liab_PB306_Vm_P7p_wt_mod)
    
    
    #PB306_Vm_P7p_wt_mod data scale
    {
      
      predict_PB306_Vm_P7p_wt_mod <- map(1:nrow(PB306_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PB306_Vm_P7p_wt_mod %*% PB306_Vm_P7p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PB306_Vm_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_PB306_Vm_P7p_wt_mod,
                      var.a = PB306_Vm_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB306_Vm_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB306_Vm_P7p_wt_mod <- data_PB306_Vm_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_PB306_Vm_P7p_wt_mod <- data_PB306_Vm_P7p_wt_mod[["mean.obs"]]
      va_data_PB306_Vm_P7p_wt_mod <- data_PB306_Vm_P7p_wt_mod[["var.a.obs"]]
      vp_data_PB306_Vm_P7p_wt_mod <- data_PB306_Vm_P7p_wt_mod[["var.obs"]]
      
      Evol_data_PB306_Vm_P7p_wt_mod <- (va_data_PB306_Vm_P7p_wt_mod/2) / (trait_mean_data_PB306_Vm_P7p_wt_mod)^2
      
      mean(h2_data_PB306_Vm_P7p_wt_mod)
      mean(trait_mean_data_PB306_Vm_P7p_wt_mod)
      mean(va_data_PB306_Vm_P7p_wt_mod)
      mean(vp_data_PB306_Vm_P7p_wt_mod)
      mean(Evol_data_PB306_Vm_P7p_wt_mod)
      
    }
    
  }
  
  
  
  
} 
  
  


#AF16----
{
  Vm_AF16_bi_CONTROL <- subset(Vm_AF16_data, Treatment =="CONTROL")
  Vm_AF16_bi_MA <- subset(Vm_AF16_data, Treatment =="MA")
  
  
  ##---- AF16 P3p ----
  
  
  #AF16_CONTROL_P3p_SS_mod 
  {
    
    AF16_CONTROL_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_AF16_bi_CONTROL,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)            
    
    saveRDS(AF16_CONTROL_P3p_SS_mod, file = "AF16_CONTROL_P3p_SS_mod.rds")
    AF16_CONTROL_P3p_SS_mod <- readRDS("AF16_CONTROL_P3p_SS_mod.rds")
    
    summary(AF16_CONTROL_P3p_SS_mod) 
    plotTrace(AF16_CONTROL_P3p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("AF16_CONTROL_P3p_SS_mod.pdf")
    plot(AF16_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(AF16_CONTROL_P3p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(AF16_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(AF16_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(AF16_CONTROL_P3p_SS_mod[["VCV"]])
    #autocorr.plot(AF16_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(AF16_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_CONTROL_P3p_SS_mod <- AF16_CONTROL_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_AF16_CONTROL_P3p_SS_mod <- AF16_CONTROL_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_AF16_CONTROL_P3p_SS_mod <- rowSums(AF16_CONTROL_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_AF16_CONTROL_P3p_SS_mod) 
      HPDinterval(va_liab_AF16_CONTROL_P3p_SS_mod) 
      
      mean(vlat_AF16_CONTROL_P3p_SS_mod) 
      
      #variance of fixed effects
      X_AF16_CONTROL_P3p_SS_mod <- AF16_CONTROL_P3p_SS_mod[["X"]]
      beta_AF16_CONTROL_P3p_SS_mod <- AF16_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]
      vf_AF16_CONTROL_P3p_SS_mod   <- apply(beta_AF16_CONTROL_P3p_SS_mod, 1, function(b) {var(as.vector(X_AF16_CONTROL_P3p_SS_mod %*% b))}) 
      mean(vf_AF16_CONTROL_P3p_SS_mod) 
      
      
      h2_liab_AF16_CONTROL_P3p_SS_mod <- va_liab_AF16_CONTROL_P3p_SS_mod / (vlat_AF16_CONTROL_P3p_SS_mod + vf_AF16_CONTROL_P3p_SS_mod)
      mean(h2_liab_AF16_CONTROL_P3p_SS_mod) 
      posterior.mode(h2_liab_AF16_CONTROL_P3p_SS_mod)	
      median(h2_liab_AF16_CONTROL_P3p_SS_mod)		
      HPDinterval(h2_liab_AF16_CONTROL_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_AF16_CONTROL_P3p_SS_mod <- rowMeans(AF16_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_AF16_CONTROL_P3p_SS_mod <- (va_liab_AF16_CONTROL_P3p_SS_mod/2) / (trait_mean_liab_AF16_CONTROL_P3p_SS_mod)^2
    mean(Evol_liab_AF16_CONTROL_P3p_SS_mod)
    
    
    #AF16_CONTROL_P3p_SS_mod data scale
    {
      
      predict_AF16_CONTROL_P3p_SS_mod <- map(1:nrow(AF16_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_AF16_CONTROL_P3p_SS_mod %*% AF16_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_AF16_CONTROL_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_AF16_CONTROL_P3p_SS_mod,
                      var.a = AF16_CONTROL_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_CONTROL_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_CONTROL_P3p_SS_mod <- data_AF16_CONTROL_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_AF16_CONTROL_P3p_SS_mod <- data_AF16_CONTROL_P3p_SS_mod[["mean.obs"]]
      va_data_AF16_CONTROL_P3p_SS_mod <- data_AF16_CONTROL_P3p_SS_mod[["var.a.obs"]]
      vp_data_AF16_CONTROL_P3p_SS_mod <- data_AF16_CONTROL_P3p_SS_mod[["var.obs"]]
      
      Evol_data_AF16_CONTROL_P3p_SS_mod <- (va_data_AF16_CONTROL_P3p_SS_mod/2) / (trait_mean_data_AF16_CONTROL_P3p_SS_mod)^2
      
      mean(h2_data_AF16_CONTROL_P3p_SS_mod) 
      mean(trait_mean_data_AF16_CONTROL_P3p_SS_mod)
      mean(va_data_AF16_CONTROL_P3p_SS_mod) 
      mean(vp_data_AF16_CONTROL_P3p_SS_mod)
      mean(Evol_data_AF16_CONTROL_P3p_SS_mod)
      
    }
    
  }
  
  #AF16_MA_P3p_SS_mod 
  {
    
    AF16_MA_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_AF16_bi_MA,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)         
    
    saveRDS(AF16_MA_P3p_SS_mod, file = "AF16_MA_P3p_SS_mod.rds")
    AF16_MA_P3p_SS_mod <- readRDS("AF16_MA_P3p_SS_mod.rds")
    
    summary(AF16_MA_P3p_SS_mod) 
    #plot(AF16_MA_P3p_SS_mod)
    
    # traces and posterior densities
    pdf("AF16_MA_P3p_SS_mod.pdf")
    plot(AF16_MA_P3p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(AF16_MA_P3p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_MA_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(AF16_MA_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_MA_P3p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(AF16_MA_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(AF16_MA_P3p_SS_mod[["VCV"]])
    #autocorr.plot(AF16_MA_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_MA_P3p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(AF16_MA_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_MA_P3p_SS_mod <- AF16_MA_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_AF16_MA_P3p_SS_mod <- AF16_MA_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_AF16_MA_P3p_SS_mod <- rowSums(AF16_MA_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_AF16_MA_P3p_SS_mod) 
      HPDinterval(va_liab_AF16_MA_P3p_SS_mod) 
      
      mean(vlat_AF16_MA_P3p_SS_mod) 
      
      #variance of fixed effects
      X_AF16_MA_P3p_SS_mod <- AF16_MA_P3p_SS_mod[["X"]]
      beta_AF16_MA_P3p_SS_mod <- AF16_MA_P3p_SS_mod[["Sol"]][,c(1:4)]
      vf_AF16_MA_P3p_SS_mod   <- apply(beta_AF16_MA_P3p_SS_mod, 1, function(b) {var(as.vector(X_AF16_MA_P3p_SS_mod %*% b))}) 
      mean(vf_AF16_MA_P3p_SS_mod) 
      
      h2_liab_AF16_MA_P3p_SS_mod <- va_liab_AF16_MA_P3p_SS_mod / (vlat_AF16_MA_P3p_SS_mod + vf_AF16_MA_P3p_SS_mod)
      mean(h2_liab_AF16_MA_P3p_SS_mod) 
      posterior.mode(h2_liab_AF16_MA_P3p_SS_mod)	
      median(h2_liab_AF16_MA_P3p_SS_mod)		
      HPDinterval(h2_liab_AF16_MA_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_AF16_MA_P3p_SS_mod <- rowMeans(AF16_MA_P3p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_AF16_MA_P3p_SS_mod <- (va_liab_AF16_MA_P3p_SS_mod/2) / (trait_mean_liab_AF16_MA_P3p_SS_mod)^2
    mean(Evol_liab_AF16_MA_P3p_SS_mod)
    
    #AF16_MA_P3p_SS_mod data scale
    {
      
      predict_AF16_MA_P3p_SS_mod <- map(1:nrow(AF16_MA_P3p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_AF16_MA_P3p_SS_mod %*% AF16_MA_P3p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_AF16_MA_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_AF16_MA_P3p_SS_mod,
                      var.a = AF16_MA_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_MA_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_MA_P3p_SS_mod <- data_AF16_MA_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_AF16_MA_P3p_SS_mod <- data_AF16_MA_P3p_SS_mod[["mean.obs"]]
      va_data_AF16_MA_P3p_SS_mod <- data_AF16_MA_P3p_SS_mod[["var.a.obs"]]
      vp_data_AF16_MA_P3p_SS_mod <- data_AF16_MA_P3p_SS_mod[["var.obs"]]
      
      Evol_data_AF16_MA_P3p_SS_mod <- (va_data_AF16_MA_P3p_SS_mod/2) / (trait_mean_data_AF16_MA_P3p_SS_mod)^2
      
      mean(h2_data_AF16_MA_P3p_SS_mod)
      mean(trait_mean_data_AF16_MA_P3p_SS_mod)
      mean(va_data_AF16_MA_P3p_SS_mod)
      mean(vp_data_AF16_MA_P3p_SS_mod)
      mean(Evol_data_AF16_MA_P3p_SS_mod)
      
    }
    
  }
  
  #AF16_Vm_P3p_SS_mod 
  {
    
    AF16_Vm_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer + Treatment -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_AF16_data,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)            
    
    saveRDS(AF16_Vm_P3p_SS_mod, file = "AF16_Vm_P3p_SS_mod.rds")
    AF16_Vm_P3p_SS_mod <- readRDS("AF16_Vm_P3p_SS_mod.rds")
    
    summary(AF16_Vm_P3p_SS_mod) 
    plotTrace(AF16_Vm_P3p_SS_mod$Sol)
    
    # traces and posterior densities
    pdf("AF16_Vm_P3p_SS_mod.pdf")
    plot(AF16_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(AF16_Vm_P3p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(AF16_Vm_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_Vm_P3p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(AF16_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(AF16_Vm_P3p_SS_mod[["VCV"]])
    #autocorr.plot(AF16_Vm_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(AF16_Vm_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_Vm_P3p_SS_mod <- AF16_Vm_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_AF16_Vm_P3p_SS_mod <- AF16_Vm_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_AF16_Vm_P3p_SS_mod <- rowSums(AF16_Vm_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_AF16_Vm_P3p_SS_mod) 
      HPDinterval(va_liab_AF16_Vm_P3p_SS_mod) 
      
      mean(vlat_AF16_Vm_P3p_SS_mod) 
      
      #variance of fixed effects
      X_AF16_Vm_P3p_SS_mod <- AF16_Vm_P3p_SS_mod[["X"]]
      beta_AF16_Vm_P3p_SS_mod <- AF16_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]
      vf_AF16_Vm_P3p_SS_mod   <- apply(beta_AF16_Vm_P3p_SS_mod, 1, function(b) {var(as.vector(X_AF16_Vm_P3p_SS_mod %*% b))}) 
      mean(vf_AF16_Vm_P3p_SS_mod) 
      
      
      h2_liab_AF16_Vm_P3p_SS_mod <- va_liab_AF16_Vm_P3p_SS_mod / (vlat_AF16_Vm_P3p_SS_mod + vf_AF16_Vm_P3p_SS_mod)
      mean(h2_liab_AF16_Vm_P3p_SS_mod) 
      posterior.mode(h2_liab_AF16_Vm_P3p_SS_mod)	
      median(h2_liab_AF16_Vm_P3p_SS_mod)		
      HPDinterval(h2_liab_AF16_Vm_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_AF16_Vm_P3p_SS_mod <- ((rowMeans(AF16_Vm_P3p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(AF16_Vm_P3p_SS_mod[["Sol"]][,c(1:4)]) + AF16_Vm_P3p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_AF16_Vm_P3p_SS_mod <- (va_liab_AF16_Vm_P3p_SS_mod/2) / (trait_mean_liab_AF16_Vm_P3p_SS_mod)^2
    mean(Evol_liab_AF16_Vm_P3p_SS_mod)
    
    
    #AF16_Vm_P3p_SS_mod data scale
    {
      
      predict_AF16_Vm_P3p_SS_mod <- map(1:nrow(AF16_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_AF16_Vm_P3p_SS_mod %*% AF16_Vm_P3p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_AF16_Vm_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_AF16_Vm_P3p_SS_mod,
                      var.a = AF16_Vm_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_Vm_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_Vm_P3p_SS_mod <- data_AF16_Vm_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_AF16_Vm_P3p_SS_mod <- data_AF16_Vm_P3p_SS_mod[["mean.obs"]]
      va_data_AF16_Vm_P3p_SS_mod <- data_AF16_Vm_P3p_SS_mod[["var.a.obs"]]
      vp_data_AF16_Vm_P3p_SS_mod <- data_AF16_Vm_P3p_SS_mod[["var.obs"]]
      
      Evol_data_AF16_Vm_P3p_SS_mod <- (va_data_AF16_Vm_P3p_SS_mod/2) / (trait_mean_data_AF16_Vm_P3p_SS_mod)^2
      
      mean(h2_data_AF16_Vm_P3p_SS_mod)
      mean(trait_mean_data_AF16_Vm_P3p_SS_mod)
      mean(va_data_AF16_Vm_P3p_SS_mod)
      mean(vp_data_AF16_Vm_P3p_SS_mod)
      mean(Evol_data_AF16_Vm_P3p_SS_mod)
      
    }
    
  }
  
  
 
  
  
  ##---- AF16 P4p ----
  
  
  #AF16_CONTROL_P4p_SS_mod 
  {
    
    AF16_CONTROL_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_AF16_bi_CONTROL,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)            
    
    saveRDS(AF16_CONTROL_P4p_SS_mod, file = "AF16_CONTROL_P4p_SS_mod.rds")
    AF16_CONTROL_P4p_SS_mod <- readRDS("AF16_CONTROL_P4p_SS_mod.rds")
    
    summary(AF16_CONTROL_P4p_SS_mod) 
    plotTrace(AF16_CONTROL_P4p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("AF16_CONTROL_P4p_SS_mod.pdf")
    plot(AF16_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(AF16_CONTROL_P4p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(AF16_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(AF16_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(AF16_CONTROL_P4p_SS_mod[["VCV"]])
    #autocorr.plot(AF16_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(AF16_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_CONTROL_P4p_SS_mod <- AF16_CONTROL_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_AF16_CONTROL_P4p_SS_mod <- AF16_CONTROL_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_AF16_CONTROL_P4p_SS_mod <- rowSums(AF16_CONTROL_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_AF16_CONTROL_P4p_SS_mod) 
      HPDinterval(va_liab_AF16_CONTROL_P4p_SS_mod) 
      
      mean(vlat_AF16_CONTROL_P4p_SS_mod) 
      
      #variance of fixed effects
      X_AF16_CONTROL_P4p_SS_mod <- AF16_CONTROL_P4p_SS_mod[["X"]]
      beta_AF16_CONTROL_P4p_SS_mod <- AF16_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]
      vf_AF16_CONTROL_P4p_SS_mod   <- apply(beta_AF16_CONTROL_P4p_SS_mod, 1, function(b) {var(as.vector(X_AF16_CONTROL_P4p_SS_mod %*% b))}) 
      mean(vf_AF16_CONTROL_P4p_SS_mod) 
      
      
      h2_liab_AF16_CONTROL_P4p_SS_mod <- va_liab_AF16_CONTROL_P4p_SS_mod / (vlat_AF16_CONTROL_P4p_SS_mod + vf_AF16_CONTROL_P4p_SS_mod)
      mean(h2_liab_AF16_CONTROL_P4p_SS_mod) 
      posterior.mode(h2_liab_AF16_CONTROL_P4p_SS_mod)	
      median(h2_liab_AF16_CONTROL_P4p_SS_mod)		
      HPDinterval(h2_liab_AF16_CONTROL_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_AF16_CONTROL_P4p_SS_mod <- rowMeans(AF16_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_AF16_CONTROL_P4p_SS_mod <- (va_liab_AF16_CONTROL_P4p_SS_mod/2) / (trait_mean_liab_AF16_CONTROL_P4p_SS_mod)^2
    mean(Evol_liab_AF16_CONTROL_P4p_SS_mod)
    
    
    #AF16_CONTROL_P4p_SS_mod data scale
    {
      
      predict_AF16_CONTROL_P4p_SS_mod <- map(1:nrow(AF16_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_AF16_CONTROL_P4p_SS_mod %*% AF16_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_AF16_CONTROL_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_AF16_CONTROL_P4p_SS_mod,
                      var.a = AF16_CONTROL_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_CONTROL_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_CONTROL_P4p_SS_mod <- data_AF16_CONTROL_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_AF16_CONTROL_P4p_SS_mod <- data_AF16_CONTROL_P4p_SS_mod[["mean.obs"]]
      va_data_AF16_CONTROL_P4p_SS_mod <- data_AF16_CONTROL_P4p_SS_mod[["var.a.obs"]]
      vp_data_AF16_CONTROL_P4p_SS_mod <- data_AF16_CONTROL_P4p_SS_mod[["var.obs"]]
      
      Evol_data_AF16_CONTROL_P4p_SS_mod <- (va_data_AF16_CONTROL_P4p_SS_mod/2) / (trait_mean_data_AF16_CONTROL_P4p_SS_mod)^2
      
      mean(h2_data_AF16_CONTROL_P4p_SS_mod) 
      mean(trait_mean_data_AF16_CONTROL_P4p_SS_mod)
      mean(va_data_AF16_CONTROL_P4p_SS_mod) 
      mean(vp_data_AF16_CONTROL_P4p_SS_mod)
      mean(Evol_data_AF16_CONTROL_P4p_SS_mod)
      
    }
    
  }
  
  #AF16_MA_P4p_SS_mod 
  {
    
    AF16_MA_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_AF16_bi_MA,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)         
    
    saveRDS(AF16_MA_P4p_SS_mod, file = "AF16_MA_P4p_SS_mod.rds")
    AF16_MA_P4p_SS_mod <- readRDS("AF16_MA_P4p_SS_mod.rds")
    
    summary(AF16_MA_P4p_SS_mod) 
    #plot(AF16_MA_P4p_SS_mod)
    
    # traces and posterior densities
    pdf("AF16_MA_P4p_SS_mod.pdf")
    plot(AF16_MA_P4p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(AF16_MA_P4p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_MA_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(AF16_MA_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_MA_P4p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(AF16_MA_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(AF16_MA_P4p_SS_mod[["VCV"]])
    #autocorr.plot(AF16_MA_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_MA_P4p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(AF16_MA_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_MA_P4p_SS_mod <- AF16_MA_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_AF16_MA_P4p_SS_mod <- AF16_MA_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_AF16_MA_P4p_SS_mod <- rowSums(AF16_MA_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_AF16_MA_P4p_SS_mod) 
      HPDinterval(va_liab_AF16_MA_P4p_SS_mod) 
      
      mean(vlat_AF16_MA_P4p_SS_mod) 
      
      #variance of fixed effects
      X_AF16_MA_P4p_SS_mod <- AF16_MA_P4p_SS_mod[["X"]]
      beta_AF16_MA_P4p_SS_mod <- AF16_MA_P4p_SS_mod[["Sol"]][,c(1:4)]
      vf_AF16_MA_P4p_SS_mod   <- apply(beta_AF16_MA_P4p_SS_mod, 1, function(b) {var(as.vector(X_AF16_MA_P4p_SS_mod %*% b))}) 
      mean(vf_AF16_MA_P4p_SS_mod) 
      
      h2_liab_AF16_MA_P4p_SS_mod <- va_liab_AF16_MA_P4p_SS_mod / (vlat_AF16_MA_P4p_SS_mod + vf_AF16_MA_P4p_SS_mod)
      mean(h2_liab_AF16_MA_P4p_SS_mod) 
      posterior.mode(h2_liab_AF16_MA_P4p_SS_mod)	
      median(h2_liab_AF16_MA_P4p_SS_mod)		
      HPDinterval(h2_liab_AF16_MA_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_AF16_MA_P4p_SS_mod <- rowMeans(AF16_MA_P4p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_AF16_MA_P4p_SS_mod <- (va_liab_AF16_MA_P4p_SS_mod/2) / (trait_mean_liab_AF16_MA_P4p_SS_mod)^2
    mean(Evol_liab_AF16_MA_P4p_SS_mod)
    
    #AF16_MA_P4p_SS_mod data scale
    {
      
      predict_AF16_MA_P4p_SS_mod <- map(1:nrow(AF16_MA_P4p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_AF16_MA_P4p_SS_mod %*% AF16_MA_P4p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_AF16_MA_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_AF16_MA_P4p_SS_mod,
                      var.a = AF16_MA_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_MA_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_MA_P4p_SS_mod <- data_AF16_MA_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_AF16_MA_P4p_SS_mod <- data_AF16_MA_P4p_SS_mod[["mean.obs"]]
      va_data_AF16_MA_P4p_SS_mod <- data_AF16_MA_P4p_SS_mod[["var.a.obs"]]
      vp_data_AF16_MA_P4p_SS_mod <- data_AF16_MA_P4p_SS_mod[["var.obs"]]
      
      Evol_data_AF16_MA_P4p_SS_mod <- (va_data_AF16_MA_P4p_SS_mod/2) / (trait_mean_data_AF16_MA_P4p_SS_mod)^2
      
      mean(h2_data_AF16_MA_P4p_SS_mod)
      mean(trait_mean_data_AF16_MA_P4p_SS_mod)
      mean(va_data_AF16_MA_P4p_SS_mod)
      mean(vp_data_AF16_MA_P4p_SS_mod)
      mean(Evol_data_AF16_MA_P4p_SS_mod)
      
    }
    
  }
  
  #AF16_Vm_P4p_SS_mod 
  {
    
    AF16_Vm_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer + Treatment -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_AF16_data,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)            
    
    saveRDS(AF16_Vm_P4p_SS_mod, file = "AF16_Vm_P4p_SS_mod.rds")
    AF16_Vm_P4p_SS_mod <- readRDS("AF16_Vm_P4p_SS_mod.rds")
    
    summary(AF16_Vm_P4p_SS_mod) 
    plotTrace(AF16_Vm_P4p_SS_mod$Sol)
    
    # traces and posterior densities
    pdf("AF16_Vm_P4p_SS_mod.pdf")
    plot(AF16_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(AF16_Vm_P4p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(AF16_Vm_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_Vm_P4p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(AF16_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(AF16_Vm_P4p_SS_mod[["VCV"]])
    #autocorr.plot(AF16_Vm_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(AF16_Vm_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_Vm_P4p_SS_mod <- AF16_Vm_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_AF16_Vm_P4p_SS_mod <- AF16_Vm_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_AF16_Vm_P4p_SS_mod <- rowSums(AF16_Vm_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_AF16_Vm_P4p_SS_mod) 
      HPDinterval(va_liab_AF16_Vm_P4p_SS_mod) 
      
      mean(vlat_AF16_Vm_P4p_SS_mod) 
      
      #variance of fixed effects
      X_AF16_Vm_P4p_SS_mod <- AF16_Vm_P4p_SS_mod[["X"]]
      beta_AF16_Vm_P4p_SS_mod <- AF16_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]
      vf_AF16_Vm_P4p_SS_mod   <- apply(beta_AF16_Vm_P4p_SS_mod, 1, function(b) {var(as.vector(X_AF16_Vm_P4p_SS_mod %*% b))}) 
      mean(vf_AF16_Vm_P4p_SS_mod) 
      
      
      h2_liab_AF16_Vm_P4p_SS_mod <- va_liab_AF16_Vm_P4p_SS_mod / (vlat_AF16_Vm_P4p_SS_mod + vf_AF16_Vm_P4p_SS_mod)
      mean(h2_liab_AF16_Vm_P4p_SS_mod) 
      posterior.mode(h2_liab_AF16_Vm_P4p_SS_mod)	
      median(h2_liab_AF16_Vm_P4p_SS_mod)		
      HPDinterval(h2_liab_AF16_Vm_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_AF16_Vm_P4p_SS_mod <- ((rowMeans(AF16_Vm_P4p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(AF16_Vm_P4p_SS_mod[["Sol"]][,c(1:4)]) + AF16_Vm_P4p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_AF16_Vm_P4p_SS_mod <- (va_liab_AF16_Vm_P4p_SS_mod/2) / (trait_mean_liab_AF16_Vm_P4p_SS_mod)^2
    mean(Evol_liab_AF16_Vm_P4p_SS_mod)
    
    
    #AF16_Vm_P4p_SS_mod data scale
    {
      
      predict_AF16_Vm_P4p_SS_mod <- map(1:nrow(AF16_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_AF16_Vm_P4p_SS_mod %*% AF16_Vm_P4p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_AF16_Vm_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_AF16_Vm_P4p_SS_mod,
                      var.a = AF16_Vm_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_Vm_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_Vm_P4p_SS_mod <- data_AF16_Vm_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_AF16_Vm_P4p_SS_mod <- data_AF16_Vm_P4p_SS_mod[["mean.obs"]]
      va_data_AF16_Vm_P4p_SS_mod <- data_AF16_Vm_P4p_SS_mod[["var.a.obs"]]
      vp_data_AF16_Vm_P4p_SS_mod <- data_AF16_Vm_P4p_SS_mod[["var.obs"]]
      
      Evol_data_AF16_Vm_P4p_SS_mod <- (va_data_AF16_Vm_P4p_SS_mod/2) / (trait_mean_data_AF16_Vm_P4p_SS_mod)^2
      
      mean(h2_data_AF16_Vm_P4p_SS_mod)
      mean(trait_mean_data_AF16_Vm_P4p_SS_mod)
      mean(va_data_AF16_Vm_P4p_SS_mod)
      mean(vp_data_AF16_Vm_P4p_SS_mod)
      mean(Evol_data_AF16_Vm_P4p_SS_mod)
      
    }
    
  }
  
  
  
  
  
  ##---- AF16 P8p ----
  
  
  #AF16_CONTROL_P8p_SS_mod 
  {
    
    AF16_CONTROL_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_AF16_bi_CONTROL,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)            
    
    saveRDS(AF16_CONTROL_P8p_SS_mod, file = "AF16_CONTROL_P8p_SS_mod.rds")
    AF16_CONTROL_P8p_SS_mod <- readRDS("AF16_CONTROL_P8p_SS_mod.rds")
    
    summary(AF16_CONTROL_P8p_SS_mod) 
    plotTrace(AF16_CONTROL_P8p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("AF16_CONTROL_P8p_SS_mod.pdf")
    plot(AF16_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(AF16_CONTROL_P8p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(AF16_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(AF16_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(AF16_CONTROL_P8p_SS_mod[["VCV"]])
    #autocorr.plot(AF16_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(AF16_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_CONTROL_P8p_SS_mod <- AF16_CONTROL_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_AF16_CONTROL_P8p_SS_mod <- AF16_CONTROL_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_AF16_CONTROL_P8p_SS_mod <- rowSums(AF16_CONTROL_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_AF16_CONTROL_P8p_SS_mod) 
      HPDinterval(va_liab_AF16_CONTROL_P8p_SS_mod) 
      
      mean(vlat_AF16_CONTROL_P8p_SS_mod) 
      
      #variance of fixed effects
      X_AF16_CONTROL_P8p_SS_mod <- AF16_CONTROL_P8p_SS_mod[["X"]]
      beta_AF16_CONTROL_P8p_SS_mod <- AF16_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]
      vf_AF16_CONTROL_P8p_SS_mod   <- apply(beta_AF16_CONTROL_P8p_SS_mod, 1, function(b) {var(as.vector(X_AF16_CONTROL_P8p_SS_mod %*% b))}) 
      mean(vf_AF16_CONTROL_P8p_SS_mod) 
      
      
      h2_liab_AF16_CONTROL_P8p_SS_mod <- va_liab_AF16_CONTROL_P8p_SS_mod / (vlat_AF16_CONTROL_P8p_SS_mod + vf_AF16_CONTROL_P8p_SS_mod)
      mean(h2_liab_AF16_CONTROL_P8p_SS_mod) 
      posterior.mode(h2_liab_AF16_CONTROL_P8p_SS_mod)	
      median(h2_liab_AF16_CONTROL_P8p_SS_mod)		
      HPDinterval(h2_liab_AF16_CONTROL_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_AF16_CONTROL_P8p_SS_mod <- rowMeans(AF16_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_AF16_CONTROL_P8p_SS_mod <- (va_liab_AF16_CONTROL_P8p_SS_mod/2) / (trait_mean_liab_AF16_CONTROL_P8p_SS_mod)^2
    mean(Evol_liab_AF16_CONTROL_P8p_SS_mod)
    
    
    #AF16_CONTROL_P8p_SS_mod data scale
    {
      
      predict_AF16_CONTROL_P8p_SS_mod <- map(1:nrow(AF16_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_AF16_CONTROL_P8p_SS_mod %*% AF16_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_AF16_CONTROL_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_AF16_CONTROL_P8p_SS_mod,
                      var.a = AF16_CONTROL_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_CONTROL_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_CONTROL_P8p_SS_mod <- data_AF16_CONTROL_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_AF16_CONTROL_P8p_SS_mod <- data_AF16_CONTROL_P8p_SS_mod[["mean.obs"]]
      va_data_AF16_CONTROL_P8p_SS_mod <- data_AF16_CONTROL_P8p_SS_mod[["var.a.obs"]]
      vp_data_AF16_CONTROL_P8p_SS_mod <- data_AF16_CONTROL_P8p_SS_mod[["var.obs"]]
      
      Evol_data_AF16_CONTROL_P8p_SS_mod <- (va_data_AF16_CONTROL_P8p_SS_mod/2) / (trait_mean_data_AF16_CONTROL_P8p_SS_mod)^2
      
      mean(h2_data_AF16_CONTROL_P8p_SS_mod) 
      mean(trait_mean_data_AF16_CONTROL_P8p_SS_mod)
      mean(va_data_AF16_CONTROL_P8p_SS_mod) 
      mean(vp_data_AF16_CONTROL_P8p_SS_mod)
      mean(Evol_data_AF16_CONTROL_P8p_SS_mod)
      
    }
    
  }
  
  #AF16_MA_P8p_SS_mod 
  {
    
    AF16_MA_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_AF16_bi_MA,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)         
    
    saveRDS(AF16_MA_P8p_SS_mod, file = "AF16_MA_P8p_SS_mod.rds")
    AF16_MA_P8p_SS_mod <- readRDS("AF16_MA_P8p_SS_mod.rds")
    
    summary(AF16_MA_P8p_SS_mod) 
    #plot(AF16_MA_P8p_SS_mod)
    
    # traces and posterior densities
    pdf("AF16_MA_P8p_SS_mod.pdf")
    plot(AF16_MA_P8p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(AF16_MA_P8p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_MA_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(AF16_MA_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_MA_P8p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(AF16_MA_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(AF16_MA_P8p_SS_mod[["VCV"]])
    #autocorr.plot(AF16_MA_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_MA_P8p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(AF16_MA_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_MA_P8p_SS_mod <- AF16_MA_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_AF16_MA_P8p_SS_mod <- AF16_MA_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_AF16_MA_P8p_SS_mod <- rowSums(AF16_MA_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_AF16_MA_P8p_SS_mod) 
      HPDinterval(va_liab_AF16_MA_P8p_SS_mod) 
      
      mean(vlat_AF16_MA_P8p_SS_mod) 
      
      #variance of fixed effects
      X_AF16_MA_P8p_SS_mod <- AF16_MA_P8p_SS_mod[["X"]]
      beta_AF16_MA_P8p_SS_mod <- AF16_MA_P8p_SS_mod[["Sol"]][,c(1:4)]
      vf_AF16_MA_P8p_SS_mod   <- apply(beta_AF16_MA_P8p_SS_mod, 1, function(b) {var(as.vector(X_AF16_MA_P8p_SS_mod %*% b))}) 
      mean(vf_AF16_MA_P8p_SS_mod) 
      
      h2_liab_AF16_MA_P8p_SS_mod <- va_liab_AF16_MA_P8p_SS_mod / (vlat_AF16_MA_P8p_SS_mod + vf_AF16_MA_P8p_SS_mod)
      mean(h2_liab_AF16_MA_P8p_SS_mod) 
      posterior.mode(h2_liab_AF16_MA_P8p_SS_mod)	
      median(h2_liab_AF16_MA_P8p_SS_mod)		
      HPDinterval(h2_liab_AF16_MA_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_AF16_MA_P8p_SS_mod <- rowMeans(AF16_MA_P8p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_AF16_MA_P8p_SS_mod <- (va_liab_AF16_MA_P8p_SS_mod/2) / (trait_mean_liab_AF16_MA_P8p_SS_mod)^2
    mean(Evol_liab_AF16_MA_P8p_SS_mod)
    
    #AF16_MA_P8p_SS_mod data scale
    {
      
      predict_AF16_MA_P8p_SS_mod <- map(1:nrow(AF16_MA_P8p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_AF16_MA_P8p_SS_mod %*% AF16_MA_P8p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_AF16_MA_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_AF16_MA_P8p_SS_mod,
                      var.a = AF16_MA_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_MA_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_MA_P8p_SS_mod <- data_AF16_MA_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_AF16_MA_P8p_SS_mod <- data_AF16_MA_P8p_SS_mod[["mean.obs"]]
      va_data_AF16_MA_P8p_SS_mod <- data_AF16_MA_P8p_SS_mod[["var.a.obs"]]
      vp_data_AF16_MA_P8p_SS_mod <- data_AF16_MA_P8p_SS_mod[["var.obs"]]
      
      Evol_data_AF16_MA_P8p_SS_mod <- (va_data_AF16_MA_P8p_SS_mod/2) / (trait_mean_data_AF16_MA_P8p_SS_mod)^2
      
      mean(h2_data_AF16_MA_P8p_SS_mod)
      mean(trait_mean_data_AF16_MA_P8p_SS_mod)
      mean(va_data_AF16_MA_P8p_SS_mod)
      mean(vp_data_AF16_MA_P8p_SS_mod)
      mean(Evol_data_AF16_MA_P8p_SS_mod)
      
    }
    
  }
  
  #AF16_Vm_P8p_SS_mod 
  {
    
    AF16_Vm_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer + Treatment -1,
                                  random      = ~ LineB + BlockRep,
                                  family      = "threshold",
                                  data        = Vm_AF16_data,
                                  prior       = prior_bi_block,
                                  nitt        = 1260000,       
                                  thin        = 500,           
                                  burnin      = 10000,
                                  trunc       = TRUE,
                                  pr          = TRUE,
                                  pl          = TRUE)            
    
    saveRDS(AF16_Vm_P8p_SS_mod, file = "AF16_Vm_P8p_SS_mod.rds")
    AF16_Vm_P8p_SS_mod <- readRDS("AF16_Vm_P8p_SS_mod.rds")
    
    summary(AF16_Vm_P8p_SS_mod) 
    plotTrace(AF16_Vm_P8p_SS_mod$Sol)
    
    # traces and posterior densities
    pdf("AF16_Vm_P8p_SS_mod.pdf")
    plot(AF16_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(AF16_Vm_P8p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(AF16_Vm_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_Vm_P8p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(AF16_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(AF16_Vm_P8p_SS_mod[["VCV"]])
    #autocorr.plot(AF16_Vm_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(AF16_Vm_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_Vm_P8p_SS_mod <- AF16_Vm_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_AF16_Vm_P8p_SS_mod <- AF16_Vm_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_AF16_Vm_P8p_SS_mod <- rowSums(AF16_Vm_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_AF16_Vm_P8p_SS_mod) 
      HPDinterval(va_liab_AF16_Vm_P8p_SS_mod) 
      
      mean(vlat_AF16_Vm_P8p_SS_mod) 
      
      #variance of fixed effects
      X_AF16_Vm_P8p_SS_mod <- AF16_Vm_P8p_SS_mod[["X"]]
      beta_AF16_Vm_P8p_SS_mod <- AF16_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]
      vf_AF16_Vm_P8p_SS_mod   <- apply(beta_AF16_Vm_P8p_SS_mod, 1, function(b) {var(as.vector(X_AF16_Vm_P8p_SS_mod %*% b))}) 
      mean(vf_AF16_Vm_P8p_SS_mod) 
      
      
      h2_liab_AF16_Vm_P8p_SS_mod <- va_liab_AF16_Vm_P8p_SS_mod / (vlat_AF16_Vm_P8p_SS_mod + vf_AF16_Vm_P8p_SS_mod)
      mean(h2_liab_AF16_Vm_P8p_SS_mod) 
      posterior.mode(h2_liab_AF16_Vm_P8p_SS_mod)	
      median(h2_liab_AF16_Vm_P8p_SS_mod)		
      HPDinterval(h2_liab_AF16_Vm_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_AF16_Vm_P8p_SS_mod <- ((rowMeans(AF16_Vm_P8p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(AF16_Vm_P8p_SS_mod[["Sol"]][,c(1:4)]) + AF16_Vm_P8p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_AF16_Vm_P8p_SS_mod <- (va_liab_AF16_Vm_P8p_SS_mod/2) / (trait_mean_liab_AF16_Vm_P8p_SS_mod)^2
    mean(Evol_liab_AF16_Vm_P8p_SS_mod)
    
    
    #AF16_Vm_P8p_SS_mod data scale
    {
      
      predict_AF16_Vm_P8p_SS_mod <- map(1:nrow(AF16_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_AF16_Vm_P8p_SS_mod %*% AF16_Vm_P8p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_AF16_Vm_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_AF16_Vm_P8p_SS_mod,
                      var.a = AF16_Vm_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_Vm_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_Vm_P8p_SS_mod <- data_AF16_Vm_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_AF16_Vm_P8p_SS_mod <- data_AF16_Vm_P8p_SS_mod[["mean.obs"]]
      va_data_AF16_Vm_P8p_SS_mod <- data_AF16_Vm_P8p_SS_mod[["var.a.obs"]]
      vp_data_AF16_Vm_P8p_SS_mod <- data_AF16_Vm_P8p_SS_mod[["var.obs"]]
      
      Evol_data_AF16_Vm_P8p_SS_mod <- (va_data_AF16_Vm_P8p_SS_mod/2) / (trait_mean_data_AF16_Vm_P8p_SS_mod)^2
      
      mean(h2_data_AF16_Vm_P8p_SS_mod)
      mean(trait_mean_data_AF16_Vm_P8p_SS_mod)
      mean(va_data_AF16_Vm_P8p_SS_mod)
      mean(vp_data_AF16_Vm_P8p_SS_mod)
      mean(Evol_data_AF16_Vm_P8p_SS_mod)
      
    }
    
  }
  
##---- AF16 P5p ----
  
  
  #AF16_CONTROL_P5p_wt_mod 
  {
    
    AF16_CONTROL_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_AF16_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(AF16_CONTROL_P5p_wt_mod, file = "AF16_CONTROL_P5p_wt_mod.rds")
    AF16_CONTROL_P5p_wt_mod <- readRDS("AF16_CONTROL_P5p_wt_mod.rds")
    
    summary(AF16_CONTROL_P5p_wt_mod) 
    plotTrace(AF16_CONTROL_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("AF16_CONTROL_P5p_wt_mod.pdf")
    plot(AF16_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(AF16_CONTROL_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(AF16_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(AF16_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(AF16_CONTROL_P5p_wt_mod[["VCV"]])
    #autocorr.plot(AF16_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(AF16_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_CONTROL_P5p_wt_mod <- AF16_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_AF16_CONTROL_P5p_wt_mod <- AF16_CONTROL_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_AF16_CONTROL_P5p_wt_mod <- rowSums(AF16_CONTROL_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_AF16_CONTROL_P5p_wt_mod) 
      HPDinterval(va_liab_AF16_CONTROL_P5p_wt_mod) 
      
      mean(vlat_AF16_CONTROL_P5p_wt_mod) 
      
      #variance of fixed effects
      X_AF16_CONTROL_P5p_wt_mod <- AF16_CONTROL_P5p_wt_mod[["X"]]
      beta_AF16_CONTROL_P5p_wt_mod <- AF16_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_AF16_CONTROL_P5p_wt_mod   <- apply(beta_AF16_CONTROL_P5p_wt_mod, 1, function(b) {var(as.vector(X_AF16_CONTROL_P5p_wt_mod %*% b))}) 
      mean(vf_AF16_CONTROL_P5p_wt_mod) 
      
      
      h2_liab_AF16_CONTROL_P5p_wt_mod <- va_liab_AF16_CONTROL_P5p_wt_mod / (vlat_AF16_CONTROL_P5p_wt_mod + vf_AF16_CONTROL_P5p_wt_mod)
      mean(h2_liab_AF16_CONTROL_P5p_wt_mod) 
      posterior.mode(h2_liab_AF16_CONTROL_P5p_wt_mod)	
      median(h2_liab_AF16_CONTROL_P5p_wt_mod)		
      HPDinterval(h2_liab_AF16_CONTROL_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_AF16_CONTROL_P5p_wt_mod <- rowMeans(AF16_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_AF16_CONTROL_P5p_wt_mod <- (va_liab_AF16_CONTROL_P5p_wt_mod/2) / (trait_mean_liab_AF16_CONTROL_P5p_wt_mod)^2
    mean(Evol_liab_AF16_CONTROL_P5p_wt_mod)
    
    
    #AF16_CONTROL_P5p_wt_mod data scale
    {
      
      predict_AF16_CONTROL_P5p_wt_mod <- map(1:nrow(AF16_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_AF16_CONTROL_P5p_wt_mod %*% AF16_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_AF16_CONTROL_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_AF16_CONTROL_P5p_wt_mod,
                      var.a = AF16_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_CONTROL_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_CONTROL_P5p_wt_mod <- data_AF16_CONTROL_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_AF16_CONTROL_P5p_wt_mod <- data_AF16_CONTROL_P5p_wt_mod[["mean.obs"]]
      va_data_AF16_CONTROL_P5p_wt_mod <- data_AF16_CONTROL_P5p_wt_mod[["var.a.obs"]]
      vp_data_AF16_CONTROL_P5p_wt_mod <- data_AF16_CONTROL_P5p_wt_mod[["var.obs"]]
      
      Evol_data_AF16_CONTROL_P5p_wt_mod <- (va_data_AF16_CONTROL_P5p_wt_mod/2) / (trait_mean_data_AF16_CONTROL_P5p_wt_mod)^2
      
      mean(h2_data_AF16_CONTROL_P5p_wt_mod) 
      mean(trait_mean_data_AF16_CONTROL_P5p_wt_mod)
      mean(va_data_AF16_CONTROL_P5p_wt_mod) 
      mean(vp_data_AF16_CONTROL_P5p_wt_mod)
      mean(Evol_data_AF16_CONTROL_P5p_wt_mod)
      
    }
    
  }
  
  #AF16_MA_P5p_wt_mod 
  {
    
    AF16_MA_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_AF16_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(AF16_MA_P5p_wt_mod, file = "AF16_MA_P5p_wt_mod.rds")
    AF16_MA_P5p_wt_mod <- readRDS("AF16_MA_P5p_wt_mod.rds")
    
    summary(AF16_MA_P5p_wt_mod) 
    #plot(AF16_MA_P5p_wt_mod)
    
    # traces and posterior densities
    pdf("AF16_MA_P5p_wt_mod.pdf")
    plot(AF16_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(AF16_MA_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(AF16_MA_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_MA_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(AF16_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(AF16_MA_P5p_wt_mod[["VCV"]])
    #autocorr.plot(AF16_MA_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(AF16_MA_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_MA_P5p_wt_mod <- AF16_MA_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_AF16_MA_P5p_wt_mod <- AF16_MA_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_AF16_MA_P5p_wt_mod <- rowSums(AF16_MA_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_AF16_MA_P5p_wt_mod) 
      HPDinterval(va_liab_AF16_MA_P5p_wt_mod) 
      
      mean(vlat_AF16_MA_P5p_wt_mod) 
      
      #variance of fixed effects
      X_AF16_MA_P5p_wt_mod <- AF16_MA_P5p_wt_mod[["X"]]
      beta_AF16_MA_P5p_wt_mod <- AF16_MA_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_AF16_MA_P5p_wt_mod   <- apply(beta_AF16_MA_P5p_wt_mod, 1, function(b) {var(as.vector(X_AF16_MA_P5p_wt_mod %*% b))}) 
      mean(vf_AF16_MA_P5p_wt_mod) 
      
      h2_liab_AF16_MA_P5p_wt_mod <- va_liab_AF16_MA_P5p_wt_mod / (vlat_AF16_MA_P5p_wt_mod + vf_AF16_MA_P5p_wt_mod)
      mean(h2_liab_AF16_MA_P5p_wt_mod) 
      posterior.mode(h2_liab_AF16_MA_P5p_wt_mod)	
      median(h2_liab_AF16_MA_P5p_wt_mod)		
      HPDinterval(h2_liab_AF16_MA_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_AF16_MA_P5p_wt_mod <- rowMeans(AF16_MA_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_AF16_MA_P5p_wt_mod <- (va_liab_AF16_MA_P5p_wt_mod/2) / (trait_mean_liab_AF16_MA_P5p_wt_mod)^2
    mean(Evol_liab_AF16_MA_P5p_wt_mod)
    
    #AF16_MA_P5p_wt_mod data scale
    {
      
      predict_AF16_MA_P5p_wt_mod <- map(1:nrow(AF16_MA_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_AF16_MA_P5p_wt_mod %*% AF16_MA_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_AF16_MA_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_AF16_MA_P5p_wt_mod,
                      var.a = AF16_MA_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_MA_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_MA_P5p_wt_mod <- data_AF16_MA_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_AF16_MA_P5p_wt_mod <- data_AF16_MA_P5p_wt_mod[["mean.obs"]]
      va_data_AF16_MA_P5p_wt_mod <- data_AF16_MA_P5p_wt_mod[["var.a.obs"]]
      vp_data_AF16_MA_P5p_wt_mod <- data_AF16_MA_P5p_wt_mod[["var.obs"]]
      
      Evol_data_AF16_MA_P5p_wt_mod <- (va_data_AF16_MA_P5p_wt_mod/2) / (trait_mean_data_AF16_MA_P5p_wt_mod)^2
      
      mean(h2_data_AF16_MA_P5p_wt_mod)
      mean(trait_mean_data_AF16_MA_P5p_wt_mod)
      mean(va_data_AF16_MA_P5p_wt_mod)
      mean(vp_data_AF16_MA_P5p_wt_mod)
      mean(Evol_data_AF16_MA_P5p_wt_mod)
      
    }
    
  }
  
  #AF16_Vm_P5p_wt_mod 
  {
    
    AF16_Vm_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_AF16_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(AF16_Vm_P5p_wt_mod, file = "AF16_Vm_P5p_wt_mod.rds")
    AF16_Vm_P5p_wt_mod <- readRDS("AF16_Vm_P5p_wt_mod.rds")
    
    summary(AF16_Vm_P5p_wt_mod) 
    plotTrace(AF16_Vm_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("AF16_Vm_P5p_wt_mod.pdf")
    plot(AF16_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(AF16_Vm_P5p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(AF16_Vm_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_Vm_P5p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(AF16_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(AF16_Vm_P5p_wt_mod[["VCV"]])
    #autocorr.plot(AF16_Vm_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(AF16_Vm_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_Vm_P5p_wt_mod <- AF16_Vm_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_AF16_Vm_P5p_wt_mod <- AF16_Vm_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_AF16_Vm_P5p_wt_mod <- rowSums(AF16_Vm_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_AF16_Vm_P5p_wt_mod) 
      HPDinterval(va_liab_AF16_Vm_P5p_wt_mod) 
      
      mean(vlat_AF16_Vm_P5p_wt_mod) 
      
      #variance of fixed effects
      X_AF16_Vm_P5p_wt_mod <- AF16_Vm_P5p_wt_mod[["X"]]
      beta_AF16_Vm_P5p_wt_mod <- AF16_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]
      vf_AF16_Vm_P5p_wt_mod   <- apply(beta_AF16_Vm_P5p_wt_mod, 1, function(b) {var(as.vector(X_AF16_Vm_P5p_wt_mod %*% b))}) 
      mean(vf_AF16_Vm_P5p_wt_mod) 
      
      
      h2_liab_AF16_Vm_P5p_wt_mod <- va_liab_AF16_Vm_P5p_wt_mod / (vlat_AF16_Vm_P5p_wt_mod + vf_AF16_Vm_P5p_wt_mod)
      mean(h2_liab_AF16_Vm_P5p_wt_mod) 
      posterior.mode(h2_liab_AF16_Vm_P5p_wt_mod)	
      median(h2_liab_AF16_Vm_P5p_wt_mod)		
      HPDinterval(h2_liab_AF16_Vm_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_AF16_Vm_P5p_wt_mod <- ((rowMeans(AF16_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(AF16_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + AF16_Vm_P5p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_AF16_Vm_P5p_wt_mod <- (va_liab_AF16_Vm_P5p_wt_mod/2) / (trait_mean_liab_AF16_Vm_P5p_wt_mod)^2
    mean(Evol_liab_AF16_Vm_P5p_wt_mod)
    
    
    #AF16_Vm_P5p_wt_mod data scale
    {
      
      predict_AF16_Vm_P5p_wt_mod <- map(1:nrow(AF16_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_AF16_Vm_P5p_wt_mod %*% AF16_Vm_P5p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_AF16_Vm_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_AF16_Vm_P5p_wt_mod,
                      var.a = AF16_Vm_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_Vm_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_Vm_P5p_wt_mod <- data_AF16_Vm_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_AF16_Vm_P5p_wt_mod <- data_AF16_Vm_P5p_wt_mod[["mean.obs"]]
      va_data_AF16_Vm_P5p_wt_mod <- data_AF16_Vm_P5p_wt_mod[["var.a.obs"]]
      vp_data_AF16_Vm_P5p_wt_mod <- data_AF16_Vm_P5p_wt_mod[["var.obs"]]
      
      Evol_data_AF16_Vm_P5p_wt_mod <- (va_data_AF16_Vm_P5p_wt_mod/2) / (trait_mean_data_AF16_Vm_P5p_wt_mod)^2
      
      mean(h2_data_AF16_Vm_P5p_wt_mod)
      mean(trait_mean_data_AF16_Vm_P5p_wt_mod)
      mean(va_data_AF16_Vm_P5p_wt_mod)
      mean(vp_data_AF16_Vm_P5p_wt_mod)
      mean(Evol_data_AF16_Vm_P5p_wt_mod)
      
    }
    
  }
  
  
  
  
  
  ##---- AF16 P6p ----
  
  
  #AF16_CONTROL_P6p_wt_mod 
  {
    
    AF16_CONTROL_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_AF16_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(AF16_CONTROL_P6p_wt_mod, file = "AF16_CONTROL_P6p_wt_mod.rds")
    AF16_CONTROL_P6p_wt_mod <- readRDS("AF16_CONTROL_P6p_wt_mod.rds")
    
    summary(AF16_CONTROL_P6p_wt_mod) 
    plotTrace(AF16_CONTROL_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("AF16_CONTROL_P6p_wt_mod.pdf")
    plot(AF16_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(AF16_CONTROL_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(AF16_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(AF16_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(AF16_CONTROL_P6p_wt_mod[["VCV"]])
    #autocorr.plot(AF16_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(AF16_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_CONTROL_P6p_wt_mod <- AF16_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_AF16_CONTROL_P6p_wt_mod <- AF16_CONTROL_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_AF16_CONTROL_P6p_wt_mod <- rowSums(AF16_CONTROL_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_AF16_CONTROL_P6p_wt_mod) 
      HPDinterval(va_liab_AF16_CONTROL_P6p_wt_mod) 
      
      mean(vlat_AF16_CONTROL_P6p_wt_mod) 
      
      #variance of fixed effects
      X_AF16_CONTROL_P6p_wt_mod <- AF16_CONTROL_P6p_wt_mod[["X"]]
      beta_AF16_CONTROL_P6p_wt_mod <- AF16_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_AF16_CONTROL_P6p_wt_mod   <- apply(beta_AF16_CONTROL_P6p_wt_mod, 1, function(b) {var(as.vector(X_AF16_CONTROL_P6p_wt_mod %*% b))}) 
      mean(vf_AF16_CONTROL_P6p_wt_mod) 
      
      
      h2_liab_AF16_CONTROL_P6p_wt_mod <- va_liab_AF16_CONTROL_P6p_wt_mod / (vlat_AF16_CONTROL_P6p_wt_mod + vf_AF16_CONTROL_P6p_wt_mod)
      mean(h2_liab_AF16_CONTROL_P6p_wt_mod) 
      posterior.mode(h2_liab_AF16_CONTROL_P6p_wt_mod)	
      median(h2_liab_AF16_CONTROL_P6p_wt_mod)		
      HPDinterval(h2_liab_AF16_CONTROL_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_AF16_CONTROL_P6p_wt_mod <- rowMeans(AF16_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_AF16_CONTROL_P6p_wt_mod <- (va_liab_AF16_CONTROL_P6p_wt_mod/2) / (trait_mean_liab_AF16_CONTROL_P6p_wt_mod)^2
    mean(Evol_liab_AF16_CONTROL_P6p_wt_mod)
    
    
    #AF16_CONTROL_P6p_wt_mod data scale
    {
      
      predict_AF16_CONTROL_P6p_wt_mod <- map(1:nrow(AF16_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_AF16_CONTROL_P6p_wt_mod %*% AF16_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_AF16_CONTROL_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_AF16_CONTROL_P6p_wt_mod,
                      var.a = AF16_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_CONTROL_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_CONTROL_P6p_wt_mod <- data_AF16_CONTROL_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_AF16_CONTROL_P6p_wt_mod <- data_AF16_CONTROL_P6p_wt_mod[["mean.obs"]]
      va_data_AF16_CONTROL_P6p_wt_mod <- data_AF16_CONTROL_P6p_wt_mod[["var.a.obs"]]
      vp_data_AF16_CONTROL_P6p_wt_mod <- data_AF16_CONTROL_P6p_wt_mod[["var.obs"]]
      
      Evol_data_AF16_CONTROL_P6p_wt_mod <- (va_data_AF16_CONTROL_P6p_wt_mod/2) / (trait_mean_data_AF16_CONTROL_P6p_wt_mod)^2
      
      mean(h2_data_AF16_CONTROL_P6p_wt_mod) 
      mean(trait_mean_data_AF16_CONTROL_P6p_wt_mod)
      mean(va_data_AF16_CONTROL_P6p_wt_mod) 
      mean(vp_data_AF16_CONTROL_P6p_wt_mod)
      mean(Evol_data_AF16_CONTROL_P6p_wt_mod)
      
    }
    
  }
  
  #AF16_MA_P6p_wt_mod 
  {
    
    AF16_MA_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_AF16_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(AF16_MA_P6p_wt_mod, file = "AF16_MA_P6p_wt_mod.rds")
    AF16_MA_P6p_wt_mod <- readRDS("AF16_MA_P6p_wt_mod.rds")
    
    summary(AF16_MA_P6p_wt_mod) 
    #plot(AF16_MA_P6p_wt_mod)
    
    # traces and posterior densities
    pdf("AF16_MA_P6p_wt_mod.pdf")
    plot(AF16_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(AF16_MA_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(AF16_MA_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_MA_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(AF16_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(AF16_MA_P6p_wt_mod[["VCV"]])
    #autocorr.plot(AF16_MA_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(AF16_MA_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_MA_P6p_wt_mod <- AF16_MA_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_AF16_MA_P6p_wt_mod <- AF16_MA_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_AF16_MA_P6p_wt_mod <- rowSums(AF16_MA_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_AF16_MA_P6p_wt_mod) 
      HPDinterval(va_liab_AF16_MA_P6p_wt_mod) 
      
      mean(vlat_AF16_MA_P6p_wt_mod) 
      
      #variance of fixed effects
      X_AF16_MA_P6p_wt_mod <- AF16_MA_P6p_wt_mod[["X"]]
      beta_AF16_MA_P6p_wt_mod <- AF16_MA_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_AF16_MA_P6p_wt_mod   <- apply(beta_AF16_MA_P6p_wt_mod, 1, function(b) {var(as.vector(X_AF16_MA_P6p_wt_mod %*% b))}) 
      mean(vf_AF16_MA_P6p_wt_mod) 
      
      h2_liab_AF16_MA_P6p_wt_mod <- va_liab_AF16_MA_P6p_wt_mod / (vlat_AF16_MA_P6p_wt_mod + vf_AF16_MA_P6p_wt_mod)
      mean(h2_liab_AF16_MA_P6p_wt_mod) 
      posterior.mode(h2_liab_AF16_MA_P6p_wt_mod)	
      median(h2_liab_AF16_MA_P6p_wt_mod)		
      HPDinterval(h2_liab_AF16_MA_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_AF16_MA_P6p_wt_mod <- rowMeans(AF16_MA_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_AF16_MA_P6p_wt_mod <- (va_liab_AF16_MA_P6p_wt_mod/2) / (trait_mean_liab_AF16_MA_P6p_wt_mod)^2
    mean(Evol_liab_AF16_MA_P6p_wt_mod)
    
    #AF16_MA_P6p_wt_mod data scale
    {
      
      predict_AF16_MA_P6p_wt_mod <- map(1:nrow(AF16_MA_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_AF16_MA_P6p_wt_mod %*% AF16_MA_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_AF16_MA_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_AF16_MA_P6p_wt_mod,
                      var.a = AF16_MA_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_MA_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_MA_P6p_wt_mod <- data_AF16_MA_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_AF16_MA_P6p_wt_mod <- data_AF16_MA_P6p_wt_mod[["mean.obs"]]
      va_data_AF16_MA_P6p_wt_mod <- data_AF16_MA_P6p_wt_mod[["var.a.obs"]]
      vp_data_AF16_MA_P6p_wt_mod <- data_AF16_MA_P6p_wt_mod[["var.obs"]]
      
      Evol_data_AF16_MA_P6p_wt_mod <- (va_data_AF16_MA_P6p_wt_mod/2) / (trait_mean_data_AF16_MA_P6p_wt_mod)^2
      
      mean(h2_data_AF16_MA_P6p_wt_mod)
      mean(trait_mean_data_AF16_MA_P6p_wt_mod)
      mean(va_data_AF16_MA_P6p_wt_mod)
      mean(vp_data_AF16_MA_P6p_wt_mod)
      mean(Evol_data_AF16_MA_P6p_wt_mod)
      
    }
    
  }
  
  #AF16_Vm_P6p_wt_mod 
  {
    
    AF16_Vm_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_AF16_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(AF16_Vm_P6p_wt_mod, file = "AF16_Vm_P6p_wt_mod.rds")
    AF16_Vm_P6p_wt_mod <- readRDS("AF16_Vm_P6p_wt_mod.rds")
    
    summary(AF16_Vm_P6p_wt_mod) 
    plotTrace(AF16_Vm_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("AF16_Vm_P6p_wt_mod.pdf")
    plot(AF16_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(AF16_Vm_P6p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(AF16_Vm_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_Vm_P6p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(AF16_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(AF16_Vm_P6p_wt_mod[["VCV"]])
    #autocorr.plot(AF16_Vm_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(AF16_Vm_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_Vm_P6p_wt_mod <- AF16_Vm_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_AF16_Vm_P6p_wt_mod <- AF16_Vm_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_AF16_Vm_P6p_wt_mod <- rowSums(AF16_Vm_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_AF16_Vm_P6p_wt_mod) 
      HPDinterval(va_liab_AF16_Vm_P6p_wt_mod) 
      
      mean(vlat_AF16_Vm_P6p_wt_mod) 
      
      #variance of fixed effects
      X_AF16_Vm_P6p_wt_mod <- AF16_Vm_P6p_wt_mod[["X"]]
      beta_AF16_Vm_P6p_wt_mod <- AF16_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]
      vf_AF16_Vm_P6p_wt_mod   <- apply(beta_AF16_Vm_P6p_wt_mod, 1, function(b) {var(as.vector(X_AF16_Vm_P6p_wt_mod %*% b))}) 
      mean(vf_AF16_Vm_P6p_wt_mod) 
      
      
      h2_liab_AF16_Vm_P6p_wt_mod <- va_liab_AF16_Vm_P6p_wt_mod / (vlat_AF16_Vm_P6p_wt_mod + vf_AF16_Vm_P6p_wt_mod)
      mean(h2_liab_AF16_Vm_P6p_wt_mod) 
      posterior.mode(h2_liab_AF16_Vm_P6p_wt_mod)	
      median(h2_liab_AF16_Vm_P6p_wt_mod)		
      HPDinterval(h2_liab_AF16_Vm_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_AF16_Vm_P6p_wt_mod <- ((rowMeans(AF16_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(AF16_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + AF16_Vm_P6p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_AF16_Vm_P6p_wt_mod <- (va_liab_AF16_Vm_P6p_wt_mod/2) / (trait_mean_liab_AF16_Vm_P6p_wt_mod)^2
    mean(Evol_liab_AF16_Vm_P6p_wt_mod)
    
    
    #AF16_Vm_P6p_wt_mod data scale
    {
      
      predict_AF16_Vm_P6p_wt_mod <- map(1:nrow(AF16_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_AF16_Vm_P6p_wt_mod %*% AF16_Vm_P6p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_AF16_Vm_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_AF16_Vm_P6p_wt_mod,
                      var.a = AF16_Vm_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_Vm_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_Vm_P6p_wt_mod <- data_AF16_Vm_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_AF16_Vm_P6p_wt_mod <- data_AF16_Vm_P6p_wt_mod[["mean.obs"]]
      va_data_AF16_Vm_P6p_wt_mod <- data_AF16_Vm_P6p_wt_mod[["var.a.obs"]]
      vp_data_AF16_Vm_P6p_wt_mod <- data_AF16_Vm_P6p_wt_mod[["var.obs"]]
      
      Evol_data_AF16_Vm_P6p_wt_mod <- (va_data_AF16_Vm_P6p_wt_mod/2) / (trait_mean_data_AF16_Vm_P6p_wt_mod)^2
      
      mean(h2_data_AF16_Vm_P6p_wt_mod)
      mean(trait_mean_data_AF16_Vm_P6p_wt_mod)
      mean(va_data_AF16_Vm_P6p_wt_mod)
      mean(vp_data_AF16_Vm_P6p_wt_mod)
      mean(Evol_data_AF16_Vm_P6p_wt_mod)
      
    }
    
  }
  
  
  
  
  
  ##---- AF16 P7p ----
  
  
  #AF16_CONTROL_P7p_wt_mod 
  {
    
    AF16_CONTROL_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_AF16_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(AF16_CONTROL_P7p_wt_mod, file = "AF16_CONTROL_P7p_wt_mod.rds")
    AF16_CONTROL_P7p_wt_mod <- readRDS("AF16_CONTROL_P7p_wt_mod.rds")
    
    summary(AF16_CONTROL_P7p_wt_mod) 
    plotTrace(AF16_CONTROL_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("AF16_CONTROL_P7p_wt_mod.pdf")
    plot(AF16_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(AF16_CONTROL_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(AF16_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(AF16_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(AF16_CONTROL_P7p_wt_mod[["VCV"]])
    #autocorr.plot(AF16_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(AF16_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_CONTROL_P7p_wt_mod <- AF16_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_AF16_CONTROL_P7p_wt_mod <- AF16_CONTROL_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_AF16_CONTROL_P7p_wt_mod <- rowSums(AF16_CONTROL_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_AF16_CONTROL_P7p_wt_mod) 
      HPDinterval(va_liab_AF16_CONTROL_P7p_wt_mod) 
      
      mean(vlat_AF16_CONTROL_P7p_wt_mod) 
      
      #variance of fixed effects
      X_AF16_CONTROL_P7p_wt_mod <- AF16_CONTROL_P7p_wt_mod[["X"]]
      beta_AF16_CONTROL_P7p_wt_mod <- AF16_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_AF16_CONTROL_P7p_wt_mod   <- apply(beta_AF16_CONTROL_P7p_wt_mod, 1, function(b) {var(as.vector(X_AF16_CONTROL_P7p_wt_mod %*% b))}) 
      mean(vf_AF16_CONTROL_P7p_wt_mod) 
      
      
      h2_liab_AF16_CONTROL_P7p_wt_mod <- va_liab_AF16_CONTROL_P7p_wt_mod / (vlat_AF16_CONTROL_P7p_wt_mod + vf_AF16_CONTROL_P7p_wt_mod)
      mean(h2_liab_AF16_CONTROL_P7p_wt_mod) 
      posterior.mode(h2_liab_AF16_CONTROL_P7p_wt_mod)	
      median(h2_liab_AF16_CONTROL_P7p_wt_mod)		
      HPDinterval(h2_liab_AF16_CONTROL_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_AF16_CONTROL_P7p_wt_mod <- rowMeans(AF16_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_AF16_CONTROL_P7p_wt_mod <- (va_liab_AF16_CONTROL_P7p_wt_mod/2) / (trait_mean_liab_AF16_CONTROL_P7p_wt_mod)^2
    mean(Evol_liab_AF16_CONTROL_P7p_wt_mod)
    
    
    #AF16_CONTROL_P7p_wt_mod data scale
    {
      
      predict_AF16_CONTROL_P7p_wt_mod <- map(1:nrow(AF16_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_AF16_CONTROL_P7p_wt_mod %*% AF16_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_AF16_CONTROL_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_AF16_CONTROL_P7p_wt_mod,
                      var.a = AF16_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_CONTROL_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_CONTROL_P7p_wt_mod <- data_AF16_CONTROL_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_AF16_CONTROL_P7p_wt_mod <- data_AF16_CONTROL_P7p_wt_mod[["mean.obs"]]
      va_data_AF16_CONTROL_P7p_wt_mod <- data_AF16_CONTROL_P7p_wt_mod[["var.a.obs"]]
      vp_data_AF16_CONTROL_P7p_wt_mod <- data_AF16_CONTROL_P7p_wt_mod[["var.obs"]]
      
      Evol_data_AF16_CONTROL_P7p_wt_mod <- (va_data_AF16_CONTROL_P7p_wt_mod/2) / (trait_mean_data_AF16_CONTROL_P7p_wt_mod)^2
      
      mean(h2_data_AF16_CONTROL_P7p_wt_mod) 
      mean(trait_mean_data_AF16_CONTROL_P7p_wt_mod)
      mean(va_data_AF16_CONTROL_P7p_wt_mod) 
      mean(vp_data_AF16_CONTROL_P7p_wt_mod)
      mean(Evol_data_AF16_CONTROL_P7p_wt_mod)
      
    }
    
  }
  
  #AF16_MA_P7p_wt_mod 
  {
    
    AF16_MA_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_AF16_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(AF16_MA_P7p_wt_mod, file = "AF16_MA_P7p_wt_mod.rds")
    AF16_MA_P7p_wt_mod <- readRDS("AF16_MA_P7p_wt_mod.rds")
    
    summary(AF16_MA_P7p_wt_mod) 
    #plot(AF16_MA_P7p_wt_mod)
    
    # traces and posterior densities
    pdf("AF16_MA_P7p_wt_mod.pdf")
    plot(AF16_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(AF16_MA_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(AF16_MA_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_MA_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(AF16_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(AF16_MA_P7p_wt_mod[["VCV"]])
    #autocorr.plot(AF16_MA_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(AF16_MA_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_MA_P7p_wt_mod <- AF16_MA_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_AF16_MA_P7p_wt_mod <- AF16_MA_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_AF16_MA_P7p_wt_mod <- rowSums(AF16_MA_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_AF16_MA_P7p_wt_mod) 
      HPDinterval(va_liab_AF16_MA_P7p_wt_mod) 
      
      mean(vlat_AF16_MA_P7p_wt_mod) 
      
      #variance of fixed effects
      X_AF16_MA_P7p_wt_mod <- AF16_MA_P7p_wt_mod[["X"]]
      beta_AF16_MA_P7p_wt_mod <- AF16_MA_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_AF16_MA_P7p_wt_mod   <- apply(beta_AF16_MA_P7p_wt_mod, 1, function(b) {var(as.vector(X_AF16_MA_P7p_wt_mod %*% b))}) 
      mean(vf_AF16_MA_P7p_wt_mod) 
      
      h2_liab_AF16_MA_P7p_wt_mod <- va_liab_AF16_MA_P7p_wt_mod / (vlat_AF16_MA_P7p_wt_mod + vf_AF16_MA_P7p_wt_mod)
      mean(h2_liab_AF16_MA_P7p_wt_mod) 
      posterior.mode(h2_liab_AF16_MA_P7p_wt_mod)	
      median(h2_liab_AF16_MA_P7p_wt_mod)		
      HPDinterval(h2_liab_AF16_MA_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_AF16_MA_P7p_wt_mod <- rowMeans(AF16_MA_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_AF16_MA_P7p_wt_mod <- (va_liab_AF16_MA_P7p_wt_mod/2) / (trait_mean_liab_AF16_MA_P7p_wt_mod)^2
    mean(Evol_liab_AF16_MA_P7p_wt_mod)
    
    #AF16_MA_P7p_wt_mod data scale
    {
      
      predict_AF16_MA_P7p_wt_mod <- map(1:nrow(AF16_MA_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_AF16_MA_P7p_wt_mod %*% AF16_MA_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_AF16_MA_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_AF16_MA_P7p_wt_mod,
                      var.a = AF16_MA_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_MA_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_MA_P7p_wt_mod <- data_AF16_MA_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_AF16_MA_P7p_wt_mod <- data_AF16_MA_P7p_wt_mod[["mean.obs"]]
      va_data_AF16_MA_P7p_wt_mod <- data_AF16_MA_P7p_wt_mod[["var.a.obs"]]
      vp_data_AF16_MA_P7p_wt_mod <- data_AF16_MA_P7p_wt_mod[["var.obs"]]
      
      Evol_data_AF16_MA_P7p_wt_mod <- (va_data_AF16_MA_P7p_wt_mod/2) / (trait_mean_data_AF16_MA_P7p_wt_mod)^2
      
      mean(h2_data_AF16_MA_P7p_wt_mod)
      mean(trait_mean_data_AF16_MA_P7p_wt_mod)
      mean(va_data_AF16_MA_P7p_wt_mod)
      mean(vp_data_AF16_MA_P7p_wt_mod)
      mean(Evol_data_AF16_MA_P7p_wt_mod)
      
    }
    
  }
  
  #AF16_Vm_P7p_wt_mod 
  {
    
    AF16_Vm_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_AF16_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(AF16_Vm_P7p_wt_mod, file = "AF16_Vm_P7p_wt_mod.rds")
    AF16_Vm_P7p_wt_mod <- readRDS("AF16_Vm_P7p_wt_mod.rds")
    
    summary(AF16_Vm_P7p_wt_mod) 
    plotTrace(AF16_Vm_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("AF16_Vm_P7p_wt_mod.pdf")
    plot(AF16_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(AF16_Vm_P7p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(AF16_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(AF16_Vm_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(AF16_Vm_P7p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(AF16_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(AF16_Vm_P7p_wt_mod[["VCV"]])
    #autocorr.plot(AF16_Vm_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(AF16_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(AF16_Vm_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_AF16_Vm_P7p_wt_mod <- AF16_Vm_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_AF16_Vm_P7p_wt_mod <- AF16_Vm_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_AF16_Vm_P7p_wt_mod <- rowSums(AF16_Vm_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_AF16_Vm_P7p_wt_mod) 
      HPDinterval(va_liab_AF16_Vm_P7p_wt_mod) 
      
      mean(vlat_AF16_Vm_P7p_wt_mod) 
      
      #variance of fixed effects
      X_AF16_Vm_P7p_wt_mod <- AF16_Vm_P7p_wt_mod[["X"]]
      beta_AF16_Vm_P7p_wt_mod <- AF16_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]
      vf_AF16_Vm_P7p_wt_mod   <- apply(beta_AF16_Vm_P7p_wt_mod, 1, function(b) {var(as.vector(X_AF16_Vm_P7p_wt_mod %*% b))}) 
      mean(vf_AF16_Vm_P7p_wt_mod) 
      
      
      h2_liab_AF16_Vm_P7p_wt_mod <- va_liab_AF16_Vm_P7p_wt_mod / (vlat_AF16_Vm_P7p_wt_mod + vf_AF16_Vm_P7p_wt_mod)
      mean(h2_liab_AF16_Vm_P7p_wt_mod) 
      posterior.mode(h2_liab_AF16_Vm_P7p_wt_mod)	
      median(h2_liab_AF16_Vm_P7p_wt_mod)		
      HPDinterval(h2_liab_AF16_Vm_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_AF16_Vm_P7p_wt_mod <- ((rowMeans(AF16_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(AF16_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + AF16_Vm_P7p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_AF16_Vm_P7p_wt_mod <- (va_liab_AF16_Vm_P7p_wt_mod/2) / (trait_mean_liab_AF16_Vm_P7p_wt_mod)^2
    mean(Evol_liab_AF16_Vm_P7p_wt_mod)
    
    
    #AF16_Vm_P7p_wt_mod data scale
    {
      
      predict_AF16_Vm_P7p_wt_mod <- map(1:nrow(AF16_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_AF16_Vm_P7p_wt_mod %*% AF16_Vm_P7p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_AF16_Vm_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_AF16_Vm_P7p_wt_mod,
                      var.a = AF16_Vm_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(AF16_Vm_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_AF16_Vm_P7p_wt_mod <- data_AF16_Vm_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_AF16_Vm_P7p_wt_mod <- data_AF16_Vm_P7p_wt_mod[["mean.obs"]]
      va_data_AF16_Vm_P7p_wt_mod <- data_AF16_Vm_P7p_wt_mod[["var.a.obs"]]
      vp_data_AF16_Vm_P7p_wt_mod <- data_AF16_Vm_P7p_wt_mod[["var.obs"]]
      
      Evol_data_AF16_Vm_P7p_wt_mod <- (va_data_AF16_Vm_P7p_wt_mod/2) / (trait_mean_data_AF16_Vm_P7p_wt_mod)^2
      
      mean(h2_data_AF16_Vm_P7p_wt_mod)
      mean(trait_mean_data_AF16_Vm_P7p_wt_mod)
      mean(va_data_AF16_Vm_P7p_wt_mod)
      mean(vp_data_AF16_Vm_P7p_wt_mod)
      mean(Evol_data_AF16_Vm_P7p_wt_mod)
      
    }
    
  }
  
  
  
}  
 

#PB800----
{
  Vm_PB800_bi_CONTROL <- subset(Vm_PB800_data, Treatment =="CONTROL")
  Vm_PB800_bi_MA <- subset(Vm_PB800_data, Treatment =="MA")
  
  
  ##---- PB800 P3p ----
  
  
  #PB800_CONTROL_P3p_SS_mod 
  {
    
    PB800_CONTROL_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_PB800_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(PB800_CONTROL_P3p_SS_mod, file = "PB800_CONTROL_P3p_SS_mod.rds")
    PB800_CONTROL_P3p_SS_mod <- readRDS("PB800_CONTROL_P3p_SS_mod.rds")
    
    summary(PB800_CONTROL_P3p_SS_mod) 
    plotTrace(PB800_CONTROL_P3p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PB800_CONTROL_P3p_SS_mod.pdf")
    plot(PB800_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(PB800_CONTROL_P3p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB800_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB800_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB800_CONTROL_P3p_SS_mod[["VCV"]])
    #autocorr.plot(PB800_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB800_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_CONTROL_P3p_SS_mod <- PB800_CONTROL_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB800_CONTROL_P3p_SS_mod <- PB800_CONTROL_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_PB800_CONTROL_P3p_SS_mod <- rowSums(PB800_CONTROL_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_PB800_CONTROL_P3p_SS_mod) 
      HPDinterval(va_liab_PB800_CONTROL_P3p_SS_mod) 
      
      mean(vlat_PB800_CONTROL_P3p_SS_mod) 
      
      #variance of fixed effects
      X_PB800_CONTROL_P3p_SS_mod <- PB800_CONTROL_P3p_SS_mod[["X"]]
      beta_PB800_CONTROL_P3p_SS_mod <- PB800_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]
      vf_PB800_CONTROL_P3p_SS_mod   <- apply(beta_PB800_CONTROL_P3p_SS_mod, 1, function(b) {var(as.vector(X_PB800_CONTROL_P3p_SS_mod %*% b))}) 
      mean(vf_PB800_CONTROL_P3p_SS_mod) 
      
      
      h2_liab_PB800_CONTROL_P3p_SS_mod <- va_liab_PB800_CONTROL_P3p_SS_mod / (vlat_PB800_CONTROL_P3p_SS_mod + vf_PB800_CONTROL_P3p_SS_mod)
      mean(h2_liab_PB800_CONTROL_P3p_SS_mod) 
      posterior.mode(h2_liab_PB800_CONTROL_P3p_SS_mod)	
      median(h2_liab_PB800_CONTROL_P3p_SS_mod)		
      HPDinterval(h2_liab_PB800_CONTROL_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB800_CONTROL_P3p_SS_mod <- rowMeans(PB800_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB800_CONTROL_P3p_SS_mod <- (va_liab_PB800_CONTROL_P3p_SS_mod/2) / (trait_mean_liab_PB800_CONTROL_P3p_SS_mod)^2
    mean(Evol_liab_PB800_CONTROL_P3p_SS_mod)
    
    
    #PB800_CONTROL_P3p_SS_mod data scale
    {
      
      predict_PB800_CONTROL_P3p_SS_mod <- map(1:nrow(PB800_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB800_CONTROL_P3p_SS_mod %*% PB800_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PB800_CONTROL_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_PB800_CONTROL_P3p_SS_mod,
                      var.a = PB800_CONTROL_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_CONTROL_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_CONTROL_P3p_SS_mod <- data_PB800_CONTROL_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_PB800_CONTROL_P3p_SS_mod <- data_PB800_CONTROL_P3p_SS_mod[["mean.obs"]]
      va_data_PB800_CONTROL_P3p_SS_mod <- data_PB800_CONTROL_P3p_SS_mod[["var.a.obs"]]
      vp_data_PB800_CONTROL_P3p_SS_mod <- data_PB800_CONTROL_P3p_SS_mod[["var.obs"]]
      
      Evol_data_PB800_CONTROL_P3p_SS_mod <- (va_data_PB800_CONTROL_P3p_SS_mod/2) / (trait_mean_data_PB800_CONTROL_P3p_SS_mod)^2
      
      mean(h2_data_PB800_CONTROL_P3p_SS_mod) 
      mean(trait_mean_data_PB800_CONTROL_P3p_SS_mod)
      mean(va_data_PB800_CONTROL_P3p_SS_mod) 
      mean(vp_data_PB800_CONTROL_P3p_SS_mod)
      mean(Evol_data_PB800_CONTROL_P3p_SS_mod)
      
    }
    
  }
  
  #PB800_MA_P3p_SS_mod 
  {
    
    PB800_MA_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_PB800_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(PB800_MA_P3p_SS_mod, file = "PB800_MA_P3p_SS_mod.rds")
    PB800_MA_P3p_SS_mod <- readRDS("PB800_MA_P3p_SS_mod.rds")
    
    summary(PB800_MA_P3p_SS_mod) 
    #plot(PB800_MA_P3p_SS_mod)
    
    # traces and posterior densities
    pdf("PB800_MA_P3p_SS_mod.pdf")
    plot(PB800_MA_P3p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(PB800_MA_P3p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_MA_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB800_MA_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_MA_P3p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB800_MA_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB800_MA_P3p_SS_mod[["VCV"]])
    #autocorr.plot(PB800_MA_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_MA_P3p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB800_MA_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_MA_P3p_SS_mod <- PB800_MA_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB800_MA_P3p_SS_mod <- PB800_MA_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_PB800_MA_P3p_SS_mod <- rowSums(PB800_MA_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_PB800_MA_P3p_SS_mod) 
      HPDinterval(va_liab_PB800_MA_P3p_SS_mod) 
      
      mean(vlat_PB800_MA_P3p_SS_mod) 
      
      #variance of fixed effects
      X_PB800_MA_P3p_SS_mod <- PB800_MA_P3p_SS_mod[["X"]]
      beta_PB800_MA_P3p_SS_mod <- PB800_MA_P3p_SS_mod[["Sol"]][,c(1:4)]
      vf_PB800_MA_P3p_SS_mod   <- apply(beta_PB800_MA_P3p_SS_mod, 1, function(b) {var(as.vector(X_PB800_MA_P3p_SS_mod %*% b))}) 
      mean(vf_PB800_MA_P3p_SS_mod) 
      
      h2_liab_PB800_MA_P3p_SS_mod <- va_liab_PB800_MA_P3p_SS_mod / (vlat_PB800_MA_P3p_SS_mod + vf_PB800_MA_P3p_SS_mod)
      mean(h2_liab_PB800_MA_P3p_SS_mod) 
      posterior.mode(h2_liab_PB800_MA_P3p_SS_mod)	
      median(h2_liab_PB800_MA_P3p_SS_mod)		
      HPDinterval(h2_liab_PB800_MA_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB800_MA_P3p_SS_mod <- rowMeans(PB800_MA_P3p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB800_MA_P3p_SS_mod <- (va_liab_PB800_MA_P3p_SS_mod/2) / (trait_mean_liab_PB800_MA_P3p_SS_mod)^2
    mean(Evol_liab_PB800_MA_P3p_SS_mod)
    
    #PB800_MA_P3p_SS_mod data scale
    {
      
      predict_PB800_MA_P3p_SS_mod <- map(1:nrow(PB800_MA_P3p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB800_MA_P3p_SS_mod %*% PB800_MA_P3p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PB800_MA_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_PB800_MA_P3p_SS_mod,
                      var.a = PB800_MA_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_MA_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_MA_P3p_SS_mod <- data_PB800_MA_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_PB800_MA_P3p_SS_mod <- data_PB800_MA_P3p_SS_mod[["mean.obs"]]
      va_data_PB800_MA_P3p_SS_mod <- data_PB800_MA_P3p_SS_mod[["var.a.obs"]]
      vp_data_PB800_MA_P3p_SS_mod <- data_PB800_MA_P3p_SS_mod[["var.obs"]]
      
      Evol_data_PB800_MA_P3p_SS_mod <- (va_data_PB800_MA_P3p_SS_mod/2) / (trait_mean_data_PB800_MA_P3p_SS_mod)^2
      
      mean(h2_data_PB800_MA_P3p_SS_mod)
      mean(trait_mean_data_PB800_MA_P3p_SS_mod)
      mean(va_data_PB800_MA_P3p_SS_mod)
      mean(vp_data_PB800_MA_P3p_SS_mod)
      mean(Evol_data_PB800_MA_P3p_SS_mod)
      
    }
    
  }
  
  #PB800_Vm_P3p_SS_mod 
  {
    
    PB800_Vm_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_PB800_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(PB800_Vm_P3p_SS_mod, file = "PB800_Vm_P3p_SS_mod.rds")
    PB800_Vm_P3p_SS_mod <- readRDS("PB800_Vm_P3p_SS_mod.rds")
    
    summary(PB800_Vm_P3p_SS_mod) 
    plotTrace(PB800_Vm_P3p_SS_mod$Sol)
    
    # traces and posterior densities
    pdf("PB800_Vm_P3p_SS_mod.pdf")
    plot(PB800_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(PB800_Vm_P3p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PB800_Vm_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_Vm_P3p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PB800_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PB800_Vm_P3p_SS_mod[["VCV"]])
    #autocorr.plot(PB800_Vm_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PB800_Vm_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_Vm_P3p_SS_mod <- PB800_Vm_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB800_Vm_P3p_SS_mod <- PB800_Vm_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_PB800_Vm_P3p_SS_mod <- rowSums(PB800_Vm_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_PB800_Vm_P3p_SS_mod) 
      HPDinterval(va_liab_PB800_Vm_P3p_SS_mod) 
      
      mean(vlat_PB800_Vm_P3p_SS_mod) 
      
      #variance of fixed effects
      X_PB800_Vm_P3p_SS_mod <- PB800_Vm_P3p_SS_mod[["X"]]
      beta_PB800_Vm_P3p_SS_mod <- PB800_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]
      vf_PB800_Vm_P3p_SS_mod   <- apply(beta_PB800_Vm_P3p_SS_mod, 1, function(b) {var(as.vector(X_PB800_Vm_P3p_SS_mod %*% b))}) 
      mean(vf_PB800_Vm_P3p_SS_mod) 
      
      
      h2_liab_PB800_Vm_P3p_SS_mod <- va_liab_PB800_Vm_P3p_SS_mod / (vlat_PB800_Vm_P3p_SS_mod + vf_PB800_Vm_P3p_SS_mod)
      mean(h2_liab_PB800_Vm_P3p_SS_mod) 
      posterior.mode(h2_liab_PB800_Vm_P3p_SS_mod)	
      median(h2_liab_PB800_Vm_P3p_SS_mod)		
      HPDinterval(h2_liab_PB800_Vm_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PB800_Vm_P3p_SS_mod <- ((rowMeans(PB800_Vm_P3p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(PB800_Vm_P3p_SS_mod[["Sol"]][,c(1:4)]) + PB800_Vm_P3p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PB800_Vm_P3p_SS_mod <- (va_liab_PB800_Vm_P3p_SS_mod/2) / (trait_mean_liab_PB800_Vm_P3p_SS_mod)^2
    mean(Evol_liab_PB800_Vm_P3p_SS_mod)
    
    
    #PB800_Vm_P3p_SS_mod data scale
    {
      
      predict_PB800_Vm_P3p_SS_mod <- map(1:nrow(PB800_Vm_P3p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PB800_Vm_P3p_SS_mod %*% PB800_Vm_P3p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PB800_Vm_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_PB800_Vm_P3p_SS_mod,
                      var.a = PB800_Vm_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_Vm_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_Vm_P3p_SS_mod <- data_PB800_Vm_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_PB800_Vm_P3p_SS_mod <- data_PB800_Vm_P3p_SS_mod[["mean.obs"]]
      va_data_PB800_Vm_P3p_SS_mod <- data_PB800_Vm_P3p_SS_mod[["var.a.obs"]]
      vp_data_PB800_Vm_P3p_SS_mod <- data_PB800_Vm_P3p_SS_mod[["var.obs"]]
      
      Evol_data_PB800_Vm_P3p_SS_mod <- (va_data_PB800_Vm_P3p_SS_mod/2) / (trait_mean_data_PB800_Vm_P3p_SS_mod)^2
      
      mean(h2_data_PB800_Vm_P3p_SS_mod)
      mean(trait_mean_data_PB800_Vm_P3p_SS_mod)
      mean(va_data_PB800_Vm_P3p_SS_mod)
      mean(vp_data_PB800_Vm_P3p_SS_mod)
      mean(Evol_data_PB800_Vm_P3p_SS_mod)
      
    }
    
  }
  
  
  
  
  
  ##---- PB800 P4p ----
  
  
  #PB800_CONTROL_P4p_SS_mod 
  {
    
    PB800_CONTROL_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_PB800_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(PB800_CONTROL_P4p_SS_mod, file = "PB800_CONTROL_P4p_SS_mod.rds")
    PB800_CONTROL_P4p_SS_mod <- readRDS("PB800_CONTROL_P4p_SS_mod.rds")
    
    summary(PB800_CONTROL_P4p_SS_mod) 
    plotTrace(PB800_CONTROL_P4p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PB800_CONTROL_P4p_SS_mod.pdf")
    plot(PB800_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(PB800_CONTROL_P4p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB800_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB800_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB800_CONTROL_P4p_SS_mod[["VCV"]])
    #autocorr.plot(PB800_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB800_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_CONTROL_P4p_SS_mod <- PB800_CONTROL_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB800_CONTROL_P4p_SS_mod <- PB800_CONTROL_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_PB800_CONTROL_P4p_SS_mod <- rowSums(PB800_CONTROL_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_PB800_CONTROL_P4p_SS_mod) 
      HPDinterval(va_liab_PB800_CONTROL_P4p_SS_mod) 
      
      mean(vlat_PB800_CONTROL_P4p_SS_mod) 
      
      #variance of fixed effects
      X_PB800_CONTROL_P4p_SS_mod <- PB800_CONTROL_P4p_SS_mod[["X"]]
      beta_PB800_CONTROL_P4p_SS_mod <- PB800_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]
      vf_PB800_CONTROL_P4p_SS_mod   <- apply(beta_PB800_CONTROL_P4p_SS_mod, 1, function(b) {var(as.vector(X_PB800_CONTROL_P4p_SS_mod %*% b))}) 
      mean(vf_PB800_CONTROL_P4p_SS_mod) 
      
      
      h2_liab_PB800_CONTROL_P4p_SS_mod <- va_liab_PB800_CONTROL_P4p_SS_mod / (vlat_PB800_CONTROL_P4p_SS_mod + vf_PB800_CONTROL_P4p_SS_mod)
      mean(h2_liab_PB800_CONTROL_P4p_SS_mod) 
      posterior.mode(h2_liab_PB800_CONTROL_P4p_SS_mod)	
      median(h2_liab_PB800_CONTROL_P4p_SS_mod)		
      HPDinterval(h2_liab_PB800_CONTROL_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB800_CONTROL_P4p_SS_mod <- rowMeans(PB800_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB800_CONTROL_P4p_SS_mod <- (va_liab_PB800_CONTROL_P4p_SS_mod/2) / (trait_mean_liab_PB800_CONTROL_P4p_SS_mod)^2
    mean(Evol_liab_PB800_CONTROL_P4p_SS_mod)
    
    
    #PB800_CONTROL_P4p_SS_mod data scale
    {
      
      predict_PB800_CONTROL_P4p_SS_mod <- map(1:nrow(PB800_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB800_CONTROL_P4p_SS_mod %*% PB800_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PB800_CONTROL_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_PB800_CONTROL_P4p_SS_mod,
                      var.a = PB800_CONTROL_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_CONTROL_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_CONTROL_P4p_SS_mod <- data_PB800_CONTROL_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_PB800_CONTROL_P4p_SS_mod <- data_PB800_CONTROL_P4p_SS_mod[["mean.obs"]]
      va_data_PB800_CONTROL_P4p_SS_mod <- data_PB800_CONTROL_P4p_SS_mod[["var.a.obs"]]
      vp_data_PB800_CONTROL_P4p_SS_mod <- data_PB800_CONTROL_P4p_SS_mod[["var.obs"]]
      
      Evol_data_PB800_CONTROL_P4p_SS_mod <- (va_data_PB800_CONTROL_P4p_SS_mod/2) / (trait_mean_data_PB800_CONTROL_P4p_SS_mod)^2
      
      mean(h2_data_PB800_CONTROL_P4p_SS_mod) 
      mean(trait_mean_data_PB800_CONTROL_P4p_SS_mod)
      mean(va_data_PB800_CONTROL_P4p_SS_mod) 
      mean(vp_data_PB800_CONTROL_P4p_SS_mod)
      mean(Evol_data_PB800_CONTROL_P4p_SS_mod)
      
    }
    
  }
  
  #PB800_MA_P4p_SS_mod 
  {
    
    PB800_MA_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_PB800_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(PB800_MA_P4p_SS_mod, file = "PB800_MA_P4p_SS_mod.rds")
    PB800_MA_P4p_SS_mod <- readRDS("PB800_MA_P4p_SS_mod.rds")
    
    summary(PB800_MA_P4p_SS_mod) 
    #plot(PB800_MA_P4p_SS_mod)
    
    # traces and posterior densities
    pdf("PB800_MA_P4p_SS_mod.pdf")
    plot(PB800_MA_P4p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(PB800_MA_P4p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_MA_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB800_MA_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_MA_P4p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB800_MA_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB800_MA_P4p_SS_mod[["VCV"]])
    #autocorr.plot(PB800_MA_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_MA_P4p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB800_MA_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_MA_P4p_SS_mod <- PB800_MA_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB800_MA_P4p_SS_mod <- PB800_MA_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_PB800_MA_P4p_SS_mod <- rowSums(PB800_MA_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_PB800_MA_P4p_SS_mod) 
      HPDinterval(va_liab_PB800_MA_P4p_SS_mod) 
      
      mean(vlat_PB800_MA_P4p_SS_mod) 
      
      #variance of fixed effects
      X_PB800_MA_P4p_SS_mod <- PB800_MA_P4p_SS_mod[["X"]]
      beta_PB800_MA_P4p_SS_mod <- PB800_MA_P4p_SS_mod[["Sol"]][,c(1:4)]
      vf_PB800_MA_P4p_SS_mod   <- apply(beta_PB800_MA_P4p_SS_mod, 1, function(b) {var(as.vector(X_PB800_MA_P4p_SS_mod %*% b))}) 
      mean(vf_PB800_MA_P4p_SS_mod) 
      
      h2_liab_PB800_MA_P4p_SS_mod <- va_liab_PB800_MA_P4p_SS_mod / (vlat_PB800_MA_P4p_SS_mod + vf_PB800_MA_P4p_SS_mod)
      mean(h2_liab_PB800_MA_P4p_SS_mod) 
      posterior.mode(h2_liab_PB800_MA_P4p_SS_mod)	
      median(h2_liab_PB800_MA_P4p_SS_mod)		
      HPDinterval(h2_liab_PB800_MA_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB800_MA_P4p_SS_mod <- rowMeans(PB800_MA_P4p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB800_MA_P4p_SS_mod <- (va_liab_PB800_MA_P4p_SS_mod/2) / (trait_mean_liab_PB800_MA_P4p_SS_mod)^2
    mean(Evol_liab_PB800_MA_P4p_SS_mod)
    
    #PB800_MA_P4p_SS_mod data scale
    {
      
      predict_PB800_MA_P4p_SS_mod <- map(1:nrow(PB800_MA_P4p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB800_MA_P4p_SS_mod %*% PB800_MA_P4p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PB800_MA_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_PB800_MA_P4p_SS_mod,
                      var.a = PB800_MA_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_MA_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_MA_P4p_SS_mod <- data_PB800_MA_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_PB800_MA_P4p_SS_mod <- data_PB800_MA_P4p_SS_mod[["mean.obs"]]
      va_data_PB800_MA_P4p_SS_mod <- data_PB800_MA_P4p_SS_mod[["var.a.obs"]]
      vp_data_PB800_MA_P4p_SS_mod <- data_PB800_MA_P4p_SS_mod[["var.obs"]]
      
      Evol_data_PB800_MA_P4p_SS_mod <- (va_data_PB800_MA_P4p_SS_mod/2) / (trait_mean_data_PB800_MA_P4p_SS_mod)^2
      
      mean(h2_data_PB800_MA_P4p_SS_mod)
      mean(trait_mean_data_PB800_MA_P4p_SS_mod)
      mean(va_data_PB800_MA_P4p_SS_mod)
      mean(vp_data_PB800_MA_P4p_SS_mod)
      mean(Evol_data_PB800_MA_P4p_SS_mod)
      
    }
    
  }
  
  #PB800_Vm_P4p_SS_mod 
  {
    
    PB800_Vm_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_PB800_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(PB800_Vm_P4p_SS_mod, file = "PB800_Vm_P4p_SS_mod.rds")
    PB800_Vm_P4p_SS_mod <- readRDS("PB800_Vm_P4p_SS_mod.rds")
    
    summary(PB800_Vm_P4p_SS_mod) 
    plotTrace(PB800_Vm_P4p_SS_mod$Sol)
    
    # traces and posterior densities
    pdf("PB800_Vm_P4p_SS_mod.pdf")
    plot(PB800_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(PB800_Vm_P4p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PB800_Vm_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_Vm_P4p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PB800_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PB800_Vm_P4p_SS_mod[["VCV"]])
    #autocorr.plot(PB800_Vm_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PB800_Vm_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_Vm_P4p_SS_mod <- PB800_Vm_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB800_Vm_P4p_SS_mod <- PB800_Vm_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_PB800_Vm_P4p_SS_mod <- rowSums(PB800_Vm_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_PB800_Vm_P4p_SS_mod) 
      HPDinterval(va_liab_PB800_Vm_P4p_SS_mod) 
      
      mean(vlat_PB800_Vm_P4p_SS_mod) 
      
      #variance of fixed effects
      X_PB800_Vm_P4p_SS_mod <- PB800_Vm_P4p_SS_mod[["X"]]
      beta_PB800_Vm_P4p_SS_mod <- PB800_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]
      vf_PB800_Vm_P4p_SS_mod   <- apply(beta_PB800_Vm_P4p_SS_mod, 1, function(b) {var(as.vector(X_PB800_Vm_P4p_SS_mod %*% b))}) 
      mean(vf_PB800_Vm_P4p_SS_mod) 
      
      
      h2_liab_PB800_Vm_P4p_SS_mod <- va_liab_PB800_Vm_P4p_SS_mod / (vlat_PB800_Vm_P4p_SS_mod + vf_PB800_Vm_P4p_SS_mod)
      mean(h2_liab_PB800_Vm_P4p_SS_mod) 
      posterior.mode(h2_liab_PB800_Vm_P4p_SS_mod)	
      median(h2_liab_PB800_Vm_P4p_SS_mod)		
      HPDinterval(h2_liab_PB800_Vm_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PB800_Vm_P4p_SS_mod <- ((rowMeans(PB800_Vm_P4p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(PB800_Vm_P4p_SS_mod[["Sol"]][,c(1:4)]) + PB800_Vm_P4p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PB800_Vm_P4p_SS_mod <- (va_liab_PB800_Vm_P4p_SS_mod/2) / (trait_mean_liab_PB800_Vm_P4p_SS_mod)^2
    mean(Evol_liab_PB800_Vm_P4p_SS_mod)
    
    
    #PB800_Vm_P4p_SS_mod data scale
    {
      
      predict_PB800_Vm_P4p_SS_mod <- map(1:nrow(PB800_Vm_P4p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PB800_Vm_P4p_SS_mod %*% PB800_Vm_P4p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PB800_Vm_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_PB800_Vm_P4p_SS_mod,
                      var.a = PB800_Vm_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_Vm_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_Vm_P4p_SS_mod <- data_PB800_Vm_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_PB800_Vm_P4p_SS_mod <- data_PB800_Vm_P4p_SS_mod[["mean.obs"]]
      va_data_PB800_Vm_P4p_SS_mod <- data_PB800_Vm_P4p_SS_mod[["var.a.obs"]]
      vp_data_PB800_Vm_P4p_SS_mod <- data_PB800_Vm_P4p_SS_mod[["var.obs"]]
      
      Evol_data_PB800_Vm_P4p_SS_mod <- (va_data_PB800_Vm_P4p_SS_mod/2) / (trait_mean_data_PB800_Vm_P4p_SS_mod)^2
      
      mean(h2_data_PB800_Vm_P4p_SS_mod)
      mean(trait_mean_data_PB800_Vm_P4p_SS_mod)
      mean(va_data_PB800_Vm_P4p_SS_mod)
      mean(vp_data_PB800_Vm_P4p_SS_mod)
      mean(Evol_data_PB800_Vm_P4p_SS_mod)
      
    }
    
  }
  
  
  
  
  ##---- PB800 P8p ----
  
  
  #PB800_CONTROL_P8p_SS_mod 
  {
    
    PB800_CONTROL_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vm_PB800_bi_CONTROL,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)            
    
    saveRDS(PB800_CONTROL_P8p_SS_mod, file = "PB800_CONTROL_P8p_SS_mod.rds")
    PB800_CONTROL_P8p_SS_mod <- readRDS("PB800_CONTROL_P8p_SS_mod.rds")
    
    summary(PB800_CONTROL_P8p_SS_mod) 
    plotTrace(PB800_CONTROL_P8p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PB800_CONTROL_P8p_SS_mod.pdf")
    plot(PB800_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(PB800_CONTROL_P8p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB800_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB800_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB800_CONTROL_P8p_SS_mod[["VCV"]])
    #autocorr.plot(PB800_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB800_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_CONTROL_P8p_SS_mod <- PB800_CONTROL_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB800_CONTROL_P8p_SS_mod <- PB800_CONTROL_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_PB800_CONTROL_P8p_SS_mod <- rowSums(PB800_CONTROL_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_PB800_CONTROL_P8p_SS_mod) 
      HPDinterval(va_liab_PB800_CONTROL_P8p_SS_mod) 
      
      mean(vlat_PB800_CONTROL_P8p_SS_mod) 
      
      #variance of fixed effects
      X_PB800_CONTROL_P8p_SS_mod <- PB800_CONTROL_P8p_SS_mod[["X"]]
      beta_PB800_CONTROL_P8p_SS_mod <- PB800_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]
      vf_PB800_CONTROL_P8p_SS_mod   <- apply(beta_PB800_CONTROL_P8p_SS_mod, 1, function(b) {var(as.vector(X_PB800_CONTROL_P8p_SS_mod %*% b))}) 
      mean(vf_PB800_CONTROL_P8p_SS_mod) 
      
      
      h2_liab_PB800_CONTROL_P8p_SS_mod <- va_liab_PB800_CONTROL_P8p_SS_mod / (vlat_PB800_CONTROL_P8p_SS_mod + vf_PB800_CONTROL_P8p_SS_mod)
      mean(h2_liab_PB800_CONTROL_P8p_SS_mod) 
      posterior.mode(h2_liab_PB800_CONTROL_P8p_SS_mod)	
      median(h2_liab_PB800_CONTROL_P8p_SS_mod)		
      HPDinterval(h2_liab_PB800_CONTROL_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB800_CONTROL_P8p_SS_mod <- rowMeans(PB800_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB800_CONTROL_P8p_SS_mod <- (va_liab_PB800_CONTROL_P8p_SS_mod/2) / (trait_mean_liab_PB800_CONTROL_P8p_SS_mod)^2
    mean(Evol_liab_PB800_CONTROL_P8p_SS_mod)
    
    
    #PB800_CONTROL_P8p_SS_mod data scale
    {
      
      predict_PB800_CONTROL_P8p_SS_mod <- map(1:nrow(PB800_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB800_CONTROL_P8p_SS_mod %*% PB800_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PB800_CONTROL_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_PB800_CONTROL_P8p_SS_mod,
                      var.a = PB800_CONTROL_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_CONTROL_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_CONTROL_P8p_SS_mod <- data_PB800_CONTROL_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_PB800_CONTROL_P8p_SS_mod <- data_PB800_CONTROL_P8p_SS_mod[["mean.obs"]]
      va_data_PB800_CONTROL_P8p_SS_mod <- data_PB800_CONTROL_P8p_SS_mod[["var.a.obs"]]
      vp_data_PB800_CONTROL_P8p_SS_mod <- data_PB800_CONTROL_P8p_SS_mod[["var.obs"]]
      
      Evol_data_PB800_CONTROL_P8p_SS_mod <- (va_data_PB800_CONTROL_P8p_SS_mod/2) / (trait_mean_data_PB800_CONTROL_P8p_SS_mod)^2
      
      mean(h2_data_PB800_CONTROL_P8p_SS_mod) 
      mean(trait_mean_data_PB800_CONTROL_P8p_SS_mod)
      mean(va_data_PB800_CONTROL_P8p_SS_mod) 
      mean(vp_data_PB800_CONTROL_P8p_SS_mod)
      mean(Evol_data_PB800_CONTROL_P8p_SS_mod)
      
    }
    
  }
  
  #PB800_MA_P8p_SS_mod 
  {
    
    PB800_MA_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_PB800_bi_MA,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)         
    
    saveRDS(PB800_MA_P8p_SS_mod, file = "PB800_MA_P8p_SS_mod.rds")
    PB800_MA_P8p_SS_mod <- readRDS("PB800_MA_P8p_SS_mod.rds")
    
    summary(PB800_MA_P8p_SS_mod) 
    #plot(PB800_MA_P8p_SS_mod)
    
    # traces and posterior densities
    pdf("PB800_MA_P8p_SS_mod.pdf")
    plot(PB800_MA_P8p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(PB800_MA_P8p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_MA_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB800_MA_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_MA_P8p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB800_MA_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB800_MA_P8p_SS_mod[["VCV"]])
    #autocorr.plot(PB800_MA_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_MA_P8p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB800_MA_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_MA_P8p_SS_mod <- PB800_MA_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB800_MA_P8p_SS_mod <- PB800_MA_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_PB800_MA_P8p_SS_mod <- rowSums(PB800_MA_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_PB800_MA_P8p_SS_mod) 
      HPDinterval(va_liab_PB800_MA_P8p_SS_mod) 
      
      mean(vlat_PB800_MA_P8p_SS_mod) 
      
      #variance of fixed effects
      X_PB800_MA_P8p_SS_mod <- PB800_MA_P8p_SS_mod[["X"]]
      beta_PB800_MA_P8p_SS_mod <- PB800_MA_P8p_SS_mod[["Sol"]][,c(1:4)]
      vf_PB800_MA_P8p_SS_mod   <- apply(beta_PB800_MA_P8p_SS_mod, 1, function(b) {var(as.vector(X_PB800_MA_P8p_SS_mod %*% b))}) 
      mean(vf_PB800_MA_P8p_SS_mod) 
      
      h2_liab_PB800_MA_P8p_SS_mod <- va_liab_PB800_MA_P8p_SS_mod / (vlat_PB800_MA_P8p_SS_mod + vf_PB800_MA_P8p_SS_mod)
      mean(h2_liab_PB800_MA_P8p_SS_mod) 
      posterior.mode(h2_liab_PB800_MA_P8p_SS_mod)	
      median(h2_liab_PB800_MA_P8p_SS_mod)		
      HPDinterval(h2_liab_PB800_MA_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB800_MA_P8p_SS_mod <- rowMeans(PB800_MA_P8p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB800_MA_P8p_SS_mod <- (va_liab_PB800_MA_P8p_SS_mod/2) / (trait_mean_liab_PB800_MA_P8p_SS_mod)^2
    mean(Evol_liab_PB800_MA_P8p_SS_mod)
    
    #PB800_MA_P8p_SS_mod data scale
    {
      
      predict_PB800_MA_P8p_SS_mod <- map(1:nrow(PB800_MA_P8p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB800_MA_P8p_SS_mod %*% PB800_MA_P8p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PB800_MA_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_PB800_MA_P8p_SS_mod,
                      var.a = PB800_MA_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_MA_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_MA_P8p_SS_mod <- data_PB800_MA_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_PB800_MA_P8p_SS_mod <- data_PB800_MA_P8p_SS_mod[["mean.obs"]]
      va_data_PB800_MA_P8p_SS_mod <- data_PB800_MA_P8p_SS_mod[["var.a.obs"]]
      vp_data_PB800_MA_P8p_SS_mod <- data_PB800_MA_P8p_SS_mod[["var.obs"]]
      
      Evol_data_PB800_MA_P8p_SS_mod <- (va_data_PB800_MA_P8p_SS_mod/2) / (trait_mean_data_PB800_MA_P8p_SS_mod)^2
      
      mean(h2_data_PB800_MA_P8p_SS_mod)
      mean(trait_mean_data_PB800_MA_P8p_SS_mod)
      mean(va_data_PB800_MA_P8p_SS_mod)
      mean(vp_data_PB800_MA_P8p_SS_mod)
      mean(Evol_data_PB800_MA_P8p_SS_mod)
      
    }
    
  }
  
  #PB800_Vm_P8p_SS_mod 
  {
    
    PB800_Vm_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer + Treatment -1,
                                   random      = ~ LineB + BlockRep,
                                   family      = "threshold",
                                   data        = Vm_PB800_data,
                                   prior       = prior_bi_block,
                                   nitt        = 1260000,       
                                   thin        = 500,           
                                   burnin      = 10000,
                                   trunc       = TRUE,
                                   pr          = TRUE,
                                   pl          = TRUE)            
    
    saveRDS(PB800_Vm_P8p_SS_mod, file = "PB800_Vm_P8p_SS_mod.rds")
    PB800_Vm_P8p_SS_mod <- readRDS("PB800_Vm_P8p_SS_mod.rds")
    
    summary(PB800_Vm_P8p_SS_mod) 
    plotTrace(PB800_Vm_P8p_SS_mod$Sol)
    
    # traces and posterior densities
    pdf("PB800_Vm_P8p_SS_mod.pdf")
    plot(PB800_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(PB800_Vm_P8p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PB800_Vm_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_Vm_P8p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PB800_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PB800_Vm_P8p_SS_mod[["VCV"]])
    #autocorr.plot(PB800_Vm_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PB800_Vm_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_Vm_P8p_SS_mod <- PB800_Vm_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_PB800_Vm_P8p_SS_mod <- PB800_Vm_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_PB800_Vm_P8p_SS_mod <- rowSums(PB800_Vm_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_PB800_Vm_P8p_SS_mod) 
      HPDinterval(va_liab_PB800_Vm_P8p_SS_mod) 
      
      mean(vlat_PB800_Vm_P8p_SS_mod) 
      
      #variance of fixed effects
      X_PB800_Vm_P8p_SS_mod <- PB800_Vm_P8p_SS_mod[["X"]]
      beta_PB800_Vm_P8p_SS_mod <- PB800_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]
      vf_PB800_Vm_P8p_SS_mod   <- apply(beta_PB800_Vm_P8p_SS_mod, 1, function(b) {var(as.vector(X_PB800_Vm_P8p_SS_mod %*% b))}) 
      mean(vf_PB800_Vm_P8p_SS_mod) 
      
      
      h2_liab_PB800_Vm_P8p_SS_mod <- va_liab_PB800_Vm_P8p_SS_mod / (vlat_PB800_Vm_P8p_SS_mod + vf_PB800_Vm_P8p_SS_mod)
      mean(h2_liab_PB800_Vm_P8p_SS_mod) 
      posterior.mode(h2_liab_PB800_Vm_P8p_SS_mod)	
      median(h2_liab_PB800_Vm_P8p_SS_mod)		
      HPDinterval(h2_liab_PB800_Vm_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PB800_Vm_P8p_SS_mod <- ((rowMeans(PB800_Vm_P8p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(PB800_Vm_P8p_SS_mod[["Sol"]][,c(1:4)]) + PB800_Vm_P8p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PB800_Vm_P8p_SS_mod <- (va_liab_PB800_Vm_P8p_SS_mod/2) / (trait_mean_liab_PB800_Vm_P8p_SS_mod)^2
    mean(Evol_liab_PB800_Vm_P8p_SS_mod)
    
    
    #PB800_Vm_P8p_SS_mod data scale
    {
      
      predict_PB800_Vm_P8p_SS_mod <- map(1:nrow(PB800_Vm_P8p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PB800_Vm_P8p_SS_mod %*% PB800_Vm_P8p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PB800_Vm_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_PB800_Vm_P8p_SS_mod,
                      var.a = PB800_Vm_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_Vm_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_Vm_P8p_SS_mod <- data_PB800_Vm_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_PB800_Vm_P8p_SS_mod <- data_PB800_Vm_P8p_SS_mod[["mean.obs"]]
      va_data_PB800_Vm_P8p_SS_mod <- data_PB800_Vm_P8p_SS_mod[["var.a.obs"]]
      vp_data_PB800_Vm_P8p_SS_mod <- data_PB800_Vm_P8p_SS_mod[["var.obs"]]
      
      Evol_data_PB800_Vm_P8p_SS_mod <- (va_data_PB800_Vm_P8p_SS_mod/2) / (trait_mean_data_PB800_Vm_P8p_SS_mod)^2
      
      mean(h2_data_PB800_Vm_P8p_SS_mod)
      mean(trait_mean_data_PB800_Vm_P8p_SS_mod)
      mean(va_data_PB800_Vm_P8p_SS_mod)
      mean(vp_data_PB800_Vm_P8p_SS_mod)
      mean(Evol_data_PB800_Vm_P8p_SS_mod)
      
    }
    
  }
  
  ##---- PB800 P5p ----
  
  
  #PB800_CONTROL_P5p_wt_mod 
  {
    
    PB800_CONTROL_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vm_PB800_bi_CONTROL,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)            
    
    saveRDS(PB800_CONTROL_P5p_wt_mod, file = "PB800_CONTROL_P5p_wt_mod.rds")
    PB800_CONTROL_P5p_wt_mod <- readRDS("PB800_CONTROL_P5p_wt_mod.rds")
    
    summary(PB800_CONTROL_P5p_wt_mod) 
    plotTrace(PB800_CONTROL_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PB800_CONTROL_P5p_wt_mod.pdf")
    plot(PB800_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PB800_CONTROL_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB800_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB800_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB800_CONTROL_P5p_wt_mod[["VCV"]])
    #autocorr.plot(PB800_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB800_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_CONTROL_P5p_wt_mod <- PB800_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB800_CONTROL_P5p_wt_mod <- PB800_CONTROL_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_PB800_CONTROL_P5p_wt_mod <- rowSums(PB800_CONTROL_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_PB800_CONTROL_P5p_wt_mod) 
      HPDinterval(va_liab_PB800_CONTROL_P5p_wt_mod) 
      
      mean(vlat_PB800_CONTROL_P5p_wt_mod) 
      
      #variance of fixed effects
      X_PB800_CONTROL_P5p_wt_mod <- PB800_CONTROL_P5p_wt_mod[["X"]]
      beta_PB800_CONTROL_P5p_wt_mod <- PB800_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_PB800_CONTROL_P5p_wt_mod   <- apply(beta_PB800_CONTROL_P5p_wt_mod, 1, function(b) {var(as.vector(X_PB800_CONTROL_P5p_wt_mod %*% b))}) 
      mean(vf_PB800_CONTROL_P5p_wt_mod) 
      
      
      h2_liab_PB800_CONTROL_P5p_wt_mod <- va_liab_PB800_CONTROL_P5p_wt_mod / (vlat_PB800_CONTROL_P5p_wt_mod + vf_PB800_CONTROL_P5p_wt_mod)
      mean(h2_liab_PB800_CONTROL_P5p_wt_mod) 
      posterior.mode(h2_liab_PB800_CONTROL_P5p_wt_mod)	
      median(h2_liab_PB800_CONTROL_P5p_wt_mod)		
      HPDinterval(h2_liab_PB800_CONTROL_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB800_CONTROL_P5p_wt_mod <- rowMeans(PB800_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB800_CONTROL_P5p_wt_mod <- (va_liab_PB800_CONTROL_P5p_wt_mod/2) / (trait_mean_liab_PB800_CONTROL_P5p_wt_mod)^2
    mean(Evol_liab_PB800_CONTROL_P5p_wt_mod)
    
    
    #PB800_CONTROL_P5p_wt_mod data scale
    {
      
      predict_PB800_CONTROL_P5p_wt_mod <- map(1:nrow(PB800_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB800_CONTROL_P5p_wt_mod %*% PB800_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PB800_CONTROL_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_PB800_CONTROL_P5p_wt_mod,
                      var.a = PB800_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_CONTROL_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_CONTROL_P5p_wt_mod <- data_PB800_CONTROL_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_PB800_CONTROL_P5p_wt_mod <- data_PB800_CONTROL_P5p_wt_mod[["mean.obs"]]
      va_data_PB800_CONTROL_P5p_wt_mod <- data_PB800_CONTROL_P5p_wt_mod[["var.a.obs"]]
      vp_data_PB800_CONTROL_P5p_wt_mod <- data_PB800_CONTROL_P5p_wt_mod[["var.obs"]]
      
      Evol_data_PB800_CONTROL_P5p_wt_mod <- (va_data_PB800_CONTROL_P5p_wt_mod/2) / (trait_mean_data_PB800_CONTROL_P5p_wt_mod)^2
      
      mean(h2_data_PB800_CONTROL_P5p_wt_mod) 
      mean(trait_mean_data_PB800_CONTROL_P5p_wt_mod)
      mean(va_data_PB800_CONTROL_P5p_wt_mod) 
      mean(vp_data_PB800_CONTROL_P5p_wt_mod)
      mean(Evol_data_PB800_CONTROL_P5p_wt_mod)
      
    }
    
  }
  
  #PB800_MA_P5p_wt_mod 
  {
    
    PB800_MA_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PB800_bi_MA,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)         
    
    saveRDS(PB800_MA_P5p_wt_mod, file = "PB800_MA_P5p_wt_mod.rds")
    PB800_MA_P5p_wt_mod <- readRDS("PB800_MA_P5p_wt_mod.rds")
    
    summary(PB800_MA_P5p_wt_mod) 
    #plot(PB800_MA_P5p_wt_mod)
    
    # traces and posterior densities
    pdf("PB800_MA_P5p_wt_mod.pdf")
    plot(PB800_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PB800_MA_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB800_MA_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_MA_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB800_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB800_MA_P5p_wt_mod[["VCV"]])
    #autocorr.plot(PB800_MA_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB800_MA_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_MA_P5p_wt_mod <- PB800_MA_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB800_MA_P5p_wt_mod <- PB800_MA_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_PB800_MA_P5p_wt_mod <- rowSums(PB800_MA_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_PB800_MA_P5p_wt_mod) 
      HPDinterval(va_liab_PB800_MA_P5p_wt_mod) 
      
      mean(vlat_PB800_MA_P5p_wt_mod) 
      
      #variance of fixed effects
      X_PB800_MA_P5p_wt_mod <- PB800_MA_P5p_wt_mod[["X"]]
      beta_PB800_MA_P5p_wt_mod <- PB800_MA_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_PB800_MA_P5p_wt_mod   <- apply(beta_PB800_MA_P5p_wt_mod, 1, function(b) {var(as.vector(X_PB800_MA_P5p_wt_mod %*% b))}) 
      mean(vf_PB800_MA_P5p_wt_mod) 
      
      h2_liab_PB800_MA_P5p_wt_mod <- va_liab_PB800_MA_P5p_wt_mod / (vlat_PB800_MA_P5p_wt_mod + vf_PB800_MA_P5p_wt_mod)
      mean(h2_liab_PB800_MA_P5p_wt_mod) 
      posterior.mode(h2_liab_PB800_MA_P5p_wt_mod)	
      median(h2_liab_PB800_MA_P5p_wt_mod)		
      HPDinterval(h2_liab_PB800_MA_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB800_MA_P5p_wt_mod <- rowMeans(PB800_MA_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB800_MA_P5p_wt_mod <- (va_liab_PB800_MA_P5p_wt_mod/2) / (trait_mean_liab_PB800_MA_P5p_wt_mod)^2
    mean(Evol_liab_PB800_MA_P5p_wt_mod)
    
    #PB800_MA_P5p_wt_mod data scale
    {
      
      predict_PB800_MA_P5p_wt_mod <- map(1:nrow(PB800_MA_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB800_MA_P5p_wt_mod %*% PB800_MA_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PB800_MA_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_PB800_MA_P5p_wt_mod,
                      var.a = PB800_MA_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_MA_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_MA_P5p_wt_mod <- data_PB800_MA_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_PB800_MA_P5p_wt_mod <- data_PB800_MA_P5p_wt_mod[["mean.obs"]]
      va_data_PB800_MA_P5p_wt_mod <- data_PB800_MA_P5p_wt_mod[["var.a.obs"]]
      vp_data_PB800_MA_P5p_wt_mod <- data_PB800_MA_P5p_wt_mod[["var.obs"]]
      
      Evol_data_PB800_MA_P5p_wt_mod <- (va_data_PB800_MA_P5p_wt_mod/2) / (trait_mean_data_PB800_MA_P5p_wt_mod)^2
      
      mean(h2_data_PB800_MA_P5p_wt_mod)
      mean(trait_mean_data_PB800_MA_P5p_wt_mod)
      mean(va_data_PB800_MA_P5p_wt_mod)
      mean(vp_data_PB800_MA_P5p_wt_mod)
      mean(Evol_data_PB800_MA_P5p_wt_mod)
      
    }
    
  }
  
  #PB800_Vm_P5p_wt_mod 
  {
    
    PB800_Vm_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Treatment -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PB800_data,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)            
    
    saveRDS(PB800_Vm_P5p_wt_mod, file = "PB800_Vm_P5p_wt_mod.rds")
    PB800_Vm_P5p_wt_mod <- readRDS("PB800_Vm_P5p_wt_mod.rds")
    
    summary(PB800_Vm_P5p_wt_mod) 
    plotTrace(PB800_Vm_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("PB800_Vm_P5p_wt_mod.pdf")
    plot(PB800_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(PB800_Vm_P5p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PB800_Vm_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_Vm_P5p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PB800_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PB800_Vm_P5p_wt_mod[["VCV"]])
    #autocorr.plot(PB800_Vm_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PB800_Vm_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_Vm_P5p_wt_mod <- PB800_Vm_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB800_Vm_P5p_wt_mod <- PB800_Vm_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_PB800_Vm_P5p_wt_mod <- rowSums(PB800_Vm_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_PB800_Vm_P5p_wt_mod) 
      HPDinterval(va_liab_PB800_Vm_P5p_wt_mod) 
      
      mean(vlat_PB800_Vm_P5p_wt_mod) 
      
      #variance of fixed effects
      X_PB800_Vm_P5p_wt_mod <- PB800_Vm_P5p_wt_mod[["X"]]
      beta_PB800_Vm_P5p_wt_mod <- PB800_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]
      vf_PB800_Vm_P5p_wt_mod   <- apply(beta_PB800_Vm_P5p_wt_mod, 1, function(b) {var(as.vector(X_PB800_Vm_P5p_wt_mod %*% b))}) 
      mean(vf_PB800_Vm_P5p_wt_mod) 
      
      
      h2_liab_PB800_Vm_P5p_wt_mod <- va_liab_PB800_Vm_P5p_wt_mod / (vlat_PB800_Vm_P5p_wt_mod + vf_PB800_Vm_P5p_wt_mod)
      mean(h2_liab_PB800_Vm_P5p_wt_mod) 
      posterior.mode(h2_liab_PB800_Vm_P5p_wt_mod)	
      median(h2_liab_PB800_Vm_P5p_wt_mod)		
      HPDinterval(h2_liab_PB800_Vm_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PB800_Vm_P5p_wt_mod <- ((rowMeans(PB800_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(PB800_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + PB800_Vm_P5p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PB800_Vm_P5p_wt_mod <- (va_liab_PB800_Vm_P5p_wt_mod/2) / (trait_mean_liab_PB800_Vm_P5p_wt_mod)^2
    mean(Evol_liab_PB800_Vm_P5p_wt_mod)
    
    
    #PB800_Vm_P5p_wt_mod data scale
    {
      
      predict_PB800_Vm_P5p_wt_mod <- map(1:nrow(PB800_Vm_P5p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PB800_Vm_P5p_wt_mod %*% PB800_Vm_P5p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PB800_Vm_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_PB800_Vm_P5p_wt_mod,
                      var.a = PB800_Vm_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_Vm_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_Vm_P5p_wt_mod <- data_PB800_Vm_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_PB800_Vm_P5p_wt_mod <- data_PB800_Vm_P5p_wt_mod[["mean.obs"]]
      va_data_PB800_Vm_P5p_wt_mod <- data_PB800_Vm_P5p_wt_mod[["var.a.obs"]]
      vp_data_PB800_Vm_P5p_wt_mod <- data_PB800_Vm_P5p_wt_mod[["var.obs"]]
      
      Evol_data_PB800_Vm_P5p_wt_mod <- (va_data_PB800_Vm_P5p_wt_mod/2) / (trait_mean_data_PB800_Vm_P5p_wt_mod)^2
      
      mean(h2_data_PB800_Vm_P5p_wt_mod)
      mean(trait_mean_data_PB800_Vm_P5p_wt_mod)
      mean(va_data_PB800_Vm_P5p_wt_mod)
      mean(vp_data_PB800_Vm_P5p_wt_mod)
      mean(Evol_data_PB800_Vm_P5p_wt_mod)
      
    }
    
  }
  
  
  
  
  ##---- PB800 P6p ----
  
  
  #PB800_CONTROL_P6p_wt_mod 
  {
    
    PB800_CONTROL_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vm_PB800_bi_CONTROL,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)            
    
    saveRDS(PB800_CONTROL_P6p_wt_mod, file = "PB800_CONTROL_P6p_wt_mod.rds")
    PB800_CONTROL_P6p_wt_mod <- readRDS("PB800_CONTROL_P6p_wt_mod.rds")
    
    summary(PB800_CONTROL_P6p_wt_mod) 
    plotTrace(PB800_CONTROL_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PB800_CONTROL_P6p_wt_mod.pdf")
    plot(PB800_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PB800_CONTROL_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB800_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB800_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB800_CONTROL_P6p_wt_mod[["VCV"]])
    #autocorr.plot(PB800_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB800_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_CONTROL_P6p_wt_mod <- PB800_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB800_CONTROL_P6p_wt_mod <- PB800_CONTROL_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_PB800_CONTROL_P6p_wt_mod <- rowSums(PB800_CONTROL_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_PB800_CONTROL_P6p_wt_mod) 
      HPDinterval(va_liab_PB800_CONTROL_P6p_wt_mod) 
      
      mean(vlat_PB800_CONTROL_P6p_wt_mod) 
      
      #variance of fixed effects
      X_PB800_CONTROL_P6p_wt_mod <- PB800_CONTROL_P6p_wt_mod[["X"]]
      beta_PB800_CONTROL_P6p_wt_mod <- PB800_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_PB800_CONTROL_P6p_wt_mod   <- apply(beta_PB800_CONTROL_P6p_wt_mod, 1, function(b) {var(as.vector(X_PB800_CONTROL_P6p_wt_mod %*% b))}) 
      mean(vf_PB800_CONTROL_P6p_wt_mod) 
      
      
      h2_liab_PB800_CONTROL_P6p_wt_mod <- va_liab_PB800_CONTROL_P6p_wt_mod / (vlat_PB800_CONTROL_P6p_wt_mod + vf_PB800_CONTROL_P6p_wt_mod)
      mean(h2_liab_PB800_CONTROL_P6p_wt_mod) 
      posterior.mode(h2_liab_PB800_CONTROL_P6p_wt_mod)	
      median(h2_liab_PB800_CONTROL_P6p_wt_mod)		
      HPDinterval(h2_liab_PB800_CONTROL_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB800_CONTROL_P6p_wt_mod <- rowMeans(PB800_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB800_CONTROL_P6p_wt_mod <- (va_liab_PB800_CONTROL_P6p_wt_mod/2) / (trait_mean_liab_PB800_CONTROL_P6p_wt_mod)^2
    mean(Evol_liab_PB800_CONTROL_P6p_wt_mod)
    
    
    #PB800_CONTROL_P6p_wt_mod data scale
    {
      
      predict_PB800_CONTROL_P6p_wt_mod <- map(1:nrow(PB800_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB800_CONTROL_P6p_wt_mod %*% PB800_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PB800_CONTROL_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_PB800_CONTROL_P6p_wt_mod,
                      var.a = PB800_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_CONTROL_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_CONTROL_P6p_wt_mod <- data_PB800_CONTROL_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_PB800_CONTROL_P6p_wt_mod <- data_PB800_CONTROL_P6p_wt_mod[["mean.obs"]]
      va_data_PB800_CONTROL_P6p_wt_mod <- data_PB800_CONTROL_P6p_wt_mod[["var.a.obs"]]
      vp_data_PB800_CONTROL_P6p_wt_mod <- data_PB800_CONTROL_P6p_wt_mod[["var.obs"]]
      
      Evol_data_PB800_CONTROL_P6p_wt_mod <- (va_data_PB800_CONTROL_P6p_wt_mod/2) / (trait_mean_data_PB800_CONTROL_P6p_wt_mod)^2
      
      mean(h2_data_PB800_CONTROL_P6p_wt_mod) 
      mean(trait_mean_data_PB800_CONTROL_P6p_wt_mod)
      mean(va_data_PB800_CONTROL_P6p_wt_mod) 
      mean(vp_data_PB800_CONTROL_P6p_wt_mod)
      mean(Evol_data_PB800_CONTROL_P6p_wt_mod)
      
    }
    
  }
  
  #PB800_MA_P6p_wt_mod 
  {
    
    PB800_MA_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PB800_bi_MA,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)         
    
    saveRDS(PB800_MA_P6p_wt_mod, file = "PB800_MA_P6p_wt_mod.rds")
    PB800_MA_P6p_wt_mod <- readRDS("PB800_MA_P6p_wt_mod.rds")
    
    summary(PB800_MA_P6p_wt_mod) 
    #plot(PB800_MA_P6p_wt_mod)
    
    # traces and posterior densities
    pdf("PB800_MA_P6p_wt_mod.pdf")
    plot(PB800_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PB800_MA_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB800_MA_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_MA_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB800_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB800_MA_P6p_wt_mod[["VCV"]])
    #autocorr.plot(PB800_MA_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB800_MA_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_MA_P6p_wt_mod <- PB800_MA_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB800_MA_P6p_wt_mod <- PB800_MA_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_PB800_MA_P6p_wt_mod <- rowSums(PB800_MA_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_PB800_MA_P6p_wt_mod) 
      HPDinterval(va_liab_PB800_MA_P6p_wt_mod) 
      
      mean(vlat_PB800_MA_P6p_wt_mod) 
      
      #variance of fixed effects
      X_PB800_MA_P6p_wt_mod <- PB800_MA_P6p_wt_mod[["X"]]
      beta_PB800_MA_P6p_wt_mod <- PB800_MA_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_PB800_MA_P6p_wt_mod   <- apply(beta_PB800_MA_P6p_wt_mod, 1, function(b) {var(as.vector(X_PB800_MA_P6p_wt_mod %*% b))}) 
      mean(vf_PB800_MA_P6p_wt_mod) 
      
      h2_liab_PB800_MA_P6p_wt_mod <- va_liab_PB800_MA_P6p_wt_mod / (vlat_PB800_MA_P6p_wt_mod + vf_PB800_MA_P6p_wt_mod)
      mean(h2_liab_PB800_MA_P6p_wt_mod) 
      posterior.mode(h2_liab_PB800_MA_P6p_wt_mod)	
      median(h2_liab_PB800_MA_P6p_wt_mod)		
      HPDinterval(h2_liab_PB800_MA_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB800_MA_P6p_wt_mod <- rowMeans(PB800_MA_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB800_MA_P6p_wt_mod <- (va_liab_PB800_MA_P6p_wt_mod/2) / (trait_mean_liab_PB800_MA_P6p_wt_mod)^2
    mean(Evol_liab_PB800_MA_P6p_wt_mod)
    
    #PB800_MA_P6p_wt_mod data scale
    {
      
      predict_PB800_MA_P6p_wt_mod <- map(1:nrow(PB800_MA_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB800_MA_P6p_wt_mod %*% PB800_MA_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PB800_MA_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_PB800_MA_P6p_wt_mod,
                      var.a = PB800_MA_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_MA_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_MA_P6p_wt_mod <- data_PB800_MA_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_PB800_MA_P6p_wt_mod <- data_PB800_MA_P6p_wt_mod[["mean.obs"]]
      va_data_PB800_MA_P6p_wt_mod <- data_PB800_MA_P6p_wt_mod[["var.a.obs"]]
      vp_data_PB800_MA_P6p_wt_mod <- data_PB800_MA_P6p_wt_mod[["var.obs"]]
      
      Evol_data_PB800_MA_P6p_wt_mod <- (va_data_PB800_MA_P6p_wt_mod/2) / (trait_mean_data_PB800_MA_P6p_wt_mod)^2
      
      mean(h2_data_PB800_MA_P6p_wt_mod)
      mean(trait_mean_data_PB800_MA_P6p_wt_mod)
      mean(va_data_PB800_MA_P6p_wt_mod)
      mean(vp_data_PB800_MA_P6p_wt_mod)
      mean(Evol_data_PB800_MA_P6p_wt_mod)
      
    }
    
  }
  
  #PB800_Vm_P6p_wt_mod 
  {
    
    PB800_Vm_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Treatment -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PB800_data,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)            
    
    saveRDS(PB800_Vm_P6p_wt_mod, file = "PB800_Vm_P6p_wt_mod.rds")
    PB800_Vm_P6p_wt_mod <- readRDS("PB800_Vm_P6p_wt_mod.rds")
    
    summary(PB800_Vm_P6p_wt_mod) 
    plotTrace(PB800_Vm_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("PB800_Vm_P6p_wt_mod.pdf")
    plot(PB800_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(PB800_Vm_P6p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PB800_Vm_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_Vm_P6p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PB800_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PB800_Vm_P6p_wt_mod[["VCV"]])
    #autocorr.plot(PB800_Vm_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PB800_Vm_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_Vm_P6p_wt_mod <- PB800_Vm_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB800_Vm_P6p_wt_mod <- PB800_Vm_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_PB800_Vm_P6p_wt_mod <- rowSums(PB800_Vm_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_PB800_Vm_P6p_wt_mod) 
      HPDinterval(va_liab_PB800_Vm_P6p_wt_mod) 
      
      mean(vlat_PB800_Vm_P6p_wt_mod) 
      
      #variance of fixed effects
      X_PB800_Vm_P6p_wt_mod <- PB800_Vm_P6p_wt_mod[["X"]]
      beta_PB800_Vm_P6p_wt_mod <- PB800_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]
      vf_PB800_Vm_P6p_wt_mod   <- apply(beta_PB800_Vm_P6p_wt_mod, 1, function(b) {var(as.vector(X_PB800_Vm_P6p_wt_mod %*% b))}) 
      mean(vf_PB800_Vm_P6p_wt_mod) 
      
      
      h2_liab_PB800_Vm_P6p_wt_mod <- va_liab_PB800_Vm_P6p_wt_mod / (vlat_PB800_Vm_P6p_wt_mod + vf_PB800_Vm_P6p_wt_mod)
      mean(h2_liab_PB800_Vm_P6p_wt_mod) 
      posterior.mode(h2_liab_PB800_Vm_P6p_wt_mod)	
      median(h2_liab_PB800_Vm_P6p_wt_mod)		
      HPDinterval(h2_liab_PB800_Vm_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PB800_Vm_P6p_wt_mod <- ((rowMeans(PB800_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(PB800_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + PB800_Vm_P6p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PB800_Vm_P6p_wt_mod <- (va_liab_PB800_Vm_P6p_wt_mod/2) / (trait_mean_liab_PB800_Vm_P6p_wt_mod)^2
    mean(Evol_liab_PB800_Vm_P6p_wt_mod)
    
    
    #PB800_Vm_P6p_wt_mod data scale
    {
      
      predict_PB800_Vm_P6p_wt_mod <- map(1:nrow(PB800_Vm_P6p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PB800_Vm_P6p_wt_mod %*% PB800_Vm_P6p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PB800_Vm_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_PB800_Vm_P6p_wt_mod,
                      var.a = PB800_Vm_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_Vm_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_Vm_P6p_wt_mod <- data_PB800_Vm_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_PB800_Vm_P6p_wt_mod <- data_PB800_Vm_P6p_wt_mod[["mean.obs"]]
      va_data_PB800_Vm_P6p_wt_mod <- data_PB800_Vm_P6p_wt_mod[["var.a.obs"]]
      vp_data_PB800_Vm_P6p_wt_mod <- data_PB800_Vm_P6p_wt_mod[["var.obs"]]
      
      Evol_data_PB800_Vm_P6p_wt_mod <- (va_data_PB800_Vm_P6p_wt_mod/2) / (trait_mean_data_PB800_Vm_P6p_wt_mod)^2
      
      mean(h2_data_PB800_Vm_P6p_wt_mod)
      mean(trait_mean_data_PB800_Vm_P6p_wt_mod)
      mean(va_data_PB800_Vm_P6p_wt_mod)
      mean(vp_data_PB800_Vm_P6p_wt_mod)
      mean(Evol_data_PB800_Vm_P6p_wt_mod)
      
    }
    
  }
  
  
  
  
  
  ##---- PB800 P7p ----
  
  
  #PB800_CONTROL_P7p_wt_mod 
  {
    
    PB800_CONTROL_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vm_PB800_bi_CONTROL,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)            
    
    saveRDS(PB800_CONTROL_P7p_wt_mod, file = "PB800_CONTROL_P7p_wt_mod.rds")
    PB800_CONTROL_P7p_wt_mod <- readRDS("PB800_CONTROL_P7p_wt_mod.rds")
    
    summary(PB800_CONTROL_P7p_wt_mod) 
    plotTrace(PB800_CONTROL_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("PB800_CONTROL_P7p_wt_mod.pdf")
    plot(PB800_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PB800_CONTROL_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB800_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB800_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB800_CONTROL_P7p_wt_mod[["VCV"]])
    #autocorr.plot(PB800_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB800_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_CONTROL_P7p_wt_mod <- PB800_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB800_CONTROL_P7p_wt_mod <- PB800_CONTROL_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_PB800_CONTROL_P7p_wt_mod <- rowSums(PB800_CONTROL_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_PB800_CONTROL_P7p_wt_mod) 
      HPDinterval(va_liab_PB800_CONTROL_P7p_wt_mod) 
      
      mean(vlat_PB800_CONTROL_P7p_wt_mod) 
      
      #variance of fixed effects
      X_PB800_CONTROL_P7p_wt_mod <- PB800_CONTROL_P7p_wt_mod[["X"]]
      beta_PB800_CONTROL_P7p_wt_mod <- PB800_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_PB800_CONTROL_P7p_wt_mod   <- apply(beta_PB800_CONTROL_P7p_wt_mod, 1, function(b) {var(as.vector(X_PB800_CONTROL_P7p_wt_mod %*% b))}) 
      mean(vf_PB800_CONTROL_P7p_wt_mod) 
      
      
      h2_liab_PB800_CONTROL_P7p_wt_mod <- va_liab_PB800_CONTROL_P7p_wt_mod / (vlat_PB800_CONTROL_P7p_wt_mod + vf_PB800_CONTROL_P7p_wt_mod)
      mean(h2_liab_PB800_CONTROL_P7p_wt_mod) 
      posterior.mode(h2_liab_PB800_CONTROL_P7p_wt_mod)	
      median(h2_liab_PB800_CONTROL_P7p_wt_mod)		
      HPDinterval(h2_liab_PB800_CONTROL_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB800_CONTROL_P7p_wt_mod <- rowMeans(PB800_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB800_CONTROL_P7p_wt_mod <- (va_liab_PB800_CONTROL_P7p_wt_mod/2) / (trait_mean_liab_PB800_CONTROL_P7p_wt_mod)^2
    mean(Evol_liab_PB800_CONTROL_P7p_wt_mod)
    
    
    #PB800_CONTROL_P7p_wt_mod data scale
    {
      
      predict_PB800_CONTROL_P7p_wt_mod <- map(1:nrow(PB800_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB800_CONTROL_P7p_wt_mod %*% PB800_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      
      data_PB800_CONTROL_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_PB800_CONTROL_P7p_wt_mod,
                      var.a = PB800_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_CONTROL_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_CONTROL_P7p_wt_mod <- data_PB800_CONTROL_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_PB800_CONTROL_P7p_wt_mod <- data_PB800_CONTROL_P7p_wt_mod[["mean.obs"]]
      va_data_PB800_CONTROL_P7p_wt_mod <- data_PB800_CONTROL_P7p_wt_mod[["var.a.obs"]]
      vp_data_PB800_CONTROL_P7p_wt_mod <- data_PB800_CONTROL_P7p_wt_mod[["var.obs"]]
      
      Evol_data_PB800_CONTROL_P7p_wt_mod <- (va_data_PB800_CONTROL_P7p_wt_mod/2) / (trait_mean_data_PB800_CONTROL_P7p_wt_mod)^2
      
      mean(h2_data_PB800_CONTROL_P7p_wt_mod) 
      mean(trait_mean_data_PB800_CONTROL_P7p_wt_mod)
      mean(va_data_PB800_CONTROL_P7p_wt_mod) 
      mean(vp_data_PB800_CONTROL_P7p_wt_mod)
      mean(Evol_data_PB800_CONTROL_P7p_wt_mod)
      
    }
    
  }
  
  #PB800_MA_P7p_wt_mod 
  {
    
    PB800_MA_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PB800_bi_MA,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)         
    
    saveRDS(PB800_MA_P7p_wt_mod, file = "PB800_MA_P7p_wt_mod.rds")
    PB800_MA_P7p_wt_mod <- readRDS("PB800_MA_P7p_wt_mod.rds")
    
    summary(PB800_MA_P7p_wt_mod) 
    #plot(PB800_MA_P7p_wt_mod)
    
    # traces and posterior densities
    pdf("PB800_MA_P7p_wt_mod.pdf")
    plot(PB800_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(PB800_MA_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(PB800_MA_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_MA_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(PB800_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(PB800_MA_P7p_wt_mod[["VCV"]])
    #autocorr.plot(PB800_MA_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(PB800_MA_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_MA_P7p_wt_mod <- PB800_MA_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB800_MA_P7p_wt_mod <- PB800_MA_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_PB800_MA_P7p_wt_mod <- rowSums(PB800_MA_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_PB800_MA_P7p_wt_mod) 
      HPDinterval(va_liab_PB800_MA_P7p_wt_mod) 
      
      mean(vlat_PB800_MA_P7p_wt_mod) 
      
      #variance of fixed effects
      X_PB800_MA_P7p_wt_mod <- PB800_MA_P7p_wt_mod[["X"]]
      beta_PB800_MA_P7p_wt_mod <- PB800_MA_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_PB800_MA_P7p_wt_mod   <- apply(beta_PB800_MA_P7p_wt_mod, 1, function(b) {var(as.vector(X_PB800_MA_P7p_wt_mod %*% b))}) 
      mean(vf_PB800_MA_P7p_wt_mod) 
      
      h2_liab_PB800_MA_P7p_wt_mod <- va_liab_PB800_MA_P7p_wt_mod / (vlat_PB800_MA_P7p_wt_mod + vf_PB800_MA_P7p_wt_mod)
      mean(h2_liab_PB800_MA_P7p_wt_mod) 
      posterior.mode(h2_liab_PB800_MA_P7p_wt_mod)	
      median(h2_liab_PB800_MA_P7p_wt_mod)		
      HPDinterval(h2_liab_PB800_MA_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_PB800_MA_P7p_wt_mod <- rowMeans(PB800_MA_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_PB800_MA_P7p_wt_mod <- (va_liab_PB800_MA_P7p_wt_mod/2) / (trait_mean_liab_PB800_MA_P7p_wt_mod)^2
    mean(Evol_liab_PB800_MA_P7p_wt_mod)
    
    #PB800_MA_P7p_wt_mod data scale
    {
      
      predict_PB800_MA_P7p_wt_mod <- map(1:nrow(PB800_MA_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_PB800_MA_P7p_wt_mod %*% PB800_MA_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_PB800_MA_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_PB800_MA_P7p_wt_mod,
                      var.a = PB800_MA_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_MA_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_MA_P7p_wt_mod <- data_PB800_MA_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_PB800_MA_P7p_wt_mod <- data_PB800_MA_P7p_wt_mod[["mean.obs"]]
      va_data_PB800_MA_P7p_wt_mod <- data_PB800_MA_P7p_wt_mod[["var.a.obs"]]
      vp_data_PB800_MA_P7p_wt_mod <- data_PB800_MA_P7p_wt_mod[["var.obs"]]
      
      Evol_data_PB800_MA_P7p_wt_mod <- (va_data_PB800_MA_P7p_wt_mod/2) / (trait_mean_data_PB800_MA_P7p_wt_mod)^2
      
      mean(h2_data_PB800_MA_P7p_wt_mod)
      mean(trait_mean_data_PB800_MA_P7p_wt_mod)
      mean(va_data_PB800_MA_P7p_wt_mod)
      mean(vp_data_PB800_MA_P7p_wt_mod)
      mean(Evol_data_PB800_MA_P7p_wt_mod)
      
    }
    
  }
  
  #PB800_Vm_P7p_wt_mod 
  {
    
    PB800_Vm_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Treatment -1,
                                    random      = ~ LineB + BlockRep,
                                    family      = "threshold",
                                    data        = Vm_PB800_data,
                                    prior       = prior_bi_block,
                                    nitt        = 1260000,       
                                    thin        = 500,           
                                    burnin      = 10000,
                                    trunc       = TRUE,
                                    pr          = TRUE,
                                    pl          = TRUE)            
    
    saveRDS(PB800_Vm_P7p_wt_mod, file = "PB800_Vm_P7p_wt_mod.rds")
    PB800_Vm_P7p_wt_mod <- readRDS("PB800_Vm_P7p_wt_mod.rds")
    
    summary(PB800_Vm_P7p_wt_mod) 
    plotTrace(PB800_Vm_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("PB800_Vm_P7p_wt_mod.pdf")
    plot(PB800_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(PB800_Vm_P7p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(PB800_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(PB800_Vm_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(PB800_Vm_P7p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(PB800_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(PB800_Vm_P7p_wt_mod[["VCV"]])
    #autocorr.plot(PB800_Vm_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(PB800_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(PB800_Vm_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_PB800_Vm_P7p_wt_mod <- PB800_Vm_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_PB800_Vm_P7p_wt_mod <- PB800_Vm_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_PB800_Vm_P7p_wt_mod <- rowSums(PB800_Vm_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_PB800_Vm_P7p_wt_mod) 
      HPDinterval(va_liab_PB800_Vm_P7p_wt_mod) 
      
      mean(vlat_PB800_Vm_P7p_wt_mod) 
      
      #variance of fixed effects
      X_PB800_Vm_P7p_wt_mod <- PB800_Vm_P7p_wt_mod[["X"]]
      beta_PB800_Vm_P7p_wt_mod <- PB800_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]
      vf_PB800_Vm_P7p_wt_mod   <- apply(beta_PB800_Vm_P7p_wt_mod, 1, function(b) {var(as.vector(X_PB800_Vm_P7p_wt_mod %*% b))}) 
      mean(vf_PB800_Vm_P7p_wt_mod) 
      
      
      h2_liab_PB800_Vm_P7p_wt_mod <- va_liab_PB800_Vm_P7p_wt_mod / (vlat_PB800_Vm_P7p_wt_mod + vf_PB800_Vm_P7p_wt_mod)
      mean(h2_liab_PB800_Vm_P7p_wt_mod) 
      posterior.mode(h2_liab_PB800_Vm_P7p_wt_mod)	
      median(h2_liab_PB800_Vm_P7p_wt_mod)		
      HPDinterval(h2_liab_PB800_Vm_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_PB800_Vm_P7p_wt_mod <- ((rowMeans(PB800_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(PB800_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + PB800_Vm_P7p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_PB800_Vm_P7p_wt_mod <- (va_liab_PB800_Vm_P7p_wt_mod/2) / (trait_mean_liab_PB800_Vm_P7p_wt_mod)^2
    mean(Evol_liab_PB800_Vm_P7p_wt_mod)
    
    
    #PB800_Vm_P7p_wt_mod data scale
    {
      
      predict_PB800_Vm_P7p_wt_mod <- map(1:nrow(PB800_Vm_P7p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_PB800_Vm_P7p_wt_mod %*% PB800_Vm_P7p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_PB800_Vm_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_PB800_Vm_P7p_wt_mod,
                      var.a = PB800_Vm_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(PB800_Vm_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_PB800_Vm_P7p_wt_mod <- data_PB800_Vm_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_PB800_Vm_P7p_wt_mod <- data_PB800_Vm_P7p_wt_mod[["mean.obs"]]
      va_data_PB800_Vm_P7p_wt_mod <- data_PB800_Vm_P7p_wt_mod[["var.a.obs"]]
      vp_data_PB800_Vm_P7p_wt_mod <- data_PB800_Vm_P7p_wt_mod[["var.obs"]]
      
      Evol_data_PB800_Vm_P7p_wt_mod <- (va_data_PB800_Vm_P7p_wt_mod/2) / (trait_mean_data_PB800_Vm_P7p_wt_mod)^2
      
      mean(h2_data_PB800_Vm_P7p_wt_mod)
      mean(trait_mean_data_PB800_Vm_P7p_wt_mod)
      mean(va_data_PB800_Vm_P7p_wt_mod)
      mean(vp_data_PB800_Vm_P7p_wt_mod)
      mean(Evol_data_PB800_Vm_P7p_wt_mod)
      
    }
    
  }
  
  
 


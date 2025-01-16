##Vm_Oscheius_Species_SSSS

library(readxl)
library(Matrix)
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

Vm_Otipulae_data <- subset(Vm_Oscheius_data, Species =="O.tipulae")
Vm_Oonirici_data <- subset(Vm_Oscheius_data, Species =="O.onirici")



prior_bi_block <-
  list( R = list(V = 1, fix = 1),      # Fixing the "residual" variance to 1 because it is not identifiable in binary responses 
        G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1),
                 G2 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1)))





#Otipulae
{
  Vm_Otipulae_bi_CONTROL <- subset(Vm_Otipulae_data, Treatment =="CONTROL")
  Vm_Otipulae_bi_MA <- subset(Vm_Otipulae_data, Treatment =="ML")
  
  
  ##---- Otipulae P3p ----
  
  
  #Otipulae_CONTROL_P3p_SSSS_mod 
  {
    
    Otipulae_CONTROL_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer + Ancestral -1,
                                           random      = ~ LineB + BlockRep,
                                           family      = "threshold",
                                           data        = Vm_Otipulae_bi_CONTROL,
                                           prior       = prior_bi_block,
                                           nitt        = 1260000,       
                                           thin        = 500,           
                                           burnin      = 10000,
                                           trunc       = TRUE,
                                           pr          = TRUE,
                                           pl          = TRUE)            
    
    saveRDS(Otipulae_CONTROL_P3p_SSSS_mod, file = "Otipulae_CONTROL_P3p_SSSS_mod.rds")
    Otipulae_CONTROL_P3p_SSSS_mod <- readRDS("Otipulae_CONTROL_P3p_SSSS_mod.rds")
    
    summary(Otipulae_CONTROL_P3p_SSSS_mod) 
    plotTrace(Otipulae_CONTROL_P3p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Otipulae_CONTROL_P3p_SSSS_mod.pdf")
    plot(Otipulae_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(Otipulae_CONTROL_P3p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_CONTROL_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(Otipulae_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_CONTROL_P3p_SSSS_mod <- Otipulae_CONTROL_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_CONTROL_P3p_SSSS_mod <- Otipulae_CONTROL_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Otipulae_CONTROL_P3p_SSSS_mod <- rowSums(Otipulae_CONTROL_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Otipulae_CONTROL_P3p_SSSS_mod) 
      HPDinterval(va_liab_Otipulae_CONTROL_P3p_SSSS_mod) 
      
      mean(vlat_Otipulae_CONTROL_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_Otipulae_CONTROL_P3p_SSSS_mod <- Otipulae_CONTROL_P3p_SSSS_mod[["X"]]
      beta_Otipulae_CONTROL_P3p_SSSS_mod <- Otipulae_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_Otipulae_CONTROL_P3p_SSSS_mod   <- apply(beta_Otipulae_CONTROL_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_Otipulae_CONTROL_P3p_SSSS_mod %*% b))}) 
      mean(vf_Otipulae_CONTROL_P3p_SSSS_mod) 
      
      h2_liab_Otipulae_CONTROL_P3p_SSSS_mod <- va_liab_Otipulae_CONTROL_P3p_SSSS_mod / (vlat_Otipulae_CONTROL_P3p_SSSS_mod + vf_Otipulae_CONTROL_P3p_SSSS_mod)
      mean(h2_liab_Otipulae_CONTROL_P3p_SSSS_mod) 
      posterior.mode(h2_liab_Otipulae_CONTROL_P3p_SSSS_mod)	
      median(h2_liab_Otipulae_CONTROL_P3p_SSSS_mod)		
      HPDinterval(h2_liab_Otipulae_CONTROL_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_CONTROL_P3p_SSSS_mod <- ((rowMeans(Otipulae_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_CONTROL_P3p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Otipulae_CONTROL_P3p_SSSS_mod <- (va_liab_Otipulae_CONTROL_P3p_SSSS_mod/2) / (trait_mean_liab_Otipulae_CONTROL_P3p_SSSS_mod)^2
    mean(Evol_liab_Otipulae_CONTROL_P3p_SSSS_mod)
    
    
    #Otipulae_CONTROL_P3p_SSSS_mod data scale
    {
      
      predict_Otipulae_CONTROL_P3p_SSSS_mod <- map(1:nrow(Otipulae_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_CONTROL_P3p_SSSS_mod %*% Otipulae_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_CONTROL_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Otipulae_CONTROL_P3p_SSSS_mod,
                      var.a = Otipulae_CONTROL_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_CONTROL_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_CONTROL_P3p_SSSS_mod <- data_Otipulae_CONTROL_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Otipulae_CONTROL_P3p_SSSS_mod <- data_Otipulae_CONTROL_P3p_SSSS_mod[["mean.obs"]]
      va_data_Otipulae_CONTROL_P3p_SSSS_mod <- data_Otipulae_CONTROL_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_Otipulae_CONTROL_P3p_SSSS_mod <- data_Otipulae_CONTROL_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_Otipulae_CONTROL_P3p_SSSS_mod <- (va_data_Otipulae_CONTROL_P3p_SSSS_mod/2) / (trait_mean_data_Otipulae_CONTROL_P3p_SSSS_mod)^2
      
      mean(h2_data_Otipulae_CONTROL_P3p_SSSS_mod) 
      mean(trait_mean_data_Otipulae_CONTROL_P3p_SSSS_mod)
      mean(va_data_Otipulae_CONTROL_P3p_SSSS_mod) 
      mean(vp_data_Otipulae_CONTROL_P3p_SSSS_mod)
      mean(Evol_data_Otipulae_CONTROL_P3p_SSSS_mod)
      
    }
    
  }
  
  #Otipulae_MA_P3p_SSSS_mod 
  {
    
    Otipulae_MA_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer + Ancestral -1,
                                      random      = ~ LineB + BlockRep,
                                      family      = "threshold",
                                      data        = Vm_Otipulae_bi_MA,
                                      prior       = prior_bi_block,
                                      nitt        = 1260000,       
                                      thin        = 500,           
                                      burnin      = 10000,
                                      trunc       = TRUE,
                                      pr          = TRUE,
                                      pl          = TRUE)         
    
    saveRDS(Otipulae_MA_P3p_SSSS_mod, file = "Otipulae_MA_P3p_SSSS_mod.rds")
    Otipulae_MA_P3p_SSSS_mod <- readRDS("Otipulae_MA_P3p_SSSS_mod.rds")
    
    summary(Otipulae_MA_P3p_SSSS_mod) 
    #plot(Otipulae_MA_P3p_SSSS_mod)
    
    # traces and posterior densities
    pdf("Otipulae_MA_P3p_SSSS_mod.pdf")
    plot(Otipulae_MA_P3p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(Otipulae_MA_P3p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_MA_P3p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_MA_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_MA_P3p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_MA_P3p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_MA_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(Otipulae_MA_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_MA_P3p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_MA_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_MA_P3p_SSSS_mod <- Otipulae_MA_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_MA_P3p_SSSS_mod <- Otipulae_MA_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Otipulae_MA_P3p_SSSS_mod <- rowSums(Otipulae_MA_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Otipulae_MA_P3p_SSSS_mod) 
      HPDinterval(va_liab_Otipulae_MA_P3p_SSSS_mod) 
      
      mean(vlat_Otipulae_MA_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_Otipulae_MA_P3p_SSSS_mod <- Otipulae_MA_P3p_SSSS_mod[["X"]]
      beta_Otipulae_MA_P3p_SSSS_mod <- Otipulae_MA_P3p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_Otipulae_MA_P3p_SSSS_mod   <- apply(beta_Otipulae_MA_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_Otipulae_MA_P3p_SSSS_mod %*% b))}) 
      mean(vf_Otipulae_MA_P3p_SSSS_mod) 
      
      h2_liab_Otipulae_MA_P3p_SSSS_mod <- va_liab_Otipulae_MA_P3p_SSSS_mod / (vlat_Otipulae_MA_P3p_SSSS_mod + vf_Otipulae_MA_P3p_SSSS_mod)
      mean(h2_liab_Otipulae_MA_P3p_SSSS_mod) 
      posterior.mode(h2_liab_Otipulae_MA_P3p_SSSS_mod)	
      median(h2_liab_Otipulae_MA_P3p_SSSS_mod)		
      HPDinterval(h2_liab_Otipulae_MA_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_MA_P3p_SSSS_mod <- ((rowMeans(Otipulae_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_MA_P3p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Otipulae_MA_P3p_SSSS_mod <- (va_liab_Otipulae_MA_P3p_SSSS_mod/2) / (trait_mean_liab_Otipulae_MA_P3p_SSSS_mod)^2
    mean(Evol_liab_Otipulae_MA_P3p_SSSS_mod)
    
    #Otipulae_MA_P3p_SSSS_mod data scale
    {
      
      predict_Otipulae_MA_P3p_SSSS_mod <- map(1:nrow(Otipulae_MA_P3p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_MA_P3p_SSSS_mod %*% Otipulae_MA_P3p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_MA_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Otipulae_MA_P3p_SSSS_mod,
                      var.a = Otipulae_MA_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_MA_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_MA_P3p_SSSS_mod <- data_Otipulae_MA_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Otipulae_MA_P3p_SSSS_mod <- data_Otipulae_MA_P3p_SSSS_mod[["mean.obs"]]
      va_data_Otipulae_MA_P3p_SSSS_mod <- data_Otipulae_MA_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_Otipulae_MA_P3p_SSSS_mod <- data_Otipulae_MA_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_Otipulae_MA_P3p_SSSS_mod <- (va_data_Otipulae_MA_P3p_SSSS_mod/2) / (trait_mean_data_Otipulae_MA_P3p_SSSS_mod)^2
      
      mean(h2_data_Otipulae_MA_P3p_SSSS_mod)
      mean(trait_mean_data_Otipulae_MA_P3p_SSSS_mod)
      mean(va_data_Otipulae_MA_P3p_SSSS_mod)
      mean(vp_data_Otipulae_MA_P3p_SSSS_mod)
      mean(Evol_data_Otipulae_MA_P3p_SSSS_mod)
      
    }
    
  }
  
  #Otipulae_Vm_P3p_SSSS_mod 
  {
    
    Otipulae_Vm_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer + Treatment:Ancestral -1,
                                      random      = ~ LineB + BlockRep,
                                      family      = "threshold",
                                      data        = Vm_Otipulae_data,
                                      prior       = prior_bi_block,
                                      nitt        = 1260000,       
                                      thin        = 500,           
                                      burnin      = 10000,
                                      trunc       = TRUE,
                                      pr          = TRUE,
                                      pl          = TRUE)            
    
    saveRDS(Otipulae_Vm_P3p_SSSS_mod, file = "Otipulae_Vm_P3p_SSSS_mod.rds")
    Otipulae_Vm_P3p_SSSS_mod <- readRDS("Otipulae_Vm_P3p_SSSS_mod.rds")
    
    summary(Otipulae_Vm_P3p_SSSS_mod) 
    plotTrace(Otipulae_Vm_P3p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("Otipulae_Vm_P3p_SSSS_mod.pdf")
    plot(Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,c(1:7)]) 
    plot(Otipulae_Vm_P3p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,c(1:7)]) #
    heidel.diag(Otipulae_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,c(1:7)])
    #autocorr.plot(Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,c(1:7)]) #
    autocorr.diag(Otipulae_Vm_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(Otipulae_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,c(1:7)]) # 
    effectiveSize(Otipulae_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_Vm_P3p_SSSS_mod <- Otipulae_Vm_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_Vm_P3p_SSSS_mod <- Otipulae_Vm_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Otipulae_Vm_P3p_SSSS_mod <- rowSums(Otipulae_Vm_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Otipulae_Vm_P3p_SSSS_mod) 
      HPDinterval(va_liab_Otipulae_Vm_P3p_SSSS_mod) 
      
      mean(vlat_Otipulae_Vm_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_Otipulae_Vm_P3p_SSSS_mod <- Otipulae_Vm_P3p_SSSS_mod[["X"]]
      beta_Otipulae_Vm_P3p_SSSS_mod <- Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,c(1:7)]
      vf_Otipulae_Vm_P3p_SSSS_mod   <- apply(beta_Otipulae_Vm_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_Otipulae_Vm_P3p_SSSS_mod %*% b))}) 
      mean(vf_Otipulae_Vm_P3p_SSSS_mod) 
      
      h2_liab_Otipulae_Vm_P3p_SSSS_mod <- va_liab_Otipulae_Vm_P3p_SSSS_mod / (vlat_Otipulae_Vm_P3p_SSSS_mod + vf_Otipulae_Vm_P3p_SSSS_mod)
      mean(h2_liab_Otipulae_Vm_P3p_SSSS_mod) 
      posterior.mode(h2_liab_Otipulae_Vm_P3p_SSSS_mod)	
      median(h2_liab_Otipulae_Vm_P3p_SSSS_mod)		
      HPDinterval(h2_liab_Otipulae_Vm_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    mean(rowMeans(Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)])) 
    mean(rowMeans(Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,5]) 
    mean(rowMeans(Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,6]) 
    mean(rowMeans(Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,7]) 
    
    trait_mean_liab_Otipulae_Vm_P3p_SSSS_mod <- ((rowMeans(Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,5]) + (rowMeans(Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,6]) + (rowMeans(Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,7]))/4)
    
    Evol_liab_Otipulae_Vm_P3p_SSSS_mod <- (va_liab_Otipulae_Vm_P3p_SSSS_mod/2) / (trait_mean_liab_Otipulae_Vm_P3p_SSSS_mod)^2
    mean(Evol_liab_Otipulae_Vm_P3p_SSSS_mod)
    
    #Otipulae_Vm_P3p_SSSS_mod data scale
    {
      
      predict_Otipulae_Vm_P3p_SSSS_mod <- map(1:nrow(Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,c(1:7)]), ~ as.vector(X_Otipulae_Vm_P3p_SSSS_mod %*% Otipulae_Vm_P3p_SSSS_mod[["Sol"]][,c(1:7)][., ]))
      
      data_Otipulae_Vm_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Otipulae_Vm_P3p_SSSS_mod,
                      var.a = Otipulae_Vm_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_Vm_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_Vm_P3p_SSSS_mod <- data_Otipulae_Vm_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Otipulae_Vm_P3p_SSSS_mod <- data_Otipulae_Vm_P3p_SSSS_mod[["mean.obs"]]
      va_data_Otipulae_Vm_P3p_SSSS_mod <- data_Otipulae_Vm_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_Otipulae_Vm_P3p_SSSS_mod <- data_Otipulae_Vm_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_Otipulae_Vm_P3p_SSSS_mod <- (va_data_Otipulae_Vm_P3p_SSSS_mod/2) / (trait_mean_data_Otipulae_Vm_P3p_SSSS_mod)^2
      
      mean(h2_data_Otipulae_Vm_P3p_SSSS_mod)
      mean(trait_mean_data_Otipulae_Vm_P3p_SSSS_mod)
      mean(va_data_Otipulae_Vm_P3p_SSSS_mod)
      mean(vp_data_Otipulae_Vm_P3p_SSSS_mod)
      mean(Evol_data_Otipulae_Vm_P3p_SSSS_mod)
      
    }
    
  }
  
  
  
  
  
  ##---- Otipulae P4p ----
  
  
  #Otipulae_CONTROL_P4p_SSSS_mod 
  {
    
    Otipulae_CONTROL_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer + Ancestral -1,
                                           random      = ~ LineB + BlockRep,
                                           family      = "threshold",
                                           data        = Vm_Otipulae_bi_CONTROL,
                                           prior       = prior_bi_block,
                                           nitt        = 1260000,       
                                           thin        = 500,           
                                           burnin      = 10000,
                                           trunc       = TRUE,
                                           pr          = TRUE,
                                           pl          = TRUE)            
    
    saveRDS(Otipulae_CONTROL_P4p_SSSS_mod, file = "Otipulae_CONTROL_P4p_SSSS_mod.rds")
    Otipulae_CONTROL_P4p_SSSS_mod <- readRDS("Otipulae_CONTROL_P4p_SSSS_mod.rds")
    
    summary(Otipulae_CONTROL_P4p_SSSS_mod) 
    plotTrace(Otipulae_CONTROL_P4p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Otipulae_CONTROL_P4p_SSSS_mod.pdf")
    plot(Otipulae_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(Otipulae_CONTROL_P4p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_CONTROL_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(Otipulae_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_CONTROL_P4p_SSSS_mod <- Otipulae_CONTROL_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_CONTROL_P4p_SSSS_mod <- Otipulae_CONTROL_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Otipulae_CONTROL_P4p_SSSS_mod <- rowSums(Otipulae_CONTROL_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Otipulae_CONTROL_P4p_SSSS_mod) 
      HPDinterval(va_liab_Otipulae_CONTROL_P4p_SSSS_mod) 
      
      mean(vlat_Otipulae_CONTROL_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_Otipulae_CONTROL_P4p_SSSS_mod <- Otipulae_CONTROL_P4p_SSSS_mod[["X"]]
      beta_Otipulae_CONTROL_P4p_SSSS_mod <- Otipulae_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_Otipulae_CONTROL_P4p_SSSS_mod   <- apply(beta_Otipulae_CONTROL_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_Otipulae_CONTROL_P4p_SSSS_mod %*% b))}) 
      mean(vf_Otipulae_CONTROL_P4p_SSSS_mod) 
      
      h2_liab_Otipulae_CONTROL_P4p_SSSS_mod <- va_liab_Otipulae_CONTROL_P4p_SSSS_mod / (vlat_Otipulae_CONTROL_P4p_SSSS_mod + vf_Otipulae_CONTROL_P4p_SSSS_mod)
      mean(h2_liab_Otipulae_CONTROL_P4p_SSSS_mod) 
      posterior.mode(h2_liab_Otipulae_CONTROL_P4p_SSSS_mod)	
      median(h2_liab_Otipulae_CONTROL_P4p_SSSS_mod)		
      HPDinterval(h2_liab_Otipulae_CONTROL_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_CONTROL_P4p_SSSS_mod <- ((rowMeans(Otipulae_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_CONTROL_P4p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Otipulae_CONTROL_P4p_SSSS_mod <- (va_liab_Otipulae_CONTROL_P4p_SSSS_mod/2) / (trait_mean_liab_Otipulae_CONTROL_P4p_SSSS_mod)^2
    mean(Evol_liab_Otipulae_CONTROL_P4p_SSSS_mod)
    
    
    #Otipulae_CONTROL_P4p_SSSS_mod data scale
    {
      
      predict_Otipulae_CONTROL_P4p_SSSS_mod <- map(1:nrow(Otipulae_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_CONTROL_P4p_SSSS_mod %*% Otipulae_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_CONTROL_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Otipulae_CONTROL_P4p_SSSS_mod,
                      var.a = Otipulae_CONTROL_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_CONTROL_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_CONTROL_P4p_SSSS_mod <- data_Otipulae_CONTROL_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Otipulae_CONTROL_P4p_SSSS_mod <- data_Otipulae_CONTROL_P4p_SSSS_mod[["mean.obs"]]
      va_data_Otipulae_CONTROL_P4p_SSSS_mod <- data_Otipulae_CONTROL_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_Otipulae_CONTROL_P4p_SSSS_mod <- data_Otipulae_CONTROL_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_Otipulae_CONTROL_P4p_SSSS_mod <- (va_data_Otipulae_CONTROL_P4p_SSSS_mod/2) / (trait_mean_data_Otipulae_CONTROL_P4p_SSSS_mod)^2
      
      mean(h2_data_Otipulae_CONTROL_P4p_SSSS_mod) 
      mean(trait_mean_data_Otipulae_CONTROL_P4p_SSSS_mod)
      mean(va_data_Otipulae_CONTROL_P4p_SSSS_mod) 
      mean(vp_data_Otipulae_CONTROL_P4p_SSSS_mod)
      mean(Evol_data_Otipulae_CONTROL_P4p_SSSS_mod)
      
    }
    
  }
  
  #Otipulae_MA_P4p_SSSS_mod 
  {
    
    Otipulae_MA_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer + Ancestral -1,
                                      random      = ~ LineB + BlockRep,
                                      family      = "threshold",
                                      data        = Vm_Otipulae_bi_MA,
                                      prior       = prior_bi_block,
                                      nitt        = 1260000,       
                                      thin        = 500,           
                                      burnin      = 10000,
                                      trunc       = TRUE,
                                      pr          = TRUE,
                                      pl          = TRUE)         
    
    saveRDS(Otipulae_MA_P4p_SSSS_mod, file = "Otipulae_MA_P4p_SSSS_mod.rds")
    Otipulae_MA_P4p_SSSS_mod <- readRDS("Otipulae_MA_P4p_SSSS_mod.rds")
    
    summary(Otipulae_MA_P4p_SSSS_mod) 
    #plot(Otipulae_MA_P4p_SSSS_mod)
    
    # traces and posterior densities
    pdf("Otipulae_MA_P4p_SSSS_mod.pdf")
    plot(Otipulae_MA_P4p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(Otipulae_MA_P4p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_MA_P4p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_MA_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_MA_P4p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_MA_P4p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_MA_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(Otipulae_MA_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_MA_P4p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_MA_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_MA_P4p_SSSS_mod <- Otipulae_MA_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_MA_P4p_SSSS_mod <- Otipulae_MA_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Otipulae_MA_P4p_SSSS_mod <- rowSums(Otipulae_MA_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Otipulae_MA_P4p_SSSS_mod) 
      HPDinterval(va_liab_Otipulae_MA_P4p_SSSS_mod) 
      
      mean(vlat_Otipulae_MA_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_Otipulae_MA_P4p_SSSS_mod <- Otipulae_MA_P4p_SSSS_mod[["X"]]
      beta_Otipulae_MA_P4p_SSSS_mod <- Otipulae_MA_P4p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_Otipulae_MA_P4p_SSSS_mod   <- apply(beta_Otipulae_MA_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_Otipulae_MA_P4p_SSSS_mod %*% b))}) 
      mean(vf_Otipulae_MA_P4p_SSSS_mod) 
      
      h2_liab_Otipulae_MA_P4p_SSSS_mod <- va_liab_Otipulae_MA_P4p_SSSS_mod / (vlat_Otipulae_MA_P4p_SSSS_mod + vf_Otipulae_MA_P4p_SSSS_mod)
      mean(h2_liab_Otipulae_MA_P4p_SSSS_mod) 
      posterior.mode(h2_liab_Otipulae_MA_P4p_SSSS_mod)	
      median(h2_liab_Otipulae_MA_P4p_SSSS_mod)		
      HPDinterval(h2_liab_Otipulae_MA_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_MA_P4p_SSSS_mod <- ((rowMeans(Otipulae_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_MA_P4p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Otipulae_MA_P4p_SSSS_mod <- (va_liab_Otipulae_MA_P4p_SSSS_mod/2) / (trait_mean_liab_Otipulae_MA_P4p_SSSS_mod)^2
    mean(Evol_liab_Otipulae_MA_P4p_SSSS_mod)
    
    #Otipulae_MA_P4p_SSSS_mod data scale
    {
      
      predict_Otipulae_MA_P4p_SSSS_mod <- map(1:nrow(Otipulae_MA_P4p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_MA_P4p_SSSS_mod %*% Otipulae_MA_P4p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_MA_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Otipulae_MA_P4p_SSSS_mod,
                      var.a = Otipulae_MA_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_MA_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_MA_P4p_SSSS_mod <- data_Otipulae_MA_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Otipulae_MA_P4p_SSSS_mod <- data_Otipulae_MA_P4p_SSSS_mod[["mean.obs"]]
      va_data_Otipulae_MA_P4p_SSSS_mod <- data_Otipulae_MA_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_Otipulae_MA_P4p_SSSS_mod <- data_Otipulae_MA_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_Otipulae_MA_P4p_SSSS_mod <- (va_data_Otipulae_MA_P4p_SSSS_mod/2) / (trait_mean_data_Otipulae_MA_P4p_SSSS_mod)^2
      
      mean(h2_data_Otipulae_MA_P4p_SSSS_mod)
      mean(trait_mean_data_Otipulae_MA_P4p_SSSS_mod)
      mean(va_data_Otipulae_MA_P4p_SSSS_mod)
      mean(vp_data_Otipulae_MA_P4p_SSSS_mod)
      mean(Evol_data_Otipulae_MA_P4p_SSSS_mod)
      
    }
    
  }
  
  #Otipulae_Vm_P4p_SSSS_mod 
  {
    
    Otipulae_Vm_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer + Treatment:Ancestral -1,
                                      random      = ~ LineB + BlockRep,
                                      family      = "threshold",
                                      data        = Vm_Otipulae_data,
                                      prior       = prior_bi_block,
                                      nitt        = 1260000,       
                                      thin        = 500,           
                                      burnin      = 10000,
                                      trunc       = TRUE,
                                      pr          = TRUE,
                                      pl          = TRUE)            
    
    saveRDS(Otipulae_Vm_P4p_SSSS_mod, file = "Otipulae_Vm_P4p_SSSS_mod.rds")
    Otipulae_Vm_P4p_SSSS_mod <- readRDS("Otipulae_Vm_P4p_SSSS_mod.rds")
    
    summary(Otipulae_Vm_P4p_SSSS_mod) 
    plotTrace(Otipulae_Vm_P4p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("Otipulae_Vm_P4p_SSSS_mod.pdf")
    plot(Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,c(1:7)]) 
    plot(Otipulae_Vm_P4p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,c(1:7)]) #
    heidel.diag(Otipulae_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,c(1:7)])
    #autocorr.plot(Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,c(1:7)]) #
    autocorr.diag(Otipulae_Vm_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(Otipulae_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,c(1:7)]) # 
    effectiveSize(Otipulae_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_Vm_P4p_SSSS_mod <- Otipulae_Vm_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_Vm_P4p_SSSS_mod <- Otipulae_Vm_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Otipulae_Vm_P4p_SSSS_mod <- rowSums(Otipulae_Vm_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Otipulae_Vm_P4p_SSSS_mod) 
      HPDinterval(va_liab_Otipulae_Vm_P4p_SSSS_mod) 
      
      mean(vlat_Otipulae_Vm_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_Otipulae_Vm_P4p_SSSS_mod <- Otipulae_Vm_P4p_SSSS_mod[["X"]]
      beta_Otipulae_Vm_P4p_SSSS_mod <- Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,c(1:7)]
      vf_Otipulae_Vm_P4p_SSSS_mod   <- apply(beta_Otipulae_Vm_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_Otipulae_Vm_P4p_SSSS_mod %*% b))}) 
      mean(vf_Otipulae_Vm_P4p_SSSS_mod) 
      
      h2_liab_Otipulae_Vm_P4p_SSSS_mod <- va_liab_Otipulae_Vm_P4p_SSSS_mod / (vlat_Otipulae_Vm_P4p_SSSS_mod + vf_Otipulae_Vm_P4p_SSSS_mod)
      mean(h2_liab_Otipulae_Vm_P4p_SSSS_mod) 
      posterior.mode(h2_liab_Otipulae_Vm_P4p_SSSS_mod)	
      median(h2_liab_Otipulae_Vm_P4p_SSSS_mod)		
      HPDinterval(h2_liab_Otipulae_Vm_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    mean(rowMeans(Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)])) 
    mean(rowMeans(Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,5]) 
    mean(rowMeans(Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,6]) 
    mean(rowMeans(Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,7]) 
    
    trait_mean_liab_Otipulae_Vm_P4p_SSSS_mod <- ((rowMeans(Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,5]) + (rowMeans(Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,6]) + (rowMeans(Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,7]))/4)
    
    Evol_liab_Otipulae_Vm_P4p_SSSS_mod <- (va_liab_Otipulae_Vm_P4p_SSSS_mod/2) / (trait_mean_liab_Otipulae_Vm_P4p_SSSS_mod)^2
    mean(Evol_liab_Otipulae_Vm_P4p_SSSS_mod)
    
    #Otipulae_Vm_P4p_SSSS_mod data scale
    {
      
      predict_Otipulae_Vm_P4p_SSSS_mod <- map(1:nrow(Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,c(1:7)]), ~ as.vector(X_Otipulae_Vm_P4p_SSSS_mod %*% Otipulae_Vm_P4p_SSSS_mod[["Sol"]][,c(1:7)][., ]))
      
      data_Otipulae_Vm_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Otipulae_Vm_P4p_SSSS_mod,
                      var.a = Otipulae_Vm_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_Vm_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_Vm_P4p_SSSS_mod <- data_Otipulae_Vm_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Otipulae_Vm_P4p_SSSS_mod <- data_Otipulae_Vm_P4p_SSSS_mod[["mean.obs"]]
      va_data_Otipulae_Vm_P4p_SSSS_mod <- data_Otipulae_Vm_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_Otipulae_Vm_P4p_SSSS_mod <- data_Otipulae_Vm_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_Otipulae_Vm_P4p_SSSS_mod <- (va_data_Otipulae_Vm_P4p_SSSS_mod/2) / (trait_mean_data_Otipulae_Vm_P4p_SSSS_mod)^2
      
      mean(h2_data_Otipulae_Vm_P4p_SSSS_mod)
      mean(trait_mean_data_Otipulae_Vm_P4p_SSSS_mod)
      mean(va_data_Otipulae_Vm_P4p_SSSS_mod)
      mean(vp_data_Otipulae_Vm_P4p_SSSS_mod)
      mean(Evol_data_Otipulae_Vm_P4p_SSSS_mod)
      
    }
    
  }
  
  
 
  
  ##---- Otipulae P8p ----
  
  #Otipulae_CONTROL_P8p_SSSS_mod 
  {
    
    Otipulae_CONTROL_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer + Ancestral -1,
                                           random      = ~ LineB + BlockRep,
                                           family      = "threshold",
                                           data        = Vm_Otipulae_bi_CONTROL,
                                           prior       = prior_bi_block,
                                           nitt        = 1260000,       
                                           thin        = 500,           
                                           burnin      = 10000,
                                           trunc       = TRUE,
                                           pr          = TRUE,
                                           pl          = TRUE)            
    
    saveRDS(Otipulae_CONTROL_P8p_SSSS_mod, file = "Otipulae_CONTROL_P8p_SSSS_mod.rds")
    Otipulae_CONTROL_P8p_SSSS_mod <- readRDS("Otipulae_CONTROL_P8p_SSSS_mod.rds")
    
    summary(Otipulae_CONTROL_P8p_SSSS_mod) 
    plotTrace(Otipulae_CONTROL_P8p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Otipulae_CONTROL_P8p_SSSS_mod.pdf")
    plot(Otipulae_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(Otipulae_CONTROL_P8p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_CONTROL_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(Otipulae_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_CONTROL_P8p_SSSS_mod <- Otipulae_CONTROL_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_CONTROL_P8p_SSSS_mod <- Otipulae_CONTROL_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Otipulae_CONTROL_P8p_SSSS_mod <- rowSums(Otipulae_CONTROL_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Otipulae_CONTROL_P8p_SSSS_mod) 
      HPDinterval(va_liab_Otipulae_CONTROL_P8p_SSSS_mod) 
      
      mean(vlat_Otipulae_CONTROL_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_Otipulae_CONTROL_P8p_SSSS_mod <- Otipulae_CONTROL_P8p_SSSS_mod[["X"]]
      beta_Otipulae_CONTROL_P8p_SSSS_mod <- Otipulae_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_Otipulae_CONTROL_P8p_SSSS_mod   <- apply(beta_Otipulae_CONTROL_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_Otipulae_CONTROL_P8p_SSSS_mod %*% b))}) 
      mean(vf_Otipulae_CONTROL_P8p_SSSS_mod) 
      
      h2_liab_Otipulae_CONTROL_P8p_SSSS_mod <- va_liab_Otipulae_CONTROL_P8p_SSSS_mod / (vlat_Otipulae_CONTROL_P8p_SSSS_mod + vf_Otipulae_CONTROL_P8p_SSSS_mod)
      mean(h2_liab_Otipulae_CONTROL_P8p_SSSS_mod) 
      posterior.mode(h2_liab_Otipulae_CONTROL_P8p_SSSS_mod)	
      median(h2_liab_Otipulae_CONTROL_P8p_SSSS_mod)		
      HPDinterval(h2_liab_Otipulae_CONTROL_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_CONTROL_P8p_SSSS_mod <- ((rowMeans(Otipulae_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_CONTROL_P8p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Otipulae_CONTROL_P8p_SSSS_mod <- (va_liab_Otipulae_CONTROL_P8p_SSSS_mod/2) / (trait_mean_liab_Otipulae_CONTROL_P8p_SSSS_mod)^2
    mean(Evol_liab_Otipulae_CONTROL_P8p_SSSS_mod)
    
    
    #Otipulae_CONTROL_P8p_SSSS_mod data scale
    {
      
      predict_Otipulae_CONTROL_P8p_SSSS_mod <- map(1:nrow(Otipulae_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_CONTROL_P8p_SSSS_mod %*% Otipulae_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_CONTROL_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Otipulae_CONTROL_P8p_SSSS_mod,
                      var.a = Otipulae_CONTROL_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_CONTROL_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_CONTROL_P8p_SSSS_mod <- data_Otipulae_CONTROL_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Otipulae_CONTROL_P8p_SSSS_mod <- data_Otipulae_CONTROL_P8p_SSSS_mod[["mean.obs"]]
      va_data_Otipulae_CONTROL_P8p_SSSS_mod <- data_Otipulae_CONTROL_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_Otipulae_CONTROL_P8p_SSSS_mod <- data_Otipulae_CONTROL_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_Otipulae_CONTROL_P8p_SSSS_mod <- (va_data_Otipulae_CONTROL_P8p_SSSS_mod/2) / (trait_mean_data_Otipulae_CONTROL_P8p_SSSS_mod)^2
      
      mean(h2_data_Otipulae_CONTROL_P8p_SSSS_mod) 
      mean(trait_mean_data_Otipulae_CONTROL_P8p_SSSS_mod)
      mean(va_data_Otipulae_CONTROL_P8p_SSSS_mod) 
      mean(vp_data_Otipulae_CONTROL_P8p_SSSS_mod)
      mean(Evol_data_Otipulae_CONTROL_P8p_SSSS_mod)
      
    }
    
  }
  
  #Otipulae_MA_P8p_SSSS_mod 
  {
    
    Otipulae_MA_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer + Ancestral -1,
                                      random      = ~ LineB + BlockRep,
                                      family      = "threshold",
                                      data        = Vm_Otipulae_bi_MA,
                                      prior       = prior_bi_block,
                                      nitt        = 1260000,       
                                      thin        = 500,           
                                      burnin      = 10000,
                                      trunc       = TRUE,
                                      pr          = TRUE,
                                      pl          = TRUE)         
    
    saveRDS(Otipulae_MA_P8p_SSSS_mod, file = "Otipulae_MA_P8p_SSSS_mod.rds")
    Otipulae_MA_P8p_SSSS_mod <- readRDS("Otipulae_MA_P8p_SSSS_mod.rds")
    
    summary(Otipulae_MA_P8p_SSSS_mod) 
    #plot(Otipulae_MA_P8p_SSSS_mod)
    
    # traces and posterior densities
    pdf("Otipulae_MA_P8p_SSSS_mod.pdf")
    plot(Otipulae_MA_P8p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(Otipulae_MA_P8p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_MA_P8p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_MA_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_MA_P8p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_MA_P8p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_MA_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(Otipulae_MA_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_MA_P8p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_MA_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_MA_P8p_SSSS_mod <- Otipulae_MA_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_MA_P8p_SSSS_mod <- Otipulae_MA_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Otipulae_MA_P8p_SSSS_mod <- rowSums(Otipulae_MA_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Otipulae_MA_P8p_SSSS_mod) 
      HPDinterval(va_liab_Otipulae_MA_P8p_SSSS_mod) 
      
      mean(vlat_Otipulae_MA_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_Otipulae_MA_P8p_SSSS_mod <- Otipulae_MA_P8p_SSSS_mod[["X"]]
      beta_Otipulae_MA_P8p_SSSS_mod <- Otipulae_MA_P8p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_Otipulae_MA_P8p_SSSS_mod   <- apply(beta_Otipulae_MA_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_Otipulae_MA_P8p_SSSS_mod %*% b))}) 
      mean(vf_Otipulae_MA_P8p_SSSS_mod) 
      
      h2_liab_Otipulae_MA_P8p_SSSS_mod <- va_liab_Otipulae_MA_P8p_SSSS_mod / (vlat_Otipulae_MA_P8p_SSSS_mod + vf_Otipulae_MA_P8p_SSSS_mod)
      mean(h2_liab_Otipulae_MA_P8p_SSSS_mod) 
      posterior.mode(h2_liab_Otipulae_MA_P8p_SSSS_mod)	
      median(h2_liab_Otipulae_MA_P8p_SSSS_mod)		
      HPDinterval(h2_liab_Otipulae_MA_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_MA_P8p_SSSS_mod <- ((rowMeans(Otipulae_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_MA_P8p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Otipulae_MA_P8p_SSSS_mod <- (va_liab_Otipulae_MA_P8p_SSSS_mod/2) / (trait_mean_liab_Otipulae_MA_P8p_SSSS_mod)^2
    mean(Evol_liab_Otipulae_MA_P8p_SSSS_mod)
    
    #Otipulae_MA_P8p_SSSS_mod data scale
    {
      
      predict_Otipulae_MA_P8p_SSSS_mod <- map(1:nrow(Otipulae_MA_P8p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_MA_P8p_SSSS_mod %*% Otipulae_MA_P8p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_MA_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Otipulae_MA_P8p_SSSS_mod,
                      var.a = Otipulae_MA_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_MA_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_MA_P8p_SSSS_mod <- data_Otipulae_MA_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Otipulae_MA_P8p_SSSS_mod <- data_Otipulae_MA_P8p_SSSS_mod[["mean.obs"]]
      va_data_Otipulae_MA_P8p_SSSS_mod <- data_Otipulae_MA_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_Otipulae_MA_P8p_SSSS_mod <- data_Otipulae_MA_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_Otipulae_MA_P8p_SSSS_mod <- (va_data_Otipulae_MA_P8p_SSSS_mod/2) / (trait_mean_data_Otipulae_MA_P8p_SSSS_mod)^2
      
      mean(h2_data_Otipulae_MA_P8p_SSSS_mod)
      mean(trait_mean_data_Otipulae_MA_P8p_SSSS_mod)
      mean(va_data_Otipulae_MA_P8p_SSSS_mod)
      mean(vp_data_Otipulae_MA_P8p_SSSS_mod)
      mean(Evol_data_Otipulae_MA_P8p_SSSS_mod)
      
    }
    
  }
  
  #Otipulae_Vm_P8p_SSSS_mod 
  {
    
    Otipulae_Vm_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer + Treatment:Ancestral -1,
                                      random      = ~ LineB + BlockRep,
                                      family      = "threshold",
                                      data        = Vm_Otipulae_data,
                                      prior       = prior_bi_block,
                                      nitt        = 1260000,       
                                      thin        = 500,           
                                      burnin      = 10000,
                                      trunc       = TRUE,
                                      pr          = TRUE,
                                      pl          = TRUE)            
    
    saveRDS(Otipulae_Vm_P8p_SSSS_mod, file = "Otipulae_Vm_P8p_SSSS_mod.rds")
    Otipulae_Vm_P8p_SSSS_mod <- readRDS("Otipulae_Vm_P8p_SSSS_mod.rds")
    
    summary(Otipulae_Vm_P8p_SSSS_mod) 
    plotTrace(Otipulae_Vm_P8p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("Otipulae_Vm_P8p_SSSS_mod.pdf")
    plot(Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,c(1:7)]) 
    plot(Otipulae_Vm_P8p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,c(1:7)]) #
    heidel.diag(Otipulae_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,c(1:7)])
    #autocorr.plot(Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,c(1:7)]) #
    autocorr.diag(Otipulae_Vm_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(Otipulae_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,c(1:7)]) # 
    effectiveSize(Otipulae_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_Vm_P8p_SSSS_mod <- Otipulae_Vm_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_Vm_P8p_SSSS_mod <- Otipulae_Vm_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Otipulae_Vm_P8p_SSSS_mod <- rowSums(Otipulae_Vm_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Otipulae_Vm_P8p_SSSS_mod) 
      HPDinterval(va_liab_Otipulae_Vm_P8p_SSSS_mod) 
      
      mean(vlat_Otipulae_Vm_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_Otipulae_Vm_P8p_SSSS_mod <- Otipulae_Vm_P8p_SSSS_mod[["X"]]
      beta_Otipulae_Vm_P8p_SSSS_mod <- Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,c(1:7)]
      vf_Otipulae_Vm_P8p_SSSS_mod   <- apply(beta_Otipulae_Vm_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_Otipulae_Vm_P8p_SSSS_mod %*% b))}) 
      mean(vf_Otipulae_Vm_P8p_SSSS_mod) 
      
      h2_liab_Otipulae_Vm_P8p_SSSS_mod <- va_liab_Otipulae_Vm_P8p_SSSS_mod / (vlat_Otipulae_Vm_P8p_SSSS_mod + vf_Otipulae_Vm_P8p_SSSS_mod)
      mean(h2_liab_Otipulae_Vm_P8p_SSSS_mod) 
      posterior.mode(h2_liab_Otipulae_Vm_P8p_SSSS_mod)	
      median(h2_liab_Otipulae_Vm_P8p_SSSS_mod)		
      HPDinterval(h2_liab_Otipulae_Vm_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    mean(rowMeans(Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)])) 
    mean(rowMeans(Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,5]) 
    mean(rowMeans(Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,6]) 
    mean(rowMeans(Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,7]) 
    
    trait_mean_liab_Otipulae_Vm_P8p_SSSS_mod <- ((rowMeans(Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,5]) + (rowMeans(Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,6]) + (rowMeans(Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,7]))/4)
    
    Evol_liab_Otipulae_Vm_P8p_SSSS_mod <- (va_liab_Otipulae_Vm_P8p_SSSS_mod/2) / (trait_mean_liab_Otipulae_Vm_P8p_SSSS_mod)^2
    mean(Evol_liab_Otipulae_Vm_P8p_SSSS_mod)
    
    #Otipulae_Vm_P8p_SSSS_mod data scale
    {
      
      predict_Otipulae_Vm_P8p_SSSS_mod <- map(1:nrow(Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,c(1:7)]), ~ as.vector(X_Otipulae_Vm_P8p_SSSS_mod %*% Otipulae_Vm_P8p_SSSS_mod[["Sol"]][,c(1:7)][., ]))
      
      data_Otipulae_Vm_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Otipulae_Vm_P8p_SSSS_mod,
                      var.a = Otipulae_Vm_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_Vm_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_Vm_P8p_SSSS_mod <- data_Otipulae_Vm_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Otipulae_Vm_P8p_SSSS_mod <- data_Otipulae_Vm_P8p_SSSS_mod[["mean.obs"]]
      va_data_Otipulae_Vm_P8p_SSSS_mod <- data_Otipulae_Vm_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_Otipulae_Vm_P8p_SSSS_mod <- data_Otipulae_Vm_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_Otipulae_Vm_P8p_SSSS_mod <- (va_data_Otipulae_Vm_P8p_SSSS_mod/2) / (trait_mean_data_Otipulae_Vm_P8p_SSSS_mod)^2
      
      mean(h2_data_Otipulae_Vm_P8p_SSSS_mod)
      mean(trait_mean_data_Otipulae_Vm_P8p_SSSS_mod)
      mean(va_data_Otipulae_Vm_P8p_SSSS_mod)
      mean(vp_data_Otipulae_Vm_P8p_SSSS_mod)
      mean(Evol_data_Otipulae_Vm_P8p_SSSS_mod)
      
    }
    
  }
  
 ## Otipulae P5p ----
  
  
  #Otipulae_CONTROL_P5p_wt_mod 
  {
    
    Otipulae_CONTROL_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Ancestral -1,
                                            random      = ~ LineB + BlockRep,
                                            family      = "threshold",
                                            data        = Vm_Otipulae_bi_CONTROL,
                                            prior       = prior_bi_block,
                                            nitt        = 1260000,       
                                            thin        = 500,           
                                            burnin      = 10000,
                                            trunc       = TRUE,
                                            pr          = TRUE,
                                            pl          = TRUE)            
    
    saveRDS(Otipulae_CONTROL_P5p_wt_mod, file = "Otipulae_CONTROL_P5p_wt_mod.rds")
    Otipulae_CONTROL_P5p_wt_mod <- readRDS("Otipulae_CONTROL_P5p_wt_mod.rds")
    
    summary(Otipulae_CONTROL_P5p_wt_mod) 
    plotTrace(Otipulae_CONTROL_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Otipulae_CONTROL_P5p_wt_mod.pdf")
    plot(Otipulae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Otipulae_CONTROL_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_CONTROL_P5p_wt_mod[["VCV"]])
    #autocorr.plot(Otipulae_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_CONTROL_P5p_wt_mod <- Otipulae_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_CONTROL_P5p_wt_mod <- Otipulae_CONTROL_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_Otipulae_CONTROL_P5p_wt_mod <- rowSums(Otipulae_CONTROL_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_Otipulae_CONTROL_P5p_wt_mod) 
      HPDinterval(va_liab_Otipulae_CONTROL_P5p_wt_mod) 
      
      mean(vlat_Otipulae_CONTROL_P5p_wt_mod) 
      
      #variance of fixed effects
      X_Otipulae_CONTROL_P5p_wt_mod <- Otipulae_CONTROL_P5p_wt_mod[["X"]]
      beta_Otipulae_CONTROL_P5p_wt_mod <- Otipulae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]
      vf_Otipulae_CONTROL_P5p_wt_mod   <- apply(beta_Otipulae_CONTROL_P5p_wt_mod, 1, function(b) {var(as.vector(X_Otipulae_CONTROL_P5p_wt_mod %*% b))}) 
      mean(vf_Otipulae_CONTROL_P5p_wt_mod) 
      
      h2_liab_Otipulae_CONTROL_P5p_wt_mod <- va_liab_Otipulae_CONTROL_P5p_wt_mod / (vlat_Otipulae_CONTROL_P5p_wt_mod + vf_Otipulae_CONTROL_P5p_wt_mod)
      mean(h2_liab_Otipulae_CONTROL_P5p_wt_mod) 
      posterior.mode(h2_liab_Otipulae_CONTROL_P5p_wt_mod)	
      median(h2_liab_Otipulae_CONTROL_P5p_wt_mod)		
      HPDinterval(h2_liab_Otipulae_CONTROL_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_CONTROL_P5p_wt_mod <- ((rowMeans(Otipulae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_CONTROL_P5p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Otipulae_CONTROL_P5p_wt_mod <- (va_liab_Otipulae_CONTROL_P5p_wt_mod/2) / (trait_mean_liab_Otipulae_CONTROL_P5p_wt_mod)^2
    mean(Evol_liab_Otipulae_CONTROL_P5p_wt_mod)
    
    
    #Otipulae_CONTROL_P5p_wt_mod data scale
    {
      
      predict_Otipulae_CONTROL_P5p_wt_mod <- map(1:nrow(Otipulae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_CONTROL_P5p_wt_mod %*% Otipulae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_CONTROL_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_Otipulae_CONTROL_P5p_wt_mod,
                      var.a = Otipulae_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_CONTROL_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_CONTROL_P5p_wt_mod <- data_Otipulae_CONTROL_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_Otipulae_CONTROL_P5p_wt_mod <- data_Otipulae_CONTROL_P5p_wt_mod[["mean.obs"]]
      va_data_Otipulae_CONTROL_P5p_wt_mod <- data_Otipulae_CONTROL_P5p_wt_mod[["var.a.obs"]]
      vp_data_Otipulae_CONTROL_P5p_wt_mod <- data_Otipulae_CONTROL_P5p_wt_mod[["var.obs"]]
      
      Evol_data_Otipulae_CONTROL_P5p_wt_mod <- (va_data_Otipulae_CONTROL_P5p_wt_mod/2) / (trait_mean_data_Otipulae_CONTROL_P5p_wt_mod)^2
      
      mean(h2_data_Otipulae_CONTROL_P5p_wt_mod) 
      mean(trait_mean_data_Otipulae_CONTROL_P5p_wt_mod)
      mean(va_data_Otipulae_CONTROL_P5p_wt_mod) 
      mean(vp_data_Otipulae_CONTROL_P5p_wt_mod)
      mean(Evol_data_Otipulae_CONTROL_P5p_wt_mod)
      
    }
    
  }
  
  #Otipulae_MA_P5p_wt_mod 
  {
    
    Otipulae_MA_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_Otipulae_bi_MA,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)         
    
    saveRDS(Otipulae_MA_P5p_wt_mod, file = "Otipulae_MA_P5p_wt_mod.rds")
    Otipulae_MA_P5p_wt_mod <- readRDS("Otipulae_MA_P5p_wt_mod.rds")
    
    summary(Otipulae_MA_P5p_wt_mod) 
    #plot(Otipulae_MA_P5p_wt_mod)
    
    # traces and posterior densities
    pdf("Otipulae_MA_P5p_wt_mod.pdf")
    plot(Otipulae_MA_P5p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Otipulae_MA_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_MA_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_MA_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_MA_P5p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_MA_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_MA_P5p_wt_mod[["VCV"]])
    #autocorr.plot(Otipulae_MA_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_MA_P5p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_MA_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_MA_P5p_wt_mod <- Otipulae_MA_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_MA_P5p_wt_mod <- Otipulae_MA_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_Otipulae_MA_P5p_wt_mod <- rowSums(Otipulae_MA_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_Otipulae_MA_P5p_wt_mod) 
      HPDinterval(va_liab_Otipulae_MA_P5p_wt_mod) 
      
      mean(vlat_Otipulae_MA_P5p_wt_mod) 
      
      #variance of fixed effects
      X_Otipulae_MA_P5p_wt_mod <- Otipulae_MA_P5p_wt_mod[["X"]]
      beta_Otipulae_MA_P5p_wt_mod <- Otipulae_MA_P5p_wt_mod[["Sol"]][,c(1:5)]
      vf_Otipulae_MA_P5p_wt_mod   <- apply(beta_Otipulae_MA_P5p_wt_mod, 1, function(b) {var(as.vector(X_Otipulae_MA_P5p_wt_mod %*% b))}) 
      mean(vf_Otipulae_MA_P5p_wt_mod) 
      
      h2_liab_Otipulae_MA_P5p_wt_mod <- va_liab_Otipulae_MA_P5p_wt_mod / (vlat_Otipulae_MA_P5p_wt_mod + vf_Otipulae_MA_P5p_wt_mod)
      mean(h2_liab_Otipulae_MA_P5p_wt_mod) 
      posterior.mode(h2_liab_Otipulae_MA_P5p_wt_mod)	
      median(h2_liab_Otipulae_MA_P5p_wt_mod)		
      HPDinterval(h2_liab_Otipulae_MA_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_MA_P5p_wt_mod <- ((rowMeans(Otipulae_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_MA_P5p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Otipulae_MA_P5p_wt_mod <- (va_liab_Otipulae_MA_P5p_wt_mod/2) / (trait_mean_liab_Otipulae_MA_P5p_wt_mod)^2
    mean(Evol_liab_Otipulae_MA_P5p_wt_mod)
    
    #Otipulae_MA_P5p_wt_mod data scale
    {
      
      predict_Otipulae_MA_P5p_wt_mod <- map(1:nrow(Otipulae_MA_P5p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_MA_P5p_wt_mod %*% Otipulae_MA_P5p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_MA_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_Otipulae_MA_P5p_wt_mod,
                      var.a = Otipulae_MA_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_MA_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_MA_P5p_wt_mod <- data_Otipulae_MA_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_Otipulae_MA_P5p_wt_mod <- data_Otipulae_MA_P5p_wt_mod[["mean.obs"]]
      va_data_Otipulae_MA_P5p_wt_mod <- data_Otipulae_MA_P5p_wt_mod[["var.a.obs"]]
      vp_data_Otipulae_MA_P5p_wt_mod <- data_Otipulae_MA_P5p_wt_mod[["var.obs"]]
      
      Evol_data_Otipulae_MA_P5p_wt_mod <- (va_data_Otipulae_MA_P5p_wt_mod/2) / (trait_mean_data_Otipulae_MA_P5p_wt_mod)^2
      
      mean(h2_data_Otipulae_MA_P5p_wt_mod)
      mean(trait_mean_data_Otipulae_MA_P5p_wt_mod)
      mean(va_data_Otipulae_MA_P5p_wt_mod)
      mean(vp_data_Otipulae_MA_P5p_wt_mod)
      mean(Evol_data_Otipulae_MA_P5p_wt_mod)
      
    }
    
  }
  
  #Otipulae_Vm_P5p_wt_mod 
  {
    
    Otipulae_Vm_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Treatment:Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_Otipulae_data,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)            
    
    saveRDS(Otipulae_Vm_P5p_wt_mod, file = "Otipulae_Vm_P5p_wt_mod.rds")
    Otipulae_Vm_P5p_wt_mod <- readRDS("Otipulae_Vm_P5p_wt_mod.rds")
    
    summary(Otipulae_Vm_P5p_wt_mod) 
    plotTrace(Otipulae_Vm_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("Otipulae_Vm_P5p_wt_mod.pdf")
    plot(Otipulae_Vm_P5p_wt_mod[["Sol"]][,c(1:7)]) 
    plot(Otipulae_Vm_P5p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_Vm_P5p_wt_mod[["Sol"]][,c(1:7)]) #
    heidel.diag(Otipulae_Vm_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_Vm_P5p_wt_mod[["Sol"]][,c(1:7)])
    #autocorr.plot(Otipulae_Vm_P5p_wt_mod[["Sol"]][,c(1:7)]) #
    autocorr.diag(Otipulae_Vm_P5p_wt_mod[["VCV"]])
    #autocorr.plot(Otipulae_Vm_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_Vm_P5p_wt_mod[["Sol"]][,c(1:7)]) # 
    effectiveSize(Otipulae_Vm_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_Vm_P5p_wt_mod <- Otipulae_Vm_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_Vm_P5p_wt_mod <- Otipulae_Vm_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_Otipulae_Vm_P5p_wt_mod <- rowSums(Otipulae_Vm_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_Otipulae_Vm_P5p_wt_mod) 
      HPDinterval(va_liab_Otipulae_Vm_P5p_wt_mod) 
      
      mean(vlat_Otipulae_Vm_P5p_wt_mod) 
      
      #variance of fixed effects
      X_Otipulae_Vm_P5p_wt_mod <- Otipulae_Vm_P5p_wt_mod[["X"]]
      beta_Otipulae_Vm_P5p_wt_mod <- Otipulae_Vm_P5p_wt_mod[["Sol"]][,c(1:7)]
      vf_Otipulae_Vm_P5p_wt_mod   <- apply(beta_Otipulae_Vm_P5p_wt_mod, 1, function(b) {var(as.vector(X_Otipulae_Vm_P5p_wt_mod %*% b))}) 
      mean(vf_Otipulae_Vm_P5p_wt_mod) 
      
      h2_liab_Otipulae_Vm_P5p_wt_mod <- va_liab_Otipulae_Vm_P5p_wt_mod / (vlat_Otipulae_Vm_P5p_wt_mod + vf_Otipulae_Vm_P5p_wt_mod)
      mean(h2_liab_Otipulae_Vm_P5p_wt_mod) 
      posterior.mode(h2_liab_Otipulae_Vm_P5p_wt_mod)	
      median(h2_liab_Otipulae_Vm_P5p_wt_mod)		
      HPDinterval(h2_liab_Otipulae_Vm_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    mean(rowMeans(Otipulae_Vm_P5p_wt_mod[["Sol"]][,c(1:4)])) 
    mean(rowMeans(Otipulae_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P5p_wt_mod[["Sol"]][,5]) 
    mean(rowMeans(Otipulae_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P5p_wt_mod[["Sol"]][,6]) 
    mean(rowMeans(Otipulae_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P5p_wt_mod[["Sol"]][,7]) 
    
    trait_mean_liab_Otipulae_Vm_P5p_wt_mod <- ((rowMeans(Otipulae_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P5p_wt_mod[["Sol"]][,5]) + (rowMeans(Otipulae_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P5p_wt_mod[["Sol"]][,6]) + (rowMeans(Otipulae_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P5p_wt_mod[["Sol"]][,7]))/4)
    
    Evol_liab_Otipulae_Vm_P5p_wt_mod <- (va_liab_Otipulae_Vm_P5p_wt_mod/2) / (trait_mean_liab_Otipulae_Vm_P5p_wt_mod)^2
    mean(Evol_liab_Otipulae_Vm_P5p_wt_mod)
    
    #Otipulae_Vm_P5p_wt_mod data scale
    {
      
      predict_Otipulae_Vm_P5p_wt_mod <- map(1:nrow(Otipulae_Vm_P5p_wt_mod[["Sol"]][,c(1:7)]), ~ as.vector(X_Otipulae_Vm_P5p_wt_mod %*% Otipulae_Vm_P5p_wt_mod[["Sol"]][,c(1:7)][., ]))
      
      data_Otipulae_Vm_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_Otipulae_Vm_P5p_wt_mod,
                      var.a = Otipulae_Vm_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_Vm_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_Vm_P5p_wt_mod <- data_Otipulae_Vm_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_Otipulae_Vm_P5p_wt_mod <- data_Otipulae_Vm_P5p_wt_mod[["mean.obs"]]
      va_data_Otipulae_Vm_P5p_wt_mod <- data_Otipulae_Vm_P5p_wt_mod[["var.a.obs"]]
      vp_data_Otipulae_Vm_P5p_wt_mod <- data_Otipulae_Vm_P5p_wt_mod[["var.obs"]]
      
      Evol_data_Otipulae_Vm_P5p_wt_mod <- (va_data_Otipulae_Vm_P5p_wt_mod/2) / (trait_mean_data_Otipulae_Vm_P5p_wt_mod)^2
      
      mean(h2_data_Otipulae_Vm_P5p_wt_mod)
      mean(trait_mean_data_Otipulae_Vm_P5p_wt_mod)
      mean(va_data_Otipulae_Vm_P5p_wt_mod)
      mean(vp_data_Otipulae_Vm_P5p_wt_mod)
      mean(Evol_data_Otipulae_Vm_P5p_wt_mod)
      
    }
    
  }
  
  
  
  
  
  ## Otipulae P6p ----
  
  
  #Otipulae_CONTROL_P6p_wt_mod 
  {
    
    Otipulae_CONTROL_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Ancestral -1,
                                            random      = ~ LineB + BlockRep,
                                            family      = "threshold",
                                            data        = Vm_Otipulae_bi_CONTROL,
                                            prior       = prior_bi_block,
                                            nitt        = 1260000,       
                                            thin        = 500,           
                                            burnin      = 10000,
                                            trunc       = TRUE,
                                            pr          = TRUE,
                                            pl          = TRUE)            
    
    saveRDS(Otipulae_CONTROL_P6p_wt_mod, file = "Otipulae_CONTROL_P6p_wt_mod.rds")
    Otipulae_CONTROL_P6p_wt_mod <- readRDS("Otipulae_CONTROL_P6p_wt_mod.rds")
    
    summary(Otipulae_CONTROL_P6p_wt_mod) 
    plotTrace(Otipulae_CONTROL_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Otipulae_CONTROL_P6p_wt_mod.pdf")
    plot(Otipulae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Otipulae_CONTROL_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_CONTROL_P6p_wt_mod[["VCV"]])
    #autocorr.plot(Otipulae_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_CONTROL_P6p_wt_mod <- Otipulae_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_CONTROL_P6p_wt_mod <- Otipulae_CONTROL_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_Otipulae_CONTROL_P6p_wt_mod <- rowSums(Otipulae_CONTROL_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_Otipulae_CONTROL_P6p_wt_mod) 
      HPDinterval(va_liab_Otipulae_CONTROL_P6p_wt_mod) 
      
      mean(vlat_Otipulae_CONTROL_P6p_wt_mod) 
      
      #variance of fixed effects
      X_Otipulae_CONTROL_P6p_wt_mod <- Otipulae_CONTROL_P6p_wt_mod[["X"]]
      beta_Otipulae_CONTROL_P6p_wt_mod <- Otipulae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]
      vf_Otipulae_CONTROL_P6p_wt_mod   <- apply(beta_Otipulae_CONTROL_P6p_wt_mod, 1, function(b) {var(as.vector(X_Otipulae_CONTROL_P6p_wt_mod %*% b))}) 
      mean(vf_Otipulae_CONTROL_P6p_wt_mod) 
      
      h2_liab_Otipulae_CONTROL_P6p_wt_mod <- va_liab_Otipulae_CONTROL_P6p_wt_mod / (vlat_Otipulae_CONTROL_P6p_wt_mod + vf_Otipulae_CONTROL_P6p_wt_mod)
      mean(h2_liab_Otipulae_CONTROL_P6p_wt_mod) 
      posterior.mode(h2_liab_Otipulae_CONTROL_P6p_wt_mod)	
      median(h2_liab_Otipulae_CONTROL_P6p_wt_mod)		
      HPDinterval(h2_liab_Otipulae_CONTROL_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_CONTROL_P6p_wt_mod <- ((rowMeans(Otipulae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_CONTROL_P6p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Otipulae_CONTROL_P6p_wt_mod <- (va_liab_Otipulae_CONTROL_P6p_wt_mod/2) / (trait_mean_liab_Otipulae_CONTROL_P6p_wt_mod)^2
    mean(Evol_liab_Otipulae_CONTROL_P6p_wt_mod)
    
    
    #Otipulae_CONTROL_P6p_wt_mod data scale
    {
      
      predict_Otipulae_CONTROL_P6p_wt_mod <- map(1:nrow(Otipulae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_CONTROL_P6p_wt_mod %*% Otipulae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_CONTROL_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_Otipulae_CONTROL_P6p_wt_mod,
                      var.a = Otipulae_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_CONTROL_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_CONTROL_P6p_wt_mod <- data_Otipulae_CONTROL_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_Otipulae_CONTROL_P6p_wt_mod <- data_Otipulae_CONTROL_P6p_wt_mod[["mean.obs"]]
      va_data_Otipulae_CONTROL_P6p_wt_mod <- data_Otipulae_CONTROL_P6p_wt_mod[["var.a.obs"]]
      vp_data_Otipulae_CONTROL_P6p_wt_mod <- data_Otipulae_CONTROL_P6p_wt_mod[["var.obs"]]
      
      Evol_data_Otipulae_CONTROL_P6p_wt_mod <- (va_data_Otipulae_CONTROL_P6p_wt_mod/2) / (trait_mean_data_Otipulae_CONTROL_P6p_wt_mod)^2
      
      mean(h2_data_Otipulae_CONTROL_P6p_wt_mod) 
      mean(trait_mean_data_Otipulae_CONTROL_P6p_wt_mod)
      mean(va_data_Otipulae_CONTROL_P6p_wt_mod) 
      mean(vp_data_Otipulae_CONTROL_P6p_wt_mod)
      mean(Evol_data_Otipulae_CONTROL_P6p_wt_mod)
      
    }
    
  }
  
  #Otipulae_MA_P6p_wt_mod 
  {
    
    Otipulae_MA_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_Otipulae_bi_MA,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)         
    
    saveRDS(Otipulae_MA_P6p_wt_mod, file = "Otipulae_MA_P6p_wt_mod.rds")
    Otipulae_MA_P6p_wt_mod <- readRDS("Otipulae_MA_P6p_wt_mod.rds")
    
    summary(Otipulae_MA_P6p_wt_mod) 
    #plot(Otipulae_MA_P6p_wt_mod)
    
    # traces and posterior densities
    pdf("Otipulae_MA_P6p_wt_mod.pdf")
    plot(Otipulae_MA_P6p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Otipulae_MA_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_MA_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_MA_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_MA_P6p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_MA_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_MA_P6p_wt_mod[["VCV"]])
    #autocorr.plot(Otipulae_MA_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_MA_P6p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_MA_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_MA_P6p_wt_mod <- Otipulae_MA_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_MA_P6p_wt_mod <- Otipulae_MA_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_Otipulae_MA_P6p_wt_mod <- rowSums(Otipulae_MA_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_Otipulae_MA_P6p_wt_mod) 
      HPDinterval(va_liab_Otipulae_MA_P6p_wt_mod) 
      
      mean(vlat_Otipulae_MA_P6p_wt_mod) 
      
      #variance of fixed effects
      X_Otipulae_MA_P6p_wt_mod <- Otipulae_MA_P6p_wt_mod[["X"]]
      beta_Otipulae_MA_P6p_wt_mod <- Otipulae_MA_P6p_wt_mod[["Sol"]][,c(1:5)]
      vf_Otipulae_MA_P6p_wt_mod   <- apply(beta_Otipulae_MA_P6p_wt_mod, 1, function(b) {var(as.vector(X_Otipulae_MA_P6p_wt_mod %*% b))}) 
      mean(vf_Otipulae_MA_P6p_wt_mod) 
      
      h2_liab_Otipulae_MA_P6p_wt_mod <- va_liab_Otipulae_MA_P6p_wt_mod / (vlat_Otipulae_MA_P6p_wt_mod + vf_Otipulae_MA_P6p_wt_mod)
      mean(h2_liab_Otipulae_MA_P6p_wt_mod) 
      posterior.mode(h2_liab_Otipulae_MA_P6p_wt_mod)	
      median(h2_liab_Otipulae_MA_P6p_wt_mod)		
      HPDinterval(h2_liab_Otipulae_MA_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_MA_P6p_wt_mod <- ((rowMeans(Otipulae_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_MA_P6p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Otipulae_MA_P6p_wt_mod <- (va_liab_Otipulae_MA_P6p_wt_mod/2) / (trait_mean_liab_Otipulae_MA_P6p_wt_mod)^2
    mean(Evol_liab_Otipulae_MA_P6p_wt_mod)
    
    #Otipulae_MA_P6p_wt_mod data scale
    {
      
      predict_Otipulae_MA_P6p_wt_mod <- map(1:nrow(Otipulae_MA_P6p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_MA_P6p_wt_mod %*% Otipulae_MA_P6p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_MA_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_Otipulae_MA_P6p_wt_mod,
                      var.a = Otipulae_MA_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_MA_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_MA_P6p_wt_mod <- data_Otipulae_MA_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_Otipulae_MA_P6p_wt_mod <- data_Otipulae_MA_P6p_wt_mod[["mean.obs"]]
      va_data_Otipulae_MA_P6p_wt_mod <- data_Otipulae_MA_P6p_wt_mod[["var.a.obs"]]
      vp_data_Otipulae_MA_P6p_wt_mod <- data_Otipulae_MA_P6p_wt_mod[["var.obs"]]
      
      Evol_data_Otipulae_MA_P6p_wt_mod <- (va_data_Otipulae_MA_P6p_wt_mod/2) / (trait_mean_data_Otipulae_MA_P6p_wt_mod)^2
      
      mean(h2_data_Otipulae_MA_P6p_wt_mod)
      mean(trait_mean_data_Otipulae_MA_P6p_wt_mod)
      mean(va_data_Otipulae_MA_P6p_wt_mod)
      mean(vp_data_Otipulae_MA_P6p_wt_mod)
      mean(Evol_data_Otipulae_MA_P6p_wt_mod)
      
    }
    
  }
  
  #Otipulae_Vm_P6p_wt_mod 
  {
    
    Otipulae_Vm_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Treatment:Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_Otipulae_data,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)            
    
    saveRDS(Otipulae_Vm_P6p_wt_mod, file = "Otipulae_Vm_P6p_wt_mod.rds")
    Otipulae_Vm_P6p_wt_mod <- readRDS("Otipulae_Vm_P6p_wt_mod.rds")
    
    summary(Otipulae_Vm_P6p_wt_mod) 
    plotTrace(Otipulae_Vm_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("Otipulae_Vm_P6p_wt_mod.pdf")
    plot(Otipulae_Vm_P6p_wt_mod[["Sol"]][,c(1:7)]) 
    plot(Otipulae_Vm_P6p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_Vm_P6p_wt_mod[["Sol"]][,c(1:7)]) #
    heidel.diag(Otipulae_Vm_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_Vm_P6p_wt_mod[["Sol"]][,c(1:7)])
    #autocorr.plot(Otipulae_Vm_P6p_wt_mod[["Sol"]][,c(1:7)]) #
    autocorr.diag(Otipulae_Vm_P6p_wt_mod[["VCV"]])
    #autocorr.plot(Otipulae_Vm_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_Vm_P6p_wt_mod[["Sol"]][,c(1:7)]) # 
    effectiveSize(Otipulae_Vm_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_Vm_P6p_wt_mod <- Otipulae_Vm_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_Vm_P6p_wt_mod <- Otipulae_Vm_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_Otipulae_Vm_P6p_wt_mod <- rowSums(Otipulae_Vm_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_Otipulae_Vm_P6p_wt_mod) 
      HPDinterval(va_liab_Otipulae_Vm_P6p_wt_mod) 
      
      mean(vlat_Otipulae_Vm_P6p_wt_mod) 
      
      #variance of fixed effects
      X_Otipulae_Vm_P6p_wt_mod <- Otipulae_Vm_P6p_wt_mod[["X"]]
      beta_Otipulae_Vm_P6p_wt_mod <- Otipulae_Vm_P6p_wt_mod[["Sol"]][,c(1:7)]
      vf_Otipulae_Vm_P6p_wt_mod   <- apply(beta_Otipulae_Vm_P6p_wt_mod, 1, function(b) {var(as.vector(X_Otipulae_Vm_P6p_wt_mod %*% b))}) 
      mean(vf_Otipulae_Vm_P6p_wt_mod) 
      
      h2_liab_Otipulae_Vm_P6p_wt_mod <- va_liab_Otipulae_Vm_P6p_wt_mod / (vlat_Otipulae_Vm_P6p_wt_mod + vf_Otipulae_Vm_P6p_wt_mod)
      mean(h2_liab_Otipulae_Vm_P6p_wt_mod) 
      posterior.mode(h2_liab_Otipulae_Vm_P6p_wt_mod)	
      median(h2_liab_Otipulae_Vm_P6p_wt_mod)		
      HPDinterval(h2_liab_Otipulae_Vm_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    mean(rowMeans(Otipulae_Vm_P6p_wt_mod[["Sol"]][,c(1:4)])) 
    mean(rowMeans(Otipulae_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P6p_wt_mod[["Sol"]][,5]) 
    mean(rowMeans(Otipulae_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P6p_wt_mod[["Sol"]][,6]) 
    mean(rowMeans(Otipulae_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P6p_wt_mod[["Sol"]][,7]) 
    
    trait_mean_liab_Otipulae_Vm_P6p_wt_mod <- ((rowMeans(Otipulae_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P6p_wt_mod[["Sol"]][,5]) + (rowMeans(Otipulae_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P6p_wt_mod[["Sol"]][,6]) + (rowMeans(Otipulae_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P6p_wt_mod[["Sol"]][,7]))/4)
    
    Evol_liab_Otipulae_Vm_P6p_wt_mod <- (va_liab_Otipulae_Vm_P6p_wt_mod/2) / (trait_mean_liab_Otipulae_Vm_P6p_wt_mod)^2
    mean(Evol_liab_Otipulae_Vm_P6p_wt_mod)
    
    #Otipulae_Vm_P6p_wt_mod data scale
    {
      
      predict_Otipulae_Vm_P6p_wt_mod <- map(1:nrow(Otipulae_Vm_P6p_wt_mod[["Sol"]][,c(1:7)]), ~ as.vector(X_Otipulae_Vm_P6p_wt_mod %*% Otipulae_Vm_P6p_wt_mod[["Sol"]][,c(1:7)][., ]))
      
      data_Otipulae_Vm_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_Otipulae_Vm_P6p_wt_mod,
                      var.a = Otipulae_Vm_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_Vm_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_Vm_P6p_wt_mod <- data_Otipulae_Vm_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_Otipulae_Vm_P6p_wt_mod <- data_Otipulae_Vm_P6p_wt_mod[["mean.obs"]]
      va_data_Otipulae_Vm_P6p_wt_mod <- data_Otipulae_Vm_P6p_wt_mod[["var.a.obs"]]
      vp_data_Otipulae_Vm_P6p_wt_mod <- data_Otipulae_Vm_P6p_wt_mod[["var.obs"]]
      
      Evol_data_Otipulae_Vm_P6p_wt_mod <- (va_data_Otipulae_Vm_P6p_wt_mod/2) / (trait_mean_data_Otipulae_Vm_P6p_wt_mod)^2
      
      mean(h2_data_Otipulae_Vm_P6p_wt_mod)
      mean(trait_mean_data_Otipulae_Vm_P6p_wt_mod)
      mean(va_data_Otipulae_Vm_P6p_wt_mod)
      mean(vp_data_Otipulae_Vm_P6p_wt_mod)
      mean(Evol_data_Otipulae_Vm_P6p_wt_mod)
      
    }
    
  }
  
  
 
  
  ## Otipulae P7p ----
  
  #Otipulae_CONTROL_P7p_wt_mod 
  {
    
    Otipulae_CONTROL_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Ancestral -1,
                                            random      = ~ LineB + BlockRep,
                                            family      = "threshold",
                                            data        = Vm_Otipulae_bi_CONTROL,
                                            prior       = prior_bi_block,
                                            nitt        = 1260000,       
                                            thin        = 500,           
                                            burnin      = 10000,
                                            trunc       = TRUE,
                                            pr          = TRUE,
                                            pl          = TRUE)            
    
    saveRDS(Otipulae_CONTROL_P7p_wt_mod, file = "Otipulae_CONTROL_P7p_wt_mod.rds")
    Otipulae_CONTROL_P7p_wt_mod <- readRDS("Otipulae_CONTROL_P7p_wt_mod.rds")
    
    summary(Otipulae_CONTROL_P7p_wt_mod) 
    plotTrace(Otipulae_CONTROL_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Otipulae_CONTROL_P7p_wt_mod.pdf")
    plot(Otipulae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Otipulae_CONTROL_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_CONTROL_P7p_wt_mod[["VCV"]])
    #autocorr.plot(Otipulae_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_CONTROL_P7p_wt_mod <- Otipulae_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_CONTROL_P7p_wt_mod <- Otipulae_CONTROL_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_Otipulae_CONTROL_P7p_wt_mod <- rowSums(Otipulae_CONTROL_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_Otipulae_CONTROL_P7p_wt_mod) 
      HPDinterval(va_liab_Otipulae_CONTROL_P7p_wt_mod) 
      
      mean(vlat_Otipulae_CONTROL_P7p_wt_mod) 
      
      #variance of fixed effects
      X_Otipulae_CONTROL_P7p_wt_mod <- Otipulae_CONTROL_P7p_wt_mod[["X"]]
      beta_Otipulae_CONTROL_P7p_wt_mod <- Otipulae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]
      vf_Otipulae_CONTROL_P7p_wt_mod   <- apply(beta_Otipulae_CONTROL_P7p_wt_mod, 1, function(b) {var(as.vector(X_Otipulae_CONTROL_P7p_wt_mod %*% b))}) 
      mean(vf_Otipulae_CONTROL_P7p_wt_mod) 
      
      h2_liab_Otipulae_CONTROL_P7p_wt_mod <- va_liab_Otipulae_CONTROL_P7p_wt_mod / (vlat_Otipulae_CONTROL_P7p_wt_mod + vf_Otipulae_CONTROL_P7p_wt_mod)
      mean(h2_liab_Otipulae_CONTROL_P7p_wt_mod) 
      posterior.mode(h2_liab_Otipulae_CONTROL_P7p_wt_mod)	
      median(h2_liab_Otipulae_CONTROL_P7p_wt_mod)		
      HPDinterval(h2_liab_Otipulae_CONTROL_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_CONTROL_P7p_wt_mod <- ((rowMeans(Otipulae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_CONTROL_P7p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Otipulae_CONTROL_P7p_wt_mod <- (va_liab_Otipulae_CONTROL_P7p_wt_mod/2) / (trait_mean_liab_Otipulae_CONTROL_P7p_wt_mod)^2
    mean(Evol_liab_Otipulae_CONTROL_P7p_wt_mod)
    
    
    #Otipulae_CONTROL_P7p_wt_mod data scale
    {
      
      predict_Otipulae_CONTROL_P7p_wt_mod <- map(1:nrow(Otipulae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_CONTROL_P7p_wt_mod %*% Otipulae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_CONTROL_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_Otipulae_CONTROL_P7p_wt_mod,
                      var.a = Otipulae_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_CONTROL_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_CONTROL_P7p_wt_mod <- data_Otipulae_CONTROL_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_Otipulae_CONTROL_P7p_wt_mod <- data_Otipulae_CONTROL_P7p_wt_mod[["mean.obs"]]
      va_data_Otipulae_CONTROL_P7p_wt_mod <- data_Otipulae_CONTROL_P7p_wt_mod[["var.a.obs"]]
      vp_data_Otipulae_CONTROL_P7p_wt_mod <- data_Otipulae_CONTROL_P7p_wt_mod[["var.obs"]]
      
      Evol_data_Otipulae_CONTROL_P7p_wt_mod <- (va_data_Otipulae_CONTROL_P7p_wt_mod/2) / (trait_mean_data_Otipulae_CONTROL_P7p_wt_mod)^2
      
      mean(h2_data_Otipulae_CONTROL_P7p_wt_mod) 
      mean(trait_mean_data_Otipulae_CONTROL_P7p_wt_mod)
      mean(va_data_Otipulae_CONTROL_P7p_wt_mod) 
      mean(vp_data_Otipulae_CONTROL_P7p_wt_mod)
      mean(Evol_data_Otipulae_CONTROL_P7p_wt_mod)
      
    }
    
  }
  
  #Otipulae_MA_P7p_wt_mod 
  {
    
    Otipulae_MA_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_Otipulae_bi_MA,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)         
    
    saveRDS(Otipulae_MA_P7p_wt_mod, file = "Otipulae_MA_P7p_wt_mod.rds")
    Otipulae_MA_P7p_wt_mod <- readRDS("Otipulae_MA_P7p_wt_mod.rds")
    
    summary(Otipulae_MA_P7p_wt_mod) 
    #plot(Otipulae_MA_P7p_wt_mod)
    
    # traces and posterior densities
    pdf("Otipulae_MA_P7p_wt_mod.pdf")
    plot(Otipulae_MA_P7p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Otipulae_MA_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_MA_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_MA_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_MA_P7p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_MA_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_MA_P7p_wt_mod[["VCV"]])
    #autocorr.plot(Otipulae_MA_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_MA_P7p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_MA_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_MA_P7p_wt_mod <- Otipulae_MA_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_MA_P7p_wt_mod <- Otipulae_MA_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_Otipulae_MA_P7p_wt_mod <- rowSums(Otipulae_MA_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_Otipulae_MA_P7p_wt_mod) 
      HPDinterval(va_liab_Otipulae_MA_P7p_wt_mod) 
      
      mean(vlat_Otipulae_MA_P7p_wt_mod) 
      
      #variance of fixed effects
      X_Otipulae_MA_P7p_wt_mod <- Otipulae_MA_P7p_wt_mod[["X"]]
      beta_Otipulae_MA_P7p_wt_mod <- Otipulae_MA_P7p_wt_mod[["Sol"]][,c(1:5)]
      vf_Otipulae_MA_P7p_wt_mod   <- apply(beta_Otipulae_MA_P7p_wt_mod, 1, function(b) {var(as.vector(X_Otipulae_MA_P7p_wt_mod %*% b))}) 
      mean(vf_Otipulae_MA_P7p_wt_mod) 
      
      h2_liab_Otipulae_MA_P7p_wt_mod <- va_liab_Otipulae_MA_P7p_wt_mod / (vlat_Otipulae_MA_P7p_wt_mod + vf_Otipulae_MA_P7p_wt_mod)
      mean(h2_liab_Otipulae_MA_P7p_wt_mod) 
      posterior.mode(h2_liab_Otipulae_MA_P7p_wt_mod)	
      median(h2_liab_Otipulae_MA_P7p_wt_mod)		
      HPDinterval(h2_liab_Otipulae_MA_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_MA_P7p_wt_mod <- ((rowMeans(Otipulae_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_MA_P7p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Otipulae_MA_P7p_wt_mod <- (va_liab_Otipulae_MA_P7p_wt_mod/2) / (trait_mean_liab_Otipulae_MA_P7p_wt_mod)^2
    mean(Evol_liab_Otipulae_MA_P7p_wt_mod)
    
    #Otipulae_MA_P7p_wt_mod data scale
    {
      
      predict_Otipulae_MA_P7p_wt_mod <- map(1:nrow(Otipulae_MA_P7p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_MA_P7p_wt_mod %*% Otipulae_MA_P7p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_MA_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_Otipulae_MA_P7p_wt_mod,
                      var.a = Otipulae_MA_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_MA_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_MA_P7p_wt_mod <- data_Otipulae_MA_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_Otipulae_MA_P7p_wt_mod <- data_Otipulae_MA_P7p_wt_mod[["mean.obs"]]
      va_data_Otipulae_MA_P7p_wt_mod <- data_Otipulae_MA_P7p_wt_mod[["var.a.obs"]]
      vp_data_Otipulae_MA_P7p_wt_mod <- data_Otipulae_MA_P7p_wt_mod[["var.obs"]]
      
      Evol_data_Otipulae_MA_P7p_wt_mod <- (va_data_Otipulae_MA_P7p_wt_mod/2) / (trait_mean_data_Otipulae_MA_P7p_wt_mod)^2
      
      mean(h2_data_Otipulae_MA_P7p_wt_mod)
      mean(trait_mean_data_Otipulae_MA_P7p_wt_mod)
      mean(va_data_Otipulae_MA_P7p_wt_mod)
      mean(vp_data_Otipulae_MA_P7p_wt_mod)
      mean(Evol_data_Otipulae_MA_P7p_wt_mod)
      
    }
    
  }
  
  #Otipulae_Vm_P7p_wt_mod 
  {
    
    Otipulae_Vm_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Treatment:Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_Otipulae_data,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)            
    
    saveRDS(Otipulae_Vm_P7p_wt_mod, file = "Otipulae_Vm_P7p_wt_mod.rds")
    Otipulae_Vm_P7p_wt_mod <- readRDS("Otipulae_Vm_P7p_wt_mod.rds")
    
    summary(Otipulae_Vm_P7p_wt_mod) 
    plotTrace(Otipulae_Vm_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("Otipulae_Vm_P7p_wt_mod.pdf")
    plot(Otipulae_Vm_P7p_wt_mod[["Sol"]][,c(1:7)]) 
    plot(Otipulae_Vm_P7p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_Vm_P7p_wt_mod[["Sol"]][,c(1:7)]) #
    heidel.diag(Otipulae_Vm_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_Vm_P7p_wt_mod[["Sol"]][,c(1:7)])
    #autocorr.plot(Otipulae_Vm_P7p_wt_mod[["Sol"]][,c(1:7)]) #
    autocorr.diag(Otipulae_Vm_P7p_wt_mod[["VCV"]])
    #autocorr.plot(Otipulae_Vm_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_Vm_P7p_wt_mod[["Sol"]][,c(1:7)]) # 
    effectiveSize(Otipulae_Vm_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_Vm_P7p_wt_mod <- Otipulae_Vm_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_Vm_P7p_wt_mod <- Otipulae_Vm_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_Otipulae_Vm_P7p_wt_mod <- rowSums(Otipulae_Vm_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_Otipulae_Vm_P7p_wt_mod) 
      HPDinterval(va_liab_Otipulae_Vm_P7p_wt_mod) 
      
      mean(vlat_Otipulae_Vm_P7p_wt_mod) 
      
      #variance of fixed effects
      X_Otipulae_Vm_P7p_wt_mod <- Otipulae_Vm_P7p_wt_mod[["X"]]
      beta_Otipulae_Vm_P7p_wt_mod <- Otipulae_Vm_P7p_wt_mod[["Sol"]][,c(1:7)]
      vf_Otipulae_Vm_P7p_wt_mod   <- apply(beta_Otipulae_Vm_P7p_wt_mod, 1, function(b) {var(as.vector(X_Otipulae_Vm_P7p_wt_mod %*% b))}) 
      mean(vf_Otipulae_Vm_P7p_wt_mod) 
      
      h2_liab_Otipulae_Vm_P7p_wt_mod <- va_liab_Otipulae_Vm_P7p_wt_mod / (vlat_Otipulae_Vm_P7p_wt_mod + vf_Otipulae_Vm_P7p_wt_mod)
      mean(h2_liab_Otipulae_Vm_P7p_wt_mod) 
      posterior.mode(h2_liab_Otipulae_Vm_P7p_wt_mod)	
      median(h2_liab_Otipulae_Vm_P7p_wt_mod)		
      HPDinterval(h2_liab_Otipulae_Vm_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    mean(rowMeans(Otipulae_Vm_P7p_wt_mod[["Sol"]][,c(1:4)])) 
    mean(rowMeans(Otipulae_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P7p_wt_mod[["Sol"]][,5]) 
    mean(rowMeans(Otipulae_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P7p_wt_mod[["Sol"]][,6]) 
    mean(rowMeans(Otipulae_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P7p_wt_mod[["Sol"]][,7]) 
    
    trait_mean_liab_Otipulae_Vm_P7p_wt_mod <- ((rowMeans(Otipulae_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Otipulae_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P7p_wt_mod[["Sol"]][,5]) + (rowMeans(Otipulae_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P7p_wt_mod[["Sol"]][,6]) + (rowMeans(Otipulae_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + Otipulae_Vm_P7p_wt_mod[["Sol"]][,7]))/4)
    
    Evol_liab_Otipulae_Vm_P7p_wt_mod <- (va_liab_Otipulae_Vm_P7p_wt_mod/2) / (trait_mean_liab_Otipulae_Vm_P7p_wt_mod)^2
    mean(Evol_liab_Otipulae_Vm_P7p_wt_mod)
    
    #Otipulae_Vm_P7p_wt_mod data scale
    {
      
      predict_Otipulae_Vm_P7p_wt_mod <- map(1:nrow(Otipulae_Vm_P7p_wt_mod[["Sol"]][,c(1:7)]), ~ as.vector(X_Otipulae_Vm_P7p_wt_mod %*% Otipulae_Vm_P7p_wt_mod[["Sol"]][,c(1:7)][., ]))
      
      data_Otipulae_Vm_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_Otipulae_Vm_P7p_wt_mod,
                      var.a = Otipulae_Vm_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_Vm_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_Vm_P7p_wt_mod <- data_Otipulae_Vm_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_Otipulae_Vm_P7p_wt_mod <- data_Otipulae_Vm_P7p_wt_mod[["mean.obs"]]
      va_data_Otipulae_Vm_P7p_wt_mod <- data_Otipulae_Vm_P7p_wt_mod[["var.a.obs"]]
      vp_data_Otipulae_Vm_P7p_wt_mod <- data_Otipulae_Vm_P7p_wt_mod[["var.obs"]]
      
      Evol_data_Otipulae_Vm_P7p_wt_mod <- (va_data_Otipulae_Vm_P7p_wt_mod/2) / (trait_mean_data_Otipulae_Vm_P7p_wt_mod)^2
      
      mean(h2_data_Otipulae_Vm_P7p_wt_mod)
      mean(trait_mean_data_Otipulae_Vm_P7p_wt_mod)
      mean(va_data_Otipulae_Vm_P7p_wt_mod)
      mean(vp_data_Otipulae_Vm_P7p_wt_mod)
      mean(Evol_data_Otipulae_Vm_P7p_wt_mod)
      
    }
    
  }
  
  
  
  
} 
  
  
  }

#Oonirici
{
  Vm_Oonirici_bi_CONTROL <- subset(Vm_Oonirici_data, Treatment =="CONTROL")
  Vm_Oonirici_bi_MA <- subset(Vm_Oonirici_data, Treatment =="ML")
  
  
  ##---- Oonirici P3p ----
  
  
  #Oonirici_CONTROL_P3p_SSSS_mod 
  {
    
    Oonirici_CONTROL_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer + Ancestral -1,
                                           random      = ~ LineB + BlockRep,
                                           family      = "threshold",
                                           data        = Vm_Oonirici_bi_CONTROL,
                                           prior       = prior_bi_block,
                                           nitt        = 1260000,       
                                           thin        = 500,           
                                           burnin      = 10000,
                                           trunc       = TRUE,
                                           pr          = TRUE,
                                           pl          = TRUE)            
    
    saveRDS(Oonirici_CONTROL_P3p_SSSS_mod, file = "Oonirici_CONTROL_P3p_SSSS_mod.rds")
    Oonirici_CONTROL_P3p_SSSS_mod <- readRDS("Oonirici_CONTROL_P3p_SSSS_mod.rds")
    
    summary(Oonirici_CONTROL_P3p_SSSS_mod) 
    plotTrace(Oonirici_CONTROL_P3p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Oonirici_CONTROL_P3p_SSSS_mod.pdf")
    plot(Oonirici_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(Oonirici_CONTROL_P3p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_CONTROL_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(Oonirici_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_CONTROL_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_CONTROL_P3p_SSSS_mod <- Oonirici_CONTROL_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_CONTROL_P3p_SSSS_mod <- Oonirici_CONTROL_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Oonirici_CONTROL_P3p_SSSS_mod <- rowSums(Oonirici_CONTROL_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Oonirici_CONTROL_P3p_SSSS_mod) 
      HPDinterval(va_liab_Oonirici_CONTROL_P3p_SSSS_mod) 
      
      mean(vlat_Oonirici_CONTROL_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_Oonirici_CONTROL_P3p_SSSS_mod <- Oonirici_CONTROL_P3p_SSSS_mod[["X"]]
      beta_Oonirici_CONTROL_P3p_SSSS_mod <- Oonirici_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_Oonirici_CONTROL_P3p_SSSS_mod   <- apply(beta_Oonirici_CONTROL_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_Oonirici_CONTROL_P3p_SSSS_mod %*% b))}) 
      mean(vf_Oonirici_CONTROL_P3p_SSSS_mod) 
      
      h2_liab_Oonirici_CONTROL_P3p_SSSS_mod <- va_liab_Oonirici_CONTROL_P3p_SSSS_mod / (vlat_Oonirici_CONTROL_P3p_SSSS_mod + vf_Oonirici_CONTROL_P3p_SSSS_mod)
      mean(h2_liab_Oonirici_CONTROL_P3p_SSSS_mod) 
      posterior.mode(h2_liab_Oonirici_CONTROL_P3p_SSSS_mod)	
      median(h2_liab_Oonirici_CONTROL_P3p_SSSS_mod)		
      HPDinterval(h2_liab_Oonirici_CONTROL_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_CONTROL_P3p_SSSS_mod <- ((rowMeans(Oonirici_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_CONTROL_P3p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Oonirici_CONTROL_P3p_SSSS_mod <- (va_liab_Oonirici_CONTROL_P3p_SSSS_mod/2) / (trait_mean_liab_Oonirici_CONTROL_P3p_SSSS_mod)^2
    mean(Evol_liab_Oonirici_CONTROL_P3p_SSSS_mod)
    
    
    #Oonirici_CONTROL_P3p_SSSS_mod data scale
    {
      
      predict_Oonirici_CONTROL_P3p_SSSS_mod <- map(1:nrow(Oonirici_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_CONTROL_P3p_SSSS_mod %*% Oonirici_CONTROL_P3p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_CONTROL_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Oonirici_CONTROL_P3p_SSSS_mod,
                      var.a = Oonirici_CONTROL_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_CONTROL_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_CONTROL_P3p_SSSS_mod <- data_Oonirici_CONTROL_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Oonirici_CONTROL_P3p_SSSS_mod <- data_Oonirici_CONTROL_P3p_SSSS_mod[["mean.obs"]]
      va_data_Oonirici_CONTROL_P3p_SSSS_mod <- data_Oonirici_CONTROL_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_Oonirici_CONTROL_P3p_SSSS_mod <- data_Oonirici_CONTROL_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_Oonirici_CONTROL_P3p_SSSS_mod <- (va_data_Oonirici_CONTROL_P3p_SSSS_mod/2) / (trait_mean_data_Oonirici_CONTROL_P3p_SSSS_mod)^2
      
      mean(h2_data_Oonirici_CONTROL_P3p_SSSS_mod) 
      mean(trait_mean_data_Oonirici_CONTROL_P3p_SSSS_mod)
      mean(va_data_Oonirici_CONTROL_P3p_SSSS_mod) 
      mean(vp_data_Oonirici_CONTROL_P3p_SSSS_mod)
      mean(Evol_data_Oonirici_CONTROL_P3p_SSSS_mod)
      
    }
    
  }
  
  #Oonirici_MA_P3p_SSSS_mod 
  {
    
    Oonirici_MA_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer + Ancestral -1,
                                      random      = ~ LineB + BlockRep,
                                      family      = "threshold",
                                      data        = Vm_Oonirici_bi_MA,
                                      prior       = prior_bi_block,
                                      nitt        = 1260000,       
                                      thin        = 500,           
                                      burnin      = 10000,
                                      trunc       = TRUE,
                                      pr          = TRUE,
                                      pl          = TRUE)         
    
    saveRDS(Oonirici_MA_P3p_SSSS_mod, file = "Oonirici_MA_P3p_SSSS_mod.rds")
    Oonirici_MA_P3p_SSSS_mod <- readRDS("Oonirici_MA_P3p_SSSS_mod.rds")
    
    summary(Oonirici_MA_P3p_SSSS_mod) 
    #plot(Oonirici_MA_P3p_SSSS_mod)
    
    # traces and posterior densities
    pdf("Oonirici_MA_P3p_SSSS_mod.pdf")
    plot(Oonirici_MA_P3p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(Oonirici_MA_P3p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_MA_P3p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_MA_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_MA_P3p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_MA_P3p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_MA_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(Oonirici_MA_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_MA_P3p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_MA_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_MA_P3p_SSSS_mod <- Oonirici_MA_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_MA_P3p_SSSS_mod <- Oonirici_MA_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Oonirici_MA_P3p_SSSS_mod <- rowSums(Oonirici_MA_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Oonirici_MA_P3p_SSSS_mod) 
      HPDinterval(va_liab_Oonirici_MA_P3p_SSSS_mod) 
      
      mean(vlat_Oonirici_MA_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_Oonirici_MA_P3p_SSSS_mod <- Oonirici_MA_P3p_SSSS_mod[["X"]]
      beta_Oonirici_MA_P3p_SSSS_mod <- Oonirici_MA_P3p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_Oonirici_MA_P3p_SSSS_mod   <- apply(beta_Oonirici_MA_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_Oonirici_MA_P3p_SSSS_mod %*% b))}) 
      mean(vf_Oonirici_MA_P3p_SSSS_mod) 
      
      h2_liab_Oonirici_MA_P3p_SSSS_mod <- va_liab_Oonirici_MA_P3p_SSSS_mod / (vlat_Oonirici_MA_P3p_SSSS_mod + vf_Oonirici_MA_P3p_SSSS_mod)
      mean(h2_liab_Oonirici_MA_P3p_SSSS_mod) 
      posterior.mode(h2_liab_Oonirici_MA_P3p_SSSS_mod)	
      median(h2_liab_Oonirici_MA_P3p_SSSS_mod)		
      HPDinterval(h2_liab_Oonirici_MA_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_MA_P3p_SSSS_mod <- ((rowMeans(Oonirici_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_MA_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_MA_P3p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Oonirici_MA_P3p_SSSS_mod <- (va_liab_Oonirici_MA_P3p_SSSS_mod/2) / (trait_mean_liab_Oonirici_MA_P3p_SSSS_mod)^2
    mean(Evol_liab_Oonirici_MA_P3p_SSSS_mod)
    
    #Oonirici_MA_P3p_SSSS_mod data scale
    {
      
      predict_Oonirici_MA_P3p_SSSS_mod <- map(1:nrow(Oonirici_MA_P3p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_MA_P3p_SSSS_mod %*% Oonirici_MA_P3p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_MA_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Oonirici_MA_P3p_SSSS_mod,
                      var.a = Oonirici_MA_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_MA_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_MA_P3p_SSSS_mod <- data_Oonirici_MA_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Oonirici_MA_P3p_SSSS_mod <- data_Oonirici_MA_P3p_SSSS_mod[["mean.obs"]]
      va_data_Oonirici_MA_P3p_SSSS_mod <- data_Oonirici_MA_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_Oonirici_MA_P3p_SSSS_mod <- data_Oonirici_MA_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_Oonirici_MA_P3p_SSSS_mod <- (va_data_Oonirici_MA_P3p_SSSS_mod/2) / (trait_mean_data_Oonirici_MA_P3p_SSSS_mod)^2
      
      mean(h2_data_Oonirici_MA_P3p_SSSS_mod)
      mean(trait_mean_data_Oonirici_MA_P3p_SSSS_mod)
      mean(va_data_Oonirici_MA_P3p_SSSS_mod)
      mean(vp_data_Oonirici_MA_P3p_SSSS_mod)
      mean(Evol_data_Oonirici_MA_P3p_SSSS_mod)
      
    }
    
  }
  
  #Oonirici_Vm_P3p_SSSS_mod 
  {
    
    Oonirici_Vm_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer + Treatment:Ancestral -1,
                                      random      = ~ LineB + BlockRep,
                                      family      = "threshold",
                                      data        = Vm_Oonirici_data,
                                      prior       = prior_bi_block,
                                      nitt        = 1260000,       
                                      thin        = 500,           
                                      burnin      = 10000,
                                      trunc       = TRUE,
                                      pr          = TRUE,
                                      pl          = TRUE)            
    
    saveRDS(Oonirici_Vm_P3p_SSSS_mod, file = "Oonirici_Vm_P3p_SSSS_mod.rds")
    Oonirici_Vm_P3p_SSSS_mod <- readRDS("Oonirici_Vm_P3p_SSSS_mod.rds")
    
    summary(Oonirici_Vm_P3p_SSSS_mod) 
    plotTrace(Oonirici_Vm_P3p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("Oonirici_Vm_P3p_SSSS_mod.pdf")
    plot(Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,c(1:7)]) 
    plot(Oonirici_Vm_P3p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,c(1:7)]) #
    heidel.diag(Oonirici_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,c(1:7)])
    #autocorr.plot(Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,c(1:7)]) #
    autocorr.diag(Oonirici_Vm_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(Oonirici_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,c(1:7)]) # 
    effectiveSize(Oonirici_Vm_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_Vm_P3p_SSSS_mod <- Oonirici_Vm_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_Vm_P3p_SSSS_mod <- Oonirici_Vm_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Oonirici_Vm_P3p_SSSS_mod <- rowSums(Oonirici_Vm_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Oonirici_Vm_P3p_SSSS_mod) 
      HPDinterval(va_liab_Oonirici_Vm_P3p_SSSS_mod) 
      
      mean(vlat_Oonirici_Vm_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_Oonirici_Vm_P3p_SSSS_mod <- Oonirici_Vm_P3p_SSSS_mod[["X"]]
      beta_Oonirici_Vm_P3p_SSSS_mod <- Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,c(1:7)]
      vf_Oonirici_Vm_P3p_SSSS_mod   <- apply(beta_Oonirici_Vm_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_Oonirici_Vm_P3p_SSSS_mod %*% b))}) 
      mean(vf_Oonirici_Vm_P3p_SSSS_mod) 
      
      h2_liab_Oonirici_Vm_P3p_SSSS_mod <- va_liab_Oonirici_Vm_P3p_SSSS_mod / (vlat_Oonirici_Vm_P3p_SSSS_mod + vf_Oonirici_Vm_P3p_SSSS_mod)
      mean(h2_liab_Oonirici_Vm_P3p_SSSS_mod) 
      posterior.mode(h2_liab_Oonirici_Vm_P3p_SSSS_mod)	
      median(h2_liab_Oonirici_Vm_P3p_SSSS_mod)		
      HPDinterval(h2_liab_Oonirici_Vm_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    mean(rowMeans(Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)])) 
    mean(rowMeans(Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,5]) 
    mean(rowMeans(Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,6]) 
    mean(rowMeans(Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,7]) 
    
    trait_mean_liab_Oonirici_Vm_P3p_SSSS_mod <- ((rowMeans(Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,5]) + (rowMeans(Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,6]) + (rowMeans(Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,7]))/4)
    
    Evol_liab_Oonirici_Vm_P3p_SSSS_mod <- (va_liab_Oonirici_Vm_P3p_SSSS_mod/2) / (trait_mean_liab_Oonirici_Vm_P3p_SSSS_mod)^2
    mean(Evol_liab_Oonirici_Vm_P3p_SSSS_mod)
    
    #Oonirici_Vm_P3p_SSSS_mod data scale
    {
      
      predict_Oonirici_Vm_P3p_SSSS_mod <- map(1:nrow(Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,c(1:7)]), ~ as.vector(X_Oonirici_Vm_P3p_SSSS_mod %*% Oonirici_Vm_P3p_SSSS_mod[["Sol"]][,c(1:7)][., ]))
      
      data_Oonirici_Vm_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Oonirici_Vm_P3p_SSSS_mod,
                      var.a = Oonirici_Vm_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_Vm_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_Vm_P3p_SSSS_mod <- data_Oonirici_Vm_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Oonirici_Vm_P3p_SSSS_mod <- data_Oonirici_Vm_P3p_SSSS_mod[["mean.obs"]]
      va_data_Oonirici_Vm_P3p_SSSS_mod <- data_Oonirici_Vm_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_Oonirici_Vm_P3p_SSSS_mod <- data_Oonirici_Vm_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_Oonirici_Vm_P3p_SSSS_mod <- (va_data_Oonirici_Vm_P3p_SSSS_mod/2) / (trait_mean_data_Oonirici_Vm_P3p_SSSS_mod)^2
      
      mean(h2_data_Oonirici_Vm_P3p_SSSS_mod)
      mean(trait_mean_data_Oonirici_Vm_P3p_SSSS_mod)
      mean(va_data_Oonirici_Vm_P3p_SSSS_mod)
      mean(vp_data_Oonirici_Vm_P3p_SSSS_mod)
      mean(Evol_data_Oonirici_Vm_P3p_SSSS_mod)
      
    }
    
  }
  
  
 
  
  
  ##---- Oonirici P4p ----
  
  
  #Oonirici_CONTROL_P4p_SSSS_mod 
  {
    
    Oonirici_CONTROL_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer + Ancestral -1,
                                           random      = ~ LineB + BlockRep,
                                           family      = "threshold",
                                           data        = Vm_Oonirici_bi_CONTROL,
                                           prior       = prior_bi_block,
                                           nitt        = 1260000,       
                                           thin        = 500,           
                                           burnin      = 10000,
                                           trunc       = TRUE,
                                           pr          = TRUE,
                                           pl          = TRUE)            
    
    saveRDS(Oonirici_CONTROL_P4p_SSSS_mod, file = "Oonirici_CONTROL_P4p_SSSS_mod.rds")
    Oonirici_CONTROL_P4p_SSSS_mod <- readRDS("Oonirici_CONTROL_P4p_SSSS_mod.rds")
    
    summary(Oonirici_CONTROL_P4p_SSSS_mod) 
    plotTrace(Oonirici_CONTROL_P4p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Oonirici_CONTROL_P4p_SSSS_mod.pdf")
    plot(Oonirici_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(Oonirici_CONTROL_P4p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_CONTROL_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(Oonirici_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_CONTROL_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_CONTROL_P4p_SSSS_mod <- Oonirici_CONTROL_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_CONTROL_P4p_SSSS_mod <- Oonirici_CONTROL_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Oonirici_CONTROL_P4p_SSSS_mod <- rowSums(Oonirici_CONTROL_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Oonirici_CONTROL_P4p_SSSS_mod) 
      HPDinterval(va_liab_Oonirici_CONTROL_P4p_SSSS_mod) 
      
      mean(vlat_Oonirici_CONTROL_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_Oonirici_CONTROL_P4p_SSSS_mod <- Oonirici_CONTROL_P4p_SSSS_mod[["X"]]
      beta_Oonirici_CONTROL_P4p_SSSS_mod <- Oonirici_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_Oonirici_CONTROL_P4p_SSSS_mod   <- apply(beta_Oonirici_CONTROL_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_Oonirici_CONTROL_P4p_SSSS_mod %*% b))}) 
      mean(vf_Oonirici_CONTROL_P4p_SSSS_mod) 
      
      h2_liab_Oonirici_CONTROL_P4p_SSSS_mod <- va_liab_Oonirici_CONTROL_P4p_SSSS_mod / (vlat_Oonirici_CONTROL_P4p_SSSS_mod + vf_Oonirici_CONTROL_P4p_SSSS_mod)
      mean(h2_liab_Oonirici_CONTROL_P4p_SSSS_mod) 
      posterior.mode(h2_liab_Oonirici_CONTROL_P4p_SSSS_mod)	
      median(h2_liab_Oonirici_CONTROL_P4p_SSSS_mod)		
      HPDinterval(h2_liab_Oonirici_CONTROL_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_CONTROL_P4p_SSSS_mod <- ((rowMeans(Oonirici_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_CONTROL_P4p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Oonirici_CONTROL_P4p_SSSS_mod <- (va_liab_Oonirici_CONTROL_P4p_SSSS_mod/2) / (trait_mean_liab_Oonirici_CONTROL_P4p_SSSS_mod)^2
    mean(Evol_liab_Oonirici_CONTROL_P4p_SSSS_mod)
    
    
    #Oonirici_CONTROL_P4p_SSSS_mod data scale
    {
      
      predict_Oonirici_CONTROL_P4p_SSSS_mod <- map(1:nrow(Oonirici_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_CONTROL_P4p_SSSS_mod %*% Oonirici_CONTROL_P4p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_CONTROL_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Oonirici_CONTROL_P4p_SSSS_mod,
                      var.a = Oonirici_CONTROL_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_CONTROL_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_CONTROL_P4p_SSSS_mod <- data_Oonirici_CONTROL_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Oonirici_CONTROL_P4p_SSSS_mod <- data_Oonirici_CONTROL_P4p_SSSS_mod[["mean.obs"]]
      va_data_Oonirici_CONTROL_P4p_SSSS_mod <- data_Oonirici_CONTROL_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_Oonirici_CONTROL_P4p_SSSS_mod <- data_Oonirici_CONTROL_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_Oonirici_CONTROL_P4p_SSSS_mod <- (va_data_Oonirici_CONTROL_P4p_SSSS_mod/2) / (trait_mean_data_Oonirici_CONTROL_P4p_SSSS_mod)^2
      
      mean(h2_data_Oonirici_CONTROL_P4p_SSSS_mod) 
      mean(trait_mean_data_Oonirici_CONTROL_P4p_SSSS_mod)
      mean(va_data_Oonirici_CONTROL_P4p_SSSS_mod) 
      mean(vp_data_Oonirici_CONTROL_P4p_SSSS_mod)
      mean(Evol_data_Oonirici_CONTROL_P4p_SSSS_mod)
      
    }
    
  }
  
  #Oonirici_MA_P4p_SSSS_mod 
  {
    
    Oonirici_MA_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer + Ancestral -1,
                                      random      = ~ LineB + BlockRep,
                                      family      = "threshold",
                                      data        = Vm_Oonirici_bi_MA,
                                      prior       = prior_bi_block,
                                      nitt        = 1260000,       
                                      thin        = 500,           
                                      burnin      = 10000,
                                      trunc       = TRUE,
                                      pr          = TRUE,
                                      pl          = TRUE)         
    
    saveRDS(Oonirici_MA_P4p_SSSS_mod, file = "Oonirici_MA_P4p_SSSS_mod.rds")
    Oonirici_MA_P4p_SSSS_mod <- readRDS("Oonirici_MA_P4p_SSSS_mod.rds")
    
    summary(Oonirici_MA_P4p_SSSS_mod) 
    #plot(Oonirici_MA_P4p_SSSS_mod)
    
    # traces and posterior densities
    pdf("Oonirici_MA_P4p_SSSS_mod.pdf")
    plot(Oonirici_MA_P4p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(Oonirici_MA_P4p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_MA_P4p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_MA_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_MA_P4p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_MA_P4p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_MA_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(Oonirici_MA_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_MA_P4p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_MA_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_MA_P4p_SSSS_mod <- Oonirici_MA_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_MA_P4p_SSSS_mod <- Oonirici_MA_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Oonirici_MA_P4p_SSSS_mod <- rowSums(Oonirici_MA_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Oonirici_MA_P4p_SSSS_mod) 
      HPDinterval(va_liab_Oonirici_MA_P4p_SSSS_mod) 
      
      mean(vlat_Oonirici_MA_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_Oonirici_MA_P4p_SSSS_mod <- Oonirici_MA_P4p_SSSS_mod[["X"]]
      beta_Oonirici_MA_P4p_SSSS_mod <- Oonirici_MA_P4p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_Oonirici_MA_P4p_SSSS_mod   <- apply(beta_Oonirici_MA_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_Oonirici_MA_P4p_SSSS_mod %*% b))}) 
      mean(vf_Oonirici_MA_P4p_SSSS_mod) 
      
      h2_liab_Oonirici_MA_P4p_SSSS_mod <- va_liab_Oonirici_MA_P4p_SSSS_mod / (vlat_Oonirici_MA_P4p_SSSS_mod + vf_Oonirici_MA_P4p_SSSS_mod)
      mean(h2_liab_Oonirici_MA_P4p_SSSS_mod) 
      posterior.mode(h2_liab_Oonirici_MA_P4p_SSSS_mod)	
      median(h2_liab_Oonirici_MA_P4p_SSSS_mod)		
      HPDinterval(h2_liab_Oonirici_MA_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_MA_P4p_SSSS_mod <- ((rowMeans(Oonirici_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_MA_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_MA_P4p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Oonirici_MA_P4p_SSSS_mod <- (va_liab_Oonirici_MA_P4p_SSSS_mod/2) / (trait_mean_liab_Oonirici_MA_P4p_SSSS_mod)^2
    mean(Evol_liab_Oonirici_MA_P4p_SSSS_mod)
    
    #Oonirici_MA_P4p_SSSS_mod data scale
    {
      
      predict_Oonirici_MA_P4p_SSSS_mod <- map(1:nrow(Oonirici_MA_P4p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_MA_P4p_SSSS_mod %*% Oonirici_MA_P4p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_MA_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Oonirici_MA_P4p_SSSS_mod,
                      var.a = Oonirici_MA_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_MA_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_MA_P4p_SSSS_mod <- data_Oonirici_MA_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Oonirici_MA_P4p_SSSS_mod <- data_Oonirici_MA_P4p_SSSS_mod[["mean.obs"]]
      va_data_Oonirici_MA_P4p_SSSS_mod <- data_Oonirici_MA_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_Oonirici_MA_P4p_SSSS_mod <- data_Oonirici_MA_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_Oonirici_MA_P4p_SSSS_mod <- (va_data_Oonirici_MA_P4p_SSSS_mod/2) / (trait_mean_data_Oonirici_MA_P4p_SSSS_mod)^2
      
      mean(h2_data_Oonirici_MA_P4p_SSSS_mod)
      mean(trait_mean_data_Oonirici_MA_P4p_SSSS_mod)
      mean(va_data_Oonirici_MA_P4p_SSSS_mod)
      mean(vp_data_Oonirici_MA_P4p_SSSS_mod)
      mean(Evol_data_Oonirici_MA_P4p_SSSS_mod)
      
    }
    
  }
  
  #Oonirici_Vm_P4p_SSSS_mod 
  {
    
    Oonirici_Vm_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer + Treatment:Ancestral -1,
                                      random      = ~ LineB + BlockRep,
                                      family      = "threshold",
                                      data        = Vm_Oonirici_data,
                                      prior       = prior_bi_block,
                                      nitt        = 1260000,       
                                      thin        = 500,           
                                      burnin      = 10000,
                                      trunc       = TRUE,
                                      pr          = TRUE,
                                      pl          = TRUE)            
    
    saveRDS(Oonirici_Vm_P4p_SSSS_mod, file = "Oonirici_Vm_P4p_SSSS_mod.rds")
    Oonirici_Vm_P4p_SSSS_mod <- readRDS("Oonirici_Vm_P4p_SSSS_mod.rds")
    
    summary(Oonirici_Vm_P4p_SSSS_mod) 
    plotTrace(Oonirici_Vm_P4p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("Oonirici_Vm_P4p_SSSS_mod.pdf")
    plot(Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,c(1:7)]) 
    plot(Oonirici_Vm_P4p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,c(1:7)]) #
    heidel.diag(Oonirici_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,c(1:7)])
    #autocorr.plot(Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,c(1:7)]) #
    autocorr.diag(Oonirici_Vm_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(Oonirici_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,c(1:7)]) # 
    effectiveSize(Oonirici_Vm_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_Vm_P4p_SSSS_mod <- Oonirici_Vm_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_Vm_P4p_SSSS_mod <- Oonirici_Vm_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Oonirici_Vm_P4p_SSSS_mod <- rowSums(Oonirici_Vm_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Oonirici_Vm_P4p_SSSS_mod) 
      HPDinterval(va_liab_Oonirici_Vm_P4p_SSSS_mod) 
      
      mean(vlat_Oonirici_Vm_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_Oonirici_Vm_P4p_SSSS_mod <- Oonirici_Vm_P4p_SSSS_mod[["X"]]
      beta_Oonirici_Vm_P4p_SSSS_mod <- Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,c(1:7)]
      vf_Oonirici_Vm_P4p_SSSS_mod   <- apply(beta_Oonirici_Vm_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_Oonirici_Vm_P4p_SSSS_mod %*% b))}) 
      mean(vf_Oonirici_Vm_P4p_SSSS_mod) 
      
      h2_liab_Oonirici_Vm_P4p_SSSS_mod <- va_liab_Oonirici_Vm_P4p_SSSS_mod / (vlat_Oonirici_Vm_P4p_SSSS_mod + vf_Oonirici_Vm_P4p_SSSS_mod)
      mean(h2_liab_Oonirici_Vm_P4p_SSSS_mod) 
      posterior.mode(h2_liab_Oonirici_Vm_P4p_SSSS_mod)	
      median(h2_liab_Oonirici_Vm_P4p_SSSS_mod)		
      HPDinterval(h2_liab_Oonirici_Vm_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    mean(rowMeans(Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)])) 
    mean(rowMeans(Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,5]) 
    mean(rowMeans(Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,6]) 
    mean(rowMeans(Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,7]) 
    
    trait_mean_liab_Oonirici_Vm_P4p_SSSS_mod <- ((rowMeans(Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,5]) + (rowMeans(Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,6]) + (rowMeans(Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,7]))/4)
    
    Evol_liab_Oonirici_Vm_P4p_SSSS_mod <- (va_liab_Oonirici_Vm_P4p_SSSS_mod/2) / (trait_mean_liab_Oonirici_Vm_P4p_SSSS_mod)^2
    mean(Evol_liab_Oonirici_Vm_P4p_SSSS_mod)
    
    #Oonirici_Vm_P4p_SSSS_mod data scale
    {
      
      predict_Oonirici_Vm_P4p_SSSS_mod <- map(1:nrow(Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,c(1:7)]), ~ as.vector(X_Oonirici_Vm_P4p_SSSS_mod %*% Oonirici_Vm_P4p_SSSS_mod[["Sol"]][,c(1:7)][., ]))
      
      data_Oonirici_Vm_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Oonirici_Vm_P4p_SSSS_mod,
                      var.a = Oonirici_Vm_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_Vm_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_Vm_P4p_SSSS_mod <- data_Oonirici_Vm_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Oonirici_Vm_P4p_SSSS_mod <- data_Oonirici_Vm_P4p_SSSS_mod[["mean.obs"]]
      va_data_Oonirici_Vm_P4p_SSSS_mod <- data_Oonirici_Vm_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_Oonirici_Vm_P4p_SSSS_mod <- data_Oonirici_Vm_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_Oonirici_Vm_P4p_SSSS_mod <- (va_data_Oonirici_Vm_P4p_SSSS_mod/2) / (trait_mean_data_Oonirici_Vm_P4p_SSSS_mod)^2
      
      mean(h2_data_Oonirici_Vm_P4p_SSSS_mod)
      mean(trait_mean_data_Oonirici_Vm_P4p_SSSS_mod)
      mean(va_data_Oonirici_Vm_P4p_SSSS_mod)
      mean(vp_data_Oonirici_Vm_P4p_SSSS_mod)
      mean(Evol_data_Oonirici_Vm_P4p_SSSS_mod)
      
    }
    
  }
  
  
  
  
  ##---- Oonirici P8p ----
  
  #Oonirici_CONTROL_P8p_SSSS_mod 
  {
    
    Oonirici_CONTROL_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer + Ancestral -1,
                                           random      = ~ LineB + BlockRep,
                                           family      = "threshold",
                                           data        = Vm_Oonirici_bi_CONTROL,
                                           prior       = prior_bi_block,
                                           nitt        = 1260000,       
                                           thin        = 500,           
                                           burnin      = 10000,
                                           trunc       = TRUE,
                                           pr          = TRUE,
                                           pl          = TRUE)            
    
    saveRDS(Oonirici_CONTROL_P8p_SSSS_mod, file = "Oonirici_CONTROL_P8p_SSSS_mod.rds")
    Oonirici_CONTROL_P8p_SSSS_mod <- readRDS("Oonirici_CONTROL_P8p_SSSS_mod.rds")
    
    summary(Oonirici_CONTROL_P8p_SSSS_mod) 
    plotTrace(Oonirici_CONTROL_P8p_SSSS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Oonirici_CONTROL_P8p_SSSS_mod.pdf")
    plot(Oonirici_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(Oonirici_CONTROL_P8p_SSSS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_CONTROL_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(Oonirici_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_CONTROL_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_CONTROL_P8p_SSSS_mod <- Oonirici_CONTROL_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_CONTROL_P8p_SSSS_mod <- Oonirici_CONTROL_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Oonirici_CONTROL_P8p_SSSS_mod <- rowSums(Oonirici_CONTROL_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Oonirici_CONTROL_P8p_SSSS_mod) 
      HPDinterval(va_liab_Oonirici_CONTROL_P8p_SSSS_mod) 
      
      mean(vlat_Oonirici_CONTROL_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_Oonirici_CONTROL_P8p_SSSS_mod <- Oonirici_CONTROL_P8p_SSSS_mod[["X"]]
      beta_Oonirici_CONTROL_P8p_SSSS_mod <- Oonirici_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_Oonirici_CONTROL_P8p_SSSS_mod   <- apply(beta_Oonirici_CONTROL_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_Oonirici_CONTROL_P8p_SSSS_mod %*% b))}) 
      mean(vf_Oonirici_CONTROL_P8p_SSSS_mod) 
      
      h2_liab_Oonirici_CONTROL_P8p_SSSS_mod <- va_liab_Oonirici_CONTROL_P8p_SSSS_mod / (vlat_Oonirici_CONTROL_P8p_SSSS_mod + vf_Oonirici_CONTROL_P8p_SSSS_mod)
      mean(h2_liab_Oonirici_CONTROL_P8p_SSSS_mod) 
      posterior.mode(h2_liab_Oonirici_CONTROL_P8p_SSSS_mod)	
      median(h2_liab_Oonirici_CONTROL_P8p_SSSS_mod)		
      HPDinterval(h2_liab_Oonirici_CONTROL_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_CONTROL_P8p_SSSS_mod <- ((rowMeans(Oonirici_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_CONTROL_P8p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Oonirici_CONTROL_P8p_SSSS_mod <- (va_liab_Oonirici_CONTROL_P8p_SSSS_mod/2) / (trait_mean_liab_Oonirici_CONTROL_P8p_SSSS_mod)^2
    mean(Evol_liab_Oonirici_CONTROL_P8p_SSSS_mod)
    
    
    #Oonirici_CONTROL_P8p_SSSS_mod data scale
    {
      
      predict_Oonirici_CONTROL_P8p_SSSS_mod <- map(1:nrow(Oonirici_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_CONTROL_P8p_SSSS_mod %*% Oonirici_CONTROL_P8p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_CONTROL_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Oonirici_CONTROL_P8p_SSSS_mod,
                      var.a = Oonirici_CONTROL_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_CONTROL_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_CONTROL_P8p_SSSS_mod <- data_Oonirici_CONTROL_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Oonirici_CONTROL_P8p_SSSS_mod <- data_Oonirici_CONTROL_P8p_SSSS_mod[["mean.obs"]]
      va_data_Oonirici_CONTROL_P8p_SSSS_mod <- data_Oonirici_CONTROL_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_Oonirici_CONTROL_P8p_SSSS_mod <- data_Oonirici_CONTROL_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_Oonirici_CONTROL_P8p_SSSS_mod <- (va_data_Oonirici_CONTROL_P8p_SSSS_mod/2) / (trait_mean_data_Oonirici_CONTROL_P8p_SSSS_mod)^2
      
      mean(h2_data_Oonirici_CONTROL_P8p_SSSS_mod) 
      mean(trait_mean_data_Oonirici_CONTROL_P8p_SSSS_mod)
      mean(va_data_Oonirici_CONTROL_P8p_SSSS_mod) 
      mean(vp_data_Oonirici_CONTROL_P8p_SSSS_mod)
      mean(Evol_data_Oonirici_CONTROL_P8p_SSSS_mod)
      
    }
    
  }
  
  #Oonirici_MA_P8p_SSSS_mod 
  {
    
    Oonirici_MA_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer + Ancestral -1,
                                      random      = ~ LineB + BlockRep,
                                      family      = "threshold",
                                      data        = Vm_Oonirici_bi_MA,
                                      prior       = prior_bi_block,
                                      nitt        = 1260000,       
                                      thin        = 500,           
                                      burnin      = 10000,
                                      trunc       = TRUE,
                                      pr          = TRUE,
                                      pl          = TRUE)         
    
    saveRDS(Oonirici_MA_P8p_SSSS_mod, file = "Oonirici_MA_P8p_SSSS_mod.rds")
    Oonirici_MA_P8p_SSSS_mod <- readRDS("Oonirici_MA_P8p_SSSS_mod.rds")
    
    summary(Oonirici_MA_P8p_SSSS_mod) 
    #plot(Oonirici_MA_P8p_SSSS_mod)
    
    # traces and posterior densities
    pdf("Oonirici_MA_P8p_SSSS_mod.pdf")
    plot(Oonirici_MA_P8p_SSSS_mod[["Sol"]][,c(1:5)]) 
    plot(Oonirici_MA_P8p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_MA_P8p_SSSS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_MA_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_MA_P8p_SSSS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_MA_P8p_SSSS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_MA_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(Oonirici_MA_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_MA_P8p_SSSS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_MA_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_MA_P8p_SSSS_mod <- Oonirici_MA_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_MA_P8p_SSSS_mod <- Oonirici_MA_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Oonirici_MA_P8p_SSSS_mod <- rowSums(Oonirici_MA_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Oonirici_MA_P8p_SSSS_mod) 
      HPDinterval(va_liab_Oonirici_MA_P8p_SSSS_mod) 
      
      mean(vlat_Oonirici_MA_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_Oonirici_MA_P8p_SSSS_mod <- Oonirici_MA_P8p_SSSS_mod[["X"]]
      beta_Oonirici_MA_P8p_SSSS_mod <- Oonirici_MA_P8p_SSSS_mod[["Sol"]][,c(1:5)]
      vf_Oonirici_MA_P8p_SSSS_mod   <- apply(beta_Oonirici_MA_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_Oonirici_MA_P8p_SSSS_mod %*% b))}) 
      mean(vf_Oonirici_MA_P8p_SSSS_mod) 
      
      h2_liab_Oonirici_MA_P8p_SSSS_mod <- va_liab_Oonirici_MA_P8p_SSSS_mod / (vlat_Oonirici_MA_P8p_SSSS_mod + vf_Oonirici_MA_P8p_SSSS_mod)
      mean(h2_liab_Oonirici_MA_P8p_SSSS_mod) 
      posterior.mode(h2_liab_Oonirici_MA_P8p_SSSS_mod)	
      median(h2_liab_Oonirici_MA_P8p_SSSS_mod)		
      HPDinterval(h2_liab_Oonirici_MA_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_MA_P8p_SSSS_mod <- ((rowMeans(Oonirici_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_MA_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_MA_P8p_SSSS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Oonirici_MA_P8p_SSSS_mod <- (va_liab_Oonirici_MA_P8p_SSSS_mod/2) / (trait_mean_liab_Oonirici_MA_P8p_SSSS_mod)^2
    mean(Evol_liab_Oonirici_MA_P8p_SSSS_mod)
    
    #Oonirici_MA_P8p_SSSS_mod data scale
    {
      
      predict_Oonirici_MA_P8p_SSSS_mod <- map(1:nrow(Oonirici_MA_P8p_SSSS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_MA_P8p_SSSS_mod %*% Oonirici_MA_P8p_SSSS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_MA_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Oonirici_MA_P8p_SSSS_mod,
                      var.a = Oonirici_MA_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_MA_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_MA_P8p_SSSS_mod <- data_Oonirici_MA_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Oonirici_MA_P8p_SSSS_mod <- data_Oonirici_MA_P8p_SSSS_mod[["mean.obs"]]
      va_data_Oonirici_MA_P8p_SSSS_mod <- data_Oonirici_MA_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_Oonirici_MA_P8p_SSSS_mod <- data_Oonirici_MA_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_Oonirici_MA_P8p_SSSS_mod <- (va_data_Oonirici_MA_P8p_SSSS_mod/2) / (trait_mean_data_Oonirici_MA_P8p_SSSS_mod)^2
      
      mean(h2_data_Oonirici_MA_P8p_SSSS_mod)
      mean(trait_mean_data_Oonirici_MA_P8p_SSSS_mod)
      mean(va_data_Oonirici_MA_P8p_SSSS_mod)
      mean(vp_data_Oonirici_MA_P8p_SSSS_mod)
      mean(Evol_data_Oonirici_MA_P8p_SSSS_mod)
      
    }
    
  }
  
  #Oonirici_Vm_P8p_SSSS_mod 
  {
    
    Oonirici_Vm_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer + Treatment:Ancestral -1,
                                      random      = ~ LineB + BlockRep,
                                      family      = "threshold",
                                      data        = Vm_Oonirici_data,
                                      prior       = prior_bi_block,
                                      nitt        = 1260000,       
                                      thin        = 500,           
                                      burnin      = 10000,
                                      trunc       = TRUE,
                                      pr          = TRUE,
                                      pl          = TRUE)            
    
    saveRDS(Oonirici_Vm_P8p_SSSS_mod, file = "Oonirici_Vm_P8p_SSSS_mod.rds")
    Oonirici_Vm_P8p_SSSS_mod <- readRDS("Oonirici_Vm_P8p_SSSS_mod.rds")
    
    summary(Oonirici_Vm_P8p_SSSS_mod) 
    plotTrace(Oonirici_Vm_P8p_SSSS_mod$Sol)
    
    # traces and posterior densities
    pdf("Oonirici_Vm_P8p_SSSS_mod.pdf")
    plot(Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,c(1:7)]) 
    plot(Oonirici_Vm_P8p_SSSS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,c(1:7)]) #
    heidel.diag(Oonirici_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,c(1:7)])
    #autocorr.plot(Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,c(1:7)]) #
    autocorr.diag(Oonirici_Vm_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(Oonirici_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,c(1:7)]) # 
    effectiveSize(Oonirici_Vm_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_Vm_P8p_SSSS_mod <- Oonirici_Vm_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_Vm_P8p_SSSS_mod <- Oonirici_Vm_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Oonirici_Vm_P8p_SSSS_mod <- rowSums(Oonirici_Vm_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Oonirici_Vm_P8p_SSSS_mod) 
      HPDinterval(va_liab_Oonirici_Vm_P8p_SSSS_mod) 
      
      mean(vlat_Oonirici_Vm_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_Oonirici_Vm_P8p_SSSS_mod <- Oonirici_Vm_P8p_SSSS_mod[["X"]]
      beta_Oonirici_Vm_P8p_SSSS_mod <- Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,c(1:7)]
      vf_Oonirici_Vm_P8p_SSSS_mod   <- apply(beta_Oonirici_Vm_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_Oonirici_Vm_P8p_SSSS_mod %*% b))}) 
      mean(vf_Oonirici_Vm_P8p_SSSS_mod) 
      
      h2_liab_Oonirici_Vm_P8p_SSSS_mod <- va_liab_Oonirici_Vm_P8p_SSSS_mod / (vlat_Oonirici_Vm_P8p_SSSS_mod + vf_Oonirici_Vm_P8p_SSSS_mod)
      mean(h2_liab_Oonirici_Vm_P8p_SSSS_mod) 
      posterior.mode(h2_liab_Oonirici_Vm_P8p_SSSS_mod)	
      median(h2_liab_Oonirici_Vm_P8p_SSSS_mod)		
      HPDinterval(h2_liab_Oonirici_Vm_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    mean(rowMeans(Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)])) 
    mean(rowMeans(Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,5]) 
    mean(rowMeans(Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,6]) 
    mean(rowMeans(Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,7]) 
    
    trait_mean_liab_Oonirici_Vm_P8p_SSSS_mod <- ((rowMeans(Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,5]) + (rowMeans(Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,6]) + (rowMeans(Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,7]))/4)
    
    Evol_liab_Oonirici_Vm_P8p_SSSS_mod <- (va_liab_Oonirici_Vm_P8p_SSSS_mod/2) / (trait_mean_liab_Oonirici_Vm_P8p_SSSS_mod)^2
    mean(Evol_liab_Oonirici_Vm_P8p_SSSS_mod)
    
    #Oonirici_Vm_P8p_SSSS_mod data scale
    {
      
      predict_Oonirici_Vm_P8p_SSSS_mod <- map(1:nrow(Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,c(1:7)]), ~ as.vector(X_Oonirici_Vm_P8p_SSSS_mod %*% Oonirici_Vm_P8p_SSSS_mod[["Sol"]][,c(1:7)][., ]))
      
      data_Oonirici_Vm_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Oonirici_Vm_P8p_SSSS_mod,
                      var.a = Oonirici_Vm_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_Vm_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_Vm_P8p_SSSS_mod <- data_Oonirici_Vm_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Oonirici_Vm_P8p_SSSS_mod <- data_Oonirici_Vm_P8p_SSSS_mod[["mean.obs"]]
      va_data_Oonirici_Vm_P8p_SSSS_mod <- data_Oonirici_Vm_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_Oonirici_Vm_P8p_SSSS_mod <- data_Oonirici_Vm_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_Oonirici_Vm_P8p_SSSS_mod <- (va_data_Oonirici_Vm_P8p_SSSS_mod/2) / (trait_mean_data_Oonirici_Vm_P8p_SSSS_mod)^2
      
      mean(h2_data_Oonirici_Vm_P8p_SSSS_mod)
      mean(trait_mean_data_Oonirici_Vm_P8p_SSSS_mod)
      mean(va_data_Oonirici_Vm_P8p_SSSS_mod)
      mean(vp_data_Oonirici_Vm_P8p_SSSS_mod)
      mean(Evol_data_Oonirici_Vm_P8p_SSSS_mod)
      
    }
    
  }
  
   ## Oonirici P5p ----
  
  
  #Oonirici_CONTROL_P5p_wt_mod 
  {
    
    Oonirici_CONTROL_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Ancestral -1,
                                            random      = ~ LineB + BlockRep,
                                            family      = "threshold",
                                            data        = Vm_Oonirici_bi_CONTROL,
                                            prior       = prior_bi_block,
                                            nitt        = 1260000,       
                                            thin        = 500,           
                                            burnin      = 10000,
                                            trunc       = TRUE,
                                            pr          = TRUE,
                                            pl          = TRUE)            
    
    saveRDS(Oonirici_CONTROL_P5p_wt_mod, file = "Oonirici_CONTROL_P5p_wt_mod.rds")
    Oonirici_CONTROL_P5p_wt_mod <- readRDS("Oonirici_CONTROL_P5p_wt_mod.rds")
    
    summary(Oonirici_CONTROL_P5p_wt_mod) 
    plotTrace(Oonirici_CONTROL_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Oonirici_CONTROL_P5p_wt_mod.pdf")
    plot(Oonirici_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Oonirici_CONTROL_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_CONTROL_P5p_wt_mod[["VCV"]])
    #autocorr.plot(Oonirici_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_CONTROL_P5p_wt_mod <- Oonirici_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_CONTROL_P5p_wt_mod <- Oonirici_CONTROL_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_Oonirici_CONTROL_P5p_wt_mod <- rowSums(Oonirici_CONTROL_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_Oonirici_CONTROL_P5p_wt_mod) 
      HPDinterval(va_liab_Oonirici_CONTROL_P5p_wt_mod) 
      
      mean(vlat_Oonirici_CONTROL_P5p_wt_mod) 
      
      #variance of fixed effects
      X_Oonirici_CONTROL_P5p_wt_mod <- Oonirici_CONTROL_P5p_wt_mod[["X"]]
      beta_Oonirici_CONTROL_P5p_wt_mod <- Oonirici_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]
      vf_Oonirici_CONTROL_P5p_wt_mod   <- apply(beta_Oonirici_CONTROL_P5p_wt_mod, 1, function(b) {var(as.vector(X_Oonirici_CONTROL_P5p_wt_mod %*% b))}) 
      mean(vf_Oonirici_CONTROL_P5p_wt_mod) 
      
      h2_liab_Oonirici_CONTROL_P5p_wt_mod <- va_liab_Oonirici_CONTROL_P5p_wt_mod / (vlat_Oonirici_CONTROL_P5p_wt_mod + vf_Oonirici_CONTROL_P5p_wt_mod)
      mean(h2_liab_Oonirici_CONTROL_P5p_wt_mod) 
      posterior.mode(h2_liab_Oonirici_CONTROL_P5p_wt_mod)	
      median(h2_liab_Oonirici_CONTROL_P5p_wt_mod)		
      HPDinterval(h2_liab_Oonirici_CONTROL_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_CONTROL_P5p_wt_mod <- ((rowMeans(Oonirici_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_CONTROL_P5p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Oonirici_CONTROL_P5p_wt_mod <- (va_liab_Oonirici_CONTROL_P5p_wt_mod/2) / (trait_mean_liab_Oonirici_CONTROL_P5p_wt_mod)^2
    mean(Evol_liab_Oonirici_CONTROL_P5p_wt_mod)
    
    
    #Oonirici_CONTROL_P5p_wt_mod data scale
    {
      
      predict_Oonirici_CONTROL_P5p_wt_mod <- map(1:nrow(Oonirici_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_CONTROL_P5p_wt_mod %*% Oonirici_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_CONTROL_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_Oonirici_CONTROL_P5p_wt_mod,
                      var.a = Oonirici_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_CONTROL_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_CONTROL_P5p_wt_mod <- data_Oonirici_CONTROL_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_Oonirici_CONTROL_P5p_wt_mod <- data_Oonirici_CONTROL_P5p_wt_mod[["mean.obs"]]
      va_data_Oonirici_CONTROL_P5p_wt_mod <- data_Oonirici_CONTROL_P5p_wt_mod[["var.a.obs"]]
      vp_data_Oonirici_CONTROL_P5p_wt_mod <- data_Oonirici_CONTROL_P5p_wt_mod[["var.obs"]]
      
      Evol_data_Oonirici_CONTROL_P5p_wt_mod <- (va_data_Oonirici_CONTROL_P5p_wt_mod/2) / (trait_mean_data_Oonirici_CONTROL_P5p_wt_mod)^2
      
      mean(h2_data_Oonirici_CONTROL_P5p_wt_mod) 
      mean(trait_mean_data_Oonirici_CONTROL_P5p_wt_mod)
      mean(va_data_Oonirici_CONTROL_P5p_wt_mod) 
      mean(vp_data_Oonirici_CONTROL_P5p_wt_mod)
      mean(Evol_data_Oonirici_CONTROL_P5p_wt_mod)
      
    }
    
  }
  
  #Oonirici_MA_P5p_wt_mod 
  {
    
    Oonirici_MA_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_Oonirici_bi_MA,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)         
    
    saveRDS(Oonirici_MA_P5p_wt_mod, file = "Oonirici_MA_P5p_wt_mod.rds")
    Oonirici_MA_P5p_wt_mod <- readRDS("Oonirici_MA_P5p_wt_mod.rds")
    
    summary(Oonirici_MA_P5p_wt_mod) 
    #plot(Oonirici_MA_P5p_wt_mod)
    
    # traces and posterior densities
    pdf("Oonirici_MA_P5p_wt_mod.pdf")
    plot(Oonirici_MA_P5p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Oonirici_MA_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_MA_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_MA_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_MA_P5p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_MA_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_MA_P5p_wt_mod[["VCV"]])
    #autocorr.plot(Oonirici_MA_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_MA_P5p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_MA_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_MA_P5p_wt_mod <- Oonirici_MA_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_MA_P5p_wt_mod <- Oonirici_MA_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_Oonirici_MA_P5p_wt_mod <- rowSums(Oonirici_MA_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_Oonirici_MA_P5p_wt_mod) 
      HPDinterval(va_liab_Oonirici_MA_P5p_wt_mod) 
      
      mean(vlat_Oonirici_MA_P5p_wt_mod) 
      
      #variance of fixed effects
      X_Oonirici_MA_P5p_wt_mod <- Oonirici_MA_P5p_wt_mod[["X"]]
      beta_Oonirici_MA_P5p_wt_mod <- Oonirici_MA_P5p_wt_mod[["Sol"]][,c(1:5)]
      vf_Oonirici_MA_P5p_wt_mod   <- apply(beta_Oonirici_MA_P5p_wt_mod, 1, function(b) {var(as.vector(X_Oonirici_MA_P5p_wt_mod %*% b))}) 
      mean(vf_Oonirici_MA_P5p_wt_mod) 
      
      h2_liab_Oonirici_MA_P5p_wt_mod <- va_liab_Oonirici_MA_P5p_wt_mod / (vlat_Oonirici_MA_P5p_wt_mod + vf_Oonirici_MA_P5p_wt_mod)
      mean(h2_liab_Oonirici_MA_P5p_wt_mod) 
      posterior.mode(h2_liab_Oonirici_MA_P5p_wt_mod)	
      median(h2_liab_Oonirici_MA_P5p_wt_mod)		
      HPDinterval(h2_liab_Oonirici_MA_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_MA_P5p_wt_mod <- ((rowMeans(Oonirici_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_MA_P5p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_MA_P5p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Oonirici_MA_P5p_wt_mod <- (va_liab_Oonirici_MA_P5p_wt_mod/2) / (trait_mean_liab_Oonirici_MA_P5p_wt_mod)^2
    mean(Evol_liab_Oonirici_MA_P5p_wt_mod)
    
    #Oonirici_MA_P5p_wt_mod data scale
    {
      
      predict_Oonirici_MA_P5p_wt_mod <- map(1:nrow(Oonirici_MA_P5p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_MA_P5p_wt_mod %*% Oonirici_MA_P5p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_MA_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_Oonirici_MA_P5p_wt_mod,
                      var.a = Oonirici_MA_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_MA_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_MA_P5p_wt_mod <- data_Oonirici_MA_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_Oonirici_MA_P5p_wt_mod <- data_Oonirici_MA_P5p_wt_mod[["mean.obs"]]
      va_data_Oonirici_MA_P5p_wt_mod <- data_Oonirici_MA_P5p_wt_mod[["var.a.obs"]]
      vp_data_Oonirici_MA_P5p_wt_mod <- data_Oonirici_MA_P5p_wt_mod[["var.obs"]]
      
      Evol_data_Oonirici_MA_P5p_wt_mod <- (va_data_Oonirici_MA_P5p_wt_mod/2) / (trait_mean_data_Oonirici_MA_P5p_wt_mod)^2
      
      mean(h2_data_Oonirici_MA_P5p_wt_mod)
      mean(trait_mean_data_Oonirici_MA_P5p_wt_mod)
      mean(va_data_Oonirici_MA_P5p_wt_mod)
      mean(vp_data_Oonirici_MA_P5p_wt_mod)
      mean(Evol_data_Oonirici_MA_P5p_wt_mod)
      
    }
    
  }
  
  #Oonirici_Vm_P5p_wt_mod 
  {
    
    Oonirici_Vm_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Treatment:Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_Oonirici_data,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)            
    
    saveRDS(Oonirici_Vm_P5p_wt_mod, file = "Oonirici_Vm_P5p_wt_mod.rds")
    Oonirici_Vm_P5p_wt_mod <- readRDS("Oonirici_Vm_P5p_wt_mod.rds")
    
    summary(Oonirici_Vm_P5p_wt_mod) 
    plotTrace(Oonirici_Vm_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("Oonirici_Vm_P5p_wt_mod.pdf")
    plot(Oonirici_Vm_P5p_wt_mod[["Sol"]][,c(1:7)]) 
    plot(Oonirici_Vm_P5p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_Vm_P5p_wt_mod[["Sol"]][,c(1:7)]) #
    heidel.diag(Oonirici_Vm_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_Vm_P5p_wt_mod[["Sol"]][,c(1:7)])
    #autocorr.plot(Oonirici_Vm_P5p_wt_mod[["Sol"]][,c(1:7)]) #
    autocorr.diag(Oonirici_Vm_P5p_wt_mod[["VCV"]])
    #autocorr.plot(Oonirici_Vm_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_Vm_P5p_wt_mod[["Sol"]][,c(1:7)]) # 
    effectiveSize(Oonirici_Vm_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_Vm_P5p_wt_mod <- Oonirici_Vm_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_Vm_P5p_wt_mod <- Oonirici_Vm_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_Oonirici_Vm_P5p_wt_mod <- rowSums(Oonirici_Vm_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_Oonirici_Vm_P5p_wt_mod) 
      HPDinterval(va_liab_Oonirici_Vm_P5p_wt_mod) 
      
      mean(vlat_Oonirici_Vm_P5p_wt_mod) 
      
      #variance of fixed effects
      X_Oonirici_Vm_P5p_wt_mod <- Oonirici_Vm_P5p_wt_mod[["X"]]
      beta_Oonirici_Vm_P5p_wt_mod <- Oonirici_Vm_P5p_wt_mod[["Sol"]][,c(1:7)]
      vf_Oonirici_Vm_P5p_wt_mod   <- apply(beta_Oonirici_Vm_P5p_wt_mod, 1, function(b) {var(as.vector(X_Oonirici_Vm_P5p_wt_mod %*% b))}) 
      mean(vf_Oonirici_Vm_P5p_wt_mod) 
      
      h2_liab_Oonirici_Vm_P5p_wt_mod <- va_liab_Oonirici_Vm_P5p_wt_mod / (vlat_Oonirici_Vm_P5p_wt_mod + vf_Oonirici_Vm_P5p_wt_mod)
      mean(h2_liab_Oonirici_Vm_P5p_wt_mod) 
      posterior.mode(h2_liab_Oonirici_Vm_P5p_wt_mod)	
      median(h2_liab_Oonirici_Vm_P5p_wt_mod)		
      HPDinterval(h2_liab_Oonirici_Vm_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    mean(rowMeans(Oonirici_Vm_P5p_wt_mod[["Sol"]][,c(1:4)])) 
    mean(rowMeans(Oonirici_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P5p_wt_mod[["Sol"]][,5]) 
    mean(rowMeans(Oonirici_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P5p_wt_mod[["Sol"]][,6]) 
    mean(rowMeans(Oonirici_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P5p_wt_mod[["Sol"]][,7]) 
    
    trait_mean_liab_Oonirici_Vm_P5p_wt_mod <- ((rowMeans(Oonirici_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P5p_wt_mod[["Sol"]][,5]) + (rowMeans(Oonirici_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P5p_wt_mod[["Sol"]][,6]) + (rowMeans(Oonirici_Vm_P5p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P5p_wt_mod[["Sol"]][,7]))/4)
    
    Evol_liab_Oonirici_Vm_P5p_wt_mod <- (va_liab_Oonirici_Vm_P5p_wt_mod/2) / (trait_mean_liab_Oonirici_Vm_P5p_wt_mod)^2
    mean(Evol_liab_Oonirici_Vm_P5p_wt_mod)
    
    #Oonirici_Vm_P5p_wt_mod data scale
    {
      
      predict_Oonirici_Vm_P5p_wt_mod <- map(1:nrow(Oonirici_Vm_P5p_wt_mod[["Sol"]][,c(1:7)]), ~ as.vector(X_Oonirici_Vm_P5p_wt_mod %*% Oonirici_Vm_P5p_wt_mod[["Sol"]][,c(1:7)][., ]))
      
      data_Oonirici_Vm_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_Oonirici_Vm_P5p_wt_mod,
                      var.a = Oonirici_Vm_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_Vm_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_Vm_P5p_wt_mod <- data_Oonirici_Vm_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_Oonirici_Vm_P5p_wt_mod <- data_Oonirici_Vm_P5p_wt_mod[["mean.obs"]]
      va_data_Oonirici_Vm_P5p_wt_mod <- data_Oonirici_Vm_P5p_wt_mod[["var.a.obs"]]
      vp_data_Oonirici_Vm_P5p_wt_mod <- data_Oonirici_Vm_P5p_wt_mod[["var.obs"]]
      
      Evol_data_Oonirici_Vm_P5p_wt_mod <- (va_data_Oonirici_Vm_P5p_wt_mod/2) / (trait_mean_data_Oonirici_Vm_P5p_wt_mod)^2
      
      mean(h2_data_Oonirici_Vm_P5p_wt_mod)
      mean(trait_mean_data_Oonirici_Vm_P5p_wt_mod)
      mean(va_data_Oonirici_Vm_P5p_wt_mod)
      mean(vp_data_Oonirici_Vm_P5p_wt_mod)
      mean(Evol_data_Oonirici_Vm_P5p_wt_mod)
      
    }
    
  }
  
  
 
  
  ## Oonirici P6p ----
  
  
  #Oonirici_CONTROL_P6p_wt_mod 
  {
    
    Oonirici_CONTROL_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Ancestral -1,
                                            random      = ~ LineB + BlockRep,
                                            family      = "threshold",
                                            data        = Vm_Oonirici_bi_CONTROL,
                                            prior       = prior_bi_block,
                                            nitt        = 1260000,       
                                            thin        = 500,           
                                            burnin      = 10000,
                                            trunc       = TRUE,
                                            pr          = TRUE,
                                            pl          = TRUE)            
    
    saveRDS(Oonirici_CONTROL_P6p_wt_mod, file = "Oonirici_CONTROL_P6p_wt_mod.rds")
    Oonirici_CONTROL_P6p_wt_mod <- readRDS("Oonirici_CONTROL_P6p_wt_mod.rds")
    
    summary(Oonirici_CONTROL_P6p_wt_mod) 
    plotTrace(Oonirici_CONTROL_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Oonirici_CONTROL_P6p_wt_mod.pdf")
    plot(Oonirici_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Oonirici_CONTROL_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_CONTROL_P6p_wt_mod[["VCV"]])
    #autocorr.plot(Oonirici_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_CONTROL_P6p_wt_mod <- Oonirici_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_CONTROL_P6p_wt_mod <- Oonirici_CONTROL_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_Oonirici_CONTROL_P6p_wt_mod <- rowSums(Oonirici_CONTROL_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_Oonirici_CONTROL_P6p_wt_mod) 
      HPDinterval(va_liab_Oonirici_CONTROL_P6p_wt_mod) 
      
      mean(vlat_Oonirici_CONTROL_P6p_wt_mod) 
      
      #variance of fixed effects
      X_Oonirici_CONTROL_P6p_wt_mod <- Oonirici_CONTROL_P6p_wt_mod[["X"]]
      beta_Oonirici_CONTROL_P6p_wt_mod <- Oonirici_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]
      vf_Oonirici_CONTROL_P6p_wt_mod   <- apply(beta_Oonirici_CONTROL_P6p_wt_mod, 1, function(b) {var(as.vector(X_Oonirici_CONTROL_P6p_wt_mod %*% b))}) 
      mean(vf_Oonirici_CONTROL_P6p_wt_mod) 
      
      h2_liab_Oonirici_CONTROL_P6p_wt_mod <- va_liab_Oonirici_CONTROL_P6p_wt_mod / (vlat_Oonirici_CONTROL_P6p_wt_mod + vf_Oonirici_CONTROL_P6p_wt_mod)
      mean(h2_liab_Oonirici_CONTROL_P6p_wt_mod) 
      posterior.mode(h2_liab_Oonirici_CONTROL_P6p_wt_mod)	
      median(h2_liab_Oonirici_CONTROL_P6p_wt_mod)		
      HPDinterval(h2_liab_Oonirici_CONTROL_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_CONTROL_P6p_wt_mod <- ((rowMeans(Oonirici_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_CONTROL_P6p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Oonirici_CONTROL_P6p_wt_mod <- (va_liab_Oonirici_CONTROL_P6p_wt_mod/2) / (trait_mean_liab_Oonirici_CONTROL_P6p_wt_mod)^2
    mean(Evol_liab_Oonirici_CONTROL_P6p_wt_mod)
    
    
    #Oonirici_CONTROL_P6p_wt_mod data scale
    {
      
      predict_Oonirici_CONTROL_P6p_wt_mod <- map(1:nrow(Oonirici_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_CONTROL_P6p_wt_mod %*% Oonirici_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_CONTROL_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_Oonirici_CONTROL_P6p_wt_mod,
                      var.a = Oonirici_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_CONTROL_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_CONTROL_P6p_wt_mod <- data_Oonirici_CONTROL_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_Oonirici_CONTROL_P6p_wt_mod <- data_Oonirici_CONTROL_P6p_wt_mod[["mean.obs"]]
      va_data_Oonirici_CONTROL_P6p_wt_mod <- data_Oonirici_CONTROL_P6p_wt_mod[["var.a.obs"]]
      vp_data_Oonirici_CONTROL_P6p_wt_mod <- data_Oonirici_CONTROL_P6p_wt_mod[["var.obs"]]
      
      Evol_data_Oonirici_CONTROL_P6p_wt_mod <- (va_data_Oonirici_CONTROL_P6p_wt_mod/2) / (trait_mean_data_Oonirici_CONTROL_P6p_wt_mod)^2
      
      mean(h2_data_Oonirici_CONTROL_P6p_wt_mod) 
      mean(trait_mean_data_Oonirici_CONTROL_P6p_wt_mod)
      mean(va_data_Oonirici_CONTROL_P6p_wt_mod) 
      mean(vp_data_Oonirici_CONTROL_P6p_wt_mod)
      mean(Evol_data_Oonirici_CONTROL_P6p_wt_mod)
      
    }
    
  }
  
  #Oonirici_MA_P6p_wt_mod 
  {
    
    Oonirici_MA_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_Oonirici_bi_MA,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)         
    
    saveRDS(Oonirici_MA_P6p_wt_mod, file = "Oonirici_MA_P6p_wt_mod.rds")
    Oonirici_MA_P6p_wt_mod <- readRDS("Oonirici_MA_P6p_wt_mod.rds")
    
    summary(Oonirici_MA_P6p_wt_mod) 
    #plot(Oonirici_MA_P6p_wt_mod)
    
    # traces and posterior densities
    pdf("Oonirici_MA_P6p_wt_mod.pdf")
    plot(Oonirici_MA_P6p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Oonirici_MA_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_MA_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_MA_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_MA_P6p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_MA_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_MA_P6p_wt_mod[["VCV"]])
    #autocorr.plot(Oonirici_MA_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_MA_P6p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_MA_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_MA_P6p_wt_mod <- Oonirici_MA_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_MA_P6p_wt_mod <- Oonirici_MA_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_Oonirici_MA_P6p_wt_mod <- rowSums(Oonirici_MA_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_Oonirici_MA_P6p_wt_mod) 
      HPDinterval(va_liab_Oonirici_MA_P6p_wt_mod) 
      
      mean(vlat_Oonirici_MA_P6p_wt_mod) 
      
      #variance of fixed effects
      X_Oonirici_MA_P6p_wt_mod <- Oonirici_MA_P6p_wt_mod[["X"]]
      beta_Oonirici_MA_P6p_wt_mod <- Oonirici_MA_P6p_wt_mod[["Sol"]][,c(1:5)]
      vf_Oonirici_MA_P6p_wt_mod   <- apply(beta_Oonirici_MA_P6p_wt_mod, 1, function(b) {var(as.vector(X_Oonirici_MA_P6p_wt_mod %*% b))}) 
      mean(vf_Oonirici_MA_P6p_wt_mod) 
      
      h2_liab_Oonirici_MA_P6p_wt_mod <- va_liab_Oonirici_MA_P6p_wt_mod / (vlat_Oonirici_MA_P6p_wt_mod + vf_Oonirici_MA_P6p_wt_mod)
      mean(h2_liab_Oonirici_MA_P6p_wt_mod) 
      posterior.mode(h2_liab_Oonirici_MA_P6p_wt_mod)	
      median(h2_liab_Oonirici_MA_P6p_wt_mod)		
      HPDinterval(h2_liab_Oonirici_MA_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_MA_P6p_wt_mod <- ((rowMeans(Oonirici_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_MA_P6p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_MA_P6p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Oonirici_MA_P6p_wt_mod <- (va_liab_Oonirici_MA_P6p_wt_mod/2) / (trait_mean_liab_Oonirici_MA_P6p_wt_mod)^2
    mean(Evol_liab_Oonirici_MA_P6p_wt_mod)
    
    #Oonirici_MA_P6p_wt_mod data scale
    {
      
      predict_Oonirici_MA_P6p_wt_mod <- map(1:nrow(Oonirici_MA_P6p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_MA_P6p_wt_mod %*% Oonirici_MA_P6p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_MA_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_Oonirici_MA_P6p_wt_mod,
                      var.a = Oonirici_MA_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_MA_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_MA_P6p_wt_mod <- data_Oonirici_MA_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_Oonirici_MA_P6p_wt_mod <- data_Oonirici_MA_P6p_wt_mod[["mean.obs"]]
      va_data_Oonirici_MA_P6p_wt_mod <- data_Oonirici_MA_P6p_wt_mod[["var.a.obs"]]
      vp_data_Oonirici_MA_P6p_wt_mod <- data_Oonirici_MA_P6p_wt_mod[["var.obs"]]
      
      Evol_data_Oonirici_MA_P6p_wt_mod <- (va_data_Oonirici_MA_P6p_wt_mod/2) / (trait_mean_data_Oonirici_MA_P6p_wt_mod)^2
      
      mean(h2_data_Oonirici_MA_P6p_wt_mod)
      mean(trait_mean_data_Oonirici_MA_P6p_wt_mod)
      mean(va_data_Oonirici_MA_P6p_wt_mod)
      mean(vp_data_Oonirici_MA_P6p_wt_mod)
      mean(Evol_data_Oonirici_MA_P6p_wt_mod)
      
    }
    
  }
  
  #Oonirici_Vm_P6p_wt_mod 
  {
    
    Oonirici_Vm_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Treatment:Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_Oonirici_data,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)            
    
    saveRDS(Oonirici_Vm_P6p_wt_mod, file = "Oonirici_Vm_P6p_wt_mod.rds")
    Oonirici_Vm_P6p_wt_mod <- readRDS("Oonirici_Vm_P6p_wt_mod.rds")
    
    summary(Oonirici_Vm_P6p_wt_mod) 
    plotTrace(Oonirici_Vm_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("Oonirici_Vm_P6p_wt_mod.pdf")
    plot(Oonirici_Vm_P6p_wt_mod[["Sol"]][,c(1:7)]) 
    plot(Oonirici_Vm_P6p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_Vm_P6p_wt_mod[["Sol"]][,c(1:7)]) #
    heidel.diag(Oonirici_Vm_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_Vm_P6p_wt_mod[["Sol"]][,c(1:7)])
    #autocorr.plot(Oonirici_Vm_P6p_wt_mod[["Sol"]][,c(1:7)]) #
    autocorr.diag(Oonirici_Vm_P6p_wt_mod[["VCV"]])
    #autocorr.plot(Oonirici_Vm_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_Vm_P6p_wt_mod[["Sol"]][,c(1:7)]) # 
    effectiveSize(Oonirici_Vm_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_Vm_P6p_wt_mod <- Oonirici_Vm_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_Vm_P6p_wt_mod <- Oonirici_Vm_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_Oonirici_Vm_P6p_wt_mod <- rowSums(Oonirici_Vm_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_Oonirici_Vm_P6p_wt_mod) 
      HPDinterval(va_liab_Oonirici_Vm_P6p_wt_mod) 
      
      mean(vlat_Oonirici_Vm_P6p_wt_mod) 
      
      #variance of fixed effects
      X_Oonirici_Vm_P6p_wt_mod <- Oonirici_Vm_P6p_wt_mod[["X"]]
      beta_Oonirici_Vm_P6p_wt_mod <- Oonirici_Vm_P6p_wt_mod[["Sol"]][,c(1:7)]
      vf_Oonirici_Vm_P6p_wt_mod   <- apply(beta_Oonirici_Vm_P6p_wt_mod, 1, function(b) {var(as.vector(X_Oonirici_Vm_P6p_wt_mod %*% b))}) 
      mean(vf_Oonirici_Vm_P6p_wt_mod) 
      
      h2_liab_Oonirici_Vm_P6p_wt_mod <- va_liab_Oonirici_Vm_P6p_wt_mod / (vlat_Oonirici_Vm_P6p_wt_mod + vf_Oonirici_Vm_P6p_wt_mod)
      mean(h2_liab_Oonirici_Vm_P6p_wt_mod) 
      posterior.mode(h2_liab_Oonirici_Vm_P6p_wt_mod)	
      median(h2_liab_Oonirici_Vm_P6p_wt_mod)		
      HPDinterval(h2_liab_Oonirici_Vm_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    mean(rowMeans(Oonirici_Vm_P6p_wt_mod[["Sol"]][,c(1:4)])) 
    mean(rowMeans(Oonirici_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P6p_wt_mod[["Sol"]][,5]) 
    mean(rowMeans(Oonirici_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P6p_wt_mod[["Sol"]][,6]) 
    mean(rowMeans(Oonirici_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P6p_wt_mod[["Sol"]][,7]) 
    
    trait_mean_liab_Oonirici_Vm_P6p_wt_mod <- ((rowMeans(Oonirici_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P6p_wt_mod[["Sol"]][,5]) + (rowMeans(Oonirici_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P6p_wt_mod[["Sol"]][,6]) + (rowMeans(Oonirici_Vm_P6p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P6p_wt_mod[["Sol"]][,7]))/4)
    
    Evol_liab_Oonirici_Vm_P6p_wt_mod <- (va_liab_Oonirici_Vm_P6p_wt_mod/2) / (trait_mean_liab_Oonirici_Vm_P6p_wt_mod)^2
    mean(Evol_liab_Oonirici_Vm_P6p_wt_mod)
    
    #Oonirici_Vm_P6p_wt_mod data scale
    {
      
      predict_Oonirici_Vm_P6p_wt_mod <- map(1:nrow(Oonirici_Vm_P6p_wt_mod[["Sol"]][,c(1:7)]), ~ as.vector(X_Oonirici_Vm_P6p_wt_mod %*% Oonirici_Vm_P6p_wt_mod[["Sol"]][,c(1:7)][., ]))
      
      data_Oonirici_Vm_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_Oonirici_Vm_P6p_wt_mod,
                      var.a = Oonirici_Vm_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_Vm_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_Vm_P6p_wt_mod <- data_Oonirici_Vm_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_Oonirici_Vm_P6p_wt_mod <- data_Oonirici_Vm_P6p_wt_mod[["mean.obs"]]
      va_data_Oonirici_Vm_P6p_wt_mod <- data_Oonirici_Vm_P6p_wt_mod[["var.a.obs"]]
      vp_data_Oonirici_Vm_P6p_wt_mod <- data_Oonirici_Vm_P6p_wt_mod[["var.obs"]]
      
      Evol_data_Oonirici_Vm_P6p_wt_mod <- (va_data_Oonirici_Vm_P6p_wt_mod/2) / (trait_mean_data_Oonirici_Vm_P6p_wt_mod)^2
      
      mean(h2_data_Oonirici_Vm_P6p_wt_mod)
      mean(trait_mean_data_Oonirici_Vm_P6p_wt_mod)
      mean(va_data_Oonirici_Vm_P6p_wt_mod)
      mean(vp_data_Oonirici_Vm_P6p_wt_mod)
      mean(Evol_data_Oonirici_Vm_P6p_wt_mod)
      
    }
    
  }
  
  
 
  
  ## Oonirici P7p ----
  
  #Oonirici_CONTROL_P7p_wt_mod 
  {
    
    Oonirici_CONTROL_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Ancestral -1,
                                            random      = ~ LineB + BlockRep,
                                            family      = "threshold",
                                            data        = Vm_Oonirici_bi_CONTROL,
                                            prior       = prior_bi_block,
                                            nitt        = 1260000,       
                                            thin        = 500,           
                                            burnin      = 10000,
                                            trunc       = TRUE,
                                            pr          = TRUE,
                                            pl          = TRUE)            
    
    saveRDS(Oonirici_CONTROL_P7p_wt_mod, file = "Oonirici_CONTROL_P7p_wt_mod.rds")
    Oonirici_CONTROL_P7p_wt_mod <- readRDS("Oonirici_CONTROL_P7p_wt_mod.rds")
    
    summary(Oonirici_CONTROL_P7p_wt_mod) 
    plotTrace(Oonirici_CONTROL_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Oonirici_CONTROL_P7p_wt_mod.pdf")
    plot(Oonirici_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Oonirici_CONTROL_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_CONTROL_P7p_wt_mod[["VCV"]])
    #autocorr.plot(Oonirici_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_CONTROL_P7p_wt_mod <- Oonirici_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_CONTROL_P7p_wt_mod <- Oonirici_CONTROL_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_Oonirici_CONTROL_P7p_wt_mod <- rowSums(Oonirici_CONTROL_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_Oonirici_CONTROL_P7p_wt_mod) 
      HPDinterval(va_liab_Oonirici_CONTROL_P7p_wt_mod) 
      
      mean(vlat_Oonirici_CONTROL_P7p_wt_mod) 
      
      #variance of fixed effects
      X_Oonirici_CONTROL_P7p_wt_mod <- Oonirici_CONTROL_P7p_wt_mod[["X"]]
      beta_Oonirici_CONTROL_P7p_wt_mod <- Oonirici_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]
      vf_Oonirici_CONTROL_P7p_wt_mod   <- apply(beta_Oonirici_CONTROL_P7p_wt_mod, 1, function(b) {var(as.vector(X_Oonirici_CONTROL_P7p_wt_mod %*% b))}) 
      mean(vf_Oonirici_CONTROL_P7p_wt_mod) 
      
      h2_liab_Oonirici_CONTROL_P7p_wt_mod <- va_liab_Oonirici_CONTROL_P7p_wt_mod / (vlat_Oonirici_CONTROL_P7p_wt_mod + vf_Oonirici_CONTROL_P7p_wt_mod)
      mean(h2_liab_Oonirici_CONTROL_P7p_wt_mod) 
      posterior.mode(h2_liab_Oonirici_CONTROL_P7p_wt_mod)	
      median(h2_liab_Oonirici_CONTROL_P7p_wt_mod)		
      HPDinterval(h2_liab_Oonirici_CONTROL_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_CONTROL_P7p_wt_mod <- ((rowMeans(Oonirici_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_CONTROL_P7p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Oonirici_CONTROL_P7p_wt_mod <- (va_liab_Oonirici_CONTROL_P7p_wt_mod/2) / (trait_mean_liab_Oonirici_CONTROL_P7p_wt_mod)^2
    mean(Evol_liab_Oonirici_CONTROL_P7p_wt_mod)
    
    
    #Oonirici_CONTROL_P7p_wt_mod data scale
    {
      
      predict_Oonirici_CONTROL_P7p_wt_mod <- map(1:nrow(Oonirici_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_CONTROL_P7p_wt_mod %*% Oonirici_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_CONTROL_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_Oonirici_CONTROL_P7p_wt_mod,
                      var.a = Oonirici_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_CONTROL_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_CONTROL_P7p_wt_mod <- data_Oonirici_CONTROL_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_Oonirici_CONTROL_P7p_wt_mod <- data_Oonirici_CONTROL_P7p_wt_mod[["mean.obs"]]
      va_data_Oonirici_CONTROL_P7p_wt_mod <- data_Oonirici_CONTROL_P7p_wt_mod[["var.a.obs"]]
      vp_data_Oonirici_CONTROL_P7p_wt_mod <- data_Oonirici_CONTROL_P7p_wt_mod[["var.obs"]]
      
      Evol_data_Oonirici_CONTROL_P7p_wt_mod <- (va_data_Oonirici_CONTROL_P7p_wt_mod/2) / (trait_mean_data_Oonirici_CONTROL_P7p_wt_mod)^2
      
      mean(h2_data_Oonirici_CONTROL_P7p_wt_mod) 
      mean(trait_mean_data_Oonirici_CONTROL_P7p_wt_mod)
      mean(va_data_Oonirici_CONTROL_P7p_wt_mod) 
      mean(vp_data_Oonirici_CONTROL_P7p_wt_mod)
      mean(Evol_data_Oonirici_CONTROL_P7p_wt_mod)
      
    }
    
  }
  
  #Oonirici_MA_P7p_wt_mod 
  {
    
    Oonirici_MA_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_Oonirici_bi_MA,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)         
    
    saveRDS(Oonirici_MA_P7p_wt_mod, file = "Oonirici_MA_P7p_wt_mod.rds")
    Oonirici_MA_P7p_wt_mod <- readRDS("Oonirici_MA_P7p_wt_mod.rds")
    
    summary(Oonirici_MA_P7p_wt_mod) 
    #plot(Oonirici_MA_P7p_wt_mod)
    
    # traces and posterior densities
    pdf("Oonirici_MA_P7p_wt_mod.pdf")
    plot(Oonirici_MA_P7p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Oonirici_MA_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_MA_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_MA_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_MA_P7p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_MA_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_MA_P7p_wt_mod[["VCV"]])
    #autocorr.plot(Oonirici_MA_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_MA_P7p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_MA_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_MA_P7p_wt_mod <- Oonirici_MA_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_MA_P7p_wt_mod <- Oonirici_MA_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_Oonirici_MA_P7p_wt_mod <- rowSums(Oonirici_MA_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_Oonirici_MA_P7p_wt_mod) 
      HPDinterval(va_liab_Oonirici_MA_P7p_wt_mod) 
      
      mean(vlat_Oonirici_MA_P7p_wt_mod) 
      
      #variance of fixed effects
      X_Oonirici_MA_P7p_wt_mod <- Oonirici_MA_P7p_wt_mod[["X"]]
      beta_Oonirici_MA_P7p_wt_mod <- Oonirici_MA_P7p_wt_mod[["Sol"]][,c(1:5)]
      vf_Oonirici_MA_P7p_wt_mod   <- apply(beta_Oonirici_MA_P7p_wt_mod, 1, function(b) {var(as.vector(X_Oonirici_MA_P7p_wt_mod %*% b))}) 
      mean(vf_Oonirici_MA_P7p_wt_mod) 
      
      h2_liab_Oonirici_MA_P7p_wt_mod <- va_liab_Oonirici_MA_P7p_wt_mod / (vlat_Oonirici_MA_P7p_wt_mod + vf_Oonirici_MA_P7p_wt_mod)
      mean(h2_liab_Oonirici_MA_P7p_wt_mod) 
      posterior.mode(h2_liab_Oonirici_MA_P7p_wt_mod)	
      median(h2_liab_Oonirici_MA_P7p_wt_mod)		
      HPDinterval(h2_liab_Oonirici_MA_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_MA_P7p_wt_mod <- ((rowMeans(Oonirici_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_MA_P7p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_MA_P7p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Oonirici_MA_P7p_wt_mod <- (va_liab_Oonirici_MA_P7p_wt_mod/2) / (trait_mean_liab_Oonirici_MA_P7p_wt_mod)^2
    mean(Evol_liab_Oonirici_MA_P7p_wt_mod)
    
    #Oonirici_MA_P7p_wt_mod data scale
    {
      
      predict_Oonirici_MA_P7p_wt_mod <- map(1:nrow(Oonirici_MA_P7p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_MA_P7p_wt_mod %*% Oonirici_MA_P7p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_MA_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_Oonirici_MA_P7p_wt_mod,
                      var.a = Oonirici_MA_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_MA_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_MA_P7p_wt_mod <- data_Oonirici_MA_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_Oonirici_MA_P7p_wt_mod <- data_Oonirici_MA_P7p_wt_mod[["mean.obs"]]
      va_data_Oonirici_MA_P7p_wt_mod <- data_Oonirici_MA_P7p_wt_mod[["var.a.obs"]]
      vp_data_Oonirici_MA_P7p_wt_mod <- data_Oonirici_MA_P7p_wt_mod[["var.obs"]]
      
      Evol_data_Oonirici_MA_P7p_wt_mod <- (va_data_Oonirici_MA_P7p_wt_mod/2) / (trait_mean_data_Oonirici_MA_P7p_wt_mod)^2
      
      mean(h2_data_Oonirici_MA_P7p_wt_mod)
      mean(trait_mean_data_Oonirici_MA_P7p_wt_mod)
      mean(va_data_Oonirici_MA_P7p_wt_mod)
      mean(vp_data_Oonirici_MA_P7p_wt_mod)
      mean(Evol_data_Oonirici_MA_P7p_wt_mod)
      
    }
    
  }
  
  #Oonirici_Vm_P7p_wt_mod 
  {
    
    Oonirici_Vm_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Treatment:Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vm_Oonirici_data,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)            
    
    saveRDS(Oonirici_Vm_P7p_wt_mod, file = "Oonirici_Vm_P7p_wt_mod.rds")
    Oonirici_Vm_P7p_wt_mod <- readRDS("Oonirici_Vm_P7p_wt_mod.rds")
    
    summary(Oonirici_Vm_P7p_wt_mod) 
    plotTrace(Oonirici_Vm_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    pdf("Oonirici_Vm_P7p_wt_mod.pdf")
    plot(Oonirici_Vm_P7p_wt_mod[["Sol"]][,c(1:7)]) 
    plot(Oonirici_Vm_P7p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_Vm_P7p_wt_mod[["Sol"]][,c(1:7)]) #
    heidel.diag(Oonirici_Vm_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_Vm_P7p_wt_mod[["Sol"]][,c(1:7)])
    #autocorr.plot(Oonirici_Vm_P7p_wt_mod[["Sol"]][,c(1:7)]) #
    autocorr.diag(Oonirici_Vm_P7p_wt_mod[["VCV"]])
    #autocorr.plot(Oonirici_Vm_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_Vm_P7p_wt_mod[["Sol"]][,c(1:7)]) # 
    effectiveSize(Oonirici_Vm_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_Vm_P7p_wt_mod <- Oonirici_Vm_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_Vm_P7p_wt_mod <- Oonirici_Vm_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_Oonirici_Vm_P7p_wt_mod <- rowSums(Oonirici_Vm_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_Oonirici_Vm_P7p_wt_mod) 
      HPDinterval(va_liab_Oonirici_Vm_P7p_wt_mod) 
      
      mean(vlat_Oonirici_Vm_P7p_wt_mod) 
      
      #variance of fixed effects
      X_Oonirici_Vm_P7p_wt_mod <- Oonirici_Vm_P7p_wt_mod[["X"]]
      beta_Oonirici_Vm_P7p_wt_mod <- Oonirici_Vm_P7p_wt_mod[["Sol"]][,c(1:7)]
      vf_Oonirici_Vm_P7p_wt_mod   <- apply(beta_Oonirici_Vm_P7p_wt_mod, 1, function(b) {var(as.vector(X_Oonirici_Vm_P7p_wt_mod %*% b))}) 
      mean(vf_Oonirici_Vm_P7p_wt_mod) 
      
      h2_liab_Oonirici_Vm_P7p_wt_mod <- va_liab_Oonirici_Vm_P7p_wt_mod / (vlat_Oonirici_Vm_P7p_wt_mod + vf_Oonirici_Vm_P7p_wt_mod)
      mean(h2_liab_Oonirici_Vm_P7p_wt_mod) 
      posterior.mode(h2_liab_Oonirici_Vm_P7p_wt_mod)	
      median(h2_liab_Oonirici_Vm_P7p_wt_mod)		
      HPDinterval(h2_liab_Oonirici_Vm_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    mean(rowMeans(Oonirici_Vm_P7p_wt_mod[["Sol"]][,c(1:4)])) 
    mean(rowMeans(Oonirici_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P7p_wt_mod[["Sol"]][,5]) 
    mean(rowMeans(Oonirici_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P7p_wt_mod[["Sol"]][,6]) 
    mean(rowMeans(Oonirici_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P7p_wt_mod[["Sol"]][,7]) 
    
    trait_mean_liab_Oonirici_Vm_P7p_wt_mod <- ((rowMeans(Oonirici_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Oonirici_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P7p_wt_mod[["Sol"]][,5]) + (rowMeans(Oonirici_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P7p_wt_mod[["Sol"]][,6]) + (rowMeans(Oonirici_Vm_P7p_wt_mod[["Sol"]][,c(1:4)]) + Oonirici_Vm_P7p_wt_mod[["Sol"]][,7]))/4)
    
    Evol_liab_Oonirici_Vm_P7p_wt_mod <- (va_liab_Oonirici_Vm_P7p_wt_mod/2) / (trait_mean_liab_Oonirici_Vm_P7p_wt_mod)^2
    mean(Evol_liab_Oonirici_Vm_P7p_wt_mod)
    
    #Oonirici_Vm_P7p_wt_mod data scale
    {
      
      predict_Oonirici_Vm_P7p_wt_mod <- map(1:nrow(Oonirici_Vm_P7p_wt_mod[["Sol"]][,c(1:7)]), ~ as.vector(X_Oonirici_Vm_P7p_wt_mod %*% Oonirici_Vm_P7p_wt_mod[["Sol"]][,c(1:7)][., ]))
      
      data_Oonirici_Vm_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_Oonirici_Vm_P7p_wt_mod,
                      var.a = Oonirici_Vm_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_Vm_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_Vm_P7p_wt_mod <- data_Oonirici_Vm_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_Oonirici_Vm_P7p_wt_mod <- data_Oonirici_Vm_P7p_wt_mod[["mean.obs"]]
      va_data_Oonirici_Vm_P7p_wt_mod <- data_Oonirici_Vm_P7p_wt_mod[["var.a.obs"]]
      vp_data_Oonirici_Vm_P7p_wt_mod <- data_Oonirici_Vm_P7p_wt_mod[["var.obs"]]
      
      Evol_data_Oonirici_Vm_P7p_wt_mod <- (va_data_Oonirici_Vm_P7p_wt_mod/2) / (trait_mean_data_Oonirici_Vm_P7p_wt_mod)^2
      
      mean(h2_data_Oonirici_Vm_P7p_wt_mod)
      mean(trait_mean_data_Oonirici_Vm_P7p_wt_mod)
      mean(va_data_Oonirici_Vm_P7p_wt_mod)
      mean(vp_data_Oonirici_Vm_P7p_wt_mod)
      mean(Evol_data_Oonirici_Vm_P7p_wt_mod)
      
    }
    
  }
  
  
 
}




##Vg_Oscheius_SSSS_vulva-wt

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
Vg_data <- subset(Vm_Vg_data, Treatment !="MA" )
Vg_Oscheius_data <- subset(Vg_data, Genus == "Oscheius" )
Vg_Oscheius_data$LineB <- ifelse(Vg_Oscheius_data$Treatment == "CONTROL" ,paste(Vg_Oscheius_data$Line,Vg_Oscheius_data$Block), paste(Vg_Oscheius_data$Line))
Vg_Oscheius_data$BlockRep <- paste(Vg_Oscheius_data$Block,Vg_Oscheius_data$Replicate)
Vg_Oscheius_data$Treatment <- as.factor(Vg_Oscheius_data$Treatment)
Vg_Oscheius_data$Observer <- as.factor(Vg_Oscheius_data$Observer)

Vg_Oscheius_data_woutCB <- subset(Vg_Oscheius_data, Observer !="CB") #Observer CB did not phenotype wild isolates of Oscheius

View(Vg_Oscheius_data_woutCB)
table(Vg_Oscheius_data_woutCB$Species)
table(Vg_Oscheius_data_woutCB$Ancestral)
table(Vg_Oscheius_data_woutCB$Observer)

Vg_Otipulae_data_woutCB <- subset(Vg_Oscheius_data_woutCB, Species =="O.tipulae")
Vg_Oonirici_data_woutCB <- subset(Vg_Oscheius_data_woutCB, Species =="O.onirici")



prior_bi_block <-
  list( R = list(V = 1, fix = 1),      # Fixing the "residual" variance to 1 because it is not identifiable in binary responses 
        G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1),
                 G2 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1)))





#Otipulae----
{
  Vg_Otipulae_bi_CONTROL_woutCB <- subset(Vg_Otipulae_data_woutCB, Treatment =="CONTROL")
  Vg_Otipulae_bi_WILD_woutCB <- subset(Vg_Otipulae_data_woutCB, Treatment =="WILD")
  table(Vg_Otipulae_bi_CONTROL_woutCB$Observer)
  table(Vg_Otipulae_bi_WILD_woutCB$Observer)
  
  ## Otipulae P3p ----
  
  
  #Otipulae_CONTROL_P3p_SSSS_mod_2 
  {
    
    Otipulae_CONTROL_P3p_SSSS_mod_2 <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer + Ancestral -1,
                                             random      = ~ LineB + BlockRep,
                                             family      = "threshold",
                                             data        = Vg_Otipulae_bi_CONTROL_woutCB,
                                             prior       = prior_bi_block,
                                             nitt        = 1260000,       
                                             thin        = 500,           
                                             burnin      = 10000,
                                             trunc       = TRUE,
                                             pr          = TRUE,
                                             pl          = TRUE)            
    
    saveRDS(Otipulae_CONTROL_P3p_SSSS_mod_2, file = "Otipulae_CONTROL_P3p_SSSS_mod_2.rds")
    Otipulae_CONTROL_P3p_SSSS_mod_2 <- readRDS("Otipulae_CONTROL_P3p_SSSS_mod_2.rds")
    
    summary(Otipulae_CONTROL_P3p_SSSS_mod_2) 
    #plotTrace(Otipulae_CONTROL_P3p_SSSS_mod_2$Sol)
    
    # traces and posterior densities
    
    pdf("Otipulae_CONTROL_P3p_SSSS_mod_2.pdf")
    plot(Otipulae_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:4)]) 
    plot(Otipulae_CONTROL_P3p_SSSS_mod_2[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:4)]) #
    heidel.diag(Otipulae_CONTROL_P3p_SSSS_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:4)])
    #autocorr.plot(Otipulae_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:4)]) #
    autocorr.diag(Otipulae_CONTROL_P3p_SSSS_mod_2[["VCV"]])
    #autocorr.plot(Otipulae_CONTROL_P3p_SSSS_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:4)]) # 
    effectiveSize(Otipulae_CONTROL_P3p_SSSS_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_CONTROL_P3p_SSSS_mod_2 <- Otipulae_CONTROL_P3p_SSSS_mod_2[["VCV"]][ , "LineB"]
      vr_Otipulae_CONTROL_P3p_SSSS_mod_2 <- Otipulae_CONTROL_P3p_SSSS_mod_2[["VCV"]][ , "units"]
      vlat_Otipulae_CONTROL_P3p_SSSS_mod_2 <- rowSums(Otipulae_CONTROL_P3p_SSSS_mod_2[["VCV"]])
      
      mean(va_liab_Otipulae_CONTROL_P3p_SSSS_mod_2) 
      HPDinterval(va_liab_Otipulae_CONTROL_P3p_SSSS_mod_2) 
      
      mean(vlat_Otipulae_CONTROL_P3p_SSSS_mod_2) 
      
      #variance of fixed effects
      X_Otipulae_CONTROL_P3p_SSSS_mod_2 <- Otipulae_CONTROL_P3p_SSSS_mod_2[["X"]]
      beta_Otipulae_CONTROL_P3p_SSSS_mod_2 <- Otipulae_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:4)]
      vf_Otipulae_CONTROL_P3p_SSSS_mod_2   <- apply(beta_Otipulae_CONTROL_P3p_SSSS_mod_2, 1, function(b) {var(as.vector(X_Otipulae_CONTROL_P3p_SSSS_mod_2 %*% b))}) 
      mean(vf_Otipulae_CONTROL_P3p_SSSS_mod_2) 
      
      h2_liab_Otipulae_CONTROL_P3p_SSSS_mod_2 <- va_liab_Otipulae_CONTROL_P3p_SSSS_mod_2 / (vlat_Otipulae_CONTROL_P3p_SSSS_mod_2 + vf_Otipulae_CONTROL_P3p_SSSS_mod_2)
      mean(h2_liab_Otipulae_CONTROL_P3p_SSSS_mod_2) 
      posterior.mode(h2_liab_Otipulae_CONTROL_P3p_SSSS_mod_2)	
      median(h2_liab_Otipulae_CONTROL_P3p_SSSS_mod_2)		
      HPDinterval(h2_liab_Otipulae_CONTROL_P3p_SSSS_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_CONTROL_P3p_SSSS_mod_2 <- ((rowMeans(Otipulae_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Otipulae_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Otipulae_CONTROL_P3p_SSSS_mod_2[["Sol"]][,4]))/2)
    
    Evol_liab_Otipulae_CONTROL_P3p_SSSS_mod_2 <- (va_liab_Otipulae_CONTROL_P3p_SSSS_mod_2/2) / (trait_mean_liab_Otipulae_CONTROL_P3p_SSSS_mod_2)^2
    mean(Evol_liab_Otipulae_CONTROL_P3p_SSSS_mod_2)
    
    
    #Otipulae_CONTROL_P3p_SSSS_mod_2 data scale
    {
      
      predict_Otipulae_CONTROL_P3p_SSSS_mod_2 <- map(1:nrow(Otipulae_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:4)]), ~ as.vector(X_Otipulae_CONTROL_P3p_SSSS_mod_2 %*% Otipulae_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:4)][., ]))
      
      data_Otipulae_CONTROL_P3p_SSSS_mod_2 <-
        pmap_dfr(list(predict = predict_Otipulae_CONTROL_P3p_SSSS_mod_2,
                      var.a = Otipulae_CONTROL_P3p_SSSS_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_CONTROL_P3p_SSSS_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_CONTROL_P3p_SSSS_mod_2 <- data_Otipulae_CONTROL_P3p_SSSS_mod_2[["h2.obs"]]
      trait_mean_data_Otipulae_CONTROL_P3p_SSSS_mod_2 <- data_Otipulae_CONTROL_P3p_SSSS_mod_2[["mean.obs"]]
      va_data_Otipulae_CONTROL_P3p_SSSS_mod_2 <- data_Otipulae_CONTROL_P3p_SSSS_mod_2[["var.a.obs"]]
      vp_data_Otipulae_CONTROL_P3p_SSSS_mod_2 <- data_Otipulae_CONTROL_P3p_SSSS_mod_2[["var.obs"]]
      
      Evol_data_Otipulae_CONTROL_P3p_SSSS_mod_2 <- (va_data_Otipulae_CONTROL_P3p_SSSS_mod_2/2) / (trait_mean_data_Otipulae_CONTROL_P3p_SSSS_mod_2)^2
      
      mean(h2_data_Otipulae_CONTROL_P3p_SSSS_mod_2) 
      mean(trait_mean_data_Otipulae_CONTROL_P3p_SSSS_mod_2)
      mean(va_data_Otipulae_CONTROL_P3p_SSSS_mod_2) 
      mean(vp_data_Otipulae_CONTROL_P3p_SSSS_mod_2)
      mean(Evol_data_Otipulae_CONTROL_P3p_SSSS_mod_2)
      
    }
    
  }
  
  #Otipulae_WILD_P3p_SSSS_mod 
  {
    
    Otipulae_WILD_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer -1,
                                           random      = ~ LineB + BlockRep,
                                           family      = "threshold",
                                           data        = Vg_Otipulae_bi_WILD,
                                           prior       = prior_bi_block,
                                           nitt        = 1260000,       
                                           thin        = 500,           
                                           burnin      = 10000,
                                           trunc       = TRUE,
                                           pr          = TRUE,
                                           pl          = TRUE)        
    
    
    
    saveRDS(Otipulae_WILD_P3p_SSSS_mod, file = "Otipulae_WILD_P3p_SSSS_mod.rds")
    Otipulae_WILD_P3p_SSSS_mod <- readRDS("Otipulae_WILD_P3p_SSSS_mod.rds")
    
    summary(Otipulae_WILD_P3p_SSSS_mod) 
    #plot(Otipulae_WILD_P3p_SSSS_mod)
    
    # traces and posterior densities
    pdf("Otipulae_WILD_P3p_SSSS_mod.pdf")
    plot(Otipulae_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)]) 
    plot(Otipulae_WILD_P3p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)]) #
    heidel.diag(Otipulae_WILD_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)])
    #autocorr.plot(Otipulae_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)]) #
    autocorr.diag(Otipulae_WILD_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(Otipulae_WILD_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)]) # 
    effectiveSize(Otipulae_WILD_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_WILD_P3p_SSSS_mod <- Otipulae_WILD_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_WILD_P3p_SSSS_mod <- Otipulae_WILD_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Otipulae_WILD_P3p_SSSS_mod <- rowSums(Otipulae_WILD_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Otipulae_WILD_P3p_SSSS_mod) 
      HPDinterval(va_liab_Otipulae_WILD_P3p_SSSS_mod) 
      
      mean(vlat_Otipulae_WILD_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_Otipulae_WILD_P3p_SSSS_mod <- Otipulae_WILD_P3p_SSSS_mod[["X"]]
      beta_Otipulae_WILD_P3p_SSSS_mod <- Otipulae_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)]
      vf_Otipulae_WILD_P3p_SSSS_mod   <- apply(beta_Otipulae_WILD_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_Otipulae_WILD_P3p_SSSS_mod %*% b))}) 
      mean(vf_Otipulae_WILD_P3p_SSSS_mod) 
      
      h2_liab_Otipulae_WILD_P3p_SSSS_mod <- va_liab_Otipulae_WILD_P3p_SSSS_mod / (vlat_Otipulae_WILD_P3p_SSSS_mod + vf_Otipulae_WILD_P3p_SSSS_mod)
      mean(h2_liab_Otipulae_WILD_P3p_SSSS_mod) 
      posterior.mode(h2_liab_Otipulae_WILD_P3p_SSSS_mod)	
      median(h2_liab_Otipulae_WILD_P3p_SSSS_mod)		
      HPDinterval(h2_liab_Otipulae_WILD_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Otipulae_WILD_P3p_SSSS_mod <- rowMeans(Otipulae_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)])
    Evol_liab_Otipulae_WILD_P3p_SSSS_mod <- va_liab_Otipulae_WILD_P3p_SSSS_mod / mean(trait_mean_liab_Otipulae_WILD_P3p_SSSS_mod)^2
    mean(Evol_liab_Otipulae_WILD_P3p_SSSS_mod)
    
    #Otipulae_WILD_P3p_SSSS_mod data scale
    {
      
      predict_Otipulae_WILD_P3p_SSSS_mod <- map(1:nrow(Otipulae_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)]), ~ as.vector(X_Otipulae_WILD_P3p_SSSS_mod %*% Otipulae_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)][., ]))
      
      data_Otipulae_WILD_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Otipulae_WILD_P3p_SSSS_mod,
                      var.a = Otipulae_WILD_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_WILD_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_WILD_P3p_SSSS_mod <- data_Otipulae_WILD_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Otipulae_WILD_P3p_SSSS_mod <- data_Otipulae_WILD_P3p_SSSS_mod[["mean.obs"]]
      va_data_Otipulae_WILD_P3p_SSSS_mod <- data_Otipulae_WILD_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_Otipulae_WILD_P3p_SSSS_mod <- data_Otipulae_WILD_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_Otipulae_WILD_P3p_SSSS_mod <- (va_data_Otipulae_WILD_P3p_SSSS_mod/2) / (trait_mean_data_Otipulae_WILD_P3p_SSSS_mod)^2
      
      mean(h2_data_Otipulae_WILD_P3p_SSSS_mod)
      mean(trait_mean_data_Otipulae_WILD_P3p_SSSS_mod)
      mean(va_data_Otipulae_WILD_P3p_SSSS_mod)
      mean(vp_data_Otipulae_WILD_P3p_SSSS_mod)
      mean(Evol_data_Otipulae_WILD_P3p_SSSS_mod)
      
    }
    
  }
  
  #Otipulae_Vg_P3p_SSSS_mod_2 
  {
    
    Otipulae_Vg_P3p_SSSS_mod_2 <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer + at.level(Treatment,1):Ancestral + at.level(Treatment,2):Ancestral -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vg_Otipulae_data_woutCB,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)        
    
    
    saveRDS(Otipulae_Vg_P3p_SSSS_mod_2, file = "Otipulae_Vg_P3p_SSSS_mod_2.rds")
    Otipulae_Vg_P3p_SSSS_mod_2 <- readRDS("Otipulae_Vg_P3p_SSSS_mod_2.rds")
    
    summary(Otipulae_Vg_P3p_SSSS_mod_2) 
    #plotTrace(Otipulae_Vg_P3p_SSSS_mod_2$Sol)
   # View(Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]])
    # traces and posterior densities
    pdf("Otipulae_Vg_P3p_SSSS_mod_2.pdf")
    plot(Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:5)]) 
    plot(Otipulae_Vg_P3p_SSSS_mod_2[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_Vg_P3p_SSSS_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_Vg_P3p_SSSS_mod_2[["VCV"]])
    #autocorr.plot(Otipulae_Vg_P3p_SSSS_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_Vg_P3p_SSSS_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_Vg_P3p_SSSS_mod_2 <- Otipulae_Vg_P3p_SSSS_mod_2[["VCV"]][ , "LineB"]
      vr_Otipulae_Vg_P3p_SSSS_mod_2 <- Otipulae_Vg_P3p_SSSS_mod_2[["VCV"]][ , "units"]
      vlat_Otipulae_Vg_P3p_SSSS_mod_2 <- rowSums(Otipulae_Vg_P3p_SSSS_mod_2[["VCV"]])
      
      mean(va_liab_Otipulae_Vg_P3p_SSSS_mod_2) 
      HPDinterval(va_liab_Otipulae_Vg_P3p_SSSS_mod_2) 
      
      mean(vlat_Otipulae_Vg_P3p_SSSS_mod_2) 
      
      #variance of fixed effects
      X_Otipulae_Vg_P3p_SSSS_mod_2 <- Otipulae_Vg_P3p_SSSS_mod_2[["X"]]
      beta_Otipulae_Vg_P3p_SSSS_mod_2 <- Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:5)]
      vf_Otipulae_Vg_P3p_SSSS_mod_2   <- apply(beta_Otipulae_Vg_P3p_SSSS_mod_2, 1, function(b) {var(as.vector(X_Otipulae_Vg_P3p_SSSS_mod_2 %*% b))}) 
      mean(vf_Otipulae_Vg_P3p_SSSS_mod_2) 
      
      h2_liab_Otipulae_Vg_P3p_SSSS_mod_2 <- va_liab_Otipulae_Vg_P3p_SSSS_mod_2 / (vlat_Otipulae_Vg_P3p_SSSS_mod_2 + vf_Otipulae_Vg_P3p_SSSS_mod_2)
      mean(h2_liab_Otipulae_Vg_P3p_SSSS_mod_2) 
      posterior.mode(h2_liab_Otipulae_Vg_P3p_SSSS_mod_2)	
      median(h2_liab_Otipulae_Vg_P3p_SSSS_mod_2)		
      HPDinterval(h2_liab_Otipulae_Vg_P3p_SSSS_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:3)])) #Otip WILD P3p_SSSS
    mean(rowMeans(Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,4]) #CEW1 CONTROL P3p_SSSS
    mean(rowMeans(Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,5]) #JU178 Control P3p_SSSS
    
    trait_mean_liab_Otipulae_Vg_P3p_SSSS_mod_2 <- ((rowMeans(Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,4]) + (rowMeans(Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,5]) )/3)
    
    Evol_liab_Otipulae_Vg_P3p_SSSS_mod_2 <- (va_liab_Otipulae_Vg_P3p_SSSS_mod_2/2) / (trait_mean_liab_Otipulae_Vg_P3p_SSSS_mod_2)^2
    mean(Evol_liab_Otipulae_Vg_P3p_SSSS_mod_2)
    
    
    #Otipulae_Vg_P3p_SSSS_mod_2 data scale
    {
      
      predict_Otipulae_Vg_P3p_SSSS_mod_2 <- map(1:nrow(Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_Vg_P3p_SSSS_mod_2 %*% Otipulae_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_Vg_P3p_SSSS_mod_2 <-
        pmap_dfr(list(predict = predict_Otipulae_Vg_P3p_SSSS_mod_2,
                      var.a = Otipulae_Vg_P3p_SSSS_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_Vg_P3p_SSSS_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_Vg_P3p_SSSS_mod_2 <- data_Otipulae_Vg_P3p_SSSS_mod_2[["h2.obs"]]
      trait_mean_data_Otipulae_Vg_P3p_SSSS_mod_2 <- data_Otipulae_Vg_P3p_SSSS_mod_2[["mean.obs"]]
      va_data_Otipulae_Vg_P3p_SSSS_mod_2 <- data_Otipulae_Vg_P3p_SSSS_mod_2[["var.a.obs"]]
      vp_data_Otipulae_Vg_P3p_SSSS_mod_2 <- data_Otipulae_Vg_P3p_SSSS_mod_2[["var.obs"]]
      
      Evol_data_Otipulae_Vg_P3p_SSSS_mod_2 <- (va_data_Otipulae_Vg_P3p_SSSS_mod_2/2) / (trait_mean_data_Otipulae_Vg_P3p_SSSS_mod_2)^2
      
      mean(h2_data_Otipulae_Vg_P3p_SSSS_mod_2)
      mean(trait_mean_data_Otipulae_Vg_P3p_SSSS_mod_2)
      mean(va_data_Otipulae_Vg_P3p_SSSS_mod_2)
      mean(vp_data_Otipulae_Vg_P3p_SSSS_mod_2)
      mean(Evol_data_Otipulae_Vg_P3p_SSSS_mod_2)
      
    }
    
  }
  

  
  ## Otipulae P4p ----
  
  
  #Otipulae_CONTROL_P4p_SSSS_mod_2 
  {
    
    Otipulae_CONTROL_P4p_SSSS_mod_2 <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer + Ancestral -1,
                                             random      = ~ LineB + BlockRep,
                                             family      = "threshold",
                                             data        = Vg_Otipulae_bi_CONTROL_woutCB,
                                             prior       = prior_bi_block,
                                             nitt        = 1260000,       
                                             thin        = 500,           
                                             burnin      = 10000,
                                             trunc       = TRUE,
                                             pr          = TRUE,
                                             pl          = TRUE)            
    
    saveRDS(Otipulae_CONTROL_P4p_SSSS_mod_2, file = "Otipulae_CONTROL_P4p_SSSS_mod_2.rds")
    Otipulae_CONTROL_P4p_SSSS_mod_2 <- readRDS("Otipulae_CONTROL_P4p_SSSS_mod_2.rds")
    
    summary(Otipulae_CONTROL_P4p_SSSS_mod_2) 
    #plotTrace(Otipulae_CONTROL_P4p_SSSS_mod_2$Sol)
    
    # traces and posterior densities
    
    pdf("Otipulae_CONTROL_P4p_SSSS_mod_2.pdf")
    plot(Otipulae_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:4)]) 
    plot(Otipulae_CONTROL_P4p_SSSS_mod_2[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:4)]) #
    heidel.diag(Otipulae_CONTROL_P4p_SSSS_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:4)])
    #autocorr.plot(Otipulae_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:4)]) #
    autocorr.diag(Otipulae_CONTROL_P4p_SSSS_mod_2[["VCV"]])
    #autocorr.plot(Otipulae_CONTROL_P4p_SSSS_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:4)]) # 
    effectiveSize(Otipulae_CONTROL_P4p_SSSS_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_CONTROL_P4p_SSSS_mod_2 <- Otipulae_CONTROL_P4p_SSSS_mod_2[["VCV"]][ , "LineB"]
      vr_Otipulae_CONTROL_P4p_SSSS_mod_2 <- Otipulae_CONTROL_P4p_SSSS_mod_2[["VCV"]][ , "units"]
      vlat_Otipulae_CONTROL_P4p_SSSS_mod_2 <- rowSums(Otipulae_CONTROL_P4p_SSSS_mod_2[["VCV"]])
      
      mean(va_liab_Otipulae_CONTROL_P4p_SSSS_mod_2) 
      HPDinterval(va_liab_Otipulae_CONTROL_P4p_SSSS_mod_2) 
      
      mean(vlat_Otipulae_CONTROL_P4p_SSSS_mod_2) 
      
      #variance of fixed effects
      X_Otipulae_CONTROL_P4p_SSSS_mod_2 <- Otipulae_CONTROL_P4p_SSSS_mod_2[["X"]]
      beta_Otipulae_CONTROL_P4p_SSSS_mod_2 <- Otipulae_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:4)]
      vf_Otipulae_CONTROL_P4p_SSSS_mod_2   <- apply(beta_Otipulae_CONTROL_P4p_SSSS_mod_2, 1, function(b) {var(as.vector(X_Otipulae_CONTROL_P4p_SSSS_mod_2 %*% b))}) 
      mean(vf_Otipulae_CONTROL_P4p_SSSS_mod_2) 
      
      h2_liab_Otipulae_CONTROL_P4p_SSSS_mod_2 <- va_liab_Otipulae_CONTROL_P4p_SSSS_mod_2 / (vlat_Otipulae_CONTROL_P4p_SSSS_mod_2 + vf_Otipulae_CONTROL_P4p_SSSS_mod_2)
      mean(h2_liab_Otipulae_CONTROL_P4p_SSSS_mod_2) 
      posterior.mode(h2_liab_Otipulae_CONTROL_P4p_SSSS_mod_2)	
      median(h2_liab_Otipulae_CONTROL_P4p_SSSS_mod_2)		
      HPDinterval(h2_liab_Otipulae_CONTROL_P4p_SSSS_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_CONTROL_P4p_SSSS_mod_2 <- ((rowMeans(Otipulae_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Otipulae_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Otipulae_CONTROL_P4p_SSSS_mod_2[["Sol"]][,4]))/2)
    
    Evol_liab_Otipulae_CONTROL_P4p_SSSS_mod_2 <- (va_liab_Otipulae_CONTROL_P4p_SSSS_mod_2/2) / (trait_mean_liab_Otipulae_CONTROL_P4p_SSSS_mod_2)^2
    mean(Evol_liab_Otipulae_CONTROL_P4p_SSSS_mod_2)
    
    
    #Otipulae_CONTROL_P4p_SSSS_mod_2 data scale
    {
      
      predict_Otipulae_CONTROL_P4p_SSSS_mod_2 <- map(1:nrow(Otipulae_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:4)]), ~ as.vector(X_Otipulae_CONTROL_P4p_SSSS_mod_2 %*% Otipulae_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:4)][., ]))
      
      data_Otipulae_CONTROL_P4p_SSSS_mod_2 <-
        pmap_dfr(list(predict = predict_Otipulae_CONTROL_P4p_SSSS_mod_2,
                      var.a = Otipulae_CONTROL_P4p_SSSS_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_CONTROL_P4p_SSSS_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_CONTROL_P4p_SSSS_mod_2 <- data_Otipulae_CONTROL_P4p_SSSS_mod_2[["h2.obs"]]
      trait_mean_data_Otipulae_CONTROL_P4p_SSSS_mod_2 <- data_Otipulae_CONTROL_P4p_SSSS_mod_2[["mean.obs"]]
      va_data_Otipulae_CONTROL_P4p_SSSS_mod_2 <- data_Otipulae_CONTROL_P4p_SSSS_mod_2[["var.a.obs"]]
      vp_data_Otipulae_CONTROL_P4p_SSSS_mod_2 <- data_Otipulae_CONTROL_P4p_SSSS_mod_2[["var.obs"]]
      
      Evol_data_Otipulae_CONTROL_P4p_SSSS_mod_2 <- (va_data_Otipulae_CONTROL_P4p_SSSS_mod_2/2) / (trait_mean_data_Otipulae_CONTROL_P4p_SSSS_mod_2)^2
      
      mean(h2_data_Otipulae_CONTROL_P4p_SSSS_mod_2) 
      mean(trait_mean_data_Otipulae_CONTROL_P4p_SSSS_mod_2)
      mean(va_data_Otipulae_CONTROL_P4p_SSSS_mod_2) 
      mean(vp_data_Otipulae_CONTROL_P4p_SSSS_mod_2)
      mean(Evol_data_Otipulae_CONTROL_P4p_SSSS_mod_2)
      
    }
    
  }
  
  #Otipulae_WILD_P4p_SSSS_mod 
  {
    
    Otipulae_WILD_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer -1,
                                           random      = ~ LineB + BlockRep,
                                           family      = "threshold",
                                           data        = Vg_Otipulae_bi_WILD,
                                           prior       = prior_bi_block,
                                           nitt        = 1260000,       
                                           thin        = 500,           
                                           burnin      = 10000,
                                           trunc       = TRUE,
                                           pr          = TRUE,
                                           pl          = TRUE)             
    
    saveRDS(Otipulae_WILD_P4p_SSSS_mod, file = "Otipulae_WILD_P4p_SSSS_mod.rds")
    Otipulae_WILD_P4p_SSSS_mod <- readRDS("Otipulae_WILD_P4p_SSSS_mod.rds")
    
    summary(Otipulae_WILD_P4p_SSSS_mod) 
    #plot(Otipulae_WILD_P4p_SSSS_mod)
    
    # traces and posterior densities
    pdf("Otipulae_WILD_P4p_SSSS_mod.pdf")
    plot(Otipulae_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)]) 
    plot(Otipulae_WILD_P4p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)]) #
    heidel.diag(Otipulae_WILD_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)])
    #autocorr.plot(Otipulae_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)]) #
    autocorr.diag(Otipulae_WILD_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(Otipulae_WILD_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)]) # 
    effectiveSize(Otipulae_WILD_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_WILD_P4p_SSSS_mod <- Otipulae_WILD_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_WILD_P4p_SSSS_mod <- Otipulae_WILD_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Otipulae_WILD_P4p_SSSS_mod <- rowSums(Otipulae_WILD_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Otipulae_WILD_P4p_SSSS_mod) 
      HPDinterval(va_liab_Otipulae_WILD_P4p_SSSS_mod) 
      
      mean(vlat_Otipulae_WILD_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_Otipulae_WILD_P4p_SSSS_mod <- Otipulae_WILD_P4p_SSSS_mod[["X"]]
      beta_Otipulae_WILD_P4p_SSSS_mod <- Otipulae_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)]
      vf_Otipulae_WILD_P4p_SSSS_mod   <- apply(beta_Otipulae_WILD_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_Otipulae_WILD_P4p_SSSS_mod %*% b))}) 
      mean(vf_Otipulae_WILD_P4p_SSSS_mod) 
      
      h2_liab_Otipulae_WILD_P4p_SSSS_mod <- va_liab_Otipulae_WILD_P4p_SSSS_mod / (vlat_Otipulae_WILD_P4p_SSSS_mod + vf_Otipulae_WILD_P4p_SSSS_mod)
      mean(h2_liab_Otipulae_WILD_P4p_SSSS_mod) 
      posterior.mode(h2_liab_Otipulae_WILD_P4p_SSSS_mod)	
      median(h2_liab_Otipulae_WILD_P4p_SSSS_mod)		
      HPDinterval(h2_liab_Otipulae_WILD_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Otipulae_WILD_P4p_SSSS_mod <- rowMeans(Otipulae_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)])
    Evol_liab_Otipulae_WILD_P4p_SSSS_mod <- va_liab_Otipulae_WILD_P4p_SSSS_mod / mean(trait_mean_liab_Otipulae_WILD_P4p_SSSS_mod)^2
    mean(Evol_liab_Otipulae_WILD_P4p_SSSS_mod)
    
    
    #Otipulae_WILD_P4p_SSSS_mod data scale
    {
      
      predict_Otipulae_WILD_P4p_SSSS_mod <- map(1:nrow(Otipulae_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)]), ~ as.vector(X_Otipulae_WILD_P4p_SSSS_mod %*% Otipulae_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)][., ]))
      
      data_Otipulae_WILD_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Otipulae_WILD_P4p_SSSS_mod,
                      var.a = Otipulae_WILD_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_WILD_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_WILD_P4p_SSSS_mod <- data_Otipulae_WILD_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Otipulae_WILD_P4p_SSSS_mod <- data_Otipulae_WILD_P4p_SSSS_mod[["mean.obs"]]
      va_data_Otipulae_WILD_P4p_SSSS_mod <- data_Otipulae_WILD_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_Otipulae_WILD_P4p_SSSS_mod <- data_Otipulae_WILD_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_Otipulae_WILD_P4p_SSSS_mod <- (va_data_Otipulae_WILD_P4p_SSSS_mod/2) / (trait_mean_data_Otipulae_WILD_P4p_SSSS_mod)^2
      
      mean(h2_data_Otipulae_WILD_P4p_SSSS_mod)
      mean(trait_mean_data_Otipulae_WILD_P4p_SSSS_mod)
      mean(va_data_Otipulae_WILD_P4p_SSSS_mod)
      mean(vp_data_Otipulae_WILD_P4p_SSSS_mod)
      mean(Evol_data_Otipulae_WILD_P4p_SSSS_mod)
      
    }
    
  }
  #Otipulae_Vg_P4p_SSSS_mod_2 
  {
    
    Otipulae_Vg_P4p_SSSS_mod_2 <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer + at.level(Treatment,1):Ancestral + at.level(Treatment,2):Ancestral -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vg_Otipulae_data_woutCB,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)        
    
    
    saveRDS(Otipulae_Vg_P4p_SSSS_mod_2, file = "Otipulae_Vg_P4p_SSSS_mod_2.rds")
    Otipulae_Vg_P4p_SSSS_mod_2 <- readRDS("Otipulae_Vg_P4p_SSSS_mod_2.rds")
    
    summary(Otipulae_Vg_P4p_SSSS_mod_2) 
    #plotTrace(Otipulae_Vg_P4p_SSSS_mod_2$Sol)
    #View(Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]])
    # traces and posterior densities
    pdf("Otipulae_Vg_P4p_SSSS_mod_2.pdf")
    plot(Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:5)]) 
    plot(Otipulae_Vg_P4p_SSSS_mod_2[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_Vg_P4p_SSSS_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_Vg_P4p_SSSS_mod_2[["VCV"]])
    #autocorr.plot(Otipulae_Vg_P4p_SSSS_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_Vg_P4p_SSSS_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_Vg_P4p_SSSS_mod_2 <- Otipulae_Vg_P4p_SSSS_mod_2[["VCV"]][ , "LineB"]
      vr_Otipulae_Vg_P4p_SSSS_mod_2 <- Otipulae_Vg_P4p_SSSS_mod_2[["VCV"]][ , "units"]
      vlat_Otipulae_Vg_P4p_SSSS_mod_2 <- rowSums(Otipulae_Vg_P4p_SSSS_mod_2[["VCV"]])
      
      mean(va_liab_Otipulae_Vg_P4p_SSSS_mod_2) 
      HPDinterval(va_liab_Otipulae_Vg_P4p_SSSS_mod_2) 
      
      mean(vlat_Otipulae_Vg_P4p_SSSS_mod_2) 
      
      #variance of fixed effects
      X_Otipulae_Vg_P4p_SSSS_mod_2 <- Otipulae_Vg_P4p_SSSS_mod_2[["X"]]
      beta_Otipulae_Vg_P4p_SSSS_mod_2 <- Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:5)]
      vf_Otipulae_Vg_P4p_SSSS_mod_2   <- apply(beta_Otipulae_Vg_P4p_SSSS_mod_2, 1, function(b) {var(as.vector(X_Otipulae_Vg_P4p_SSSS_mod_2 %*% b))}) 
      mean(vf_Otipulae_Vg_P4p_SSSS_mod_2) 
      
      h2_liab_Otipulae_Vg_P4p_SSSS_mod_2 <- va_liab_Otipulae_Vg_P4p_SSSS_mod_2 / (vlat_Otipulae_Vg_P4p_SSSS_mod_2 + vf_Otipulae_Vg_P4p_SSSS_mod_2)
      mean(h2_liab_Otipulae_Vg_P4p_SSSS_mod_2) 
      posterior.mode(h2_liab_Otipulae_Vg_P4p_SSSS_mod_2)	
      median(h2_liab_Otipulae_Vg_P4p_SSSS_mod_2)		
      HPDinterval(h2_liab_Otipulae_Vg_P4p_SSSS_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:3)])) #Otip WILD P4p_SSSS
    mean(rowMeans(Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,4]) #CEW1 CONTROL P4p_SSSS
    mean(rowMeans(Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,5]) #JU178 Control P4p_SSSS
    
    trait_mean_liab_Otipulae_Vg_P4p_SSSS_mod_2 <- ((rowMeans(Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,4]) + (rowMeans(Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,5]) )/3)
    
    Evol_liab_Otipulae_Vg_P4p_SSSS_mod_2 <- (va_liab_Otipulae_Vg_P4p_SSSS_mod_2/2) / (trait_mean_liab_Otipulae_Vg_P4p_SSSS_mod_2)^2
    mean(Evol_liab_Otipulae_Vg_P4p_SSSS_mod_2)
    
    
    #Otipulae_Vg_P4p_SSSS_mod_2 data scale
    {
      
      predict_Otipulae_Vg_P4p_SSSS_mod_2 <- map(1:nrow(Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_Vg_P4p_SSSS_mod_2 %*% Otipulae_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_Vg_P4p_SSSS_mod_2 <-
        pmap_dfr(list(predict = predict_Otipulae_Vg_P4p_SSSS_mod_2,
                      var.a = Otipulae_Vg_P4p_SSSS_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_Vg_P4p_SSSS_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_Vg_P4p_SSSS_mod_2 <- data_Otipulae_Vg_P4p_SSSS_mod_2[["h2.obs"]]
      trait_mean_data_Otipulae_Vg_P4p_SSSS_mod_2 <- data_Otipulae_Vg_P4p_SSSS_mod_2[["mean.obs"]]
      va_data_Otipulae_Vg_P4p_SSSS_mod_2 <- data_Otipulae_Vg_P4p_SSSS_mod_2[["var.a.obs"]]
      vp_data_Otipulae_Vg_P4p_SSSS_mod_2 <- data_Otipulae_Vg_P4p_SSSS_mod_2[["var.obs"]]
      
      Evol_data_Otipulae_Vg_P4p_SSSS_mod_2 <- (va_data_Otipulae_Vg_P4p_SSSS_mod_2/2) / (trait_mean_data_Otipulae_Vg_P4p_SSSS_mod_2)^2
      
      mean(h2_data_Otipulae_Vg_P4p_SSSS_mod_2)
      mean(trait_mean_data_Otipulae_Vg_P4p_SSSS_mod_2)
      mean(va_data_Otipulae_Vg_P4p_SSSS_mod_2)
      mean(vp_data_Otipulae_Vg_P4p_SSSS_mod_2)
      mean(Evol_data_Otipulae_Vg_P4p_SSSS_mod_2)
      
      
      
      
    }
    
  }
  
 
  ## Otipulae P8p ----
  
  
  #Otipulae_CONTROL_P8p_SSSS_mod_2 
  {
    
    Otipulae_CONTROL_P8p_SSSS_mod_2 <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer + Ancestral -1,
                                             random      = ~ LineB + BlockRep,
                                             family      = "threshold",
                                             data        = Vg_Otipulae_bi_CONTROL_woutCB,
                                             prior       = prior_bi_block,
                                             nitt        = 1260000,       
                                             thin        = 500,           
                                             burnin      = 10000,
                                             trunc       = TRUE,
                                             pr          = TRUE,
                                             pl          = TRUE)            
    
    saveRDS(Otipulae_CONTROL_P8p_SSSS_mod_2, file = "Otipulae_CONTROL_P8p_SSSS_mod_2.rds")
    Otipulae_CONTROL_P8p_SSSS_mod_2 <- readRDS("Otipulae_CONTROL_P8p_SSSS_mod_2.rds")
    
    summary(Otipulae_CONTROL_P8p_SSSS_mod_2) 
    #plotTrace(Otipulae_CONTROL_P8p_SSSS_mod_2$Sol)
    
    # traces and posterior densities
    
    pdf("Otipulae_CONTROL_P8p_SSSS_mod_2.pdf")
    plot(Otipulae_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:4)]) 
    plot(Otipulae_CONTROL_P8p_SSSS_mod_2[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:4)]) #
    heidel.diag(Otipulae_CONTROL_P8p_SSSS_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:4)])
    #autocorr.plot(Otipulae_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:4)]) #
    autocorr.diag(Otipulae_CONTROL_P8p_SSSS_mod_2[["VCV"]])
    #autocorr.plot(Otipulae_CONTROL_P8p_SSSS_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:4)]) # 
    effectiveSize(Otipulae_CONTROL_P8p_SSSS_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_CONTROL_P8p_SSSS_mod_2 <- Otipulae_CONTROL_P8p_SSSS_mod_2[["VCV"]][ , "LineB"]
      vr_Otipulae_CONTROL_P8p_SSSS_mod_2 <- Otipulae_CONTROL_P8p_SSSS_mod_2[["VCV"]][ , "units"]
      vlat_Otipulae_CONTROL_P8p_SSSS_mod_2 <- rowSums(Otipulae_CONTROL_P8p_SSSS_mod_2[["VCV"]])
      
      mean(va_liab_Otipulae_CONTROL_P8p_SSSS_mod_2) 
      HPDinterval(va_liab_Otipulae_CONTROL_P8p_SSSS_mod_2) 
      
      mean(vlat_Otipulae_CONTROL_P8p_SSSS_mod_2) 
      
      #variance of fixed effects
      X_Otipulae_CONTROL_P8p_SSSS_mod_2 <- Otipulae_CONTROL_P8p_SSSS_mod_2[["X"]]
      beta_Otipulae_CONTROL_P8p_SSSS_mod_2 <- Otipulae_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:4)]
      vf_Otipulae_CONTROL_P8p_SSSS_mod_2   <- apply(beta_Otipulae_CONTROL_P8p_SSSS_mod_2, 1, function(b) {var(as.vector(X_Otipulae_CONTROL_P8p_SSSS_mod_2 %*% b))}) 
      mean(vf_Otipulae_CONTROL_P8p_SSSS_mod_2) 
      
      h2_liab_Otipulae_CONTROL_P8p_SSSS_mod_2 <- va_liab_Otipulae_CONTROL_P8p_SSSS_mod_2 / (vlat_Otipulae_CONTROL_P8p_SSSS_mod_2 + vf_Otipulae_CONTROL_P8p_SSSS_mod_2)
      mean(h2_liab_Otipulae_CONTROL_P8p_SSSS_mod_2) 
      posterior.mode(h2_liab_Otipulae_CONTROL_P8p_SSSS_mod_2)	
      median(h2_liab_Otipulae_CONTROL_P8p_SSSS_mod_2)		
      HPDinterval(h2_liab_Otipulae_CONTROL_P8p_SSSS_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_CONTROL_P8p_SSSS_mod_2 <- ((rowMeans(Otipulae_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Otipulae_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Otipulae_CONTROL_P8p_SSSS_mod_2[["Sol"]][,4]))/2)
    
    Evol_liab_Otipulae_CONTROL_P8p_SSSS_mod_2 <- (va_liab_Otipulae_CONTROL_P8p_SSSS_mod_2/2) / (trait_mean_liab_Otipulae_CONTROL_P8p_SSSS_mod_2)^2
    mean(Evol_liab_Otipulae_CONTROL_P8p_SSSS_mod_2)
    
    
    #Otipulae_CONTROL_P8p_SSSS_mod_2 data scale
    {
      
      predict_Otipulae_CONTROL_P8p_SSSS_mod_2 <- map(1:nrow(Otipulae_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:4)]), ~ as.vector(X_Otipulae_CONTROL_P8p_SSSS_mod_2 %*% Otipulae_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:4)][., ]))
      
      data_Otipulae_CONTROL_P8p_SSSS_mod_2 <-
        pmap_dfr(list(predict = predict_Otipulae_CONTROL_P8p_SSSS_mod_2,
                      var.a = Otipulae_CONTROL_P8p_SSSS_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_CONTROL_P8p_SSSS_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_CONTROL_P8p_SSSS_mod_2 <- data_Otipulae_CONTROL_P8p_SSSS_mod_2[["h2.obs"]]
      trait_mean_data_Otipulae_CONTROL_P8p_SSSS_mod_2 <- data_Otipulae_CONTROL_P8p_SSSS_mod_2[["mean.obs"]]
      va_data_Otipulae_CONTROL_P8p_SSSS_mod_2 <- data_Otipulae_CONTROL_P8p_SSSS_mod_2[["var.a.obs"]]
      vp_data_Otipulae_CONTROL_P8p_SSSS_mod_2 <- data_Otipulae_CONTROL_P8p_SSSS_mod_2[["var.obs"]]
      
      Evol_data_Otipulae_CONTROL_P8p_SSSS_mod_2 <- (va_data_Otipulae_CONTROL_P8p_SSSS_mod_2/2) / (trait_mean_data_Otipulae_CONTROL_P8p_SSSS_mod_2)^2
      
      mean(h2_data_Otipulae_CONTROL_P8p_SSSS_mod_2) 
      mean(trait_mean_data_Otipulae_CONTROL_P8p_SSSS_mod_2)
      mean(va_data_Otipulae_CONTROL_P8p_SSSS_mod_2) 
      mean(vp_data_Otipulae_CONTROL_P8p_SSSS_mod_2)
      mean(Evol_data_Otipulae_CONTROL_P8p_SSSS_mod_2)
      
    }
    
  }
  
  #Otipulae_WILD_P8p_SSSS_mod 
  {
    
    Otipulae_WILD_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer -1,
                                           random      = ~ LineB + BlockRep,
                                           family      = "threshold",
                                           data        = Vg_Otipulae_bi_WILD,
                                           prior       = prior_bi_block,
                                           nitt        = 1260000,       
                                           thin        = 500,           
                                           burnin      = 10000,
                                           trunc       = TRUE,
                                           pr          = TRUE,
                                           pl          = TRUE)             
    
    saveRDS(Otipulae_WILD_P8p_SSSS_mod, file = "Otipulae_WILD_P8p_SSSS_mod.rds")
    Otipulae_WILD_P8p_SSSS_mod <- readRDS("Otipulae_WILD_P8p_SSSS_mod.rds")
    
    summary(Otipulae_WILD_P8p_SSSS_mod) 
    #plot(Otipulae_WILD_P8p_SSSS_mod)
    
    # traces and posterior densities
    pdf("Otipulae_WILD_P8p_SSSS_mod.pdf")
    plot(Otipulae_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)]) 
    plot(Otipulae_WILD_P8p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)]) #
    heidel.diag(Otipulae_WILD_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)])
    #autocorr.plot(Otipulae_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)]) #
    autocorr.diag(Otipulae_WILD_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(Otipulae_WILD_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)]) # 
    effectiveSize(Otipulae_WILD_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_WILD_P8p_SSSS_mod <- Otipulae_WILD_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_WILD_P8p_SSSS_mod <- Otipulae_WILD_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Otipulae_WILD_P8p_SSSS_mod <- rowSums(Otipulae_WILD_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Otipulae_WILD_P8p_SSSS_mod) 
      HPDinterval(va_liab_Otipulae_WILD_P8p_SSSS_mod) 
      
      mean(vlat_Otipulae_WILD_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_Otipulae_WILD_P8p_SSSS_mod <- Otipulae_WILD_P8p_SSSS_mod[["X"]]
      beta_Otipulae_WILD_P8p_SSSS_mod <- Otipulae_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)]
      vf_Otipulae_WILD_P8p_SSSS_mod   <- apply(beta_Otipulae_WILD_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_Otipulae_WILD_P8p_SSSS_mod %*% b))}) 
      mean(vf_Otipulae_WILD_P8p_SSSS_mod) 
      
      h2_liab_Otipulae_WILD_P8p_SSSS_mod <- va_liab_Otipulae_WILD_P8p_SSSS_mod / (vlat_Otipulae_WILD_P8p_SSSS_mod + vf_Otipulae_WILD_P8p_SSSS_mod)
      mean(h2_liab_Otipulae_WILD_P8p_SSSS_mod) 
      posterior.mode(h2_liab_Otipulae_WILD_P8p_SSSS_mod)	
      median(h2_liab_Otipulae_WILD_P8p_SSSS_mod)		
      HPDinterval(h2_liab_Otipulae_WILD_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2

    trait_mean_liab_Otipulae_WILD_P8p_SSSS_mod <- rowMeans(Otipulae_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)])
    Evol_liab_Otipulae_WILD_P8p_SSSS_mod <- va_liab_Otipulae_WILD_P8p_SSSS_mod / mean(trait_mean_liab_Otipulae_WILD_P8p_SSSS_mod)^2
    mean(Evol_liab_Otipulae_WILD_P8p_SSSS_mod)
    
    #Otipulae_WILD_P8p_SSSS_mod data scale
    {
      
      predict_Otipulae_WILD_P8p_SSSS_mod <- map(1:nrow(Otipulae_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)]), ~ as.vector(X_Otipulae_WILD_P8p_SSSS_mod %*% Otipulae_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)][., ]))
      
      data_Otipulae_WILD_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Otipulae_WILD_P8p_SSSS_mod,
                      var.a = Otipulae_WILD_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_WILD_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_WILD_P8p_SSSS_mod <- data_Otipulae_WILD_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Otipulae_WILD_P8p_SSSS_mod <- data_Otipulae_WILD_P8p_SSSS_mod[["mean.obs"]]
      va_data_Otipulae_WILD_P8p_SSSS_mod <- data_Otipulae_WILD_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_Otipulae_WILD_P8p_SSSS_mod <- data_Otipulae_WILD_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_Otipulae_WILD_P8p_SSSS_mod <- (va_data_Otipulae_WILD_P8p_SSSS_mod/2) / (trait_mean_data_Otipulae_WILD_P8p_SSSS_mod)^2
      
      mean(h2_data_Otipulae_WILD_P8p_SSSS_mod)
      mean(trait_mean_data_Otipulae_WILD_P8p_SSSS_mod)
      mean(va_data_Otipulae_WILD_P8p_SSSS_mod)
      mean(vp_data_Otipulae_WILD_P8p_SSSS_mod)
      mean(Evol_data_Otipulae_WILD_P8p_SSSS_mod)
      
    }
    
  }
  
  #Otipulae_Vg_P8p_SSSS_mod_2 
  {
    
    Otipulae_Vg_P8p_SSSS_mod_2 <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer + at.level(Treatment,1):Ancestral + at.level(Treatment,2):Ancestral -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vg_Otipulae_data_woutCB,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)        
    
    saveRDS(Otipulae_Vg_P8p_SSSS_mod_2, file = "Otipulae_Vg_P8p_SSSS_mod_2.rds")
    Otipulae_Vg_P8p_SSSS_mod_2 <- readRDS("Otipulae_Vg_P8p_SSSS_mod_2.rds")
    
    summary(Otipulae_Vg_P8p_SSSS_mod_2) 
    #plotTrace(Otipulae_Vg_P8p_SSSS_mod_2$Sol)
    #View(Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]])
    
    # traces and posterior densities
    pdf("Otipulae_Vg_P8p_SSSS_mod_2.pdf")
    plot(Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:5)]) 
    plot(Otipulae_Vg_P8p_SSSS_mod_2[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_Vg_P8p_SSSS_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_Vg_P8p_SSSS_mod_2[["VCV"]])
    #autocorr.plot(Otipulae_Vg_P8p_SSSS_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_Vg_P8p_SSSS_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_Vg_P8p_SSSS_mod_2 <- Otipulae_Vg_P8p_SSSS_mod_2[["VCV"]][ , "LineB"]
      vr_Otipulae_Vg_P8p_SSSS_mod_2 <- Otipulae_Vg_P8p_SSSS_mod_2[["VCV"]][ , "units"]
      vlat_Otipulae_Vg_P8p_SSSS_mod_2 <- rowSums(Otipulae_Vg_P8p_SSSS_mod_2[["VCV"]])
      
      mean(va_liab_Otipulae_Vg_P8p_SSSS_mod_2) 
      HPDinterval(va_liab_Otipulae_Vg_P8p_SSSS_mod_2) 
      
      mean(vlat_Otipulae_Vg_P8p_SSSS_mod_2) 
      
      #variance of fixed effects
      X_Otipulae_Vg_P8p_SSSS_mod_2 <- Otipulae_Vg_P8p_SSSS_mod_2[["X"]]
      beta_Otipulae_Vg_P8p_SSSS_mod_2 <- Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:5)]
      vf_Otipulae_Vg_P8p_SSSS_mod_2   <- apply(beta_Otipulae_Vg_P8p_SSSS_mod_2, 1, function(b) {var(as.vector(X_Otipulae_Vg_P8p_SSSS_mod_2 %*% b))}) 
      mean(vf_Otipulae_Vg_P8p_SSSS_mod_2) 
      
      h2_liab_Otipulae_Vg_P8p_SSSS_mod_2 <- va_liab_Otipulae_Vg_P8p_SSSS_mod_2 / (vlat_Otipulae_Vg_P8p_SSSS_mod_2 + vf_Otipulae_Vg_P8p_SSSS_mod_2)
      mean(h2_liab_Otipulae_Vg_P8p_SSSS_mod_2) 
      posterior.mode(h2_liab_Otipulae_Vg_P8p_SSSS_mod_2)	
      median(h2_liab_Otipulae_Vg_P8p_SSSS_mod_2)		
      HPDinterval(h2_liab_Otipulae_Vg_P8p_SSSS_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:3)])) #Otip WILD P8p_SSSS
    mean(rowMeans(Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,4]) #CEW1 CONTROL P8p_SSSS
    mean(rowMeans(Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,5]) #JU178 Control P8p_SSSS
    
    trait_mean_liab_Otipulae_Vg_P8p_SSSS_mod_2 <- ((rowMeans(Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,4]) + (rowMeans(Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,5]) )/3)
    
    Evol_liab_Otipulae_Vg_P8p_SSSS_mod_2 <- (va_liab_Otipulae_Vg_P8p_SSSS_mod_2/2) / (trait_mean_liab_Otipulae_Vg_P8p_SSSS_mod_2)^2
    mean(Evol_liab_Otipulae_Vg_P8p_SSSS_mod_2)
    
    
    #Otipulae_Vg_P8p_SSSS_mod_2 data scale
    {
      
      predict_Otipulae_Vg_P8p_SSSS_mod_2 <- map(1:nrow(Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_Vg_P8p_SSSS_mod_2 %*% Otipulae_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_Vg_P8p_SSSS_mod_2 <-
        pmap_dfr(list(predict = predict_Otipulae_Vg_P8p_SSSS_mod_2,
                      var.a = Otipulae_Vg_P8p_SSSS_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_Vg_P8p_SSSS_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_Vg_P8p_SSSS_mod_2 <- data_Otipulae_Vg_P8p_SSSS_mod_2[["h2.obs"]]
      trait_mean_data_Otipulae_Vg_P8p_SSSS_mod_2 <- data_Otipulae_Vg_P8p_SSSS_mod_2[["mean.obs"]]
      va_data_Otipulae_Vg_P8p_SSSS_mod_2 <- data_Otipulae_Vg_P8p_SSSS_mod_2[["var.a.obs"]]
      vp_data_Otipulae_Vg_P8p_SSSS_mod_2 <- data_Otipulae_Vg_P8p_SSSS_mod_2[["var.obs"]]
      
      Evol_data_Otipulae_Vg_P8p_SSSS_mod_2 <- (va_data_Otipulae_Vg_P8p_SSSS_mod_2/2) / (trait_mean_data_Otipulae_Vg_P8p_SSSS_mod_2)^2
      
      mean(h2_data_Otipulae_Vg_P8p_SSSS_mod_2)
      mean(trait_mean_data_Otipulae_Vg_P8p_SSSS_mod_2)
      mean(va_data_Otipulae_Vg_P8p_SSSS_mod_2)
      mean(vp_data_Otipulae_Vg_P8p_SSSS_mod_2)
      mean(Evol_data_Otipulae_Vg_P8p_SSSS_mod_2)
      
    }
    
  }
  
   ## Otipulae P5p ----
  
  
  #Otipulae_CONTROL_P5p_wt_mod_2   {
    
    Otipulae_CONTROL_P5p_wt_mod_2 <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Ancestral -1,
                                             random      = ~ LineB + BlockRep,
                                             family      = "threshold",
                                             data        = Vg_Otipulae_bi_CONTROL_woutCB,
                                             prior       = prior_bi_block,
                                             nitt        = 1260000,       
                                             thin        = 500,           
                                             burnin      = 10000,
                                             trunc       = TRUE,
                                             pr          = TRUE,
                                             pl          = TRUE)            
    
    saveRDS(Otipulae_CONTROL_P5p_wt_mod_2, file = "Otipulae_CONTROL_P5p_wt_mod_2.rds")
    Otipulae_CONTROL_P5p_wt_mod_2 <- readRDS("Otipulae_CONTROL_P5p_wt_mod_2.rds")
    
    summary(Otipulae_CONTROL_P5p_wt_mod_2) 
    plotTrace(Otipulae_CONTROL_P5p_wt_mod_2$Sol)
    
    # traces and posterior densities
    
    pdf("Otipulae_CONTROL_P5p_wt_mod_2.pdf")
    plot(Otipulae_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:4)]) 
    plot(Otipulae_CONTROL_P5p_wt_mod_2[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:4)]) #
    heidel.diag(Otipulae_CONTROL_P5p_wt_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:4)])
    #autocorr.plot(Otipulae_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:4)]) #
    autocorr.diag(Otipulae_CONTROL_P5p_wt_mod_2[["VCV"]])
    #autocorr.plot(Otipulae_CONTROL_P5p_wt_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:4)]) # 
    effectiveSize(Otipulae_CONTROL_P5p_wt_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_CONTROL_P5p_wt_mod_2 <- Otipulae_CONTROL_P5p_wt_mod_2[["VCV"]][ , "LineB"]
      vr_Otipulae_CONTROL_P5p_wt_mod_2 <- Otipulae_CONTROL_P5p_wt_mod_2[["VCV"]][ , "units"]
      vlat_Otipulae_CONTROL_P5p_wt_mod_2 <- rowSums(Otipulae_CONTROL_P5p_wt_mod_2[["VCV"]])
      
      mean(va_liab_Otipulae_CONTROL_P5p_wt_mod_2) 
      HPDinterval(va_liab_Otipulae_CONTROL_P5p_wt_mod_2) 
      
      mean(vlat_Otipulae_CONTROL_P5p_wt_mod_2) 
      
      #variance of fixed effects
      X_Otipulae_CONTROL_P5p_wt_mod_2 <- Otipulae_CONTROL_P5p_wt_mod_2[["X"]]
      beta_Otipulae_CONTROL_P5p_wt_mod_2 <- Otipulae_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:4)]
      vf_Otipulae_CONTROL_P5p_wt_mod_2   <- apply(beta_Otipulae_CONTROL_P5p_wt_mod_2, 1, function(b) {var(as.vector(X_Otipulae_CONTROL_P5p_wt_mod_2 %*% b))}) 
      mean(vf_Otipulae_CONTROL_P5p_wt_mod_2) 
      
      h2_liab_Otipulae_CONTROL_P5p_wt_mod_2 <- va_liab_Otipulae_CONTROL_P5p_wt_mod_2 / (vlat_Otipulae_CONTROL_P5p_wt_mod_2 + vf_Otipulae_CONTROL_P5p_wt_mod_2)
      mean(h2_liab_Otipulae_CONTROL_P5p_wt_mod_2) 
      posterior.mode(h2_liab_Otipulae_CONTROL_P5p_wt_mod_2)	
      median(h2_liab_Otipulae_CONTROL_P5p_wt_mod_2)		
      HPDinterval(h2_liab_Otipulae_CONTROL_P5p_wt_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_CONTROL_P5p_wt_mod_2 <- ((rowMeans(Otipulae_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Otipulae_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:3)]) + Otipulae_CONTROL_P5p_wt_mod_2[["Sol"]][,4]))/2)
    
    Evol_liab_Otipulae_CONTROL_P5p_wt_mod_2 <- (va_liab_Otipulae_CONTROL_P5p_wt_mod_2/2) / (trait_mean_liab_Otipulae_CONTROL_P5p_wt_mod_2)^2
    mean(Evol_liab_Otipulae_CONTROL_P5p_wt_mod_2)
    
    
    #Otipulae_CONTROL_P5p_wt_mod_2 data scale
    {
      
      predict_Otipulae_CONTROL_P5p_wt_mod_2 <- map(1:nrow(Otipulae_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:4)]), ~ as.vector(X_Otipulae_CONTROL_P5p_wt_mod_2 %*% Otipulae_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:4)][., ]))
      
      data_Otipulae_CONTROL_P5p_wt_mod_2 <-
        pmap_dfr(list(predict = predict_Otipulae_CONTROL_P5p_wt_mod_2,
                      var.a = Otipulae_CONTROL_P5p_wt_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_CONTROL_P5p_wt_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_CONTROL_P5p_wt_mod_2 <- data_Otipulae_CONTROL_P5p_wt_mod_2[["h2.obs"]]
      trait_mean_data_Otipulae_CONTROL_P5p_wt_mod_2 <- data_Otipulae_CONTROL_P5p_wt_mod_2[["mean.obs"]]
      va_data_Otipulae_CONTROL_P5p_wt_mod_2 <- data_Otipulae_CONTROL_P5p_wt_mod_2[["var.a.obs"]]
      vp_data_Otipulae_CONTROL_P5p_wt_mod_2 <- data_Otipulae_CONTROL_P5p_wt_mod_2[["var.obs"]]
      
      Evol_data_Otipulae_CONTROL_P5p_wt_mod_2 <- (va_data_Otipulae_CONTROL_P5p_wt_mod_2/2) / (trait_mean_data_Otipulae_CONTROL_P5p_wt_mod_2)^2
      
      mean(h2_data_Otipulae_CONTROL_P5p_wt_mod_2) 
      mean(trait_mean_data_Otipulae_CONTROL_P5p_wt_mod_2)
      mean(va_data_Otipulae_CONTROL_P5p_wt_mod_2) 
      mean(vp_data_Otipulae_CONTROL_P5p_wt_mod_2)
      mean(Evol_data_Otipulae_CONTROL_P5p_wt_mod_2)
      
    }
    
  }
  
  #Otipulae_WILD_P5p_wt_mod   {
    
    Otipulae_WILD_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vg_Otipulae_bi_WILD,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)         
    
    saveRDS(Otipulae_WILD_P5p_wt_mod, file = "Otipulae_WILD_P5p_wt_mod.rds")
    Otipulae_WILD_P5p_wt_mod <- readRDS("Otipulae_WILD_P5p_wt_mod.rds")
    
    summary(Otipulae_WILD_P5p_wt_mod) 
    #plot(Otipulae_WILD_P5p_wt_mod)
    
    # traces and posterior densities
    pdf("Otipulae_WILD_P5p_wt_mod.pdf")
    plot(Otipulae_WILD_P5p_wt_mod[["Sol"]][,c(1:3)]) 
    plot(Otipulae_WILD_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_WILD_P5p_wt_mod[["Sol"]][,c(1:3)]) #
    heidel.diag(Otipulae_WILD_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_WILD_P5p_wt_mod[["Sol"]][,c(1:3)])
    #autocorr.plot(Otipulae_WILD_P5p_wt_mod[["Sol"]][,c(1:3)]) #
    autocorr.diag(Otipulae_WILD_P5p_wt_mod[["VCV"]])
    #autocorr.plot(Otipulae_WILD_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_WILD_P5p_wt_mod[["Sol"]][,c(1:3)]) # 
    effectiveSize(Otipulae_WILD_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_WILD_P5p_wt_mod <- Otipulae_WILD_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_WILD_P5p_wt_mod <- Otipulae_WILD_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_Otipulae_WILD_P5p_wt_mod <- rowSums(Otipulae_WILD_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_Otipulae_WILD_P5p_wt_mod) 
      HPDinterval(va_liab_Otipulae_WILD_P5p_wt_mod) 
      
      mean(vlat_Otipulae_WILD_P5p_wt_mod) 
      
      #variance of fixed effects
      X_Otipulae_WILD_P5p_wt_mod <- Otipulae_WILD_P5p_wt_mod[["X"]]
      beta_Otipulae_WILD_P5p_wt_mod <- Otipulae_WILD_P5p_wt_mod[["Sol"]][,c(1:3)]
      vf_Otipulae_WILD_P5p_wt_mod   <- apply(beta_Otipulae_WILD_P5p_wt_mod, 1, function(b) {var(as.vector(X_Otipulae_WILD_P5p_wt_mod %*% b))}) 
      mean(vf_Otipulae_WILD_P5p_wt_mod) 
      
      h2_liab_Otipulae_WILD_P5p_wt_mod <- va_liab_Otipulae_WILD_P5p_wt_mod / (vlat_Otipulae_WILD_P5p_wt_mod + vf_Otipulae_WILD_P5p_wt_mod)
      mean(h2_liab_Otipulae_WILD_P5p_wt_mod) 
      posterior.mode(h2_liab_Otipulae_WILD_P5p_wt_mod)	
      median(h2_liab_Otipulae_WILD_P5p_wt_mod)		
      HPDinterval(h2_liab_Otipulae_WILD_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Otipulae_WILD_P5p_wt_mod <- rowMeans(Otipulae_WILD_P5p_wt_mod[["Sol"]][,c(1:3)])
    
    Evol_liab_Otipulae_WILD_P5p_wt_mod <- (va_liab_Otipulae_WILD_P5p_wt_mod/2) / (trait_mean_liab_Otipulae_WILD_P5p_wt_mod)^2
    mean(Evol_liab_Otipulae_WILD_P5p_wt_mod)
    
    
    #Otipulae_WILD_P5p_wt_mod data scale
    {
      
      predict_Otipulae_WILD_P5p_wt_mod <- map(1:nrow(Otipulae_WILD_P5p_wt_mod[["Sol"]][,c(1:3)]), ~ as.vector(X_Otipulae_WILD_P5p_wt_mod %*% Otipulae_WILD_P5p_wt_mod[["Sol"]][,c(1:3)][., ]))
      
      data_Otipulae_WILD_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_Otipulae_WILD_P5p_wt_mod,
                      var.a = Otipulae_WILD_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_WILD_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_WILD_P5p_wt_mod <- data_Otipulae_WILD_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_Otipulae_WILD_P5p_wt_mod <- data_Otipulae_WILD_P5p_wt_mod[["mean.obs"]]
      va_data_Otipulae_WILD_P5p_wt_mod <- data_Otipulae_WILD_P5p_wt_mod[["var.a.obs"]]
      vp_data_Otipulae_WILD_P5p_wt_mod <- data_Otipulae_WILD_P5p_wt_mod[["var.obs"]]
      
      Evol_data_Otipulae_WILD_P5p_wt_mod <- (va_data_Otipulae_WILD_P5p_wt_mod/2) / (trait_mean_data_Otipulae_WILD_P5p_wt_mod)^2
      
      mean(h2_data_Otipulae_WILD_P5p_wt_mod)
      mean(trait_mean_data_Otipulae_WILD_P5p_wt_mod)
      mean(va_data_Otipulae_WILD_P5p_wt_mod)
      mean(vp_data_Otipulae_WILD_P5p_wt_mod)
      mean(Evol_data_Otipulae_WILD_P5p_wt_mod)
      
    }
    
  }
  
  #Otipulae_Vg_P5p_wt_mod_2   {
    
    Otipulae_Vg_P5p_wt_mod_2 <- MCMCglmm(fixed       = P5.p_wt ~ Observer + at.level(Treatment,1):Ancestral + at.level(Treatment,2):Ancestral -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vg_Otipulae_data_woutCB,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)        
    
    
    saveRDS(Otipulae_Vg_P5p_wt_mod_2, file = "Otipulae_Vg_P5p_wt_mod_2.rds")
    Otipulae_Vg_P5p_wt_mod_2 <- readRDS("Otipulae_Vg_P5p_wt_mod_2.rds")
    
    summary(Otipulae_Vg_P5p_wt_mod_2) 
    plotTrace(Otipulae_Vg_P5p_wt_mod_2$Sol)
    View(Otipulae_Vg_P5p_wt_mod_2[["Sol"]])
    # traces and posterior densities
    pdf("Otipulae_Vg_P5p_wt_mod_2.pdf")
    plot(Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,c(1:5)]) 
    plot(Otipulae_Vg_P5p_wt_mod_2[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_Vg_P5p_wt_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_Vg_P5p_wt_mod_2[["VCV"]])
    #autocorr.plot(Otipulae_Vg_P5p_wt_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_Vg_P5p_wt_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_Vg_P5p_wt_mod_2 <- Otipulae_Vg_P5p_wt_mod_2[["VCV"]][ , "LineB"]
      vr_Otipulae_Vg_P5p_wt_mod_2 <- Otipulae_Vg_P5p_wt_mod_2[["VCV"]][ , "units"]
      vlat_Otipulae_Vg_P5p_wt_mod_2 <- rowSums(Otipulae_Vg_P5p_wt_mod_2[["VCV"]])
      
      mean(va_liab_Otipulae_Vg_P5p_wt_mod_2) 
      HPDinterval(va_liab_Otipulae_Vg_P5p_wt_mod_2) 
      
      mean(vlat_Otipulae_Vg_P5p_wt_mod_2) 
      
      #variance of fixed effects
      X_Otipulae_Vg_P5p_wt_mod_2 <- Otipulae_Vg_P5p_wt_mod_2[["X"]]
      beta_Otipulae_Vg_P5p_wt_mod_2 <- Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,c(1:5)]
      vf_Otipulae_Vg_P5p_wt_mod_2   <- apply(beta_Otipulae_Vg_P5p_wt_mod_2, 1, function(b) {var(as.vector(X_Otipulae_Vg_P5p_wt_mod_2 %*% b))}) 
      mean(vf_Otipulae_Vg_P5p_wt_mod_2) 
      
      h2_liab_Otipulae_Vg_P5p_wt_mod_2 <- va_liab_Otipulae_Vg_P5p_wt_mod_2 / (vlat_Otipulae_Vg_P5p_wt_mod_2 + vf_Otipulae_Vg_P5p_wt_mod_2)
      mean(h2_liab_Otipulae_Vg_P5p_wt_mod_2) 
      posterior.mode(h2_liab_Otipulae_Vg_P5p_wt_mod_2)	
      median(h2_liab_Otipulae_Vg_P5p_wt_mod_2)		
      HPDinterval(h2_liab_Otipulae_Vg_P5p_wt_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,c(1:3)])) #Otip WILD P5p_wt
    mean(rowMeans(Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,4]) #CEW1 CONTROL P5p_wt
    mean(rowMeans(Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,5]) #JU178 Control P5p_wt
    
    trait_mean_liab_Otipulae_Vg_P5p_wt_mod_2 <- ((rowMeans(Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,4]) + (rowMeans(Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,5]) )/3)
    
    Evol_liab_Otipulae_Vg_P5p_wt_mod_2 <- (va_liab_Otipulae_Vg_P5p_wt_mod_2/2) / (trait_mean_liab_Otipulae_Vg_P5p_wt_mod_2)^2
    mean(Evol_liab_Otipulae_Vg_P5p_wt_mod_2)
    
    
    #Otipulae_Vg_P5p_wt_mod_2 data scale
    {
      
      predict_Otipulae_Vg_P5p_wt_mod_2 <- map(1:nrow(Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_Vg_P5p_wt_mod_2 %*% Otipulae_Vg_P5p_wt_mod_2[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_Vg_P5p_wt_mod_2 <-
        pmap_dfr(list(predict = predict_Otipulae_Vg_P5p_wt_mod_2,
                      var.a = Otipulae_Vg_P5p_wt_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_Vg_P5p_wt_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_Vg_P5p_wt_mod_2 <- data_Otipulae_Vg_P5p_wt_mod_2[["h2.obs"]]
      trait_mean_data_Otipulae_Vg_P5p_wt_mod_2 <- data_Otipulae_Vg_P5p_wt_mod_2[["mean.obs"]]
      va_data_Otipulae_Vg_P5p_wt_mod_2 <- data_Otipulae_Vg_P5p_wt_mod_2[["var.a.obs"]]
      vp_data_Otipulae_Vg_P5p_wt_mod_2 <- data_Otipulae_Vg_P5p_wt_mod_2[["var.obs"]]
      
      Evol_data_Otipulae_Vg_P5p_wt_mod_2 <- (va_data_Otipulae_Vg_P5p_wt_mod_2/2) / (trait_mean_data_Otipulae_Vg_P5p_wt_mod_2)^2
      
      mean(h2_data_Otipulae_Vg_P5p_wt_mod_2)
      mean(trait_mean_data_Otipulae_Vg_P5p_wt_mod_2)
      mean(va_data_Otipulae_Vg_P5p_wt_mod_2)
      mean(vp_data_Otipulae_Vg_P5p_wt_mod_2)
      mean(Evol_data_Otipulae_Vg_P5p_wt_mod_2)
      
    }
    
  }
  
  
  
  ## Otipulae P6p ----
  
  
  #Otipulae_CONTROL_P6p_wt_mod_2   {
    
    Otipulae_CONTROL_P6p_wt_mod_2 <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Ancestral -1,
                                             random      = ~ LineB + BlockRep,
                                             family      = "threshold",
                                             data        = Vg_Otipulae_bi_CONTROL_woutCB,
                                             prior       = prior_bi_block,
                                             nitt        = 1260000,       
                                             thin        = 500,           
                                             burnin      = 10000,
                                             trunc       = TRUE,
                                             pr          = TRUE,
                                             pl          = TRUE)            
    
    saveRDS(Otipulae_CONTROL_P6p_wt_mod_2, file = "Otipulae_CONTROL_P6p_wt_mod_2.rds")
    Otipulae_CONTROL_P6p_wt_mod_2 <- readRDS("Otipulae_CONTROL_P6p_wt_mod_2.rds")
    
    summary(Otipulae_CONTROL_P6p_wt_mod_2) 
    plotTrace(Otipulae_CONTROL_P6p_wt_mod_2$Sol)
    
    # traces and posterior densities
    
    pdf("Otipulae_CONTROL_P6p_wt_mod_2.pdf")
    plot(Otipulae_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:4)]) 
    plot(Otipulae_CONTROL_P6p_wt_mod_2[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:4)]) #
    heidel.diag(Otipulae_CONTROL_P6p_wt_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:4)])
    #autocorr.plot(Otipulae_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:4)]) #
    autocorr.diag(Otipulae_CONTROL_P6p_wt_mod_2[["VCV"]])
    #autocorr.plot(Otipulae_CONTROL_P6p_wt_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:4)]) # 
    effectiveSize(Otipulae_CONTROL_P6p_wt_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_CONTROL_P6p_wt_mod_2 <- Otipulae_CONTROL_P6p_wt_mod_2[["VCV"]][ , "LineB"]
      vr_Otipulae_CONTROL_P6p_wt_mod_2 <- Otipulae_CONTROL_P6p_wt_mod_2[["VCV"]][ , "units"]
      vlat_Otipulae_CONTROL_P6p_wt_mod_2 <- rowSums(Otipulae_CONTROL_P6p_wt_mod_2[["VCV"]])
      
      mean(va_liab_Otipulae_CONTROL_P6p_wt_mod_2) 
      HPDinterval(va_liab_Otipulae_CONTROL_P6p_wt_mod_2) 
      
      mean(vlat_Otipulae_CONTROL_P6p_wt_mod_2) 
      
      #variance of fixed effects
      X_Otipulae_CONTROL_P6p_wt_mod_2 <- Otipulae_CONTROL_P6p_wt_mod_2[["X"]]
      beta_Otipulae_CONTROL_P6p_wt_mod_2 <- Otipulae_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:4)]
      vf_Otipulae_CONTROL_P6p_wt_mod_2   <- apply(beta_Otipulae_CONTROL_P6p_wt_mod_2, 1, function(b) {var(as.vector(X_Otipulae_CONTROL_P6p_wt_mod_2 %*% b))}) 
      mean(vf_Otipulae_CONTROL_P6p_wt_mod_2) 
      
      h2_liab_Otipulae_CONTROL_P6p_wt_mod_2 <- va_liab_Otipulae_CONTROL_P6p_wt_mod_2 / (vlat_Otipulae_CONTROL_P6p_wt_mod_2 + vf_Otipulae_CONTROL_P6p_wt_mod_2)
      mean(h2_liab_Otipulae_CONTROL_P6p_wt_mod_2) 
      posterior.mode(h2_liab_Otipulae_CONTROL_P6p_wt_mod_2)	
      median(h2_liab_Otipulae_CONTROL_P6p_wt_mod_2)		
      HPDinterval(h2_liab_Otipulae_CONTROL_P6p_wt_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_CONTROL_P6p_wt_mod_2 <- ((rowMeans(Otipulae_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Otipulae_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:3)]) + Otipulae_CONTROL_P6p_wt_mod_2[["Sol"]][,4]))/2)
    
    Evol_liab_Otipulae_CONTROL_P6p_wt_mod_2 <- (va_liab_Otipulae_CONTROL_P6p_wt_mod_2/2) / (trait_mean_liab_Otipulae_CONTROL_P6p_wt_mod_2)^2
    mean(Evol_liab_Otipulae_CONTROL_P6p_wt_mod_2)
    
    
    #Otipulae_CONTROL_P6p_wt_mod_2 data scale
    {
      
      predict_Otipulae_CONTROL_P6p_wt_mod_2 <- map(1:nrow(Otipulae_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:4)]), ~ as.vector(X_Otipulae_CONTROL_P6p_wt_mod_2 %*% Otipulae_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:4)][., ]))
      
      data_Otipulae_CONTROL_P6p_wt_mod_2 <-
        pmap_dfr(list(predict = predict_Otipulae_CONTROL_P6p_wt_mod_2,
                      var.a = Otipulae_CONTROL_P6p_wt_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_CONTROL_P6p_wt_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_CONTROL_P6p_wt_mod_2 <- data_Otipulae_CONTROL_P6p_wt_mod_2[["h2.obs"]]
      trait_mean_data_Otipulae_CONTROL_P6p_wt_mod_2 <- data_Otipulae_CONTROL_P6p_wt_mod_2[["mean.obs"]]
      va_data_Otipulae_CONTROL_P6p_wt_mod_2 <- data_Otipulae_CONTROL_P6p_wt_mod_2[["var.a.obs"]]
      vp_data_Otipulae_CONTROL_P6p_wt_mod_2 <- data_Otipulae_CONTROL_P6p_wt_mod_2[["var.obs"]]
      
      Evol_data_Otipulae_CONTROL_P6p_wt_mod_2 <- (va_data_Otipulae_CONTROL_P6p_wt_mod_2/2) / (trait_mean_data_Otipulae_CONTROL_P6p_wt_mod_2)^2
      
      mean(h2_data_Otipulae_CONTROL_P6p_wt_mod_2) 
      mean(trait_mean_data_Otipulae_CONTROL_P6p_wt_mod_2)
      mean(va_data_Otipulae_CONTROL_P6p_wt_mod_2) 
      mean(vp_data_Otipulae_CONTROL_P6p_wt_mod_2)
      mean(Evol_data_Otipulae_CONTROL_P6p_wt_mod_2)
      
    }
    
  }
  
  #Otipulae_WILD_P6p_wt_mod   {
    
    Otipulae_WILD_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vg_Otipulae_bi_WILD,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)         
    
    saveRDS(Otipulae_WILD_P6p_wt_mod, file = "Otipulae_WILD_P6p_wt_mod.rds")
    Otipulae_WILD_P6p_wt_mod <- readRDS("Otipulae_WILD_P6p_wt_mod.rds")
    
    summary(Otipulae_WILD_P6p_wt_mod) 
    #plot(Otipulae_WILD_P6p_wt_mod)
    
    # traces and posterior densities
    pdf("Otipulae_WILD_P6p_wt_mod.pdf")
    plot(Otipulae_WILD_P6p_wt_mod[["Sol"]][,c(1:3)]) 
    plot(Otipulae_WILD_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_WILD_P6p_wt_mod[["Sol"]][,c(1:3)]) #
    heidel.diag(Otipulae_WILD_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_WILD_P6p_wt_mod[["Sol"]][,c(1:3)])
    #autocorr.plot(Otipulae_WILD_P6p_wt_mod[["Sol"]][,c(1:3)]) #
    autocorr.diag(Otipulae_WILD_P6p_wt_mod[["VCV"]])
    #autocorr.plot(Otipulae_WILD_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_WILD_P6p_wt_mod[["Sol"]][,c(1:3)]) # 
    effectiveSize(Otipulae_WILD_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_WILD_P6p_wt_mod <- Otipulae_WILD_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_WILD_P6p_wt_mod <- Otipulae_WILD_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_Otipulae_WILD_P6p_wt_mod <- rowSums(Otipulae_WILD_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_Otipulae_WILD_P6p_wt_mod) 
      HPDinterval(va_liab_Otipulae_WILD_P6p_wt_mod) 
      
      mean(vlat_Otipulae_WILD_P6p_wt_mod) 
      
      #variance of fixed effects
      X_Otipulae_WILD_P6p_wt_mod <- Otipulae_WILD_P6p_wt_mod[["X"]]
      beta_Otipulae_WILD_P6p_wt_mod <- Otipulae_WILD_P6p_wt_mod[["Sol"]][,c(1:3)]
      vf_Otipulae_WILD_P6p_wt_mod   <- apply(beta_Otipulae_WILD_P6p_wt_mod, 1, function(b) {var(as.vector(X_Otipulae_WILD_P6p_wt_mod %*% b))}) 
      mean(vf_Otipulae_WILD_P6p_wt_mod) 
      
      h2_liab_Otipulae_WILD_P6p_wt_mod <- va_liab_Otipulae_WILD_P6p_wt_mod / (vlat_Otipulae_WILD_P6p_wt_mod + vf_Otipulae_WILD_P6p_wt_mod)
      mean(h2_liab_Otipulae_WILD_P6p_wt_mod) 
      posterior.mode(h2_liab_Otipulae_WILD_P6p_wt_mod)	
      median(h2_liab_Otipulae_WILD_P6p_wt_mod)		
      HPDinterval(h2_liab_Otipulae_WILD_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Otipulae_WILD_P6p_wt_mod <- rowMeans(Otipulae_WILD_P6p_wt_mod[["Sol"]][,c(1:3)])
    
    Evol_liab_Otipulae_WILD_P6p_wt_mod <- (va_liab_Otipulae_WILD_P6p_wt_mod/2) / (trait_mean_liab_Otipulae_WILD_P6p_wt_mod)^2
    mean(Evol_liab_Otipulae_WILD_P6p_wt_mod)
    
    
    #Otipulae_WILD_P6p_wt_mod data scale
    {
      
      predict_Otipulae_WILD_P6p_wt_mod <- map(1:nrow(Otipulae_WILD_P6p_wt_mod[["Sol"]][,c(1:3)]), ~ as.vector(X_Otipulae_WILD_P6p_wt_mod %*% Otipulae_WILD_P6p_wt_mod[["Sol"]][,c(1:3)][., ]))
      
      data_Otipulae_WILD_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_Otipulae_WILD_P6p_wt_mod,
                      var.a = Otipulae_WILD_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_WILD_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_WILD_P6p_wt_mod <- data_Otipulae_WILD_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_Otipulae_WILD_P6p_wt_mod <- data_Otipulae_WILD_P6p_wt_mod[["mean.obs"]]
      va_data_Otipulae_WILD_P6p_wt_mod <- data_Otipulae_WILD_P6p_wt_mod[["var.a.obs"]]
      vp_data_Otipulae_WILD_P6p_wt_mod <- data_Otipulae_WILD_P6p_wt_mod[["var.obs"]]
      
      Evol_data_Otipulae_WILD_P6p_wt_mod <- (va_data_Otipulae_WILD_P6p_wt_mod/2) / (trait_mean_data_Otipulae_WILD_P6p_wt_mod)^2
      
      mean(h2_data_Otipulae_WILD_P6p_wt_mod)
      mean(trait_mean_data_Otipulae_WILD_P6p_wt_mod)
      mean(va_data_Otipulae_WILD_P6p_wt_mod)
      mean(vp_data_Otipulae_WILD_P6p_wt_mod)
      mean(Evol_data_Otipulae_WILD_P6p_wt_mod)
      
    }
    
  }
  
  
  #Otipulae_Vg_P6p_wt_mod_2   {
    
    Otipulae_Vg_P6p_wt_mod_2 <- MCMCglmm(fixed       = P6.p_wt ~ Observer + at.level(Treatment,1):Ancestral + at.level(Treatment,2):Ancestral -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vg_Otipulae_data_woutCB,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)        
    
   
    saveRDS(Otipulae_Vg_P6p_wt_mod_2, file = "Otipulae_Vg_P6p_wt_mod_2.rds")
    Otipulae_Vg_P6p_wt_mod_2 <- readRDS("Otipulae_Vg_P6p_wt_mod_2.rds")
    
    summary(Otipulae_Vg_P6p_wt_mod_2) 
    plotTrace(Otipulae_Vg_P6p_wt_mod_2$Sol)
    View(Otipulae_Vg_P6p_wt_mod_2[["Sol"]])
    # traces and posterior densities
    pdf("Otipulae_Vg_P6p_wt_mod_2.pdf")
    plot(Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,c(1:5)]) 
    plot(Otipulae_Vg_P6p_wt_mod_2[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_Vg_P6p_wt_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_Vg_P6p_wt_mod_2[["VCV"]])
    #autocorr.plot(Otipulae_Vg_P6p_wt_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_Vg_P6p_wt_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_Vg_P6p_wt_mod_2 <- Otipulae_Vg_P6p_wt_mod_2[["VCV"]][ , "LineB"]
      vr_Otipulae_Vg_P6p_wt_mod_2 <- Otipulae_Vg_P6p_wt_mod_2[["VCV"]][ , "units"]
      vlat_Otipulae_Vg_P6p_wt_mod_2 <- rowSums(Otipulae_Vg_P6p_wt_mod_2[["VCV"]])
      
      mean(va_liab_Otipulae_Vg_P6p_wt_mod_2) 
      HPDinterval(va_liab_Otipulae_Vg_P6p_wt_mod_2) 
      
      mean(vlat_Otipulae_Vg_P6p_wt_mod_2) 
      
      #variance of fixed effects
      X_Otipulae_Vg_P6p_wt_mod_2 <- Otipulae_Vg_P6p_wt_mod_2[["X"]]
      beta_Otipulae_Vg_P6p_wt_mod_2 <- Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,c(1:5)]
      vf_Otipulae_Vg_P6p_wt_mod_2   <- apply(beta_Otipulae_Vg_P6p_wt_mod_2, 1, function(b) {var(as.vector(X_Otipulae_Vg_P6p_wt_mod_2 %*% b))}) 
      mean(vf_Otipulae_Vg_P6p_wt_mod_2) 
      
      h2_liab_Otipulae_Vg_P6p_wt_mod_2 <- va_liab_Otipulae_Vg_P6p_wt_mod_2 / (vlat_Otipulae_Vg_P6p_wt_mod_2 + vf_Otipulae_Vg_P6p_wt_mod_2)
      mean(h2_liab_Otipulae_Vg_P6p_wt_mod_2) 
      posterior.mode(h2_liab_Otipulae_Vg_P6p_wt_mod_2)	
      median(h2_liab_Otipulae_Vg_P6p_wt_mod_2)		
      HPDinterval(h2_liab_Otipulae_Vg_P6p_wt_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,c(1:3)])) #Otip WILD P6p_wt
    mean(rowMeans(Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,4]) #CEW1 CONTROL P6p_wt
    mean(rowMeans(Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,5]) #JU178 Control P6p_wt
    
    trait_mean_liab_Otipulae_Vg_P6p_wt_mod_2 <- ((rowMeans(Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,4]) + (rowMeans(Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,5]) )/3)
    
    Evol_liab_Otipulae_Vg_P6p_wt_mod_2 <- (va_liab_Otipulae_Vg_P6p_wt_mod_2/2) / (trait_mean_liab_Otipulae_Vg_P6p_wt_mod_2)^2
    mean(Evol_liab_Otipulae_Vg_P6p_wt_mod_2)
    
    
    #Otipulae_Vg_P6p_wt_mod_2 data scale
    {
      
      predict_Otipulae_Vg_P6p_wt_mod_2 <- map(1:nrow(Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_Vg_P6p_wt_mod_2 %*% Otipulae_Vg_P6p_wt_mod_2[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_Vg_P6p_wt_mod_2 <-
        pmap_dfr(list(predict = predict_Otipulae_Vg_P6p_wt_mod_2,
                      var.a = Otipulae_Vg_P6p_wt_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_Vg_P6p_wt_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_Vg_P6p_wt_mod_2 <- data_Otipulae_Vg_P6p_wt_mod_2[["h2.obs"]]
      trait_mean_data_Otipulae_Vg_P6p_wt_mod_2 <- data_Otipulae_Vg_P6p_wt_mod_2[["mean.obs"]]
      va_data_Otipulae_Vg_P6p_wt_mod_2 <- data_Otipulae_Vg_P6p_wt_mod_2[["var.a.obs"]]
      vp_data_Otipulae_Vg_P6p_wt_mod_2 <- data_Otipulae_Vg_P6p_wt_mod_2[["var.obs"]]
      
      Evol_data_Otipulae_Vg_P6p_wt_mod_2 <- (va_data_Otipulae_Vg_P6p_wt_mod_2/2) / (trait_mean_data_Otipulae_Vg_P6p_wt_mod_2)^2
      
      mean(h2_data_Otipulae_Vg_P6p_wt_mod_2)
      mean(trait_mean_data_Otipulae_Vg_P6p_wt_mod_2)
      mean(va_data_Otipulae_Vg_P6p_wt_mod_2)
      mean(vp_data_Otipulae_Vg_P6p_wt_mod_2)
      mean(Evol_data_Otipulae_Vg_P6p_wt_mod_2)
      
    }
    
  }
  
 
  ## Otipulae P7p ----
  
  
  #Otipulae_CONTROL_P7p_wt_mod_2   {
    
    Otipulae_CONTROL_P7p_wt_mod_2 <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Ancestral -1,
                                             random      = ~ LineB + BlockRep,
                                             family      = "threshold",
                                             data        = Vg_Otipulae_bi_CONTROL_woutCB,
                                             prior       = prior_bi_block,
                                             nitt        = 1260000,       
                                             thin        = 500,           
                                             burnin      = 10000,
                                             trunc       = TRUE,
                                             pr          = TRUE,
                                             pl          = TRUE)            
    
    saveRDS(Otipulae_CONTROL_P7p_wt_mod_2, file = "Otipulae_CONTROL_P7p_wt_mod_2.rds")
    Otipulae_CONTROL_P7p_wt_mod_2 <- readRDS("Otipulae_CONTROL_P7p_wt_mod_2.rds")
    
    summary(Otipulae_CONTROL_P7p_wt_mod_2) 
    plotTrace(Otipulae_CONTROL_P7p_wt_mod_2$Sol)
    
    # traces and posterior densities
    
    pdf("Otipulae_CONTROL_P7p_wt_mod_2.pdf")
    plot(Otipulae_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:4)]) 
    plot(Otipulae_CONTROL_P7p_wt_mod_2[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:4)]) #
    heidel.diag(Otipulae_CONTROL_P7p_wt_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:4)])
    #autocorr.plot(Otipulae_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:4)]) #
    autocorr.diag(Otipulae_CONTROL_P7p_wt_mod_2[["VCV"]])
    #autocorr.plot(Otipulae_CONTROL_P7p_wt_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:4)]) # 
    effectiveSize(Otipulae_CONTROL_P7p_wt_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_CONTROL_P7p_wt_mod_2 <- Otipulae_CONTROL_P7p_wt_mod_2[["VCV"]][ , "LineB"]
      vr_Otipulae_CONTROL_P7p_wt_mod_2 <- Otipulae_CONTROL_P7p_wt_mod_2[["VCV"]][ , "units"]
      vlat_Otipulae_CONTROL_P7p_wt_mod_2 <- rowSums(Otipulae_CONTROL_P7p_wt_mod_2[["VCV"]])
      
      mean(va_liab_Otipulae_CONTROL_P7p_wt_mod_2) 
      HPDinterval(va_liab_Otipulae_CONTROL_P7p_wt_mod_2) 
      
      mean(vlat_Otipulae_CONTROL_P7p_wt_mod_2) 
      
      #variance of fixed effects
      X_Otipulae_CONTROL_P7p_wt_mod_2 <- Otipulae_CONTROL_P7p_wt_mod_2[["X"]]
      beta_Otipulae_CONTROL_P7p_wt_mod_2 <- Otipulae_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:4)]
      vf_Otipulae_CONTROL_P7p_wt_mod_2   <- apply(beta_Otipulae_CONTROL_P7p_wt_mod_2, 1, function(b) {var(as.vector(X_Otipulae_CONTROL_P7p_wt_mod_2 %*% b))}) 
      mean(vf_Otipulae_CONTROL_P7p_wt_mod_2) 
      
      h2_liab_Otipulae_CONTROL_P7p_wt_mod_2 <- va_liab_Otipulae_CONTROL_P7p_wt_mod_2 / (vlat_Otipulae_CONTROL_P7p_wt_mod_2 + vf_Otipulae_CONTROL_P7p_wt_mod_2)
      mean(h2_liab_Otipulae_CONTROL_P7p_wt_mod_2) 
      posterior.mode(h2_liab_Otipulae_CONTROL_P7p_wt_mod_2)	
      median(h2_liab_Otipulae_CONTROL_P7p_wt_mod_2)		
      HPDinterval(h2_liab_Otipulae_CONTROL_P7p_wt_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Otipulae_CONTROL_P7p_wt_mod_2 <- ((rowMeans(Otipulae_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Otipulae_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:3)]) + Otipulae_CONTROL_P7p_wt_mod_2[["Sol"]][,4]))/2)
    
    Evol_liab_Otipulae_CONTROL_P7p_wt_mod_2 <- (va_liab_Otipulae_CONTROL_P7p_wt_mod_2/2) / (trait_mean_liab_Otipulae_CONTROL_P7p_wt_mod_2)^2
    mean(Evol_liab_Otipulae_CONTROL_P7p_wt_mod_2)
    
    
    #Otipulae_CONTROL_P7p_wt_mod_2 data scale
    {
      
      predict_Otipulae_CONTROL_P7p_wt_mod_2 <- map(1:nrow(Otipulae_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:4)]), ~ as.vector(X_Otipulae_CONTROL_P7p_wt_mod_2 %*% Otipulae_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:4)][., ]))
      
      data_Otipulae_CONTROL_P7p_wt_mod_2 <-
        pmap_dfr(list(predict = predict_Otipulae_CONTROL_P7p_wt_mod_2,
                      var.a = Otipulae_CONTROL_P7p_wt_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_CONTROL_P7p_wt_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_CONTROL_P7p_wt_mod_2 <- data_Otipulae_CONTROL_P7p_wt_mod_2[["h2.obs"]]
      trait_mean_data_Otipulae_CONTROL_P7p_wt_mod_2 <- data_Otipulae_CONTROL_P7p_wt_mod_2[["mean.obs"]]
      va_data_Otipulae_CONTROL_P7p_wt_mod_2 <- data_Otipulae_CONTROL_P7p_wt_mod_2[["var.a.obs"]]
      vp_data_Otipulae_CONTROL_P7p_wt_mod_2 <- data_Otipulae_CONTROL_P7p_wt_mod_2[["var.obs"]]
      
      Evol_data_Otipulae_CONTROL_P7p_wt_mod_2 <- (va_data_Otipulae_CONTROL_P7p_wt_mod_2/2) / (trait_mean_data_Otipulae_CONTROL_P7p_wt_mod_2)^2
      
      mean(h2_data_Otipulae_CONTROL_P7p_wt_mod_2) 
      mean(trait_mean_data_Otipulae_CONTROL_P7p_wt_mod_2)
      mean(va_data_Otipulae_CONTROL_P7p_wt_mod_2) 
      mean(vp_data_Otipulae_CONTROL_P7p_wt_mod_2)
      mean(Evol_data_Otipulae_CONTROL_P7p_wt_mod_2)
      
    }
    
  }
  
  #Otipulae_WILD_P7p_wt_mod   {
    
    Otipulae_WILD_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vg_Otipulae_bi_WILD,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)         
    
    saveRDS(Otipulae_WILD_P7p_wt_mod, file = "Otipulae_WILD_P7p_wt_mod.rds")
    Otipulae_WILD_P7p_wt_mod <- readRDS("Otipulae_WILD_P7p_wt_mod.rds")
    
    summary(Otipulae_WILD_P7p_wt_mod) 
    #plot(Otipulae_WILD_P7p_wt_mod)
    
    # traces and posterior densities
    pdf("Otipulae_WILD_P7p_wt_mod.pdf")
    plot(Otipulae_WILD_P7p_wt_mod[["Sol"]][,c(1:3)]) 
    plot(Otipulae_WILD_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_WILD_P7p_wt_mod[["Sol"]][,c(1:3)]) #
    heidel.diag(Otipulae_WILD_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_WILD_P7p_wt_mod[["Sol"]][,c(1:3)])
    #autocorr.plot(Otipulae_WILD_P7p_wt_mod[["Sol"]][,c(1:3)]) #
    autocorr.diag(Otipulae_WILD_P7p_wt_mod[["VCV"]])
    #autocorr.plot(Otipulae_WILD_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_WILD_P7p_wt_mod[["Sol"]][,c(1:3)]) # 
    effectiveSize(Otipulae_WILD_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_WILD_P7p_wt_mod <- Otipulae_WILD_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_Otipulae_WILD_P7p_wt_mod <- Otipulae_WILD_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_Otipulae_WILD_P7p_wt_mod <- rowSums(Otipulae_WILD_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_Otipulae_WILD_P7p_wt_mod) 
      HPDinterval(va_liab_Otipulae_WILD_P7p_wt_mod) 
      
      mean(vlat_Otipulae_WILD_P7p_wt_mod) 
      
      #variance of fixed effects
      X_Otipulae_WILD_P7p_wt_mod <- Otipulae_WILD_P7p_wt_mod[["X"]]
      beta_Otipulae_WILD_P7p_wt_mod <- Otipulae_WILD_P7p_wt_mod[["Sol"]][,c(1:3)]
      vf_Otipulae_WILD_P7p_wt_mod   <- apply(beta_Otipulae_WILD_P7p_wt_mod, 1, function(b) {var(as.vector(X_Otipulae_WILD_P7p_wt_mod %*% b))}) 
      mean(vf_Otipulae_WILD_P7p_wt_mod) 
      
      h2_liab_Otipulae_WILD_P7p_wt_mod <- va_liab_Otipulae_WILD_P7p_wt_mod / (vlat_Otipulae_WILD_P7p_wt_mod + vf_Otipulae_WILD_P7p_wt_mod)
      mean(h2_liab_Otipulae_WILD_P7p_wt_mod) 
      posterior.mode(h2_liab_Otipulae_WILD_P7p_wt_mod)	
      median(h2_liab_Otipulae_WILD_P7p_wt_mod)		
      HPDinterval(h2_liab_Otipulae_WILD_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Otipulae_WILD_P7p_wt_mod <- rowMeans(Otipulae_WILD_P7p_wt_mod[["Sol"]][,c(1:3)])
    
    Evol_liab_Otipulae_WILD_P7p_wt_mod <- (va_liab_Otipulae_WILD_P7p_wt_mod/2) / (trait_mean_liab_Otipulae_WILD_P7p_wt_mod)^2
    mean(Evol_liab_Otipulae_WILD_P7p_wt_mod)
    
    
    #Otipulae_WILD_P7p_wt_mod data scale
    {
      
      predict_Otipulae_WILD_P7p_wt_mod <- map(1:nrow(Otipulae_WILD_P7p_wt_mod[["Sol"]][,c(1:3)]), ~ as.vector(X_Otipulae_WILD_P7p_wt_mod %*% Otipulae_WILD_P7p_wt_mod[["Sol"]][,c(1:3)][., ]))
      
      data_Otipulae_WILD_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_Otipulae_WILD_P7p_wt_mod,
                      var.a = Otipulae_WILD_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_WILD_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_WILD_P7p_wt_mod <- data_Otipulae_WILD_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_Otipulae_WILD_P7p_wt_mod <- data_Otipulae_WILD_P7p_wt_mod[["mean.obs"]]
      va_data_Otipulae_WILD_P7p_wt_mod <- data_Otipulae_WILD_P7p_wt_mod[["var.a.obs"]]
      vp_data_Otipulae_WILD_P7p_wt_mod <- data_Otipulae_WILD_P7p_wt_mod[["var.obs"]]
      
      Evol_data_Otipulae_WILD_P7p_wt_mod <- (va_data_Otipulae_WILD_P7p_wt_mod/2) / (trait_mean_data_Otipulae_WILD_P7p_wt_mod)^2
      
      mean(h2_data_Otipulae_WILD_P7p_wt_mod)
      mean(trait_mean_data_Otipulae_WILD_P7p_wt_mod)
      mean(va_data_Otipulae_WILD_P7p_wt_mod)
      mean(vp_data_Otipulae_WILD_P7p_wt_mod)
      mean(Evol_data_Otipulae_WILD_P7p_wt_mod)
      
    }
    
  }
  
  #Otipulae_Vg_P7p_wt_mod_2   {
    
    Otipulae_Vg_P7p_wt_mod_2 <- MCMCglmm(fixed       = P7.p_wt ~ Observer + at.level(Treatment,1):Ancestral + at.level(Treatment,2):Ancestral -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vg_Otipulae_data_woutCB,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)        
    
    
    saveRDS(Otipulae_Vg_P7p_wt_mod_2, file = "Otipulae_Vg_P7p_wt_mod_2.rds")
    Otipulae_Vg_P7p_wt_mod_2 <- readRDS("Otipulae_Vg_P7p_wt_mod_2.rds")
    
    summary(Otipulae_Vg_P7p_wt_mod_2) 
    plotTrace(Otipulae_Vg_P7p_wt_mod_2$Sol)
    View(Otipulae_Vg_P7p_wt_mod_2[["Sol"]])
    # traces and posterior densities
    pdf("Otipulae_Vg_P7p_wt_mod_2.pdf")
    plot(Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,c(1:5)]) 
    plot(Otipulae_Vg_P7p_wt_mod_2[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,c(1:5)]) #
    heidel.diag(Otipulae_Vg_P7p_wt_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,c(1:5)])
    #autocorr.plot(Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,c(1:5)]) #
    autocorr.diag(Otipulae_Vg_P7p_wt_mod_2[["VCV"]])
    #autocorr.plot(Otipulae_Vg_P7p_wt_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,c(1:5)]) # 
    effectiveSize(Otipulae_Vg_P7p_wt_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Otipulae_Vg_P7p_wt_mod_2 <- Otipulae_Vg_P7p_wt_mod_2[["VCV"]][ , "LineB"]
      vr_Otipulae_Vg_P7p_wt_mod_2 <- Otipulae_Vg_P7p_wt_mod_2[["VCV"]][ , "units"]
      vlat_Otipulae_Vg_P7p_wt_mod_2 <- rowSums(Otipulae_Vg_P7p_wt_mod_2[["VCV"]])
      
      mean(va_liab_Otipulae_Vg_P7p_wt_mod_2) 
      HPDinterval(va_liab_Otipulae_Vg_P7p_wt_mod_2) 
      
      mean(vlat_Otipulae_Vg_P7p_wt_mod_2) 
      
      #variance of fixed effects
      X_Otipulae_Vg_P7p_wt_mod_2 <- Otipulae_Vg_P7p_wt_mod_2[["X"]]
      beta_Otipulae_Vg_P7p_wt_mod_2 <- Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,c(1:5)]
      vf_Otipulae_Vg_P7p_wt_mod_2   <- apply(beta_Otipulae_Vg_P7p_wt_mod_2, 1, function(b) {var(as.vector(X_Otipulae_Vg_P7p_wt_mod_2 %*% b))}) 
      mean(vf_Otipulae_Vg_P7p_wt_mod_2) 
      
      h2_liab_Otipulae_Vg_P7p_wt_mod_2 <- va_liab_Otipulae_Vg_P7p_wt_mod_2 / (vlat_Otipulae_Vg_P7p_wt_mod_2 + vf_Otipulae_Vg_P7p_wt_mod_2)
      mean(h2_liab_Otipulae_Vg_P7p_wt_mod_2) 
      posterior.mode(h2_liab_Otipulae_Vg_P7p_wt_mod_2)	
      median(h2_liab_Otipulae_Vg_P7p_wt_mod_2)		
      HPDinterval(h2_liab_Otipulae_Vg_P7p_wt_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,c(1:3)])) #Otip WILD P7p_wt
    mean(rowMeans(Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,4]) #CEW1 CONTROL P7p_wt
    mean(rowMeans(Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,5]) #JU178 Control P7p_wt
    
    trait_mean_liab_Otipulae_Vg_P7p_wt_mod_2 <- ((rowMeans(Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,4]) + (rowMeans(Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,c(1:3)]) + Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,5]) )/3)
    
    Evol_liab_Otipulae_Vg_P7p_wt_mod_2 <- (va_liab_Otipulae_Vg_P7p_wt_mod_2/2) / (trait_mean_liab_Otipulae_Vg_P7p_wt_mod_2)^2
    mean(Evol_liab_Otipulae_Vg_P7p_wt_mod_2)
    
    
    #Otipulae_Vg_P7p_wt_mod_2 data scale
    {
      
      predict_Otipulae_Vg_P7p_wt_mod_2 <- map(1:nrow(Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,c(1:5)]), ~ as.vector(X_Otipulae_Vg_P7p_wt_mod_2 %*% Otipulae_Vg_P7p_wt_mod_2[["Sol"]][,c(1:5)][., ]))
      
      data_Otipulae_Vg_P7p_wt_mod_2 <-
        pmap_dfr(list(predict = predict_Otipulae_Vg_P7p_wt_mod_2,
                      var.a = Otipulae_Vg_P7p_wt_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Otipulae_Vg_P7p_wt_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Otipulae_Vg_P7p_wt_mod_2 <- data_Otipulae_Vg_P7p_wt_mod_2[["h2.obs"]]
      trait_mean_data_Otipulae_Vg_P7p_wt_mod_2 <- data_Otipulae_Vg_P7p_wt_mod_2[["mean.obs"]]
      va_data_Otipulae_Vg_P7p_wt_mod_2 <- data_Otipulae_Vg_P7p_wt_mod_2[["var.a.obs"]]
      vp_data_Otipulae_Vg_P7p_wt_mod_2 <- data_Otipulae_Vg_P7p_wt_mod_2[["var.obs"]]
      
      Evol_data_Otipulae_Vg_P7p_wt_mod_2 <- (va_data_Otipulae_Vg_P7p_wt_mod_2/2) / (trait_mean_data_Otipulae_Vg_P7p_wt_mod_2)^2
      
      mean(h2_data_Otipulae_Vg_P7p_wt_mod_2)
      mean(trait_mean_data_Otipulae_Vg_P7p_wt_mod_2)
      mean(va_data_Otipulae_Vg_P7p_wt_mod_2)
      mean(vp_data_Otipulae_Vg_P7p_wt_mod_2)
      mean(Evol_data_Otipulae_Vg_P7p_wt_mod_2)
      
    }
    
  }


#Oonirici----
{
  Vg_Oonirici_bi_CONTROL_woutCB <- subset(Vg_Oonirici_data_woutCB, Treatment =="CONTROL")
  Vg_Oonirici_bi_WILD_woutCB <- subset(Vg_Oonirici_data_woutCB, Treatment =="WILD")
  table(Vg_Oonirici_bi_CONTROL_woutCB$Observer)
  table(Vg_Oonirici_bi_WILD_woutCB$Observer)
  
  ## Oonirici P3p ----
  
  
  #Oonirici_CONTROL_P3p_SSSS_mod_2 
  {
    
    Oonirici_CONTROL_P3p_SSSS_mod_2 <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer + Ancestral -1,
                                             random      = ~ LineB + BlockRep,
                                             family      = "threshold",
                                             data        = Vg_Oonirici_bi_CONTROL_woutCB,
                                             prior       = prior_bi_block,
                                             nitt        = 1260000,       
                                             thin        = 500,           
                                             burnin      = 10000,
                                             trunc       = TRUE,
                                             pr          = TRUE,
                                             pl          = TRUE)            
    
    saveRDS(Oonirici_CONTROL_P3p_SSSS_mod_2, file = "Oonirici_CONTROL_P3p_SSSS_mod_2.rds")
    Oonirici_CONTROL_P3p_SSSS_mod_2 <- readRDS("Oonirici_CONTROL_P3p_SSSS_mod_2.rds")
    
    summary(Oonirici_CONTROL_P3p_SSSS_mod_2) 
    #plotTrace(Oonirici_CONTROL_P3p_SSSS_mod_2$Sol)
    
    # traces and posterior densities
    
    pdf("Oonirici_CONTROL_P3p_SSSS_mod_2.pdf")
    plot(Oonirici_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:4)]) 
    plot(Oonirici_CONTROL_P3p_SSSS_mod_2[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:4)]) #
    heidel.diag(Oonirici_CONTROL_P3p_SSSS_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:4)])
    #autocorr.plot(Oonirici_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:4)]) #
    autocorr.diag(Oonirici_CONTROL_P3p_SSSS_mod_2[["VCV"]])
    #autocorr.plot(Oonirici_CONTROL_P3p_SSSS_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:4)]) # 
    effectiveSize(Oonirici_CONTROL_P3p_SSSS_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_CONTROL_P3p_SSSS_mod_2 <- Oonirici_CONTROL_P3p_SSSS_mod_2[["VCV"]][ , "LineB"]
      vr_Oonirici_CONTROL_P3p_SSSS_mod_2 <- Oonirici_CONTROL_P3p_SSSS_mod_2[["VCV"]][ , "units"]
      vlat_Oonirici_CONTROL_P3p_SSSS_mod_2 <- rowSums(Oonirici_CONTROL_P3p_SSSS_mod_2[["VCV"]])
      
      mean(va_liab_Oonirici_CONTROL_P3p_SSSS_mod_2) 
      HPDinterval(va_liab_Oonirici_CONTROL_P3p_SSSS_mod_2) 
      
      mean(vlat_Oonirici_CONTROL_P3p_SSSS_mod_2) 
      
      #variance of fixed effects
      X_Oonirici_CONTROL_P3p_SSSS_mod_2 <- Oonirici_CONTROL_P3p_SSSS_mod_2[["X"]]
      beta_Oonirici_CONTROL_P3p_SSSS_mod_2 <- Oonirici_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:4)]
      vf_Oonirici_CONTROL_P3p_SSSS_mod_2   <- apply(beta_Oonirici_CONTROL_P3p_SSSS_mod_2, 1, function(b) {var(as.vector(X_Oonirici_CONTROL_P3p_SSSS_mod_2 %*% b))}) 
      mean(vf_Oonirici_CONTROL_P3p_SSSS_mod_2) 
      
      h2_liab_Oonirici_CONTROL_P3p_SSSS_mod_2 <- va_liab_Oonirici_CONTROL_P3p_SSSS_mod_2 / (vlat_Oonirici_CONTROL_P3p_SSSS_mod_2 + vf_Oonirici_CONTROL_P3p_SSSS_mod_2)
      mean(h2_liab_Oonirici_CONTROL_P3p_SSSS_mod_2) 
      posterior.mode(h2_liab_Oonirici_CONTROL_P3p_SSSS_mod_2)	
      median(h2_liab_Oonirici_CONTROL_P3p_SSSS_mod_2)		
      HPDinterval(h2_liab_Oonirici_CONTROL_P3p_SSSS_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_CONTROL_P3p_SSSS_mod_2 <- ((rowMeans(Oonirici_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Oonirici_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Oonirici_CONTROL_P3p_SSSS_mod_2[["Sol"]][,4]))/2)
    
    Evol_liab_Oonirici_CONTROL_P3p_SSSS_mod_2 <- (va_liab_Oonirici_CONTROL_P3p_SSSS_mod_2/2) / (trait_mean_liab_Oonirici_CONTROL_P3p_SSSS_mod_2)^2
    mean(Evol_liab_Oonirici_CONTROL_P3p_SSSS_mod_2)
    
    
    #Oonirici_CONTROL_P3p_SSSS_mod_2 data scale
    {
      
      predict_Oonirici_CONTROL_P3p_SSSS_mod_2 <- map(1:nrow(Oonirici_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:4)]), ~ as.vector(X_Oonirici_CONTROL_P3p_SSSS_mod_2 %*% Oonirici_CONTROL_P3p_SSSS_mod_2[["Sol"]][,c(1:4)][., ]))
      
      data_Oonirici_CONTROL_P3p_SSSS_mod_2 <-
        pmap_dfr(list(predict = predict_Oonirici_CONTROL_P3p_SSSS_mod_2,
                      var.a = Oonirici_CONTROL_P3p_SSSS_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_CONTROL_P3p_SSSS_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_CONTROL_P3p_SSSS_mod_2 <- data_Oonirici_CONTROL_P3p_SSSS_mod_2[["h2.obs"]]
      trait_mean_data_Oonirici_CONTROL_P3p_SSSS_mod_2 <- data_Oonirici_CONTROL_P3p_SSSS_mod_2[["mean.obs"]]
      va_data_Oonirici_CONTROL_P3p_SSSS_mod_2 <- data_Oonirici_CONTROL_P3p_SSSS_mod_2[["var.a.obs"]]
      vp_data_Oonirici_CONTROL_P3p_SSSS_mod_2 <- data_Oonirici_CONTROL_P3p_SSSS_mod_2[["var.obs"]]
      
      Evol_data_Oonirici_CONTROL_P3p_SSSS_mod_2 <- (va_data_Oonirici_CONTROL_P3p_SSSS_mod_2/2) / (trait_mean_data_Oonirici_CONTROL_P3p_SSSS_mod_2)^2
      
      mean(h2_data_Oonirici_CONTROL_P3p_SSSS_mod_2) 
      mean(trait_mean_data_Oonirici_CONTROL_P3p_SSSS_mod_2)
      mean(va_data_Oonirici_CONTROL_P3p_SSSS_mod_2) 
      mean(vp_data_Oonirici_CONTROL_P3p_SSSS_mod_2)
      mean(Evol_data_Oonirici_CONTROL_P3p_SSSS_mod_2)
      
    }
    
  }
  
  #Oonirici_WILD_P3p_SSSS_mod 
  {
    
    Oonirici_WILD_P3p_SSSS_mod <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer -1,
                                           random      = ~ LineB + BlockRep,
                                           family      = "threshold",
                                           data        = Vg_Oonirici_bi_WILD,
                                           prior       = prior_bi_block,
                                           nitt        = 1260000,       
                                           thin        = 500,           
                                           burnin      = 10000,
                                           trunc       = TRUE,
                                           pr          = TRUE,
                                           pl          = TRUE)        
    
    
    
    saveRDS(Oonirici_WILD_P3p_SSSS_mod, file = "Oonirici_WILD_P3p_SSSS_mod.rds")
    Oonirici_WILD_P3p_SSSS_mod <- readRDS("Oonirici_WILD_P3p_SSSS_mod.rds")
    
    summary(Oonirici_WILD_P3p_SSSS_mod) 
    #plot(Oonirici_WILD_P3p_SSSS_mod)
    
    # traces and posterior densities
    pdf("Oonirici_WILD_P3p_SSSS_mod.pdf")
    plot(Oonirici_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)]) 
    plot(Oonirici_WILD_P3p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)]) #
    heidel.diag(Oonirici_WILD_P3p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)])
    #autocorr.plot(Oonirici_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)]) #
    autocorr.diag(Oonirici_WILD_P3p_SSSS_mod[["VCV"]])
    #autocorr.plot(Oonirici_WILD_P3p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)]) # 
    effectiveSize(Oonirici_WILD_P3p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_WILD_P3p_SSSS_mod <- Oonirici_WILD_P3p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_WILD_P3p_SSSS_mod <- Oonirici_WILD_P3p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Oonirici_WILD_P3p_SSSS_mod <- rowSums(Oonirici_WILD_P3p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Oonirici_WILD_P3p_SSSS_mod) 
      HPDinterval(va_liab_Oonirici_WILD_P3p_SSSS_mod) 
      
      mean(vlat_Oonirici_WILD_P3p_SSSS_mod) 
      
      #variance of fixed effects
      X_Oonirici_WILD_P3p_SSSS_mod <- Oonirici_WILD_P3p_SSSS_mod[["X"]]
      beta_Oonirici_WILD_P3p_SSSS_mod <- Oonirici_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)]
      vf_Oonirici_WILD_P3p_SSSS_mod   <- apply(beta_Oonirici_WILD_P3p_SSSS_mod, 1, function(b) {var(as.vector(X_Oonirici_WILD_P3p_SSSS_mod %*% b))}) 
      mean(vf_Oonirici_WILD_P3p_SSSS_mod) 
      
      h2_liab_Oonirici_WILD_P3p_SSSS_mod <- va_liab_Oonirici_WILD_P3p_SSSS_mod / (vlat_Oonirici_WILD_P3p_SSSS_mod + vf_Oonirici_WILD_P3p_SSSS_mod)
      mean(h2_liab_Oonirici_WILD_P3p_SSSS_mod) 
      posterior.mode(h2_liab_Oonirici_WILD_P3p_SSSS_mod)	
      median(h2_liab_Oonirici_WILD_P3p_SSSS_mod)		
      HPDinterval(h2_liab_Oonirici_WILD_P3p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Oonirici_WILD_P3p_SSSS_mod <- rowMeans(Oonirici_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)])
    Evol_liab_Oonirici_WILD_P3p_SSSS_mod <- va_liab_Oonirici_WILD_P3p_SSSS_mod / mean(trait_mean_liab_Oonirici_WILD_P3p_SSSS_mod)^2
    mean(Evol_liab_Oonirici_WILD_P3p_SSSS_mod)
    
    #Oonirici_WILD_P3p_SSSS_mod data scale
    {
      
      predict_Oonirici_WILD_P3p_SSSS_mod <- map(1:nrow(Oonirici_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)]), ~ as.vector(X_Oonirici_WILD_P3p_SSSS_mod %*% Oonirici_WILD_P3p_SSSS_mod[["Sol"]][,c(1:3)][., ]))
      
      data_Oonirici_WILD_P3p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Oonirici_WILD_P3p_SSSS_mod,
                      var.a = Oonirici_WILD_P3p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_WILD_P3p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_WILD_P3p_SSSS_mod <- data_Oonirici_WILD_P3p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Oonirici_WILD_P3p_SSSS_mod <- data_Oonirici_WILD_P3p_SSSS_mod[["mean.obs"]]
      va_data_Oonirici_WILD_P3p_SSSS_mod <- data_Oonirici_WILD_P3p_SSSS_mod[["var.a.obs"]]
      vp_data_Oonirici_WILD_P3p_SSSS_mod <- data_Oonirici_WILD_P3p_SSSS_mod[["var.obs"]]
      
      Evol_data_Oonirici_WILD_P3p_SSSS_mod <- (va_data_Oonirici_WILD_P3p_SSSS_mod/2) / (trait_mean_data_Oonirici_WILD_P3p_SSSS_mod)^2
      
      mean(h2_data_Oonirici_WILD_P3p_SSSS_mod)
      mean(trait_mean_data_Oonirici_WILD_P3p_SSSS_mod)
      mean(va_data_Oonirici_WILD_P3p_SSSS_mod)
      mean(vp_data_Oonirici_WILD_P3p_SSSS_mod)
      mean(Evol_data_Oonirici_WILD_P3p_SSSS_mod)
      
    }
    
  }
  
  #Oonirici_Vg_P3p_SSSS_mod_2 
  {
    
    Oonirici_Vg_P3p_SSSS_mod_2 <- MCMCglmm(fixed       = P3.p_SSSS ~ Observer + at.level(Treatment,1):Ancestral + at.level(Treatment,2):Ancestral -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vg_Oonirici_data_woutCB,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)        
    
    
    saveRDS(Oonirici_Vg_P3p_SSSS_mod_2, file = "Oonirici_Vg_P3p_SSSS_mod_2.rds")
    Oonirici_Vg_P3p_SSSS_mod_2 <- readRDS("Oonirici_Vg_P3p_SSSS_mod_2.rds")
    
    summary(Oonirici_Vg_P3p_SSSS_mod_2) 
    #plotTrace(Oonirici_Vg_P3p_SSSS_mod_2$Sol)
    #View(Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]])
    # traces and posterior densities
    pdf("Oonirici_Vg_P3p_SSSS_mod_2.pdf")
    plot(Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:5)]) 
    plot(Oonirici_Vg_P3p_SSSS_mod_2[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_Vg_P3p_SSSS_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_Vg_P3p_SSSS_mod_2[["VCV"]])
    #autocorr.plot(Oonirici_Vg_P3p_SSSS_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_Vg_P3p_SSSS_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_Vg_P3p_SSSS_mod_2 <- Oonirici_Vg_P3p_SSSS_mod_2[["VCV"]][ , "LineB"]
      vr_Oonirici_Vg_P3p_SSSS_mod_2 <- Oonirici_Vg_P3p_SSSS_mod_2[["VCV"]][ , "units"]
      vlat_Oonirici_Vg_P3p_SSSS_mod_2 <- rowSums(Oonirici_Vg_P3p_SSSS_mod_2[["VCV"]])
      
      mean(va_liab_Oonirici_Vg_P3p_SSSS_mod_2) 
      HPDinterval(va_liab_Oonirici_Vg_P3p_SSSS_mod_2) 
      
      mean(vlat_Oonirici_Vg_P3p_SSSS_mod_2) 
      
      #variance of fixed effects
      X_Oonirici_Vg_P3p_SSSS_mod_2 <- Oonirici_Vg_P3p_SSSS_mod_2[["X"]]
      beta_Oonirici_Vg_P3p_SSSS_mod_2 <- Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:5)]
      vf_Oonirici_Vg_P3p_SSSS_mod_2   <- apply(beta_Oonirici_Vg_P3p_SSSS_mod_2, 1, function(b) {var(as.vector(X_Oonirici_Vg_P3p_SSSS_mod_2 %*% b))}) 
      mean(vf_Oonirici_Vg_P3p_SSSS_mod_2) 
      
      h2_liab_Oonirici_Vg_P3p_SSSS_mod_2 <- va_liab_Oonirici_Vg_P3p_SSSS_mod_2 / (vlat_Oonirici_Vg_P3p_SSSS_mod_2 + vf_Oonirici_Vg_P3p_SSSS_mod_2)
      mean(h2_liab_Oonirici_Vg_P3p_SSSS_mod_2) 
      posterior.mode(h2_liab_Oonirici_Vg_P3p_SSSS_mod_2)	
      median(h2_liab_Oonirici_Vg_P3p_SSSS_mod_2)		
      HPDinterval(h2_liab_Oonirici_Vg_P3p_SSSS_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:3)])) #Oni WILD P3p_SSSS
    mean(rowMeans(Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,4]) #JU77 CONTROL P3p_SSSS
    mean(rowMeans(Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,5]) #PS2068 Control P3p_SSSS
    
    trait_mean_liab_Oonirici_Vg_P3p_SSSS_mod_2 <- ((rowMeans(Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,4]) + (rowMeans(Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,5]) )/3)
    
    Evol_liab_Oonirici_Vg_P3p_SSSS_mod_2 <- (va_liab_Oonirici_Vg_P3p_SSSS_mod_2/2) / (trait_mean_liab_Oonirici_Vg_P3p_SSSS_mod_2)^2
    mean(Evol_liab_Oonirici_Vg_P3p_SSSS_mod_2)
    
    
    #Oonirici_Vg_P3p_SSSS_mod_2 data scale
    {
      
      predict_Oonirici_Vg_P3p_SSSS_mod_2 <- map(1:nrow(Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_Vg_P3p_SSSS_mod_2 %*% Oonirici_Vg_P3p_SSSS_mod_2[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_Vg_P3p_SSSS_mod_2 <-
        pmap_dfr(list(predict = predict_Oonirici_Vg_P3p_SSSS_mod_2,
                      var.a = Oonirici_Vg_P3p_SSSS_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_Vg_P3p_SSSS_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_Vg_P3p_SSSS_mod_2 <- data_Oonirici_Vg_P3p_SSSS_mod_2[["h2.obs"]]
      trait_mean_data_Oonirici_Vg_P3p_SSSS_mod_2 <- data_Oonirici_Vg_P3p_SSSS_mod_2[["mean.obs"]]
      va_data_Oonirici_Vg_P3p_SSSS_mod_2 <- data_Oonirici_Vg_P3p_SSSS_mod_2[["var.a.obs"]]
      vp_data_Oonirici_Vg_P3p_SSSS_mod_2 <- data_Oonirici_Vg_P3p_SSSS_mod_2[["var.obs"]]
      
      Evol_data_Oonirici_Vg_P3p_SSSS_mod_2 <- (va_data_Oonirici_Vg_P3p_SSSS_mod_2/2) / (trait_mean_data_Oonirici_Vg_P3p_SSSS_mod_2)^2
      
      mean(h2_data_Oonirici_Vg_P3p_SSSS_mod_2)
      mean(trait_mean_data_Oonirici_Vg_P3p_SSSS_mod_2)
      mean(va_data_Oonirici_Vg_P3p_SSSS_mod_2)
      mean(vp_data_Oonirici_Vg_P3p_SSSS_mod_2)
      mean(Evol_data_Oonirici_Vg_P3p_SSSS_mod_2)
      
    }
    
  }
  
  
  
  ## Oonirici P4p ----
  
  
  #Oonirici_CONTROL_P4p_SSSS_mod_2 
  {
    
    Oonirici_CONTROL_P4p_SSSS_mod_2 <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer + Ancestral -1,
                                             random      = ~ LineB + BlockRep,
                                             family      = "threshold",
                                             data        = Vg_Oonirici_bi_CONTROL_woutCB,
                                             prior       = prior_bi_block,
                                             nitt        = 1260000,       
                                             thin        = 500,           
                                             burnin      = 10000,
                                             trunc       = TRUE,
                                             pr          = TRUE,
                                             pl          = TRUE)            
    
    saveRDS(Oonirici_CONTROL_P4p_SSSS_mod_2, file = "Oonirici_CONTROL_P4p_SSSS_mod_2.rds")
    Oonirici_CONTROL_P4p_SSSS_mod_2 <- readRDS("Oonirici_CONTROL_P4p_SSSS_mod_2.rds")
    
    summary(Oonirici_CONTROL_P4p_SSSS_mod_2) 
    #plotTrace(Oonirici_CONTROL_P4p_SSSS_mod_2$Sol)
    
    # traces and posterior densities
    
    pdf("Oonirici_CONTROL_P4p_SSSS_mod_2.pdf")
    plot(Oonirici_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:4)]) 
    plot(Oonirici_CONTROL_P4p_SSSS_mod_2[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:4)]) #
    heidel.diag(Oonirici_CONTROL_P4p_SSSS_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:4)])
    #autocorr.plot(Oonirici_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:4)]) #
    autocorr.diag(Oonirici_CONTROL_P4p_SSSS_mod_2[["VCV"]])
    #autocorr.plot(Oonirici_CONTROL_P4p_SSSS_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:4)]) # 
    effectiveSize(Oonirici_CONTROL_P4p_SSSS_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_CONTROL_P4p_SSSS_mod_2 <- Oonirici_CONTROL_P4p_SSSS_mod_2[["VCV"]][ , "LineB"]
      vr_Oonirici_CONTROL_P4p_SSSS_mod_2 <- Oonirici_CONTROL_P4p_SSSS_mod_2[["VCV"]][ , "units"]
      vlat_Oonirici_CONTROL_P4p_SSSS_mod_2 <- rowSums(Oonirici_CONTROL_P4p_SSSS_mod_2[["VCV"]])
      
      mean(va_liab_Oonirici_CONTROL_P4p_SSSS_mod_2) 
      HPDinterval(va_liab_Oonirici_CONTROL_P4p_SSSS_mod_2) 
      
      mean(vlat_Oonirici_CONTROL_P4p_SSSS_mod_2) 
      
      #variance of fixed effects
      X_Oonirici_CONTROL_P4p_SSSS_mod_2 <- Oonirici_CONTROL_P4p_SSSS_mod_2[["X"]]
      beta_Oonirici_CONTROL_P4p_SSSS_mod_2 <- Oonirici_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:4)]
      vf_Oonirici_CONTROL_P4p_SSSS_mod_2   <- apply(beta_Oonirici_CONTROL_P4p_SSSS_mod_2, 1, function(b) {var(as.vector(X_Oonirici_CONTROL_P4p_SSSS_mod_2 %*% b))}) 
      mean(vf_Oonirici_CONTROL_P4p_SSSS_mod_2) 
      
      h2_liab_Oonirici_CONTROL_P4p_SSSS_mod_2 <- va_liab_Oonirici_CONTROL_P4p_SSSS_mod_2 / (vlat_Oonirici_CONTROL_P4p_SSSS_mod_2 + vf_Oonirici_CONTROL_P4p_SSSS_mod_2)
      mean(h2_liab_Oonirici_CONTROL_P4p_SSSS_mod_2) 
      posterior.mode(h2_liab_Oonirici_CONTROL_P4p_SSSS_mod_2)	
      median(h2_liab_Oonirici_CONTROL_P4p_SSSS_mod_2)		
      HPDinterval(h2_liab_Oonirici_CONTROL_P4p_SSSS_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_CONTROL_P4p_SSSS_mod_2 <- ((rowMeans(Oonirici_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Oonirici_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Oonirici_CONTROL_P4p_SSSS_mod_2[["Sol"]][,4]))/2)
    
    Evol_liab_Oonirici_CONTROL_P4p_SSSS_mod_2 <- (va_liab_Oonirici_CONTROL_P4p_SSSS_mod_2/2) / (trait_mean_liab_Oonirici_CONTROL_P4p_SSSS_mod_2)^2
    mean(Evol_liab_Oonirici_CONTROL_P4p_SSSS_mod_2)
    
    
    #Oonirici_CONTROL_P4p_SSSS_mod_2 data scale
    {
      
      predict_Oonirici_CONTROL_P4p_SSSS_mod_2 <- map(1:nrow(Oonirici_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:4)]), ~ as.vector(X_Oonirici_CONTROL_P4p_SSSS_mod_2 %*% Oonirici_CONTROL_P4p_SSSS_mod_2[["Sol"]][,c(1:4)][., ]))
      
      data_Oonirici_CONTROL_P4p_SSSS_mod_2 <-
        pmap_dfr(list(predict = predict_Oonirici_CONTROL_P4p_SSSS_mod_2,
                      var.a = Oonirici_CONTROL_P4p_SSSS_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_CONTROL_P4p_SSSS_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_CONTROL_P4p_SSSS_mod_2 <- data_Oonirici_CONTROL_P4p_SSSS_mod_2[["h2.obs"]]
      trait_mean_data_Oonirici_CONTROL_P4p_SSSS_mod_2 <- data_Oonirici_CONTROL_P4p_SSSS_mod_2[["mean.obs"]]
      va_data_Oonirici_CONTROL_P4p_SSSS_mod_2 <- data_Oonirici_CONTROL_P4p_SSSS_mod_2[["var.a.obs"]]
      vp_data_Oonirici_CONTROL_P4p_SSSS_mod_2 <- data_Oonirici_CONTROL_P4p_SSSS_mod_2[["var.obs"]]
      
      Evol_data_Oonirici_CONTROL_P4p_SSSS_mod_2 <- (va_data_Oonirici_CONTROL_P4p_SSSS_mod_2/2) / (trait_mean_data_Oonirici_CONTROL_P4p_SSSS_mod_2)^2
      
      mean(h2_data_Oonirici_CONTROL_P4p_SSSS_mod_2) 
      mean(trait_mean_data_Oonirici_CONTROL_P4p_SSSS_mod_2)
      mean(va_data_Oonirici_CONTROL_P4p_SSSS_mod_2) 
      mean(vp_data_Oonirici_CONTROL_P4p_SSSS_mod_2)
      mean(Evol_data_Oonirici_CONTROL_P4p_SSSS_mod_2)
      
    }
    
  }
  
  #Oonirici_WILD_P4p_SSSS_mod 
  {
    
    Oonirici_WILD_P4p_SSSS_mod <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer -1,
                                           random      = ~ LineB + BlockRep,
                                           family      = "threshold",
                                           data        = Vg_Oonirici_bi_WILD,
                                           prior       = prior_bi_block,
                                           nitt        = 1260000,       
                                           thin        = 500,           
                                           burnin      = 10000,
                                           trunc       = TRUE,
                                           pr          = TRUE,
                                           pl          = TRUE)             
    
    saveRDS(Oonirici_WILD_P4p_SSSS_mod, file = "Oonirici_WILD_P4p_SSSS_mod.rds")
    Oonirici_WILD_P4p_SSSS_mod <- readRDS("Oonirici_WILD_P4p_SSSS_mod.rds")
    
    summary(Oonirici_WILD_P4p_SSSS_mod) 
    #plot(Oonirici_WILD_P4p_SSSS_mod)
    
    # traces and posterior densities
    pdf("Oonirici_WILD_P4p_SSSS_mod.pdf")
    plot(Oonirici_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)]) 
    plot(Oonirici_WILD_P4p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)]) #
    heidel.diag(Oonirici_WILD_P4p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)])
    #autocorr.plot(Oonirici_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)]) #
    autocorr.diag(Oonirici_WILD_P4p_SSSS_mod[["VCV"]])
    #autocorr.plot(Oonirici_WILD_P4p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)]) # 
    effectiveSize(Oonirici_WILD_P4p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_WILD_P4p_SSSS_mod <- Oonirici_WILD_P4p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_WILD_P4p_SSSS_mod <- Oonirici_WILD_P4p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Oonirici_WILD_P4p_SSSS_mod <- rowSums(Oonirici_WILD_P4p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Oonirici_WILD_P4p_SSSS_mod) 
      HPDinterval(va_liab_Oonirici_WILD_P4p_SSSS_mod) 
      
      mean(vlat_Oonirici_WILD_P4p_SSSS_mod) 
      
      #variance of fixed effects
      X_Oonirici_WILD_P4p_SSSS_mod <- Oonirici_WILD_P4p_SSSS_mod[["X"]]
      beta_Oonirici_WILD_P4p_SSSS_mod <- Oonirici_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)]
      vf_Oonirici_WILD_P4p_SSSS_mod   <- apply(beta_Oonirici_WILD_P4p_SSSS_mod, 1, function(b) {var(as.vector(X_Oonirici_WILD_P4p_SSSS_mod %*% b))}) 
      mean(vf_Oonirici_WILD_P4p_SSSS_mod) 
      
      h2_liab_Oonirici_WILD_P4p_SSSS_mod <- va_liab_Oonirici_WILD_P4p_SSSS_mod / (vlat_Oonirici_WILD_P4p_SSSS_mod + vf_Oonirici_WILD_P4p_SSSS_mod)
      mean(h2_liab_Oonirici_WILD_P4p_SSSS_mod) 
      posterior.mode(h2_liab_Oonirici_WILD_P4p_SSSS_mod)	
      median(h2_liab_Oonirici_WILD_P4p_SSSS_mod)		
      HPDinterval(h2_liab_Oonirici_WILD_P4p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_WILD_P4p_SSSS_mod <- rowMeans(Oonirici_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)])
    Evol_liab_Oonirici_WILD_P4p_SSSS_mod <- va_liab_Oonirici_WILD_P4p_SSSS_mod / mean(trait_mean_liab_Oonirici_WILD_P4p_SSSS_mod)^2
    mean(Evol_liab_Oonirici_WILD_P4p_SSSS_mod)
    
    #Oonirici_WILD_P4p_SSSS_mod data scale
    {
      
      predict_Oonirici_WILD_P4p_SSSS_mod <- map(1:nrow(Oonirici_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)]), ~ as.vector(X_Oonirici_WILD_P4p_SSSS_mod %*% Oonirici_WILD_P4p_SSSS_mod[["Sol"]][,c(1:3)][., ]))
      
      data_Oonirici_WILD_P4p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Oonirici_WILD_P4p_SSSS_mod,
                      var.a = Oonirici_WILD_P4p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_WILD_P4p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_WILD_P4p_SSSS_mod <- data_Oonirici_WILD_P4p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Oonirici_WILD_P4p_SSSS_mod <- data_Oonirici_WILD_P4p_SSSS_mod[["mean.obs"]]
      va_data_Oonirici_WILD_P4p_SSSS_mod <- data_Oonirici_WILD_P4p_SSSS_mod[["var.a.obs"]]
      vp_data_Oonirici_WILD_P4p_SSSS_mod <- data_Oonirici_WILD_P4p_SSSS_mod[["var.obs"]]
      
      Evol_data_Oonirici_WILD_P4p_SSSS_mod <- (va_data_Oonirici_WILD_P4p_SSSS_mod/2) / (trait_mean_data_Oonirici_WILD_P4p_SSSS_mod)^2
      
      mean(h2_data_Oonirici_WILD_P4p_SSSS_mod)
      mean(trait_mean_data_Oonirici_WILD_P4p_SSSS_mod)
      mean(va_data_Oonirici_WILD_P4p_SSSS_mod)
      mean(vp_data_Oonirici_WILD_P4p_SSSS_mod)
      mean(Evol_data_Oonirici_WILD_P4p_SSSS_mod)
      
    }
    
  }
  
  #Oonirici_Vg_P4p_SSSS_mod_2 
  {
    
    Oonirici_Vg_P4p_SSSS_mod_2 <- MCMCglmm(fixed       = P4.p_SSSS ~ Observer + at.level(Treatment,1):Ancestral + at.level(Treatment,2):Ancestral -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vg_Oonirici_data_woutCB,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)        
    
    
    saveRDS(Oonirici_Vg_P4p_SSSS_mod_2, file = "Oonirici_Vg_P4p_SSSS_mod_2.rds")
    Oonirici_Vg_P4p_SSSS_mod_2 <- readRDS("Oonirici_Vg_P4p_SSSS_mod_2.rds")
    
    summary(Oonirici_Vg_P4p_SSSS_mod_2) 
    #plotTrace(Oonirici_Vg_P4p_SSSS_mod_2$Sol)
    #View(Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]])
    
    # traces and posterior densities
    pdf("Oonirici_Vg_P4p_SSSS_mod_2.pdf")
    plot(Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:5)]) 
    plot(Oonirici_Vg_P4p_SSSS_mod_2[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_Vg_P4p_SSSS_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_Vg_P4p_SSSS_mod_2[["VCV"]])
    #autocorr.plot(Oonirici_Vg_P4p_SSSS_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_Vg_P4p_SSSS_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_Vg_P4p_SSSS_mod_2 <- Oonirici_Vg_P4p_SSSS_mod_2[["VCV"]][ , "LineB"]
      vr_Oonirici_Vg_P4p_SSSS_mod_2 <- Oonirici_Vg_P4p_SSSS_mod_2[["VCV"]][ , "units"]
      vlat_Oonirici_Vg_P4p_SSSS_mod_2 <- rowSums(Oonirici_Vg_P4p_SSSS_mod_2[["VCV"]])
      
      mean(va_liab_Oonirici_Vg_P4p_SSSS_mod_2) 
      HPDinterval(va_liab_Oonirici_Vg_P4p_SSSS_mod_2) 
      
      mean(vlat_Oonirici_Vg_P4p_SSSS_mod_2) 
      
      #variance of fixed effects
      X_Oonirici_Vg_P4p_SSSS_mod_2 <- Oonirici_Vg_P4p_SSSS_mod_2[["X"]]
      beta_Oonirici_Vg_P4p_SSSS_mod_2 <- Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:5)]
      vf_Oonirici_Vg_P4p_SSSS_mod_2   <- apply(beta_Oonirici_Vg_P4p_SSSS_mod_2, 1, function(b) {var(as.vector(X_Oonirici_Vg_P4p_SSSS_mod_2 %*% b))}) 
      mean(vf_Oonirici_Vg_P4p_SSSS_mod_2) 
      
      h2_liab_Oonirici_Vg_P4p_SSSS_mod_2 <- va_liab_Oonirici_Vg_P4p_SSSS_mod_2 / (vlat_Oonirici_Vg_P4p_SSSS_mod_2 + vf_Oonirici_Vg_P4p_SSSS_mod_2)
      mean(h2_liab_Oonirici_Vg_P4p_SSSS_mod_2) 
      posterior.mode(h2_liab_Oonirici_Vg_P4p_SSSS_mod_2)	
      median(h2_liab_Oonirici_Vg_P4p_SSSS_mod_2)		
      HPDinterval(h2_liab_Oonirici_Vg_P4p_SSSS_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:3)])) #Oni WILD P4p_SSSS
    mean(rowMeans(Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,4]) #JU77 CONTROL P4p_SSSS
    mean(rowMeans(Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,5]) #PS2068 Control P4p_SSSS
    
    trait_mean_liab_Oonirici_Vg_P4p_SSSS_mod_2 <- ((rowMeans(Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,4]) + (rowMeans(Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,5]) )/3)
    
    Evol_liab_Oonirici_Vg_P4p_SSSS_mod_2 <- (va_liab_Oonirici_Vg_P4p_SSSS_mod_2/2) / (trait_mean_liab_Oonirici_Vg_P4p_SSSS_mod_2)^2
    mean(Evol_liab_Oonirici_Vg_P4p_SSSS_mod_2)
    
    
    #Oonirici_Vg_P4p_SSSS_mod_2 data scale
    {
      
      predict_Oonirici_Vg_P4p_SSSS_mod_2 <- map(1:nrow(Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_Vg_P4p_SSSS_mod_2 %*% Oonirici_Vg_P4p_SSSS_mod_2[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_Vg_P4p_SSSS_mod_2 <-
        pmap_dfr(list(predict = predict_Oonirici_Vg_P4p_SSSS_mod_2,
                      var.a = Oonirici_Vg_P4p_SSSS_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_Vg_P4p_SSSS_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_Vg_P4p_SSSS_mod_2 <- data_Oonirici_Vg_P4p_SSSS_mod_2[["h2.obs"]]
      trait_mean_data_Oonirici_Vg_P4p_SSSS_mod_2 <- data_Oonirici_Vg_P4p_SSSS_mod_2[["mean.obs"]]
      va_data_Oonirici_Vg_P4p_SSSS_mod_2 <- data_Oonirici_Vg_P4p_SSSS_mod_2[["var.a.obs"]]
      vp_data_Oonirici_Vg_P4p_SSSS_mod_2 <- data_Oonirici_Vg_P4p_SSSS_mod_2[["var.obs"]]
      
      Evol_data_Oonirici_Vg_P4p_SSSS_mod_2 <- (va_data_Oonirici_Vg_P4p_SSSS_mod_2/2) / (trait_mean_data_Oonirici_Vg_P4p_SSSS_mod_2)^2
      
      mean(h2_data_Oonirici_Vg_P4p_SSSS_mod_2)
      mean(trait_mean_data_Oonirici_Vg_P4p_SSSS_mod_2)
      mean(va_data_Oonirici_Vg_P4p_SSSS_mod_2)
      mean(vp_data_Oonirici_Vg_P4p_SSSS_mod_2)
      mean(Evol_data_Oonirici_Vg_P4p_SSSS_mod_2)
      
    }
    
  }
  
  
  ## Oonirici P8p ----
  
  
  #Oonirici_CONTROL_P8p_SSSS_mod_2 
  {
    
    Oonirici_CONTROL_P8p_SSSS_mod_2 <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer + Ancestral -1,
                                             random      = ~ LineB + BlockRep,
                                             family      = "threshold",
                                             data        = Vg_Oonirici_bi_CONTROL_woutCB,
                                             prior       = prior_bi_block,
                                             nitt        = 1260000,       
                                             thin        = 500,           
                                             burnin      = 10000,
                                             trunc       = TRUE,
                                             pr          = TRUE,
                                             pl          = TRUE)            
    
    saveRDS(Oonirici_CONTROL_P8p_SSSS_mod_2, file = "Oonirici_CONTROL_P8p_SSSS_mod_2.rds")
    Oonirici_CONTROL_P8p_SSSS_mod_2 <- readRDS("Oonirici_CONTROL_P8p_SSSS_mod_2.rds")
    
    summary(Oonirici_CONTROL_P8p_SSSS_mod_2) 
    #plotTrace(Oonirici_CONTROL_P8p_SSSS_mod_2$Sol)
    
    # traces and posterior densities
    
    pdf("Oonirici_CONTROL_P8p_SSSS_mod_2.pdf")
    plot(Oonirici_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:4)]) 
    plot(Oonirici_CONTROL_P8p_SSSS_mod_2[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:4)]) #
    heidel.diag(Oonirici_CONTROL_P8p_SSSS_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:4)])
    #autocorr.plot(Oonirici_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:4)]) #
    autocorr.diag(Oonirici_CONTROL_P8p_SSSS_mod_2[["VCV"]])
    #autocorr.plot(Oonirici_CONTROL_P8p_SSSS_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:4)]) # 
    effectiveSize(Oonirici_CONTROL_P8p_SSSS_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_CONTROL_P8p_SSSS_mod_2 <- Oonirici_CONTROL_P8p_SSSS_mod_2[["VCV"]][ , "LineB"]
      vr_Oonirici_CONTROL_P8p_SSSS_mod_2 <- Oonirici_CONTROL_P8p_SSSS_mod_2[["VCV"]][ , "units"]
      vlat_Oonirici_CONTROL_P8p_SSSS_mod_2 <- rowSums(Oonirici_CONTROL_P8p_SSSS_mod_2[["VCV"]])
      
      mean(va_liab_Oonirici_CONTROL_P8p_SSSS_mod_2) 
      HPDinterval(va_liab_Oonirici_CONTROL_P8p_SSSS_mod_2) 
      
      mean(vlat_Oonirici_CONTROL_P8p_SSSS_mod_2) 
      
      #variance of fixed effects
      X_Oonirici_CONTROL_P8p_SSSS_mod_2 <- Oonirici_CONTROL_P8p_SSSS_mod_2[["X"]]
      beta_Oonirici_CONTROL_P8p_SSSS_mod_2 <- Oonirici_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:4)]
      vf_Oonirici_CONTROL_P8p_SSSS_mod_2   <- apply(beta_Oonirici_CONTROL_P8p_SSSS_mod_2, 1, function(b) {var(as.vector(X_Oonirici_CONTROL_P8p_SSSS_mod_2 %*% b))}) 
      mean(vf_Oonirici_CONTROL_P8p_SSSS_mod_2) 
      
      h2_liab_Oonirici_CONTROL_P8p_SSSS_mod_2 <- va_liab_Oonirici_CONTROL_P8p_SSSS_mod_2 / (vlat_Oonirici_CONTROL_P8p_SSSS_mod_2 + vf_Oonirici_CONTROL_P8p_SSSS_mod_2)
      mean(h2_liab_Oonirici_CONTROL_P8p_SSSS_mod_2) 
      posterior.mode(h2_liab_Oonirici_CONTROL_P8p_SSSS_mod_2)	
      median(h2_liab_Oonirici_CONTROL_P8p_SSSS_mod_2)		
      HPDinterval(h2_liab_Oonirici_CONTROL_P8p_SSSS_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_CONTROL_P8p_SSSS_mod_2 <- ((rowMeans(Oonirici_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Oonirici_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Oonirici_CONTROL_P8p_SSSS_mod_2[["Sol"]][,4]))/2)
    
    Evol_liab_Oonirici_CONTROL_P8p_SSSS_mod_2 <- (va_liab_Oonirici_CONTROL_P8p_SSSS_mod_2/2) / (trait_mean_liab_Oonirici_CONTROL_P8p_SSSS_mod_2)^2
    mean(Evol_liab_Oonirici_CONTROL_P8p_SSSS_mod_2)
    
   
    
    #Oonirici_CONTROL_P8p_SSSS_mod_2 data scale
    {
      
      predict_Oonirici_CONTROL_P8p_SSSS_mod_2 <- map(1:nrow(Oonirici_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:4)]), ~ as.vector(X_Oonirici_CONTROL_P8p_SSSS_mod_2 %*% Oonirici_CONTROL_P8p_SSSS_mod_2[["Sol"]][,c(1:4)][., ]))
      
      data_Oonirici_CONTROL_P8p_SSSS_mod_2 <-
        pmap_dfr(list(predict = predict_Oonirici_CONTROL_P8p_SSSS_mod_2,
                      var.a = Oonirici_CONTROL_P8p_SSSS_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_CONTROL_P8p_SSSS_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_CONTROL_P8p_SSSS_mod_2 <- data_Oonirici_CONTROL_P8p_SSSS_mod_2[["h2.obs"]]
      trait_mean_data_Oonirici_CONTROL_P8p_SSSS_mod_2 <- data_Oonirici_CONTROL_P8p_SSSS_mod_2[["mean.obs"]]
      va_data_Oonirici_CONTROL_P8p_SSSS_mod_2 <- data_Oonirici_CONTROL_P8p_SSSS_mod_2[["var.a.obs"]]
      vp_data_Oonirici_CONTROL_P8p_SSSS_mod_2 <- data_Oonirici_CONTROL_P8p_SSSS_mod_2[["var.obs"]]
      
      Evol_data_Oonirici_CONTROL_P8p_SSSS_mod_2 <- (va_data_Oonirici_CONTROL_P8p_SSSS_mod_2/2) / (trait_mean_data_Oonirici_CONTROL_P8p_SSSS_mod_2)^2
      
      mean(h2_data_Oonirici_CONTROL_P8p_SSSS_mod_2) 
      mean(trait_mean_data_Oonirici_CONTROL_P8p_SSSS_mod_2)
      mean(va_data_Oonirici_CONTROL_P8p_SSSS_mod_2) 
      mean(vp_data_Oonirici_CONTROL_P8p_SSSS_mod_2)
      mean(Evol_data_Oonirici_CONTROL_P8p_SSSS_mod_2)
      
    }
    
  }
  
  #Oonirici_WILD_P8p_SSSS_mod 
  {
    
    Oonirici_WILD_P8p_SSSS_mod <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer -1,
                                           random      = ~ LineB + BlockRep,
                                           family      = "threshold",
                                           data        = Vg_Oonirici_bi_WILD,
                                           prior       = prior_bi_block,
                                           nitt        = 1260000,       
                                           thin        = 500,           
                                           burnin      = 10000,
                                           trunc       = TRUE,
                                           pr          = TRUE,
                                           pl          = TRUE)             
    
    saveRDS(Oonirici_WILD_P8p_SSSS_mod, file = "Oonirici_WILD_P8p_SSSS_mod.rds")
    Oonirici_WILD_P8p_SSSS_mod <- readRDS("Oonirici_WILD_P8p_SSSS_mod.rds")
    
    summary(Oonirici_WILD_P8p_SSSS_mod) 
    #plot(Oonirici_WILD_P8p_SSSS_mod)
    
    # traces and posterior densities
    pdf("Oonirici_WILD_P8p_SSSS_mod.pdf")
    plot(Oonirici_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)]) 
    plot(Oonirici_WILD_P8p_SSSS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)]) #
    heidel.diag(Oonirici_WILD_P8p_SSSS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)])
    #autocorr.plot(Oonirici_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)]) #
    autocorr.diag(Oonirici_WILD_P8p_SSSS_mod[["VCV"]])
    #autocorr.plot(Oonirici_WILD_P8p_SSSS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)]) # 
    effectiveSize(Oonirici_WILD_P8p_SSSS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_WILD_P8p_SSSS_mod <- Oonirici_WILD_P8p_SSSS_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_WILD_P8p_SSSS_mod <- Oonirici_WILD_P8p_SSSS_mod[["VCV"]][ , "units"]
      vlat_Oonirici_WILD_P8p_SSSS_mod <- rowSums(Oonirici_WILD_P8p_SSSS_mod[["VCV"]])
      
      mean(va_liab_Oonirici_WILD_P8p_SSSS_mod) 
      HPDinterval(va_liab_Oonirici_WILD_P8p_SSSS_mod) 
      
      mean(vlat_Oonirici_WILD_P8p_SSSS_mod) 
      
      #variance of fixed effects
      X_Oonirici_WILD_P8p_SSSS_mod <- Oonirici_WILD_P8p_SSSS_mod[["X"]]
      beta_Oonirici_WILD_P8p_SSSS_mod <- Oonirici_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)]
      vf_Oonirici_WILD_P8p_SSSS_mod   <- apply(beta_Oonirici_WILD_P8p_SSSS_mod, 1, function(b) {var(as.vector(X_Oonirici_WILD_P8p_SSSS_mod %*% b))}) 
      mean(vf_Oonirici_WILD_P8p_SSSS_mod) 
      
      h2_liab_Oonirici_WILD_P8p_SSSS_mod <- va_liab_Oonirici_WILD_P8p_SSSS_mod / (vlat_Oonirici_WILD_P8p_SSSS_mod + vf_Oonirici_WILD_P8p_SSSS_mod)
      mean(h2_liab_Oonirici_WILD_P8p_SSSS_mod) 
      posterior.mode(h2_liab_Oonirici_WILD_P8p_SSSS_mod)	
      median(h2_liab_Oonirici_WILD_P8p_SSSS_mod)		
      HPDinterval(h2_liab_Oonirici_WILD_P8p_SSSS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
 
    trait_mean_liab_Oonirici_WILD_P8p_SSSS_mod <- rowMeans(Oonirici_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)])
    Evol_liab_Oonirici_WILD_P8p_SSSS_mod <- va_liab_Oonirici_WILD_P8p_SSSS_mod / mean(trait_mean_liab_Oonirici_WILD_P8p_SSSS_mod)^2
    mean(Evol_liab_Oonirici_WILD_P8p_SSSS_mod)
    
    
    
    #Oonirici_WILD_P8p_SSSS_mod data scale
    {
      
      predict_Oonirici_WILD_P8p_SSSS_mod <- map(1:nrow(Oonirici_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)]), ~ as.vector(X_Oonirici_WILD_P8p_SSSS_mod %*% Oonirici_WILD_P8p_SSSS_mod[["Sol"]][,c(1:3)][., ]))
      
      data_Oonirici_WILD_P8p_SSSS_mod <-
        pmap_dfr(list(predict = predict_Oonirici_WILD_P8p_SSSS_mod,
                      var.a = Oonirici_WILD_P8p_SSSS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_WILD_P8p_SSSS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_WILD_P8p_SSSS_mod <- data_Oonirici_WILD_P8p_SSSS_mod[["h2.obs"]]
      trait_mean_data_Oonirici_WILD_P8p_SSSS_mod <- data_Oonirici_WILD_P8p_SSSS_mod[["mean.obs"]]
      va_data_Oonirici_WILD_P8p_SSSS_mod <- data_Oonirici_WILD_P8p_SSSS_mod[["var.a.obs"]]
      vp_data_Oonirici_WILD_P8p_SSSS_mod <- data_Oonirici_WILD_P8p_SSSS_mod[["var.obs"]]
      
      Evol_data_Oonirici_WILD_P8p_SSSS_mod <- (va_data_Oonirici_WILD_P8p_SSSS_mod/2) / (trait_mean_data_Oonirici_WILD_P8p_SSSS_mod)^2
      
      mean(h2_data_Oonirici_WILD_P8p_SSSS_mod)
      mean(trait_mean_data_Oonirici_WILD_P8p_SSSS_mod)
      mean(va_data_Oonirici_WILD_P8p_SSSS_mod)
      mean(vp_data_Oonirici_WILD_P8p_SSSS_mod)
      mean(Evol_data_Oonirici_WILD_P8p_SSSS_mod)
      
    }
    
  }
  
  
  #Oonirici_Vg_P8p_SSSS_mod_2 
  {
    
    Oonirici_Vg_P8p_SSSS_mod_2 <- MCMCglmm(fixed       = P8.p_SSSS ~ Observer + at.level(Treatment,1):Ancestral + at.level(Treatment,2):Ancestral -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vg_Oonirici_data_woutCB,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)        
    
    
    saveRDS(Oonirici_Vg_P8p_SSSS_mod_2, file = "Oonirici_Vg_P8p_SSSS_mod_2.rds")
    Oonirici_Vg_P8p_SSSS_mod_2 <- readRDS("Oonirici_Vg_P8p_SSSS_mod_2.rds")
    
    summary(Oonirici_Vg_P8p_SSSS_mod_2) 
    #plotTrace(Oonirici_Vg_P8p_SSSS_mod_2$Sol)
    #View(Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]])
    
    # traces and posterior densities
    pdf("Oonirici_Vg_P8p_SSSS_mod_2.pdf")
    plot(Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:5)]) 
    plot(Oonirici_Vg_P8p_SSSS_mod_2[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_Vg_P8p_SSSS_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_Vg_P8p_SSSS_mod_2[["VCV"]])
    #autocorr.plot(Oonirici_Vg_P8p_SSSS_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_Vg_P8p_SSSS_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_Vg_P8p_SSSS_mod_2 <- Oonirici_Vg_P8p_SSSS_mod_2[["VCV"]][ , "LineB"]
      vr_Oonirici_Vg_P8p_SSSS_mod_2 <- Oonirici_Vg_P8p_SSSS_mod_2[["VCV"]][ , "units"]
      vlat_Oonirici_Vg_P8p_SSSS_mod_2 <- rowSums(Oonirici_Vg_P8p_SSSS_mod_2[["VCV"]])
      
      mean(va_liab_Oonirici_Vg_P8p_SSSS_mod_2) 
      HPDinterval(va_liab_Oonirici_Vg_P8p_SSSS_mod_2) 
      
      mean(vlat_Oonirici_Vg_P8p_SSSS_mod_2) 
      
      #variance of fixed effects
      X_Oonirici_Vg_P8p_SSSS_mod_2 <- Oonirici_Vg_P8p_SSSS_mod_2[["X"]]
      beta_Oonirici_Vg_P8p_SSSS_mod_2 <- Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:5)]
      vf_Oonirici_Vg_P8p_SSSS_mod_2   <- apply(beta_Oonirici_Vg_P8p_SSSS_mod_2, 1, function(b) {var(as.vector(X_Oonirici_Vg_P8p_SSSS_mod_2 %*% b))}) 
      mean(vf_Oonirici_Vg_P8p_SSSS_mod_2) 
      
      h2_liab_Oonirici_Vg_P8p_SSSS_mod_2 <- va_liab_Oonirici_Vg_P8p_SSSS_mod_2 / (vlat_Oonirici_Vg_P8p_SSSS_mod_2 + vf_Oonirici_Vg_P8p_SSSS_mod_2)
      mean(h2_liab_Oonirici_Vg_P8p_SSSS_mod_2) 
      posterior.mode(h2_liab_Oonirici_Vg_P8p_SSSS_mod_2)	
      median(h2_liab_Oonirici_Vg_P8p_SSSS_mod_2)		
      HPDinterval(h2_liab_Oonirici_Vg_P8p_SSSS_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:3)])) #Oni WILD P8p_SSSS
    mean(rowMeans(Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,4]) #JU77 CONTROL P8p_SSSS
    mean(rowMeans(Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,5]) #PS2068 Control P8p_SSSS
    
    trait_mean_liab_Oonirici_Vg_P8p_SSSS_mod_2 <- ((rowMeans(Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,4]) + (rowMeans(Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,5]) )/3)
    
    Evol_liab_Oonirici_Vg_P8p_SSSS_mod_2 <- (va_liab_Oonirici_Vg_P8p_SSSS_mod_2/2) / (trait_mean_liab_Oonirici_Vg_P8p_SSSS_mod_2)^2
    mean(Evol_liab_Oonirici_Vg_P8p_SSSS_mod_2)
    
    
    #Oonirici_Vg_P8p_SSSS_mod_2 data scale
    {
      
      predict_Oonirici_Vg_P8p_SSSS_mod_2 <- map(1:nrow(Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_Vg_P8p_SSSS_mod_2 %*% Oonirici_Vg_P8p_SSSS_mod_2[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_Vg_P8p_SSSS_mod_2 <-
        pmap_dfr(list(predict = predict_Oonirici_Vg_P8p_SSSS_mod_2,
                      var.a = Oonirici_Vg_P8p_SSSS_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_Vg_P8p_SSSS_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_Vg_P8p_SSSS_mod_2 <- data_Oonirici_Vg_P8p_SSSS_mod_2[["h2.obs"]]
      trait_mean_data_Oonirici_Vg_P8p_SSSS_mod_2 <- data_Oonirici_Vg_P8p_SSSS_mod_2[["mean.obs"]]
      va_data_Oonirici_Vg_P8p_SSSS_mod_2 <- data_Oonirici_Vg_P8p_SSSS_mod_2[["var.a.obs"]]
      vp_data_Oonirici_Vg_P8p_SSSS_mod_2 <- data_Oonirici_Vg_P8p_SSSS_mod_2[["var.obs"]]
      
      Evol_data_Oonirici_Vg_P8p_SSSS_mod_2 <- (va_data_Oonirici_Vg_P8p_SSSS_mod_2/2) / (trait_mean_data_Oonirici_Vg_P8p_SSSS_mod_2)^2
      
      mean(h2_data_Oonirici_Vg_P8p_SSSS_mod_2)
      mean(trait_mean_data_Oonirici_Vg_P8p_SSSS_mod_2)
      mean(va_data_Oonirici_Vg_P8p_SSSS_mod_2)
      mean(vp_data_Oonirici_Vg_P8p_SSSS_mod_2)
      mean(Evol_data_Oonirici_Vg_P8p_SSSS_mod_2)
      
    }
    
  }
  
  

## Oonirici P5p ----
  
  
  #Oonirici_CONTROL_P5p_wt_mod_2   {
    
    Oonirici_CONTROL_P5p_wt_mod_2 <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Ancestral -1,
                                              random      = ~ LineB + BlockRep,
                                              family      = "threshold",
                                              data        = Vg_Oonirici_bi_CONTROL_woutCB,
                                              prior       = prior_bi_block,
                                              nitt        = 1260000,       
                                              thin        = 500,           
                                              burnin      = 10000,
                                              trunc       = TRUE,
                                              pr          = TRUE,
                                              pl          = TRUE)            
    
    saveRDS(Oonirici_CONTROL_P5p_wt_mod_2, file = "Oonirici_CONTROL_P5p_wt_mod_2.rds")
    Oonirici_CONTROL_P5p_wt_mod_2 <- readRDS("Oonirici_CONTROL_P5p_wt_mod_2.rds")
    
    summary(Oonirici_CONTROL_P5p_wt_mod_2) 
    plotTrace(Oonirici_CONTROL_P5p_wt_mod_2$Sol)
    
    # traces and posterior densities
    
    pdf("Oonirici_CONTROL_P5p_wt_mod_2.pdf")
    plot(Oonirici_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:4)]) 
    plot(Oonirici_CONTROL_P5p_wt_mod_2[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:4)]) #
    heidel.diag(Oonirici_CONTROL_P5p_wt_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:4)])
    #autocorr.plot(Oonirici_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:4)]) #
    autocorr.diag(Oonirici_CONTROL_P5p_wt_mod_2[["VCV"]])
    #autocorr.plot(Oonirici_CONTROL_P5p_wt_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:4)]) # 
    effectiveSize(Oonirici_CONTROL_P5p_wt_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_CONTROL_P5p_wt_mod_2 <- Oonirici_CONTROL_P5p_wt_mod_2[["VCV"]][ , "LineB"]
      vr_Oonirici_CONTROL_P5p_wt_mod_2 <- Oonirici_CONTROL_P5p_wt_mod_2[["VCV"]][ , "units"]
      vlat_Oonirici_CONTROL_P5p_wt_mod_2 <- rowSums(Oonirici_CONTROL_P5p_wt_mod_2[["VCV"]])
      
      mean(va_liab_Oonirici_CONTROL_P5p_wt_mod_2) 
      HPDinterval(va_liab_Oonirici_CONTROL_P5p_wt_mod_2) 
      
      mean(vlat_Oonirici_CONTROL_P5p_wt_mod_2) 
      
      #variance of fixed effects
      X_Oonirici_CONTROL_P5p_wt_mod_2 <- Oonirici_CONTROL_P5p_wt_mod_2[["X"]]
      beta_Oonirici_CONTROL_P5p_wt_mod_2 <- Oonirici_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:4)]
      vf_Oonirici_CONTROL_P5p_wt_mod_2   <- apply(beta_Oonirici_CONTROL_P5p_wt_mod_2, 1, function(b) {var(as.vector(X_Oonirici_CONTROL_P5p_wt_mod_2 %*% b))}) 
      mean(vf_Oonirici_CONTROL_P5p_wt_mod_2) 
      
      h2_liab_Oonirici_CONTROL_P5p_wt_mod_2 <- va_liab_Oonirici_CONTROL_P5p_wt_mod_2 / (vlat_Oonirici_CONTROL_P5p_wt_mod_2 + vf_Oonirici_CONTROL_P5p_wt_mod_2)
      mean(h2_liab_Oonirici_CONTROL_P5p_wt_mod_2) 
      posterior.mode(h2_liab_Oonirici_CONTROL_P5p_wt_mod_2)	
      median(h2_liab_Oonirici_CONTROL_P5p_wt_mod_2)		
      HPDinterval(h2_liab_Oonirici_CONTROL_P5p_wt_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_CONTROL_P5p_wt_mod_2 <- ((rowMeans(Oonirici_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Oonirici_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:3)]) + Oonirici_CONTROL_P5p_wt_mod_2[["Sol"]][,4]))/2)
    
    Evol_liab_Oonirici_CONTROL_P5p_wt_mod_2 <- (va_liab_Oonirici_CONTROL_P5p_wt_mod_2/2) / (trait_mean_liab_Oonirici_CONTROL_P5p_wt_mod_2)^2
    mean(Evol_liab_Oonirici_CONTROL_P5p_wt_mod_2)
    
    
    #Oonirici_CONTROL_P5p_wt_mod_2 data scale
    {
      
      predict_Oonirici_CONTROL_P5p_wt_mod_2 <- map(1:nrow(Oonirici_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:4)]), ~ as.vector(X_Oonirici_CONTROL_P5p_wt_mod_2 %*% Oonirici_CONTROL_P5p_wt_mod_2[["Sol"]][,c(1:4)][., ]))
      
      data_Oonirici_CONTROL_P5p_wt_mod_2 <-
        pmap_dfr(list(predict = predict_Oonirici_CONTROL_P5p_wt_mod_2,
                      var.a = Oonirici_CONTROL_P5p_wt_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_CONTROL_P5p_wt_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_CONTROL_P5p_wt_mod_2 <- data_Oonirici_CONTROL_P5p_wt_mod_2[["h2.obs"]]
      trait_mean_data_Oonirici_CONTROL_P5p_wt_mod_2 <- data_Oonirici_CONTROL_P5p_wt_mod_2[["mean.obs"]]
      va_data_Oonirici_CONTROL_P5p_wt_mod_2 <- data_Oonirici_CONTROL_P5p_wt_mod_2[["var.a.obs"]]
      vp_data_Oonirici_CONTROL_P5p_wt_mod_2 <- data_Oonirici_CONTROL_P5p_wt_mod_2[["var.obs"]]
      
      Evol_data_Oonirici_CONTROL_P5p_wt_mod_2 <- (va_data_Oonirici_CONTROL_P5p_wt_mod_2/2) / (trait_mean_data_Oonirici_CONTROL_P5p_wt_mod_2)^2
      
      mean(h2_data_Oonirici_CONTROL_P5p_wt_mod_2) 
      mean(trait_mean_data_Oonirici_CONTROL_P5p_wt_mod_2)
      mean(va_data_Oonirici_CONTROL_P5p_wt_mod_2) 
      mean(vp_data_Oonirici_CONTROL_P5p_wt_mod_2)
      mean(Evol_data_Oonirici_CONTROL_P5p_wt_mod_2)
      
    }
    
  }
  
  #Oonirici_WILD_P5p_wt_mod   {
    
    Oonirici_WILD_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vg_Oonirici_bi_WILD,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)         
    
    saveRDS(Oonirici_WILD_P5p_wt_mod, file = "Oonirici_WILD_P5p_wt_mod.rds")
    Oonirici_WILD_P5p_wt_mod <- readRDS("Oonirici_WILD_P5p_wt_mod.rds")
    
    summary(Oonirici_WILD_P5p_wt_mod) 
    #plot(Oonirici_WILD_P5p_wt_mod)
    
    # traces and posterior densities
    pdf("Oonirici_WILD_P5p_wt_mod.pdf")
    plot(Oonirici_WILD_P5p_wt_mod[["Sol"]][,c(1:3)]) 
    plot(Oonirici_WILD_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_WILD_P5p_wt_mod[["Sol"]][,c(1:3)]) #
    heidel.diag(Oonirici_WILD_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_WILD_P5p_wt_mod[["Sol"]][,c(1:3)])
    #autocorr.plot(Oonirici_WILD_P5p_wt_mod[["Sol"]][,c(1:3)]) #
    autocorr.diag(Oonirici_WILD_P5p_wt_mod[["VCV"]])
    #autocorr.plot(Oonirici_WILD_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_WILD_P5p_wt_mod[["Sol"]][,c(1:3)]) # 
    effectiveSize(Oonirici_WILD_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_WILD_P5p_wt_mod <- Oonirici_WILD_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_WILD_P5p_wt_mod <- Oonirici_WILD_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_Oonirici_WILD_P5p_wt_mod <- rowSums(Oonirici_WILD_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_Oonirici_WILD_P5p_wt_mod) 
      HPDinterval(va_liab_Oonirici_WILD_P5p_wt_mod) 
      
      mean(vlat_Oonirici_WILD_P5p_wt_mod) 
      
      #variance of fixed effects
      X_Oonirici_WILD_P5p_wt_mod <- Oonirici_WILD_P5p_wt_mod[["X"]]
      beta_Oonirici_WILD_P5p_wt_mod <- Oonirici_WILD_P5p_wt_mod[["Sol"]][,c(1:3)]
      vf_Oonirici_WILD_P5p_wt_mod   <- apply(beta_Oonirici_WILD_P5p_wt_mod, 1, function(b) {var(as.vector(X_Oonirici_WILD_P5p_wt_mod %*% b))}) 
      mean(vf_Oonirici_WILD_P5p_wt_mod) 
      
      h2_liab_Oonirici_WILD_P5p_wt_mod <- va_liab_Oonirici_WILD_P5p_wt_mod / (vlat_Oonirici_WILD_P5p_wt_mod + vf_Oonirici_WILD_P5p_wt_mod)
      mean(h2_liab_Oonirici_WILD_P5p_wt_mod) 
      posterior.mode(h2_liab_Oonirici_WILD_P5p_wt_mod)	
      median(h2_liab_Oonirici_WILD_P5p_wt_mod)		
      HPDinterval(h2_liab_Oonirici_WILD_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Oonirici_WILD_P5p_wt_mod <- rowMeans(Oonirici_WILD_P5p_wt_mod[["Sol"]][,c(1:3)])
    
    Evol_liab_Oonirici_WILD_P5p_wt_mod <- (va_liab_Oonirici_WILD_P5p_wt_mod/2) / (trait_mean_liab_Oonirici_WILD_P5p_wt_mod)^2
    mean(Evol_liab_Oonirici_WILD_P5p_wt_mod)
    
    
    #Oonirici_WILD_P5p_wt_mod data scale
    {
      
      predict_Oonirici_WILD_P5p_wt_mod <- map(1:nrow(Oonirici_WILD_P5p_wt_mod[["Sol"]][,c(1:3)]), ~ as.vector(X_Oonirici_WILD_P5p_wt_mod %*% Oonirici_WILD_P5p_wt_mod[["Sol"]][,c(1:3)][., ]))
      
      data_Oonirici_WILD_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_Oonirici_WILD_P5p_wt_mod,
                      var.a = Oonirici_WILD_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_WILD_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_WILD_P5p_wt_mod <- data_Oonirici_WILD_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_Oonirici_WILD_P5p_wt_mod <- data_Oonirici_WILD_P5p_wt_mod[["mean.obs"]]
      va_data_Oonirici_WILD_P5p_wt_mod <- data_Oonirici_WILD_P5p_wt_mod[["var.a.obs"]]
      vp_data_Oonirici_WILD_P5p_wt_mod <- data_Oonirici_WILD_P5p_wt_mod[["var.obs"]]
      
      Evol_data_Oonirici_WILD_P5p_wt_mod <- (va_data_Oonirici_WILD_P5p_wt_mod/2) / (trait_mean_data_Oonirici_WILD_P5p_wt_mod)^2
      
      mean(h2_data_Oonirici_WILD_P5p_wt_mod)
      mean(trait_mean_data_Oonirici_WILD_P5p_wt_mod)
      mean(va_data_Oonirici_WILD_P5p_wt_mod)
      mean(vp_data_Oonirici_WILD_P5p_wt_mod)
      mean(Evol_data_Oonirici_WILD_P5p_wt_mod)
      
    }
    
  }
  
  #Oonirici_Vg_P5p_wt_mod_2   {
    
    Oonirici_Vg_P5p_wt_mod_2 <- MCMCglmm(fixed       = P5.p_wt ~ Observer + at.level(Treatment,1):Ancestral + at.level(Treatment,2):Ancestral -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vg_Oonirici_data_woutCB,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)        
    
    saveRDS(Oonirici_Vg_P5p_wt_mod_2, file = "Oonirici_Vg_P5p_wt_mod_2.rds")
    Oonirici_Vg_P5p_wt_mod_2 <- readRDS("Oonirici_Vg_P5p_wt_mod_2.rds")
    
    summary(Oonirici_Vg_P5p_wt_mod_2) 
    plotTrace(Oonirici_Vg_P5p_wt_mod_2$Sol)
    View(Oonirici_Vg_P5p_wt_mod_2[["Sol"]])
    # traces and posterior densities
    pdf("Oonirici_Vg_P5p_wt_mod_2.pdf")
    plot(Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,c(1:5)]) 
    plot(Oonirici_Vg_P5p_wt_mod_2[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_Vg_P5p_wt_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_Vg_P5p_wt_mod_2[["VCV"]])
    #autocorr.plot(Oonirici_Vg_P5p_wt_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_Vg_P5p_wt_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_Vg_P5p_wt_mod_2 <- Oonirici_Vg_P5p_wt_mod_2[["VCV"]][ , "LineB"]
      vr_Oonirici_Vg_P5p_wt_mod_2 <- Oonirici_Vg_P5p_wt_mod_2[["VCV"]][ , "units"]
      vlat_Oonirici_Vg_P5p_wt_mod_2 <- rowSums(Oonirici_Vg_P5p_wt_mod_2[["VCV"]])
      
      mean(va_liab_Oonirici_Vg_P5p_wt_mod_2) 
      HPDinterval(va_liab_Oonirici_Vg_P5p_wt_mod_2) 
      
      mean(vlat_Oonirici_Vg_P5p_wt_mod_2) 
      
      #variance of fixed effects
      X_Oonirici_Vg_P5p_wt_mod_2 <- Oonirici_Vg_P5p_wt_mod_2[["X"]]
      beta_Oonirici_Vg_P5p_wt_mod_2 <- Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,c(1:5)]
      vf_Oonirici_Vg_P5p_wt_mod_2   <- apply(beta_Oonirici_Vg_P5p_wt_mod_2, 1, function(b) {var(as.vector(X_Oonirici_Vg_P5p_wt_mod_2 %*% b))}) 
      mean(vf_Oonirici_Vg_P5p_wt_mod_2) 
      
      h2_liab_Oonirici_Vg_P5p_wt_mod_2 <- va_liab_Oonirici_Vg_P5p_wt_mod_2 / (vlat_Oonirici_Vg_P5p_wt_mod_2 + vf_Oonirici_Vg_P5p_wt_mod_2)
      mean(h2_liab_Oonirici_Vg_P5p_wt_mod_2) 
      posterior.mode(h2_liab_Oonirici_Vg_P5p_wt_mod_2)	
      median(h2_liab_Oonirici_Vg_P5p_wt_mod_2)		
      HPDinterval(h2_liab_Oonirici_Vg_P5p_wt_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,c(1:3)])) #Oni WILD P5p_wt
    mean(rowMeans(Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,4]) #JU77 CONTROL P5p_wt
    mean(rowMeans(Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,5]) #PS2068 Control P5p_wt
    
    trait_mean_liab_Oonirici_Vg_P5p_wt_mod_2 <- ((rowMeans(Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,4]) + (rowMeans(Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,5]) )/3)
    
    Evol_liab_Oonirici_Vg_P5p_wt_mod_2 <- (va_liab_Oonirici_Vg_P5p_wt_mod_2/2) / (trait_mean_liab_Oonirici_Vg_P5p_wt_mod_2)^2
    mean(Evol_liab_Oonirici_Vg_P5p_wt_mod_2)
    
    
    #Oonirici_Vg_P5p_wt_mod_2 data scale
    {
      
      predict_Oonirici_Vg_P5p_wt_mod_2 <- map(1:nrow(Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_Vg_P5p_wt_mod_2 %*% Oonirici_Vg_P5p_wt_mod_2[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_Vg_P5p_wt_mod_2 <-
        pmap_dfr(list(predict = predict_Oonirici_Vg_P5p_wt_mod_2,
                      var.a = Oonirici_Vg_P5p_wt_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_Vg_P5p_wt_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_Vg_P5p_wt_mod_2 <- data_Oonirici_Vg_P5p_wt_mod_2[["h2.obs"]]
      trait_mean_data_Oonirici_Vg_P5p_wt_mod_2 <- data_Oonirici_Vg_P5p_wt_mod_2[["mean.obs"]]
      va_data_Oonirici_Vg_P5p_wt_mod_2 <- data_Oonirici_Vg_P5p_wt_mod_2[["var.a.obs"]]
      vp_data_Oonirici_Vg_P5p_wt_mod_2 <- data_Oonirici_Vg_P5p_wt_mod_2[["var.obs"]]
      
      Evol_data_Oonirici_Vg_P5p_wt_mod_2 <- (va_data_Oonirici_Vg_P5p_wt_mod_2/2) / (trait_mean_data_Oonirici_Vg_P5p_wt_mod_2)^2
      
      mean(h2_data_Oonirici_Vg_P5p_wt_mod_2)
      mean(trait_mean_data_Oonirici_Vg_P5p_wt_mod_2)
      mean(va_data_Oonirici_Vg_P5p_wt_mod_2)
      mean(vp_data_Oonirici_Vg_P5p_wt_mod_2)
      mean(Evol_data_Oonirici_Vg_P5p_wt_mod_2)
      
    }
    
  }
  
  
  
  ## Oonirici P6p ----
  
  
  #Oonirici_CONTROL_P6p_wt_mod_2   {
    
    Oonirici_CONTROL_P6p_wt_mod_2 <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Ancestral -1,
                                              random      = ~ LineB + BlockRep,
                                              family      = "threshold",
                                              data        = Vg_Oonirici_bi_CONTROL_woutCB,
                                              prior       = prior_bi_block,
                                              nitt        = 1260000,       
                                              thin        = 500,           
                                              burnin      = 10000,
                                              trunc       = TRUE,
                                              pr          = TRUE,
                                              pl          = TRUE)            
    
    saveRDS(Oonirici_CONTROL_P6p_wt_mod_2, file = "Oonirici_CONTROL_P6p_wt_mod_2.rds")
    Oonirici_CONTROL_P6p_wt_mod_2 <- readRDS("Oonirici_CONTROL_P6p_wt_mod_2.rds")
    
    summary(Oonirici_CONTROL_P6p_wt_mod_2) 
    plotTrace(Oonirici_CONTROL_P6p_wt_mod_2$Sol)
    
    # traces and posterior densities
    
    pdf("Oonirici_CONTROL_P6p_wt_mod_2.pdf")
    plot(Oonirici_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:4)]) 
    plot(Oonirici_CONTROL_P6p_wt_mod_2[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:4)]) #
    heidel.diag(Oonirici_CONTROL_P6p_wt_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:4)])
    #autocorr.plot(Oonirici_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:4)]) #
    autocorr.diag(Oonirici_CONTROL_P6p_wt_mod_2[["VCV"]])
    #autocorr.plot(Oonirici_CONTROL_P6p_wt_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:4)]) # 
    effectiveSize(Oonirici_CONTROL_P6p_wt_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_CONTROL_P6p_wt_mod_2 <- Oonirici_CONTROL_P6p_wt_mod_2[["VCV"]][ , "LineB"]
      vr_Oonirici_CONTROL_P6p_wt_mod_2 <- Oonirici_CONTROL_P6p_wt_mod_2[["VCV"]][ , "units"]
      vlat_Oonirici_CONTROL_P6p_wt_mod_2 <- rowSums(Oonirici_CONTROL_P6p_wt_mod_2[["VCV"]])
      
      mean(va_liab_Oonirici_CONTROL_P6p_wt_mod_2) 
      HPDinterval(va_liab_Oonirici_CONTROL_P6p_wt_mod_2) 
      
      mean(vlat_Oonirici_CONTROL_P6p_wt_mod_2) 
      
      #variance of fixed effects
      X_Oonirici_CONTROL_P6p_wt_mod_2 <- Oonirici_CONTROL_P6p_wt_mod_2[["X"]]
      beta_Oonirici_CONTROL_P6p_wt_mod_2 <- Oonirici_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:4)]
      vf_Oonirici_CONTROL_P6p_wt_mod_2   <- apply(beta_Oonirici_CONTROL_P6p_wt_mod_2, 1, function(b) {var(as.vector(X_Oonirici_CONTROL_P6p_wt_mod_2 %*% b))}) 
      mean(vf_Oonirici_CONTROL_P6p_wt_mod_2) 
      
      h2_liab_Oonirici_CONTROL_P6p_wt_mod_2 <- va_liab_Oonirici_CONTROL_P6p_wt_mod_2 / (vlat_Oonirici_CONTROL_P6p_wt_mod_2 + vf_Oonirici_CONTROL_P6p_wt_mod_2)
      mean(h2_liab_Oonirici_CONTROL_P6p_wt_mod_2) 
      posterior.mode(h2_liab_Oonirici_CONTROL_P6p_wt_mod_2)	
      median(h2_liab_Oonirici_CONTROL_P6p_wt_mod_2)		
      HPDinterval(h2_liab_Oonirici_CONTROL_P6p_wt_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_CONTROL_P6p_wt_mod_2 <- ((rowMeans(Oonirici_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Oonirici_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:3)]) + Oonirici_CONTROL_P6p_wt_mod_2[["Sol"]][,4]))/2)
    
    Evol_liab_Oonirici_CONTROL_P6p_wt_mod_2 <- (va_liab_Oonirici_CONTROL_P6p_wt_mod_2/2) / (trait_mean_liab_Oonirici_CONTROL_P6p_wt_mod_2)^2
    mean(Evol_liab_Oonirici_CONTROL_P6p_wt_mod_2)
    
    
    
    #Oonirici_CONTROL_P6p_wt_mod_2 data scale
    {
      
      predict_Oonirici_CONTROL_P6p_wt_mod_2 <- map(1:nrow(Oonirici_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:4)]), ~ as.vector(X_Oonirici_CONTROL_P6p_wt_mod_2 %*% Oonirici_CONTROL_P6p_wt_mod_2[["Sol"]][,c(1:4)][., ]))
      
      data_Oonirici_CONTROL_P6p_wt_mod_2 <-
        pmap_dfr(list(predict = predict_Oonirici_CONTROL_P6p_wt_mod_2,
                      var.a = Oonirici_CONTROL_P6p_wt_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_CONTROL_P6p_wt_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_CONTROL_P6p_wt_mod_2 <- data_Oonirici_CONTROL_P6p_wt_mod_2[["h2.obs"]]
      trait_mean_data_Oonirici_CONTROL_P6p_wt_mod_2 <- data_Oonirici_CONTROL_P6p_wt_mod_2[["mean.obs"]]
      va_data_Oonirici_CONTROL_P6p_wt_mod_2 <- data_Oonirici_CONTROL_P6p_wt_mod_2[["var.a.obs"]]
      vp_data_Oonirici_CONTROL_P6p_wt_mod_2 <- data_Oonirici_CONTROL_P6p_wt_mod_2[["var.obs"]]
      
      Evol_data_Oonirici_CONTROL_P6p_wt_mod_2 <- (va_data_Oonirici_CONTROL_P6p_wt_mod_2/2) / (trait_mean_data_Oonirici_CONTROL_P6p_wt_mod_2)^2
      
      mean(h2_data_Oonirici_CONTROL_P6p_wt_mod_2) 
      mean(trait_mean_data_Oonirici_CONTROL_P6p_wt_mod_2)
      mean(va_data_Oonirici_CONTROL_P6p_wt_mod_2) 
      mean(vp_data_Oonirici_CONTROL_P6p_wt_mod_2)
      mean(Evol_data_Oonirici_CONTROL_P6p_wt_mod_2)
      
    }
    
  }
  
  #Oonirici_WILD_P6p_wt_mod   {
    
    Oonirici_WILD_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vg_Oonirici_bi_WILD,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)         
    
    saveRDS(Oonirici_WILD_P6p_wt_mod, file = "Oonirici_WILD_P6p_wt_mod.rds")
    Oonirici_WILD_P6p_wt_mod <- readRDS("Oonirici_WILD_P6p_wt_mod.rds")
    
    summary(Oonirici_WILD_P6p_wt_mod) 
    #plot(Oonirici_WILD_P6p_wt_mod)
    
    # traces and posterior densities
    pdf("Oonirici_WILD_P6p_wt_mod.pdf")
    plot(Oonirici_WILD_P6p_wt_mod[["Sol"]][,c(1:3)]) 
    plot(Oonirici_WILD_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_WILD_P6p_wt_mod[["Sol"]][,c(1:3)]) #
    heidel.diag(Oonirici_WILD_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_WILD_P6p_wt_mod[["Sol"]][,c(1:3)])
    #autocorr.plot(Oonirici_WILD_P6p_wt_mod[["Sol"]][,c(1:3)]) #
    autocorr.diag(Oonirici_WILD_P6p_wt_mod[["VCV"]])
    #autocorr.plot(Oonirici_WILD_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_WILD_P6p_wt_mod[["Sol"]][,c(1:3)]) # 
    effectiveSize(Oonirici_WILD_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_WILD_P6p_wt_mod <- Oonirici_WILD_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_WILD_P6p_wt_mod <- Oonirici_WILD_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_Oonirici_WILD_P6p_wt_mod <- rowSums(Oonirici_WILD_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_Oonirici_WILD_P6p_wt_mod) 
      HPDinterval(va_liab_Oonirici_WILD_P6p_wt_mod) 
      
      mean(vlat_Oonirici_WILD_P6p_wt_mod) 
      
      #variance of fixed effects
      X_Oonirici_WILD_P6p_wt_mod <- Oonirici_WILD_P6p_wt_mod[["X"]]
      beta_Oonirici_WILD_P6p_wt_mod <- Oonirici_WILD_P6p_wt_mod[["Sol"]][,c(1:3)]
      vf_Oonirici_WILD_P6p_wt_mod   <- apply(beta_Oonirici_WILD_P6p_wt_mod, 1, function(b) {var(as.vector(X_Oonirici_WILD_P6p_wt_mod %*% b))}) 
      mean(vf_Oonirici_WILD_P6p_wt_mod) 
      
      h2_liab_Oonirici_WILD_P6p_wt_mod <- va_liab_Oonirici_WILD_P6p_wt_mod / (vlat_Oonirici_WILD_P6p_wt_mod + vf_Oonirici_WILD_P6p_wt_mod)
      mean(h2_liab_Oonirici_WILD_P6p_wt_mod) 
      posterior.mode(h2_liab_Oonirici_WILD_P6p_wt_mod)	
      median(h2_liab_Oonirici_WILD_P6p_wt_mod)		
      HPDinterval(h2_liab_Oonirici_WILD_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Oonirici_WILD_P6p_wt_mod <- rowMeans(Oonirici_WILD_P6p_wt_mod[["Sol"]][,c(1:3)])
    
    Evol_liab_Oonirici_WILD_P6p_wt_mod <- (va_liab_Oonirici_WILD_P6p_wt_mod/2) / (trait_mean_liab_Oonirici_WILD_P6p_wt_mod)^2
    mean(Evol_liab_Oonirici_WILD_P6p_wt_mod)
    
    
    #Oonirici_WILD_P6p_wt_mod data scale
    {
      
      predict_Oonirici_WILD_P6p_wt_mod <- map(1:nrow(Oonirici_WILD_P6p_wt_mod[["Sol"]][,c(1:3)]), ~ as.vector(X_Oonirici_WILD_P6p_wt_mod %*% Oonirici_WILD_P6p_wt_mod[["Sol"]][,c(1:3)][., ]))
      
      data_Oonirici_WILD_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_Oonirici_WILD_P6p_wt_mod,
                      var.a = Oonirici_WILD_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_WILD_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_WILD_P6p_wt_mod <- data_Oonirici_WILD_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_Oonirici_WILD_P6p_wt_mod <- data_Oonirici_WILD_P6p_wt_mod[["mean.obs"]]
      va_data_Oonirici_WILD_P6p_wt_mod <- data_Oonirici_WILD_P6p_wt_mod[["var.a.obs"]]
      vp_data_Oonirici_WILD_P6p_wt_mod <- data_Oonirici_WILD_P6p_wt_mod[["var.obs"]]
      
      Evol_data_Oonirici_WILD_P6p_wt_mod <- (va_data_Oonirici_WILD_P6p_wt_mod/2) / (trait_mean_data_Oonirici_WILD_P6p_wt_mod)^2
      
      mean(h2_data_Oonirici_WILD_P6p_wt_mod)
      mean(trait_mean_data_Oonirici_WILD_P6p_wt_mod)
      mean(va_data_Oonirici_WILD_P6p_wt_mod)
      mean(vp_data_Oonirici_WILD_P6p_wt_mod)
      mean(Evol_data_Oonirici_WILD_P6p_wt_mod)
      
    }
    
  }
  #Oonirici_Vg_P6p_wt_mod_2   {
    
    Oonirici_Vg_P6p_wt_mod_2 <- MCMCglmm(fixed       = P6.p_wt ~ Observer + at.level(Treatment,1):Ancestral + at.level(Treatment,2):Ancestral -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vg_Oonirici_data_woutCB,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)        
    
    
    saveRDS(Oonirici_Vg_P6p_wt_mod_2, file = "Oonirici_Vg_P6p_wt_mod_2.rds")
    Oonirici_Vg_P6p_wt_mod_2 <- readRDS("Oonirici_Vg_P6p_wt_mod_2.rds")
    
    summary(Oonirici_Vg_P6p_wt_mod_2) 
    plotTrace(Oonirici_Vg_P6p_wt_mod_2$Sol)
    View(Oonirici_Vg_P6p_wt_mod_2[["Sol"]])
    # traces and posterior densities
    pdf("Oonirici_Vg_P6p_wt_mod_2.pdf")
    plot(Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,c(1:5)]) 
    plot(Oonirici_Vg_P6p_wt_mod_2[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_Vg_P6p_wt_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_Vg_P6p_wt_mod_2[["VCV"]])
    #autocorr.plot(Oonirici_Vg_P6p_wt_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_Vg_P6p_wt_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_Vg_P6p_wt_mod_2 <- Oonirici_Vg_P6p_wt_mod_2[["VCV"]][ , "LineB"]
      vr_Oonirici_Vg_P6p_wt_mod_2 <- Oonirici_Vg_P6p_wt_mod_2[["VCV"]][ , "units"]
      vlat_Oonirici_Vg_P6p_wt_mod_2 <- rowSums(Oonirici_Vg_P6p_wt_mod_2[["VCV"]])
      
      mean(va_liab_Oonirici_Vg_P6p_wt_mod_2) 
      HPDinterval(va_liab_Oonirici_Vg_P6p_wt_mod_2) 
      
      mean(vlat_Oonirici_Vg_P6p_wt_mod_2) 
      
      #variance of fixed effects
      X_Oonirici_Vg_P6p_wt_mod_2 <- Oonirici_Vg_P6p_wt_mod_2[["X"]]
      beta_Oonirici_Vg_P6p_wt_mod_2 <- Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,c(1:5)]
      vf_Oonirici_Vg_P6p_wt_mod_2   <- apply(beta_Oonirici_Vg_P6p_wt_mod_2, 1, function(b) {var(as.vector(X_Oonirici_Vg_P6p_wt_mod_2 %*% b))}) 
      mean(vf_Oonirici_Vg_P6p_wt_mod_2) 
      
      h2_liab_Oonirici_Vg_P6p_wt_mod_2 <- va_liab_Oonirici_Vg_P6p_wt_mod_2 / (vlat_Oonirici_Vg_P6p_wt_mod_2 + vf_Oonirici_Vg_P6p_wt_mod_2)
      mean(h2_liab_Oonirici_Vg_P6p_wt_mod_2) 
      posterior.mode(h2_liab_Oonirici_Vg_P6p_wt_mod_2)	
      median(h2_liab_Oonirici_Vg_P6p_wt_mod_2)		
      HPDinterval(h2_liab_Oonirici_Vg_P6p_wt_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,c(1:3)])) #Oni WILD P6p_wt
    mean(rowMeans(Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,4]) #JU77 CONTROL P6p_wt
    mean(rowMeans(Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,5]) #PS2068 Control P6p_wt
    
    trait_mean_liab_Oonirici_Vg_P6p_wt_mod_2 <- ((rowMeans(Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,4]) + (rowMeans(Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,5]) )/3)
    
    Evol_liab_Oonirici_Vg_P6p_wt_mod_2 <- (va_liab_Oonirici_Vg_P6p_wt_mod_2/2) / (trait_mean_liab_Oonirici_Vg_P6p_wt_mod_2)^2
    mean(Evol_liab_Oonirici_Vg_P6p_wt_mod_2)
    
    
    #Oonirici_Vg_P6p_wt_mod_2 data scale
    {
      
      predict_Oonirici_Vg_P6p_wt_mod_2 <- map(1:nrow(Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_Vg_P6p_wt_mod_2 %*% Oonirici_Vg_P6p_wt_mod_2[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_Vg_P6p_wt_mod_2 <-
        pmap_dfr(list(predict = predict_Oonirici_Vg_P6p_wt_mod_2,
                      var.a = Oonirici_Vg_P6p_wt_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_Vg_P6p_wt_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_Vg_P6p_wt_mod_2 <- data_Oonirici_Vg_P6p_wt_mod_2[["h2.obs"]]
      trait_mean_data_Oonirici_Vg_P6p_wt_mod_2 <- data_Oonirici_Vg_P6p_wt_mod_2[["mean.obs"]]
      va_data_Oonirici_Vg_P6p_wt_mod_2 <- data_Oonirici_Vg_P6p_wt_mod_2[["var.a.obs"]]
      vp_data_Oonirici_Vg_P6p_wt_mod_2 <- data_Oonirici_Vg_P6p_wt_mod_2[["var.obs"]]
      
      Evol_data_Oonirici_Vg_P6p_wt_mod_2 <- (va_data_Oonirici_Vg_P6p_wt_mod_2/2) / (trait_mean_data_Oonirici_Vg_P6p_wt_mod_2)^2
      
      mean(h2_data_Oonirici_Vg_P6p_wt_mod_2)
      mean(trait_mean_data_Oonirici_Vg_P6p_wt_mod_2)
      mean(va_data_Oonirici_Vg_P6p_wt_mod_2)
      mean(vp_data_Oonirici_Vg_P6p_wt_mod_2)
      mean(Evol_data_Oonirici_Vg_P6p_wt_mod_2)
      
    }
    
  }
  
 
  ## Oonirici P7p ----
  
  
  #Oonirici_CONTROL_P7p_wt_mod_2   {
    
    Oonirici_CONTROL_P7p_wt_mod_2 <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Ancestral -1,
                                              random      = ~ LineB + BlockRep,
                                              family      = "threshold",
                                              data        = Vg_Oonirici_bi_CONTROL_woutCB,
                                              prior       = prior_bi_block,
                                              nitt        = 1260000,       
                                              thin        = 500,           
                                              burnin      = 10000,
                                              trunc       = TRUE,
                                              pr          = TRUE,
                                              pl          = TRUE)            
    
    saveRDS(Oonirici_CONTROL_P7p_wt_mod_2, file = "Oonirici_CONTROL_P7p_wt_mod_2.rds")
    Oonirici_CONTROL_P7p_wt_mod_2 <- readRDS("Oonirici_CONTROL_P7p_wt_mod_2.rds")
    
    summary(Oonirici_CONTROL_P7p_wt_mod_2) 
    plotTrace(Oonirici_CONTROL_P7p_wt_mod_2$Sol)
    
    # traces and posterior densities
    
    pdf("Oonirici_CONTROL_P7p_wt_mod_2.pdf")
    plot(Oonirici_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:4)]) 
    plot(Oonirici_CONTROL_P7p_wt_mod_2[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:4)]) #
    heidel.diag(Oonirici_CONTROL_P7p_wt_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:4)])
    #autocorr.plot(Oonirici_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:4)]) #
    autocorr.diag(Oonirici_CONTROL_P7p_wt_mod_2[["VCV"]])
    #autocorr.plot(Oonirici_CONTROL_P7p_wt_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:4)]) # 
    effectiveSize(Oonirici_CONTROL_P7p_wt_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_CONTROL_P7p_wt_mod_2 <- Oonirici_CONTROL_P7p_wt_mod_2[["VCV"]][ , "LineB"]
      vr_Oonirici_CONTROL_P7p_wt_mod_2 <- Oonirici_CONTROL_P7p_wt_mod_2[["VCV"]][ , "units"]
      vlat_Oonirici_CONTROL_P7p_wt_mod_2 <- rowSums(Oonirici_CONTROL_P7p_wt_mod_2[["VCV"]])
      
      mean(va_liab_Oonirici_CONTROL_P7p_wt_mod_2) 
      HPDinterval(va_liab_Oonirici_CONTROL_P7p_wt_mod_2) 
      
      mean(vlat_Oonirici_CONTROL_P7p_wt_mod_2) 
      
      #variance of fixed effects
      X_Oonirici_CONTROL_P7p_wt_mod_2 <- Oonirici_CONTROL_P7p_wt_mod_2[["X"]]
      beta_Oonirici_CONTROL_P7p_wt_mod_2 <- Oonirici_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:4)]
      vf_Oonirici_CONTROL_P7p_wt_mod_2   <- apply(beta_Oonirici_CONTROL_P7p_wt_mod_2, 1, function(b) {var(as.vector(X_Oonirici_CONTROL_P7p_wt_mod_2 %*% b))}) 
      mean(vf_Oonirici_CONTROL_P7p_wt_mod_2) 
      
      h2_liab_Oonirici_CONTROL_P7p_wt_mod_2 <- va_liab_Oonirici_CONTROL_P7p_wt_mod_2 / (vlat_Oonirici_CONTROL_P7p_wt_mod_2 + vf_Oonirici_CONTROL_P7p_wt_mod_2)
      mean(h2_liab_Oonirici_CONTROL_P7p_wt_mod_2) 
      posterior.mode(h2_liab_Oonirici_CONTROL_P7p_wt_mod_2)	
      median(h2_liab_Oonirici_CONTROL_P7p_wt_mod_2)		
      HPDinterval(h2_liab_Oonirici_CONTROL_P7p_wt_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Oonirici_CONTROL_P7p_wt_mod_2 <- ((rowMeans(Oonirici_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Oonirici_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:3)]) + Oonirici_CONTROL_P7p_wt_mod_2[["Sol"]][,4]))/2)
    
    Evol_liab_Oonirici_CONTROL_P7p_wt_mod_2 <- (va_liab_Oonirici_CONTROL_P7p_wt_mod_2/2) / (trait_mean_liab_Oonirici_CONTROL_P7p_wt_mod_2)^2
    mean(Evol_liab_Oonirici_CONTROL_P7p_wt_mod_2)
    
    
    #Oonirici_CONTROL_P7p_wt_mod_2 data scale
    {
      
      predict_Oonirici_CONTROL_P7p_wt_mod_2 <- map(1:nrow(Oonirici_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:4)]), ~ as.vector(X_Oonirici_CONTROL_P7p_wt_mod_2 %*% Oonirici_CONTROL_P7p_wt_mod_2[["Sol"]][,c(1:4)][., ]))
      
      data_Oonirici_CONTROL_P7p_wt_mod_2 <-
        pmap_dfr(list(predict = predict_Oonirici_CONTROL_P7p_wt_mod_2,
                      var.a = Oonirici_CONTROL_P7p_wt_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_CONTROL_P7p_wt_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_CONTROL_P7p_wt_mod_2 <- data_Oonirici_CONTROL_P7p_wt_mod_2[["h2.obs"]]
      trait_mean_data_Oonirici_CONTROL_P7p_wt_mod_2 <- data_Oonirici_CONTROL_P7p_wt_mod_2[["mean.obs"]]
      va_data_Oonirici_CONTROL_P7p_wt_mod_2 <- data_Oonirici_CONTROL_P7p_wt_mod_2[["var.a.obs"]]
      vp_data_Oonirici_CONTROL_P7p_wt_mod_2 <- data_Oonirici_CONTROL_P7p_wt_mod_2[["var.obs"]]
      
      Evol_data_Oonirici_CONTROL_P7p_wt_mod_2 <- (va_data_Oonirici_CONTROL_P7p_wt_mod_2/2) / (trait_mean_data_Oonirici_CONTROL_P7p_wt_mod_2)^2
      
      mean(h2_data_Oonirici_CONTROL_P7p_wt_mod_2) 
      mean(trait_mean_data_Oonirici_CONTROL_P7p_wt_mod_2)
      mean(va_data_Oonirici_CONTROL_P7p_wt_mod_2) 
      mean(vp_data_Oonirici_CONTROL_P7p_wt_mod_2)
      mean(Evol_data_Oonirici_CONTROL_P7p_wt_mod_2)
      
    }
    
  }
  
  #Oonirici_WILD_P7p_wt_mod   {
    
    Oonirici_WILD_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vg_Oonirici_bi_WILD,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)         
    
    saveRDS(Oonirici_WILD_P7p_wt_mod, file = "Oonirici_WILD_P7p_wt_mod.rds")
    Oonirici_WILD_P7p_wt_mod <- readRDS("Oonirici_WILD_P7p_wt_mod.rds")
    
    summary(Oonirici_WILD_P7p_wt_mod) 
    #plot(Oonirici_WILD_P7p_wt_mod)
    
    # traces and posterior densities
    pdf("Oonirici_WILD_P7p_wt_mod.pdf")
    plot(Oonirici_WILD_P7p_wt_mod[["Sol"]][,c(1:3)]) 
    plot(Oonirici_WILD_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_WILD_P7p_wt_mod[["Sol"]][,c(1:3)]) #
    heidel.diag(Oonirici_WILD_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_WILD_P7p_wt_mod[["Sol"]][,c(1:3)])
    #autocorr.plot(Oonirici_WILD_P7p_wt_mod[["Sol"]][,c(1:3)]) #
    autocorr.diag(Oonirici_WILD_P7p_wt_mod[["VCV"]])
    #autocorr.plot(Oonirici_WILD_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_WILD_P7p_wt_mod[["Sol"]][,c(1:3)]) # 
    effectiveSize(Oonirici_WILD_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_WILD_P7p_wt_mod <- Oonirici_WILD_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_Oonirici_WILD_P7p_wt_mod <- Oonirici_WILD_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_Oonirici_WILD_P7p_wt_mod <- rowSums(Oonirici_WILD_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_Oonirici_WILD_P7p_wt_mod) 
      HPDinterval(va_liab_Oonirici_WILD_P7p_wt_mod) 
      
      mean(vlat_Oonirici_WILD_P7p_wt_mod) 
      
      #variance of fixed effects
      X_Oonirici_WILD_P7p_wt_mod <- Oonirici_WILD_P7p_wt_mod[["X"]]
      beta_Oonirici_WILD_P7p_wt_mod <- Oonirici_WILD_P7p_wt_mod[["Sol"]][,c(1:3)]
      vf_Oonirici_WILD_P7p_wt_mod   <- apply(beta_Oonirici_WILD_P7p_wt_mod, 1, function(b) {var(as.vector(X_Oonirici_WILD_P7p_wt_mod %*% b))}) 
      mean(vf_Oonirici_WILD_P7p_wt_mod) 
      
      h2_liab_Oonirici_WILD_P7p_wt_mod <- va_liab_Oonirici_WILD_P7p_wt_mod / (vlat_Oonirici_WILD_P7p_wt_mod + vf_Oonirici_WILD_P7p_wt_mod)
      mean(h2_liab_Oonirici_WILD_P7p_wt_mod) 
      posterior.mode(h2_liab_Oonirici_WILD_P7p_wt_mod)	
      median(h2_liab_Oonirici_WILD_P7p_wt_mod)		
      HPDinterval(h2_liab_Oonirici_WILD_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Oonirici_WILD_P7p_wt_mod <- rowMeans(Oonirici_WILD_P7p_wt_mod[["Sol"]][,c(1:3)])
    
    Evol_liab_Oonirici_WILD_P7p_wt_mod <- (va_liab_Oonirici_WILD_P7p_wt_mod/2) / (trait_mean_liab_Oonirici_WILD_P7p_wt_mod)^2
    mean(Evol_liab_Oonirici_WILD_P7p_wt_mod)
    
    
    #Oonirici_WILD_P7p_wt_mod data scale
    {
      
      predict_Oonirici_WILD_P7p_wt_mod <- map(1:nrow(Oonirici_WILD_P7p_wt_mod[["Sol"]][,c(1:3)]), ~ as.vector(X_Oonirici_WILD_P7p_wt_mod %*% Oonirici_WILD_P7p_wt_mod[["Sol"]][,c(1:3)][., ]))
      
      data_Oonirici_WILD_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_Oonirici_WILD_P7p_wt_mod,
                      var.a = Oonirici_WILD_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_WILD_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_WILD_P7p_wt_mod <- data_Oonirici_WILD_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_Oonirici_WILD_P7p_wt_mod <- data_Oonirici_WILD_P7p_wt_mod[["mean.obs"]]
      va_data_Oonirici_WILD_P7p_wt_mod <- data_Oonirici_WILD_P7p_wt_mod[["var.a.obs"]]
      vp_data_Oonirici_WILD_P7p_wt_mod <- data_Oonirici_WILD_P7p_wt_mod[["var.obs"]]
      
      Evol_data_Oonirici_WILD_P7p_wt_mod <- (va_data_Oonirici_WILD_P7p_wt_mod/2) / (trait_mean_data_Oonirici_WILD_P7p_wt_mod)^2
      
      mean(h2_data_Oonirici_WILD_P7p_wt_mod)
      mean(trait_mean_data_Oonirici_WILD_P7p_wt_mod)
      mean(va_data_Oonirici_WILD_P7p_wt_mod)
      mean(vp_data_Oonirici_WILD_P7p_wt_mod)
      mean(Evol_data_Oonirici_WILD_P7p_wt_mod)
      
    }
    
  }
  
  
  #Oonirici_Vg_P7p_wt_mod_2   {
    
    Oonirici_Vg_P7p_wt_mod_2 <- MCMCglmm(fixed       = P7.p_wt ~ Observer + at.level(Treatment,1):Ancestral + at.level(Treatment,2):Ancestral -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vg_Oonirici_data_woutCB,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)        
    
    
    saveRDS(Oonirici_Vg_P7p_wt_mod_2, file = "Oonirici_Vg_P7p_wt_mod_2.rds")
    Oonirici_Vg_P7p_wt_mod_2 <- readRDS("Oonirici_Vg_P7p_wt_mod_2.rds")
    
    summary(Oonirici_Vg_P7p_wt_mod_2) 
    plotTrace(Oonirici_Vg_P7p_wt_mod_2$Sol)
    View(Oonirici_Vg_P7p_wt_mod_2[["Sol"]])
    # traces and posterior densities
    pdf("Oonirici_Vg_P7p_wt_mod_2.pdf")
    plot(Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,c(1:5)]) 
    plot(Oonirici_Vg_P7p_wt_mod_2[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,c(1:5)]) #
    heidel.diag(Oonirici_Vg_P7p_wt_mod_2[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,c(1:5)])
    #autocorr.plot(Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,c(1:5)]) #
    autocorr.diag(Oonirici_Vg_P7p_wt_mod_2[["VCV"]])
    #autocorr.plot(Oonirici_Vg_P7p_wt_mod_2[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,c(1:5)]) # 
    effectiveSize(Oonirici_Vg_P7p_wt_mod_2[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Oonirici_Vg_P7p_wt_mod_2 <- Oonirici_Vg_P7p_wt_mod_2[["VCV"]][ , "LineB"]
      vr_Oonirici_Vg_P7p_wt_mod_2 <- Oonirici_Vg_P7p_wt_mod_2[["VCV"]][ , "units"]
      vlat_Oonirici_Vg_P7p_wt_mod_2 <- rowSums(Oonirici_Vg_P7p_wt_mod_2[["VCV"]])
      
      mean(va_liab_Oonirici_Vg_P7p_wt_mod_2) 
      HPDinterval(va_liab_Oonirici_Vg_P7p_wt_mod_2) 
      
      mean(vlat_Oonirici_Vg_P7p_wt_mod_2) 
      
      #variance of fixed effects
      X_Oonirici_Vg_P7p_wt_mod_2 <- Oonirici_Vg_P7p_wt_mod_2[["X"]]
      beta_Oonirici_Vg_P7p_wt_mod_2 <- Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,c(1:5)]
      vf_Oonirici_Vg_P7p_wt_mod_2   <- apply(beta_Oonirici_Vg_P7p_wt_mod_2, 1, function(b) {var(as.vector(X_Oonirici_Vg_P7p_wt_mod_2 %*% b))}) 
      mean(vf_Oonirici_Vg_P7p_wt_mod_2) 
      
      h2_liab_Oonirici_Vg_P7p_wt_mod_2 <- va_liab_Oonirici_Vg_P7p_wt_mod_2 / (vlat_Oonirici_Vg_P7p_wt_mod_2 + vf_Oonirici_Vg_P7p_wt_mod_2)
      mean(h2_liab_Oonirici_Vg_P7p_wt_mod_2) 
      posterior.mode(h2_liab_Oonirici_Vg_P7p_wt_mod_2)	
      median(h2_liab_Oonirici_Vg_P7p_wt_mod_2)		
      HPDinterval(h2_liab_Oonirici_Vg_P7p_wt_mod_2)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,c(1:3)])) #Oni WILD P7p_wt
    mean(rowMeans(Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,4]) #JU77 CONTROL P7p_wt
    mean(rowMeans(Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,5]) #PS2068 Control P7p_wt
    
    trait_mean_liab_Oonirici_Vg_P7p_wt_mod_2 <- ((rowMeans(Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,c(1:3)]) + (rowMeans(Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,4]) + (rowMeans(Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,c(1:3)]) + Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,5]) )/3)
    
    Evol_liab_Oonirici_Vg_P7p_wt_mod_2 <- (va_liab_Oonirici_Vg_P7p_wt_mod_2/2) / (trait_mean_liab_Oonirici_Vg_P7p_wt_mod_2)^2
    mean(Evol_liab_Oonirici_Vg_P7p_wt_mod_2)
    
    
    #Oonirici_Vg_P7p_wt_mod_2 data scale
    {
      
      predict_Oonirici_Vg_P7p_wt_mod_2 <- map(1:nrow(Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,c(1:5)]), ~ as.vector(X_Oonirici_Vg_P7p_wt_mod_2 %*% Oonirici_Vg_P7p_wt_mod_2[["Sol"]][,c(1:5)][., ]))
      
      data_Oonirici_Vg_P7p_wt_mod_2 <-
        pmap_dfr(list(predict = predict_Oonirici_Vg_P7p_wt_mod_2,
                      var.a = Oonirici_Vg_P7p_wt_mod_2[["VCV"]][ , "LineB"],
                      var.p = rowSums(Oonirici_Vg_P7p_wt_mod_2[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Oonirici_Vg_P7p_wt_mod_2 <- data_Oonirici_Vg_P7p_wt_mod_2[["h2.obs"]]
      trait_mean_data_Oonirici_Vg_P7p_wt_mod_2 <- data_Oonirici_Vg_P7p_wt_mod_2[["mean.obs"]]
      va_data_Oonirici_Vg_P7p_wt_mod_2 <- data_Oonirici_Vg_P7p_wt_mod_2[["var.a.obs"]]
      vp_data_Oonirici_Vg_P7p_wt_mod_2 <- data_Oonirici_Vg_P7p_wt_mod_2[["var.obs"]]
      
      Evol_data_Oonirici_Vg_P7p_wt_mod_2 <- (va_data_Oonirici_Vg_P7p_wt_mod_2/2) / (trait_mean_data_Oonirici_Vg_P7p_wt_mod_2)^2
      
      mean(h2_data_Oonirici_Vg_P7p_wt_mod_2)
      mean(trait_mean_data_Oonirici_Vg_P7p_wt_mod_2)
      mean(va_data_Oonirici_Vg_P7p_wt_mod_2)
      mean(vp_data_Oonirici_Vg_P7p_wt_mod_2)
      mean(Evol_data_Oonirici_Vg_P7p_wt_mod_2)
      
    }
    
  }
  
  

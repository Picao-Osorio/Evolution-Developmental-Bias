#Vg_MCMCglmm_Caenorhabditis_SS_vulva

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
library("postMCMCglmm")

Vm_Vg_data<- as.data.frame(read_xlsx("Data S6 phenotyping for VG VM.xlsx"))
View(Vm_Vg_data)
Vm_Vg_data <- Vm_Vg_data[,-c(49,50)]
Vg_data <- subset(Vm_Vg_data, Treatment !="MA" )
Vg_Caenorhabditis_data <- subset(Vg_data, Genus == "Caenorhabditis" )
Vg_Caenorhabditis_data$LineB <- ifelse(Vg_Caenorhabditis_data$Treatment == "CONTROL" ,paste(Vg_Caenorhabditis_data$Line,Vg_Caenorhabditis_data$Block), paste(Vg_Caenorhabditis_data$Line))
Vg_Caenorhabditis_data$BlockRep <- paste(Vg_Caenorhabditis_data$Block,Vg_Caenorhabditis_data$Replicate)
Vg_Caenorhabditis_data$fTreatment <- as.factor(Vg_Caenorhabditis_data$Treatment)

View(Vg_Caenorhabditis_data)
table(Vg_Caenorhabditis_data$Species)
table(Vg_Caenorhabditis_data$Ancestral)
table(Vg_Caenorhabditis_data$Treatment)


Vg_Celegans_data <- subset(Vg_Caenorhabditis_data, Species =="C.elegans")
Vg_Cbriggsae_data <- subset(Vg_Caenorhabditis_data, Species =="C.briggsae")
table(Vg_Celegans_data$Observer)
table(Vg_Cbriggsae_data$Observer)


prior_bi_block <-
  list( R = list(V = 1, fix = 1),      # Fixing the "residual" variance to 1 because it is not identifiable in binary responses 
        G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1),
                 G2 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1)))





#Celegans----
{
  Vg_Celegans_bi_CONTROL <- subset(Vg_Celegans_data, Treatment =="CONTROL")
  Vg_Celegans_bi_WILD <- subset(Vg_Celegans_data, Treatment =="WILD")
  table(Vg_Celegans_bi_CONTROL$Observer)
  table(Vg_Celegans_bi_WILD$Observer)
  
  ## Celegans P3p ----
  
  
  #Celegans_CONTROL_P3p_SS_mod 
  {
    
    Celegans_CONTROL_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer + Ancestral -1,
                                           random      = ~ LineB + BlockRep,
                                           family      = "threshold",
                                           data        = Vg_Celegans_bi_CONTROL,
                                           prior       = prior_bi_block,
                                           nitt        = 1260000,       
                                           thin        = 500,           
                                           burnin      = 10000,
                                           trunc       = TRUE,
                                           pr          = TRUE,
                                           pl          = TRUE)            
    
    saveRDS(Celegans_CONTROL_P3p_SS_mod, file = "Celegans_CONTROL_P3p_SS_mod.rds")
    Celegans_CONTROL_P3p_SS_mod <- readRDS("Celegans_CONTROL_P3p_SS_mod.rds")
    
    summary(Celegans_CONTROL_P3p_SS_mod) 
    plotTrace(Celegans_CONTROL_P3p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Celegans_CONTROL_P3p_SS_mod.pdf")
    plot(Celegans_CONTROL_P3p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(Celegans_CONTROL_P3p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_CONTROL_P3p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Celegans_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_CONTROL_P3p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Celegans_CONTROL_P3p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Celegans_CONTROL_P3p_SS_mod[["VCV"]])
    #autocorr.plot(Celegans_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_CONTROL_P3p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Celegans_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_CONTROL_P3p_SS_mod <- Celegans_CONTROL_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_Celegans_CONTROL_P3p_SS_mod <- Celegans_CONTROL_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_Celegans_CONTROL_P3p_SS_mod <- rowSums(Celegans_CONTROL_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_Celegans_CONTROL_P3p_SS_mod) 
      HPDinterval(va_liab_Celegans_CONTROL_P3p_SS_mod) 
      
      mean(vlat_Celegans_CONTROL_P3p_SS_mod) 
      
      #variance of fixed effects
      X_Celegans_CONTROL_P3p_SS_mod <- Celegans_CONTROL_P3p_SS_mod[["X"]]
      beta_Celegans_CONTROL_P3p_SS_mod <- Celegans_CONTROL_P3p_SS_mod[["Sol"]][,c(1:5)]
      vf_Celegans_CONTROL_P3p_SS_mod   <- apply(beta_Celegans_CONTROL_P3p_SS_mod, 1, function(b) {var(as.vector(X_Celegans_CONTROL_P3p_SS_mod %*% b))}) 
      mean(vf_Celegans_CONTROL_P3p_SS_mod) 
      
      h2_liab_Celegans_CONTROL_P3p_SS_mod <- va_liab_Celegans_CONTROL_P3p_SS_mod / (vlat_Celegans_CONTROL_P3p_SS_mod + vf_Celegans_CONTROL_P3p_SS_mod)
      mean(h2_liab_Celegans_CONTROL_P3p_SS_mod) 
      posterior.mode(h2_liab_Celegans_CONTROL_P3p_SS_mod)	
      median(h2_liab_Celegans_CONTROL_P3p_SS_mod)		
      HPDinterval(h2_liab_Celegans_CONTROL_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Celegans_CONTROL_P3p_SS_mod <- ((rowMeans(Celegans_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Celegans_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) + Celegans_CONTROL_P3p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Celegans_CONTROL_P3p_SS_mod <- (va_liab_Celegans_CONTROL_P3p_SS_mod/2) / (trait_mean_liab_Celegans_CONTROL_P3p_SS_mod)^2
    mean(Evol_liab_Celegans_CONTROL_P3p_SS_mod)
   
    
    #Celegans_CONTROL_P3p_SS_mod data scale
    {
      
      predict_Celegans_CONTROL_P3p_SS_mod <- map(1:nrow(Celegans_CONTROL_P3p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Celegans_CONTROL_P3p_SS_mod %*% Celegans_CONTROL_P3p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Celegans_CONTROL_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_Celegans_CONTROL_P3p_SS_mod,
                      var.a = Celegans_CONTROL_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_CONTROL_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_CONTROL_P3p_SS_mod <- data_Celegans_CONTROL_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_Celegans_CONTROL_P3p_SS_mod <- data_Celegans_CONTROL_P3p_SS_mod[["mean.obs"]]
      va_data_Celegans_CONTROL_P3p_SS_mod <- data_Celegans_CONTROL_P3p_SS_mod[["var.a.obs"]]
      vp_data_Celegans_CONTROL_P3p_SS_mod <- data_Celegans_CONTROL_P3p_SS_mod[["var.obs"]]
      
      Evol_data_Celegans_CONTROL_P3p_SS_mod <- (va_data_Celegans_CONTROL_P3p_SS_mod/2) / (trait_mean_data_Celegans_CONTROL_P3p_SS_mod)^2
      
      mean(h2_data_Celegans_CONTROL_P3p_SS_mod) 
      mean(trait_mean_data_Celegans_CONTROL_P3p_SS_mod)
      mean(va_data_Celegans_CONTROL_P3p_SS_mod) 
      mean(vp_data_Celegans_CONTROL_P3p_SS_mod)
      mean(Evol_data_Celegans_CONTROL_P3p_SS_mod)
      
    }
    
  }
  
  #Celegans_WILD_P3p_SS_mod 
  {
    
    Celegans_WILD_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vg_Celegans_bi_WILD,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)         
    
    saveRDS(Celegans_WILD_P3p_SS_mod, file = "Celegans_WILD_P3p_SS_mod.rds")
    Celegans_WILD_P3p_SS_mod <- readRDS("Celegans_WILD_P3p_SS_mod.rds")
    
    summary(Celegans_WILD_P3p_SS_mod) 
    #plot(Celegans_WILD_P3p_SS_mod)
    
    # traces and posterior densities
    pdf("Celegans_WILD_P3p_SS_mod.pdf")
    plot(Celegans_WILD_P3p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(Celegans_WILD_P3p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_WILD_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(Celegans_WILD_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_WILD_P3p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(Celegans_WILD_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(Celegans_WILD_P3p_SS_mod[["VCV"]])
    #autocorr.plot(Celegans_WILD_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_WILD_P3p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(Celegans_WILD_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_WILD_P3p_SS_mod <- Celegans_WILD_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_Celegans_WILD_P3p_SS_mod <- Celegans_WILD_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_Celegans_WILD_P3p_SS_mod <- rowSums(Celegans_WILD_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_Celegans_WILD_P3p_SS_mod) 
      HPDinterval(va_liab_Celegans_WILD_P3p_SS_mod) 
      
      mean(vlat_Celegans_WILD_P3p_SS_mod) 
      
      #variance of fixed effects
      X_Celegans_WILD_P3p_SS_mod <- Celegans_WILD_P3p_SS_mod[["X"]]
      beta_Celegans_WILD_P3p_SS_mod <- Celegans_WILD_P3p_SS_mod[["Sol"]][,c(1:4)]
      vf_Celegans_WILD_P3p_SS_mod   <- apply(beta_Celegans_WILD_P3p_SS_mod, 1, function(b) {var(as.vector(X_Celegans_WILD_P3p_SS_mod %*% b))}) 
      mean(vf_Celegans_WILD_P3p_SS_mod) 
      
      h2_liab_Celegans_WILD_P3p_SS_mod <- va_liab_Celegans_WILD_P3p_SS_mod / (vlat_Celegans_WILD_P3p_SS_mod + vf_Celegans_WILD_P3p_SS_mod)
      mean(h2_liab_Celegans_WILD_P3p_SS_mod) 
      posterior.mode(h2_liab_Celegans_WILD_P3p_SS_mod)	
      median(h2_liab_Celegans_WILD_P3p_SS_mod)		
      HPDinterval(h2_liab_Celegans_WILD_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Celegans_WILD_P3p_SS_mod <- rowMeans(Celegans_WILD_P3p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_Celegans_WILD_P3p_SS_mod <- (va_liab_Celegans_WILD_P3p_SS_mod/2) / (trait_mean_liab_Celegans_WILD_P3p_SS_mod)^2
    mean(Evol_liab_Celegans_WILD_P3p_SS_mod)
    
    
    #Celegans_WILD_P3p_SS_mod data scale
    {
      
      predict_Celegans_WILD_P3p_SS_mod <- map(1:nrow(Celegans_WILD_P3p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_Celegans_WILD_P3p_SS_mod %*% Celegans_WILD_P3p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_Celegans_WILD_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_Celegans_WILD_P3p_SS_mod,
                      var.a = Celegans_WILD_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_WILD_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_WILD_P3p_SS_mod <- data_Celegans_WILD_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_Celegans_WILD_P3p_SS_mod <- data_Celegans_WILD_P3p_SS_mod[["mean.obs"]]
      va_data_Celegans_WILD_P3p_SS_mod <- data_Celegans_WILD_P3p_SS_mod[["var.a.obs"]]
      vp_data_Celegans_WILD_P3p_SS_mod <- data_Celegans_WILD_P3p_SS_mod[["var.obs"]]
      
      Evol_data_Celegans_WILD_P3p_SS_mod <- (va_data_Celegans_WILD_P3p_SS_mod/2) / (trait_mean_data_Celegans_WILD_P3p_SS_mod)^2
      
      mean(h2_data_Celegans_WILD_P3p_SS_mod)
      mean(trait_mean_data_Celegans_WILD_P3p_SS_mod)
      mean(va_data_Celegans_WILD_P3p_SS_mod)
      mean(vp_data_Celegans_WILD_P3p_SS_mod)
      mean(Evol_data_Celegans_WILD_P3p_SS_mod)
      
    }
    
  }
  
  #Celegans_Vg_P3p_SS_mod 
  {
    
    Celegans_Vg_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer + at.level(fTreatment,1):Ancestral + at.level(fTreatment,2):Ancestral -1,
                                      random      = ~ LineB + BlockRep,
                                      family      = "threshold",
                                      data        = Vg_Celegans_data,
                                      prior       = prior_bi_block,
                                      nitt        = 1260000,       
                                      thin        = 500,           
                                      burnin      = 10000,
                                      trunc       = TRUE,
                                      pr          = TRUE,
                                      pl          = TRUE)        
    
    
    
    
    
    saveRDS(Celegans_Vg_P3p_SS_mod, file = "Celegans_Vg_P3p_SS_mod.rds")
    Celegans_Vg_P3p_SS_mod <- readRDS("Celegans_Vg_P3p_SS_mod.rds")
    
    summary(Celegans_Vg_P3p_SS_mod) 
    plotTrace(Celegans_Vg_P3p_SS_mod$Sol)
    View(Celegans_Vg_P3p_SS_mod[["Sol"]])
    # traces and posterior densities
    pdf("Celegans_Vg_P3p_SS_mod.pdf")
    plot(Celegans_Vg_P3p_SS_mod[["Sol"]][,c(1:6)]) 
    plot(Celegans_Vg_P3p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_Vg_P3p_SS_mod[["Sol"]][,c(1:6)]) #
    heidel.diag(Celegans_Vg_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_Vg_P3p_SS_mod[["Sol"]][,c(1:6)])
    #autocorr.plot(Celegans_Vg_P3p_SS_mod[["Sol"]][,c(1:6)]) #
    autocorr.diag(Celegans_Vg_P3p_SS_mod[["VCV"]])
    #autocorr.plot(Celegans_Vg_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_Vg_P3p_SS_mod[["Sol"]][,c(1:6)]) # 
    effectiveSize(Celegans_Vg_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_Vg_P3p_SS_mod <- Celegans_Vg_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_Celegans_Vg_P3p_SS_mod <- Celegans_Vg_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_Celegans_Vg_P3p_SS_mod <- rowSums(Celegans_Vg_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_Celegans_Vg_P3p_SS_mod) 
      HPDinterval(va_liab_Celegans_Vg_P3p_SS_mod) 
      
      mean(vlat_Celegans_Vg_P3p_SS_mod) 
      
      #variance of fixed effects
      X_Celegans_Vg_P3p_SS_mod <- Celegans_Vg_P3p_SS_mod[["X"]]
      beta_Celegans_Vg_P3p_SS_mod <- Celegans_Vg_P3p_SS_mod[["Sol"]][,c(1:6)]
      vf_Celegans_Vg_P3p_SS_mod   <- apply(beta_Celegans_Vg_P3p_SS_mod, 1, function(b) {var(as.vector(X_Celegans_Vg_P3p_SS_mod %*% b))}) 
      mean(vf_Celegans_Vg_P3p_SS_mod) 
      
      h2_liab_Celegans_Vg_P3p_SS_mod <- va_liab_Celegans_Vg_P3p_SS_mod / (vlat_Celegans_Vg_P3p_SS_mod + vf_Celegans_Vg_P3p_SS_mod)
      mean(h2_liab_Celegans_Vg_P3p_SS_mod) 
      posterior.mode(h2_liab_Celegans_Vg_P3p_SS_mod)	
      median(h2_liab_Celegans_Vg_P3p_SS_mod)		
      HPDinterval(h2_liab_Celegans_Vg_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Celegans_Vg_P3p_SS_mod[["Sol"]][,c(1:4)])) #Cel WILD P3p_SS
    mean(rowMeans(Celegans_Vg_P3p_SS_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P3p_SS_mod[["Sol"]][,5]) #JU1200 CONTROL P3p_SS
    mean(rowMeans(Celegans_Vg_P3p_SS_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P3p_SS_mod[["Sol"]][,6]) #PB306 Control P3p_SS
    
    trait_mean_liab_Celegans_Vg_P3p_SS_mod <- ((rowMeans(Celegans_Vg_P3p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Celegans_Vg_P3p_SS_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P3p_SS_mod[["Sol"]][,5]) + (rowMeans(Celegans_Vg_P3p_SS_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P3p_SS_mod[["Sol"]][,6]) )/3)
    
    Evol_liab_Celegans_Vg_P3p_SS_mod <- (va_liab_Celegans_Vg_P3p_SS_mod/2) / (trait_mean_liab_Celegans_Vg_P3p_SS_mod)^2
    mean(Evol_liab_Celegans_Vg_P3p_SS_mod)
    
    
    #Celegans_Vg_P3p_SS_mod data scale
    {
      
      predict_Celegans_Vg_P3p_SS_mod <- map(1:nrow(Celegans_Vg_P3p_SS_mod[["Sol"]][,c(1:6)]), ~ as.vector(X_Celegans_Vg_P3p_SS_mod %*% Celegans_Vg_P3p_SS_mod[["Sol"]][,c(1:6)][., ]))
      
      data_Celegans_Vg_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_Celegans_Vg_P3p_SS_mod,
                      var.a = Celegans_Vg_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_Vg_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_Vg_P3p_SS_mod <- data_Celegans_Vg_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_Celegans_Vg_P3p_SS_mod <- data_Celegans_Vg_P3p_SS_mod[["mean.obs"]]
      va_data_Celegans_Vg_P3p_SS_mod <- data_Celegans_Vg_P3p_SS_mod[["var.a.obs"]]
      vp_data_Celegans_Vg_P3p_SS_mod <- data_Celegans_Vg_P3p_SS_mod[["var.obs"]]
      
      Evol_data_Celegans_Vg_P3p_SS_mod <- (va_data_Celegans_Vg_P3p_SS_mod/2) / (trait_mean_data_Celegans_Vg_P3p_SS_mod)^2
      
      mean(h2_data_Celegans_Vg_P3p_SS_mod)
      mean(trait_mean_data_Celegans_Vg_P3p_SS_mod)
      mean(va_data_Celegans_Vg_P3p_SS_mod)
      mean(vp_data_Celegans_Vg_P3p_SS_mod)
      mean(Evol_data_Celegans_Vg_P3p_SS_mod)
      
    }
    
  }
  
 
  
  ## Celegans P4p ----
  
  
  #Celegans_CONTROL_P4p_SS_mod 
  {
    
    Celegans_CONTROL_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer + Ancestral -1,
                                           random      = ~ LineB + BlockRep,
                                           family      = "threshold",
                                           data        = Vg_Celegans_bi_CONTROL,
                                           prior       = prior_bi_block,
                                           nitt        = 1260000,       
                                           thin        = 500,           
                                           burnin      = 10000,
                                           trunc       = TRUE,
                                           pr          = TRUE,
                                           pl          = TRUE)            
    
    saveRDS(Celegans_CONTROL_P4p_SS_mod, file = "Celegans_CONTROL_P4p_SS_mod.rds")
    Celegans_CONTROL_P4p_SS_mod <- readRDS("Celegans_CONTROL_P4p_SS_mod.rds")
    
    summary(Celegans_CONTROL_P4p_SS_mod) 
    plotTrace(Celegans_CONTROL_P4p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Celegans_CONTROL_P4p_SS_mod.pdf")
    plot(Celegans_CONTROL_P4p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(Celegans_CONTROL_P4p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_CONTROL_P4p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Celegans_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_CONTROL_P4p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Celegans_CONTROL_P4p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Celegans_CONTROL_P4p_SS_mod[["VCV"]])
    #autocorr.plot(Celegans_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_CONTROL_P4p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Celegans_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_CONTROL_P4p_SS_mod <- Celegans_CONTROL_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_Celegans_CONTROL_P4p_SS_mod <- Celegans_CONTROL_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_Celegans_CONTROL_P4p_SS_mod <- rowSums(Celegans_CONTROL_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_Celegans_CONTROL_P4p_SS_mod) 
      HPDinterval(va_liab_Celegans_CONTROL_P4p_SS_mod) 
      
      mean(vlat_Celegans_CONTROL_P4p_SS_mod) 
      
      #variance of fixed effects
      X_Celegans_CONTROL_P4p_SS_mod <- Celegans_CONTROL_P4p_SS_mod[["X"]]
      beta_Celegans_CONTROL_P4p_SS_mod <- Celegans_CONTROL_P4p_SS_mod[["Sol"]][,c(1:5)]
      vf_Celegans_CONTROL_P4p_SS_mod   <- apply(beta_Celegans_CONTROL_P4p_SS_mod, 1, function(b) {var(as.vector(X_Celegans_CONTROL_P4p_SS_mod %*% b))}) 
      mean(vf_Celegans_CONTROL_P4p_SS_mod) 
      
      h2_liab_Celegans_CONTROL_P4p_SS_mod <- va_liab_Celegans_CONTROL_P4p_SS_mod / (vlat_Celegans_CONTROL_P4p_SS_mod + vf_Celegans_CONTROL_P4p_SS_mod)
      mean(h2_liab_Celegans_CONTROL_P4p_SS_mod) 
      posterior.mode(h2_liab_Celegans_CONTROL_P4p_SS_mod)	
      median(h2_liab_Celegans_CONTROL_P4p_SS_mod)		
      HPDinterval(h2_liab_Celegans_CONTROL_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Celegans_CONTROL_P4p_SS_mod <- ((rowMeans(Celegans_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Celegans_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) + Celegans_CONTROL_P4p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Celegans_CONTROL_P4p_SS_mod <- (va_liab_Celegans_CONTROL_P4p_SS_mod/2) / (trait_mean_liab_Celegans_CONTROL_P4p_SS_mod)^2
    mean(Evol_liab_Celegans_CONTROL_P4p_SS_mod)
    
    
    #Celegans_CONTROL_P4p_SS_mod data scale
    {
      
      predict_Celegans_CONTROL_P4p_SS_mod <- map(1:nrow(Celegans_CONTROL_P4p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Celegans_CONTROL_P4p_SS_mod %*% Celegans_CONTROL_P4p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Celegans_CONTROL_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_Celegans_CONTROL_P4p_SS_mod,
                      var.a = Celegans_CONTROL_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_CONTROL_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_CONTROL_P4p_SS_mod <- data_Celegans_CONTROL_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_Celegans_CONTROL_P4p_SS_mod <- data_Celegans_CONTROL_P4p_SS_mod[["mean.obs"]]
      va_data_Celegans_CONTROL_P4p_SS_mod <- data_Celegans_CONTROL_P4p_SS_mod[["var.a.obs"]]
      vp_data_Celegans_CONTROL_P4p_SS_mod <- data_Celegans_CONTROL_P4p_SS_mod[["var.obs"]]
      
      Evol_data_Celegans_CONTROL_P4p_SS_mod <- (va_data_Celegans_CONTROL_P4p_SS_mod/2) / (trait_mean_data_Celegans_CONTROL_P4p_SS_mod)^2
      
      mean(h2_data_Celegans_CONTROL_P4p_SS_mod) 
      mean(trait_mean_data_Celegans_CONTROL_P4p_SS_mod)
      mean(va_data_Celegans_CONTROL_P4p_SS_mod) 
      mean(vp_data_Celegans_CONTROL_P4p_SS_mod)
      mean(Evol_data_Celegans_CONTROL_P4p_SS_mod)
      
    }
    
  }
  
  #Celegans_WILD_P4p_SS_mod 
  {
    
    Celegans_WILD_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vg_Celegans_bi_WILD,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)         
    
    saveRDS(Celegans_WILD_P4p_SS_mod, file = "Celegans_WILD_P4p_SS_mod.rds")
    Celegans_WILD_P4p_SS_mod <- readRDS("Celegans_WILD_P4p_SS_mod.rds")
    
    summary(Celegans_WILD_P4p_SS_mod) 
    #plot(Celegans_WILD_P4p_SS_mod)
    
    # traces and posterior densities
    pdf("Celegans_WILD_P4p_SS_mod.pdf")
    plot(Celegans_WILD_P4p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(Celegans_WILD_P4p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_WILD_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(Celegans_WILD_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_WILD_P4p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(Celegans_WILD_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(Celegans_WILD_P4p_SS_mod[["VCV"]])
    #autocorr.plot(Celegans_WILD_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_WILD_P4p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(Celegans_WILD_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_WILD_P4p_SS_mod <- Celegans_WILD_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_Celegans_WILD_P4p_SS_mod <- Celegans_WILD_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_Celegans_WILD_P4p_SS_mod <- rowSums(Celegans_WILD_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_Celegans_WILD_P4p_SS_mod) 
      HPDinterval(va_liab_Celegans_WILD_P4p_SS_mod) 
      
      mean(vlat_Celegans_WILD_P4p_SS_mod) 
      
      #variance of fixed effects
      X_Celegans_WILD_P4p_SS_mod <- Celegans_WILD_P4p_SS_mod[["X"]]
      beta_Celegans_WILD_P4p_SS_mod <- Celegans_WILD_P4p_SS_mod[["Sol"]][,c(1:4)]
      vf_Celegans_WILD_P4p_SS_mod   <- apply(beta_Celegans_WILD_P4p_SS_mod, 1, function(b) {var(as.vector(X_Celegans_WILD_P4p_SS_mod %*% b))}) 
      mean(vf_Celegans_WILD_P4p_SS_mod) 
      
      h2_liab_Celegans_WILD_P4p_SS_mod <- va_liab_Celegans_WILD_P4p_SS_mod / (vlat_Celegans_WILD_P4p_SS_mod + vf_Celegans_WILD_P4p_SS_mod)
      mean(h2_liab_Celegans_WILD_P4p_SS_mod) 
      posterior.mode(h2_liab_Celegans_WILD_P4p_SS_mod)	
      median(h2_liab_Celegans_WILD_P4p_SS_mod)		
      HPDinterval(h2_liab_Celegans_WILD_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Celegans_WILD_P4p_SS_mod <- rowMeans(Celegans_WILD_P4p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_Celegans_WILD_P4p_SS_mod <- (va_liab_Celegans_WILD_P4p_SS_mod/2) / (trait_mean_liab_Celegans_WILD_P4p_SS_mod)^2
    mean(Evol_liab_Celegans_WILD_P4p_SS_mod)
    
    
    #Celegans_WILD_P4p_SS_mod data scale
    {
      
      predict_Celegans_WILD_P4p_SS_mod <- map(1:nrow(Celegans_WILD_P4p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_Celegans_WILD_P4p_SS_mod %*% Celegans_WILD_P4p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_Celegans_WILD_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_Celegans_WILD_P4p_SS_mod,
                      var.a = Celegans_WILD_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_WILD_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_WILD_P4p_SS_mod <- data_Celegans_WILD_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_Celegans_WILD_P4p_SS_mod <- data_Celegans_WILD_P4p_SS_mod[["mean.obs"]]
      va_data_Celegans_WILD_P4p_SS_mod <- data_Celegans_WILD_P4p_SS_mod[["var.a.obs"]]
      vp_data_Celegans_WILD_P4p_SS_mod <- data_Celegans_WILD_P4p_SS_mod[["var.obs"]]
      
      Evol_data_Celegans_WILD_P4p_SS_mod <- (va_data_Celegans_WILD_P4p_SS_mod/2) / (trait_mean_data_Celegans_WILD_P4p_SS_mod)^2
      
      mean(h2_data_Celegans_WILD_P4p_SS_mod)
      mean(trait_mean_data_Celegans_WILD_P4p_SS_mod)
      mean(va_data_Celegans_WILD_P4p_SS_mod)
      mean(vp_data_Celegans_WILD_P4p_SS_mod)
      mean(Evol_data_Celegans_WILD_P4p_SS_mod)
      
    }
    
  }
  
  #Celegans_Vg_P4p_SS_mod 
  {
    
    Celegans_Vg_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer + at.level(fTreatment,1):Ancestral + at.level(fTreatment,2):Ancestral -1,
                                      random      = ~ LineB + BlockRep,
                                      family      = "threshold",
                                      data        = Vg_Celegans_data,
                                      prior       = prior_bi_block,
                                      nitt        = 1260000,       
                                      thin        = 500,           
                                      burnin      = 10000,
                                      trunc       = TRUE,
                                      pr          = TRUE,
                                      pl          = TRUE)        
    
    
    saveRDS(Celegans_Vg_P4p_SS_mod, file = "Celegans_Vg_P4p_SS_mod.rds")
    Celegans_Vg_P4p_SS_mod <- readRDS("Celegans_Vg_P4p_SS_mod.rds")
    
    summary(Celegans_Vg_P4p_SS_mod) 
    plotTrace(Celegans_Vg_P4p_SS_mod$Sol)
    View(Celegans_Vg_P4p_SS_mod[["Sol"]])
    # traces and posterior densities
    pdf("Celegans_Vg_P4p_SS_mod.pdf")
    plot(Celegans_Vg_P4p_SS_mod[["Sol"]][,c(1:6)]) 
    plot(Celegans_Vg_P4p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_Vg_P4p_SS_mod[["Sol"]][,c(1:6)]) #
    heidel.diag(Celegans_Vg_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_Vg_P4p_SS_mod[["Sol"]][,c(1:6)])
    #autocorr.plot(Celegans_Vg_P4p_SS_mod[["Sol"]][,c(1:6)]) #
    autocorr.diag(Celegans_Vg_P4p_SS_mod[["VCV"]])
    #autocorr.plot(Celegans_Vg_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_Vg_P4p_SS_mod[["Sol"]][,c(1:6)]) # 
    effectiveSize(Celegans_Vg_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_Vg_P4p_SS_mod <- Celegans_Vg_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_Celegans_Vg_P4p_SS_mod <- Celegans_Vg_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_Celegans_Vg_P4p_SS_mod <- rowSums(Celegans_Vg_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_Celegans_Vg_P4p_SS_mod) 
      HPDinterval(va_liab_Celegans_Vg_P4p_SS_mod) 
      
      mean(vlat_Celegans_Vg_P4p_SS_mod) 
      
      #variance of fixed effects
      X_Celegans_Vg_P4p_SS_mod <- Celegans_Vg_P4p_SS_mod[["X"]]
      beta_Celegans_Vg_P4p_SS_mod <- Celegans_Vg_P4p_SS_mod[["Sol"]][,c(1:6)]
      vf_Celegans_Vg_P4p_SS_mod   <- apply(beta_Celegans_Vg_P4p_SS_mod, 1, function(b) {var(as.vector(X_Celegans_Vg_P4p_SS_mod %*% b))}) 
      mean(vf_Celegans_Vg_P4p_SS_mod) 
      
      h2_liab_Celegans_Vg_P4p_SS_mod <- va_liab_Celegans_Vg_P4p_SS_mod / (vlat_Celegans_Vg_P4p_SS_mod + vf_Celegans_Vg_P4p_SS_mod)
      mean(h2_liab_Celegans_Vg_P4p_SS_mod) 
      posterior.mode(h2_liab_Celegans_Vg_P4p_SS_mod)	
      median(h2_liab_Celegans_Vg_P4p_SS_mod)		
      HPDinterval(h2_liab_Celegans_Vg_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Celegans_Vg_P4p_SS_mod[["Sol"]][,c(1:4)])) #Cel WILD P4p_SS
    mean(rowMeans(Celegans_Vg_P4p_SS_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P4p_SS_mod[["Sol"]][,5]) #JU1200 CONTROL P4p_SS
    mean(rowMeans(Celegans_Vg_P4p_SS_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P4p_SS_mod[["Sol"]][,6]) #PB306 Control P4p_SS
    
    trait_mean_liab_Celegans_Vg_P4p_SS_mod <- ((rowMeans(Celegans_Vg_P4p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Celegans_Vg_P4p_SS_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P4p_SS_mod[["Sol"]][,5]) + (rowMeans(Celegans_Vg_P4p_SS_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P4p_SS_mod[["Sol"]][,6]) )/3)
    
    Evol_liab_Celegans_Vg_P4p_SS_mod <- (va_liab_Celegans_Vg_P4p_SS_mod/2) / (trait_mean_liab_Celegans_Vg_P4p_SS_mod)^2
    mean(Evol_liab_Celegans_Vg_P4p_SS_mod)
    
    
    #Celegans_Vg_P4p_SS_mod data scale
    {
      
      predict_Celegans_Vg_P4p_SS_mod <- map(1:nrow(Celegans_Vg_P4p_SS_mod[["Sol"]][,c(1:6)]), ~ as.vector(X_Celegans_Vg_P4p_SS_mod %*% Celegans_Vg_P4p_SS_mod[["Sol"]][,c(1:6)][., ]))
      
      data_Celegans_Vg_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_Celegans_Vg_P4p_SS_mod,
                      var.a = Celegans_Vg_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_Vg_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_Vg_P4p_SS_mod <- data_Celegans_Vg_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_Celegans_Vg_P4p_SS_mod <- data_Celegans_Vg_P4p_SS_mod[["mean.obs"]]
      va_data_Celegans_Vg_P4p_SS_mod <- data_Celegans_Vg_P4p_SS_mod[["var.a.obs"]]
      vp_data_Celegans_Vg_P4p_SS_mod <- data_Celegans_Vg_P4p_SS_mod[["var.obs"]]
      
      Evol_data_Celegans_Vg_P4p_SS_mod <- (va_data_Celegans_Vg_P4p_SS_mod/2) / (trait_mean_data_Celegans_Vg_P4p_SS_mod)^2
      
      mean(h2_data_Celegans_Vg_P4p_SS_mod)
      mean(trait_mean_data_Celegans_Vg_P4p_SS_mod)
      mean(va_data_Celegans_Vg_P4p_SS_mod)
      mean(vp_data_Celegans_Vg_P4p_SS_mod)
      mean(Evol_data_Celegans_Vg_P4p_SS_mod)
      
    }
    
  }
  
  
  
  ## Celegans P8p ----
  
  
  #Celegans_CONTROL_P8p_SS_mod 
  {
    
    Celegans_CONTROL_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer + Ancestral -1,
                                           random      = ~ LineB + BlockRep,
                                           family      = "threshold",
                                           data        = Vg_Celegans_bi_CONTROL,
                                           prior       = prior_bi_block,
                                           nitt        = 1260000,       
                                           thin        = 500,           
                                           burnin      = 10000,
                                           trunc       = TRUE,
                                           pr          = TRUE,
                                           pl          = TRUE)            
    
    saveRDS(Celegans_CONTROL_P8p_SS_mod, file = "Celegans_CONTROL_P8p_SS_mod.rds")
    Celegans_CONTROL_P8p_SS_mod <- readRDS("Celegans_CONTROL_P8p_SS_mod.rds")
    
    summary(Celegans_CONTROL_P8p_SS_mod) 
    plotTrace(Celegans_CONTROL_P8p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Celegans_CONTROL_P8p_SS_mod.pdf")
    plot(Celegans_CONTROL_P8p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(Celegans_CONTROL_P8p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_CONTROL_P8p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Celegans_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_CONTROL_P8p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Celegans_CONTROL_P8p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Celegans_CONTROL_P8p_SS_mod[["VCV"]])
    #autocorr.plot(Celegans_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_CONTROL_P8p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Celegans_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_CONTROL_P8p_SS_mod <- Celegans_CONTROL_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_Celegans_CONTROL_P8p_SS_mod <- Celegans_CONTROL_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_Celegans_CONTROL_P8p_SS_mod <- rowSums(Celegans_CONTROL_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_Celegans_CONTROL_P8p_SS_mod) 
      HPDinterval(va_liab_Celegans_CONTROL_P8p_SS_mod) 
      
      mean(vlat_Celegans_CONTROL_P8p_SS_mod) 
      
      #variance of fixed effects
      X_Celegans_CONTROL_P8p_SS_mod <- Celegans_CONTROL_P8p_SS_mod[["X"]]
      beta_Celegans_CONTROL_P8p_SS_mod <- Celegans_CONTROL_P8p_SS_mod[["Sol"]][,c(1:5)]
      vf_Celegans_CONTROL_P8p_SS_mod   <- apply(beta_Celegans_CONTROL_P8p_SS_mod, 1, function(b) {var(as.vector(X_Celegans_CONTROL_P8p_SS_mod %*% b))}) 
      mean(vf_Celegans_CONTROL_P8p_SS_mod) 
      
      h2_liab_Celegans_CONTROL_P8p_SS_mod <- va_liab_Celegans_CONTROL_P8p_SS_mod / (vlat_Celegans_CONTROL_P8p_SS_mod + vf_Celegans_CONTROL_P8p_SS_mod)
      mean(h2_liab_Celegans_CONTROL_P8p_SS_mod) 
      posterior.mode(h2_liab_Celegans_CONTROL_P8p_SS_mod)	
      median(h2_liab_Celegans_CONTROL_P8p_SS_mod)		
      HPDinterval(h2_liab_Celegans_CONTROL_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Celegans_CONTROL_P8p_SS_mod <- ((rowMeans(Celegans_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Celegans_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) + Celegans_CONTROL_P8p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Celegans_CONTROL_P8p_SS_mod <- (va_liab_Celegans_CONTROL_P8p_SS_mod/2) / (trait_mean_liab_Celegans_CONTROL_P8p_SS_mod)^2
    mean(Evol_liab_Celegans_CONTROL_P8p_SS_mod)
    
    
    #Celegans_CONTROL_P8p_SS_mod data scale
    {
      
      predict_Celegans_CONTROL_P8p_SS_mod <- map(1:nrow(Celegans_CONTROL_P8p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Celegans_CONTROL_P8p_SS_mod %*% Celegans_CONTROL_P8p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Celegans_CONTROL_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_Celegans_CONTROL_P8p_SS_mod,
                      var.a = Celegans_CONTROL_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_CONTROL_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_CONTROL_P8p_SS_mod <- data_Celegans_CONTROL_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_Celegans_CONTROL_P8p_SS_mod <- data_Celegans_CONTROL_P8p_SS_mod[["mean.obs"]]
      va_data_Celegans_CONTROL_P8p_SS_mod <- data_Celegans_CONTROL_P8p_SS_mod[["var.a.obs"]]
      vp_data_Celegans_CONTROL_P8p_SS_mod <- data_Celegans_CONTROL_P8p_SS_mod[["var.obs"]]
      
      Evol_data_Celegans_CONTROL_P8p_SS_mod <- (va_data_Celegans_CONTROL_P8p_SS_mod/2) / (trait_mean_data_Celegans_CONTROL_P8p_SS_mod)^2
      
      mean(h2_data_Celegans_CONTROL_P8p_SS_mod) 
      mean(trait_mean_data_Celegans_CONTROL_P8p_SS_mod)
      mean(va_data_Celegans_CONTROL_P8p_SS_mod) 
      mean(vp_data_Celegans_CONTROL_P8p_SS_mod)
      mean(Evol_data_Celegans_CONTROL_P8p_SS_mod)
      
    }
    
  }
  
  #Celegans_WILD_P8p_SS_mod 
  {
    
    Celegans_WILD_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vg_Celegans_bi_WILD,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)         
    
    saveRDS(Celegans_WILD_P8p_SS_mod, file = "Celegans_WILD_P8p_SS_mod.rds")
    Celegans_WILD_P8p_SS_mod <- readRDS("Celegans_WILD_P8p_SS_mod.rds")
    
    summary(Celegans_WILD_P8p_SS_mod) 
    #plot(Celegans_WILD_P8p_SS_mod)
    
    # traces and posterior densities
    pdf("Celegans_WILD_P8p_SS_mod.pdf")
    plot(Celegans_WILD_P8p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(Celegans_WILD_P8p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_WILD_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(Celegans_WILD_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_WILD_P8p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(Celegans_WILD_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(Celegans_WILD_P8p_SS_mod[["VCV"]])
    #autocorr.plot(Celegans_WILD_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_WILD_P8p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(Celegans_WILD_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_WILD_P8p_SS_mod <- Celegans_WILD_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_Celegans_WILD_P8p_SS_mod <- Celegans_WILD_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_Celegans_WILD_P8p_SS_mod <- rowSums(Celegans_WILD_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_Celegans_WILD_P8p_SS_mod) 
      HPDinterval(va_liab_Celegans_WILD_P8p_SS_mod) 
      
      mean(vlat_Celegans_WILD_P8p_SS_mod) 
      
      #variance of fixed effects
      X_Celegans_WILD_P8p_SS_mod <- Celegans_WILD_P8p_SS_mod[["X"]]
      beta_Celegans_WILD_P8p_SS_mod <- Celegans_WILD_P8p_SS_mod[["Sol"]][,c(1:4)]
      vf_Celegans_WILD_P8p_SS_mod   <- apply(beta_Celegans_WILD_P8p_SS_mod, 1, function(b) {var(as.vector(X_Celegans_WILD_P8p_SS_mod %*% b))}) 
      mean(vf_Celegans_WILD_P8p_SS_mod) 
      
      h2_liab_Celegans_WILD_P8p_SS_mod <- va_liab_Celegans_WILD_P8p_SS_mod / (vlat_Celegans_WILD_P8p_SS_mod + vf_Celegans_WILD_P8p_SS_mod)
      mean(h2_liab_Celegans_WILD_P8p_SS_mod) 
      posterior.mode(h2_liab_Celegans_WILD_P8p_SS_mod)	
      median(h2_liab_Celegans_WILD_P8p_SS_mod)		
      HPDinterval(h2_liab_Celegans_WILD_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Celegans_WILD_P8p_SS_mod <- rowMeans(Celegans_WILD_P8p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_Celegans_WILD_P8p_SS_mod <- (va_liab_Celegans_WILD_P8p_SS_mod/2) / (trait_mean_liab_Celegans_WILD_P8p_SS_mod)^2
    mean(Evol_liab_Celegans_WILD_P8p_SS_mod)
    
    
    #Celegans_WILD_P8p_SS_mod data scale
    {
      
      predict_Celegans_WILD_P8p_SS_mod <- map(1:nrow(Celegans_WILD_P8p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_Celegans_WILD_P8p_SS_mod %*% Celegans_WILD_P8p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_Celegans_WILD_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_Celegans_WILD_P8p_SS_mod,
                      var.a = Celegans_WILD_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_WILD_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_WILD_P8p_SS_mod <- data_Celegans_WILD_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_Celegans_WILD_P8p_SS_mod <- data_Celegans_WILD_P8p_SS_mod[["mean.obs"]]
      va_data_Celegans_WILD_P8p_SS_mod <- data_Celegans_WILD_P8p_SS_mod[["var.a.obs"]]
      vp_data_Celegans_WILD_P8p_SS_mod <- data_Celegans_WILD_P8p_SS_mod[["var.obs"]]
      
      Evol_data_Celegans_WILD_P8p_SS_mod <- (va_data_Celegans_WILD_P8p_SS_mod/2) / (trait_mean_data_Celegans_WILD_P8p_SS_mod)^2
      
      mean(h2_data_Celegans_WILD_P8p_SS_mod)
      mean(trait_mean_data_Celegans_WILD_P8p_SS_mod)
      mean(va_data_Celegans_WILD_P8p_SS_mod)
      mean(vp_data_Celegans_WILD_P8p_SS_mod)
      mean(Evol_data_Celegans_WILD_P8p_SS_mod)
      
    }
    
  }
  
  #Celegans_Vg_P8p_SS_mod 
  {
    
    Celegans_Vg_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer + at.level(fTreatment,1):Ancestral + at.level(fTreatment,2):Ancestral -1,
                                      random      = ~ LineB + BlockRep,
                                      family      = "threshold",
                                      data        = Vg_Celegans_data,
                                      prior       = prior_bi_block,
                                      nitt        = 1260000,       
                                      thin        = 500,           
                                      burnin      = 10000,
                                      trunc       = TRUE,
                                      pr          = TRUE,
                                      pl          = TRUE)        
    
    
    saveRDS(Celegans_Vg_P8p_SS_mod, file = "Celegans_Vg_P8p_SS_mod.rds")
    Celegans_Vg_P8p_SS_mod <- readRDS("Celegans_Vg_P8p_SS_mod.rds")
    
    summary(Celegans_Vg_P8p_SS_mod) 
    plotTrace(Celegans_Vg_P8p_SS_mod$Sol)
    View(Celegans_Vg_P8p_SS_mod[["Sol"]])
    # traces and posterior densities
    pdf("Celegans_Vg_P8p_SS_mod.pdf")
    plot(Celegans_Vg_P8p_SS_mod[["Sol"]][,c(1:6)]) 
    plot(Celegans_Vg_P8p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_Vg_P8p_SS_mod[["Sol"]][,c(1:6)]) #
    heidel.diag(Celegans_Vg_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_Vg_P8p_SS_mod[["Sol"]][,c(1:6)])
    #autocorr.plot(Celegans_Vg_P8p_SS_mod[["Sol"]][,c(1:6)]) #
    autocorr.diag(Celegans_Vg_P8p_SS_mod[["VCV"]])
    #autocorr.plot(Celegans_Vg_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_Vg_P8p_SS_mod[["Sol"]][,c(1:6)]) # 
    effectiveSize(Celegans_Vg_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_Vg_P8p_SS_mod <- Celegans_Vg_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_Celegans_Vg_P8p_SS_mod <- Celegans_Vg_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_Celegans_Vg_P8p_SS_mod <- rowSums(Celegans_Vg_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_Celegans_Vg_P8p_SS_mod) 
      HPDinterval(va_liab_Celegans_Vg_P8p_SS_mod) 
      
      mean(vlat_Celegans_Vg_P8p_SS_mod) 
      
      #variance of fixed effects
      X_Celegans_Vg_P8p_SS_mod <- Celegans_Vg_P8p_SS_mod[["X"]]
      beta_Celegans_Vg_P8p_SS_mod <- Celegans_Vg_P8p_SS_mod[["Sol"]][,c(1:6)]
      vf_Celegans_Vg_P8p_SS_mod   <- apply(beta_Celegans_Vg_P8p_SS_mod, 1, function(b) {var(as.vector(X_Celegans_Vg_P8p_SS_mod %*% b))}) 
      mean(vf_Celegans_Vg_P8p_SS_mod) 
      
      h2_liab_Celegans_Vg_P8p_SS_mod <- va_liab_Celegans_Vg_P8p_SS_mod / (vlat_Celegans_Vg_P8p_SS_mod + vf_Celegans_Vg_P8p_SS_mod)
      mean(h2_liab_Celegans_Vg_P8p_SS_mod) 
      posterior.mode(h2_liab_Celegans_Vg_P8p_SS_mod)	
      median(h2_liab_Celegans_Vg_P8p_SS_mod)		
      HPDinterval(h2_liab_Celegans_Vg_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Celegans_Vg_P8p_SS_mod[["Sol"]][,c(1:4)])) #Cel WILD P8p_SS
    mean(rowMeans(Celegans_Vg_P8p_SS_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P8p_SS_mod[["Sol"]][,5]) #JU1200 CONTROL P8p_SS
    mean(rowMeans(Celegans_Vg_P8p_SS_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P8p_SS_mod[["Sol"]][,6]) #PB306 Control P8p_SS
    
    trait_mean_liab_Celegans_Vg_P8p_SS_mod <- ((rowMeans(Celegans_Vg_P8p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Celegans_Vg_P8p_SS_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P8p_SS_mod[["Sol"]][,5]) + (rowMeans(Celegans_Vg_P8p_SS_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P8p_SS_mod[["Sol"]][,6]) )/3)
    
    Evol_liab_Celegans_Vg_P8p_SS_mod <- (va_liab_Celegans_Vg_P8p_SS_mod/2) / (trait_mean_liab_Celegans_Vg_P8p_SS_mod)^2
    mean(Evol_liab_Celegans_Vg_P8p_SS_mod)
    
    
    #Celegans_Vg_P8p_SS_mod data scale
    {
      
      predict_Celegans_Vg_P8p_SS_mod <- map(1:nrow(Celegans_Vg_P8p_SS_mod[["Sol"]][,c(1:6)]), ~ as.vector(X_Celegans_Vg_P8p_SS_mod %*% Celegans_Vg_P8p_SS_mod[["Sol"]][,c(1:6)][., ]))
      
      data_Celegans_Vg_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_Celegans_Vg_P8p_SS_mod,
                      var.a = Celegans_Vg_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_Vg_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_Vg_P8p_SS_mod <- data_Celegans_Vg_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_Celegans_Vg_P8p_SS_mod <- data_Celegans_Vg_P8p_SS_mod[["mean.obs"]]
      va_data_Celegans_Vg_P8p_SS_mod <- data_Celegans_Vg_P8p_SS_mod[["var.a.obs"]]
      vp_data_Celegans_Vg_P8p_SS_mod <- data_Celegans_Vg_P8p_SS_mod[["var.obs"]]
      
      Evol_data_Celegans_Vg_P8p_SS_mod <- (va_data_Celegans_Vg_P8p_SS_mod/2) / (trait_mean_data_Celegans_Vg_P8p_SS_mod)^2
      
      mean(h2_data_Celegans_Vg_P8p_SS_mod)
      mean(trait_mean_data_Celegans_Vg_P8p_SS_mod)
      mean(va_data_Celegans_Vg_P8p_SS_mod)
      mean(vp_data_Celegans_Vg_P8p_SS_mod)
      mean(Evol_data_Celegans_Vg_P8p_SS_mod)
      
    }
    
  }
  
  ## Celegans P5p ----
  
  
  #Celegans_CONTROL_P5p_wt_mod 
  {
    
    Celegans_CONTROL_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Ancestral -1,
                                            random      = ~ LineB + BlockRep,
                                            family      = "threshold",
                                            data        = Vg_Celegans_bi_CONTROL,
                                            prior       = prior_bi_block,
                                            nitt        = 1260000,       
                                            thin        = 500,           
                                            burnin      = 10000,
                                            trunc       = TRUE,
                                            pr          = TRUE,
                                            pl          = TRUE)            
    
    saveRDS(Celegans_CONTROL_P5p_wt_mod, file = "Celegans_CONTROL_P5p_wt_mod.rds")
    Celegans_CONTROL_P5p_wt_mod <- readRDS("Celegans_CONTROL_P5p_wt_mod.rds")
    
    summary(Celegans_CONTROL_P5p_wt_mod) 
    plotTrace(Celegans_CONTROL_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Celegans_CONTROL_P5p_wt_mod.pdf")
    plot(Celegans_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Celegans_CONTROL_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Celegans_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Celegans_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Celegans_CONTROL_P5p_wt_mod[["VCV"]])
    #autocorr.plot(Celegans_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Celegans_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_CONTROL_P5p_wt_mod <- Celegans_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_Celegans_CONTROL_P5p_wt_mod <- Celegans_CONTROL_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_Celegans_CONTROL_P5p_wt_mod <- rowSums(Celegans_CONTROL_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_Celegans_CONTROL_P5p_wt_mod) 
      HPDinterval(va_liab_Celegans_CONTROL_P5p_wt_mod) 
      
      mean(vlat_Celegans_CONTROL_P5p_wt_mod) 
      
      #variance of fixed effects
      X_Celegans_CONTROL_P5p_wt_mod <- Celegans_CONTROL_P5p_wt_mod[["X"]]
      beta_Celegans_CONTROL_P5p_wt_mod <- Celegans_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]
      vf_Celegans_CONTROL_P5p_wt_mod   <- apply(beta_Celegans_CONTROL_P5p_wt_mod, 1, function(b) {var(as.vector(X_Celegans_CONTROL_P5p_wt_mod %*% b))}) 
      mean(vf_Celegans_CONTROL_P5p_wt_mod) 
      
      h2_liab_Celegans_CONTROL_P5p_wt_mod <- va_liab_Celegans_CONTROL_P5p_wt_mod / (vlat_Celegans_CONTROL_P5p_wt_mod + vf_Celegans_CONTROL_P5p_wt_mod)
      mean(h2_liab_Celegans_CONTROL_P5p_wt_mod) 
      posterior.mode(h2_liab_Celegans_CONTROL_P5p_wt_mod)	
      median(h2_liab_Celegans_CONTROL_P5p_wt_mod)		
      HPDinterval(h2_liab_Celegans_CONTROL_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Celegans_CONTROL_P5p_wt_mod <- ((rowMeans(Celegans_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Celegans_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) + Celegans_CONTROL_P5p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Celegans_CONTROL_P5p_wt_mod <- (va_liab_Celegans_CONTROL_P5p_wt_mod/2) / (trait_mean_liab_Celegans_CONTROL_P5p_wt_mod)^2
    mean(Evol_liab_Celegans_CONTROL_P5p_wt_mod)
    
    
    #Celegans_CONTROL_P5p_wt_mod data scale
    {
      
      predict_Celegans_CONTROL_P5p_wt_mod <- map(1:nrow(Celegans_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Celegans_CONTROL_P5p_wt_mod %*% Celegans_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Celegans_CONTROL_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_Celegans_CONTROL_P5p_wt_mod,
                      var.a = Celegans_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_CONTROL_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_CONTROL_P5p_wt_mod <- data_Celegans_CONTROL_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_Celegans_CONTROL_P5p_wt_mod <- data_Celegans_CONTROL_P5p_wt_mod[["mean.obs"]]
      va_data_Celegans_CONTROL_P5p_wt_mod <- data_Celegans_CONTROL_P5p_wt_mod[["var.a.obs"]]
      vp_data_Celegans_CONTROL_P5p_wt_mod <- data_Celegans_CONTROL_P5p_wt_mod[["var.obs"]]
      
      Evol_data_Celegans_CONTROL_P5p_wt_mod <- (va_data_Celegans_CONTROL_P5p_wt_mod/2) / (trait_mean_data_Celegans_CONTROL_P5p_wt_mod)^2
      
      mean(h2_data_Celegans_CONTROL_P5p_wt_mod) 
      mean(trait_mean_data_Celegans_CONTROL_P5p_wt_mod)
      mean(va_data_Celegans_CONTROL_P5p_wt_mod) 
      mean(vp_data_Celegans_CONTROL_P5p_wt_mod)
      mean(Evol_data_Celegans_CONTROL_P5p_wt_mod)
      
    }
    
  }
  
  #Celegans_WILD_P5p_wt_mod 
  {
    
    Celegans_WILD_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vg_Celegans_bi_WILD,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)         
    
    saveRDS(Celegans_WILD_P5p_wt_mod, file = "Celegans_WILD_P5p_wt_mod.rds")
    Celegans_WILD_P5p_wt_mod <- readRDS("Celegans_WILD_P5p_wt_mod.rds")
    
    summary(Celegans_WILD_P5p_wt_mod) 
    #plot(Celegans_WILD_P5p_wt_mod)
    
    # traces and posterior densities
    pdf("Celegans_WILD_P5p_wt_mod.pdf")
    plot(Celegans_WILD_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(Celegans_WILD_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_WILD_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(Celegans_WILD_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_WILD_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(Celegans_WILD_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(Celegans_WILD_P5p_wt_mod[["VCV"]])
    #autocorr.plot(Celegans_WILD_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_WILD_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(Celegans_WILD_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_WILD_P5p_wt_mod <- Celegans_WILD_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_Celegans_WILD_P5p_wt_mod <- Celegans_WILD_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_Celegans_WILD_P5p_wt_mod <- rowSums(Celegans_WILD_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_Celegans_WILD_P5p_wt_mod) 
      HPDinterval(va_liab_Celegans_WILD_P5p_wt_mod) 
      
      mean(vlat_Celegans_WILD_P5p_wt_mod) 
      
      #variance of fixed effects
      X_Celegans_WILD_P5p_wt_mod <- Celegans_WILD_P5p_wt_mod[["X"]]
      beta_Celegans_WILD_P5p_wt_mod <- Celegans_WILD_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_Celegans_WILD_P5p_wt_mod   <- apply(beta_Celegans_WILD_P5p_wt_mod, 1, function(b) {var(as.vector(X_Celegans_WILD_P5p_wt_mod %*% b))}) 
      mean(vf_Celegans_WILD_P5p_wt_mod) 
      
      h2_liab_Celegans_WILD_P5p_wt_mod <- va_liab_Celegans_WILD_P5p_wt_mod / (vlat_Celegans_WILD_P5p_wt_mod + vf_Celegans_WILD_P5p_wt_mod)
      mean(h2_liab_Celegans_WILD_P5p_wt_mod) 
      posterior.mode(h2_liab_Celegans_WILD_P5p_wt_mod)	
      median(h2_liab_Celegans_WILD_P5p_wt_mod)		
      HPDinterval(h2_liab_Celegans_WILD_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Celegans_WILD_P5p_wt_mod <- rowMeans(Celegans_WILD_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_Celegans_WILD_P5p_wt_mod <- (va_liab_Celegans_WILD_P5p_wt_mod/2) / (trait_mean_liab_Celegans_WILD_P5p_wt_mod)^2
    mean(Evol_liab_Celegans_WILD_P5p_wt_mod)
    
    
    #Celegans_WILD_P5p_wt_mod data scale
    {
      
      predict_Celegans_WILD_P5p_wt_mod <- map(1:nrow(Celegans_WILD_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_Celegans_WILD_P5p_wt_mod %*% Celegans_WILD_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_Celegans_WILD_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_Celegans_WILD_P5p_wt_mod,
                      var.a = Celegans_WILD_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_WILD_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_WILD_P5p_wt_mod <- data_Celegans_WILD_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_Celegans_WILD_P5p_wt_mod <- data_Celegans_WILD_P5p_wt_mod[["mean.obs"]]
      va_data_Celegans_WILD_P5p_wt_mod <- data_Celegans_WILD_P5p_wt_mod[["var.a.obs"]]
      vp_data_Celegans_WILD_P5p_wt_mod <- data_Celegans_WILD_P5p_wt_mod[["var.obs"]]
      
      Evol_data_Celegans_WILD_P5p_wt_mod <- (va_data_Celegans_WILD_P5p_wt_mod/2) / (trait_mean_data_Celegans_WILD_P5p_wt_mod)^2
      
      mean(h2_data_Celegans_WILD_P5p_wt_mod)
      mean(trait_mean_data_Celegans_WILD_P5p_wt_mod)
      mean(va_data_Celegans_WILD_P5p_wt_mod)
      mean(vp_data_Celegans_WILD_P5p_wt_mod)
      mean(Evol_data_Celegans_WILD_P5p_wt_mod)
      
    }
    
  }
  
  #Celegans_Vg_P5p_wt_mod 
  {
    
    Celegans_Vg_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + at.level(fTreatment,1):Ancestral + at.level(fTreatment,2):Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vg_Celegans_data,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)        
    
    
    saveRDS(Celegans_Vg_P5p_wt_mod, file = "Celegans_Vg_P5p_wt_mod.rds")
    Celegans_Vg_P5p_wt_mod <- readRDS("Celegans_Vg_P5p_wt_mod.rds")
    
    summary(Celegans_Vg_P5p_wt_mod) 
    plotTrace(Celegans_Vg_P5p_wt_mod$Sol)
    View(Celegans_Vg_P5p_wt_mod[["Sol"]])
    # traces and posterior densities
    pdf("Celegans_Vg_P5p_wt_mod.pdf")
    plot(Celegans_Vg_P5p_wt_mod[["Sol"]][,c(1:6)]) 
    plot(Celegans_Vg_P5p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_Vg_P5p_wt_mod[["Sol"]][,c(1:6)]) #
    heidel.diag(Celegans_Vg_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_Vg_P5p_wt_mod[["Sol"]][,c(1:6)])
    #autocorr.plot(Celegans_Vg_P5p_wt_mod[["Sol"]][,c(1:6)]) #
    autocorr.diag(Celegans_Vg_P5p_wt_mod[["VCV"]])
    #autocorr.plot(Celegans_Vg_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_Vg_P5p_wt_mod[["Sol"]][,c(1:6)]) # 
    effectiveSize(Celegans_Vg_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_Vg_P5p_wt_mod <- Celegans_Vg_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_Celegans_Vg_P5p_wt_mod <- Celegans_Vg_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_Celegans_Vg_P5p_wt_mod <- rowSums(Celegans_Vg_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_Celegans_Vg_P5p_wt_mod) 
      HPDinterval(va_liab_Celegans_Vg_P5p_wt_mod) 
      
      mean(vlat_Celegans_Vg_P5p_wt_mod) 
      
      #variance of fixed effects
      X_Celegans_Vg_P5p_wt_mod <- Celegans_Vg_P5p_wt_mod[["X"]]
      beta_Celegans_Vg_P5p_wt_mod <- Celegans_Vg_P5p_wt_mod[["Sol"]][,c(1:6)]
      vf_Celegans_Vg_P5p_wt_mod   <- apply(beta_Celegans_Vg_P5p_wt_mod, 1, function(b) {var(as.vector(X_Celegans_Vg_P5p_wt_mod %*% b))}) 
      mean(vf_Celegans_Vg_P5p_wt_mod) 
      
      h2_liab_Celegans_Vg_P5p_wt_mod <- va_liab_Celegans_Vg_P5p_wt_mod / (vlat_Celegans_Vg_P5p_wt_mod + vf_Celegans_Vg_P5p_wt_mod)
      mean(h2_liab_Celegans_Vg_P5p_wt_mod) 
      posterior.mode(h2_liab_Celegans_Vg_P5p_wt_mod)	
      median(h2_liab_Celegans_Vg_P5p_wt_mod)		
      HPDinterval(h2_liab_Celegans_Vg_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Celegans_Vg_P5p_wt_mod[["Sol"]][,c(1:4)])) #Cel WILD P5p_wt
    mean(rowMeans(Celegans_Vg_P5p_wt_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P5p_wt_mod[["Sol"]][,5]) #JU1200 CONTROL P5p_wt
    mean(rowMeans(Celegans_Vg_P5p_wt_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P5p_wt_mod[["Sol"]][,6]) #PB306 Control P5p_wt
    
    trait_mean_liab_Celegans_Vg_P5p_wt_mod <- ((rowMeans(Celegans_Vg_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Celegans_Vg_P5p_wt_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P5p_wt_mod[["Sol"]][,5]) + (rowMeans(Celegans_Vg_P5p_wt_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P5p_wt_mod[["Sol"]][,6]) )/3)
    
    Evol_liab_Celegans_Vg_P5p_wt_mod <- (va_liab_Celegans_Vg_P5p_wt_mod/2) / (trait_mean_liab_Celegans_Vg_P5p_wt_mod)^2
    mean(Evol_liab_Celegans_Vg_P5p_wt_mod)
    
    
    #Celegans_Vg_P5p_wt_mod data scale
    {
      
      predict_Celegans_Vg_P5p_wt_mod <- map(1:nrow(Celegans_Vg_P5p_wt_mod[["Sol"]][,c(1:6)]), ~ as.vector(X_Celegans_Vg_P5p_wt_mod %*% Celegans_Vg_P5p_wt_mod[["Sol"]][,c(1:6)][., ]))
      
      data_Celegans_Vg_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_Celegans_Vg_P5p_wt_mod,
                      var.a = Celegans_Vg_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_Vg_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_Vg_P5p_wt_mod <- data_Celegans_Vg_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_Celegans_Vg_P5p_wt_mod <- data_Celegans_Vg_P5p_wt_mod[["mean.obs"]]
      va_data_Celegans_Vg_P5p_wt_mod <- data_Celegans_Vg_P5p_wt_mod[["var.a.obs"]]
      vp_data_Celegans_Vg_P5p_wt_mod <- data_Celegans_Vg_P5p_wt_mod[["var.obs"]]
      
      Evol_data_Celegans_Vg_P5p_wt_mod <- (va_data_Celegans_Vg_P5p_wt_mod/2) / (trait_mean_data_Celegans_Vg_P5p_wt_mod)^2
      
      mean(h2_data_Celegans_Vg_P5p_wt_mod)
      mean(trait_mean_data_Celegans_Vg_P5p_wt_mod)
      mean(va_data_Celegans_Vg_P5p_wt_mod)
      mean(vp_data_Celegans_Vg_P5p_wt_mod)
      mean(Evol_data_Celegans_Vg_P5p_wt_mod)
      
    }
    
  }
  
  
  
  ## Celegans P6p ----
  
  
  #Celegans_CONTROL_P6p_wt_mod 
  {
    
    Celegans_CONTROL_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Ancestral -1,
                                            random      = ~ LineB + BlockRep,
                                            family      = "threshold",
                                            data        = Vg_Celegans_bi_CONTROL,
                                            prior       = prior_bi_block,
                                            nitt        = 1260000,       
                                            thin        = 500,           
                                            burnin      = 10000,
                                            trunc       = TRUE,
                                            pr          = TRUE,
                                            pl          = TRUE)            
    
    saveRDS(Celegans_CONTROL_P6p_wt_mod, file = "Celegans_CONTROL_P6p_wt_mod.rds")
    Celegans_CONTROL_P6p_wt_mod <- readRDS("Celegans_CONTROL_P6p_wt_mod.rds")
    
    summary(Celegans_CONTROL_P6p_wt_mod) 
    plotTrace(Celegans_CONTROL_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Celegans_CONTROL_P6p_wt_mod.pdf")
    plot(Celegans_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Celegans_CONTROL_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Celegans_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Celegans_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Celegans_CONTROL_P6p_wt_mod[["VCV"]])
    #autocorr.plot(Celegans_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Celegans_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_CONTROL_P6p_wt_mod <- Celegans_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_Celegans_CONTROL_P6p_wt_mod <- Celegans_CONTROL_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_Celegans_CONTROL_P6p_wt_mod <- rowSums(Celegans_CONTROL_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_Celegans_CONTROL_P6p_wt_mod) 
      HPDinterval(va_liab_Celegans_CONTROL_P6p_wt_mod) 
      
      mean(vlat_Celegans_CONTROL_P6p_wt_mod) 
      
      #variance of fixed effects
      X_Celegans_CONTROL_P6p_wt_mod <- Celegans_CONTROL_P6p_wt_mod[["X"]]
      beta_Celegans_CONTROL_P6p_wt_mod <- Celegans_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]
      vf_Celegans_CONTROL_P6p_wt_mod   <- apply(beta_Celegans_CONTROL_P6p_wt_mod, 1, function(b) {var(as.vector(X_Celegans_CONTROL_P6p_wt_mod %*% b))}) 
      mean(vf_Celegans_CONTROL_P6p_wt_mod) 
      
      h2_liab_Celegans_CONTROL_P6p_wt_mod <- va_liab_Celegans_CONTROL_P6p_wt_mod / (vlat_Celegans_CONTROL_P6p_wt_mod + vf_Celegans_CONTROL_P6p_wt_mod)
      mean(h2_liab_Celegans_CONTROL_P6p_wt_mod) 
      posterior.mode(h2_liab_Celegans_CONTROL_P6p_wt_mod)	
      median(h2_liab_Celegans_CONTROL_P6p_wt_mod)		
      HPDinterval(h2_liab_Celegans_CONTROL_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Celegans_CONTROL_P6p_wt_mod <- ((rowMeans(Celegans_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Celegans_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) + Celegans_CONTROL_P6p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Celegans_CONTROL_P6p_wt_mod <- (va_liab_Celegans_CONTROL_P6p_wt_mod/2) / (trait_mean_liab_Celegans_CONTROL_P6p_wt_mod)^2
    mean(Evol_liab_Celegans_CONTROL_P6p_wt_mod)
    
    
    #Celegans_CONTROL_P6p_wt_mod data scale
    {
      
      predict_Celegans_CONTROL_P6p_wt_mod <- map(1:nrow(Celegans_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Celegans_CONTROL_P6p_wt_mod %*% Celegans_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Celegans_CONTROL_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_Celegans_CONTROL_P6p_wt_mod,
                      var.a = Celegans_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_CONTROL_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_CONTROL_P6p_wt_mod <- data_Celegans_CONTROL_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_Celegans_CONTROL_P6p_wt_mod <- data_Celegans_CONTROL_P6p_wt_mod[["mean.obs"]]
      va_data_Celegans_CONTROL_P6p_wt_mod <- data_Celegans_CONTROL_P6p_wt_mod[["var.a.obs"]]
      vp_data_Celegans_CONTROL_P6p_wt_mod <- data_Celegans_CONTROL_P6p_wt_mod[["var.obs"]]
      
      Evol_data_Celegans_CONTROL_P6p_wt_mod <- (va_data_Celegans_CONTROL_P6p_wt_mod/2) / (trait_mean_data_Celegans_CONTROL_P6p_wt_mod)^2
      
      mean(h2_data_Celegans_CONTROL_P6p_wt_mod) 
      mean(trait_mean_data_Celegans_CONTROL_P6p_wt_mod)
      mean(va_data_Celegans_CONTROL_P6p_wt_mod) 
      mean(vp_data_Celegans_CONTROL_P6p_wt_mod)
      mean(Evol_data_Celegans_CONTROL_P6p_wt_mod)
      
    }
    
  }
  
  #Celegans_WILD_P6p_wt_mod 
  {
    
    Celegans_WILD_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vg_Celegans_bi_WILD,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)         
    
    saveRDS(Celegans_WILD_P6p_wt_mod, file = "Celegans_WILD_P6p_wt_mod.rds")
    Celegans_WILD_P6p_wt_mod <- readRDS("Celegans_WILD_P6p_wt_mod.rds")
    
    summary(Celegans_WILD_P6p_wt_mod) 
    #plot(Celegans_WILD_P6p_wt_mod)
    
    # traces and posterior densities
    pdf("Celegans_WILD_P6p_wt_mod.pdf")
    plot(Celegans_WILD_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(Celegans_WILD_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_WILD_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(Celegans_WILD_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_WILD_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(Celegans_WILD_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(Celegans_WILD_P6p_wt_mod[["VCV"]])
    #autocorr.plot(Celegans_WILD_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_WILD_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(Celegans_WILD_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_WILD_P6p_wt_mod <- Celegans_WILD_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_Celegans_WILD_P6p_wt_mod <- Celegans_WILD_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_Celegans_WILD_P6p_wt_mod <- rowSums(Celegans_WILD_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_Celegans_WILD_P6p_wt_mod) 
      HPDinterval(va_liab_Celegans_WILD_P6p_wt_mod) 
      
      mean(vlat_Celegans_WILD_P6p_wt_mod) 
      
      #variance of fixed effects
      X_Celegans_WILD_P6p_wt_mod <- Celegans_WILD_P6p_wt_mod[["X"]]
      beta_Celegans_WILD_P6p_wt_mod <- Celegans_WILD_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_Celegans_WILD_P6p_wt_mod   <- apply(beta_Celegans_WILD_P6p_wt_mod, 1, function(b) {var(as.vector(X_Celegans_WILD_P6p_wt_mod %*% b))}) 
      mean(vf_Celegans_WILD_P6p_wt_mod) 
      
      h2_liab_Celegans_WILD_P6p_wt_mod <- va_liab_Celegans_WILD_P6p_wt_mod / (vlat_Celegans_WILD_P6p_wt_mod + vf_Celegans_WILD_P6p_wt_mod)
      mean(h2_liab_Celegans_WILD_P6p_wt_mod) 
      posterior.mode(h2_liab_Celegans_WILD_P6p_wt_mod)	
      median(h2_liab_Celegans_WILD_P6p_wt_mod)		
      HPDinterval(h2_liab_Celegans_WILD_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Celegans_WILD_P6p_wt_mod <- rowMeans(Celegans_WILD_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_Celegans_WILD_P6p_wt_mod <- (va_liab_Celegans_WILD_P6p_wt_mod/2) / (trait_mean_liab_Celegans_WILD_P6p_wt_mod)^2
    mean(Evol_liab_Celegans_WILD_P6p_wt_mod)
    
    
    #Celegans_WILD_P6p_wt_mod data scale
    {
      
      predict_Celegans_WILD_P6p_wt_mod <- map(1:nrow(Celegans_WILD_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_Celegans_WILD_P6p_wt_mod %*% Celegans_WILD_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_Celegans_WILD_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_Celegans_WILD_P6p_wt_mod,
                      var.a = Celegans_WILD_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_WILD_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_WILD_P6p_wt_mod <- data_Celegans_WILD_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_Celegans_WILD_P6p_wt_mod <- data_Celegans_WILD_P6p_wt_mod[["mean.obs"]]
      va_data_Celegans_WILD_P6p_wt_mod <- data_Celegans_WILD_P6p_wt_mod[["var.a.obs"]]
      vp_data_Celegans_WILD_P6p_wt_mod <- data_Celegans_WILD_P6p_wt_mod[["var.obs"]]
      
      Evol_data_Celegans_WILD_P6p_wt_mod <- (va_data_Celegans_WILD_P6p_wt_mod/2) / (trait_mean_data_Celegans_WILD_P6p_wt_mod)^2
      
      mean(h2_data_Celegans_WILD_P6p_wt_mod)
      mean(trait_mean_data_Celegans_WILD_P6p_wt_mod)
      mean(va_data_Celegans_WILD_P6p_wt_mod)
      mean(vp_data_Celegans_WILD_P6p_wt_mod)
      mean(Evol_data_Celegans_WILD_P6p_wt_mod)
      
    }
    
  }
  
  #Celegans_Vg_P6p_wt_mod 
  {
    
    Celegans_Vg_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + at.level(fTreatment,1):Ancestral + at.level(fTreatment,2):Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vg_Celegans_data,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)        
    
    
    saveRDS(Celegans_Vg_P6p_wt_mod, file = "Celegans_Vg_P6p_wt_mod.rds")
    Celegans_Vg_P6p_wt_mod <- readRDS("Celegans_Vg_P6p_wt_mod.rds")
    
    summary(Celegans_Vg_P6p_wt_mod) 
    plotTrace(Celegans_Vg_P6p_wt_mod$Sol)
    View(Celegans_Vg_P6p_wt_mod[["Sol"]])
    # traces and posterior densities
    pdf("Celegans_Vg_P6p_wt_mod.pdf")
    plot(Celegans_Vg_P6p_wt_mod[["Sol"]][,c(1:6)]) 
    plot(Celegans_Vg_P6p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_Vg_P6p_wt_mod[["Sol"]][,c(1:6)]) #
    heidel.diag(Celegans_Vg_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_Vg_P6p_wt_mod[["Sol"]][,c(1:6)])
    #autocorr.plot(Celegans_Vg_P6p_wt_mod[["Sol"]][,c(1:6)]) #
    autocorr.diag(Celegans_Vg_P6p_wt_mod[["VCV"]])
    #autocorr.plot(Celegans_Vg_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_Vg_P6p_wt_mod[["Sol"]][,c(1:6)]) # 
    effectiveSize(Celegans_Vg_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_Vg_P6p_wt_mod <- Celegans_Vg_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_Celegans_Vg_P6p_wt_mod <- Celegans_Vg_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_Celegans_Vg_P6p_wt_mod <- rowSums(Celegans_Vg_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_Celegans_Vg_P6p_wt_mod) 
      HPDinterval(va_liab_Celegans_Vg_P6p_wt_mod) 
      
      mean(vlat_Celegans_Vg_P6p_wt_mod) 
      
      #variance of fixed effects
      X_Celegans_Vg_P6p_wt_mod <- Celegans_Vg_P6p_wt_mod[["X"]]
      beta_Celegans_Vg_P6p_wt_mod <- Celegans_Vg_P6p_wt_mod[["Sol"]][,c(1:6)]
      vf_Celegans_Vg_P6p_wt_mod   <- apply(beta_Celegans_Vg_P6p_wt_mod, 1, function(b) {var(as.vector(X_Celegans_Vg_P6p_wt_mod %*% b))}) 
      mean(vf_Celegans_Vg_P6p_wt_mod) 
      
      h2_liab_Celegans_Vg_P6p_wt_mod <- va_liab_Celegans_Vg_P6p_wt_mod / (vlat_Celegans_Vg_P6p_wt_mod + vf_Celegans_Vg_P6p_wt_mod)
      mean(h2_liab_Celegans_Vg_P6p_wt_mod) 
      posterior.mode(h2_liab_Celegans_Vg_P6p_wt_mod)	
      median(h2_liab_Celegans_Vg_P6p_wt_mod)		
      HPDinterval(h2_liab_Celegans_Vg_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Celegans_Vg_P6p_wt_mod[["Sol"]][,c(1:4)])) #Cel WILD P6p_wt
    mean(rowMeans(Celegans_Vg_P6p_wt_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P6p_wt_mod[["Sol"]][,5]) #JU1200 CONTROL P6p_wt
    mean(rowMeans(Celegans_Vg_P6p_wt_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P6p_wt_mod[["Sol"]][,6]) #PB306 Control P6p_wt
    
    trait_mean_liab_Celegans_Vg_P6p_wt_mod <- ((rowMeans(Celegans_Vg_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Celegans_Vg_P6p_wt_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P6p_wt_mod[["Sol"]][,5]) + (rowMeans(Celegans_Vg_P6p_wt_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P6p_wt_mod[["Sol"]][,6]) )/3)
    
    Evol_liab_Celegans_Vg_P6p_wt_mod <- (va_liab_Celegans_Vg_P6p_wt_mod/2) / (trait_mean_liab_Celegans_Vg_P6p_wt_mod)^2
    mean(Evol_liab_Celegans_Vg_P6p_wt_mod)
    
    
    #Celegans_Vg_P6p_wt_mod data scale
    {
      
      predict_Celegans_Vg_P6p_wt_mod <- map(1:nrow(Celegans_Vg_P6p_wt_mod[["Sol"]][,c(1:6)]), ~ as.vector(X_Celegans_Vg_P6p_wt_mod %*% Celegans_Vg_P6p_wt_mod[["Sol"]][,c(1:6)][., ]))
      
      data_Celegans_Vg_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_Celegans_Vg_P6p_wt_mod,
                      var.a = Celegans_Vg_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_Vg_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_Vg_P6p_wt_mod <- data_Celegans_Vg_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_Celegans_Vg_P6p_wt_mod <- data_Celegans_Vg_P6p_wt_mod[["mean.obs"]]
      va_data_Celegans_Vg_P6p_wt_mod <- data_Celegans_Vg_P6p_wt_mod[["var.a.obs"]]
      vp_data_Celegans_Vg_P6p_wt_mod <- data_Celegans_Vg_P6p_wt_mod[["var.obs"]]
      
      Evol_data_Celegans_Vg_P6p_wt_mod <- (va_data_Celegans_Vg_P6p_wt_mod/2) / (trait_mean_data_Celegans_Vg_P6p_wt_mod)^2
      
      mean(h2_data_Celegans_Vg_P6p_wt_mod)
      mean(trait_mean_data_Celegans_Vg_P6p_wt_mod)
      mean(va_data_Celegans_Vg_P6p_wt_mod)
      mean(vp_data_Celegans_Vg_P6p_wt_mod)
      mean(Evol_data_Celegans_Vg_P6p_wt_mod)
      
    }
    
  }
  
  
  
  ## Celegans P7p ----
  
  
  #Celegans_CONTROL_P7p_wt_mod 
  {
    
    Celegans_CONTROL_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Ancestral -1,
                                            random      = ~ LineB + BlockRep,
                                            family      = "threshold",
                                            data        = Vg_Celegans_bi_CONTROL,
                                            prior       = prior_bi_block,
                                            nitt        = 1260000,       
                                            thin        = 500,           
                                            burnin      = 10000,
                                            trunc       = TRUE,
                                            pr          = TRUE,
                                            pl          = TRUE)            
    
    saveRDS(Celegans_CONTROL_P7p_wt_mod, file = "Celegans_CONTROL_P7p_wt_mod.rds")
    Celegans_CONTROL_P7p_wt_mod <- readRDS("Celegans_CONTROL_P7p_wt_mod.rds")
    
    summary(Celegans_CONTROL_P7p_wt_mod) 
    plotTrace(Celegans_CONTROL_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Celegans_CONTROL_P7p_wt_mod.pdf")
    plot(Celegans_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Celegans_CONTROL_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Celegans_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Celegans_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Celegans_CONTROL_P7p_wt_mod[["VCV"]])
    #autocorr.plot(Celegans_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Celegans_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_CONTROL_P7p_wt_mod <- Celegans_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_Celegans_CONTROL_P7p_wt_mod <- Celegans_CONTROL_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_Celegans_CONTROL_P7p_wt_mod <- rowSums(Celegans_CONTROL_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_Celegans_CONTROL_P7p_wt_mod) 
      HPDinterval(va_liab_Celegans_CONTROL_P7p_wt_mod) 
      
      mean(vlat_Celegans_CONTROL_P7p_wt_mod) 
      
      #variance of fixed effects
      X_Celegans_CONTROL_P7p_wt_mod <- Celegans_CONTROL_P7p_wt_mod[["X"]]
      beta_Celegans_CONTROL_P7p_wt_mod <- Celegans_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]
      vf_Celegans_CONTROL_P7p_wt_mod   <- apply(beta_Celegans_CONTROL_P7p_wt_mod, 1, function(b) {var(as.vector(X_Celegans_CONTROL_P7p_wt_mod %*% b))}) 
      mean(vf_Celegans_CONTROL_P7p_wt_mod) 
      
      h2_liab_Celegans_CONTROL_P7p_wt_mod <- va_liab_Celegans_CONTROL_P7p_wt_mod / (vlat_Celegans_CONTROL_P7p_wt_mod + vf_Celegans_CONTROL_P7p_wt_mod)
      mean(h2_liab_Celegans_CONTROL_P7p_wt_mod) 
      posterior.mode(h2_liab_Celegans_CONTROL_P7p_wt_mod)	
      median(h2_liab_Celegans_CONTROL_P7p_wt_mod)		
      HPDinterval(h2_liab_Celegans_CONTROL_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Celegans_CONTROL_P7p_wt_mod <- ((rowMeans(Celegans_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Celegans_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) + Celegans_CONTROL_P7p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Celegans_CONTROL_P7p_wt_mod <- (va_liab_Celegans_CONTROL_P7p_wt_mod/2) / (trait_mean_liab_Celegans_CONTROL_P7p_wt_mod)^2
    mean(Evol_liab_Celegans_CONTROL_P7p_wt_mod)
    
    
    #Celegans_CONTROL_P7p_wt_mod data scale
    {
      
      predict_Celegans_CONTROL_P7p_wt_mod <- map(1:nrow(Celegans_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Celegans_CONTROL_P7p_wt_mod %*% Celegans_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Celegans_CONTROL_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_Celegans_CONTROL_P7p_wt_mod,
                      var.a = Celegans_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_CONTROL_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_CONTROL_P7p_wt_mod <- data_Celegans_CONTROL_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_Celegans_CONTROL_P7p_wt_mod <- data_Celegans_CONTROL_P7p_wt_mod[["mean.obs"]]
      va_data_Celegans_CONTROL_P7p_wt_mod <- data_Celegans_CONTROL_P7p_wt_mod[["var.a.obs"]]
      vp_data_Celegans_CONTROL_P7p_wt_mod <- data_Celegans_CONTROL_P7p_wt_mod[["var.obs"]]
      
      Evol_data_Celegans_CONTROL_P7p_wt_mod <- (va_data_Celegans_CONTROL_P7p_wt_mod/2) / (trait_mean_data_Celegans_CONTROL_P7p_wt_mod)^2
      
      mean(h2_data_Celegans_CONTROL_P7p_wt_mod) 
      mean(trait_mean_data_Celegans_CONTROL_P7p_wt_mod)
      mean(va_data_Celegans_CONTROL_P7p_wt_mod) 
      mean(vp_data_Celegans_CONTROL_P7p_wt_mod)
      mean(Evol_data_Celegans_CONTROL_P7p_wt_mod)
      
    }
    
  }
  
  #Celegans_WILD_P7p_wt_mod 
  {
    
    Celegans_WILD_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vg_Celegans_bi_WILD,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)         
    
    saveRDS(Celegans_WILD_P7p_wt_mod, file = "Celegans_WILD_P7p_wt_mod.rds")
    Celegans_WILD_P7p_wt_mod <- readRDS("Celegans_WILD_P7p_wt_mod.rds")
    
    summary(Celegans_WILD_P7p_wt_mod) 
    #plot(Celegans_WILD_P7p_wt_mod)
    
    # traces and posterior densities
    pdf("Celegans_WILD_P7p_wt_mod.pdf")
    plot(Celegans_WILD_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(Celegans_WILD_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_WILD_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(Celegans_WILD_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_WILD_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(Celegans_WILD_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(Celegans_WILD_P7p_wt_mod[["VCV"]])
    #autocorr.plot(Celegans_WILD_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_WILD_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(Celegans_WILD_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_WILD_P7p_wt_mod <- Celegans_WILD_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_Celegans_WILD_P7p_wt_mod <- Celegans_WILD_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_Celegans_WILD_P7p_wt_mod <- rowSums(Celegans_WILD_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_Celegans_WILD_P7p_wt_mod) 
      HPDinterval(va_liab_Celegans_WILD_P7p_wt_mod) 
      
      mean(vlat_Celegans_WILD_P7p_wt_mod) 
      
      #variance of fixed effects
      X_Celegans_WILD_P7p_wt_mod <- Celegans_WILD_P7p_wt_mod[["X"]]
      beta_Celegans_WILD_P7p_wt_mod <- Celegans_WILD_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_Celegans_WILD_P7p_wt_mod   <- apply(beta_Celegans_WILD_P7p_wt_mod, 1, function(b) {var(as.vector(X_Celegans_WILD_P7p_wt_mod %*% b))}) 
      mean(vf_Celegans_WILD_P7p_wt_mod) 
      
      h2_liab_Celegans_WILD_P7p_wt_mod <- va_liab_Celegans_WILD_P7p_wt_mod / (vlat_Celegans_WILD_P7p_wt_mod + vf_Celegans_WILD_P7p_wt_mod)
      mean(h2_liab_Celegans_WILD_P7p_wt_mod) 
      posterior.mode(h2_liab_Celegans_WILD_P7p_wt_mod)	
      median(h2_liab_Celegans_WILD_P7p_wt_mod)		
      HPDinterval(h2_liab_Celegans_WILD_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Celegans_WILD_P7p_wt_mod <- rowMeans(Celegans_WILD_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_Celegans_WILD_P7p_wt_mod <- (va_liab_Celegans_WILD_P7p_wt_mod/2) / (trait_mean_liab_Celegans_WILD_P7p_wt_mod)^2
    mean(Evol_liab_Celegans_WILD_P7p_wt_mod)
    
    
    #Celegans_WILD_P7p_wt_mod data scale
    {
      
      predict_Celegans_WILD_P7p_wt_mod <- map(1:nrow(Celegans_WILD_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_Celegans_WILD_P7p_wt_mod %*% Celegans_WILD_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_Celegans_WILD_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_Celegans_WILD_P7p_wt_mod,
                      var.a = Celegans_WILD_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_WILD_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_WILD_P7p_wt_mod <- data_Celegans_WILD_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_Celegans_WILD_P7p_wt_mod <- data_Celegans_WILD_P7p_wt_mod[["mean.obs"]]
      va_data_Celegans_WILD_P7p_wt_mod <- data_Celegans_WILD_P7p_wt_mod[["var.a.obs"]]
      vp_data_Celegans_WILD_P7p_wt_mod <- data_Celegans_WILD_P7p_wt_mod[["var.obs"]]
      
      Evol_data_Celegans_WILD_P7p_wt_mod <- (va_data_Celegans_WILD_P7p_wt_mod/2) / (trait_mean_data_Celegans_WILD_P7p_wt_mod)^2
      
      mean(h2_data_Celegans_WILD_P7p_wt_mod)
      mean(trait_mean_data_Celegans_WILD_P7p_wt_mod)
      mean(va_data_Celegans_WILD_P7p_wt_mod)
      mean(vp_data_Celegans_WILD_P7p_wt_mod)
      mean(Evol_data_Celegans_WILD_P7p_wt_mod)
      
    }
    
  }
  
  #Celegans_Vg_P7p_wt_mod 
  {
    
    Celegans_Vg_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + at.level(fTreatment,1):Ancestral + at.level(fTreatment,2):Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vg_Celegans_data,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)        
    
    
    saveRDS(Celegans_Vg_P7p_wt_mod, file = "Celegans_Vg_P7p_wt_mod.rds")
    Celegans_Vg_P7p_wt_mod <- readRDS("Celegans_Vg_P7p_wt_mod.rds")
    
    summary(Celegans_Vg_P7p_wt_mod) 
    plotTrace(Celegans_Vg_P7p_wt_mod$Sol)
    View(Celegans_Vg_P7p_wt_mod[["Sol"]])
    # traces and posterior densities
    pdf("Celegans_Vg_P7p_wt_mod.pdf")
    plot(Celegans_Vg_P7p_wt_mod[["Sol"]][,c(1:6)]) 
    plot(Celegans_Vg_P7p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Celegans_Vg_P7p_wt_mod[["Sol"]][,c(1:6)]) #
    heidel.diag(Celegans_Vg_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Celegans_Vg_P7p_wt_mod[["Sol"]][,c(1:6)])
    #autocorr.plot(Celegans_Vg_P7p_wt_mod[["Sol"]][,c(1:6)]) #
    autocorr.diag(Celegans_Vg_P7p_wt_mod[["VCV"]])
    #autocorr.plot(Celegans_Vg_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Celegans_Vg_P7p_wt_mod[["Sol"]][,c(1:6)]) # 
    effectiveSize(Celegans_Vg_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Celegans_Vg_P7p_wt_mod <- Celegans_Vg_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_Celegans_Vg_P7p_wt_mod <- Celegans_Vg_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_Celegans_Vg_P7p_wt_mod <- rowSums(Celegans_Vg_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_Celegans_Vg_P7p_wt_mod) 
      HPDinterval(va_liab_Celegans_Vg_P7p_wt_mod) 
      
      mean(vlat_Celegans_Vg_P7p_wt_mod) 
      
      #variance of fixed effects
      X_Celegans_Vg_P7p_wt_mod <- Celegans_Vg_P7p_wt_mod[["X"]]
      beta_Celegans_Vg_P7p_wt_mod <- Celegans_Vg_P7p_wt_mod[["Sol"]][,c(1:6)]
      vf_Celegans_Vg_P7p_wt_mod   <- apply(beta_Celegans_Vg_P7p_wt_mod, 1, function(b) {var(as.vector(X_Celegans_Vg_P7p_wt_mod %*% b))}) 
      mean(vf_Celegans_Vg_P7p_wt_mod) 
      
      h2_liab_Celegans_Vg_P7p_wt_mod <- va_liab_Celegans_Vg_P7p_wt_mod / (vlat_Celegans_Vg_P7p_wt_mod + vf_Celegans_Vg_P7p_wt_mod)
      mean(h2_liab_Celegans_Vg_P7p_wt_mod) 
      posterior.mode(h2_liab_Celegans_Vg_P7p_wt_mod)	
      median(h2_liab_Celegans_Vg_P7p_wt_mod)		
      HPDinterval(h2_liab_Celegans_Vg_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Celegans_Vg_P7p_wt_mod[["Sol"]][,c(1:4)])) #Cel WILD P7p_wt
    mean(rowMeans(Celegans_Vg_P7p_wt_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P7p_wt_mod[["Sol"]][,5]) #JU1200 CONTROL P7p_wt
    mean(rowMeans(Celegans_Vg_P7p_wt_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P7p_wt_mod[["Sol"]][,6]) #PB306 Control P7p_wt
    
    trait_mean_liab_Celegans_Vg_P7p_wt_mod <- ((rowMeans(Celegans_Vg_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Celegans_Vg_P7p_wt_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P7p_wt_mod[["Sol"]][,5]) + (rowMeans(Celegans_Vg_P7p_wt_mod[["Sol"]][,c(1:4)]) + Celegans_Vg_P7p_wt_mod[["Sol"]][,6]) )/3)
    
    Evol_liab_Celegans_Vg_P7p_wt_mod <- (va_liab_Celegans_Vg_P7p_wt_mod/2) / (trait_mean_liab_Celegans_Vg_P7p_wt_mod)^2
    mean(Evol_liab_Celegans_Vg_P7p_wt_mod)
    
    
    #Celegans_Vg_P7p_wt_mod data scale
    {
      
      predict_Celegans_Vg_P7p_wt_mod <- map(1:nrow(Celegans_Vg_P7p_wt_mod[["Sol"]][,c(1:6)]), ~ as.vector(X_Celegans_Vg_P7p_wt_mod %*% Celegans_Vg_P7p_wt_mod[["Sol"]][,c(1:6)][., ]))
      
      data_Celegans_Vg_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_Celegans_Vg_P7p_wt_mod,
                      var.a = Celegans_Vg_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Celegans_Vg_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Celegans_Vg_P7p_wt_mod <- data_Celegans_Vg_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_Celegans_Vg_P7p_wt_mod <- data_Celegans_Vg_P7p_wt_mod[["mean.obs"]]
      va_data_Celegans_Vg_P7p_wt_mod <- data_Celegans_Vg_P7p_wt_mod[["var.a.obs"]]
      vp_data_Celegans_Vg_P7p_wt_mod <- data_Celegans_Vg_P7p_wt_mod[["var.obs"]]
      
      Evol_data_Celegans_Vg_P7p_wt_mod <- (va_data_Celegans_Vg_P7p_wt_mod/2) / (trait_mean_data_Celegans_Vg_P7p_wt_mod)^2
      
      mean(h2_data_Celegans_Vg_P7p_wt_mod)
      mean(trait_mean_data_Celegans_Vg_P7p_wt_mod)
      mean(va_data_Celegans_Vg_P7p_wt_mod)
      mean(vp_data_Celegans_Vg_P7p_wt_mod)
      mean(Evol_data_Celegans_Vg_P7p_wt_mod)
      
    }
    
  }
  
 

#Cbriggsae----
{
  Vg_Cbriggsae_bi_CONTROL <- subset(Vg_Cbriggsae_data, Treatment =="CONTROL")
  Vg_Cbriggsae_bi_WILD <- subset(Vg_Cbriggsae_data, Treatment =="WILD")
  table(Vg_Cbriggsae_bi_CONTROL$Observer)
  table(Vg_Cbriggsae_bi_WILD$Observer)
  
  ## Cbriggsae P3p ----
  
  
  #Cbriggsae_CONTROL_P3p_SS_mod 
  {
    
    Cbriggsae_CONTROL_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer + Ancestral -1,
                                            random      = ~ LineB + BlockRep,
                                            family      = "threshold",
                                            data        = Vg_Cbriggsae_bi_CONTROL,
                                            prior       = prior_bi_block,
                                            nitt        = 1260000,       
                                            thin        = 500,           
                                            burnin      = 10000,
                                            trunc       = TRUE,
                                            pr          = TRUE,
                                            pl          = TRUE)            
    
    saveRDS(Cbriggsae_CONTROL_P3p_SS_mod, file = "Cbriggsae_CONTROL_P3p_SS_mod.rds")
    Cbriggsae_CONTROL_P3p_SS_mod <- readRDS("Cbriggsae_CONTROL_P3p_SS_mod.rds")
    
    summary(Cbriggsae_CONTROL_P3p_SS_mod) 
    plotTrace(Cbriggsae_CONTROL_P3p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Cbriggsae_CONTROL_P3p_SS_mod.pdf")
    plot(Cbriggsae_CONTROL_P3p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(Cbriggsae_CONTROL_P3p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_CONTROL_P3p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Cbriggsae_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_CONTROL_P3p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Cbriggsae_CONTROL_P3p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Cbriggsae_CONTROL_P3p_SS_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_CONTROL_P3p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Cbriggsae_CONTROL_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_CONTROL_P3p_SS_mod <- Cbriggsae_CONTROL_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_CONTROL_P3p_SS_mod <- Cbriggsae_CONTROL_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_CONTROL_P3p_SS_mod <- rowSums(Cbriggsae_CONTROL_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_CONTROL_P3p_SS_mod) 
      HPDinterval(va_liab_Cbriggsae_CONTROL_P3p_SS_mod) 
      
      mean(vlat_Cbriggsae_CONTROL_P3p_SS_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_CONTROL_P3p_SS_mod <- Cbriggsae_CONTROL_P3p_SS_mod[["X"]]
      beta_Cbriggsae_CONTROL_P3p_SS_mod <- Cbriggsae_CONTROL_P3p_SS_mod[["Sol"]][,c(1:5)]
      vf_Cbriggsae_CONTROL_P3p_SS_mod   <- apply(beta_Cbriggsae_CONTROL_P3p_SS_mod, 1, function(b) {var(as.vector(X_Cbriggsae_CONTROL_P3p_SS_mod %*% b))}) 
      mean(vf_Cbriggsae_CONTROL_P3p_SS_mod) 
      
      h2_liab_Cbriggsae_CONTROL_P3p_SS_mod <- va_liab_Cbriggsae_CONTROL_P3p_SS_mod / (vlat_Cbriggsae_CONTROL_P3p_SS_mod + vf_Cbriggsae_CONTROL_P3p_SS_mod)
      mean(h2_liab_Cbriggsae_CONTROL_P3p_SS_mod) 
      posterior.mode(h2_liab_Cbriggsae_CONTROL_P3p_SS_mod)	
      median(h2_liab_Cbriggsae_CONTROL_P3p_SS_mod)		
      HPDinterval(h2_liab_Cbriggsae_CONTROL_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Cbriggsae_CONTROL_P3p_SS_mod <- ((rowMeans(Cbriggsae_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Cbriggsae_CONTROL_P3p_SS_mod[["Sol"]][,c(1:4)]) + Cbriggsae_CONTROL_P3p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Cbriggsae_CONTROL_P3p_SS_mod <- (va_liab_Cbriggsae_CONTROL_P3p_SS_mod/2) / (trait_mean_liab_Cbriggsae_CONTROL_P3p_SS_mod)^2
    mean(Evol_liab_Cbriggsae_CONTROL_P3p_SS_mod)
    
    
    #Cbriggsae_CONTROL_P3p_SS_mod data scale
    {
      
      predict_Cbriggsae_CONTROL_P3p_SS_mod <- map(1:nrow(Cbriggsae_CONTROL_P3p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Cbriggsae_CONTROL_P3p_SS_mod %*% Cbriggsae_CONTROL_P3p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Cbriggsae_CONTROL_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_CONTROL_P3p_SS_mod,
                      var.a = Cbriggsae_CONTROL_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_CONTROL_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_CONTROL_P3p_SS_mod <- data_Cbriggsae_CONTROL_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_CONTROL_P3p_SS_mod <- data_Cbriggsae_CONTROL_P3p_SS_mod[["mean.obs"]]
      va_data_Cbriggsae_CONTROL_P3p_SS_mod <- data_Cbriggsae_CONTROL_P3p_SS_mod[["var.a.obs"]]
      vp_data_Cbriggsae_CONTROL_P3p_SS_mod <- data_Cbriggsae_CONTROL_P3p_SS_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_CONTROL_P3p_SS_mod <- (va_data_Cbriggsae_CONTROL_P3p_SS_mod/2) / (trait_mean_data_Cbriggsae_CONTROL_P3p_SS_mod)^2
      
      mean(h2_data_Cbriggsae_CONTROL_P3p_SS_mod) 
      mean(trait_mean_data_Cbriggsae_CONTROL_P3p_SS_mod)
      mean(va_data_Cbriggsae_CONTROL_P3p_SS_mod) 
      mean(vp_data_Cbriggsae_CONTROL_P3p_SS_mod)
      mean(Evol_data_Cbriggsae_CONTROL_P3p_SS_mod)
      
    }
    
  }
  
  #Cbriggsae_WILD_P3p_SS_mod 
  {
    
    Cbriggsae_WILD_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vg_Cbriggsae_bi_WILD,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)         
    
    saveRDS(Cbriggsae_WILD_P3p_SS_mod, file = "Cbriggsae_WILD_P3p_SS_mod.rds")
    Cbriggsae_WILD_P3p_SS_mod <- readRDS("Cbriggsae_WILD_P3p_SS_mod.rds")
    
    summary(Cbriggsae_WILD_P3p_SS_mod) 
    #plot(Cbriggsae_WILD_P3p_SS_mod)
    
    # traces and posterior densities
    pdf("Cbriggsae_WILD_P3p_SS_mod.pdf")
    plot(Cbriggsae_WILD_P3p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(Cbriggsae_WILD_P3p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_WILD_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(Cbriggsae_WILD_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_WILD_P3p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(Cbriggsae_WILD_P3p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(Cbriggsae_WILD_P3p_SS_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_WILD_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_WILD_P3p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(Cbriggsae_WILD_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_WILD_P3p_SS_mod <- Cbriggsae_WILD_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_WILD_P3p_SS_mod <- Cbriggsae_WILD_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_WILD_P3p_SS_mod <- rowSums(Cbriggsae_WILD_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_WILD_P3p_SS_mod) 
      HPDinterval(va_liab_Cbriggsae_WILD_P3p_SS_mod) 
      
      mean(vlat_Cbriggsae_WILD_P3p_SS_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_WILD_P3p_SS_mod <- Cbriggsae_WILD_P3p_SS_mod[["X"]]
      beta_Cbriggsae_WILD_P3p_SS_mod <- Cbriggsae_WILD_P3p_SS_mod[["Sol"]][,c(1:4)]
      vf_Cbriggsae_WILD_P3p_SS_mod   <- apply(beta_Cbriggsae_WILD_P3p_SS_mod, 1, function(b) {var(as.vector(X_Cbriggsae_WILD_P3p_SS_mod %*% b))}) 
      mean(vf_Cbriggsae_WILD_P3p_SS_mod) 
      
      h2_liab_Cbriggsae_WILD_P3p_SS_mod <- va_liab_Cbriggsae_WILD_P3p_SS_mod / (vlat_Cbriggsae_WILD_P3p_SS_mod + vf_Cbriggsae_WILD_P3p_SS_mod)
      mean(h2_liab_Cbriggsae_WILD_P3p_SS_mod) 
      posterior.mode(h2_liab_Cbriggsae_WILD_P3p_SS_mod)	
      median(h2_liab_Cbriggsae_WILD_P3p_SS_mod)		
      HPDinterval(h2_liab_Cbriggsae_WILD_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Cbriggsae_WILD_P3p_SS_mod <- rowMeans(Cbriggsae_WILD_P3p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_Cbriggsae_WILD_P3p_SS_mod <- (va_liab_Cbriggsae_WILD_P3p_SS_mod/2) / (trait_mean_liab_Cbriggsae_WILD_P3p_SS_mod)^2
    mean(Evol_liab_Cbriggsae_WILD_P3p_SS_mod)
    
    
    #Cbriggsae_WILD_P3p_SS_mod data scale
    {
      
      predict_Cbriggsae_WILD_P3p_SS_mod <- map(1:nrow(Cbriggsae_WILD_P3p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_Cbriggsae_WILD_P3p_SS_mod %*% Cbriggsae_WILD_P3p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_Cbriggsae_WILD_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_WILD_P3p_SS_mod,
                      var.a = Cbriggsae_WILD_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_WILD_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_WILD_P3p_SS_mod <- data_Cbriggsae_WILD_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_WILD_P3p_SS_mod <- data_Cbriggsae_WILD_P3p_SS_mod[["mean.obs"]]
      va_data_Cbriggsae_WILD_P3p_SS_mod <- data_Cbriggsae_WILD_P3p_SS_mod[["var.a.obs"]]
      vp_data_Cbriggsae_WILD_P3p_SS_mod <- data_Cbriggsae_WILD_P3p_SS_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_WILD_P3p_SS_mod <- (va_data_Cbriggsae_WILD_P3p_SS_mod/2) / (trait_mean_data_Cbriggsae_WILD_P3p_SS_mod)^2
      
      mean(h2_data_Cbriggsae_WILD_P3p_SS_mod)
      mean(trait_mean_data_Cbriggsae_WILD_P3p_SS_mod)
      mean(va_data_Cbriggsae_WILD_P3p_SS_mod)
      mean(vp_data_Cbriggsae_WILD_P3p_SS_mod)
      mean(Evol_data_Cbriggsae_WILD_P3p_SS_mod)
      
    }
    
  }
  
  #Cbriggsae_Vg_P3p_SS_mod 
  {
    
    Cbriggsae_Vg_P3p_SS_mod <- MCMCglmm(fixed       = P3.p_SS ~ Observer + at.level(fTreatment,1):Ancestral + at.level(fTreatment,2):Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vg_Cbriggsae_data,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)        
    
    saveRDS(Cbriggsae_Vg_P3p_SS_mod, file = "Cbriggsae_Vg_P3p_SS_mod.rds")
    Cbriggsae_Vg_P3p_SS_mod <- readRDS("Cbriggsae_Vg_P3p_SS_mod.rds")
    
    summary(Cbriggsae_Vg_P3p_SS_mod) 
    plotTrace(Cbriggsae_Vg_P3p_SS_mod$Sol)
    View(Cbriggsae_Vg_P3p_SS_mod[["Sol"]])
    # traces and posterior densities
    pdf("Cbriggsae_Vg_P3p_SS_mod.pdf")
    plot(Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,c(1:6)]) 
    plot(Cbriggsae_Vg_P3p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,c(1:6)]) #
    heidel.diag(Cbriggsae_Vg_P3p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,c(1:6)])
    #autocorr.plot(Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,c(1:6)]) #
    autocorr.diag(Cbriggsae_Vg_P3p_SS_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_Vg_P3p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,c(1:6)]) # 
    effectiveSize(Cbriggsae_Vg_P3p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_Vg_P3p_SS_mod <- Cbriggsae_Vg_P3p_SS_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_Vg_P3p_SS_mod <- Cbriggsae_Vg_P3p_SS_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_Vg_P3p_SS_mod <- rowSums(Cbriggsae_Vg_P3p_SS_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_Vg_P3p_SS_mod) 
      HPDinterval(va_liab_Cbriggsae_Vg_P3p_SS_mod) 
      
      mean(vlat_Cbriggsae_Vg_P3p_SS_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_Vg_P3p_SS_mod <- Cbriggsae_Vg_P3p_SS_mod[["X"]]
      beta_Cbriggsae_Vg_P3p_SS_mod <- Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,c(1:6)]
      vf_Cbriggsae_Vg_P3p_SS_mod   <- apply(beta_Cbriggsae_Vg_P3p_SS_mod, 1, function(b) {var(as.vector(X_Cbriggsae_Vg_P3p_SS_mod %*% b))}) 
      mean(vf_Cbriggsae_Vg_P3p_SS_mod) 
      
      h2_liab_Cbriggsae_Vg_P3p_SS_mod <- va_liab_Cbriggsae_Vg_P3p_SS_mod / (vlat_Cbriggsae_Vg_P3p_SS_mod + vf_Cbriggsae_Vg_P3p_SS_mod)
      mean(h2_liab_Cbriggsae_Vg_P3p_SS_mod) 
      posterior.mode(h2_liab_Cbriggsae_Vg_P3p_SS_mod)	
      median(h2_liab_Cbriggsae_Vg_P3p_SS_mod)		
      HPDinterval(h2_liab_Cbriggsae_Vg_P3p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,c(1:4)])) #Cbri WILD P3p_SS
    mean(rowMeans(Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,5]) #AF16 CONTROL P3p_SS
    mean(rowMeans(Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,6]) #PB800 Control P3p_SS
    
    trait_mean_liab_Cbriggsae_Vg_P3p_SS_mod <- ((rowMeans(Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,5]) + (rowMeans(Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,6]) )/3)
    
    Evol_liab_Cbriggsae_Vg_P3p_SS_mod <- (va_liab_Cbriggsae_Vg_P3p_SS_mod/2) / (trait_mean_liab_Cbriggsae_Vg_P3p_SS_mod)^2
    mean(Evol_liab_Cbriggsae_Vg_P3p_SS_mod)
    
    
    #Cbriggsae_Vg_P3p_SS_mod data scale
    {
      
      predict_Cbriggsae_Vg_P3p_SS_mod <- map(1:nrow(Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,c(1:6)]), ~ as.vector(X_Cbriggsae_Vg_P3p_SS_mod %*% Cbriggsae_Vg_P3p_SS_mod[["Sol"]][,c(1:6)][., ]))
      
      data_Cbriggsae_Vg_P3p_SS_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_Vg_P3p_SS_mod,
                      var.a = Cbriggsae_Vg_P3p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_Vg_P3p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_Vg_P3p_SS_mod <- data_Cbriggsae_Vg_P3p_SS_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_Vg_P3p_SS_mod <- data_Cbriggsae_Vg_P3p_SS_mod[["mean.obs"]]
      va_data_Cbriggsae_Vg_P3p_SS_mod <- data_Cbriggsae_Vg_P3p_SS_mod[["var.a.obs"]]
      vp_data_Cbriggsae_Vg_P3p_SS_mod <- data_Cbriggsae_Vg_P3p_SS_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_Vg_P3p_SS_mod <- (va_data_Cbriggsae_Vg_P3p_SS_mod/2) / (trait_mean_data_Cbriggsae_Vg_P3p_SS_mod)^2
      
      mean(h2_data_Cbriggsae_Vg_P3p_SS_mod)
      mean(trait_mean_data_Cbriggsae_Vg_P3p_SS_mod)
      mean(va_data_Cbriggsae_Vg_P3p_SS_mod)
      mean(vp_data_Cbriggsae_Vg_P3p_SS_mod)
      mean(Evol_data_Cbriggsae_Vg_P3p_SS_mod)
      
    }
    
  }
  
  
  
  ## Cbriggsae P4p ----
  
  
  #Cbriggsae_CONTROL_P4p_SS_mod 
  {
    
    Cbriggsae_CONTROL_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer + Ancestral -1,
                                            random      = ~ LineB + BlockRep,
                                            family      = "threshold",
                                            data        = Vg_Cbriggsae_bi_CONTROL,
                                            prior       = prior_bi_block,
                                            nitt        = 1260000,       
                                            thin        = 500,           
                                            burnin      = 10000,
                                            trunc       = TRUE,
                                            pr          = TRUE,
                                            pl          = TRUE)            
    
    saveRDS(Cbriggsae_CONTROL_P4p_SS_mod, file = "Cbriggsae_CONTROL_P4p_SS_mod.rds")
    Cbriggsae_CONTROL_P4p_SS_mod <- readRDS("Cbriggsae_CONTROL_P4p_SS_mod.rds")
    
    summary(Cbriggsae_CONTROL_P4p_SS_mod) 
    plotTrace(Cbriggsae_CONTROL_P4p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Cbriggsae_CONTROL_P4p_SS_mod.pdf")
    plot(Cbriggsae_CONTROL_P4p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(Cbriggsae_CONTROL_P4p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_CONTROL_P4p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Cbriggsae_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_CONTROL_P4p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Cbriggsae_CONTROL_P4p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Cbriggsae_CONTROL_P4p_SS_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_CONTROL_P4p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Cbriggsae_CONTROL_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_CONTROL_P4p_SS_mod <- Cbriggsae_CONTROL_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_CONTROL_P4p_SS_mod <- Cbriggsae_CONTROL_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_CONTROL_P4p_SS_mod <- rowSums(Cbriggsae_CONTROL_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_CONTROL_P4p_SS_mod) 
      HPDinterval(va_liab_Cbriggsae_CONTROL_P4p_SS_mod) 
      
      mean(vlat_Cbriggsae_CONTROL_P4p_SS_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_CONTROL_P4p_SS_mod <- Cbriggsae_CONTROL_P4p_SS_mod[["X"]]
      beta_Cbriggsae_CONTROL_P4p_SS_mod <- Cbriggsae_CONTROL_P4p_SS_mod[["Sol"]][,c(1:5)]
      vf_Cbriggsae_CONTROL_P4p_SS_mod   <- apply(beta_Cbriggsae_CONTROL_P4p_SS_mod, 1, function(b) {var(as.vector(X_Cbriggsae_CONTROL_P4p_SS_mod %*% b))}) 
      mean(vf_Cbriggsae_CONTROL_P4p_SS_mod) 
      
      h2_liab_Cbriggsae_CONTROL_P4p_SS_mod <- va_liab_Cbriggsae_CONTROL_P4p_SS_mod / (vlat_Cbriggsae_CONTROL_P4p_SS_mod + vf_Cbriggsae_CONTROL_P4p_SS_mod)
      mean(h2_liab_Cbriggsae_CONTROL_P4p_SS_mod) 
      posterior.mode(h2_liab_Cbriggsae_CONTROL_P4p_SS_mod)	
      median(h2_liab_Cbriggsae_CONTROL_P4p_SS_mod)		
      HPDinterval(h2_liab_Cbriggsae_CONTROL_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Cbriggsae_CONTROL_P4p_SS_mod <- ((rowMeans(Cbriggsae_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Cbriggsae_CONTROL_P4p_SS_mod[["Sol"]][,c(1:4)]) + Cbriggsae_CONTROL_P4p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Cbriggsae_CONTROL_P4p_SS_mod <- (va_liab_Cbriggsae_CONTROL_P4p_SS_mod/2) / (trait_mean_liab_Cbriggsae_CONTROL_P4p_SS_mod)^2
    mean(Evol_liab_Cbriggsae_CONTROL_P4p_SS_mod)
    
    
    #Cbriggsae_CONTROL_P4p_SS_mod data scale
    {
      
      predict_Cbriggsae_CONTROL_P4p_SS_mod <- map(1:nrow(Cbriggsae_CONTROL_P4p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Cbriggsae_CONTROL_P4p_SS_mod %*% Cbriggsae_CONTROL_P4p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Cbriggsae_CONTROL_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_CONTROL_P4p_SS_mod,
                      var.a = Cbriggsae_CONTROL_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_CONTROL_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_CONTROL_P4p_SS_mod <- data_Cbriggsae_CONTROL_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_CONTROL_P4p_SS_mod <- data_Cbriggsae_CONTROL_P4p_SS_mod[["mean.obs"]]
      va_data_Cbriggsae_CONTROL_P4p_SS_mod <- data_Cbriggsae_CONTROL_P4p_SS_mod[["var.a.obs"]]
      vp_data_Cbriggsae_CONTROL_P4p_SS_mod <- data_Cbriggsae_CONTROL_P4p_SS_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_CONTROL_P4p_SS_mod <- (va_data_Cbriggsae_CONTROL_P4p_SS_mod/2) / (trait_mean_data_Cbriggsae_CONTROL_P4p_SS_mod)^2
      
      mean(h2_data_Cbriggsae_CONTROL_P4p_SS_mod) 
      mean(trait_mean_data_Cbriggsae_CONTROL_P4p_SS_mod)
      mean(va_data_Cbriggsae_CONTROL_P4p_SS_mod) 
      mean(vp_data_Cbriggsae_CONTROL_P4p_SS_mod)
      mean(Evol_data_Cbriggsae_CONTROL_P4p_SS_mod)
      
    }
    
  }
  
  #Cbriggsae_WILD_P4p_SS_mod 
  {
    
    Cbriggsae_WILD_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vg_Cbriggsae_bi_WILD,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)         
    
    saveRDS(Cbriggsae_WILD_P4p_SS_mod, file = "Cbriggsae_WILD_P4p_SS_mod.rds")
    Cbriggsae_WILD_P4p_SS_mod <- readRDS("Cbriggsae_WILD_P4p_SS_mod.rds")
    
    summary(Cbriggsae_WILD_P4p_SS_mod) 
    #plot(Cbriggsae_WILD_P4p_SS_mod)
    
    # traces and posterior densities
    pdf("Cbriggsae_WILD_P4p_SS_mod.pdf")
    plot(Cbriggsae_WILD_P4p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(Cbriggsae_WILD_P4p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_WILD_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(Cbriggsae_WILD_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_WILD_P4p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(Cbriggsae_WILD_P4p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(Cbriggsae_WILD_P4p_SS_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_WILD_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_WILD_P4p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(Cbriggsae_WILD_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_WILD_P4p_SS_mod <- Cbriggsae_WILD_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_WILD_P4p_SS_mod <- Cbriggsae_WILD_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_WILD_P4p_SS_mod <- rowSums(Cbriggsae_WILD_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_WILD_P4p_SS_mod) 
      HPDinterval(va_liab_Cbriggsae_WILD_P4p_SS_mod) 
      
      mean(vlat_Cbriggsae_WILD_P4p_SS_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_WILD_P4p_SS_mod <- Cbriggsae_WILD_P4p_SS_mod[["X"]]
      beta_Cbriggsae_WILD_P4p_SS_mod <- Cbriggsae_WILD_P4p_SS_mod[["Sol"]][,c(1:4)]
      vf_Cbriggsae_WILD_P4p_SS_mod   <- apply(beta_Cbriggsae_WILD_P4p_SS_mod, 1, function(b) {var(as.vector(X_Cbriggsae_WILD_P4p_SS_mod %*% b))}) 
      mean(vf_Cbriggsae_WILD_P4p_SS_mod) 
      
      h2_liab_Cbriggsae_WILD_P4p_SS_mod <- va_liab_Cbriggsae_WILD_P4p_SS_mod / (vlat_Cbriggsae_WILD_P4p_SS_mod + vf_Cbriggsae_WILD_P4p_SS_mod)
      mean(h2_liab_Cbriggsae_WILD_P4p_SS_mod) 
      posterior.mode(h2_liab_Cbriggsae_WILD_P4p_SS_mod)	
      median(h2_liab_Cbriggsae_WILD_P4p_SS_mod)		
      HPDinterval(h2_liab_Cbriggsae_WILD_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Cbriggsae_WILD_P4p_SS_mod <- rowMeans(Cbriggsae_WILD_P4p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_Cbriggsae_WILD_P4p_SS_mod <- (va_liab_Cbriggsae_WILD_P4p_SS_mod/2) / (trait_mean_liab_Cbriggsae_WILD_P4p_SS_mod)^2
    mean(Evol_liab_Cbriggsae_WILD_P4p_SS_mod)
    
    
    #Cbriggsae_WILD_P4p_SS_mod data scale
    {
      
      predict_Cbriggsae_WILD_P4p_SS_mod <- map(1:nrow(Cbriggsae_WILD_P4p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_Cbriggsae_WILD_P4p_SS_mod %*% Cbriggsae_WILD_P4p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_Cbriggsae_WILD_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_WILD_P4p_SS_mod,
                      var.a = Cbriggsae_WILD_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_WILD_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_WILD_P4p_SS_mod <- data_Cbriggsae_WILD_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_WILD_P4p_SS_mod <- data_Cbriggsae_WILD_P4p_SS_mod[["mean.obs"]]
      va_data_Cbriggsae_WILD_P4p_SS_mod <- data_Cbriggsae_WILD_P4p_SS_mod[["var.a.obs"]]
      vp_data_Cbriggsae_WILD_P4p_SS_mod <- data_Cbriggsae_WILD_P4p_SS_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_WILD_P4p_SS_mod <- (va_data_Cbriggsae_WILD_P4p_SS_mod/2) / (trait_mean_data_Cbriggsae_WILD_P4p_SS_mod)^2
      
      mean(h2_data_Cbriggsae_WILD_P4p_SS_mod)
      mean(trait_mean_data_Cbriggsae_WILD_P4p_SS_mod)
      mean(va_data_Cbriggsae_WILD_P4p_SS_mod)
      mean(vp_data_Cbriggsae_WILD_P4p_SS_mod)
      mean(Evol_data_Cbriggsae_WILD_P4p_SS_mod)
      
    }
    
  }
  
  #Cbriggsae_Vg_P4p_SS_mod 
  {
    
    Cbriggsae_Vg_P4p_SS_mod <- MCMCglmm(fixed       = P4.p_SS ~ Observer + at.level(fTreatment,1):Ancestral + at.level(fTreatment,2):Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vg_Cbriggsae_data,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)        
    
    
    saveRDS(Cbriggsae_Vg_P4p_SS_mod, file = "Cbriggsae_Vg_P4p_SS_mod.rds")
    Cbriggsae_Vg_P4p_SS_mod <- readRDS("Cbriggsae_Vg_P4p_SS_mod.rds")
    
    summary(Cbriggsae_Vg_P4p_SS_mod) 
    plotTrace(Cbriggsae_Vg_P4p_SS_mod$Sol)
    View(Cbriggsae_Vg_P4p_SS_mod[["Sol"]])
    # traces and posterior densities
    pdf("Cbriggsae_Vg_P4p_SS_mod.pdf")
    plot(Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,c(1:6)]) 
    plot(Cbriggsae_Vg_P4p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,c(1:6)]) #
    heidel.diag(Cbriggsae_Vg_P4p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,c(1:6)])
    #autocorr.plot(Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,c(1:6)]) #
    autocorr.diag(Cbriggsae_Vg_P4p_SS_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_Vg_P4p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,c(1:6)]) # 
    effectiveSize(Cbriggsae_Vg_P4p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_Vg_P4p_SS_mod <- Cbriggsae_Vg_P4p_SS_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_Vg_P4p_SS_mod <- Cbriggsae_Vg_P4p_SS_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_Vg_P4p_SS_mod <- rowSums(Cbriggsae_Vg_P4p_SS_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_Vg_P4p_SS_mod) 
      HPDinterval(va_liab_Cbriggsae_Vg_P4p_SS_mod) 
      
      mean(vlat_Cbriggsae_Vg_P4p_SS_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_Vg_P4p_SS_mod <- Cbriggsae_Vg_P4p_SS_mod[["X"]]
      beta_Cbriggsae_Vg_P4p_SS_mod <- Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,c(1:6)]
      vf_Cbriggsae_Vg_P4p_SS_mod   <- apply(beta_Cbriggsae_Vg_P4p_SS_mod, 1, function(b) {var(as.vector(X_Cbriggsae_Vg_P4p_SS_mod %*% b))}) 
      mean(vf_Cbriggsae_Vg_P4p_SS_mod) 
      
      h2_liab_Cbriggsae_Vg_P4p_SS_mod <- va_liab_Cbriggsae_Vg_P4p_SS_mod / (vlat_Cbriggsae_Vg_P4p_SS_mod + vf_Cbriggsae_Vg_P4p_SS_mod)
      mean(h2_liab_Cbriggsae_Vg_P4p_SS_mod) 
      posterior.mode(h2_liab_Cbriggsae_Vg_P4p_SS_mod)	
      median(h2_liab_Cbriggsae_Vg_P4p_SS_mod)		
      HPDinterval(h2_liab_Cbriggsae_Vg_P4p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,c(1:4)])) #Cbri WILD P4p_SS
    mean(rowMeans(Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,5]) #AF16 CONTROL P4p_SS
    mean(rowMeans(Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,6]) #PB800 Control P4p_SS
    
    trait_mean_liab_Cbriggsae_Vg_P4p_SS_mod <- ((rowMeans(Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,5]) + (rowMeans(Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,6]) )/3)
    
    Evol_liab_Cbriggsae_Vg_P4p_SS_mod <- (va_liab_Cbriggsae_Vg_P4p_SS_mod/2) / (trait_mean_liab_Cbriggsae_Vg_P4p_SS_mod)^2
    mean(Evol_liab_Cbriggsae_Vg_P4p_SS_mod)
    
    
    #Cbriggsae_Vg_P4p_SS_mod data scale
    {
      
      predict_Cbriggsae_Vg_P4p_SS_mod <- map(1:nrow(Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,c(1:6)]), ~ as.vector(X_Cbriggsae_Vg_P4p_SS_mod %*% Cbriggsae_Vg_P4p_SS_mod[["Sol"]][,c(1:6)][., ]))
      
      data_Cbriggsae_Vg_P4p_SS_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_Vg_P4p_SS_mod,
                      var.a = Cbriggsae_Vg_P4p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_Vg_P4p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_Vg_P4p_SS_mod <- data_Cbriggsae_Vg_P4p_SS_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_Vg_P4p_SS_mod <- data_Cbriggsae_Vg_P4p_SS_mod[["mean.obs"]]
      va_data_Cbriggsae_Vg_P4p_SS_mod <- data_Cbriggsae_Vg_P4p_SS_mod[["var.a.obs"]]
      vp_data_Cbriggsae_Vg_P4p_SS_mod <- data_Cbriggsae_Vg_P4p_SS_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_Vg_P4p_SS_mod <- (va_data_Cbriggsae_Vg_P4p_SS_mod/2) / (trait_mean_data_Cbriggsae_Vg_P4p_SS_mod)^2
      
      mean(h2_data_Cbriggsae_Vg_P4p_SS_mod)
      mean(trait_mean_data_Cbriggsae_Vg_P4p_SS_mod)
      mean(va_data_Cbriggsae_Vg_P4p_SS_mod)
      mean(vp_data_Cbriggsae_Vg_P4p_SS_mod)
      mean(Evol_data_Cbriggsae_Vg_P4p_SS_mod)
      
    }
    
  }
  
  
  
  ## Cbriggsae P8p ----
  
  
  #Cbriggsae_CONTROL_P8p_SS_mod 
  {
    
    Cbriggsae_CONTROL_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer + Ancestral -1,
                                            random      = ~ LineB + BlockRep,
                                            family      = "threshold",
                                            data        = Vg_Cbriggsae_bi_CONTROL,
                                            prior       = prior_bi_block,
                                            nitt        = 1260000,       
                                            thin        = 500,           
                                            burnin      = 10000,
                                            trunc       = TRUE,
                                            pr          = TRUE,
                                            pl          = TRUE)            
    
    saveRDS(Cbriggsae_CONTROL_P8p_SS_mod, file = "Cbriggsae_CONTROL_P8p_SS_mod.rds")
    Cbriggsae_CONTROL_P8p_SS_mod <- readRDS("Cbriggsae_CONTROL_P8p_SS_mod.rds")
    
    summary(Cbriggsae_CONTROL_P8p_SS_mod) 
    plotTrace(Cbriggsae_CONTROL_P8p_SS_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Cbriggsae_CONTROL_P8p_SS_mod.pdf")
    plot(Cbriggsae_CONTROL_P8p_SS_mod[["Sol"]][,c(1:5)]) 
    plot(Cbriggsae_CONTROL_P8p_SS_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_CONTROL_P8p_SS_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Cbriggsae_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_CONTROL_P8p_SS_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Cbriggsae_CONTROL_P8p_SS_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Cbriggsae_CONTROL_P8p_SS_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_CONTROL_P8p_SS_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Cbriggsae_CONTROL_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_CONTROL_P8p_SS_mod <- Cbriggsae_CONTROL_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_CONTROL_P8p_SS_mod <- Cbriggsae_CONTROL_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_CONTROL_P8p_SS_mod <- rowSums(Cbriggsae_CONTROL_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_CONTROL_P8p_SS_mod) 
      HPDinterval(va_liab_Cbriggsae_CONTROL_P8p_SS_mod) 
      
      mean(vlat_Cbriggsae_CONTROL_P8p_SS_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_CONTROL_P8p_SS_mod <- Cbriggsae_CONTROL_P8p_SS_mod[["X"]]
      beta_Cbriggsae_CONTROL_P8p_SS_mod <- Cbriggsae_CONTROL_P8p_SS_mod[["Sol"]][,c(1:5)]
      vf_Cbriggsae_CONTROL_P8p_SS_mod   <- apply(beta_Cbriggsae_CONTROL_P8p_SS_mod, 1, function(b) {var(as.vector(X_Cbriggsae_CONTROL_P8p_SS_mod %*% b))}) 
      mean(vf_Cbriggsae_CONTROL_P8p_SS_mod) 
      
      h2_liab_Cbriggsae_CONTROL_P8p_SS_mod <- va_liab_Cbriggsae_CONTROL_P8p_SS_mod / (vlat_Cbriggsae_CONTROL_P8p_SS_mod + vf_Cbriggsae_CONTROL_P8p_SS_mod)
      mean(h2_liab_Cbriggsae_CONTROL_P8p_SS_mod) 
      posterior.mode(h2_liab_Cbriggsae_CONTROL_P8p_SS_mod)	
      median(h2_liab_Cbriggsae_CONTROL_P8p_SS_mod)		
      HPDinterval(h2_liab_Cbriggsae_CONTROL_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Cbriggsae_CONTROL_P8p_SS_mod <- ((rowMeans(Cbriggsae_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Cbriggsae_CONTROL_P8p_SS_mod[["Sol"]][,c(1:4)]) + Cbriggsae_CONTROL_P8p_SS_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Cbriggsae_CONTROL_P8p_SS_mod <- (va_liab_Cbriggsae_CONTROL_P8p_SS_mod/2) / (trait_mean_liab_Cbriggsae_CONTROL_P8p_SS_mod)^2
    mean(Evol_liab_Cbriggsae_CONTROL_P8p_SS_mod)
    
    
    #Cbriggsae_CONTROL_P8p_SS_mod data scale
    {
      
      predict_Cbriggsae_CONTROL_P8p_SS_mod <- map(1:nrow(Cbriggsae_CONTROL_P8p_SS_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Cbriggsae_CONTROL_P8p_SS_mod %*% Cbriggsae_CONTROL_P8p_SS_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Cbriggsae_CONTROL_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_CONTROL_P8p_SS_mod,
                      var.a = Cbriggsae_CONTROL_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_CONTROL_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_CONTROL_P8p_SS_mod <- data_Cbriggsae_CONTROL_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_CONTROL_P8p_SS_mod <- data_Cbriggsae_CONTROL_P8p_SS_mod[["mean.obs"]]
      va_data_Cbriggsae_CONTROL_P8p_SS_mod <- data_Cbriggsae_CONTROL_P8p_SS_mod[["var.a.obs"]]
      vp_data_Cbriggsae_CONTROL_P8p_SS_mod <- data_Cbriggsae_CONTROL_P8p_SS_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_CONTROL_P8p_SS_mod <- (va_data_Cbriggsae_CONTROL_P8p_SS_mod/2) / (trait_mean_data_Cbriggsae_CONTROL_P8p_SS_mod)^2
      
      mean(h2_data_Cbriggsae_CONTROL_P8p_SS_mod) 
      mean(trait_mean_data_Cbriggsae_CONTROL_P8p_SS_mod)
      mean(va_data_Cbriggsae_CONTROL_P8p_SS_mod) 
      mean(vp_data_Cbriggsae_CONTROL_P8p_SS_mod)
      mean(Evol_data_Cbriggsae_CONTROL_P8p_SS_mod)
      
    }
    
  }
  
  #Cbriggsae_WILD_P8p_SS_mod 
  {
    
    Cbriggsae_WILD_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer -1,
                                         random      = ~ LineB + BlockRep,
                                         family      = "threshold",
                                         data        = Vg_Cbriggsae_bi_WILD,
                                         prior       = prior_bi_block,
                                         nitt        = 1260000,       
                                         thin        = 500,           
                                         burnin      = 10000,
                                         trunc       = TRUE,
                                         pr          = TRUE,
                                         pl          = TRUE)         
    
    saveRDS(Cbriggsae_WILD_P8p_SS_mod, file = "Cbriggsae_WILD_P8p_SS_mod.rds")
    Cbriggsae_WILD_P8p_SS_mod <- readRDS("Cbriggsae_WILD_P8p_SS_mod.rds")
    
    summary(Cbriggsae_WILD_P8p_SS_mod) 
    #plot(Cbriggsae_WILD_P8p_SS_mod)
    
    # traces and posterior densities
    pdf("Cbriggsae_WILD_P8p_SS_mod.pdf")
    plot(Cbriggsae_WILD_P8p_SS_mod[["Sol"]][,c(1:4)]) 
    plot(Cbriggsae_WILD_P8p_SS_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_WILD_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(Cbriggsae_WILD_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_WILD_P8p_SS_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(Cbriggsae_WILD_P8p_SS_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(Cbriggsae_WILD_P8p_SS_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_WILD_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_WILD_P8p_SS_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(Cbriggsae_WILD_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_WILD_P8p_SS_mod <- Cbriggsae_WILD_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_WILD_P8p_SS_mod <- Cbriggsae_WILD_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_WILD_P8p_SS_mod <- rowSums(Cbriggsae_WILD_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_WILD_P8p_SS_mod) 
      HPDinterval(va_liab_Cbriggsae_WILD_P8p_SS_mod) 
      
      mean(vlat_Cbriggsae_WILD_P8p_SS_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_WILD_P8p_SS_mod <- Cbriggsae_WILD_P8p_SS_mod[["X"]]
      beta_Cbriggsae_WILD_P8p_SS_mod <- Cbriggsae_WILD_P8p_SS_mod[["Sol"]][,c(1:4)]
      vf_Cbriggsae_WILD_P8p_SS_mod   <- apply(beta_Cbriggsae_WILD_P8p_SS_mod, 1, function(b) {var(as.vector(X_Cbriggsae_WILD_P8p_SS_mod %*% b))}) 
      mean(vf_Cbriggsae_WILD_P8p_SS_mod) 
      
      h2_liab_Cbriggsae_WILD_P8p_SS_mod <- va_liab_Cbriggsae_WILD_P8p_SS_mod / (vlat_Cbriggsae_WILD_P8p_SS_mod + vf_Cbriggsae_WILD_P8p_SS_mod)
      mean(h2_liab_Cbriggsae_WILD_P8p_SS_mod) 
      posterior.mode(h2_liab_Cbriggsae_WILD_P8p_SS_mod)	
      median(h2_liab_Cbriggsae_WILD_P8p_SS_mod)		
      HPDinterval(h2_liab_Cbriggsae_WILD_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Cbriggsae_WILD_P8p_SS_mod <- rowMeans(Cbriggsae_WILD_P8p_SS_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_Cbriggsae_WILD_P8p_SS_mod <- (va_liab_Cbriggsae_WILD_P8p_SS_mod/2) / (trait_mean_liab_Cbriggsae_WILD_P8p_SS_mod)^2
    mean(Evol_liab_Cbriggsae_WILD_P8p_SS_mod)
    
    
    #Cbriggsae_WILD_P8p_SS_mod data scale
    {
      
      predict_Cbriggsae_WILD_P8p_SS_mod <- map(1:nrow(Cbriggsae_WILD_P8p_SS_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_Cbriggsae_WILD_P8p_SS_mod %*% Cbriggsae_WILD_P8p_SS_mod[["Sol"]][,c(1:4)][., ]))
      
      data_Cbriggsae_WILD_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_WILD_P8p_SS_mod,
                      var.a = Cbriggsae_WILD_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_WILD_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_WILD_P8p_SS_mod <- data_Cbriggsae_WILD_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_WILD_P8p_SS_mod <- data_Cbriggsae_WILD_P8p_SS_mod[["mean.obs"]]
      va_data_Cbriggsae_WILD_P8p_SS_mod <- data_Cbriggsae_WILD_P8p_SS_mod[["var.a.obs"]]
      vp_data_Cbriggsae_WILD_P8p_SS_mod <- data_Cbriggsae_WILD_P8p_SS_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_WILD_P8p_SS_mod <- (va_data_Cbriggsae_WILD_P8p_SS_mod/2) / (trait_mean_data_Cbriggsae_WILD_P8p_SS_mod)^2
      
      mean(h2_data_Cbriggsae_WILD_P8p_SS_mod)
      mean(trait_mean_data_Cbriggsae_WILD_P8p_SS_mod)
      mean(va_data_Cbriggsae_WILD_P8p_SS_mod)
      mean(vp_data_Cbriggsae_WILD_P8p_SS_mod)
      mean(Evol_data_Cbriggsae_WILD_P8p_SS_mod)
      
    }
    
  }
  
  #Cbriggsae_Vg_P8p_SS_mod 
  {
    
    Cbriggsae_Vg_P8p_SS_mod <- MCMCglmm(fixed       = P8.p_SS ~ Observer + at.level(fTreatment,1):Ancestral + at.level(fTreatment,2):Ancestral -1,
                                       random      = ~ LineB + BlockRep,
                                       family      = "threshold",
                                       data        = Vg_Cbriggsae_data,
                                       prior       = prior_bi_block,
                                       nitt        = 1260000,       
                                       thin        = 500,           
                                       burnin      = 10000,
                                       trunc       = TRUE,
                                       pr          = TRUE,
                                       pl          = TRUE)        
    
    
    saveRDS(Cbriggsae_Vg_P8p_SS_mod, file = "Cbriggsae_Vg_P8p_SS_mod.rds")
    Cbriggsae_Vg_P8p_SS_mod <- readRDS("Cbriggsae_Vg_P8p_SS_mod.rds")
    
    summary(Cbriggsae_Vg_P8p_SS_mod) 
    plotTrace(Cbriggsae_Vg_P8p_SS_mod$Sol)
    View(Cbriggsae_Vg_P8p_SS_mod[["Sol"]])
    # traces and posterior densities
    pdf("Cbriggsae_Vg_P8p_SS_mod.pdf")
    plot(Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,c(1:6)]) 
    plot(Cbriggsae_Vg_P8p_SS_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,c(1:6)]) #
    heidel.diag(Cbriggsae_Vg_P8p_SS_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,c(1:6)])
    #autocorr.plot(Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,c(1:6)]) #
    autocorr.diag(Cbriggsae_Vg_P8p_SS_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_Vg_P8p_SS_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,c(1:6)]) # 
    effectiveSize(Cbriggsae_Vg_P8p_SS_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_Vg_P8p_SS_mod <- Cbriggsae_Vg_P8p_SS_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_Vg_P8p_SS_mod <- Cbriggsae_Vg_P8p_SS_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_Vg_P8p_SS_mod <- rowSums(Cbriggsae_Vg_P8p_SS_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_Vg_P8p_SS_mod) 
      HPDinterval(va_liab_Cbriggsae_Vg_P8p_SS_mod) 
      
      mean(vlat_Cbriggsae_Vg_P8p_SS_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_Vg_P8p_SS_mod <- Cbriggsae_Vg_P8p_SS_mod[["X"]]
      beta_Cbriggsae_Vg_P8p_SS_mod <- Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,c(1:6)]
      vf_Cbriggsae_Vg_P8p_SS_mod   <- apply(beta_Cbriggsae_Vg_P8p_SS_mod, 1, function(b) {var(as.vector(X_Cbriggsae_Vg_P8p_SS_mod %*% b))}) 
      mean(vf_Cbriggsae_Vg_P8p_SS_mod) 
      
      h2_liab_Cbriggsae_Vg_P8p_SS_mod <- va_liab_Cbriggsae_Vg_P8p_SS_mod / (vlat_Cbriggsae_Vg_P8p_SS_mod + vf_Cbriggsae_Vg_P8p_SS_mod)
      mean(h2_liab_Cbriggsae_Vg_P8p_SS_mod) 
      posterior.mode(h2_liab_Cbriggsae_Vg_P8p_SS_mod)	
      median(h2_liab_Cbriggsae_Vg_P8p_SS_mod)		
      HPDinterval(h2_liab_Cbriggsae_Vg_P8p_SS_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,c(1:4)])) #Cbri WILD P8p_SS
    mean(rowMeans(Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,5]) #AF16 CONTROL P8p_SS
    mean(rowMeans(Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,6]) #PB800 Control P8p_SS
    
    trait_mean_liab_Cbriggsae_Vg_P8p_SS_mod <- ((rowMeans(Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,c(1:4)]) + (rowMeans(Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,5]) + (rowMeans(Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,6]) )/3)
    
    Evol_liab_Cbriggsae_Vg_P8p_SS_mod <- (va_liab_Cbriggsae_Vg_P8p_SS_mod/2) / (trait_mean_liab_Cbriggsae_Vg_P8p_SS_mod)^2
    mean(Evol_liab_Cbriggsae_Vg_P8p_SS_mod)
    
    
    #Cbriggsae_Vg_P8p_SS_mod data scale
    {
      
      predict_Cbriggsae_Vg_P8p_SS_mod <- map(1:nrow(Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,c(1:6)]), ~ as.vector(X_Cbriggsae_Vg_P8p_SS_mod %*% Cbriggsae_Vg_P8p_SS_mod[["Sol"]][,c(1:6)][., ]))
      
      data_Cbriggsae_Vg_P8p_SS_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_Vg_P8p_SS_mod,
                      var.a = Cbriggsae_Vg_P8p_SS_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_Vg_P8p_SS_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_Vg_P8p_SS_mod <- data_Cbriggsae_Vg_P8p_SS_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_Vg_P8p_SS_mod <- data_Cbriggsae_Vg_P8p_SS_mod[["mean.obs"]]
      va_data_Cbriggsae_Vg_P8p_SS_mod <- data_Cbriggsae_Vg_P8p_SS_mod[["var.a.obs"]]
      vp_data_Cbriggsae_Vg_P8p_SS_mod <- data_Cbriggsae_Vg_P8p_SS_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_Vg_P8p_SS_mod <- (va_data_Cbriggsae_Vg_P8p_SS_mod/2) / (trait_mean_data_Cbriggsae_Vg_P8p_SS_mod)^2
      
      mean(h2_data_Cbriggsae_Vg_P8p_SS_mod)
      mean(trait_mean_data_Cbriggsae_Vg_P8p_SS_mod)
      mean(va_data_Cbriggsae_Vg_P8p_SS_mod)
      mean(vp_data_Cbriggsae_Vg_P8p_SS_mod)
      mean(Evol_data_Cbriggsae_Vg_P8p_SS_mod)
      
    }
    
  }
  
 ## Cbriggsae P5p ----
  
  
  #Cbriggsae_CONTROL_P5p_wt_mod 
  {
    
    Cbriggsae_CONTROL_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + Ancestral -1,
                                             random      = ~ LineB + BlockRep,
                                             family      = "threshold",
                                             data        = Vg_Cbriggsae_bi_CONTROL,
                                             prior       = prior_bi_block,
                                             nitt        = 1260000,       
                                             thin        = 500,           
                                             burnin      = 10000,
                                             trunc       = TRUE,
                                             pr          = TRUE,
                                             pl          = TRUE)            
    
    saveRDS(Cbriggsae_CONTROL_P5p_wt_mod, file = "Cbriggsae_CONTROL_P5p_wt_mod.rds")
    Cbriggsae_CONTROL_P5p_wt_mod <- readRDS("Cbriggsae_CONTROL_P5p_wt_mod.rds")
    
    summary(Cbriggsae_CONTROL_P5p_wt_mod) 
    plotTrace(Cbriggsae_CONTROL_P5p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Cbriggsae_CONTROL_P5p_wt_mod.pdf")
    plot(Cbriggsae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Cbriggsae_CONTROL_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Cbriggsae_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Cbriggsae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Cbriggsae_CONTROL_P5p_wt_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Cbriggsae_CONTROL_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_CONTROL_P5p_wt_mod <- Cbriggsae_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_CONTROL_P5p_wt_mod <- Cbriggsae_CONTROL_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_CONTROL_P5p_wt_mod <- rowSums(Cbriggsae_CONTROL_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_CONTROL_P5p_wt_mod) 
      HPDinterval(va_liab_Cbriggsae_CONTROL_P5p_wt_mod) 
      
      mean(vlat_Cbriggsae_CONTROL_P5p_wt_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_CONTROL_P5p_wt_mod <- Cbriggsae_CONTROL_P5p_wt_mod[["X"]]
      beta_Cbriggsae_CONTROL_P5p_wt_mod <- Cbriggsae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]
      vf_Cbriggsae_CONTROL_P5p_wt_mod   <- apply(beta_Cbriggsae_CONTROL_P5p_wt_mod, 1, function(b) {var(as.vector(X_Cbriggsae_CONTROL_P5p_wt_mod %*% b))}) 
      mean(vf_Cbriggsae_CONTROL_P5p_wt_mod) 
      
      h2_liab_Cbriggsae_CONTROL_P5p_wt_mod <- va_liab_Cbriggsae_CONTROL_P5p_wt_mod / (vlat_Cbriggsae_CONTROL_P5p_wt_mod + vf_Cbriggsae_CONTROL_P5p_wt_mod)
      mean(h2_liab_Cbriggsae_CONTROL_P5p_wt_mod) 
      posterior.mode(h2_liab_Cbriggsae_CONTROL_P5p_wt_mod)	
      median(h2_liab_Cbriggsae_CONTROL_P5p_wt_mod)		
      HPDinterval(h2_liab_Cbriggsae_CONTROL_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Cbriggsae_CONTROL_P5p_wt_mod <- ((rowMeans(Cbriggsae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Cbriggsae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:4)]) + Cbriggsae_CONTROL_P5p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Cbriggsae_CONTROL_P5p_wt_mod <- (va_liab_Cbriggsae_CONTROL_P5p_wt_mod/2) / (trait_mean_liab_Cbriggsae_CONTROL_P5p_wt_mod)^2
    mean(Evol_liab_Cbriggsae_CONTROL_P5p_wt_mod)
    
    
    #Cbriggsae_CONTROL_P5p_wt_mod data scale
    {
      
      predict_Cbriggsae_CONTROL_P5p_wt_mod <- map(1:nrow(Cbriggsae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Cbriggsae_CONTROL_P5p_wt_mod %*% Cbriggsae_CONTROL_P5p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Cbriggsae_CONTROL_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_CONTROL_P5p_wt_mod,
                      var.a = Cbriggsae_CONTROL_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_CONTROL_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_CONTROL_P5p_wt_mod <- data_Cbriggsae_CONTROL_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_CONTROL_P5p_wt_mod <- data_Cbriggsae_CONTROL_P5p_wt_mod[["mean.obs"]]
      va_data_Cbriggsae_CONTROL_P5p_wt_mod <- data_Cbriggsae_CONTROL_P5p_wt_mod[["var.a.obs"]]
      vp_data_Cbriggsae_CONTROL_P5p_wt_mod <- data_Cbriggsae_CONTROL_P5p_wt_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_CONTROL_P5p_wt_mod <- (va_data_Cbriggsae_CONTROL_P5p_wt_mod/2) / (trait_mean_data_Cbriggsae_CONTROL_P5p_wt_mod)^2
      
      mean(h2_data_Cbriggsae_CONTROL_P5p_wt_mod) 
      mean(trait_mean_data_Cbriggsae_CONTROL_P5p_wt_mod)
      mean(va_data_Cbriggsae_CONTROL_P5p_wt_mod) 
      mean(vp_data_Cbriggsae_CONTROL_P5p_wt_mod)
      mean(Evol_data_Cbriggsae_CONTROL_P5p_wt_mod)
      
    }
    
  }
  
  #Cbriggsae_WILD_P5p_wt_mod 
  {
    
    Cbriggsae_WILD_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer -1,
                                          random      = ~ LineB + BlockRep,
                                          family      = "threshold",
                                          data        = Vg_Cbriggsae_bi_WILD,
                                          prior       = prior_bi_block,
                                          nitt        = 1260000,       
                                          thin        = 500,           
                                          burnin      = 10000,
                                          trunc       = TRUE,
                                          pr          = TRUE,
                                          pl          = TRUE)         
    
    saveRDS(Cbriggsae_WILD_P5p_wt_mod, file = "Cbriggsae_WILD_P5p_wt_mod.rds")
    Cbriggsae_WILD_P5p_wt_mod <- readRDS("Cbriggsae_WILD_P5p_wt_mod.rds")
    
    summary(Cbriggsae_WILD_P5p_wt_mod) 
    #plot(Cbriggsae_WILD_P5p_wt_mod)
    
    # traces and posterior densities
    pdf("Cbriggsae_WILD_P5p_wt_mod.pdf")
    plot(Cbriggsae_WILD_P5p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(Cbriggsae_WILD_P5p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_WILD_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(Cbriggsae_WILD_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_WILD_P5p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(Cbriggsae_WILD_P5p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(Cbriggsae_WILD_P5p_wt_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_WILD_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_WILD_P5p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(Cbriggsae_WILD_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_WILD_P5p_wt_mod <- Cbriggsae_WILD_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_WILD_P5p_wt_mod <- Cbriggsae_WILD_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_WILD_P5p_wt_mod <- rowSums(Cbriggsae_WILD_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_WILD_P5p_wt_mod) 
      HPDinterval(va_liab_Cbriggsae_WILD_P5p_wt_mod) 
      
      mean(vlat_Cbriggsae_WILD_P5p_wt_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_WILD_P5p_wt_mod <- Cbriggsae_WILD_P5p_wt_mod[["X"]]
      beta_Cbriggsae_WILD_P5p_wt_mod <- Cbriggsae_WILD_P5p_wt_mod[["Sol"]][,c(1:4)]
      vf_Cbriggsae_WILD_P5p_wt_mod   <- apply(beta_Cbriggsae_WILD_P5p_wt_mod, 1, function(b) {var(as.vector(X_Cbriggsae_WILD_P5p_wt_mod %*% b))}) 
      mean(vf_Cbriggsae_WILD_P5p_wt_mod) 
      
      h2_liab_Cbriggsae_WILD_P5p_wt_mod <- va_liab_Cbriggsae_WILD_P5p_wt_mod / (vlat_Cbriggsae_WILD_P5p_wt_mod + vf_Cbriggsae_WILD_P5p_wt_mod)
      mean(h2_liab_Cbriggsae_WILD_P5p_wt_mod) 
      posterior.mode(h2_liab_Cbriggsae_WILD_P5p_wt_mod)	
      median(h2_liab_Cbriggsae_WILD_P5p_wt_mod)		
      HPDinterval(h2_liab_Cbriggsae_WILD_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Cbriggsae_WILD_P5p_wt_mod <- rowMeans(Cbriggsae_WILD_P5p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_Cbriggsae_WILD_P5p_wt_mod <- (va_liab_Cbriggsae_WILD_P5p_wt_mod/2) / (trait_mean_liab_Cbriggsae_WILD_P5p_wt_mod)^2
    mean(Evol_liab_Cbriggsae_WILD_P5p_wt_mod)
    
    
    #Cbriggsae_WILD_P5p_wt_mod data scale
    {
      
      predict_Cbriggsae_WILD_P5p_wt_mod <- map(1:nrow(Cbriggsae_WILD_P5p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_Cbriggsae_WILD_P5p_wt_mod %*% Cbriggsae_WILD_P5p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_Cbriggsae_WILD_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_WILD_P5p_wt_mod,
                      var.a = Cbriggsae_WILD_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_WILD_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_WILD_P5p_wt_mod <- data_Cbriggsae_WILD_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_WILD_P5p_wt_mod <- data_Cbriggsae_WILD_P5p_wt_mod[["mean.obs"]]
      va_data_Cbriggsae_WILD_P5p_wt_mod <- data_Cbriggsae_WILD_P5p_wt_mod[["var.a.obs"]]
      vp_data_Cbriggsae_WILD_P5p_wt_mod <- data_Cbriggsae_WILD_P5p_wt_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_WILD_P5p_wt_mod <- (va_data_Cbriggsae_WILD_P5p_wt_mod/2) / (trait_mean_data_Cbriggsae_WILD_P5p_wt_mod)^2
      
      mean(h2_data_Cbriggsae_WILD_P5p_wt_mod)
      mean(trait_mean_data_Cbriggsae_WILD_P5p_wt_mod)
      mean(va_data_Cbriggsae_WILD_P5p_wt_mod)
      mean(vp_data_Cbriggsae_WILD_P5p_wt_mod)
      mean(Evol_data_Cbriggsae_WILD_P5p_wt_mod)
      
    }
    
  }
  
  #Cbriggsae_Vg_P5p_wt_mod 
  {
    
    Cbriggsae_Vg_P5p_wt_mod <- MCMCglmm(fixed       = P5.p_wt ~ Observer + at.level(fTreatment,1):Ancestral + at.level(fTreatment,2):Ancestral -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vg_Cbriggsae_data,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)        
    
    saveRDS(Cbriggsae_Vg_P5p_wt_mod, file = "Cbriggsae_Vg_P5p_wt_mod.rds")
    Cbriggsae_Vg_P5p_wt_mod <- readRDS("Cbriggsae_Vg_P5p_wt_mod.rds")
    
    summary(Cbriggsae_Vg_P5p_wt_mod) 
    plotTrace(Cbriggsae_Vg_P5p_wt_mod$Sol)
    View(Cbriggsae_Vg_P5p_wt_mod[["Sol"]])
    # traces and posterior densities
    pdf("Cbriggsae_Vg_P5p_wt_mod.pdf")
    plot(Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,c(1:6)]) 
    plot(Cbriggsae_Vg_P5p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,c(1:6)]) #
    heidel.diag(Cbriggsae_Vg_P5p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,c(1:6)])
    #autocorr.plot(Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,c(1:6)]) #
    autocorr.diag(Cbriggsae_Vg_P5p_wt_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_Vg_P5p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,c(1:6)]) # 
    effectiveSize(Cbriggsae_Vg_P5p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_Vg_P5p_wt_mod <- Cbriggsae_Vg_P5p_wt_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_Vg_P5p_wt_mod <- Cbriggsae_Vg_P5p_wt_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_Vg_P5p_wt_mod <- rowSums(Cbriggsae_Vg_P5p_wt_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_Vg_P5p_wt_mod) 
      HPDinterval(va_liab_Cbriggsae_Vg_P5p_wt_mod) 
      
      mean(vlat_Cbriggsae_Vg_P5p_wt_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_Vg_P5p_wt_mod <- Cbriggsae_Vg_P5p_wt_mod[["X"]]
      beta_Cbriggsae_Vg_P5p_wt_mod <- Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,c(1:6)]
      vf_Cbriggsae_Vg_P5p_wt_mod   <- apply(beta_Cbriggsae_Vg_P5p_wt_mod, 1, function(b) {var(as.vector(X_Cbriggsae_Vg_P5p_wt_mod %*% b))}) 
      mean(vf_Cbriggsae_Vg_P5p_wt_mod) 
      
      h2_liab_Cbriggsae_Vg_P5p_wt_mod <- va_liab_Cbriggsae_Vg_P5p_wt_mod / (vlat_Cbriggsae_Vg_P5p_wt_mod + vf_Cbriggsae_Vg_P5p_wt_mod)
      mean(h2_liab_Cbriggsae_Vg_P5p_wt_mod) 
      posterior.mode(h2_liab_Cbriggsae_Vg_P5p_wt_mod)	
      median(h2_liab_Cbriggsae_Vg_P5p_wt_mod)		
      HPDinterval(h2_liab_Cbriggsae_Vg_P5p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,c(1:4)])) #Cbri WILD P5p_wt
    mean(rowMeans(Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,5]) #AF16 CONTROL P5p_wt
    mean(rowMeans(Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,6]) #PB800 Control P5p_wt
    
    trait_mean_liab_Cbriggsae_Vg_P5p_wt_mod <- ((rowMeans(Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,5]) + (rowMeans(Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,6]) )/3)
    
    Evol_liab_Cbriggsae_Vg_P5p_wt_mod <- (va_liab_Cbriggsae_Vg_P5p_wt_mod/2) / (trait_mean_liab_Cbriggsae_Vg_P5p_wt_mod)^2
    mean(Evol_liab_Cbriggsae_Vg_P5p_wt_mod)
    
    
    #Cbriggsae_Vg_P5p_wt_mod data scale
    {
      
      predict_Cbriggsae_Vg_P5p_wt_mod <- map(1:nrow(Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,c(1:6)]), ~ as.vector(X_Cbriggsae_Vg_P5p_wt_mod %*% Cbriggsae_Vg_P5p_wt_mod[["Sol"]][,c(1:6)][., ]))
      
      data_Cbriggsae_Vg_P5p_wt_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_Vg_P5p_wt_mod,
                      var.a = Cbriggsae_Vg_P5p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_Vg_P5p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_Vg_P5p_wt_mod <- data_Cbriggsae_Vg_P5p_wt_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_Vg_P5p_wt_mod <- data_Cbriggsae_Vg_P5p_wt_mod[["mean.obs"]]
      va_data_Cbriggsae_Vg_P5p_wt_mod <- data_Cbriggsae_Vg_P5p_wt_mod[["var.a.obs"]]
      vp_data_Cbriggsae_Vg_P5p_wt_mod <- data_Cbriggsae_Vg_P5p_wt_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_Vg_P5p_wt_mod <- (va_data_Cbriggsae_Vg_P5p_wt_mod/2) / (trait_mean_data_Cbriggsae_Vg_P5p_wt_mod)^2
      
      mean(h2_data_Cbriggsae_Vg_P5p_wt_mod)
      mean(trait_mean_data_Cbriggsae_Vg_P5p_wt_mod)
      mean(va_data_Cbriggsae_Vg_P5p_wt_mod)
      mean(vp_data_Cbriggsae_Vg_P5p_wt_mod)
      mean(Evol_data_Cbriggsae_Vg_P5p_wt_mod)
      
    }
    
  }
  
  
  
  ## Cbriggsae P6p ----
  
  
  #Cbriggsae_CONTROL_P6p_wt_mod 
  {
    
    Cbriggsae_CONTROL_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + Ancestral -1,
                                             random      = ~ LineB + BlockRep,
                                             family      = "threshold",
                                             data        = Vg_Cbriggsae_bi_CONTROL,
                                             prior       = prior_bi_block,
                                             nitt        = 1260000,       
                                             thin        = 500,           
                                             burnin      = 10000,
                                             trunc       = TRUE,
                                             pr          = TRUE,
                                             pl          = TRUE)            
    
    saveRDS(Cbriggsae_CONTROL_P6p_wt_mod, file = "Cbriggsae_CONTROL_P6p_wt_mod.rds")
    Cbriggsae_CONTROL_P6p_wt_mod <- readRDS("Cbriggsae_CONTROL_P6p_wt_mod.rds")
    
    summary(Cbriggsae_CONTROL_P6p_wt_mod) 
    plotTrace(Cbriggsae_CONTROL_P6p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Cbriggsae_CONTROL_P6p_wt_mod.pdf")
    plot(Cbriggsae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Cbriggsae_CONTROL_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Cbriggsae_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Cbriggsae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Cbriggsae_CONTROL_P6p_wt_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Cbriggsae_CONTROL_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_CONTROL_P6p_wt_mod <- Cbriggsae_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_CONTROL_P6p_wt_mod <- Cbriggsae_CONTROL_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_CONTROL_P6p_wt_mod <- rowSums(Cbriggsae_CONTROL_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_CONTROL_P6p_wt_mod) 
      HPDinterval(va_liab_Cbriggsae_CONTROL_P6p_wt_mod) 
      
      mean(vlat_Cbriggsae_CONTROL_P6p_wt_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_CONTROL_P6p_wt_mod <- Cbriggsae_CONTROL_P6p_wt_mod[["X"]]
      beta_Cbriggsae_CONTROL_P6p_wt_mod <- Cbriggsae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]
      vf_Cbriggsae_CONTROL_P6p_wt_mod   <- apply(beta_Cbriggsae_CONTROL_P6p_wt_mod, 1, function(b) {var(as.vector(X_Cbriggsae_CONTROL_P6p_wt_mod %*% b))}) 
      mean(vf_Cbriggsae_CONTROL_P6p_wt_mod) 
      
      h2_liab_Cbriggsae_CONTROL_P6p_wt_mod <- va_liab_Cbriggsae_CONTROL_P6p_wt_mod / (vlat_Cbriggsae_CONTROL_P6p_wt_mod + vf_Cbriggsae_CONTROL_P6p_wt_mod)
      mean(h2_liab_Cbriggsae_CONTROL_P6p_wt_mod) 
      posterior.mode(h2_liab_Cbriggsae_CONTROL_P6p_wt_mod)	
      median(h2_liab_Cbriggsae_CONTROL_P6p_wt_mod)		
      HPDinterval(h2_liab_Cbriggsae_CONTROL_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Cbriggsae_CONTROL_P6p_wt_mod <- ((rowMeans(Cbriggsae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Cbriggsae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:4)]) + Cbriggsae_CONTROL_P6p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Cbriggsae_CONTROL_P6p_wt_mod <- (va_liab_Cbriggsae_CONTROL_P6p_wt_mod/2) / (trait_mean_liab_Cbriggsae_CONTROL_P6p_wt_mod)^2
    mean(Evol_liab_Cbriggsae_CONTROL_P6p_wt_mod)
    
    
    #Cbriggsae_CONTROL_P6p_wt_mod data scale
    {
      
      predict_Cbriggsae_CONTROL_P6p_wt_mod <- map(1:nrow(Cbriggsae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Cbriggsae_CONTROL_P6p_wt_mod %*% Cbriggsae_CONTROL_P6p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Cbriggsae_CONTROL_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_CONTROL_P6p_wt_mod,
                      var.a = Cbriggsae_CONTROL_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_CONTROL_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_CONTROL_P6p_wt_mod <- data_Cbriggsae_CONTROL_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_CONTROL_P6p_wt_mod <- data_Cbriggsae_CONTROL_P6p_wt_mod[["mean.obs"]]
      va_data_Cbriggsae_CONTROL_P6p_wt_mod <- data_Cbriggsae_CONTROL_P6p_wt_mod[["var.a.obs"]]
      vp_data_Cbriggsae_CONTROL_P6p_wt_mod <- data_Cbriggsae_CONTROL_P6p_wt_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_CONTROL_P6p_wt_mod <- (va_data_Cbriggsae_CONTROL_P6p_wt_mod/2) / (trait_mean_data_Cbriggsae_CONTROL_P6p_wt_mod)^2
      
      mean(h2_data_Cbriggsae_CONTROL_P6p_wt_mod) 
      mean(trait_mean_data_Cbriggsae_CONTROL_P6p_wt_mod)
      mean(va_data_Cbriggsae_CONTROL_P6p_wt_mod) 
      mean(vp_data_Cbriggsae_CONTROL_P6p_wt_mod)
      mean(Evol_data_Cbriggsae_CONTROL_P6p_wt_mod)
      
    }
    
  }
  
  #Cbriggsae_WILD_P6p_wt_mod 
  {
    
    Cbriggsae_WILD_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer -1,
                                          random      = ~ LineB + BlockRep,
                                          family      = "threshold",
                                          data        = Vg_Cbriggsae_bi_WILD,
                                          prior       = prior_bi_block,
                                          nitt        = 1260000,       
                                          thin        = 500,           
                                          burnin      = 10000,
                                          trunc       = TRUE,
                                          pr          = TRUE,
                                          pl          = TRUE)         
    
    saveRDS(Cbriggsae_WILD_P6p_wt_mod, file = "Cbriggsae_WILD_P6p_wt_mod.rds")
    Cbriggsae_WILD_P6p_wt_mod <- readRDS("Cbriggsae_WILD_P6p_wt_mod.rds")
    
    summary(Cbriggsae_WILD_P6p_wt_mod) 
    #plot(Cbriggsae_WILD_P6p_wt_mod)
    
    # traces and posterior densities
    pdf("Cbriggsae_WILD_P6p_wt_mod.pdf")
    plot(Cbriggsae_WILD_P6p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(Cbriggsae_WILD_P6p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_WILD_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(Cbriggsae_WILD_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_WILD_P6p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(Cbriggsae_WILD_P6p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(Cbriggsae_WILD_P6p_wt_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_WILD_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_WILD_P6p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(Cbriggsae_WILD_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_WILD_P6p_wt_mod <- Cbriggsae_WILD_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_WILD_P6p_wt_mod <- Cbriggsae_WILD_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_WILD_P6p_wt_mod <- rowSums(Cbriggsae_WILD_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_WILD_P6p_wt_mod) 
      HPDinterval(va_liab_Cbriggsae_WILD_P6p_wt_mod) 
      
      mean(vlat_Cbriggsae_WILD_P6p_wt_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_WILD_P6p_wt_mod <- Cbriggsae_WILD_P6p_wt_mod[["X"]]
      beta_Cbriggsae_WILD_P6p_wt_mod <- Cbriggsae_WILD_P6p_wt_mod[["Sol"]][,c(1:4)]
      vf_Cbriggsae_WILD_P6p_wt_mod   <- apply(beta_Cbriggsae_WILD_P6p_wt_mod, 1, function(b) {var(as.vector(X_Cbriggsae_WILD_P6p_wt_mod %*% b))}) 
      mean(vf_Cbriggsae_WILD_P6p_wt_mod) 
      
      h2_liab_Cbriggsae_WILD_P6p_wt_mod <- va_liab_Cbriggsae_WILD_P6p_wt_mod / (vlat_Cbriggsae_WILD_P6p_wt_mod + vf_Cbriggsae_WILD_P6p_wt_mod)
      mean(h2_liab_Cbriggsae_WILD_P6p_wt_mod) 
      posterior.mode(h2_liab_Cbriggsae_WILD_P6p_wt_mod)	
      median(h2_liab_Cbriggsae_WILD_P6p_wt_mod)		
      HPDinterval(h2_liab_Cbriggsae_WILD_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Cbriggsae_WILD_P6p_wt_mod <- rowMeans(Cbriggsae_WILD_P6p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_Cbriggsae_WILD_P6p_wt_mod <- (va_liab_Cbriggsae_WILD_P6p_wt_mod/2) / (trait_mean_liab_Cbriggsae_WILD_P6p_wt_mod)^2
    mean(Evol_liab_Cbriggsae_WILD_P6p_wt_mod)
    
    
    #Cbriggsae_WILD_P6p_wt_mod data scale
    {
      
      predict_Cbriggsae_WILD_P6p_wt_mod <- map(1:nrow(Cbriggsae_WILD_P6p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_Cbriggsae_WILD_P6p_wt_mod %*% Cbriggsae_WILD_P6p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_Cbriggsae_WILD_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_WILD_P6p_wt_mod,
                      var.a = Cbriggsae_WILD_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_WILD_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_WILD_P6p_wt_mod <- data_Cbriggsae_WILD_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_WILD_P6p_wt_mod <- data_Cbriggsae_WILD_P6p_wt_mod[["mean.obs"]]
      va_data_Cbriggsae_WILD_P6p_wt_mod <- data_Cbriggsae_WILD_P6p_wt_mod[["var.a.obs"]]
      vp_data_Cbriggsae_WILD_P6p_wt_mod <- data_Cbriggsae_WILD_P6p_wt_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_WILD_P6p_wt_mod <- (va_data_Cbriggsae_WILD_P6p_wt_mod/2) / (trait_mean_data_Cbriggsae_WILD_P6p_wt_mod)^2
      
      mean(h2_data_Cbriggsae_WILD_P6p_wt_mod)
      mean(trait_mean_data_Cbriggsae_WILD_P6p_wt_mod)
      mean(va_data_Cbriggsae_WILD_P6p_wt_mod)
      mean(vp_data_Cbriggsae_WILD_P6p_wt_mod)
      mean(Evol_data_Cbriggsae_WILD_P6p_wt_mod)
      
    }
    
  }
  
  #Cbriggsae_Vg_P6p_wt_mod 
  {
    
    Cbriggsae_Vg_P6p_wt_mod <- MCMCglmm(fixed       = P6.p_wt ~ Observer + at.level(fTreatment,1):Ancestral + at.level(fTreatment,2):Ancestral -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vg_Cbriggsae_data,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)        
    
    
    saveRDS(Cbriggsae_Vg_P6p_wt_mod, file = "Cbriggsae_Vg_P6p_wt_mod.rds")
    Cbriggsae_Vg_P6p_wt_mod <- readRDS("Cbriggsae_Vg_P6p_wt_mod.rds")
    
    summary(Cbriggsae_Vg_P6p_wt_mod) 
    plotTrace(Cbriggsae_Vg_P6p_wt_mod$Sol)
    View(Cbriggsae_Vg_P6p_wt_mod[["Sol"]])
    # traces and posterior densities
    pdf("Cbriggsae_Vg_P6p_wt_mod.pdf")
    plot(Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,c(1:6)]) 
    plot(Cbriggsae_Vg_P6p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,c(1:6)]) #
    heidel.diag(Cbriggsae_Vg_P6p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,c(1:6)])
    #autocorr.plot(Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,c(1:6)]) #
    autocorr.diag(Cbriggsae_Vg_P6p_wt_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_Vg_P6p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,c(1:6)]) # 
    effectiveSize(Cbriggsae_Vg_P6p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_Vg_P6p_wt_mod <- Cbriggsae_Vg_P6p_wt_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_Vg_P6p_wt_mod <- Cbriggsae_Vg_P6p_wt_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_Vg_P6p_wt_mod <- rowSums(Cbriggsae_Vg_P6p_wt_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_Vg_P6p_wt_mod) 
      HPDinterval(va_liab_Cbriggsae_Vg_P6p_wt_mod) 
      
      mean(vlat_Cbriggsae_Vg_P6p_wt_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_Vg_P6p_wt_mod <- Cbriggsae_Vg_P6p_wt_mod[["X"]]
      beta_Cbriggsae_Vg_P6p_wt_mod <- Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,c(1:6)]
      vf_Cbriggsae_Vg_P6p_wt_mod   <- apply(beta_Cbriggsae_Vg_P6p_wt_mod, 1, function(b) {var(as.vector(X_Cbriggsae_Vg_P6p_wt_mod %*% b))}) 
      mean(vf_Cbriggsae_Vg_P6p_wt_mod) 
      
      h2_liab_Cbriggsae_Vg_P6p_wt_mod <- va_liab_Cbriggsae_Vg_P6p_wt_mod / (vlat_Cbriggsae_Vg_P6p_wt_mod + vf_Cbriggsae_Vg_P6p_wt_mod)
      mean(h2_liab_Cbriggsae_Vg_P6p_wt_mod) 
      posterior.mode(h2_liab_Cbriggsae_Vg_P6p_wt_mod)	
      median(h2_liab_Cbriggsae_Vg_P6p_wt_mod)		
      HPDinterval(h2_liab_Cbriggsae_Vg_P6p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,c(1:4)])) #Cbri WILD P6p_wt
    mean(rowMeans(Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,5]) #AF16 CONTROL P6p_wt
    mean(rowMeans(Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,6]) #PB800 Control P6p_wt
    
    trait_mean_liab_Cbriggsae_Vg_P6p_wt_mod <- ((rowMeans(Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,5]) + (rowMeans(Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,6]) )/3)
    
    Evol_liab_Cbriggsae_Vg_P6p_wt_mod <- (va_liab_Cbriggsae_Vg_P6p_wt_mod/2) / (trait_mean_liab_Cbriggsae_Vg_P6p_wt_mod)^2
    mean(Evol_liab_Cbriggsae_Vg_P6p_wt_mod)
    
    
    #Cbriggsae_Vg_P6p_wt_mod data scale
    {
      
      predict_Cbriggsae_Vg_P6p_wt_mod <- map(1:nrow(Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,c(1:6)]), ~ as.vector(X_Cbriggsae_Vg_P6p_wt_mod %*% Cbriggsae_Vg_P6p_wt_mod[["Sol"]][,c(1:6)][., ]))
      
      data_Cbriggsae_Vg_P6p_wt_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_Vg_P6p_wt_mod,
                      var.a = Cbriggsae_Vg_P6p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_Vg_P6p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_Vg_P6p_wt_mod <- data_Cbriggsae_Vg_P6p_wt_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_Vg_P6p_wt_mod <- data_Cbriggsae_Vg_P6p_wt_mod[["mean.obs"]]
      va_data_Cbriggsae_Vg_P6p_wt_mod <- data_Cbriggsae_Vg_P6p_wt_mod[["var.a.obs"]]
      vp_data_Cbriggsae_Vg_P6p_wt_mod <- data_Cbriggsae_Vg_P6p_wt_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_Vg_P6p_wt_mod <- (va_data_Cbriggsae_Vg_P6p_wt_mod/2) / (trait_mean_data_Cbriggsae_Vg_P6p_wt_mod)^2
      
      mean(h2_data_Cbriggsae_Vg_P6p_wt_mod)
      mean(trait_mean_data_Cbriggsae_Vg_P6p_wt_mod)
      mean(va_data_Cbriggsae_Vg_P6p_wt_mod)
      mean(vp_data_Cbriggsae_Vg_P6p_wt_mod)
      mean(Evol_data_Cbriggsae_Vg_P6p_wt_mod)
      
    }
    
  }
  
  
  
  ## Cbriggsae P7p ----
  
  
  #Cbriggsae_CONTROL_P7p_wt_mod 
  {
    
    Cbriggsae_CONTROL_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + Ancestral -1,
                                             random      = ~ LineB + BlockRep,
                                             family      = "threshold",
                                             data        = Vg_Cbriggsae_bi_CONTROL,
                                             prior       = prior_bi_block,
                                             nitt        = 1260000,       
                                             thin        = 500,           
                                             burnin      = 10000,
                                             trunc       = TRUE,
                                             pr          = TRUE,
                                             pl          = TRUE)            
    
    saveRDS(Cbriggsae_CONTROL_P7p_wt_mod, file = "Cbriggsae_CONTROL_P7p_wt_mod.rds")
    Cbriggsae_CONTROL_P7p_wt_mod <- readRDS("Cbriggsae_CONTROL_P7p_wt_mod.rds")
    
    summary(Cbriggsae_CONTROL_P7p_wt_mod) 
    plotTrace(Cbriggsae_CONTROL_P7p_wt_mod$Sol)
    
    # traces and posterior densities
    
    pdf("Cbriggsae_CONTROL_P7p_wt_mod.pdf")
    plot(Cbriggsae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]) 
    plot(Cbriggsae_CONTROL_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    heidel.diag(Cbriggsae_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)])
    #autocorr.plot(Cbriggsae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]) #
    autocorr.diag(Cbriggsae_CONTROL_P7p_wt_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]) # 
    effectiveSize(Cbriggsae_CONTROL_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_CONTROL_P7p_wt_mod <- Cbriggsae_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_CONTROL_P7p_wt_mod <- Cbriggsae_CONTROL_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_CONTROL_P7p_wt_mod <- rowSums(Cbriggsae_CONTROL_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_CONTROL_P7p_wt_mod) 
      HPDinterval(va_liab_Cbriggsae_CONTROL_P7p_wt_mod) 
      
      mean(vlat_Cbriggsae_CONTROL_P7p_wt_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_CONTROL_P7p_wt_mod <- Cbriggsae_CONTROL_P7p_wt_mod[["X"]]
      beta_Cbriggsae_CONTROL_P7p_wt_mod <- Cbriggsae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]
      vf_Cbriggsae_CONTROL_P7p_wt_mod   <- apply(beta_Cbriggsae_CONTROL_P7p_wt_mod, 1, function(b) {var(as.vector(X_Cbriggsae_CONTROL_P7p_wt_mod %*% b))}) 
      mean(vf_Cbriggsae_CONTROL_P7p_wt_mod) 
      
      h2_liab_Cbriggsae_CONTROL_P7p_wt_mod <- va_liab_Cbriggsae_CONTROL_P7p_wt_mod / (vlat_Cbriggsae_CONTROL_P7p_wt_mod + vf_Cbriggsae_CONTROL_P7p_wt_mod)
      mean(h2_liab_Cbriggsae_CONTROL_P7p_wt_mod) 
      posterior.mode(h2_liab_Cbriggsae_CONTROL_P7p_wt_mod)	
      median(h2_liab_Cbriggsae_CONTROL_P7p_wt_mod)		
      HPDinterval(h2_liab_Cbriggsae_CONTROL_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Cbriggsae_CONTROL_P7p_wt_mod <- ((rowMeans(Cbriggsae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Cbriggsae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:4)]) + Cbriggsae_CONTROL_P7p_wt_mod[["Sol"]][,5]))/2)
    
    Evol_liab_Cbriggsae_CONTROL_P7p_wt_mod <- (va_liab_Cbriggsae_CONTROL_P7p_wt_mod/2) / (trait_mean_liab_Cbriggsae_CONTROL_P7p_wt_mod)^2
    mean(Evol_liab_Cbriggsae_CONTROL_P7p_wt_mod)
    
    
    #Cbriggsae_CONTROL_P7p_wt_mod data scale
    {
      
      predict_Cbriggsae_CONTROL_P7p_wt_mod <- map(1:nrow(Cbriggsae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)]), ~ as.vector(X_Cbriggsae_CONTROL_P7p_wt_mod %*% Cbriggsae_CONTROL_P7p_wt_mod[["Sol"]][,c(1:5)][., ]))
      
      data_Cbriggsae_CONTROL_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_CONTROL_P7p_wt_mod,
                      var.a = Cbriggsae_CONTROL_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_CONTROL_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_CONTROL_P7p_wt_mod <- data_Cbriggsae_CONTROL_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_CONTROL_P7p_wt_mod <- data_Cbriggsae_CONTROL_P7p_wt_mod[["mean.obs"]]
      va_data_Cbriggsae_CONTROL_P7p_wt_mod <- data_Cbriggsae_CONTROL_P7p_wt_mod[["var.a.obs"]]
      vp_data_Cbriggsae_CONTROL_P7p_wt_mod <- data_Cbriggsae_CONTROL_P7p_wt_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_CONTROL_P7p_wt_mod <- (va_data_Cbriggsae_CONTROL_P7p_wt_mod/2) / (trait_mean_data_Cbriggsae_CONTROL_P7p_wt_mod)^2
      
      mean(h2_data_Cbriggsae_CONTROL_P7p_wt_mod) 
      mean(trait_mean_data_Cbriggsae_CONTROL_P7p_wt_mod)
      mean(va_data_Cbriggsae_CONTROL_P7p_wt_mod) 
      mean(vp_data_Cbriggsae_CONTROL_P7p_wt_mod)
      mean(Evol_data_Cbriggsae_CONTROL_P7p_wt_mod)
      
    }
    
  }
  
  #Cbriggsae_WILD_P7p_wt_mod 
  {
    
    Cbriggsae_WILD_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer -1,
                                          random      = ~ LineB + BlockRep,
                                          family      = "threshold",
                                          data        = Vg_Cbriggsae_bi_WILD,
                                          prior       = prior_bi_block,
                                          nitt        = 1260000,       
                                          thin        = 500,           
                                          burnin      = 10000,
                                          trunc       = TRUE,
                                          pr          = TRUE,
                                          pl          = TRUE)         
    
    saveRDS(Cbriggsae_WILD_P7p_wt_mod, file = "Cbriggsae_WILD_P7p_wt_mod.rds")
    Cbriggsae_WILD_P7p_wt_mod <- readRDS("Cbriggsae_WILD_P7p_wt_mod.rds")
    
    summary(Cbriggsae_WILD_P7p_wt_mod) 
    #plot(Cbriggsae_WILD_P7p_wt_mod)
    
    # traces and posterior densities
    pdf("Cbriggsae_WILD_P7p_wt_mod.pdf")
    plot(Cbriggsae_WILD_P7p_wt_mod[["Sol"]][,c(1:4)]) 
    plot(Cbriggsae_WILD_P7p_wt_mod[["VCV"]])  
    dev.off()
    
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_WILD_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    heidel.diag(Cbriggsae_WILD_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_WILD_P7p_wt_mod[["Sol"]][,c(1:4)])
    #autocorr.plot(Cbriggsae_WILD_P7p_wt_mod[["Sol"]][,c(1:4)]) #
    autocorr.diag(Cbriggsae_WILD_P7p_wt_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_WILD_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_WILD_P7p_wt_mod[["Sol"]][,c(1:4)]) # 
    effectiveSize(Cbriggsae_WILD_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_WILD_P7p_wt_mod <- Cbriggsae_WILD_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_WILD_P7p_wt_mod <- Cbriggsae_WILD_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_WILD_P7p_wt_mod <- rowSums(Cbriggsae_WILD_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_WILD_P7p_wt_mod) 
      HPDinterval(va_liab_Cbriggsae_WILD_P7p_wt_mod) 
      
      mean(vlat_Cbriggsae_WILD_P7p_wt_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_WILD_P7p_wt_mod <- Cbriggsae_WILD_P7p_wt_mod[["X"]]
      beta_Cbriggsae_WILD_P7p_wt_mod <- Cbriggsae_WILD_P7p_wt_mod[["Sol"]][,c(1:4)]
      vf_Cbriggsae_WILD_P7p_wt_mod   <- apply(beta_Cbriggsae_WILD_P7p_wt_mod, 1, function(b) {var(as.vector(X_Cbriggsae_WILD_P7p_wt_mod %*% b))}) 
      mean(vf_Cbriggsae_WILD_P7p_wt_mod) 
      
      h2_liab_Cbriggsae_WILD_P7p_wt_mod <- va_liab_Cbriggsae_WILD_P7p_wt_mod / (vlat_Cbriggsae_WILD_P7p_wt_mod + vf_Cbriggsae_WILD_P7p_wt_mod)
      mean(h2_liab_Cbriggsae_WILD_P7p_wt_mod) 
      posterior.mode(h2_liab_Cbriggsae_WILD_P7p_wt_mod)	
      median(h2_liab_Cbriggsae_WILD_P7p_wt_mod)		
      HPDinterval(h2_liab_Cbriggsae_WILD_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Cbriggsae_WILD_P7p_wt_mod <- rowMeans(Cbriggsae_WILD_P7p_wt_mod[["Sol"]][,c(1:4)])
    
    Evol_liab_Cbriggsae_WILD_P7p_wt_mod <- (va_liab_Cbriggsae_WILD_P7p_wt_mod/2) / (trait_mean_liab_Cbriggsae_WILD_P7p_wt_mod)^2
    mean(Evol_liab_Cbriggsae_WILD_P7p_wt_mod)
    
    
    #Cbriggsae_WILD_P7p_wt_mod data scale
    {
      
      predict_Cbriggsae_WILD_P7p_wt_mod <- map(1:nrow(Cbriggsae_WILD_P7p_wt_mod[["Sol"]][,c(1:4)]), ~ as.vector(X_Cbriggsae_WILD_P7p_wt_mod %*% Cbriggsae_WILD_P7p_wt_mod[["Sol"]][,c(1:4)][., ]))
      
      data_Cbriggsae_WILD_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_WILD_P7p_wt_mod,
                      var.a = Cbriggsae_WILD_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_WILD_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_WILD_P7p_wt_mod <- data_Cbriggsae_WILD_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_WILD_P7p_wt_mod <- data_Cbriggsae_WILD_P7p_wt_mod[["mean.obs"]]
      va_data_Cbriggsae_WILD_P7p_wt_mod <- data_Cbriggsae_WILD_P7p_wt_mod[["var.a.obs"]]
      vp_data_Cbriggsae_WILD_P7p_wt_mod <- data_Cbriggsae_WILD_P7p_wt_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_WILD_P7p_wt_mod <- (va_data_Cbriggsae_WILD_P7p_wt_mod/2) / (trait_mean_data_Cbriggsae_WILD_P7p_wt_mod)^2
      
      mean(h2_data_Cbriggsae_WILD_P7p_wt_mod)
      mean(trait_mean_data_Cbriggsae_WILD_P7p_wt_mod)
      mean(va_data_Cbriggsae_WILD_P7p_wt_mod)
      mean(vp_data_Cbriggsae_WILD_P7p_wt_mod)
      mean(Evol_data_Cbriggsae_WILD_P7p_wt_mod)
      
    }
    
  }
  
  #Cbriggsae_Vg_P7p_wt_mod 
  {
    
    Cbriggsae_Vg_P7p_wt_mod <- MCMCglmm(fixed       = P7.p_wt ~ Observer + at.level(fTreatment,1):Ancestral + at.level(fTreatment,2):Ancestral -1,
                                        random      = ~ LineB + BlockRep,
                                        family      = "threshold",
                                        data        = Vg_Cbriggsae_data,
                                        prior       = prior_bi_block,
                                        nitt        = 1260000,       
                                        thin        = 500,           
                                        burnin      = 10000,
                                        trunc       = TRUE,
                                        pr          = TRUE,
                                        pl          = TRUE)        
    
    
    saveRDS(Cbriggsae_Vg_P7p_wt_mod, file = "Cbriggsae_Vg_P7p_wt_mod.rds")
    Cbriggsae_Vg_P7p_wt_mod <- readRDS("Cbriggsae_Vg_P7p_wt_mod.rds")
    
    summary(Cbriggsae_Vg_P7p_wt_mod) 
    plotTrace(Cbriggsae_Vg_P7p_wt_mod$Sol)
    View(Cbriggsae_Vg_P7p_wt_mod[["Sol"]])
    # traces and posterior densities
    pdf("Cbriggsae_Vg_P7p_wt_mod.pdf")
    plot(Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,c(1:6)]) 
    plot(Cbriggsae_Vg_P7p_wt_mod[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,c(1:6)]) #
    heidel.diag(Cbriggsae_Vg_P7p_wt_mod[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,c(1:6)])
    #autocorr.plot(Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,c(1:6)]) #
    autocorr.diag(Cbriggsae_Vg_P7p_wt_mod[["VCV"]])
    #autocorr.plot(Cbriggsae_Vg_P7p_wt_mod[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,c(1:6)]) # 
    effectiveSize(Cbriggsae_Vg_P7p_wt_mod[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Cbriggsae_Vg_P7p_wt_mod <- Cbriggsae_Vg_P7p_wt_mod[["VCV"]][ , "LineB"]
      vr_Cbriggsae_Vg_P7p_wt_mod <- Cbriggsae_Vg_P7p_wt_mod[["VCV"]][ , "units"]
      vlat_Cbriggsae_Vg_P7p_wt_mod <- rowSums(Cbriggsae_Vg_P7p_wt_mod[["VCV"]])
      
      mean(va_liab_Cbriggsae_Vg_P7p_wt_mod) 
      HPDinterval(va_liab_Cbriggsae_Vg_P7p_wt_mod) 
      
      mean(vlat_Cbriggsae_Vg_P7p_wt_mod) 
      
      #variance of fixed effects
      X_Cbriggsae_Vg_P7p_wt_mod <- Cbriggsae_Vg_P7p_wt_mod[["X"]]
      beta_Cbriggsae_Vg_P7p_wt_mod <- Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,c(1:6)]
      vf_Cbriggsae_Vg_P7p_wt_mod   <- apply(beta_Cbriggsae_Vg_P7p_wt_mod, 1, function(b) {var(as.vector(X_Cbriggsae_Vg_P7p_wt_mod %*% b))}) 
      mean(vf_Cbriggsae_Vg_P7p_wt_mod) 
      
      h2_liab_Cbriggsae_Vg_P7p_wt_mod <- va_liab_Cbriggsae_Vg_P7p_wt_mod / (vlat_Cbriggsae_Vg_P7p_wt_mod + vf_Cbriggsae_Vg_P7p_wt_mod)
      mean(h2_liab_Cbriggsae_Vg_P7p_wt_mod) 
      posterior.mode(h2_liab_Cbriggsae_Vg_P7p_wt_mod)	
      median(h2_liab_Cbriggsae_Vg_P7p_wt_mod)		
      HPDinterval(h2_liab_Cbriggsae_Vg_P7p_wt_mod)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    mean(rowMeans(Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,c(1:4)])) #Cbri WILD P7p_wt
    mean(rowMeans(Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,5]) #AF16 CONTROL P7p_wt
    mean(rowMeans(Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,6]) #PB800 Control P7p_wt
    
    trait_mean_liab_Cbriggsae_Vg_P7p_wt_mod <- ((rowMeans(Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,c(1:4)]) + (rowMeans(Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,5]) + (rowMeans(Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,c(1:4)]) + Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,6]) )/3)
    
    Evol_liab_Cbriggsae_Vg_P7p_wt_mod <- (va_liab_Cbriggsae_Vg_P7p_wt_mod/2) / (trait_mean_liab_Cbriggsae_Vg_P7p_wt_mod)^2
    mean(Evol_liab_Cbriggsae_Vg_P7p_wt_mod)
    
    
    #Cbriggsae_Vg_P7p_wt_mod data scale
    {
      
      predict_Cbriggsae_Vg_P7p_wt_mod <- map(1:nrow(Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,c(1:6)]), ~ as.vector(X_Cbriggsae_Vg_P7p_wt_mod %*% Cbriggsae_Vg_P7p_wt_mod[["Sol"]][,c(1:6)][., ]))
      
      data_Cbriggsae_Vg_P7p_wt_mod <-
        pmap_dfr(list(predict = predict_Cbriggsae_Vg_P7p_wt_mod,
                      var.a = Cbriggsae_Vg_P7p_wt_mod[["VCV"]][ , "LineB"],
                      var.p = rowSums(Cbriggsae_Vg_P7p_wt_mod[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Cbriggsae_Vg_P7p_wt_mod <- data_Cbriggsae_Vg_P7p_wt_mod[["h2.obs"]]
      trait_mean_data_Cbriggsae_Vg_P7p_wt_mod <- data_Cbriggsae_Vg_P7p_wt_mod[["mean.obs"]]
      va_data_Cbriggsae_Vg_P7p_wt_mod <- data_Cbriggsae_Vg_P7p_wt_mod[["var.a.obs"]]
      vp_data_Cbriggsae_Vg_P7p_wt_mod <- data_Cbriggsae_Vg_P7p_wt_mod[["var.obs"]]
      
      Evol_data_Cbriggsae_Vg_P7p_wt_mod <- (va_data_Cbriggsae_Vg_P7p_wt_mod/2) / (trait_mean_data_Cbriggsae_Vg_P7p_wt_mod)^2
      
      mean(h2_data_Cbriggsae_Vg_P7p_wt_mod)
      mean(trait_mean_data_Cbriggsae_Vg_P7p_wt_mod)
      mean(va_data_Cbriggsae_Vg_P7p_wt_mod)
      mean(vp_data_Cbriggsae_Vg_P7p_wt_mod)
      mean(Evol_data_Cbriggsae_Vg_P7p_wt_mod)
      
    }
    
  }
  
  








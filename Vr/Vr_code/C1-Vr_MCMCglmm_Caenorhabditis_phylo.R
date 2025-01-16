#Vr_Caenorhabditis
library(readxl)
library(Matrix)
library(ggplot2)
library(coda)       # <- to handle, explore and diagnose MCMC chains
library(ape)
library(MCMCglmm)
library(QGglmm)
library(writexl)
library(plotMCMC)
library(ggdark)
library(phytools)
library(purrr)
library(reshape2) 
library(graph4lg)
library(writexl)
library(plotMCMC)
library("postMCMCglmm")

Vr_data <- as.data.frame(read_xlsx("Data S1 phenotyping species diversity.xlsx"))
View(Vr_data)
dim(Vr_data)
Vr_data$Species_replicate <- paste(Vr_data$Species,Vr_data$Replicate)
Vr_data$Species_phylo <- paste(Vr_data$Species)

table(Vr_data$Variance)
table(Vr_data$Genus)

Vr_Caenorhabditis_data <- subset(Vr_data, Genus =="Caenorhabditis")
table(Vr_Caenorhabditis_data$Species)
length(table(Vr_Caenorhabditis_data$Species))
length(table(Vr_Caenorhabditis_data$Line))

View(Vr_Caenorhabditis_data)
table(Vr_Caenorhabditis_data$Observer)
table(Vr_Caenorhabditis_data$P3.p)
table(Vr_Caenorhabditis_data$P3.p_multinomial)
table(Vr_Caenorhabditis_data$P3.p_S)
table(Vr_Caenorhabditis_data$P3.p_SS)
table(Vr_Caenorhabditis_data$P4.p)
table(Vr_Caenorhabditis_data$P4.p_multinomial)
table(Vr_Caenorhabditis_data$P4.p_S)
table(Vr_Caenorhabditis_data$P4.p_SS)
table(Vr_Caenorhabditis_data$P8.p)
table(Vr_Caenorhabditis_data$P8.p_multinomial)
table(Vr_Caenorhabditis_data$P8.p_S)
table(Vr_Caenorhabditis_data$P8.p_SS)
table(Vr_Caenorhabditis_data$P8.p_Other)


#Priors
{
  prior_bi_G3 <-
    list( R = list(V = 1, fix = 1),      # Fixing the "residual" variance to 1 because it is not identifiable in binary responses
          G = list(G1 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1),
                   G2 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1),
                   G3 = list(V = 1, nu = 1000, alpha.mu = 0, alpha.V = 1)))
  
}



#Caenorhabditis_phylogeny
{
  Caenorhabditis_chronogram<-read.nexus("Chronogram-Caenorhabditis.nex")
  plot(Caenorhabditis_chronogram)
  is.ultrametric(Caenorhabditis_chronogram) 
  
  Caenorhabditis_chronogram_ultra <- force.ultrametric(Caenorhabditis_chronogram, method="extend")
  is.ultrametric(Caenorhabditis_chronogram_ultra)
  plot(Caenorhabditis_chronogram_ultra)
  
  writeNexus(Caenorhabditis_chronogram_ultra, file="Caenorhabditis_chronogram_ultra")
  
  
  inv.Caenorhabditis_chronogram_scaled<-inverseA(Caenorhabditis_chronogram_ultra,nodes="ALL",scale=TRUE)
  
} 



#----Vr_Caenorhabditis P3p----
{
  
  
  ##Vr_Caenorhabditis_P3p_SS_mod3_scaled - 
  {
    
    Vr_Caenorhabditis_P3p_SS_mod3_scaled <- MCMCglmm(fixed       = P3.p_SS ~  1,
                                                                       random      = ~ Species_phylo + Species + Species_replicate,
                                                                       family      = "threshold",
                                                                       data        = Vr_Caenorhabditis_data,
                                                                       ginverse    = list(Species_phylo=inv.Caenorhabditis_chronogram_scaled$Ainv),
                                                                       prior       = prior_bi_G3,
                                                                       nitt        = 5010000,       
                                                                       thin        = 2000,           
                                                                       burnin      = 10000,
                                                                       trunc       = TRUE,
                                                                       pr          = TRUE,
                                                                       pl          = TRUE)    
    
    
    
    
    saveRDS(Vr_Caenorhabditis_P3p_SS_mod3_scaled, file = "Vr_Caenorhabditis_P3p_SS_mod3_scaled.rds")
    Vr_Caenorhabditis_P3p_SS_mod3_scaled <- readRDS("Vr_Caenorhabditis_P3p_SS_mod3_scaled.rds")
    
    summary(Vr_Caenorhabditis_P3p_SS_mod3_scaled) 
    plotTrace(Vr_Caenorhabditis_P3p_SS_mod3_scaled$Sol)
    
    # traces and posterior densities
    pdf("Vr_Caenorhabditis_P3p_SS_mod3_scaled.pdf")
    plot(Vr_Caenorhabditis_P3p_SS_mod3_scaled[["Sol"]][,1]) 
    plot(Vr_Caenorhabditis_P3p_SS_mod3_scaled[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Vr_Caenorhabditis_P3p_SS_mod3_scaled[["Sol"]][,1]) #
    heidel.diag(Vr_Caenorhabditis_P3p_SS_mod3_scaled[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Vr_Caenorhabditis_P3p_SS_mod3_scaled[["Sol"]][,1])
    #autocorr.plot(Vr_Caenorhabditis_P3p_SS_mod3_scaled[["Sol"]][,1]) #
    autocorr.diag(Vr_Caenorhabditis_P3p_SS_mod3_scaled[["VCV"]])
    #autocorr.plot(Vr_Caenorhabditis_P3p_SS_mod3_scaled[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Vr_Caenorhabditis_P3p_SS_mod3_scaled[["Sol"]][,1]) # 
    effectiveSize(Vr_Caenorhabditis_P3p_SS_mod3_scaled[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled <- Vr_Caenorhabditis_P3p_SS_mod3_scaled[["VCV"]][ , "Species_phylo"]
      vr_Vr_Caenorhabditis_P3p_SS_mod3_scaled <- Vr_Caenorhabditis_P3p_SS_mod3_scaled[["VCV"]][ , "units"]
      vlat_Vr_Caenorhabditis_P3p_SS_mod3_scaled <- rowSums(Vr_Caenorhabditis_P3p_SS_mod3_scaled[["VCV"]])
      
      mean(va_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled) 
      HPDinterval(va_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled) 
      
      mean(vlat_Vr_Caenorhabditis_P3p_SS_mod3_scaled) 
      
      #variance of fixed effects
      X_Vr_Caenorhabditis_P3p_SS_mod3_scaled <- Vr_Caenorhabditis_P3p_SS_mod3_scaled[["X"]]
      beta_Vr_Caenorhabditis_P3p_SS_mod3_scaled <- as.data.frame(Vr_Caenorhabditis_P3p_SS_mod3_scaled[["Sol"]][,1])
      vf_Vr_Caenorhabditis_P3p_SS_mod3_scaled   <- apply(beta_Vr_Caenorhabditis_P3p_SS_mod3_scaled, 1, function(b) {var(as.vector(X_Vr_Caenorhabditis_P3p_SS_mod3_scaled %*% b))}) 
      mean(vf_Vr_Caenorhabditis_P3p_SS_mod3_scaled) 
      
      h2_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled <- va_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled / (vlat_Vr_Caenorhabditis_P3p_SS_mod3_scaled + vf_Vr_Caenorhabditis_P3p_SS_mod3_scaled)
      mean(h2_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled) 
      posterior.mode(h2_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled)	
      median(h2_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled)		
      HPDinterval(h2_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled<- Vr_Caenorhabditis_P3p_SS_mod3_scaled[["Sol"]][,1]
    
    #NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.Since the Tree has been scaled to 1,it's divided by 106.30 to give Va per million years
    
    Evol_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled<- (va_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30)) / (trait_mean_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled)^2
    mean(Evol_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled)
   
    
    #Vr_Caenorhabditis_P3p_SS_mod3_scaled  data scale
    {
      
      predict_Vr_Caenorhabditis_P3p_SS_mod3_scaled <- map(1:nrow(beta_Vr_Caenorhabditis_P3p_SS_mod3_scaled), ~ as.vector(X_Vr_Caenorhabditis_P3p_SS_mod3_scaled %*% beta_Vr_Caenorhabditis_P3p_SS_mod3_scaled[., ]))
      
      data_Vr_Caenorhabditis_P3p_SS_mod3_scaled <-
        pmap_dfr(list(predict = predict_Vr_Caenorhabditis_P3p_SS_mod3_scaled,
                      var.a = Vr_Caenorhabditis_P3p_SS_mod3_scaled[["VCV"]][ , "Species_phylo"],
                      var.p = rowSums(Vr_Caenorhabditis_P3p_SS_mod3_scaled[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled <- data_Vr_Caenorhabditis_P3p_SS_mod3_scaled[["h2.obs"]]
      trait_mean_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled <- data_Vr_Caenorhabditis_P3p_SS_mod3_scaled[["mean.obs"]]
      va_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled <- data_Vr_Caenorhabditis_P3p_SS_mod3_scaled[["var.a.obs"]]
      vp_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled <- data_Vr_Caenorhabditis_P3p_SS_mod3_scaled[["var.obs"]]
      
      Evol_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled <- (va_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30)) / (trait_mean_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled)^2
      
      mean(h2_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled)
      mean(trait_mean_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled)
      mean(va_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled)
      mean(vp_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled)
      mean(Evol_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled)
      
    }
    
    va_NotPhylo_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled <- (Vr_Caenorhabditis_P3p_SS_mod3_scaled[["VCV"]][ , "Species"])
    
    
    #Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled  data scale
    {
      
      predict_Vr_Caenorhabditis_P3p_SS_mod3_scaled <- map(1:nrow(beta_Vr_Caenorhabditis_P3p_SS_mod3_scaled), ~ as.vector(X_Vr_Caenorhabditis_P3p_SS_mod3_scaled %*% beta_Vr_Caenorhabditis_P3p_SS_mod3_scaled[., ]))
      
     data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled <-
        pmap_dfr(list(predict = predict_Vr_Caenorhabditis_P3p_SS_mod3_scaled,
                      var.a = Vr_Caenorhabditis_P3p_SS_mod3_scaled[["VCV"]][ , "Species"],
                      var.p = rowSums(Vr_Caenorhabditis_P3p_SS_mod3_scaled[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled[["h2.obs"]]
      trait_mean_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled[["mean.obs"]]
      va_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled[["var.a.obs"]]
      vp_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled[["var.obs"]]
      
      Evol_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled <- (va_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30)) / (trait_mean_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled)^2
      
      mean(h2_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled)
      mean(trait_mean_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled)
      mean(va_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled)
      mean(vp_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled)
      mean(Evol_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled)
      
      
    }
    
  }
  
  
  
}

#----Vr_Caenorhabditis P4p----
{
  
  
  #Vr_Caenorhabditis_P4p_SS_mod3_scaled - 
  {
    
    Vr_Caenorhabditis_P4p_SS_mod3_scaled <- MCMCglmm(fixed       = P4.p_SS ~  1,
                                                                       random      = ~ Species_phylo + Species + Species_replicate,
                                                                       family      = "threshold",
                                                                       ginverse    = list(Species_phylo=inv.Caenorhabditis_chronogram_scaled$Ainv),
                                                                       data        = Vr_Caenorhabditis_data,
                                                                       prior       = prior_bi_G3,
                                                                       nitt        = 5010000,       
                                                                       thin        = 2000,           
                                                                       burnin      = 10000,
                                                                       trunc       = TRUE,
                                                                       pr          = TRUE,
                                                                       pl          = TRUE)               
    
    saveRDS(Vr_Caenorhabditis_P4p_SS_mod3_scaled, file = "Vr_Caenorhabditis_P4p_SS_mod3_scaled.rds")
    Vr_Caenorhabditis_P4p_SS_mod3_scaled <- readRDS("Vr_Caenorhabditis_P4p_SS_mod3_scaled.rds")
    
    summary(Vr_Caenorhabditis_P4p_SS_mod3_scaled) 
    plotTrace(Vr_Caenorhabditis_P4p_SS_mod3_scaled$Sol)
    
    # traces and posterior densities
    pdf("Vr_Caenorhabditis_P4p_SS_mod3_scaled.pdf")
    plot(Vr_Caenorhabditis_P4p_SS_mod3_scaled[["Sol"]][,1]) 
    plot(Vr_Caenorhabditis_P4p_SS_mod3_scaled[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Vr_Caenorhabditis_P4p_SS_mod3_scaled[["Sol"]][,1]) #
    heidel.diag(Vr_Caenorhabditis_P4p_SS_mod3_scaled[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Vr_Caenorhabditis_P4p_SS_mod3_scaled[["Sol"]][,1])
    #autocorr.plot(Vr_Caenorhabditis_P4p_SS_mod3_scaled[["Sol"]][,1]) #
    autocorr.diag(Vr_Caenorhabditis_P4p_SS_mod3_scaled[["VCV"]])
    #autocorr.plot(Vr_Caenorhabditis_P4p_SS_mod3_scaled[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Vr_Caenorhabditis_P4p_SS_mod3_scaled[["Sol"]][,1]) # 
    effectiveSize(Vr_Caenorhabditis_P4p_SS_mod3_scaled[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled <- Vr_Caenorhabditis_P4p_SS_mod3_scaled[["VCV"]][ , "Species_phylo"]
      vr_Vr_Caenorhabditis_P4p_SS_mod3_scaled <- Vr_Caenorhabditis_P4p_SS_mod3_scaled[["VCV"]][ , "units"]
      vlat_Vr_Caenorhabditis_P4p_SS_mod3_scaled <- rowSums(Vr_Caenorhabditis_P4p_SS_mod3_scaled[["VCV"]])
      
      mean(va_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled) 
      HPDinterval(va_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled) 
      
      mean(vlat_Vr_Caenorhabditis_P4p_SS_mod3_scaled) 
      
      #variance of fixed effects
      X_Vr_Caenorhabditis_P4p_SS_mod3_scaled <- Vr_Caenorhabditis_P4p_SS_mod3_scaled[["X"]]
      beta_Vr_Caenorhabditis_P4p_SS_mod3_scaled <- as.data.frame(Vr_Caenorhabditis_P4p_SS_mod3_scaled[["Sol"]][,1])
      vf_Vr_Caenorhabditis_P4p_SS_mod3_scaled   <- apply(beta_Vr_Caenorhabditis_P4p_SS_mod3_scaled, 1, function(b) {var(as.vector(X_Vr_Caenorhabditis_P4p_SS_mod3_scaled %*% b))}) 
      mean(vf_Vr_Caenorhabditis_P4p_SS_mod3_scaled) 
      
      h2_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled <- va_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled / (vlat_Vr_Caenorhabditis_P4p_SS_mod3_scaled + vf_Vr_Caenorhabditis_P4p_SS_mod3_scaled)
      mean(h2_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled) 
      posterior.mode(h2_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled)	
      median(h2_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled)		
      HPDinterval(h2_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled<- Vr_Caenorhabditis_P4p_SS_mod3_scaled[["Sol"]][,1]
    
    #NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.Since the Tree has been scaled to 1,it's divided by 106.30 to give Va per million years
    
    Evol_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled<- (va_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30)) / (trait_mean_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled)^2
    mean(Evol_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled)
    
    #Vr_Caenorhabditis_P4p_SS_mod3_scaled  data scale
    {
      
      predict_Vr_Caenorhabditis_P4p_SS_mod3_scaled <- map(1:nrow(beta_Vr_Caenorhabditis_P4p_SS_mod3_scaled), ~ as.vector(X_Vr_Caenorhabditis_P4p_SS_mod3_scaled %*% beta_Vr_Caenorhabditis_P4p_SS_mod3_scaled[., ]))
      
      data_Vr_Caenorhabditis_P4p_SS_mod3_scaled <-
        pmap_dfr(list(predict = predict_Vr_Caenorhabditis_P4p_SS_mod3_scaled,
                      var.a = Vr_Caenorhabditis_P4p_SS_mod3_scaled[["VCV"]][ , "Species_phylo"],
                      var.p = rowSums(Vr_Caenorhabditis_P4p_SS_mod3_scaled[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled <- data_Vr_Caenorhabditis_P4p_SS_mod3_scaled[["h2.obs"]]
      trait_mean_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled <- data_Vr_Caenorhabditis_P4p_SS_mod3_scaled[["mean.obs"]]
      va_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled <- data_Vr_Caenorhabditis_P4p_SS_mod3_scaled[["var.a.obs"]]
      vp_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled <- data_Vr_Caenorhabditis_P4p_SS_mod3_scaled[["var.obs"]]
      
      Evol_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled <- (va_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30)) / (trait_mean_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled)^2
      
      mean(h2_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled)
      mean(trait_mean_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled)
      mean(va_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled)
      mean(vp_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled)
      mean(Evol_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled)
      
    }
    
    va_NotPhylo_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled <- (Vr_Caenorhabditis_P4p_SS_mod3_scaled[["VCV"]][ , "Species"])
    #Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled  data scale
    {
      
      predict_Vr_Caenorhabditis_P4p_SS_mod3_scaled <- map(1:nrow(beta_Vr_Caenorhabditis_P4p_SS_mod3_scaled), ~ as.vector(X_Vr_Caenorhabditis_P4p_SS_mod3_scaled %*% beta_Vr_Caenorhabditis_P4p_SS_mod3_scaled[., ]))
      
     data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled <-
        pmap_dfr(list(predict = predict_Vr_Caenorhabditis_P4p_SS_mod3_scaled,
                      var.a = Vr_Caenorhabditis_P4p_SS_mod3_scaled[["VCV"]][ , "Species"],
                      var.p = rowSums(Vr_Caenorhabditis_P4p_SS_mod3_scaled[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled[["h2.obs"]]
      trait_mean_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled[["mean.obs"]]
      va_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled[["var.a.obs"]]
      vp_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled[["var.obs"]]
      
      Evol_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled <- (va_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30)) / (trait_mean_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled)^2
      
      mean(h2_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled)
      mean(trait_mean_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled)
      mean(va_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled)
      mean(vp_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled)
      mean(Evol_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled)
      
      
    }
    
  }
  
  
}

#----Vr_Caenorhabditis P8p----
{
  
  #Vr_Caenorhabditis_P8p_SS_mod3_scaled - 
  {
    
    Vr_Caenorhabditis_P8p_SS_mod3_scaled <- MCMCglmm(fixed       = P8.p_SS ~  1,
                                                                       random      = ~ Species_phylo + Species + Species_replicate,
                                                                       family      = "threshold",
                                                                       ginverse    = list(Species_phylo=inv.Caenorhabditis_chronogram_scaled$Ainv),
                                                                       data        = Vr_Caenorhabditis_data,
                                                                       prior       = prior_bi_G3,
                                                                       nitt        = 5010000,       
                                                                       thin        = 2000,           
                                                                       burnin      = 10000,
                                                                       trunc       = TRUE,
                                                                       pr          = TRUE,
                                                                       pl          = TRUE)               
    
    saveRDS(Vr_Caenorhabditis_P8p_SS_mod3_scaled, file = "Vr_Caenorhabditis_P8p_SS_mod3_scaled.rds")
    Vr_Caenorhabditis_P8p_SS_mod3_scaled <- readRDS("Vr_Caenorhabditis_P8p_SS_mod3_scaled.rds")
    
    summary(Vr_Caenorhabditis_P8p_SS_mod3_scaled) 
    plotTrace(Vr_Caenorhabditis_P8p_SS_mod3_scaled$Sol)
    
    # traces and posterior densities
    pdf("Vr_Caenorhabditis_P8p_SS_mod3_scaled.pdf")
    plot(Vr_Caenorhabditis_P8p_SS_mod3_scaled[["Sol"]][,1]) 
    plot(Vr_Caenorhabditis_P8p_SS_mod3_scaled[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Vr_Caenorhabditis_P8p_SS_mod3_scaled[["Sol"]][,1]) #
    heidel.diag(Vr_Caenorhabditis_P8p_SS_mod3_scaled[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Vr_Caenorhabditis_P8p_SS_mod3_scaled[["Sol"]][,1])
    #autocorr.plot(Vr_Caenorhabditis_P8p_SS_mod3_scaled[["Sol"]][,1]) #
    autocorr.diag(Vr_Caenorhabditis_P8p_SS_mod3_scaled[["VCV"]])
    #autocorr.plot(Vr_Caenorhabditis_P8p_SS_mod3_scaled[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Vr_Caenorhabditis_P8p_SS_mod3_scaled[["Sol"]][,1]) # 
    effectiveSize(Vr_Caenorhabditis_P8p_SS_mod3_scaled[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled <- Vr_Caenorhabditis_P8p_SS_mod3_scaled[["VCV"]][ , "Species_phylo"]
      vr_Vr_Caenorhabditis_P8p_SS_mod3_scaled <- Vr_Caenorhabditis_P8p_SS_mod3_scaled[["VCV"]][ , "units"]
      vlat_Vr_Caenorhabditis_P8p_SS_mod3_scaled <- rowSums(Vr_Caenorhabditis_P8p_SS_mod3_scaled[["VCV"]])
      
      mean(va_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled) 
      HPDinterval(va_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled) 
      
      mean(vlat_Vr_Caenorhabditis_P8p_SS_mod3_scaled) 
      
      #variance of fixed effects
      X_Vr_Caenorhabditis_P8p_SS_mod3_scaled <- Vr_Caenorhabditis_P8p_SS_mod3_scaled[["X"]]
      beta_Vr_Caenorhabditis_P8p_SS_mod3_scaled <- as.data.frame(Vr_Caenorhabditis_P8p_SS_mod3_scaled[["Sol"]][,1])
      vf_Vr_Caenorhabditis_P8p_SS_mod3_scaled   <- apply(beta_Vr_Caenorhabditis_P8p_SS_mod3_scaled, 1, function(b) {var(as.vector(X_Vr_Caenorhabditis_P8p_SS_mod3_scaled %*% b))}) 
      mean(vf_Vr_Caenorhabditis_P8p_SS_mod3_scaled) 
      
      h2_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled <- va_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled / (vlat_Vr_Caenorhabditis_P8p_SS_mod3_scaled + vf_Vr_Caenorhabditis_P8p_SS_mod3_scaled)
      mean(h2_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled) 
      posterior.mode(h2_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled)	
      median(h2_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled)		
      HPDinterval(h2_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled<- Vr_Caenorhabditis_P8p_SS_mod3_scaled[["Sol"]][,1]
    
    #NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.Since the Tree has been scaled to 1,it's divided by 106.30 to give Va per million years
    
    Evol_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled<- (va_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30)) / (trait_mean_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled)^2
    mean(Evol_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled)
    
    
    #Vr_Caenorhabditis_P8p_SS_mod3_scaled  data scale
    {
      
      predict_Vr_Caenorhabditis_P8p_SS_mod3_scaled <- map(1:nrow(beta_Vr_Caenorhabditis_P8p_SS_mod3_scaled), ~ as.vector(X_Vr_Caenorhabditis_P8p_SS_mod3_scaled %*% beta_Vr_Caenorhabditis_P8p_SS_mod3_scaled[., ]))
      
      data_Vr_Caenorhabditis_P8p_SS_mod3_scaled <-
        pmap_dfr(list(predict = predict_Vr_Caenorhabditis_P8p_SS_mod3_scaled,
                      var.a = Vr_Caenorhabditis_P8p_SS_mod3_scaled[["VCV"]][ , "Species_phylo"],
                      var.p = rowSums(Vr_Caenorhabditis_P8p_SS_mod3_scaled[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled <- data_Vr_Caenorhabditis_P8p_SS_mod3_scaled[["h2.obs"]]
      trait_mean_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled <- data_Vr_Caenorhabditis_P8p_SS_mod3_scaled[["mean.obs"]]
      va_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled <- data_Vr_Caenorhabditis_P8p_SS_mod3_scaled[["var.a.obs"]]
      vp_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled <- data_Vr_Caenorhabditis_P8p_SS_mod3_scaled[["var.obs"]]
      
      Evol_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled <- (va_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30)) / (trait_mean_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled)^2
      
      mean(h2_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled)
      mean(trait_mean_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled)
      mean(va_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled)
      mean(vp_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled)
      mean(Evol_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled)
      
    }
    
    va_NotPhylo_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled <- (Vr_Caenorhabditis_P8p_SS_mod3_scaled[["VCV"]][ , "Species"])
    
    
    #Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled  data scale
    {
      
      predict_Vr_Caenorhabditis_P8p_SS_mod3_scaled <- map(1:nrow(beta_Vr_Caenorhabditis_P8p_SS_mod3_scaled), ~ as.vector(X_Vr_Caenorhabditis_P8p_SS_mod3_scaled %*% beta_Vr_Caenorhabditis_P8p_SS_mod3_scaled[., ]))
      
     data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled <-
        pmap_dfr(list(predict = predict_Vr_Caenorhabditis_P8p_SS_mod3_scaled,
                      var.a = Vr_Caenorhabditis_P8p_SS_mod3_scaled[["VCV"]][ , "Species"],
                      var.p = rowSums(Vr_Caenorhabditis_P8p_SS_mod3_scaled[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled[["h2.obs"]]
      trait_mean_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled[["mean.obs"]]
      va_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled[["var.a.obs"]]
      vp_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled[["var.obs"]]
      
      Evol_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled <- (va_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30)) / (trait_mean_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled)^2
      
      mean(h2_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled)
      mean(trait_mean_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled)
      mean(va_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled)
      mean(vp_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled)
      mean(Evol_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled)
      
      
    }
  }
  
  
}

#----Vr_Caenorhabditis_vulva ----
{
  
  ##Vr_Caenorhabditis_P5p_wt_mod3_scaled - 
  {
    
    Vr_Caenorhabditis_P5p_wt_mod3_scaled <- MCMCglmm(fixed       = P5.p_wt ~  1,
                                                                       random      = ~ Species_phylo + Species + Species_replicate,
                                                                       family      = "threshold",
                                                                       data        = Vr_Caenorhabditis_data,
                                                                       ginverse    = list(Species_phylo=inv.Caenorhabditis_chronogram_scaled$Ainv),
                                                                       prior       = prior_bi_G3,
                                                                       nitt        = 5010000,       
                                                                       thin        = 2000,           
                                                                       burnin      = 10000,
                                                                       trunc       = TRUE,
                                                                       pr          = TRUE,
                                                                       pl          = TRUE)    
    
    
    
    
    saveRDS(Vr_Caenorhabditis_P5p_wt_mod3_scaled, file = "Vr_Caenorhabditis_P5p_wt_mod3_scaled.rds")
    Vr_Caenorhabditis_P5p_wt_mod3_scaled <- readRDS("Vr_Caenorhabditis_P5p_wt_mod3_scaled.rds")
    
    summary(Vr_Caenorhabditis_P5p_wt_mod3_scaled) 
    plotTrace(Vr_Caenorhabditis_P5p_wt_mod3_scaled$Sol)
    
    # traces and posterior densities
    pdf("Vr_Caenorhabditis_P5p_wt_mod3_scaled.pdf")
    plot(Vr_Caenorhabditis_P5p_wt_mod3_scaled[["Sol"]][,1]) 
    plot(Vr_Caenorhabditis_P5p_wt_mod3_scaled[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Vr_Caenorhabditis_P5p_wt_mod3_scaled[["Sol"]][,1]) #
    heidel.diag(Vr_Caenorhabditis_P5p_wt_mod3_scaled[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Vr_Caenorhabditis_P5p_wt_mod3_scaled[["Sol"]][,1])
    #autocorr.plot(Vr_Caenorhabditis_P5p_wt_mod3_scaled[["Sol"]][,1]) #
    autocorr.diag(Vr_Caenorhabditis_P5p_wt_mod3_scaled[["VCV"]])
    #autocorr.plot(Vr_Caenorhabditis_P5p_wt_mod3_scaled[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Vr_Caenorhabditis_P5p_wt_mod3_scaled[["Sol"]][,1]) # 
    effectiveSize(Vr_Caenorhabditis_P5p_wt_mod3_scaled[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled <- Vr_Caenorhabditis_P5p_wt_mod3_scaled[["VCV"]][ , "Species_phylo"]
      vr_Vr_Caenorhabditis_P5p_wt_mod3_scaled <- Vr_Caenorhabditis_P5p_wt_mod3_scaled[["VCV"]][ , "units"]
      vlat_Vr_Caenorhabditis_P5p_wt_mod3_scaled <- rowSums(Vr_Caenorhabditis_P5p_wt_mod3_scaled[["VCV"]])
      
      mean(va_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled) 
      HPDinterval(va_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled) 
      
      mean(vlat_Vr_Caenorhabditis_P5p_wt_mod3_scaled) 
      
      #variance of fixed effects
      X_Vr_Caenorhabditis_P5p_wt_mod3_scaled <- Vr_Caenorhabditis_P5p_wt_mod3_scaled[["X"]]
      beta_Vr_Caenorhabditis_P5p_wt_mod3_scaled <- as.data.frame(Vr_Caenorhabditis_P5p_wt_mod3_scaled[["Sol"]][,1])
      vf_Vr_Caenorhabditis_P5p_wt_mod3_scaled   <- apply(beta_Vr_Caenorhabditis_P5p_wt_mod3_scaled, 1, function(b) {var(as.vector(X_Vr_Caenorhabditis_P5p_wt_mod3_scaled %*% b))}) 
      mean(vf_Vr_Caenorhabditis_P5p_wt_mod3_scaled) 
      
      h2_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled <- va_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled / (vlat_Vr_Caenorhabditis_P5p_wt_mod3_scaled + vf_Vr_Caenorhabditis_P5p_wt_mod3_scaled)
      mean(h2_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled) 
      posterior.mode(h2_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled)	
      median(h2_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled)		
      HPDinterval(h2_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled<- Vr_Caenorhabditis_P5p_wt_mod3_scaled[["Sol"]][,1]
    
    #NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.Since the Tree has been scaled to 1,it's divided by 106.30 to give Va per million years
    
    Evol_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled<- (va_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30)) / (trait_mean_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled)^2
    mean(Evol_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled)
    
    
    #Vr_Caenorhabditis_P5p_wt_mod3_scaled  data scale
    {
      
      predict_Vr_Caenorhabditis_P5p_wt_mod3_scaled <- map(1:nrow(beta_Vr_Caenorhabditis_P5p_wt_mod3_scaled), ~ as.vector(X_Vr_Caenorhabditis_P5p_wt_mod3_scaled %*% beta_Vr_Caenorhabditis_P5p_wt_mod3_scaled[., ]))
      
      data_Vr_Caenorhabditis_P5p_wt_mod3_scaled <-
        pmap_dfr(list(predict = predict_Vr_Caenorhabditis_P5p_wt_mod3_scaled,
                      var.a = Vr_Caenorhabditis_P5p_wt_mod3_scaled[["VCV"]][ , "Species_phylo"],
                      var.p = rowSums(Vr_Caenorhabditis_P5p_wt_mod3_scaled[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled <- data_Vr_Caenorhabditis_P5p_wt_mod3_scaled[["h2.obs"]]
      trait_mean_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled <- data_Vr_Caenorhabditis_P5p_wt_mod3_scaled[["mean.obs"]]
      va_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled <- data_Vr_Caenorhabditis_P5p_wt_mod3_scaled[["var.a.obs"]]
      vp_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled <- data_Vr_Caenorhabditis_P5p_wt_mod3_scaled[["var.obs"]]
      
      Evol_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled <- (va_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30)) / (trait_mean_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled)^2
      
      mean(h2_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled)
      mean(trait_mean_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled)
      mean(va_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled)
      mean(vp_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled)
      mean(Evol_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled)
      
    }
    
    va_NotPhylo_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled <- (Vr_Caenorhabditis_P5p_wt_mod3_scaled[["VCV"]][ , "Species"])
    
    
    #Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled  data scale
    {
      
      predict_Vr_Caenorhabditis_P5p_wt_mod3_scaled <- map(1:nrow(beta_Vr_Caenorhabditis_P5p_wt_mod3_scaled), ~ as.vector(X_Vr_Caenorhabditis_P5p_wt_mod3_scaled %*% beta_Vr_Caenorhabditis_P5p_wt_mod3_scaled[., ]))
      
     data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled <-
        pmap_dfr(list(predict = predict_Vr_Caenorhabditis_P5p_wt_mod3_scaled,
                      var.a = Vr_Caenorhabditis_P5p_wt_mod3_scaled[["VCV"]][ , "Species"],
                      var.p = rowSums(Vr_Caenorhabditis_P5p_wt_mod3_scaled[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled[["h2.obs"]]
      trait_mean_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled[["mean.obs"]]
      va_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled[["var.a.obs"]]
      vp_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled[["var.obs"]]
      
      Evol_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled <- (va_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30)) / (trait_mean_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled)^2
      
      mean(h2_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled)
      mean(trait_mean_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled)
      mean(va_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled)
      mean(vp_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled)
      mean(Evol_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled)
      
      
    }
    
  }
  
  #Vr_Caenorhabditis_P6p_wt_mod3_scaled - 
  {
    
    Vr_Caenorhabditis_P6p_wt_mod3_scaled <- MCMCglmm(fixed       = P6.p_wt ~  1,
                                                                       random      = ~ Species_phylo + Species + Species_replicate,
                                                                       family      = "threshold",
                                                                       ginverse    = list(Species_phylo=inv.Caenorhabditis_chronogram_scaled$Ainv),
                                                                       data        = Vr_Caenorhabditis_data,
                                                                       prior       = prior_bi_G3,
                                                                       nitt        = 5010000,       
                                                                       thin        = 2000,           
                                                                       burnin      = 10000,
                                                                       trunc       = TRUE,
                                                                       pr          = TRUE,
                                                                       pl          = TRUE)               
    
    saveRDS(Vr_Caenorhabditis_P6p_wt_mod3_scaled, file = "Vr_Caenorhabditis_P6p_wt_mod3_scaled.rds")
    Vr_Caenorhabditis_P6p_wt_mod3_scaled <- readRDS("Vr_Caenorhabditis_P6p_wt_mod3_scaled.rds")
    
    summary(Vr_Caenorhabditis_P6p_wt_mod3_scaled) 
    plotTrace(Vr_Caenorhabditis_P6p_wt_mod3_scaled$Sol)
    
    # traces and posterior densities
    pdf("Vr_Caenorhabditis_P6p_wt_mod3_scaled.pdf")
    plot(Vr_Caenorhabditis_P6p_wt_mod3_scaled[["Sol"]][,1]) 
    plot(Vr_Caenorhabditis_P6p_wt_mod3_scaled[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Vr_Caenorhabditis_P6p_wt_mod3_scaled[["Sol"]][,1]) #
    heidel.diag(Vr_Caenorhabditis_P6p_wt_mod3_scaled[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Vr_Caenorhabditis_P6p_wt_mod3_scaled[["Sol"]][,1])
    #autocorr.plot(Vr_Caenorhabditis_P6p_wt_mod3_scaled[["Sol"]][,1]) #
    autocorr.diag(Vr_Caenorhabditis_P6p_wt_mod3_scaled[["VCV"]])
    #autocorr.plot(Vr_Caenorhabditis_P6p_wt_mod3_scaled[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Vr_Caenorhabditis_P6p_wt_mod3_scaled[["Sol"]][,1]) # 
    effectiveSize(Vr_Caenorhabditis_P6p_wt_mod3_scaled[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled <- Vr_Caenorhabditis_P6p_wt_mod3_scaled[["VCV"]][ , "Species_phylo"]
      vr_Vr_Caenorhabditis_P6p_wt_mod3_scaled <- Vr_Caenorhabditis_P6p_wt_mod3_scaled[["VCV"]][ , "units"]
      vlat_Vr_Caenorhabditis_P6p_wt_mod3_scaled <- rowSums(Vr_Caenorhabditis_P6p_wt_mod3_scaled[["VCV"]])
      
      mean(va_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled) 
      HPDinterval(va_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled) 
      
      mean(vlat_Vr_Caenorhabditis_P6p_wt_mod3_scaled) 
      
      #variance of fixed effects
      X_Vr_Caenorhabditis_P6p_wt_mod3_scaled <- Vr_Caenorhabditis_P6p_wt_mod3_scaled[["X"]]
      beta_Vr_Caenorhabditis_P6p_wt_mod3_scaled <- as.data.frame(Vr_Caenorhabditis_P6p_wt_mod3_scaled[["Sol"]][,1])
      vf_Vr_Caenorhabditis_P6p_wt_mod3_scaled   <- apply(beta_Vr_Caenorhabditis_P6p_wt_mod3_scaled, 1, function(b) {var(as.vector(X_Vr_Caenorhabditis_P6p_wt_mod3_scaled %*% b))}) 
      mean(vf_Vr_Caenorhabditis_P6p_wt_mod3_scaled) 
      
      h2_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled <- va_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled / (vlat_Vr_Caenorhabditis_P6p_wt_mod3_scaled + vf_Vr_Caenorhabditis_P6p_wt_mod3_scaled)
      mean(h2_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled) 
      posterior.mode(h2_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled)	
      median(h2_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled)		
      HPDinterval(h2_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    trait_mean_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled<- Vr_Caenorhabditis_P6p_wt_mod3_scaled[["Sol"]][,1]
    
    #NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.Since the Tree has been scaled to 1,it's divided by 106.30 to give Va per million years
    
    Evol_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled<- (va_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30)) / (trait_mean_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled)^2
    mean(Evol_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled)
    
    #Vr_Caenorhabditis_P6p_wt_mod3_scaled  data scale
    {
      
      predict_Vr_Caenorhabditis_P6p_wt_mod3_scaled <- map(1:nrow(beta_Vr_Caenorhabditis_P6p_wt_mod3_scaled), ~ as.vector(X_Vr_Caenorhabditis_P6p_wt_mod3_scaled %*% beta_Vr_Caenorhabditis_P6p_wt_mod3_scaled[., ]))
      
      data_Vr_Caenorhabditis_P6p_wt_mod3_scaled <-
        pmap_dfr(list(predict = predict_Vr_Caenorhabditis_P6p_wt_mod3_scaled,
                      var.a = Vr_Caenorhabditis_P6p_wt_mod3_scaled[["VCV"]][ , "Species_phylo"],
                      var.p = rowSums(Vr_Caenorhabditis_P6p_wt_mod3_scaled[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled <- data_Vr_Caenorhabditis_P6p_wt_mod3_scaled[["h2.obs"]]
      trait_mean_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled <- data_Vr_Caenorhabditis_P6p_wt_mod3_scaled[["mean.obs"]]
      va_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled <- data_Vr_Caenorhabditis_P6p_wt_mod3_scaled[["var.a.obs"]]
      vp_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled <- data_Vr_Caenorhabditis_P6p_wt_mod3_scaled[["var.obs"]]
      
      Evol_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled <- (va_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30)) / (trait_mean_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled)^2
      
      mean(h2_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled)
      mean(trait_mean_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled)
      mean(va_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled)
      mean(vp_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled)
      mean(Evol_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled)
      
    }
    
    va_NotPhylo_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled <- (Vr_Caenorhabditis_P6p_wt_mod3_scaled[["VCV"]][ , "Species"])
    
    
    #Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled  data scale
    {
      
      predict_Vr_Caenorhabditis_P6p_wt_mod3_scaled <- map(1:nrow(beta_Vr_Caenorhabditis_P6p_wt_mod3_scaled), ~ as.vector(X_Vr_Caenorhabditis_P6p_wt_mod3_scaled %*% beta_Vr_Caenorhabditis_P6p_wt_mod3_scaled[., ]))
      
     data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled <-
        pmap_dfr(list(predict = predict_Vr_Caenorhabditis_P6p_wt_mod3_scaled,
                      var.a = Vr_Caenorhabditis_P6p_wt_mod3_scaled[["VCV"]][ , "Species"],
                      var.p = rowSums(Vr_Caenorhabditis_P6p_wt_mod3_scaled[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled[["h2.obs"]]
      trait_mean_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled[["mean.obs"]]
      va_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled[["var.a.obs"]]
      vp_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled[["var.obs"]]
      
      Evol_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled <- (va_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30)) / (trait_mean_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled)^2
      
      mean(h2_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled)
      mean(trait_mean_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled)
      mean(va_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled)
      mean(vp_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled)
      mean(Evol_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled)
      
      
    }
    
  }
  
  #Vr_Caenorhabditis_P7p_wt_mod3_scaled - 
  {
    
    Vr_Caenorhabditis_P7p_wt_mod3_scaled <- MCMCglmm(fixed       = P7.p_wt ~  1,
                                                                       random      = ~ Species_phylo + Species + Species_replicate,
                                                                       family      = "threshold",
                                                                       ginverse    = list(Species_phylo=inv.Caenorhabditis_chronogram_scaled$Ainv),
                                                                       data        = Vr_Caenorhabditis_data,
                                                                       prior       = prior_bi_G3,
                                                                       nitt        = 5010000,       
                                                                       thin        = 2000,           
                                                                       burnin      = 10000,
                                                                       trunc       = TRUE,
                                                                       pr          = TRUE,
                                                                       pl          = TRUE)               
    
    saveRDS(Vr_Caenorhabditis_P7p_wt_mod3_scaled, file = "Vr_Caenorhabditis_P7p_wt_mod3_scaled.rds")
    Vr_Caenorhabditis_P7p_wt_mod3_scaled <- readRDS("Vr_Caenorhabditis_P7p_wt_mod3_scaled.rds")
    
    summary(Vr_Caenorhabditis_P7p_wt_mod3_scaled) 
    plotTrace(Vr_Caenorhabditis_P7p_wt_mod3_scaled$Sol)
    
    # traces and posterior densities
    pdf("Vr_Caenorhabditis_P7p_wt_mod3_scaled.pdf")
    plot(Vr_Caenorhabditis_P7p_wt_mod3_scaled[["Sol"]][,1]) 
    plot(Vr_Caenorhabditis_P7p_wt_mod3_scaled[["VCV"]])  
    dev.off() 
    
    # Heidelberger & Welsh test to check convergence
    heidel.diag(Vr_Caenorhabditis_P7p_wt_mod3_scaled[["Sol"]][,1]) #
    heidel.diag(Vr_Caenorhabditis_P7p_wt_mod3_scaled[["VCV"]]) #
    
    #  autocorrelation and effective sample size
    autocorr.diag(Vr_Caenorhabditis_P7p_wt_mod3_scaled[["Sol"]][,1])
    #autocorr.plot(Vr_Caenorhabditis_P7p_wt_mod3_scaled[["Sol"]][,1]) #
    autocorr.diag(Vr_Caenorhabditis_P7p_wt_mod3_scaled[["VCV"]])
    #autocorr.plot(Vr_Caenorhabditis_P7p_wt_mod3_scaled[["VCV"]]) #
    
    # effective sample size (note that this is also provided by MCMCglmm)
    effectiveSize(Vr_Caenorhabditis_P7p_wt_mod3_scaled[["Sol"]][,1]) # 
    effectiveSize(Vr_Caenorhabditis_P7p_wt_mod3_scaled[["VCV"]]) #
    
    ##heritability h² = va / (vf + vlat), where vlat = va+vr
    {
      
      va_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled <- Vr_Caenorhabditis_P7p_wt_mod3_scaled[["VCV"]][ , "Species_phylo"]
      vr_Vr_Caenorhabditis_P7p_wt_mod3_scaled <- Vr_Caenorhabditis_P7p_wt_mod3_scaled[["VCV"]][ , "units"]
      vlat_Vr_Caenorhabditis_P7p_wt_mod3_scaled <- rowSums(Vr_Caenorhabditis_P7p_wt_mod3_scaled[["VCV"]])
      
      mean(va_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled) 
      HPDinterval(va_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled) 
      
      mean(vlat_Vr_Caenorhabditis_P7p_wt_mod3_scaled) 
      
      #variance of fixed effects
      X_Vr_Caenorhabditis_P7p_wt_mod3_scaled <- Vr_Caenorhabditis_P7p_wt_mod3_scaled[["X"]]
      beta_Vr_Caenorhabditis_P7p_wt_mod3_scaled <- as.data.frame(Vr_Caenorhabditis_P7p_wt_mod3_scaled[["Sol"]][,1])
      vf_Vr_Caenorhabditis_P7p_wt_mod3_scaled   <- apply(beta_Vr_Caenorhabditis_P7p_wt_mod3_scaled, 1, function(b) {var(as.vector(X_Vr_Caenorhabditis_P7p_wt_mod3_scaled %*% b))}) 
      mean(vf_Vr_Caenorhabditis_P7p_wt_mod3_scaled) 
      
      h2_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled <- va_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled / (vlat_Vr_Caenorhabditis_P7p_wt_mod3_scaled + vf_Vr_Caenorhabditis_P7p_wt_mod3_scaled)
      mean(h2_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled) 
      posterior.mode(h2_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled)	
      median(h2_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled)		
      HPDinterval(h2_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled)		#
      
    }
    
    ##Evolvability Evol=Va/mean^2
    
    trait_mean_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled<- Vr_Caenorhabditis_P7p_wt_mod3_scaled[["Sol"]][,1]
    
    #NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.Since the Tree has been scaled to 1,it's divided by 106.30 to give Va per million years
    
    Evol_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled<- (va_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30)) / (trait_mean_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled)^2
    mean(Evol_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled)
    
    
    #Vr_Caenorhabditis_P7p_wt_mod3_scaled  data scale
    {
      
      predict_Vr_Caenorhabditis_P7p_wt_mod3_scaled <- map(1:nrow(beta_Vr_Caenorhabditis_P7p_wt_mod3_scaled), ~ as.vector(X_Vr_Caenorhabditis_P7p_wt_mod3_scaled %*% beta_Vr_Caenorhabditis_P7p_wt_mod3_scaled[., ]))
      
      data_Vr_Caenorhabditis_P7p_wt_mod3_scaled <-
        pmap_dfr(list(predict = predict_Vr_Caenorhabditis_P7p_wt_mod3_scaled,
                      var.a = Vr_Caenorhabditis_P7p_wt_mod3_scaled[["VCV"]][ , "Species_phylo"],
                      var.p = rowSums(Vr_Caenorhabditis_P7p_wt_mod3_scaled[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled <- data_Vr_Caenorhabditis_P7p_wt_mod3_scaled[["h2.obs"]]
      trait_mean_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled <- data_Vr_Caenorhabditis_P7p_wt_mod3_scaled[["mean.obs"]]
      va_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled <- data_Vr_Caenorhabditis_P7p_wt_mod3_scaled[["var.a.obs"]]
      vp_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled <- data_Vr_Caenorhabditis_P7p_wt_mod3_scaled[["var.obs"]]
      
      Evol_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled <- (va_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30)) / (trait_mean_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled)^2
      
      mean(h2_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled)
      mean(trait_mean_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled)
      mean(va_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled)
      mean(vp_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled)
      mean(Evol_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled)
      
    }
    
    va_NotPhylo_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled <- (Vr_Caenorhabditis_P7p_wt_mod3_scaled[["VCV"]][ , "Species"])
    
    
    #Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled  data scale
    {
      
      predict_Vr_Caenorhabditis_P7p_wt_mod3_scaled <- map(1:nrow(beta_Vr_Caenorhabditis_P7p_wt_mod3_scaled), ~ as.vector(X_Vr_Caenorhabditis_P7p_wt_mod3_scaled %*% beta_Vr_Caenorhabditis_P7p_wt_mod3_scaled[., ]))
      
     data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled <-
        pmap_dfr(list(predict = predict_Vr_Caenorhabditis_P7p_wt_mod3_scaled,
                      var.a = Vr_Caenorhabditis_P7p_wt_mod3_scaled[["VCV"]][ , "Species"],
                      var.p = rowSums(Vr_Caenorhabditis_P7p_wt_mod3_scaled[["VCV"]]) - 1),
                 QGparams,
                 model = "binom1.probit",
                 verbose = FALSE) 
      
      h2_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled[["h2.obs"]]
      trait_mean_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled[["mean.obs"]]
      va_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled[["var.a.obs"]]
      vp_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled <-data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled[["var.obs"]]
      
      Evol_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled <- (va_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30)) / (trait_mean_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled)^2
      
      mean(h2_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled)
      mean(trait_mean_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled)
      mean(va_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled)
      mean(vp_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled)
      mean(Evol_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled)
      
      
    }
  }
  
  
}
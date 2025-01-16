#Vg_Oscheius_summary_SSSS_2

# Otipulae ----
{
  ##Summary Otipulae P3p ----
  {
    #Summary liability scale Otipulae P3p
    {
      #Summary va_liab_Otipulae_P3p_SSSS_2: NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        
        mean_va_liab_Otipulae_P3p_SSSS_2 <- rbind(mean(va_liab_Otipulae_CONTROL_P3p_SSSS_mod_2/2),mean(va_liab_Otipulae_WILD_P3p_SSSS_mod/2),mean(va_liab_Otipulae_Vg_P3p_SSSS_mod_2/2))
        colnames(mean_va_liab_Otipulae_P3p_SSSS_2) <- c("mean")
        median_va_liab_Otipulae_P3p_SSSS_2 <- rbind(median(va_liab_Otipulae_CONTROL_P3p_SSSS_mod_2/2),median(va_liab_Otipulae_WILD_P3p_SSSS_mod/2),median(va_liab_Otipulae_Vg_P3p_SSSS_mod_2/2))
        colnames(median_va_liab_Otipulae_P3p_SSSS_2) <- c("median")
        posterior.mode_va_liab_Otipulae_P3p_SSSS_2 <- rbind(posterior.mode(va_liab_Otipulae_CONTROL_P3p_SSSS_mod_2/2),posterior.mode(va_liab_Otipulae_WILD_P3p_SSSS_mod/2),posterior.mode(va_liab_Otipulae_Vg_P3p_SSSS_mod_2/2))
        colnames(posterior.mode_va_liab_Otipulae_P3p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Otipulae_P3p_SSSS_2 <- rbind(HPDinterval(va_liab_Otipulae_CONTROL_P3p_SSSS_mod_2/2),HPDinterval(va_liab_Otipulae_WILD_P3p_SSSS_mod/2),HPDinterval(va_liab_Otipulae_Vg_P3p_SSSS_mod_2/2))
        colnames(HPDinterval_0.95_va_liab_Otipulae_P3p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Otipulae_P3p_SSSS_2 <- rbind(HPDinterval(va_liab_Otipulae_CONTROL_P3p_SSSS_mod_2/2,prob=.83),HPDinterval(va_liab_Otipulae_WILD_P3p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_Otipulae_Vg_P3p_SSSS_mod_2/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Otipulae_P3p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Otipulae_P3p_SSSS_2 <- rbind(effectiveSize(va_liab_Otipulae_CONTROL_P3p_SSSS_mod_2/2),effectiveSize(va_liab_Otipulae_WILD_P3p_SSSS_mod/2),effectiveSize(va_liab_Otipulae_Vg_P3p_SSSS_mod_2/2))
        colnames(effectiveSize_va_liab_Otipulae_P3p_SSSS_2) <- c("effectiveSize")
        va_liab_Otipulae_P3p_SSSS_2 <- cbind.data.frame(mean_va_liab_Otipulae_P3p_SSSS_2,median_va_liab_Otipulae_P3p_SSSS_2,posterior.mode_va_liab_Otipulae_P3p_SSSS_2,HPDinterval_0.95_va_liab_Otipulae_P3p_SSSS_2,HPDinterval_0.83_va_liab_Otipulae_P3p_SSSS_2,effectiveSize_va_liab_Otipulae_P3p_SSSS_2)
        rownames(va_liab_Otipulae_P3p_SSSS_2) <- c("va_liab_Otipulae_CONTROL_P3p_SSSS_mod_2","va_liab_Otipulae_WILD_P3p_SSSS_mod","va_liab_Otipulae_Vg_P3p_SSSS_mod_2")
        va_liab_Otipulae_P3p_SSSS_2 <- cbind(Models = rownames(va_liab_Otipulae_P3p_SSSS_2),va_liab_Otipulae_P3p_SSSS_2)
        rownames(va_liab_Otipulae_P3p_SSSS_2) <- NULL
        va_liab_Otipulae_P3p_SSSS_2$Pnp <- c("P3.p","P3.p","P3.p")
        va_liab_Otipulae_P3p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        va_liab_Otipulae_P3p_SSSS_2$Measure <- c("Va","Va","Va")
        va_liab_Otipulae_P3p_SSSS_2$Scale <- c("liab","liab","liab")
        va_liab_Otipulae_P3p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        va_liab_Otipulae_P3p_SSSS_2
      }
      
      #Summary h2_liab_Otipulae_P3p_SSSS_2
      {
        mean_h2_liab_Otipulae_P3p_SSSS_2 <- rbind(mean(h2_liab_Otipulae_CONTROL_P3p_SSSS_mod_2),mean(h2_liab_Otipulae_WILD_P3p_SSSS_mod),mean(h2_liab_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(mean_h2_liab_Otipulae_P3p_SSSS_2) <- c("mean")
        median_h2_liab_Otipulae_P3p_SSSS_2 <- rbind(median(h2_liab_Otipulae_CONTROL_P3p_SSSS_mod_2),median(h2_liab_Otipulae_WILD_P3p_SSSS_mod),median(h2_liab_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(median_h2_liab_Otipulae_P3p_SSSS_2) <- c("median")
        posterior.mode_h2_liab_Otipulae_P3p_SSSS_2 <- rbind(posterior.mode(h2_liab_Otipulae_CONTROL_P3p_SSSS_mod_2),posterior.mode(h2_liab_Otipulae_WILD_P3p_SSSS_mod),posterior.mode(h2_liab_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(posterior.mode_h2_liab_Otipulae_P3p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Otipulae_P3p_SSSS_2 <- rbind(HPDinterval(h2_liab_Otipulae_CONTROL_P3p_SSSS_mod_2),HPDinterval(h2_liab_Otipulae_WILD_P3p_SSSS_mod),HPDinterval(h2_liab_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(HPDinterval_0.95_h2_liab_Otipulae_P3p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Otipulae_P3p_SSSS_2 <- rbind(HPDinterval(h2_liab_Otipulae_CONTROL_P3p_SSSS_mod_2,prob=.83),HPDinterval(h2_liab_Otipulae_WILD_P3p_SSSS_mod,prob=.83),HPDinterval(h2_liab_Otipulae_Vg_P3p_SSSS_mod_2,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Otipulae_P3p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Otipulae_P3p_SSSS_2 <- rbind(effectiveSize(h2_liab_Otipulae_CONTROL_P3p_SSSS_mod_2),effectiveSize(h2_liab_Otipulae_WILD_P3p_SSSS_mod),effectiveSize(h2_liab_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(effectiveSize_h2_liab_Otipulae_P3p_SSSS_2) <- c("effectiveSize")
        h2_liab_Otipulae_P3p_SSSS_2 <- cbind.data.frame(mean_h2_liab_Otipulae_P3p_SSSS_2,median_h2_liab_Otipulae_P3p_SSSS_2,posterior.mode_h2_liab_Otipulae_P3p_SSSS_2,HPDinterval_0.95_h2_liab_Otipulae_P3p_SSSS_2,HPDinterval_0.83_h2_liab_Otipulae_P3p_SSSS_2,effectiveSize_h2_liab_Otipulae_P3p_SSSS_2)
        rownames(h2_liab_Otipulae_P3p_SSSS_2) <- c("h2_liab_Otipulae_CONTROL_P3p_SSSS_mod_2","h2_liab_Otipulae_WILD_P3p_SSSS_mod","h2_liab_Otipulae_Vg_P3p_SSSS_mod_2")
        h2_liab_Otipulae_P3p_SSSS_2 <- cbind(Models = rownames(h2_liab_Otipulae_P3p_SSSS_2),h2_liab_Otipulae_P3p_SSSS_2)
        rownames(h2_liab_Otipulae_P3p_SSSS_2) <- NULL
        h2_liab_Otipulae_P3p_SSSS_2$Pnp <- c("P3.p","P3.p","P3.p")
        h2_liab_Otipulae_P3p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        h2_liab_Otipulae_P3p_SSSS_2$Measure <- c("H2","H2","H2")
        h2_liab_Otipulae_P3p_SSSS_2$Scale <- c("liab","liab","liab")
        h2_liab_Otipulae_P3p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        h2_liab_Otipulae_P3p_SSSS_2
      }
      
      #Summary Evol_liab_Otipulae_P3p_SSSS_2
      {
        mean_Evol_liab_Otipulae_P3p_SSSS_2 <- rbind(mean(Evol_liab_Otipulae_CONTROL_P3p_SSSS_mod_2),mean(Evol_liab_Otipulae_WILD_P3p_SSSS_mod),mean(Evol_liab_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(mean_Evol_liab_Otipulae_P3p_SSSS_2) <- c("mean")
        median_Evol_liab_Otipulae_P3p_SSSS_2 <- rbind(median(Evol_liab_Otipulae_CONTROL_P3p_SSSS_mod_2),median(Evol_liab_Otipulae_WILD_P3p_SSSS_mod),median(Evol_liab_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(median_Evol_liab_Otipulae_P3p_SSSS_2) <- c("median")
        posterior.mode_Evol_liab_Otipulae_P3p_SSSS_2 <- rbind(posterior.mode(Evol_liab_Otipulae_CONTROL_P3p_SSSS_mod_2),posterior.mode(Evol_liab_Otipulae_WILD_P3p_SSSS_mod),posterior.mode(Evol_liab_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(posterior.mode_Evol_liab_Otipulae_P3p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Otipulae_P3p_SSSS_2 <- rbind(HPDinterval(Evol_liab_Otipulae_CONTROL_P3p_SSSS_mod_2),HPDinterval(Evol_liab_Otipulae_WILD_P3p_SSSS_mod),HPDinterval(Evol_liab_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(HPDinterval_0.95_Evol_liab_Otipulae_P3p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Otipulae_P3p_SSSS_2 <- rbind(HPDinterval(Evol_liab_Otipulae_CONTROL_P3p_SSSS_mod_2,prob=.83),HPDinterval(Evol_liab_Otipulae_WILD_P3p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_Otipulae_Vg_P3p_SSSS_mod_2,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Otipulae_P3p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Otipulae_P3p_SSSS_2 <- rbind(effectiveSize(Evol_liab_Otipulae_CONTROL_P3p_SSSS_mod_2),effectiveSize(Evol_liab_Otipulae_WILD_P3p_SSSS_mod),effectiveSize(Evol_liab_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(effectiveSize_Evol_liab_Otipulae_P3p_SSSS_2) <- c("effectiveSize")
        Evol_liab_Otipulae_P3p_SSSS_2 <- cbind.data.frame(mean_Evol_liab_Otipulae_P3p_SSSS_2,median_Evol_liab_Otipulae_P3p_SSSS_2,posterior.mode_Evol_liab_Otipulae_P3p_SSSS_2,HPDinterval_0.95_Evol_liab_Otipulae_P3p_SSSS_2,HPDinterval_0.83_Evol_liab_Otipulae_P3p_SSSS_2,effectiveSize_Evol_liab_Otipulae_P3p_SSSS_2)
        rownames(Evol_liab_Otipulae_P3p_SSSS_2) <- c("Evol_liab_Otipulae_CONTROL_P3p_SSSS_mod_2","Evol_liab_Otipulae_WILD_P3p_SSSS_mod","Evol_liab_Otipulae_Vg_P3p_SSSS_mod_2")
        Evol_liab_Otipulae_P3p_SSSS_2 <- cbind(Models = rownames(Evol_liab_Otipulae_P3p_SSSS_2),Evol_liab_Otipulae_P3p_SSSS_2)
        rownames(Evol_liab_Otipulae_P3p_SSSS_2) <- NULL
        Evol_liab_Otipulae_P3p_SSSS_2$Pnp <- c("P3.p","P3.p","P3.p")
        Evol_liab_Otipulae_P3p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        Evol_liab_Otipulae_P3p_SSSS_2$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Otipulae_P3p_SSSS_2$Scale <- c("liab","liab","liab")
        Evol_liab_Otipulae_P3p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        Evol_liab_Otipulae_P3p_SSSS_2
      }
      
      #Summary trait_mean_liab_Otipulae_P3p_SSSS_2
      {
        mean_trait_mean_liab_Otipulae_P3p_SSSS_2 <- rbind(mean(trait_mean_liab_Otipulae_CONTROL_P3p_SSSS_mod_2),mean(trait_mean_liab_Otipulae_WILD_P3p_SSSS_mod),mean(trait_mean_liab_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(mean_trait_mean_liab_Otipulae_P3p_SSSS_2) <- c("mean")
        median_trait_mean_liab_Otipulae_P3p_SSSS_2 <- rbind(median(trait_mean_liab_Otipulae_CONTROL_P3p_SSSS_mod_2),median(trait_mean_liab_Otipulae_WILD_P3p_SSSS_mod),median(trait_mean_liab_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(median_trait_mean_liab_Otipulae_P3p_SSSS_2) <- c("median")
        posterior.mode_trait_mean_liab_Otipulae_P3p_SSSS_2 <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P3p_SSSS_mod_2)),posterior.mode(as.mcmc(trait_mean_liab_Otipulae_WILD_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Otipulae_Vg_P3p_SSSS_mod_2)))
        colnames(posterior.mode_trait_mean_liab_Otipulae_P3p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Otipulae_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P3p_SSSS_mod_2)),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_WILD_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_Vg_P3p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_liab_Otipulae_P3p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Otipulae_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P3p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_WILD_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_Vg_P3p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Otipulae_P3p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Otipulae_P3p_SSSS_2 <- rbind(effectiveSize(trait_mean_liab_Otipulae_CONTROL_P3p_SSSS_mod_2),effectiveSize(trait_mean_liab_Otipulae_WILD_P3p_SSSS_mod),effectiveSize(trait_mean_liab_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(effectiveSize_trait_mean_liab_Otipulae_P3p_SSSS_2) <- c("effectiveSize")
        trait_mean_liab_Otipulae_P3p_SSSS_2 <- cbind.data.frame(mean_trait_mean_liab_Otipulae_P3p_SSSS_2,median_trait_mean_liab_Otipulae_P3p_SSSS_2,posterior.mode_trait_mean_liab_Otipulae_P3p_SSSS_2,HPDinterval_0.95_trait_mean_liab_Otipulae_P3p_SSSS_2,HPDinterval_0.83_trait_mean_liab_Otipulae_P3p_SSSS_2,effectiveSize_trait_mean_liab_Otipulae_P3p_SSSS_2)
        rownames(trait_mean_liab_Otipulae_P3p_SSSS_2) <- c("trait_mean_liab_Otipulae_CONTROL_P3p_SSSS_mod_2","trait_mean_liab_Otipulae_WILD_P3p_SSSS_mod","trait_mean_liab_Otipulae_Vg_P3p_SSSS_mod_2")
        trait_mean_liab_Otipulae_P3p_SSSS_2 <- cbind(Models = rownames(trait_mean_liab_Otipulae_P3p_SSSS_2),trait_mean_liab_Otipulae_P3p_SSSS_2)
        rownames(trait_mean_liab_Otipulae_P3p_SSSS_2) <- NULL
        trait_mean_liab_Otipulae_P3p_SSSS_2$Pnp <- c("P3.p","P3.p","P3.p")
        trait_mean_liab_Otipulae_P3p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_liab_Otipulae_P3p_SSSS_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Otipulae_P3p_SSSS_2$Scale <- c("liab","liab","liab")
        trait_mean_liab_Otipulae_P3p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_liab_Otipulae_P3p_SSSS_2
      }
      
      liab_Otipulae_P3p_SSSS_2 <- rbind.data.frame(va_liab_Otipulae_P3p_SSSS_2, h2_liab_Otipulae_P3p_SSSS_2,Evol_liab_Otipulae_P3p_SSSS_2,trait_mean_liab_Otipulae_P3p_SSSS_2)
      liab_Otipulae_P3p_SSSS_2
    }
    #Summary data scale Otipulae P3p
    {
      #Summary va_data_Otipulae_P3p_SSSS_2:NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        mean_va_data_Otipulae_P3p_SSSS_2 <- rbind(mean(va_data_Otipulae_CONTROL_P3p_SSSS_mod_2/2),mean(va_data_Otipulae_WILD_P3p_SSSS_mod/2),mean(va_data_Otipulae_Vg_P3p_SSSS_mod_2/2))
        colnames(mean_va_data_Otipulae_P3p_SSSS_2) <- c("mean")
        median_va_data_Otipulae_P3p_SSSS_2 <- rbind(median(va_data_Otipulae_CONTROL_P3p_SSSS_mod_2/2),median(va_data_Otipulae_WILD_P3p_SSSS_mod/2),median(va_data_Otipulae_Vg_P3p_SSSS_mod_2/2))
        colnames(median_va_data_Otipulae_P3p_SSSS_2) <- c("median")
        posterior.mode_va_data_Otipulae_P3p_SSSS_2 <- rbind(posterior.mode(as.mcmc(va_data_Otipulae_CONTROL_P3p_SSSS_mod_2/2)),posterior.mode(as.mcmc(va_data_Otipulae_WILD_P3p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_Otipulae_Vg_P3p_SSSS_mod_2/2)))
        colnames(posterior.mode_va_data_Otipulae_P3p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Otipulae_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(va_data_Otipulae_CONTROL_P3p_SSSS_mod_2/2)),HPDinterval(as.mcmc(va_data_Otipulae_WILD_P3p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_Otipulae_Vg_P3p_SSSS_mod_2/2)))
        colnames(HPDinterval_0.95_va_data_Otipulae_P3p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Otipulae_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(va_data_Otipulae_CONTROL_P3p_SSSS_mod_2/2),prob=.83),HPDinterval(as.mcmc(va_data_Otipulae_WILD_P3p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Otipulae_Vg_P3p_SSSS_mod_2/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Otipulae_P3p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Otipulae_P3p_SSSS_2 <- rbind(effectiveSize(va_data_Otipulae_CONTROL_P3p_SSSS_mod_2/2),effectiveSize(va_data_Otipulae_WILD_P3p_SSSS_mod/2),effectiveSize(va_data_Otipulae_Vg_P3p_SSSS_mod_2/2))
        colnames(effectiveSize_va_data_Otipulae_P3p_SSSS_2) <- c("effectiveSize")
        va_data_Otipulae_P3p_SSSS_2 <- cbind.data.frame(mean_va_data_Otipulae_P3p_SSSS_2,median_va_data_Otipulae_P3p_SSSS_2,posterior.mode_va_data_Otipulae_P3p_SSSS_2,HPDinterval_0.95_va_data_Otipulae_P3p_SSSS_2,HPDinterval_0.83_va_data_Otipulae_P3p_SSSS_2,effectiveSize_va_data_Otipulae_P3p_SSSS_2)
        rownames(va_data_Otipulae_P3p_SSSS_2) <- c("va_data_Otipulae_CONTROL_P3p_SSSS_mod_2","va_data_Otipulae_WILD_P3p_SSSS_mod","va_data_Otipulae_Vg_P3p_SSSS_mod_2")
        va_data_Otipulae_P3p_SSSS_2 <- cbind(Models = rownames(va_data_Otipulae_P3p_SSSS_2),va_data_Otipulae_P3p_SSSS_2)
        rownames(va_data_Otipulae_P3p_SSSS_2) <- NULL
        va_data_Otipulae_P3p_SSSS_2$Pnp <- c("P3.p","P3.p","P3.p")
        va_data_Otipulae_P3p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        va_data_Otipulae_P3p_SSSS_2$Measure <- c("Va","Va","Va")
        va_data_Otipulae_P3p_SSSS_2$Scale <- c("data","data","data")
        va_data_Otipulae_P3p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        va_data_Otipulae_P3p_SSSS_2
      }
      
      #Summary h2_data_Otipulae_P3p_SSSS_2
      {
        mean_h2_data_Otipulae_P3p_SSSS_2 <- rbind(mean(h2_data_Otipulae_CONTROL_P3p_SSSS_mod_2),mean(h2_data_Otipulae_WILD_P3p_SSSS_mod),mean(h2_data_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(mean_h2_data_Otipulae_P3p_SSSS_2) <- c("mean")
        median_h2_data_Otipulae_P3p_SSSS_2 <- rbind(median(h2_data_Otipulae_CONTROL_P3p_SSSS_mod_2),median(h2_data_Otipulae_WILD_P3p_SSSS_mod),median(h2_data_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(median_h2_data_Otipulae_P3p_SSSS_2) <- c("median")
        posterior.mode_h2_data_Otipulae_P3p_SSSS_2 <- rbind(posterior.mode(as.mcmc(h2_data_Otipulae_CONTROL_P3p_SSSS_mod_2)),posterior.mode(as.mcmc(h2_data_Otipulae_WILD_P3p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_Otipulae_Vg_P3p_SSSS_mod_2)))
        colnames(posterior.mode_h2_data_Otipulae_P3p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Otipulae_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(h2_data_Otipulae_CONTROL_P3p_SSSS_mod_2)),HPDinterval(as.mcmc(h2_data_Otipulae_WILD_P3p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_Otipulae_Vg_P3p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_h2_data_Otipulae_P3p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Otipulae_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(h2_data_Otipulae_CONTROL_P3p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(h2_data_Otipulae_WILD_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Otipulae_Vg_P3p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Otipulae_P3p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Otipulae_P3p_SSSS_2 <- rbind(effectiveSize(h2_data_Otipulae_CONTROL_P3p_SSSS_mod_2),effectiveSize(h2_data_Otipulae_WILD_P3p_SSSS_mod),effectiveSize(h2_data_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(effectiveSize_h2_data_Otipulae_P3p_SSSS_2) <- c("effectiveSize")
        h2_data_Otipulae_P3p_SSSS_2 <- cbind.data.frame(mean_h2_data_Otipulae_P3p_SSSS_2,median_h2_data_Otipulae_P3p_SSSS_2,posterior.mode_h2_data_Otipulae_P3p_SSSS_2,HPDinterval_0.95_h2_data_Otipulae_P3p_SSSS_2,HPDinterval_0.83_h2_data_Otipulae_P3p_SSSS_2,effectiveSize_h2_data_Otipulae_P3p_SSSS_2)
        rownames(h2_data_Otipulae_P3p_SSSS_2) <- c("h2_data_Otipulae_CONTROL_P3p_SSSS_mod_2","h2_data_Otipulae_WILD_P3p_SSSS_mod","h2_data_Otipulae_Vg_P3p_SSSS_mod_2")
        h2_data_Otipulae_P3p_SSSS_2 <- cbind(Models = rownames(h2_data_Otipulae_P3p_SSSS_2),h2_data_Otipulae_P3p_SSSS_2)
        rownames(h2_data_Otipulae_P3p_SSSS_2) <- NULL
        h2_data_Otipulae_P3p_SSSS_2$Pnp <- c("P3.p","P3.p","P3.p")
        h2_data_Otipulae_P3p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        h2_data_Otipulae_P3p_SSSS_2$Measure <- c("H2","H2","H2")
        h2_data_Otipulae_P3p_SSSS_2$Scale <- c("data","data","data")
        h2_data_Otipulae_P3p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        h2_data_Otipulae_P3p_SSSS_2
      }
      
      #Summary Evol_data_Otipulae_P3p_SSSS_2
      {
        mean_Evol_data_Otipulae_P3p_SSSS_2 <- rbind(mean(Evol_data_Otipulae_CONTROL_P3p_SSSS_mod_2),mean(Evol_data_Otipulae_WILD_P3p_SSSS_mod),mean(Evol_data_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(mean_Evol_data_Otipulae_P3p_SSSS_2) <- c("mean")
        median_Evol_data_Otipulae_P3p_SSSS_2 <- rbind(median(Evol_data_Otipulae_CONTROL_P3p_SSSS_mod_2),median(Evol_data_Otipulae_WILD_P3p_SSSS_mod),median(Evol_data_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(median_Evol_data_Otipulae_P3p_SSSS_2) <- c("median")
        posterior.mode_Evol_data_Otipulae_P3p_SSSS_2 <- rbind(posterior.mode(as.mcmc(Evol_data_Otipulae_CONTROL_P3p_SSSS_mod_2)),posterior.mode(as.mcmc(Evol_data_Otipulae_WILD_P3p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_Otipulae_Vg_P3p_SSSS_mod_2)))
        colnames(posterior.mode_Evol_data_Otipulae_P3p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Otipulae_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Otipulae_CONTROL_P3p_SSSS_mod_2)),HPDinterval(as.mcmc(Evol_data_Otipulae_WILD_P3p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_Otipulae_Vg_P3p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_Evol_data_Otipulae_P3p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Otipulae_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Otipulae_CONTROL_P3p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(Evol_data_Otipulae_WILD_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Otipulae_Vg_P3p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Otipulae_P3p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Otipulae_P3p_SSSS_2 <- rbind(effectiveSize(Evol_data_Otipulae_CONTROL_P3p_SSSS_mod_2),effectiveSize(Evol_data_Otipulae_WILD_P3p_SSSS_mod),effectiveSize(Evol_data_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(effectiveSize_Evol_data_Otipulae_P3p_SSSS_2) <- c("effectiveSize")
        Evol_data_Otipulae_P3p_SSSS_2 <- cbind.data.frame(mean_Evol_data_Otipulae_P3p_SSSS_2,median_Evol_data_Otipulae_P3p_SSSS_2,posterior.mode_Evol_data_Otipulae_P3p_SSSS_2,HPDinterval_0.95_Evol_data_Otipulae_P3p_SSSS_2,HPDinterval_0.83_Evol_data_Otipulae_P3p_SSSS_2,effectiveSize_Evol_data_Otipulae_P3p_SSSS_2)
        rownames(Evol_data_Otipulae_P3p_SSSS_2) <- c("Evol_data_Otipulae_CONTROL_P3p_SSSS_mod_2","Evol_data_Otipulae_WILD_P3p_SSSS_mod","Evol_data_Otipulae_Vg_P3p_SSSS_mod_2")
        Evol_data_Otipulae_P3p_SSSS_2 <- cbind(Models = rownames(Evol_data_Otipulae_P3p_SSSS_2),Evol_data_Otipulae_P3p_SSSS_2)
        rownames(Evol_data_Otipulae_P3p_SSSS_2) <- NULL
        Evol_data_Otipulae_P3p_SSSS_2$Pnp <- c("P3.p","P3.p","P3.p")
        Evol_data_Otipulae_P3p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        Evol_data_Otipulae_P3p_SSSS_2$Measure <- c("Evol","Evol","Evol")
        Evol_data_Otipulae_P3p_SSSS_2$Scale <- c("data","data","data")
        Evol_data_Otipulae_P3p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        Evol_data_Otipulae_P3p_SSSS_2
      }
      
      #Summary trait_mean_data_Otipulae_P3p_SSSS_2
      {
        mean_trait_mean_data_Otipulae_P3p_SSSS_2 <- rbind(mean(trait_mean_data_Otipulae_CONTROL_P3p_SSSS_mod_2),mean(trait_mean_data_Otipulae_WILD_P3p_SSSS_mod),mean(trait_mean_data_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(mean_trait_mean_data_Otipulae_P3p_SSSS_2) <- c("mean")
        median_trait_mean_data_Otipulae_P3p_SSSS_2 <- rbind(median(trait_mean_data_Otipulae_CONTROL_P3p_SSSS_mod_2),median(trait_mean_data_Otipulae_WILD_P3p_SSSS_mod),median(trait_mean_data_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(median_trait_mean_data_Otipulae_P3p_SSSS_2) <- c("median")
        posterior.mode_trait_mean_data_Otipulae_P3p_SSSS_2 <- rbind(posterior.mode(as.mcmc(trait_mean_data_Otipulae_CONTROL_P3p_SSSS_mod_2)),posterior.mode(as.mcmc(trait_mean_data_Otipulae_WILD_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_Otipulae_Vg_P3p_SSSS_mod_2)))
        colnames(posterior.mode_trait_mean_data_Otipulae_P3p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Otipulae_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Otipulae_CONTROL_P3p_SSSS_mod_2)),HPDinterval(as.mcmc(trait_mean_data_Otipulae_WILD_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_Otipulae_Vg_P3p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_data_Otipulae_P3p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Otipulae_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Otipulae_CONTROL_P3p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Otipulae_WILD_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Otipulae_Vg_P3p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Otipulae_P3p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Otipulae_P3p_SSSS_2 <- rbind(effectiveSize(trait_mean_data_Otipulae_CONTROL_P3p_SSSS_mod_2),effectiveSize(trait_mean_data_Otipulae_WILD_P3p_SSSS_mod),effectiveSize(trait_mean_data_Otipulae_Vg_P3p_SSSS_mod_2))
        colnames(effectiveSize_trait_mean_data_Otipulae_P3p_SSSS_2) <- c("effectiveSize")
        trait_mean_data_Otipulae_P3p_SSSS_2 <- cbind.data.frame(mean_trait_mean_data_Otipulae_P3p_SSSS_2,median_trait_mean_data_Otipulae_P3p_SSSS_2,posterior.mode_trait_mean_data_Otipulae_P3p_SSSS_2,HPDinterval_0.95_trait_mean_data_Otipulae_P3p_SSSS_2,HPDinterval_0.83_trait_mean_data_Otipulae_P3p_SSSS_2,effectiveSize_trait_mean_data_Otipulae_P3p_SSSS_2)
        rownames(trait_mean_data_Otipulae_P3p_SSSS_2) <- c("trait_mean_data_Otipulae_CONTROL_P3p_SSSS_mod_2","trait_mean_data_Otipulae_WILD_P3p_SSSS_mod","trait_mean_data_Otipulae_Vg_P3p_SSSS_mod_2")
        trait_mean_data_Otipulae_P3p_SSSS_2 <- cbind(Models = rownames(trait_mean_data_Otipulae_P3p_SSSS_2),trait_mean_data_Otipulae_P3p_SSSS_2)
        rownames(trait_mean_data_Otipulae_P3p_SSSS_2) <- NULL
        trait_mean_data_Otipulae_P3p_SSSS_2$Pnp <- c("P3.p","P3.p","P3.p")
        trait_mean_data_Otipulae_P3p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_data_Otipulae_P3p_SSSS_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Otipulae_P3p_SSSS_2$Scale <- c("data","data","data")
        trait_mean_data_Otipulae_P3p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_data_Otipulae_P3p_SSSS_2
      }
      
      data_Otipulae_P3p_SSSS_2 <- rbind.data.frame(va_data_Otipulae_P3p_SSSS_2, h2_data_Otipulae_P3p_SSSS_2,Evol_data_Otipulae_P3p_SSSS_2,trait_mean_data_Otipulae_P3p_SSSS_2)
      data_Otipulae_P3p_SSSS_2
      
    }
    Vg_Otipulae_P3p_SSSS_2 <- rbind.data.frame(liab_Otipulae_P3p_SSSS_2, data_Otipulae_P3p_SSSS_2)
    Vg_Otipulae_P3p_SSSS_2$Pnp_fate <- rep("SSSS", 24)
    Vg_Otipulae_P3p_SSSS_2
    #remove Otipulae P3p_SSSS_2 models
    {
      remove(Otipulae_CONTROL_P3p_SSSS_mod_2)
      remove(Otipulae_WILD_P3p_SSSS_mod)
      remove(Otipulae_Vg_P3p_SSSS_mod_2)
    }
  }
  
  ##Summary Otipulae P4p -----
  {
    #Summary liability scale Otipulae P4p
    {
      #Summary va_liab_Otipulae_P4p_SSSS_2: NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        
        mean_va_liab_Otipulae_P4p_SSSS_2 <- rbind(mean(va_liab_Otipulae_CONTROL_P4p_SSSS_mod_2/2),mean(va_liab_Otipulae_WILD_P4p_SSSS_mod/2),mean(va_liab_Otipulae_Vg_P4p_SSSS_mod_2/2))
        colnames(mean_va_liab_Otipulae_P4p_SSSS_2) <- c("mean")
        median_va_liab_Otipulae_P4p_SSSS_2 <- rbind(median(va_liab_Otipulae_CONTROL_P4p_SSSS_mod_2/2),median(va_liab_Otipulae_WILD_P4p_SSSS_mod/2),median(va_liab_Otipulae_Vg_P4p_SSSS_mod_2/2))
        colnames(median_va_liab_Otipulae_P4p_SSSS_2) <- c("median")
        posterior.mode_va_liab_Otipulae_P4p_SSSS_2 <- rbind(posterior.mode(va_liab_Otipulae_CONTROL_P4p_SSSS_mod_2/2),posterior.mode(va_liab_Otipulae_WILD_P4p_SSSS_mod/2),posterior.mode(va_liab_Otipulae_Vg_P4p_SSSS_mod_2/2))
        colnames(posterior.mode_va_liab_Otipulae_P4p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Otipulae_P4p_SSSS_2 <- rbind(HPDinterval(va_liab_Otipulae_CONTROL_P4p_SSSS_mod_2/2),HPDinterval(va_liab_Otipulae_WILD_P4p_SSSS_mod/2),HPDinterval(va_liab_Otipulae_Vg_P4p_SSSS_mod_2/2))
        colnames(HPDinterval_0.95_va_liab_Otipulae_P4p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Otipulae_P4p_SSSS_2 <- rbind(HPDinterval(va_liab_Otipulae_CONTROL_P4p_SSSS_mod_2/2,prob=.83),HPDinterval(va_liab_Otipulae_WILD_P4p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_Otipulae_Vg_P4p_SSSS_mod_2/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Otipulae_P4p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Otipulae_P4p_SSSS_2 <- rbind(effectiveSize(va_liab_Otipulae_CONTROL_P4p_SSSS_mod_2/2),effectiveSize(va_liab_Otipulae_WILD_P4p_SSSS_mod/2),effectiveSize(va_liab_Otipulae_Vg_P4p_SSSS_mod_2/2))
        colnames(effectiveSize_va_liab_Otipulae_P4p_SSSS_2) <- c("effectiveSize")
        va_liab_Otipulae_P4p_SSSS_2 <- cbind.data.frame(mean_va_liab_Otipulae_P4p_SSSS_2,median_va_liab_Otipulae_P4p_SSSS_2,posterior.mode_va_liab_Otipulae_P4p_SSSS_2,HPDinterval_0.95_va_liab_Otipulae_P4p_SSSS_2,HPDinterval_0.83_va_liab_Otipulae_P4p_SSSS_2,effectiveSize_va_liab_Otipulae_P4p_SSSS_2)
        rownames(va_liab_Otipulae_P4p_SSSS_2) <- c("va_liab_Otipulae_CONTROL_P4p_SSSS_mod_2","va_liab_Otipulae_WILD_P4p_SSSS_mod","va_liab_Otipulae_Vg_P4p_SSSS_mod_2")
        va_liab_Otipulae_P4p_SSSS_2 <- cbind(Models = rownames(va_liab_Otipulae_P4p_SSSS_2),va_liab_Otipulae_P4p_SSSS_2)
        rownames(va_liab_Otipulae_P4p_SSSS_2) <- NULL
        va_liab_Otipulae_P4p_SSSS_2$Pnp <- c("P4.p","P4.p","P4.p")
        va_liab_Otipulae_P4p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        va_liab_Otipulae_P4p_SSSS_2$Measure <- c("Va","Va","Va")
        va_liab_Otipulae_P4p_SSSS_2$Scale <- c("liab","liab","liab")
        va_liab_Otipulae_P4p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        va_liab_Otipulae_P4p_SSSS_2
      }
      
      #Summary h2_liab_Otipulae_P4p_SSSS_2
      {
        mean_h2_liab_Otipulae_P4p_SSSS_2 <- rbind(mean(h2_liab_Otipulae_CONTROL_P4p_SSSS_mod_2),mean(h2_liab_Otipulae_WILD_P4p_SSSS_mod),mean(h2_liab_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(mean_h2_liab_Otipulae_P4p_SSSS_2) <- c("mean")
        median_h2_liab_Otipulae_P4p_SSSS_2 <- rbind(median(h2_liab_Otipulae_CONTROL_P4p_SSSS_mod_2),median(h2_liab_Otipulae_WILD_P4p_SSSS_mod),median(h2_liab_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(median_h2_liab_Otipulae_P4p_SSSS_2) <- c("median")
        posterior.mode_h2_liab_Otipulae_P4p_SSSS_2 <- rbind(posterior.mode(h2_liab_Otipulae_CONTROL_P4p_SSSS_mod_2),posterior.mode(h2_liab_Otipulae_WILD_P4p_SSSS_mod),posterior.mode(h2_liab_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(posterior.mode_h2_liab_Otipulae_P4p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Otipulae_P4p_SSSS_2 <- rbind(HPDinterval(h2_liab_Otipulae_CONTROL_P4p_SSSS_mod_2),HPDinterval(h2_liab_Otipulae_WILD_P4p_SSSS_mod),HPDinterval(h2_liab_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(HPDinterval_0.95_h2_liab_Otipulae_P4p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Otipulae_P4p_SSSS_2 <- rbind(HPDinterval(h2_liab_Otipulae_CONTROL_P4p_SSSS_mod_2,prob=.83),HPDinterval(h2_liab_Otipulae_WILD_P4p_SSSS_mod,prob=.83),HPDinterval(h2_liab_Otipulae_Vg_P4p_SSSS_mod_2,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Otipulae_P4p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Otipulae_P4p_SSSS_2 <- rbind(effectiveSize(h2_liab_Otipulae_CONTROL_P4p_SSSS_mod_2),effectiveSize(h2_liab_Otipulae_WILD_P4p_SSSS_mod),effectiveSize(h2_liab_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(effectiveSize_h2_liab_Otipulae_P4p_SSSS_2) <- c("effectiveSize")
        h2_liab_Otipulae_P4p_SSSS_2 <- cbind.data.frame(mean_h2_liab_Otipulae_P4p_SSSS_2,median_h2_liab_Otipulae_P4p_SSSS_2,posterior.mode_h2_liab_Otipulae_P4p_SSSS_2,HPDinterval_0.95_h2_liab_Otipulae_P4p_SSSS_2,HPDinterval_0.83_h2_liab_Otipulae_P4p_SSSS_2,effectiveSize_h2_liab_Otipulae_P4p_SSSS_2)
        rownames(h2_liab_Otipulae_P4p_SSSS_2) <- c("h2_liab_Otipulae_CONTROL_P4p_SSSS_mod_2","h2_liab_Otipulae_WILD_P4p_SSSS_mod","h2_liab_Otipulae_Vg_P4p_SSSS_mod_2")
        h2_liab_Otipulae_P4p_SSSS_2 <- cbind(Models = rownames(h2_liab_Otipulae_P4p_SSSS_2),h2_liab_Otipulae_P4p_SSSS_2)
        rownames(h2_liab_Otipulae_P4p_SSSS_2) <- NULL
        h2_liab_Otipulae_P4p_SSSS_2$Pnp <- c("P4.p","P4.p","P4.p")
        h2_liab_Otipulae_P4p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        h2_liab_Otipulae_P4p_SSSS_2$Measure <- c("H2","H2","H2")
        h2_liab_Otipulae_P4p_SSSS_2$Scale <- c("liab","liab","liab")
        h2_liab_Otipulae_P4p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        h2_liab_Otipulae_P4p_SSSS_2
      }
      
      #Summary Evol_liab_Otipulae_P4p_SSSS_2
      {
        mean_Evol_liab_Otipulae_P4p_SSSS_2 <- rbind(mean(Evol_liab_Otipulae_CONTROL_P4p_SSSS_mod_2),mean(Evol_liab_Otipulae_WILD_P4p_SSSS_mod),mean(Evol_liab_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(mean_Evol_liab_Otipulae_P4p_SSSS_2) <- c("mean")
        median_Evol_liab_Otipulae_P4p_SSSS_2 <- rbind(median(Evol_liab_Otipulae_CONTROL_P4p_SSSS_mod_2),median(Evol_liab_Otipulae_WILD_P4p_SSSS_mod),median(Evol_liab_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(median_Evol_liab_Otipulae_P4p_SSSS_2) <- c("median")
        posterior.mode_Evol_liab_Otipulae_P4p_SSSS_2 <- rbind(posterior.mode(Evol_liab_Otipulae_CONTROL_P4p_SSSS_mod_2),posterior.mode(Evol_liab_Otipulae_WILD_P4p_SSSS_mod),posterior.mode(Evol_liab_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(posterior.mode_Evol_liab_Otipulae_P4p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Otipulae_P4p_SSSS_2 <- rbind(HPDinterval(Evol_liab_Otipulae_CONTROL_P4p_SSSS_mod_2),HPDinterval(Evol_liab_Otipulae_WILD_P4p_SSSS_mod),HPDinterval(Evol_liab_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(HPDinterval_0.95_Evol_liab_Otipulae_P4p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Otipulae_P4p_SSSS_2 <- rbind(HPDinterval(Evol_liab_Otipulae_CONTROL_P4p_SSSS_mod_2,prob=.83),HPDinterval(Evol_liab_Otipulae_WILD_P4p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_Otipulae_Vg_P4p_SSSS_mod_2,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Otipulae_P4p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Otipulae_P4p_SSSS_2 <- rbind(effectiveSize(Evol_liab_Otipulae_CONTROL_P4p_SSSS_mod_2),effectiveSize(Evol_liab_Otipulae_WILD_P4p_SSSS_mod),effectiveSize(Evol_liab_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(effectiveSize_Evol_liab_Otipulae_P4p_SSSS_2) <- c("effectiveSize")
        Evol_liab_Otipulae_P4p_SSSS_2 <- cbind.data.frame(mean_Evol_liab_Otipulae_P4p_SSSS_2,median_Evol_liab_Otipulae_P4p_SSSS_2,posterior.mode_Evol_liab_Otipulae_P4p_SSSS_2,HPDinterval_0.95_Evol_liab_Otipulae_P4p_SSSS_2,HPDinterval_0.83_Evol_liab_Otipulae_P4p_SSSS_2,effectiveSize_Evol_liab_Otipulae_P4p_SSSS_2)
        rownames(Evol_liab_Otipulae_P4p_SSSS_2) <- c("Evol_liab_Otipulae_CONTROL_P4p_SSSS_mod_2","Evol_liab_Otipulae_WILD_P4p_SSSS_mod","Evol_liab_Otipulae_Vg_P4p_SSSS_mod_2")
        Evol_liab_Otipulae_P4p_SSSS_2 <- cbind(Models = rownames(Evol_liab_Otipulae_P4p_SSSS_2),Evol_liab_Otipulae_P4p_SSSS_2)
        rownames(Evol_liab_Otipulae_P4p_SSSS_2) <- NULL
        Evol_liab_Otipulae_P4p_SSSS_2$Pnp <- c("P4.p","P4.p","P4.p")
        Evol_liab_Otipulae_P4p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        Evol_liab_Otipulae_P4p_SSSS_2$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Otipulae_P4p_SSSS_2$Scale <- c("liab","liab","liab")
        Evol_liab_Otipulae_P4p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        Evol_liab_Otipulae_P4p_SSSS_2
      }
      
      #Summary trait_mean_liab_Otipulae_P4p_SSSS_2
      {
        mean_trait_mean_liab_Otipulae_P4p_SSSS_2 <- rbind(mean(trait_mean_liab_Otipulae_CONTROL_P4p_SSSS_mod_2),mean(trait_mean_liab_Otipulae_WILD_P4p_SSSS_mod),mean(trait_mean_liab_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(mean_trait_mean_liab_Otipulae_P4p_SSSS_2) <- c("mean")
        median_trait_mean_liab_Otipulae_P4p_SSSS_2 <- rbind(median(trait_mean_liab_Otipulae_CONTROL_P4p_SSSS_mod_2),median(trait_mean_liab_Otipulae_WILD_P4p_SSSS_mod),median(trait_mean_liab_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(median_trait_mean_liab_Otipulae_P4p_SSSS_2) <- c("median")
        posterior.mode_trait_mean_liab_Otipulae_P4p_SSSS_2 <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P4p_SSSS_mod_2)),posterior.mode(as.mcmc(trait_mean_liab_Otipulae_WILD_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Otipulae_Vg_P4p_SSSS_mod_2)))
        colnames(posterior.mode_trait_mean_liab_Otipulae_P4p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Otipulae_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P4p_SSSS_mod_2)),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_WILD_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_Vg_P4p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_liab_Otipulae_P4p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Otipulae_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P4p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_WILD_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_Vg_P4p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Otipulae_P4p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Otipulae_P4p_SSSS_2 <- rbind(effectiveSize(trait_mean_liab_Otipulae_CONTROL_P4p_SSSS_mod_2),effectiveSize(trait_mean_liab_Otipulae_WILD_P4p_SSSS_mod),effectiveSize(trait_mean_liab_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(effectiveSize_trait_mean_liab_Otipulae_P4p_SSSS_2) <- c("effectiveSize")
        trait_mean_liab_Otipulae_P4p_SSSS_2 <- cbind.data.frame(mean_trait_mean_liab_Otipulae_P4p_SSSS_2,median_trait_mean_liab_Otipulae_P4p_SSSS_2,posterior.mode_trait_mean_liab_Otipulae_P4p_SSSS_2,HPDinterval_0.95_trait_mean_liab_Otipulae_P4p_SSSS_2,HPDinterval_0.83_trait_mean_liab_Otipulae_P4p_SSSS_2,effectiveSize_trait_mean_liab_Otipulae_P4p_SSSS_2)
        rownames(trait_mean_liab_Otipulae_P4p_SSSS_2) <- c("trait_mean_liab_Otipulae_CONTROL_P4p_SSSS_mod_2","trait_mean_liab_Otipulae_WILD_P4p_SSSS_mod","trait_mean_liab_Otipulae_Vg_P4p_SSSS_mod_2")
        trait_mean_liab_Otipulae_P4p_SSSS_2 <- cbind(Models = rownames(trait_mean_liab_Otipulae_P4p_SSSS_2),trait_mean_liab_Otipulae_P4p_SSSS_2)
        rownames(trait_mean_liab_Otipulae_P4p_SSSS_2) <- NULL
        trait_mean_liab_Otipulae_P4p_SSSS_2$Pnp <- c("P4.p","P4.p","P4.p")
        trait_mean_liab_Otipulae_P4p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_liab_Otipulae_P4p_SSSS_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Otipulae_P4p_SSSS_2$Scale <- c("liab","liab","liab")
        trait_mean_liab_Otipulae_P4p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_liab_Otipulae_P4p_SSSS_2
      }
      
      liab_Otipulae_P4p_SSSS_2 <- rbind.data.frame(va_liab_Otipulae_P4p_SSSS_2, h2_liab_Otipulae_P4p_SSSS_2,Evol_liab_Otipulae_P4p_SSSS_2,trait_mean_liab_Otipulae_P4p_SSSS_2)
      liab_Otipulae_P4p_SSSS_2
    }
    #Summary data scale Otipulae P4p
    {
      #Summary va_data_Otipulae_P4p_SSSS_2:NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        mean_va_data_Otipulae_P4p_SSSS_2 <- rbind(mean(va_data_Otipulae_CONTROL_P4p_SSSS_mod_2/2),mean(va_data_Otipulae_WILD_P4p_SSSS_mod/2),mean(va_data_Otipulae_Vg_P4p_SSSS_mod_2/2))
        colnames(mean_va_data_Otipulae_P4p_SSSS_2) <- c("mean")
        median_va_data_Otipulae_P4p_SSSS_2 <- rbind(median(va_data_Otipulae_CONTROL_P4p_SSSS_mod_2/2),median(va_data_Otipulae_WILD_P4p_SSSS_mod/2),median(va_data_Otipulae_Vg_P4p_SSSS_mod_2/2))
        colnames(median_va_data_Otipulae_P4p_SSSS_2) <- c("median")
        posterior.mode_va_data_Otipulae_P4p_SSSS_2 <- rbind(posterior.mode(as.mcmc(va_data_Otipulae_CONTROL_P4p_SSSS_mod_2/2)),posterior.mode(as.mcmc(va_data_Otipulae_WILD_P4p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_Otipulae_Vg_P4p_SSSS_mod_2/2)))
        colnames(posterior.mode_va_data_Otipulae_P4p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Otipulae_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(va_data_Otipulae_CONTROL_P4p_SSSS_mod_2/2)),HPDinterval(as.mcmc(va_data_Otipulae_WILD_P4p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_Otipulae_Vg_P4p_SSSS_mod_2/2)))
        colnames(HPDinterval_0.95_va_data_Otipulae_P4p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Otipulae_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(va_data_Otipulae_CONTROL_P4p_SSSS_mod_2/2),prob=.83),HPDinterval(as.mcmc(va_data_Otipulae_WILD_P4p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Otipulae_Vg_P4p_SSSS_mod_2/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Otipulae_P4p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Otipulae_P4p_SSSS_2 <- rbind(effectiveSize(va_data_Otipulae_CONTROL_P4p_SSSS_mod_2/2),effectiveSize(va_data_Otipulae_WILD_P4p_SSSS_mod/2),effectiveSize(va_data_Otipulae_Vg_P4p_SSSS_mod_2/2))
        colnames(effectiveSize_va_data_Otipulae_P4p_SSSS_2) <- c("effectiveSize")
        va_data_Otipulae_P4p_SSSS_2 <- cbind.data.frame(mean_va_data_Otipulae_P4p_SSSS_2,median_va_data_Otipulae_P4p_SSSS_2,posterior.mode_va_data_Otipulae_P4p_SSSS_2,HPDinterval_0.95_va_data_Otipulae_P4p_SSSS_2,HPDinterval_0.83_va_data_Otipulae_P4p_SSSS_2,effectiveSize_va_data_Otipulae_P4p_SSSS_2)
        rownames(va_data_Otipulae_P4p_SSSS_2) <- c("va_data_Otipulae_CONTROL_P4p_SSSS_mod_2","va_data_Otipulae_WILD_P4p_SSSS_mod","va_data_Otipulae_Vg_P4p_SSSS_mod_2")
        va_data_Otipulae_P4p_SSSS_2 <- cbind(Models = rownames(va_data_Otipulae_P4p_SSSS_2),va_data_Otipulae_P4p_SSSS_2)
        rownames(va_data_Otipulae_P4p_SSSS_2) <- NULL
        va_data_Otipulae_P4p_SSSS_2$Pnp <- c("P4.p","P4.p","P4.p")
        va_data_Otipulae_P4p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        va_data_Otipulae_P4p_SSSS_2$Measure <- c("Va","Va","Va")
        va_data_Otipulae_P4p_SSSS_2$Scale <- c("data","data","data")
        va_data_Otipulae_P4p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        va_data_Otipulae_P4p_SSSS_2
      }
      
      #Summary h2_data_Otipulae_P4p_SSSS_2
      {
        mean_h2_data_Otipulae_P4p_SSSS_2 <- rbind(mean(h2_data_Otipulae_CONTROL_P4p_SSSS_mod_2),mean(h2_data_Otipulae_WILD_P4p_SSSS_mod),mean(h2_data_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(mean_h2_data_Otipulae_P4p_SSSS_2) <- c("mean")
        median_h2_data_Otipulae_P4p_SSSS_2 <- rbind(median(h2_data_Otipulae_CONTROL_P4p_SSSS_mod_2),median(h2_data_Otipulae_WILD_P4p_SSSS_mod),median(h2_data_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(median_h2_data_Otipulae_P4p_SSSS_2) <- c("median")
        posterior.mode_h2_data_Otipulae_P4p_SSSS_2 <- rbind(posterior.mode(as.mcmc(h2_data_Otipulae_CONTROL_P4p_SSSS_mod_2)),posterior.mode(as.mcmc(h2_data_Otipulae_WILD_P4p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_Otipulae_Vg_P4p_SSSS_mod_2)))
        colnames(posterior.mode_h2_data_Otipulae_P4p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Otipulae_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(h2_data_Otipulae_CONTROL_P4p_SSSS_mod_2)),HPDinterval(as.mcmc(h2_data_Otipulae_WILD_P4p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_Otipulae_Vg_P4p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_h2_data_Otipulae_P4p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Otipulae_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(h2_data_Otipulae_CONTROL_P4p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(h2_data_Otipulae_WILD_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Otipulae_Vg_P4p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Otipulae_P4p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Otipulae_P4p_SSSS_2 <- rbind(effectiveSize(h2_data_Otipulae_CONTROL_P4p_SSSS_mod_2),effectiveSize(h2_data_Otipulae_WILD_P4p_SSSS_mod),effectiveSize(h2_data_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(effectiveSize_h2_data_Otipulae_P4p_SSSS_2) <- c("effectiveSize")
        h2_data_Otipulae_P4p_SSSS_2 <- cbind.data.frame(mean_h2_data_Otipulae_P4p_SSSS_2,median_h2_data_Otipulae_P4p_SSSS_2,posterior.mode_h2_data_Otipulae_P4p_SSSS_2,HPDinterval_0.95_h2_data_Otipulae_P4p_SSSS_2,HPDinterval_0.83_h2_data_Otipulae_P4p_SSSS_2,effectiveSize_h2_data_Otipulae_P4p_SSSS_2)
        rownames(h2_data_Otipulae_P4p_SSSS_2) <- c("h2_data_Otipulae_CONTROL_P4p_SSSS_mod_2","h2_data_Otipulae_WILD_P4p_SSSS_mod","h2_data_Otipulae_Vg_P4p_SSSS_mod_2")
        h2_data_Otipulae_P4p_SSSS_2 <- cbind(Models = rownames(h2_data_Otipulae_P4p_SSSS_2),h2_data_Otipulae_P4p_SSSS_2)
        rownames(h2_data_Otipulae_P4p_SSSS_2) <- NULL
        h2_data_Otipulae_P4p_SSSS_2$Pnp <- c("P4.p","P4.p","P4.p")
        h2_data_Otipulae_P4p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        h2_data_Otipulae_P4p_SSSS_2$Measure <- c("H2","H2","H2")
        h2_data_Otipulae_P4p_SSSS_2$Scale <- c("data","data","data")
        h2_data_Otipulae_P4p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        h2_data_Otipulae_P4p_SSSS_2
      }
      
      #Summary Evol_data_Otipulae_P4p_SSSS_2
      {
        mean_Evol_data_Otipulae_P4p_SSSS_2 <- rbind(mean(Evol_data_Otipulae_CONTROL_P4p_SSSS_mod_2),mean(Evol_data_Otipulae_WILD_P4p_SSSS_mod),mean(Evol_data_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(mean_Evol_data_Otipulae_P4p_SSSS_2) <- c("mean")
        median_Evol_data_Otipulae_P4p_SSSS_2 <- rbind(median(Evol_data_Otipulae_CONTROL_P4p_SSSS_mod_2),median(Evol_data_Otipulae_WILD_P4p_SSSS_mod),median(Evol_data_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(median_Evol_data_Otipulae_P4p_SSSS_2) <- c("median")
        posterior.mode_Evol_data_Otipulae_P4p_SSSS_2 <- rbind(posterior.mode(as.mcmc(Evol_data_Otipulae_CONTROL_P4p_SSSS_mod_2)),posterior.mode(as.mcmc(Evol_data_Otipulae_WILD_P4p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_Otipulae_Vg_P4p_SSSS_mod_2)))
        colnames(posterior.mode_Evol_data_Otipulae_P4p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Otipulae_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Otipulae_CONTROL_P4p_SSSS_mod_2)),HPDinterval(as.mcmc(Evol_data_Otipulae_WILD_P4p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_Otipulae_Vg_P4p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_Evol_data_Otipulae_P4p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Otipulae_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Otipulae_CONTROL_P4p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(Evol_data_Otipulae_WILD_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Otipulae_Vg_P4p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Otipulae_P4p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Otipulae_P4p_SSSS_2 <- rbind(effectiveSize(Evol_data_Otipulae_CONTROL_P4p_SSSS_mod_2),effectiveSize(Evol_data_Otipulae_WILD_P4p_SSSS_mod),effectiveSize(Evol_data_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(effectiveSize_Evol_data_Otipulae_P4p_SSSS_2) <- c("effectiveSize")
        Evol_data_Otipulae_P4p_SSSS_2 <- cbind.data.frame(mean_Evol_data_Otipulae_P4p_SSSS_2,median_Evol_data_Otipulae_P4p_SSSS_2,posterior.mode_Evol_data_Otipulae_P4p_SSSS_2,HPDinterval_0.95_Evol_data_Otipulae_P4p_SSSS_2,HPDinterval_0.83_Evol_data_Otipulae_P4p_SSSS_2,effectiveSize_Evol_data_Otipulae_P4p_SSSS_2)
        rownames(Evol_data_Otipulae_P4p_SSSS_2) <- c("Evol_data_Otipulae_CONTROL_P4p_SSSS_mod_2","Evol_data_Otipulae_WILD_P4p_SSSS_mod","Evol_data_Otipulae_Vg_P4p_SSSS_mod_2")
        Evol_data_Otipulae_P4p_SSSS_2 <- cbind(Models = rownames(Evol_data_Otipulae_P4p_SSSS_2),Evol_data_Otipulae_P4p_SSSS_2)
        rownames(Evol_data_Otipulae_P4p_SSSS_2) <- NULL
        Evol_data_Otipulae_P4p_SSSS_2$Pnp <- c("P4.p","P4.p","P4.p")
        Evol_data_Otipulae_P4p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        Evol_data_Otipulae_P4p_SSSS_2$Measure <- c("Evol","Evol","Evol")
        Evol_data_Otipulae_P4p_SSSS_2$Scale <- c("data","data","data")
        Evol_data_Otipulae_P4p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        Evol_data_Otipulae_P4p_SSSS_2
      }
      
      #Summary trait_mean_data_Otipulae_P4p_SSSS_2
      {
        mean_trait_mean_data_Otipulae_P4p_SSSS_2 <- rbind(mean(trait_mean_data_Otipulae_CONTROL_P4p_SSSS_mod_2),mean(trait_mean_data_Otipulae_WILD_P4p_SSSS_mod),mean(trait_mean_data_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(mean_trait_mean_data_Otipulae_P4p_SSSS_2) <- c("mean")
        median_trait_mean_data_Otipulae_P4p_SSSS_2 <- rbind(median(trait_mean_data_Otipulae_CONTROL_P4p_SSSS_mod_2),median(trait_mean_data_Otipulae_WILD_P4p_SSSS_mod),median(trait_mean_data_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(median_trait_mean_data_Otipulae_P4p_SSSS_2) <- c("median")
        posterior.mode_trait_mean_data_Otipulae_P4p_SSSS_2 <- rbind(posterior.mode(as.mcmc(trait_mean_data_Otipulae_CONTROL_P4p_SSSS_mod_2)),posterior.mode(as.mcmc(trait_mean_data_Otipulae_WILD_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_Otipulae_Vg_P4p_SSSS_mod_2)))
        colnames(posterior.mode_trait_mean_data_Otipulae_P4p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Otipulae_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Otipulae_CONTROL_P4p_SSSS_mod_2)),HPDinterval(as.mcmc(trait_mean_data_Otipulae_WILD_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_Otipulae_Vg_P4p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_data_Otipulae_P4p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Otipulae_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Otipulae_CONTROL_P4p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Otipulae_WILD_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Otipulae_Vg_P4p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Otipulae_P4p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Otipulae_P4p_SSSS_2 <- rbind(effectiveSize(trait_mean_data_Otipulae_CONTROL_P4p_SSSS_mod_2),effectiveSize(trait_mean_data_Otipulae_WILD_P4p_SSSS_mod),effectiveSize(trait_mean_data_Otipulae_Vg_P4p_SSSS_mod_2))
        colnames(effectiveSize_trait_mean_data_Otipulae_P4p_SSSS_2) <- c("effectiveSize")
        trait_mean_data_Otipulae_P4p_SSSS_2 <- cbind.data.frame(mean_trait_mean_data_Otipulae_P4p_SSSS_2,median_trait_mean_data_Otipulae_P4p_SSSS_2,posterior.mode_trait_mean_data_Otipulae_P4p_SSSS_2,HPDinterval_0.95_trait_mean_data_Otipulae_P4p_SSSS_2,HPDinterval_0.83_trait_mean_data_Otipulae_P4p_SSSS_2,effectiveSize_trait_mean_data_Otipulae_P4p_SSSS_2)
        rownames(trait_mean_data_Otipulae_P4p_SSSS_2) <- c("trait_mean_data_Otipulae_CONTROL_P4p_SSSS_mod_2","trait_mean_data_Otipulae_WILD_P4p_SSSS_mod","trait_mean_data_Otipulae_Vg_P4p_SSSS_mod_2")
        trait_mean_data_Otipulae_P4p_SSSS_2 <- cbind(Models = rownames(trait_mean_data_Otipulae_P4p_SSSS_2),trait_mean_data_Otipulae_P4p_SSSS_2)
        rownames(trait_mean_data_Otipulae_P4p_SSSS_2) <- NULL
        trait_mean_data_Otipulae_P4p_SSSS_2$Pnp <- c("P4.p","P4.p","P4.p")
        trait_mean_data_Otipulae_P4p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_data_Otipulae_P4p_SSSS_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Otipulae_P4p_SSSS_2$Scale <- c("data","data","data")
        trait_mean_data_Otipulae_P4p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_data_Otipulae_P4p_SSSS_2
      }
      
      data_Otipulae_P4p_SSSS_2 <- rbind.data.frame(va_data_Otipulae_P4p_SSSS_2, h2_data_Otipulae_P4p_SSSS_2,Evol_data_Otipulae_P4p_SSSS_2,trait_mean_data_Otipulae_P4p_SSSS_2)
      data_Otipulae_P4p_SSSS_2
      
    }
    Vg_Otipulae_P4p_SSSS_2 <- rbind.data.frame(liab_Otipulae_P4p_SSSS_2, data_Otipulae_P4p_SSSS_2)
    Vg_Otipulae_P4p_SSSS_2$Pnp_fate <- rep("SSSS", 24)
    Vg_Otipulae_P4p_SSSS_2
    #remove Otipulae P4p_SSSS_2 models
    {
      remove(Otipulae_CONTROL_P4p_SSSS_mod_2)
      remove(Otipulae_WILD_P4p_SSSS_mod)
      remove(Otipulae_Vg_P4p_SSSS_mod_2)
    }
  }
  
  ##Summary Otipulae P8p ----
  {
    #Summary liability scale Otipulae P8p
    {
      #Summary va_liab_Otipulae_P8p_SSSS_2: NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        
        mean_va_liab_Otipulae_P8p_SSSS_2 <- rbind(mean(va_liab_Otipulae_CONTROL_P8p_SSSS_mod_2/2),mean(va_liab_Otipulae_WILD_P8p_SSSS_mod/2),mean(va_liab_Otipulae_Vg_P8p_SSSS_mod_2/2))
        colnames(mean_va_liab_Otipulae_P8p_SSSS_2) <- c("mean")
        median_va_liab_Otipulae_P8p_SSSS_2 <- rbind(median(va_liab_Otipulae_CONTROL_P8p_SSSS_mod_2/2),median(va_liab_Otipulae_WILD_P8p_SSSS_mod/2),median(va_liab_Otipulae_Vg_P8p_SSSS_mod_2/2))
        colnames(median_va_liab_Otipulae_P8p_SSSS_2) <- c("median")
        posterior.mode_va_liab_Otipulae_P8p_SSSS_2 <- rbind(posterior.mode(va_liab_Otipulae_CONTROL_P8p_SSSS_mod_2/2),posterior.mode(va_liab_Otipulae_WILD_P8p_SSSS_mod/2),posterior.mode(va_liab_Otipulae_Vg_P8p_SSSS_mod_2/2))
        colnames(posterior.mode_va_liab_Otipulae_P8p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Otipulae_P8p_SSSS_2 <- rbind(HPDinterval(va_liab_Otipulae_CONTROL_P8p_SSSS_mod_2/2),HPDinterval(va_liab_Otipulae_WILD_P8p_SSSS_mod/2),HPDinterval(va_liab_Otipulae_Vg_P8p_SSSS_mod_2/2))
        colnames(HPDinterval_0.95_va_liab_Otipulae_P8p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Otipulae_P8p_SSSS_2 <- rbind(HPDinterval(va_liab_Otipulae_CONTROL_P8p_SSSS_mod_2/2,prob=.83),HPDinterval(va_liab_Otipulae_WILD_P8p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_Otipulae_Vg_P8p_SSSS_mod_2/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Otipulae_P8p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Otipulae_P8p_SSSS_2 <- rbind(effectiveSize(va_liab_Otipulae_CONTROL_P8p_SSSS_mod_2/2),effectiveSize(va_liab_Otipulae_WILD_P8p_SSSS_mod/2),effectiveSize(va_liab_Otipulae_Vg_P8p_SSSS_mod_2/2))
        colnames(effectiveSize_va_liab_Otipulae_P8p_SSSS_2) <- c("effectiveSize")
        va_liab_Otipulae_P8p_SSSS_2 <- cbind.data.frame(mean_va_liab_Otipulae_P8p_SSSS_2,median_va_liab_Otipulae_P8p_SSSS_2,posterior.mode_va_liab_Otipulae_P8p_SSSS_2,HPDinterval_0.95_va_liab_Otipulae_P8p_SSSS_2,HPDinterval_0.83_va_liab_Otipulae_P8p_SSSS_2,effectiveSize_va_liab_Otipulae_P8p_SSSS_2)
        rownames(va_liab_Otipulae_P8p_SSSS_2) <- c("va_liab_Otipulae_CONTROL_P8p_SSSS_mod_2","va_liab_Otipulae_WILD_P8p_SSSS_mod","va_liab_Otipulae_Vg_P8p_SSSS_mod_2")
        va_liab_Otipulae_P8p_SSSS_2 <- cbind(Models = rownames(va_liab_Otipulae_P8p_SSSS_2),va_liab_Otipulae_P8p_SSSS_2)
        rownames(va_liab_Otipulae_P8p_SSSS_2) <- NULL
        va_liab_Otipulae_P8p_SSSS_2$Pnp <- c("P8.p","P8.p","P8.p")
        va_liab_Otipulae_P8p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        va_liab_Otipulae_P8p_SSSS_2$Measure <- c("Va","Va","Va")
        va_liab_Otipulae_P8p_SSSS_2$Scale <- c("liab","liab","liab")
        va_liab_Otipulae_P8p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        va_liab_Otipulae_P8p_SSSS_2
      }
      
      #Summary h2_liab_Otipulae_P8p_SSSS_2
      {
        mean_h2_liab_Otipulae_P8p_SSSS_2 <- rbind(mean(h2_liab_Otipulae_CONTROL_P8p_SSSS_mod_2),mean(h2_liab_Otipulae_WILD_P8p_SSSS_mod),mean(h2_liab_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(mean_h2_liab_Otipulae_P8p_SSSS_2) <- c("mean")
        median_h2_liab_Otipulae_P8p_SSSS_2 <- rbind(median(h2_liab_Otipulae_CONTROL_P8p_SSSS_mod_2),median(h2_liab_Otipulae_WILD_P8p_SSSS_mod),median(h2_liab_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(median_h2_liab_Otipulae_P8p_SSSS_2) <- c("median")
        posterior.mode_h2_liab_Otipulae_P8p_SSSS_2 <- rbind(posterior.mode(h2_liab_Otipulae_CONTROL_P8p_SSSS_mod_2),posterior.mode(h2_liab_Otipulae_WILD_P8p_SSSS_mod),posterior.mode(h2_liab_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(posterior.mode_h2_liab_Otipulae_P8p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Otipulae_P8p_SSSS_2 <- rbind(HPDinterval(h2_liab_Otipulae_CONTROL_P8p_SSSS_mod_2),HPDinterval(h2_liab_Otipulae_WILD_P8p_SSSS_mod),HPDinterval(h2_liab_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(HPDinterval_0.95_h2_liab_Otipulae_P8p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Otipulae_P8p_SSSS_2 <- rbind(HPDinterval(h2_liab_Otipulae_CONTROL_P8p_SSSS_mod_2,prob=.83),HPDinterval(h2_liab_Otipulae_WILD_P8p_SSSS_mod,prob=.83),HPDinterval(h2_liab_Otipulae_Vg_P8p_SSSS_mod_2,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Otipulae_P8p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Otipulae_P8p_SSSS_2 <- rbind(effectiveSize(h2_liab_Otipulae_CONTROL_P8p_SSSS_mod_2),effectiveSize(h2_liab_Otipulae_WILD_P8p_SSSS_mod),effectiveSize(h2_liab_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(effectiveSize_h2_liab_Otipulae_P8p_SSSS_2) <- c("effectiveSize")
        h2_liab_Otipulae_P8p_SSSS_2 <- cbind.data.frame(mean_h2_liab_Otipulae_P8p_SSSS_2,median_h2_liab_Otipulae_P8p_SSSS_2,posterior.mode_h2_liab_Otipulae_P8p_SSSS_2,HPDinterval_0.95_h2_liab_Otipulae_P8p_SSSS_2,HPDinterval_0.83_h2_liab_Otipulae_P8p_SSSS_2,effectiveSize_h2_liab_Otipulae_P8p_SSSS_2)
        rownames(h2_liab_Otipulae_P8p_SSSS_2) <- c("h2_liab_Otipulae_CONTROL_P8p_SSSS_mod_2","h2_liab_Otipulae_WILD_P8p_SSSS_mod","h2_liab_Otipulae_Vg_P8p_SSSS_mod_2")
        h2_liab_Otipulae_P8p_SSSS_2 <- cbind(Models = rownames(h2_liab_Otipulae_P8p_SSSS_2),h2_liab_Otipulae_P8p_SSSS_2)
        rownames(h2_liab_Otipulae_P8p_SSSS_2) <- NULL
        h2_liab_Otipulae_P8p_SSSS_2$Pnp <- c("P8.p","P8.p","P8.p")
        h2_liab_Otipulae_P8p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        h2_liab_Otipulae_P8p_SSSS_2$Measure <- c("H2","H2","H2")
        h2_liab_Otipulae_P8p_SSSS_2$Scale <- c("liab","liab","liab")
        h2_liab_Otipulae_P8p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        h2_liab_Otipulae_P8p_SSSS_2
      }
      
      #Summary Evol_liab_Otipulae_P8p_SSSS_2
      {
        mean_Evol_liab_Otipulae_P8p_SSSS_2 <- rbind(mean(Evol_liab_Otipulae_CONTROL_P8p_SSSS_mod_2),mean(Evol_liab_Otipulae_WILD_P8p_SSSS_mod),mean(Evol_liab_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(mean_Evol_liab_Otipulae_P8p_SSSS_2) <- c("mean")
        median_Evol_liab_Otipulae_P8p_SSSS_2 <- rbind(median(Evol_liab_Otipulae_CONTROL_P8p_SSSS_mod_2),median(Evol_liab_Otipulae_WILD_P8p_SSSS_mod),median(Evol_liab_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(median_Evol_liab_Otipulae_P8p_SSSS_2) <- c("median")
        posterior.mode_Evol_liab_Otipulae_P8p_SSSS_2 <- rbind(posterior.mode(Evol_liab_Otipulae_CONTROL_P8p_SSSS_mod_2),posterior.mode(Evol_liab_Otipulae_WILD_P8p_SSSS_mod),posterior.mode(Evol_liab_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(posterior.mode_Evol_liab_Otipulae_P8p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Otipulae_P8p_SSSS_2 <- rbind(HPDinterval(Evol_liab_Otipulae_CONTROL_P8p_SSSS_mod_2),HPDinterval(Evol_liab_Otipulae_WILD_P8p_SSSS_mod),HPDinterval(Evol_liab_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(HPDinterval_0.95_Evol_liab_Otipulae_P8p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Otipulae_P8p_SSSS_2 <- rbind(HPDinterval(Evol_liab_Otipulae_CONTROL_P8p_SSSS_mod_2,prob=.83),HPDinterval(Evol_liab_Otipulae_WILD_P8p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_Otipulae_Vg_P8p_SSSS_mod_2,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Otipulae_P8p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Otipulae_P8p_SSSS_2 <- rbind(effectiveSize(Evol_liab_Otipulae_CONTROL_P8p_SSSS_mod_2),effectiveSize(Evol_liab_Otipulae_WILD_P8p_SSSS_mod),effectiveSize(Evol_liab_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(effectiveSize_Evol_liab_Otipulae_P8p_SSSS_2) <- c("effectiveSize")
        Evol_liab_Otipulae_P8p_SSSS_2 <- cbind.data.frame(mean_Evol_liab_Otipulae_P8p_SSSS_2,median_Evol_liab_Otipulae_P8p_SSSS_2,posterior.mode_Evol_liab_Otipulae_P8p_SSSS_2,HPDinterval_0.95_Evol_liab_Otipulae_P8p_SSSS_2,HPDinterval_0.83_Evol_liab_Otipulae_P8p_SSSS_2,effectiveSize_Evol_liab_Otipulae_P8p_SSSS_2)
        rownames(Evol_liab_Otipulae_P8p_SSSS_2) <- c("Evol_liab_Otipulae_CONTROL_P8p_SSSS_mod_2","Evol_liab_Otipulae_WILD_P8p_SSSS_mod","Evol_liab_Otipulae_Vg_P8p_SSSS_mod_2")
        Evol_liab_Otipulae_P8p_SSSS_2 <- cbind(Models = rownames(Evol_liab_Otipulae_P8p_SSSS_2),Evol_liab_Otipulae_P8p_SSSS_2)
        rownames(Evol_liab_Otipulae_P8p_SSSS_2) <- NULL
        Evol_liab_Otipulae_P8p_SSSS_2$Pnp <- c("P8.p","P8.p","P8.p")
        Evol_liab_Otipulae_P8p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        Evol_liab_Otipulae_P8p_SSSS_2$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Otipulae_P8p_SSSS_2$Scale <- c("liab","liab","liab")
        Evol_liab_Otipulae_P8p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        Evol_liab_Otipulae_P8p_SSSS_2
      }
      
      #Summary trait_mean_liab_Otipulae_P8p_SSSS_2
      {
        mean_trait_mean_liab_Otipulae_P8p_SSSS_2 <- rbind(mean(trait_mean_liab_Otipulae_CONTROL_P8p_SSSS_mod_2),mean(trait_mean_liab_Otipulae_WILD_P8p_SSSS_mod),mean(trait_mean_liab_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(mean_trait_mean_liab_Otipulae_P8p_SSSS_2) <- c("mean")
        median_trait_mean_liab_Otipulae_P8p_SSSS_2 <- rbind(median(trait_mean_liab_Otipulae_CONTROL_P8p_SSSS_mod_2),median(trait_mean_liab_Otipulae_WILD_P8p_SSSS_mod),median(trait_mean_liab_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(median_trait_mean_liab_Otipulae_P8p_SSSS_2) <- c("median")
        posterior.mode_trait_mean_liab_Otipulae_P8p_SSSS_2 <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P8p_SSSS_mod_2)),posterior.mode(as.mcmc(trait_mean_liab_Otipulae_WILD_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Otipulae_Vg_P8p_SSSS_mod_2)))
        colnames(posterior.mode_trait_mean_liab_Otipulae_P8p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Otipulae_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P8p_SSSS_mod_2)),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_WILD_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_Vg_P8p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_liab_Otipulae_P8p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Otipulae_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P8p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_WILD_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_Vg_P8p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Otipulae_P8p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Otipulae_P8p_SSSS_2 <- rbind(effectiveSize(trait_mean_liab_Otipulae_CONTROL_P8p_SSSS_mod_2),effectiveSize(trait_mean_liab_Otipulae_WILD_P8p_SSSS_mod),effectiveSize(trait_mean_liab_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(effectiveSize_trait_mean_liab_Otipulae_P8p_SSSS_2) <- c("effectiveSize")
        trait_mean_liab_Otipulae_P8p_SSSS_2 <- cbind.data.frame(mean_trait_mean_liab_Otipulae_P8p_SSSS_2,median_trait_mean_liab_Otipulae_P8p_SSSS_2,posterior.mode_trait_mean_liab_Otipulae_P8p_SSSS_2,HPDinterval_0.95_trait_mean_liab_Otipulae_P8p_SSSS_2,HPDinterval_0.83_trait_mean_liab_Otipulae_P8p_SSSS_2,effectiveSize_trait_mean_liab_Otipulae_P8p_SSSS_2)
        rownames(trait_mean_liab_Otipulae_P8p_SSSS_2) <- c("trait_mean_liab_Otipulae_CONTROL_P8p_SSSS_mod_2","trait_mean_liab_Otipulae_WILD_P8p_SSSS_mod","trait_mean_liab_Otipulae_Vg_P8p_SSSS_mod_2")
        trait_mean_liab_Otipulae_P8p_SSSS_2 <- cbind(Models = rownames(trait_mean_liab_Otipulae_P8p_SSSS_2),trait_mean_liab_Otipulae_P8p_SSSS_2)
        rownames(trait_mean_liab_Otipulae_P8p_SSSS_2) <- NULL
        trait_mean_liab_Otipulae_P8p_SSSS_2$Pnp <- c("P8.p","P8.p","P8.p")
        trait_mean_liab_Otipulae_P8p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_liab_Otipulae_P8p_SSSS_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Otipulae_P8p_SSSS_2$Scale <- c("liab","liab","liab")
        trait_mean_liab_Otipulae_P8p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_liab_Otipulae_P8p_SSSS_2
      }
      
      liab_Otipulae_P8p_SSSS_2 <- rbind.data.frame(va_liab_Otipulae_P8p_SSSS_2, h2_liab_Otipulae_P8p_SSSS_2,Evol_liab_Otipulae_P8p_SSSS_2,trait_mean_liab_Otipulae_P8p_SSSS_2)
      liab_Otipulae_P8p_SSSS_2
    }
    #Summary data scale Otipulae P8p
    {
      #Summary va_data_Otipulae_P8p_SSSS_2:NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        mean_va_data_Otipulae_P8p_SSSS_2 <- rbind(mean(va_data_Otipulae_CONTROL_P8p_SSSS_mod_2/2),mean(va_data_Otipulae_WILD_P8p_SSSS_mod/2),mean(va_data_Otipulae_Vg_P8p_SSSS_mod_2/2))
        colnames(mean_va_data_Otipulae_P8p_SSSS_2) <- c("mean")
        median_va_data_Otipulae_P8p_SSSS_2 <- rbind(median(va_data_Otipulae_CONTROL_P8p_SSSS_mod_2/2),median(va_data_Otipulae_WILD_P8p_SSSS_mod/2),median(va_data_Otipulae_Vg_P8p_SSSS_mod_2/2))
        colnames(median_va_data_Otipulae_P8p_SSSS_2) <- c("median")
        posterior.mode_va_data_Otipulae_P8p_SSSS_2 <- rbind(posterior.mode(as.mcmc(va_data_Otipulae_CONTROL_P8p_SSSS_mod_2/2)),posterior.mode(as.mcmc(va_data_Otipulae_WILD_P8p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_Otipulae_Vg_P8p_SSSS_mod_2/2)))
        colnames(posterior.mode_va_data_Otipulae_P8p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Otipulae_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(va_data_Otipulae_CONTROL_P8p_SSSS_mod_2/2)),HPDinterval(as.mcmc(va_data_Otipulae_WILD_P8p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_Otipulae_Vg_P8p_SSSS_mod_2/2)))
        colnames(HPDinterval_0.95_va_data_Otipulae_P8p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Otipulae_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(va_data_Otipulae_CONTROL_P8p_SSSS_mod_2/2),prob=.83),HPDinterval(as.mcmc(va_data_Otipulae_WILD_P8p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Otipulae_Vg_P8p_SSSS_mod_2/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Otipulae_P8p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Otipulae_P8p_SSSS_2 <- rbind(effectiveSize(va_data_Otipulae_CONTROL_P8p_SSSS_mod_2/2),effectiveSize(va_data_Otipulae_WILD_P8p_SSSS_mod/2),effectiveSize(va_data_Otipulae_Vg_P8p_SSSS_mod_2/2))
        colnames(effectiveSize_va_data_Otipulae_P8p_SSSS_2) <- c("effectiveSize")
        va_data_Otipulae_P8p_SSSS_2 <- cbind.data.frame(mean_va_data_Otipulae_P8p_SSSS_2,median_va_data_Otipulae_P8p_SSSS_2,posterior.mode_va_data_Otipulae_P8p_SSSS_2,HPDinterval_0.95_va_data_Otipulae_P8p_SSSS_2,HPDinterval_0.83_va_data_Otipulae_P8p_SSSS_2,effectiveSize_va_data_Otipulae_P8p_SSSS_2)
        rownames(va_data_Otipulae_P8p_SSSS_2) <- c("va_data_Otipulae_CONTROL_P8p_SSSS_mod_2","va_data_Otipulae_WILD_P8p_SSSS_mod","va_data_Otipulae_Vg_P8p_SSSS_mod_2")
        va_data_Otipulae_P8p_SSSS_2 <- cbind(Models = rownames(va_data_Otipulae_P8p_SSSS_2),va_data_Otipulae_P8p_SSSS_2)
        rownames(va_data_Otipulae_P8p_SSSS_2) <- NULL
        va_data_Otipulae_P8p_SSSS_2$Pnp <- c("P8.p","P8.p","P8.p")
        va_data_Otipulae_P8p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        va_data_Otipulae_P8p_SSSS_2$Measure <- c("Va","Va","Va")
        va_data_Otipulae_P8p_SSSS_2$Scale <- c("data","data","data")
        va_data_Otipulae_P8p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        va_data_Otipulae_P8p_SSSS_2
      }
      
      #Summary h2_data_Otipulae_P8p_SSSS_2
      {
        mean_h2_data_Otipulae_P8p_SSSS_2 <- rbind(mean(h2_data_Otipulae_CONTROL_P8p_SSSS_mod_2),mean(h2_data_Otipulae_WILD_P8p_SSSS_mod),mean(h2_data_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(mean_h2_data_Otipulae_P8p_SSSS_2) <- c("mean")
        median_h2_data_Otipulae_P8p_SSSS_2 <- rbind(median(h2_data_Otipulae_CONTROL_P8p_SSSS_mod_2),median(h2_data_Otipulae_WILD_P8p_SSSS_mod),median(h2_data_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(median_h2_data_Otipulae_P8p_SSSS_2) <- c("median")
        posterior.mode_h2_data_Otipulae_P8p_SSSS_2 <- rbind(posterior.mode(as.mcmc(h2_data_Otipulae_CONTROL_P8p_SSSS_mod_2)),posterior.mode(as.mcmc(h2_data_Otipulae_WILD_P8p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_Otipulae_Vg_P8p_SSSS_mod_2)))
        colnames(posterior.mode_h2_data_Otipulae_P8p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Otipulae_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(h2_data_Otipulae_CONTROL_P8p_SSSS_mod_2)),HPDinterval(as.mcmc(h2_data_Otipulae_WILD_P8p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_Otipulae_Vg_P8p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_h2_data_Otipulae_P8p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Otipulae_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(h2_data_Otipulae_CONTROL_P8p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(h2_data_Otipulae_WILD_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Otipulae_Vg_P8p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Otipulae_P8p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Otipulae_P8p_SSSS_2 <- rbind(effectiveSize(h2_data_Otipulae_CONTROL_P8p_SSSS_mod_2),effectiveSize(h2_data_Otipulae_WILD_P8p_SSSS_mod),effectiveSize(h2_data_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(effectiveSize_h2_data_Otipulae_P8p_SSSS_2) <- c("effectiveSize")
        h2_data_Otipulae_P8p_SSSS_2 <- cbind.data.frame(mean_h2_data_Otipulae_P8p_SSSS_2,median_h2_data_Otipulae_P8p_SSSS_2,posterior.mode_h2_data_Otipulae_P8p_SSSS_2,HPDinterval_0.95_h2_data_Otipulae_P8p_SSSS_2,HPDinterval_0.83_h2_data_Otipulae_P8p_SSSS_2,effectiveSize_h2_data_Otipulae_P8p_SSSS_2)
        rownames(h2_data_Otipulae_P8p_SSSS_2) <- c("h2_data_Otipulae_CONTROL_P8p_SSSS_mod_2","h2_data_Otipulae_WILD_P8p_SSSS_mod","h2_data_Otipulae_Vg_P8p_SSSS_mod_2")
        h2_data_Otipulae_P8p_SSSS_2 <- cbind(Models = rownames(h2_data_Otipulae_P8p_SSSS_2),h2_data_Otipulae_P8p_SSSS_2)
        rownames(h2_data_Otipulae_P8p_SSSS_2) <- NULL
        h2_data_Otipulae_P8p_SSSS_2$Pnp <- c("P8.p","P8.p","P8.p")
        h2_data_Otipulae_P8p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        h2_data_Otipulae_P8p_SSSS_2$Measure <- c("H2","H2","H2")
        h2_data_Otipulae_P8p_SSSS_2$Scale <- c("data","data","data")
        h2_data_Otipulae_P8p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        h2_data_Otipulae_P8p_SSSS_2
      }
      
      #Summary Evol_data_Otipulae_P8p_SSSS_2
      {
        mean_Evol_data_Otipulae_P8p_SSSS_2 <- rbind(mean(Evol_data_Otipulae_CONTROL_P8p_SSSS_mod_2),mean(Evol_data_Otipulae_WILD_P8p_SSSS_mod),mean(Evol_data_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(mean_Evol_data_Otipulae_P8p_SSSS_2) <- c("mean")
        median_Evol_data_Otipulae_P8p_SSSS_2 <- rbind(median(Evol_data_Otipulae_CONTROL_P8p_SSSS_mod_2),median(Evol_data_Otipulae_WILD_P8p_SSSS_mod),median(Evol_data_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(median_Evol_data_Otipulae_P8p_SSSS_2) <- c("median")
        posterior.mode_Evol_data_Otipulae_P8p_SSSS_2 <- rbind(posterior.mode(as.mcmc(Evol_data_Otipulae_CONTROL_P8p_SSSS_mod_2)),posterior.mode(as.mcmc(Evol_data_Otipulae_WILD_P8p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_Otipulae_Vg_P8p_SSSS_mod_2)))
        colnames(posterior.mode_Evol_data_Otipulae_P8p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Otipulae_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Otipulae_CONTROL_P8p_SSSS_mod_2)),HPDinterval(as.mcmc(Evol_data_Otipulae_WILD_P8p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_Otipulae_Vg_P8p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_Evol_data_Otipulae_P8p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Otipulae_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Otipulae_CONTROL_P8p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(Evol_data_Otipulae_WILD_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Otipulae_Vg_P8p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Otipulae_P8p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Otipulae_P8p_SSSS_2 <- rbind(effectiveSize(Evol_data_Otipulae_CONTROL_P8p_SSSS_mod_2),effectiveSize(Evol_data_Otipulae_WILD_P8p_SSSS_mod),effectiveSize(Evol_data_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(effectiveSize_Evol_data_Otipulae_P8p_SSSS_2) <- c("effectiveSize")
        Evol_data_Otipulae_P8p_SSSS_2 <- cbind.data.frame(mean_Evol_data_Otipulae_P8p_SSSS_2,median_Evol_data_Otipulae_P8p_SSSS_2,posterior.mode_Evol_data_Otipulae_P8p_SSSS_2,HPDinterval_0.95_Evol_data_Otipulae_P8p_SSSS_2,HPDinterval_0.83_Evol_data_Otipulae_P8p_SSSS_2,effectiveSize_Evol_data_Otipulae_P8p_SSSS_2)
        rownames(Evol_data_Otipulae_P8p_SSSS_2) <- c("Evol_data_Otipulae_CONTROL_P8p_SSSS_mod_2","Evol_data_Otipulae_WILD_P8p_SSSS_mod","Evol_data_Otipulae_Vg_P8p_SSSS_mod_2")
        Evol_data_Otipulae_P8p_SSSS_2 <- cbind(Models = rownames(Evol_data_Otipulae_P8p_SSSS_2),Evol_data_Otipulae_P8p_SSSS_2)
        rownames(Evol_data_Otipulae_P8p_SSSS_2) <- NULL
        Evol_data_Otipulae_P8p_SSSS_2$Pnp <- c("P8.p","P8.p","P8.p")
        Evol_data_Otipulae_P8p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        Evol_data_Otipulae_P8p_SSSS_2$Measure <- c("Evol","Evol","Evol")
        Evol_data_Otipulae_P8p_SSSS_2$Scale <- c("data","data","data")
        Evol_data_Otipulae_P8p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        Evol_data_Otipulae_P8p_SSSS_2
      }
      
      #Summary trait_mean_data_Otipulae_P8p_SSSS_2
      {
        mean_trait_mean_data_Otipulae_P8p_SSSS_2 <- rbind(mean(trait_mean_data_Otipulae_CONTROL_P8p_SSSS_mod_2),mean(trait_mean_data_Otipulae_WILD_P8p_SSSS_mod),mean(trait_mean_data_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(mean_trait_mean_data_Otipulae_P8p_SSSS_2) <- c("mean")
        median_trait_mean_data_Otipulae_P8p_SSSS_2 <- rbind(median(trait_mean_data_Otipulae_CONTROL_P8p_SSSS_mod_2),median(trait_mean_data_Otipulae_WILD_P8p_SSSS_mod),median(trait_mean_data_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(median_trait_mean_data_Otipulae_P8p_SSSS_2) <- c("median")
        posterior.mode_trait_mean_data_Otipulae_P8p_SSSS_2 <- rbind(posterior.mode(as.mcmc(trait_mean_data_Otipulae_CONTROL_P8p_SSSS_mod_2)),posterior.mode(as.mcmc(trait_mean_data_Otipulae_WILD_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_Otipulae_Vg_P8p_SSSS_mod_2)))
        colnames(posterior.mode_trait_mean_data_Otipulae_P8p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Otipulae_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Otipulae_CONTROL_P8p_SSSS_mod_2)),HPDinterval(as.mcmc(trait_mean_data_Otipulae_WILD_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_Otipulae_Vg_P8p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_data_Otipulae_P8p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Otipulae_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Otipulae_CONTROL_P8p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Otipulae_WILD_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Otipulae_Vg_P8p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Otipulae_P8p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Otipulae_P8p_SSSS_2 <- rbind(effectiveSize(trait_mean_data_Otipulae_CONTROL_P8p_SSSS_mod_2),effectiveSize(trait_mean_data_Otipulae_WILD_P8p_SSSS_mod),effectiveSize(trait_mean_data_Otipulae_Vg_P8p_SSSS_mod_2))
        colnames(effectiveSize_trait_mean_data_Otipulae_P8p_SSSS_2) <- c("effectiveSize")
        trait_mean_data_Otipulae_P8p_SSSS_2 <- cbind.data.frame(mean_trait_mean_data_Otipulae_P8p_SSSS_2,median_trait_mean_data_Otipulae_P8p_SSSS_2,posterior.mode_trait_mean_data_Otipulae_P8p_SSSS_2,HPDinterval_0.95_trait_mean_data_Otipulae_P8p_SSSS_2,HPDinterval_0.83_trait_mean_data_Otipulae_P8p_SSSS_2,effectiveSize_trait_mean_data_Otipulae_P8p_SSSS_2)
        rownames(trait_mean_data_Otipulae_P8p_SSSS_2) <- c("trait_mean_data_Otipulae_CONTROL_P8p_SSSS_mod_2","trait_mean_data_Otipulae_WILD_P8p_SSSS_mod","trait_mean_data_Otipulae_Vg_P8p_SSSS_mod_2")
        trait_mean_data_Otipulae_P8p_SSSS_2 <- cbind(Models = rownames(trait_mean_data_Otipulae_P8p_SSSS_2),trait_mean_data_Otipulae_P8p_SSSS_2)
        rownames(trait_mean_data_Otipulae_P8p_SSSS_2) <- NULL
        trait_mean_data_Otipulae_P8p_SSSS_2$Pnp <- c("P8.p","P8.p","P8.p")
        trait_mean_data_Otipulae_P8p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_data_Otipulae_P8p_SSSS_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Otipulae_P8p_SSSS_2$Scale <- c("data","data","data")
        trait_mean_data_Otipulae_P8p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_data_Otipulae_P8p_SSSS_2
      }
      
      data_Otipulae_P8p_SSSS_2 <- rbind.data.frame(va_data_Otipulae_P8p_SSSS_2, h2_data_Otipulae_P8p_SSSS_2,Evol_data_Otipulae_P8p_SSSS_2,trait_mean_data_Otipulae_P8p_SSSS_2)
      data_Otipulae_P8p_SSSS_2
      
    }
    Vg_Otipulae_P8p_SSSS_2 <- rbind.data.frame(liab_Otipulae_P8p_SSSS_2, data_Otipulae_P8p_SSSS_2)
    Vg_Otipulae_P8p_SSSS_2$Pnp_fate <- rep("SSSS", 24)
    Vg_Otipulae_P8p_SSSS_2
    #remove Otipulae P8p_SSSS_2 models
    {
      remove(Otipulae_CONTROL_P8p_SSSS_mod_2)
      remove(Otipulae_WILD_P8p_SSSS_mod)
      remove(Otipulae_Vg_P8p_SSSS_mod_2)
    }
  }
  
  ##Summary Otipulae P5p ----
  {
    #Summary liability scale Otipulae P5p
    {
      #Summary va_liab_Otipulae_P5p_wt_2: NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        
        mean_va_liab_Otipulae_P5p_wt_2 <- rbind(mean(va_liab_Otipulae_CONTROL_P5p_wt_mod_2/2),mean(va_liab_Otipulae_WILD_P5p_wt_mod/2),mean(va_liab_Otipulae_Vg_P5p_wt_mod_2/2))
        colnames(mean_va_liab_Otipulae_P5p_wt_2) <- c("mean")
        median_va_liab_Otipulae_P5p_wt_2 <- rbind(median(va_liab_Otipulae_CONTROL_P5p_wt_mod_2/2),median(va_liab_Otipulae_WILD_P5p_wt_mod/2),median(va_liab_Otipulae_Vg_P5p_wt_mod_2/2))
        colnames(median_va_liab_Otipulae_P5p_wt_2) <- c("median")
        posterior.mode_va_liab_Otipulae_P5p_wt_2 <- rbind(posterior.mode(va_liab_Otipulae_CONTROL_P5p_wt_mod_2/2),posterior.mode(va_liab_Otipulae_WILD_P5p_wt_mod/2),posterior.mode(va_liab_Otipulae_Vg_P5p_wt_mod_2/2))
        colnames(posterior.mode_va_liab_Otipulae_P5p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Otipulae_P5p_wt_2 <- rbind(HPDinterval(va_liab_Otipulae_CONTROL_P5p_wt_mod_2/2),HPDinterval(va_liab_Otipulae_WILD_P5p_wt_mod/2),HPDinterval(va_liab_Otipulae_Vg_P5p_wt_mod_2/2))
        colnames(HPDinterval_0.95_va_liab_Otipulae_P5p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Otipulae_P5p_wt_2 <- rbind(HPDinterval(va_liab_Otipulae_CONTROL_P5p_wt_mod_2/2,prob=.83),HPDinterval(va_liab_Otipulae_WILD_P5p_wt_mod/2,prob=.83),HPDinterval(va_liab_Otipulae_Vg_P5p_wt_mod_2/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Otipulae_P5p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Otipulae_P5p_wt_2 <- rbind(effectiveSize(va_liab_Otipulae_CONTROL_P5p_wt_mod_2/2),effectiveSize(va_liab_Otipulae_WILD_P5p_wt_mod/2),effectiveSize(va_liab_Otipulae_Vg_P5p_wt_mod_2/2))
        colnames(effectiveSize_va_liab_Otipulae_P5p_wt_2) <- c("effectiveSize")
        va_liab_Otipulae_P5p_wt_2 <- cbind.data.frame(mean_va_liab_Otipulae_P5p_wt_2,median_va_liab_Otipulae_P5p_wt_2,posterior.mode_va_liab_Otipulae_P5p_wt_2,HPDinterval_0.95_va_liab_Otipulae_P5p_wt_2,HPDinterval_0.83_va_liab_Otipulae_P5p_wt_2,effectiveSize_va_liab_Otipulae_P5p_wt_2)
        rownames(va_liab_Otipulae_P5p_wt_2) <- c("va_liab_Otipulae_CONTROL_P5p_wt_mod_2","va_liab_Otipulae_WILD_P5p_wt_mod","va_liab_Otipulae_Vg_P5p_wt_mod_2")
        va_liab_Otipulae_P5p_wt_2 <- cbind(Models = rownames(va_liab_Otipulae_P5p_wt_2),va_liab_Otipulae_P5p_wt_2)
        rownames(va_liab_Otipulae_P5p_wt_2) <- NULL
        va_liab_Otipulae_P5p_wt_2$Pnp <- c("P5.p","P5.p","P5.p")
        va_liab_Otipulae_P5p_wt_2$Treatment <- c("Control","WILD","Vg")
        va_liab_Otipulae_P5p_wt_2$Measure <- c("Va","Va","Va")
        va_liab_Otipulae_P5p_wt_2$Scale <- c("liab","liab","liab")
        va_liab_Otipulae_P5p_wt_2$Variance <- c("Vg","Vg","Vg")
        va_liab_Otipulae_P5p_wt_2
      }
      
      #Summary h2_liab_Otipulae_P5p_wt_2
      {
        mean_h2_liab_Otipulae_P5p_wt_2 <- rbind(mean(h2_liab_Otipulae_CONTROL_P5p_wt_mod_2),mean(h2_liab_Otipulae_WILD_P5p_wt_mod),mean(h2_liab_Otipulae_Vg_P5p_wt_mod_2))
        colnames(mean_h2_liab_Otipulae_P5p_wt_2) <- c("mean")
        median_h2_liab_Otipulae_P5p_wt_2 <- rbind(median(h2_liab_Otipulae_CONTROL_P5p_wt_mod_2),median(h2_liab_Otipulae_WILD_P5p_wt_mod),median(h2_liab_Otipulae_Vg_P5p_wt_mod_2))
        colnames(median_h2_liab_Otipulae_P5p_wt_2) <- c("median")
        posterior.mode_h2_liab_Otipulae_P5p_wt_2 <- rbind(posterior.mode(h2_liab_Otipulae_CONTROL_P5p_wt_mod_2),posterior.mode(h2_liab_Otipulae_WILD_P5p_wt_mod),posterior.mode(h2_liab_Otipulae_Vg_P5p_wt_mod_2))
        colnames(posterior.mode_h2_liab_Otipulae_P5p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Otipulae_P5p_wt_2 <- rbind(HPDinterval(h2_liab_Otipulae_CONTROL_P5p_wt_mod_2),HPDinterval(h2_liab_Otipulae_WILD_P5p_wt_mod),HPDinterval(h2_liab_Otipulae_Vg_P5p_wt_mod_2))
        colnames(HPDinterval_0.95_h2_liab_Otipulae_P5p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Otipulae_P5p_wt_2 <- rbind(HPDinterval(h2_liab_Otipulae_CONTROL_P5p_wt_mod_2,prob=.83),HPDinterval(h2_liab_Otipulae_WILD_P5p_wt_mod,prob=.83),HPDinterval(h2_liab_Otipulae_Vg_P5p_wt_mod_2,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Otipulae_P5p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Otipulae_P5p_wt_2 <- rbind(effectiveSize(h2_liab_Otipulae_CONTROL_P5p_wt_mod_2),effectiveSize(h2_liab_Otipulae_WILD_P5p_wt_mod),effectiveSize(h2_liab_Otipulae_Vg_P5p_wt_mod_2))
        colnames(effectiveSize_h2_liab_Otipulae_P5p_wt_2) <- c("effectiveSize")
        h2_liab_Otipulae_P5p_wt_2 <- cbind.data.frame(mean_h2_liab_Otipulae_P5p_wt_2,median_h2_liab_Otipulae_P5p_wt_2,posterior.mode_h2_liab_Otipulae_P5p_wt_2,HPDinterval_0.95_h2_liab_Otipulae_P5p_wt_2,HPDinterval_0.83_h2_liab_Otipulae_P5p_wt_2,effectiveSize_h2_liab_Otipulae_P5p_wt_2)
        rownames(h2_liab_Otipulae_P5p_wt_2) <- c("h2_liab_Otipulae_CONTROL_P5p_wt_mod_2","h2_liab_Otipulae_WILD_P5p_wt_mod","h2_liab_Otipulae_Vg_P5p_wt_mod_2")
        h2_liab_Otipulae_P5p_wt_2 <- cbind(Models = rownames(h2_liab_Otipulae_P5p_wt_2),h2_liab_Otipulae_P5p_wt_2)
        rownames(h2_liab_Otipulae_P5p_wt_2) <- NULL
        h2_liab_Otipulae_P5p_wt_2$Pnp <- c("P5.p","P5.p","P5.p")
        h2_liab_Otipulae_P5p_wt_2$Treatment <- c("Control","WILD","Vg")
        h2_liab_Otipulae_P5p_wt_2$Measure <- c("H2","H2","H2")
        h2_liab_Otipulae_P5p_wt_2$Scale <- c("liab","liab","liab")
        h2_liab_Otipulae_P5p_wt_2$Variance <- c("Vg","Vg","Vg")
        h2_liab_Otipulae_P5p_wt_2
      }
      
      #Summary Evol_liab_Otipulae_P5p_wt_2
      {
        mean_Evol_liab_Otipulae_P5p_wt_2 <- rbind(mean(Evol_liab_Otipulae_CONTROL_P5p_wt_mod_2),mean(Evol_liab_Otipulae_WILD_P5p_wt_mod),mean(Evol_liab_Otipulae_Vg_P5p_wt_mod_2))
        colnames(mean_Evol_liab_Otipulae_P5p_wt_2) <- c("mean")
        median_Evol_liab_Otipulae_P5p_wt_2 <- rbind(median(Evol_liab_Otipulae_CONTROL_P5p_wt_mod_2),median(Evol_liab_Otipulae_WILD_P5p_wt_mod),median(Evol_liab_Otipulae_Vg_P5p_wt_mod_2))
        colnames(median_Evol_liab_Otipulae_P5p_wt_2) <- c("median")
        posterior.mode_Evol_liab_Otipulae_P5p_wt_2 <- rbind(posterior.mode(Evol_liab_Otipulae_CONTROL_P5p_wt_mod_2),posterior.mode(Evol_liab_Otipulae_WILD_P5p_wt_mod),posterior.mode(Evol_liab_Otipulae_Vg_P5p_wt_mod_2))
        colnames(posterior.mode_Evol_liab_Otipulae_P5p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Otipulae_P5p_wt_2 <- rbind(HPDinterval(Evol_liab_Otipulae_CONTROL_P5p_wt_mod_2),HPDinterval(Evol_liab_Otipulae_WILD_P5p_wt_mod),HPDinterval(Evol_liab_Otipulae_Vg_P5p_wt_mod_2))
        colnames(HPDinterval_0.95_Evol_liab_Otipulae_P5p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Otipulae_P5p_wt_2 <- rbind(HPDinterval(Evol_liab_Otipulae_CONTROL_P5p_wt_mod_2,prob=.83),HPDinterval(Evol_liab_Otipulae_WILD_P5p_wt_mod,prob=.83),HPDinterval(Evol_liab_Otipulae_Vg_P5p_wt_mod_2,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Otipulae_P5p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Otipulae_P5p_wt_2 <- rbind(effectiveSize(Evol_liab_Otipulae_CONTROL_P5p_wt_mod_2),effectiveSize(Evol_liab_Otipulae_WILD_P5p_wt_mod),effectiveSize(Evol_liab_Otipulae_Vg_P5p_wt_mod_2))
        colnames(effectiveSize_Evol_liab_Otipulae_P5p_wt_2) <- c("effectiveSize")
        Evol_liab_Otipulae_P5p_wt_2 <- cbind.data.frame(mean_Evol_liab_Otipulae_P5p_wt_2,median_Evol_liab_Otipulae_P5p_wt_2,posterior.mode_Evol_liab_Otipulae_P5p_wt_2,HPDinterval_0.95_Evol_liab_Otipulae_P5p_wt_2,HPDinterval_0.83_Evol_liab_Otipulae_P5p_wt_2,effectiveSize_Evol_liab_Otipulae_P5p_wt_2)
        rownames(Evol_liab_Otipulae_P5p_wt_2) <- c("Evol_liab_Otipulae_CONTROL_P5p_wt_mod_2","Evol_liab_Otipulae_WILD_P5p_wt_mod","Evol_liab_Otipulae_Vg_P5p_wt_mod_2")
        Evol_liab_Otipulae_P5p_wt_2 <- cbind(Models = rownames(Evol_liab_Otipulae_P5p_wt_2),Evol_liab_Otipulae_P5p_wt_2)
        rownames(Evol_liab_Otipulae_P5p_wt_2) <- NULL
        Evol_liab_Otipulae_P5p_wt_2$Pnp <- c("P5.p","P5.p","P5.p")
        Evol_liab_Otipulae_P5p_wt_2$Treatment <- c("Control","WILD","Vg")
        Evol_liab_Otipulae_P5p_wt_2$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Otipulae_P5p_wt_2$Scale <- c("liab","liab","liab")
        Evol_liab_Otipulae_P5p_wt_2$Variance <- c("Vg","Vg","Vg")
        Evol_liab_Otipulae_P5p_wt_2
      }
      
      #Summary trait_mean_liab_Otipulae_P5p_wt_2
      {
        mean_trait_mean_liab_Otipulae_P5p_wt_2 <- rbind(mean(trait_mean_liab_Otipulae_CONTROL_P5p_wt_mod_2),mean(trait_mean_liab_Otipulae_WILD_P5p_wt_mod),mean(trait_mean_liab_Otipulae_Vg_P5p_wt_mod_2))
        colnames(mean_trait_mean_liab_Otipulae_P5p_wt_2) <- c("mean")
        median_trait_mean_liab_Otipulae_P5p_wt_2 <- rbind(median(trait_mean_liab_Otipulae_CONTROL_P5p_wt_mod_2),median(trait_mean_liab_Otipulae_WILD_P5p_wt_mod),median(trait_mean_liab_Otipulae_Vg_P5p_wt_mod_2))
        colnames(median_trait_mean_liab_Otipulae_P5p_wt_2) <- c("median")
        posterior.mode_trait_mean_liab_Otipulae_P5p_wt_2 <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P5p_wt_mod_2)),posterior.mode(as.mcmc(trait_mean_liab_Otipulae_WILD_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Otipulae_Vg_P5p_wt_mod_2)))
        colnames(posterior.mode_trait_mean_liab_Otipulae_P5p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Otipulae_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P5p_wt_mod_2)),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_WILD_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_Vg_P5p_wt_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_liab_Otipulae_P5p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Otipulae_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P5p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_WILD_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_Vg_P5p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Otipulae_P5p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Otipulae_P5p_wt_2 <- rbind(effectiveSize(trait_mean_liab_Otipulae_CONTROL_P5p_wt_mod_2),effectiveSize(trait_mean_liab_Otipulae_WILD_P5p_wt_mod),effectiveSize(trait_mean_liab_Otipulae_Vg_P5p_wt_mod_2))
        colnames(effectiveSize_trait_mean_liab_Otipulae_P5p_wt_2) <- c("effectiveSize")
        trait_mean_liab_Otipulae_P5p_wt_2 <- cbind.data.frame(mean_trait_mean_liab_Otipulae_P5p_wt_2,median_trait_mean_liab_Otipulae_P5p_wt_2,posterior.mode_trait_mean_liab_Otipulae_P5p_wt_2,HPDinterval_0.95_trait_mean_liab_Otipulae_P5p_wt_2,HPDinterval_0.83_trait_mean_liab_Otipulae_P5p_wt_2,effectiveSize_trait_mean_liab_Otipulae_P5p_wt_2)
        rownames(trait_mean_liab_Otipulae_P5p_wt_2) <- c("trait_mean_liab_Otipulae_CONTROL_P5p_wt_mod_2","trait_mean_liab_Otipulae_WILD_P5p_wt_mod","trait_mean_liab_Otipulae_Vg_P5p_wt_mod_2")
        trait_mean_liab_Otipulae_P5p_wt_2 <- cbind(Models = rownames(trait_mean_liab_Otipulae_P5p_wt_2),trait_mean_liab_Otipulae_P5p_wt_2)
        rownames(trait_mean_liab_Otipulae_P5p_wt_2) <- NULL
        trait_mean_liab_Otipulae_P5p_wt_2$Pnp <- c("P5.p","P5.p","P5.p")
        trait_mean_liab_Otipulae_P5p_wt_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_liab_Otipulae_P5p_wt_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Otipulae_P5p_wt_2$Scale <- c("liab","liab","liab")
        trait_mean_liab_Otipulae_P5p_wt_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_liab_Otipulae_P5p_wt_2
      }
      
      liab_Otipulae_P5p_wt_2 <- rbind.data.frame(va_liab_Otipulae_P5p_wt_2, h2_liab_Otipulae_P5p_wt_2,Evol_liab_Otipulae_P5p_wt_2,trait_mean_liab_Otipulae_P5p_wt_2)
      liab_Otipulae_P5p_wt_2
    }
    #Summary data scale Otipulae P5p
    {
      #Summary va_data_Otipulae_P5p_wt_2:NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        mean_va_data_Otipulae_P5p_wt_2 <- rbind(mean(va_data_Otipulae_CONTROL_P5p_wt_mod_2/2),mean(va_data_Otipulae_WILD_P5p_wt_mod/2),mean(va_data_Otipulae_Vg_P5p_wt_mod_2/2))
        colnames(mean_va_data_Otipulae_P5p_wt_2) <- c("mean")
        median_va_data_Otipulae_P5p_wt_2 <- rbind(median(va_data_Otipulae_CONTROL_P5p_wt_mod_2/2),median(va_data_Otipulae_WILD_P5p_wt_mod/2),median(va_data_Otipulae_Vg_P5p_wt_mod_2/2))
        colnames(median_va_data_Otipulae_P5p_wt_2) <- c("median")
        posterior.mode_va_data_Otipulae_P5p_wt_2 <- rbind(posterior.mode(as.mcmc(va_data_Otipulae_CONTROL_P5p_wt_mod_2/2)),posterior.mode(as.mcmc(va_data_Otipulae_WILD_P5p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Otipulae_Vg_P5p_wt_mod_2/2)))
        colnames(posterior.mode_va_data_Otipulae_P5p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Otipulae_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(va_data_Otipulae_CONTROL_P5p_wt_mod_2/2)),HPDinterval(as.mcmc(va_data_Otipulae_WILD_P5p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Otipulae_Vg_P5p_wt_mod_2/2)))
        colnames(HPDinterval_0.95_va_data_Otipulae_P5p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Otipulae_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(va_data_Otipulae_CONTROL_P5p_wt_mod_2/2),prob=.83),HPDinterval(as.mcmc(va_data_Otipulae_WILD_P5p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Otipulae_Vg_P5p_wt_mod_2/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Otipulae_P5p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Otipulae_P5p_wt_2 <- rbind(effectiveSize(va_data_Otipulae_CONTROL_P5p_wt_mod_2/2),effectiveSize(va_data_Otipulae_WILD_P5p_wt_mod/2),effectiveSize(va_data_Otipulae_Vg_P5p_wt_mod_2/2))
        colnames(effectiveSize_va_data_Otipulae_P5p_wt_2) <- c("effectiveSize")
        va_data_Otipulae_P5p_wt_2 <- cbind.data.frame(mean_va_data_Otipulae_P5p_wt_2,median_va_data_Otipulae_P5p_wt_2,posterior.mode_va_data_Otipulae_P5p_wt_2,HPDinterval_0.95_va_data_Otipulae_P5p_wt_2,HPDinterval_0.83_va_data_Otipulae_P5p_wt_2,effectiveSize_va_data_Otipulae_P5p_wt_2)
        rownames(va_data_Otipulae_P5p_wt_2) <- c("va_data_Otipulae_CONTROL_P5p_wt_mod_2","va_data_Otipulae_WILD_P5p_wt_mod","va_data_Otipulae_Vg_P5p_wt_mod_2")
        va_data_Otipulae_P5p_wt_2 <- cbind(Models = rownames(va_data_Otipulae_P5p_wt_2),va_data_Otipulae_P5p_wt_2)
        rownames(va_data_Otipulae_P5p_wt_2) <- NULL
        va_data_Otipulae_P5p_wt_2$Pnp <- c("P5.p","P5.p","P5.p")
        va_data_Otipulae_P5p_wt_2$Treatment <- c("Control","WILD","Vg")
        va_data_Otipulae_P5p_wt_2$Measure <- c("Va","Va","Va")
        va_data_Otipulae_P5p_wt_2$Scale <- c("data","data","data")
        va_data_Otipulae_P5p_wt_2$Variance <- c("Vg","Vg","Vg")
        va_data_Otipulae_P5p_wt_2
      }
      
      #Summary h2_data_Otipulae_P5p_wt_2
      {
        mean_h2_data_Otipulae_P5p_wt_2 <- rbind(mean(h2_data_Otipulae_CONTROL_P5p_wt_mod_2),mean(h2_data_Otipulae_WILD_P5p_wt_mod),mean(h2_data_Otipulae_Vg_P5p_wt_mod_2))
        colnames(mean_h2_data_Otipulae_P5p_wt_2) <- c("mean")
        median_h2_data_Otipulae_P5p_wt_2 <- rbind(median(h2_data_Otipulae_CONTROL_P5p_wt_mod_2),median(h2_data_Otipulae_WILD_P5p_wt_mod),median(h2_data_Otipulae_Vg_P5p_wt_mod_2))
        colnames(median_h2_data_Otipulae_P5p_wt_2) <- c("median")
        posterior.mode_h2_data_Otipulae_P5p_wt_2 <- rbind(posterior.mode(as.mcmc(h2_data_Otipulae_CONTROL_P5p_wt_mod_2)),posterior.mode(as.mcmc(h2_data_Otipulae_WILD_P5p_wt_mod)),posterior.mode(as.mcmc(h2_data_Otipulae_Vg_P5p_wt_mod_2)))
        colnames(posterior.mode_h2_data_Otipulae_P5p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Otipulae_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(h2_data_Otipulae_CONTROL_P5p_wt_mod_2)),HPDinterval(as.mcmc(h2_data_Otipulae_WILD_P5p_wt_mod)),HPDinterval(as.mcmc(h2_data_Otipulae_Vg_P5p_wt_mod_2)))
        colnames(HPDinterval_0.95_h2_data_Otipulae_P5p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Otipulae_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(h2_data_Otipulae_CONTROL_P5p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(h2_data_Otipulae_WILD_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Otipulae_Vg_P5p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Otipulae_P5p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Otipulae_P5p_wt_2 <- rbind(effectiveSize(h2_data_Otipulae_CONTROL_P5p_wt_mod_2),effectiveSize(h2_data_Otipulae_WILD_P5p_wt_mod),effectiveSize(h2_data_Otipulae_Vg_P5p_wt_mod_2))
        colnames(effectiveSize_h2_data_Otipulae_P5p_wt_2) <- c("effectiveSize")
        h2_data_Otipulae_P5p_wt_2 <- cbind.data.frame(mean_h2_data_Otipulae_P5p_wt_2,median_h2_data_Otipulae_P5p_wt_2,posterior.mode_h2_data_Otipulae_P5p_wt_2,HPDinterval_0.95_h2_data_Otipulae_P5p_wt_2,HPDinterval_0.83_h2_data_Otipulae_P5p_wt_2,effectiveSize_h2_data_Otipulae_P5p_wt_2)
        rownames(h2_data_Otipulae_P5p_wt_2) <- c("h2_data_Otipulae_CONTROL_P5p_wt_mod_2","h2_data_Otipulae_WILD_P5p_wt_mod","h2_data_Otipulae_Vg_P5p_wt_mod_2")
        h2_data_Otipulae_P5p_wt_2 <- cbind(Models = rownames(h2_data_Otipulae_P5p_wt_2),h2_data_Otipulae_P5p_wt_2)
        rownames(h2_data_Otipulae_P5p_wt_2) <- NULL
        h2_data_Otipulae_P5p_wt_2$Pnp <- c("P5.p","P5.p","P5.p")
        h2_data_Otipulae_P5p_wt_2$Treatment <- c("Control","WILD","Vg")
        h2_data_Otipulae_P5p_wt_2$Measure <- c("H2","H2","H2")
        h2_data_Otipulae_P5p_wt_2$Scale <- c("data","data","data")
        h2_data_Otipulae_P5p_wt_2$Variance <- c("Vg","Vg","Vg")
        h2_data_Otipulae_P5p_wt_2
      }
      
      #Summary Evol_data_Otipulae_P5p_wt_2
      {
        mean_Evol_data_Otipulae_P5p_wt_2 <- rbind(mean(Evol_data_Otipulae_CONTROL_P5p_wt_mod_2),mean(Evol_data_Otipulae_WILD_P5p_wt_mod),mean(Evol_data_Otipulae_Vg_P5p_wt_mod_2))
        colnames(mean_Evol_data_Otipulae_P5p_wt_2) <- c("mean")
        median_Evol_data_Otipulae_P5p_wt_2 <- rbind(median(Evol_data_Otipulae_CONTROL_P5p_wt_mod_2),median(Evol_data_Otipulae_WILD_P5p_wt_mod),median(Evol_data_Otipulae_Vg_P5p_wt_mod_2))
        colnames(median_Evol_data_Otipulae_P5p_wt_2) <- c("median")
        posterior.mode_Evol_data_Otipulae_P5p_wt_2 <- rbind(posterior.mode(as.mcmc(Evol_data_Otipulae_CONTROL_P5p_wt_mod_2)),posterior.mode(as.mcmc(Evol_data_Otipulae_WILD_P5p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Otipulae_Vg_P5p_wt_mod_2)))
        colnames(posterior.mode_Evol_data_Otipulae_P5p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Otipulae_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Otipulae_CONTROL_P5p_wt_mod_2)),HPDinterval(as.mcmc(Evol_data_Otipulae_WILD_P5p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Otipulae_Vg_P5p_wt_mod_2)))
        colnames(HPDinterval_0.95_Evol_data_Otipulae_P5p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Otipulae_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Otipulae_CONTROL_P5p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(Evol_data_Otipulae_WILD_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Otipulae_Vg_P5p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Otipulae_P5p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Otipulae_P5p_wt_2 <- rbind(effectiveSize(Evol_data_Otipulae_CONTROL_P5p_wt_mod_2),effectiveSize(Evol_data_Otipulae_WILD_P5p_wt_mod),effectiveSize(Evol_data_Otipulae_Vg_P5p_wt_mod_2))
        colnames(effectiveSize_Evol_data_Otipulae_P5p_wt_2) <- c("effectiveSize")
        Evol_data_Otipulae_P5p_wt_2 <- cbind.data.frame(mean_Evol_data_Otipulae_P5p_wt_2,median_Evol_data_Otipulae_P5p_wt_2,posterior.mode_Evol_data_Otipulae_P5p_wt_2,HPDinterval_0.95_Evol_data_Otipulae_P5p_wt_2,HPDinterval_0.83_Evol_data_Otipulae_P5p_wt_2,effectiveSize_Evol_data_Otipulae_P5p_wt_2)
        rownames(Evol_data_Otipulae_P5p_wt_2) <- c("Evol_data_Otipulae_CONTROL_P5p_wt_mod_2","Evol_data_Otipulae_WILD_P5p_wt_mod","Evol_data_Otipulae_Vg_P5p_wt_mod_2")
        Evol_data_Otipulae_P5p_wt_2 <- cbind(Models = rownames(Evol_data_Otipulae_P5p_wt_2),Evol_data_Otipulae_P5p_wt_2)
        rownames(Evol_data_Otipulae_P5p_wt_2) <- NULL
        Evol_data_Otipulae_P5p_wt_2$Pnp <- c("P5.p","P5.p","P5.p")
        Evol_data_Otipulae_P5p_wt_2$Treatment <- c("Control","WILD","Vg")
        Evol_data_Otipulae_P5p_wt_2$Measure <- c("Evol","Evol","Evol")
        Evol_data_Otipulae_P5p_wt_2$Scale <- c("data","data","data")
        Evol_data_Otipulae_P5p_wt_2$Variance <- c("Vg","Vg","Vg")
        Evol_data_Otipulae_P5p_wt_2
      }
      
      #Summary trait_mean_data_Otipulae_P5p_wt_2
      {
        mean_trait_mean_data_Otipulae_P5p_wt_2 <- rbind(mean(trait_mean_data_Otipulae_CONTROL_P5p_wt_mod_2),mean(trait_mean_data_Otipulae_WILD_P5p_wt_mod),mean(trait_mean_data_Otipulae_Vg_P5p_wt_mod_2))
        colnames(mean_trait_mean_data_Otipulae_P5p_wt_2) <- c("mean")
        median_trait_mean_data_Otipulae_P5p_wt_2 <- rbind(median(trait_mean_data_Otipulae_CONTROL_P5p_wt_mod_2),median(trait_mean_data_Otipulae_WILD_P5p_wt_mod),median(trait_mean_data_Otipulae_Vg_P5p_wt_mod_2))
        colnames(median_trait_mean_data_Otipulae_P5p_wt_2) <- c("median")
        posterior.mode_trait_mean_data_Otipulae_P5p_wt_2 <- rbind(posterior.mode(as.mcmc(trait_mean_data_Otipulae_CONTROL_P5p_wt_mod_2)),posterior.mode(as.mcmc(trait_mean_data_Otipulae_WILD_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Otipulae_Vg_P5p_wt_mod_2)))
        colnames(posterior.mode_trait_mean_data_Otipulae_P5p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Otipulae_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Otipulae_CONTROL_P5p_wt_mod_2)),HPDinterval(as.mcmc(trait_mean_data_Otipulae_WILD_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Otipulae_Vg_P5p_wt_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_data_Otipulae_P5p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Otipulae_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Otipulae_CONTROL_P5p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Otipulae_WILD_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Otipulae_Vg_P5p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Otipulae_P5p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Otipulae_P5p_wt_2 <- rbind(effectiveSize(trait_mean_data_Otipulae_CONTROL_P5p_wt_mod_2),effectiveSize(trait_mean_data_Otipulae_WILD_P5p_wt_mod),effectiveSize(trait_mean_data_Otipulae_Vg_P5p_wt_mod_2))
        colnames(effectiveSize_trait_mean_data_Otipulae_P5p_wt_2) <- c("effectiveSize")
        trait_mean_data_Otipulae_P5p_wt_2 <- cbind.data.frame(mean_trait_mean_data_Otipulae_P5p_wt_2,median_trait_mean_data_Otipulae_P5p_wt_2,posterior.mode_trait_mean_data_Otipulae_P5p_wt_2,HPDinterval_0.95_trait_mean_data_Otipulae_P5p_wt_2,HPDinterval_0.83_trait_mean_data_Otipulae_P5p_wt_2,effectiveSize_trait_mean_data_Otipulae_P5p_wt_2)
        rownames(trait_mean_data_Otipulae_P5p_wt_2) <- c("trait_mean_data_Otipulae_CONTROL_P5p_wt_mod_2","trait_mean_data_Otipulae_WILD_P5p_wt_mod","trait_mean_data_Otipulae_Vg_P5p_wt_mod_2")
        trait_mean_data_Otipulae_P5p_wt_2 <- cbind(Models = rownames(trait_mean_data_Otipulae_P5p_wt_2),trait_mean_data_Otipulae_P5p_wt_2)
        rownames(trait_mean_data_Otipulae_P5p_wt_2) <- NULL
        trait_mean_data_Otipulae_P5p_wt_2$Pnp <- c("P5.p","P5.p","P5.p")
        trait_mean_data_Otipulae_P5p_wt_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_data_Otipulae_P5p_wt_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Otipulae_P5p_wt_2$Scale <- c("data","data","data")
        trait_mean_data_Otipulae_P5p_wt_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_data_Otipulae_P5p_wt_2
      }
      
      data_Otipulae_P5p_wt_2 <- rbind.data.frame(va_data_Otipulae_P5p_wt_2, h2_data_Otipulae_P5p_wt_2,Evol_data_Otipulae_P5p_wt_2,trait_mean_data_Otipulae_P5p_wt_2)
      data_Otipulae_P5p_wt_2
      
    }
    Vg_Otipulae_P5p_wt_2 <- rbind.data.frame(liab_Otipulae_P5p_wt_2, data_Otipulae_P5p_wt_2)
    Vg_Otipulae_P5p_wt_2$Pnp_fate <- rep("wt", 24)
    Vg_Otipulae_P5p_wt_2
    #remove Otipulae P5p_wt_2 models
    {
      remove(Otipulae_CONTROL_P5p_wt_mod_2)
      remove(Otipulae_WILD_P5p_wt_mod)
      remove(Otipulae_Vg_P5p_wt_mod_2)
    }
  }
  
  ##Summary Otipulae P6p----
  {
    #Summary liability scale Otipulae P6p
    {
      #Summary va_liab_Otipulae_P6p_wt_2: NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        
        mean_va_liab_Otipulae_P6p_wt_2 <- rbind(mean(va_liab_Otipulae_CONTROL_P6p_wt_mod_2/2),mean(va_liab_Otipulae_WILD_P6p_wt_mod/2),mean(va_liab_Otipulae_Vg_P6p_wt_mod_2/2))
        colnames(mean_va_liab_Otipulae_P6p_wt_2) <- c("mean")
        median_va_liab_Otipulae_P6p_wt_2 <- rbind(median(va_liab_Otipulae_CONTROL_P6p_wt_mod_2/2),median(va_liab_Otipulae_WILD_P6p_wt_mod/2),median(va_liab_Otipulae_Vg_P6p_wt_mod_2/2))
        colnames(median_va_liab_Otipulae_P6p_wt_2) <- c("median")
        posterior.mode_va_liab_Otipulae_P6p_wt_2 <- rbind(posterior.mode(va_liab_Otipulae_CONTROL_P6p_wt_mod_2/2),posterior.mode(va_liab_Otipulae_WILD_P6p_wt_mod/2),posterior.mode(va_liab_Otipulae_Vg_P6p_wt_mod_2/2))
        colnames(posterior.mode_va_liab_Otipulae_P6p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Otipulae_P6p_wt_2 <- rbind(HPDinterval(va_liab_Otipulae_CONTROL_P6p_wt_mod_2/2),HPDinterval(va_liab_Otipulae_WILD_P6p_wt_mod/2),HPDinterval(va_liab_Otipulae_Vg_P6p_wt_mod_2/2))
        colnames(HPDinterval_0.95_va_liab_Otipulae_P6p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Otipulae_P6p_wt_2 <- rbind(HPDinterval(va_liab_Otipulae_CONTROL_P6p_wt_mod_2/2,prob=.83),HPDinterval(va_liab_Otipulae_WILD_P6p_wt_mod/2,prob=.83),HPDinterval(va_liab_Otipulae_Vg_P6p_wt_mod_2/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Otipulae_P6p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Otipulae_P6p_wt_2 <- rbind(effectiveSize(va_liab_Otipulae_CONTROL_P6p_wt_mod_2/2),effectiveSize(va_liab_Otipulae_WILD_P6p_wt_mod/2),effectiveSize(va_liab_Otipulae_Vg_P6p_wt_mod_2/2))
        colnames(effectiveSize_va_liab_Otipulae_P6p_wt_2) <- c("effectiveSize")
        va_liab_Otipulae_P6p_wt_2 <- cbind.data.frame(mean_va_liab_Otipulae_P6p_wt_2,median_va_liab_Otipulae_P6p_wt_2,posterior.mode_va_liab_Otipulae_P6p_wt_2,HPDinterval_0.95_va_liab_Otipulae_P6p_wt_2,HPDinterval_0.83_va_liab_Otipulae_P6p_wt_2,effectiveSize_va_liab_Otipulae_P6p_wt_2)
        rownames(va_liab_Otipulae_P6p_wt_2) <- c("va_liab_Otipulae_CONTROL_P6p_wt_mod_2","va_liab_Otipulae_WILD_P6p_wt_mod","va_liab_Otipulae_Vg_P6p_wt_mod_2")
        va_liab_Otipulae_P6p_wt_2 <- cbind(Models = rownames(va_liab_Otipulae_P6p_wt_2),va_liab_Otipulae_P6p_wt_2)
        rownames(va_liab_Otipulae_P6p_wt_2) <- NULL
        va_liab_Otipulae_P6p_wt_2$Pnp <- c("P6.p","P6.p","P6.p")
        va_liab_Otipulae_P6p_wt_2$Treatment <- c("Control","WILD","Vg")
        va_liab_Otipulae_P6p_wt_2$Measure <- c("Va","Va","Va")
        va_liab_Otipulae_P6p_wt_2$Scale <- c("liab","liab","liab")
        va_liab_Otipulae_P6p_wt_2$Variance <- c("Vg","Vg","Vg")
        va_liab_Otipulae_P6p_wt_2
      }
      
      #Summary h2_liab_Otipulae_P6p_wt_2
      {
        mean_h2_liab_Otipulae_P6p_wt_2 <- rbind(mean(h2_liab_Otipulae_CONTROL_P6p_wt_mod_2),mean(h2_liab_Otipulae_WILD_P6p_wt_mod),mean(h2_liab_Otipulae_Vg_P6p_wt_mod_2))
        colnames(mean_h2_liab_Otipulae_P6p_wt_2) <- c("mean")
        median_h2_liab_Otipulae_P6p_wt_2 <- rbind(median(h2_liab_Otipulae_CONTROL_P6p_wt_mod_2),median(h2_liab_Otipulae_WILD_P6p_wt_mod),median(h2_liab_Otipulae_Vg_P6p_wt_mod_2))
        colnames(median_h2_liab_Otipulae_P6p_wt_2) <- c("median")
        posterior.mode_h2_liab_Otipulae_P6p_wt_2 <- rbind(posterior.mode(h2_liab_Otipulae_CONTROL_P6p_wt_mod_2),posterior.mode(h2_liab_Otipulae_WILD_P6p_wt_mod),posterior.mode(h2_liab_Otipulae_Vg_P6p_wt_mod_2))
        colnames(posterior.mode_h2_liab_Otipulae_P6p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Otipulae_P6p_wt_2 <- rbind(HPDinterval(h2_liab_Otipulae_CONTROL_P6p_wt_mod_2),HPDinterval(h2_liab_Otipulae_WILD_P6p_wt_mod),HPDinterval(h2_liab_Otipulae_Vg_P6p_wt_mod_2))
        colnames(HPDinterval_0.95_h2_liab_Otipulae_P6p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Otipulae_P6p_wt_2 <- rbind(HPDinterval(h2_liab_Otipulae_CONTROL_P6p_wt_mod_2,prob=.83),HPDinterval(h2_liab_Otipulae_WILD_P6p_wt_mod,prob=.83),HPDinterval(h2_liab_Otipulae_Vg_P6p_wt_mod_2,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Otipulae_P6p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Otipulae_P6p_wt_2 <- rbind(effectiveSize(h2_liab_Otipulae_CONTROL_P6p_wt_mod_2),effectiveSize(h2_liab_Otipulae_WILD_P6p_wt_mod),effectiveSize(h2_liab_Otipulae_Vg_P6p_wt_mod_2))
        colnames(effectiveSize_h2_liab_Otipulae_P6p_wt_2) <- c("effectiveSize")
        h2_liab_Otipulae_P6p_wt_2 <- cbind.data.frame(mean_h2_liab_Otipulae_P6p_wt_2,median_h2_liab_Otipulae_P6p_wt_2,posterior.mode_h2_liab_Otipulae_P6p_wt_2,HPDinterval_0.95_h2_liab_Otipulae_P6p_wt_2,HPDinterval_0.83_h2_liab_Otipulae_P6p_wt_2,effectiveSize_h2_liab_Otipulae_P6p_wt_2)
        rownames(h2_liab_Otipulae_P6p_wt_2) <- c("h2_liab_Otipulae_CONTROL_P6p_wt_mod_2","h2_liab_Otipulae_WILD_P6p_wt_mod","h2_liab_Otipulae_Vg_P6p_wt_mod_2")
        h2_liab_Otipulae_P6p_wt_2 <- cbind(Models = rownames(h2_liab_Otipulae_P6p_wt_2),h2_liab_Otipulae_P6p_wt_2)
        rownames(h2_liab_Otipulae_P6p_wt_2) <- NULL
        h2_liab_Otipulae_P6p_wt_2$Pnp <- c("P6.p","P6.p","P6.p")
        h2_liab_Otipulae_P6p_wt_2$Treatment <- c("Control","WILD","Vg")
        h2_liab_Otipulae_P6p_wt_2$Measure <- c("H2","H2","H2")
        h2_liab_Otipulae_P6p_wt_2$Scale <- c("liab","liab","liab")
        h2_liab_Otipulae_P6p_wt_2$Variance <- c("Vg","Vg","Vg")
        h2_liab_Otipulae_P6p_wt_2
      }
      
      #Summary Evol_liab_Otipulae_P6p_wt_2
      {
        mean_Evol_liab_Otipulae_P6p_wt_2 <- rbind(mean(Evol_liab_Otipulae_CONTROL_P6p_wt_mod_2),mean(Evol_liab_Otipulae_WILD_P6p_wt_mod),mean(Evol_liab_Otipulae_Vg_P6p_wt_mod_2))
        colnames(mean_Evol_liab_Otipulae_P6p_wt_2) <- c("mean")
        median_Evol_liab_Otipulae_P6p_wt_2 <- rbind(median(Evol_liab_Otipulae_CONTROL_P6p_wt_mod_2),median(Evol_liab_Otipulae_WILD_P6p_wt_mod),median(Evol_liab_Otipulae_Vg_P6p_wt_mod_2))
        colnames(median_Evol_liab_Otipulae_P6p_wt_2) <- c("median")
        posterior.mode_Evol_liab_Otipulae_P6p_wt_2 <- rbind(posterior.mode(Evol_liab_Otipulae_CONTROL_P6p_wt_mod_2),posterior.mode(Evol_liab_Otipulae_WILD_P6p_wt_mod),posterior.mode(Evol_liab_Otipulae_Vg_P6p_wt_mod_2))
        colnames(posterior.mode_Evol_liab_Otipulae_P6p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Otipulae_P6p_wt_2 <- rbind(HPDinterval(Evol_liab_Otipulae_CONTROL_P6p_wt_mod_2),HPDinterval(Evol_liab_Otipulae_WILD_P6p_wt_mod),HPDinterval(Evol_liab_Otipulae_Vg_P6p_wt_mod_2))
        colnames(HPDinterval_0.95_Evol_liab_Otipulae_P6p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Otipulae_P6p_wt_2 <- rbind(HPDinterval(Evol_liab_Otipulae_CONTROL_P6p_wt_mod_2,prob=.83),HPDinterval(Evol_liab_Otipulae_WILD_P6p_wt_mod,prob=.83),HPDinterval(Evol_liab_Otipulae_Vg_P6p_wt_mod_2,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Otipulae_P6p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Otipulae_P6p_wt_2 <- rbind(effectiveSize(Evol_liab_Otipulae_CONTROL_P6p_wt_mod_2),effectiveSize(Evol_liab_Otipulae_WILD_P6p_wt_mod),effectiveSize(Evol_liab_Otipulae_Vg_P6p_wt_mod_2))
        colnames(effectiveSize_Evol_liab_Otipulae_P6p_wt_2) <- c("effectiveSize")
        Evol_liab_Otipulae_P6p_wt_2 <- cbind.data.frame(mean_Evol_liab_Otipulae_P6p_wt_2,median_Evol_liab_Otipulae_P6p_wt_2,posterior.mode_Evol_liab_Otipulae_P6p_wt_2,HPDinterval_0.95_Evol_liab_Otipulae_P6p_wt_2,HPDinterval_0.83_Evol_liab_Otipulae_P6p_wt_2,effectiveSize_Evol_liab_Otipulae_P6p_wt_2)
        rownames(Evol_liab_Otipulae_P6p_wt_2) <- c("Evol_liab_Otipulae_CONTROL_P6p_wt_mod_2","Evol_liab_Otipulae_WILD_P6p_wt_mod","Evol_liab_Otipulae_Vg_P6p_wt_mod_2")
        Evol_liab_Otipulae_P6p_wt_2 <- cbind(Models = rownames(Evol_liab_Otipulae_P6p_wt_2),Evol_liab_Otipulae_P6p_wt_2)
        rownames(Evol_liab_Otipulae_P6p_wt_2) <- NULL
        Evol_liab_Otipulae_P6p_wt_2$Pnp <- c("P6.p","P6.p","P6.p")
        Evol_liab_Otipulae_P6p_wt_2$Treatment <- c("Control","WILD","Vg")
        Evol_liab_Otipulae_P6p_wt_2$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Otipulae_P6p_wt_2$Scale <- c("liab","liab","liab")
        Evol_liab_Otipulae_P6p_wt_2$Variance <- c("Vg","Vg","Vg")
        Evol_liab_Otipulae_P6p_wt_2
      }
      
      #Summary trait_mean_liab_Otipulae_P6p_wt_2
      {
        mean_trait_mean_liab_Otipulae_P6p_wt_2 <- rbind(mean(trait_mean_liab_Otipulae_CONTROL_P6p_wt_mod_2),mean(trait_mean_liab_Otipulae_WILD_P6p_wt_mod),mean(trait_mean_liab_Otipulae_Vg_P6p_wt_mod_2))
        colnames(mean_trait_mean_liab_Otipulae_P6p_wt_2) <- c("mean")
        median_trait_mean_liab_Otipulae_P6p_wt_2 <- rbind(median(trait_mean_liab_Otipulae_CONTROL_P6p_wt_mod_2),median(trait_mean_liab_Otipulae_WILD_P6p_wt_mod),median(trait_mean_liab_Otipulae_Vg_P6p_wt_mod_2))
        colnames(median_trait_mean_liab_Otipulae_P6p_wt_2) <- c("median")
        posterior.mode_trait_mean_liab_Otipulae_P6p_wt_2 <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P6p_wt_mod_2)),posterior.mode(as.mcmc(trait_mean_liab_Otipulae_WILD_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Otipulae_Vg_P6p_wt_mod_2)))
        colnames(posterior.mode_trait_mean_liab_Otipulae_P6p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Otipulae_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P6p_wt_mod_2)),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_WILD_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_Vg_P6p_wt_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_liab_Otipulae_P6p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Otipulae_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P6p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_WILD_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_Vg_P6p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Otipulae_P6p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Otipulae_P6p_wt_2 <- rbind(effectiveSize(trait_mean_liab_Otipulae_CONTROL_P6p_wt_mod_2),effectiveSize(trait_mean_liab_Otipulae_WILD_P6p_wt_mod),effectiveSize(trait_mean_liab_Otipulae_Vg_P6p_wt_mod_2))
        colnames(effectiveSize_trait_mean_liab_Otipulae_P6p_wt_2) <- c("effectiveSize")
        trait_mean_liab_Otipulae_P6p_wt_2 <- cbind.data.frame(mean_trait_mean_liab_Otipulae_P6p_wt_2,median_trait_mean_liab_Otipulae_P6p_wt_2,posterior.mode_trait_mean_liab_Otipulae_P6p_wt_2,HPDinterval_0.95_trait_mean_liab_Otipulae_P6p_wt_2,HPDinterval_0.83_trait_mean_liab_Otipulae_P6p_wt_2,effectiveSize_trait_mean_liab_Otipulae_P6p_wt_2)
        rownames(trait_mean_liab_Otipulae_P6p_wt_2) <- c("trait_mean_liab_Otipulae_CONTROL_P6p_wt_mod_2","trait_mean_liab_Otipulae_WILD_P6p_wt_mod","trait_mean_liab_Otipulae_Vg_P6p_wt_mod_2")
        trait_mean_liab_Otipulae_P6p_wt_2 <- cbind(Models = rownames(trait_mean_liab_Otipulae_P6p_wt_2),trait_mean_liab_Otipulae_P6p_wt_2)
        rownames(trait_mean_liab_Otipulae_P6p_wt_2) <- NULL
        trait_mean_liab_Otipulae_P6p_wt_2$Pnp <- c("P6.p","P6.p","P6.p")
        trait_mean_liab_Otipulae_P6p_wt_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_liab_Otipulae_P6p_wt_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Otipulae_P6p_wt_2$Scale <- c("liab","liab","liab")
        trait_mean_liab_Otipulae_P6p_wt_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_liab_Otipulae_P6p_wt_2
      }
      
      liab_Otipulae_P6p_wt_2 <- rbind.data.frame(va_liab_Otipulae_P6p_wt_2, h2_liab_Otipulae_P6p_wt_2,Evol_liab_Otipulae_P6p_wt_2,trait_mean_liab_Otipulae_P6p_wt_2)
      liab_Otipulae_P6p_wt_2
    }
    #Summary data scale Otipulae P6p
    {
      #Summary va_data_Otipulae_P6p_wt_2:NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        mean_va_data_Otipulae_P6p_wt_2 <- rbind(mean(va_data_Otipulae_CONTROL_P6p_wt_mod_2/2),mean(va_data_Otipulae_WILD_P6p_wt_mod/2),mean(va_data_Otipulae_Vg_P6p_wt_mod_2/2))
        colnames(mean_va_data_Otipulae_P6p_wt_2) <- c("mean")
        median_va_data_Otipulae_P6p_wt_2 <- rbind(median(va_data_Otipulae_CONTROL_P6p_wt_mod_2/2),median(va_data_Otipulae_WILD_P6p_wt_mod/2),median(va_data_Otipulae_Vg_P6p_wt_mod_2/2))
        colnames(median_va_data_Otipulae_P6p_wt_2) <- c("median")
        posterior.mode_va_data_Otipulae_P6p_wt_2 <- rbind(posterior.mode(as.mcmc(va_data_Otipulae_CONTROL_P6p_wt_mod_2/2)),posterior.mode(as.mcmc(va_data_Otipulae_WILD_P6p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Otipulae_Vg_P6p_wt_mod_2/2)))
        colnames(posterior.mode_va_data_Otipulae_P6p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Otipulae_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(va_data_Otipulae_CONTROL_P6p_wt_mod_2/2)),HPDinterval(as.mcmc(va_data_Otipulae_WILD_P6p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Otipulae_Vg_P6p_wt_mod_2/2)))
        colnames(HPDinterval_0.95_va_data_Otipulae_P6p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Otipulae_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(va_data_Otipulae_CONTROL_P6p_wt_mod_2/2),prob=.83),HPDinterval(as.mcmc(va_data_Otipulae_WILD_P6p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Otipulae_Vg_P6p_wt_mod_2/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Otipulae_P6p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Otipulae_P6p_wt_2 <- rbind(effectiveSize(va_data_Otipulae_CONTROL_P6p_wt_mod_2/2),effectiveSize(va_data_Otipulae_WILD_P6p_wt_mod/2),effectiveSize(va_data_Otipulae_Vg_P6p_wt_mod_2/2))
        colnames(effectiveSize_va_data_Otipulae_P6p_wt_2) <- c("effectiveSize")
        va_data_Otipulae_P6p_wt_2 <- cbind.data.frame(mean_va_data_Otipulae_P6p_wt_2,median_va_data_Otipulae_P6p_wt_2,posterior.mode_va_data_Otipulae_P6p_wt_2,HPDinterval_0.95_va_data_Otipulae_P6p_wt_2,HPDinterval_0.83_va_data_Otipulae_P6p_wt_2,effectiveSize_va_data_Otipulae_P6p_wt_2)
        rownames(va_data_Otipulae_P6p_wt_2) <- c("va_data_Otipulae_CONTROL_P6p_wt_mod_2","va_data_Otipulae_WILD_P6p_wt_mod","va_data_Otipulae_Vg_P6p_wt_mod_2")
        va_data_Otipulae_P6p_wt_2 <- cbind(Models = rownames(va_data_Otipulae_P6p_wt_2),va_data_Otipulae_P6p_wt_2)
        rownames(va_data_Otipulae_P6p_wt_2) <- NULL
        va_data_Otipulae_P6p_wt_2$Pnp <- c("P6.p","P6.p","P6.p")
        va_data_Otipulae_P6p_wt_2$Treatment <- c("Control","WILD","Vg")
        va_data_Otipulae_P6p_wt_2$Measure <- c("Va","Va","Va")
        va_data_Otipulae_P6p_wt_2$Scale <- c("data","data","data")
        va_data_Otipulae_P6p_wt_2$Variance <- c("Vg","Vg","Vg")
        va_data_Otipulae_P6p_wt_2
      }
      
      #Summary h2_data_Otipulae_P6p_wt_2
      {
        mean_h2_data_Otipulae_P6p_wt_2 <- rbind(mean(h2_data_Otipulae_CONTROL_P6p_wt_mod_2),mean(h2_data_Otipulae_WILD_P6p_wt_mod),mean(h2_data_Otipulae_Vg_P6p_wt_mod_2))
        colnames(mean_h2_data_Otipulae_P6p_wt_2) <- c("mean")
        median_h2_data_Otipulae_P6p_wt_2 <- rbind(median(h2_data_Otipulae_CONTROL_P6p_wt_mod_2),median(h2_data_Otipulae_WILD_P6p_wt_mod),median(h2_data_Otipulae_Vg_P6p_wt_mod_2))
        colnames(median_h2_data_Otipulae_P6p_wt_2) <- c("median")
        posterior.mode_h2_data_Otipulae_P6p_wt_2 <- rbind(posterior.mode(as.mcmc(h2_data_Otipulae_CONTROL_P6p_wt_mod_2)),posterior.mode(as.mcmc(h2_data_Otipulae_WILD_P6p_wt_mod)),posterior.mode(as.mcmc(h2_data_Otipulae_Vg_P6p_wt_mod_2)))
        colnames(posterior.mode_h2_data_Otipulae_P6p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Otipulae_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(h2_data_Otipulae_CONTROL_P6p_wt_mod_2)),HPDinterval(as.mcmc(h2_data_Otipulae_WILD_P6p_wt_mod)),HPDinterval(as.mcmc(h2_data_Otipulae_Vg_P6p_wt_mod_2)))
        colnames(HPDinterval_0.95_h2_data_Otipulae_P6p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Otipulae_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(h2_data_Otipulae_CONTROL_P6p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(h2_data_Otipulae_WILD_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Otipulae_Vg_P6p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Otipulae_P6p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Otipulae_P6p_wt_2 <- rbind(effectiveSize(h2_data_Otipulae_CONTROL_P6p_wt_mod_2),effectiveSize(h2_data_Otipulae_WILD_P6p_wt_mod),effectiveSize(h2_data_Otipulae_Vg_P6p_wt_mod_2))
        colnames(effectiveSize_h2_data_Otipulae_P6p_wt_2) <- c("effectiveSize")
        h2_data_Otipulae_P6p_wt_2 <- cbind.data.frame(mean_h2_data_Otipulae_P6p_wt_2,median_h2_data_Otipulae_P6p_wt_2,posterior.mode_h2_data_Otipulae_P6p_wt_2,HPDinterval_0.95_h2_data_Otipulae_P6p_wt_2,HPDinterval_0.83_h2_data_Otipulae_P6p_wt_2,effectiveSize_h2_data_Otipulae_P6p_wt_2)
        rownames(h2_data_Otipulae_P6p_wt_2) <- c("h2_data_Otipulae_CONTROL_P6p_wt_mod_2","h2_data_Otipulae_WILD_P6p_wt_mod","h2_data_Otipulae_Vg_P6p_wt_mod_2")
        h2_data_Otipulae_P6p_wt_2 <- cbind(Models = rownames(h2_data_Otipulae_P6p_wt_2),h2_data_Otipulae_P6p_wt_2)
        rownames(h2_data_Otipulae_P6p_wt_2) <- NULL
        h2_data_Otipulae_P6p_wt_2$Pnp <- c("P6.p","P6.p","P6.p")
        h2_data_Otipulae_P6p_wt_2$Treatment <- c("Control","WILD","Vg")
        h2_data_Otipulae_P6p_wt_2$Measure <- c("H2","H2","H2")
        h2_data_Otipulae_P6p_wt_2$Scale <- c("data","data","data")
        h2_data_Otipulae_P6p_wt_2$Variance <- c("Vg","Vg","Vg")
        h2_data_Otipulae_P6p_wt_2
      }
      
      #Summary Evol_data_Otipulae_P6p_wt_2
      {
        mean_Evol_data_Otipulae_P6p_wt_2 <- rbind(mean(Evol_data_Otipulae_CONTROL_P6p_wt_mod_2),mean(Evol_data_Otipulae_WILD_P6p_wt_mod),mean(Evol_data_Otipulae_Vg_P6p_wt_mod_2))
        colnames(mean_Evol_data_Otipulae_P6p_wt_2) <- c("mean")
        median_Evol_data_Otipulae_P6p_wt_2 <- rbind(median(Evol_data_Otipulae_CONTROL_P6p_wt_mod_2),median(Evol_data_Otipulae_WILD_P6p_wt_mod),median(Evol_data_Otipulae_Vg_P6p_wt_mod_2))
        colnames(median_Evol_data_Otipulae_P6p_wt_2) <- c("median")
        posterior.mode_Evol_data_Otipulae_P6p_wt_2 <- rbind(posterior.mode(as.mcmc(Evol_data_Otipulae_CONTROL_P6p_wt_mod_2)),posterior.mode(as.mcmc(Evol_data_Otipulae_WILD_P6p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Otipulae_Vg_P6p_wt_mod_2)))
        colnames(posterior.mode_Evol_data_Otipulae_P6p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Otipulae_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Otipulae_CONTROL_P6p_wt_mod_2)),HPDinterval(as.mcmc(Evol_data_Otipulae_WILD_P6p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Otipulae_Vg_P6p_wt_mod_2)))
        colnames(HPDinterval_0.95_Evol_data_Otipulae_P6p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Otipulae_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Otipulae_CONTROL_P6p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(Evol_data_Otipulae_WILD_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Otipulae_Vg_P6p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Otipulae_P6p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Otipulae_P6p_wt_2 <- rbind(effectiveSize(Evol_data_Otipulae_CONTROL_P6p_wt_mod_2),effectiveSize(Evol_data_Otipulae_WILD_P6p_wt_mod),effectiveSize(Evol_data_Otipulae_Vg_P6p_wt_mod_2))
        colnames(effectiveSize_Evol_data_Otipulae_P6p_wt_2) <- c("effectiveSize")
        Evol_data_Otipulae_P6p_wt_2 <- cbind.data.frame(mean_Evol_data_Otipulae_P6p_wt_2,median_Evol_data_Otipulae_P6p_wt_2,posterior.mode_Evol_data_Otipulae_P6p_wt_2,HPDinterval_0.95_Evol_data_Otipulae_P6p_wt_2,HPDinterval_0.83_Evol_data_Otipulae_P6p_wt_2,effectiveSize_Evol_data_Otipulae_P6p_wt_2)
        rownames(Evol_data_Otipulae_P6p_wt_2) <- c("Evol_data_Otipulae_CONTROL_P6p_wt_mod_2","Evol_data_Otipulae_WILD_P6p_wt_mod","Evol_data_Otipulae_Vg_P6p_wt_mod_2")
        Evol_data_Otipulae_P6p_wt_2 <- cbind(Models = rownames(Evol_data_Otipulae_P6p_wt_2),Evol_data_Otipulae_P6p_wt_2)
        rownames(Evol_data_Otipulae_P6p_wt_2) <- NULL
        Evol_data_Otipulae_P6p_wt_2$Pnp <- c("P6.p","P6.p","P6.p")
        Evol_data_Otipulae_P6p_wt_2$Treatment <- c("Control","WILD","Vg")
        Evol_data_Otipulae_P6p_wt_2$Measure <- c("Evol","Evol","Evol")
        Evol_data_Otipulae_P6p_wt_2$Scale <- c("data","data","data")
        Evol_data_Otipulae_P6p_wt_2$Variance <- c("Vg","Vg","Vg")
        Evol_data_Otipulae_P6p_wt_2
      }
      
      #Summary trait_mean_data_Otipulae_P6p_wt_2
      {
        mean_trait_mean_data_Otipulae_P6p_wt_2 <- rbind(mean(trait_mean_data_Otipulae_CONTROL_P6p_wt_mod_2),mean(trait_mean_data_Otipulae_WILD_P6p_wt_mod),mean(trait_mean_data_Otipulae_Vg_P6p_wt_mod_2))
        colnames(mean_trait_mean_data_Otipulae_P6p_wt_2) <- c("mean")
        median_trait_mean_data_Otipulae_P6p_wt_2 <- rbind(median(trait_mean_data_Otipulae_CONTROL_P6p_wt_mod_2),median(trait_mean_data_Otipulae_WILD_P6p_wt_mod),median(trait_mean_data_Otipulae_Vg_P6p_wt_mod_2))
        colnames(median_trait_mean_data_Otipulae_P6p_wt_2) <- c("median")
        posterior.mode_trait_mean_data_Otipulae_P6p_wt_2 <- rbind(posterior.mode(as.mcmc(trait_mean_data_Otipulae_CONTROL_P6p_wt_mod_2)),posterior.mode(as.mcmc(trait_mean_data_Otipulae_WILD_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Otipulae_Vg_P6p_wt_mod_2)))
        colnames(posterior.mode_trait_mean_data_Otipulae_P6p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Otipulae_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Otipulae_CONTROL_P6p_wt_mod_2)),HPDinterval(as.mcmc(trait_mean_data_Otipulae_WILD_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Otipulae_Vg_P6p_wt_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_data_Otipulae_P6p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Otipulae_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Otipulae_CONTROL_P6p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Otipulae_WILD_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Otipulae_Vg_P6p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Otipulae_P6p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Otipulae_P6p_wt_2 <- rbind(effectiveSize(trait_mean_data_Otipulae_CONTROL_P6p_wt_mod_2),effectiveSize(trait_mean_data_Otipulae_WILD_P6p_wt_mod),effectiveSize(trait_mean_data_Otipulae_Vg_P6p_wt_mod_2))
        colnames(effectiveSize_trait_mean_data_Otipulae_P6p_wt_2) <- c("effectiveSize")
        trait_mean_data_Otipulae_P6p_wt_2 <- cbind.data.frame(mean_trait_mean_data_Otipulae_P6p_wt_2,median_trait_mean_data_Otipulae_P6p_wt_2,posterior.mode_trait_mean_data_Otipulae_P6p_wt_2,HPDinterval_0.95_trait_mean_data_Otipulae_P6p_wt_2,HPDinterval_0.83_trait_mean_data_Otipulae_P6p_wt_2,effectiveSize_trait_mean_data_Otipulae_P6p_wt_2)
        rownames(trait_mean_data_Otipulae_P6p_wt_2) <- c("trait_mean_data_Otipulae_CONTROL_P6p_wt_mod_2","trait_mean_data_Otipulae_WILD_P6p_wt_mod","trait_mean_data_Otipulae_Vg_P6p_wt_mod_2")
        trait_mean_data_Otipulae_P6p_wt_2 <- cbind(Models = rownames(trait_mean_data_Otipulae_P6p_wt_2),trait_mean_data_Otipulae_P6p_wt_2)
        rownames(trait_mean_data_Otipulae_P6p_wt_2) <- NULL
        trait_mean_data_Otipulae_P6p_wt_2$Pnp <- c("P6.p","P6.p","P6.p")
        trait_mean_data_Otipulae_P6p_wt_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_data_Otipulae_P6p_wt_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Otipulae_P6p_wt_2$Scale <- c("data","data","data")
        trait_mean_data_Otipulae_P6p_wt_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_data_Otipulae_P6p_wt_2
      }
      
      data_Otipulae_P6p_wt_2 <- rbind.data.frame(va_data_Otipulae_P6p_wt_2, h2_data_Otipulae_P6p_wt_2,Evol_data_Otipulae_P6p_wt_2,trait_mean_data_Otipulae_P6p_wt_2)
      data_Otipulae_P6p_wt_2
      
    }
    Vg_Otipulae_P6p_wt_2 <- rbind.data.frame(liab_Otipulae_P6p_wt_2, data_Otipulae_P6p_wt_2)
    Vg_Otipulae_P6p_wt_2$Pnp_fate <- rep("wt", 24)
    Vg_Otipulae_P6p_wt_2
    #remove Otipulae P6p_wt_2 models
    {
      remove(Otipulae_CONTROL_P6p_wt_mod_2)
      remove(Otipulae_WILD_P6p_wt_mod)
      remove(Otipulae_Vg_P6p_wt_mod_2)
    }
  }
  
  ##Summary Otipulae P7p----
  {
    #Summary liability scale Otipulae P7p
    {
      #Summary va_liab_Otipulae_P7p_wt_2: NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        
        mean_va_liab_Otipulae_P7p_wt_2 <- rbind(mean(va_liab_Otipulae_CONTROL_P7p_wt_mod_2/2),mean(va_liab_Otipulae_WILD_P7p_wt_mod/2),mean(va_liab_Otipulae_Vg_P7p_wt_mod_2/2))
        colnames(mean_va_liab_Otipulae_P7p_wt_2) <- c("mean")
        median_va_liab_Otipulae_P7p_wt_2 <- rbind(median(va_liab_Otipulae_CONTROL_P7p_wt_mod_2/2),median(va_liab_Otipulae_WILD_P7p_wt_mod/2),median(va_liab_Otipulae_Vg_P7p_wt_mod_2/2))
        colnames(median_va_liab_Otipulae_P7p_wt_2) <- c("median")
        posterior.mode_va_liab_Otipulae_P7p_wt_2 <- rbind(posterior.mode(va_liab_Otipulae_CONTROL_P7p_wt_mod_2/2),posterior.mode(va_liab_Otipulae_WILD_P7p_wt_mod/2),posterior.mode(va_liab_Otipulae_Vg_P7p_wt_mod_2/2))
        colnames(posterior.mode_va_liab_Otipulae_P7p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Otipulae_P7p_wt_2 <- rbind(HPDinterval(va_liab_Otipulae_CONTROL_P7p_wt_mod_2/2),HPDinterval(va_liab_Otipulae_WILD_P7p_wt_mod/2),HPDinterval(va_liab_Otipulae_Vg_P7p_wt_mod_2/2))
        colnames(HPDinterval_0.95_va_liab_Otipulae_P7p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Otipulae_P7p_wt_2 <- rbind(HPDinterval(va_liab_Otipulae_CONTROL_P7p_wt_mod_2/2,prob=.83),HPDinterval(va_liab_Otipulae_WILD_P7p_wt_mod/2,prob=.83),HPDinterval(va_liab_Otipulae_Vg_P7p_wt_mod_2/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Otipulae_P7p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Otipulae_P7p_wt_2 <- rbind(effectiveSize(va_liab_Otipulae_CONTROL_P7p_wt_mod_2/2),effectiveSize(va_liab_Otipulae_WILD_P7p_wt_mod/2),effectiveSize(va_liab_Otipulae_Vg_P7p_wt_mod_2/2))
        colnames(effectiveSize_va_liab_Otipulae_P7p_wt_2) <- c("effectiveSize")
        va_liab_Otipulae_P7p_wt_2 <- cbind.data.frame(mean_va_liab_Otipulae_P7p_wt_2,median_va_liab_Otipulae_P7p_wt_2,posterior.mode_va_liab_Otipulae_P7p_wt_2,HPDinterval_0.95_va_liab_Otipulae_P7p_wt_2,HPDinterval_0.83_va_liab_Otipulae_P7p_wt_2,effectiveSize_va_liab_Otipulae_P7p_wt_2)
        rownames(va_liab_Otipulae_P7p_wt_2) <- c("va_liab_Otipulae_CONTROL_P7p_wt_mod_2","va_liab_Otipulae_WILD_P7p_wt_mod","va_liab_Otipulae_Vg_P7p_wt_mod_2")
        va_liab_Otipulae_P7p_wt_2 <- cbind(Models = rownames(va_liab_Otipulae_P7p_wt_2),va_liab_Otipulae_P7p_wt_2)
        rownames(va_liab_Otipulae_P7p_wt_2) <- NULL
        va_liab_Otipulae_P7p_wt_2$Pnp <- c("P7.p","P7.p","P7.p")
        va_liab_Otipulae_P7p_wt_2$Treatment <- c("Control","WILD","Vg")
        va_liab_Otipulae_P7p_wt_2$Measure <- c("Va","Va","Va")
        va_liab_Otipulae_P7p_wt_2$Scale <- c("liab","liab","liab")
        va_liab_Otipulae_P7p_wt_2$Variance <- c("Vg","Vg","Vg")
        va_liab_Otipulae_P7p_wt_2
      }
      
      #Summary h2_liab_Otipulae_P7p_wt_2
      {
        mean_h2_liab_Otipulae_P7p_wt_2 <- rbind(mean(h2_liab_Otipulae_CONTROL_P7p_wt_mod_2),mean(h2_liab_Otipulae_WILD_P7p_wt_mod),mean(h2_liab_Otipulae_Vg_P7p_wt_mod_2))
        colnames(mean_h2_liab_Otipulae_P7p_wt_2) <- c("mean")
        median_h2_liab_Otipulae_P7p_wt_2 <- rbind(median(h2_liab_Otipulae_CONTROL_P7p_wt_mod_2),median(h2_liab_Otipulae_WILD_P7p_wt_mod),median(h2_liab_Otipulae_Vg_P7p_wt_mod_2))
        colnames(median_h2_liab_Otipulae_P7p_wt_2) <- c("median")
        posterior.mode_h2_liab_Otipulae_P7p_wt_2 <- rbind(posterior.mode(h2_liab_Otipulae_CONTROL_P7p_wt_mod_2),posterior.mode(h2_liab_Otipulae_WILD_P7p_wt_mod),posterior.mode(h2_liab_Otipulae_Vg_P7p_wt_mod_2))
        colnames(posterior.mode_h2_liab_Otipulae_P7p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Otipulae_P7p_wt_2 <- rbind(HPDinterval(h2_liab_Otipulae_CONTROL_P7p_wt_mod_2),HPDinterval(h2_liab_Otipulae_WILD_P7p_wt_mod),HPDinterval(h2_liab_Otipulae_Vg_P7p_wt_mod_2))
        colnames(HPDinterval_0.95_h2_liab_Otipulae_P7p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Otipulae_P7p_wt_2 <- rbind(HPDinterval(h2_liab_Otipulae_CONTROL_P7p_wt_mod_2,prob=.83),HPDinterval(h2_liab_Otipulae_WILD_P7p_wt_mod,prob=.83),HPDinterval(h2_liab_Otipulae_Vg_P7p_wt_mod_2,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Otipulae_P7p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Otipulae_P7p_wt_2 <- rbind(effectiveSize(h2_liab_Otipulae_CONTROL_P7p_wt_mod_2),effectiveSize(h2_liab_Otipulae_WILD_P7p_wt_mod),effectiveSize(h2_liab_Otipulae_Vg_P7p_wt_mod_2))
        colnames(effectiveSize_h2_liab_Otipulae_P7p_wt_2) <- c("effectiveSize")
        h2_liab_Otipulae_P7p_wt_2 <- cbind.data.frame(mean_h2_liab_Otipulae_P7p_wt_2,median_h2_liab_Otipulae_P7p_wt_2,posterior.mode_h2_liab_Otipulae_P7p_wt_2,HPDinterval_0.95_h2_liab_Otipulae_P7p_wt_2,HPDinterval_0.83_h2_liab_Otipulae_P7p_wt_2,effectiveSize_h2_liab_Otipulae_P7p_wt_2)
        rownames(h2_liab_Otipulae_P7p_wt_2) <- c("h2_liab_Otipulae_CONTROL_P7p_wt_mod_2","h2_liab_Otipulae_WILD_P7p_wt_mod","h2_liab_Otipulae_Vg_P7p_wt_mod_2")
        h2_liab_Otipulae_P7p_wt_2 <- cbind(Models = rownames(h2_liab_Otipulae_P7p_wt_2),h2_liab_Otipulae_P7p_wt_2)
        rownames(h2_liab_Otipulae_P7p_wt_2) <- NULL
        h2_liab_Otipulae_P7p_wt_2$Pnp <- c("P7.p","P7.p","P7.p")
        h2_liab_Otipulae_P7p_wt_2$Treatment <- c("Control","WILD","Vg")
        h2_liab_Otipulae_P7p_wt_2$Measure <- c("H2","H2","H2")
        h2_liab_Otipulae_P7p_wt_2$Scale <- c("liab","liab","liab")
        h2_liab_Otipulae_P7p_wt_2$Variance <- c("Vg","Vg","Vg")
        h2_liab_Otipulae_P7p_wt_2
      }
      
      #Summary Evol_liab_Otipulae_P7p_wt_2
      {
        mean_Evol_liab_Otipulae_P7p_wt_2 <- rbind(mean(Evol_liab_Otipulae_CONTROL_P7p_wt_mod_2),mean(Evol_liab_Otipulae_WILD_P7p_wt_mod),mean(Evol_liab_Otipulae_Vg_P7p_wt_mod_2))
        colnames(mean_Evol_liab_Otipulae_P7p_wt_2) <- c("mean")
        median_Evol_liab_Otipulae_P7p_wt_2 <- rbind(median(Evol_liab_Otipulae_CONTROL_P7p_wt_mod_2),median(Evol_liab_Otipulae_WILD_P7p_wt_mod),median(Evol_liab_Otipulae_Vg_P7p_wt_mod_2))
        colnames(median_Evol_liab_Otipulae_P7p_wt_2) <- c("median")
        posterior.mode_Evol_liab_Otipulae_P7p_wt_2 <- rbind(posterior.mode(Evol_liab_Otipulae_CONTROL_P7p_wt_mod_2),posterior.mode(Evol_liab_Otipulae_WILD_P7p_wt_mod),posterior.mode(Evol_liab_Otipulae_Vg_P7p_wt_mod_2))
        colnames(posterior.mode_Evol_liab_Otipulae_P7p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Otipulae_P7p_wt_2 <- rbind(HPDinterval(Evol_liab_Otipulae_CONTROL_P7p_wt_mod_2),HPDinterval(Evol_liab_Otipulae_WILD_P7p_wt_mod),HPDinterval(Evol_liab_Otipulae_Vg_P7p_wt_mod_2))
        colnames(HPDinterval_0.95_Evol_liab_Otipulae_P7p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Otipulae_P7p_wt_2 <- rbind(HPDinterval(Evol_liab_Otipulae_CONTROL_P7p_wt_mod_2,prob=.83),HPDinterval(Evol_liab_Otipulae_WILD_P7p_wt_mod,prob=.83),HPDinterval(Evol_liab_Otipulae_Vg_P7p_wt_mod_2,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Otipulae_P7p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Otipulae_P7p_wt_2 <- rbind(effectiveSize(Evol_liab_Otipulae_CONTROL_P7p_wt_mod_2),effectiveSize(Evol_liab_Otipulae_WILD_P7p_wt_mod),effectiveSize(Evol_liab_Otipulae_Vg_P7p_wt_mod_2))
        colnames(effectiveSize_Evol_liab_Otipulae_P7p_wt_2) <- c("effectiveSize")
        Evol_liab_Otipulae_P7p_wt_2 <- cbind.data.frame(mean_Evol_liab_Otipulae_P7p_wt_2,median_Evol_liab_Otipulae_P7p_wt_2,posterior.mode_Evol_liab_Otipulae_P7p_wt_2,HPDinterval_0.95_Evol_liab_Otipulae_P7p_wt_2,HPDinterval_0.83_Evol_liab_Otipulae_P7p_wt_2,effectiveSize_Evol_liab_Otipulae_P7p_wt_2)
        rownames(Evol_liab_Otipulae_P7p_wt_2) <- c("Evol_liab_Otipulae_CONTROL_P7p_wt_mod_2","Evol_liab_Otipulae_WILD_P7p_wt_mod","Evol_liab_Otipulae_Vg_P7p_wt_mod_2")
        Evol_liab_Otipulae_P7p_wt_2 <- cbind(Models = rownames(Evol_liab_Otipulae_P7p_wt_2),Evol_liab_Otipulae_P7p_wt_2)
        rownames(Evol_liab_Otipulae_P7p_wt_2) <- NULL
        Evol_liab_Otipulae_P7p_wt_2$Pnp <- c("P7.p","P7.p","P7.p")
        Evol_liab_Otipulae_P7p_wt_2$Treatment <- c("Control","WILD","Vg")
        Evol_liab_Otipulae_P7p_wt_2$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Otipulae_P7p_wt_2$Scale <- c("liab","liab","liab")
        Evol_liab_Otipulae_P7p_wt_2$Variance <- c("Vg","Vg","Vg")
        Evol_liab_Otipulae_P7p_wt_2
      }
      
      #Summary trait_mean_liab_Otipulae_P7p_wt_2
      {
        mean_trait_mean_liab_Otipulae_P7p_wt_2 <- rbind(mean(trait_mean_liab_Otipulae_CONTROL_P7p_wt_mod_2),mean(trait_mean_liab_Otipulae_WILD_P7p_wt_mod),mean(trait_mean_liab_Otipulae_Vg_P7p_wt_mod_2))
        colnames(mean_trait_mean_liab_Otipulae_P7p_wt_2) <- c("mean")
        median_trait_mean_liab_Otipulae_P7p_wt_2 <- rbind(median(trait_mean_liab_Otipulae_CONTROL_P7p_wt_mod_2),median(trait_mean_liab_Otipulae_WILD_P7p_wt_mod),median(trait_mean_liab_Otipulae_Vg_P7p_wt_mod_2))
        colnames(median_trait_mean_liab_Otipulae_P7p_wt_2) <- c("median")
        posterior.mode_trait_mean_liab_Otipulae_P7p_wt_2 <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P7p_wt_mod_2)),posterior.mode(as.mcmc(trait_mean_liab_Otipulae_WILD_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Otipulae_Vg_P7p_wt_mod_2)))
        colnames(posterior.mode_trait_mean_liab_Otipulae_P7p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Otipulae_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P7p_wt_mod_2)),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_WILD_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_Vg_P7p_wt_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_liab_Otipulae_P7p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Otipulae_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Otipulae_CONTROL_P7p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_WILD_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Otipulae_Vg_P7p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Otipulae_P7p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Otipulae_P7p_wt_2 <- rbind(effectiveSize(trait_mean_liab_Otipulae_CONTROL_P7p_wt_mod_2),effectiveSize(trait_mean_liab_Otipulae_WILD_P7p_wt_mod),effectiveSize(trait_mean_liab_Otipulae_Vg_P7p_wt_mod_2))
        colnames(effectiveSize_trait_mean_liab_Otipulae_P7p_wt_2) <- c("effectiveSize")
        trait_mean_liab_Otipulae_P7p_wt_2 <- cbind.data.frame(mean_trait_mean_liab_Otipulae_P7p_wt_2,median_trait_mean_liab_Otipulae_P7p_wt_2,posterior.mode_trait_mean_liab_Otipulae_P7p_wt_2,HPDinterval_0.95_trait_mean_liab_Otipulae_P7p_wt_2,HPDinterval_0.83_trait_mean_liab_Otipulae_P7p_wt_2,effectiveSize_trait_mean_liab_Otipulae_P7p_wt_2)
        rownames(trait_mean_liab_Otipulae_P7p_wt_2) <- c("trait_mean_liab_Otipulae_CONTROL_P7p_wt_mod_2","trait_mean_liab_Otipulae_WILD_P7p_wt_mod","trait_mean_liab_Otipulae_Vg_P7p_wt_mod_2")
        trait_mean_liab_Otipulae_P7p_wt_2 <- cbind(Models = rownames(trait_mean_liab_Otipulae_P7p_wt_2),trait_mean_liab_Otipulae_P7p_wt_2)
        rownames(trait_mean_liab_Otipulae_P7p_wt_2) <- NULL
        trait_mean_liab_Otipulae_P7p_wt_2$Pnp <- c("P7.p","P7.p","P7.p")
        trait_mean_liab_Otipulae_P7p_wt_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_liab_Otipulae_P7p_wt_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Otipulae_P7p_wt_2$Scale <- c("liab","liab","liab")
        trait_mean_liab_Otipulae_P7p_wt_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_liab_Otipulae_P7p_wt_2
      }
      
      liab_Otipulae_P7p_wt_2 <- rbind.data.frame(va_liab_Otipulae_P7p_wt_2, h2_liab_Otipulae_P7p_wt_2,Evol_liab_Otipulae_P7p_wt_2,trait_mean_liab_Otipulae_P7p_wt_2)
      liab_Otipulae_P7p_wt_2
    }
    #Summary data scale Otipulae P7p
    {
      #Summary va_data_Otipulae_P7p_wt_2:NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        mean_va_data_Otipulae_P7p_wt_2 <- rbind(mean(va_data_Otipulae_CONTROL_P7p_wt_mod_2/2),mean(va_data_Otipulae_WILD_P7p_wt_mod/2),mean(va_data_Otipulae_Vg_P7p_wt_mod_2/2))
        colnames(mean_va_data_Otipulae_P7p_wt_2) <- c("mean")
        median_va_data_Otipulae_P7p_wt_2 <- rbind(median(va_data_Otipulae_CONTROL_P7p_wt_mod_2/2),median(va_data_Otipulae_WILD_P7p_wt_mod/2),median(va_data_Otipulae_Vg_P7p_wt_mod_2/2))
        colnames(median_va_data_Otipulae_P7p_wt_2) <- c("median")
        posterior.mode_va_data_Otipulae_P7p_wt_2 <- rbind(posterior.mode(as.mcmc(va_data_Otipulae_CONTROL_P7p_wt_mod_2/2)),posterior.mode(as.mcmc(va_data_Otipulae_WILD_P7p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Otipulae_Vg_P7p_wt_mod_2/2)))
        colnames(posterior.mode_va_data_Otipulae_P7p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Otipulae_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(va_data_Otipulae_CONTROL_P7p_wt_mod_2/2)),HPDinterval(as.mcmc(va_data_Otipulae_WILD_P7p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Otipulae_Vg_P7p_wt_mod_2/2)))
        colnames(HPDinterval_0.95_va_data_Otipulae_P7p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Otipulae_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(va_data_Otipulae_CONTROL_P7p_wt_mod_2/2),prob=.83),HPDinterval(as.mcmc(va_data_Otipulae_WILD_P7p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Otipulae_Vg_P7p_wt_mod_2/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Otipulae_P7p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Otipulae_P7p_wt_2 <- rbind(effectiveSize(va_data_Otipulae_CONTROL_P7p_wt_mod_2/2),effectiveSize(va_data_Otipulae_WILD_P7p_wt_mod/2),effectiveSize(va_data_Otipulae_Vg_P7p_wt_mod_2/2))
        colnames(effectiveSize_va_data_Otipulae_P7p_wt_2) <- c("effectiveSize")
        va_data_Otipulae_P7p_wt_2 <- cbind.data.frame(mean_va_data_Otipulae_P7p_wt_2,median_va_data_Otipulae_P7p_wt_2,posterior.mode_va_data_Otipulae_P7p_wt_2,HPDinterval_0.95_va_data_Otipulae_P7p_wt_2,HPDinterval_0.83_va_data_Otipulae_P7p_wt_2,effectiveSize_va_data_Otipulae_P7p_wt_2)
        rownames(va_data_Otipulae_P7p_wt_2) <- c("va_data_Otipulae_CONTROL_P7p_wt_mod_2","va_data_Otipulae_WILD_P7p_wt_mod","va_data_Otipulae_Vg_P7p_wt_mod_2")
        va_data_Otipulae_P7p_wt_2 <- cbind(Models = rownames(va_data_Otipulae_P7p_wt_2),va_data_Otipulae_P7p_wt_2)
        rownames(va_data_Otipulae_P7p_wt_2) <- NULL
        va_data_Otipulae_P7p_wt_2$Pnp <- c("P7.p","P7.p","P7.p")
        va_data_Otipulae_P7p_wt_2$Treatment <- c("Control","WILD","Vg")
        va_data_Otipulae_P7p_wt_2$Measure <- c("Va","Va","Va")
        va_data_Otipulae_P7p_wt_2$Scale <- c("data","data","data")
        va_data_Otipulae_P7p_wt_2$Variance <- c("Vg","Vg","Vg")
        va_data_Otipulae_P7p_wt_2
      }
      
      #Summary h2_data_Otipulae_P7p_wt_2
      {
        mean_h2_data_Otipulae_P7p_wt_2 <- rbind(mean(h2_data_Otipulae_CONTROL_P7p_wt_mod_2),mean(h2_data_Otipulae_WILD_P7p_wt_mod),mean(h2_data_Otipulae_Vg_P7p_wt_mod_2))
        colnames(mean_h2_data_Otipulae_P7p_wt_2) <- c("mean")
        median_h2_data_Otipulae_P7p_wt_2 <- rbind(median(h2_data_Otipulae_CONTROL_P7p_wt_mod_2),median(h2_data_Otipulae_WILD_P7p_wt_mod),median(h2_data_Otipulae_Vg_P7p_wt_mod_2))
        colnames(median_h2_data_Otipulae_P7p_wt_2) <- c("median")
        posterior.mode_h2_data_Otipulae_P7p_wt_2 <- rbind(posterior.mode(as.mcmc(h2_data_Otipulae_CONTROL_P7p_wt_mod_2)),posterior.mode(as.mcmc(h2_data_Otipulae_WILD_P7p_wt_mod)),posterior.mode(as.mcmc(h2_data_Otipulae_Vg_P7p_wt_mod_2)))
        colnames(posterior.mode_h2_data_Otipulae_P7p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Otipulae_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(h2_data_Otipulae_CONTROL_P7p_wt_mod_2)),HPDinterval(as.mcmc(h2_data_Otipulae_WILD_P7p_wt_mod)),HPDinterval(as.mcmc(h2_data_Otipulae_Vg_P7p_wt_mod_2)))
        colnames(HPDinterval_0.95_h2_data_Otipulae_P7p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Otipulae_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(h2_data_Otipulae_CONTROL_P7p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(h2_data_Otipulae_WILD_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Otipulae_Vg_P7p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Otipulae_P7p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Otipulae_P7p_wt_2 <- rbind(effectiveSize(h2_data_Otipulae_CONTROL_P7p_wt_mod_2),effectiveSize(h2_data_Otipulae_WILD_P7p_wt_mod),effectiveSize(h2_data_Otipulae_Vg_P7p_wt_mod_2))
        colnames(effectiveSize_h2_data_Otipulae_P7p_wt_2) <- c("effectiveSize")
        h2_data_Otipulae_P7p_wt_2 <- cbind.data.frame(mean_h2_data_Otipulae_P7p_wt_2,median_h2_data_Otipulae_P7p_wt_2,posterior.mode_h2_data_Otipulae_P7p_wt_2,HPDinterval_0.95_h2_data_Otipulae_P7p_wt_2,HPDinterval_0.83_h2_data_Otipulae_P7p_wt_2,effectiveSize_h2_data_Otipulae_P7p_wt_2)
        rownames(h2_data_Otipulae_P7p_wt_2) <- c("h2_data_Otipulae_CONTROL_P7p_wt_mod_2","h2_data_Otipulae_WILD_P7p_wt_mod","h2_data_Otipulae_Vg_P7p_wt_mod_2")
        h2_data_Otipulae_P7p_wt_2 <- cbind(Models = rownames(h2_data_Otipulae_P7p_wt_2),h2_data_Otipulae_P7p_wt_2)
        rownames(h2_data_Otipulae_P7p_wt_2) <- NULL
        h2_data_Otipulae_P7p_wt_2$Pnp <- c("P7.p","P7.p","P7.p")
        h2_data_Otipulae_P7p_wt_2$Treatment <- c("Control","WILD","Vg")
        h2_data_Otipulae_P7p_wt_2$Measure <- c("H2","H2","H2")
        h2_data_Otipulae_P7p_wt_2$Scale <- c("data","data","data")
        h2_data_Otipulae_P7p_wt_2$Variance <- c("Vg","Vg","Vg")
        h2_data_Otipulae_P7p_wt_2
      }
      
      #Summary Evol_data_Otipulae_P7p_wt_2
      {
        mean_Evol_data_Otipulae_P7p_wt_2 <- rbind(mean(Evol_data_Otipulae_CONTROL_P7p_wt_mod_2),mean(Evol_data_Otipulae_WILD_P7p_wt_mod),mean(Evol_data_Otipulae_Vg_P7p_wt_mod_2))
        colnames(mean_Evol_data_Otipulae_P7p_wt_2) <- c("mean")
        median_Evol_data_Otipulae_P7p_wt_2 <- rbind(median(Evol_data_Otipulae_CONTROL_P7p_wt_mod_2),median(Evol_data_Otipulae_WILD_P7p_wt_mod),median(Evol_data_Otipulae_Vg_P7p_wt_mod_2))
        colnames(median_Evol_data_Otipulae_P7p_wt_2) <- c("median")
        posterior.mode_Evol_data_Otipulae_P7p_wt_2 <- rbind(posterior.mode(as.mcmc(Evol_data_Otipulae_CONTROL_P7p_wt_mod_2)),posterior.mode(as.mcmc(Evol_data_Otipulae_WILD_P7p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Otipulae_Vg_P7p_wt_mod_2)))
        colnames(posterior.mode_Evol_data_Otipulae_P7p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Otipulae_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Otipulae_CONTROL_P7p_wt_mod_2)),HPDinterval(as.mcmc(Evol_data_Otipulae_WILD_P7p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Otipulae_Vg_P7p_wt_mod_2)))
        colnames(HPDinterval_0.95_Evol_data_Otipulae_P7p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Otipulae_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Otipulae_CONTROL_P7p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(Evol_data_Otipulae_WILD_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Otipulae_Vg_P7p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Otipulae_P7p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Otipulae_P7p_wt_2 <- rbind(effectiveSize(Evol_data_Otipulae_CONTROL_P7p_wt_mod_2),effectiveSize(Evol_data_Otipulae_WILD_P7p_wt_mod),effectiveSize(Evol_data_Otipulae_Vg_P7p_wt_mod_2))
        colnames(effectiveSize_Evol_data_Otipulae_P7p_wt_2) <- c("effectiveSize")
        Evol_data_Otipulae_P7p_wt_2 <- cbind.data.frame(mean_Evol_data_Otipulae_P7p_wt_2,median_Evol_data_Otipulae_P7p_wt_2,posterior.mode_Evol_data_Otipulae_P7p_wt_2,HPDinterval_0.95_Evol_data_Otipulae_P7p_wt_2,HPDinterval_0.83_Evol_data_Otipulae_P7p_wt_2,effectiveSize_Evol_data_Otipulae_P7p_wt_2)
        rownames(Evol_data_Otipulae_P7p_wt_2) <- c("Evol_data_Otipulae_CONTROL_P7p_wt_mod_2","Evol_data_Otipulae_WILD_P7p_wt_mod","Evol_data_Otipulae_Vg_P7p_wt_mod_2")
        Evol_data_Otipulae_P7p_wt_2 <- cbind(Models = rownames(Evol_data_Otipulae_P7p_wt_2),Evol_data_Otipulae_P7p_wt_2)
        rownames(Evol_data_Otipulae_P7p_wt_2) <- NULL
        Evol_data_Otipulae_P7p_wt_2$Pnp <- c("P7.p","P7.p","P7.p")
        Evol_data_Otipulae_P7p_wt_2$Treatment <- c("Control","WILD","Vg")
        Evol_data_Otipulae_P7p_wt_2$Measure <- c("Evol","Evol","Evol")
        Evol_data_Otipulae_P7p_wt_2$Scale <- c("data","data","data")
        Evol_data_Otipulae_P7p_wt_2$Variance <- c("Vg","Vg","Vg")
        Evol_data_Otipulae_P7p_wt_2
      }
      
      #Summary trait_mean_data_Otipulae_P7p_wt_2
      {
        mean_trait_mean_data_Otipulae_P7p_wt_2 <- rbind(mean(trait_mean_data_Otipulae_CONTROL_P7p_wt_mod_2),mean(trait_mean_data_Otipulae_WILD_P7p_wt_mod),mean(trait_mean_data_Otipulae_Vg_P7p_wt_mod_2))
        colnames(mean_trait_mean_data_Otipulae_P7p_wt_2) <- c("mean")
        median_trait_mean_data_Otipulae_P7p_wt_2 <- rbind(median(trait_mean_data_Otipulae_CONTROL_P7p_wt_mod_2),median(trait_mean_data_Otipulae_WILD_P7p_wt_mod),median(trait_mean_data_Otipulae_Vg_P7p_wt_mod_2))
        colnames(median_trait_mean_data_Otipulae_P7p_wt_2) <- c("median")
        posterior.mode_trait_mean_data_Otipulae_P7p_wt_2 <- rbind(posterior.mode(as.mcmc(trait_mean_data_Otipulae_CONTROL_P7p_wt_mod_2)),posterior.mode(as.mcmc(trait_mean_data_Otipulae_WILD_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Otipulae_Vg_P7p_wt_mod_2)))
        colnames(posterior.mode_trait_mean_data_Otipulae_P7p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Otipulae_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Otipulae_CONTROL_P7p_wt_mod_2)),HPDinterval(as.mcmc(trait_mean_data_Otipulae_WILD_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Otipulae_Vg_P7p_wt_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_data_Otipulae_P7p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Otipulae_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Otipulae_CONTROL_P7p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Otipulae_WILD_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Otipulae_Vg_P7p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Otipulae_P7p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Otipulae_P7p_wt_2 <- rbind(effectiveSize(trait_mean_data_Otipulae_CONTROL_P7p_wt_mod_2),effectiveSize(trait_mean_data_Otipulae_WILD_P7p_wt_mod),effectiveSize(trait_mean_data_Otipulae_Vg_P7p_wt_mod_2))
        colnames(effectiveSize_trait_mean_data_Otipulae_P7p_wt_2) <- c("effectiveSize")
        trait_mean_data_Otipulae_P7p_wt_2 <- cbind.data.frame(mean_trait_mean_data_Otipulae_P7p_wt_2,median_trait_mean_data_Otipulae_P7p_wt_2,posterior.mode_trait_mean_data_Otipulae_P7p_wt_2,HPDinterval_0.95_trait_mean_data_Otipulae_P7p_wt_2,HPDinterval_0.83_trait_mean_data_Otipulae_P7p_wt_2,effectiveSize_trait_mean_data_Otipulae_P7p_wt_2)
        rownames(trait_mean_data_Otipulae_P7p_wt_2) <- c("trait_mean_data_Otipulae_CONTROL_P7p_wt_mod_2","trait_mean_data_Otipulae_WILD_P7p_wt_mod","trait_mean_data_Otipulae_Vg_P7p_wt_mod_2")
        trait_mean_data_Otipulae_P7p_wt_2 <- cbind(Models = rownames(trait_mean_data_Otipulae_P7p_wt_2),trait_mean_data_Otipulae_P7p_wt_2)
        rownames(trait_mean_data_Otipulae_P7p_wt_2) <- NULL
        trait_mean_data_Otipulae_P7p_wt_2$Pnp <- c("P7.p","P7.p","P7.p")
        trait_mean_data_Otipulae_P7p_wt_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_data_Otipulae_P7p_wt_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Otipulae_P7p_wt_2$Scale <- c("data","data","data")
        trait_mean_data_Otipulae_P7p_wt_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_data_Otipulae_P7p_wt_2
      }
      
      data_Otipulae_P7p_wt_2 <- rbind.data.frame(va_data_Otipulae_P7p_wt_2, h2_data_Otipulae_P7p_wt_2,Evol_data_Otipulae_P7p_wt_2,trait_mean_data_Otipulae_P7p_wt_2)
      data_Otipulae_P7p_wt_2
      
    }
    Vg_Otipulae_P7p_wt_2 <- rbind.data.frame(liab_Otipulae_P7p_wt_2, data_Otipulae_P7p_wt_2)
    Vg_Otipulae_P7p_wt_2$Pnp_fate <- rep("wt", 24)
    Vg_Otipulae_P7p_wt_2
    #remove Otipulae P7p_wt_2 models
    {
      remove(Otipulae_CONTROL_P7p_wt_mod_2)
      remove(Otipulae_WILD_P7p_wt_mod)
      remove(Otipulae_Vg_P7p_wt_mod_2)
    }
  }
  
  Vg_Otipulae_summary_2 <- rbind.data.frame(Vg_Otipulae_P3p_SSSS_2,Vg_Otipulae_P4p_SSSS_2,Vg_Otipulae_P5p_wt_2,Vg_Otipulae_P6p_wt_2,Vg_Otipulae_P7p_wt_2,Vg_Otipulae_P8p_SSSS_2)
  Vg_Otipulae_summary_2$Species <- rep("O.tipulae",144)
  Vg_Otipulae_summary_2$Genus <- rep("Oscheius",144)
  View(Vg_Otipulae_summary_2)
  
  
  ##Vg_Otipulae_P3p_divided_P4p_SSSS_2----
  {
    #Vg_Otipulae_P3p_divided_P4p_SSSS_2_liab
    {
      
      va_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2 <- va_liab_Otipulae_Vg_P3p_SSSS_mod_2 / va_liab_Otipulae_Vg_P4p_SSSS_mod_2
      h2_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2 <- h2_liab_Otipulae_Vg_P3p_SSSS_mod_2 / h2_liab_Otipulae_Vg_P4p_SSSS_mod_2
      Evol_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2 <- Evol_liab_Otipulae_Vg_P3p_SSSS_mod_2 / Evol_liab_Otipulae_Vg_P4p_SSSS_mod_2
      
      mean_va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2 <- rbind(mean(log10(va_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)),mean(log10(h2_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)), mean(log10(Evol_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)))
      colnames(mean_va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2) <- c("mean")
      median_va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2 <- rbind(median(log10(va_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)),median(log10(h2_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)), median(log10(Evol_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)))
      colnames(median_va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2) <- c("median")
      posterior.mode_va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2 <- rbind(posterior.mode(as.mcmc(log10(va_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2))),posterior.mode(as.mcmc(log10(h2_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2))),posterior.mode(as.mcmc(log10(Evol_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2))))
      colnames(posterior.mode_va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2) <- c("posterior.mode")
      HPDinterval_0.95_va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(log10(va_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2))),HPDinterval(as.mcmc(log10(h2_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2))),HPDinterval(as.mcmc(log10(Evol_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2))))
      colnames(HPDinterval_0.95_va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2) <- c("CI_lower_0.95","CI_upper_0.95")
      HPDinterval_0.83_va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(log10(va_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)),prob=.83),HPDinterval(as.mcmc(log10(h2_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)),prob=.83),HPDinterval(as.mcmc(log10(Evol_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)),prob=.83))
      colnames(HPDinterval_0.83_va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2) <- c("CI_lower_0.83","CI_upper_0.83")
      va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2 <- cbind.data.frame(mean_va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2,median_va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2,posterior.mode_va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2,HPDinterval_0.95_va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2,HPDinterval_0.83_va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2)
      rownames(va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2) <- c("va_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_log10","h2_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_log10","Evol_liab_Otipulae_Vg_P3p_divided_P4p_SSSS_log10")
      va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2$Measure <- c("Va","H2", "Evol")
      va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2$Species <- rep("O.tipulae",3)
      va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2$Genus <- rep("Oscheius",3)
      va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2$Scale <- rep("liab",3)
      va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2
      
      pdf("Vg_va_h2_Evol_liab_P3p_divided_P4p_SSSS_2_log10_Otipulae.pdf")
      ggplot(va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2, aes(x=Measure, y= median)) +
        geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
        geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
        geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
        theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
        labs(y = "Va log10 ( P3.p / P4.p)", title = "Otipulae_log10(P3p/P4p)")
      dev.off() 
      
    }
    
    #Vg_Otipulae_P3p_divided_P4p_SSSS_2_data
    
    {
      va_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2 <- va_data_Otipulae_Vg_P3p_SSSS_mod_2 / va_data_Otipulae_Vg_P4p_SSSS_mod_2
      h2_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2 <- h2_data_Otipulae_Vg_P3p_SSSS_mod_2 / h2_data_Otipulae_Vg_P4p_SSSS_mod_2
      Evol_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2 <- Evol_data_Otipulae_Vg_P3p_SSSS_mod_2 / Evol_data_Otipulae_Vg_P4p_SSSS_mod_2
      
      mean_va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2 <- rbind(mean(log10(va_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)),mean(log10(h2_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)), mean(log10(Evol_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)))
      colnames(mean_va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2) <- c("mean")
      median_va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2 <- rbind(median(log10(va_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)),median(log10(h2_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)), median(log10(Evol_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)))
      colnames(median_va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2) <- c("median")
      posterior.mode_va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2 <- rbind(posterior.mode(as.mcmc(log10(va_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2))),posterior.mode(as.mcmc(log10(h2_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2))),posterior.mode(as.mcmc(log10(Evol_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2))))
      colnames(posterior.mode_va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2) <- c("posterior.mode")
      HPDinterval_0.95_va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(log10(va_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2))),HPDinterval(as.mcmc(log10(h2_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2))),HPDinterval(as.mcmc(log10(Evol_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2))))
      colnames(HPDinterval_0.95_va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2) <- c("CI_lower_0.95","CI_upper_0.95")
      HPDinterval_0.83_va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(log10(va_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)),prob=.83),HPDinterval(as.mcmc(log10(h2_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)),prob=.83),HPDinterval(as.mcmc(log10(Evol_data_Otipulae_Vg_P3p_divided_P4p_SSSS_mod_2)),prob=.83))
      colnames(HPDinterval_0.83_va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2) <- c("CI_lower_0.83","CI_upper_0.83")
      va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2 <- cbind.data.frame(mean_va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2,median_va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2,posterior.mode_va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2,HPDinterval_0.95_va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2,HPDinterval_0.83_va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2)
      rownames(va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2) <- c("va_data_Otipulae_Vg_P3p_divided_P4p_SSSS_log10","h2_data_Otipulae_Vg_P3p_divided_P4p_SSSS_log10","Evol_data_Otipulae_Vg_P3p_divided_P4p_SSSS_log10")
      va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2$Measure <- c("Va","H2", "Evol")
      va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2$Species <- rep("O.tipulae",3)
      va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2$Genus <- rep("Oscheius",3)
      va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2$Scale <- rep("data",3)
      va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2
      
      pdf("Vg_va_h2_Evol_data_P3p_divided_P4p_SSSS_2_log10_Otipulae.pdf")
      ggplot(va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2, aes(x=Measure, y= median)) +
        geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
        geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
        geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
        theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
        labs(y = "Va log10 ( P3.p / P4.p)", title = "Otipulae_log10(P3p/P4p)")
      dev.off() 
      
    }
    
    va_h2_Evol_Vg_Otipulae_P3p_divided_P4p_SSSS_summary_2 <- rbind.data.frame(va_h2_Evol_liab_Vg_Otipulae_P3p_divided_P4p_SSSS_2,va_h2_Evol_data_Vg_Otipulae_P3p_divided_P4p_SSSS_2)
    va_h2_Evol_Vg_Otipulae_P3p_divided_P4p_SSSS_summary_2
    
  }
}


# Oonirici ----
{
  ##Summary Oonirici P3p ----
  {
    #Summary liability scale Oonirici P3p
    {
      #Summary va_liab_Oonirici_P3p_SSSS_2: NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        
        mean_va_liab_Oonirici_P3p_SSSS_2 <- rbind(mean(va_liab_Oonirici_CONTROL_P3p_SSSS_mod_2/2),mean(va_liab_Oonirici_WILD_P3p_SSSS_mod/2),mean(va_liab_Oonirici_Vg_P3p_SSSS_mod_2/2))
        colnames(mean_va_liab_Oonirici_P3p_SSSS_2) <- c("mean")
        median_va_liab_Oonirici_P3p_SSSS_2 <- rbind(median(va_liab_Oonirici_CONTROL_P3p_SSSS_mod_2/2),median(va_liab_Oonirici_WILD_P3p_SSSS_mod/2),median(va_liab_Oonirici_Vg_P3p_SSSS_mod_2/2))
        colnames(median_va_liab_Oonirici_P3p_SSSS_2) <- c("median")
        posterior.mode_va_liab_Oonirici_P3p_SSSS_2 <- rbind(posterior.mode(va_liab_Oonirici_CONTROL_P3p_SSSS_mod_2/2),posterior.mode(va_liab_Oonirici_WILD_P3p_SSSS_mod/2),posterior.mode(va_liab_Oonirici_Vg_P3p_SSSS_mod_2/2))
        colnames(posterior.mode_va_liab_Oonirici_P3p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Oonirici_P3p_SSSS_2 <- rbind(HPDinterval(va_liab_Oonirici_CONTROL_P3p_SSSS_mod_2/2),HPDinterval(va_liab_Oonirici_WILD_P3p_SSSS_mod/2),HPDinterval(va_liab_Oonirici_Vg_P3p_SSSS_mod_2/2))
        colnames(HPDinterval_0.95_va_liab_Oonirici_P3p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Oonirici_P3p_SSSS_2 <- rbind(HPDinterval(va_liab_Oonirici_CONTROL_P3p_SSSS_mod_2/2,prob=.83),HPDinterval(va_liab_Oonirici_WILD_P3p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_Oonirici_Vg_P3p_SSSS_mod_2/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Oonirici_P3p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Oonirici_P3p_SSSS_2 <- rbind(effectiveSize(va_liab_Oonirici_CONTROL_P3p_SSSS_mod_2/2),effectiveSize(va_liab_Oonirici_WILD_P3p_SSSS_mod/2),effectiveSize(va_liab_Oonirici_Vg_P3p_SSSS_mod_2/2))
        colnames(effectiveSize_va_liab_Oonirici_P3p_SSSS_2) <- c("effectiveSize")
        va_liab_Oonirici_P3p_SSSS_2 <- cbind.data.frame(mean_va_liab_Oonirici_P3p_SSSS_2,median_va_liab_Oonirici_P3p_SSSS_2,posterior.mode_va_liab_Oonirici_P3p_SSSS_2,HPDinterval_0.95_va_liab_Oonirici_P3p_SSSS_2,HPDinterval_0.83_va_liab_Oonirici_P3p_SSSS_2,effectiveSize_va_liab_Oonirici_P3p_SSSS_2)
        rownames(va_liab_Oonirici_P3p_SSSS_2) <- c("va_liab_Oonirici_CONTROL_P3p_SSSS_mod_2","va_liab_Oonirici_WILD_P3p_SSSS_mod","va_liab_Oonirici_Vg_P3p_SSSS_mod_2")
        va_liab_Oonirici_P3p_SSSS_2 <- cbind(Models = rownames(va_liab_Oonirici_P3p_SSSS_2),va_liab_Oonirici_P3p_SSSS_2)
        rownames(va_liab_Oonirici_P3p_SSSS_2) <- NULL
        va_liab_Oonirici_P3p_SSSS_2$Pnp <- c("P3.p","P3.p","P3.p")
        va_liab_Oonirici_P3p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        va_liab_Oonirici_P3p_SSSS_2$Measure <- c("Va","Va","Va")
        va_liab_Oonirici_P3p_SSSS_2$Scale <- c("liab","liab","liab")
        va_liab_Oonirici_P3p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        va_liab_Oonirici_P3p_SSSS_2
      }
      
      #Summary h2_liab_Oonirici_P3p_SSSS_2
      {
        mean_h2_liab_Oonirici_P3p_SSSS_2 <- rbind(mean(h2_liab_Oonirici_CONTROL_P3p_SSSS_mod_2),mean(h2_liab_Oonirici_WILD_P3p_SSSS_mod),mean(h2_liab_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(mean_h2_liab_Oonirici_P3p_SSSS_2) <- c("mean")
        median_h2_liab_Oonirici_P3p_SSSS_2 <- rbind(median(h2_liab_Oonirici_CONTROL_P3p_SSSS_mod_2),median(h2_liab_Oonirici_WILD_P3p_SSSS_mod),median(h2_liab_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(median_h2_liab_Oonirici_P3p_SSSS_2) <- c("median")
        posterior.mode_h2_liab_Oonirici_P3p_SSSS_2 <- rbind(posterior.mode(h2_liab_Oonirici_CONTROL_P3p_SSSS_mod_2),posterior.mode(h2_liab_Oonirici_WILD_P3p_SSSS_mod),posterior.mode(h2_liab_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(posterior.mode_h2_liab_Oonirici_P3p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Oonirici_P3p_SSSS_2 <- rbind(HPDinterval(h2_liab_Oonirici_CONTROL_P3p_SSSS_mod_2),HPDinterval(h2_liab_Oonirici_WILD_P3p_SSSS_mod),HPDinterval(h2_liab_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(HPDinterval_0.95_h2_liab_Oonirici_P3p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Oonirici_P3p_SSSS_2 <- rbind(HPDinterval(h2_liab_Oonirici_CONTROL_P3p_SSSS_mod_2,prob=.83),HPDinterval(h2_liab_Oonirici_WILD_P3p_SSSS_mod,prob=.83),HPDinterval(h2_liab_Oonirici_Vg_P3p_SSSS_mod_2,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Oonirici_P3p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Oonirici_P3p_SSSS_2 <- rbind(effectiveSize(h2_liab_Oonirici_CONTROL_P3p_SSSS_mod_2),effectiveSize(h2_liab_Oonirici_WILD_P3p_SSSS_mod),effectiveSize(h2_liab_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(effectiveSize_h2_liab_Oonirici_P3p_SSSS_2) <- c("effectiveSize")
        h2_liab_Oonirici_P3p_SSSS_2 <- cbind.data.frame(mean_h2_liab_Oonirici_P3p_SSSS_2,median_h2_liab_Oonirici_P3p_SSSS_2,posterior.mode_h2_liab_Oonirici_P3p_SSSS_2,HPDinterval_0.95_h2_liab_Oonirici_P3p_SSSS_2,HPDinterval_0.83_h2_liab_Oonirici_P3p_SSSS_2,effectiveSize_h2_liab_Oonirici_P3p_SSSS_2)
        rownames(h2_liab_Oonirici_P3p_SSSS_2) <- c("h2_liab_Oonirici_CONTROL_P3p_SSSS_mod_2","h2_liab_Oonirici_WILD_P3p_SSSS_mod","h2_liab_Oonirici_Vg_P3p_SSSS_mod_2")
        h2_liab_Oonirici_P3p_SSSS_2 <- cbind(Models = rownames(h2_liab_Oonirici_P3p_SSSS_2),h2_liab_Oonirici_P3p_SSSS_2)
        rownames(h2_liab_Oonirici_P3p_SSSS_2) <- NULL
        h2_liab_Oonirici_P3p_SSSS_2$Pnp <- c("P3.p","P3.p","P3.p")
        h2_liab_Oonirici_P3p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        h2_liab_Oonirici_P3p_SSSS_2$Measure <- c("H2","H2","H2")
        h2_liab_Oonirici_P3p_SSSS_2$Scale <- c("liab","liab","liab")
        h2_liab_Oonirici_P3p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        h2_liab_Oonirici_P3p_SSSS_2
      }
      
      #Summary Evol_liab_Oonirici_P3p_SSSS_2
      {
        mean_Evol_liab_Oonirici_P3p_SSSS_2 <- rbind(mean(Evol_liab_Oonirici_CONTROL_P3p_SSSS_mod_2),mean(Evol_liab_Oonirici_WILD_P3p_SSSS_mod),mean(Evol_liab_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(mean_Evol_liab_Oonirici_P3p_SSSS_2) <- c("mean")
        median_Evol_liab_Oonirici_P3p_SSSS_2 <- rbind(median(Evol_liab_Oonirici_CONTROL_P3p_SSSS_mod_2),median(Evol_liab_Oonirici_WILD_P3p_SSSS_mod),median(Evol_liab_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(median_Evol_liab_Oonirici_P3p_SSSS_2) <- c("median")
        posterior.mode_Evol_liab_Oonirici_P3p_SSSS_2 <- rbind(posterior.mode(Evol_liab_Oonirici_CONTROL_P3p_SSSS_mod_2),posterior.mode(Evol_liab_Oonirici_WILD_P3p_SSSS_mod),posterior.mode(Evol_liab_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(posterior.mode_Evol_liab_Oonirici_P3p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Oonirici_P3p_SSSS_2 <- rbind(HPDinterval(Evol_liab_Oonirici_CONTROL_P3p_SSSS_mod_2),HPDinterval(Evol_liab_Oonirici_WILD_P3p_SSSS_mod),HPDinterval(Evol_liab_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(HPDinterval_0.95_Evol_liab_Oonirici_P3p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Oonirici_P3p_SSSS_2 <- rbind(HPDinterval(Evol_liab_Oonirici_CONTROL_P3p_SSSS_mod_2,prob=.83),HPDinterval(Evol_liab_Oonirici_WILD_P3p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_Oonirici_Vg_P3p_SSSS_mod_2,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Oonirici_P3p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Oonirici_P3p_SSSS_2 <- rbind(effectiveSize(Evol_liab_Oonirici_CONTROL_P3p_SSSS_mod_2),effectiveSize(Evol_liab_Oonirici_WILD_P3p_SSSS_mod),effectiveSize(Evol_liab_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(effectiveSize_Evol_liab_Oonirici_P3p_SSSS_2) <- c("effectiveSize")
        Evol_liab_Oonirici_P3p_SSSS_2 <- cbind.data.frame(mean_Evol_liab_Oonirici_P3p_SSSS_2,median_Evol_liab_Oonirici_P3p_SSSS_2,posterior.mode_Evol_liab_Oonirici_P3p_SSSS_2,HPDinterval_0.95_Evol_liab_Oonirici_P3p_SSSS_2,HPDinterval_0.83_Evol_liab_Oonirici_P3p_SSSS_2,effectiveSize_Evol_liab_Oonirici_P3p_SSSS_2)
        rownames(Evol_liab_Oonirici_P3p_SSSS_2) <- c("Evol_liab_Oonirici_CONTROL_P3p_SSSS_mod_2","Evol_liab_Oonirici_WILD_P3p_SSSS_mod","Evol_liab_Oonirici_Vg_P3p_SSSS_mod_2")
        Evol_liab_Oonirici_P3p_SSSS_2 <- cbind(Models = rownames(Evol_liab_Oonirici_P3p_SSSS_2),Evol_liab_Oonirici_P3p_SSSS_2)
        rownames(Evol_liab_Oonirici_P3p_SSSS_2) <- NULL
        Evol_liab_Oonirici_P3p_SSSS_2$Pnp <- c("P3.p","P3.p","P3.p")
        Evol_liab_Oonirici_P3p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        Evol_liab_Oonirici_P3p_SSSS_2$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Oonirici_P3p_SSSS_2$Scale <- c("liab","liab","liab")
        Evol_liab_Oonirici_P3p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        Evol_liab_Oonirici_P3p_SSSS_2
      }
      
      #Summary trait_mean_liab_Oonirici_P3p_SSSS_2
      {
        mean_trait_mean_liab_Oonirici_P3p_SSSS_2 <- rbind(mean(trait_mean_liab_Oonirici_CONTROL_P3p_SSSS_mod_2),mean(trait_mean_liab_Oonirici_WILD_P3p_SSSS_mod),mean(trait_mean_liab_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(mean_trait_mean_liab_Oonirici_P3p_SSSS_2) <- c("mean")
        median_trait_mean_liab_Oonirici_P3p_SSSS_2 <- rbind(median(trait_mean_liab_Oonirici_CONTROL_P3p_SSSS_mod_2),median(trait_mean_liab_Oonirici_WILD_P3p_SSSS_mod),median(trait_mean_liab_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(median_trait_mean_liab_Oonirici_P3p_SSSS_2) <- c("median")
        posterior.mode_trait_mean_liab_Oonirici_P3p_SSSS_2 <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P3p_SSSS_mod_2)),posterior.mode(as.mcmc(trait_mean_liab_Oonirici_WILD_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Oonirici_Vg_P3p_SSSS_mod_2)))
        colnames(posterior.mode_trait_mean_liab_Oonirici_P3p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Oonirici_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P3p_SSSS_mod_2)),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_WILD_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_Vg_P3p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_liab_Oonirici_P3p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Oonirici_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P3p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_WILD_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_Vg_P3p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Oonirici_P3p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Oonirici_P3p_SSSS_2 <- rbind(effectiveSize(trait_mean_liab_Oonirici_CONTROL_P3p_SSSS_mod_2),effectiveSize(trait_mean_liab_Oonirici_WILD_P3p_SSSS_mod),effectiveSize(trait_mean_liab_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(effectiveSize_trait_mean_liab_Oonirici_P3p_SSSS_2) <- c("effectiveSize")
        trait_mean_liab_Oonirici_P3p_SSSS_2 <- cbind.data.frame(mean_trait_mean_liab_Oonirici_P3p_SSSS_2,median_trait_mean_liab_Oonirici_P3p_SSSS_2,posterior.mode_trait_mean_liab_Oonirici_P3p_SSSS_2,HPDinterval_0.95_trait_mean_liab_Oonirici_P3p_SSSS_2,HPDinterval_0.83_trait_mean_liab_Oonirici_P3p_SSSS_2,effectiveSize_trait_mean_liab_Oonirici_P3p_SSSS_2)
        rownames(trait_mean_liab_Oonirici_P3p_SSSS_2) <- c("trait_mean_liab_Oonirici_CONTROL_P3p_SSSS_mod_2","trait_mean_liab_Oonirici_WILD_P3p_SSSS_mod","trait_mean_liab_Oonirici_Vg_P3p_SSSS_mod_2")
        trait_mean_liab_Oonirici_P3p_SSSS_2 <- cbind(Models = rownames(trait_mean_liab_Oonirici_P3p_SSSS_2),trait_mean_liab_Oonirici_P3p_SSSS_2)
        rownames(trait_mean_liab_Oonirici_P3p_SSSS_2) <- NULL
        trait_mean_liab_Oonirici_P3p_SSSS_2$Pnp <- c("P3.p","P3.p","P3.p")
        trait_mean_liab_Oonirici_P3p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_liab_Oonirici_P3p_SSSS_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Oonirici_P3p_SSSS_2$Scale <- c("liab","liab","liab")
        trait_mean_liab_Oonirici_P3p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_liab_Oonirici_P3p_SSSS_2
      }
      
      liab_Oonirici_P3p_SSSS_2 <- rbind.data.frame(va_liab_Oonirici_P3p_SSSS_2, h2_liab_Oonirici_P3p_SSSS_2,Evol_liab_Oonirici_P3p_SSSS_2,trait_mean_liab_Oonirici_P3p_SSSS_2)
      liab_Oonirici_P3p_SSSS_2
    }
    #Summary data scale Oonirici P3p
    {
      #Summary va_data_Oonirici_P3p_SSSS_2:NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        mean_va_data_Oonirici_P3p_SSSS_2 <- rbind(mean(va_data_Oonirici_CONTROL_P3p_SSSS_mod_2/2),mean(va_data_Oonirici_WILD_P3p_SSSS_mod/2),mean(va_data_Oonirici_Vg_P3p_SSSS_mod_2/2))
        colnames(mean_va_data_Oonirici_P3p_SSSS_2) <- c("mean")
        median_va_data_Oonirici_P3p_SSSS_2 <- rbind(median(va_data_Oonirici_CONTROL_P3p_SSSS_mod_2/2),median(va_data_Oonirici_WILD_P3p_SSSS_mod/2),median(va_data_Oonirici_Vg_P3p_SSSS_mod_2/2))
        colnames(median_va_data_Oonirici_P3p_SSSS_2) <- c("median")
        posterior.mode_va_data_Oonirici_P3p_SSSS_2 <- rbind(posterior.mode(as.mcmc(va_data_Oonirici_CONTROL_P3p_SSSS_mod_2/2)),posterior.mode(as.mcmc(va_data_Oonirici_WILD_P3p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_Oonirici_Vg_P3p_SSSS_mod_2/2)))
        colnames(posterior.mode_va_data_Oonirici_P3p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Oonirici_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(va_data_Oonirici_CONTROL_P3p_SSSS_mod_2/2)),HPDinterval(as.mcmc(va_data_Oonirici_WILD_P3p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_Oonirici_Vg_P3p_SSSS_mod_2/2)))
        colnames(HPDinterval_0.95_va_data_Oonirici_P3p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Oonirici_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(va_data_Oonirici_CONTROL_P3p_SSSS_mod_2/2),prob=.83),HPDinterval(as.mcmc(va_data_Oonirici_WILD_P3p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Oonirici_Vg_P3p_SSSS_mod_2/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Oonirici_P3p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Oonirici_P3p_SSSS_2 <- rbind(effectiveSize(va_data_Oonirici_CONTROL_P3p_SSSS_mod_2/2),effectiveSize(va_data_Oonirici_WILD_P3p_SSSS_mod/2),effectiveSize(va_data_Oonirici_Vg_P3p_SSSS_mod_2/2))
        colnames(effectiveSize_va_data_Oonirici_P3p_SSSS_2) <- c("effectiveSize")
        va_data_Oonirici_P3p_SSSS_2 <- cbind.data.frame(mean_va_data_Oonirici_P3p_SSSS_2,median_va_data_Oonirici_P3p_SSSS_2,posterior.mode_va_data_Oonirici_P3p_SSSS_2,HPDinterval_0.95_va_data_Oonirici_P3p_SSSS_2,HPDinterval_0.83_va_data_Oonirici_P3p_SSSS_2,effectiveSize_va_data_Oonirici_P3p_SSSS_2)
        rownames(va_data_Oonirici_P3p_SSSS_2) <- c("va_data_Oonirici_CONTROL_P3p_SSSS_mod_2","va_data_Oonirici_WILD_P3p_SSSS_mod","va_data_Oonirici_Vg_P3p_SSSS_mod_2")
        va_data_Oonirici_P3p_SSSS_2 <- cbind(Models = rownames(va_data_Oonirici_P3p_SSSS_2),va_data_Oonirici_P3p_SSSS_2)
        rownames(va_data_Oonirici_P3p_SSSS_2) <- NULL
        va_data_Oonirici_P3p_SSSS_2$Pnp <- c("P3.p","P3.p","P3.p")
        va_data_Oonirici_P3p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        va_data_Oonirici_P3p_SSSS_2$Measure <- c("Va","Va","Va")
        va_data_Oonirici_P3p_SSSS_2$Scale <- c("data","data","data")
        va_data_Oonirici_P3p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        va_data_Oonirici_P3p_SSSS_2
      }
      
      #Summary h2_data_Oonirici_P3p_SSSS_2
      {
        mean_h2_data_Oonirici_P3p_SSSS_2 <- rbind(mean(h2_data_Oonirici_CONTROL_P3p_SSSS_mod_2),mean(h2_data_Oonirici_WILD_P3p_SSSS_mod),mean(h2_data_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(mean_h2_data_Oonirici_P3p_SSSS_2) <- c("mean")
        median_h2_data_Oonirici_P3p_SSSS_2 <- rbind(median(h2_data_Oonirici_CONTROL_P3p_SSSS_mod_2),median(h2_data_Oonirici_WILD_P3p_SSSS_mod),median(h2_data_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(median_h2_data_Oonirici_P3p_SSSS_2) <- c("median")
        posterior.mode_h2_data_Oonirici_P3p_SSSS_2 <- rbind(posterior.mode(as.mcmc(h2_data_Oonirici_CONTROL_P3p_SSSS_mod_2)),posterior.mode(as.mcmc(h2_data_Oonirici_WILD_P3p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_Oonirici_Vg_P3p_SSSS_mod_2)))
        colnames(posterior.mode_h2_data_Oonirici_P3p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Oonirici_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(h2_data_Oonirici_CONTROL_P3p_SSSS_mod_2)),HPDinterval(as.mcmc(h2_data_Oonirici_WILD_P3p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_Oonirici_Vg_P3p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_h2_data_Oonirici_P3p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Oonirici_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(h2_data_Oonirici_CONTROL_P3p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(h2_data_Oonirici_WILD_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Oonirici_Vg_P3p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Oonirici_P3p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Oonirici_P3p_SSSS_2 <- rbind(effectiveSize(h2_data_Oonirici_CONTROL_P3p_SSSS_mod_2),effectiveSize(h2_data_Oonirici_WILD_P3p_SSSS_mod),effectiveSize(h2_data_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(effectiveSize_h2_data_Oonirici_P3p_SSSS_2) <- c("effectiveSize")
        h2_data_Oonirici_P3p_SSSS_2 <- cbind.data.frame(mean_h2_data_Oonirici_P3p_SSSS_2,median_h2_data_Oonirici_P3p_SSSS_2,posterior.mode_h2_data_Oonirici_P3p_SSSS_2,HPDinterval_0.95_h2_data_Oonirici_P3p_SSSS_2,HPDinterval_0.83_h2_data_Oonirici_P3p_SSSS_2,effectiveSize_h2_data_Oonirici_P3p_SSSS_2)
        rownames(h2_data_Oonirici_P3p_SSSS_2) <- c("h2_data_Oonirici_CONTROL_P3p_SSSS_mod_2","h2_data_Oonirici_WILD_P3p_SSSS_mod","h2_data_Oonirici_Vg_P3p_SSSS_mod_2")
        h2_data_Oonirici_P3p_SSSS_2 <- cbind(Models = rownames(h2_data_Oonirici_P3p_SSSS_2),h2_data_Oonirici_P3p_SSSS_2)
        rownames(h2_data_Oonirici_P3p_SSSS_2) <- NULL
        h2_data_Oonirici_P3p_SSSS_2$Pnp <- c("P3.p","P3.p","P3.p")
        h2_data_Oonirici_P3p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        h2_data_Oonirici_P3p_SSSS_2$Measure <- c("H2","H2","H2")
        h2_data_Oonirici_P3p_SSSS_2$Scale <- c("data","data","data")
        h2_data_Oonirici_P3p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        h2_data_Oonirici_P3p_SSSS_2
      }
      
      #Summary Evol_data_Oonirici_P3p_SSSS_2
      {
        mean_Evol_data_Oonirici_P3p_SSSS_2 <- rbind(mean(Evol_data_Oonirici_CONTROL_P3p_SSSS_mod_2),mean(Evol_data_Oonirici_WILD_P3p_SSSS_mod),mean(Evol_data_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(mean_Evol_data_Oonirici_P3p_SSSS_2) <- c("mean")
        median_Evol_data_Oonirici_P3p_SSSS_2 <- rbind(median(Evol_data_Oonirici_CONTROL_P3p_SSSS_mod_2),median(Evol_data_Oonirici_WILD_P3p_SSSS_mod),median(Evol_data_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(median_Evol_data_Oonirici_P3p_SSSS_2) <- c("median")
        posterior.mode_Evol_data_Oonirici_P3p_SSSS_2 <- rbind(posterior.mode(as.mcmc(Evol_data_Oonirici_CONTROL_P3p_SSSS_mod_2)),posterior.mode(as.mcmc(Evol_data_Oonirici_WILD_P3p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_Oonirici_Vg_P3p_SSSS_mod_2)))
        colnames(posterior.mode_Evol_data_Oonirici_P3p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Oonirici_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Oonirici_CONTROL_P3p_SSSS_mod_2)),HPDinterval(as.mcmc(Evol_data_Oonirici_WILD_P3p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_Oonirici_Vg_P3p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_Evol_data_Oonirici_P3p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Oonirici_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Oonirici_CONTROL_P3p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(Evol_data_Oonirici_WILD_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Oonirici_Vg_P3p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Oonirici_P3p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Oonirici_P3p_SSSS_2 <- rbind(effectiveSize(Evol_data_Oonirici_CONTROL_P3p_SSSS_mod_2),effectiveSize(Evol_data_Oonirici_WILD_P3p_SSSS_mod),effectiveSize(Evol_data_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(effectiveSize_Evol_data_Oonirici_P3p_SSSS_2) <- c("effectiveSize")
        Evol_data_Oonirici_P3p_SSSS_2 <- cbind.data.frame(mean_Evol_data_Oonirici_P3p_SSSS_2,median_Evol_data_Oonirici_P3p_SSSS_2,posterior.mode_Evol_data_Oonirici_P3p_SSSS_2,HPDinterval_0.95_Evol_data_Oonirici_P3p_SSSS_2,HPDinterval_0.83_Evol_data_Oonirici_P3p_SSSS_2,effectiveSize_Evol_data_Oonirici_P3p_SSSS_2)
        rownames(Evol_data_Oonirici_P3p_SSSS_2) <- c("Evol_data_Oonirici_CONTROL_P3p_SSSS_mod_2","Evol_data_Oonirici_WILD_P3p_SSSS_mod","Evol_data_Oonirici_Vg_P3p_SSSS_mod_2")
        Evol_data_Oonirici_P3p_SSSS_2 <- cbind(Models = rownames(Evol_data_Oonirici_P3p_SSSS_2),Evol_data_Oonirici_P3p_SSSS_2)
        rownames(Evol_data_Oonirici_P3p_SSSS_2) <- NULL
        Evol_data_Oonirici_P3p_SSSS_2$Pnp <- c("P3.p","P3.p","P3.p")
        Evol_data_Oonirici_P3p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        Evol_data_Oonirici_P3p_SSSS_2$Measure <- c("Evol","Evol","Evol")
        Evol_data_Oonirici_P3p_SSSS_2$Scale <- c("data","data","data")
        Evol_data_Oonirici_P3p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        Evol_data_Oonirici_P3p_SSSS_2
      }
      
      #Summary trait_mean_data_Oonirici_P3p_SSSS_2
      {
        mean_trait_mean_data_Oonirici_P3p_SSSS_2 <- rbind(mean(trait_mean_data_Oonirici_CONTROL_P3p_SSSS_mod_2),mean(trait_mean_data_Oonirici_WILD_P3p_SSSS_mod),mean(trait_mean_data_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(mean_trait_mean_data_Oonirici_P3p_SSSS_2) <- c("mean")
        median_trait_mean_data_Oonirici_P3p_SSSS_2 <- rbind(median(trait_mean_data_Oonirici_CONTROL_P3p_SSSS_mod_2),median(trait_mean_data_Oonirici_WILD_P3p_SSSS_mod),median(trait_mean_data_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(median_trait_mean_data_Oonirici_P3p_SSSS_2) <- c("median")
        posterior.mode_trait_mean_data_Oonirici_P3p_SSSS_2 <- rbind(posterior.mode(as.mcmc(trait_mean_data_Oonirici_CONTROL_P3p_SSSS_mod_2)),posterior.mode(as.mcmc(trait_mean_data_Oonirici_WILD_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_Oonirici_Vg_P3p_SSSS_mod_2)))
        colnames(posterior.mode_trait_mean_data_Oonirici_P3p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Oonirici_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Oonirici_CONTROL_P3p_SSSS_mod_2)),HPDinterval(as.mcmc(trait_mean_data_Oonirici_WILD_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_Oonirici_Vg_P3p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_data_Oonirici_P3p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Oonirici_P3p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Oonirici_CONTROL_P3p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Oonirici_WILD_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Oonirici_Vg_P3p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Oonirici_P3p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Oonirici_P3p_SSSS_2 <- rbind(effectiveSize(trait_mean_data_Oonirici_CONTROL_P3p_SSSS_mod_2),effectiveSize(trait_mean_data_Oonirici_WILD_P3p_SSSS_mod),effectiveSize(trait_mean_data_Oonirici_Vg_P3p_SSSS_mod_2))
        colnames(effectiveSize_trait_mean_data_Oonirici_P3p_SSSS_2) <- c("effectiveSize")
        trait_mean_data_Oonirici_P3p_SSSS_2 <- cbind.data.frame(mean_trait_mean_data_Oonirici_P3p_SSSS_2,median_trait_mean_data_Oonirici_P3p_SSSS_2,posterior.mode_trait_mean_data_Oonirici_P3p_SSSS_2,HPDinterval_0.95_trait_mean_data_Oonirici_P3p_SSSS_2,HPDinterval_0.83_trait_mean_data_Oonirici_P3p_SSSS_2,effectiveSize_trait_mean_data_Oonirici_P3p_SSSS_2)
        rownames(trait_mean_data_Oonirici_P3p_SSSS_2) <- c("trait_mean_data_Oonirici_CONTROL_P3p_SSSS_mod_2","trait_mean_data_Oonirici_WILD_P3p_SSSS_mod","trait_mean_data_Oonirici_Vg_P3p_SSSS_mod_2")
        trait_mean_data_Oonirici_P3p_SSSS_2 <- cbind(Models = rownames(trait_mean_data_Oonirici_P3p_SSSS_2),trait_mean_data_Oonirici_P3p_SSSS_2)
        rownames(trait_mean_data_Oonirici_P3p_SSSS_2) <- NULL
        trait_mean_data_Oonirici_P3p_SSSS_2$Pnp <- c("P3.p","P3.p","P3.p")
        trait_mean_data_Oonirici_P3p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_data_Oonirici_P3p_SSSS_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Oonirici_P3p_SSSS_2$Scale <- c("data","data","data")
        trait_mean_data_Oonirici_P3p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_data_Oonirici_P3p_SSSS_2
      }
      
      data_Oonirici_P3p_SSSS_2 <- rbind.data.frame(va_data_Oonirici_P3p_SSSS_2, h2_data_Oonirici_P3p_SSSS_2,Evol_data_Oonirici_P3p_SSSS_2,trait_mean_data_Oonirici_P3p_SSSS_2)
      data_Oonirici_P3p_SSSS_2
      
    }
    Vg_Oonirici_P3p_SSSS_2 <- rbind.data.frame(liab_Oonirici_P3p_SSSS_2, data_Oonirici_P3p_SSSS_2)
    Vg_Oonirici_P3p_SSSS_2$Pnp_fate <- rep("SSSS", 24)
    Vg_Oonirici_P3p_SSSS_2
    #remove Oonirici P3p_SSSS_2 models
    {
      remove(Oonirici_CONTROL_P3p_SSSS_mod_2)
      remove(Oonirici_WILD_P3p_SSSS_mod)
      remove(Oonirici_Vg_P3p_SSSS_mod_2)
    }
  }
  
  ##Summary Oonirici P4p -----
  {
    #Summary liability scale Oonirici P4p
    {
      #Summary va_liab_Oonirici_P4p_SSSS_2: NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        
        mean_va_liab_Oonirici_P4p_SSSS_2 <- rbind(mean(va_liab_Oonirici_CONTROL_P4p_SSSS_mod_2/2),mean(va_liab_Oonirici_WILD_P4p_SSSS_mod/2),mean(va_liab_Oonirici_Vg_P4p_SSSS_mod_2/2))
        colnames(mean_va_liab_Oonirici_P4p_SSSS_2) <- c("mean")
        median_va_liab_Oonirici_P4p_SSSS_2 <- rbind(median(va_liab_Oonirici_CONTROL_P4p_SSSS_mod_2/2),median(va_liab_Oonirici_WILD_P4p_SSSS_mod/2),median(va_liab_Oonirici_Vg_P4p_SSSS_mod_2/2))
        colnames(median_va_liab_Oonirici_P4p_SSSS_2) <- c("median")
        posterior.mode_va_liab_Oonirici_P4p_SSSS_2 <- rbind(posterior.mode(va_liab_Oonirici_CONTROL_P4p_SSSS_mod_2/2),posterior.mode(va_liab_Oonirici_WILD_P4p_SSSS_mod/2),posterior.mode(va_liab_Oonirici_Vg_P4p_SSSS_mod_2/2))
        colnames(posterior.mode_va_liab_Oonirici_P4p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Oonirici_P4p_SSSS_2 <- rbind(HPDinterval(va_liab_Oonirici_CONTROL_P4p_SSSS_mod_2/2),HPDinterval(va_liab_Oonirici_WILD_P4p_SSSS_mod/2),HPDinterval(va_liab_Oonirici_Vg_P4p_SSSS_mod_2/2))
        colnames(HPDinterval_0.95_va_liab_Oonirici_P4p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Oonirici_P4p_SSSS_2 <- rbind(HPDinterval(va_liab_Oonirici_CONTROL_P4p_SSSS_mod_2/2,prob=.83),HPDinterval(va_liab_Oonirici_WILD_P4p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_Oonirici_Vg_P4p_SSSS_mod_2/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Oonirici_P4p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Oonirici_P4p_SSSS_2 <- rbind(effectiveSize(va_liab_Oonirici_CONTROL_P4p_SSSS_mod_2/2),effectiveSize(va_liab_Oonirici_WILD_P4p_SSSS_mod/2),effectiveSize(va_liab_Oonirici_Vg_P4p_SSSS_mod_2/2))
        colnames(effectiveSize_va_liab_Oonirici_P4p_SSSS_2) <- c("effectiveSize")
        va_liab_Oonirici_P4p_SSSS_2 <- cbind.data.frame(mean_va_liab_Oonirici_P4p_SSSS_2,median_va_liab_Oonirici_P4p_SSSS_2,posterior.mode_va_liab_Oonirici_P4p_SSSS_2,HPDinterval_0.95_va_liab_Oonirici_P4p_SSSS_2,HPDinterval_0.83_va_liab_Oonirici_P4p_SSSS_2,effectiveSize_va_liab_Oonirici_P4p_SSSS_2)
        rownames(va_liab_Oonirici_P4p_SSSS_2) <- c("va_liab_Oonirici_CONTROL_P4p_SSSS_mod_2","va_liab_Oonirici_WILD_P4p_SSSS_mod","va_liab_Oonirici_Vg_P4p_SSSS_mod_2")
        va_liab_Oonirici_P4p_SSSS_2 <- cbind(Models = rownames(va_liab_Oonirici_P4p_SSSS_2),va_liab_Oonirici_P4p_SSSS_2)
        rownames(va_liab_Oonirici_P4p_SSSS_2) <- NULL
        va_liab_Oonirici_P4p_SSSS_2$Pnp <- c("P4.p","P4.p","P4.p")
        va_liab_Oonirici_P4p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        va_liab_Oonirici_P4p_SSSS_2$Measure <- c("Va","Va","Va")
        va_liab_Oonirici_P4p_SSSS_2$Scale <- c("liab","liab","liab")
        va_liab_Oonirici_P4p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        va_liab_Oonirici_P4p_SSSS_2
      }
      
      #Summary h2_liab_Oonirici_P4p_SSSS_2
      {
        mean_h2_liab_Oonirici_P4p_SSSS_2 <- rbind(mean(h2_liab_Oonirici_CONTROL_P4p_SSSS_mod_2),mean(h2_liab_Oonirici_WILD_P4p_SSSS_mod),mean(h2_liab_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(mean_h2_liab_Oonirici_P4p_SSSS_2) <- c("mean")
        median_h2_liab_Oonirici_P4p_SSSS_2 <- rbind(median(h2_liab_Oonirici_CONTROL_P4p_SSSS_mod_2),median(h2_liab_Oonirici_WILD_P4p_SSSS_mod),median(h2_liab_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(median_h2_liab_Oonirici_P4p_SSSS_2) <- c("median")
        posterior.mode_h2_liab_Oonirici_P4p_SSSS_2 <- rbind(posterior.mode(h2_liab_Oonirici_CONTROL_P4p_SSSS_mod_2),posterior.mode(h2_liab_Oonirici_WILD_P4p_SSSS_mod),posterior.mode(h2_liab_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(posterior.mode_h2_liab_Oonirici_P4p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Oonirici_P4p_SSSS_2 <- rbind(HPDinterval(h2_liab_Oonirici_CONTROL_P4p_SSSS_mod_2),HPDinterval(h2_liab_Oonirici_WILD_P4p_SSSS_mod),HPDinterval(h2_liab_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(HPDinterval_0.95_h2_liab_Oonirici_P4p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Oonirici_P4p_SSSS_2 <- rbind(HPDinterval(h2_liab_Oonirici_CONTROL_P4p_SSSS_mod_2,prob=.83),HPDinterval(h2_liab_Oonirici_WILD_P4p_SSSS_mod,prob=.83),HPDinterval(h2_liab_Oonirici_Vg_P4p_SSSS_mod_2,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Oonirici_P4p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Oonirici_P4p_SSSS_2 <- rbind(effectiveSize(h2_liab_Oonirici_CONTROL_P4p_SSSS_mod_2),effectiveSize(h2_liab_Oonirici_WILD_P4p_SSSS_mod),effectiveSize(h2_liab_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(effectiveSize_h2_liab_Oonirici_P4p_SSSS_2) <- c("effectiveSize")
        h2_liab_Oonirici_P4p_SSSS_2 <- cbind.data.frame(mean_h2_liab_Oonirici_P4p_SSSS_2,median_h2_liab_Oonirici_P4p_SSSS_2,posterior.mode_h2_liab_Oonirici_P4p_SSSS_2,HPDinterval_0.95_h2_liab_Oonirici_P4p_SSSS_2,HPDinterval_0.83_h2_liab_Oonirici_P4p_SSSS_2,effectiveSize_h2_liab_Oonirici_P4p_SSSS_2)
        rownames(h2_liab_Oonirici_P4p_SSSS_2) <- c("h2_liab_Oonirici_CONTROL_P4p_SSSS_mod_2","h2_liab_Oonirici_WILD_P4p_SSSS_mod","h2_liab_Oonirici_Vg_P4p_SSSS_mod_2")
        h2_liab_Oonirici_P4p_SSSS_2 <- cbind(Models = rownames(h2_liab_Oonirici_P4p_SSSS_2),h2_liab_Oonirici_P4p_SSSS_2)
        rownames(h2_liab_Oonirici_P4p_SSSS_2) <- NULL
        h2_liab_Oonirici_P4p_SSSS_2$Pnp <- c("P4.p","P4.p","P4.p")
        h2_liab_Oonirici_P4p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        h2_liab_Oonirici_P4p_SSSS_2$Measure <- c("H2","H2","H2")
        h2_liab_Oonirici_P4p_SSSS_2$Scale <- c("liab","liab","liab")
        h2_liab_Oonirici_P4p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        h2_liab_Oonirici_P4p_SSSS_2
      }
      
      #Summary Evol_liab_Oonirici_P4p_SSSS_2
      {
        mean_Evol_liab_Oonirici_P4p_SSSS_2 <- rbind(mean(Evol_liab_Oonirici_CONTROL_P4p_SSSS_mod_2),mean(Evol_liab_Oonirici_WILD_P4p_SSSS_mod),mean(Evol_liab_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(mean_Evol_liab_Oonirici_P4p_SSSS_2) <- c("mean")
        median_Evol_liab_Oonirici_P4p_SSSS_2 <- rbind(median(Evol_liab_Oonirici_CONTROL_P4p_SSSS_mod_2),median(Evol_liab_Oonirici_WILD_P4p_SSSS_mod),median(Evol_liab_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(median_Evol_liab_Oonirici_P4p_SSSS_2) <- c("median")
        posterior.mode_Evol_liab_Oonirici_P4p_SSSS_2 <- rbind(posterior.mode(Evol_liab_Oonirici_CONTROL_P4p_SSSS_mod_2),posterior.mode(Evol_liab_Oonirici_WILD_P4p_SSSS_mod),posterior.mode(Evol_liab_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(posterior.mode_Evol_liab_Oonirici_P4p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Oonirici_P4p_SSSS_2 <- rbind(HPDinterval(Evol_liab_Oonirici_CONTROL_P4p_SSSS_mod_2),HPDinterval(Evol_liab_Oonirici_WILD_P4p_SSSS_mod),HPDinterval(Evol_liab_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(HPDinterval_0.95_Evol_liab_Oonirici_P4p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Oonirici_P4p_SSSS_2 <- rbind(HPDinterval(Evol_liab_Oonirici_CONTROL_P4p_SSSS_mod_2,prob=.83),HPDinterval(Evol_liab_Oonirici_WILD_P4p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_Oonirici_Vg_P4p_SSSS_mod_2,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Oonirici_P4p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Oonirici_P4p_SSSS_2 <- rbind(effectiveSize(Evol_liab_Oonirici_CONTROL_P4p_SSSS_mod_2),effectiveSize(Evol_liab_Oonirici_WILD_P4p_SSSS_mod),effectiveSize(Evol_liab_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(effectiveSize_Evol_liab_Oonirici_P4p_SSSS_2) <- c("effectiveSize")
        Evol_liab_Oonirici_P4p_SSSS_2 <- cbind.data.frame(mean_Evol_liab_Oonirici_P4p_SSSS_2,median_Evol_liab_Oonirici_P4p_SSSS_2,posterior.mode_Evol_liab_Oonirici_P4p_SSSS_2,HPDinterval_0.95_Evol_liab_Oonirici_P4p_SSSS_2,HPDinterval_0.83_Evol_liab_Oonirici_P4p_SSSS_2,effectiveSize_Evol_liab_Oonirici_P4p_SSSS_2)
        rownames(Evol_liab_Oonirici_P4p_SSSS_2) <- c("Evol_liab_Oonirici_CONTROL_P4p_SSSS_mod_2","Evol_liab_Oonirici_WILD_P4p_SSSS_mod","Evol_liab_Oonirici_Vg_P4p_SSSS_mod_2")
        Evol_liab_Oonirici_P4p_SSSS_2 <- cbind(Models = rownames(Evol_liab_Oonirici_P4p_SSSS_2),Evol_liab_Oonirici_P4p_SSSS_2)
        rownames(Evol_liab_Oonirici_P4p_SSSS_2) <- NULL
        Evol_liab_Oonirici_P4p_SSSS_2$Pnp <- c("P4.p","P4.p","P4.p")
        Evol_liab_Oonirici_P4p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        Evol_liab_Oonirici_P4p_SSSS_2$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Oonirici_P4p_SSSS_2$Scale <- c("liab","liab","liab")
        Evol_liab_Oonirici_P4p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        Evol_liab_Oonirici_P4p_SSSS_2
      }
      
      #Summary trait_mean_liab_Oonirici_P4p_SSSS_2
      {
        mean_trait_mean_liab_Oonirici_P4p_SSSS_2 <- rbind(mean(trait_mean_liab_Oonirici_CONTROL_P4p_SSSS_mod_2),mean(trait_mean_liab_Oonirici_WILD_P4p_SSSS_mod),mean(trait_mean_liab_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(mean_trait_mean_liab_Oonirici_P4p_SSSS_2) <- c("mean")
        median_trait_mean_liab_Oonirici_P4p_SSSS_2 <- rbind(median(trait_mean_liab_Oonirici_CONTROL_P4p_SSSS_mod_2),median(trait_mean_liab_Oonirici_WILD_P4p_SSSS_mod),median(trait_mean_liab_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(median_trait_mean_liab_Oonirici_P4p_SSSS_2) <- c("median")
        posterior.mode_trait_mean_liab_Oonirici_P4p_SSSS_2 <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P4p_SSSS_mod_2)),posterior.mode(as.mcmc(trait_mean_liab_Oonirici_WILD_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Oonirici_Vg_P4p_SSSS_mod_2)))
        colnames(posterior.mode_trait_mean_liab_Oonirici_P4p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Oonirici_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P4p_SSSS_mod_2)),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_WILD_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_Vg_P4p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_liab_Oonirici_P4p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Oonirici_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P4p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_WILD_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_Vg_P4p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Oonirici_P4p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Oonirici_P4p_SSSS_2 <- rbind(effectiveSize(trait_mean_liab_Oonirici_CONTROL_P4p_SSSS_mod_2),effectiveSize(trait_mean_liab_Oonirici_WILD_P4p_SSSS_mod),effectiveSize(trait_mean_liab_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(effectiveSize_trait_mean_liab_Oonirici_P4p_SSSS_2) <- c("effectiveSize")
        trait_mean_liab_Oonirici_P4p_SSSS_2 <- cbind.data.frame(mean_trait_mean_liab_Oonirici_P4p_SSSS_2,median_trait_mean_liab_Oonirici_P4p_SSSS_2,posterior.mode_trait_mean_liab_Oonirici_P4p_SSSS_2,HPDinterval_0.95_trait_mean_liab_Oonirici_P4p_SSSS_2,HPDinterval_0.83_trait_mean_liab_Oonirici_P4p_SSSS_2,effectiveSize_trait_mean_liab_Oonirici_P4p_SSSS_2)
        rownames(trait_mean_liab_Oonirici_P4p_SSSS_2) <- c("trait_mean_liab_Oonirici_CONTROL_P4p_SSSS_mod_2","trait_mean_liab_Oonirici_WILD_P4p_SSSS_mod","trait_mean_liab_Oonirici_Vg_P4p_SSSS_mod_2")
        trait_mean_liab_Oonirici_P4p_SSSS_2 <- cbind(Models = rownames(trait_mean_liab_Oonirici_P4p_SSSS_2),trait_mean_liab_Oonirici_P4p_SSSS_2)
        rownames(trait_mean_liab_Oonirici_P4p_SSSS_2) <- NULL
        trait_mean_liab_Oonirici_P4p_SSSS_2$Pnp <- c("P4.p","P4.p","P4.p")
        trait_mean_liab_Oonirici_P4p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_liab_Oonirici_P4p_SSSS_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Oonirici_P4p_SSSS_2$Scale <- c("liab","liab","liab")
        trait_mean_liab_Oonirici_P4p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_liab_Oonirici_P4p_SSSS_2
      }
      
      liab_Oonirici_P4p_SSSS_2 <- rbind.data.frame(va_liab_Oonirici_P4p_SSSS_2, h2_liab_Oonirici_P4p_SSSS_2,Evol_liab_Oonirici_P4p_SSSS_2,trait_mean_liab_Oonirici_P4p_SSSS_2)
      liab_Oonirici_P4p_SSSS_2
    }
    #Summary data scale Oonirici P4p
    {
      #Summary va_data_Oonirici_P4p_SSSS_2:NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        mean_va_data_Oonirici_P4p_SSSS_2 <- rbind(mean(va_data_Oonirici_CONTROL_P4p_SSSS_mod_2/2),mean(va_data_Oonirici_WILD_P4p_SSSS_mod/2),mean(va_data_Oonirici_Vg_P4p_SSSS_mod_2/2))
        colnames(mean_va_data_Oonirici_P4p_SSSS_2) <- c("mean")
        median_va_data_Oonirici_P4p_SSSS_2 <- rbind(median(va_data_Oonirici_CONTROL_P4p_SSSS_mod_2/2),median(va_data_Oonirici_WILD_P4p_SSSS_mod/2),median(va_data_Oonirici_Vg_P4p_SSSS_mod_2/2))
        colnames(median_va_data_Oonirici_P4p_SSSS_2) <- c("median")
        posterior.mode_va_data_Oonirici_P4p_SSSS_2 <- rbind(posterior.mode(as.mcmc(va_data_Oonirici_CONTROL_P4p_SSSS_mod_2/2)),posterior.mode(as.mcmc(va_data_Oonirici_WILD_P4p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_Oonirici_Vg_P4p_SSSS_mod_2/2)))
        colnames(posterior.mode_va_data_Oonirici_P4p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Oonirici_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(va_data_Oonirici_CONTROL_P4p_SSSS_mod_2/2)),HPDinterval(as.mcmc(va_data_Oonirici_WILD_P4p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_Oonirici_Vg_P4p_SSSS_mod_2/2)))
        colnames(HPDinterval_0.95_va_data_Oonirici_P4p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Oonirici_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(va_data_Oonirici_CONTROL_P4p_SSSS_mod_2/2),prob=.83),HPDinterval(as.mcmc(va_data_Oonirici_WILD_P4p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Oonirici_Vg_P4p_SSSS_mod_2/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Oonirici_P4p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Oonirici_P4p_SSSS_2 <- rbind(effectiveSize(va_data_Oonirici_CONTROL_P4p_SSSS_mod_2/2),effectiveSize(va_data_Oonirici_WILD_P4p_SSSS_mod/2),effectiveSize(va_data_Oonirici_Vg_P4p_SSSS_mod_2/2))
        colnames(effectiveSize_va_data_Oonirici_P4p_SSSS_2) <- c("effectiveSize")
        va_data_Oonirici_P4p_SSSS_2 <- cbind.data.frame(mean_va_data_Oonirici_P4p_SSSS_2,median_va_data_Oonirici_P4p_SSSS_2,posterior.mode_va_data_Oonirici_P4p_SSSS_2,HPDinterval_0.95_va_data_Oonirici_P4p_SSSS_2,HPDinterval_0.83_va_data_Oonirici_P4p_SSSS_2,effectiveSize_va_data_Oonirici_P4p_SSSS_2)
        rownames(va_data_Oonirici_P4p_SSSS_2) <- c("va_data_Oonirici_CONTROL_P4p_SSSS_mod_2","va_data_Oonirici_WILD_P4p_SSSS_mod","va_data_Oonirici_Vg_P4p_SSSS_mod_2")
        va_data_Oonirici_P4p_SSSS_2 <- cbind(Models = rownames(va_data_Oonirici_P4p_SSSS_2),va_data_Oonirici_P4p_SSSS_2)
        rownames(va_data_Oonirici_P4p_SSSS_2) <- NULL
        va_data_Oonirici_P4p_SSSS_2$Pnp <- c("P4.p","P4.p","P4.p")
        va_data_Oonirici_P4p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        va_data_Oonirici_P4p_SSSS_2$Measure <- c("Va","Va","Va")
        va_data_Oonirici_P4p_SSSS_2$Scale <- c("data","data","data")
        va_data_Oonirici_P4p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        va_data_Oonirici_P4p_SSSS_2
      }
      
      #Summary h2_data_Oonirici_P4p_SSSS_2
      {
        mean_h2_data_Oonirici_P4p_SSSS_2 <- rbind(mean(h2_data_Oonirici_CONTROL_P4p_SSSS_mod_2),mean(h2_data_Oonirici_WILD_P4p_SSSS_mod),mean(h2_data_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(mean_h2_data_Oonirici_P4p_SSSS_2) <- c("mean")
        median_h2_data_Oonirici_P4p_SSSS_2 <- rbind(median(h2_data_Oonirici_CONTROL_P4p_SSSS_mod_2),median(h2_data_Oonirici_WILD_P4p_SSSS_mod),median(h2_data_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(median_h2_data_Oonirici_P4p_SSSS_2) <- c("median")
        posterior.mode_h2_data_Oonirici_P4p_SSSS_2 <- rbind(posterior.mode(as.mcmc(h2_data_Oonirici_CONTROL_P4p_SSSS_mod_2)),posterior.mode(as.mcmc(h2_data_Oonirici_WILD_P4p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_Oonirici_Vg_P4p_SSSS_mod_2)))
        colnames(posterior.mode_h2_data_Oonirici_P4p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Oonirici_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(h2_data_Oonirici_CONTROL_P4p_SSSS_mod_2)),HPDinterval(as.mcmc(h2_data_Oonirici_WILD_P4p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_Oonirici_Vg_P4p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_h2_data_Oonirici_P4p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Oonirici_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(h2_data_Oonirici_CONTROL_P4p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(h2_data_Oonirici_WILD_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Oonirici_Vg_P4p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Oonirici_P4p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Oonirici_P4p_SSSS_2 <- rbind(effectiveSize(h2_data_Oonirici_CONTROL_P4p_SSSS_mod_2),effectiveSize(h2_data_Oonirici_WILD_P4p_SSSS_mod),effectiveSize(h2_data_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(effectiveSize_h2_data_Oonirici_P4p_SSSS_2) <- c("effectiveSize")
        h2_data_Oonirici_P4p_SSSS_2 <- cbind.data.frame(mean_h2_data_Oonirici_P4p_SSSS_2,median_h2_data_Oonirici_P4p_SSSS_2,posterior.mode_h2_data_Oonirici_P4p_SSSS_2,HPDinterval_0.95_h2_data_Oonirici_P4p_SSSS_2,HPDinterval_0.83_h2_data_Oonirici_P4p_SSSS_2,effectiveSize_h2_data_Oonirici_P4p_SSSS_2)
        rownames(h2_data_Oonirici_P4p_SSSS_2) <- c("h2_data_Oonirici_CONTROL_P4p_SSSS_mod_2","h2_data_Oonirici_WILD_P4p_SSSS_mod","h2_data_Oonirici_Vg_P4p_SSSS_mod_2")
        h2_data_Oonirici_P4p_SSSS_2 <- cbind(Models = rownames(h2_data_Oonirici_P4p_SSSS_2),h2_data_Oonirici_P4p_SSSS_2)
        rownames(h2_data_Oonirici_P4p_SSSS_2) <- NULL
        h2_data_Oonirici_P4p_SSSS_2$Pnp <- c("P4.p","P4.p","P4.p")
        h2_data_Oonirici_P4p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        h2_data_Oonirici_P4p_SSSS_2$Measure <- c("H2","H2","H2")
        h2_data_Oonirici_P4p_SSSS_2$Scale <- c("data","data","data")
        h2_data_Oonirici_P4p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        h2_data_Oonirici_P4p_SSSS_2
      }
      
      #Summary Evol_data_Oonirici_P4p_SSSS_2
      {
        mean_Evol_data_Oonirici_P4p_SSSS_2 <- rbind(mean(Evol_data_Oonirici_CONTROL_P4p_SSSS_mod_2),mean(Evol_data_Oonirici_WILD_P4p_SSSS_mod),mean(Evol_data_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(mean_Evol_data_Oonirici_P4p_SSSS_2) <- c("mean")
        median_Evol_data_Oonirici_P4p_SSSS_2 <- rbind(median(Evol_data_Oonirici_CONTROL_P4p_SSSS_mod_2),median(Evol_data_Oonirici_WILD_P4p_SSSS_mod),median(Evol_data_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(median_Evol_data_Oonirici_P4p_SSSS_2) <- c("median")
        posterior.mode_Evol_data_Oonirici_P4p_SSSS_2 <- rbind(posterior.mode(as.mcmc(Evol_data_Oonirici_CONTROL_P4p_SSSS_mod_2)),posterior.mode(as.mcmc(Evol_data_Oonirici_WILD_P4p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_Oonirici_Vg_P4p_SSSS_mod_2)))
        colnames(posterior.mode_Evol_data_Oonirici_P4p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Oonirici_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Oonirici_CONTROL_P4p_SSSS_mod_2)),HPDinterval(as.mcmc(Evol_data_Oonirici_WILD_P4p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_Oonirici_Vg_P4p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_Evol_data_Oonirici_P4p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Oonirici_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Oonirici_CONTROL_P4p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(Evol_data_Oonirici_WILD_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Oonirici_Vg_P4p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Oonirici_P4p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Oonirici_P4p_SSSS_2 <- rbind(effectiveSize(Evol_data_Oonirici_CONTROL_P4p_SSSS_mod_2),effectiveSize(Evol_data_Oonirici_WILD_P4p_SSSS_mod),effectiveSize(Evol_data_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(effectiveSize_Evol_data_Oonirici_P4p_SSSS_2) <- c("effectiveSize")
        Evol_data_Oonirici_P4p_SSSS_2 <- cbind.data.frame(mean_Evol_data_Oonirici_P4p_SSSS_2,median_Evol_data_Oonirici_P4p_SSSS_2,posterior.mode_Evol_data_Oonirici_P4p_SSSS_2,HPDinterval_0.95_Evol_data_Oonirici_P4p_SSSS_2,HPDinterval_0.83_Evol_data_Oonirici_P4p_SSSS_2,effectiveSize_Evol_data_Oonirici_P4p_SSSS_2)
        rownames(Evol_data_Oonirici_P4p_SSSS_2) <- c("Evol_data_Oonirici_CONTROL_P4p_SSSS_mod_2","Evol_data_Oonirici_WILD_P4p_SSSS_mod","Evol_data_Oonirici_Vg_P4p_SSSS_mod_2")
        Evol_data_Oonirici_P4p_SSSS_2 <- cbind(Models = rownames(Evol_data_Oonirici_P4p_SSSS_2),Evol_data_Oonirici_P4p_SSSS_2)
        rownames(Evol_data_Oonirici_P4p_SSSS_2) <- NULL
        Evol_data_Oonirici_P4p_SSSS_2$Pnp <- c("P4.p","P4.p","P4.p")
        Evol_data_Oonirici_P4p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        Evol_data_Oonirici_P4p_SSSS_2$Measure <- c("Evol","Evol","Evol")
        Evol_data_Oonirici_P4p_SSSS_2$Scale <- c("data","data","data")
        Evol_data_Oonirici_P4p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        Evol_data_Oonirici_P4p_SSSS_2
      }
      
      #Summary trait_mean_data_Oonirici_P4p_SSSS_2
      {
        mean_trait_mean_data_Oonirici_P4p_SSSS_2 <- rbind(mean(trait_mean_data_Oonirici_CONTROL_P4p_SSSS_mod_2),mean(trait_mean_data_Oonirici_WILD_P4p_SSSS_mod),mean(trait_mean_data_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(mean_trait_mean_data_Oonirici_P4p_SSSS_2) <- c("mean")
        median_trait_mean_data_Oonirici_P4p_SSSS_2 <- rbind(median(trait_mean_data_Oonirici_CONTROL_P4p_SSSS_mod_2),median(trait_mean_data_Oonirici_WILD_P4p_SSSS_mod),median(trait_mean_data_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(median_trait_mean_data_Oonirici_P4p_SSSS_2) <- c("median")
        posterior.mode_trait_mean_data_Oonirici_P4p_SSSS_2 <- rbind(posterior.mode(as.mcmc(trait_mean_data_Oonirici_CONTROL_P4p_SSSS_mod_2)),posterior.mode(as.mcmc(trait_mean_data_Oonirici_WILD_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_Oonirici_Vg_P4p_SSSS_mod_2)))
        colnames(posterior.mode_trait_mean_data_Oonirici_P4p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Oonirici_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Oonirici_CONTROL_P4p_SSSS_mod_2)),HPDinterval(as.mcmc(trait_mean_data_Oonirici_WILD_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_Oonirici_Vg_P4p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_data_Oonirici_P4p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Oonirici_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Oonirici_CONTROL_P4p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Oonirici_WILD_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Oonirici_Vg_P4p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Oonirici_P4p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Oonirici_P4p_SSSS_2 <- rbind(effectiveSize(trait_mean_data_Oonirici_CONTROL_P4p_SSSS_mod_2),effectiveSize(trait_mean_data_Oonirici_WILD_P4p_SSSS_mod),effectiveSize(trait_mean_data_Oonirici_Vg_P4p_SSSS_mod_2))
        colnames(effectiveSize_trait_mean_data_Oonirici_P4p_SSSS_2) <- c("effectiveSize")
        trait_mean_data_Oonirici_P4p_SSSS_2 <- cbind.data.frame(mean_trait_mean_data_Oonirici_P4p_SSSS_2,median_trait_mean_data_Oonirici_P4p_SSSS_2,posterior.mode_trait_mean_data_Oonirici_P4p_SSSS_2,HPDinterval_0.95_trait_mean_data_Oonirici_P4p_SSSS_2,HPDinterval_0.83_trait_mean_data_Oonirici_P4p_SSSS_2,effectiveSize_trait_mean_data_Oonirici_P4p_SSSS_2)
        rownames(trait_mean_data_Oonirici_P4p_SSSS_2) <- c("trait_mean_data_Oonirici_CONTROL_P4p_SSSS_mod_2","trait_mean_data_Oonirici_WILD_P4p_SSSS_mod","trait_mean_data_Oonirici_Vg_P4p_SSSS_mod_2")
        trait_mean_data_Oonirici_P4p_SSSS_2 <- cbind(Models = rownames(trait_mean_data_Oonirici_P4p_SSSS_2),trait_mean_data_Oonirici_P4p_SSSS_2)
        rownames(trait_mean_data_Oonirici_P4p_SSSS_2) <- NULL
        trait_mean_data_Oonirici_P4p_SSSS_2$Pnp <- c("P4.p","P4.p","P4.p")
        trait_mean_data_Oonirici_P4p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_data_Oonirici_P4p_SSSS_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Oonirici_P4p_SSSS_2$Scale <- c("data","data","data")
        trait_mean_data_Oonirici_P4p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_data_Oonirici_P4p_SSSS_2
      }
      
      data_Oonirici_P4p_SSSS_2 <- rbind.data.frame(va_data_Oonirici_P4p_SSSS_2, h2_data_Oonirici_P4p_SSSS_2,Evol_data_Oonirici_P4p_SSSS_2,trait_mean_data_Oonirici_P4p_SSSS_2)
      data_Oonirici_P4p_SSSS_2
      
    }
    Vg_Oonirici_P4p_SSSS_2 <- rbind.data.frame(liab_Oonirici_P4p_SSSS_2, data_Oonirici_P4p_SSSS_2)
    Vg_Oonirici_P4p_SSSS_2$Pnp_fate <- rep("SSSS", 24)
    Vg_Oonirici_P4p_SSSS_2
    #remove Oonirici P4p_SSSS_2 models
    {
      remove(Oonirici_CONTROL_P4p_SSSS_mod_2)
      remove(Oonirici_WILD_P4p_SSSS_mod)
      remove(Oonirici_Vg_P4p_SSSS_mod_2)
    }
  }
  
  ##Summary Oonirici P8p ----
  {
    #Summary liability scale Oonirici P8p
    {
      #Summary va_liab_Oonirici_P8p_SSSS_2: NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        
        mean_va_liab_Oonirici_P8p_SSSS_2 <- rbind(mean(va_liab_Oonirici_CONTROL_P8p_SSSS_mod_2/2),mean(va_liab_Oonirici_WILD_P8p_SSSS_mod/2),mean(va_liab_Oonirici_Vg_P8p_SSSS_mod_2/2))
        colnames(mean_va_liab_Oonirici_P8p_SSSS_2) <- c("mean")
        median_va_liab_Oonirici_P8p_SSSS_2 <- rbind(median(va_liab_Oonirici_CONTROL_P8p_SSSS_mod_2/2),median(va_liab_Oonirici_WILD_P8p_SSSS_mod/2),median(va_liab_Oonirici_Vg_P8p_SSSS_mod_2/2))
        colnames(median_va_liab_Oonirici_P8p_SSSS_2) <- c("median")
        posterior.mode_va_liab_Oonirici_P8p_SSSS_2 <- rbind(posterior.mode(va_liab_Oonirici_CONTROL_P8p_SSSS_mod_2/2),posterior.mode(va_liab_Oonirici_WILD_P8p_SSSS_mod/2),posterior.mode(va_liab_Oonirici_Vg_P8p_SSSS_mod_2/2))
        colnames(posterior.mode_va_liab_Oonirici_P8p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Oonirici_P8p_SSSS_2 <- rbind(HPDinterval(va_liab_Oonirici_CONTROL_P8p_SSSS_mod_2/2),HPDinterval(va_liab_Oonirici_WILD_P8p_SSSS_mod/2),HPDinterval(va_liab_Oonirici_Vg_P8p_SSSS_mod_2/2))
        colnames(HPDinterval_0.95_va_liab_Oonirici_P8p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Oonirici_P8p_SSSS_2 <- rbind(HPDinterval(va_liab_Oonirici_CONTROL_P8p_SSSS_mod_2/2,prob=.83),HPDinterval(va_liab_Oonirici_WILD_P8p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_Oonirici_Vg_P8p_SSSS_mod_2/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Oonirici_P8p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Oonirici_P8p_SSSS_2 <- rbind(effectiveSize(va_liab_Oonirici_CONTROL_P8p_SSSS_mod_2/2),effectiveSize(va_liab_Oonirici_WILD_P8p_SSSS_mod/2),effectiveSize(va_liab_Oonirici_Vg_P8p_SSSS_mod_2/2))
        colnames(effectiveSize_va_liab_Oonirici_P8p_SSSS_2) <- c("effectiveSize")
        va_liab_Oonirici_P8p_SSSS_2 <- cbind.data.frame(mean_va_liab_Oonirici_P8p_SSSS_2,median_va_liab_Oonirici_P8p_SSSS_2,posterior.mode_va_liab_Oonirici_P8p_SSSS_2,HPDinterval_0.95_va_liab_Oonirici_P8p_SSSS_2,HPDinterval_0.83_va_liab_Oonirici_P8p_SSSS_2,effectiveSize_va_liab_Oonirici_P8p_SSSS_2)
        rownames(va_liab_Oonirici_P8p_SSSS_2) <- c("va_liab_Oonirici_CONTROL_P8p_SSSS_mod_2","va_liab_Oonirici_WILD_P8p_SSSS_mod","va_liab_Oonirici_Vg_P8p_SSSS_mod_2")
        va_liab_Oonirici_P8p_SSSS_2 <- cbind(Models = rownames(va_liab_Oonirici_P8p_SSSS_2),va_liab_Oonirici_P8p_SSSS_2)
        rownames(va_liab_Oonirici_P8p_SSSS_2) <- NULL
        va_liab_Oonirici_P8p_SSSS_2$Pnp <- c("P8.p","P8.p","P8.p")
        va_liab_Oonirici_P8p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        va_liab_Oonirici_P8p_SSSS_2$Measure <- c("Va","Va","Va")
        va_liab_Oonirici_P8p_SSSS_2$Scale <- c("liab","liab","liab")
        va_liab_Oonirici_P8p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        va_liab_Oonirici_P8p_SSSS_2
      }
      
      #Summary h2_liab_Oonirici_P8p_SSSS_2
      {
        mean_h2_liab_Oonirici_P8p_SSSS_2 <- rbind(mean(h2_liab_Oonirici_CONTROL_P8p_SSSS_mod_2),mean(h2_liab_Oonirici_WILD_P8p_SSSS_mod),mean(h2_liab_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(mean_h2_liab_Oonirici_P8p_SSSS_2) <- c("mean")
        median_h2_liab_Oonirici_P8p_SSSS_2 <- rbind(median(h2_liab_Oonirici_CONTROL_P8p_SSSS_mod_2),median(h2_liab_Oonirici_WILD_P8p_SSSS_mod),median(h2_liab_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(median_h2_liab_Oonirici_P8p_SSSS_2) <- c("median")
        posterior.mode_h2_liab_Oonirici_P8p_SSSS_2 <- rbind(posterior.mode(h2_liab_Oonirici_CONTROL_P8p_SSSS_mod_2),posterior.mode(h2_liab_Oonirici_WILD_P8p_SSSS_mod),posterior.mode(h2_liab_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(posterior.mode_h2_liab_Oonirici_P8p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Oonirici_P8p_SSSS_2 <- rbind(HPDinterval(h2_liab_Oonirici_CONTROL_P8p_SSSS_mod_2),HPDinterval(h2_liab_Oonirici_WILD_P8p_SSSS_mod),HPDinterval(h2_liab_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(HPDinterval_0.95_h2_liab_Oonirici_P8p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Oonirici_P8p_SSSS_2 <- rbind(HPDinterval(h2_liab_Oonirici_CONTROL_P8p_SSSS_mod_2,prob=.83),HPDinterval(h2_liab_Oonirici_WILD_P8p_SSSS_mod,prob=.83),HPDinterval(h2_liab_Oonirici_Vg_P8p_SSSS_mod_2,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Oonirici_P8p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Oonirici_P8p_SSSS_2 <- rbind(effectiveSize(h2_liab_Oonirici_CONTROL_P8p_SSSS_mod_2),effectiveSize(h2_liab_Oonirici_WILD_P8p_SSSS_mod),effectiveSize(h2_liab_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(effectiveSize_h2_liab_Oonirici_P8p_SSSS_2) <- c("effectiveSize")
        h2_liab_Oonirici_P8p_SSSS_2 <- cbind.data.frame(mean_h2_liab_Oonirici_P8p_SSSS_2,median_h2_liab_Oonirici_P8p_SSSS_2,posterior.mode_h2_liab_Oonirici_P8p_SSSS_2,HPDinterval_0.95_h2_liab_Oonirici_P8p_SSSS_2,HPDinterval_0.83_h2_liab_Oonirici_P8p_SSSS_2,effectiveSize_h2_liab_Oonirici_P8p_SSSS_2)
        rownames(h2_liab_Oonirici_P8p_SSSS_2) <- c("h2_liab_Oonirici_CONTROL_P8p_SSSS_mod_2","h2_liab_Oonirici_WILD_P8p_SSSS_mod","h2_liab_Oonirici_Vg_P8p_SSSS_mod_2")
        h2_liab_Oonirici_P8p_SSSS_2 <- cbind(Models = rownames(h2_liab_Oonirici_P8p_SSSS_2),h2_liab_Oonirici_P8p_SSSS_2)
        rownames(h2_liab_Oonirici_P8p_SSSS_2) <- NULL
        h2_liab_Oonirici_P8p_SSSS_2$Pnp <- c("P8.p","P8.p","P8.p")
        h2_liab_Oonirici_P8p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        h2_liab_Oonirici_P8p_SSSS_2$Measure <- c("H2","H2","H2")
        h2_liab_Oonirici_P8p_SSSS_2$Scale <- c("liab","liab","liab")
        h2_liab_Oonirici_P8p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        h2_liab_Oonirici_P8p_SSSS_2
      }
      
      #Summary Evol_liab_Oonirici_P8p_SSSS_2
      {
        mean_Evol_liab_Oonirici_P8p_SSSS_2 <- rbind(mean(Evol_liab_Oonirici_CONTROL_P8p_SSSS_mod_2),mean(Evol_liab_Oonirici_WILD_P8p_SSSS_mod),mean(Evol_liab_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(mean_Evol_liab_Oonirici_P8p_SSSS_2) <- c("mean")
        median_Evol_liab_Oonirici_P8p_SSSS_2 <- rbind(median(Evol_liab_Oonirici_CONTROL_P8p_SSSS_mod_2),median(Evol_liab_Oonirici_WILD_P8p_SSSS_mod),median(Evol_liab_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(median_Evol_liab_Oonirici_P8p_SSSS_2) <- c("median")
        posterior.mode_Evol_liab_Oonirici_P8p_SSSS_2 <- rbind(posterior.mode(Evol_liab_Oonirici_CONTROL_P8p_SSSS_mod_2),posterior.mode(Evol_liab_Oonirici_WILD_P8p_SSSS_mod),posterior.mode(Evol_liab_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(posterior.mode_Evol_liab_Oonirici_P8p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Oonirici_P8p_SSSS_2 <- rbind(HPDinterval(Evol_liab_Oonirici_CONTROL_P8p_SSSS_mod_2),HPDinterval(Evol_liab_Oonirici_WILD_P8p_SSSS_mod),HPDinterval(Evol_liab_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(HPDinterval_0.95_Evol_liab_Oonirici_P8p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Oonirici_P8p_SSSS_2 <- rbind(HPDinterval(Evol_liab_Oonirici_CONTROL_P8p_SSSS_mod_2,prob=.83),HPDinterval(Evol_liab_Oonirici_WILD_P8p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_Oonirici_Vg_P8p_SSSS_mod_2,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Oonirici_P8p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Oonirici_P8p_SSSS_2 <- rbind(effectiveSize(Evol_liab_Oonirici_CONTROL_P8p_SSSS_mod_2),effectiveSize(Evol_liab_Oonirici_WILD_P8p_SSSS_mod),effectiveSize(Evol_liab_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(effectiveSize_Evol_liab_Oonirici_P8p_SSSS_2) <- c("effectiveSize")
        Evol_liab_Oonirici_P8p_SSSS_2 <- cbind.data.frame(mean_Evol_liab_Oonirici_P8p_SSSS_2,median_Evol_liab_Oonirici_P8p_SSSS_2,posterior.mode_Evol_liab_Oonirici_P8p_SSSS_2,HPDinterval_0.95_Evol_liab_Oonirici_P8p_SSSS_2,HPDinterval_0.83_Evol_liab_Oonirici_P8p_SSSS_2,effectiveSize_Evol_liab_Oonirici_P8p_SSSS_2)
        rownames(Evol_liab_Oonirici_P8p_SSSS_2) <- c("Evol_liab_Oonirici_CONTROL_P8p_SSSS_mod_2","Evol_liab_Oonirici_WILD_P8p_SSSS_mod","Evol_liab_Oonirici_Vg_P8p_SSSS_mod_2")
        Evol_liab_Oonirici_P8p_SSSS_2 <- cbind(Models = rownames(Evol_liab_Oonirici_P8p_SSSS_2),Evol_liab_Oonirici_P8p_SSSS_2)
        rownames(Evol_liab_Oonirici_P8p_SSSS_2) <- NULL
        Evol_liab_Oonirici_P8p_SSSS_2$Pnp <- c("P8.p","P8.p","P8.p")
        Evol_liab_Oonirici_P8p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        Evol_liab_Oonirici_P8p_SSSS_2$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Oonirici_P8p_SSSS_2$Scale <- c("liab","liab","liab")
        Evol_liab_Oonirici_P8p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        Evol_liab_Oonirici_P8p_SSSS_2
      }
      
      #Summary trait_mean_liab_Oonirici_P8p_SSSS_2
      {
        mean_trait_mean_liab_Oonirici_P8p_SSSS_2 <- rbind(mean(trait_mean_liab_Oonirici_CONTROL_P8p_SSSS_mod_2),mean(trait_mean_liab_Oonirici_WILD_P8p_SSSS_mod),mean(trait_mean_liab_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(mean_trait_mean_liab_Oonirici_P8p_SSSS_2) <- c("mean")
        median_trait_mean_liab_Oonirici_P8p_SSSS_2 <- rbind(median(trait_mean_liab_Oonirici_CONTROL_P8p_SSSS_mod_2),median(trait_mean_liab_Oonirici_WILD_P8p_SSSS_mod),median(trait_mean_liab_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(median_trait_mean_liab_Oonirici_P8p_SSSS_2) <- c("median")
        posterior.mode_trait_mean_liab_Oonirici_P8p_SSSS_2 <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P8p_SSSS_mod_2)),posterior.mode(as.mcmc(trait_mean_liab_Oonirici_WILD_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Oonirici_Vg_P8p_SSSS_mod_2)))
        colnames(posterior.mode_trait_mean_liab_Oonirici_P8p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Oonirici_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P8p_SSSS_mod_2)),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_WILD_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_Vg_P8p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_liab_Oonirici_P8p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Oonirici_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P8p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_WILD_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_Vg_P8p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Oonirici_P8p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Oonirici_P8p_SSSS_2 <- rbind(effectiveSize(trait_mean_liab_Oonirici_CONTROL_P8p_SSSS_mod_2),effectiveSize(trait_mean_liab_Oonirici_WILD_P8p_SSSS_mod),effectiveSize(trait_mean_liab_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(effectiveSize_trait_mean_liab_Oonirici_P8p_SSSS_2) <- c("effectiveSize")
        trait_mean_liab_Oonirici_P8p_SSSS_2 <- cbind.data.frame(mean_trait_mean_liab_Oonirici_P8p_SSSS_2,median_trait_mean_liab_Oonirici_P8p_SSSS_2,posterior.mode_trait_mean_liab_Oonirici_P8p_SSSS_2,HPDinterval_0.95_trait_mean_liab_Oonirici_P8p_SSSS_2,HPDinterval_0.83_trait_mean_liab_Oonirici_P8p_SSSS_2,effectiveSize_trait_mean_liab_Oonirici_P8p_SSSS_2)
        rownames(trait_mean_liab_Oonirici_P8p_SSSS_2) <- c("trait_mean_liab_Oonirici_CONTROL_P8p_SSSS_mod_2","trait_mean_liab_Oonirici_WILD_P8p_SSSS_mod","trait_mean_liab_Oonirici_Vg_P8p_SSSS_mod_2")
        trait_mean_liab_Oonirici_P8p_SSSS_2 <- cbind(Models = rownames(trait_mean_liab_Oonirici_P8p_SSSS_2),trait_mean_liab_Oonirici_P8p_SSSS_2)
        rownames(trait_mean_liab_Oonirici_P8p_SSSS_2) <- NULL
        trait_mean_liab_Oonirici_P8p_SSSS_2$Pnp <- c("P8.p","P8.p","P8.p")
        trait_mean_liab_Oonirici_P8p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_liab_Oonirici_P8p_SSSS_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Oonirici_P8p_SSSS_2$Scale <- c("liab","liab","liab")
        trait_mean_liab_Oonirici_P8p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_liab_Oonirici_P8p_SSSS_2
      }
      
      liab_Oonirici_P8p_SSSS_2 <- rbind.data.frame(va_liab_Oonirici_P8p_SSSS_2, h2_liab_Oonirici_P8p_SSSS_2,Evol_liab_Oonirici_P8p_SSSS_2,trait_mean_liab_Oonirici_P8p_SSSS_2)
      liab_Oonirici_P8p_SSSS_2
    }
    #Summary data scale Oonirici P8p
    {
      #Summary va_data_Oonirici_P8p_SSSS_2:NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        mean_va_data_Oonirici_P8p_SSSS_2 <- rbind(mean(va_data_Oonirici_CONTROL_P8p_SSSS_mod_2/2),mean(va_data_Oonirici_WILD_P8p_SSSS_mod/2),mean(va_data_Oonirici_Vg_P8p_SSSS_mod_2/2))
        colnames(mean_va_data_Oonirici_P8p_SSSS_2) <- c("mean")
        median_va_data_Oonirici_P8p_SSSS_2 <- rbind(median(va_data_Oonirici_CONTROL_P8p_SSSS_mod_2/2),median(va_data_Oonirici_WILD_P8p_SSSS_mod/2),median(va_data_Oonirici_Vg_P8p_SSSS_mod_2/2))
        colnames(median_va_data_Oonirici_P8p_SSSS_2) <- c("median")
        posterior.mode_va_data_Oonirici_P8p_SSSS_2 <- rbind(posterior.mode(as.mcmc(va_data_Oonirici_CONTROL_P8p_SSSS_mod_2/2)),posterior.mode(as.mcmc(va_data_Oonirici_WILD_P8p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_Oonirici_Vg_P8p_SSSS_mod_2/2)))
        colnames(posterior.mode_va_data_Oonirici_P8p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Oonirici_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(va_data_Oonirici_CONTROL_P8p_SSSS_mod_2/2)),HPDinterval(as.mcmc(va_data_Oonirici_WILD_P8p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_Oonirici_Vg_P8p_SSSS_mod_2/2)))
        colnames(HPDinterval_0.95_va_data_Oonirici_P8p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Oonirici_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(va_data_Oonirici_CONTROL_P8p_SSSS_mod_2/2),prob=.83),HPDinterval(as.mcmc(va_data_Oonirici_WILD_P8p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Oonirici_Vg_P8p_SSSS_mod_2/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Oonirici_P8p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Oonirici_P8p_SSSS_2 <- rbind(effectiveSize(va_data_Oonirici_CONTROL_P8p_SSSS_mod_2/2),effectiveSize(va_data_Oonirici_WILD_P8p_SSSS_mod/2),effectiveSize(va_data_Oonirici_Vg_P8p_SSSS_mod_2/2))
        colnames(effectiveSize_va_data_Oonirici_P8p_SSSS_2) <- c("effectiveSize")
        va_data_Oonirici_P8p_SSSS_2 <- cbind.data.frame(mean_va_data_Oonirici_P8p_SSSS_2,median_va_data_Oonirici_P8p_SSSS_2,posterior.mode_va_data_Oonirici_P8p_SSSS_2,HPDinterval_0.95_va_data_Oonirici_P8p_SSSS_2,HPDinterval_0.83_va_data_Oonirici_P8p_SSSS_2,effectiveSize_va_data_Oonirici_P8p_SSSS_2)
        rownames(va_data_Oonirici_P8p_SSSS_2) <- c("va_data_Oonirici_CONTROL_P8p_SSSS_mod_2","va_data_Oonirici_WILD_P8p_SSSS_mod","va_data_Oonirici_Vg_P8p_SSSS_mod_2")
        va_data_Oonirici_P8p_SSSS_2 <- cbind(Models = rownames(va_data_Oonirici_P8p_SSSS_2),va_data_Oonirici_P8p_SSSS_2)
        rownames(va_data_Oonirici_P8p_SSSS_2) <- NULL
        va_data_Oonirici_P8p_SSSS_2$Pnp <- c("P8.p","P8.p","P8.p")
        va_data_Oonirici_P8p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        va_data_Oonirici_P8p_SSSS_2$Measure <- c("Va","Va","Va")
        va_data_Oonirici_P8p_SSSS_2$Scale <- c("data","data","data")
        va_data_Oonirici_P8p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        va_data_Oonirici_P8p_SSSS_2
      }
      
      #Summary h2_data_Oonirici_P8p_SSSS_2
      {
        mean_h2_data_Oonirici_P8p_SSSS_2 <- rbind(mean(h2_data_Oonirici_CONTROL_P8p_SSSS_mod_2),mean(h2_data_Oonirici_WILD_P8p_SSSS_mod),mean(h2_data_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(mean_h2_data_Oonirici_P8p_SSSS_2) <- c("mean")
        median_h2_data_Oonirici_P8p_SSSS_2 <- rbind(median(h2_data_Oonirici_CONTROL_P8p_SSSS_mod_2),median(h2_data_Oonirici_WILD_P8p_SSSS_mod),median(h2_data_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(median_h2_data_Oonirici_P8p_SSSS_2) <- c("median")
        posterior.mode_h2_data_Oonirici_P8p_SSSS_2 <- rbind(posterior.mode(as.mcmc(h2_data_Oonirici_CONTROL_P8p_SSSS_mod_2)),posterior.mode(as.mcmc(h2_data_Oonirici_WILD_P8p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_Oonirici_Vg_P8p_SSSS_mod_2)))
        colnames(posterior.mode_h2_data_Oonirici_P8p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Oonirici_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(h2_data_Oonirici_CONTROL_P8p_SSSS_mod_2)),HPDinterval(as.mcmc(h2_data_Oonirici_WILD_P8p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_Oonirici_Vg_P8p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_h2_data_Oonirici_P8p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Oonirici_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(h2_data_Oonirici_CONTROL_P8p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(h2_data_Oonirici_WILD_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Oonirici_Vg_P8p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Oonirici_P8p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Oonirici_P8p_SSSS_2 <- rbind(effectiveSize(h2_data_Oonirici_CONTROL_P8p_SSSS_mod_2),effectiveSize(h2_data_Oonirici_WILD_P8p_SSSS_mod),effectiveSize(h2_data_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(effectiveSize_h2_data_Oonirici_P8p_SSSS_2) <- c("effectiveSize")
        h2_data_Oonirici_P8p_SSSS_2 <- cbind.data.frame(mean_h2_data_Oonirici_P8p_SSSS_2,median_h2_data_Oonirici_P8p_SSSS_2,posterior.mode_h2_data_Oonirici_P8p_SSSS_2,HPDinterval_0.95_h2_data_Oonirici_P8p_SSSS_2,HPDinterval_0.83_h2_data_Oonirici_P8p_SSSS_2,effectiveSize_h2_data_Oonirici_P8p_SSSS_2)
        rownames(h2_data_Oonirici_P8p_SSSS_2) <- c("h2_data_Oonirici_CONTROL_P8p_SSSS_mod_2","h2_data_Oonirici_WILD_P8p_SSSS_mod","h2_data_Oonirici_Vg_P8p_SSSS_mod_2")
        h2_data_Oonirici_P8p_SSSS_2 <- cbind(Models = rownames(h2_data_Oonirici_P8p_SSSS_2),h2_data_Oonirici_P8p_SSSS_2)
        rownames(h2_data_Oonirici_P8p_SSSS_2) <- NULL
        h2_data_Oonirici_P8p_SSSS_2$Pnp <- c("P8.p","P8.p","P8.p")
        h2_data_Oonirici_P8p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        h2_data_Oonirici_P8p_SSSS_2$Measure <- c("H2","H2","H2")
        h2_data_Oonirici_P8p_SSSS_2$Scale <- c("data","data","data")
        h2_data_Oonirici_P8p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        h2_data_Oonirici_P8p_SSSS_2
      }
      
      #Summary Evol_data_Oonirici_P8p_SSSS_2
      {
        mean_Evol_data_Oonirici_P8p_SSSS_2 <- rbind(mean(Evol_data_Oonirici_CONTROL_P8p_SSSS_mod_2),mean(Evol_data_Oonirici_WILD_P8p_SSSS_mod),mean(Evol_data_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(mean_Evol_data_Oonirici_P8p_SSSS_2) <- c("mean")
        median_Evol_data_Oonirici_P8p_SSSS_2 <- rbind(median(Evol_data_Oonirici_CONTROL_P8p_SSSS_mod_2),median(Evol_data_Oonirici_WILD_P8p_SSSS_mod),median(Evol_data_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(median_Evol_data_Oonirici_P8p_SSSS_2) <- c("median")
        posterior.mode_Evol_data_Oonirici_P8p_SSSS_2 <- rbind(posterior.mode(as.mcmc(Evol_data_Oonirici_CONTROL_P8p_SSSS_mod_2)),posterior.mode(as.mcmc(Evol_data_Oonirici_WILD_P8p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_Oonirici_Vg_P8p_SSSS_mod_2)))
        colnames(posterior.mode_Evol_data_Oonirici_P8p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Oonirici_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Oonirici_CONTROL_P8p_SSSS_mod_2)),HPDinterval(as.mcmc(Evol_data_Oonirici_WILD_P8p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_Oonirici_Vg_P8p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_Evol_data_Oonirici_P8p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Oonirici_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Oonirici_CONTROL_P8p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(Evol_data_Oonirici_WILD_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Oonirici_Vg_P8p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Oonirici_P8p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Oonirici_P8p_SSSS_2 <- rbind(effectiveSize(Evol_data_Oonirici_CONTROL_P8p_SSSS_mod_2),effectiveSize(Evol_data_Oonirici_WILD_P8p_SSSS_mod),effectiveSize(Evol_data_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(effectiveSize_Evol_data_Oonirici_P8p_SSSS_2) <- c("effectiveSize")
        Evol_data_Oonirici_P8p_SSSS_2 <- cbind.data.frame(mean_Evol_data_Oonirici_P8p_SSSS_2,median_Evol_data_Oonirici_P8p_SSSS_2,posterior.mode_Evol_data_Oonirici_P8p_SSSS_2,HPDinterval_0.95_Evol_data_Oonirici_P8p_SSSS_2,HPDinterval_0.83_Evol_data_Oonirici_P8p_SSSS_2,effectiveSize_Evol_data_Oonirici_P8p_SSSS_2)
        rownames(Evol_data_Oonirici_P8p_SSSS_2) <- c("Evol_data_Oonirici_CONTROL_P8p_SSSS_mod_2","Evol_data_Oonirici_WILD_P8p_SSSS_mod","Evol_data_Oonirici_Vg_P8p_SSSS_mod_2")
        Evol_data_Oonirici_P8p_SSSS_2 <- cbind(Models = rownames(Evol_data_Oonirici_P8p_SSSS_2),Evol_data_Oonirici_P8p_SSSS_2)
        rownames(Evol_data_Oonirici_P8p_SSSS_2) <- NULL
        Evol_data_Oonirici_P8p_SSSS_2$Pnp <- c("P8.p","P8.p","P8.p")
        Evol_data_Oonirici_P8p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        Evol_data_Oonirici_P8p_SSSS_2$Measure <- c("Evol","Evol","Evol")
        Evol_data_Oonirici_P8p_SSSS_2$Scale <- c("data","data","data")
        Evol_data_Oonirici_P8p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        Evol_data_Oonirici_P8p_SSSS_2
      }
      
      #Summary trait_mean_data_Oonirici_P8p_SSSS_2
      {
        mean_trait_mean_data_Oonirici_P8p_SSSS_2 <- rbind(mean(trait_mean_data_Oonirici_CONTROL_P8p_SSSS_mod_2),mean(trait_mean_data_Oonirici_WILD_P8p_SSSS_mod),mean(trait_mean_data_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(mean_trait_mean_data_Oonirici_P8p_SSSS_2) <- c("mean")
        median_trait_mean_data_Oonirici_P8p_SSSS_2 <- rbind(median(trait_mean_data_Oonirici_CONTROL_P8p_SSSS_mod_2),median(trait_mean_data_Oonirici_WILD_P8p_SSSS_mod),median(trait_mean_data_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(median_trait_mean_data_Oonirici_P8p_SSSS_2) <- c("median")
        posterior.mode_trait_mean_data_Oonirici_P8p_SSSS_2 <- rbind(posterior.mode(as.mcmc(trait_mean_data_Oonirici_CONTROL_P8p_SSSS_mod_2)),posterior.mode(as.mcmc(trait_mean_data_Oonirici_WILD_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_Oonirici_Vg_P8p_SSSS_mod_2)))
        colnames(posterior.mode_trait_mean_data_Oonirici_P8p_SSSS_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Oonirici_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Oonirici_CONTROL_P8p_SSSS_mod_2)),HPDinterval(as.mcmc(trait_mean_data_Oonirici_WILD_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_Oonirici_Vg_P8p_SSSS_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_data_Oonirici_P8p_SSSS_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Oonirici_P8p_SSSS_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Oonirici_CONTROL_P8p_SSSS_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Oonirici_WILD_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Oonirici_Vg_P8p_SSSS_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Oonirici_P8p_SSSS_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Oonirici_P8p_SSSS_2 <- rbind(effectiveSize(trait_mean_data_Oonirici_CONTROL_P8p_SSSS_mod_2),effectiveSize(trait_mean_data_Oonirici_WILD_P8p_SSSS_mod),effectiveSize(trait_mean_data_Oonirici_Vg_P8p_SSSS_mod_2))
        colnames(effectiveSize_trait_mean_data_Oonirici_P8p_SSSS_2) <- c("effectiveSize")
        trait_mean_data_Oonirici_P8p_SSSS_2 <- cbind.data.frame(mean_trait_mean_data_Oonirici_P8p_SSSS_2,median_trait_mean_data_Oonirici_P8p_SSSS_2,posterior.mode_trait_mean_data_Oonirici_P8p_SSSS_2,HPDinterval_0.95_trait_mean_data_Oonirici_P8p_SSSS_2,HPDinterval_0.83_trait_mean_data_Oonirici_P8p_SSSS_2,effectiveSize_trait_mean_data_Oonirici_P8p_SSSS_2)
        rownames(trait_mean_data_Oonirici_P8p_SSSS_2) <- c("trait_mean_data_Oonirici_CONTROL_P8p_SSSS_mod_2","trait_mean_data_Oonirici_WILD_P8p_SSSS_mod","trait_mean_data_Oonirici_Vg_P8p_SSSS_mod_2")
        trait_mean_data_Oonirici_P8p_SSSS_2 <- cbind(Models = rownames(trait_mean_data_Oonirici_P8p_SSSS_2),trait_mean_data_Oonirici_P8p_SSSS_2)
        rownames(trait_mean_data_Oonirici_P8p_SSSS_2) <- NULL
        trait_mean_data_Oonirici_P8p_SSSS_2$Pnp <- c("P8.p","P8.p","P8.p")
        trait_mean_data_Oonirici_P8p_SSSS_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_data_Oonirici_P8p_SSSS_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Oonirici_P8p_SSSS_2$Scale <- c("data","data","data")
        trait_mean_data_Oonirici_P8p_SSSS_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_data_Oonirici_P8p_SSSS_2
      }
      
      data_Oonirici_P8p_SSSS_2 <- rbind.data.frame(va_data_Oonirici_P8p_SSSS_2, h2_data_Oonirici_P8p_SSSS_2,Evol_data_Oonirici_P8p_SSSS_2,trait_mean_data_Oonirici_P8p_SSSS_2)
      data_Oonirici_P8p_SSSS_2
      
    }
    Vg_Oonirici_P8p_SSSS_2 <- rbind.data.frame(liab_Oonirici_P8p_SSSS_2, data_Oonirici_P8p_SSSS_2)
    Vg_Oonirici_P8p_SSSS_2$Pnp_fate <- rep("SSSS", 24)
    Vg_Oonirici_P8p_SSSS_2
    #remove Oonirici P8p_SSSS_2 models
    {
      remove(Oonirici_CONTROL_P8p_SSSS_mod_2)
      remove(Oonirici_WILD_P8p_SSSS_mod)
      remove(Oonirici_Vg_P8p_SSSS_mod_2)
    }
  }
  
  ##Summary Oonirici P5p ----
  {
    #Summary liability scale Oonirici P5p
    {
      #Summary va_liab_Oonirici_P5p_wt_2: NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        
        mean_va_liab_Oonirici_P5p_wt_2 <- rbind(mean(va_liab_Oonirici_CONTROL_P5p_wt_mod_2/2),mean(va_liab_Oonirici_WILD_P5p_wt_mod/2),mean(va_liab_Oonirici_Vg_P5p_wt_mod_2/2))
        colnames(mean_va_liab_Oonirici_P5p_wt_2) <- c("mean")
        median_va_liab_Oonirici_P5p_wt_2 <- rbind(median(va_liab_Oonirici_CONTROL_P5p_wt_mod_2/2),median(va_liab_Oonirici_WILD_P5p_wt_mod/2),median(va_liab_Oonirici_Vg_P5p_wt_mod_2/2))
        colnames(median_va_liab_Oonirici_P5p_wt_2) <- c("median")
        posterior.mode_va_liab_Oonirici_P5p_wt_2 <- rbind(posterior.mode(va_liab_Oonirici_CONTROL_P5p_wt_mod_2/2),posterior.mode(va_liab_Oonirici_WILD_P5p_wt_mod/2),posterior.mode(va_liab_Oonirici_Vg_P5p_wt_mod_2/2))
        colnames(posterior.mode_va_liab_Oonirici_P5p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Oonirici_P5p_wt_2 <- rbind(HPDinterval(va_liab_Oonirici_CONTROL_P5p_wt_mod_2/2),HPDinterval(va_liab_Oonirici_WILD_P5p_wt_mod/2),HPDinterval(va_liab_Oonirici_Vg_P5p_wt_mod_2/2))
        colnames(HPDinterval_0.95_va_liab_Oonirici_P5p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Oonirici_P5p_wt_2 <- rbind(HPDinterval(va_liab_Oonirici_CONTROL_P5p_wt_mod_2/2,prob=.83),HPDinterval(va_liab_Oonirici_WILD_P5p_wt_mod/2,prob=.83),HPDinterval(va_liab_Oonirici_Vg_P5p_wt_mod_2/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Oonirici_P5p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Oonirici_P5p_wt_2 <- rbind(effectiveSize(va_liab_Oonirici_CONTROL_P5p_wt_mod_2/2),effectiveSize(va_liab_Oonirici_WILD_P5p_wt_mod/2),effectiveSize(va_liab_Oonirici_Vg_P5p_wt_mod_2/2))
        colnames(effectiveSize_va_liab_Oonirici_P5p_wt_2) <- c("effectiveSize")
        va_liab_Oonirici_P5p_wt_2 <- cbind.data.frame(mean_va_liab_Oonirici_P5p_wt_2,median_va_liab_Oonirici_P5p_wt_2,posterior.mode_va_liab_Oonirici_P5p_wt_2,HPDinterval_0.95_va_liab_Oonirici_P5p_wt_2,HPDinterval_0.83_va_liab_Oonirici_P5p_wt_2,effectiveSize_va_liab_Oonirici_P5p_wt_2)
        rownames(va_liab_Oonirici_P5p_wt_2) <- c("va_liab_Oonirici_CONTROL_P5p_wt_mod_2","va_liab_Oonirici_WILD_P5p_wt_mod","va_liab_Oonirici_Vg_P5p_wt_mod_2")
        va_liab_Oonirici_P5p_wt_2 <- cbind(Models = rownames(va_liab_Oonirici_P5p_wt_2),va_liab_Oonirici_P5p_wt_2)
        rownames(va_liab_Oonirici_P5p_wt_2) <- NULL
        va_liab_Oonirici_P5p_wt_2$Pnp <- c("P5.p","P5.p","P5.p")
        va_liab_Oonirici_P5p_wt_2$Treatment <- c("Control","WILD","Vg")
        va_liab_Oonirici_P5p_wt_2$Measure <- c("Va","Va","Va")
        va_liab_Oonirici_P5p_wt_2$Scale <- c("liab","liab","liab")
        va_liab_Oonirici_P5p_wt_2$Variance <- c("Vg","Vg","Vg")
        va_liab_Oonirici_P5p_wt_2
      }
      
      #Summary h2_liab_Oonirici_P5p_wt_2
      {
        mean_h2_liab_Oonirici_P5p_wt_2 <- rbind(mean(h2_liab_Oonirici_CONTROL_P5p_wt_mod_2),mean(h2_liab_Oonirici_WILD_P5p_wt_mod),mean(h2_liab_Oonirici_Vg_P5p_wt_mod_2))
        colnames(mean_h2_liab_Oonirici_P5p_wt_2) <- c("mean")
        median_h2_liab_Oonirici_P5p_wt_2 <- rbind(median(h2_liab_Oonirici_CONTROL_P5p_wt_mod_2),median(h2_liab_Oonirici_WILD_P5p_wt_mod),median(h2_liab_Oonirici_Vg_P5p_wt_mod_2))
        colnames(median_h2_liab_Oonirici_P5p_wt_2) <- c("median")
        posterior.mode_h2_liab_Oonirici_P5p_wt_2 <- rbind(posterior.mode(h2_liab_Oonirici_CONTROL_P5p_wt_mod_2),posterior.mode(h2_liab_Oonirici_WILD_P5p_wt_mod),posterior.mode(h2_liab_Oonirici_Vg_P5p_wt_mod_2))
        colnames(posterior.mode_h2_liab_Oonirici_P5p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Oonirici_P5p_wt_2 <- rbind(HPDinterval(h2_liab_Oonirici_CONTROL_P5p_wt_mod_2),HPDinterval(h2_liab_Oonirici_WILD_P5p_wt_mod),HPDinterval(h2_liab_Oonirici_Vg_P5p_wt_mod_2))
        colnames(HPDinterval_0.95_h2_liab_Oonirici_P5p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Oonirici_P5p_wt_2 <- rbind(HPDinterval(h2_liab_Oonirici_CONTROL_P5p_wt_mod_2,prob=.83),HPDinterval(h2_liab_Oonirici_WILD_P5p_wt_mod,prob=.83),HPDinterval(h2_liab_Oonirici_Vg_P5p_wt_mod_2,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Oonirici_P5p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Oonirici_P5p_wt_2 <- rbind(effectiveSize(h2_liab_Oonirici_CONTROL_P5p_wt_mod_2),effectiveSize(h2_liab_Oonirici_WILD_P5p_wt_mod),effectiveSize(h2_liab_Oonirici_Vg_P5p_wt_mod_2))
        colnames(effectiveSize_h2_liab_Oonirici_P5p_wt_2) <- c("effectiveSize")
        h2_liab_Oonirici_P5p_wt_2 <- cbind.data.frame(mean_h2_liab_Oonirici_P5p_wt_2,median_h2_liab_Oonirici_P5p_wt_2,posterior.mode_h2_liab_Oonirici_P5p_wt_2,HPDinterval_0.95_h2_liab_Oonirici_P5p_wt_2,HPDinterval_0.83_h2_liab_Oonirici_P5p_wt_2,effectiveSize_h2_liab_Oonirici_P5p_wt_2)
        rownames(h2_liab_Oonirici_P5p_wt_2) <- c("h2_liab_Oonirici_CONTROL_P5p_wt_mod_2","h2_liab_Oonirici_WILD_P5p_wt_mod","h2_liab_Oonirici_Vg_P5p_wt_mod_2")
        h2_liab_Oonirici_P5p_wt_2 <- cbind(Models = rownames(h2_liab_Oonirici_P5p_wt_2),h2_liab_Oonirici_P5p_wt_2)
        rownames(h2_liab_Oonirici_P5p_wt_2) <- NULL
        h2_liab_Oonirici_P5p_wt_2$Pnp <- c("P5.p","P5.p","P5.p")
        h2_liab_Oonirici_P5p_wt_2$Treatment <- c("Control","WILD","Vg")
        h2_liab_Oonirici_P5p_wt_2$Measure <- c("H2","H2","H2")
        h2_liab_Oonirici_P5p_wt_2$Scale <- c("liab","liab","liab")
        h2_liab_Oonirici_P5p_wt_2$Variance <- c("Vg","Vg","Vg")
        h2_liab_Oonirici_P5p_wt_2
      }
      
      #Summary Evol_liab_Oonirici_P5p_wt_2
      {
        mean_Evol_liab_Oonirici_P5p_wt_2 <- rbind(mean(Evol_liab_Oonirici_CONTROL_P5p_wt_mod_2),mean(Evol_liab_Oonirici_WILD_P5p_wt_mod),mean(Evol_liab_Oonirici_Vg_P5p_wt_mod_2))
        colnames(mean_Evol_liab_Oonirici_P5p_wt_2) <- c("mean")
        median_Evol_liab_Oonirici_P5p_wt_2 <- rbind(median(Evol_liab_Oonirici_CONTROL_P5p_wt_mod_2),median(Evol_liab_Oonirici_WILD_P5p_wt_mod),median(Evol_liab_Oonirici_Vg_P5p_wt_mod_2))
        colnames(median_Evol_liab_Oonirici_P5p_wt_2) <- c("median")
        posterior.mode_Evol_liab_Oonirici_P5p_wt_2 <- rbind(posterior.mode(Evol_liab_Oonirici_CONTROL_P5p_wt_mod_2),posterior.mode(Evol_liab_Oonirici_WILD_P5p_wt_mod),posterior.mode(Evol_liab_Oonirici_Vg_P5p_wt_mod_2))
        colnames(posterior.mode_Evol_liab_Oonirici_P5p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Oonirici_P5p_wt_2 <- rbind(HPDinterval(Evol_liab_Oonirici_CONTROL_P5p_wt_mod_2),HPDinterval(Evol_liab_Oonirici_WILD_P5p_wt_mod),HPDinterval(Evol_liab_Oonirici_Vg_P5p_wt_mod_2))
        colnames(HPDinterval_0.95_Evol_liab_Oonirici_P5p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Oonirici_P5p_wt_2 <- rbind(HPDinterval(Evol_liab_Oonirici_CONTROL_P5p_wt_mod_2,prob=.83),HPDinterval(Evol_liab_Oonirici_WILD_P5p_wt_mod,prob=.83),HPDinterval(Evol_liab_Oonirici_Vg_P5p_wt_mod_2,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Oonirici_P5p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Oonirici_P5p_wt_2 <- rbind(effectiveSize(Evol_liab_Oonirici_CONTROL_P5p_wt_mod_2),effectiveSize(Evol_liab_Oonirici_WILD_P5p_wt_mod),effectiveSize(Evol_liab_Oonirici_Vg_P5p_wt_mod_2))
        colnames(effectiveSize_Evol_liab_Oonirici_P5p_wt_2) <- c("effectiveSize")
        Evol_liab_Oonirici_P5p_wt_2 <- cbind.data.frame(mean_Evol_liab_Oonirici_P5p_wt_2,median_Evol_liab_Oonirici_P5p_wt_2,posterior.mode_Evol_liab_Oonirici_P5p_wt_2,HPDinterval_0.95_Evol_liab_Oonirici_P5p_wt_2,HPDinterval_0.83_Evol_liab_Oonirici_P5p_wt_2,effectiveSize_Evol_liab_Oonirici_P5p_wt_2)
        rownames(Evol_liab_Oonirici_P5p_wt_2) <- c("Evol_liab_Oonirici_CONTROL_P5p_wt_mod_2","Evol_liab_Oonirici_WILD_P5p_wt_mod","Evol_liab_Oonirici_Vg_P5p_wt_mod_2")
        Evol_liab_Oonirici_P5p_wt_2 <- cbind(Models = rownames(Evol_liab_Oonirici_P5p_wt_2),Evol_liab_Oonirici_P5p_wt_2)
        rownames(Evol_liab_Oonirici_P5p_wt_2) <- NULL
        Evol_liab_Oonirici_P5p_wt_2$Pnp <- c("P5.p","P5.p","P5.p")
        Evol_liab_Oonirici_P5p_wt_2$Treatment <- c("Control","WILD","Vg")
        Evol_liab_Oonirici_P5p_wt_2$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Oonirici_P5p_wt_2$Scale <- c("liab","liab","liab")
        Evol_liab_Oonirici_P5p_wt_2$Variance <- c("Vg","Vg","Vg")
        Evol_liab_Oonirici_P5p_wt_2
      }
      
      #Summary trait_mean_liab_Oonirici_P5p_wt_2
      {
        mean_trait_mean_liab_Oonirici_P5p_wt_2 <- rbind(mean(trait_mean_liab_Oonirici_CONTROL_P5p_wt_mod_2),mean(trait_mean_liab_Oonirici_WILD_P5p_wt_mod),mean(trait_mean_liab_Oonirici_Vg_P5p_wt_mod_2))
        colnames(mean_trait_mean_liab_Oonirici_P5p_wt_2) <- c("mean")
        median_trait_mean_liab_Oonirici_P5p_wt_2 <- rbind(median(trait_mean_liab_Oonirici_CONTROL_P5p_wt_mod_2),median(trait_mean_liab_Oonirici_WILD_P5p_wt_mod),median(trait_mean_liab_Oonirici_Vg_P5p_wt_mod_2))
        colnames(median_trait_mean_liab_Oonirici_P5p_wt_2) <- c("median")
        posterior.mode_trait_mean_liab_Oonirici_P5p_wt_2 <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P5p_wt_mod_2)),posterior.mode(as.mcmc(trait_mean_liab_Oonirici_WILD_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Oonirici_Vg_P5p_wt_mod_2)))
        colnames(posterior.mode_trait_mean_liab_Oonirici_P5p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Oonirici_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P5p_wt_mod_2)),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_WILD_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_Vg_P5p_wt_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_liab_Oonirici_P5p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Oonirici_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P5p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_WILD_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_Vg_P5p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Oonirici_P5p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Oonirici_P5p_wt_2 <- rbind(effectiveSize(trait_mean_liab_Oonirici_CONTROL_P5p_wt_mod_2),effectiveSize(trait_mean_liab_Oonirici_WILD_P5p_wt_mod),effectiveSize(trait_mean_liab_Oonirici_Vg_P5p_wt_mod_2))
        colnames(effectiveSize_trait_mean_liab_Oonirici_P5p_wt_2) <- c("effectiveSize")
        trait_mean_liab_Oonirici_P5p_wt_2 <- cbind.data.frame(mean_trait_mean_liab_Oonirici_P5p_wt_2,median_trait_mean_liab_Oonirici_P5p_wt_2,posterior.mode_trait_mean_liab_Oonirici_P5p_wt_2,HPDinterval_0.95_trait_mean_liab_Oonirici_P5p_wt_2,HPDinterval_0.83_trait_mean_liab_Oonirici_P5p_wt_2,effectiveSize_trait_mean_liab_Oonirici_P5p_wt_2)
        rownames(trait_mean_liab_Oonirici_P5p_wt_2) <- c("trait_mean_liab_Oonirici_CONTROL_P5p_wt_mod_2","trait_mean_liab_Oonirici_WILD_P5p_wt_mod","trait_mean_liab_Oonirici_Vg_P5p_wt_mod_2")
        trait_mean_liab_Oonirici_P5p_wt_2 <- cbind(Models = rownames(trait_mean_liab_Oonirici_P5p_wt_2),trait_mean_liab_Oonirici_P5p_wt_2)
        rownames(trait_mean_liab_Oonirici_P5p_wt_2) <- NULL
        trait_mean_liab_Oonirici_P5p_wt_2$Pnp <- c("P5.p","P5.p","P5.p")
        trait_mean_liab_Oonirici_P5p_wt_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_liab_Oonirici_P5p_wt_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Oonirici_P5p_wt_2$Scale <- c("liab","liab","liab")
        trait_mean_liab_Oonirici_P5p_wt_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_liab_Oonirici_P5p_wt_2
      }
      
      liab_Oonirici_P5p_wt_2 <- rbind.data.frame(va_liab_Oonirici_P5p_wt_2, h2_liab_Oonirici_P5p_wt_2,Evol_liab_Oonirici_P5p_wt_2,trait_mean_liab_Oonirici_P5p_wt_2)
      liab_Oonirici_P5p_wt_2
    }
    #Summary data scale Oonirici P5p
    {
      #Summary va_data_Oonirici_P5p_wt_2:NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        mean_va_data_Oonirici_P5p_wt_2 <- rbind(mean(va_data_Oonirici_CONTROL_P5p_wt_mod_2/2),mean(va_data_Oonirici_WILD_P5p_wt_mod/2),mean(va_data_Oonirici_Vg_P5p_wt_mod_2/2))
        colnames(mean_va_data_Oonirici_P5p_wt_2) <- c("mean")
        median_va_data_Oonirici_P5p_wt_2 <- rbind(median(va_data_Oonirici_CONTROL_P5p_wt_mod_2/2),median(va_data_Oonirici_WILD_P5p_wt_mod/2),median(va_data_Oonirici_Vg_P5p_wt_mod_2/2))
        colnames(median_va_data_Oonirici_P5p_wt_2) <- c("median")
        posterior.mode_va_data_Oonirici_P5p_wt_2 <- rbind(posterior.mode(as.mcmc(va_data_Oonirici_CONTROL_P5p_wt_mod_2/2)),posterior.mode(as.mcmc(va_data_Oonirici_WILD_P5p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Oonirici_Vg_P5p_wt_mod_2/2)))
        colnames(posterior.mode_va_data_Oonirici_P5p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Oonirici_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(va_data_Oonirici_CONTROL_P5p_wt_mod_2/2)),HPDinterval(as.mcmc(va_data_Oonirici_WILD_P5p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Oonirici_Vg_P5p_wt_mod_2/2)))
        colnames(HPDinterval_0.95_va_data_Oonirici_P5p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Oonirici_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(va_data_Oonirici_CONTROL_P5p_wt_mod_2/2),prob=.83),HPDinterval(as.mcmc(va_data_Oonirici_WILD_P5p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Oonirici_Vg_P5p_wt_mod_2/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Oonirici_P5p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Oonirici_P5p_wt_2 <- rbind(effectiveSize(va_data_Oonirici_CONTROL_P5p_wt_mod_2/2),effectiveSize(va_data_Oonirici_WILD_P5p_wt_mod/2),effectiveSize(va_data_Oonirici_Vg_P5p_wt_mod_2/2))
        colnames(effectiveSize_va_data_Oonirici_P5p_wt_2) <- c("effectiveSize")
        va_data_Oonirici_P5p_wt_2 <- cbind.data.frame(mean_va_data_Oonirici_P5p_wt_2,median_va_data_Oonirici_P5p_wt_2,posterior.mode_va_data_Oonirici_P5p_wt_2,HPDinterval_0.95_va_data_Oonirici_P5p_wt_2,HPDinterval_0.83_va_data_Oonirici_P5p_wt_2,effectiveSize_va_data_Oonirici_P5p_wt_2)
        rownames(va_data_Oonirici_P5p_wt_2) <- c("va_data_Oonirici_CONTROL_P5p_wt_mod_2","va_data_Oonirici_WILD_P5p_wt_mod","va_data_Oonirici_Vg_P5p_wt_mod_2")
        va_data_Oonirici_P5p_wt_2 <- cbind(Models = rownames(va_data_Oonirici_P5p_wt_2),va_data_Oonirici_P5p_wt_2)
        rownames(va_data_Oonirici_P5p_wt_2) <- NULL
        va_data_Oonirici_P5p_wt_2$Pnp <- c("P5.p","P5.p","P5.p")
        va_data_Oonirici_P5p_wt_2$Treatment <- c("Control","WILD","Vg")
        va_data_Oonirici_P5p_wt_2$Measure <- c("Va","Va","Va")
        va_data_Oonirici_P5p_wt_2$Scale <- c("data","data","data")
        va_data_Oonirici_P5p_wt_2$Variance <- c("Vg","Vg","Vg")
        va_data_Oonirici_P5p_wt_2
      }
      
      #Summary h2_data_Oonirici_P5p_wt_2
      {
        mean_h2_data_Oonirici_P5p_wt_2 <- rbind(mean(h2_data_Oonirici_CONTROL_P5p_wt_mod_2),mean(h2_data_Oonirici_WILD_P5p_wt_mod),mean(h2_data_Oonirici_Vg_P5p_wt_mod_2))
        colnames(mean_h2_data_Oonirici_P5p_wt_2) <- c("mean")
        median_h2_data_Oonirici_P5p_wt_2 <- rbind(median(h2_data_Oonirici_CONTROL_P5p_wt_mod_2),median(h2_data_Oonirici_WILD_P5p_wt_mod),median(h2_data_Oonirici_Vg_P5p_wt_mod_2))
        colnames(median_h2_data_Oonirici_P5p_wt_2) <- c("median")
        posterior.mode_h2_data_Oonirici_P5p_wt_2 <- rbind(posterior.mode(as.mcmc(h2_data_Oonirici_CONTROL_P5p_wt_mod_2)),posterior.mode(as.mcmc(h2_data_Oonirici_WILD_P5p_wt_mod)),posterior.mode(as.mcmc(h2_data_Oonirici_Vg_P5p_wt_mod_2)))
        colnames(posterior.mode_h2_data_Oonirici_P5p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Oonirici_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(h2_data_Oonirici_CONTROL_P5p_wt_mod_2)),HPDinterval(as.mcmc(h2_data_Oonirici_WILD_P5p_wt_mod)),HPDinterval(as.mcmc(h2_data_Oonirici_Vg_P5p_wt_mod_2)))
        colnames(HPDinterval_0.95_h2_data_Oonirici_P5p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Oonirici_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(h2_data_Oonirici_CONTROL_P5p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(h2_data_Oonirici_WILD_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Oonirici_Vg_P5p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Oonirici_P5p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Oonirici_P5p_wt_2 <- rbind(effectiveSize(h2_data_Oonirici_CONTROL_P5p_wt_mod_2),effectiveSize(h2_data_Oonirici_WILD_P5p_wt_mod),effectiveSize(h2_data_Oonirici_Vg_P5p_wt_mod_2))
        colnames(effectiveSize_h2_data_Oonirici_P5p_wt_2) <- c("effectiveSize")
        h2_data_Oonirici_P5p_wt_2 <- cbind.data.frame(mean_h2_data_Oonirici_P5p_wt_2,median_h2_data_Oonirici_P5p_wt_2,posterior.mode_h2_data_Oonirici_P5p_wt_2,HPDinterval_0.95_h2_data_Oonirici_P5p_wt_2,HPDinterval_0.83_h2_data_Oonirici_P5p_wt_2,effectiveSize_h2_data_Oonirici_P5p_wt_2)
        rownames(h2_data_Oonirici_P5p_wt_2) <- c("h2_data_Oonirici_CONTROL_P5p_wt_mod_2","h2_data_Oonirici_WILD_P5p_wt_mod","h2_data_Oonirici_Vg_P5p_wt_mod_2")
        h2_data_Oonirici_P5p_wt_2 <- cbind(Models = rownames(h2_data_Oonirici_P5p_wt_2),h2_data_Oonirici_P5p_wt_2)
        rownames(h2_data_Oonirici_P5p_wt_2) <- NULL
        h2_data_Oonirici_P5p_wt_2$Pnp <- c("P5.p","P5.p","P5.p")
        h2_data_Oonirici_P5p_wt_2$Treatment <- c("Control","WILD","Vg")
        h2_data_Oonirici_P5p_wt_2$Measure <- c("H2","H2","H2")
        h2_data_Oonirici_P5p_wt_2$Scale <- c("data","data","data")
        h2_data_Oonirici_P5p_wt_2$Variance <- c("Vg","Vg","Vg")
        h2_data_Oonirici_P5p_wt_2
      }
      
      #Summary Evol_data_Oonirici_P5p_wt_2
      {
        mean_Evol_data_Oonirici_P5p_wt_2 <- rbind(mean(Evol_data_Oonirici_CONTROL_P5p_wt_mod_2),mean(Evol_data_Oonirici_WILD_P5p_wt_mod),mean(Evol_data_Oonirici_Vg_P5p_wt_mod_2))
        colnames(mean_Evol_data_Oonirici_P5p_wt_2) <- c("mean")
        median_Evol_data_Oonirici_P5p_wt_2 <- rbind(median(Evol_data_Oonirici_CONTROL_P5p_wt_mod_2),median(Evol_data_Oonirici_WILD_P5p_wt_mod),median(Evol_data_Oonirici_Vg_P5p_wt_mod_2))
        colnames(median_Evol_data_Oonirici_P5p_wt_2) <- c("median")
        posterior.mode_Evol_data_Oonirici_P5p_wt_2 <- rbind(posterior.mode(as.mcmc(Evol_data_Oonirici_CONTROL_P5p_wt_mod_2)),posterior.mode(as.mcmc(Evol_data_Oonirici_WILD_P5p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Oonirici_Vg_P5p_wt_mod_2)))
        colnames(posterior.mode_Evol_data_Oonirici_P5p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Oonirici_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Oonirici_CONTROL_P5p_wt_mod_2)),HPDinterval(as.mcmc(Evol_data_Oonirici_WILD_P5p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Oonirici_Vg_P5p_wt_mod_2)))
        colnames(HPDinterval_0.95_Evol_data_Oonirici_P5p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Oonirici_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Oonirici_CONTROL_P5p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(Evol_data_Oonirici_WILD_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Oonirici_Vg_P5p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Oonirici_P5p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Oonirici_P5p_wt_2 <- rbind(effectiveSize(Evol_data_Oonirici_CONTROL_P5p_wt_mod_2),effectiveSize(Evol_data_Oonirici_WILD_P5p_wt_mod),effectiveSize(Evol_data_Oonirici_Vg_P5p_wt_mod_2))
        colnames(effectiveSize_Evol_data_Oonirici_P5p_wt_2) <- c("effectiveSize")
        Evol_data_Oonirici_P5p_wt_2 <- cbind.data.frame(mean_Evol_data_Oonirici_P5p_wt_2,median_Evol_data_Oonirici_P5p_wt_2,posterior.mode_Evol_data_Oonirici_P5p_wt_2,HPDinterval_0.95_Evol_data_Oonirici_P5p_wt_2,HPDinterval_0.83_Evol_data_Oonirici_P5p_wt_2,effectiveSize_Evol_data_Oonirici_P5p_wt_2)
        rownames(Evol_data_Oonirici_P5p_wt_2) <- c("Evol_data_Oonirici_CONTROL_P5p_wt_mod_2","Evol_data_Oonirici_WILD_P5p_wt_mod","Evol_data_Oonirici_Vg_P5p_wt_mod_2")
        Evol_data_Oonirici_P5p_wt_2 <- cbind(Models = rownames(Evol_data_Oonirici_P5p_wt_2),Evol_data_Oonirici_P5p_wt_2)
        rownames(Evol_data_Oonirici_P5p_wt_2) <- NULL
        Evol_data_Oonirici_P5p_wt_2$Pnp <- c("P5.p","P5.p","P5.p")
        Evol_data_Oonirici_P5p_wt_2$Treatment <- c("Control","WILD","Vg")
        Evol_data_Oonirici_P5p_wt_2$Measure <- c("Evol","Evol","Evol")
        Evol_data_Oonirici_P5p_wt_2$Scale <- c("data","data","data")
        Evol_data_Oonirici_P5p_wt_2$Variance <- c("Vg","Vg","Vg")
        Evol_data_Oonirici_P5p_wt_2
      }
      
      #Summary trait_mean_data_Oonirici_P5p_wt_2
      {
        mean_trait_mean_data_Oonirici_P5p_wt_2 <- rbind(mean(trait_mean_data_Oonirici_CONTROL_P5p_wt_mod_2),mean(trait_mean_data_Oonirici_WILD_P5p_wt_mod),mean(trait_mean_data_Oonirici_Vg_P5p_wt_mod_2))
        colnames(mean_trait_mean_data_Oonirici_P5p_wt_2) <- c("mean")
        median_trait_mean_data_Oonirici_P5p_wt_2 <- rbind(median(trait_mean_data_Oonirici_CONTROL_P5p_wt_mod_2),median(trait_mean_data_Oonirici_WILD_P5p_wt_mod),median(trait_mean_data_Oonirici_Vg_P5p_wt_mod_2))
        colnames(median_trait_mean_data_Oonirici_P5p_wt_2) <- c("median")
        posterior.mode_trait_mean_data_Oonirici_P5p_wt_2 <- rbind(posterior.mode(as.mcmc(trait_mean_data_Oonirici_CONTROL_P5p_wt_mod_2)),posterior.mode(as.mcmc(trait_mean_data_Oonirici_WILD_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Oonirici_Vg_P5p_wt_mod_2)))
        colnames(posterior.mode_trait_mean_data_Oonirici_P5p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Oonirici_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Oonirici_CONTROL_P5p_wt_mod_2)),HPDinterval(as.mcmc(trait_mean_data_Oonirici_WILD_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Oonirici_Vg_P5p_wt_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_data_Oonirici_P5p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Oonirici_P5p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Oonirici_CONTROL_P5p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Oonirici_WILD_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Oonirici_Vg_P5p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Oonirici_P5p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Oonirici_P5p_wt_2 <- rbind(effectiveSize(trait_mean_data_Oonirici_CONTROL_P5p_wt_mod_2),effectiveSize(trait_mean_data_Oonirici_WILD_P5p_wt_mod),effectiveSize(trait_mean_data_Oonirici_Vg_P5p_wt_mod_2))
        colnames(effectiveSize_trait_mean_data_Oonirici_P5p_wt_2) <- c("effectiveSize")
        trait_mean_data_Oonirici_P5p_wt_2 <- cbind.data.frame(mean_trait_mean_data_Oonirici_P5p_wt_2,median_trait_mean_data_Oonirici_P5p_wt_2,posterior.mode_trait_mean_data_Oonirici_P5p_wt_2,HPDinterval_0.95_trait_mean_data_Oonirici_P5p_wt_2,HPDinterval_0.83_trait_mean_data_Oonirici_P5p_wt_2,effectiveSize_trait_mean_data_Oonirici_P5p_wt_2)
        rownames(trait_mean_data_Oonirici_P5p_wt_2) <- c("trait_mean_data_Oonirici_CONTROL_P5p_wt_mod_2","trait_mean_data_Oonirici_WILD_P5p_wt_mod","trait_mean_data_Oonirici_Vg_P5p_wt_mod_2")
        trait_mean_data_Oonirici_P5p_wt_2 <- cbind(Models = rownames(trait_mean_data_Oonirici_P5p_wt_2),trait_mean_data_Oonirici_P5p_wt_2)
        rownames(trait_mean_data_Oonirici_P5p_wt_2) <- NULL
        trait_mean_data_Oonirici_P5p_wt_2$Pnp <- c("P5.p","P5.p","P5.p")
        trait_mean_data_Oonirici_P5p_wt_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_data_Oonirici_P5p_wt_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Oonirici_P5p_wt_2$Scale <- c("data","data","data")
        trait_mean_data_Oonirici_P5p_wt_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_data_Oonirici_P5p_wt_2
      }
      
      data_Oonirici_P5p_wt_2 <- rbind.data.frame(va_data_Oonirici_P5p_wt_2, h2_data_Oonirici_P5p_wt_2,Evol_data_Oonirici_P5p_wt_2,trait_mean_data_Oonirici_P5p_wt_2)
      data_Oonirici_P5p_wt_2
      
    }
    Vg_Oonirici_P5p_wt_2 <- rbind.data.frame(liab_Oonirici_P5p_wt_2, data_Oonirici_P5p_wt_2)
    Vg_Oonirici_P5p_wt_2$Pnp_fate <- rep("wt", 24)
    Vg_Oonirici_P5p_wt_2
    #remove Oonirici P5p_wt_2 models
    {
      remove(Oonirici_CONTROL_P5p_wt_mod_2)
      remove(Oonirici_WILD_P5p_wt_mod)
      remove(Oonirici_Vg_P5p_wt_mod_2)
    }
  }
  
  ##Summary Oonirici P6p----
  {
    #Summary liability scale Oonirici P6p
    {
      #Summary va_liab_Oonirici_P6p_wt_2: NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        
        mean_va_liab_Oonirici_P6p_wt_2 <- rbind(mean(va_liab_Oonirici_CONTROL_P6p_wt_mod_2/2),mean(va_liab_Oonirici_WILD_P6p_wt_mod/2),mean(va_liab_Oonirici_Vg_P6p_wt_mod_2/2))
        colnames(mean_va_liab_Oonirici_P6p_wt_2) <- c("mean")
        median_va_liab_Oonirici_P6p_wt_2 <- rbind(median(va_liab_Oonirici_CONTROL_P6p_wt_mod_2/2),median(va_liab_Oonirici_WILD_P6p_wt_mod/2),median(va_liab_Oonirici_Vg_P6p_wt_mod_2/2))
        colnames(median_va_liab_Oonirici_P6p_wt_2) <- c("median")
        posterior.mode_va_liab_Oonirici_P6p_wt_2 <- rbind(posterior.mode(va_liab_Oonirici_CONTROL_P6p_wt_mod_2/2),posterior.mode(va_liab_Oonirici_WILD_P6p_wt_mod/2),posterior.mode(va_liab_Oonirici_Vg_P6p_wt_mod_2/2))
        colnames(posterior.mode_va_liab_Oonirici_P6p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Oonirici_P6p_wt_2 <- rbind(HPDinterval(va_liab_Oonirici_CONTROL_P6p_wt_mod_2/2),HPDinterval(va_liab_Oonirici_WILD_P6p_wt_mod/2),HPDinterval(va_liab_Oonirici_Vg_P6p_wt_mod_2/2))
        colnames(HPDinterval_0.95_va_liab_Oonirici_P6p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Oonirici_P6p_wt_2 <- rbind(HPDinterval(va_liab_Oonirici_CONTROL_P6p_wt_mod_2/2,prob=.83),HPDinterval(va_liab_Oonirici_WILD_P6p_wt_mod/2,prob=.83),HPDinterval(va_liab_Oonirici_Vg_P6p_wt_mod_2/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Oonirici_P6p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Oonirici_P6p_wt_2 <- rbind(effectiveSize(va_liab_Oonirici_CONTROL_P6p_wt_mod_2/2),effectiveSize(va_liab_Oonirici_WILD_P6p_wt_mod/2),effectiveSize(va_liab_Oonirici_Vg_P6p_wt_mod_2/2))
        colnames(effectiveSize_va_liab_Oonirici_P6p_wt_2) <- c("effectiveSize")
        va_liab_Oonirici_P6p_wt_2 <- cbind.data.frame(mean_va_liab_Oonirici_P6p_wt_2,median_va_liab_Oonirici_P6p_wt_2,posterior.mode_va_liab_Oonirici_P6p_wt_2,HPDinterval_0.95_va_liab_Oonirici_P6p_wt_2,HPDinterval_0.83_va_liab_Oonirici_P6p_wt_2,effectiveSize_va_liab_Oonirici_P6p_wt_2)
        rownames(va_liab_Oonirici_P6p_wt_2) <- c("va_liab_Oonirici_CONTROL_P6p_wt_mod_2","va_liab_Oonirici_WILD_P6p_wt_mod","va_liab_Oonirici_Vg_P6p_wt_mod_2")
        va_liab_Oonirici_P6p_wt_2 <- cbind(Models = rownames(va_liab_Oonirici_P6p_wt_2),va_liab_Oonirici_P6p_wt_2)
        rownames(va_liab_Oonirici_P6p_wt_2) <- NULL
        va_liab_Oonirici_P6p_wt_2$Pnp <- c("P6.p","P6.p","P6.p")
        va_liab_Oonirici_P6p_wt_2$Treatment <- c("Control","WILD","Vg")
        va_liab_Oonirici_P6p_wt_2$Measure <- c("Va","Va","Va")
        va_liab_Oonirici_P6p_wt_2$Scale <- c("liab","liab","liab")
        va_liab_Oonirici_P6p_wt_2$Variance <- c("Vg","Vg","Vg")
        va_liab_Oonirici_P6p_wt_2
      }
      
      #Summary h2_liab_Oonirici_P6p_wt_2
      {
        mean_h2_liab_Oonirici_P6p_wt_2 <- rbind(mean(h2_liab_Oonirici_CONTROL_P6p_wt_mod_2),mean(h2_liab_Oonirici_WILD_P6p_wt_mod),mean(h2_liab_Oonirici_Vg_P6p_wt_mod_2))
        colnames(mean_h2_liab_Oonirici_P6p_wt_2) <- c("mean")
        median_h2_liab_Oonirici_P6p_wt_2 <- rbind(median(h2_liab_Oonirici_CONTROL_P6p_wt_mod_2),median(h2_liab_Oonirici_WILD_P6p_wt_mod),median(h2_liab_Oonirici_Vg_P6p_wt_mod_2))
        colnames(median_h2_liab_Oonirici_P6p_wt_2) <- c("median")
        posterior.mode_h2_liab_Oonirici_P6p_wt_2 <- rbind(posterior.mode(h2_liab_Oonirici_CONTROL_P6p_wt_mod_2),posterior.mode(h2_liab_Oonirici_WILD_P6p_wt_mod),posterior.mode(h2_liab_Oonirici_Vg_P6p_wt_mod_2))
        colnames(posterior.mode_h2_liab_Oonirici_P6p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Oonirici_P6p_wt_2 <- rbind(HPDinterval(h2_liab_Oonirici_CONTROL_P6p_wt_mod_2),HPDinterval(h2_liab_Oonirici_WILD_P6p_wt_mod),HPDinterval(h2_liab_Oonirici_Vg_P6p_wt_mod_2))
        colnames(HPDinterval_0.95_h2_liab_Oonirici_P6p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Oonirici_P6p_wt_2 <- rbind(HPDinterval(h2_liab_Oonirici_CONTROL_P6p_wt_mod_2,prob=.83),HPDinterval(h2_liab_Oonirici_WILD_P6p_wt_mod,prob=.83),HPDinterval(h2_liab_Oonirici_Vg_P6p_wt_mod_2,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Oonirici_P6p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Oonirici_P6p_wt_2 <- rbind(effectiveSize(h2_liab_Oonirici_CONTROL_P6p_wt_mod_2),effectiveSize(h2_liab_Oonirici_WILD_P6p_wt_mod),effectiveSize(h2_liab_Oonirici_Vg_P6p_wt_mod_2))
        colnames(effectiveSize_h2_liab_Oonirici_P6p_wt_2) <- c("effectiveSize")
        h2_liab_Oonirici_P6p_wt_2 <- cbind.data.frame(mean_h2_liab_Oonirici_P6p_wt_2,median_h2_liab_Oonirici_P6p_wt_2,posterior.mode_h2_liab_Oonirici_P6p_wt_2,HPDinterval_0.95_h2_liab_Oonirici_P6p_wt_2,HPDinterval_0.83_h2_liab_Oonirici_P6p_wt_2,effectiveSize_h2_liab_Oonirici_P6p_wt_2)
        rownames(h2_liab_Oonirici_P6p_wt_2) <- c("h2_liab_Oonirici_CONTROL_P6p_wt_mod_2","h2_liab_Oonirici_WILD_P6p_wt_mod","h2_liab_Oonirici_Vg_P6p_wt_mod_2")
        h2_liab_Oonirici_P6p_wt_2 <- cbind(Models = rownames(h2_liab_Oonirici_P6p_wt_2),h2_liab_Oonirici_P6p_wt_2)
        rownames(h2_liab_Oonirici_P6p_wt_2) <- NULL
        h2_liab_Oonirici_P6p_wt_2$Pnp <- c("P6.p","P6.p","P6.p")
        h2_liab_Oonirici_P6p_wt_2$Treatment <- c("Control","WILD","Vg")
        h2_liab_Oonirici_P6p_wt_2$Measure <- c("H2","H2","H2")
        h2_liab_Oonirici_P6p_wt_2$Scale <- c("liab","liab","liab")
        h2_liab_Oonirici_P6p_wt_2$Variance <- c("Vg","Vg","Vg")
        h2_liab_Oonirici_P6p_wt_2
      }
      
      #Summary Evol_liab_Oonirici_P6p_wt_2
      {
        mean_Evol_liab_Oonirici_P6p_wt_2 <- rbind(mean(Evol_liab_Oonirici_CONTROL_P6p_wt_mod_2),mean(Evol_liab_Oonirici_WILD_P6p_wt_mod),mean(Evol_liab_Oonirici_Vg_P6p_wt_mod_2))
        colnames(mean_Evol_liab_Oonirici_P6p_wt_2) <- c("mean")
        median_Evol_liab_Oonirici_P6p_wt_2 <- rbind(median(Evol_liab_Oonirici_CONTROL_P6p_wt_mod_2),median(Evol_liab_Oonirici_WILD_P6p_wt_mod),median(Evol_liab_Oonirici_Vg_P6p_wt_mod_2))
        colnames(median_Evol_liab_Oonirici_P6p_wt_2) <- c("median")
        posterior.mode_Evol_liab_Oonirici_P6p_wt_2 <- rbind(posterior.mode(Evol_liab_Oonirici_CONTROL_P6p_wt_mod_2),posterior.mode(Evol_liab_Oonirici_WILD_P6p_wt_mod),posterior.mode(Evol_liab_Oonirici_Vg_P6p_wt_mod_2))
        colnames(posterior.mode_Evol_liab_Oonirici_P6p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Oonirici_P6p_wt_2 <- rbind(HPDinterval(Evol_liab_Oonirici_CONTROL_P6p_wt_mod_2),HPDinterval(Evol_liab_Oonirici_WILD_P6p_wt_mod),HPDinterval(Evol_liab_Oonirici_Vg_P6p_wt_mod_2))
        colnames(HPDinterval_0.95_Evol_liab_Oonirici_P6p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Oonirici_P6p_wt_2 <- rbind(HPDinterval(Evol_liab_Oonirici_CONTROL_P6p_wt_mod_2,prob=.83),HPDinterval(Evol_liab_Oonirici_WILD_P6p_wt_mod,prob=.83),HPDinterval(Evol_liab_Oonirici_Vg_P6p_wt_mod_2,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Oonirici_P6p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Oonirici_P6p_wt_2 <- rbind(effectiveSize(Evol_liab_Oonirici_CONTROL_P6p_wt_mod_2),effectiveSize(Evol_liab_Oonirici_WILD_P6p_wt_mod),effectiveSize(Evol_liab_Oonirici_Vg_P6p_wt_mod_2))
        colnames(effectiveSize_Evol_liab_Oonirici_P6p_wt_2) <- c("effectiveSize")
        Evol_liab_Oonirici_P6p_wt_2 <- cbind.data.frame(mean_Evol_liab_Oonirici_P6p_wt_2,median_Evol_liab_Oonirici_P6p_wt_2,posterior.mode_Evol_liab_Oonirici_P6p_wt_2,HPDinterval_0.95_Evol_liab_Oonirici_P6p_wt_2,HPDinterval_0.83_Evol_liab_Oonirici_P6p_wt_2,effectiveSize_Evol_liab_Oonirici_P6p_wt_2)
        rownames(Evol_liab_Oonirici_P6p_wt_2) <- c("Evol_liab_Oonirici_CONTROL_P6p_wt_mod_2","Evol_liab_Oonirici_WILD_P6p_wt_mod","Evol_liab_Oonirici_Vg_P6p_wt_mod_2")
        Evol_liab_Oonirici_P6p_wt_2 <- cbind(Models = rownames(Evol_liab_Oonirici_P6p_wt_2),Evol_liab_Oonirici_P6p_wt_2)
        rownames(Evol_liab_Oonirici_P6p_wt_2) <- NULL
        Evol_liab_Oonirici_P6p_wt_2$Pnp <- c("P6.p","P6.p","P6.p")
        Evol_liab_Oonirici_P6p_wt_2$Treatment <- c("Control","WILD","Vg")
        Evol_liab_Oonirici_P6p_wt_2$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Oonirici_P6p_wt_2$Scale <- c("liab","liab","liab")
        Evol_liab_Oonirici_P6p_wt_2$Variance <- c("Vg","Vg","Vg")
        Evol_liab_Oonirici_P6p_wt_2
      }
      
      #Summary trait_mean_liab_Oonirici_P6p_wt_2
      {
        mean_trait_mean_liab_Oonirici_P6p_wt_2 <- rbind(mean(trait_mean_liab_Oonirici_CONTROL_P6p_wt_mod_2),mean(trait_mean_liab_Oonirici_WILD_P6p_wt_mod),mean(trait_mean_liab_Oonirici_Vg_P6p_wt_mod_2))
        colnames(mean_trait_mean_liab_Oonirici_P6p_wt_2) <- c("mean")
        median_trait_mean_liab_Oonirici_P6p_wt_2 <- rbind(median(trait_mean_liab_Oonirici_CONTROL_P6p_wt_mod_2),median(trait_mean_liab_Oonirici_WILD_P6p_wt_mod),median(trait_mean_liab_Oonirici_Vg_P6p_wt_mod_2))
        colnames(median_trait_mean_liab_Oonirici_P6p_wt_2) <- c("median")
        posterior.mode_trait_mean_liab_Oonirici_P6p_wt_2 <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P6p_wt_mod_2)),posterior.mode(as.mcmc(trait_mean_liab_Oonirici_WILD_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Oonirici_Vg_P6p_wt_mod_2)))
        colnames(posterior.mode_trait_mean_liab_Oonirici_P6p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Oonirici_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P6p_wt_mod_2)),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_WILD_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_Vg_P6p_wt_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_liab_Oonirici_P6p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Oonirici_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P6p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_WILD_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_Vg_P6p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Oonirici_P6p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Oonirici_P6p_wt_2 <- rbind(effectiveSize(trait_mean_liab_Oonirici_CONTROL_P6p_wt_mod_2),effectiveSize(trait_mean_liab_Oonirici_WILD_P6p_wt_mod),effectiveSize(trait_mean_liab_Oonirici_Vg_P6p_wt_mod_2))
        colnames(effectiveSize_trait_mean_liab_Oonirici_P6p_wt_2) <- c("effectiveSize")
        trait_mean_liab_Oonirici_P6p_wt_2 <- cbind.data.frame(mean_trait_mean_liab_Oonirici_P6p_wt_2,median_trait_mean_liab_Oonirici_P6p_wt_2,posterior.mode_trait_mean_liab_Oonirici_P6p_wt_2,HPDinterval_0.95_trait_mean_liab_Oonirici_P6p_wt_2,HPDinterval_0.83_trait_mean_liab_Oonirici_P6p_wt_2,effectiveSize_trait_mean_liab_Oonirici_P6p_wt_2)
        rownames(trait_mean_liab_Oonirici_P6p_wt_2) <- c("trait_mean_liab_Oonirici_CONTROL_P6p_wt_mod_2","trait_mean_liab_Oonirici_WILD_P6p_wt_mod","trait_mean_liab_Oonirici_Vg_P6p_wt_mod_2")
        trait_mean_liab_Oonirici_P6p_wt_2 <- cbind(Models = rownames(trait_mean_liab_Oonirici_P6p_wt_2),trait_mean_liab_Oonirici_P6p_wt_2)
        rownames(trait_mean_liab_Oonirici_P6p_wt_2) <- NULL
        trait_mean_liab_Oonirici_P6p_wt_2$Pnp <- c("P6.p","P6.p","P6.p")
        trait_mean_liab_Oonirici_P6p_wt_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_liab_Oonirici_P6p_wt_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Oonirici_P6p_wt_2$Scale <- c("liab","liab","liab")
        trait_mean_liab_Oonirici_P6p_wt_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_liab_Oonirici_P6p_wt_2
      }
      
      liab_Oonirici_P6p_wt_2 <- rbind.data.frame(va_liab_Oonirici_P6p_wt_2, h2_liab_Oonirici_P6p_wt_2,Evol_liab_Oonirici_P6p_wt_2,trait_mean_liab_Oonirici_P6p_wt_2)
      liab_Oonirici_P6p_wt_2
    }
    #Summary data scale Oonirici P6p
    {
      #Summary va_data_Oonirici_P6p_wt_2:NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        mean_va_data_Oonirici_P6p_wt_2 <- rbind(mean(va_data_Oonirici_CONTROL_P6p_wt_mod_2/2),mean(va_data_Oonirici_WILD_P6p_wt_mod/2),mean(va_data_Oonirici_Vg_P6p_wt_mod_2/2))
        colnames(mean_va_data_Oonirici_P6p_wt_2) <- c("mean")
        median_va_data_Oonirici_P6p_wt_2 <- rbind(median(va_data_Oonirici_CONTROL_P6p_wt_mod_2/2),median(va_data_Oonirici_WILD_P6p_wt_mod/2),median(va_data_Oonirici_Vg_P6p_wt_mod_2/2))
        colnames(median_va_data_Oonirici_P6p_wt_2) <- c("median")
        posterior.mode_va_data_Oonirici_P6p_wt_2 <- rbind(posterior.mode(as.mcmc(va_data_Oonirici_CONTROL_P6p_wt_mod_2/2)),posterior.mode(as.mcmc(va_data_Oonirici_WILD_P6p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Oonirici_Vg_P6p_wt_mod_2/2)))
        colnames(posterior.mode_va_data_Oonirici_P6p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Oonirici_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(va_data_Oonirici_CONTROL_P6p_wt_mod_2/2)),HPDinterval(as.mcmc(va_data_Oonirici_WILD_P6p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Oonirici_Vg_P6p_wt_mod_2/2)))
        colnames(HPDinterval_0.95_va_data_Oonirici_P6p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Oonirici_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(va_data_Oonirici_CONTROL_P6p_wt_mod_2/2),prob=.83),HPDinterval(as.mcmc(va_data_Oonirici_WILD_P6p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Oonirici_Vg_P6p_wt_mod_2/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Oonirici_P6p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Oonirici_P6p_wt_2 <- rbind(effectiveSize(va_data_Oonirici_CONTROL_P6p_wt_mod_2/2),effectiveSize(va_data_Oonirici_WILD_P6p_wt_mod/2),effectiveSize(va_data_Oonirici_Vg_P6p_wt_mod_2/2))
        colnames(effectiveSize_va_data_Oonirici_P6p_wt_2) <- c("effectiveSize")
        va_data_Oonirici_P6p_wt_2 <- cbind.data.frame(mean_va_data_Oonirici_P6p_wt_2,median_va_data_Oonirici_P6p_wt_2,posterior.mode_va_data_Oonirici_P6p_wt_2,HPDinterval_0.95_va_data_Oonirici_P6p_wt_2,HPDinterval_0.83_va_data_Oonirici_P6p_wt_2,effectiveSize_va_data_Oonirici_P6p_wt_2)
        rownames(va_data_Oonirici_P6p_wt_2) <- c("va_data_Oonirici_CONTROL_P6p_wt_mod_2","va_data_Oonirici_WILD_P6p_wt_mod","va_data_Oonirici_Vg_P6p_wt_mod_2")
        va_data_Oonirici_P6p_wt_2 <- cbind(Models = rownames(va_data_Oonirici_P6p_wt_2),va_data_Oonirici_P6p_wt_2)
        rownames(va_data_Oonirici_P6p_wt_2) <- NULL
        va_data_Oonirici_P6p_wt_2$Pnp <- c("P6.p","P6.p","P6.p")
        va_data_Oonirici_P6p_wt_2$Treatment <- c("Control","WILD","Vg")
        va_data_Oonirici_P6p_wt_2$Measure <- c("Va","Va","Va")
        va_data_Oonirici_P6p_wt_2$Scale <- c("data","data","data")
        va_data_Oonirici_P6p_wt_2$Variance <- c("Vg","Vg","Vg")
        va_data_Oonirici_P6p_wt_2
      }
      
      #Summary h2_data_Oonirici_P6p_wt_2
      {
        mean_h2_data_Oonirici_P6p_wt_2 <- rbind(mean(h2_data_Oonirici_CONTROL_P6p_wt_mod_2),mean(h2_data_Oonirici_WILD_P6p_wt_mod),mean(h2_data_Oonirici_Vg_P6p_wt_mod_2))
        colnames(mean_h2_data_Oonirici_P6p_wt_2) <- c("mean")
        median_h2_data_Oonirici_P6p_wt_2 <- rbind(median(h2_data_Oonirici_CONTROL_P6p_wt_mod_2),median(h2_data_Oonirici_WILD_P6p_wt_mod),median(h2_data_Oonirici_Vg_P6p_wt_mod_2))
        colnames(median_h2_data_Oonirici_P6p_wt_2) <- c("median")
        posterior.mode_h2_data_Oonirici_P6p_wt_2 <- rbind(posterior.mode(as.mcmc(h2_data_Oonirici_CONTROL_P6p_wt_mod_2)),posterior.mode(as.mcmc(h2_data_Oonirici_WILD_P6p_wt_mod)),posterior.mode(as.mcmc(h2_data_Oonirici_Vg_P6p_wt_mod_2)))
        colnames(posterior.mode_h2_data_Oonirici_P6p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Oonirici_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(h2_data_Oonirici_CONTROL_P6p_wt_mod_2)),HPDinterval(as.mcmc(h2_data_Oonirici_WILD_P6p_wt_mod)),HPDinterval(as.mcmc(h2_data_Oonirici_Vg_P6p_wt_mod_2)))
        colnames(HPDinterval_0.95_h2_data_Oonirici_P6p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Oonirici_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(h2_data_Oonirici_CONTROL_P6p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(h2_data_Oonirici_WILD_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Oonirici_Vg_P6p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Oonirici_P6p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Oonirici_P6p_wt_2 <- rbind(effectiveSize(h2_data_Oonirici_CONTROL_P6p_wt_mod_2),effectiveSize(h2_data_Oonirici_WILD_P6p_wt_mod),effectiveSize(h2_data_Oonirici_Vg_P6p_wt_mod_2))
        colnames(effectiveSize_h2_data_Oonirici_P6p_wt_2) <- c("effectiveSize")
        h2_data_Oonirici_P6p_wt_2 <- cbind.data.frame(mean_h2_data_Oonirici_P6p_wt_2,median_h2_data_Oonirici_P6p_wt_2,posterior.mode_h2_data_Oonirici_P6p_wt_2,HPDinterval_0.95_h2_data_Oonirici_P6p_wt_2,HPDinterval_0.83_h2_data_Oonirici_P6p_wt_2,effectiveSize_h2_data_Oonirici_P6p_wt_2)
        rownames(h2_data_Oonirici_P6p_wt_2) <- c("h2_data_Oonirici_CONTROL_P6p_wt_mod_2","h2_data_Oonirici_WILD_P6p_wt_mod","h2_data_Oonirici_Vg_P6p_wt_mod_2")
        h2_data_Oonirici_P6p_wt_2 <- cbind(Models = rownames(h2_data_Oonirici_P6p_wt_2),h2_data_Oonirici_P6p_wt_2)
        rownames(h2_data_Oonirici_P6p_wt_2) <- NULL
        h2_data_Oonirici_P6p_wt_2$Pnp <- c("P6.p","P6.p","P6.p")
        h2_data_Oonirici_P6p_wt_2$Treatment <- c("Control","WILD","Vg")
        h2_data_Oonirici_P6p_wt_2$Measure <- c("H2","H2","H2")
        h2_data_Oonirici_P6p_wt_2$Scale <- c("data","data","data")
        h2_data_Oonirici_P6p_wt_2$Variance <- c("Vg","Vg","Vg")
        h2_data_Oonirici_P6p_wt_2
      }
      
      #Summary Evol_data_Oonirici_P6p_wt_2
      {
        mean_Evol_data_Oonirici_P6p_wt_2 <- rbind(mean(Evol_data_Oonirici_CONTROL_P6p_wt_mod_2),mean(Evol_data_Oonirici_WILD_P6p_wt_mod),mean(Evol_data_Oonirici_Vg_P6p_wt_mod_2))
        colnames(mean_Evol_data_Oonirici_P6p_wt_2) <- c("mean")
        median_Evol_data_Oonirici_P6p_wt_2 <- rbind(median(Evol_data_Oonirici_CONTROL_P6p_wt_mod_2),median(Evol_data_Oonirici_WILD_P6p_wt_mod),median(Evol_data_Oonirici_Vg_P6p_wt_mod_2))
        colnames(median_Evol_data_Oonirici_P6p_wt_2) <- c("median")
        posterior.mode_Evol_data_Oonirici_P6p_wt_2 <- rbind(posterior.mode(as.mcmc(Evol_data_Oonirici_CONTROL_P6p_wt_mod_2)),posterior.mode(as.mcmc(Evol_data_Oonirici_WILD_P6p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Oonirici_Vg_P6p_wt_mod_2)))
        colnames(posterior.mode_Evol_data_Oonirici_P6p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Oonirici_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Oonirici_CONTROL_P6p_wt_mod_2)),HPDinterval(as.mcmc(Evol_data_Oonirici_WILD_P6p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Oonirici_Vg_P6p_wt_mod_2)))
        colnames(HPDinterval_0.95_Evol_data_Oonirici_P6p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Oonirici_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Oonirici_CONTROL_P6p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(Evol_data_Oonirici_WILD_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Oonirici_Vg_P6p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Oonirici_P6p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Oonirici_P6p_wt_2 <- rbind(effectiveSize(Evol_data_Oonirici_CONTROL_P6p_wt_mod_2),effectiveSize(Evol_data_Oonirici_WILD_P6p_wt_mod),effectiveSize(Evol_data_Oonirici_Vg_P6p_wt_mod_2))
        colnames(effectiveSize_Evol_data_Oonirici_P6p_wt_2) <- c("effectiveSize")
        Evol_data_Oonirici_P6p_wt_2 <- cbind.data.frame(mean_Evol_data_Oonirici_P6p_wt_2,median_Evol_data_Oonirici_P6p_wt_2,posterior.mode_Evol_data_Oonirici_P6p_wt_2,HPDinterval_0.95_Evol_data_Oonirici_P6p_wt_2,HPDinterval_0.83_Evol_data_Oonirici_P6p_wt_2,effectiveSize_Evol_data_Oonirici_P6p_wt_2)
        rownames(Evol_data_Oonirici_P6p_wt_2) <- c("Evol_data_Oonirici_CONTROL_P6p_wt_mod_2","Evol_data_Oonirici_WILD_P6p_wt_mod","Evol_data_Oonirici_Vg_P6p_wt_mod_2")
        Evol_data_Oonirici_P6p_wt_2 <- cbind(Models = rownames(Evol_data_Oonirici_P6p_wt_2),Evol_data_Oonirici_P6p_wt_2)
        rownames(Evol_data_Oonirici_P6p_wt_2) <- NULL
        Evol_data_Oonirici_P6p_wt_2$Pnp <- c("P6.p","P6.p","P6.p")
        Evol_data_Oonirici_P6p_wt_2$Treatment <- c("Control","WILD","Vg")
        Evol_data_Oonirici_P6p_wt_2$Measure <- c("Evol","Evol","Evol")
        Evol_data_Oonirici_P6p_wt_2$Scale <- c("data","data","data")
        Evol_data_Oonirici_P6p_wt_2$Variance <- c("Vg","Vg","Vg")
        Evol_data_Oonirici_P6p_wt_2
      }
      
      #Summary trait_mean_data_Oonirici_P6p_wt_2
      {
        mean_trait_mean_data_Oonirici_P6p_wt_2 <- rbind(mean(trait_mean_data_Oonirici_CONTROL_P6p_wt_mod_2),mean(trait_mean_data_Oonirici_WILD_P6p_wt_mod),mean(trait_mean_data_Oonirici_Vg_P6p_wt_mod_2))
        colnames(mean_trait_mean_data_Oonirici_P6p_wt_2) <- c("mean")
        median_trait_mean_data_Oonirici_P6p_wt_2 <- rbind(median(trait_mean_data_Oonirici_CONTROL_P6p_wt_mod_2),median(trait_mean_data_Oonirici_WILD_P6p_wt_mod),median(trait_mean_data_Oonirici_Vg_P6p_wt_mod_2))
        colnames(median_trait_mean_data_Oonirici_P6p_wt_2) <- c("median")
        posterior.mode_trait_mean_data_Oonirici_P6p_wt_2 <- rbind(posterior.mode(as.mcmc(trait_mean_data_Oonirici_CONTROL_P6p_wt_mod_2)),posterior.mode(as.mcmc(trait_mean_data_Oonirici_WILD_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Oonirici_Vg_P6p_wt_mod_2)))
        colnames(posterior.mode_trait_mean_data_Oonirici_P6p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Oonirici_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Oonirici_CONTROL_P6p_wt_mod_2)),HPDinterval(as.mcmc(trait_mean_data_Oonirici_WILD_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Oonirici_Vg_P6p_wt_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_data_Oonirici_P6p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Oonirici_P6p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Oonirici_CONTROL_P6p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Oonirici_WILD_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Oonirici_Vg_P6p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Oonirici_P6p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Oonirici_P6p_wt_2 <- rbind(effectiveSize(trait_mean_data_Oonirici_CONTROL_P6p_wt_mod_2),effectiveSize(trait_mean_data_Oonirici_WILD_P6p_wt_mod),effectiveSize(trait_mean_data_Oonirici_Vg_P6p_wt_mod_2))
        colnames(effectiveSize_trait_mean_data_Oonirici_P6p_wt_2) <- c("effectiveSize")
        trait_mean_data_Oonirici_P6p_wt_2 <- cbind.data.frame(mean_trait_mean_data_Oonirici_P6p_wt_2,median_trait_mean_data_Oonirici_P6p_wt_2,posterior.mode_trait_mean_data_Oonirici_P6p_wt_2,HPDinterval_0.95_trait_mean_data_Oonirici_P6p_wt_2,HPDinterval_0.83_trait_mean_data_Oonirici_P6p_wt_2,effectiveSize_trait_mean_data_Oonirici_P6p_wt_2)
        rownames(trait_mean_data_Oonirici_P6p_wt_2) <- c("trait_mean_data_Oonirici_CONTROL_P6p_wt_mod_2","trait_mean_data_Oonirici_WILD_P6p_wt_mod","trait_mean_data_Oonirici_Vg_P6p_wt_mod_2")
        trait_mean_data_Oonirici_P6p_wt_2 <- cbind(Models = rownames(trait_mean_data_Oonirici_P6p_wt_2),trait_mean_data_Oonirici_P6p_wt_2)
        rownames(trait_mean_data_Oonirici_P6p_wt_2) <- NULL
        trait_mean_data_Oonirici_P6p_wt_2$Pnp <- c("P6.p","P6.p","P6.p")
        trait_mean_data_Oonirici_P6p_wt_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_data_Oonirici_P6p_wt_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Oonirici_P6p_wt_2$Scale <- c("data","data","data")
        trait_mean_data_Oonirici_P6p_wt_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_data_Oonirici_P6p_wt_2
      }
      
      data_Oonirici_P6p_wt_2 <- rbind.data.frame(va_data_Oonirici_P6p_wt_2, h2_data_Oonirici_P6p_wt_2,Evol_data_Oonirici_P6p_wt_2,trait_mean_data_Oonirici_P6p_wt_2)
      data_Oonirici_P6p_wt_2
      
    }
    Vg_Oonirici_P6p_wt_2 <- rbind.data.frame(liab_Oonirici_P6p_wt_2, data_Oonirici_P6p_wt_2)
    Vg_Oonirici_P6p_wt_2$Pnp_fate <- rep("wt", 24)
    Vg_Oonirici_P6p_wt_2
    #remove Oonirici P6p_wt_2 models
    {
      remove(Oonirici_CONTROL_P6p_wt_mod_2)
      remove(Oonirici_WILD_P6p_wt_mod)
      remove(Oonirici_Vg_P6p_wt_mod_2)
    }
  }
  
  ##Summary Oonirici P7p----
  {
    #Summary liability scale Oonirici P7p
    {
      #Summary va_liab_Oonirici_P7p_wt_2: NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        
        mean_va_liab_Oonirici_P7p_wt_2 <- rbind(mean(va_liab_Oonirici_CONTROL_P7p_wt_mod_2/2),mean(va_liab_Oonirici_WILD_P7p_wt_mod/2),mean(va_liab_Oonirici_Vg_P7p_wt_mod_2/2))
        colnames(mean_va_liab_Oonirici_P7p_wt_2) <- c("mean")
        median_va_liab_Oonirici_P7p_wt_2 <- rbind(median(va_liab_Oonirici_CONTROL_P7p_wt_mod_2/2),median(va_liab_Oonirici_WILD_P7p_wt_mod/2),median(va_liab_Oonirici_Vg_P7p_wt_mod_2/2))
        colnames(median_va_liab_Oonirici_P7p_wt_2) <- c("median")
        posterior.mode_va_liab_Oonirici_P7p_wt_2 <- rbind(posterior.mode(va_liab_Oonirici_CONTROL_P7p_wt_mod_2/2),posterior.mode(va_liab_Oonirici_WILD_P7p_wt_mod/2),posterior.mode(va_liab_Oonirici_Vg_P7p_wt_mod_2/2))
        colnames(posterior.mode_va_liab_Oonirici_P7p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Oonirici_P7p_wt_2 <- rbind(HPDinterval(va_liab_Oonirici_CONTROL_P7p_wt_mod_2/2),HPDinterval(va_liab_Oonirici_WILD_P7p_wt_mod/2),HPDinterval(va_liab_Oonirici_Vg_P7p_wt_mod_2/2))
        colnames(HPDinterval_0.95_va_liab_Oonirici_P7p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Oonirici_P7p_wt_2 <- rbind(HPDinterval(va_liab_Oonirici_CONTROL_P7p_wt_mod_2/2,prob=.83),HPDinterval(va_liab_Oonirici_WILD_P7p_wt_mod/2,prob=.83),HPDinterval(va_liab_Oonirici_Vg_P7p_wt_mod_2/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Oonirici_P7p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Oonirici_P7p_wt_2 <- rbind(effectiveSize(va_liab_Oonirici_CONTROL_P7p_wt_mod_2/2),effectiveSize(va_liab_Oonirici_WILD_P7p_wt_mod/2),effectiveSize(va_liab_Oonirici_Vg_P7p_wt_mod_2/2))
        colnames(effectiveSize_va_liab_Oonirici_P7p_wt_2) <- c("effectiveSize")
        va_liab_Oonirici_P7p_wt_2 <- cbind.data.frame(mean_va_liab_Oonirici_P7p_wt_2,median_va_liab_Oonirici_P7p_wt_2,posterior.mode_va_liab_Oonirici_P7p_wt_2,HPDinterval_0.95_va_liab_Oonirici_P7p_wt_2,HPDinterval_0.83_va_liab_Oonirici_P7p_wt_2,effectiveSize_va_liab_Oonirici_P7p_wt_2)
        rownames(va_liab_Oonirici_P7p_wt_2) <- c("va_liab_Oonirici_CONTROL_P7p_wt_mod_2","va_liab_Oonirici_WILD_P7p_wt_mod","va_liab_Oonirici_Vg_P7p_wt_mod_2")
        va_liab_Oonirici_P7p_wt_2 <- cbind(Models = rownames(va_liab_Oonirici_P7p_wt_2),va_liab_Oonirici_P7p_wt_2)
        rownames(va_liab_Oonirici_P7p_wt_2) <- NULL
        va_liab_Oonirici_P7p_wt_2$Pnp <- c("P7.p","P7.p","P7.p")
        va_liab_Oonirici_P7p_wt_2$Treatment <- c("Control","WILD","Vg")
        va_liab_Oonirici_P7p_wt_2$Measure <- c("Va","Va","Va")
        va_liab_Oonirici_P7p_wt_2$Scale <- c("liab","liab","liab")
        va_liab_Oonirici_P7p_wt_2$Variance <- c("Vg","Vg","Vg")
        va_liab_Oonirici_P7p_wt_2
      }
      
      #Summary h2_liab_Oonirici_P7p_wt_2
      {
        mean_h2_liab_Oonirici_P7p_wt_2 <- rbind(mean(h2_liab_Oonirici_CONTROL_P7p_wt_mod_2),mean(h2_liab_Oonirici_WILD_P7p_wt_mod),mean(h2_liab_Oonirici_Vg_P7p_wt_mod_2))
        colnames(mean_h2_liab_Oonirici_P7p_wt_2) <- c("mean")
        median_h2_liab_Oonirici_P7p_wt_2 <- rbind(median(h2_liab_Oonirici_CONTROL_P7p_wt_mod_2),median(h2_liab_Oonirici_WILD_P7p_wt_mod),median(h2_liab_Oonirici_Vg_P7p_wt_mod_2))
        colnames(median_h2_liab_Oonirici_P7p_wt_2) <- c("median")
        posterior.mode_h2_liab_Oonirici_P7p_wt_2 <- rbind(posterior.mode(h2_liab_Oonirici_CONTROL_P7p_wt_mod_2),posterior.mode(h2_liab_Oonirici_WILD_P7p_wt_mod),posterior.mode(h2_liab_Oonirici_Vg_P7p_wt_mod_2))
        colnames(posterior.mode_h2_liab_Oonirici_P7p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Oonirici_P7p_wt_2 <- rbind(HPDinterval(h2_liab_Oonirici_CONTROL_P7p_wt_mod_2),HPDinterval(h2_liab_Oonirici_WILD_P7p_wt_mod),HPDinterval(h2_liab_Oonirici_Vg_P7p_wt_mod_2))
        colnames(HPDinterval_0.95_h2_liab_Oonirici_P7p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Oonirici_P7p_wt_2 <- rbind(HPDinterval(h2_liab_Oonirici_CONTROL_P7p_wt_mod_2,prob=.83),HPDinterval(h2_liab_Oonirici_WILD_P7p_wt_mod,prob=.83),HPDinterval(h2_liab_Oonirici_Vg_P7p_wt_mod_2,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Oonirici_P7p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Oonirici_P7p_wt_2 <- rbind(effectiveSize(h2_liab_Oonirici_CONTROL_P7p_wt_mod_2),effectiveSize(h2_liab_Oonirici_WILD_P7p_wt_mod),effectiveSize(h2_liab_Oonirici_Vg_P7p_wt_mod_2))
        colnames(effectiveSize_h2_liab_Oonirici_P7p_wt_2) <- c("effectiveSize")
        h2_liab_Oonirici_P7p_wt_2 <- cbind.data.frame(mean_h2_liab_Oonirici_P7p_wt_2,median_h2_liab_Oonirici_P7p_wt_2,posterior.mode_h2_liab_Oonirici_P7p_wt_2,HPDinterval_0.95_h2_liab_Oonirici_P7p_wt_2,HPDinterval_0.83_h2_liab_Oonirici_P7p_wt_2,effectiveSize_h2_liab_Oonirici_P7p_wt_2)
        rownames(h2_liab_Oonirici_P7p_wt_2) <- c("h2_liab_Oonirici_CONTROL_P7p_wt_mod_2","h2_liab_Oonirici_WILD_P7p_wt_mod","h2_liab_Oonirici_Vg_P7p_wt_mod_2")
        h2_liab_Oonirici_P7p_wt_2 <- cbind(Models = rownames(h2_liab_Oonirici_P7p_wt_2),h2_liab_Oonirici_P7p_wt_2)
        rownames(h2_liab_Oonirici_P7p_wt_2) <- NULL
        h2_liab_Oonirici_P7p_wt_2$Pnp <- c("P7.p","P7.p","P7.p")
        h2_liab_Oonirici_P7p_wt_2$Treatment <- c("Control","WILD","Vg")
        h2_liab_Oonirici_P7p_wt_2$Measure <- c("H2","H2","H2")
        h2_liab_Oonirici_P7p_wt_2$Scale <- c("liab","liab","liab")
        h2_liab_Oonirici_P7p_wt_2$Variance <- c("Vg","Vg","Vg")
        h2_liab_Oonirici_P7p_wt_2
      }
      
      #Summary Evol_liab_Oonirici_P7p_wt_2
      {
        mean_Evol_liab_Oonirici_P7p_wt_2 <- rbind(mean(Evol_liab_Oonirici_CONTROL_P7p_wt_mod_2),mean(Evol_liab_Oonirici_WILD_P7p_wt_mod),mean(Evol_liab_Oonirici_Vg_P7p_wt_mod_2))
        colnames(mean_Evol_liab_Oonirici_P7p_wt_2) <- c("mean")
        median_Evol_liab_Oonirici_P7p_wt_2 <- rbind(median(Evol_liab_Oonirici_CONTROL_P7p_wt_mod_2),median(Evol_liab_Oonirici_WILD_P7p_wt_mod),median(Evol_liab_Oonirici_Vg_P7p_wt_mod_2))
        colnames(median_Evol_liab_Oonirici_P7p_wt_2) <- c("median")
        posterior.mode_Evol_liab_Oonirici_P7p_wt_2 <- rbind(posterior.mode(Evol_liab_Oonirici_CONTROL_P7p_wt_mod_2),posterior.mode(Evol_liab_Oonirici_WILD_P7p_wt_mod),posterior.mode(Evol_liab_Oonirici_Vg_P7p_wt_mod_2))
        colnames(posterior.mode_Evol_liab_Oonirici_P7p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Oonirici_P7p_wt_2 <- rbind(HPDinterval(Evol_liab_Oonirici_CONTROL_P7p_wt_mod_2),HPDinterval(Evol_liab_Oonirici_WILD_P7p_wt_mod),HPDinterval(Evol_liab_Oonirici_Vg_P7p_wt_mod_2))
        colnames(HPDinterval_0.95_Evol_liab_Oonirici_P7p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Oonirici_P7p_wt_2 <- rbind(HPDinterval(Evol_liab_Oonirici_CONTROL_P7p_wt_mod_2,prob=.83),HPDinterval(Evol_liab_Oonirici_WILD_P7p_wt_mod,prob=.83),HPDinterval(Evol_liab_Oonirici_Vg_P7p_wt_mod_2,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Oonirici_P7p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Oonirici_P7p_wt_2 <- rbind(effectiveSize(Evol_liab_Oonirici_CONTROL_P7p_wt_mod_2),effectiveSize(Evol_liab_Oonirici_WILD_P7p_wt_mod),effectiveSize(Evol_liab_Oonirici_Vg_P7p_wt_mod_2))
        colnames(effectiveSize_Evol_liab_Oonirici_P7p_wt_2) <- c("effectiveSize")
        Evol_liab_Oonirici_P7p_wt_2 <- cbind.data.frame(mean_Evol_liab_Oonirici_P7p_wt_2,median_Evol_liab_Oonirici_P7p_wt_2,posterior.mode_Evol_liab_Oonirici_P7p_wt_2,HPDinterval_0.95_Evol_liab_Oonirici_P7p_wt_2,HPDinterval_0.83_Evol_liab_Oonirici_P7p_wt_2,effectiveSize_Evol_liab_Oonirici_P7p_wt_2)
        rownames(Evol_liab_Oonirici_P7p_wt_2) <- c("Evol_liab_Oonirici_CONTROL_P7p_wt_mod_2","Evol_liab_Oonirici_WILD_P7p_wt_mod","Evol_liab_Oonirici_Vg_P7p_wt_mod_2")
        Evol_liab_Oonirici_P7p_wt_2 <- cbind(Models = rownames(Evol_liab_Oonirici_P7p_wt_2),Evol_liab_Oonirici_P7p_wt_2)
        rownames(Evol_liab_Oonirici_P7p_wt_2) <- NULL
        Evol_liab_Oonirici_P7p_wt_2$Pnp <- c("P7.p","P7.p","P7.p")
        Evol_liab_Oonirici_P7p_wt_2$Treatment <- c("Control","WILD","Vg")
        Evol_liab_Oonirici_P7p_wt_2$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Oonirici_P7p_wt_2$Scale <- c("liab","liab","liab")
        Evol_liab_Oonirici_P7p_wt_2$Variance <- c("Vg","Vg","Vg")
        Evol_liab_Oonirici_P7p_wt_2
      }
      
      #Summary trait_mean_liab_Oonirici_P7p_wt_2
      {
        mean_trait_mean_liab_Oonirici_P7p_wt_2 <- rbind(mean(trait_mean_liab_Oonirici_CONTROL_P7p_wt_mod_2),mean(trait_mean_liab_Oonirici_WILD_P7p_wt_mod),mean(trait_mean_liab_Oonirici_Vg_P7p_wt_mod_2))
        colnames(mean_trait_mean_liab_Oonirici_P7p_wt_2) <- c("mean")
        median_trait_mean_liab_Oonirici_P7p_wt_2 <- rbind(median(trait_mean_liab_Oonirici_CONTROL_P7p_wt_mod_2),median(trait_mean_liab_Oonirici_WILD_P7p_wt_mod),median(trait_mean_liab_Oonirici_Vg_P7p_wt_mod_2))
        colnames(median_trait_mean_liab_Oonirici_P7p_wt_2) <- c("median")
        posterior.mode_trait_mean_liab_Oonirici_P7p_wt_2 <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P7p_wt_mod_2)),posterior.mode(as.mcmc(trait_mean_liab_Oonirici_WILD_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Oonirici_Vg_P7p_wt_mod_2)))
        colnames(posterior.mode_trait_mean_liab_Oonirici_P7p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Oonirici_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P7p_wt_mod_2)),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_WILD_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_Vg_P7p_wt_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_liab_Oonirici_P7p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Oonirici_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Oonirici_CONTROL_P7p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_WILD_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Oonirici_Vg_P7p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Oonirici_P7p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Oonirici_P7p_wt_2 <- rbind(effectiveSize(trait_mean_liab_Oonirici_CONTROL_P7p_wt_mod_2),effectiveSize(trait_mean_liab_Oonirici_WILD_P7p_wt_mod),effectiveSize(trait_mean_liab_Oonirici_Vg_P7p_wt_mod_2))
        colnames(effectiveSize_trait_mean_liab_Oonirici_P7p_wt_2) <- c("effectiveSize")
        trait_mean_liab_Oonirici_P7p_wt_2 <- cbind.data.frame(mean_trait_mean_liab_Oonirici_P7p_wt_2,median_trait_mean_liab_Oonirici_P7p_wt_2,posterior.mode_trait_mean_liab_Oonirici_P7p_wt_2,HPDinterval_0.95_trait_mean_liab_Oonirici_P7p_wt_2,HPDinterval_0.83_trait_mean_liab_Oonirici_P7p_wt_2,effectiveSize_trait_mean_liab_Oonirici_P7p_wt_2)
        rownames(trait_mean_liab_Oonirici_P7p_wt_2) <- c("trait_mean_liab_Oonirici_CONTROL_P7p_wt_mod_2","trait_mean_liab_Oonirici_WILD_P7p_wt_mod","trait_mean_liab_Oonirici_Vg_P7p_wt_mod_2")
        trait_mean_liab_Oonirici_P7p_wt_2 <- cbind(Models = rownames(trait_mean_liab_Oonirici_P7p_wt_2),trait_mean_liab_Oonirici_P7p_wt_2)
        rownames(trait_mean_liab_Oonirici_P7p_wt_2) <- NULL
        trait_mean_liab_Oonirici_P7p_wt_2$Pnp <- c("P7.p","P7.p","P7.p")
        trait_mean_liab_Oonirici_P7p_wt_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_liab_Oonirici_P7p_wt_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Oonirici_P7p_wt_2$Scale <- c("liab","liab","liab")
        trait_mean_liab_Oonirici_P7p_wt_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_liab_Oonirici_P7p_wt_2
      }
      
      liab_Oonirici_P7p_wt_2 <- rbind.data.frame(va_liab_Oonirici_P7p_wt_2, h2_liab_Oonirici_P7p_wt_2,Evol_liab_Oonirici_P7p_wt_2,trait_mean_liab_Oonirici_P7p_wt_2)
      liab_Oonirici_P7p_wt_2
    }
    #Summary data scale Oonirici P7p
    {
      #Summary va_data_Oonirici_P7p_wt_2:NB Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance.
      {
        mean_va_data_Oonirici_P7p_wt_2 <- rbind(mean(va_data_Oonirici_CONTROL_P7p_wt_mod_2/2),mean(va_data_Oonirici_WILD_P7p_wt_mod/2),mean(va_data_Oonirici_Vg_P7p_wt_mod_2/2))
        colnames(mean_va_data_Oonirici_P7p_wt_2) <- c("mean")
        median_va_data_Oonirici_P7p_wt_2 <- rbind(median(va_data_Oonirici_CONTROL_P7p_wt_mod_2/2),median(va_data_Oonirici_WILD_P7p_wt_mod/2),median(va_data_Oonirici_Vg_P7p_wt_mod_2/2))
        colnames(median_va_data_Oonirici_P7p_wt_2) <- c("median")
        posterior.mode_va_data_Oonirici_P7p_wt_2 <- rbind(posterior.mode(as.mcmc(va_data_Oonirici_CONTROL_P7p_wt_mod_2/2)),posterior.mode(as.mcmc(va_data_Oonirici_WILD_P7p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Oonirici_Vg_P7p_wt_mod_2/2)))
        colnames(posterior.mode_va_data_Oonirici_P7p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Oonirici_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(va_data_Oonirici_CONTROL_P7p_wt_mod_2/2)),HPDinterval(as.mcmc(va_data_Oonirici_WILD_P7p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Oonirici_Vg_P7p_wt_mod_2/2)))
        colnames(HPDinterval_0.95_va_data_Oonirici_P7p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Oonirici_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(va_data_Oonirici_CONTROL_P7p_wt_mod_2/2),prob=.83),HPDinterval(as.mcmc(va_data_Oonirici_WILD_P7p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Oonirici_Vg_P7p_wt_mod_2/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Oonirici_P7p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Oonirici_P7p_wt_2 <- rbind(effectiveSize(va_data_Oonirici_CONTROL_P7p_wt_mod_2/2),effectiveSize(va_data_Oonirici_WILD_P7p_wt_mod/2),effectiveSize(va_data_Oonirici_Vg_P7p_wt_mod_2/2))
        colnames(effectiveSize_va_data_Oonirici_P7p_wt_2) <- c("effectiveSize")
        va_data_Oonirici_P7p_wt_2 <- cbind.data.frame(mean_va_data_Oonirici_P7p_wt_2,median_va_data_Oonirici_P7p_wt_2,posterior.mode_va_data_Oonirici_P7p_wt_2,HPDinterval_0.95_va_data_Oonirici_P7p_wt_2,HPDinterval_0.83_va_data_Oonirici_P7p_wt_2,effectiveSize_va_data_Oonirici_P7p_wt_2)
        rownames(va_data_Oonirici_P7p_wt_2) <- c("va_data_Oonirici_CONTROL_P7p_wt_mod_2","va_data_Oonirici_WILD_P7p_wt_mod","va_data_Oonirici_Vg_P7p_wt_mod_2")
        va_data_Oonirici_P7p_wt_2 <- cbind(Models = rownames(va_data_Oonirici_P7p_wt_2),va_data_Oonirici_P7p_wt_2)
        rownames(va_data_Oonirici_P7p_wt_2) <- NULL
        va_data_Oonirici_P7p_wt_2$Pnp <- c("P7.p","P7.p","P7.p")
        va_data_Oonirici_P7p_wt_2$Treatment <- c("Control","WILD","Vg")
        va_data_Oonirici_P7p_wt_2$Measure <- c("Va","Va","Va")
        va_data_Oonirici_P7p_wt_2$Scale <- c("data","data","data")
        va_data_Oonirici_P7p_wt_2$Variance <- c("Vg","Vg","Vg")
        va_data_Oonirici_P7p_wt_2
      }
      
      #Summary h2_data_Oonirici_P7p_wt_2
      {
        mean_h2_data_Oonirici_P7p_wt_2 <- rbind(mean(h2_data_Oonirici_CONTROL_P7p_wt_mod_2),mean(h2_data_Oonirici_WILD_P7p_wt_mod),mean(h2_data_Oonirici_Vg_P7p_wt_mod_2))
        colnames(mean_h2_data_Oonirici_P7p_wt_2) <- c("mean")
        median_h2_data_Oonirici_P7p_wt_2 <- rbind(median(h2_data_Oonirici_CONTROL_P7p_wt_mod_2),median(h2_data_Oonirici_WILD_P7p_wt_mod),median(h2_data_Oonirici_Vg_P7p_wt_mod_2))
        colnames(median_h2_data_Oonirici_P7p_wt_2) <- c("median")
        posterior.mode_h2_data_Oonirici_P7p_wt_2 <- rbind(posterior.mode(as.mcmc(h2_data_Oonirici_CONTROL_P7p_wt_mod_2)),posterior.mode(as.mcmc(h2_data_Oonirici_WILD_P7p_wt_mod)),posterior.mode(as.mcmc(h2_data_Oonirici_Vg_P7p_wt_mod_2)))
        colnames(posterior.mode_h2_data_Oonirici_P7p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Oonirici_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(h2_data_Oonirici_CONTROL_P7p_wt_mod_2)),HPDinterval(as.mcmc(h2_data_Oonirici_WILD_P7p_wt_mod)),HPDinterval(as.mcmc(h2_data_Oonirici_Vg_P7p_wt_mod_2)))
        colnames(HPDinterval_0.95_h2_data_Oonirici_P7p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Oonirici_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(h2_data_Oonirici_CONTROL_P7p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(h2_data_Oonirici_WILD_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Oonirici_Vg_P7p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Oonirici_P7p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Oonirici_P7p_wt_2 <- rbind(effectiveSize(h2_data_Oonirici_CONTROL_P7p_wt_mod_2),effectiveSize(h2_data_Oonirici_WILD_P7p_wt_mod),effectiveSize(h2_data_Oonirici_Vg_P7p_wt_mod_2))
        colnames(effectiveSize_h2_data_Oonirici_P7p_wt_2) <- c("effectiveSize")
        h2_data_Oonirici_P7p_wt_2 <- cbind.data.frame(mean_h2_data_Oonirici_P7p_wt_2,median_h2_data_Oonirici_P7p_wt_2,posterior.mode_h2_data_Oonirici_P7p_wt_2,HPDinterval_0.95_h2_data_Oonirici_P7p_wt_2,HPDinterval_0.83_h2_data_Oonirici_P7p_wt_2,effectiveSize_h2_data_Oonirici_P7p_wt_2)
        rownames(h2_data_Oonirici_P7p_wt_2) <- c("h2_data_Oonirici_CONTROL_P7p_wt_mod_2","h2_data_Oonirici_WILD_P7p_wt_mod","h2_data_Oonirici_Vg_P7p_wt_mod_2")
        h2_data_Oonirici_P7p_wt_2 <- cbind(Models = rownames(h2_data_Oonirici_P7p_wt_2),h2_data_Oonirici_P7p_wt_2)
        rownames(h2_data_Oonirici_P7p_wt_2) <- NULL
        h2_data_Oonirici_P7p_wt_2$Pnp <- c("P7.p","P7.p","P7.p")
        h2_data_Oonirici_P7p_wt_2$Treatment <- c("Control","WILD","Vg")
        h2_data_Oonirici_P7p_wt_2$Measure <- c("H2","H2","H2")
        h2_data_Oonirici_P7p_wt_2$Scale <- c("data","data","data")
        h2_data_Oonirici_P7p_wt_2$Variance <- c("Vg","Vg","Vg")
        h2_data_Oonirici_P7p_wt_2
      }
      
      #Summary Evol_data_Oonirici_P7p_wt_2
      {
        mean_Evol_data_Oonirici_P7p_wt_2 <- rbind(mean(Evol_data_Oonirici_CONTROL_P7p_wt_mod_2),mean(Evol_data_Oonirici_WILD_P7p_wt_mod),mean(Evol_data_Oonirici_Vg_P7p_wt_mod_2))
        colnames(mean_Evol_data_Oonirici_P7p_wt_2) <- c("mean")
        median_Evol_data_Oonirici_P7p_wt_2 <- rbind(median(Evol_data_Oonirici_CONTROL_P7p_wt_mod_2),median(Evol_data_Oonirici_WILD_P7p_wt_mod),median(Evol_data_Oonirici_Vg_P7p_wt_mod_2))
        colnames(median_Evol_data_Oonirici_P7p_wt_2) <- c("median")
        posterior.mode_Evol_data_Oonirici_P7p_wt_2 <- rbind(posterior.mode(as.mcmc(Evol_data_Oonirici_CONTROL_P7p_wt_mod_2)),posterior.mode(as.mcmc(Evol_data_Oonirici_WILD_P7p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Oonirici_Vg_P7p_wt_mod_2)))
        colnames(posterior.mode_Evol_data_Oonirici_P7p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Oonirici_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Oonirici_CONTROL_P7p_wt_mod_2)),HPDinterval(as.mcmc(Evol_data_Oonirici_WILD_P7p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Oonirici_Vg_P7p_wt_mod_2)))
        colnames(HPDinterval_0.95_Evol_data_Oonirici_P7p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Oonirici_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(Evol_data_Oonirici_CONTROL_P7p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(Evol_data_Oonirici_WILD_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Oonirici_Vg_P7p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Oonirici_P7p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Oonirici_P7p_wt_2 <- rbind(effectiveSize(Evol_data_Oonirici_CONTROL_P7p_wt_mod_2),effectiveSize(Evol_data_Oonirici_WILD_P7p_wt_mod),effectiveSize(Evol_data_Oonirici_Vg_P7p_wt_mod_2))
        colnames(effectiveSize_Evol_data_Oonirici_P7p_wt_2) <- c("effectiveSize")
        Evol_data_Oonirici_P7p_wt_2 <- cbind.data.frame(mean_Evol_data_Oonirici_P7p_wt_2,median_Evol_data_Oonirici_P7p_wt_2,posterior.mode_Evol_data_Oonirici_P7p_wt_2,HPDinterval_0.95_Evol_data_Oonirici_P7p_wt_2,HPDinterval_0.83_Evol_data_Oonirici_P7p_wt_2,effectiveSize_Evol_data_Oonirici_P7p_wt_2)
        rownames(Evol_data_Oonirici_P7p_wt_2) <- c("Evol_data_Oonirici_CONTROL_P7p_wt_mod_2","Evol_data_Oonirici_WILD_P7p_wt_mod","Evol_data_Oonirici_Vg_P7p_wt_mod_2")
        Evol_data_Oonirici_P7p_wt_2 <- cbind(Models = rownames(Evol_data_Oonirici_P7p_wt_2),Evol_data_Oonirici_P7p_wt_2)
        rownames(Evol_data_Oonirici_P7p_wt_2) <- NULL
        Evol_data_Oonirici_P7p_wt_2$Pnp <- c("P7.p","P7.p","P7.p")
        Evol_data_Oonirici_P7p_wt_2$Treatment <- c("Control","WILD","Vg")
        Evol_data_Oonirici_P7p_wt_2$Measure <- c("Evol","Evol","Evol")
        Evol_data_Oonirici_P7p_wt_2$Scale <- c("data","data","data")
        Evol_data_Oonirici_P7p_wt_2$Variance <- c("Vg","Vg","Vg")
        Evol_data_Oonirici_P7p_wt_2
      }
      
      #Summary trait_mean_data_Oonirici_P7p_wt_2
      {
        mean_trait_mean_data_Oonirici_P7p_wt_2 <- rbind(mean(trait_mean_data_Oonirici_CONTROL_P7p_wt_mod_2),mean(trait_mean_data_Oonirici_WILD_P7p_wt_mod),mean(trait_mean_data_Oonirici_Vg_P7p_wt_mod_2))
        colnames(mean_trait_mean_data_Oonirici_P7p_wt_2) <- c("mean")
        median_trait_mean_data_Oonirici_P7p_wt_2 <- rbind(median(trait_mean_data_Oonirici_CONTROL_P7p_wt_mod_2),median(trait_mean_data_Oonirici_WILD_P7p_wt_mod),median(trait_mean_data_Oonirici_Vg_P7p_wt_mod_2))
        colnames(median_trait_mean_data_Oonirici_P7p_wt_2) <- c("median")
        posterior.mode_trait_mean_data_Oonirici_P7p_wt_2 <- rbind(posterior.mode(as.mcmc(trait_mean_data_Oonirici_CONTROL_P7p_wt_mod_2)),posterior.mode(as.mcmc(trait_mean_data_Oonirici_WILD_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Oonirici_Vg_P7p_wt_mod_2)))
        colnames(posterior.mode_trait_mean_data_Oonirici_P7p_wt_2) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Oonirici_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Oonirici_CONTROL_P7p_wt_mod_2)),HPDinterval(as.mcmc(trait_mean_data_Oonirici_WILD_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Oonirici_Vg_P7p_wt_mod_2)))
        colnames(HPDinterval_0.95_trait_mean_data_Oonirici_P7p_wt_2) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Oonirici_P7p_wt_2 <- rbind(HPDinterval(as.mcmc(trait_mean_data_Oonirici_CONTROL_P7p_wt_mod_2),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Oonirici_WILD_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Oonirici_Vg_P7p_wt_mod_2),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Oonirici_P7p_wt_2) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Oonirici_P7p_wt_2 <- rbind(effectiveSize(trait_mean_data_Oonirici_CONTROL_P7p_wt_mod_2),effectiveSize(trait_mean_data_Oonirici_WILD_P7p_wt_mod),effectiveSize(trait_mean_data_Oonirici_Vg_P7p_wt_mod_2))
        colnames(effectiveSize_trait_mean_data_Oonirici_P7p_wt_2) <- c("effectiveSize")
        trait_mean_data_Oonirici_P7p_wt_2 <- cbind.data.frame(mean_trait_mean_data_Oonirici_P7p_wt_2,median_trait_mean_data_Oonirici_P7p_wt_2,posterior.mode_trait_mean_data_Oonirici_P7p_wt_2,HPDinterval_0.95_trait_mean_data_Oonirici_P7p_wt_2,HPDinterval_0.83_trait_mean_data_Oonirici_P7p_wt_2,effectiveSize_trait_mean_data_Oonirici_P7p_wt_2)
        rownames(trait_mean_data_Oonirici_P7p_wt_2) <- c("trait_mean_data_Oonirici_CONTROL_P7p_wt_mod_2","trait_mean_data_Oonirici_WILD_P7p_wt_mod","trait_mean_data_Oonirici_Vg_P7p_wt_mod_2")
        trait_mean_data_Oonirici_P7p_wt_2 <- cbind(Models = rownames(trait_mean_data_Oonirici_P7p_wt_2),trait_mean_data_Oonirici_P7p_wt_2)
        rownames(trait_mean_data_Oonirici_P7p_wt_2) <- NULL
        trait_mean_data_Oonirici_P7p_wt_2$Pnp <- c("P7.p","P7.p","P7.p")
        trait_mean_data_Oonirici_P7p_wt_2$Treatment <- c("Control","WILD","Vg")
        trait_mean_data_Oonirici_P7p_wt_2$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Oonirici_P7p_wt_2$Scale <- c("data","data","data")
        trait_mean_data_Oonirici_P7p_wt_2$Variance <- c("Vg","Vg","Vg")
        trait_mean_data_Oonirici_P7p_wt_2
      }
      
      data_Oonirici_P7p_wt_2 <- rbind.data.frame(va_data_Oonirici_P7p_wt_2, h2_data_Oonirici_P7p_wt_2,Evol_data_Oonirici_P7p_wt_2,trait_mean_data_Oonirici_P7p_wt_2)
      data_Oonirici_P7p_wt_2
      
    }
    Vg_Oonirici_P7p_wt_2 <- rbind.data.frame(liab_Oonirici_P7p_wt_2, data_Oonirici_P7p_wt_2)
    Vg_Oonirici_P7p_wt_2$Pnp_fate <- rep("wt", 24)
    Vg_Oonirici_P7p_wt_2
    #remove Oonirici P7p_wt_2 models
    {
      remove(Oonirici_CONTROL_P7p_wt_mod_2)
      remove(Oonirici_WILD_P7p_wt_mod)
      remove(Oonirici_Vg_P7p_wt_mod_2)
    }
  }
  
  Vg_Oonirici_summary_2 <- rbind.data.frame(Vg_Oonirici_P3p_SSSS_2,Vg_Oonirici_P4p_SSSS_2,Vg_Oonirici_P5p_wt_2,Vg_Oonirici_P6p_wt_2,Vg_Oonirici_P7p_wt_2,Vg_Oonirici_P8p_SSSS_2)
  Vg_Oonirici_summary_2$Species <- rep("O.onirici",144)
  Vg_Oonirici_summary_2$Genus <- rep("Oscheius",144)
  View(Vg_Oonirici_summary_2)
  
  
  ##Vg_Oonirici_P3p_divided_P4p_SSSS_2----
  {
    #Vg_Oonirici_P3p_divided_P4p_SSSS_2_liab
    {
      
      va_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2 <- va_liab_Oonirici_Vg_P3p_SSSS_mod_2 / va_liab_Oonirici_Vg_P4p_SSSS_mod_2
      h2_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2 <- h2_liab_Oonirici_Vg_P3p_SSSS_mod_2 / h2_liab_Oonirici_Vg_P4p_SSSS_mod_2
      Evol_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2 <- Evol_liab_Oonirici_Vg_P3p_SSSS_mod_2 / Evol_liab_Oonirici_Vg_P4p_SSSS_mod_2
      
      mean_va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2 <- rbind(mean(log10(va_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)),mean(log10(h2_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)), mean(log10(Evol_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)))
      colnames(mean_va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2) <- c("mean")
      median_va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2 <- rbind(median(log10(va_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)),median(log10(h2_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)), median(log10(Evol_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)))
      colnames(median_va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2) <- c("median")
      posterior.mode_va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2 <- rbind(posterior.mode(as.mcmc(log10(va_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2))),posterior.mode(as.mcmc(log10(h2_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2))),posterior.mode(as.mcmc(log10(Evol_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2))))
      colnames(posterior.mode_va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2) <- c("posterior.mode")
      HPDinterval_0.95_va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(log10(va_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2))),HPDinterval(as.mcmc(log10(h2_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2))),HPDinterval(as.mcmc(log10(Evol_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2))))
      colnames(HPDinterval_0.95_va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2) <- c("CI_lower_0.95","CI_upper_0.95")
      HPDinterval_0.83_va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(log10(va_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)),prob=.83),HPDinterval(as.mcmc(log10(h2_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)),prob=.83),HPDinterval(as.mcmc(log10(Evol_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)),prob=.83))
      colnames(HPDinterval_0.83_va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2) <- c("CI_lower_0.83","CI_upper_0.83")
      va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2 <- cbind.data.frame(mean_va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2,median_va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2,posterior.mode_va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2,HPDinterval_0.95_va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2,HPDinterval_0.83_va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2)
      rownames(va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2) <- c("va_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_log10","h2_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_log10","Evol_liab_Oonirici_Vg_P3p_divided_P4p_SSSS_log10")
      va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2$Measure <- c("Va","H2", "Evol")
      va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2$Species <- rep("O.onirici",3)
      va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2$Genus <- rep("Oscheius",3)
      va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2$Scale <- rep("liab",3)
      va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2
      
      pdf("Vg_va_h2_Evol_liab_P3p_divided_P4p_SSSS_2_log10_Oonirici.pdf")
      ggplot(va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2, aes(x=Measure, y= median)) +
        geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
        geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
        geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
        theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
        labs(y = "Va log10 ( P3.p / P4.p)", title = "Oonirici_log10(P3p/P4p)")
      dev.off() 
      
    }
    
    #Vg_Oonirici_P3p_divided_P4p_SSSS_2_data
    
    {
      va_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2 <- va_data_Oonirici_Vg_P3p_SSSS_mod_2 / va_data_Oonirici_Vg_P4p_SSSS_mod_2
      h2_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2 <- h2_data_Oonirici_Vg_P3p_SSSS_mod_2 / h2_data_Oonirici_Vg_P4p_SSSS_mod_2
      Evol_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2 <- Evol_data_Oonirici_Vg_P3p_SSSS_mod_2 / Evol_data_Oonirici_Vg_P4p_SSSS_mod_2
      
      mean_va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2 <- rbind(mean(log10(va_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)),mean(log10(h2_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)), mean(log10(Evol_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)))
      colnames(mean_va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2) <- c("mean")
      median_va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2 <- rbind(median(log10(va_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)),median(log10(h2_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)), median(log10(Evol_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)))
      colnames(median_va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2) <- c("median")
      posterior.mode_va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2 <- rbind(posterior.mode(as.mcmc(log10(va_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2))),posterior.mode(as.mcmc(log10(h2_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2))),posterior.mode(as.mcmc(log10(Evol_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2))))
      colnames(posterior.mode_va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2) <- c("posterior.mode")
      HPDinterval_0.95_va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(log10(va_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2))),HPDinterval(as.mcmc(log10(h2_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2))),HPDinterval(as.mcmc(log10(Evol_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2))))
      colnames(HPDinterval_0.95_va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2) <- c("CI_lower_0.95","CI_upper_0.95")
      HPDinterval_0.83_va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2 <- rbind(HPDinterval(as.mcmc(log10(va_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)),prob=.83),HPDinterval(as.mcmc(log10(h2_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)),prob=.83),HPDinterval(as.mcmc(log10(Evol_data_Oonirici_Vg_P3p_divided_P4p_SSSS_mod_2)),prob=.83))
      colnames(HPDinterval_0.83_va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2) <- c("CI_lower_0.83","CI_upper_0.83")
      va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2 <- cbind.data.frame(mean_va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2,median_va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2,posterior.mode_va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2,HPDinterval_0.95_va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2,HPDinterval_0.83_va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2)
      rownames(va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2) <- c("va_data_Oonirici_Vg_P3p_divided_P4p_SSSS_log10","h2_data_Oonirici_Vg_P3p_divided_P4p_SSSS_log10","Evol_data_Oonirici_Vg_P3p_divided_P4p_SSSS_log10")
      va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2$Measure <- c("Va","H2", "Evol")
      va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2$Species <- rep("O.onirici",3)
      va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2$Genus <- rep("Oscheius",3)
      va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2$Scale <- rep("data",3)
      va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2
      
      pdf("Vg_va_h2_Evol_data_P3p_divided_P4p_SSSS_2_log10_Oonirici.pdf")
      ggplot(va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2, aes(x=Measure, y= median)) +
        geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
        geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
        geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
        theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
        labs(y = "Va log10 ( P3.p / P4.p)", title = "Oonirici_log10(P3p/P4p)")
      dev.off() 
      
    }
    
    va_h2_Evol_Vg_Oonirici_P3p_divided_P4p_SSSS_summary_2 <- rbind.data.frame(va_h2_Evol_liab_Vg_Oonirici_P3p_divided_P4p_SSSS_2,va_h2_Evol_data_Vg_Oonirici_P3p_divided_P4p_SSSS_2)
    va_h2_Evol_Vg_Oonirici_P3p_divided_P4p_SSSS_summary_2
    
  }
}


## ---- Vg_Oscheius_summary ----
Vg_Oscheius_summary_2 <-  rbind.data.frame(Vg_Otipulae_summary_2,Vg_Oonirici_summary_2)
names(Vg_Oscheius_summary_2)[names(Vg_Oscheius_summary_2) == "Models"] <- "Model_name"
names(Vg_Oscheius_summary_2)[names(Vg_Oscheius_summary_2) == "Treatment"] <- "Model_set"
View(Vg_Oscheius_summary_2)

names(Vg_Oscheius_summary_2)
write_xlsx(Vg_Oscheius_summary_2, "Vg_Oscheius_summary_SSSS_2.xlsx")

Vg_Oscheius_summary_2_Vg <- subset(Vg_Oscheius_summary_2, Model_set=="Vg")
View(Vg_Oscheius_summary_2_Vg)
write_xlsx(Vg_Oscheius_summary_2_Vg, "Vg_Oscheius_summary_SSSS_2_Vg.xlsx")



## ---- Vg_Oscheius_P3p_divided_P4p_SSSS ----
va_h2_Evol_Vg_Oscheius_P3p_divided_P4p_SSSS_summary_2 <-  rbind.data.frame(va_h2_Evol_Vg_Otipulae_P3p_divided_P4p_SSSS_summary_2,va_h2_Evol_Vg_Oonirici_P3p_divided_P4p_SSSS_summary_2)
va_h2_Evol_Vg_Oscheius_P3p_divided_P4p_SSSS_summary_2$Variance <- rep("Vg",12)

write_xlsx(va_h2_Evol_Vg_Oscheius_P3p_divided_P4p_SSSS_summary_2, "va_h2_Evol_Vg_Oscheius_log10_P3p_divided_P4p_SSSS_summary_2.xlsx")


pdf("va_h2_Evol_Vg_Oscheius_P3p_divided_P4p_SSSS_summary_2.pdf")
ggplot(va_h2_Evol_Vg_Oscheius_P3p_divided_P4p_SSSS_summary_2, aes(x=Species, y= median)) +
  geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
  geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
  geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
  theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) +facet_grid(Scale~Measure) + theme(aspect.ratio=1) + labs(y = "Va log10 ( P3.p / P4.p)") 
dev.off()


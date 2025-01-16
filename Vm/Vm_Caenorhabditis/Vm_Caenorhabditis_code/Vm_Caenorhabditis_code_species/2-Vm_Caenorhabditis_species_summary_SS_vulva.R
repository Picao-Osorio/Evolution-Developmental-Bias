#Vm_Caenorhabditis_species_summary_SS_vulva_SS

#---- Celegans ----
{
  #Summary Celegans P3p
  {
    #Summary liability scale Celegans P3p
    {
      #Summary va_liab_Celegans_P3p_SS: NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_Celegans_P3p_SS <- rbind(mean(va_liab_Celegans_CONTROL_P3p_SS_mod/2),mean(va_liab_Celegans_MA_P3p_SS_mod/2),mean(va_liab_Celegans_Vm_P3p_SS_mod/2))
        colnames(mean_va_liab_Celegans_P3p_SS) <- c("mean")
        median_va_liab_Celegans_P3p_SS <- rbind(median(va_liab_Celegans_CONTROL_P3p_SS_mod/2),median(va_liab_Celegans_MA_P3p_SS_mod/2),median(va_liab_Celegans_Vm_P3p_SS_mod/2))
        colnames(median_va_liab_Celegans_P3p_SS) <- c("median")
        posterior.mode_va_liab_Celegans_P3p_SS <- rbind(posterior.mode(va_liab_Celegans_CONTROL_P3p_SS_mod/2),posterior.mode(va_liab_Celegans_MA_P3p_SS_mod/2),posterior.mode(va_liab_Celegans_Vm_P3p_SS_mod/2))
        colnames(posterior.mode_va_liab_Celegans_P3p_SS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Celegans_P3p_SS <- rbind(HPDinterval(va_liab_Celegans_CONTROL_P3p_SS_mod/2),HPDinterval(va_liab_Celegans_MA_P3p_SS_mod/2),HPDinterval(va_liab_Celegans_Vm_P3p_SS_mod/2))
        colnames(HPDinterval_0.95_va_liab_Celegans_P3p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Celegans_P3p_SS <- rbind(HPDinterval(va_liab_Celegans_CONTROL_P3p_SS_mod/2,prob=.83),HPDinterval(va_liab_Celegans_MA_P3p_SS_mod/2,prob=.83),HPDinterval(va_liab_Celegans_Vm_P3p_SS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Celegans_P3p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Celegans_P3p_SS <- rbind(effectiveSize(va_liab_Celegans_CONTROL_P3p_SS_mod/2),effectiveSize(va_liab_Celegans_MA_P3p_SS_mod/2),effectiveSize(va_liab_Celegans_Vm_P3p_SS_mod/2))
        colnames(effectiveSize_va_liab_Celegans_P3p_SS) <- c("effectiveSize")
        va_liab_Celegans_P3p_SS <- cbind.data.frame(mean_va_liab_Celegans_P3p_SS,median_va_liab_Celegans_P3p_SS,posterior.mode_va_liab_Celegans_P3p_SS,HPDinterval_0.95_va_liab_Celegans_P3p_SS,HPDinterval_0.83_va_liab_Celegans_P3p_SS,effectiveSize_va_liab_Celegans_P3p_SS)
        rownames(va_liab_Celegans_P3p_SS) <- c("va_liab_Celegans_CONTROL_P3p_SS_mod","va_liab_Celegans_MA_P3p_SS_mod","va_liab_Celegans_Vm_P3p_SS_mod")
        va_liab_Celegans_P3p_SS <- cbind(Models = rownames(va_liab_Celegans_P3p_SS),va_liab_Celegans_P3p_SS)
        rownames(va_liab_Celegans_P3p_SS) <- NULL
        va_liab_Celegans_P3p_SS$Pnp <- c("P3.p","P3.p","P3.p")
        va_liab_Celegans_P3p_SS$Treatment <- c("Control","MA","Vm")
        va_liab_Celegans_P3p_SS$Measure <- c("Va","Va","Va")
        va_liab_Celegans_P3p_SS$Scale <- c("liab","liab","liab")
        va_liab_Celegans_P3p_SS$Variance <- c("Vm","Vm","Vm")
        va_liab_Celegans_P3p_SS
      }
      
      #Summary h2_liab_Celegans_P3p_SS
      {
        mean_h2_liab_Celegans_P3p_SS <- rbind(mean(h2_liab_Celegans_CONTROL_P3p_SS_mod),mean(h2_liab_Celegans_MA_P3p_SS_mod),mean(h2_liab_Celegans_Vm_P3p_SS_mod))
        colnames(mean_h2_liab_Celegans_P3p_SS) <- c("mean")
        median_h2_liab_Celegans_P3p_SS <- rbind(median(h2_liab_Celegans_CONTROL_P3p_SS_mod),median(h2_liab_Celegans_MA_P3p_SS_mod),median(h2_liab_Celegans_Vm_P3p_SS_mod))
        colnames(median_h2_liab_Celegans_P3p_SS) <- c("median")
        posterior.mode_h2_liab_Celegans_P3p_SS <- rbind(posterior.mode(h2_liab_Celegans_CONTROL_P3p_SS_mod),posterior.mode(h2_liab_Celegans_MA_P3p_SS_mod),posterior.mode(h2_liab_Celegans_Vm_P3p_SS_mod))
        colnames(posterior.mode_h2_liab_Celegans_P3p_SS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Celegans_P3p_SS <- rbind(HPDinterval(h2_liab_Celegans_CONTROL_P3p_SS_mod),HPDinterval(h2_liab_Celegans_MA_P3p_SS_mod),HPDinterval(h2_liab_Celegans_Vm_P3p_SS_mod))
        colnames(HPDinterval_0.95_h2_liab_Celegans_P3p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Celegans_P3p_SS <- rbind(HPDinterval(h2_liab_Celegans_CONTROL_P3p_SS_mod,prob=.83),HPDinterval(h2_liab_Celegans_MA_P3p_SS_mod,prob=.83),HPDinterval(h2_liab_Celegans_Vm_P3p_SS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Celegans_P3p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Celegans_P3p_SS <- rbind(effectiveSize(h2_liab_Celegans_CONTROL_P3p_SS_mod),effectiveSize(h2_liab_Celegans_MA_P3p_SS_mod),effectiveSize(h2_liab_Celegans_Vm_P3p_SS_mod))
        colnames(effectiveSize_h2_liab_Celegans_P3p_SS) <- c("effectiveSize")
        h2_liab_Celegans_P3p_SS <- cbind.data.frame(mean_h2_liab_Celegans_P3p_SS,median_h2_liab_Celegans_P3p_SS,posterior.mode_h2_liab_Celegans_P3p_SS,HPDinterval_0.95_h2_liab_Celegans_P3p_SS,HPDinterval_0.83_h2_liab_Celegans_P3p_SS,effectiveSize_h2_liab_Celegans_P3p_SS)
        rownames(h2_liab_Celegans_P3p_SS) <- c("h2_liab_Celegans_CONTROL_P3p_SS_mod","h2_liab_Celegans_MA_P3p_SS_mod","h2_liab_Celegans_Vm_P3p_SS_mod")
        h2_liab_Celegans_P3p_SS <- cbind(Models = rownames(h2_liab_Celegans_P3p_SS),h2_liab_Celegans_P3p_SS)
        rownames(h2_liab_Celegans_P3p_SS) <- NULL
        h2_liab_Celegans_P3p_SS$Pnp <- c("P3.p","P3.p","P3.p")
        h2_liab_Celegans_P3p_SS$Treatment <- c("Control","MA","Vm")
        h2_liab_Celegans_P3p_SS$Measure <- c("H2","H2","H2")
        h2_liab_Celegans_P3p_SS$Scale <- c("liab","liab","liab")
        h2_liab_Celegans_P3p_SS$Variance <- c("Vm","Vm","Vm")
        h2_liab_Celegans_P3p_SS
      }
      
      #Summary Evol_liab_Celegans_P3p_SS
      {
        mean_Evol_liab_Celegans_P3p_SS <- rbind(mean(Evol_liab_Celegans_CONTROL_P3p_SS_mod),mean(Evol_liab_Celegans_MA_P3p_SS_mod),mean(Evol_liab_Celegans_Vm_P3p_SS_mod))
        colnames(mean_Evol_liab_Celegans_P3p_SS) <- c("mean")
        median_Evol_liab_Celegans_P3p_SS <- rbind(median(Evol_liab_Celegans_CONTROL_P3p_SS_mod),median(Evol_liab_Celegans_MA_P3p_SS_mod),median(Evol_liab_Celegans_Vm_P3p_SS_mod))
        colnames(median_Evol_liab_Celegans_P3p_SS) <- c("median")
        posterior.mode_Evol_liab_Celegans_P3p_SS <- rbind(posterior.mode(Evol_liab_Celegans_CONTROL_P3p_SS_mod),posterior.mode(Evol_liab_Celegans_MA_P3p_SS_mod),posterior.mode(Evol_liab_Celegans_Vm_P3p_SS_mod))
        colnames(posterior.mode_Evol_liab_Celegans_P3p_SS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Celegans_P3p_SS <- rbind(HPDinterval(Evol_liab_Celegans_CONTROL_P3p_SS_mod),HPDinterval(Evol_liab_Celegans_MA_P3p_SS_mod),HPDinterval(Evol_liab_Celegans_Vm_P3p_SS_mod))
        colnames(HPDinterval_0.95_Evol_liab_Celegans_P3p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Celegans_P3p_SS <- rbind(HPDinterval(Evol_liab_Celegans_CONTROL_P3p_SS_mod,prob=.83),HPDinterval(Evol_liab_Celegans_MA_P3p_SS_mod,prob=.83),HPDinterval(Evol_liab_Celegans_Vm_P3p_SS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Celegans_P3p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Celegans_P3p_SS <- rbind(effectiveSize(Evol_liab_Celegans_CONTROL_P3p_SS_mod),effectiveSize(Evol_liab_Celegans_MA_P3p_SS_mod),effectiveSize(Evol_liab_Celegans_Vm_P3p_SS_mod))
        colnames(effectiveSize_Evol_liab_Celegans_P3p_SS) <- c("effectiveSize")
        Evol_liab_Celegans_P3p_SS <- cbind.data.frame(mean_Evol_liab_Celegans_P3p_SS,median_Evol_liab_Celegans_P3p_SS,posterior.mode_Evol_liab_Celegans_P3p_SS,HPDinterval_0.95_Evol_liab_Celegans_P3p_SS,HPDinterval_0.83_Evol_liab_Celegans_P3p_SS,effectiveSize_Evol_liab_Celegans_P3p_SS)
        rownames(Evol_liab_Celegans_P3p_SS) <- c("Evol_liab_Celegans_CONTROL_P3p_SS_mod","Evol_liab_Celegans_MA_P3p_SS_mod","Evol_liab_Celegans_Vm_P3p_SS_mod")
        Evol_liab_Celegans_P3p_SS <- cbind(Models = rownames(Evol_liab_Celegans_P3p_SS),Evol_liab_Celegans_P3p_SS)
        rownames(Evol_liab_Celegans_P3p_SS) <- NULL
        Evol_liab_Celegans_P3p_SS$Pnp <- c("P3.p","P3.p","P3.p")
        Evol_liab_Celegans_P3p_SS$Treatment <- c("Control","MA","Vm")
        Evol_liab_Celegans_P3p_SS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Celegans_P3p_SS$Scale <- c("liab","liab","liab")
        Evol_liab_Celegans_P3p_SS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_Celegans_P3p_SS
      }
      
      #Summary trait_mean_liab_Celegans_P3p_SS
      {
        mean_trait_mean_liab_Celegans_P3p_SS <- rbind(mean(trait_mean_liab_Celegans_CONTROL_P3p_SS_mod),mean(trait_mean_liab_Celegans_MA_P3p_SS_mod),mean(trait_mean_liab_Celegans_Vm_P3p_SS_mod))
        colnames(mean_trait_mean_liab_Celegans_P3p_SS) <- c("mean")
        median_trait_mean_liab_Celegans_P3p_SS <- rbind(median(trait_mean_liab_Celegans_CONTROL_P3p_SS_mod),median(trait_mean_liab_Celegans_MA_P3p_SS_mod),median(trait_mean_liab_Celegans_Vm_P3p_SS_mod))
        colnames(median_trait_mean_liab_Celegans_P3p_SS) <- c("median")
        posterior.mode_trait_mean_liab_Celegans_P3p_SS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Celegans_CONTROL_P3p_SS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Celegans_MA_P3p_SS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Celegans_Vm_P3p_SS_mod)))
        colnames(posterior.mode_trait_mean_liab_Celegans_P3p_SS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Celegans_P3p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Celegans_CONTROL_P3p_SS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Celegans_MA_P3p_SS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Celegans_Vm_P3p_SS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_Celegans_P3p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Celegans_P3p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Celegans_CONTROL_P3p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Celegans_MA_P3p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Celegans_Vm_P3p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Celegans_P3p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Celegans_P3p_SS <- rbind(effectiveSize(trait_mean_liab_Celegans_CONTROL_P3p_SS_mod),effectiveSize(trait_mean_liab_Celegans_MA_P3p_SS_mod),effectiveSize(trait_mean_liab_Celegans_Vm_P3p_SS_mod))
        colnames(effectiveSize_trait_mean_liab_Celegans_P3p_SS) <- c("effectiveSize")
        trait_mean_liab_Celegans_P3p_SS <- cbind.data.frame(mean_trait_mean_liab_Celegans_P3p_SS,median_trait_mean_liab_Celegans_P3p_SS,posterior.mode_trait_mean_liab_Celegans_P3p_SS,HPDinterval_0.95_trait_mean_liab_Celegans_P3p_SS,HPDinterval_0.83_trait_mean_liab_Celegans_P3p_SS,effectiveSize_trait_mean_liab_Celegans_P3p_SS)
        rownames(trait_mean_liab_Celegans_P3p_SS) <- c("trait_mean_liab_Celegans_CONTROL_P3p_SS_mod","trait_mean_liab_Celegans_MA_P3p_SS_mod","trait_mean_liab_Celegans_Vm_P3p_SS_mod")
        trait_mean_liab_Celegans_P3p_SS <- cbind(Models = rownames(trait_mean_liab_Celegans_P3p_SS),trait_mean_liab_Celegans_P3p_SS)
        rownames(trait_mean_liab_Celegans_P3p_SS) <- NULL
        trait_mean_liab_Celegans_P3p_SS$Pnp <- c("P3.p","P3.p","P3.p")
        trait_mean_liab_Celegans_P3p_SS$Treatment <- c("Control","MA","Vm")
        trait_mean_liab_Celegans_P3p_SS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Celegans_P3p_SS$Scale <- c("liab","liab","liab")
        trait_mean_liab_Celegans_P3p_SS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_Celegans_P3p_SS
      }
      
      liab_Celegans_P3p_SS <- rbind.data.frame(va_liab_Celegans_P3p_SS, h2_liab_Celegans_P3p_SS,Evol_liab_Celegans_P3p_SS,trait_mean_liab_Celegans_P3p_SS)
      liab_Celegans_P3p_SS
    }
    #Summary data scale Celegans P3p
    {
      #Summary va_data_Celegans_P3p_SS:NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_Celegans_P3p_SS <- rbind(mean(va_data_Celegans_CONTROL_P3p_SS_mod/2),mean(va_data_Celegans_MA_P3p_SS_mod/2),mean(va_data_Celegans_Vm_P3p_SS_mod/2))
        colnames(mean_va_data_Celegans_P3p_SS) <- c("mean")
        median_va_data_Celegans_P3p_SS <- rbind(median(va_data_Celegans_CONTROL_P3p_SS_mod/2),median(va_data_Celegans_MA_P3p_SS_mod/2),median(va_data_Celegans_Vm_P3p_SS_mod/2))
        colnames(median_va_data_Celegans_P3p_SS) <- c("median")
        posterior.mode_va_data_Celegans_P3p_SS <- rbind(posterior.mode(as.mcmc(va_data_Celegans_CONTROL_P3p_SS_mod/2)),posterior.mode(as.mcmc(va_data_Celegans_MA_P3p_SS_mod/2)),posterior.mode(as.mcmc(va_data_Celegans_Vm_P3p_SS_mod/2)))
        colnames(posterior.mode_va_data_Celegans_P3p_SS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Celegans_P3p_SS <- rbind(HPDinterval(as.mcmc(va_data_Celegans_CONTROL_P3p_SS_mod/2)),HPDinterval(as.mcmc(va_data_Celegans_MA_P3p_SS_mod/2)),HPDinterval(as.mcmc(va_data_Celegans_Vm_P3p_SS_mod/2)))
        colnames(HPDinterval_0.95_va_data_Celegans_P3p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Celegans_P3p_SS <- rbind(HPDinterval(as.mcmc(va_data_Celegans_CONTROL_P3p_SS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Celegans_MA_P3p_SS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Celegans_Vm_P3p_SS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Celegans_P3p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Celegans_P3p_SS <- rbind(effectiveSize(va_data_Celegans_CONTROL_P3p_SS_mod/2),effectiveSize(va_data_Celegans_MA_P3p_SS_mod/2),effectiveSize(va_data_Celegans_Vm_P3p_SS_mod/2))
        colnames(effectiveSize_va_data_Celegans_P3p_SS) <- c("effectiveSize")
        va_data_Celegans_P3p_SS <- cbind.data.frame(mean_va_data_Celegans_P3p_SS,median_va_data_Celegans_P3p_SS,posterior.mode_va_data_Celegans_P3p_SS,HPDinterval_0.95_va_data_Celegans_P3p_SS,HPDinterval_0.83_va_data_Celegans_P3p_SS,effectiveSize_va_data_Celegans_P3p_SS)
        rownames(va_data_Celegans_P3p_SS) <- c("va_data_Celegans_CONTROL_P3p_SS_mod","va_data_Celegans_MA_P3p_SS_mod","va_data_Celegans_Vm_P3p_SS_mod")
        va_data_Celegans_P3p_SS <- cbind(Models = rownames(va_data_Celegans_P3p_SS),va_data_Celegans_P3p_SS)
        rownames(va_data_Celegans_P3p_SS) <- NULL
        va_data_Celegans_P3p_SS$Pnp <- c("P3.p","P3.p","P3.p")
        va_data_Celegans_P3p_SS$Treatment <- c("Control","MA","Vm")
        va_data_Celegans_P3p_SS$Measure <- c("Va","Va","Va")
        va_data_Celegans_P3p_SS$Scale <- c("data","data","data")
        va_data_Celegans_P3p_SS$Variance <- c("Vm","Vm","Vm")
        va_data_Celegans_P3p_SS
      }
      
      #Summary h2_data_Celegans_P3p_SS
      {
        mean_h2_data_Celegans_P3p_SS <- rbind(mean(h2_data_Celegans_CONTROL_P3p_SS_mod),mean(h2_data_Celegans_MA_P3p_SS_mod),mean(h2_data_Celegans_Vm_P3p_SS_mod))
        colnames(mean_h2_data_Celegans_P3p_SS) <- c("mean")
        median_h2_data_Celegans_P3p_SS <- rbind(median(h2_data_Celegans_CONTROL_P3p_SS_mod),median(h2_data_Celegans_MA_P3p_SS_mod),median(h2_data_Celegans_Vm_P3p_SS_mod))
        colnames(median_h2_data_Celegans_P3p_SS) <- c("median")
        posterior.mode_h2_data_Celegans_P3p_SS <- rbind(posterior.mode(as.mcmc(h2_data_Celegans_CONTROL_P3p_SS_mod)),posterior.mode(as.mcmc(h2_data_Celegans_MA_P3p_SS_mod)),posterior.mode(as.mcmc(h2_data_Celegans_Vm_P3p_SS_mod)))
        colnames(posterior.mode_h2_data_Celegans_P3p_SS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Celegans_P3p_SS <- rbind(HPDinterval(as.mcmc(h2_data_Celegans_CONTROL_P3p_SS_mod)),HPDinterval(as.mcmc(h2_data_Celegans_MA_P3p_SS_mod)),HPDinterval(as.mcmc(h2_data_Celegans_Vm_P3p_SS_mod)))
        colnames(HPDinterval_0.95_h2_data_Celegans_P3p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Celegans_P3p_SS <- rbind(HPDinterval(as.mcmc(h2_data_Celegans_CONTROL_P3p_SS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Celegans_MA_P3p_SS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Celegans_Vm_P3p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Celegans_P3p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Celegans_P3p_SS <- rbind(effectiveSize(h2_data_Celegans_CONTROL_P3p_SS_mod),effectiveSize(h2_data_Celegans_MA_P3p_SS_mod),effectiveSize(h2_data_Celegans_Vm_P3p_SS_mod))
        colnames(effectiveSize_h2_data_Celegans_P3p_SS) <- c("effectiveSize")
        h2_data_Celegans_P3p_SS <- cbind.data.frame(mean_h2_data_Celegans_P3p_SS,median_h2_data_Celegans_P3p_SS,posterior.mode_h2_data_Celegans_P3p_SS,HPDinterval_0.95_h2_data_Celegans_P3p_SS,HPDinterval_0.83_h2_data_Celegans_P3p_SS,effectiveSize_h2_data_Celegans_P3p_SS)
        rownames(h2_data_Celegans_P3p_SS) <- c("h2_data_Celegans_CONTROL_P3p_SS_mod","h2_data_Celegans_MA_P3p_SS_mod","h2_data_Celegans_Vm_P3p_SS_mod")
        h2_data_Celegans_P3p_SS <- cbind(Models = rownames(h2_data_Celegans_P3p_SS),h2_data_Celegans_P3p_SS)
        rownames(h2_data_Celegans_P3p_SS) <- NULL
        h2_data_Celegans_P3p_SS$Pnp <- c("P3.p","P3.p","P3.p")
        h2_data_Celegans_P3p_SS$Treatment <- c("Control","MA","Vm")
        h2_data_Celegans_P3p_SS$Measure <- c("H2","H2","H2")
        h2_data_Celegans_P3p_SS$Scale <- c("data","data","data")
        h2_data_Celegans_P3p_SS$Variance <- c("Vm","Vm","Vm")
        h2_data_Celegans_P3p_SS
      }
      
      #Summary Evol_data_Celegans_P3p_SS
      {
        mean_Evol_data_Celegans_P3p_SS <- rbind(mean(Evol_data_Celegans_CONTROL_P3p_SS_mod),mean(Evol_data_Celegans_MA_P3p_SS_mod),mean(Evol_data_Celegans_Vm_P3p_SS_mod))
        colnames(mean_Evol_data_Celegans_P3p_SS) <- c("mean")
        median_Evol_data_Celegans_P3p_SS <- rbind(median(Evol_data_Celegans_CONTROL_P3p_SS_mod),median(Evol_data_Celegans_MA_P3p_SS_mod),median(Evol_data_Celegans_Vm_P3p_SS_mod))
        colnames(median_Evol_data_Celegans_P3p_SS) <- c("median")
        posterior.mode_Evol_data_Celegans_P3p_SS <- rbind(posterior.mode(as.mcmc(Evol_data_Celegans_CONTROL_P3p_SS_mod)),posterior.mode(as.mcmc(Evol_data_Celegans_MA_P3p_SS_mod)),posterior.mode(as.mcmc(Evol_data_Celegans_Vm_P3p_SS_mod)))
        colnames(posterior.mode_Evol_data_Celegans_P3p_SS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Celegans_P3p_SS <- rbind(HPDinterval(as.mcmc(Evol_data_Celegans_CONTROL_P3p_SS_mod)),HPDinterval(as.mcmc(Evol_data_Celegans_MA_P3p_SS_mod)),HPDinterval(as.mcmc(Evol_data_Celegans_Vm_P3p_SS_mod)))
        colnames(HPDinterval_0.95_Evol_data_Celegans_P3p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Celegans_P3p_SS <- rbind(HPDinterval(as.mcmc(Evol_data_Celegans_CONTROL_P3p_SS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Celegans_MA_P3p_SS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Celegans_Vm_P3p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Celegans_P3p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Celegans_P3p_SS <- rbind(effectiveSize(Evol_data_Celegans_CONTROL_P3p_SS_mod),effectiveSize(Evol_data_Celegans_MA_P3p_SS_mod),effectiveSize(Evol_data_Celegans_Vm_P3p_SS_mod))
        colnames(effectiveSize_Evol_data_Celegans_P3p_SS) <- c("effectiveSize")
        Evol_data_Celegans_P3p_SS <- cbind.data.frame(mean_Evol_data_Celegans_P3p_SS,median_Evol_data_Celegans_P3p_SS,posterior.mode_Evol_data_Celegans_P3p_SS,HPDinterval_0.95_Evol_data_Celegans_P3p_SS,HPDinterval_0.83_Evol_data_Celegans_P3p_SS,effectiveSize_Evol_data_Celegans_P3p_SS)
        rownames(Evol_data_Celegans_P3p_SS) <- c("Evol_data_Celegans_CONTROL_P3p_SS_mod","Evol_data_Celegans_MA_P3p_SS_mod","Evol_data_Celegans_Vm_P3p_SS_mod")
        Evol_data_Celegans_P3p_SS <- cbind(Models = rownames(Evol_data_Celegans_P3p_SS),Evol_data_Celegans_P3p_SS)
        rownames(Evol_data_Celegans_P3p_SS) <- NULL
        Evol_data_Celegans_P3p_SS$Pnp <- c("P3.p","P3.p","P3.p")
        Evol_data_Celegans_P3p_SS$Treatment <- c("Control","MA","Vm")
        Evol_data_Celegans_P3p_SS$Measure <- c("Evol","Evol","Evol")
        Evol_data_Celegans_P3p_SS$Scale <- c("data","data","data")
        Evol_data_Celegans_P3p_SS$Variance <- c("Vm","Vm","Vm")
        Evol_data_Celegans_P3p_SS
      }
      
      #Summary trait_mean_data_Celegans_P3p_SS
      {
        mean_trait_mean_data_Celegans_P3p_SS <- rbind(mean(trait_mean_data_Celegans_CONTROL_P3p_SS_mod),mean(trait_mean_data_Celegans_MA_P3p_SS_mod),mean(trait_mean_data_Celegans_Vm_P3p_SS_mod))
        colnames(mean_trait_mean_data_Celegans_P3p_SS) <- c("mean")
        median_trait_mean_data_Celegans_P3p_SS <- rbind(median(trait_mean_data_Celegans_CONTROL_P3p_SS_mod),median(trait_mean_data_Celegans_MA_P3p_SS_mod),median(trait_mean_data_Celegans_Vm_P3p_SS_mod))
        colnames(median_trait_mean_data_Celegans_P3p_SS) <- c("median")
        posterior.mode_trait_mean_data_Celegans_P3p_SS <- rbind(posterior.mode(as.mcmc(trait_mean_data_Celegans_CONTROL_P3p_SS_mod)),posterior.mode(as.mcmc(trait_mean_data_Celegans_MA_P3p_SS_mod)),posterior.mode(as.mcmc(trait_mean_data_Celegans_Vm_P3p_SS_mod)))
        colnames(posterior.mode_trait_mean_data_Celegans_P3p_SS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Celegans_P3p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_data_Celegans_CONTROL_P3p_SS_mod)),HPDinterval(as.mcmc(trait_mean_data_Celegans_MA_P3p_SS_mod)),HPDinterval(as.mcmc(trait_mean_data_Celegans_Vm_P3p_SS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_Celegans_P3p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Celegans_P3p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_data_Celegans_CONTROL_P3p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Celegans_MA_P3p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Celegans_Vm_P3p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Celegans_P3p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Celegans_P3p_SS <- rbind(effectiveSize(trait_mean_data_Celegans_CONTROL_P3p_SS_mod),effectiveSize(trait_mean_data_Celegans_MA_P3p_SS_mod),effectiveSize(trait_mean_data_Celegans_Vm_P3p_SS_mod))
        colnames(effectiveSize_trait_mean_data_Celegans_P3p_SS) <- c("effectiveSize")
        trait_mean_data_Celegans_P3p_SS <- cbind.data.frame(mean_trait_mean_data_Celegans_P3p_SS,median_trait_mean_data_Celegans_P3p_SS,posterior.mode_trait_mean_data_Celegans_P3p_SS,HPDinterval_0.95_trait_mean_data_Celegans_P3p_SS,HPDinterval_0.83_trait_mean_data_Celegans_P3p_SS,effectiveSize_trait_mean_data_Celegans_P3p_SS)
        rownames(trait_mean_data_Celegans_P3p_SS) <- c("trait_mean_data_Celegans_CONTROL_P3p_SS_mod","trait_mean_data_Celegans_MA_P3p_SS_mod","trait_mean_data_Celegans_Vm_P3p_SS_mod")
        trait_mean_data_Celegans_P3p_SS <- cbind(Models = rownames(trait_mean_data_Celegans_P3p_SS),trait_mean_data_Celegans_P3p_SS)
        rownames(trait_mean_data_Celegans_P3p_SS) <- NULL
        trait_mean_data_Celegans_P3p_SS$Pnp <- c("P3.p","P3.p","P3.p")
        trait_mean_data_Celegans_P3p_SS$Treatment <- c("Control","MA","Vm")
        trait_mean_data_Celegans_P3p_SS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Celegans_P3p_SS$Scale <- c("data","data","data")
        trait_mean_data_Celegans_P3p_SS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_Celegans_P3p_SS
      }
      
      data_Celegans_P3p_SS <- rbind.data.frame(va_data_Celegans_P3p_SS, h2_data_Celegans_P3p_SS,Evol_data_Celegans_P3p_SS,trait_mean_data_Celegans_P3p_SS)
      data_Celegans_P3p_SS
      
    }
    Vm_Celegans_P3p_SS <- rbind.data.frame(liab_Celegans_P3p_SS, data_Celegans_P3p_SS)
    Vm_Celegans_P3p_SS$Pnp_fate <- rep("SS", 24)
    Vm_Celegans_P3p_SS
    #remove Celegans P3p_SS models
    {
      remove(Celegans_CONTROL_P3p_SS_mod)
      remove(Celegans_MA_P3p_SS_mod)
      remove(Celegans_Vm_P3p_SS_mod)
    }
  }
  
  #Summary Celegans P4p
  {
    #Summary liability scale Celegans P4p
    {
      #Summary va_liab_Celegans_P4p_SS: NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_Celegans_P4p_SS <- rbind(mean(va_liab_Celegans_CONTROL_P4p_SS_mod/2),mean(va_liab_Celegans_MA_P4p_SS_mod/2),mean(va_liab_Celegans_Vm_P4p_SS_mod/2))
        colnames(mean_va_liab_Celegans_P4p_SS) <- c("mean")
        median_va_liab_Celegans_P4p_SS <- rbind(median(va_liab_Celegans_CONTROL_P4p_SS_mod/2),median(va_liab_Celegans_MA_P4p_SS_mod/2),median(va_liab_Celegans_Vm_P4p_SS_mod/2))
        colnames(median_va_liab_Celegans_P4p_SS) <- c("median")
        posterior.mode_va_liab_Celegans_P4p_SS <- rbind(posterior.mode(va_liab_Celegans_CONTROL_P4p_SS_mod/2),posterior.mode(va_liab_Celegans_MA_P4p_SS_mod/2),posterior.mode(va_liab_Celegans_Vm_P4p_SS_mod/2))
        colnames(posterior.mode_va_liab_Celegans_P4p_SS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Celegans_P4p_SS <- rbind(HPDinterval(va_liab_Celegans_CONTROL_P4p_SS_mod/2),HPDinterval(va_liab_Celegans_MA_P4p_SS_mod/2),HPDinterval(va_liab_Celegans_Vm_P4p_SS_mod/2))
        colnames(HPDinterval_0.95_va_liab_Celegans_P4p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Celegans_P4p_SS <- rbind(HPDinterval(va_liab_Celegans_CONTROL_P4p_SS_mod/2,prob=.83),HPDinterval(va_liab_Celegans_MA_P4p_SS_mod/2,prob=.83),HPDinterval(va_liab_Celegans_Vm_P4p_SS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Celegans_P4p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Celegans_P4p_SS <- rbind(effectiveSize(va_liab_Celegans_CONTROL_P4p_SS_mod/2),effectiveSize(va_liab_Celegans_MA_P4p_SS_mod/2),effectiveSize(va_liab_Celegans_Vm_P4p_SS_mod/2))
        colnames(effectiveSize_va_liab_Celegans_P4p_SS) <- c("effectiveSize")
        va_liab_Celegans_P4p_SS <- cbind.data.frame(mean_va_liab_Celegans_P4p_SS,median_va_liab_Celegans_P4p_SS,posterior.mode_va_liab_Celegans_P4p_SS,HPDinterval_0.95_va_liab_Celegans_P4p_SS,HPDinterval_0.83_va_liab_Celegans_P4p_SS,effectiveSize_va_liab_Celegans_P4p_SS)
        rownames(va_liab_Celegans_P4p_SS) <- c("va_liab_Celegans_CONTROL_P4p_SS_mod","va_liab_Celegans_MA_P4p_SS_mod","va_liab_Celegans_Vm_P4p_SS_mod")
        va_liab_Celegans_P4p_SS <- cbind(Models = rownames(va_liab_Celegans_P4p_SS),va_liab_Celegans_P4p_SS)
        rownames(va_liab_Celegans_P4p_SS) <- NULL
        va_liab_Celegans_P4p_SS$Pnp <- c("P4.p","P4.p","P4.p")
        va_liab_Celegans_P4p_SS$Treatment <- c("Control","MA","Vm")
        va_liab_Celegans_P4p_SS$Measure <- c("Va","Va","Va")
        va_liab_Celegans_P4p_SS$Scale <- c("liab","liab","liab")
        va_liab_Celegans_P4p_SS$Variance <- c("Vm","Vm","Vm")
        va_liab_Celegans_P4p_SS
      }
      
      #Summary h2_liab_Celegans_P4p_SS
      {
        mean_h2_liab_Celegans_P4p_SS <- rbind(mean(h2_liab_Celegans_CONTROL_P4p_SS_mod),mean(h2_liab_Celegans_MA_P4p_SS_mod),mean(h2_liab_Celegans_Vm_P4p_SS_mod))
        colnames(mean_h2_liab_Celegans_P4p_SS) <- c("mean")
        median_h2_liab_Celegans_P4p_SS <- rbind(median(h2_liab_Celegans_CONTROL_P4p_SS_mod),median(h2_liab_Celegans_MA_P4p_SS_mod),median(h2_liab_Celegans_Vm_P4p_SS_mod))
        colnames(median_h2_liab_Celegans_P4p_SS) <- c("median")
        posterior.mode_h2_liab_Celegans_P4p_SS <- rbind(posterior.mode(h2_liab_Celegans_CONTROL_P4p_SS_mod),posterior.mode(h2_liab_Celegans_MA_P4p_SS_mod),posterior.mode(h2_liab_Celegans_Vm_P4p_SS_mod))
        colnames(posterior.mode_h2_liab_Celegans_P4p_SS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Celegans_P4p_SS <- rbind(HPDinterval(h2_liab_Celegans_CONTROL_P4p_SS_mod),HPDinterval(h2_liab_Celegans_MA_P4p_SS_mod),HPDinterval(h2_liab_Celegans_Vm_P4p_SS_mod))
        colnames(HPDinterval_0.95_h2_liab_Celegans_P4p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Celegans_P4p_SS <- rbind(HPDinterval(h2_liab_Celegans_CONTROL_P4p_SS_mod,prob=.83),HPDinterval(h2_liab_Celegans_MA_P4p_SS_mod,prob=.83),HPDinterval(h2_liab_Celegans_Vm_P4p_SS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Celegans_P4p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Celegans_P4p_SS <- rbind(effectiveSize(h2_liab_Celegans_CONTROL_P4p_SS_mod),effectiveSize(h2_liab_Celegans_MA_P4p_SS_mod),effectiveSize(h2_liab_Celegans_Vm_P4p_SS_mod))
        colnames(effectiveSize_h2_liab_Celegans_P4p_SS) <- c("effectiveSize")
        h2_liab_Celegans_P4p_SS <- cbind.data.frame(mean_h2_liab_Celegans_P4p_SS,median_h2_liab_Celegans_P4p_SS,posterior.mode_h2_liab_Celegans_P4p_SS,HPDinterval_0.95_h2_liab_Celegans_P4p_SS,HPDinterval_0.83_h2_liab_Celegans_P4p_SS,effectiveSize_h2_liab_Celegans_P4p_SS)
        rownames(h2_liab_Celegans_P4p_SS) <- c("h2_liab_Celegans_CONTROL_P4p_SS_mod","h2_liab_Celegans_MA_P4p_SS_mod","h2_liab_Celegans_Vm_P4p_SS_mod")
        h2_liab_Celegans_P4p_SS <- cbind(Models = rownames(h2_liab_Celegans_P4p_SS),h2_liab_Celegans_P4p_SS)
        rownames(h2_liab_Celegans_P4p_SS) <- NULL
        h2_liab_Celegans_P4p_SS$Pnp <- c("P4.p","P4.p","P4.p")
        h2_liab_Celegans_P4p_SS$Treatment <- c("Control","MA","Vm")
        h2_liab_Celegans_P4p_SS$Measure <- c("H2","H2","H2")
        h2_liab_Celegans_P4p_SS$Scale <- c("liab","liab","liab")
        h2_liab_Celegans_P4p_SS$Variance <- c("Vm","Vm","Vm")
        h2_liab_Celegans_P4p_SS
      }
      
      #Summary Evol_liab_Celegans_P4p_SS
      {
        mean_Evol_liab_Celegans_P4p_SS <- rbind(mean(Evol_liab_Celegans_CONTROL_P4p_SS_mod),mean(Evol_liab_Celegans_MA_P4p_SS_mod),mean(Evol_liab_Celegans_Vm_P4p_SS_mod))
        colnames(mean_Evol_liab_Celegans_P4p_SS) <- c("mean")
        median_Evol_liab_Celegans_P4p_SS <- rbind(median(Evol_liab_Celegans_CONTROL_P4p_SS_mod),median(Evol_liab_Celegans_MA_P4p_SS_mod),median(Evol_liab_Celegans_Vm_P4p_SS_mod))
        colnames(median_Evol_liab_Celegans_P4p_SS) <- c("median")
        posterior.mode_Evol_liab_Celegans_P4p_SS <- rbind(posterior.mode(Evol_liab_Celegans_CONTROL_P4p_SS_mod),posterior.mode(Evol_liab_Celegans_MA_P4p_SS_mod),posterior.mode(Evol_liab_Celegans_Vm_P4p_SS_mod))
        colnames(posterior.mode_Evol_liab_Celegans_P4p_SS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Celegans_P4p_SS <- rbind(HPDinterval(Evol_liab_Celegans_CONTROL_P4p_SS_mod),HPDinterval(Evol_liab_Celegans_MA_P4p_SS_mod),HPDinterval(Evol_liab_Celegans_Vm_P4p_SS_mod))
        colnames(HPDinterval_0.95_Evol_liab_Celegans_P4p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Celegans_P4p_SS <- rbind(HPDinterval(Evol_liab_Celegans_CONTROL_P4p_SS_mod,prob=.83),HPDinterval(Evol_liab_Celegans_MA_P4p_SS_mod,prob=.83),HPDinterval(Evol_liab_Celegans_Vm_P4p_SS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Celegans_P4p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Celegans_P4p_SS <- rbind(effectiveSize(Evol_liab_Celegans_CONTROL_P4p_SS_mod),effectiveSize(Evol_liab_Celegans_MA_P4p_SS_mod),effectiveSize(Evol_liab_Celegans_Vm_P4p_SS_mod))
        colnames(effectiveSize_Evol_liab_Celegans_P4p_SS) <- c("effectiveSize")
        Evol_liab_Celegans_P4p_SS <- cbind.data.frame(mean_Evol_liab_Celegans_P4p_SS,median_Evol_liab_Celegans_P4p_SS,posterior.mode_Evol_liab_Celegans_P4p_SS,HPDinterval_0.95_Evol_liab_Celegans_P4p_SS,HPDinterval_0.83_Evol_liab_Celegans_P4p_SS,effectiveSize_Evol_liab_Celegans_P4p_SS)
        rownames(Evol_liab_Celegans_P4p_SS) <- c("Evol_liab_Celegans_CONTROL_P4p_SS_mod","Evol_liab_Celegans_MA_P4p_SS_mod","Evol_liab_Celegans_Vm_P4p_SS_mod")
        Evol_liab_Celegans_P4p_SS <- cbind(Models = rownames(Evol_liab_Celegans_P4p_SS),Evol_liab_Celegans_P4p_SS)
        rownames(Evol_liab_Celegans_P4p_SS) <- NULL
        Evol_liab_Celegans_P4p_SS$Pnp <- c("P4.p","P4.p","P4.p")
        Evol_liab_Celegans_P4p_SS$Treatment <- c("Control","MA","Vm")
        Evol_liab_Celegans_P4p_SS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Celegans_P4p_SS$Scale <- c("liab","liab","liab")
        Evol_liab_Celegans_P4p_SS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_Celegans_P4p_SS
      }
      
      #Summary trait_mean_liab_Celegans_P4p_SS
      {
        mean_trait_mean_liab_Celegans_P4p_SS <- rbind(mean(trait_mean_liab_Celegans_CONTROL_P4p_SS_mod),mean(trait_mean_liab_Celegans_MA_P4p_SS_mod),mean(trait_mean_liab_Celegans_Vm_P4p_SS_mod))
        colnames(mean_trait_mean_liab_Celegans_P4p_SS) <- c("mean")
        median_trait_mean_liab_Celegans_P4p_SS <- rbind(median(trait_mean_liab_Celegans_CONTROL_P4p_SS_mod),median(trait_mean_liab_Celegans_MA_P4p_SS_mod),median(trait_mean_liab_Celegans_Vm_P4p_SS_mod))
        colnames(median_trait_mean_liab_Celegans_P4p_SS) <- c("median")
        posterior.mode_trait_mean_liab_Celegans_P4p_SS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Celegans_CONTROL_P4p_SS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Celegans_MA_P4p_SS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Celegans_Vm_P4p_SS_mod)))
        colnames(posterior.mode_trait_mean_liab_Celegans_P4p_SS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Celegans_P4p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Celegans_CONTROL_P4p_SS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Celegans_MA_P4p_SS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Celegans_Vm_P4p_SS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_Celegans_P4p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Celegans_P4p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Celegans_CONTROL_P4p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Celegans_MA_P4p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Celegans_Vm_P4p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Celegans_P4p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Celegans_P4p_SS <- rbind(effectiveSize(trait_mean_liab_Celegans_CONTROL_P4p_SS_mod),effectiveSize(trait_mean_liab_Celegans_MA_P4p_SS_mod),effectiveSize(trait_mean_liab_Celegans_Vm_P4p_SS_mod))
        colnames(effectiveSize_trait_mean_liab_Celegans_P4p_SS) <- c("effectiveSize")
        trait_mean_liab_Celegans_P4p_SS <- cbind.data.frame(mean_trait_mean_liab_Celegans_P4p_SS,median_trait_mean_liab_Celegans_P4p_SS,posterior.mode_trait_mean_liab_Celegans_P4p_SS,HPDinterval_0.95_trait_mean_liab_Celegans_P4p_SS,HPDinterval_0.83_trait_mean_liab_Celegans_P4p_SS,effectiveSize_trait_mean_liab_Celegans_P4p_SS)
        rownames(trait_mean_liab_Celegans_P4p_SS) <- c("trait_mean_liab_Celegans_CONTROL_P4p_SS_mod","trait_mean_liab_Celegans_MA_P4p_SS_mod","trait_mean_liab_Celegans_Vm_P4p_SS_mod")
        trait_mean_liab_Celegans_P4p_SS <- cbind(Models = rownames(trait_mean_liab_Celegans_P4p_SS),trait_mean_liab_Celegans_P4p_SS)
        rownames(trait_mean_liab_Celegans_P4p_SS) <- NULL
        trait_mean_liab_Celegans_P4p_SS$Pnp <- c("P4.p","P4.p","P4.p")
        trait_mean_liab_Celegans_P4p_SS$Treatment <- c("Control","MA","Vm")
        trait_mean_liab_Celegans_P4p_SS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Celegans_P4p_SS$Scale <- c("liab","liab","liab")
        trait_mean_liab_Celegans_P4p_SS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_Celegans_P4p_SS
      }
      
      liab_Celegans_P4p_SS <- rbind.data.frame(va_liab_Celegans_P4p_SS, h2_liab_Celegans_P4p_SS,Evol_liab_Celegans_P4p_SS,trait_mean_liab_Celegans_P4p_SS)
      liab_Celegans_P4p_SS
    }
    #Summary data scale Celegans P4p
    {
      #Summary va_data_Celegans_P4p_SS:NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_Celegans_P4p_SS <- rbind(mean(va_data_Celegans_CONTROL_P4p_SS_mod/2),mean(va_data_Celegans_MA_P4p_SS_mod/2),mean(va_data_Celegans_Vm_P4p_SS_mod/2))
        colnames(mean_va_data_Celegans_P4p_SS) <- c("mean")
        median_va_data_Celegans_P4p_SS <- rbind(median(va_data_Celegans_CONTROL_P4p_SS_mod/2),median(va_data_Celegans_MA_P4p_SS_mod/2),median(va_data_Celegans_Vm_P4p_SS_mod/2))
        colnames(median_va_data_Celegans_P4p_SS) <- c("median")
        posterior.mode_va_data_Celegans_P4p_SS <- rbind(posterior.mode(as.mcmc(va_data_Celegans_CONTROL_P4p_SS_mod/2)),posterior.mode(as.mcmc(va_data_Celegans_MA_P4p_SS_mod/2)),posterior.mode(as.mcmc(va_data_Celegans_Vm_P4p_SS_mod/2)))
        colnames(posterior.mode_va_data_Celegans_P4p_SS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Celegans_P4p_SS <- rbind(HPDinterval(as.mcmc(va_data_Celegans_CONTROL_P4p_SS_mod/2)),HPDinterval(as.mcmc(va_data_Celegans_MA_P4p_SS_mod/2)),HPDinterval(as.mcmc(va_data_Celegans_Vm_P4p_SS_mod/2)))
        colnames(HPDinterval_0.95_va_data_Celegans_P4p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Celegans_P4p_SS <- rbind(HPDinterval(as.mcmc(va_data_Celegans_CONTROL_P4p_SS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Celegans_MA_P4p_SS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Celegans_Vm_P4p_SS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Celegans_P4p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Celegans_P4p_SS <- rbind(effectiveSize(va_data_Celegans_CONTROL_P4p_SS_mod/2),effectiveSize(va_data_Celegans_MA_P4p_SS_mod/2),effectiveSize(va_data_Celegans_Vm_P4p_SS_mod/2))
        colnames(effectiveSize_va_data_Celegans_P4p_SS) <- c("effectiveSize")
        va_data_Celegans_P4p_SS <- cbind.data.frame(mean_va_data_Celegans_P4p_SS,median_va_data_Celegans_P4p_SS,posterior.mode_va_data_Celegans_P4p_SS,HPDinterval_0.95_va_data_Celegans_P4p_SS,HPDinterval_0.83_va_data_Celegans_P4p_SS,effectiveSize_va_data_Celegans_P4p_SS)
        rownames(va_data_Celegans_P4p_SS) <- c("va_data_Celegans_CONTROL_P4p_SS_mod","va_data_Celegans_MA_P4p_SS_mod","va_data_Celegans_Vm_P4p_SS_mod")
        va_data_Celegans_P4p_SS <- cbind(Models = rownames(va_data_Celegans_P4p_SS),va_data_Celegans_P4p_SS)
        rownames(va_data_Celegans_P4p_SS) <- NULL
        va_data_Celegans_P4p_SS$Pnp <- c("P4.p","P4.p","P4.p")
        va_data_Celegans_P4p_SS$Treatment <- c("Control","MA","Vm")
        va_data_Celegans_P4p_SS$Measure <- c("Va","Va","Va")
        va_data_Celegans_P4p_SS$Scale <- c("data","data","data")
        va_data_Celegans_P4p_SS$Variance <- c("Vm","Vm","Vm")
        va_data_Celegans_P4p_SS
      }
      
      #Summary h2_data_Celegans_P4p_SS
      {
        mean_h2_data_Celegans_P4p_SS <- rbind(mean(h2_data_Celegans_CONTROL_P4p_SS_mod),mean(h2_data_Celegans_MA_P4p_SS_mod),mean(h2_data_Celegans_Vm_P4p_SS_mod))
        colnames(mean_h2_data_Celegans_P4p_SS) <- c("mean")
        median_h2_data_Celegans_P4p_SS <- rbind(median(h2_data_Celegans_CONTROL_P4p_SS_mod),median(h2_data_Celegans_MA_P4p_SS_mod),median(h2_data_Celegans_Vm_P4p_SS_mod))
        colnames(median_h2_data_Celegans_P4p_SS) <- c("median")
        posterior.mode_h2_data_Celegans_P4p_SS <- rbind(posterior.mode(as.mcmc(h2_data_Celegans_CONTROL_P4p_SS_mod)),posterior.mode(as.mcmc(h2_data_Celegans_MA_P4p_SS_mod)),posterior.mode(as.mcmc(h2_data_Celegans_Vm_P4p_SS_mod)))
        colnames(posterior.mode_h2_data_Celegans_P4p_SS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Celegans_P4p_SS <- rbind(HPDinterval(as.mcmc(h2_data_Celegans_CONTROL_P4p_SS_mod)),HPDinterval(as.mcmc(h2_data_Celegans_MA_P4p_SS_mod)),HPDinterval(as.mcmc(h2_data_Celegans_Vm_P4p_SS_mod)))
        colnames(HPDinterval_0.95_h2_data_Celegans_P4p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Celegans_P4p_SS <- rbind(HPDinterval(as.mcmc(h2_data_Celegans_CONTROL_P4p_SS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Celegans_MA_P4p_SS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Celegans_Vm_P4p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Celegans_P4p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Celegans_P4p_SS <- rbind(effectiveSize(h2_data_Celegans_CONTROL_P4p_SS_mod),effectiveSize(h2_data_Celegans_MA_P4p_SS_mod),effectiveSize(h2_data_Celegans_Vm_P4p_SS_mod))
        colnames(effectiveSize_h2_data_Celegans_P4p_SS) <- c("effectiveSize")
        h2_data_Celegans_P4p_SS <- cbind.data.frame(mean_h2_data_Celegans_P4p_SS,median_h2_data_Celegans_P4p_SS,posterior.mode_h2_data_Celegans_P4p_SS,HPDinterval_0.95_h2_data_Celegans_P4p_SS,HPDinterval_0.83_h2_data_Celegans_P4p_SS,effectiveSize_h2_data_Celegans_P4p_SS)
        rownames(h2_data_Celegans_P4p_SS) <- c("h2_data_Celegans_CONTROL_P4p_SS_mod","h2_data_Celegans_MA_P4p_SS_mod","h2_data_Celegans_Vm_P4p_SS_mod")
        h2_data_Celegans_P4p_SS <- cbind(Models = rownames(h2_data_Celegans_P4p_SS),h2_data_Celegans_P4p_SS)
        rownames(h2_data_Celegans_P4p_SS) <- NULL
        h2_data_Celegans_P4p_SS$Pnp <- c("P4.p","P4.p","P4.p")
        h2_data_Celegans_P4p_SS$Treatment <- c("Control","MA","Vm")
        h2_data_Celegans_P4p_SS$Measure <- c("H2","H2","H2")
        h2_data_Celegans_P4p_SS$Scale <- c("data","data","data")
        h2_data_Celegans_P4p_SS$Variance <- c("Vm","Vm","Vm")
        h2_data_Celegans_P4p_SS
      }
      
      #Summary Evol_data_Celegans_P4p_SS
      {
        mean_Evol_data_Celegans_P4p_SS <- rbind(mean(Evol_data_Celegans_CONTROL_P4p_SS_mod),mean(Evol_data_Celegans_MA_P4p_SS_mod),mean(Evol_data_Celegans_Vm_P4p_SS_mod))
        colnames(mean_Evol_data_Celegans_P4p_SS) <- c("mean")
        median_Evol_data_Celegans_P4p_SS <- rbind(median(Evol_data_Celegans_CONTROL_P4p_SS_mod),median(Evol_data_Celegans_MA_P4p_SS_mod),median(Evol_data_Celegans_Vm_P4p_SS_mod))
        colnames(median_Evol_data_Celegans_P4p_SS) <- c("median")
        posterior.mode_Evol_data_Celegans_P4p_SS <- rbind(posterior.mode(as.mcmc(Evol_data_Celegans_CONTROL_P4p_SS_mod)),posterior.mode(as.mcmc(Evol_data_Celegans_MA_P4p_SS_mod)),posterior.mode(as.mcmc(Evol_data_Celegans_Vm_P4p_SS_mod)))
        colnames(posterior.mode_Evol_data_Celegans_P4p_SS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Celegans_P4p_SS <- rbind(HPDinterval(as.mcmc(Evol_data_Celegans_CONTROL_P4p_SS_mod)),HPDinterval(as.mcmc(Evol_data_Celegans_MA_P4p_SS_mod)),HPDinterval(as.mcmc(Evol_data_Celegans_Vm_P4p_SS_mod)))
        colnames(HPDinterval_0.95_Evol_data_Celegans_P4p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Celegans_P4p_SS <- rbind(HPDinterval(as.mcmc(Evol_data_Celegans_CONTROL_P4p_SS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Celegans_MA_P4p_SS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Celegans_Vm_P4p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Celegans_P4p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Celegans_P4p_SS <- rbind(effectiveSize(Evol_data_Celegans_CONTROL_P4p_SS_mod),effectiveSize(Evol_data_Celegans_MA_P4p_SS_mod),effectiveSize(Evol_data_Celegans_Vm_P4p_SS_mod))
        colnames(effectiveSize_Evol_data_Celegans_P4p_SS) <- c("effectiveSize")
        Evol_data_Celegans_P4p_SS <- cbind.data.frame(mean_Evol_data_Celegans_P4p_SS,median_Evol_data_Celegans_P4p_SS,posterior.mode_Evol_data_Celegans_P4p_SS,HPDinterval_0.95_Evol_data_Celegans_P4p_SS,HPDinterval_0.83_Evol_data_Celegans_P4p_SS,effectiveSize_Evol_data_Celegans_P4p_SS)
        rownames(Evol_data_Celegans_P4p_SS) <- c("Evol_data_Celegans_CONTROL_P4p_SS_mod","Evol_data_Celegans_MA_P4p_SS_mod","Evol_data_Celegans_Vm_P4p_SS_mod")
        Evol_data_Celegans_P4p_SS <- cbind(Models = rownames(Evol_data_Celegans_P4p_SS),Evol_data_Celegans_P4p_SS)
        rownames(Evol_data_Celegans_P4p_SS) <- NULL
        Evol_data_Celegans_P4p_SS$Pnp <- c("P4.p","P4.p","P4.p")
        Evol_data_Celegans_P4p_SS$Treatment <- c("Control","MA","Vm")
        Evol_data_Celegans_P4p_SS$Measure <- c("Evol","Evol","Evol")
        Evol_data_Celegans_P4p_SS$Scale <- c("data","data","data")
        Evol_data_Celegans_P4p_SS$Variance <- c("Vm","Vm","Vm")
        Evol_data_Celegans_P4p_SS
      }
      
      #Summary trait_mean_data_Celegans_P4p_SS
      {
        mean_trait_mean_data_Celegans_P4p_SS <- rbind(mean(trait_mean_data_Celegans_CONTROL_P4p_SS_mod),mean(trait_mean_data_Celegans_MA_P4p_SS_mod),mean(trait_mean_data_Celegans_Vm_P4p_SS_mod))
        colnames(mean_trait_mean_data_Celegans_P4p_SS) <- c("mean")
        median_trait_mean_data_Celegans_P4p_SS <- rbind(median(trait_mean_data_Celegans_CONTROL_P4p_SS_mod),median(trait_mean_data_Celegans_MA_P4p_SS_mod),median(trait_mean_data_Celegans_Vm_P4p_SS_mod))
        colnames(median_trait_mean_data_Celegans_P4p_SS) <- c("median")
        posterior.mode_trait_mean_data_Celegans_P4p_SS <- rbind(posterior.mode(as.mcmc(trait_mean_data_Celegans_CONTROL_P4p_SS_mod)),posterior.mode(as.mcmc(trait_mean_data_Celegans_MA_P4p_SS_mod)),posterior.mode(as.mcmc(trait_mean_data_Celegans_Vm_P4p_SS_mod)))
        colnames(posterior.mode_trait_mean_data_Celegans_P4p_SS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Celegans_P4p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_data_Celegans_CONTROL_P4p_SS_mod)),HPDinterval(as.mcmc(trait_mean_data_Celegans_MA_P4p_SS_mod)),HPDinterval(as.mcmc(trait_mean_data_Celegans_Vm_P4p_SS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_Celegans_P4p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Celegans_P4p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_data_Celegans_CONTROL_P4p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Celegans_MA_P4p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Celegans_Vm_P4p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Celegans_P4p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Celegans_P4p_SS <- rbind(effectiveSize(trait_mean_data_Celegans_CONTROL_P4p_SS_mod),effectiveSize(trait_mean_data_Celegans_MA_P4p_SS_mod),effectiveSize(trait_mean_data_Celegans_Vm_P4p_SS_mod))
        colnames(effectiveSize_trait_mean_data_Celegans_P4p_SS) <- c("effectiveSize")
        trait_mean_data_Celegans_P4p_SS <- cbind.data.frame(mean_trait_mean_data_Celegans_P4p_SS,median_trait_mean_data_Celegans_P4p_SS,posterior.mode_trait_mean_data_Celegans_P4p_SS,HPDinterval_0.95_trait_mean_data_Celegans_P4p_SS,HPDinterval_0.83_trait_mean_data_Celegans_P4p_SS,effectiveSize_trait_mean_data_Celegans_P4p_SS)
        rownames(trait_mean_data_Celegans_P4p_SS) <- c("trait_mean_data_Celegans_CONTROL_P4p_SS_mod","trait_mean_data_Celegans_MA_P4p_SS_mod","trait_mean_data_Celegans_Vm_P4p_SS_mod")
        trait_mean_data_Celegans_P4p_SS <- cbind(Models = rownames(trait_mean_data_Celegans_P4p_SS),trait_mean_data_Celegans_P4p_SS)
        rownames(trait_mean_data_Celegans_P4p_SS) <- NULL
        trait_mean_data_Celegans_P4p_SS$Pnp <- c("P4.p","P4.p","P4.p")
        trait_mean_data_Celegans_P4p_SS$Treatment <- c("Control","MA","Vm")
        trait_mean_data_Celegans_P4p_SS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Celegans_P4p_SS$Scale <- c("data","data","data")
        trait_mean_data_Celegans_P4p_SS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_Celegans_P4p_SS
      }
      
      data_Celegans_P4p_SS <- rbind.data.frame(va_data_Celegans_P4p_SS, h2_data_Celegans_P4p_SS,Evol_data_Celegans_P4p_SS,trait_mean_data_Celegans_P4p_SS)
      data_Celegans_P4p_SS
      
    }
    Vm_Celegans_P4p_SS <- rbind.data.frame(liab_Celegans_P4p_SS, data_Celegans_P4p_SS)
    Vm_Celegans_P4p_SS$Pnp_fate <- rep("SS", 24)
    Vm_Celegans_P4p_SS
    #remove Celegans P4p_SS models
    {
      remove(Celegans_CONTROL_P4p_SS_mod)
      remove(Celegans_MA_P4p_SS_mod)
      remove(Celegans_Vm_P4p_SS_mod)
    }
  }
  
  #Summary Celegans P8p
  {
    #Summary liability scale Celegans P8p
    {
      #Summary va_liab_Celegans_P8p_SS: NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_Celegans_P8p_SS <- rbind(mean(va_liab_Celegans_CONTROL_P8p_SS_mod/2),mean(va_liab_Celegans_MA_P8p_SS_mod/2),mean(va_liab_Celegans_Vm_P8p_SS_mod/2))
        colnames(mean_va_liab_Celegans_P8p_SS) <- c("mean")
        median_va_liab_Celegans_P8p_SS <- rbind(median(va_liab_Celegans_CONTROL_P8p_SS_mod/2),median(va_liab_Celegans_MA_P8p_SS_mod/2),median(va_liab_Celegans_Vm_P8p_SS_mod/2))
        colnames(median_va_liab_Celegans_P8p_SS) <- c("median")
        posterior.mode_va_liab_Celegans_P8p_SS <- rbind(posterior.mode(va_liab_Celegans_CONTROL_P8p_SS_mod/2),posterior.mode(va_liab_Celegans_MA_P8p_SS_mod/2),posterior.mode(va_liab_Celegans_Vm_P8p_SS_mod/2))
        colnames(posterior.mode_va_liab_Celegans_P8p_SS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Celegans_P8p_SS <- rbind(HPDinterval(va_liab_Celegans_CONTROL_P8p_SS_mod/2),HPDinterval(va_liab_Celegans_MA_P8p_SS_mod/2),HPDinterval(va_liab_Celegans_Vm_P8p_SS_mod/2))
        colnames(HPDinterval_0.95_va_liab_Celegans_P8p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Celegans_P8p_SS <- rbind(HPDinterval(va_liab_Celegans_CONTROL_P8p_SS_mod/2,prob=.83),HPDinterval(va_liab_Celegans_MA_P8p_SS_mod/2,prob=.83),HPDinterval(va_liab_Celegans_Vm_P8p_SS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Celegans_P8p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Celegans_P8p_SS <- rbind(effectiveSize(va_liab_Celegans_CONTROL_P8p_SS_mod/2),effectiveSize(va_liab_Celegans_MA_P8p_SS_mod/2),effectiveSize(va_liab_Celegans_Vm_P8p_SS_mod/2))
        colnames(effectiveSize_va_liab_Celegans_P8p_SS) <- c("effectiveSize")
        va_liab_Celegans_P8p_SS <- cbind.data.frame(mean_va_liab_Celegans_P8p_SS,median_va_liab_Celegans_P8p_SS,posterior.mode_va_liab_Celegans_P8p_SS,HPDinterval_0.95_va_liab_Celegans_P8p_SS,HPDinterval_0.83_va_liab_Celegans_P8p_SS,effectiveSize_va_liab_Celegans_P8p_SS)
        rownames(va_liab_Celegans_P8p_SS) <- c("va_liab_Celegans_CONTROL_P8p_SS_mod","va_liab_Celegans_MA_P8p_SS_mod","va_liab_Celegans_Vm_P8p_SS_mod")
        va_liab_Celegans_P8p_SS <- cbind(Models = rownames(va_liab_Celegans_P8p_SS),va_liab_Celegans_P8p_SS)
        rownames(va_liab_Celegans_P8p_SS) <- NULL
        va_liab_Celegans_P8p_SS$Pnp <- c("P8.p","P8.p","P8.p")
        va_liab_Celegans_P8p_SS$Treatment <- c("Control","MA","Vm")
        va_liab_Celegans_P8p_SS$Measure <- c("Va","Va","Va")
        va_liab_Celegans_P8p_SS$Scale <- c("liab","liab","liab")
        va_liab_Celegans_P8p_SS$Variance <- c("Vm","Vm","Vm")
        va_liab_Celegans_P8p_SS
      }
      
      #Summary h2_liab_Celegans_P8p_SS
      {
        mean_h2_liab_Celegans_P8p_SS <- rbind(mean(h2_liab_Celegans_CONTROL_P8p_SS_mod),mean(h2_liab_Celegans_MA_P8p_SS_mod),mean(h2_liab_Celegans_Vm_P8p_SS_mod))
        colnames(mean_h2_liab_Celegans_P8p_SS) <- c("mean")
        median_h2_liab_Celegans_P8p_SS <- rbind(median(h2_liab_Celegans_CONTROL_P8p_SS_mod),median(h2_liab_Celegans_MA_P8p_SS_mod),median(h2_liab_Celegans_Vm_P8p_SS_mod))
        colnames(median_h2_liab_Celegans_P8p_SS) <- c("median")
        posterior.mode_h2_liab_Celegans_P8p_SS <- rbind(posterior.mode(h2_liab_Celegans_CONTROL_P8p_SS_mod),posterior.mode(h2_liab_Celegans_MA_P8p_SS_mod),posterior.mode(h2_liab_Celegans_Vm_P8p_SS_mod))
        colnames(posterior.mode_h2_liab_Celegans_P8p_SS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Celegans_P8p_SS <- rbind(HPDinterval(h2_liab_Celegans_CONTROL_P8p_SS_mod),HPDinterval(h2_liab_Celegans_MA_P8p_SS_mod),HPDinterval(h2_liab_Celegans_Vm_P8p_SS_mod))
        colnames(HPDinterval_0.95_h2_liab_Celegans_P8p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Celegans_P8p_SS <- rbind(HPDinterval(h2_liab_Celegans_CONTROL_P8p_SS_mod,prob=.83),HPDinterval(h2_liab_Celegans_MA_P8p_SS_mod,prob=.83),HPDinterval(h2_liab_Celegans_Vm_P8p_SS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Celegans_P8p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Celegans_P8p_SS <- rbind(effectiveSize(h2_liab_Celegans_CONTROL_P8p_SS_mod),effectiveSize(h2_liab_Celegans_MA_P8p_SS_mod),effectiveSize(h2_liab_Celegans_Vm_P8p_SS_mod))
        colnames(effectiveSize_h2_liab_Celegans_P8p_SS) <- c("effectiveSize")
        h2_liab_Celegans_P8p_SS <- cbind.data.frame(mean_h2_liab_Celegans_P8p_SS,median_h2_liab_Celegans_P8p_SS,posterior.mode_h2_liab_Celegans_P8p_SS,HPDinterval_0.95_h2_liab_Celegans_P8p_SS,HPDinterval_0.83_h2_liab_Celegans_P8p_SS,effectiveSize_h2_liab_Celegans_P8p_SS)
        rownames(h2_liab_Celegans_P8p_SS) <- c("h2_liab_Celegans_CONTROL_P8p_SS_mod","h2_liab_Celegans_MA_P8p_SS_mod","h2_liab_Celegans_Vm_P8p_SS_mod")
        h2_liab_Celegans_P8p_SS <- cbind(Models = rownames(h2_liab_Celegans_P8p_SS),h2_liab_Celegans_P8p_SS)
        rownames(h2_liab_Celegans_P8p_SS) <- NULL
        h2_liab_Celegans_P8p_SS$Pnp <- c("P8.p","P8.p","P8.p")
        h2_liab_Celegans_P8p_SS$Treatment <- c("Control","MA","Vm")
        h2_liab_Celegans_P8p_SS$Measure <- c("H2","H2","H2")
        h2_liab_Celegans_P8p_SS$Scale <- c("liab","liab","liab")
        h2_liab_Celegans_P8p_SS$Variance <- c("Vm","Vm","Vm")
        h2_liab_Celegans_P8p_SS
      }
      
      #Summary Evol_liab_Celegans_P8p_SS
      {
        mean_Evol_liab_Celegans_P8p_SS <- rbind(mean(Evol_liab_Celegans_CONTROL_P8p_SS_mod),mean(Evol_liab_Celegans_MA_P8p_SS_mod),mean(Evol_liab_Celegans_Vm_P8p_SS_mod))
        colnames(mean_Evol_liab_Celegans_P8p_SS) <- c("mean")
        median_Evol_liab_Celegans_P8p_SS <- rbind(median(Evol_liab_Celegans_CONTROL_P8p_SS_mod),median(Evol_liab_Celegans_MA_P8p_SS_mod),median(Evol_liab_Celegans_Vm_P8p_SS_mod))
        colnames(median_Evol_liab_Celegans_P8p_SS) <- c("median")
        posterior.mode_Evol_liab_Celegans_P8p_SS <- rbind(posterior.mode(Evol_liab_Celegans_CONTROL_P8p_SS_mod),posterior.mode(Evol_liab_Celegans_MA_P8p_SS_mod),posterior.mode(Evol_liab_Celegans_Vm_P8p_SS_mod))
        colnames(posterior.mode_Evol_liab_Celegans_P8p_SS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Celegans_P8p_SS <- rbind(HPDinterval(Evol_liab_Celegans_CONTROL_P8p_SS_mod),HPDinterval(Evol_liab_Celegans_MA_P8p_SS_mod),HPDinterval(Evol_liab_Celegans_Vm_P8p_SS_mod))
        colnames(HPDinterval_0.95_Evol_liab_Celegans_P8p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Celegans_P8p_SS <- rbind(HPDinterval(Evol_liab_Celegans_CONTROL_P8p_SS_mod,prob=.83),HPDinterval(Evol_liab_Celegans_MA_P8p_SS_mod,prob=.83),HPDinterval(Evol_liab_Celegans_Vm_P8p_SS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Celegans_P8p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Celegans_P8p_SS <- rbind(effectiveSize(Evol_liab_Celegans_CONTROL_P8p_SS_mod),effectiveSize(Evol_liab_Celegans_MA_P8p_SS_mod),effectiveSize(Evol_liab_Celegans_Vm_P8p_SS_mod))
        colnames(effectiveSize_Evol_liab_Celegans_P8p_SS) <- c("effectiveSize")
        Evol_liab_Celegans_P8p_SS <- cbind.data.frame(mean_Evol_liab_Celegans_P8p_SS,median_Evol_liab_Celegans_P8p_SS,posterior.mode_Evol_liab_Celegans_P8p_SS,HPDinterval_0.95_Evol_liab_Celegans_P8p_SS,HPDinterval_0.83_Evol_liab_Celegans_P8p_SS,effectiveSize_Evol_liab_Celegans_P8p_SS)
        rownames(Evol_liab_Celegans_P8p_SS) <- c("Evol_liab_Celegans_CONTROL_P8p_SS_mod","Evol_liab_Celegans_MA_P8p_SS_mod","Evol_liab_Celegans_Vm_P8p_SS_mod")
        Evol_liab_Celegans_P8p_SS <- cbind(Models = rownames(Evol_liab_Celegans_P8p_SS),Evol_liab_Celegans_P8p_SS)
        rownames(Evol_liab_Celegans_P8p_SS) <- NULL
        Evol_liab_Celegans_P8p_SS$Pnp <- c("P8.p","P8.p","P8.p")
        Evol_liab_Celegans_P8p_SS$Treatment <- c("Control","MA","Vm")
        Evol_liab_Celegans_P8p_SS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Celegans_P8p_SS$Scale <- c("liab","liab","liab")
        Evol_liab_Celegans_P8p_SS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_Celegans_P8p_SS
      }
      
      #Summary trait_mean_liab_Celegans_P8p_SS
      {
        mean_trait_mean_liab_Celegans_P8p_SS <- rbind(mean(trait_mean_liab_Celegans_CONTROL_P8p_SS_mod),mean(trait_mean_liab_Celegans_MA_P8p_SS_mod),mean(trait_mean_liab_Celegans_Vm_P8p_SS_mod))
        colnames(mean_trait_mean_liab_Celegans_P8p_SS) <- c("mean")
        median_trait_mean_liab_Celegans_P8p_SS <- rbind(median(trait_mean_liab_Celegans_CONTROL_P8p_SS_mod),median(trait_mean_liab_Celegans_MA_P8p_SS_mod),median(trait_mean_liab_Celegans_Vm_P8p_SS_mod))
        colnames(median_trait_mean_liab_Celegans_P8p_SS) <- c("median")
        posterior.mode_trait_mean_liab_Celegans_P8p_SS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Celegans_CONTROL_P8p_SS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Celegans_MA_P8p_SS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Celegans_Vm_P8p_SS_mod)))
        colnames(posterior.mode_trait_mean_liab_Celegans_P8p_SS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Celegans_P8p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Celegans_CONTROL_P8p_SS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Celegans_MA_P8p_SS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Celegans_Vm_P8p_SS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_Celegans_P8p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Celegans_P8p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Celegans_CONTROL_P8p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Celegans_MA_P8p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Celegans_Vm_P8p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Celegans_P8p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Celegans_P8p_SS <- rbind(effectiveSize(trait_mean_liab_Celegans_CONTROL_P8p_SS_mod),effectiveSize(trait_mean_liab_Celegans_MA_P8p_SS_mod),effectiveSize(trait_mean_liab_Celegans_Vm_P8p_SS_mod))
        colnames(effectiveSize_trait_mean_liab_Celegans_P8p_SS) <- c("effectiveSize")
        trait_mean_liab_Celegans_P8p_SS <- cbind.data.frame(mean_trait_mean_liab_Celegans_P8p_SS,median_trait_mean_liab_Celegans_P8p_SS,posterior.mode_trait_mean_liab_Celegans_P8p_SS,HPDinterval_0.95_trait_mean_liab_Celegans_P8p_SS,HPDinterval_0.83_trait_mean_liab_Celegans_P8p_SS,effectiveSize_trait_mean_liab_Celegans_P8p_SS)
        rownames(trait_mean_liab_Celegans_P8p_SS) <- c("trait_mean_liab_Celegans_CONTROL_P8p_SS_mod","trait_mean_liab_Celegans_MA_P8p_SS_mod","trait_mean_liab_Celegans_Vm_P8p_SS_mod")
        trait_mean_liab_Celegans_P8p_SS <- cbind(Models = rownames(trait_mean_liab_Celegans_P8p_SS),trait_mean_liab_Celegans_P8p_SS)
        rownames(trait_mean_liab_Celegans_P8p_SS) <- NULL
        trait_mean_liab_Celegans_P8p_SS$Pnp <- c("P8.p","P8.p","P8.p")
        trait_mean_liab_Celegans_P8p_SS$Treatment <- c("Control","MA","Vm")
        trait_mean_liab_Celegans_P8p_SS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Celegans_P8p_SS$Scale <- c("liab","liab","liab")
        trait_mean_liab_Celegans_P8p_SS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_Celegans_P8p_SS
      }
      
      liab_Celegans_P8p_SS <- rbind.data.frame(va_liab_Celegans_P8p_SS, h2_liab_Celegans_P8p_SS,Evol_liab_Celegans_P8p_SS,trait_mean_liab_Celegans_P8p_SS)
      liab_Celegans_P8p_SS
    }
    #Summary data scale Celegans P8p
    {
      #Summary va_data_Celegans_P8p_SS:NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_Celegans_P8p_SS <- rbind(mean(va_data_Celegans_CONTROL_P8p_SS_mod/2),mean(va_data_Celegans_MA_P8p_SS_mod/2),mean(va_data_Celegans_Vm_P8p_SS_mod/2))
        colnames(mean_va_data_Celegans_P8p_SS) <- c("mean")
        median_va_data_Celegans_P8p_SS <- rbind(median(va_data_Celegans_CONTROL_P8p_SS_mod/2),median(va_data_Celegans_MA_P8p_SS_mod/2),median(va_data_Celegans_Vm_P8p_SS_mod/2))
        colnames(median_va_data_Celegans_P8p_SS) <- c("median")
        posterior.mode_va_data_Celegans_P8p_SS <- rbind(posterior.mode(as.mcmc(va_data_Celegans_CONTROL_P8p_SS_mod/2)),posterior.mode(as.mcmc(va_data_Celegans_MA_P8p_SS_mod/2)),posterior.mode(as.mcmc(va_data_Celegans_Vm_P8p_SS_mod/2)))
        colnames(posterior.mode_va_data_Celegans_P8p_SS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Celegans_P8p_SS <- rbind(HPDinterval(as.mcmc(va_data_Celegans_CONTROL_P8p_SS_mod/2)),HPDinterval(as.mcmc(va_data_Celegans_MA_P8p_SS_mod/2)),HPDinterval(as.mcmc(va_data_Celegans_Vm_P8p_SS_mod/2)))
        colnames(HPDinterval_0.95_va_data_Celegans_P8p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Celegans_P8p_SS <- rbind(HPDinterval(as.mcmc(va_data_Celegans_CONTROL_P8p_SS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Celegans_MA_P8p_SS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Celegans_Vm_P8p_SS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Celegans_P8p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Celegans_P8p_SS <- rbind(effectiveSize(va_data_Celegans_CONTROL_P8p_SS_mod/2),effectiveSize(va_data_Celegans_MA_P8p_SS_mod/2),effectiveSize(va_data_Celegans_Vm_P8p_SS_mod/2))
        colnames(effectiveSize_va_data_Celegans_P8p_SS) <- c("effectiveSize")
        va_data_Celegans_P8p_SS <- cbind.data.frame(mean_va_data_Celegans_P8p_SS,median_va_data_Celegans_P8p_SS,posterior.mode_va_data_Celegans_P8p_SS,HPDinterval_0.95_va_data_Celegans_P8p_SS,HPDinterval_0.83_va_data_Celegans_P8p_SS,effectiveSize_va_data_Celegans_P8p_SS)
        rownames(va_data_Celegans_P8p_SS) <- c("va_data_Celegans_CONTROL_P8p_SS_mod","va_data_Celegans_MA_P8p_SS_mod","va_data_Celegans_Vm_P8p_SS_mod")
        va_data_Celegans_P8p_SS <- cbind(Models = rownames(va_data_Celegans_P8p_SS),va_data_Celegans_P8p_SS)
        rownames(va_data_Celegans_P8p_SS) <- NULL
        va_data_Celegans_P8p_SS$Pnp <- c("P8.p","P8.p","P8.p")
        va_data_Celegans_P8p_SS$Treatment <- c("Control","MA","Vm")
        va_data_Celegans_P8p_SS$Measure <- c("Va","Va","Va")
        va_data_Celegans_P8p_SS$Scale <- c("data","data","data")
        va_data_Celegans_P8p_SS$Variance <- c("Vm","Vm","Vm")
        va_data_Celegans_P8p_SS
      }
      
      #Summary h2_data_Celegans_P8p_SS
      {
        mean_h2_data_Celegans_P8p_SS <- rbind(mean(h2_data_Celegans_CONTROL_P8p_SS_mod),mean(h2_data_Celegans_MA_P8p_SS_mod),mean(h2_data_Celegans_Vm_P8p_SS_mod))
        colnames(mean_h2_data_Celegans_P8p_SS) <- c("mean")
        median_h2_data_Celegans_P8p_SS <- rbind(median(h2_data_Celegans_CONTROL_P8p_SS_mod),median(h2_data_Celegans_MA_P8p_SS_mod),median(h2_data_Celegans_Vm_P8p_SS_mod))
        colnames(median_h2_data_Celegans_P8p_SS) <- c("median")
        posterior.mode_h2_data_Celegans_P8p_SS <- rbind(posterior.mode(as.mcmc(h2_data_Celegans_CONTROL_P8p_SS_mod)),posterior.mode(as.mcmc(h2_data_Celegans_MA_P8p_SS_mod)),posterior.mode(as.mcmc(h2_data_Celegans_Vm_P8p_SS_mod)))
        colnames(posterior.mode_h2_data_Celegans_P8p_SS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Celegans_P8p_SS <- rbind(HPDinterval(as.mcmc(h2_data_Celegans_CONTROL_P8p_SS_mod)),HPDinterval(as.mcmc(h2_data_Celegans_MA_P8p_SS_mod)),HPDinterval(as.mcmc(h2_data_Celegans_Vm_P8p_SS_mod)))
        colnames(HPDinterval_0.95_h2_data_Celegans_P8p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Celegans_P8p_SS <- rbind(HPDinterval(as.mcmc(h2_data_Celegans_CONTROL_P8p_SS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Celegans_MA_P8p_SS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Celegans_Vm_P8p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Celegans_P8p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Celegans_P8p_SS <- rbind(effectiveSize(h2_data_Celegans_CONTROL_P8p_SS_mod),effectiveSize(h2_data_Celegans_MA_P8p_SS_mod),effectiveSize(h2_data_Celegans_Vm_P8p_SS_mod))
        colnames(effectiveSize_h2_data_Celegans_P8p_SS) <- c("effectiveSize")
        h2_data_Celegans_P8p_SS <- cbind.data.frame(mean_h2_data_Celegans_P8p_SS,median_h2_data_Celegans_P8p_SS,posterior.mode_h2_data_Celegans_P8p_SS,HPDinterval_0.95_h2_data_Celegans_P8p_SS,HPDinterval_0.83_h2_data_Celegans_P8p_SS,effectiveSize_h2_data_Celegans_P8p_SS)
        rownames(h2_data_Celegans_P8p_SS) <- c("h2_data_Celegans_CONTROL_P8p_SS_mod","h2_data_Celegans_MA_P8p_SS_mod","h2_data_Celegans_Vm_P8p_SS_mod")
        h2_data_Celegans_P8p_SS <- cbind(Models = rownames(h2_data_Celegans_P8p_SS),h2_data_Celegans_P8p_SS)
        rownames(h2_data_Celegans_P8p_SS) <- NULL
        h2_data_Celegans_P8p_SS$Pnp <- c("P8.p","P8.p","P8.p")
        h2_data_Celegans_P8p_SS$Treatment <- c("Control","MA","Vm")
        h2_data_Celegans_P8p_SS$Measure <- c("H2","H2","H2")
        h2_data_Celegans_P8p_SS$Scale <- c("data","data","data")
        h2_data_Celegans_P8p_SS$Variance <- c("Vm","Vm","Vm")
        h2_data_Celegans_P8p_SS
      }
      
      #Summary Evol_data_Celegans_P8p_SS
      {
        mean_Evol_data_Celegans_P8p_SS <- rbind(mean(Evol_data_Celegans_CONTROL_P8p_SS_mod),mean(Evol_data_Celegans_MA_P8p_SS_mod),mean(Evol_data_Celegans_Vm_P8p_SS_mod))
        colnames(mean_Evol_data_Celegans_P8p_SS) <- c("mean")
        median_Evol_data_Celegans_P8p_SS <- rbind(median(Evol_data_Celegans_CONTROL_P8p_SS_mod),median(Evol_data_Celegans_MA_P8p_SS_mod),median(Evol_data_Celegans_Vm_P8p_SS_mod))
        colnames(median_Evol_data_Celegans_P8p_SS) <- c("median")
        posterior.mode_Evol_data_Celegans_P8p_SS <- rbind(posterior.mode(as.mcmc(Evol_data_Celegans_CONTROL_P8p_SS_mod)),posterior.mode(as.mcmc(Evol_data_Celegans_MA_P8p_SS_mod)),posterior.mode(as.mcmc(Evol_data_Celegans_Vm_P8p_SS_mod)))
        colnames(posterior.mode_Evol_data_Celegans_P8p_SS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Celegans_P8p_SS <- rbind(HPDinterval(as.mcmc(Evol_data_Celegans_CONTROL_P8p_SS_mod)),HPDinterval(as.mcmc(Evol_data_Celegans_MA_P8p_SS_mod)),HPDinterval(as.mcmc(Evol_data_Celegans_Vm_P8p_SS_mod)))
        colnames(HPDinterval_0.95_Evol_data_Celegans_P8p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Celegans_P8p_SS <- rbind(HPDinterval(as.mcmc(Evol_data_Celegans_CONTROL_P8p_SS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Celegans_MA_P8p_SS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Celegans_Vm_P8p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Celegans_P8p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Celegans_P8p_SS <- rbind(effectiveSize(Evol_data_Celegans_CONTROL_P8p_SS_mod),effectiveSize(Evol_data_Celegans_MA_P8p_SS_mod),effectiveSize(Evol_data_Celegans_Vm_P8p_SS_mod))
        colnames(effectiveSize_Evol_data_Celegans_P8p_SS) <- c("effectiveSize")
        Evol_data_Celegans_P8p_SS <- cbind.data.frame(mean_Evol_data_Celegans_P8p_SS,median_Evol_data_Celegans_P8p_SS,posterior.mode_Evol_data_Celegans_P8p_SS,HPDinterval_0.95_Evol_data_Celegans_P8p_SS,HPDinterval_0.83_Evol_data_Celegans_P8p_SS,effectiveSize_Evol_data_Celegans_P8p_SS)
        rownames(Evol_data_Celegans_P8p_SS) <- c("Evol_data_Celegans_CONTROL_P8p_SS_mod","Evol_data_Celegans_MA_P8p_SS_mod","Evol_data_Celegans_Vm_P8p_SS_mod")
        Evol_data_Celegans_P8p_SS <- cbind(Models = rownames(Evol_data_Celegans_P8p_SS),Evol_data_Celegans_P8p_SS)
        rownames(Evol_data_Celegans_P8p_SS) <- NULL
        Evol_data_Celegans_P8p_SS$Pnp <- c("P8.p","P8.p","P8.p")
        Evol_data_Celegans_P8p_SS$Treatment <- c("Control","MA","Vm")
        Evol_data_Celegans_P8p_SS$Measure <- c("Evol","Evol","Evol")
        Evol_data_Celegans_P8p_SS$Scale <- c("data","data","data")
        Evol_data_Celegans_P8p_SS$Variance <- c("Vm","Vm","Vm")
        Evol_data_Celegans_P8p_SS
      }
      
      #Summary trait_mean_data_Celegans_P8p_SS
      {
        mean_trait_mean_data_Celegans_P8p_SS <- rbind(mean(trait_mean_data_Celegans_CONTROL_P8p_SS_mod),mean(trait_mean_data_Celegans_MA_P8p_SS_mod),mean(trait_mean_data_Celegans_Vm_P8p_SS_mod))
        colnames(mean_trait_mean_data_Celegans_P8p_SS) <- c("mean")
        median_trait_mean_data_Celegans_P8p_SS <- rbind(median(trait_mean_data_Celegans_CONTROL_P8p_SS_mod),median(trait_mean_data_Celegans_MA_P8p_SS_mod),median(trait_mean_data_Celegans_Vm_P8p_SS_mod))
        colnames(median_trait_mean_data_Celegans_P8p_SS) <- c("median")
        posterior.mode_trait_mean_data_Celegans_P8p_SS <- rbind(posterior.mode(as.mcmc(trait_mean_data_Celegans_CONTROL_P8p_SS_mod)),posterior.mode(as.mcmc(trait_mean_data_Celegans_MA_P8p_SS_mod)),posterior.mode(as.mcmc(trait_mean_data_Celegans_Vm_P8p_SS_mod)))
        colnames(posterior.mode_trait_mean_data_Celegans_P8p_SS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Celegans_P8p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_data_Celegans_CONTROL_P8p_SS_mod)),HPDinterval(as.mcmc(trait_mean_data_Celegans_MA_P8p_SS_mod)),HPDinterval(as.mcmc(trait_mean_data_Celegans_Vm_P8p_SS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_Celegans_P8p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Celegans_P8p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_data_Celegans_CONTROL_P8p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Celegans_MA_P8p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Celegans_Vm_P8p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Celegans_P8p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Celegans_P8p_SS <- rbind(effectiveSize(trait_mean_data_Celegans_CONTROL_P8p_SS_mod),effectiveSize(trait_mean_data_Celegans_MA_P8p_SS_mod),effectiveSize(trait_mean_data_Celegans_Vm_P8p_SS_mod))
        colnames(effectiveSize_trait_mean_data_Celegans_P8p_SS) <- c("effectiveSize")
        trait_mean_data_Celegans_P8p_SS <- cbind.data.frame(mean_trait_mean_data_Celegans_P8p_SS,median_trait_mean_data_Celegans_P8p_SS,posterior.mode_trait_mean_data_Celegans_P8p_SS,HPDinterval_0.95_trait_mean_data_Celegans_P8p_SS,HPDinterval_0.83_trait_mean_data_Celegans_P8p_SS,effectiveSize_trait_mean_data_Celegans_P8p_SS)
        rownames(trait_mean_data_Celegans_P8p_SS) <- c("trait_mean_data_Celegans_CONTROL_P8p_SS_mod","trait_mean_data_Celegans_MA_P8p_SS_mod","trait_mean_data_Celegans_Vm_P8p_SS_mod")
        trait_mean_data_Celegans_P8p_SS <- cbind(Models = rownames(trait_mean_data_Celegans_P8p_SS),trait_mean_data_Celegans_P8p_SS)
        rownames(trait_mean_data_Celegans_P8p_SS) <- NULL
        trait_mean_data_Celegans_P8p_SS$Pnp <- c("P8.p","P8.p","P8.p")
        trait_mean_data_Celegans_P8p_SS$Treatment <- c("Control","MA","Vm")
        trait_mean_data_Celegans_P8p_SS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Celegans_P8p_SS$Scale <- c("data","data","data")
        trait_mean_data_Celegans_P8p_SS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_Celegans_P8p_SS
      }
      
      data_Celegans_P8p_SS <- rbind.data.frame(va_data_Celegans_P8p_SS, h2_data_Celegans_P8p_SS,Evol_data_Celegans_P8p_SS,trait_mean_data_Celegans_P8p_SS)
      data_Celegans_P8p_SS
      
    }
    Vm_Celegans_P8p_SS <- rbind.data.frame(liab_Celegans_P8p_SS, data_Celegans_P8p_SS)
    Vm_Celegans_P8p_SS$Pnp_fate <- rep("SS", 24)
    Vm_Celegans_P8p_SS
    #remove Celegans P8p_SS models
    {
      remove(Celegans_CONTROL_P8p_SS_mod)
      remove(Celegans_MA_P8p_SS_mod)
      remove(Celegans_Vm_P8p_SS_mod)
    }
  }
  
  #Summary Celegans P5p
  {
    #Summary liability scale Celegans P5p
    {
      #Summary va_liab_Celegans_P5p_wt: NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_Celegans_P5p_wt <- rbind(mean(va_liab_Celegans_CONTROL_P5p_wt_mod/2),mean(va_liab_Celegans_MA_P5p_wt_mod/2),mean(va_liab_Celegans_Vm_P5p_wt_mod/2))
        colnames(mean_va_liab_Celegans_P5p_wt) <- c("mean")
        median_va_liab_Celegans_P5p_wt <- rbind(median(va_liab_Celegans_CONTROL_P5p_wt_mod/2),median(va_liab_Celegans_MA_P5p_wt_mod/2),median(va_liab_Celegans_Vm_P5p_wt_mod/2))
        colnames(median_va_liab_Celegans_P5p_wt) <- c("median")
        posterior.mode_va_liab_Celegans_P5p_wt <- rbind(posterior.mode(va_liab_Celegans_CONTROL_P5p_wt_mod/2),posterior.mode(va_liab_Celegans_MA_P5p_wt_mod/2),posterior.mode(va_liab_Celegans_Vm_P5p_wt_mod/2))
        colnames(posterior.mode_va_liab_Celegans_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Celegans_P5p_wt <- rbind(HPDinterval(va_liab_Celegans_CONTROL_P5p_wt_mod/2),HPDinterval(va_liab_Celegans_MA_P5p_wt_mod/2),HPDinterval(va_liab_Celegans_Vm_P5p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_Celegans_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Celegans_P5p_wt <- rbind(HPDinterval(va_liab_Celegans_CONTROL_P5p_wt_mod/2,prob=.83),HPDinterval(va_liab_Celegans_MA_P5p_wt_mod/2,prob=.83),HPDinterval(va_liab_Celegans_Vm_P5p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Celegans_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Celegans_P5p_wt <- rbind(effectiveSize(va_liab_Celegans_CONTROL_P5p_wt_mod/2),effectiveSize(va_liab_Celegans_MA_P5p_wt_mod/2),effectiveSize(va_liab_Celegans_Vm_P5p_wt_mod/2))
        colnames(effectiveSize_va_liab_Celegans_P5p_wt) <- c("effectiveSize")
        va_liab_Celegans_P5p_wt <- cbind.data.frame(mean_va_liab_Celegans_P5p_wt,median_va_liab_Celegans_P5p_wt,posterior.mode_va_liab_Celegans_P5p_wt,HPDinterval_0.95_va_liab_Celegans_P5p_wt,HPDinterval_0.83_va_liab_Celegans_P5p_wt,effectiveSize_va_liab_Celegans_P5p_wt)
        rownames(va_liab_Celegans_P5p_wt) <- c("va_liab_Celegans_CONTROL_P5p_wt_mod","va_liab_Celegans_MA_P5p_wt_mod","va_liab_Celegans_Vm_P5p_wt_mod")
        va_liab_Celegans_P5p_wt <- cbind(Models = rownames(va_liab_Celegans_P5p_wt),va_liab_Celegans_P5p_wt)
        rownames(va_liab_Celegans_P5p_wt) <- NULL
        va_liab_Celegans_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        va_liab_Celegans_P5p_wt$Treatment <- c("Control","MA","Vm")
        va_liab_Celegans_P5p_wt$Measure <- c("Va","Va","Va")
        va_liab_Celegans_P5p_wt$Scale <- c("liab","liab","liab")
        va_liab_Celegans_P5p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_Celegans_P5p_wt
      }
      
      #Summary h2_liab_Celegans_P5p_wt
      {
        mean_h2_liab_Celegans_P5p_wt <- rbind(mean(h2_liab_Celegans_CONTROL_P5p_wt_mod),mean(h2_liab_Celegans_MA_P5p_wt_mod),mean(h2_liab_Celegans_Vm_P5p_wt_mod))
        colnames(mean_h2_liab_Celegans_P5p_wt) <- c("mean")
        median_h2_liab_Celegans_P5p_wt <- rbind(median(h2_liab_Celegans_CONTROL_P5p_wt_mod),median(h2_liab_Celegans_MA_P5p_wt_mod),median(h2_liab_Celegans_Vm_P5p_wt_mod))
        colnames(median_h2_liab_Celegans_P5p_wt) <- c("median")
        posterior.mode_h2_liab_Celegans_P5p_wt <- rbind(posterior.mode(h2_liab_Celegans_CONTROL_P5p_wt_mod),posterior.mode(h2_liab_Celegans_MA_P5p_wt_mod),posterior.mode(h2_liab_Celegans_Vm_P5p_wt_mod))
        colnames(posterior.mode_h2_liab_Celegans_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Celegans_P5p_wt <- rbind(HPDinterval(h2_liab_Celegans_CONTROL_P5p_wt_mod),HPDinterval(h2_liab_Celegans_MA_P5p_wt_mod),HPDinterval(h2_liab_Celegans_Vm_P5p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_Celegans_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Celegans_P5p_wt <- rbind(HPDinterval(h2_liab_Celegans_CONTROL_P5p_wt_mod,prob=.83),HPDinterval(h2_liab_Celegans_MA_P5p_wt_mod,prob=.83),HPDinterval(h2_liab_Celegans_Vm_P5p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Celegans_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Celegans_P5p_wt <- rbind(effectiveSize(h2_liab_Celegans_CONTROL_P5p_wt_mod),effectiveSize(h2_liab_Celegans_MA_P5p_wt_mod),effectiveSize(h2_liab_Celegans_Vm_P5p_wt_mod))
        colnames(effectiveSize_h2_liab_Celegans_P5p_wt) <- c("effectiveSize")
        h2_liab_Celegans_P5p_wt <- cbind.data.frame(mean_h2_liab_Celegans_P5p_wt,median_h2_liab_Celegans_P5p_wt,posterior.mode_h2_liab_Celegans_P5p_wt,HPDinterval_0.95_h2_liab_Celegans_P5p_wt,HPDinterval_0.83_h2_liab_Celegans_P5p_wt,effectiveSize_h2_liab_Celegans_P5p_wt)
        rownames(h2_liab_Celegans_P5p_wt) <- c("h2_liab_Celegans_CONTROL_P5p_wt_mod","h2_liab_Celegans_MA_P5p_wt_mod","h2_liab_Celegans_Vm_P5p_wt_mod")
        h2_liab_Celegans_P5p_wt <- cbind(Models = rownames(h2_liab_Celegans_P5p_wt),h2_liab_Celegans_P5p_wt)
        rownames(h2_liab_Celegans_P5p_wt) <- NULL
        h2_liab_Celegans_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        h2_liab_Celegans_P5p_wt$Treatment <- c("Control","MA","Vm")
        h2_liab_Celegans_P5p_wt$Measure <- c("H2","H2","H2")
        h2_liab_Celegans_P5p_wt$Scale <- c("liab","liab","liab")
        h2_liab_Celegans_P5p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_Celegans_P5p_wt
      }
      
      #Summary Evol_liab_Celegans_P5p_wt
      {
        mean_Evol_liab_Celegans_P5p_wt <- rbind(mean(Evol_liab_Celegans_CONTROL_P5p_wt_mod),mean(Evol_liab_Celegans_MA_P5p_wt_mod),mean(Evol_liab_Celegans_Vm_P5p_wt_mod))
        colnames(mean_Evol_liab_Celegans_P5p_wt) <- c("mean")
        median_Evol_liab_Celegans_P5p_wt <- rbind(median(Evol_liab_Celegans_CONTROL_P5p_wt_mod),median(Evol_liab_Celegans_MA_P5p_wt_mod),median(Evol_liab_Celegans_Vm_P5p_wt_mod))
        colnames(median_Evol_liab_Celegans_P5p_wt) <- c("median")
        posterior.mode_Evol_liab_Celegans_P5p_wt <- rbind(posterior.mode(Evol_liab_Celegans_CONTROL_P5p_wt_mod),posterior.mode(Evol_liab_Celegans_MA_P5p_wt_mod),posterior.mode(Evol_liab_Celegans_Vm_P5p_wt_mod))
        colnames(posterior.mode_Evol_liab_Celegans_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Celegans_P5p_wt <- rbind(HPDinterval(Evol_liab_Celegans_CONTROL_P5p_wt_mod),HPDinterval(Evol_liab_Celegans_MA_P5p_wt_mod),HPDinterval(Evol_liab_Celegans_Vm_P5p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_Celegans_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Celegans_P5p_wt <- rbind(HPDinterval(Evol_liab_Celegans_CONTROL_P5p_wt_mod,prob=.83),HPDinterval(Evol_liab_Celegans_MA_P5p_wt_mod,prob=.83),HPDinterval(Evol_liab_Celegans_Vm_P5p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Celegans_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Celegans_P5p_wt <- rbind(effectiveSize(Evol_liab_Celegans_CONTROL_P5p_wt_mod),effectiveSize(Evol_liab_Celegans_MA_P5p_wt_mod),effectiveSize(Evol_liab_Celegans_Vm_P5p_wt_mod))
        colnames(effectiveSize_Evol_liab_Celegans_P5p_wt) <- c("effectiveSize")
        Evol_liab_Celegans_P5p_wt <- cbind.data.frame(mean_Evol_liab_Celegans_P5p_wt,median_Evol_liab_Celegans_P5p_wt,posterior.mode_Evol_liab_Celegans_P5p_wt,HPDinterval_0.95_Evol_liab_Celegans_P5p_wt,HPDinterval_0.83_Evol_liab_Celegans_P5p_wt,effectiveSize_Evol_liab_Celegans_P5p_wt)
        rownames(Evol_liab_Celegans_P5p_wt) <- c("Evol_liab_Celegans_CONTROL_P5p_wt_mod","Evol_liab_Celegans_MA_P5p_wt_mod","Evol_liab_Celegans_Vm_P5p_wt_mod")
        Evol_liab_Celegans_P5p_wt <- cbind(Models = rownames(Evol_liab_Celegans_P5p_wt),Evol_liab_Celegans_P5p_wt)
        rownames(Evol_liab_Celegans_P5p_wt) <- NULL
        Evol_liab_Celegans_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        Evol_liab_Celegans_P5p_wt$Treatment <- c("Control","MA","Vm")
        Evol_liab_Celegans_P5p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Celegans_P5p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_Celegans_P5p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_Celegans_P5p_wt
      }
      
      #Summary trait_mean_liab_Celegans_P5p_wt
      {
        mean_trait_mean_liab_Celegans_P5p_wt <- rbind(mean(trait_mean_liab_Celegans_CONTROL_P5p_wt_mod),mean(trait_mean_liab_Celegans_MA_P5p_wt_mod),mean(trait_mean_liab_Celegans_Vm_P5p_wt_mod))
        colnames(mean_trait_mean_liab_Celegans_P5p_wt) <- c("mean")
        median_trait_mean_liab_Celegans_P5p_wt <- rbind(median(trait_mean_liab_Celegans_CONTROL_P5p_wt_mod),median(trait_mean_liab_Celegans_MA_P5p_wt_mod),median(trait_mean_liab_Celegans_Vm_P5p_wt_mod))
        colnames(median_trait_mean_liab_Celegans_P5p_wt) <- c("median")
        posterior.mode_trait_mean_liab_Celegans_P5p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Celegans_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Celegans_MA_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Celegans_Vm_P5p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_Celegans_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Celegans_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Celegans_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Celegans_MA_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Celegans_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_Celegans_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Celegans_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Celegans_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Celegans_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Celegans_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Celegans_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Celegans_P5p_wt <- rbind(effectiveSize(trait_mean_liab_Celegans_CONTROL_P5p_wt_mod),effectiveSize(trait_mean_liab_Celegans_MA_P5p_wt_mod),effectiveSize(trait_mean_liab_Celegans_Vm_P5p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_Celegans_P5p_wt) <- c("effectiveSize")
        trait_mean_liab_Celegans_P5p_wt <- cbind.data.frame(mean_trait_mean_liab_Celegans_P5p_wt,median_trait_mean_liab_Celegans_P5p_wt,posterior.mode_trait_mean_liab_Celegans_P5p_wt,HPDinterval_0.95_trait_mean_liab_Celegans_P5p_wt,HPDinterval_0.83_trait_mean_liab_Celegans_P5p_wt,effectiveSize_trait_mean_liab_Celegans_P5p_wt)
        rownames(trait_mean_liab_Celegans_P5p_wt) <- c("trait_mean_liab_Celegans_CONTROL_P5p_wt_mod","trait_mean_liab_Celegans_MA_P5p_wt_mod","trait_mean_liab_Celegans_Vm_P5p_wt_mod")
        trait_mean_liab_Celegans_P5p_wt <- cbind(Models = rownames(trait_mean_liab_Celegans_P5p_wt),trait_mean_liab_Celegans_P5p_wt)
        rownames(trait_mean_liab_Celegans_P5p_wt) <- NULL
        trait_mean_liab_Celegans_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        trait_mean_liab_Celegans_P5p_wt$Treatment <- c("Control","MA","Vm")
        trait_mean_liab_Celegans_P5p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Celegans_P5p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_Celegans_P5p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_Celegans_P5p_wt
      }
      
      liab_Celegans_P5p_wt <- rbind.data.frame(va_liab_Celegans_P5p_wt, h2_liab_Celegans_P5p_wt,Evol_liab_Celegans_P5p_wt,trait_mean_liab_Celegans_P5p_wt)
      liab_Celegans_P5p_wt
    }
    #Summary data scale Celegans P5p
    {
      #Summary va_data_Celegans_P5p_wt:NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_Celegans_P5p_wt <- rbind(mean(va_data_Celegans_CONTROL_P5p_wt_mod/2),mean(va_data_Celegans_MA_P5p_wt_mod/2),mean(va_data_Celegans_Vm_P5p_wt_mod/2))
        colnames(mean_va_data_Celegans_P5p_wt) <- c("mean")
        median_va_data_Celegans_P5p_wt <- rbind(median(va_data_Celegans_CONTROL_P5p_wt_mod/2),median(va_data_Celegans_MA_P5p_wt_mod/2),median(va_data_Celegans_Vm_P5p_wt_mod/2))
        colnames(median_va_data_Celegans_P5p_wt) <- c("median")
        posterior.mode_va_data_Celegans_P5p_wt <- rbind(posterior.mode(as.mcmc(va_data_Celegans_CONTROL_P5p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Celegans_MA_P5p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Celegans_Vm_P5p_wt_mod/2)))
        colnames(posterior.mode_va_data_Celegans_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Celegans_P5p_wt <- rbind(HPDinterval(as.mcmc(va_data_Celegans_CONTROL_P5p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Celegans_MA_P5p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Celegans_Vm_P5p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_Celegans_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Celegans_P5p_wt <- rbind(HPDinterval(as.mcmc(va_data_Celegans_CONTROL_P5p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Celegans_MA_P5p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Celegans_Vm_P5p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Celegans_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Celegans_P5p_wt <- rbind(effectiveSize(va_data_Celegans_CONTROL_P5p_wt_mod/2),effectiveSize(va_data_Celegans_MA_P5p_wt_mod/2),effectiveSize(va_data_Celegans_Vm_P5p_wt_mod/2))
        colnames(effectiveSize_va_data_Celegans_P5p_wt) <- c("effectiveSize")
        va_data_Celegans_P5p_wt <- cbind.data.frame(mean_va_data_Celegans_P5p_wt,median_va_data_Celegans_P5p_wt,posterior.mode_va_data_Celegans_P5p_wt,HPDinterval_0.95_va_data_Celegans_P5p_wt,HPDinterval_0.83_va_data_Celegans_P5p_wt,effectiveSize_va_data_Celegans_P5p_wt)
        rownames(va_data_Celegans_P5p_wt) <- c("va_data_Celegans_CONTROL_P5p_wt_mod","va_data_Celegans_MA_P5p_wt_mod","va_data_Celegans_Vm_P5p_wt_mod")
        va_data_Celegans_P5p_wt <- cbind(Models = rownames(va_data_Celegans_P5p_wt),va_data_Celegans_P5p_wt)
        rownames(va_data_Celegans_P5p_wt) <- NULL
        va_data_Celegans_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        va_data_Celegans_P5p_wt$Treatment <- c("Control","MA","Vm")
        va_data_Celegans_P5p_wt$Measure <- c("Va","Va","Va")
        va_data_Celegans_P5p_wt$Scale <- c("data","data","data")
        va_data_Celegans_P5p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_Celegans_P5p_wt
      }
      
      #Summary h2_data_Celegans_P5p_wt
      {
        mean_h2_data_Celegans_P5p_wt <- rbind(mean(h2_data_Celegans_CONTROL_P5p_wt_mod),mean(h2_data_Celegans_MA_P5p_wt_mod),mean(h2_data_Celegans_Vm_P5p_wt_mod))
        colnames(mean_h2_data_Celegans_P5p_wt) <- c("mean")
        median_h2_data_Celegans_P5p_wt <- rbind(median(h2_data_Celegans_CONTROL_P5p_wt_mod),median(h2_data_Celegans_MA_P5p_wt_mod),median(h2_data_Celegans_Vm_P5p_wt_mod))
        colnames(median_h2_data_Celegans_P5p_wt) <- c("median")
        posterior.mode_h2_data_Celegans_P5p_wt <- rbind(posterior.mode(as.mcmc(h2_data_Celegans_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(h2_data_Celegans_MA_P5p_wt_mod)),posterior.mode(as.mcmc(h2_data_Celegans_Vm_P5p_wt_mod)))
        colnames(posterior.mode_h2_data_Celegans_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Celegans_P5p_wt <- rbind(HPDinterval(as.mcmc(h2_data_Celegans_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(h2_data_Celegans_MA_P5p_wt_mod)),HPDinterval(as.mcmc(h2_data_Celegans_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_Celegans_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Celegans_P5p_wt <- rbind(HPDinterval(as.mcmc(h2_data_Celegans_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Celegans_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Celegans_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Celegans_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Celegans_P5p_wt <- rbind(effectiveSize(h2_data_Celegans_CONTROL_P5p_wt_mod),effectiveSize(h2_data_Celegans_MA_P5p_wt_mod),effectiveSize(h2_data_Celegans_Vm_P5p_wt_mod))
        colnames(effectiveSize_h2_data_Celegans_P5p_wt) <- c("effectiveSize")
        h2_data_Celegans_P5p_wt <- cbind.data.frame(mean_h2_data_Celegans_P5p_wt,median_h2_data_Celegans_P5p_wt,posterior.mode_h2_data_Celegans_P5p_wt,HPDinterval_0.95_h2_data_Celegans_P5p_wt,HPDinterval_0.83_h2_data_Celegans_P5p_wt,effectiveSize_h2_data_Celegans_P5p_wt)
        rownames(h2_data_Celegans_P5p_wt) <- c("h2_data_Celegans_CONTROL_P5p_wt_mod","h2_data_Celegans_MA_P5p_wt_mod","h2_data_Celegans_Vm_P5p_wt_mod")
        h2_data_Celegans_P5p_wt <- cbind(Models = rownames(h2_data_Celegans_P5p_wt),h2_data_Celegans_P5p_wt)
        rownames(h2_data_Celegans_P5p_wt) <- NULL
        h2_data_Celegans_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        h2_data_Celegans_P5p_wt$Treatment <- c("Control","MA","Vm")
        h2_data_Celegans_P5p_wt$Measure <- c("H2","H2","H2")
        h2_data_Celegans_P5p_wt$Scale <- c("data","data","data")
        h2_data_Celegans_P5p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_Celegans_P5p_wt
      }
      
      #Summary Evol_data_Celegans_P5p_wt
      {
        mean_Evol_data_Celegans_P5p_wt <- rbind(mean(Evol_data_Celegans_CONTROL_P5p_wt_mod),mean(Evol_data_Celegans_MA_P5p_wt_mod),mean(Evol_data_Celegans_Vm_P5p_wt_mod))
        colnames(mean_Evol_data_Celegans_P5p_wt) <- c("mean")
        median_Evol_data_Celegans_P5p_wt <- rbind(median(Evol_data_Celegans_CONTROL_P5p_wt_mod),median(Evol_data_Celegans_MA_P5p_wt_mod),median(Evol_data_Celegans_Vm_P5p_wt_mod))
        colnames(median_Evol_data_Celegans_P5p_wt) <- c("median")
        posterior.mode_Evol_data_Celegans_P5p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_Celegans_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Celegans_MA_P5p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Celegans_Vm_P5p_wt_mod)))
        colnames(posterior.mode_Evol_data_Celegans_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Celegans_P5p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_Celegans_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Celegans_MA_P5p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Celegans_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_Celegans_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Celegans_P5p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_Celegans_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Celegans_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Celegans_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Celegans_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Celegans_P5p_wt <- rbind(effectiveSize(Evol_data_Celegans_CONTROL_P5p_wt_mod),effectiveSize(Evol_data_Celegans_MA_P5p_wt_mod),effectiveSize(Evol_data_Celegans_Vm_P5p_wt_mod))
        colnames(effectiveSize_Evol_data_Celegans_P5p_wt) <- c("effectiveSize")
        Evol_data_Celegans_P5p_wt <- cbind.data.frame(mean_Evol_data_Celegans_P5p_wt,median_Evol_data_Celegans_P5p_wt,posterior.mode_Evol_data_Celegans_P5p_wt,HPDinterval_0.95_Evol_data_Celegans_P5p_wt,HPDinterval_0.83_Evol_data_Celegans_P5p_wt,effectiveSize_Evol_data_Celegans_P5p_wt)
        rownames(Evol_data_Celegans_P5p_wt) <- c("Evol_data_Celegans_CONTROL_P5p_wt_mod","Evol_data_Celegans_MA_P5p_wt_mod","Evol_data_Celegans_Vm_P5p_wt_mod")
        Evol_data_Celegans_P5p_wt <- cbind(Models = rownames(Evol_data_Celegans_P5p_wt),Evol_data_Celegans_P5p_wt)
        rownames(Evol_data_Celegans_P5p_wt) <- NULL
        Evol_data_Celegans_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        Evol_data_Celegans_P5p_wt$Treatment <- c("Control","MA","Vm")
        Evol_data_Celegans_P5p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_Celegans_P5p_wt$Scale <- c("data","data","data")
        Evol_data_Celegans_P5p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_Celegans_P5p_wt
      }
      
      #Summary trait_mean_data_Celegans_P5p_wt
      {
        mean_trait_mean_data_Celegans_P5p_wt <- rbind(mean(trait_mean_data_Celegans_CONTROL_P5p_wt_mod),mean(trait_mean_data_Celegans_MA_P5p_wt_mod),mean(trait_mean_data_Celegans_Vm_P5p_wt_mod))
        colnames(mean_trait_mean_data_Celegans_P5p_wt) <- c("mean")
        median_trait_mean_data_Celegans_P5p_wt <- rbind(median(trait_mean_data_Celegans_CONTROL_P5p_wt_mod),median(trait_mean_data_Celegans_MA_P5p_wt_mod),median(trait_mean_data_Celegans_Vm_P5p_wt_mod))
        colnames(median_trait_mean_data_Celegans_P5p_wt) <- c("median")
        posterior.mode_trait_mean_data_Celegans_P5p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_Celegans_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Celegans_MA_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Celegans_Vm_P5p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_Celegans_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Celegans_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_Celegans_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Celegans_MA_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Celegans_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_Celegans_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Celegans_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_Celegans_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Celegans_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Celegans_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Celegans_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Celegans_P5p_wt <- rbind(effectiveSize(trait_mean_data_Celegans_CONTROL_P5p_wt_mod),effectiveSize(trait_mean_data_Celegans_MA_P5p_wt_mod),effectiveSize(trait_mean_data_Celegans_Vm_P5p_wt_mod))
        colnames(effectiveSize_trait_mean_data_Celegans_P5p_wt) <- c("effectiveSize")
        trait_mean_data_Celegans_P5p_wt <- cbind.data.frame(mean_trait_mean_data_Celegans_P5p_wt,median_trait_mean_data_Celegans_P5p_wt,posterior.mode_trait_mean_data_Celegans_P5p_wt,HPDinterval_0.95_trait_mean_data_Celegans_P5p_wt,HPDinterval_0.83_trait_mean_data_Celegans_P5p_wt,effectiveSize_trait_mean_data_Celegans_P5p_wt)
        rownames(trait_mean_data_Celegans_P5p_wt) <- c("trait_mean_data_Celegans_CONTROL_P5p_wt_mod","trait_mean_data_Celegans_MA_P5p_wt_mod","trait_mean_data_Celegans_Vm_P5p_wt_mod")
        trait_mean_data_Celegans_P5p_wt <- cbind(Models = rownames(trait_mean_data_Celegans_P5p_wt),trait_mean_data_Celegans_P5p_wt)
        rownames(trait_mean_data_Celegans_P5p_wt) <- NULL
        trait_mean_data_Celegans_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        trait_mean_data_Celegans_P5p_wt$Treatment <- c("Control","MA","Vm")
        trait_mean_data_Celegans_P5p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Celegans_P5p_wt$Scale <- c("data","data","data")
        trait_mean_data_Celegans_P5p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_Celegans_P5p_wt
      }
      
      data_Celegans_P5p_wt <- rbind.data.frame(va_data_Celegans_P5p_wt, h2_data_Celegans_P5p_wt,Evol_data_Celegans_P5p_wt,trait_mean_data_Celegans_P5p_wt)
      data_Celegans_P5p_wt
      
    }
    Vm_Celegans_P5p_wt <- rbind.data.frame(liab_Celegans_P5p_wt, data_Celegans_P5p_wt)
    Vm_Celegans_P5p_wt$Pnp_fate <- rep("wt", 24)
    Vm_Celegans_P5p_wt
    #remove Celegans P5p_wt models
    {
      remove(Celegans_CONTROL_P5p_wt_mod)
      remove(Celegans_MA_P5p_wt_mod)
      remove(Celegans_Vm_P5p_wt_mod)
    }
  }
  
  #Summary Celegans P6p
  {
    #Summary liability scale Celegans P6p
    {
      #Summary va_liab_Celegans_P6p_wt: NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_Celegans_P6p_wt <- rbind(mean(va_liab_Celegans_CONTROL_P6p_wt_mod/2),mean(va_liab_Celegans_MA_P6p_wt_mod/2),mean(va_liab_Celegans_Vm_P6p_wt_mod/2))
        colnames(mean_va_liab_Celegans_P6p_wt) <- c("mean")
        median_va_liab_Celegans_P6p_wt <- rbind(median(va_liab_Celegans_CONTROL_P6p_wt_mod/2),median(va_liab_Celegans_MA_P6p_wt_mod/2),median(va_liab_Celegans_Vm_P6p_wt_mod/2))
        colnames(median_va_liab_Celegans_P6p_wt) <- c("median")
        posterior.mode_va_liab_Celegans_P6p_wt <- rbind(posterior.mode(va_liab_Celegans_CONTROL_P6p_wt_mod/2),posterior.mode(va_liab_Celegans_MA_P6p_wt_mod/2),posterior.mode(va_liab_Celegans_Vm_P6p_wt_mod/2))
        colnames(posterior.mode_va_liab_Celegans_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Celegans_P6p_wt <- rbind(HPDinterval(va_liab_Celegans_CONTROL_P6p_wt_mod/2),HPDinterval(va_liab_Celegans_MA_P6p_wt_mod/2),HPDinterval(va_liab_Celegans_Vm_P6p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_Celegans_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Celegans_P6p_wt <- rbind(HPDinterval(va_liab_Celegans_CONTROL_P6p_wt_mod/2,prob=.83),HPDinterval(va_liab_Celegans_MA_P6p_wt_mod/2,prob=.83),HPDinterval(va_liab_Celegans_Vm_P6p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Celegans_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Celegans_P6p_wt <- rbind(effectiveSize(va_liab_Celegans_CONTROL_P6p_wt_mod/2),effectiveSize(va_liab_Celegans_MA_P6p_wt_mod/2),effectiveSize(va_liab_Celegans_Vm_P6p_wt_mod/2))
        colnames(effectiveSize_va_liab_Celegans_P6p_wt) <- c("effectiveSize")
        va_liab_Celegans_P6p_wt <- cbind.data.frame(mean_va_liab_Celegans_P6p_wt,median_va_liab_Celegans_P6p_wt,posterior.mode_va_liab_Celegans_P6p_wt,HPDinterval_0.95_va_liab_Celegans_P6p_wt,HPDinterval_0.83_va_liab_Celegans_P6p_wt,effectiveSize_va_liab_Celegans_P6p_wt)
        rownames(va_liab_Celegans_P6p_wt) <- c("va_liab_Celegans_CONTROL_P6p_wt_mod","va_liab_Celegans_MA_P6p_wt_mod","va_liab_Celegans_Vm_P6p_wt_mod")
        va_liab_Celegans_P6p_wt <- cbind(Models = rownames(va_liab_Celegans_P6p_wt),va_liab_Celegans_P6p_wt)
        rownames(va_liab_Celegans_P6p_wt) <- NULL
        va_liab_Celegans_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        va_liab_Celegans_P6p_wt$Treatment <- c("Control","MA","Vm")
        va_liab_Celegans_P6p_wt$Measure <- c("Va","Va","Va")
        va_liab_Celegans_P6p_wt$Scale <- c("liab","liab","liab")
        va_liab_Celegans_P6p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_Celegans_P6p_wt
      }
      
      #Summary h2_liab_Celegans_P6p_wt
      {
        mean_h2_liab_Celegans_P6p_wt <- rbind(mean(h2_liab_Celegans_CONTROL_P6p_wt_mod),mean(h2_liab_Celegans_MA_P6p_wt_mod),mean(h2_liab_Celegans_Vm_P6p_wt_mod))
        colnames(mean_h2_liab_Celegans_P6p_wt) <- c("mean")
        median_h2_liab_Celegans_P6p_wt <- rbind(median(h2_liab_Celegans_CONTROL_P6p_wt_mod),median(h2_liab_Celegans_MA_P6p_wt_mod),median(h2_liab_Celegans_Vm_P6p_wt_mod))
        colnames(median_h2_liab_Celegans_P6p_wt) <- c("median")
        posterior.mode_h2_liab_Celegans_P6p_wt <- rbind(posterior.mode(h2_liab_Celegans_CONTROL_P6p_wt_mod),posterior.mode(h2_liab_Celegans_MA_P6p_wt_mod),posterior.mode(h2_liab_Celegans_Vm_P6p_wt_mod))
        colnames(posterior.mode_h2_liab_Celegans_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Celegans_P6p_wt <- rbind(HPDinterval(h2_liab_Celegans_CONTROL_P6p_wt_mod),HPDinterval(h2_liab_Celegans_MA_P6p_wt_mod),HPDinterval(h2_liab_Celegans_Vm_P6p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_Celegans_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Celegans_P6p_wt <- rbind(HPDinterval(h2_liab_Celegans_CONTROL_P6p_wt_mod,prob=.83),HPDinterval(h2_liab_Celegans_MA_P6p_wt_mod,prob=.83),HPDinterval(h2_liab_Celegans_Vm_P6p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Celegans_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Celegans_P6p_wt <- rbind(effectiveSize(h2_liab_Celegans_CONTROL_P6p_wt_mod),effectiveSize(h2_liab_Celegans_MA_P6p_wt_mod),effectiveSize(h2_liab_Celegans_Vm_P6p_wt_mod))
        colnames(effectiveSize_h2_liab_Celegans_P6p_wt) <- c("effectiveSize")
        h2_liab_Celegans_P6p_wt <- cbind.data.frame(mean_h2_liab_Celegans_P6p_wt,median_h2_liab_Celegans_P6p_wt,posterior.mode_h2_liab_Celegans_P6p_wt,HPDinterval_0.95_h2_liab_Celegans_P6p_wt,HPDinterval_0.83_h2_liab_Celegans_P6p_wt,effectiveSize_h2_liab_Celegans_P6p_wt)
        rownames(h2_liab_Celegans_P6p_wt) <- c("h2_liab_Celegans_CONTROL_P6p_wt_mod","h2_liab_Celegans_MA_P6p_wt_mod","h2_liab_Celegans_Vm_P6p_wt_mod")
        h2_liab_Celegans_P6p_wt <- cbind(Models = rownames(h2_liab_Celegans_P6p_wt),h2_liab_Celegans_P6p_wt)
        rownames(h2_liab_Celegans_P6p_wt) <- NULL
        h2_liab_Celegans_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        h2_liab_Celegans_P6p_wt$Treatment <- c("Control","MA","Vm")
        h2_liab_Celegans_P6p_wt$Measure <- c("H2","H2","H2")
        h2_liab_Celegans_P6p_wt$Scale <- c("liab","liab","liab")
        h2_liab_Celegans_P6p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_Celegans_P6p_wt
      }
      
      #Summary Evol_liab_Celegans_P6p_wt
      {
        mean_Evol_liab_Celegans_P6p_wt <- rbind(mean(Evol_liab_Celegans_CONTROL_P6p_wt_mod),mean(Evol_liab_Celegans_MA_P6p_wt_mod),mean(Evol_liab_Celegans_Vm_P6p_wt_mod))
        colnames(mean_Evol_liab_Celegans_P6p_wt) <- c("mean")
        median_Evol_liab_Celegans_P6p_wt <- rbind(median(Evol_liab_Celegans_CONTROL_P6p_wt_mod),median(Evol_liab_Celegans_MA_P6p_wt_mod),median(Evol_liab_Celegans_Vm_P6p_wt_mod))
        colnames(median_Evol_liab_Celegans_P6p_wt) <- c("median")
        posterior.mode_Evol_liab_Celegans_P6p_wt <- rbind(posterior.mode(Evol_liab_Celegans_CONTROL_P6p_wt_mod),posterior.mode(Evol_liab_Celegans_MA_P6p_wt_mod),posterior.mode(Evol_liab_Celegans_Vm_P6p_wt_mod))
        colnames(posterior.mode_Evol_liab_Celegans_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Celegans_P6p_wt <- rbind(HPDinterval(Evol_liab_Celegans_CONTROL_P6p_wt_mod),HPDinterval(Evol_liab_Celegans_MA_P6p_wt_mod),HPDinterval(Evol_liab_Celegans_Vm_P6p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_Celegans_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Celegans_P6p_wt <- rbind(HPDinterval(Evol_liab_Celegans_CONTROL_P6p_wt_mod,prob=.83),HPDinterval(Evol_liab_Celegans_MA_P6p_wt_mod,prob=.83),HPDinterval(Evol_liab_Celegans_Vm_P6p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Celegans_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Celegans_P6p_wt <- rbind(effectiveSize(Evol_liab_Celegans_CONTROL_P6p_wt_mod),effectiveSize(Evol_liab_Celegans_MA_P6p_wt_mod),effectiveSize(Evol_liab_Celegans_Vm_P6p_wt_mod))
        colnames(effectiveSize_Evol_liab_Celegans_P6p_wt) <- c("effectiveSize")
        Evol_liab_Celegans_P6p_wt <- cbind.data.frame(mean_Evol_liab_Celegans_P6p_wt,median_Evol_liab_Celegans_P6p_wt,posterior.mode_Evol_liab_Celegans_P6p_wt,HPDinterval_0.95_Evol_liab_Celegans_P6p_wt,HPDinterval_0.83_Evol_liab_Celegans_P6p_wt,effectiveSize_Evol_liab_Celegans_P6p_wt)
        rownames(Evol_liab_Celegans_P6p_wt) <- c("Evol_liab_Celegans_CONTROL_P6p_wt_mod","Evol_liab_Celegans_MA_P6p_wt_mod","Evol_liab_Celegans_Vm_P6p_wt_mod")
        Evol_liab_Celegans_P6p_wt <- cbind(Models = rownames(Evol_liab_Celegans_P6p_wt),Evol_liab_Celegans_P6p_wt)
        rownames(Evol_liab_Celegans_P6p_wt) <- NULL
        Evol_liab_Celegans_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        Evol_liab_Celegans_P6p_wt$Treatment <- c("Control","MA","Vm")
        Evol_liab_Celegans_P6p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Celegans_P6p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_Celegans_P6p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_Celegans_P6p_wt
      }
      
      #Summary trait_mean_liab_Celegans_P6p_wt
      {
        mean_trait_mean_liab_Celegans_P6p_wt <- rbind(mean(trait_mean_liab_Celegans_CONTROL_P6p_wt_mod),mean(trait_mean_liab_Celegans_MA_P6p_wt_mod),mean(trait_mean_liab_Celegans_Vm_P6p_wt_mod))
        colnames(mean_trait_mean_liab_Celegans_P6p_wt) <- c("mean")
        median_trait_mean_liab_Celegans_P6p_wt <- rbind(median(trait_mean_liab_Celegans_CONTROL_P6p_wt_mod),median(trait_mean_liab_Celegans_MA_P6p_wt_mod),median(trait_mean_liab_Celegans_Vm_P6p_wt_mod))
        colnames(median_trait_mean_liab_Celegans_P6p_wt) <- c("median")
        posterior.mode_trait_mean_liab_Celegans_P6p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Celegans_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Celegans_MA_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Celegans_Vm_P6p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_Celegans_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Celegans_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Celegans_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Celegans_MA_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Celegans_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_Celegans_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Celegans_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Celegans_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Celegans_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Celegans_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Celegans_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Celegans_P6p_wt <- rbind(effectiveSize(trait_mean_liab_Celegans_CONTROL_P6p_wt_mod),effectiveSize(trait_mean_liab_Celegans_MA_P6p_wt_mod),effectiveSize(trait_mean_liab_Celegans_Vm_P6p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_Celegans_P6p_wt) <- c("effectiveSize")
        trait_mean_liab_Celegans_P6p_wt <- cbind.data.frame(mean_trait_mean_liab_Celegans_P6p_wt,median_trait_mean_liab_Celegans_P6p_wt,posterior.mode_trait_mean_liab_Celegans_P6p_wt,HPDinterval_0.95_trait_mean_liab_Celegans_P6p_wt,HPDinterval_0.83_trait_mean_liab_Celegans_P6p_wt,effectiveSize_trait_mean_liab_Celegans_P6p_wt)
        rownames(trait_mean_liab_Celegans_P6p_wt) <- c("trait_mean_liab_Celegans_CONTROL_P6p_wt_mod","trait_mean_liab_Celegans_MA_P6p_wt_mod","trait_mean_liab_Celegans_Vm_P6p_wt_mod")
        trait_mean_liab_Celegans_P6p_wt <- cbind(Models = rownames(trait_mean_liab_Celegans_P6p_wt),trait_mean_liab_Celegans_P6p_wt)
        rownames(trait_mean_liab_Celegans_P6p_wt) <- NULL
        trait_mean_liab_Celegans_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        trait_mean_liab_Celegans_P6p_wt$Treatment <- c("Control","MA","Vm")
        trait_mean_liab_Celegans_P6p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Celegans_P6p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_Celegans_P6p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_Celegans_P6p_wt
      }
      
      liab_Celegans_P6p_wt <- rbind.data.frame(va_liab_Celegans_P6p_wt, h2_liab_Celegans_P6p_wt,Evol_liab_Celegans_P6p_wt,trait_mean_liab_Celegans_P6p_wt)
      liab_Celegans_P6p_wt
    }
    #Summary data scale Celegans P6p
    {
      #Summary va_data_Celegans_P6p_wt:NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_Celegans_P6p_wt <- rbind(mean(va_data_Celegans_CONTROL_P6p_wt_mod/2),mean(va_data_Celegans_MA_P6p_wt_mod/2),mean(va_data_Celegans_Vm_P6p_wt_mod/2))
        colnames(mean_va_data_Celegans_P6p_wt) <- c("mean")
        median_va_data_Celegans_P6p_wt <- rbind(median(va_data_Celegans_CONTROL_P6p_wt_mod/2),median(va_data_Celegans_MA_P6p_wt_mod/2),median(va_data_Celegans_Vm_P6p_wt_mod/2))
        colnames(median_va_data_Celegans_P6p_wt) <- c("median")
        posterior.mode_va_data_Celegans_P6p_wt <- rbind(posterior.mode(as.mcmc(va_data_Celegans_CONTROL_P6p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Celegans_MA_P6p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Celegans_Vm_P6p_wt_mod/2)))
        colnames(posterior.mode_va_data_Celegans_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Celegans_P6p_wt <- rbind(HPDinterval(as.mcmc(va_data_Celegans_CONTROL_P6p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Celegans_MA_P6p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Celegans_Vm_P6p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_Celegans_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Celegans_P6p_wt <- rbind(HPDinterval(as.mcmc(va_data_Celegans_CONTROL_P6p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Celegans_MA_P6p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Celegans_Vm_P6p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Celegans_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Celegans_P6p_wt <- rbind(effectiveSize(va_data_Celegans_CONTROL_P6p_wt_mod/2),effectiveSize(va_data_Celegans_MA_P6p_wt_mod/2),effectiveSize(va_data_Celegans_Vm_P6p_wt_mod/2))
        colnames(effectiveSize_va_data_Celegans_P6p_wt) <- c("effectiveSize")
        va_data_Celegans_P6p_wt <- cbind.data.frame(mean_va_data_Celegans_P6p_wt,median_va_data_Celegans_P6p_wt,posterior.mode_va_data_Celegans_P6p_wt,HPDinterval_0.95_va_data_Celegans_P6p_wt,HPDinterval_0.83_va_data_Celegans_P6p_wt,effectiveSize_va_data_Celegans_P6p_wt)
        rownames(va_data_Celegans_P6p_wt) <- c("va_data_Celegans_CONTROL_P6p_wt_mod","va_data_Celegans_MA_P6p_wt_mod","va_data_Celegans_Vm_P6p_wt_mod")
        va_data_Celegans_P6p_wt <- cbind(Models = rownames(va_data_Celegans_P6p_wt),va_data_Celegans_P6p_wt)
        rownames(va_data_Celegans_P6p_wt) <- NULL
        va_data_Celegans_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        va_data_Celegans_P6p_wt$Treatment <- c("Control","MA","Vm")
        va_data_Celegans_P6p_wt$Measure <- c("Va","Va","Va")
        va_data_Celegans_P6p_wt$Scale <- c("data","data","data")
        va_data_Celegans_P6p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_Celegans_P6p_wt
      }
      
      #Summary h2_data_Celegans_P6p_wt
      {
        mean_h2_data_Celegans_P6p_wt <- rbind(mean(h2_data_Celegans_CONTROL_P6p_wt_mod),mean(h2_data_Celegans_MA_P6p_wt_mod),mean(h2_data_Celegans_Vm_P6p_wt_mod))
        colnames(mean_h2_data_Celegans_P6p_wt) <- c("mean")
        median_h2_data_Celegans_P6p_wt <- rbind(median(h2_data_Celegans_CONTROL_P6p_wt_mod),median(h2_data_Celegans_MA_P6p_wt_mod),median(h2_data_Celegans_Vm_P6p_wt_mod))
        colnames(median_h2_data_Celegans_P6p_wt) <- c("median")
        posterior.mode_h2_data_Celegans_P6p_wt <- rbind(posterior.mode(as.mcmc(h2_data_Celegans_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(h2_data_Celegans_MA_P6p_wt_mod)),posterior.mode(as.mcmc(h2_data_Celegans_Vm_P6p_wt_mod)))
        colnames(posterior.mode_h2_data_Celegans_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Celegans_P6p_wt <- rbind(HPDinterval(as.mcmc(h2_data_Celegans_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(h2_data_Celegans_MA_P6p_wt_mod)),HPDinterval(as.mcmc(h2_data_Celegans_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_Celegans_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Celegans_P6p_wt <- rbind(HPDinterval(as.mcmc(h2_data_Celegans_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Celegans_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Celegans_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Celegans_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Celegans_P6p_wt <- rbind(effectiveSize(h2_data_Celegans_CONTROL_P6p_wt_mod),effectiveSize(h2_data_Celegans_MA_P6p_wt_mod),effectiveSize(h2_data_Celegans_Vm_P6p_wt_mod))
        colnames(effectiveSize_h2_data_Celegans_P6p_wt) <- c("effectiveSize")
        h2_data_Celegans_P6p_wt <- cbind.data.frame(mean_h2_data_Celegans_P6p_wt,median_h2_data_Celegans_P6p_wt,posterior.mode_h2_data_Celegans_P6p_wt,HPDinterval_0.95_h2_data_Celegans_P6p_wt,HPDinterval_0.83_h2_data_Celegans_P6p_wt,effectiveSize_h2_data_Celegans_P6p_wt)
        rownames(h2_data_Celegans_P6p_wt) <- c("h2_data_Celegans_CONTROL_P6p_wt_mod","h2_data_Celegans_MA_P6p_wt_mod","h2_data_Celegans_Vm_P6p_wt_mod")
        h2_data_Celegans_P6p_wt <- cbind(Models = rownames(h2_data_Celegans_P6p_wt),h2_data_Celegans_P6p_wt)
        rownames(h2_data_Celegans_P6p_wt) <- NULL
        h2_data_Celegans_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        h2_data_Celegans_P6p_wt$Treatment <- c("Control","MA","Vm")
        h2_data_Celegans_P6p_wt$Measure <- c("H2","H2","H2")
        h2_data_Celegans_P6p_wt$Scale <- c("data","data","data")
        h2_data_Celegans_P6p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_Celegans_P6p_wt
      }
      
      #Summary Evol_data_Celegans_P6p_wt
      {
        mean_Evol_data_Celegans_P6p_wt <- rbind(mean(Evol_data_Celegans_CONTROL_P6p_wt_mod),mean(Evol_data_Celegans_MA_P6p_wt_mod),mean(Evol_data_Celegans_Vm_P6p_wt_mod))
        colnames(mean_Evol_data_Celegans_P6p_wt) <- c("mean")
        median_Evol_data_Celegans_P6p_wt <- rbind(median(Evol_data_Celegans_CONTROL_P6p_wt_mod),median(Evol_data_Celegans_MA_P6p_wt_mod),median(Evol_data_Celegans_Vm_P6p_wt_mod))
        colnames(median_Evol_data_Celegans_P6p_wt) <- c("median")
        posterior.mode_Evol_data_Celegans_P6p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_Celegans_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Celegans_MA_P6p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Celegans_Vm_P6p_wt_mod)))
        colnames(posterior.mode_Evol_data_Celegans_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Celegans_P6p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_Celegans_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Celegans_MA_P6p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Celegans_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_Celegans_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Celegans_P6p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_Celegans_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Celegans_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Celegans_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Celegans_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Celegans_P6p_wt <- rbind(effectiveSize(Evol_data_Celegans_CONTROL_P6p_wt_mod),effectiveSize(Evol_data_Celegans_MA_P6p_wt_mod),effectiveSize(Evol_data_Celegans_Vm_P6p_wt_mod))
        colnames(effectiveSize_Evol_data_Celegans_P6p_wt) <- c("effectiveSize")
        Evol_data_Celegans_P6p_wt <- cbind.data.frame(mean_Evol_data_Celegans_P6p_wt,median_Evol_data_Celegans_P6p_wt,posterior.mode_Evol_data_Celegans_P6p_wt,HPDinterval_0.95_Evol_data_Celegans_P6p_wt,HPDinterval_0.83_Evol_data_Celegans_P6p_wt,effectiveSize_Evol_data_Celegans_P6p_wt)
        rownames(Evol_data_Celegans_P6p_wt) <- c("Evol_data_Celegans_CONTROL_P6p_wt_mod","Evol_data_Celegans_MA_P6p_wt_mod","Evol_data_Celegans_Vm_P6p_wt_mod")
        Evol_data_Celegans_P6p_wt <- cbind(Models = rownames(Evol_data_Celegans_P6p_wt),Evol_data_Celegans_P6p_wt)
        rownames(Evol_data_Celegans_P6p_wt) <- NULL
        Evol_data_Celegans_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        Evol_data_Celegans_P6p_wt$Treatment <- c("Control","MA","Vm")
        Evol_data_Celegans_P6p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_Celegans_P6p_wt$Scale <- c("data","data","data")
        Evol_data_Celegans_P6p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_Celegans_P6p_wt
      }
      
      #Summary trait_mean_data_Celegans_P6p_wt
      {
        mean_trait_mean_data_Celegans_P6p_wt <- rbind(mean(trait_mean_data_Celegans_CONTROL_P6p_wt_mod),mean(trait_mean_data_Celegans_MA_P6p_wt_mod),mean(trait_mean_data_Celegans_Vm_P6p_wt_mod))
        colnames(mean_trait_mean_data_Celegans_P6p_wt) <- c("mean")
        median_trait_mean_data_Celegans_P6p_wt <- rbind(median(trait_mean_data_Celegans_CONTROL_P6p_wt_mod),median(trait_mean_data_Celegans_MA_P6p_wt_mod),median(trait_mean_data_Celegans_Vm_P6p_wt_mod))
        colnames(median_trait_mean_data_Celegans_P6p_wt) <- c("median")
        posterior.mode_trait_mean_data_Celegans_P6p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_Celegans_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Celegans_MA_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Celegans_Vm_P6p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_Celegans_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Celegans_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_Celegans_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Celegans_MA_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Celegans_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_Celegans_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Celegans_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_Celegans_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Celegans_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Celegans_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Celegans_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Celegans_P6p_wt <- rbind(effectiveSize(trait_mean_data_Celegans_CONTROL_P6p_wt_mod),effectiveSize(trait_mean_data_Celegans_MA_P6p_wt_mod),effectiveSize(trait_mean_data_Celegans_Vm_P6p_wt_mod))
        colnames(effectiveSize_trait_mean_data_Celegans_P6p_wt) <- c("effectiveSize")
        trait_mean_data_Celegans_P6p_wt <- cbind.data.frame(mean_trait_mean_data_Celegans_P6p_wt,median_trait_mean_data_Celegans_P6p_wt,posterior.mode_trait_mean_data_Celegans_P6p_wt,HPDinterval_0.95_trait_mean_data_Celegans_P6p_wt,HPDinterval_0.83_trait_mean_data_Celegans_P6p_wt,effectiveSize_trait_mean_data_Celegans_P6p_wt)
        rownames(trait_mean_data_Celegans_P6p_wt) <- c("trait_mean_data_Celegans_CONTROL_P6p_wt_mod","trait_mean_data_Celegans_MA_P6p_wt_mod","trait_mean_data_Celegans_Vm_P6p_wt_mod")
        trait_mean_data_Celegans_P6p_wt <- cbind(Models = rownames(trait_mean_data_Celegans_P6p_wt),trait_mean_data_Celegans_P6p_wt)
        rownames(trait_mean_data_Celegans_P6p_wt) <- NULL
        trait_mean_data_Celegans_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        trait_mean_data_Celegans_P6p_wt$Treatment <- c("Control","MA","Vm")
        trait_mean_data_Celegans_P6p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Celegans_P6p_wt$Scale <- c("data","data","data")
        trait_mean_data_Celegans_P6p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_Celegans_P6p_wt
      }
      
      data_Celegans_P6p_wt <- rbind.data.frame(va_data_Celegans_P6p_wt, h2_data_Celegans_P6p_wt,Evol_data_Celegans_P6p_wt,trait_mean_data_Celegans_P6p_wt)
      data_Celegans_P6p_wt
      
    }
    Vm_Celegans_P6p_wt <- rbind.data.frame(liab_Celegans_P6p_wt, data_Celegans_P6p_wt)
    Vm_Celegans_P6p_wt$Pnp_fate <- rep("wt", 24)
    Vm_Celegans_P6p_wt
    #remove Celegans P6p_wt models
    {
      remove(Celegans_CONTROL_P6p_wt_mod)
      remove(Celegans_MA_P6p_wt_mod)
      remove(Celegans_Vm_P6p_wt_mod)
    }
  }
  
  #Summary Celegans P7p
  {
    #Summary liability scale Celegans P7p
    {
      #Summary va_liab_Celegans_P7p_wt: NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_Celegans_P7p_wt <- rbind(mean(va_liab_Celegans_CONTROL_P7p_wt_mod/2),mean(va_liab_Celegans_MA_P7p_wt_mod/2),mean(va_liab_Celegans_Vm_P7p_wt_mod/2))
        colnames(mean_va_liab_Celegans_P7p_wt) <- c("mean")
        median_va_liab_Celegans_P7p_wt <- rbind(median(va_liab_Celegans_CONTROL_P7p_wt_mod/2),median(va_liab_Celegans_MA_P7p_wt_mod/2),median(va_liab_Celegans_Vm_P7p_wt_mod/2))
        colnames(median_va_liab_Celegans_P7p_wt) <- c("median")
        posterior.mode_va_liab_Celegans_P7p_wt <- rbind(posterior.mode(va_liab_Celegans_CONTROL_P7p_wt_mod/2),posterior.mode(va_liab_Celegans_MA_P7p_wt_mod/2),posterior.mode(va_liab_Celegans_Vm_P7p_wt_mod/2))
        colnames(posterior.mode_va_liab_Celegans_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Celegans_P7p_wt <- rbind(HPDinterval(va_liab_Celegans_CONTROL_P7p_wt_mod/2),HPDinterval(va_liab_Celegans_MA_P7p_wt_mod/2),HPDinterval(va_liab_Celegans_Vm_P7p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_Celegans_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Celegans_P7p_wt <- rbind(HPDinterval(va_liab_Celegans_CONTROL_P7p_wt_mod/2,prob=.83),HPDinterval(va_liab_Celegans_MA_P7p_wt_mod/2,prob=.83),HPDinterval(va_liab_Celegans_Vm_P7p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Celegans_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Celegans_P7p_wt <- rbind(effectiveSize(va_liab_Celegans_CONTROL_P7p_wt_mod/2),effectiveSize(va_liab_Celegans_MA_P7p_wt_mod/2),effectiveSize(va_liab_Celegans_Vm_P7p_wt_mod/2))
        colnames(effectiveSize_va_liab_Celegans_P7p_wt) <- c("effectiveSize")
        va_liab_Celegans_P7p_wt <- cbind.data.frame(mean_va_liab_Celegans_P7p_wt,median_va_liab_Celegans_P7p_wt,posterior.mode_va_liab_Celegans_P7p_wt,HPDinterval_0.95_va_liab_Celegans_P7p_wt,HPDinterval_0.83_va_liab_Celegans_P7p_wt,effectiveSize_va_liab_Celegans_P7p_wt)
        rownames(va_liab_Celegans_P7p_wt) <- c("va_liab_Celegans_CONTROL_P7p_wt_mod","va_liab_Celegans_MA_P7p_wt_mod","va_liab_Celegans_Vm_P7p_wt_mod")
        va_liab_Celegans_P7p_wt <- cbind(Models = rownames(va_liab_Celegans_P7p_wt),va_liab_Celegans_P7p_wt)
        rownames(va_liab_Celegans_P7p_wt) <- NULL
        va_liab_Celegans_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        va_liab_Celegans_P7p_wt$Treatment <- c("Control","MA","Vm")
        va_liab_Celegans_P7p_wt$Measure <- c("Va","Va","Va")
        va_liab_Celegans_P7p_wt$Scale <- c("liab","liab","liab")
        va_liab_Celegans_P7p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_Celegans_P7p_wt
      }
      
      #Summary h2_liab_Celegans_P7p_wt
      {
        mean_h2_liab_Celegans_P7p_wt <- rbind(mean(h2_liab_Celegans_CONTROL_P7p_wt_mod),mean(h2_liab_Celegans_MA_P7p_wt_mod),mean(h2_liab_Celegans_Vm_P7p_wt_mod))
        colnames(mean_h2_liab_Celegans_P7p_wt) <- c("mean")
        median_h2_liab_Celegans_P7p_wt <- rbind(median(h2_liab_Celegans_CONTROL_P7p_wt_mod),median(h2_liab_Celegans_MA_P7p_wt_mod),median(h2_liab_Celegans_Vm_P7p_wt_mod))
        colnames(median_h2_liab_Celegans_P7p_wt) <- c("median")
        posterior.mode_h2_liab_Celegans_P7p_wt <- rbind(posterior.mode(h2_liab_Celegans_CONTROL_P7p_wt_mod),posterior.mode(h2_liab_Celegans_MA_P7p_wt_mod),posterior.mode(h2_liab_Celegans_Vm_P7p_wt_mod))
        colnames(posterior.mode_h2_liab_Celegans_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Celegans_P7p_wt <- rbind(HPDinterval(h2_liab_Celegans_CONTROL_P7p_wt_mod),HPDinterval(h2_liab_Celegans_MA_P7p_wt_mod),HPDinterval(h2_liab_Celegans_Vm_P7p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_Celegans_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Celegans_P7p_wt <- rbind(HPDinterval(h2_liab_Celegans_CONTROL_P7p_wt_mod,prob=.83),HPDinterval(h2_liab_Celegans_MA_P7p_wt_mod,prob=.83),HPDinterval(h2_liab_Celegans_Vm_P7p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Celegans_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Celegans_P7p_wt <- rbind(effectiveSize(h2_liab_Celegans_CONTROL_P7p_wt_mod),effectiveSize(h2_liab_Celegans_MA_P7p_wt_mod),effectiveSize(h2_liab_Celegans_Vm_P7p_wt_mod))
        colnames(effectiveSize_h2_liab_Celegans_P7p_wt) <- c("effectiveSize")
        h2_liab_Celegans_P7p_wt <- cbind.data.frame(mean_h2_liab_Celegans_P7p_wt,median_h2_liab_Celegans_P7p_wt,posterior.mode_h2_liab_Celegans_P7p_wt,HPDinterval_0.95_h2_liab_Celegans_P7p_wt,HPDinterval_0.83_h2_liab_Celegans_P7p_wt,effectiveSize_h2_liab_Celegans_P7p_wt)
        rownames(h2_liab_Celegans_P7p_wt) <- c("h2_liab_Celegans_CONTROL_P7p_wt_mod","h2_liab_Celegans_MA_P7p_wt_mod","h2_liab_Celegans_Vm_P7p_wt_mod")
        h2_liab_Celegans_P7p_wt <- cbind(Models = rownames(h2_liab_Celegans_P7p_wt),h2_liab_Celegans_P7p_wt)
        rownames(h2_liab_Celegans_P7p_wt) <- NULL
        h2_liab_Celegans_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        h2_liab_Celegans_P7p_wt$Treatment <- c("Control","MA","Vm")
        h2_liab_Celegans_P7p_wt$Measure <- c("H2","H2","H2")
        h2_liab_Celegans_P7p_wt$Scale <- c("liab","liab","liab")
        h2_liab_Celegans_P7p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_Celegans_P7p_wt
      }
      
      #Summary Evol_liab_Celegans_P7p_wt
      {
        mean_Evol_liab_Celegans_P7p_wt <- rbind(mean(Evol_liab_Celegans_CONTROL_P7p_wt_mod),mean(Evol_liab_Celegans_MA_P7p_wt_mod),mean(Evol_liab_Celegans_Vm_P7p_wt_mod))
        colnames(mean_Evol_liab_Celegans_P7p_wt) <- c("mean")
        median_Evol_liab_Celegans_P7p_wt <- rbind(median(Evol_liab_Celegans_CONTROL_P7p_wt_mod),median(Evol_liab_Celegans_MA_P7p_wt_mod),median(Evol_liab_Celegans_Vm_P7p_wt_mod))
        colnames(median_Evol_liab_Celegans_P7p_wt) <- c("median")
        posterior.mode_Evol_liab_Celegans_P7p_wt <- rbind(posterior.mode(Evol_liab_Celegans_CONTROL_P7p_wt_mod),posterior.mode(Evol_liab_Celegans_MA_P7p_wt_mod),posterior.mode(Evol_liab_Celegans_Vm_P7p_wt_mod))
        colnames(posterior.mode_Evol_liab_Celegans_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Celegans_P7p_wt <- rbind(HPDinterval(Evol_liab_Celegans_CONTROL_P7p_wt_mod),HPDinterval(Evol_liab_Celegans_MA_P7p_wt_mod),HPDinterval(Evol_liab_Celegans_Vm_P7p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_Celegans_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Celegans_P7p_wt <- rbind(HPDinterval(Evol_liab_Celegans_CONTROL_P7p_wt_mod,prob=.83),HPDinterval(Evol_liab_Celegans_MA_P7p_wt_mod,prob=.83),HPDinterval(Evol_liab_Celegans_Vm_P7p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Celegans_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Celegans_P7p_wt <- rbind(effectiveSize(Evol_liab_Celegans_CONTROL_P7p_wt_mod),effectiveSize(Evol_liab_Celegans_MA_P7p_wt_mod),effectiveSize(Evol_liab_Celegans_Vm_P7p_wt_mod))
        colnames(effectiveSize_Evol_liab_Celegans_P7p_wt) <- c("effectiveSize")
        Evol_liab_Celegans_P7p_wt <- cbind.data.frame(mean_Evol_liab_Celegans_P7p_wt,median_Evol_liab_Celegans_P7p_wt,posterior.mode_Evol_liab_Celegans_P7p_wt,HPDinterval_0.95_Evol_liab_Celegans_P7p_wt,HPDinterval_0.83_Evol_liab_Celegans_P7p_wt,effectiveSize_Evol_liab_Celegans_P7p_wt)
        rownames(Evol_liab_Celegans_P7p_wt) <- c("Evol_liab_Celegans_CONTROL_P7p_wt_mod","Evol_liab_Celegans_MA_P7p_wt_mod","Evol_liab_Celegans_Vm_P7p_wt_mod")
        Evol_liab_Celegans_P7p_wt <- cbind(Models = rownames(Evol_liab_Celegans_P7p_wt),Evol_liab_Celegans_P7p_wt)
        rownames(Evol_liab_Celegans_P7p_wt) <- NULL
        Evol_liab_Celegans_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        Evol_liab_Celegans_P7p_wt$Treatment <- c("Control","MA","Vm")
        Evol_liab_Celegans_P7p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Celegans_P7p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_Celegans_P7p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_Celegans_P7p_wt
      }
      
      #Summary trait_mean_liab_Celegans_P7p_wt
      {
        mean_trait_mean_liab_Celegans_P7p_wt <- rbind(mean(trait_mean_liab_Celegans_CONTROL_P7p_wt_mod),mean(trait_mean_liab_Celegans_MA_P7p_wt_mod),mean(trait_mean_liab_Celegans_Vm_P7p_wt_mod))
        colnames(mean_trait_mean_liab_Celegans_P7p_wt) <- c("mean")
        median_trait_mean_liab_Celegans_P7p_wt <- rbind(median(trait_mean_liab_Celegans_CONTROL_P7p_wt_mod),median(trait_mean_liab_Celegans_MA_P7p_wt_mod),median(trait_mean_liab_Celegans_Vm_P7p_wt_mod))
        colnames(median_trait_mean_liab_Celegans_P7p_wt) <- c("median")
        posterior.mode_trait_mean_liab_Celegans_P7p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Celegans_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Celegans_MA_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Celegans_Vm_P7p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_Celegans_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Celegans_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Celegans_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Celegans_MA_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Celegans_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_Celegans_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Celegans_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Celegans_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Celegans_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Celegans_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Celegans_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Celegans_P7p_wt <- rbind(effectiveSize(trait_mean_liab_Celegans_CONTROL_P7p_wt_mod),effectiveSize(trait_mean_liab_Celegans_MA_P7p_wt_mod),effectiveSize(trait_mean_liab_Celegans_Vm_P7p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_Celegans_P7p_wt) <- c("effectiveSize")
        trait_mean_liab_Celegans_P7p_wt <- cbind.data.frame(mean_trait_mean_liab_Celegans_P7p_wt,median_trait_mean_liab_Celegans_P7p_wt,posterior.mode_trait_mean_liab_Celegans_P7p_wt,HPDinterval_0.95_trait_mean_liab_Celegans_P7p_wt,HPDinterval_0.83_trait_mean_liab_Celegans_P7p_wt,effectiveSize_trait_mean_liab_Celegans_P7p_wt)
        rownames(trait_mean_liab_Celegans_P7p_wt) <- c("trait_mean_liab_Celegans_CONTROL_P7p_wt_mod","trait_mean_liab_Celegans_MA_P7p_wt_mod","trait_mean_liab_Celegans_Vm_P7p_wt_mod")
        trait_mean_liab_Celegans_P7p_wt <- cbind(Models = rownames(trait_mean_liab_Celegans_P7p_wt),trait_mean_liab_Celegans_P7p_wt)
        rownames(trait_mean_liab_Celegans_P7p_wt) <- NULL
        trait_mean_liab_Celegans_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        trait_mean_liab_Celegans_P7p_wt$Treatment <- c("Control","MA","Vm")
        trait_mean_liab_Celegans_P7p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Celegans_P7p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_Celegans_P7p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_Celegans_P7p_wt
      }
      
      liab_Celegans_P7p_wt <- rbind.data.frame(va_liab_Celegans_P7p_wt, h2_liab_Celegans_P7p_wt,Evol_liab_Celegans_P7p_wt,trait_mean_liab_Celegans_P7p_wt)
      liab_Celegans_P7p_wt
    }
    #Summary data scale Celegans P7p
    {
      #Summary va_data_Celegans_P7p_wt:NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_Celegans_P7p_wt <- rbind(mean(va_data_Celegans_CONTROL_P7p_wt_mod/2),mean(va_data_Celegans_MA_P7p_wt_mod/2),mean(va_data_Celegans_Vm_P7p_wt_mod/2))
        colnames(mean_va_data_Celegans_P7p_wt) <- c("mean")
        median_va_data_Celegans_P7p_wt <- rbind(median(va_data_Celegans_CONTROL_P7p_wt_mod/2),median(va_data_Celegans_MA_P7p_wt_mod/2),median(va_data_Celegans_Vm_P7p_wt_mod/2))
        colnames(median_va_data_Celegans_P7p_wt) <- c("median")
        posterior.mode_va_data_Celegans_P7p_wt <- rbind(posterior.mode(as.mcmc(va_data_Celegans_CONTROL_P7p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Celegans_MA_P7p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Celegans_Vm_P7p_wt_mod/2)))
        colnames(posterior.mode_va_data_Celegans_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Celegans_P7p_wt <- rbind(HPDinterval(as.mcmc(va_data_Celegans_CONTROL_P7p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Celegans_MA_P7p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Celegans_Vm_P7p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_Celegans_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Celegans_P7p_wt <- rbind(HPDinterval(as.mcmc(va_data_Celegans_CONTROL_P7p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Celegans_MA_P7p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Celegans_Vm_P7p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Celegans_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Celegans_P7p_wt <- rbind(effectiveSize(va_data_Celegans_CONTROL_P7p_wt_mod/2),effectiveSize(va_data_Celegans_MA_P7p_wt_mod/2),effectiveSize(va_data_Celegans_Vm_P7p_wt_mod/2))
        colnames(effectiveSize_va_data_Celegans_P7p_wt) <- c("effectiveSize")
        va_data_Celegans_P7p_wt <- cbind.data.frame(mean_va_data_Celegans_P7p_wt,median_va_data_Celegans_P7p_wt,posterior.mode_va_data_Celegans_P7p_wt,HPDinterval_0.95_va_data_Celegans_P7p_wt,HPDinterval_0.83_va_data_Celegans_P7p_wt,effectiveSize_va_data_Celegans_P7p_wt)
        rownames(va_data_Celegans_P7p_wt) <- c("va_data_Celegans_CONTROL_P7p_wt_mod","va_data_Celegans_MA_P7p_wt_mod","va_data_Celegans_Vm_P7p_wt_mod")
        va_data_Celegans_P7p_wt <- cbind(Models = rownames(va_data_Celegans_P7p_wt),va_data_Celegans_P7p_wt)
        rownames(va_data_Celegans_P7p_wt) <- NULL
        va_data_Celegans_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        va_data_Celegans_P7p_wt$Treatment <- c("Control","MA","Vm")
        va_data_Celegans_P7p_wt$Measure <- c("Va","Va","Va")
        va_data_Celegans_P7p_wt$Scale <- c("data","data","data")
        va_data_Celegans_P7p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_Celegans_P7p_wt
      }
      
      #Summary h2_data_Celegans_P7p_wt
      {
        mean_h2_data_Celegans_P7p_wt <- rbind(mean(h2_data_Celegans_CONTROL_P7p_wt_mod),mean(h2_data_Celegans_MA_P7p_wt_mod),mean(h2_data_Celegans_Vm_P7p_wt_mod))
        colnames(mean_h2_data_Celegans_P7p_wt) <- c("mean")
        median_h2_data_Celegans_P7p_wt <- rbind(median(h2_data_Celegans_CONTROL_P7p_wt_mod),median(h2_data_Celegans_MA_P7p_wt_mod),median(h2_data_Celegans_Vm_P7p_wt_mod))
        colnames(median_h2_data_Celegans_P7p_wt) <- c("median")
        posterior.mode_h2_data_Celegans_P7p_wt <- rbind(posterior.mode(as.mcmc(h2_data_Celegans_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(h2_data_Celegans_MA_P7p_wt_mod)),posterior.mode(as.mcmc(h2_data_Celegans_Vm_P7p_wt_mod)))
        colnames(posterior.mode_h2_data_Celegans_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Celegans_P7p_wt <- rbind(HPDinterval(as.mcmc(h2_data_Celegans_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(h2_data_Celegans_MA_P7p_wt_mod)),HPDinterval(as.mcmc(h2_data_Celegans_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_Celegans_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Celegans_P7p_wt <- rbind(HPDinterval(as.mcmc(h2_data_Celegans_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Celegans_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Celegans_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Celegans_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Celegans_P7p_wt <- rbind(effectiveSize(h2_data_Celegans_CONTROL_P7p_wt_mod),effectiveSize(h2_data_Celegans_MA_P7p_wt_mod),effectiveSize(h2_data_Celegans_Vm_P7p_wt_mod))
        colnames(effectiveSize_h2_data_Celegans_P7p_wt) <- c("effectiveSize")
        h2_data_Celegans_P7p_wt <- cbind.data.frame(mean_h2_data_Celegans_P7p_wt,median_h2_data_Celegans_P7p_wt,posterior.mode_h2_data_Celegans_P7p_wt,HPDinterval_0.95_h2_data_Celegans_P7p_wt,HPDinterval_0.83_h2_data_Celegans_P7p_wt,effectiveSize_h2_data_Celegans_P7p_wt)
        rownames(h2_data_Celegans_P7p_wt) <- c("h2_data_Celegans_CONTROL_P7p_wt_mod","h2_data_Celegans_MA_P7p_wt_mod","h2_data_Celegans_Vm_P7p_wt_mod")
        h2_data_Celegans_P7p_wt <- cbind(Models = rownames(h2_data_Celegans_P7p_wt),h2_data_Celegans_P7p_wt)
        rownames(h2_data_Celegans_P7p_wt) <- NULL
        h2_data_Celegans_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        h2_data_Celegans_P7p_wt$Treatment <- c("Control","MA","Vm")
        h2_data_Celegans_P7p_wt$Measure <- c("H2","H2","H2")
        h2_data_Celegans_P7p_wt$Scale <- c("data","data","data")
        h2_data_Celegans_P7p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_Celegans_P7p_wt
      }
      
      #Summary Evol_data_Celegans_P7p_wt
      {
        mean_Evol_data_Celegans_P7p_wt <- rbind(mean(Evol_data_Celegans_CONTROL_P7p_wt_mod),mean(Evol_data_Celegans_MA_P7p_wt_mod),mean(Evol_data_Celegans_Vm_P7p_wt_mod))
        colnames(mean_Evol_data_Celegans_P7p_wt) <- c("mean")
        median_Evol_data_Celegans_P7p_wt <- rbind(median(Evol_data_Celegans_CONTROL_P7p_wt_mod),median(Evol_data_Celegans_MA_P7p_wt_mod),median(Evol_data_Celegans_Vm_P7p_wt_mod))
        colnames(median_Evol_data_Celegans_P7p_wt) <- c("median")
        posterior.mode_Evol_data_Celegans_P7p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_Celegans_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Celegans_MA_P7p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Celegans_Vm_P7p_wt_mod)))
        colnames(posterior.mode_Evol_data_Celegans_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Celegans_P7p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_Celegans_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Celegans_MA_P7p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Celegans_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_Celegans_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Celegans_P7p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_Celegans_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Celegans_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Celegans_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Celegans_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Celegans_P7p_wt <- rbind(effectiveSize(Evol_data_Celegans_CONTROL_P7p_wt_mod),effectiveSize(Evol_data_Celegans_MA_P7p_wt_mod),effectiveSize(Evol_data_Celegans_Vm_P7p_wt_mod))
        colnames(effectiveSize_Evol_data_Celegans_P7p_wt) <- c("effectiveSize")
        Evol_data_Celegans_P7p_wt <- cbind.data.frame(mean_Evol_data_Celegans_P7p_wt,median_Evol_data_Celegans_P7p_wt,posterior.mode_Evol_data_Celegans_P7p_wt,HPDinterval_0.95_Evol_data_Celegans_P7p_wt,HPDinterval_0.83_Evol_data_Celegans_P7p_wt,effectiveSize_Evol_data_Celegans_P7p_wt)
        rownames(Evol_data_Celegans_P7p_wt) <- c("Evol_data_Celegans_CONTROL_P7p_wt_mod","Evol_data_Celegans_MA_P7p_wt_mod","Evol_data_Celegans_Vm_P7p_wt_mod")
        Evol_data_Celegans_P7p_wt <- cbind(Models = rownames(Evol_data_Celegans_P7p_wt),Evol_data_Celegans_P7p_wt)
        rownames(Evol_data_Celegans_P7p_wt) <- NULL
        Evol_data_Celegans_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        Evol_data_Celegans_P7p_wt$Treatment <- c("Control","MA","Vm")
        Evol_data_Celegans_P7p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_Celegans_P7p_wt$Scale <- c("data","data","data")
        Evol_data_Celegans_P7p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_Celegans_P7p_wt
      }
      
      #Summary trait_mean_data_Celegans_P7p_wt
      {
        mean_trait_mean_data_Celegans_P7p_wt <- rbind(mean(trait_mean_data_Celegans_CONTROL_P7p_wt_mod),mean(trait_mean_data_Celegans_MA_P7p_wt_mod),mean(trait_mean_data_Celegans_Vm_P7p_wt_mod))
        colnames(mean_trait_mean_data_Celegans_P7p_wt) <- c("mean")
        median_trait_mean_data_Celegans_P7p_wt <- rbind(median(trait_mean_data_Celegans_CONTROL_P7p_wt_mod),median(trait_mean_data_Celegans_MA_P7p_wt_mod),median(trait_mean_data_Celegans_Vm_P7p_wt_mod))
        colnames(median_trait_mean_data_Celegans_P7p_wt) <- c("median")
        posterior.mode_trait_mean_data_Celegans_P7p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_Celegans_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Celegans_MA_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Celegans_Vm_P7p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_Celegans_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Celegans_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_Celegans_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Celegans_MA_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Celegans_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_Celegans_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Celegans_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_Celegans_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Celegans_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Celegans_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Celegans_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Celegans_P7p_wt <- rbind(effectiveSize(trait_mean_data_Celegans_CONTROL_P7p_wt_mod),effectiveSize(trait_mean_data_Celegans_MA_P7p_wt_mod),effectiveSize(trait_mean_data_Celegans_Vm_P7p_wt_mod))
        colnames(effectiveSize_trait_mean_data_Celegans_P7p_wt) <- c("effectiveSize")
        trait_mean_data_Celegans_P7p_wt <- cbind.data.frame(mean_trait_mean_data_Celegans_P7p_wt,median_trait_mean_data_Celegans_P7p_wt,posterior.mode_trait_mean_data_Celegans_P7p_wt,HPDinterval_0.95_trait_mean_data_Celegans_P7p_wt,HPDinterval_0.83_trait_mean_data_Celegans_P7p_wt,effectiveSize_trait_mean_data_Celegans_P7p_wt)
        rownames(trait_mean_data_Celegans_P7p_wt) <- c("trait_mean_data_Celegans_CONTROL_P7p_wt_mod","trait_mean_data_Celegans_MA_P7p_wt_mod","trait_mean_data_Celegans_Vm_P7p_wt_mod")
        trait_mean_data_Celegans_P7p_wt <- cbind(Models = rownames(trait_mean_data_Celegans_P7p_wt),trait_mean_data_Celegans_P7p_wt)
        rownames(trait_mean_data_Celegans_P7p_wt) <- NULL
        trait_mean_data_Celegans_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        trait_mean_data_Celegans_P7p_wt$Treatment <- c("Control","MA","Vm")
        trait_mean_data_Celegans_P7p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Celegans_P7p_wt$Scale <- c("data","data","data")
        trait_mean_data_Celegans_P7p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_Celegans_P7p_wt
      }
      
      data_Celegans_P7p_wt <- rbind.data.frame(va_data_Celegans_P7p_wt, h2_data_Celegans_P7p_wt,Evol_data_Celegans_P7p_wt,trait_mean_data_Celegans_P7p_wt)
      data_Celegans_P7p_wt
      
    }
    Vm_Celegans_P7p_wt <- rbind.data.frame(liab_Celegans_P7p_wt, data_Celegans_P7p_wt)
    Vm_Celegans_P7p_wt$Pnp_fate <- rep("wt", 24)
    Vm_Celegans_P7p_wt
    #remove Celegans P7p_wt models
    {
      remove(Celegans_CONTROL_P7p_wt_mod)
      remove(Celegans_MA_P7p_wt_mod)
      remove(Celegans_Vm_P7p_wt_mod)
    }
  }
  
  Vm_Celegans_summary_SS_vulva <- rbind.data.frame(Vm_Celegans_P3p_SS,Vm_Celegans_P4p_SS,Vm_Celegans_P5p_wt,Vm_Celegans_P6p_wt,Vm_Celegans_P7p_wt,Vm_Celegans_P8p_SS)
  Vm_Celegans_summary_SS_vulva$Species <- rep("C.elegans",144)
  Vm_Celegans_summary_SS_vulva$Genus <- rep("Caenorhabditis",144)
  View(Vm_Celegans_summary_SS_vulva)
  
  
  #Vm_Celegans_P3p_divided_P4p_SS
  {
    #Vm_Celegans_P3p_divided_P4p_SS_liab
    {
      
      va_liab_Celegans_Vm_P3p_divided_P4p_SS_mod <- va_liab_Celegans_Vm_P3p_SS_mod / va_liab_Celegans_Vm_P4p_SS_mod
      h2_liab_Celegans_Vm_P3p_divided_P4p_SS_mod <- h2_liab_Celegans_Vm_P3p_SS_mod / h2_liab_Celegans_Vm_P4p_SS_mod
      Evol_liab_Celegans_Vm_P3p_divided_P4p_SS_mod <- Evol_liab_Celegans_Vm_P3p_SS_mod / Evol_liab_Celegans_Vm_P4p_SS_mod
      
      mean_va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS <- rbind(mean(log10(va_liab_Celegans_Vm_P3p_divided_P4p_SS_mod)),mean(log10(h2_liab_Celegans_Vm_P3p_divided_P4p_SS_mod)), mean(log10(Evol_liab_Celegans_Vm_P3p_divided_P4p_SS_mod)))
      colnames(mean_va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS) <- c("mean")
      median_va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS <- rbind(median(log10(va_liab_Celegans_Vm_P3p_divided_P4p_SS_mod)),median(log10(h2_liab_Celegans_Vm_P3p_divided_P4p_SS_mod)), median(log10(Evol_liab_Celegans_Vm_P3p_divided_P4p_SS_mod)))
      colnames(median_va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS) <- c("median")
      posterior.mode_va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS <- rbind(posterior.mode(as.mcmc(log10(va_liab_Celegans_Vm_P3p_divided_P4p_SS_mod))),posterior.mode(as.mcmc(log10(h2_liab_Celegans_Vm_P3p_divided_P4p_SS_mod))),posterior.mode(as.mcmc(log10(Evol_liab_Celegans_Vm_P3p_divided_P4p_SS_mod))))
      colnames(posterior.mode_va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS) <- c("posterior.mode")
      HPDinterval_0.95_va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS <- rbind(HPDinterval(as.mcmc(log10(va_liab_Celegans_Vm_P3p_divided_P4p_SS_mod))),HPDinterval(as.mcmc(log10(h2_liab_Celegans_Vm_P3p_divided_P4p_SS_mod))),HPDinterval(as.mcmc(log10(Evol_liab_Celegans_Vm_P3p_divided_P4p_SS_mod))))
      colnames(HPDinterval_0.95_va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS) <- c("CI_lower_0.95","CI_upper_0.95")
      HPDinterval_0.83_va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS <- rbind(HPDinterval(as.mcmc(log10(va_liab_Celegans_Vm_P3p_divided_P4p_SS_mod)),prob=.83),HPDinterval(as.mcmc(log10(h2_liab_Celegans_Vm_P3p_divided_P4p_SS_mod)),prob=.83),HPDinterval(as.mcmc(log10(Evol_liab_Celegans_Vm_P3p_divided_P4p_SS_mod)),prob=.83))
      colnames(HPDinterval_0.83_va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS) <- c("CI_lower_0.83","CI_upper_0.83")
      va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS <- cbind.data.frame(mean_va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS,median_va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS,posterior.mode_va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS,HPDinterval_0.95_va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS,HPDinterval_0.83_va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS)
      rownames(va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS) <- c("va_liab_Celegans_Vm_P3p_divided_P4p_SS_log10","h2_liab_Celegans_Vm_P3p_divided_P4p_SS_log10","Evol_liab_Celegans_Vm_P3p_divided_P4p_SS_log10")
      va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS$Measure <- c("Va","H2", "Evol")
      va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS$Species <- rep("C.elegans",3)
      va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS$Genus <- rep("Caenorhabditis",3)
      va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS$Scale <- rep("liab",3)
      va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS
      
      pdf("Vm_va_h2_Evol_liab_P3p_divided_P4p_SS_log10_Celegans.pdf")
      ggplot(va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS, aes(x=Measure, y= median)) +
        geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
        geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
        geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
        theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
        labs(y = "Va log10 ( P3.p / P4.p)", title = "Celegans_log10(P3p/P4p)_liab")
      dev.off() 
      
    }
    
    #Vm_Celegans_P3p_divided_P4p_SS_data
    
    {
      va_data_Celegans_Vm_P3p_divided_P4p_SS_mod <- va_data_Celegans_Vm_P3p_SS_mod / va_data_Celegans_Vm_P4p_SS_mod
      h2_data_Celegans_Vm_P3p_divided_P4p_SS_mod <- h2_data_Celegans_Vm_P3p_SS_mod / h2_data_Celegans_Vm_P4p_SS_mod
      Evol_data_Celegans_Vm_P3p_divided_P4p_SS_mod <- Evol_data_Celegans_Vm_P3p_SS_mod / Evol_data_Celegans_Vm_P4p_SS_mod
      
      mean_va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS <- rbind(mean(log10(va_data_Celegans_Vm_P3p_divided_P4p_SS_mod)),mean(log10(h2_data_Celegans_Vm_P3p_divided_P4p_SS_mod)), mean(log10(Evol_data_Celegans_Vm_P3p_divided_P4p_SS_mod)))
      colnames(mean_va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS) <- c("mean")
      median_va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS <- rbind(median(log10(va_data_Celegans_Vm_P3p_divided_P4p_SS_mod)),median(log10(h2_data_Celegans_Vm_P3p_divided_P4p_SS_mod)), median(log10(Evol_data_Celegans_Vm_P3p_divided_P4p_SS_mod)))
      colnames(median_va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS) <- c("median")
      posterior.mode_va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS <- rbind(posterior.mode(as.mcmc(log10(va_data_Celegans_Vm_P3p_divided_P4p_SS_mod))),posterior.mode(as.mcmc(log10(h2_data_Celegans_Vm_P3p_divided_P4p_SS_mod))),posterior.mode(as.mcmc(log10(Evol_data_Celegans_Vm_P3p_divided_P4p_SS_mod))))
      colnames(posterior.mode_va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS) <- c("posterior.mode")
      HPDinterval_0.95_va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS <- rbind(HPDinterval(as.mcmc(log10(va_data_Celegans_Vm_P3p_divided_P4p_SS_mod))),HPDinterval(as.mcmc(log10(h2_data_Celegans_Vm_P3p_divided_P4p_SS_mod))),HPDinterval(as.mcmc(log10(Evol_data_Celegans_Vm_P3p_divided_P4p_SS_mod))))
      colnames(HPDinterval_0.95_va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS) <- c("CI_lower_0.95","CI_upper_0.95")
      HPDinterval_0.83_va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS <- rbind(HPDinterval(as.mcmc(log10(va_data_Celegans_Vm_P3p_divided_P4p_SS_mod)),prob=.83),HPDinterval(as.mcmc(log10(h2_data_Celegans_Vm_P3p_divided_P4p_SS_mod)),prob=.83),HPDinterval(as.mcmc(log10(Evol_data_Celegans_Vm_P3p_divided_P4p_SS_mod)),prob=.83))
      colnames(HPDinterval_0.83_va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS) <- c("CI_lower_0.83","CI_upper_0.83")
      va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS <- cbind.data.frame(mean_va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS,median_va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS,posterior.mode_va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS,HPDinterval_0.95_va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS,HPDinterval_0.83_va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS)
      rownames(va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS) <- c("va_data_Celegans_Vm_P3p_divided_P4p_SS_log10","h2_data_Celegans_Vm_P3p_divided_P4p_SS_log10","Evol_data_Celegans_Vm_P3p_divided_P4p_SS_log10")
      va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS$Measure <- c("Va","H2", "Evol")
      va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS$Species <- rep("C.elegans",3)
      va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS$Genus <- rep("Caenorhabditis",3)
      va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS$Scale <- rep("data",3)
      va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS
      
      pdf("Vm_va_h2_Evol_data_P3p_divided_P4p_SS_log10_Celegans.pdf")
      ggplot(va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS, aes(x=Measure, y= median)) +
        geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
        geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
        geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
        theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
        labs(y = "Va log10 ( P3.p / P4.p)", title = "Celegans_log10(P3p/P4p)_data")
      dev.off() 
      
    }
    
    va_h2_Evol_Vm_Celegans_P3p_divided_P4p_SS_summary <- rbind.data.frame(va_h2_Evol_liab_Vm_Celegans_P3p_divided_P4p_SS,va_h2_Evol_data_Vm_Celegans_P3p_divided_P4p_SS)
    va_h2_Evol_Vm_Celegans_P3p_divided_P4p_SS_summary
    
  }
  
  
}


#---- Cbriggsae ----
{
  #Summary Cbriggsae P3p
  {
    #Summary liability scale Cbriggsae P3p
    {
      #Summary va_liab_Cbriggsae_P3p_SS: NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_Cbriggsae_P3p_SS <- rbind(mean(va_liab_Cbriggsae_CONTROL_P3p_SS_mod/2),mean(va_liab_Cbriggsae_MA_P3p_SS_mod/2),mean(va_liab_Cbriggsae_Vm_P3p_SS_mod/2))
        colnames(mean_va_liab_Cbriggsae_P3p_SS) <- c("mean")
        median_va_liab_Cbriggsae_P3p_SS <- rbind(median(va_liab_Cbriggsae_CONTROL_P3p_SS_mod/2),median(va_liab_Cbriggsae_MA_P3p_SS_mod/2),median(va_liab_Cbriggsae_Vm_P3p_SS_mod/2))
        colnames(median_va_liab_Cbriggsae_P3p_SS) <- c("median")
        posterior.mode_va_liab_Cbriggsae_P3p_SS <- rbind(posterior.mode(va_liab_Cbriggsae_CONTROL_P3p_SS_mod/2),posterior.mode(va_liab_Cbriggsae_MA_P3p_SS_mod/2),posterior.mode(va_liab_Cbriggsae_Vm_P3p_SS_mod/2))
        colnames(posterior.mode_va_liab_Cbriggsae_P3p_SS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Cbriggsae_P3p_SS <- rbind(HPDinterval(va_liab_Cbriggsae_CONTROL_P3p_SS_mod/2),HPDinterval(va_liab_Cbriggsae_MA_P3p_SS_mod/2),HPDinterval(va_liab_Cbriggsae_Vm_P3p_SS_mod/2))
        colnames(HPDinterval_0.95_va_liab_Cbriggsae_P3p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Cbriggsae_P3p_SS <- rbind(HPDinterval(va_liab_Cbriggsae_CONTROL_P3p_SS_mod/2,prob=.83),HPDinterval(va_liab_Cbriggsae_MA_P3p_SS_mod/2,prob=.83),HPDinterval(va_liab_Cbriggsae_Vm_P3p_SS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Cbriggsae_P3p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Cbriggsae_P3p_SS <- rbind(effectiveSize(va_liab_Cbriggsae_CONTROL_P3p_SS_mod/2),effectiveSize(va_liab_Cbriggsae_MA_P3p_SS_mod/2),effectiveSize(va_liab_Cbriggsae_Vm_P3p_SS_mod/2))
        colnames(effectiveSize_va_liab_Cbriggsae_P3p_SS) <- c("effectiveSize")
        va_liab_Cbriggsae_P3p_SS <- cbind.data.frame(mean_va_liab_Cbriggsae_P3p_SS,median_va_liab_Cbriggsae_P3p_SS,posterior.mode_va_liab_Cbriggsae_P3p_SS,HPDinterval_0.95_va_liab_Cbriggsae_P3p_SS,HPDinterval_0.83_va_liab_Cbriggsae_P3p_SS,effectiveSize_va_liab_Cbriggsae_P3p_SS)
        rownames(va_liab_Cbriggsae_P3p_SS) <- c("va_liab_Cbriggsae_CONTROL_P3p_SS_mod","va_liab_Cbriggsae_MA_P3p_SS_mod","va_liab_Cbriggsae_Vm_P3p_SS_mod")
        va_liab_Cbriggsae_P3p_SS <- cbind(Models = rownames(va_liab_Cbriggsae_P3p_SS),va_liab_Cbriggsae_P3p_SS)
        rownames(va_liab_Cbriggsae_P3p_SS) <- NULL
        va_liab_Cbriggsae_P3p_SS$Pnp <- c("P3.p","P3.p","P3.p")
        va_liab_Cbriggsae_P3p_SS$Treatment <- c("Control","MA","Vm")
        va_liab_Cbriggsae_P3p_SS$Measure <- c("Va","Va","Va")
        va_liab_Cbriggsae_P3p_SS$Scale <- c("liab","liab","liab")
        va_liab_Cbriggsae_P3p_SS$Variance <- c("Vm","Vm","Vm")
        va_liab_Cbriggsae_P3p_SS
      }
      
      #Summary h2_liab_Cbriggsae_P3p_SS
      {
        mean_h2_liab_Cbriggsae_P3p_SS <- rbind(mean(h2_liab_Cbriggsae_CONTROL_P3p_SS_mod),mean(h2_liab_Cbriggsae_MA_P3p_SS_mod),mean(h2_liab_Cbriggsae_Vm_P3p_SS_mod))
        colnames(mean_h2_liab_Cbriggsae_P3p_SS) <- c("mean")
        median_h2_liab_Cbriggsae_P3p_SS <- rbind(median(h2_liab_Cbriggsae_CONTROL_P3p_SS_mod),median(h2_liab_Cbriggsae_MA_P3p_SS_mod),median(h2_liab_Cbriggsae_Vm_P3p_SS_mod))
        colnames(median_h2_liab_Cbriggsae_P3p_SS) <- c("median")
        posterior.mode_h2_liab_Cbriggsae_P3p_SS <- rbind(posterior.mode(h2_liab_Cbriggsae_CONTROL_P3p_SS_mod),posterior.mode(h2_liab_Cbriggsae_MA_P3p_SS_mod),posterior.mode(h2_liab_Cbriggsae_Vm_P3p_SS_mod))
        colnames(posterior.mode_h2_liab_Cbriggsae_P3p_SS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Cbriggsae_P3p_SS <- rbind(HPDinterval(h2_liab_Cbriggsae_CONTROL_P3p_SS_mod),HPDinterval(h2_liab_Cbriggsae_MA_P3p_SS_mod),HPDinterval(h2_liab_Cbriggsae_Vm_P3p_SS_mod))
        colnames(HPDinterval_0.95_h2_liab_Cbriggsae_P3p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Cbriggsae_P3p_SS <- rbind(HPDinterval(h2_liab_Cbriggsae_CONTROL_P3p_SS_mod,prob=.83),HPDinterval(h2_liab_Cbriggsae_MA_P3p_SS_mod,prob=.83),HPDinterval(h2_liab_Cbriggsae_Vm_P3p_SS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Cbriggsae_P3p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Cbriggsae_P3p_SS <- rbind(effectiveSize(h2_liab_Cbriggsae_CONTROL_P3p_SS_mod),effectiveSize(h2_liab_Cbriggsae_MA_P3p_SS_mod),effectiveSize(h2_liab_Cbriggsae_Vm_P3p_SS_mod))
        colnames(effectiveSize_h2_liab_Cbriggsae_P3p_SS) <- c("effectiveSize")
        h2_liab_Cbriggsae_P3p_SS <- cbind.data.frame(mean_h2_liab_Cbriggsae_P3p_SS,median_h2_liab_Cbriggsae_P3p_SS,posterior.mode_h2_liab_Cbriggsae_P3p_SS,HPDinterval_0.95_h2_liab_Cbriggsae_P3p_SS,HPDinterval_0.83_h2_liab_Cbriggsae_P3p_SS,effectiveSize_h2_liab_Cbriggsae_P3p_SS)
        rownames(h2_liab_Cbriggsae_P3p_SS) <- c("h2_liab_Cbriggsae_CONTROL_P3p_SS_mod","h2_liab_Cbriggsae_MA_P3p_SS_mod","h2_liab_Cbriggsae_Vm_P3p_SS_mod")
        h2_liab_Cbriggsae_P3p_SS <- cbind(Models = rownames(h2_liab_Cbriggsae_P3p_SS),h2_liab_Cbriggsae_P3p_SS)
        rownames(h2_liab_Cbriggsae_P3p_SS) <- NULL
        h2_liab_Cbriggsae_P3p_SS$Pnp <- c("P3.p","P3.p","P3.p")
        h2_liab_Cbriggsae_P3p_SS$Treatment <- c("Control","MA","Vm")
        h2_liab_Cbriggsae_P3p_SS$Measure <- c("H2","H2","H2")
        h2_liab_Cbriggsae_P3p_SS$Scale <- c("liab","liab","liab")
        h2_liab_Cbriggsae_P3p_SS$Variance <- c("Vm","Vm","Vm")
        h2_liab_Cbriggsae_P3p_SS
      }
      
      #Summary Evol_liab_Cbriggsae_P3p_SS
      {
        mean_Evol_liab_Cbriggsae_P3p_SS <- rbind(mean(Evol_liab_Cbriggsae_CONTROL_P3p_SS_mod),mean(Evol_liab_Cbriggsae_MA_P3p_SS_mod),mean(Evol_liab_Cbriggsae_Vm_P3p_SS_mod))
        colnames(mean_Evol_liab_Cbriggsae_P3p_SS) <- c("mean")
        median_Evol_liab_Cbriggsae_P3p_SS <- rbind(median(Evol_liab_Cbriggsae_CONTROL_P3p_SS_mod),median(Evol_liab_Cbriggsae_MA_P3p_SS_mod),median(Evol_liab_Cbriggsae_Vm_P3p_SS_mod))
        colnames(median_Evol_liab_Cbriggsae_P3p_SS) <- c("median")
        posterior.mode_Evol_liab_Cbriggsae_P3p_SS <- rbind(posterior.mode(Evol_liab_Cbriggsae_CONTROL_P3p_SS_mod),posterior.mode(Evol_liab_Cbriggsae_MA_P3p_SS_mod),posterior.mode(Evol_liab_Cbriggsae_Vm_P3p_SS_mod))
        colnames(posterior.mode_Evol_liab_Cbriggsae_P3p_SS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Cbriggsae_P3p_SS <- rbind(HPDinterval(Evol_liab_Cbriggsae_CONTROL_P3p_SS_mod),HPDinterval(Evol_liab_Cbriggsae_MA_P3p_SS_mod),HPDinterval(Evol_liab_Cbriggsae_Vm_P3p_SS_mod))
        colnames(HPDinterval_0.95_Evol_liab_Cbriggsae_P3p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Cbriggsae_P3p_SS <- rbind(HPDinterval(Evol_liab_Cbriggsae_CONTROL_P3p_SS_mod,prob=.83),HPDinterval(Evol_liab_Cbriggsae_MA_P3p_SS_mod,prob=.83),HPDinterval(Evol_liab_Cbriggsae_Vm_P3p_SS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Cbriggsae_P3p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Cbriggsae_P3p_SS <- rbind(effectiveSize(Evol_liab_Cbriggsae_CONTROL_P3p_SS_mod),effectiveSize(Evol_liab_Cbriggsae_MA_P3p_SS_mod),effectiveSize(Evol_liab_Cbriggsae_Vm_P3p_SS_mod))
        colnames(effectiveSize_Evol_liab_Cbriggsae_P3p_SS) <- c("effectiveSize")
        Evol_liab_Cbriggsae_P3p_SS <- cbind.data.frame(mean_Evol_liab_Cbriggsae_P3p_SS,median_Evol_liab_Cbriggsae_P3p_SS,posterior.mode_Evol_liab_Cbriggsae_P3p_SS,HPDinterval_0.95_Evol_liab_Cbriggsae_P3p_SS,HPDinterval_0.83_Evol_liab_Cbriggsae_P3p_SS,effectiveSize_Evol_liab_Cbriggsae_P3p_SS)
        rownames(Evol_liab_Cbriggsae_P3p_SS) <- c("Evol_liab_Cbriggsae_CONTROL_P3p_SS_mod","Evol_liab_Cbriggsae_MA_P3p_SS_mod","Evol_liab_Cbriggsae_Vm_P3p_SS_mod")
        Evol_liab_Cbriggsae_P3p_SS <- cbind(Models = rownames(Evol_liab_Cbriggsae_P3p_SS),Evol_liab_Cbriggsae_P3p_SS)
        rownames(Evol_liab_Cbriggsae_P3p_SS) <- NULL
        Evol_liab_Cbriggsae_P3p_SS$Pnp <- c("P3.p","P3.p","P3.p")
        Evol_liab_Cbriggsae_P3p_SS$Treatment <- c("Control","MA","Vm")
        Evol_liab_Cbriggsae_P3p_SS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Cbriggsae_P3p_SS$Scale <- c("liab","liab","liab")
        Evol_liab_Cbriggsae_P3p_SS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_Cbriggsae_P3p_SS
      }
      
      #Summary trait_mean_liab_Cbriggsae_P3p_SS
      {
        mean_trait_mean_liab_Cbriggsae_P3p_SS <- rbind(mean(trait_mean_liab_Cbriggsae_CONTROL_P3p_SS_mod),mean(trait_mean_liab_Cbriggsae_MA_P3p_SS_mod),mean(trait_mean_liab_Cbriggsae_Vm_P3p_SS_mod))
        colnames(mean_trait_mean_liab_Cbriggsae_P3p_SS) <- c("mean")
        median_trait_mean_liab_Cbriggsae_P3p_SS <- rbind(median(trait_mean_liab_Cbriggsae_CONTROL_P3p_SS_mod),median(trait_mean_liab_Cbriggsae_MA_P3p_SS_mod),median(trait_mean_liab_Cbriggsae_Vm_P3p_SS_mod))
        colnames(median_trait_mean_liab_Cbriggsae_P3p_SS) <- c("median")
        posterior.mode_trait_mean_liab_Cbriggsae_P3p_SS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P3p_SS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_MA_P3p_SS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P3p_SS_mod)))
        colnames(posterior.mode_trait_mean_liab_Cbriggsae_P3p_SS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Cbriggsae_P3p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P3p_SS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_MA_P3p_SS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P3p_SS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_Cbriggsae_P3p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Cbriggsae_P3p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P3p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_MA_P3p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P3p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Cbriggsae_P3p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Cbriggsae_P3p_SS <- rbind(effectiveSize(trait_mean_liab_Cbriggsae_CONTROL_P3p_SS_mod),effectiveSize(trait_mean_liab_Cbriggsae_MA_P3p_SS_mod),effectiveSize(trait_mean_liab_Cbriggsae_Vm_P3p_SS_mod))
        colnames(effectiveSize_trait_mean_liab_Cbriggsae_P3p_SS) <- c("effectiveSize")
        trait_mean_liab_Cbriggsae_P3p_SS <- cbind.data.frame(mean_trait_mean_liab_Cbriggsae_P3p_SS,median_trait_mean_liab_Cbriggsae_P3p_SS,posterior.mode_trait_mean_liab_Cbriggsae_P3p_SS,HPDinterval_0.95_trait_mean_liab_Cbriggsae_P3p_SS,HPDinterval_0.83_trait_mean_liab_Cbriggsae_P3p_SS,effectiveSize_trait_mean_liab_Cbriggsae_P3p_SS)
        rownames(trait_mean_liab_Cbriggsae_P3p_SS) <- c("trait_mean_liab_Cbriggsae_CONTROL_P3p_SS_mod","trait_mean_liab_Cbriggsae_MA_P3p_SS_mod","trait_mean_liab_Cbriggsae_Vm_P3p_SS_mod")
        trait_mean_liab_Cbriggsae_P3p_SS <- cbind(Models = rownames(trait_mean_liab_Cbriggsae_P3p_SS),trait_mean_liab_Cbriggsae_P3p_SS)
        rownames(trait_mean_liab_Cbriggsae_P3p_SS) <- NULL
        trait_mean_liab_Cbriggsae_P3p_SS$Pnp <- c("P3.p","P3.p","P3.p")
        trait_mean_liab_Cbriggsae_P3p_SS$Treatment <- c("Control","MA","Vm")
        trait_mean_liab_Cbriggsae_P3p_SS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Cbriggsae_P3p_SS$Scale <- c("liab","liab","liab")
        trait_mean_liab_Cbriggsae_P3p_SS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_Cbriggsae_P3p_SS
      }
      
      liab_Cbriggsae_P3p_SS <- rbind.data.frame(va_liab_Cbriggsae_P3p_SS, h2_liab_Cbriggsae_P3p_SS,Evol_liab_Cbriggsae_P3p_SS,trait_mean_liab_Cbriggsae_P3p_SS)
      liab_Cbriggsae_P3p_SS
    }
    #Summary data scale Cbriggsae P3p
    {
      #Summary va_data_Cbriggsae_P3p_SS:NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_Cbriggsae_P3p_SS <- rbind(mean(va_data_Cbriggsae_CONTROL_P3p_SS_mod/2),mean(va_data_Cbriggsae_MA_P3p_SS_mod/2),mean(va_data_Cbriggsae_Vm_P3p_SS_mod/2))
        colnames(mean_va_data_Cbriggsae_P3p_SS) <- c("mean")
        median_va_data_Cbriggsae_P3p_SS <- rbind(median(va_data_Cbriggsae_CONTROL_P3p_SS_mod/2),median(va_data_Cbriggsae_MA_P3p_SS_mod/2),median(va_data_Cbriggsae_Vm_P3p_SS_mod/2))
        colnames(median_va_data_Cbriggsae_P3p_SS) <- c("median")
        posterior.mode_va_data_Cbriggsae_P3p_SS <- rbind(posterior.mode(as.mcmc(va_data_Cbriggsae_CONTROL_P3p_SS_mod/2)),posterior.mode(as.mcmc(va_data_Cbriggsae_MA_P3p_SS_mod/2)),posterior.mode(as.mcmc(va_data_Cbriggsae_Vm_P3p_SS_mod/2)))
        colnames(posterior.mode_va_data_Cbriggsae_P3p_SS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Cbriggsae_P3p_SS <- rbind(HPDinterval(as.mcmc(va_data_Cbriggsae_CONTROL_P3p_SS_mod/2)),HPDinterval(as.mcmc(va_data_Cbriggsae_MA_P3p_SS_mod/2)),HPDinterval(as.mcmc(va_data_Cbriggsae_Vm_P3p_SS_mod/2)))
        colnames(HPDinterval_0.95_va_data_Cbriggsae_P3p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Cbriggsae_P3p_SS <- rbind(HPDinterval(as.mcmc(va_data_Cbriggsae_CONTROL_P3p_SS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Cbriggsae_MA_P3p_SS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Cbriggsae_Vm_P3p_SS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Cbriggsae_P3p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Cbriggsae_P3p_SS <- rbind(effectiveSize(va_data_Cbriggsae_CONTROL_P3p_SS_mod/2),effectiveSize(va_data_Cbriggsae_MA_P3p_SS_mod/2),effectiveSize(va_data_Cbriggsae_Vm_P3p_SS_mod/2))
        colnames(effectiveSize_va_data_Cbriggsae_P3p_SS) <- c("effectiveSize")
        va_data_Cbriggsae_P3p_SS <- cbind.data.frame(mean_va_data_Cbriggsae_P3p_SS,median_va_data_Cbriggsae_P3p_SS,posterior.mode_va_data_Cbriggsae_P3p_SS,HPDinterval_0.95_va_data_Cbriggsae_P3p_SS,HPDinterval_0.83_va_data_Cbriggsae_P3p_SS,effectiveSize_va_data_Cbriggsae_P3p_SS)
        rownames(va_data_Cbriggsae_P3p_SS) <- c("va_data_Cbriggsae_CONTROL_P3p_SS_mod","va_data_Cbriggsae_MA_P3p_SS_mod","va_data_Cbriggsae_Vm_P3p_SS_mod")
        va_data_Cbriggsae_P3p_SS <- cbind(Models = rownames(va_data_Cbriggsae_P3p_SS),va_data_Cbriggsae_P3p_SS)
        rownames(va_data_Cbriggsae_P3p_SS) <- NULL
        va_data_Cbriggsae_P3p_SS$Pnp <- c("P3.p","P3.p","P3.p")
        va_data_Cbriggsae_P3p_SS$Treatment <- c("Control","MA","Vm")
        va_data_Cbriggsae_P3p_SS$Measure <- c("Va","Va","Va")
        va_data_Cbriggsae_P3p_SS$Scale <- c("data","data","data")
        va_data_Cbriggsae_P3p_SS$Variance <- c("Vm","Vm","Vm")
        va_data_Cbriggsae_P3p_SS
      }
      
      #Summary h2_data_Cbriggsae_P3p_SS
      {
        mean_h2_data_Cbriggsae_P3p_SS <- rbind(mean(h2_data_Cbriggsae_CONTROL_P3p_SS_mod),mean(h2_data_Cbriggsae_MA_P3p_SS_mod),mean(h2_data_Cbriggsae_Vm_P3p_SS_mod))
        colnames(mean_h2_data_Cbriggsae_P3p_SS) <- c("mean")
        median_h2_data_Cbriggsae_P3p_SS <- rbind(median(h2_data_Cbriggsae_CONTROL_P3p_SS_mod),median(h2_data_Cbriggsae_MA_P3p_SS_mod),median(h2_data_Cbriggsae_Vm_P3p_SS_mod))
        colnames(median_h2_data_Cbriggsae_P3p_SS) <- c("median")
        posterior.mode_h2_data_Cbriggsae_P3p_SS <- rbind(posterior.mode(as.mcmc(h2_data_Cbriggsae_CONTROL_P3p_SS_mod)),posterior.mode(as.mcmc(h2_data_Cbriggsae_MA_P3p_SS_mod)),posterior.mode(as.mcmc(h2_data_Cbriggsae_Vm_P3p_SS_mod)))
        colnames(posterior.mode_h2_data_Cbriggsae_P3p_SS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Cbriggsae_P3p_SS <- rbind(HPDinterval(as.mcmc(h2_data_Cbriggsae_CONTROL_P3p_SS_mod)),HPDinterval(as.mcmc(h2_data_Cbriggsae_MA_P3p_SS_mod)),HPDinterval(as.mcmc(h2_data_Cbriggsae_Vm_P3p_SS_mod)))
        colnames(HPDinterval_0.95_h2_data_Cbriggsae_P3p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Cbriggsae_P3p_SS <- rbind(HPDinterval(as.mcmc(h2_data_Cbriggsae_CONTROL_P3p_SS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Cbriggsae_MA_P3p_SS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Cbriggsae_Vm_P3p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Cbriggsae_P3p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Cbriggsae_P3p_SS <- rbind(effectiveSize(h2_data_Cbriggsae_CONTROL_P3p_SS_mod),effectiveSize(h2_data_Cbriggsae_MA_P3p_SS_mod),effectiveSize(h2_data_Cbriggsae_Vm_P3p_SS_mod))
        colnames(effectiveSize_h2_data_Cbriggsae_P3p_SS) <- c("effectiveSize")
        h2_data_Cbriggsae_P3p_SS <- cbind.data.frame(mean_h2_data_Cbriggsae_P3p_SS,median_h2_data_Cbriggsae_P3p_SS,posterior.mode_h2_data_Cbriggsae_P3p_SS,HPDinterval_0.95_h2_data_Cbriggsae_P3p_SS,HPDinterval_0.83_h2_data_Cbriggsae_P3p_SS,effectiveSize_h2_data_Cbriggsae_P3p_SS)
        rownames(h2_data_Cbriggsae_P3p_SS) <- c("h2_data_Cbriggsae_CONTROL_P3p_SS_mod","h2_data_Cbriggsae_MA_P3p_SS_mod","h2_data_Cbriggsae_Vm_P3p_SS_mod")
        h2_data_Cbriggsae_P3p_SS <- cbind(Models = rownames(h2_data_Cbriggsae_P3p_SS),h2_data_Cbriggsae_P3p_SS)
        rownames(h2_data_Cbriggsae_P3p_SS) <- NULL
        h2_data_Cbriggsae_P3p_SS$Pnp <- c("P3.p","P3.p","P3.p")
        h2_data_Cbriggsae_P3p_SS$Treatment <- c("Control","MA","Vm")
        h2_data_Cbriggsae_P3p_SS$Measure <- c("H2","H2","H2")
        h2_data_Cbriggsae_P3p_SS$Scale <- c("data","data","data")
        h2_data_Cbriggsae_P3p_SS$Variance <- c("Vm","Vm","Vm")
        h2_data_Cbriggsae_P3p_SS
      }
      
      #Summary Evol_data_Cbriggsae_P3p_SS
      {
        mean_Evol_data_Cbriggsae_P3p_SS <- rbind(mean(Evol_data_Cbriggsae_CONTROL_P3p_SS_mod),mean(Evol_data_Cbriggsae_MA_P3p_SS_mod),mean(Evol_data_Cbriggsae_Vm_P3p_SS_mod))
        colnames(mean_Evol_data_Cbriggsae_P3p_SS) <- c("mean")
        median_Evol_data_Cbriggsae_P3p_SS <- rbind(median(Evol_data_Cbriggsae_CONTROL_P3p_SS_mod),median(Evol_data_Cbriggsae_MA_P3p_SS_mod),median(Evol_data_Cbriggsae_Vm_P3p_SS_mod))
        colnames(median_Evol_data_Cbriggsae_P3p_SS) <- c("median")
        posterior.mode_Evol_data_Cbriggsae_P3p_SS <- rbind(posterior.mode(as.mcmc(Evol_data_Cbriggsae_CONTROL_P3p_SS_mod)),posterior.mode(as.mcmc(Evol_data_Cbriggsae_MA_P3p_SS_mod)),posterior.mode(as.mcmc(Evol_data_Cbriggsae_Vm_P3p_SS_mod)))
        colnames(posterior.mode_Evol_data_Cbriggsae_P3p_SS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Cbriggsae_P3p_SS <- rbind(HPDinterval(as.mcmc(Evol_data_Cbriggsae_CONTROL_P3p_SS_mod)),HPDinterval(as.mcmc(Evol_data_Cbriggsae_MA_P3p_SS_mod)),HPDinterval(as.mcmc(Evol_data_Cbriggsae_Vm_P3p_SS_mod)))
        colnames(HPDinterval_0.95_Evol_data_Cbriggsae_P3p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Cbriggsae_P3p_SS <- rbind(HPDinterval(as.mcmc(Evol_data_Cbriggsae_CONTROL_P3p_SS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Cbriggsae_MA_P3p_SS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Cbriggsae_Vm_P3p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Cbriggsae_P3p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Cbriggsae_P3p_SS <- rbind(effectiveSize(Evol_data_Cbriggsae_CONTROL_P3p_SS_mod),effectiveSize(Evol_data_Cbriggsae_MA_P3p_SS_mod),effectiveSize(Evol_data_Cbriggsae_Vm_P3p_SS_mod))
        colnames(effectiveSize_Evol_data_Cbriggsae_P3p_SS) <- c("effectiveSize")
        Evol_data_Cbriggsae_P3p_SS <- cbind.data.frame(mean_Evol_data_Cbriggsae_P3p_SS,median_Evol_data_Cbriggsae_P3p_SS,posterior.mode_Evol_data_Cbriggsae_P3p_SS,HPDinterval_0.95_Evol_data_Cbriggsae_P3p_SS,HPDinterval_0.83_Evol_data_Cbriggsae_P3p_SS,effectiveSize_Evol_data_Cbriggsae_P3p_SS)
        rownames(Evol_data_Cbriggsae_P3p_SS) <- c("Evol_data_Cbriggsae_CONTROL_P3p_SS_mod","Evol_data_Cbriggsae_MA_P3p_SS_mod","Evol_data_Cbriggsae_Vm_P3p_SS_mod")
        Evol_data_Cbriggsae_P3p_SS <- cbind(Models = rownames(Evol_data_Cbriggsae_P3p_SS),Evol_data_Cbriggsae_P3p_SS)
        rownames(Evol_data_Cbriggsae_P3p_SS) <- NULL
        Evol_data_Cbriggsae_P3p_SS$Pnp <- c("P3.p","P3.p","P3.p")
        Evol_data_Cbriggsae_P3p_SS$Treatment <- c("Control","MA","Vm")
        Evol_data_Cbriggsae_P3p_SS$Measure <- c("Evol","Evol","Evol")
        Evol_data_Cbriggsae_P3p_SS$Scale <- c("data","data","data")
        Evol_data_Cbriggsae_P3p_SS$Variance <- c("Vm","Vm","Vm")
        Evol_data_Cbriggsae_P3p_SS
      }
      
      #Summary trait_mean_data_Cbriggsae_P3p_SS
      {
        mean_trait_mean_data_Cbriggsae_P3p_SS <- rbind(mean(trait_mean_data_Cbriggsae_CONTROL_P3p_SS_mod),mean(trait_mean_data_Cbriggsae_MA_P3p_SS_mod),mean(trait_mean_data_Cbriggsae_Vm_P3p_SS_mod))
        colnames(mean_trait_mean_data_Cbriggsae_P3p_SS) <- c("mean")
        median_trait_mean_data_Cbriggsae_P3p_SS <- rbind(median(trait_mean_data_Cbriggsae_CONTROL_P3p_SS_mod),median(trait_mean_data_Cbriggsae_MA_P3p_SS_mod),median(trait_mean_data_Cbriggsae_Vm_P3p_SS_mod))
        colnames(median_trait_mean_data_Cbriggsae_P3p_SS) <- c("median")
        posterior.mode_trait_mean_data_Cbriggsae_P3p_SS <- rbind(posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P3p_SS_mod)),posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_MA_P3p_SS_mod)),posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_Vm_P3p_SS_mod)))
        colnames(posterior.mode_trait_mean_data_Cbriggsae_P3p_SS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Cbriggsae_P3p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P3p_SS_mod)),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_MA_P3p_SS_mod)),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_Vm_P3p_SS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_Cbriggsae_P3p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Cbriggsae_P3p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P3p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_MA_P3p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_Vm_P3p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Cbriggsae_P3p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Cbriggsae_P3p_SS <- rbind(effectiveSize(trait_mean_data_Cbriggsae_CONTROL_P3p_SS_mod),effectiveSize(trait_mean_data_Cbriggsae_MA_P3p_SS_mod),effectiveSize(trait_mean_data_Cbriggsae_Vm_P3p_SS_mod))
        colnames(effectiveSize_trait_mean_data_Cbriggsae_P3p_SS) <- c("effectiveSize")
        trait_mean_data_Cbriggsae_P3p_SS <- cbind.data.frame(mean_trait_mean_data_Cbriggsae_P3p_SS,median_trait_mean_data_Cbriggsae_P3p_SS,posterior.mode_trait_mean_data_Cbriggsae_P3p_SS,HPDinterval_0.95_trait_mean_data_Cbriggsae_P3p_SS,HPDinterval_0.83_trait_mean_data_Cbriggsae_P3p_SS,effectiveSize_trait_mean_data_Cbriggsae_P3p_SS)
        rownames(trait_mean_data_Cbriggsae_P3p_SS) <- c("trait_mean_data_Cbriggsae_CONTROL_P3p_SS_mod","trait_mean_data_Cbriggsae_MA_P3p_SS_mod","trait_mean_data_Cbriggsae_Vm_P3p_SS_mod")
        trait_mean_data_Cbriggsae_P3p_SS <- cbind(Models = rownames(trait_mean_data_Cbriggsae_P3p_SS),trait_mean_data_Cbriggsae_P3p_SS)
        rownames(trait_mean_data_Cbriggsae_P3p_SS) <- NULL
        trait_mean_data_Cbriggsae_P3p_SS$Pnp <- c("P3.p","P3.p","P3.p")
        trait_mean_data_Cbriggsae_P3p_SS$Treatment <- c("Control","MA","Vm")
        trait_mean_data_Cbriggsae_P3p_SS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Cbriggsae_P3p_SS$Scale <- c("data","data","data")
        trait_mean_data_Cbriggsae_P3p_SS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_Cbriggsae_P3p_SS
      }
      
      data_Cbriggsae_P3p_SS <- rbind.data.frame(va_data_Cbriggsae_P3p_SS, h2_data_Cbriggsae_P3p_SS,Evol_data_Cbriggsae_P3p_SS,trait_mean_data_Cbriggsae_P3p_SS)
      data_Cbriggsae_P3p_SS
      
    }
    Vm_Cbriggsae_P3p_SS <- rbind.data.frame(liab_Cbriggsae_P3p_SS, data_Cbriggsae_P3p_SS)
    Vm_Cbriggsae_P3p_SS$Pnp_fate <- rep("SS", 24)
    Vm_Cbriggsae_P3p_SS
    #remove Cbriggsae P3p_SS models
    {
      remove(Cbriggsae_CONTROL_P3p_SS_mod)
      remove(Cbriggsae_MA_P3p_SS_mod)
      remove(Cbriggsae_Vm_P3p_SS_mod)
    }
  }
  
  #Summary Cbriggsae P4p
  {
    #Summary liability scale Cbriggsae P4p
    {
      #Summary va_liab_Cbriggsae_P4p_SS: NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_Cbriggsae_P4p_SS <- rbind(mean(va_liab_Cbriggsae_CONTROL_P4p_SS_mod/2),mean(va_liab_Cbriggsae_MA_P4p_SS_mod/2),mean(va_liab_Cbriggsae_Vm_P4p_SS_mod/2))
        colnames(mean_va_liab_Cbriggsae_P4p_SS) <- c("mean")
        median_va_liab_Cbriggsae_P4p_SS <- rbind(median(va_liab_Cbriggsae_CONTROL_P4p_SS_mod/2),median(va_liab_Cbriggsae_MA_P4p_SS_mod/2),median(va_liab_Cbriggsae_Vm_P4p_SS_mod/2))
        colnames(median_va_liab_Cbriggsae_P4p_SS) <- c("median")
        posterior.mode_va_liab_Cbriggsae_P4p_SS <- rbind(posterior.mode(va_liab_Cbriggsae_CONTROL_P4p_SS_mod/2),posterior.mode(va_liab_Cbriggsae_MA_P4p_SS_mod/2),posterior.mode(va_liab_Cbriggsae_Vm_P4p_SS_mod/2))
        colnames(posterior.mode_va_liab_Cbriggsae_P4p_SS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Cbriggsae_P4p_SS <- rbind(HPDinterval(va_liab_Cbriggsae_CONTROL_P4p_SS_mod/2),HPDinterval(va_liab_Cbriggsae_MA_P4p_SS_mod/2),HPDinterval(va_liab_Cbriggsae_Vm_P4p_SS_mod/2))
        colnames(HPDinterval_0.95_va_liab_Cbriggsae_P4p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Cbriggsae_P4p_SS <- rbind(HPDinterval(va_liab_Cbriggsae_CONTROL_P4p_SS_mod/2,prob=.83),HPDinterval(va_liab_Cbriggsae_MA_P4p_SS_mod/2,prob=.83),HPDinterval(va_liab_Cbriggsae_Vm_P4p_SS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Cbriggsae_P4p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Cbriggsae_P4p_SS <- rbind(effectiveSize(va_liab_Cbriggsae_CONTROL_P4p_SS_mod/2),effectiveSize(va_liab_Cbriggsae_MA_P4p_SS_mod/2),effectiveSize(va_liab_Cbriggsae_Vm_P4p_SS_mod/2))
        colnames(effectiveSize_va_liab_Cbriggsae_P4p_SS) <- c("effectiveSize")
        va_liab_Cbriggsae_P4p_SS <- cbind.data.frame(mean_va_liab_Cbriggsae_P4p_SS,median_va_liab_Cbriggsae_P4p_SS,posterior.mode_va_liab_Cbriggsae_P4p_SS,HPDinterval_0.95_va_liab_Cbriggsae_P4p_SS,HPDinterval_0.83_va_liab_Cbriggsae_P4p_SS,effectiveSize_va_liab_Cbriggsae_P4p_SS)
        rownames(va_liab_Cbriggsae_P4p_SS) <- c("va_liab_Cbriggsae_CONTROL_P4p_SS_mod","va_liab_Cbriggsae_MA_P4p_SS_mod","va_liab_Cbriggsae_Vm_P4p_SS_mod")
        va_liab_Cbriggsae_P4p_SS <- cbind(Models = rownames(va_liab_Cbriggsae_P4p_SS),va_liab_Cbriggsae_P4p_SS)
        rownames(va_liab_Cbriggsae_P4p_SS) <- NULL
        va_liab_Cbriggsae_P4p_SS$Pnp <- c("P4.p","P4.p","P4.p")
        va_liab_Cbriggsae_P4p_SS$Treatment <- c("Control","MA","Vm")
        va_liab_Cbriggsae_P4p_SS$Measure <- c("Va","Va","Va")
        va_liab_Cbriggsae_P4p_SS$Scale <- c("liab","liab","liab")
        va_liab_Cbriggsae_P4p_SS$Variance <- c("Vm","Vm","Vm")
        va_liab_Cbriggsae_P4p_SS
      }
      
      #Summary h2_liab_Cbriggsae_P4p_SS
      {
        mean_h2_liab_Cbriggsae_P4p_SS <- rbind(mean(h2_liab_Cbriggsae_CONTROL_P4p_SS_mod),mean(h2_liab_Cbriggsae_MA_P4p_SS_mod),mean(h2_liab_Cbriggsae_Vm_P4p_SS_mod))
        colnames(mean_h2_liab_Cbriggsae_P4p_SS) <- c("mean")
        median_h2_liab_Cbriggsae_P4p_SS <- rbind(median(h2_liab_Cbriggsae_CONTROL_P4p_SS_mod),median(h2_liab_Cbriggsae_MA_P4p_SS_mod),median(h2_liab_Cbriggsae_Vm_P4p_SS_mod))
        colnames(median_h2_liab_Cbriggsae_P4p_SS) <- c("median")
        posterior.mode_h2_liab_Cbriggsae_P4p_SS <- rbind(posterior.mode(h2_liab_Cbriggsae_CONTROL_P4p_SS_mod),posterior.mode(h2_liab_Cbriggsae_MA_P4p_SS_mod),posterior.mode(h2_liab_Cbriggsae_Vm_P4p_SS_mod))
        colnames(posterior.mode_h2_liab_Cbriggsae_P4p_SS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Cbriggsae_P4p_SS <- rbind(HPDinterval(h2_liab_Cbriggsae_CONTROL_P4p_SS_mod),HPDinterval(h2_liab_Cbriggsae_MA_P4p_SS_mod),HPDinterval(h2_liab_Cbriggsae_Vm_P4p_SS_mod))
        colnames(HPDinterval_0.95_h2_liab_Cbriggsae_P4p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Cbriggsae_P4p_SS <- rbind(HPDinterval(h2_liab_Cbriggsae_CONTROL_P4p_SS_mod,prob=.83),HPDinterval(h2_liab_Cbriggsae_MA_P4p_SS_mod,prob=.83),HPDinterval(h2_liab_Cbriggsae_Vm_P4p_SS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Cbriggsae_P4p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Cbriggsae_P4p_SS <- rbind(effectiveSize(h2_liab_Cbriggsae_CONTROL_P4p_SS_mod),effectiveSize(h2_liab_Cbriggsae_MA_P4p_SS_mod),effectiveSize(h2_liab_Cbriggsae_Vm_P4p_SS_mod))
        colnames(effectiveSize_h2_liab_Cbriggsae_P4p_SS) <- c("effectiveSize")
        h2_liab_Cbriggsae_P4p_SS <- cbind.data.frame(mean_h2_liab_Cbriggsae_P4p_SS,median_h2_liab_Cbriggsae_P4p_SS,posterior.mode_h2_liab_Cbriggsae_P4p_SS,HPDinterval_0.95_h2_liab_Cbriggsae_P4p_SS,HPDinterval_0.83_h2_liab_Cbriggsae_P4p_SS,effectiveSize_h2_liab_Cbriggsae_P4p_SS)
        rownames(h2_liab_Cbriggsae_P4p_SS) <- c("h2_liab_Cbriggsae_CONTROL_P4p_SS_mod","h2_liab_Cbriggsae_MA_P4p_SS_mod","h2_liab_Cbriggsae_Vm_P4p_SS_mod")
        h2_liab_Cbriggsae_P4p_SS <- cbind(Models = rownames(h2_liab_Cbriggsae_P4p_SS),h2_liab_Cbriggsae_P4p_SS)
        rownames(h2_liab_Cbriggsae_P4p_SS) <- NULL
        h2_liab_Cbriggsae_P4p_SS$Pnp <- c("P4.p","P4.p","P4.p")
        h2_liab_Cbriggsae_P4p_SS$Treatment <- c("Control","MA","Vm")
        h2_liab_Cbriggsae_P4p_SS$Measure <- c("H2","H2","H2")
        h2_liab_Cbriggsae_P4p_SS$Scale <- c("liab","liab","liab")
        h2_liab_Cbriggsae_P4p_SS$Variance <- c("Vm","Vm","Vm")
        h2_liab_Cbriggsae_P4p_SS
      }
      
      #Summary Evol_liab_Cbriggsae_P4p_SS
      {
        mean_Evol_liab_Cbriggsae_P4p_SS <- rbind(mean(Evol_liab_Cbriggsae_CONTROL_P4p_SS_mod),mean(Evol_liab_Cbriggsae_MA_P4p_SS_mod),mean(Evol_liab_Cbriggsae_Vm_P4p_SS_mod))
        colnames(mean_Evol_liab_Cbriggsae_P4p_SS) <- c("mean")
        median_Evol_liab_Cbriggsae_P4p_SS <- rbind(median(Evol_liab_Cbriggsae_CONTROL_P4p_SS_mod),median(Evol_liab_Cbriggsae_MA_P4p_SS_mod),median(Evol_liab_Cbriggsae_Vm_P4p_SS_mod))
        colnames(median_Evol_liab_Cbriggsae_P4p_SS) <- c("median")
        posterior.mode_Evol_liab_Cbriggsae_P4p_SS <- rbind(posterior.mode(Evol_liab_Cbriggsae_CONTROL_P4p_SS_mod),posterior.mode(Evol_liab_Cbriggsae_MA_P4p_SS_mod),posterior.mode(Evol_liab_Cbriggsae_Vm_P4p_SS_mod))
        colnames(posterior.mode_Evol_liab_Cbriggsae_P4p_SS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Cbriggsae_P4p_SS <- rbind(HPDinterval(Evol_liab_Cbriggsae_CONTROL_P4p_SS_mod),HPDinterval(Evol_liab_Cbriggsae_MA_P4p_SS_mod),HPDinterval(Evol_liab_Cbriggsae_Vm_P4p_SS_mod))
        colnames(HPDinterval_0.95_Evol_liab_Cbriggsae_P4p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Cbriggsae_P4p_SS <- rbind(HPDinterval(Evol_liab_Cbriggsae_CONTROL_P4p_SS_mod,prob=.83),HPDinterval(Evol_liab_Cbriggsae_MA_P4p_SS_mod,prob=.83),HPDinterval(Evol_liab_Cbriggsae_Vm_P4p_SS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Cbriggsae_P4p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Cbriggsae_P4p_SS <- rbind(effectiveSize(Evol_liab_Cbriggsae_CONTROL_P4p_SS_mod),effectiveSize(Evol_liab_Cbriggsae_MA_P4p_SS_mod),effectiveSize(Evol_liab_Cbriggsae_Vm_P4p_SS_mod))
        colnames(effectiveSize_Evol_liab_Cbriggsae_P4p_SS) <- c("effectiveSize")
        Evol_liab_Cbriggsae_P4p_SS <- cbind.data.frame(mean_Evol_liab_Cbriggsae_P4p_SS,median_Evol_liab_Cbriggsae_P4p_SS,posterior.mode_Evol_liab_Cbriggsae_P4p_SS,HPDinterval_0.95_Evol_liab_Cbriggsae_P4p_SS,HPDinterval_0.83_Evol_liab_Cbriggsae_P4p_SS,effectiveSize_Evol_liab_Cbriggsae_P4p_SS)
        rownames(Evol_liab_Cbriggsae_P4p_SS) <- c("Evol_liab_Cbriggsae_CONTROL_P4p_SS_mod","Evol_liab_Cbriggsae_MA_P4p_SS_mod","Evol_liab_Cbriggsae_Vm_P4p_SS_mod")
        Evol_liab_Cbriggsae_P4p_SS <- cbind(Models = rownames(Evol_liab_Cbriggsae_P4p_SS),Evol_liab_Cbriggsae_P4p_SS)
        rownames(Evol_liab_Cbriggsae_P4p_SS) <- NULL
        Evol_liab_Cbriggsae_P4p_SS$Pnp <- c("P4.p","P4.p","P4.p")
        Evol_liab_Cbriggsae_P4p_SS$Treatment <- c("Control","MA","Vm")
        Evol_liab_Cbriggsae_P4p_SS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Cbriggsae_P4p_SS$Scale <- c("liab","liab","liab")
        Evol_liab_Cbriggsae_P4p_SS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_Cbriggsae_P4p_SS
      }
      
      #Summary trait_mean_liab_Cbriggsae_P4p_SS
      {
        mean_trait_mean_liab_Cbriggsae_P4p_SS <- rbind(mean(trait_mean_liab_Cbriggsae_CONTROL_P4p_SS_mod),mean(trait_mean_liab_Cbriggsae_MA_P4p_SS_mod),mean(trait_mean_liab_Cbriggsae_Vm_P4p_SS_mod))
        colnames(mean_trait_mean_liab_Cbriggsae_P4p_SS) <- c("mean")
        median_trait_mean_liab_Cbriggsae_P4p_SS <- rbind(median(trait_mean_liab_Cbriggsae_CONTROL_P4p_SS_mod),median(trait_mean_liab_Cbriggsae_MA_P4p_SS_mod),median(trait_mean_liab_Cbriggsae_Vm_P4p_SS_mod))
        colnames(median_trait_mean_liab_Cbriggsae_P4p_SS) <- c("median")
        posterior.mode_trait_mean_liab_Cbriggsae_P4p_SS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P4p_SS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_MA_P4p_SS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P4p_SS_mod)))
        colnames(posterior.mode_trait_mean_liab_Cbriggsae_P4p_SS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Cbriggsae_P4p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P4p_SS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_MA_P4p_SS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P4p_SS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_Cbriggsae_P4p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Cbriggsae_P4p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P4p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_MA_P4p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P4p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Cbriggsae_P4p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Cbriggsae_P4p_SS <- rbind(effectiveSize(trait_mean_liab_Cbriggsae_CONTROL_P4p_SS_mod),effectiveSize(trait_mean_liab_Cbriggsae_MA_P4p_SS_mod),effectiveSize(trait_mean_liab_Cbriggsae_Vm_P4p_SS_mod))
        colnames(effectiveSize_trait_mean_liab_Cbriggsae_P4p_SS) <- c("effectiveSize")
        trait_mean_liab_Cbriggsae_P4p_SS <- cbind.data.frame(mean_trait_mean_liab_Cbriggsae_P4p_SS,median_trait_mean_liab_Cbriggsae_P4p_SS,posterior.mode_trait_mean_liab_Cbriggsae_P4p_SS,HPDinterval_0.95_trait_mean_liab_Cbriggsae_P4p_SS,HPDinterval_0.83_trait_mean_liab_Cbriggsae_P4p_SS,effectiveSize_trait_mean_liab_Cbriggsae_P4p_SS)
        rownames(trait_mean_liab_Cbriggsae_P4p_SS) <- c("trait_mean_liab_Cbriggsae_CONTROL_P4p_SS_mod","trait_mean_liab_Cbriggsae_MA_P4p_SS_mod","trait_mean_liab_Cbriggsae_Vm_P4p_SS_mod")
        trait_mean_liab_Cbriggsae_P4p_SS <- cbind(Models = rownames(trait_mean_liab_Cbriggsae_P4p_SS),trait_mean_liab_Cbriggsae_P4p_SS)
        rownames(trait_mean_liab_Cbriggsae_P4p_SS) <- NULL
        trait_mean_liab_Cbriggsae_P4p_SS$Pnp <- c("P4.p","P4.p","P4.p")
        trait_mean_liab_Cbriggsae_P4p_SS$Treatment <- c("Control","MA","Vm")
        trait_mean_liab_Cbriggsae_P4p_SS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Cbriggsae_P4p_SS$Scale <- c("liab","liab","liab")
        trait_mean_liab_Cbriggsae_P4p_SS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_Cbriggsae_P4p_SS
      }
      
      liab_Cbriggsae_P4p_SS <- rbind.data.frame(va_liab_Cbriggsae_P4p_SS, h2_liab_Cbriggsae_P4p_SS,Evol_liab_Cbriggsae_P4p_SS,trait_mean_liab_Cbriggsae_P4p_SS)
      liab_Cbriggsae_P4p_SS
    }
    #Summary data scale Cbriggsae P4p
    {
      #Summary va_data_Cbriggsae_P4p_SS:NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_Cbriggsae_P4p_SS <- rbind(mean(va_data_Cbriggsae_CONTROL_P4p_SS_mod/2),mean(va_data_Cbriggsae_MA_P4p_SS_mod/2),mean(va_data_Cbriggsae_Vm_P4p_SS_mod/2))
        colnames(mean_va_data_Cbriggsae_P4p_SS) <- c("mean")
        median_va_data_Cbriggsae_P4p_SS <- rbind(median(va_data_Cbriggsae_CONTROL_P4p_SS_mod/2),median(va_data_Cbriggsae_MA_P4p_SS_mod/2),median(va_data_Cbriggsae_Vm_P4p_SS_mod/2))
        colnames(median_va_data_Cbriggsae_P4p_SS) <- c("median")
        posterior.mode_va_data_Cbriggsae_P4p_SS <- rbind(posterior.mode(as.mcmc(va_data_Cbriggsae_CONTROL_P4p_SS_mod/2)),posterior.mode(as.mcmc(va_data_Cbriggsae_MA_P4p_SS_mod/2)),posterior.mode(as.mcmc(va_data_Cbriggsae_Vm_P4p_SS_mod/2)))
        colnames(posterior.mode_va_data_Cbriggsae_P4p_SS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Cbriggsae_P4p_SS <- rbind(HPDinterval(as.mcmc(va_data_Cbriggsae_CONTROL_P4p_SS_mod/2)),HPDinterval(as.mcmc(va_data_Cbriggsae_MA_P4p_SS_mod/2)),HPDinterval(as.mcmc(va_data_Cbriggsae_Vm_P4p_SS_mod/2)))
        colnames(HPDinterval_0.95_va_data_Cbriggsae_P4p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Cbriggsae_P4p_SS <- rbind(HPDinterval(as.mcmc(va_data_Cbriggsae_CONTROL_P4p_SS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Cbriggsae_MA_P4p_SS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Cbriggsae_Vm_P4p_SS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Cbriggsae_P4p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Cbriggsae_P4p_SS <- rbind(effectiveSize(va_data_Cbriggsae_CONTROL_P4p_SS_mod/2),effectiveSize(va_data_Cbriggsae_MA_P4p_SS_mod/2),effectiveSize(va_data_Cbriggsae_Vm_P4p_SS_mod/2))
        colnames(effectiveSize_va_data_Cbriggsae_P4p_SS) <- c("effectiveSize")
        va_data_Cbriggsae_P4p_SS <- cbind.data.frame(mean_va_data_Cbriggsae_P4p_SS,median_va_data_Cbriggsae_P4p_SS,posterior.mode_va_data_Cbriggsae_P4p_SS,HPDinterval_0.95_va_data_Cbriggsae_P4p_SS,HPDinterval_0.83_va_data_Cbriggsae_P4p_SS,effectiveSize_va_data_Cbriggsae_P4p_SS)
        rownames(va_data_Cbriggsae_P4p_SS) <- c("va_data_Cbriggsae_CONTROL_P4p_SS_mod","va_data_Cbriggsae_MA_P4p_SS_mod","va_data_Cbriggsae_Vm_P4p_SS_mod")
        va_data_Cbriggsae_P4p_SS <- cbind(Models = rownames(va_data_Cbriggsae_P4p_SS),va_data_Cbriggsae_P4p_SS)
        rownames(va_data_Cbriggsae_P4p_SS) <- NULL
        va_data_Cbriggsae_P4p_SS$Pnp <- c("P4.p","P4.p","P4.p")
        va_data_Cbriggsae_P4p_SS$Treatment <- c("Control","MA","Vm")
        va_data_Cbriggsae_P4p_SS$Measure <- c("Va","Va","Va")
        va_data_Cbriggsae_P4p_SS$Scale <- c("data","data","data")
        va_data_Cbriggsae_P4p_SS$Variance <- c("Vm","Vm","Vm")
        va_data_Cbriggsae_P4p_SS
      }
      
      #Summary h2_data_Cbriggsae_P4p_SS
      {
        mean_h2_data_Cbriggsae_P4p_SS <- rbind(mean(h2_data_Cbriggsae_CONTROL_P4p_SS_mod),mean(h2_data_Cbriggsae_MA_P4p_SS_mod),mean(h2_data_Cbriggsae_Vm_P4p_SS_mod))
        colnames(mean_h2_data_Cbriggsae_P4p_SS) <- c("mean")
        median_h2_data_Cbriggsae_P4p_SS <- rbind(median(h2_data_Cbriggsae_CONTROL_P4p_SS_mod),median(h2_data_Cbriggsae_MA_P4p_SS_mod),median(h2_data_Cbriggsae_Vm_P4p_SS_mod))
        colnames(median_h2_data_Cbriggsae_P4p_SS) <- c("median")
        posterior.mode_h2_data_Cbriggsae_P4p_SS <- rbind(posterior.mode(as.mcmc(h2_data_Cbriggsae_CONTROL_P4p_SS_mod)),posterior.mode(as.mcmc(h2_data_Cbriggsae_MA_P4p_SS_mod)),posterior.mode(as.mcmc(h2_data_Cbriggsae_Vm_P4p_SS_mod)))
        colnames(posterior.mode_h2_data_Cbriggsae_P4p_SS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Cbriggsae_P4p_SS <- rbind(HPDinterval(as.mcmc(h2_data_Cbriggsae_CONTROL_P4p_SS_mod)),HPDinterval(as.mcmc(h2_data_Cbriggsae_MA_P4p_SS_mod)),HPDinterval(as.mcmc(h2_data_Cbriggsae_Vm_P4p_SS_mod)))
        colnames(HPDinterval_0.95_h2_data_Cbriggsae_P4p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Cbriggsae_P4p_SS <- rbind(HPDinterval(as.mcmc(h2_data_Cbriggsae_CONTROL_P4p_SS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Cbriggsae_MA_P4p_SS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Cbriggsae_Vm_P4p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Cbriggsae_P4p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Cbriggsae_P4p_SS <- rbind(effectiveSize(h2_data_Cbriggsae_CONTROL_P4p_SS_mod),effectiveSize(h2_data_Cbriggsae_MA_P4p_SS_mod),effectiveSize(h2_data_Cbriggsae_Vm_P4p_SS_mod))
        colnames(effectiveSize_h2_data_Cbriggsae_P4p_SS) <- c("effectiveSize")
        h2_data_Cbriggsae_P4p_SS <- cbind.data.frame(mean_h2_data_Cbriggsae_P4p_SS,median_h2_data_Cbriggsae_P4p_SS,posterior.mode_h2_data_Cbriggsae_P4p_SS,HPDinterval_0.95_h2_data_Cbriggsae_P4p_SS,HPDinterval_0.83_h2_data_Cbriggsae_P4p_SS,effectiveSize_h2_data_Cbriggsae_P4p_SS)
        rownames(h2_data_Cbriggsae_P4p_SS) <- c("h2_data_Cbriggsae_CONTROL_P4p_SS_mod","h2_data_Cbriggsae_MA_P4p_SS_mod","h2_data_Cbriggsae_Vm_P4p_SS_mod")
        h2_data_Cbriggsae_P4p_SS <- cbind(Models = rownames(h2_data_Cbriggsae_P4p_SS),h2_data_Cbriggsae_P4p_SS)
        rownames(h2_data_Cbriggsae_P4p_SS) <- NULL
        h2_data_Cbriggsae_P4p_SS$Pnp <- c("P4.p","P4.p","P4.p")
        h2_data_Cbriggsae_P4p_SS$Treatment <- c("Control","MA","Vm")
        h2_data_Cbriggsae_P4p_SS$Measure <- c("H2","H2","H2")
        h2_data_Cbriggsae_P4p_SS$Scale <- c("data","data","data")
        h2_data_Cbriggsae_P4p_SS$Variance <- c("Vm","Vm","Vm")
        h2_data_Cbriggsae_P4p_SS
      }
      
      #Summary Evol_data_Cbriggsae_P4p_SS
      {
        mean_Evol_data_Cbriggsae_P4p_SS <- rbind(mean(Evol_data_Cbriggsae_CONTROL_P4p_SS_mod),mean(Evol_data_Cbriggsae_MA_P4p_SS_mod),mean(Evol_data_Cbriggsae_Vm_P4p_SS_mod))
        colnames(mean_Evol_data_Cbriggsae_P4p_SS) <- c("mean")
        median_Evol_data_Cbriggsae_P4p_SS <- rbind(median(Evol_data_Cbriggsae_CONTROL_P4p_SS_mod),median(Evol_data_Cbriggsae_MA_P4p_SS_mod),median(Evol_data_Cbriggsae_Vm_P4p_SS_mod))
        colnames(median_Evol_data_Cbriggsae_P4p_SS) <- c("median")
        posterior.mode_Evol_data_Cbriggsae_P4p_SS <- rbind(posterior.mode(as.mcmc(Evol_data_Cbriggsae_CONTROL_P4p_SS_mod)),posterior.mode(as.mcmc(Evol_data_Cbriggsae_MA_P4p_SS_mod)),posterior.mode(as.mcmc(Evol_data_Cbriggsae_Vm_P4p_SS_mod)))
        colnames(posterior.mode_Evol_data_Cbriggsae_P4p_SS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Cbriggsae_P4p_SS <- rbind(HPDinterval(as.mcmc(Evol_data_Cbriggsae_CONTROL_P4p_SS_mod)),HPDinterval(as.mcmc(Evol_data_Cbriggsae_MA_P4p_SS_mod)),HPDinterval(as.mcmc(Evol_data_Cbriggsae_Vm_P4p_SS_mod)))
        colnames(HPDinterval_0.95_Evol_data_Cbriggsae_P4p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Cbriggsae_P4p_SS <- rbind(HPDinterval(as.mcmc(Evol_data_Cbriggsae_CONTROL_P4p_SS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Cbriggsae_MA_P4p_SS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Cbriggsae_Vm_P4p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Cbriggsae_P4p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Cbriggsae_P4p_SS <- rbind(effectiveSize(Evol_data_Cbriggsae_CONTROL_P4p_SS_mod),effectiveSize(Evol_data_Cbriggsae_MA_P4p_SS_mod),effectiveSize(Evol_data_Cbriggsae_Vm_P4p_SS_mod))
        colnames(effectiveSize_Evol_data_Cbriggsae_P4p_SS) <- c("effectiveSize")
        Evol_data_Cbriggsae_P4p_SS <- cbind.data.frame(mean_Evol_data_Cbriggsae_P4p_SS,median_Evol_data_Cbriggsae_P4p_SS,posterior.mode_Evol_data_Cbriggsae_P4p_SS,HPDinterval_0.95_Evol_data_Cbriggsae_P4p_SS,HPDinterval_0.83_Evol_data_Cbriggsae_P4p_SS,effectiveSize_Evol_data_Cbriggsae_P4p_SS)
        rownames(Evol_data_Cbriggsae_P4p_SS) <- c("Evol_data_Cbriggsae_CONTROL_P4p_SS_mod","Evol_data_Cbriggsae_MA_P4p_SS_mod","Evol_data_Cbriggsae_Vm_P4p_SS_mod")
        Evol_data_Cbriggsae_P4p_SS <- cbind(Models = rownames(Evol_data_Cbriggsae_P4p_SS),Evol_data_Cbriggsae_P4p_SS)
        rownames(Evol_data_Cbriggsae_P4p_SS) <- NULL
        Evol_data_Cbriggsae_P4p_SS$Pnp <- c("P4.p","P4.p","P4.p")
        Evol_data_Cbriggsae_P4p_SS$Treatment <- c("Control","MA","Vm")
        Evol_data_Cbriggsae_P4p_SS$Measure <- c("Evol","Evol","Evol")
        Evol_data_Cbriggsae_P4p_SS$Scale <- c("data","data","data")
        Evol_data_Cbriggsae_P4p_SS$Variance <- c("Vm","Vm","Vm")
        Evol_data_Cbriggsae_P4p_SS
      }
      
      #Summary trait_mean_data_Cbriggsae_P4p_SS
      {
        mean_trait_mean_data_Cbriggsae_P4p_SS <- rbind(mean(trait_mean_data_Cbriggsae_CONTROL_P4p_SS_mod),mean(trait_mean_data_Cbriggsae_MA_P4p_SS_mod),mean(trait_mean_data_Cbriggsae_Vm_P4p_SS_mod))
        colnames(mean_trait_mean_data_Cbriggsae_P4p_SS) <- c("mean")
        median_trait_mean_data_Cbriggsae_P4p_SS <- rbind(median(trait_mean_data_Cbriggsae_CONTROL_P4p_SS_mod),median(trait_mean_data_Cbriggsae_MA_P4p_SS_mod),median(trait_mean_data_Cbriggsae_Vm_P4p_SS_mod))
        colnames(median_trait_mean_data_Cbriggsae_P4p_SS) <- c("median")
        posterior.mode_trait_mean_data_Cbriggsae_P4p_SS <- rbind(posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P4p_SS_mod)),posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_MA_P4p_SS_mod)),posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_Vm_P4p_SS_mod)))
        colnames(posterior.mode_trait_mean_data_Cbriggsae_P4p_SS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Cbriggsae_P4p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P4p_SS_mod)),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_MA_P4p_SS_mod)),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_Vm_P4p_SS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_Cbriggsae_P4p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Cbriggsae_P4p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P4p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_MA_P4p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_Vm_P4p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Cbriggsae_P4p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Cbriggsae_P4p_SS <- rbind(effectiveSize(trait_mean_data_Cbriggsae_CONTROL_P4p_SS_mod),effectiveSize(trait_mean_data_Cbriggsae_MA_P4p_SS_mod),effectiveSize(trait_mean_data_Cbriggsae_Vm_P4p_SS_mod))
        colnames(effectiveSize_trait_mean_data_Cbriggsae_P4p_SS) <- c("effectiveSize")
        trait_mean_data_Cbriggsae_P4p_SS <- cbind.data.frame(mean_trait_mean_data_Cbriggsae_P4p_SS,median_trait_mean_data_Cbriggsae_P4p_SS,posterior.mode_trait_mean_data_Cbriggsae_P4p_SS,HPDinterval_0.95_trait_mean_data_Cbriggsae_P4p_SS,HPDinterval_0.83_trait_mean_data_Cbriggsae_P4p_SS,effectiveSize_trait_mean_data_Cbriggsae_P4p_SS)
        rownames(trait_mean_data_Cbriggsae_P4p_SS) <- c("trait_mean_data_Cbriggsae_CONTROL_P4p_SS_mod","trait_mean_data_Cbriggsae_MA_P4p_SS_mod","trait_mean_data_Cbriggsae_Vm_P4p_SS_mod")
        trait_mean_data_Cbriggsae_P4p_SS <- cbind(Models = rownames(trait_mean_data_Cbriggsae_P4p_SS),trait_mean_data_Cbriggsae_P4p_SS)
        rownames(trait_mean_data_Cbriggsae_P4p_SS) <- NULL
        trait_mean_data_Cbriggsae_P4p_SS$Pnp <- c("P4.p","P4.p","P4.p")
        trait_mean_data_Cbriggsae_P4p_SS$Treatment <- c("Control","MA","Vm")
        trait_mean_data_Cbriggsae_P4p_SS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Cbriggsae_P4p_SS$Scale <- c("data","data","data")
        trait_mean_data_Cbriggsae_P4p_SS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_Cbriggsae_P4p_SS
      }
      
      data_Cbriggsae_P4p_SS <- rbind.data.frame(va_data_Cbriggsae_P4p_SS, h2_data_Cbriggsae_P4p_SS,Evol_data_Cbriggsae_P4p_SS,trait_mean_data_Cbriggsae_P4p_SS)
      data_Cbriggsae_P4p_SS
      
    }
    Vm_Cbriggsae_P4p_SS <- rbind.data.frame(liab_Cbriggsae_P4p_SS, data_Cbriggsae_P4p_SS)
    Vm_Cbriggsae_P4p_SS$Pnp_fate <- rep("SS", 24)
    Vm_Cbriggsae_P4p_SS
    #remove Cbriggsae P4p_SS models
    {
      remove(Cbriggsae_CONTROL_P4p_SS_mod)
      remove(Cbriggsae_MA_P4p_SS_mod)
      remove(Cbriggsae_Vm_P4p_SS_mod)
    }
  }
  
  #Summary Cbriggsae P8p
  {
    #Summary liability scale Cbriggsae P8p
    {
      #Summary va_liab_Cbriggsae_P8p_SS: NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_Cbriggsae_P8p_SS <- rbind(mean(va_liab_Cbriggsae_CONTROL_P8p_SS_mod/2),mean(va_liab_Cbriggsae_MA_P8p_SS_mod/2),mean(va_liab_Cbriggsae_Vm_P8p_SS_mod/2))
        colnames(mean_va_liab_Cbriggsae_P8p_SS) <- c("mean")
        median_va_liab_Cbriggsae_P8p_SS <- rbind(median(va_liab_Cbriggsae_CONTROL_P8p_SS_mod/2),median(va_liab_Cbriggsae_MA_P8p_SS_mod/2),median(va_liab_Cbriggsae_Vm_P8p_SS_mod/2))
        colnames(median_va_liab_Cbriggsae_P8p_SS) <- c("median")
        posterior.mode_va_liab_Cbriggsae_P8p_SS <- rbind(posterior.mode(va_liab_Cbriggsae_CONTROL_P8p_SS_mod/2),posterior.mode(va_liab_Cbriggsae_MA_P8p_SS_mod/2),posterior.mode(va_liab_Cbriggsae_Vm_P8p_SS_mod/2))
        colnames(posterior.mode_va_liab_Cbriggsae_P8p_SS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Cbriggsae_P8p_SS <- rbind(HPDinterval(va_liab_Cbriggsae_CONTROL_P8p_SS_mod/2),HPDinterval(va_liab_Cbriggsae_MA_P8p_SS_mod/2),HPDinterval(va_liab_Cbriggsae_Vm_P8p_SS_mod/2))
        colnames(HPDinterval_0.95_va_liab_Cbriggsae_P8p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Cbriggsae_P8p_SS <- rbind(HPDinterval(va_liab_Cbriggsae_CONTROL_P8p_SS_mod/2,prob=.83),HPDinterval(va_liab_Cbriggsae_MA_P8p_SS_mod/2,prob=.83),HPDinterval(va_liab_Cbriggsae_Vm_P8p_SS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Cbriggsae_P8p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Cbriggsae_P8p_SS <- rbind(effectiveSize(va_liab_Cbriggsae_CONTROL_P8p_SS_mod/2),effectiveSize(va_liab_Cbriggsae_MA_P8p_SS_mod/2),effectiveSize(va_liab_Cbriggsae_Vm_P8p_SS_mod/2))
        colnames(effectiveSize_va_liab_Cbriggsae_P8p_SS) <- c("effectiveSize")
        va_liab_Cbriggsae_P8p_SS <- cbind.data.frame(mean_va_liab_Cbriggsae_P8p_SS,median_va_liab_Cbriggsae_P8p_SS,posterior.mode_va_liab_Cbriggsae_P8p_SS,HPDinterval_0.95_va_liab_Cbriggsae_P8p_SS,HPDinterval_0.83_va_liab_Cbriggsae_P8p_SS,effectiveSize_va_liab_Cbriggsae_P8p_SS)
        rownames(va_liab_Cbriggsae_P8p_SS) <- c("va_liab_Cbriggsae_CONTROL_P8p_SS_mod","va_liab_Cbriggsae_MA_P8p_SS_mod","va_liab_Cbriggsae_Vm_P8p_SS_mod")
        va_liab_Cbriggsae_P8p_SS <- cbind(Models = rownames(va_liab_Cbriggsae_P8p_SS),va_liab_Cbriggsae_P8p_SS)
        rownames(va_liab_Cbriggsae_P8p_SS) <- NULL
        va_liab_Cbriggsae_P8p_SS$Pnp <- c("P8.p","P8.p","P8.p")
        va_liab_Cbriggsae_P8p_SS$Treatment <- c("Control","MA","Vm")
        va_liab_Cbriggsae_P8p_SS$Measure <- c("Va","Va","Va")
        va_liab_Cbriggsae_P8p_SS$Scale <- c("liab","liab","liab")
        va_liab_Cbriggsae_P8p_SS$Variance <- c("Vm","Vm","Vm")
        va_liab_Cbriggsae_P8p_SS
      }
      
      #Summary h2_liab_Cbriggsae_P8p_SS
      {
        mean_h2_liab_Cbriggsae_P8p_SS <- rbind(mean(h2_liab_Cbriggsae_CONTROL_P8p_SS_mod),mean(h2_liab_Cbriggsae_MA_P8p_SS_mod),mean(h2_liab_Cbriggsae_Vm_P8p_SS_mod))
        colnames(mean_h2_liab_Cbriggsae_P8p_SS) <- c("mean")
        median_h2_liab_Cbriggsae_P8p_SS <- rbind(median(h2_liab_Cbriggsae_CONTROL_P8p_SS_mod),median(h2_liab_Cbriggsae_MA_P8p_SS_mod),median(h2_liab_Cbriggsae_Vm_P8p_SS_mod))
        colnames(median_h2_liab_Cbriggsae_P8p_SS) <- c("median")
        posterior.mode_h2_liab_Cbriggsae_P8p_SS <- rbind(posterior.mode(h2_liab_Cbriggsae_CONTROL_P8p_SS_mod),posterior.mode(h2_liab_Cbriggsae_MA_P8p_SS_mod),posterior.mode(h2_liab_Cbriggsae_Vm_P8p_SS_mod))
        colnames(posterior.mode_h2_liab_Cbriggsae_P8p_SS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Cbriggsae_P8p_SS <- rbind(HPDinterval(h2_liab_Cbriggsae_CONTROL_P8p_SS_mod),HPDinterval(h2_liab_Cbriggsae_MA_P8p_SS_mod),HPDinterval(h2_liab_Cbriggsae_Vm_P8p_SS_mod))
        colnames(HPDinterval_0.95_h2_liab_Cbriggsae_P8p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Cbriggsae_P8p_SS <- rbind(HPDinterval(h2_liab_Cbriggsae_CONTROL_P8p_SS_mod,prob=.83),HPDinterval(h2_liab_Cbriggsae_MA_P8p_SS_mod,prob=.83),HPDinterval(h2_liab_Cbriggsae_Vm_P8p_SS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Cbriggsae_P8p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Cbriggsae_P8p_SS <- rbind(effectiveSize(h2_liab_Cbriggsae_CONTROL_P8p_SS_mod),effectiveSize(h2_liab_Cbriggsae_MA_P8p_SS_mod),effectiveSize(h2_liab_Cbriggsae_Vm_P8p_SS_mod))
        colnames(effectiveSize_h2_liab_Cbriggsae_P8p_SS) <- c("effectiveSize")
        h2_liab_Cbriggsae_P8p_SS <- cbind.data.frame(mean_h2_liab_Cbriggsae_P8p_SS,median_h2_liab_Cbriggsae_P8p_SS,posterior.mode_h2_liab_Cbriggsae_P8p_SS,HPDinterval_0.95_h2_liab_Cbriggsae_P8p_SS,HPDinterval_0.83_h2_liab_Cbriggsae_P8p_SS,effectiveSize_h2_liab_Cbriggsae_P8p_SS)
        rownames(h2_liab_Cbriggsae_P8p_SS) <- c("h2_liab_Cbriggsae_CONTROL_P8p_SS_mod","h2_liab_Cbriggsae_MA_P8p_SS_mod","h2_liab_Cbriggsae_Vm_P8p_SS_mod")
        h2_liab_Cbriggsae_P8p_SS <- cbind(Models = rownames(h2_liab_Cbriggsae_P8p_SS),h2_liab_Cbriggsae_P8p_SS)
        rownames(h2_liab_Cbriggsae_P8p_SS) <- NULL
        h2_liab_Cbriggsae_P8p_SS$Pnp <- c("P8.p","P8.p","P8.p")
        h2_liab_Cbriggsae_P8p_SS$Treatment <- c("Control","MA","Vm")
        h2_liab_Cbriggsae_P8p_SS$Measure <- c("H2","H2","H2")
        h2_liab_Cbriggsae_P8p_SS$Scale <- c("liab","liab","liab")
        h2_liab_Cbriggsae_P8p_SS$Variance <- c("Vm","Vm","Vm")
        h2_liab_Cbriggsae_P8p_SS
      }
      
      #Summary Evol_liab_Cbriggsae_P8p_SS
      {
        mean_Evol_liab_Cbriggsae_P8p_SS <- rbind(mean(Evol_liab_Cbriggsae_CONTROL_P8p_SS_mod),mean(Evol_liab_Cbriggsae_MA_P8p_SS_mod),mean(Evol_liab_Cbriggsae_Vm_P8p_SS_mod))
        colnames(mean_Evol_liab_Cbriggsae_P8p_SS) <- c("mean")
        median_Evol_liab_Cbriggsae_P8p_SS <- rbind(median(Evol_liab_Cbriggsae_CONTROL_P8p_SS_mod),median(Evol_liab_Cbriggsae_MA_P8p_SS_mod),median(Evol_liab_Cbriggsae_Vm_P8p_SS_mod))
        colnames(median_Evol_liab_Cbriggsae_P8p_SS) <- c("median")
        posterior.mode_Evol_liab_Cbriggsae_P8p_SS <- rbind(posterior.mode(Evol_liab_Cbriggsae_CONTROL_P8p_SS_mod),posterior.mode(Evol_liab_Cbriggsae_MA_P8p_SS_mod),posterior.mode(Evol_liab_Cbriggsae_Vm_P8p_SS_mod))
        colnames(posterior.mode_Evol_liab_Cbriggsae_P8p_SS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Cbriggsae_P8p_SS <- rbind(HPDinterval(Evol_liab_Cbriggsae_CONTROL_P8p_SS_mod),HPDinterval(Evol_liab_Cbriggsae_MA_P8p_SS_mod),HPDinterval(Evol_liab_Cbriggsae_Vm_P8p_SS_mod))
        colnames(HPDinterval_0.95_Evol_liab_Cbriggsae_P8p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Cbriggsae_P8p_SS <- rbind(HPDinterval(Evol_liab_Cbriggsae_CONTROL_P8p_SS_mod,prob=.83),HPDinterval(Evol_liab_Cbriggsae_MA_P8p_SS_mod,prob=.83),HPDinterval(Evol_liab_Cbriggsae_Vm_P8p_SS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Cbriggsae_P8p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Cbriggsae_P8p_SS <- rbind(effectiveSize(Evol_liab_Cbriggsae_CONTROL_P8p_SS_mod),effectiveSize(Evol_liab_Cbriggsae_MA_P8p_SS_mod),effectiveSize(Evol_liab_Cbriggsae_Vm_P8p_SS_mod))
        colnames(effectiveSize_Evol_liab_Cbriggsae_P8p_SS) <- c("effectiveSize")
        Evol_liab_Cbriggsae_P8p_SS <- cbind.data.frame(mean_Evol_liab_Cbriggsae_P8p_SS,median_Evol_liab_Cbriggsae_P8p_SS,posterior.mode_Evol_liab_Cbriggsae_P8p_SS,HPDinterval_0.95_Evol_liab_Cbriggsae_P8p_SS,HPDinterval_0.83_Evol_liab_Cbriggsae_P8p_SS,effectiveSize_Evol_liab_Cbriggsae_P8p_SS)
        rownames(Evol_liab_Cbriggsae_P8p_SS) <- c("Evol_liab_Cbriggsae_CONTROL_P8p_SS_mod","Evol_liab_Cbriggsae_MA_P8p_SS_mod","Evol_liab_Cbriggsae_Vm_P8p_SS_mod")
        Evol_liab_Cbriggsae_P8p_SS <- cbind(Models = rownames(Evol_liab_Cbriggsae_P8p_SS),Evol_liab_Cbriggsae_P8p_SS)
        rownames(Evol_liab_Cbriggsae_P8p_SS) <- NULL
        Evol_liab_Cbriggsae_P8p_SS$Pnp <- c("P8.p","P8.p","P8.p")
        Evol_liab_Cbriggsae_P8p_SS$Treatment <- c("Control","MA","Vm")
        Evol_liab_Cbriggsae_P8p_SS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Cbriggsae_P8p_SS$Scale <- c("liab","liab","liab")
        Evol_liab_Cbriggsae_P8p_SS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_Cbriggsae_P8p_SS
      }
      
      #Summary trait_mean_liab_Cbriggsae_P8p_SS
      {
        mean_trait_mean_liab_Cbriggsae_P8p_SS <- rbind(mean(trait_mean_liab_Cbriggsae_CONTROL_P8p_SS_mod),mean(trait_mean_liab_Cbriggsae_MA_P8p_SS_mod),mean(trait_mean_liab_Cbriggsae_Vm_P8p_SS_mod))
        colnames(mean_trait_mean_liab_Cbriggsae_P8p_SS) <- c("mean")
        median_trait_mean_liab_Cbriggsae_P8p_SS <- rbind(median(trait_mean_liab_Cbriggsae_CONTROL_P8p_SS_mod),median(trait_mean_liab_Cbriggsae_MA_P8p_SS_mod),median(trait_mean_liab_Cbriggsae_Vm_P8p_SS_mod))
        colnames(median_trait_mean_liab_Cbriggsae_P8p_SS) <- c("median")
        posterior.mode_trait_mean_liab_Cbriggsae_P8p_SS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P8p_SS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_MA_P8p_SS_mod)),posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P8p_SS_mod)))
        colnames(posterior.mode_trait_mean_liab_Cbriggsae_P8p_SS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Cbriggsae_P8p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P8p_SS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_MA_P8p_SS_mod)),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P8p_SS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_Cbriggsae_P8p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Cbriggsae_P8p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P8p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_MA_P8p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P8p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Cbriggsae_P8p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Cbriggsae_P8p_SS <- rbind(effectiveSize(trait_mean_liab_Cbriggsae_CONTROL_P8p_SS_mod),effectiveSize(trait_mean_liab_Cbriggsae_MA_P8p_SS_mod),effectiveSize(trait_mean_liab_Cbriggsae_Vm_P8p_SS_mod))
        colnames(effectiveSize_trait_mean_liab_Cbriggsae_P8p_SS) <- c("effectiveSize")
        trait_mean_liab_Cbriggsae_P8p_SS <- cbind.data.frame(mean_trait_mean_liab_Cbriggsae_P8p_SS,median_trait_mean_liab_Cbriggsae_P8p_SS,posterior.mode_trait_mean_liab_Cbriggsae_P8p_SS,HPDinterval_0.95_trait_mean_liab_Cbriggsae_P8p_SS,HPDinterval_0.83_trait_mean_liab_Cbriggsae_P8p_SS,effectiveSize_trait_mean_liab_Cbriggsae_P8p_SS)
        rownames(trait_mean_liab_Cbriggsae_P8p_SS) <- c("trait_mean_liab_Cbriggsae_CONTROL_P8p_SS_mod","trait_mean_liab_Cbriggsae_MA_P8p_SS_mod","trait_mean_liab_Cbriggsae_Vm_P8p_SS_mod")
        trait_mean_liab_Cbriggsae_P8p_SS <- cbind(Models = rownames(trait_mean_liab_Cbriggsae_P8p_SS),trait_mean_liab_Cbriggsae_P8p_SS)
        rownames(trait_mean_liab_Cbriggsae_P8p_SS) <- NULL
        trait_mean_liab_Cbriggsae_P8p_SS$Pnp <- c("P8.p","P8.p","P8.p")
        trait_mean_liab_Cbriggsae_P8p_SS$Treatment <- c("Control","MA","Vm")
        trait_mean_liab_Cbriggsae_P8p_SS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Cbriggsae_P8p_SS$Scale <- c("liab","liab","liab")
        trait_mean_liab_Cbriggsae_P8p_SS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_Cbriggsae_P8p_SS
      }
      
      liab_Cbriggsae_P8p_SS <- rbind.data.frame(va_liab_Cbriggsae_P8p_SS, h2_liab_Cbriggsae_P8p_SS,Evol_liab_Cbriggsae_P8p_SS,trait_mean_liab_Cbriggsae_P8p_SS)
      liab_Cbriggsae_P8p_SS
    }
    #Summary data scale Cbriggsae P8p
    {
      #Summary va_data_Cbriggsae_P8p_SS:NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_Cbriggsae_P8p_SS <- rbind(mean(va_data_Cbriggsae_CONTROL_P8p_SS_mod/2),mean(va_data_Cbriggsae_MA_P8p_SS_mod/2),mean(va_data_Cbriggsae_Vm_P8p_SS_mod/2))
        colnames(mean_va_data_Cbriggsae_P8p_SS) <- c("mean")
        median_va_data_Cbriggsae_P8p_SS <- rbind(median(va_data_Cbriggsae_CONTROL_P8p_SS_mod/2),median(va_data_Cbriggsae_MA_P8p_SS_mod/2),median(va_data_Cbriggsae_Vm_P8p_SS_mod/2))
        colnames(median_va_data_Cbriggsae_P8p_SS) <- c("median")
        posterior.mode_va_data_Cbriggsae_P8p_SS <- rbind(posterior.mode(as.mcmc(va_data_Cbriggsae_CONTROL_P8p_SS_mod/2)),posterior.mode(as.mcmc(va_data_Cbriggsae_MA_P8p_SS_mod/2)),posterior.mode(as.mcmc(va_data_Cbriggsae_Vm_P8p_SS_mod/2)))
        colnames(posterior.mode_va_data_Cbriggsae_P8p_SS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Cbriggsae_P8p_SS <- rbind(HPDinterval(as.mcmc(va_data_Cbriggsae_CONTROL_P8p_SS_mod/2)),HPDinterval(as.mcmc(va_data_Cbriggsae_MA_P8p_SS_mod/2)),HPDinterval(as.mcmc(va_data_Cbriggsae_Vm_P8p_SS_mod/2)))
        colnames(HPDinterval_0.95_va_data_Cbriggsae_P8p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Cbriggsae_P8p_SS <- rbind(HPDinterval(as.mcmc(va_data_Cbriggsae_CONTROL_P8p_SS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Cbriggsae_MA_P8p_SS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Cbriggsae_Vm_P8p_SS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Cbriggsae_P8p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Cbriggsae_P8p_SS <- rbind(effectiveSize(va_data_Cbriggsae_CONTROL_P8p_SS_mod/2),effectiveSize(va_data_Cbriggsae_MA_P8p_SS_mod/2),effectiveSize(va_data_Cbriggsae_Vm_P8p_SS_mod/2))
        colnames(effectiveSize_va_data_Cbriggsae_P8p_SS) <- c("effectiveSize")
        va_data_Cbriggsae_P8p_SS <- cbind.data.frame(mean_va_data_Cbriggsae_P8p_SS,median_va_data_Cbriggsae_P8p_SS,posterior.mode_va_data_Cbriggsae_P8p_SS,HPDinterval_0.95_va_data_Cbriggsae_P8p_SS,HPDinterval_0.83_va_data_Cbriggsae_P8p_SS,effectiveSize_va_data_Cbriggsae_P8p_SS)
        rownames(va_data_Cbriggsae_P8p_SS) <- c("va_data_Cbriggsae_CONTROL_P8p_SS_mod","va_data_Cbriggsae_MA_P8p_SS_mod","va_data_Cbriggsae_Vm_P8p_SS_mod")
        va_data_Cbriggsae_P8p_SS <- cbind(Models = rownames(va_data_Cbriggsae_P8p_SS),va_data_Cbriggsae_P8p_SS)
        rownames(va_data_Cbriggsae_P8p_SS) <- NULL
        va_data_Cbriggsae_P8p_SS$Pnp <- c("P8.p","P8.p","P8.p")
        va_data_Cbriggsae_P8p_SS$Treatment <- c("Control","MA","Vm")
        va_data_Cbriggsae_P8p_SS$Measure <- c("Va","Va","Va")
        va_data_Cbriggsae_P8p_SS$Scale <- c("data","data","data")
        va_data_Cbriggsae_P8p_SS$Variance <- c("Vm","Vm","Vm")
        va_data_Cbriggsae_P8p_SS
      }
      
      #Summary h2_data_Cbriggsae_P8p_SS
      {
        mean_h2_data_Cbriggsae_P8p_SS <- rbind(mean(h2_data_Cbriggsae_CONTROL_P8p_SS_mod),mean(h2_data_Cbriggsae_MA_P8p_SS_mod),mean(h2_data_Cbriggsae_Vm_P8p_SS_mod))
        colnames(mean_h2_data_Cbriggsae_P8p_SS) <- c("mean")
        median_h2_data_Cbriggsae_P8p_SS <- rbind(median(h2_data_Cbriggsae_CONTROL_P8p_SS_mod),median(h2_data_Cbriggsae_MA_P8p_SS_mod),median(h2_data_Cbriggsae_Vm_P8p_SS_mod))
        colnames(median_h2_data_Cbriggsae_P8p_SS) <- c("median")
        posterior.mode_h2_data_Cbriggsae_P8p_SS <- rbind(posterior.mode(as.mcmc(h2_data_Cbriggsae_CONTROL_P8p_SS_mod)),posterior.mode(as.mcmc(h2_data_Cbriggsae_MA_P8p_SS_mod)),posterior.mode(as.mcmc(h2_data_Cbriggsae_Vm_P8p_SS_mod)))
        colnames(posterior.mode_h2_data_Cbriggsae_P8p_SS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Cbriggsae_P8p_SS <- rbind(HPDinterval(as.mcmc(h2_data_Cbriggsae_CONTROL_P8p_SS_mod)),HPDinterval(as.mcmc(h2_data_Cbriggsae_MA_P8p_SS_mod)),HPDinterval(as.mcmc(h2_data_Cbriggsae_Vm_P8p_SS_mod)))
        colnames(HPDinterval_0.95_h2_data_Cbriggsae_P8p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Cbriggsae_P8p_SS <- rbind(HPDinterval(as.mcmc(h2_data_Cbriggsae_CONTROL_P8p_SS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Cbriggsae_MA_P8p_SS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Cbriggsae_Vm_P8p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Cbriggsae_P8p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Cbriggsae_P8p_SS <- rbind(effectiveSize(h2_data_Cbriggsae_CONTROL_P8p_SS_mod),effectiveSize(h2_data_Cbriggsae_MA_P8p_SS_mod),effectiveSize(h2_data_Cbriggsae_Vm_P8p_SS_mod))
        colnames(effectiveSize_h2_data_Cbriggsae_P8p_SS) <- c("effectiveSize")
        h2_data_Cbriggsae_P8p_SS <- cbind.data.frame(mean_h2_data_Cbriggsae_P8p_SS,median_h2_data_Cbriggsae_P8p_SS,posterior.mode_h2_data_Cbriggsae_P8p_SS,HPDinterval_0.95_h2_data_Cbriggsae_P8p_SS,HPDinterval_0.83_h2_data_Cbriggsae_P8p_SS,effectiveSize_h2_data_Cbriggsae_P8p_SS)
        rownames(h2_data_Cbriggsae_P8p_SS) <- c("h2_data_Cbriggsae_CONTROL_P8p_SS_mod","h2_data_Cbriggsae_MA_P8p_SS_mod","h2_data_Cbriggsae_Vm_P8p_SS_mod")
        h2_data_Cbriggsae_P8p_SS <- cbind(Models = rownames(h2_data_Cbriggsae_P8p_SS),h2_data_Cbriggsae_P8p_SS)
        rownames(h2_data_Cbriggsae_P8p_SS) <- NULL
        h2_data_Cbriggsae_P8p_SS$Pnp <- c("P8.p","P8.p","P8.p")
        h2_data_Cbriggsae_P8p_SS$Treatment <- c("Control","MA","Vm")
        h2_data_Cbriggsae_P8p_SS$Measure <- c("H2","H2","H2")
        h2_data_Cbriggsae_P8p_SS$Scale <- c("data","data","data")
        h2_data_Cbriggsae_P8p_SS$Variance <- c("Vm","Vm","Vm")
        h2_data_Cbriggsae_P8p_SS
      }
      
      #Summary Evol_data_Cbriggsae_P8p_SS
      {
        mean_Evol_data_Cbriggsae_P8p_SS <- rbind(mean(Evol_data_Cbriggsae_CONTROL_P8p_SS_mod),mean(Evol_data_Cbriggsae_MA_P8p_SS_mod),mean(Evol_data_Cbriggsae_Vm_P8p_SS_mod))
        colnames(mean_Evol_data_Cbriggsae_P8p_SS) <- c("mean")
        median_Evol_data_Cbriggsae_P8p_SS <- rbind(median(Evol_data_Cbriggsae_CONTROL_P8p_SS_mod),median(Evol_data_Cbriggsae_MA_P8p_SS_mod),median(Evol_data_Cbriggsae_Vm_P8p_SS_mod))
        colnames(median_Evol_data_Cbriggsae_P8p_SS) <- c("median")
        posterior.mode_Evol_data_Cbriggsae_P8p_SS <- rbind(posterior.mode(as.mcmc(Evol_data_Cbriggsae_CONTROL_P8p_SS_mod)),posterior.mode(as.mcmc(Evol_data_Cbriggsae_MA_P8p_SS_mod)),posterior.mode(as.mcmc(Evol_data_Cbriggsae_Vm_P8p_SS_mod)))
        colnames(posterior.mode_Evol_data_Cbriggsae_P8p_SS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Cbriggsae_P8p_SS <- rbind(HPDinterval(as.mcmc(Evol_data_Cbriggsae_CONTROL_P8p_SS_mod)),HPDinterval(as.mcmc(Evol_data_Cbriggsae_MA_P8p_SS_mod)),HPDinterval(as.mcmc(Evol_data_Cbriggsae_Vm_P8p_SS_mod)))
        colnames(HPDinterval_0.95_Evol_data_Cbriggsae_P8p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Cbriggsae_P8p_SS <- rbind(HPDinterval(as.mcmc(Evol_data_Cbriggsae_CONTROL_P8p_SS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Cbriggsae_MA_P8p_SS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Cbriggsae_Vm_P8p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Cbriggsae_P8p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Cbriggsae_P8p_SS <- rbind(effectiveSize(Evol_data_Cbriggsae_CONTROL_P8p_SS_mod),effectiveSize(Evol_data_Cbriggsae_MA_P8p_SS_mod),effectiveSize(Evol_data_Cbriggsae_Vm_P8p_SS_mod))
        colnames(effectiveSize_Evol_data_Cbriggsae_P8p_SS) <- c("effectiveSize")
        Evol_data_Cbriggsae_P8p_SS <- cbind.data.frame(mean_Evol_data_Cbriggsae_P8p_SS,median_Evol_data_Cbriggsae_P8p_SS,posterior.mode_Evol_data_Cbriggsae_P8p_SS,HPDinterval_0.95_Evol_data_Cbriggsae_P8p_SS,HPDinterval_0.83_Evol_data_Cbriggsae_P8p_SS,effectiveSize_Evol_data_Cbriggsae_P8p_SS)
        rownames(Evol_data_Cbriggsae_P8p_SS) <- c("Evol_data_Cbriggsae_CONTROL_P8p_SS_mod","Evol_data_Cbriggsae_MA_P8p_SS_mod","Evol_data_Cbriggsae_Vm_P8p_SS_mod")
        Evol_data_Cbriggsae_P8p_SS <- cbind(Models = rownames(Evol_data_Cbriggsae_P8p_SS),Evol_data_Cbriggsae_P8p_SS)
        rownames(Evol_data_Cbriggsae_P8p_SS) <- NULL
        Evol_data_Cbriggsae_P8p_SS$Pnp <- c("P8.p","P8.p","P8.p")
        Evol_data_Cbriggsae_P8p_SS$Treatment <- c("Control","MA","Vm")
        Evol_data_Cbriggsae_P8p_SS$Measure <- c("Evol","Evol","Evol")
        Evol_data_Cbriggsae_P8p_SS$Scale <- c("data","data","data")
        Evol_data_Cbriggsae_P8p_SS$Variance <- c("Vm","Vm","Vm")
        Evol_data_Cbriggsae_P8p_SS
      }
      
      #Summary trait_mean_data_Cbriggsae_P8p_SS
      {
        mean_trait_mean_data_Cbriggsae_P8p_SS <- rbind(mean(trait_mean_data_Cbriggsae_CONTROL_P8p_SS_mod),mean(trait_mean_data_Cbriggsae_MA_P8p_SS_mod),mean(trait_mean_data_Cbriggsae_Vm_P8p_SS_mod))
        colnames(mean_trait_mean_data_Cbriggsae_P8p_SS) <- c("mean")
        median_trait_mean_data_Cbriggsae_P8p_SS <- rbind(median(trait_mean_data_Cbriggsae_CONTROL_P8p_SS_mod),median(trait_mean_data_Cbriggsae_MA_P8p_SS_mod),median(trait_mean_data_Cbriggsae_Vm_P8p_SS_mod))
        colnames(median_trait_mean_data_Cbriggsae_P8p_SS) <- c("median")
        posterior.mode_trait_mean_data_Cbriggsae_P8p_SS <- rbind(posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P8p_SS_mod)),posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_MA_P8p_SS_mod)),posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_Vm_P8p_SS_mod)))
        colnames(posterior.mode_trait_mean_data_Cbriggsae_P8p_SS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Cbriggsae_P8p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P8p_SS_mod)),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_MA_P8p_SS_mod)),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_Vm_P8p_SS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_Cbriggsae_P8p_SS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Cbriggsae_P8p_SS <- rbind(HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P8p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_MA_P8p_SS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_Vm_P8p_SS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Cbriggsae_P8p_SS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Cbriggsae_P8p_SS <- rbind(effectiveSize(trait_mean_data_Cbriggsae_CONTROL_P8p_SS_mod),effectiveSize(trait_mean_data_Cbriggsae_MA_P8p_SS_mod),effectiveSize(trait_mean_data_Cbriggsae_Vm_P8p_SS_mod))
        colnames(effectiveSize_trait_mean_data_Cbriggsae_P8p_SS) <- c("effectiveSize")
        trait_mean_data_Cbriggsae_P8p_SS <- cbind.data.frame(mean_trait_mean_data_Cbriggsae_P8p_SS,median_trait_mean_data_Cbriggsae_P8p_SS,posterior.mode_trait_mean_data_Cbriggsae_P8p_SS,HPDinterval_0.95_trait_mean_data_Cbriggsae_P8p_SS,HPDinterval_0.83_trait_mean_data_Cbriggsae_P8p_SS,effectiveSize_trait_mean_data_Cbriggsae_P8p_SS)
        rownames(trait_mean_data_Cbriggsae_P8p_SS) <- c("trait_mean_data_Cbriggsae_CONTROL_P8p_SS_mod","trait_mean_data_Cbriggsae_MA_P8p_SS_mod","trait_mean_data_Cbriggsae_Vm_P8p_SS_mod")
        trait_mean_data_Cbriggsae_P8p_SS <- cbind(Models = rownames(trait_mean_data_Cbriggsae_P8p_SS),trait_mean_data_Cbriggsae_P8p_SS)
        rownames(trait_mean_data_Cbriggsae_P8p_SS) <- NULL
        trait_mean_data_Cbriggsae_P8p_SS$Pnp <- c("P8.p","P8.p","P8.p")
        trait_mean_data_Cbriggsae_P8p_SS$Treatment <- c("Control","MA","Vm")
        trait_mean_data_Cbriggsae_P8p_SS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Cbriggsae_P8p_SS$Scale <- c("data","data","data")
        trait_mean_data_Cbriggsae_P8p_SS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_Cbriggsae_P8p_SS
      }
      
      data_Cbriggsae_P8p_SS <- rbind.data.frame(va_data_Cbriggsae_P8p_SS, h2_data_Cbriggsae_P8p_SS,Evol_data_Cbriggsae_P8p_SS,trait_mean_data_Cbriggsae_P8p_SS)
      data_Cbriggsae_P8p_SS
      
    }
    Vm_Cbriggsae_P8p_SS <- rbind.data.frame(liab_Cbriggsae_P8p_SS, data_Cbriggsae_P8p_SS)
    Vm_Cbriggsae_P8p_SS$Pnp_fate <- rep("SS", 24)
    Vm_Cbriggsae_P8p_SS
    #remove Cbriggsae P8p_SS models
    {
      remove(Cbriggsae_CONTROL_P8p_SS_mod)
      remove(Cbriggsae_MA_P8p_SS_mod)
      remove(Cbriggsae_Vm_P8p_SS_mod)
    }
  }
  
  #Summary Cbriggsae P5p
  {
    #Summary liability scale Cbriggsae P5p
    {
      #Summary va_liab_Cbriggsae_P5p_wt: NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_Cbriggsae_P5p_wt <- rbind(mean(va_liab_Cbriggsae_CONTROL_P5p_wt_mod/2),mean(va_liab_Cbriggsae_MA_P5p_wt_mod/2),mean(va_liab_Cbriggsae_Vm_P5p_wt_mod/2))
        colnames(mean_va_liab_Cbriggsae_P5p_wt) <- c("mean")
        median_va_liab_Cbriggsae_P5p_wt <- rbind(median(va_liab_Cbriggsae_CONTROL_P5p_wt_mod/2),median(va_liab_Cbriggsae_MA_P5p_wt_mod/2),median(va_liab_Cbriggsae_Vm_P5p_wt_mod/2))
        colnames(median_va_liab_Cbriggsae_P5p_wt) <- c("median")
        posterior.mode_va_liab_Cbriggsae_P5p_wt <- rbind(posterior.mode(va_liab_Cbriggsae_CONTROL_P5p_wt_mod/2),posterior.mode(va_liab_Cbriggsae_MA_P5p_wt_mod/2),posterior.mode(va_liab_Cbriggsae_Vm_P5p_wt_mod/2))
        colnames(posterior.mode_va_liab_Cbriggsae_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Cbriggsae_P5p_wt <- rbind(HPDinterval(va_liab_Cbriggsae_CONTROL_P5p_wt_mod/2),HPDinterval(va_liab_Cbriggsae_MA_P5p_wt_mod/2),HPDinterval(va_liab_Cbriggsae_Vm_P5p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_Cbriggsae_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Cbriggsae_P5p_wt <- rbind(HPDinterval(va_liab_Cbriggsae_CONTROL_P5p_wt_mod/2,prob=.83),HPDinterval(va_liab_Cbriggsae_MA_P5p_wt_mod/2,prob=.83),HPDinterval(va_liab_Cbriggsae_Vm_P5p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Cbriggsae_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Cbriggsae_P5p_wt <- rbind(effectiveSize(va_liab_Cbriggsae_CONTROL_P5p_wt_mod/2),effectiveSize(va_liab_Cbriggsae_MA_P5p_wt_mod/2),effectiveSize(va_liab_Cbriggsae_Vm_P5p_wt_mod/2))
        colnames(effectiveSize_va_liab_Cbriggsae_P5p_wt) <- c("effectiveSize")
        va_liab_Cbriggsae_P5p_wt <- cbind.data.frame(mean_va_liab_Cbriggsae_P5p_wt,median_va_liab_Cbriggsae_P5p_wt,posterior.mode_va_liab_Cbriggsae_P5p_wt,HPDinterval_0.95_va_liab_Cbriggsae_P5p_wt,HPDinterval_0.83_va_liab_Cbriggsae_P5p_wt,effectiveSize_va_liab_Cbriggsae_P5p_wt)
        rownames(va_liab_Cbriggsae_P5p_wt) <- c("va_liab_Cbriggsae_CONTROL_P5p_wt_mod","va_liab_Cbriggsae_MA_P5p_wt_mod","va_liab_Cbriggsae_Vm_P5p_wt_mod")
        va_liab_Cbriggsae_P5p_wt <- cbind(Models = rownames(va_liab_Cbriggsae_P5p_wt),va_liab_Cbriggsae_P5p_wt)
        rownames(va_liab_Cbriggsae_P5p_wt) <- NULL
        va_liab_Cbriggsae_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        va_liab_Cbriggsae_P5p_wt$Treatment <- c("Control","MA","Vm")
        va_liab_Cbriggsae_P5p_wt$Measure <- c("Va","Va","Va")
        va_liab_Cbriggsae_P5p_wt$Scale <- c("liab","liab","liab")
        va_liab_Cbriggsae_P5p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_Cbriggsae_P5p_wt
      }
      
      #Summary h2_liab_Cbriggsae_P5p_wt
      {
        mean_h2_liab_Cbriggsae_P5p_wt <- rbind(mean(h2_liab_Cbriggsae_CONTROL_P5p_wt_mod),mean(h2_liab_Cbriggsae_MA_P5p_wt_mod),mean(h2_liab_Cbriggsae_Vm_P5p_wt_mod))
        colnames(mean_h2_liab_Cbriggsae_P5p_wt) <- c("mean")
        median_h2_liab_Cbriggsae_P5p_wt <- rbind(median(h2_liab_Cbriggsae_CONTROL_P5p_wt_mod),median(h2_liab_Cbriggsae_MA_P5p_wt_mod),median(h2_liab_Cbriggsae_Vm_P5p_wt_mod))
        colnames(median_h2_liab_Cbriggsae_P5p_wt) <- c("median")
        posterior.mode_h2_liab_Cbriggsae_P5p_wt <- rbind(posterior.mode(h2_liab_Cbriggsae_CONTROL_P5p_wt_mod),posterior.mode(h2_liab_Cbriggsae_MA_P5p_wt_mod),posterior.mode(h2_liab_Cbriggsae_Vm_P5p_wt_mod))
        colnames(posterior.mode_h2_liab_Cbriggsae_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Cbriggsae_P5p_wt <- rbind(HPDinterval(h2_liab_Cbriggsae_CONTROL_P5p_wt_mod),HPDinterval(h2_liab_Cbriggsae_MA_P5p_wt_mod),HPDinterval(h2_liab_Cbriggsae_Vm_P5p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_Cbriggsae_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Cbriggsae_P5p_wt <- rbind(HPDinterval(h2_liab_Cbriggsae_CONTROL_P5p_wt_mod,prob=.83),HPDinterval(h2_liab_Cbriggsae_MA_P5p_wt_mod,prob=.83),HPDinterval(h2_liab_Cbriggsae_Vm_P5p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Cbriggsae_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Cbriggsae_P5p_wt <- rbind(effectiveSize(h2_liab_Cbriggsae_CONTROL_P5p_wt_mod),effectiveSize(h2_liab_Cbriggsae_MA_P5p_wt_mod),effectiveSize(h2_liab_Cbriggsae_Vm_P5p_wt_mod))
        colnames(effectiveSize_h2_liab_Cbriggsae_P5p_wt) <- c("effectiveSize")
        h2_liab_Cbriggsae_P5p_wt <- cbind.data.frame(mean_h2_liab_Cbriggsae_P5p_wt,median_h2_liab_Cbriggsae_P5p_wt,posterior.mode_h2_liab_Cbriggsae_P5p_wt,HPDinterval_0.95_h2_liab_Cbriggsae_P5p_wt,HPDinterval_0.83_h2_liab_Cbriggsae_P5p_wt,effectiveSize_h2_liab_Cbriggsae_P5p_wt)
        rownames(h2_liab_Cbriggsae_P5p_wt) <- c("h2_liab_Cbriggsae_CONTROL_P5p_wt_mod","h2_liab_Cbriggsae_MA_P5p_wt_mod","h2_liab_Cbriggsae_Vm_P5p_wt_mod")
        h2_liab_Cbriggsae_P5p_wt <- cbind(Models = rownames(h2_liab_Cbriggsae_P5p_wt),h2_liab_Cbriggsae_P5p_wt)
        rownames(h2_liab_Cbriggsae_P5p_wt) <- NULL
        h2_liab_Cbriggsae_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        h2_liab_Cbriggsae_P5p_wt$Treatment <- c("Control","MA","Vm")
        h2_liab_Cbriggsae_P5p_wt$Measure <- c("H2","H2","H2")
        h2_liab_Cbriggsae_P5p_wt$Scale <- c("liab","liab","liab")
        h2_liab_Cbriggsae_P5p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_Cbriggsae_P5p_wt
      }
      
      #Summary Evol_liab_Cbriggsae_P5p_wt
      {
        mean_Evol_liab_Cbriggsae_P5p_wt <- rbind(mean(Evol_liab_Cbriggsae_CONTROL_P5p_wt_mod),mean(Evol_liab_Cbriggsae_MA_P5p_wt_mod),mean(Evol_liab_Cbriggsae_Vm_P5p_wt_mod))
        colnames(mean_Evol_liab_Cbriggsae_P5p_wt) <- c("mean")
        median_Evol_liab_Cbriggsae_P5p_wt <- rbind(median(Evol_liab_Cbriggsae_CONTROL_P5p_wt_mod),median(Evol_liab_Cbriggsae_MA_P5p_wt_mod),median(Evol_liab_Cbriggsae_Vm_P5p_wt_mod))
        colnames(median_Evol_liab_Cbriggsae_P5p_wt) <- c("median")
        posterior.mode_Evol_liab_Cbriggsae_P5p_wt <- rbind(posterior.mode(Evol_liab_Cbriggsae_CONTROL_P5p_wt_mod),posterior.mode(Evol_liab_Cbriggsae_MA_P5p_wt_mod),posterior.mode(Evol_liab_Cbriggsae_Vm_P5p_wt_mod))
        colnames(posterior.mode_Evol_liab_Cbriggsae_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Cbriggsae_P5p_wt <- rbind(HPDinterval(Evol_liab_Cbriggsae_CONTROL_P5p_wt_mod),HPDinterval(Evol_liab_Cbriggsae_MA_P5p_wt_mod),HPDinterval(Evol_liab_Cbriggsae_Vm_P5p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_Cbriggsae_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Cbriggsae_P5p_wt <- rbind(HPDinterval(Evol_liab_Cbriggsae_CONTROL_P5p_wt_mod,prob=.83),HPDinterval(Evol_liab_Cbriggsae_MA_P5p_wt_mod,prob=.83),HPDinterval(Evol_liab_Cbriggsae_Vm_P5p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Cbriggsae_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Cbriggsae_P5p_wt <- rbind(effectiveSize(Evol_liab_Cbriggsae_CONTROL_P5p_wt_mod),effectiveSize(Evol_liab_Cbriggsae_MA_P5p_wt_mod),effectiveSize(Evol_liab_Cbriggsae_Vm_P5p_wt_mod))
        colnames(effectiveSize_Evol_liab_Cbriggsae_P5p_wt) <- c("effectiveSize")
        Evol_liab_Cbriggsae_P5p_wt <- cbind.data.frame(mean_Evol_liab_Cbriggsae_P5p_wt,median_Evol_liab_Cbriggsae_P5p_wt,posterior.mode_Evol_liab_Cbriggsae_P5p_wt,HPDinterval_0.95_Evol_liab_Cbriggsae_P5p_wt,HPDinterval_0.83_Evol_liab_Cbriggsae_P5p_wt,effectiveSize_Evol_liab_Cbriggsae_P5p_wt)
        rownames(Evol_liab_Cbriggsae_P5p_wt) <- c("Evol_liab_Cbriggsae_CONTROL_P5p_wt_mod","Evol_liab_Cbriggsae_MA_P5p_wt_mod","Evol_liab_Cbriggsae_Vm_P5p_wt_mod")
        Evol_liab_Cbriggsae_P5p_wt <- cbind(Models = rownames(Evol_liab_Cbriggsae_P5p_wt),Evol_liab_Cbriggsae_P5p_wt)
        rownames(Evol_liab_Cbriggsae_P5p_wt) <- NULL
        Evol_liab_Cbriggsae_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        Evol_liab_Cbriggsae_P5p_wt$Treatment <- c("Control","MA","Vm")
        Evol_liab_Cbriggsae_P5p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Cbriggsae_P5p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_Cbriggsae_P5p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_Cbriggsae_P5p_wt
      }
      
      #Summary trait_mean_liab_Cbriggsae_P5p_wt
      {
        mean_trait_mean_liab_Cbriggsae_P5p_wt <- rbind(mean(trait_mean_liab_Cbriggsae_CONTROL_P5p_wt_mod),mean(trait_mean_liab_Cbriggsae_MA_P5p_wt_mod),mean(trait_mean_liab_Cbriggsae_Vm_P5p_wt_mod))
        colnames(mean_trait_mean_liab_Cbriggsae_P5p_wt) <- c("mean")
        median_trait_mean_liab_Cbriggsae_P5p_wt <- rbind(median(trait_mean_liab_Cbriggsae_CONTROL_P5p_wt_mod),median(trait_mean_liab_Cbriggsae_MA_P5p_wt_mod),median(trait_mean_liab_Cbriggsae_Vm_P5p_wt_mod))
        colnames(median_trait_mean_liab_Cbriggsae_P5p_wt) <- c("median")
        posterior.mode_trait_mean_liab_Cbriggsae_P5p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_MA_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P5p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_Cbriggsae_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Cbriggsae_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_MA_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_Cbriggsae_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Cbriggsae_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Cbriggsae_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Cbriggsae_P5p_wt <- rbind(effectiveSize(trait_mean_liab_Cbriggsae_CONTROL_P5p_wt_mod),effectiveSize(trait_mean_liab_Cbriggsae_MA_P5p_wt_mod),effectiveSize(trait_mean_liab_Cbriggsae_Vm_P5p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_Cbriggsae_P5p_wt) <- c("effectiveSize")
        trait_mean_liab_Cbriggsae_P5p_wt <- cbind.data.frame(mean_trait_mean_liab_Cbriggsae_P5p_wt,median_trait_mean_liab_Cbriggsae_P5p_wt,posterior.mode_trait_mean_liab_Cbriggsae_P5p_wt,HPDinterval_0.95_trait_mean_liab_Cbriggsae_P5p_wt,HPDinterval_0.83_trait_mean_liab_Cbriggsae_P5p_wt,effectiveSize_trait_mean_liab_Cbriggsae_P5p_wt)
        rownames(trait_mean_liab_Cbriggsae_P5p_wt) <- c("trait_mean_liab_Cbriggsae_CONTROL_P5p_wt_mod","trait_mean_liab_Cbriggsae_MA_P5p_wt_mod","trait_mean_liab_Cbriggsae_Vm_P5p_wt_mod")
        trait_mean_liab_Cbriggsae_P5p_wt <- cbind(Models = rownames(trait_mean_liab_Cbriggsae_P5p_wt),trait_mean_liab_Cbriggsae_P5p_wt)
        rownames(trait_mean_liab_Cbriggsae_P5p_wt) <- NULL
        trait_mean_liab_Cbriggsae_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        trait_mean_liab_Cbriggsae_P5p_wt$Treatment <- c("Control","MA","Vm")
        trait_mean_liab_Cbriggsae_P5p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Cbriggsae_P5p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_Cbriggsae_P5p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_Cbriggsae_P5p_wt
      }
      
      liab_Cbriggsae_P5p_wt <- rbind.data.frame(va_liab_Cbriggsae_P5p_wt, h2_liab_Cbriggsae_P5p_wt,Evol_liab_Cbriggsae_P5p_wt,trait_mean_liab_Cbriggsae_P5p_wt)
      liab_Cbriggsae_P5p_wt
    }
    #Summary data scale Cbriggsae P5p
    {
      #Summary va_data_Cbriggsae_P5p_wt:NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_Cbriggsae_P5p_wt <- rbind(mean(va_data_Cbriggsae_CONTROL_P5p_wt_mod/2),mean(va_data_Cbriggsae_MA_P5p_wt_mod/2),mean(va_data_Cbriggsae_Vm_P5p_wt_mod/2))
        colnames(mean_va_data_Cbriggsae_P5p_wt) <- c("mean")
        median_va_data_Cbriggsae_P5p_wt <- rbind(median(va_data_Cbriggsae_CONTROL_P5p_wt_mod/2),median(va_data_Cbriggsae_MA_P5p_wt_mod/2),median(va_data_Cbriggsae_Vm_P5p_wt_mod/2))
        colnames(median_va_data_Cbriggsae_P5p_wt) <- c("median")
        posterior.mode_va_data_Cbriggsae_P5p_wt <- rbind(posterior.mode(as.mcmc(va_data_Cbriggsae_CONTROL_P5p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Cbriggsae_MA_P5p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Cbriggsae_Vm_P5p_wt_mod/2)))
        colnames(posterior.mode_va_data_Cbriggsae_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Cbriggsae_P5p_wt <- rbind(HPDinterval(as.mcmc(va_data_Cbriggsae_CONTROL_P5p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Cbriggsae_MA_P5p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Cbriggsae_Vm_P5p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_Cbriggsae_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Cbriggsae_P5p_wt <- rbind(HPDinterval(as.mcmc(va_data_Cbriggsae_CONTROL_P5p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Cbriggsae_MA_P5p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Cbriggsae_Vm_P5p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Cbriggsae_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Cbriggsae_P5p_wt <- rbind(effectiveSize(va_data_Cbriggsae_CONTROL_P5p_wt_mod/2),effectiveSize(va_data_Cbriggsae_MA_P5p_wt_mod/2),effectiveSize(va_data_Cbriggsae_Vm_P5p_wt_mod/2))
        colnames(effectiveSize_va_data_Cbriggsae_P5p_wt) <- c("effectiveSize")
        va_data_Cbriggsae_P5p_wt <- cbind.data.frame(mean_va_data_Cbriggsae_P5p_wt,median_va_data_Cbriggsae_P5p_wt,posterior.mode_va_data_Cbriggsae_P5p_wt,HPDinterval_0.95_va_data_Cbriggsae_P5p_wt,HPDinterval_0.83_va_data_Cbriggsae_P5p_wt,effectiveSize_va_data_Cbriggsae_P5p_wt)
        rownames(va_data_Cbriggsae_P5p_wt) <- c("va_data_Cbriggsae_CONTROL_P5p_wt_mod","va_data_Cbriggsae_MA_P5p_wt_mod","va_data_Cbriggsae_Vm_P5p_wt_mod")
        va_data_Cbriggsae_P5p_wt <- cbind(Models = rownames(va_data_Cbriggsae_P5p_wt),va_data_Cbriggsae_P5p_wt)
        rownames(va_data_Cbriggsae_P5p_wt) <- NULL
        va_data_Cbriggsae_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        va_data_Cbriggsae_P5p_wt$Treatment <- c("Control","MA","Vm")
        va_data_Cbriggsae_P5p_wt$Measure <- c("Va","Va","Va")
        va_data_Cbriggsae_P5p_wt$Scale <- c("data","data","data")
        va_data_Cbriggsae_P5p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_Cbriggsae_P5p_wt
      }
      
      #Summary h2_data_Cbriggsae_P5p_wt
      {
        mean_h2_data_Cbriggsae_P5p_wt <- rbind(mean(h2_data_Cbriggsae_CONTROL_P5p_wt_mod),mean(h2_data_Cbriggsae_MA_P5p_wt_mod),mean(h2_data_Cbriggsae_Vm_P5p_wt_mod))
        colnames(mean_h2_data_Cbriggsae_P5p_wt) <- c("mean")
        median_h2_data_Cbriggsae_P5p_wt <- rbind(median(h2_data_Cbriggsae_CONTROL_P5p_wt_mod),median(h2_data_Cbriggsae_MA_P5p_wt_mod),median(h2_data_Cbriggsae_Vm_P5p_wt_mod))
        colnames(median_h2_data_Cbriggsae_P5p_wt) <- c("median")
        posterior.mode_h2_data_Cbriggsae_P5p_wt <- rbind(posterior.mode(as.mcmc(h2_data_Cbriggsae_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(h2_data_Cbriggsae_MA_P5p_wt_mod)),posterior.mode(as.mcmc(h2_data_Cbriggsae_Vm_P5p_wt_mod)))
        colnames(posterior.mode_h2_data_Cbriggsae_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Cbriggsae_P5p_wt <- rbind(HPDinterval(as.mcmc(h2_data_Cbriggsae_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(h2_data_Cbriggsae_MA_P5p_wt_mod)),HPDinterval(as.mcmc(h2_data_Cbriggsae_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_Cbriggsae_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Cbriggsae_P5p_wt <- rbind(HPDinterval(as.mcmc(h2_data_Cbriggsae_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Cbriggsae_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Cbriggsae_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Cbriggsae_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Cbriggsae_P5p_wt <- rbind(effectiveSize(h2_data_Cbriggsae_CONTROL_P5p_wt_mod),effectiveSize(h2_data_Cbriggsae_MA_P5p_wt_mod),effectiveSize(h2_data_Cbriggsae_Vm_P5p_wt_mod))
        colnames(effectiveSize_h2_data_Cbriggsae_P5p_wt) <- c("effectiveSize")
        h2_data_Cbriggsae_P5p_wt <- cbind.data.frame(mean_h2_data_Cbriggsae_P5p_wt,median_h2_data_Cbriggsae_P5p_wt,posterior.mode_h2_data_Cbriggsae_P5p_wt,HPDinterval_0.95_h2_data_Cbriggsae_P5p_wt,HPDinterval_0.83_h2_data_Cbriggsae_P5p_wt,effectiveSize_h2_data_Cbriggsae_P5p_wt)
        rownames(h2_data_Cbriggsae_P5p_wt) <- c("h2_data_Cbriggsae_CONTROL_P5p_wt_mod","h2_data_Cbriggsae_MA_P5p_wt_mod","h2_data_Cbriggsae_Vm_P5p_wt_mod")
        h2_data_Cbriggsae_P5p_wt <- cbind(Models = rownames(h2_data_Cbriggsae_P5p_wt),h2_data_Cbriggsae_P5p_wt)
        rownames(h2_data_Cbriggsae_P5p_wt) <- NULL
        h2_data_Cbriggsae_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        h2_data_Cbriggsae_P5p_wt$Treatment <- c("Control","MA","Vm")
        h2_data_Cbriggsae_P5p_wt$Measure <- c("H2","H2","H2")
        h2_data_Cbriggsae_P5p_wt$Scale <- c("data","data","data")
        h2_data_Cbriggsae_P5p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_Cbriggsae_P5p_wt
      }
      
      #Summary Evol_data_Cbriggsae_P5p_wt
      {
        mean_Evol_data_Cbriggsae_P5p_wt <- rbind(mean(Evol_data_Cbriggsae_CONTROL_P5p_wt_mod),mean(Evol_data_Cbriggsae_MA_P5p_wt_mod),mean(Evol_data_Cbriggsae_Vm_P5p_wt_mod))
        colnames(mean_Evol_data_Cbriggsae_P5p_wt) <- c("mean")
        median_Evol_data_Cbriggsae_P5p_wt <- rbind(median(Evol_data_Cbriggsae_CONTROL_P5p_wt_mod),median(Evol_data_Cbriggsae_MA_P5p_wt_mod),median(Evol_data_Cbriggsae_Vm_P5p_wt_mod))
        colnames(median_Evol_data_Cbriggsae_P5p_wt) <- c("median")
        posterior.mode_Evol_data_Cbriggsae_P5p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_Cbriggsae_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Cbriggsae_MA_P5p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Cbriggsae_Vm_P5p_wt_mod)))
        colnames(posterior.mode_Evol_data_Cbriggsae_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Cbriggsae_P5p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_Cbriggsae_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Cbriggsae_MA_P5p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Cbriggsae_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_Cbriggsae_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Cbriggsae_P5p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_Cbriggsae_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Cbriggsae_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Cbriggsae_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Cbriggsae_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Cbriggsae_P5p_wt <- rbind(effectiveSize(Evol_data_Cbriggsae_CONTROL_P5p_wt_mod),effectiveSize(Evol_data_Cbriggsae_MA_P5p_wt_mod),effectiveSize(Evol_data_Cbriggsae_Vm_P5p_wt_mod))
        colnames(effectiveSize_Evol_data_Cbriggsae_P5p_wt) <- c("effectiveSize")
        Evol_data_Cbriggsae_P5p_wt <- cbind.data.frame(mean_Evol_data_Cbriggsae_P5p_wt,median_Evol_data_Cbriggsae_P5p_wt,posterior.mode_Evol_data_Cbriggsae_P5p_wt,HPDinterval_0.95_Evol_data_Cbriggsae_P5p_wt,HPDinterval_0.83_Evol_data_Cbriggsae_P5p_wt,effectiveSize_Evol_data_Cbriggsae_P5p_wt)
        rownames(Evol_data_Cbriggsae_P5p_wt) <- c("Evol_data_Cbriggsae_CONTROL_P5p_wt_mod","Evol_data_Cbriggsae_MA_P5p_wt_mod","Evol_data_Cbriggsae_Vm_P5p_wt_mod")
        Evol_data_Cbriggsae_P5p_wt <- cbind(Models = rownames(Evol_data_Cbriggsae_P5p_wt),Evol_data_Cbriggsae_P5p_wt)
        rownames(Evol_data_Cbriggsae_P5p_wt) <- NULL
        Evol_data_Cbriggsae_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        Evol_data_Cbriggsae_P5p_wt$Treatment <- c("Control","MA","Vm")
        Evol_data_Cbriggsae_P5p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_Cbriggsae_P5p_wt$Scale <- c("data","data","data")
        Evol_data_Cbriggsae_P5p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_Cbriggsae_P5p_wt
      }
      
      #Summary trait_mean_data_Cbriggsae_P5p_wt
      {
        mean_trait_mean_data_Cbriggsae_P5p_wt <- rbind(mean(trait_mean_data_Cbriggsae_CONTROL_P5p_wt_mod),mean(trait_mean_data_Cbriggsae_MA_P5p_wt_mod),mean(trait_mean_data_Cbriggsae_Vm_P5p_wt_mod))
        colnames(mean_trait_mean_data_Cbriggsae_P5p_wt) <- c("mean")
        median_trait_mean_data_Cbriggsae_P5p_wt <- rbind(median(trait_mean_data_Cbriggsae_CONTROL_P5p_wt_mod),median(trait_mean_data_Cbriggsae_MA_P5p_wt_mod),median(trait_mean_data_Cbriggsae_Vm_P5p_wt_mod))
        colnames(median_trait_mean_data_Cbriggsae_P5p_wt) <- c("median")
        posterior.mode_trait_mean_data_Cbriggsae_P5p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_MA_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_Vm_P5p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_Cbriggsae_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Cbriggsae_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_MA_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_Cbriggsae_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Cbriggsae_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Cbriggsae_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Cbriggsae_P5p_wt <- rbind(effectiveSize(trait_mean_data_Cbriggsae_CONTROL_P5p_wt_mod),effectiveSize(trait_mean_data_Cbriggsae_MA_P5p_wt_mod),effectiveSize(trait_mean_data_Cbriggsae_Vm_P5p_wt_mod))
        colnames(effectiveSize_trait_mean_data_Cbriggsae_P5p_wt) <- c("effectiveSize")
        trait_mean_data_Cbriggsae_P5p_wt <- cbind.data.frame(mean_trait_mean_data_Cbriggsae_P5p_wt,median_trait_mean_data_Cbriggsae_P5p_wt,posterior.mode_trait_mean_data_Cbriggsae_P5p_wt,HPDinterval_0.95_trait_mean_data_Cbriggsae_P5p_wt,HPDinterval_0.83_trait_mean_data_Cbriggsae_P5p_wt,effectiveSize_trait_mean_data_Cbriggsae_P5p_wt)
        rownames(trait_mean_data_Cbriggsae_P5p_wt) <- c("trait_mean_data_Cbriggsae_CONTROL_P5p_wt_mod","trait_mean_data_Cbriggsae_MA_P5p_wt_mod","trait_mean_data_Cbriggsae_Vm_P5p_wt_mod")
        trait_mean_data_Cbriggsae_P5p_wt <- cbind(Models = rownames(trait_mean_data_Cbriggsae_P5p_wt),trait_mean_data_Cbriggsae_P5p_wt)
        rownames(trait_mean_data_Cbriggsae_P5p_wt) <- NULL
        trait_mean_data_Cbriggsae_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        trait_mean_data_Cbriggsae_P5p_wt$Treatment <- c("Control","MA","Vm")
        trait_mean_data_Cbriggsae_P5p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Cbriggsae_P5p_wt$Scale <- c("data","data","data")
        trait_mean_data_Cbriggsae_P5p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_Cbriggsae_P5p_wt
      }
      
      data_Cbriggsae_P5p_wt <- rbind.data.frame(va_data_Cbriggsae_P5p_wt, h2_data_Cbriggsae_P5p_wt,Evol_data_Cbriggsae_P5p_wt,trait_mean_data_Cbriggsae_P5p_wt)
      data_Cbriggsae_P5p_wt
      
    }
    Vm_Cbriggsae_P5p_wt <- rbind.data.frame(liab_Cbriggsae_P5p_wt, data_Cbriggsae_P5p_wt)
    Vm_Cbriggsae_P5p_wt$Pnp_fate <- rep("wt", 24)
    Vm_Cbriggsae_P5p_wt
    #remove Cbriggsae P5p_wt models
    {
      remove(Cbriggsae_CONTROL_P5p_wt_mod)
      remove(Cbriggsae_MA_P5p_wt_mod)
      remove(Cbriggsae_Vm_P5p_wt_mod)
    }
  }
  
  #Summary Cbriggsae P6p
  {
    #Summary liability scale Cbriggsae P6p
    {
      #Summary va_liab_Cbriggsae_P6p_wt: NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_Cbriggsae_P6p_wt <- rbind(mean(va_liab_Cbriggsae_CONTROL_P6p_wt_mod/2),mean(va_liab_Cbriggsae_MA_P6p_wt_mod/2),mean(va_liab_Cbriggsae_Vm_P6p_wt_mod/2))
        colnames(mean_va_liab_Cbriggsae_P6p_wt) <- c("mean")
        median_va_liab_Cbriggsae_P6p_wt <- rbind(median(va_liab_Cbriggsae_CONTROL_P6p_wt_mod/2),median(va_liab_Cbriggsae_MA_P6p_wt_mod/2),median(va_liab_Cbriggsae_Vm_P6p_wt_mod/2))
        colnames(median_va_liab_Cbriggsae_P6p_wt) <- c("median")
        posterior.mode_va_liab_Cbriggsae_P6p_wt <- rbind(posterior.mode(va_liab_Cbriggsae_CONTROL_P6p_wt_mod/2),posterior.mode(va_liab_Cbriggsae_MA_P6p_wt_mod/2),posterior.mode(va_liab_Cbriggsae_Vm_P6p_wt_mod/2))
        colnames(posterior.mode_va_liab_Cbriggsae_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Cbriggsae_P6p_wt <- rbind(HPDinterval(va_liab_Cbriggsae_CONTROL_P6p_wt_mod/2),HPDinterval(va_liab_Cbriggsae_MA_P6p_wt_mod/2),HPDinterval(va_liab_Cbriggsae_Vm_P6p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_Cbriggsae_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Cbriggsae_P6p_wt <- rbind(HPDinterval(va_liab_Cbriggsae_CONTROL_P6p_wt_mod/2,prob=.83),HPDinterval(va_liab_Cbriggsae_MA_P6p_wt_mod/2,prob=.83),HPDinterval(va_liab_Cbriggsae_Vm_P6p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Cbriggsae_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Cbriggsae_P6p_wt <- rbind(effectiveSize(va_liab_Cbriggsae_CONTROL_P6p_wt_mod/2),effectiveSize(va_liab_Cbriggsae_MA_P6p_wt_mod/2),effectiveSize(va_liab_Cbriggsae_Vm_P6p_wt_mod/2))
        colnames(effectiveSize_va_liab_Cbriggsae_P6p_wt) <- c("effectiveSize")
        va_liab_Cbriggsae_P6p_wt <- cbind.data.frame(mean_va_liab_Cbriggsae_P6p_wt,median_va_liab_Cbriggsae_P6p_wt,posterior.mode_va_liab_Cbriggsae_P6p_wt,HPDinterval_0.95_va_liab_Cbriggsae_P6p_wt,HPDinterval_0.83_va_liab_Cbriggsae_P6p_wt,effectiveSize_va_liab_Cbriggsae_P6p_wt)
        rownames(va_liab_Cbriggsae_P6p_wt) <- c("va_liab_Cbriggsae_CONTROL_P6p_wt_mod","va_liab_Cbriggsae_MA_P6p_wt_mod","va_liab_Cbriggsae_Vm_P6p_wt_mod")
        va_liab_Cbriggsae_P6p_wt <- cbind(Models = rownames(va_liab_Cbriggsae_P6p_wt),va_liab_Cbriggsae_P6p_wt)
        rownames(va_liab_Cbriggsae_P6p_wt) <- NULL
        va_liab_Cbriggsae_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        va_liab_Cbriggsae_P6p_wt$Treatment <- c("Control","MA","Vm")
        va_liab_Cbriggsae_P6p_wt$Measure <- c("Va","Va","Va")
        va_liab_Cbriggsae_P6p_wt$Scale <- c("liab","liab","liab")
        va_liab_Cbriggsae_P6p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_Cbriggsae_P6p_wt
      }
      
      #Summary h2_liab_Cbriggsae_P6p_wt
      {
        mean_h2_liab_Cbriggsae_P6p_wt <- rbind(mean(h2_liab_Cbriggsae_CONTROL_P6p_wt_mod),mean(h2_liab_Cbriggsae_MA_P6p_wt_mod),mean(h2_liab_Cbriggsae_Vm_P6p_wt_mod))
        colnames(mean_h2_liab_Cbriggsae_P6p_wt) <- c("mean")
        median_h2_liab_Cbriggsae_P6p_wt <- rbind(median(h2_liab_Cbriggsae_CONTROL_P6p_wt_mod),median(h2_liab_Cbriggsae_MA_P6p_wt_mod),median(h2_liab_Cbriggsae_Vm_P6p_wt_mod))
        colnames(median_h2_liab_Cbriggsae_P6p_wt) <- c("median")
        posterior.mode_h2_liab_Cbriggsae_P6p_wt <- rbind(posterior.mode(h2_liab_Cbriggsae_CONTROL_P6p_wt_mod),posterior.mode(h2_liab_Cbriggsae_MA_P6p_wt_mod),posterior.mode(h2_liab_Cbriggsae_Vm_P6p_wt_mod))
        colnames(posterior.mode_h2_liab_Cbriggsae_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Cbriggsae_P6p_wt <- rbind(HPDinterval(h2_liab_Cbriggsae_CONTROL_P6p_wt_mod),HPDinterval(h2_liab_Cbriggsae_MA_P6p_wt_mod),HPDinterval(h2_liab_Cbriggsae_Vm_P6p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_Cbriggsae_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Cbriggsae_P6p_wt <- rbind(HPDinterval(h2_liab_Cbriggsae_CONTROL_P6p_wt_mod,prob=.83),HPDinterval(h2_liab_Cbriggsae_MA_P6p_wt_mod,prob=.83),HPDinterval(h2_liab_Cbriggsae_Vm_P6p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Cbriggsae_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Cbriggsae_P6p_wt <- rbind(effectiveSize(h2_liab_Cbriggsae_CONTROL_P6p_wt_mod),effectiveSize(h2_liab_Cbriggsae_MA_P6p_wt_mod),effectiveSize(h2_liab_Cbriggsae_Vm_P6p_wt_mod))
        colnames(effectiveSize_h2_liab_Cbriggsae_P6p_wt) <- c("effectiveSize")
        h2_liab_Cbriggsae_P6p_wt <- cbind.data.frame(mean_h2_liab_Cbriggsae_P6p_wt,median_h2_liab_Cbriggsae_P6p_wt,posterior.mode_h2_liab_Cbriggsae_P6p_wt,HPDinterval_0.95_h2_liab_Cbriggsae_P6p_wt,HPDinterval_0.83_h2_liab_Cbriggsae_P6p_wt,effectiveSize_h2_liab_Cbriggsae_P6p_wt)
        rownames(h2_liab_Cbriggsae_P6p_wt) <- c("h2_liab_Cbriggsae_CONTROL_P6p_wt_mod","h2_liab_Cbriggsae_MA_P6p_wt_mod","h2_liab_Cbriggsae_Vm_P6p_wt_mod")
        h2_liab_Cbriggsae_P6p_wt <- cbind(Models = rownames(h2_liab_Cbriggsae_P6p_wt),h2_liab_Cbriggsae_P6p_wt)
        rownames(h2_liab_Cbriggsae_P6p_wt) <- NULL
        h2_liab_Cbriggsae_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        h2_liab_Cbriggsae_P6p_wt$Treatment <- c("Control","MA","Vm")
        h2_liab_Cbriggsae_P6p_wt$Measure <- c("H2","H2","H2")
        h2_liab_Cbriggsae_P6p_wt$Scale <- c("liab","liab","liab")
        h2_liab_Cbriggsae_P6p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_Cbriggsae_P6p_wt
      }
      
      #Summary Evol_liab_Cbriggsae_P6p_wt
      {
        mean_Evol_liab_Cbriggsae_P6p_wt <- rbind(mean(Evol_liab_Cbriggsae_CONTROL_P6p_wt_mod),mean(Evol_liab_Cbriggsae_MA_P6p_wt_mod),mean(Evol_liab_Cbriggsae_Vm_P6p_wt_mod))
        colnames(mean_Evol_liab_Cbriggsae_P6p_wt) <- c("mean")
        median_Evol_liab_Cbriggsae_P6p_wt <- rbind(median(Evol_liab_Cbriggsae_CONTROL_P6p_wt_mod),median(Evol_liab_Cbriggsae_MA_P6p_wt_mod),median(Evol_liab_Cbriggsae_Vm_P6p_wt_mod))
        colnames(median_Evol_liab_Cbriggsae_P6p_wt) <- c("median")
        posterior.mode_Evol_liab_Cbriggsae_P6p_wt <- rbind(posterior.mode(Evol_liab_Cbriggsae_CONTROL_P6p_wt_mod),posterior.mode(Evol_liab_Cbriggsae_MA_P6p_wt_mod),posterior.mode(Evol_liab_Cbriggsae_Vm_P6p_wt_mod))
        colnames(posterior.mode_Evol_liab_Cbriggsae_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Cbriggsae_P6p_wt <- rbind(HPDinterval(Evol_liab_Cbriggsae_CONTROL_P6p_wt_mod),HPDinterval(Evol_liab_Cbriggsae_MA_P6p_wt_mod),HPDinterval(Evol_liab_Cbriggsae_Vm_P6p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_Cbriggsae_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Cbriggsae_P6p_wt <- rbind(HPDinterval(Evol_liab_Cbriggsae_CONTROL_P6p_wt_mod,prob=.83),HPDinterval(Evol_liab_Cbriggsae_MA_P6p_wt_mod,prob=.83),HPDinterval(Evol_liab_Cbriggsae_Vm_P6p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Cbriggsae_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Cbriggsae_P6p_wt <- rbind(effectiveSize(Evol_liab_Cbriggsae_CONTROL_P6p_wt_mod),effectiveSize(Evol_liab_Cbriggsae_MA_P6p_wt_mod),effectiveSize(Evol_liab_Cbriggsae_Vm_P6p_wt_mod))
        colnames(effectiveSize_Evol_liab_Cbriggsae_P6p_wt) <- c("effectiveSize")
        Evol_liab_Cbriggsae_P6p_wt <- cbind.data.frame(mean_Evol_liab_Cbriggsae_P6p_wt,median_Evol_liab_Cbriggsae_P6p_wt,posterior.mode_Evol_liab_Cbriggsae_P6p_wt,HPDinterval_0.95_Evol_liab_Cbriggsae_P6p_wt,HPDinterval_0.83_Evol_liab_Cbriggsae_P6p_wt,effectiveSize_Evol_liab_Cbriggsae_P6p_wt)
        rownames(Evol_liab_Cbriggsae_P6p_wt) <- c("Evol_liab_Cbriggsae_CONTROL_P6p_wt_mod","Evol_liab_Cbriggsae_MA_P6p_wt_mod","Evol_liab_Cbriggsae_Vm_P6p_wt_mod")
        Evol_liab_Cbriggsae_P6p_wt <- cbind(Models = rownames(Evol_liab_Cbriggsae_P6p_wt),Evol_liab_Cbriggsae_P6p_wt)
        rownames(Evol_liab_Cbriggsae_P6p_wt) <- NULL
        Evol_liab_Cbriggsae_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        Evol_liab_Cbriggsae_P6p_wt$Treatment <- c("Control","MA","Vm")
        Evol_liab_Cbriggsae_P6p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Cbriggsae_P6p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_Cbriggsae_P6p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_Cbriggsae_P6p_wt
      }
      
      #Summary trait_mean_liab_Cbriggsae_P6p_wt
      {
        mean_trait_mean_liab_Cbriggsae_P6p_wt <- rbind(mean(trait_mean_liab_Cbriggsae_CONTROL_P6p_wt_mod),mean(trait_mean_liab_Cbriggsae_MA_P6p_wt_mod),mean(trait_mean_liab_Cbriggsae_Vm_P6p_wt_mod))
        colnames(mean_trait_mean_liab_Cbriggsae_P6p_wt) <- c("mean")
        median_trait_mean_liab_Cbriggsae_P6p_wt <- rbind(median(trait_mean_liab_Cbriggsae_CONTROL_P6p_wt_mod),median(trait_mean_liab_Cbriggsae_MA_P6p_wt_mod),median(trait_mean_liab_Cbriggsae_Vm_P6p_wt_mod))
        colnames(median_trait_mean_liab_Cbriggsae_P6p_wt) <- c("median")
        posterior.mode_trait_mean_liab_Cbriggsae_P6p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_MA_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P6p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_Cbriggsae_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Cbriggsae_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_MA_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_Cbriggsae_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Cbriggsae_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Cbriggsae_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Cbriggsae_P6p_wt <- rbind(effectiveSize(trait_mean_liab_Cbriggsae_CONTROL_P6p_wt_mod),effectiveSize(trait_mean_liab_Cbriggsae_MA_P6p_wt_mod),effectiveSize(trait_mean_liab_Cbriggsae_Vm_P6p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_Cbriggsae_P6p_wt) <- c("effectiveSize")
        trait_mean_liab_Cbriggsae_P6p_wt <- cbind.data.frame(mean_trait_mean_liab_Cbriggsae_P6p_wt,median_trait_mean_liab_Cbriggsae_P6p_wt,posterior.mode_trait_mean_liab_Cbriggsae_P6p_wt,HPDinterval_0.95_trait_mean_liab_Cbriggsae_P6p_wt,HPDinterval_0.83_trait_mean_liab_Cbriggsae_P6p_wt,effectiveSize_trait_mean_liab_Cbriggsae_P6p_wt)
        rownames(trait_mean_liab_Cbriggsae_P6p_wt) <- c("trait_mean_liab_Cbriggsae_CONTROL_P6p_wt_mod","trait_mean_liab_Cbriggsae_MA_P6p_wt_mod","trait_mean_liab_Cbriggsae_Vm_P6p_wt_mod")
        trait_mean_liab_Cbriggsae_P6p_wt <- cbind(Models = rownames(trait_mean_liab_Cbriggsae_P6p_wt),trait_mean_liab_Cbriggsae_P6p_wt)
        rownames(trait_mean_liab_Cbriggsae_P6p_wt) <- NULL
        trait_mean_liab_Cbriggsae_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        trait_mean_liab_Cbriggsae_P6p_wt$Treatment <- c("Control","MA","Vm")
        trait_mean_liab_Cbriggsae_P6p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Cbriggsae_P6p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_Cbriggsae_P6p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_Cbriggsae_P6p_wt
      }
      
      liab_Cbriggsae_P6p_wt <- rbind.data.frame(va_liab_Cbriggsae_P6p_wt, h2_liab_Cbriggsae_P6p_wt,Evol_liab_Cbriggsae_P6p_wt,trait_mean_liab_Cbriggsae_P6p_wt)
      liab_Cbriggsae_P6p_wt
    }
    #Summary data scale Cbriggsae P6p
    {
      #Summary va_data_Cbriggsae_P6p_wt:NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_Cbriggsae_P6p_wt <- rbind(mean(va_data_Cbriggsae_CONTROL_P6p_wt_mod/2),mean(va_data_Cbriggsae_MA_P6p_wt_mod/2),mean(va_data_Cbriggsae_Vm_P6p_wt_mod/2))
        colnames(mean_va_data_Cbriggsae_P6p_wt) <- c("mean")
        median_va_data_Cbriggsae_P6p_wt <- rbind(median(va_data_Cbriggsae_CONTROL_P6p_wt_mod/2),median(va_data_Cbriggsae_MA_P6p_wt_mod/2),median(va_data_Cbriggsae_Vm_P6p_wt_mod/2))
        colnames(median_va_data_Cbriggsae_P6p_wt) <- c("median")
        posterior.mode_va_data_Cbriggsae_P6p_wt <- rbind(posterior.mode(as.mcmc(va_data_Cbriggsae_CONTROL_P6p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Cbriggsae_MA_P6p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Cbriggsae_Vm_P6p_wt_mod/2)))
        colnames(posterior.mode_va_data_Cbriggsae_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Cbriggsae_P6p_wt <- rbind(HPDinterval(as.mcmc(va_data_Cbriggsae_CONTROL_P6p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Cbriggsae_MA_P6p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Cbriggsae_Vm_P6p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_Cbriggsae_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Cbriggsae_P6p_wt <- rbind(HPDinterval(as.mcmc(va_data_Cbriggsae_CONTROL_P6p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Cbriggsae_MA_P6p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Cbriggsae_Vm_P6p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Cbriggsae_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Cbriggsae_P6p_wt <- rbind(effectiveSize(va_data_Cbriggsae_CONTROL_P6p_wt_mod/2),effectiveSize(va_data_Cbriggsae_MA_P6p_wt_mod/2),effectiveSize(va_data_Cbriggsae_Vm_P6p_wt_mod/2))
        colnames(effectiveSize_va_data_Cbriggsae_P6p_wt) <- c("effectiveSize")
        va_data_Cbriggsae_P6p_wt <- cbind.data.frame(mean_va_data_Cbriggsae_P6p_wt,median_va_data_Cbriggsae_P6p_wt,posterior.mode_va_data_Cbriggsae_P6p_wt,HPDinterval_0.95_va_data_Cbriggsae_P6p_wt,HPDinterval_0.83_va_data_Cbriggsae_P6p_wt,effectiveSize_va_data_Cbriggsae_P6p_wt)
        rownames(va_data_Cbriggsae_P6p_wt) <- c("va_data_Cbriggsae_CONTROL_P6p_wt_mod","va_data_Cbriggsae_MA_P6p_wt_mod","va_data_Cbriggsae_Vm_P6p_wt_mod")
        va_data_Cbriggsae_P6p_wt <- cbind(Models = rownames(va_data_Cbriggsae_P6p_wt),va_data_Cbriggsae_P6p_wt)
        rownames(va_data_Cbriggsae_P6p_wt) <- NULL
        va_data_Cbriggsae_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        va_data_Cbriggsae_P6p_wt$Treatment <- c("Control","MA","Vm")
        va_data_Cbriggsae_P6p_wt$Measure <- c("Va","Va","Va")
        va_data_Cbriggsae_P6p_wt$Scale <- c("data","data","data")
        va_data_Cbriggsae_P6p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_Cbriggsae_P6p_wt
      }
      
      #Summary h2_data_Cbriggsae_P6p_wt
      {
        mean_h2_data_Cbriggsae_P6p_wt <- rbind(mean(h2_data_Cbriggsae_CONTROL_P6p_wt_mod),mean(h2_data_Cbriggsae_MA_P6p_wt_mod),mean(h2_data_Cbriggsae_Vm_P6p_wt_mod))
        colnames(mean_h2_data_Cbriggsae_P6p_wt) <- c("mean")
        median_h2_data_Cbriggsae_P6p_wt <- rbind(median(h2_data_Cbriggsae_CONTROL_P6p_wt_mod),median(h2_data_Cbriggsae_MA_P6p_wt_mod),median(h2_data_Cbriggsae_Vm_P6p_wt_mod))
        colnames(median_h2_data_Cbriggsae_P6p_wt) <- c("median")
        posterior.mode_h2_data_Cbriggsae_P6p_wt <- rbind(posterior.mode(as.mcmc(h2_data_Cbriggsae_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(h2_data_Cbriggsae_MA_P6p_wt_mod)),posterior.mode(as.mcmc(h2_data_Cbriggsae_Vm_P6p_wt_mod)))
        colnames(posterior.mode_h2_data_Cbriggsae_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Cbriggsae_P6p_wt <- rbind(HPDinterval(as.mcmc(h2_data_Cbriggsae_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(h2_data_Cbriggsae_MA_P6p_wt_mod)),HPDinterval(as.mcmc(h2_data_Cbriggsae_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_Cbriggsae_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Cbriggsae_P6p_wt <- rbind(HPDinterval(as.mcmc(h2_data_Cbriggsae_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Cbriggsae_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Cbriggsae_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Cbriggsae_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Cbriggsae_P6p_wt <- rbind(effectiveSize(h2_data_Cbriggsae_CONTROL_P6p_wt_mod),effectiveSize(h2_data_Cbriggsae_MA_P6p_wt_mod),effectiveSize(h2_data_Cbriggsae_Vm_P6p_wt_mod))
        colnames(effectiveSize_h2_data_Cbriggsae_P6p_wt) <- c("effectiveSize")
        h2_data_Cbriggsae_P6p_wt <- cbind.data.frame(mean_h2_data_Cbriggsae_P6p_wt,median_h2_data_Cbriggsae_P6p_wt,posterior.mode_h2_data_Cbriggsae_P6p_wt,HPDinterval_0.95_h2_data_Cbriggsae_P6p_wt,HPDinterval_0.83_h2_data_Cbriggsae_P6p_wt,effectiveSize_h2_data_Cbriggsae_P6p_wt)
        rownames(h2_data_Cbriggsae_P6p_wt) <- c("h2_data_Cbriggsae_CONTROL_P6p_wt_mod","h2_data_Cbriggsae_MA_P6p_wt_mod","h2_data_Cbriggsae_Vm_P6p_wt_mod")
        h2_data_Cbriggsae_P6p_wt <- cbind(Models = rownames(h2_data_Cbriggsae_P6p_wt),h2_data_Cbriggsae_P6p_wt)
        rownames(h2_data_Cbriggsae_P6p_wt) <- NULL
        h2_data_Cbriggsae_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        h2_data_Cbriggsae_P6p_wt$Treatment <- c("Control","MA","Vm")
        h2_data_Cbriggsae_P6p_wt$Measure <- c("H2","H2","H2")
        h2_data_Cbriggsae_P6p_wt$Scale <- c("data","data","data")
        h2_data_Cbriggsae_P6p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_Cbriggsae_P6p_wt
      }
      
      #Summary Evol_data_Cbriggsae_P6p_wt
      {
        mean_Evol_data_Cbriggsae_P6p_wt <- rbind(mean(Evol_data_Cbriggsae_CONTROL_P6p_wt_mod),mean(Evol_data_Cbriggsae_MA_P6p_wt_mod),mean(Evol_data_Cbriggsae_Vm_P6p_wt_mod))
        colnames(mean_Evol_data_Cbriggsae_P6p_wt) <- c("mean")
        median_Evol_data_Cbriggsae_P6p_wt <- rbind(median(Evol_data_Cbriggsae_CONTROL_P6p_wt_mod),median(Evol_data_Cbriggsae_MA_P6p_wt_mod),median(Evol_data_Cbriggsae_Vm_P6p_wt_mod))
        colnames(median_Evol_data_Cbriggsae_P6p_wt) <- c("median")
        posterior.mode_Evol_data_Cbriggsae_P6p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_Cbriggsae_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Cbriggsae_MA_P6p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Cbriggsae_Vm_P6p_wt_mod)))
        colnames(posterior.mode_Evol_data_Cbriggsae_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Cbriggsae_P6p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_Cbriggsae_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Cbriggsae_MA_P6p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Cbriggsae_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_Cbriggsae_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Cbriggsae_P6p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_Cbriggsae_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Cbriggsae_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Cbriggsae_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Cbriggsae_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Cbriggsae_P6p_wt <- rbind(effectiveSize(Evol_data_Cbriggsae_CONTROL_P6p_wt_mod),effectiveSize(Evol_data_Cbriggsae_MA_P6p_wt_mod),effectiveSize(Evol_data_Cbriggsae_Vm_P6p_wt_mod))
        colnames(effectiveSize_Evol_data_Cbriggsae_P6p_wt) <- c("effectiveSize")
        Evol_data_Cbriggsae_P6p_wt <- cbind.data.frame(mean_Evol_data_Cbriggsae_P6p_wt,median_Evol_data_Cbriggsae_P6p_wt,posterior.mode_Evol_data_Cbriggsae_P6p_wt,HPDinterval_0.95_Evol_data_Cbriggsae_P6p_wt,HPDinterval_0.83_Evol_data_Cbriggsae_P6p_wt,effectiveSize_Evol_data_Cbriggsae_P6p_wt)
        rownames(Evol_data_Cbriggsae_P6p_wt) <- c("Evol_data_Cbriggsae_CONTROL_P6p_wt_mod","Evol_data_Cbriggsae_MA_P6p_wt_mod","Evol_data_Cbriggsae_Vm_P6p_wt_mod")
        Evol_data_Cbriggsae_P6p_wt <- cbind(Models = rownames(Evol_data_Cbriggsae_P6p_wt),Evol_data_Cbriggsae_P6p_wt)
        rownames(Evol_data_Cbriggsae_P6p_wt) <- NULL
        Evol_data_Cbriggsae_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        Evol_data_Cbriggsae_P6p_wt$Treatment <- c("Control","MA","Vm")
        Evol_data_Cbriggsae_P6p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_Cbriggsae_P6p_wt$Scale <- c("data","data","data")
        Evol_data_Cbriggsae_P6p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_Cbriggsae_P6p_wt
      }
      
      #Summary trait_mean_data_Cbriggsae_P6p_wt
      {
        mean_trait_mean_data_Cbriggsae_P6p_wt <- rbind(mean(trait_mean_data_Cbriggsae_CONTROL_P6p_wt_mod),mean(trait_mean_data_Cbriggsae_MA_P6p_wt_mod),mean(trait_mean_data_Cbriggsae_Vm_P6p_wt_mod))
        colnames(mean_trait_mean_data_Cbriggsae_P6p_wt) <- c("mean")
        median_trait_mean_data_Cbriggsae_P6p_wt <- rbind(median(trait_mean_data_Cbriggsae_CONTROL_P6p_wt_mod),median(trait_mean_data_Cbriggsae_MA_P6p_wt_mod),median(trait_mean_data_Cbriggsae_Vm_P6p_wt_mod))
        colnames(median_trait_mean_data_Cbriggsae_P6p_wt) <- c("median")
        posterior.mode_trait_mean_data_Cbriggsae_P6p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_MA_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_Vm_P6p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_Cbriggsae_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Cbriggsae_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_MA_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_Cbriggsae_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Cbriggsae_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Cbriggsae_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Cbriggsae_P6p_wt <- rbind(effectiveSize(trait_mean_data_Cbriggsae_CONTROL_P6p_wt_mod),effectiveSize(trait_mean_data_Cbriggsae_MA_P6p_wt_mod),effectiveSize(trait_mean_data_Cbriggsae_Vm_P6p_wt_mod))
        colnames(effectiveSize_trait_mean_data_Cbriggsae_P6p_wt) <- c("effectiveSize")
        trait_mean_data_Cbriggsae_P6p_wt <- cbind.data.frame(mean_trait_mean_data_Cbriggsae_P6p_wt,median_trait_mean_data_Cbriggsae_P6p_wt,posterior.mode_trait_mean_data_Cbriggsae_P6p_wt,HPDinterval_0.95_trait_mean_data_Cbriggsae_P6p_wt,HPDinterval_0.83_trait_mean_data_Cbriggsae_P6p_wt,effectiveSize_trait_mean_data_Cbriggsae_P6p_wt)
        rownames(trait_mean_data_Cbriggsae_P6p_wt) <- c("trait_mean_data_Cbriggsae_CONTROL_P6p_wt_mod","trait_mean_data_Cbriggsae_MA_P6p_wt_mod","trait_mean_data_Cbriggsae_Vm_P6p_wt_mod")
        trait_mean_data_Cbriggsae_P6p_wt <- cbind(Models = rownames(trait_mean_data_Cbriggsae_P6p_wt),trait_mean_data_Cbriggsae_P6p_wt)
        rownames(trait_mean_data_Cbriggsae_P6p_wt) <- NULL
        trait_mean_data_Cbriggsae_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        trait_mean_data_Cbriggsae_P6p_wt$Treatment <- c("Control","MA","Vm")
        trait_mean_data_Cbriggsae_P6p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Cbriggsae_P6p_wt$Scale <- c("data","data","data")
        trait_mean_data_Cbriggsae_P6p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_Cbriggsae_P6p_wt
      }
      
      data_Cbriggsae_P6p_wt <- rbind.data.frame(va_data_Cbriggsae_P6p_wt, h2_data_Cbriggsae_P6p_wt,Evol_data_Cbriggsae_P6p_wt,trait_mean_data_Cbriggsae_P6p_wt)
      data_Cbriggsae_P6p_wt
      
    }
    Vm_Cbriggsae_P6p_wt <- rbind.data.frame(liab_Cbriggsae_P6p_wt, data_Cbriggsae_P6p_wt)
    Vm_Cbriggsae_P6p_wt$Pnp_fate <- rep("wt", 24)
    Vm_Cbriggsae_P6p_wt
    #remove Cbriggsae P6p_wt models
    {
      remove(Cbriggsae_CONTROL_P6p_wt_mod)
      remove(Cbriggsae_MA_P6p_wt_mod)
      remove(Cbriggsae_Vm_P6p_wt_mod)
    }
  }
  
  #Summary Cbriggsae P7p
  {
    #Summary liability scale Cbriggsae P7p
    {
      #Summary va_liab_Cbriggsae_P7p_wt: NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_Cbriggsae_P7p_wt <- rbind(mean(va_liab_Cbriggsae_CONTROL_P7p_wt_mod/2),mean(va_liab_Cbriggsae_MA_P7p_wt_mod/2),mean(va_liab_Cbriggsae_Vm_P7p_wt_mod/2))
        colnames(mean_va_liab_Cbriggsae_P7p_wt) <- c("mean")
        median_va_liab_Cbriggsae_P7p_wt <- rbind(median(va_liab_Cbriggsae_CONTROL_P7p_wt_mod/2),median(va_liab_Cbriggsae_MA_P7p_wt_mod/2),median(va_liab_Cbriggsae_Vm_P7p_wt_mod/2))
        colnames(median_va_liab_Cbriggsae_P7p_wt) <- c("median")
        posterior.mode_va_liab_Cbriggsae_P7p_wt <- rbind(posterior.mode(va_liab_Cbriggsae_CONTROL_P7p_wt_mod/2),posterior.mode(va_liab_Cbriggsae_MA_P7p_wt_mod/2),posterior.mode(va_liab_Cbriggsae_Vm_P7p_wt_mod/2))
        colnames(posterior.mode_va_liab_Cbriggsae_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Cbriggsae_P7p_wt <- rbind(HPDinterval(va_liab_Cbriggsae_CONTROL_P7p_wt_mod/2),HPDinterval(va_liab_Cbriggsae_MA_P7p_wt_mod/2),HPDinterval(va_liab_Cbriggsae_Vm_P7p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_Cbriggsae_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Cbriggsae_P7p_wt <- rbind(HPDinterval(va_liab_Cbriggsae_CONTROL_P7p_wt_mod/2,prob=.83),HPDinterval(va_liab_Cbriggsae_MA_P7p_wt_mod/2,prob=.83),HPDinterval(va_liab_Cbriggsae_Vm_P7p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_Cbriggsae_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Cbriggsae_P7p_wt <- rbind(effectiveSize(va_liab_Cbriggsae_CONTROL_P7p_wt_mod/2),effectiveSize(va_liab_Cbriggsae_MA_P7p_wt_mod/2),effectiveSize(va_liab_Cbriggsae_Vm_P7p_wt_mod/2))
        colnames(effectiveSize_va_liab_Cbriggsae_P7p_wt) <- c("effectiveSize")
        va_liab_Cbriggsae_P7p_wt <- cbind.data.frame(mean_va_liab_Cbriggsae_P7p_wt,median_va_liab_Cbriggsae_P7p_wt,posterior.mode_va_liab_Cbriggsae_P7p_wt,HPDinterval_0.95_va_liab_Cbriggsae_P7p_wt,HPDinterval_0.83_va_liab_Cbriggsae_P7p_wt,effectiveSize_va_liab_Cbriggsae_P7p_wt)
        rownames(va_liab_Cbriggsae_P7p_wt) <- c("va_liab_Cbriggsae_CONTROL_P7p_wt_mod","va_liab_Cbriggsae_MA_P7p_wt_mod","va_liab_Cbriggsae_Vm_P7p_wt_mod")
        va_liab_Cbriggsae_P7p_wt <- cbind(Models = rownames(va_liab_Cbriggsae_P7p_wt),va_liab_Cbriggsae_P7p_wt)
        rownames(va_liab_Cbriggsae_P7p_wt) <- NULL
        va_liab_Cbriggsae_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        va_liab_Cbriggsae_P7p_wt$Treatment <- c("Control","MA","Vm")
        va_liab_Cbriggsae_P7p_wt$Measure <- c("Va","Va","Va")
        va_liab_Cbriggsae_P7p_wt$Scale <- c("liab","liab","liab")
        va_liab_Cbriggsae_P7p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_Cbriggsae_P7p_wt
      }
      
      #Summary h2_liab_Cbriggsae_P7p_wt
      {
        mean_h2_liab_Cbriggsae_P7p_wt <- rbind(mean(h2_liab_Cbriggsae_CONTROL_P7p_wt_mod),mean(h2_liab_Cbriggsae_MA_P7p_wt_mod),mean(h2_liab_Cbriggsae_Vm_P7p_wt_mod))
        colnames(mean_h2_liab_Cbriggsae_P7p_wt) <- c("mean")
        median_h2_liab_Cbriggsae_P7p_wt <- rbind(median(h2_liab_Cbriggsae_CONTROL_P7p_wt_mod),median(h2_liab_Cbriggsae_MA_P7p_wt_mod),median(h2_liab_Cbriggsae_Vm_P7p_wt_mod))
        colnames(median_h2_liab_Cbriggsae_P7p_wt) <- c("median")
        posterior.mode_h2_liab_Cbriggsae_P7p_wt <- rbind(posterior.mode(h2_liab_Cbriggsae_CONTROL_P7p_wt_mod),posterior.mode(h2_liab_Cbriggsae_MA_P7p_wt_mod),posterior.mode(h2_liab_Cbriggsae_Vm_P7p_wt_mod))
        colnames(posterior.mode_h2_liab_Cbriggsae_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_Cbriggsae_P7p_wt <- rbind(HPDinterval(h2_liab_Cbriggsae_CONTROL_P7p_wt_mod),HPDinterval(h2_liab_Cbriggsae_MA_P7p_wt_mod),HPDinterval(h2_liab_Cbriggsae_Vm_P7p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_Cbriggsae_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_Cbriggsae_P7p_wt <- rbind(HPDinterval(h2_liab_Cbriggsae_CONTROL_P7p_wt_mod,prob=.83),HPDinterval(h2_liab_Cbriggsae_MA_P7p_wt_mod,prob=.83),HPDinterval(h2_liab_Cbriggsae_Vm_P7p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_Cbriggsae_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_Cbriggsae_P7p_wt <- rbind(effectiveSize(h2_liab_Cbriggsae_CONTROL_P7p_wt_mod),effectiveSize(h2_liab_Cbriggsae_MA_P7p_wt_mod),effectiveSize(h2_liab_Cbriggsae_Vm_P7p_wt_mod))
        colnames(effectiveSize_h2_liab_Cbriggsae_P7p_wt) <- c("effectiveSize")
        h2_liab_Cbriggsae_P7p_wt <- cbind.data.frame(mean_h2_liab_Cbriggsae_P7p_wt,median_h2_liab_Cbriggsae_P7p_wt,posterior.mode_h2_liab_Cbriggsae_P7p_wt,HPDinterval_0.95_h2_liab_Cbriggsae_P7p_wt,HPDinterval_0.83_h2_liab_Cbriggsae_P7p_wt,effectiveSize_h2_liab_Cbriggsae_P7p_wt)
        rownames(h2_liab_Cbriggsae_P7p_wt) <- c("h2_liab_Cbriggsae_CONTROL_P7p_wt_mod","h2_liab_Cbriggsae_MA_P7p_wt_mod","h2_liab_Cbriggsae_Vm_P7p_wt_mod")
        h2_liab_Cbriggsae_P7p_wt <- cbind(Models = rownames(h2_liab_Cbriggsae_P7p_wt),h2_liab_Cbriggsae_P7p_wt)
        rownames(h2_liab_Cbriggsae_P7p_wt) <- NULL
        h2_liab_Cbriggsae_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        h2_liab_Cbriggsae_P7p_wt$Treatment <- c("Control","MA","Vm")
        h2_liab_Cbriggsae_P7p_wt$Measure <- c("H2","H2","H2")
        h2_liab_Cbriggsae_P7p_wt$Scale <- c("liab","liab","liab")
        h2_liab_Cbriggsae_P7p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_Cbriggsae_P7p_wt
      }
      
      #Summary Evol_liab_Cbriggsae_P7p_wt
      {
        mean_Evol_liab_Cbriggsae_P7p_wt <- rbind(mean(Evol_liab_Cbriggsae_CONTROL_P7p_wt_mod),mean(Evol_liab_Cbriggsae_MA_P7p_wt_mod),mean(Evol_liab_Cbriggsae_Vm_P7p_wt_mod))
        colnames(mean_Evol_liab_Cbriggsae_P7p_wt) <- c("mean")
        median_Evol_liab_Cbriggsae_P7p_wt <- rbind(median(Evol_liab_Cbriggsae_CONTROL_P7p_wt_mod),median(Evol_liab_Cbriggsae_MA_P7p_wt_mod),median(Evol_liab_Cbriggsae_Vm_P7p_wt_mod))
        colnames(median_Evol_liab_Cbriggsae_P7p_wt) <- c("median")
        posterior.mode_Evol_liab_Cbriggsae_P7p_wt <- rbind(posterior.mode(Evol_liab_Cbriggsae_CONTROL_P7p_wt_mod),posterior.mode(Evol_liab_Cbriggsae_MA_P7p_wt_mod),posterior.mode(Evol_liab_Cbriggsae_Vm_P7p_wt_mod))
        colnames(posterior.mode_Evol_liab_Cbriggsae_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Cbriggsae_P7p_wt <- rbind(HPDinterval(Evol_liab_Cbriggsae_CONTROL_P7p_wt_mod),HPDinterval(Evol_liab_Cbriggsae_MA_P7p_wt_mod),HPDinterval(Evol_liab_Cbriggsae_Vm_P7p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_Cbriggsae_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Cbriggsae_P7p_wt <- rbind(HPDinterval(Evol_liab_Cbriggsae_CONTROL_P7p_wt_mod,prob=.83),HPDinterval(Evol_liab_Cbriggsae_MA_P7p_wt_mod,prob=.83),HPDinterval(Evol_liab_Cbriggsae_Vm_P7p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Cbriggsae_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Cbriggsae_P7p_wt <- rbind(effectiveSize(Evol_liab_Cbriggsae_CONTROL_P7p_wt_mod),effectiveSize(Evol_liab_Cbriggsae_MA_P7p_wt_mod),effectiveSize(Evol_liab_Cbriggsae_Vm_P7p_wt_mod))
        colnames(effectiveSize_Evol_liab_Cbriggsae_P7p_wt) <- c("effectiveSize")
        Evol_liab_Cbriggsae_P7p_wt <- cbind.data.frame(mean_Evol_liab_Cbriggsae_P7p_wt,median_Evol_liab_Cbriggsae_P7p_wt,posterior.mode_Evol_liab_Cbriggsae_P7p_wt,HPDinterval_0.95_Evol_liab_Cbriggsae_P7p_wt,HPDinterval_0.83_Evol_liab_Cbriggsae_P7p_wt,effectiveSize_Evol_liab_Cbriggsae_P7p_wt)
        rownames(Evol_liab_Cbriggsae_P7p_wt) <- c("Evol_liab_Cbriggsae_CONTROL_P7p_wt_mod","Evol_liab_Cbriggsae_MA_P7p_wt_mod","Evol_liab_Cbriggsae_Vm_P7p_wt_mod")
        Evol_liab_Cbriggsae_P7p_wt <- cbind(Models = rownames(Evol_liab_Cbriggsae_P7p_wt),Evol_liab_Cbriggsae_P7p_wt)
        rownames(Evol_liab_Cbriggsae_P7p_wt) <- NULL
        Evol_liab_Cbriggsae_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        Evol_liab_Cbriggsae_P7p_wt$Treatment <- c("Control","MA","Vm")
        Evol_liab_Cbriggsae_P7p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_Cbriggsae_P7p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_Cbriggsae_P7p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_Cbriggsae_P7p_wt
      }
      
      #Summary trait_mean_liab_Cbriggsae_P7p_wt
      {
        mean_trait_mean_liab_Cbriggsae_P7p_wt <- rbind(mean(trait_mean_liab_Cbriggsae_CONTROL_P7p_wt_mod),mean(trait_mean_liab_Cbriggsae_MA_P7p_wt_mod),mean(trait_mean_liab_Cbriggsae_Vm_P7p_wt_mod))
        colnames(mean_trait_mean_liab_Cbriggsae_P7p_wt) <- c("mean")
        median_trait_mean_liab_Cbriggsae_P7p_wt <- rbind(median(trait_mean_liab_Cbriggsae_CONTROL_P7p_wt_mod),median(trait_mean_liab_Cbriggsae_MA_P7p_wt_mod),median(trait_mean_liab_Cbriggsae_Vm_P7p_wt_mod))
        colnames(median_trait_mean_liab_Cbriggsae_P7p_wt) <- c("median")
        posterior.mode_trait_mean_liab_Cbriggsae_P7p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_MA_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P7p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_Cbriggsae_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Cbriggsae_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_MA_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_Cbriggsae_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Cbriggsae_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_Cbriggsae_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Cbriggsae_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Cbriggsae_P7p_wt <- rbind(effectiveSize(trait_mean_liab_Cbriggsae_CONTROL_P7p_wt_mod),effectiveSize(trait_mean_liab_Cbriggsae_MA_P7p_wt_mod),effectiveSize(trait_mean_liab_Cbriggsae_Vm_P7p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_Cbriggsae_P7p_wt) <- c("effectiveSize")
        trait_mean_liab_Cbriggsae_P7p_wt <- cbind.data.frame(mean_trait_mean_liab_Cbriggsae_P7p_wt,median_trait_mean_liab_Cbriggsae_P7p_wt,posterior.mode_trait_mean_liab_Cbriggsae_P7p_wt,HPDinterval_0.95_trait_mean_liab_Cbriggsae_P7p_wt,HPDinterval_0.83_trait_mean_liab_Cbriggsae_P7p_wt,effectiveSize_trait_mean_liab_Cbriggsae_P7p_wt)
        rownames(trait_mean_liab_Cbriggsae_P7p_wt) <- c("trait_mean_liab_Cbriggsae_CONTROL_P7p_wt_mod","trait_mean_liab_Cbriggsae_MA_P7p_wt_mod","trait_mean_liab_Cbriggsae_Vm_P7p_wt_mod")
        trait_mean_liab_Cbriggsae_P7p_wt <- cbind(Models = rownames(trait_mean_liab_Cbriggsae_P7p_wt),trait_mean_liab_Cbriggsae_P7p_wt)
        rownames(trait_mean_liab_Cbriggsae_P7p_wt) <- NULL
        trait_mean_liab_Cbriggsae_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        trait_mean_liab_Cbriggsae_P7p_wt$Treatment <- c("Control","MA","Vm")
        trait_mean_liab_Cbriggsae_P7p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Cbriggsae_P7p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_Cbriggsae_P7p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_Cbriggsae_P7p_wt
      }
      
      liab_Cbriggsae_P7p_wt <- rbind.data.frame(va_liab_Cbriggsae_P7p_wt, h2_liab_Cbriggsae_P7p_wt,Evol_liab_Cbriggsae_P7p_wt,trait_mean_liab_Cbriggsae_P7p_wt)
      liab_Cbriggsae_P7p_wt
    }
    #Summary data scale Cbriggsae P7p
    {
      #Summary va_data_Cbriggsae_P7p_wt:NB Given that mutant lines are homozygous, Va is divided by to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_Cbriggsae_P7p_wt <- rbind(mean(va_data_Cbriggsae_CONTROL_P7p_wt_mod/2),mean(va_data_Cbriggsae_MA_P7p_wt_mod/2),mean(va_data_Cbriggsae_Vm_P7p_wt_mod/2))
        colnames(mean_va_data_Cbriggsae_P7p_wt) <- c("mean")
        median_va_data_Cbriggsae_P7p_wt <- rbind(median(va_data_Cbriggsae_CONTROL_P7p_wt_mod/2),median(va_data_Cbriggsae_MA_P7p_wt_mod/2),median(va_data_Cbriggsae_Vm_P7p_wt_mod/2))
        colnames(median_va_data_Cbriggsae_P7p_wt) <- c("median")
        posterior.mode_va_data_Cbriggsae_P7p_wt <- rbind(posterior.mode(as.mcmc(va_data_Cbriggsae_CONTROL_P7p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Cbriggsae_MA_P7p_wt_mod/2)),posterior.mode(as.mcmc(va_data_Cbriggsae_Vm_P7p_wt_mod/2)))
        colnames(posterior.mode_va_data_Cbriggsae_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Cbriggsae_P7p_wt <- rbind(HPDinterval(as.mcmc(va_data_Cbriggsae_CONTROL_P7p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Cbriggsae_MA_P7p_wt_mod/2)),HPDinterval(as.mcmc(va_data_Cbriggsae_Vm_P7p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_Cbriggsae_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Cbriggsae_P7p_wt <- rbind(HPDinterval(as.mcmc(va_data_Cbriggsae_CONTROL_P7p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Cbriggsae_MA_P7p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_Cbriggsae_Vm_P7p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_Cbriggsae_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Cbriggsae_P7p_wt <- rbind(effectiveSize(va_data_Cbriggsae_CONTROL_P7p_wt_mod/2),effectiveSize(va_data_Cbriggsae_MA_P7p_wt_mod/2),effectiveSize(va_data_Cbriggsae_Vm_P7p_wt_mod/2))
        colnames(effectiveSize_va_data_Cbriggsae_P7p_wt) <- c("effectiveSize")
        va_data_Cbriggsae_P7p_wt <- cbind.data.frame(mean_va_data_Cbriggsae_P7p_wt,median_va_data_Cbriggsae_P7p_wt,posterior.mode_va_data_Cbriggsae_P7p_wt,HPDinterval_0.95_va_data_Cbriggsae_P7p_wt,HPDinterval_0.83_va_data_Cbriggsae_P7p_wt,effectiveSize_va_data_Cbriggsae_P7p_wt)
        rownames(va_data_Cbriggsae_P7p_wt) <- c("va_data_Cbriggsae_CONTROL_P7p_wt_mod","va_data_Cbriggsae_MA_P7p_wt_mod","va_data_Cbriggsae_Vm_P7p_wt_mod")
        va_data_Cbriggsae_P7p_wt <- cbind(Models = rownames(va_data_Cbriggsae_P7p_wt),va_data_Cbriggsae_P7p_wt)
        rownames(va_data_Cbriggsae_P7p_wt) <- NULL
        va_data_Cbriggsae_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        va_data_Cbriggsae_P7p_wt$Treatment <- c("Control","MA","Vm")
        va_data_Cbriggsae_P7p_wt$Measure <- c("Va","Va","Va")
        va_data_Cbriggsae_P7p_wt$Scale <- c("data","data","data")
        va_data_Cbriggsae_P7p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_Cbriggsae_P7p_wt
      }
      
      #Summary h2_data_Cbriggsae_P7p_wt
      {
        mean_h2_data_Cbriggsae_P7p_wt <- rbind(mean(h2_data_Cbriggsae_CONTROL_P7p_wt_mod),mean(h2_data_Cbriggsae_MA_P7p_wt_mod),mean(h2_data_Cbriggsae_Vm_P7p_wt_mod))
        colnames(mean_h2_data_Cbriggsae_P7p_wt) <- c("mean")
        median_h2_data_Cbriggsae_P7p_wt <- rbind(median(h2_data_Cbriggsae_CONTROL_P7p_wt_mod),median(h2_data_Cbriggsae_MA_P7p_wt_mod),median(h2_data_Cbriggsae_Vm_P7p_wt_mod))
        colnames(median_h2_data_Cbriggsae_P7p_wt) <- c("median")
        posterior.mode_h2_data_Cbriggsae_P7p_wt <- rbind(posterior.mode(as.mcmc(h2_data_Cbriggsae_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(h2_data_Cbriggsae_MA_P7p_wt_mod)),posterior.mode(as.mcmc(h2_data_Cbriggsae_Vm_P7p_wt_mod)))
        colnames(posterior.mode_h2_data_Cbriggsae_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_Cbriggsae_P7p_wt <- rbind(HPDinterval(as.mcmc(h2_data_Cbriggsae_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(h2_data_Cbriggsae_MA_P7p_wt_mod)),HPDinterval(as.mcmc(h2_data_Cbriggsae_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_Cbriggsae_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_Cbriggsae_P7p_wt <- rbind(HPDinterval(as.mcmc(h2_data_Cbriggsae_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Cbriggsae_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_Cbriggsae_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_Cbriggsae_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_Cbriggsae_P7p_wt <- rbind(effectiveSize(h2_data_Cbriggsae_CONTROL_P7p_wt_mod),effectiveSize(h2_data_Cbriggsae_MA_P7p_wt_mod),effectiveSize(h2_data_Cbriggsae_Vm_P7p_wt_mod))
        colnames(effectiveSize_h2_data_Cbriggsae_P7p_wt) <- c("effectiveSize")
        h2_data_Cbriggsae_P7p_wt <- cbind.data.frame(mean_h2_data_Cbriggsae_P7p_wt,median_h2_data_Cbriggsae_P7p_wt,posterior.mode_h2_data_Cbriggsae_P7p_wt,HPDinterval_0.95_h2_data_Cbriggsae_P7p_wt,HPDinterval_0.83_h2_data_Cbriggsae_P7p_wt,effectiveSize_h2_data_Cbriggsae_P7p_wt)
        rownames(h2_data_Cbriggsae_P7p_wt) <- c("h2_data_Cbriggsae_CONTROL_P7p_wt_mod","h2_data_Cbriggsae_MA_P7p_wt_mod","h2_data_Cbriggsae_Vm_P7p_wt_mod")
        h2_data_Cbriggsae_P7p_wt <- cbind(Models = rownames(h2_data_Cbriggsae_P7p_wt),h2_data_Cbriggsae_P7p_wt)
        rownames(h2_data_Cbriggsae_P7p_wt) <- NULL
        h2_data_Cbriggsae_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        h2_data_Cbriggsae_P7p_wt$Treatment <- c("Control","MA","Vm")
        h2_data_Cbriggsae_P7p_wt$Measure <- c("H2","H2","H2")
        h2_data_Cbriggsae_P7p_wt$Scale <- c("data","data","data")
        h2_data_Cbriggsae_P7p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_Cbriggsae_P7p_wt
      }
      
      #Summary Evol_data_Cbriggsae_P7p_wt
      {
        mean_Evol_data_Cbriggsae_P7p_wt <- rbind(mean(Evol_data_Cbriggsae_CONTROL_P7p_wt_mod),mean(Evol_data_Cbriggsae_MA_P7p_wt_mod),mean(Evol_data_Cbriggsae_Vm_P7p_wt_mod))
        colnames(mean_Evol_data_Cbriggsae_P7p_wt) <- c("mean")
        median_Evol_data_Cbriggsae_P7p_wt <- rbind(median(Evol_data_Cbriggsae_CONTROL_P7p_wt_mod),median(Evol_data_Cbriggsae_MA_P7p_wt_mod),median(Evol_data_Cbriggsae_Vm_P7p_wt_mod))
        colnames(median_Evol_data_Cbriggsae_P7p_wt) <- c("median")
        posterior.mode_Evol_data_Cbriggsae_P7p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_Cbriggsae_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Cbriggsae_MA_P7p_wt_mod)),posterior.mode(as.mcmc(Evol_data_Cbriggsae_Vm_P7p_wt_mod)))
        colnames(posterior.mode_Evol_data_Cbriggsae_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Cbriggsae_P7p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_Cbriggsae_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Cbriggsae_MA_P7p_wt_mod)),HPDinterval(as.mcmc(Evol_data_Cbriggsae_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_Cbriggsae_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Cbriggsae_P7p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_Cbriggsae_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Cbriggsae_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_Cbriggsae_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Cbriggsae_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Cbriggsae_P7p_wt <- rbind(effectiveSize(Evol_data_Cbriggsae_CONTROL_P7p_wt_mod),effectiveSize(Evol_data_Cbriggsae_MA_P7p_wt_mod),effectiveSize(Evol_data_Cbriggsae_Vm_P7p_wt_mod))
        colnames(effectiveSize_Evol_data_Cbriggsae_P7p_wt) <- c("effectiveSize")
        Evol_data_Cbriggsae_P7p_wt <- cbind.data.frame(mean_Evol_data_Cbriggsae_P7p_wt,median_Evol_data_Cbriggsae_P7p_wt,posterior.mode_Evol_data_Cbriggsae_P7p_wt,HPDinterval_0.95_Evol_data_Cbriggsae_P7p_wt,HPDinterval_0.83_Evol_data_Cbriggsae_P7p_wt,effectiveSize_Evol_data_Cbriggsae_P7p_wt)
        rownames(Evol_data_Cbriggsae_P7p_wt) <- c("Evol_data_Cbriggsae_CONTROL_P7p_wt_mod","Evol_data_Cbriggsae_MA_P7p_wt_mod","Evol_data_Cbriggsae_Vm_P7p_wt_mod")
        Evol_data_Cbriggsae_P7p_wt <- cbind(Models = rownames(Evol_data_Cbriggsae_P7p_wt),Evol_data_Cbriggsae_P7p_wt)
        rownames(Evol_data_Cbriggsae_P7p_wt) <- NULL
        Evol_data_Cbriggsae_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        Evol_data_Cbriggsae_P7p_wt$Treatment <- c("Control","MA","Vm")
        Evol_data_Cbriggsae_P7p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_Cbriggsae_P7p_wt$Scale <- c("data","data","data")
        Evol_data_Cbriggsae_P7p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_Cbriggsae_P7p_wt
      }
      
      #Summary trait_mean_data_Cbriggsae_P7p_wt
      {
        mean_trait_mean_data_Cbriggsae_P7p_wt <- rbind(mean(trait_mean_data_Cbriggsae_CONTROL_P7p_wt_mod),mean(trait_mean_data_Cbriggsae_MA_P7p_wt_mod),mean(trait_mean_data_Cbriggsae_Vm_P7p_wt_mod))
        colnames(mean_trait_mean_data_Cbriggsae_P7p_wt) <- c("mean")
        median_trait_mean_data_Cbriggsae_P7p_wt <- rbind(median(trait_mean_data_Cbriggsae_CONTROL_P7p_wt_mod),median(trait_mean_data_Cbriggsae_MA_P7p_wt_mod),median(trait_mean_data_Cbriggsae_Vm_P7p_wt_mod))
        colnames(median_trait_mean_data_Cbriggsae_P7p_wt) <- c("median")
        posterior.mode_trait_mean_data_Cbriggsae_P7p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_MA_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_Cbriggsae_Vm_P7p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_Cbriggsae_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Cbriggsae_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_MA_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_Cbriggsae_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Cbriggsae_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Cbriggsae_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Cbriggsae_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Cbriggsae_P7p_wt <- rbind(effectiveSize(trait_mean_data_Cbriggsae_CONTROL_P7p_wt_mod),effectiveSize(trait_mean_data_Cbriggsae_MA_P7p_wt_mod),effectiveSize(trait_mean_data_Cbriggsae_Vm_P7p_wt_mod))
        colnames(effectiveSize_trait_mean_data_Cbriggsae_P7p_wt) <- c("effectiveSize")
        trait_mean_data_Cbriggsae_P7p_wt <- cbind.data.frame(mean_trait_mean_data_Cbriggsae_P7p_wt,median_trait_mean_data_Cbriggsae_P7p_wt,posterior.mode_trait_mean_data_Cbriggsae_P7p_wt,HPDinterval_0.95_trait_mean_data_Cbriggsae_P7p_wt,HPDinterval_0.83_trait_mean_data_Cbriggsae_P7p_wt,effectiveSize_trait_mean_data_Cbriggsae_P7p_wt)
        rownames(trait_mean_data_Cbriggsae_P7p_wt) <- c("trait_mean_data_Cbriggsae_CONTROL_P7p_wt_mod","trait_mean_data_Cbriggsae_MA_P7p_wt_mod","trait_mean_data_Cbriggsae_Vm_P7p_wt_mod")
        trait_mean_data_Cbriggsae_P7p_wt <- cbind(Models = rownames(trait_mean_data_Cbriggsae_P7p_wt),trait_mean_data_Cbriggsae_P7p_wt)
        rownames(trait_mean_data_Cbriggsae_P7p_wt) <- NULL
        trait_mean_data_Cbriggsae_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        trait_mean_data_Cbriggsae_P7p_wt$Treatment <- c("Control","MA","Vm")
        trait_mean_data_Cbriggsae_P7p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_Cbriggsae_P7p_wt$Scale <- c("data","data","data")
        trait_mean_data_Cbriggsae_P7p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_Cbriggsae_P7p_wt
      }
      
      data_Cbriggsae_P7p_wt <- rbind.data.frame(va_data_Cbriggsae_P7p_wt, h2_data_Cbriggsae_P7p_wt,Evol_data_Cbriggsae_P7p_wt,trait_mean_data_Cbriggsae_P7p_wt)
      data_Cbriggsae_P7p_wt
      
    }
    Vm_Cbriggsae_P7p_wt <- rbind.data.frame(liab_Cbriggsae_P7p_wt, data_Cbriggsae_P7p_wt)
    Vm_Cbriggsae_P7p_wt$Pnp_fate <- rep("wt", 24)
    Vm_Cbriggsae_P7p_wt
    #remove Cbriggsae P7p_wt models
    {
      remove(Cbriggsae_CONTROL_P7p_wt_mod)
      remove(Cbriggsae_MA_P7p_wt_mod)
      remove(Cbriggsae_Vm_P7p_wt_mod)
    }
  }
  
  Vm_Cbriggsae_summary_SS <- rbind.data.frame(Vm_Cbriggsae_P3p_SS,Vm_Cbriggsae_P4p_SS,Vm_Cbriggsae_P5p_wt,Vm_Cbriggsae_P6p_wt,Vm_Cbriggsae_P7p_wt,Vm_Cbriggsae_P8p_SS)
  Vm_Cbriggsae_summary_SS$Species <- rep("C.briggsae",144)
  Vm_Cbriggsae_summary_SS$Genus <- rep("Caenorhabditis",144)
  View(Vm_Cbriggsae_summary_SS)
  
  #Vm_Cbriggsae_P3p_divided_P4p_SS
  {
    #Vm_Cbriggsae_P3p_divided_P4p_SS_liab
    {
      
      va_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod <- va_liab_Cbriggsae_Vm_P3p_SS_mod / va_liab_Cbriggsae_Vm_P4p_SS_mod
      h2_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod <- h2_liab_Cbriggsae_Vm_P3p_SS_mod / h2_liab_Cbriggsae_Vm_P4p_SS_mod
      Evol_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod <- Evol_liab_Cbriggsae_Vm_P3p_SS_mod / Evol_liab_Cbriggsae_Vm_P4p_SS_mod
      
      mean_va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS <- rbind(mean(log10(va_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)),mean(log10(h2_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)), mean(log10(Evol_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)))
      colnames(mean_va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS) <- c("mean")
      median_va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS <- rbind(median(log10(va_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)),median(log10(h2_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)), median(log10(Evol_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)))
      colnames(median_va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS) <- c("median")
      posterior.mode_va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS <- rbind(posterior.mode(as.mcmc(log10(va_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod))),posterior.mode(as.mcmc(log10(h2_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod))),posterior.mode(as.mcmc(log10(Evol_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod))))
      colnames(posterior.mode_va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS) <- c("posterior.mode")
      HPDinterval_0.95_va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS <- rbind(HPDinterval(as.mcmc(log10(va_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod))),HPDinterval(as.mcmc(log10(h2_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod))),HPDinterval(as.mcmc(log10(Evol_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod))))
      colnames(HPDinterval_0.95_va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS) <- c("CI_lower_0.95","CI_upper_0.95")
      HPDinterval_0.83_va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS <- rbind(HPDinterval(as.mcmc(log10(va_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)),prob=.83),HPDinterval(as.mcmc(log10(h2_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)),prob=.83),HPDinterval(as.mcmc(log10(Evol_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)),prob=.83))
      colnames(HPDinterval_0.83_va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS) <- c("CI_lower_0.83","CI_upper_0.83")
      va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS <- cbind.data.frame(mean_va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS,median_va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS,posterior.mode_va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS,HPDinterval_0.95_va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS,HPDinterval_0.83_va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS)
      rownames(va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS) <- c("va_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_log10","h2_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_log10","Evol_liab_Cbriggsae_Vm_P3p_divided_P4p_SS_log10")
      va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS$Measure <- c("Va","H2", "Evol")
      va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS$Species <- rep("C.briggsae",3)
      va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS$Genus <- rep("Caenorhabditis",3)
      va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS$Scale <- rep("liab",3)
      va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS
      
      pdf("Vm_va_h2_Evol_liab_P3p_divided_P4p_SS_log10_Cbriggsae.pdf")
      ggplot(va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS, aes(x=Measure, y= median)) +
        geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
        geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
        geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
        theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
        labs(y = "Va log10 ( P3.p / P4.p)", title = "Cbriggsae_log10(P3p/P4p)_liab")
      dev.off() 
      
    }
    
    #Vm_Cbriggsae_P3p_divided_P4p_SS_data
    
    {
      va_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod <- va_data_Cbriggsae_Vm_P3p_SS_mod / va_data_Cbriggsae_Vm_P4p_SS_mod
      h2_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod <- h2_data_Cbriggsae_Vm_P3p_SS_mod / h2_data_Cbriggsae_Vm_P4p_SS_mod
      Evol_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod <- Evol_data_Cbriggsae_Vm_P3p_SS_mod / Evol_data_Cbriggsae_Vm_P4p_SS_mod
      
      mean_va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS <- rbind(mean(log10(va_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)),mean(log10(h2_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)), mean(log10(Evol_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)))
      colnames(mean_va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS) <- c("mean")
      median_va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS <- rbind(median(log10(va_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)),median(log10(h2_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)), median(log10(Evol_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)))
      colnames(median_va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS) <- c("median")
      posterior.mode_va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS <- rbind(posterior.mode(as.mcmc(log10(va_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod))),posterior.mode(as.mcmc(log10(h2_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod))),posterior.mode(as.mcmc(log10(Evol_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod))))
      colnames(posterior.mode_va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS) <- c("posterior.mode")
      HPDinterval_0.95_va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS <- rbind(HPDinterval(as.mcmc(log10(va_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod))),HPDinterval(as.mcmc(log10(h2_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod))),HPDinterval(as.mcmc(log10(Evol_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod))))
      colnames(HPDinterval_0.95_va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS) <- c("CI_lower_0.95","CI_upper_0.95")
      HPDinterval_0.83_va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS <- rbind(HPDinterval(as.mcmc(log10(va_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)),prob=.83),HPDinterval(as.mcmc(log10(h2_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)),prob=.83),HPDinterval(as.mcmc(log10(Evol_data_Cbriggsae_Vm_P3p_divided_P4p_SS_mod)),prob=.83))
      colnames(HPDinterval_0.83_va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS) <- c("CI_lower_0.83","CI_upper_0.83")
      va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS <- cbind.data.frame(mean_va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS,median_va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS,posterior.mode_va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS,HPDinterval_0.95_va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS,HPDinterval_0.83_va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS)
      rownames(va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS) <- c("va_data_Cbriggsae_Vm_P3p_divided_P4p_SS_log10","h2_data_Cbriggsae_Vm_P3p_divided_P4p_SS_log10","Evol_data_Cbriggsae_Vm_P3p_divided_P4p_SS_log10")
      va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS$Measure <- c("Va","H2", "Evol")
      va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS$Species <- rep("C.briggsae",3)
      va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS$Genus <- rep("Caenorhabditis",3)
      va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS$Scale <- rep("data",3)
      va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS
      
      pdf("Vm_va_h2_Evol_data_P3p_divided_P4p_SS_log10_Cbriggsae.pdf")
      ggplot(va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS, aes(x=Measure, y= median)) +
        geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
        geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
        geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
        theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
        labs(y = "Va log10 ( P3.p / P4.p)", title = "Cbriggsae_log10(P3p/P4p)_data")
      dev.off() 
      
    }
    
    va_h2_Evol_Vm_Cbriggsae_P3p_divided_P4p_SS_summary <- rbind.data.frame(va_h2_Evol_liab_Vm_Cbriggsae_P3p_divided_P4p_SS,va_h2_Evol_data_Vm_Cbriggsae_P3p_divided_P4p_SS)
    va_h2_Evol_Vm_Cbriggsae_P3p_divided_P4p_SS_summary
    
  }
  
  
}

## ---- Vm_Caenorhabditis_species_summary_SS_vulva ----
Vm_Caenorhabditis_species_summary_SS_vulva <-  rbind.data.frame(Vm_Celegans_summary_SS_vulva,Vm_Cbriggsae_summary_SS)
View(Vm_Caenorhabditis_species_summary_SS_vulva)

names(Vm_Caenorhabditis_species_summary_SS_vulva)[names(Vm_Caenorhabditis_species_summary_SS_vulva) == "Models"] <- "Model_name"
names(Vm_Caenorhabditis_species_summary_SS_vulva)[names(Vm_Caenorhabditis_species_summary_SS_vulva) == "Treatment"] <- "Model_set"

write_xlsx(Vm_Caenorhabditis_species_summary_SS_vulva, "Vm_Caenorhabditis_species_summary_SS_vulva.xlsx")

Vm_Caenorhabditis_species_summary_SS_vulva_Vm <- subset(Vm_Caenorhabditis_species_summary_SS_vulva, Model_set=="Vm")
View(Vm_Caenorhabditis_species_summary_SS_vulva_Vm)
write_xlsx(Vm_Caenorhabditis_species_summary_SS_vulva_Vm, "Vm_Caenorhabditis_species_summary_SS_vulva_Vm.xlsx")



## ---- Vm_Caenorhabditis_species_P3p_divided_P4p_SS ----
va_h2_Evol_Vm_Caenorhabditis_species_P3p_divided_P4p_SS_summary <-  rbind.data.frame(va_h2_Evol_Vm_Celegans_P3p_divided_P4p_SS_summary,va_h2_Evol_Vm_Cbriggsae_P3p_divided_P4p_SS_summary)
write_xlsx(va_h2_Evol_Vm_Caenorhabditis_species_P3p_divided_P4p_SS_summary, "va_h2_Evol_Vm_Caenorhabditis_species_log10_P3p_divided_P4p_SS_summary.xlsx")


pdf("va_h2_Evol_Vm_Caenorhabditis_species_P3p_divided_P4p_SS_summary.pdf")
ggplot(va_h2_Evol_Vm_Caenorhabditis_species_P3p_divided_P4p_SS_summary, aes(x=Species, y= median)) +
  geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
  geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
  geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
  theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) +facet_grid(Scale~Measure) + theme(aspect.ratio=1) + labs(y = "Va log10 ( P3.p / P4.p)") 
dev.off()


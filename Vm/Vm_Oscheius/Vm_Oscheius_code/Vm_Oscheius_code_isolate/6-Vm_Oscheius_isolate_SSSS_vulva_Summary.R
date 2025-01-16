#Vm_Oscheius_isolate_SSSS_vulva_Summary


#---- CEW1 ----
{
  ##Summary CEW1 P3p----
  {
    #Summary liability scale CEW1 P3p
    {
      #Summary va_liab_CEW1_P3p_SSSS: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_CEW1_P3p_SSSS <- rbind(mean(va_liab_CEW1_CONTROL_P3p_SSSS_mod/2),mean(va_liab_CEW1_MA_P3p_SSSS_mod/2),mean(va_liab_CEW1_Vm_P3p_SSSS_mod/2))
        colnames(mean_va_liab_CEW1_P3p_SSSS) <- c("mean")
        median_va_liab_CEW1_P3p_SSSS <- rbind(median(va_liab_CEW1_CONTROL_P3p_SSSS_mod/2),median(va_liab_CEW1_MA_P3p_SSSS_mod/2),median(va_liab_CEW1_Vm_P3p_SSSS_mod/2))
        colnames(median_va_liab_CEW1_P3p_SSSS) <- c("median")
        posterior.mode_va_liab_CEW1_P3p_SSSS <- rbind(posterior.mode(va_liab_CEW1_CONTROL_P3p_SSSS_mod/2),posterior.mode(va_liab_CEW1_MA_P3p_SSSS_mod/2),posterior.mode(va_liab_CEW1_Vm_P3p_SSSS_mod/2))
        colnames(posterior.mode_va_liab_CEW1_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_CEW1_P3p_SSSS <- rbind(HPDinterval(va_liab_CEW1_CONTROL_P3p_SSSS_mod/2),HPDinterval(va_liab_CEW1_MA_P3p_SSSS_mod/2),HPDinterval(va_liab_CEW1_Vm_P3p_SSSS_mod/2))
        colnames(HPDinterval_0.95_va_liab_CEW1_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_CEW1_P3p_SSSS <- rbind(HPDinterval(va_liab_CEW1_CONTROL_P3p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_CEW1_MA_P3p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_CEW1_Vm_P3p_SSSS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_CEW1_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_CEW1_P3p_SSSS <- rbind(effectiveSize(va_liab_CEW1_CONTROL_P3p_SSSS_mod/2),effectiveSize(va_liab_CEW1_MA_P3p_SSSS_mod/2),effectiveSize(va_liab_CEW1_Vm_P3p_SSSS_mod/2))
        colnames(effectiveSize_va_liab_CEW1_P3p_SSSS) <- c("effectiveSize")
        va_liab_CEW1_P3p_SSSS <- cbind.data.frame(mean_va_liab_CEW1_P3p_SSSS,median_va_liab_CEW1_P3p_SSSS,posterior.mode_va_liab_CEW1_P3p_SSSS,HPDinterval_0.95_va_liab_CEW1_P3p_SSSS,HPDinterval_0.83_va_liab_CEW1_P3p_SSSS,effectiveSize_va_liab_CEW1_P3p_SSSS)
        rownames(va_liab_CEW1_P3p_SSSS) <- c("va_liab_CEW1_CONTROL_P3p_SSSS_mod","va_liab_CEW1_MA_P3p_SSSS_mod","va_liab_CEW1_Vm_P3p_SSSS_mod")
        va_liab_CEW1_P3p_SSSS <- cbind(Models = rownames(va_liab_CEW1_P3p_SSSS),va_liab_CEW1_P3p_SSSS)
        rownames(va_liab_CEW1_P3p_SSSS) <- NULL
        va_liab_CEW1_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        va_liab_CEW1_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        va_liab_CEW1_P3p_SSSS$Measure <- c("Va","Va","Va")
        va_liab_CEW1_P3p_SSSS$Scale <- c("liab","liab","liab")
        va_liab_CEW1_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_liab_CEW1_P3p_SSSS
      }
      
      #Summary h2_liab_CEW1_P3p_SSSS
      {
        mean_h2_liab_CEW1_P3p_SSSS <- rbind(mean(h2_liab_CEW1_CONTROL_P3p_SSSS_mod),mean(h2_liab_CEW1_MA_P3p_SSSS_mod),mean(h2_liab_CEW1_Vm_P3p_SSSS_mod))
        colnames(mean_h2_liab_CEW1_P3p_SSSS) <- c("mean")
        median_h2_liab_CEW1_P3p_SSSS <- rbind(median(h2_liab_CEW1_CONTROL_P3p_SSSS_mod),median(h2_liab_CEW1_MA_P3p_SSSS_mod),median(h2_liab_CEW1_Vm_P3p_SSSS_mod))
        colnames(median_h2_liab_CEW1_P3p_SSSS) <- c("median")
        posterior.mode_h2_liab_CEW1_P3p_SSSS <- rbind(posterior.mode(h2_liab_CEW1_CONTROL_P3p_SSSS_mod),posterior.mode(h2_liab_CEW1_MA_P3p_SSSS_mod),posterior.mode(h2_liab_CEW1_Vm_P3p_SSSS_mod))
        colnames(posterior.mode_h2_liab_CEW1_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_CEW1_P3p_SSSS <- rbind(HPDinterval(h2_liab_CEW1_CONTROL_P3p_SSSS_mod),HPDinterval(h2_liab_CEW1_MA_P3p_SSSS_mod),HPDinterval(h2_liab_CEW1_Vm_P3p_SSSS_mod))
        colnames(HPDinterval_0.95_h2_liab_CEW1_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_CEW1_P3p_SSSS <- rbind(HPDinterval(h2_liab_CEW1_CONTROL_P3p_SSSS_mod,prob=.83),HPDinterval(h2_liab_CEW1_MA_P3p_SSSS_mod,prob=.83),HPDinterval(h2_liab_CEW1_Vm_P3p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_CEW1_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_CEW1_P3p_SSSS <- rbind(effectiveSize(h2_liab_CEW1_CONTROL_P3p_SSSS_mod),effectiveSize(h2_liab_CEW1_MA_P3p_SSSS_mod),effectiveSize(h2_liab_CEW1_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_h2_liab_CEW1_P3p_SSSS) <- c("effectiveSize")
        h2_liab_CEW1_P3p_SSSS <- cbind.data.frame(mean_h2_liab_CEW1_P3p_SSSS,median_h2_liab_CEW1_P3p_SSSS,posterior.mode_h2_liab_CEW1_P3p_SSSS,HPDinterval_0.95_h2_liab_CEW1_P3p_SSSS,HPDinterval_0.83_h2_liab_CEW1_P3p_SSSS,effectiveSize_h2_liab_CEW1_P3p_SSSS)
        rownames(h2_liab_CEW1_P3p_SSSS) <- c("h2_liab_CEW1_CONTROL_P3p_SSSS_mod","h2_liab_CEW1_MA_P3p_SSSS_mod","h2_liab_CEW1_Vm_P3p_SSSS_mod")
        h2_liab_CEW1_P3p_SSSS <- cbind(Models = rownames(h2_liab_CEW1_P3p_SSSS),h2_liab_CEW1_P3p_SSSS)
        rownames(h2_liab_CEW1_P3p_SSSS) <- NULL
        h2_liab_CEW1_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        h2_liab_CEW1_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_liab_CEW1_P3p_SSSS$Measure <- c("H2","H2","H2")
        h2_liab_CEW1_P3p_SSSS$Scale <- c("liab","liab","liab")
        h2_liab_CEW1_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_liab_CEW1_P3p_SSSS
      }
      
      #Summary Evol_liab_CEW1_P3p_SSSS
      {
        mean_Evol_liab_CEW1_P3p_SSSS <- rbind(mean(Evol_liab_CEW1_CONTROL_P3p_SSSS_mod),mean(Evol_liab_CEW1_MA_P3p_SSSS_mod),mean(Evol_liab_CEW1_Vm_P3p_SSSS_mod))
        colnames(mean_Evol_liab_CEW1_P3p_SSSS) <- c("mean")
        median_Evol_liab_CEW1_P3p_SSSS <- rbind(median(Evol_liab_CEW1_CONTROL_P3p_SSSS_mod),median(Evol_liab_CEW1_MA_P3p_SSSS_mod),median(Evol_liab_CEW1_Vm_P3p_SSSS_mod))
        colnames(median_Evol_liab_CEW1_P3p_SSSS) <- c("median")
        posterior.mode_Evol_liab_CEW1_P3p_SSSS <- rbind(posterior.mode(Evol_liab_CEW1_CONTROL_P3p_SSSS_mod),posterior.mode(Evol_liab_CEW1_MA_P3p_SSSS_mod),posterior.mode(Evol_liab_CEW1_Vm_P3p_SSSS_mod))
        colnames(posterior.mode_Evol_liab_CEW1_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_CEW1_P3p_SSSS <- rbind(HPDinterval(Evol_liab_CEW1_CONTROL_P3p_SSSS_mod),HPDinterval(Evol_liab_CEW1_MA_P3p_SSSS_mod),HPDinterval(Evol_liab_CEW1_Vm_P3p_SSSS_mod))
        colnames(HPDinterval_0.95_Evol_liab_CEW1_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_CEW1_P3p_SSSS <- rbind(HPDinterval(Evol_liab_CEW1_CONTROL_P3p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_CEW1_MA_P3p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_CEW1_Vm_P3p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_CEW1_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_CEW1_P3p_SSSS <- rbind(effectiveSize(Evol_liab_CEW1_CONTROL_P3p_SSSS_mod),effectiveSize(Evol_liab_CEW1_MA_P3p_SSSS_mod),effectiveSize(Evol_liab_CEW1_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_Evol_liab_CEW1_P3p_SSSS) <- c("effectiveSize")
        Evol_liab_CEW1_P3p_SSSS <- cbind.data.frame(mean_Evol_liab_CEW1_P3p_SSSS,median_Evol_liab_CEW1_P3p_SSSS,posterior.mode_Evol_liab_CEW1_P3p_SSSS,HPDinterval_0.95_Evol_liab_CEW1_P3p_SSSS,HPDinterval_0.83_Evol_liab_CEW1_P3p_SSSS,effectiveSize_Evol_liab_CEW1_P3p_SSSS)
        rownames(Evol_liab_CEW1_P3p_SSSS) <- c("Evol_liab_CEW1_CONTROL_P3p_SSSS_mod","Evol_liab_CEW1_MA_P3p_SSSS_mod","Evol_liab_CEW1_Vm_P3p_SSSS_mod")
        Evol_liab_CEW1_P3p_SSSS <- cbind(Models = rownames(Evol_liab_CEW1_P3p_SSSS),Evol_liab_CEW1_P3p_SSSS)
        rownames(Evol_liab_CEW1_P3p_SSSS) <- NULL
        Evol_liab_CEW1_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        Evol_liab_CEW1_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_liab_CEW1_P3p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_CEW1_P3p_SSSS$Scale <- c("liab","liab","liab")
        Evol_liab_CEW1_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_CEW1_P3p_SSSS
      }
      
      #Summary trait_mean_liab_CEW1_P3p_SSSS
      {
        mean_trait_mean_liab_CEW1_P3p_SSSS <- rbind(mean(trait_mean_liab_CEW1_CONTROL_P3p_SSSS_mod),mean(trait_mean_liab_CEW1_MA_P3p_SSSS_mod),mean(trait_mean_liab_CEW1_Vm_P3p_SSSS_mod))
        colnames(mean_trait_mean_liab_CEW1_P3p_SSSS) <- c("mean")
        median_trait_mean_liab_CEW1_P3p_SSSS <- rbind(median(trait_mean_liab_CEW1_CONTROL_P3p_SSSS_mod),median(trait_mean_liab_CEW1_MA_P3p_SSSS_mod),median(trait_mean_liab_CEW1_Vm_P3p_SSSS_mod))
        colnames(median_trait_mean_liab_CEW1_P3p_SSSS) <- c("median")
        posterior.mode_trait_mean_liab_CEW1_P3p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_CEW1_CONTROL_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_CEW1_MA_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_CEW1_Vm_P3p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_liab_CEW1_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_CEW1_P3p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_CEW1_CONTROL_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_CEW1_MA_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_CEW1_Vm_P3p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_CEW1_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_CEW1_P3p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_CEW1_CONTROL_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_CEW1_MA_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_CEW1_Vm_P3p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_CEW1_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_CEW1_P3p_SSSS <- rbind(effectiveSize(trait_mean_liab_CEW1_CONTROL_P3p_SSSS_mod),effectiveSize(trait_mean_liab_CEW1_MA_P3p_SSSS_mod),effectiveSize(trait_mean_liab_CEW1_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_trait_mean_liab_CEW1_P3p_SSSS) <- c("effectiveSize")
        trait_mean_liab_CEW1_P3p_SSSS <- cbind.data.frame(mean_trait_mean_liab_CEW1_P3p_SSSS,median_trait_mean_liab_CEW1_P3p_SSSS,posterior.mode_trait_mean_liab_CEW1_P3p_SSSS,HPDinterval_0.95_trait_mean_liab_CEW1_P3p_SSSS,HPDinterval_0.83_trait_mean_liab_CEW1_P3p_SSSS,effectiveSize_trait_mean_liab_CEW1_P3p_SSSS)
        rownames(trait_mean_liab_CEW1_P3p_SSSS) <- c("trait_mean_liab_CEW1_CONTROL_P3p_SSSS_mod","trait_mean_liab_CEW1_MA_P3p_SSSS_mod","trait_mean_liab_CEW1_Vm_P3p_SSSS_mod")
        trait_mean_liab_CEW1_P3p_SSSS <- cbind(Models = rownames(trait_mean_liab_CEW1_P3p_SSSS),trait_mean_liab_CEW1_P3p_SSSS)
        rownames(trait_mean_liab_CEW1_P3p_SSSS) <- NULL
        trait_mean_liab_CEW1_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        trait_mean_liab_CEW1_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_CEW1_P3p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_CEW1_P3p_SSSS$Scale <- c("liab","liab","liab")
        trait_mean_liab_CEW1_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_CEW1_P3p_SSSS
      }
      
      liab_CEW1_P3p_SSSS <- rbind.data.frame(va_liab_CEW1_P3p_SSSS, h2_liab_CEW1_P3p_SSSS,Evol_liab_CEW1_P3p_SSSS,trait_mean_liab_CEW1_P3p_SSSS)
      liab_CEW1_P3p_SSSS
    }
    #Summary data scale CEW1 P3p
    {
      #Summary va_data_CEW1_P3p_SSSS:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_CEW1_P3p_SSSS <- rbind(mean(va_data_CEW1_CONTROL_P3p_SSSS_mod/2),mean(va_data_CEW1_MA_P3p_SSSS_mod/2),mean(va_data_CEW1_Vm_P3p_SSSS_mod/2))
        colnames(mean_va_data_CEW1_P3p_SSSS) <- c("mean")
        median_va_data_CEW1_P3p_SSSS <- rbind(median(va_data_CEW1_CONTROL_P3p_SSSS_mod/2),median(va_data_CEW1_MA_P3p_SSSS_mod/2),median(va_data_CEW1_Vm_P3p_SSSS_mod/2))
        colnames(median_va_data_CEW1_P3p_SSSS) <- c("median")
        posterior.mode_va_data_CEW1_P3p_SSSS <- rbind(posterior.mode(as.mcmc(va_data_CEW1_CONTROL_P3p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_CEW1_MA_P3p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_CEW1_Vm_P3p_SSSS_mod/2)))
        colnames(posterior.mode_va_data_CEW1_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_CEW1_P3p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_CEW1_CONTROL_P3p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_CEW1_MA_P3p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_CEW1_Vm_P3p_SSSS_mod/2)))
        colnames(HPDinterval_0.95_va_data_CEW1_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_CEW1_P3p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_CEW1_CONTROL_P3p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_CEW1_MA_P3p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_CEW1_Vm_P3p_SSSS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_CEW1_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_CEW1_P3p_SSSS <- rbind(effectiveSize(va_data_CEW1_CONTROL_P3p_SSSS_mod/2),effectiveSize(va_data_CEW1_MA_P3p_SSSS_mod/2),effectiveSize(va_data_CEW1_Vm_P3p_SSSS_mod/2))
        colnames(effectiveSize_va_data_CEW1_P3p_SSSS) <- c("effectiveSize")
        va_data_CEW1_P3p_SSSS <- cbind.data.frame(mean_va_data_CEW1_P3p_SSSS,median_va_data_CEW1_P3p_SSSS,posterior.mode_va_data_CEW1_P3p_SSSS,HPDinterval_0.95_va_data_CEW1_P3p_SSSS,HPDinterval_0.83_va_data_CEW1_P3p_SSSS,effectiveSize_va_data_CEW1_P3p_SSSS)
        rownames(va_data_CEW1_P3p_SSSS) <- c("va_data_CEW1_CONTROL_P3p_SSSS_mod","va_data_CEW1_MA_P3p_SSSS_mod","va_data_CEW1_Vm_P3p_SSSS_mod")
        va_data_CEW1_P3p_SSSS <- cbind(Models = rownames(va_data_CEW1_P3p_SSSS),va_data_CEW1_P3p_SSSS)
        rownames(va_data_CEW1_P3p_SSSS) <- NULL
        va_data_CEW1_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        va_data_CEW1_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        va_data_CEW1_P3p_SSSS$Measure <- c("Va","Va","Va")
        va_data_CEW1_P3p_SSSS$Scale <- c("data","data","data")
        va_data_CEW1_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_data_CEW1_P3p_SSSS
      }
      
      #Summary h2_data_CEW1_P3p_SSSS
      {
        mean_h2_data_CEW1_P3p_SSSS <- rbind(mean(h2_data_CEW1_CONTROL_P3p_SSSS_mod),mean(h2_data_CEW1_MA_P3p_SSSS_mod),mean(h2_data_CEW1_Vm_P3p_SSSS_mod))
        colnames(mean_h2_data_CEW1_P3p_SSSS) <- c("mean")
        median_h2_data_CEW1_P3p_SSSS <- rbind(median(h2_data_CEW1_CONTROL_P3p_SSSS_mod),median(h2_data_CEW1_MA_P3p_SSSS_mod),median(h2_data_CEW1_Vm_P3p_SSSS_mod))
        colnames(median_h2_data_CEW1_P3p_SSSS) <- c("median")
        posterior.mode_h2_data_CEW1_P3p_SSSS <- rbind(posterior.mode(as.mcmc(h2_data_CEW1_CONTROL_P3p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_CEW1_MA_P3p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_CEW1_Vm_P3p_SSSS_mod)))
        colnames(posterior.mode_h2_data_CEW1_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_CEW1_P3p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_CEW1_CONTROL_P3p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_CEW1_MA_P3p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_CEW1_Vm_P3p_SSSS_mod)))
        colnames(HPDinterval_0.95_h2_data_CEW1_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_CEW1_P3p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_CEW1_CONTROL_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_CEW1_MA_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_CEW1_Vm_P3p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_CEW1_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_CEW1_P3p_SSSS <- rbind(effectiveSize(h2_data_CEW1_CONTROL_P3p_SSSS_mod),effectiveSize(h2_data_CEW1_MA_P3p_SSSS_mod),effectiveSize(h2_data_CEW1_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_h2_data_CEW1_P3p_SSSS) <- c("effectiveSize")
        h2_data_CEW1_P3p_SSSS <- cbind.data.frame(mean_h2_data_CEW1_P3p_SSSS,median_h2_data_CEW1_P3p_SSSS,posterior.mode_h2_data_CEW1_P3p_SSSS,HPDinterval_0.95_h2_data_CEW1_P3p_SSSS,HPDinterval_0.83_h2_data_CEW1_P3p_SSSS,effectiveSize_h2_data_CEW1_P3p_SSSS)
        rownames(h2_data_CEW1_P3p_SSSS) <- c("h2_data_CEW1_CONTROL_P3p_SSSS_mod","h2_data_CEW1_MA_P3p_SSSS_mod","h2_data_CEW1_Vm_P3p_SSSS_mod")
        h2_data_CEW1_P3p_SSSS <- cbind(Models = rownames(h2_data_CEW1_P3p_SSSS),h2_data_CEW1_P3p_SSSS)
        rownames(h2_data_CEW1_P3p_SSSS) <- NULL
        h2_data_CEW1_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        h2_data_CEW1_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_data_CEW1_P3p_SSSS$Measure <- c("H2","H2","H2")
        h2_data_CEW1_P3p_SSSS$Scale <- c("data","data","data")
        h2_data_CEW1_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_data_CEW1_P3p_SSSS
      }
      
      #Summary Evol_data_CEW1_P3p_SSSS
      {
        mean_Evol_data_CEW1_P3p_SSSS <- rbind(mean(Evol_data_CEW1_CONTROL_P3p_SSSS_mod),mean(Evol_data_CEW1_MA_P3p_SSSS_mod),mean(Evol_data_CEW1_Vm_P3p_SSSS_mod))
        colnames(mean_Evol_data_CEW1_P3p_SSSS) <- c("mean")
        median_Evol_data_CEW1_P3p_SSSS <- rbind(median(Evol_data_CEW1_CONTROL_P3p_SSSS_mod),median(Evol_data_CEW1_MA_P3p_SSSS_mod),median(Evol_data_CEW1_Vm_P3p_SSSS_mod))
        colnames(median_Evol_data_CEW1_P3p_SSSS) <- c("median")
        posterior.mode_Evol_data_CEW1_P3p_SSSS <- rbind(posterior.mode(as.mcmc(Evol_data_CEW1_CONTROL_P3p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_CEW1_MA_P3p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_CEW1_Vm_P3p_SSSS_mod)))
        colnames(posterior.mode_Evol_data_CEW1_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_CEW1_P3p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_CEW1_CONTROL_P3p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_CEW1_MA_P3p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_CEW1_Vm_P3p_SSSS_mod)))
        colnames(HPDinterval_0.95_Evol_data_CEW1_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_CEW1_P3p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_CEW1_CONTROL_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_CEW1_MA_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_CEW1_Vm_P3p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_CEW1_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_CEW1_P3p_SSSS <- rbind(effectiveSize(Evol_data_CEW1_CONTROL_P3p_SSSS_mod),effectiveSize(Evol_data_CEW1_MA_P3p_SSSS_mod),effectiveSize(Evol_data_CEW1_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_Evol_data_CEW1_P3p_SSSS) <- c("effectiveSize")
        Evol_data_CEW1_P3p_SSSS <- cbind.data.frame(mean_Evol_data_CEW1_P3p_SSSS,median_Evol_data_CEW1_P3p_SSSS,posterior.mode_Evol_data_CEW1_P3p_SSSS,HPDinterval_0.95_Evol_data_CEW1_P3p_SSSS,HPDinterval_0.83_Evol_data_CEW1_P3p_SSSS,effectiveSize_Evol_data_CEW1_P3p_SSSS)
        rownames(Evol_data_CEW1_P3p_SSSS) <- c("Evol_data_CEW1_CONTROL_P3p_SSSS_mod","Evol_data_CEW1_MA_P3p_SSSS_mod","Evol_data_CEW1_Vm_P3p_SSSS_mod")
        Evol_data_CEW1_P3p_SSSS <- cbind(Models = rownames(Evol_data_CEW1_P3p_SSSS),Evol_data_CEW1_P3p_SSSS)
        rownames(Evol_data_CEW1_P3p_SSSS) <- NULL
        Evol_data_CEW1_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        Evol_data_CEW1_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_data_CEW1_P3p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_data_CEW1_P3p_SSSS$Scale <- c("data","data","data")
        Evol_data_CEW1_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_data_CEW1_P3p_SSSS
      }
      
      #Summary trait_mean_data_CEW1_P3p_SSSS
      {
        mean_trait_mean_data_CEW1_P3p_SSSS <- rbind(mean(trait_mean_data_CEW1_CONTROL_P3p_SSSS_mod),mean(trait_mean_data_CEW1_MA_P3p_SSSS_mod),mean(trait_mean_data_CEW1_Vm_P3p_SSSS_mod))
        colnames(mean_trait_mean_data_CEW1_P3p_SSSS) <- c("mean")
        median_trait_mean_data_CEW1_P3p_SSSS <- rbind(median(trait_mean_data_CEW1_CONTROL_P3p_SSSS_mod),median(trait_mean_data_CEW1_MA_P3p_SSSS_mod),median(trait_mean_data_CEW1_Vm_P3p_SSSS_mod))
        colnames(median_trait_mean_data_CEW1_P3p_SSSS) <- c("median")
        posterior.mode_trait_mean_data_CEW1_P3p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_data_CEW1_CONTROL_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_CEW1_MA_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_CEW1_Vm_P3p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_data_CEW1_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_CEW1_P3p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_CEW1_CONTROL_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_CEW1_MA_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_CEW1_Vm_P3p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_CEW1_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_CEW1_P3p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_CEW1_CONTROL_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_CEW1_MA_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_CEW1_Vm_P3p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_CEW1_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_CEW1_P3p_SSSS <- rbind(effectiveSize(trait_mean_data_CEW1_CONTROL_P3p_SSSS_mod),effectiveSize(trait_mean_data_CEW1_MA_P3p_SSSS_mod),effectiveSize(trait_mean_data_CEW1_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_trait_mean_data_CEW1_P3p_SSSS) <- c("effectiveSize")
        trait_mean_data_CEW1_P3p_SSSS <- cbind.data.frame(mean_trait_mean_data_CEW1_P3p_SSSS,median_trait_mean_data_CEW1_P3p_SSSS,posterior.mode_trait_mean_data_CEW1_P3p_SSSS,HPDinterval_0.95_trait_mean_data_CEW1_P3p_SSSS,HPDinterval_0.83_trait_mean_data_CEW1_P3p_SSSS,effectiveSize_trait_mean_data_CEW1_P3p_SSSS)
        rownames(trait_mean_data_CEW1_P3p_SSSS) <- c("trait_mean_data_CEW1_CONTROL_P3p_SSSS_mod","trait_mean_data_CEW1_MA_P3p_SSSS_mod","trait_mean_data_CEW1_Vm_P3p_SSSS_mod")
        trait_mean_data_CEW1_P3p_SSSS <- cbind(Models = rownames(trait_mean_data_CEW1_P3p_SSSS),trait_mean_data_CEW1_P3p_SSSS)
        rownames(trait_mean_data_CEW1_P3p_SSSS) <- NULL
        trait_mean_data_CEW1_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        trait_mean_data_CEW1_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_data_CEW1_P3p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_CEW1_P3p_SSSS$Scale <- c("data","data","data")
        trait_mean_data_CEW1_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_CEW1_P3p_SSSS
      }
      
      data_CEW1_P3p_SSSS <- rbind.data.frame(va_data_CEW1_P3p_SSSS, h2_data_CEW1_P3p_SSSS,Evol_data_CEW1_P3p_SSSS,trait_mean_data_CEW1_P3p_SSSS)
      data_CEW1_P3p_SSSS
      
    }
    Vm_CEW1_P3p_SSSS <- rbind.data.frame(liab_CEW1_P3p_SSSS, data_CEW1_P3p_SSSS)
    Vm_CEW1_P3p_SSSS$Pnp_fate <- rep("SSSS", 24)
    Vm_CEW1_P3p_SSSS
    #remove CEW1 P3p_SSSSS models
    {
      remove(CEW1_CONTROL_P3p_SSSS_mod)
      remove(CEW1_MA_P3p_SSSS_mod)
      remove(CEW1_Vm_P3p_SSSS_mod)
    }
  }
  
  ##Summary CEW1 P4p----
  {
    #Summary liability scale CEW1 P4p
    {
      #Summary va_liab_CEW1_P4p_SSSS: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_CEW1_P4p_SSSS <- rbind(mean(va_liab_CEW1_CONTROL_P4p_SSSS_mod/2),mean(va_liab_CEW1_MA_P4p_SSSS_mod/2),mean(va_liab_CEW1_Vm_P4p_SSSS_mod/2))
        colnames(mean_va_liab_CEW1_P4p_SSSS) <- c("mean")
        median_va_liab_CEW1_P4p_SSSS <- rbind(median(va_liab_CEW1_CONTROL_P4p_SSSS_mod/2),median(va_liab_CEW1_MA_P4p_SSSS_mod/2),median(va_liab_CEW1_Vm_P4p_SSSS_mod/2))
        colnames(median_va_liab_CEW1_P4p_SSSS) <- c("median")
        posterior.mode_va_liab_CEW1_P4p_SSSS <- rbind(posterior.mode(va_liab_CEW1_CONTROL_P4p_SSSS_mod/2),posterior.mode(va_liab_CEW1_MA_P4p_SSSS_mod/2),posterior.mode(va_liab_CEW1_Vm_P4p_SSSS_mod/2))
        colnames(posterior.mode_va_liab_CEW1_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_CEW1_P4p_SSSS <- rbind(HPDinterval(va_liab_CEW1_CONTROL_P4p_SSSS_mod/2),HPDinterval(va_liab_CEW1_MA_P4p_SSSS_mod/2),HPDinterval(va_liab_CEW1_Vm_P4p_SSSS_mod/2))
        colnames(HPDinterval_0.95_va_liab_CEW1_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_CEW1_P4p_SSSS <- rbind(HPDinterval(va_liab_CEW1_CONTROL_P4p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_CEW1_MA_P4p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_CEW1_Vm_P4p_SSSS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_CEW1_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_CEW1_P4p_SSSS <- rbind(effectiveSize(va_liab_CEW1_CONTROL_P4p_SSSS_mod/2),effectiveSize(va_liab_CEW1_MA_P4p_SSSS_mod/2),effectiveSize(va_liab_CEW1_Vm_P4p_SSSS_mod/2))
        colnames(effectiveSize_va_liab_CEW1_P4p_SSSS) <- c("effectiveSize")
        va_liab_CEW1_P4p_SSSS <- cbind.data.frame(mean_va_liab_CEW1_P4p_SSSS,median_va_liab_CEW1_P4p_SSSS,posterior.mode_va_liab_CEW1_P4p_SSSS,HPDinterval_0.95_va_liab_CEW1_P4p_SSSS,HPDinterval_0.83_va_liab_CEW1_P4p_SSSS,effectiveSize_va_liab_CEW1_P4p_SSSS)
        rownames(va_liab_CEW1_P4p_SSSS) <- c("va_liab_CEW1_CONTROL_P4p_SSSS_mod","va_liab_CEW1_MA_P4p_SSSS_mod","va_liab_CEW1_Vm_P4p_SSSS_mod")
        va_liab_CEW1_P4p_SSSS <- cbind(Models = rownames(va_liab_CEW1_P4p_SSSS),va_liab_CEW1_P4p_SSSS)
        rownames(va_liab_CEW1_P4p_SSSS) <- NULL
        va_liab_CEW1_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        va_liab_CEW1_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        va_liab_CEW1_P4p_SSSS$Measure <- c("Va","Va","Va")
        va_liab_CEW1_P4p_SSSS$Scale <- c("liab","liab","liab")
        va_liab_CEW1_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_liab_CEW1_P4p_SSSS
      }
      
      #Summary h2_liab_CEW1_P4p_SSSS
      {
        mean_h2_liab_CEW1_P4p_SSSS <- rbind(mean(h2_liab_CEW1_CONTROL_P4p_SSSS_mod),mean(h2_liab_CEW1_MA_P4p_SSSS_mod),mean(h2_liab_CEW1_Vm_P4p_SSSS_mod))
        colnames(mean_h2_liab_CEW1_P4p_SSSS) <- c("mean")
        median_h2_liab_CEW1_P4p_SSSS <- rbind(median(h2_liab_CEW1_CONTROL_P4p_SSSS_mod),median(h2_liab_CEW1_MA_P4p_SSSS_mod),median(h2_liab_CEW1_Vm_P4p_SSSS_mod))
        colnames(median_h2_liab_CEW1_P4p_SSSS) <- c("median")
        posterior.mode_h2_liab_CEW1_P4p_SSSS <- rbind(posterior.mode(h2_liab_CEW1_CONTROL_P4p_SSSS_mod),posterior.mode(h2_liab_CEW1_MA_P4p_SSSS_mod),posterior.mode(h2_liab_CEW1_Vm_P4p_SSSS_mod))
        colnames(posterior.mode_h2_liab_CEW1_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_CEW1_P4p_SSSS <- rbind(HPDinterval(h2_liab_CEW1_CONTROL_P4p_SSSS_mod),HPDinterval(h2_liab_CEW1_MA_P4p_SSSS_mod),HPDinterval(h2_liab_CEW1_Vm_P4p_SSSS_mod))
        colnames(HPDinterval_0.95_h2_liab_CEW1_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_CEW1_P4p_SSSS <- rbind(HPDinterval(h2_liab_CEW1_CONTROL_P4p_SSSS_mod,prob=.83),HPDinterval(h2_liab_CEW1_MA_P4p_SSSS_mod,prob=.83),HPDinterval(h2_liab_CEW1_Vm_P4p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_CEW1_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_CEW1_P4p_SSSS <- rbind(effectiveSize(h2_liab_CEW1_CONTROL_P4p_SSSS_mod),effectiveSize(h2_liab_CEW1_MA_P4p_SSSS_mod),effectiveSize(h2_liab_CEW1_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_h2_liab_CEW1_P4p_SSSS) <- c("effectiveSize")
        h2_liab_CEW1_P4p_SSSS <- cbind.data.frame(mean_h2_liab_CEW1_P4p_SSSS,median_h2_liab_CEW1_P4p_SSSS,posterior.mode_h2_liab_CEW1_P4p_SSSS,HPDinterval_0.95_h2_liab_CEW1_P4p_SSSS,HPDinterval_0.83_h2_liab_CEW1_P4p_SSSS,effectiveSize_h2_liab_CEW1_P4p_SSSS)
        rownames(h2_liab_CEW1_P4p_SSSS) <- c("h2_liab_CEW1_CONTROL_P4p_SSSS_mod","h2_liab_CEW1_MA_P4p_SSSS_mod","h2_liab_CEW1_Vm_P4p_SSSS_mod")
        h2_liab_CEW1_P4p_SSSS <- cbind(Models = rownames(h2_liab_CEW1_P4p_SSSS),h2_liab_CEW1_P4p_SSSS)
        rownames(h2_liab_CEW1_P4p_SSSS) <- NULL
        h2_liab_CEW1_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        h2_liab_CEW1_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_liab_CEW1_P4p_SSSS$Measure <- c("H2","H2","H2")
        h2_liab_CEW1_P4p_SSSS$Scale <- c("liab","liab","liab")
        h2_liab_CEW1_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_liab_CEW1_P4p_SSSS
      }
      
      #Summary Evol_liab_CEW1_P4p_SSSS
      {
        mean_Evol_liab_CEW1_P4p_SSSS <- rbind(mean(Evol_liab_CEW1_CONTROL_P4p_SSSS_mod),mean(Evol_liab_CEW1_MA_P4p_SSSS_mod),mean(Evol_liab_CEW1_Vm_P4p_SSSS_mod))
        colnames(mean_Evol_liab_CEW1_P4p_SSSS) <- c("mean")
        median_Evol_liab_CEW1_P4p_SSSS <- rbind(median(Evol_liab_CEW1_CONTROL_P4p_SSSS_mod),median(Evol_liab_CEW1_MA_P4p_SSSS_mod),median(Evol_liab_CEW1_Vm_P4p_SSSS_mod))
        colnames(median_Evol_liab_CEW1_P4p_SSSS) <- c("median")
        posterior.mode_Evol_liab_CEW1_P4p_SSSS <- rbind(posterior.mode(Evol_liab_CEW1_CONTROL_P4p_SSSS_mod),posterior.mode(Evol_liab_CEW1_MA_P4p_SSSS_mod),posterior.mode(Evol_liab_CEW1_Vm_P4p_SSSS_mod))
        colnames(posterior.mode_Evol_liab_CEW1_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_CEW1_P4p_SSSS <- rbind(HPDinterval(Evol_liab_CEW1_CONTROL_P4p_SSSS_mod),HPDinterval(Evol_liab_CEW1_MA_P4p_SSSS_mod),HPDinterval(Evol_liab_CEW1_Vm_P4p_SSSS_mod))
        colnames(HPDinterval_0.95_Evol_liab_CEW1_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_CEW1_P4p_SSSS <- rbind(HPDinterval(Evol_liab_CEW1_CONTROL_P4p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_CEW1_MA_P4p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_CEW1_Vm_P4p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_CEW1_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_CEW1_P4p_SSSS <- rbind(effectiveSize(Evol_liab_CEW1_CONTROL_P4p_SSSS_mod),effectiveSize(Evol_liab_CEW1_MA_P4p_SSSS_mod),effectiveSize(Evol_liab_CEW1_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_Evol_liab_CEW1_P4p_SSSS) <- c("effectiveSize")
        Evol_liab_CEW1_P4p_SSSS <- cbind.data.frame(mean_Evol_liab_CEW1_P4p_SSSS,median_Evol_liab_CEW1_P4p_SSSS,posterior.mode_Evol_liab_CEW1_P4p_SSSS,HPDinterval_0.95_Evol_liab_CEW1_P4p_SSSS,HPDinterval_0.83_Evol_liab_CEW1_P4p_SSSS,effectiveSize_Evol_liab_CEW1_P4p_SSSS)
        rownames(Evol_liab_CEW1_P4p_SSSS) <- c("Evol_liab_CEW1_CONTROL_P4p_SSSS_mod","Evol_liab_CEW1_MA_P4p_SSSS_mod","Evol_liab_CEW1_Vm_P4p_SSSS_mod")
        Evol_liab_CEW1_P4p_SSSS <- cbind(Models = rownames(Evol_liab_CEW1_P4p_SSSS),Evol_liab_CEW1_P4p_SSSS)
        rownames(Evol_liab_CEW1_P4p_SSSS) <- NULL
        Evol_liab_CEW1_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        Evol_liab_CEW1_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_liab_CEW1_P4p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_CEW1_P4p_SSSS$Scale <- c("liab","liab","liab")
        Evol_liab_CEW1_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_CEW1_P4p_SSSS
      }
      
      #Summary trait_mean_liab_CEW1_P4p_SSSS
      {
        mean_trait_mean_liab_CEW1_P4p_SSSS <- rbind(mean(trait_mean_liab_CEW1_CONTROL_P4p_SSSS_mod),mean(trait_mean_liab_CEW1_MA_P4p_SSSS_mod),mean(trait_mean_liab_CEW1_Vm_P4p_SSSS_mod))
        colnames(mean_trait_mean_liab_CEW1_P4p_SSSS) <- c("mean")
        median_trait_mean_liab_CEW1_P4p_SSSS <- rbind(median(trait_mean_liab_CEW1_CONTROL_P4p_SSSS_mod),median(trait_mean_liab_CEW1_MA_P4p_SSSS_mod),median(trait_mean_liab_CEW1_Vm_P4p_SSSS_mod))
        colnames(median_trait_mean_liab_CEW1_P4p_SSSS) <- c("median")
        posterior.mode_trait_mean_liab_CEW1_P4p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_CEW1_CONTROL_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_CEW1_MA_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_CEW1_Vm_P4p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_liab_CEW1_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_CEW1_P4p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_CEW1_CONTROL_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_CEW1_MA_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_CEW1_Vm_P4p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_CEW1_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_CEW1_P4p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_CEW1_CONTROL_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_CEW1_MA_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_CEW1_Vm_P4p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_CEW1_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_CEW1_P4p_SSSS <- rbind(effectiveSize(trait_mean_liab_CEW1_CONTROL_P4p_SSSS_mod),effectiveSize(trait_mean_liab_CEW1_MA_P4p_SSSS_mod),effectiveSize(trait_mean_liab_CEW1_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_trait_mean_liab_CEW1_P4p_SSSS) <- c("effectiveSize")
        trait_mean_liab_CEW1_P4p_SSSS <- cbind.data.frame(mean_trait_mean_liab_CEW1_P4p_SSSS,median_trait_mean_liab_CEW1_P4p_SSSS,posterior.mode_trait_mean_liab_CEW1_P4p_SSSS,HPDinterval_0.95_trait_mean_liab_CEW1_P4p_SSSS,HPDinterval_0.83_trait_mean_liab_CEW1_P4p_SSSS,effectiveSize_trait_mean_liab_CEW1_P4p_SSSS)
        rownames(trait_mean_liab_CEW1_P4p_SSSS) <- c("trait_mean_liab_CEW1_CONTROL_P4p_SSSS_mod","trait_mean_liab_CEW1_MA_P4p_SSSS_mod","trait_mean_liab_CEW1_Vm_P4p_SSSS_mod")
        trait_mean_liab_CEW1_P4p_SSSS <- cbind(Models = rownames(trait_mean_liab_CEW1_P4p_SSSS),trait_mean_liab_CEW1_P4p_SSSS)
        rownames(trait_mean_liab_CEW1_P4p_SSSS) <- NULL
        trait_mean_liab_CEW1_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        trait_mean_liab_CEW1_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_CEW1_P4p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_CEW1_P4p_SSSS$Scale <- c("liab","liab","liab")
        trait_mean_liab_CEW1_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_CEW1_P4p_SSSS
      }
      
      liab_CEW1_P4p_SSSS <- rbind.data.frame(va_liab_CEW1_P4p_SSSS, h2_liab_CEW1_P4p_SSSS,Evol_liab_CEW1_P4p_SSSS,trait_mean_liab_CEW1_P4p_SSSS)
      liab_CEW1_P4p_SSSS
    }
    #Summary data scale CEW1 P4p
    {
      #Summary va_data_CEW1_P4p_SSSS:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_CEW1_P4p_SSSS <- rbind(mean(va_data_CEW1_CONTROL_P4p_SSSS_mod/2),mean(va_data_CEW1_MA_P4p_SSSS_mod/2),mean(va_data_CEW1_Vm_P4p_SSSS_mod/2))
        colnames(mean_va_data_CEW1_P4p_SSSS) <- c("mean")
        median_va_data_CEW1_P4p_SSSS <- rbind(median(va_data_CEW1_CONTROL_P4p_SSSS_mod/2),median(va_data_CEW1_MA_P4p_SSSS_mod/2),median(va_data_CEW1_Vm_P4p_SSSS_mod/2))
        colnames(median_va_data_CEW1_P4p_SSSS) <- c("median")
        posterior.mode_va_data_CEW1_P4p_SSSS <- rbind(posterior.mode(as.mcmc(va_data_CEW1_CONTROL_P4p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_CEW1_MA_P4p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_CEW1_Vm_P4p_SSSS_mod/2)))
        colnames(posterior.mode_va_data_CEW1_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_CEW1_P4p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_CEW1_CONTROL_P4p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_CEW1_MA_P4p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_CEW1_Vm_P4p_SSSS_mod/2)))
        colnames(HPDinterval_0.95_va_data_CEW1_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_CEW1_P4p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_CEW1_CONTROL_P4p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_CEW1_MA_P4p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_CEW1_Vm_P4p_SSSS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_CEW1_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_CEW1_P4p_SSSS <- rbind(effectiveSize(va_data_CEW1_CONTROL_P4p_SSSS_mod/2),effectiveSize(va_data_CEW1_MA_P4p_SSSS_mod/2),effectiveSize(va_data_CEW1_Vm_P4p_SSSS_mod/2))
        colnames(effectiveSize_va_data_CEW1_P4p_SSSS) <- c("effectiveSize")
        va_data_CEW1_P4p_SSSS <- cbind.data.frame(mean_va_data_CEW1_P4p_SSSS,median_va_data_CEW1_P4p_SSSS,posterior.mode_va_data_CEW1_P4p_SSSS,HPDinterval_0.95_va_data_CEW1_P4p_SSSS,HPDinterval_0.83_va_data_CEW1_P4p_SSSS,effectiveSize_va_data_CEW1_P4p_SSSS)
        rownames(va_data_CEW1_P4p_SSSS) <- c("va_data_CEW1_CONTROL_P4p_SSSS_mod","va_data_CEW1_MA_P4p_SSSS_mod","va_data_CEW1_Vm_P4p_SSSS_mod")
        va_data_CEW1_P4p_SSSS <- cbind(Models = rownames(va_data_CEW1_P4p_SSSS),va_data_CEW1_P4p_SSSS)
        rownames(va_data_CEW1_P4p_SSSS) <- NULL
        va_data_CEW1_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        va_data_CEW1_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        va_data_CEW1_P4p_SSSS$Measure <- c("Va","Va","Va")
        va_data_CEW1_P4p_SSSS$Scale <- c("data","data","data")
        va_data_CEW1_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_data_CEW1_P4p_SSSS
      }
      
      #Summary h2_data_CEW1_P4p_SSSS
      {
        mean_h2_data_CEW1_P4p_SSSS <- rbind(mean(h2_data_CEW1_CONTROL_P4p_SSSS_mod),mean(h2_data_CEW1_MA_P4p_SSSS_mod),mean(h2_data_CEW1_Vm_P4p_SSSS_mod))
        colnames(mean_h2_data_CEW1_P4p_SSSS) <- c("mean")
        median_h2_data_CEW1_P4p_SSSS <- rbind(median(h2_data_CEW1_CONTROL_P4p_SSSS_mod),median(h2_data_CEW1_MA_P4p_SSSS_mod),median(h2_data_CEW1_Vm_P4p_SSSS_mod))
        colnames(median_h2_data_CEW1_P4p_SSSS) <- c("median")
        posterior.mode_h2_data_CEW1_P4p_SSSS <- rbind(posterior.mode(as.mcmc(h2_data_CEW1_CONTROL_P4p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_CEW1_MA_P4p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_CEW1_Vm_P4p_SSSS_mod)))
        colnames(posterior.mode_h2_data_CEW1_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_CEW1_P4p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_CEW1_CONTROL_P4p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_CEW1_MA_P4p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_CEW1_Vm_P4p_SSSS_mod)))
        colnames(HPDinterval_0.95_h2_data_CEW1_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_CEW1_P4p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_CEW1_CONTROL_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_CEW1_MA_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_CEW1_Vm_P4p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_CEW1_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_CEW1_P4p_SSSS <- rbind(effectiveSize(h2_data_CEW1_CONTROL_P4p_SSSS_mod),effectiveSize(h2_data_CEW1_MA_P4p_SSSS_mod),effectiveSize(h2_data_CEW1_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_h2_data_CEW1_P4p_SSSS) <- c("effectiveSize")
        h2_data_CEW1_P4p_SSSS <- cbind.data.frame(mean_h2_data_CEW1_P4p_SSSS,median_h2_data_CEW1_P4p_SSSS,posterior.mode_h2_data_CEW1_P4p_SSSS,HPDinterval_0.95_h2_data_CEW1_P4p_SSSS,HPDinterval_0.83_h2_data_CEW1_P4p_SSSS,effectiveSize_h2_data_CEW1_P4p_SSSS)
        rownames(h2_data_CEW1_P4p_SSSS) <- c("h2_data_CEW1_CONTROL_P4p_SSSS_mod","h2_data_CEW1_MA_P4p_SSSS_mod","h2_data_CEW1_Vm_P4p_SSSS_mod")
        h2_data_CEW1_P4p_SSSS <- cbind(Models = rownames(h2_data_CEW1_P4p_SSSS),h2_data_CEW1_P4p_SSSS)
        rownames(h2_data_CEW1_P4p_SSSS) <- NULL
        h2_data_CEW1_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        h2_data_CEW1_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_data_CEW1_P4p_SSSS$Measure <- c("H2","H2","H2")
        h2_data_CEW1_P4p_SSSS$Scale <- c("data","data","data")
        h2_data_CEW1_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_data_CEW1_P4p_SSSS
      }
      
      #Summary Evol_data_CEW1_P4p_SSSS
      {
        mean_Evol_data_CEW1_P4p_SSSS <- rbind(mean(Evol_data_CEW1_CONTROL_P4p_SSSS_mod),mean(Evol_data_CEW1_MA_P4p_SSSS_mod),mean(Evol_data_CEW1_Vm_P4p_SSSS_mod))
        colnames(mean_Evol_data_CEW1_P4p_SSSS) <- c("mean")
        median_Evol_data_CEW1_P4p_SSSS <- rbind(median(Evol_data_CEW1_CONTROL_P4p_SSSS_mod),median(Evol_data_CEW1_MA_P4p_SSSS_mod),median(Evol_data_CEW1_Vm_P4p_SSSS_mod))
        colnames(median_Evol_data_CEW1_P4p_SSSS) <- c("median")
        posterior.mode_Evol_data_CEW1_P4p_SSSS <- rbind(posterior.mode(as.mcmc(Evol_data_CEW1_CONTROL_P4p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_CEW1_MA_P4p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_CEW1_Vm_P4p_SSSS_mod)))
        colnames(posterior.mode_Evol_data_CEW1_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_CEW1_P4p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_CEW1_CONTROL_P4p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_CEW1_MA_P4p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_CEW1_Vm_P4p_SSSS_mod)))
        colnames(HPDinterval_0.95_Evol_data_CEW1_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_CEW1_P4p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_CEW1_CONTROL_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_CEW1_MA_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_CEW1_Vm_P4p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_CEW1_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_CEW1_P4p_SSSS <- rbind(effectiveSize(Evol_data_CEW1_CONTROL_P4p_SSSS_mod),effectiveSize(Evol_data_CEW1_MA_P4p_SSSS_mod),effectiveSize(Evol_data_CEW1_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_Evol_data_CEW1_P4p_SSSS) <- c("effectiveSize")
        Evol_data_CEW1_P4p_SSSS <- cbind.data.frame(mean_Evol_data_CEW1_P4p_SSSS,median_Evol_data_CEW1_P4p_SSSS,posterior.mode_Evol_data_CEW1_P4p_SSSS,HPDinterval_0.95_Evol_data_CEW1_P4p_SSSS,HPDinterval_0.83_Evol_data_CEW1_P4p_SSSS,effectiveSize_Evol_data_CEW1_P4p_SSSS)
        rownames(Evol_data_CEW1_P4p_SSSS) <- c("Evol_data_CEW1_CONTROL_P4p_SSSS_mod","Evol_data_CEW1_MA_P4p_SSSS_mod","Evol_data_CEW1_Vm_P4p_SSSS_mod")
        Evol_data_CEW1_P4p_SSSS <- cbind(Models = rownames(Evol_data_CEW1_P4p_SSSS),Evol_data_CEW1_P4p_SSSS)
        rownames(Evol_data_CEW1_P4p_SSSS) <- NULL
        Evol_data_CEW1_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        Evol_data_CEW1_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_data_CEW1_P4p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_data_CEW1_P4p_SSSS$Scale <- c("data","data","data")
        Evol_data_CEW1_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_data_CEW1_P4p_SSSS
      }
      
      #Summary trait_mean_data_CEW1_P4p_SSSS
      {
        mean_trait_mean_data_CEW1_P4p_SSSS <- rbind(mean(trait_mean_data_CEW1_CONTROL_P4p_SSSS_mod),mean(trait_mean_data_CEW1_MA_P4p_SSSS_mod),mean(trait_mean_data_CEW1_Vm_P4p_SSSS_mod))
        colnames(mean_trait_mean_data_CEW1_P4p_SSSS) <- c("mean")
        median_trait_mean_data_CEW1_P4p_SSSS <- rbind(median(trait_mean_data_CEW1_CONTROL_P4p_SSSS_mod),median(trait_mean_data_CEW1_MA_P4p_SSSS_mod),median(trait_mean_data_CEW1_Vm_P4p_SSSS_mod))
        colnames(median_trait_mean_data_CEW1_P4p_SSSS) <- c("median")
        posterior.mode_trait_mean_data_CEW1_P4p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_data_CEW1_CONTROL_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_CEW1_MA_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_CEW1_Vm_P4p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_data_CEW1_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_CEW1_P4p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_CEW1_CONTROL_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_CEW1_MA_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_CEW1_Vm_P4p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_CEW1_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_CEW1_P4p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_CEW1_CONTROL_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_CEW1_MA_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_CEW1_Vm_P4p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_CEW1_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_CEW1_P4p_SSSS <- rbind(effectiveSize(trait_mean_data_CEW1_CONTROL_P4p_SSSS_mod),effectiveSize(trait_mean_data_CEW1_MA_P4p_SSSS_mod),effectiveSize(trait_mean_data_CEW1_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_trait_mean_data_CEW1_P4p_SSSS) <- c("effectiveSize")
        trait_mean_data_CEW1_P4p_SSSS <- cbind.data.frame(mean_trait_mean_data_CEW1_P4p_SSSS,median_trait_mean_data_CEW1_P4p_SSSS,posterior.mode_trait_mean_data_CEW1_P4p_SSSS,HPDinterval_0.95_trait_mean_data_CEW1_P4p_SSSS,HPDinterval_0.83_trait_mean_data_CEW1_P4p_SSSS,effectiveSize_trait_mean_data_CEW1_P4p_SSSS)
        rownames(trait_mean_data_CEW1_P4p_SSSS) <- c("trait_mean_data_CEW1_CONTROL_P4p_SSSS_mod","trait_mean_data_CEW1_MA_P4p_SSSS_mod","trait_mean_data_CEW1_Vm_P4p_SSSS_mod")
        trait_mean_data_CEW1_P4p_SSSS <- cbind(Models = rownames(trait_mean_data_CEW1_P4p_SSSS),trait_mean_data_CEW1_P4p_SSSS)
        rownames(trait_mean_data_CEW1_P4p_SSSS) <- NULL
        trait_mean_data_CEW1_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        trait_mean_data_CEW1_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_data_CEW1_P4p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_CEW1_P4p_SSSS$Scale <- c("data","data","data")
        trait_mean_data_CEW1_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_CEW1_P4p_SSSS
      }
      
      data_CEW1_P4p_SSSS <- rbind.data.frame(va_data_CEW1_P4p_SSSS, h2_data_CEW1_P4p_SSSS,Evol_data_CEW1_P4p_SSSS,trait_mean_data_CEW1_P4p_SSSS)
      data_CEW1_P4p_SSSS
      
    }
    Vm_CEW1_P4p_SSSS <- rbind.data.frame(liab_CEW1_P4p_SSSS, data_CEW1_P4p_SSSS)
    Vm_CEW1_P4p_SSSS$Pnp_fate <- rep("SSSS", 24)
    Vm_CEW1_P4p_SSSS
    #remove CEW1 P4p_SSSSS models
    {
      remove(CEW1_CONTROL_P4p_SSSS_mod)
      remove(CEW1_MA_P4p_SSSS_mod)
      remove(CEW1_Vm_P4p_SSSS_mod)
    }
  }
  
  ##Summary CEW1 P8p----
  {
    #Summary liability scale CEW1 P8p
    {
      #Summary va_liab_CEW1_P8p_SSSS: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_CEW1_P8p_SSSS <- rbind(mean(va_liab_CEW1_CONTROL_P8p_SSSS_mod/2),mean(va_liab_CEW1_MA_P8p_SSSS_mod/2),mean(va_liab_CEW1_Vm_P8p_SSSS_mod/2))
        colnames(mean_va_liab_CEW1_P8p_SSSS) <- c("mean")
        median_va_liab_CEW1_P8p_SSSS <- rbind(median(va_liab_CEW1_CONTROL_P8p_SSSS_mod/2),median(va_liab_CEW1_MA_P8p_SSSS_mod/2),median(va_liab_CEW1_Vm_P8p_SSSS_mod/2))
        colnames(median_va_liab_CEW1_P8p_SSSS) <- c("median")
        posterior.mode_va_liab_CEW1_P8p_SSSS <- rbind(posterior.mode(va_liab_CEW1_CONTROL_P8p_SSSS_mod/2),posterior.mode(va_liab_CEW1_MA_P8p_SSSS_mod/2),posterior.mode(va_liab_CEW1_Vm_P8p_SSSS_mod/2))
        colnames(posterior.mode_va_liab_CEW1_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_CEW1_P8p_SSSS <- rbind(HPDinterval(va_liab_CEW1_CONTROL_P8p_SSSS_mod/2),HPDinterval(va_liab_CEW1_MA_P8p_SSSS_mod/2),HPDinterval(va_liab_CEW1_Vm_P8p_SSSS_mod/2))
        colnames(HPDinterval_0.95_va_liab_CEW1_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_CEW1_P8p_SSSS <- rbind(HPDinterval(va_liab_CEW1_CONTROL_P8p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_CEW1_MA_P8p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_CEW1_Vm_P8p_SSSS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_CEW1_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_CEW1_P8p_SSSS <- rbind(effectiveSize(va_liab_CEW1_CONTROL_P8p_SSSS_mod/2),effectiveSize(va_liab_CEW1_MA_P8p_SSSS_mod/2),effectiveSize(va_liab_CEW1_Vm_P8p_SSSS_mod/2))
        colnames(effectiveSize_va_liab_CEW1_P8p_SSSS) <- c("effectiveSize")
        va_liab_CEW1_P8p_SSSS <- cbind.data.frame(mean_va_liab_CEW1_P8p_SSSS,median_va_liab_CEW1_P8p_SSSS,posterior.mode_va_liab_CEW1_P8p_SSSS,HPDinterval_0.95_va_liab_CEW1_P8p_SSSS,HPDinterval_0.83_va_liab_CEW1_P8p_SSSS,effectiveSize_va_liab_CEW1_P8p_SSSS)
        rownames(va_liab_CEW1_P8p_SSSS) <- c("va_liab_CEW1_CONTROL_P8p_SSSS_mod","va_liab_CEW1_MA_P8p_SSSS_mod","va_liab_CEW1_Vm_P8p_SSSS_mod")
        va_liab_CEW1_P8p_SSSS <- cbind(Models = rownames(va_liab_CEW1_P8p_SSSS),va_liab_CEW1_P8p_SSSS)
        rownames(va_liab_CEW1_P8p_SSSS) <- NULL
        va_liab_CEW1_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        va_liab_CEW1_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        va_liab_CEW1_P8p_SSSS$Measure <- c("Va","Va","Va")
        va_liab_CEW1_P8p_SSSS$Scale <- c("liab","liab","liab")
        va_liab_CEW1_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_liab_CEW1_P8p_SSSS
      }
      
      #Summary h2_liab_CEW1_P8p_SSSS
      {
        mean_h2_liab_CEW1_P8p_SSSS <- rbind(mean(h2_liab_CEW1_CONTROL_P8p_SSSS_mod),mean(h2_liab_CEW1_MA_P8p_SSSS_mod),mean(h2_liab_CEW1_Vm_P8p_SSSS_mod))
        colnames(mean_h2_liab_CEW1_P8p_SSSS) <- c("mean")
        median_h2_liab_CEW1_P8p_SSSS <- rbind(median(h2_liab_CEW1_CONTROL_P8p_SSSS_mod),median(h2_liab_CEW1_MA_P8p_SSSS_mod),median(h2_liab_CEW1_Vm_P8p_SSSS_mod))
        colnames(median_h2_liab_CEW1_P8p_SSSS) <- c("median")
        posterior.mode_h2_liab_CEW1_P8p_SSSS <- rbind(posterior.mode(h2_liab_CEW1_CONTROL_P8p_SSSS_mod),posterior.mode(h2_liab_CEW1_MA_P8p_SSSS_mod),posterior.mode(h2_liab_CEW1_Vm_P8p_SSSS_mod))
        colnames(posterior.mode_h2_liab_CEW1_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_CEW1_P8p_SSSS <- rbind(HPDinterval(h2_liab_CEW1_CONTROL_P8p_SSSS_mod),HPDinterval(h2_liab_CEW1_MA_P8p_SSSS_mod),HPDinterval(h2_liab_CEW1_Vm_P8p_SSSS_mod))
        colnames(HPDinterval_0.95_h2_liab_CEW1_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_CEW1_P8p_SSSS <- rbind(HPDinterval(h2_liab_CEW1_CONTROL_P8p_SSSS_mod,prob=.83),HPDinterval(h2_liab_CEW1_MA_P8p_SSSS_mod,prob=.83),HPDinterval(h2_liab_CEW1_Vm_P8p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_CEW1_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_CEW1_P8p_SSSS <- rbind(effectiveSize(h2_liab_CEW1_CONTROL_P8p_SSSS_mod),effectiveSize(h2_liab_CEW1_MA_P8p_SSSS_mod),effectiveSize(h2_liab_CEW1_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_h2_liab_CEW1_P8p_SSSS) <- c("effectiveSize")
        h2_liab_CEW1_P8p_SSSS <- cbind.data.frame(mean_h2_liab_CEW1_P8p_SSSS,median_h2_liab_CEW1_P8p_SSSS,posterior.mode_h2_liab_CEW1_P8p_SSSS,HPDinterval_0.95_h2_liab_CEW1_P8p_SSSS,HPDinterval_0.83_h2_liab_CEW1_P8p_SSSS,effectiveSize_h2_liab_CEW1_P8p_SSSS)
        rownames(h2_liab_CEW1_P8p_SSSS) <- c("h2_liab_CEW1_CONTROL_P8p_SSSS_mod","h2_liab_CEW1_MA_P8p_SSSS_mod","h2_liab_CEW1_Vm_P8p_SSSS_mod")
        h2_liab_CEW1_P8p_SSSS <- cbind(Models = rownames(h2_liab_CEW1_P8p_SSSS),h2_liab_CEW1_P8p_SSSS)
        rownames(h2_liab_CEW1_P8p_SSSS) <- NULL
        h2_liab_CEW1_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        h2_liab_CEW1_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_liab_CEW1_P8p_SSSS$Measure <- c("H2","H2","H2")
        h2_liab_CEW1_P8p_SSSS$Scale <- c("liab","liab","liab")
        h2_liab_CEW1_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_liab_CEW1_P8p_SSSS
      }
      
      #Summary Evol_liab_CEW1_P8p_SSSS
      {
        mean_Evol_liab_CEW1_P8p_SSSS <- rbind(mean(Evol_liab_CEW1_CONTROL_P8p_SSSS_mod),mean(Evol_liab_CEW1_MA_P8p_SSSS_mod),mean(Evol_liab_CEW1_Vm_P8p_SSSS_mod))
        colnames(mean_Evol_liab_CEW1_P8p_SSSS) <- c("mean")
        median_Evol_liab_CEW1_P8p_SSSS <- rbind(median(Evol_liab_CEW1_CONTROL_P8p_SSSS_mod),median(Evol_liab_CEW1_MA_P8p_SSSS_mod),median(Evol_liab_CEW1_Vm_P8p_SSSS_mod))
        colnames(median_Evol_liab_CEW1_P8p_SSSS) <- c("median")
        posterior.mode_Evol_liab_CEW1_P8p_SSSS <- rbind(posterior.mode(Evol_liab_CEW1_CONTROL_P8p_SSSS_mod),posterior.mode(Evol_liab_CEW1_MA_P8p_SSSS_mod),posterior.mode(Evol_liab_CEW1_Vm_P8p_SSSS_mod))
        colnames(posterior.mode_Evol_liab_CEW1_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_CEW1_P8p_SSSS <- rbind(HPDinterval(Evol_liab_CEW1_CONTROL_P8p_SSSS_mod),HPDinterval(Evol_liab_CEW1_MA_P8p_SSSS_mod),HPDinterval(Evol_liab_CEW1_Vm_P8p_SSSS_mod))
        colnames(HPDinterval_0.95_Evol_liab_CEW1_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_CEW1_P8p_SSSS <- rbind(HPDinterval(Evol_liab_CEW1_CONTROL_P8p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_CEW1_MA_P8p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_CEW1_Vm_P8p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_CEW1_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_CEW1_P8p_SSSS <- rbind(effectiveSize(Evol_liab_CEW1_CONTROL_P8p_SSSS_mod),effectiveSize(Evol_liab_CEW1_MA_P8p_SSSS_mod),effectiveSize(Evol_liab_CEW1_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_Evol_liab_CEW1_P8p_SSSS) <- c("effectiveSize")
        Evol_liab_CEW1_P8p_SSSS <- cbind.data.frame(mean_Evol_liab_CEW1_P8p_SSSS,median_Evol_liab_CEW1_P8p_SSSS,posterior.mode_Evol_liab_CEW1_P8p_SSSS,HPDinterval_0.95_Evol_liab_CEW1_P8p_SSSS,HPDinterval_0.83_Evol_liab_CEW1_P8p_SSSS,effectiveSize_Evol_liab_CEW1_P8p_SSSS)
        rownames(Evol_liab_CEW1_P8p_SSSS) <- c("Evol_liab_CEW1_CONTROL_P8p_SSSS_mod","Evol_liab_CEW1_MA_P8p_SSSS_mod","Evol_liab_CEW1_Vm_P8p_SSSS_mod")
        Evol_liab_CEW1_P8p_SSSS <- cbind(Models = rownames(Evol_liab_CEW1_P8p_SSSS),Evol_liab_CEW1_P8p_SSSS)
        rownames(Evol_liab_CEW1_P8p_SSSS) <- NULL
        Evol_liab_CEW1_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        Evol_liab_CEW1_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_liab_CEW1_P8p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_CEW1_P8p_SSSS$Scale <- c("liab","liab","liab")
        Evol_liab_CEW1_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_CEW1_P8p_SSSS
      }
      
      #Summary trait_mean_liab_CEW1_P8p_SSSS
      {
        mean_trait_mean_liab_CEW1_P8p_SSSS <- rbind(mean(trait_mean_liab_CEW1_CONTROL_P8p_SSSS_mod),mean(trait_mean_liab_CEW1_MA_P8p_SSSS_mod),mean(trait_mean_liab_CEW1_Vm_P8p_SSSS_mod))
        colnames(mean_trait_mean_liab_CEW1_P8p_SSSS) <- c("mean")
        median_trait_mean_liab_CEW1_P8p_SSSS <- rbind(median(trait_mean_liab_CEW1_CONTROL_P8p_SSSS_mod),median(trait_mean_liab_CEW1_MA_P8p_SSSS_mod),median(trait_mean_liab_CEW1_Vm_P8p_SSSS_mod))
        colnames(median_trait_mean_liab_CEW1_P8p_SSSS) <- c("median")
        posterior.mode_trait_mean_liab_CEW1_P8p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_CEW1_CONTROL_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_CEW1_MA_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_CEW1_Vm_P8p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_liab_CEW1_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_CEW1_P8p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_CEW1_CONTROL_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_CEW1_MA_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_CEW1_Vm_P8p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_CEW1_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_CEW1_P8p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_CEW1_CONTROL_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_CEW1_MA_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_CEW1_Vm_P8p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_CEW1_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_CEW1_P8p_SSSS <- rbind(effectiveSize(trait_mean_liab_CEW1_CONTROL_P8p_SSSS_mod),effectiveSize(trait_mean_liab_CEW1_MA_P8p_SSSS_mod),effectiveSize(trait_mean_liab_CEW1_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_trait_mean_liab_CEW1_P8p_SSSS) <- c("effectiveSize")
        trait_mean_liab_CEW1_P8p_SSSS <- cbind.data.frame(mean_trait_mean_liab_CEW1_P8p_SSSS,median_trait_mean_liab_CEW1_P8p_SSSS,posterior.mode_trait_mean_liab_CEW1_P8p_SSSS,HPDinterval_0.95_trait_mean_liab_CEW1_P8p_SSSS,HPDinterval_0.83_trait_mean_liab_CEW1_P8p_SSSS,effectiveSize_trait_mean_liab_CEW1_P8p_SSSS)
        rownames(trait_mean_liab_CEW1_P8p_SSSS) <- c("trait_mean_liab_CEW1_CONTROL_P8p_SSSS_mod","trait_mean_liab_CEW1_MA_P8p_SSSS_mod","trait_mean_liab_CEW1_Vm_P8p_SSSS_mod")
        trait_mean_liab_CEW1_P8p_SSSS <- cbind(Models = rownames(trait_mean_liab_CEW1_P8p_SSSS),trait_mean_liab_CEW1_P8p_SSSS)
        rownames(trait_mean_liab_CEW1_P8p_SSSS) <- NULL
        trait_mean_liab_CEW1_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        trait_mean_liab_CEW1_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_CEW1_P8p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_CEW1_P8p_SSSS$Scale <- c("liab","liab","liab")
        trait_mean_liab_CEW1_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_CEW1_P8p_SSSS
      }
      
      liab_CEW1_P8p_SSSS <- rbind.data.frame(va_liab_CEW1_P8p_SSSS, h2_liab_CEW1_P8p_SSSS,Evol_liab_CEW1_P8p_SSSS,trait_mean_liab_CEW1_P8p_SSSS)
      liab_CEW1_P8p_SSSS
    }
    #Summary data scale CEW1 P8p
    {
      #Summary va_data_CEW1_P8p_SSSS:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_CEW1_P8p_SSSS <- rbind(mean(va_data_CEW1_CONTROL_P8p_SSSS_mod/2),mean(va_data_CEW1_MA_P8p_SSSS_mod/2),mean(va_data_CEW1_Vm_P8p_SSSS_mod/2))
        colnames(mean_va_data_CEW1_P8p_SSSS) <- c("mean")
        median_va_data_CEW1_P8p_SSSS <- rbind(median(va_data_CEW1_CONTROL_P8p_SSSS_mod/2),median(va_data_CEW1_MA_P8p_SSSS_mod/2),median(va_data_CEW1_Vm_P8p_SSSS_mod/2))
        colnames(median_va_data_CEW1_P8p_SSSS) <- c("median")
        posterior.mode_va_data_CEW1_P8p_SSSS <- rbind(posterior.mode(as.mcmc(va_data_CEW1_CONTROL_P8p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_CEW1_MA_P8p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_CEW1_Vm_P8p_SSSS_mod/2)))
        colnames(posterior.mode_va_data_CEW1_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_CEW1_P8p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_CEW1_CONTROL_P8p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_CEW1_MA_P8p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_CEW1_Vm_P8p_SSSS_mod/2)))
        colnames(HPDinterval_0.95_va_data_CEW1_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_CEW1_P8p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_CEW1_CONTROL_P8p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_CEW1_MA_P8p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_CEW1_Vm_P8p_SSSS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_CEW1_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_CEW1_P8p_SSSS <- rbind(effectiveSize(va_data_CEW1_CONTROL_P8p_SSSS_mod/2),effectiveSize(va_data_CEW1_MA_P8p_SSSS_mod/2),effectiveSize(va_data_CEW1_Vm_P8p_SSSS_mod/2))
        colnames(effectiveSize_va_data_CEW1_P8p_SSSS) <- c("effectiveSize")
        va_data_CEW1_P8p_SSSS <- cbind.data.frame(mean_va_data_CEW1_P8p_SSSS,median_va_data_CEW1_P8p_SSSS,posterior.mode_va_data_CEW1_P8p_SSSS,HPDinterval_0.95_va_data_CEW1_P8p_SSSS,HPDinterval_0.83_va_data_CEW1_P8p_SSSS,effectiveSize_va_data_CEW1_P8p_SSSS)
        rownames(va_data_CEW1_P8p_SSSS) <- c("va_data_CEW1_CONTROL_P8p_SSSS_mod","va_data_CEW1_MA_P8p_SSSS_mod","va_data_CEW1_Vm_P8p_SSSS_mod")
        va_data_CEW1_P8p_SSSS <- cbind(Models = rownames(va_data_CEW1_P8p_SSSS),va_data_CEW1_P8p_SSSS)
        rownames(va_data_CEW1_P8p_SSSS) <- NULL
        va_data_CEW1_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        va_data_CEW1_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        va_data_CEW1_P8p_SSSS$Measure <- c("Va","Va","Va")
        va_data_CEW1_P8p_SSSS$Scale <- c("data","data","data")
        va_data_CEW1_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_data_CEW1_P8p_SSSS
      }
      
      #Summary h2_data_CEW1_P8p_SSSS
      {
        mean_h2_data_CEW1_P8p_SSSS <- rbind(mean(h2_data_CEW1_CONTROL_P8p_SSSS_mod),mean(h2_data_CEW1_MA_P8p_SSSS_mod),mean(h2_data_CEW1_Vm_P8p_SSSS_mod))
        colnames(mean_h2_data_CEW1_P8p_SSSS) <- c("mean")
        median_h2_data_CEW1_P8p_SSSS <- rbind(median(h2_data_CEW1_CONTROL_P8p_SSSS_mod),median(h2_data_CEW1_MA_P8p_SSSS_mod),median(h2_data_CEW1_Vm_P8p_SSSS_mod))
        colnames(median_h2_data_CEW1_P8p_SSSS) <- c("median")
        posterior.mode_h2_data_CEW1_P8p_SSSS <- rbind(posterior.mode(as.mcmc(h2_data_CEW1_CONTROL_P8p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_CEW1_MA_P8p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_CEW1_Vm_P8p_SSSS_mod)))
        colnames(posterior.mode_h2_data_CEW1_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_CEW1_P8p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_CEW1_CONTROL_P8p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_CEW1_MA_P8p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_CEW1_Vm_P8p_SSSS_mod)))
        colnames(HPDinterval_0.95_h2_data_CEW1_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_CEW1_P8p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_CEW1_CONTROL_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_CEW1_MA_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_CEW1_Vm_P8p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_CEW1_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_CEW1_P8p_SSSS <- rbind(effectiveSize(h2_data_CEW1_CONTROL_P8p_SSSS_mod),effectiveSize(h2_data_CEW1_MA_P8p_SSSS_mod),effectiveSize(h2_data_CEW1_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_h2_data_CEW1_P8p_SSSS) <- c("effectiveSize")
        h2_data_CEW1_P8p_SSSS <- cbind.data.frame(mean_h2_data_CEW1_P8p_SSSS,median_h2_data_CEW1_P8p_SSSS,posterior.mode_h2_data_CEW1_P8p_SSSS,HPDinterval_0.95_h2_data_CEW1_P8p_SSSS,HPDinterval_0.83_h2_data_CEW1_P8p_SSSS,effectiveSize_h2_data_CEW1_P8p_SSSS)
        rownames(h2_data_CEW1_P8p_SSSS) <- c("h2_data_CEW1_CONTROL_P8p_SSSS_mod","h2_data_CEW1_MA_P8p_SSSS_mod","h2_data_CEW1_Vm_P8p_SSSS_mod")
        h2_data_CEW1_P8p_SSSS <- cbind(Models = rownames(h2_data_CEW1_P8p_SSSS),h2_data_CEW1_P8p_SSSS)
        rownames(h2_data_CEW1_P8p_SSSS) <- NULL
        h2_data_CEW1_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        h2_data_CEW1_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_data_CEW1_P8p_SSSS$Measure <- c("H2","H2","H2")
        h2_data_CEW1_P8p_SSSS$Scale <- c("data","data","data")
        h2_data_CEW1_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_data_CEW1_P8p_SSSS
      }
      
      #Summary Evol_data_CEW1_P8p_SSSS
      {
        mean_Evol_data_CEW1_P8p_SSSS <- rbind(mean(Evol_data_CEW1_CONTROL_P8p_SSSS_mod),mean(Evol_data_CEW1_MA_P8p_SSSS_mod),mean(Evol_data_CEW1_Vm_P8p_SSSS_mod))
        colnames(mean_Evol_data_CEW1_P8p_SSSS) <- c("mean")
        median_Evol_data_CEW1_P8p_SSSS <- rbind(median(Evol_data_CEW1_CONTROL_P8p_SSSS_mod),median(Evol_data_CEW1_MA_P8p_SSSS_mod),median(Evol_data_CEW1_Vm_P8p_SSSS_mod))
        colnames(median_Evol_data_CEW1_P8p_SSSS) <- c("median")
        posterior.mode_Evol_data_CEW1_P8p_SSSS <- rbind(posterior.mode(as.mcmc(Evol_data_CEW1_CONTROL_P8p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_CEW1_MA_P8p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_CEW1_Vm_P8p_SSSS_mod)))
        colnames(posterior.mode_Evol_data_CEW1_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_CEW1_P8p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_CEW1_CONTROL_P8p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_CEW1_MA_P8p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_CEW1_Vm_P8p_SSSS_mod)))
        colnames(HPDinterval_0.95_Evol_data_CEW1_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_CEW1_P8p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_CEW1_CONTROL_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_CEW1_MA_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_CEW1_Vm_P8p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_CEW1_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_CEW1_P8p_SSSS <- rbind(effectiveSize(Evol_data_CEW1_CONTROL_P8p_SSSS_mod),effectiveSize(Evol_data_CEW1_MA_P8p_SSSS_mod),effectiveSize(Evol_data_CEW1_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_Evol_data_CEW1_P8p_SSSS) <- c("effectiveSize")
        Evol_data_CEW1_P8p_SSSS <- cbind.data.frame(mean_Evol_data_CEW1_P8p_SSSS,median_Evol_data_CEW1_P8p_SSSS,posterior.mode_Evol_data_CEW1_P8p_SSSS,HPDinterval_0.95_Evol_data_CEW1_P8p_SSSS,HPDinterval_0.83_Evol_data_CEW1_P8p_SSSS,effectiveSize_Evol_data_CEW1_P8p_SSSS)
        rownames(Evol_data_CEW1_P8p_SSSS) <- c("Evol_data_CEW1_CONTROL_P8p_SSSS_mod","Evol_data_CEW1_MA_P8p_SSSS_mod","Evol_data_CEW1_Vm_P8p_SSSS_mod")
        Evol_data_CEW1_P8p_SSSS <- cbind(Models = rownames(Evol_data_CEW1_P8p_SSSS),Evol_data_CEW1_P8p_SSSS)
        rownames(Evol_data_CEW1_P8p_SSSS) <- NULL
        Evol_data_CEW1_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        Evol_data_CEW1_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_data_CEW1_P8p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_data_CEW1_P8p_SSSS$Scale <- c("data","data","data")
        Evol_data_CEW1_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_data_CEW1_P8p_SSSS
      }
      
      #Summary trait_mean_data_CEW1_P8p_SSSS
      {
        mean_trait_mean_data_CEW1_P8p_SSSS <- rbind(mean(trait_mean_data_CEW1_CONTROL_P8p_SSSS_mod),mean(trait_mean_data_CEW1_MA_P8p_SSSS_mod),mean(trait_mean_data_CEW1_Vm_P8p_SSSS_mod))
        colnames(mean_trait_mean_data_CEW1_P8p_SSSS) <- c("mean")
        median_trait_mean_data_CEW1_P8p_SSSS <- rbind(median(trait_mean_data_CEW1_CONTROL_P8p_SSSS_mod),median(trait_mean_data_CEW1_MA_P8p_SSSS_mod),median(trait_mean_data_CEW1_Vm_P8p_SSSS_mod))
        colnames(median_trait_mean_data_CEW1_P8p_SSSS) <- c("median")
        posterior.mode_trait_mean_data_CEW1_P8p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_data_CEW1_CONTROL_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_CEW1_MA_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_CEW1_Vm_P8p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_data_CEW1_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_CEW1_P8p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_CEW1_CONTROL_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_CEW1_MA_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_CEW1_Vm_P8p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_CEW1_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_CEW1_P8p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_CEW1_CONTROL_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_CEW1_MA_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_CEW1_Vm_P8p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_CEW1_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_CEW1_P8p_SSSS <- rbind(effectiveSize(trait_mean_data_CEW1_CONTROL_P8p_SSSS_mod),effectiveSize(trait_mean_data_CEW1_MA_P8p_SSSS_mod),effectiveSize(trait_mean_data_CEW1_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_trait_mean_data_CEW1_P8p_SSSS) <- c("effectiveSize")
        trait_mean_data_CEW1_P8p_SSSS <- cbind.data.frame(mean_trait_mean_data_CEW1_P8p_SSSS,median_trait_mean_data_CEW1_P8p_SSSS,posterior.mode_trait_mean_data_CEW1_P8p_SSSS,HPDinterval_0.95_trait_mean_data_CEW1_P8p_SSSS,HPDinterval_0.83_trait_mean_data_CEW1_P8p_SSSS,effectiveSize_trait_mean_data_CEW1_P8p_SSSS)
        rownames(trait_mean_data_CEW1_P8p_SSSS) <- c("trait_mean_data_CEW1_CONTROL_P8p_SSSS_mod","trait_mean_data_CEW1_MA_P8p_SSSS_mod","trait_mean_data_CEW1_Vm_P8p_SSSS_mod")
        trait_mean_data_CEW1_P8p_SSSS <- cbind(Models = rownames(trait_mean_data_CEW1_P8p_SSSS),trait_mean_data_CEW1_P8p_SSSS)
        rownames(trait_mean_data_CEW1_P8p_SSSS) <- NULL
        trait_mean_data_CEW1_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        trait_mean_data_CEW1_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_data_CEW1_P8p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_CEW1_P8p_SSSS$Scale <- c("data","data","data")
        trait_mean_data_CEW1_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_CEW1_P8p_SSSS
      }
      
      data_CEW1_P8p_SSSS <- rbind.data.frame(va_data_CEW1_P8p_SSSS, h2_data_CEW1_P8p_SSSS,Evol_data_CEW1_P8p_SSSS,trait_mean_data_CEW1_P8p_SSSS)
      data_CEW1_P8p_SSSS
      
    }
    Vm_CEW1_P8p_SSSS <- rbind.data.frame(liab_CEW1_P8p_SSSS, data_CEW1_P8p_SSSS)
    Vm_CEW1_P8p_SSSS$Pnp_fate <- rep("SSSS", 24)
    Vm_CEW1_P8p_SSSS
    #remove CEW1 P8p_SSSSS models
    {
      remove(CEW1_CONTROL_P8p_SSSS_mod)
      remove(CEW1_MA_P8p_SSSS_mod)
      remove(CEW1_Vm_P8p_SSSS_mod)
    }
  }
  
  ##Summary CEW1 P5p----
  {
    #Summary liability scale CEW1 P5p
    {
      #Summary va_liab_CEW1_P5p_wt: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_CEW1_P5p_wt <- rbind(mean(va_liab_CEW1_CONTROL_P5p_wt_mod/2),mean(va_liab_CEW1_MA_P5p_wt_mod/2),mean(va_liab_CEW1_Vm_P5p_wt_mod/2))
        colnames(mean_va_liab_CEW1_P5p_wt) <- c("mean")
        median_va_liab_CEW1_P5p_wt <- rbind(median(va_liab_CEW1_CONTROL_P5p_wt_mod/2),median(va_liab_CEW1_MA_P5p_wt_mod/2),median(va_liab_CEW1_Vm_P5p_wt_mod/2))
        colnames(median_va_liab_CEW1_P5p_wt) <- c("median")
        posterior.mode_va_liab_CEW1_P5p_wt <- rbind(posterior.mode(va_liab_CEW1_CONTROL_P5p_wt_mod/2),posterior.mode(va_liab_CEW1_MA_P5p_wt_mod/2),posterior.mode(va_liab_CEW1_Vm_P5p_wt_mod/2))
        colnames(posterior.mode_va_liab_CEW1_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_CEW1_P5p_wt <- rbind(HPDinterval(va_liab_CEW1_CONTROL_P5p_wt_mod/2),HPDinterval(va_liab_CEW1_MA_P5p_wt_mod/2),HPDinterval(va_liab_CEW1_Vm_P5p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_CEW1_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_CEW1_P5p_wt <- rbind(HPDinterval(va_liab_CEW1_CONTROL_P5p_wt_mod/2,prob=.83),HPDinterval(va_liab_CEW1_MA_P5p_wt_mod/2,prob=.83),HPDinterval(va_liab_CEW1_Vm_P5p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_CEW1_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_CEW1_P5p_wt <- rbind(effectiveSize(va_liab_CEW1_CONTROL_P5p_wt_mod/2),effectiveSize(va_liab_CEW1_MA_P5p_wt_mod/2),effectiveSize(va_liab_CEW1_Vm_P5p_wt_mod/2))
        colnames(effectiveSize_va_liab_CEW1_P5p_wt) <- c("effectiveSize")
        va_liab_CEW1_P5p_wt <- cbind.data.frame(mean_va_liab_CEW1_P5p_wt,median_va_liab_CEW1_P5p_wt,posterior.mode_va_liab_CEW1_P5p_wt,HPDinterval_0.95_va_liab_CEW1_P5p_wt,HPDinterval_0.83_va_liab_CEW1_P5p_wt,effectiveSize_va_liab_CEW1_P5p_wt)
        rownames(va_liab_CEW1_P5p_wt) <- c("va_liab_CEW1_CONTROL_P5p_wt_mod","va_liab_CEW1_MA_P5p_wt_mod","va_liab_CEW1_Vm_P5p_wt_mod")
        va_liab_CEW1_P5p_wt <- cbind(Models = rownames(va_liab_CEW1_P5p_wt),va_liab_CEW1_P5p_wt)
        rownames(va_liab_CEW1_P5p_wt) <- NULL
        va_liab_CEW1_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        va_liab_CEW1_P5p_wt$Treatment <- c("Control","ML","Vm")
        va_liab_CEW1_P5p_wt$Measure <- c("Va","Va","Va")
        va_liab_CEW1_P5p_wt$Scale <- c("liab","liab","liab")
        va_liab_CEW1_P5p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_CEW1_P5p_wt
      }
      
      #Summary h2_liab_CEW1_P5p_wt
      {
        mean_h2_liab_CEW1_P5p_wt <- rbind(mean(h2_liab_CEW1_CONTROL_P5p_wt_mod),mean(h2_liab_CEW1_MA_P5p_wt_mod),mean(h2_liab_CEW1_Vm_P5p_wt_mod))
        colnames(mean_h2_liab_CEW1_P5p_wt) <- c("mean")
        median_h2_liab_CEW1_P5p_wt <- rbind(median(h2_liab_CEW1_CONTROL_P5p_wt_mod),median(h2_liab_CEW1_MA_P5p_wt_mod),median(h2_liab_CEW1_Vm_P5p_wt_mod))
        colnames(median_h2_liab_CEW1_P5p_wt) <- c("median")
        posterior.mode_h2_liab_CEW1_P5p_wt <- rbind(posterior.mode(h2_liab_CEW1_CONTROL_P5p_wt_mod),posterior.mode(h2_liab_CEW1_MA_P5p_wt_mod),posterior.mode(h2_liab_CEW1_Vm_P5p_wt_mod))
        colnames(posterior.mode_h2_liab_CEW1_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_CEW1_P5p_wt <- rbind(HPDinterval(h2_liab_CEW1_CONTROL_P5p_wt_mod),HPDinterval(h2_liab_CEW1_MA_P5p_wt_mod),HPDinterval(h2_liab_CEW1_Vm_P5p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_CEW1_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_CEW1_P5p_wt <- rbind(HPDinterval(h2_liab_CEW1_CONTROL_P5p_wt_mod,prob=.83),HPDinterval(h2_liab_CEW1_MA_P5p_wt_mod,prob=.83),HPDinterval(h2_liab_CEW1_Vm_P5p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_CEW1_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_CEW1_P5p_wt <- rbind(effectiveSize(h2_liab_CEW1_CONTROL_P5p_wt_mod),effectiveSize(h2_liab_CEW1_MA_P5p_wt_mod),effectiveSize(h2_liab_CEW1_Vm_P5p_wt_mod))
        colnames(effectiveSize_h2_liab_CEW1_P5p_wt) <- c("effectiveSize")
        h2_liab_CEW1_P5p_wt <- cbind.data.frame(mean_h2_liab_CEW1_P5p_wt,median_h2_liab_CEW1_P5p_wt,posterior.mode_h2_liab_CEW1_P5p_wt,HPDinterval_0.95_h2_liab_CEW1_P5p_wt,HPDinterval_0.83_h2_liab_CEW1_P5p_wt,effectiveSize_h2_liab_CEW1_P5p_wt)
        rownames(h2_liab_CEW1_P5p_wt) <- c("h2_liab_CEW1_CONTROL_P5p_wt_mod","h2_liab_CEW1_MA_P5p_wt_mod","h2_liab_CEW1_Vm_P5p_wt_mod")
        h2_liab_CEW1_P5p_wt <- cbind(Models = rownames(h2_liab_CEW1_P5p_wt),h2_liab_CEW1_P5p_wt)
        rownames(h2_liab_CEW1_P5p_wt) <- NULL
        h2_liab_CEW1_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        h2_liab_CEW1_P5p_wt$Treatment <- c("Control","ML","Vm")
        h2_liab_CEW1_P5p_wt$Measure <- c("H2","H2","H2")
        h2_liab_CEW1_P5p_wt$Scale <- c("liab","liab","liab")
        h2_liab_CEW1_P5p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_CEW1_P5p_wt
      }
      
      #Summary Evol_liab_CEW1_P5p_wt
      {
        mean_Evol_liab_CEW1_P5p_wt <- rbind(mean(Evol_liab_CEW1_CONTROL_P5p_wt_mod),mean(Evol_liab_CEW1_MA_P5p_wt_mod),mean(Evol_liab_CEW1_Vm_P5p_wt_mod))
        colnames(mean_Evol_liab_CEW1_P5p_wt) <- c("mean")
        median_Evol_liab_CEW1_P5p_wt <- rbind(median(Evol_liab_CEW1_CONTROL_P5p_wt_mod),median(Evol_liab_CEW1_MA_P5p_wt_mod),median(Evol_liab_CEW1_Vm_P5p_wt_mod))
        colnames(median_Evol_liab_CEW1_P5p_wt) <- c("median")
        posterior.mode_Evol_liab_CEW1_P5p_wt <- rbind(posterior.mode(Evol_liab_CEW1_CONTROL_P5p_wt_mod),posterior.mode(Evol_liab_CEW1_MA_P5p_wt_mod),posterior.mode(Evol_liab_CEW1_Vm_P5p_wt_mod))
        colnames(posterior.mode_Evol_liab_CEW1_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_CEW1_P5p_wt <- rbind(HPDinterval(Evol_liab_CEW1_CONTROL_P5p_wt_mod),HPDinterval(Evol_liab_CEW1_MA_P5p_wt_mod),HPDinterval(Evol_liab_CEW1_Vm_P5p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_CEW1_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_CEW1_P5p_wt <- rbind(HPDinterval(Evol_liab_CEW1_CONTROL_P5p_wt_mod,prob=.83),HPDinterval(Evol_liab_CEW1_MA_P5p_wt_mod,prob=.83),HPDinterval(Evol_liab_CEW1_Vm_P5p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_CEW1_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_CEW1_P5p_wt <- rbind(effectiveSize(Evol_liab_CEW1_CONTROL_P5p_wt_mod),effectiveSize(Evol_liab_CEW1_MA_P5p_wt_mod),effectiveSize(Evol_liab_CEW1_Vm_P5p_wt_mod))
        colnames(effectiveSize_Evol_liab_CEW1_P5p_wt) <- c("effectiveSize")
        Evol_liab_CEW1_P5p_wt <- cbind.data.frame(mean_Evol_liab_CEW1_P5p_wt,median_Evol_liab_CEW1_P5p_wt,posterior.mode_Evol_liab_CEW1_P5p_wt,HPDinterval_0.95_Evol_liab_CEW1_P5p_wt,HPDinterval_0.83_Evol_liab_CEW1_P5p_wt,effectiveSize_Evol_liab_CEW1_P5p_wt)
        rownames(Evol_liab_CEW1_P5p_wt) <- c("Evol_liab_CEW1_CONTROL_P5p_wt_mod","Evol_liab_CEW1_MA_P5p_wt_mod","Evol_liab_CEW1_Vm_P5p_wt_mod")
        Evol_liab_CEW1_P5p_wt <- cbind(Models = rownames(Evol_liab_CEW1_P5p_wt),Evol_liab_CEW1_P5p_wt)
        rownames(Evol_liab_CEW1_P5p_wt) <- NULL
        Evol_liab_CEW1_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        Evol_liab_CEW1_P5p_wt$Treatment <- c("Control","ML","Vm")
        Evol_liab_CEW1_P5p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_CEW1_P5p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_CEW1_P5p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_CEW1_P5p_wt
      }
      
      #Summary trait_mean_liab_CEW1_P5p_wt
      {
        mean_trait_mean_liab_CEW1_P5p_wt <- rbind(mean(trait_mean_liab_CEW1_CONTROL_P5p_wt_mod),mean(trait_mean_liab_CEW1_MA_P5p_wt_mod),mean(trait_mean_liab_CEW1_Vm_P5p_wt_mod))
        colnames(mean_trait_mean_liab_CEW1_P5p_wt) <- c("mean")
        median_trait_mean_liab_CEW1_P5p_wt <- rbind(median(trait_mean_liab_CEW1_CONTROL_P5p_wt_mod),median(trait_mean_liab_CEW1_MA_P5p_wt_mod),median(trait_mean_liab_CEW1_Vm_P5p_wt_mod))
        colnames(median_trait_mean_liab_CEW1_P5p_wt) <- c("median")
        posterior.mode_trait_mean_liab_CEW1_P5p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_CEW1_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_CEW1_MA_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_CEW1_Vm_P5p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_CEW1_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_CEW1_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_CEW1_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_CEW1_MA_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_CEW1_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_CEW1_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_CEW1_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_CEW1_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_CEW1_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_CEW1_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_CEW1_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_CEW1_P5p_wt <- rbind(effectiveSize(trait_mean_liab_CEW1_CONTROL_P5p_wt_mod),effectiveSize(trait_mean_liab_CEW1_MA_P5p_wt_mod),effectiveSize(trait_mean_liab_CEW1_Vm_P5p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_CEW1_P5p_wt) <- c("effectiveSize")
        trait_mean_liab_CEW1_P5p_wt <- cbind.data.frame(mean_trait_mean_liab_CEW1_P5p_wt,median_trait_mean_liab_CEW1_P5p_wt,posterior.mode_trait_mean_liab_CEW1_P5p_wt,HPDinterval_0.95_trait_mean_liab_CEW1_P5p_wt,HPDinterval_0.83_trait_mean_liab_CEW1_P5p_wt,effectiveSize_trait_mean_liab_CEW1_P5p_wt)
        rownames(trait_mean_liab_CEW1_P5p_wt) <- c("trait_mean_liab_CEW1_CONTROL_P5p_wt_mod","trait_mean_liab_CEW1_MA_P5p_wt_mod","trait_mean_liab_CEW1_Vm_P5p_wt_mod")
        trait_mean_liab_CEW1_P5p_wt <- cbind(Models = rownames(trait_mean_liab_CEW1_P5p_wt),trait_mean_liab_CEW1_P5p_wt)
        rownames(trait_mean_liab_CEW1_P5p_wt) <- NULL
        trait_mean_liab_CEW1_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        trait_mean_liab_CEW1_P5p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_CEW1_P5p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_CEW1_P5p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_CEW1_P5p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_CEW1_P5p_wt
      }
      
      liab_CEW1_P5p_wt <- rbind.data.frame(va_liab_CEW1_P5p_wt, h2_liab_CEW1_P5p_wt,Evol_liab_CEW1_P5p_wt,trait_mean_liab_CEW1_P5p_wt)
      liab_CEW1_P5p_wt
    }
    #Summary data scale CEW1 P5p
    {
      #Summary va_data_CEW1_P5p_wt:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_CEW1_P5p_wt <- rbind(mean(va_data_CEW1_CONTROL_P5p_wt_mod/2),mean(va_data_CEW1_MA_P5p_wt_mod/2),mean(va_data_CEW1_Vm_P5p_wt_mod/2))
        colnames(mean_va_data_CEW1_P5p_wt) <- c("mean")
        median_va_data_CEW1_P5p_wt <- rbind(median(va_data_CEW1_CONTROL_P5p_wt_mod/2),median(va_data_CEW1_MA_P5p_wt_mod/2),median(va_data_CEW1_Vm_P5p_wt_mod/2))
        colnames(median_va_data_CEW1_P5p_wt) <- c("median")
        posterior.mode_va_data_CEW1_P5p_wt <- rbind(posterior.mode(as.mcmc(va_data_CEW1_CONTROL_P5p_wt_mod/2)),posterior.mode(as.mcmc(va_data_CEW1_MA_P5p_wt_mod/2)),posterior.mode(as.mcmc(va_data_CEW1_Vm_P5p_wt_mod/2)))
        colnames(posterior.mode_va_data_CEW1_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_CEW1_P5p_wt <- rbind(HPDinterval(as.mcmc(va_data_CEW1_CONTROL_P5p_wt_mod/2)),HPDinterval(as.mcmc(va_data_CEW1_MA_P5p_wt_mod/2)),HPDinterval(as.mcmc(va_data_CEW1_Vm_P5p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_CEW1_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_CEW1_P5p_wt <- rbind(HPDinterval(as.mcmc(va_data_CEW1_CONTROL_P5p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_CEW1_MA_P5p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_CEW1_Vm_P5p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_CEW1_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_CEW1_P5p_wt <- rbind(effectiveSize(va_data_CEW1_CONTROL_P5p_wt_mod/2),effectiveSize(va_data_CEW1_MA_P5p_wt_mod/2),effectiveSize(va_data_CEW1_Vm_P5p_wt_mod/2))
        colnames(effectiveSize_va_data_CEW1_P5p_wt) <- c("effectiveSize")
        va_data_CEW1_P5p_wt <- cbind.data.frame(mean_va_data_CEW1_P5p_wt,median_va_data_CEW1_P5p_wt,posterior.mode_va_data_CEW1_P5p_wt,HPDinterval_0.95_va_data_CEW1_P5p_wt,HPDinterval_0.83_va_data_CEW1_P5p_wt,effectiveSize_va_data_CEW1_P5p_wt)
        rownames(va_data_CEW1_P5p_wt) <- c("va_data_CEW1_CONTROL_P5p_wt_mod","va_data_CEW1_MA_P5p_wt_mod","va_data_CEW1_Vm_P5p_wt_mod")
        va_data_CEW1_P5p_wt <- cbind(Models = rownames(va_data_CEW1_P5p_wt),va_data_CEW1_P5p_wt)
        rownames(va_data_CEW1_P5p_wt) <- NULL
        va_data_CEW1_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        va_data_CEW1_P5p_wt$Treatment <- c("Control","ML","Vm")
        va_data_CEW1_P5p_wt$Measure <- c("Va","Va","Va")
        va_data_CEW1_P5p_wt$Scale <- c("data","data","data")
        va_data_CEW1_P5p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_CEW1_P5p_wt
      }
      
      #Summary h2_data_CEW1_P5p_wt
      {
        mean_h2_data_CEW1_P5p_wt <- rbind(mean(h2_data_CEW1_CONTROL_P5p_wt_mod),mean(h2_data_CEW1_MA_P5p_wt_mod),mean(h2_data_CEW1_Vm_P5p_wt_mod))
        colnames(mean_h2_data_CEW1_P5p_wt) <- c("mean")
        median_h2_data_CEW1_P5p_wt <- rbind(median(h2_data_CEW1_CONTROL_P5p_wt_mod),median(h2_data_CEW1_MA_P5p_wt_mod),median(h2_data_CEW1_Vm_P5p_wt_mod))
        colnames(median_h2_data_CEW1_P5p_wt) <- c("median")
        posterior.mode_h2_data_CEW1_P5p_wt <- rbind(posterior.mode(as.mcmc(h2_data_CEW1_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(h2_data_CEW1_MA_P5p_wt_mod)),posterior.mode(as.mcmc(h2_data_CEW1_Vm_P5p_wt_mod)))
        colnames(posterior.mode_h2_data_CEW1_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_CEW1_P5p_wt <- rbind(HPDinterval(as.mcmc(h2_data_CEW1_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(h2_data_CEW1_MA_P5p_wt_mod)),HPDinterval(as.mcmc(h2_data_CEW1_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_CEW1_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_CEW1_P5p_wt <- rbind(HPDinterval(as.mcmc(h2_data_CEW1_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_CEW1_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_CEW1_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_CEW1_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_CEW1_P5p_wt <- rbind(effectiveSize(h2_data_CEW1_CONTROL_P5p_wt_mod),effectiveSize(h2_data_CEW1_MA_P5p_wt_mod),effectiveSize(h2_data_CEW1_Vm_P5p_wt_mod))
        colnames(effectiveSize_h2_data_CEW1_P5p_wt) <- c("effectiveSize")
        h2_data_CEW1_P5p_wt <- cbind.data.frame(mean_h2_data_CEW1_P5p_wt,median_h2_data_CEW1_P5p_wt,posterior.mode_h2_data_CEW1_P5p_wt,HPDinterval_0.95_h2_data_CEW1_P5p_wt,HPDinterval_0.83_h2_data_CEW1_P5p_wt,effectiveSize_h2_data_CEW1_P5p_wt)
        rownames(h2_data_CEW1_P5p_wt) <- c("h2_data_CEW1_CONTROL_P5p_wt_mod","h2_data_CEW1_MA_P5p_wt_mod","h2_data_CEW1_Vm_P5p_wt_mod")
        h2_data_CEW1_P5p_wt <- cbind(Models = rownames(h2_data_CEW1_P5p_wt),h2_data_CEW1_P5p_wt)
        rownames(h2_data_CEW1_P5p_wt) <- NULL
        h2_data_CEW1_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        h2_data_CEW1_P5p_wt$Treatment <- c("Control","ML","Vm")
        h2_data_CEW1_P5p_wt$Measure <- c("H2","H2","H2")
        h2_data_CEW1_P5p_wt$Scale <- c("data","data","data")
        h2_data_CEW1_P5p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_CEW1_P5p_wt
      }
      
      #Summary Evol_data_CEW1_P5p_wt
      {
        mean_Evol_data_CEW1_P5p_wt <- rbind(mean(Evol_data_CEW1_CONTROL_P5p_wt_mod),mean(Evol_data_CEW1_MA_P5p_wt_mod),mean(Evol_data_CEW1_Vm_P5p_wt_mod))
        colnames(mean_Evol_data_CEW1_P5p_wt) <- c("mean")
        median_Evol_data_CEW1_P5p_wt <- rbind(median(Evol_data_CEW1_CONTROL_P5p_wt_mod),median(Evol_data_CEW1_MA_P5p_wt_mod),median(Evol_data_CEW1_Vm_P5p_wt_mod))
        colnames(median_Evol_data_CEW1_P5p_wt) <- c("median")
        posterior.mode_Evol_data_CEW1_P5p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_CEW1_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(Evol_data_CEW1_MA_P5p_wt_mod)),posterior.mode(as.mcmc(Evol_data_CEW1_Vm_P5p_wt_mod)))
        colnames(posterior.mode_Evol_data_CEW1_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_CEW1_P5p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_CEW1_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(Evol_data_CEW1_MA_P5p_wt_mod)),HPDinterval(as.mcmc(Evol_data_CEW1_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_CEW1_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_CEW1_P5p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_CEW1_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_CEW1_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_CEW1_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_CEW1_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_CEW1_P5p_wt <- rbind(effectiveSize(Evol_data_CEW1_CONTROL_P5p_wt_mod),effectiveSize(Evol_data_CEW1_MA_P5p_wt_mod),effectiveSize(Evol_data_CEW1_Vm_P5p_wt_mod))
        colnames(effectiveSize_Evol_data_CEW1_P5p_wt) <- c("effectiveSize")
        Evol_data_CEW1_P5p_wt <- cbind.data.frame(mean_Evol_data_CEW1_P5p_wt,median_Evol_data_CEW1_P5p_wt,posterior.mode_Evol_data_CEW1_P5p_wt,HPDinterval_0.95_Evol_data_CEW1_P5p_wt,HPDinterval_0.83_Evol_data_CEW1_P5p_wt,effectiveSize_Evol_data_CEW1_P5p_wt)
        rownames(Evol_data_CEW1_P5p_wt) <- c("Evol_data_CEW1_CONTROL_P5p_wt_mod","Evol_data_CEW1_MA_P5p_wt_mod","Evol_data_CEW1_Vm_P5p_wt_mod")
        Evol_data_CEW1_P5p_wt <- cbind(Models = rownames(Evol_data_CEW1_P5p_wt),Evol_data_CEW1_P5p_wt)
        rownames(Evol_data_CEW1_P5p_wt) <- NULL
        Evol_data_CEW1_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        Evol_data_CEW1_P5p_wt$Treatment <- c("Control","ML","Vm")
        Evol_data_CEW1_P5p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_CEW1_P5p_wt$Scale <- c("data","data","data")
        Evol_data_CEW1_P5p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_CEW1_P5p_wt
      }
      
      #Summary trait_mean_data_CEW1_P5p_wt
      {
        mean_trait_mean_data_CEW1_P5p_wt <- rbind(mean(trait_mean_data_CEW1_CONTROL_P5p_wt_mod),mean(trait_mean_data_CEW1_MA_P5p_wt_mod),mean(trait_mean_data_CEW1_Vm_P5p_wt_mod))
        colnames(mean_trait_mean_data_CEW1_P5p_wt) <- c("mean")
        median_trait_mean_data_CEW1_P5p_wt <- rbind(median(trait_mean_data_CEW1_CONTROL_P5p_wt_mod),median(trait_mean_data_CEW1_MA_P5p_wt_mod),median(trait_mean_data_CEW1_Vm_P5p_wt_mod))
        colnames(median_trait_mean_data_CEW1_P5p_wt) <- c("median")
        posterior.mode_trait_mean_data_CEW1_P5p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_CEW1_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_CEW1_MA_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_CEW1_Vm_P5p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_CEW1_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_CEW1_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_CEW1_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_CEW1_MA_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_CEW1_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_CEW1_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_CEW1_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_CEW1_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_CEW1_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_CEW1_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_CEW1_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_CEW1_P5p_wt <- rbind(effectiveSize(trait_mean_data_CEW1_CONTROL_P5p_wt_mod),effectiveSize(trait_mean_data_CEW1_MA_P5p_wt_mod),effectiveSize(trait_mean_data_CEW1_Vm_P5p_wt_mod))
        colnames(effectiveSize_trait_mean_data_CEW1_P5p_wt) <- c("effectiveSize")
        trait_mean_data_CEW1_P5p_wt <- cbind.data.frame(mean_trait_mean_data_CEW1_P5p_wt,median_trait_mean_data_CEW1_P5p_wt,posterior.mode_trait_mean_data_CEW1_P5p_wt,HPDinterval_0.95_trait_mean_data_CEW1_P5p_wt,HPDinterval_0.83_trait_mean_data_CEW1_P5p_wt,effectiveSize_trait_mean_data_CEW1_P5p_wt)
        rownames(trait_mean_data_CEW1_P5p_wt) <- c("trait_mean_data_CEW1_CONTROL_P5p_wt_mod","trait_mean_data_CEW1_MA_P5p_wt_mod","trait_mean_data_CEW1_Vm_P5p_wt_mod")
        trait_mean_data_CEW1_P5p_wt <- cbind(Models = rownames(trait_mean_data_CEW1_P5p_wt),trait_mean_data_CEW1_P5p_wt)
        rownames(trait_mean_data_CEW1_P5p_wt) <- NULL
        trait_mean_data_CEW1_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        trait_mean_data_CEW1_P5p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_data_CEW1_P5p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_CEW1_P5p_wt$Scale <- c("data","data","data")
        trait_mean_data_CEW1_P5p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_CEW1_P5p_wt
      }
      
      data_CEW1_P5p_wt <- rbind.data.frame(va_data_CEW1_P5p_wt, h2_data_CEW1_P5p_wt,Evol_data_CEW1_P5p_wt,trait_mean_data_CEW1_P5p_wt)
      data_CEW1_P5p_wt
      
    }
    Vm_CEW1_P5p_wt <- rbind.data.frame(liab_CEW1_P5p_wt, data_CEW1_P5p_wt)
    Vm_CEW1_P5p_wt$Pnp_fate <- rep("wt", 24)
    Vm_CEW1_P5p_wt
    #remove CEW1 P5p_wt models
    {
      remove(CEW1_CONTROL_P5p_wt_mod)
      remove(CEW1_MA_P5p_wt_mod)
      remove(CEW1_Vm_P5p_wt_mod)
    }
  }
  
  ##Summary CEW1 P6p----
  {
    #Summary liability scale CEW1 P6p
    {
      #Summary va_liab_CEW1_P6p_wt: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_CEW1_P6p_wt <- rbind(mean(va_liab_CEW1_CONTROL_P6p_wt_mod/2),mean(va_liab_CEW1_MA_P6p_wt_mod/2),mean(va_liab_CEW1_Vm_P6p_wt_mod/2))
        colnames(mean_va_liab_CEW1_P6p_wt) <- c("mean")
        median_va_liab_CEW1_P6p_wt <- rbind(median(va_liab_CEW1_CONTROL_P6p_wt_mod/2),median(va_liab_CEW1_MA_P6p_wt_mod/2),median(va_liab_CEW1_Vm_P6p_wt_mod/2))
        colnames(median_va_liab_CEW1_P6p_wt) <- c("median")
        posterior.mode_va_liab_CEW1_P6p_wt <- rbind(posterior.mode(va_liab_CEW1_CONTROL_P6p_wt_mod/2),posterior.mode(va_liab_CEW1_MA_P6p_wt_mod/2),posterior.mode(va_liab_CEW1_Vm_P6p_wt_mod/2))
        colnames(posterior.mode_va_liab_CEW1_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_CEW1_P6p_wt <- rbind(HPDinterval(va_liab_CEW1_CONTROL_P6p_wt_mod/2),HPDinterval(va_liab_CEW1_MA_P6p_wt_mod/2),HPDinterval(va_liab_CEW1_Vm_P6p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_CEW1_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_CEW1_P6p_wt <- rbind(HPDinterval(va_liab_CEW1_CONTROL_P6p_wt_mod/2,prob=.83),HPDinterval(va_liab_CEW1_MA_P6p_wt_mod/2,prob=.83),HPDinterval(va_liab_CEW1_Vm_P6p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_CEW1_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_CEW1_P6p_wt <- rbind(effectiveSize(va_liab_CEW1_CONTROL_P6p_wt_mod/2),effectiveSize(va_liab_CEW1_MA_P6p_wt_mod/2),effectiveSize(va_liab_CEW1_Vm_P6p_wt_mod/2))
        colnames(effectiveSize_va_liab_CEW1_P6p_wt) <- c("effectiveSize")
        va_liab_CEW1_P6p_wt <- cbind.data.frame(mean_va_liab_CEW1_P6p_wt,median_va_liab_CEW1_P6p_wt,posterior.mode_va_liab_CEW1_P6p_wt,HPDinterval_0.95_va_liab_CEW1_P6p_wt,HPDinterval_0.83_va_liab_CEW1_P6p_wt,effectiveSize_va_liab_CEW1_P6p_wt)
        rownames(va_liab_CEW1_P6p_wt) <- c("va_liab_CEW1_CONTROL_P6p_wt_mod","va_liab_CEW1_MA_P6p_wt_mod","va_liab_CEW1_Vm_P6p_wt_mod")
        va_liab_CEW1_P6p_wt <- cbind(Models = rownames(va_liab_CEW1_P6p_wt),va_liab_CEW1_P6p_wt)
        rownames(va_liab_CEW1_P6p_wt) <- NULL
        va_liab_CEW1_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        va_liab_CEW1_P6p_wt$Treatment <- c("Control","ML","Vm")
        va_liab_CEW1_P6p_wt$Measure <- c("Va","Va","Va")
        va_liab_CEW1_P6p_wt$Scale <- c("liab","liab","liab")
        va_liab_CEW1_P6p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_CEW1_P6p_wt
      }
      
      #Summary h2_liab_CEW1_P6p_wt
      {
        mean_h2_liab_CEW1_P6p_wt <- rbind(mean(h2_liab_CEW1_CONTROL_P6p_wt_mod),mean(h2_liab_CEW1_MA_P6p_wt_mod),mean(h2_liab_CEW1_Vm_P6p_wt_mod))
        colnames(mean_h2_liab_CEW1_P6p_wt) <- c("mean")
        median_h2_liab_CEW1_P6p_wt <- rbind(median(h2_liab_CEW1_CONTROL_P6p_wt_mod),median(h2_liab_CEW1_MA_P6p_wt_mod),median(h2_liab_CEW1_Vm_P6p_wt_mod))
        colnames(median_h2_liab_CEW1_P6p_wt) <- c("median")
        posterior.mode_h2_liab_CEW1_P6p_wt <- rbind(posterior.mode(h2_liab_CEW1_CONTROL_P6p_wt_mod),posterior.mode(h2_liab_CEW1_MA_P6p_wt_mod),posterior.mode(h2_liab_CEW1_Vm_P6p_wt_mod))
        colnames(posterior.mode_h2_liab_CEW1_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_CEW1_P6p_wt <- rbind(HPDinterval(h2_liab_CEW1_CONTROL_P6p_wt_mod),HPDinterval(h2_liab_CEW1_MA_P6p_wt_mod),HPDinterval(h2_liab_CEW1_Vm_P6p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_CEW1_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_CEW1_P6p_wt <- rbind(HPDinterval(h2_liab_CEW1_CONTROL_P6p_wt_mod,prob=.83),HPDinterval(h2_liab_CEW1_MA_P6p_wt_mod,prob=.83),HPDinterval(h2_liab_CEW1_Vm_P6p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_CEW1_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_CEW1_P6p_wt <- rbind(effectiveSize(h2_liab_CEW1_CONTROL_P6p_wt_mod),effectiveSize(h2_liab_CEW1_MA_P6p_wt_mod),effectiveSize(h2_liab_CEW1_Vm_P6p_wt_mod))
        colnames(effectiveSize_h2_liab_CEW1_P6p_wt) <- c("effectiveSize")
        h2_liab_CEW1_P6p_wt <- cbind.data.frame(mean_h2_liab_CEW1_P6p_wt,median_h2_liab_CEW1_P6p_wt,posterior.mode_h2_liab_CEW1_P6p_wt,HPDinterval_0.95_h2_liab_CEW1_P6p_wt,HPDinterval_0.83_h2_liab_CEW1_P6p_wt,effectiveSize_h2_liab_CEW1_P6p_wt)
        rownames(h2_liab_CEW1_P6p_wt) <- c("h2_liab_CEW1_CONTROL_P6p_wt_mod","h2_liab_CEW1_MA_P6p_wt_mod","h2_liab_CEW1_Vm_P6p_wt_mod")
        h2_liab_CEW1_P6p_wt <- cbind(Models = rownames(h2_liab_CEW1_P6p_wt),h2_liab_CEW1_P6p_wt)
        rownames(h2_liab_CEW1_P6p_wt) <- NULL
        h2_liab_CEW1_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        h2_liab_CEW1_P6p_wt$Treatment <- c("Control","ML","Vm")
        h2_liab_CEW1_P6p_wt$Measure <- c("H2","H2","H2")
        h2_liab_CEW1_P6p_wt$Scale <- c("liab","liab","liab")
        h2_liab_CEW1_P6p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_CEW1_P6p_wt
      }
      
      #Summary Evol_liab_CEW1_P6p_wt
      {
        mean_Evol_liab_CEW1_P6p_wt <- rbind(mean(Evol_liab_CEW1_CONTROL_P6p_wt_mod),mean(Evol_liab_CEW1_MA_P6p_wt_mod),mean(Evol_liab_CEW1_Vm_P6p_wt_mod))
        colnames(mean_Evol_liab_CEW1_P6p_wt) <- c("mean")
        median_Evol_liab_CEW1_P6p_wt <- rbind(median(Evol_liab_CEW1_CONTROL_P6p_wt_mod),median(Evol_liab_CEW1_MA_P6p_wt_mod),median(Evol_liab_CEW1_Vm_P6p_wt_mod))
        colnames(median_Evol_liab_CEW1_P6p_wt) <- c("median")
        posterior.mode_Evol_liab_CEW1_P6p_wt <- rbind(posterior.mode(Evol_liab_CEW1_CONTROL_P6p_wt_mod),posterior.mode(Evol_liab_CEW1_MA_P6p_wt_mod),posterior.mode(Evol_liab_CEW1_Vm_P6p_wt_mod))
        colnames(posterior.mode_Evol_liab_CEW1_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_CEW1_P6p_wt <- rbind(HPDinterval(Evol_liab_CEW1_CONTROL_P6p_wt_mod),HPDinterval(Evol_liab_CEW1_MA_P6p_wt_mod),HPDinterval(Evol_liab_CEW1_Vm_P6p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_CEW1_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_CEW1_P6p_wt <- rbind(HPDinterval(Evol_liab_CEW1_CONTROL_P6p_wt_mod,prob=.83),HPDinterval(Evol_liab_CEW1_MA_P6p_wt_mod,prob=.83),HPDinterval(Evol_liab_CEW1_Vm_P6p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_CEW1_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_CEW1_P6p_wt <- rbind(effectiveSize(Evol_liab_CEW1_CONTROL_P6p_wt_mod),effectiveSize(Evol_liab_CEW1_MA_P6p_wt_mod),effectiveSize(Evol_liab_CEW1_Vm_P6p_wt_mod))
        colnames(effectiveSize_Evol_liab_CEW1_P6p_wt) <- c("effectiveSize")
        Evol_liab_CEW1_P6p_wt <- cbind.data.frame(mean_Evol_liab_CEW1_P6p_wt,median_Evol_liab_CEW1_P6p_wt,posterior.mode_Evol_liab_CEW1_P6p_wt,HPDinterval_0.95_Evol_liab_CEW1_P6p_wt,HPDinterval_0.83_Evol_liab_CEW1_P6p_wt,effectiveSize_Evol_liab_CEW1_P6p_wt)
        rownames(Evol_liab_CEW1_P6p_wt) <- c("Evol_liab_CEW1_CONTROL_P6p_wt_mod","Evol_liab_CEW1_MA_P6p_wt_mod","Evol_liab_CEW1_Vm_P6p_wt_mod")
        Evol_liab_CEW1_P6p_wt <- cbind(Models = rownames(Evol_liab_CEW1_P6p_wt),Evol_liab_CEW1_P6p_wt)
        rownames(Evol_liab_CEW1_P6p_wt) <- NULL
        Evol_liab_CEW1_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        Evol_liab_CEW1_P6p_wt$Treatment <- c("Control","ML","Vm")
        Evol_liab_CEW1_P6p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_CEW1_P6p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_CEW1_P6p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_CEW1_P6p_wt
      }
      
      #Summary trait_mean_liab_CEW1_P6p_wt
      {
        mean_trait_mean_liab_CEW1_P6p_wt <- rbind(mean(trait_mean_liab_CEW1_CONTROL_P6p_wt_mod),mean(trait_mean_liab_CEW1_MA_P6p_wt_mod),mean(trait_mean_liab_CEW1_Vm_P6p_wt_mod))
        colnames(mean_trait_mean_liab_CEW1_P6p_wt) <- c("mean")
        median_trait_mean_liab_CEW1_P6p_wt <- rbind(median(trait_mean_liab_CEW1_CONTROL_P6p_wt_mod),median(trait_mean_liab_CEW1_MA_P6p_wt_mod),median(trait_mean_liab_CEW1_Vm_P6p_wt_mod))
        colnames(median_trait_mean_liab_CEW1_P6p_wt) <- c("median")
        posterior.mode_trait_mean_liab_CEW1_P6p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_CEW1_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_CEW1_MA_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_CEW1_Vm_P6p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_CEW1_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_CEW1_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_CEW1_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_CEW1_MA_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_CEW1_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_CEW1_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_CEW1_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_CEW1_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_CEW1_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_CEW1_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_CEW1_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_CEW1_P6p_wt <- rbind(effectiveSize(trait_mean_liab_CEW1_CONTROL_P6p_wt_mod),effectiveSize(trait_mean_liab_CEW1_MA_P6p_wt_mod),effectiveSize(trait_mean_liab_CEW1_Vm_P6p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_CEW1_P6p_wt) <- c("effectiveSize")
        trait_mean_liab_CEW1_P6p_wt <- cbind.data.frame(mean_trait_mean_liab_CEW1_P6p_wt,median_trait_mean_liab_CEW1_P6p_wt,posterior.mode_trait_mean_liab_CEW1_P6p_wt,HPDinterval_0.95_trait_mean_liab_CEW1_P6p_wt,HPDinterval_0.83_trait_mean_liab_CEW1_P6p_wt,effectiveSize_trait_mean_liab_CEW1_P6p_wt)
        rownames(trait_mean_liab_CEW1_P6p_wt) <- c("trait_mean_liab_CEW1_CONTROL_P6p_wt_mod","trait_mean_liab_CEW1_MA_P6p_wt_mod","trait_mean_liab_CEW1_Vm_P6p_wt_mod")
        trait_mean_liab_CEW1_P6p_wt <- cbind(Models = rownames(trait_mean_liab_CEW1_P6p_wt),trait_mean_liab_CEW1_P6p_wt)
        rownames(trait_mean_liab_CEW1_P6p_wt) <- NULL
        trait_mean_liab_CEW1_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        trait_mean_liab_CEW1_P6p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_CEW1_P6p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_CEW1_P6p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_CEW1_P6p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_CEW1_P6p_wt
      }
      
      liab_CEW1_P6p_wt <- rbind.data.frame(va_liab_CEW1_P6p_wt, h2_liab_CEW1_P6p_wt,Evol_liab_CEW1_P6p_wt,trait_mean_liab_CEW1_P6p_wt)
      liab_CEW1_P6p_wt
    }
    #Summary data scale CEW1 P6p
    {
      #Summary va_data_CEW1_P6p_wt:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_CEW1_P6p_wt <- rbind(mean(va_data_CEW1_CONTROL_P6p_wt_mod/2),mean(va_data_CEW1_MA_P6p_wt_mod/2),mean(va_data_CEW1_Vm_P6p_wt_mod/2))
        colnames(mean_va_data_CEW1_P6p_wt) <- c("mean")
        median_va_data_CEW1_P6p_wt <- rbind(median(va_data_CEW1_CONTROL_P6p_wt_mod/2),median(va_data_CEW1_MA_P6p_wt_mod/2),median(va_data_CEW1_Vm_P6p_wt_mod/2))
        colnames(median_va_data_CEW1_P6p_wt) <- c("median")
        posterior.mode_va_data_CEW1_P6p_wt <- rbind(posterior.mode(as.mcmc(va_data_CEW1_CONTROL_P6p_wt_mod/2)),posterior.mode(as.mcmc(va_data_CEW1_MA_P6p_wt_mod/2)),posterior.mode(as.mcmc(va_data_CEW1_Vm_P6p_wt_mod/2)))
        colnames(posterior.mode_va_data_CEW1_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_CEW1_P6p_wt <- rbind(HPDinterval(as.mcmc(va_data_CEW1_CONTROL_P6p_wt_mod/2)),HPDinterval(as.mcmc(va_data_CEW1_MA_P6p_wt_mod/2)),HPDinterval(as.mcmc(va_data_CEW1_Vm_P6p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_CEW1_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_CEW1_P6p_wt <- rbind(HPDinterval(as.mcmc(va_data_CEW1_CONTROL_P6p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_CEW1_MA_P6p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_CEW1_Vm_P6p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_CEW1_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_CEW1_P6p_wt <- rbind(effectiveSize(va_data_CEW1_CONTROL_P6p_wt_mod/2),effectiveSize(va_data_CEW1_MA_P6p_wt_mod/2),effectiveSize(va_data_CEW1_Vm_P6p_wt_mod/2))
        colnames(effectiveSize_va_data_CEW1_P6p_wt) <- c("effectiveSize")
        va_data_CEW1_P6p_wt <- cbind.data.frame(mean_va_data_CEW1_P6p_wt,median_va_data_CEW1_P6p_wt,posterior.mode_va_data_CEW1_P6p_wt,HPDinterval_0.95_va_data_CEW1_P6p_wt,HPDinterval_0.83_va_data_CEW1_P6p_wt,effectiveSize_va_data_CEW1_P6p_wt)
        rownames(va_data_CEW1_P6p_wt) <- c("va_data_CEW1_CONTROL_P6p_wt_mod","va_data_CEW1_MA_P6p_wt_mod","va_data_CEW1_Vm_P6p_wt_mod")
        va_data_CEW1_P6p_wt <- cbind(Models = rownames(va_data_CEW1_P6p_wt),va_data_CEW1_P6p_wt)
        rownames(va_data_CEW1_P6p_wt) <- NULL
        va_data_CEW1_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        va_data_CEW1_P6p_wt$Treatment <- c("Control","ML","Vm")
        va_data_CEW1_P6p_wt$Measure <- c("Va","Va","Va")
        va_data_CEW1_P6p_wt$Scale <- c("data","data","data")
        va_data_CEW1_P6p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_CEW1_P6p_wt
      }
      
      #Summary h2_data_CEW1_P6p_wt
      {
        mean_h2_data_CEW1_P6p_wt <- rbind(mean(h2_data_CEW1_CONTROL_P6p_wt_mod),mean(h2_data_CEW1_MA_P6p_wt_mod),mean(h2_data_CEW1_Vm_P6p_wt_mod))
        colnames(mean_h2_data_CEW1_P6p_wt) <- c("mean")
        median_h2_data_CEW1_P6p_wt <- rbind(median(h2_data_CEW1_CONTROL_P6p_wt_mod),median(h2_data_CEW1_MA_P6p_wt_mod),median(h2_data_CEW1_Vm_P6p_wt_mod))
        colnames(median_h2_data_CEW1_P6p_wt) <- c("median")
        posterior.mode_h2_data_CEW1_P6p_wt <- rbind(posterior.mode(as.mcmc(h2_data_CEW1_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(h2_data_CEW1_MA_P6p_wt_mod)),posterior.mode(as.mcmc(h2_data_CEW1_Vm_P6p_wt_mod)))
        colnames(posterior.mode_h2_data_CEW1_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_CEW1_P6p_wt <- rbind(HPDinterval(as.mcmc(h2_data_CEW1_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(h2_data_CEW1_MA_P6p_wt_mod)),HPDinterval(as.mcmc(h2_data_CEW1_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_CEW1_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_CEW1_P6p_wt <- rbind(HPDinterval(as.mcmc(h2_data_CEW1_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_CEW1_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_CEW1_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_CEW1_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_CEW1_P6p_wt <- rbind(effectiveSize(h2_data_CEW1_CONTROL_P6p_wt_mod),effectiveSize(h2_data_CEW1_MA_P6p_wt_mod),effectiveSize(h2_data_CEW1_Vm_P6p_wt_mod))
        colnames(effectiveSize_h2_data_CEW1_P6p_wt) <- c("effectiveSize")
        h2_data_CEW1_P6p_wt <- cbind.data.frame(mean_h2_data_CEW1_P6p_wt,median_h2_data_CEW1_P6p_wt,posterior.mode_h2_data_CEW1_P6p_wt,HPDinterval_0.95_h2_data_CEW1_P6p_wt,HPDinterval_0.83_h2_data_CEW1_P6p_wt,effectiveSize_h2_data_CEW1_P6p_wt)
        rownames(h2_data_CEW1_P6p_wt) <- c("h2_data_CEW1_CONTROL_P6p_wt_mod","h2_data_CEW1_MA_P6p_wt_mod","h2_data_CEW1_Vm_P6p_wt_mod")
        h2_data_CEW1_P6p_wt <- cbind(Models = rownames(h2_data_CEW1_P6p_wt),h2_data_CEW1_P6p_wt)
        rownames(h2_data_CEW1_P6p_wt) <- NULL
        h2_data_CEW1_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        h2_data_CEW1_P6p_wt$Treatment <- c("Control","ML","Vm")
        h2_data_CEW1_P6p_wt$Measure <- c("H2","H2","H2")
        h2_data_CEW1_P6p_wt$Scale <- c("data","data","data")
        h2_data_CEW1_P6p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_CEW1_P6p_wt
      }
      
      #Summary Evol_data_CEW1_P6p_wt
      {
        mean_Evol_data_CEW1_P6p_wt <- rbind(mean(Evol_data_CEW1_CONTROL_P6p_wt_mod),mean(Evol_data_CEW1_MA_P6p_wt_mod),mean(Evol_data_CEW1_Vm_P6p_wt_mod))
        colnames(mean_Evol_data_CEW1_P6p_wt) <- c("mean")
        median_Evol_data_CEW1_P6p_wt <- rbind(median(Evol_data_CEW1_CONTROL_P6p_wt_mod),median(Evol_data_CEW1_MA_P6p_wt_mod),median(Evol_data_CEW1_Vm_P6p_wt_mod))
        colnames(median_Evol_data_CEW1_P6p_wt) <- c("median")
        posterior.mode_Evol_data_CEW1_P6p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_CEW1_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(Evol_data_CEW1_MA_P6p_wt_mod)),posterior.mode(as.mcmc(Evol_data_CEW1_Vm_P6p_wt_mod)))
        colnames(posterior.mode_Evol_data_CEW1_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_CEW1_P6p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_CEW1_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(Evol_data_CEW1_MA_P6p_wt_mod)),HPDinterval(as.mcmc(Evol_data_CEW1_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_CEW1_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_CEW1_P6p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_CEW1_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_CEW1_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_CEW1_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_CEW1_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_CEW1_P6p_wt <- rbind(effectiveSize(Evol_data_CEW1_CONTROL_P6p_wt_mod),effectiveSize(Evol_data_CEW1_MA_P6p_wt_mod),effectiveSize(Evol_data_CEW1_Vm_P6p_wt_mod))
        colnames(effectiveSize_Evol_data_CEW1_P6p_wt) <- c("effectiveSize")
        Evol_data_CEW1_P6p_wt <- cbind.data.frame(mean_Evol_data_CEW1_P6p_wt,median_Evol_data_CEW1_P6p_wt,posterior.mode_Evol_data_CEW1_P6p_wt,HPDinterval_0.95_Evol_data_CEW1_P6p_wt,HPDinterval_0.83_Evol_data_CEW1_P6p_wt,effectiveSize_Evol_data_CEW1_P6p_wt)
        rownames(Evol_data_CEW1_P6p_wt) <- c("Evol_data_CEW1_CONTROL_P6p_wt_mod","Evol_data_CEW1_MA_P6p_wt_mod","Evol_data_CEW1_Vm_P6p_wt_mod")
        Evol_data_CEW1_P6p_wt <- cbind(Models = rownames(Evol_data_CEW1_P6p_wt),Evol_data_CEW1_P6p_wt)
        rownames(Evol_data_CEW1_P6p_wt) <- NULL
        Evol_data_CEW1_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        Evol_data_CEW1_P6p_wt$Treatment <- c("Control","ML","Vm")
        Evol_data_CEW1_P6p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_CEW1_P6p_wt$Scale <- c("data","data","data")
        Evol_data_CEW1_P6p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_CEW1_P6p_wt
      }
      
      #Summary trait_mean_data_CEW1_P6p_wt
      {
        mean_trait_mean_data_CEW1_P6p_wt <- rbind(mean(trait_mean_data_CEW1_CONTROL_P6p_wt_mod),mean(trait_mean_data_CEW1_MA_P6p_wt_mod),mean(trait_mean_data_CEW1_Vm_P6p_wt_mod))
        colnames(mean_trait_mean_data_CEW1_P6p_wt) <- c("mean")
        median_trait_mean_data_CEW1_P6p_wt <- rbind(median(trait_mean_data_CEW1_CONTROL_P6p_wt_mod),median(trait_mean_data_CEW1_MA_P6p_wt_mod),median(trait_mean_data_CEW1_Vm_P6p_wt_mod))
        colnames(median_trait_mean_data_CEW1_P6p_wt) <- c("median")
        posterior.mode_trait_mean_data_CEW1_P6p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_CEW1_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_CEW1_MA_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_CEW1_Vm_P6p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_CEW1_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_CEW1_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_CEW1_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_CEW1_MA_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_CEW1_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_CEW1_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_CEW1_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_CEW1_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_CEW1_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_CEW1_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_CEW1_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_CEW1_P6p_wt <- rbind(effectiveSize(trait_mean_data_CEW1_CONTROL_P6p_wt_mod),effectiveSize(trait_mean_data_CEW1_MA_P6p_wt_mod),effectiveSize(trait_mean_data_CEW1_Vm_P6p_wt_mod))
        colnames(effectiveSize_trait_mean_data_CEW1_P6p_wt) <- c("effectiveSize")
        trait_mean_data_CEW1_P6p_wt <- cbind.data.frame(mean_trait_mean_data_CEW1_P6p_wt,median_trait_mean_data_CEW1_P6p_wt,posterior.mode_trait_mean_data_CEW1_P6p_wt,HPDinterval_0.95_trait_mean_data_CEW1_P6p_wt,HPDinterval_0.83_trait_mean_data_CEW1_P6p_wt,effectiveSize_trait_mean_data_CEW1_P6p_wt)
        rownames(trait_mean_data_CEW1_P6p_wt) <- c("trait_mean_data_CEW1_CONTROL_P6p_wt_mod","trait_mean_data_CEW1_MA_P6p_wt_mod","trait_mean_data_CEW1_Vm_P6p_wt_mod")
        trait_mean_data_CEW1_P6p_wt <- cbind(Models = rownames(trait_mean_data_CEW1_P6p_wt),trait_mean_data_CEW1_P6p_wt)
        rownames(trait_mean_data_CEW1_P6p_wt) <- NULL
        trait_mean_data_CEW1_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        trait_mean_data_CEW1_P6p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_data_CEW1_P6p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_CEW1_P6p_wt$Scale <- c("data","data","data")
        trait_mean_data_CEW1_P6p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_CEW1_P6p_wt
      }
      
      data_CEW1_P6p_wt <- rbind.data.frame(va_data_CEW1_P6p_wt, h2_data_CEW1_P6p_wt,Evol_data_CEW1_P6p_wt,trait_mean_data_CEW1_P6p_wt)
      data_CEW1_P6p_wt
      
    }
    Vm_CEW1_P6p_wt <- rbind.data.frame(liab_CEW1_P6p_wt, data_CEW1_P6p_wt)
    Vm_CEW1_P6p_wt$Pnp_fate <- rep("wt", 24)
    Vm_CEW1_P6p_wt
    #remove CEW1 P6p_wt models
    {
      remove(CEW1_CONTROL_P6p_wt_mod)
      remove(CEW1_MA_P6p_wt_mod)
      remove(CEW1_Vm_P6p_wt_mod)
    }
  }
  
  ##Summary CEW1 P7p----
  {
    #Summary liability scale CEW1 P7p
    {
      #Summary va_liab_CEW1_P7p_wt: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_CEW1_P7p_wt <- rbind(mean(va_liab_CEW1_CONTROL_P7p_wt_mod/2),mean(va_liab_CEW1_MA_P7p_wt_mod/2),mean(va_liab_CEW1_Vm_P7p_wt_mod/2))
        colnames(mean_va_liab_CEW1_P7p_wt) <- c("mean")
        median_va_liab_CEW1_P7p_wt <- rbind(median(va_liab_CEW1_CONTROL_P7p_wt_mod/2),median(va_liab_CEW1_MA_P7p_wt_mod/2),median(va_liab_CEW1_Vm_P7p_wt_mod/2))
        colnames(median_va_liab_CEW1_P7p_wt) <- c("median")
        posterior.mode_va_liab_CEW1_P7p_wt <- rbind(posterior.mode(va_liab_CEW1_CONTROL_P7p_wt_mod/2),posterior.mode(va_liab_CEW1_MA_P7p_wt_mod/2),posterior.mode(va_liab_CEW1_Vm_P7p_wt_mod/2))
        colnames(posterior.mode_va_liab_CEW1_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_CEW1_P7p_wt <- rbind(HPDinterval(va_liab_CEW1_CONTROL_P7p_wt_mod/2),HPDinterval(va_liab_CEW1_MA_P7p_wt_mod/2),HPDinterval(va_liab_CEW1_Vm_P7p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_CEW1_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_CEW1_P7p_wt <- rbind(HPDinterval(va_liab_CEW1_CONTROL_P7p_wt_mod/2,prob=.83),HPDinterval(va_liab_CEW1_MA_P7p_wt_mod/2,prob=.83),HPDinterval(va_liab_CEW1_Vm_P7p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_CEW1_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_CEW1_P7p_wt <- rbind(effectiveSize(va_liab_CEW1_CONTROL_P7p_wt_mod/2),effectiveSize(va_liab_CEW1_MA_P7p_wt_mod/2),effectiveSize(va_liab_CEW1_Vm_P7p_wt_mod/2))
        colnames(effectiveSize_va_liab_CEW1_P7p_wt) <- c("effectiveSize")
        va_liab_CEW1_P7p_wt <- cbind.data.frame(mean_va_liab_CEW1_P7p_wt,median_va_liab_CEW1_P7p_wt,posterior.mode_va_liab_CEW1_P7p_wt,HPDinterval_0.95_va_liab_CEW1_P7p_wt,HPDinterval_0.83_va_liab_CEW1_P7p_wt,effectiveSize_va_liab_CEW1_P7p_wt)
        rownames(va_liab_CEW1_P7p_wt) <- c("va_liab_CEW1_CONTROL_P7p_wt_mod","va_liab_CEW1_MA_P7p_wt_mod","va_liab_CEW1_Vm_P7p_wt_mod")
        va_liab_CEW1_P7p_wt <- cbind(Models = rownames(va_liab_CEW1_P7p_wt),va_liab_CEW1_P7p_wt)
        rownames(va_liab_CEW1_P7p_wt) <- NULL
        va_liab_CEW1_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        va_liab_CEW1_P7p_wt$Treatment <- c("Control","ML","Vm")
        va_liab_CEW1_P7p_wt$Measure <- c("Va","Va","Va")
        va_liab_CEW1_P7p_wt$Scale <- c("liab","liab","liab")
        va_liab_CEW1_P7p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_CEW1_P7p_wt
      }
      
      #Summary h2_liab_CEW1_P7p_wt
      {
        mean_h2_liab_CEW1_P7p_wt <- rbind(mean(h2_liab_CEW1_CONTROL_P7p_wt_mod),mean(h2_liab_CEW1_MA_P7p_wt_mod),mean(h2_liab_CEW1_Vm_P7p_wt_mod))
        colnames(mean_h2_liab_CEW1_P7p_wt) <- c("mean")
        median_h2_liab_CEW1_P7p_wt <- rbind(median(h2_liab_CEW1_CONTROL_P7p_wt_mod),median(h2_liab_CEW1_MA_P7p_wt_mod),median(h2_liab_CEW1_Vm_P7p_wt_mod))
        colnames(median_h2_liab_CEW1_P7p_wt) <- c("median")
        posterior.mode_h2_liab_CEW1_P7p_wt <- rbind(posterior.mode(h2_liab_CEW1_CONTROL_P7p_wt_mod),posterior.mode(h2_liab_CEW1_MA_P7p_wt_mod),posterior.mode(h2_liab_CEW1_Vm_P7p_wt_mod))
        colnames(posterior.mode_h2_liab_CEW1_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_CEW1_P7p_wt <- rbind(HPDinterval(h2_liab_CEW1_CONTROL_P7p_wt_mod),HPDinterval(h2_liab_CEW1_MA_P7p_wt_mod),HPDinterval(h2_liab_CEW1_Vm_P7p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_CEW1_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_CEW1_P7p_wt <- rbind(HPDinterval(h2_liab_CEW1_CONTROL_P7p_wt_mod,prob=.83),HPDinterval(h2_liab_CEW1_MA_P7p_wt_mod,prob=.83),HPDinterval(h2_liab_CEW1_Vm_P7p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_CEW1_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_CEW1_P7p_wt <- rbind(effectiveSize(h2_liab_CEW1_CONTROL_P7p_wt_mod),effectiveSize(h2_liab_CEW1_MA_P7p_wt_mod),effectiveSize(h2_liab_CEW1_Vm_P7p_wt_mod))
        colnames(effectiveSize_h2_liab_CEW1_P7p_wt) <- c("effectiveSize")
        h2_liab_CEW1_P7p_wt <- cbind.data.frame(mean_h2_liab_CEW1_P7p_wt,median_h2_liab_CEW1_P7p_wt,posterior.mode_h2_liab_CEW1_P7p_wt,HPDinterval_0.95_h2_liab_CEW1_P7p_wt,HPDinterval_0.83_h2_liab_CEW1_P7p_wt,effectiveSize_h2_liab_CEW1_P7p_wt)
        rownames(h2_liab_CEW1_P7p_wt) <- c("h2_liab_CEW1_CONTROL_P7p_wt_mod","h2_liab_CEW1_MA_P7p_wt_mod","h2_liab_CEW1_Vm_P7p_wt_mod")
        h2_liab_CEW1_P7p_wt <- cbind(Models = rownames(h2_liab_CEW1_P7p_wt),h2_liab_CEW1_P7p_wt)
        rownames(h2_liab_CEW1_P7p_wt) <- NULL
        h2_liab_CEW1_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        h2_liab_CEW1_P7p_wt$Treatment <- c("Control","ML","Vm")
        h2_liab_CEW1_P7p_wt$Measure <- c("H2","H2","H2")
        h2_liab_CEW1_P7p_wt$Scale <- c("liab","liab","liab")
        h2_liab_CEW1_P7p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_CEW1_P7p_wt
      }
      
      #Summary Evol_liab_CEW1_P7p_wt
      {
        mean_Evol_liab_CEW1_P7p_wt <- rbind(mean(Evol_liab_CEW1_CONTROL_P7p_wt_mod),mean(Evol_liab_CEW1_MA_P7p_wt_mod),mean(Evol_liab_CEW1_Vm_P7p_wt_mod))
        colnames(mean_Evol_liab_CEW1_P7p_wt) <- c("mean")
        median_Evol_liab_CEW1_P7p_wt <- rbind(median(Evol_liab_CEW1_CONTROL_P7p_wt_mod),median(Evol_liab_CEW1_MA_P7p_wt_mod),median(Evol_liab_CEW1_Vm_P7p_wt_mod))
        colnames(median_Evol_liab_CEW1_P7p_wt) <- c("median")
        posterior.mode_Evol_liab_CEW1_P7p_wt <- rbind(posterior.mode(Evol_liab_CEW1_CONTROL_P7p_wt_mod),posterior.mode(Evol_liab_CEW1_MA_P7p_wt_mod),posterior.mode(Evol_liab_CEW1_Vm_P7p_wt_mod))
        colnames(posterior.mode_Evol_liab_CEW1_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_CEW1_P7p_wt <- rbind(HPDinterval(Evol_liab_CEW1_CONTROL_P7p_wt_mod),HPDinterval(Evol_liab_CEW1_MA_P7p_wt_mod),HPDinterval(Evol_liab_CEW1_Vm_P7p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_CEW1_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_CEW1_P7p_wt <- rbind(HPDinterval(Evol_liab_CEW1_CONTROL_P7p_wt_mod,prob=.83),HPDinterval(Evol_liab_CEW1_MA_P7p_wt_mod,prob=.83),HPDinterval(Evol_liab_CEW1_Vm_P7p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_CEW1_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_CEW1_P7p_wt <- rbind(effectiveSize(Evol_liab_CEW1_CONTROL_P7p_wt_mod),effectiveSize(Evol_liab_CEW1_MA_P7p_wt_mod),effectiveSize(Evol_liab_CEW1_Vm_P7p_wt_mod))
        colnames(effectiveSize_Evol_liab_CEW1_P7p_wt) <- c("effectiveSize")
        Evol_liab_CEW1_P7p_wt <- cbind.data.frame(mean_Evol_liab_CEW1_P7p_wt,median_Evol_liab_CEW1_P7p_wt,posterior.mode_Evol_liab_CEW1_P7p_wt,HPDinterval_0.95_Evol_liab_CEW1_P7p_wt,HPDinterval_0.83_Evol_liab_CEW1_P7p_wt,effectiveSize_Evol_liab_CEW1_P7p_wt)
        rownames(Evol_liab_CEW1_P7p_wt) <- c("Evol_liab_CEW1_CONTROL_P7p_wt_mod","Evol_liab_CEW1_MA_P7p_wt_mod","Evol_liab_CEW1_Vm_P7p_wt_mod")
        Evol_liab_CEW1_P7p_wt <- cbind(Models = rownames(Evol_liab_CEW1_P7p_wt),Evol_liab_CEW1_P7p_wt)
        rownames(Evol_liab_CEW1_P7p_wt) <- NULL
        Evol_liab_CEW1_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        Evol_liab_CEW1_P7p_wt$Treatment <- c("Control","ML","Vm")
        Evol_liab_CEW1_P7p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_CEW1_P7p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_CEW1_P7p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_CEW1_P7p_wt
      }
      
      #Summary trait_mean_liab_CEW1_P7p_wt
      {
        mean_trait_mean_liab_CEW1_P7p_wt <- rbind(mean(trait_mean_liab_CEW1_CONTROL_P7p_wt_mod),mean(trait_mean_liab_CEW1_MA_P7p_wt_mod),mean(trait_mean_liab_CEW1_Vm_P7p_wt_mod))
        colnames(mean_trait_mean_liab_CEW1_P7p_wt) <- c("mean")
        median_trait_mean_liab_CEW1_P7p_wt <- rbind(median(trait_mean_liab_CEW1_CONTROL_P7p_wt_mod),median(trait_mean_liab_CEW1_MA_P7p_wt_mod),median(trait_mean_liab_CEW1_Vm_P7p_wt_mod))
        colnames(median_trait_mean_liab_CEW1_P7p_wt) <- c("median")
        posterior.mode_trait_mean_liab_CEW1_P7p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_CEW1_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_CEW1_MA_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_CEW1_Vm_P7p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_CEW1_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_CEW1_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_CEW1_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_CEW1_MA_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_CEW1_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_CEW1_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_CEW1_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_CEW1_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_CEW1_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_CEW1_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_CEW1_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_CEW1_P7p_wt <- rbind(effectiveSize(trait_mean_liab_CEW1_CONTROL_P7p_wt_mod),effectiveSize(trait_mean_liab_CEW1_MA_P7p_wt_mod),effectiveSize(trait_mean_liab_CEW1_Vm_P7p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_CEW1_P7p_wt) <- c("effectiveSize")
        trait_mean_liab_CEW1_P7p_wt <- cbind.data.frame(mean_trait_mean_liab_CEW1_P7p_wt,median_trait_mean_liab_CEW1_P7p_wt,posterior.mode_trait_mean_liab_CEW1_P7p_wt,HPDinterval_0.95_trait_mean_liab_CEW1_P7p_wt,HPDinterval_0.83_trait_mean_liab_CEW1_P7p_wt,effectiveSize_trait_mean_liab_CEW1_P7p_wt)
        rownames(trait_mean_liab_CEW1_P7p_wt) <- c("trait_mean_liab_CEW1_CONTROL_P7p_wt_mod","trait_mean_liab_CEW1_MA_P7p_wt_mod","trait_mean_liab_CEW1_Vm_P7p_wt_mod")
        trait_mean_liab_CEW1_P7p_wt <- cbind(Models = rownames(trait_mean_liab_CEW1_P7p_wt),trait_mean_liab_CEW1_P7p_wt)
        rownames(trait_mean_liab_CEW1_P7p_wt) <- NULL
        trait_mean_liab_CEW1_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        trait_mean_liab_CEW1_P7p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_CEW1_P7p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_CEW1_P7p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_CEW1_P7p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_CEW1_P7p_wt
      }
      
      liab_CEW1_P7p_wt <- rbind.data.frame(va_liab_CEW1_P7p_wt, h2_liab_CEW1_P7p_wt,Evol_liab_CEW1_P7p_wt,trait_mean_liab_CEW1_P7p_wt)
      liab_CEW1_P7p_wt
    }
    #Summary data scale CEW1 P7p
    {
      #Summary va_data_CEW1_P7p_wt:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_CEW1_P7p_wt <- rbind(mean(va_data_CEW1_CONTROL_P7p_wt_mod/2),mean(va_data_CEW1_MA_P7p_wt_mod/2),mean(va_data_CEW1_Vm_P7p_wt_mod/2))
        colnames(mean_va_data_CEW1_P7p_wt) <- c("mean")
        median_va_data_CEW1_P7p_wt <- rbind(median(va_data_CEW1_CONTROL_P7p_wt_mod/2),median(va_data_CEW1_MA_P7p_wt_mod/2),median(va_data_CEW1_Vm_P7p_wt_mod/2))
        colnames(median_va_data_CEW1_P7p_wt) <- c("median")
        posterior.mode_va_data_CEW1_P7p_wt <- rbind(posterior.mode(as.mcmc(va_data_CEW1_CONTROL_P7p_wt_mod/2)),posterior.mode(as.mcmc(va_data_CEW1_MA_P7p_wt_mod/2)),posterior.mode(as.mcmc(va_data_CEW1_Vm_P7p_wt_mod/2)))
        colnames(posterior.mode_va_data_CEW1_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_CEW1_P7p_wt <- rbind(HPDinterval(as.mcmc(va_data_CEW1_CONTROL_P7p_wt_mod/2)),HPDinterval(as.mcmc(va_data_CEW1_MA_P7p_wt_mod/2)),HPDinterval(as.mcmc(va_data_CEW1_Vm_P7p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_CEW1_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_CEW1_P7p_wt <- rbind(HPDinterval(as.mcmc(va_data_CEW1_CONTROL_P7p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_CEW1_MA_P7p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_CEW1_Vm_P7p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_CEW1_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_CEW1_P7p_wt <- rbind(effectiveSize(va_data_CEW1_CONTROL_P7p_wt_mod/2),effectiveSize(va_data_CEW1_MA_P7p_wt_mod/2),effectiveSize(va_data_CEW1_Vm_P7p_wt_mod/2))
        colnames(effectiveSize_va_data_CEW1_P7p_wt) <- c("effectiveSize")
        va_data_CEW1_P7p_wt <- cbind.data.frame(mean_va_data_CEW1_P7p_wt,median_va_data_CEW1_P7p_wt,posterior.mode_va_data_CEW1_P7p_wt,HPDinterval_0.95_va_data_CEW1_P7p_wt,HPDinterval_0.83_va_data_CEW1_P7p_wt,effectiveSize_va_data_CEW1_P7p_wt)
        rownames(va_data_CEW1_P7p_wt) <- c("va_data_CEW1_CONTROL_P7p_wt_mod","va_data_CEW1_MA_P7p_wt_mod","va_data_CEW1_Vm_P7p_wt_mod")
        va_data_CEW1_P7p_wt <- cbind(Models = rownames(va_data_CEW1_P7p_wt),va_data_CEW1_P7p_wt)
        rownames(va_data_CEW1_P7p_wt) <- NULL
        va_data_CEW1_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        va_data_CEW1_P7p_wt$Treatment <- c("Control","ML","Vm")
        va_data_CEW1_P7p_wt$Measure <- c("Va","Va","Va")
        va_data_CEW1_P7p_wt$Scale <- c("data","data","data")
        va_data_CEW1_P7p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_CEW1_P7p_wt
      }
      
      #Summary h2_data_CEW1_P7p_wt
      {
        mean_h2_data_CEW1_P7p_wt <- rbind(mean(h2_data_CEW1_CONTROL_P7p_wt_mod),mean(h2_data_CEW1_MA_P7p_wt_mod),mean(h2_data_CEW1_Vm_P7p_wt_mod))
        colnames(mean_h2_data_CEW1_P7p_wt) <- c("mean")
        median_h2_data_CEW1_P7p_wt <- rbind(median(h2_data_CEW1_CONTROL_P7p_wt_mod),median(h2_data_CEW1_MA_P7p_wt_mod),median(h2_data_CEW1_Vm_P7p_wt_mod))
        colnames(median_h2_data_CEW1_P7p_wt) <- c("median")
        posterior.mode_h2_data_CEW1_P7p_wt <- rbind(posterior.mode(as.mcmc(h2_data_CEW1_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(h2_data_CEW1_MA_P7p_wt_mod)),posterior.mode(as.mcmc(h2_data_CEW1_Vm_P7p_wt_mod)))
        colnames(posterior.mode_h2_data_CEW1_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_CEW1_P7p_wt <- rbind(HPDinterval(as.mcmc(h2_data_CEW1_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(h2_data_CEW1_MA_P7p_wt_mod)),HPDinterval(as.mcmc(h2_data_CEW1_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_CEW1_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_CEW1_P7p_wt <- rbind(HPDinterval(as.mcmc(h2_data_CEW1_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_CEW1_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_CEW1_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_CEW1_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_CEW1_P7p_wt <- rbind(effectiveSize(h2_data_CEW1_CONTROL_P7p_wt_mod),effectiveSize(h2_data_CEW1_MA_P7p_wt_mod),effectiveSize(h2_data_CEW1_Vm_P7p_wt_mod))
        colnames(effectiveSize_h2_data_CEW1_P7p_wt) <- c("effectiveSize")
        h2_data_CEW1_P7p_wt <- cbind.data.frame(mean_h2_data_CEW1_P7p_wt,median_h2_data_CEW1_P7p_wt,posterior.mode_h2_data_CEW1_P7p_wt,HPDinterval_0.95_h2_data_CEW1_P7p_wt,HPDinterval_0.83_h2_data_CEW1_P7p_wt,effectiveSize_h2_data_CEW1_P7p_wt)
        rownames(h2_data_CEW1_P7p_wt) <- c("h2_data_CEW1_CONTROL_P7p_wt_mod","h2_data_CEW1_MA_P7p_wt_mod","h2_data_CEW1_Vm_P7p_wt_mod")
        h2_data_CEW1_P7p_wt <- cbind(Models = rownames(h2_data_CEW1_P7p_wt),h2_data_CEW1_P7p_wt)
        rownames(h2_data_CEW1_P7p_wt) <- NULL
        h2_data_CEW1_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        h2_data_CEW1_P7p_wt$Treatment <- c("Control","ML","Vm")
        h2_data_CEW1_P7p_wt$Measure <- c("H2","H2","H2")
        h2_data_CEW1_P7p_wt$Scale <- c("data","data","data")
        h2_data_CEW1_P7p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_CEW1_P7p_wt
      }
      
      #Summary Evol_data_CEW1_P7p_wt
      {
        mean_Evol_data_CEW1_P7p_wt <- rbind(mean(Evol_data_CEW1_CONTROL_P7p_wt_mod),mean(Evol_data_CEW1_MA_P7p_wt_mod),mean(Evol_data_CEW1_Vm_P7p_wt_mod))
        colnames(mean_Evol_data_CEW1_P7p_wt) <- c("mean")
        median_Evol_data_CEW1_P7p_wt <- rbind(median(Evol_data_CEW1_CONTROL_P7p_wt_mod),median(Evol_data_CEW1_MA_P7p_wt_mod),median(Evol_data_CEW1_Vm_P7p_wt_mod))
        colnames(median_Evol_data_CEW1_P7p_wt) <- c("median")
        posterior.mode_Evol_data_CEW1_P7p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_CEW1_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(Evol_data_CEW1_MA_P7p_wt_mod)),posterior.mode(as.mcmc(Evol_data_CEW1_Vm_P7p_wt_mod)))
        colnames(posterior.mode_Evol_data_CEW1_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_CEW1_P7p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_CEW1_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(Evol_data_CEW1_MA_P7p_wt_mod)),HPDinterval(as.mcmc(Evol_data_CEW1_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_CEW1_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_CEW1_P7p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_CEW1_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_CEW1_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_CEW1_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_CEW1_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_CEW1_P7p_wt <- rbind(effectiveSize(Evol_data_CEW1_CONTROL_P7p_wt_mod),effectiveSize(Evol_data_CEW1_MA_P7p_wt_mod),effectiveSize(Evol_data_CEW1_Vm_P7p_wt_mod))
        colnames(effectiveSize_Evol_data_CEW1_P7p_wt) <- c("effectiveSize")
        Evol_data_CEW1_P7p_wt <- cbind.data.frame(mean_Evol_data_CEW1_P7p_wt,median_Evol_data_CEW1_P7p_wt,posterior.mode_Evol_data_CEW1_P7p_wt,HPDinterval_0.95_Evol_data_CEW1_P7p_wt,HPDinterval_0.83_Evol_data_CEW1_P7p_wt,effectiveSize_Evol_data_CEW1_P7p_wt)
        rownames(Evol_data_CEW1_P7p_wt) <- c("Evol_data_CEW1_CONTROL_P7p_wt_mod","Evol_data_CEW1_MA_P7p_wt_mod","Evol_data_CEW1_Vm_P7p_wt_mod")
        Evol_data_CEW1_P7p_wt <- cbind(Models = rownames(Evol_data_CEW1_P7p_wt),Evol_data_CEW1_P7p_wt)
        rownames(Evol_data_CEW1_P7p_wt) <- NULL
        Evol_data_CEW1_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        Evol_data_CEW1_P7p_wt$Treatment <- c("Control","ML","Vm")
        Evol_data_CEW1_P7p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_CEW1_P7p_wt$Scale <- c("data","data","data")
        Evol_data_CEW1_P7p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_CEW1_P7p_wt
      }
      
      #Summary trait_mean_data_CEW1_P7p_wt
      {
        mean_trait_mean_data_CEW1_P7p_wt <- rbind(mean(trait_mean_data_CEW1_CONTROL_P7p_wt_mod),mean(trait_mean_data_CEW1_MA_P7p_wt_mod),mean(trait_mean_data_CEW1_Vm_P7p_wt_mod))
        colnames(mean_trait_mean_data_CEW1_P7p_wt) <- c("mean")
        median_trait_mean_data_CEW1_P7p_wt <- rbind(median(trait_mean_data_CEW1_CONTROL_P7p_wt_mod),median(trait_mean_data_CEW1_MA_P7p_wt_mod),median(trait_mean_data_CEW1_Vm_P7p_wt_mod))
        colnames(median_trait_mean_data_CEW1_P7p_wt) <- c("median")
        posterior.mode_trait_mean_data_CEW1_P7p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_CEW1_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_CEW1_MA_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_CEW1_Vm_P7p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_CEW1_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_CEW1_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_CEW1_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_CEW1_MA_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_CEW1_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_CEW1_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_CEW1_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_CEW1_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_CEW1_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_CEW1_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_CEW1_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_CEW1_P7p_wt <- rbind(effectiveSize(trait_mean_data_CEW1_CONTROL_P7p_wt_mod),effectiveSize(trait_mean_data_CEW1_MA_P7p_wt_mod),effectiveSize(trait_mean_data_CEW1_Vm_P7p_wt_mod))
        colnames(effectiveSize_trait_mean_data_CEW1_P7p_wt) <- c("effectiveSize")
        trait_mean_data_CEW1_P7p_wt <- cbind.data.frame(mean_trait_mean_data_CEW1_P7p_wt,median_trait_mean_data_CEW1_P7p_wt,posterior.mode_trait_mean_data_CEW1_P7p_wt,HPDinterval_0.95_trait_mean_data_CEW1_P7p_wt,HPDinterval_0.83_trait_mean_data_CEW1_P7p_wt,effectiveSize_trait_mean_data_CEW1_P7p_wt)
        rownames(trait_mean_data_CEW1_P7p_wt) <- c("trait_mean_data_CEW1_CONTROL_P7p_wt_mod","trait_mean_data_CEW1_MA_P7p_wt_mod","trait_mean_data_CEW1_Vm_P7p_wt_mod")
        trait_mean_data_CEW1_P7p_wt <- cbind(Models = rownames(trait_mean_data_CEW1_P7p_wt),trait_mean_data_CEW1_P7p_wt)
        rownames(trait_mean_data_CEW1_P7p_wt) <- NULL
        trait_mean_data_CEW1_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        trait_mean_data_CEW1_P7p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_data_CEW1_P7p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_CEW1_P7p_wt$Scale <- c("data","data","data")
        trait_mean_data_CEW1_P7p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_CEW1_P7p_wt
      }
      
      data_CEW1_P7p_wt <- rbind.data.frame(va_data_CEW1_P7p_wt, h2_data_CEW1_P7p_wt,Evol_data_CEW1_P7p_wt,trait_mean_data_CEW1_P7p_wt)
      data_CEW1_P7p_wt
      
    }
    Vm_CEW1_P7p_wt <- rbind.data.frame(liab_CEW1_P7p_wt, data_CEW1_P7p_wt)
    Vm_CEW1_P7p_wt$Pnp_fate <- rep("wt", 24)
    Vm_CEW1_P7p_wt
    #remove CEW1 P7p_wt models
    {
      remove(CEW1_CONTROL_P7p_wt_mod)
      remove(CEW1_MA_P7p_wt_mod)
      remove(CEW1_Vm_P7p_wt_mod)
    }
  }
  
  Vm_CEW1_SSSS_Summary <- rbind.data.frame(Vm_CEW1_P3p_SSSS,Vm_CEW1_P4p_SSSS,Vm_CEW1_P5p_wt,Vm_CEW1_P6p_wt,Vm_CEW1_P7p_wt,Vm_CEW1_P8p_SSSS)
  Vm_CEW1_SSSS_Summary$Ancestral <- rep("CEW1",144)
  Vm_CEW1_SSSS_Summary$Species <- rep("O.tipulae",144)
  Vm_CEW1_SSSS_Summary$Genus <- rep("Oscheius",144)
  View(Vm_CEW1_SSSS_Summary)
  
  
  ##Vm_CEW1_P3p_divided_P4p_SSSS----
  {
    #Vm_CEW1_P3p_divided_P4p_SSSS_liab
    {
      
      va_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod <- va_liab_CEW1_Vm_P3p_SSSS_mod / va_liab_CEW1_Vm_P4p_SSSS_mod
      h2_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod <- h2_liab_CEW1_Vm_P3p_SSSS_mod / h2_liab_CEW1_Vm_P4p_SSSS_mod
      Evol_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod <- Evol_liab_CEW1_Vm_P3p_SSSS_mod / Evol_liab_CEW1_Vm_P4p_SSSS_mod
      
      mean_va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS <- rbind(mean(log10(va_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod)),mean(log10(h2_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod)), mean(log10(Evol_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod)))
      colnames(mean_va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS) <- c("mean")
      median_va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS <- rbind(median(log10(va_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod)),median(log10(h2_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod)), median(log10(Evol_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod)))
      colnames(median_va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS) <- c("median")
      posterior.mode_va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS <- rbind(posterior.mode(as.mcmc(log10(va_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod))),posterior.mode(as.mcmc(log10(h2_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod))),posterior.mode(as.mcmc(log10(Evol_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod))))
      colnames(posterior.mode_va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS) <- c("posterior.mode")
      HPDinterval_0.95_va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod))),HPDinterval(as.mcmc(log10(h2_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod))),HPDinterval(as.mcmc(log10(Evol_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod))))
      colnames(HPDinterval_0.95_va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS) <- c("CI_lower_0.95","CI_upper_0.95")
      HPDinterval_0.83_va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83),HPDinterval(as.mcmc(log10(h2_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83),HPDinterval(as.mcmc(log10(Evol_liab_CEW1_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83))
      colnames(HPDinterval_0.83_va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS) <- c("CI_lower_0.83","CI_upper_0.83")
      va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS <- cbind.data.frame(mean_va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS,median_va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS,posterior.mode_va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS,HPDinterval_0.95_va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS,HPDinterval_0.83_va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS)
      rownames(va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS) <- c("va_liab_CEW1_Vm_P3p_divided_P4p_SSSS_log10","h2_liab_CEW1_Vm_P3p_divided_P4p_SSSS_log10","Evol_liab_CEW1_Vm_P3p_divided_P4p_SSSS_log10")
      va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS$Measure <- c("Va","H2", "Evol")
      va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS$Ancestral <- rep("CEW1",3)
      va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS$Species <- rep("O.tipulae",3)
      va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS$Genus <- rep("Oscheius",3)
      va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS$Scale <- rep("liab",3)
      va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS
      
      pdf("Vm_va_h2_Evol_liab_P3p_divided_P4p_SSSS_log10_CEW1.pdf")
      ggplot(va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS, aes(x=Measure, y= median)) +
        geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
        geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
        geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
        theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
        labs(y = "Va log10 ( P3.p / P4.p)", title = "CEW1_log10(P3p/P4p)_liab")
      dev.off() 
      
    }
    
    #Vm_CEW1_P3p_divided_P4p_SSSS_data
    
    {
      va_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod <- va_data_CEW1_Vm_P3p_SSSS_mod / va_data_CEW1_Vm_P4p_SSSS_mod
      h2_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod <- h2_data_CEW1_Vm_P3p_SSSS_mod / h2_data_CEW1_Vm_P4p_SSSS_mod
      Evol_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod <- Evol_data_CEW1_Vm_P3p_SSSS_mod / Evol_data_CEW1_Vm_P4p_SSSS_mod
      
      mean_va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS <- rbind(mean(log10(va_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod)),mean(log10(h2_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod)), mean(log10(Evol_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod)))
      colnames(mean_va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS) <- c("mean")
      median_va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS <- rbind(median(log10(va_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod)),median(log10(h2_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod)), median(log10(Evol_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod)))
      colnames(median_va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS) <- c("median")
      posterior.mode_va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS <- rbind(posterior.mode(as.mcmc(log10(va_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod))),posterior.mode(as.mcmc(log10(h2_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod))),posterior.mode(as.mcmc(log10(Evol_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod))))
      colnames(posterior.mode_va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS) <- c("posterior.mode")
      HPDinterval_0.95_va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod))),HPDinterval(as.mcmc(log10(h2_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod))),HPDinterval(as.mcmc(log10(Evol_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod))))
      colnames(HPDinterval_0.95_va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS) <- c("CI_lower_0.95","CI_upper_0.95")
      HPDinterval_0.83_va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83),HPDinterval(as.mcmc(log10(h2_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83),HPDinterval(as.mcmc(log10(Evol_data_CEW1_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83))
      colnames(HPDinterval_0.83_va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS) <- c("CI_lower_0.83","CI_upper_0.83")
      va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS <- cbind.data.frame(mean_va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS,median_va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS,posterior.mode_va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS,HPDinterval_0.95_va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS,HPDinterval_0.83_va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS)
      rownames(va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS) <- c("va_data_CEW1_Vm_P3p_divided_P4p_SSSS_log10","h2_data_CEW1_Vm_P3p_divided_P4p_SSSS_log10","Evol_data_CEW1_Vm_P3p_divided_P4p_SSSS_log10")
      va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS$Measure <- c("Va","H2", "Evol")
      va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS$Ancestral <- rep("CEW1",3)
      va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS$Species <- rep("O.tipulae",3)
      va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS$Genus <- rep("Oscheius",3)
      va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS$Scale <- rep("data",3)
      va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS
      
      pdf("Vm_va_h2_Evol_data_P3p_divided_P4p_SSSS_log10_CEW1.pdf")
      ggplot(va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS, aes(x=Measure, y= median)) +
        geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
        geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
        geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
        theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
        labs(y = "Va log10 ( P3.p / P4.p)", title = "CEW1_log10(P3p/P4p)_data")
      dev.off() 
      
    }
    
    va_h2_Evol_Vm_CEW1_P3p_divided_P4p_SSSS_Summary <- rbind.data.frame(va_h2_Evol_liab_Vm_CEW1_P3p_divided_P4p_SSSS,va_h2_Evol_data_Vm_CEW1_P3p_divided_P4p_SSSS)
    va_h2_Evol_Vm_CEW1_P3p_divided_P4p_SSSS_Summary
    
  }
  
  
}

#---- JU178 ----
{
  ##Summary JU178 P3p----
  {
    #Summary liability scale JU178 P3p
    {
      #Summary va_liab_JU178_P3p_SSSS: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_JU178_P3p_SSSS <- rbind(mean(va_liab_JU178_CONTROL_P3p_SSSS_mod/2),mean(va_liab_JU178_MA_P3p_SSSS_mod/2),mean(va_liab_JU178_Vm_P3p_SSSS_mod/2))
        colnames(mean_va_liab_JU178_P3p_SSSS) <- c("mean")
        median_va_liab_JU178_P3p_SSSS <- rbind(median(va_liab_JU178_CONTROL_P3p_SSSS_mod/2),median(va_liab_JU178_MA_P3p_SSSS_mod/2),median(va_liab_JU178_Vm_P3p_SSSS_mod/2))
        colnames(median_va_liab_JU178_P3p_SSSS) <- c("median")
        posterior.mode_va_liab_JU178_P3p_SSSS <- rbind(posterior.mode(va_liab_JU178_CONTROL_P3p_SSSS_mod/2),posterior.mode(va_liab_JU178_MA_P3p_SSSS_mod/2),posterior.mode(va_liab_JU178_Vm_P3p_SSSS_mod/2))
        colnames(posterior.mode_va_liab_JU178_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_JU178_P3p_SSSS <- rbind(HPDinterval(va_liab_JU178_CONTROL_P3p_SSSS_mod/2),HPDinterval(va_liab_JU178_MA_P3p_SSSS_mod/2),HPDinterval(va_liab_JU178_Vm_P3p_SSSS_mod/2))
        colnames(HPDinterval_0.95_va_liab_JU178_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_JU178_P3p_SSSS <- rbind(HPDinterval(va_liab_JU178_CONTROL_P3p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_JU178_MA_P3p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_JU178_Vm_P3p_SSSS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_JU178_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_JU178_P3p_SSSS <- rbind(effectiveSize(va_liab_JU178_CONTROL_P3p_SSSS_mod/2),effectiveSize(va_liab_JU178_MA_P3p_SSSS_mod/2),effectiveSize(va_liab_JU178_Vm_P3p_SSSS_mod/2))
        colnames(effectiveSize_va_liab_JU178_P3p_SSSS) <- c("effectiveSize")
        va_liab_JU178_P3p_SSSS <- cbind.data.frame(mean_va_liab_JU178_P3p_SSSS,median_va_liab_JU178_P3p_SSSS,posterior.mode_va_liab_JU178_P3p_SSSS,HPDinterval_0.95_va_liab_JU178_P3p_SSSS,HPDinterval_0.83_va_liab_JU178_P3p_SSSS,effectiveSize_va_liab_JU178_P3p_SSSS)
        rownames(va_liab_JU178_P3p_SSSS) <- c("va_liab_JU178_CONTROL_P3p_SSSS_mod","va_liab_JU178_MA_P3p_SSSS_mod","va_liab_JU178_Vm_P3p_SSSS_mod")
        va_liab_JU178_P3p_SSSS <- cbind(Models = rownames(va_liab_JU178_P3p_SSSS),va_liab_JU178_P3p_SSSS)
        rownames(va_liab_JU178_P3p_SSSS) <- NULL
        va_liab_JU178_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        va_liab_JU178_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        va_liab_JU178_P3p_SSSS$Measure <- c("Va","Va","Va")
        va_liab_JU178_P3p_SSSS$Scale <- c("liab","liab","liab")
        va_liab_JU178_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_liab_JU178_P3p_SSSS
      }
      
      #Summary h2_liab_JU178_P3p_SSSS
      {
        mean_h2_liab_JU178_P3p_SSSS <- rbind(mean(h2_liab_JU178_CONTROL_P3p_SSSS_mod),mean(h2_liab_JU178_MA_P3p_SSSS_mod),mean(h2_liab_JU178_Vm_P3p_SSSS_mod))
        colnames(mean_h2_liab_JU178_P3p_SSSS) <- c("mean")
        median_h2_liab_JU178_P3p_SSSS <- rbind(median(h2_liab_JU178_CONTROL_P3p_SSSS_mod),median(h2_liab_JU178_MA_P3p_SSSS_mod),median(h2_liab_JU178_Vm_P3p_SSSS_mod))
        colnames(median_h2_liab_JU178_P3p_SSSS) <- c("median")
        posterior.mode_h2_liab_JU178_P3p_SSSS <- rbind(posterior.mode(h2_liab_JU178_CONTROL_P3p_SSSS_mod),posterior.mode(h2_liab_JU178_MA_P3p_SSSS_mod),posterior.mode(h2_liab_JU178_Vm_P3p_SSSS_mod))
        colnames(posterior.mode_h2_liab_JU178_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_JU178_P3p_SSSS <- rbind(HPDinterval(h2_liab_JU178_CONTROL_P3p_SSSS_mod),HPDinterval(h2_liab_JU178_MA_P3p_SSSS_mod),HPDinterval(h2_liab_JU178_Vm_P3p_SSSS_mod))
        colnames(HPDinterval_0.95_h2_liab_JU178_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_JU178_P3p_SSSS <- rbind(HPDinterval(h2_liab_JU178_CONTROL_P3p_SSSS_mod,prob=.83),HPDinterval(h2_liab_JU178_MA_P3p_SSSS_mod,prob=.83),HPDinterval(h2_liab_JU178_Vm_P3p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_JU178_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_JU178_P3p_SSSS <- rbind(effectiveSize(h2_liab_JU178_CONTROL_P3p_SSSS_mod),effectiveSize(h2_liab_JU178_MA_P3p_SSSS_mod),effectiveSize(h2_liab_JU178_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_h2_liab_JU178_P3p_SSSS) <- c("effectiveSize")
        h2_liab_JU178_P3p_SSSS <- cbind.data.frame(mean_h2_liab_JU178_P3p_SSSS,median_h2_liab_JU178_P3p_SSSS,posterior.mode_h2_liab_JU178_P3p_SSSS,HPDinterval_0.95_h2_liab_JU178_P3p_SSSS,HPDinterval_0.83_h2_liab_JU178_P3p_SSSS,effectiveSize_h2_liab_JU178_P3p_SSSS)
        rownames(h2_liab_JU178_P3p_SSSS) <- c("h2_liab_JU178_CONTROL_P3p_SSSS_mod","h2_liab_JU178_MA_P3p_SSSS_mod","h2_liab_JU178_Vm_P3p_SSSS_mod")
        h2_liab_JU178_P3p_SSSS <- cbind(Models = rownames(h2_liab_JU178_P3p_SSSS),h2_liab_JU178_P3p_SSSS)
        rownames(h2_liab_JU178_P3p_SSSS) <- NULL
        h2_liab_JU178_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        h2_liab_JU178_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_liab_JU178_P3p_SSSS$Measure <- c("H2","H2","H2")
        h2_liab_JU178_P3p_SSSS$Scale <- c("liab","liab","liab")
        h2_liab_JU178_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_liab_JU178_P3p_SSSS
      }
      
      #Summary Evol_liab_JU178_P3p_SSSS
      {
        mean_Evol_liab_JU178_P3p_SSSS <- rbind(mean(Evol_liab_JU178_CONTROL_P3p_SSSS_mod),mean(Evol_liab_JU178_MA_P3p_SSSS_mod),mean(Evol_liab_JU178_Vm_P3p_SSSS_mod))
        colnames(mean_Evol_liab_JU178_P3p_SSSS) <- c("mean")
        median_Evol_liab_JU178_P3p_SSSS <- rbind(median(Evol_liab_JU178_CONTROL_P3p_SSSS_mod),median(Evol_liab_JU178_MA_P3p_SSSS_mod),median(Evol_liab_JU178_Vm_P3p_SSSS_mod))
        colnames(median_Evol_liab_JU178_P3p_SSSS) <- c("median")
        posterior.mode_Evol_liab_JU178_P3p_SSSS <- rbind(posterior.mode(Evol_liab_JU178_CONTROL_P3p_SSSS_mod),posterior.mode(Evol_liab_JU178_MA_P3p_SSSS_mod),posterior.mode(Evol_liab_JU178_Vm_P3p_SSSS_mod))
        colnames(posterior.mode_Evol_liab_JU178_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_JU178_P3p_SSSS <- rbind(HPDinterval(Evol_liab_JU178_CONTROL_P3p_SSSS_mod),HPDinterval(Evol_liab_JU178_MA_P3p_SSSS_mod),HPDinterval(Evol_liab_JU178_Vm_P3p_SSSS_mod))
        colnames(HPDinterval_0.95_Evol_liab_JU178_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_JU178_P3p_SSSS <- rbind(HPDinterval(Evol_liab_JU178_CONTROL_P3p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_JU178_MA_P3p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_JU178_Vm_P3p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_JU178_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_JU178_P3p_SSSS <- rbind(effectiveSize(Evol_liab_JU178_CONTROL_P3p_SSSS_mod),effectiveSize(Evol_liab_JU178_MA_P3p_SSSS_mod),effectiveSize(Evol_liab_JU178_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_Evol_liab_JU178_P3p_SSSS) <- c("effectiveSize")
        Evol_liab_JU178_P3p_SSSS <- cbind.data.frame(mean_Evol_liab_JU178_P3p_SSSS,median_Evol_liab_JU178_P3p_SSSS,posterior.mode_Evol_liab_JU178_P3p_SSSS,HPDinterval_0.95_Evol_liab_JU178_P3p_SSSS,HPDinterval_0.83_Evol_liab_JU178_P3p_SSSS,effectiveSize_Evol_liab_JU178_P3p_SSSS)
        rownames(Evol_liab_JU178_P3p_SSSS) <- c("Evol_liab_JU178_CONTROL_P3p_SSSS_mod","Evol_liab_JU178_MA_P3p_SSSS_mod","Evol_liab_JU178_Vm_P3p_SSSS_mod")
        Evol_liab_JU178_P3p_SSSS <- cbind(Models = rownames(Evol_liab_JU178_P3p_SSSS),Evol_liab_JU178_P3p_SSSS)
        rownames(Evol_liab_JU178_P3p_SSSS) <- NULL
        Evol_liab_JU178_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        Evol_liab_JU178_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_liab_JU178_P3p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_JU178_P3p_SSSS$Scale <- c("liab","liab","liab")
        Evol_liab_JU178_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_JU178_P3p_SSSS
      }
      
      #Summary trait_mean_liab_JU178_P3p_SSSS
      {
        mean_trait_mean_liab_JU178_P3p_SSSS <- rbind(mean(trait_mean_liab_JU178_CONTROL_P3p_SSSS_mod),mean(trait_mean_liab_JU178_MA_P3p_SSSS_mod),mean(trait_mean_liab_JU178_Vm_P3p_SSSS_mod))
        colnames(mean_trait_mean_liab_JU178_P3p_SSSS) <- c("mean")
        median_trait_mean_liab_JU178_P3p_SSSS <- rbind(median(trait_mean_liab_JU178_CONTROL_P3p_SSSS_mod),median(trait_mean_liab_JU178_MA_P3p_SSSS_mod),median(trait_mean_liab_JU178_Vm_P3p_SSSS_mod))
        colnames(median_trait_mean_liab_JU178_P3p_SSSS) <- c("median")
        posterior.mode_trait_mean_liab_JU178_P3p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_JU178_CONTROL_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU178_MA_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU178_Vm_P3p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_liab_JU178_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_JU178_P3p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU178_CONTROL_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU178_MA_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU178_Vm_P3p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_JU178_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_JU178_P3p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU178_CONTROL_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU178_MA_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU178_Vm_P3p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_JU178_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_JU178_P3p_SSSS <- rbind(effectiveSize(trait_mean_liab_JU178_CONTROL_P3p_SSSS_mod),effectiveSize(trait_mean_liab_JU178_MA_P3p_SSSS_mod),effectiveSize(trait_mean_liab_JU178_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_trait_mean_liab_JU178_P3p_SSSS) <- c("effectiveSize")
        trait_mean_liab_JU178_P3p_SSSS <- cbind.data.frame(mean_trait_mean_liab_JU178_P3p_SSSS,median_trait_mean_liab_JU178_P3p_SSSS,posterior.mode_trait_mean_liab_JU178_P3p_SSSS,HPDinterval_0.95_trait_mean_liab_JU178_P3p_SSSS,HPDinterval_0.83_trait_mean_liab_JU178_P3p_SSSS,effectiveSize_trait_mean_liab_JU178_P3p_SSSS)
        rownames(trait_mean_liab_JU178_P3p_SSSS) <- c("trait_mean_liab_JU178_CONTROL_P3p_SSSS_mod","trait_mean_liab_JU178_MA_P3p_SSSS_mod","trait_mean_liab_JU178_Vm_P3p_SSSS_mod")
        trait_mean_liab_JU178_P3p_SSSS <- cbind(Models = rownames(trait_mean_liab_JU178_P3p_SSSS),trait_mean_liab_JU178_P3p_SSSS)
        rownames(trait_mean_liab_JU178_P3p_SSSS) <- NULL
        trait_mean_liab_JU178_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        trait_mean_liab_JU178_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_JU178_P3p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_JU178_P3p_SSSS$Scale <- c("liab","liab","liab")
        trait_mean_liab_JU178_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_JU178_P3p_SSSS
      }
      
      liab_JU178_P3p_SSSS <- rbind.data.frame(va_liab_JU178_P3p_SSSS, h2_liab_JU178_P3p_SSSS,Evol_liab_JU178_P3p_SSSS,trait_mean_liab_JU178_P3p_SSSS)
      liab_JU178_P3p_SSSS
    }
    #Summary data scale JU178 P3p
    {
      #Summary va_data_JU178_P3p_SSSS:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_JU178_P3p_SSSS <- rbind(mean(va_data_JU178_CONTROL_P3p_SSSS_mod/2),mean(va_data_JU178_MA_P3p_SSSS_mod/2),mean(va_data_JU178_Vm_P3p_SSSS_mod/2))
        colnames(mean_va_data_JU178_P3p_SSSS) <- c("mean")
        median_va_data_JU178_P3p_SSSS <- rbind(median(va_data_JU178_CONTROL_P3p_SSSS_mod/2),median(va_data_JU178_MA_P3p_SSSS_mod/2),median(va_data_JU178_Vm_P3p_SSSS_mod/2))
        colnames(median_va_data_JU178_P3p_SSSS) <- c("median")
        posterior.mode_va_data_JU178_P3p_SSSS <- rbind(posterior.mode(as.mcmc(va_data_JU178_CONTROL_P3p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_JU178_MA_P3p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_JU178_Vm_P3p_SSSS_mod/2)))
        colnames(posterior.mode_va_data_JU178_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_JU178_P3p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_JU178_CONTROL_P3p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_JU178_MA_P3p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_JU178_Vm_P3p_SSSS_mod/2)))
        colnames(HPDinterval_0.95_va_data_JU178_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_JU178_P3p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_JU178_CONTROL_P3p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU178_MA_P3p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU178_Vm_P3p_SSSS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_JU178_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_JU178_P3p_SSSS <- rbind(effectiveSize(va_data_JU178_CONTROL_P3p_SSSS_mod/2),effectiveSize(va_data_JU178_MA_P3p_SSSS_mod/2),effectiveSize(va_data_JU178_Vm_P3p_SSSS_mod/2))
        colnames(effectiveSize_va_data_JU178_P3p_SSSS) <- c("effectiveSize")
        va_data_JU178_P3p_SSSS <- cbind.data.frame(mean_va_data_JU178_P3p_SSSS,median_va_data_JU178_P3p_SSSS,posterior.mode_va_data_JU178_P3p_SSSS,HPDinterval_0.95_va_data_JU178_P3p_SSSS,HPDinterval_0.83_va_data_JU178_P3p_SSSS,effectiveSize_va_data_JU178_P3p_SSSS)
        rownames(va_data_JU178_P3p_SSSS) <- c("va_data_JU178_CONTROL_P3p_SSSS_mod","va_data_JU178_MA_P3p_SSSS_mod","va_data_JU178_Vm_P3p_SSSS_mod")
        va_data_JU178_P3p_SSSS <- cbind(Models = rownames(va_data_JU178_P3p_SSSS),va_data_JU178_P3p_SSSS)
        rownames(va_data_JU178_P3p_SSSS) <- NULL
        va_data_JU178_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        va_data_JU178_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        va_data_JU178_P3p_SSSS$Measure <- c("Va","Va","Va")
        va_data_JU178_P3p_SSSS$Scale <- c("data","data","data")
        va_data_JU178_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_data_JU178_P3p_SSSS
      }
      
      #Summary h2_data_JU178_P3p_SSSS
      {
        mean_h2_data_JU178_P3p_SSSS <- rbind(mean(h2_data_JU178_CONTROL_P3p_SSSS_mod),mean(h2_data_JU178_MA_P3p_SSSS_mod),mean(h2_data_JU178_Vm_P3p_SSSS_mod))
        colnames(mean_h2_data_JU178_P3p_SSSS) <- c("mean")
        median_h2_data_JU178_P3p_SSSS <- rbind(median(h2_data_JU178_CONTROL_P3p_SSSS_mod),median(h2_data_JU178_MA_P3p_SSSS_mod),median(h2_data_JU178_Vm_P3p_SSSS_mod))
        colnames(median_h2_data_JU178_P3p_SSSS) <- c("median")
        posterior.mode_h2_data_JU178_P3p_SSSS <- rbind(posterior.mode(as.mcmc(h2_data_JU178_CONTROL_P3p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_JU178_MA_P3p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_JU178_Vm_P3p_SSSS_mod)))
        colnames(posterior.mode_h2_data_JU178_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_JU178_P3p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_JU178_CONTROL_P3p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_JU178_MA_P3p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_JU178_Vm_P3p_SSSS_mod)))
        colnames(HPDinterval_0.95_h2_data_JU178_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_JU178_P3p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_JU178_CONTROL_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU178_MA_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU178_Vm_P3p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_JU178_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_JU178_P3p_SSSS <- rbind(effectiveSize(h2_data_JU178_CONTROL_P3p_SSSS_mod),effectiveSize(h2_data_JU178_MA_P3p_SSSS_mod),effectiveSize(h2_data_JU178_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_h2_data_JU178_P3p_SSSS) <- c("effectiveSize")
        h2_data_JU178_P3p_SSSS <- cbind.data.frame(mean_h2_data_JU178_P3p_SSSS,median_h2_data_JU178_P3p_SSSS,posterior.mode_h2_data_JU178_P3p_SSSS,HPDinterval_0.95_h2_data_JU178_P3p_SSSS,HPDinterval_0.83_h2_data_JU178_P3p_SSSS,effectiveSize_h2_data_JU178_P3p_SSSS)
        rownames(h2_data_JU178_P3p_SSSS) <- c("h2_data_JU178_CONTROL_P3p_SSSS_mod","h2_data_JU178_MA_P3p_SSSS_mod","h2_data_JU178_Vm_P3p_SSSS_mod")
        h2_data_JU178_P3p_SSSS <- cbind(Models = rownames(h2_data_JU178_P3p_SSSS),h2_data_JU178_P3p_SSSS)
        rownames(h2_data_JU178_P3p_SSSS) <- NULL
        h2_data_JU178_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        h2_data_JU178_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_data_JU178_P3p_SSSS$Measure <- c("H2","H2","H2")
        h2_data_JU178_P3p_SSSS$Scale <- c("data","data","data")
        h2_data_JU178_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_data_JU178_P3p_SSSS
      }
      
      #Summary Evol_data_JU178_P3p_SSSS
      {
        mean_Evol_data_JU178_P3p_SSSS <- rbind(mean(Evol_data_JU178_CONTROL_P3p_SSSS_mod),mean(Evol_data_JU178_MA_P3p_SSSS_mod),mean(Evol_data_JU178_Vm_P3p_SSSS_mod))
        colnames(mean_Evol_data_JU178_P3p_SSSS) <- c("mean")
        median_Evol_data_JU178_P3p_SSSS <- rbind(median(Evol_data_JU178_CONTROL_P3p_SSSS_mod),median(Evol_data_JU178_MA_P3p_SSSS_mod),median(Evol_data_JU178_Vm_P3p_SSSS_mod))
        colnames(median_Evol_data_JU178_P3p_SSSS) <- c("median")
        posterior.mode_Evol_data_JU178_P3p_SSSS <- rbind(posterior.mode(as.mcmc(Evol_data_JU178_CONTROL_P3p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_JU178_MA_P3p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_JU178_Vm_P3p_SSSS_mod)))
        colnames(posterior.mode_Evol_data_JU178_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_JU178_P3p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_JU178_CONTROL_P3p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_JU178_MA_P3p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_JU178_Vm_P3p_SSSS_mod)))
        colnames(HPDinterval_0.95_Evol_data_JU178_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_JU178_P3p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_JU178_CONTROL_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU178_MA_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU178_Vm_P3p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_JU178_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_JU178_P3p_SSSS <- rbind(effectiveSize(Evol_data_JU178_CONTROL_P3p_SSSS_mod),effectiveSize(Evol_data_JU178_MA_P3p_SSSS_mod),effectiveSize(Evol_data_JU178_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_Evol_data_JU178_P3p_SSSS) <- c("effectiveSize")
        Evol_data_JU178_P3p_SSSS <- cbind.data.frame(mean_Evol_data_JU178_P3p_SSSS,median_Evol_data_JU178_P3p_SSSS,posterior.mode_Evol_data_JU178_P3p_SSSS,HPDinterval_0.95_Evol_data_JU178_P3p_SSSS,HPDinterval_0.83_Evol_data_JU178_P3p_SSSS,effectiveSize_Evol_data_JU178_P3p_SSSS)
        rownames(Evol_data_JU178_P3p_SSSS) <- c("Evol_data_JU178_CONTROL_P3p_SSSS_mod","Evol_data_JU178_MA_P3p_SSSS_mod","Evol_data_JU178_Vm_P3p_SSSS_mod")
        Evol_data_JU178_P3p_SSSS <- cbind(Models = rownames(Evol_data_JU178_P3p_SSSS),Evol_data_JU178_P3p_SSSS)
        rownames(Evol_data_JU178_P3p_SSSS) <- NULL
        Evol_data_JU178_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        Evol_data_JU178_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_data_JU178_P3p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_data_JU178_P3p_SSSS$Scale <- c("data","data","data")
        Evol_data_JU178_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_data_JU178_P3p_SSSS
      }
      
      #Summary trait_mean_data_JU178_P3p_SSSS
      {
        mean_trait_mean_data_JU178_P3p_SSSS <- rbind(mean(trait_mean_data_JU178_CONTROL_P3p_SSSS_mod),mean(trait_mean_data_JU178_MA_P3p_SSSS_mod),mean(trait_mean_data_JU178_Vm_P3p_SSSS_mod))
        colnames(mean_trait_mean_data_JU178_P3p_SSSS) <- c("mean")
        median_trait_mean_data_JU178_P3p_SSSS <- rbind(median(trait_mean_data_JU178_CONTROL_P3p_SSSS_mod),median(trait_mean_data_JU178_MA_P3p_SSSS_mod),median(trait_mean_data_JU178_Vm_P3p_SSSS_mod))
        colnames(median_trait_mean_data_JU178_P3p_SSSS) <- c("median")
        posterior.mode_trait_mean_data_JU178_P3p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_data_JU178_CONTROL_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_JU178_MA_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_JU178_Vm_P3p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_data_JU178_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_JU178_P3p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU178_CONTROL_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_JU178_MA_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_JU178_Vm_P3p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_JU178_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_JU178_P3p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU178_CONTROL_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU178_MA_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU178_Vm_P3p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_JU178_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_JU178_P3p_SSSS <- rbind(effectiveSize(trait_mean_data_JU178_CONTROL_P3p_SSSS_mod),effectiveSize(trait_mean_data_JU178_MA_P3p_SSSS_mod),effectiveSize(trait_mean_data_JU178_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_trait_mean_data_JU178_P3p_SSSS) <- c("effectiveSize")
        trait_mean_data_JU178_P3p_SSSS <- cbind.data.frame(mean_trait_mean_data_JU178_P3p_SSSS,median_trait_mean_data_JU178_P3p_SSSS,posterior.mode_trait_mean_data_JU178_P3p_SSSS,HPDinterval_0.95_trait_mean_data_JU178_P3p_SSSS,HPDinterval_0.83_trait_mean_data_JU178_P3p_SSSS,effectiveSize_trait_mean_data_JU178_P3p_SSSS)
        rownames(trait_mean_data_JU178_P3p_SSSS) <- c("trait_mean_data_JU178_CONTROL_P3p_SSSS_mod","trait_mean_data_JU178_MA_P3p_SSSS_mod","trait_mean_data_JU178_Vm_P3p_SSSS_mod")
        trait_mean_data_JU178_P3p_SSSS <- cbind(Models = rownames(trait_mean_data_JU178_P3p_SSSS),trait_mean_data_JU178_P3p_SSSS)
        rownames(trait_mean_data_JU178_P3p_SSSS) <- NULL
        trait_mean_data_JU178_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        trait_mean_data_JU178_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_data_JU178_P3p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_JU178_P3p_SSSS$Scale <- c("data","data","data")
        trait_mean_data_JU178_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_JU178_P3p_SSSS
      }
      
      data_JU178_P3p_SSSS <- rbind.data.frame(va_data_JU178_P3p_SSSS, h2_data_JU178_P3p_SSSS,Evol_data_JU178_P3p_SSSS,trait_mean_data_JU178_P3p_SSSS)
      data_JU178_P3p_SSSS
      
    }
    Vm_JU178_P3p_SSSS <- rbind.data.frame(liab_JU178_P3p_SSSS, data_JU178_P3p_SSSS)
    Vm_JU178_P3p_SSSS$Pnp_fate <- rep("SSSS", 24)
    Vm_JU178_P3p_SSSS
    #remove JU178 P3p_SSSSS models
    {
      remove(JU178_CONTROL_P3p_SSSS_mod)
      remove(JU178_MA_P3p_SSSS_mod)
      remove(JU178_Vm_P3p_SSSS_mod)
    }
  }
  
  ##Summary JU178 P4p----
  {
    #Summary liability scale JU178 P4p
    {
      #Summary va_liab_JU178_P4p_SSSS: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_JU178_P4p_SSSS <- rbind(mean(va_liab_JU178_CONTROL_P4p_SSSS_mod/2),mean(va_liab_JU178_MA_P4p_SSSS_mod/2),mean(va_liab_JU178_Vm_P4p_SSSS_mod/2))
        colnames(mean_va_liab_JU178_P4p_SSSS) <- c("mean")
        median_va_liab_JU178_P4p_SSSS <- rbind(median(va_liab_JU178_CONTROL_P4p_SSSS_mod/2),median(va_liab_JU178_MA_P4p_SSSS_mod/2),median(va_liab_JU178_Vm_P4p_SSSS_mod/2))
        colnames(median_va_liab_JU178_P4p_SSSS) <- c("median")
        posterior.mode_va_liab_JU178_P4p_SSSS <- rbind(posterior.mode(va_liab_JU178_CONTROL_P4p_SSSS_mod/2),posterior.mode(va_liab_JU178_MA_P4p_SSSS_mod/2),posterior.mode(va_liab_JU178_Vm_P4p_SSSS_mod/2))
        colnames(posterior.mode_va_liab_JU178_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_JU178_P4p_SSSS <- rbind(HPDinterval(va_liab_JU178_CONTROL_P4p_SSSS_mod/2),HPDinterval(va_liab_JU178_MA_P4p_SSSS_mod/2),HPDinterval(va_liab_JU178_Vm_P4p_SSSS_mod/2))
        colnames(HPDinterval_0.95_va_liab_JU178_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_JU178_P4p_SSSS <- rbind(HPDinterval(va_liab_JU178_CONTROL_P4p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_JU178_MA_P4p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_JU178_Vm_P4p_SSSS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_JU178_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_JU178_P4p_SSSS <- rbind(effectiveSize(va_liab_JU178_CONTROL_P4p_SSSS_mod/2),effectiveSize(va_liab_JU178_MA_P4p_SSSS_mod/2),effectiveSize(va_liab_JU178_Vm_P4p_SSSS_mod/2))
        colnames(effectiveSize_va_liab_JU178_P4p_SSSS) <- c("effectiveSize")
        va_liab_JU178_P4p_SSSS <- cbind.data.frame(mean_va_liab_JU178_P4p_SSSS,median_va_liab_JU178_P4p_SSSS,posterior.mode_va_liab_JU178_P4p_SSSS,HPDinterval_0.95_va_liab_JU178_P4p_SSSS,HPDinterval_0.83_va_liab_JU178_P4p_SSSS,effectiveSize_va_liab_JU178_P4p_SSSS)
        rownames(va_liab_JU178_P4p_SSSS) <- c("va_liab_JU178_CONTROL_P4p_SSSS_mod","va_liab_JU178_MA_P4p_SSSS_mod","va_liab_JU178_Vm_P4p_SSSS_mod")
        va_liab_JU178_P4p_SSSS <- cbind(Models = rownames(va_liab_JU178_P4p_SSSS),va_liab_JU178_P4p_SSSS)
        rownames(va_liab_JU178_P4p_SSSS) <- NULL
        va_liab_JU178_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        va_liab_JU178_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        va_liab_JU178_P4p_SSSS$Measure <- c("Va","Va","Va")
        va_liab_JU178_P4p_SSSS$Scale <- c("liab","liab","liab")
        va_liab_JU178_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_liab_JU178_P4p_SSSS
      }
      
      #Summary h2_liab_JU178_P4p_SSSS
      {
        mean_h2_liab_JU178_P4p_SSSS <- rbind(mean(h2_liab_JU178_CONTROL_P4p_SSSS_mod),mean(h2_liab_JU178_MA_P4p_SSSS_mod),mean(h2_liab_JU178_Vm_P4p_SSSS_mod))
        colnames(mean_h2_liab_JU178_P4p_SSSS) <- c("mean")
        median_h2_liab_JU178_P4p_SSSS <- rbind(median(h2_liab_JU178_CONTROL_P4p_SSSS_mod),median(h2_liab_JU178_MA_P4p_SSSS_mod),median(h2_liab_JU178_Vm_P4p_SSSS_mod))
        colnames(median_h2_liab_JU178_P4p_SSSS) <- c("median")
        posterior.mode_h2_liab_JU178_P4p_SSSS <- rbind(posterior.mode(h2_liab_JU178_CONTROL_P4p_SSSS_mod),posterior.mode(h2_liab_JU178_MA_P4p_SSSS_mod),posterior.mode(h2_liab_JU178_Vm_P4p_SSSS_mod))
        colnames(posterior.mode_h2_liab_JU178_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_JU178_P4p_SSSS <- rbind(HPDinterval(h2_liab_JU178_CONTROL_P4p_SSSS_mod),HPDinterval(h2_liab_JU178_MA_P4p_SSSS_mod),HPDinterval(h2_liab_JU178_Vm_P4p_SSSS_mod))
        colnames(HPDinterval_0.95_h2_liab_JU178_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_JU178_P4p_SSSS <- rbind(HPDinterval(h2_liab_JU178_CONTROL_P4p_SSSS_mod,prob=.83),HPDinterval(h2_liab_JU178_MA_P4p_SSSS_mod,prob=.83),HPDinterval(h2_liab_JU178_Vm_P4p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_JU178_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_JU178_P4p_SSSS <- rbind(effectiveSize(h2_liab_JU178_CONTROL_P4p_SSSS_mod),effectiveSize(h2_liab_JU178_MA_P4p_SSSS_mod),effectiveSize(h2_liab_JU178_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_h2_liab_JU178_P4p_SSSS) <- c("effectiveSize")
        h2_liab_JU178_P4p_SSSS <- cbind.data.frame(mean_h2_liab_JU178_P4p_SSSS,median_h2_liab_JU178_P4p_SSSS,posterior.mode_h2_liab_JU178_P4p_SSSS,HPDinterval_0.95_h2_liab_JU178_P4p_SSSS,HPDinterval_0.83_h2_liab_JU178_P4p_SSSS,effectiveSize_h2_liab_JU178_P4p_SSSS)
        rownames(h2_liab_JU178_P4p_SSSS) <- c("h2_liab_JU178_CONTROL_P4p_SSSS_mod","h2_liab_JU178_MA_P4p_SSSS_mod","h2_liab_JU178_Vm_P4p_SSSS_mod")
        h2_liab_JU178_P4p_SSSS <- cbind(Models = rownames(h2_liab_JU178_P4p_SSSS),h2_liab_JU178_P4p_SSSS)
        rownames(h2_liab_JU178_P4p_SSSS) <- NULL
        h2_liab_JU178_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        h2_liab_JU178_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_liab_JU178_P4p_SSSS$Measure <- c("H2","H2","H2")
        h2_liab_JU178_P4p_SSSS$Scale <- c("liab","liab","liab")
        h2_liab_JU178_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_liab_JU178_P4p_SSSS
      }
      
      #Summary Evol_liab_JU178_P4p_SSSS
      {
        mean_Evol_liab_JU178_P4p_SSSS <- rbind(mean(Evol_liab_JU178_CONTROL_P4p_SSSS_mod),mean(Evol_liab_JU178_MA_P4p_SSSS_mod),mean(Evol_liab_JU178_Vm_P4p_SSSS_mod))
        colnames(mean_Evol_liab_JU178_P4p_SSSS) <- c("mean")
        median_Evol_liab_JU178_P4p_SSSS <- rbind(median(Evol_liab_JU178_CONTROL_P4p_SSSS_mod),median(Evol_liab_JU178_MA_P4p_SSSS_mod),median(Evol_liab_JU178_Vm_P4p_SSSS_mod))
        colnames(median_Evol_liab_JU178_P4p_SSSS) <- c("median")
        posterior.mode_Evol_liab_JU178_P4p_SSSS <- rbind(posterior.mode(Evol_liab_JU178_CONTROL_P4p_SSSS_mod),posterior.mode(Evol_liab_JU178_MA_P4p_SSSS_mod),posterior.mode(Evol_liab_JU178_Vm_P4p_SSSS_mod))
        colnames(posterior.mode_Evol_liab_JU178_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_JU178_P4p_SSSS <- rbind(HPDinterval(Evol_liab_JU178_CONTROL_P4p_SSSS_mod),HPDinterval(Evol_liab_JU178_MA_P4p_SSSS_mod),HPDinterval(Evol_liab_JU178_Vm_P4p_SSSS_mod))
        colnames(HPDinterval_0.95_Evol_liab_JU178_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_JU178_P4p_SSSS <- rbind(HPDinterval(Evol_liab_JU178_CONTROL_P4p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_JU178_MA_P4p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_JU178_Vm_P4p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_JU178_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_JU178_P4p_SSSS <- rbind(effectiveSize(Evol_liab_JU178_CONTROL_P4p_SSSS_mod),effectiveSize(Evol_liab_JU178_MA_P4p_SSSS_mod),effectiveSize(Evol_liab_JU178_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_Evol_liab_JU178_P4p_SSSS) <- c("effectiveSize")
        Evol_liab_JU178_P4p_SSSS <- cbind.data.frame(mean_Evol_liab_JU178_P4p_SSSS,median_Evol_liab_JU178_P4p_SSSS,posterior.mode_Evol_liab_JU178_P4p_SSSS,HPDinterval_0.95_Evol_liab_JU178_P4p_SSSS,HPDinterval_0.83_Evol_liab_JU178_P4p_SSSS,effectiveSize_Evol_liab_JU178_P4p_SSSS)
        rownames(Evol_liab_JU178_P4p_SSSS) <- c("Evol_liab_JU178_CONTROL_P4p_SSSS_mod","Evol_liab_JU178_MA_P4p_SSSS_mod","Evol_liab_JU178_Vm_P4p_SSSS_mod")
        Evol_liab_JU178_P4p_SSSS <- cbind(Models = rownames(Evol_liab_JU178_P4p_SSSS),Evol_liab_JU178_P4p_SSSS)
        rownames(Evol_liab_JU178_P4p_SSSS) <- NULL
        Evol_liab_JU178_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        Evol_liab_JU178_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_liab_JU178_P4p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_JU178_P4p_SSSS$Scale <- c("liab","liab","liab")
        Evol_liab_JU178_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_JU178_P4p_SSSS
      }
      
      #Summary trait_mean_liab_JU178_P4p_SSSS
      {
        mean_trait_mean_liab_JU178_P4p_SSSS <- rbind(mean(trait_mean_liab_JU178_CONTROL_P4p_SSSS_mod),mean(trait_mean_liab_JU178_MA_P4p_SSSS_mod),mean(trait_mean_liab_JU178_Vm_P4p_SSSS_mod))
        colnames(mean_trait_mean_liab_JU178_P4p_SSSS) <- c("mean")
        median_trait_mean_liab_JU178_P4p_SSSS <- rbind(median(trait_mean_liab_JU178_CONTROL_P4p_SSSS_mod),median(trait_mean_liab_JU178_MA_P4p_SSSS_mod),median(trait_mean_liab_JU178_Vm_P4p_SSSS_mod))
        colnames(median_trait_mean_liab_JU178_P4p_SSSS) <- c("median")
        posterior.mode_trait_mean_liab_JU178_P4p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_JU178_CONTROL_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU178_MA_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU178_Vm_P4p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_liab_JU178_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_JU178_P4p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU178_CONTROL_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU178_MA_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU178_Vm_P4p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_JU178_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_JU178_P4p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU178_CONTROL_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU178_MA_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU178_Vm_P4p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_JU178_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_JU178_P4p_SSSS <- rbind(effectiveSize(trait_mean_liab_JU178_CONTROL_P4p_SSSS_mod),effectiveSize(trait_mean_liab_JU178_MA_P4p_SSSS_mod),effectiveSize(trait_mean_liab_JU178_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_trait_mean_liab_JU178_P4p_SSSS) <- c("effectiveSize")
        trait_mean_liab_JU178_P4p_SSSS <- cbind.data.frame(mean_trait_mean_liab_JU178_P4p_SSSS,median_trait_mean_liab_JU178_P4p_SSSS,posterior.mode_trait_mean_liab_JU178_P4p_SSSS,HPDinterval_0.95_trait_mean_liab_JU178_P4p_SSSS,HPDinterval_0.83_trait_mean_liab_JU178_P4p_SSSS,effectiveSize_trait_mean_liab_JU178_P4p_SSSS)
        rownames(trait_mean_liab_JU178_P4p_SSSS) <- c("trait_mean_liab_JU178_CONTROL_P4p_SSSS_mod","trait_mean_liab_JU178_MA_P4p_SSSS_mod","trait_mean_liab_JU178_Vm_P4p_SSSS_mod")
        trait_mean_liab_JU178_P4p_SSSS <- cbind(Models = rownames(trait_mean_liab_JU178_P4p_SSSS),trait_mean_liab_JU178_P4p_SSSS)
        rownames(trait_mean_liab_JU178_P4p_SSSS) <- NULL
        trait_mean_liab_JU178_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        trait_mean_liab_JU178_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_JU178_P4p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_JU178_P4p_SSSS$Scale <- c("liab","liab","liab")
        trait_mean_liab_JU178_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_JU178_P4p_SSSS
      }
      
      liab_JU178_P4p_SSSS <- rbind.data.frame(va_liab_JU178_P4p_SSSS, h2_liab_JU178_P4p_SSSS,Evol_liab_JU178_P4p_SSSS,trait_mean_liab_JU178_P4p_SSSS)
      liab_JU178_P4p_SSSS
    }
    #Summary data scale JU178 P4p
    {
      #Summary va_data_JU178_P4p_SSSS:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_JU178_P4p_SSSS <- rbind(mean(va_data_JU178_CONTROL_P4p_SSSS_mod/2),mean(va_data_JU178_MA_P4p_SSSS_mod/2),mean(va_data_JU178_Vm_P4p_SSSS_mod/2))
        colnames(mean_va_data_JU178_P4p_SSSS) <- c("mean")
        median_va_data_JU178_P4p_SSSS <- rbind(median(va_data_JU178_CONTROL_P4p_SSSS_mod/2),median(va_data_JU178_MA_P4p_SSSS_mod/2),median(va_data_JU178_Vm_P4p_SSSS_mod/2))
        colnames(median_va_data_JU178_P4p_SSSS) <- c("median")
        posterior.mode_va_data_JU178_P4p_SSSS <- rbind(posterior.mode(as.mcmc(va_data_JU178_CONTROL_P4p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_JU178_MA_P4p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_JU178_Vm_P4p_SSSS_mod/2)))
        colnames(posterior.mode_va_data_JU178_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_JU178_P4p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_JU178_CONTROL_P4p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_JU178_MA_P4p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_JU178_Vm_P4p_SSSS_mod/2)))
        colnames(HPDinterval_0.95_va_data_JU178_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_JU178_P4p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_JU178_CONTROL_P4p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU178_MA_P4p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU178_Vm_P4p_SSSS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_JU178_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_JU178_P4p_SSSS <- rbind(effectiveSize(va_data_JU178_CONTROL_P4p_SSSS_mod/2),effectiveSize(va_data_JU178_MA_P4p_SSSS_mod/2),effectiveSize(va_data_JU178_Vm_P4p_SSSS_mod/2))
        colnames(effectiveSize_va_data_JU178_P4p_SSSS) <- c("effectiveSize")
        va_data_JU178_P4p_SSSS <- cbind.data.frame(mean_va_data_JU178_P4p_SSSS,median_va_data_JU178_P4p_SSSS,posterior.mode_va_data_JU178_P4p_SSSS,HPDinterval_0.95_va_data_JU178_P4p_SSSS,HPDinterval_0.83_va_data_JU178_P4p_SSSS,effectiveSize_va_data_JU178_P4p_SSSS)
        rownames(va_data_JU178_P4p_SSSS) <- c("va_data_JU178_CONTROL_P4p_SSSS_mod","va_data_JU178_MA_P4p_SSSS_mod","va_data_JU178_Vm_P4p_SSSS_mod")
        va_data_JU178_P4p_SSSS <- cbind(Models = rownames(va_data_JU178_P4p_SSSS),va_data_JU178_P4p_SSSS)
        rownames(va_data_JU178_P4p_SSSS) <- NULL
        va_data_JU178_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        va_data_JU178_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        va_data_JU178_P4p_SSSS$Measure <- c("Va","Va","Va")
        va_data_JU178_P4p_SSSS$Scale <- c("data","data","data")
        va_data_JU178_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_data_JU178_P4p_SSSS
      }
      
      #Summary h2_data_JU178_P4p_SSSS
      {
        mean_h2_data_JU178_P4p_SSSS <- rbind(mean(h2_data_JU178_CONTROL_P4p_SSSS_mod),mean(h2_data_JU178_MA_P4p_SSSS_mod),mean(h2_data_JU178_Vm_P4p_SSSS_mod))
        colnames(mean_h2_data_JU178_P4p_SSSS) <- c("mean")
        median_h2_data_JU178_P4p_SSSS <- rbind(median(h2_data_JU178_CONTROL_P4p_SSSS_mod),median(h2_data_JU178_MA_P4p_SSSS_mod),median(h2_data_JU178_Vm_P4p_SSSS_mod))
        colnames(median_h2_data_JU178_P4p_SSSS) <- c("median")
        posterior.mode_h2_data_JU178_P4p_SSSS <- rbind(posterior.mode(as.mcmc(h2_data_JU178_CONTROL_P4p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_JU178_MA_P4p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_JU178_Vm_P4p_SSSS_mod)))
        colnames(posterior.mode_h2_data_JU178_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_JU178_P4p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_JU178_CONTROL_P4p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_JU178_MA_P4p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_JU178_Vm_P4p_SSSS_mod)))
        colnames(HPDinterval_0.95_h2_data_JU178_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_JU178_P4p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_JU178_CONTROL_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU178_MA_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU178_Vm_P4p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_JU178_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_JU178_P4p_SSSS <- rbind(effectiveSize(h2_data_JU178_CONTROL_P4p_SSSS_mod),effectiveSize(h2_data_JU178_MA_P4p_SSSS_mod),effectiveSize(h2_data_JU178_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_h2_data_JU178_P4p_SSSS) <- c("effectiveSize")
        h2_data_JU178_P4p_SSSS <- cbind.data.frame(mean_h2_data_JU178_P4p_SSSS,median_h2_data_JU178_P4p_SSSS,posterior.mode_h2_data_JU178_P4p_SSSS,HPDinterval_0.95_h2_data_JU178_P4p_SSSS,HPDinterval_0.83_h2_data_JU178_P4p_SSSS,effectiveSize_h2_data_JU178_P4p_SSSS)
        rownames(h2_data_JU178_P4p_SSSS) <- c("h2_data_JU178_CONTROL_P4p_SSSS_mod","h2_data_JU178_MA_P4p_SSSS_mod","h2_data_JU178_Vm_P4p_SSSS_mod")
        h2_data_JU178_P4p_SSSS <- cbind(Models = rownames(h2_data_JU178_P4p_SSSS),h2_data_JU178_P4p_SSSS)
        rownames(h2_data_JU178_P4p_SSSS) <- NULL
        h2_data_JU178_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        h2_data_JU178_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_data_JU178_P4p_SSSS$Measure <- c("H2","H2","H2")
        h2_data_JU178_P4p_SSSS$Scale <- c("data","data","data")
        h2_data_JU178_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_data_JU178_P4p_SSSS
      }
      
      #Summary Evol_data_JU178_P4p_SSSS
      {
        mean_Evol_data_JU178_P4p_SSSS <- rbind(mean(Evol_data_JU178_CONTROL_P4p_SSSS_mod),mean(Evol_data_JU178_MA_P4p_SSSS_mod),mean(Evol_data_JU178_Vm_P4p_SSSS_mod))
        colnames(mean_Evol_data_JU178_P4p_SSSS) <- c("mean")
        median_Evol_data_JU178_P4p_SSSS <- rbind(median(Evol_data_JU178_CONTROL_P4p_SSSS_mod),median(Evol_data_JU178_MA_P4p_SSSS_mod),median(Evol_data_JU178_Vm_P4p_SSSS_mod))
        colnames(median_Evol_data_JU178_P4p_SSSS) <- c("median")
        posterior.mode_Evol_data_JU178_P4p_SSSS <- rbind(posterior.mode(as.mcmc(Evol_data_JU178_CONTROL_P4p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_JU178_MA_P4p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_JU178_Vm_P4p_SSSS_mod)))
        colnames(posterior.mode_Evol_data_JU178_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_JU178_P4p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_JU178_CONTROL_P4p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_JU178_MA_P4p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_JU178_Vm_P4p_SSSS_mod)))
        colnames(HPDinterval_0.95_Evol_data_JU178_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_JU178_P4p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_JU178_CONTROL_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU178_MA_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU178_Vm_P4p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_JU178_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_JU178_P4p_SSSS <- rbind(effectiveSize(Evol_data_JU178_CONTROL_P4p_SSSS_mod),effectiveSize(Evol_data_JU178_MA_P4p_SSSS_mod),effectiveSize(Evol_data_JU178_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_Evol_data_JU178_P4p_SSSS) <- c("effectiveSize")
        Evol_data_JU178_P4p_SSSS <- cbind.data.frame(mean_Evol_data_JU178_P4p_SSSS,median_Evol_data_JU178_P4p_SSSS,posterior.mode_Evol_data_JU178_P4p_SSSS,HPDinterval_0.95_Evol_data_JU178_P4p_SSSS,HPDinterval_0.83_Evol_data_JU178_P4p_SSSS,effectiveSize_Evol_data_JU178_P4p_SSSS)
        rownames(Evol_data_JU178_P4p_SSSS) <- c("Evol_data_JU178_CONTROL_P4p_SSSS_mod","Evol_data_JU178_MA_P4p_SSSS_mod","Evol_data_JU178_Vm_P4p_SSSS_mod")
        Evol_data_JU178_P4p_SSSS <- cbind(Models = rownames(Evol_data_JU178_P4p_SSSS),Evol_data_JU178_P4p_SSSS)
        rownames(Evol_data_JU178_P4p_SSSS) <- NULL
        Evol_data_JU178_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        Evol_data_JU178_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_data_JU178_P4p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_data_JU178_P4p_SSSS$Scale <- c("data","data","data")
        Evol_data_JU178_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_data_JU178_P4p_SSSS
      }
      
      #Summary trait_mean_data_JU178_P4p_SSSS
      {
        mean_trait_mean_data_JU178_P4p_SSSS <- rbind(mean(trait_mean_data_JU178_CONTROL_P4p_SSSS_mod),mean(trait_mean_data_JU178_MA_P4p_SSSS_mod),mean(trait_mean_data_JU178_Vm_P4p_SSSS_mod))
        colnames(mean_trait_mean_data_JU178_P4p_SSSS) <- c("mean")
        median_trait_mean_data_JU178_P4p_SSSS <- rbind(median(trait_mean_data_JU178_CONTROL_P4p_SSSS_mod),median(trait_mean_data_JU178_MA_P4p_SSSS_mod),median(trait_mean_data_JU178_Vm_P4p_SSSS_mod))
        colnames(median_trait_mean_data_JU178_P4p_SSSS) <- c("median")
        posterior.mode_trait_mean_data_JU178_P4p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_data_JU178_CONTROL_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_JU178_MA_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_JU178_Vm_P4p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_data_JU178_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_JU178_P4p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU178_CONTROL_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_JU178_MA_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_JU178_Vm_P4p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_JU178_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_JU178_P4p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU178_CONTROL_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU178_MA_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU178_Vm_P4p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_JU178_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_JU178_P4p_SSSS <- rbind(effectiveSize(trait_mean_data_JU178_CONTROL_P4p_SSSS_mod),effectiveSize(trait_mean_data_JU178_MA_P4p_SSSS_mod),effectiveSize(trait_mean_data_JU178_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_trait_mean_data_JU178_P4p_SSSS) <- c("effectiveSize")
        trait_mean_data_JU178_P4p_SSSS <- cbind.data.frame(mean_trait_mean_data_JU178_P4p_SSSS,median_trait_mean_data_JU178_P4p_SSSS,posterior.mode_trait_mean_data_JU178_P4p_SSSS,HPDinterval_0.95_trait_mean_data_JU178_P4p_SSSS,HPDinterval_0.83_trait_mean_data_JU178_P4p_SSSS,effectiveSize_trait_mean_data_JU178_P4p_SSSS)
        rownames(trait_mean_data_JU178_P4p_SSSS) <- c("trait_mean_data_JU178_CONTROL_P4p_SSSS_mod","trait_mean_data_JU178_MA_P4p_SSSS_mod","trait_mean_data_JU178_Vm_P4p_SSSS_mod")
        trait_mean_data_JU178_P4p_SSSS <- cbind(Models = rownames(trait_mean_data_JU178_P4p_SSSS),trait_mean_data_JU178_P4p_SSSS)
        rownames(trait_mean_data_JU178_P4p_SSSS) <- NULL
        trait_mean_data_JU178_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        trait_mean_data_JU178_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_data_JU178_P4p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_JU178_P4p_SSSS$Scale <- c("data","data","data")
        trait_mean_data_JU178_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_JU178_P4p_SSSS
      }
      
      data_JU178_P4p_SSSS <- rbind.data.frame(va_data_JU178_P4p_SSSS, h2_data_JU178_P4p_SSSS,Evol_data_JU178_P4p_SSSS,trait_mean_data_JU178_P4p_SSSS)
      data_JU178_P4p_SSSS
      
    }
    Vm_JU178_P4p_SSSS <- rbind.data.frame(liab_JU178_P4p_SSSS, data_JU178_P4p_SSSS)
    Vm_JU178_P4p_SSSS$Pnp_fate <- rep("SSSS", 24)
    Vm_JU178_P4p_SSSS
    #remove JU178 P4p_SSSSS models
    {
      remove(JU178_CONTROL_P4p_SSSS_mod)
      remove(JU178_MA_P4p_SSSS_mod)
      remove(JU178_Vm_P4p_SSSS_mod)
    }
  }
  
  ##Summary JU178 P8p----
  {
    #Summary liability scale JU178 P8p
    {
      #Summary va_liab_JU178_P8p_SSSS: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_JU178_P8p_SSSS <- rbind(mean(va_liab_JU178_CONTROL_P8p_SSSS_mod/2),mean(va_liab_JU178_MA_P8p_SSSS_mod/2),mean(va_liab_JU178_Vm_P8p_SSSS_mod/2))
        colnames(mean_va_liab_JU178_P8p_SSSS) <- c("mean")
        median_va_liab_JU178_P8p_SSSS <- rbind(median(va_liab_JU178_CONTROL_P8p_SSSS_mod/2),median(va_liab_JU178_MA_P8p_SSSS_mod/2),median(va_liab_JU178_Vm_P8p_SSSS_mod/2))
        colnames(median_va_liab_JU178_P8p_SSSS) <- c("median")
        posterior.mode_va_liab_JU178_P8p_SSSS <- rbind(posterior.mode(va_liab_JU178_CONTROL_P8p_SSSS_mod/2),posterior.mode(va_liab_JU178_MA_P8p_SSSS_mod/2),posterior.mode(va_liab_JU178_Vm_P8p_SSSS_mod/2))
        colnames(posterior.mode_va_liab_JU178_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_JU178_P8p_SSSS <- rbind(HPDinterval(va_liab_JU178_CONTROL_P8p_SSSS_mod/2),HPDinterval(va_liab_JU178_MA_P8p_SSSS_mod/2),HPDinterval(va_liab_JU178_Vm_P8p_SSSS_mod/2))
        colnames(HPDinterval_0.95_va_liab_JU178_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_JU178_P8p_SSSS <- rbind(HPDinterval(va_liab_JU178_CONTROL_P8p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_JU178_MA_P8p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_JU178_Vm_P8p_SSSS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_JU178_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_JU178_P8p_SSSS <- rbind(effectiveSize(va_liab_JU178_CONTROL_P8p_SSSS_mod/2),effectiveSize(va_liab_JU178_MA_P8p_SSSS_mod/2),effectiveSize(va_liab_JU178_Vm_P8p_SSSS_mod/2))
        colnames(effectiveSize_va_liab_JU178_P8p_SSSS) <- c("effectiveSize")
        va_liab_JU178_P8p_SSSS <- cbind.data.frame(mean_va_liab_JU178_P8p_SSSS,median_va_liab_JU178_P8p_SSSS,posterior.mode_va_liab_JU178_P8p_SSSS,HPDinterval_0.95_va_liab_JU178_P8p_SSSS,HPDinterval_0.83_va_liab_JU178_P8p_SSSS,effectiveSize_va_liab_JU178_P8p_SSSS)
        rownames(va_liab_JU178_P8p_SSSS) <- c("va_liab_JU178_CONTROL_P8p_SSSS_mod","va_liab_JU178_MA_P8p_SSSS_mod","va_liab_JU178_Vm_P8p_SSSS_mod")
        va_liab_JU178_P8p_SSSS <- cbind(Models = rownames(va_liab_JU178_P8p_SSSS),va_liab_JU178_P8p_SSSS)
        rownames(va_liab_JU178_P8p_SSSS) <- NULL
        va_liab_JU178_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        va_liab_JU178_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        va_liab_JU178_P8p_SSSS$Measure <- c("Va","Va","Va")
        va_liab_JU178_P8p_SSSS$Scale <- c("liab","liab","liab")
        va_liab_JU178_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_liab_JU178_P8p_SSSS
      }
      
      #Summary h2_liab_JU178_P8p_SSSS
      {
        mean_h2_liab_JU178_P8p_SSSS <- rbind(mean(h2_liab_JU178_CONTROL_P8p_SSSS_mod),mean(h2_liab_JU178_MA_P8p_SSSS_mod),mean(h2_liab_JU178_Vm_P8p_SSSS_mod))
        colnames(mean_h2_liab_JU178_P8p_SSSS) <- c("mean")
        median_h2_liab_JU178_P8p_SSSS <- rbind(median(h2_liab_JU178_CONTROL_P8p_SSSS_mod),median(h2_liab_JU178_MA_P8p_SSSS_mod),median(h2_liab_JU178_Vm_P8p_SSSS_mod))
        colnames(median_h2_liab_JU178_P8p_SSSS) <- c("median")
        posterior.mode_h2_liab_JU178_P8p_SSSS <- rbind(posterior.mode(h2_liab_JU178_CONTROL_P8p_SSSS_mod),posterior.mode(h2_liab_JU178_MA_P8p_SSSS_mod),posterior.mode(h2_liab_JU178_Vm_P8p_SSSS_mod))
        colnames(posterior.mode_h2_liab_JU178_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_JU178_P8p_SSSS <- rbind(HPDinterval(h2_liab_JU178_CONTROL_P8p_SSSS_mod),HPDinterval(h2_liab_JU178_MA_P8p_SSSS_mod),HPDinterval(h2_liab_JU178_Vm_P8p_SSSS_mod))
        colnames(HPDinterval_0.95_h2_liab_JU178_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_JU178_P8p_SSSS <- rbind(HPDinterval(h2_liab_JU178_CONTROL_P8p_SSSS_mod,prob=.83),HPDinterval(h2_liab_JU178_MA_P8p_SSSS_mod,prob=.83),HPDinterval(h2_liab_JU178_Vm_P8p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_JU178_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_JU178_P8p_SSSS <- rbind(effectiveSize(h2_liab_JU178_CONTROL_P8p_SSSS_mod),effectiveSize(h2_liab_JU178_MA_P8p_SSSS_mod),effectiveSize(h2_liab_JU178_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_h2_liab_JU178_P8p_SSSS) <- c("effectiveSize")
        h2_liab_JU178_P8p_SSSS <- cbind.data.frame(mean_h2_liab_JU178_P8p_SSSS,median_h2_liab_JU178_P8p_SSSS,posterior.mode_h2_liab_JU178_P8p_SSSS,HPDinterval_0.95_h2_liab_JU178_P8p_SSSS,HPDinterval_0.83_h2_liab_JU178_P8p_SSSS,effectiveSize_h2_liab_JU178_P8p_SSSS)
        rownames(h2_liab_JU178_P8p_SSSS) <- c("h2_liab_JU178_CONTROL_P8p_SSSS_mod","h2_liab_JU178_MA_P8p_SSSS_mod","h2_liab_JU178_Vm_P8p_SSSS_mod")
        h2_liab_JU178_P8p_SSSS <- cbind(Models = rownames(h2_liab_JU178_P8p_SSSS),h2_liab_JU178_P8p_SSSS)
        rownames(h2_liab_JU178_P8p_SSSS) <- NULL
        h2_liab_JU178_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        h2_liab_JU178_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_liab_JU178_P8p_SSSS$Measure <- c("H2","H2","H2")
        h2_liab_JU178_P8p_SSSS$Scale <- c("liab","liab","liab")
        h2_liab_JU178_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_liab_JU178_P8p_SSSS
      }
      
      #Summary Evol_liab_JU178_P8p_SSSS
      {
        mean_Evol_liab_JU178_P8p_SSSS <- rbind(mean(Evol_liab_JU178_CONTROL_P8p_SSSS_mod),mean(Evol_liab_JU178_MA_P8p_SSSS_mod),mean(Evol_liab_JU178_Vm_P8p_SSSS_mod))
        colnames(mean_Evol_liab_JU178_P8p_SSSS) <- c("mean")
        median_Evol_liab_JU178_P8p_SSSS <- rbind(median(Evol_liab_JU178_CONTROL_P8p_SSSS_mod),median(Evol_liab_JU178_MA_P8p_SSSS_mod),median(Evol_liab_JU178_Vm_P8p_SSSS_mod))
        colnames(median_Evol_liab_JU178_P8p_SSSS) <- c("median")
        posterior.mode_Evol_liab_JU178_P8p_SSSS <- rbind(posterior.mode(Evol_liab_JU178_CONTROL_P8p_SSSS_mod),posterior.mode(Evol_liab_JU178_MA_P8p_SSSS_mod),posterior.mode(Evol_liab_JU178_Vm_P8p_SSSS_mod))
        colnames(posterior.mode_Evol_liab_JU178_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_JU178_P8p_SSSS <- rbind(HPDinterval(Evol_liab_JU178_CONTROL_P8p_SSSS_mod),HPDinterval(Evol_liab_JU178_MA_P8p_SSSS_mod),HPDinterval(Evol_liab_JU178_Vm_P8p_SSSS_mod))
        colnames(HPDinterval_0.95_Evol_liab_JU178_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_JU178_P8p_SSSS <- rbind(HPDinterval(Evol_liab_JU178_CONTROL_P8p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_JU178_MA_P8p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_JU178_Vm_P8p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_JU178_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_JU178_P8p_SSSS <- rbind(effectiveSize(Evol_liab_JU178_CONTROL_P8p_SSSS_mod),effectiveSize(Evol_liab_JU178_MA_P8p_SSSS_mod),effectiveSize(Evol_liab_JU178_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_Evol_liab_JU178_P8p_SSSS) <- c("effectiveSize")
        Evol_liab_JU178_P8p_SSSS <- cbind.data.frame(mean_Evol_liab_JU178_P8p_SSSS,median_Evol_liab_JU178_P8p_SSSS,posterior.mode_Evol_liab_JU178_P8p_SSSS,HPDinterval_0.95_Evol_liab_JU178_P8p_SSSS,HPDinterval_0.83_Evol_liab_JU178_P8p_SSSS,effectiveSize_Evol_liab_JU178_P8p_SSSS)
        rownames(Evol_liab_JU178_P8p_SSSS) <- c("Evol_liab_JU178_CONTROL_P8p_SSSS_mod","Evol_liab_JU178_MA_P8p_SSSS_mod","Evol_liab_JU178_Vm_P8p_SSSS_mod")
        Evol_liab_JU178_P8p_SSSS <- cbind(Models = rownames(Evol_liab_JU178_P8p_SSSS),Evol_liab_JU178_P8p_SSSS)
        rownames(Evol_liab_JU178_P8p_SSSS) <- NULL
        Evol_liab_JU178_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        Evol_liab_JU178_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_liab_JU178_P8p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_JU178_P8p_SSSS$Scale <- c("liab","liab","liab")
        Evol_liab_JU178_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_JU178_P8p_SSSS
      }
      
      #Summary trait_mean_liab_JU178_P8p_SSSS
      {
        mean_trait_mean_liab_JU178_P8p_SSSS <- rbind(mean(trait_mean_liab_JU178_CONTROL_P8p_SSSS_mod),mean(trait_mean_liab_JU178_MA_P8p_SSSS_mod),mean(trait_mean_liab_JU178_Vm_P8p_SSSS_mod))
        colnames(mean_trait_mean_liab_JU178_P8p_SSSS) <- c("mean")
        median_trait_mean_liab_JU178_P8p_SSSS <- rbind(median(trait_mean_liab_JU178_CONTROL_P8p_SSSS_mod),median(trait_mean_liab_JU178_MA_P8p_SSSS_mod),median(trait_mean_liab_JU178_Vm_P8p_SSSS_mod))
        colnames(median_trait_mean_liab_JU178_P8p_SSSS) <- c("median")
        posterior.mode_trait_mean_liab_JU178_P8p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_JU178_CONTROL_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU178_MA_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU178_Vm_P8p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_liab_JU178_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_JU178_P8p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU178_CONTROL_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU178_MA_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU178_Vm_P8p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_JU178_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_JU178_P8p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU178_CONTROL_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU178_MA_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU178_Vm_P8p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_JU178_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_JU178_P8p_SSSS <- rbind(effectiveSize(trait_mean_liab_JU178_CONTROL_P8p_SSSS_mod),effectiveSize(trait_mean_liab_JU178_MA_P8p_SSSS_mod),effectiveSize(trait_mean_liab_JU178_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_trait_mean_liab_JU178_P8p_SSSS) <- c("effectiveSize")
        trait_mean_liab_JU178_P8p_SSSS <- cbind.data.frame(mean_trait_mean_liab_JU178_P8p_SSSS,median_trait_mean_liab_JU178_P8p_SSSS,posterior.mode_trait_mean_liab_JU178_P8p_SSSS,HPDinterval_0.95_trait_mean_liab_JU178_P8p_SSSS,HPDinterval_0.83_trait_mean_liab_JU178_P8p_SSSS,effectiveSize_trait_mean_liab_JU178_P8p_SSSS)
        rownames(trait_mean_liab_JU178_P8p_SSSS) <- c("trait_mean_liab_JU178_CONTROL_P8p_SSSS_mod","trait_mean_liab_JU178_MA_P8p_SSSS_mod","trait_mean_liab_JU178_Vm_P8p_SSSS_mod")
        trait_mean_liab_JU178_P8p_SSSS <- cbind(Models = rownames(trait_mean_liab_JU178_P8p_SSSS),trait_mean_liab_JU178_P8p_SSSS)
        rownames(trait_mean_liab_JU178_P8p_SSSS) <- NULL
        trait_mean_liab_JU178_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        trait_mean_liab_JU178_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_JU178_P8p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_JU178_P8p_SSSS$Scale <- c("liab","liab","liab")
        trait_mean_liab_JU178_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_JU178_P8p_SSSS
      }
      
      liab_JU178_P8p_SSSS <- rbind.data.frame(va_liab_JU178_P8p_SSSS, h2_liab_JU178_P8p_SSSS,Evol_liab_JU178_P8p_SSSS,trait_mean_liab_JU178_P8p_SSSS)
      liab_JU178_P8p_SSSS
    }
    #Summary data scale JU178 P8p
    {
      #Summary va_data_JU178_P8p_SSSS:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_JU178_P8p_SSSS <- rbind(mean(va_data_JU178_CONTROL_P8p_SSSS_mod/2),mean(va_data_JU178_MA_P8p_SSSS_mod/2),mean(va_data_JU178_Vm_P8p_SSSS_mod/2))
        colnames(mean_va_data_JU178_P8p_SSSS) <- c("mean")
        median_va_data_JU178_P8p_SSSS <- rbind(median(va_data_JU178_CONTROL_P8p_SSSS_mod/2),median(va_data_JU178_MA_P8p_SSSS_mod/2),median(va_data_JU178_Vm_P8p_SSSS_mod/2))
        colnames(median_va_data_JU178_P8p_SSSS) <- c("median")
        posterior.mode_va_data_JU178_P8p_SSSS <- rbind(posterior.mode(as.mcmc(va_data_JU178_CONTROL_P8p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_JU178_MA_P8p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_JU178_Vm_P8p_SSSS_mod/2)))
        colnames(posterior.mode_va_data_JU178_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_JU178_P8p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_JU178_CONTROL_P8p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_JU178_MA_P8p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_JU178_Vm_P8p_SSSS_mod/2)))
        colnames(HPDinterval_0.95_va_data_JU178_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_JU178_P8p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_JU178_CONTROL_P8p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU178_MA_P8p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU178_Vm_P8p_SSSS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_JU178_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_JU178_P8p_SSSS <- rbind(effectiveSize(va_data_JU178_CONTROL_P8p_SSSS_mod/2),effectiveSize(va_data_JU178_MA_P8p_SSSS_mod/2),effectiveSize(va_data_JU178_Vm_P8p_SSSS_mod/2))
        colnames(effectiveSize_va_data_JU178_P8p_SSSS) <- c("effectiveSize")
        va_data_JU178_P8p_SSSS <- cbind.data.frame(mean_va_data_JU178_P8p_SSSS,median_va_data_JU178_P8p_SSSS,posterior.mode_va_data_JU178_P8p_SSSS,HPDinterval_0.95_va_data_JU178_P8p_SSSS,HPDinterval_0.83_va_data_JU178_P8p_SSSS,effectiveSize_va_data_JU178_P8p_SSSS)
        rownames(va_data_JU178_P8p_SSSS) <- c("va_data_JU178_CONTROL_P8p_SSSS_mod","va_data_JU178_MA_P8p_SSSS_mod","va_data_JU178_Vm_P8p_SSSS_mod")
        va_data_JU178_P8p_SSSS <- cbind(Models = rownames(va_data_JU178_P8p_SSSS),va_data_JU178_P8p_SSSS)
        rownames(va_data_JU178_P8p_SSSS) <- NULL
        va_data_JU178_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        va_data_JU178_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        va_data_JU178_P8p_SSSS$Measure <- c("Va","Va","Va")
        va_data_JU178_P8p_SSSS$Scale <- c("data","data","data")
        va_data_JU178_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_data_JU178_P8p_SSSS
      }
      
      #Summary h2_data_JU178_P8p_SSSS
      {
        mean_h2_data_JU178_P8p_SSSS <- rbind(mean(h2_data_JU178_CONTROL_P8p_SSSS_mod),mean(h2_data_JU178_MA_P8p_SSSS_mod),mean(h2_data_JU178_Vm_P8p_SSSS_mod))
        colnames(mean_h2_data_JU178_P8p_SSSS) <- c("mean")
        median_h2_data_JU178_P8p_SSSS <- rbind(median(h2_data_JU178_CONTROL_P8p_SSSS_mod),median(h2_data_JU178_MA_P8p_SSSS_mod),median(h2_data_JU178_Vm_P8p_SSSS_mod))
        colnames(median_h2_data_JU178_P8p_SSSS) <- c("median")
        posterior.mode_h2_data_JU178_P8p_SSSS <- rbind(posterior.mode(as.mcmc(h2_data_JU178_CONTROL_P8p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_JU178_MA_P8p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_JU178_Vm_P8p_SSSS_mod)))
        colnames(posterior.mode_h2_data_JU178_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_JU178_P8p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_JU178_CONTROL_P8p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_JU178_MA_P8p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_JU178_Vm_P8p_SSSS_mod)))
        colnames(HPDinterval_0.95_h2_data_JU178_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_JU178_P8p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_JU178_CONTROL_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU178_MA_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU178_Vm_P8p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_JU178_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_JU178_P8p_SSSS <- rbind(effectiveSize(h2_data_JU178_CONTROL_P8p_SSSS_mod),effectiveSize(h2_data_JU178_MA_P8p_SSSS_mod),effectiveSize(h2_data_JU178_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_h2_data_JU178_P8p_SSSS) <- c("effectiveSize")
        h2_data_JU178_P8p_SSSS <- cbind.data.frame(mean_h2_data_JU178_P8p_SSSS,median_h2_data_JU178_P8p_SSSS,posterior.mode_h2_data_JU178_P8p_SSSS,HPDinterval_0.95_h2_data_JU178_P8p_SSSS,HPDinterval_0.83_h2_data_JU178_P8p_SSSS,effectiveSize_h2_data_JU178_P8p_SSSS)
        rownames(h2_data_JU178_P8p_SSSS) <- c("h2_data_JU178_CONTROL_P8p_SSSS_mod","h2_data_JU178_MA_P8p_SSSS_mod","h2_data_JU178_Vm_P8p_SSSS_mod")
        h2_data_JU178_P8p_SSSS <- cbind(Models = rownames(h2_data_JU178_P8p_SSSS),h2_data_JU178_P8p_SSSS)
        rownames(h2_data_JU178_P8p_SSSS) <- NULL
        h2_data_JU178_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        h2_data_JU178_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_data_JU178_P8p_SSSS$Measure <- c("H2","H2","H2")
        h2_data_JU178_P8p_SSSS$Scale <- c("data","data","data")
        h2_data_JU178_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_data_JU178_P8p_SSSS
      }
      
      #Summary Evol_data_JU178_P8p_SSSS
      {
        mean_Evol_data_JU178_P8p_SSSS <- rbind(mean(Evol_data_JU178_CONTROL_P8p_SSSS_mod),mean(Evol_data_JU178_MA_P8p_SSSS_mod),mean(Evol_data_JU178_Vm_P8p_SSSS_mod))
        colnames(mean_Evol_data_JU178_P8p_SSSS) <- c("mean")
        median_Evol_data_JU178_P8p_SSSS <- rbind(median(Evol_data_JU178_CONTROL_P8p_SSSS_mod),median(Evol_data_JU178_MA_P8p_SSSS_mod),median(Evol_data_JU178_Vm_P8p_SSSS_mod))
        colnames(median_Evol_data_JU178_P8p_SSSS) <- c("median")
        posterior.mode_Evol_data_JU178_P8p_SSSS <- rbind(posterior.mode(as.mcmc(Evol_data_JU178_CONTROL_P8p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_JU178_MA_P8p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_JU178_Vm_P8p_SSSS_mod)))
        colnames(posterior.mode_Evol_data_JU178_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_JU178_P8p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_JU178_CONTROL_P8p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_JU178_MA_P8p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_JU178_Vm_P8p_SSSS_mod)))
        colnames(HPDinterval_0.95_Evol_data_JU178_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_JU178_P8p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_JU178_CONTROL_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU178_MA_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU178_Vm_P8p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_JU178_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_JU178_P8p_SSSS <- rbind(effectiveSize(Evol_data_JU178_CONTROL_P8p_SSSS_mod),effectiveSize(Evol_data_JU178_MA_P8p_SSSS_mod),effectiveSize(Evol_data_JU178_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_Evol_data_JU178_P8p_SSSS) <- c("effectiveSize")
        Evol_data_JU178_P8p_SSSS <- cbind.data.frame(mean_Evol_data_JU178_P8p_SSSS,median_Evol_data_JU178_P8p_SSSS,posterior.mode_Evol_data_JU178_P8p_SSSS,HPDinterval_0.95_Evol_data_JU178_P8p_SSSS,HPDinterval_0.83_Evol_data_JU178_P8p_SSSS,effectiveSize_Evol_data_JU178_P8p_SSSS)
        rownames(Evol_data_JU178_P8p_SSSS) <- c("Evol_data_JU178_CONTROL_P8p_SSSS_mod","Evol_data_JU178_MA_P8p_SSSS_mod","Evol_data_JU178_Vm_P8p_SSSS_mod")
        Evol_data_JU178_P8p_SSSS <- cbind(Models = rownames(Evol_data_JU178_P8p_SSSS),Evol_data_JU178_P8p_SSSS)
        rownames(Evol_data_JU178_P8p_SSSS) <- NULL
        Evol_data_JU178_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        Evol_data_JU178_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_data_JU178_P8p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_data_JU178_P8p_SSSS$Scale <- c("data","data","data")
        Evol_data_JU178_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_data_JU178_P8p_SSSS
      }
      
      #Summary trait_mean_data_JU178_P8p_SSSS
      {
        mean_trait_mean_data_JU178_P8p_SSSS <- rbind(mean(trait_mean_data_JU178_CONTROL_P8p_SSSS_mod),mean(trait_mean_data_JU178_MA_P8p_SSSS_mod),mean(trait_mean_data_JU178_Vm_P8p_SSSS_mod))
        colnames(mean_trait_mean_data_JU178_P8p_SSSS) <- c("mean")
        median_trait_mean_data_JU178_P8p_SSSS <- rbind(median(trait_mean_data_JU178_CONTROL_P8p_SSSS_mod),median(trait_mean_data_JU178_MA_P8p_SSSS_mod),median(trait_mean_data_JU178_Vm_P8p_SSSS_mod))
        colnames(median_trait_mean_data_JU178_P8p_SSSS) <- c("median")
        posterior.mode_trait_mean_data_JU178_P8p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_data_JU178_CONTROL_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_JU178_MA_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_JU178_Vm_P8p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_data_JU178_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_JU178_P8p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU178_CONTROL_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_JU178_MA_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_JU178_Vm_P8p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_JU178_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_JU178_P8p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU178_CONTROL_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU178_MA_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU178_Vm_P8p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_JU178_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_JU178_P8p_SSSS <- rbind(effectiveSize(trait_mean_data_JU178_CONTROL_P8p_SSSS_mod),effectiveSize(trait_mean_data_JU178_MA_P8p_SSSS_mod),effectiveSize(trait_mean_data_JU178_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_trait_mean_data_JU178_P8p_SSSS) <- c("effectiveSize")
        trait_mean_data_JU178_P8p_SSSS <- cbind.data.frame(mean_trait_mean_data_JU178_P8p_SSSS,median_trait_mean_data_JU178_P8p_SSSS,posterior.mode_trait_mean_data_JU178_P8p_SSSS,HPDinterval_0.95_trait_mean_data_JU178_P8p_SSSS,HPDinterval_0.83_trait_mean_data_JU178_P8p_SSSS,effectiveSize_trait_mean_data_JU178_P8p_SSSS)
        rownames(trait_mean_data_JU178_P8p_SSSS) <- c("trait_mean_data_JU178_CONTROL_P8p_SSSS_mod","trait_mean_data_JU178_MA_P8p_SSSS_mod","trait_mean_data_JU178_Vm_P8p_SSSS_mod")
        trait_mean_data_JU178_P8p_SSSS <- cbind(Models = rownames(trait_mean_data_JU178_P8p_SSSS),trait_mean_data_JU178_P8p_SSSS)
        rownames(trait_mean_data_JU178_P8p_SSSS) <- NULL
        trait_mean_data_JU178_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        trait_mean_data_JU178_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_data_JU178_P8p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_JU178_P8p_SSSS$Scale <- c("data","data","data")
        trait_mean_data_JU178_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_JU178_P8p_SSSS
      }
      
      data_JU178_P8p_SSSS <- rbind.data.frame(va_data_JU178_P8p_SSSS, h2_data_JU178_P8p_SSSS,Evol_data_JU178_P8p_SSSS,trait_mean_data_JU178_P8p_SSSS)
      data_JU178_P8p_SSSS
      
    }
    Vm_JU178_P8p_SSSS <- rbind.data.frame(liab_JU178_P8p_SSSS, data_JU178_P8p_SSSS)
    Vm_JU178_P8p_SSSS$Pnp_fate <- rep("SSSS", 24)
    Vm_JU178_P8p_SSSS
    #remove JU178 P8p_SSSSS models
    {
      remove(JU178_CONTROL_P8p_SSSS_mod)
      remove(JU178_MA_P8p_SSSS_mod)
      remove(JU178_Vm_P8p_SSSS_mod)
    }
  }
  
  ##Summary JU178 P5p----
  {
    #Summary liability scale JU178 P5p
    {
      #Summary va_liab_JU178_P5p_wt: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_JU178_P5p_wt <- rbind(mean(va_liab_JU178_CONTROL_P5p_wt_mod/2),mean(va_liab_JU178_MA_P5p_wt_mod/2),mean(va_liab_JU178_Vm_P5p_wt_mod/2))
        colnames(mean_va_liab_JU178_P5p_wt) <- c("mean")
        median_va_liab_JU178_P5p_wt <- rbind(median(va_liab_JU178_CONTROL_P5p_wt_mod/2),median(va_liab_JU178_MA_P5p_wt_mod/2),median(va_liab_JU178_Vm_P5p_wt_mod/2))
        colnames(median_va_liab_JU178_P5p_wt) <- c("median")
        posterior.mode_va_liab_JU178_P5p_wt <- rbind(posterior.mode(va_liab_JU178_CONTROL_P5p_wt_mod/2),posterior.mode(va_liab_JU178_MA_P5p_wt_mod/2),posterior.mode(va_liab_JU178_Vm_P5p_wt_mod/2))
        colnames(posterior.mode_va_liab_JU178_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_JU178_P5p_wt <- rbind(HPDinterval(va_liab_JU178_CONTROL_P5p_wt_mod/2),HPDinterval(va_liab_JU178_MA_P5p_wt_mod/2),HPDinterval(va_liab_JU178_Vm_P5p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_JU178_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_JU178_P5p_wt <- rbind(HPDinterval(va_liab_JU178_CONTROL_P5p_wt_mod/2,prob=.83),HPDinterval(va_liab_JU178_MA_P5p_wt_mod/2,prob=.83),HPDinterval(va_liab_JU178_Vm_P5p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_JU178_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_JU178_P5p_wt <- rbind(effectiveSize(va_liab_JU178_CONTROL_P5p_wt_mod/2),effectiveSize(va_liab_JU178_MA_P5p_wt_mod/2),effectiveSize(va_liab_JU178_Vm_P5p_wt_mod/2))
        colnames(effectiveSize_va_liab_JU178_P5p_wt) <- c("effectiveSize")
        va_liab_JU178_P5p_wt <- cbind.data.frame(mean_va_liab_JU178_P5p_wt,median_va_liab_JU178_P5p_wt,posterior.mode_va_liab_JU178_P5p_wt,HPDinterval_0.95_va_liab_JU178_P5p_wt,HPDinterval_0.83_va_liab_JU178_P5p_wt,effectiveSize_va_liab_JU178_P5p_wt)
        rownames(va_liab_JU178_P5p_wt) <- c("va_liab_JU178_CONTROL_P5p_wt_mod","va_liab_JU178_MA_P5p_wt_mod","va_liab_JU178_Vm_P5p_wt_mod")
        va_liab_JU178_P5p_wt <- cbind(Models = rownames(va_liab_JU178_P5p_wt),va_liab_JU178_P5p_wt)
        rownames(va_liab_JU178_P5p_wt) <- NULL
        va_liab_JU178_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        va_liab_JU178_P5p_wt$Treatment <- c("Control","ML","Vm")
        va_liab_JU178_P5p_wt$Measure <- c("Va","Va","Va")
        va_liab_JU178_P5p_wt$Scale <- c("liab","liab","liab")
        va_liab_JU178_P5p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_JU178_P5p_wt
      }
      
      #Summary h2_liab_JU178_P5p_wt
      {
        mean_h2_liab_JU178_P5p_wt <- rbind(mean(h2_liab_JU178_CONTROL_P5p_wt_mod),mean(h2_liab_JU178_MA_P5p_wt_mod),mean(h2_liab_JU178_Vm_P5p_wt_mod))
        colnames(mean_h2_liab_JU178_P5p_wt) <- c("mean")
        median_h2_liab_JU178_P5p_wt <- rbind(median(h2_liab_JU178_CONTROL_P5p_wt_mod),median(h2_liab_JU178_MA_P5p_wt_mod),median(h2_liab_JU178_Vm_P5p_wt_mod))
        colnames(median_h2_liab_JU178_P5p_wt) <- c("median")
        posterior.mode_h2_liab_JU178_P5p_wt <- rbind(posterior.mode(h2_liab_JU178_CONTROL_P5p_wt_mod),posterior.mode(h2_liab_JU178_MA_P5p_wt_mod),posterior.mode(h2_liab_JU178_Vm_P5p_wt_mod))
        colnames(posterior.mode_h2_liab_JU178_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_JU178_P5p_wt <- rbind(HPDinterval(h2_liab_JU178_CONTROL_P5p_wt_mod),HPDinterval(h2_liab_JU178_MA_P5p_wt_mod),HPDinterval(h2_liab_JU178_Vm_P5p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_JU178_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_JU178_P5p_wt <- rbind(HPDinterval(h2_liab_JU178_CONTROL_P5p_wt_mod,prob=.83),HPDinterval(h2_liab_JU178_MA_P5p_wt_mod,prob=.83),HPDinterval(h2_liab_JU178_Vm_P5p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_JU178_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_JU178_P5p_wt <- rbind(effectiveSize(h2_liab_JU178_CONTROL_P5p_wt_mod),effectiveSize(h2_liab_JU178_MA_P5p_wt_mod),effectiveSize(h2_liab_JU178_Vm_P5p_wt_mod))
        colnames(effectiveSize_h2_liab_JU178_P5p_wt) <- c("effectiveSize")
        h2_liab_JU178_P5p_wt <- cbind.data.frame(mean_h2_liab_JU178_P5p_wt,median_h2_liab_JU178_P5p_wt,posterior.mode_h2_liab_JU178_P5p_wt,HPDinterval_0.95_h2_liab_JU178_P5p_wt,HPDinterval_0.83_h2_liab_JU178_P5p_wt,effectiveSize_h2_liab_JU178_P5p_wt)
        rownames(h2_liab_JU178_P5p_wt) <- c("h2_liab_JU178_CONTROL_P5p_wt_mod","h2_liab_JU178_MA_P5p_wt_mod","h2_liab_JU178_Vm_P5p_wt_mod")
        h2_liab_JU178_P5p_wt <- cbind(Models = rownames(h2_liab_JU178_P5p_wt),h2_liab_JU178_P5p_wt)
        rownames(h2_liab_JU178_P5p_wt) <- NULL
        h2_liab_JU178_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        h2_liab_JU178_P5p_wt$Treatment <- c("Control","ML","Vm")
        h2_liab_JU178_P5p_wt$Measure <- c("H2","H2","H2")
        h2_liab_JU178_P5p_wt$Scale <- c("liab","liab","liab")
        h2_liab_JU178_P5p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_JU178_P5p_wt
      }
      
      #Summary Evol_liab_JU178_P5p_wt
      {
        mean_Evol_liab_JU178_P5p_wt <- rbind(mean(Evol_liab_JU178_CONTROL_P5p_wt_mod),mean(Evol_liab_JU178_MA_P5p_wt_mod),mean(Evol_liab_JU178_Vm_P5p_wt_mod))
        colnames(mean_Evol_liab_JU178_P5p_wt) <- c("mean")
        median_Evol_liab_JU178_P5p_wt <- rbind(median(Evol_liab_JU178_CONTROL_P5p_wt_mod),median(Evol_liab_JU178_MA_P5p_wt_mod),median(Evol_liab_JU178_Vm_P5p_wt_mod))
        colnames(median_Evol_liab_JU178_P5p_wt) <- c("median")
        posterior.mode_Evol_liab_JU178_P5p_wt <- rbind(posterior.mode(Evol_liab_JU178_CONTROL_P5p_wt_mod),posterior.mode(Evol_liab_JU178_MA_P5p_wt_mod),posterior.mode(Evol_liab_JU178_Vm_P5p_wt_mod))
        colnames(posterior.mode_Evol_liab_JU178_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_JU178_P5p_wt <- rbind(HPDinterval(Evol_liab_JU178_CONTROL_P5p_wt_mod),HPDinterval(Evol_liab_JU178_MA_P5p_wt_mod),HPDinterval(Evol_liab_JU178_Vm_P5p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_JU178_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_JU178_P5p_wt <- rbind(HPDinterval(Evol_liab_JU178_CONTROL_P5p_wt_mod,prob=.83),HPDinterval(Evol_liab_JU178_MA_P5p_wt_mod,prob=.83),HPDinterval(Evol_liab_JU178_Vm_P5p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_JU178_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_JU178_P5p_wt <- rbind(effectiveSize(Evol_liab_JU178_CONTROL_P5p_wt_mod),effectiveSize(Evol_liab_JU178_MA_P5p_wt_mod),effectiveSize(Evol_liab_JU178_Vm_P5p_wt_mod))
        colnames(effectiveSize_Evol_liab_JU178_P5p_wt) <- c("effectiveSize")
        Evol_liab_JU178_P5p_wt <- cbind.data.frame(mean_Evol_liab_JU178_P5p_wt,median_Evol_liab_JU178_P5p_wt,posterior.mode_Evol_liab_JU178_P5p_wt,HPDinterval_0.95_Evol_liab_JU178_P5p_wt,HPDinterval_0.83_Evol_liab_JU178_P5p_wt,effectiveSize_Evol_liab_JU178_P5p_wt)
        rownames(Evol_liab_JU178_P5p_wt) <- c("Evol_liab_JU178_CONTROL_P5p_wt_mod","Evol_liab_JU178_MA_P5p_wt_mod","Evol_liab_JU178_Vm_P5p_wt_mod")
        Evol_liab_JU178_P5p_wt <- cbind(Models = rownames(Evol_liab_JU178_P5p_wt),Evol_liab_JU178_P5p_wt)
        rownames(Evol_liab_JU178_P5p_wt) <- NULL
        Evol_liab_JU178_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        Evol_liab_JU178_P5p_wt$Treatment <- c("Control","ML","Vm")
        Evol_liab_JU178_P5p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_JU178_P5p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_JU178_P5p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_JU178_P5p_wt
      }
      
      #Summary trait_mean_liab_JU178_P5p_wt
      {
        mean_trait_mean_liab_JU178_P5p_wt <- rbind(mean(trait_mean_liab_JU178_CONTROL_P5p_wt_mod),mean(trait_mean_liab_JU178_MA_P5p_wt_mod),mean(trait_mean_liab_JU178_Vm_P5p_wt_mod))
        colnames(mean_trait_mean_liab_JU178_P5p_wt) <- c("mean")
        median_trait_mean_liab_JU178_P5p_wt <- rbind(median(trait_mean_liab_JU178_CONTROL_P5p_wt_mod),median(trait_mean_liab_JU178_MA_P5p_wt_mod),median(trait_mean_liab_JU178_Vm_P5p_wt_mod))
        colnames(median_trait_mean_liab_JU178_P5p_wt) <- c("median")
        posterior.mode_trait_mean_liab_JU178_P5p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_JU178_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU178_MA_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU178_Vm_P5p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_JU178_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_JU178_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU178_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU178_MA_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU178_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_JU178_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_JU178_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU178_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU178_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU178_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_JU178_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_JU178_P5p_wt <- rbind(effectiveSize(trait_mean_liab_JU178_CONTROL_P5p_wt_mod),effectiveSize(trait_mean_liab_JU178_MA_P5p_wt_mod),effectiveSize(trait_mean_liab_JU178_Vm_P5p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_JU178_P5p_wt) <- c("effectiveSize")
        trait_mean_liab_JU178_P5p_wt <- cbind.data.frame(mean_trait_mean_liab_JU178_P5p_wt,median_trait_mean_liab_JU178_P5p_wt,posterior.mode_trait_mean_liab_JU178_P5p_wt,HPDinterval_0.95_trait_mean_liab_JU178_P5p_wt,HPDinterval_0.83_trait_mean_liab_JU178_P5p_wt,effectiveSize_trait_mean_liab_JU178_P5p_wt)
        rownames(trait_mean_liab_JU178_P5p_wt) <- c("trait_mean_liab_JU178_CONTROL_P5p_wt_mod","trait_mean_liab_JU178_MA_P5p_wt_mod","trait_mean_liab_JU178_Vm_P5p_wt_mod")
        trait_mean_liab_JU178_P5p_wt <- cbind(Models = rownames(trait_mean_liab_JU178_P5p_wt),trait_mean_liab_JU178_P5p_wt)
        rownames(trait_mean_liab_JU178_P5p_wt) <- NULL
        trait_mean_liab_JU178_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        trait_mean_liab_JU178_P5p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_JU178_P5p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_JU178_P5p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_JU178_P5p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_JU178_P5p_wt
      }
      
      liab_JU178_P5p_wt <- rbind.data.frame(va_liab_JU178_P5p_wt, h2_liab_JU178_P5p_wt,Evol_liab_JU178_P5p_wt,trait_mean_liab_JU178_P5p_wt)
      liab_JU178_P5p_wt
    }
    #Summary data scale JU178 P5p
    {
      #Summary va_data_JU178_P5p_wt:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_JU178_P5p_wt <- rbind(mean(va_data_JU178_CONTROL_P5p_wt_mod/2),mean(va_data_JU178_MA_P5p_wt_mod/2),mean(va_data_JU178_Vm_P5p_wt_mod/2))
        colnames(mean_va_data_JU178_P5p_wt) <- c("mean")
        median_va_data_JU178_P5p_wt <- rbind(median(va_data_JU178_CONTROL_P5p_wt_mod/2),median(va_data_JU178_MA_P5p_wt_mod/2),median(va_data_JU178_Vm_P5p_wt_mod/2))
        colnames(median_va_data_JU178_P5p_wt) <- c("median")
        posterior.mode_va_data_JU178_P5p_wt <- rbind(posterior.mode(as.mcmc(va_data_JU178_CONTROL_P5p_wt_mod/2)),posterior.mode(as.mcmc(va_data_JU178_MA_P5p_wt_mod/2)),posterior.mode(as.mcmc(va_data_JU178_Vm_P5p_wt_mod/2)))
        colnames(posterior.mode_va_data_JU178_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_JU178_P5p_wt <- rbind(HPDinterval(as.mcmc(va_data_JU178_CONTROL_P5p_wt_mod/2)),HPDinterval(as.mcmc(va_data_JU178_MA_P5p_wt_mod/2)),HPDinterval(as.mcmc(va_data_JU178_Vm_P5p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_JU178_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_JU178_P5p_wt <- rbind(HPDinterval(as.mcmc(va_data_JU178_CONTROL_P5p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU178_MA_P5p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU178_Vm_P5p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_JU178_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_JU178_P5p_wt <- rbind(effectiveSize(va_data_JU178_CONTROL_P5p_wt_mod/2),effectiveSize(va_data_JU178_MA_P5p_wt_mod/2),effectiveSize(va_data_JU178_Vm_P5p_wt_mod/2))
        colnames(effectiveSize_va_data_JU178_P5p_wt) <- c("effectiveSize")
        va_data_JU178_P5p_wt <- cbind.data.frame(mean_va_data_JU178_P5p_wt,median_va_data_JU178_P5p_wt,posterior.mode_va_data_JU178_P5p_wt,HPDinterval_0.95_va_data_JU178_P5p_wt,HPDinterval_0.83_va_data_JU178_P5p_wt,effectiveSize_va_data_JU178_P5p_wt)
        rownames(va_data_JU178_P5p_wt) <- c("va_data_JU178_CONTROL_P5p_wt_mod","va_data_JU178_MA_P5p_wt_mod","va_data_JU178_Vm_P5p_wt_mod")
        va_data_JU178_P5p_wt <- cbind(Models = rownames(va_data_JU178_P5p_wt),va_data_JU178_P5p_wt)
        rownames(va_data_JU178_P5p_wt) <- NULL
        va_data_JU178_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        va_data_JU178_P5p_wt$Treatment <- c("Control","ML","Vm")
        va_data_JU178_P5p_wt$Measure <- c("Va","Va","Va")
        va_data_JU178_P5p_wt$Scale <- c("data","data","data")
        va_data_JU178_P5p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_JU178_P5p_wt
      }
      
      #Summary h2_data_JU178_P5p_wt
      {
        mean_h2_data_JU178_P5p_wt <- rbind(mean(h2_data_JU178_CONTROL_P5p_wt_mod),mean(h2_data_JU178_MA_P5p_wt_mod),mean(h2_data_JU178_Vm_P5p_wt_mod))
        colnames(mean_h2_data_JU178_P5p_wt) <- c("mean")
        median_h2_data_JU178_P5p_wt <- rbind(median(h2_data_JU178_CONTROL_P5p_wt_mod),median(h2_data_JU178_MA_P5p_wt_mod),median(h2_data_JU178_Vm_P5p_wt_mod))
        colnames(median_h2_data_JU178_P5p_wt) <- c("median")
        posterior.mode_h2_data_JU178_P5p_wt <- rbind(posterior.mode(as.mcmc(h2_data_JU178_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(h2_data_JU178_MA_P5p_wt_mod)),posterior.mode(as.mcmc(h2_data_JU178_Vm_P5p_wt_mod)))
        colnames(posterior.mode_h2_data_JU178_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_JU178_P5p_wt <- rbind(HPDinterval(as.mcmc(h2_data_JU178_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(h2_data_JU178_MA_P5p_wt_mod)),HPDinterval(as.mcmc(h2_data_JU178_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_JU178_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_JU178_P5p_wt <- rbind(HPDinterval(as.mcmc(h2_data_JU178_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU178_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU178_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_JU178_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_JU178_P5p_wt <- rbind(effectiveSize(h2_data_JU178_CONTROL_P5p_wt_mod),effectiveSize(h2_data_JU178_MA_P5p_wt_mod),effectiveSize(h2_data_JU178_Vm_P5p_wt_mod))
        colnames(effectiveSize_h2_data_JU178_P5p_wt) <- c("effectiveSize")
        h2_data_JU178_P5p_wt <- cbind.data.frame(mean_h2_data_JU178_P5p_wt,median_h2_data_JU178_P5p_wt,posterior.mode_h2_data_JU178_P5p_wt,HPDinterval_0.95_h2_data_JU178_P5p_wt,HPDinterval_0.83_h2_data_JU178_P5p_wt,effectiveSize_h2_data_JU178_P5p_wt)
        rownames(h2_data_JU178_P5p_wt) <- c("h2_data_JU178_CONTROL_P5p_wt_mod","h2_data_JU178_MA_P5p_wt_mod","h2_data_JU178_Vm_P5p_wt_mod")
        h2_data_JU178_P5p_wt <- cbind(Models = rownames(h2_data_JU178_P5p_wt),h2_data_JU178_P5p_wt)
        rownames(h2_data_JU178_P5p_wt) <- NULL
        h2_data_JU178_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        h2_data_JU178_P5p_wt$Treatment <- c("Control","ML","Vm")
        h2_data_JU178_P5p_wt$Measure <- c("H2","H2","H2")
        h2_data_JU178_P5p_wt$Scale <- c("data","data","data")
        h2_data_JU178_P5p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_JU178_P5p_wt
      }
      
      #Summary Evol_data_JU178_P5p_wt
      {
        mean_Evol_data_JU178_P5p_wt <- rbind(mean(Evol_data_JU178_CONTROL_P5p_wt_mod),mean(Evol_data_JU178_MA_P5p_wt_mod),mean(Evol_data_JU178_Vm_P5p_wt_mod))
        colnames(mean_Evol_data_JU178_P5p_wt) <- c("mean")
        median_Evol_data_JU178_P5p_wt <- rbind(median(Evol_data_JU178_CONTROL_P5p_wt_mod),median(Evol_data_JU178_MA_P5p_wt_mod),median(Evol_data_JU178_Vm_P5p_wt_mod))
        colnames(median_Evol_data_JU178_P5p_wt) <- c("median")
        posterior.mode_Evol_data_JU178_P5p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_JU178_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(Evol_data_JU178_MA_P5p_wt_mod)),posterior.mode(as.mcmc(Evol_data_JU178_Vm_P5p_wt_mod)))
        colnames(posterior.mode_Evol_data_JU178_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_JU178_P5p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_JU178_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(Evol_data_JU178_MA_P5p_wt_mod)),HPDinterval(as.mcmc(Evol_data_JU178_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_JU178_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_JU178_P5p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_JU178_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU178_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU178_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_JU178_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_JU178_P5p_wt <- rbind(effectiveSize(Evol_data_JU178_CONTROL_P5p_wt_mod),effectiveSize(Evol_data_JU178_MA_P5p_wt_mod),effectiveSize(Evol_data_JU178_Vm_P5p_wt_mod))
        colnames(effectiveSize_Evol_data_JU178_P5p_wt) <- c("effectiveSize")
        Evol_data_JU178_P5p_wt <- cbind.data.frame(mean_Evol_data_JU178_P5p_wt,median_Evol_data_JU178_P5p_wt,posterior.mode_Evol_data_JU178_P5p_wt,HPDinterval_0.95_Evol_data_JU178_P5p_wt,HPDinterval_0.83_Evol_data_JU178_P5p_wt,effectiveSize_Evol_data_JU178_P5p_wt)
        rownames(Evol_data_JU178_P5p_wt) <- c("Evol_data_JU178_CONTROL_P5p_wt_mod","Evol_data_JU178_MA_P5p_wt_mod","Evol_data_JU178_Vm_P5p_wt_mod")
        Evol_data_JU178_P5p_wt <- cbind(Models = rownames(Evol_data_JU178_P5p_wt),Evol_data_JU178_P5p_wt)
        rownames(Evol_data_JU178_P5p_wt) <- NULL
        Evol_data_JU178_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        Evol_data_JU178_P5p_wt$Treatment <- c("Control","ML","Vm")
        Evol_data_JU178_P5p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_JU178_P5p_wt$Scale <- c("data","data","data")
        Evol_data_JU178_P5p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_JU178_P5p_wt
      }
      
      #Summary trait_mean_data_JU178_P5p_wt
      {
        mean_trait_mean_data_JU178_P5p_wt <- rbind(mean(trait_mean_data_JU178_CONTROL_P5p_wt_mod),mean(trait_mean_data_JU178_MA_P5p_wt_mod),mean(trait_mean_data_JU178_Vm_P5p_wt_mod))
        colnames(mean_trait_mean_data_JU178_P5p_wt) <- c("mean")
        median_trait_mean_data_JU178_P5p_wt <- rbind(median(trait_mean_data_JU178_CONTROL_P5p_wt_mod),median(trait_mean_data_JU178_MA_P5p_wt_mod),median(trait_mean_data_JU178_Vm_P5p_wt_mod))
        colnames(median_trait_mean_data_JU178_P5p_wt) <- c("median")
        posterior.mode_trait_mean_data_JU178_P5p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_JU178_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_JU178_MA_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_JU178_Vm_P5p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_JU178_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_JU178_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU178_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_JU178_MA_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_JU178_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_JU178_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_JU178_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU178_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU178_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU178_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_JU178_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_JU178_P5p_wt <- rbind(effectiveSize(trait_mean_data_JU178_CONTROL_P5p_wt_mod),effectiveSize(trait_mean_data_JU178_MA_P5p_wt_mod),effectiveSize(trait_mean_data_JU178_Vm_P5p_wt_mod))
        colnames(effectiveSize_trait_mean_data_JU178_P5p_wt) <- c("effectiveSize")
        trait_mean_data_JU178_P5p_wt <- cbind.data.frame(mean_trait_mean_data_JU178_P5p_wt,median_trait_mean_data_JU178_P5p_wt,posterior.mode_trait_mean_data_JU178_P5p_wt,HPDinterval_0.95_trait_mean_data_JU178_P5p_wt,HPDinterval_0.83_trait_mean_data_JU178_P5p_wt,effectiveSize_trait_mean_data_JU178_P5p_wt)
        rownames(trait_mean_data_JU178_P5p_wt) <- c("trait_mean_data_JU178_CONTROL_P5p_wt_mod","trait_mean_data_JU178_MA_P5p_wt_mod","trait_mean_data_JU178_Vm_P5p_wt_mod")
        trait_mean_data_JU178_P5p_wt <- cbind(Models = rownames(trait_mean_data_JU178_P5p_wt),trait_mean_data_JU178_P5p_wt)
        rownames(trait_mean_data_JU178_P5p_wt) <- NULL
        trait_mean_data_JU178_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        trait_mean_data_JU178_P5p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_data_JU178_P5p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_JU178_P5p_wt$Scale <- c("data","data","data")
        trait_mean_data_JU178_P5p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_JU178_P5p_wt
      }
      
      data_JU178_P5p_wt <- rbind.data.frame(va_data_JU178_P5p_wt, h2_data_JU178_P5p_wt,Evol_data_JU178_P5p_wt,trait_mean_data_JU178_P5p_wt)
      data_JU178_P5p_wt
      
    }
    Vm_JU178_P5p_wt <- rbind.data.frame(liab_JU178_P5p_wt, data_JU178_P5p_wt)
    Vm_JU178_P5p_wt$Pnp_fate <- rep("wt", 24)
    Vm_JU178_P5p_wt
    #remove JU178 P5p_wt models
    {
      remove(JU178_CONTROL_P5p_wt_mod)
      remove(JU178_MA_P5p_wt_mod)
      remove(JU178_Vm_P5p_wt_mod)
    }
  }
  
  ##Summary JU178 P6p----
  {
    #Summary liability scale JU178 P6p
    {
      #Summary va_liab_JU178_P6p_wt: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_JU178_P6p_wt <- rbind(mean(va_liab_JU178_CONTROL_P6p_wt_mod/2),mean(va_liab_JU178_MA_P6p_wt_mod/2),mean(va_liab_JU178_Vm_P6p_wt_mod/2))
        colnames(mean_va_liab_JU178_P6p_wt) <- c("mean")
        median_va_liab_JU178_P6p_wt <- rbind(median(va_liab_JU178_CONTROL_P6p_wt_mod/2),median(va_liab_JU178_MA_P6p_wt_mod/2),median(va_liab_JU178_Vm_P6p_wt_mod/2))
        colnames(median_va_liab_JU178_P6p_wt) <- c("median")
        posterior.mode_va_liab_JU178_P6p_wt <- rbind(posterior.mode(va_liab_JU178_CONTROL_P6p_wt_mod/2),posterior.mode(va_liab_JU178_MA_P6p_wt_mod/2),posterior.mode(va_liab_JU178_Vm_P6p_wt_mod/2))
        colnames(posterior.mode_va_liab_JU178_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_JU178_P6p_wt <- rbind(HPDinterval(va_liab_JU178_CONTROL_P6p_wt_mod/2),HPDinterval(va_liab_JU178_MA_P6p_wt_mod/2),HPDinterval(va_liab_JU178_Vm_P6p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_JU178_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_JU178_P6p_wt <- rbind(HPDinterval(va_liab_JU178_CONTROL_P6p_wt_mod/2,prob=.83),HPDinterval(va_liab_JU178_MA_P6p_wt_mod/2,prob=.83),HPDinterval(va_liab_JU178_Vm_P6p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_JU178_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_JU178_P6p_wt <- rbind(effectiveSize(va_liab_JU178_CONTROL_P6p_wt_mod/2),effectiveSize(va_liab_JU178_MA_P6p_wt_mod/2),effectiveSize(va_liab_JU178_Vm_P6p_wt_mod/2))
        colnames(effectiveSize_va_liab_JU178_P6p_wt) <- c("effectiveSize")
        va_liab_JU178_P6p_wt <- cbind.data.frame(mean_va_liab_JU178_P6p_wt,median_va_liab_JU178_P6p_wt,posterior.mode_va_liab_JU178_P6p_wt,HPDinterval_0.95_va_liab_JU178_P6p_wt,HPDinterval_0.83_va_liab_JU178_P6p_wt,effectiveSize_va_liab_JU178_P6p_wt)
        rownames(va_liab_JU178_P6p_wt) <- c("va_liab_JU178_CONTROL_P6p_wt_mod","va_liab_JU178_MA_P6p_wt_mod","va_liab_JU178_Vm_P6p_wt_mod")
        va_liab_JU178_P6p_wt <- cbind(Models = rownames(va_liab_JU178_P6p_wt),va_liab_JU178_P6p_wt)
        rownames(va_liab_JU178_P6p_wt) <- NULL
        va_liab_JU178_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        va_liab_JU178_P6p_wt$Treatment <- c("Control","ML","Vm")
        va_liab_JU178_P6p_wt$Measure <- c("Va","Va","Va")
        va_liab_JU178_P6p_wt$Scale <- c("liab","liab","liab")
        va_liab_JU178_P6p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_JU178_P6p_wt
      }
      
      #Summary h2_liab_JU178_P6p_wt
      {
        mean_h2_liab_JU178_P6p_wt <- rbind(mean(h2_liab_JU178_CONTROL_P6p_wt_mod),mean(h2_liab_JU178_MA_P6p_wt_mod),mean(h2_liab_JU178_Vm_P6p_wt_mod))
        colnames(mean_h2_liab_JU178_P6p_wt) <- c("mean")
        median_h2_liab_JU178_P6p_wt <- rbind(median(h2_liab_JU178_CONTROL_P6p_wt_mod),median(h2_liab_JU178_MA_P6p_wt_mod),median(h2_liab_JU178_Vm_P6p_wt_mod))
        colnames(median_h2_liab_JU178_P6p_wt) <- c("median")
        posterior.mode_h2_liab_JU178_P6p_wt <- rbind(posterior.mode(h2_liab_JU178_CONTROL_P6p_wt_mod),posterior.mode(h2_liab_JU178_MA_P6p_wt_mod),posterior.mode(h2_liab_JU178_Vm_P6p_wt_mod))
        colnames(posterior.mode_h2_liab_JU178_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_JU178_P6p_wt <- rbind(HPDinterval(h2_liab_JU178_CONTROL_P6p_wt_mod),HPDinterval(h2_liab_JU178_MA_P6p_wt_mod),HPDinterval(h2_liab_JU178_Vm_P6p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_JU178_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_JU178_P6p_wt <- rbind(HPDinterval(h2_liab_JU178_CONTROL_P6p_wt_mod,prob=.83),HPDinterval(h2_liab_JU178_MA_P6p_wt_mod,prob=.83),HPDinterval(h2_liab_JU178_Vm_P6p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_JU178_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_JU178_P6p_wt <- rbind(effectiveSize(h2_liab_JU178_CONTROL_P6p_wt_mod),effectiveSize(h2_liab_JU178_MA_P6p_wt_mod),effectiveSize(h2_liab_JU178_Vm_P6p_wt_mod))
        colnames(effectiveSize_h2_liab_JU178_P6p_wt) <- c("effectiveSize")
        h2_liab_JU178_P6p_wt <- cbind.data.frame(mean_h2_liab_JU178_P6p_wt,median_h2_liab_JU178_P6p_wt,posterior.mode_h2_liab_JU178_P6p_wt,HPDinterval_0.95_h2_liab_JU178_P6p_wt,HPDinterval_0.83_h2_liab_JU178_P6p_wt,effectiveSize_h2_liab_JU178_P6p_wt)
        rownames(h2_liab_JU178_P6p_wt) <- c("h2_liab_JU178_CONTROL_P6p_wt_mod","h2_liab_JU178_MA_P6p_wt_mod","h2_liab_JU178_Vm_P6p_wt_mod")
        h2_liab_JU178_P6p_wt <- cbind(Models = rownames(h2_liab_JU178_P6p_wt),h2_liab_JU178_P6p_wt)
        rownames(h2_liab_JU178_P6p_wt) <- NULL
        h2_liab_JU178_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        h2_liab_JU178_P6p_wt$Treatment <- c("Control","ML","Vm")
        h2_liab_JU178_P6p_wt$Measure <- c("H2","H2","H2")
        h2_liab_JU178_P6p_wt$Scale <- c("liab","liab","liab")
        h2_liab_JU178_P6p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_JU178_P6p_wt
      }
      
      #Summary Evol_liab_JU178_P6p_wt
      {
        mean_Evol_liab_JU178_P6p_wt <- rbind(mean(Evol_liab_JU178_CONTROL_P6p_wt_mod),mean(Evol_liab_JU178_MA_P6p_wt_mod),mean(Evol_liab_JU178_Vm_P6p_wt_mod))
        colnames(mean_Evol_liab_JU178_P6p_wt) <- c("mean")
        median_Evol_liab_JU178_P6p_wt <- rbind(median(Evol_liab_JU178_CONTROL_P6p_wt_mod),median(Evol_liab_JU178_MA_P6p_wt_mod),median(Evol_liab_JU178_Vm_P6p_wt_mod))
        colnames(median_Evol_liab_JU178_P6p_wt) <- c("median")
        posterior.mode_Evol_liab_JU178_P6p_wt <- rbind(posterior.mode(Evol_liab_JU178_CONTROL_P6p_wt_mod),posterior.mode(Evol_liab_JU178_MA_P6p_wt_mod),posterior.mode(Evol_liab_JU178_Vm_P6p_wt_mod))
        colnames(posterior.mode_Evol_liab_JU178_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_JU178_P6p_wt <- rbind(HPDinterval(Evol_liab_JU178_CONTROL_P6p_wt_mod),HPDinterval(Evol_liab_JU178_MA_P6p_wt_mod),HPDinterval(Evol_liab_JU178_Vm_P6p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_JU178_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_JU178_P6p_wt <- rbind(HPDinterval(Evol_liab_JU178_CONTROL_P6p_wt_mod,prob=.83),HPDinterval(Evol_liab_JU178_MA_P6p_wt_mod,prob=.83),HPDinterval(Evol_liab_JU178_Vm_P6p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_JU178_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_JU178_P6p_wt <- rbind(effectiveSize(Evol_liab_JU178_CONTROL_P6p_wt_mod),effectiveSize(Evol_liab_JU178_MA_P6p_wt_mod),effectiveSize(Evol_liab_JU178_Vm_P6p_wt_mod))
        colnames(effectiveSize_Evol_liab_JU178_P6p_wt) <- c("effectiveSize")
        Evol_liab_JU178_P6p_wt <- cbind.data.frame(mean_Evol_liab_JU178_P6p_wt,median_Evol_liab_JU178_P6p_wt,posterior.mode_Evol_liab_JU178_P6p_wt,HPDinterval_0.95_Evol_liab_JU178_P6p_wt,HPDinterval_0.83_Evol_liab_JU178_P6p_wt,effectiveSize_Evol_liab_JU178_P6p_wt)
        rownames(Evol_liab_JU178_P6p_wt) <- c("Evol_liab_JU178_CONTROL_P6p_wt_mod","Evol_liab_JU178_MA_P6p_wt_mod","Evol_liab_JU178_Vm_P6p_wt_mod")
        Evol_liab_JU178_P6p_wt <- cbind(Models = rownames(Evol_liab_JU178_P6p_wt),Evol_liab_JU178_P6p_wt)
        rownames(Evol_liab_JU178_P6p_wt) <- NULL
        Evol_liab_JU178_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        Evol_liab_JU178_P6p_wt$Treatment <- c("Control","ML","Vm")
        Evol_liab_JU178_P6p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_JU178_P6p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_JU178_P6p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_JU178_P6p_wt
      }
      
      #Summary trait_mean_liab_JU178_P6p_wt
      {
        mean_trait_mean_liab_JU178_P6p_wt <- rbind(mean(trait_mean_liab_JU178_CONTROL_P6p_wt_mod),mean(trait_mean_liab_JU178_MA_P6p_wt_mod),mean(trait_mean_liab_JU178_Vm_P6p_wt_mod))
        colnames(mean_trait_mean_liab_JU178_P6p_wt) <- c("mean")
        median_trait_mean_liab_JU178_P6p_wt <- rbind(median(trait_mean_liab_JU178_CONTROL_P6p_wt_mod),median(trait_mean_liab_JU178_MA_P6p_wt_mod),median(trait_mean_liab_JU178_Vm_P6p_wt_mod))
        colnames(median_trait_mean_liab_JU178_P6p_wt) <- c("median")
        posterior.mode_trait_mean_liab_JU178_P6p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_JU178_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU178_MA_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU178_Vm_P6p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_JU178_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_JU178_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU178_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU178_MA_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU178_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_JU178_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_JU178_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU178_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU178_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU178_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_JU178_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_JU178_P6p_wt <- rbind(effectiveSize(trait_mean_liab_JU178_CONTROL_P6p_wt_mod),effectiveSize(trait_mean_liab_JU178_MA_P6p_wt_mod),effectiveSize(trait_mean_liab_JU178_Vm_P6p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_JU178_P6p_wt) <- c("effectiveSize")
        trait_mean_liab_JU178_P6p_wt <- cbind.data.frame(mean_trait_mean_liab_JU178_P6p_wt,median_trait_mean_liab_JU178_P6p_wt,posterior.mode_trait_mean_liab_JU178_P6p_wt,HPDinterval_0.95_trait_mean_liab_JU178_P6p_wt,HPDinterval_0.83_trait_mean_liab_JU178_P6p_wt,effectiveSize_trait_mean_liab_JU178_P6p_wt)
        rownames(trait_mean_liab_JU178_P6p_wt) <- c("trait_mean_liab_JU178_CONTROL_P6p_wt_mod","trait_mean_liab_JU178_MA_P6p_wt_mod","trait_mean_liab_JU178_Vm_P6p_wt_mod")
        trait_mean_liab_JU178_P6p_wt <- cbind(Models = rownames(trait_mean_liab_JU178_P6p_wt),trait_mean_liab_JU178_P6p_wt)
        rownames(trait_mean_liab_JU178_P6p_wt) <- NULL
        trait_mean_liab_JU178_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        trait_mean_liab_JU178_P6p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_JU178_P6p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_JU178_P6p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_JU178_P6p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_JU178_P6p_wt
      }
      
      liab_JU178_P6p_wt <- rbind.data.frame(va_liab_JU178_P6p_wt, h2_liab_JU178_P6p_wt,Evol_liab_JU178_P6p_wt,trait_mean_liab_JU178_P6p_wt)
      liab_JU178_P6p_wt
    }
    #Summary data scale JU178 P6p
    {
      #Summary va_data_JU178_P6p_wt:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_JU178_P6p_wt <- rbind(mean(va_data_JU178_CONTROL_P6p_wt_mod/2),mean(va_data_JU178_MA_P6p_wt_mod/2),mean(va_data_JU178_Vm_P6p_wt_mod/2))
        colnames(mean_va_data_JU178_P6p_wt) <- c("mean")
        median_va_data_JU178_P6p_wt <- rbind(median(va_data_JU178_CONTROL_P6p_wt_mod/2),median(va_data_JU178_MA_P6p_wt_mod/2),median(va_data_JU178_Vm_P6p_wt_mod/2))
        colnames(median_va_data_JU178_P6p_wt) <- c("median")
        posterior.mode_va_data_JU178_P6p_wt <- rbind(posterior.mode(as.mcmc(va_data_JU178_CONTROL_P6p_wt_mod/2)),posterior.mode(as.mcmc(va_data_JU178_MA_P6p_wt_mod/2)),posterior.mode(as.mcmc(va_data_JU178_Vm_P6p_wt_mod/2)))
        colnames(posterior.mode_va_data_JU178_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_JU178_P6p_wt <- rbind(HPDinterval(as.mcmc(va_data_JU178_CONTROL_P6p_wt_mod/2)),HPDinterval(as.mcmc(va_data_JU178_MA_P6p_wt_mod/2)),HPDinterval(as.mcmc(va_data_JU178_Vm_P6p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_JU178_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_JU178_P6p_wt <- rbind(HPDinterval(as.mcmc(va_data_JU178_CONTROL_P6p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU178_MA_P6p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU178_Vm_P6p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_JU178_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_JU178_P6p_wt <- rbind(effectiveSize(va_data_JU178_CONTROL_P6p_wt_mod/2),effectiveSize(va_data_JU178_MA_P6p_wt_mod/2),effectiveSize(va_data_JU178_Vm_P6p_wt_mod/2))
        colnames(effectiveSize_va_data_JU178_P6p_wt) <- c("effectiveSize")
        va_data_JU178_P6p_wt <- cbind.data.frame(mean_va_data_JU178_P6p_wt,median_va_data_JU178_P6p_wt,posterior.mode_va_data_JU178_P6p_wt,HPDinterval_0.95_va_data_JU178_P6p_wt,HPDinterval_0.83_va_data_JU178_P6p_wt,effectiveSize_va_data_JU178_P6p_wt)
        rownames(va_data_JU178_P6p_wt) <- c("va_data_JU178_CONTROL_P6p_wt_mod","va_data_JU178_MA_P6p_wt_mod","va_data_JU178_Vm_P6p_wt_mod")
        va_data_JU178_P6p_wt <- cbind(Models = rownames(va_data_JU178_P6p_wt),va_data_JU178_P6p_wt)
        rownames(va_data_JU178_P6p_wt) <- NULL
        va_data_JU178_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        va_data_JU178_P6p_wt$Treatment <- c("Control","ML","Vm")
        va_data_JU178_P6p_wt$Measure <- c("Va","Va","Va")
        va_data_JU178_P6p_wt$Scale <- c("data","data","data")
        va_data_JU178_P6p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_JU178_P6p_wt
      }
      
      #Summary h2_data_JU178_P6p_wt
      {
        mean_h2_data_JU178_P6p_wt <- rbind(mean(h2_data_JU178_CONTROL_P6p_wt_mod),mean(h2_data_JU178_MA_P6p_wt_mod),mean(h2_data_JU178_Vm_P6p_wt_mod))
        colnames(mean_h2_data_JU178_P6p_wt) <- c("mean")
        median_h2_data_JU178_P6p_wt <- rbind(median(h2_data_JU178_CONTROL_P6p_wt_mod),median(h2_data_JU178_MA_P6p_wt_mod),median(h2_data_JU178_Vm_P6p_wt_mod))
        colnames(median_h2_data_JU178_P6p_wt) <- c("median")
        posterior.mode_h2_data_JU178_P6p_wt <- rbind(posterior.mode(as.mcmc(h2_data_JU178_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(h2_data_JU178_MA_P6p_wt_mod)),posterior.mode(as.mcmc(h2_data_JU178_Vm_P6p_wt_mod)))
        colnames(posterior.mode_h2_data_JU178_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_JU178_P6p_wt <- rbind(HPDinterval(as.mcmc(h2_data_JU178_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(h2_data_JU178_MA_P6p_wt_mod)),HPDinterval(as.mcmc(h2_data_JU178_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_JU178_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_JU178_P6p_wt <- rbind(HPDinterval(as.mcmc(h2_data_JU178_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU178_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU178_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_JU178_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_JU178_P6p_wt <- rbind(effectiveSize(h2_data_JU178_CONTROL_P6p_wt_mod),effectiveSize(h2_data_JU178_MA_P6p_wt_mod),effectiveSize(h2_data_JU178_Vm_P6p_wt_mod))
        colnames(effectiveSize_h2_data_JU178_P6p_wt) <- c("effectiveSize")
        h2_data_JU178_P6p_wt <- cbind.data.frame(mean_h2_data_JU178_P6p_wt,median_h2_data_JU178_P6p_wt,posterior.mode_h2_data_JU178_P6p_wt,HPDinterval_0.95_h2_data_JU178_P6p_wt,HPDinterval_0.83_h2_data_JU178_P6p_wt,effectiveSize_h2_data_JU178_P6p_wt)
        rownames(h2_data_JU178_P6p_wt) <- c("h2_data_JU178_CONTROL_P6p_wt_mod","h2_data_JU178_MA_P6p_wt_mod","h2_data_JU178_Vm_P6p_wt_mod")
        h2_data_JU178_P6p_wt <- cbind(Models = rownames(h2_data_JU178_P6p_wt),h2_data_JU178_P6p_wt)
        rownames(h2_data_JU178_P6p_wt) <- NULL
        h2_data_JU178_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        h2_data_JU178_P6p_wt$Treatment <- c("Control","ML","Vm")
        h2_data_JU178_P6p_wt$Measure <- c("H2","H2","H2")
        h2_data_JU178_P6p_wt$Scale <- c("data","data","data")
        h2_data_JU178_P6p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_JU178_P6p_wt
      }
      
      #Summary Evol_data_JU178_P6p_wt
      {
        mean_Evol_data_JU178_P6p_wt <- rbind(mean(Evol_data_JU178_CONTROL_P6p_wt_mod),mean(Evol_data_JU178_MA_P6p_wt_mod),mean(Evol_data_JU178_Vm_P6p_wt_mod))
        colnames(mean_Evol_data_JU178_P6p_wt) <- c("mean")
        median_Evol_data_JU178_P6p_wt <- rbind(median(Evol_data_JU178_CONTROL_P6p_wt_mod),median(Evol_data_JU178_MA_P6p_wt_mod),median(Evol_data_JU178_Vm_P6p_wt_mod))
        colnames(median_Evol_data_JU178_P6p_wt) <- c("median")
        posterior.mode_Evol_data_JU178_P6p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_JU178_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(Evol_data_JU178_MA_P6p_wt_mod)),posterior.mode(as.mcmc(Evol_data_JU178_Vm_P6p_wt_mod)))
        colnames(posterior.mode_Evol_data_JU178_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_JU178_P6p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_JU178_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(Evol_data_JU178_MA_P6p_wt_mod)),HPDinterval(as.mcmc(Evol_data_JU178_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_JU178_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_JU178_P6p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_JU178_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU178_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU178_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_JU178_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_JU178_P6p_wt <- rbind(effectiveSize(Evol_data_JU178_CONTROL_P6p_wt_mod),effectiveSize(Evol_data_JU178_MA_P6p_wt_mod),effectiveSize(Evol_data_JU178_Vm_P6p_wt_mod))
        colnames(effectiveSize_Evol_data_JU178_P6p_wt) <- c("effectiveSize")
        Evol_data_JU178_P6p_wt <- cbind.data.frame(mean_Evol_data_JU178_P6p_wt,median_Evol_data_JU178_P6p_wt,posterior.mode_Evol_data_JU178_P6p_wt,HPDinterval_0.95_Evol_data_JU178_P6p_wt,HPDinterval_0.83_Evol_data_JU178_P6p_wt,effectiveSize_Evol_data_JU178_P6p_wt)
        rownames(Evol_data_JU178_P6p_wt) <- c("Evol_data_JU178_CONTROL_P6p_wt_mod","Evol_data_JU178_MA_P6p_wt_mod","Evol_data_JU178_Vm_P6p_wt_mod")
        Evol_data_JU178_P6p_wt <- cbind(Models = rownames(Evol_data_JU178_P6p_wt),Evol_data_JU178_P6p_wt)
        rownames(Evol_data_JU178_P6p_wt) <- NULL
        Evol_data_JU178_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        Evol_data_JU178_P6p_wt$Treatment <- c("Control","ML","Vm")
        Evol_data_JU178_P6p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_JU178_P6p_wt$Scale <- c("data","data","data")
        Evol_data_JU178_P6p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_JU178_P6p_wt
      }
      
      #Summary trait_mean_data_JU178_P6p_wt
      {
        mean_trait_mean_data_JU178_P6p_wt <- rbind(mean(trait_mean_data_JU178_CONTROL_P6p_wt_mod),mean(trait_mean_data_JU178_MA_P6p_wt_mod),mean(trait_mean_data_JU178_Vm_P6p_wt_mod))
        colnames(mean_trait_mean_data_JU178_P6p_wt) <- c("mean")
        median_trait_mean_data_JU178_P6p_wt <- rbind(median(trait_mean_data_JU178_CONTROL_P6p_wt_mod),median(trait_mean_data_JU178_MA_P6p_wt_mod),median(trait_mean_data_JU178_Vm_P6p_wt_mod))
        colnames(median_trait_mean_data_JU178_P6p_wt) <- c("median")
        posterior.mode_trait_mean_data_JU178_P6p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_JU178_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_JU178_MA_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_JU178_Vm_P6p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_JU178_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_JU178_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU178_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_JU178_MA_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_JU178_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_JU178_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_JU178_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU178_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU178_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU178_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_JU178_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_JU178_P6p_wt <- rbind(effectiveSize(trait_mean_data_JU178_CONTROL_P6p_wt_mod),effectiveSize(trait_mean_data_JU178_MA_P6p_wt_mod),effectiveSize(trait_mean_data_JU178_Vm_P6p_wt_mod))
        colnames(effectiveSize_trait_mean_data_JU178_P6p_wt) <- c("effectiveSize")
        trait_mean_data_JU178_P6p_wt <- cbind.data.frame(mean_trait_mean_data_JU178_P6p_wt,median_trait_mean_data_JU178_P6p_wt,posterior.mode_trait_mean_data_JU178_P6p_wt,HPDinterval_0.95_trait_mean_data_JU178_P6p_wt,HPDinterval_0.83_trait_mean_data_JU178_P6p_wt,effectiveSize_trait_mean_data_JU178_P6p_wt)
        rownames(trait_mean_data_JU178_P6p_wt) <- c("trait_mean_data_JU178_CONTROL_P6p_wt_mod","trait_mean_data_JU178_MA_P6p_wt_mod","trait_mean_data_JU178_Vm_P6p_wt_mod")
        trait_mean_data_JU178_P6p_wt <- cbind(Models = rownames(trait_mean_data_JU178_P6p_wt),trait_mean_data_JU178_P6p_wt)
        rownames(trait_mean_data_JU178_P6p_wt) <- NULL
        trait_mean_data_JU178_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        trait_mean_data_JU178_P6p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_data_JU178_P6p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_JU178_P6p_wt$Scale <- c("data","data","data")
        trait_mean_data_JU178_P6p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_JU178_P6p_wt
      }
      
      data_JU178_P6p_wt <- rbind.data.frame(va_data_JU178_P6p_wt, h2_data_JU178_P6p_wt,Evol_data_JU178_P6p_wt,trait_mean_data_JU178_P6p_wt)
      data_JU178_P6p_wt
      
    }
    Vm_JU178_P6p_wt <- rbind.data.frame(liab_JU178_P6p_wt, data_JU178_P6p_wt)
    Vm_JU178_P6p_wt$Pnp_fate <- rep("wt", 24)
    Vm_JU178_P6p_wt
    #remove JU178 P6p_wt models
    {
      remove(JU178_CONTROL_P6p_wt_mod)
      remove(JU178_MA_P6p_wt_mod)
      remove(JU178_Vm_P6p_wt_mod)
    }
  }
  
  ##Summary JU178 P7p----
  {
    #Summary liability scale JU178 P7p
    {
      #Summary va_liab_JU178_P7p_wt: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_JU178_P7p_wt <- rbind(mean(va_liab_JU178_CONTROL_P7p_wt_mod/2),mean(va_liab_JU178_MA_P7p_wt_mod/2),mean(va_liab_JU178_Vm_P7p_wt_mod/2))
        colnames(mean_va_liab_JU178_P7p_wt) <- c("mean")
        median_va_liab_JU178_P7p_wt <- rbind(median(va_liab_JU178_CONTROL_P7p_wt_mod/2),median(va_liab_JU178_MA_P7p_wt_mod/2),median(va_liab_JU178_Vm_P7p_wt_mod/2))
        colnames(median_va_liab_JU178_P7p_wt) <- c("median")
        posterior.mode_va_liab_JU178_P7p_wt <- rbind(posterior.mode(va_liab_JU178_CONTROL_P7p_wt_mod/2),posterior.mode(va_liab_JU178_MA_P7p_wt_mod/2),posterior.mode(va_liab_JU178_Vm_P7p_wt_mod/2))
        colnames(posterior.mode_va_liab_JU178_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_JU178_P7p_wt <- rbind(HPDinterval(va_liab_JU178_CONTROL_P7p_wt_mod/2),HPDinterval(va_liab_JU178_MA_P7p_wt_mod/2),HPDinterval(va_liab_JU178_Vm_P7p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_JU178_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_JU178_P7p_wt <- rbind(HPDinterval(va_liab_JU178_CONTROL_P7p_wt_mod/2,prob=.83),HPDinterval(va_liab_JU178_MA_P7p_wt_mod/2,prob=.83),HPDinterval(va_liab_JU178_Vm_P7p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_JU178_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_JU178_P7p_wt <- rbind(effectiveSize(va_liab_JU178_CONTROL_P7p_wt_mod/2),effectiveSize(va_liab_JU178_MA_P7p_wt_mod/2),effectiveSize(va_liab_JU178_Vm_P7p_wt_mod/2))
        colnames(effectiveSize_va_liab_JU178_P7p_wt) <- c("effectiveSize")
        va_liab_JU178_P7p_wt <- cbind.data.frame(mean_va_liab_JU178_P7p_wt,median_va_liab_JU178_P7p_wt,posterior.mode_va_liab_JU178_P7p_wt,HPDinterval_0.95_va_liab_JU178_P7p_wt,HPDinterval_0.83_va_liab_JU178_P7p_wt,effectiveSize_va_liab_JU178_P7p_wt)
        rownames(va_liab_JU178_P7p_wt) <- c("va_liab_JU178_CONTROL_P7p_wt_mod","va_liab_JU178_MA_P7p_wt_mod","va_liab_JU178_Vm_P7p_wt_mod")
        va_liab_JU178_P7p_wt <- cbind(Models = rownames(va_liab_JU178_P7p_wt),va_liab_JU178_P7p_wt)
        rownames(va_liab_JU178_P7p_wt) <- NULL
        va_liab_JU178_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        va_liab_JU178_P7p_wt$Treatment <- c("Control","ML","Vm")
        va_liab_JU178_P7p_wt$Measure <- c("Va","Va","Va")
        va_liab_JU178_P7p_wt$Scale <- c("liab","liab","liab")
        va_liab_JU178_P7p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_JU178_P7p_wt
      }
      
      #Summary h2_liab_JU178_P7p_wt
      {
        mean_h2_liab_JU178_P7p_wt <- rbind(mean(h2_liab_JU178_CONTROL_P7p_wt_mod),mean(h2_liab_JU178_MA_P7p_wt_mod),mean(h2_liab_JU178_Vm_P7p_wt_mod))
        colnames(mean_h2_liab_JU178_P7p_wt) <- c("mean")
        median_h2_liab_JU178_P7p_wt <- rbind(median(h2_liab_JU178_CONTROL_P7p_wt_mod),median(h2_liab_JU178_MA_P7p_wt_mod),median(h2_liab_JU178_Vm_P7p_wt_mod))
        colnames(median_h2_liab_JU178_P7p_wt) <- c("median")
        posterior.mode_h2_liab_JU178_P7p_wt <- rbind(posterior.mode(h2_liab_JU178_CONTROL_P7p_wt_mod),posterior.mode(h2_liab_JU178_MA_P7p_wt_mod),posterior.mode(h2_liab_JU178_Vm_P7p_wt_mod))
        colnames(posterior.mode_h2_liab_JU178_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_JU178_P7p_wt <- rbind(HPDinterval(h2_liab_JU178_CONTROL_P7p_wt_mod),HPDinterval(h2_liab_JU178_MA_P7p_wt_mod),HPDinterval(h2_liab_JU178_Vm_P7p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_JU178_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_JU178_P7p_wt <- rbind(HPDinterval(h2_liab_JU178_CONTROL_P7p_wt_mod,prob=.83),HPDinterval(h2_liab_JU178_MA_P7p_wt_mod,prob=.83),HPDinterval(h2_liab_JU178_Vm_P7p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_JU178_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_JU178_P7p_wt <- rbind(effectiveSize(h2_liab_JU178_CONTROL_P7p_wt_mod),effectiveSize(h2_liab_JU178_MA_P7p_wt_mod),effectiveSize(h2_liab_JU178_Vm_P7p_wt_mod))
        colnames(effectiveSize_h2_liab_JU178_P7p_wt) <- c("effectiveSize")
        h2_liab_JU178_P7p_wt <- cbind.data.frame(mean_h2_liab_JU178_P7p_wt,median_h2_liab_JU178_P7p_wt,posterior.mode_h2_liab_JU178_P7p_wt,HPDinterval_0.95_h2_liab_JU178_P7p_wt,HPDinterval_0.83_h2_liab_JU178_P7p_wt,effectiveSize_h2_liab_JU178_P7p_wt)
        rownames(h2_liab_JU178_P7p_wt) <- c("h2_liab_JU178_CONTROL_P7p_wt_mod","h2_liab_JU178_MA_P7p_wt_mod","h2_liab_JU178_Vm_P7p_wt_mod")
        h2_liab_JU178_P7p_wt <- cbind(Models = rownames(h2_liab_JU178_P7p_wt),h2_liab_JU178_P7p_wt)
        rownames(h2_liab_JU178_P7p_wt) <- NULL
        h2_liab_JU178_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        h2_liab_JU178_P7p_wt$Treatment <- c("Control","ML","Vm")
        h2_liab_JU178_P7p_wt$Measure <- c("H2","H2","H2")
        h2_liab_JU178_P7p_wt$Scale <- c("liab","liab","liab")
        h2_liab_JU178_P7p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_JU178_P7p_wt
      }
      
      #Summary Evol_liab_JU178_P7p_wt
      {
        mean_Evol_liab_JU178_P7p_wt <- rbind(mean(Evol_liab_JU178_CONTROL_P7p_wt_mod),mean(Evol_liab_JU178_MA_P7p_wt_mod),mean(Evol_liab_JU178_Vm_P7p_wt_mod))
        colnames(mean_Evol_liab_JU178_P7p_wt) <- c("mean")
        median_Evol_liab_JU178_P7p_wt <- rbind(median(Evol_liab_JU178_CONTROL_P7p_wt_mod),median(Evol_liab_JU178_MA_P7p_wt_mod),median(Evol_liab_JU178_Vm_P7p_wt_mod))
        colnames(median_Evol_liab_JU178_P7p_wt) <- c("median")
        posterior.mode_Evol_liab_JU178_P7p_wt <- rbind(posterior.mode(Evol_liab_JU178_CONTROL_P7p_wt_mod),posterior.mode(Evol_liab_JU178_MA_P7p_wt_mod),posterior.mode(Evol_liab_JU178_Vm_P7p_wt_mod))
        colnames(posterior.mode_Evol_liab_JU178_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_JU178_P7p_wt <- rbind(HPDinterval(Evol_liab_JU178_CONTROL_P7p_wt_mod),HPDinterval(Evol_liab_JU178_MA_P7p_wt_mod),HPDinterval(Evol_liab_JU178_Vm_P7p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_JU178_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_JU178_P7p_wt <- rbind(HPDinterval(Evol_liab_JU178_CONTROL_P7p_wt_mod,prob=.83),HPDinterval(Evol_liab_JU178_MA_P7p_wt_mod,prob=.83),HPDinterval(Evol_liab_JU178_Vm_P7p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_JU178_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_JU178_P7p_wt <- rbind(effectiveSize(Evol_liab_JU178_CONTROL_P7p_wt_mod),effectiveSize(Evol_liab_JU178_MA_P7p_wt_mod),effectiveSize(Evol_liab_JU178_Vm_P7p_wt_mod))
        colnames(effectiveSize_Evol_liab_JU178_P7p_wt) <- c("effectiveSize")
        Evol_liab_JU178_P7p_wt <- cbind.data.frame(mean_Evol_liab_JU178_P7p_wt,median_Evol_liab_JU178_P7p_wt,posterior.mode_Evol_liab_JU178_P7p_wt,HPDinterval_0.95_Evol_liab_JU178_P7p_wt,HPDinterval_0.83_Evol_liab_JU178_P7p_wt,effectiveSize_Evol_liab_JU178_P7p_wt)
        rownames(Evol_liab_JU178_P7p_wt) <- c("Evol_liab_JU178_CONTROL_P7p_wt_mod","Evol_liab_JU178_MA_P7p_wt_mod","Evol_liab_JU178_Vm_P7p_wt_mod")
        Evol_liab_JU178_P7p_wt <- cbind(Models = rownames(Evol_liab_JU178_P7p_wt),Evol_liab_JU178_P7p_wt)
        rownames(Evol_liab_JU178_P7p_wt) <- NULL
        Evol_liab_JU178_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        Evol_liab_JU178_P7p_wt$Treatment <- c("Control","ML","Vm")
        Evol_liab_JU178_P7p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_JU178_P7p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_JU178_P7p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_JU178_P7p_wt
      }
      
      #Summary trait_mean_liab_JU178_P7p_wt
      {
        mean_trait_mean_liab_JU178_P7p_wt <- rbind(mean(trait_mean_liab_JU178_CONTROL_P7p_wt_mod),mean(trait_mean_liab_JU178_MA_P7p_wt_mod),mean(trait_mean_liab_JU178_Vm_P7p_wt_mod))
        colnames(mean_trait_mean_liab_JU178_P7p_wt) <- c("mean")
        median_trait_mean_liab_JU178_P7p_wt <- rbind(median(trait_mean_liab_JU178_CONTROL_P7p_wt_mod),median(trait_mean_liab_JU178_MA_P7p_wt_mod),median(trait_mean_liab_JU178_Vm_P7p_wt_mod))
        colnames(median_trait_mean_liab_JU178_P7p_wt) <- c("median")
        posterior.mode_trait_mean_liab_JU178_P7p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_JU178_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU178_MA_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU178_Vm_P7p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_JU178_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_JU178_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU178_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU178_MA_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU178_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_JU178_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_JU178_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU178_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU178_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU178_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_JU178_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_JU178_P7p_wt <- rbind(effectiveSize(trait_mean_liab_JU178_CONTROL_P7p_wt_mod),effectiveSize(trait_mean_liab_JU178_MA_P7p_wt_mod),effectiveSize(trait_mean_liab_JU178_Vm_P7p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_JU178_P7p_wt) <- c("effectiveSize")
        trait_mean_liab_JU178_P7p_wt <- cbind.data.frame(mean_trait_mean_liab_JU178_P7p_wt,median_trait_mean_liab_JU178_P7p_wt,posterior.mode_trait_mean_liab_JU178_P7p_wt,HPDinterval_0.95_trait_mean_liab_JU178_P7p_wt,HPDinterval_0.83_trait_mean_liab_JU178_P7p_wt,effectiveSize_trait_mean_liab_JU178_P7p_wt)
        rownames(trait_mean_liab_JU178_P7p_wt) <- c("trait_mean_liab_JU178_CONTROL_P7p_wt_mod","trait_mean_liab_JU178_MA_P7p_wt_mod","trait_mean_liab_JU178_Vm_P7p_wt_mod")
        trait_mean_liab_JU178_P7p_wt <- cbind(Models = rownames(trait_mean_liab_JU178_P7p_wt),trait_mean_liab_JU178_P7p_wt)
        rownames(trait_mean_liab_JU178_P7p_wt) <- NULL
        trait_mean_liab_JU178_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        trait_mean_liab_JU178_P7p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_JU178_P7p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_JU178_P7p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_JU178_P7p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_JU178_P7p_wt
      }
      
      liab_JU178_P7p_wt <- rbind.data.frame(va_liab_JU178_P7p_wt, h2_liab_JU178_P7p_wt,Evol_liab_JU178_P7p_wt,trait_mean_liab_JU178_P7p_wt)
      liab_JU178_P7p_wt
    }
    #Summary data scale JU178 P7p
    {
      #Summary va_data_JU178_P7p_wt:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_JU178_P7p_wt <- rbind(mean(va_data_JU178_CONTROL_P7p_wt_mod/2),mean(va_data_JU178_MA_P7p_wt_mod/2),mean(va_data_JU178_Vm_P7p_wt_mod/2))
        colnames(mean_va_data_JU178_P7p_wt) <- c("mean")
        median_va_data_JU178_P7p_wt <- rbind(median(va_data_JU178_CONTROL_P7p_wt_mod/2),median(va_data_JU178_MA_P7p_wt_mod/2),median(va_data_JU178_Vm_P7p_wt_mod/2))
        colnames(median_va_data_JU178_P7p_wt) <- c("median")
        posterior.mode_va_data_JU178_P7p_wt <- rbind(posterior.mode(as.mcmc(va_data_JU178_CONTROL_P7p_wt_mod/2)),posterior.mode(as.mcmc(va_data_JU178_MA_P7p_wt_mod/2)),posterior.mode(as.mcmc(va_data_JU178_Vm_P7p_wt_mod/2)))
        colnames(posterior.mode_va_data_JU178_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_JU178_P7p_wt <- rbind(HPDinterval(as.mcmc(va_data_JU178_CONTROL_P7p_wt_mod/2)),HPDinterval(as.mcmc(va_data_JU178_MA_P7p_wt_mod/2)),HPDinterval(as.mcmc(va_data_JU178_Vm_P7p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_JU178_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_JU178_P7p_wt <- rbind(HPDinterval(as.mcmc(va_data_JU178_CONTROL_P7p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU178_MA_P7p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU178_Vm_P7p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_JU178_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_JU178_P7p_wt <- rbind(effectiveSize(va_data_JU178_CONTROL_P7p_wt_mod/2),effectiveSize(va_data_JU178_MA_P7p_wt_mod/2),effectiveSize(va_data_JU178_Vm_P7p_wt_mod/2))
        colnames(effectiveSize_va_data_JU178_P7p_wt) <- c("effectiveSize")
        va_data_JU178_P7p_wt <- cbind.data.frame(mean_va_data_JU178_P7p_wt,median_va_data_JU178_P7p_wt,posterior.mode_va_data_JU178_P7p_wt,HPDinterval_0.95_va_data_JU178_P7p_wt,HPDinterval_0.83_va_data_JU178_P7p_wt,effectiveSize_va_data_JU178_P7p_wt)
        rownames(va_data_JU178_P7p_wt) <- c("va_data_JU178_CONTROL_P7p_wt_mod","va_data_JU178_MA_P7p_wt_mod","va_data_JU178_Vm_P7p_wt_mod")
        va_data_JU178_P7p_wt <- cbind(Models = rownames(va_data_JU178_P7p_wt),va_data_JU178_P7p_wt)
        rownames(va_data_JU178_P7p_wt) <- NULL
        va_data_JU178_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        va_data_JU178_P7p_wt$Treatment <- c("Control","ML","Vm")
        va_data_JU178_P7p_wt$Measure <- c("Va","Va","Va")
        va_data_JU178_P7p_wt$Scale <- c("data","data","data")
        va_data_JU178_P7p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_JU178_P7p_wt
      }
      
      #Summary h2_data_JU178_P7p_wt
      {
        mean_h2_data_JU178_P7p_wt <- rbind(mean(h2_data_JU178_CONTROL_P7p_wt_mod),mean(h2_data_JU178_MA_P7p_wt_mod),mean(h2_data_JU178_Vm_P7p_wt_mod))
        colnames(mean_h2_data_JU178_P7p_wt) <- c("mean")
        median_h2_data_JU178_P7p_wt <- rbind(median(h2_data_JU178_CONTROL_P7p_wt_mod),median(h2_data_JU178_MA_P7p_wt_mod),median(h2_data_JU178_Vm_P7p_wt_mod))
        colnames(median_h2_data_JU178_P7p_wt) <- c("median")
        posterior.mode_h2_data_JU178_P7p_wt <- rbind(posterior.mode(as.mcmc(h2_data_JU178_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(h2_data_JU178_MA_P7p_wt_mod)),posterior.mode(as.mcmc(h2_data_JU178_Vm_P7p_wt_mod)))
        colnames(posterior.mode_h2_data_JU178_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_JU178_P7p_wt <- rbind(HPDinterval(as.mcmc(h2_data_JU178_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(h2_data_JU178_MA_P7p_wt_mod)),HPDinterval(as.mcmc(h2_data_JU178_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_JU178_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_JU178_P7p_wt <- rbind(HPDinterval(as.mcmc(h2_data_JU178_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU178_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU178_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_JU178_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_JU178_P7p_wt <- rbind(effectiveSize(h2_data_JU178_CONTROL_P7p_wt_mod),effectiveSize(h2_data_JU178_MA_P7p_wt_mod),effectiveSize(h2_data_JU178_Vm_P7p_wt_mod))
        colnames(effectiveSize_h2_data_JU178_P7p_wt) <- c("effectiveSize")
        h2_data_JU178_P7p_wt <- cbind.data.frame(mean_h2_data_JU178_P7p_wt,median_h2_data_JU178_P7p_wt,posterior.mode_h2_data_JU178_P7p_wt,HPDinterval_0.95_h2_data_JU178_P7p_wt,HPDinterval_0.83_h2_data_JU178_P7p_wt,effectiveSize_h2_data_JU178_P7p_wt)
        rownames(h2_data_JU178_P7p_wt) <- c("h2_data_JU178_CONTROL_P7p_wt_mod","h2_data_JU178_MA_P7p_wt_mod","h2_data_JU178_Vm_P7p_wt_mod")
        h2_data_JU178_P7p_wt <- cbind(Models = rownames(h2_data_JU178_P7p_wt),h2_data_JU178_P7p_wt)
        rownames(h2_data_JU178_P7p_wt) <- NULL
        h2_data_JU178_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        h2_data_JU178_P7p_wt$Treatment <- c("Control","ML","Vm")
        h2_data_JU178_P7p_wt$Measure <- c("H2","H2","H2")
        h2_data_JU178_P7p_wt$Scale <- c("data","data","data")
        h2_data_JU178_P7p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_JU178_P7p_wt
      }
      
      #Summary Evol_data_JU178_P7p_wt
      {
        mean_Evol_data_JU178_P7p_wt <- rbind(mean(Evol_data_JU178_CONTROL_P7p_wt_mod),mean(Evol_data_JU178_MA_P7p_wt_mod),mean(Evol_data_JU178_Vm_P7p_wt_mod))
        colnames(mean_Evol_data_JU178_P7p_wt) <- c("mean")
        median_Evol_data_JU178_P7p_wt <- rbind(median(Evol_data_JU178_CONTROL_P7p_wt_mod),median(Evol_data_JU178_MA_P7p_wt_mod),median(Evol_data_JU178_Vm_P7p_wt_mod))
        colnames(median_Evol_data_JU178_P7p_wt) <- c("median")
        posterior.mode_Evol_data_JU178_P7p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_JU178_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(Evol_data_JU178_MA_P7p_wt_mod)),posterior.mode(as.mcmc(Evol_data_JU178_Vm_P7p_wt_mod)))
        colnames(posterior.mode_Evol_data_JU178_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_JU178_P7p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_JU178_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(Evol_data_JU178_MA_P7p_wt_mod)),HPDinterval(as.mcmc(Evol_data_JU178_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_JU178_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_JU178_P7p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_JU178_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU178_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU178_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_JU178_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_JU178_P7p_wt <- rbind(effectiveSize(Evol_data_JU178_CONTROL_P7p_wt_mod),effectiveSize(Evol_data_JU178_MA_P7p_wt_mod),effectiveSize(Evol_data_JU178_Vm_P7p_wt_mod))
        colnames(effectiveSize_Evol_data_JU178_P7p_wt) <- c("effectiveSize")
        Evol_data_JU178_P7p_wt <- cbind.data.frame(mean_Evol_data_JU178_P7p_wt,median_Evol_data_JU178_P7p_wt,posterior.mode_Evol_data_JU178_P7p_wt,HPDinterval_0.95_Evol_data_JU178_P7p_wt,HPDinterval_0.83_Evol_data_JU178_P7p_wt,effectiveSize_Evol_data_JU178_P7p_wt)
        rownames(Evol_data_JU178_P7p_wt) <- c("Evol_data_JU178_CONTROL_P7p_wt_mod","Evol_data_JU178_MA_P7p_wt_mod","Evol_data_JU178_Vm_P7p_wt_mod")
        Evol_data_JU178_P7p_wt <- cbind(Models = rownames(Evol_data_JU178_P7p_wt),Evol_data_JU178_P7p_wt)
        rownames(Evol_data_JU178_P7p_wt) <- NULL
        Evol_data_JU178_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        Evol_data_JU178_P7p_wt$Treatment <- c("Control","ML","Vm")
        Evol_data_JU178_P7p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_JU178_P7p_wt$Scale <- c("data","data","data")
        Evol_data_JU178_P7p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_JU178_P7p_wt
      }
      
      #Summary trait_mean_data_JU178_P7p_wt
      {
        mean_trait_mean_data_JU178_P7p_wt <- rbind(mean(trait_mean_data_JU178_CONTROL_P7p_wt_mod),mean(trait_mean_data_JU178_MA_P7p_wt_mod),mean(trait_mean_data_JU178_Vm_P7p_wt_mod))
        colnames(mean_trait_mean_data_JU178_P7p_wt) <- c("mean")
        median_trait_mean_data_JU178_P7p_wt <- rbind(median(trait_mean_data_JU178_CONTROL_P7p_wt_mod),median(trait_mean_data_JU178_MA_P7p_wt_mod),median(trait_mean_data_JU178_Vm_P7p_wt_mod))
        colnames(median_trait_mean_data_JU178_P7p_wt) <- c("median")
        posterior.mode_trait_mean_data_JU178_P7p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_JU178_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_JU178_MA_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_JU178_Vm_P7p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_JU178_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_JU178_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU178_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_JU178_MA_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_JU178_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_JU178_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_JU178_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU178_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU178_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU178_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_JU178_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_JU178_P7p_wt <- rbind(effectiveSize(trait_mean_data_JU178_CONTROL_P7p_wt_mod),effectiveSize(trait_mean_data_JU178_MA_P7p_wt_mod),effectiveSize(trait_mean_data_JU178_Vm_P7p_wt_mod))
        colnames(effectiveSize_trait_mean_data_JU178_P7p_wt) <- c("effectiveSize")
        trait_mean_data_JU178_P7p_wt <- cbind.data.frame(mean_trait_mean_data_JU178_P7p_wt,median_trait_mean_data_JU178_P7p_wt,posterior.mode_trait_mean_data_JU178_P7p_wt,HPDinterval_0.95_trait_mean_data_JU178_P7p_wt,HPDinterval_0.83_trait_mean_data_JU178_P7p_wt,effectiveSize_trait_mean_data_JU178_P7p_wt)
        rownames(trait_mean_data_JU178_P7p_wt) <- c("trait_mean_data_JU178_CONTROL_P7p_wt_mod","trait_mean_data_JU178_MA_P7p_wt_mod","trait_mean_data_JU178_Vm_P7p_wt_mod")
        trait_mean_data_JU178_P7p_wt <- cbind(Models = rownames(trait_mean_data_JU178_P7p_wt),trait_mean_data_JU178_P7p_wt)
        rownames(trait_mean_data_JU178_P7p_wt) <- NULL
        trait_mean_data_JU178_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        trait_mean_data_JU178_P7p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_data_JU178_P7p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_JU178_P7p_wt$Scale <- c("data","data","data")
        trait_mean_data_JU178_P7p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_JU178_P7p_wt
      }
      
      data_JU178_P7p_wt <- rbind.data.frame(va_data_JU178_P7p_wt, h2_data_JU178_P7p_wt,Evol_data_JU178_P7p_wt,trait_mean_data_JU178_P7p_wt)
      data_JU178_P7p_wt
      
    }
    Vm_JU178_P7p_wt <- rbind.data.frame(liab_JU178_P7p_wt, data_JU178_P7p_wt)
    Vm_JU178_P7p_wt$Pnp_fate <- rep("wt", 24)
    Vm_JU178_P7p_wt
    #remove JU178 P7p_wt models
    {
      remove(JU178_CONTROL_P7p_wt_mod)
      remove(JU178_MA_P7p_wt_mod)
      remove(JU178_Vm_P7p_wt_mod)
    }
  }
  
  Vm_JU178_SSSS_Summary <- rbind.data.frame(Vm_JU178_P3p_SSSS,Vm_JU178_P4p_SSSS,Vm_JU178_P5p_wt,Vm_JU178_P6p_wt,Vm_JU178_P7p_wt,Vm_JU178_P8p_SSSS)
  Vm_JU178_SSSS_Summary$Ancestral <- rep("JU178",144)
  Vm_JU178_SSSS_Summary$Species <- rep("O.tipulae",144)
  Vm_JU178_SSSS_Summary$Genus <- rep("Oscheius",144)
  View(Vm_JU178_SSSS_Summary)
  
  ##Vm_JU178_P3p_divided_P4p_SSSS----
  {
    #Vm_JU178_P3p_divided_P4p_SSSS_liab
    {
      
      va_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod <- va_liab_JU178_Vm_P3p_SSSS_mod / va_liab_JU178_Vm_P4p_SSSS_mod
      h2_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod <- h2_liab_JU178_Vm_P3p_SSSS_mod / h2_liab_JU178_Vm_P4p_SSSS_mod
      Evol_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod <- Evol_liab_JU178_Vm_P3p_SSSS_mod / Evol_liab_JU178_Vm_P4p_SSSS_mod
      
      mean_va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS <- rbind(mean(log10(va_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod)),mean(log10(h2_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod)), mean(log10(Evol_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod)))
      colnames(mean_va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS) <- c("mean")
      median_va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS <- rbind(median(log10(va_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod)),median(log10(h2_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod)), median(log10(Evol_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod)))
      colnames(median_va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS) <- c("median")
      posterior.mode_va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS <- rbind(posterior.mode(as.mcmc(log10(va_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod))),posterior.mode(as.mcmc(log10(h2_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod))),posterior.mode(as.mcmc(log10(Evol_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod))))
      colnames(posterior.mode_va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS) <- c("posterior.mode")
      HPDinterval_0.95_va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod))),HPDinterval(as.mcmc(log10(h2_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod))),HPDinterval(as.mcmc(log10(Evol_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod))))
      colnames(HPDinterval_0.95_va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS) <- c("CI_lower_0.95","CI_upper_0.95")
      HPDinterval_0.83_va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83),HPDinterval(as.mcmc(log10(h2_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83),HPDinterval(as.mcmc(log10(Evol_liab_JU178_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83))
      colnames(HPDinterval_0.83_va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS) <- c("CI_lower_0.83","CI_upper_0.83")
      va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS <- cbind.data.frame(mean_va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS,median_va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS,posterior.mode_va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS,HPDinterval_0.95_va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS,HPDinterval_0.83_va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS)
      rownames(va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS) <- c("va_liab_JU178_Vm_P3p_divided_P4p_SSSS_log10","h2_liab_JU178_Vm_P3p_divided_P4p_SSSS_log10","Evol_liab_JU178_Vm_P3p_divided_P4p_SSSS_log10")
      va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS$Measure <- c("Va","H2", "Evol")
      va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS$Ancestral <- rep("JU178",3)
      va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS$Species <- rep("O.tipulae",3)
      va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS$Genus <- rep("Oscheius",3)
      va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS$Scale <- rep("liab",3)
      va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS
      
      pdf("Vm_va_h2_Evol_liab_P3p_divided_P4p_SSSS_log10_JU178.pdf")
      ggplot(va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS, aes(x=Measure, y= median)) +
        geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
        geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
        geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
        theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
        labs(y = "Va log10 ( P3.p / P4.p)", title = "JU178_log10(P3p/P4p)_liab")
      dev.off() 
      
    }
    
    #Vm_JU178_P3p_divided_P4p_SSSS_data
    
    {
      va_data_JU178_Vm_P3p_divided_P4p_SSSS_mod <- va_data_JU178_Vm_P3p_SSSS_mod / va_data_JU178_Vm_P4p_SSSS_mod
      h2_data_JU178_Vm_P3p_divided_P4p_SSSS_mod <- h2_data_JU178_Vm_P3p_SSSS_mod / h2_data_JU178_Vm_P4p_SSSS_mod
      Evol_data_JU178_Vm_P3p_divided_P4p_SSSS_mod <- Evol_data_JU178_Vm_P3p_SSSS_mod / Evol_data_JU178_Vm_P4p_SSSS_mod
      
      mean_va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS <- rbind(mean(log10(va_data_JU178_Vm_P3p_divided_P4p_SSSS_mod)),mean(log10(h2_data_JU178_Vm_P3p_divided_P4p_SSSS_mod)), mean(log10(Evol_data_JU178_Vm_P3p_divided_P4p_SSSS_mod)))
      colnames(mean_va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS) <- c("mean")
      median_va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS <- rbind(median(log10(va_data_JU178_Vm_P3p_divided_P4p_SSSS_mod)),median(log10(h2_data_JU178_Vm_P3p_divided_P4p_SSSS_mod)), median(log10(Evol_data_JU178_Vm_P3p_divided_P4p_SSSS_mod)))
      colnames(median_va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS) <- c("median")
      posterior.mode_va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS <- rbind(posterior.mode(as.mcmc(log10(va_data_JU178_Vm_P3p_divided_P4p_SSSS_mod))),posterior.mode(as.mcmc(log10(h2_data_JU178_Vm_P3p_divided_P4p_SSSS_mod))),posterior.mode(as.mcmc(log10(Evol_data_JU178_Vm_P3p_divided_P4p_SSSS_mod))))
      colnames(posterior.mode_va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS) <- c("posterior.mode")
      HPDinterval_0.95_va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_data_JU178_Vm_P3p_divided_P4p_SSSS_mod))),HPDinterval(as.mcmc(log10(h2_data_JU178_Vm_P3p_divided_P4p_SSSS_mod))),HPDinterval(as.mcmc(log10(Evol_data_JU178_Vm_P3p_divided_P4p_SSSS_mod))))
      colnames(HPDinterval_0.95_va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS) <- c("CI_lower_0.95","CI_upper_0.95")
      HPDinterval_0.83_va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_data_JU178_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83),HPDinterval(as.mcmc(log10(h2_data_JU178_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83),HPDinterval(as.mcmc(log10(Evol_data_JU178_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83))
      colnames(HPDinterval_0.83_va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS) <- c("CI_lower_0.83","CI_upper_0.83")
      va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS <- cbind.data.frame(mean_va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS,median_va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS,posterior.mode_va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS,HPDinterval_0.95_va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS,HPDinterval_0.83_va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS)
      rownames(va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS) <- c("va_data_JU178_Vm_P3p_divided_P4p_SSSS_log10","h2_data_JU178_Vm_P3p_divided_P4p_SSSS_log10","Evol_data_JU178_Vm_P3p_divided_P4p_SSSS_log10")
      va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS$Measure <- c("Va","H2", "Evol")
      va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS$Ancestral <- rep("JU178",3)
      va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS$Species <- rep("O.tipulae",3)
      va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS$Genus <- rep("Oscheius",3)
      va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS$Scale <- rep("data",3)
      va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS
      
      pdf("Vm_va_h2_Evol_data_P3p_divided_P4p_SSSS_log10_JU178.pdf")
      ggplot(va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS, aes(x=Measure, y= median)) +
        geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
        geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
        geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
        theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
        labs(y = "Va log10 ( P3.p / P4.p)", title = "JU178_log10(P3p/P4p)_data")
      dev.off() 
      
    }
    
    va_h2_Evol_Vm_JU178_P3p_divided_P4p_SSSS_Summary <- rbind.data.frame(va_h2_Evol_liab_Vm_JU178_P3p_divided_P4p_SSSS,va_h2_Evol_data_Vm_JU178_P3p_divided_P4p_SSSS)
    va_h2_Evol_Vm_JU178_P3p_divided_P4p_SSSS_Summary
    
  }
  
  
}

#---- PS2068 ----
{
  ##Summary PS2068 P3p----
  {
    #Summary liability scale PS2068 P3p
    {
      #Summary va_liab_PS2068_P3p_SSSS: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_PS2068_P3p_SSSS <- rbind(mean(va_liab_PS2068_CONTROL_P3p_SSSS_mod/2),mean(va_liab_PS2068_MA_P3p_SSSS_mod/2),mean(va_liab_PS2068_Vm_P3p_SSSS_mod/2))
        colnames(mean_va_liab_PS2068_P3p_SSSS) <- c("mean")
        median_va_liab_PS2068_P3p_SSSS <- rbind(median(va_liab_PS2068_CONTROL_P3p_SSSS_mod/2),median(va_liab_PS2068_MA_P3p_SSSS_mod/2),median(va_liab_PS2068_Vm_P3p_SSSS_mod/2))
        colnames(median_va_liab_PS2068_P3p_SSSS) <- c("median")
        posterior.mode_va_liab_PS2068_P3p_SSSS <- rbind(posterior.mode(va_liab_PS2068_CONTROL_P3p_SSSS_mod/2),posterior.mode(va_liab_PS2068_MA_P3p_SSSS_mod/2),posterior.mode(va_liab_PS2068_Vm_P3p_SSSS_mod/2))
        colnames(posterior.mode_va_liab_PS2068_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_PS2068_P3p_SSSS <- rbind(HPDinterval(va_liab_PS2068_CONTROL_P3p_SSSS_mod/2),HPDinterval(va_liab_PS2068_MA_P3p_SSSS_mod/2),HPDinterval(va_liab_PS2068_Vm_P3p_SSSS_mod/2))
        colnames(HPDinterval_0.95_va_liab_PS2068_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_PS2068_P3p_SSSS <- rbind(HPDinterval(va_liab_PS2068_CONTROL_P3p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_PS2068_MA_P3p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_PS2068_Vm_P3p_SSSS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_PS2068_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_PS2068_P3p_SSSS <- rbind(effectiveSize(va_liab_PS2068_CONTROL_P3p_SSSS_mod/2),effectiveSize(va_liab_PS2068_MA_P3p_SSSS_mod/2),effectiveSize(va_liab_PS2068_Vm_P3p_SSSS_mod/2))
        colnames(effectiveSize_va_liab_PS2068_P3p_SSSS) <- c("effectiveSize")
        va_liab_PS2068_P3p_SSSS <- cbind.data.frame(mean_va_liab_PS2068_P3p_SSSS,median_va_liab_PS2068_P3p_SSSS,posterior.mode_va_liab_PS2068_P3p_SSSS,HPDinterval_0.95_va_liab_PS2068_P3p_SSSS,HPDinterval_0.83_va_liab_PS2068_P3p_SSSS,effectiveSize_va_liab_PS2068_P3p_SSSS)
        rownames(va_liab_PS2068_P3p_SSSS) <- c("va_liab_PS2068_CONTROL_P3p_SSSS_mod","va_liab_PS2068_MA_P3p_SSSS_mod","va_liab_PS2068_Vm_P3p_SSSS_mod")
        va_liab_PS2068_P3p_SSSS <- cbind(Models = rownames(va_liab_PS2068_P3p_SSSS),va_liab_PS2068_P3p_SSSS)
        rownames(va_liab_PS2068_P3p_SSSS) <- NULL
        va_liab_PS2068_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        va_liab_PS2068_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        va_liab_PS2068_P3p_SSSS$Measure <- c("Va","Va","Va")
        va_liab_PS2068_P3p_SSSS$Scale <- c("liab","liab","liab")
        va_liab_PS2068_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_liab_PS2068_P3p_SSSS
      }
      
      #Summary h2_liab_PS2068_P3p_SSSS
      {
        mean_h2_liab_PS2068_P3p_SSSS <- rbind(mean(h2_liab_PS2068_CONTROL_P3p_SSSS_mod),mean(h2_liab_PS2068_MA_P3p_SSSS_mod),mean(h2_liab_PS2068_Vm_P3p_SSSS_mod))
        colnames(mean_h2_liab_PS2068_P3p_SSSS) <- c("mean")
        median_h2_liab_PS2068_P3p_SSSS <- rbind(median(h2_liab_PS2068_CONTROL_P3p_SSSS_mod),median(h2_liab_PS2068_MA_P3p_SSSS_mod),median(h2_liab_PS2068_Vm_P3p_SSSS_mod))
        colnames(median_h2_liab_PS2068_P3p_SSSS) <- c("median")
        posterior.mode_h2_liab_PS2068_P3p_SSSS <- rbind(posterior.mode(h2_liab_PS2068_CONTROL_P3p_SSSS_mod),posterior.mode(h2_liab_PS2068_MA_P3p_SSSS_mod),posterior.mode(h2_liab_PS2068_Vm_P3p_SSSS_mod))
        colnames(posterior.mode_h2_liab_PS2068_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_PS2068_P3p_SSSS <- rbind(HPDinterval(h2_liab_PS2068_CONTROL_P3p_SSSS_mod),HPDinterval(h2_liab_PS2068_MA_P3p_SSSS_mod),HPDinterval(h2_liab_PS2068_Vm_P3p_SSSS_mod))
        colnames(HPDinterval_0.95_h2_liab_PS2068_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_PS2068_P3p_SSSS <- rbind(HPDinterval(h2_liab_PS2068_CONTROL_P3p_SSSS_mod,prob=.83),HPDinterval(h2_liab_PS2068_MA_P3p_SSSS_mod,prob=.83),HPDinterval(h2_liab_PS2068_Vm_P3p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_PS2068_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_PS2068_P3p_SSSS <- rbind(effectiveSize(h2_liab_PS2068_CONTROL_P3p_SSSS_mod),effectiveSize(h2_liab_PS2068_MA_P3p_SSSS_mod),effectiveSize(h2_liab_PS2068_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_h2_liab_PS2068_P3p_SSSS) <- c("effectiveSize")
        h2_liab_PS2068_P3p_SSSS <- cbind.data.frame(mean_h2_liab_PS2068_P3p_SSSS,median_h2_liab_PS2068_P3p_SSSS,posterior.mode_h2_liab_PS2068_P3p_SSSS,HPDinterval_0.95_h2_liab_PS2068_P3p_SSSS,HPDinterval_0.83_h2_liab_PS2068_P3p_SSSS,effectiveSize_h2_liab_PS2068_P3p_SSSS)
        rownames(h2_liab_PS2068_P3p_SSSS) <- c("h2_liab_PS2068_CONTROL_P3p_SSSS_mod","h2_liab_PS2068_MA_P3p_SSSS_mod","h2_liab_PS2068_Vm_P3p_SSSS_mod")
        h2_liab_PS2068_P3p_SSSS <- cbind(Models = rownames(h2_liab_PS2068_P3p_SSSS),h2_liab_PS2068_P3p_SSSS)
        rownames(h2_liab_PS2068_P3p_SSSS) <- NULL
        h2_liab_PS2068_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        h2_liab_PS2068_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_liab_PS2068_P3p_SSSS$Measure <- c("H2","H2","H2")
        h2_liab_PS2068_P3p_SSSS$Scale <- c("liab","liab","liab")
        h2_liab_PS2068_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_liab_PS2068_P3p_SSSS
      }
      
      #Summary Evol_liab_PS2068_P3p_SSSS
      {
        mean_Evol_liab_PS2068_P3p_SSSS <- rbind(mean(Evol_liab_PS2068_CONTROL_P3p_SSSS_mod),mean(Evol_liab_PS2068_MA_P3p_SSSS_mod),mean(Evol_liab_PS2068_Vm_P3p_SSSS_mod))
        colnames(mean_Evol_liab_PS2068_P3p_SSSS) <- c("mean")
        median_Evol_liab_PS2068_P3p_SSSS <- rbind(median(Evol_liab_PS2068_CONTROL_P3p_SSSS_mod),median(Evol_liab_PS2068_MA_P3p_SSSS_mod),median(Evol_liab_PS2068_Vm_P3p_SSSS_mod))
        colnames(median_Evol_liab_PS2068_P3p_SSSS) <- c("median")
        posterior.mode_Evol_liab_PS2068_P3p_SSSS <- rbind(posterior.mode(Evol_liab_PS2068_CONTROL_P3p_SSSS_mod),posterior.mode(Evol_liab_PS2068_MA_P3p_SSSS_mod),posterior.mode(Evol_liab_PS2068_Vm_P3p_SSSS_mod))
        colnames(posterior.mode_Evol_liab_PS2068_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_PS2068_P3p_SSSS <- rbind(HPDinterval(Evol_liab_PS2068_CONTROL_P3p_SSSS_mod),HPDinterval(Evol_liab_PS2068_MA_P3p_SSSS_mod),HPDinterval(Evol_liab_PS2068_Vm_P3p_SSSS_mod))
        colnames(HPDinterval_0.95_Evol_liab_PS2068_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_PS2068_P3p_SSSS <- rbind(HPDinterval(Evol_liab_PS2068_CONTROL_P3p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_PS2068_MA_P3p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_PS2068_Vm_P3p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_PS2068_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_PS2068_P3p_SSSS <- rbind(effectiveSize(Evol_liab_PS2068_CONTROL_P3p_SSSS_mod),effectiveSize(Evol_liab_PS2068_MA_P3p_SSSS_mod),effectiveSize(Evol_liab_PS2068_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_Evol_liab_PS2068_P3p_SSSS) <- c("effectiveSize")
        Evol_liab_PS2068_P3p_SSSS <- cbind.data.frame(mean_Evol_liab_PS2068_P3p_SSSS,median_Evol_liab_PS2068_P3p_SSSS,posterior.mode_Evol_liab_PS2068_P3p_SSSS,HPDinterval_0.95_Evol_liab_PS2068_P3p_SSSS,HPDinterval_0.83_Evol_liab_PS2068_P3p_SSSS,effectiveSize_Evol_liab_PS2068_P3p_SSSS)
        rownames(Evol_liab_PS2068_P3p_SSSS) <- c("Evol_liab_PS2068_CONTROL_P3p_SSSS_mod","Evol_liab_PS2068_MA_P3p_SSSS_mod","Evol_liab_PS2068_Vm_P3p_SSSS_mod")
        Evol_liab_PS2068_P3p_SSSS <- cbind(Models = rownames(Evol_liab_PS2068_P3p_SSSS),Evol_liab_PS2068_P3p_SSSS)
        rownames(Evol_liab_PS2068_P3p_SSSS) <- NULL
        Evol_liab_PS2068_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        Evol_liab_PS2068_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_liab_PS2068_P3p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_PS2068_P3p_SSSS$Scale <- c("liab","liab","liab")
        Evol_liab_PS2068_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_PS2068_P3p_SSSS
      }
      
      #Summary trait_mean_liab_PS2068_P3p_SSSS
      {
        mean_trait_mean_liab_PS2068_P3p_SSSS <- rbind(mean(trait_mean_liab_PS2068_CONTROL_P3p_SSSS_mod),mean(trait_mean_liab_PS2068_MA_P3p_SSSS_mod),mean(trait_mean_liab_PS2068_Vm_P3p_SSSS_mod))
        colnames(mean_trait_mean_liab_PS2068_P3p_SSSS) <- c("mean")
        median_trait_mean_liab_PS2068_P3p_SSSS <- rbind(median(trait_mean_liab_PS2068_CONTROL_P3p_SSSS_mod),median(trait_mean_liab_PS2068_MA_P3p_SSSS_mod),median(trait_mean_liab_PS2068_Vm_P3p_SSSS_mod))
        colnames(median_trait_mean_liab_PS2068_P3p_SSSS) <- c("median")
        posterior.mode_trait_mean_liab_PS2068_P3p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_PS2068_CONTROL_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_PS2068_MA_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_PS2068_Vm_P3p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_liab_PS2068_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_PS2068_P3p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_PS2068_CONTROL_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_PS2068_MA_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_PS2068_Vm_P3p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_PS2068_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_PS2068_P3p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_PS2068_CONTROL_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_PS2068_MA_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_PS2068_Vm_P3p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_PS2068_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_PS2068_P3p_SSSS <- rbind(effectiveSize(trait_mean_liab_PS2068_CONTROL_P3p_SSSS_mod),effectiveSize(trait_mean_liab_PS2068_MA_P3p_SSSS_mod),effectiveSize(trait_mean_liab_PS2068_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_trait_mean_liab_PS2068_P3p_SSSS) <- c("effectiveSize")
        trait_mean_liab_PS2068_P3p_SSSS <- cbind.data.frame(mean_trait_mean_liab_PS2068_P3p_SSSS,median_trait_mean_liab_PS2068_P3p_SSSS,posterior.mode_trait_mean_liab_PS2068_P3p_SSSS,HPDinterval_0.95_trait_mean_liab_PS2068_P3p_SSSS,HPDinterval_0.83_trait_mean_liab_PS2068_P3p_SSSS,effectiveSize_trait_mean_liab_PS2068_P3p_SSSS)
        rownames(trait_mean_liab_PS2068_P3p_SSSS) <- c("trait_mean_liab_PS2068_CONTROL_P3p_SSSS_mod","trait_mean_liab_PS2068_MA_P3p_SSSS_mod","trait_mean_liab_PS2068_Vm_P3p_SSSS_mod")
        trait_mean_liab_PS2068_P3p_SSSS <- cbind(Models = rownames(trait_mean_liab_PS2068_P3p_SSSS),trait_mean_liab_PS2068_P3p_SSSS)
        rownames(trait_mean_liab_PS2068_P3p_SSSS) <- NULL
        trait_mean_liab_PS2068_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        trait_mean_liab_PS2068_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_PS2068_P3p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_PS2068_P3p_SSSS$Scale <- c("liab","liab","liab")
        trait_mean_liab_PS2068_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_PS2068_P3p_SSSS
      }
      
      liab_PS2068_P3p_SSSS <- rbind.data.frame(va_liab_PS2068_P3p_SSSS, h2_liab_PS2068_P3p_SSSS,Evol_liab_PS2068_P3p_SSSS,trait_mean_liab_PS2068_P3p_SSSS)
      liab_PS2068_P3p_SSSS
    }
    #Summary data scale PS2068 P3p
    {
      #Summary va_data_PS2068_P3p_SSSS:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_PS2068_P3p_SSSS <- rbind(mean(va_data_PS2068_CONTROL_P3p_SSSS_mod/2),mean(va_data_PS2068_MA_P3p_SSSS_mod/2),mean(va_data_PS2068_Vm_P3p_SSSS_mod/2))
        colnames(mean_va_data_PS2068_P3p_SSSS) <- c("mean")
        median_va_data_PS2068_P3p_SSSS <- rbind(median(va_data_PS2068_CONTROL_P3p_SSSS_mod/2),median(va_data_PS2068_MA_P3p_SSSS_mod/2),median(va_data_PS2068_Vm_P3p_SSSS_mod/2))
        colnames(median_va_data_PS2068_P3p_SSSS) <- c("median")
        posterior.mode_va_data_PS2068_P3p_SSSS <- rbind(posterior.mode(as.mcmc(va_data_PS2068_CONTROL_P3p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_PS2068_MA_P3p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_PS2068_Vm_P3p_SSSS_mod/2)))
        colnames(posterior.mode_va_data_PS2068_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_PS2068_P3p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_PS2068_CONTROL_P3p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_PS2068_MA_P3p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_PS2068_Vm_P3p_SSSS_mod/2)))
        colnames(HPDinterval_0.95_va_data_PS2068_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_PS2068_P3p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_PS2068_CONTROL_P3p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_PS2068_MA_P3p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_PS2068_Vm_P3p_SSSS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_PS2068_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_PS2068_P3p_SSSS <- rbind(effectiveSize(va_data_PS2068_CONTROL_P3p_SSSS_mod/2),effectiveSize(va_data_PS2068_MA_P3p_SSSS_mod/2),effectiveSize(va_data_PS2068_Vm_P3p_SSSS_mod/2))
        colnames(effectiveSize_va_data_PS2068_P3p_SSSS) <- c("effectiveSize")
        va_data_PS2068_P3p_SSSS <- cbind.data.frame(mean_va_data_PS2068_P3p_SSSS,median_va_data_PS2068_P3p_SSSS,posterior.mode_va_data_PS2068_P3p_SSSS,HPDinterval_0.95_va_data_PS2068_P3p_SSSS,HPDinterval_0.83_va_data_PS2068_P3p_SSSS,effectiveSize_va_data_PS2068_P3p_SSSS)
        rownames(va_data_PS2068_P3p_SSSS) <- c("va_data_PS2068_CONTROL_P3p_SSSS_mod","va_data_PS2068_MA_P3p_SSSS_mod","va_data_PS2068_Vm_P3p_SSSS_mod")
        va_data_PS2068_P3p_SSSS <- cbind(Models = rownames(va_data_PS2068_P3p_SSSS),va_data_PS2068_P3p_SSSS)
        rownames(va_data_PS2068_P3p_SSSS) <- NULL
        va_data_PS2068_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        va_data_PS2068_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        va_data_PS2068_P3p_SSSS$Measure <- c("Va","Va","Va")
        va_data_PS2068_P3p_SSSS$Scale <- c("data","data","data")
        va_data_PS2068_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_data_PS2068_P3p_SSSS
      }
      
      #Summary h2_data_PS2068_P3p_SSSS
      {
        mean_h2_data_PS2068_P3p_SSSS <- rbind(mean(h2_data_PS2068_CONTROL_P3p_SSSS_mod),mean(h2_data_PS2068_MA_P3p_SSSS_mod),mean(h2_data_PS2068_Vm_P3p_SSSS_mod))
        colnames(mean_h2_data_PS2068_P3p_SSSS) <- c("mean")
        median_h2_data_PS2068_P3p_SSSS <- rbind(median(h2_data_PS2068_CONTROL_P3p_SSSS_mod),median(h2_data_PS2068_MA_P3p_SSSS_mod),median(h2_data_PS2068_Vm_P3p_SSSS_mod))
        colnames(median_h2_data_PS2068_P3p_SSSS) <- c("median")
        posterior.mode_h2_data_PS2068_P3p_SSSS <- rbind(posterior.mode(as.mcmc(h2_data_PS2068_CONTROL_P3p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_PS2068_MA_P3p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_PS2068_Vm_P3p_SSSS_mod)))
        colnames(posterior.mode_h2_data_PS2068_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_PS2068_P3p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_PS2068_CONTROL_P3p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_PS2068_MA_P3p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_PS2068_Vm_P3p_SSSS_mod)))
        colnames(HPDinterval_0.95_h2_data_PS2068_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_PS2068_P3p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_PS2068_CONTROL_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_PS2068_MA_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_PS2068_Vm_P3p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_PS2068_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_PS2068_P3p_SSSS <- rbind(effectiveSize(h2_data_PS2068_CONTROL_P3p_SSSS_mod),effectiveSize(h2_data_PS2068_MA_P3p_SSSS_mod),effectiveSize(h2_data_PS2068_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_h2_data_PS2068_P3p_SSSS) <- c("effectiveSize")
        h2_data_PS2068_P3p_SSSS <- cbind.data.frame(mean_h2_data_PS2068_P3p_SSSS,median_h2_data_PS2068_P3p_SSSS,posterior.mode_h2_data_PS2068_P3p_SSSS,HPDinterval_0.95_h2_data_PS2068_P3p_SSSS,HPDinterval_0.83_h2_data_PS2068_P3p_SSSS,effectiveSize_h2_data_PS2068_P3p_SSSS)
        rownames(h2_data_PS2068_P3p_SSSS) <- c("h2_data_PS2068_CONTROL_P3p_SSSS_mod","h2_data_PS2068_MA_P3p_SSSS_mod","h2_data_PS2068_Vm_P3p_SSSS_mod")
        h2_data_PS2068_P3p_SSSS <- cbind(Models = rownames(h2_data_PS2068_P3p_SSSS),h2_data_PS2068_P3p_SSSS)
        rownames(h2_data_PS2068_P3p_SSSS) <- NULL
        h2_data_PS2068_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        h2_data_PS2068_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_data_PS2068_P3p_SSSS$Measure <- c("H2","H2","H2")
        h2_data_PS2068_P3p_SSSS$Scale <- c("data","data","data")
        h2_data_PS2068_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_data_PS2068_P3p_SSSS
      }
      
      #Summary Evol_data_PS2068_P3p_SSSS
      {
        mean_Evol_data_PS2068_P3p_SSSS <- rbind(mean(Evol_data_PS2068_CONTROL_P3p_SSSS_mod),mean(Evol_data_PS2068_MA_P3p_SSSS_mod),mean(Evol_data_PS2068_Vm_P3p_SSSS_mod))
        colnames(mean_Evol_data_PS2068_P3p_SSSS) <- c("mean")
        median_Evol_data_PS2068_P3p_SSSS <- rbind(median(Evol_data_PS2068_CONTROL_P3p_SSSS_mod),median(Evol_data_PS2068_MA_P3p_SSSS_mod),median(Evol_data_PS2068_Vm_P3p_SSSS_mod))
        colnames(median_Evol_data_PS2068_P3p_SSSS) <- c("median")
        posterior.mode_Evol_data_PS2068_P3p_SSSS <- rbind(posterior.mode(as.mcmc(Evol_data_PS2068_CONTROL_P3p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_PS2068_MA_P3p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_PS2068_Vm_P3p_SSSS_mod)))
        colnames(posterior.mode_Evol_data_PS2068_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_PS2068_P3p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_PS2068_CONTROL_P3p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_PS2068_MA_P3p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_PS2068_Vm_P3p_SSSS_mod)))
        colnames(HPDinterval_0.95_Evol_data_PS2068_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_PS2068_P3p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_PS2068_CONTROL_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_PS2068_MA_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_PS2068_Vm_P3p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_PS2068_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_PS2068_P3p_SSSS <- rbind(effectiveSize(Evol_data_PS2068_CONTROL_P3p_SSSS_mod),effectiveSize(Evol_data_PS2068_MA_P3p_SSSS_mod),effectiveSize(Evol_data_PS2068_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_Evol_data_PS2068_P3p_SSSS) <- c("effectiveSize")
        Evol_data_PS2068_P3p_SSSS <- cbind.data.frame(mean_Evol_data_PS2068_P3p_SSSS,median_Evol_data_PS2068_P3p_SSSS,posterior.mode_Evol_data_PS2068_P3p_SSSS,HPDinterval_0.95_Evol_data_PS2068_P3p_SSSS,HPDinterval_0.83_Evol_data_PS2068_P3p_SSSS,effectiveSize_Evol_data_PS2068_P3p_SSSS)
        rownames(Evol_data_PS2068_P3p_SSSS) <- c("Evol_data_PS2068_CONTROL_P3p_SSSS_mod","Evol_data_PS2068_MA_P3p_SSSS_mod","Evol_data_PS2068_Vm_P3p_SSSS_mod")
        Evol_data_PS2068_P3p_SSSS <- cbind(Models = rownames(Evol_data_PS2068_P3p_SSSS),Evol_data_PS2068_P3p_SSSS)
        rownames(Evol_data_PS2068_P3p_SSSS) <- NULL
        Evol_data_PS2068_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        Evol_data_PS2068_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_data_PS2068_P3p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_data_PS2068_P3p_SSSS$Scale <- c("data","data","data")
        Evol_data_PS2068_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_data_PS2068_P3p_SSSS
      }
      
      #Summary trait_mean_data_PS2068_P3p_SSSS
      {
        mean_trait_mean_data_PS2068_P3p_SSSS <- rbind(mean(trait_mean_data_PS2068_CONTROL_P3p_SSSS_mod),mean(trait_mean_data_PS2068_MA_P3p_SSSS_mod),mean(trait_mean_data_PS2068_Vm_P3p_SSSS_mod))
        colnames(mean_trait_mean_data_PS2068_P3p_SSSS) <- c("mean")
        median_trait_mean_data_PS2068_P3p_SSSS <- rbind(median(trait_mean_data_PS2068_CONTROL_P3p_SSSS_mod),median(trait_mean_data_PS2068_MA_P3p_SSSS_mod),median(trait_mean_data_PS2068_Vm_P3p_SSSS_mod))
        colnames(median_trait_mean_data_PS2068_P3p_SSSS) <- c("median")
        posterior.mode_trait_mean_data_PS2068_P3p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_data_PS2068_CONTROL_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_PS2068_MA_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_PS2068_Vm_P3p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_data_PS2068_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_PS2068_P3p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_PS2068_CONTROL_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_PS2068_MA_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_PS2068_Vm_P3p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_PS2068_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_PS2068_P3p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_PS2068_CONTROL_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_PS2068_MA_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_PS2068_Vm_P3p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_PS2068_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_PS2068_P3p_SSSS <- rbind(effectiveSize(trait_mean_data_PS2068_CONTROL_P3p_SSSS_mod),effectiveSize(trait_mean_data_PS2068_MA_P3p_SSSS_mod),effectiveSize(trait_mean_data_PS2068_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_trait_mean_data_PS2068_P3p_SSSS) <- c("effectiveSize")
        trait_mean_data_PS2068_P3p_SSSS <- cbind.data.frame(mean_trait_mean_data_PS2068_P3p_SSSS,median_trait_mean_data_PS2068_P3p_SSSS,posterior.mode_trait_mean_data_PS2068_P3p_SSSS,HPDinterval_0.95_trait_mean_data_PS2068_P3p_SSSS,HPDinterval_0.83_trait_mean_data_PS2068_P3p_SSSS,effectiveSize_trait_mean_data_PS2068_P3p_SSSS)
        rownames(trait_mean_data_PS2068_P3p_SSSS) <- c("trait_mean_data_PS2068_CONTROL_P3p_SSSS_mod","trait_mean_data_PS2068_MA_P3p_SSSS_mod","trait_mean_data_PS2068_Vm_P3p_SSSS_mod")
        trait_mean_data_PS2068_P3p_SSSS <- cbind(Models = rownames(trait_mean_data_PS2068_P3p_SSSS),trait_mean_data_PS2068_P3p_SSSS)
        rownames(trait_mean_data_PS2068_P3p_SSSS) <- NULL
        trait_mean_data_PS2068_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        trait_mean_data_PS2068_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_data_PS2068_P3p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_PS2068_P3p_SSSS$Scale <- c("data","data","data")
        trait_mean_data_PS2068_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_PS2068_P3p_SSSS
      }
      
      data_PS2068_P3p_SSSS <- rbind.data.frame(va_data_PS2068_P3p_SSSS, h2_data_PS2068_P3p_SSSS,Evol_data_PS2068_P3p_SSSS,trait_mean_data_PS2068_P3p_SSSS)
      data_PS2068_P3p_SSSS
      
    }
    Vm_PS2068_P3p_SSSS <- rbind.data.frame(liab_PS2068_P3p_SSSS, data_PS2068_P3p_SSSS)
    Vm_PS2068_P3p_SSSS$Pnp_fate <- rep("SSSS", 24)
    Vm_PS2068_P3p_SSSS
    #remove PS2068 P3p_SSSSS models
    {
      remove(PS2068_CONTROL_P3p_SSSS_mod)
      remove(PS2068_MA_P3p_SSSS_mod)
      remove(PS2068_Vm_P3p_SSSS_mod)
    }
  }
  
  ##Summary PS2068 P4p----
  {
    #Summary liability scale PS2068 P4p
    {
      #Summary va_liab_PS2068_P4p_SSSS: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_PS2068_P4p_SSSS <- rbind(mean(va_liab_PS2068_CONTROL_P4p_SSSS_mod/2),mean(va_liab_PS2068_MA_P4p_SSSS_mod/2),mean(va_liab_PS2068_Vm_P4p_SSSS_mod/2))
        colnames(mean_va_liab_PS2068_P4p_SSSS) <- c("mean")
        median_va_liab_PS2068_P4p_SSSS <- rbind(median(va_liab_PS2068_CONTROL_P4p_SSSS_mod/2),median(va_liab_PS2068_MA_P4p_SSSS_mod/2),median(va_liab_PS2068_Vm_P4p_SSSS_mod/2))
        colnames(median_va_liab_PS2068_P4p_SSSS) <- c("median")
        posterior.mode_va_liab_PS2068_P4p_SSSS <- rbind(posterior.mode(va_liab_PS2068_CONTROL_P4p_SSSS_mod/2),posterior.mode(va_liab_PS2068_MA_P4p_SSSS_mod/2),posterior.mode(va_liab_PS2068_Vm_P4p_SSSS_mod/2))
        colnames(posterior.mode_va_liab_PS2068_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_PS2068_P4p_SSSS <- rbind(HPDinterval(va_liab_PS2068_CONTROL_P4p_SSSS_mod/2),HPDinterval(va_liab_PS2068_MA_P4p_SSSS_mod/2),HPDinterval(va_liab_PS2068_Vm_P4p_SSSS_mod/2))
        colnames(HPDinterval_0.95_va_liab_PS2068_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_PS2068_P4p_SSSS <- rbind(HPDinterval(va_liab_PS2068_CONTROL_P4p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_PS2068_MA_P4p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_PS2068_Vm_P4p_SSSS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_PS2068_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_PS2068_P4p_SSSS <- rbind(effectiveSize(va_liab_PS2068_CONTROL_P4p_SSSS_mod/2),effectiveSize(va_liab_PS2068_MA_P4p_SSSS_mod/2),effectiveSize(va_liab_PS2068_Vm_P4p_SSSS_mod/2))
        colnames(effectiveSize_va_liab_PS2068_P4p_SSSS) <- c("effectiveSize")
        va_liab_PS2068_P4p_SSSS <- cbind.data.frame(mean_va_liab_PS2068_P4p_SSSS,median_va_liab_PS2068_P4p_SSSS,posterior.mode_va_liab_PS2068_P4p_SSSS,HPDinterval_0.95_va_liab_PS2068_P4p_SSSS,HPDinterval_0.83_va_liab_PS2068_P4p_SSSS,effectiveSize_va_liab_PS2068_P4p_SSSS)
        rownames(va_liab_PS2068_P4p_SSSS) <- c("va_liab_PS2068_CONTROL_P4p_SSSS_mod","va_liab_PS2068_MA_P4p_SSSS_mod","va_liab_PS2068_Vm_P4p_SSSS_mod")
        va_liab_PS2068_P4p_SSSS <- cbind(Models = rownames(va_liab_PS2068_P4p_SSSS),va_liab_PS2068_P4p_SSSS)
        rownames(va_liab_PS2068_P4p_SSSS) <- NULL
        va_liab_PS2068_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        va_liab_PS2068_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        va_liab_PS2068_P4p_SSSS$Measure <- c("Va","Va","Va")
        va_liab_PS2068_P4p_SSSS$Scale <- c("liab","liab","liab")
        va_liab_PS2068_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_liab_PS2068_P4p_SSSS
      }
      
      #Summary h2_liab_PS2068_P4p_SSSS
      {
        mean_h2_liab_PS2068_P4p_SSSS <- rbind(mean(h2_liab_PS2068_CONTROL_P4p_SSSS_mod),mean(h2_liab_PS2068_MA_P4p_SSSS_mod),mean(h2_liab_PS2068_Vm_P4p_SSSS_mod))
        colnames(mean_h2_liab_PS2068_P4p_SSSS) <- c("mean")
        median_h2_liab_PS2068_P4p_SSSS <- rbind(median(h2_liab_PS2068_CONTROL_P4p_SSSS_mod),median(h2_liab_PS2068_MA_P4p_SSSS_mod),median(h2_liab_PS2068_Vm_P4p_SSSS_mod))
        colnames(median_h2_liab_PS2068_P4p_SSSS) <- c("median")
        posterior.mode_h2_liab_PS2068_P4p_SSSS <- rbind(posterior.mode(h2_liab_PS2068_CONTROL_P4p_SSSS_mod),posterior.mode(h2_liab_PS2068_MA_P4p_SSSS_mod),posterior.mode(h2_liab_PS2068_Vm_P4p_SSSS_mod))
        colnames(posterior.mode_h2_liab_PS2068_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_PS2068_P4p_SSSS <- rbind(HPDinterval(h2_liab_PS2068_CONTROL_P4p_SSSS_mod),HPDinterval(h2_liab_PS2068_MA_P4p_SSSS_mod),HPDinterval(h2_liab_PS2068_Vm_P4p_SSSS_mod))
        colnames(HPDinterval_0.95_h2_liab_PS2068_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_PS2068_P4p_SSSS <- rbind(HPDinterval(h2_liab_PS2068_CONTROL_P4p_SSSS_mod,prob=.83),HPDinterval(h2_liab_PS2068_MA_P4p_SSSS_mod,prob=.83),HPDinterval(h2_liab_PS2068_Vm_P4p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_PS2068_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_PS2068_P4p_SSSS <- rbind(effectiveSize(h2_liab_PS2068_CONTROL_P4p_SSSS_mod),effectiveSize(h2_liab_PS2068_MA_P4p_SSSS_mod),effectiveSize(h2_liab_PS2068_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_h2_liab_PS2068_P4p_SSSS) <- c("effectiveSize")
        h2_liab_PS2068_P4p_SSSS <- cbind.data.frame(mean_h2_liab_PS2068_P4p_SSSS,median_h2_liab_PS2068_P4p_SSSS,posterior.mode_h2_liab_PS2068_P4p_SSSS,HPDinterval_0.95_h2_liab_PS2068_P4p_SSSS,HPDinterval_0.83_h2_liab_PS2068_P4p_SSSS,effectiveSize_h2_liab_PS2068_P4p_SSSS)
        rownames(h2_liab_PS2068_P4p_SSSS) <- c("h2_liab_PS2068_CONTROL_P4p_SSSS_mod","h2_liab_PS2068_MA_P4p_SSSS_mod","h2_liab_PS2068_Vm_P4p_SSSS_mod")
        h2_liab_PS2068_P4p_SSSS <- cbind(Models = rownames(h2_liab_PS2068_P4p_SSSS),h2_liab_PS2068_P4p_SSSS)
        rownames(h2_liab_PS2068_P4p_SSSS) <- NULL
        h2_liab_PS2068_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        h2_liab_PS2068_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_liab_PS2068_P4p_SSSS$Measure <- c("H2","H2","H2")
        h2_liab_PS2068_P4p_SSSS$Scale <- c("liab","liab","liab")
        h2_liab_PS2068_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_liab_PS2068_P4p_SSSS
      }
      
      #Summary Evol_liab_PS2068_P4p_SSSS
      {
        mean_Evol_liab_PS2068_P4p_SSSS <- rbind(mean(Evol_liab_PS2068_CONTROL_P4p_SSSS_mod),mean(Evol_liab_PS2068_MA_P4p_SSSS_mod),mean(Evol_liab_PS2068_Vm_P4p_SSSS_mod))
        colnames(mean_Evol_liab_PS2068_P4p_SSSS) <- c("mean")
        median_Evol_liab_PS2068_P4p_SSSS <- rbind(median(Evol_liab_PS2068_CONTROL_P4p_SSSS_mod),median(Evol_liab_PS2068_MA_P4p_SSSS_mod),median(Evol_liab_PS2068_Vm_P4p_SSSS_mod))
        colnames(median_Evol_liab_PS2068_P4p_SSSS) <- c("median")
        posterior.mode_Evol_liab_PS2068_P4p_SSSS <- rbind(posterior.mode(Evol_liab_PS2068_CONTROL_P4p_SSSS_mod),posterior.mode(Evol_liab_PS2068_MA_P4p_SSSS_mod),posterior.mode(Evol_liab_PS2068_Vm_P4p_SSSS_mod))
        colnames(posterior.mode_Evol_liab_PS2068_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_PS2068_P4p_SSSS <- rbind(HPDinterval(Evol_liab_PS2068_CONTROL_P4p_SSSS_mod),HPDinterval(Evol_liab_PS2068_MA_P4p_SSSS_mod),HPDinterval(Evol_liab_PS2068_Vm_P4p_SSSS_mod))
        colnames(HPDinterval_0.95_Evol_liab_PS2068_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_PS2068_P4p_SSSS <- rbind(HPDinterval(Evol_liab_PS2068_CONTROL_P4p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_PS2068_MA_P4p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_PS2068_Vm_P4p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_PS2068_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_PS2068_P4p_SSSS <- rbind(effectiveSize(Evol_liab_PS2068_CONTROL_P4p_SSSS_mod),effectiveSize(Evol_liab_PS2068_MA_P4p_SSSS_mod),effectiveSize(Evol_liab_PS2068_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_Evol_liab_PS2068_P4p_SSSS) <- c("effectiveSize")
        Evol_liab_PS2068_P4p_SSSS <- cbind.data.frame(mean_Evol_liab_PS2068_P4p_SSSS,median_Evol_liab_PS2068_P4p_SSSS,posterior.mode_Evol_liab_PS2068_P4p_SSSS,HPDinterval_0.95_Evol_liab_PS2068_P4p_SSSS,HPDinterval_0.83_Evol_liab_PS2068_P4p_SSSS,effectiveSize_Evol_liab_PS2068_P4p_SSSS)
        rownames(Evol_liab_PS2068_P4p_SSSS) <- c("Evol_liab_PS2068_CONTROL_P4p_SSSS_mod","Evol_liab_PS2068_MA_P4p_SSSS_mod","Evol_liab_PS2068_Vm_P4p_SSSS_mod")
        Evol_liab_PS2068_P4p_SSSS <- cbind(Models = rownames(Evol_liab_PS2068_P4p_SSSS),Evol_liab_PS2068_P4p_SSSS)
        rownames(Evol_liab_PS2068_P4p_SSSS) <- NULL
        Evol_liab_PS2068_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        Evol_liab_PS2068_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_liab_PS2068_P4p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_PS2068_P4p_SSSS$Scale <- c("liab","liab","liab")
        Evol_liab_PS2068_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_PS2068_P4p_SSSS
      }
      
      #Summary trait_mean_liab_PS2068_P4p_SSSS
      {
        mean_trait_mean_liab_PS2068_P4p_SSSS <- rbind(mean(trait_mean_liab_PS2068_CONTROL_P4p_SSSS_mod),mean(trait_mean_liab_PS2068_MA_P4p_SSSS_mod),mean(trait_mean_liab_PS2068_Vm_P4p_SSSS_mod))
        colnames(mean_trait_mean_liab_PS2068_P4p_SSSS) <- c("mean")
        median_trait_mean_liab_PS2068_P4p_SSSS <- rbind(median(trait_mean_liab_PS2068_CONTROL_P4p_SSSS_mod),median(trait_mean_liab_PS2068_MA_P4p_SSSS_mod),median(trait_mean_liab_PS2068_Vm_P4p_SSSS_mod))
        colnames(median_trait_mean_liab_PS2068_P4p_SSSS) <- c("median")
        posterior.mode_trait_mean_liab_PS2068_P4p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_PS2068_CONTROL_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_PS2068_MA_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_PS2068_Vm_P4p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_liab_PS2068_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_PS2068_P4p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_PS2068_CONTROL_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_PS2068_MA_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_PS2068_Vm_P4p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_PS2068_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_PS2068_P4p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_PS2068_CONTROL_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_PS2068_MA_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_PS2068_Vm_P4p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_PS2068_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_PS2068_P4p_SSSS <- rbind(effectiveSize(trait_mean_liab_PS2068_CONTROL_P4p_SSSS_mod),effectiveSize(trait_mean_liab_PS2068_MA_P4p_SSSS_mod),effectiveSize(trait_mean_liab_PS2068_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_trait_mean_liab_PS2068_P4p_SSSS) <- c("effectiveSize")
        trait_mean_liab_PS2068_P4p_SSSS <- cbind.data.frame(mean_trait_mean_liab_PS2068_P4p_SSSS,median_trait_mean_liab_PS2068_P4p_SSSS,posterior.mode_trait_mean_liab_PS2068_P4p_SSSS,HPDinterval_0.95_trait_mean_liab_PS2068_P4p_SSSS,HPDinterval_0.83_trait_mean_liab_PS2068_P4p_SSSS,effectiveSize_trait_mean_liab_PS2068_P4p_SSSS)
        rownames(trait_mean_liab_PS2068_P4p_SSSS) <- c("trait_mean_liab_PS2068_CONTROL_P4p_SSSS_mod","trait_mean_liab_PS2068_MA_P4p_SSSS_mod","trait_mean_liab_PS2068_Vm_P4p_SSSS_mod")
        trait_mean_liab_PS2068_P4p_SSSS <- cbind(Models = rownames(trait_mean_liab_PS2068_P4p_SSSS),trait_mean_liab_PS2068_P4p_SSSS)
        rownames(trait_mean_liab_PS2068_P4p_SSSS) <- NULL
        trait_mean_liab_PS2068_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        trait_mean_liab_PS2068_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_PS2068_P4p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_PS2068_P4p_SSSS$Scale <- c("liab","liab","liab")
        trait_mean_liab_PS2068_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_PS2068_P4p_SSSS
      }
      
      liab_PS2068_P4p_SSSS <- rbind.data.frame(va_liab_PS2068_P4p_SSSS, h2_liab_PS2068_P4p_SSSS,Evol_liab_PS2068_P4p_SSSS,trait_mean_liab_PS2068_P4p_SSSS)
      liab_PS2068_P4p_SSSS
    }
    #Summary data scale PS2068 P4p
    {
      #Summary va_data_PS2068_P4p_SSSS:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_PS2068_P4p_SSSS <- rbind(mean(va_data_PS2068_CONTROL_P4p_SSSS_mod/2),mean(va_data_PS2068_MA_P4p_SSSS_mod/2),mean(va_data_PS2068_Vm_P4p_SSSS_mod/2))
        colnames(mean_va_data_PS2068_P4p_SSSS) <- c("mean")
        median_va_data_PS2068_P4p_SSSS <- rbind(median(va_data_PS2068_CONTROL_P4p_SSSS_mod/2),median(va_data_PS2068_MA_P4p_SSSS_mod/2),median(va_data_PS2068_Vm_P4p_SSSS_mod/2))
        colnames(median_va_data_PS2068_P4p_SSSS) <- c("median")
        posterior.mode_va_data_PS2068_P4p_SSSS <- rbind(posterior.mode(as.mcmc(va_data_PS2068_CONTROL_P4p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_PS2068_MA_P4p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_PS2068_Vm_P4p_SSSS_mod/2)))
        colnames(posterior.mode_va_data_PS2068_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_PS2068_P4p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_PS2068_CONTROL_P4p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_PS2068_MA_P4p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_PS2068_Vm_P4p_SSSS_mod/2)))
        colnames(HPDinterval_0.95_va_data_PS2068_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_PS2068_P4p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_PS2068_CONTROL_P4p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_PS2068_MA_P4p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_PS2068_Vm_P4p_SSSS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_PS2068_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_PS2068_P4p_SSSS <- rbind(effectiveSize(va_data_PS2068_CONTROL_P4p_SSSS_mod/2),effectiveSize(va_data_PS2068_MA_P4p_SSSS_mod/2),effectiveSize(va_data_PS2068_Vm_P4p_SSSS_mod/2))
        colnames(effectiveSize_va_data_PS2068_P4p_SSSS) <- c("effectiveSize")
        va_data_PS2068_P4p_SSSS <- cbind.data.frame(mean_va_data_PS2068_P4p_SSSS,median_va_data_PS2068_P4p_SSSS,posterior.mode_va_data_PS2068_P4p_SSSS,HPDinterval_0.95_va_data_PS2068_P4p_SSSS,HPDinterval_0.83_va_data_PS2068_P4p_SSSS,effectiveSize_va_data_PS2068_P4p_SSSS)
        rownames(va_data_PS2068_P4p_SSSS) <- c("va_data_PS2068_CONTROL_P4p_SSSS_mod","va_data_PS2068_MA_P4p_SSSS_mod","va_data_PS2068_Vm_P4p_SSSS_mod")
        va_data_PS2068_P4p_SSSS <- cbind(Models = rownames(va_data_PS2068_P4p_SSSS),va_data_PS2068_P4p_SSSS)
        rownames(va_data_PS2068_P4p_SSSS) <- NULL
        va_data_PS2068_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        va_data_PS2068_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        va_data_PS2068_P4p_SSSS$Measure <- c("Va","Va","Va")
        va_data_PS2068_P4p_SSSS$Scale <- c("data","data","data")
        va_data_PS2068_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_data_PS2068_P4p_SSSS
      }
      
      #Summary h2_data_PS2068_P4p_SSSS
      {
        mean_h2_data_PS2068_P4p_SSSS <- rbind(mean(h2_data_PS2068_CONTROL_P4p_SSSS_mod),mean(h2_data_PS2068_MA_P4p_SSSS_mod),mean(h2_data_PS2068_Vm_P4p_SSSS_mod))
        colnames(mean_h2_data_PS2068_P4p_SSSS) <- c("mean")
        median_h2_data_PS2068_P4p_SSSS <- rbind(median(h2_data_PS2068_CONTROL_P4p_SSSS_mod),median(h2_data_PS2068_MA_P4p_SSSS_mod),median(h2_data_PS2068_Vm_P4p_SSSS_mod))
        colnames(median_h2_data_PS2068_P4p_SSSS) <- c("median")
        posterior.mode_h2_data_PS2068_P4p_SSSS <- rbind(posterior.mode(as.mcmc(h2_data_PS2068_CONTROL_P4p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_PS2068_MA_P4p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_PS2068_Vm_P4p_SSSS_mod)))
        colnames(posterior.mode_h2_data_PS2068_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_PS2068_P4p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_PS2068_CONTROL_P4p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_PS2068_MA_P4p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_PS2068_Vm_P4p_SSSS_mod)))
        colnames(HPDinterval_0.95_h2_data_PS2068_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_PS2068_P4p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_PS2068_CONTROL_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_PS2068_MA_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_PS2068_Vm_P4p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_PS2068_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_PS2068_P4p_SSSS <- rbind(effectiveSize(h2_data_PS2068_CONTROL_P4p_SSSS_mod),effectiveSize(h2_data_PS2068_MA_P4p_SSSS_mod),effectiveSize(h2_data_PS2068_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_h2_data_PS2068_P4p_SSSS) <- c("effectiveSize")
        h2_data_PS2068_P4p_SSSS <- cbind.data.frame(mean_h2_data_PS2068_P4p_SSSS,median_h2_data_PS2068_P4p_SSSS,posterior.mode_h2_data_PS2068_P4p_SSSS,HPDinterval_0.95_h2_data_PS2068_P4p_SSSS,HPDinterval_0.83_h2_data_PS2068_P4p_SSSS,effectiveSize_h2_data_PS2068_P4p_SSSS)
        rownames(h2_data_PS2068_P4p_SSSS) <- c("h2_data_PS2068_CONTROL_P4p_SSSS_mod","h2_data_PS2068_MA_P4p_SSSS_mod","h2_data_PS2068_Vm_P4p_SSSS_mod")
        h2_data_PS2068_P4p_SSSS <- cbind(Models = rownames(h2_data_PS2068_P4p_SSSS),h2_data_PS2068_P4p_SSSS)
        rownames(h2_data_PS2068_P4p_SSSS) <- NULL
        h2_data_PS2068_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        h2_data_PS2068_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_data_PS2068_P4p_SSSS$Measure <- c("H2","H2","H2")
        h2_data_PS2068_P4p_SSSS$Scale <- c("data","data","data")
        h2_data_PS2068_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_data_PS2068_P4p_SSSS
      }
      
      #Summary Evol_data_PS2068_P4p_SSSS
      {
        mean_Evol_data_PS2068_P4p_SSSS <- rbind(mean(Evol_data_PS2068_CONTROL_P4p_SSSS_mod),mean(Evol_data_PS2068_MA_P4p_SSSS_mod),mean(Evol_data_PS2068_Vm_P4p_SSSS_mod))
        colnames(mean_Evol_data_PS2068_P4p_SSSS) <- c("mean")
        median_Evol_data_PS2068_P4p_SSSS <- rbind(median(Evol_data_PS2068_CONTROL_P4p_SSSS_mod),median(Evol_data_PS2068_MA_P4p_SSSS_mod),median(Evol_data_PS2068_Vm_P4p_SSSS_mod))
        colnames(median_Evol_data_PS2068_P4p_SSSS) <- c("median")
        posterior.mode_Evol_data_PS2068_P4p_SSSS <- rbind(posterior.mode(as.mcmc(Evol_data_PS2068_CONTROL_P4p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_PS2068_MA_P4p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_PS2068_Vm_P4p_SSSS_mod)))
        colnames(posterior.mode_Evol_data_PS2068_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_PS2068_P4p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_PS2068_CONTROL_P4p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_PS2068_MA_P4p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_PS2068_Vm_P4p_SSSS_mod)))
        colnames(HPDinterval_0.95_Evol_data_PS2068_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_PS2068_P4p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_PS2068_CONTROL_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_PS2068_MA_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_PS2068_Vm_P4p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_PS2068_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_PS2068_P4p_SSSS <- rbind(effectiveSize(Evol_data_PS2068_CONTROL_P4p_SSSS_mod),effectiveSize(Evol_data_PS2068_MA_P4p_SSSS_mod),effectiveSize(Evol_data_PS2068_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_Evol_data_PS2068_P4p_SSSS) <- c("effectiveSize")
        Evol_data_PS2068_P4p_SSSS <- cbind.data.frame(mean_Evol_data_PS2068_P4p_SSSS,median_Evol_data_PS2068_P4p_SSSS,posterior.mode_Evol_data_PS2068_P4p_SSSS,HPDinterval_0.95_Evol_data_PS2068_P4p_SSSS,HPDinterval_0.83_Evol_data_PS2068_P4p_SSSS,effectiveSize_Evol_data_PS2068_P4p_SSSS)
        rownames(Evol_data_PS2068_P4p_SSSS) <- c("Evol_data_PS2068_CONTROL_P4p_SSSS_mod","Evol_data_PS2068_MA_P4p_SSSS_mod","Evol_data_PS2068_Vm_P4p_SSSS_mod")
        Evol_data_PS2068_P4p_SSSS <- cbind(Models = rownames(Evol_data_PS2068_P4p_SSSS),Evol_data_PS2068_P4p_SSSS)
        rownames(Evol_data_PS2068_P4p_SSSS) <- NULL
        Evol_data_PS2068_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        Evol_data_PS2068_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_data_PS2068_P4p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_data_PS2068_P4p_SSSS$Scale <- c("data","data","data")
        Evol_data_PS2068_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_data_PS2068_P4p_SSSS
      }
      
      #Summary trait_mean_data_PS2068_P4p_SSSS
      {
        mean_trait_mean_data_PS2068_P4p_SSSS <- rbind(mean(trait_mean_data_PS2068_CONTROL_P4p_SSSS_mod),mean(trait_mean_data_PS2068_MA_P4p_SSSS_mod),mean(trait_mean_data_PS2068_Vm_P4p_SSSS_mod))
        colnames(mean_trait_mean_data_PS2068_P4p_SSSS) <- c("mean")
        median_trait_mean_data_PS2068_P4p_SSSS <- rbind(median(trait_mean_data_PS2068_CONTROL_P4p_SSSS_mod),median(trait_mean_data_PS2068_MA_P4p_SSSS_mod),median(trait_mean_data_PS2068_Vm_P4p_SSSS_mod))
        colnames(median_trait_mean_data_PS2068_P4p_SSSS) <- c("median")
        posterior.mode_trait_mean_data_PS2068_P4p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_data_PS2068_CONTROL_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_PS2068_MA_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_PS2068_Vm_P4p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_data_PS2068_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_PS2068_P4p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_PS2068_CONTROL_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_PS2068_MA_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_PS2068_Vm_P4p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_PS2068_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_PS2068_P4p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_PS2068_CONTROL_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_PS2068_MA_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_PS2068_Vm_P4p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_PS2068_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_PS2068_P4p_SSSS <- rbind(effectiveSize(trait_mean_data_PS2068_CONTROL_P4p_SSSS_mod),effectiveSize(trait_mean_data_PS2068_MA_P4p_SSSS_mod),effectiveSize(trait_mean_data_PS2068_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_trait_mean_data_PS2068_P4p_SSSS) <- c("effectiveSize")
        trait_mean_data_PS2068_P4p_SSSS <- cbind.data.frame(mean_trait_mean_data_PS2068_P4p_SSSS,median_trait_mean_data_PS2068_P4p_SSSS,posterior.mode_trait_mean_data_PS2068_P4p_SSSS,HPDinterval_0.95_trait_mean_data_PS2068_P4p_SSSS,HPDinterval_0.83_trait_mean_data_PS2068_P4p_SSSS,effectiveSize_trait_mean_data_PS2068_P4p_SSSS)
        rownames(trait_mean_data_PS2068_P4p_SSSS) <- c("trait_mean_data_PS2068_CONTROL_P4p_SSSS_mod","trait_mean_data_PS2068_MA_P4p_SSSS_mod","trait_mean_data_PS2068_Vm_P4p_SSSS_mod")
        trait_mean_data_PS2068_P4p_SSSS <- cbind(Models = rownames(trait_mean_data_PS2068_P4p_SSSS),trait_mean_data_PS2068_P4p_SSSS)
        rownames(trait_mean_data_PS2068_P4p_SSSS) <- NULL
        trait_mean_data_PS2068_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        trait_mean_data_PS2068_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_data_PS2068_P4p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_PS2068_P4p_SSSS$Scale <- c("data","data","data")
        trait_mean_data_PS2068_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_PS2068_P4p_SSSS
      }
      
      data_PS2068_P4p_SSSS <- rbind.data.frame(va_data_PS2068_P4p_SSSS, h2_data_PS2068_P4p_SSSS,Evol_data_PS2068_P4p_SSSS,trait_mean_data_PS2068_P4p_SSSS)
      data_PS2068_P4p_SSSS
      
    }
    Vm_PS2068_P4p_SSSS <- rbind.data.frame(liab_PS2068_P4p_SSSS, data_PS2068_P4p_SSSS)
    Vm_PS2068_P4p_SSSS$Pnp_fate <- rep("SSSS", 24)
    Vm_PS2068_P4p_SSSS
    #remove PS2068 P4p_SSSSS models
    {
      remove(PS2068_CONTROL_P4p_SSSS_mod)
      remove(PS2068_MA_P4p_SSSS_mod)
      remove(PS2068_Vm_P4p_SSSS_mod)
    }
  }
  
  ##Summary PS2068 P8p----
  {
    #Summary liability scale PS2068 P8p
    {
      #Summary va_liab_PS2068_P8p_SSSS: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_PS2068_P8p_SSSS <- rbind(mean(va_liab_PS2068_CONTROL_P8p_SSSS_mod/2),mean(va_liab_PS2068_MA_P8p_SSSS_mod/2),mean(va_liab_PS2068_Vm_P8p_SSSS_mod/2))
        colnames(mean_va_liab_PS2068_P8p_SSSS) <- c("mean")
        median_va_liab_PS2068_P8p_SSSS <- rbind(median(va_liab_PS2068_CONTROL_P8p_SSSS_mod/2),median(va_liab_PS2068_MA_P8p_SSSS_mod/2),median(va_liab_PS2068_Vm_P8p_SSSS_mod/2))
        colnames(median_va_liab_PS2068_P8p_SSSS) <- c("median")
        posterior.mode_va_liab_PS2068_P8p_SSSS <- rbind(posterior.mode(va_liab_PS2068_CONTROL_P8p_SSSS_mod/2),posterior.mode(va_liab_PS2068_MA_P8p_SSSS_mod/2),posterior.mode(va_liab_PS2068_Vm_P8p_SSSS_mod/2))
        colnames(posterior.mode_va_liab_PS2068_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_PS2068_P8p_SSSS <- rbind(HPDinterval(va_liab_PS2068_CONTROL_P8p_SSSS_mod/2),HPDinterval(va_liab_PS2068_MA_P8p_SSSS_mod/2),HPDinterval(va_liab_PS2068_Vm_P8p_SSSS_mod/2))
        colnames(HPDinterval_0.95_va_liab_PS2068_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_PS2068_P8p_SSSS <- rbind(HPDinterval(va_liab_PS2068_CONTROL_P8p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_PS2068_MA_P8p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_PS2068_Vm_P8p_SSSS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_PS2068_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_PS2068_P8p_SSSS <- rbind(effectiveSize(va_liab_PS2068_CONTROL_P8p_SSSS_mod/2),effectiveSize(va_liab_PS2068_MA_P8p_SSSS_mod/2),effectiveSize(va_liab_PS2068_Vm_P8p_SSSS_mod/2))
        colnames(effectiveSize_va_liab_PS2068_P8p_SSSS) <- c("effectiveSize")
        va_liab_PS2068_P8p_SSSS <- cbind.data.frame(mean_va_liab_PS2068_P8p_SSSS,median_va_liab_PS2068_P8p_SSSS,posterior.mode_va_liab_PS2068_P8p_SSSS,HPDinterval_0.95_va_liab_PS2068_P8p_SSSS,HPDinterval_0.83_va_liab_PS2068_P8p_SSSS,effectiveSize_va_liab_PS2068_P8p_SSSS)
        rownames(va_liab_PS2068_P8p_SSSS) <- c("va_liab_PS2068_CONTROL_P8p_SSSS_mod","va_liab_PS2068_MA_P8p_SSSS_mod","va_liab_PS2068_Vm_P8p_SSSS_mod")
        va_liab_PS2068_P8p_SSSS <- cbind(Models = rownames(va_liab_PS2068_P8p_SSSS),va_liab_PS2068_P8p_SSSS)
        rownames(va_liab_PS2068_P8p_SSSS) <- NULL
        va_liab_PS2068_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        va_liab_PS2068_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        va_liab_PS2068_P8p_SSSS$Measure <- c("Va","Va","Va")
        va_liab_PS2068_P8p_SSSS$Scale <- c("liab","liab","liab")
        va_liab_PS2068_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_liab_PS2068_P8p_SSSS
      }
      
      #Summary h2_liab_PS2068_P8p_SSSS
      {
        mean_h2_liab_PS2068_P8p_SSSS <- rbind(mean(h2_liab_PS2068_CONTROL_P8p_SSSS_mod),mean(h2_liab_PS2068_MA_P8p_SSSS_mod),mean(h2_liab_PS2068_Vm_P8p_SSSS_mod))
        colnames(mean_h2_liab_PS2068_P8p_SSSS) <- c("mean")
        median_h2_liab_PS2068_P8p_SSSS <- rbind(median(h2_liab_PS2068_CONTROL_P8p_SSSS_mod),median(h2_liab_PS2068_MA_P8p_SSSS_mod),median(h2_liab_PS2068_Vm_P8p_SSSS_mod))
        colnames(median_h2_liab_PS2068_P8p_SSSS) <- c("median")
        posterior.mode_h2_liab_PS2068_P8p_SSSS <- rbind(posterior.mode(h2_liab_PS2068_CONTROL_P8p_SSSS_mod),posterior.mode(h2_liab_PS2068_MA_P8p_SSSS_mod),posterior.mode(h2_liab_PS2068_Vm_P8p_SSSS_mod))
        colnames(posterior.mode_h2_liab_PS2068_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_PS2068_P8p_SSSS <- rbind(HPDinterval(h2_liab_PS2068_CONTROL_P8p_SSSS_mod),HPDinterval(h2_liab_PS2068_MA_P8p_SSSS_mod),HPDinterval(h2_liab_PS2068_Vm_P8p_SSSS_mod))
        colnames(HPDinterval_0.95_h2_liab_PS2068_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_PS2068_P8p_SSSS <- rbind(HPDinterval(h2_liab_PS2068_CONTROL_P8p_SSSS_mod,prob=.83),HPDinterval(h2_liab_PS2068_MA_P8p_SSSS_mod,prob=.83),HPDinterval(h2_liab_PS2068_Vm_P8p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_PS2068_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_PS2068_P8p_SSSS <- rbind(effectiveSize(h2_liab_PS2068_CONTROL_P8p_SSSS_mod),effectiveSize(h2_liab_PS2068_MA_P8p_SSSS_mod),effectiveSize(h2_liab_PS2068_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_h2_liab_PS2068_P8p_SSSS) <- c("effectiveSize")
        h2_liab_PS2068_P8p_SSSS <- cbind.data.frame(mean_h2_liab_PS2068_P8p_SSSS,median_h2_liab_PS2068_P8p_SSSS,posterior.mode_h2_liab_PS2068_P8p_SSSS,HPDinterval_0.95_h2_liab_PS2068_P8p_SSSS,HPDinterval_0.83_h2_liab_PS2068_P8p_SSSS,effectiveSize_h2_liab_PS2068_P8p_SSSS)
        rownames(h2_liab_PS2068_P8p_SSSS) <- c("h2_liab_PS2068_CONTROL_P8p_SSSS_mod","h2_liab_PS2068_MA_P8p_SSSS_mod","h2_liab_PS2068_Vm_P8p_SSSS_mod")
        h2_liab_PS2068_P8p_SSSS <- cbind(Models = rownames(h2_liab_PS2068_P8p_SSSS),h2_liab_PS2068_P8p_SSSS)
        rownames(h2_liab_PS2068_P8p_SSSS) <- NULL
        h2_liab_PS2068_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        h2_liab_PS2068_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_liab_PS2068_P8p_SSSS$Measure <- c("H2","H2","H2")
        h2_liab_PS2068_P8p_SSSS$Scale <- c("liab","liab","liab")
        h2_liab_PS2068_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_liab_PS2068_P8p_SSSS
      }
      
      #Summary Evol_liab_PS2068_P8p_SSSS
      {
        mean_Evol_liab_PS2068_P8p_SSSS <- rbind(mean(Evol_liab_PS2068_CONTROL_P8p_SSSS_mod),mean(Evol_liab_PS2068_MA_P8p_SSSS_mod),mean(Evol_liab_PS2068_Vm_P8p_SSSS_mod))
        colnames(mean_Evol_liab_PS2068_P8p_SSSS) <- c("mean")
        median_Evol_liab_PS2068_P8p_SSSS <- rbind(median(Evol_liab_PS2068_CONTROL_P8p_SSSS_mod),median(Evol_liab_PS2068_MA_P8p_SSSS_mod),median(Evol_liab_PS2068_Vm_P8p_SSSS_mod))
        colnames(median_Evol_liab_PS2068_P8p_SSSS) <- c("median")
        posterior.mode_Evol_liab_PS2068_P8p_SSSS <- rbind(posterior.mode(Evol_liab_PS2068_CONTROL_P8p_SSSS_mod),posterior.mode(Evol_liab_PS2068_MA_P8p_SSSS_mod),posterior.mode(Evol_liab_PS2068_Vm_P8p_SSSS_mod))
        colnames(posterior.mode_Evol_liab_PS2068_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_PS2068_P8p_SSSS <- rbind(HPDinterval(Evol_liab_PS2068_CONTROL_P8p_SSSS_mod),HPDinterval(Evol_liab_PS2068_MA_P8p_SSSS_mod),HPDinterval(Evol_liab_PS2068_Vm_P8p_SSSS_mod))
        colnames(HPDinterval_0.95_Evol_liab_PS2068_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_PS2068_P8p_SSSS <- rbind(HPDinterval(Evol_liab_PS2068_CONTROL_P8p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_PS2068_MA_P8p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_PS2068_Vm_P8p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_PS2068_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_PS2068_P8p_SSSS <- rbind(effectiveSize(Evol_liab_PS2068_CONTROL_P8p_SSSS_mod),effectiveSize(Evol_liab_PS2068_MA_P8p_SSSS_mod),effectiveSize(Evol_liab_PS2068_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_Evol_liab_PS2068_P8p_SSSS) <- c("effectiveSize")
        Evol_liab_PS2068_P8p_SSSS <- cbind.data.frame(mean_Evol_liab_PS2068_P8p_SSSS,median_Evol_liab_PS2068_P8p_SSSS,posterior.mode_Evol_liab_PS2068_P8p_SSSS,HPDinterval_0.95_Evol_liab_PS2068_P8p_SSSS,HPDinterval_0.83_Evol_liab_PS2068_P8p_SSSS,effectiveSize_Evol_liab_PS2068_P8p_SSSS)
        rownames(Evol_liab_PS2068_P8p_SSSS) <- c("Evol_liab_PS2068_CONTROL_P8p_SSSS_mod","Evol_liab_PS2068_MA_P8p_SSSS_mod","Evol_liab_PS2068_Vm_P8p_SSSS_mod")
        Evol_liab_PS2068_P8p_SSSS <- cbind(Models = rownames(Evol_liab_PS2068_P8p_SSSS),Evol_liab_PS2068_P8p_SSSS)
        rownames(Evol_liab_PS2068_P8p_SSSS) <- NULL
        Evol_liab_PS2068_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        Evol_liab_PS2068_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_liab_PS2068_P8p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_PS2068_P8p_SSSS$Scale <- c("liab","liab","liab")
        Evol_liab_PS2068_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_PS2068_P8p_SSSS
      }
      
      #Summary trait_mean_liab_PS2068_P8p_SSSS
      {
        mean_trait_mean_liab_PS2068_P8p_SSSS <- rbind(mean(trait_mean_liab_PS2068_CONTROL_P8p_SSSS_mod),mean(trait_mean_liab_PS2068_MA_P8p_SSSS_mod),mean(trait_mean_liab_PS2068_Vm_P8p_SSSS_mod))
        colnames(mean_trait_mean_liab_PS2068_P8p_SSSS) <- c("mean")
        median_trait_mean_liab_PS2068_P8p_SSSS <- rbind(median(trait_mean_liab_PS2068_CONTROL_P8p_SSSS_mod),median(trait_mean_liab_PS2068_MA_P8p_SSSS_mod),median(trait_mean_liab_PS2068_Vm_P8p_SSSS_mod))
        colnames(median_trait_mean_liab_PS2068_P8p_SSSS) <- c("median")
        posterior.mode_trait_mean_liab_PS2068_P8p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_PS2068_CONTROL_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_PS2068_MA_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_PS2068_Vm_P8p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_liab_PS2068_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_PS2068_P8p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_PS2068_CONTROL_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_PS2068_MA_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_PS2068_Vm_P8p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_PS2068_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_PS2068_P8p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_PS2068_CONTROL_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_PS2068_MA_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_PS2068_Vm_P8p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_PS2068_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_PS2068_P8p_SSSS <- rbind(effectiveSize(trait_mean_liab_PS2068_CONTROL_P8p_SSSS_mod),effectiveSize(trait_mean_liab_PS2068_MA_P8p_SSSS_mod),effectiveSize(trait_mean_liab_PS2068_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_trait_mean_liab_PS2068_P8p_SSSS) <- c("effectiveSize")
        trait_mean_liab_PS2068_P8p_SSSS <- cbind.data.frame(mean_trait_mean_liab_PS2068_P8p_SSSS,median_trait_mean_liab_PS2068_P8p_SSSS,posterior.mode_trait_mean_liab_PS2068_P8p_SSSS,HPDinterval_0.95_trait_mean_liab_PS2068_P8p_SSSS,HPDinterval_0.83_trait_mean_liab_PS2068_P8p_SSSS,effectiveSize_trait_mean_liab_PS2068_P8p_SSSS)
        rownames(trait_mean_liab_PS2068_P8p_SSSS) <- c("trait_mean_liab_PS2068_CONTROL_P8p_SSSS_mod","trait_mean_liab_PS2068_MA_P8p_SSSS_mod","trait_mean_liab_PS2068_Vm_P8p_SSSS_mod")
        trait_mean_liab_PS2068_P8p_SSSS <- cbind(Models = rownames(trait_mean_liab_PS2068_P8p_SSSS),trait_mean_liab_PS2068_P8p_SSSS)
        rownames(trait_mean_liab_PS2068_P8p_SSSS) <- NULL
        trait_mean_liab_PS2068_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        trait_mean_liab_PS2068_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_PS2068_P8p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_PS2068_P8p_SSSS$Scale <- c("liab","liab","liab")
        trait_mean_liab_PS2068_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_PS2068_P8p_SSSS
      }
      
      liab_PS2068_P8p_SSSS <- rbind.data.frame(va_liab_PS2068_P8p_SSSS, h2_liab_PS2068_P8p_SSSS,Evol_liab_PS2068_P8p_SSSS,trait_mean_liab_PS2068_P8p_SSSS)
      liab_PS2068_P8p_SSSS
    }
    #Summary data scale PS2068 P8p
    {
      #Summary va_data_PS2068_P8p_SSSS:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_PS2068_P8p_SSSS <- rbind(mean(va_data_PS2068_CONTROL_P8p_SSSS_mod/2),mean(va_data_PS2068_MA_P8p_SSSS_mod/2),mean(va_data_PS2068_Vm_P8p_SSSS_mod/2))
        colnames(mean_va_data_PS2068_P8p_SSSS) <- c("mean")
        median_va_data_PS2068_P8p_SSSS <- rbind(median(va_data_PS2068_CONTROL_P8p_SSSS_mod/2),median(va_data_PS2068_MA_P8p_SSSS_mod/2),median(va_data_PS2068_Vm_P8p_SSSS_mod/2))
        colnames(median_va_data_PS2068_P8p_SSSS) <- c("median")
        posterior.mode_va_data_PS2068_P8p_SSSS <- rbind(posterior.mode(as.mcmc(va_data_PS2068_CONTROL_P8p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_PS2068_MA_P8p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_PS2068_Vm_P8p_SSSS_mod/2)))
        colnames(posterior.mode_va_data_PS2068_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_PS2068_P8p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_PS2068_CONTROL_P8p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_PS2068_MA_P8p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_PS2068_Vm_P8p_SSSS_mod/2)))
        colnames(HPDinterval_0.95_va_data_PS2068_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_PS2068_P8p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_PS2068_CONTROL_P8p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_PS2068_MA_P8p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_PS2068_Vm_P8p_SSSS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_PS2068_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_PS2068_P8p_SSSS <- rbind(effectiveSize(va_data_PS2068_CONTROL_P8p_SSSS_mod/2),effectiveSize(va_data_PS2068_MA_P8p_SSSS_mod/2),effectiveSize(va_data_PS2068_Vm_P8p_SSSS_mod/2))
        colnames(effectiveSize_va_data_PS2068_P8p_SSSS) <- c("effectiveSize")
        va_data_PS2068_P8p_SSSS <- cbind.data.frame(mean_va_data_PS2068_P8p_SSSS,median_va_data_PS2068_P8p_SSSS,posterior.mode_va_data_PS2068_P8p_SSSS,HPDinterval_0.95_va_data_PS2068_P8p_SSSS,HPDinterval_0.83_va_data_PS2068_P8p_SSSS,effectiveSize_va_data_PS2068_P8p_SSSS)
        rownames(va_data_PS2068_P8p_SSSS) <- c("va_data_PS2068_CONTROL_P8p_SSSS_mod","va_data_PS2068_MA_P8p_SSSS_mod","va_data_PS2068_Vm_P8p_SSSS_mod")
        va_data_PS2068_P8p_SSSS <- cbind(Models = rownames(va_data_PS2068_P8p_SSSS),va_data_PS2068_P8p_SSSS)
        rownames(va_data_PS2068_P8p_SSSS) <- NULL
        va_data_PS2068_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        va_data_PS2068_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        va_data_PS2068_P8p_SSSS$Measure <- c("Va","Va","Va")
        va_data_PS2068_P8p_SSSS$Scale <- c("data","data","data")
        va_data_PS2068_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_data_PS2068_P8p_SSSS
      }
      
      #Summary h2_data_PS2068_P8p_SSSS
      {
        mean_h2_data_PS2068_P8p_SSSS <- rbind(mean(h2_data_PS2068_CONTROL_P8p_SSSS_mod),mean(h2_data_PS2068_MA_P8p_SSSS_mod),mean(h2_data_PS2068_Vm_P8p_SSSS_mod))
        colnames(mean_h2_data_PS2068_P8p_SSSS) <- c("mean")
        median_h2_data_PS2068_P8p_SSSS <- rbind(median(h2_data_PS2068_CONTROL_P8p_SSSS_mod),median(h2_data_PS2068_MA_P8p_SSSS_mod),median(h2_data_PS2068_Vm_P8p_SSSS_mod))
        colnames(median_h2_data_PS2068_P8p_SSSS) <- c("median")
        posterior.mode_h2_data_PS2068_P8p_SSSS <- rbind(posterior.mode(as.mcmc(h2_data_PS2068_CONTROL_P8p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_PS2068_MA_P8p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_PS2068_Vm_P8p_SSSS_mod)))
        colnames(posterior.mode_h2_data_PS2068_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_PS2068_P8p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_PS2068_CONTROL_P8p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_PS2068_MA_P8p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_PS2068_Vm_P8p_SSSS_mod)))
        colnames(HPDinterval_0.95_h2_data_PS2068_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_PS2068_P8p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_PS2068_CONTROL_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_PS2068_MA_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_PS2068_Vm_P8p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_PS2068_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_PS2068_P8p_SSSS <- rbind(effectiveSize(h2_data_PS2068_CONTROL_P8p_SSSS_mod),effectiveSize(h2_data_PS2068_MA_P8p_SSSS_mod),effectiveSize(h2_data_PS2068_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_h2_data_PS2068_P8p_SSSS) <- c("effectiveSize")
        h2_data_PS2068_P8p_SSSS <- cbind.data.frame(mean_h2_data_PS2068_P8p_SSSS,median_h2_data_PS2068_P8p_SSSS,posterior.mode_h2_data_PS2068_P8p_SSSS,HPDinterval_0.95_h2_data_PS2068_P8p_SSSS,HPDinterval_0.83_h2_data_PS2068_P8p_SSSS,effectiveSize_h2_data_PS2068_P8p_SSSS)
        rownames(h2_data_PS2068_P8p_SSSS) <- c("h2_data_PS2068_CONTROL_P8p_SSSS_mod","h2_data_PS2068_MA_P8p_SSSS_mod","h2_data_PS2068_Vm_P8p_SSSS_mod")
        h2_data_PS2068_P8p_SSSS <- cbind(Models = rownames(h2_data_PS2068_P8p_SSSS),h2_data_PS2068_P8p_SSSS)
        rownames(h2_data_PS2068_P8p_SSSS) <- NULL
        h2_data_PS2068_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        h2_data_PS2068_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_data_PS2068_P8p_SSSS$Measure <- c("H2","H2","H2")
        h2_data_PS2068_P8p_SSSS$Scale <- c("data","data","data")
        h2_data_PS2068_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_data_PS2068_P8p_SSSS
      }
      
      #Summary Evol_data_PS2068_P8p_SSSS
      {
        mean_Evol_data_PS2068_P8p_SSSS <- rbind(mean(Evol_data_PS2068_CONTROL_P8p_SSSS_mod),mean(Evol_data_PS2068_MA_P8p_SSSS_mod),mean(Evol_data_PS2068_Vm_P8p_SSSS_mod))
        colnames(mean_Evol_data_PS2068_P8p_SSSS) <- c("mean")
        median_Evol_data_PS2068_P8p_SSSS <- rbind(median(Evol_data_PS2068_CONTROL_P8p_SSSS_mod),median(Evol_data_PS2068_MA_P8p_SSSS_mod),median(Evol_data_PS2068_Vm_P8p_SSSS_mod))
        colnames(median_Evol_data_PS2068_P8p_SSSS) <- c("median")
        posterior.mode_Evol_data_PS2068_P8p_SSSS <- rbind(posterior.mode(as.mcmc(Evol_data_PS2068_CONTROL_P8p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_PS2068_MA_P8p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_PS2068_Vm_P8p_SSSS_mod)))
        colnames(posterior.mode_Evol_data_PS2068_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_PS2068_P8p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_PS2068_CONTROL_P8p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_PS2068_MA_P8p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_PS2068_Vm_P8p_SSSS_mod)))
        colnames(HPDinterval_0.95_Evol_data_PS2068_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_PS2068_P8p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_PS2068_CONTROL_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_PS2068_MA_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_PS2068_Vm_P8p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_PS2068_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_PS2068_P8p_SSSS <- rbind(effectiveSize(Evol_data_PS2068_CONTROL_P8p_SSSS_mod),effectiveSize(Evol_data_PS2068_MA_P8p_SSSS_mod),effectiveSize(Evol_data_PS2068_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_Evol_data_PS2068_P8p_SSSS) <- c("effectiveSize")
        Evol_data_PS2068_P8p_SSSS <- cbind.data.frame(mean_Evol_data_PS2068_P8p_SSSS,median_Evol_data_PS2068_P8p_SSSS,posterior.mode_Evol_data_PS2068_P8p_SSSS,HPDinterval_0.95_Evol_data_PS2068_P8p_SSSS,HPDinterval_0.83_Evol_data_PS2068_P8p_SSSS,effectiveSize_Evol_data_PS2068_P8p_SSSS)
        rownames(Evol_data_PS2068_P8p_SSSS) <- c("Evol_data_PS2068_CONTROL_P8p_SSSS_mod","Evol_data_PS2068_MA_P8p_SSSS_mod","Evol_data_PS2068_Vm_P8p_SSSS_mod")
        Evol_data_PS2068_P8p_SSSS <- cbind(Models = rownames(Evol_data_PS2068_P8p_SSSS),Evol_data_PS2068_P8p_SSSS)
        rownames(Evol_data_PS2068_P8p_SSSS) <- NULL
        Evol_data_PS2068_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        Evol_data_PS2068_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_data_PS2068_P8p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_data_PS2068_P8p_SSSS$Scale <- c("data","data","data")
        Evol_data_PS2068_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_data_PS2068_P8p_SSSS
      }
      
      #Summary trait_mean_data_PS2068_P8p_SSSS
      {
        mean_trait_mean_data_PS2068_P8p_SSSS <- rbind(mean(trait_mean_data_PS2068_CONTROL_P8p_SSSS_mod),mean(trait_mean_data_PS2068_MA_P8p_SSSS_mod),mean(trait_mean_data_PS2068_Vm_P8p_SSSS_mod))
        colnames(mean_trait_mean_data_PS2068_P8p_SSSS) <- c("mean")
        median_trait_mean_data_PS2068_P8p_SSSS <- rbind(median(trait_mean_data_PS2068_CONTROL_P8p_SSSS_mod),median(trait_mean_data_PS2068_MA_P8p_SSSS_mod),median(trait_mean_data_PS2068_Vm_P8p_SSSS_mod))
        colnames(median_trait_mean_data_PS2068_P8p_SSSS) <- c("median")
        posterior.mode_trait_mean_data_PS2068_P8p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_data_PS2068_CONTROL_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_PS2068_MA_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_PS2068_Vm_P8p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_data_PS2068_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_PS2068_P8p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_PS2068_CONTROL_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_PS2068_MA_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_PS2068_Vm_P8p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_PS2068_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_PS2068_P8p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_PS2068_CONTROL_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_PS2068_MA_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_PS2068_Vm_P8p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_PS2068_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_PS2068_P8p_SSSS <- rbind(effectiveSize(trait_mean_data_PS2068_CONTROL_P8p_SSSS_mod),effectiveSize(trait_mean_data_PS2068_MA_P8p_SSSS_mod),effectiveSize(trait_mean_data_PS2068_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_trait_mean_data_PS2068_P8p_SSSS) <- c("effectiveSize")
        trait_mean_data_PS2068_P8p_SSSS <- cbind.data.frame(mean_trait_mean_data_PS2068_P8p_SSSS,median_trait_mean_data_PS2068_P8p_SSSS,posterior.mode_trait_mean_data_PS2068_P8p_SSSS,HPDinterval_0.95_trait_mean_data_PS2068_P8p_SSSS,HPDinterval_0.83_trait_mean_data_PS2068_P8p_SSSS,effectiveSize_trait_mean_data_PS2068_P8p_SSSS)
        rownames(trait_mean_data_PS2068_P8p_SSSS) <- c("trait_mean_data_PS2068_CONTROL_P8p_SSSS_mod","trait_mean_data_PS2068_MA_P8p_SSSS_mod","trait_mean_data_PS2068_Vm_P8p_SSSS_mod")
        trait_mean_data_PS2068_P8p_SSSS <- cbind(Models = rownames(trait_mean_data_PS2068_P8p_SSSS),trait_mean_data_PS2068_P8p_SSSS)
        rownames(trait_mean_data_PS2068_P8p_SSSS) <- NULL
        trait_mean_data_PS2068_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        trait_mean_data_PS2068_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_data_PS2068_P8p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_PS2068_P8p_SSSS$Scale <- c("data","data","data")
        trait_mean_data_PS2068_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_PS2068_P8p_SSSS
      }
      
      data_PS2068_P8p_SSSS <- rbind.data.frame(va_data_PS2068_P8p_SSSS, h2_data_PS2068_P8p_SSSS,Evol_data_PS2068_P8p_SSSS,trait_mean_data_PS2068_P8p_SSSS)
      data_PS2068_P8p_SSSS
      
    }
    Vm_PS2068_P8p_SSSS <- rbind.data.frame(liab_PS2068_P8p_SSSS, data_PS2068_P8p_SSSS)
    Vm_PS2068_P8p_SSSS$Pnp_fate <- rep("SSSS", 24)
    Vm_PS2068_P8p_SSSS
    #remove PS2068 P8p_SSSSS models
    {
      remove(PS2068_CONTROL_P8p_SSSS_mod)
      remove(PS2068_MA_P8p_SSSS_mod)
      remove(PS2068_Vm_P8p_SSSS_mod)
    }
  }
  
  ##Summary PS2068 P5p----
  {
    #Summary liability scale PS2068 P5p
    {
      #Summary va_liab_PS2068_P5p_wt: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_PS2068_P5p_wt <- rbind(mean(va_liab_PS2068_CONTROL_P5p_wt_mod/2),mean(va_liab_PS2068_MA_P5p_wt_mod/2),mean(va_liab_PS2068_Vm_P5p_wt_mod/2))
        colnames(mean_va_liab_PS2068_P5p_wt) <- c("mean")
        median_va_liab_PS2068_P5p_wt <- rbind(median(va_liab_PS2068_CONTROL_P5p_wt_mod/2),median(va_liab_PS2068_MA_P5p_wt_mod/2),median(va_liab_PS2068_Vm_P5p_wt_mod/2))
        colnames(median_va_liab_PS2068_P5p_wt) <- c("median")
        posterior.mode_va_liab_PS2068_P5p_wt <- rbind(posterior.mode(va_liab_PS2068_CONTROL_P5p_wt_mod/2),posterior.mode(va_liab_PS2068_MA_P5p_wt_mod/2),posterior.mode(va_liab_PS2068_Vm_P5p_wt_mod/2))
        colnames(posterior.mode_va_liab_PS2068_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_PS2068_P5p_wt <- rbind(HPDinterval(va_liab_PS2068_CONTROL_P5p_wt_mod/2),HPDinterval(va_liab_PS2068_MA_P5p_wt_mod/2),HPDinterval(va_liab_PS2068_Vm_P5p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_PS2068_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_PS2068_P5p_wt <- rbind(HPDinterval(va_liab_PS2068_CONTROL_P5p_wt_mod/2,prob=.83),HPDinterval(va_liab_PS2068_MA_P5p_wt_mod/2,prob=.83),HPDinterval(va_liab_PS2068_Vm_P5p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_PS2068_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_PS2068_P5p_wt <- rbind(effectiveSize(va_liab_PS2068_CONTROL_P5p_wt_mod/2),effectiveSize(va_liab_PS2068_MA_P5p_wt_mod/2),effectiveSize(va_liab_PS2068_Vm_P5p_wt_mod/2))
        colnames(effectiveSize_va_liab_PS2068_P5p_wt) <- c("effectiveSize")
        va_liab_PS2068_P5p_wt <- cbind.data.frame(mean_va_liab_PS2068_P5p_wt,median_va_liab_PS2068_P5p_wt,posterior.mode_va_liab_PS2068_P5p_wt,HPDinterval_0.95_va_liab_PS2068_P5p_wt,HPDinterval_0.83_va_liab_PS2068_P5p_wt,effectiveSize_va_liab_PS2068_P5p_wt)
        rownames(va_liab_PS2068_P5p_wt) <- c("va_liab_PS2068_CONTROL_P5p_wt_mod","va_liab_PS2068_MA_P5p_wt_mod","va_liab_PS2068_Vm_P5p_wt_mod")
        va_liab_PS2068_P5p_wt <- cbind(Models = rownames(va_liab_PS2068_P5p_wt),va_liab_PS2068_P5p_wt)
        rownames(va_liab_PS2068_P5p_wt) <- NULL
        va_liab_PS2068_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        va_liab_PS2068_P5p_wt$Treatment <- c("Control","ML","Vm")
        va_liab_PS2068_P5p_wt$Measure <- c("Va","Va","Va")
        va_liab_PS2068_P5p_wt$Scale <- c("liab","liab","liab")
        va_liab_PS2068_P5p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_PS2068_P5p_wt
      }
      
      #Summary h2_liab_PS2068_P5p_wt
      {
        mean_h2_liab_PS2068_P5p_wt <- rbind(mean(h2_liab_PS2068_CONTROL_P5p_wt_mod),mean(h2_liab_PS2068_MA_P5p_wt_mod),mean(h2_liab_PS2068_Vm_P5p_wt_mod))
        colnames(mean_h2_liab_PS2068_P5p_wt) <- c("mean")
        median_h2_liab_PS2068_P5p_wt <- rbind(median(h2_liab_PS2068_CONTROL_P5p_wt_mod),median(h2_liab_PS2068_MA_P5p_wt_mod),median(h2_liab_PS2068_Vm_P5p_wt_mod))
        colnames(median_h2_liab_PS2068_P5p_wt) <- c("median")
        posterior.mode_h2_liab_PS2068_P5p_wt <- rbind(posterior.mode(h2_liab_PS2068_CONTROL_P5p_wt_mod),posterior.mode(h2_liab_PS2068_MA_P5p_wt_mod),posterior.mode(h2_liab_PS2068_Vm_P5p_wt_mod))
        colnames(posterior.mode_h2_liab_PS2068_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_PS2068_P5p_wt <- rbind(HPDinterval(h2_liab_PS2068_CONTROL_P5p_wt_mod),HPDinterval(h2_liab_PS2068_MA_P5p_wt_mod),HPDinterval(h2_liab_PS2068_Vm_P5p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_PS2068_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_PS2068_P5p_wt <- rbind(HPDinterval(h2_liab_PS2068_CONTROL_P5p_wt_mod,prob=.83),HPDinterval(h2_liab_PS2068_MA_P5p_wt_mod,prob=.83),HPDinterval(h2_liab_PS2068_Vm_P5p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_PS2068_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_PS2068_P5p_wt <- rbind(effectiveSize(h2_liab_PS2068_CONTROL_P5p_wt_mod),effectiveSize(h2_liab_PS2068_MA_P5p_wt_mod),effectiveSize(h2_liab_PS2068_Vm_P5p_wt_mod))
        colnames(effectiveSize_h2_liab_PS2068_P5p_wt) <- c("effectiveSize")
        h2_liab_PS2068_P5p_wt <- cbind.data.frame(mean_h2_liab_PS2068_P5p_wt,median_h2_liab_PS2068_P5p_wt,posterior.mode_h2_liab_PS2068_P5p_wt,HPDinterval_0.95_h2_liab_PS2068_P5p_wt,HPDinterval_0.83_h2_liab_PS2068_P5p_wt,effectiveSize_h2_liab_PS2068_P5p_wt)
        rownames(h2_liab_PS2068_P5p_wt) <- c("h2_liab_PS2068_CONTROL_P5p_wt_mod","h2_liab_PS2068_MA_P5p_wt_mod","h2_liab_PS2068_Vm_P5p_wt_mod")
        h2_liab_PS2068_P5p_wt <- cbind(Models = rownames(h2_liab_PS2068_P5p_wt),h2_liab_PS2068_P5p_wt)
        rownames(h2_liab_PS2068_P5p_wt) <- NULL
        h2_liab_PS2068_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        h2_liab_PS2068_P5p_wt$Treatment <- c("Control","ML","Vm")
        h2_liab_PS2068_P5p_wt$Measure <- c("H2","H2","H2")
        h2_liab_PS2068_P5p_wt$Scale <- c("liab","liab","liab")
        h2_liab_PS2068_P5p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_PS2068_P5p_wt
      }
      
      #Summary Evol_liab_PS2068_P5p_wt
      {
        mean_Evol_liab_PS2068_P5p_wt <- rbind(mean(Evol_liab_PS2068_CONTROL_P5p_wt_mod),mean(Evol_liab_PS2068_MA_P5p_wt_mod),mean(Evol_liab_PS2068_Vm_P5p_wt_mod))
        colnames(mean_Evol_liab_PS2068_P5p_wt) <- c("mean")
        median_Evol_liab_PS2068_P5p_wt <- rbind(median(Evol_liab_PS2068_CONTROL_P5p_wt_mod),median(Evol_liab_PS2068_MA_P5p_wt_mod),median(Evol_liab_PS2068_Vm_P5p_wt_mod))
        colnames(median_Evol_liab_PS2068_P5p_wt) <- c("median")
        posterior.mode_Evol_liab_PS2068_P5p_wt <- rbind(posterior.mode(Evol_liab_PS2068_CONTROL_P5p_wt_mod),posterior.mode(Evol_liab_PS2068_MA_P5p_wt_mod),posterior.mode(Evol_liab_PS2068_Vm_P5p_wt_mod))
        colnames(posterior.mode_Evol_liab_PS2068_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_PS2068_P5p_wt <- rbind(HPDinterval(Evol_liab_PS2068_CONTROL_P5p_wt_mod),HPDinterval(Evol_liab_PS2068_MA_P5p_wt_mod),HPDinterval(Evol_liab_PS2068_Vm_P5p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_PS2068_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_PS2068_P5p_wt <- rbind(HPDinterval(Evol_liab_PS2068_CONTROL_P5p_wt_mod,prob=.83),HPDinterval(Evol_liab_PS2068_MA_P5p_wt_mod,prob=.83),HPDinterval(Evol_liab_PS2068_Vm_P5p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_PS2068_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_PS2068_P5p_wt <- rbind(effectiveSize(Evol_liab_PS2068_CONTROL_P5p_wt_mod),effectiveSize(Evol_liab_PS2068_MA_P5p_wt_mod),effectiveSize(Evol_liab_PS2068_Vm_P5p_wt_mod))
        colnames(effectiveSize_Evol_liab_PS2068_P5p_wt) <- c("effectiveSize")
        Evol_liab_PS2068_P5p_wt <- cbind.data.frame(mean_Evol_liab_PS2068_P5p_wt,median_Evol_liab_PS2068_P5p_wt,posterior.mode_Evol_liab_PS2068_P5p_wt,HPDinterval_0.95_Evol_liab_PS2068_P5p_wt,HPDinterval_0.83_Evol_liab_PS2068_P5p_wt,effectiveSize_Evol_liab_PS2068_P5p_wt)
        rownames(Evol_liab_PS2068_P5p_wt) <- c("Evol_liab_PS2068_CONTROL_P5p_wt_mod","Evol_liab_PS2068_MA_P5p_wt_mod","Evol_liab_PS2068_Vm_P5p_wt_mod")
        Evol_liab_PS2068_P5p_wt <- cbind(Models = rownames(Evol_liab_PS2068_P5p_wt),Evol_liab_PS2068_P5p_wt)
        rownames(Evol_liab_PS2068_P5p_wt) <- NULL
        Evol_liab_PS2068_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        Evol_liab_PS2068_P5p_wt$Treatment <- c("Control","ML","Vm")
        Evol_liab_PS2068_P5p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_PS2068_P5p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_PS2068_P5p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_PS2068_P5p_wt
      }
      
      #Summary trait_mean_liab_PS2068_P5p_wt
      {
        mean_trait_mean_liab_PS2068_P5p_wt <- rbind(mean(trait_mean_liab_PS2068_CONTROL_P5p_wt_mod),mean(trait_mean_liab_PS2068_MA_P5p_wt_mod),mean(trait_mean_liab_PS2068_Vm_P5p_wt_mod))
        colnames(mean_trait_mean_liab_PS2068_P5p_wt) <- c("mean")
        median_trait_mean_liab_PS2068_P5p_wt <- rbind(median(trait_mean_liab_PS2068_CONTROL_P5p_wt_mod),median(trait_mean_liab_PS2068_MA_P5p_wt_mod),median(trait_mean_liab_PS2068_Vm_P5p_wt_mod))
        colnames(median_trait_mean_liab_PS2068_P5p_wt) <- c("median")
        posterior.mode_trait_mean_liab_PS2068_P5p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_PS2068_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_PS2068_MA_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_PS2068_Vm_P5p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_PS2068_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_PS2068_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_PS2068_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_PS2068_MA_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_PS2068_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_PS2068_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_PS2068_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_PS2068_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_PS2068_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_PS2068_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_PS2068_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_PS2068_P5p_wt <- rbind(effectiveSize(trait_mean_liab_PS2068_CONTROL_P5p_wt_mod),effectiveSize(trait_mean_liab_PS2068_MA_P5p_wt_mod),effectiveSize(trait_mean_liab_PS2068_Vm_P5p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_PS2068_P5p_wt) <- c("effectiveSize")
        trait_mean_liab_PS2068_P5p_wt <- cbind.data.frame(mean_trait_mean_liab_PS2068_P5p_wt,median_trait_mean_liab_PS2068_P5p_wt,posterior.mode_trait_mean_liab_PS2068_P5p_wt,HPDinterval_0.95_trait_mean_liab_PS2068_P5p_wt,HPDinterval_0.83_trait_mean_liab_PS2068_P5p_wt,effectiveSize_trait_mean_liab_PS2068_P5p_wt)
        rownames(trait_mean_liab_PS2068_P5p_wt) <- c("trait_mean_liab_PS2068_CONTROL_P5p_wt_mod","trait_mean_liab_PS2068_MA_P5p_wt_mod","trait_mean_liab_PS2068_Vm_P5p_wt_mod")
        trait_mean_liab_PS2068_P5p_wt <- cbind(Models = rownames(trait_mean_liab_PS2068_P5p_wt),trait_mean_liab_PS2068_P5p_wt)
        rownames(trait_mean_liab_PS2068_P5p_wt) <- NULL
        trait_mean_liab_PS2068_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        trait_mean_liab_PS2068_P5p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_PS2068_P5p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_PS2068_P5p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_PS2068_P5p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_PS2068_P5p_wt
      }
      
      liab_PS2068_P5p_wt <- rbind.data.frame(va_liab_PS2068_P5p_wt, h2_liab_PS2068_P5p_wt,Evol_liab_PS2068_P5p_wt,trait_mean_liab_PS2068_P5p_wt)
      liab_PS2068_P5p_wt
    }
    #Summary data scale PS2068 P5p
    {
      #Summary va_data_PS2068_P5p_wt:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_PS2068_P5p_wt <- rbind(mean(va_data_PS2068_CONTROL_P5p_wt_mod/2),mean(va_data_PS2068_MA_P5p_wt_mod/2),mean(va_data_PS2068_Vm_P5p_wt_mod/2))
        colnames(mean_va_data_PS2068_P5p_wt) <- c("mean")
        median_va_data_PS2068_P5p_wt <- rbind(median(va_data_PS2068_CONTROL_P5p_wt_mod/2),median(va_data_PS2068_MA_P5p_wt_mod/2),median(va_data_PS2068_Vm_P5p_wt_mod/2))
        colnames(median_va_data_PS2068_P5p_wt) <- c("median")
        posterior.mode_va_data_PS2068_P5p_wt <- rbind(posterior.mode(as.mcmc(va_data_PS2068_CONTROL_P5p_wt_mod/2)),posterior.mode(as.mcmc(va_data_PS2068_MA_P5p_wt_mod/2)),posterior.mode(as.mcmc(va_data_PS2068_Vm_P5p_wt_mod/2)))
        colnames(posterior.mode_va_data_PS2068_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_PS2068_P5p_wt <- rbind(HPDinterval(as.mcmc(va_data_PS2068_CONTROL_P5p_wt_mod/2)),HPDinterval(as.mcmc(va_data_PS2068_MA_P5p_wt_mod/2)),HPDinterval(as.mcmc(va_data_PS2068_Vm_P5p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_PS2068_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_PS2068_P5p_wt <- rbind(HPDinterval(as.mcmc(va_data_PS2068_CONTROL_P5p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_PS2068_MA_P5p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_PS2068_Vm_P5p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_PS2068_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_PS2068_P5p_wt <- rbind(effectiveSize(va_data_PS2068_CONTROL_P5p_wt_mod/2),effectiveSize(va_data_PS2068_MA_P5p_wt_mod/2),effectiveSize(va_data_PS2068_Vm_P5p_wt_mod/2))
        colnames(effectiveSize_va_data_PS2068_P5p_wt) <- c("effectiveSize")
        va_data_PS2068_P5p_wt <- cbind.data.frame(mean_va_data_PS2068_P5p_wt,median_va_data_PS2068_P5p_wt,posterior.mode_va_data_PS2068_P5p_wt,HPDinterval_0.95_va_data_PS2068_P5p_wt,HPDinterval_0.83_va_data_PS2068_P5p_wt,effectiveSize_va_data_PS2068_P5p_wt)
        rownames(va_data_PS2068_P5p_wt) <- c("va_data_PS2068_CONTROL_P5p_wt_mod","va_data_PS2068_MA_P5p_wt_mod","va_data_PS2068_Vm_P5p_wt_mod")
        va_data_PS2068_P5p_wt <- cbind(Models = rownames(va_data_PS2068_P5p_wt),va_data_PS2068_P5p_wt)
        rownames(va_data_PS2068_P5p_wt) <- NULL
        va_data_PS2068_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        va_data_PS2068_P5p_wt$Treatment <- c("Control","ML","Vm")
        va_data_PS2068_P5p_wt$Measure <- c("Va","Va","Va")
        va_data_PS2068_P5p_wt$Scale <- c("data","data","data")
        va_data_PS2068_P5p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_PS2068_P5p_wt
      }
      
      #Summary h2_data_PS2068_P5p_wt
      {
        mean_h2_data_PS2068_P5p_wt <- rbind(mean(h2_data_PS2068_CONTROL_P5p_wt_mod),mean(h2_data_PS2068_MA_P5p_wt_mod),mean(h2_data_PS2068_Vm_P5p_wt_mod))
        colnames(mean_h2_data_PS2068_P5p_wt) <- c("mean")
        median_h2_data_PS2068_P5p_wt <- rbind(median(h2_data_PS2068_CONTROL_P5p_wt_mod),median(h2_data_PS2068_MA_P5p_wt_mod),median(h2_data_PS2068_Vm_P5p_wt_mod))
        colnames(median_h2_data_PS2068_P5p_wt) <- c("median")
        posterior.mode_h2_data_PS2068_P5p_wt <- rbind(posterior.mode(as.mcmc(h2_data_PS2068_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(h2_data_PS2068_MA_P5p_wt_mod)),posterior.mode(as.mcmc(h2_data_PS2068_Vm_P5p_wt_mod)))
        colnames(posterior.mode_h2_data_PS2068_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_PS2068_P5p_wt <- rbind(HPDinterval(as.mcmc(h2_data_PS2068_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(h2_data_PS2068_MA_P5p_wt_mod)),HPDinterval(as.mcmc(h2_data_PS2068_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_PS2068_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_PS2068_P5p_wt <- rbind(HPDinterval(as.mcmc(h2_data_PS2068_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_PS2068_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_PS2068_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_PS2068_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_PS2068_P5p_wt <- rbind(effectiveSize(h2_data_PS2068_CONTROL_P5p_wt_mod),effectiveSize(h2_data_PS2068_MA_P5p_wt_mod),effectiveSize(h2_data_PS2068_Vm_P5p_wt_mod))
        colnames(effectiveSize_h2_data_PS2068_P5p_wt) <- c("effectiveSize")
        h2_data_PS2068_P5p_wt <- cbind.data.frame(mean_h2_data_PS2068_P5p_wt,median_h2_data_PS2068_P5p_wt,posterior.mode_h2_data_PS2068_P5p_wt,HPDinterval_0.95_h2_data_PS2068_P5p_wt,HPDinterval_0.83_h2_data_PS2068_P5p_wt,effectiveSize_h2_data_PS2068_P5p_wt)
        rownames(h2_data_PS2068_P5p_wt) <- c("h2_data_PS2068_CONTROL_P5p_wt_mod","h2_data_PS2068_MA_P5p_wt_mod","h2_data_PS2068_Vm_P5p_wt_mod")
        h2_data_PS2068_P5p_wt <- cbind(Models = rownames(h2_data_PS2068_P5p_wt),h2_data_PS2068_P5p_wt)
        rownames(h2_data_PS2068_P5p_wt) <- NULL
        h2_data_PS2068_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        h2_data_PS2068_P5p_wt$Treatment <- c("Control","ML","Vm")
        h2_data_PS2068_P5p_wt$Measure <- c("H2","H2","H2")
        h2_data_PS2068_P5p_wt$Scale <- c("data","data","data")
        h2_data_PS2068_P5p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_PS2068_P5p_wt
      }
      
      #Summary Evol_data_PS2068_P5p_wt
      {
        mean_Evol_data_PS2068_P5p_wt <- rbind(mean(Evol_data_PS2068_CONTROL_P5p_wt_mod),mean(Evol_data_PS2068_MA_P5p_wt_mod),mean(Evol_data_PS2068_Vm_P5p_wt_mod))
        colnames(mean_Evol_data_PS2068_P5p_wt) <- c("mean")
        median_Evol_data_PS2068_P5p_wt <- rbind(median(Evol_data_PS2068_CONTROL_P5p_wt_mod),median(Evol_data_PS2068_MA_P5p_wt_mod),median(Evol_data_PS2068_Vm_P5p_wt_mod))
        colnames(median_Evol_data_PS2068_P5p_wt) <- c("median")
        posterior.mode_Evol_data_PS2068_P5p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_PS2068_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(Evol_data_PS2068_MA_P5p_wt_mod)),posterior.mode(as.mcmc(Evol_data_PS2068_Vm_P5p_wt_mod)))
        colnames(posterior.mode_Evol_data_PS2068_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_PS2068_P5p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_PS2068_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(Evol_data_PS2068_MA_P5p_wt_mod)),HPDinterval(as.mcmc(Evol_data_PS2068_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_PS2068_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_PS2068_P5p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_PS2068_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_PS2068_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_PS2068_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_PS2068_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_PS2068_P5p_wt <- rbind(effectiveSize(Evol_data_PS2068_CONTROL_P5p_wt_mod),effectiveSize(Evol_data_PS2068_MA_P5p_wt_mod),effectiveSize(Evol_data_PS2068_Vm_P5p_wt_mod))
        colnames(effectiveSize_Evol_data_PS2068_P5p_wt) <- c("effectiveSize")
        Evol_data_PS2068_P5p_wt <- cbind.data.frame(mean_Evol_data_PS2068_P5p_wt,median_Evol_data_PS2068_P5p_wt,posterior.mode_Evol_data_PS2068_P5p_wt,HPDinterval_0.95_Evol_data_PS2068_P5p_wt,HPDinterval_0.83_Evol_data_PS2068_P5p_wt,effectiveSize_Evol_data_PS2068_P5p_wt)
        rownames(Evol_data_PS2068_P5p_wt) <- c("Evol_data_PS2068_CONTROL_P5p_wt_mod","Evol_data_PS2068_MA_P5p_wt_mod","Evol_data_PS2068_Vm_P5p_wt_mod")
        Evol_data_PS2068_P5p_wt <- cbind(Models = rownames(Evol_data_PS2068_P5p_wt),Evol_data_PS2068_P5p_wt)
        rownames(Evol_data_PS2068_P5p_wt) <- NULL
        Evol_data_PS2068_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        Evol_data_PS2068_P5p_wt$Treatment <- c("Control","ML","Vm")
        Evol_data_PS2068_P5p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_PS2068_P5p_wt$Scale <- c("data","data","data")
        Evol_data_PS2068_P5p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_PS2068_P5p_wt
      }
      
      #Summary trait_mean_data_PS2068_P5p_wt
      {
        mean_trait_mean_data_PS2068_P5p_wt <- rbind(mean(trait_mean_data_PS2068_CONTROL_P5p_wt_mod),mean(trait_mean_data_PS2068_MA_P5p_wt_mod),mean(trait_mean_data_PS2068_Vm_P5p_wt_mod))
        colnames(mean_trait_mean_data_PS2068_P5p_wt) <- c("mean")
        median_trait_mean_data_PS2068_P5p_wt <- rbind(median(trait_mean_data_PS2068_CONTROL_P5p_wt_mod),median(trait_mean_data_PS2068_MA_P5p_wt_mod),median(trait_mean_data_PS2068_Vm_P5p_wt_mod))
        colnames(median_trait_mean_data_PS2068_P5p_wt) <- c("median")
        posterior.mode_trait_mean_data_PS2068_P5p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_PS2068_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_PS2068_MA_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_PS2068_Vm_P5p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_PS2068_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_PS2068_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_PS2068_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_PS2068_MA_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_PS2068_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_PS2068_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_PS2068_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_PS2068_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_PS2068_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_PS2068_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_PS2068_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_PS2068_P5p_wt <- rbind(effectiveSize(trait_mean_data_PS2068_CONTROL_P5p_wt_mod),effectiveSize(trait_mean_data_PS2068_MA_P5p_wt_mod),effectiveSize(trait_mean_data_PS2068_Vm_P5p_wt_mod))
        colnames(effectiveSize_trait_mean_data_PS2068_P5p_wt) <- c("effectiveSize")
        trait_mean_data_PS2068_P5p_wt <- cbind.data.frame(mean_trait_mean_data_PS2068_P5p_wt,median_trait_mean_data_PS2068_P5p_wt,posterior.mode_trait_mean_data_PS2068_P5p_wt,HPDinterval_0.95_trait_mean_data_PS2068_P5p_wt,HPDinterval_0.83_trait_mean_data_PS2068_P5p_wt,effectiveSize_trait_mean_data_PS2068_P5p_wt)
        rownames(trait_mean_data_PS2068_P5p_wt) <- c("trait_mean_data_PS2068_CONTROL_P5p_wt_mod","trait_mean_data_PS2068_MA_P5p_wt_mod","trait_mean_data_PS2068_Vm_P5p_wt_mod")
        trait_mean_data_PS2068_P5p_wt <- cbind(Models = rownames(trait_mean_data_PS2068_P5p_wt),trait_mean_data_PS2068_P5p_wt)
        rownames(trait_mean_data_PS2068_P5p_wt) <- NULL
        trait_mean_data_PS2068_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        trait_mean_data_PS2068_P5p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_data_PS2068_P5p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_PS2068_P5p_wt$Scale <- c("data","data","data")
        trait_mean_data_PS2068_P5p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_PS2068_P5p_wt
      }
      
      data_PS2068_P5p_wt <- rbind.data.frame(va_data_PS2068_P5p_wt, h2_data_PS2068_P5p_wt,Evol_data_PS2068_P5p_wt,trait_mean_data_PS2068_P5p_wt)
      data_PS2068_P5p_wt
      
    }
    Vm_PS2068_P5p_wt <- rbind.data.frame(liab_PS2068_P5p_wt, data_PS2068_P5p_wt)
    Vm_PS2068_P5p_wt$Pnp_fate <- rep("wt", 24)
    Vm_PS2068_P5p_wt
    #remove PS2068 P5p_wt models
    {
      remove(PS2068_CONTROL_P5p_wt_mod)
      remove(PS2068_MA_P5p_wt_mod)
      remove(PS2068_Vm_P5p_wt_mod)
    }
  }
  
  ##Summary PS2068 P6p----
  {
    #Summary liability scale PS2068 P6p
    {
      #Summary va_liab_PS2068_P6p_wt: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_PS2068_P6p_wt <- rbind(mean(va_liab_PS2068_CONTROL_P6p_wt_mod/2),mean(va_liab_PS2068_MA_P6p_wt_mod/2),mean(va_liab_PS2068_Vm_P6p_wt_mod/2))
        colnames(mean_va_liab_PS2068_P6p_wt) <- c("mean")
        median_va_liab_PS2068_P6p_wt <- rbind(median(va_liab_PS2068_CONTROL_P6p_wt_mod/2),median(va_liab_PS2068_MA_P6p_wt_mod/2),median(va_liab_PS2068_Vm_P6p_wt_mod/2))
        colnames(median_va_liab_PS2068_P6p_wt) <- c("median")
        posterior.mode_va_liab_PS2068_P6p_wt <- rbind(posterior.mode(va_liab_PS2068_CONTROL_P6p_wt_mod/2),posterior.mode(va_liab_PS2068_MA_P6p_wt_mod/2),posterior.mode(va_liab_PS2068_Vm_P6p_wt_mod/2))
        colnames(posterior.mode_va_liab_PS2068_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_PS2068_P6p_wt <- rbind(HPDinterval(va_liab_PS2068_CONTROL_P6p_wt_mod/2),HPDinterval(va_liab_PS2068_MA_P6p_wt_mod/2),HPDinterval(va_liab_PS2068_Vm_P6p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_PS2068_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_PS2068_P6p_wt <- rbind(HPDinterval(va_liab_PS2068_CONTROL_P6p_wt_mod/2,prob=.83),HPDinterval(va_liab_PS2068_MA_P6p_wt_mod/2,prob=.83),HPDinterval(va_liab_PS2068_Vm_P6p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_PS2068_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_PS2068_P6p_wt <- rbind(effectiveSize(va_liab_PS2068_CONTROL_P6p_wt_mod/2),effectiveSize(va_liab_PS2068_MA_P6p_wt_mod/2),effectiveSize(va_liab_PS2068_Vm_P6p_wt_mod/2))
        colnames(effectiveSize_va_liab_PS2068_P6p_wt) <- c("effectiveSize")
        va_liab_PS2068_P6p_wt <- cbind.data.frame(mean_va_liab_PS2068_P6p_wt,median_va_liab_PS2068_P6p_wt,posterior.mode_va_liab_PS2068_P6p_wt,HPDinterval_0.95_va_liab_PS2068_P6p_wt,HPDinterval_0.83_va_liab_PS2068_P6p_wt,effectiveSize_va_liab_PS2068_P6p_wt)
        rownames(va_liab_PS2068_P6p_wt) <- c("va_liab_PS2068_CONTROL_P6p_wt_mod","va_liab_PS2068_MA_P6p_wt_mod","va_liab_PS2068_Vm_P6p_wt_mod")
        va_liab_PS2068_P6p_wt <- cbind(Models = rownames(va_liab_PS2068_P6p_wt),va_liab_PS2068_P6p_wt)
        rownames(va_liab_PS2068_P6p_wt) <- NULL
        va_liab_PS2068_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        va_liab_PS2068_P6p_wt$Treatment <- c("Control","ML","Vm")
        va_liab_PS2068_P6p_wt$Measure <- c("Va","Va","Va")
        va_liab_PS2068_P6p_wt$Scale <- c("liab","liab","liab")
        va_liab_PS2068_P6p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_PS2068_P6p_wt
      }
      
      #Summary h2_liab_PS2068_P6p_wt
      {
        mean_h2_liab_PS2068_P6p_wt <- rbind(mean(h2_liab_PS2068_CONTROL_P6p_wt_mod),mean(h2_liab_PS2068_MA_P6p_wt_mod),mean(h2_liab_PS2068_Vm_P6p_wt_mod))
        colnames(mean_h2_liab_PS2068_P6p_wt) <- c("mean")
        median_h2_liab_PS2068_P6p_wt <- rbind(median(h2_liab_PS2068_CONTROL_P6p_wt_mod),median(h2_liab_PS2068_MA_P6p_wt_mod),median(h2_liab_PS2068_Vm_P6p_wt_mod))
        colnames(median_h2_liab_PS2068_P6p_wt) <- c("median")
        posterior.mode_h2_liab_PS2068_P6p_wt <- rbind(posterior.mode(h2_liab_PS2068_CONTROL_P6p_wt_mod),posterior.mode(h2_liab_PS2068_MA_P6p_wt_mod),posterior.mode(h2_liab_PS2068_Vm_P6p_wt_mod))
        colnames(posterior.mode_h2_liab_PS2068_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_PS2068_P6p_wt <- rbind(HPDinterval(h2_liab_PS2068_CONTROL_P6p_wt_mod),HPDinterval(h2_liab_PS2068_MA_P6p_wt_mod),HPDinterval(h2_liab_PS2068_Vm_P6p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_PS2068_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_PS2068_P6p_wt <- rbind(HPDinterval(h2_liab_PS2068_CONTROL_P6p_wt_mod,prob=.83),HPDinterval(h2_liab_PS2068_MA_P6p_wt_mod,prob=.83),HPDinterval(h2_liab_PS2068_Vm_P6p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_PS2068_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_PS2068_P6p_wt <- rbind(effectiveSize(h2_liab_PS2068_CONTROL_P6p_wt_mod),effectiveSize(h2_liab_PS2068_MA_P6p_wt_mod),effectiveSize(h2_liab_PS2068_Vm_P6p_wt_mod))
        colnames(effectiveSize_h2_liab_PS2068_P6p_wt) <- c("effectiveSize")
        h2_liab_PS2068_P6p_wt <- cbind.data.frame(mean_h2_liab_PS2068_P6p_wt,median_h2_liab_PS2068_P6p_wt,posterior.mode_h2_liab_PS2068_P6p_wt,HPDinterval_0.95_h2_liab_PS2068_P6p_wt,HPDinterval_0.83_h2_liab_PS2068_P6p_wt,effectiveSize_h2_liab_PS2068_P6p_wt)
        rownames(h2_liab_PS2068_P6p_wt) <- c("h2_liab_PS2068_CONTROL_P6p_wt_mod","h2_liab_PS2068_MA_P6p_wt_mod","h2_liab_PS2068_Vm_P6p_wt_mod")
        h2_liab_PS2068_P6p_wt <- cbind(Models = rownames(h2_liab_PS2068_P6p_wt),h2_liab_PS2068_P6p_wt)
        rownames(h2_liab_PS2068_P6p_wt) <- NULL
        h2_liab_PS2068_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        h2_liab_PS2068_P6p_wt$Treatment <- c("Control","ML","Vm")
        h2_liab_PS2068_P6p_wt$Measure <- c("H2","H2","H2")
        h2_liab_PS2068_P6p_wt$Scale <- c("liab","liab","liab")
        h2_liab_PS2068_P6p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_PS2068_P6p_wt
      }
      
      #Summary Evol_liab_PS2068_P6p_wt
      {
        mean_Evol_liab_PS2068_P6p_wt <- rbind(mean(Evol_liab_PS2068_CONTROL_P6p_wt_mod),mean(Evol_liab_PS2068_MA_P6p_wt_mod),mean(Evol_liab_PS2068_Vm_P6p_wt_mod))
        colnames(mean_Evol_liab_PS2068_P6p_wt) <- c("mean")
        median_Evol_liab_PS2068_P6p_wt <- rbind(median(Evol_liab_PS2068_CONTROL_P6p_wt_mod),median(Evol_liab_PS2068_MA_P6p_wt_mod),median(Evol_liab_PS2068_Vm_P6p_wt_mod))
        colnames(median_Evol_liab_PS2068_P6p_wt) <- c("median")
        posterior.mode_Evol_liab_PS2068_P6p_wt <- rbind(posterior.mode(Evol_liab_PS2068_CONTROL_P6p_wt_mod),posterior.mode(Evol_liab_PS2068_MA_P6p_wt_mod),posterior.mode(Evol_liab_PS2068_Vm_P6p_wt_mod))
        colnames(posterior.mode_Evol_liab_PS2068_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_PS2068_P6p_wt <- rbind(HPDinterval(Evol_liab_PS2068_CONTROL_P6p_wt_mod),HPDinterval(Evol_liab_PS2068_MA_P6p_wt_mod),HPDinterval(Evol_liab_PS2068_Vm_P6p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_PS2068_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_PS2068_P6p_wt <- rbind(HPDinterval(Evol_liab_PS2068_CONTROL_P6p_wt_mod,prob=.83),HPDinterval(Evol_liab_PS2068_MA_P6p_wt_mod,prob=.83),HPDinterval(Evol_liab_PS2068_Vm_P6p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_PS2068_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_PS2068_P6p_wt <- rbind(effectiveSize(Evol_liab_PS2068_CONTROL_P6p_wt_mod),effectiveSize(Evol_liab_PS2068_MA_P6p_wt_mod),effectiveSize(Evol_liab_PS2068_Vm_P6p_wt_mod))
        colnames(effectiveSize_Evol_liab_PS2068_P6p_wt) <- c("effectiveSize")
        Evol_liab_PS2068_P6p_wt <- cbind.data.frame(mean_Evol_liab_PS2068_P6p_wt,median_Evol_liab_PS2068_P6p_wt,posterior.mode_Evol_liab_PS2068_P6p_wt,HPDinterval_0.95_Evol_liab_PS2068_P6p_wt,HPDinterval_0.83_Evol_liab_PS2068_P6p_wt,effectiveSize_Evol_liab_PS2068_P6p_wt)
        rownames(Evol_liab_PS2068_P6p_wt) <- c("Evol_liab_PS2068_CONTROL_P6p_wt_mod","Evol_liab_PS2068_MA_P6p_wt_mod","Evol_liab_PS2068_Vm_P6p_wt_mod")
        Evol_liab_PS2068_P6p_wt <- cbind(Models = rownames(Evol_liab_PS2068_P6p_wt),Evol_liab_PS2068_P6p_wt)
        rownames(Evol_liab_PS2068_P6p_wt) <- NULL
        Evol_liab_PS2068_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        Evol_liab_PS2068_P6p_wt$Treatment <- c("Control","ML","Vm")
        Evol_liab_PS2068_P6p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_PS2068_P6p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_PS2068_P6p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_PS2068_P6p_wt
      }
      
      #Summary trait_mean_liab_PS2068_P6p_wt
      {
        mean_trait_mean_liab_PS2068_P6p_wt <- rbind(mean(trait_mean_liab_PS2068_CONTROL_P6p_wt_mod),mean(trait_mean_liab_PS2068_MA_P6p_wt_mod),mean(trait_mean_liab_PS2068_Vm_P6p_wt_mod))
        colnames(mean_trait_mean_liab_PS2068_P6p_wt) <- c("mean")
        median_trait_mean_liab_PS2068_P6p_wt <- rbind(median(trait_mean_liab_PS2068_CONTROL_P6p_wt_mod),median(trait_mean_liab_PS2068_MA_P6p_wt_mod),median(trait_mean_liab_PS2068_Vm_P6p_wt_mod))
        colnames(median_trait_mean_liab_PS2068_P6p_wt) <- c("median")
        posterior.mode_trait_mean_liab_PS2068_P6p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_PS2068_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_PS2068_MA_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_PS2068_Vm_P6p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_PS2068_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_PS2068_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_PS2068_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_PS2068_MA_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_PS2068_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_PS2068_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_PS2068_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_PS2068_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_PS2068_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_PS2068_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_PS2068_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_PS2068_P6p_wt <- rbind(effectiveSize(trait_mean_liab_PS2068_CONTROL_P6p_wt_mod),effectiveSize(trait_mean_liab_PS2068_MA_P6p_wt_mod),effectiveSize(trait_mean_liab_PS2068_Vm_P6p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_PS2068_P6p_wt) <- c("effectiveSize")
        trait_mean_liab_PS2068_P6p_wt <- cbind.data.frame(mean_trait_mean_liab_PS2068_P6p_wt,median_trait_mean_liab_PS2068_P6p_wt,posterior.mode_trait_mean_liab_PS2068_P6p_wt,HPDinterval_0.95_trait_mean_liab_PS2068_P6p_wt,HPDinterval_0.83_trait_mean_liab_PS2068_P6p_wt,effectiveSize_trait_mean_liab_PS2068_P6p_wt)
        rownames(trait_mean_liab_PS2068_P6p_wt) <- c("trait_mean_liab_PS2068_CONTROL_P6p_wt_mod","trait_mean_liab_PS2068_MA_P6p_wt_mod","trait_mean_liab_PS2068_Vm_P6p_wt_mod")
        trait_mean_liab_PS2068_P6p_wt <- cbind(Models = rownames(trait_mean_liab_PS2068_P6p_wt),trait_mean_liab_PS2068_P6p_wt)
        rownames(trait_mean_liab_PS2068_P6p_wt) <- NULL
        trait_mean_liab_PS2068_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        trait_mean_liab_PS2068_P6p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_PS2068_P6p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_PS2068_P6p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_PS2068_P6p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_PS2068_P6p_wt
      }
      
      liab_PS2068_P6p_wt <- rbind.data.frame(va_liab_PS2068_P6p_wt, h2_liab_PS2068_P6p_wt,Evol_liab_PS2068_P6p_wt,trait_mean_liab_PS2068_P6p_wt)
      liab_PS2068_P6p_wt
    }
    #Summary data scale PS2068 P6p
    {
      #Summary va_data_PS2068_P6p_wt:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_PS2068_P6p_wt <- rbind(mean(va_data_PS2068_CONTROL_P6p_wt_mod/2),mean(va_data_PS2068_MA_P6p_wt_mod/2),mean(va_data_PS2068_Vm_P6p_wt_mod/2))
        colnames(mean_va_data_PS2068_P6p_wt) <- c("mean")
        median_va_data_PS2068_P6p_wt <- rbind(median(va_data_PS2068_CONTROL_P6p_wt_mod/2),median(va_data_PS2068_MA_P6p_wt_mod/2),median(va_data_PS2068_Vm_P6p_wt_mod/2))
        colnames(median_va_data_PS2068_P6p_wt) <- c("median")
        posterior.mode_va_data_PS2068_P6p_wt <- rbind(posterior.mode(as.mcmc(va_data_PS2068_CONTROL_P6p_wt_mod/2)),posterior.mode(as.mcmc(va_data_PS2068_MA_P6p_wt_mod/2)),posterior.mode(as.mcmc(va_data_PS2068_Vm_P6p_wt_mod/2)))
        colnames(posterior.mode_va_data_PS2068_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_PS2068_P6p_wt <- rbind(HPDinterval(as.mcmc(va_data_PS2068_CONTROL_P6p_wt_mod/2)),HPDinterval(as.mcmc(va_data_PS2068_MA_P6p_wt_mod/2)),HPDinterval(as.mcmc(va_data_PS2068_Vm_P6p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_PS2068_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_PS2068_P6p_wt <- rbind(HPDinterval(as.mcmc(va_data_PS2068_CONTROL_P6p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_PS2068_MA_P6p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_PS2068_Vm_P6p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_PS2068_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_PS2068_P6p_wt <- rbind(effectiveSize(va_data_PS2068_CONTROL_P6p_wt_mod/2),effectiveSize(va_data_PS2068_MA_P6p_wt_mod/2),effectiveSize(va_data_PS2068_Vm_P6p_wt_mod/2))
        colnames(effectiveSize_va_data_PS2068_P6p_wt) <- c("effectiveSize")
        va_data_PS2068_P6p_wt <- cbind.data.frame(mean_va_data_PS2068_P6p_wt,median_va_data_PS2068_P6p_wt,posterior.mode_va_data_PS2068_P6p_wt,HPDinterval_0.95_va_data_PS2068_P6p_wt,HPDinterval_0.83_va_data_PS2068_P6p_wt,effectiveSize_va_data_PS2068_P6p_wt)
        rownames(va_data_PS2068_P6p_wt) <- c("va_data_PS2068_CONTROL_P6p_wt_mod","va_data_PS2068_MA_P6p_wt_mod","va_data_PS2068_Vm_P6p_wt_mod")
        va_data_PS2068_P6p_wt <- cbind(Models = rownames(va_data_PS2068_P6p_wt),va_data_PS2068_P6p_wt)
        rownames(va_data_PS2068_P6p_wt) <- NULL
        va_data_PS2068_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        va_data_PS2068_P6p_wt$Treatment <- c("Control","ML","Vm")
        va_data_PS2068_P6p_wt$Measure <- c("Va","Va","Va")
        va_data_PS2068_P6p_wt$Scale <- c("data","data","data")
        va_data_PS2068_P6p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_PS2068_P6p_wt
      }
      
      #Summary h2_data_PS2068_P6p_wt
      {
        mean_h2_data_PS2068_P6p_wt <- rbind(mean(h2_data_PS2068_CONTROL_P6p_wt_mod),mean(h2_data_PS2068_MA_P6p_wt_mod),mean(h2_data_PS2068_Vm_P6p_wt_mod))
        colnames(mean_h2_data_PS2068_P6p_wt) <- c("mean")
        median_h2_data_PS2068_P6p_wt <- rbind(median(h2_data_PS2068_CONTROL_P6p_wt_mod),median(h2_data_PS2068_MA_P6p_wt_mod),median(h2_data_PS2068_Vm_P6p_wt_mod))
        colnames(median_h2_data_PS2068_P6p_wt) <- c("median")
        posterior.mode_h2_data_PS2068_P6p_wt <- rbind(posterior.mode(as.mcmc(h2_data_PS2068_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(h2_data_PS2068_MA_P6p_wt_mod)),posterior.mode(as.mcmc(h2_data_PS2068_Vm_P6p_wt_mod)))
        colnames(posterior.mode_h2_data_PS2068_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_PS2068_P6p_wt <- rbind(HPDinterval(as.mcmc(h2_data_PS2068_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(h2_data_PS2068_MA_P6p_wt_mod)),HPDinterval(as.mcmc(h2_data_PS2068_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_PS2068_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_PS2068_P6p_wt <- rbind(HPDinterval(as.mcmc(h2_data_PS2068_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_PS2068_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_PS2068_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_PS2068_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_PS2068_P6p_wt <- rbind(effectiveSize(h2_data_PS2068_CONTROL_P6p_wt_mod),effectiveSize(h2_data_PS2068_MA_P6p_wt_mod),effectiveSize(h2_data_PS2068_Vm_P6p_wt_mod))
        colnames(effectiveSize_h2_data_PS2068_P6p_wt) <- c("effectiveSize")
        h2_data_PS2068_P6p_wt <- cbind.data.frame(mean_h2_data_PS2068_P6p_wt,median_h2_data_PS2068_P6p_wt,posterior.mode_h2_data_PS2068_P6p_wt,HPDinterval_0.95_h2_data_PS2068_P6p_wt,HPDinterval_0.83_h2_data_PS2068_P6p_wt,effectiveSize_h2_data_PS2068_P6p_wt)
        rownames(h2_data_PS2068_P6p_wt) <- c("h2_data_PS2068_CONTROL_P6p_wt_mod","h2_data_PS2068_MA_P6p_wt_mod","h2_data_PS2068_Vm_P6p_wt_mod")
        h2_data_PS2068_P6p_wt <- cbind(Models = rownames(h2_data_PS2068_P6p_wt),h2_data_PS2068_P6p_wt)
        rownames(h2_data_PS2068_P6p_wt) <- NULL
        h2_data_PS2068_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        h2_data_PS2068_P6p_wt$Treatment <- c("Control","ML","Vm")
        h2_data_PS2068_P6p_wt$Measure <- c("H2","H2","H2")
        h2_data_PS2068_P6p_wt$Scale <- c("data","data","data")
        h2_data_PS2068_P6p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_PS2068_P6p_wt
      }
      
      #Summary Evol_data_PS2068_P6p_wt
      {
        mean_Evol_data_PS2068_P6p_wt <- rbind(mean(Evol_data_PS2068_CONTROL_P6p_wt_mod),mean(Evol_data_PS2068_MA_P6p_wt_mod),mean(Evol_data_PS2068_Vm_P6p_wt_mod))
        colnames(mean_Evol_data_PS2068_P6p_wt) <- c("mean")
        median_Evol_data_PS2068_P6p_wt <- rbind(median(Evol_data_PS2068_CONTROL_P6p_wt_mod),median(Evol_data_PS2068_MA_P6p_wt_mod),median(Evol_data_PS2068_Vm_P6p_wt_mod))
        colnames(median_Evol_data_PS2068_P6p_wt) <- c("median")
        posterior.mode_Evol_data_PS2068_P6p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_PS2068_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(Evol_data_PS2068_MA_P6p_wt_mod)),posterior.mode(as.mcmc(Evol_data_PS2068_Vm_P6p_wt_mod)))
        colnames(posterior.mode_Evol_data_PS2068_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_PS2068_P6p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_PS2068_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(Evol_data_PS2068_MA_P6p_wt_mod)),HPDinterval(as.mcmc(Evol_data_PS2068_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_PS2068_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_PS2068_P6p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_PS2068_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_PS2068_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_PS2068_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_PS2068_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_PS2068_P6p_wt <- rbind(effectiveSize(Evol_data_PS2068_CONTROL_P6p_wt_mod),effectiveSize(Evol_data_PS2068_MA_P6p_wt_mod),effectiveSize(Evol_data_PS2068_Vm_P6p_wt_mod))
        colnames(effectiveSize_Evol_data_PS2068_P6p_wt) <- c("effectiveSize")
        Evol_data_PS2068_P6p_wt <- cbind.data.frame(mean_Evol_data_PS2068_P6p_wt,median_Evol_data_PS2068_P6p_wt,posterior.mode_Evol_data_PS2068_P6p_wt,HPDinterval_0.95_Evol_data_PS2068_P6p_wt,HPDinterval_0.83_Evol_data_PS2068_P6p_wt,effectiveSize_Evol_data_PS2068_P6p_wt)
        rownames(Evol_data_PS2068_P6p_wt) <- c("Evol_data_PS2068_CONTROL_P6p_wt_mod","Evol_data_PS2068_MA_P6p_wt_mod","Evol_data_PS2068_Vm_P6p_wt_mod")
        Evol_data_PS2068_P6p_wt <- cbind(Models = rownames(Evol_data_PS2068_P6p_wt),Evol_data_PS2068_P6p_wt)
        rownames(Evol_data_PS2068_P6p_wt) <- NULL
        Evol_data_PS2068_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        Evol_data_PS2068_P6p_wt$Treatment <- c("Control","ML","Vm")
        Evol_data_PS2068_P6p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_PS2068_P6p_wt$Scale <- c("data","data","data")
        Evol_data_PS2068_P6p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_PS2068_P6p_wt
      }
      
      #Summary trait_mean_data_PS2068_P6p_wt
      {
        mean_trait_mean_data_PS2068_P6p_wt <- rbind(mean(trait_mean_data_PS2068_CONTROL_P6p_wt_mod),mean(trait_mean_data_PS2068_MA_P6p_wt_mod),mean(trait_mean_data_PS2068_Vm_P6p_wt_mod))
        colnames(mean_trait_mean_data_PS2068_P6p_wt) <- c("mean")
        median_trait_mean_data_PS2068_P6p_wt <- rbind(median(trait_mean_data_PS2068_CONTROL_P6p_wt_mod),median(trait_mean_data_PS2068_MA_P6p_wt_mod),median(trait_mean_data_PS2068_Vm_P6p_wt_mod))
        colnames(median_trait_mean_data_PS2068_P6p_wt) <- c("median")
        posterior.mode_trait_mean_data_PS2068_P6p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_PS2068_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_PS2068_MA_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_PS2068_Vm_P6p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_PS2068_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_PS2068_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_PS2068_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_PS2068_MA_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_PS2068_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_PS2068_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_PS2068_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_PS2068_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_PS2068_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_PS2068_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_PS2068_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_PS2068_P6p_wt <- rbind(effectiveSize(trait_mean_data_PS2068_CONTROL_P6p_wt_mod),effectiveSize(trait_mean_data_PS2068_MA_P6p_wt_mod),effectiveSize(trait_mean_data_PS2068_Vm_P6p_wt_mod))
        colnames(effectiveSize_trait_mean_data_PS2068_P6p_wt) <- c("effectiveSize")
        trait_mean_data_PS2068_P6p_wt <- cbind.data.frame(mean_trait_mean_data_PS2068_P6p_wt,median_trait_mean_data_PS2068_P6p_wt,posterior.mode_trait_mean_data_PS2068_P6p_wt,HPDinterval_0.95_trait_mean_data_PS2068_P6p_wt,HPDinterval_0.83_trait_mean_data_PS2068_P6p_wt,effectiveSize_trait_mean_data_PS2068_P6p_wt)
        rownames(trait_mean_data_PS2068_P6p_wt) <- c("trait_mean_data_PS2068_CONTROL_P6p_wt_mod","trait_mean_data_PS2068_MA_P6p_wt_mod","trait_mean_data_PS2068_Vm_P6p_wt_mod")
        trait_mean_data_PS2068_P6p_wt <- cbind(Models = rownames(trait_mean_data_PS2068_P6p_wt),trait_mean_data_PS2068_P6p_wt)
        rownames(trait_mean_data_PS2068_P6p_wt) <- NULL
        trait_mean_data_PS2068_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        trait_mean_data_PS2068_P6p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_data_PS2068_P6p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_PS2068_P6p_wt$Scale <- c("data","data","data")
        trait_mean_data_PS2068_P6p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_PS2068_P6p_wt
      }
      
      data_PS2068_P6p_wt <- rbind.data.frame(va_data_PS2068_P6p_wt, h2_data_PS2068_P6p_wt,Evol_data_PS2068_P6p_wt,trait_mean_data_PS2068_P6p_wt)
      data_PS2068_P6p_wt
      
    }
    Vm_PS2068_P6p_wt <- rbind.data.frame(liab_PS2068_P6p_wt, data_PS2068_P6p_wt)
    Vm_PS2068_P6p_wt$Pnp_fate <- rep("wt", 24)
    Vm_PS2068_P6p_wt
    #remove PS2068 P6p_wt models
    {
      remove(PS2068_CONTROL_P6p_wt_mod)
      remove(PS2068_MA_P6p_wt_mod)
      remove(PS2068_Vm_P6p_wt_mod)
    }
  }
  
  ##Summary PS2068 P7p----
  {
    #Summary liability scale PS2068 P7p
    {
      #Summary va_liab_PS2068_P7p_wt: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_PS2068_P7p_wt <- rbind(mean(va_liab_PS2068_CONTROL_P7p_wt_mod/2),mean(va_liab_PS2068_MA_P7p_wt_mod/2),mean(va_liab_PS2068_Vm_P7p_wt_mod/2))
        colnames(mean_va_liab_PS2068_P7p_wt) <- c("mean")
        median_va_liab_PS2068_P7p_wt <- rbind(median(va_liab_PS2068_CONTROL_P7p_wt_mod/2),median(va_liab_PS2068_MA_P7p_wt_mod/2),median(va_liab_PS2068_Vm_P7p_wt_mod/2))
        colnames(median_va_liab_PS2068_P7p_wt) <- c("median")
        posterior.mode_va_liab_PS2068_P7p_wt <- rbind(posterior.mode(va_liab_PS2068_CONTROL_P7p_wt_mod/2),posterior.mode(va_liab_PS2068_MA_P7p_wt_mod/2),posterior.mode(va_liab_PS2068_Vm_P7p_wt_mod/2))
        colnames(posterior.mode_va_liab_PS2068_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_PS2068_P7p_wt <- rbind(HPDinterval(va_liab_PS2068_CONTROL_P7p_wt_mod/2),HPDinterval(va_liab_PS2068_MA_P7p_wt_mod/2),HPDinterval(va_liab_PS2068_Vm_P7p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_PS2068_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_PS2068_P7p_wt <- rbind(HPDinterval(va_liab_PS2068_CONTROL_P7p_wt_mod/2,prob=.83),HPDinterval(va_liab_PS2068_MA_P7p_wt_mod/2,prob=.83),HPDinterval(va_liab_PS2068_Vm_P7p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_PS2068_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_PS2068_P7p_wt <- rbind(effectiveSize(va_liab_PS2068_CONTROL_P7p_wt_mod/2),effectiveSize(va_liab_PS2068_MA_P7p_wt_mod/2),effectiveSize(va_liab_PS2068_Vm_P7p_wt_mod/2))
        colnames(effectiveSize_va_liab_PS2068_P7p_wt) <- c("effectiveSize")
        va_liab_PS2068_P7p_wt <- cbind.data.frame(mean_va_liab_PS2068_P7p_wt,median_va_liab_PS2068_P7p_wt,posterior.mode_va_liab_PS2068_P7p_wt,HPDinterval_0.95_va_liab_PS2068_P7p_wt,HPDinterval_0.83_va_liab_PS2068_P7p_wt,effectiveSize_va_liab_PS2068_P7p_wt)
        rownames(va_liab_PS2068_P7p_wt) <- c("va_liab_PS2068_CONTROL_P7p_wt_mod","va_liab_PS2068_MA_P7p_wt_mod","va_liab_PS2068_Vm_P7p_wt_mod")
        va_liab_PS2068_P7p_wt <- cbind(Models = rownames(va_liab_PS2068_P7p_wt),va_liab_PS2068_P7p_wt)
        rownames(va_liab_PS2068_P7p_wt) <- NULL
        va_liab_PS2068_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        va_liab_PS2068_P7p_wt$Treatment <- c("Control","ML","Vm")
        va_liab_PS2068_P7p_wt$Measure <- c("Va","Va","Va")
        va_liab_PS2068_P7p_wt$Scale <- c("liab","liab","liab")
        va_liab_PS2068_P7p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_PS2068_P7p_wt
      }
      
      #Summary h2_liab_PS2068_P7p_wt
      {
        mean_h2_liab_PS2068_P7p_wt <- rbind(mean(h2_liab_PS2068_CONTROL_P7p_wt_mod),mean(h2_liab_PS2068_MA_P7p_wt_mod),mean(h2_liab_PS2068_Vm_P7p_wt_mod))
        colnames(mean_h2_liab_PS2068_P7p_wt) <- c("mean")
        median_h2_liab_PS2068_P7p_wt <- rbind(median(h2_liab_PS2068_CONTROL_P7p_wt_mod),median(h2_liab_PS2068_MA_P7p_wt_mod),median(h2_liab_PS2068_Vm_P7p_wt_mod))
        colnames(median_h2_liab_PS2068_P7p_wt) <- c("median")
        posterior.mode_h2_liab_PS2068_P7p_wt <- rbind(posterior.mode(h2_liab_PS2068_CONTROL_P7p_wt_mod),posterior.mode(h2_liab_PS2068_MA_P7p_wt_mod),posterior.mode(h2_liab_PS2068_Vm_P7p_wt_mod))
        colnames(posterior.mode_h2_liab_PS2068_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_PS2068_P7p_wt <- rbind(HPDinterval(h2_liab_PS2068_CONTROL_P7p_wt_mod),HPDinterval(h2_liab_PS2068_MA_P7p_wt_mod),HPDinterval(h2_liab_PS2068_Vm_P7p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_PS2068_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_PS2068_P7p_wt <- rbind(HPDinterval(h2_liab_PS2068_CONTROL_P7p_wt_mod,prob=.83),HPDinterval(h2_liab_PS2068_MA_P7p_wt_mod,prob=.83),HPDinterval(h2_liab_PS2068_Vm_P7p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_PS2068_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_PS2068_P7p_wt <- rbind(effectiveSize(h2_liab_PS2068_CONTROL_P7p_wt_mod),effectiveSize(h2_liab_PS2068_MA_P7p_wt_mod),effectiveSize(h2_liab_PS2068_Vm_P7p_wt_mod))
        colnames(effectiveSize_h2_liab_PS2068_P7p_wt) <- c("effectiveSize")
        h2_liab_PS2068_P7p_wt <- cbind.data.frame(mean_h2_liab_PS2068_P7p_wt,median_h2_liab_PS2068_P7p_wt,posterior.mode_h2_liab_PS2068_P7p_wt,HPDinterval_0.95_h2_liab_PS2068_P7p_wt,HPDinterval_0.83_h2_liab_PS2068_P7p_wt,effectiveSize_h2_liab_PS2068_P7p_wt)
        rownames(h2_liab_PS2068_P7p_wt) <- c("h2_liab_PS2068_CONTROL_P7p_wt_mod","h2_liab_PS2068_MA_P7p_wt_mod","h2_liab_PS2068_Vm_P7p_wt_mod")
        h2_liab_PS2068_P7p_wt <- cbind(Models = rownames(h2_liab_PS2068_P7p_wt),h2_liab_PS2068_P7p_wt)
        rownames(h2_liab_PS2068_P7p_wt) <- NULL
        h2_liab_PS2068_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        h2_liab_PS2068_P7p_wt$Treatment <- c("Control","ML","Vm")
        h2_liab_PS2068_P7p_wt$Measure <- c("H2","H2","H2")
        h2_liab_PS2068_P7p_wt$Scale <- c("liab","liab","liab")
        h2_liab_PS2068_P7p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_PS2068_P7p_wt
      }
      
      #Summary Evol_liab_PS2068_P7p_wt
      {
        mean_Evol_liab_PS2068_P7p_wt <- rbind(mean(Evol_liab_PS2068_CONTROL_P7p_wt_mod),mean(Evol_liab_PS2068_MA_P7p_wt_mod),mean(Evol_liab_PS2068_Vm_P7p_wt_mod))
        colnames(mean_Evol_liab_PS2068_P7p_wt) <- c("mean")
        median_Evol_liab_PS2068_P7p_wt <- rbind(median(Evol_liab_PS2068_CONTROL_P7p_wt_mod),median(Evol_liab_PS2068_MA_P7p_wt_mod),median(Evol_liab_PS2068_Vm_P7p_wt_mod))
        colnames(median_Evol_liab_PS2068_P7p_wt) <- c("median")
        posterior.mode_Evol_liab_PS2068_P7p_wt <- rbind(posterior.mode(Evol_liab_PS2068_CONTROL_P7p_wt_mod),posterior.mode(Evol_liab_PS2068_MA_P7p_wt_mod),posterior.mode(Evol_liab_PS2068_Vm_P7p_wt_mod))
        colnames(posterior.mode_Evol_liab_PS2068_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_PS2068_P7p_wt <- rbind(HPDinterval(Evol_liab_PS2068_CONTROL_P7p_wt_mod),HPDinterval(Evol_liab_PS2068_MA_P7p_wt_mod),HPDinterval(Evol_liab_PS2068_Vm_P7p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_PS2068_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_PS2068_P7p_wt <- rbind(HPDinterval(Evol_liab_PS2068_CONTROL_P7p_wt_mod,prob=.83),HPDinterval(Evol_liab_PS2068_MA_P7p_wt_mod,prob=.83),HPDinterval(Evol_liab_PS2068_Vm_P7p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_PS2068_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_PS2068_P7p_wt <- rbind(effectiveSize(Evol_liab_PS2068_CONTROL_P7p_wt_mod),effectiveSize(Evol_liab_PS2068_MA_P7p_wt_mod),effectiveSize(Evol_liab_PS2068_Vm_P7p_wt_mod))
        colnames(effectiveSize_Evol_liab_PS2068_P7p_wt) <- c("effectiveSize")
        Evol_liab_PS2068_P7p_wt <- cbind.data.frame(mean_Evol_liab_PS2068_P7p_wt,median_Evol_liab_PS2068_P7p_wt,posterior.mode_Evol_liab_PS2068_P7p_wt,HPDinterval_0.95_Evol_liab_PS2068_P7p_wt,HPDinterval_0.83_Evol_liab_PS2068_P7p_wt,effectiveSize_Evol_liab_PS2068_P7p_wt)
        rownames(Evol_liab_PS2068_P7p_wt) <- c("Evol_liab_PS2068_CONTROL_P7p_wt_mod","Evol_liab_PS2068_MA_P7p_wt_mod","Evol_liab_PS2068_Vm_P7p_wt_mod")
        Evol_liab_PS2068_P7p_wt <- cbind(Models = rownames(Evol_liab_PS2068_P7p_wt),Evol_liab_PS2068_P7p_wt)
        rownames(Evol_liab_PS2068_P7p_wt) <- NULL
        Evol_liab_PS2068_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        Evol_liab_PS2068_P7p_wt$Treatment <- c("Control","ML","Vm")
        Evol_liab_PS2068_P7p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_PS2068_P7p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_PS2068_P7p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_PS2068_P7p_wt
      }
      
      #Summary trait_mean_liab_PS2068_P7p_wt
      {
        mean_trait_mean_liab_PS2068_P7p_wt <- rbind(mean(trait_mean_liab_PS2068_CONTROL_P7p_wt_mod),mean(trait_mean_liab_PS2068_MA_P7p_wt_mod),mean(trait_mean_liab_PS2068_Vm_P7p_wt_mod))
        colnames(mean_trait_mean_liab_PS2068_P7p_wt) <- c("mean")
        median_trait_mean_liab_PS2068_P7p_wt <- rbind(median(trait_mean_liab_PS2068_CONTROL_P7p_wt_mod),median(trait_mean_liab_PS2068_MA_P7p_wt_mod),median(trait_mean_liab_PS2068_Vm_P7p_wt_mod))
        colnames(median_trait_mean_liab_PS2068_P7p_wt) <- c("median")
        posterior.mode_trait_mean_liab_PS2068_P7p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_PS2068_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_PS2068_MA_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_PS2068_Vm_P7p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_PS2068_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_PS2068_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_PS2068_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_PS2068_MA_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_PS2068_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_PS2068_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_PS2068_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_PS2068_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_PS2068_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_PS2068_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_PS2068_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_PS2068_P7p_wt <- rbind(effectiveSize(trait_mean_liab_PS2068_CONTROL_P7p_wt_mod),effectiveSize(trait_mean_liab_PS2068_MA_P7p_wt_mod),effectiveSize(trait_mean_liab_PS2068_Vm_P7p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_PS2068_P7p_wt) <- c("effectiveSize")
        trait_mean_liab_PS2068_P7p_wt <- cbind.data.frame(mean_trait_mean_liab_PS2068_P7p_wt,median_trait_mean_liab_PS2068_P7p_wt,posterior.mode_trait_mean_liab_PS2068_P7p_wt,HPDinterval_0.95_trait_mean_liab_PS2068_P7p_wt,HPDinterval_0.83_trait_mean_liab_PS2068_P7p_wt,effectiveSize_trait_mean_liab_PS2068_P7p_wt)
        rownames(trait_mean_liab_PS2068_P7p_wt) <- c("trait_mean_liab_PS2068_CONTROL_P7p_wt_mod","trait_mean_liab_PS2068_MA_P7p_wt_mod","trait_mean_liab_PS2068_Vm_P7p_wt_mod")
        trait_mean_liab_PS2068_P7p_wt <- cbind(Models = rownames(trait_mean_liab_PS2068_P7p_wt),trait_mean_liab_PS2068_P7p_wt)
        rownames(trait_mean_liab_PS2068_P7p_wt) <- NULL
        trait_mean_liab_PS2068_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        trait_mean_liab_PS2068_P7p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_PS2068_P7p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_PS2068_P7p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_PS2068_P7p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_PS2068_P7p_wt
      }
      
      liab_PS2068_P7p_wt <- rbind.data.frame(va_liab_PS2068_P7p_wt, h2_liab_PS2068_P7p_wt,Evol_liab_PS2068_P7p_wt,trait_mean_liab_PS2068_P7p_wt)
      liab_PS2068_P7p_wt
    }
    #Summary data scale PS2068 P7p
    {
      #Summary va_data_PS2068_P7p_wt:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_PS2068_P7p_wt <- rbind(mean(va_data_PS2068_CONTROL_P7p_wt_mod/2),mean(va_data_PS2068_MA_P7p_wt_mod/2),mean(va_data_PS2068_Vm_P7p_wt_mod/2))
        colnames(mean_va_data_PS2068_P7p_wt) <- c("mean")
        median_va_data_PS2068_P7p_wt <- rbind(median(va_data_PS2068_CONTROL_P7p_wt_mod/2),median(va_data_PS2068_MA_P7p_wt_mod/2),median(va_data_PS2068_Vm_P7p_wt_mod/2))
        colnames(median_va_data_PS2068_P7p_wt) <- c("median")
        posterior.mode_va_data_PS2068_P7p_wt <- rbind(posterior.mode(as.mcmc(va_data_PS2068_CONTROL_P7p_wt_mod/2)),posterior.mode(as.mcmc(va_data_PS2068_MA_P7p_wt_mod/2)),posterior.mode(as.mcmc(va_data_PS2068_Vm_P7p_wt_mod/2)))
        colnames(posterior.mode_va_data_PS2068_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_PS2068_P7p_wt <- rbind(HPDinterval(as.mcmc(va_data_PS2068_CONTROL_P7p_wt_mod/2)),HPDinterval(as.mcmc(va_data_PS2068_MA_P7p_wt_mod/2)),HPDinterval(as.mcmc(va_data_PS2068_Vm_P7p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_PS2068_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_PS2068_P7p_wt <- rbind(HPDinterval(as.mcmc(va_data_PS2068_CONTROL_P7p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_PS2068_MA_P7p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_PS2068_Vm_P7p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_PS2068_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_PS2068_P7p_wt <- rbind(effectiveSize(va_data_PS2068_CONTROL_P7p_wt_mod/2),effectiveSize(va_data_PS2068_MA_P7p_wt_mod/2),effectiveSize(va_data_PS2068_Vm_P7p_wt_mod/2))
        colnames(effectiveSize_va_data_PS2068_P7p_wt) <- c("effectiveSize")
        va_data_PS2068_P7p_wt <- cbind.data.frame(mean_va_data_PS2068_P7p_wt,median_va_data_PS2068_P7p_wt,posterior.mode_va_data_PS2068_P7p_wt,HPDinterval_0.95_va_data_PS2068_P7p_wt,HPDinterval_0.83_va_data_PS2068_P7p_wt,effectiveSize_va_data_PS2068_P7p_wt)
        rownames(va_data_PS2068_P7p_wt) <- c("va_data_PS2068_CONTROL_P7p_wt_mod","va_data_PS2068_MA_P7p_wt_mod","va_data_PS2068_Vm_P7p_wt_mod")
        va_data_PS2068_P7p_wt <- cbind(Models = rownames(va_data_PS2068_P7p_wt),va_data_PS2068_P7p_wt)
        rownames(va_data_PS2068_P7p_wt) <- NULL
        va_data_PS2068_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        va_data_PS2068_P7p_wt$Treatment <- c("Control","ML","Vm")
        va_data_PS2068_P7p_wt$Measure <- c("Va","Va","Va")
        va_data_PS2068_P7p_wt$Scale <- c("data","data","data")
        va_data_PS2068_P7p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_PS2068_P7p_wt
      }
      
      #Summary h2_data_PS2068_P7p_wt
      {
        mean_h2_data_PS2068_P7p_wt <- rbind(mean(h2_data_PS2068_CONTROL_P7p_wt_mod),mean(h2_data_PS2068_MA_P7p_wt_mod),mean(h2_data_PS2068_Vm_P7p_wt_mod))
        colnames(mean_h2_data_PS2068_P7p_wt) <- c("mean")
        median_h2_data_PS2068_P7p_wt <- rbind(median(h2_data_PS2068_CONTROL_P7p_wt_mod),median(h2_data_PS2068_MA_P7p_wt_mod),median(h2_data_PS2068_Vm_P7p_wt_mod))
        colnames(median_h2_data_PS2068_P7p_wt) <- c("median")
        posterior.mode_h2_data_PS2068_P7p_wt <- rbind(posterior.mode(as.mcmc(h2_data_PS2068_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(h2_data_PS2068_MA_P7p_wt_mod)),posterior.mode(as.mcmc(h2_data_PS2068_Vm_P7p_wt_mod)))
        colnames(posterior.mode_h2_data_PS2068_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_PS2068_P7p_wt <- rbind(HPDinterval(as.mcmc(h2_data_PS2068_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(h2_data_PS2068_MA_P7p_wt_mod)),HPDinterval(as.mcmc(h2_data_PS2068_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_PS2068_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_PS2068_P7p_wt <- rbind(HPDinterval(as.mcmc(h2_data_PS2068_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_PS2068_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_PS2068_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_PS2068_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_PS2068_P7p_wt <- rbind(effectiveSize(h2_data_PS2068_CONTROL_P7p_wt_mod),effectiveSize(h2_data_PS2068_MA_P7p_wt_mod),effectiveSize(h2_data_PS2068_Vm_P7p_wt_mod))
        colnames(effectiveSize_h2_data_PS2068_P7p_wt) <- c("effectiveSize")
        h2_data_PS2068_P7p_wt <- cbind.data.frame(mean_h2_data_PS2068_P7p_wt,median_h2_data_PS2068_P7p_wt,posterior.mode_h2_data_PS2068_P7p_wt,HPDinterval_0.95_h2_data_PS2068_P7p_wt,HPDinterval_0.83_h2_data_PS2068_P7p_wt,effectiveSize_h2_data_PS2068_P7p_wt)
        rownames(h2_data_PS2068_P7p_wt) <- c("h2_data_PS2068_CONTROL_P7p_wt_mod","h2_data_PS2068_MA_P7p_wt_mod","h2_data_PS2068_Vm_P7p_wt_mod")
        h2_data_PS2068_P7p_wt <- cbind(Models = rownames(h2_data_PS2068_P7p_wt),h2_data_PS2068_P7p_wt)
        rownames(h2_data_PS2068_P7p_wt) <- NULL
        h2_data_PS2068_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        h2_data_PS2068_P7p_wt$Treatment <- c("Control","ML","Vm")
        h2_data_PS2068_P7p_wt$Measure <- c("H2","H2","H2")
        h2_data_PS2068_P7p_wt$Scale <- c("data","data","data")
        h2_data_PS2068_P7p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_PS2068_P7p_wt
      }
      
      #Summary Evol_data_PS2068_P7p_wt
      {
        mean_Evol_data_PS2068_P7p_wt <- rbind(mean(Evol_data_PS2068_CONTROL_P7p_wt_mod),mean(Evol_data_PS2068_MA_P7p_wt_mod),mean(Evol_data_PS2068_Vm_P7p_wt_mod))
        colnames(mean_Evol_data_PS2068_P7p_wt) <- c("mean")
        median_Evol_data_PS2068_P7p_wt <- rbind(median(Evol_data_PS2068_CONTROL_P7p_wt_mod),median(Evol_data_PS2068_MA_P7p_wt_mod),median(Evol_data_PS2068_Vm_P7p_wt_mod))
        colnames(median_Evol_data_PS2068_P7p_wt) <- c("median")
        posterior.mode_Evol_data_PS2068_P7p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_PS2068_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(Evol_data_PS2068_MA_P7p_wt_mod)),posterior.mode(as.mcmc(Evol_data_PS2068_Vm_P7p_wt_mod)))
        colnames(posterior.mode_Evol_data_PS2068_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_PS2068_P7p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_PS2068_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(Evol_data_PS2068_MA_P7p_wt_mod)),HPDinterval(as.mcmc(Evol_data_PS2068_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_PS2068_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_PS2068_P7p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_PS2068_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_PS2068_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_PS2068_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_PS2068_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_PS2068_P7p_wt <- rbind(effectiveSize(Evol_data_PS2068_CONTROL_P7p_wt_mod),effectiveSize(Evol_data_PS2068_MA_P7p_wt_mod),effectiveSize(Evol_data_PS2068_Vm_P7p_wt_mod))
        colnames(effectiveSize_Evol_data_PS2068_P7p_wt) <- c("effectiveSize")
        Evol_data_PS2068_P7p_wt <- cbind.data.frame(mean_Evol_data_PS2068_P7p_wt,median_Evol_data_PS2068_P7p_wt,posterior.mode_Evol_data_PS2068_P7p_wt,HPDinterval_0.95_Evol_data_PS2068_P7p_wt,HPDinterval_0.83_Evol_data_PS2068_P7p_wt,effectiveSize_Evol_data_PS2068_P7p_wt)
        rownames(Evol_data_PS2068_P7p_wt) <- c("Evol_data_PS2068_CONTROL_P7p_wt_mod","Evol_data_PS2068_MA_P7p_wt_mod","Evol_data_PS2068_Vm_P7p_wt_mod")
        Evol_data_PS2068_P7p_wt <- cbind(Models = rownames(Evol_data_PS2068_P7p_wt),Evol_data_PS2068_P7p_wt)
        rownames(Evol_data_PS2068_P7p_wt) <- NULL
        Evol_data_PS2068_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        Evol_data_PS2068_P7p_wt$Treatment <- c("Control","ML","Vm")
        Evol_data_PS2068_P7p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_PS2068_P7p_wt$Scale <- c("data","data","data")
        Evol_data_PS2068_P7p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_PS2068_P7p_wt
      }
      
      #Summary trait_mean_data_PS2068_P7p_wt
      {
        mean_trait_mean_data_PS2068_P7p_wt <- rbind(mean(trait_mean_data_PS2068_CONTROL_P7p_wt_mod),mean(trait_mean_data_PS2068_MA_P7p_wt_mod),mean(trait_mean_data_PS2068_Vm_P7p_wt_mod))
        colnames(mean_trait_mean_data_PS2068_P7p_wt) <- c("mean")
        median_trait_mean_data_PS2068_P7p_wt <- rbind(median(trait_mean_data_PS2068_CONTROL_P7p_wt_mod),median(trait_mean_data_PS2068_MA_P7p_wt_mod),median(trait_mean_data_PS2068_Vm_P7p_wt_mod))
        colnames(median_trait_mean_data_PS2068_P7p_wt) <- c("median")
        posterior.mode_trait_mean_data_PS2068_P7p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_PS2068_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_PS2068_MA_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_PS2068_Vm_P7p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_PS2068_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_PS2068_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_PS2068_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_PS2068_MA_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_PS2068_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_PS2068_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_PS2068_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_PS2068_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_PS2068_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_PS2068_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_PS2068_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_PS2068_P7p_wt <- rbind(effectiveSize(trait_mean_data_PS2068_CONTROL_P7p_wt_mod),effectiveSize(trait_mean_data_PS2068_MA_P7p_wt_mod),effectiveSize(trait_mean_data_PS2068_Vm_P7p_wt_mod))
        colnames(effectiveSize_trait_mean_data_PS2068_P7p_wt) <- c("effectiveSize")
        trait_mean_data_PS2068_P7p_wt <- cbind.data.frame(mean_trait_mean_data_PS2068_P7p_wt,median_trait_mean_data_PS2068_P7p_wt,posterior.mode_trait_mean_data_PS2068_P7p_wt,HPDinterval_0.95_trait_mean_data_PS2068_P7p_wt,HPDinterval_0.83_trait_mean_data_PS2068_P7p_wt,effectiveSize_trait_mean_data_PS2068_P7p_wt)
        rownames(trait_mean_data_PS2068_P7p_wt) <- c("trait_mean_data_PS2068_CONTROL_P7p_wt_mod","trait_mean_data_PS2068_MA_P7p_wt_mod","trait_mean_data_PS2068_Vm_P7p_wt_mod")
        trait_mean_data_PS2068_P7p_wt <- cbind(Models = rownames(trait_mean_data_PS2068_P7p_wt),trait_mean_data_PS2068_P7p_wt)
        rownames(trait_mean_data_PS2068_P7p_wt) <- NULL
        trait_mean_data_PS2068_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        trait_mean_data_PS2068_P7p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_data_PS2068_P7p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_PS2068_P7p_wt$Scale <- c("data","data","data")
        trait_mean_data_PS2068_P7p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_PS2068_P7p_wt
      }
      
      data_PS2068_P7p_wt <- rbind.data.frame(va_data_PS2068_P7p_wt, h2_data_PS2068_P7p_wt,Evol_data_PS2068_P7p_wt,trait_mean_data_PS2068_P7p_wt)
      data_PS2068_P7p_wt
      
    }
    Vm_PS2068_P7p_wt <- rbind.data.frame(liab_PS2068_P7p_wt, data_PS2068_P7p_wt)
    Vm_PS2068_P7p_wt$Pnp_fate <- rep("wt", 24)
    Vm_PS2068_P7p_wt
    #remove PS2068 P7p_wt models
    {
      remove(PS2068_CONTROL_P7p_wt_mod)
      remove(PS2068_MA_P7p_wt_mod)
      remove(PS2068_Vm_P7p_wt_mod)
    }
  }
  
  Vm_PS2068_SSSS_Summary <- rbind.data.frame(Vm_PS2068_P3p_SSSS,Vm_PS2068_P4p_SSSS,Vm_PS2068_P5p_wt,Vm_PS2068_P6p_wt,Vm_PS2068_P7p_wt,Vm_PS2068_P8p_SSSS)
  Vm_PS2068_SSSS_Summary$Ancestral <- rep("PS2068",144)
  Vm_PS2068_SSSS_Summary$Species <- rep("O.onirici",144)
  Vm_PS2068_SSSS_Summary$Genus <- rep("Oscheius",144)
  View(Vm_PS2068_SSSS_Summary)
  
  ##Vm_PS2068_P3p_divided_P4p_SSSS----
  {
    #Vm_PS2068_P3p_divided_P4p_SSSS_liab
    {
      
      va_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod <- va_liab_PS2068_Vm_P3p_SSSS_mod / va_liab_PS2068_Vm_P4p_SSSS_mod
      h2_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod <- h2_liab_PS2068_Vm_P3p_SSSS_mod / h2_liab_PS2068_Vm_P4p_SSSS_mod
      Evol_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod <- Evol_liab_PS2068_Vm_P3p_SSSS_mod / Evol_liab_PS2068_Vm_P4p_SSSS_mod
      
      mean_va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS <- rbind(mean(log10(va_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod)),mean(log10(h2_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod)), mean(log10(Evol_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod)))
      colnames(mean_va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS) <- c("mean")
      median_va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS <- rbind(median(log10(va_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod)),median(log10(h2_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod)), median(log10(Evol_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod)))
      colnames(median_va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS) <- c("median")
      posterior.mode_va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS <- rbind(posterior.mode(as.mcmc(log10(va_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod))),posterior.mode(as.mcmc(log10(h2_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod))),posterior.mode(as.mcmc(log10(Evol_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod))))
      colnames(posterior.mode_va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS) <- c("posterior.mode")
      HPDinterval_0.95_va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod))),HPDinterval(as.mcmc(log10(h2_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod))),HPDinterval(as.mcmc(log10(Evol_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod))))
      colnames(HPDinterval_0.95_va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS) <- c("CI_lower_0.95","CI_upper_0.95")
      HPDinterval_0.83_va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83),HPDinterval(as.mcmc(log10(h2_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83),HPDinterval(as.mcmc(log10(Evol_liab_PS2068_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83))
      colnames(HPDinterval_0.83_va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS) <- c("CI_lower_0.83","CI_upper_0.83")
      va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS <- cbind.data.frame(mean_va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS,median_va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS,posterior.mode_va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS,HPDinterval_0.95_va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS,HPDinterval_0.83_va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS)
      rownames(va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS) <- c("va_liab_PS2068_Vm_P3p_divided_P4p_SSSS_log10","h2_liab_PS2068_Vm_P3p_divided_P4p_SSSS_log10","Evol_liab_PS2068_Vm_P3p_divided_P4p_SSSS_log10")
      va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS$Measure <- c("Va","H2", "Evol")
      va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS$Ancestral <- rep("PS2068",3)
      va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS$Species <- rep("O.onirici",3)
      va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS$Genus <- rep("Oscheius",3)
      va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS$Scale <- rep("liab",3)
      va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS
      
      pdf("Vm_va_h2_Evol_liab_P3p_divided_P4p_SSSS_log10_PS2068.pdf")
      ggplot(va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS, aes(x=Measure, y= median)) +
        geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
        geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
        geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
        theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
        labs(y = "Va log10 ( P3.p / P4.p)", title = "PS2068_log10(P3p/P4p)_liab")
      dev.off() 
      
    }
    
    #Vm_PS2068_P3p_divided_P4p_SSSS_data
    
    {
      va_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod <- va_data_PS2068_Vm_P3p_SSSS_mod / va_data_PS2068_Vm_P4p_SSSS_mod
      h2_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod <- h2_data_PS2068_Vm_P3p_SSSS_mod / h2_data_PS2068_Vm_P4p_SSSS_mod
      Evol_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod <- Evol_data_PS2068_Vm_P3p_SSSS_mod / Evol_data_PS2068_Vm_P4p_SSSS_mod
      
      mean_va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS <- rbind(mean(log10(va_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod)),mean(log10(h2_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod)), mean(log10(Evol_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod)))
      colnames(mean_va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS) <- c("mean")
      median_va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS <- rbind(median(log10(va_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod)),median(log10(h2_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod)), median(log10(Evol_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod)))
      colnames(median_va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS) <- c("median")
      posterior.mode_va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS <- rbind(posterior.mode(as.mcmc(log10(va_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod))),posterior.mode(as.mcmc(log10(h2_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod))),posterior.mode(as.mcmc(log10(Evol_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod))))
      colnames(posterior.mode_va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS) <- c("posterior.mode")
      HPDinterval_0.95_va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod))),HPDinterval(as.mcmc(log10(h2_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod))),HPDinterval(as.mcmc(log10(Evol_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod))))
      colnames(HPDinterval_0.95_va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS) <- c("CI_lower_0.95","CI_upper_0.95")
      HPDinterval_0.83_va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83),HPDinterval(as.mcmc(log10(h2_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83),HPDinterval(as.mcmc(log10(Evol_data_PS2068_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83))
      colnames(HPDinterval_0.83_va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS) <- c("CI_lower_0.83","CI_upper_0.83")
      va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS <- cbind.data.frame(mean_va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS,median_va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS,posterior.mode_va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS,HPDinterval_0.95_va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS,HPDinterval_0.83_va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS)
      rownames(va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS) <- c("va_data_PS2068_Vm_P3p_divided_P4p_SSSS_log10","h2_data_PS2068_Vm_P3p_divided_P4p_SSSS_log10","Evol_data_PS2068_Vm_P3p_divided_P4p_SSSS_log10")
      va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS$Measure <- c("Va","H2", "Evol")
      va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS$Ancestral <- rep("PS2068",3)
      va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS$Species <- rep("O.onirici",3)
      va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS$Genus <- rep("Oscheius",3)
      va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS$Scale <- rep("data",3)
      va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS
      
      pdf("Vm_va_h2_Evol_data_P3p_divided_P4p_SSSS_log10_PS2068.pdf")
      ggplot(va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS, aes(x=Measure, y= median)) +
        geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
        geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
        geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
        theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
        labs(y = "Va log10 ( P3.p / P4.p)", title = "PS2068_log10(P3p/P4p)_data")
      dev.off() 
      
    }
    
    va_h2_Evol_Vm_PS2068_P3p_divided_P4p_SSSS_Summary <- rbind.data.frame(va_h2_Evol_liab_Vm_PS2068_P3p_divided_P4p_SSSS,va_h2_Evol_data_Vm_PS2068_P3p_divided_P4p_SSSS)
    va_h2_Evol_Vm_PS2068_P3p_divided_P4p_SSSS_Summary
    
  }
  
  
}

#---- JU77 ----
{
  
  ##Summary JU77 P3p----
  {
    #Summary liability scale JU77 P3p
    {
      #Summary va_liab_JU77_P3p_SSSS: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_JU77_P3p_SSSS <- rbind(mean(va_liab_JU77_CONTROL_P3p_SSSS_mod/2),mean(va_liab_JU77_MA_P3p_SSSS_mod/2),mean(va_liab_JU77_Vm_P3p_SSSS_mod/2))
        colnames(mean_va_liab_JU77_P3p_SSSS) <- c("mean")
        median_va_liab_JU77_P3p_SSSS <- rbind(median(va_liab_JU77_CONTROL_P3p_SSSS_mod/2),median(va_liab_JU77_MA_P3p_SSSS_mod/2),median(va_liab_JU77_Vm_P3p_SSSS_mod/2))
        colnames(median_va_liab_JU77_P3p_SSSS) <- c("median")
        posterior.mode_va_liab_JU77_P3p_SSSS <- rbind(posterior.mode(va_liab_JU77_CONTROL_P3p_SSSS_mod/2),posterior.mode(va_liab_JU77_MA_P3p_SSSS_mod/2),posterior.mode(va_liab_JU77_Vm_P3p_SSSS_mod/2))
        colnames(posterior.mode_va_liab_JU77_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_JU77_P3p_SSSS <- rbind(HPDinterval(va_liab_JU77_CONTROL_P3p_SSSS_mod/2),HPDinterval(va_liab_JU77_MA_P3p_SSSS_mod/2),HPDinterval(va_liab_JU77_Vm_P3p_SSSS_mod/2))
        colnames(HPDinterval_0.95_va_liab_JU77_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_JU77_P3p_SSSS <- rbind(HPDinterval(va_liab_JU77_CONTROL_P3p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_JU77_MA_P3p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_JU77_Vm_P3p_SSSS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_JU77_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_JU77_P3p_SSSS <- rbind(effectiveSize(va_liab_JU77_CONTROL_P3p_SSSS_mod/2),effectiveSize(va_liab_JU77_MA_P3p_SSSS_mod/2),effectiveSize(va_liab_JU77_Vm_P3p_SSSS_mod/2))
        colnames(effectiveSize_va_liab_JU77_P3p_SSSS) <- c("effectiveSize")
        va_liab_JU77_P3p_SSSS <- cbind.data.frame(mean_va_liab_JU77_P3p_SSSS,median_va_liab_JU77_P3p_SSSS,posterior.mode_va_liab_JU77_P3p_SSSS,HPDinterval_0.95_va_liab_JU77_P3p_SSSS,HPDinterval_0.83_va_liab_JU77_P3p_SSSS,effectiveSize_va_liab_JU77_P3p_SSSS)
        rownames(va_liab_JU77_P3p_SSSS) <- c("va_liab_JU77_CONTROL_P3p_SSSS_mod","va_liab_JU77_MA_P3p_SSSS_mod","va_liab_JU77_Vm_P3p_SSSS_mod")
        va_liab_JU77_P3p_SSSS <- cbind(Models = rownames(va_liab_JU77_P3p_SSSS),va_liab_JU77_P3p_SSSS)
        rownames(va_liab_JU77_P3p_SSSS) <- NULL
        va_liab_JU77_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        va_liab_JU77_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        va_liab_JU77_P3p_SSSS$Measure <- c("Va","Va","Va")
        va_liab_JU77_P3p_SSSS$Scale <- c("liab","liab","liab")
        va_liab_JU77_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_liab_JU77_P3p_SSSS
      }
      
      #Summary h2_liab_JU77_P3p_SSSS
      {
        mean_h2_liab_JU77_P3p_SSSS <- rbind(mean(h2_liab_JU77_CONTROL_P3p_SSSS_mod),mean(h2_liab_JU77_MA_P3p_SSSS_mod),mean(h2_liab_JU77_Vm_P3p_SSSS_mod))
        colnames(mean_h2_liab_JU77_P3p_SSSS) <- c("mean")
        median_h2_liab_JU77_P3p_SSSS <- rbind(median(h2_liab_JU77_CONTROL_P3p_SSSS_mod),median(h2_liab_JU77_MA_P3p_SSSS_mod),median(h2_liab_JU77_Vm_P3p_SSSS_mod))
        colnames(median_h2_liab_JU77_P3p_SSSS) <- c("median")
        posterior.mode_h2_liab_JU77_P3p_SSSS <- rbind(posterior.mode(h2_liab_JU77_CONTROL_P3p_SSSS_mod),posterior.mode(h2_liab_JU77_MA_P3p_SSSS_mod),posterior.mode(h2_liab_JU77_Vm_P3p_SSSS_mod))
        colnames(posterior.mode_h2_liab_JU77_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_JU77_P3p_SSSS <- rbind(HPDinterval(h2_liab_JU77_CONTROL_P3p_SSSS_mod),HPDinterval(h2_liab_JU77_MA_P3p_SSSS_mod),HPDinterval(h2_liab_JU77_Vm_P3p_SSSS_mod))
        colnames(HPDinterval_0.95_h2_liab_JU77_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_JU77_P3p_SSSS <- rbind(HPDinterval(h2_liab_JU77_CONTROL_P3p_SSSS_mod,prob=.83),HPDinterval(h2_liab_JU77_MA_P3p_SSSS_mod,prob=.83),HPDinterval(h2_liab_JU77_Vm_P3p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_JU77_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_JU77_P3p_SSSS <- rbind(effectiveSize(h2_liab_JU77_CONTROL_P3p_SSSS_mod),effectiveSize(h2_liab_JU77_MA_P3p_SSSS_mod),effectiveSize(h2_liab_JU77_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_h2_liab_JU77_P3p_SSSS) <- c("effectiveSize")
        h2_liab_JU77_P3p_SSSS <- cbind.data.frame(mean_h2_liab_JU77_P3p_SSSS,median_h2_liab_JU77_P3p_SSSS,posterior.mode_h2_liab_JU77_P3p_SSSS,HPDinterval_0.95_h2_liab_JU77_P3p_SSSS,HPDinterval_0.83_h2_liab_JU77_P3p_SSSS,effectiveSize_h2_liab_JU77_P3p_SSSS)
        rownames(h2_liab_JU77_P3p_SSSS) <- c("h2_liab_JU77_CONTROL_P3p_SSSS_mod","h2_liab_JU77_MA_P3p_SSSS_mod","h2_liab_JU77_Vm_P3p_SSSS_mod")
        h2_liab_JU77_P3p_SSSS <- cbind(Models = rownames(h2_liab_JU77_P3p_SSSS),h2_liab_JU77_P3p_SSSS)
        rownames(h2_liab_JU77_P3p_SSSS) <- NULL
        h2_liab_JU77_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        h2_liab_JU77_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_liab_JU77_P3p_SSSS$Measure <- c("H2","H2","H2")
        h2_liab_JU77_P3p_SSSS$Scale <- c("liab","liab","liab")
        h2_liab_JU77_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_liab_JU77_P3p_SSSS
      }
      
      #Summary Evol_liab_JU77_P3p_SSSS
      {
        mean_Evol_liab_JU77_P3p_SSSS <- rbind(mean(Evol_liab_JU77_CONTROL_P3p_SSSS_mod),mean(Evol_liab_JU77_MA_P3p_SSSS_mod),mean(Evol_liab_JU77_Vm_P3p_SSSS_mod))
        colnames(mean_Evol_liab_JU77_P3p_SSSS) <- c("mean")
        median_Evol_liab_JU77_P3p_SSSS <- rbind(median(Evol_liab_JU77_CONTROL_P3p_SSSS_mod),median(Evol_liab_JU77_MA_P3p_SSSS_mod),median(Evol_liab_JU77_Vm_P3p_SSSS_mod))
        colnames(median_Evol_liab_JU77_P3p_SSSS) <- c("median")
        posterior.mode_Evol_liab_JU77_P3p_SSSS <- rbind(posterior.mode(Evol_liab_JU77_CONTROL_P3p_SSSS_mod),posterior.mode(Evol_liab_JU77_MA_P3p_SSSS_mod),posterior.mode(Evol_liab_JU77_Vm_P3p_SSSS_mod))
        colnames(posterior.mode_Evol_liab_JU77_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_JU77_P3p_SSSS <- rbind(HPDinterval(Evol_liab_JU77_CONTROL_P3p_SSSS_mod),HPDinterval(Evol_liab_JU77_MA_P3p_SSSS_mod),HPDinterval(Evol_liab_JU77_Vm_P3p_SSSS_mod))
        colnames(HPDinterval_0.95_Evol_liab_JU77_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_JU77_P3p_SSSS <- rbind(HPDinterval(Evol_liab_JU77_CONTROL_P3p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_JU77_MA_P3p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_JU77_Vm_P3p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_JU77_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_JU77_P3p_SSSS <- rbind(effectiveSize(Evol_liab_JU77_CONTROL_P3p_SSSS_mod),effectiveSize(Evol_liab_JU77_MA_P3p_SSSS_mod),effectiveSize(Evol_liab_JU77_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_Evol_liab_JU77_P3p_SSSS) <- c("effectiveSize")
        Evol_liab_JU77_P3p_SSSS <- cbind.data.frame(mean_Evol_liab_JU77_P3p_SSSS,median_Evol_liab_JU77_P3p_SSSS,posterior.mode_Evol_liab_JU77_P3p_SSSS,HPDinterval_0.95_Evol_liab_JU77_P3p_SSSS,HPDinterval_0.83_Evol_liab_JU77_P3p_SSSS,effectiveSize_Evol_liab_JU77_P3p_SSSS)
        rownames(Evol_liab_JU77_P3p_SSSS) <- c("Evol_liab_JU77_CONTROL_P3p_SSSS_mod","Evol_liab_JU77_MA_P3p_SSSS_mod","Evol_liab_JU77_Vm_P3p_SSSS_mod")
        Evol_liab_JU77_P3p_SSSS <- cbind(Models = rownames(Evol_liab_JU77_P3p_SSSS),Evol_liab_JU77_P3p_SSSS)
        rownames(Evol_liab_JU77_P3p_SSSS) <- NULL
        Evol_liab_JU77_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        Evol_liab_JU77_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_liab_JU77_P3p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_JU77_P3p_SSSS$Scale <- c("liab","liab","liab")
        Evol_liab_JU77_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_JU77_P3p_SSSS
      }
      
      #Summary trait_mean_liab_JU77_P3p_SSSS
      {
        mean_trait_mean_liab_JU77_P3p_SSSS <- rbind(mean(trait_mean_liab_JU77_CONTROL_P3p_SSSS_mod),mean(trait_mean_liab_JU77_MA_P3p_SSSS_mod),mean(trait_mean_liab_JU77_Vm_P3p_SSSS_mod))
        colnames(mean_trait_mean_liab_JU77_P3p_SSSS) <- c("mean")
        median_trait_mean_liab_JU77_P3p_SSSS <- rbind(median(trait_mean_liab_JU77_CONTROL_P3p_SSSS_mod),median(trait_mean_liab_JU77_MA_P3p_SSSS_mod),median(trait_mean_liab_JU77_Vm_P3p_SSSS_mod))
        colnames(median_trait_mean_liab_JU77_P3p_SSSS) <- c("median")
        posterior.mode_trait_mean_liab_JU77_P3p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_JU77_CONTROL_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU77_MA_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU77_Vm_P3p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_liab_JU77_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_JU77_P3p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU77_CONTROL_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU77_MA_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU77_Vm_P3p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_JU77_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_JU77_P3p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU77_CONTROL_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU77_MA_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU77_Vm_P3p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_JU77_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_JU77_P3p_SSSS <- rbind(effectiveSize(trait_mean_liab_JU77_CONTROL_P3p_SSSS_mod),effectiveSize(trait_mean_liab_JU77_MA_P3p_SSSS_mod),effectiveSize(trait_mean_liab_JU77_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_trait_mean_liab_JU77_P3p_SSSS) <- c("effectiveSize")
        trait_mean_liab_JU77_P3p_SSSS <- cbind.data.frame(mean_trait_mean_liab_JU77_P3p_SSSS,median_trait_mean_liab_JU77_P3p_SSSS,posterior.mode_trait_mean_liab_JU77_P3p_SSSS,HPDinterval_0.95_trait_mean_liab_JU77_P3p_SSSS,HPDinterval_0.83_trait_mean_liab_JU77_P3p_SSSS,effectiveSize_trait_mean_liab_JU77_P3p_SSSS)
        rownames(trait_mean_liab_JU77_P3p_SSSS) <- c("trait_mean_liab_JU77_CONTROL_P3p_SSSS_mod","trait_mean_liab_JU77_MA_P3p_SSSS_mod","trait_mean_liab_JU77_Vm_P3p_SSSS_mod")
        trait_mean_liab_JU77_P3p_SSSS <- cbind(Models = rownames(trait_mean_liab_JU77_P3p_SSSS),trait_mean_liab_JU77_P3p_SSSS)
        rownames(trait_mean_liab_JU77_P3p_SSSS) <- NULL
        trait_mean_liab_JU77_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        trait_mean_liab_JU77_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_JU77_P3p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_JU77_P3p_SSSS$Scale <- c("liab","liab","liab")
        trait_mean_liab_JU77_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_JU77_P3p_SSSS
      }
      
      liab_JU77_P3p_SSSS <- rbind.data.frame(va_liab_JU77_P3p_SSSS, h2_liab_JU77_P3p_SSSS,Evol_liab_JU77_P3p_SSSS,trait_mean_liab_JU77_P3p_SSSS)
      liab_JU77_P3p_SSSS
    }
    #Summary data scale JU77 P3p
    {
      #Summary va_data_JU77_P3p_SSSS:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_JU77_P3p_SSSS <- rbind(mean(va_data_JU77_CONTROL_P3p_SSSS_mod/2),mean(va_data_JU77_MA_P3p_SSSS_mod/2),mean(va_data_JU77_Vm_P3p_SSSS_mod/2))
        colnames(mean_va_data_JU77_P3p_SSSS) <- c("mean")
        median_va_data_JU77_P3p_SSSS <- rbind(median(va_data_JU77_CONTROL_P3p_SSSS_mod/2),median(va_data_JU77_MA_P3p_SSSS_mod/2),median(va_data_JU77_Vm_P3p_SSSS_mod/2))
        colnames(median_va_data_JU77_P3p_SSSS) <- c("median")
        posterior.mode_va_data_JU77_P3p_SSSS <- rbind(posterior.mode(as.mcmc(va_data_JU77_CONTROL_P3p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_JU77_MA_P3p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_JU77_Vm_P3p_SSSS_mod/2)))
        colnames(posterior.mode_va_data_JU77_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_JU77_P3p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_JU77_CONTROL_P3p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_JU77_MA_P3p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_JU77_Vm_P3p_SSSS_mod/2)))
        colnames(HPDinterval_0.95_va_data_JU77_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_JU77_P3p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_JU77_CONTROL_P3p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU77_MA_P3p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU77_Vm_P3p_SSSS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_JU77_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_JU77_P3p_SSSS <- rbind(effectiveSize(va_data_JU77_CONTROL_P3p_SSSS_mod/2),effectiveSize(va_data_JU77_MA_P3p_SSSS_mod/2),effectiveSize(va_data_JU77_Vm_P3p_SSSS_mod/2))
        colnames(effectiveSize_va_data_JU77_P3p_SSSS) <- c("effectiveSize")
        va_data_JU77_P3p_SSSS <- cbind.data.frame(mean_va_data_JU77_P3p_SSSS,median_va_data_JU77_P3p_SSSS,posterior.mode_va_data_JU77_P3p_SSSS,HPDinterval_0.95_va_data_JU77_P3p_SSSS,HPDinterval_0.83_va_data_JU77_P3p_SSSS,effectiveSize_va_data_JU77_P3p_SSSS)
        rownames(va_data_JU77_P3p_SSSS) <- c("va_data_JU77_CONTROL_P3p_SSSS_mod","va_data_JU77_MA_P3p_SSSS_mod","va_data_JU77_Vm_P3p_SSSS_mod")
        va_data_JU77_P3p_SSSS <- cbind(Models = rownames(va_data_JU77_P3p_SSSS),va_data_JU77_P3p_SSSS)
        rownames(va_data_JU77_P3p_SSSS) <- NULL
        va_data_JU77_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        va_data_JU77_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        va_data_JU77_P3p_SSSS$Measure <- c("Va","Va","Va")
        va_data_JU77_P3p_SSSS$Scale <- c("data","data","data")
        va_data_JU77_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_data_JU77_P3p_SSSS
      }
      
      #Summary h2_data_JU77_P3p_SSSS
      {
        mean_h2_data_JU77_P3p_SSSS <- rbind(mean(h2_data_JU77_CONTROL_P3p_SSSS_mod),mean(h2_data_JU77_MA_P3p_SSSS_mod),mean(h2_data_JU77_Vm_P3p_SSSS_mod))
        colnames(mean_h2_data_JU77_P3p_SSSS) <- c("mean")
        median_h2_data_JU77_P3p_SSSS <- rbind(median(h2_data_JU77_CONTROL_P3p_SSSS_mod),median(h2_data_JU77_MA_P3p_SSSS_mod),median(h2_data_JU77_Vm_P3p_SSSS_mod))
        colnames(median_h2_data_JU77_P3p_SSSS) <- c("median")
        posterior.mode_h2_data_JU77_P3p_SSSS <- rbind(posterior.mode(as.mcmc(h2_data_JU77_CONTROL_P3p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_JU77_MA_P3p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_JU77_Vm_P3p_SSSS_mod)))
        colnames(posterior.mode_h2_data_JU77_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_JU77_P3p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_JU77_CONTROL_P3p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_JU77_MA_P3p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_JU77_Vm_P3p_SSSS_mod)))
        colnames(HPDinterval_0.95_h2_data_JU77_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_JU77_P3p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_JU77_CONTROL_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU77_MA_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU77_Vm_P3p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_JU77_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_JU77_P3p_SSSS <- rbind(effectiveSize(h2_data_JU77_CONTROL_P3p_SSSS_mod),effectiveSize(h2_data_JU77_MA_P3p_SSSS_mod),effectiveSize(h2_data_JU77_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_h2_data_JU77_P3p_SSSS) <- c("effectiveSize")
        h2_data_JU77_P3p_SSSS <- cbind.data.frame(mean_h2_data_JU77_P3p_SSSS,median_h2_data_JU77_P3p_SSSS,posterior.mode_h2_data_JU77_P3p_SSSS,HPDinterval_0.95_h2_data_JU77_P3p_SSSS,HPDinterval_0.83_h2_data_JU77_P3p_SSSS,effectiveSize_h2_data_JU77_P3p_SSSS)
        rownames(h2_data_JU77_P3p_SSSS) <- c("h2_data_JU77_CONTROL_P3p_SSSS_mod","h2_data_JU77_MA_P3p_SSSS_mod","h2_data_JU77_Vm_P3p_SSSS_mod")
        h2_data_JU77_P3p_SSSS <- cbind(Models = rownames(h2_data_JU77_P3p_SSSS),h2_data_JU77_P3p_SSSS)
        rownames(h2_data_JU77_P3p_SSSS) <- NULL
        h2_data_JU77_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        h2_data_JU77_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_data_JU77_P3p_SSSS$Measure <- c("H2","H2","H2")
        h2_data_JU77_P3p_SSSS$Scale <- c("data","data","data")
        h2_data_JU77_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_data_JU77_P3p_SSSS
      }
      
      #Summary Evol_data_JU77_P3p_SSSS
      {
        mean_Evol_data_JU77_P3p_SSSS <- rbind(mean(Evol_data_JU77_CONTROL_P3p_SSSS_mod),mean(Evol_data_JU77_MA_P3p_SSSS_mod),mean(Evol_data_JU77_Vm_P3p_SSSS_mod))
        colnames(mean_Evol_data_JU77_P3p_SSSS) <- c("mean")
        median_Evol_data_JU77_P3p_SSSS <- rbind(median(Evol_data_JU77_CONTROL_P3p_SSSS_mod),median(Evol_data_JU77_MA_P3p_SSSS_mod),median(Evol_data_JU77_Vm_P3p_SSSS_mod))
        colnames(median_Evol_data_JU77_P3p_SSSS) <- c("median")
        posterior.mode_Evol_data_JU77_P3p_SSSS <- rbind(posterior.mode(as.mcmc(Evol_data_JU77_CONTROL_P3p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_JU77_MA_P3p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_JU77_Vm_P3p_SSSS_mod)))
        colnames(posterior.mode_Evol_data_JU77_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_JU77_P3p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_JU77_CONTROL_P3p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_JU77_MA_P3p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_JU77_Vm_P3p_SSSS_mod)))
        colnames(HPDinterval_0.95_Evol_data_JU77_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_JU77_P3p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_JU77_CONTROL_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU77_MA_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU77_Vm_P3p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_JU77_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_JU77_P3p_SSSS <- rbind(effectiveSize(Evol_data_JU77_CONTROL_P3p_SSSS_mod),effectiveSize(Evol_data_JU77_MA_P3p_SSSS_mod),effectiveSize(Evol_data_JU77_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_Evol_data_JU77_P3p_SSSS) <- c("effectiveSize")
        Evol_data_JU77_P3p_SSSS <- cbind.data.frame(mean_Evol_data_JU77_P3p_SSSS,median_Evol_data_JU77_P3p_SSSS,posterior.mode_Evol_data_JU77_P3p_SSSS,HPDinterval_0.95_Evol_data_JU77_P3p_SSSS,HPDinterval_0.83_Evol_data_JU77_P3p_SSSS,effectiveSize_Evol_data_JU77_P3p_SSSS)
        rownames(Evol_data_JU77_P3p_SSSS) <- c("Evol_data_JU77_CONTROL_P3p_SSSS_mod","Evol_data_JU77_MA_P3p_SSSS_mod","Evol_data_JU77_Vm_P3p_SSSS_mod")
        Evol_data_JU77_P3p_SSSS <- cbind(Models = rownames(Evol_data_JU77_P3p_SSSS),Evol_data_JU77_P3p_SSSS)
        rownames(Evol_data_JU77_P3p_SSSS) <- NULL
        Evol_data_JU77_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        Evol_data_JU77_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_data_JU77_P3p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_data_JU77_P3p_SSSS$Scale <- c("data","data","data")
        Evol_data_JU77_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_data_JU77_P3p_SSSS
      }
      
      #Summary trait_mean_data_JU77_P3p_SSSS
      {
        mean_trait_mean_data_JU77_P3p_SSSS <- rbind(mean(trait_mean_data_JU77_CONTROL_P3p_SSSS_mod),mean(trait_mean_data_JU77_MA_P3p_SSSS_mod),mean(trait_mean_data_JU77_Vm_P3p_SSSS_mod))
        colnames(mean_trait_mean_data_JU77_P3p_SSSS) <- c("mean")
        median_trait_mean_data_JU77_P3p_SSSS <- rbind(median(trait_mean_data_JU77_CONTROL_P3p_SSSS_mod),median(trait_mean_data_JU77_MA_P3p_SSSS_mod),median(trait_mean_data_JU77_Vm_P3p_SSSS_mod))
        colnames(median_trait_mean_data_JU77_P3p_SSSS) <- c("median")
        posterior.mode_trait_mean_data_JU77_P3p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_data_JU77_CONTROL_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_JU77_MA_P3p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_JU77_Vm_P3p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_data_JU77_P3p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_JU77_P3p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU77_CONTROL_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_JU77_MA_P3p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_JU77_Vm_P3p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_JU77_P3p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_JU77_P3p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU77_CONTROL_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU77_MA_P3p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU77_Vm_P3p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_JU77_P3p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_JU77_P3p_SSSS <- rbind(effectiveSize(trait_mean_data_JU77_CONTROL_P3p_SSSS_mod),effectiveSize(trait_mean_data_JU77_MA_P3p_SSSS_mod),effectiveSize(trait_mean_data_JU77_Vm_P3p_SSSS_mod))
        colnames(effectiveSize_trait_mean_data_JU77_P3p_SSSS) <- c("effectiveSize")
        trait_mean_data_JU77_P3p_SSSS <- cbind.data.frame(mean_trait_mean_data_JU77_P3p_SSSS,median_trait_mean_data_JU77_P3p_SSSS,posterior.mode_trait_mean_data_JU77_P3p_SSSS,HPDinterval_0.95_trait_mean_data_JU77_P3p_SSSS,HPDinterval_0.83_trait_mean_data_JU77_P3p_SSSS,effectiveSize_trait_mean_data_JU77_P3p_SSSS)
        rownames(trait_mean_data_JU77_P3p_SSSS) <- c("trait_mean_data_JU77_CONTROL_P3p_SSSS_mod","trait_mean_data_JU77_MA_P3p_SSSS_mod","trait_mean_data_JU77_Vm_P3p_SSSS_mod")
        trait_mean_data_JU77_P3p_SSSS <- cbind(Models = rownames(trait_mean_data_JU77_P3p_SSSS),trait_mean_data_JU77_P3p_SSSS)
        rownames(trait_mean_data_JU77_P3p_SSSS) <- NULL
        trait_mean_data_JU77_P3p_SSSS$Pnp <- c("P3.p","P3.p","P3.p")
        trait_mean_data_JU77_P3p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_data_JU77_P3p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_JU77_P3p_SSSS$Scale <- c("data","data","data")
        trait_mean_data_JU77_P3p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_JU77_P3p_SSSS
      }
      
      data_JU77_P3p_SSSS <- rbind.data.frame(va_data_JU77_P3p_SSSS, h2_data_JU77_P3p_SSSS,Evol_data_JU77_P3p_SSSS,trait_mean_data_JU77_P3p_SSSS)
      data_JU77_P3p_SSSS
      
    }
    Vm_JU77_P3p_SSSS <- rbind.data.frame(liab_JU77_P3p_SSSS, data_JU77_P3p_SSSS)
    Vm_JU77_P3p_SSSS$Pnp_fate <- rep("SSSS", 24)
    Vm_JU77_P3p_SSSS
    #remove JU77 P3p_SSSSS models
    {
      remove(JU77_CONTROL_P3p_SSSS_mod)
      remove(JU77_MA_P3p_SSSS_mod)
      remove(JU77_Vm_P3p_SSSS_mod)
    }
  }
  
  ##Summary JU77 P4p----
  {
    #Summary liability scale JU77 P4p
    {
      #Summary va_liab_JU77_P4p_SSSS: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_JU77_P4p_SSSS <- rbind(mean(va_liab_JU77_CONTROL_P4p_SSSS_mod/2),mean(va_liab_JU77_MA_P4p_SSSS_mod/2),mean(va_liab_JU77_Vm_P4p_SSSS_mod/2))
        colnames(mean_va_liab_JU77_P4p_SSSS) <- c("mean")
        median_va_liab_JU77_P4p_SSSS <- rbind(median(va_liab_JU77_CONTROL_P4p_SSSS_mod/2),median(va_liab_JU77_MA_P4p_SSSS_mod/2),median(va_liab_JU77_Vm_P4p_SSSS_mod/2))
        colnames(median_va_liab_JU77_P4p_SSSS) <- c("median")
        posterior.mode_va_liab_JU77_P4p_SSSS <- rbind(posterior.mode(va_liab_JU77_CONTROL_P4p_SSSS_mod/2),posterior.mode(va_liab_JU77_MA_P4p_SSSS_mod/2),posterior.mode(va_liab_JU77_Vm_P4p_SSSS_mod/2))
        colnames(posterior.mode_va_liab_JU77_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_JU77_P4p_SSSS <- rbind(HPDinterval(va_liab_JU77_CONTROL_P4p_SSSS_mod/2),HPDinterval(va_liab_JU77_MA_P4p_SSSS_mod/2),HPDinterval(va_liab_JU77_Vm_P4p_SSSS_mod/2))
        colnames(HPDinterval_0.95_va_liab_JU77_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_JU77_P4p_SSSS <- rbind(HPDinterval(va_liab_JU77_CONTROL_P4p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_JU77_MA_P4p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_JU77_Vm_P4p_SSSS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_JU77_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_JU77_P4p_SSSS <- rbind(effectiveSize(va_liab_JU77_CONTROL_P4p_SSSS_mod/2),effectiveSize(va_liab_JU77_MA_P4p_SSSS_mod/2),effectiveSize(va_liab_JU77_Vm_P4p_SSSS_mod/2))
        colnames(effectiveSize_va_liab_JU77_P4p_SSSS) <- c("effectiveSize")
        va_liab_JU77_P4p_SSSS <- cbind.data.frame(mean_va_liab_JU77_P4p_SSSS,median_va_liab_JU77_P4p_SSSS,posterior.mode_va_liab_JU77_P4p_SSSS,HPDinterval_0.95_va_liab_JU77_P4p_SSSS,HPDinterval_0.83_va_liab_JU77_P4p_SSSS,effectiveSize_va_liab_JU77_P4p_SSSS)
        rownames(va_liab_JU77_P4p_SSSS) <- c("va_liab_JU77_CONTROL_P4p_SSSS_mod","va_liab_JU77_MA_P4p_SSSS_mod","va_liab_JU77_Vm_P4p_SSSS_mod")
        va_liab_JU77_P4p_SSSS <- cbind(Models = rownames(va_liab_JU77_P4p_SSSS),va_liab_JU77_P4p_SSSS)
        rownames(va_liab_JU77_P4p_SSSS) <- NULL
        va_liab_JU77_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        va_liab_JU77_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        va_liab_JU77_P4p_SSSS$Measure <- c("Va","Va","Va")
        va_liab_JU77_P4p_SSSS$Scale <- c("liab","liab","liab")
        va_liab_JU77_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_liab_JU77_P4p_SSSS
      }
      
      #Summary h2_liab_JU77_P4p_SSSS
      {
        mean_h2_liab_JU77_P4p_SSSS <- rbind(mean(h2_liab_JU77_CONTROL_P4p_SSSS_mod),mean(h2_liab_JU77_MA_P4p_SSSS_mod),mean(h2_liab_JU77_Vm_P4p_SSSS_mod))
        colnames(mean_h2_liab_JU77_P4p_SSSS) <- c("mean")
        median_h2_liab_JU77_P4p_SSSS <- rbind(median(h2_liab_JU77_CONTROL_P4p_SSSS_mod),median(h2_liab_JU77_MA_P4p_SSSS_mod),median(h2_liab_JU77_Vm_P4p_SSSS_mod))
        colnames(median_h2_liab_JU77_P4p_SSSS) <- c("median")
        posterior.mode_h2_liab_JU77_P4p_SSSS <- rbind(posterior.mode(h2_liab_JU77_CONTROL_P4p_SSSS_mod),posterior.mode(h2_liab_JU77_MA_P4p_SSSS_mod),posterior.mode(h2_liab_JU77_Vm_P4p_SSSS_mod))
        colnames(posterior.mode_h2_liab_JU77_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_JU77_P4p_SSSS <- rbind(HPDinterval(h2_liab_JU77_CONTROL_P4p_SSSS_mod),HPDinterval(h2_liab_JU77_MA_P4p_SSSS_mod),HPDinterval(h2_liab_JU77_Vm_P4p_SSSS_mod))
        colnames(HPDinterval_0.95_h2_liab_JU77_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_JU77_P4p_SSSS <- rbind(HPDinterval(h2_liab_JU77_CONTROL_P4p_SSSS_mod,prob=.83),HPDinterval(h2_liab_JU77_MA_P4p_SSSS_mod,prob=.83),HPDinterval(h2_liab_JU77_Vm_P4p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_JU77_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_JU77_P4p_SSSS <- rbind(effectiveSize(h2_liab_JU77_CONTROL_P4p_SSSS_mod),effectiveSize(h2_liab_JU77_MA_P4p_SSSS_mod),effectiveSize(h2_liab_JU77_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_h2_liab_JU77_P4p_SSSS) <- c("effectiveSize")
        h2_liab_JU77_P4p_SSSS <- cbind.data.frame(mean_h2_liab_JU77_P4p_SSSS,median_h2_liab_JU77_P4p_SSSS,posterior.mode_h2_liab_JU77_P4p_SSSS,HPDinterval_0.95_h2_liab_JU77_P4p_SSSS,HPDinterval_0.83_h2_liab_JU77_P4p_SSSS,effectiveSize_h2_liab_JU77_P4p_SSSS)
        rownames(h2_liab_JU77_P4p_SSSS) <- c("h2_liab_JU77_CONTROL_P4p_SSSS_mod","h2_liab_JU77_MA_P4p_SSSS_mod","h2_liab_JU77_Vm_P4p_SSSS_mod")
        h2_liab_JU77_P4p_SSSS <- cbind(Models = rownames(h2_liab_JU77_P4p_SSSS),h2_liab_JU77_P4p_SSSS)
        rownames(h2_liab_JU77_P4p_SSSS) <- NULL
        h2_liab_JU77_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        h2_liab_JU77_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_liab_JU77_P4p_SSSS$Measure <- c("H2","H2","H2")
        h2_liab_JU77_P4p_SSSS$Scale <- c("liab","liab","liab")
        h2_liab_JU77_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_liab_JU77_P4p_SSSS
      }
      
      #Summary Evol_liab_JU77_P4p_SSSS
      {
        mean_Evol_liab_JU77_P4p_SSSS <- rbind(mean(Evol_liab_JU77_CONTROL_P4p_SSSS_mod),mean(Evol_liab_JU77_MA_P4p_SSSS_mod),mean(Evol_liab_JU77_Vm_P4p_SSSS_mod))
        colnames(mean_Evol_liab_JU77_P4p_SSSS) <- c("mean")
        median_Evol_liab_JU77_P4p_SSSS <- rbind(median(Evol_liab_JU77_CONTROL_P4p_SSSS_mod),median(Evol_liab_JU77_MA_P4p_SSSS_mod),median(Evol_liab_JU77_Vm_P4p_SSSS_mod))
        colnames(median_Evol_liab_JU77_P4p_SSSS) <- c("median")
        posterior.mode_Evol_liab_JU77_P4p_SSSS <- rbind(posterior.mode(Evol_liab_JU77_CONTROL_P4p_SSSS_mod),posterior.mode(Evol_liab_JU77_MA_P4p_SSSS_mod),posterior.mode(Evol_liab_JU77_Vm_P4p_SSSS_mod))
        colnames(posterior.mode_Evol_liab_JU77_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_JU77_P4p_SSSS <- rbind(HPDinterval(Evol_liab_JU77_CONTROL_P4p_SSSS_mod),HPDinterval(Evol_liab_JU77_MA_P4p_SSSS_mod),HPDinterval(Evol_liab_JU77_Vm_P4p_SSSS_mod))
        colnames(HPDinterval_0.95_Evol_liab_JU77_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_JU77_P4p_SSSS <- rbind(HPDinterval(Evol_liab_JU77_CONTROL_P4p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_JU77_MA_P4p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_JU77_Vm_P4p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_JU77_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_JU77_P4p_SSSS <- rbind(effectiveSize(Evol_liab_JU77_CONTROL_P4p_SSSS_mod),effectiveSize(Evol_liab_JU77_MA_P4p_SSSS_mod),effectiveSize(Evol_liab_JU77_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_Evol_liab_JU77_P4p_SSSS) <- c("effectiveSize")
        Evol_liab_JU77_P4p_SSSS <- cbind.data.frame(mean_Evol_liab_JU77_P4p_SSSS,median_Evol_liab_JU77_P4p_SSSS,posterior.mode_Evol_liab_JU77_P4p_SSSS,HPDinterval_0.95_Evol_liab_JU77_P4p_SSSS,HPDinterval_0.83_Evol_liab_JU77_P4p_SSSS,effectiveSize_Evol_liab_JU77_P4p_SSSS)
        rownames(Evol_liab_JU77_P4p_SSSS) <- c("Evol_liab_JU77_CONTROL_P4p_SSSS_mod","Evol_liab_JU77_MA_P4p_SSSS_mod","Evol_liab_JU77_Vm_P4p_SSSS_mod")
        Evol_liab_JU77_P4p_SSSS <- cbind(Models = rownames(Evol_liab_JU77_P4p_SSSS),Evol_liab_JU77_P4p_SSSS)
        rownames(Evol_liab_JU77_P4p_SSSS) <- NULL
        Evol_liab_JU77_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        Evol_liab_JU77_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_liab_JU77_P4p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_JU77_P4p_SSSS$Scale <- c("liab","liab","liab")
        Evol_liab_JU77_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_JU77_P4p_SSSS
      }
      
      #Summary trait_mean_liab_JU77_P4p_SSSS
      {
        mean_trait_mean_liab_JU77_P4p_SSSS <- rbind(mean(trait_mean_liab_JU77_CONTROL_P4p_SSSS_mod),mean(trait_mean_liab_JU77_MA_P4p_SSSS_mod),mean(trait_mean_liab_JU77_Vm_P4p_SSSS_mod))
        colnames(mean_trait_mean_liab_JU77_P4p_SSSS) <- c("mean")
        median_trait_mean_liab_JU77_P4p_SSSS <- rbind(median(trait_mean_liab_JU77_CONTROL_P4p_SSSS_mod),median(trait_mean_liab_JU77_MA_P4p_SSSS_mod),median(trait_mean_liab_JU77_Vm_P4p_SSSS_mod))
        colnames(median_trait_mean_liab_JU77_P4p_SSSS) <- c("median")
        posterior.mode_trait_mean_liab_JU77_P4p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_JU77_CONTROL_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU77_MA_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU77_Vm_P4p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_liab_JU77_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_JU77_P4p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU77_CONTROL_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU77_MA_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU77_Vm_P4p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_JU77_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_JU77_P4p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU77_CONTROL_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU77_MA_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU77_Vm_P4p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_JU77_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_JU77_P4p_SSSS <- rbind(effectiveSize(trait_mean_liab_JU77_CONTROL_P4p_SSSS_mod),effectiveSize(trait_mean_liab_JU77_MA_P4p_SSSS_mod),effectiveSize(trait_mean_liab_JU77_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_trait_mean_liab_JU77_P4p_SSSS) <- c("effectiveSize")
        trait_mean_liab_JU77_P4p_SSSS <- cbind.data.frame(mean_trait_mean_liab_JU77_P4p_SSSS,median_trait_mean_liab_JU77_P4p_SSSS,posterior.mode_trait_mean_liab_JU77_P4p_SSSS,HPDinterval_0.95_trait_mean_liab_JU77_P4p_SSSS,HPDinterval_0.83_trait_mean_liab_JU77_P4p_SSSS,effectiveSize_trait_mean_liab_JU77_P4p_SSSS)
        rownames(trait_mean_liab_JU77_P4p_SSSS) <- c("trait_mean_liab_JU77_CONTROL_P4p_SSSS_mod","trait_mean_liab_JU77_MA_P4p_SSSS_mod","trait_mean_liab_JU77_Vm_P4p_SSSS_mod")
        trait_mean_liab_JU77_P4p_SSSS <- cbind(Models = rownames(trait_mean_liab_JU77_P4p_SSSS),trait_mean_liab_JU77_P4p_SSSS)
        rownames(trait_mean_liab_JU77_P4p_SSSS) <- NULL
        trait_mean_liab_JU77_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        trait_mean_liab_JU77_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_JU77_P4p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_JU77_P4p_SSSS$Scale <- c("liab","liab","liab")
        trait_mean_liab_JU77_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_JU77_P4p_SSSS
      }
      
      liab_JU77_P4p_SSSS <- rbind.data.frame(va_liab_JU77_P4p_SSSS, h2_liab_JU77_P4p_SSSS,Evol_liab_JU77_P4p_SSSS,trait_mean_liab_JU77_P4p_SSSS)
      liab_JU77_P4p_SSSS
    }
    #Summary data scale JU77 P4p
    {
      #Summary va_data_JU77_P4p_SSSS:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_JU77_P4p_SSSS <- rbind(mean(va_data_JU77_CONTROL_P4p_SSSS_mod/2),mean(va_data_JU77_MA_P4p_SSSS_mod/2),mean(va_data_JU77_Vm_P4p_SSSS_mod/2))
        colnames(mean_va_data_JU77_P4p_SSSS) <- c("mean")
        median_va_data_JU77_P4p_SSSS <- rbind(median(va_data_JU77_CONTROL_P4p_SSSS_mod/2),median(va_data_JU77_MA_P4p_SSSS_mod/2),median(va_data_JU77_Vm_P4p_SSSS_mod/2))
        colnames(median_va_data_JU77_P4p_SSSS) <- c("median")
        posterior.mode_va_data_JU77_P4p_SSSS <- rbind(posterior.mode(as.mcmc(va_data_JU77_CONTROL_P4p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_JU77_MA_P4p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_JU77_Vm_P4p_SSSS_mod/2)))
        colnames(posterior.mode_va_data_JU77_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_JU77_P4p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_JU77_CONTROL_P4p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_JU77_MA_P4p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_JU77_Vm_P4p_SSSS_mod/2)))
        colnames(HPDinterval_0.95_va_data_JU77_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_JU77_P4p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_JU77_CONTROL_P4p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU77_MA_P4p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU77_Vm_P4p_SSSS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_JU77_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_JU77_P4p_SSSS <- rbind(effectiveSize(va_data_JU77_CONTROL_P4p_SSSS_mod/2),effectiveSize(va_data_JU77_MA_P4p_SSSS_mod/2),effectiveSize(va_data_JU77_Vm_P4p_SSSS_mod/2))
        colnames(effectiveSize_va_data_JU77_P4p_SSSS) <- c("effectiveSize")
        va_data_JU77_P4p_SSSS <- cbind.data.frame(mean_va_data_JU77_P4p_SSSS,median_va_data_JU77_P4p_SSSS,posterior.mode_va_data_JU77_P4p_SSSS,HPDinterval_0.95_va_data_JU77_P4p_SSSS,HPDinterval_0.83_va_data_JU77_P4p_SSSS,effectiveSize_va_data_JU77_P4p_SSSS)
        rownames(va_data_JU77_P4p_SSSS) <- c("va_data_JU77_CONTROL_P4p_SSSS_mod","va_data_JU77_MA_P4p_SSSS_mod","va_data_JU77_Vm_P4p_SSSS_mod")
        va_data_JU77_P4p_SSSS <- cbind(Models = rownames(va_data_JU77_P4p_SSSS),va_data_JU77_P4p_SSSS)
        rownames(va_data_JU77_P4p_SSSS) <- NULL
        va_data_JU77_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        va_data_JU77_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        va_data_JU77_P4p_SSSS$Measure <- c("Va","Va","Va")
        va_data_JU77_P4p_SSSS$Scale <- c("data","data","data")
        va_data_JU77_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_data_JU77_P4p_SSSS
      }
      
      #Summary h2_data_JU77_P4p_SSSS
      {
        mean_h2_data_JU77_P4p_SSSS <- rbind(mean(h2_data_JU77_CONTROL_P4p_SSSS_mod),mean(h2_data_JU77_MA_P4p_SSSS_mod),mean(h2_data_JU77_Vm_P4p_SSSS_mod))
        colnames(mean_h2_data_JU77_P4p_SSSS) <- c("mean")
        median_h2_data_JU77_P4p_SSSS <- rbind(median(h2_data_JU77_CONTROL_P4p_SSSS_mod),median(h2_data_JU77_MA_P4p_SSSS_mod),median(h2_data_JU77_Vm_P4p_SSSS_mod))
        colnames(median_h2_data_JU77_P4p_SSSS) <- c("median")
        posterior.mode_h2_data_JU77_P4p_SSSS <- rbind(posterior.mode(as.mcmc(h2_data_JU77_CONTROL_P4p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_JU77_MA_P4p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_JU77_Vm_P4p_SSSS_mod)))
        colnames(posterior.mode_h2_data_JU77_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_JU77_P4p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_JU77_CONTROL_P4p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_JU77_MA_P4p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_JU77_Vm_P4p_SSSS_mod)))
        colnames(HPDinterval_0.95_h2_data_JU77_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_JU77_P4p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_JU77_CONTROL_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU77_MA_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU77_Vm_P4p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_JU77_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_JU77_P4p_SSSS <- rbind(effectiveSize(h2_data_JU77_CONTROL_P4p_SSSS_mod),effectiveSize(h2_data_JU77_MA_P4p_SSSS_mod),effectiveSize(h2_data_JU77_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_h2_data_JU77_P4p_SSSS) <- c("effectiveSize")
        h2_data_JU77_P4p_SSSS <- cbind.data.frame(mean_h2_data_JU77_P4p_SSSS,median_h2_data_JU77_P4p_SSSS,posterior.mode_h2_data_JU77_P4p_SSSS,HPDinterval_0.95_h2_data_JU77_P4p_SSSS,HPDinterval_0.83_h2_data_JU77_P4p_SSSS,effectiveSize_h2_data_JU77_P4p_SSSS)
        rownames(h2_data_JU77_P4p_SSSS) <- c("h2_data_JU77_CONTROL_P4p_SSSS_mod","h2_data_JU77_MA_P4p_SSSS_mod","h2_data_JU77_Vm_P4p_SSSS_mod")
        h2_data_JU77_P4p_SSSS <- cbind(Models = rownames(h2_data_JU77_P4p_SSSS),h2_data_JU77_P4p_SSSS)
        rownames(h2_data_JU77_P4p_SSSS) <- NULL
        h2_data_JU77_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        h2_data_JU77_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_data_JU77_P4p_SSSS$Measure <- c("H2","H2","H2")
        h2_data_JU77_P4p_SSSS$Scale <- c("data","data","data")
        h2_data_JU77_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_data_JU77_P4p_SSSS
      }
      
      #Summary Evol_data_JU77_P4p_SSSS
      {
        mean_Evol_data_JU77_P4p_SSSS <- rbind(mean(Evol_data_JU77_CONTROL_P4p_SSSS_mod),mean(Evol_data_JU77_MA_P4p_SSSS_mod),mean(Evol_data_JU77_Vm_P4p_SSSS_mod))
        colnames(mean_Evol_data_JU77_P4p_SSSS) <- c("mean")
        median_Evol_data_JU77_P4p_SSSS <- rbind(median(Evol_data_JU77_CONTROL_P4p_SSSS_mod),median(Evol_data_JU77_MA_P4p_SSSS_mod),median(Evol_data_JU77_Vm_P4p_SSSS_mod))
        colnames(median_Evol_data_JU77_P4p_SSSS) <- c("median")
        posterior.mode_Evol_data_JU77_P4p_SSSS <- rbind(posterior.mode(as.mcmc(Evol_data_JU77_CONTROL_P4p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_JU77_MA_P4p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_JU77_Vm_P4p_SSSS_mod)))
        colnames(posterior.mode_Evol_data_JU77_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_JU77_P4p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_JU77_CONTROL_P4p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_JU77_MA_P4p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_JU77_Vm_P4p_SSSS_mod)))
        colnames(HPDinterval_0.95_Evol_data_JU77_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_JU77_P4p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_JU77_CONTROL_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU77_MA_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU77_Vm_P4p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_JU77_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_JU77_P4p_SSSS <- rbind(effectiveSize(Evol_data_JU77_CONTROL_P4p_SSSS_mod),effectiveSize(Evol_data_JU77_MA_P4p_SSSS_mod),effectiveSize(Evol_data_JU77_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_Evol_data_JU77_P4p_SSSS) <- c("effectiveSize")
        Evol_data_JU77_P4p_SSSS <- cbind.data.frame(mean_Evol_data_JU77_P4p_SSSS,median_Evol_data_JU77_P4p_SSSS,posterior.mode_Evol_data_JU77_P4p_SSSS,HPDinterval_0.95_Evol_data_JU77_P4p_SSSS,HPDinterval_0.83_Evol_data_JU77_P4p_SSSS,effectiveSize_Evol_data_JU77_P4p_SSSS)
        rownames(Evol_data_JU77_P4p_SSSS) <- c("Evol_data_JU77_CONTROL_P4p_SSSS_mod","Evol_data_JU77_MA_P4p_SSSS_mod","Evol_data_JU77_Vm_P4p_SSSS_mod")
        Evol_data_JU77_P4p_SSSS <- cbind(Models = rownames(Evol_data_JU77_P4p_SSSS),Evol_data_JU77_P4p_SSSS)
        rownames(Evol_data_JU77_P4p_SSSS) <- NULL
        Evol_data_JU77_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        Evol_data_JU77_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_data_JU77_P4p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_data_JU77_P4p_SSSS$Scale <- c("data","data","data")
        Evol_data_JU77_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_data_JU77_P4p_SSSS
      }
      
      #Summary trait_mean_data_JU77_P4p_SSSS
      {
        mean_trait_mean_data_JU77_P4p_SSSS <- rbind(mean(trait_mean_data_JU77_CONTROL_P4p_SSSS_mod),mean(trait_mean_data_JU77_MA_P4p_SSSS_mod),mean(trait_mean_data_JU77_Vm_P4p_SSSS_mod))
        colnames(mean_trait_mean_data_JU77_P4p_SSSS) <- c("mean")
        median_trait_mean_data_JU77_P4p_SSSS <- rbind(median(trait_mean_data_JU77_CONTROL_P4p_SSSS_mod),median(trait_mean_data_JU77_MA_P4p_SSSS_mod),median(trait_mean_data_JU77_Vm_P4p_SSSS_mod))
        colnames(median_trait_mean_data_JU77_P4p_SSSS) <- c("median")
        posterior.mode_trait_mean_data_JU77_P4p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_data_JU77_CONTROL_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_JU77_MA_P4p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_JU77_Vm_P4p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_data_JU77_P4p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_JU77_P4p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU77_CONTROL_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_JU77_MA_P4p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_JU77_Vm_P4p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_JU77_P4p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_JU77_P4p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU77_CONTROL_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU77_MA_P4p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU77_Vm_P4p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_JU77_P4p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_JU77_P4p_SSSS <- rbind(effectiveSize(trait_mean_data_JU77_CONTROL_P4p_SSSS_mod),effectiveSize(trait_mean_data_JU77_MA_P4p_SSSS_mod),effectiveSize(trait_mean_data_JU77_Vm_P4p_SSSS_mod))
        colnames(effectiveSize_trait_mean_data_JU77_P4p_SSSS) <- c("effectiveSize")
        trait_mean_data_JU77_P4p_SSSS <- cbind.data.frame(mean_trait_mean_data_JU77_P4p_SSSS,median_trait_mean_data_JU77_P4p_SSSS,posterior.mode_trait_mean_data_JU77_P4p_SSSS,HPDinterval_0.95_trait_mean_data_JU77_P4p_SSSS,HPDinterval_0.83_trait_mean_data_JU77_P4p_SSSS,effectiveSize_trait_mean_data_JU77_P4p_SSSS)
        rownames(trait_mean_data_JU77_P4p_SSSS) <- c("trait_mean_data_JU77_CONTROL_P4p_SSSS_mod","trait_mean_data_JU77_MA_P4p_SSSS_mod","trait_mean_data_JU77_Vm_P4p_SSSS_mod")
        trait_mean_data_JU77_P4p_SSSS <- cbind(Models = rownames(trait_mean_data_JU77_P4p_SSSS),trait_mean_data_JU77_P4p_SSSS)
        rownames(trait_mean_data_JU77_P4p_SSSS) <- NULL
        trait_mean_data_JU77_P4p_SSSS$Pnp <- c("P4.p","P4.p","P4.p")
        trait_mean_data_JU77_P4p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_data_JU77_P4p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_JU77_P4p_SSSS$Scale <- c("data","data","data")
        trait_mean_data_JU77_P4p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_JU77_P4p_SSSS
      }
      
      data_JU77_P4p_SSSS <- rbind.data.frame(va_data_JU77_P4p_SSSS, h2_data_JU77_P4p_SSSS,Evol_data_JU77_P4p_SSSS,trait_mean_data_JU77_P4p_SSSS)
      data_JU77_P4p_SSSS
      
    }
    Vm_JU77_P4p_SSSS <- rbind.data.frame(liab_JU77_P4p_SSSS, data_JU77_P4p_SSSS)
    Vm_JU77_P4p_SSSS$Pnp_fate <- rep("SSSS", 24)
    Vm_JU77_P4p_SSSS
    #remove JU77 P4p_SSSSS models
    {
      remove(JU77_CONTROL_P4p_SSSS_mod)
      remove(JU77_MA_P4p_SSSS_mod)
      remove(JU77_Vm_P4p_SSSS_mod)
    }
  }
  
  ##Summary JU77 P8p----
  {
    #Summary liability scale JU77 P8p
    {
      #Summary va_liab_JU77_P8p_SSSS: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_JU77_P8p_SSSS <- rbind(mean(va_liab_JU77_CONTROL_P8p_SSSS_mod/2),mean(va_liab_JU77_MA_P8p_SSSS_mod/2),mean(va_liab_JU77_Vm_P8p_SSSS_mod/2))
        colnames(mean_va_liab_JU77_P8p_SSSS) <- c("mean")
        median_va_liab_JU77_P8p_SSSS <- rbind(median(va_liab_JU77_CONTROL_P8p_SSSS_mod/2),median(va_liab_JU77_MA_P8p_SSSS_mod/2),median(va_liab_JU77_Vm_P8p_SSSS_mod/2))
        colnames(median_va_liab_JU77_P8p_SSSS) <- c("median")
        posterior.mode_va_liab_JU77_P8p_SSSS <- rbind(posterior.mode(va_liab_JU77_CONTROL_P8p_SSSS_mod/2),posterior.mode(va_liab_JU77_MA_P8p_SSSS_mod/2),posterior.mode(va_liab_JU77_Vm_P8p_SSSS_mod/2))
        colnames(posterior.mode_va_liab_JU77_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_JU77_P8p_SSSS <- rbind(HPDinterval(va_liab_JU77_CONTROL_P8p_SSSS_mod/2),HPDinterval(va_liab_JU77_MA_P8p_SSSS_mod/2),HPDinterval(va_liab_JU77_Vm_P8p_SSSS_mod/2))
        colnames(HPDinterval_0.95_va_liab_JU77_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_JU77_P8p_SSSS <- rbind(HPDinterval(va_liab_JU77_CONTROL_P8p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_JU77_MA_P8p_SSSS_mod/2,prob=.83),HPDinterval(va_liab_JU77_Vm_P8p_SSSS_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_JU77_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_JU77_P8p_SSSS <- rbind(effectiveSize(va_liab_JU77_CONTROL_P8p_SSSS_mod/2),effectiveSize(va_liab_JU77_MA_P8p_SSSS_mod/2),effectiveSize(va_liab_JU77_Vm_P8p_SSSS_mod/2))
        colnames(effectiveSize_va_liab_JU77_P8p_SSSS) <- c("effectiveSize")
        va_liab_JU77_P8p_SSSS <- cbind.data.frame(mean_va_liab_JU77_P8p_SSSS,median_va_liab_JU77_P8p_SSSS,posterior.mode_va_liab_JU77_P8p_SSSS,HPDinterval_0.95_va_liab_JU77_P8p_SSSS,HPDinterval_0.83_va_liab_JU77_P8p_SSSS,effectiveSize_va_liab_JU77_P8p_SSSS)
        rownames(va_liab_JU77_P8p_SSSS) <- c("va_liab_JU77_CONTROL_P8p_SSSS_mod","va_liab_JU77_MA_P8p_SSSS_mod","va_liab_JU77_Vm_P8p_SSSS_mod")
        va_liab_JU77_P8p_SSSS <- cbind(Models = rownames(va_liab_JU77_P8p_SSSS),va_liab_JU77_P8p_SSSS)
        rownames(va_liab_JU77_P8p_SSSS) <- NULL
        va_liab_JU77_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        va_liab_JU77_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        va_liab_JU77_P8p_SSSS$Measure <- c("Va","Va","Va")
        va_liab_JU77_P8p_SSSS$Scale <- c("liab","liab","liab")
        va_liab_JU77_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_liab_JU77_P8p_SSSS
      }
      
      #Summary h2_liab_JU77_P8p_SSSS
      {
        mean_h2_liab_JU77_P8p_SSSS <- rbind(mean(h2_liab_JU77_CONTROL_P8p_SSSS_mod),mean(h2_liab_JU77_MA_P8p_SSSS_mod),mean(h2_liab_JU77_Vm_P8p_SSSS_mod))
        colnames(mean_h2_liab_JU77_P8p_SSSS) <- c("mean")
        median_h2_liab_JU77_P8p_SSSS <- rbind(median(h2_liab_JU77_CONTROL_P8p_SSSS_mod),median(h2_liab_JU77_MA_P8p_SSSS_mod),median(h2_liab_JU77_Vm_P8p_SSSS_mod))
        colnames(median_h2_liab_JU77_P8p_SSSS) <- c("median")
        posterior.mode_h2_liab_JU77_P8p_SSSS <- rbind(posterior.mode(h2_liab_JU77_CONTROL_P8p_SSSS_mod),posterior.mode(h2_liab_JU77_MA_P8p_SSSS_mod),posterior.mode(h2_liab_JU77_Vm_P8p_SSSS_mod))
        colnames(posterior.mode_h2_liab_JU77_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_JU77_P8p_SSSS <- rbind(HPDinterval(h2_liab_JU77_CONTROL_P8p_SSSS_mod),HPDinterval(h2_liab_JU77_MA_P8p_SSSS_mod),HPDinterval(h2_liab_JU77_Vm_P8p_SSSS_mod))
        colnames(HPDinterval_0.95_h2_liab_JU77_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_JU77_P8p_SSSS <- rbind(HPDinterval(h2_liab_JU77_CONTROL_P8p_SSSS_mod,prob=.83),HPDinterval(h2_liab_JU77_MA_P8p_SSSS_mod,prob=.83),HPDinterval(h2_liab_JU77_Vm_P8p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_JU77_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_JU77_P8p_SSSS <- rbind(effectiveSize(h2_liab_JU77_CONTROL_P8p_SSSS_mod),effectiveSize(h2_liab_JU77_MA_P8p_SSSS_mod),effectiveSize(h2_liab_JU77_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_h2_liab_JU77_P8p_SSSS) <- c("effectiveSize")
        h2_liab_JU77_P8p_SSSS <- cbind.data.frame(mean_h2_liab_JU77_P8p_SSSS,median_h2_liab_JU77_P8p_SSSS,posterior.mode_h2_liab_JU77_P8p_SSSS,HPDinterval_0.95_h2_liab_JU77_P8p_SSSS,HPDinterval_0.83_h2_liab_JU77_P8p_SSSS,effectiveSize_h2_liab_JU77_P8p_SSSS)
        rownames(h2_liab_JU77_P8p_SSSS) <- c("h2_liab_JU77_CONTROL_P8p_SSSS_mod","h2_liab_JU77_MA_P8p_SSSS_mod","h2_liab_JU77_Vm_P8p_SSSS_mod")
        h2_liab_JU77_P8p_SSSS <- cbind(Models = rownames(h2_liab_JU77_P8p_SSSS),h2_liab_JU77_P8p_SSSS)
        rownames(h2_liab_JU77_P8p_SSSS) <- NULL
        h2_liab_JU77_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        h2_liab_JU77_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_liab_JU77_P8p_SSSS$Measure <- c("H2","H2","H2")
        h2_liab_JU77_P8p_SSSS$Scale <- c("liab","liab","liab")
        h2_liab_JU77_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_liab_JU77_P8p_SSSS
      }
      
      #Summary Evol_liab_JU77_P8p_SSSS
      {
        mean_Evol_liab_JU77_P8p_SSSS <- rbind(mean(Evol_liab_JU77_CONTROL_P8p_SSSS_mod),mean(Evol_liab_JU77_MA_P8p_SSSS_mod),mean(Evol_liab_JU77_Vm_P8p_SSSS_mod))
        colnames(mean_Evol_liab_JU77_P8p_SSSS) <- c("mean")
        median_Evol_liab_JU77_P8p_SSSS <- rbind(median(Evol_liab_JU77_CONTROL_P8p_SSSS_mod),median(Evol_liab_JU77_MA_P8p_SSSS_mod),median(Evol_liab_JU77_Vm_P8p_SSSS_mod))
        colnames(median_Evol_liab_JU77_P8p_SSSS) <- c("median")
        posterior.mode_Evol_liab_JU77_P8p_SSSS <- rbind(posterior.mode(Evol_liab_JU77_CONTROL_P8p_SSSS_mod),posterior.mode(Evol_liab_JU77_MA_P8p_SSSS_mod),posterior.mode(Evol_liab_JU77_Vm_P8p_SSSS_mod))
        colnames(posterior.mode_Evol_liab_JU77_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_JU77_P8p_SSSS <- rbind(HPDinterval(Evol_liab_JU77_CONTROL_P8p_SSSS_mod),HPDinterval(Evol_liab_JU77_MA_P8p_SSSS_mod),HPDinterval(Evol_liab_JU77_Vm_P8p_SSSS_mod))
        colnames(HPDinterval_0.95_Evol_liab_JU77_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_JU77_P8p_SSSS <- rbind(HPDinterval(Evol_liab_JU77_CONTROL_P8p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_JU77_MA_P8p_SSSS_mod,prob=.83),HPDinterval(Evol_liab_JU77_Vm_P8p_SSSS_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_JU77_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_JU77_P8p_SSSS <- rbind(effectiveSize(Evol_liab_JU77_CONTROL_P8p_SSSS_mod),effectiveSize(Evol_liab_JU77_MA_P8p_SSSS_mod),effectiveSize(Evol_liab_JU77_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_Evol_liab_JU77_P8p_SSSS) <- c("effectiveSize")
        Evol_liab_JU77_P8p_SSSS <- cbind.data.frame(mean_Evol_liab_JU77_P8p_SSSS,median_Evol_liab_JU77_P8p_SSSS,posterior.mode_Evol_liab_JU77_P8p_SSSS,HPDinterval_0.95_Evol_liab_JU77_P8p_SSSS,HPDinterval_0.83_Evol_liab_JU77_P8p_SSSS,effectiveSize_Evol_liab_JU77_P8p_SSSS)
        rownames(Evol_liab_JU77_P8p_SSSS) <- c("Evol_liab_JU77_CONTROL_P8p_SSSS_mod","Evol_liab_JU77_MA_P8p_SSSS_mod","Evol_liab_JU77_Vm_P8p_SSSS_mod")
        Evol_liab_JU77_P8p_SSSS <- cbind(Models = rownames(Evol_liab_JU77_P8p_SSSS),Evol_liab_JU77_P8p_SSSS)
        rownames(Evol_liab_JU77_P8p_SSSS) <- NULL
        Evol_liab_JU77_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        Evol_liab_JU77_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_liab_JU77_P8p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_liab_JU77_P8p_SSSS$Scale <- c("liab","liab","liab")
        Evol_liab_JU77_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_liab_JU77_P8p_SSSS
      }
      
      #Summary trait_mean_liab_JU77_P8p_SSSS
      {
        mean_trait_mean_liab_JU77_P8p_SSSS <- rbind(mean(trait_mean_liab_JU77_CONTROL_P8p_SSSS_mod),mean(trait_mean_liab_JU77_MA_P8p_SSSS_mod),mean(trait_mean_liab_JU77_Vm_P8p_SSSS_mod))
        colnames(mean_trait_mean_liab_JU77_P8p_SSSS) <- c("mean")
        median_trait_mean_liab_JU77_P8p_SSSS <- rbind(median(trait_mean_liab_JU77_CONTROL_P8p_SSSS_mod),median(trait_mean_liab_JU77_MA_P8p_SSSS_mod),median(trait_mean_liab_JU77_Vm_P8p_SSSS_mod))
        colnames(median_trait_mean_liab_JU77_P8p_SSSS) <- c("median")
        posterior.mode_trait_mean_liab_JU77_P8p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_liab_JU77_CONTROL_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU77_MA_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU77_Vm_P8p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_liab_JU77_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_JU77_P8p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU77_CONTROL_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU77_MA_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU77_Vm_P8p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_JU77_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_JU77_P8p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU77_CONTROL_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU77_MA_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU77_Vm_P8p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_JU77_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_JU77_P8p_SSSS <- rbind(effectiveSize(trait_mean_liab_JU77_CONTROL_P8p_SSSS_mod),effectiveSize(trait_mean_liab_JU77_MA_P8p_SSSS_mod),effectiveSize(trait_mean_liab_JU77_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_trait_mean_liab_JU77_P8p_SSSS) <- c("effectiveSize")
        trait_mean_liab_JU77_P8p_SSSS <- cbind.data.frame(mean_trait_mean_liab_JU77_P8p_SSSS,median_trait_mean_liab_JU77_P8p_SSSS,posterior.mode_trait_mean_liab_JU77_P8p_SSSS,HPDinterval_0.95_trait_mean_liab_JU77_P8p_SSSS,HPDinterval_0.83_trait_mean_liab_JU77_P8p_SSSS,effectiveSize_trait_mean_liab_JU77_P8p_SSSS)
        rownames(trait_mean_liab_JU77_P8p_SSSS) <- c("trait_mean_liab_JU77_CONTROL_P8p_SSSS_mod","trait_mean_liab_JU77_MA_P8p_SSSS_mod","trait_mean_liab_JU77_Vm_P8p_SSSS_mod")
        trait_mean_liab_JU77_P8p_SSSS <- cbind(Models = rownames(trait_mean_liab_JU77_P8p_SSSS),trait_mean_liab_JU77_P8p_SSSS)
        rownames(trait_mean_liab_JU77_P8p_SSSS) <- NULL
        trait_mean_liab_JU77_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        trait_mean_liab_JU77_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_JU77_P8p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_JU77_P8p_SSSS$Scale <- c("liab","liab","liab")
        trait_mean_liab_JU77_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_JU77_P8p_SSSS
      }
      
      liab_JU77_P8p_SSSS <- rbind.data.frame(va_liab_JU77_P8p_SSSS, h2_liab_JU77_P8p_SSSS,Evol_liab_JU77_P8p_SSSS,trait_mean_liab_JU77_P8p_SSSS)
      liab_JU77_P8p_SSSS
    }
    #Summary data scale JU77 P8p
    {
      #Summary va_data_JU77_P8p_SSSS:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_JU77_P8p_SSSS <- rbind(mean(va_data_JU77_CONTROL_P8p_SSSS_mod/2),mean(va_data_JU77_MA_P8p_SSSS_mod/2),mean(va_data_JU77_Vm_P8p_SSSS_mod/2))
        colnames(mean_va_data_JU77_P8p_SSSS) <- c("mean")
        median_va_data_JU77_P8p_SSSS <- rbind(median(va_data_JU77_CONTROL_P8p_SSSS_mod/2),median(va_data_JU77_MA_P8p_SSSS_mod/2),median(va_data_JU77_Vm_P8p_SSSS_mod/2))
        colnames(median_va_data_JU77_P8p_SSSS) <- c("median")
        posterior.mode_va_data_JU77_P8p_SSSS <- rbind(posterior.mode(as.mcmc(va_data_JU77_CONTROL_P8p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_JU77_MA_P8p_SSSS_mod/2)),posterior.mode(as.mcmc(va_data_JU77_Vm_P8p_SSSS_mod/2)))
        colnames(posterior.mode_va_data_JU77_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_va_data_JU77_P8p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_JU77_CONTROL_P8p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_JU77_MA_P8p_SSSS_mod/2)),HPDinterval(as.mcmc(va_data_JU77_Vm_P8p_SSSS_mod/2)))
        colnames(HPDinterval_0.95_va_data_JU77_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_JU77_P8p_SSSS <- rbind(HPDinterval(as.mcmc(va_data_JU77_CONTROL_P8p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU77_MA_P8p_SSSS_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU77_Vm_P8p_SSSS_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_JU77_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_JU77_P8p_SSSS <- rbind(effectiveSize(va_data_JU77_CONTROL_P8p_SSSS_mod/2),effectiveSize(va_data_JU77_MA_P8p_SSSS_mod/2),effectiveSize(va_data_JU77_Vm_P8p_SSSS_mod/2))
        colnames(effectiveSize_va_data_JU77_P8p_SSSS) <- c("effectiveSize")
        va_data_JU77_P8p_SSSS <- cbind.data.frame(mean_va_data_JU77_P8p_SSSS,median_va_data_JU77_P8p_SSSS,posterior.mode_va_data_JU77_P8p_SSSS,HPDinterval_0.95_va_data_JU77_P8p_SSSS,HPDinterval_0.83_va_data_JU77_P8p_SSSS,effectiveSize_va_data_JU77_P8p_SSSS)
        rownames(va_data_JU77_P8p_SSSS) <- c("va_data_JU77_CONTROL_P8p_SSSS_mod","va_data_JU77_MA_P8p_SSSS_mod","va_data_JU77_Vm_P8p_SSSS_mod")
        va_data_JU77_P8p_SSSS <- cbind(Models = rownames(va_data_JU77_P8p_SSSS),va_data_JU77_P8p_SSSS)
        rownames(va_data_JU77_P8p_SSSS) <- NULL
        va_data_JU77_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        va_data_JU77_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        va_data_JU77_P8p_SSSS$Measure <- c("Va","Va","Va")
        va_data_JU77_P8p_SSSS$Scale <- c("data","data","data")
        va_data_JU77_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        va_data_JU77_P8p_SSSS
      }
      
      #Summary h2_data_JU77_P8p_SSSS
      {
        mean_h2_data_JU77_P8p_SSSS <- rbind(mean(h2_data_JU77_CONTROL_P8p_SSSS_mod),mean(h2_data_JU77_MA_P8p_SSSS_mod),mean(h2_data_JU77_Vm_P8p_SSSS_mod))
        colnames(mean_h2_data_JU77_P8p_SSSS) <- c("mean")
        median_h2_data_JU77_P8p_SSSS <- rbind(median(h2_data_JU77_CONTROL_P8p_SSSS_mod),median(h2_data_JU77_MA_P8p_SSSS_mod),median(h2_data_JU77_Vm_P8p_SSSS_mod))
        colnames(median_h2_data_JU77_P8p_SSSS) <- c("median")
        posterior.mode_h2_data_JU77_P8p_SSSS <- rbind(posterior.mode(as.mcmc(h2_data_JU77_CONTROL_P8p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_JU77_MA_P8p_SSSS_mod)),posterior.mode(as.mcmc(h2_data_JU77_Vm_P8p_SSSS_mod)))
        colnames(posterior.mode_h2_data_JU77_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_JU77_P8p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_JU77_CONTROL_P8p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_JU77_MA_P8p_SSSS_mod)),HPDinterval(as.mcmc(h2_data_JU77_Vm_P8p_SSSS_mod)))
        colnames(HPDinterval_0.95_h2_data_JU77_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_JU77_P8p_SSSS <- rbind(HPDinterval(as.mcmc(h2_data_JU77_CONTROL_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU77_MA_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU77_Vm_P8p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_JU77_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_JU77_P8p_SSSS <- rbind(effectiveSize(h2_data_JU77_CONTROL_P8p_SSSS_mod),effectiveSize(h2_data_JU77_MA_P8p_SSSS_mod),effectiveSize(h2_data_JU77_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_h2_data_JU77_P8p_SSSS) <- c("effectiveSize")
        h2_data_JU77_P8p_SSSS <- cbind.data.frame(mean_h2_data_JU77_P8p_SSSS,median_h2_data_JU77_P8p_SSSS,posterior.mode_h2_data_JU77_P8p_SSSS,HPDinterval_0.95_h2_data_JU77_P8p_SSSS,HPDinterval_0.83_h2_data_JU77_P8p_SSSS,effectiveSize_h2_data_JU77_P8p_SSSS)
        rownames(h2_data_JU77_P8p_SSSS) <- c("h2_data_JU77_CONTROL_P8p_SSSS_mod","h2_data_JU77_MA_P8p_SSSS_mod","h2_data_JU77_Vm_P8p_SSSS_mod")
        h2_data_JU77_P8p_SSSS <- cbind(Models = rownames(h2_data_JU77_P8p_SSSS),h2_data_JU77_P8p_SSSS)
        rownames(h2_data_JU77_P8p_SSSS) <- NULL
        h2_data_JU77_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        h2_data_JU77_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        h2_data_JU77_P8p_SSSS$Measure <- c("H2","H2","H2")
        h2_data_JU77_P8p_SSSS$Scale <- c("data","data","data")
        h2_data_JU77_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        h2_data_JU77_P8p_SSSS
      }
      
      #Summary Evol_data_JU77_P8p_SSSS
      {
        mean_Evol_data_JU77_P8p_SSSS <- rbind(mean(Evol_data_JU77_CONTROL_P8p_SSSS_mod),mean(Evol_data_JU77_MA_P8p_SSSS_mod),mean(Evol_data_JU77_Vm_P8p_SSSS_mod))
        colnames(mean_Evol_data_JU77_P8p_SSSS) <- c("mean")
        median_Evol_data_JU77_P8p_SSSS <- rbind(median(Evol_data_JU77_CONTROL_P8p_SSSS_mod),median(Evol_data_JU77_MA_P8p_SSSS_mod),median(Evol_data_JU77_Vm_P8p_SSSS_mod))
        colnames(median_Evol_data_JU77_P8p_SSSS) <- c("median")
        posterior.mode_Evol_data_JU77_P8p_SSSS <- rbind(posterior.mode(as.mcmc(Evol_data_JU77_CONTROL_P8p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_JU77_MA_P8p_SSSS_mod)),posterior.mode(as.mcmc(Evol_data_JU77_Vm_P8p_SSSS_mod)))
        colnames(posterior.mode_Evol_data_JU77_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_JU77_P8p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_JU77_CONTROL_P8p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_JU77_MA_P8p_SSSS_mod)),HPDinterval(as.mcmc(Evol_data_JU77_Vm_P8p_SSSS_mod)))
        colnames(HPDinterval_0.95_Evol_data_JU77_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_JU77_P8p_SSSS <- rbind(HPDinterval(as.mcmc(Evol_data_JU77_CONTROL_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU77_MA_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU77_Vm_P8p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_JU77_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_JU77_P8p_SSSS <- rbind(effectiveSize(Evol_data_JU77_CONTROL_P8p_SSSS_mod),effectiveSize(Evol_data_JU77_MA_P8p_SSSS_mod),effectiveSize(Evol_data_JU77_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_Evol_data_JU77_P8p_SSSS) <- c("effectiveSize")
        Evol_data_JU77_P8p_SSSS <- cbind.data.frame(mean_Evol_data_JU77_P8p_SSSS,median_Evol_data_JU77_P8p_SSSS,posterior.mode_Evol_data_JU77_P8p_SSSS,HPDinterval_0.95_Evol_data_JU77_P8p_SSSS,HPDinterval_0.83_Evol_data_JU77_P8p_SSSS,effectiveSize_Evol_data_JU77_P8p_SSSS)
        rownames(Evol_data_JU77_P8p_SSSS) <- c("Evol_data_JU77_CONTROL_P8p_SSSS_mod","Evol_data_JU77_MA_P8p_SSSS_mod","Evol_data_JU77_Vm_P8p_SSSS_mod")
        Evol_data_JU77_P8p_SSSS <- cbind(Models = rownames(Evol_data_JU77_P8p_SSSS),Evol_data_JU77_P8p_SSSS)
        rownames(Evol_data_JU77_P8p_SSSS) <- NULL
        Evol_data_JU77_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        Evol_data_JU77_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        Evol_data_JU77_P8p_SSSS$Measure <- c("Evol","Evol","Evol")
        Evol_data_JU77_P8p_SSSS$Scale <- c("data","data","data")
        Evol_data_JU77_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        Evol_data_JU77_P8p_SSSS
      }
      
      #Summary trait_mean_data_JU77_P8p_SSSS
      {
        mean_trait_mean_data_JU77_P8p_SSSS <- rbind(mean(trait_mean_data_JU77_CONTROL_P8p_SSSS_mod),mean(trait_mean_data_JU77_MA_P8p_SSSS_mod),mean(trait_mean_data_JU77_Vm_P8p_SSSS_mod))
        colnames(mean_trait_mean_data_JU77_P8p_SSSS) <- c("mean")
        median_trait_mean_data_JU77_P8p_SSSS <- rbind(median(trait_mean_data_JU77_CONTROL_P8p_SSSS_mod),median(trait_mean_data_JU77_MA_P8p_SSSS_mod),median(trait_mean_data_JU77_Vm_P8p_SSSS_mod))
        colnames(median_trait_mean_data_JU77_P8p_SSSS) <- c("median")
        posterior.mode_trait_mean_data_JU77_P8p_SSSS <- rbind(posterior.mode(as.mcmc(trait_mean_data_JU77_CONTROL_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_JU77_MA_P8p_SSSS_mod)),posterior.mode(as.mcmc(trait_mean_data_JU77_Vm_P8p_SSSS_mod)))
        colnames(posterior.mode_trait_mean_data_JU77_P8p_SSSS) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_JU77_P8p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU77_CONTROL_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_JU77_MA_P8p_SSSS_mod)),HPDinterval(as.mcmc(trait_mean_data_JU77_Vm_P8p_SSSS_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_JU77_P8p_SSSS) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_JU77_P8p_SSSS <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU77_CONTROL_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU77_MA_P8p_SSSS_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU77_Vm_P8p_SSSS_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_JU77_P8p_SSSS) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_JU77_P8p_SSSS <- rbind(effectiveSize(trait_mean_data_JU77_CONTROL_P8p_SSSS_mod),effectiveSize(trait_mean_data_JU77_MA_P8p_SSSS_mod),effectiveSize(trait_mean_data_JU77_Vm_P8p_SSSS_mod))
        colnames(effectiveSize_trait_mean_data_JU77_P8p_SSSS) <- c("effectiveSize")
        trait_mean_data_JU77_P8p_SSSS <- cbind.data.frame(mean_trait_mean_data_JU77_P8p_SSSS,median_trait_mean_data_JU77_P8p_SSSS,posterior.mode_trait_mean_data_JU77_P8p_SSSS,HPDinterval_0.95_trait_mean_data_JU77_P8p_SSSS,HPDinterval_0.83_trait_mean_data_JU77_P8p_SSSS,effectiveSize_trait_mean_data_JU77_P8p_SSSS)
        rownames(trait_mean_data_JU77_P8p_SSSS) <- c("trait_mean_data_JU77_CONTROL_P8p_SSSS_mod","trait_mean_data_JU77_MA_P8p_SSSS_mod","trait_mean_data_JU77_Vm_P8p_SSSS_mod")
        trait_mean_data_JU77_P8p_SSSS <- cbind(Models = rownames(trait_mean_data_JU77_P8p_SSSS),trait_mean_data_JU77_P8p_SSSS)
        rownames(trait_mean_data_JU77_P8p_SSSS) <- NULL
        trait_mean_data_JU77_P8p_SSSS$Pnp <- c("P8.p","P8.p","P8.p")
        trait_mean_data_JU77_P8p_SSSS$Treatment <- c("Control","ML","Vm")
        trait_mean_data_JU77_P8p_SSSS$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_JU77_P8p_SSSS$Scale <- c("data","data","data")
        trait_mean_data_JU77_P8p_SSSS$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_JU77_P8p_SSSS
      }
      
      data_JU77_P8p_SSSS <- rbind.data.frame(va_data_JU77_P8p_SSSS, h2_data_JU77_P8p_SSSS,Evol_data_JU77_P8p_SSSS,trait_mean_data_JU77_P8p_SSSS)
      data_JU77_P8p_SSSS
      
    }
    Vm_JU77_P8p_SSSS <- rbind.data.frame(liab_JU77_P8p_SSSS, data_JU77_P8p_SSSS)
    Vm_JU77_P8p_SSSS$Pnp_fate <- rep("SSSS", 24)
    Vm_JU77_P8p_SSSS
    #remove JU77 P8p_SSSSS models
    {
      remove(JU77_CONTROL_P8p_SSSS_mod)
      remove(JU77_MA_P8p_SSSS_mod)
      remove(JU77_Vm_P8p_SSSS_mod)
    }
  }
  
  ##Summary JU77 P5p----
  {
    #Summary liability scale JU77 P5p
    {
      #Summary va_liab_JU77_P5p_wt: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_JU77_P5p_wt <- rbind(mean(va_liab_JU77_CONTROL_P5p_wt_mod/2),mean(va_liab_JU77_MA_P5p_wt_mod/2),mean(va_liab_JU77_Vm_P5p_wt_mod/2))
        colnames(mean_va_liab_JU77_P5p_wt) <- c("mean")
        median_va_liab_JU77_P5p_wt <- rbind(median(va_liab_JU77_CONTROL_P5p_wt_mod/2),median(va_liab_JU77_MA_P5p_wt_mod/2),median(va_liab_JU77_Vm_P5p_wt_mod/2))
        colnames(median_va_liab_JU77_P5p_wt) <- c("median")
        posterior.mode_va_liab_JU77_P5p_wt <- rbind(posterior.mode(va_liab_JU77_CONTROL_P5p_wt_mod/2),posterior.mode(va_liab_JU77_MA_P5p_wt_mod/2),posterior.mode(va_liab_JU77_Vm_P5p_wt_mod/2))
        colnames(posterior.mode_va_liab_JU77_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_JU77_P5p_wt <- rbind(HPDinterval(va_liab_JU77_CONTROL_P5p_wt_mod/2),HPDinterval(va_liab_JU77_MA_P5p_wt_mod/2),HPDinterval(va_liab_JU77_Vm_P5p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_JU77_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_JU77_P5p_wt <- rbind(HPDinterval(va_liab_JU77_CONTROL_P5p_wt_mod/2,prob=.83),HPDinterval(va_liab_JU77_MA_P5p_wt_mod/2,prob=.83),HPDinterval(va_liab_JU77_Vm_P5p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_JU77_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_JU77_P5p_wt <- rbind(effectiveSize(va_liab_JU77_CONTROL_P5p_wt_mod/2),effectiveSize(va_liab_JU77_MA_P5p_wt_mod/2),effectiveSize(va_liab_JU77_Vm_P5p_wt_mod/2))
        colnames(effectiveSize_va_liab_JU77_P5p_wt) <- c("effectiveSize")
        va_liab_JU77_P5p_wt <- cbind.data.frame(mean_va_liab_JU77_P5p_wt,median_va_liab_JU77_P5p_wt,posterior.mode_va_liab_JU77_P5p_wt,HPDinterval_0.95_va_liab_JU77_P5p_wt,HPDinterval_0.83_va_liab_JU77_P5p_wt,effectiveSize_va_liab_JU77_P5p_wt)
        rownames(va_liab_JU77_P5p_wt) <- c("va_liab_JU77_CONTROL_P5p_wt_mod","va_liab_JU77_MA_P5p_wt_mod","va_liab_JU77_Vm_P5p_wt_mod")
        va_liab_JU77_P5p_wt <- cbind(Models = rownames(va_liab_JU77_P5p_wt),va_liab_JU77_P5p_wt)
        rownames(va_liab_JU77_P5p_wt) <- NULL
        va_liab_JU77_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        va_liab_JU77_P5p_wt$Treatment <- c("Control","ML","Vm")
        va_liab_JU77_P5p_wt$Measure <- c("Va","Va","Va")
        va_liab_JU77_P5p_wt$Scale <- c("liab","liab","liab")
        va_liab_JU77_P5p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_JU77_P5p_wt
      }
      
      #Summary h2_liab_JU77_P5p_wt
      {
        mean_h2_liab_JU77_P5p_wt <- rbind(mean(h2_liab_JU77_CONTROL_P5p_wt_mod),mean(h2_liab_JU77_MA_P5p_wt_mod),mean(h2_liab_JU77_Vm_P5p_wt_mod))
        colnames(mean_h2_liab_JU77_P5p_wt) <- c("mean")
        median_h2_liab_JU77_P5p_wt <- rbind(median(h2_liab_JU77_CONTROL_P5p_wt_mod),median(h2_liab_JU77_MA_P5p_wt_mod),median(h2_liab_JU77_Vm_P5p_wt_mod))
        colnames(median_h2_liab_JU77_P5p_wt) <- c("median")
        posterior.mode_h2_liab_JU77_P5p_wt <- rbind(posterior.mode(h2_liab_JU77_CONTROL_P5p_wt_mod),posterior.mode(h2_liab_JU77_MA_P5p_wt_mod),posterior.mode(h2_liab_JU77_Vm_P5p_wt_mod))
        colnames(posterior.mode_h2_liab_JU77_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_JU77_P5p_wt <- rbind(HPDinterval(h2_liab_JU77_CONTROL_P5p_wt_mod),HPDinterval(h2_liab_JU77_MA_P5p_wt_mod),HPDinterval(h2_liab_JU77_Vm_P5p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_JU77_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_JU77_P5p_wt <- rbind(HPDinterval(h2_liab_JU77_CONTROL_P5p_wt_mod,prob=.83),HPDinterval(h2_liab_JU77_MA_P5p_wt_mod,prob=.83),HPDinterval(h2_liab_JU77_Vm_P5p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_JU77_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_JU77_P5p_wt <- rbind(effectiveSize(h2_liab_JU77_CONTROL_P5p_wt_mod),effectiveSize(h2_liab_JU77_MA_P5p_wt_mod),effectiveSize(h2_liab_JU77_Vm_P5p_wt_mod))
        colnames(effectiveSize_h2_liab_JU77_P5p_wt) <- c("effectiveSize")
        h2_liab_JU77_P5p_wt <- cbind.data.frame(mean_h2_liab_JU77_P5p_wt,median_h2_liab_JU77_P5p_wt,posterior.mode_h2_liab_JU77_P5p_wt,HPDinterval_0.95_h2_liab_JU77_P5p_wt,HPDinterval_0.83_h2_liab_JU77_P5p_wt,effectiveSize_h2_liab_JU77_P5p_wt)
        rownames(h2_liab_JU77_P5p_wt) <- c("h2_liab_JU77_CONTROL_P5p_wt_mod","h2_liab_JU77_MA_P5p_wt_mod","h2_liab_JU77_Vm_P5p_wt_mod")
        h2_liab_JU77_P5p_wt <- cbind(Models = rownames(h2_liab_JU77_P5p_wt),h2_liab_JU77_P5p_wt)
        rownames(h2_liab_JU77_P5p_wt) <- NULL
        h2_liab_JU77_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        h2_liab_JU77_P5p_wt$Treatment <- c("Control","ML","Vm")
        h2_liab_JU77_P5p_wt$Measure <- c("H2","H2","H2")
        h2_liab_JU77_P5p_wt$Scale <- c("liab","liab","liab")
        h2_liab_JU77_P5p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_JU77_P5p_wt
      }
      
      #Summary Evol_liab_JU77_P5p_wt
      {
        mean_Evol_liab_JU77_P5p_wt <- rbind(mean(Evol_liab_JU77_CONTROL_P5p_wt_mod),mean(Evol_liab_JU77_MA_P5p_wt_mod),mean(Evol_liab_JU77_Vm_P5p_wt_mod))
        colnames(mean_Evol_liab_JU77_P5p_wt) <- c("mean")
        median_Evol_liab_JU77_P5p_wt <- rbind(median(Evol_liab_JU77_CONTROL_P5p_wt_mod),median(Evol_liab_JU77_MA_P5p_wt_mod),median(Evol_liab_JU77_Vm_P5p_wt_mod))
        colnames(median_Evol_liab_JU77_P5p_wt) <- c("median")
        posterior.mode_Evol_liab_JU77_P5p_wt <- rbind(posterior.mode(Evol_liab_JU77_CONTROL_P5p_wt_mod),posterior.mode(Evol_liab_JU77_MA_P5p_wt_mod),posterior.mode(Evol_liab_JU77_Vm_P5p_wt_mod))
        colnames(posterior.mode_Evol_liab_JU77_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_JU77_P5p_wt <- rbind(HPDinterval(Evol_liab_JU77_CONTROL_P5p_wt_mod),HPDinterval(Evol_liab_JU77_MA_P5p_wt_mod),HPDinterval(Evol_liab_JU77_Vm_P5p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_JU77_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_JU77_P5p_wt <- rbind(HPDinterval(Evol_liab_JU77_CONTROL_P5p_wt_mod,prob=.83),HPDinterval(Evol_liab_JU77_MA_P5p_wt_mod,prob=.83),HPDinterval(Evol_liab_JU77_Vm_P5p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_JU77_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_JU77_P5p_wt <- rbind(effectiveSize(Evol_liab_JU77_CONTROL_P5p_wt_mod),effectiveSize(Evol_liab_JU77_MA_P5p_wt_mod),effectiveSize(Evol_liab_JU77_Vm_P5p_wt_mod))
        colnames(effectiveSize_Evol_liab_JU77_P5p_wt) <- c("effectiveSize")
        Evol_liab_JU77_P5p_wt <- cbind.data.frame(mean_Evol_liab_JU77_P5p_wt,median_Evol_liab_JU77_P5p_wt,posterior.mode_Evol_liab_JU77_P5p_wt,HPDinterval_0.95_Evol_liab_JU77_P5p_wt,HPDinterval_0.83_Evol_liab_JU77_P5p_wt,effectiveSize_Evol_liab_JU77_P5p_wt)
        rownames(Evol_liab_JU77_P5p_wt) <- c("Evol_liab_JU77_CONTROL_P5p_wt_mod","Evol_liab_JU77_MA_P5p_wt_mod","Evol_liab_JU77_Vm_P5p_wt_mod")
        Evol_liab_JU77_P5p_wt <- cbind(Models = rownames(Evol_liab_JU77_P5p_wt),Evol_liab_JU77_P5p_wt)
        rownames(Evol_liab_JU77_P5p_wt) <- NULL
        Evol_liab_JU77_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        Evol_liab_JU77_P5p_wt$Treatment <- c("Control","ML","Vm")
        Evol_liab_JU77_P5p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_JU77_P5p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_JU77_P5p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_JU77_P5p_wt
      }
      
      #Summary trait_mean_liab_JU77_P5p_wt
      {
        mean_trait_mean_liab_JU77_P5p_wt <- rbind(mean(trait_mean_liab_JU77_CONTROL_P5p_wt_mod),mean(trait_mean_liab_JU77_MA_P5p_wt_mod),mean(trait_mean_liab_JU77_Vm_P5p_wt_mod))
        colnames(mean_trait_mean_liab_JU77_P5p_wt) <- c("mean")
        median_trait_mean_liab_JU77_P5p_wt <- rbind(median(trait_mean_liab_JU77_CONTROL_P5p_wt_mod),median(trait_mean_liab_JU77_MA_P5p_wt_mod),median(trait_mean_liab_JU77_Vm_P5p_wt_mod))
        colnames(median_trait_mean_liab_JU77_P5p_wt) <- c("median")
        posterior.mode_trait_mean_liab_JU77_P5p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_JU77_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU77_MA_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU77_Vm_P5p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_JU77_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_JU77_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU77_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU77_MA_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU77_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_JU77_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_JU77_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU77_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU77_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU77_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_JU77_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_JU77_P5p_wt <- rbind(effectiveSize(trait_mean_liab_JU77_CONTROL_P5p_wt_mod),effectiveSize(trait_mean_liab_JU77_MA_P5p_wt_mod),effectiveSize(trait_mean_liab_JU77_Vm_P5p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_JU77_P5p_wt) <- c("effectiveSize")
        trait_mean_liab_JU77_P5p_wt <- cbind.data.frame(mean_trait_mean_liab_JU77_P5p_wt,median_trait_mean_liab_JU77_P5p_wt,posterior.mode_trait_mean_liab_JU77_P5p_wt,HPDinterval_0.95_trait_mean_liab_JU77_P5p_wt,HPDinterval_0.83_trait_mean_liab_JU77_P5p_wt,effectiveSize_trait_mean_liab_JU77_P5p_wt)
        rownames(trait_mean_liab_JU77_P5p_wt) <- c("trait_mean_liab_JU77_CONTROL_P5p_wt_mod","trait_mean_liab_JU77_MA_P5p_wt_mod","trait_mean_liab_JU77_Vm_P5p_wt_mod")
        trait_mean_liab_JU77_P5p_wt <- cbind(Models = rownames(trait_mean_liab_JU77_P5p_wt),trait_mean_liab_JU77_P5p_wt)
        rownames(trait_mean_liab_JU77_P5p_wt) <- NULL
        trait_mean_liab_JU77_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        trait_mean_liab_JU77_P5p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_JU77_P5p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_JU77_P5p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_JU77_P5p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_JU77_P5p_wt
      }
      
      liab_JU77_P5p_wt <- rbind.data.frame(va_liab_JU77_P5p_wt, h2_liab_JU77_P5p_wt,Evol_liab_JU77_P5p_wt,trait_mean_liab_JU77_P5p_wt)
      liab_JU77_P5p_wt
    }
    #Summary data scale JU77 P5p
    {
      #Summary va_data_JU77_P5p_wt:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_JU77_P5p_wt <- rbind(mean(va_data_JU77_CONTROL_P5p_wt_mod/2),mean(va_data_JU77_MA_P5p_wt_mod/2),mean(va_data_JU77_Vm_P5p_wt_mod/2))
        colnames(mean_va_data_JU77_P5p_wt) <- c("mean")
        median_va_data_JU77_P5p_wt <- rbind(median(va_data_JU77_CONTROL_P5p_wt_mod/2),median(va_data_JU77_MA_P5p_wt_mod/2),median(va_data_JU77_Vm_P5p_wt_mod/2))
        colnames(median_va_data_JU77_P5p_wt) <- c("median")
        posterior.mode_va_data_JU77_P5p_wt <- rbind(posterior.mode(as.mcmc(va_data_JU77_CONTROL_P5p_wt_mod/2)),posterior.mode(as.mcmc(va_data_JU77_MA_P5p_wt_mod/2)),posterior.mode(as.mcmc(va_data_JU77_Vm_P5p_wt_mod/2)))
        colnames(posterior.mode_va_data_JU77_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_JU77_P5p_wt <- rbind(HPDinterval(as.mcmc(va_data_JU77_CONTROL_P5p_wt_mod/2)),HPDinterval(as.mcmc(va_data_JU77_MA_P5p_wt_mod/2)),HPDinterval(as.mcmc(va_data_JU77_Vm_P5p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_JU77_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_JU77_P5p_wt <- rbind(HPDinterval(as.mcmc(va_data_JU77_CONTROL_P5p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU77_MA_P5p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU77_Vm_P5p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_JU77_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_JU77_P5p_wt <- rbind(effectiveSize(va_data_JU77_CONTROL_P5p_wt_mod/2),effectiveSize(va_data_JU77_MA_P5p_wt_mod/2),effectiveSize(va_data_JU77_Vm_P5p_wt_mod/2))
        colnames(effectiveSize_va_data_JU77_P5p_wt) <- c("effectiveSize")
        va_data_JU77_P5p_wt <- cbind.data.frame(mean_va_data_JU77_P5p_wt,median_va_data_JU77_P5p_wt,posterior.mode_va_data_JU77_P5p_wt,HPDinterval_0.95_va_data_JU77_P5p_wt,HPDinterval_0.83_va_data_JU77_P5p_wt,effectiveSize_va_data_JU77_P5p_wt)
        rownames(va_data_JU77_P5p_wt) <- c("va_data_JU77_CONTROL_P5p_wt_mod","va_data_JU77_MA_P5p_wt_mod","va_data_JU77_Vm_P5p_wt_mod")
        va_data_JU77_P5p_wt <- cbind(Models = rownames(va_data_JU77_P5p_wt),va_data_JU77_P5p_wt)
        rownames(va_data_JU77_P5p_wt) <- NULL
        va_data_JU77_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        va_data_JU77_P5p_wt$Treatment <- c("Control","ML","Vm")
        va_data_JU77_P5p_wt$Measure <- c("Va","Va","Va")
        va_data_JU77_P5p_wt$Scale <- c("data","data","data")
        va_data_JU77_P5p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_JU77_P5p_wt
      }
      
      #Summary h2_data_JU77_P5p_wt
      {
        mean_h2_data_JU77_P5p_wt <- rbind(mean(h2_data_JU77_CONTROL_P5p_wt_mod),mean(h2_data_JU77_MA_P5p_wt_mod),mean(h2_data_JU77_Vm_P5p_wt_mod))
        colnames(mean_h2_data_JU77_P5p_wt) <- c("mean")
        median_h2_data_JU77_P5p_wt <- rbind(median(h2_data_JU77_CONTROL_P5p_wt_mod),median(h2_data_JU77_MA_P5p_wt_mod),median(h2_data_JU77_Vm_P5p_wt_mod))
        colnames(median_h2_data_JU77_P5p_wt) <- c("median")
        posterior.mode_h2_data_JU77_P5p_wt <- rbind(posterior.mode(as.mcmc(h2_data_JU77_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(h2_data_JU77_MA_P5p_wt_mod)),posterior.mode(as.mcmc(h2_data_JU77_Vm_P5p_wt_mod)))
        colnames(posterior.mode_h2_data_JU77_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_JU77_P5p_wt <- rbind(HPDinterval(as.mcmc(h2_data_JU77_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(h2_data_JU77_MA_P5p_wt_mod)),HPDinterval(as.mcmc(h2_data_JU77_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_JU77_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_JU77_P5p_wt <- rbind(HPDinterval(as.mcmc(h2_data_JU77_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU77_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU77_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_JU77_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_JU77_P5p_wt <- rbind(effectiveSize(h2_data_JU77_CONTROL_P5p_wt_mod),effectiveSize(h2_data_JU77_MA_P5p_wt_mod),effectiveSize(h2_data_JU77_Vm_P5p_wt_mod))
        colnames(effectiveSize_h2_data_JU77_P5p_wt) <- c("effectiveSize")
        h2_data_JU77_P5p_wt <- cbind.data.frame(mean_h2_data_JU77_P5p_wt,median_h2_data_JU77_P5p_wt,posterior.mode_h2_data_JU77_P5p_wt,HPDinterval_0.95_h2_data_JU77_P5p_wt,HPDinterval_0.83_h2_data_JU77_P5p_wt,effectiveSize_h2_data_JU77_P5p_wt)
        rownames(h2_data_JU77_P5p_wt) <- c("h2_data_JU77_CONTROL_P5p_wt_mod","h2_data_JU77_MA_P5p_wt_mod","h2_data_JU77_Vm_P5p_wt_mod")
        h2_data_JU77_P5p_wt <- cbind(Models = rownames(h2_data_JU77_P5p_wt),h2_data_JU77_P5p_wt)
        rownames(h2_data_JU77_P5p_wt) <- NULL
        h2_data_JU77_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        h2_data_JU77_P5p_wt$Treatment <- c("Control","ML","Vm")
        h2_data_JU77_P5p_wt$Measure <- c("H2","H2","H2")
        h2_data_JU77_P5p_wt$Scale <- c("data","data","data")
        h2_data_JU77_P5p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_JU77_P5p_wt
      }
      
      #Summary Evol_data_JU77_P5p_wt
      {
        mean_Evol_data_JU77_P5p_wt <- rbind(mean(Evol_data_JU77_CONTROL_P5p_wt_mod),mean(Evol_data_JU77_MA_P5p_wt_mod),mean(Evol_data_JU77_Vm_P5p_wt_mod))
        colnames(mean_Evol_data_JU77_P5p_wt) <- c("mean")
        median_Evol_data_JU77_P5p_wt <- rbind(median(Evol_data_JU77_CONTROL_P5p_wt_mod),median(Evol_data_JU77_MA_P5p_wt_mod),median(Evol_data_JU77_Vm_P5p_wt_mod))
        colnames(median_Evol_data_JU77_P5p_wt) <- c("median")
        posterior.mode_Evol_data_JU77_P5p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_JU77_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(Evol_data_JU77_MA_P5p_wt_mod)),posterior.mode(as.mcmc(Evol_data_JU77_Vm_P5p_wt_mod)))
        colnames(posterior.mode_Evol_data_JU77_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_JU77_P5p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_JU77_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(Evol_data_JU77_MA_P5p_wt_mod)),HPDinterval(as.mcmc(Evol_data_JU77_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_JU77_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_JU77_P5p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_JU77_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU77_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU77_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_JU77_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_JU77_P5p_wt <- rbind(effectiveSize(Evol_data_JU77_CONTROL_P5p_wt_mod),effectiveSize(Evol_data_JU77_MA_P5p_wt_mod),effectiveSize(Evol_data_JU77_Vm_P5p_wt_mod))
        colnames(effectiveSize_Evol_data_JU77_P5p_wt) <- c("effectiveSize")
        Evol_data_JU77_P5p_wt <- cbind.data.frame(mean_Evol_data_JU77_P5p_wt,median_Evol_data_JU77_P5p_wt,posterior.mode_Evol_data_JU77_P5p_wt,HPDinterval_0.95_Evol_data_JU77_P5p_wt,HPDinterval_0.83_Evol_data_JU77_P5p_wt,effectiveSize_Evol_data_JU77_P5p_wt)
        rownames(Evol_data_JU77_P5p_wt) <- c("Evol_data_JU77_CONTROL_P5p_wt_mod","Evol_data_JU77_MA_P5p_wt_mod","Evol_data_JU77_Vm_P5p_wt_mod")
        Evol_data_JU77_P5p_wt <- cbind(Models = rownames(Evol_data_JU77_P5p_wt),Evol_data_JU77_P5p_wt)
        rownames(Evol_data_JU77_P5p_wt) <- NULL
        Evol_data_JU77_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        Evol_data_JU77_P5p_wt$Treatment <- c("Control","ML","Vm")
        Evol_data_JU77_P5p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_JU77_P5p_wt$Scale <- c("data","data","data")
        Evol_data_JU77_P5p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_JU77_P5p_wt
      }
      
      #Summary trait_mean_data_JU77_P5p_wt
      {
        mean_trait_mean_data_JU77_P5p_wt <- rbind(mean(trait_mean_data_JU77_CONTROL_P5p_wt_mod),mean(trait_mean_data_JU77_MA_P5p_wt_mod),mean(trait_mean_data_JU77_Vm_P5p_wt_mod))
        colnames(mean_trait_mean_data_JU77_P5p_wt) <- c("mean")
        median_trait_mean_data_JU77_P5p_wt <- rbind(median(trait_mean_data_JU77_CONTROL_P5p_wt_mod),median(trait_mean_data_JU77_MA_P5p_wt_mod),median(trait_mean_data_JU77_Vm_P5p_wt_mod))
        colnames(median_trait_mean_data_JU77_P5p_wt) <- c("median")
        posterior.mode_trait_mean_data_JU77_P5p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_JU77_CONTROL_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_JU77_MA_P5p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_JU77_Vm_P5p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_JU77_P5p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_JU77_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU77_CONTROL_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_JU77_MA_P5p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_JU77_Vm_P5p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_JU77_P5p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_JU77_P5p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU77_CONTROL_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU77_MA_P5p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU77_Vm_P5p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_JU77_P5p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_JU77_P5p_wt <- rbind(effectiveSize(trait_mean_data_JU77_CONTROL_P5p_wt_mod),effectiveSize(trait_mean_data_JU77_MA_P5p_wt_mod),effectiveSize(trait_mean_data_JU77_Vm_P5p_wt_mod))
        colnames(effectiveSize_trait_mean_data_JU77_P5p_wt) <- c("effectiveSize")
        trait_mean_data_JU77_P5p_wt <- cbind.data.frame(mean_trait_mean_data_JU77_P5p_wt,median_trait_mean_data_JU77_P5p_wt,posterior.mode_trait_mean_data_JU77_P5p_wt,HPDinterval_0.95_trait_mean_data_JU77_P5p_wt,HPDinterval_0.83_trait_mean_data_JU77_P5p_wt,effectiveSize_trait_mean_data_JU77_P5p_wt)
        rownames(trait_mean_data_JU77_P5p_wt) <- c("trait_mean_data_JU77_CONTROL_P5p_wt_mod","trait_mean_data_JU77_MA_P5p_wt_mod","trait_mean_data_JU77_Vm_P5p_wt_mod")
        trait_mean_data_JU77_P5p_wt <- cbind(Models = rownames(trait_mean_data_JU77_P5p_wt),trait_mean_data_JU77_P5p_wt)
        rownames(trait_mean_data_JU77_P5p_wt) <- NULL
        trait_mean_data_JU77_P5p_wt$Pnp <- c("P5.p","P5.p","P5.p")
        trait_mean_data_JU77_P5p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_data_JU77_P5p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_JU77_P5p_wt$Scale <- c("data","data","data")
        trait_mean_data_JU77_P5p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_JU77_P5p_wt
      }
      
      data_JU77_P5p_wt <- rbind.data.frame(va_data_JU77_P5p_wt, h2_data_JU77_P5p_wt,Evol_data_JU77_P5p_wt,trait_mean_data_JU77_P5p_wt)
      data_JU77_P5p_wt
      
    }
    Vm_JU77_P5p_wt <- rbind.data.frame(liab_JU77_P5p_wt, data_JU77_P5p_wt)
    Vm_JU77_P5p_wt$Pnp_fate <- rep("wt", 24)
    Vm_JU77_P5p_wt
    #remove JU77 P5p_wt models
    {
      remove(JU77_CONTROL_P5p_wt_mod)
      remove(JU77_MA_P5p_wt_mod)
      remove(JU77_Vm_P5p_wt_mod)
    }
  }
  
  ##Summary JU77 P6p----
  {
    #Summary liability scale JU77 P6p
    {
      #Summary va_liab_JU77_P6p_wt: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_JU77_P6p_wt <- rbind(mean(va_liab_JU77_CONTROL_P6p_wt_mod/2),mean(va_liab_JU77_MA_P6p_wt_mod/2),mean(va_liab_JU77_Vm_P6p_wt_mod/2))
        colnames(mean_va_liab_JU77_P6p_wt) <- c("mean")
        median_va_liab_JU77_P6p_wt <- rbind(median(va_liab_JU77_CONTROL_P6p_wt_mod/2),median(va_liab_JU77_MA_P6p_wt_mod/2),median(va_liab_JU77_Vm_P6p_wt_mod/2))
        colnames(median_va_liab_JU77_P6p_wt) <- c("median")
        posterior.mode_va_liab_JU77_P6p_wt <- rbind(posterior.mode(va_liab_JU77_CONTROL_P6p_wt_mod/2),posterior.mode(va_liab_JU77_MA_P6p_wt_mod/2),posterior.mode(va_liab_JU77_Vm_P6p_wt_mod/2))
        colnames(posterior.mode_va_liab_JU77_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_JU77_P6p_wt <- rbind(HPDinterval(va_liab_JU77_CONTROL_P6p_wt_mod/2),HPDinterval(va_liab_JU77_MA_P6p_wt_mod/2),HPDinterval(va_liab_JU77_Vm_P6p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_JU77_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_JU77_P6p_wt <- rbind(HPDinterval(va_liab_JU77_CONTROL_P6p_wt_mod/2,prob=.83),HPDinterval(va_liab_JU77_MA_P6p_wt_mod/2,prob=.83),HPDinterval(va_liab_JU77_Vm_P6p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_JU77_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_JU77_P6p_wt <- rbind(effectiveSize(va_liab_JU77_CONTROL_P6p_wt_mod/2),effectiveSize(va_liab_JU77_MA_P6p_wt_mod/2),effectiveSize(va_liab_JU77_Vm_P6p_wt_mod/2))
        colnames(effectiveSize_va_liab_JU77_P6p_wt) <- c("effectiveSize")
        va_liab_JU77_P6p_wt <- cbind.data.frame(mean_va_liab_JU77_P6p_wt,median_va_liab_JU77_P6p_wt,posterior.mode_va_liab_JU77_P6p_wt,HPDinterval_0.95_va_liab_JU77_P6p_wt,HPDinterval_0.83_va_liab_JU77_P6p_wt,effectiveSize_va_liab_JU77_P6p_wt)
        rownames(va_liab_JU77_P6p_wt) <- c("va_liab_JU77_CONTROL_P6p_wt_mod","va_liab_JU77_MA_P6p_wt_mod","va_liab_JU77_Vm_P6p_wt_mod")
        va_liab_JU77_P6p_wt <- cbind(Models = rownames(va_liab_JU77_P6p_wt),va_liab_JU77_P6p_wt)
        rownames(va_liab_JU77_P6p_wt) <- NULL
        va_liab_JU77_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        va_liab_JU77_P6p_wt$Treatment <- c("Control","ML","Vm")
        va_liab_JU77_P6p_wt$Measure <- c("Va","Va","Va")
        va_liab_JU77_P6p_wt$Scale <- c("liab","liab","liab")
        va_liab_JU77_P6p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_JU77_P6p_wt
      }
      
      #Summary h2_liab_JU77_P6p_wt
      {
        mean_h2_liab_JU77_P6p_wt <- rbind(mean(h2_liab_JU77_CONTROL_P6p_wt_mod),mean(h2_liab_JU77_MA_P6p_wt_mod),mean(h2_liab_JU77_Vm_P6p_wt_mod))
        colnames(mean_h2_liab_JU77_P6p_wt) <- c("mean")
        median_h2_liab_JU77_P6p_wt <- rbind(median(h2_liab_JU77_CONTROL_P6p_wt_mod),median(h2_liab_JU77_MA_P6p_wt_mod),median(h2_liab_JU77_Vm_P6p_wt_mod))
        colnames(median_h2_liab_JU77_P6p_wt) <- c("median")
        posterior.mode_h2_liab_JU77_P6p_wt <- rbind(posterior.mode(h2_liab_JU77_CONTROL_P6p_wt_mod),posterior.mode(h2_liab_JU77_MA_P6p_wt_mod),posterior.mode(h2_liab_JU77_Vm_P6p_wt_mod))
        colnames(posterior.mode_h2_liab_JU77_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_JU77_P6p_wt <- rbind(HPDinterval(h2_liab_JU77_CONTROL_P6p_wt_mod),HPDinterval(h2_liab_JU77_MA_P6p_wt_mod),HPDinterval(h2_liab_JU77_Vm_P6p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_JU77_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_JU77_P6p_wt <- rbind(HPDinterval(h2_liab_JU77_CONTROL_P6p_wt_mod,prob=.83),HPDinterval(h2_liab_JU77_MA_P6p_wt_mod,prob=.83),HPDinterval(h2_liab_JU77_Vm_P6p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_JU77_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_JU77_P6p_wt <- rbind(effectiveSize(h2_liab_JU77_CONTROL_P6p_wt_mod),effectiveSize(h2_liab_JU77_MA_P6p_wt_mod),effectiveSize(h2_liab_JU77_Vm_P6p_wt_mod))
        colnames(effectiveSize_h2_liab_JU77_P6p_wt) <- c("effectiveSize")
        h2_liab_JU77_P6p_wt <- cbind.data.frame(mean_h2_liab_JU77_P6p_wt,median_h2_liab_JU77_P6p_wt,posterior.mode_h2_liab_JU77_P6p_wt,HPDinterval_0.95_h2_liab_JU77_P6p_wt,HPDinterval_0.83_h2_liab_JU77_P6p_wt,effectiveSize_h2_liab_JU77_P6p_wt)
        rownames(h2_liab_JU77_P6p_wt) <- c("h2_liab_JU77_CONTROL_P6p_wt_mod","h2_liab_JU77_MA_P6p_wt_mod","h2_liab_JU77_Vm_P6p_wt_mod")
        h2_liab_JU77_P6p_wt <- cbind(Models = rownames(h2_liab_JU77_P6p_wt),h2_liab_JU77_P6p_wt)
        rownames(h2_liab_JU77_P6p_wt) <- NULL
        h2_liab_JU77_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        h2_liab_JU77_P6p_wt$Treatment <- c("Control","ML","Vm")
        h2_liab_JU77_P6p_wt$Measure <- c("H2","H2","H2")
        h2_liab_JU77_P6p_wt$Scale <- c("liab","liab","liab")
        h2_liab_JU77_P6p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_JU77_P6p_wt
      }
      
      #Summary Evol_liab_JU77_P6p_wt
      {
        mean_Evol_liab_JU77_P6p_wt <- rbind(mean(Evol_liab_JU77_CONTROL_P6p_wt_mod),mean(Evol_liab_JU77_MA_P6p_wt_mod),mean(Evol_liab_JU77_Vm_P6p_wt_mod))
        colnames(mean_Evol_liab_JU77_P6p_wt) <- c("mean")
        median_Evol_liab_JU77_P6p_wt <- rbind(median(Evol_liab_JU77_CONTROL_P6p_wt_mod),median(Evol_liab_JU77_MA_P6p_wt_mod),median(Evol_liab_JU77_Vm_P6p_wt_mod))
        colnames(median_Evol_liab_JU77_P6p_wt) <- c("median")
        posterior.mode_Evol_liab_JU77_P6p_wt <- rbind(posterior.mode(Evol_liab_JU77_CONTROL_P6p_wt_mod),posterior.mode(Evol_liab_JU77_MA_P6p_wt_mod),posterior.mode(Evol_liab_JU77_Vm_P6p_wt_mod))
        colnames(posterior.mode_Evol_liab_JU77_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_JU77_P6p_wt <- rbind(HPDinterval(Evol_liab_JU77_CONTROL_P6p_wt_mod),HPDinterval(Evol_liab_JU77_MA_P6p_wt_mod),HPDinterval(Evol_liab_JU77_Vm_P6p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_JU77_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_JU77_P6p_wt <- rbind(HPDinterval(Evol_liab_JU77_CONTROL_P6p_wt_mod,prob=.83),HPDinterval(Evol_liab_JU77_MA_P6p_wt_mod,prob=.83),HPDinterval(Evol_liab_JU77_Vm_P6p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_JU77_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_JU77_P6p_wt <- rbind(effectiveSize(Evol_liab_JU77_CONTROL_P6p_wt_mod),effectiveSize(Evol_liab_JU77_MA_P6p_wt_mod),effectiveSize(Evol_liab_JU77_Vm_P6p_wt_mod))
        colnames(effectiveSize_Evol_liab_JU77_P6p_wt) <- c("effectiveSize")
        Evol_liab_JU77_P6p_wt <- cbind.data.frame(mean_Evol_liab_JU77_P6p_wt,median_Evol_liab_JU77_P6p_wt,posterior.mode_Evol_liab_JU77_P6p_wt,HPDinterval_0.95_Evol_liab_JU77_P6p_wt,HPDinterval_0.83_Evol_liab_JU77_P6p_wt,effectiveSize_Evol_liab_JU77_P6p_wt)
        rownames(Evol_liab_JU77_P6p_wt) <- c("Evol_liab_JU77_CONTROL_P6p_wt_mod","Evol_liab_JU77_MA_P6p_wt_mod","Evol_liab_JU77_Vm_P6p_wt_mod")
        Evol_liab_JU77_P6p_wt <- cbind(Models = rownames(Evol_liab_JU77_P6p_wt),Evol_liab_JU77_P6p_wt)
        rownames(Evol_liab_JU77_P6p_wt) <- NULL
        Evol_liab_JU77_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        Evol_liab_JU77_P6p_wt$Treatment <- c("Control","ML","Vm")
        Evol_liab_JU77_P6p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_JU77_P6p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_JU77_P6p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_JU77_P6p_wt
      }
      
      #Summary trait_mean_liab_JU77_P6p_wt
      {
        mean_trait_mean_liab_JU77_P6p_wt <- rbind(mean(trait_mean_liab_JU77_CONTROL_P6p_wt_mod),mean(trait_mean_liab_JU77_MA_P6p_wt_mod),mean(trait_mean_liab_JU77_Vm_P6p_wt_mod))
        colnames(mean_trait_mean_liab_JU77_P6p_wt) <- c("mean")
        median_trait_mean_liab_JU77_P6p_wt <- rbind(median(trait_mean_liab_JU77_CONTROL_P6p_wt_mod),median(trait_mean_liab_JU77_MA_P6p_wt_mod),median(trait_mean_liab_JU77_Vm_P6p_wt_mod))
        colnames(median_trait_mean_liab_JU77_P6p_wt) <- c("median")
        posterior.mode_trait_mean_liab_JU77_P6p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_JU77_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU77_MA_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU77_Vm_P6p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_JU77_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_JU77_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU77_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU77_MA_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU77_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_JU77_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_JU77_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU77_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU77_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU77_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_JU77_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_JU77_P6p_wt <- rbind(effectiveSize(trait_mean_liab_JU77_CONTROL_P6p_wt_mod),effectiveSize(trait_mean_liab_JU77_MA_P6p_wt_mod),effectiveSize(trait_mean_liab_JU77_Vm_P6p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_JU77_P6p_wt) <- c("effectiveSize")
        trait_mean_liab_JU77_P6p_wt <- cbind.data.frame(mean_trait_mean_liab_JU77_P6p_wt,median_trait_mean_liab_JU77_P6p_wt,posterior.mode_trait_mean_liab_JU77_P6p_wt,HPDinterval_0.95_trait_mean_liab_JU77_P6p_wt,HPDinterval_0.83_trait_mean_liab_JU77_P6p_wt,effectiveSize_trait_mean_liab_JU77_P6p_wt)
        rownames(trait_mean_liab_JU77_P6p_wt) <- c("trait_mean_liab_JU77_CONTROL_P6p_wt_mod","trait_mean_liab_JU77_MA_P6p_wt_mod","trait_mean_liab_JU77_Vm_P6p_wt_mod")
        trait_mean_liab_JU77_P6p_wt <- cbind(Models = rownames(trait_mean_liab_JU77_P6p_wt),trait_mean_liab_JU77_P6p_wt)
        rownames(trait_mean_liab_JU77_P6p_wt) <- NULL
        trait_mean_liab_JU77_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        trait_mean_liab_JU77_P6p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_JU77_P6p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_JU77_P6p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_JU77_P6p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_JU77_P6p_wt
      }
      
      liab_JU77_P6p_wt <- rbind.data.frame(va_liab_JU77_P6p_wt, h2_liab_JU77_P6p_wt,Evol_liab_JU77_P6p_wt,trait_mean_liab_JU77_P6p_wt)
      liab_JU77_P6p_wt
    }
    #Summary data scale JU77 P6p
    {
      #Summary va_data_JU77_P6p_wt:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_JU77_P6p_wt <- rbind(mean(va_data_JU77_CONTROL_P6p_wt_mod/2),mean(va_data_JU77_MA_P6p_wt_mod/2),mean(va_data_JU77_Vm_P6p_wt_mod/2))
        colnames(mean_va_data_JU77_P6p_wt) <- c("mean")
        median_va_data_JU77_P6p_wt <- rbind(median(va_data_JU77_CONTROL_P6p_wt_mod/2),median(va_data_JU77_MA_P6p_wt_mod/2),median(va_data_JU77_Vm_P6p_wt_mod/2))
        colnames(median_va_data_JU77_P6p_wt) <- c("median")
        posterior.mode_va_data_JU77_P6p_wt <- rbind(posterior.mode(as.mcmc(va_data_JU77_CONTROL_P6p_wt_mod/2)),posterior.mode(as.mcmc(va_data_JU77_MA_P6p_wt_mod/2)),posterior.mode(as.mcmc(va_data_JU77_Vm_P6p_wt_mod/2)))
        colnames(posterior.mode_va_data_JU77_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_JU77_P6p_wt <- rbind(HPDinterval(as.mcmc(va_data_JU77_CONTROL_P6p_wt_mod/2)),HPDinterval(as.mcmc(va_data_JU77_MA_P6p_wt_mod/2)),HPDinterval(as.mcmc(va_data_JU77_Vm_P6p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_JU77_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_JU77_P6p_wt <- rbind(HPDinterval(as.mcmc(va_data_JU77_CONTROL_P6p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU77_MA_P6p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU77_Vm_P6p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_JU77_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_JU77_P6p_wt <- rbind(effectiveSize(va_data_JU77_CONTROL_P6p_wt_mod/2),effectiveSize(va_data_JU77_MA_P6p_wt_mod/2),effectiveSize(va_data_JU77_Vm_P6p_wt_mod/2))
        colnames(effectiveSize_va_data_JU77_P6p_wt) <- c("effectiveSize")
        va_data_JU77_P6p_wt <- cbind.data.frame(mean_va_data_JU77_P6p_wt,median_va_data_JU77_P6p_wt,posterior.mode_va_data_JU77_P6p_wt,HPDinterval_0.95_va_data_JU77_P6p_wt,HPDinterval_0.83_va_data_JU77_P6p_wt,effectiveSize_va_data_JU77_P6p_wt)
        rownames(va_data_JU77_P6p_wt) <- c("va_data_JU77_CONTROL_P6p_wt_mod","va_data_JU77_MA_P6p_wt_mod","va_data_JU77_Vm_P6p_wt_mod")
        va_data_JU77_P6p_wt <- cbind(Models = rownames(va_data_JU77_P6p_wt),va_data_JU77_P6p_wt)
        rownames(va_data_JU77_P6p_wt) <- NULL
        va_data_JU77_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        va_data_JU77_P6p_wt$Treatment <- c("Control","ML","Vm")
        va_data_JU77_P6p_wt$Measure <- c("Va","Va","Va")
        va_data_JU77_P6p_wt$Scale <- c("data","data","data")
        va_data_JU77_P6p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_JU77_P6p_wt
      }
      
      #Summary h2_data_JU77_P6p_wt
      {
        mean_h2_data_JU77_P6p_wt <- rbind(mean(h2_data_JU77_CONTROL_P6p_wt_mod),mean(h2_data_JU77_MA_P6p_wt_mod),mean(h2_data_JU77_Vm_P6p_wt_mod))
        colnames(mean_h2_data_JU77_P6p_wt) <- c("mean")
        median_h2_data_JU77_P6p_wt <- rbind(median(h2_data_JU77_CONTROL_P6p_wt_mod),median(h2_data_JU77_MA_P6p_wt_mod),median(h2_data_JU77_Vm_P6p_wt_mod))
        colnames(median_h2_data_JU77_P6p_wt) <- c("median")
        posterior.mode_h2_data_JU77_P6p_wt <- rbind(posterior.mode(as.mcmc(h2_data_JU77_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(h2_data_JU77_MA_P6p_wt_mod)),posterior.mode(as.mcmc(h2_data_JU77_Vm_P6p_wt_mod)))
        colnames(posterior.mode_h2_data_JU77_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_JU77_P6p_wt <- rbind(HPDinterval(as.mcmc(h2_data_JU77_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(h2_data_JU77_MA_P6p_wt_mod)),HPDinterval(as.mcmc(h2_data_JU77_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_JU77_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_JU77_P6p_wt <- rbind(HPDinterval(as.mcmc(h2_data_JU77_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU77_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU77_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_JU77_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_JU77_P6p_wt <- rbind(effectiveSize(h2_data_JU77_CONTROL_P6p_wt_mod),effectiveSize(h2_data_JU77_MA_P6p_wt_mod),effectiveSize(h2_data_JU77_Vm_P6p_wt_mod))
        colnames(effectiveSize_h2_data_JU77_P6p_wt) <- c("effectiveSize")
        h2_data_JU77_P6p_wt <- cbind.data.frame(mean_h2_data_JU77_P6p_wt,median_h2_data_JU77_P6p_wt,posterior.mode_h2_data_JU77_P6p_wt,HPDinterval_0.95_h2_data_JU77_P6p_wt,HPDinterval_0.83_h2_data_JU77_P6p_wt,effectiveSize_h2_data_JU77_P6p_wt)
        rownames(h2_data_JU77_P6p_wt) <- c("h2_data_JU77_CONTROL_P6p_wt_mod","h2_data_JU77_MA_P6p_wt_mod","h2_data_JU77_Vm_P6p_wt_mod")
        h2_data_JU77_P6p_wt <- cbind(Models = rownames(h2_data_JU77_P6p_wt),h2_data_JU77_P6p_wt)
        rownames(h2_data_JU77_P6p_wt) <- NULL
        h2_data_JU77_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        h2_data_JU77_P6p_wt$Treatment <- c("Control","ML","Vm")
        h2_data_JU77_P6p_wt$Measure <- c("H2","H2","H2")
        h2_data_JU77_P6p_wt$Scale <- c("data","data","data")
        h2_data_JU77_P6p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_JU77_P6p_wt
      }
      
      #Summary Evol_data_JU77_P6p_wt
      {
        mean_Evol_data_JU77_P6p_wt <- rbind(mean(Evol_data_JU77_CONTROL_P6p_wt_mod),mean(Evol_data_JU77_MA_P6p_wt_mod),mean(Evol_data_JU77_Vm_P6p_wt_mod))
        colnames(mean_Evol_data_JU77_P6p_wt) <- c("mean")
        median_Evol_data_JU77_P6p_wt <- rbind(median(Evol_data_JU77_CONTROL_P6p_wt_mod),median(Evol_data_JU77_MA_P6p_wt_mod),median(Evol_data_JU77_Vm_P6p_wt_mod))
        colnames(median_Evol_data_JU77_P6p_wt) <- c("median")
        posterior.mode_Evol_data_JU77_P6p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_JU77_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(Evol_data_JU77_MA_P6p_wt_mod)),posterior.mode(as.mcmc(Evol_data_JU77_Vm_P6p_wt_mod)))
        colnames(posterior.mode_Evol_data_JU77_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_JU77_P6p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_JU77_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(Evol_data_JU77_MA_P6p_wt_mod)),HPDinterval(as.mcmc(Evol_data_JU77_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_JU77_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_JU77_P6p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_JU77_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU77_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU77_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_JU77_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_JU77_P6p_wt <- rbind(effectiveSize(Evol_data_JU77_CONTROL_P6p_wt_mod),effectiveSize(Evol_data_JU77_MA_P6p_wt_mod),effectiveSize(Evol_data_JU77_Vm_P6p_wt_mod))
        colnames(effectiveSize_Evol_data_JU77_P6p_wt) <- c("effectiveSize")
        Evol_data_JU77_P6p_wt <- cbind.data.frame(mean_Evol_data_JU77_P6p_wt,median_Evol_data_JU77_P6p_wt,posterior.mode_Evol_data_JU77_P6p_wt,HPDinterval_0.95_Evol_data_JU77_P6p_wt,HPDinterval_0.83_Evol_data_JU77_P6p_wt,effectiveSize_Evol_data_JU77_P6p_wt)
        rownames(Evol_data_JU77_P6p_wt) <- c("Evol_data_JU77_CONTROL_P6p_wt_mod","Evol_data_JU77_MA_P6p_wt_mod","Evol_data_JU77_Vm_P6p_wt_mod")
        Evol_data_JU77_P6p_wt <- cbind(Models = rownames(Evol_data_JU77_P6p_wt),Evol_data_JU77_P6p_wt)
        rownames(Evol_data_JU77_P6p_wt) <- NULL
        Evol_data_JU77_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        Evol_data_JU77_P6p_wt$Treatment <- c("Control","ML","Vm")
        Evol_data_JU77_P6p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_JU77_P6p_wt$Scale <- c("data","data","data")
        Evol_data_JU77_P6p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_JU77_P6p_wt
      }
      
      #Summary trait_mean_data_JU77_P6p_wt
      {
        mean_trait_mean_data_JU77_P6p_wt <- rbind(mean(trait_mean_data_JU77_CONTROL_P6p_wt_mod),mean(trait_mean_data_JU77_MA_P6p_wt_mod),mean(trait_mean_data_JU77_Vm_P6p_wt_mod))
        colnames(mean_trait_mean_data_JU77_P6p_wt) <- c("mean")
        median_trait_mean_data_JU77_P6p_wt <- rbind(median(trait_mean_data_JU77_CONTROL_P6p_wt_mod),median(trait_mean_data_JU77_MA_P6p_wt_mod),median(trait_mean_data_JU77_Vm_P6p_wt_mod))
        colnames(median_trait_mean_data_JU77_P6p_wt) <- c("median")
        posterior.mode_trait_mean_data_JU77_P6p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_JU77_CONTROL_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_JU77_MA_P6p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_JU77_Vm_P6p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_JU77_P6p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_JU77_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU77_CONTROL_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_JU77_MA_P6p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_JU77_Vm_P6p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_JU77_P6p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_JU77_P6p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU77_CONTROL_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU77_MA_P6p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU77_Vm_P6p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_JU77_P6p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_JU77_P6p_wt <- rbind(effectiveSize(trait_mean_data_JU77_CONTROL_P6p_wt_mod),effectiveSize(trait_mean_data_JU77_MA_P6p_wt_mod),effectiveSize(trait_mean_data_JU77_Vm_P6p_wt_mod))
        colnames(effectiveSize_trait_mean_data_JU77_P6p_wt) <- c("effectiveSize")
        trait_mean_data_JU77_P6p_wt <- cbind.data.frame(mean_trait_mean_data_JU77_P6p_wt,median_trait_mean_data_JU77_P6p_wt,posterior.mode_trait_mean_data_JU77_P6p_wt,HPDinterval_0.95_trait_mean_data_JU77_P6p_wt,HPDinterval_0.83_trait_mean_data_JU77_P6p_wt,effectiveSize_trait_mean_data_JU77_P6p_wt)
        rownames(trait_mean_data_JU77_P6p_wt) <- c("trait_mean_data_JU77_CONTROL_P6p_wt_mod","trait_mean_data_JU77_MA_P6p_wt_mod","trait_mean_data_JU77_Vm_P6p_wt_mod")
        trait_mean_data_JU77_P6p_wt <- cbind(Models = rownames(trait_mean_data_JU77_P6p_wt),trait_mean_data_JU77_P6p_wt)
        rownames(trait_mean_data_JU77_P6p_wt) <- NULL
        trait_mean_data_JU77_P6p_wt$Pnp <- c("P6.p","P6.p","P6.p")
        trait_mean_data_JU77_P6p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_data_JU77_P6p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_JU77_P6p_wt$Scale <- c("data","data","data")
        trait_mean_data_JU77_P6p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_JU77_P6p_wt
      }
      
      data_JU77_P6p_wt <- rbind.data.frame(va_data_JU77_P6p_wt, h2_data_JU77_P6p_wt,Evol_data_JU77_P6p_wt,trait_mean_data_JU77_P6p_wt)
      data_JU77_P6p_wt
      
    }
    Vm_JU77_P6p_wt <- rbind.data.frame(liab_JU77_P6p_wt, data_JU77_P6p_wt)
    Vm_JU77_P6p_wt$Pnp_fate <- rep("wt", 24)
    Vm_JU77_P6p_wt
    #remove JU77 P6p_wt models
    {
      remove(JU77_CONTROL_P6p_wt_mod)
      remove(JU77_MA_P6p_wt_mod)
      remove(JU77_Vm_P6p_wt_mod)
    }
  }
  
  ##Summary JU77 P7p----
  {
    #Summary liability scale JU77 P7p
    {
      #Summary va_liab_JU77_P7p_wt: NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        
        mean_va_liab_JU77_P7p_wt <- rbind(mean(va_liab_JU77_CONTROL_P7p_wt_mod/2),mean(va_liab_JU77_MA_P7p_wt_mod/2),mean(va_liab_JU77_Vm_P7p_wt_mod/2))
        colnames(mean_va_liab_JU77_P7p_wt) <- c("mean")
        median_va_liab_JU77_P7p_wt <- rbind(median(va_liab_JU77_CONTROL_P7p_wt_mod/2),median(va_liab_JU77_MA_P7p_wt_mod/2),median(va_liab_JU77_Vm_P7p_wt_mod/2))
        colnames(median_va_liab_JU77_P7p_wt) <- c("median")
        posterior.mode_va_liab_JU77_P7p_wt <- rbind(posterior.mode(va_liab_JU77_CONTROL_P7p_wt_mod/2),posterior.mode(va_liab_JU77_MA_P7p_wt_mod/2),posterior.mode(va_liab_JU77_Vm_P7p_wt_mod/2))
        colnames(posterior.mode_va_liab_JU77_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_JU77_P7p_wt <- rbind(HPDinterval(va_liab_JU77_CONTROL_P7p_wt_mod/2),HPDinterval(va_liab_JU77_MA_P7p_wt_mod/2),HPDinterval(va_liab_JU77_Vm_P7p_wt_mod/2))
        colnames(HPDinterval_0.95_va_liab_JU77_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_JU77_P7p_wt <- rbind(HPDinterval(va_liab_JU77_CONTROL_P7p_wt_mod/2,prob=.83),HPDinterval(va_liab_JU77_MA_P7p_wt_mod/2,prob=.83),HPDinterval(va_liab_JU77_Vm_P7p_wt_mod/2,prob=.83))
        colnames(HPDinterval_0.83_va_liab_JU77_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_JU77_P7p_wt <- rbind(effectiveSize(va_liab_JU77_CONTROL_P7p_wt_mod/2),effectiveSize(va_liab_JU77_MA_P7p_wt_mod/2),effectiveSize(va_liab_JU77_Vm_P7p_wt_mod/2))
        colnames(effectiveSize_va_liab_JU77_P7p_wt) <- c("effectiveSize")
        va_liab_JU77_P7p_wt <- cbind.data.frame(mean_va_liab_JU77_P7p_wt,median_va_liab_JU77_P7p_wt,posterior.mode_va_liab_JU77_P7p_wt,HPDinterval_0.95_va_liab_JU77_P7p_wt,HPDinterval_0.83_va_liab_JU77_P7p_wt,effectiveSize_va_liab_JU77_P7p_wt)
        rownames(va_liab_JU77_P7p_wt) <- c("va_liab_JU77_CONTROL_P7p_wt_mod","va_liab_JU77_MA_P7p_wt_mod","va_liab_JU77_Vm_P7p_wt_mod")
        va_liab_JU77_P7p_wt <- cbind(Models = rownames(va_liab_JU77_P7p_wt),va_liab_JU77_P7p_wt)
        rownames(va_liab_JU77_P7p_wt) <- NULL
        va_liab_JU77_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        va_liab_JU77_P7p_wt$Treatment <- c("Control","ML","Vm")
        va_liab_JU77_P7p_wt$Measure <- c("Va","Va","Va")
        va_liab_JU77_P7p_wt$Scale <- c("liab","liab","liab")
        va_liab_JU77_P7p_wt$Variance <- c("Vm","Vm","Vm")
        va_liab_JU77_P7p_wt
      }
      
      #Summary h2_liab_JU77_P7p_wt
      {
        mean_h2_liab_JU77_P7p_wt <- rbind(mean(h2_liab_JU77_CONTROL_P7p_wt_mod),mean(h2_liab_JU77_MA_P7p_wt_mod),mean(h2_liab_JU77_Vm_P7p_wt_mod))
        colnames(mean_h2_liab_JU77_P7p_wt) <- c("mean")
        median_h2_liab_JU77_P7p_wt <- rbind(median(h2_liab_JU77_CONTROL_P7p_wt_mod),median(h2_liab_JU77_MA_P7p_wt_mod),median(h2_liab_JU77_Vm_P7p_wt_mod))
        colnames(median_h2_liab_JU77_P7p_wt) <- c("median")
        posterior.mode_h2_liab_JU77_P7p_wt <- rbind(posterior.mode(h2_liab_JU77_CONTROL_P7p_wt_mod),posterior.mode(h2_liab_JU77_MA_P7p_wt_mod),posterior.mode(h2_liab_JU77_Vm_P7p_wt_mod))
        colnames(posterior.mode_h2_liab_JU77_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_liab_JU77_P7p_wt <- rbind(HPDinterval(h2_liab_JU77_CONTROL_P7p_wt_mod),HPDinterval(h2_liab_JU77_MA_P7p_wt_mod),HPDinterval(h2_liab_JU77_Vm_P7p_wt_mod))
        colnames(HPDinterval_0.95_h2_liab_JU77_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_liab_JU77_P7p_wt <- rbind(HPDinterval(h2_liab_JU77_CONTROL_P7p_wt_mod,prob=.83),HPDinterval(h2_liab_JU77_MA_P7p_wt_mod,prob=.83),HPDinterval(h2_liab_JU77_Vm_P7p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_h2_liab_JU77_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_liab_JU77_P7p_wt <- rbind(effectiveSize(h2_liab_JU77_CONTROL_P7p_wt_mod),effectiveSize(h2_liab_JU77_MA_P7p_wt_mod),effectiveSize(h2_liab_JU77_Vm_P7p_wt_mod))
        colnames(effectiveSize_h2_liab_JU77_P7p_wt) <- c("effectiveSize")
        h2_liab_JU77_P7p_wt <- cbind.data.frame(mean_h2_liab_JU77_P7p_wt,median_h2_liab_JU77_P7p_wt,posterior.mode_h2_liab_JU77_P7p_wt,HPDinterval_0.95_h2_liab_JU77_P7p_wt,HPDinterval_0.83_h2_liab_JU77_P7p_wt,effectiveSize_h2_liab_JU77_P7p_wt)
        rownames(h2_liab_JU77_P7p_wt) <- c("h2_liab_JU77_CONTROL_P7p_wt_mod","h2_liab_JU77_MA_P7p_wt_mod","h2_liab_JU77_Vm_P7p_wt_mod")
        h2_liab_JU77_P7p_wt <- cbind(Models = rownames(h2_liab_JU77_P7p_wt),h2_liab_JU77_P7p_wt)
        rownames(h2_liab_JU77_P7p_wt) <- NULL
        h2_liab_JU77_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        h2_liab_JU77_P7p_wt$Treatment <- c("Control","ML","Vm")
        h2_liab_JU77_P7p_wt$Measure <- c("H2","H2","H2")
        h2_liab_JU77_P7p_wt$Scale <- c("liab","liab","liab")
        h2_liab_JU77_P7p_wt$Variance <- c("Vm","Vm","Vm")
        h2_liab_JU77_P7p_wt
      }
      
      #Summary Evol_liab_JU77_P7p_wt
      {
        mean_Evol_liab_JU77_P7p_wt <- rbind(mean(Evol_liab_JU77_CONTROL_P7p_wt_mod),mean(Evol_liab_JU77_MA_P7p_wt_mod),mean(Evol_liab_JU77_Vm_P7p_wt_mod))
        colnames(mean_Evol_liab_JU77_P7p_wt) <- c("mean")
        median_Evol_liab_JU77_P7p_wt <- rbind(median(Evol_liab_JU77_CONTROL_P7p_wt_mod),median(Evol_liab_JU77_MA_P7p_wt_mod),median(Evol_liab_JU77_Vm_P7p_wt_mod))
        colnames(median_Evol_liab_JU77_P7p_wt) <- c("median")
        posterior.mode_Evol_liab_JU77_P7p_wt <- rbind(posterior.mode(Evol_liab_JU77_CONTROL_P7p_wt_mod),posterior.mode(Evol_liab_JU77_MA_P7p_wt_mod),posterior.mode(Evol_liab_JU77_Vm_P7p_wt_mod))
        colnames(posterior.mode_Evol_liab_JU77_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_JU77_P7p_wt <- rbind(HPDinterval(Evol_liab_JU77_CONTROL_P7p_wt_mod),HPDinterval(Evol_liab_JU77_MA_P7p_wt_mod),HPDinterval(Evol_liab_JU77_Vm_P7p_wt_mod))
        colnames(HPDinterval_0.95_Evol_liab_JU77_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_JU77_P7p_wt <- rbind(HPDinterval(Evol_liab_JU77_CONTROL_P7p_wt_mod,prob=.83),HPDinterval(Evol_liab_JU77_MA_P7p_wt_mod,prob=.83),HPDinterval(Evol_liab_JU77_Vm_P7p_wt_mod,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_JU77_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_JU77_P7p_wt <- rbind(effectiveSize(Evol_liab_JU77_CONTROL_P7p_wt_mod),effectiveSize(Evol_liab_JU77_MA_P7p_wt_mod),effectiveSize(Evol_liab_JU77_Vm_P7p_wt_mod))
        colnames(effectiveSize_Evol_liab_JU77_P7p_wt) <- c("effectiveSize")
        Evol_liab_JU77_P7p_wt <- cbind.data.frame(mean_Evol_liab_JU77_P7p_wt,median_Evol_liab_JU77_P7p_wt,posterior.mode_Evol_liab_JU77_P7p_wt,HPDinterval_0.95_Evol_liab_JU77_P7p_wt,HPDinterval_0.83_Evol_liab_JU77_P7p_wt,effectiveSize_Evol_liab_JU77_P7p_wt)
        rownames(Evol_liab_JU77_P7p_wt) <- c("Evol_liab_JU77_CONTROL_P7p_wt_mod","Evol_liab_JU77_MA_P7p_wt_mod","Evol_liab_JU77_Vm_P7p_wt_mod")
        Evol_liab_JU77_P7p_wt <- cbind(Models = rownames(Evol_liab_JU77_P7p_wt),Evol_liab_JU77_P7p_wt)
        rownames(Evol_liab_JU77_P7p_wt) <- NULL
        Evol_liab_JU77_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        Evol_liab_JU77_P7p_wt$Treatment <- c("Control","ML","Vm")
        Evol_liab_JU77_P7p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_liab_JU77_P7p_wt$Scale <- c("liab","liab","liab")
        Evol_liab_JU77_P7p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_liab_JU77_P7p_wt
      }
      
      #Summary trait_mean_liab_JU77_P7p_wt
      {
        mean_trait_mean_liab_JU77_P7p_wt <- rbind(mean(trait_mean_liab_JU77_CONTROL_P7p_wt_mod),mean(trait_mean_liab_JU77_MA_P7p_wt_mod),mean(trait_mean_liab_JU77_Vm_P7p_wt_mod))
        colnames(mean_trait_mean_liab_JU77_P7p_wt) <- c("mean")
        median_trait_mean_liab_JU77_P7p_wt <- rbind(median(trait_mean_liab_JU77_CONTROL_P7p_wt_mod),median(trait_mean_liab_JU77_MA_P7p_wt_mod),median(trait_mean_liab_JU77_Vm_P7p_wt_mod))
        colnames(median_trait_mean_liab_JU77_P7p_wt) <- c("median")
        posterior.mode_trait_mean_liab_JU77_P7p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_liab_JU77_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU77_MA_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_liab_JU77_Vm_P7p_wt_mod)))
        colnames(posterior.mode_trait_mean_liab_JU77_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_JU77_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU77_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU77_MA_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_liab_JU77_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_liab_JU77_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_JU77_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_liab_JU77_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU77_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_liab_JU77_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_JU77_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_JU77_P7p_wt <- rbind(effectiveSize(trait_mean_liab_JU77_CONTROL_P7p_wt_mod),effectiveSize(trait_mean_liab_JU77_MA_P7p_wt_mod),effectiveSize(trait_mean_liab_JU77_Vm_P7p_wt_mod))
        colnames(effectiveSize_trait_mean_liab_JU77_P7p_wt) <- c("effectiveSize")
        trait_mean_liab_JU77_P7p_wt <- cbind.data.frame(mean_trait_mean_liab_JU77_P7p_wt,median_trait_mean_liab_JU77_P7p_wt,posterior.mode_trait_mean_liab_JU77_P7p_wt,HPDinterval_0.95_trait_mean_liab_JU77_P7p_wt,HPDinterval_0.83_trait_mean_liab_JU77_P7p_wt,effectiveSize_trait_mean_liab_JU77_P7p_wt)
        rownames(trait_mean_liab_JU77_P7p_wt) <- c("trait_mean_liab_JU77_CONTROL_P7p_wt_mod","trait_mean_liab_JU77_MA_P7p_wt_mod","trait_mean_liab_JU77_Vm_P7p_wt_mod")
        trait_mean_liab_JU77_P7p_wt <- cbind(Models = rownames(trait_mean_liab_JU77_P7p_wt),trait_mean_liab_JU77_P7p_wt)
        rownames(trait_mean_liab_JU77_P7p_wt) <- NULL
        trait_mean_liab_JU77_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        trait_mean_liab_JU77_P7p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_liab_JU77_P7p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_liab_JU77_P7p_wt$Scale <- c("liab","liab","liab")
        trait_mean_liab_JU77_P7p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_liab_JU77_P7p_wt
      }
      
      liab_JU77_P7p_wt <- rbind.data.frame(va_liab_JU77_P7p_wt, h2_liab_JU77_P7p_wt,Evol_liab_JU77_P7p_wt,trait_mean_liab_JU77_P7p_wt)
      liab_JU77_P7p_wt
    }
    #Summary data scale JU77 P7p
    {
      #Summary va_data_JU77_P7p_wt:NB Given that mutant lines are homozygous, Va is divided by 2 to estimate the increase in genetic variance resulting from new mutations
      {
        mean_va_data_JU77_P7p_wt <- rbind(mean(va_data_JU77_CONTROL_P7p_wt_mod/2),mean(va_data_JU77_MA_P7p_wt_mod/2),mean(va_data_JU77_Vm_P7p_wt_mod/2))
        colnames(mean_va_data_JU77_P7p_wt) <- c("mean")
        median_va_data_JU77_P7p_wt <- rbind(median(va_data_JU77_CONTROL_P7p_wt_mod/2),median(va_data_JU77_MA_P7p_wt_mod/2),median(va_data_JU77_Vm_P7p_wt_mod/2))
        colnames(median_va_data_JU77_P7p_wt) <- c("median")
        posterior.mode_va_data_JU77_P7p_wt <- rbind(posterior.mode(as.mcmc(va_data_JU77_CONTROL_P7p_wt_mod/2)),posterior.mode(as.mcmc(va_data_JU77_MA_P7p_wt_mod/2)),posterior.mode(as.mcmc(va_data_JU77_Vm_P7p_wt_mod/2)))
        colnames(posterior.mode_va_data_JU77_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_va_data_JU77_P7p_wt <- rbind(HPDinterval(as.mcmc(va_data_JU77_CONTROL_P7p_wt_mod/2)),HPDinterval(as.mcmc(va_data_JU77_MA_P7p_wt_mod/2)),HPDinterval(as.mcmc(va_data_JU77_Vm_P7p_wt_mod/2)))
        colnames(HPDinterval_0.95_va_data_JU77_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_JU77_P7p_wt <- rbind(HPDinterval(as.mcmc(va_data_JU77_CONTROL_P7p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU77_MA_P7p_wt_mod/2),prob=.83),HPDinterval(as.mcmc(va_data_JU77_Vm_P7p_wt_mod/2),prob=.83))
        colnames(HPDinterval_0.83_va_data_JU77_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_JU77_P7p_wt <- rbind(effectiveSize(va_data_JU77_CONTROL_P7p_wt_mod/2),effectiveSize(va_data_JU77_MA_P7p_wt_mod/2),effectiveSize(va_data_JU77_Vm_P7p_wt_mod/2))
        colnames(effectiveSize_va_data_JU77_P7p_wt) <- c("effectiveSize")
        va_data_JU77_P7p_wt <- cbind.data.frame(mean_va_data_JU77_P7p_wt,median_va_data_JU77_P7p_wt,posterior.mode_va_data_JU77_P7p_wt,HPDinterval_0.95_va_data_JU77_P7p_wt,HPDinterval_0.83_va_data_JU77_P7p_wt,effectiveSize_va_data_JU77_P7p_wt)
        rownames(va_data_JU77_P7p_wt) <- c("va_data_JU77_CONTROL_P7p_wt_mod","va_data_JU77_MA_P7p_wt_mod","va_data_JU77_Vm_P7p_wt_mod")
        va_data_JU77_P7p_wt <- cbind(Models = rownames(va_data_JU77_P7p_wt),va_data_JU77_P7p_wt)
        rownames(va_data_JU77_P7p_wt) <- NULL
        va_data_JU77_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        va_data_JU77_P7p_wt$Treatment <- c("Control","ML","Vm")
        va_data_JU77_P7p_wt$Measure <- c("Va","Va","Va")
        va_data_JU77_P7p_wt$Scale <- c("data","data","data")
        va_data_JU77_P7p_wt$Variance <- c("Vm","Vm","Vm")
        va_data_JU77_P7p_wt
      }
      
      #Summary h2_data_JU77_P7p_wt
      {
        mean_h2_data_JU77_P7p_wt <- rbind(mean(h2_data_JU77_CONTROL_P7p_wt_mod),mean(h2_data_JU77_MA_P7p_wt_mod),mean(h2_data_JU77_Vm_P7p_wt_mod))
        colnames(mean_h2_data_JU77_P7p_wt) <- c("mean")
        median_h2_data_JU77_P7p_wt <- rbind(median(h2_data_JU77_CONTROL_P7p_wt_mod),median(h2_data_JU77_MA_P7p_wt_mod),median(h2_data_JU77_Vm_P7p_wt_mod))
        colnames(median_h2_data_JU77_P7p_wt) <- c("median")
        posterior.mode_h2_data_JU77_P7p_wt <- rbind(posterior.mode(as.mcmc(h2_data_JU77_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(h2_data_JU77_MA_P7p_wt_mod)),posterior.mode(as.mcmc(h2_data_JU77_Vm_P7p_wt_mod)))
        colnames(posterior.mode_h2_data_JU77_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_h2_data_JU77_P7p_wt <- rbind(HPDinterval(as.mcmc(h2_data_JU77_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(h2_data_JU77_MA_P7p_wt_mod)),HPDinterval(as.mcmc(h2_data_JU77_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_h2_data_JU77_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_h2_data_JU77_P7p_wt <- rbind(HPDinterval(as.mcmc(h2_data_JU77_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU77_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(h2_data_JU77_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_h2_data_JU77_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_h2_data_JU77_P7p_wt <- rbind(effectiveSize(h2_data_JU77_CONTROL_P7p_wt_mod),effectiveSize(h2_data_JU77_MA_P7p_wt_mod),effectiveSize(h2_data_JU77_Vm_P7p_wt_mod))
        colnames(effectiveSize_h2_data_JU77_P7p_wt) <- c("effectiveSize")
        h2_data_JU77_P7p_wt <- cbind.data.frame(mean_h2_data_JU77_P7p_wt,median_h2_data_JU77_P7p_wt,posterior.mode_h2_data_JU77_P7p_wt,HPDinterval_0.95_h2_data_JU77_P7p_wt,HPDinterval_0.83_h2_data_JU77_P7p_wt,effectiveSize_h2_data_JU77_P7p_wt)
        rownames(h2_data_JU77_P7p_wt) <- c("h2_data_JU77_CONTROL_P7p_wt_mod","h2_data_JU77_MA_P7p_wt_mod","h2_data_JU77_Vm_P7p_wt_mod")
        h2_data_JU77_P7p_wt <- cbind(Models = rownames(h2_data_JU77_P7p_wt),h2_data_JU77_P7p_wt)
        rownames(h2_data_JU77_P7p_wt) <- NULL
        h2_data_JU77_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        h2_data_JU77_P7p_wt$Treatment <- c("Control","ML","Vm")
        h2_data_JU77_P7p_wt$Measure <- c("H2","H2","H2")
        h2_data_JU77_P7p_wt$Scale <- c("data","data","data")
        h2_data_JU77_P7p_wt$Variance <- c("Vm","Vm","Vm")
        h2_data_JU77_P7p_wt
      }
      
      #Summary Evol_data_JU77_P7p_wt
      {
        mean_Evol_data_JU77_P7p_wt <- rbind(mean(Evol_data_JU77_CONTROL_P7p_wt_mod),mean(Evol_data_JU77_MA_P7p_wt_mod),mean(Evol_data_JU77_Vm_P7p_wt_mod))
        colnames(mean_Evol_data_JU77_P7p_wt) <- c("mean")
        median_Evol_data_JU77_P7p_wt <- rbind(median(Evol_data_JU77_CONTROL_P7p_wt_mod),median(Evol_data_JU77_MA_P7p_wt_mod),median(Evol_data_JU77_Vm_P7p_wt_mod))
        colnames(median_Evol_data_JU77_P7p_wt) <- c("median")
        posterior.mode_Evol_data_JU77_P7p_wt <- rbind(posterior.mode(as.mcmc(Evol_data_JU77_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(Evol_data_JU77_MA_P7p_wt_mod)),posterior.mode(as.mcmc(Evol_data_JU77_Vm_P7p_wt_mod)))
        colnames(posterior.mode_Evol_data_JU77_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_JU77_P7p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_JU77_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(Evol_data_JU77_MA_P7p_wt_mod)),HPDinterval(as.mcmc(Evol_data_JU77_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_Evol_data_JU77_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_JU77_P7p_wt <- rbind(HPDinterval(as.mcmc(Evol_data_JU77_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU77_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(Evol_data_JU77_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_JU77_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_JU77_P7p_wt <- rbind(effectiveSize(Evol_data_JU77_CONTROL_P7p_wt_mod),effectiveSize(Evol_data_JU77_MA_P7p_wt_mod),effectiveSize(Evol_data_JU77_Vm_P7p_wt_mod))
        colnames(effectiveSize_Evol_data_JU77_P7p_wt) <- c("effectiveSize")
        Evol_data_JU77_P7p_wt <- cbind.data.frame(mean_Evol_data_JU77_P7p_wt,median_Evol_data_JU77_P7p_wt,posterior.mode_Evol_data_JU77_P7p_wt,HPDinterval_0.95_Evol_data_JU77_P7p_wt,HPDinterval_0.83_Evol_data_JU77_P7p_wt,effectiveSize_Evol_data_JU77_P7p_wt)
        rownames(Evol_data_JU77_P7p_wt) <- c("Evol_data_JU77_CONTROL_P7p_wt_mod","Evol_data_JU77_MA_P7p_wt_mod","Evol_data_JU77_Vm_P7p_wt_mod")
        Evol_data_JU77_P7p_wt <- cbind(Models = rownames(Evol_data_JU77_P7p_wt),Evol_data_JU77_P7p_wt)
        rownames(Evol_data_JU77_P7p_wt) <- NULL
        Evol_data_JU77_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        Evol_data_JU77_P7p_wt$Treatment <- c("Control","ML","Vm")
        Evol_data_JU77_P7p_wt$Measure <- c("Evol","Evol","Evol")
        Evol_data_JU77_P7p_wt$Scale <- c("data","data","data")
        Evol_data_JU77_P7p_wt$Variance <- c("Vm","Vm","Vm")
        Evol_data_JU77_P7p_wt
      }
      
      #Summary trait_mean_data_JU77_P7p_wt
      {
        mean_trait_mean_data_JU77_P7p_wt <- rbind(mean(trait_mean_data_JU77_CONTROL_P7p_wt_mod),mean(trait_mean_data_JU77_MA_P7p_wt_mod),mean(trait_mean_data_JU77_Vm_P7p_wt_mod))
        colnames(mean_trait_mean_data_JU77_P7p_wt) <- c("mean")
        median_trait_mean_data_JU77_P7p_wt <- rbind(median(trait_mean_data_JU77_CONTROL_P7p_wt_mod),median(trait_mean_data_JU77_MA_P7p_wt_mod),median(trait_mean_data_JU77_Vm_P7p_wt_mod))
        colnames(median_trait_mean_data_JU77_P7p_wt) <- c("median")
        posterior.mode_trait_mean_data_JU77_P7p_wt <- rbind(posterior.mode(as.mcmc(trait_mean_data_JU77_CONTROL_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_JU77_MA_P7p_wt_mod)),posterior.mode(as.mcmc(trait_mean_data_JU77_Vm_P7p_wt_mod)))
        colnames(posterior.mode_trait_mean_data_JU77_P7p_wt) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_JU77_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU77_CONTROL_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_JU77_MA_P7p_wt_mod)),HPDinterval(as.mcmc(trait_mean_data_JU77_Vm_P7p_wt_mod)))
        colnames(HPDinterval_0.95_trait_mean_data_JU77_P7p_wt) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_JU77_P7p_wt <- rbind(HPDinterval(as.mcmc(trait_mean_data_JU77_CONTROL_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU77_MA_P7p_wt_mod),prob=.83),HPDinterval(as.mcmc(trait_mean_data_JU77_Vm_P7p_wt_mod),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_JU77_P7p_wt) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_JU77_P7p_wt <- rbind(effectiveSize(trait_mean_data_JU77_CONTROL_P7p_wt_mod),effectiveSize(trait_mean_data_JU77_MA_P7p_wt_mod),effectiveSize(trait_mean_data_JU77_Vm_P7p_wt_mod))
        colnames(effectiveSize_trait_mean_data_JU77_P7p_wt) <- c("effectiveSize")
        trait_mean_data_JU77_P7p_wt <- cbind.data.frame(mean_trait_mean_data_JU77_P7p_wt,median_trait_mean_data_JU77_P7p_wt,posterior.mode_trait_mean_data_JU77_P7p_wt,HPDinterval_0.95_trait_mean_data_JU77_P7p_wt,HPDinterval_0.83_trait_mean_data_JU77_P7p_wt,effectiveSize_trait_mean_data_JU77_P7p_wt)
        rownames(trait_mean_data_JU77_P7p_wt) <- c("trait_mean_data_JU77_CONTROL_P7p_wt_mod","trait_mean_data_JU77_MA_P7p_wt_mod","trait_mean_data_JU77_Vm_P7p_wt_mod")
        trait_mean_data_JU77_P7p_wt <- cbind(Models = rownames(trait_mean_data_JU77_P7p_wt),trait_mean_data_JU77_P7p_wt)
        rownames(trait_mean_data_JU77_P7p_wt) <- NULL
        trait_mean_data_JU77_P7p_wt$Pnp <- c("P7.p","P7.p","P7.p")
        trait_mean_data_JU77_P7p_wt$Treatment <- c("Control","ML","Vm")
        trait_mean_data_JU77_P7p_wt$Measure <- c("trait_mean","trait_mean","trait_mean")
        trait_mean_data_JU77_P7p_wt$Scale <- c("data","data","data")
        trait_mean_data_JU77_P7p_wt$Variance <- c("Vm","Vm","Vm")
        trait_mean_data_JU77_P7p_wt
      }
      
      data_JU77_P7p_wt <- rbind.data.frame(va_data_JU77_P7p_wt, h2_data_JU77_P7p_wt,Evol_data_JU77_P7p_wt,trait_mean_data_JU77_P7p_wt)
      data_JU77_P7p_wt
      
    }
    Vm_JU77_P7p_wt <- rbind.data.frame(liab_JU77_P7p_wt, data_JU77_P7p_wt)
    Vm_JU77_P7p_wt$Pnp_fate <- rep("wt", 24)
    Vm_JU77_P7p_wt
    #remove JU77 P7p_wt models
    {
      remove(JU77_CONTROL_P7p_wt_mod)
      remove(JU77_MA_P7p_wt_mod)
      remove(JU77_Vm_P7p_wt_mod)
    }
  }
  
  Vm_JU77_SSSS_Summary <- rbind.data.frame(Vm_JU77_P3p_SSSS,Vm_JU77_P4p_SSSS,Vm_JU77_P5p_wt,Vm_JU77_P6p_wt,Vm_JU77_P7p_wt,Vm_JU77_P8p_SSSS)
  Vm_JU77_SSSS_Summary$Ancestral <- rep("JU77",144)
  Vm_JU77_SSSS_Summary$Species <- rep("O.onirici",144)
  Vm_JU77_SSSS_Summary$Genus <- rep("Oscheius",144)
  View(Vm_JU77_SSSS_Summary)
  
  
  ##Vm_JU77_P3p_divided_P4p_SSSS----
  {
    #Vm_JU77_P3p_divided_P4p_SSSS_liab
    {
      
      va_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod <- va_liab_JU77_Vm_P3p_SSSS_mod / va_liab_JU77_Vm_P4p_SSSS_mod
      h2_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod <- h2_liab_JU77_Vm_P3p_SSSS_mod / h2_liab_JU77_Vm_P4p_SSSS_mod
      Evol_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod <- Evol_liab_JU77_Vm_P3p_SSSS_mod / Evol_liab_JU77_Vm_P4p_SSSS_mod
      
      mean_va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS <- rbind(mean(log10(va_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod)),mean(log10(h2_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod)), mean(log10(Evol_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod)))
      colnames(mean_va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS) <- c("mean")
      median_va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS <- rbind(median(log10(va_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod)),median(log10(h2_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod)), median(log10(Evol_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod)))
      colnames(median_va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS) <- c("median")
      posterior.mode_va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS <- rbind(posterior.mode(as.mcmc(log10(va_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod))),posterior.mode(as.mcmc(log10(h2_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod))),posterior.mode(as.mcmc(log10(Evol_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod))))
      colnames(posterior.mode_va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS) <- c("posterior.mode")
      HPDinterval_0.95_va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod))),HPDinterval(as.mcmc(log10(h2_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod))),HPDinterval(as.mcmc(log10(Evol_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod))))
      colnames(HPDinterval_0.95_va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS) <- c("CI_lower_0.95","CI_upper_0.95")
      HPDinterval_0.83_va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83),HPDinterval(as.mcmc(log10(h2_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83),HPDinterval(as.mcmc(log10(Evol_liab_JU77_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83))
      colnames(HPDinterval_0.83_va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS) <- c("CI_lower_0.83","CI_upper_0.83")
      va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS <- cbind.data.frame(mean_va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS,median_va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS,posterior.mode_va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS,HPDinterval_0.95_va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS,HPDinterval_0.83_va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS)
      rownames(va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS) <- c("va_liab_JU77_Vm_P3p_divided_P4p_SSSS_log10","h2_liab_JU77_Vm_P3p_divided_P4p_SSSS_log10","Evol_liab_JU77_Vm_P3p_divided_P4p_SSSS_log10")
      va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS$Measure <- c("Va","H2", "Evol")
      va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS$Ancestral <- rep("JU77",3)
      va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS$Species <- rep("O.onirici",3)
      va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS$Genus <- rep("Oscheius",3)
      va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS$Scale <- rep("liab",3)
      va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS
      
      pdf("Vm_va_h2_Evol_liab_P3p_divided_P4p_SSSS_log10_JU77.pdf")
      ggplot(va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS, aes(x=Measure, y= median)) +
        geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
        geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
        geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
        theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
        labs(y = "Va log10 ( P3.p / P4.p)", title = "JU77_log10(P3p/P4p)_liab")
      dev.off() 
      
    }
    
    #Vm_JU77_P3p_divided_P4p_SSSS_data
    
    {
      va_data_JU77_Vm_P3p_divided_P4p_SSSS_mod <- va_data_JU77_Vm_P3p_SSSS_mod / va_data_JU77_Vm_P4p_SSSS_mod
      h2_data_JU77_Vm_P3p_divided_P4p_SSSS_mod <- h2_data_JU77_Vm_P3p_SSSS_mod / h2_data_JU77_Vm_P4p_SSSS_mod
      Evol_data_JU77_Vm_P3p_divided_P4p_SSSS_mod <- Evol_data_JU77_Vm_P3p_SSSS_mod / Evol_data_JU77_Vm_P4p_SSSS_mod
      
      mean_va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS <- rbind(mean(log10(va_data_JU77_Vm_P3p_divided_P4p_SSSS_mod)),mean(log10(h2_data_JU77_Vm_P3p_divided_P4p_SSSS_mod)), mean(log10(Evol_data_JU77_Vm_P3p_divided_P4p_SSSS_mod)))
      colnames(mean_va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS) <- c("mean")
      median_va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS <- rbind(median(log10(va_data_JU77_Vm_P3p_divided_P4p_SSSS_mod)),median(log10(h2_data_JU77_Vm_P3p_divided_P4p_SSSS_mod)), median(log10(Evol_data_JU77_Vm_P3p_divided_P4p_SSSS_mod)))
      colnames(median_va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS) <- c("median")
      posterior.mode_va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS <- rbind(posterior.mode(as.mcmc(log10(va_data_JU77_Vm_P3p_divided_P4p_SSSS_mod))),posterior.mode(as.mcmc(log10(h2_data_JU77_Vm_P3p_divided_P4p_SSSS_mod))),posterior.mode(as.mcmc(log10(Evol_data_JU77_Vm_P3p_divided_P4p_SSSS_mod))))
      colnames(posterior.mode_va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS) <- c("posterior.mode")
      HPDinterval_0.95_va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_data_JU77_Vm_P3p_divided_P4p_SSSS_mod))),HPDinterval(as.mcmc(log10(h2_data_JU77_Vm_P3p_divided_P4p_SSSS_mod))),HPDinterval(as.mcmc(log10(Evol_data_JU77_Vm_P3p_divided_P4p_SSSS_mod))))
      colnames(HPDinterval_0.95_va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS) <- c("CI_lower_0.95","CI_upper_0.95")
      HPDinterval_0.83_va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_data_JU77_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83),HPDinterval(as.mcmc(log10(h2_data_JU77_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83),HPDinterval(as.mcmc(log10(Evol_data_JU77_Vm_P3p_divided_P4p_SSSS_mod)),prob=.83))
      colnames(HPDinterval_0.83_va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS) <- c("CI_lower_0.83","CI_upper_0.83")
      va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS <- cbind.data.frame(mean_va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS,median_va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS,posterior.mode_va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS,HPDinterval_0.95_va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS,HPDinterval_0.83_va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS)
      rownames(va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS) <- c("va_data_JU77_Vm_P3p_divided_P4p_SSSS_log10","h2_data_JU77_Vm_P3p_divided_P4p_SSSS_log10","Evol_data_JU77_Vm_P3p_divided_P4p_SSSS_log10")
      va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS$Measure <- c("Va","H2", "Evol")
      va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS$Ancestral <- rep("JU77",3)
      va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS$Species <- rep("O.onirici",3)
      va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS$Genus <- rep("Oscheius",3)
      va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS$Scale <- rep("data",3)
      va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS
      
      pdf("Vm_va_h2_Evol_data_P3p_divided_P4p_SSSS_log10_JU77.pdf")
      ggplot(va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS, aes(x=Measure, y= median)) +
        geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
        geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
        geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
        theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
        labs(y = "Va log10 ( P3.p / P4.p)", title = "JU77_log10(P3p/P4p)_data")
      dev.off() 
      
    }
    
    va_h2_Evol_Vm_JU77_P3p_divided_P4p_SSSS_Summary <- rbind.data.frame(va_h2_Evol_liab_Vm_JU77_P3p_divided_P4p_SSSS,va_h2_Evol_data_Vm_JU77_P3p_divided_P4p_SSSS)
    va_h2_Evol_Vm_JU77_P3p_divided_P4p_SSSS_Summary
    
  }
  
  
}


## ---- Vm_Oscheius_SSSS_Summary ----
Vm_Oscheius_isolate_SSSS_Summary <-  rbind.data.frame(Vm_CEW1_SSSS_Summary,Vm_JU178_SSSS_Summary,Vm_PS2068_SSSS_Summary,Vm_JU77_SSSS_Summary)
View(Vm_Oscheius_isolate_SSSS_Summary)
names(Vm_Oscheius_isolate_SSSS_Summary)[names(Vm_Oscheius_isolate_SSSS_Summary) == "Models"] <- "Model_name"
names(Vm_Oscheius_isolate_SSSS_Summary)[names(Vm_Oscheius_isolate_SSSS_Summary) == "Treatment"] <- "Model_Set"

write_xlsx(Vm_Oscheius_isolate_SSSS_Summary, "Vm_Oscheius_isolate_SSSS_Summary.xlsx")

Vm_Oscheius_isolate_SSSS_Summary_Vm <- subset(Vm_Oscheius_isolate_SSSS_Summary, Model_Set=="Vm")
View(Vm_Oscheius_isolate_SSSS_Summary_Vm)
write_xlsx(Vm_Oscheius_isolate_SSSS_Summary_Vm, "Vm_Oscheius_isolate_SSSS_Summary_Vm.xlsx")


## ---- Vm_Oscheius_P3p_divided_P4p_SSSS ----
va_h2_Evol_Vm_Oscheius_isolate_P3p_divided_P4p_SSSS_Summary <-  rbind.data.frame(va_h2_Evol_Vm_CEW1_P3p_divided_P4p_SSSS_Summary,va_h2_Evol_Vm_JU178_P3p_divided_P4p_SSSS_Summary,va_h2_Evol_Vm_PS2068_P3p_divided_P4p_SSSS_Summary,va_h2_Evol_Vm_JU77_P3p_divided_P4p_SSSS_Summary)
write_xlsx(va_h2_Evol_Vm_Oscheius_isolate_P3p_divided_P4p_SSSS_Summary, "va_h2_Evol_Vm_Oscheius_isolate_log10_P3p_divided_P4p_SSSS_Summary.xlsx")


pdf("va_h2_Evol_Vm_Oscheius_P3p_divided_P4p_SSSS_Summary.pdf")
ggplot(va_h2_Evol_Vm_Oscheius_isolate_P3p_divided_P4p_SSSS_Summary, aes(x=Ancestral, y= median)) +
  geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
  geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
  geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
  theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) +facet_grid(Scale~Measure) + theme(aspect.ratio=1) + labs(y = "Va log10 ( P3.p / P4.p)") 
dev.off()


##NB  Total separation in CEW1_CONTROL_P3p_SSSS
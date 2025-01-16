
#NB: Vr=Va/(2*81.58) - Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance & divided by the total length of the tree to show the increase in Variance per Myr

#----Vr_Oscheius_summary_SSSS----
{
  
  #Summary Vr_Oscheius
  {
    ----##Summary liability scale Vr_Oscheius----
    {
      #Summary va_liab_Vr_Oscheius
      {
        
        mean_va_liab_Vr_Oscheius <- rbind(mean(va_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58)),mean(va_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58)),mean(va_liab_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58)),mean(va_liab_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58)),mean(va_liab_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58)),mean(va_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58)))
        colnames(mean_va_liab_Vr_Oscheius) <- c("mean")
        median_va_liab_Vr_Oscheius <- rbind(median(va_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58)),median(va_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58)),median(va_liab_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58)),median(va_liab_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58)),median(va_liab_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58)),median(va_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58)))
        colnames(median_va_liab_Vr_Oscheius) <- c("median")
        posterior.mode_va_liab_Vr_Oscheius <- rbind(posterior.mode(va_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58)),posterior.mode(va_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58)),posterior.mode(va_liab_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58)),posterior.mode(va_liab_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58)),posterior.mode(va_liab_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58)),posterior.mode(va_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58)))
        colnames(posterior.mode_va_liab_Vr_Oscheius) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Vr_Oscheius <- rbind(HPDinterval(va_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58)),HPDinterval(va_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58)),HPDinterval(va_liab_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58)),HPDinterval(va_liab_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58)),HPDinterval(va_liab_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58)),HPDinterval(va_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58)))
        colnames(HPDinterval_0.95_va_liab_Vr_Oscheius) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Vr_Oscheius <- rbind(HPDinterval(va_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58),prob=.83),HPDinterval(va_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58),prob=.83),HPDinterval(va_liab_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58),prob=.83),HPDinterval(va_liab_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58),prob=.83),HPDinterval(va_liab_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58),prob=.83),HPDinterval(va_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58),prob=.83))
        colnames(HPDinterval_0.83_va_liab_Vr_Oscheius) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Vr_Oscheius <- rbind(effectiveSize(va_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled),effectiveSize(va_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled),effectiveSize(va_liab_Vr_Oscheius_P5p_wt_mod3_scaled),effectiveSize(va_liab_Vr_Oscheius_P6p_wt_mod3_scaled),effectiveSize(va_liab_Vr_Oscheius_P7p_wt_mod3_scaled),effectiveSize(va_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(effectiveSize_va_liab_Vr_Oscheius) <- c("effectiveSize")
        va_liab_Vr_Oscheius <- cbind.data.frame(mean_va_liab_Vr_Oscheius,median_va_liab_Vr_Oscheius,posterior.mode_va_liab_Vr_Oscheius,HPDinterval_0.95_va_liab_Vr_Oscheius,HPDinterval_0.83_va_liab_Vr_Oscheius,effectiveSize_va_liab_Vr_Oscheius)
        rownames(va_liab_Vr_Oscheius) <- c("va_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled","va_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled","va_liab_Vr_Oscheius_P5p_wt_mod3_scaled","va_liab_Vr_Oscheius_P6p_wt_mod3_scaled","va_liab_Vr_Oscheius_P7p_wt_mod3_scaled","va_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled")
        va_liab_Vr_Oscheius <- cbind(Models = rownames(va_liab_Vr_Oscheius),va_liab_Vr_Oscheius)
        rownames(va_liab_Vr_Oscheius) <- NULL
        va_liab_Vr_Oscheius$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        va_liab_Vr_Oscheius$Pnp_fate <- c("SSSS","SSSS","wt","wt","wt","SSSS")
        va_liab_Vr_Oscheius$Measure <- c("Va_Phylo","Va_Phylo","Va_Phylo","Va_Phylo","Va_Phylo","Va_Phylo")
        va_liab_Vr_Oscheius$Scale <- c("liab","liab","liab","liab","liab","liab")
        va_liab_Vr_Oscheius$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        va_liab_Vr_Oscheius
      }
      
      #Summary h2_liab_Vr_Oscheius
      {
        
        mean_H2_liab_Vr_Oscheius <- rbind(mean(h2_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled),mean(h2_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled),mean(h2_liab_Vr_Oscheius_P5p_wt_mod3_scaled),mean(h2_liab_Vr_Oscheius_P6p_wt_mod3_scaled),mean(h2_liab_Vr_Oscheius_P7p_wt_mod3_scaled),mean(h2_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(mean_H2_liab_Vr_Oscheius) <- c("mean")
        median_H2_liab_Vr_Oscheius <- rbind(median(h2_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled),median(h2_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled),median(h2_liab_Vr_Oscheius_P5p_wt_mod3_scaled),median(h2_liab_Vr_Oscheius_P6p_wt_mod3_scaled),median(h2_liab_Vr_Oscheius_P7p_wt_mod3_scaled),median(h2_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(median_H2_liab_Vr_Oscheius) <- c("median")
        posterior.mode_H2_liab_Vr_Oscheius <- rbind(posterior.mode(h2_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled),posterior.mode(h2_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled),posterior.mode(h2_liab_Vr_Oscheius_P5p_wt_mod3_scaled),posterior.mode(h2_liab_Vr_Oscheius_P6p_wt_mod3_scaled),posterior.mode(h2_liab_Vr_Oscheius_P7p_wt_mod3_scaled),posterior.mode(h2_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(posterior.mode_H2_liab_Vr_Oscheius) <- c("posterior.mode")
        HPDinterval_0.95_H2_liab_Vr_Oscheius <- rbind(HPDinterval(h2_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled),HPDinterval(h2_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled),HPDinterval(h2_liab_Vr_Oscheius_P5p_wt_mod3_scaled),HPDinterval(h2_liab_Vr_Oscheius_P6p_wt_mod3_scaled),HPDinterval(h2_liab_Vr_Oscheius_P7p_wt_mod3_scaled),HPDinterval(h2_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(HPDinterval_0.95_H2_liab_Vr_Oscheius) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_H2_liab_Vr_Oscheius <- rbind(HPDinterval(h2_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled,prob=.83),HPDinterval(h2_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled,prob=.83),HPDinterval(h2_liab_Vr_Oscheius_P5p_wt_mod3_scaled,prob=.83),HPDinterval(h2_liab_Vr_Oscheius_P6p_wt_mod3_scaled,prob=.83),HPDinterval(h2_liab_Vr_Oscheius_P7p_wt_mod3_scaled,prob=.83),HPDinterval(h2_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled,prob=.83))
        colnames(HPDinterval_0.83_H2_liab_Vr_Oscheius) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_H2_liab_Vr_Oscheius <- rbind(effectiveSize(h2_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled),effectiveSize(h2_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled),effectiveSize(h2_liab_Vr_Oscheius_P5p_wt_mod3_scaled),effectiveSize(h2_liab_Vr_Oscheius_P6p_wt_mod3_scaled),effectiveSize(h2_liab_Vr_Oscheius_P7p_wt_mod3_scaled),effectiveSize(h2_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(effectiveSize_H2_liab_Vr_Oscheius) <- c("effectiveSize")
        H2_liab_Vr_Oscheius <- cbind.data.frame(mean_H2_liab_Vr_Oscheius,median_H2_liab_Vr_Oscheius,posterior.mode_H2_liab_Vr_Oscheius,HPDinterval_0.95_H2_liab_Vr_Oscheius,HPDinterval_0.83_H2_liab_Vr_Oscheius,effectiveSize_H2_liab_Vr_Oscheius)
        rownames(H2_liab_Vr_Oscheius) <- c("h2_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled","h2_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled","h2_liab_Vr_Oscheius_P5p_wt_mod3_scaled","h2_liab_Vr_Oscheius_P6p_wt_mod3_scaled","h2_liab_Vr_Oscheius_P7p_wt_mod3_scaled","h2_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled")
        H2_liab_Vr_Oscheius <- cbind(Models = rownames(H2_liab_Vr_Oscheius),H2_liab_Vr_Oscheius)
        rownames(H2_liab_Vr_Oscheius) <- NULL
        H2_liab_Vr_Oscheius$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        H2_liab_Vr_Oscheius$Pnp_fate <- c("SSSS","SSSS","wt","wt","wt","SSSS")
        H2_liab_Vr_Oscheius$Measure <- c("H2","H2","H2","H2","H2","H2")
        H2_liab_Vr_Oscheius$Scale <- c("liab","liab","liab","liab","liab","liab")
        H2_liab_Vr_Oscheius$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        H2_liab_Vr_Oscheius
      }
      
      #Summary Evol_liab_Vr_Oscheius
      {
        
        mean_Evol_liab_Vr_Oscheius <- rbind(mean(Evol_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled),mean(Evol_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled),mean(Evol_liab_Vr_Oscheius_P5p_wt_mod3_scaled),mean(Evol_liab_Vr_Oscheius_P6p_wt_mod3_scaled),mean(Evol_liab_Vr_Oscheius_P7p_wt_mod3_scaled),mean(Evol_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(mean_Evol_liab_Vr_Oscheius) <- c("mean")
        median_Evol_liab_Vr_Oscheius <- rbind(median(Evol_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled),median(Evol_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled),median(Evol_liab_Vr_Oscheius_P5p_wt_mod3_scaled),median(Evol_liab_Vr_Oscheius_P6p_wt_mod3_scaled),median(Evol_liab_Vr_Oscheius_P7p_wt_mod3_scaled),median(Evol_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(median_Evol_liab_Vr_Oscheius) <- c("median")
        posterior.mode_Evol_liab_Vr_Oscheius <- rbind(posterior.mode(Evol_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled),posterior.mode(Evol_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled),posterior.mode(Evol_liab_Vr_Oscheius_P5p_wt_mod3_scaled),posterior.mode(Evol_liab_Vr_Oscheius_P6p_wt_mod3_scaled),posterior.mode(Evol_liab_Vr_Oscheius_P7p_wt_mod3_scaled),posterior.mode(Evol_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(posterior.mode_Evol_liab_Vr_Oscheius) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Vr_Oscheius <- rbind(HPDinterval(Evol_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled),HPDinterval(Evol_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled),HPDinterval(Evol_liab_Vr_Oscheius_P5p_wt_mod3_scaled),HPDinterval(Evol_liab_Vr_Oscheius_P6p_wt_mod3_scaled),HPDinterval(Evol_liab_Vr_Oscheius_P7p_wt_mod3_scaled),HPDinterval(Evol_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(HPDinterval_0.95_Evol_liab_Vr_Oscheius) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Vr_Oscheius <- rbind(HPDinterval(Evol_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled,prob=.83),HPDinterval(Evol_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled,prob=.83),HPDinterval(Evol_liab_Vr_Oscheius_P5p_wt_mod3_scaled,prob=.83),HPDinterval(Evol_liab_Vr_Oscheius_P6p_wt_mod3_scaled,prob=.83),HPDinterval(Evol_liab_Vr_Oscheius_P7p_wt_mod3_scaled,prob=.83),HPDinterval(Evol_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Vr_Oscheius) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Vr_Oscheius <- rbind(effectiveSize(Evol_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled),effectiveSize(Evol_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled),effectiveSize(Evol_liab_Vr_Oscheius_P5p_wt_mod3_scaled),effectiveSize(Evol_liab_Vr_Oscheius_P6p_wt_mod3_scaled),effectiveSize(Evol_liab_Vr_Oscheius_P7p_wt_mod3_scaled),effectiveSize(Evol_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(effectiveSize_Evol_liab_Vr_Oscheius) <- c("effectiveSize")
        Evol_liab_Vr_Oscheius <- cbind.data.frame(mean_Evol_liab_Vr_Oscheius,median_Evol_liab_Vr_Oscheius,posterior.mode_Evol_liab_Vr_Oscheius,HPDinterval_0.95_Evol_liab_Vr_Oscheius,HPDinterval_0.83_Evol_liab_Vr_Oscheius,effectiveSize_Evol_liab_Vr_Oscheius)
        rownames(Evol_liab_Vr_Oscheius) <- c("Evol_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled","Evol_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled","Evol_liab_Vr_Oscheius_P5p_wt_mod3_scaled","Evol_liab_Vr_Oscheius_P6p_wt_mod3_scaled","Evol_liab_Vr_Oscheius_P7p_wt_mod3_scaled","Evol_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled")
        Evol_liab_Vr_Oscheius <- cbind(Models = rownames(Evol_liab_Vr_Oscheius),Evol_liab_Vr_Oscheius)
        rownames(Evol_liab_Vr_Oscheius) <- NULL
        Evol_liab_Vr_Oscheius$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        Evol_liab_Vr_Oscheius$Pnp_fate <- c("SSSS","SSSS","wt","wt","wt","SSSS")
        Evol_liab_Vr_Oscheius$Measure <- c("Evol","Evol","Evol","Evol","Evol","Evol")
        Evol_liab_Vr_Oscheius$Scale <- c("liab","liab","liab","liab","liab","liab")
        Evol_liab_Vr_Oscheius$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        Evol_liab_Vr_Oscheius
      }
      
      #Summary va_NotPhylo_liab_Vr_Oscheius
      {
        
        mean_va_NotPhylo_liab_Vr_Oscheius <- rbind(mean(va_NotPhylo_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58)),mean(va_NotPhylo_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58)),mean(va_NotPhylo_liab_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58)),mean(va_NotPhylo_liab_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58)),mean(va_NotPhylo_liab_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58)),mean(va_NotPhylo_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58)))
        colnames(mean_va_NotPhylo_liab_Vr_Oscheius) <- c("mean")
        median_va_NotPhylo_liab_Vr_Oscheius <- rbind(median(va_NotPhylo_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58)),median(va_NotPhylo_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58)),median(va_NotPhylo_liab_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58)),median(va_NotPhylo_liab_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58)),median(va_NotPhylo_liab_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58)),median(va_NotPhylo_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58)))
        colnames(median_va_NotPhylo_liab_Vr_Oscheius) <- c("median")
        posterior.mode_va_NotPhylo_liab_Vr_Oscheius <- rbind(posterior.mode(va_NotPhylo_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58)),posterior.mode(va_NotPhylo_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58)),posterior.mode(va_NotPhylo_liab_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58)),posterior.mode(va_NotPhylo_liab_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58)),posterior.mode(va_NotPhylo_liab_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58)),posterior.mode(va_NotPhylo_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58)))
        colnames(posterior.mode_va_NotPhylo_liab_Vr_Oscheius) <- c("posterior.mode")
        HPDinterval_0.95_va_NotPhylo_liab_Vr_Oscheius <- rbind(HPDinterval(va_NotPhylo_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58)),HPDinterval(va_NotPhylo_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58)),HPDinterval(va_NotPhylo_liab_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58)),HPDinterval(va_NotPhylo_liab_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58)),HPDinterval(va_NotPhylo_liab_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58)),HPDinterval(va_NotPhylo_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58)))
        colnames(HPDinterval_0.95_va_NotPhylo_liab_Vr_Oscheius) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_NotPhylo_liab_Vr_Oscheius <- rbind(HPDinterval(va_NotPhylo_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58),prob=.83),HPDinterval(va_NotPhylo_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58),prob=.83),HPDinterval(va_NotPhylo_liab_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58),prob=.83),HPDinterval(va_NotPhylo_liab_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58),prob=.83),HPDinterval(va_NotPhylo_liab_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58),prob=.83),HPDinterval(va_NotPhylo_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58),prob=.83))
        colnames(HPDinterval_0.83_va_NotPhylo_liab_Vr_Oscheius) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_NotPhylo_liab_Vr_Oscheius <- rbind(effectiveSize(va_NotPhylo_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled),effectiveSize(va_NotPhylo_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled),effectiveSize(va_NotPhylo_liab_Vr_Oscheius_P5p_wt_mod3_scaled),effectiveSize(va_NotPhylo_liab_Vr_Oscheius_P6p_wt_mod3_scaled),effectiveSize(va_NotPhylo_liab_Vr_Oscheius_P7p_wt_mod3_scaled),effectiveSize(va_NotPhylo_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(effectiveSize_va_NotPhylo_liab_Vr_Oscheius) <- c("effectiveSize")
        va_NotPhylo_liab_Vr_Oscheius <- cbind.data.frame(mean_va_NotPhylo_liab_Vr_Oscheius,median_va_NotPhylo_liab_Vr_Oscheius,posterior.mode_va_NotPhylo_liab_Vr_Oscheius,HPDinterval_0.95_va_NotPhylo_liab_Vr_Oscheius,HPDinterval_0.83_va_NotPhylo_liab_Vr_Oscheius,effectiveSize_va_NotPhylo_liab_Vr_Oscheius)
        rownames(va_NotPhylo_liab_Vr_Oscheius) <- c("va_NotPhylo_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled","va_NotPhylo_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled","va_NotPhylo_liab_Vr_Oscheius_P5p_wt_mod3_scaled","va_NotPhylo_liab_Vr_Oscheius_P6p_wt_mod3_scaled","va_NotPhylo_liab_Vr_Oscheius_P7p_wt_mod3_scaled","va_NotPhylo_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled")
        va_NotPhylo_liab_Vr_Oscheius <- cbind(Models = rownames(va_NotPhylo_liab_Vr_Oscheius),va_NotPhylo_liab_Vr_Oscheius)
        rownames(va_NotPhylo_liab_Vr_Oscheius) <- NULL
        va_NotPhylo_liab_Vr_Oscheius$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        va_NotPhylo_liab_Vr_Oscheius$Pnp_fate <- c("SSSS","SSSS","wt","wt","wt","SSSS")
        va_NotPhylo_liab_Vr_Oscheius$Measure <- c("va_NotPhylo","va_NotPhylo","va_NotPhylo","va_NotPhylo","va_NotPhylo","va_NotPhylo")
        va_NotPhylo_liab_Vr_Oscheius$Scale <- c("liab","liab","liab","liab","liab","liab")
        va_NotPhylo_liab_Vr_Oscheius$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        va_NotPhylo_liab_Vr_Oscheius
      }
      
      #Summary trait_mean_liab_Vr_Oscheius
      {
        
        mean_trait_mean_liab_Vr_Oscheius <- rbind(mean(trait_mean_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled),mean(trait_mean_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled),mean(trait_mean_liab_Vr_Oscheius_P5p_wt_mod3_scaled),mean(trait_mean_liab_Vr_Oscheius_P6p_wt_mod3_scaled),mean(trait_mean_liab_Vr_Oscheius_P7p_wt_mod3_scaled),mean(trait_mean_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(mean_trait_mean_liab_Vr_Oscheius) <- c("mean")
        median_trait_mean_liab_Vr_Oscheius <- rbind(median(trait_mean_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled),median(trait_mean_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled),median(trait_mean_liab_Vr_Oscheius_P5p_wt_mod3_scaled),median(trait_mean_liab_Vr_Oscheius_P6p_wt_mod3_scaled),median(trait_mean_liab_Vr_Oscheius_P7p_wt_mod3_scaled),median(trait_mean_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(median_trait_mean_liab_Vr_Oscheius) <- c("median")
        posterior.mode_trait_mean_liab_Vr_Oscheius <- rbind(posterior.mode(trait_mean_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled),posterior.mode(trait_mean_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled),posterior.mode(trait_mean_liab_Vr_Oscheius_P5p_wt_mod3_scaled),posterior.mode(trait_mean_liab_Vr_Oscheius_P6p_wt_mod3_scaled),posterior.mode(trait_mean_liab_Vr_Oscheius_P7p_wt_mod3_scaled),posterior.mode(trait_mean_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(posterior.mode_trait_mean_liab_Vr_Oscheius) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Vr_Oscheius <- rbind(HPDinterval(trait_mean_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled),HPDinterval(trait_mean_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled),HPDinterval(trait_mean_liab_Vr_Oscheius_P5p_wt_mod3_scaled),HPDinterval(trait_mean_liab_Vr_Oscheius_P6p_wt_mod3_scaled),HPDinterval(trait_mean_liab_Vr_Oscheius_P7p_wt_mod3_scaled),HPDinterval(trait_mean_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(HPDinterval_0.95_trait_mean_liab_Vr_Oscheius) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Vr_Oscheius <- rbind(HPDinterval(trait_mean_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled,prob=.83),HPDinterval(trait_mean_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled,prob=.83),HPDinterval(trait_mean_liab_Vr_Oscheius_P5p_wt_mod3_scaled,prob=.83),HPDinterval(trait_mean_liab_Vr_Oscheius_P6p_wt_mod3_scaled,prob=.83),HPDinterval(trait_mean_liab_Vr_Oscheius_P7p_wt_mod3_scaled,prob=.83),HPDinterval(trait_mean_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled,prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Vr_Oscheius) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Vr_Oscheius <- rbind(effectiveSize(trait_mean_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled),effectiveSize(trait_mean_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled),effectiveSize(trait_mean_liab_Vr_Oscheius_P5p_wt_mod3_scaled),effectiveSize(trait_mean_liab_Vr_Oscheius_P6p_wt_mod3_scaled),effectiveSize(trait_mean_liab_Vr_Oscheius_P7p_wt_mod3_scaled),effectiveSize(trait_mean_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(effectiveSize_trait_mean_liab_Vr_Oscheius) <- c("effectiveSize")
        trait_mean_liab_Vr_Oscheius <- cbind.data.frame(mean_trait_mean_liab_Vr_Oscheius,median_trait_mean_liab_Vr_Oscheius,posterior.mode_trait_mean_liab_Vr_Oscheius,HPDinterval_0.95_trait_mean_liab_Vr_Oscheius,HPDinterval_0.83_trait_mean_liab_Vr_Oscheius,effectiveSize_trait_mean_liab_Vr_Oscheius)
        rownames(trait_mean_liab_Vr_Oscheius) <- c("trait_mean_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled","trait_mean_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled","trait_mean_liab_Vr_Oscheius_P5p_wt_mod3_scaled","trait_mean_liab_Vr_Oscheius_P6p_wt_mod3_scaled","trait_mean_liab_Vr_Oscheius_P7p_wt_mod3_scaled","trait_mean_liab_Vr_Oscheius_P8p_SSSS_mod3_scaled")
        trait_mean_liab_Vr_Oscheius <- cbind(Models = rownames(trait_mean_liab_Vr_Oscheius),trait_mean_liab_Vr_Oscheius)
        rownames(trait_mean_liab_Vr_Oscheius) <- NULL
        trait_mean_liab_Vr_Oscheius$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        trait_mean_liab_Vr_Oscheius$Pnp_fate <- c("SSSS","SSSS","wt","wt","wt","SSSS")
        trait_mean_liab_Vr_Oscheius$Measure <- c("trait_mean","trait_mean","trait_mean","trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Vr_Oscheius$Scale <- c("liab","liab","liab","liab","liab","liab")
        trait_mean_liab_Vr_Oscheius$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        trait_mean_liab_Vr_Oscheius
      }
      
      liab_Vr_Oscheius <- rbind.data.frame(va_liab_Vr_Oscheius, H2_liab_Vr_Oscheius,Evol_liab_Vr_Oscheius,va_NotPhylo_liab_Vr_Oscheius,trait_mean_liab_Vr_Oscheius)
      liab_Vr_Oscheius
    }
    
    
    
    ----##Summary data scale Vr_Oscheius----
    {
      #Summary va_data_Vr_Oscheius
      {
        
        mean_va_data_Vr_Oscheius <- rbind(mean(va_data_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58)),mean(va_data_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58)),mean(va_data_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58)),mean(va_data_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58)),mean(va_data_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58)),mean(va_data_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58)))
        colnames(mean_va_data_Vr_Oscheius) <- c("mean")
        median_va_data_Vr_Oscheius <- rbind(median(va_data_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58)),median(va_data_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58)),median(va_data_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58)),median(va_data_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58)),median(va_data_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58)),median(va_data_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58)))
        colnames(median_va_data_Vr_Oscheius) <- c("median")
        posterior.mode_va_data_Vr_Oscheius <- rbind(posterior.mode(as.mcmc(va_data_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58))),posterior.mode(as.mcmc(va_data_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58))),posterior.mode(as.mcmc(va_data_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58))),posterior.mode(as.mcmc(va_data_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58))),posterior.mode(as.mcmc(va_data_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58))),posterior.mode(as.mcmc(va_data_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58))))
        colnames(posterior.mode_va_data_Vr_Oscheius) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Vr_Oscheius <- rbind(HPDinterval(as.mcmc(va_data_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58))),HPDinterval(as.mcmc(va_data_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58))),HPDinterval(as.mcmc(va_data_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58))),HPDinterval(as.mcmc(va_data_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58))),HPDinterval(as.mcmc(va_data_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58))),HPDinterval(as.mcmc(va_data_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58))))
        colnames(HPDinterval_0.95_va_data_Vr_Oscheius) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Vr_Oscheius <- rbind(HPDinterval(as.mcmc(va_data_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58)),prob=.83),HPDinterval(as.mcmc(va_data_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58)),prob=.83),HPDinterval(as.mcmc(va_data_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58)),prob=.83),HPDinterval(as.mcmc(va_data_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58)),prob=.83),HPDinterval(as.mcmc(va_data_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58)),prob=.83),HPDinterval(as.mcmc(va_data_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58)),prob=.83))
        colnames(HPDinterval_0.83_va_data_Vr_Oscheius) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Vr_Oscheius <- rbind(effectiveSize(va_data_Vr_Oscheius_P3p_SSSS_mod3_scaled),effectiveSize(va_data_Vr_Oscheius_P4p_SSSS_mod3_scaled),effectiveSize(va_data_Vr_Oscheius_P5p_wt_mod3_scaled),effectiveSize(va_data_Vr_Oscheius_P6p_wt_mod3_scaled),effectiveSize(va_data_Vr_Oscheius_P7p_wt_mod3_scaled),effectiveSize(va_data_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(effectiveSize_va_data_Vr_Oscheius) <- c("effectiveSize")
        va_data_Vr_Oscheius <- cbind.data.frame(mean_va_data_Vr_Oscheius,median_va_data_Vr_Oscheius,posterior.mode_va_data_Vr_Oscheius,HPDinterval_0.95_va_data_Vr_Oscheius,HPDinterval_0.83_va_data_Vr_Oscheius,effectiveSize_va_data_Vr_Oscheius)
        rownames(va_data_Vr_Oscheius) <- c("va_data_Vr_Oscheius_P3p_SSSS_mod3_scaled","va_data_Vr_Oscheius_P4p_SSSS_mod3_scaled","va_data_Vr_Oscheius_P5p_wt_mod3_scaled","va_data_Vr_Oscheius_P6p_wt_mod3_scaled","va_data_Vr_Oscheius_P7p_wt_mod3_scaled","va_data_Vr_Oscheius_P8p_SSSS_mod3_scaled")
        va_data_Vr_Oscheius <- cbind(Models = rownames(va_data_Vr_Oscheius),va_data_Vr_Oscheius)
        rownames(va_data_Vr_Oscheius) <- NULL
        va_data_Vr_Oscheius$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        va_data_Vr_Oscheius$Pnp_fate <- c("SSSS","SSSS","wt","wt","wt","SSSS")
        va_data_Vr_Oscheius$Measure <- c("Va_Phylo","Va_Phylo","Va_Phylo","Va_Phylo","Va_Phylo","Va_Phylo")
        va_data_Vr_Oscheius$Scale <- c("data","data","data","data","data","data")
        va_data_Vr_Oscheius$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        va_data_Vr_Oscheius
      }
      
      #Summary h2_data_Vr_Oscheius
      {
        
        mean_H2_data_Vr_Oscheius <- rbind(mean(h2_data_Vr_Oscheius_P3p_SSSS_mod3_scaled),mean(h2_data_Vr_Oscheius_P4p_SSSS_mod3_scaled),mean(h2_data_Vr_Oscheius_P5p_wt_mod3_scaled),mean(h2_data_Vr_Oscheius_P6p_wt_mod3_scaled),mean(h2_data_Vr_Oscheius_P7p_wt_mod3_scaled),mean(h2_data_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(mean_H2_data_Vr_Oscheius) <- c("mean")
        median_H2_data_Vr_Oscheius <- rbind(median(h2_data_Vr_Oscheius_P3p_SSSS_mod3_scaled),median(h2_data_Vr_Oscheius_P4p_SSSS_mod3_scaled),median(h2_data_Vr_Oscheius_P5p_wt_mod3_scaled),median(h2_data_Vr_Oscheius_P6p_wt_mod3_scaled),median(h2_data_Vr_Oscheius_P7p_wt_mod3_scaled),median(h2_data_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(median_H2_data_Vr_Oscheius) <- c("median")
        posterior.mode_H2_data_Vr_Oscheius <- rbind(posterior.mode(as.mcmc(h2_data_Vr_Oscheius_P3p_SSSS_mod3_scaled)),posterior.mode(as.mcmc(h2_data_Vr_Oscheius_P4p_SSSS_mod3_scaled)),posterior.mode(as.mcmc(h2_data_Vr_Oscheius_P5p_wt_mod3_scaled)),posterior.mode(as.mcmc(h2_data_Vr_Oscheius_P6p_wt_mod3_scaled)),posterior.mode(as.mcmc(h2_data_Vr_Oscheius_P7p_wt_mod3_scaled)),posterior.mode(as.mcmc(h2_data_Vr_Oscheius_P8p_SSSS_mod3_scaled)))
        colnames(posterior.mode_H2_data_Vr_Oscheius) <- c("posterior.mode")
        HPDinterval_0.95_H2_data_Vr_Oscheius <- rbind(HPDinterval(as.mcmc(h2_data_Vr_Oscheius_P3p_SSSS_mod3_scaled)),HPDinterval(as.mcmc(h2_data_Vr_Oscheius_P4p_SSSS_mod3_scaled)),HPDinterval(as.mcmc(h2_data_Vr_Oscheius_P5p_wt_mod3_scaled)),HPDinterval(as.mcmc(h2_data_Vr_Oscheius_P6p_wt_mod3_scaled)),HPDinterval(as.mcmc(h2_data_Vr_Oscheius_P7p_wt_mod3_scaled)),HPDinterval(as.mcmc(h2_data_Vr_Oscheius_P8p_SSSS_mod3_scaled)))
        colnames(HPDinterval_0.95_H2_data_Vr_Oscheius) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_H2_data_Vr_Oscheius <- rbind(HPDinterval(as.mcmc(h2_data_Vr_Oscheius_P3p_SSSS_mod3_scaled),prob=.83),HPDinterval(as.mcmc(h2_data_Vr_Oscheius_P4p_SSSS_mod3_scaled),prob=.83),HPDinterval(as.mcmc(h2_data_Vr_Oscheius_P5p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(h2_data_Vr_Oscheius_P6p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(h2_data_Vr_Oscheius_P7p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(h2_data_Vr_Oscheius_P8p_SSSS_mod3_scaled),prob=.83))
        colnames(HPDinterval_0.83_H2_data_Vr_Oscheius) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_H2_data_Vr_Oscheius <- rbind(effectiveSize(h2_data_Vr_Oscheius_P3p_SSSS_mod3_scaled),effectiveSize(h2_data_Vr_Oscheius_P4p_SSSS_mod3_scaled),effectiveSize(h2_data_Vr_Oscheius_P5p_wt_mod3_scaled),effectiveSize(h2_data_Vr_Oscheius_P6p_wt_mod3_scaled),effectiveSize(h2_data_Vr_Oscheius_P7p_wt_mod3_scaled),effectiveSize(h2_data_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(effectiveSize_H2_data_Vr_Oscheius) <- c("effectiveSize")
        H2_data_Vr_Oscheius <- cbind.data.frame(mean_H2_data_Vr_Oscheius,median_H2_data_Vr_Oscheius,posterior.mode_H2_data_Vr_Oscheius,HPDinterval_0.95_H2_data_Vr_Oscheius,HPDinterval_0.83_H2_data_Vr_Oscheius,effectiveSize_H2_data_Vr_Oscheius)
        rownames(H2_data_Vr_Oscheius) <- c("h2_data_Vr_Oscheius_P3p_SSSS_mod3_scaled","h2_data_Vr_Oscheius_P4p_SSSS_mod3_scaled","h2_data_Vr_Oscheius_P5p_wt_mod3_scaled","h2_data_Vr_Oscheius_P6p_wt_mod3_scaled","h2_data_Vr_Oscheius_P7p_wt_mod3_scaled","h2_data_Vr_Oscheius_P8p_SSSS_mod3_scaled")
        H2_data_Vr_Oscheius <- cbind(Models = rownames(H2_data_Vr_Oscheius),H2_data_Vr_Oscheius)
        rownames(H2_data_Vr_Oscheius) <- NULL
        H2_data_Vr_Oscheius$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        H2_data_Vr_Oscheius$Pnp_fate <- c("SSSS","SSSS","wt","wt","wt","SSSS")
        H2_data_Vr_Oscheius$Measure <- c("H2","H2","H2","H2","H2","H2")
        H2_data_Vr_Oscheius$Scale <- c("data","data","data","data","data","data")
        H2_data_Vr_Oscheius$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        H2_data_Vr_Oscheius
      }
      
      #Summary Evol_data_Vr_Oscheius
      {
        
        mean_Evol_data_Vr_Oscheius <- rbind(mean(Evol_data_Vr_Oscheius_P3p_SSSS_mod3_scaled),mean(Evol_data_Vr_Oscheius_P4p_SSSS_mod3_scaled),mean(Evol_data_Vr_Oscheius_P5p_wt_mod3_scaled),mean(Evol_data_Vr_Oscheius_P6p_wt_mod3_scaled),mean(Evol_data_Vr_Oscheius_P7p_wt_mod3_scaled),mean(Evol_data_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(mean_Evol_data_Vr_Oscheius) <- c("mean")
        median_Evol_data_Vr_Oscheius <- rbind(median(Evol_data_Vr_Oscheius_P3p_SSSS_mod3_scaled),median(Evol_data_Vr_Oscheius_P4p_SSSS_mod3_scaled),median(Evol_data_Vr_Oscheius_P5p_wt_mod3_scaled),median(Evol_data_Vr_Oscheius_P6p_wt_mod3_scaled),median(Evol_data_Vr_Oscheius_P7p_wt_mod3_scaled),median(Evol_data_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(median_Evol_data_Vr_Oscheius) <- c("median")
        posterior.mode_Evol_data_Vr_Oscheius <- rbind(posterior.mode(as.mcmc(Evol_data_Vr_Oscheius_P3p_SSSS_mod3_scaled)),posterior.mode(as.mcmc(Evol_data_Vr_Oscheius_P4p_SSSS_mod3_scaled)),posterior.mode(as.mcmc(Evol_data_Vr_Oscheius_P5p_wt_mod3_scaled)),posterior.mode(as.mcmc(Evol_data_Vr_Oscheius_P6p_wt_mod3_scaled)),posterior.mode(as.mcmc(Evol_data_Vr_Oscheius_P7p_wt_mod3_scaled)),posterior.mode(as.mcmc(Evol_data_Vr_Oscheius_P8p_SSSS_mod3_scaled)))
        colnames(posterior.mode_Evol_data_Vr_Oscheius) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Vr_Oscheius <- rbind(HPDinterval(as.mcmc(Evol_data_Vr_Oscheius_P3p_SSSS_mod3_scaled)),HPDinterval(as.mcmc(Evol_data_Vr_Oscheius_P4p_SSSS_mod3_scaled)),HPDinterval(as.mcmc(Evol_data_Vr_Oscheius_P5p_wt_mod3_scaled)),HPDinterval(as.mcmc(Evol_data_Vr_Oscheius_P6p_wt_mod3_scaled)),HPDinterval(as.mcmc(Evol_data_Vr_Oscheius_P7p_wt_mod3_scaled)),HPDinterval(as.mcmc(Evol_data_Vr_Oscheius_P8p_SSSS_mod3_scaled)))
        colnames(HPDinterval_0.95_Evol_data_Vr_Oscheius) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Vr_Oscheius <- rbind(HPDinterval(as.mcmc(Evol_data_Vr_Oscheius_P3p_SSSS_mod3_scaled),prob=.83),HPDinterval(as.mcmc(Evol_data_Vr_Oscheius_P4p_SSSS_mod3_scaled),prob=.83),HPDinterval(as.mcmc(Evol_data_Vr_Oscheius_P5p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(Evol_data_Vr_Oscheius_P6p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(Evol_data_Vr_Oscheius_P7p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(Evol_data_Vr_Oscheius_P8p_SSSS_mod3_scaled),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Vr_Oscheius) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Vr_Oscheius <- rbind(effectiveSize(Evol_data_Vr_Oscheius_P3p_SSSS_mod3_scaled),effectiveSize(Evol_data_Vr_Oscheius_P4p_SSSS_mod3_scaled),effectiveSize(Evol_data_Vr_Oscheius_P5p_wt_mod3_scaled),effectiveSize(Evol_data_Vr_Oscheius_P6p_wt_mod3_scaled),effectiveSize(Evol_data_Vr_Oscheius_P7p_wt_mod3_scaled),effectiveSize(Evol_data_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(effectiveSize_Evol_data_Vr_Oscheius) <- c("effectiveSize")
        Evol_data_Vr_Oscheius <- cbind.data.frame(mean_Evol_data_Vr_Oscheius,median_Evol_data_Vr_Oscheius,posterior.mode_Evol_data_Vr_Oscheius,HPDinterval_0.95_Evol_data_Vr_Oscheius,HPDinterval_0.83_Evol_data_Vr_Oscheius,effectiveSize_Evol_data_Vr_Oscheius)
        rownames(Evol_data_Vr_Oscheius) <- c("Evol_data_Vr_Oscheius_P3p_SSSS_mod3_scaled","Evol_data_Vr_Oscheius_P4p_SSSS_mod3_scaled","Evol_data_Vr_Oscheius_P5p_wt_mod3_scaled","Evol_data_Vr_Oscheius_P6p_wt_mod3_scaled","Evol_data_Vr_Oscheius_P7p_wt_mod3_scaled","Evol_data_Vr_Oscheius_P8p_SSSS_mod3_scaled")
        Evol_data_Vr_Oscheius <- cbind(Models = rownames(Evol_data_Vr_Oscheius),Evol_data_Vr_Oscheius)
        rownames(Evol_data_Vr_Oscheius) <- NULL
        Evol_data_Vr_Oscheius$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        Evol_data_Vr_Oscheius$Pnp_fate <- c("SSSS","SSSS","wt","wt","wt","SSSS")
        Evol_data_Vr_Oscheius$Measure <- c("Evol","Evol","Evol","Evol","Evol","Evol")
        Evol_data_Vr_Oscheius$Scale <- c("data","data","data","data","data","data")
        Evol_data_Vr_Oscheius$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        Evol_data_Vr_Oscheius
      }
      
      #Summary va_NotPhylo_data_Vr_Oscheius
      {
        
        mean_va_NotPhylo_data_Vr_Oscheius <- rbind(mean(va_NotPhylo_data_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58)),mean(va_NotPhylo_data_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58)),mean(va_NotPhylo_data_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58)),mean(va_NotPhylo_data_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58)),mean(va_NotPhylo_data_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58)),mean(va_NotPhylo_data_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58)))
        colnames(mean_va_NotPhylo_data_Vr_Oscheius) <- c("mean")
        median_va_NotPhylo_data_Vr_Oscheius <- rbind(median(va_NotPhylo_data_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58)),median(va_NotPhylo_data_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58)),median(va_NotPhylo_data_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58)),median(va_NotPhylo_data_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58)),median(va_NotPhylo_data_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58)),median(va_NotPhylo_data_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58)))
        colnames(median_va_NotPhylo_data_Vr_Oscheius) <- c("median")
        posterior.mode_va_NotPhylo_data_Vr_Oscheius <- rbind(posterior.mode(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58))),posterior.mode(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58))),posterior.mode(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58))),posterior.mode(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58))),posterior.mode(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58))),posterior.mode(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58))))
        colnames(posterior.mode_va_NotPhylo_data_Vr_Oscheius) <- c("posterior.mode")
        HPDinterval_0.95_va_NotPhylo_data_Vr_Oscheius <- rbind(HPDinterval(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58))),HPDinterval(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58))),HPDinterval(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58))),HPDinterval(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58))),HPDinterval(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58))),HPDinterval(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58))))
        colnames(HPDinterval_0.95_va_NotPhylo_data_Vr_Oscheius) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_NotPhylo_data_Vr_Oscheius <- rbind(HPDinterval(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P3p_SSSS_mod3_scaled/(2*81.58)),prob=.83),HPDinterval(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P4p_SSSS_mod3_scaled/(2*81.58)),prob=.83),HPDinterval(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P5p_wt_mod3_scaled/(2*81.58)),prob=.83),HPDinterval(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P6p_wt_mod3_scaled/(2*81.58)),prob=.83),HPDinterval(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P7p_wt_mod3_scaled/(2*81.58)),prob=.83),HPDinterval(as.mcmc(va_NotPhylo_data_Vr_Oscheius_P8p_SSSS_mod3_scaled/(2*81.58)),prob=.83))
        colnames(HPDinterval_0.83_va_NotPhylo_data_Vr_Oscheius) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_NotPhylo_data_Vr_Oscheius <- rbind(effectiveSize(va_NotPhylo_data_Vr_Oscheius_P3p_SSSS_mod3_scaled),effectiveSize(va_NotPhylo_data_Vr_Oscheius_P4p_SSSS_mod3_scaled),effectiveSize(va_NotPhylo_data_Vr_Oscheius_P5p_wt_mod3_scaled),effectiveSize(va_NotPhylo_data_Vr_Oscheius_P6p_wt_mod3_scaled),effectiveSize(va_NotPhylo_data_Vr_Oscheius_P7p_wt_mod3_scaled),effectiveSize(va_NotPhylo_data_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(effectiveSize_va_NotPhylo_data_Vr_Oscheius) <- c("effectiveSize")
        va_NotPhylo_data_Vr_Oscheius <- cbind.data.frame(mean_va_NotPhylo_data_Vr_Oscheius,median_va_NotPhylo_data_Vr_Oscheius,posterior.mode_va_NotPhylo_data_Vr_Oscheius,HPDinterval_0.95_va_NotPhylo_data_Vr_Oscheius,HPDinterval_0.83_va_NotPhylo_data_Vr_Oscheius,effectiveSize_va_NotPhylo_data_Vr_Oscheius)
        rownames(va_NotPhylo_data_Vr_Oscheius) <- c("va_NotPhylo_data_Vr_Oscheius_P3p_SSSS_mod3_scaled","va_NotPhylo_data_Vr_Oscheius_P4p_SSSS_mod3_scaled","va_NotPhylo_data_Vr_Oscheius_P5p_wt_mod3_scaled","va_NotPhylo_data_Vr_Oscheius_P6p_wt_mod3_scaled","va_NotPhylo_data_Vr_Oscheius_P7p_wt_mod3_scaled","va_NotPhylo_data_Vr_Oscheius_P8p_SSSS_mod3_scaled")
        va_NotPhylo_data_Vr_Oscheius <- cbind(Models = rownames(va_NotPhylo_data_Vr_Oscheius),va_NotPhylo_data_Vr_Oscheius)
        rownames(va_NotPhylo_data_Vr_Oscheius) <- NULL
        va_NotPhylo_data_Vr_Oscheius$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        va_NotPhylo_data_Vr_Oscheius$Pnp_fate <- c("SSSS","SSSS","wt","wt","wt","SSSS")
        va_NotPhylo_data_Vr_Oscheius$Measure <- c("va_NotPhylo","va_NotPhylo","va_NotPhylo","va_NotPhylo","va_NotPhylo","va_NotPhylo")
        va_NotPhylo_data_Vr_Oscheius$Scale <- c("data","data","data","data","data","data")
        va_NotPhylo_data_Vr_Oscheius$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        va_NotPhylo_data_Vr_Oscheius
      }
      
      #Summary trait_mean_data_Vr_Oscheius
      {
        
        mean_trait_mean_data_Vr_Oscheius <- rbind(mean(trait_mean_data_Vr_Oscheius_P3p_SSSS_mod3_scaled),mean(trait_mean_data_Vr_Oscheius_P4p_SSSS_mod3_scaled),mean(trait_mean_data_Vr_Oscheius_P5p_wt_mod3_scaled),mean(trait_mean_data_Vr_Oscheius_P6p_wt_mod3_scaled),mean(trait_mean_data_Vr_Oscheius_P7p_wt_mod3_scaled),mean(trait_mean_data_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(mean_trait_mean_data_Vr_Oscheius) <- c("mean")
        median_trait_mean_data_Vr_Oscheius <- rbind(median(trait_mean_data_Vr_Oscheius_P3p_SSSS_mod3_scaled),median(trait_mean_data_Vr_Oscheius_P4p_SSSS_mod3_scaled),median(trait_mean_data_Vr_Oscheius_P5p_wt_mod3_scaled),median(trait_mean_data_Vr_Oscheius_P6p_wt_mod3_scaled),median(trait_mean_data_Vr_Oscheius_P7p_wt_mod3_scaled),median(trait_mean_data_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(median_trait_mean_data_Vr_Oscheius) <- c("median")
        posterior.mode_trait_mean_data_Vr_Oscheius <- rbind(posterior.mode(as.mcmc(trait_mean_data_Vr_Oscheius_P3p_SSSS_mod3_scaled)),posterior.mode(as.mcmc(trait_mean_data_Vr_Oscheius_P4p_SSSS_mod3_scaled)),posterior.mode(as.mcmc(trait_mean_data_Vr_Oscheius_P5p_wt_mod3_scaled)),posterior.mode(as.mcmc(trait_mean_data_Vr_Oscheius_P6p_wt_mod3_scaled)),posterior.mode(as.mcmc(trait_mean_data_Vr_Oscheius_P7p_wt_mod3_scaled)),posterior.mode(as.mcmc(trait_mean_data_Vr_Oscheius_P8p_SSSS_mod3_scaled)))
        colnames(posterior.mode_trait_mean_data_Vr_Oscheius) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Vr_Oscheius <- rbind(HPDinterval(as.mcmc(trait_mean_data_Vr_Oscheius_P3p_SSSS_mod3_scaled)),HPDinterval(as.mcmc(trait_mean_data_Vr_Oscheius_P4p_SSSS_mod3_scaled)),HPDinterval(as.mcmc(trait_mean_data_Vr_Oscheius_P5p_wt_mod3_scaled)),HPDinterval(as.mcmc(trait_mean_data_Vr_Oscheius_P6p_wt_mod3_scaled)),HPDinterval(as.mcmc(trait_mean_data_Vr_Oscheius_P7p_wt_mod3_scaled)),HPDinterval(as.mcmc(trait_mean_data_Vr_Oscheius_P8p_SSSS_mod3_scaled)))
        colnames(HPDinterval_0.95_trait_mean_data_Vr_Oscheius) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Vr_Oscheius <- rbind(HPDinterval(as.mcmc(trait_mean_data_Vr_Oscheius_P3p_SSSS_mod3_scaled),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Vr_Oscheius_P4p_SSSS_mod3_scaled),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Vr_Oscheius_P5p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Vr_Oscheius_P6p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Vr_Oscheius_P7p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Vr_Oscheius_P8p_SSSS_mod3_scaled),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Vr_Oscheius) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Vr_Oscheius <- rbind(effectiveSize(trait_mean_data_Vr_Oscheius_P3p_SSSS_mod3_scaled),effectiveSize(trait_mean_data_Vr_Oscheius_P4p_SSSS_mod3_scaled),effectiveSize(trait_mean_data_Vr_Oscheius_P5p_wt_mod3_scaled),effectiveSize(trait_mean_data_Vr_Oscheius_P6p_wt_mod3_scaled),effectiveSize(trait_mean_data_Vr_Oscheius_P7p_wt_mod3_scaled),effectiveSize(trait_mean_data_Vr_Oscheius_P8p_SSSS_mod3_scaled))
        colnames(effectiveSize_trait_mean_data_Vr_Oscheius) <- c("effectiveSize")
        trait_mean_data_Vr_Oscheius <- cbind.data.frame(mean_trait_mean_data_Vr_Oscheius,median_trait_mean_data_Vr_Oscheius,posterior.mode_trait_mean_data_Vr_Oscheius,HPDinterval_0.95_trait_mean_data_Vr_Oscheius,HPDinterval_0.83_trait_mean_data_Vr_Oscheius,effectiveSize_trait_mean_data_Vr_Oscheius)
        rownames(trait_mean_data_Vr_Oscheius) <- c("trait_mean_data_Vr_Oscheius_P3p_SSSS_mod3_scaled","trait_mean_data_Vr_Oscheius_P4p_SSSS_mod3_scaled","trait_mean_data_Vr_Oscheius_P5p_wt_mod3_scaled","trait_mean_data_Vr_Oscheius_P6p_wt_mod3_scaled","trait_mean_data_Vr_Oscheius_P7p_wt_mod3_scaled","trait_mean_data_Vr_Oscheius_P8p_SSSS_mod3_scaled")
        trait_mean_data_Vr_Oscheius <- cbind(Models = rownames(trait_mean_data_Vr_Oscheius),trait_mean_data_Vr_Oscheius)
        rownames(trait_mean_data_Vr_Oscheius) <- NULL
        trait_mean_data_Vr_Oscheius$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        trait_mean_data_Vr_Oscheius$Pnp_fate <- c("SSSS","SSSS","wt","wt","wt","SSSS")
        trait_mean_data_Vr_Oscheius$Measure <- c("trait_mean","trait_mean","trait_mean","trait_mean","trait_mean","trait_mean")
        trait_mean_data_Vr_Oscheius$Scale <- c("data","data","data","data","data","data")
        trait_mean_data_Vr_Oscheius$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        trait_mean_data_Vr_Oscheius
      }
      
      data_Vr_Oscheius <- rbind.data.frame(va_data_Vr_Oscheius, H2_data_Vr_Oscheius,Evol_data_Vr_Oscheius,va_NotPhylo_data_Vr_Oscheius,trait_mean_data_Vr_Oscheius)
      data_Vr_Oscheius
    } 
    
    
    
    Vr_Oscheius_summary <- rbind.data.frame(liab_Vr_Oscheius, data_Vr_Oscheius)
    View(Vr_Oscheius_summary)
    
    names(Vr_Oscheius_summary)[names(Vr_Oscheius_summary) == "Models"] <- "Model_name"
    dim(Vr_Oscheius_summary)
    Vr_Oscheius_summary$Genus <- rep("Oscheius",60)
    
    write_xlsx(Vr_Oscheius_summary, "Vr_Oscheius_summary_SSSS.xlsx")
    
    
    
    #remove Vr_Oscheius models 
    {
      remove( Vr_Oscheius_P3p_SSSS_mod3_scaled)
      remove( Vr_Oscheius_P4p_SSSS_mod3_scaled)
      remove( Vr_Oscheius_P8p_SSSS_mod3_scaled)
      remove( Vr_Oscheius_P5p_wt_mod3_scaled)
      remove( Vr_Oscheius_P6p_wt_mod3_scaled)
      remove( Vr_Oscheius_P7p_wt_mod3_scaled)
      
    }
  }
  
  
  
}

## ---- Vr_Oscheius_P3p_divided_P4p ----



#Vr_Oscheius_P3p_divided_P4p_SSSS_liab
{
  
  va_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled <- va_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled / va_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled
  h2_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled <- h2_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled / h2_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled
  Evol_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled <- Evol_liab_Vr_Oscheius_P3p_SSSS_mod3_scaled / Evol_liab_Vr_Oscheius_P4p_SSSS_mod3_scaled
  
  mean_va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS <- rbind(mean(log10(va_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)),mean(log10(h2_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)), mean(log10(Evol_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)))
  colnames(mean_va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS) <- c("mean")
  median_va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS <- rbind(median(log10(va_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)),median(log10(h2_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)), median(log10(Evol_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)))
  colnames(median_va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS) <- c("median")
  posterior.mode_va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS <- rbind(posterior.mode(as.mcmc(log10(va_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled))),posterior.mode(as.mcmc(log10(h2_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled))),posterior.mode(as.mcmc(log10(Evol_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled))))
  colnames(posterior.mode_va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS) <- c("posterior.mode")
  HPDinterval_0.95_va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled))),HPDinterval(as.mcmc(log10(h2_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled))),HPDinterval(as.mcmc(log10(Evol_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled))))
  colnames(HPDinterval_0.95_va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS) <- c("CI_lower_0.95","CI_upper_0.95")
  HPDinterval_0.83_va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)),prob=.83),HPDinterval(as.mcmc(log10(h2_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)),prob=.83),HPDinterval(as.mcmc(log10(Evol_liab_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)),prob=.83))
  colnames(HPDinterval_0.83_va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS) <- c("CI_lower_0.83","CI_upper_0.83")
  va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS <- cbind.data.frame(mean_va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS,median_va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS,posterior.mode_va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS,HPDinterval_0.95_va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS,HPDinterval_0.83_va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS)
  rownames(va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS) <- c("va_liab_Vr_Oscheius_P3p_divided_P4p_SSSS_log10","h2_liab_Vr_Oscheius_P3p_divided_P4p_SSSS_log10","Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS_log10")
  va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS <- cbind(Comparisons = rownames(va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS),va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS)
  rownames(va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS) <- NULL
  va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS$Measure <- c("Va_Phylo","H2", "Evol")
  va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS$Pnp_fate <- rep("SSSS",3)
  va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS$Genus <- rep("Oscheius",3)
  va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS$Scale <- rep("liab",3)
  va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS
  
  
  
}



pdf("va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_summary.pdf")
ggplot(va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS, aes(x=Measure, y= median)) +
  geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
  geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
  geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
  theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
  labs(y = "log10 ( P3.p / P4.p)", title = "Vr_Oscheius_liab_log10(P3p/P4p)")
dev.off() 


#Vr_Oscheius_P3p_divided_P4p_SSSS_data
{
  
  va_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled <- va_data_Vr_Oscheius_P3p_SSSS_mod3_scaled / va_data_Vr_Oscheius_P4p_SSSS_mod3_scaled
  h2_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled <- h2_data_Vr_Oscheius_P3p_SSSS_mod3_scaled / h2_data_Vr_Oscheius_P4p_SSSS_mod3_scaled
  Evol_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled <- Evol_data_Vr_Oscheius_P3p_SSSS_mod3_scaled / Evol_data_Vr_Oscheius_P4p_SSSS_mod3_scaled
  
  mean_va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS <- rbind(mean(log10(va_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)),mean(log10(h2_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)), mean(log10(Evol_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)))
  colnames(mean_va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS) <- c("mean")
  median_va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS <- rbind(median(log10(va_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)),median(log10(h2_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)), median(log10(Evol_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)))
  colnames(median_va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS) <- c("median")
  posterior.mode_va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS <- rbind(posterior.mode(as.mcmc(log10(va_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled))),posterior.mode(as.mcmc(log10(h2_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled))),posterior.mode(as.mcmc(log10(Evol_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled))))
  colnames(posterior.mode_va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS) <- c("posterior.mode")
  HPDinterval_0.95_va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled))),HPDinterval(as.mcmc(log10(h2_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled))),HPDinterval(as.mcmc(log10(Evol_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled))))
  colnames(HPDinterval_0.95_va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS) <- c("CI_lower_0.95","CI_upper_0.95")
  HPDinterval_0.83_va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS <- rbind(HPDinterval(as.mcmc(log10(va_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)),prob=.83),HPDinterval(as.mcmc(log10(h2_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)),prob=.83),HPDinterval(as.mcmc(log10(Evol_data_Vr_Oscheius_P3p_divided_P4p_mod3_scaled)),prob=.83))
  colnames(HPDinterval_0.83_va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS) <- c("CI_lower_0.83","CI_upper_0.83")
  va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS <- cbind.data.frame(mean_va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS,median_va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS,posterior.mode_va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS,HPDinterval_0.95_va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS,HPDinterval_0.83_va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS)
  rownames(va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS) <- c("va_data_Vr_Oscheius_P3p_divided_P4p_SSSS_log10","h2_data_Vr_Oscheius_P3p_divided_P4p_SSSS_log10","Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS_log10")
  va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS <- cbind(Comparisons = rownames(va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS),va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS)
  rownames(va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS) <- NULL
  va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS$Measure <- c("Va_Phylo","H2", "Evol")
  va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS$Pnp_fate <- rep("SSSS",3)
  va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS$Genus <- rep("Oscheius",3)
  va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS$Scale <- rep("data",3)
  va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS
  
}


pdf("va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_summary.pdf")
ggplot(va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS, aes(x=Measure, y= median)) +
  geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
  geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
  geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
  theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
  labs(y = "log10 ( P3.p / P4.p)", title = "Vr_Oscheius_data_log10(P3p/P4p)")
dev.off() 



va_h2_Evol_Vr_Oscheius_P3p_divided_P4p_SSSS_summary <- rbind.data.frame(va_h2_Evol_liab_Vr_Oscheius_P3p_divided_P4p_SSSS,va_h2_Evol_data_Vr_Oscheius_P3p_divided_P4p_SSSS)
va_h2_Evol_Vr_Oscheius_P3p_divided_P4p_SSSS_summary


pdf("va_h2_Evol_Vr_Oscheius_P3p_divided_P4p_SSSS_summary.pdf")
ggplot(va_h2_Evol_Vr_Oscheius_P3p_divided_P4p_SSSS_summary, aes(x=Measure, y= median)) +
  geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
  geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
  geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
  theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
  facet_grid(rows= vars(Scale))+
  labs(y = "log10 ( P3.p / P4.p)", title = "Vr_Oscheius_log10(P3p/P4p)")
dev.off() 


write_xlsx(va_h2_Evol_Vr_Oscheius_P3p_divided_P4p_SSSS_summary, "2025.01.07-va_h2_Evol_Vr_Oscheius_P3p_divided_P4p_SSSS_summary.xlsx")















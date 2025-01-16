#NB: Vr=Va/(2*106.30) - Va is half the line identity variances since we have measured homozygous diploid inbred lines and assume codominance & divided by the total length of the tree to show the increase in Variance per Myr

#----Vr_Caenorhabditis_summary_SS----
{
  
  #Summary Vr_Caenorhabditis
  {
  ----##Summary liability scale Vr_Caenorhabditis----
    {
      #Summary va_liab_Vr_Caenorhabditis
      {
        
        mean_va_liab_Vr_Caenorhabditis <- rbind(mean(va_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30)),mean(va_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30)),mean(va_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30)),mean(va_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30)),mean(va_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30)),mean(va_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30)))
        colnames(mean_va_liab_Vr_Caenorhabditis) <- c("mean")
        median_va_liab_Vr_Caenorhabditis <- rbind(median(va_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30)),median(va_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30)),median(va_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30)),median(va_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30)),median(va_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30)),median(va_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30)))
        colnames(median_va_liab_Vr_Caenorhabditis) <- c("median")
        posterior.mode_va_liab_Vr_Caenorhabditis <- rbind(posterior.mode(va_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30)),posterior.mode(va_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30)),posterior.mode(va_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30)),posterior.mode(va_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30)),posterior.mode(va_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30)),posterior.mode(va_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30)))
        colnames(posterior.mode_va_liab_Vr_Caenorhabditis) <- c("posterior.mode")
        HPDinterval_0.95_va_liab_Vr_Caenorhabditis <- rbind(HPDinterval(va_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30)),HPDinterval(va_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30)),HPDinterval(va_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30)),HPDinterval(va_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30)),HPDinterval(va_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30)),HPDinterval(va_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30)))
        colnames(HPDinterval_0.95_va_liab_Vr_Caenorhabditis) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_liab_Vr_Caenorhabditis <- rbind(HPDinterval(va_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30),prob=.83),HPDinterval(va_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30),prob=.83),HPDinterval(va_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30),prob=.83),HPDinterval(va_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30),prob=.83),HPDinterval(va_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30),prob=.83),HPDinterval(va_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30),prob=.83))
        colnames(HPDinterval_0.83_va_liab_Vr_Caenorhabditis) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_liab_Vr_Caenorhabditis <- rbind(effectiveSize(va_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled),effectiveSize(va_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled),effectiveSize(va_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled),effectiveSize(va_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled),effectiveSize(va_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled),effectiveSize(va_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(effectiveSize_va_liab_Vr_Caenorhabditis) <- c("effectiveSize")
        va_liab_Vr_Caenorhabditis <- cbind.data.frame(mean_va_liab_Vr_Caenorhabditis,median_va_liab_Vr_Caenorhabditis,posterior.mode_va_liab_Vr_Caenorhabditis,HPDinterval_0.95_va_liab_Vr_Caenorhabditis,HPDinterval_0.83_va_liab_Vr_Caenorhabditis,effectiveSize_va_liab_Vr_Caenorhabditis)
        rownames(va_liab_Vr_Caenorhabditis) <- c("va_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled","va_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled","va_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled","va_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled","va_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled","va_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled")
        va_liab_Vr_Caenorhabditis <- cbind(Models = rownames(va_liab_Vr_Caenorhabditis),va_liab_Vr_Caenorhabditis)
        rownames(va_liab_Vr_Caenorhabditis) <- NULL
        va_liab_Vr_Caenorhabditis$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        va_liab_Vr_Caenorhabditis$Pnp_fate <- c("SS","SS","wt","wt","wt","SS")
        va_liab_Vr_Caenorhabditis$Measure <- c("Va_Phylo","Va_Phylo","Va_Phylo","Va_Phylo","Va_Phylo","Va_Phylo")
        va_liab_Vr_Caenorhabditis$Scale <- c("liab","liab","liab","liab","liab","liab")
        va_liab_Vr_Caenorhabditis$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        va_liab_Vr_Caenorhabditis
      }
      
      #Summary h2_liab_Vr_Caenorhabditis
      {
        
        mean_H2_liab_Vr_Caenorhabditis <- rbind(mean(h2_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled),mean(h2_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled),mean(h2_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled),mean(h2_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled),mean(h2_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled),mean(h2_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(mean_H2_liab_Vr_Caenorhabditis) <- c("mean")
        median_H2_liab_Vr_Caenorhabditis <- rbind(median(h2_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled),median(h2_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled),median(h2_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled),median(h2_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled),median(h2_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled),median(h2_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(median_H2_liab_Vr_Caenorhabditis) <- c("median")
        posterior.mode_H2_liab_Vr_Caenorhabditis <- rbind(posterior.mode(h2_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled),posterior.mode(h2_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled),posterior.mode(h2_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled),posterior.mode(h2_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled),posterior.mode(h2_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled),posterior.mode(h2_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(posterior.mode_H2_liab_Vr_Caenorhabditis) <- c("posterior.mode")
        HPDinterval_0.95_H2_liab_Vr_Caenorhabditis <- rbind(HPDinterval(h2_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled),HPDinterval(h2_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled),HPDinterval(h2_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled),HPDinterval(h2_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled),HPDinterval(h2_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled),HPDinterval(h2_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(HPDinterval_0.95_H2_liab_Vr_Caenorhabditis) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_H2_liab_Vr_Caenorhabditis <- rbind(HPDinterval(h2_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled,prob=.83),HPDinterval(h2_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled,prob=.83),HPDinterval(h2_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled,prob=.83),HPDinterval(h2_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled,prob=.83),HPDinterval(h2_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled,prob=.83),HPDinterval(h2_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled,prob=.83))
        colnames(HPDinterval_0.83_H2_liab_Vr_Caenorhabditis) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_H2_liab_Vr_Caenorhabditis <- rbind(effectiveSize(h2_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled),effectiveSize(h2_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled),effectiveSize(h2_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled),effectiveSize(h2_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled),effectiveSize(h2_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled),effectiveSize(h2_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(effectiveSize_H2_liab_Vr_Caenorhabditis) <- c("effectiveSize")
        H2_liab_Vr_Caenorhabditis <- cbind.data.frame(mean_H2_liab_Vr_Caenorhabditis,median_H2_liab_Vr_Caenorhabditis,posterior.mode_H2_liab_Vr_Caenorhabditis,HPDinterval_0.95_H2_liab_Vr_Caenorhabditis,HPDinterval_0.83_H2_liab_Vr_Caenorhabditis,effectiveSize_H2_liab_Vr_Caenorhabditis)
        rownames(H2_liab_Vr_Caenorhabditis) <- c("h2_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled","h2_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled","h2_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled","h2_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled","h2_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled","h2_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled")
        H2_liab_Vr_Caenorhabditis <- cbind(Models = rownames(H2_liab_Vr_Caenorhabditis),H2_liab_Vr_Caenorhabditis)
        rownames(H2_liab_Vr_Caenorhabditis) <- NULL
        H2_liab_Vr_Caenorhabditis$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        H2_liab_Vr_Caenorhabditis$Pnp_fate <- c("SS","SS","wt","wt","wt","SS")
        H2_liab_Vr_Caenorhabditis$Measure <- c("H2","H2","H2","H2","H2","H2")
        H2_liab_Vr_Caenorhabditis$Scale <- c("liab","liab","liab","liab","liab","liab")
        H2_liab_Vr_Caenorhabditis$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        H2_liab_Vr_Caenorhabditis
      }
      
      #Summary Evol_liab_Vr_Caenorhabditis
      {
        
        mean_Evol_liab_Vr_Caenorhabditis <- rbind(mean(Evol_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled),mean(Evol_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled),mean(Evol_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled),mean(Evol_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled),mean(Evol_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled),mean(Evol_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(mean_Evol_liab_Vr_Caenorhabditis) <- c("mean")
        median_Evol_liab_Vr_Caenorhabditis <- rbind(median(Evol_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled),median(Evol_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled),median(Evol_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled),median(Evol_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled),median(Evol_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled),median(Evol_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(median_Evol_liab_Vr_Caenorhabditis) <- c("median")
        posterior.mode_Evol_liab_Vr_Caenorhabditis <- rbind(posterior.mode(Evol_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled),posterior.mode(Evol_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled),posterior.mode(Evol_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled),posterior.mode(Evol_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled),posterior.mode(Evol_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled),posterior.mode(Evol_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(posterior.mode_Evol_liab_Vr_Caenorhabditis) <- c("posterior.mode")
        HPDinterval_0.95_Evol_liab_Vr_Caenorhabditis <- rbind(HPDinterval(Evol_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled),HPDinterval(Evol_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled),HPDinterval(Evol_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled),HPDinterval(Evol_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled),HPDinterval(Evol_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled),HPDinterval(Evol_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(HPDinterval_0.95_Evol_liab_Vr_Caenorhabditis) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_liab_Vr_Caenorhabditis <- rbind(HPDinterval(Evol_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled,prob=.83),HPDinterval(Evol_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled,prob=.83),HPDinterval(Evol_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled,prob=.83),HPDinterval(Evol_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled,prob=.83),HPDinterval(Evol_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled,prob=.83),HPDinterval(Evol_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled,prob=.83))
        colnames(HPDinterval_0.83_Evol_liab_Vr_Caenorhabditis) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_liab_Vr_Caenorhabditis <- rbind(effectiveSize(Evol_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled),effectiveSize(Evol_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled),effectiveSize(Evol_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled),effectiveSize(Evol_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled),effectiveSize(Evol_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled),effectiveSize(Evol_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(effectiveSize_Evol_liab_Vr_Caenorhabditis) <- c("effectiveSize")
        Evol_liab_Vr_Caenorhabditis <- cbind.data.frame(mean_Evol_liab_Vr_Caenorhabditis,median_Evol_liab_Vr_Caenorhabditis,posterior.mode_Evol_liab_Vr_Caenorhabditis,HPDinterval_0.95_Evol_liab_Vr_Caenorhabditis,HPDinterval_0.83_Evol_liab_Vr_Caenorhabditis,effectiveSize_Evol_liab_Vr_Caenorhabditis)
        rownames(Evol_liab_Vr_Caenorhabditis) <- c("Evol_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled","Evol_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled","Evol_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled","Evol_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled","Evol_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled","Evol_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled")
        Evol_liab_Vr_Caenorhabditis <- cbind(Models = rownames(Evol_liab_Vr_Caenorhabditis),Evol_liab_Vr_Caenorhabditis)
        rownames(Evol_liab_Vr_Caenorhabditis) <- NULL
        Evol_liab_Vr_Caenorhabditis$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        Evol_liab_Vr_Caenorhabditis$Pnp_fate <- c("SS","SS","wt","wt","wt","SS")
        Evol_liab_Vr_Caenorhabditis$Measure <- c("Evol","Evol","Evol","Evol","Evol","Evol")
        Evol_liab_Vr_Caenorhabditis$Scale <- c("liab","liab","liab","liab","liab","liab")
        Evol_liab_Vr_Caenorhabditis$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        Evol_liab_Vr_Caenorhabditis
      }
      
      #Summary va_NotPhylo_liab_Vr_Caenorhabditis
      {
        
        mean_va_NotPhylo_liab_Vr_Caenorhabditis <- rbind(mean(va_NotPhylo_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30)),mean(va_NotPhylo_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30)),mean(va_NotPhylo_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30)),mean(va_NotPhylo_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30)),mean(va_NotPhylo_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30)),mean(va_NotPhylo_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30)))
        colnames(mean_va_NotPhylo_liab_Vr_Caenorhabditis) <- c("mean")
        median_va_NotPhylo_liab_Vr_Caenorhabditis <- rbind(median(va_NotPhylo_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30)),median(va_NotPhylo_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30)),median(va_NotPhylo_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30)),median(va_NotPhylo_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30)),median(va_NotPhylo_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30)),median(va_NotPhylo_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30)))
        colnames(median_va_NotPhylo_liab_Vr_Caenorhabditis) <- c("median")
        posterior.mode_va_NotPhylo_liab_Vr_Caenorhabditis <- rbind(posterior.mode(va_NotPhylo_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30)),posterior.mode(va_NotPhylo_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30)),posterior.mode(va_NotPhylo_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30)),posterior.mode(va_NotPhylo_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30)),posterior.mode(va_NotPhylo_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30)),posterior.mode(va_NotPhylo_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30)))
        colnames(posterior.mode_va_NotPhylo_liab_Vr_Caenorhabditis) <- c("posterior.mode")
        HPDinterval_0.95_va_NotPhylo_liab_Vr_Caenorhabditis <- rbind(HPDinterval(va_NotPhylo_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30)),HPDinterval(va_NotPhylo_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30)),HPDinterval(va_NotPhylo_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30)),HPDinterval(va_NotPhylo_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30)),HPDinterval(va_NotPhylo_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30)),HPDinterval(va_NotPhylo_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30)))
        colnames(HPDinterval_0.95_va_NotPhylo_liab_Vr_Caenorhabditis) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_NotPhylo_liab_Vr_Caenorhabditis <- rbind(HPDinterval(va_NotPhylo_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30),prob=.83),HPDinterval(va_NotPhylo_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30),prob=.83),HPDinterval(va_NotPhylo_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30),prob=.83),HPDinterval(va_NotPhylo_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30),prob=.83),HPDinterval(va_NotPhylo_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30),prob=.83),HPDinterval(va_NotPhylo_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30),prob=.83))
        colnames(HPDinterval_0.83_va_NotPhylo_liab_Vr_Caenorhabditis) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_NotPhylo_liab_Vr_Caenorhabditis <- rbind(effectiveSize(va_NotPhylo_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled),effectiveSize(va_NotPhylo_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled),effectiveSize(va_NotPhylo_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled),effectiveSize(va_NotPhylo_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled),effectiveSize(va_NotPhylo_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled),effectiveSize(va_NotPhylo_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(effectiveSize_va_NotPhylo_liab_Vr_Caenorhabditis) <- c("effectiveSize")
        va_NotPhylo_liab_Vr_Caenorhabditis <- cbind.data.frame(mean_va_NotPhylo_liab_Vr_Caenorhabditis,median_va_NotPhylo_liab_Vr_Caenorhabditis,posterior.mode_va_NotPhylo_liab_Vr_Caenorhabditis,HPDinterval_0.95_va_NotPhylo_liab_Vr_Caenorhabditis,HPDinterval_0.83_va_NotPhylo_liab_Vr_Caenorhabditis,effectiveSize_va_NotPhylo_liab_Vr_Caenorhabditis)
        rownames(va_NotPhylo_liab_Vr_Caenorhabditis) <- c("va_NotPhylo_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled","va_NotPhylo_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled","va_NotPhylo_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled","va_NotPhylo_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled","va_NotPhylo_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled","va_NotPhylo_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled")
        va_NotPhylo_liab_Vr_Caenorhabditis <- cbind(Models = rownames(va_NotPhylo_liab_Vr_Caenorhabditis),va_NotPhylo_liab_Vr_Caenorhabditis)
        rownames(va_NotPhylo_liab_Vr_Caenorhabditis) <- NULL
        va_NotPhylo_liab_Vr_Caenorhabditis$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        va_NotPhylo_liab_Vr_Caenorhabditis$Pnp_fate <- c("SS","SS","wt","wt","wt","SS")
        va_NotPhylo_liab_Vr_Caenorhabditis$Measure <- c("va_NotPhylo","va_NotPhylo","va_NotPhylo","va_NotPhylo","va_NotPhylo","va_NotPhylo")
        va_NotPhylo_liab_Vr_Caenorhabditis$Scale <- c("liab","liab","liab","liab","liab","liab")
        va_NotPhylo_liab_Vr_Caenorhabditis$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        va_NotPhylo_liab_Vr_Caenorhabditis
      }
      
      #Summary trait_mean_liab_Vr_Caenorhabditis
      {
        
        mean_trait_mean_liab_Vr_Caenorhabditis <- rbind(mean(trait_mean_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled),mean(trait_mean_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled),mean(trait_mean_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled),mean(trait_mean_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled),mean(trait_mean_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled),mean(trait_mean_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(mean_trait_mean_liab_Vr_Caenorhabditis) <- c("mean")
        median_trait_mean_liab_Vr_Caenorhabditis <- rbind(median(trait_mean_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled),median(trait_mean_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled),median(trait_mean_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled),median(trait_mean_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled),median(trait_mean_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled),median(trait_mean_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(median_trait_mean_liab_Vr_Caenorhabditis) <- c("median")
        posterior.mode_trait_mean_liab_Vr_Caenorhabditis <- rbind(posterior.mode(trait_mean_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled),posterior.mode(trait_mean_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled),posterior.mode(trait_mean_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled),posterior.mode(trait_mean_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled),posterior.mode(trait_mean_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled),posterior.mode(trait_mean_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(posterior.mode_trait_mean_liab_Vr_Caenorhabditis) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_liab_Vr_Caenorhabditis <- rbind(HPDinterval(trait_mean_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled),HPDinterval(trait_mean_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled),HPDinterval(trait_mean_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled),HPDinterval(trait_mean_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled),HPDinterval(trait_mean_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled),HPDinterval(trait_mean_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(HPDinterval_0.95_trait_mean_liab_Vr_Caenorhabditis) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_liab_Vr_Caenorhabditis <- rbind(HPDinterval(trait_mean_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled,prob=.83),HPDinterval(trait_mean_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled,prob=.83),HPDinterval(trait_mean_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled,prob=.83),HPDinterval(trait_mean_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled,prob=.83),HPDinterval(trait_mean_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled,prob=.83),HPDinterval(trait_mean_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled,prob=.83))
        colnames(HPDinterval_0.83_trait_mean_liab_Vr_Caenorhabditis) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_liab_Vr_Caenorhabditis <- rbind(effectiveSize(trait_mean_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled),effectiveSize(trait_mean_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled),effectiveSize(trait_mean_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled),effectiveSize(trait_mean_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled),effectiveSize(trait_mean_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled),effectiveSize(trait_mean_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(effectiveSize_trait_mean_liab_Vr_Caenorhabditis) <- c("effectiveSize")
        trait_mean_liab_Vr_Caenorhabditis <- cbind.data.frame(mean_trait_mean_liab_Vr_Caenorhabditis,median_trait_mean_liab_Vr_Caenorhabditis,posterior.mode_trait_mean_liab_Vr_Caenorhabditis,HPDinterval_0.95_trait_mean_liab_Vr_Caenorhabditis,HPDinterval_0.83_trait_mean_liab_Vr_Caenorhabditis,effectiveSize_trait_mean_liab_Vr_Caenorhabditis)
        rownames(trait_mean_liab_Vr_Caenorhabditis) <- c("trait_mean_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled","trait_mean_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled","trait_mean_liab_Vr_Caenorhabditis_P5p_wt_mod3_scaled","trait_mean_liab_Vr_Caenorhabditis_P6p_wt_mod3_scaled","trait_mean_liab_Vr_Caenorhabditis_P7p_wt_mod3_scaled","trait_mean_liab_Vr_Caenorhabditis_P8p_SS_mod3_scaled")
        trait_mean_liab_Vr_Caenorhabditis <- cbind(Models = rownames(trait_mean_liab_Vr_Caenorhabditis),trait_mean_liab_Vr_Caenorhabditis)
        rownames(trait_mean_liab_Vr_Caenorhabditis) <- NULL
        trait_mean_liab_Vr_Caenorhabditis$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        trait_mean_liab_Vr_Caenorhabditis$Pnp_fate <- c("SS","SS","wt","wt","wt","SS")
        trait_mean_liab_Vr_Caenorhabditis$Measure <- c("trait_mean","trait_mean","trait_mean","trait_mean","trait_mean","trait_mean")
        trait_mean_liab_Vr_Caenorhabditis$Scale <- c("liab","liab","liab","liab","liab","liab")
        trait_mean_liab_Vr_Caenorhabditis$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        trait_mean_liab_Vr_Caenorhabditis
      }
      
      liab_Vr_Caenorhabditis <- rbind.data.frame(va_liab_Vr_Caenorhabditis, H2_liab_Vr_Caenorhabditis,Evol_liab_Vr_Caenorhabditis,va_NotPhylo_liab_Vr_Caenorhabditis,trait_mean_liab_Vr_Caenorhabditis)
      liab_Vr_Caenorhabditis
    }
    
    
    
    ----##Summary data scale Vr_Caenorhabditis----
    {
      #Summary va_data_Vr_Caenorhabditis
      {
        
        mean_va_data_Vr_Caenorhabditis <- rbind(mean(va_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30)),mean(va_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30)),mean(va_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30)),mean(va_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30)),mean(va_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30)),mean(va_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30)))
        colnames(mean_va_data_Vr_Caenorhabditis) <- c("mean")
        median_va_data_Vr_Caenorhabditis <- rbind(median(va_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30)),median(va_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30)),median(va_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30)),median(va_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30)),median(va_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30)),median(va_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30)))
        colnames(median_va_data_Vr_Caenorhabditis) <- c("median")
        posterior.mode_va_data_Vr_Caenorhabditis <- rbind(posterior.mode(as.mcmc(va_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30))),posterior.mode(as.mcmc(va_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30))),posterior.mode(as.mcmc(va_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30))),posterior.mode(as.mcmc(va_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30))),posterior.mode(as.mcmc(va_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30))),posterior.mode(as.mcmc(va_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30))))
        colnames(posterior.mode_va_data_Vr_Caenorhabditis) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Vr_Caenorhabditis <- rbind(HPDinterval(as.mcmc(va_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30))),HPDinterval(as.mcmc(va_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30))),HPDinterval(as.mcmc(va_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30))),HPDinterval(as.mcmc(va_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30))),HPDinterval(as.mcmc(va_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30))),HPDinterval(as.mcmc(va_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30))))
        colnames(HPDinterval_0.95_va_data_Vr_Caenorhabditis) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Vr_Caenorhabditis <- rbind(HPDinterval(as.mcmc(va_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30)),prob=.83),HPDinterval(as.mcmc(va_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30)),prob=.83),HPDinterval(as.mcmc(va_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30)),prob=.83),HPDinterval(as.mcmc(va_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30)),prob=.83),HPDinterval(as.mcmc(va_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30)),prob=.83),HPDinterval(as.mcmc(va_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30)),prob=.83))
        colnames(HPDinterval_0.83_va_data_Vr_Caenorhabditis) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Vr_Caenorhabditis <- rbind(effectiveSize(va_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled),effectiveSize(va_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled),effectiveSize(va_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled),effectiveSize(va_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled),effectiveSize(va_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled),effectiveSize(va_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(effectiveSize_va_data_Vr_Caenorhabditis) <- c("effectiveSize")
        va_data_Vr_Caenorhabditis <- cbind.data.frame(mean_va_data_Vr_Caenorhabditis,median_va_data_Vr_Caenorhabditis,posterior.mode_va_data_Vr_Caenorhabditis,HPDinterval_0.95_va_data_Vr_Caenorhabditis,HPDinterval_0.83_va_data_Vr_Caenorhabditis,effectiveSize_va_data_Vr_Caenorhabditis)
        rownames(va_data_Vr_Caenorhabditis) <- c("va_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled","va_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled","va_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled","va_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled","va_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled","va_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled")
        va_data_Vr_Caenorhabditis <- cbind(Models = rownames(va_data_Vr_Caenorhabditis),va_data_Vr_Caenorhabditis)
        rownames(va_data_Vr_Caenorhabditis) <- NULL
        va_data_Vr_Caenorhabditis$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        va_data_Vr_Caenorhabditis$Pnp_fate <- c("SS","SS","wt","wt","wt","SS")
        va_data_Vr_Caenorhabditis$Measure <- c("Va_Phylo","Va_Phylo","Va_Phylo","Va_Phylo","Va_Phylo","Va_Phylo")
        va_data_Vr_Caenorhabditis$Scale <- c("data","data","data","data","data","data")
        va_data_Vr_Caenorhabditis$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        va_data_Vr_Caenorhabditis
      }
      
      #Summary h2_data_Vr_Caenorhabditis
      {
        
        mean_H2_data_Vr_Caenorhabditis <- rbind(mean(h2_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled),mean(h2_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled),mean(h2_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled),mean(h2_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled),mean(h2_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled),mean(h2_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(mean_H2_data_Vr_Caenorhabditis) <- c("mean")
        median_H2_data_Vr_Caenorhabditis <- rbind(median(h2_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled),median(h2_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled),median(h2_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled),median(h2_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled),median(h2_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled),median(h2_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(median_H2_data_Vr_Caenorhabditis) <- c("median")
        posterior.mode_H2_data_Vr_Caenorhabditis <- rbind(posterior.mode(as.mcmc(h2_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled)),posterior.mode(as.mcmc(h2_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled)),posterior.mode(as.mcmc(h2_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled)),posterior.mode(as.mcmc(h2_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled)),posterior.mode(as.mcmc(h2_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled)),posterior.mode(as.mcmc(h2_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled)))
        colnames(posterior.mode_H2_data_Vr_Caenorhabditis) <- c("posterior.mode")
        HPDinterval_0.95_H2_data_Vr_Caenorhabditis <- rbind(HPDinterval(as.mcmc(h2_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled)),HPDinterval(as.mcmc(h2_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled)),HPDinterval(as.mcmc(h2_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled)),HPDinterval(as.mcmc(h2_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled)),HPDinterval(as.mcmc(h2_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled)),HPDinterval(as.mcmc(h2_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled)))
        colnames(HPDinterval_0.95_H2_data_Vr_Caenorhabditis) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_H2_data_Vr_Caenorhabditis <- rbind(HPDinterval(as.mcmc(h2_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled),prob=.83),HPDinterval(as.mcmc(h2_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled),prob=.83),HPDinterval(as.mcmc(h2_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(h2_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(h2_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(h2_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled),prob=.83))
        colnames(HPDinterval_0.83_H2_data_Vr_Caenorhabditis) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_H2_data_Vr_Caenorhabditis <- rbind(effectiveSize(h2_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled),effectiveSize(h2_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled),effectiveSize(h2_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled),effectiveSize(h2_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled),effectiveSize(h2_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled),effectiveSize(h2_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(effectiveSize_H2_data_Vr_Caenorhabditis) <- c("effectiveSize")
        H2_data_Vr_Caenorhabditis <- cbind.data.frame(mean_H2_data_Vr_Caenorhabditis,median_H2_data_Vr_Caenorhabditis,posterior.mode_H2_data_Vr_Caenorhabditis,HPDinterval_0.95_H2_data_Vr_Caenorhabditis,HPDinterval_0.83_H2_data_Vr_Caenorhabditis,effectiveSize_H2_data_Vr_Caenorhabditis)
        rownames(H2_data_Vr_Caenorhabditis) <- c("h2_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled","h2_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled","h2_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled","h2_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled","h2_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled","h2_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled")
        H2_data_Vr_Caenorhabditis <- cbind(Models = rownames(H2_data_Vr_Caenorhabditis),H2_data_Vr_Caenorhabditis)
        rownames(H2_data_Vr_Caenorhabditis) <- NULL
        H2_data_Vr_Caenorhabditis$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        H2_data_Vr_Caenorhabditis$Pnp_fate <- c("SS","SS","wt","wt","wt","SS")
        H2_data_Vr_Caenorhabditis$Measure <- c("H2","H2","H2","H2","H2","H2")
        H2_data_Vr_Caenorhabditis$Scale <- c("data","data","data","data","data","data")
        H2_data_Vr_Caenorhabditis$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        H2_data_Vr_Caenorhabditis
      }
      
      #Summary Evol_data_Vr_Caenorhabditis
      {
        
        mean_Evol_data_Vr_Caenorhabditis <- rbind(mean(Evol_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled),mean(Evol_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled),mean(Evol_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled),mean(Evol_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled),mean(Evol_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled),mean(Evol_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(mean_Evol_data_Vr_Caenorhabditis) <- c("mean")
        median_Evol_data_Vr_Caenorhabditis <- rbind(median(Evol_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled),median(Evol_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled),median(Evol_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled),median(Evol_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled),median(Evol_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled),median(Evol_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(median_Evol_data_Vr_Caenorhabditis) <- c("median")
        posterior.mode_Evol_data_Vr_Caenorhabditis <- rbind(posterior.mode(as.mcmc(Evol_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled)),posterior.mode(as.mcmc(Evol_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled)),posterior.mode(as.mcmc(Evol_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled)),posterior.mode(as.mcmc(Evol_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled)),posterior.mode(as.mcmc(Evol_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled)),posterior.mode(as.mcmc(Evol_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled)))
        colnames(posterior.mode_Evol_data_Vr_Caenorhabditis) <- c("posterior.mode")
        HPDinterval_0.95_Evol_data_Vr_Caenorhabditis <- rbind(HPDinterval(as.mcmc(Evol_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled)),HPDinterval(as.mcmc(Evol_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled)),HPDinterval(as.mcmc(Evol_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled)),HPDinterval(as.mcmc(Evol_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled)),HPDinterval(as.mcmc(Evol_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled)),HPDinterval(as.mcmc(Evol_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled)))
        colnames(HPDinterval_0.95_Evol_data_Vr_Caenorhabditis) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_Evol_data_Vr_Caenorhabditis <- rbind(HPDinterval(as.mcmc(Evol_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled),prob=.83),HPDinterval(as.mcmc(Evol_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled),prob=.83),HPDinterval(as.mcmc(Evol_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(Evol_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(Evol_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(Evol_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled),prob=.83))
        colnames(HPDinterval_0.83_Evol_data_Vr_Caenorhabditis) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_Evol_data_Vr_Caenorhabditis <- rbind(effectiveSize(Evol_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled),effectiveSize(Evol_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled),effectiveSize(Evol_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled),effectiveSize(Evol_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled),effectiveSize(Evol_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled),effectiveSize(Evol_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(effectiveSize_Evol_data_Vr_Caenorhabditis) <- c("effectiveSize")
        Evol_data_Vr_Caenorhabditis <- cbind.data.frame(mean_Evol_data_Vr_Caenorhabditis,median_Evol_data_Vr_Caenorhabditis,posterior.mode_Evol_data_Vr_Caenorhabditis,HPDinterval_0.95_Evol_data_Vr_Caenorhabditis,HPDinterval_0.83_Evol_data_Vr_Caenorhabditis,effectiveSize_Evol_data_Vr_Caenorhabditis)
        rownames(Evol_data_Vr_Caenorhabditis) <- c("Evol_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled","Evol_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled","Evol_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled","Evol_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled","Evol_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled","Evol_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled")
        Evol_data_Vr_Caenorhabditis <- cbind(Models = rownames(Evol_data_Vr_Caenorhabditis),Evol_data_Vr_Caenorhabditis)
        rownames(Evol_data_Vr_Caenorhabditis) <- NULL
        Evol_data_Vr_Caenorhabditis$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        Evol_data_Vr_Caenorhabditis$Pnp_fate <- c("SS","SS","wt","wt","wt","SS")
        Evol_data_Vr_Caenorhabditis$Measure <- c("Evol","Evol","Evol","Evol","Evol","Evol")
        Evol_data_Vr_Caenorhabditis$Scale <- c("data","data","data","data","data","data")
        Evol_data_Vr_Caenorhabditis$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        Evol_data_Vr_Caenorhabditis
      }
      
      #Summary va_data_Vr_NotPhylo_Caenorhabditis
      {
        
        mean_va_data_Vr_NotPhylo_Caenorhabditis <- rbind(mean(va_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30)),mean(va_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30)),mean(va_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30)),mean(va_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30)),mean(va_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30)),mean(va_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30)))
        colnames(mean_va_data_Vr_NotPhylo_Caenorhabditis) <- c("mean")
        median_va_data_Vr_NotPhylo_Caenorhabditis <- rbind(median(va_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30)),median(va_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30)),median(va_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30)),median(va_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30)),median(va_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30)),median(va_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30)))
        colnames(median_va_data_Vr_NotPhylo_Caenorhabditis) <- c("median")
        posterior.mode_va_data_Vr_NotPhylo_Caenorhabditis <- rbind(posterior.mode(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30))),posterior.mode(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30))),posterior.mode(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30))),posterior.mode(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30))),posterior.mode(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30))),posterior.mode(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30))))
        colnames(posterior.mode_va_data_Vr_NotPhylo_Caenorhabditis) <- c("posterior.mode")
        HPDinterval_0.95_va_data_Vr_NotPhylo_Caenorhabditis <- rbind(HPDinterval(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30))),HPDinterval(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30))),HPDinterval(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30))),HPDinterval(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30))),HPDinterval(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30))),HPDinterval(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30))))
        colnames(HPDinterval_0.95_va_data_Vr_NotPhylo_Caenorhabditis) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_va_data_Vr_NotPhylo_Caenorhabditis <- rbind(HPDinterval(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled/(2*106.30)),prob=.83),HPDinterval(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled/(2*106.30)),prob=.83),HPDinterval(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled/(2*106.30)),prob=.83),HPDinterval(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled/(2*106.30)),prob=.83),HPDinterval(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled/(2*106.30)),prob=.83),HPDinterval(as.mcmc(va_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled/(2*106.30)),prob=.83))
        colnames(HPDinterval_0.83_va_data_Vr_NotPhylo_Caenorhabditis) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_va_data_Vr_NotPhylo_Caenorhabditis <- rbind(effectiveSize(va_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled),effectiveSize(va_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled),effectiveSize(va_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled),effectiveSize(va_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled),effectiveSize(va_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled),effectiveSize(va_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(effectiveSize_va_data_Vr_NotPhylo_Caenorhabditis) <- c("effectiveSize")
        va_data_Vr_NotPhylo_Caenorhabditis <- cbind.data.frame(mean_va_data_Vr_NotPhylo_Caenorhabditis,median_va_data_Vr_NotPhylo_Caenorhabditis,posterior.mode_va_data_Vr_NotPhylo_Caenorhabditis,HPDinterval_0.95_va_data_Vr_NotPhylo_Caenorhabditis,HPDinterval_0.83_va_data_Vr_NotPhylo_Caenorhabditis,effectiveSize_va_data_Vr_NotPhylo_Caenorhabditis)
        rownames(va_data_Vr_NotPhylo_Caenorhabditis) <- c("va_data_Vr_NotPhylo_Caenorhabditis_P3p_SS_mod3_scaled","va_data_Vr_NotPhylo_Caenorhabditis_P4p_SS_mod3_scaled","va_data_Vr_NotPhylo_Caenorhabditis_P5p_wt_mod3_scaled","va_data_Vr_NotPhylo_Caenorhabditis_P6p_wt_mod3_scaled","va_data_Vr_NotPhylo_Caenorhabditis_P7p_wt_mod3_scaled","va_data_Vr_NotPhylo_Caenorhabditis_P8p_SS_mod3_scaled")
        va_data_Vr_NotPhylo_Caenorhabditis <- cbind(Models = rownames(va_data_Vr_NotPhylo_Caenorhabditis),va_data_Vr_NotPhylo_Caenorhabditis)
        rownames(va_data_Vr_NotPhylo_Caenorhabditis) <- NULL
        va_data_Vr_NotPhylo_Caenorhabditis$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        va_data_Vr_NotPhylo_Caenorhabditis$Pnp_fate <- c("SS","SS","wt","wt","wt","SS")
        va_data_Vr_NotPhylo_Caenorhabditis$Measure <- c("va_NotPhylo","va_NotPhylo","va_NotPhylo","va_NotPhylo","va_NotPhylo","va_NotPhylo")
        va_data_Vr_NotPhylo_Caenorhabditis$Scale <- c("data","data","data","data","data","data")
        va_data_Vr_NotPhylo_Caenorhabditis$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        va_data_Vr_NotPhylo_Caenorhabditis
      }
      
      #Summary trait_mean_data_Vr_Caenorhabditis
      {
        
        mean_trait_mean_data_Vr_Caenorhabditis <- rbind(mean(trait_mean_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled),mean(trait_mean_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled),mean(trait_mean_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled),mean(trait_mean_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled),mean(trait_mean_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled),mean(trait_mean_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(mean_trait_mean_data_Vr_Caenorhabditis) <- c("mean")
        median_trait_mean_data_Vr_Caenorhabditis <- rbind(median(trait_mean_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled),median(trait_mean_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled),median(trait_mean_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled),median(trait_mean_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled),median(trait_mean_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled),median(trait_mean_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(median_trait_mean_data_Vr_Caenorhabditis) <- c("median")
        posterior.mode_trait_mean_data_Vr_Caenorhabditis <- rbind(posterior.mode(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled)),posterior.mode(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled)),posterior.mode(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled)),posterior.mode(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled)),posterior.mode(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled)),posterior.mode(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled)))
        colnames(posterior.mode_trait_mean_data_Vr_Caenorhabditis) <- c("posterior.mode")
        HPDinterval_0.95_trait_mean_data_Vr_Caenorhabditis <- rbind(HPDinterval(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled)),HPDinterval(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled)),HPDinterval(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled)),HPDinterval(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled)),HPDinterval(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled)),HPDinterval(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled)))
        colnames(HPDinterval_0.95_trait_mean_data_Vr_Caenorhabditis) <- c("lowerHPD_0.95","upperHPD_0.95")
        HPDinterval_0.83_trait_mean_data_Vr_Caenorhabditis <- rbind(HPDinterval(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled),prob=.83),HPDinterval(as.mcmc(trait_mean_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled),prob=.83))
        colnames(HPDinterval_0.83_trait_mean_data_Vr_Caenorhabditis) <- c("lowerHPD_0.83","upperHPD_0.83")
        effectiveSize_trait_mean_data_Vr_Caenorhabditis <- rbind(effectiveSize(trait_mean_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled),effectiveSize(trait_mean_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled),effectiveSize(trait_mean_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled),effectiveSize(trait_mean_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled),effectiveSize(trait_mean_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled),effectiveSize(trait_mean_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled))
        colnames(effectiveSize_trait_mean_data_Vr_Caenorhabditis) <- c("effectiveSize")
        trait_mean_data_Vr_Caenorhabditis <- cbind.data.frame(mean_trait_mean_data_Vr_Caenorhabditis,median_trait_mean_data_Vr_Caenorhabditis,posterior.mode_trait_mean_data_Vr_Caenorhabditis,HPDinterval_0.95_trait_mean_data_Vr_Caenorhabditis,HPDinterval_0.83_trait_mean_data_Vr_Caenorhabditis,effectiveSize_trait_mean_data_Vr_Caenorhabditis)
        rownames(trait_mean_data_Vr_Caenorhabditis) <- c("trait_mean_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled","trait_mean_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled","trait_mean_data_Vr_Caenorhabditis_P5p_wt_mod3_scaled","trait_mean_data_Vr_Caenorhabditis_P6p_wt_mod3_scaled","trait_mean_data_Vr_Caenorhabditis_P7p_wt_mod3_scaled","trait_mean_data_Vr_Caenorhabditis_P8p_SS_mod3_scaled")
        trait_mean_data_Vr_Caenorhabditis <- cbind(Models = rownames(trait_mean_data_Vr_Caenorhabditis),trait_mean_data_Vr_Caenorhabditis)
        rownames(trait_mean_data_Vr_Caenorhabditis) <- NULL
        trait_mean_data_Vr_Caenorhabditis$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
        trait_mean_data_Vr_Caenorhabditis$Pnp_fate <- c("SS","SS","wt","wt","wt","SS")
        trait_mean_data_Vr_Caenorhabditis$Measure <- c("trait_mean","trait_mean","trait_mean","trait_mean","trait_mean","trait_mean")
        trait_mean_data_Vr_Caenorhabditis$Scale <- c("data","data","data","data","data","data")
        trait_mean_data_Vr_Caenorhabditis$Variance <- c("Vr","Vr","Vr","Vr","Vr","Vr")
        trait_mean_data_Vr_Caenorhabditis
      }
      
      data_Vr_Caenorhabditis <- rbind.data.frame(va_data_Vr_Caenorhabditis, H2_data_Vr_Caenorhabditis,Evol_data_Vr_Caenorhabditis,va_data_Vr_NotPhylo_Caenorhabditis,trait_mean_data_Vr_Caenorhabditis)
      data_Vr_Caenorhabditis
    } 
    
    
    
    Vr_Caenorhabditis_summary <- rbind.data.frame(liab_Vr_Caenorhabditis, data_Vr_Caenorhabditis)
    View(Vr_Caenorhabditis_summary)
    
    names(Vr_Caenorhabditis_summary)[names(Vr_Caenorhabditis_summary) == "Models"] <- "Model_name"
    dim(Vr_Caenorhabditis_summary)
    Vr_Caenorhabditis_summary$Genus <- rep("Caenorhabditis",60)
    
    write_xlsx(Vr_Caenorhabditis_summary, "Vr_Caenorhabditis_summary_SS.xlsx")
    
    
    
    #remove Vr_Caenorhabditis models 
    {
      remove( Vr_Caenorhabditis_P3p_SS_mod3_scaled)
      remove( Vr_Caenorhabditis_P4p_SS_mod3_scaled)
      remove( Vr_Caenorhabditis_P8p_SS_mod3_scaled)
      remove( Vr_Caenorhabditis_P5p_wt_mod3_scaled)
      remove( Vr_Caenorhabditis_P6p_wt_mod3_scaled)
      remove( Vr_Caenorhabditis_P7p_wt_mod3_scaled)
      
    }
  }
  
  
  
}

## ---- Vr_Caenorhabditis_P3p_divided_P4p ----



#Vr_Caenorhabditis_P3p_divided_P4p_SS_liab
{
  
  va_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled <- va_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled / va_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled
  h2_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled <- h2_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled / h2_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled
  Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled <- Evol_liab_Vr_Caenorhabditis_P3p_SS_mod3_scaled / Evol_liab_Vr_Caenorhabditis_P4p_SS_mod3_scaled
  
  mean_va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS <- rbind(mean(log10(va_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)),mean(log10(h2_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)), mean(log10(Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)))
  colnames(mean_va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS) <- c("mean")
  median_va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS <- rbind(median(log10(va_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)),median(log10(h2_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)), median(log10(Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)))
  colnames(median_va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS) <- c("median")
  posterior.mode_va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS <- rbind(posterior.mode(as.mcmc(log10(va_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled))),posterior.mode(as.mcmc(log10(h2_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled))),posterior.mode(as.mcmc(log10(Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled))))
  colnames(posterior.mode_va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS) <- c("posterior.mode")
  HPDinterval_0.95_va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS <- rbind(HPDinterval(as.mcmc(log10(va_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled))),HPDinterval(as.mcmc(log10(h2_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled))),HPDinterval(as.mcmc(log10(Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled))))
  colnames(HPDinterval_0.95_va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS) <- c("CI_lower_0.95","CI_upper_0.95")
  HPDinterval_0.83_va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS <- rbind(HPDinterval(as.mcmc(log10(va_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)),prob=.83),HPDinterval(as.mcmc(log10(h2_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)),prob=.83),HPDinterval(as.mcmc(log10(Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)),prob=.83))
  colnames(HPDinterval_0.83_va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS) <- c("CI_lower_0.83","CI_upper_0.83")
  va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS <- cbind.data.frame(mean_va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS,median_va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS,posterior.mode_va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS,HPDinterval_0.95_va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS,HPDinterval_0.83_va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS)
  rownames(va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS) <- c("va_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS_log10","h2_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS_log10","Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS_log10")
  va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS <- cbind(Comparisons = rownames(va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS),va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS)
  rownames(va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS) <- NULL
  va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS$Measure <- c("Va_Phylo","H2", "Evol")
  va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS$Pnp_fate <- rep("SS",3)
  va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS$Genus <- rep("Caenorhabditis",3)
  va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS$Scale <- rep("liab",3)
  va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS
  
  
  
}



pdf("va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_summary.pdf")
ggplot(va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS, aes(x=Measure, y= median)) +
  geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
  geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
  geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
  theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
  labs(y = "log10 ( P3.p / P4.p)", title = "Vr_Caenorhabditis_liab_log10(P3p/P4p)")
dev.off() 


#Vr_Caenorhabditis_P3p_divided_P4p_SS_data
{
  
  va_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled <- va_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled / va_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled
  h2_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled <- h2_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled / h2_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled
  Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled <- Evol_data_Vr_Caenorhabditis_P3p_SS_mod3_scaled / Evol_data_Vr_Caenorhabditis_P4p_SS_mod3_scaled
  
  mean_va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS <- rbind(mean(log10(va_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)),mean(log10(h2_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)), mean(log10(Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)))
  colnames(mean_va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS) <- c("mean")
  median_va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS <- rbind(median(log10(va_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)),median(log10(h2_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)), median(log10(Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)))
  colnames(median_va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS) <- c("median")
  posterior.mode_va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS <- rbind(posterior.mode(as.mcmc(log10(va_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled))),posterior.mode(as.mcmc(log10(h2_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled))),posterior.mode(as.mcmc(log10(Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled))))
  colnames(posterior.mode_va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS) <- c("posterior.mode")
  HPDinterval_0.95_va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS <- rbind(HPDinterval(as.mcmc(log10(va_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled))),HPDinterval(as.mcmc(log10(h2_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled))),HPDinterval(as.mcmc(log10(Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled))))
  colnames(HPDinterval_0.95_va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS) <- c("CI_lower_0.95","CI_upper_0.95")
  HPDinterval_0.83_va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS <- rbind(HPDinterval(as.mcmc(log10(va_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)),prob=.83),HPDinterval(as.mcmc(log10(h2_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)),prob=.83),HPDinterval(as.mcmc(log10(Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_mod3_scaled)),prob=.83))
  colnames(HPDinterval_0.83_va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS) <- c("CI_lower_0.83","CI_upper_0.83")
  va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS <- cbind.data.frame(mean_va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS,median_va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS,posterior.mode_va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS,HPDinterval_0.95_va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS,HPDinterval_0.83_va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS)
  rownames(va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS) <- c("va_data_Vr_Caenorhabditis_P3p_divided_P4p_SS_log10","h2_data_Vr_Caenorhabditis_P3p_divided_P4p_SS_log10","Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS_log10")
  va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS <- cbind(Comparisons = rownames(va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS),va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS)
  rownames(va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS) <- NULL
  va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS$Measure <- c("Va_Phylo","H2", "Evol")
  va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS$Pnp_fate <- rep("SS",3)
  va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS$Genus <- rep("Caenorhabditis",3)
  va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS$Scale <- rep("data",3)
  va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS
  
}


pdf("va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_summary.pdf")
ggplot(va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS, aes(x=Measure, y= median)) +
  geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
  geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
  geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
  theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
  labs(y = "log10 ( P3.p / P4.p)", title = "Vr_Caenorhabditis_data_log10(P3p/P4p)")
dev.off() 



va_h2_Evol_Vr_Caenorhabditis_P3p_divided_P4p_SS_summary <- rbind.data.frame(va_h2_Evol_liab_Vr_Caenorhabditis_P3p_divided_P4p_SS,va_h2_Evol_data_Vr_Caenorhabditis_P3p_divided_P4p_SS)
va_h2_Evol_Vr_Caenorhabditis_P3p_divided_P4p_SS_summary


pdf("va_h2_Evol_Vr_Caenorhabditis_P3p_divided_P4p_SS_summary.pdf")
ggplot(va_h2_Evol_Vr_Caenorhabditis_P3p_divided_P4p_SS_summary, aes(x=Measure, y= median)) +
  geom_bar(stat="identity", position=position_dodge(), linewidth = 1.5, fill = "grey40")+
  geom_errorbar(aes(ymin=CI_lower_0.95, ymax=CI_upper_0.95), width=0.1, linewidth=0.5) + 
  geom_linerange(aes(ymin=CI_lower_0.83, ymax=CI_upper_0.83), color="red", linewidth=1.5) +
  theme_bw() +  geom_hline(yintercept=0, color = "black", linewidth = 1.5) + theme(aspect.ratio=1)+
  facet_grid(rows= vars(Scale))+
  labs(y = "log10 ( P3.p / P4.p)", title = "Vr_Caenorhabditis_log10(P3p/P4p)")
dev.off() 


write_xlsx(va_h2_Evol_Vr_Caenorhabditis_P3p_divided_P4p_SS_summary, "va_h2_Evol_Vr_Caenorhabditis_P3p_divided_P4p_SS_summary.xlsx")















#Vm_Oscheius_isolate_TreatmentMA_liab: to extract the pMCMC
library(emmeans)

##CEW1
{
CEW1_Vm_P3p_SSSS_mod_summary_table <-  summary(CEW1_Vm_P3p_SSSS_mod) 
CEW1_Vm_P4p_SSSS_mod_summary_table <-  summary(CEW1_Vm_P4p_SSSS_mod) 
CEW1_Vm_P5p_wt_mod_summary_table <-  summary(CEW1_Vm_P5p_wt_mod) 
CEW1_Vm_P6p_wt_mod_summary_table <-  summary(CEW1_Vm_P6p_wt_mod) 
CEW1_Vm_P7p_wt_mod_summary_table <-  summary(CEW1_Vm_P7p_wt_mod) 
CEW1_Vm_P8p_SSSS_mod_summary_table <-  summary(CEW1_Vm_P8p_SSSS_mod) 


CEW1_Vm_SSSS_vulva_summary_table_TreatmentMA_liab  <- t(cbind.data.frame(CEW1_Vm_P3p_SSSS_mod_summary_table$solutions[5,],CEW1_Vm_P4p_SSSS_mod_summary_table$solutions[5,], CEW1_Vm_P5p_wt_mod_summary_table$solutions[5,], CEW1_Vm_P6p_wt_mod_summary_table$solutions[5,], CEW1_Vm_P7p_wt_mod_summary_table$solutions[5,], CEW1_Vm_P8p_SSSS_mod_summary_table$solutions[5,])) 
rownames(CEW1_Vm_SSSS_vulva_summary_table_TreatmentMA_liab) <- c("CEW1_Vm_P3p_SSSS_TreatmentMA", "CEW1_Vm_P4p_SSSS_TreatmentMA", "CEW1_Vm_P5p_wt_TreatmentMA", "CEW1_Vm_P6p_wt_TreatmentMA", "CEW1_Vm_P7p_wt_TreatmentMA", "CEW1_Vm_P8p_SSSS_TreatmentMA")
CEW1_Vm_SSSS_vulva_summary_table_TreatmentMA_liab <- cbind.data.frame(Models = rownames(CEW1_Vm_SSSS_vulva_summary_table_TreatmentMA_liab),CEW1_Vm_SSSS_vulva_summary_table_TreatmentMA_liab)
rownames(CEW1_Vm_SSSS_vulva_summary_table_TreatmentMA_liab) <- NULL
CEW1_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
CEW1_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Pnp_fate <- c("SSSS","SSSS","wt","wt","wt","SSSS")
CEW1_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Ancestral <- rep("CEW1",6)
CEW1_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Species <- rep("O.tipulae",6)
CEW1_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Genus <- rep("Oscheius",6)
CEW1_Vm_SSSS_vulva_summary_table_TreatmentMA_liab

}

##JU178
{
  JU178_Vm_P3p_SSSS_mod_summary_table <-  summary(JU178_Vm_P3p_SSSS_mod) 
  JU178_Vm_P4p_SSSS_mod_summary_table <-  summary(JU178_Vm_P4p_SSSS_mod) 
  JU178_Vm_P5p_wt_mod_summary_table <-  summary(JU178_Vm_P5p_wt_mod) 
  JU178_Vm_P6p_wt_mod_summary_table <-  summary(JU178_Vm_P6p_wt_mod) 
  JU178_Vm_P7p_wt_mod_summary_table <-  summary(JU178_Vm_P7p_wt_mod) 
  JU178_Vm_P8p_SSSS_mod_summary_table <-  summary(JU178_Vm_P8p_SSSS_mod) 
  
  
  JU178_Vm_SSSS_vulva_summary_table_TreatmentMA_liab  <- t(cbind.data.frame(JU178_Vm_P3p_SSSS_mod_summary_table$solutions[5,],JU178_Vm_P4p_SSSS_mod_summary_table$solutions[5,], JU178_Vm_P5p_wt_mod_summary_table$solutions[5,], JU178_Vm_P6p_wt_mod_summary_table$solutions[5,], JU178_Vm_P7p_wt_mod_summary_table$solutions[5,], JU178_Vm_P8p_SSSS_mod_summary_table$solutions[5,])) 
  rownames(JU178_Vm_SSSS_vulva_summary_table_TreatmentMA_liab) <- c("JU178_Vm_P3p_SSSS_TreatmentMA", "JU178_Vm_P4p_SSSS_TreatmentMA", "JU178_Vm_P5p_wt_TreatmentMA", "JU178_Vm_P6p_wt_TreatmentMA", "JU178_Vm_P7p_wt_TreatmentMA", "JU178_Vm_P8p_SSSS_TreatmentMA")
  JU178_Vm_SSSS_vulva_summary_table_TreatmentMA_liab <- cbind.data.frame(Models = rownames(JU178_Vm_SSSS_vulva_summary_table_TreatmentMA_liab),JU178_Vm_SSSS_vulva_summary_table_TreatmentMA_liab)
  rownames(JU178_Vm_SSSS_vulva_summary_table_TreatmentMA_liab) <- NULL
  JU178_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
  JU178_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Pnp_fate <- c("SSSS","SSSS","wt","wt","wt","SSSS")
  JU178_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Ancestral <- rep("JU178",6)
  JU178_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Species <- rep("O.tipulae",6)
  JU178_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Genus <- rep("Oscheius",6)
  JU178_Vm_SSSS_vulva_summary_table_TreatmentMA_liab
}

##PS2068
{
  PS2068_Vm_P3p_SSSS_mod_summary_table <-  summary(PS2068_Vm_P3p_SSSS_mod) 
  PS2068_Vm_P4p_SSSS_mod_summary_table <-  summary(PS2068_Vm_P4p_SSSS_mod) 
  PS2068_Vm_P5p_wt_mod_summary_table <-  summary(PS2068_Vm_P5p_wt_mod) 
  PS2068_Vm_P6p_wt_mod_summary_table <-  summary(PS2068_Vm_P6p_wt_mod) 
  PS2068_Vm_P7p_wt_mod_summary_table <-  summary(PS2068_Vm_P7p_wt_mod) 
  PS2068_Vm_P8p_SSSS_mod_summary_table <-  summary(PS2068_Vm_P8p_SSSS_mod) 
  
  
  PS2068_Vm_SSSS_vulva_summary_table_TreatmentMA_liab  <- t(cbind.data.frame(PS2068_Vm_P3p_SSSS_mod_summary_table$solutions[5,],PS2068_Vm_P4p_SSSS_mod_summary_table$solutions[5,], PS2068_Vm_P5p_wt_mod_summary_table$solutions[5,], PS2068_Vm_P6p_wt_mod_summary_table$solutions[5,], PS2068_Vm_P7p_wt_mod_summary_table$solutions[5,], PS2068_Vm_P8p_SSSS_mod_summary_table$solutions[5,])) 
  rownames(PS2068_Vm_SSSS_vulva_summary_table_TreatmentMA_liab) <- c("PS2068_Vm_P3p_SSSS_TreatmentMA", "PS2068_Vm_P4p_SSSS_TreatmentMA", "PS2068_Vm_P5p_wt_TreatmentMA", "PS2068_Vm_P6p_wt_TreatmentMA", "PS2068_Vm_P7p_wt_TreatmentMA", "PS2068_Vm_P8p_SSSS_TreatmentMA")
  PS2068_Vm_SSSS_vulva_summary_table_TreatmentMA_liab <- cbind.data.frame(Models = rownames(PS2068_Vm_SSSS_vulva_summary_table_TreatmentMA_liab),PS2068_Vm_SSSS_vulva_summary_table_TreatmentMA_liab)
  rownames(PS2068_Vm_SSSS_vulva_summary_table_TreatmentMA_liab) <- NULL
  PS2068_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
  PS2068_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Pnp_fate <- c("SSSS","SSSS","wt","wt","wt","SSSS")
  PS2068_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Ancestral <- rep("PS2068",6)
  PS2068_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Species <- rep("O.onirici",6)
  PS2068_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Genus <- rep("Oscheius",6)
  PS2068_Vm_SSSS_vulva_summary_table_TreatmentMA_liab
}

##JU77
{
  JU77_Vm_P3p_SSSS_mod_summary_table <-  summary(JU77_Vm_P3p_SSSS_mod) 
  JU77_Vm_P4p_SSSS_mod_summary_table <-  summary(JU77_Vm_P4p_SSSS_mod) 
  JU77_Vm_P5p_wt_mod_summary_table <-  summary(JU77_Vm_P5p_wt_mod) 
  JU77_Vm_P6p_wt_mod_summary_table <-  summary(JU77_Vm_P6p_wt_mod) 
  JU77_Vm_P7p_wt_mod_summary_table <-  summary(JU77_Vm_P7p_wt_mod) 
  JU77_Vm_P8p_SSSS_mod_summary_table <-  summary(JU77_Vm_P8p_SSSS_mod) 
  
  
  JU77_Vm_SSSS_vulva_summary_table_TreatmentMA_liab  <- t(cbind.data.frame(JU77_Vm_P3p_SSSS_mod_summary_table$solutions[5,],JU77_Vm_P4p_SSSS_mod_summary_table$solutions[5,], JU77_Vm_P5p_wt_mod_summary_table$solutions[5,], JU77_Vm_P6p_wt_mod_summary_table$solutions[5,], JU77_Vm_P7p_wt_mod_summary_table$solutions[5,], JU77_Vm_P8p_SSSS_mod_summary_table$solutions[5,])) 
  rownames(JU77_Vm_SSSS_vulva_summary_table_TreatmentMA_liab) <- c("JU77_Vm_P3p_SSSS_TreatmentMA", "JU77_Vm_P4p_SSSS_TreatmentMA", "JU77_Vm_P5p_wt_TreatmentMA", "JU77_Vm_P6p_wt_TreatmentMA", "JU77_Vm_P7p_wt_TreatmentMA", "JU77_Vm_P8p_SSSS_TreatmentMA")
  JU77_Vm_SSSS_vulva_summary_table_TreatmentMA_liab <- cbind.data.frame(Models = rownames(JU77_Vm_SSSS_vulva_summary_table_TreatmentMA_liab),JU77_Vm_SSSS_vulva_summary_table_TreatmentMA_liab)
  rownames(JU77_Vm_SSSS_vulva_summary_table_TreatmentMA_liab) <- NULL
  JU77_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
  JU77_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Pnp_fate <- c("SSSS","SSSS","wt","wt","wt","SSSS")
  JU77_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Ancestral <- rep("JU77",6)
  JU77_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Species <- rep("O.onirici",6)
  JU77_Vm_SSSS_vulva_summary_table_TreatmentMA_liab$Genus <- rep("Oscheius",6)
  JU77_Vm_SSSS_vulva_summary_table_TreatmentMA_liab
}


Vm_Oscheius_isolate_TreatmentMA_liab <-  rbind.data.frame(CEW1_Vm_SSSS_vulva_summary_table_TreatmentMA_liab,JU178_Vm_SSSS_vulva_summary_table_TreatmentMA_liab,PS2068_Vm_SSSS_vulva_summary_table_TreatmentMA_liab,JU77_Vm_SSSS_vulva_summary_table_TreatmentMA_liab)
View(Vm_Oscheius_isolate_TreatmentMA_liab)

write_xlsx(Vm_Oscheius_isolate_TreatmentMA_liab, "Vm_Oscheius_isolate_TreatmentMA_liab.xlsx")

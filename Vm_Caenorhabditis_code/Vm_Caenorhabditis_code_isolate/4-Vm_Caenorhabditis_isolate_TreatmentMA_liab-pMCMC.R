#Vm_Caenorhabditis_isolate_TreatmentMA_liab: to extract the pMCMC
library(emmeans)

##JU1200
{
  JU1200_Vm_P3p_SS_mod_summary_table <-  summary(JU1200_Vm_P3p_SS_mod) 
  JU1200_Vm_P4p_SS_mod_summary_table <-  summary(JU1200_Vm_P4p_SS_mod) 
  JU1200_Vm_P5p_wt_mod_summary_table <-  summary(JU1200_Vm_P5p_wt_mod) 
  JU1200_Vm_P6p_wt_mod_summary_table <-  summary(JU1200_Vm_P6p_wt_mod) 
  JU1200_Vm_P7p_wt_mod_summary_table <-  summary(JU1200_Vm_P7p_wt_mod) 
  JU1200_Vm_P8p_SS_mod_summary_table <-  summary(JU1200_Vm_P8p_SS_mod) 
  
  
  JU1200_Vm_SS_vulva_summary_table_TreatmentMA_liab  <- t(cbind.data.frame(JU1200_Vm_P3p_SS_mod_summary_table$solutions[5,],JU1200_Vm_P4p_SS_mod_summary_table$solutions[5,], JU1200_Vm_P5p_wt_mod_summary_table$solutions[5,], JU1200_Vm_P6p_wt_mod_summary_table$solutions[5,], JU1200_Vm_P7p_wt_mod_summary_table$solutions[5,], JU1200_Vm_P8p_SS_mod_summary_table$solutions[5,])) 
  rownames(JU1200_Vm_SS_vulva_summary_table_TreatmentMA_liab) <- c("JU1200_Vm_P3p_SS_TreatmentMA", "JU1200_Vm_P4p_SS_TreatmentMA", "JU1200_Vm_P5p_wt_TreatmentMA", "JU1200_Vm_P6p_wt_TreatmentMA", "JU1200_Vm_P7p_wt_TreatmentMA", "JU1200_Vm_P8p_SS_TreatmentMA")
  JU1200_Vm_SS_vulva_summary_table_TreatmentMA_liab <- cbind.data.frame(Models = rownames(JU1200_Vm_SS_vulva_summary_table_TreatmentMA_liab),JU1200_Vm_SS_vulva_summary_table_TreatmentMA_liab)
  rownames(JU1200_Vm_SS_vulva_summary_table_TreatmentMA_liab) <- NULL
  JU1200_Vm_SS_vulva_summary_table_TreatmentMA_liab$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
  JU1200_Vm_SS_vulva_summary_table_TreatmentMA_liab$Pnp_fate <- c("SS","SS","wt","wt","wt","SS")
  JU1200_Vm_SS_vulva_summary_table_TreatmentMA_liab$Ancestral <- rep("JU1200",6)
  JU1200_Vm_SS_vulva_summary_table_TreatmentMA_liab$Species <- rep("C.elegans",6)
  JU1200_Vm_SS_vulva_summary_table_TreatmentMA_liab$Genus <- rep("Caenorhabditis",6)
  JU1200_Vm_SS_vulva_summary_table_TreatmentMA_liab
  
}

##PB306
{
  PB306_Vm_P3p_SS_mod_summary_table <-  summary(PB306_Vm_P3p_SS_mod) 
  PB306_Vm_P4p_SS_mod_summary_table <-  summary(PB306_Vm_P4p_SS_mod) 
  PB306_Vm_P5p_wt_mod_summary_table <-  summary(PB306_Vm_P5p_wt_mod) 
  PB306_Vm_P6p_wt_mod_summary_table <-  summary(PB306_Vm_P6p_wt_mod) 
  PB306_Vm_P7p_wt_mod_summary_table <-  summary(PB306_Vm_P7p_wt_mod) 
  PB306_Vm_P8p_SS_mod_summary_table <-  summary(PB306_Vm_P8p_SS_mod) 
  
  
  PB306_Vm_SS_vulva_summary_table_TreatmentMA_liab  <- t(cbind.data.frame(PB306_Vm_P3p_SS_mod_summary_table$solutions[5,],PB306_Vm_P4p_SS_mod_summary_table$solutions[5,], PB306_Vm_P5p_wt_mod_summary_table$solutions[5,], PB306_Vm_P6p_wt_mod_summary_table$solutions[5,], PB306_Vm_P7p_wt_mod_summary_table$solutions[5,], PB306_Vm_P8p_SS_mod_summary_table$solutions[5,])) 
  rownames(PB306_Vm_SS_vulva_summary_table_TreatmentMA_liab) <- c("PB306_Vm_P3p_SS_TreatmentMA", "PB306_Vm_P4p_SS_TreatmentMA", "PB306_Vm_P5p_wt_TreatmentMA", "PB306_Vm_P6p_wt_TreatmentMA", "PB306_Vm_P7p_wt_TreatmentMA", "PB306_Vm_P8p_SS_TreatmentMA")
  PB306_Vm_SS_vulva_summary_table_TreatmentMA_liab <- cbind.data.frame(Models = rownames(PB306_Vm_SS_vulva_summary_table_TreatmentMA_liab),PB306_Vm_SS_vulva_summary_table_TreatmentMA_liab)
  rownames(PB306_Vm_SS_vulva_summary_table_TreatmentMA_liab) <- NULL
  PB306_Vm_SS_vulva_summary_table_TreatmentMA_liab$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
  PB306_Vm_SS_vulva_summary_table_TreatmentMA_liab$Pnp_fate <- c("SS","SS","wt","wt","wt","SS")
  PB306_Vm_SS_vulva_summary_table_TreatmentMA_liab$Ancestral <- rep("PB306",6)
  PB306_Vm_SS_vulva_summary_table_TreatmentMA_liab$Species <- rep("C.elegans",6)
  PB306_Vm_SS_vulva_summary_table_TreatmentMA_liab$Genus <- rep("Caenorhabditis",6)
  PB306_Vm_SS_vulva_summary_table_TreatmentMA_liab
}

##AF16
{
  AF16_Vm_P3p_SS_mod_summary_table <-  summary(AF16_Vm_P3p_SS_mod) 
  AF16_Vm_P4p_SS_mod_summary_table <-  summary(AF16_Vm_P4p_SS_mod) 
  AF16_Vm_P5p_wt_mod_summary_table <-  summary(AF16_Vm_P5p_wt_mod) 
  AF16_Vm_P6p_wt_mod_summary_table <-  summary(AF16_Vm_P6p_wt_mod) 
  AF16_Vm_P7p_wt_mod_summary_table <-  summary(AF16_Vm_P7p_wt_mod) 
  AF16_Vm_P8p_SS_mod_summary_table <-  summary(AF16_Vm_P8p_SS_mod) 
  
  
  AF16_Vm_SS_vulva_summary_table_TreatmentMA_liab  <- t(cbind.data.frame(AF16_Vm_P3p_SS_mod_summary_table$solutions[5,],AF16_Vm_P4p_SS_mod_summary_table$solutions[5,], AF16_Vm_P5p_wt_mod_summary_table$solutions[5,], AF16_Vm_P6p_wt_mod_summary_table$solutions[5,], AF16_Vm_P7p_wt_mod_summary_table$solutions[5,], AF16_Vm_P8p_SS_mod_summary_table$solutions[5,])) 
  rownames(AF16_Vm_SS_vulva_summary_table_TreatmentMA_liab) <- c("AF16_Vm_P3p_SS_TreatmentMA", "AF16_Vm_P4p_SS_TreatmentMA", "AF16_Vm_P5p_wt_TreatmentMA", "AF16_Vm_P6p_wt_TreatmentMA", "AF16_Vm_P7p_wt_TreatmentMA", "AF16_Vm_P8p_SS_TreatmentMA")
  AF16_Vm_SS_vulva_summary_table_TreatmentMA_liab <- cbind.data.frame(Models = rownames(AF16_Vm_SS_vulva_summary_table_TreatmentMA_liab),AF16_Vm_SS_vulva_summary_table_TreatmentMA_liab)
  rownames(AF16_Vm_SS_vulva_summary_table_TreatmentMA_liab) <- NULL
  AF16_Vm_SS_vulva_summary_table_TreatmentMA_liab$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
  AF16_Vm_SS_vulva_summary_table_TreatmentMA_liab$Pnp_fate <- c("SS","SS","wt","wt","wt","SS")
  AF16_Vm_SS_vulva_summary_table_TreatmentMA_liab$Ancestral <- rep("AF16",6)
  AF16_Vm_SS_vulva_summary_table_TreatmentMA_liab$Species <- rep("C.briggsae",6)
  AF16_Vm_SS_vulva_summary_table_TreatmentMA_liab$Genus <- rep("Caenorhabditis",6)
  AF16_Vm_SS_vulva_summary_table_TreatmentMA_liab
}

##PB800
{
  PB800_Vm_P3p_SS_mod_summary_table <-  summary(PB800_Vm_P3p_SS_mod) 
  PB800_Vm_P4p_SS_mod_summary_table <-  summary(PB800_Vm_P4p_SS_mod) 
  PB800_Vm_P5p_wt_mod_summary_table <-  summary(PB800_Vm_P5p_wt_mod) 
  PB800_Vm_P6p_wt_mod_summary_table <-  summary(PB800_Vm_P6p_wt_mod) 
  PB800_Vm_P7p_wt_mod_summary_table <-  summary(PB800_Vm_P7p_wt_mod) 
  PB800_Vm_P8p_SS_mod_summary_table <-  summary(PB800_Vm_P8p_SS_mod) 
  
  
  PB800_Vm_SS_vulva_summary_table_TreatmentMA_liab  <- t(cbind.data.frame(PB800_Vm_P3p_SS_mod_summary_table$solutions[5,],PB800_Vm_P4p_SS_mod_summary_table$solutions[5,], PB800_Vm_P5p_wt_mod_summary_table$solutions[5,], PB800_Vm_P6p_wt_mod_summary_table$solutions[5,], PB800_Vm_P7p_wt_mod_summary_table$solutions[5,], PB800_Vm_P8p_SS_mod_summary_table$solutions[5,])) 
  rownames(PB800_Vm_SS_vulva_summary_table_TreatmentMA_liab) <- c("PB800_Vm_P3p_SS_TreatmentMA", "PB800_Vm_P4p_SS_TreatmentMA", "PB800_Vm_P5p_wt_TreatmentMA", "PB800_Vm_P6p_wt_TreatmentMA", "PB800_Vm_P7p_wt_TreatmentMA", "PB800_Vm_P8p_SS_TreatmentMA")
  PB800_Vm_SS_vulva_summary_table_TreatmentMA_liab <- cbind.data.frame(Models = rownames(PB800_Vm_SS_vulva_summary_table_TreatmentMA_liab),PB800_Vm_SS_vulva_summary_table_TreatmentMA_liab)
  rownames(PB800_Vm_SS_vulva_summary_table_TreatmentMA_liab) <- NULL
  PB800_Vm_SS_vulva_summary_table_TreatmentMA_liab$Pnp <- c("P3.p","P4.p","P5.p","P6.p","P7.p","P8.p")
  PB800_Vm_SS_vulva_summary_table_TreatmentMA_liab$Pnp_fate <- c("SS","SS","wt","wt","wt","SS")
  PB800_Vm_SS_vulva_summary_table_TreatmentMA_liab$Ancestral <- rep("PB800",6)
  PB800_Vm_SS_vulva_summary_table_TreatmentMA_liab$Species <- rep("C.briggsae",6)
  PB800_Vm_SS_vulva_summary_table_TreatmentMA_liab$Genus <- rep("Caenorhabditis",6)
  PB800_Vm_SS_vulva_summary_table_TreatmentMA_liab
}


Vm_Caenorhabditis_isolate_TreatmentMA_liab <-  rbind.data.frame(JU1200_Vm_SS_vulva_summary_table_TreatmentMA_liab,PB306_Vm_SS_vulva_summary_table_TreatmentMA_liab,AF16_Vm_SS_vulva_summary_table_TreatmentMA_liab,PB800_Vm_SS_vulva_summary_table_TreatmentMA_liab)
View(Vm_Caenorhabditis_isolate_TreatmentMA_liab)

write_xlsx(Vm_Caenorhabditis_isolate_TreatmentMA_liab, "Vm_Caenorhabditis_isolate_TreatmentMA_liab.xlsx")

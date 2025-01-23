# get seperate implist for sex

# now incidence data, make implist for four biomarkers seperately (complete info needed)
implist_inci_nfl_f <- as.mids(implist_c[implist_c$ergoid %in% ergoids_inci & implist_c$ergoid %in% ergoids_nfl & implist_c$sex == 1,])
implist_inci_tau_f <- as.mids(implist_c[implist_c$ergoid %in% ergoids_inci & implist_c$ergoid %in% ergoids_tau & implist_c$sex == 1,])
implist_inci_ab40_f <- as.mids(implist_c[implist_c$ergoid %in% ergoids_inci & implist_c$ergoid %in% ergoids_ab40 & implist_c$sex == 1,])
implist_inci_ab42_f <- as.mids(implist_c[implist_c$ergoid %in% ergoids_inci & implist_c$ergoid %in% ergoids_ab42 & implist_c$sex == 1,])
implist_inci_combined_f <- as.mids(implist_c[implist_c$ergoid %in% ergoids_inci & implist_c$ergoid %in% ergoids_nfl & 
                                            implist_c$ergoid %in% ergoids_tau & implist_c$ergoid %in% ergoids_ab40 &
                                            implist_c$ergoid %in% ergoids_ab42 & implist_c$sex == 1,])

# now incidence data, make implist for four biomarkers seperately (complete info needed)
implist_inci_nfl_m <- as.mids(implist_c[implist_c$ergoid %in% ergoids_inci & implist_c$ergoid %in% ergoids_nfl & implist_c$sex == 0,])
implist_inci_tau_m <- as.mids(implist_c[implist_c$ergoid %in% ergoids_inci & implist_c$ergoid %in% ergoids_tau & implist_c$sex == 0,])
implist_inci_ab40_m <- as.mids(implist_c[implist_c$ergoid %in% ergoids_inci & implist_c$ergoid %in% ergoids_ab40 & implist_c$sex == 0,])
implist_inci_ab42_m <- as.mids(implist_c[implist_c$ergoid %in% ergoids_inci & implist_c$ergoid %in% ergoids_ab42 & implist_c$sex == 0,])
implist_inci_combined_m <- as.mids(implist_c[implist_c$ergoid %in% ergoids_inci & implist_c$ergoid %in% ergoids_nfl & 
                                               implist_c$ergoid %in% ergoids_tau & implist_c$ergoid %in% ergoids_ab40 &
                                               implist_c$ergoid %in% ergoids_ab42 & implist_c$sex == 0,])

# first ces-d, make implist for four biomarkers seperately (complete info needed)
implist_cesd_nfl_f <- as.mids(implist_c[implist_c$ergoid %in% ergoids_cesd & implist_c$ergoid %in% ergoids_nfl & implist_c$sex == 1,])
implist_cesd_tau_f <- as.mids(implist_c[implist_c$ergoid %in% ergoids_cesd & implist_c$ergoid %in% ergoids_tau & implist_c$sex == 1,])
implist_cesd_ab40_f <- as.mids(implist_c[implist_c$ergoid %in% ergoids_cesd & implist_c$ergoid %in% ergoids_ab40 & implist_c$sex == 1,])
implist_cesd_ab42_f <- as.mids(implist_c[implist_c$ergoid %in% ergoids_cesd & implist_c$ergoid %in% ergoids_ab42 & implist_c$sex == 1,])
implist_cesd_combined_f <- as.mids(implist_c[implist_c$ergoid %in% ergoids_cesd & implist_c$ergoid %in% ergoids_nfl & 
                                             implist_c$ergoid %in% ergoids_tau & implist_c$ergoid %in% ergoids_ab40 &
                                             implist_c$ergoid %in% ergoids_ab42 & implist_c$sex == 1,])

# first ces-d, make implist for four biomarkers seperately (complete info needed)
implist_cesd_nfl_m <- as.mids(implist_c[implist_c$ergoid %in% ergoids_cesd & implist_c$ergoid %in% ergoids_nfl & implist_c$sex == 0,])
implist_cesd_tau_m <- as.mids(implist_c[implist_c$ergoid %in% ergoids_cesd & implist_c$ergoid %in% ergoids_tau & implist_c$sex == 0,])
implist_cesd_ab40_m <- as.mids(implist_c[implist_c$ergoid %in% ergoids_cesd & implist_c$ergoid %in% ergoids_ab40 & implist_c$sex == 0,])
implist_cesd_ab42_m <- as.mids(implist_c[implist_c$ergoid %in% ergoids_cesd & implist_c$ergoid %in% ergoids_ab42 & implist_c$sex == 0,])
implist_cesd_combined_m <- as.mids(implist_c[implist_c$ergoid %in% ergoids_cesd & implist_c$ergoid %in% ergoids_nfl & 
                                               implist_c$ergoid %in% ergoids_tau & implist_c$ergoid %in% ergoids_ab40 &
                                               implist_c$ergoid %in% ergoids_ab42 & implist_c$sex == 0,])
#-------------------------------------------------------------------------
# CESD ANALYSES
#-------------------------------------------------------------------------

# 1: age, sex and batch number of biomarker analysis. 
# 2: ..., educational level, paid employment, smoking status, alcohol intake, and BMI.

#-INDVIDUAL MODEL---------------------------------------------------------

# make table
table2_f <- table2_m <- as.data.frame(matrix(NA, 4,2))
colnames(table2_f) <- colnames(table2_m) <- c('model 1', 'model 2')
rownames(table2_f) <- rownames(table2_m) <- c('nfl_ind', 'tau_ind', 'ab40_ind', 'ab42_ind')

# put in coefficients males
table2_m[1,] <- coefficients_cesd_individual(implist_cesd_nfl_m, 'log2nfl')
table2_m[2,] <- coefficients_cesd_individual(implist_cesd_tau_m, 'log2tau')
table2_m[3,] <- coefficients_cesd_individual(implist_cesd_ab40_m, 'log2ab40')
table2_m[4,] <- coefficients_cesd_individual(implist_cesd_ab42_m, 'log2ab42')

# put in coefficients females
table2_f[1,] <- coefficients_cesd_individual(implist_cesd_nfl_f, 'log2nfl')
table2_f[2,] <- coefficients_cesd_individual(implist_cesd_tau_f, 'log2tau')
table2_f[3,] <- coefficients_cesd_individual(implist_cesd_ab40_f, 'log2ab40')
table2_f[4,] <- coefficients_cesd_individual(implist_cesd_ab42_f, 'log2ab42')

# write out
write.xlsx(table2_m, 'O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/tables/table2_crosssectional_males.xlsx')
write.xlsx(table2_f, 'O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/tables/table2_crosssectional_females.xlsx')

#-------------------------------------------------------------------------
# INCIDENCE DATA ANALYSES - ALL
#-------------------------------------------------------------------------

#-INDVIDUAL MODEL---------------------------------------------------------

# make table
incidence_individual_all_f <- incidence_individual_mdd_f <- 
  incidence_individual_all_m <- incidence_individual_mdd_m <-
  as.data.frame(matrix(NA, 4,2))
rownames(incidence_individual_all_f) <- rownames(incidence_individual_mdd_f) <- 
  rownames(incidence_individual_all_m) <- rownames(incidence_individual_mdd_m) <- 
  c('nfl_ind', 'tau_ind', 'ab40_ind', 'ab42_ind')
colnames(incidence_individual_all_f) <- colnames(incidence_individual_mdd_f) <- 
  colnames(incidence_individual_all_m) <- colnames(incidence_individual_mdd_m) <- 
  c('model 1', 'model 2')

# put in coefficients - ALL OUTCOMES
incidence_individual_all_f[1,] <- coefficients_inci_individual(implist_inci_nfl_f, 'log2nfl', 'status_all')
incidence_individual_all_f[2,] <- coefficients_inci_individual(implist_inci_tau_f, 'log2tau', 'status_all')
incidence_individual_all_f[3,] <- coefficients_inci_individual(implist_inci_ab40_f, 'log2ab40', 'status_all')
incidence_individual_all_f[4,] <- coefficients_inci_individual(implist_inci_ab42_f, 'log2ab42', 'status_all')

# put in coefficients - MDD OUTCOMES
incidence_individual_mdd_f[1,] <- coefficients_inci_individual(implist_inci_nfl_f, 'log2nfl', 'status_mdd')
incidence_individual_mdd_f[2,] <- coefficients_inci_individual(implist_inci_tau_f, 'log2tau', 'status_mdd')
incidence_individual_mdd_f[3,] <- coefficients_inci_individual(implist_inci_ab40_f, 'log2ab40', 'status_mdd')
incidence_individual_mdd_f[4,] <- coefficients_inci_individual(implist_inci_ab42_f, 'log2ab42', 'status_mdd')


# put in coefficients - ALL OUTCOMES
incidence_individual_all_m[1,] <- coefficients_inci_individual(implist_inci_nfl_m, 'log2nfl', 'status_all')
incidence_individual_all_m[2,] <- coefficients_inci_individual(implist_inci_tau_m, 'log2tau', 'status_all')
incidence_individual_all_m[3,] <- coefficients_inci_individual(implist_inci_ab40_m, 'log2ab40', 'status_all')
incidence_individual_all_m[4,] <- coefficients_inci_individual(implist_inci_ab42_m, 'log2ab42', 'status_all')

# put in coefficients - MDD OUTCOMES
incidence_individual_mdd_m[1,] <- coefficients_inci_individual(implist_inci_nfl_m, 'log2nfl', 'status_mdd')
incidence_individual_mdd_m[2,] <- coefficients_inci_individual(implist_inci_tau_m, 'log2tau', 'status_mdd')
incidence_individual_mdd_m[3,] <- coefficients_inci_individual(implist_inci_ab40_m, 'log2ab40', 'status_mdd')
incidence_individual_mdd_m[4,] <- coefficients_inci_individual(implist_inci_ab42_m, 'log2ab42', 'status_mdd')

# write out
table3_f <- cbind(incidence_individual_all_f, incidence_individual_mdd_f)
write.xlsx(table3_f, 'O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/tables/table3_incidence_females.xlsx')
table3_m <- cbind(incidence_individual_all_m, incidence_individual_mdd_m)
write.xlsx(table3_m, 'O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/tables/table3_incidence_males.xlsx')

#-------------------------------------------------------------------------
# GET P_VALUES ALL MODELS FOR CORRECTION 
#-------------------------------------------------------------------------

# make table
p_cesd_m <- p_cesd_f <- as.data.frame(matrix(NA, 4,4))
colnames(p_cesd_m) <- colnames(p_cesd_f) <-c('p model 1',"p adj model 1", 'p model 2', "p adj model 2")
rownames(p_cesd_m) <- rownames(p_cesd_f) <- c('nfl_ind', 'tau_ind', 'ab40_ind', 'ab42_ind')

# put in coefficients
p_cesd_m[1,c(1,3)] <- p_cesd_individual(implist_cesd_nfl_m, 'log2nfl')
p_cesd_m[2,c(1,3)] <- p_cesd_individual(implist_cesd_tau_m, 'log2tau')
p_cesd_m[3,c(1,3)] <- p_cesd_individual(implist_cesd_ab40_m, 'log2ab40')
p_cesd_m[4,c(1,3)] <- p_cesd_individual(implist_cesd_ab42_m, 'log2ab42')

# p adjust
p_cesd_m[,2] <- round(p.adjust(p_cesd_m[,1], method = "BH"),3)
p_cesd_m[,4] <- round(p.adjust(p_cesd_m[,3], method = "BH"),3)

# put in coefficients
p_cesd_f[1,c(1,3)] <- p_cesd_individual(implist_cesd_nfl_f, 'log2nfl')
p_cesd_f[2,c(1,3)] <- p_cesd_individual(implist_cesd_tau_f, 'log2tau')
p_cesd_f[3,c(1,3)] <- p_cesd_individual(implist_cesd_ab40_f, 'log2ab40')
p_cesd_f[4,c(1,3)] <- p_cesd_individual(implist_cesd_ab42_f, 'log2ab42')

# p adjust
p_cesd_f[,2] <- round(p.adjust(p_cesd_f[,1], method = "BH"),3)
p_cesd_f[,4] <- round(p.adjust(p_cesd_f[,3], method = "BH"),3)


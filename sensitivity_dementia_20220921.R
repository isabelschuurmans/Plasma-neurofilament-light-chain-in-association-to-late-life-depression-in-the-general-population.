# senstivity analyses to study differences between those who develop dementia and who dont

# FIRST CREATE SUBGROUPS

implist_cesd_nfl_c <- complete(implist_cesd_nfl, action = "long", include = T)
implist_cesd_nfl_yes <- as.mids(implist_cesd_nfl_c[implist_cesd_nfl_c$later_dementia == 1,])
implist_cesd_nfl_no <- as.mids(implist_cesd_nfl_c[implist_cesd_nfl_c$later_dementia == 0,])

implist_cesd_tau_c <- complete(implist_cesd_tau, action = "long", include = T)
implist_cesd_tau_yes <- as.mids(implist_cesd_tau_c[implist_cesd_tau_c$later_dementia == 1,])
implist_cesd_tau_no <- as.mids(implist_cesd_tau_c[implist_cesd_tau_c$later_dementia == 0,])

implist_cesd_ab40_c <- complete(implist_cesd_ab40, action = "long", include = T)
implist_cesd_ab40_yes <- as.mids(implist_cesd_ab40_c[implist_cesd_ab40_c$later_dementia == 1,])
implist_cesd_ab40_no <- as.mids(implist_cesd_ab40_c[implist_cesd_ab40_c$later_dementia == 0,])

implist_cesd_ab42_c <- complete(implist_cesd_ab42, action = "long", include = T)
implist_cesd_ab42_yes <- as.mids(implist_cesd_ab42_c[implist_cesd_ab42_c$later_dementia == 1,])
implist_cesd_ab42_no <- as.mids(implist_cesd_ab42_c[implist_cesd_ab42_c$later_dementia == 0,])

implist_cesd_combined_c <- complete(implist_cesd_combined, action = "long", include = T)
implist_cesd_combined_yes <- as.mids(implist_cesd_combined_c[implist_cesd_combined_c$later_dementia == 1,])
implist_cesd_combined_no <- as.mids(implist_cesd_combined_c[implist_cesd_combined_c$later_dementia == 0,])

implist_inci_nfl_c <- complete(implist_inci_nfl, action = "long", include = T)
implist_inci_nfl_yes <- as.mids(implist_inci_nfl_c[implist_inci_nfl_c$later_dementia == 1,])
implist_inci_nfl_no <- as.mids(implist_inci_nfl_c[implist_inci_nfl_c$later_dementia == 0,])

implist_inci_tau_c <- complete(implist_inci_tau, action = "long", include = T)
implist_inci_tau_yes <- as.mids(implist_inci_tau_c[implist_inci_tau_c$later_dementia == 1,])
implist_inci_tau_no <- as.mids(implist_inci_tau_c[implist_inci_tau_c$later_dementia == 0,])

implist_inci_ab40_c <- complete(implist_inci_ab40, action = "long", include = T)
implist_inci_ab40_yes <- as.mids(implist_inci_ab40_c[implist_inci_ab40_c$later_dementia == 1,])
implist_inci_ab40_no <- as.mids(implist_inci_ab40_c[implist_inci_ab40_c$later_dementia == 0,])

implist_inci_ab42_c <- complete(implist_inci_ab42, action = "long", include = T)
implist_inci_ab42_yes <- as.mids(implist_inci_ab42_c[implist_inci_ab42_c$later_dementia == 1,])
implist_inci_ab42_no <- as.mids(implist_inci_ab42_c[implist_inci_ab42_c$later_dementia == 0,])

implist_inci_combined_c <- complete(implist_inci_combined, action = "long", include = T)
implist_inci_combined_yes <- as.mids(implist_inci_combined_c[implist_inci_combined_c$later_dementia == 1,])
implist_inci_combined_no <- as.mids(implist_inci_combined_c[implist_inci_combined_c$later_dementia == 0,])

##########################################################################
# DOES DEVELOP DEMENTIA
##########################################################################

#-------------------------------------------------------------------------
# CESD ANALYSES
#-------------------------------------------------------------------------

#-INDVIDUAL MODEL---------------------------------------------------------

# make table
crosssectional_individual_yes <- crosssectional_simultaneous_yes <- as.data.frame(matrix(NA, 4,3))
colnames(crosssectional_individual_yes) <- colnames(crosssectional_simultaneous_yes) <-c('model 1', 'model 2', 'model 3')
rownames(crosssectional_individual_yes) <- rownames(crosssectional_simultaneous_yes) <- c('nfl_ind', 'tau_ind', 'ab40_ind', 'ab42_ind')

# put in coefficients
crosssectional_individual_yes[1,] <- coefficients_cesd_individual(implist_cesd_nfl_yes, 'log2nfl')
crosssectional_individual_yes[2,] <- coefficients_cesd_individual(implist_cesd_tau_yes, 'log2tau')
crosssectional_individual_yes[3,] <- coefficients_cesd_individual(implist_cesd_ab40_yes, 'log2ab40')
crosssectional_individual_yes[4,] <- coefficients_cesd_individual(implist_cesd_ab42_yes, 'log2ab42')

#-simultaneous_yes MODEL------------------------------------------------------

# put in coefficient
crosssectional_simultaneous_yes[1,] <- coefficients_cesd_simultaneous(implist_cesd_combined_yes, 'log2nfl')
crosssectional_simultaneous_yes[2,] <- coefficients_cesd_simultaneous(implist_cesd_combined_yes, 'log2tau')
crosssectional_simultaneous_yes[3,] <- coefficients_cesd_simultaneous(implist_cesd_combined_yes, 'log2ab40')
crosssectional_simultaneous_yes[4,] <- coefficients_cesd_simultaneous(implist_cesd_combined_yes, 'log2ab42')

#-WRITE OUT---------------------------------------------------------------

table4 <- rbind(crosssectional_individual_yes, crosssectional_simultaneous_yes)
write.xlsx(table4, 'O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/tables/table4_crosssectional_yesdementia.xlsx')

#-------------------------------------------------------------------------
# INCIDENCE DATA ANALYSES - ALL
#-------------------------------------------------------------------------

#-INDVIDUAL MODEL---------------------------------------------------------

# make table
incidence_individual_all_yes <- incidence_individual_syn_yes <- incidence_individual_mdd_yes <- 
  incidence_simultaneous_all_yes <- incidence_simultaneous_syn_yes <- incidence_simultaneous_mdd_yes <- as.data.frame(matrix(NA, 4,3))
rownames(incidence_individual_all_yes) <- rownames(incidence_individual_syn_yes) <- rownames(incidence_individual_mdd_yes) <- 
  rownames(incidence_simultaneous_all_yes) <- rownames(incidence_simultaneous_syn_yes) <- rownames(incidence_simultaneous_mdd_yes) <- c('nfl_ind', 'tau_ind', 'ab40_ind', 'ab42_ind')
colnames(incidence_individual_all_yes) <- colnames(incidence_individual_syn_yes) <- colnames(incidence_individual_mdd_yes) <- 
  colnames(incidence_simultaneous_all_yes) <- colnames(incidence_simultaneous_syn_yes) <- colnames(incidence_simultaneous_mdd_yes) <- c('model 1', 'model 2', 'model 3')

# put in coefficients - all_yes OUTCOMES
incidence_individual_all_yes[1,] <- coefficients_inci_individual(implist_inci_nfl_yes, 'log2nfl', 'status_all')
incidence_individual_all_yes[2,] <- coefficients_inci_individual(implist_inci_tau_yes, 'log2tau', 'status_all')
incidence_individual_all_yes[3,] <- coefficients_inci_individual(implist_inci_ab40_yes, 'log2ab40', 'status_all')
incidence_individual_all_yes[4,] <- coefficients_inci_individual(implist_inci_ab42_yes, 'log2ab42', 'status_all')

# put in coefficients - syn_yesDROME OUTCOMES
incidence_individual_syn_yes[1,] <- coefficients_inci_individual(implist_inci_nfl_yes, 'log2nfl', 'status_syndromes')
incidence_individual_syn_yes[2,] <- coefficients_inci_individual(implist_inci_tau_yes, 'log2tau', 'status_syndromes')
incidence_individual_syn_yes[3,] <- coefficients_inci_individual(implist_inci_ab40_yes, 'log2ab40', 'status_syndromes')
incidence_individual_syn_yes[4,] <- coefficients_inci_individual(implist_inci_ab42_yes, 'log2ab42', 'status_syndromes')

# put in coefficients - mdd_yes OUTCOMES
incidence_individual_mdd_yes[1,] <- coefficients_inci_individual(implist_inci_nfl_yes, 'log2nfl', 'status_mdd')
incidence_individual_mdd_yes[2,] <- coefficients_inci_individual(implist_inci_tau_yes, 'log2tau', 'status_mdd')
incidence_individual_mdd_yes[3,] <- coefficients_inci_individual(implist_inci_ab40_yes, 'log2ab40', 'status_mdd')
incidence_individual_mdd_yes[4,] <- coefficients_inci_individual(implist_inci_ab42_yes, 'log2ab42', 'status_mdd')

#-SIMULTANEOUS MODEL------------------------------------------------------

# put in coefficients - ALL OUTCOMES
incidence_simultaneous_all_yes[1,] <- coefficients_inci_simultaneous(implist_inci_nfl_yes, 'log2nfl', 'status_all')
incidence_simultaneous_all_yes[2,] <- coefficients_inci_simultaneous(implist_inci_tau_yes, 'log2tau', 'status_all')
incidence_simultaneous_all_yes[3,] <- coefficients_inci_simultaneous(implist_inci_ab40_yes, 'log2ab40', 'status_all')
incidence_simultaneous_all_yes[4,] <- coefficients_inci_simultaneous(implist_inci_ab42_yes, 'log2ab42', 'status_all')

# put in coefficients - SYNDROME OUTCOMES
incidence_simultaneous_syn_yes[1,] <- coefficients_inci_simultaneous(implist_inci_nfl_yes, 'log2nfl', 'status_syndromes')
incidence_simultaneous_syn_yes[2,] <- coefficients_inci_simultaneous(implist_inci_tau_yes, 'log2tau', 'status_syndromes')
incidence_simultaneous_syn_yes[3,] <- coefficients_inci_simultaneous(implist_inci_ab40_yes, 'log2ab40', 'status_syndromes')
incidence_simultaneous_syn_yes[4,] <- coefficients_inci_simultaneous(implist_inci_ab42_yes, 'log2ab42', 'status_syndromes')

# put in coefficients - MDD OUTCOMES
incidence_simultaneous_mdd_yes[1,] <- coefficients_inci_simultaneous(implist_inci_nfl_yes, 'log2nfl', 'status_mdd')
incidence_simultaneous_mdd_yes[2,] <- coefficients_inci_simultaneous(implist_inci_tau_yes, 'log2tau', 'status_mdd')
incidence_simultaneous_mdd_yes[3,] <- coefficients_inci_simultaneous(implist_inci_ab40_yes, 'log2ab40', 'status_mdd')
incidence_simultaneous_mdd_yes[4,] <- coefficients_inci_simultaneous(implist_inci_ab42_yes, 'log2ab42', 'status_mdd')

#-WRITE OUT---------------------------------------------------------------

incidence_individual_yes <- cbind(incidence_individual_all_yes, incidence_individual_syn_yes, incidence_individual_mdd_yes)
incidence_simultaneous_yes <- cbind(incidence_simultaneous_all_yes, incidence_simultaneous_syn_yes, incidence_simultaneous_mdd_yes)
table5 <- rbind(incidence_individual_yes, incidence_simultaneous_yes)
write.xlsx(table5, 'O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/tables/table5_incidence_yesdementia.xlsx')

##########################################################################
# DOES NOT DEVELOP DEMENTIA
##########################################################################

#-------------------------------------------------------------------------
# CESD ANALYSES
#-------------------------------------------------------------------------

#-INDVIDUAL MODEL---------------------------------------------------------

# make table
crosssectional_individual_no <- crosssectional_simultaneous_no <- as.data.frame(matrix(NA, 4,3))
colnames(crosssectional_individual_no) <- colnames(crosssectional_simultaneous_no) <-c('model 1', 'model 2', 'model 3')
rownames(crosssectional_individual_no) <- rownames(crosssectional_simultaneous_no) <- c('nfl_ind', 'tau_ind', 'ab40_ind', 'ab42_ind')

# put in coefficients
crosssectional_individual_no[1,] <- coefficients_cesd_individual(implist_cesd_nfl_no, 'log2nfl')
crosssectional_individual_no[2,] <- coefficients_cesd_individual(implist_cesd_tau_no, 'log2tau')
crosssectional_individual_no[3,] <- coefficients_cesd_individual(implist_cesd_ab40_no, 'log2ab40')
crosssectional_individual_no[4,] <- coefficients_cesd_individual(implist_cesd_ab42_no, 'log2ab42')

#-simultaneous_no MODEL------------------------------------------------------

# put in coefficient
crosssectional_simultaneous_no[1,] <- coefficients_cesd_simultaneous(implist_cesd_combined_no, 'log2nfl')
crosssectional_simultaneous_no[2,] <- coefficients_cesd_simultaneous(implist_cesd_combined_no, 'log2tau')
crosssectional_simultaneous_no[3,] <- coefficients_cesd_simultaneous(implist_cesd_combined_no, 'log2ab40')
crosssectional_simultaneous_no[4,] <- coefficients_cesd_simultaneous(implist_cesd_combined_no, 'log2ab42')

#-WRITE OUT---------------------------------------------------------------

table6 <- rbind(crosssectional_individual_no, crosssectional_simultaneous_no)
write.xlsx(table6, 'O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/tables/table6_crosssectional_nodementia.xlsx')

#-------------------------------------------------------------------------
# INCIDENCE DATA ANALYSES - ALL
#-------------------------------------------------------------------------

#-INDVIDUAL MODEL---------------------------------------------------------

# make table
incidence_individual_all_no <- incidence_individual_syn_no <- incidence_individual_mdd_no <- 
  incidence_simultaneous_all_no <- incidence_simultaneous_syn_no <- incidence_simultaneous_mdd_no <- as.data.frame(matrix(NA, 4,3))
rownames(incidence_individual_all_no) <- rownames(incidence_individual_syn_no) <- rownames(incidence_individual_mdd_no) <- 
  rownames(incidence_simultaneous_all_no) <- rownames(incidence_simultaneous_syn_no) <- rownames(incidence_simultaneous_mdd_no) <- c('nfl_ind', 'tau_ind', 'ab40_ind', 'ab42_ind')
colnames(incidence_individual_all_no) <- colnames(incidence_individual_syn_no) <- colnames(incidence_individual_mdd_no) <- 
  colnames(incidence_simultaneous_all_no) <- colnames(incidence_simultaneous_syn_no) <- colnames(incidence_simultaneous_mdd_no) <- c('model 1', 'model 2', 'model 3')

# put in coefficients - all_no OUTCOMES
incidence_individual_all_no[1,] <- coefficients_inci_individual(implist_inci_nfl_no, 'log2nfl', 'status_all')
incidence_individual_all_no[2,] <- coefficients_inci_individual(implist_inci_tau_no, 'log2tau', 'status_all')
incidence_individual_all_no[3,] <- coefficients_inci_individual(implist_inci_ab40_no, 'log2ab40', 'status_all')
incidence_individual_all_no[4,] <- coefficients_inci_individual(implist_inci_ab42_no, 'log2ab42', 'status_all')

# put in coefficients - syn_noDROME OUTCOMES
incidence_individual_syn_no[1,] <- coefficients_inci_individual(implist_inci_nfl_no, 'log2nfl', 'status_syndromes')
incidence_individual_syn_no[2,] <- coefficients_inci_individual(implist_inci_tau_no, 'log2tau', 'status_syndromes')
incidence_individual_syn_no[3,] <- coefficients_inci_individual(implist_inci_ab40_no, 'log2ab40', 'status_syndromes')
incidence_individual_syn_no[4,] <- coefficients_inci_individual(implist_inci_ab42_no, 'log2ab42', 'status_syndromes')

# put in coefficients - mdd_no OUTCOMES
incidence_individual_mdd_no[1,] <- coefficients_inci_individual(implist_inci_nfl_no, 'log2nfl', 'status_mdd')
incidence_individual_mdd_no[2,] <- coefficients_inci_individual(implist_inci_tau_no, 'log2tau', 'status_mdd')
incidence_individual_mdd_no[3,] <- coefficients_inci_individual(implist_inci_ab40_no, 'log2ab40', 'status_mdd')
incidence_individual_mdd_no[4,] <- coefficients_inci_individual(implist_inci_ab42_no, 'log2ab42', 'status_mdd')

#-SIMULTANEOUS MODEL------------------------------------------------------

# put in coefficients - ALL OUTCOMES
incidence_simultaneous_all_no[1,] <- coefficients_inci_simultaneous(implist_inci_nfl_no, 'log2nfl', 'status_all')
incidence_simultaneous_all_no[2,] <- coefficients_inci_simultaneous(implist_inci_tau_no, 'log2tau', 'status_all')
incidence_simultaneous_all_no[3,] <- coefficients_inci_simultaneous(implist_inci_ab40_no, 'log2ab40', 'status_all')
incidence_simultaneous_all_no[4,] <- coefficients_inci_simultaneous(implist_inci_ab42_no, 'log2ab42', 'status_all')

# put in coefficients - SYNDROME OUTCOMES
incidence_simultaneous_syn_no[1,] <- coefficients_inci_simultaneous(implist_inci_nfl_no, 'log2nfl', 'status_syndromes')
incidence_simultaneous_syn_no[2,] <- coefficients_inci_simultaneous(implist_inci_tau_no, 'log2tau', 'status_syndromes')
incidence_simultaneous_syn_no[3,] <- coefficients_inci_simultaneous(implist_inci_ab40_no, 'log2ab40', 'status_syndromes')
incidence_simultaneous_syn_no[4,] <- coefficients_inci_simultaneous(implist_inci_ab42_no, 'log2ab42', 'status_syndromes')

# put in coefficients - MDD OUTCOMES
incidence_simultaneous_mdd_no[1,] <- coefficients_inci_simultaneous(implist_inci_nfl_no, 'log2nfl', 'status_mdd')
incidence_simultaneous_mdd_no[2,] <- coefficients_inci_simultaneous(implist_inci_tau_no, 'log2tau', 'status_mdd')
incidence_simultaneous_mdd_no[3,] <- coefficients_inci_simultaneous(implist_inci_ab40_no, 'log2ab40', 'status_mdd')
incidence_simultaneous_mdd_no[4,] <- coefficients_inci_simultaneous(implist_inci_ab42_no, 'log2ab42', 'status_mdd')

#-WRITE OUT---------------------------------------------------------------

incidence_individual_no <- cbind(incidence_individual_all_no, incidence_individual_syn_no, incidence_individual_mdd_no)
incidence_simultaneous_no <- cbind(incidence_simultaneous_all_no, incidence_simultaneous_syn_no, incidence_simultaneous_mdd_no)
table7 <- rbind(incidence_individual_no, incidence_simultaneous_no)
write.xlsx(table7, 'O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/tables/table7_incidence_nodementia.xlsx')

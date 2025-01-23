# script to merge data
# Isabel Schuurmans
# 28-07-2022
# plasma markers of neurodegenerative disease and risk for depression

# QUESTIONS
# data blood draw: during research visit?
# depression 5 years before!!
# exclude dementie baseline!
# also exclude outcomes?
# bespreek met annemarie hoe je timing hebt gedaan
# imputing outcomes? only 2.5%, same is for biomarkers!
# time-dependent variation?
# one participants with batch missing??

# library
library(haven)
library(readxl)
library(mice)
library(survival)
library(xlsx)
library(ggfortify)
library(ggplot2)
library(lubridate)
library(grid)
library(cowplot)

source("O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/script/function_20220914.R")

#-------------------------------------------------------------------------
# READ IN ALL THE DATA
#-------------------------------------------------------------------------

## DEPRESSION INCIDENCE

# read in depression data: INCIDENCE
dep1 <- read_sav("O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/data/RS-I_Shortened_Cohort_IncidenceDepression_(11-2013).sav") #RSI
dep2 <- read_sav("O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/data/RS-II_ShortenedCohort_IncidenceDepression_(07-2020).sav") #RSII
dep2$rs_cohort <- 2 # dataset missed this variable, otherwise rbind doesnt work
dep <- rbind(dep1, dep2)
# dep <- rbind(dep, dep3) # in case we need RSIII after all

## DEPRESSION CROSS SECTIONAL

# read in depression data: CROSS-SECTIONAL 
cesd1 <- read_sav("O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/data/E4_RS-I_CESD.sav")
cesd1 <- cesd1[,c("ergoid", "e4_discorewgt")]
cesd2 <- read_sav("O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/data/E4_RS-II_CESD.sav")
cesd2 <- cesd2[,c("ergoid", "discorewgt")]
names(cesd2)<-names(cesd1)
cesd <- rbind(cesd1, cesd2)

## PLASMA DATA

# read in plasma data
plasma <- read_excel("O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/Data/AD Biomarkers Data.xlsx")

# remove flagged information
plasma[plasma$flagnfl == 1,c("nfl", "log2nfl")] <- NA
plasma[plasma$flagtau == 1,c("tau", "log2tau")] <- NA
plasma[plasma$flagab40 == 1,c("ab40", "log2ab40")] <- NA
plasma[plasma$flagab42 == 1,c("ab42", "log2ab42")] <- NA

# select variables
plasma <- plasma[,c('ergoid', 'log2nfl', 'log2tau', 'log2ab40', 'log2ab42', "AGE")]
plasma <- as.data.frame(apply(plasma, 2, as.numeric))

## BATCH NUMBER

# read in batch number
batch <- read_excel("O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/Data/BatchNummer20220912.xlsx")

# select variables
batch <- batch[,c("ergoid", "batch")]

## DATE VISIT

# read in date of visit
dates <- read_sav("O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/Data/e4_(4)_BLDAFNAM_(10-jul-2017).sav")
dates <- dates[c("ergoid", "e4_2686")]

## AGE AND SEX

# covs: age, sex and  
agesex <- read_sav("O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/data/RoterdamStudy_Basics2014.sav")
agesex$sex <- as.numeric(ifelse(agesex$sex == 0, 0, 1))
# select variables
agesex <- agesex[,c('ergoid', 'sex', 'date_of_birth')]

## EDUCATION

# covs: educational level 
edu <- read_sav("O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/data/Education RS-I-II-III (UNESCO class)_(12-MAR-2015).sav")
# give edu more logical name
edu$education <- as.numeric(ifelse(edu$ses_UNESCO_recoded == 0, 0,
                        ifelse(edu$ses_UNESCO_recoded == 1, 1,
                               ifelse(edu$ses_UNESCO_recoded == 2, 2,
                                      ifelse(edu$ses_UNESCO_recoded == 3, 3, NA)))))
# select variables
edu <- edu[,c('ergoid', 'education')]

## PAID EMPLOYMENT

# covs: paid employment
ses <- read_sav("O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/data/e4_intvw_SES_(04-nov-2011).sav")
# recode missings in hrs
ses$e4_dijobhr[ses$e4_dijobhr==888] <- NA
ses$e4_dijobhr[ses$e4_dijobhr==999] <- NA
ses$e4_dijobhr[ses$e4_dijobhr==777] <- NA
ses$e4_dijobhr[ses$e4_dijobhr==88] <- NA
ses$e4_dijobhr[ses$e4_dijobhr==77] <- NA
ses$e4_dijobhr[ses$e4_dijobhr==99] <- NA
# make paid empl variable (in werkkring voor meer/gelijk aan 12 hrs per week)
ses$paidemployment <- ifelse(ses$e4_dises2 == 0 & ses$e4_dijobhr >= 12, 1, 0)
# select variables
ses <- ses[,c('ergoid', 'paidemployment')]

## SMOKING STATUS

## covs: smoking status (0 = never, 1 = ever, 2 = current)
smoking <- read_sav("O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/data/e4_intvw_SMOKING_(04-nov-2011).sav")
# recode smoking
smoking$e4_dicg[smoking$e4_dicg == 3] <- 2
smoking$e4_dipi[smoking$e4_dipi == 3] <- 2
smoking$e4_dict[smoking$e4_dict == 3] <- 2
smoking$smoking <- ifelse(smoking$e4_dicg == 0 & smoking$e4_dipi == 0 & smoking$e4_dict == 0, 0, 
                          ifelse(smoking$e4_dicg == 1 | smoking$e4_dipi == 1 | smoking$e4_dict == 1, 1,
                                 ifelse(smoking$e4_dicg == 2 | smoking$e4_dipi == 2 | smoking$e4_dict == 2, 2, NA)))
# select variables
smoking <- smoking[,c('ergoid', 'smoking')]
    
## ALCOHOL INTAKE

# covs: alcohol intake
alcohol <- read_sav("O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/data/e4_intvw_Alcoholperday_22-11-2013.sav")
# give alcohol more logical name
alcohol$alcohol <- alcohol$e4_Alc_Tot 
# select variables
alcohol <- alcohol[c('ergoid', 'alcohol')]

## BMI

# covs: BMI
bmi <- read_sav("O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/data/e4_(4)_UITSCHR_(06-nov-2014)_ANTHROPO-PART.sav")
bmi$bmi <- bmi$e4_230/(bmi$e4_229/100)^2
bmi <- bmi[,c('ergoid', 'bmi', 'e4_230', 'e4_229')]

## EGFR

# covs: eGFR
egfr_e3 <- read_sav("O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/data/eGFRcreat3p4_Isabel.sav")

## DEMENTIA 

# excl: dementia
dementia <- read_sav("O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/data/Dementia_RSI_II_III_2021_12_03.sav")
dementia <- dementia[,c("ergoid", "dementia_prevalent", "dementia_incident", "dementia_date")]

#-------------------------------------------------------------------------
# MERGE
#-------------------------------------------------------------------------

# merge predictor and outcome
dep_cont <- merge(cesd, plasma, by = 'ergoid', all = T)
df_inc <- merge(dep, dep_cont, by = 'ergoid', all = T)

# add covs
df_inc1 <- merge(df_inc, agesex, by = 'ergoid', all.x = T)
df_inc2 <- merge(df_inc1, ses, by = 'ergoid', all.x = T)
df_inc3 <- merge(df_inc2, edu, by = 'ergoid', all.x = T)
df_inc4 <- merge(df_inc3, smoking, by = 'ergoid', all.x = T)
df_inc5 <- merge(df_inc4, alcohol, by = 'ergoid', all.x = T)
df_inc6 <- merge(df_inc5, bmi, by = 'ergoid', all.x = T)
df_inc7 <- merge(df_inc6, batch, by = "ergoid", all.x = T)
df_inc8 <- merge(df_inc7, dates, by = "ergoid", all.x = T)
df_inc9 <- merge(df_inc8, dementia, by = "ergoid", all.x = T)
df_inc10 <- merge(df_inc9, egfr_e3, by = "ergoid", all.x = T)

df_inc <- df_inc10

#-------------------------------------------------------------------------
# CONSTRUCT VARIABLES
#-------------------------------------------------------------------------

# calculate time
df_inc$time1 <- ifelse(is.na(df_inc$CLEANdate_totalevent2yr1), df_inc$enddate - df_inc$e4_2686, df_inc$CLEANdate_totalevent2yr1-df_inc$e4_2686)
df_inc$time2 <- ifelse(is.na(df_inc$CLEANdate_totalevent2yr2), df_inc$enddate - df_inc$e4_2686, df_inc$CLEANdate_totalevent2yr2-df_inc$e4_2686)
df_inc$time3 <- ifelse(is.na(df_inc$CLEANdate_totalevent2yr3), df_inc$enddate - df_inc$e4_2686, df_inc$CLEANdate_totalevent2yr3-df_inc$e4_2686)
df_inc$time4 <- ifelse(is.na(df_inc$CLEANdate_totalevent2yr4), df_inc$enddate - df_inc$e4_2686, df_inc$CLEANdate_totalevent2yr4-df_inc$e4_2686)
df_inc$time5 <- ifelse(is.na(df_inc$CLEANdate_totalevent2yr5), df_inc$enddate - df_inc$e4_2686, df_inc$CLEANdate_totalevent2yr5-df_inc$e4_2686)
df_inc$time6 <- ifelse(is.na(df_inc$CLEANdate_totalevent2yr6), df_inc$enddate - df_inc$e4_2686, df_inc$CLEANdate_totalevent2yr6-df_inc$e4_2686)

# make history of depression variable (365*5yrs)
df_inc$depressionhistory <- 0
df_inc$depressionhistory[df_inc$time1 < 0 & df_inc$time1 > -1825] <- 1
df_inc$depressionhistory[df_inc$time2 < 0 & df_inc$time2 > -1825] <- 1
df_inc$depressionhistory[df_inc$time3 < 0 & df_inc$time3 > -1825] <- 1
df_inc$depressionhistory[df_inc$time4 < 0 & df_inc$time4 > -1825] <- 1
df_inc$depressionhistory[df_inc$time5 < 0 & df_inc$time5 > -1825] <- 1
df_inc$depressionhistory[df_inc$time6 < 0 & df_inc$time6 > -1825] <- 1

# make time variable, start with indicator variable which event someone needs
df_inc$indicator <- ifelse(df_inc$time1 > 0, 1,
                           ifelse(df_inc$time2 > 0, 2,
                                  ifelse(df_inc$time3 > 0, 3, 
                                         ifelse(df_inc$time4 > 0, 4,
                                                ifelse(df_inc$time5 > 0, 5,
                                                       ifelse(df_inc$time6 > 0, 6, 7))))))

# make time variable; make sure it only captures time after nfl measure
df_inc$time <- 0
df_inc$time[which(df_inc$indicator == 1)] <- df_inc$time1[which(df_inc$indicator == 1)]
df_inc$time[which(df_inc$indicator == 2)] <- df_inc$time2[which(df_inc$indicator == 2)]
df_inc$time[which(df_inc$indicator == 3)] <- df_inc$time3[which(df_inc$indicator == 3)]
df_inc$time[which(df_inc$indicator == 4)] <- df_inc$time4[which(df_inc$indicator == 4)]
df_inc$time[which(df_inc$indicator == 5)] <- df_inc$time5[which(df_inc$indicator == 5)]
df_inc$time[which(df_inc$indicator == 6)] <- df_inc$time6[which(df_inc$indicator == 6)]
df_inc$time <- ifelse(df_inc$indicator == 7, df_inc$enddate - df_inc$e4_2686, df_inc$time)
# still negative dates / either coding error or other problems, exclude
df_inc$time[which(df_inc$time < 0)] <- NA

# make status variable
df_inc$status <- 0
df_inc$status[which(df_inc$indicator == 1)] <- df_inc$CLEANtotalevent2yr1[which(df_inc$indicator == 1)]
df_inc$status[which(df_inc$indicator == 2)] <- df_inc$CLEANtotalevent2yr2[which(df_inc$indicator == 2)]
df_inc$status[which(df_inc$indicator == 3)] <- df_inc$CLEANtotalevent2yr3[which(df_inc$indicator == 3)]
df_inc$status[which(df_inc$indicator == 4)] <- df_inc$CLEANtotalevent2yr4[which(df_inc$indicator == 4)]
df_inc$status[which(df_inc$indicator == 5)] <- df_inc$CLEANtotalevent2yr5[which(df_inc$indicator == 5)]
df_inc$status[which(df_inc$indicator == 6)] <- df_inc$CLEANtotalevent2yr6[which(df_inc$indicator == 6)]

# recode status
df_inc$status_all <- ifelse(df_inc$status > 0 & df_inc$status < 4, 1, 0)
df_inc$status_syndromes <- ifelse(df_inc$status > 0 & df_inc$status < 3, 1, 0)
df_inc$status_mdd <- ifelse(df_inc$status == 1, 1, 0)
  
# ever a bipolar disorder diagnosis
df_inc$bipolardiagnosis <- 0
df_inc$bipolardiagnosis[df_inc$CLEANtotalevent2yr1 == 4] <- 1
df_inc$bipolardiagnosis[df_inc$CLEANtotalevent2yr2 == 4] <- 1
df_inc$bipolardiagnosis[df_inc$CLEANtotalevent2yr3 == 4] <- 1
df_inc$bipolardiagnosis[df_inc$CLEANtotalevent2yr4 == 4] <- 1
df_inc$bipolardiagnosis[df_inc$CLEANtotalevent2yr5 == 4] <- 1
df_inc$bipolardiagnosis[df_inc$CLEANtotalevent2yr6 == 4] <- 1

# prevalent depression at biomarker assessment
df_inc$incident_dementia <- ifelse(df_inc$dementia_prevalent == 0, 0,
                                 ifelse(df_inc$dementia_prevalent == 1, 1, NA))
df_inc$incident_dementia <- ifelse(is.na(df_inc$dementia_date) | is.na(df_inc$e4_2686), df_inc$incident_dementia,
                                    ifelse(df_inc$dementia_date < df_inc$e4_2686, 1, df_inc$incident_dementia))

# got dementia afterwards
df_inc$later_dementia <- ifelse(is.na(df_inc$dementia_date) | is.na(df_inc$e4_2686), 0,
                                ifelse(df_inc$dementia_date > df_inc$e4_2686, 1, 0))

# define time
df_inc$time_dementia <- ifelse(is.na(df_inc$dementia_date), df_inc$enddate - df_inc$e4_2686, df_inc$dementia_date-df_inc$e4_2686)

#-------------------------------------------------------------------------
# EXCLUDE PARTICIPANTS
#-------------------------------------------------------------------------

# only get the sufficient samples
df_inc_start <- df_inc[!is.na(df_inc$AGE),]

# exclude missing all biomarkers
df_inc1 <- df_inc_start[!is.na(df_inc_start$log2nfl) | !is.na(df_inc_start$log2tau) | !is.na(df_inc_start$log2ab40) | !is.na(df_inc_start$log2ab42),]

# exclude either missing ces-d/incidence
df_inc2 <- df_inc1[!is.na(df_inc1$e4_discorewgt) & !is.na(df_inc1$startdate),]

# depression before date of blood sampling
df_inc3 <- df_inc2[which(df_inc2$depressionhistory == 0),] # we only keep those who had an event after NFL measure, or at least 5 years before

# bipolar diagnosis ever
df_inc4 <- df_inc3[which(df_inc3$bipolardiagnosis == 0),]

# exclude dementia!!
df_inc5 <- df_inc4[which(df_inc4$incident_dementia == 0),]

#-------------------------------------------------------------------------
# Multiple imputation
#-------------------------------------------------------------------------

df_inc5$rankdep <- qnorm((rank(df_inc5$e4_discorewgt, na.last="keep")-0.5)/sum(!is.na(df_inc5$e4_discorewgt)))

# select variablles
df_to_impute <- df_inc5[,c("ergoid","status_all", "status_syndromes", "status_mdd", "time","e4_discorewgt","rankdep",
                           "log2nfl","log2tau","log2ab40","log2ab42","batch",           
                           "AGE","sex", "paidemployment", "education", "smoking","alcohol","bmi", "later_dementia", "time_dementia",
                           "eGFRcreat_RF_e4", "eGFRcreat_RF_e3", "eGFRcreat_RF_ep")]

# initial run
imp0 <- mice(df_to_impute, maxit = 0, defaultMethod = c('pmm', 'pmm', 'pmm', 'pmm'))

# fix predictormatrix: ergoid not needed for imputations
predictormatrix <- imp0$predictorMatrix
predictormatrix[,c("ergoid","status_syndromes", "status_mdd")] <- 0

# run imputation
implist <- mice(df_to_impute, m = 10, maxit = 10, predictorMatrix = predictormatrix, defaultMethod = c('pmm', 'pmm', 'pmm', 'pmm'))

# get ergoids for those with complete ces-d, and those with complete incidence data
ergoids_cesd <- ergoids_inci <- df_to_impute[!is.na(df_to_impute$e4_discorewgt) & !is.na(df_to_impute$time),"ergoid"]

# get ergoids for those with complete nfl data
ergoids_nfl <- df_to_impute[!is.na(df_to_impute$log2nfl),"ergoid"]
ergoids_tau <- df_to_impute[!is.na(df_to_impute$log2tau),"ergoid"]
ergoids_ab40 <- df_to_impute[!is.na(df_to_impute$log2ab40),"ergoid"]
ergoids_ab42 <- df_to_impute[!is.na(df_to_impute$log2ab42),"ergoid"]

# make dataset complete CES-D & complete incidence
implist_c <- complete(implist, action = "long", include = T)

# first ces-d, make implist for four biomarkers seperately (complete info needed)
implist_cesd_nfl <- as.mids(implist_c[implist_c$ergoid %in% ergoids_cesd & implist_c$ergoid %in% ergoids_nfl,])
implist_cesd_tau <- as.mids(implist_c[implist_c$ergoid %in% ergoids_cesd & implist_c$ergoid %in% ergoids_tau,])
implist_cesd_ab40 <- as.mids(implist_c[implist_c$ergoid %in% ergoids_cesd & implist_c$ergoid %in% ergoids_ab40,])
implist_cesd_ab42 <- as.mids(implist_c[implist_c$ergoid %in% ergoids_cesd & implist_c$ergoid %in% ergoids_ab42,])
implist_cesd_combined <- as.mids(implist_c[implist_c$ergoid %in% ergoids_cesd & implist_c$ergoid %in% ergoids_nfl & 
                                             implist_c$ergoid %in% ergoids_tau & implist_c$ergoid %in% ergoids_ab40 &
                                             implist_c$ergoid %in% ergoids_ab42,])

# now incidence data, make implist for four biomarkers seperately (complete info needed)
implist_inci_nfl <- as.mids(implist_c[implist_c$ergoid %in% ergoids_inci & implist_c$ergoid %in% ergoids_nfl,])
implist_inci_tau <- as.mids(implist_c[implist_c$ergoid %in% ergoids_inci & implist_c$ergoid %in% ergoids_tau,])
implist_inci_ab40 <- as.mids(implist_c[implist_c$ergoid %in% ergoids_inci & implist_c$ergoid %in% ergoids_ab40,])
implist_inci_ab42 <- as.mids(implist_c[implist_c$ergoid %in% ergoids_inci & implist_c$ergoid %in% ergoids_ab42,])
implist_inci_combined <- as.mids(implist_c[implist_c$ergoid %in% ergoids_inci & implist_c$ergoid %in% ergoids_nfl & 
                                            implist_c$ergoid %in% ergoids_tau & implist_c$ergoid %in% ergoids_ab40 &
                                            implist_c$ergoid %in% ergoids_ab42,])

#-------------------------------------------------------------------------
# Standardize ces-d
#-------------------------------------------------------------------------

##--- nfl

# complete implist
implist_c <- complete(implist_cesd_nfl, action = 'long', include = T)

# standardize per dataset
pred <- matrix(nrow=nrow(implist_c)/11, ncol=11)
for (i in 1:11){
  pred[,i] <- scale(complete(implist_cesd_nfl,action=i-1)[,c("e4_discorewgt")])}
implist_c$e4_discorewgt_z <- as.numeric(pred)

# write out
implist_cesd_nfl <- as.mids(implist_c)

##--- tau

# complete implist
implist_c <- complete(implist_cesd_tau, action = 'long', include = T)

# standardize per dataset
pred <- matrix(nrow=nrow(implist_c)/11, ncol=11)
for (i in 1:11){
  pred[,i] <- scale(complete(implist_cesd_tau,action=i-1)[,c("e4_discorewgt")])}
implist_c$e4_discorewgt_z <- as.numeric(pred)

# write out
implist_cesd_tau <- as.mids(implist_c)

##--- ab40

# complete implist
implist_c <- complete(implist_cesd_ab40, action = 'long', include = T)

# standardize per dataset
pred <- matrix(nrow=nrow(implist_c)/11, ncol=11)
for (i in 1:11){
  pred[,i] <- scale(complete(implist_cesd_ab40,action=i-1)[,c("e4_discorewgt")])}
implist_c$e4_discorewgt_z <- as.numeric(pred)

# write out
implist_cesd_ab40 <- as.mids(implist_c)

##--- ab42

# complete implist
implist_c <- complete(implist_cesd_ab42, action = 'long', include = T)

# standardize per dataset
pred <- matrix(nrow=nrow(implist_c)/11, ncol=11)
for (i in 1:11){
  pred[,i] <- scale(complete(implist_cesd_ab42,action=i-1)[,c("e4_discorewgt")])}
implist_c$e4_discorewgt_z <- as.numeric(pred)

# write out
implist_cesd_ab42 <- as.mids(implist_c)

#-------------------------------------------------------------------------
# Flowchart
#-------------------------------------------------------------------------

# Full cohort
step1 <- nrow(df_inc_start)

# exclude missing all biomarkers
step2 <- nrow(df_inc1)

# exclude both missing ces-d/incidence
step3 <- nrow(df_inc2) 

# exclude history of dep (5 years prior to biomarker assessment)
step4 <- nrow(df_inc3)

# exclude bipolar diagnosis ever
step5 <- nrow(df_inc4)

# exclude history depression
step6 <- nrow(df_inc5)

# subgroups per branch
step6nfl <- table(ergoids_cesd %in% ergoids_nfl)[2]
step6tau <- table(ergoids_cesd %in% ergoids_tau)[2]
step6ab40 <- table(ergoids_cesd %in% ergoids_ab40)[2]
step6ab42 <- table(ergoids_cesd %in% ergoids_ab42)[2]
step6combined <- table(ergoids_cesd %in% ergoids_nfl &
                              ergoids_cesd %in% ergoids_tau &
                              ergoids_cesd %in% ergoids_ab40 &
                              ergoids_cesd %in% ergoids_ab42)[2]


# flowchart
c(step1, step1-step2, step2, step2-step3, step3, step3-step4, step4, step4-step5, step5, step5-step6, step6, 
  step6nfl, step6tau, step6ab40, step6ab42, step6combined)

#-------------------------------------------------------------------------
# TABLE 1 - PARTICIPANT CHARACTERISTICS
#-------------------------------------------------------------------------

# recode sex
df_to_impute$sex0 <- ifelse(df_to_impute$sex == 0, 1, 0)
df_to_impute$sex1 <- ifelse(df_to_impute$sex == 1, 1, 0)

# recode paid employment
df_to_impute$paidemployment0 <- ifelse(df_to_impute$paidemployment == 0, 1, 0)
df_to_impute$paidemployment1 <- ifelse(df_to_impute$paidemployment == 1, 1, 0)

# recode edu 
df_to_impute$edu0 <- ifelse(df_to_impute$education == 0, 1, 0)
df_to_impute$edu1 <- ifelse(df_to_impute$education == 1, 1, 0)
df_to_impute$edu2 <- ifelse(df_to_impute$education == 2, 1, 0)
df_to_impute$edu3 <- ifelse(df_to_impute$education == 3, 1, 0)

# recode smoking 
df_to_impute$smoking0 <- ifelse(df_to_impute$smoking == 0, 1, 0)
df_to_impute$smoking1 <- ifelse(df_to_impute$smoking == 1, 1, 0)
df_to_impute$smoking2 <- ifelse(df_to_impute$smoking == 2, 1, 0)

# recode dementia
df_to_impute$dementiayes <- ifelse(df_to_impute$later_dementia == 1, 1, 0)
df_to_impute$dementiano <- ifelse(df_to_impute$later_dementia == 1, 0, 1)

# get follow-up time
df_to_impute$followup <- df_to_impute$time/365.25
df_to_impute$followup <- df_to_impute$time/365.25

# get NAs per row
df_to_impute$eduNA <- ifelse(is.na(df_to_impute$education), 1, 0)
df_to_impute$paidemploymentNA <- ifelse(is.na(df_to_impute$paidemployment), 1, 0)
df_to_impute$smokingNA <- ifelse(is.na(df_to_impute$smoking), 1, 0)
df_to_impute$alcoholNA <- ifelse(is.na(df_to_impute$alcohol), 1, 0)
df_to_impute$bmiNA <- ifelse(is.na(df_to_impute$bmi), 1, 0)
df_to_impute$eGFRNA <- ifelse(is.na(df_to_impute$eGFRcreat_RF_e4), 1, 0)
df_to_impute$nflNA <- ifelse(is.na(df_to_impute$log2nfl), 1, 0)
df_to_impute$tauNA <- ifelse(is.na(df_to_impute$log2tau), 1, 0)
df_to_impute$ab40NA <- ifelse(is.na(df_to_impute$log2ab40), 1, 0)
df_to_impute$ab42NA <- ifelse(is.na(df_to_impute$log2ab42), 1, 0)
df_to_impute$e4_discorewgtNA <- ifelse(is.na(df_to_impute$e4_discorewgt), 1, 0)
df_to_impute$status_allNA <- ifelse(is.na(df_to_impute$status_all), 1, 0)

# select vars for df
df_table1 <- df_to_impute[,c("AGE","followup","sex0","sex1","edu0", "edu1", "edu2", "edu3",
                             "paidemployment0","paidemployment1",
                             "smoking0","smoking1","smoking2",
                             "alcohol", "bmi","eGFRcreat_RF_e4","dementiayes", "dementiano",
                             "log2nfl","log2tau" ,"log2ab40","log2ab42", 
                             "e4_discorewgt","status_all","status_syndromes","status_mdd")]

df_table1_NA <- df_to_impute[,c("status_allNA","eduNA","paidemploymentNA","smokingNA","alcoholNA","bmiNA","eGFRNA",
                                                "nflNA", "tauNA", "ab40NA", "ab42NA", "e4_discorewgtNA", "status_allNA")]

# recode to factors
df_table1[,c("sex0","sex1","status_all","status_syndromes","status_mdd",
             "edu0", "edu1", "edu2", "edu3",
             "paidemployment0","paidemployment1",
             "smoking0","smoking1","smoking2",
             "dementiayes", "dementiano")] <- 
  apply(df_table1[,c("sex0","sex1","status_all","status_syndromes","status_mdd",
                     "edu0", "edu1", "edu2", "edu3",
                     "paidemployment0","paidemployment1",
                     "smoking0","smoking1","smoking2",
                     "dementiayes", "dementiano"
                     )], 2, as.factor)

df_table1_NA <- apply(df_table1_NA,2,as.factor)

# get info
table1<- get_table1(df_table1)
colnames(table1) <- c("M SD total", "N % total")
rownames(table1) <- c('N',colnames(df_table1))

# overwrite follow-up time with median and IQR
table1["followup", 1] <- paste0(round(median(df_to_impute$followup),2),
                                ' [', round(IQR(df_to_impute$followup),2), "]")

# get missings (use status for NAs follow-up time)
table1_missings <- get_table1(df_table1_NA)
colnames(table1_missings) <- c("M SD total", "N % total")
rownames(table1_missings) <- c('N',colnames(df_table1_NA))

# write out
write.xlsx(table1, 'O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/tables/table1_participantcharacteristics_A.xlsx')   
write.xlsx(table1_missings, 'O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/tables/table1_participantcharacteristics_B.xlsx')   

#-------------------------------------------------------------------------
# CORRELATION
#-------------------------------------------------------------------------

tableS1 <- round(cor(df_to_impute[,c("log2nfl","log2tau","log2ab40","log2ab42")], use = "pairwise.complete.obs"),2)
write.xlsx(tableS1, 'O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/tables/tableS1_correlationmatrix.xlsx')   

#-------------------------------------------------------------------------
# CESD ANALYSES
#-------------------------------------------------------------------------

# 1: age, sex and batch number of biomarker analysis. 
# 2: ..., educational level, paid employment, smoking status, alcohol intake, and BMI.

#-NON STANDARDIZED---------------------------------------------------------

# make table
table2 <- as.data.frame(matrix(NA, 4,2))
colnames(table2) <-c('model 1', 'model 2')
rownames(table2) <- rownames(table2) <- c('nfl_ind', 'tau_ind', 'ab40_ind', 'ab42_ind')

# put in coefficients
table2[1,] <- coefficients_cesd_individual(implist_cesd_nfl, 'log2nfl', "e4_discorewgt")
table2[2,] <- coefficients_cesd_individual(implist_cesd_tau, 'log2tau', "e4_discorewgt")
table2[3,] <- coefficients_cesd_individual(implist_cesd_ab40, 'log2ab40', "e4_discorewgt")
table2[4,] <- coefficients_cesd_individual(implist_cesd_ab42, 'log2ab42', "e4_discorewgt")

# write out
write.xlsx(table2, 'O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/tables/table2_crosssectional.xlsx')


#- STANDARDIZED---------------------------------------------------------

# make table
tableS2 <- as.data.frame(matrix(NA, 4,2))
colnames(tableS2) <-c('model 1', 'model 2')
rownames(tableS2) <- rownames(table2) <- c('nfl_ind', 'tau_ind', 'ab40_ind', 'ab42_ind')

# put in coefficients
tableS2[1,] <- coefficients_cesd_individual(implist_cesd_nfl, 'scale(log2nfl)', "rankdep")
tableS2[2,] <- coefficients_cesd_individual(implist_cesd_tau, 'scale(log2tau)', "rankdep")
tableS2[3,] <- coefficients_cesd_individual(implist_cesd_ab40, 'scale(log2ab40)', "rankdep")
tableS2[4,] <- coefficients_cesd_individual(implist_cesd_ab42, 'scale(log2ab42)', "rankdep")

# write out
write.xlsx(tableS2, 'O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/tables/tableS2_crosssectional_standardized.xlsx')


#-------------------------------------------------------------------------
# INCIDENCE DATA ANALYSES - ALL
#-------------------------------------------------------------------------

#-PLOT SURVIVAL-----------------------------------------------------------

# recode NFL to group
df_to_impute$groups_nfl <- NA
df_to_impute$groups_nfl[df_to_impute$log2nfl < mean(df_to_impute$log2nfl, na.rm=T)-sd(df_to_impute$log2nfl, na.rm=T)] <- "Low NfL"
df_to_impute$groups_nfl[df_to_impute$log2nfl > mean(df_to_impute$log2nfl, na.rm=T)-sd(df_to_impute$log2nfl, na.rm=T) & df_to_impute$log2nfl < mean(df_to_impute$log2nfl, na.rm=T)+sd(df_to_impute$log2nfl, na.rm=T)] <- "Mean NfL"
df_to_impute$groups_nfl[df_to_impute$log2nfl > mean(df_to_impute$log2nfl, na.rm=T)+sd(df_to_impute$log2nfl, na.rm=T)] <- "High NfL"

# recode AB40 to group
df_to_impute$groups_ab40 <- NA
df_to_impute$groups_ab40[df_to_impute$log2ab40 < mean(df_to_impute$log2ab40, na.rm=T)-sd(df_to_impute$log2ab40, na.rm=T)] <- "Low ab40"
df_to_impute$groups_ab40[df_to_impute$log2ab40 > mean(df_to_impute$log2ab40, na.rm=T)-sd(df_to_impute$log2ab40, na.rm=T) & df_to_impute$log2ab40 < mean(df_to_impute$log2ab40, na.rm=T)+sd(df_to_impute$log2ab40, na.rm=T)] <- "Mean ab40"
df_to_impute$groups_ab40[df_to_impute$log2ab40 > mean(df_to_impute$log2ab40, na.rm=T)+sd(df_to_impute$log2ab40, na.rm=T)] <- "High ab40"

# recode AB42 to group
df_to_impute$groups_ab42 <- NA
df_to_impute$groups_ab42[df_to_impute$log2ab42 < mean(df_to_impute$log2ab42, na.rm=T)-sd(df_to_impute$log2ab42, na.rm=T)] <- "Low ab42"
df_to_impute$groups_ab42[df_to_impute$log2ab42 > mean(df_to_impute$log2ab42, na.rm=T)-sd(df_to_impute$log2ab42, na.rm=T) & df_to_impute$log2ab42 < mean(df_to_impute$log2ab42, na.rm=T)+sd(df_to_impute$log2ab42, na.rm=T)] <- "Mean ab42"
df_to_impute$groups_ab42[df_to_impute$log2ab42 > mean(df_to_impute$log2ab42, na.rm=T)+sd(df_to_impute$log2ab42, na.rm=T)] <- "High ab42"

# recode TAU to group
df_to_impute$groups_tau <- NA
df_to_impute$groups_tau[df_to_impute$log2tau < mean(df_to_impute$log2tau, na.rm=T)-sd(df_to_impute$log2tau, na.rm=T)] <- "Low tau"
df_to_impute$groups_tau[df_to_impute$log2tau > mean(df_to_impute$log2tau, na.rm=T)-sd(df_to_impute$log2tau, na.rm=T) & df_to_impute$log2tau < mean(df_to_impute$log2tau, na.rm=T)+sd(df_to_impute$log2tau, na.rm=T)] <- "Mean tau"
df_to_impute$groups_tau[df_to_impute$log2tau > mean(df_to_impute$log2tau, na.rm=T)+sd(df_to_impute$log2tau, na.rm=T)] <- "High tau"

# plot survival for status all
kmfit_nfl_all <- survfit(Surv(time/365.25, status_all) ~ groups_nfl, data = df_to_impute)
kmfit_ab40_all <- survfit(Surv(time/365.25, status_all) ~ groups_ab40, data = df_to_impute)
kmfit_ab42_all <- survfit(Surv(time/365.25, status_all) ~ groups_ab42, data = df_to_impute)
kmfit_tau_all <- survfit(Surv(time/365.25, status_all) ~ groups_tau, data = df_to_impute)

# plot survival for status mdd
kmfit_nfl_mdd <- survfit(Surv(time/365.25, status_mdd) ~ groups_nfl, data = df_to_impute)
kmfit_ab40_mdd <- survfit(Surv(time/365.25, status_mdd) ~ groups_ab40, data = df_to_impute)
kmfit_ab42_mdd <- survfit(Surv(time/365.25, status_mdd) ~ groups_ab42, data = df_to_impute)
kmfit_tau_mdd <- survfit(Surv(time/365.25, status_mdd) ~ groups_tau, data = df_to_impute)

# plot, format specific to upper row
plotkm_any <- function(kmfit) {
  gt <- autoplot(kmfit, censor = F, ylim = c(0.8, 1), xlim = c(0,  10), surv.size = 0.5) +
    scale_y_continuous(limits = c(0.6, 1),
                       labels = function(x) {
                         x = c(0, 0.7, 0.8, 0.9, 1)
                         format(x, nsmall = 2)}, 
                       breaks = c(0.60,0.70, 0.80, 0.90, 1.00)) +
  labs(x = " ", y = " ") + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
    theme_classic() +
    theme(legend.position = "none", axis.title = element_blank()) 
  
  gt <- ggplotGrob(gt)
  
  is_yaxis <- which(gt$layout$name == "axis-l")
  yaxis <- gt$grobs[[is_yaxis]]
  
  # You should grab the polyline child
  yline <- yaxis$children[[1]]
  
  yline$x <- unit(rep(1, 4), "npc")
  yline$y <- unit(c(0, 0.1, 1, 0.15), "npc")
  yline$id <- c(1, 1, 2, 2)
  yline$arrow <- arrow(angle = 90, length = unit(0.10, 'inches'))
  
  yaxis$children[[1]] <- yline
  
  gt$grobs[[is_yaxis]] <- yaxis
  
  # grid plotting syntax
  grid.newpage(); grid.draw(gt)      
  
  return(gt)
  
}

# plot the full row
png("O:\\medewerkers\\042647 Schuurmans, I\\Project_21_MDD_NFL\\Tables\\figure1.png", width = 6000, height = 3000,res = 600,type = "cairo-png")

plot_grid(plotkm_any(kmfit_nfl_all),
          plotkm_any(kmfit_tau_all),
          plotkm_any(kmfit_ab40_all),
          plotkm_any(kmfit_ab42_all),
          
          plotkm_any(kmfit_nfl_mdd),
          plotkm_any(kmfit_tau_mdd),
          plotkm_any(kmfit_ab40_mdd),
          plotkm_any(kmfit_ab42_mdd),

          nrow = 2, ncol = 4)

dev.off()

# just to get the legend
autoplot(kmfit_nfl_all, censor = F, surv.size = 1) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  theme_classic() +
  theme(legend.position = "bottom") 

#-NON STANDARDIZED ---------------------------------------------------------

# make table
incidence_individual_all <- incidence_individual_mdd <- as.data.frame(matrix(NA, 4,2))
rownames(incidence_individual_all) <- rownames(incidence_individual_mdd) <- 
  c('nfl_ind', 'tau_ind', 'ab40_ind', 'ab42_ind')
colnames(incidence_individual_all) <- colnames(incidence_individual_mdd) <- 
  c('model 1', 'model 2')

# put in coefficients - ALL OUTCOMES
incidence_individual_all[1,] <- coefficients_inci_individual(implist_inci_nfl, 'log2nfl', 'status_all')
incidence_individual_all[2,] <- coefficients_inci_individual(implist_inci_tau, 'log2tau', 'status_all')
incidence_individual_all[3,] <- coefficients_inci_individual(implist_inci_ab40, 'log2ab40', 'status_all')
incidence_individual_all[4,] <- coefficients_inci_individual(implist_inci_ab42, 'log2ab42', 'status_all')

# put in coefficients - MDD OUTCOMES
incidence_individual_mdd[1,] <- coefficients_inci_individual(implist_inci_nfl, 'log2nfl', 'status_mdd')
incidence_individual_mdd[2,] <- coefficients_inci_individual(implist_inci_tau, 'log2tau', 'status_mdd')
incidence_individual_mdd[3,] <- coefficients_inci_individual(implist_inci_ab40, 'log2ab40', 'status_mdd')
incidence_individual_mdd[4,] <- coefficients_inci_individual(implist_inci_ab42, 'log2ab42', 'status_mdd')

# write out
table3 <- cbind(incidence_individual_all, incidence_individual_mdd)
write.xlsx(table3, 'O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/tables/table3_incidence.xlsx')

#-STANDARDIZED---------------------------------------------------------

# make table
incidence_individual_all_z <- incidence_individual_mdd_z <- as.data.frame(matrix(NA, 4,2))
rownames(incidence_individual_all_z) <- rownames(incidence_individual_mdd_z) <- 
  c('nfl_ind', 'tau_ind', 'ab40_ind', 'ab42_ind')
colnames(incidence_individual_all_z) <- colnames(incidence_individual_mdd_z) <- 
  c('model 1', 'model 2')

# put in coefficients - ALL OUTCOMES
incidence_individual_all_z[1,] <- coefficients_inci_individual(implist_inci_nfl, 'scale(log2nfl)', 'status_all')
incidence_individual_all_z[2,] <- coefficients_inci_individual(implist_inci_tau, 'scale(log2tau)', 'status_all')
incidence_individual_all_z[3,] <- coefficients_inci_individual(implist_inci_ab40, 'scale(log2ab40)', 'status_all')
incidence_individual_all_z[4,] <- coefficients_inci_individual(implist_inci_ab42, 'scale(log2ab42)', 'status_all')

# put in coefficients - MDD OUTCOMES
incidence_individual_mdd_z[1,] <- coefficients_inci_individual(implist_inci_nfl, 'scale(log2nfl)', 'status_mdd')
incidence_individual_mdd_z[2,] <- coefficients_inci_individual(implist_inci_tau, 'scale(log2tau)', 'status_mdd')
incidence_individual_mdd_z[3,] <- coefficients_inci_individual(implist_inci_ab40, 'scale(log2ab40)', 'status_mdd')
incidence_individual_mdd_z[4,] <- coefficients_inci_individual(implist_inci_ab42, 'scale(log2ab42)', 'status_mdd')

# write out
tableS3 <- cbind(incidence_individual_all_z, incidence_individual_mdd_z)
write.xlsx(tableS3, 'O:/medewerkers/042647 Schuurmans, I/Project_21_MDD_NFL/tables/tableS3_incidence_standardized.xlsx')

#-------------------------------------------------------------------------
# GET P_VALUES ALL MODELS FOR CORRECTION 
#-------------------------------------------------------------------------

# make table
p_cesd <- as.data.frame(matrix(NA, 4,4))
colnames(p_cesd) <-c('p model 1',"p adj model 1", 'p model 2', "p adj model 2")
rownames(p_cesd) <- c('nfl_ind', 'tau_ind', 'ab40_ind', 'ab42_ind')

# put in coefficients
p_cesd[1,c(1,3)] <- p_cesd_individual(implist_cesd_nfl, 'log2nfl')
p_cesd[2,c(1,3)] <- p_cesd_individual(implist_cesd_tau, 'log2tau')
p_cesd[3,c(1,3)] <- p_cesd_individual(implist_cesd_ab40, 'log2ab40')
p_cesd[4,c(1,3)] <- p_cesd_individual(implist_cesd_ab42, 'log2ab42')

# p adjust
p_cesd[,2] <- round(p.adjust(p_cesd[,1], method = "BH"),3)
p_cesd[,4] <- round(p.adjust(p_cesd[,3], method = "BH"),3)

# make table
p_inci_all <- as.data.frame(matrix(NA, 4,4))
colnames(p_inci_all) <-c('p model 1',"p adj model 1", 'p model 2', "p adj model 2")
rownames(p_inci_all) <- c('nfl_ind', 'tau_ind', 'ab40_ind', 'ab42_ind')

# put in coefficients
p_inci_all[1,c(1,3)] <- p_inci_all_individual(implist_inci_nfl, 'log2nfl', 'status_all')
p_inci_all[2,c(1,3)] <- p_inci_all_individual(implist_inci_tau, 'log2tau', 'status_all')
p_inci_all[3,c(1,3)] <- p_inci_all_individual(implist_inci_ab40, 'log2ab40', 'status_all')
p_inci_all[4,c(1,3)] <- p_inci_all_individual(implist_inci_ab42, 'log2ab42', 'status_all')

# p adjust
p_inci_all[,2] <- round(p.adjust(p_inci_all[,1], method = "BH"),3)
p_inci_all[,4] <- round(p.adjust(p_inci_all[,3], method = "BH"),3)


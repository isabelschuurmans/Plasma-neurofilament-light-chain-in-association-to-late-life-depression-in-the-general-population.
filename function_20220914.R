get_table1 <- function(df){
  
  table <- data.frame(rep(NA, ncol(df)+1),
                      rep(NA, ncol(df)+1)) 
  
  table[1,2] <- nrow(df)
  
  for (i in 1:ncol(df)){
    
    # get n & % for factor data
    if (is.factor(df[,i]) | is.character(df[,i])){
      
      n <- length(df[which(df[,i]==1),i])
      pt <- round(n/nrow(df)*100,1)
      table[(i+1),2] <- paste0(n, ' (', pt, ')', collapse = '')
      
    }
    
    else {
      
      m <- round(mean(df[,i], na.rm=T),2)
      sd <- round(sd(df[,i], na.rm=T),2)
      table[(i+1),1] <- paste0(m, ' ± ', sd, collapse = '')
      
    }
    
  }
  
  return(table)
  
}

#-------------------------------------------------------------------------------------------------------------------------------------------------

coefficients_cesd_individual <- function(implist, predictor, outcome){
  
  # model 1
  mod1 <- with(implist, lm(as.formula(paste0(outcome,' ~ ', predictor, ' + AGE + sex'))))
  coefs1 <- summary(pool(mod1))
  info1 <-ifelse(coefs1$p.value[2] < .05,
                 paste0(round(coefs1[2,2],2), '* (',
                        round(coefs1[2,2] - 1.96*coefs1[2,3],2), ', ',
                        round(coefs1[2,2] + 1.96*coefs1[2,3],2), ')', sep = ''),
                 paste0(round(coefs1[2,2],2), ' (',
                        round(coefs1[2,2] - 1.96*coefs1[2,3],2), ', ',
                        round(coefs1[2,2] + 1.96*coefs1[2,3],2), ')', sep = ''))
  
  # model 3
  mod3 <- with(implist, lm(as.formula(paste0(outcome,' ~ ', predictor, ' + AGE + sex + education + paidemployment + smoking + alcohol + bmi + eGFRcreat_RF_e4'))))
  coefs3 <- summary(pool(mod3))
  info3 <-ifelse(coefs3$p.value[2] < .05,
                 paste0(round(coefs3[2,2],2), '* (',
                        round(coefs3[2,2] - 1.96*coefs3[2,3],2), ', ',
                        round(coefs3[2,2] + 1.96*coefs3[2,3],2), ')', sep = ''),
                 paste0(round(coefs3[2,2],2), ' (',
                        round(coefs3[2,2] - 1.96*coefs3[2,3],2), ', ',
                        round(coefs3[2,2] + 1.96*coefs3[2,3],2), ')', sep = ''))
  
  # write out info
  return(c(info1, info3))
}

#-------------------------------------------------------------------------------------------------------------------------------------------------

coefficients_cesd_simultaneous <- function(implist, predictor){
  
  # model 1
  mod1 <- with(implist, lm(as.formula(paste0('e4_discorewgt ~ ', predictor, ' + log2nfl + log2tau + log2ab40 + log2ab42 + AGE + sex'))))
  coefs1 <- summary(pool(mod1))
  info1 <-ifelse(coefs1$p.value[2] < .05,
                 paste0(round(coefs1[2,2],2), '* (',
                        round(coefs1[2,2] - 1.96*coefs1[2,3],2), ', ',
                        round(coefs1[2,2] + 1.96*coefs1[2,3],2), ')', sep = ''),
                 paste0(round(coefs1[2,2],2), ' (',
                        round(coefs1[2,2] - 1.96*coefs1[2,3],2), ', ',
                        round(coefs1[2,2] + 1.96*coefs1[2,3],2), ')', sep = ''))
  
  # model 3
  mod3 <- with(implist, lm(as.formula(paste0('e4_discorewgt ~ ', predictor, ' + log2nfl + log2tau + log2ab40 + log2ab42 + AGE + sex + education + paidemployment + smoking + alcohol + bmi + eGFRcreat_RF_e4'))))
  coefs3 <- summary(pool(mod3))
  info3 <-ifelse(coefs3$p.value[2] < .05,
                 paste0(round(coefs3[2,2],2), '* (',
                        round(coefs3[2,2] - 1.96*coefs3[2,3],2), ', ',
                        round(coefs3[2,2] + 1.96*coefs3[2,3],2), ')', sep = ''),
                 paste0(round(coefs3[2,2],2), ' (',
                        round(coefs3[2,2] - 1.96*coefs3[2,3],2), ', ',
                        round(coefs3[2,2] + 1.96*coefs3[2,3],2), ')', sep = ''))
  
  # write out info
  return(c(info1, info3))
}

#-------------------------------------------------------------------------------------------------------------------------------------------------

outcome <- "status_all"
predictor <- "log2nfl"

coefficients_inci_individual <- function(implist, predictor, outcome){
  
  # model 1
  mod1 <- with(implist, coxph(as.formula(paste0('Surv(time, ', outcome, ') ~ ', predictor, ' + AGE + sex'))))
  coefs1 <- summary(pool(mod1))
  info1 <-ifelse(coefs1$p.value[1] < .05,
                 paste0(round(exp(coefs1[1,2]),2), '* (',
                        round(exp(coefs1[1,2] - 1.96*coefs1[1,3]),2), ', ',
                        round(exp(coefs1[1,2] + 1.96*coefs1[1,3]),2), ')', sep = ''),
                 paste0(round(exp(coefs1[1,2]),2), ' (',
                        round(exp(coefs1[1,2] - 1.96*coefs1[1,3]),2), ', ',
                        round(exp(coefs1[1,2] + 1.96*coefs1[1,3]),2), ')', sep = ''))
  
  # model 3
  mod3 <- with(implist, coxph(as.formula(paste0('Surv(time, ', outcome, ') ~ ', predictor, ' + AGE + sex + education + paidemployment + smoking + alcohol + bmi + eGFRcreat_RF_e4'))))
  coefs3 <- summary(pool(mod3))
  info3 <-ifelse(coefs3$p.value[1] < .05,
                 paste0(round(exp(coefs3[1,2]),2), '* (',
                        round(exp(coefs3[1,2] - 1.96*coefs3[1,3]),2), ', ',
                        round(exp(coefs3[1,2] + 1.96*coefs3[1,3]),2), ')', sep = ''),
                 paste0(round(exp(coefs3[1,2]),2), ' (',
                        round(exp(coefs3[1,2] - 1.96*coefs3[1,3]),2), ', ',
                        round(exp(coefs3[1,2] + 1.96*coefs3[1,3]),2), ')', sep = ''))
  
  # write out info
  return(c(info1, info3))
}

#-------------------------------------------------------------------------------------------------------------------------------------------------

coefficients_inci_simultaneous <- function(implist, predictor, outcome){
  
  # model 1
  mod1 <- with(implist, coxph(as.formula(paste0('Surv(time, ', outcome, ') ~ ', predictor, ' + log2nfl + log2tau + log2ab40 + log2ab42 + AGE + sex'))))
  coefs1 <- summary(pool(mod1))
  info1 <-ifelse(coefs1$p.value[1] < .05,
                 paste0(round(exp(coefs1[1,2]),2), '* (',
                        round(exp(coefs1[1,2] - 1.96*coefs1[1,3]),2), ', ',
                        round(exp(coefs1[1,2] + 1.96*coefs1[1,3]),2), ')', sep = ''),
                 paste0(round(exp(coefs1[1,2]),2), ' (',
                        round(exp(coefs1[1,2] - 1.96*coefs1[1,3]),2), ', ',
                        round(exp(coefs1[1,2] + 1.96*coefs1[1,3]),2), ')', sep = ''))
  
  # model 3
  mod3 <- with(implist, coxph(as.formula(paste0('Surv(time, ', outcome, ') ~ ', predictor, '+ log2nfl + log2tau + log2ab40 + log2ab42 + AGE + sex + education + paidemployment + smoking + alcohol + bmi + eGFRcreat_RF_e4'))))
  coefs3 <- summary(pool(mod3))
  info3 <-ifelse(coefs3$p.value[1] < .05,
                 paste0(round(exp(coefs3[1,2]),2), '* (',
                        round(exp(coefs3[1,2] - 1.96*coefs3[1,3]),2), ', ',
                        round(exp(coefs3[1,2] + 1.96*coefs3[1,3]),2), ')', sep = ''),
                 paste0(round(exp(coefs3[1,2]),2), ' (',
                        round(exp(coefs3[1,2] - 1.96*coefs3[1,3]),2), ', ',
                        round(exp(coefs3[1,2] + 1.96*coefs3[1,3]),2), ')', sep = ''))
  
  # write out info
  return(c(info1, info3))
}


#-------------------------------------------------------------------------------------------------------------------------------------------------

p_cesd_individual <- function(implist, predictor){
  
  # model 1
  mod1 <- with(implist, lm(as.formula(paste0('e4_discorewgt ~ ', predictor, ' + AGE + sex'))))
  coefs1 <- summary(pool(mod1))
  info1 <-round(coefs1$p.value[2],3) 
  
  # model 3
  mod3 <- with(implist, lm(as.formula(paste0('e4_discorewgt ~ ', predictor, ' + AGE + sex + education + paidemployment + smoking + alcohol + bmi + eGFRcreat_RF_e4'))))
  coefs3 <- summary(pool(mod3))
  info3 <-round(coefs3$p.value[2],3)
  
  # write out info
  return(c(info1, info3))
}

#-------------------------------------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------------------------------------

p_inci_all_individual <- function(implist, predictor, outcome){
  
  # model 1
  mod1 <- with(implist, coxph(as.formula(paste0('Surv(time, ', outcome, ') ~ ', predictor, ' + AGE + sex'))))
  coefs1 <- summary(pool(mod1))
  info1 <-round(coefs1$p.value[1],3)
  
  # model 3
  mod3 <- with(implist, coxph(as.formula(paste0('Surv(time, ', outcome, ') ~ ', predictor, ' + AGE + sex + education + paidemployment + smoking + alcohol + bmi + eGFRcreat_RF_e4'))))
  coefs3 <- summary(pool(mod3))
  info3 <-round(coefs3$p.value[1],3)
  
  # write out info
  return(c(info1, info3))
}

library(plyr)
library(tidyverse)
library(tidycensus)

dat <- read.csv("./it_nyc_2008_2017.csv")

#SE for pop_12
#SE for italian_12
#SE for pop_17
#SE for italian_17

get_SE <- function(MOE){
  SE = MOE/1.645
  return(SE)
}

get_crit_value <- function(est1,est2,SE1,SE2){
  numerator <- est1-est2
  SE1_squared <- (SE1)^2
  SE2_squared <- (SE2)^2
  denominator <- SE1_squared + SE2_squared
  denominator <- sqrt(denominator)
  crit <- (numerator)/(denominator)
  return(crit)
}

nan_to_zero <- function(a,b){
  res <- NA
  res <- ifelse(a==0 & b ==0, 0, a/b)
  return(res)
}

prop_se <- function(est_a,est_b,se_a,se_b){
  # necessary variables
  seA_sq <- se_a^2
  seB_sq <- se_b^2
  p <- nan_to_zero(est_a, est_b)
  p_sq <- p^2
  
  #conditions for formula
  pro_se <- NA
  
  if(p < 1 & (p_sq * seB_sq < seA_sq)){
    pro_se <- (1/est_b) * sqrt(seA_sq - (p_sq*seB_sq))
  } else if(p < 1 & (p_sq * seB_sq > seA_sq)){
    pro_se <- (1/est_b) * sqrt(seA_sq + (p_sq*seB_sq))
  } else if(p == 1){
    pro_se <- se_a / est_b
  } else {
    pro_se <- NA
  }
  return(pro_se)     
}

prop_se <- Vectorize(prop_se)

# obtain standard errors
SE_est <- data.frame(apply(dat[,c(6,8,13,15)],2,get_SE))
names(SE_est) <- c("Pop_SE_12", "Italian_SE_12", "Pop_SE_17", "Italian_SE_17")
dat <- cbind(dat, SE_est)

# obtain crit values for italian population 
dat$italian_pop_crit_value <- abs(get_crit_value(est1=dat$TR_Italian_17, est2 = dat$TR_Italian_12, SE1 = dat$Italian_SE_17,
                                             SE2 = dat$Italian_SE_12))

# difference in total reporting italian ancestry (2008-2012) v. (2013-2017) statistically significant? 
dat$diff_tr_italian_ancestry_statistically_sig <- ifelse(dat$italian_pop_crit_value > 1.645, "stat sig", "not stat sig")

# create variable for proportion of Italians in 2012 and proportion of Italians in 2017
dat$pro_2012 <- nan_to_zero(dat$TR_Italian_12, dat$TotalPop_12)
dat$pro_2017 <- nan_to_zero(dat$TR_Italian_17, dat$TotalPop_17)

# obtain se for proportion 2012 + 2017
dat$prop_se_2012 <- prop_se(dat$TR_Italian_12, dat$TotalPop_12, dat$Italian_SE_12, dat$Pop_SE_12)
dat$prop_se_2017 <- prop_se(dat$TR_Italian_17, dat$TotalPop_17, dat$Italian_SE_17, dat$Pop_SE_17)

# obtain crit values for proportion  
dat$prop_ital_crit <- (dat$pro_2017 - dat$pro_2012)/sqrt((dat$prop_se_2017)^2+ (dat$prop_se_2012)^2)

dat$prop_ital_sig <- ifelse(dat$prop_ital_crit > 1.645 | dat$prop_ital_crit < -1.645, "stat sig", "not stat sig")

write_csv(dat, "it_nyc_2008_2017_est.csv")

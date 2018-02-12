## Function to run age and sex cohorts

#Required package dplyr

require(dplyr)

run_cohorts <- function(in_idata, in_sex, in_mid_age)
{
  # Create a Life Table data frame
 
  # Filter in_idata by age and select columns for lifetable calculations
  lf_df <- filter(in_idata, age >= in_mid_age & sex == in_sex) %>% select(sex, age, qx, pyld_rate, mx)
  
  # Create list of required columns (variables)
  # number of survivors
  lf_df$lx <- 0
  # Create it for males population
  lf_df$lx[1] <- filter(in_idata, age == in_mid_age & sex == in_sex) %>% select(five_year_population)
  lf_df$lx <- as.numeric(lf_df$lx)
  
  # number died
  lf_df$dx <- 0
  # Create it for males population
  lf_df$dx[1] <- lf_df$lx[1] * lf_df$qx[1]
  
  for (i in 2:nrow(lf_df)){
    lf_df$lx[i] <- lf_df$lx[i - 1] - lf_df$dx[i - 1]
    lf_df$dx[i] <- lf_df$lx[i] * lf_df$qx[i]
  }
  
  # number of persons lived by cohort to age x + 1/2 (average people)
  lf_df$Lx <- 0
  
  for (i in 1:nrow(lf_df)){
    if (i < nrow(lf_df))
      lf_df$Lx[i] <- sum(lf_df$lx[i] + lf_df$lx[i + 1]) / 2
    else
      lf_df$Lx[i] <- lf_df$lx[i] / lf_df$mx[i]
    
  }
  
  # Create life expectancy variable
  for (i in 1:nrow(lf_df)){
    lf_df$ex[i] <- sum(lf_df$Lx[i:nrow(lf_df)]) / lf_df$lx[i]
  }
  
  # Create health adjusted life years variable
  lf_df$Lwx <- lf_df$Lx * (1 - lf_df$pyld_rate)
  
  # Create health adjusted life expectancy variable
  for (i in 1:nrow(lf_df)){
    lf_df$ewx[i] <- sum(lf_df$Lwx[i:nrow(lf_df)]) / lf_df$lx[i]
  }
  
  lf_df
  
}


# Create disease Life Table data frame function
    
  run_disease <- function(in_idata, in_mid_age, in_sex, in_disease) 
  {
    
  #Create disease variable for the disease life table function 
    
  disease <- c("ihd", "istroke", "diabetes", "colon_cancer", "breast_cancer")
  
  in_disease <- names(idata %>% select(contains("disease")))
  
  #Input disease
  
  incidence <- paste("incidence", disease, sep = "_")
  case_fatality <- paste("case_fatality", disease, sep = "_")
  dw <- paste("dw", disease, sep = "_")
  

  # Filter in_idata by age and select columns for lifetable calculations
  dlt_df <- filter(idata == in_idata, age >= in_mid_age, sex == in_sex & disease == in_disease ) %>% select(sex, age, incidence_ihd, case_fatality_ihd, dw_ihd, incidence_istroke, case_fatality_istroke, dw_istroke, incidence_diabetes, case_fatality_diabetes, dw_diabetes, breast_cancer_incidence, breast_cancer_case_fatality, breast_cancer_dw, colon_cancer_incidence, colon_cancer_case_fatality, colon_cancer_dw)
    
    #Create list of required columns
    ##Intermediate variables lx, qx, wx and vx
    ###lx
    dlt_df$lx <- 0
    dlt_df$lx <- idata$incidence + idata$case_fatality
    ###qx
    dlt_df$qx <- 0
    dlt_df$qx <-  sqrt((idata$incidence- idata$case_fatality)*(idata$incidence - idata$case_fatality))
    ### wx
    dlt_df$wx <- exp(-1*(dlt_df$lx+dlt_df$qx)/2)
    ### vx
    dlt_df$vx <- exp(-1*(dlt_df$lx-dlt_df$qx)/2)
    
  }

  
    
    

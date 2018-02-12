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
  
  
  # Uncomment the variables below to debug your code  
  # in_idata = sub_idata
  # in_sex = "males"
  # in_mid_age = 22
  # in_disease = "ihd"
  
  # Create disease variable for the disease life table function 
  dw_disease <- paste("dw", in_disease, sep = "_")
  incidence_disease <- paste("incidence", in_disease, sep = "_")
  case_fatality_disease <- paste("case_fatality", in_disease, sep = "_")
  
  ## Add generic variable names to the source data frame (in_idata)
  in_idata$dw_disease <- in_idata[[dw_disease]]
  in_idata$incidence_disease <- in_idata[[incidence_disease]]
  in_idata$case_fatality_disease <- in_idata[[case_fatality_disease]]
  
  # Filter in_idata by age and select columns for lifetable calculations
  dlt_df <- filter(in_idata, age >= in_mid_age & sex == in_sex) %>% 
    select(sex, age, dw_disease, incidence_disease, case_fatality_disease)
  
  #Create list of required columns
  ##Intermediate variables lx, qx, wx and vx
  ###lx
  dlt_df$lx <- dlt_df$incidence_disease + dlt_df$case_fatality_disease
  ###qx
  dlt_df$qx <-  sqrt((dlt_df$incidence_disease - dlt_df$case_fatality_disease) * (dlt_df$incidence_disease - dlt_df$case_fatality_disease))
  ### wx
  dlt_df$wx <- exp(-1*(dlt_df$lx+dlt_df$qx)/2)
  ### vx
  dlt_df$vx <- exp(-1*(dlt_df$lx-dlt_df$qx)/2)
  ## Healthy (Sx), Disease (Cx) and Death (Dx)
  
  dlt_df$Sx <- 0
  dlt_df$Cx <- 0
  dlt_df$Dx <- 0
  
  for (i in 1:nrow(dlt_df)){
    if (dlt_df$age[i] == in_mid_age){
      dlt_df$Sx[i] <- 1000
      dlt_df$Cx[i] <- 0
      dlt_df$Dx[i] <- 0
    }
    else{
      dlt_df$Sx[i] <- ifelse(dlt_df$qx[i-1] > 0, ((2*(dlt_df$vx[i-1]-dlt_df$wx[i-1])*(dlt_df$Sx[i-1]*(dlt_df$case_fatality_disease[i-1]+0)+dlt_df$Cx[i-1]*0)+dlt_df$Sx[i-1]*(dlt_df$vx[i-1]*(dlt_df$qx[i-1]-dlt_df$lx[i-1])+dlt_df$wx[i-1]*(dlt_df$qx[i-1]+dlt_df$lx[i-1])))) / 2 * (dlt_df$qx[i-1]), dlt_df$Sx[i - 1] )
      dlt_df$Cx[i] <- ifelse(dlt_df$qx[i-1] > 0, -1*((dlt_df$vx[i-1]-dlt_df$wx[i-1])*(2*((dlt_df$case_fatality_disease[i-1]+0+0)*(dlt_df$Sx[i-1]+dlt_df$Cx[i-1])-dlt_df$lx[i-1]*dlt_df$Sx[i-1]+0*dlt_df$Sx[i-1])-dlt_df$Cx[i-1]*dlt_df$lx[i-1])-dlt_df$Cx[i-1]*dlt_df$qx[i-1]*(dlt_df$vx[i-1]+dlt_df$wx[i-1])) / 2 * (dlt_df$qx[i-1]), dlt_df$Cx[i - 1] )
      dlt_df$Dx[i] <- ifelse(dlt_df$qx[i-1] > 0, ((dlt_df$vx[i-1] - dlt_df$wx[i-1]) * 
                               (2 * dlt_df$case_fatality_disease[i-1] * 
                                  dlt_df$Cx[i-1] - dlt_df$lx[i-1]*
                                  (dlt_df$Sx[i-1] + dlt_df$Cx[i-1]))
                                   - dlt_df$qx[i-1] * (dlt_df$Sx[i-1] + dlt_df$Cx[i-1])
                                      *(dlt_df$vx[i-1]+dlt_df$wx[i-1]) + 2 * dlt_df$qx[i-1] * 
                                       (dlt_df$Sx[i-1]+dlt_df$Cx[i-1]+dlt_df$Dx[i-1])) / 2 * (dlt_df$qx[i-1]), dlt_df$Cx[i - 1] )
    }
  }
  
  dlt_df
  
}

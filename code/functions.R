## Function to run age and sex cohorts



#Required packages

require(dplyr)
require(tidyverse)

#############################Function for general life table###################################

run_life_table <- function(in_idata, in_sex, in_mid_age)
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


#############################Function for disease life tables###################################

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
  
  ## Healthy (Sx), Disease (Cx) and Death (Dx), total (Tx) (control check, has to be 1000), total alive (Ax)
  ##persons years live at risk (PYx), prevalence rate (px), mortality rate (mx)
  ###Remission and mortality from other causes were replaced by zero in the formulas (as we assume no remission and independence of disease mortality with total mortlaity). 
  
  ####First create empty variables
  
  dlt_df$Sx <- 0
  dlt_df$Cx <- 0
  dlt_df$Dx <- 0
  dlt_df$Tx  <- 0
  dlt_df$Ax <- 0
  dlt_df$PYx <- 0
  dlt_df$px <- 0
  dlt_df$mx <- 0
  
  #####Start with variables without calculation exceptions
  
  for (i in 1:nrow(dlt_df)){
    dlt_df$Tx   <- dlt_df$Sx + dlt_df$Cx + dlt_df$Dx 
    dlt_df$Ax <- dlt_df$Sx + dlt_df$Cx
    
    
    #####Variables with exceptions  
    
    for (i in 1:nrow(dlt_df)){
      if (i < nrow(dlt_df))
        dlt_df$PYx[i] <- sum(dlt_df$Ax[i] + dlt_df$Ax[i + 1])/2
      else
        dlt_df$PYx[i] <- 0
      
      
      if(is.na(sum(dlt_df$Cx[i] + dlt_df$Cx[i + 1])/2)) { 
        dlt_df$px[i] <- 0
      }
      else{
        dlt_df$px[i] <- (sum(dlt_df$Cx[i] + dlt_df$Cx[i + 1])/2) / dlt_df$PYx[i]    
        
        
        
        if ((dlt_df$Dx[i+1] - dlt_df$Dx[i]) < 0){
          dlt_df$mx[i] <- 0
        }
        else{
          dlt_df$mx[i] <- ((dlt_df$Dx[i+1] - dlt_df$Dx[i])/dlt_df$PYx[i])
          
          
          if (dlt_df$age[i] == in_mid_age){
            dlt_df$Sx[i] <- 1000
            dlt_df$Cx[i] <- 0
            dlt_df$Dx[i] <- 0
            
          }
          else{
            dlt_df$Sx[i] <- ifelse(dlt_df$qx[i-1] > 0, (2*(dlt_df$vx[i-1] - dlt_df$wx[i-1]) * 
                                                          (dlt_df$Sx[i-1] * (dlt_df$case_fatality_disease [i-1] + 0 +0) + 
                                                             dlt_df$Cx[i-1] * 0) + dlt_df$Sx[i-1] * (dlt_df$vx[i-1] * (dlt_df$qx[i-1] 
                                                                                                                       - dlt_df$lx[i-1]) + dlt_df$wx[i-1] * (dlt_df$qx[i-1] + dlt_df$lx[i-1]))) 
                                   / (2 * (dlt_df$qx[i-1])), dlt_df$Sx[i - 1] )
            dlt_df$Cx[i] <- ifelse(dlt_df$qx[i-1] > 0, -1*((dlt_df$vx[i-1] - dlt_df$wx[i-1])*
                                                             (2*((dlt_df$case_fatality_disease[i-1] + 0 + 0) * 
                                                                   (dlt_df$Sx[i-1]+dlt_df$Cx[i-1]) - dlt_df$lx[i-1] * dlt_df$Sx[i-1] + 0 * 
                                                                   dlt_df$Sx[i-1]) - dlt_df$Cx[i-1] * dlt_df$lx[i-1]) - dlt_df$Cx[i-1] * 
                                                             dlt_df$qx[i-1] * (dlt_df$vx[i-1]+dlt_df$wx[i-1])) 
                                   / (2 * (dlt_df$qx[i-1])), dlt_df$Cx[i - 1])
            dlt_df$Dx[i] <- ifelse(dlt_df$qx[i-1] > 0, ((dlt_df$vx[i-1] - dlt_df$wx[i-1]) * 
                                                          (2 * dlt_df$case_fatality_disease[i-1] * 
                                                             dlt_df$Cx[i-1] - dlt_df$lx[i-1]*
                                                             (dlt_df$Sx[i-1] + dlt_df$Cx[i-1]))
                                                        - dlt_df$qx[i-1] * (dlt_df$Sx[i-1] + dlt_df$Cx[i-1])
                                                        *(dlt_df$vx[i-1]+dlt_df$wx[i-1]) + 2 * dlt_df$qx[i-1] * 
                                                          (dlt_df$Sx[i-1]+dlt_df$Cx[i-1]+dlt_df$Dx[i-1])) 
                                   / (2 * (dlt_df$qx[i-1])), dlt_df$Cx[i - 1])
            
            
          }}}}}
  dlt_df
}


#######################################Function for PIFs########################################

##The code for PIFs will depend on the data sources. 


run_pif <- function(in_idata, i_irr, i_exposure, in_mid_age, in_sex, in_disease, in_met_sc) 
  # 
{
  
  ##Uncomment to debug function
  
  # in_idata = idata
  # i_irr = irr
  # i_exposure = edata
  # in_sex = "females"
  # in_mid_age = 22
  # in_disease = "ihd"
  ###Filter data to use in pif calculations (age and sex). Add rrs, ee and calculations
  
  pif_df <- filter(in_idata, age >= in_mid_age & sex == in_sex) %>%
    select(sex, age)
  
  ###Add varaibles to data.frame (different age category for breast cancer)
  
  pif_df$disease <- in_disease
  
  if(in_disease == "breast_cancer") {
    pif_df$age_cat [pif_df$age <=30] <- 30
    pif_df$age_cat [pif_df$age >30 & pif_df$age <=45 ] <- 45
    pif_df$age_cat [pif_df$age >45 & pif_df$age <=70 ] <- 70
    pif_df$age_cat [pif_df$age >70 & pif_df$age <=100 ] <- 80}
  else {
    pif_df$age_cat [pif_df$age <=30] <- 30
    pif_df$age_cat [pif_df$age >30 & pif_df$age <=70 ] <- 70
    pif_df$age_cat [pif_df$age >70 & pif_df$age <=100 ] <- 80
    
    pif_df$sex_cat <- ifelse(in_disease == "breast_cancer", "female", "female_male")
    
    ##Create concatenated variables to match pif_df with i_irr
    pif_df$sex_age_dis_cat <- paste(pif_df$disease,pif_df$age_cat, pif_df$sex_cat, sep = "_"  )
    i_irr$sex_age_dis_cat <- paste(i_irr$disease,i_irr$age, i_irr$sex, sep = "_"  )
    
    # Remove sex, age and disease variables from i_irr df, as they are not needed
    i_irr <- select(i_irr, -one_of('sex','age', 'disease'))
    
    ## The code below is working but copies age, sex and disease for x and y, how can this be avoided?
    pif_df <-  inner_join(pif_df, i_irr, by = c("sex_age_dis_cat" = "sex_age_dis_cat") , copy = FALSE)
    
    ## Creation of splineFun which uses baseline's RR and EE to use to estimate intervention RRs
    for (i in 1:nrow(pif_df)){
      sp_obj <-  splinefun(y = c(pif_df$rr_inactive[i], 
                                 pif_df$rr_insufficiently_active[i], 
                                 pif_df$rr_recommended_level_active[i], 
                                 pif_df$rr_highly_active[i]), 
                           x = c(pif_df$ee_inactive[i], 
                                 pif_df$ee_insufficiently_active[i], 
                                 pif_df$ee_recommended_level_active[i], 
                                 pif_df$ee_highly_active[i]), 
                           method = "hyman")
      
      ## Use created spline function above to estimate intervention RRs
      
      pif_df$sc_rr_inactive[i] <- sp_obj(x = pif_df$ee_inactive[i] + in_met_sc)
      pif_df$sc_rr_insufficiently_active[i] <-  sp_obj(x = pif_df$ee_insufficiently_active[i] + in_met_sc)
      pif_df$sc_rr_recommended_level_active[i] <-  sp_obj(x = pif_df$ee_recommended_level_active[i] + in_met_sc)
      pif_df$sc_rr_highly_active[i] <-  sp_obj(x = pif_df$ee_highly_active[i] + in_met_sc)
      
      # plot(sp_obj, xlab = "RR", ylab = "EE", main = paste("Spline ", i))
    }
    
    
    ## round sc_rr_highly_active column - it should be 1
    pif_df$sc_rr_highly_active <- round(pif_df$sc_rr_highly_active)
    
    ##Calculate PIFs. I already process the data to generate categories in stata.
    ##First add PA categories to pif_df
    
    pif_df$sex_age_cat <- paste(pif_df$sex, pif_df$age, sep = "_"  )
    i_exposure$sex_age_cat <- paste(i_exposure$sex, i_exposure$age, sep = "_"  )
    
    # Remove sex, age and disease variables from i_irr df, as they are not needed
    i_exposure <- select(i_exposure, -one_of('sex','age'))
    
    #Join edata (PA prevalence to pif_df)
    
    pif_df <-  inner_join(pif_df, i_exposure, by = c("sex_age_cat" = "sex_age_cat") , copy = FALSE)
    
    
    ##We need to adapt to ITHIMR developments. REPLACE DATA FRAME FROM WHICH PREVALENCE OF PA IS TAKEN
    
    pif_df$pif <- 1-(pif_df$sc_rr_inactive *pif_df$inactive +
                       pif_df$sc_rr_insufficiently_active*pif_df$insufficiently_active +
                       pif_df$sc_rr_recommended_level_active*pif_df$recommended_level_active +
                       pif_df$sc_rr_highly_active *pif_df$highly_active)/
      (pif_df$rr_inactive *pif_df$inactive  +
         pif_df$rr_insufficiently_active *pif_df$insufficiently_active +
         pif_df$rr_recommended_level_active *pif_df$recommended_level_active +
         pif_df$rr_highly_active *pif_df$highly_active)
  }
  
  pif_df
  
  }




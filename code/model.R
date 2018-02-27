  ##########################Prepare data and environment to run code###########################################################
  
  ##### Clean the Global Environment
  
  rm (list = ls())
  
  ##### Prevent scientific notation in data frame
  
  options(scipen=999)
  
  ##### Load all functions
  
  source("code/functions.R")
  
  ##### Read data. idata: life table and disease life tables (including trends). edata: exposure data (e.g. physical activity).
  #### irr.data: relative risks data and ee: energy expenditure
  #### For exposure (edata) and input (idata) data I am using already generated categories, but a function should be developed to work from raw data. 
  
  idata <- read.csv("data/idata.csv", stringsAsFactors = F)
  edata <- read.csv("data/edata.csv", stringsAsFactors = F)
  irr <- read.csv("data/irr.csv", stringsAsFactors = F)
  ee <- read.csv("data/ee.csv", stringsAsFactors = F)
  
  #####To do
  
  ## Note that disease trends are excluded for now. We need to generate a trends function for diseases, mortality (all cause), 
  ## change in disease due to change in risk factor exposure (discuss with Niel M), road injuries effect over time. 
  ## Eventually, we want to add (1) CRA ITHIM approach to compare results and (2) use all cause mortaltiy instead of 
  ## disease specific dose responses for incidence to understand the difference in results for all cause mortality. 
  
  #################################Model parameters#########################################################################
  
  p_age_cohort <- c(22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)
  
  p_sex <- c("males", "females")
  
  p_disease <- c("ihd", "istroke", "diabetes", "colon_cancer", "breast_cancer")
  
  ###As an expample, an increase in 100 METs per week
  
  p_intervention_effect <- 100
  
  ##############################Prepare general life table (age, death rate, population #)########
  
  ##### Make lower cases values for sex column
  
  idata$sex <- tolower(idata$sex)
  
  ##### Rename mortality_rate to mx
  
  idata$mx <- idata$mortality_rate
  
  ##### Remove mortality_rate column
  
  idata$mortality_rate <- NULL
  
  ##### Rename male for males and female for females in edata to match idata. 
  
  edata$sex[edata$sex=="male"] <- "males"
  edata$sex[edata$sex=="female"] <- "females"
  
  ##### Create new variable for 5_year population (age cohorts). Depends on the age-cohorts of interest (5-yrs here)
  
  idata$five_year_population <- NA
  start_index <- 3
  index <- 1
  end_index <- 4
  for (i in 1:20){
    if (i == 1)
      index <- 1
    else 
      index <- ((i - 1)  * 5) + 1
    if (index == 96){
      end_index <- 5
    }
    cat("population sum ", start_index, "start index ", index, " and end index ", index + end_index, "\n")
    idata$five_year_population[start_index] <- sum(idata$population[index:(index + end_index)])
    start_index <- start_index + 5
  }
  
  end_index <- 4
  start_index <- 104
  
  for (i in 1:20){
    if (i == 1)
      index <- 102
    else
      index <- (((i - 1)  * 5) + 1) + 101
    if (index == 197){
      end_index <- 5
    }
    cat("population sum ", start_index, "start index ", index, " and end index ", index + end_index, "\n")
    idata$five_year_population[start_index] <- sum(idata$population[index:(index + end_index)])
    start_index <- start_index + 5
  }
  
  
  
  ##### Generate list of general life tables (baseline and scenario), disease life tables (baseline and sceanrio),
  ##### potential impact fraction (pif, one for now, number depends on interventions), and general and disease life
  ##### tables for interventions. 
  
  ##########################Prepare general life table###########################################
  
  ##### General lifetable. Starts from population numbers per one year interval and mortality rates
  ##### mortaltiy rates = deaths (1-yr/people 1-yr)
  
  ##### Create a new variable for the probability of dying between age now and now + 1
  ##### Formula = IF(age<100,1-EXP(-mortality rate),1)
  
  
  idata$qx <- ifelse(idata$age < 100, 1 - exp(-1 * idata$mx), 1)
  
  ########################Generate baseline (bl) general life tables##################################
  
  #### p_age_cohort and p_sex are defined parameters at the start of the code. Here, we generate a general
  #### life table per age and sex. 

  general_life_table_list_bl <- list()
  index <- 1
  
  for (age in p_age_cohort){
    for (sex in p_sex){
      cat("age ", age, " and sex ", sex, "\n")
      general_life_table_list_bl[[index]] <- run_life_table(in_idata = idata, in_sex = sex, in_mid_age = age)
      index <- index + 1
    }
  }
  
  ##### Uncommnet to check life table list
  # View(general_life_table_list_bl[[1]])
  
  ######################Generate baseline disease life tables##################################
  
  ##### p_age_cohort, p_sex and p_disease are defined parameters at the start of the code. Here, we generate a disease
  ##### life table per age, sex and disease. 
  
  disease_life_table_list_bl <- list()
  index <- 1
  
  for (age in p_age_cohort){
    for (sex in p_sex){
      for (disease in p_disease) {
      # Exclude breast_cancer for Males
      if (sex == "males" && disease == "breast_cancer"){
        cat("\n")
      }
      else {
        cat("age ", age, " sex ", sex, "and disease", disease, "\n")
        disease_life_table_list_bl[[index]] <- run_disease(in_idata = idata, in_sex = sex, in_mid_age = age, in_disease = disease)
        index <- index + 1
        }
      }
    }
  }
  
  ##### Uncommnet to check disease life table list
  # View(disease_life_table_list_bl[[1]])
  
  #######################Generate pifs#########################################################
  
  ##### p_age_cohort, p_sex, p_disease and p_intervention_effect are defined parameters at the start of the code. 
  ##### Here, we generate a pif per age, sex and disease.  
  
  pifs <- list()
  index <- 1
  
  for (age in p_age_cohort){
    for (sex in p_sex){
      for (disease in p_disease) {
        for (effect in p_intervention_effect) {
          # Exclude breast_cancer for Males
          if (sex == "males" && disease == "breast_cancer"){
            cat("\n")
          }
          else {
            cat("age ", age, " sex ", sex, "disease", disease, "and effect", effect,  "\n")
            pifs[[index]] <- run_pif(in_idata = idata, i_irr = irr, i_exposure = edata, in_mid_age = age, in_sex = sex, in_disease = disease, in_met_sc = effect) 
            index <- index + 1
          }
        }
      }
    }
  }
  
  ##### Uncommnet to check pifs
  # View(pifs[[1]])
  
  ########################Scenario calculations####################################################
  
  ##### The mechanism of change start with incidence in the disease life tables. Incidence is modified with 
  ##### (1-PIF)
  
  
  #####Generate scenario incidence (for each disease)

   incidence_sc <- list()
   index <- 1
  
   for (age in p_age_cohort){
     for (sex in p_sex){
       for (disease in p_disease) {
  
           # Exclude breast_cancer for Males
           if (sex == "males" && disease == "breast_cancer"){
             cat("\n")
           }
           else {
  
             incidence_sc[[index]] <- disease_life_table_list_bl[[index]]$incidence_disease * (1-(pifs[[index]]$pif))
             index <- index + 1
  
         }
       }
     }
   }
  
  ##### Uncommnet to check scenario incidence
View(incidence_sc[[1]])
  
  
  ##### Calculate disease life tables with new incidence (this in turn, modifies prevalence and mortality)
  
  disease_life_table_list_sc <- list()
  index <- 1


  for (age in p_age_cohort){
    for (sex in p_sex){
      for (disease in p_disease) {
        # Exclude breast_cancer for Males
        if (sex == "males" && disease == "breast_cancer"){
          cat("\n")
        }
        else {
          
          cat("age ", age, " sex ", sex, "and disease", disease, "\n")
          # modify idata's incidence for the said scenario
          td <- idata
          td[td$age >= age & td$sex == sex,][[paste("incidence", disease, sep = "_")]] <- incidence_sc[[index]]
          
          # Instead of idata, feed td to run scenarios
          disease_life_table_list_sc[[index]] <- run_disease(in_idata = td, in_sex = sex, in_mid_age = age, in_disease = disease)
          disease_life_table_list_sc[[index]]$diff_inc_disease <- disease_life_table_list_sc[[index]]$incidence_disease - disease_life_table_list_bl[[index]]$incidence_disease
          disease_life_table_list_sc[[index]]$diff_prev_disease <- disease_life_table_list_sc[[index]]$px - disease_life_table_list_bl[[index]]$px
          disease_life_table_list_sc[[index]]$diff_mort_disease <- disease_life_table_list_sc[[index]]$mx - disease_life_table_list_bl[[index]]$mx
          
          index <- index + 1
        }
      }
    }
  }
  ##### Uncommnet to check scenario life tables
  # View(disease_life_table_list_sc[[1]])


  ##########################################Calculate new life table parameters###########################################

  ###Generate total change in mortality rate
  ##### Sum mortality rate scenarios (mx_sc_total)
  ####THE FUNCTION IS ONLY PICKING UP THE CHANGE IN THE LAST DISEASE IN THE INDEX (colon cancer), 
  ####IT IS NOT SUMMING UP ALL CHANGES

  mx_sc_total <- list()
  l_index <- 1
  index <- 1
  for (age in p_age_cohort){
    for (sex in p_sex){
      mortality_sum <- NULL
      create_new <- T
      
      for (disease in p_disease) {
        if (sex == "males" && disease == "breast_cancer"){
          cat("\n")
        }else{
          
          if (create_new){
            mortality_sum <- select(disease_life_table_list_sc[[index]], c('age', 'sex'))
            mortality_sum$total <- 0
          }else{
            create_new <- F
          }
          
          mortality_sum$total <- mortality_sum$total + (disease_life_table_list_sc[[index]]$diff_mort_disease)
          
          cat(age, " - ", sex," - ",  disease," - ",  index, " - ", l_index,  "\n")
          
          index <- index + 1
        }
        
      }
      mx_sc_total[[l_index]] <- mortality_sum
      l_index <- l_index + 1
    }
  }  
  

  
  ##### Uncommnet to check sceanrio mortality and changes 
  View(mx_sc_total[[2]])
  ####Cross check total difference in disease mortality with total added up in mx_sc_total
  View(disease_life_table_list_sc[[5]]$diff_mort_disease)
  View(disease_life_table_list_sc[[6]]$diff_mort_disease)
  View(disease_life_table_list_sc[[7]]$diff_mort_disease)
  View(disease_life_table_list_sc[[8]]$diff_mort_disease)
  View(disease_life_table_list_sc[[9]]$diff_mort_disease)
  
  
  
  
  ####WIP
  #####Generate total change in prevalent yld rates
  #####total ylds rate + (sum-all diseases change in prevelence*total yld rate)


#   pyld_rate_sc_total <- list()
#   l_index <- 1
#   index <- 1
# 
# 
#   for (age in p_age_cohort){
#     for (sex in p_sex){
#     
#       pyld_rate_sum <- NULL
#       create_new <- T
#     
#     for (disease in p_disease) {
#       if (sex == "males" && disease == "breast_cancer")
#         cat("\n")
#       else{
#         if (create_new){
#           pyld_rate_sum <- select(disease_life_table_list_sc[[index]], c('age', 'sex'))
#           create_new <- F
#         }
#         pyld_rate_sum$total <- (disease_life_table_list_sc[[index]]$diff_prev_disease)*
#           general_life_table_list_bl[[index]]$pyld_rate
#         index <- index + 1
#       }
#       
#       cat(age, " - ", sex," - ",  disease," - ",  index, " - ", l_index,  "\n")
#     }
#       pyld_rate_sc_total[[l_index]] <-  pyld_rate_sum
#     l_index <- l_index + 1
#   }
# }  
# 
# 
# 
# ##### Uncommnet to check sceanrio mortality and changes 
# View(pyld_rate_sc_total[[32]])


  ###TO USE LATER TO GENERATE TOTALS 
  # # Sum of columns
  # 
  # 
  # for (age in p_age_cohort){
  #   for (sex in p_sex){
  #     
  #     mortality_sum <- 0
  #     
  #     for (disease in p_disease) {
  #       
  #       
  #       if (sex == "males" && disease == "breast_cancer")
  #         cat("\n")
  #       else{
  #         mortality_sum <- mortality_sum + sum(disease_life_table_list_sc[[index]]$diff_mort_disease)
  #         # mx_sc_total[[index]] <- reduce(disease_life_table_list_sc[[index]]$diff_mort_disease, sum)
  #         index <- index + 1
  #       }
  #       
  #       cat(age, " - ", sex," - ",  disease," - ",  index, "\n")
  #     }
  #     mx_sc_total[[l_index]] <- mortality_sum
  #     l_index <- l_index + 1
  #   }
  # }    
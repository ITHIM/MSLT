# Remove all variables

rm (list = ls())

##To prevent scientific notation in data frame

options(scipen=999)

# Load all functions

source("code/functions.R")

##Note that disease trends are excluded for now. We need to generate a trends function for diseases, mortality (all cause), 
##change in disease due to change in risk factor exposure (discuss with Niel M), road injuries effect over time. 
##Eventually, we want to add (1) CRA ITHIM approach to compare results and (2) use all cause mortaltiy instead of 
##disease specific dose responses for incidence to understand the difference in results for all cause mortality. 


# Read data. idata: life table and disease life tables (including trends). edata: exposure data (e.g. physical activity).
#irr.data: relative risks data and ee: energy expenditure
##For exposure (edata) and input (idata) data I am using already generated categories, but a function should be developed to work from raw data. 
idata <- read.csv("data/idata.csv", stringsAsFactors = F)
edata <- read.csv("data/edata.csv", stringsAsFactors = F)
irr <- read.csv("data/irr.csv", stringsAsFactors = F)
ee <- read.csv("data/ee.csv", stringsAsFactors = F)

#################################Model parameters#########################################################################

p_age_cohort <- c(22, 27, 32, 37, 42, 47, 52, 57, 62, 67, 72, 77, 82, 87, 92, 97)

p_sex <- c("males", "females")

p_disease <- c("ihd", "istroke", "diabetes", "colon_cancer", "breast_cancer")

###As an expample, an increase in 100 METs per week

p_intervention_effect <- 100

##############################Prepare general life table (age, death rate, population #)########

### Make lower cases values for sex column
idata$sex <- tolower(idata$sex)

### Rename mortality_rate to mx
idata$mx <- idata$mortality_rate

### Remove mortality_rate column
idata$mortality_rate <- NULL

### Rename male for males and female for females in edata to match idata. 

edata$sex[edata$sex=="male"] <- "males"
edata$sex[edata$sex=="female"] <- "females"

# Create new variable for 5_year population (age cohorts). Depends on the age-cohorts of interest (5-yrs here)
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



##########################Prepare general life table###########################################

## General lifetable. Starts from population numbers per one year interval and mortality rates
##mortaltiy rates = deaths (1-yr/people 1-yr)

### Create a new variable for the probability of dying between age now and now + 1
#### Formula = IF(age<100,1-EXP(-mortality rate),1)


idata$qx <- ifelse(idata$age < 100, 1 - exp(-1 * idata$mx), 1)

## Cohort specific age, data and sex settings
####Loops or functions will be better here to generate all cohorts data for baseline and intervention at the same time and save values. 
# In this case we are generating cohorts for females, mid_aged 27

########################Generate baseline general life tables##################################

general_life_table_list_bl <- list()
index <- 1

for (age in p_age_cohort){
  for (sex in p_sex){
    cat("age ", age, " and sex ", sex, "\n")
    general_life_table_list_bl[[index]] <- run_life_table(in_idata = idata, in_sex = sex, in_mid_age = age)
    index <- index + 1
  }
}

###Uncommnet to check life table list
# str(general_life_table_list_bl)

######################Generate baseline disease life tables##################################

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

###Uncommnet to check disease life table list
# str(disease_life_table_list_bl)

#######################Generate pifs#########################################################

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

###Uncommnet to check pifs
# str(pifs)


########################Sceanrio calculations####################################################

######Create scenario disease life tables (incidence should change here)

##Extract PIF values from pif lists
pifs[[24]]

##Create disease life table scenario and multiply incidence rate by (1-PIF)

##DO WE NEED A LOOP HERE TO CREATE A SCENARIOS BY AGE, SEX AND DISEASE
disease_life_table_list_sc <- disease_life_table_list_bl 

## Modify individual incidence in disease life table list
for (i in 1:length(disease_life_table_list_sc)){
  cat("index ", i," \n")
  disease_life_table_list_sc[[i]]$incidence_disease <-   disease_life_table_list_sc[[i]]$incidence_disease *
                                                          (1-(pifs[[i]]$pif))
}

# See the first disease lifetable in viewer
View(disease_life_table_list_sc[[1]])
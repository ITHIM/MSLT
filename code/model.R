# Remove all variables

rm (list = ls())

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


##Prepare input data. Depends on data sources

### Make lower cases values for sex column
idata$sex <- tolower(idata$sex)

### Rename mortality_rate to mx
idata$mx <- idata$mortality_rate

### Remove mortality_rate column
idata$mortality_rate <- NULL

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

#Generate life tables: general life table and disease specific life tables. 

## General lifetable. Starts from population numbers per one year interval and mortality rates
##mortaltiy rates = deaths (1-yr/people 1-yr)

### Create a new variable for the probability of dying between age now and now + 1
#### Formula = IF(age<100,1-EXP(-mortality rate),1)


idata$qx <- ifelse(idata$age < 100, 1 - exp(-1 * idata$mx), 1)


##General life table calculations: for cohorts, we need to set start and end age. 
###May be best to move this at the begining of the code. 
#### Assume start and end age specified by users
##### For now we assume static values

# start cohort mid age
sc_age <- 20

# end cohort mid age
ec_age <- 24

# Specify cohort middle age
c_mid_age <- round(sum(sc_age, ec_age) / 2)

## Cohort specific age, data and sex settings
####Loops or functions will be better here to generate all cohorts data for baseline and intervention at the same time and save values. 
# In this case we are generating cohorts for females, mid_aged 27

lt_df_females_bl <- run_cohorts(in_idata = idata, in_sex = "females", in_mid_age = 27)


## Import *practice* scenario life table data
sc_data <- read.csv("data/sc_lf.csv", header = T, stringsAsFactors = F)

sub_idata <- filter(idata, age >= 22)

sub_idata[sub_idata$sex == "males" ,]$mx <- sc_data[sc_data$age <= 100 & sc_data$sex == "male",]$sc_mx

sub_idata[sub_idata$sex == "females" ,]$mx <- sc_data[sc_data$age <= 100 & sc_data$sex == "female",]$sc_mx

sub_idata[sub_idata$sex == "males" ,]$pyld_rate <- sc_data[sc_data$age <= 100 & sc_data$sex == "male",]$sc_wx

sub_idata[sub_idata$sex == "females" ,]$pyld_rate <- sc_data[sc_data$age <= 100 & sc_data$sex == "female",]$sc_wx



##Here we would need loops ove age and sex and storate results for baseline and scenario. 
## General life talble: practice scenario's cohort specific age, data and sex settings. 
## change function parameters to visualise other cohorts and sex. 

# In this case we are generating cohorts for females, mid_aged 27

lt_df_females_sc <- run_cohorts(in_idata = sub_idata, in_sex = "females", in_mid_age = 22)

##Disease life table: uses run_disease function, change function arguments to visualise other diseases

dlt_df_females_bl <- run_disease(in_idata = sub_idata, in_sex = "females", in_mid_age = 22, in_disease = "ihd")

##To prevent scientific notation in data frame
options(scipen=999)

dlt_df_males_bl <- run_disease(in_idata = sub_idata, in_sex = "males", in_mid_age = 22, in_disease = "ihd")

##To prevent scientific notation in data frame
options(scipen=999)

# # ####Generate RRs data frame
# # 
# # 
pif_bl <- run_pif(in_idata = idata , in_sex = "females", in_mid_age = 22, in_disease = "ihd")

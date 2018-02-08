# Load all functions
source("code/functions.R")
# Read data
idata <- read.csv("data/data.csv", stringsAsFactors = F)

# Make lower cases values for sex column
idata$sex <- tolower(idata$sex)

# Rename mortality_rate to mx
idata$mx <- idata$mortality_rate

# Remove mortality_rate column
idata$mortality_rate <- NULL

# Create new variable for 5_year population (age cohorts)
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

### General lifetable

# Create a new variable for the probability of dying between age now and now + 1
# Formula = IF(age<100,1-EXP(-mortality rate),1)

idata$qx <- ifelse(idata$age < 100, 1 - exp(-1 * idata$mx), 1)

# Assume start and end age specified by users
# For now we assume static values

# start cohort mid age
sc_age <- 20

# end cohort mid age
ec_age <- 24

# Specify cohort middle age
c_mid_age <- round(sum(sc_age, ec_age) / 2)

## Cohort specific age, data and sex settings
# In this case we are generating cohorts for females, mid_aged 27
lf_df_females <- run_cohorts(in_idata = idata, sex = "females", in_mid_age = 27)
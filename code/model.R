# Read data
idata <- read.csv("data/data.csv", stringsAsFactors = F)

# Make lower cases values for sex column
idata$sex <- tolower(idata$sex)

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

idata$lf_qx <- ifelse(idata$age < 100, 1 - exp(-1 * idata$mortality_rate), 1)


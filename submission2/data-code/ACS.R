if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidycensus, ggplot2, dplyr, lubridate, readr, readxl, scales, acs, tidyr)

install.packages("tidycensus")
library(tidycensus)
install.packages("acs")



## see list of variable names and tables
# Load all variable names from 2012 1-year ACS
vars_2012_1yr <- load_variables(year = 2012, dataset = "acs1", cache = TRUE)

# Filter to find B27010 table
b27010_vars <- vars_2012_1yr %>%
  filter(str_detect(name, "B27010"))

# View the result
View(b27010_vars)  # or head(b27010_vars)

# The variables of interest are:
b27010_vars <- c(
  all_18to34      = "B27010_018",
  employer_18to34 = "B27010_020",
  direct_18to34   = "B27010_021",
  medicare_18to34 = "B27010_022",
  medicaid_18to34 = "B27010_023",
  tricare_18to34  = "B27010_024",
  va_18to34       = "B27010_025",
  none_18to34     = "B27010_033",
  all_35to64      = "B27010_034",
  employer_35to64 = "B27010_036",
  direct_35to64   = "B27010_037",
  medicare_35to64 = "B27010_038",
  medicaid_35to64 = "B27010_039",
  tricare_35to64  = "B27010_040",
  va_35to64       = "B27010_041",
  none_35to64     = "B27010_050"
)

# Initialize empty list to store yearly data
insurance_list <- list()

# Loop through each year and pull data
for (t in 2012:2019) {
  message("Loading data for ", t)
  
  acs_data <- get_acs(
    geography = "state",
    variables = b27010_vars,
    year = t,
    survey = "acs1",
    output = "wide"
  )
  
  # Rename columns to your desired labels
  clean_data <- acs_data %>%
    transmute(
      State = NAME,
      year = t,
      all_18to34      = all_18to34E,
      employer_18to34 = employer_18to34E,
      direct_18to34   = direct_18to34E,
      medicare_18to34 = medicare_18to34E,
      medicaid_18to34 = medicaid_18to34E,
      tricare_18to34  = tricare_18to34E,
      va_18to34       = va_18to34E,
      none_18to34     = none_18to34E,
      all_35to64      = all_35to64E,
      employer_35to64 = employer_35to64E,
      direct_35to64   = direct_35to64E,
      medicare_35to64 = medicare_35to64E,
      medicaid_35to64 = medicaid_35to64E,
      tricare_35to64  = tricare_35to64E,
      va_35to64       = va_35to64E,
      none_35to64     = none_35to64E
    )
  
  insurance_list[[as.character(t)]] <- clean_data
}

# Combine all years
final.insurance <- bind_rows(insurance_list)

# Summarize the final variables like in your original code
final.insurance <- final.insurance %>%
  mutate(
    adult_pop    = all_18to34 + all_35to64,
    ins_employer = employer_18to34 + employer_35to64,
    ins_direct   = direct_18to34 + direct_35to64,
    ins_medicare = medicare_18to34 + medicare_35to64,
    ins_medicaid = medicaid_18to34 + medicaid_35to64,
    uninsured    = none_18to34 + none_35to64
  ) %>%
  select(State, year, adult_pop, ins_employer, ins_direct, 
         ins_medicare, ins_medicaid, uninsured)

# Save the output
write_tsv(final.insurance, "data/output/acs_insurance.txt")

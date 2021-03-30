'
This script reads and cleans the data from 
datasets. 
'


library(haven)
library(tidyverse)
library(data.table)
library(matrixStats)


# Expand memory for large datasets.
memory.size() ### Checking your memory size
memory.limit() ## Checking the set limit 
memory.limit(size=56000)


# Paths for datasets.
path_in_data <- "src/original_data/"

# Recommended to use CohortProfile as the starting point of any analysis.
df_cp <- read_dta(str_c(path_in_data, "SC1_CohortProfile_D_8-0-0.dta")) %>%
  # Drop variables which are the same for all participants or not of interest
  select(-c(cohort, tx80107, tx80533, tx80524, tx80529))

# Find out whether this is not rather categorical instead of count data!
# Find out and merge the daatsets with the relevant information.
# To check selective attrition would need all SES variables b/c those are the
# dimensions that matter.
# Check out Rauh/Renée how exactly their data look like and then also del bono, 
# Dohmen

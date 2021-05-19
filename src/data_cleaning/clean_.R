
library(haven)
library(tidyverse)
library(data.table)
#library(mclust)


# Paths for datasets.
path_in_data <- "src/original_data/"
path_out_data <- "bld/out/data/"


# Recommended to use CohortProfile as the starting point of any analysis.
df_cp <- read_dta(str_c(path_in_data, "SC1_CohortProfile_D_8-0-0.dta")) %>%
  setDT(., key = c("ID_t", "wave")) %>%
  # Drop variables which are the same for all participants or not of interest
  .[, c("cohort", "tx80107", "tx80533", "tx80524", "tx80529") := NULL]





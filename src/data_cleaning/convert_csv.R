'
This script converts the .dta to .csv files for faster readability.
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
path_out_data <- "src/original_data/data/"

# Get names of all files in directory.
all_files <- list.files(path_in_data) %>%
  str_subset(., "\\.(csv|dta)")

# If there are already .csv's, assume that they've been 
# converted already.
if (length(str_subset(all_files, ".csv")) != 0) {
  alrdy_conv <- str_split(
    str_subset(all_files, ".csv"), "\\.csv", simplify = TRUE
  )[, 1]
} else {
  alrdy_conv <- c()
}

# Get only names of datasets without file-specific ending.
all_files_wo_end <- all_files %>%
  str_split(., "\\.", simplify = TRUE) %>%
  .[, 1] 

# Convert all files except for the already converted ones.
to_convert <- setdiff(all_files_wo_end, alrdy_conv)

# Iterate over this list.
for (j in seq_along(to_convert)) {
  
  file_name_old <- str_c(to_convert[[j]], ".dta")
  file_name_new <- str_c(to_convert[[j]], ".csv")
  
  # Read .dta
  df_dta <- haven::read_dta(str_c(path_in_data, file_name_old, sep = "/"))
  # Write as .csv
  fwrite(df_dta, str_c(path_in_data, file_name_new, sep = "/"))
         
}


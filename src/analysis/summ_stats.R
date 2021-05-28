library(tidyverse)
library(data.table)
library(modelsummary)


# Paths for datasets.
path_in_data <- "src/original_data/"
path_out_data <- "bld/out/data/"
source("src/functions/functions.r")


df <- fread(str_c(path_out_data, "df_class_cs.csv")) %>%
  as_tibble() 


df_summ <- df %>%
  mutate(
    SES = ifelse(low_ses_books == 1, "Low", "High")
  ) %>%
  select(
    Female = fem_child,
    `Migration Background` = migback,
    # Family structure
    `Siblings (birth)` = siblings_at_birth,
    `Married` = married,
    # Parents
    `High School Degree` = both_p_abi,
    `Univ. Degree` = both_p_univ_deg,
    `Unemployed` = unemp,
    `Mother's age` = age_mom,
    `Father's age` = age_dad,
    `Net household income` = net_hh_inc,
    #`Parenting Style` = class_lab,
    SES
  )


pa <- data.frame("A. Child Characteristics", "", "", "", "", "", "", "")
#attr(pa, which = "position") <- 1
pb <- data.frame("B. Family Structure", "", "", "", "", "", "", "")
#attr(pb, which = "position") <- 4
pc <- data.frame("C. Household Characteristics", "", "", "", "", "", "", "")
#attr(pc, which = "position") <- 7

nrow_df <- rbindlist(list(pa,pb,pc))

datasummary_balance(~ SES,
                    data = df_summ, 
                    fmt = 3, dinm_statistic = "p.value",
                    #add_rows = rbindlist(pa, pb, pc)
                    )

#rownames(new_rows) <- 1

datasummary_balance(~ class_lab,
            data = df_summ, 
            fmt = 3, dinm = F)
            #dinm_statistic = "p.value")



datasummary_balance(
    ~ Treatment,
    data = df_balance,
    fmt = 3,
    dinm_statistic = "p.value",
    notes = "D_1 is a dummy equal to one if the survey year is one of 2011, 2012,
    and 2013. D_2 i the same but for 2014, 2015, and 2016.",
    output = str_c(path_out_analysis, "balancing_table_mz.tex")
  )

#re-classify ses
  
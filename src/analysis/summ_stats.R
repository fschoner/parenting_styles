library(tidyverse)
library(data.table)
library(modelsummary)


# Paths for datasets.
path_in_data <- "src/original_data/"
path_out_data <- "bld/out/data/"
path_out_analysis <- "bld/out/analysis/"

source("src/functions/functions.r")


df <- fread(str_c(path_out_data, "df_class_cs.csv")) %>%
  as_tibble() 


df_summ <- df %>%
  mutate(
    SES = ifelse(low_ses_books == 1, "Low", "High")
  ) %>%
  select(
    Female = fem_child,
    `Migration back.` = migback,
    # Family structure
    `Siblings` = siblings_at_birth,
    `Married` = married,
    # Parents
    `High School` = both_p_abi,
    #`Univ. Degree` = both_p_univ_deg,
    `Unemployed` = unemp,
    `Mother's age` = age_mom,
    #`Father's age` = age_dad,
    `Income` = net_hh_inc,
    #`Parenting Style` = class_lab,
    SES, class_lab,
    `Time investment*` = "Time investment",
    # Parenting Styles:
    #`Power. Enforce.` = power_enforce,
    #``
    # Interaction behaviors
    `Sensitivity*` = sens_n_stress_p_ib,
    `Intrusiveness*` = intrusiveness_p_ib,
    `Detachment*`  = detachment_p_ib,
    `Stimulation*` = stimulation_p_ib,
    `Pos. Regard*` = pos_regard_p_ib,
    `Neg. Regard*` = neg_regard_p_ib,
    `Emotionality*` = emotionality_p_ib
    # Summary Measure
    #`Interact. Qual. ` = qib_m
    
    # Child
    #`Pos. Mood (C)` = pos_mood_c_ib,
    #`Neg. Mood (C)` = neg_mood_c_ib,
    #`Activity (C)` = activity_lvl_c_ib,
    #`Attention (C)`= ns_sust_att_c_ib,
    #`Pos. Engagement (C)` = pos_engage_c_ib,
    
    # Skills
    #`SON-R` = can4_sc1,
    #`Vocabulary` = voc_sum,
    #`Del. Gratification` = dg_waiting_time
    
  )


datasummary_balance(~ SES,
                    data = df_summ, 
                    fmt = 3, dinm_statistic = "p.value",
                    #add_rows = pa,
                    align = c("l", rep("c", 7)),
                    #output = str_c(path_out_analysis, "summ_stats_ses.tex")
                    )


#datasummary_balance(~ class_lab,
#            data = df_summ, 
#            fmt = 3, dinm = F)
#            dinm_statistic = "p.value")


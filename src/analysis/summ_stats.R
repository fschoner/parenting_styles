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
    SES,
    `Time Invest.` = time_invest,
    # Parenting Styles: not include
    # Interaction behaviors
    `Sensitivity (P)` = sens_n_stress_p_ib,
    `Intrusiveness (P)` = intrusiveness_p_ib,
    `Detachment (P)` = detachment_p_ib,
    `Stimulation (P)` = stimulation_p_ib,
    `Pos. Regard (P)` = pos_regard_p_ib,
    `Neg. Regard (P)` = neg_regard_p_ib,
    `Emotionality (P)` = emotionality_p_ib,
    # Child
    `Pos. Mood (C)` = pos_mood_c_ib,
    `Neg. Mood (C)` = neg_mood_c_ib,
    `Activity (C)` = activity_lvl_c_ib,
    `Attention (C)`= ns_sust_att_c_ib,
    `Pos. Engagement (C)` = pos_engage_c_ib,
    # Summary Measure
    `Interact. Qual. (P)` = qib_m,
    
    
    # Skills
    `SON-R` = can4_sc1,
    `Vocabulary` = voc_sum,
    `Del. Gratification` = dg_waiting_time
    
  )


pa <- data.frame("A. Child Characteristics", "", "", "", "", "", "", "")
#attr(pa, which = "position") <- 1
pb <- data.frame("B. Family Structure", "", "", "", "", "", "", "")
#attr(pb, which = "position") <- 4
pc <- data.frame("C. Household Characteristics", "", "", "", "", "", "", "")
#attr(pc, which = "position") <- 7

#nrow_df <- rbindlist(list(pa,pb,pc))

datasummary_balance(~ SES,
                    data = df_summ, 
                    fmt = 3, dinm_statistic = "p.value",
                    #add_rows = rbindlist(pa, pb, pc),
                    output = str_c(path_out_analysis, "summ_stats_ses.tex")
                    )
datasummary_balance(~ SES,
                    data = df_summ, 
                    fmt = 3, dinm_statistic = "p.value")

#rownames(new_rows) <- 1

datasummary_balance(~ class_lab,
            data = df_summ, 
            fmt = 3, dinm = F)
            #dinm_statistic = "p.value")


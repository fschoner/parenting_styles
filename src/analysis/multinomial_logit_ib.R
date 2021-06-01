
library(tidyverse)
library(data.table)
library(viridis)
library(hrbrthemes)
library(ggpubr)
library(mlogit)
library(nnet)
library(haven)
library(sandwich)
library(modelsummary)
# Paths for datasets.
path_in_data <- "src/original_data/"
path_out_data <- "bld/out/data/"
path_out_analysis <- "bld/out/analysis/"

source("src/functions/functions.r")



df <- fread(str_c(path_out_data, "df_class_cs.csv")) %>%
  .[, AV := fifelse(class_lab == "AV", 1, 0)] %>%
  .[, AR := fifelse(class_lab == "AR", 1, 0)] %>%
  .[, SES := fifelse(low_ses_books == 1, 0, 1)] %>%
  as_tibble() %>%
  select(
    Female = fem_child,
    #`Migration background` = migback,
    # Family structure
    `Siblings` = siblings_at_birth,
    `Married` = married,
    `Migration background` = migback,
    # Parents
    `High school` = both_p_abi,
    #`Univ. Degree` = both_p_univ_deg,
    #`Unemployed` = unemp,
    `Mother's age` = age_mom,
    #`Father's age` = age_dad,
    `Income` = net_hh_inc,
    #`Parenting Style` = class_lab,
    `Time investment` = "Time investment",
    # Parenting Styles: not include
    # Interaction behaviors
    `Sensitivity` = sens_n_stress_p_ib,
    `Intrusiveness` = intrusiveness_p_ib,
    `Detachment` = detachment_p_ib,
    `Stimulation` = stimulation_p_ib,
    `Pos. Regard` = pos_regard_p_ib,
    `Neg. Regard` = neg_regard_p_ib,
    `Emotionality` = emotionality_p_ib,
    # Summary Measure
    #`Interaction qual.` = qib_m,
    class_lab, 
    `High-SES` = SES
  ) %>%
  drop_na()

df$class_lab_2 <- relevel(as.factor(df$class_lab), ref = "PE")

ib_p <- c(
  "Sensitivity", "Intrusiveness", "Detachment", "Stimulation",
  "`Pos. Regard`", "`Neg. Regard`", "Emotionality"
)

ses <- c(
  "`Mother's age`", "`High school`", "Income", "`High-SES`", "Siblings",
  "`Time investment`",
  "Married", "`Migration background`"
)
ses_wo_ti <- c(
  "`Mother's age`", "`High school`", "Income", "`High-SES`", "Siblings",
  #"`Time investment`",
  "Married", "`Migration background`"
)

fmla_ib <- as.formula(
  str_c("class_lab_2", str_c(ib_p, collapse = " + "), sep = " ~ ")
)
fmla_ib_ses <- as.formula(
  str_c("class_lab_2", str_c(c(ib_p, ses), collapse = " + "), sep = " ~ ")
)
fmla_ib_ses_zw <- as.formula(
  str_c("class_lab_2", str_c(c(ib_p, ses_wo_ti), collapse = " + "), sep = " ~ ")
)

#df[, by = "class", lapply(.SD, mean, na.rm= TRUE), .SDcols = patterns("p_ib$")]


#mod_zw <- multinom(fmla_ib_ses_zw, data = df)
#modelsummary(mod_zw, stars = T)
mdls <- list(
  "Interact. behav." = multinom(fmla_ib, data = df),
  "Interact. behav. + SES" = multinom(fmla_ib_ses_zw, data = df),
  "Interact. behav. + SES + TI" = multinom(fmla_ib_ses, data = df)
) 
#summary(mlogit_ib_ses)
#summary(mlogit)
#tidy(mlogit_ib)


gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "r.squared", "R<sup>2</sup>", 2
)
rows <- tribble(
  ~term, ~y.level, ~`Interact. behav.`, ~`Interact. behav. + SES`, ~`Interact. behav. + SES + TI`,
  "", "", "", "", "",
  "", "", "", "", "",
  "", "", "", "", "",
  "", "", "", "", "",
  "", "", "", "", "",
  "Further controls?", "", "No", "Yes", "Yes"
)
attr(rows, 'position') <- c(3, 6, 9, 12, 15, 18)
modelsummary(
  mdls, stars = TRUE,
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  coef_omit = "(Intercept)|Income|`Mother's age`|`High school`|Married|Pos. Regard|Neg. Regard|`Migration background`|`High-SES`|Stimulation",
  coef_rename = c(
    "`Pos. Regard`" = "Pos. Regard",
    "`Neg. Regard`" = "Neg. Regard",
    "`Time investment`" = "Time investment"
    ),
  #values_fn = list(summary(mlogit_ib), summary(mlogit_ib_ses)),
  group = term + y.level ~ model,
  gof_map = gm,
  align = c("l", rep("l", 4)),
  add_rows = rows,
  output = str_c(path_out_analysis, "logit.tex")
  )








broom::tidy(mlogit_ib, mlogit_ib_ses)
z <- summary(mlogit)$coefficients/summary(mlogit)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(mlogit))




# SES regs-
fmla_ses <- as.formula(
  str_c(
    "class_lab_2",
    str_c(c(
      "low_ses_books", "age_mom", "siblings_at_birth", "both_p_abi", "net_hh_inc",
      "married", "migback", "unemp", "fem_child", "net_hh_inc", "time_invest"
      ), collapse = " + "),
    sep = " ~ "
  )
)

mlogit_ses <- multinom(fmla_ses, data = df)
modelsummary(mlogit_ses, group = model ~ term, stars = TRUE)
cor(df$time_invest, df$siblings_at_birth, use = "complete.obs")
#summary(mlogit)

z <- summary(mlogit_ses)$coefficients/summary(mlogit_ses)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


#fmla HC
# DO OLS with two dummies, PE as reference category :-)
fmla_sr <- as.formula(
  str_c(
    "dg_waiting_time", str_c(c(
      "D_av", "D_ar", "age_mom", "siblings_at_birth", "both_p_abi",
      "fem_child", "net_hh_inc", "low_ses_books",
      "married", "migback", "unemp", "fem_child", "time_invest"
    ), collapse = " + "), sep = " ~ "
  )
)

fmla_sr_dummies <- as.formula(
  str_c(
    "dg_waiting_time", str_c(c(
      "D_av", "D_ar"
    ), collapse = " + "), sep = " ~ "
  )
)

res_cogn <- lm(fmla_sr, data = df)
summary(res_cogn)
vcov_cogn <- sqrt(diag(vcovHC(res_cogn, type = "HC0")))


res_cogn_d <- lm(fmla_sr_dummies, data = df)
summary(res_cogn_d)
vcov_cogn_d <- sqrt(diag(vcovHC(res_cogn_d, type = "HC0")))


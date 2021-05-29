
library(tidyverse)
library(data.table)
library(viridis)
library(hrbrthemes)
library(ggpubr)
library(mlogit)
library(nnet)
library(haven)
library(sandwich)
# Paths for datasets.
path_in_data <- "src/original_data/"
path_out_data <- "bld/out/data/"
source("src/functions/functions.r")



df <- fread(str_c(path_out_data, "df_class_cs.csv")) %>%
  .[, D_av := fifelse(class_lab == "AV", 1, 0)] %>%
  .[, D_ar := fifelse(class_lab == "AR", 1, 0)]
  
df$class_lab_2 <- relevel(as.factor(df$class_lab), ref = "PE")
fmla_p_ib <- as.formula(
  str_c(
    "class_lab_2",
    str_c(c(str_subset(names(df), "p_ib$"), "age_mom", "siblings_at_birth", "both_p_abi", "fem_child", "net_hh_inc", "low_ses_books"
            ), collapse = " + "),
    sep = " ~ "
  )
)
mlogit <- multinom(fmla_p_ib, data = df)
#summary(mlogit)

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
      "married", "migback", "unemp", "fem_child", "net_hh_inc"
      ), collapse = " + "),
    sep = " ~ "
  )
)

mlogit_ses <- multinom(fmla_ses, data = df)
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
      "married", "migback", "unemp", "fem_child"#, "time_invest"
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


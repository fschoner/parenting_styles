
library(haven)
library(tidyverse)
library(data.table)
library(viridis)
library(hrbrthemes)
library(mclust)


# Paths for datasets.
path_in_data <- "src/original_data/"
path_out_data <- "bld/out/data/"



df_p_ps <- fread(str_c(path_out_data, "df_parent_styles_cs.csv"))
df_p_ca <- fread(str_c(path_out_data, "df_collective_act_cs.csv"))
df_ib <- fread(str_c(path_out_data, "df_ib_cs.csv"))
df_ses <- fread(str_c(path_out_data, "df_ses_nc.csv"))
plot(density(df_p_ps$emot_warmth))

# Check few basic correlations
df_ib_ca <- merge.data.table(
  df_p_ca, df_ib, by = "ID_t", all.y = TRUE
) %>%
  merge.data.table(
    ., df_p_ps, by = "ID_t", all.y = TRUE
  ) %>%
  merge.data.table(., df_ses, all.x = TRUE) %>%
  na.omit()
cor(df_ib_ca$time_invest, df_ib_ca$PC1)
cor(df_ib_ca$time_invest, df_ib_ca$monitoring)
cor(df_ib_ca$time_invest, df_ib_ca$low_ses_books)
cor(df_ib_ca$time_invest, df_ib_ca$nc_patience)
cor(df_ib_ca$time_invest, df_ib_ca$married)
cor(df_ib_ca$time_invest, df_ib_ca$migback)
cor(df_ib_ca$time_invest, df_ib_ca$stimulation_p)


# Plot pairwise scatterplots
pairs(df_p_ps[, -1], lower.panel = NULL)

# # PCA
# ps_pca <- prcomp(df_p_ps_2[, -1], scale = TRUE)
# ps_pca
# 
# ps_var <- ps_pca$sdev^2
# # proportion of variance explained
# ps_var/sum(ps_var)
# # cumulative
# cumsum(ps_var)/sum(ps_var)
# 
# # Plot, looks interesting
# biplot(ps_pca)
# 
# # predictions
# ps_pred <- predict(ps_pca)

# Estimate Gaussian Mixture Model
gmm <- Mclust(df_p_ps[, - c(1)])
df_p_ps %>%
  .[, class := gmm$classification]
summary(gmm)

# class probabilities
round(gmm$z, 4)
# uncertainties, how are they computed?
summary(round(gmm$uncertainty,4))
summary(gmm)
plot(gmm)
round(gmm$parameters$mean, 3)

round(colMeans(df_p_ps), 4)
# ONLY LEARN ON ITEMS YOU CAN UNDOUBTEDLY CLASSIFY AS AR AV PE,
# OR CLASSIFY based on single items st. don't need to average


df_p_test <- merge.data.table(
  df_p_ca[, c("ID_t", "time_invest")],
  df_p_ps[, c("ID_t", "class")],
  by = "ID_t",
  all.x = TRUE
) %>%
  na.omit() %>%
  .[, class := as.factor(class)] %>%
  .[, class2 := as.factor(sample(c(1:3), size = 1504, replace = TRUE))]

t.test(
  df_p_test[class == 2, "time_invest"],
  df_p_test[class == 3, "time_invest"],
  alternative = "t"
  )

plot <- ggplot(df_p_test, aes(x = time_invest, group = class, fill = class)) +
  geom_density(adjust=1.5, alpha=.4) + 
  theme_ipsum()
plot




# Try GMM on ib's
df_ib_p <- df_ib %>%
  .[, .SD, .SDcols = patterns("_p$")]
gmm_ib <- Mclust(df_ib_p, warn = T)
summary(gmm_ib)

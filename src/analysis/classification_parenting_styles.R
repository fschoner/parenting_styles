
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
gmm <- Mclust(df_p_ps[, -1])
df_p_ps %>%
  .[, class := gmm$classification]
summary(gmm)
#plot(gmm)





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

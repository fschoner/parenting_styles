# Packages
library(haven)
library(tidyverse)
library(data.table)
library(viridis)
library(hrbrthemes)
library(mclust)
library(factoextra)
library(ggpubr)
library(xtable)
library(scales)
library(Cairo)
library(extrafont)

# Paths for datasets.
path_in_data <- "src/original_data/"
path_out_data <- "bld/out/data/"
path_out_analysis <- "bld/out/analysis/"
source("src/functions/functions.r")


# Read all datasets.
df_p_ps <- fread(str_c(path_out_data, "df_parent_styles_cs.csv"))
df_p_ca <- fread(str_c(path_out_data, "df_collective_act_cs.csv"))
df_ib <- fread(str_c(path_out_data, "df_ib_cs.csv"))
df_ses <- fread(str_c(path_out_data, "df_ses_nc.csv"))
df_cogn <- fread(str_c(path_out_data, "df_target_competencies_cs.csv"))
df_pg <- fread(str_c(path_out_data, "df_pg.csv"))
#plot(density(df_p_ps$emot_warmth))

# Merge datasets, core is df_p_ps since they are used to derive the classifica-
# tion.
df_ib_ca <- merge.data.table(df_p_ca, df_ib, by = "ID_t", all = TRUE) %>%
  merge.data.table(., df_p_ps, by = "ID_t", all.y = TRUE) %>%
  merge.data.table(., df_cogn, by = "ID_t", all.x = TRUE) %>%
  merge.data.table(., df_ses, all.x = TRUE) %>%
  merge.data.table(., df_pg, all.x = TRUE)


# Check a few basic correlations.
cor(df_ib_ca$time_invest, df_ib_ca$siblings_at_birth, use = "complete.obs")
cor.test(df_ib_ca$time_invest, df_ib_ca$siblings_at_birth, use = "complete.obs")


# Estimate Gaussian Mixture Model
gmm <- Mclust(df_p_ps[, - c(1)])
summary(gmm)
# Add classification, uncertainty and class probabilities to dataset.
df_ib_ca %>%
  .[,  `:=` (
    class = gmm$classification, uncert = gmm$uncertainty, pr_1 = gmm$z[, 1],
    pr_2 = gmm$z[, 2], pr_3 = gmm$z[, 3]
    )] 

# Proportions of classes.
tabulate(gmm$classification) / nrow(df_p_ps)
# class probabilities


# Drop high-uncertainty observations.
#df_ib_ca <- df_ib_ca %>%
# .[uncert < quantile(uncert)[4], ]

# Means of the classes
cols <- str_c("mean_cl_", 1:3)
mean_class_df <- data.table(
  item = rownames(gmm$parameters$mean),
  mean_cl_1 = gmm$parameters$mean[, 1],
  mean_cl_2 = gmm$parameters$mean[, 2],
  mean_cl_3 = gmm$parameters$mean[, 3],
  item_mean = colMeans(df_p_ps[, -1])
) %>%
  # Subtract raw item means to facilitate above/below-mean interpretations.
  #.[, (cols) := lapply(.SD, "-", item_mean), .SDcols = patterns("^mean_cl")] %>%
  .[, (cols) := lapply(.SD, round, 3), .SDcols = patterns("^mean_cl")] %>%
  .[, !c("item_mean")] %>%
  setnames(
    .,
    old = c("item", "mean_cl_1", "mean_cl_2", "mean_cl_3"),
    new = c("variable", "AV", "AR", "PE")
  )
# Should be able to export this using xtable.
cols_ps <- names(df_p_ps)[-1]
df_ib_ca %>%
  .[, by = "class", lapply(.SD, mean, na.rm = T), .SDcols = cols_ps]


# Disagreement: patience.
#df_ib_ca %>%
#  .[, dis_pat := abs(nc_patience - dg_waiting_time)] 

# Give labels to classes
df_ib_ca[, class_lab := fcase(
  class == 1, "AV",
  class == 2, "AR",
  class == 3, "PE"
)]



# Parenting goals.
#df_ib_ca[, by = "class", lapply(.SD, mean, na.rm = T), .SDcols = c("obedient", "independent", "diligent")]



# Compute p-values for mean differences in IB.
ib_av_ar <- df_ib_ca %>%
  .[class %in% c(1, 2),] 

ib_av_pe <- df_ib_ca %>%
  .[class %in% c(1, 3),]

ib_ar_pe <- df_ib_ca %>%
  .[class %in% c(2, 3),]

p_ib_av_ar <- ib_av_ar %>%
  .[, lapply(.SD, function(x) t.test(x ~ class_lab)$p.value), .SDcols = patterns("(p|c)_ib$")] %>%
  melt(.) %>%
  setnames(., old = c("value"), new = c("1/2"))

p_ib_av_pe <- ib_av_pe %>%
  .[, lapply(.SD, function(x) t.test(x ~ class_lab)$p.value), .SDcols = patterns("(p|c)_ib$")] %>%
  melt(.) %>%
  setnames(., old = c("value"), new = c("1/3"))

p_ib_ar_pe <- ib_ar_pe %>%
  .[, lapply(.SD, function(x) t.test(x ~ class_lab)$p.value), .SDcols = patterns("(p|c)_ib$")] %>%
  melt(.) %>%
  setnames(., old = c("value"), new = c("2/3"))


p_ib <- merge.data.table(
  x = p_ib_av_ar, y = p_ib_av_pe, by = "variable"
) %>%
  merge.data.table(x = ., y = p_ib_ar_pe, by = "variable")

cols_p_vals <- str_subset(names(p_ib), "^p")

p_ib[, (cols_p_vals) := lapply(.SD, round, 3), .SDcols = cols_p_vals]

cols_ib <- str_subset(names(df_ib_ca), "(p|c)_ib$")
means_ib <- df_ib_ca %>%
  .[, by = "class_lab", lapply(.SD, mean, na.rm = T), .SDcols = patterns("(p|c)_ib$")] 

means_ib_f <- data.table(
  variable = colnames(means_ib)[-1],
  Mean_AV = as.double(means_ib[class_lab == "AV", -1]),
  Mean_AR = as.double(means_ib[class_lab == "AR", -1]),
  Mean_PE = as.double(means_ib[class_lab == "PE", -1])
) 

cols_ib_f <- str_subset(names(means_ib_f), "^Mean")
means_ib_f[, (cols_ib_f) := lapply(.SD, round, 3), .SDcols = cols_ib_f]

merge_ib <- merge.data.table(
  x = means_ib_f, y = p_ib, by = "variable"
)

#2)
# NOw the same classes means of parenting styles.
cols_ps <- names(df_p_ps)[-1]

p_ps_av_ar <- ib_av_ar %>%
  .[, lapply(.SD, function(x) t.test(x ~ class_lab)$p.value), .SDcols = cols_ps] %>%
  melt(.) %>%
  setnames(., old = c("value"), new = c("1/2"))

p_ps_av_pe <- ib_av_pe %>%
  .[, lapply(.SD, function(x) t.test(x ~ class_lab)$p.value), .SDcols = cols_ps] %>%
  melt(.) %>%
  setnames(., old = c("value"), new = c("1/3"))

p_ps_ar_pe <- ib_ar_pe %>%
  .[, lapply(.SD, function(x) t.test(x ~ class_lab)$p.value), .SDcols = cols_ps] %>%
  melt(.) %>%
  setnames(., old = c("value"), new = c("2/3"))

p_ps <- merge.data.table(
  x = p_ps_av_ar, y = p_ps_av_pe, by = "variable"
) %>%
  merge.data.table(x = ., y = p_ps_ar_pe, by = "variable")

cols <- str_subset(names(p_ps), "^p")
ps_mean_pval <- p_ps %>%
  .[, (cols) := lapply(.SD, round, 3), .SDcols = cols] %>%
  merge.data.table(
    x = mean_class_df, y = ., by = "variable"
  ) %>%
  setnames(., old = c("variable", "AV", "AR", "PE"), new = c("Dimension", "1", "2", "3"))

ps_mean_pval$Dimension <- c(
  "Powerful enforcement", "Emotional warmth", "Inconsistent parent.",
  "Neg. communication", "Monitoring", "Autonomy", "Pos. parent. behavior",
  "Psychological control"
)

# Write this to disk using xtab.
xtab <- xtable(
  ps_mean_pval,
  type = "latex", digits = c(3), align = c(rep("l", 2), rep("c", 6))
)
addtorow <- list()
addtorow$pos <- list(-1, 8)
addtorow$command <- c(
  "\\hline \\hline\\\\[-1.8ex] \n &    &    &    &\\multicolumn{3}{c}{p-values} \\\\ \n \\cline{5-7} \\\\[-1.8ex]",
    "\\hline")
print(
  xtab,
      file = str_c(path_out_analysis, "mean_class_dim.tex", sep = "/"),
      sanitize.text.function = function(x) {
        x
      }, include.rownames = FALSE, add.to.row = addtorow,
      hline.after = c(0, nrow(xtab)), booktabs = TRUE
)



# Preparation for plot
cols_std <- c("time_invest", str_subset(names(df_ib_ca), "(p|c)_ib$"))
df_ib_ca %>%
  .[, Class := factor(class_lab, levels = c("AR", "AV", "PE"))] %>%
  #.[, class2_plot := as.factor(sample(c(1:3), size = 1504, replace = TRUE))] %>%
  .[, (cols_std) := lapply(.SD,  function(x) as.vector(scale(x))), .SDcols = cols_std] %>%
  setnames(., old = c("time_invest"), new = "Time investment")

fwrite(df_ib_ca, str_c(path_out_data, "df_class_cs.csv"))


density_cols <- c("Time investment")
#plot_list <- unlist(purrr::map(density_cols, density_plot, df = df_ib_ca))

#plot <- ggarrange(plotlist = plot_list, nrow = 1, ncol = 1,
#          common.legend = TRUE, legend = "bottom")
plot <- density_plot(density_cols, df = df_ib_ca)
ggsave(plot, width = 8, height = 6
       filename = str_c(path_out_analysis, "time_invest_density.png"))

# 
# # Try GMM on ib's
# df_ib_p <- df_ib %>%
#   .[, .SD, .SDcols = patterns("_p$")]
# gmm_ib <- Mclust(df_ib_p, warn = T)
# summary(gmm_ib)
# 
# 
# 
# 
# 
# 
# fit <- princomp(df_p_ps[, - c(1)], cor=TRUE)
# plot(fit, type = "lines")
# summary(fit)
# biplot(fit) 
# fit$scores
# 
# 
# pca_res <- prcomp(df_p_ps[, - c(1)], scale = TRUE)
# pc_fac <- factor(df_p_ps[, 1])
# fviz_pca_var(pca_res,
#              col.var = "contrib", # Color by contributions to the PC
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
# # Conclusion of all this: PCA finds 3 components according to eigenvalue 
# # criterion. Ofc, we should investigate the robustness more thoroughly, 
# # but in the interest of time, I'll work with the classification provided by 
# # the GMM
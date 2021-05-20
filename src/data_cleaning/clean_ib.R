library(haven)
library(tidyverse)
library(data.table)
#library(mclust)


# Paths for datasets.
path_in_data <- "src/original_data/"
path_out_data <- "bld/out/data/"


keep_dm <- c(
  "ID_t", str_c("wave_w", 1:3),
  # Data vailable W1, W2, W3
  "ihn1m001_c", "ihn1m001_sc1n2_c", "ihn1m001_sc1n3_c",
  # Parent-child interaction: parent W1, W2, W3
  # In W3 they have 3 stimulation items (numeracy, language, and global), of
  # which I only take the latter.
  str_c("ihn1p00", 1:8, "_c"), str_c("ihn1p00", 1:8, "_sc1n2_c"),
  str_c("ihn1p00", c(1:4, 6:8), "_sc1n3_c"), "ihn1p015_sc1n3_c",
  # Parent-child interaction: child W1, W2, W3
  str_c("ihn1c00", 1:5, "_c"), str_c("ihn1c00", 1:5, "_sc1n2_c"),
  str_c("ihn1c00", 1:5, "_sc1n3_c"),
  # Deviation from standard, Parent does not speak W1, W2, W3
  str_c("ihn1m00", 4:5, "_c"), str_c("ihn1m00", 4:5, "_sc1n2_c"),
  str_c("ihn1m00", 4:5, "_sc1n3_c")
)


# Trouble is that there are super few obs for whom we have sens_stress_p for all
# 3 waves (7). Even for waves 1 and 3 there are only 29. I should skip these!
df_dm <- read_dta(str_c(path_in_data, "SC1_xDirectMeasures_D_8-0-0.dta")) %>%
  # Convert to data.table
  setDT(., key = c("ID_t")) %>%
  # Keep only columns from above
  .[, .SD, .SDcols = keep_dm] %>%
  # Convert to panel format
  melt(
    .,
    id.vars = c("ID_t"), 
    measure.vars = list(
      wav_avail = str_c("wave_w", 1:3),
      dat_avail = c("ihn1m001_c", "ihn1m001_sc1n2_c", "ihn1m001_sc1n3_c"), 
      # Parent
      #sens_stress_p = c("ihn1p001_c", "ihn1p001_sc1n2_c", "ihn1p001_sc1n3_c"),
      sens_n_stress_p = c("ihn1p002_c", "ihn1p002_sc1n2_c", "ihn1p002_sc1n3_c"), 
      intrusiveness_p = c("ihn1p003_c", "ihn1p003_sc1n2_c", "ihn1p003_sc1n3_c"),
      detachment_p = c("ihn1p004_c", "ihn1p004_sc1n2_c", "ihn1p004_sc1n3_c"),
      stimulation_p = c("ihn1p005_c", "ihn1p005_sc1n2_c", "ihn1p015_sc1n3_c"),
      pos_regard_p = c("ihn1p006_c", "ihn1p006_sc1n2_c", "ihn1p006_sc1n3_c"),
      neg_regard_p = c("ihn1p007_c", "ihn1p007_sc1n2_c", "ihn1p007_sc1n3_c"),
      emotionality_p = c("ihn1p008_c", "ihn1p008_sc1n2_c", "ihn1p008_sc1n3_c"),
      # Child
      pos_mood_c = c("ihn1c001_c", "ihn1c001_sc1n2_c", "ihn1c001_sc1n3_c"),
      neg_mood_c = c("ihn1c002_c", "ihn1c002_sc1n2_c", "ihn1c002_sc1n3_c"),
      activity_lvl_c = c("ihn1c003_c", "ihn1c003_sc1n2_c", "ihn1c003_sc1n3_c"),
      ns_sust_att_c = c("ihn1c004_c", "ihn1c004_sc1n2_c", "ihn1c004_sc1n3_c"),
      pos_engage_c = c("ihn1c005_c", "ihn1c005_sc1n2_c", "ihn1c005_sc1n3_c"),
      # Deviation from standard, parent does not speak
      deviations = c("ihn1m004_c", "ihn1m004_sc1n2_c", "ihn1m004_sc1n3_c"),
      not_speak = c("ihn1m005_c", "ihn1m005_sc1n2_c", "ihn1m005_sc1n3_c")
    ),
    variable.name = "wave"
  ) %>%
  # Sort by ID
  setorderv(., "ID_t") 

# Note that instead of measure.vars = list(data_avail = ...) I coul've used
# measure.vars = patterns(data_avail = "^ihn1p001") but like this it is more
# transparent such that I can look up which variables exactly are in.

# Replace negative values by missings.
for (col in names(df_dm)) {
  set(
    df_dm,
    i = which(df_dm[[col]] %in% c(-21, -55, -56)),
    j = col,
    value = NA
  )
}

# Actually NEPS has sth on the interaction behavior OF THE CHILD!!!
# Let's see whether I can think of a way how parents adapt.


# Build a cross section 
cols_dm <- c(names(df_dm)[names(df_dm) %like% "_(p|c)$"], "not_speak")
df_dm_cs <- df_dm %>%
  #.[!deviations == 1, ] %>%
  .[, by = "ID_t", lapply(.SD, mean, na.rm = TRUE), .SDcols = cols_dm] %>%
  #.[, (cols_dm) := lapply(.SD, function(x) as.vector(scale(x))), .SDcols = cols_dm] %>%
  .[,
    qib_m := rowMeans(.SD),
    .SDcols = c("sens_n_stress_p", "pos_regard_p", "emotionality_p", "stimulation_p")
    ] %>%
  na.omit()
# PCA
ps_pca <- prcomp(
  df_dm_cs %>%
    .[, c("sens_n_stress_p", "pos_regard_p", "emotionality_p", "stimulation_p")]
)
# Predictions
ps_pred <- predict(ps_pca)
# Add principal components to dataframe
df_dm_cs[, c("PC1", "PC2") := list(ps_pred[, 1], ps_pred[, 2])] %>%
  # Write to disc.
  fwrite(., str_c(path_out_data, "df_ib_cs.csv"))

cor(df_dm_cs$PC1, df_dm_cs$qib_m)

# ps_var <- ps_pca$sdev^2
# # cumulative
# cumsum(ps_var)/sum(ps_var)
# # Plot, looks interesting
# biplot(ps_pca)
# # predictions
# ps_pred <- predict(ps_pca)
#plot(ps_pred[, 1:2], xlab = "PC1", ylab = "PC2")
# perfectly negatively correlated

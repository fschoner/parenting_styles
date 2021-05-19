
library(haven)
library(tidyverse)
library(data.table)
#library(mclust)


# Paths for datasets.
path_in_data <- "src/original_data/"
path_out_data <- "bld/out/data/"


# Direct measures: SON-R Test and delayed gratification
keep_tc <- c(
  "ID_t", str_c("wave_w", c(4, 6)),
  # Delayed Gratification: waiting time, has waited (W4 and W6)
  "den40002", "den40001_c", "den60002", "den60001_c",
  # SON-R (W4): items, WLE, SE
  str_c("can4000", 1:9), str_c("can400", 10:15), str_c("can4_sc", 1:2),
  # Vocabulary: sum of correct items, ceiling set (W4 and W6)
  "von4_sc3", "von4cs_sc8", "von6_sc3", "von6cs_sc9"
)

# Read data-
df_tc <- read_dta(str_c(path_in_data, "SC1_xTargetCompetencies_D_8-0-0.dta")) %>%
  setDT(., key = "ID_t") %>%
  .[, .SD, .SDcols = keep_tc]

# Missings.
for (col in names(df_tc)) {
  set(
    df_tc,
    i = which(df_tc[[col]] %in% c(-21:-25, -54:-56, -94, -97)),
    j = col,
    value = NA
  )
}

# SON-R: Create sum of correct items, scale it, remove initial items.
sr_items <- names(df_tc)[names(df_tc) %like% "^can4"] 
df_sr <- df_tc %>%
  .[, .SD, .SDcols = c("ID_t", "wave_w4", sr_items)] %>%
  # Create sum of correct items
  .[
    wave_w4 == 1,
    sr_sum := as.vector(scale(rowSums(.SD, na.rm = TRUE))),
    .SDcols = patterns("can\\d{5}")
  ] %>%
  # Remove initial items
  .[, names(df_tc)[which(grepl("can\\d{5}", names(df_tc)))] := NULL] %>%
  .[, "wave_w4" := NULL] %>%
  na.omit()
# Does their SON-R overall measure correlate with the sum? YES
#cor(df_tc_sr$sr_sum, df_tc_sr$can4_sc1, use = "complete.obs")

# Delayed gratification: Build panel dataset, scale variables, average to get a
# a cross section
df_tc <- df_tc %>%
  # Melt to long format
  melt(
    .,
    id.vars = "ID_t",
    measure.vars = patterns(
      dg_waiting_time = "den(4|6)0002",
      dg_waited = "den(4|6)0001_c",
      voc_sum = "von(4|6)_sc3",
      voc_ceil = "von(4|6)cs_sc(8|9)"
    ),
    variable.name = "wave"
  ) %>%
  na.omit() 


cols_tc <- c("dg_waited", "dg_waiting_time", "voc_sum", "voc_ceil")

# Scale
df_tc <- df_tc %>%
  .[, 
    (cols_tc) := lapply(.SD, function(x) as.vector(scale(x))),
    .SDcols = cols_tc,
    by = "wave"
  ] %>%
  .[, lapply(.SD, mean, na.rm = TRUE), .SDcols = cols_tc, by = "ID_t"] 



df_tc <- merge.data.table(x = df_tc, y = df_sr, by = c("ID_t"), all = TRUE) %>%
  fwrite(., str_c(path_out_data, "df_target_competencies_cs.csv"))
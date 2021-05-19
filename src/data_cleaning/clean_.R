
library(haven)
library(tidyverse)
library(data.table)
#library(mclust)


# Paths for datasets.
path_in_data <- "src/original_data/"
path_out_data <- "bld/out/data/"


# Recommended to use CohortProfile as the starting point of any analysis.
df_cp <- read_dta(str_c(path_in_data, "SC1_CohortProfile_D_8-0-0.dta")) %>%
  setDT(., key = c("ID_t", "wave")) %>%
  # Drop variables which are the same for all participants or not of interest
  .[, c("cohort", "tx80107", "tx80533", "tx80524", "tx80529") := NULL]








# FOr SON-R, create own outcome (sum of solved items = 1), and compare it to WLE
# -> are they correlated?
keep_tc <- c(
  "ID_t", str_c("wave_w", c(4, 6)),
  # Delayed Gratification: waiting time, has waited (W4 and W6)
  "den40002", "den40001_c", "den60002", "den60001_c",
  # SON-R (W4): items, WLE, SE
  str_c("can4000", 1:9), str_c("can400", 10:15), str_c("can4_sc", 1:2),
  # Vocabulary: sum of correct items, ceiling set (W4 and W6)
  "von4_sc3", "von4cs_sc8", "von6_sc3", "von6cs_sc9"
)

df_tc <- read_dta(str_c(path_in_data, "SC1_xTargetCompetencies_D_8-0-0.dta")) %>%
  setDT(., key = "ID_t") %>%
  .[, .SD, .SDcols = keep_tc]

for (col in names(df_tc)) {
  set(
    df_tc,
    i = which(df_tc[[col]] %in% c(-21:-25, -55, -56, -94)),
    j = col,
    value = NA
  )
}

sr_items <- names(df_tc)[names(df_tc) %like% "^can4"] 
df_tc_sr <- df_tc %>%
  .[, .SD, .SDcols = c("ID_t", "wave_w4", sr_items)] %>%
  .[
    wave_w4 == 1,
    sr_sum := rowSums(.SD, na.rm = TRUE), .SDcols = patterns("can\\d{5}")
    ] %>%
  .[, names(df_tc)[which(grepl("can\\d{5}", names(df_tc)))] := NULL] %>%
  .[, wave := 4] %>%
  .[, "wave_w4" := NULL]
# do they correlate?
cor(df_tc_sr$sr_sum, df_tc_sr$can4_sc1, use = "complete.obs")

df_tc <- df_tc %>%
  .[, (sr_items) := NULL] %>%
  melt(
    .,
    id.vars = "ID_t",
    measure.vars = patterns(
      wav_avail = "^wave_w",
      dg_waiting_time = "den(4|6)0002",
      dg_waited = "den(4|6)0001_c",
      voc_sum = "von(4|6)_sc3",
      voc_ceil = "von(4|6)cs_sc(8|9)"
    ),
    variable.name = "wave"
  ) %>%
  # Code wave correctly
  .[, wave := fcase(
    wave == 1, 4,
    wave == 2, 6
  )] %>%
  # Sort by ID
  setorderv(., "ID_t") 

df_tc <- merge.data.table(
  x = df_tc, y = df_tc_sr, by = c("ID_t", "wave"),
  all.x = TRUE
  )

cols_tc <- c(names(df_tc)[names(df_tc) %like% "^(dg|voc|sr)"], "can4_sc1") 
df_tc %>%
  .[, 
    (cols_tc) := lapply(.SD, function(x) as.vector(scale(x))),
    .SDcols = cols_tc,
    by = "wave"
    ]
# Check distributions of all of these things


















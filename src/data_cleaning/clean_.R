
library(haven)
library(tidyverse)
library(data.table)
library(mclust)


# Paths for datasets.
path_in_data <- "src/original_data/"


# Recommended to use CohortProfile as the starting point of any analysis.
df_cp <- read_dta(str_c(path_in_data, "SC1_CohortProfile_D_8-0-0.dta")) %>%
  setDT(., key = c("ID_t", "wave")) %>%
  # Drop variables which are the same for all participants or not of interest
  .[, c("cohort", "tx80107", "tx80533", "tx80524", "tx80529") := NULL]

##############################################################################
######################################## PARENT################################
##############################################################################

# The first set of items just goes through the codebook on pParent from the 
# start and contains the things I deem worth of keeping.
# The second set focuses more specifically on the main categories of items I 
# want to employ for my analyses.

keep_p <- c(
  "ID_t", "wave",
  # Week of pregnancy at birth
  #"p529100",
  # Weight at birth
  "p529000_R",
  # Temperament
  #str_c("p66804", c("c", "e", "f", "k", "n", "o")),
  # Coparenting
  #str_c("pa030", c(1:5), 0),
  # Breastfeeding duration
  "p526200",
  # Communicative gestures,
  #str_c("p1010", 1:2, 0),
  # Receptive language
  #"p102040", "p102010",
  #Productive language
  #str_c("p1020", 2:3, 0),
  #Cognition: means‐end task
  #"p103040",
  #(Fine) Motor skills
  #str_c("p1040", c(1, 2), 0), "p104040_v1",
  # Sibling year of birth (1-3)
  str_c("p73222y_w", 1:3),
  # Cognition
  #"p103060", "p101050",
  # Productive language
  #str_c("p1020", 5:6, 0), str_c("p10207", 1:3),
  # Cognition
  #str_c("p1030", c(1, 5), 0),
  # Fine) Motor skills: 
  #str_c("p1040", 4:7, 0),
  # Books at home, HOMEPOS
  "p34005a",# str_c("p34006", c("d", "h", "e", "j", "f")),
  # Employment 12 month prior to birth, partner 
  "p731501", "p731601",
  # parental leave respondent, partner
  #str_c("pa0", 4:5, "020"),
  # Proportion of friends w/ higher education
  #"p321104",
  # Temperament questions might be sth "what the child does"
  # Television consumption
  #str_c("p20145", 0:2),
  # HH HNE everyday integration
  #str_c("p28243", 1:6),
  # HH HNE orientation
  #str_c("p28244", 1:6),
  # Household activities of the child
  #str_c("p28181", 0:9),
  # number of siblings
  "pb10000",
  # rules for use of media
  #str_c("p20241", 0:2), "p208450",
  # religiousness
  #"p435000",
  # domestic learning environment
  #str_c("p28136", 2:7),
  # Gender child
  "p700010",
  # Gender respondent
  "p731702",
  # (foster) mom/dad
  "p731701", #"p731117", 
  # Marital status
  "p731110",
  # Living together?, permanent partner
  #"p731111", "p731112",
  # nof siblings, (in the household)
  "p732103", #"p732104",
  # year of birth respondent, partner
  "p73170y", "p73175y",
  # germborn respondent., partner
  "p400000", "p403000",
  # GER citizenship, partner
  #"p401100", "p404000",
  # highest educational qualification respondent, partner
  "p731802", "p731852",
  #(Highest) professional qualification Respondent, partner
  #"p731813", "p731863",
  # Employment respondent, partner
  #"p731901", "p731951",
  # Mother tongue respondent, child
  #"p413000_g1D", "p410000_g1D",
  # MIgration background
  "p400500_g1",
  # special needs child
  #"p190201",
  # nursery or day care
  #"pa0100a",
  #net household income
  "p510005_g1",
  # Social trust (W7), Gen. willing. to take risk (W7), patience (W7)
  "p517100", "p515051", "p515100",
  
  # Collective activities (W1)
  str_c("p28130", 1:9),
  # Collective activities, pretending (W2)
  str_c("p28131", 1:6), "p281326",
  # Collective activities (W3, 4, 5)
  str_c("p28132", 1:7),
  
  # Parenting styles (powerful enforcement, W6)
  str_c("p66813", letters[1:4]),
  #Parenting styles (emotional warmth, W7)
  str_c("p66810", letters[1:3]), 
  #Parenting styles (inconsistent parenting, W5, 7; see age gradient!)
  str_c("p66812", letters[1:4]), 
  #Parenting styles (negative communication, W5, 7)
  str_c("p66811", letters[1:3]),
  #Parenting styles (monitoring, W7)
  str_c("p66814", letters[1:4]),
  # Parenting styles (autonomy, W6, 8)
  str_c("p66816", letters[1:5]),
  # Parenting styles (pos. parent. behav., W8)
  str_c("p66818", letters[1:3]),
  # Parenting styles (psych. control, W8; could be used for disagreement?)
  str_c("p66817", letters[1:3]),
  
  # Parenting goals (first 3 status, 2 autonomy, 2 competencies)
  str_c("p67800", letters[1:7])

)

# nof siblings + sibling's year of birth can be used for birth order!


df_parent <- read_dta(str_c(path_in_data, "SC1_pParent_D_8-0-0.dta")) %>%
  setDT(., key = c("ID_t", "wave")) %>%
  .[, .SD, .SDcols = keep_p] %>%
  setnames(
    .,
    old = c(
      "p529000_R", "p526200", "p34005a", "p731501", "p731601",
      "pb10000", "p700010", "p731701", "p731702", "p731110",
      "p400000", "p403000", "p400500_g1",
      "p510005_g1", "p517100", "p515051", "p515100"
      ),
    new = c(
      "birthweight", "dur_bf_month", "nof_books", "unemp", "unemp_p",
      "nof_siblings", "fem_child", "fem_parent", "mom_responds", "married",
      "germborn", "germborn_p", "migback",
      "net_hh_inc", "trust", "risk", "patience"
      )
  )

# Recode variables that give meaning to otherwise missing coding.
df_parent %>%
  .[dur_bf_month %in% c(-93, 0), dur_bf_month := 0] %>%
  .[wave %in% c(4, 8), nof_siblings := p732103] %>%
  .[, fh_abi := fcase(
      p731802 %in% c(4, 5), 1,
      p731802 %in% c(-20, 1:3, 6), 0
    )] %>%
  .[, fh_abi_p := fcase(
      p731852 %in% c(4, 5), 1,
      p731852 %in% c(-20, 1:3, 6), 0
    )] %>%
  .[, c("p732103") := NULL]


# Check what negative values there are
#vals <- unique(as.vector(as.matrix(df_parent)))
#sort(vals[vals < 0])

for (col in names(df_parent)) {
  set(
    df_parent,
    i = which(df_parent[[col]] %in% c(-98:-90, -54, -53, -23:-20)),
    j = col,
    value = NA
  )
}

# Recode further variables.
df_parent %>%
  .[, low_ses_books := fcase(
    nof_books %in% c(1, 2), 1,
    nof_books %in% c(3:6), 0
  )] %>%
  .[, `:=` (unemp = unemp - 1,
            unemp_p = unemp_p - 1)
    ] %>%
  .[, siblings := fcase(
    nof_siblings == 0, 0,
    nof_siblings >= 1, 1
  )] %>%
  .[, `:=` (
    fem_child = fcase(
      fem_child == 1, 1,
      fem_child == 2, 0
      ),
    fem_parent = fem_parent - 1
    )] %>%
  .[, mom_responds := fcase(
    mom_responds == 1, 1,
    mom_responds == 2, 0
  )] %>%
  .[, married := fcase(
    married %in% c(1, 2, 6), 1,
    # divorced, widowed, single
    married %in% c(3:5), 0
  )] %>%
  .[, `:=` (
    germborn = fcase(
      germborn == 1, 1,
      germborn %in% c(2, 3), 0
      ),
    germborn_p = fcase(
      germborn_p == 1, 1, 
      germborn_p %in% c(2, 3), 0
      )
    )
    ] %>%
  .[, migback := fcase(
    migback == 0, 0,
    migback %in% c(1:10), 1
  )]

# Parenting styles items
cols_ps <- c(
  # Parenting styles (powerful enforcement, W6)
  str_c("p66813", letters[1:4]),
  #Parenting styles (emotional warmth, W7)
  str_c("p66810", letters[1:3]), 
  #Parenting styles (inconsistent parenting, W5, 7; see age gradient!)
  str_c("p66812", letters[1:4]), 
  #Parenting styles (negative communication, W5, 7)
  str_c("p66811", letters[1:3]),
  #Parenting styles (monitoring, W7)
  str_c("p66814", letters[1:4]),
  # Parenting styles (autonomy, W6, 8)
  str_c("p66816", letters[1:5]),
  # Parenting styles (pos. parent. behav., W8)
  str_c("p66818", letters[1:3]),
  # Parenting styles (psych. control, W8; could be used for disagreement?)
  str_c("p66817", letters[1:3])
)

# First build items yourself
cols_ps_agg <- c(
  "power_enforce", "emot_warmth", "inconsist_parent", "neg_comm", "monitoring",
  "autonomy", "pos_parent_behav", "psych_control"
  )
df_p_ps_2 <- df_parent %>%
  # Parenting styles only available from W6 onward.
  .[wave %in% c(6:8), .SD, .SDcols = c("ID_t", "wave", cols_ps)] %>%
  # Construct measures of latent constructs from individual items.
  .[,
    power_enforce := rowMeans(.SD, na.rm = TRUE),
    .SDcols = str_c("p66813", letters[1:4])
    ] %>%
  .[,
    emot_warmth := rowMeans(.SD, na.rm = TRUE),
    .SDcols = str_c("p66810", letters[1:3])
    ] %>%
  .[,
    inconsist_parent := rowMeans(.SD, na.rm = TRUE),
    .SDcols = str_c("p66812", letters[1:4])
    ] %>% 
  .[,
    neg_comm := rowMeans(.SD, na.rm = TRUE),
    .SDcols = str_c("p66811", letters[1:3])
  ] %>% 
  .[,
    monitoring := rowMeans(.SD, na.rm = TRUE),
    .SDcols = str_c("p66814", letters[1:4])
  ] %>% 
  .[,
    autonomy := rowMeans(.SD, na.rm = TRUE),
    .SDcols = str_c("p66816", letters[1:5])
  ] %>% 
  .[,
    pos_parent_behav := rowMeans(.SD, na.rm = TRUE),
    .SDcols = str_c("p66818", letters[1:3])
  ] %>% 
  .[,
    psych_control := rowMeans(.SD, na.rm = TRUE),
    .SDcols = str_c("p66817", letters[1:3])
  ] %>% 
  # Delete individual items
  .[, (cols_ps) := NULL] %>%
  # Subtract means, but do not divide by SD.
  .[, 
    (cols_ps_agg) := lapply(.SD, function(x) as.vector(scale(x, scale = F))),
    .SDcols = cols_ps_agg,
    by = "wave"
    ] %>%
  # Build individual means across waves. See above which items were elicited
  # multiple times.
  .[, by = "ID_t", lapply(.SD, mean, na.rm = TRUE), .SDcols = cols_ps_agg] %>%
  # This reduces the number of observations drastically.
  na.omit()

pairs(df_p_ps_2[, -1], lower.panel = NULL)

# PCA
ps_pca <- prcomp(df_p_ps_2[, -1], scale = TRUE)
ps_pca

ps_var <- ps_pca$sdev^2
# proportion of variance explained
ps_var/sum(ps_var)
# cumulative
cumsum(ps_var)/sum(ps_var)

# Plot, looks interesting
biplot(ps_pca)

# predictions
ps_pred <- predict(ps_pca)


gmm <- Mclust(df_p_ps_2[, -1])
gmm$classification
df_p_ps_2 %>%
  .[, class := gmm$classification]
summary(gmm)
#plot(gmm)



# This keeps all single items, but apart from that does the same as above.
  df_p_ps <- df_parent %>%
  .[wave %in% c(6:8), .SD, .SDcols = c("ID_t", "wave", cols_ps)] %>%
  .[,
    (cols_ps) := lapply(.SD, function(x) as.vector(scale(x, scale = FALSE))),
    .SDcols = cols_ps,
    by = "wave"
    ] %>%
  .[, by = "ID_t", lapply(.SD, mean, na.rm = TRUE), .SDcols = cols_ps] %>%
  na.omit()


# Collective Activities
cols_ca <- c(
  # Collective activities (W1)
  str_c("p28130", 1:9),
  # Collective activities, pretending (W2)
  str_c("p28131", 1:6), #"p281326",
  # Collective activities (W3, 4, 5)
  str_c("p28132", 1:7)  
)

df_p_ca <- df_parent %>%
  .[wave %in% c(1:5), .SD, .SDcols = c("ID_t", "wave", cols_ca)] %>%
  .[,
    (cols_ca) := lapply(.SD, function(x) as.vector(scale(x, scale = FALSE))),
    .SDcols = cols_ca,
    by = "wave"
    ] %>%
  .[, by = "ID_t", lapply(.SD, mean, na.rm = TRUE), .SDcols = cols_ca] %>%
  .[, time_invest := rowMeans(.SD, na.rm = TRUE), .SDcols = patterns("^p28")] %>%
  .[,
    time_invest := lapply(.SD, function(x) as.vector(scale(x))),
    .SDcols = "time_invest"
  ]



df_p_test <- merge.data.table(
  df_p_ca[, c("ID_t", "time_invest")], df_p_ps_2[, c("ID_t", "class")], by = "ID_t",
  all.x = TRUE
) %>%
  na.omit()

plot <- ggplot(df_p_test, aes(x = time_invest, group = class, fill = class)) +
  geom_density(adjust=1.5, alpha=.4)

# Find out and merge the daatsets with the relevant information.
# To check selective attrition would need all SES variables b/c those are the
# dimensions that matter.
# Check out Rauh/Renée how exactly their data look like and then also del bono, 
# Dohmen


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
      sens_stress_p = c("ihn1p001_c", "ihn1p001_sc1n2_c", "ihn1p001_sc1n3_c"),
      sens_n_stress_p = c("ihn1p002_c", "ihn1p002_sc1n2_c", "ihn1p003_sc1n3_c"), 
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

# Scale the interaction items by wave (wave FE), mainly to remove age effects.
cols <- names(df_dm)[names(df_dm) %like% "_(p|c)"]
# Scale
df_dm %>%
  .[, 
    (cols) := lapply(.SD, function(x) as.vector(scale(x))),
    .SDcols = cols,
    by = "wave"
  ]
# Check whether there are correlations across waves of the values?!

cols_dm <- c(names(df_dm)[names(df_dm) %like% "_(p|c)$"], "not_speak")
df_dm_cs <- df_dm %>%
  .[!deviations == 1, ] %>%
  .[, by = "ID_t", lapply(.SD, mean, na.rm = TRUE), .SDcols = cols_dm]

# Did the scaling work? (also with mean)
#df_t <- df_dm %>%
#  .[, by = "wave", lapply(.SD, mean, na.rm = T), .SDcols = cols]



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



















library(haven)
library(tidyverse)
library(data.table)
library(viridis)
library(hrbrthemes)

#library(mclust)


# Paths for datasets.
path_in_data <- "src/original_data/"
path_out_data <- "bld/out/data/"

source("src/functions/functions.r")
# The first set of items just goes through the codebook on pParent from the 
# start and contains the things I deem worth of keeping.
# The second set focuses more specifically on the main categories of items I 
# want to employ for my analyses.

cols_basic <- c(
  "ID_t", "wave",
  # Week of pregnancy at birth
  #"p529100",
  # Weight at birth
  #"p529000_R",
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
  "p435000",
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
  # Parenting goals (first 3 status, 2 autonomy, 2 competencies)
  # c is (impotance of) obedience, d is independence, f is diligence 
  str_c("p67800", letters[1:7]),
  # highest professional qualification, partner
  "p731813", "p731863"
)

cols_pg <- str_c("p67800", letters[1:7])

cols_ca <- c(
  # Collective activities (W1)
  str_c("p28130", 1:9),
  # Collective activities, pretending (W2)
  str_c("p28131", 1:6),
  # Collective activities (W3, 4, 5)
  str_c("p28132", 1:7)
)

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
# nof siblings + sibling's year of birth can be used for birth order!


df_parent <- read_dta(str_c(path_in_data, "SC1_pParent_D_8-0-0.dta")) %>%
  setDT(., key = c("ID_t", "wave")) %>%
  .[, .SD, .SDcols = c(cols_basic, cols_ps, cols_ca)] %>%
  setnames(
    .,
    old = c(
      "p526200", "p34005a", "p731501", "p731601",
      "pb10000", "p700010", "p731701", "p731702", "p731110",
      "p400000", "p403000", "p400500_g1",
      "p510005_g1", "p517100", "p515051", "p515100", "p435000",
      "p731813", "p731863", "p73170y", "p73175y"
    ),
    new = c(
      "dur_bf_month", "nof_books", "unemp", "unemp_p",
      "nof_siblings", "fem_child", "fem_parent", "mom_responds", "married",
      "germborn", "germborn_p", "migback",
      "net_hh_inc", "nc_trust", "nc_risk", "nc_patience", "religious",
      "univ_deg", "univ_deg_p", "age_mom", "age_dad"
      
    )
  )

# Recode variables that give meaning to otherwise missing coding.
df_parent %>%
  # p. 
  .[dur_bf_month %in% c(-93, 0), dur_bf_month := 0] %>%
  # p. 3730, 3919
  .[wave %in% c(4, 8), nof_siblings := p732103] %>%
  .[, fh_abi := fcase(
    p731802 %in% c(4, 5), 1,
    p731802 %in% c(-20, 1:3, 6), 0
  )] %>%
  .[, fh_abi_p := fcase(
    p731852 %in% c(4, 5), 1,
    p731852 %in% c(-20, 1:3, 6), 0
  )] %>%
  # no professional degree
  .[univ_deg == -20, univ_deg := 0] %>%
  .[univ_deg_p == -20, univ_deg_p := 0] %>%
  .[, c("p732103") := NULL]


# Check what negative values there are
#vals <- unique(as.vector(as.matrix(df_parent)))
#sort(vals[vals < 0])

for (col in names(df_parent)) {
  set(
    df_parent,
    i = which(df_parent[[col]] %in% c(-98:-90, -55:-53, -23:-20)),
    j = col,
    value = NA
  )
}

# Recode further variables.
df_parent %>%
  .[, low_ses_books := fcase(
    nof_books %in% c(1, 2, 3), 1,
    nof_books %in% c(4:6), 0
  )] %>%
  .[, `:=` (unemp = unemp - 1,
            unemp_p = unemp_p - 1)
  ] %>%
  .[, siblings := fcase(
    nof_siblings == 0, 0,
    nof_siblings >= 1, 1
  )] %>%
  .[wave == 1, siblings_at_birth := fcase(
    nof_siblings >= 1, 1,
    nof_siblings == 0, 0)] %>%
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
    married %in% c(1, 6), 1,
    # divorced, widowed, single
    married %in% c(2:5), 0
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
  )] %>%
  .[, univ_deg := fcase(
    univ_deg %in% c(0, 1:7, 17, 19, 21), 0,
    univ_deg %in% c(8:16), 1
  )] %>%
  .[, univ_deg_p := fcase(
    univ_deg_p %in% c(0, 1:7, 17, 19, 21), 0,
    univ_deg_p %in% c(8:16), 1
  )] %>%
  .[, univ_deg_b := rowSums(.SD, na.rm = T), .SDcols = patterns("^univ_")] %>%
  .[is.na(univ_deg) & is.na(univ_deg_p), univ_deg_b := NA] %>%
  .[, both_p_univ_deg := fcase(
    univ_deg_b == 2, 1,
    univ_deg_b %in% c(0,1), 0
    )] %>%
  .[, `:=`(
    age_mom = 2012 - age_mom, age_dad = 2012 - age_dad
  )]


# Build parenting styles.
cols_ps_agg <- c(
  "power_enforce", "emot_warmth", "inconsist_parent", "neg_comm", "monitoring",
  "autonomy", "pos_parent_behav", "psych_control"
)
# Copy the dataframe
df_p_ps <- copy(df_parent)
# Construct concepts by taking rowmeans, get rid of age fixed effects, and build
# a cross section.
df_p_ps <- df_p_ps %>%
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
  # Subtract means, but do not divide by SD to get rid of age effects.
  .[, 
    (cols_ps_agg) := lapply(.SD, function(x) as.vector(scale(x, scale = F))),
    .SDcols = cols_ps_agg,
    by = "wave"
  ] %>%
  # Build individual means across waves. See above which items were elicited
  # multiple times.
  .[, by = "ID_t", lapply(.SD, mean, na.rm = TRUE), .SDcols = cols_ps_agg] %>%
  #.[, 
  #  (cols_ps_agg) := lapply(.SD, function(x) as.vector(scale(x))),
  #  .SDcols = cols_ps_agg,
  #] %>%
  # This reduces the number of observations drastically.
  na.omit() %>%
  fwrite(., str_c(path_out_data, "df_parent_styles_cs.csv"))

# Remove parenting styles columns from dataframe.
df_parent[, (cols_ps) := NULL]




# Collectice activities
df_p_ca <- copy(df_parent)
df_p_ca <- df_p_ca %>%
  .[wave %in% c(1:5), .SD, .SDcols = c("ID_t", "wave", cols_ca)] %>%
  # Reverse scales.
  .[, (cols_ca) := lapply(.SD, rev_likert, min = 1, max = 5), .SDcols = cols_ca] %>%
  # Get rid of age FE.
  .[,
    (cols_ca) := lapply(.SD, function(x) as.vector(scale(x, scale = FALSE))),
    .SDcols = cols_ca,
    by = "wave"
  ] %>%
  # Build cross section by taking individual means across waves.
  .[, by = "ID_t", lapply(.SD, mean, na.rm = TRUE), .SDcols = cols_ca] %>%
  .[, time_invest := rowMeans(.SD, na.rm = TRUE), .SDcols = cols_ca] %>%
  .[,
    time_invest := lapply(.SD, function(x) as.vector(scale(x))),
    .SDcols = "time_invest"
  ] %>%
  fwrite(., str_c(path_out_data, "df_collective_act_cs.csv"))

df_parent[, (cols_ca) := NULL]



# Build SES cross-section

df_ses <- copy(df_parent)
cols_ses <- c(
  "unemp", "unemp_p", "fem_child", "married", "germborn", "germborn_p",
  "migback", "net_hh_inc", "nc_trust", "nc_risk", "nc_patience", "fh_abi",
  "fh_abi_p",
  "low_ses_books", "ID_t", "religious", "univ_deg", "univ_deg_p", "univ_deg_b",
  "both_p_univ_deg", "age_mom", "age_dad", "siblings_at_birth"
)
cols_univ <- str_subset(cols_ses, "^univ_deg")
cols_nc <- c(str_subset(cols_ses, "^nc_"))
df_ses <- df_ses %>%
  .[, .SD, .SDcols = cols_ses] %>%
  .[, by = "ID_t", (cols_univ) := lapply(.SD, max_fun), .SDcols = cols_univ] %>%
  .[, lapply(.SD, mean, na.rm = TRUE), by = "ID_t"] %>%
  .[, (cols_nc) := lapply(.SD, function(x) as.vector(scale(x))), 
    .SDcols = cols_nc
    ] %>%
  .[, both_p_abi := fcase(
    fh_abi == 1 & fh_abi_p == 1, 1,
    fh_abi == 0 | fh_abi == 0, 0
  )] %>%
  fwrite(., str_c(path_out_data, "df_ses_nc.csv"))



# Parenting goals to validate classes
df_pg <- copy(df_parent)
df_pg <- df_pg %>%
  .[wave == 2, .SD, .SDcols = c("ID_t", "wave", cols_pg)] %>%
  # standardize
  .[, (cols_pg) := lapply(.SD, function(x) as.vector(scale(x))),
    .SDcols = cols_pg] %>%
  setnames(
    ., 
    old = str_c("p67800", letters[1:7]),
    # could differ by gender.
    new = c(
      "respected", "assertive", "obedient", "independent", "own_opin",
      "diligent", "sense_of_resp"
      )
  ) %>%
  .[, !c("wave")] %>%
  fwrite(., str_c(path_out_data, "df_pg.csv"))

# Find out and merge the daatsets with the relevant information.
# To check selective attrition would need all SES variables b/c those are the
# dimensions that matter.
# Check out Rauh/Renée how exactly their data look like and then also del bono, 
# Dohmen

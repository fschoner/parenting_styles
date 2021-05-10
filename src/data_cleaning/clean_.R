
library(haven)
library(tidyverse)
library(data.table)



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
  "p529100",
  # Weight at birth
  "p529000_R",
  # Temperament
  str_c("p66804", c("c", "e", "f", "k", "n", "o")),
  # Coparenting
  str_c("pa030", c(3:5), 0),
  # Breastfeeding duration
  str_c("p52620", 0:1),
  # Communicative gestures,
  str_c("p1010", 1:2, 0),
  # Receptive language
  "p102040", "p102010",
  #Productive language
  str_c("p1020", 2:3, 0),
  #Cognition: means‐end task
  "p103040",
  #(Fine) Motor skills
  str_c("p1040", c(1, 2), 0), "p104040_v1",
  # Sibling year of birth (1-3)
  str_c("p73222y_w", 1:3),
  # Collective activities, pretending
  str_c("p28131", 1:6), "p281326",
  # Cognition
  "p103060", "p101050",
  # Productive language
  str_c("p1020", 5:6, 0), str_c("p10207", 1:3),
  # Cognition
  str_c("p1030", c(1, 5), 0),
  # Fine) Motor skills: 
  str_c("p1040", 4:7, 0),
  # Books at home, HOMEPOS
  "p34005a", str_c("p34006", c("d", "h", "e", "j", "f")),
  # Employment 12 month prior to birth 
  "p731501", "p731601",
  # COntinue on p. 1751 of the codebook
  
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
  str_c("p67800", letters[1:7]),
  
  # Social trust (W7), Gen. willing. to take risk (W7), patience (W7)
  "p517100", "p515051", "p515100"
  
  # Migration background?
)



df_parent <- read_dta(str_c(path_in_data, "SC1_pParent_D_8-0-0.dta")) %>%
  setDT(., key = c("ID_t", "wave")) %>%
  .[, .SD, .SDcols = keep_p]




# Find out and merge the daatsets with the relevant information.
# To check selective attrition would need all SES variables b/c those are the
# dimensions that matter.
# Check out Rauh/Renée how exactly their data look like and then also del bono, 
# Dohmen

# I need xTargetCompetencies and xDirectMeasures
path_in_data <- "src/original_data/"

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
  setDT(., key = c("ID_t")) %>%
  .[, .SD, .SDcols = keep_dm] %>%
  # COnvert to panel format
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
  setorderv(., "ID_t") 


for (col in names(df_dm)) {
  set(
    df_dm,
    i = which(df_dm[[col]] %in% c(-21, -55, -56)),
    j = col,
    value = NA
  )
}
cols <- names(df_dm)[names(df_dm) %like% "_(p|c)"]
# Scale the columns by wave
df_dm %>%
  .[, 
    (cols) := lapply(.SD, function(x) as.vector(scale(x))),
    .SDcols = cols,
    by = "wave"
  ]

#works
df_t <- df_dm %>%
  .[, by = "wave", lapply(.SD, sd, na.rm = T), .SDcols = cols]

# standardize in each wave before taking individual-specific averages.
# and read about them.





















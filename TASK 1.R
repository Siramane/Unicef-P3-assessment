

# ----------------
# ----- TASK 1
# ----------------


# ------------------------------   Data preparation

# libraries

library(dplyr)
library(readxl)

# Importation with some cleaning 

GLOBAL_2018_2022 <- read_excel("GLOBAL_DATAFLOW_2018-2022.xlsx")
TRACK_STATUS <- read_excel("TRACK.xlsx")
F_POP <- read_excel("F_POP.xlsx") # I already cleaned this data with excel to keep 2022 population


names(GLOBAL_2018_2022)[1]<-"GEO"
names(TRACK_STATUS)[1]<-"GEO_CODE"
names(TRACK_STATUS)[2]<-"GEO"
names(TRACK_STATUS)[3]<-"STATUS"

keeps = c("GEO","Indicator","TIME_PERIOD","Sex","OBS_VALUE")
DATA_ANC_SAB = GLOBAL_2018_2022[keeps]



# Merge DATA_ANC_SAB with TRACK_STATUS by GEO

DATA_ANC_SAB_TRACK <- DATA_ANC_SAB %>%
  inner_join(TRACK_STATUS, by = "GEO")

# Merge DATA_ANC_SAB_TRACK with Female Population data (projected births for 2022 = F_POP)

DATA <- DATA_ANC_SAB_TRACK %>%
  inner_join(F_POP %>% filter(TIME_PERIOD == 2022), by = "GEO")

DATA$F_POP <- as.numeric(DATA$F_POP)
DATA$OBS_VALUE <- as.numeric(DATA$OBS_VALUE)

# Separate DATA into ANC4 and SAB

DATA_ANC <- DATA %>% filter(DATA$Indicator == "Antenatal care 4+ visits - percentage of women (aged 15-49 years) attended at least four times during pregnancy by any provider")

names(DATA_ANC)[5]<-"ANC"

DATA_SAB <- DATA %>% filter(DATA$Indicator == "Skilled birth attendant - percentage of deliveries attended by skilled health personnel")

names(DATA_SAB)[5]<-"SAB"


# --------------------------------------Calculate weighted averages for on-track and off-track countries



# Separate DATA into on-track and off-track for DATA_ANC and DATA_SAB

ON_TRACK_ANC  <- DATA_ANC %>% filter(DATA_ANC$STATUS %in% c("Achieved", "On Track"))
OFF_TRACK_ANC <- DATA_ANC %>% filter(DATA_ANC$STATUS == "Acceleration Needed")

ON_TRACK_SAB  <- DATA_SAB %>% filter(DATA_SAB$STATUS %in% c("Achieved", "On Track"))
OFF_TRACK_SAB <- DATA_SAB %>% filter(DATA_SAB$STATUS == "Acceleration Needed")


# Function to calculate weighted coverage

calculate_weighted_coverages <- function(data, coverage_col, weight_col) {
  sum(data[[coverage_col]] * data[[weight_col]]) / sum(data[[weight_col]])
}

# Calculate weighted coverages for ANC and SAB for on-track and off-track countries

# ANC
WEIGH_ON_TRACK_ANC <- calculate_weighted_coverages(ON_TRACK_ANC, "ANC", "F_POP")    # Where F_POP = projected births for 2022
print(paste("On-track ANC weighted coverage:", round(WEIGH_ON_TRACK_ANC, 2)))

WEIGH_OFF_TRACK_ANC <- calculate_weighted_coverages(OFF_TRACK_ANC, "ANC", "F_POP")
print(paste("Off-track ANC weighted coverage:", round(WEIGH_OFF_TRACK_ANC, 2)))

# SAB
WEIGH_ON_TRACK_SAB <- calculate_weighted_coverages(ON_TRACK_SAB, "SAB", "F_POP")
print(paste("On-track SAB weighted coverage:", round(WEIGH_ON_TRACK_SAB, 2)))

WEIGH_OFF_TRACK_SAB <- calculate_weighted_coverages(OFF_TRACK_SAB, "SAB", "F_POP")
print(paste("Off-track SAB weighted coverage:", round(WEIGH_OFF_TRACK_SAB, 2)))


#------------------------- visual

library(ggplot2)


# Data for plotting
COVERAGE <- data.frame(
  Status = c("On-track", "Off-track"),
  ANC = c(WEIGH_ON_TRACK_ANC, WEIGH_OFF_TRACK_ANC),
  SAB = c(WEIGH_ON_TRACK_SAB, WEIGH_OFF_TRACK_SAB)
)


# Export data to excel for visualization

library(writexl)
write_xlsx(x = COVERAGE, path = "COVERAGE.xlsx", col_names = TRUE)


# Plot
ggplot(COVERAGE, aes(x = Status)) +
  geom_bar(aes(y = ANC, fill = "ANC"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = SAB, fill = "SAB"), stat = "identity", position = "dodge") +
  labs(title = "Population-Weighted Coverage of Health Services",
       y = "Population-Weighted Coverage (%)",
       fill = "Indicator") +
  theme_minimal()


# Interpretation

# The population-weighted coverage for ANC and SAB tends to be higher in on-track countries compared to off-track countries. 
# This indicates that countries on track to achieve under-5 mortality targets are generally providing better maternal health services.



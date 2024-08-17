
# ----------------
# ----- TASK 2
# ----------------

# libraries

library(dplyr)
library(readxl)

library(readr)
library(lubridate)


# Import data 


Zimbabwe_children <- read_csv("Zimbabwe_children.csv")


# Convert categorical responses (Yes=1/No=2/DK=8) to more interpretable formats

Zimbabwe_children <- Zimbabwe_children %>%
  mutate(across(starts_with("EC"), ~ ifelse(. == 1, "Yes", ifelse(. == 2, "No", "DK"))))


# Calculate age in months
Zimbabwe <- Zimbabwe_children %>%
  mutate(interview_date = ymd(interview_date),
         child_birthday = ymd(child_birthday),
         child_age_months = as.numeric(difftime(interview_date, child_birthday, units = "days")) / 30.44)


# Rounds values to specified number of decimal places.
Zimbabwe$child_age_months = round(Zimbabwe$child_age_months, digits = 0)

# Define 4 categories for age in months
Zimbabwe_group_month <- Zimbabwe %>%
  mutate(
    age_category = case_when(
      child_age_months >= 36 & child_age_months <= 41 ~ "36-41 months",
      child_age_months >= 42 & child_age_months <= 47 ~ "42-47 months",
      child_age_months >= 48 & child_age_months <= 53 ~ "48-53 months",
      child_age_months >= 54 & child_age_months <= 60 ~ "54-59 months"
    )
  )

# Remove NA values in the new category column
Zimbabwe_group_month <- Zimbabwe_group_month %>% filter(!is.na(age_category))

# Group data by age and calculate proportions for each educational area

group_Zimba <- Zimbabwe_group_month %>%
  group_by(age_category) %>%
  summarize(LITERACY_rate = mean(EC6 == "Yes" | EC7 == "Yes", na.rm = TRUE),
            MATH_rate = mean(EC8 == "Yes", na.rm = TRUE),
            PHYSICAL_rate = mean(EC9 == "Yes" | EC10 == "Yes", na.rm = TRUE),
            LEARNING_rate = mean(EC11 == "Yes" | EC12 == "Yes", na.rm = TRUE),
            SOCIO_EMOTIONAL_rate = mean(EC13 == "Yes" | EC14 == "Yes" | EC15 == "Yes", na.rm = TRUE))


# Export data to excel for visualization

library(writexl)
write_xlsx(x = group_Zimba, path = "group_Zimba.xlsx", col_names = TRUE)


# Create a bar plot to visualize the proportions by age category
ggplot(group_Zimba, aes(x = age_category)) +
  geom_bar(aes(y = LITERACY_rate, fill = "Literacy"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = MATH_rate, fill = "Math"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = PHYSICAL_rate, fill = "Physical Development"), stat = "identity", position = "dodge") +
  labs(
    title = "Proportion of Children Achieving Educational Milestones by Age Category",
    x = "Age Category (Months)",
    y = "Proportion of Children",
    fill = "Milestone"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )


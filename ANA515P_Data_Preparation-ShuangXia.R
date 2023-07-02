# ANA515P Data Preparation - Shuang Xia
# Cleaning NYC GT data

# Load required packages
library(stringr)
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

# Read the two Excel files copied to this working directory
data1 <- read_excel("Copy of G&T Results 2017-18 (Responses) - Form Responses 1.xlsx")
data2 <- read_excel("Copy of G&T Results 2018-19 Responses - Sheet1.xlsx")

# The last two columns of the 2018-19 file have no column title and contain some
#  home schooling information that the 2017-18 file doesn't have. Need to remove
#  those values before combining two sheets.

# Remove the last two columns from the second data frame
data2 <- data2 %>% select(-ncol(data2):-(ncol(data2)-1))

# Combine the two data frames
combined_data <- rbind(data1, data2)

# Some of the dates in 'Timestamp' column are impossible, set to NA
combined_data$Timestamp <- with_tz(combined_data$Timestamp, tzone = "America/New_York")
# Replace dates outside 2017 and 2018 with NA_Date_
combined_data <- combined_data %>% 
  mutate(Timestamp = ifelse(year(Timestamp) %in% c(2017, 2018), Timestamp, NA_Date_))
# Convert back to datetime format
combined_data$Timestamp <- as.POSIXct(combined_data$Timestamp, origin = "1970-01-01", tz = "America/New_York")

# Standardize 'Entering Grade Level' column
combined_data <- combined_data %>% 
  mutate(`Entering Grade Level` = case_when(
    `Entering Grade Level` %in% c("first") ~ "1",
    `Entering Grade Level` %in% c("second") ~ "2",
    `Entering Grade Level` %in% c("kinder", "Kinder", "kindergarten", "Kindergarten", "k") ~ "K",
    TRUE ~ as.character(`Entering Grade Level`)
  ))

# Set non-integer values in 'District' column to NA
combined_data <- combined_data %>%
  mutate(District = suppressWarnings(as.integer(District)))

# Standardize 'Birth Month' column
combined_data <- combined_data %>% 
  mutate(`Birth Month` = ifelse(`Birth Month` %in% 1:12, month.name[as.integer(`Birth Month`)], `Birth Month`))

# Convert 'OLSAT Verbal Score' and 'NNAT Non Verbal Raw Score' to numeric, apply rules
combined_data <- combined_data %>%
  mutate(`OLSAT Verbal Score` = suppressWarnings(as.numeric(`OLSAT Verbal Score`)),
         `OLSAT Verbal Score` = ifelse(`OLSAT Verbal Score` <= 1, `OLSAT Verbal Score` * 30, `OLSAT Verbal Score`),
         `OLSAT Verbal Score` = ifelse((`OLSAT Verbal Score` > 1 & `OLSAT Verbal Score` < 10) | `OLSAT Verbal Score` > 30, NA, `OLSAT Verbal Score`),
         `OLSAT Verbal Score` = as.integer(`OLSAT Verbal Score`),
         `NNAT Non Verbal Raw Score` = suppressWarnings(as.numeric(`NNAT Non Verbal Raw Score`)),
         `NNAT Non Verbal Raw Score` = ifelse(`NNAT Non Verbal Raw Score` <= 1, `NNAT Non Verbal Raw Score` * 48, `NNAT Non Verbal Raw Score`),
         `NNAT Non Verbal Raw Score` = ifelse((`NNAT Non Verbal Raw Score` > 1 & `NNAT Non Verbal Raw Score` < 10) | `NNAT Non Verbal Raw Score` > 48, NA, `NNAT Non Verbal Raw Score`))

# Convert 'OLSAT Verbal Percentile' and 'NNAT Non Verbal Percentile' to numeric, apply rules
combined_data <- combined_data %>%
  mutate(`OLSAT Verbal Percentile` = suppressWarnings(as.numeric(`OLSAT Verbal Percentile`)),
         `OLSAT Verbal Percentile` = ifelse(`OLSAT Verbal Percentile` < 1 & `OLSAT Verbal Percentile` > 0, `OLSAT Verbal Percentile` * 100, `OLSAT Verbal Percentile`),
         `OLSAT Verbal Percentile` = ifelse(`OLSAT Verbal Percentile` < 10, NA, `OLSAT Verbal Percentile`),
         `OLSAT Verbal Percentile` = as.integer(`OLSAT Verbal Percentile`),
         `NNAT Non Verbal Percentile` = suppressWarnings(as.numeric(`NNAT Non Verbal Percentile`)),
         `NNAT Non Verbal Percentile` = ifelse(`NNAT Non Verbal Percentile` < 1 & `NNAT Non Verbal Percentile` > 0, `NNAT Non Verbal Percentile` * 100, `NNAT Non Verbal Percentile`),
         `NNAT Non Verbal Percentile` = ifelse(`NNAT Non Verbal Percentile` < 10, NA, `NNAT Non Verbal Percentile`),
         `NNAT Non Verbal Percentile` = as.integer(`NNAT Non Verbal Percentile`))

# Fill missing percentiles
combined_data <- combined_data %>%
  arrange(`OLSAT Verbal Score`) %>%
  fill(`OLSAT Verbal Percentile`, .direction = "downup") %>%
  arrange(`NNAT Non Verbal Raw Score`) %>%
  fill(`NNAT Non Verbal Percentile`, .direction = "downup")

# For here it would be dangerous to try to back fill scores with the filled 
# percentiles, which requires heavy assumption about the data. Not doing that.

# Edit 'Will you enroll there?' column
combined_data <- combined_data %>%
  mutate(`Will you enroll there?` = case_when(
    grepl("^Yes|^YES|^yes", `Will you enroll there?`, ignore.case = TRUE) ~ "Yes",
    grepl("^No|^NO|^no", `Will you enroll there?`, ignore.case = TRUE) ~ "No",
    !is.na(`Will you enroll there?`) ~ "Maybe",
    TRUE ~ NA_character_
  ))

# School name columns have complex variation of names, not touching them. Will
# only use in plot to see if assigned school meets preference.

# Save the cleaned data to a CSV file
write.csv(combined_data, "cleaned_NYC_GT.csv", row.names = FALSE)


# Plots

# Box plot of percentiles
ggplot(percentiles, aes(y = Percentile, x = Test)) +
  geom_boxplot(fill = "skyblue", outlier.shape = NA) +
  coord_flip() +
  labs(x = "Test", y = "Percentile", title = "Distribution of Test Percentiles")

# Pie chart of "Will you enroll there?"
ggplot(combined_data, aes(x = "", fill = `Will you enroll there?`)) +
  geom_bar(width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(fill = "Enrollment Decision", title = "Enrollment Decision Frequencies")

# Create a new variable for school matching
combined_data <- combined_data %>% 
  mutate(Assigned_in_Preferences = ifelse(str_detect(`School Preferences`, fixed(`School Assigned`)), "Yes", "No"))
# Plot a bar chart
ggplot(combined_data, aes(x = `Assigned_in_Preferences`)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Is Assigned School in Preferences?", y = "Count", title = "Assigned Schools vs. School Preferences")


#1. INSTALL REQUIRED LIBRARIES
library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(dplyr)

cat("Libraries loaded successfully.\n\n")


# 2. READ THE DATA

cat("=====================================================\n")
cat("STEP 2: Reading the dataset...\n")
cat("=====================================================\n\n")

crash_data_raw <- read_csv("~/Documents/Principle and Stat method of Data Science/project /Motor_Vehicle_Collisions_-_Crashes_20260218.csv",
                           show_col_types = FALSE)

cat("Rows:", nrow(crash_data_raw), "\n")
cat("Columns:", ncol(crash_data_raw), "\n\n")



print(head(crash_data_raw, 5))


print(names(crash_data_raw))

# 3. CLEAN COLUMN NAMES

cat("STEP 3: Standardizing column names...\n")

crash_data <- crash_data_raw %>%
  clean_names()

cat("Column names (in snake_case):\n")
print(names(crash_data))

cat("Structure of the dataset:\n")
glimpse(crash_data)
cat("\n")


# 4. INITIAL MISSING DATA ANALYSIS

cat("STEP 4: Analyzing missing values in each column...\n")

missing_summary <- crash_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "column_name",
               values_to = "missing_count") %>%
  mutate(
    missing_percent = round((missing_count / nrow(crash_data)) * 100, 2)
  ) %>%
  arrange(desc(missing_percent))

cat("Missing value summary (top columns with the most missing data):\n")
print(missing_summary)
cat("\n")

cat("Observation:\n")
cat("- Some columns have extremely high missingness.\n")
cat("- We will remove columns with more than 90% missing values.\n")
cat("- For identifier-like or categorical fields, we will use labels like 'Unknown' or 'Not Reported'.\n")
cat("- For coordinates, we will remove rows with missing latitude/longitude instead of imputing fake coordinates.\n")
cat("- For remaining numeric fields, if any NAs remain, we will impute with median.\n\n")


# 5. REMOVE VERY SPARSE COLUMNS
cat("STEP 5: Removing columns with more than 90% missing values...\n")

columns_to_drop <- missing_summary %>%
  filter(missing_percent > 90) %>%
  pull(column_name)

cat("Columns identified for removal (>90% missing):\n")
print(columns_to_drop)
cat("\n")

crash_data <- crash_data %>%
  select(-all_of(columns_to_drop))

cat("Dataset dimensions after dropping very sparse columns:\n")
cat("Rows:", nrow(crash_data), "\n")
cat("Columns:", ncol(crash_data), "\n\n")


# 6. CONVERT DATE/TIME FIELDS

cat("STEP 6: Converting crash_date and crash_time into usable formats...\n")

# Convert crash_date so monthly trend can be created
crash_data <- crash_data %>%
  mutate(
    crash_date = mdy(crash_date),
    crash_month = floor_date(crash_date, unit = "month")
  )

cat("Converted crash_date to Date format.\n")
cat("Created crash_month for monthly trend analysis.\n\n")

# Convert crash_time safely using a dummy date, then extract hour
crash_data <- crash_data %>%
  mutate(
    crash_time_posix = as.POSIXct(
      paste("1970-01-01", crash_time),
      format = "%Y-%m-%d %H:%M",
      tz = "America/New_York"
    ),
    crash_hour = as.numeric(format(crash_time_posix, "%H"))
  )

cat("Converted crash_time to POSIXct using a dummy date.\n")
cat("Extracted crash_hour for hourly trend analysis.\n\n")

cat("Preview of derived date/time fields:\n")
print(head(crash_data %>% select(crash_date, crash_month, crash_time, crash_time_posix, crash_hour), 5))
cat("\n")



# 7. HANDLE LOCATION/COORDINATE MISSINGNESS

cat("=====================================================\n")
cat("STEP 7: Handling missing latitude/longitude values...\n")
cat("=====================================================\n\n")

rows_before_geo_clean <- nrow(crash_data)
rows_before_geo_clean


missing_lat_long_rows <- crash_data %>%
  filter(is.na(latitude) | is.na(longitude)) %>%
  nrow()

cat("Rows with missing latitude or longitude:", missing_lat_long_rows, "\n")

cat("Because latitude/longitude are geographic coordinates,\n")
cat("we will REMOVE rows with missing coordinates instead of imputing median values.\n\n")

crash_data <- crash_data %>%
  filter(!is.na(latitude), !is.na(longitude))

rows_after_geo_clean <- nrow(crash_data)

cat("Rows removed due to missing coordinates:", rows_before_geo_clean - rows_after_geo_clean, "\n")
cat("Remaining rows:", rows_after_geo_clean, "\n\n")



# 8. HANDLE CATEGORICAL NAs

cat("STEP 8: Filling missing categorical values...\n")

# ZIP code is treated like an identifier, so character is more appropriate than numeric.
if ("zip_code" %in% names(crash_data)) {
  crash_data <- crash_data %>%
    mutate(zip_code = as.character(zip_code))
  cat("Converted zip_code to character because ZIP codes are identifiers, not numeric measures.\n\n")
}

categorical_fill_unknown <- c(
  "borough",
  "zip_code",
  "on_street_name",
  "cross_street_name",
  "off_street_name",
  "vehicle_type_code_1",
  "vehicle_type_code_2"
)

categorical_fill_not_reported <- c(
  "contributing_factor_vehicle_1",
  "contributing_factor_vehicle_2"
)

# Fill known categorical fields if they still exist in the dataset
existing_unknown_cols <- categorical_fill_unknown[categorical_fill_unknown %in% names(crash_data)]
existing_reported_cols <- categorical_fill_not_reported[categorical_fill_not_reported %in% names(crash_data)]

if (length(existing_unknown_cols) > 0) {
  crash_data <- crash_data %>%
    mutate(across(all_of(existing_unknown_cols), ~ replace_na(as.character(.), "Unknown")))
  cat("Filled missing values with 'Unknown' in:\n")
  print(existing_unknown_cols)
  cat("\n")
}

if (length(existing_reported_cols) > 0) {
  crash_data <- crash_data %>%
    mutate(across(all_of(existing_reported_cols), ~ replace_na(as.character(.), "Not Reported")))
  cat("Filled missing values with 'Not Reported' in:\n")
  print(existing_reported_cols)
  cat("\n")
}

# 9. HANDLE REMAINING NUMERIC NAs

cat("=====================================================\n")
cat("STEP 9: Imputing any remaining numeric NAs with the median...\n")
cat("=====================================================\n\n")

numeric_cols <- crash_data %>%
  select(where(is.numeric)) %>%
  names()
numeric_cols

for (col_name in numeric_cols) {
  missing_count <- sum(is.na(crash_data[[col_name]]))
  
  if (missing_count > 0) {
    median_value <- median(crash_data[[col_name]], na.rm = TRUE)
    
    cat("Column:", col_name, "\n")
    cat(" -> Missing values found:", missing_count, "\n")
    cat(" -> Replacing missing values with median:", median_value, "\n\n")
    
    crash_data[[col_name]][is.na(crash_data[[col_name]])] <- median_value
  }
}

cat("Numeric NA imputation step completed.\n\n")


# 10. FILL ANY REMAINING CHARACTER/FCTOR NAs

cat("=====================================================\n")
cat("STEP 10: Final pass to remove any remaining NA values...\n")
cat("=====================================================\n\n")

char_cols <- crash_data %>%
  select(where(is.character)) %>%
  names()

if (length(char_cols) > 0) {
  crash_data <- crash_data %>%
    mutate(across(all_of(char_cols), ~ replace_na(., "Unknown")))
}

factor_cols <- crash_data %>%
  select(where(is.factor)) %>%
  names()

if (length(factor_cols) > 0) {
  crash_data <- crash_data %>%
    mutate(across(all_of(factor_cols), ~ fct_explicit_na(., na_level = "Unknown")))
}

final_na_total <- sum(is.na(crash_data))

cat("Total NA values remaining in the dataset:", final_na_total, "\n\n")

if (final_na_total == 0) {
  cat("Success: The cleaned dataset now contains no missing values.\n\n")
} else {
  cat("Warning: Some missing values still remain and may need manual inspection.\n\n")
}


# 11. CREATE A FEW HELPER VARIABLES
# -----------------------------#
cat("=====================================================\n")
cat("STEP 11: Creating helper variables for analysis...\n")
cat("=====================================================\n\n")

crash_data <- crash_data %>%
  mutate(
    total_casualties = number_of_persons_injured + number_of_persons_killed,
    severe_crash = if_else(total_casualties > 0, "Injury/Fatality Crash", "No Injury Crash")
  )

cat("Added:\n")
cat("- total_casualties\n")
cat("- severe_crash\n\n")

cat("Final cleaned dataset dimensions:\n")
cat("Rows:", nrow(crash_data), "\n")
cat("Columns:", ncol(crash_data), "\n\n")


# 12. VISUALIZATION 1
# Missingness before cleaning
# -----------------------------#
cat("=====================================================\n")
cat("VISUAL 1: Missing data percentage by column (before cleaning)\n")
cat("=====================================================\n\n")

p1 <- missing_summary %>%
  filter(missing_percent > 0) %>%
  ggplot(aes(x = reorder(column_name, missing_percent), y = missing_percent)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Missing Data Percentage by Column (Before Cleaning)",
    x = "Column",
    y = "Missing Percentage"
  ) +
  theme_minimal()

print(p1)


# -----------------------------#
# 13. VISUALIZATION 2
# Crashes by borough
# -----------------------------#
cat("=====================================================\n")
cat("VISUAL 2: Number of crashes by borough\n")
cat("=====================================================\n\n")

borough_summary <- crash_data %>%
  count(borough, sort = TRUE)

print(borough_summary)
cat("\n")

p2 <- borough_summary %>%
  ggplot(aes(x = reorder(borough, n), y = n)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Crash Counts by Borough",
    x = "Borough",
    y = "Number of Crashes"
  ) +
  theme_minimal()

print(p2)


# -----------------------------#
# 14. VISUALIZATION 3
# Monthly crash trend
# -----------------------------#
cat("=====================================================\n")
cat("VISUAL 3: Monthly crash trend over time\n")
cat("=====================================================\n\n")

monthly_crashes <- crash_data %>%
  count(crash_month)

print(head(monthly_crashes, 12))
cat("\n")

p3 <- monthly_crashes %>%
  ggplot(aes(x = crash_month, y = n)) +
  geom_line(linewidth = 1, color = "firebrick") +
  geom_point(size = 2, color = "firebrick") +
  labs(
    title = "Monthly Crash Trend",
    x = "Month",
    y = "Number of Crashes"
  ) +
  theme_minimal()

print(p3)


# -----------------------------#
# 15. VISUALIZATION 4
# Hour-of-day by weekday heatmap
# -----------------------------#
cat("=====================================================\n")
cat("VISUAL 4: Crash frequency by weekday and hour\n")
cat("=====================================================\n\n")

hourly_crashes <- crash_data %>%
  count(crash_hour)

cat("Crash counts by hour:\n")
print(hourly_crashes)
cat("\n")

p_hour <- hourly_crashes %>%
  ggplot(aes(x = crash_hour, y = n)) +
  geom_line(linewidth = 1, color = "firebrick") +
  geom_point(size = 2, color = "firebrick") +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Crash Frequency by Hour of Day",
    x = "Hour of Day (0–23)",
    y = "Number of Crashes"
  ) +
  theme_minimal()

print(p_hour)


# -----------------------------#
# 16. VISUALIZATION 5
# Top contributing factors
# -----------------------------#
cat("=====================================================\n")
cat("VISUAL 5: Top contributing factors (vehicle 1)\n")
cat("=====================================================\n\n")

top_factors <- crash_data %>%
  filter(!contributing_factor_vehicle_1 %in% c("Unspecified", "Unknown", "Not Reported")) %>%
  count(contributing_factor_vehicle_1, sort = TRUE) %>%
  slice_head(n = 10)

print(top_factors)
cat("\n")

p5 <- top_factors %>%
  ggplot(aes(x = reorder(contributing_factor_vehicle_1, n), y = n)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(
    title = "Top 10 Contributing Factors",
    x = "Contributing Factor",
    y = "Number of Crashes"
  ) +
  theme_minimal()

print(p5)


# -----------------------------#
# 17. VISUALIZATION 6
# Correlation heatmap for injury/fatality-related numeric fields
# -----------------------------#
cat("=====================================================\n")
cat("VISUAL 6: Correlation heatmap for injury/fatality measures\n")
cat("=====================================================\n\n")

injury_cols <- c(
  "number_of_persons_injured",
  "number_of_persons_killed",
  "number_of_pedestrians_injured",
  "number_of_pedestrians_killed",
  "number_of_cyclist_injured",
  "number_of_cyclist_killed",
  "number_of_motorist_injured",
  "number_of_motorist_killed",
  "total_casualties"
)

injury_cols <- injury_cols[injury_cols %in% names(crash_data)]

cor_matrix <- crash_data %>%
  select(all_of(injury_cols)) %>%
  cor(use = "complete.obs")

cat("Correlation matrix:\n")
print(round(cor_matrix, 3))
cat("\n")

cor_long <- as.data.frame(cor_matrix) %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation")

p6 <- cor_long %>%
  ggplot(aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = round(correlation, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Correlation Heatmap of Injury/Fatality Variables",
    x = "",
    y = "",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p6)


# -----------------------------#
# 18. OPTIONAL: SAVE CLEANED DATASET
# -----------------------------#
cat("=====================================================\n")
cat("FINAL STEP: Saving cleaned dataset (optional)\n")
cat("=====================================================\n\n")

write_csv(crash_data, "cleaned_motor_vehicle_collisions.csv")

cat("Cleaned dataset saved as: cleaned_motor_vehicle_collisions.csv\n")
cat("All preliminary analysis steps completed successfully.\n")
cat("You now have a cleaned dataset and 6 initial visualizations.\n")
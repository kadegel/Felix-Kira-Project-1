___Objective 2___

The code identifies the most recent area to report its first confirmed COVID-19 case by iterating through the dataset. It checks each region's time-series data to find the earliest date when cases exceed zero. It then compares these dates to determine the latest first-case occurrence across all regions and prints the result.
```

```{r}
# Extract only the date columns (starting from the 5th column)
date_columns <- colnames(confirmations_df)[5:ncol(confirmations_df)]

# Initialize variables to track the most recent first case
latest_first_case_date <- as.Date("2020-01-01", format="%Y-%m-%d")
latest_region <- ""

# Loop through each row to find the first confirmed case
for (i in 1:nrow(confirmations_df)) {
  region_name <- paste(confirmations_df$`Province/State`[i], confirmations_df$`Country/Region`[i], sep=", ")

  # Find the first date where cases > 0
  first_case_index <- which(confirmations_df[i, date_columns] > 0)[1]

  if (!is.na(first_case_index)) {
    first_case_date <- as.Date(date_columns[first_case_index], format="%m/%d/%y")

    # Check if this is the most recent first case
    if (first_case_date > latest_first_case_date) {
      latest_first_case_date <- first_case_date
      latest_region <- region_name
    }
  }
}

# Split latest_region into Province/State and Country/Region
latest_parts <- unlist(strsplit(latest_region, ", "))
province <- latest_parts[1]
country <- latest_parts[2]

# Create recent_confirm for use in Objective 3
recent_confirm <- confirmations_df %>%
  filter(`Province/State` == province, `Country/Region` == country)

# Print the result
print(paste("The most recent area to have its first confirmed case is:", latest_region, "on", latest_first_case_date))

```
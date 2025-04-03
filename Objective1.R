#create variable to hold firts date
first_date <- colnames(confirmations_df)[5]  # "1/22/20"

# Find the region with the most confirmed cases on the first date
max_confirmations <- max(confirmations_df[[first_date]], na.rm = TRUE)
origin_confirm <- confirmations_df %>%
  filter(!!sym(first_date) == max_confirmations) %>%
  select(`Province/State`, `Country/Region`)

# Find the region with the most deaths on the first date
max_deaths <- max(deaths_df[[first_date]], na.rm = TRUE)
origin_death <- deaths_df %>%
  filter(!!sym(first_date) == max_deaths) %>%
  select(`Province/State`, `Country/Region`)

# Convert to character strings for better comparison
origin_confirm_str <- paste(origin_confirm$`Province/State`, origin_confirm$`Country/Region`, sep=", ")
origin_death_str <- paste(origin_death$`Province/State`, origin_death$`Country/Region`, sep=", ")

# Check if both values match using an if statement
if (all(origin_confirm_str %in% origin_death_str)) {
  print(paste("The origin of COVID-19 is:", origin_confirm_str))
} else {
  print("The origin is uncertain as the highest confirmations and deaths do not match exactly.")
}

# Print results
print(origin_confirm)
print(origin_death)
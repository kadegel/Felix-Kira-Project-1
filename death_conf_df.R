# Create data frames for deaths and confirmations from csv files
deaths_df <- readr::read_csv("time_series_covid19_deaths_global.csv")
confirmations_df <- readr::read_csv("time_series_covid19_confirmed_global.csv")
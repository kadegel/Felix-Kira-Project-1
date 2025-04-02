# Create a function to create a new data frame with top 5 countries and sums for 
# deaths and confirmations
top_5_country_sums <- function(df)
{
  # Find all unique countries from the data set
  countries <- unique(df[, "Country/Region", drop=TRUE])
  
  # Create empty list to hold sum for each country
  sum_list <- list()
  
  # For loop to add death sum for each country to the list
  for(country in countries)
  {
    sum_list[[country]] <- sum(df[df[,2,drop=TRUE]==country,
                                  grep("[0-9]{1,2}/[0-9]{1,2}/[0-9]{1,2}",
                                       colnames(df))])
  }
  
  # Create new data frame with countries and sums
  sums_df <- data.frame("Country"=countries, "Total"=unlist(sum_list))
  
  # Return df with only the top 5 countries
  return(sums_df[order(sums_df$Total, decreasing=TRUE),][1:5,])
}

# Create tables for the top 5 countries for deaths and confirmations
top_5_death <- top_5_country_sums(deaths_df)
knitr::kable(top_5_death, caption = "Total Deaths by Country: Top 5 Countries",
             row.names = FALSE)

top_5_confirmations <- top_5_country_sums(confirmations_df)
knitr::kable(top_5_confirmations,
             caption = "Total Confirmations by Country: Top 5 Countries",
             row.names = FALSE)
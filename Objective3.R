___Objective 3___

This R code calculates and prints the distance in miles between an origin location and the most recent confirmed case using latitude and longitude data from a dataset.
```
```{r} 
library(geosphere) # Load geosphere package

# Extract coordinates for origin
origin_location <- confirmations_df %>%
  filter(`Province/State` == origin_confirm$`Province/State`, 
         `Country/Region` == origin_confirm$`Country/Region`)

origin_lat <- origin_location$Lat
origin_long <- origin_location$Long

# Extract coordinates for the most recent confirmed case
recent_lat <- recent_confirm$Lat
recent_long <- recent_confirm$Long

# Compute distance in meters
distance_meters <- distm(c(origin_long, origin_lat), c(recent_long, recent_lat))

# Convert to miles
distance_miles <- distance_meters * 0.000621371

# Print formatted output
print(paste(recent_confirm$`Province/State`, recent_confirm$`Country/Region`, 
            "is", round(distance_miles, 2), "miles away from", 
            origin_confirm$`Province/State`, origin_confirm$`Country/Region`))

``` 
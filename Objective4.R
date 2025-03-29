# Create function to calculate risk scores
# Convert NaN values to 0, since they mean there were not any confirmations
risk_score <- function(deaths, confirmations)
{
  risk <- 100 * (deaths/confirmations)
  risk[is.nan(risk)] <- 0
  return(risk)
}

# Filter out cruise ships from the two data frames
deaths_no_cruise <- deaths_df[deaths_df$Lat != 0 & deaths_df$Long != 0
                              & !is.na(deaths_df$Lat)
                              & !is.na(deaths_df$Long),]
confirmations_no_cruise <- confirmations_df[confirmations_df$Lat != 0
                                            & confirmations_df$Long != 0
                                            & !is.na(confirmations_df$Lat)
                                            & !is.na(confirmations_df$Long),]

# Create a vector containing the most recent risk score for each area
current_deaths <- deaths_no_cruise[, ncol(deaths_no_cruise), drop=TRUE]
current_confirmations <- confirmations_no_cruise[, ncol(confirmations_no_cruise),
                                                 drop=TRUE]
current_risk_scores <- risk_score(current_deaths, current_confirmations)

# Find the index (which will correspond to the row) of the lowest risk score
# If more than one are the same, show the one with more confirmations
low_index <- which(current_risk_scores == min(current_risk_scores))
if(length(low_index) > 1)
{
  low_index <- which(current_confirmations == max(current_confirmations[low_index]))
}

# Find and display the area with the lowest risk score
print(paste0("The area that currently has the lowest risk score is ",
             confirmations_no_cruise[low_index,1,drop=TRUE], ", ",
             confirmations_no_cruise[low_index,2,drop=TRUE], ", with a score of ",
             round(current_risk_scores[low_index],2), "%."))

# Find the index (which will refer to the row) of the highest risk score
# If more than one are the same, show the one with more confirmations
high_index <- which(current_risk_scores == max(current_risk_scores))
if(length(high_index) > 1)
{
  high_index <- which(current_confirmations == max(current_confirmations[high_index]))
}

# Find and display the area with the highest risk score
print(paste0("The area that currently has the highest risk score is ",
             confirmations_no_cruise[high_index,2,drop=TRUE],
             ", with a score of ", round(current_risk_scores[high_index],2), "%."))

# Since the highest risk score is over 100%...
# Find the highest risk score which is less than or equal to 100%
risk_scores_adjusted <- ifelse(current_risk_scores > 100, NA, current_risk_scores)
high_index_adjusted <- which(risk_scores_adjusted == max(risk_scores_adjusted, na.rm=TRUE))
if(length(high_index_adjusted) > 1)
{
  high_index_adjusted <- which(current_confirmations == max(
    current_confirmations[high_index_adjusted], na.rm=TRUE))
}

# Find and display the area with the highest risk score <=100
print(paste0("The area that currently has the highest risk score is ",
             confirmations_no_cruise[high_index_adjusted,2,drop=TRUE],
             ", with a score of ",
             round(current_risk_scores[high_index_adjusted],2), "%."))

# Find global risk score
global_risk <- risk_score(sum(current_deaths), sum(current_confirmations))

# Compare global risk score to the highest and lowest risk areas
low_to_global <- global_risk - current_risk_scores[low_index]
high_to_global <- global_risk - current_risk_scores[high_index]
high_adj_to_global <- global_risk - current_risk_scores[high_index_adjusted]

# Display comparisons of high, low, and global risk scores
print(paste0("The global risk score is currently ", round(global_risk,2), "%. ",
             "The area that currently has the lowest risk score has a score ",
             "that is ", round(low_to_global,2), "% lower than the global ",
             "risk score. The area that currently has the highest risk score ",
             "(<= 100%) has a score that is ", abs(round(high_adj_to_global,2)),
             "% higher than the global risk score."))
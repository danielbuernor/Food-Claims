install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
colnames(food_claims)


## Data cleaning
##looking out for missing values
summary(food_claims)
sum(is.na(food_claims$amount_paid))
sum(is.na(food_claims$linked_cases))

## create a duplicate dataset to replace missing values
new_food_claims <- food_claims

group_cause <- new_food_claims %>%
  group_by(cause)

new_food_claims$amount_paid[which(is.na(new_food_claims$amount_paid))] <- 20106
sum(is.na(new_food_claims$amount_paid))

new_food_claims$linked_cases[which(is.na(new_food_claims$linked_cases))] <- 'False'

## Cause column contains 5 categories instead of 3 
unique_causes <- unique(new_food_claims$cause)

## converting all causes to make it consistent
replace_cause <- new_food_claims %>%
  mutate_all(~ gsub("VEGETABLES", "vegetable", ., ignore.case = TRUE)) %>%
  mutate_all(~ gsub("Meat", "meat", ., ignore.case = TRUE))

unique_causes <- unique(new_food_claims$cause)
summary(food_claims)


## removing currency sign in claim_amount to make the claim amount numerical
new_food_claims$claim_amount <- gsub("R\\$ ", "", new_food_claims$claim_amount)

# R still recognise claim_amount as a character value to convert to numeric,
new_food_claims$claim_amount <- as.numeric(gsub("R\\$ ", "", new_food_claims$claim_amount))

library(ggplot2)
# Create a data frame with location and claim_id columns
df_location <- c(new_food_claims, location = 'location', claim_id = 'claim_id')

# Count the number of claims in each location
# to explore the relationship between locations and the number of individuals on claims i created data aggregation for locations and number of claims
claims_agg <- aggregate(individuals_on_claim ~ location, data = new_food_claims, FUN = sum)

ggplot(claims_agg, aes(x = location, y = individuals_on_claim)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = individuals_on_claim), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Number of Individual Claims by Location",
       x = "Location",
       y = "Number of Individual Claims") +
  theme_bw()

## how each location differs in the time it takes to close claims.
claims_agg_2 <- aggregate(time_to_close ~ location, data = new_food_claims, FUN = sum)

ggplot(new_food_claims, aes(x = location, y = time_to_close)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Time to Close Claims by Location",
       x = "Location",
       y = "Time to Close (in days)") +
  theme_minimal()


## How long it takes to reply customers and close claims 

# Created a data frame with the summary statistics
summary(new_food_claims$time_to_close)

summary_df <- data.frame(
  stat = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"),
  value = c(76.0, 158.0, 179.0, 185.6, 204.0, 518.0))

# Plot the histogram
ggplot() +
  geom_histogram(
    data = new_food_claims,  
    aes(x = time_to_close),
    bins = 30,  
    fill = "blue",
    color = "black"
  ) +
  labs(
    title = "Distribution of Time to Close Claims",
    x = "Time to Close (days)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(data = summary_df, aes(xintercept = value, color = stat),
             linetype = "dashed", size = 1, show.legend = TRUE) +
  scale_color_manual(values = c("black", "red", "blue", "green", "red", "black")) +
  annotate("text", x = summary_df$value, y = 25, label = summary_df$stat, color = "black", size = 3.5)
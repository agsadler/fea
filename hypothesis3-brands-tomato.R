# Load necessary libraries
library(dplyr)
library(tidyr)

# Set working directory
setwd("C:/Users/s1985751/Documents/GitHub/fea")

# Load cleaned data
data_clean <- read.xlsx("C:/Users/s1985751/Documents/GitHub/fea/data_clean.xlsx", sheet = "data")


# Initialize an empty list to store the results
results <- list()

# Group by country and brand, and count occurrences
product_summary <- data_clean %>%
  group_by(country, tom_org_brand_new) %>%
  filter(!is.na(tom_org_brand_new)) %>%  # Remove NA values
  summarize(count = n(), .groups = 'drop') %>%
  arrange(country, desc(count)) %>%
  group_by(country) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 3) %>%
  ungroup() %>%
  group_by(country) %>%
  mutate(total = sum(count),
         percentage = round((count / total) * 100, 2)) %>%
  select(country, tom_org_brand_new, count, percentage)

# Print the summary for the specified product
print(product_summary)

# Load necessary libraries
library(dplyr)
library(tidyr)

# Set working directory
setwd("C:/Users/s1985751/Documents/GitHub/fea")

# Load cleaned data
data_clean <- read.xlsx("C:/Users/s1985751/Documents/GitHub/fea/data_clean.xlsx", sheet = "data")

# List of products and their abbreviations
products <- c("tom", "leaf", "ban", "man", "fj", "milk", "cof", 
              "tea", "mill", "chic", "daal", "wht", "rice", "nut")

# Initialize an empty list to store the results
results <- list()

# Loop through each product
for (product in products) {
  brand_column <- paste0(product, "_org_brand_new")
  
  # Group by country and brand, and count occurrences
  product_summary <- data_clean %>%
    group_by(country, !!sym(brand_column)) %>%
    summarize(count = n(), .groups = 'drop') %>%
    arrange(country, desc(count)) %>%
    group_by(country) %>%
    mutate(rank = row_number()) %>%
    filter(rank <= 3) %>%
    ungroup() %>%
    group_by(country) %>%
    mutate(total = sum(count),
           percentage = round((count / total) * 100, 2)) %>%
    select(country, brand = !!sym(brand_column), count, percentage)
  
  # Store the result
  results[[product]] <- product_summary
}

# Combine results into a single dataframe
final_results <- bind_rows(results, .id = "product")

# Reshape the data to wide format
final_table <- final_results %>%
  pivot_wider(names_from = country, values_from = c(count, percentage),
              names_glue = "{country}_{.value}")

# Print the final table
print(final_table)
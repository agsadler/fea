# Load necessary libraries
library(dplyr)
library(gmodels)
library(gt)
library(knitr)
library(kableExtra)
library(openxlsx)
library(readxl)
library(readr)
library(stats)
library(tidyr)

# Set working directory
setwd("C:/Users/s1985751/Documents/GitHub/fea")


# Load cleaned data
data_clean <- read.xlsx("C:/Users/s1985751/Documents/GitHub/fea/data_clean.xlsx", sheet = "data")




# TABLE 6

# Note: If I want to summarise just the number of vendors using the term,
# the code for summing should be >0

# Create new instance of dataset for Table 6
tab6_data <- data_clean


## OVERALL----

# PRODUCT-LEVEL SUMMARY

# Vector of product abbreviations
products <- c("tom", "leaf", "ban", "man", "fj", "milk", "cof", "tea", "mill", "chic", "daal", "wht", "rice", "nut")

# Define a function to summarize data for a given product and dataset
tab6_product_fn <- function(product) {
  tab6_data %>%
    summarise(
      org_count = sum(get(paste0(product, "_org")) == "Yes", na.rm = TRUE),
      term_organic_count = sum(get(paste0(product, "_org_terms_Organic")) == 1, na.rm = TRUE),
      term_organic_perc = term_organic_count / org_count,
      term_natural_count = sum(get(paste0(product, "_org_terms_Natural")) == 1, na.rm = TRUE),
      term_natural_perc = term_natural_count / org_count,
      term_chemfree_count = sum(get(paste0(product, "_org_terms_Chemical-free")) == 1, na.rm = TRUE),
      term_chemfree_perc = term_chemfree_count / org_count,
      term_pestfree_count = sum(get(paste0(product, "_org_terms_Pesticide-free")) == 1, na.rm = TRUE),
      term_pestfree_perc = term_pestfree_count / org_count,
      term_bioprod_count = sum(get(paste0(product, "_org_terms_Bioproducts")) == 1, na.rm = TRUE),
      term_bioprod_perc = term_bioprod_count / org_count,
      term_bio_count = sum(get(paste0(product, "_org_terms_Bio")) == 1, na.rm = TRUE),
      term_bio_perc = term_bio_count / org_count,
      term_eco_count = sum(get(paste0(product, "_org_terms_Eco")) == 1, na.rm = TRUE),
      term_eco_perc = term_eco_count / org_count,
      term_gmo_count = sum(get(paste0(product, "_org_terms_GMO-free")) == 1, na.rm = TRUE),
      term_gmo_perc = term_gmo_count / org_count,
      #term_dontknow_count = sum(get(paste0(product, "_org_terms_Don't know")) == 1, na.rm = TRUE),
      #term_dontknow_perc = term_dontknow_count / org_count
    ) %>%
    mutate(
      product = product,
      category = case_when(product %in% c("fj", "milk", "cof", "tea") ~ "beverages",
                           product %in% c("tom", "leaf", "ban", "man") ~ "fresh produce",
                           TRUE ~ "other")
      ) %>% 
    select(category, product, everything())
}

# Apply the function to each product and combine the results
product_summary_list <- lapply(products, tab6_product_fn)
tab6_product_summary <- bind_rows(product_summary_list)

## CATEGORY-LEVEL SUMMARY

tab6_category_summary <- tab6_product_summary %>%
  group_by(category) %>%
  summarise(
    org_count = sum(org_count, na.rm = TRUE),
    term_organic_count = sum(term_organic_count, na.rm = TRUE),
    term_organic_perc = mean(term_organic_perc, na.rm = TRUE),
    term_natural_count = sum(term_natural_count, na.rm = TRUE),
    term_natural_perc = mean(term_natural_perc, na.rm = TRUE),
    term_chemfree_count = sum(term_chemfree_count, na.rm = TRUE),
    term_chemfree_perc = mean(term_chemfree_perc, na.rm = TRUE),
    term_pestfree_count = sum(term_pestfree_count, na.rm = TRUE),
    term_pestfree_perc = mean(term_pestfree_perc, na.rm = TRUE),
    term_bioprod_count = sum(term_bioprod_count, na.rm = TRUE),
    term_bioprod_perc = mean(term_bioprod_perc, na.rm = TRUE),
    term_bio_count = sum(term_bio_count, na.rm = TRUE),
    term_bio_perc = mean(term_bio_perc, na.rm = TRUE),
    term_eco_count = sum(term_eco_count, na.rm = TRUE),
    term_eco_perc = mean(term_eco_perc, na.rm = TRUE),
    term_gmo_count = sum(term_gmo_count, na.rm = TRUE),
    term_gmo_perc = mean(term_gmo_perc, na.rm = TRUE),
    #term_dontknow_count = sum(term_dontknow_count, na.rm = TRUE),
    #term_dontknow_perc = mean(term_dontknow_perc, na.rm = TRUE)
  )


# Create gt table
tab6_categories_overall <- tab6_category_summary %>%
  gt() %>%
  cols_label(
    category = "Category",
    org_count = "Total organic products",
    term_organic_count = "Organic, n(%)",
    term_organic_perc = " ",
    term_natural_count = "Natural, n(%)",
    term_natural_perc = " ",
    term_chemfree_count = "Chemical-free, n(%)",
    term_chemfree_perc = " ",
    term_pestfree_count = "Pesticide-free, n(%)",
    term_pestfree_perc = " ",
    term_bioprod_count = "Bioproduct, n(%)",
    term_bioprod_perc = " ",
    term_bio_count = "Bio, n(%)",
    term_bio_perc = " ",
    term_eco_count = "Eco, n(%)",
    term_eco_perc = " ",
    term_gmo_count = "GMO-free, n(%)",
    term_gmo_perc = " ",
    #term_dontknow_count = "Don't know, n(%)",
    #term_dontknow_perc = " ",
  ) %>%
  cols_merge(
    columns = c("term_organic_count", "term_organic_perc"),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = c("term_natural_count", "term_natural_perc"),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = c("term_chemfree_count", "term_chemfree_perc"),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = c("term_pestfree_count", "term_pestfree_perc"),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = c("term_bioprod_count", "term_bioprod_perc"),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = c("term_bio_count", "term_bio_perc"),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = c("term_eco_count", "term_eco_perc"),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = c("term_gmo_count", "term_gmo_perc"),
    pattern = "{1} ({2})"
  ) %>%
  #cols_merge(
  #  columns = c("term_dontknow_count", "term_dontknow_perc"),
  #  pattern = "{1} ({2})"
  #) %>%
  fmt_percent(
    columns = c(term_organic_perc, term_natural_perc, term_chemfree_perc,
                   term_pestfree_perc, term_bioprod_perc, term_bio_perc,
                   term_eco_perc, term_gmo_perc),
    decimals = 0
  ) %>%
  tab_header(
    title = "Table 6 - Overall"
  ) %>%
  grand_summary_rows(
    columns = c(org_count, term_organic_count, term_natural_count, term_chemfree_count,
                   term_pestfree_count, term_bioprod_count, term_bio_count,
                   term_eco_count, term_gmo_count),
    fns = list(total = ~sum(., na.rm = TRUE)),
    formatter = fmt_number,
    decimals = 0,
  ) %>%
  sub_missing() 

# Display the table
tab6_categories_overall




# COUNTRY LEVEL SUMMARY: BRAZIL----

# Create new instance of dataset for Table 6 - Brazil
tab6_data_brazil <- tab6_data %>%
  filter(country == "Brazil")

# PRODUCT-LEVEL SUMMARY

# Vector of product abbreviations
products <- c("tom", "leaf", "ban", "man", "fj", "milk", "cof", "tea", "mill", "chic", "daal", "wht", "rice", "nut")

# Define a function to summarize data for a given product and dataset
tab6_product_fn_brazil <- function(product) {
  tab6_data_brazil %>%
    summarise(
      org_count = sum(get(paste0(product, "_org")) == "Yes", na.rm = TRUE),
      term_organic_count = sum(get(paste0(product, "_org_terms_Organic")) == 1, na.rm = TRUE),
      term_organic_perc = term_organic_count / org_count,
      term_natural_count = sum(get(paste0(product, "_org_terms_Natural")) == 1, na.rm = TRUE),
      term_natural_perc = term_natural_count / org_count,
      term_chemfree_count = sum(get(paste0(product, "_org_terms_Chemical-free")) == 1, na.rm = TRUE),
      term_chemfree_perc = term_chemfree_count / org_count,
      term_pestfree_count = sum(get(paste0(product, "_org_terms_Pesticide-free")) == 1, na.rm = TRUE),
      term_pestfree_perc = term_pestfree_count / org_count,
      term_bioprod_count = sum(get(paste0(product, "_org_terms_Bioproducts")) == 1, na.rm = TRUE),
      term_bioprod_perc = term_bioprod_count / org_count,
      term_bio_count = sum(get(paste0(product, "_org_terms_Bio")) == 1, na.rm = TRUE),
      term_bio_perc = term_bio_count / org_count,
      term_eco_count = sum(get(paste0(product, "_org_terms_Eco")) == 1, na.rm = TRUE),
      term_eco_perc = term_eco_count / org_count,
      term_gmo_count = sum(get(paste0(product, "_org_terms_GMO-free")) == 1, na.rm = TRUE),
      term_gmo_perc = term_gmo_count / org_count,
      #term_dontknow_count = sum(get(paste0(product, "_org_terms_Don't know")) == 1, na.rm = TRUE),
      #term_dontknow_perc = term_dontknow_count / org_count
    ) %>%
    mutate(
      product = product,
      category = case_when(product %in% c("fj", "milk", "cof", "tea") ~ "beverages",
                           product %in% c("tom", "leaf", "ban", "man") ~ "fresh produce",
                           TRUE ~ "other")
    ) %>% 
    select(category, product, everything())
}

# Apply the function to each product and combine the results
product_summary_list_brazil <- lapply(products, tab6_product_fn_brazil)
tab6_product_summary_brazil <- bind_rows(product_summary_list_brazil)

## CATEGORY-LEVEL SUMMARY

tab6_category_summary_brazil <- tab6_product_summary_brazil %>%
  group_by(category) %>%
  summarise(
    org_count = sum(org_count, na.rm = TRUE),
    term_organic_count = sum(term_organic_count, na.rm = TRUE),
    term_organic_perc = mean(term_organic_perc, na.rm = TRUE),
    term_natural_count = sum(term_natural_count, na.rm = TRUE),
    term_natural_perc = mean(term_natural_perc, na.rm = TRUE),
    term_chemfree_count = sum(term_chemfree_count, na.rm = TRUE),
    term_chemfree_perc = mean(term_chemfree_perc, na.rm = TRUE),
    term_pestfree_count = sum(term_pestfree_count, na.rm = TRUE),
    term_pestfree_perc = mean(term_pestfree_perc, na.rm = TRUE),
    term_bioprod_count = sum(term_bioprod_count, na.rm = TRUE),
    term_bioprod_perc = mean(term_bioprod_perc, na.rm = TRUE),
    term_bio_count = sum(term_bio_count, na.rm = TRUE),
    term_bio_perc = mean(term_bio_perc, na.rm = TRUE),
    term_eco_count = sum(term_eco_count, na.rm = TRUE),
    term_eco_perc = mean(term_eco_perc, na.rm = TRUE),
    term_gmo_count = sum(term_gmo_count, na.rm = TRUE),
    term_gmo_perc = mean(term_gmo_perc, na.rm = TRUE),
    #term_dontknow_count = sum(term_dontknow_count, na.rm = TRUE),
    #term_dontknow_perc = mean(term_dontknow_perc, na.rm = TRUE)
  )


# Create gt table
tab6_categories_brazil <- tab6_category_summary_brazil %>%
  gt() %>%
  cols_label(
    category = "Category",
    org_count = "Total organic products",
    term_organic_count = "Organic, n(%)",
    term_organic_perc = " ",
    term_natural_count = "Natural, n(%)",
    term_natural_perc = " ",
    term_chemfree_count = "Chemical-free, n(%)",
    term_chemfree_perc = " ",
    term_pestfree_count = "Pesticide-free, n(%)",
    term_pestfree_perc = " ",
    term_bioprod_count = "Bioproduct, n(%)",
    term_bioprod_perc = " ",
    term_bio_count = "Bio, n(%)",
    term_bio_perc = " ",
    term_eco_count = "Eco, n(%)",
    term_eco_perc = " ",
    term_gmo_count = "GMO-free, n(%)",
    term_gmo_perc = " ",
    #term_dontknow_count = "Don't know, n(%)",
    #term_dontknow_perc = " ",
  ) %>%
  cols_merge(
    columns = c("term_organic_count", "term_organic_perc"),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = c("term_natural_count", "term_natural_perc"),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = c("term_chemfree_count", "term_chemfree_perc"),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = c("term_pestfree_count", "term_pestfree_perc"),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = c("term_bioprod_count", "term_bioprod_perc"),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = c("term_bio_count", "term_bio_perc"),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = c("term_eco_count", "term_eco_perc"),
    pattern = "{1} ({2})"
  ) %>%
  cols_merge(
    columns = c("term_gmo_count", "term_gmo_perc"),
    pattern = "{1} ({2})"
  ) %>%
  #cols_merge(
  #  columns = c("term_dontknow_count", "term_dontknow_perc"),
  #  pattern = "{1} ({2})"
  #) %>%
  fmt_percent(
    columns = c(term_organic_perc, term_natural_perc, term_chemfree_perc,
                term_pestfree_perc, term_bioprod_perc, term_bio_perc,
                term_eco_perc, term_gmo_perc),
    decimals = 0
  ) %>%
  tab_header(
    title = "Table 6 - Brazil"
  ) %>%
  grand_summary_rows(
    columns = c(org_count, term_organic_count, term_natural_count, term_chemfree_count,
                term_pestfree_count, term_bioprod_count, term_bio_count,
                term_eco_count, term_gmo_count),
    fns = list(total = ~sum(., na.rm = TRUE)),
    formatter = fmt_number,
    decimals = 0,
  ) %>%
  sub_missing() 

# Display the table
tab6_categories_brazil



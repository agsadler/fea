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

# Overall totals

tab6_overall <- data_clean %>%
  summarise(
    total_org_vendors = sum(org_vendor == 1, na.rm = TRUE),
    term_organic_count = sum(vendor_term_organic_count > 0),
    term_organic_perc = term_organic_count / total_org_vendors,
    term_natural_count = sum(vendor_term_natural_count > 0),
    term_natural_perc = term_natural_count / total_org_vendors,
    term_chemfree_count = sum(vendor_term_chemfree_count > 0),
    term_chemfree_perc = term_chemfree_count / total_org_vendors,
    term_pestfree_count = sum(vendor_term_pestfree_count > 0),
    term_pestfree_perc = term_pestfree_count / total_org_vendors,
    term_bioprod_count = sum(vendor_term_bioprod_count > 0),
    term_bioprod_perc = term_bioprod_count / total_org_vendors,
    term_bio_count = sum(vendor_term_bio_count > 0),
    term_bio_perc = term_bio_count / total_org_vendors,
    term_eco_count = sum(vendor_term_eco_count > 0),
    term_eco_perc = term_eco_count / total_org_vendors,
    term_gmo_count = sum(vendor_term_gmo_count > 0),
    term_gmo_perc = term_gmo_count / total_org_vendors,
    term_dontknow_count = sum(vendor_term_dontknow_count > 0),
    term_dontknow_perc = term_dontknow_count / total_org_vendors
  )

# Country-level totals

tab6_country <- data_clean %>%
  group_by(country) %>%
  summarise(
    total_org_vendors = sum(org_vendor == 1, na.rm = TRUE),
    term_organic_count = sum(vendor_term_organic_count > 0),
    term_organic_perc = term_organic_count / total_org_vendors,
    term_natural_count = sum(vendor_term_natural_count > 0),
    term_natural_perc = term_natural_count / total_org_vendors,
    term_chemfree_count = sum(vendor_term_chemfree_count > 0),
    term_chemfree_perc = term_chemfree_count / total_org_vendors,
    term_pestfree_count = sum(vendor_term_pestfree_count > 0),
    term_pestfree_perc = term_pestfree_count / total_org_vendors,
    term_bioprod_count = sum(vendor_term_bioprod_count > 0),
    term_bioprod_perc = term_bioprod_count / total_org_vendors,
    term_bio_count = sum(vendor_term_bio_count > 0),
    term_bio_perc = term_bio_count / total_org_vendors,
    term_eco_count = sum(vendor_term_eco_count > 0),
    term_eco_perc = term_eco_count / total_org_vendors,
    term_gmo_count = sum(vendor_term_gmo_count > 0),
    term_gmo_perc = term_gmo_count / total_org_vendors,
    term_dontknow_count = sum(vendor_term_dontknow_count > 0),
    term_dontknow_perc = term_dontknow_count / total_org_vendors
  )

# Create gt table for overall and country level totals

# Create the gt table
tab6_totals <- tab6_country %>%
  gt() %>%
  cols_label(
    country = "Country",
    total_org_vendors = "Total organic vendors",
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
    term_dontknow_count = "Don't know, n(%)",
    term_dontknow_perc = " ",
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
  cols_merge(
    columns = c("term_dontknow_count", "term_dontknow_perc"),
    pattern = "{1} ({2})"
  ) %>%
  fmt_percent(
    columns = vars(term_organic_perc, term_natural_perc, term_chemfree_perc,
                   term_pestfree_perc, term_bioprod_perc, term_bio_perc,
                   term_eco_perc, term_gmo_perc, term_dontknow_perc),
    decimals = 0
  ) %>%
  grand_summary_rows(
    columns = vars(total_org_vendors, term_organic_count, term_natural_count, term_chemfree_count,
                   term_pestfree_count, term_bioprod_count, term_bio_count,
                   term_eco_count, term_gmo_count, term_dontknow_count),
    fns = list(total = ~sum(., na.rm = TRUE)),
    formatter = fmt_number,
    decimals = 0,
  ) %>%
  sub_missing() 

# Display the table
tab6_totals


count_products <- data_clean %>%
  summarise(
    tom_vendor_count = sum(tom_sell == "Yes", na.rm = TRUE),
    leaf_vendor_count = sum(leaf_sell == "Yes", na.rm = TRUE),
    ban_vendor_count = sum(ban_sell == "Yes", na.rm = TRUE),
    man_vendor_count = sum(man_sell == "Yes", na.rm = TRUE),
    fj_vendor_count = sum(fj_sell == "Yes", na.rm = TRUE),
    milk_vendor_count = sum(milk_sell == "Yes", na.rm = TRUE),
    cof_vendor_count = sum(cof_sell == "Yes", na.rm = TRUE),
    tea_vendor_count = sum(tea_sell == "Yes", na.rm = TRUE),
    mill_vendor_count = sum(mill_sell == "Yes", na.rm = TRUE),
    chic_vendor_count = sum(chic_sell == "Yes", na.rm = TRUE),
    daal_vendor_count = sum(daal_sell == "Yes", na.rm = TRUE),
    wht_vendor_count = sum(wht_sell == "Yes", na.rm = TRUE),
    rice_vendor_count = sum(rice_sell == "Yes", na.rm = TRUE),
    nut_vendor_count = sum(nut_sell == "Yes", na.rm = TRUE)
    )

product_terms_summary <- data_clean %>%
  summarise(
    tom_vendor_count = sum(tom_sell == "Yes", na.rm = TRUE),
    tom_org_count
  )

# FILE SETUP ----

library(dplyr)
library(kableExtra)
library(openxlsx)
library(readxl)
library(readr)
library(stats)
library(tidyr)

# Load cleaned data
data <- read.xlsx("data_clean.xlsx", sheet = "data")

# Create India dataset

data1 <- data %>%
  filter(country == "India")

# Create worksheet for India table outputs

table_outputs_india <- createWorkbook()
saveWorkbook(table_outputs_india, "Outputs/table_outputs_india.xlsx", overwrite = TRUE)






# Table 2 (all) - Summary of vendors and products ----

# Define order of neighbourhood variable
circle_order <- c("Higher","Middle","Lower")

# Convert the circle column to a factor with the specified levels
data1 <- data1 %>%
  mutate(circle = factor(circle, levels = circle_order))

# Table 2 - neighbourhood level

tab2_neighbourhood_india <- data1 %>%
  group_by(country, city, circle) %>%
  summarise(
    vendors = n(),
    count_org_vendors = sum(org_vendor == 1, na.rm = TRUE),
    org_vendor_perc = round(count_org_vendors / vendors * 100),
    median_org_products_count = median(org_products_count, na.rm = TRUE),
    iqr_org_products_count = IQR(org_products_count, na.rm = TRUE)
  ) %>%
  mutate(
    sum_orgvendors_perc = paste0(count_org_vendors, " (", org_vendor_perc, "%)"),
    sum_med_iqr = paste0(median_org_products_count, " (", iqr_org_products_count, ")")
    ) %>%
  select(country, city, circle, vendors, sum_orgvendors_perc, sum_med_iqr) %>%
  ungroup()

  # Print Table 2 - neighbourhood level
  addWorksheet(table_outputs_india, "Tab2 - neighbourhood")
  writeData(table_outputs_india, sheet = "Tab2 - neighbourhood", x = tab2_neighbourhood_india)
  saveWorkbook(table_outputs_india, "Outputs/table_outputs.xlsx", overwrite = TRUE)


# Table 2 - city level

tab2_city_india <- data1 %>%
  group_by(country, city) %>%
  summarise(
    vendors = n(),
    count_org_vendors = sum(org_vendor == 1, na.rm = TRUE),
    org_vendor_perc = round(count_org_vendors / vendors * 100),
    median_org_products_count = median(org_products_count, na.rm = TRUE),
    iqr_org_products_count = IQR(org_products_count, na.rm = TRUE)
  ) %>%
  mutate(
    sum_orgvendors_perc = paste0(count_org_vendors, " (", org_vendor_perc, "%)"),
    sum_med_iqr = paste0(median_org_products_count, " (", iqr_org_products_count, ")")
  ) %>%
  select(country, city, vendors, sum_orgvendors_perc, sum_med_iqr) %>%
  ungroup()

  # Print Table 2 - city level
  addWorksheet(table_outputs_india, "Tab2 - city")
  writeData(table_outputs_india, sheet = "Tab2 - city", x = tab2_city_india)
  saveWorkbook(table_outputs_india, "Outputs/table_outputs.xlsx", overwrite = TRUE)


# Table 2 - country level

tab2_country_india <- data1 %>%
  group_by(country) %>%
  summarise(
    vendors = n(),
    count_org_vendors = sum(org_vendor == 1, na.rm = TRUE),
    org_vendor_perc = round(count_org_vendors / vendors * 100),
    median_org_products_count = median(org_products_count, na.rm = TRUE),
    iqr_org_products_count = IQR(org_products_count, na.rm = TRUE)
  ) %>%
  mutate(
    sum_orgvendors_perc = paste0(count_org_vendors, " (", org_vendor_perc, "%)"),
    sum_med_iqr = paste0(median_org_products_count, " (", iqr_org_products_count, ")")
  ) %>%
  select(country, vendors, sum_orgvendors_perc, sum_med_iqr) %>%
  ungroup()

  # Print Table 2 - country level
  addWorksheet(table_outputs_india, "Tab2 - country")
  writeData(table_outputs_india, sheet = "Tab2 - country", x = tab2_country_india)
  saveWorkbook(table_outputs_india, "Outputs/table_outputs_india.xlsx", overwrite = TRUE)



# Table 2 (all) - Products - Kruskal-Wallis ----
  
# City-level
  
  ## Hyderabad
  
  # Create data frame for Hyderabad
  hyderabad <- data1 %>%
    filter(city == "Hyderabad")
  
  # Create data frame of circle and org_products_count
  kruskal_hyderabad_data <- hyderabad %>%
    select(circle, org_products_count)
  
  # Perform the Kruskal-Wallis test
  result_hyderabad <- kruskal.test(org_products_count ~ circle, data = kruskal_hyderabad_data)
  
  # Display the results
  print(result_hyderabad)
  
  
  ## Latur
  
  # Create data frame for latur
  latur <- data1 %>%
    filter(city == "Latur")
  
  # Create data frame of circle and org_products_count
  kruskal_latur_data <- latur %>%
    select(circle, org_products_count)
  
  # Perform the Kruskal-Wallis test
  result_latur <- kruskal.test(org_products_count ~ circle, data = kruskal_latur_data)
  
  # Display the results
  print(result_latur)
  
  ## Visakhapatnam
  
  # Create data frame for Visakhapatnam
  visakhapatnam <- data1 %>%
    filter(city == "Visakhapatnam")
  
  # Create data frame of circle and org_products_count
  kruskal_visakhapatnam_data <- visakhapatnam %>%
    select(circle, org_products_count)
  
  # Perform the Kruskal-Wallis test
  result_visakhapatnam <- kruskal.test(org_products_count ~ circle, data = kruskal_visakhapatnam_data)
  
  # Display the results
  print(result_visakhapatnam)
  
  
  
# Country level
  
  # Create data frame of city and org_products_count
  kruskal_india_city_data <- data1 %>%
    select(city, org_products_count)
  
  # Perform the Kruskal-Wallis test
  result_india <- kruskal.test(org_products_count ~ city, data = kruskal_india_city_data)
  
  # Display the results
  print(result_india)
  
  
  
  
  
# Table 2 (all) - Vendors - Chi-squared ----

# City level
  
  ## Hyderabad
  
    # Prepare a contingency table
    contingency_table <- hyderabad %>%
      group_by(circle, org_vendor) %>%
      tally() %>%
      pivot_wider(names_from = org_vendor, values_from = n, values_fill = list(n = 0))
    
    # Convert to matrix 
    data_matrix <- as.matrix(contingency_table[, -1])
    
    # Set row names
    rownames(data_matrix) <- contingency_table$circle
    
    # Display the matrix to check if it's correct
    print(data_matrix)
    
    # Perform Chi-squared test
    chi_result_hyderabad <- chisq.test(data_matrix)
    
    # Display the results
    print(chi_result_hyderabad)
    
  ## Latur
    
    # Prepare a contingency table
    contingency_table <- latur %>%
      group_by(circle, org_vendor) %>%
      tally() %>%
      pivot_wider(names_from = org_vendor, values_from = n, values_fill = list(n = 0))
    
    # Convert to matrix 
    data_matrix <- as.matrix(contingency_table[, -1])
    
    # Set row names
    rownames(data_matrix) <- contingency_table$circle
    
    # Display the matrix to check if it's correct
    print(data_matrix)
    
    # Perform Chi-squared test
    chi_result_latur <- chisq.test(data_matrix)
    
    # Display the results
    print(chi_result_latur)
    
  ## Visakhapatnam
    
    # Prepare a contingency table
    contingency_table <- visakhapatnam %>%
      group_by(circle, org_vendor) %>%
      tally() %>%
      pivot_wider(names_from = org_vendor, values_from = n, values_fill = list(n = 0))
    
    # Convert to matrix 
    data_matrix <- as.matrix(contingency_table[, -1])
    
    # Set row names
    rownames(data_matrix) <- contingency_table$circle
    
    # Display the matrix to check if it's correct
    print(data_matrix)
    
    # Perform Chi-squared test
    chi_result_visakhapatnam <- chisq.test(data_matrix)
    
    # Display the results
    print(chi_result_visakhapatnam)

# Country level
    
    # Prepare a contingency table
    contingency_table <- data1 %>%
      group_by(city, org_vendor) %>%
      tally() %>%
      pivot_wider(names_from = org_vendor, values_from = n, values_fill = list(n = 0))
    
    # Convert to matrix 
    data_matrix <- as.matrix(contingency_table[, -1])
    
    # Set row names
    rownames(data_matrix) <- contingency_table$city
    
    # Display the matrix to check if it's correct
    print(data_matrix)
    
    # Perform Chi-squared test
    chi_result_india <- chisq.test(data_matrix)
    
    # Display the results
    print(chi_result_india)
    


    
    
# Table 2 (food only) - Summary of vendors and products ----

# Define order of neighbourhood variable
circle_order <- c("Higher","Middle","Lower")
    
# Convert the circle column to a factor with the specified levels
data1 <- data1 %>%
  mutate(circle = factor(circle, levels = circle_order))
    
# Table 2 - neighbourhood level - food only
    
    tab2_neighbourhood_india_foodonly <- data1 %>%
      group_by(country, city, circle) %>%
      summarise(
        vendors = n(),
        count_org_vendors_foodonly = sum(org_vendor_foodonly == 1, na.rm = TRUE),
        org_vendor_perc_foodonly = round(count_org_vendors_foodonly / vendors * 100),
        median_org_foodonly_count = median(org_foodonly_count, na.rm = TRUE),
        iqr_org_foodonly_count = IQR(org_foodonly_count, na.rm = TRUE)
      ) %>%
      mutate(
        sum_orgvendors_perc = paste0(count_org_vendors_foodonly, " (", org_vendor_perc_foodonly, "%)"),
        sum_med_iqr = paste0(median_org_foodonly_count, " (", iqr_org_foodonly_count, ")")
      ) %>%
      select(country, city, circle, vendors, sum_orgvendors_perc, sum_med_iqr) %>%
      ungroup()
    
    # Print Table 2 - neighbourhood level - food only 
    addWorksheet(table_outputs_india, "Tab2 - neighbourhood - F")
    writeData(table_outputs_india, sheet = "Tab2 - neighbourhood - F", x = tab2_neighbourhood_india_foodonly)
    saveWorkbook(table_outputs_india, "Outputs/table_outputs_india.xlsx", overwrite = TRUE)
    
    
    # Table 2 - city level - food only
    
    tab2_city_india_foodonly <- data1 %>%
      group_by(country, city) %>%
      summarise(
        vendors = n(),
        count_org_vendors_foodonly = sum(org_vendor_foodonly == 1, na.rm = TRUE),
        org_vendor_perc_foodonly = round(count_org_vendors_foodonly / vendors * 100),
        median_org_foodonly_count = median(org_foodonly_count, na.rm = TRUE),
        iqr_org_foodonly_count = IQR(org_foodonly_count, na.rm = TRUE)
      ) %>%
      mutate(
        sum_orgvendors_perc = paste0(count_org_vendors_foodonly, " (", org_vendor_perc_foodonly, "%)"),
        sum_med_iqr = paste0(median_org_foodonly_count, " (", iqr_org_foodonly_count, ")")
      ) %>%
      select(country, city, vendors, sum_orgvendors_perc, sum_med_iqr) %>%
      ungroup()
    
    # Print Table 2 - city level - food only 
    addWorksheet(table_outputs_india, "Tab2 - city - F")
    writeData(table_outputs_india, sheet = "Tab2 - city - F", x = tab2_city_india_foodonly)
    saveWorkbook(table_outputs_india, "Outputs/table_outputs_india.xlsx", overwrite = TRUE)
    
    
    # Table 2 - country level - food only
    
    tab2_country_india_foodonly <- data1 %>%
      group_by(country) %>%
      summarise(
        vendors = n(),
        count_org_vendors_foodonly = sum(org_vendor_foodonly == 1, na.rm = TRUE),
        org_vendor_perc_foodonly = round(count_org_vendors_foodonly / vendors * 100),
        median_org_foodonly_count = median(org_foodonly_count, na.rm = TRUE),
        iqr_org_foodonly_count = IQR(org_foodonly_count, na.rm = TRUE)
      ) %>%
      mutate(
        sum_orgvendors_perc = paste0(count_org_vendors_foodonly, " (", org_vendor_perc_foodonly, "%)"),
        sum_med_iqr = paste0(median_org_foodonly_count, " (", iqr_org_foodonly_count, ")")
      ) %>%
      select(country, city, circle, vendors, sum_orgvendors_perc, sum_med_iqr) %>%
      ungroup()
    
    # Print Table 2 - country level - food only 
    addWorksheet(table_outputs_india, "Tab2 - country - F")
    writeData(table_outputs_india, sheet = "Tab2 - country - F", x = tab2_country_india_foodonly)
    saveWorkbook(table_outputs_india, "Outputs/table_outputs_india.xlsx", overwrite = TRUE)
    

# Table 2 (food only) Products - Kruskal-Wallis ----

# City level

  ## Hyderabad
    
    # Create data frame of circle and org_foodonly_count
    kruskal_hyderabad_data_foodonly <- hyderabad %>%
      select(circle, org_foodonly_count)
    
    # Perform the Kruskal-Wallis test
    result_hyderabad_foodonly <- kruskal.test(org_foodonly_count ~ circle, data = kruskal_hyderabad_data_foodonly)
    
    # Display the results
    print(result_hyderabad_foodonly)
    
    
  ## Latur
    
    # Create data frame of circle and org_foodonly_count
    kruskal_latur_data_foodonly <- latur %>%
      select(circle, org_foodonly_count)
    
    # Perform the Kruskal-Wallis test
    result_latur_foodonly <- kruskal.test(org_foodonly_count ~ circle, data = kruskal_latur_data_foodonly)
    
    # Display the results
    print(result_latur_foodonly)
    
  ## Visakhapatnam
    
    # Create data frame of circle and org_foodonly_count
    kruskal_visakhapatnam_data_foodonly <- visakhapatnam %>%
      select(circle, org_foodonly_count)
    
    # Perform the Kruskal-Wallis test
    result_visakhapatnam_foodonly <- kruskal.test(org_foodonly_count ~ circle, data = kruskal_visakhapatnam_data_foodonly)
    
    # Display the results
    print(result_visakhapatnam_foodonly)
    
    
    
  # Table 2 - Kruskal-Wallis - country level
    
    # Create data frame of city and org_foodonly_count
    kruskal_india_city_data_foodonly <- data1 %>%
      select(city, org_foodonly_count)
    
    # Perform the Kruskal-Wallis test
    result_india_foodonly <- kruskal.test(org_foodonly_count ~ city, data = kruskal_india_city_data_foodonly)
    
    # Display the results
    print(result_india_foodonly)
    
    
# Table 2 (food only) - Vendors - Chi-squared ----
    
# City level
    
  ## Hyderabad
    
    # Prepare a contingency table
    contingency_table <- hyderabad %>%
      group_by(circle, org_vendor_foodonly) %>%
      tally() %>%
      pivot_wider(names_from = org_vendor_foodonly, values_from = n, values_fill = list(n = 0))
    
    # Convert to matrix 
    data_matrix <- as.matrix(contingency_table[, -1])
    
    # Set row names
    rownames(data_matrix) <- contingency_table$circle
    
    # Display the matrix to check if it's correct
    print(data_matrix)
    
    # Perform Chi-squared test
    chi_result_hyderabad <- chisq.test(data_matrix)
    
    # Display the results
    print(chi_result_hyderabad)
    
    ## Latur
    
    # Prepare a contingency table
    contingency_table <- latur %>%
      group_by(circle, org_vendor_foodonly) %>%
      tally() %>%
      pivot_wider(names_from = org_vendor_foodonly, values_from = n, values_fill = list(n = 0))
    
    # Convert to matrix 
    data_matrix <- as.matrix(contingency_table[, -1])
    
    # Set row names
    rownames(data_matrix) <- contingency_table$circle
    
    # Display the matrix to check if it's correct
    print(data_matrix)
    
    # Perform Chi-squared test
    chi_result_latur <- chisq.test(data_matrix)
    
    # Display the results
    print(chi_result_latur)
    
    ## Visakhapatnam
    
    # Prepare a contingency table
    contingency_table <- visakhapatnam %>%
      group_by(circle, org_vendor_foodonly) %>%
      tally() %>%
      pivot_wider(names_from = org_vendor_foodonly, values_from = n, values_fill = list(n = 0))
    
    # Convert to matrix 
    data_matrix <- as.matrix(contingency_table[, -1])
    
    # Set row names
    rownames(data_matrix) <- contingency_table$circle
    
    # Display the matrix to check if it's correct
    print(data_matrix)
    
    # Perform Chi-squared test
    chi_result_visakhapatnam <- chisq.test(data_matrix)
    
    # Display the results
    print(chi_result_visakhapatnam)
    

# Country level
    
    # Prepare a contingency table
    contingency_table <- data1 %>%
      group_by(city, org_vendor_foodonly) %>%
      tally() %>%
      pivot_wider(names_from = org_vendor_foodonly, values_from = n, values_fill = list(n = 0))
    
    # Convert to matrix 
    data_matrix <- as.matrix(contingency_table[, -1])
    
    # Set row names
    rownames(data_matrix) <- contingency_table$city
    
    # Display the matrix to check if it's correct
    print(data_matrix)
    
    # Perform Chi-squared test
    chi_result_india <- chisq.test(data_matrix)
    
    # Display the results
    print(chi_result_india)
    
    
        

# SUMMARY OF ORGANIC PRODUCTS ----        

# Vector of organic sentinel product columns
org_foodonly <- c("tom_org", "leaf_org", "ban_org", "man_org",
                  "fj_org", "milk_org", "cof_org", "tea_org",
                  "mill_org", "chic_org", "daal_org", "wht_org",
                  "rice_org", "nut_org")

tabindia2 <- data %>%
  select(country, city, org_products) %>%
  filter(country == "India") %>%
  group_by(city) %>%
  summarise(
    tom_yes = sum(tom_org == "Yes", na.rm = TRUE),
    leaf_yes = sum(leaf_org == "Yes", na.rm = TRUE),
    ban_yes = sum(ban_org == "Yes", na.rm = TRUE),
    man_yes = sum(man_org == "Yes", na.rm = TRUE),
    fj_yes = sum(fj_org == "Yes", na.rm = TRUE),
    milk_yes = sum(milk_org == "Yes", na.rm = TRUE),
    cof_yes = sum(cof_org == "Yes", na.rm = TRUE),
    tea_yes = sum(tea_org == "Yes", na.rm = TRUE),
    mill_yes = sum(mill_org == "Yes", na.rm = TRUE),
    chic_yes = sum(chic_org == "Yes", na.rm = TRUE),
    daal_yes = sum(daal_org == "Yes", na.rm = TRUE),
    wht_yes = sum(wht_org == "Yes", na.rm = TRUE),
    rice_yes = sum(rice_org == "Yes", na.rm = TRUE),
    nut_yes = sum(nut_org == "Yes", na.rm = TRUE)
    )





# AFFORDABILITY - Table 5 ----

# Load cleaned rice prices data
rice_prices_data <- read.xlsx("rice_prices_data_clean.xlsx", sheet = "data")

# India rice prices dataset
rice_india <- rice_prices_data %>%
  filter(country == "India")

# Table 5 - city level
tab5_city_data_inr <- rice_india %>%
  group_by(city) %>%
  summarise(
    count_org_rice_vendors = sum(!is.na(rice_price_org_kg)),
    count_conv_rice_vendors = sum(!is.na(rice_price_conv_kg)),
    median_org_rice = median(rice_price_org_kg, na.rm = TRUE),
    iqr_org_rice = IQR(rice_price_org_kg, na.rm = TRUE),
    min_org_rice = min(rice_price_org_kg, na.rm = TRUE),
    max_org_rice = max(rice_price_org_kg, na.rm = TRUE),
    median_conv_rice = median(rice_price_conv_kg, na.rm = TRUE),
    iqr_conv_rice = IQR(rice_price_conv_kg, na.rm = TRUE),
    min_conv_rice = min(rice_price_conv_kg, na.rm = TRUE),
    max_conv_rice = max(rice_price_conv_kg, na.rm = TRUE),
  ) %>%
  ungroup()

# Table 5 - country level  
tab5_country_data_inr <- rice_india %>%
  group_by(country) %>%
  summarise(
    count_org_rice_vendors = sum(!is.na(rice_price_org_kg)),
    count_conv_rice_vendors = sum(!is.na(rice_price_conv_kg)),
    median_org_rice = median(rice_price_org_kg, na.rm = TRUE),
    iqr_org_rice = IQR(rice_price_org_kg, na.rm = TRUE),
    min_org_rice = min(rice_price_org_kg, na.rm = TRUE),
    max_org_rice = max(rice_price_org_kg, na.rm = TRUE),
    median_conv_rice = median(rice_price_conv_kg, na.rm = TRUE),
    iqr_conv_rice = IQR(rice_price_conv_kg, na.rm = TRUE),
    min_conv_rice = min(rice_price_conv_kg, na.rm = TRUE),
    max_conv_rice = max(rice_price_conv_kg, na.rm = TRUE),
  ) %>%
  ungroup()






# TERMINOLOGY - Table 6 ----

# Create new instance of dataset for Table 6
tab6_data_india <- data1

# OVERALL - Product-level summary

# Vector of product abbreviations
products <- c("tom", "leaf", "ban", "man", "fj", "milk", "cof", "tea", "mill", "chic", "daal", "wht", "rice", "nut")

# Define a function to summarize data for a given product and dataset
tab6_product_fn <- function(product) {
  tab6_data_india %>%
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

tab6_india <- tab6_product_summary %>%
  mutate(
    term_organic = paste0(term_organic_count, " (", round(term_organic_perc * 100), "%)"),
    term_natural = paste0(term_natural_count, " (", round(term_natural_perc * 100), "%)"),
    term_chemfree = paste0(term_chemfree_count, " (", round(term_chemfree_perc * 100), "%)"),
    term_pestfree = paste0(term_pestfree_count, " (", round(term_pestfree_perc * 100), "%)"),
    term_bioprod = paste0(term_bioprod_count, " (", round(term_bioprod_perc * 100), "%)"),
    term_bio = paste0(term_bio_count, " (", round(term_bio_perc * 100), "%)"),
    term_eco = paste0(term_eco_count, " (", round(term_eco_perc * 100), "%)"),
    term_gmo = paste0(term_gmo_count, " (", round(term_gmo_perc * 100), "%)")
  ) %>%
  select(category, product, org_count, term_organic, term_natural,
         term_chemfree, term_pestfree, term_bioprod, term_bio, term_eco, term_gmo)

# Print Table 6 
addWorksheet(table_outputs_india, "Tab6")
writeData(table_outputs_india, sheet = "Tab6", x = tab6_india)
saveWorkbook(table_outputs_india, "Outputs/table_outputs_india.xlsx", overwrite = TRUE)




    

# BRANDS - Table 7 ----

# vector with brand names variables
vector_brandnames <- data1 %>% 
  select(contains("brand_new")) %>%
  names()

tab_india <- left_join(
  # number of brands in each sentinel food
  data1 %>%
    select(country, vector_brandnames) %>%
    pivot_longer(cols = c(vector_brandnames), 
                 names_to = "food", 
                 values_to = "brand") %>%
    filter(!is.na(brand)) %>%
    group_by(city, food) %>%
    summarise(n_brands = n()),
  
  # top three brands for each sentinel food
  data1 %>%
    select(city, vector_brandnames) %>%
    pivot_longer(cols = c(vector_brandnames), 
                 names_to = "food", 
                 values_to = "brand") %>%
    filter(!is.na(brand)) %>%
    group_by(city, food) %>%
    count(brand) %>%
    arrange(desc(n), .by_group = TRUE) %>%
    slice(1:3) %>%
    mutate(rank = row_number())
) %>%
  # adjust variables
  mutate(food = paste(food, rank),
         perc = round(n/n_brands*100,2),
         brand = paste0(brand, " (", n, ", ", perc, "%)")) %>%
  select(city, food, brand) %>%
  # reshape table to match the analysis plan
  pivot_wider(names_from = city,
              values_from = brand)



# CERTIFICATION


tab_cert <- data1 %>%
  summarise(
    org_count = sum(org_products_count, na.rm = TRUE),
    cert_count = sum(cert_count, na.rm = TRUE),
    cert_perc = round(cert_count / org_count * 100)
  )

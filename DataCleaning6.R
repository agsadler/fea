# ORGANIC FOOD ENVIRONMENT ANALYSIS
# Data Cleaning Script
# Alexandra Sadler - 19.7.24



# 1. SET UP WORKSPACE----

# Load necessary libraries

library(dplyr)
library(ggforce)
library(ggmap)
library(ggplot2)
library(lubridate)
library(openxlsx)
library(osmdata)
library(readxl)
library(readr)
library(rlang)
library(sf)
library(stringr)
library(tibble)
library(tidyr)
library(tmap)

# Set working directory
setwd("C:/Users/s1985751/OneDrive - University of Edinburgh/Food Environment Assessment/Data analysis/R")





# 2. PREPARE QUALTRICS DATASET----

# Read Qualtrics data and column mapping files
qdata <- read_excel("C:/Users/s1985751/OneDrive - University of Edinburgh/Food Environment Assessment/Data analysis/R/Qualtrics_19.6.24_Clean.xlsx")

column_mapping_q <- read_excel("C:/Users/s1985751/OneDrive - University of Edinburgh/Food Environment Assessment/Data analysis/R/column_mapping_qualtrics2.xlsx")

# Initialise Qualtrics data
qdata1 <- qdata

# Check no NA column names in column mapping
column_mapping_q <- column_mapping_q %>% filter(!is.na(new_name))

# Rename Qualtrics columns
colnames(qdata1) <- column_mapping_q$new_name

# Delete first two data rows - headers
qdata1 <- qdata1[-c(1,2),]

# Function to log columns with conversion issues
log_conversion_issue <- function(column_name) {
  cat("Warning: NAs introduced by coercion in column", column_name, "\n")
}

# Set column data type
for (i in seq_along(column_mapping_q$new_name)) {
  column_q <- column_mapping_q$new_name[i]
  data_type <- column_mapping_q$data_type[i]
  if (!is.na(column_q) && column_q %in% colnames(qdata1)) {
    if (data_type == "numeric") {
      # Try to convert to numeric and log if NAs are introduced
      new_col <- suppressWarnings(as.numeric(qdata1[[column_q]]))
      if (any(is.na(new_col) & !is.na(qdata1[[column_q]]))) {
        log_conversion_issue(column_q)
      }
      qdata1[[column_q]] <- new_col
    } else if (data_type == "character" || data_type == "text") {
      qdata1[[column_q]] <- as.character(qdata1[[column_q]])
    } else if (data_type == "logical") {
      qdata1[[column_q]] <- as.logical(qdata1[[column_q]])
    }
  }
}

# Delete irrelevant columns
columns_to_delete <- c("startdatetime","enddatetime","language","status","ipaddress","progress","duration","finished","recorddatetime","responseid",
                       "recipientlastname","recipientfirstname","recipientemail","externalref","latitude_system","longitude_system","distribution",
                       "deviceid","qlanguage","newfield")

qdata1 <- qdata1[, !names(qdata1) %in% columns_to_delete]

# Set unique ID per row/response
qdata1 <- qdata1 %>%
  mutate(id = paste0("q_",row_number())) %>%
  select(id, everything())

# Fix date format
qdata1 <- qdata1 %>%
  mutate(date_formatted = as.Date(date_formatted, format = "%d-%m-%Y"))

# Clean up responses with commas
qdata1$select_products <- gsub("Green leafy vegetables \\(spinach, otherwise closest available alternative\\)", "Leafy", qdata1$select_products)
qdata1$select_products <- gsub("Fruit juice \\(orange, otherwise closest available alternative\\)", "FJ", qdata1$select_products)
qdata1$select_products <- gsub("Daal / lentils \\(moorg, otherwise closest available alternative\\)", "Daal", qdata1$select_products)
qdata1$select_products <- gsub("Wheat flour \\(white, otherwise closest available alternative\\)", "Wheat", qdata1$select_products)

promo_type_rename <- c("tom_org_promo_type","leaf_org_promo_type","ban_org_promo_type","man_org_promo_type",
                       "fj_org_promo_type","milk_org_promo_type","cof_org_promo_type","tea_org_promo_type",
                       "mill_org_promo_type","chic_org_promo_type","daal_org_promo_type","wht_org_promo_type","rice_org_promo_type","nut_org_promo_type",
                       "app_org_promo_type","pot_org_promo_type","chkn_org_promo_type","egg_org_promo_type")

for (product_promo in promo_type_rename) {
  qdata1[[product_promo]] <- gsub("Large promotional displays \\(e.g. hanging display, big stand with products on it\\)", "Large displays", qdata1[[product_promo]])
}

# Clean up certification options with Portuguese keyboard

certif_rename <- c("tom_org_certif","leaf_org_certif","ban_org_certif","man_org_certif",
                   "fj_org_certif","milk_org_certif","cof_org_certif","tea_org_certif",
                   "mill_org_certif","chic_org_certif","daal_org_certif","wht_org_certif","rice_org_certif","nut_org_certif",
                   "app_org_certif","pot_org_certif","chkn_org_certif","egg_org_certif")

for (certif in certif_rename) {
  qdata1[[certif]] <- gsub("Produto OrgÃ¢nico Brasil", "Produto Organico Brasil", qdata1[[certif]])
}

certif_other_rename <- c("tom_org_certif_other","leaf_org_certif_other","ban_org_certif_other","man_org_certif_other",
                         "fj_org_certif_other","milk_org_certif_other","cof_org_certif_other","tea_org_certif_other",
                         "mill_org_certif_other","chic_org_certif_other","daal_org_certif_other","wht_org_certif_other","rice_org_certif_other","nut_org_certif_other",
                         "app_org_certif_other","pot_org_certif_other","chkn_org_certif_other","egg_org_certif_other")

for (certif_other in certif_other_rename) {
  qdata1[[certif_other]] <- gsub("Produto OrgÃ¢nico Brasil", "Produto Organico Brasil", qdata1[[certif_other]])
  qdata1[[certif_other]] <- gsub("Produtos OrgÃ¢nicos Brasil", "Produto Organico Brasil", qdata1[[certif_other]])
  qdata1[[certif_other]] <- gsub("Produto OrgÃ¢nicos Brasil", "Produto Organico Brasil", qdata1[[certif_other]])
}


# Convert single to multiple columns

  # Columns to split
  concat_cols <- c("vendor_days","select_products",
                   "tom_org_terms","tom_org_certif","tom_org_pack_themes","tom_org_promo_type","tom_org_promo_themes",
                   "man_org_terms","man_org_certif","man_org_pack_themes","man_org_promo_type","man_org_promo_themes",
                   "leaf_org_terms","leaf_org_certif","leaf_org_pack_themes","leaf_org_promo_type","leaf_org_promo_themes",
                   "ban_org_terms","ban_org_certif","ban_org_pack_themes","ban_org_promo_type","ban_org_promo_themes",
                   "fj_org_terms","fj_org_certif","fj_org_pack_themes","fj_org_promo_type","fj_org_promo_themes",
                   "milk_org_terms","milk_org_certif","milk_org_pack_themes","milk_org_promo_type","milk_org_promo_themes",
                   "cof_org_terms","cof_org_certif","cof_org_pack_themes","cof_org_promo_type","cof_org_promo_themes",
                   "tea_org_terms","tea_org_certif","tea_org_pack_themes","tea_org_promo_type","tea_org_promo_themes",
                   "mill_org_terms","mill_org_certif","mill_org_pack_themes","mill_org_promo_type","mill_org_promo_themes",
                   "chic_org_terms","chic_org_certif","chic_org_pack_themes","chic_org_promo_type","chic_org_promo_themes",
                   "daal_org_terms","daal_org_certif","daal_org_pack_themes","daal_org_promo_type","daal_org_promo_themes",
                   "wht_org_terms","wht_org_certif","wht_org_pack_themes","wht_org_promo_type","wht_org_promo_themes",
                   "rice_org_terms","rice_org_certif","rice_org_pack_themes","rice_org_promo_type","rice_org_promo_themes",
                   "nut_org_terms","nut_org_certif","nut_org_pack_themes","nut_org_promo_type","nut_org_promo_themes",
                   "app_org_terms","app_org_certif","app_org_pack_themes","app_org_promo_type","app_org_promo_themes",
                   "pot_org_terms","pot_org_certif","pot_org_pack_themes","pot_org_promo_type","pot_org_promo_themes",
                   "chkn_org_terms","chkn_org_certif","chkn_org_pack_themes","chkn_org_promo_type","chkn_org_promo_themes",
                   "egg_org_terms","egg_org_certif","egg_org_pack_themes","egg_org_promo_type","egg_org_promo_themes")
    
  # Loop through each variable
  for (col in concat_cols) {
      
    # Create separate columns
    temp_data <- qdata1 %>% 
      select(id, col) %>%
      separate(col = col, into = paste0(col, "_", 1:20), sep = ",", fill = "right")
      
    # Convert to long format
    temp_long <- temp_data %>%
      pivot_longer(cols = starts_with(paste0(col,"_")),
                   values_to = "value",
                   values_drop_na = TRUE) %>%
      select(-name)
      
    # Create dummy variables
    temp_dummy <- temp_long %>%
      mutate(dummy=1) %>%
      pivot_wider(names_from = value, 
                  values_from = dummy,
                  values_fill = list(dummy=0),
                  names_prefix = paste0(col,"_")) %>%
      mutate(across(-id, as.numeric))
      
    # Join with original dataset
    qdata1 <- left_join(qdata1, temp_dummy, by = "id", suffix = c("", ".temp"))
      
    # Find the position of the original column
    original_col_position <- which(names(qdata1) == col)
      
    # Get the new columns added from temp_dummy, excluding 'id'
    new_cols <- setdiff(names(temp_dummy), "id")
      
    # Reorder the new columns next to the original column
    qdata1 <- qdata1 %>%
      select(all_of(names(qdata1)[1:original_col_position]),
             all_of(new_cols),
             all_of(names(qdata1)[(original_col_position + 1):length(names(qdata1))]))
  }

  qdata1 <- qdata1 %>% select(-all_of(concat_cols))

# Ensure all column names are valid R identifiers
#colnames(qdata1) <- make.names(colnames(qdata1), unique = TRUE)
  




# 3. PREPARE REDCAP DATASET----
  
# Read RedCap data and column mapping files
rdata <- read_excel("C:/Users/s1985751/OneDrive - University of Edinburgh/Food Environment Assessment/Data analysis/R/RedCap_Labels_13.6.24.xlsx")

column_mapping_r <- read_excel("C:/Users/s1985751/OneDrive - University of Edinburgh/Food Environment Assessment/Data analysis/R/column_mapping_redcap2.xlsx")
  
# Initialise RedCap data
rdata1 <- rdata
  
# Check no NA column names in column mapping
column_mapping_r <- column_mapping_r %>% filter(!is.na(new_name))
  
# Rename RedCap columns
colnames(rdata1) <- column_mapping_r$new_name

# Ensure all column names are valid R identifiers
#colnames(rdata1) <- make.names(colnames(rdata1), unique = TRUE)


# Check initial data types for qdata and rdata
#initial_qdata_types <- sapply(qdata, class)
#initial_rdata_types <- sapply(rdata, class)

# Combine the initial data types
#initial_data_types <- data.frame(
#  Variable = names(initial_qdata_types),
#  Initial_QData_Type = initial_qdata_types,
#  Initial_RData_Type = initial_rdata_types[names(initial_qdata_types)]
#)


# Identify columns that contain 'Checked' or 'Unchecked'
contains_checked_unchecked <- function(column) {
  any(column %in% c("Checked", "Unchecked"))
}

# Convert 'Checked' and 'Unchecked' to binary (1/0) variables, excluding non-text columns like dates
rdata1 <- rdata1 %>%
  mutate(across(where(~ contains_checked_unchecked(.)), ~ ifelse(. == "Checked", 1, ifelse(. == "Unchecked", 0, .))))

# Set column data type
for (i in seq_along(column_mapping_r$new_name)) {
  column_r <- column_mapping_r$new_name[i]
  data_type <- column_mapping_r$data_type[i]
  if (!is.na(column_r) && column_r %in% colnames(rdata1)) {
    if (data_type == "numeric") {
      # Try to convert to numeric and log if NAs are introduced
      new_col <- suppressWarnings(as.numeric(rdata1[[column_r]]))
      if (any(is.na(new_col) & !is.na(rdata1[[column_r]]))) {
        log_conversion_issue(column_r)
      }
      rdata1[[column_r]] <- new_col
    } else if (data_type == "character" || data_type == "text") {
      rdata1[[column_r]] <- as.character(rdata1[[column_r]])
    } else if (data_type == "logical") {
      rdata1[[column_r]] <- as.logical(rdata1[[column_r]])
    }
  }
}


# Set unique ID per row/response
rdata1 <- rdata1 %>%
  mutate(id = paste0("r_", row_number())) %>%
  select(id, everything())

# Delete irrelevant columns
columns_to_delete_redcap <- c("id_redcap","responseid","enddatetime")

rdata1 <- rdata1[, !names(rdata1) %in% columns_to_delete_redcap]

# Fix date format
rdata1 <- rdata1 %>%
  mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d"))

# Rename Produto Organico column
#colnames(rdata1) <- gsub("Produto OrgÃ¢nico Brasil", "Produto Organico Brasil", colnames(rdata1))

# Get data types for each column in qdata1
#qdata1_types <- sapply(qdata1, class)

# Get data types for each column in rdata1
#rdata1_types <- sapply(rdata1, class)

# Combine the results into a data frame
#data_types <- data.frame(
#  Variable = names(qdata1_types),
#  QData1_Type = qdata1_types,
#  RData1_Type = rdata1_types[names(qdata1_types)]
#)





# 4. MERGE DATASETS----

combined_data <- bind_rows(rdata1, qdata1)





# 5. REMOVE INVALID RESPONSES----

# Delete where blank for lat, lon, vendor name, and vendor type
blank_responses <- rowSums(is.na(combined_data[,c("latitude","longitude","vendor_name","vendor_type")])) == 4
print(table(blank_responses))
blank_rows <- combined_data[blank_responses, ]
print(blank_rows)
combined_data <- combined_data[!blank_responses,]

# Delete where entire row is blank
combined_data <- combined_data %>%
  filter(rowSums(is.na(.)) != ncol(.))

# Delete where vendor name is Testing
testing_responses <- combined_data$vendor_name == "Testing"
print(table(testing_responses))
testing_rows <- combined_data[testing_responses, ]
print(testing_rows)
combined_data <- combined_data[!testing_responses,]


# Check incomplete records - only relevant for RedCap data
print(table(combined_data$complete))
incomplete <- combined_data %>%
  filter(complete == "Incomplete")

# Delete specific unique IDs based on manual checks, including above data frame of incomplete records
other_delete <- c("q_423", "r_93", "r_94", "r_155", "r_156", "r_160", "r_170", "r_180", "r_182", "r_201", "r_211", "r_226", "r_227", "r_249") # add in other IDs as necessary
combined_data <- combined_data %>%
  filter(!id %in% other_delete)

# Delete columns for apples, potatoes, chicken, and eggs, as data only gathered for Brazil and London
  # Create a vector of prefixes for products to delete
  products_delete <- c("app_", "pot_", "chkn_", "egg_")
  
  # Create a regular expression to match columns starting with these prefixes
  pattern <- paste0("^(", paste(products_delete, collapse = "|"), ")")
  
  # Use select to keep all columns that do not match the pattern
  combined_data <- combined_data %>% select(-matches(pattern))

# Print records for which data collection could not proceed and then delete them
  
  permission_denied <- combined_data %>%
    filter(proceed == "No, vendor refuses to allow data collection to proceed")

  vendor_closed <- combined_data %>%
    filter(proceed == "No, vendor is closed")
  
  combined_data <- combined_data %>%
    filter(!proceed %in% c("No, vendor refuses to allow data collection to proceed",
                           "No, vendor is closed"))

# Delete records where the vendor is open zero days
  
  vendor_days_vector <- c("vendor_days_Monday","vendor_days_Tuesday","vendor_days_Wednesday","vendor_days_Thursday",
                         "vendor_days_Friday","vendor_days_Saturday","vendor_days_Sunday")
  
  combined_data <- combined_data %>%
    mutate(vendor_days_count = rowSums(select(., all_of(vendor_days_vector)) == 1, na.rm = TRUE))
  
  vendor_zero_days <- combined_data %>%
    filter(vendor_days_count == 0) # Confirmed no records with zero days open
  
# Delete records where the vendor sells zero sentinel products
  
  zero_products_vector <- c("tom_sell","leaf_sell","ban_sell","man_sell","fj_sell",
                            "milk_sell","cof_sell","tea_sell","mill_sell","daal_sell",
                            "wht_sell","rice_sell","nut_sell","chic_sell")

  combined_data <- combined_data %>%
    mutate(zero_products_count = rowSums(select(., all_of(zero_products_vector)) == "Yes", na.rm = TRUE))
  
  vendor_zero_products <- combined_data %>%
    filter(zero_products_count == 0) # 17 records with zero days open
  
  combined_data <- combined_data %>%
    filter(zero_products_count != 0)

  
  
  
  
  
# 6. CLEAN UP OTHER RESPONSES----

# Clean up other vendor type responses

table(combined_data$vendor_type_other)

vendor_type_reclassify_small <- c("Bakery", "Bombonieri", "Butcher", "Butcher shop", "Casa de produtos naturais",
                           "Casa do Norte","Coffe shop", "Franquia de produtos naturais", "Greengrocer",
                           "Greengrocer franchise","Mercearia/bombonieri","Milk dairy","Quitanda", "Small vendor - eggs and chicken")

vendor_type_reclassify_mobile <- c("Raitu bajaru","Raitu Bazaru","Raitu Bazuru","Rythu Bazaar")

combined_data$vendor_type <- ifelse(combined_data$vendor_type == "Other (specify)" & 
                                      combined_data$vendor_type_other %in% vendor_type_reclassify_small, 
                             "Stationary small local vendor", 
                             combined_data$vendor_type)

combined_data$vendor_type <- ifelse(combined_data$vendor_type == "Other (specify)" & 
                                      combined_data$vendor_type_other %in% vendor_type_reclassify_mobile, 
                                    "Mobile vendor",
                                    combined_data$vendor_type)



# Clean up other city responses

table(combined_data$city_other) # All Birmingham responses were listed as Cuiaba due to limitations with survey updating

combined_data$city <- gsub("Cuiaba", "Birmingham", combined_data$city) # Relabel Cuiaba to Birmingham

table(combined_data$city) # Confirmed all are relabelled

  


# Clean up other proceed responses

table(combined_data$proceed_other) # Initially was one other response - a testing row; doesn't exist anymore

  #proceed_other_filter <- combined_data %>%
  #  filter(proceed_other == "Testing") # Verified it was a test row
  
  #combined_data <- combined_data %>%
  #  filter(proceed_other != "Testing") # Delete row
  
  #table(combined_data$proceed_other) # Validate that row has been deleted


  

# Clean up brand columns

  # Create a vector of the short names for products
  products_brand <- c("tom","leaf","ban","man","fj","milk","cof","tea","mill","chic",
                "daal","wht","rice","nut")
  
  # Run a loop through each product to create product specific columns
  for (p_b in products_brand) {
    brand_col <- paste0(p_b, "_org_brand")
    other_col <- paste0(p_b, "_org_brand_other")
    new_col <- paste0(p_b, "_org_brand_new")
    
    # Run loop through each product to create new column with combined brand names
    combined_data[[new_col]] <- ifelse(
      combined_data[[brand_col]] == "Other (specify)",
      combined_data[[other_col]],
      combined_data[[brand_col]]
    )
    
    combined_data <- combined_data %>%
      select(all_of(c(names(combined_data)[1:which(names(combined_data) == other_col)])), 
             all_of(new_col), 
             everything())
    }

  # Create a vector of all new brand columns
  
  brand_new_cols <- c("tom_org_brand_new","leaf_org_brand_new","ban_org_brand_new","man_org_brand_new",
                   "fj_org_brand_new","milk_org_brand_new","cof_org_brand_new","tea_org_brand_new",
                   "mill_org_brand_new","chic_org_brand_new","daal_org_brand_new","wht_org_brand_new",
                   "rice_org_brand_new","nut_org_brand_new")

  # Clean up Brazil responses where certifications were listed as brands
  
    # Function to replace variations of Ecocerti/Brasil orgânico with NA
    replace_ecocerti_with_na <- function(column) {
      column <- case_when(
        column %in% c("Ecocerti/Brasil orgânico", "Ecocerti/Orgânico Brasil", "Ecocerti/orgânico Brasil") ~ NA_character_,
        TRUE ~ column
      )
      return(column)
    }
    
    # Apply the replacement function to relevant columns
    combined_data <- combined_data %>%
      mutate(across(all_of(brand_new_cols), replace_ecocerti_with_na))
    
    # Function to replace IBD Brasil with NA
    
    replace_ibd_with_na <- function(column) {
      column <- ifelse(column == "IBD Brasil", NA, column)
      return(column)
    }
      
      # Apply the replacement function to relevant columns
      combined_data <- combined_data %>%
        mutate(across(all_of(brand_new_cols), replace_ibd_with_na))
      
    
  # Print a list of all unique brand names across all products
  
    # Check which columns in brand_new_cols exist in combined_data
    existing_cols <- brand_new_cols[brand_new_cols %in% names(combined_data)]
    
    # Combine all responses into a long format for existing columns
    all_responses <- combined_data %>%
      select(all_of(existing_cols)) %>%
      pivot_longer(cols = everything(), names_to = "column", values_to = "response") %>%
      filter(!is.na(response)) %>%
      distinct(response) %>%
      arrange(response)
    
    # Print all unique responses
    print(all_responses$response)
  
  # Rename duplicate or misspelled brand names, including selecting first response where multiple are provided  

    # List of gsub replacements to be made
    replacements <- list(
      "Rio Bonito; Bio vida." = "Rio Bonito", # select first brand
      "Rio Bonito; Bio Vida" = "Rio Bonito", # select first brand
      "Korin, Tio Joao, Camil" = "Korin", # select first brand
      "Rio de Una; Bio Vida" = "Rio de Una", # select first brand
      "MÃ£e terra, korin, Tio JoÃ£o, kamil." = "Mae Terra", # select first brand
      "3 coraÃ§Ãµes" = "3 coracoes",
      "Aashirvaada" = "Aashirvaad",
      "Aashirvad" = "Aashirvaad",
      "aashirvaad" = "Aashirvaad",
      "B natural" = "B Natural",
      "Balaji grand bazaar" = "Balaji Grand Bazaar",
      "bio vida" = "Bio Vida",
      "biovida" = "Bio Vida",
      "Cafe direct" = "Cafe Direct",
      "Café Direct" = "Cafe Direct",
      "Campo verde" = "Chas Campo Verde",
      "ChÃ¡s Campo Verde" = "Chas Campo Verde",
      "Clipper (tea)" = "Clipper",
      "Dalgety Strong Pure Tumeric" = "Dalgety",
      "Doveâ€™s Farm" = "Dove's Farm",
      "Dove's farm" = "Dove's Farm",
      "Dover" = "Dove's Farm",
      "Doves farm" = "Dove's Farm",
      "Doves Farm" = "Dove's Farm",
      "Doves" = "Dove's Farm",
      "Fazenda da Toca OrgÃ¢nicos" = "Fazenda da Toca Organicos",
      "Gâ€™s Naturally Fresh" = "G's",
      "Gâ€™s" = "G's",
      "Gailâ€™s" = "Gails",
      "Grahams (milk)" = "Grahams",
      "Graham's" = "Grahams",
      "Haldiram a" = "Haldirams",
      "Haldiram" = "Haldirams",
      "Infinity food" = "Infinity Foods",
      "Infinity foods" = "Infinity Foods",
      "KÃ³rin" = "Korin",
      "Lipton green tea" = "Lipton",
      "Lipton - Tulsi Natura" = "Lipton",
      "Mae terra" = "Mae Terra",
      "Manna health mix" = "Manna",
      "Mitra organics" = "Mitra Organics",
      "Mokhtar Tea London" = "Mokhtar Tea",
      "Mr Organic" = "Mr. Organic",
      "MÃ£e terra" = "Mae Terra",
      "Mãe terra" = "Mae Terra",
      "NATURAL MILK" = "Natural",
      "NATURAL" = "Natural",
      "natural" = "Natural",
      "Natural milk" = "Natural",
      "Native produtos da natureza; Orfeu cafÃ©s especiais" = "Native",
      "Native;" = "Native",
      "Native; Poder da Terra" = "Native",
      "NaÃ§Ã£o Verde" = "Nacao Verde",
      "Organic tattva" = "Organic Tattva",
      "OrgÃ¢nicos Solo Vivo" = "Organicos Solo Vivo",
      "Pip Organic" = "Pip",
      "Pronature" = "Pro Nature",
      "Pure o natural" = "Pure O Natural",
      "Red label" = "Red Label - Natural Care",
      "Red label natural care" = "Red Label - Natural Care",
      "Rio de una" = "Rio de Una",
      "Rio de Una OrgÃ¢nicos" = "Rio de Una",
      "Sainsburyâ€™s" = "Sainsbury's",
      "Tetly" = "Tetley",
      "TimbaÃºba" = "Timbauba",
      "Tio JoÃ£o" = "Tio Joao",
      "Tio joÃ£o" = "Tio Joao",
      "Yeo Valley Organic" = "Yeo Valley",
      "Yep Valley Organic" = "Yeo Valley",
      "Yogi tea" = "Yogi Tea",
      "Yeo Valley Organic" = "Yeo Valley"
    )
    
    # Sort replacements by length of pattern in descending order
    sorted_replacements <- replacements[order(-nchar(names(replacements)))]
    
    # Loop over each brand column
    for (brand_name in brand_new_cols) {
      if (brand_name %in% colnames(combined_data)) {
        for (pattern in names(replacements)) {
          combined_data[[brand_name]] <- gsub(pattern, replacements[[pattern]], combined_data[[brand_name]])
        }
      }
    }
    
  # Print another list of all brand names to confirm change
    
    # Check which columns in brand_new_cols exist in combined_data
    existing_cols2 <- brand_new_cols[brand_new_cols %in% names(combined_data)]
    
    # Combine all responses into a long format for existing columns
    all_responses2 <- combined_data %>%
      select(all_of(existing_cols2)) %>%
      pivot_longer(cols = everything(), names_to = "column", values_to = "response") %>%
      filter(!is.na(response)) %>%
      distinct(response) %>%
      arrange(response)
    
  # NOTE: There are some errors, e.g. for Haldirams and Grahams and Clipper and Pure O Natural. Check in results table.  

    
    
  
# Clean up organic certification columns
    
  # Clean up names of other organic certifications
    
    # Print list of other certifications
    
      # Create vector of other certifications
      certif_other_cols <- c("tom_org_certif_other","leaf_org_certif_other","ban_org_certif_other","man_org_certif_other",
                             "fj_org_certif_other","milk_org_certif_other","cof_org_certif_other","tea_org_certif_other",
                             "mill_org_certif_other","chic_org_certif_other","daal_org_certif_other","wht_org_certif_other",
                             "rice_org_certif_other","nut_org_certif_other")
      
      # Check which columns in certif_other_cols exist in combined_data
      existing_certif_cols <- certif_other_cols[certif_other_cols %in% names(combined_data)]
    
      # Combine all responses into a long format for existing columns
      all_responses_cert <- combined_data %>%
        select(all_of(existing_certif_cols)) %>%
        pivot_longer(cols = everything(), names_to = "column", values_to = "response") %>%
        filter(!is.na(response)) %>%
        distinct(response) %>%
        arrange(response)
      
      # Print all unique responses
      print(all_responses_cert$response) 
    
      
    # Rename duplicate or misspelled certifications
      
      # List of gsub replacements to be made
      replacements_cert <- list(
        "B-corp" = "B-Corp",
        "buy social" = "Buy Social",
        "certified b" = "Certified B",
        "Eco Ceri" = "Ecocert",
        "Eco Cert" = "Ecocert",
        "Eco cert" = "Ecocert",
        "eurecicle" = "Eureciclo",
        "EuReciclo" = "Eureciclo",
        "Eu reciclo" = "Eureciclo",
        "Fair for life" = "Fair for Life",
        "Rainforest alliance" = "Rainforest Alliance",
        "N SAI" = "NSAI",
        "N sai" = "NSAI",
        "Red tractor" = "Red Tractor",
        "Sopa" = "SOPA"
      )
      
      # Sort replacements by length of pattern in descending order
      sorted_replacements <- replacements_cert[order(-nchar(names(replacements_cert)))]
      
      # Loop over each certification column
      for (certification in certif_other_cols) {
        if (certification %in% colnames(combined_data)) {
          for (pattern in names(replacements_cert)) {
            combined_data[[certification]] <- gsub(pattern, replacements_cert[[pattern]], combined_data[[certification]])
          }
        }
      }
      
      
    # Print another list of all certification names to confirm change
    
      # Check which columns in existing_certif_cols exist in combined_data
      existing_certif_cols2 <- certif_other_cols[certif_other_cols %in% names(combined_data)]
        
      # Combine all responses into a long format for existing columns
      all_responses_cert2 <- combined_data %>%
        select(all_of(existing_certif_cols2)) %>%
        pivot_longer(cols = everything(), names_to = "column", values_to = "response") %>%
        filter(!is.na(response)) %>%
        distinct(response) %>%
        arrange(response)
      
      # Print all unique responses
      print(all_responses_cert2$response)
    
      
  # Create separate columns for each other response

    # Create a vector of the short names for products
    products_certif <- c("tom","leaf","ban","man","fj","milk","cof","tea","mill","chic",
                         "daal","wht","rice","nut")
    
    # Run a loop through each product to create product-specific columns for other responses
    for (p_c in products_certif) {
      other_col <- paste0(p_c, "_org_certif_other")
      new_col1 <- paste0(p_c, "_org_certif_other_1")
      new_col2 <- paste0(p_c, "_org_certif_other_2")
      new_col3 <- paste0(p_c, "_org_certif_other_3")
      new_col4 <- paste0(p_c, "_org_certif_other_4")
      new_col5 <- paste0(p_c, "_org_certif_other_5")
      
      # Check if the column exists in the data frame and print the result
      if (other_col %in% colnames(combined_data)) {
        print(paste("Column", other_col, "exists in the data frame. Proceeding with splitting."))
        
        # Replace ' e ', ';', and ',' with a common delimiter ('|') for consistent splitting
        combined_data[[other_col]] <- gsub(" e |;|,", "|", combined_data[[other_col]])
        
        # Separate the responses into new columns
        combined_data <- combined_data %>%
          separate(other_col, into = c(new_col1, new_col2, new_col3, new_col4, new_col5), sep = "\\|", fill = "right", extra = "drop")
      } else {
        warning(paste("Column", other_col, "does not exist in the data frame. Skipping this column."))
      }
    }
    
    
    
  
  
# Clean up other packaging themes
    
  # Print all other packaging themes
    
    # Create vector of other packaging themes
    pack_themes_other_cols <- c("tom_org_pack_themes_other","leaf_org_pack_themes_other","ban_org_pack_themes_other","man_org_pack_themes_other",
                           "fj_org_pack_themes_other","milk_org_pack_themes_other","cof_org_pack_themes_other","tea_org_pack_themes_other",
                           "mill_org_pack_themes_other","chic_org_pack_themes_other","daal_org_pack_themes_other","wht_org_pack_themes_other",
                           "rice_org_pack_themes_other","nut_org_pack_themes_other")
    
    # Check which columns in existing_pack_cols exist in combined_data
    existing_pack_cols <- pack_themes_other_cols[pack_themes_other_cols %in% names(combined_data)]
    
    # Combine all responses into a long format for existing columns
    all_responses_pack <- combined_data %>%
      select(all_of(existing_pack_cols)) %>%
      pivot_longer(cols = everything(), names_to = "column", values_to = "response") %>%
      filter(!is.na(response)) %>%
      distinct(response) %>%
      arrange(response)
    
    # Print all unique responses
    print(all_responses_pack$response)
    
  
  # Rename duplicate or misspelled certifications
    
    # List of gsub replacements to be made
    replacements_pack_themes <- list(
      "Animal welfare (free range)" = "Animal welfare",
      "Hand harvested" = "Hand harvested/picked",
      "Hand-picked" = "Hand harvested/picked",
      "International standerds" = "International standards",
      "Supporting The Prince of Walesâ€™s Charities" = "Prince of Wales Charities",
      "Supporting prince of Wales charitable fund" = "Prince of Wales Charities",
      "Supporting the Prince of Walesâ€™s Charities" = "Prince of Wales Charities"
    )
    
    # Sort replacements by length of pattern in descending order
    sorted_replacements_pack_themes <- replacements_pack_themes[order(-nchar(names(replacements_pack_themes)))]
    
    # Apply replacements to combined_data
    for (pack_themes in pack_themes_other_cols) {
      if (pack_themes %in% colnames(combined_data)) {
        for (pattern_pack_themes in names(replacements_pack_themes)) {
          combined_data[[pack_themes]] <- gsub(pattern_pack_themes, replacements_pack_themes[[pattern_pack_themes]], combined_data[[pack_themes]])
        }
      }
    }
    
    
  # Print another list of all other pack themes to confirm change

    # Check which columns in existing_pack_cols exist in combined_data
    existing_pack_cols2 <- pack_themes_other_cols[pack_themes_other_cols %in% names(combined_data)]
    
    # Combine all responses into a long format for existing columns
    all_responses_pack2 <- combined_data %>%
      select(all_of(existing_pack_cols2)) %>%
      pivot_longer(cols = everything(), names_to = "column", values_to = "response") %>%
      filter(!is.na(response)) %>%
      distinct(response) %>%
      arrange(response)
    
    # Print all unique responses
    print(all_responses_pack2$response)
    
    # NOTE: Loop isn't working perfectly as animal welfare free range won't convert to animal, etc.
    
    
    
# Clean up other promo types
  
  # Create a vector of the short names for products
  products_promotype <- c("tom","leaf","ban","man","fj","milk","cof","tea","mill","chic",
                     "daal","wht","rice","nut")
  
  for (p_pr in products_promotype) {
    promo_col <- paste0(p_pr, "_org_promo_type_other")
    print(paste("Responses for", promo_col))
    print(table(qdata1[[promo_col]]))
  }
    # Only three entries; the rice promo shows a Testing row but can't find it; will keep all as is

  
  
  
  
# Clean up other promo themes
  
  promothemesother <- c("tom","leaf","ban","man","fj","milk","cof","tea","mill","chic",
                          "daal","wht","rice","nut")
  
  for (promothemes in promothemesother) {
    promothemes_col <- paste0(promothemes, "_org_promo_themes_other")
    print(paste("Responses for", promothemes_col))
    print(table(qdata1[[promothemes_col]]))
  }
  
  # Only a couple of relevant entries; will keep as is


  

  

# 7. CLEANING GPS DATA----
  
# Function to convert DMS string to decimal degrees
  convert_dms_string_to_decimal <- function(dms_string) {
    # Use regex to extract components
    matches <- regmatches(dms_string, regexec("([NSEW])\\s(\\d+)°(\\d+)'(\\d+\\.\\d+)", dms_string))
    
    if (length(matches[[1]]) == 0) return(as.numeric(dms_string))  # Return the input if it doesn't match the DMS format
    
    direction <- matches[[1]][2]
    degrees <- as.numeric(matches[[1]][3])
    minutes <- as.numeric(matches[[1]][4])
    seconds <- as.numeric(matches[[1]][5])
    
    # Convert DMS to decimal
    decimal <- degrees + minutes / 60 + seconds / 3600
    if (direction == 'S' || direction == 'W') {
      decimal <- -decimal
    }
    return(decimal)
  }
  
  
# Apply the conversion to the latitude and longitude columns
  combined_data <- combined_data %>%
    mutate(
      latitude = sapply(latitude, function(x) {
        result <- tryCatch(convert_dms_string_to_decimal(x), warning = function(w) NA, error = function(e) NA)
        return(result)
      }),
      longitude = sapply(longitude, function(x) {
        result <- tryCatch(convert_dms_string_to_decimal(x), warning = function(w) NA, error = function(e) NA)
        return(result)
      })
    )

# Check entries without latitude and longitude data
  missing_gps <- combined_data %>%
    filter(is.na(latitude) | is.na(longitude)) # Manually cleaned up mistyped entries in input files; confirmed zero entries remain
  

# Create a dataset with only unique ids, lat, lon, city, date and vendor name
  map_data <- combined_data %>%
    dplyr::select(id, date_formatted, enumerator, city, latitude, longitude, vendor_name)
  write.xlsx(map_data, "map_data.xlsx")
  

  

  
# 8. MAP POINTS TO ASSIGN CIRCLES AND CHECK FOR VALUES OUTSIDE CIRCLES----
  
  # Load your datasets
  circlepoints <- read_excel("C:/Users/s1985751/OneDrive - University of Edinburgh/Food Environment Assessment/Data analysis/R/circle_coordinates_combined.xlsx")
  vendors <- read_excel("C:/Users/s1985751/OneDrive - University of Edinburgh/Food Environment Assessment/Data analysis/R/map_data.xlsx")
  
  # Confirm no missing values for latitude and longitude
  vendors <- vendors %>%
    filter(rowSums(is.na(.)) != ncol(.))
  
  
  # Convert datasets to spatial objects
  circlepoints_sf <- st_as_sf(circlepoints, coords = c("lon", "lat"), crs = 4326)
  vendors_sf <- st_as_sf(vendors, coords = c("longitude", "latitude"), crs = 4326)
  
  # Create circles with 0.5km radius
  circles_sf <- st_buffer(circlepoints_sf, dist = 0.5 * 1000) # 0.5 km to meters
  
  
  # Ensure CRS is consistent
  circlepoints_sf <- st_transform(circlepoints_sf, crs = 4326)
  vendors_sf <- st_transform(vendors_sf, crs = 4326)
  circles_sf <- st_transform(circles_sf, crs = 4326)
  
  # Set tmap mode to view
  #tmap_mode("view")
  
  # Use tmap to create a map
  tm_shape(circles_sf) + 
    tm_polygons(col = "blue", alpha = 0.5) + 
    tm_shape(vendors_sf) + 
    tm_dots(col = "red", size = 0.1)
  
  
  # Join points to circles if within a circle
  vendors_within_circles <- st_join(vendors_sf, circles_sf, join = st_within)
  
  # Identify points outside the circles
  vendors_outside_circles <- vendors_sf[is.na(vendors_within_circles$circle), ]
  
  # Print vendors outside circles
  print(vendors_outside_circles) # 59 points are outside of the circles
  
  # Map points by circle assignment
  tm_shape(circles_sf) + 
    tm_polygons(col = "white", alpha = 0.5) + 
    tm_shape(vendors_within_circles) + 
    tm_dots(col = "circle", size = 0.1)
  
  # Extract relevant columns from sf object to data frame for export
  vendors_to_check_df <- data.frame(
    latitude = st_coordinates(vendors_outside_circles)[, "Y"],
    longitude = st_coordinates(vendors_outside_circles)[, "X"],
    id = vendors_outside_circles$id,
    date_formatted = vendors_outside_circles$date_formatted,
    enumerator = vendors_outside_circles$enumerator,
    city = vendors_outside_circles$city,
    vendor_name = vendors_outside_circles$vendor_name
  )
  
  # Export points outside circles to manually check in Google My Maps original data collection maps
  write.xlsx(vendors_to_check_df, "vendors_to_check.xlsx")
  
  
  
  
  

# 9. FIX POINTS OUTSIDE CIRCLES----
  
  
  # r_17 longitude used for latitude. Averaging two latitudes collected before and after point.
  
  r_17_new_lat <- (18.394833+18.394485)/2
  print(r_17_new_lat)
  
  combined_data <- combined_data %>%
    mutate(latitude = ifelse(id == 'r_17', r_17_new_lat, latitude))
  
  # q_425 latitude used for longitude. Averaging two longitudes collected before and after point.
  
  q_425_new_lon <- (-46.70753+-46.70823)/2
  print(q_425_new_lon)
  
  combined_data <- combined_data %>%
    mutate(longitude = ifelse(id == 'q_425', q_425_new_lon, longitude))
  
  # q_449 latitude entered incorrectly - 26 instead of 23
  
  q_449_new_lat <- -23.56346
  
  combined_data <- combined_data %>%
    mutate(latitude = ifelse(id == 'q_449', q_449_new_lat, latitude))
 
  # q_470 latitude entered incorrectly - 23.53 instead of 23.56
  
  q_470_new_lat <- -23.56385
  
  combined_data <- combined_data %>%
    mutate(latitude = ifelse(id == 'q_470', q_470_new_lat, latitude))
  
  # r_12 revised location, should have been in circle
  
  r_12_new_lat <- 18.39405
  r_12_new_lon <- 76.58681
  combined_data <- combined_data %>%
    mutate(
      latitude = ifelse(id == 'r_12', r_12_new_lat, latitude),
      longitude = ifelse(id == 'r_12', r_12_new_lon, longitude)
    ) 
  
  # r_13 revised location, should have been in circle
  
  r_13_new_lat <- 18.39438
  r_13_new_lon <- 76.58668
  combined_data <- combined_data %>%
    mutate(
      latitude = ifelse(id == 'r_13', r_13_new_lat, latitude),
      longitude = ifelse(id == 'r_13', r_13_new_lon, longitude)
    )
  
  # r_14 revised location, should have been in circle
  
  r_14_new_lat <- 18.39417
  r_14_new_lon <- 76.58677
  combined_data <- combined_data %>%
    mutate(
      latitude = ifelse(id == 'r_14', r_14_new_lat, latitude),
      longitude = ifelse(id == 'r_14', r_14_new_lon, longitude)
    ) 
  
  # r_34 revised location, should have been in circle
  
  r_34_new_lat <- 18.3998
  r_34_new_lon <- 76.59
  combined_data <- combined_data %>%
    mutate(
      latitude = ifelse(id == 'r_34', r_34_new_lat, latitude),
      longitude = ifelse(id == 'r_34', r_34_new_lon, longitude)
    ) 
  
  # r_36 revised location, should have been in circle
  
  r_36_new_lat <- 18.39992
  r_36_new_lon <- 76.5907
  combined_data <- combined_data %>%
    mutate(
      latitude = ifelse(id == 'r_36', r_36_new_lat, latitude),
      longitude = ifelse(id == 'r_36', r_36_new_lon, longitude)
    ) 
  
  # q_30 revised location, should have been in circle
  
  q_30_new_lat <- 17.7425
  q_30_new_lon <- 83.32323
  combined_data <- combined_data %>%
    mutate(
      latitude = ifelse(id == 'q_30', q_30_new_lat, latitude),
      longitude = ifelse(id == 'q_30', q_30_new_lon, longitude)
    ) 
  
  # q_199 revised location, should have been in circle
  
  q_199_new_lat <- 17.74502
  q_199_new_lon <- 83.32913
  combined_data <- combined_data %>%
    mutate(
      latitude = ifelse(id == 'q_199', q_199_new_lat, latitude),
      longitude = ifelse(id == 'q_199', q_199_new_lon, longitude)
    ) 
  
  # q_213 revised location, should have been in circle
  
  q_213_new_lat <- 17.74325
  q_213_new_lon <- 83.33814
  combined_data <- combined_data %>%
    mutate(
      latitude = ifelse(id == 'q_213', q_213_new_lat, latitude),
      longitude = ifelse(id == 'q_213', q_213_new_lon, longitude)
    ) 
  
  # q_165 revised location, should have been in circle
  
  q_165_new_lat <- 17.7406
  q_165_new_lon <- 83.33691
  combined_data <- combined_data %>%
    mutate(
      latitude = ifelse(id == 'q_165', q_165_new_lat, latitude),
      longitude = ifelse(id == 'q_165', q_165_new_lon, longitude)
    ) 
  
  # r_139 revised location, should have been in circle
  
  r_139_new_lat <- 17.44086
  r_139_new_lon <- 78.39594
  combined_data <- combined_data %>%
    mutate(
      latitude = ifelse(id == 'r_139', r_139_new_lat, latitude),
      longitude = ifelse(id == 'r_139', r_139_new_lon, longitude)
    ) 
  
  # r_145 revised location, should have been in circle
  
  r_145_new_lat <- 17.44836
  r_145_new_lon <- 78.39791
  combined_data <- combined_data %>%
    mutate(
      latitude = ifelse(id == 'r_145', r_145_new_lat, latitude),
      longitude = ifelse(id == 'r_145', r_145_new_lon, longitude)
    ) 
  
  # q_570 revised location, should have been in circle
  
  q_570_new_lat <- -22.90206
  q_570_new_lon <- -43.28144
  combined_data <- combined_data %>%
    mutate(
      latitude = ifelse(id == 'q_570', q_570_new_lat, latitude),
      longitude = ifelse(id == 'q_570', q_570_new_lon, longitude)
    ) 
  
  # q_487 revised location, should have been in circle
  
  q_487_new_lat <- -23.56652
  q_487_new_lon <- -46.61824
  combined_data <- combined_data %>%
    mutate(
      latitude = ifelse(id == 'q_487', q_487_new_lat, latitude),
      longitude = ifelse(id == 'q_487', q_487_new_lon, longitude)
    ) 
  
  # r_42 exclude, point at agricultural college so assume test round
  
  combined_data <- combined_data %>%
    filter(id != 'r_42')
  
  # excluding responses accidentally taken from outside circle boundary
  
  error_outside_circle <- c('r_59', 'r_43', 'r_120', 'r_6', 'r_10', 'r_37', 'r_74', 'q_252', 'q_33', 'q_32', 'q_31', 'q_36', 
                            'q_246', 'q_239', 'q_240', 'q_241', 'q_242','q_243', 'q_245', 'q_244', 'q_217', 'q_218', 'q_216', 'q_164',
                            'q_162','q_163', 'q_92','q_93','q_91','q_90','q_75','q_76','q_77','q_78','q_79','q_80','q_81','q_82',
                            'r_304', 'r_322', 'q_570', 'q_480')
  combined_data <- combined_data %>%
    filter(!id %in% error_outside_circle)


  
  
  
# 10. RE-MAP POINTS TO ASSIGN CIRCLES AND CHECK FOR VALUES OUTSIDE CIRCLES----
  
  # Define datasets
  
    # Use same circlepoints dataset
    circlepoints_2 <- circlepoints
    
    # Create a dataset with only unique ids, lat, lon, city, date and vendor name
    vendors_2 <- combined_data %>%
      dplyr::select(id, date_formatted, enumerator, city, latitude, longitude, vendor_name)
    
  
  # Confirm no missing values for latitude and longitude
  vendors_2 <- vendors_2 %>%
    filter(rowSums(is.na(.)) != ncol(.))
  
  
  # Convert datasets to spatial objects
  circlepoints_sf_2 <- st_as_sf(circlepoints_2, coords = c("lon", "lat"), crs = 4326)
  vendors_sf_2 <- st_as_sf(vendors_2, coords = c("longitude", "latitude"), crs = 4326)
  
  # Create circles with 0.5km radius
  circles_sf_2 <- st_buffer(circlepoints_sf_2, dist = 0.5 * 1000) # 0.5 km to meters
  
  
  # Ensure CRS is consistent
  circlepoints_sf_2 <- st_transform(circlepoints_sf_2, crs = 4326)
  vendors_sf_2 <- st_transform(vendors_sf_2, crs = 4326)
  circles_sf_2 <- st_transform(circles_sf_2, crs = 4326)
  
  # Set tmap mode to view
  #tmap_mode("view")
  
  # Use tmap to create a map
  tm_shape(circles_sf_2) + 
    tm_polygons(col = "blue", alpha = 0.5) + 
    tm_shape(vendors_sf_2) + 
    tm_dots(col = "red", size = 0.1)
  
  
  # Join points to circles if within a circle
  vendors_within_circles_2 <- st_join(vendors_sf_2, circles_sf_2, join = st_within)
  
  # Identify points outside the circles
  vendors_outside_circles_2 <- vendors_sf_2[is.na(vendors_within_circles_2$circle), ]
  
  # Print vendors outside circles
  print(vendors_outside_circles_2) # zero observations
  
  # Map points by circle assignment
  tm_shape(circles_sf_2) + 
    tm_polygons(col = "white", alpha = 0.5) + 
    tm_shape(vendors_within_circles_2) + 
    tm_dots(col = "circle", size = 0.1)
  
  # Extract relevant columns from sf object to data frame
  vendors_circle_details <- data.frame(
    id = vendors_within_circles_2$id,
    circle = vendors_within_circles_2$circle,
    country = vendors_within_circles_2$country
  )
  
  # Join circle column to combined_data based on id
  combined_data <- merge(combined_data, vendors_circle_details, by = "id", all.x = TRUE)
  
  # Reorder to put circle after city
  combined_data <- combined_data %>%
    select(id:enumerator_other, country, city, city_other, circle, everything())
  

  
  
  
  
# 11. REVIEW RICE PRICE DATA----

  # Create new data frame for responses with rice data
  
  rice_prices_data <- combined_data %>%
    filter(!is.na(rice_price_conv) | !is.na(rice_price_org))
  
  # Remove irrelevant columns
  
  rice_columns_keep <- c("id","date_formatted","country","city","circle","vendor_name","vendor_type",
                         "rice_price_conv","rice_price_conv_unit","rice_prive_conv_unit_other","rice_price_conv_displ",
                         "rice_price_org","rice_price_org_unit","rice_price_org_unit_other","rice_price_org_displ")
  
  rice_prices_data <- rice_prices_data %>%
    select(all_of(rice_columns_keep))
  
  # Check unique price values for conventional and organic rice
  print(unique(rice_prices_data$rice_price_conv))
  print(unique(rice_prices_data$rice_price_org))
  
  # Clean up N/A entries and use of currencies
  
  rice_prices_data <- rice_prices_data %>%
    mutate(rice_price_conv = ifelse(rice_price_conv %in% c('N/A', 'Na', 'NA', 'n/a'), NA, rice_price_conv), # removes NA entries
           rice_price_conv = gsub(" reais", "", rice_price_conv), # changes entries with reias suffix
           rice_price_conv = gsub("R\\$", "", rice_price_conv), # changes entries with R prefix
           rice_price_conv = gsub("89P", "0.89", rice_price_conv), # changes entries with a P
           rice_price_conv = gsub(",", ".", rice_price_conv), # changes , to .
           rice_price_conv = gsub("55-60", "57.5", rice_price_conv), # average out range
           rice_price_conv = gsub("60-75", "67.5", rice_price_conv), # average out range
           rice_price_conv = gsub("60-70", "65", rice_price_conv), # average out range
           rice_price_conv = gsub("30-50", "40", rice_price_conv), # average out range
           rice_price_conv = gsub("50-70", "60", rice_price_conv), # average out range
           rice_price_conv = gsub("50-65", "57.5", rice_price_conv), # average out range
           rice_price_conv = gsub("21.99/20kg", "1.10", rice_price_conv), # 21.99/20kg converted to per kg - check with all prices marked other later
           rice_price_conv = as.numeric(rice_price_conv) # Convert to numeric
           )
  print(unique(rice_prices_data$rice_price_conv)) # confirmed all fixed
  
  rice_prices_data <- rice_prices_data %>%
    mutate(rice_price_org = ifelse(rice_price_org %in% c('N/A', 'Na', 'NA'), NA, rice_price_org),
           rice_price_org = gsub("R\\$", "", rice_price_org), # changes entries with R prefix
           rice_price_org = gsub(",", ".", rice_price_org), # changes , to .
           rice_price_org = as.numeric(rice_price_org) # Convert to numeric
           )
  print(unique(rice_prices_data$rice_price_org))   # confirmed all fixed
  
  # Convert prices to per kg
  
    # Create new column for conventional rice price per kg
    
    rice_prices_data <- rice_prices_data %>%
      mutate(rice_price_conv_kg = case_when(
        rice_price_conv_unit %in% c("Per kilogram", "1kg bag") ~ rice_price_conv,
        rice_price_conv_unit == "5kg bag" ~ rice_price_conv / 5,
        rice_price_conv_unit == "10kg bag" ~ rice_price_conv / 10,
        rice_price_conv_unit == "25kg bag" ~ rice_price_conv / 25,
        rice_price_conv_unit == "500g bag" ~ rice_price_conv * 2,
        rice_price_conv_unit == "Other (specify)" & rice_prive_conv_unit_other == "100g" ~ rice_price_conv * (1000/100),
        rice_price_conv_unit == "Other (specify)" & rice_prive_conv_unit_other == "250g" ~ rice_price_conv * (1000/250),
        rice_price_conv_unit == "Other (specify)" & rice_prive_conv_unit_other == "260g" ~ rice_price_conv * (1000/260),
        rice_price_conv_unit == "Other (specify)" & rice_prive_conv_unit_other == "400g" ~ rice_price_conv * (1000/400),
        rice_price_conv_unit == "Other (specify)" & rice_prive_conv_unit_other == "900g" ~ rice_price_conv * (1000/900),
        rice_price_conv_unit == "Other (specify)" & rice_prive_conv_unit_other == "2kg" ~ rice_price_conv / 2,
        rice_price_conv_unit == "Other (specify)" & rice_prive_conv_unit_other == "N/A" ~ rice_price_conv,
        TRUE ~ NA_real_  # Default case if none of the above conditions are met
      ))
    
    # Reorder to place after conventional price columns
    
    rice_prices_data <- rice_prices_data %>%
      select(id:rice_price_conv_displ, rice_price_conv_kg, everything())

    # Create new column for organic rice price per kg
    
    rice_prices_data <- rice_prices_data %>%
      mutate(rice_price_org_kg = case_when(
        rice_price_org_unit %in% c("Per kilogram", "1kg bag") ~ rice_price_org,
        rice_price_org_unit == "5kg bag" ~ rice_price_org / 5,
        rice_price_org_unit == "10kg bag" ~ rice_price_org / 10,
        rice_price_org_unit == "25kg bag" ~ rice_price_org / 25,
        rice_price_org_unit == "500g bag" ~ rice_price_org * 2,
        rice_price_org_unit == "Other (specify)" & rice_price_org_unit_other == "250" ~ rice_price_org * (1000/250),
        rice_price_org_unit == "Other (specify)" & rice_price_org_unit_other == "9kg" ~ rice_price_org / 9,
        rice_price_org_unit == "Other (specify)" & rice_price_org_unit_other == "N/A" ~ rice_price_org,
        TRUE ~ NA_real_  # Default case if none of the above conditions are met
      ))
    
    # Reorder to place after organic price columns
    
    rice_prices_data <- rice_prices_data %>%
      select(id:rice_price_org_displ, rice_price_org_kg, everything())
  
  # Convert to USD
    
    # Import World Bank Official Exchange Rate dataset
    exchange_rates <- read_excel("C:/Users/s1985751/OneDrive - University of Edinburgh/Food Environment Assessment/Data analysis/R/exchange_rates.xlsx")
    
    # Ensure the date_formatted column is in the same format as the date column in exchange_rates - i.e. first of the month
    rice_prices_data <- rice_prices_data %>%
      mutate(date_formatted = floor_date(date_formatted, "month"))
    
    # Merge rice_prices_data with exchange_rates
    merged_rice_data <- rice_prices_data %>%
      left_join(exchange_rates, by = c("date_formatted" = "date"))    
    
    # Create rice_price_conv_unit_kg_usd column
    rice_prices_data <- merged_rice_data %>%
      mutate(rice_price_conv_kg_usd = case_when(
        country == "India" ~ rice_price_conv_kg / IND,
        country == "UK" ~ rice_price_conv_kg / GBR,
        country == "Brazil" ~ rice_price_conv_kg / BRA,
        TRUE ~ NA_real_
      ))
    
    # Create rice_price_org_unit_kg_usd column
    rice_prices_data <- rice_prices_data %>%
      mutate(rice_price_org_kg_usd = case_when(
        country == "India" ~ rice_price_org_kg / IND,
        country == "UK" ~ rice_price_org_kg / GBR,
        country == "Brazil" ~ rice_price_org_kg / BRA,
        TRUE ~ NA_real_
      ))
    
    
    # Check if any prices are greater than 2 standard deviations from the mean
    
      # Calculate mean and standard deviation of rice_price_conv_kg_usd
      stats <- rice_prices_data %>%
        summarize(
          mean_price = mean(rice_price_conv_kg_usd, na.rm = TRUE),
          sd_price = sd(rice_price_conv_kg_usd, na.rm = TRUE)
        )
      
      # Calculate threshold for 2 standard deviations above the mean
      threshold <- stats$mean_price + 2 * stats$sd_price
      
      # Identify prices greater than 2 standard deviations from the mean
      outliers_conv <- rice_prices_data %>%
        filter(rice_price_conv_kg_usd > threshold)
      
      # There are 23 outliers, all in the UK, mostly in London, probably because prices can be so extreme there.
      # Would make more sense to test within a given city/country.
      
      # Calculate mean and standard deviation of rice_price_org_kg_usd
      stats <- rice_prices_data %>%
        summarize(
          mean_price = mean(rice_price_org_kg_usd, na.rm = TRUE),
          sd_price = sd(rice_price_org_kg_usd, na.rm = TRUE)
        )
      
      # Calculate threshold for 2 standard deviations above the mean
      threshold <- stats$mean_price + 2 * stats$sd_price
      
      # Identify prices greater than 2 standard deviations from the mean
      outliers_org <- rice_prices_data %>%
        filter(rice_price_org_kg_usd > threshold)
      
      # There are 4 outliers, again all in the UK, mostly in London

      
      
      

# 12. PREPARE COMPOSITE VARIABLES FOR ANALYSIS----

  # Create rice_price_both variable for when a vendor has both organic and conventional rice  

  rice_prices_data <- rice_prices_data %>%
    mutate(rice_price_both = ifelse(!is.na(rice_price_conv_kg_usd) & !is.na(rice_price_org_kg_usd), 1, 0))
      
  # Create org_foods_count variable to count how many sentinel foods a vendor sells an organic version of
  
  org_foods_vector <- c("tom_org", "leaf_org", "ban_org", "man_org",
                     "fj_org", "milk_org", "cof_org", "tea_org",
                     "mill_org", "chic_org", "daal_org", "wht_org",
                     "rice_org", "nut_org")
  
  combined_data <- combined_data %>%
    mutate(org_foods_count = rowSums(select(., all_of(org_foods_vector)) == "Yes", na.rm = TRUE))
  
  # Create org_vendor binary variable for if vendors sell at least one organic sentinel food

  combined_data <- combined_data %>%
    mutate(org_vendor = case_when(
      org_foods_count >= 1 ~ 1, # Assign 1 if count is greater than or equal to 1
      TRUE ~ 0 # Assign 0 otherwise
    ))
  
  # Create multiple_org_count variable to count per vendor the number of organic sentinel foods with multiple options
  
  multiple_org_vector <- c("tom_org_options", "leaf_org_options", "ban_org_options", "man_org_options",
                        "fj_org_options", "milk_org_options", "cof_org_options", "tea_org_options",
                        "mill_org_options", "chic_org_options", "daal_org_options", "wht_org_options",
                        "rice_org_options", "nut_org_options")
  
  combined_data <- combined_data %>%
    mutate(multiple_org_count = rowSums(select(., all_of(multiple_org_vector)) == "Yes", na.rm = TRUE))
  
  # Create multiple_org_binary variable for if vendors sell at least one organic sentinel food with multiple options
  
  combined_data <- combined_data %>%
    mutate(multiple_org_binary = case_when(
      multiple_org_count >= 1 ~ 1, # Assign 1 if count is greater than or equal to 1
      TRUE ~ 0 # Assign 0 otherwise
    ))
  
  # Create days_open_count variable to count number of days vendors are open
  
  days_vector <- c("vendor_days_Monday","vendor_days_Tuesday","vendor_days_Wednesday","vendor_days_Thursday",
                   "vendor_days_Friday","vendor_days_Saturday","vendor_days_Sunday")
  
  combined_data <- combined_data %>%
    mutate(days_open_count = rowSums(select(., all_of(days_vector)) == 1, na.rm = TRUE))
  
  # Create org_discount_count variable to count organic sentinel foods with a discount
  
  org_discount_vector <- c("tom_org_discount", "leaf_org_discount", "ban_org_discount", "man_org_discount",
                           "fj_org_discount", "milk_org_discount", "cof_org_discount", "tea_org_discount",
                           "mill_org_discount", "chic_org_discount", "daal_org_discount", "wht_org_discount",
                           "rice_org_discount", "nut_org_discount")
  
  combined_data <- combined_data %>%
    mutate(org_discount_count = rowSums(select(., all_of(org_discount_vector)) == "Yes", na.rm = TRUE))
  
  # Create org_discount_binary variable for if vendors sell at least one organic sentinel food with a discount
  
  combined_data <- combined_data %>%
    mutate(org_discount_binary = case_when(
      org_discount_count >= 1 ~ 1, # Assign 1 if count is greater than or equal to 1
      TRUE ~ 0 # Assign 0 otherwise
    ))
  
  # Create vendor_term_organic_count variable to count foods with term organic
  
  terms_organic_vector <- c("tom_org_terms_Organic", "leaf_org_terms_Organic", "ban_org_terms_Organic", "man_org_terms_Organic",
                           "fj_org_terms_Organic", "milk_org_terms_Organic", "cof_org_terms_Organic", "tea_org_terms_Organic",
                           "mill_org_terms_Organic", "chic_org_terms_Organic", "daal_org_terms_Organic", "wht_org_terms_Organic",
                           "rice_org_terms_Organic", "nut_org_terms_Organic")
  
  combined_data <- combined_data %>%
    mutate(vendor_term_organic_count = rowSums(select(., all_of(terms_organic_vector)) == 1, na.rm = TRUE))
  
  # Create vendor_term_natural_count variable to count foods with term natural
  
  terms_natural_vector <- c("tom_org_terms_Natural", "leaf_org_terms_Natural", "ban_org_terms_Natural", "man_org_terms_Natural",
                            "fj_org_terms_Natural", "milk_org_terms_Natural", "cof_org_terms_Natural", "tea_org_terms_Natural",
                            "mill_org_terms_Natural", "chic_org_terms_Natural", "daal_org_terms_Natural", "wht_org_terms_Natural",
                            "rice_org_terms_Natural", "nut_org_terms_Natural")
  
  combined_data <- combined_data %>%
    mutate(vendor_term_natural_count = rowSums(select(., all_of(terms_natural_vector)) == 1, na.rm = TRUE))
  
  # Create vendor_term_chemfree_count variable to count foods with term chemical-free
  
  terms_chemfree_vector <- c("tom_org_terms_Chemical-free", "leaf_org_terms_Chemical-free", "ban_org_terms_Chemical-free", "man_org_terms_Chemical-free",
                             "fj_org_terms_Chemical-free", "milk_org_terms_Chemical-free", "cof_org_terms_Chemical-free", "tea_org_terms_Chemical-free",
                             "mill_org_terms_Chemical-free", "chic_org_terms_Chemical-free", "daal_org_terms_Chemical-free", "wht_org_terms_Chemical-free",
                             "rice_org_terms_Chemical-free", "nut_org_terms_Chemical-free")
  
  combined_data <- combined_data %>%
    mutate(vendor_term_chemfree_count = rowSums(select(., all_of(terms_chemfree_vector)) == 1, na.rm = TRUE))
  
  # Create vendor_term_pestfree_count variable to count foods with term pesticide-free
  
  terms_pestfree_vector <- c("tom_org_terms_Pesticide-free", "leaf_org_terms_Pesticide-free", "ban_org_terms_Pesticide-free", "man_org_terms_Pesticide-free",
                             "fj_org_terms_Pesticide-free", "milk_org_terms_Pesticide-free", "cof_org_terms_Pesticide-free", "tea_org_terms_Pesticide-free",
                             "mill_org_terms_Pesticide-free", "chic_org_terms_Pesticide-free", "daal_org_terms_Pesticide-free", "wht_org_terms_Pesticide-free",
                             "rice_org_terms_Pesticide-free", "nut_org_terms_Pesticide-free")
  
  combined_data <- combined_data %>%
    mutate(vendor_term_pestfree_count = rowSums(select(., all_of(terms_pestfree_vector)) == 1, na.rm = TRUE))
  
  # Create vendor_term_bioprod_count variable to count foods with term bioproducts
  
  terms_bioprod_vector <- c("tom_org_terms_Bioproducts", "leaf_org_terms_Bioproducts", "ban_org_terms_Bioproducts", "man_org_terms_Bioproducts",
                             "fj_org_terms_Bioproducts", "milk_org_terms_Bioproducts", "cof_org_terms_Bioproducts", "tea_org_terms_Bioproducts",
                             "mill_org_terms_Bioproducts", "chic_org_terms_Bioproducts", "daal_org_terms_Bioproducts", "wht_org_terms_Bioproducts",
                             "rice_org_terms_Bioproducts", "nut_org_terms_Bioproducts")
  
  combined_data <- combined_data %>%
    mutate(vendor_term_bioprod_count = rowSums(select(., all_of(terms_bioprod_vector)) == 1, na.rm = TRUE))
  
  # Create vendor_term_bio_count variable to count foods with term bio
  
  terms_bio_vector <- c("tom_org_terms_Bio", "leaf_org_terms_Bio", "ban_org_terms_Bio", "man_org_terms_Bio",
                        "fj_org_terms_Bio", "milk_org_terms_Bio", "cof_org_terms_Bio", "tea_org_terms_Bio",
                        "mill_org_terms_Bio", "chic_org_terms_Bio", "daal_org_terms_Bio", "wht_org_terms_Bio",
                        "rice_org_terms_Bio", "nut_org_terms_Bio")
  
  combined_data <- combined_data %>%
    mutate(vendor_term_bio_count = rowSums(select(., all_of(terms_bio_vector)) == 1, na.rm = TRUE))
  
  # Create vendor_term_eco_count variable to count foods with term eco
  
  terms_eco_vector <- c("tom_org_terms_Eco", "leaf_org_terms_Eco", "ban_org_terms_Eco", "man_org_terms_Eco",
                        "fj_org_terms_Eco", "milk_org_terms_Eco", "cof_org_terms_Eco", "tea_org_terms_Eco",
                        "mill_org_terms_Eco", "chic_org_terms_Eco", "daal_org_terms_Eco", "wht_org_terms_Eco",
                        "rice_org_terms_Eco", "nut_org_terms_Eco")
  
  combined_data <- combined_data %>%
    mutate(vendor_term_eco_count = rowSums(select(., all_of(terms_eco_vector)) == 1, na.rm = TRUE))
  
  # Create vendor_term_gmo_count variable to count foods with term GMO-free
  
  terms_gmo_vector <- c("tom_org_terms_GMO-free", "leaf_org_terms_GMO-free", "ban_org_terms_GMO-free", "man_org_terms_GMO-free",
                        "fj_org_terms_GMO-free", "milk_org_terms_GMO-free", "cof_org_terms_GMO-free", "tea_org_terms_GMO-free",
                        "mill_org_terms_GMO-free", "chic_org_terms_GMO-free", "daal_org_terms_GMO-free", "wht_org_terms_GMO-free",
                        "rice_org_terms_GMO-free", "nut_org_terms_GMO-free")
  
  combined_data <- combined_data %>%
    mutate(vendor_term_gmo_count = rowSums(select(., all_of(terms_gmo_vector)) == 1, na.rm = TRUE))
  
  # Create vendor_term_dontknow_count variable to count foods with unclear terminology
  
  terms_dontknow_vector <- c("tom_org_terms_Don't know", "leaf_org_terms_Don't know", "ban_org_terms_Don't know", "man_org_terms_Don't know",
                               "fj_org_terms_Don't know", "milk_org_terms_Don't know", "cof_org_terms_Don't know", "tea_org_terms_Don't know",
                               "mill_org_terms_Don't know", "chic_org_terms_Don't know", "daal_org_terms_Don't know", "wht_org_terms_Don't know",
                               "rice_org_terms_Don't know", "nut_org_terms_Don't know")
  
  combined_data <- combined_data %>%
    mutate(vendor_term_dontknow_count = rowSums(select(., all_of(terms_dontknow_vector)) == 1, na.rm = TRUE))
  
  # Create a variable per product based on whether the organic product is certified
  # Note that while I could code as anything that isn't the 'None' option, there is a risk of missing things due to data entry error
  
  tom_cert_vector <- c("tom_org_certif_Biodynamic Association Certification","tom_org_certif_EU Green Leaf","tom_org_certif_IBD Brasil",
                       "tom_org_certif_India Organic (blue and red circle and swirl)","tom_org_certif_Jaivik Bharat (green check mark)",
                       "tom_org_certif_Organic Farmers and Growers (OF&G)","tom_org_certif_Organic Food Federation",
                       "tom_org_certif_PGS-India Green (red orb above green leaves)","tom_org_certif_PGS-India Organic (blue orb above green leaves)",
                       "tom_org_certif_Produto Organico Brasil","tom_org_certif_Quality Welsh Food Certification",
                       "tom_org_certif_Soil Association","tom_org_certif_USDA","tom_org_certif_Other (specify)")
  
  combined_data <- combined_data %>%
    mutate(tom_cert = ifelse(rowSums(select(., all_of(tom_cert_vector)), na.rm = TRUE) >= 1, 1, 0))

  leaf_cert_vector <- c("leaf_org_certif_Biodynamic Association Certification","leaf_org_certif_EU Green Leaf","leaf_org_certif_IBD Brasil",
                        "leaf_org_certif_India Organic (blue and red circle and swirl)","leaf_org_certif_Jaivik Bharat (green check mark)",
                        "leaf_org_certif_Organic Farmers and Growers (OF&G)","leaf_org_certif_Organic Food Federation",
                        "leaf_org_certif_PGS-India Green (red orb above green leaves)","leaf_org_certif_PGS-India Organic (blue orb above green leaves)",
                        "leaf_org_certif_Produto Organico Brasil","leaf_org_certif_Quality Welsh Food Certification",
                        "leaf_org_certif_Soil Association","leaf_org_certif_USDA","leaf_org_certif_Other (specify)")
  
  combined_data <- combined_data %>%
    mutate(leaf_cert = ifelse(rowSums(select(., all_of(leaf_cert_vector)), na.rm = TRUE) >= 1, 1, 0))
  
  ban_cert_vector <- c("ban_org_certif_Biodynamic Association Certification","ban_org_certif_EU Green Leaf","ban_org_certif_IBD Brasil",
                       "ban_org_certif_India Organic (blue and red circle and swirl)","ban_org_certif_Jaivik Bharat (green check mark)",
                       "ban_org_certif_Organic Farmers and Growers (OF&G)","ban_org_certif_Organic Food Federation",
                       "ban_org_certif_PGS-India Green (red orb above green leaves)","ban_org_certif_PGS-India Organic (blue orb above green leaves)",
                       "ban_org_certif_Produto Organico Brasil","ban_org_certif_Quality Welsh Food Certification",
                       "ban_org_certif_Soil Association","ban_org_certif_USDA","ban_org_certif_Other (specify)")
  
  combined_data <- combined_data %>%
    mutate(ban_cert = ifelse(rowSums(select(., all_of(ban_cert_vector)), na.rm = TRUE) >= 1, 1, 0))
  
  man_cert_vector <- c("man_org_certif_Biodynamic Association Certification","man_org_certif_EU Green Leaf","man_org_certif_IBD Brasil",
                       "man_org_certif_India Organic (blue and red circle and swirl)","man_org_certif_Jaivik Bharat (green check mark)",
                       "man_org_certif_Organic Farmers and Growers (OF&G)","man_org_certif_Organic Food Federation",
                       "man_org_certif_PGS-India Green (red orb above green leaves)","man_org_certif_PGS-India Organic (blue orb above green leaves)",
                       "man_org_certif_Produto Organico Brasil","man_org_certif_Quality Welsh Food Certification",
                       "man_org_certif_Soil Association","man_org_certif_USDA","man_org_certif_Other (specify)")
  
  combined_data <- combined_data %>%
    mutate(man_cert = ifelse(rowSums(select(., all_of(man_cert_vector)), na.rm = TRUE) >= 1, 1, 0))

  fj_cert_vector <- c("fj_org_certif_Biodynamic Association Certification","fj_org_certif_EU Green Leaf","fj_org_certif_IBD Brasil",
                      "fj_org_certif_India Organic (blue and red circle and swirl)","fj_org_certif_Jaivik Bharat (green check mark)",
                      "fj_org_certif_Organic Farmers and Growers (OF&G)","fj_org_certif_Organic Food Federation",
                      "fj_org_certif_PGS-India Green (red orb above green leaves)","fj_org_certif_PGS-India Organic (blue orb above green leaves)",
                      "fj_org_certif_Produto Organico Brasil","fj_org_certif_Quality Welsh Food Certification",
                      "fj_org_certif_Soil Association","fj_org_certif_USDA","fj_org_certif_Other (specify)")
  
  combined_data <- combined_data %>%
    mutate(fj_cert = ifelse(rowSums(select(., all_of(fj_cert_vector)), na.rm = TRUE) >= 1, 1, 0))

  milk_cert_vector <- c("milk_org_certif_Biodynamic Association Certification","milk_org_certif_EU Green Leaf","milk_org_certif_IBD Brasil",
                        "milk_org_certif_India Organic (blue and red circle and swirl)","milk_org_certif_Jaivik Bharat (green check mark)",
                        "milk_org_certif_Organic Farmers and Growers (OF&G)","milk_org_certif_Organic Food Federation",
                        "milk_org_certif_PGS-India Green (red orb above green leaves)","milk_org_certif_PGS-India Organic (blue orb above green leaves)",
                        "milk_org_certif_Produto Organico Brasil","milk_org_certif_Quality Welsh Food Certification",
                        "milk_org_certif_Soil Association","milk_org_certif_USDA","milk_org_certif_Other (specify)")
  
  combined_data <- combined_data %>%
    mutate(milk_cert = ifelse(rowSums(select(., all_of(milk_cert_vector)), na.rm = TRUE) >= 1, 1, 0))
  
  cof_cert_vector <- c("cof_org_certif_Biodynamic Association Certification","cof_org_certif_EU Green Leaf","cof_org_certif_IBD Brasil",
                       "cof_org_certif_India Organic (blue and red circle and swirl)","cof_org_certif_Jaivik Bharat (green check mark)",
                       "cof_org_certif_Organic Farmers and Growers (OF&G)","cof_org_certif_Organic Food Federation",
                       "cof_org_certif_PGS-India Green (red orb above green leaves)","cof_org_certif_PGS-India Organic (blue orb above green leaves)",
                       "cof_org_certif_Produto Organico Brasil","cof_org_certif_Quality Welsh Food Certification",
                       "cof_org_certif_Soil Association","cof_org_certif_USDA","cof_org_certif_Other (specify)")
  
  combined_data <- combined_data %>%
    mutate(cof_cert = ifelse(rowSums(select(., all_of(cof_cert_vector)), na.rm = TRUE) >= 1, 1, 0))
  
  tea_cert_vector <- c("tea_org_certif_Biodynamic Association Certification","tea_org_certif_EU Green Leaf","tea_org_certif_IBD Brasil",
                       "tea_org_certif_India Organic (blue and red circle and swirl)","tea_org_certif_Jaivik Bharat (green check mark)",
                       "tea_org_certif_Organic Farmers and Growers (OF&G)","tea_org_certif_Organic Food Federation",
                       "tea_org_certif_PGS-India Green (red orb above green leaves)","tea_org_certif_PGS-India Organic (blue orb above green leaves)",
                       "tea_org_certif_Produto Organico Brasil","tea_org_certif_Quality Welsh Food Certification",
                       "tea_org_certif_Soil Association","tea_org_certif_USDA","tea_org_certif_Other (specify)")
  
  combined_data <- combined_data %>%
    mutate(tea_cert = ifelse(rowSums(select(., all_of(tea_cert_vector)), na.rm = TRUE) >= 1, 1, 0))

  mill_cert_vector <- c("mill_org_certif_Biodynamic Association Certification","mill_org_certif_EU Green Leaf","mill_org_certif_IBD Brasil",
                        "mill_org_certif_India Organic (blue and red circle and swirl)","mill_org_certif_Jaivik Bharat (green check mark)",
                        "mill_org_certif_Organic Farmers and Growers (OF&G)","mill_org_certif_Organic Food Federation",
                        "mill_org_certif_PGS-India Green (red orb above green leaves)","mill_org_certif_PGS-India Organic (blue orb above green leaves)",
                        "mill_org_certif_Produto Organico Brasil","mill_org_certif_Quality Welsh Food Certification",
                        "mill_org_certif_Soil Association","mill_org_certif_USDA","mill_org_certif_Other (specify)")
  
  combined_data <- combined_data %>%
    mutate(mill_cert = ifelse(rowSums(select(., all_of(mill_cert_vector)), na.rm = TRUE) >= 1, 1, 0))
  
  daal_cert_vector <- c("daal_org_certif_Biodynamic Association Certification","daal_org_certif_EU Green Leaf","daal_org_certif_IBD Brasil",
                        "daal_org_certif_India Organic (blue and red circle and swirl)","daal_org_certif_Jaivik Bharat (green check mark)",
                        "daal_org_certif_Organic Farmers and Growers (OF&G)","daal_org_certif_Organic Food Federation",
                        "daal_org_certif_PGS-India Green (red orb above green leaves)","daal_org_certif_PGS-India Organic (blue orb above green leaves)",
                        "daal_org_certif_Produto Organico Brasil","daal_org_certif_Quality Welsh Food Certification",
                        "daal_org_certif_Soil Association","daal_org_certif_USDA","daal_org_certif_Other (specify)")
  
  combined_data <- combined_data %>%
    mutate(daal_cert = ifelse(rowSums(select(., all_of(daal_cert_vector)), na.rm = TRUE) >= 1, 1, 0))

  wht_cert_vector <- c("wht_org_certif_Biodynamic Association Certification","wht_org_certif_EU Green Leaf","wht_org_certif_IBD Brasil",
                       "wht_org_certif_India Organic (blue and red circle and swirl)","wht_org_certif_Jaivik Bharat (green check mark)",
                       "wht_org_certif_Organic Farmers and Growers (OF&G)","wht_org_certif_Organic Food Federation",
                       "wht_org_certif_PGS-India Green (red orb above green leaves)","wht_org_certif_PGS-India Organic (blue orb above green leaves)",
                       "wht_org_certif_Produto Organico Brasil","wht_org_certif_Quality Welsh Food Certification",
                       "wht_org_certif_Soil Association","wht_org_certif_USDA","wht_org_certif_Other (specify)")
  
  combined_data <- combined_data %>%
    mutate(wht_cert = ifelse(rowSums(select(., all_of(wht_cert_vector)), na.rm = TRUE) >= 1, 1, 0))

  rice_cert_vector <- c("rice_org_certif_Biodynamic Association Certification","rice_org_certif_EU Green Leaf","rice_org_certif_IBD Brasil",
                        "rice_org_certif_India Organic (blue and red circle and swirl)","rice_org_certif_Jaivik Bharat (green check mark)",
                        "rice_org_certif_Organic Farmers and Growers (OF&G)","rice_org_certif_Organic Food Federation",
                        "rice_org_certif_PGS-India Green (red orb above green leaves)","rice_org_certif_PGS-India Organic (blue orb above green leaves)",
                        "rice_org_certif_Produto Organico Brasil","rice_org_certif_Quality Welsh Food Certification",
                        "rice_org_certif_Soil Association","rice_org_certif_USDA","rice_org_certif_Other (specify)")
  
  combined_data <- combined_data %>%
    mutate(rice_cert = ifelse(rowSums(select(., all_of(rice_cert_vector)), na.rm = TRUE) >= 1, 1, 0))
  
  nut_cert_vector <- c("nut_org_certif_Biodynamic Association Certification","nut_org_certif_EU Green Leaf","nut_org_certif_IBD Brasil",
                       "nut_org_certif_India Organic (blue and red circle and swirl)","nut_org_certif_Jaivik Bharat (green check mark)",
                       "nut_org_certif_Organic Farmers and Growers (OF&G)","nut_org_certif_Organic Food Federation",
                       "nut_org_certif_PGS-India Green (red orb above green leaves)","nut_org_certif_PGS-India Organic (blue orb above green leaves)",
                       "nut_org_certif_Produto Organico Brasil","nut_org_certif_Quality Welsh Food Certification",
                       "nut_org_certif_Soil Association","nut_org_certif_USDA","nut_org_certif_Other (specify)")
  
  combined_data <- combined_data %>%
    mutate(nut_cert = ifelse(rowSums(select(., all_of(nut_cert_vector)), na.rm = TRUE) >= 1, 1, 0))
  
  chic_cert_vector <- c("chic_org_certif_Biodynamic Association Certification","chic_org_certif_EU Green Leaf","chic_org_certif_IBD Brasil",
                        "chic_org_certif_India Organic (blue and red circle and swirl)","chic_org_certif_Jaivik Bharat (green check mark)",
                        "chic_org_certif_Organic Farmers and Growers (OF&G)","chic_org_certif_Organic Food Federation",
                        "chic_org_certif_PGS-India Green (red orb above green leaves)","chic_org_certif_PGS-India Organic (blue orb above green leaves)",
                        "chic_org_certif_Produto Organico Brasil","chic_org_certif_Quality Welsh Food Certification",
                        "chic_org_certif_Soil Association","chic_org_certif_USDA","chic_org_certif_Other (specify)")
  
  combined_data <- combined_data %>%
    mutate(chic_cert = ifelse(rowSums(select(., all_of(chic_cert_vector)), na.rm = TRUE) >= 1, 1, 0))
  

  # Create cert_count variable to count per vendor the number of organic sentinel foods that are certified
  
  certif_vector <- c("tom_cert","leaf_cert","ban_cert","man_cert","fj_cert","milk_cert","cof_cert",
                     "tea_cert","mill_cert","daal_cert","wht_cert","rice_cert","nut_cert","chic_cert")
  
  combined_data <- combined_data %>%
    mutate(cert_count = rowSums(select(., all_of(certif_vector)) == 1, na.rm = TRUE))
    
  # Create theme_health_count variable to count per vendor the number of organic sentinel foods with the theme health
  
  theme_health_vector <- c("tom_org_pack_themes_Health benefits","leaf_org_pack_themes_Health benefits","ban_org_pack_themes_Health benefits",
                    "man_org_pack_themes_Health benefits","fj_org_pack_themes_Health benefits","milk_org_pack_themes_Health benefits",
                    "cof_org_pack_themes_Health benefits","tea_org_pack_themes_Health benefits","mill_org_pack_themes_Health benefits",
                    "daal_org_pack_themes_Health benefits","wht_org_pack_themes_Health benefits","rice_org_pack_themes_Health benefits",
                    "nut_org_pack_themes_Health benefits","chic_org_pack_themes_Health benefits")
  
  combined_data <- combined_data %>%
    mutate(theme_health_count = rowSums(select(., all_of(theme_health_vector)) == 1, na.rm = TRUE))
  
  # Create theme_sj_count variable to count per vendor the number of organic sentinel foods with the theme social justice
  
  theme_sj_vector <- c("tom_org_pack_themes_Social justice (e.g. farmer wellbeing)","leaf_org_pack_themes_Social justice (e.g. farmer wellbeing)","ban_org_pack_themes_Social justice (e.g. farmer wellbeing)",
                           "man_org_pack_themes_Social justice (e.g. farmer wellbeing)","fj_org_pack_themes_Social justice (e.g. farmer wellbeing)","milk_org_pack_themes_Social justice (e.g. farmer wellbeing)",
                           "cof_org_pack_themes_Social justice (e.g. farmer wellbeing)","tea_org_pack_themes_Social justice (e.g. farmer wellbeing)","mill_org_pack_themes_Social justice (e.g. farmer wellbeing)",
                           "daal_org_pack_themes_Social justice (e.g. farmer wellbeing)","wht_org_pack_themes_Social justice (e.g. farmer wellbeing)","rice_org_pack_themes_Social justice (e.g. farmer wellbeing)",
                           "nut_org_pack_themes_Social justice (e.g. farmer wellbeing)","chic_org_pack_themes_Social justice (e.g. farmer wellbeing)")
  
  combined_data <- combined_data %>%
    mutate(theme_sj_count = rowSums(select(., all_of(theme_sj_vector)) == 1, na.rm = TRUE))
  
  # Create theme_env_count variable to count per vendor the number of organic sentinel foods with the theme environmental benefits
  
  theme_env_vector <- c("tom_org_pack_themes_Environmental benefits","leaf_org_pack_themes_Environmental benefits","ban_org_pack_themes_Environmental benefits",
                           "man_org_pack_themes_Environmental benefits","fj_org_pack_themes_Environmental benefits","milk_org_pack_themes_Environmental benefits",
                           "cof_org_pack_themes_Environmental benefits","tea_org_pack_themes_Environmental benefits","mill_org_pack_themes_Environmental benefits",
                           "daal_org_pack_themes_Environmental benefits","wht_org_pack_themes_Environmental benefits","rice_org_pack_themes_Environmental benefits",
                           "nut_org_pack_themes_Environmental benefits","chic_org_pack_themes_Environmental benefits")
  
  combined_data <- combined_data %>%
    mutate(theme_env_count = rowSums(select(., all_of(theme_env_vector)) == 1, na.rm = TRUE))
  
  # Create theme_trad_count variable to count per vendor the number of organic sentinel foods with the theme tradition / culture
  
  theme_trad_vector <- c("tom_org_pack_themes_Tradition / culture","leaf_org_pack_themes_Tradition / culture","ban_org_pack_themes_Tradition / culture",
                           "man_org_pack_themes_Tradition / culture","fj_org_pack_themes_Tradition / culture","milk_org_pack_themes_Tradition / culture",
                           "cof_org_pack_themes_Tradition / culture","tea_org_pack_themes_Tradition / culture","mill_org_pack_themes_Tradition / culture",
                           "daal_org_pack_themes_Tradition / culture","wht_org_pack_themes_Tradition / culture","rice_org_pack_themes_Tradition / culture",
                           "nut_org_pack_themes_Tradition / culture","chic_org_pack_themes_Tradition / culture")
  
  combined_data <- combined_data %>%
    mutate(theme_trad_count = rowSums(select(., all_of(theme_trad_vector)) == 1, na.rm = TRUE))
  
  # Create theme_taste_count variable to count per vendor the number of organic sentinel foods with the theme taste
  
  theme_taste_vector <- c("tom_org_pack_themes_Taste","leaf_org_pack_themes_Taste","ban_org_pack_themes_Taste",
                          "man_org_pack_themes_Taste","fj_org_pack_themes_Taste","milk_org_pack_themes_Taste",
                          "cof_org_pack_themes_Taste","tea_org_pack_themes_Taste","mill_org_pack_themes_Taste",
                          "daal_org_pack_themes_Taste","wht_org_pack_themes_Taste","rice_org_pack_themes_Taste",
                          "nut_org_pack_themes_Taste","chic_org_pack_themes_Taste")
  
  combined_data <- combined_data %>%
    mutate(theme_taste_count = rowSums(select(., all_of(theme_taste_vector)) == 1, na.rm = TRUE))
  
  # Create theme_qual_count variable to count per vendor the number of organic sentinel foods with the theme quality
  
  theme_qual_vector <- c("tom_org_pack_themes_Quality","leaf_org_pack_themes_Quality","ban_org_pack_themes_Quality",
                         "man_org_pack_themes_Quality","fj_org_pack_themes_Quality","milk_org_pack_themes_Quality",
                         "cof_org_pack_themes_Quality","tea_org_pack_themes_Quality","mill_org_pack_themes_Quality",
                         "daal_org_pack_themes_Quality","wht_org_pack_themes_Quality","rice_org_pack_themes_Quality",
                         "nut_org_pack_themes_Quality","chic_org_pack_themes_Quality")
  
  combined_data <- combined_data %>%
    mutate(theme_qual_count = rowSums(select(., all_of(theme_qual_vector)) == 1, na.rm = TRUE))
  
  # Create theme_life_count variable to count per vendor the number of organic sentinel foods with the theme lifestyle (e.g. “live an organic life”)
  
  theme_life_vector <- c('tom_org_pack_themes_Lifestyle (e.g. "live an organic life")', 'leaf_org_pack_themes_Lifestyle (e.g. "live an organic life")', 
                         'ban_org_pack_themes_Lifestyle (e.g. "live an organic life")', 'man_org_pack_themes_Lifestyle (e.g. "live an organic life")', 
                         'fj_org_pack_themes_Lifestyle (e.g. "live an organic life")', 'milk_org_pack_themes_Lifestyle (e.g. "live an organic life")', 
                         'cof_org_pack_themes_Lifestyle (e.g. "live an organic life")', 'tea_org_pack_themes_Lifestyle (e.g. "live an organic life")', 
                         'mill_org_pack_themes_Lifestyle (e.g. "live an organic life")', 'daal_org_pack_themes_Lifestyle (e.g. "live an organic life")', 
                         'wht_org_pack_themes_Lifestyle (e.g. "live an organic life")', 'rice_org_pack_themes_Lifestyle (e.g. "live an organic life")', 
                         'nut_org_pack_themes_Lifestyle (e.g. "live an organic life")', 'chic_org_pack_themes_Lifestyle (e.g. "live an organic life")')
  
  combined_data <- combined_data %>%
    mutate(theme_life_count = rowSums(select(., all_of(theme_life_vector)) == 1, na.rm = TRUE))
  
  # Create theme_none_count variable to count per vendor the number of organic sentinel foods with no theme
  
  theme_none_vector <- c("tom_org_pack_themes_None","leaf_org_pack_themes_None","ban_org_pack_themes_None",
                         "man_org_pack_themes_None","fj_org_pack_themes_None","milk_org_pack_themes_None",
                         "cof_org_pack_themes_None","tea_org_pack_themes_None","mill_org_pack_themes_None",
                         "daal_org_pack_themes_None","wht_org_pack_themes_None","rice_org_pack_themes_None",
                         "nut_org_pack_themes_None","chic_org_pack_themes_None")
  
  combined_data <- combined_data %>%
    mutate(theme_none_count = rowSums(select(., all_of(theme_none_vector)) == 1, na.rm = TRUE))
  
  # Create theme_na_count variable to count per vendor the number of organic sentinel foods for which theme is not applicable (e.g. no/blank packaging)
  
  theme_na_vector <- c("tom_org_pack_themes_Not applicable (no packaging or blank packaging)","leaf_org_pack_themes_Not applicable (no packaging or blank packaging)","ban_org_pack_themes_Not applicable (no packaging or blank packaging)",
                       "man_org_pack_themes_Not applicable (no packaging or blank packaging)","fj_org_pack_themes_Not applicable (no packaging or blank packaging)","milk_org_pack_themes_Not applicable (no packaging or blank packaging)",
                       "cof_org_pack_themes_Not applicable (no packaging or blank packaging)","tea_org_pack_themes_Not applicable (no packaging or blank packaging)","mill_org_pack_themes_Not applicable (no packaging or blank packaging)",
                       "daal_org_pack_themes_Not applicable (no packaging or blank packaging)","wht_org_pack_themes_Not applicable (no packaging or blank packaging)","rice_org_pack_themes_Not applicable (no packaging or blank packaging)",
                       "nut_org_pack_themes_Not applicable (no packaging or blank packaging)","chic_org_pack_themes_Not applicable (no packaging or blank packaging)")
  
  combined_data <- combined_data %>%
    mutate(theme_na_count = rowSums(select(., all_of(theme_na_vector)) == 1, na.rm = TRUE))
  
  # Create theme_other_count variable to count per vendor the number of organic sentinel foods with an unspecified/other theme
  
  theme_other_vector <- c("tom_org_pack_themes_Other (specify)","leaf_org_pack_themes_Other (specify)","ban_org_pack_themes_Other (specify)",
                          "man_org_pack_themes_Other (specify)","fj_org_pack_themes_Other (specify)","milk_org_pack_themes_Other (specify)",
                          "cof_org_pack_themes_Other (specify)","tea_org_pack_themes_Other (specify)","mill_org_pack_themes_Other (specify)",
                          "daal_org_pack_themes_Other (specify)","wht_org_pack_themes_Other (specify)","rice_org_pack_themes_Other (specify)",
                          "nut_org_pack_themes_Other (specify)","chic_org_pack_themes_Other (specify)")
  
  combined_data <- combined_data %>%
    mutate(theme_other_count = rowSums(select(., all_of(theme_other_vector)) == 1, na.rm = TRUE))
  
  # Create foods_count variable to count per vendor the number of sentinel foods, both organic and non-organic
  
  foods_count_vector <- c("tom_sell","leaf_sell","ban_sell","man_sell","fj_sell",
                          "milk_sell","cof_sell","tea_sell","mill_sell","daal_sell",
                          "wht_sell","rice_sell","nut_sell","chic_sell")
  
  combined_data <- combined_data %>%
    mutate(foods_count = rowSums(select(., all_of(foods_count_vector)) == "Yes", na.rm = TRUE))
  
  
        
# 13. EXPORT DATA TO EXCEL FILES----

# OVERALL DATA
  
  # Create workbook of cleaned data
  data_clean <- createWorkbook()
  addWorksheet(data_clean, "data")
    
  # Export cleaned data
  writeData(data_clean, sheet = "data", x = combined_data)
  
  # Save the workbook
  saveWorkbook(data_clean, "C:/Users/s1985751/Documents/GitHub/fea/data_clean.xlsx", overwrite = TRUE)


  
# RICE PRICE DATA
  
  # Create workbook of rice price data
  rice_prices_data_clean <- createWorkbook()
  addWorksheet(rice_prices_data_clean, "data")
  
  # Export cleaned data
  writeData(rice_prices_data_clean, sheet = "data", x = rice_prices_data)
  
  # Save the workbook
  saveWorkbook(rice_prices_data_clean, "C:/Users/s1985751/Documents/GitHub/fea/rice_prices_data_clean.xlsx", overwrite = TRUE)
  
  
  
#write.xlsx(combined_data, "combined_data.xlsx")  
#write.xlsx(qdata1, "qdata1.xlsx")
#write.xlsx(rdata1, "rdata1.xlsx")
#write.xlsx(data_types, "data_types.xlsx")
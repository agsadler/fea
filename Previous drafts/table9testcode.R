# Vector of product abbreviations
products <- c("tom", "leaf", "ban", "man", "fj", "milk", "cof", "tea", "mill", "chic", "daal", "wht", "rice", "nut")

# PRODUCT-LEVEL SUMMARY
# Define a function to summarize data for a given product and dataset
product_summary_list <- lapply(products, function(product) {
  data %>%
    summarise(
      cert_count = sum(get(paste0(product, "_cert")) == 1),
      cert_bac_count = sum(get(paste0(product, "_org_certif_Biodynamic.Association.Certification")) == 1, na.rm = TRUE),
      cert_eu_count = sum(get(paste0(product, "_org_certif_EU.Green.Leaf")) == 1, na.rm = TRUE),
      cert_ibd_count = sum(get(paste0(product, "_org_certif_IBD.Brasil")) == 1, na.rm = TRUE),
      cert_io_count = sum(get(paste0(product, "_org_certif_India.Organic.(blue.and.red.circle.and.swirl)")) == 1, na.rm = TRUE),
      cert_jb_count = sum(get(paste0(product, "_org_certif_Jaivik.Bharat.(green.check.mark)")) == 1, na.rm = TRUE),
      cert_ofg_count = sum(get(paste0(product, "_org_certif_Organic.Farmers.and.Growers.(OF&G)")) == 1, na.rm = TRUE),
      cert_off_count = sum(get(paste0(product, "_org_certif_Organic.Food.Federation")) == 1, na.rm = TRUE),
      cert_pgsg_count = sum(get(paste0(product, "_org_certif_PGS-India.Green.(red.orb.above.green.leaves)")) == 1, na.rm = TRUE),
      cert_pgso_count = sum(get(paste0(product, "_org_certif_PGS-India.Organic.(blue.orb.above.green.leaves)")) == 1, na.rm = TRUE),
      cert_pob_count = sum(get(paste0(product, "_org_certif_Produto.Organico.Brasil")) == 1, na.rm = TRUE),
      cert_qwfc_count = sum(get(paste0(product, "_org_certif_Quality.Welsh.Food.Certification")) == 1, na.rm = TRUE),
      cert_sa_count = sum(get(paste0(product, "_org_certif_Soil.Association")) == 1, na.rm = TRUE),
      cert_usda_count = sum(get(paste0(product, "_org_certif_USDA")) == 1, na.rm = TRUE))
})
#    ) %>%
#    mutate(
#      product = product,
#      category = case_when(product %in% c("fj", "milk", "cof", "tea") ~ "beverages",
#                           product %in% c("tom", "leaf", "ban", "man") ~ "fresh produce",
#                           TRUE ~ "other")
#    ) %>% 
#    select(category, product, everything())
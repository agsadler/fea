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
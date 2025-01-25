# ==============================================================================
# Project: Justifiable Homicides (JH) and SYG and RTC laws
# Version: Article v1
#
# Purpose: This code performs the creation of tables with all crimes for
#          additional tests.
#
# Funding: This research was supported by funding from the Center for Studies of 
# the Economic Order at the Federal University of Sao Paulo (CEOE/Unifesp). The 
# funding was allocated through Decentralized Execution Agreement (02/2020) in 
# partnership with the Diffuse Rights Fund, managed by the National Consumer 
# Secretariat of the Ministry of Justice and Public Security (Process SEI 
# no. 08012.003253/2018-45).
#
# Author: Nelson Coelho
# Organization: Centro de Estudos da Ordem Economica - CEOE/Unifesp
# Date: 2024-1-16
#
# File Name: all_crimes_tablecreation.R
#
# Description: This file contains R code to create tables with all crimes.
#              This new table builds up on the tables we already had created
#              with the two different imputation methods.
#
# Modifications:
#
# Changes:
#
# - 2024-1-16: Initial version by Nelson Coelho
#
# ==============================================================================

# Install and load required packages ===========================================
required_packages <- c("ggplot2","dplyr", "plyr", "haven", "R.utils", 
                       "tibble", "tidyr", "xts", "tidyverse", "Hmisc",
                       "readxl", "knitr", "janitor", "jsonlite", "readr",
                       "tidycensus", "psych", "missForest", "mice", "skimr",
                       "devtools", "magrittr", "kableExtra", "data.table",
                       "pastecs", "gsynth")

installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
}

# Load libraries ===============================================================
library(ggplot2)     # Used for creating graphics
library(dplyr)       # Used for data manipulation
library(plyr)        # Used for data manipulation
library(haven)       # Import and Export 'SPSS', 'Stata' and 'SAS' Files
library(R.utils)     # Various Programming Utilities
library(tibble)      # Provides a 'tbl_df' class (the 'tibble') with stricter checking and better formatting than the traditional data frame.
library(tidyr)       # Used for data manipulation
library(xts)         # Used for data manipulation
library(tidyverse)   # Used for data manipulation
library(Hmisc)       # Used for data manipulation
library(readxl)      # Read excel files
library(knitr)       # Used for data manipulation
library(janitor)     # Used for data manipulation
library(jsonlite)    # Converting between R objects and JSON
library(readr)       # Read csv files
library(tidycensus)  # Used to retrieve CENSUS data
library(psych)       # Used for correlation test
library(missForest)  # Used for missing data imputation
library(mice)        # Used for missing data imputation
library(skimr)       # Used to provide summary statistics
library(devtools)    # Used to install packages
library(magrittr)    # A Forward-Pipe Operator
library(kableExtra)  # Adds to "magrittr" and "knitr"
library(data.table)  # Converts list into dataframe
library(pastecs)     # Gets descriptive statistics
library(gsynth)      # Performs the Generalized Synthetic Control Tests

#===============================================================================
# here we add several additional crimes to our databases
# initially, we add it to the regular tables where imputation was performed by making NA to 0
# then we add it to the MICE RF imputation table

#load regular tables 
JH_db_reg_diversecrimes <- read_csv('C:/Users/sarah/Desktop/lookinto/CEOE/_official_tables/JH_Oct_with_LEOKA.csv')

#load tables created with MICE RF algorithm
JH_db_MICERF_diversecrimes <- read_csv('C:/Users/sarah/Desktop/lookinto/CEOE/_official_tables/JH_Oct_clean.csv')

#load table that has all crimes by state
estimated_crimes_1979_2023 <- read_csv('C:/Users/sarah/Desktop/lookinto/CEOE/_official_tables/estimated_crimes_1979_2023.csv')

#clean estimated crimes table

#removing years not used in study
estimated_crimes_1979_2023 <- estimated_crimes_1979_2023 %>%  filter(estimated_crimes_1979_2023$year<='2020')

#filter out NA state names (national level data)
estimated_crimes_1979_2023 <- estimated_crimes_1979_2023 %>%  filter(estimated_crimes_1979_2023$state_abbr!='NA')

#make var lowercase
estimated_crimes_1979_2023$state_name <- tolower(estimated_crimes_1979_2023$state_name)

#adding a variable that is used to join tables later on
estimated_crimes_1979_2023 <- estimated_crimes_1979_2023 %>% unite('BREAKln', year, state_name, remove = FALSE)

#remove dups
estimated_crimes_1979_2023 <- estimated_crimes_1979_2023 %>% distinct(BREAKln, .keep_all = TRUE)

#remove vars not needed
estimated_crimes_1979_2023 <- subset(estimated_crimes_1979_2023, select = -c(year, caveats))

#joining tables
JH_all_crimes_regular <- left_join(JH_db_reg_diversecrimes, estimated_crimes_1979_2023, by = "BREAKln")
JH_all_crimes_MICERF <- left_join(JH_db_MICERF_diversecrimes, estimated_crimes_1979_2023, by = "BREAKln")

#save tables
write.csv(JH_all_crimes_regular, "C:/Users/sarah/Desktop/lookinto/CEOE/_official_tables/JH_all_crimes_regular.csv", row.names=FALSE)
write.csv(JH_all_crimes_MICERF, "C:/Users/sarah/Desktop/lookinto/CEOE/_official_tables/JH_all_crimes_MICERF.csv", row.names=FALSE)

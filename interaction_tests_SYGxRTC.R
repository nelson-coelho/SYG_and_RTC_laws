# ==============================================================================
# Project: Justifiable Homicides (JH) and SYG and RTC laws
# Version: Article v1
#
# Purpose: This code performs the FENB tests for the regular table with policy
#          interactions.
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
# File Name: interaction_tests_SYGxRTC.R
#
# Description: This file contains R code to perform the FENB tests for the 
#              regular table with policy interactions.
#
# Modifications:
#
# Changes:
#
# - 2025-1-16: Initial version by Nelson Coelho
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
#load tests
JH_db_Oct_w_LEOKA <- read_csv('C:/JH_Oct_with_LEOKA.csv')


#check for duplicates for regular table
n_occur <- data.frame(table(JH_db_Oct_w_LEOKA$BREAKln))

#which rows happened more than once
n_occur[n_occur$Freq > 1,]

#list dups
JH_db_Oct_w_LEOKA[JH_db_Oct_w_LEOKA$BREAKln %in% n_occur$Var1[n_occur$Freq > 1],]



######################################################################################
#
#   BELOW WE FINALLY RUN FENB and ZINB tests
# # all specification tests below indicated fixed effects for this panel
#

#######################
#citizen model
model_cit <- JH_cit ~ shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate + SYGxRTC | state
model_cit.fenegbin <- fenegbin(model_cit, data=JH_db_Oct_w_LEOKA)
summary(model_cit.fenegbin)

#hausman test
fixed <- plm(model_cit, data=JH_db_Oct_w_LEOKA, index=c("state", "year"), model="within")  #fixed model
random <- plm(model_cit, data=JH_db_Oct_w_LEOKA, index=c("state", "year"), model="random")  #random model
phtest(fixed,random) #Hausman test
#hausman is significant, thus we use fe negbin

#bp test
plmtest(fixed, c("time"), type=("bp"))
plmtest(fixed, c("individual"), type=("bp"))


#citizen robustness check
model_cit_test <- JH_cit ~ shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate + pop_20_39 + SYGxRTC | state
model_cit_test.fenegbin <- fenegbin(model_cit_test, data=JH_db_Oct_w_LEOKA)
summary(model_cit_test.fenegbin)


#######################
#police model
model_pol <- JH_pol ~ shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate + SYGxRTC | state
model_pol.fenegbin <- fenegbin(model_pol, data=JH_db_Oct_w_LEOKA)
summary(model_pol.fenegbin)

#hausman test
fixed <- plm(model_pol, data=JH_db_Oct_w_LEOKA, index=c("state", "year"), model="within")  #fixed model
random <- plm(model_pol, data=JH_db_Oct_w_LEOKA, index=c("state", "year"), model="random")  #random model
phtest(fixed,random) #Hausman test
#hausman is significant, thus we use fe negbin

#bp test
plmtest(fixed, c("time"), type=("bp"))
plmtest(fixed, c("individual"), type=("bp"))

#police robustness check
model_pol_test <- JH_pol ~ shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate + pop_20_39 + SYGxRTC | state
model_pol_test.fenegbin <- fenegbin(model_pol_test, data=JH_db_Oct_w_LEOKA)
summary(model_pol_test.fenegbin)



#######################
#total model
model_tot <- JH_tot ~ shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate + SYGxRTC | state
model_tot.fenegbin <- fenegbin(model_tot, data=JH_db_Oct_w_LEOKA)
summary(model_tot.fenegbin)

#hausman test
fixed <- plm(model_tot, data=JH_db_Oct_w_LEOKA, index=c("state", "year"), model="within")  #fixed model
random <- plm(model_tot, data=JH_db_Oct_w_LEOKA, index=c("state", "year"), model="random")  #random model
phtest(fixed,random) #Hausman test
#hausman is significant, thus we use fe negbin

#bp test
plmtest(fixed, c("time"), type=("bp"))
plmtest(fixed, c("individual"), type=("bp"))


#total robustness check
model_tot_test <- JH_tot ~ shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate + pop_20_39 + SYGxRTC | state
model_tot_test.fenegbin <- fenegbin(model_tot_test, data=JH_db_Oct_w_LEOKA)
summary(model_tot_test.fenegbin)



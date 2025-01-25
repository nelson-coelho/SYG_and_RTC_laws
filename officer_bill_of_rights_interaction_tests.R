# ==============================================================================
# Project: Justifiable Homicides (JH) and SYG and RTC laws
# Version: Article v1
#
# Purpose: This code performs the FENB and ZINB tests for the JH dependent variable
#          on the regular table and MICE RF table as it relates to the officer
#          bill of rights.
#
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
# Date: 2024-1-23
#
# File Name: officer_bill_of_rights_interaction_tests.R
#
# Description: This file contains R code to test the officer bill of rights
#              interaction with the JH.
#
# Modifications:
#
# Changes:
#
# - 2024-1-23: Initial version by Nelson Coelho
#
# ==============================================================================

# Install and load required packages ===========================================
required_packages <- c("ggplot2","dplyr", "plyr", "haven", "R.utils", 
                       "tibble", "tidyr", "xts", "tidyverse", "Hmisc",
                       "readxl", "knitr", "janitor", "jsonlite", "readr",
                       "tidycensus", "psych", "missForest", "mice", "skimr",
                       "devtools", "magrittr", "kableExtra", "data.table",
                       "pastecs", "gsynth", "plm", "fixest", "pscl")

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
library(plm)         # Used to check if we should use fixed of random effects
library(fixest)      # Used to perform fixed effects negative binomial regression
library(pscl)        # Used to perform zero inflated negative binomial regression


# ==============================================================================
#load regular table
JH_all_crimes_regular <- read_csv('C:/Users/sarah/Desktop/lookinto/CEOE/_official_tables/JH_all_crimes_regular.csv')

#load MICERF table
JH_all_crimes_MICERF <- read_csv('C:/Users/sarah/Desktop/lookinto/CEOE/_official_tables/JH_all_crimes_MICERF.csv')

#create interaction variable
JH_all_crimes_MICERF$SYGxRTC <- JH_all_crimes_MICERF$shall_issue * JH_all_crimes_MICERF$syg_law

#load officer bill of rights db
officer_bor <- read_csv('C:/Users/sarah/Desktop/lookinto/CEOE/_official_tables/Officer_Bill_Of_Rights_db.csv')

#joining tables
JH_all_crimes_regular_officer_BoR <- left_join(JH_all_crimes_regular, officer_bor, by = "BREAKln")
JH_all_crimes_MICERF_officer_BoR <- left_join(JH_all_crimes_MICERF, officer_bor, by = "BREAKln")

# ==============================================================================
# FENB regular table tests

#JH cit model with interaction
model_cit_BoR_reg <- JH_cit ~ shall_issue + syg_law + SYGxRTC + officer_bill_of_rights + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate
model_cit_BoR_reg.fenegbin <- fenegbin(model_cit_BoR_reg, data=JH_all_crimes_regular_officer_BoR)
summary(model_cit_BoR_reg.fenegbin)

#JH pol model with interaction
model_pol_BoR_reg <- JH_pol ~ shall_issue + syg_law + SYGxRTC + officer_bill_of_rights + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate
model_pol_BoR_reg.fenegbin <- fenegbin(model_pol_BoR_reg, data=JH_all_crimes_regular_officer_BoR)
summary(model_pol_BoR_reg.fenegbin)


# ==============================================================================
# ZINB regular table tests

#cit JH
inf_JH_cit <- zeroinfl(JH_cit ~ shall_issue + syg_law + SYGxRTC + officer_bill_of_rights + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate |
                         shall_issue + syg_law + SYGxRTC + officer_bill_of_rights + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate,
                       dist = 'negbin',
                       data = JH_all_crimes_regular_officer_BoR)
summary(inf_JH_cit)

# Dispersion Statistic
inf_JH_cit.res <- resid(inf_JH_cit, type = "pearson")
N  <- nrow(JH_all_crimes_regular_officer_BoR)
p  <- length(coef(inf_JH_cit)) + 1 # '+1' is due to theta
sum(inf_JH_cit.res^2) / (N - p)


## Exponentiated coefficients
expCoef <- exp(coef((inf_JH_cit)))
expCoef <- matrix(expCoef, ncol = 2)
rownames(expCoef) <- c("Intercept", "shall_issue","syg_law","SYGxRTC","officer_bill_of_rights","unemp_rate","log_murder_rate","log_police_rate","log_arrest_rate","poverty_rate","log_popstate")
colnames(expCoef) <- c("Count_model","Zero_inflation_model")

round(expCoef,
      digits = 4 # rounded to 4 decimals
)



#pol JH
inf_JH_pol <- zeroinfl(JH_pol ~ shall_issue + syg_law + SYGxRTC + officer_bill_of_rights + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate |
                         shall_issue + syg_law + SYGxRTC + officer_bill_of_rights + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate,
                       dist = 'negbin',
                       data = JH_all_crimes_regular_officer_BoR)
summary(inf_JH_pol, digits = 3)

# Dispersion Statistic
inf_JH_pol.res <- resid(inf_JH_pol, type = "pearson")
N  <- nrow(JH_all_crimes_regular_officer_BoR)
p  <- length(coef(inf_JH_pol)) + 1 # '+1' is due to theta
sum(inf_JH_pol.res^2) / (N - p)

## Exponentiated coefficients
expCoef <- exp(coef((inf_JH_pol)))
expCoef <- matrix(expCoef, ncol = 2)
rownames(expCoef) <- c("Intercept", "shall_issue","syg_law","SYGxRTC","officer_bill_of_rights","unemp_rate","log_murder_rate","log_police_rate","log_arrest_rate","poverty_rate","log_popstate")
colnames(expCoef) <- c("Count_model","Zero_inflation_model")

round(expCoef,
      digits = 4 # rounded to 4 decimals
)


# ==============================================================================
# FENB MICERF table tests

#JH cit model with interaction
model_cit_BoR <- JH_cit_imp ~ shall_issue + syg_law + SYGxRTC + officer_bill_of_rights + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_cit_BoR.fenegbin <- fenegbin(model_cit_BoR, data=JH_all_crimes_MICERF_officer_BoR)
summary(model_cit_BoR.fenegbin)

#JH pol model with interaction
model_pol_BoR <- JH_pol_imp ~ shall_issue + syg_law + SYGxRTC + officer_bill_of_rights + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_pol_BoR.fenegbin <- fenegbin(model_pol_BoR, data=JH_all_crimes_MICERF_officer_BoR)
summary(model_pol_BoR.fenegbin)

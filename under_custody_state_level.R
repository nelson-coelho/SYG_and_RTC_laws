# ==============================================================================
# Project: Justifiable Homicides (JH) and SYG and RTC laws
# Version: Article v1
#
# Purpose: This code performs the FENB and ZINB tests for the JH dependent variable
#          on the regular table and MICE RF table as it relates to prisoners in
#          custody
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
# Description: This file contains R code to add the variable of prisoners in
#              custody per state.
#
# Modifications:
#
# Changes:
#
# - 2025-2-08: Initial version by Nelson Coelho
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
#loads database from National Prisoner Statistics, [United States], 1978-2020 (ICPSR 38249)
load_db <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
under_custody_db <- load_db("C:/38249-0001-Data.rda")


#because our variable of interest has differing columns depending on the year, we split our db into two
#then rename the focus variable and rejoin

#removing years not used in study
under_custody_db_1978_1982 <- under_custody_db %>%  filter(under_custody_db$YEAR<='1982')
under_custody_db_1983_2020 <- under_custody_db %>%  filter(under_custody_db$YEAR>'1982')

#keep only needed vars
under_custody_db_1978_1982 <- subset(under_custody_db_1978_1982, select = c(YEAR, STATEID, STATE, CUSTOTM, CUSTOTF, CUSTOTT))
under_custody_db_1983_2020 <- subset(under_custody_db_1983_2020, select = c(YEAR, STATEID, STATE, CUSTOTM, CUSTOTF))

#add total of males and females for later years
under_custody_db_1983_2020$CUSTOTT <- under_custody_db_1983_2020$CUSTOTM + under_custody_db_1983_2020$CUSTOTF


#cleaning and adding a variable that is used to join tables later on
under_custody_db_1978_1982 <- under_custody_db_1978_1982 %>% unite('BREAKln', YEAR, STATE, remove = FALSE)
under_custody_db_1983_2020 <- under_custody_db_1983_2020 %>% unite('BREAKln', YEAR, STATE, remove = FALSE)


#below join tables
under_custody_db_final <- rbind(under_custody_db_1978_1982, under_custody_db_1983_2020)


#removing all duplicates
under_custody_db_final <- under_custody_db_final %>% distinct(BREAKln, .keep_all = TRUE)

#make lower caps all characters in this column
under_custody_db_final$STATEID <- tolower(under_custody_db_final$STATEID)

#clean stateid string
under_custody_db_final$STATEID <- sub(".*. ", "", under_custody_db_final$STATEID)

#create new BREAKln
under_custody_db_final <- under_custody_db_final %>% unite('BREAKln', YEAR, STATEID, remove = FALSE)

#keep only needed vars
under_custody_db_final <- subset(under_custody_db_final, select = c(BREAKln, CUSTOTM, CUSTOTF, CUSTOTT))



#load regular table
JH_all_crimes_regular_officer_BoR <- read_csv("C:/JH_all_crimes_regular_officer_BoR.csv")

#load MICERF table
JH_all_crimes_MICERF_officer_BoR <- read_csv("C:/JH_all_crimes_MICERF_officer_BoR.csv")

#below join tables
JH_all_crimes_regular_custody_pop <- left_join(JH_all_crimes_regular_officer_BoR, under_custody_db_final, by = "BREAKln")
JH_all_crimes_MICERF_custody_pop <- left_join(JH_all_crimes_MICERF_officer_BoR, under_custody_db_final, by = "BREAKln")


#removing all duplicates
JH_all_crimes_regular_custody_pop <- JH_all_crimes_regular_custody_pop %>% distinct(BREAKln, .keep_all = TRUE)
JH_all_crimes_MICERF_custody_pop <- JH_all_crimes_MICERF_custody_pop %>% distinct(BREAKln, .keep_all = TRUE)


#save tables
write.csv(JH_all_crimes_regular_custody_pop, "C:/JH_all_crimes_regular_custody_pop.csv", row.names=FALSE)
write.csv(JH_all_crimes_MICERF_custody_pop, "C:/JH_all_crimes_MICERF_custody_pop.csv", row.names=FALSE)



# ==============================================================================
# FENB regular table tests

#JH cit model with interaction
model_cit_reg <- JH_cit ~ shall_issue + syg_law + officer_bill_of_rights + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate + CUSTOTT | state
model_cit_reg.fenegbin <- fenegbin(model_cit_reg, data=JH_all_crimes_regular_custody_pop)
summary(model_cit_reg.fenegbin)

#JH pol model with interaction
model_pol_reg <- JH_pol ~ shall_issue + syg_law + officer_bill_of_rights + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate + CUSTOTT | state
model_pol_reg.fenegbin <- fenegbin(model_pol_reg, data=JH_all_crimes_regular_custody_pop)
summary(model_pol_reg.fenegbin)


# ==============================================================================
# ZINB regular table tests

#cit JH
inf_JH_cit <- zeroinfl(JH_cit ~ shall_issue + syg_law + SYGxRTC + officer_bill_of_rights + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate + CUSTOTT |
                         shall_issue + syg_law + SYGxRTC + officer_bill_of_rights + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate + CUSTOTT,
                       dist = 'negbin',
                       data = JH_all_crimes_regular_custody_pop)
summary(inf_JH_cit)

# Dispersion Statistic
inf_JH_cit.res <- resid(inf_JH_cit, type = "pearson")
N  <- nrow(JH_all_crimes_regular_custody_pop)
p  <- length(coef(inf_JH_cit)) + 1 # '+1' is due to theta
sum(inf_JH_cit.res^2) / (N - p)


## Exponentiated coefficients
expCoef <- exp(coef((inf_JH_cit)))
expCoef <- matrix(expCoef, ncol = 2)
rownames(expCoef) <- c("Intercept", "shall_issue","syg_law","SYGxRTC","officer_bill_of_rights","unemp_rate","log_murder_rate","log_police_rate","log_arrest_rate","poverty_rate","log_popstate","CUSTOTT")
colnames(expCoef) <- c("Count_model","Zero_inflation_model")

round(expCoef,
      digits = 4 # rounded to 4 decimals
)



#pol JH
inf_JH_pol <- zeroinfl(JH_pol ~ shall_issue + syg_law + SYGxRTC + officer_bill_of_rights + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate + CUSTOTT |
                         shall_issue + syg_law + SYGxRTC + officer_bill_of_rights + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate + CUSTOTT,
                       dist = 'negbin',
                       data = JH_all_crimes_regular_custody_pop)
summary(inf_JH_pol, digits = 3)

# Dispersion Statistic
inf_JH_pol.res <- resid(inf_JH_pol, type = "pearson")
N  <- nrow(JH_all_crimes_regular_custody_pop)
p  <- length(coef(inf_JH_pol)) + 1 # '+1' is due to theta
sum(inf_JH_pol.res^2) / (N - p)

## Exponentiated coefficients
expCoef <- exp(coef((inf_JH_pol)))
expCoef <- matrix(expCoef, ncol = 2)
rownames(expCoef) <- c("Intercept", "shall_issue","syg_law","SYGxRTC","officer_bill_of_rights","unemp_rate","log_murder_rate","log_police_rate","log_arrest_rate","poverty_rate","log_popstate","CUSTOTT")
colnames(expCoef) <- c("Count_model","Zero_inflation_model")

round(expCoef,
      digits = 4 # rounded to 4 decimals
)


# ==============================================================================
# FENB MICERF table tests

#JH cit model with interaction
model_cit_MICERF<- JH_cit_imp ~ shall_issue + syg_law + officer_bill_of_rights + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp + CUSTOTT | state
model_cit_MICERF.fenegbin <- fenegbin(model_cit_MICERF, data=JH_all_crimes_MICERF_custody_pop)
summary(model_cit_MICERF.fenegbin)

#JH pol model with interaction
model_pol_MICERF <- JH_pol_imp ~ shall_issue + syg_law + officer_bill_of_rights + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp + CUSTOTT | state
model_pol_MICERF.fenegbin <- fenegbin(model_pol_MICERF, data=JH_all_crimes_MICERF_custody_pop)
summary(model_pol_MICERF.fenegbin)
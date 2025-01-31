# ==============================================================================
# Project: Justifiable Homicides (JH) and SYG and RTC laws
# Version: Article v1
#
# Purpose: This code performs the FENB tests for all crimes on MICE RF table 
#          as additional outcomes of the SYG and RTC laws.
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
# Date: 2024-1-17
#
# File Name: FENBtest_for_allcrimes_MICERFtable.R
#
# Description: This file contains R code to test the MICE RF imputation
#              table with all crimes.
#
# Modifications:
#
# Changes:
#
# - 2025-1-17: Initial version by Nelson Coelho
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
#load MICERF table
JH_all_crimes_MICERF <- read_csv('C:/JH_all_crimes_MICERF.csv')



#check for duplicates for MICERF table
n_occur <- data.frame(table(JH_all_crimes_MICERF$BREAKln))

#which rows happened more than once
n_occur[n_occur$Freq > 1,]

#list dups
JH_all_crimes_MICERF[JH_all_crimes_MICERF$BREAKln %in% n_occur$Var1[n_occur$Freq > 1],]

#remove dups
JH_all_crimes_MICERF <- JH_all_crimes_MICERF %>% distinct(BREAKln, .keep_all = TRUE)


#create interaction variable
JH_all_crimes_MICERF$SYGxRTC <- JH_all_crimes_MICERF$shall_issue * JH_all_crimes_MICERF$syg_law

#######################
#JH cit murder model
model_murder <- homicide ~ JH_cit_imp + shall_issue + syg_law + unemp_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_murder.fenegbin <- fenegbin(model_murder, data=JH_all_crimes_MICERF)
summary(model_murder.fenegbin)


#murder model robustness check
model_murder_rcheck <- homicide ~ JH_cit_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_murder_rcheck.fenegbin <- fenegbin(model_murder_rcheck, data=JH_all_crimes_MICERF)
summary(model_murder_rcheck.fenegbin)

#######################
#JH pol murder model
model_murder <- homicide ~ JH_pol_imp + shall_issue + syg_law + unemp_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_murder.fenegbin <- fenegbin(model_murder, data=JH_all_crimes_MICERF)
summary(model_murder.fenegbin)


#murder model robustness check
model_murder_rcheck <- homicide ~ JH_pol_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_murder_rcheck.fenegbin <- fenegbin(model_murder_rcheck, data=JH_all_crimes_MICERF)
summary(model_murder_rcheck.fenegbin)

#==========================================================================

#######################
#JH cit burglary model
model_burglary <- burglary ~ JH_cit_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_burglary.fenegbin <- fenegbin(model_burglary, data=JH_all_crimes_MICERF)
summary(model_burglary.fenegbin)


#burglary model robustness check
model_burglary_rcheck <- burglary ~ JH_cit_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_burglary_rcheck.fenegbin <- fenegbin(model_burglary_rcheck, data=JH_all_crimes_MICERF)
summary(model_burglary_rcheck.fenegbin)


#######################
#JH pol burglary model
model_burglary <- burglary ~ JH_pol_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_burglary.fenegbin <- fenegbin(model_burglary, data=JH_all_crimes_MICERF)
summary(model_burglary.fenegbin)


#burglary model robustness check
model_burglary_rcheck <- burglary ~ JH_pol_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_burglary_rcheck.fenegbin <- fenegbin(model_burglary_rcheck, data=JH_all_crimes_MICERF)
summary(model_burglary_rcheck.fenegbin)

#==========================================================================

#######################
#JH cit assault model
model_assault <- aggravated_assault ~ JH_cit_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_assault.fenegbin <- fenegbin(model_assault, data=JH_all_crimes_MICERF)
summary(model_assault.fenegbin)


#assault model robustness check
model_assault_rcheck <- aggravated_assault ~ JH_cit_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_assault_rcheck.fenegbin <- fenegbin(model_assault_rcheck, data=JH_all_crimes_MICERF)
summary(model_assault_rcheck.fenegbin)


#######################
#JH pol assault model
model_assault <- aggravated_assault ~ JH_pol_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_assault.fenegbin <- fenegbin(model_assault, data=JH_all_crimes_MICERF)
summary(model_assault.fenegbin)


#assault model robustness check
model_assault_rcheck <- aggravated_assault ~ JH_pol_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_assault_rcheck.fenegbin <- fenegbin(model_assault_rcheck, data=JH_all_crimes_MICERF)
summary(model_assault_rcheck.fenegbin)


#==========================================================================

#######################
#JH cit rape legacy model
model_rape_legacy <- rape_legacy ~ JH_cit_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_rape_legacy.fenegbin <- fenegbin(model_rape_legacy, data=JH_all_crimes_MICERF)
summary(model_rape_legacy.fenegbin)


#rape legacy model robustness check
model_rape_legacy_rcheck <- rape_legacy ~ JH_cit_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_rape_legacy_rcheck.fenegbin <- fenegbin(model_rape_legacy_rcheck, data=JH_all_crimes_MICERF)
summary(model_rape_legacy_rcheck.fenegbin)


#######################
#JH pol rape legacy model
model_rape_legacy <- rape_legacy ~ JH_pol_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_rape_legacy.fenegbin <- fenegbin(model_rape_legacy, data=JH_all_crimes_MICERF)
summary(model_rape_legacy.fenegbin)


#rape legacy model robustness check
model_rape_legacy_rcheck <- rape_legacy ~ JH_pol_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_rape_legacy_rcheck.fenegbin <- fenegbin(model_rape_legacy_rcheck, data=JH_all_crimes_MICERF)
summary(model_rape_legacy_rcheck.fenegbin)

#==========================================================================

#######################
#JH cit rape revised model
model_rape_revised <- rape_revised ~ JH_cit_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_rape_revised.fenegbin <- fenegbin(model_rape_revised, data=JH_all_crimes_MICERF)
summary(model_rape_revised.fenegbin)


#rape revised model robustness check
model_rape_revised_rcheck <- rape_revised ~ JH_cit_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_rape_revised_rcheck.fenegbin <- fenegbin(model_rape_revised_rcheck, data=JH_all_crimes_MICERF)
summary(model_rape_revised_rcheck.fenegbin)


#######################
#JH pol rape revised model
model_rape_revised <- rape_revised ~ JH_pol_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_rape_revised.fenegbin <- fenegbin(model_rape_revised, data=JH_all_crimes_MICERF)
summary(model_rape_revised.fenegbin)


#rape revised model robustness check
model_rape_revised_rcheck <- rape_revised ~ JH_pol_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_rape_revised_rcheck.fenegbin <- fenegbin(model_rape_revised_rcheck, data=JH_all_crimes_MICERF)
summary(model_rape_revised_rcheck.fenegbin)


#==========================================================================

#######################
#JH cit robbery model
model_robbery <- robbery ~ JH_cit_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_robbery.fenegbin <- fenegbin(model_robbery, data=JH_all_crimes_MICERF)
summary(model_robbery.fenegbin)


#robbery model robustness check
model_robbery_rcheck <- robbery ~ JH_cit_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_robbery_rcheck.fenegbin <- fenegbin(model_robbery_rcheck, data=JH_all_crimes_MICERF)
summary(model_robbery_rcheck.fenegbin)


#######################
#JH pol robbery model
model_robbery <- robbery ~ JH_pol_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_robbery.fenegbin <- fenegbin(model_robbery, data=JH_all_crimes_MICERF)
summary(model_robbery.fenegbin)


#robbery model robustness check
model_robbery_rcheck <- robbery ~ JH_pol_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_robbery_rcheck.fenegbin <- fenegbin(model_robbery_rcheck, data=JH_all_crimes_MICERF)
summary(model_robbery_rcheck.fenegbin)


#==========================================================================

#######################
#JH cit larceny model
model_larceny <- larceny ~ JH_cit_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_larceny.fenegbin <- fenegbin(model_larceny, data=JH_all_crimes_MICERF)
summary(model_larceny.fenegbin)


#larceny model robustness check
model_larceny_rcheck <- larceny ~ JH_cit_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_larceny_rcheck.fenegbin <- fenegbin(model_larceny_rcheck, data=JH_all_crimes_MICERF)
summary(model_larceny_rcheck.fenegbin)


#######################
#JH pol larceny model
model_larceny <- larceny ~ JH_pol_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_larceny.fenegbin <- fenegbin(model_larceny, data=JH_all_crimes_MICERF)
summary(model_larceny.fenegbin)


#larceny model robustness check
model_larceny_rcheck <- larceny ~ JH_pol_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_larceny_rcheck.fenegbin <- fenegbin(model_larceny_rcheck, data=JH_all_crimes_MICERF)
summary(model_larceny_rcheck.fenegbin)

#==========================================================================

#######################
#JH cit vehicle theft model
model_vehicle_theft <- motor_vehicle_theft ~ JH_cit_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_vehicle_theft.fenegbin <- fenegbin(model_vehicle_theft, data=JH_all_crimes_MICERF)
summary(model_vehicle_theft.fenegbin)


#vehicle theft model robustness check
model_vehicle_theft_rcheck <- motor_vehicle_theft ~ JH_cit_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_vehicle_theft_rcheck.fenegbin <- fenegbin(model_vehicle_theft_rcheck, data=JH_all_crimes_MICERF)
summary(model_vehicle_theft_rcheck.fenegbin)


#######################
#JH pol vehicle theft model
model_vehicle_theft <- motor_vehicle_theft ~ JH_pol_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_vehicle_theft.fenegbin <- fenegbin(model_vehicle_theft, data=JH_all_crimes_MICERF)
summary(model_vehicle_theft.fenegbin)


#vehicle theft model robustness check
model_vehicle_theft_rcheck <- motor_vehicle_theft ~ JH_pol_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_vehicle_theft_rcheck.fenegbin <- fenegbin(model_vehicle_theft_rcheck, data=JH_all_crimes_MICERF)
summary(model_vehicle_theft_rcheck.fenegbin)


#==========================================================================

#######################
#JH cit violent crime model
model_violent_crime <- violent_crime ~ JH_cit_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_violent_crime.fenegbin <- fenegbin(model_violent_crime, data=JH_all_crimes_MICERF)
summary(model_violent_crime.fenegbin)


#violent crime model robustness check
model_violent_crime_rcheck <- violent_crime ~ JH_cit_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_violent_crime_rcheck.fenegbin <- fenegbin(model_violent_crime_rcheck, data=JH_all_crimes_MICERF)
summary(model_violent_crime_rcheck.fenegbin)


#######################
#JH pol violent crime model
model_violent_crime <- violent_crime ~ JH_pol_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_violent_crime.fenegbin <- fenegbin(model_violent_crime, data=JH_all_crimes_MICERF)
summary(model_violent_crime.fenegbin)


#violent crime model robustness check
model_violent_crime_rcheck <- violent_crime ~ JH_pol_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_violent_crime_rcheck.fenegbin <- fenegbin(model_violent_crime_rcheck, data=JH_all_crimes_MICERF)
summary(model_violent_crime_rcheck.fenegbin)


#==========================================================================

#######################
#JH cit property crime model
model_property_crime <- property_crime ~ JH_cit_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_property_crime.fenegbin <- fenegbin(model_property_crime, data=JH_all_crimes_MICERF)
summary(model_property_crime.fenegbin)


#property crime model robustness check
model_property_crime_rcheck <- property_crime ~ JH_cit_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_property_crime_rcheck.fenegbin <- fenegbin(model_property_crime_rcheck, data=JH_all_crimes_MICERF)
summary(model_property_crime_rcheck.fenegbin)


#######################
#JH pol property crime model
model_property_crime <- property_crime ~ JH_pol_imp + shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp

#perform regression
model_property_crime.fenegbin <- fenegbin(model_property_crime, data=JH_all_crimes_MICERF)
summary(model_property_crime.fenegbin)


#property crime model robustness check
model_property_crime_rcheck <- property_crime ~ JH_pol_imp + shall_issue + syg_law + SYGxRTC + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp
model_property_crime_rcheck.fenegbin <- fenegbin(model_property_crime_rcheck, data=JH_all_crimes_MICERF)
summary(model_property_crime_rcheck.fenegbin)
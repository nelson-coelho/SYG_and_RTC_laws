# ==============================================================================
# Project: Justifiable Homicides (JH) and SYG and RTC laws
# Version: Article v1
#
# Purpose: This code creates a new table with the 0 for NA imputation
#          method for missing data and joins with covariates from the old table
#          then runs FENB and ZINB tests
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
# File Name: main_table.R
#
# Description: This file contains R code to create the regular table and run tests.
#
# Modifications:
#
# Changes:
#
# - 2024-11-23: Initial version by Nelson Coelho
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

#load kaplan's db
shr_db_JH <- read_dta('C:/shr_1976_2022.dta')


#removing years not used in study
shr_db_JH <- shr_db_JH %>%  filter(shr_db_JH$year<='2020')


################################
######## citizen JH ############
################################
#
# in this section we filter kaplan's database for citizen justifiable homicides (CJH)
#

#create a copy
shr_db_JH1 <- shr_db_JH


JH_db_new_cit <- shr_db_JH1 %>%  filter(shr_db_JH1$offender_1_circumstance=='felon killed by private citizen') #filter by occurrence of CJH
JH_db_new_cit_additional <- JH_db_new_cit %>%  filter(JH_db_new_cit$additional_victim_count > 0) #filter if there were additional victims
JH_db_new_cit_offender_additional <- JH_db_new_cit %>%  filter(JH_db_new_cit$additional_offender_count > 0) #filter if there were additional offenders

#doing the counts of CJH
JH_db_new_cit_ct <- JH_db_new_cit %>%
  group_by(state, year) %>%
  tally()

JH_db_new_cit_ct <- as.data.frame(JH_db_new_cit_ct) #transform into dataframe

#cleaning and adding a variable that is used to join tables later on
JH_db_new_cit_ct <- JH_db_new_cit_ct %>% unite('year_ymo', year, state, remove = FALSE)
JH_db_new_cit_additional <- JH_db_new_cit_additional %>% unite('year_ymo', year, state, remove = FALSE)
JH_db_new_cit_offender_additional <- JH_db_new_cit_offender_additional %>% unite('year_ymo', year, state, remove = FALSE)
JH_db_new_cit_additional <- subset(JH_db_new_cit_additional, select = c(additional_victim_count, year_ymo))
JH_db_new_cit_offender_additional <- subset(JH_db_new_cit_offender_additional, select = c(additional_offender_count, year_ymo))

#below join tables
JH_db_new_cit_ct <- left_join(JH_db_new_cit_ct, JH_db_new_cit_additional, by = "year_ymo")
JH_db_new_cit_ct <- left_join(JH_db_new_cit_ct, JH_db_new_cit_offender_additional, by = "year_ymo")



#below we count the total of offenders and victims
JH_db_new_cit_ct$cit_total_victims <- (JH_db_new_cit_ct$additional_victim_count + JH_db_new_cit_ct$n)
JH_db_new_cit_ct$cit_total_offenders <- (JH_db_new_cit_ct$additional_offender_count + JH_db_new_cit_ct$n)

#simply renaming the column that is used to join tables
JH_db_new_cit_ct <- JH_db_new_cit_ct %>% rename_with( ~"BREAKln", "year_ymo")

#renaming other variables
JH_db_new_cit_ct <- JH_db_new_cit_ct %>% rename_with( ~"cit_JHs_occurrence", "n")
JH_db_new_cit_ct <- JH_db_new_cit_ct %>% rename_with( ~"cit_additional_victim_count", "additional_victim_count")
JH_db_new_cit_ct <- JH_db_new_cit_ct %>% rename_with( ~"cit_additional_offender_count", "additional_offender_count")



################################
########  police JH ############
################################
#
# in this section we filter kaplan's database for police justifiable homicides (PJH)
#

#create a copy
shr_db_JH1 <- shr_db_JH


JH_db_new_pol <- shr_db_JH1 %>%  filter(shr_db_JH1$offender_1_circumstance=='felon killed by police') #filter by occurrence of PJH
JH_db_new_pol_additional <- JH_db_new_pol %>%  filter(JH_db_new_pol$additional_victim_count > 0) #filter if there were additional victims
JH_db_new_pol_offender_additional <- JH_db_new_pol %>%  filter(JH_db_new_pol$additional_offender_count > 0) #filter if there were additional offenders


#doing the counts of PJH
JH_db_new_pol_ct <- JH_db_new_pol %>%
  group_by(state, year) %>%
  tally()


JH_db_new_pol_ct <- as.data.frame(JH_db_new_pol_ct) #transform into dataframe

#cleaning and adding a variable that is used to join tables later on
JH_db_new_pol_ct <- JH_db_new_pol_ct %>% unite('year_ymo', year, state, remove = FALSE)
JH_db_new_pol_additional <- JH_db_new_pol_additional %>% unite('year_ymo', year, state, remove = FALSE)
JH_db_new_pol_offender_additional <- JH_db_new_pol_offender_additional %>% unite('year_ymo', year, state, remove = FALSE)
JH_db_new_pol_additional <- subset(JH_db_new_pol_additional, select = c(additional_victim_count, year_ymo))
JH_db_new_pol_offender_additional <- subset(JH_db_new_pol_offender_additional, select = c(additional_offender_count, year_ymo))

#below join tables
JH_db_new_pol_ct <- left_join(JH_db_new_pol_ct, JH_db_new_pol_additional, by = "year_ymo")
JH_db_new_pol_ct <- left_join(JH_db_new_pol_ct, JH_db_new_pol_offender_additional, by = "year_ymo")


#below we count the total of offenders and victims
JH_db_new_pol_ct$pol_total_victims <- (JH_db_new_pol_ct$additional_victim_count + JH_db_new_pol_ct$n)
JH_db_new_pol_ct$pol_total_offenders <- (JH_db_new_pol_ct$additional_offender_count + JH_db_new_pol_ct$n)

#simply renaming the column that is used to join tables
JH_db_new_pol_ct <- JH_db_new_pol_ct %>% rename_with( ~"BREAKln", "year_ymo")

#renaming other variables
JH_db_new_pol_ct <- JH_db_new_pol_ct %>% rename_with( ~"pol_JHs_occurrence", "n")
JH_db_new_pol_ct <- JH_db_new_pol_ct %>% rename_with( ~"pol_additional_victim_count", "additional_victim_count")
JH_db_new_pol_ct <- JH_db_new_pol_ct %>% rename_with( ~"pol_additional_offender_count", "additional_offender_count")



#####################################
##########   homicides ##############
#####################################
#
# in this section we filter kaplan's database for homicides
#

#create a copy
shr_db_homs_for_JH1 <- shr_db_JH

# here we filter out the occurrences of CJH and PJH
homs_db <- shr_db_homs_for_JH1 %>%  filter(!shr_db_homs_for_JH1$offender_1_circumstance=='felon killed by private citizen' & !shr_db_homs_for_JH1$offender_1_circumstance=='felon killed by police')
# next we filter out the occurrences of CJH and PJH so long as there were additional victims
homs_db_additional <- shr_db_homs_for_JH1 %>%  filter(!shr_db_homs_for_JH1$offender_1_circumstance=='felon killed by private citizen' & !shr_db_homs_for_JH1$offender_1_circumstance=='felon killed by police' & shr_db_homs_for_JH1$additional_victim_count > 0)
# next we filter out the occurrences of CJH and PJH so long as there were additional offenders
homs_db_offender_additional <- shr_db_homs_for_JH1 %>%  filter(!shr_db_homs_for_JH1$offender_1_circumstance=='felon killed by private citizen' & !shr_db_homs_for_JH1$offender_1_circumstance=='felon killed by police' & shr_db_homs_for_JH1$additional_offender_count > 0)


#doing the counts of homicides
homs_db_ct <- homs_db %>%
  group_by(state, year) %>%
  tally()


homs_db_ct <- as.data.frame(homs_db_ct) #transform into dataframe

#cleaning and adding a variable that is used to join tables later on
homs_db_ct <- homs_db_ct %>% unite('year_ymo', year, state, remove = FALSE)
homs_db_additional <- homs_db_additional %>% unite('year_ymo', year, state, remove = FALSE)
homs_db_offender_additional <- homs_db_offender_additional %>% unite('year_ymo', year, state, remove = FALSE)
homs_db_additional <- subset(homs_db_additional, select = c(additional_victim_count, year_ymo))
homs_db_offender_additional <- subset(homs_db_offender_additional, select = c(additional_offender_count, year_ymo))

#below join tables
homs_db_ct <- left_join(homs_db_ct, homs_db_additional, by = "year_ymo")
homs_db_ct <- left_join(homs_db_ct, homs_db_offender_additional, by = "year_ymo")

#below we count the total of offenders and victims
homs_db_ct$homs_total_victims <- (homs_db_ct$additional_victim_count + homs_db_ct$n)
homs_db_ct$homs_total_offenders <- (homs_db_ct$additional_offender_count + homs_db_ct$n)



#simply renaming the column that is used to join tables
homs_db_ct <- homs_db_ct %>% rename_with( ~"BREAKln", "year_ymo")

#removing all duplicates
homs_db_ct <- homs_db_ct %>% distinct(BREAKln, .keep_all = TRUE)

#renaming other variables
homs_db_ct <- homs_db_ct %>% rename_with( ~"homs_occurrence", "n")
homs_db_ct <- homs_db_ct %>% rename_with( ~"homs_additional_victim_count", "additional_victim_count")
homs_db_ct <- homs_db_ct %>% rename_with( ~"homs_additional_offender_count", "additional_offender_count")



################################
#########  total JH ############
################################
#
# in this section we filter kaplan's database for both CJH and PJH
#

#create a copy
shr_db_JH1 <- shr_db_JH

#filter for TJH
JH_db_new <- shr_db_JH1 %>%  filter(shr_db_JH1$offender_1_circumstance=='felon killed by private citizen' | shr_db_JH1$offender_1_circumstance=='felon killed by police')
JH_db_new_additional <- JH_db_new %>%  filter(JH_db_new$additional_victim_count > 0) #filter if there were additional victims
JH_db_new_offender_additional <- JH_db_new %>%  filter(JH_db_new$additional_offender_count > 0) #filter if there were additional offenders


#doing the counts of TJH
JH_db_new_ct <- JH_db_new %>%
  group_by(state, year) %>%
  tally()


JH_db_new_ct <- as.data.frame(JH_db_new_ct) #transform into dataframe

#cleaning and adding a variable that is used to join tables later on
JH_db_new_ct <- JH_db_new_ct %>% unite('year_ymo', year, state, remove = FALSE)
JH_db_new_additional <- JH_db_new_additional %>% unite('year_ymo', year, state, remove = FALSE)
JH_db_new_offender_additional <- JH_db_new_offender_additional %>% unite('year_ymo', year, state, remove = FALSE)
JH_db_new_additional <- subset(JH_db_new_additional, select = c(additional_victim_count, year_ymo))
JH_db_new_offender_additional <- subset(JH_db_new_offender_additional, select = c(additional_offender_count, year_ymo))

#below join tables
JH_db_new_ct <- left_join(JH_db_new_ct, JH_db_new_additional, by = "year_ymo")
JH_db_new_ct <- left_join(JH_db_new_ct, JH_db_new_offender_additional, by = "year_ymo")


#below we count the total of offenders and victims
JH_db_new_ct$tot_JH_total_victims <- (JH_db_new_ct$additional_victim_count + JH_db_new_ct$n)
JH_db_new_ct$tot_JH_total_offenders <- (JH_db_new_ct$additional_offender_count + JH_db_new_ct$n)


#simply renaming the column that is used to join tables
JH_db_new_ct <- JH_db_new_ct %>% rename_with( ~"BREAKln", "year_ymo")

#renaming other variables
JH_db_new_ct <- JH_db_new_ct %>% rename_with( ~"tot_JH_occurrence", "n")
JH_db_new_ct <- JH_db_new_ct %>% rename_with( ~"tot_JH_additional_victim_count", "additional_victim_count")
JH_db_new_ct <- JH_db_new_ct %>% rename_with( ~"tot_JH_additional_offender_count", "additional_offender_count")





#now we add our old DB that consists of Ivans original table and Kaplans as well, with the ACS variables from CENSUS
JH_db <- read_excel('C:/JH_db_new_May.xlsx')

#vars are loaded as char so we transform them into doubles
JH_db$unemp <- as.double(JH_db$unemp)
JH_db$percentPoverty <- as.double(JH_db$percentPoverty)
JH_db$popstate <- as.double(JH_db$popstate)
JH_db$police <- as.double(JH_db$police)

#renaming vars
JH_db <- JH_db %>% rename_with( ~'unemp_rate', 'unemp')
JH_db <- JH_db %>% rename_with( ~'poverty_rate', 'percentPoverty')
JH_db <- JH_db %>% rename_with( ~'shall_issue', 'shalll')
JH_db <- JH_db %>% rename_with( ~'pop_20_39', 'ppx2039')

#calculate police rate per 100,000 population
JH_db$police_rate <- (JH_db$police / JH_db$popstate) * 100000

#make lower caps all characters in this column
JH_db$BREAKln <- tolower(JH_db$BREAKln)

#remove dups
JH_db_new_ct <- JH_db_new_ct %>% distinct(BREAKln, .keep_all = TRUE)
JH_db_new_pol_ct1 <- JH_db_new_pol_ct %>% distinct(BREAKln, .keep_all = TRUE)
JH_db_new_cit_ct1 <- JH_db_new_cit_ct %>% distinct(BREAKln, .keep_all = TRUE)

#remove vars for join
JH_db_new_ct <- subset(JH_db_new_ct, select = -c(state, year))
JH_db_new_pol_ct1 <- subset(JH_db_new_pol_ct1, select = -c(state, year))
JH_db_new_cit_ct1 <- subset(JH_db_new_cit_ct1, select = -c(state, year))


#join homs, CJH, and PJH tables
JH_db_new_ct1 <- left_join(homs_db_ct, JH_db_new_ct, by = "BREAKln")

JH_db_new_ct1 <- left_join(JH_db_new_ct1, JH_db_new_pol_ct1, by = "BREAKln")

JH_db_new_ct1 <- left_join(JH_db_new_ct1, JH_db_new_cit_ct1, by = "BREAKln")


#here we remove states that had missing  or incomplete data for that year
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('1999_alabama', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('2012_alabama', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('2013_alabama', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('2014_alabama', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('2015_alabama', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('2016_alabama', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('2017_alabama', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('2018_alabama', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('2019_alabama', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('2020_alabama', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('1996_florida', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('2019_florida', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('2020_florida', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('1984_illinois', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('1985_illinois', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('1987_illinois', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('1987_kentucky', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('1994_kentucky', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('1981_new mexico', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  filter(!grepl('1987_kentucky', BREAKln))


#remove DC, remove virgin islands, guam
JH_db_new_ct1 <- JH_db_new_ct1 %>% 
  filter(!grepl('district of columbia', state))
JH_db_new_ct1 <- JH_db_new_ct1 %>% 
  filter(!grepl('virgin islands', state))
JH_db_new_ct1 <- JH_db_new_ct1 %>% 
  filter(!grepl('2012_guam', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>% 
  filter(!grepl('2013_guam', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>% 
  filter(!grepl('2014_guam', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>% 
  filter(!grepl('2015_guam', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>% 
  filter(!grepl('2016_guam', BREAKln))
JH_db_new_ct1 <- JH_db_new_ct1 %>% 
  filter(!grepl('2017_guam', BREAKln))


#remove unnecessary vars
JH_db_new_ct1 <- subset(JH_db_new_ct1, select = -c(state, year))



#save a copy
#write.csv(JH_db_new_ct1, "C:/JH_June_dirty.csv", row.names=FALSE)


####
#now we clean the table
####

#keep only necessary vars
JH_db <- subset(JH_db, select = c(BREAKln, popstate, shall_issue, syg_law, unemp_rate, state, year, pop_20_39,
                                  poverty_rate, police, police_rate))

#finally we join with the old table
JH_db_June <- left_join(JH_db, JH_db_new_ct1, by = "BREAKln")


#reshuffle main vars
JH_db_June <- JH_db_June %>%
  relocate(year, .after = BREAKln)
JH_db_June <- JH_db_June %>%
  relocate(state, .after = BREAKln)




# here we get arrest data from FBI API Crime Explorer
# update 10/17/2024: they changed their API call and the layout of the information
# API requests below may no longer work
# we simply retrieved the data we had for arrests from a previous db
Arrests_db_JH <- read_csv('C:/JH_July_cleaner.csv')
Arrests_db_JH <- subset(Arrests_db_JH, select = c(BREAKln, arrests))


# #we create a list of all states
# States <- c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
# 
# #create arrest dataframe
# Arrest_db_JH <- data.frame()
# 
# #for loop that calls FBI Crime Explorer API
# for (i in 1:50) {
#   randname <- fromJSON(paste0("https://api.usa.gov/crime/fbi/cde/arrest/state/"
#                               , as.character(States[i])
#                               , "/all?from=1976&to=2020&API_KEY=6kJ4gIRT29jihAED612EtwkhVreM3OREHRLDwQFJ"))
#   randname$data$state = States[i] #log the name of the state
#   Arrest_db_JH <- rbind(Arrest_db_JH, randname$data) #bind data each iteration
# }
# 
# #sum rows
# Arrest_db_JH <- Arrest_db_JH %>%
#   mutate(arrests = rowSums(select(., where(is.numeric))))
# 
# #relocate var
# Arrest_db_JH <- Arrest_db_JH %>% relocate(arrests, .after = data_year)
# 
# #rename vars
# JH_db <- JH_db %>% rename_with( ~'unemp_rate', 'unemp')
# Arrest_db_JH <- Arrest_db_JH %>% rename_with( ~'year','data_year')
# Arrest_db_JH <- Arrest_db_JH %>% rename_with( ~"murder_arrests","Murder and Nonnegligent Manslaughter")
# 
# #we perform the following to make it easier to join tables
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="AL",'alabama',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="AK",'alaska',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="AZ",'arizona',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="AR",'arkansas',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="CA",'california',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="CO",'colorado',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="CT",'connecticut',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="DE",'delaware',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="FL",'florida',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="GA",'georgia',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="HI",'hawaii',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="ID",'idaho',state))
# 
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="IL",'illinois',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="IN",'indiana',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="IA",'iowa',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="KS",'kansas',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="KY",'kentucky',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="LA",'louisiana',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="ME",'maine',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="MD",'maryland',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="MA",'massachusetts',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="MI",'michigan',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="MN",'minnesota',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="MS",'mississippi',state))
# 
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="MO",'missouri',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="MT",'montana',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="NE",'nebraska',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="NV",'nevada',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="NH",'new hampshire',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="NJ",'new jersey',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="NM",'new mexico',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="NY",'new york',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="NC",'north carolina',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="ND",'north dakota',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="OH",'ohio',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="OK",'oklahoma',state))
# 
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="OR",'oregon',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="PA",'pennsylvania',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="RI",'rhode island',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="SC",'south carolina',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="SD",'south dakota',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="TN",'tennessee',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="TX",'texas',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="UT",'utah',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="VT",'vermont',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="VA",'virginia',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="WA",'washington',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="WV",'west virginia',state))
# 
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="WI",'wisconsin',state))
# Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="WY",'wyoming',state))
# 
# #cleaning
# Arrest_db_JH <- subset(Arrest_db_JH, select = c(year, arrests,murder_arrests,state))
# 
# Arrest_db_JH <- Arrest_db_JH %>% unite('BREAKln', year, state, remove = FALSE) #use this to join tables
# 
# #cleaning
# Arrest_db_JH <- subset(Arrest_db_JH, select = -c(year, state))


#join arrests
JH_db_June <- left_join(JH_db_June, Arrests_db_JH, by = "BREAKln")


JH_db_June$tot_JH_occurrence <- as.double(JH_db_June$tot_JH_occurrence)
JH_db_June$cit_JHs_occurrence <- as.double(JH_db_June$cit_JHs_occurrence)
JH_db_June$pol_JHs_occurrence <- as.double(JH_db_June$pol_JHs_occurrence)

#this is not data imputation (we do this simply so when we add, this NA works as a 0)
JH_db_June <- JH_db_June %>%
  mutate(tot_JH_additional_victim_count = coalesce(tot_JH_additional_victim_count, 0),
         cit_additional_victim_count = coalesce(cit_additional_victim_count, 0),
        pol_additional_victim_count = coalesce(pol_additional_victim_count, 0),
        homs_additional_victim_count = coalesce(homs_additional_victim_count, 0)
  )

#we are doing it my way (explanation on article)
for (i in 1:nrow(JH_db_June)) {
  row <- JH_db_June[i,]
  if (!is.na(row$arrests) & is.na(row$tot_JH_occurrence & !is.na(row$homs_occurrence))) {
    row$tot_JH_occurrence = 0
    row$tot_JH_additional_victim_count = 0
    row$tot_JH_total_victims = 0

    print("i got hhere")
    print(i)
    JH_db_June[i,] <- row
  }
  else {
    print("Hi")
  }
}

for (i in 1:nrow(JH_db_June)) {
  row <- JH_db_June[i,]
  if (!is.na(row$arrests) & is.na(row$cit_JHs_occurrence & !is.na(row$homs_occurrence))) {
    row$cit_JHs_occurrence = 0
    row$cit_additional_victim_count = 0
    row$cit_total_victims = 0

    print("i got hhere")
    print(i)
    JH_db_June[i,] <- row
  }
  else {
    print("Hi")
  }
}

for (i in 1:nrow(JH_db_June)) {
  row <- JH_db_June[i,]
  if (!is.na(row$arrests) & is.na(row$pol_JHs_occurrence & !is.na(row$homs_occurrence))) {
    row$pol_JHs_occurrence = 0
    row$pol_additional_victim_count = 0
    row$pol_total_victims = 0

    print("i got hhere")
    print(i)
    JH_db_June[i,] <- row
  }
  else {
    print("Hi")
  }
}


JH_db_June$tot_JH_total_victims <- JH_db_June$tot_JH_occurrence + JH_db_June$tot_JH_additional_victim_count
JH_db_June$pol_total_victims <- JH_db_June$pol_JHs_occurrence + JH_db_June$pol_additional_victim_count
JH_db_June$cit_total_victims <- JH_db_June$cit_JHs_occurrence + JH_db_June$cit_additional_victim_count
JH_db_June$homs_total_victims <- JH_db_June$homs_occurrence + JH_db_June$homs_additional_victim_count


#write.csv(JH_db_June, "C:/JH_Oct_cleaner.csv", row.names=FALSE)



#we add total of homicides with JH and have the total of homs and JH because JH is in fact a homicide as well
JH_db_June$homs_plus_JH <- JH_db_June$homs_total_victims + JH_db_June$tot_JH_total_victims

#calculate murder rate per 100,000 population
JH_db_June$murder_rate <- ifelse (is.na((JH_db_June$homs_plus_JH / JH_db_June$popstate) * 100000), NA, (JH_db_June$homs_plus_JH / JH_db_June$popstate) * 100000)

#rename and relocate several variables
JH_db_June <- JH_db_June %>% rename_with( ~"JH_cit", "cit_total_victims")
JH_db_June <- JH_db_June %>%
  relocate(JH_cit, .after = year)
JH_db_June <- JH_db_June %>% rename_with( ~"JH_pol", "pol_total_victims")
JH_db_June <- JH_db_June %>%
  relocate(JH_pol, .after = year)
JH_db_June <- JH_db_June %>% rename_with( ~"JH_tot", "tot_JH_total_victims")
JH_db_June <- JH_db_June %>%
  relocate(JH_tot, .after = year)
JH_db_June <- JH_db_June %>%
  relocate(murder_rate, .after = year)


#relocate vars
JH_db_June <- JH_db_June %>%
  relocate(arrests, .after = year)
# JH_db_June <- JH_db_June %>%
#   relocate(murder_arrests, .after = year)
JH_db_June <- JH_db_June %>%
  relocate(homs_plus_JH, .after = year)

#calculate arrest rate per 100,000 population and do logs
JH_db_June$arrest_rate <- ifelse (is.na((JH_db_June$arrests / JH_db_June$popstate) * 100000), NA, (JH_db_June$arrests / JH_db_June$popstate) * 100000)
JH_db_June$log_murder_rate <- log(JH_db_June$murder_rate + 1)
JH_db_June$log_arrest_rate <- log(JH_db_June$arrest_rate + 1)
JH_db_June$log_popstate <- log(JH_db_June$popstate)
JH_db_June$log_police_rate <- log(JH_db_June$police_rate + 1)

#reshuffle
JH_db_June <- JH_db_June %>%
  relocate(arrest_rate, .after = year)
JH_db_June <- JH_db_June %>%
  relocate(log_murder_rate, .after = year)
JH_db_June <- JH_db_June %>%
  relocate(log_arrest_rate, .after = year)


#save copy of clean table with arrests
#write.csv(JH_db_June, "C:/JH_July_cleaner.csv", row.names=FALSE)


#load tables 
JH_db_July <- read_csv('C:/JH_Oct_cleaner.csv')


#######################
##
##
## code below added to retrieve var of black pop rate on 27/09/2024
##
##
########################

#below is done because we needed to retrieve the blackPOPrate var
JH_db_bPOP <- read_csv('C:/JH_db_old_nov.csv')

#make column lower caps
JH_db_bPOP$BREAKln <- tolower(JH_db_bPOP$BREAKln)

#now we clean it
JH_db_bPOP$percentBLACK <- as.double(JH_db_bPOP$percentBLACK)
JH_db_bPOP <- JH_db_bPOP %>% rename_with( ~'blackpop_rate', 'percentBLACK')
JH_db_bPOP <- subset(JH_db_bPOP, select = c(BREAKln, blackpop_rate))

#join these two old tables to retrieve black pop rate var
JH_db_addingBpop1 <- left_join(JH_db_July, JH_db_bPOP, by = "BREAKln")

#copy with new name
JH_db_Sept_0_for_NA <- JH_db_addingBpop1


#######################
##
##
## code above added to retrieve var of black pop rate on 27/09/2024
##
##
########################
#write.csv(JH_db_Sept_0_for_NA, "C:/JH_Oct_usethis.csv", row.names=FALSE)



#######################
##
##
## code below added to retrieve vars from LEOKA on 10/24/2024
##
##
########################



#retrieving officers assaulted or killed in line


leoka_db2020 <- read_dta('C:/leoka_monthly_2020.dta')
leoka_db2019 <- read_dta('C:/leoka_monthly_2019.dta')
leoka_db2018 <- read_dta('C:/leoka_monthly_2018.dta')
leoka_db2017 <- read_dta('C:/leoka_monthly_2017.dta')
leoka_db2016 <- read_dta('C:/leoka_monthly_2016.dta')
leoka_db2015 <- read_dta('C:/leoka_monthly_2015.dta')
leoka_db2014 <- read_dta('C:/leoka_monthly_2014.dta')
leoka_db2013 <- read_dta('C:/leoka_monthly_2013.dta')
leoka_db2012 <- read_dta('C:/leoka_monthly_2012.dta')
leoka_db2011 <- read_dta('C:/leoka_monthly_2011.dta')
leoka_db2010 <- read_dta('C:/leoka_monthly_2010.dta')
leoka_db2009 <- read_dta('C:/leoka_monthly_2009.dta')
leoka_db2008 <- read_dta('C:/leoka_monthly_2008.dta')
leoka_db2007 <- read_dta('C:/leoka_monthly_2007.dta')
leoka_db2006 <- read_dta('C:/leoka_monthly_2006.dta')
leoka_db2005 <- read_dta('C:/leoka_monthly_2005.dta')
leoka_db2004 <- read_dta('C:/leoka_monthly_2004.dta')
leoka_db2003 <- read_dta('C:/leoka_monthly_2003.dta')
leoka_db2002 <- read_dta('C:/leoka_monthly_2002.dta')
leoka_db2001 <- read_dta('C:/leoka_monthly_2001.dta')
leoka_db2000 <- read_dta('C:/leoka_monthly_2000.dta')
leoka_db1999 <- read_dta('C:/leoka_monthly_1999.dta')
leoka_db1998 <- read_dta('C:/leoka_monthly_1998.dta')
leoka_db1997 <- read_dta('C:/leoka_monthly_1997.dta')
leoka_db1996 <- read_dta('C:/leoka_monthly_1996.dta')
leoka_db1995 <- read_dta('C:/leoka_monthly_1995.dta')
leoka_db1994 <- read_dta('C:/leoka_monthly_1994.dta')
leoka_db1993 <- read_dta('C:/leoka_monthly_1993.dta')
leoka_db1992 <- read_dta('C:/leoka_monthly_1992.dta')
leoka_db1991 <- read_dta('C:/leoka_monthly_1991.dta')
leoka_db1990 <- read_dta('C:/leoka_monthly_1990.dta')
leoka_db1989 <- read_dta('C:/leoka_monthly_1989.dta')
leoka_db1988 <- read_dta('C:/leoka_monthly_1988.dta')
leoka_db1987 <- read_dta('C:/leoka_monthly_1987.dta')
leoka_db1986 <- read_dta('C:/leoka_monthly_1986.dta')
leoka_db1985 <- read_dta('C:/leoka_monthly_1985.dta')
leoka_db1984 <- read_dta('C:/leoka_monthly_1984.dta')
leoka_db1983 <- read_dta('C:/leoka_monthly_1983.dta')
leoka_db1982 <- read_dta('C:/leoka_monthly_1982.dta')
leoka_db1981 <- read_dta('C:/leoka_monthly_1981.dta')
leoka_db1980 <- read_dta('C:/leoka_monthly_1980.dta')
leoka_db1979 <- read_dta('C:/leoka_monthly_1979.dta')
leoka_db1978 <- read_dta('C:/leoka_monthly_1978.dta')
leoka_db1977 <- read_dta('C:/leoka_monthly_1977.dta')
leoka_db1976 <- read_dta('C:/leoka_monthly_1976.dta')


#2020 to 2010

leoka_20_to_10 <- rbind(leoka_db2020, leoka_db2019)
leoka_20_to_10 <- rbind(leoka_20_to_10, leoka_db2018)
leoka_20_to_10 <- rbind(leoka_20_to_10, leoka_db2017)
leoka_20_to_10 <- rbind(leoka_20_to_10, leoka_db2016)
leoka_20_to_10 <- rbind(leoka_20_to_10, leoka_db2015)
leoka_20_to_10 <- rbind(leoka_20_to_10, leoka_db2014)
leoka_20_to_10 <- rbind(leoka_20_to_10, leoka_db2013)
leoka_20_to_10 <- rbind(leoka_20_to_10, leoka_db2012)
leoka_20_to_10 <- rbind(leoka_20_to_10, leoka_db2011)
leoka_20_to_10 <- rbind(leoka_20_to_10, leoka_db2010)

leoka_20_to_10_cp <- leoka_20_to_10

#officers killed
leoka_db1_okt_20_to_10 <- leoka_20_to_10 %>%  filter(leoka_20_to_10_cp$officers_killed_total >'0') #filter by occurrence of killed
#officers assaulted
leoka_20_to_10_cp <- leoka_20_to_10
leoka_db1_assaults_total_20_to_10 <- leoka_20_to_10 %>%  filter(leoka_20_to_10_cp$total_assaults_total >'0') #filter by occurrence of assaults
#officers assaulted
leoka_20_to_10_cp <- leoka_20_to_10
leoka_db1_awinjurytotal_20_to_10 <- leoka_20_to_10 %>%  filter(leoka_20_to_10_cp$assaults_with_injury_total >'0') #filter by assaults with injury

#doing the counts of officers killed
leoka_db1_okt_20_to_10 <- leoka_db1_okt_20_to_10 %>%
  group_by(state, year) %>%
  tally()

#doing the counts of officers assaulted total
leoka_db1_assaults_total_20_to_10 <- leoka_db1_assaults_total_20_to_10 %>%
  group_by(state, year) %>%
  tally()

#doing the counts of officers assaulted with injury
leoka_db1_awinjurytotal_20_to_10 <- leoka_db1_awinjurytotal_20_to_10 %>%
  group_by(state, year) %>%
  tally()




#2009 to 1999

leoka_09_to_99 <- rbind(leoka_db2009, leoka_db2008)
leoka_09_to_99 <- rbind(leoka_09_to_99, leoka_db2007)
leoka_09_to_99 <- rbind(leoka_09_to_99, leoka_db2006)
leoka_09_to_99 <- rbind(leoka_09_to_99, leoka_db2005)
leoka_09_to_99 <- rbind(leoka_09_to_99, leoka_db2004)
leoka_09_to_99 <- rbind(leoka_09_to_99, leoka_db2003)
leoka_09_to_99 <- rbind(leoka_09_to_99, leoka_db2002)
leoka_09_to_99 <- rbind(leoka_09_to_99, leoka_db2001)
leoka_09_to_99 <- rbind(leoka_09_to_99, leoka_db2000)
leoka_09_to_99 <- rbind(leoka_09_to_99, leoka_db1999)


leoka_09_to_99_cp <- leoka_09_to_99

#officers killed
leoka_db1_okt_09_to_99 <- leoka_09_to_99 %>%  filter(leoka_09_to_99_cp$officers_killed_total >'0') #filter by occurrence of killed
#officers assaulted
leoka_09_to_99_cp <- leoka_09_to_99
leoka_db1_assaults_total_09_to_99 <- leoka_09_to_99 %>%  filter(leoka_09_to_99_cp$total_assaults_total >'0') #filter by occurrence of assaults
#officers assaulted
leoka_09_to_99_cp <- leoka_09_to_99
leoka_db1_awinjurytotal_09_to_99 <- leoka_09_to_99 %>%  filter(leoka_09_to_99_cp$assaults_with_injury_total >'0') #filter by assaults with injury

#doing the counts of officers killed
leoka_db1_okt_09_to_99 <- leoka_db1_okt_09_to_99 %>%
  group_by(state, year) %>%
  tally()

#doing the counts of officers assaulted total
leoka_db1_assaults_total_09_to_99 <- leoka_db1_assaults_total_09_to_99 %>%
  group_by(state, year) %>%
  tally()

#doing the counts of officers assaulted with injury
leoka_db1_awinjurytotal_09_to_99 <- leoka_db1_awinjurytotal_09_to_99 %>%
  group_by(state, year) %>%
  tally()




#1998 to 1988

leoka_98_to_88 <- rbind(leoka_db1998, leoka_db1997)
leoka_98_to_88 <- rbind(leoka_98_to_88, leoka_db1996)
leoka_98_to_88 <- rbind(leoka_98_to_88, leoka_db1995)
leoka_98_to_88 <- rbind(leoka_98_to_88, leoka_db1994)
leoka_98_to_88 <- rbind(leoka_98_to_88, leoka_db1993)
leoka_98_to_88 <- rbind(leoka_98_to_88, leoka_db1992)
leoka_98_to_88 <- rbind(leoka_98_to_88, leoka_db1991)
leoka_98_to_88 <- rbind(leoka_98_to_88, leoka_db1990)
leoka_98_to_88 <- rbind(leoka_98_to_88, leoka_db1989)
leoka_98_to_88 <- rbind(leoka_98_to_88, leoka_db1988)

leoka_db1_98_to_88 <- leoka_98_to_88

#officers killed
leoka_db1_okt_98_to_88 <- leoka_98_to_88 %>%  filter(leoka_db1_98_to_88$officers_killed_total >'0') #filter by occurrence of killed
#officers assaulted
leoka_db1_98_to_88 <- leoka_98_to_88
leoka_db1_assaults_total_98_to_88 <- leoka_98_to_88 %>%  filter(leoka_db1_98_to_88$total_assaults_total >'0') #filter by occurrence of assaults
#officers assaulted
leoka_db1_98_to_88 <- leoka_98_to_88
leoka_db1_awinjurytotal_98_to_88 <- leoka_98_to_88 %>%  filter(leoka_db1_98_to_88$assaults_with_injury_total >'0') #filter by assaults with injury

#doing the counts of officers killed
leoka_db1_okt_98_to_88 <- leoka_db1_okt_98_to_88 %>%
  group_by(state, year) %>%
  tally()

#doing the counts of officers assaulted total
leoka_db1_assaults_total_98_to_88 <- leoka_db1_assaults_total_98_to_88 %>%
  group_by(state, year) %>%
  tally()

#doing the counts of officers assaulted with injury
leoka_db1_awinjurytotal_98_to_88 <- leoka_db1_awinjurytotal_98_to_88 %>%
  group_by(state, year) %>%
  tally()




#1987 to 1976

leoka_87_to_76 <- rbind(leoka_db1987, leoka_db1986)
leoka_87_to_76 <- rbind(leoka_87_to_76, leoka_db1985)
leoka_87_to_76 <- rbind(leoka_87_to_76, leoka_db1984)
leoka_87_to_76 <- rbind(leoka_87_to_76, leoka_db1983)
leoka_87_to_76 <- rbind(leoka_87_to_76, leoka_db1982)
leoka_87_to_76 <- rbind(leoka_87_to_76, leoka_db1981)
leoka_87_to_76 <- rbind(leoka_87_to_76, leoka_db1980)
leoka_87_to_76 <- rbind(leoka_87_to_76, leoka_db1979)
leoka_87_to_76 <- rbind(leoka_87_to_76, leoka_db1978)
leoka_87_to_76 <- rbind(leoka_87_to_76, leoka_db1977)
leoka_87_to_76 <- rbind(leoka_87_to_76, leoka_db1976)


leoka_db1_87_to_76 <- leoka_87_to_76

#officers killed
leoka_db1_okt_87_to_76 <- leoka_87_to_76 %>%  filter(leoka_db1_87_to_76$officers_killed_total >'0') #filter by occurrence of killed
#officers assaulted
leoka_db1_87_to_76 <- leoka_87_to_76
leoka_db1_assaults_total_87_to_76 <- leoka_87_to_76 %>%  filter(leoka_db1_87_to_76$total_assaults_total >'0') #filter by occurrence of assaults
#officers assaulted
leoka_db1_87_to_76 <- leoka_87_to_76
leoka_db1_awinjurytotal_87_to_76 <- leoka_87_to_76 %>%  filter(leoka_db1_87_to_76$assaults_with_injury_total >'0') #filter by assaults with injury

#doing the counts of officers killed
leoka_db1_okt_87_to_76 <- leoka_db1_okt_87_to_76 %>%
  group_by(state, year) %>%
  tally()

#doing the counts of officers assaulted total
leoka_db1_assaults_total_87_to_76 <- leoka_db1_assaults_total_87_to_76 %>%
  group_by(state, year) %>%
  tally()

#doing the counts of officers assaulted with injury
leoka_db1_awinjurytotal_87_to_76 <- leoka_db1_awinjurytotal_87_to_76 %>%
  group_by(state, year) %>%
  tally()




#bind all iterations

#officers injury
leoka_injury_total <- rbind(leoka_db1_awinjurytotal_87_to_76, leoka_db1_awinjurytotal_98_to_88)
leoka_injury_total <- rbind(leoka_injury_total, leoka_db1_awinjurytotal_09_to_99)
leoka_injury_total <- rbind(leoka_injury_total, leoka_db1_awinjurytotal_20_to_10)

#officers assaulted
leoka_assaulted_total <- rbind(leoka_db1_assaults_total_87_to_76, leoka_db1_assaults_total_98_to_88)
leoka_assaulted_total <- rbind(leoka_assaulted_total, leoka_db1_assaults_total_09_to_99)
leoka_assaulted_total <- rbind(leoka_assaulted_total, leoka_db1_assaults_total_20_to_10)

#officers killed
leoka_killed_total <- rbind(leoka_db1_okt_87_to_76, leoka_db1_okt_98_to_88)
leoka_killed_total <- rbind(leoka_killed_total, leoka_db1_okt_09_to_99)
leoka_killed_total <- rbind(leoka_killed_total, leoka_db1_okt_20_to_10)


#add breakln
leoka_killed_total <- leoka_killed_total %>% unite('BREAKln', year, state, remove = FALSE)
leoka_assaulted_total <- leoka_assaulted_total %>% unite('BREAKln', year, state, remove = FALSE)
leoka_injury_total <- leoka_injury_total %>% unite('BREAKln', year, state, remove = FALSE)

#rename vars
leoka_killed_total <- leoka_killed_total %>% rename_with( ~"leoka_killed", "n")
leoka_assaulted_total <- leoka_assaulted_total %>% rename_with( ~"leoka_assaulted", "n")
leoka_injury_total <- leoka_injury_total %>% rename_with( ~"leoka_injured", "n")


#remove unnecessary vars
leoka_killed_total <- subset(leoka_killed_total, select = -c(state, year))
leoka_assaulted_total <- subset(leoka_assaulted_total, select = -c(state, year))
leoka_injury_total <- subset(leoka_injury_total, select = -c(state, year))


#bind to main table
JH_db_Oct_w_LEOKA <- left_join(JH_db_Sept_0_for_NA, leoka_killed_total, by = "BREAKln")
JH_db_Oct_w_LEOKA <- left_join(JH_db_Oct_w_LEOKA, leoka_assaulted_total, by = "BREAKln")
JH_db_Oct_w_LEOKA <- left_join(JH_db_Oct_w_LEOKA, leoka_injury_total, by = "BREAKln")

#doing the same validation for leoka_killed
#because it is a table of occurrences

for (i in 1:nrow(JH_db_Oct_w_LEOKA)) {
  row <- JH_db_Oct_w_LEOKA[i,]
  if (!is.na(row$arrests) & is.na(row$leoka_killed)) {
    row$leoka_killed = 0
    
    print("i got hhere")
    print(i)
    JH_db_Oct_w_LEOKA[i,] <- row
  }
  else {
    print("Hi")
  }
}


#save new table with leoka data

#creating interaction variable
JH_db_Oct_w_LEOKA$SYGxRTC <- JH_db_Oct_w_LEOKA$shall_issue * JH_db_Oct_w_LEOKA$syg_law


#write.csv(JH_db_Oct_w_LEOKA, "C:/JH_Oct_with_LEOKA.csv", row.names=FALSE)



#load table
JH_db_Oct_w_LEOKA <- read_csv('C:/JH_Oct_with_LEOKA.csv')


#below is created for the graph that shows the yearly growth of the dependent variables
# doing the total sum for all states per year of the dependent variables
sum_JH_tot <- aggregate(JH_tot ~ year, JH_db_Oct_w_LEOKA, sum)
sum_JH_pol <- aggregate(JH_pol ~ year, JH_db_Oct_w_LEOKA, sum)
sum_JH_cit <- aggregate(JH_cit ~ year, JH_db_Oct_w_LEOKA, sum)

#renaming variables
sum_JH_tot <- sum_JH_tot %>% rename_with( ~"sum_JH_tot", "JH_tot")
sum_JH_pol <- sum_JH_pol %>% rename_with( ~"sum_JH_pol", "JH_pol")
sum_JH_cit <- sum_JH_cit %>% rename_with( ~"sum_JH_cit", "JH_cit")


#year list
JH_graph_data <- seq(1976,2020,1)
#create database
JH_graph_data <- data.frame(JH_graph_data)

#rename year column
JH_graph_data <- JH_graph_data %>% rename_with( ~"year", "JH_graph_data")


#merge with original db
JH_graph_data <- left_join(JH_graph_data, sum_JH_tot, by = "year")
JH_graph_data <- left_join(JH_graph_data, sum_JH_pol, by = "year")
JH_graph_data <- left_join(JH_graph_data, sum_JH_cit, by = "year")


JH_plot <- ggplot(JH_graph_data, aes(x=year)) + 
  geom_line(aes(y=sum_JH_tot, col="Total"), size=1.0) +
  geom_line(aes(y=sum_JH_cit, col="Civilian"), size=1.0) +
  geom_line(aes(y=sum_JH_pol, col="Police"), size=1.0) + 
  
  labs(y="count") +  # title and caption
  scale_color_manual(name="", 
                     values = c("Total"="red", "Civilian"="orange", "Police"="blue")) +  # line color
  theme() +
  
  scale_y_continuous(breaks=seq(0,1000,200), limits = c(0, 1000)) +
  scale_x_continuous(breaks=seq(1976,2020,2), expand = expansion(add = c(0.5,0.5))) +
  
  # Customize the theme
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 20)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20)),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),   
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="black" ), 
    # remove the vertical grid lines
    panel.grid = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # Add titles and caption
  labs(
    title = "Justifiable Homicides 1976-2020",
    #  subtitle = "Year of adoption by state",
    #  caption = "DR = Duty to Retreat / CD = Castle Doctrine \n\n Note: States marked DR or CD have not adopted Stand Your Ground laws"
  )
JH_plot

ggsave("C:/JustifiableHoms_YearlyEvolution.pdf", width = 12, height = 10, dpi = 300)


#rename
JH_graph_data <- JH_graph_data %>% rename_with( ~"total_pjh", "sum_JH_pol")
JH_graph_data <- JH_graph_data %>% rename_with( ~"total_cjh", "sum_JH_cit")


#keep only necessary vars
JH_graph_data <- subset(JH_graph_data, select = c(year, total_cjh, total_pjh))

#save graph data
write.csv(JH_graph_data, "C:/JH_graph_data.csv", row.names=FALSE)





#dataframe for the table with CJH all years and all states evolution
CJH_table_by_state <- subset(JH_db_Oct_w_LEOKA, select = c(state, JH_cit, year))
write.csv(CJH_table_by_state, "C:/JH_table_data.csv", row.names=FALSE)

CJH_table_by_state <- read_excel('C:/CJH_table_by_state.xlsx')

#making a tab
CJH_table_by_state %>%
  kable(caption = 'Citizen Justifiable Homicides') %>%
  kable_styling(bootstrap_options = "bordered",
                full_width = FALSE) %>%
  collapse_rows(columns = 1,
                valign = "middle") %>%
  save_kable("C:/Citizen_JH_table.html")

#webshot2::webshot("C:/Citizen_JH_table.html", "C:/Citizen_JH_table.jpeg")

#dataframe for the table with PJH all years and all states evolution
PJH_table_by_state <- subset(JH_db_Oct_w_LEOKA, select = c(state, JH_pol, year))
write.csv(PJH_table_by_state, "C:/PJH_table_data.csv", row.names=FALSE)

PJH_table_by_state <- read_excel('C:/PJH_table_by_state.xlsx')

#making a tab
PJH_table_by_state %>%
  kable(caption = 'Police Justifiable Homicides') %>%
  kable_styling(bootstrap_options = "bordered",
                full_width = FALSE) %>%
  collapse_rows(columns = 1,
                valign = "middle") %>% 
  save_kable("C:/Police_JH_table.html")



######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
################################                                            ##########################
################################    below we get descriptive statistics     ##########################
################################          and run FENB and ZINB             ##########################
################################                                            ##########################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################

#check for duplicates for regular table
n_occur <- data.frame(table(JH_db_Oct_w_LEOKA$BREAKln))

#which rows happened more than once
n_occur[n_occur$Freq > 1,]

#list dups
JH_db_Oct_w_LEOKA[JH_db_Oct_w_LEOKA$BREAKln %in% n_occur$Var1[n_occur$Freq > 1],]

#descriptive stats

#descriptive stats for CJH
Hmisc::describe(JH_db_Oct_w_LEOKA$JH_cit)
stat.desc(JH_db_Oct_w_LEOKA$JH_cit)
#percentage of zeroes for JH cit
sumCol <- JH_db_Oct_w_LEOKA$JH_cit
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for PJH
Hmisc::describe(JH_db_Oct_w_LEOKA$JH_pol)
stat.desc(JH_db_Oct_w_LEOKA$JH_pol)
#percentage of zeroes for JH pol
sumCol <- JH_db_Oct_w_LEOKA$JH_pol
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for TJH
Hmisc::describe(JH_db_Oct_w_LEOKA$JH_tot)
stat.desc(JH_db_Oct_w_LEOKA$JH_tot)
#percentage of zeroes for JH tot
sumCol <- JH_db_Oct_w_LEOKA$JH_tot
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for black population rate
Hmisc::describe(JH_db_Oct_w_LEOKA$blackpop_rate)
stat.desc(JH_db_Oct_w_LEOKA$blackpop_rate)
#percentage of zeroes for black population rate
sumCol <- JH_db_Oct_w_LEOKA$blackpop_rate
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for log of murder rate
Hmisc::describe(JH_db_Oct_w_LEOKA$log_murder_rate)
stat.desc(JH_db_Oct_w_LEOKA$log_murder_rate)
#percentage of zeroes for log of murder rate
sumCol <- JH_db_Oct_w_LEOKA$log_murder_rate
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for population from 20 through 39 years
Hmisc::describe(JH_db_Oct_w_LEOKA$pop_20_39)
stat.desc(JH_db_Oct_w_LEOKA$pop_20_39)
#percentage of zeroes for population from 20 through 39 years
sumCol <- JH_db_Oct_w_LEOKA$pop_20_39
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for log of state population
Hmisc::describe(JH_db_Oct_w_LEOKA$log_popstate)
stat.desc(JH_db_Oct_w_LEOKA$log_popstate)
#percentage of zeroes for log of state population
sumCol <- JH_db_Oct_w_LEOKA$log_popstate
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for log of arrest rate
Hmisc::describe(JH_db_Oct_w_LEOKA$log_arrest_rate)
stat.desc(JH_db_Oct_w_LEOKA$log_arrest_rate)
#percentage of zeroes for log of arrest rate
sumCol <- JH_db_Oct_w_LEOKA$log_arrest_rate
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for log of police rate
Hmisc::describe(JH_db_Oct_w_LEOKA$log_police_rate)
stat.desc(JH_db_Oct_w_LEOKA$log_police_rate)
#percentage of zeroes for log of police rate
sumCol <- JH_db_Oct_w_LEOKA$log_police_rate
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for poverty rate
Hmisc::describe(JH_db_Oct_w_LEOKA$poverty_rate)
stat.desc(JH_db_Oct_w_LEOKA$poverty_rate)
#percentage of zeroes for poverty rate
sumCol <- JH_db_Oct_w_LEOKA$poverty_rate
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for unemployment rate
Hmisc::describe(JH_db_Oct_w_LEOKA$unemp_rate)
stat.desc(JH_db_Oct_w_LEOKA$unemp_rate)
#percentage of zeroes for unemployment rate
sumCol <- JH_db_Oct_w_LEOKA$unemp_rate
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for SYG LAW
Hmisc::describe(JH_db_Oct_w_LEOKA$syg_law)
stat.desc(JH_db_Oct_w_LEOKA$syg_law)
#percentage of zeroes for SYG LAW
sumCol <- JH_db_Oct_w_LEOKA$syg_law
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for RTC LAW
Hmisc::describe(JH_db_Oct_w_LEOKA$shall_issue)
stat.desc(JH_db_Oct_w_LEOKA$shall_issue)
#percentage of zeroes for RTC LAW
sumCol <- JH_db_Oct_w_LEOKA$shall_issue
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res



#here we create a correlation table
covariates <- subset(JH_db_Oct_w_LEOKA, select = c(JH_cit, JH_pol, JH_tot, shall_issue, syg_law, unemp_rate, poverty_rate, log_arrest_rate, log_police_rate, log_murder_rate, log_popstate, pop_20_39))

round(cor(covariates,  use = "complete.obs"),
      digits = 2 # rounded to 2 decimals
)

#here we create a the policy interaction correlation table
covariates <- subset(JH_db_Oct_w_LEOKA, select = c(shall_issue, syg_law, SYGxRTC))

round(cor(covariates,  use = "complete.obs"),
      digits = 2 # rounded to 2 decimals
)

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
model_cit <- JH_cit ~ shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate | state
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
model_cit_test <- JH_cit ~ shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate + pop_20_39 | state
model_cit_test.fenegbin <- fenegbin(model_cit_test, data=JH_db_Oct_w_LEOKA)
summary(model_cit_test.fenegbin)


#######################
#police model
model_pol <- JH_pol ~ shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate | state
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
model_pol_test <- JH_pol ~ shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate + pop_20_39 | state
model_pol_test.fenegbin <- fenegbin(model_pol_test, data=JH_db_Oct_w_LEOKA)
summary(model_pol_test.fenegbin)



#######################
#total model
model_tot <- JH_tot ~ shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate | state
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
model_tot_test <- JH_tot ~ shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate + pop_20_39 | state
model_tot_test.fenegbin <- fenegbin(model_tot_test, data=JH_db_Oct_w_LEOKA)
summary(model_tot_test.fenegbin)








#zero inflated Neg Bin

#tot JH
inf_JH_tot <- zeroinfl(JH_tot ~ shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate |
                         shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate,
                       dist = 'negbin',
                       data = JH_db_Oct_w_LEOKA)
summary(inf_JH_tot)

# Dispersion Statistic
inf_JH_tot.res <- resid(inf_JH_tot, type = "pearson")
N  <- nrow(JH_db_Oct_w_LEOKA)
p  <- length(coef(inf_JH_tot)) + 1 # '+1' is due to theta
sum(inf_JH_tot.res^2) / (N - p)

## Exponentiated coefficients
expCoef <- exp(coef((inf_JH_tot)))
expCoef <- matrix(expCoef, ncol = 2)
rownames(expCoef) <- c("Intercept", "shall_issue","syg_law","unemp_rate","log_murder_rate","log_police_rate","log_arrest_rate","poverty_rate","log_popstate")
colnames(expCoef) <- c("Count_model","Zero_inflation_model")

round(expCoef,
      digits = 4 # rounded to 4 decimals
)


#cit JH
inf_JH_cit <- zeroinfl(JH_cit ~ shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate |
                         shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate,
                       dist = 'negbin',
                       data = JH_db_Oct_w_LEOKA)
summary(inf_JH_cit)

# Dispersion Statistic
inf_JH_cit.res <- resid(inf_JH_cit, type = "pearson")
N  <- nrow(JH_db_Oct_w_LEOKA)
p  <- length(coef(inf_JH_cit)) + 1 # '+1' is due to theta
sum(inf_JH_cit.res^2) / (N - p)


## Exponentiated coefficients
expCoef <- exp(coef((inf_JH_cit)))
expCoef <- matrix(expCoef, ncol = 2)
rownames(expCoef) <- c("Intercept", "shall_issue","syg_law","unemp_rate","log_murder_rate","log_police_rate","log_arrest_rate","poverty_rate","log_popstate")
colnames(expCoef) <- c("Count_model","Zero_inflation_model")

round(expCoef,
      digits = 4 # rounded to 4 decimals
)



#pol JH
inf_JH_pol <- zeroinfl(JH_pol ~ shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate|
                         shall_issue + syg_law + unemp_rate + log_murder_rate + log_police_rate + log_arrest_rate + poverty_rate + log_popstate,
                       dist = 'negbin',
                       data = JH_db_Oct_w_LEOKA)
summary(inf_JH_pol, digits = 3)

# Dispersion Statistic
inf_JH_pol.res <- resid(inf_JH_pol, type = "pearson")
N  <- nrow(JH_db_Oct_w_LEOKA)
p  <- length(coef(inf_JH_pol)) + 1 # '+1' is due to theta
sum(inf_JH_pol.res^2) / (N - p)

## Exponentiated coefficients
expCoef <- exp(coef((inf_JH_pol)))
expCoef <- matrix(expCoef, ncol = 2)
rownames(expCoef) <- c("Intercept", "shall_issue","syg_law","unemp_rate","log_murder_rate","log_police_rate","log_arrest_rate","poverty_rate","log_popstate")
colnames(expCoef) <- c("Count_model","Zero_inflation_model")

round(expCoef,
      digits = 4 # rounded to 4 decimals
)



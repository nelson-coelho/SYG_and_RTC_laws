# this script creates a new table with the MICE Random Forest imputation method for missing data
# and joins with covariates from the old table then runs NB tests

#load kaplan's db
shr_db_JH <- read_dta('C:/Users/sarah/Desktop/lookinto/CEOE/shr_1976_2022.dta')


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

#below we make perform the imputation of "0" for "NA", because they are not real NAs, 
#given that Kaplan's db is a table of occurrences and our table has all periods
JH_db_new_cit_ct <- JH_db_new_cit_ct %>%
  mutate(additional_offender_count = coalesce(additional_offender_count, 0),
         additional_victim_count = coalesce(additional_victim_count, 0)
  )


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

JH_db_new_pol_ct <- as.data.frame(JH_db_new_pol_ct)  #transform into dataframe

#cleaning and adding a variable that is used to join tables later on
JH_db_new_pol_ct <- JH_db_new_pol_ct %>% unite('year_ymo', year, state, remove = FALSE)
JH_db_new_pol_additional <- JH_db_new_pol_additional %>% unite('year_ymo', year, state, remove = FALSE)
JH_db_new_pol_offender_additional <- JH_db_new_pol_offender_additional %>% unite('year_ymo', year, state, remove = FALSE)
JH_db_new_pol_additional <- subset(JH_db_new_pol_additional, select = c(additional_victim_count, year_ymo))
JH_db_new_pol_offender_additional <- subset(JH_db_new_pol_offender_additional, select = c(additional_offender_count, year_ymo))

#below join tables
JH_db_new_pol_ct <- left_join(JH_db_new_pol_ct, JH_db_new_pol_additional, by = "year_ymo")
JH_db_new_pol_ct <- left_join(JH_db_new_pol_ct, JH_db_new_pol_offender_additional, by = "year_ymo")

#below we make perform the imputation of "0" for "NA", because they are not real NAs, 
#given that Kaplan's db is a table of occurrences and our table has all periods
JH_db_new_pol_ct <- JH_db_new_pol_ct %>%
  mutate(additional_offender_count = coalesce(additional_offender_count, 0),
         additional_victim_count = coalesce(additional_victim_count, 0)
  )

#below we count the total of offenders and victims
JH_db_new_pol_ct$pol_total_victims <- (JH_db_new_pol_ct$additional_victim_count + JH_db_new_pol_ct$n)
JH_db_new_pol_ct$pol_total_offenders <- (JH_db_new_pol_ct$additional_offender_count + JH_db_new_pol_ct$n)

#simply renaming the column that is used to join tables
JH_db_new_pol_ct <- JH_db_new_pol_ct %>% rename_with( ~"BREAKln", "year_ymo")

#renaming other variables
JH_db_new_pol_ct <- JH_db_new_pol_ct %>% rename_with( ~"pol_JHs_occurrence", "n")
JH_db_new_pol_ct <- JH_db_new_pol_ct %>% rename_with( ~"pol_additional_victim_count", "additional_victim_count")
JH_db_new_pol_ct <- JH_db_new_pol_ct %>% rename_with( ~"pol_additional_offender_count", "additional_offender_count")



################################
##########   homs ##############
################################
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

#below we make perform the imputation of "0" for "NA", because they are not real NAs, 
#given that Kaplan's db is a table of occurrences and our table has all periods
homs_db_ct <- homs_db_ct %>%
  mutate(additional_offender_count = coalesce(additional_offender_count, 0),
         additional_victim_count = coalesce(additional_victim_count, 0)
  )

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

#below we make perform the imputation of "0" for "NA", because they are not real NAs, 
#given that Kaplan's db is a table of occurrences and our table has all periods
JH_db_new_ct <- JH_db_new_ct %>%
  mutate(additional_offender_count = coalesce(additional_offender_count, 0),
         additional_victim_count = coalesce(additional_victim_count, 0)
  )

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
JH_db <- read_excel('C:/Users/sarah/Desktop/lookinto/CEOE/JH_db_new_May.xlsx')

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

#keep only necessary vars
JH_db <- subset(JH_db, select = c(BREAKln, popstate, shall_issue, syg_law, unemp_rate, state, year, pop_20_39,
                                  poverty_rate, police))

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


#join tables
JH_db_new_ct1 <- left_join(homs_db_ct, JH_db_new_ct, by = "BREAKln")
JH_db_new_ct1 <- left_join(JH_db_new_ct1, JH_db_new_pol_ct1, by = "BREAKln")
JH_db_new_ct1 <- left_join(JH_db_new_ct1, JH_db_new_cit_ct1, by = "BREAKln")



# here we get arrest data from FBI API Crime Explorer
# update 10/17/2024: they changed their API call and the layout of the information
# API requests below may no longer work

#we create a list of all states
States <- c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')

#create arrest dataframe
Arrest_db_JH <- data.frame()

#for loop that calls FBI Crime Explorer API
for (i in 1:50) {
  randname <- fromJSON(paste0("https://api.usa.gov/crime/fbi/cde/arrest/state/"
                              , as.character(States[i])
                              , "/all?from=1976&to=2020&API_KEY=6kJ4gIRT29jihAED612EtwkhVreM3OREHRLDwQFJ"))
  randname$data$state = States[i] #log the name of the state
  Arrest_db_JH <- rbind(Arrest_db_JH, randname$data) #bind data each iteration
}

#sum rows
Arrest_db_JH <- Arrest_db_JH %>%
  mutate(arrests = rowSums(select(., where(is.numeric))))

#relocate var
Arrest_db_JH <- Arrest_db_JH %>% relocate(arrests, .after = data_year)

#rename vars
Arrest_db_JH <- Arrest_db_JH %>% rename_with( ~'year','data_year')
Arrest_db_JH <- Arrest_db_JH %>% rename_with( ~"murder_arrests","Murder and Nonnegligent Manslaughter")

#we perform the following to make it easier to join tables
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="AL",'alabama',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="AK",'alaska',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="AZ",'arizona',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="AR",'arkansas',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="CA",'california',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="CO",'colorado',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="CT",'connecticut',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="DE",'delaware',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="FL",'florida',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="GA",'georgia',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="HI",'hawaii',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="ID",'idaho',state))

Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="IL",'illinois',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="IN",'indiana',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="IA",'iowa',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="KS",'kansas',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="KY",'kentucky',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="LA",'louisiana',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="ME",'maine',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="MD",'maryland',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="MA",'massachusetts',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="MI",'michigan',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="MN",'minnesota',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="MS",'mississippi',state))

Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="MO",'missouri',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="MT",'montana',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="NE",'nebraska',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="NV",'nevada',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="NH",'new hampshire',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="NJ",'new jersey',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="NM",'new mexico',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="NY",'new york',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="NC",'north carolina',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="ND",'north dakota',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="OH",'ohio',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="OK",'oklahoma',state))

Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="OR",'oregon',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="PA",'pennsylvania',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="RI",'rhode island',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="SC",'south carolina',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="SD",'south dakota',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="TN",'tennessee',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="TX",'texas',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="UT",'utah',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="VT",'vermont',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="VA",'virginia',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="WA",'washington',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="WV",'west virginia',state))

Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="WI",'wisconsin',state))
Arrest_db_JH <-Arrest_db_JH %>% mutate(state=ifelse(state=="WY",'wyoming',state))

#cleaning
Arrest_db_JH <- subset(Arrest_db_JH, select = c(year, arrests,murder_arrests,state))

Arrest_db_JH <- Arrest_db_JH %>% unite('BREAKln', year, state, remove = FALSE) #use this to join tables

#cleaning
Arrest_db_JH <- subset(Arrest_db_JH, select = -c(year, state))



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


#cleaning
JH_db <- subset(JH_db, select = -c(state, year))



#finally we join with the old table
JH_db_new_ct1 <- left_join(JH_db, JH_db_new_ct1, by = "BREAKln")


#join arrests
JH_db_new_ct1 <- left_join(JH_db_new_ct1, Arrest_db_JH, by = "BREAKln")

#relocate var
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  mutate(newstate = str_sub(BREAKln, 6, -1))

#relocate var
JH_db_new_ct1 <- JH_db_new_ct1 %>%
  mutate(newyear = str_sub(BREAKln, 1, 4))



#################################################################
#
#
#
#
###   code below is done to run mice RF imputation
####
#####

#we do this to make it easier to separate vars for mice
JH_db_new_ct1$state1 <- JH_db_new_ct1$newstate

#we perform the following to make it easier to join tables
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="alabama",'1',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="alaska",'2',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="arizona",'3',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="arkansas",'4',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="california",'5',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="colorado",'6',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="connecticut",'7',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="delaware",'8',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="florida",'9',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="georgia",'10',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="hawaii",'11',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="idaho",'12',state1))

JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="illinois",'13',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="indiana",'14',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="iowa",'15',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="kansas",'16',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="kentucky",'17',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="louisiana",'18',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="maine",'19',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="maryland",'20',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="massachusetts",'21',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="michigan",'22',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="minnesota",'23',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="mississippi",'24',state1))

JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="missouri",'25',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="montana",'26',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="nebraska",'27',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="nevada",'28',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="new hampshire",'29',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="new jersey",'30',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="new mexico",'31',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="new york",'32',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="north carolina",'33',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="north dakota",'34',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="ohio",'35',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="oklahoma",'36',state1))

JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="oregon",'37',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="pennsylvania",'38',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="rhode island",'39',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="south carolina",'40',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="south dakota",'41',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="tennessee",'42',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="texas",'43',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="utah",'44',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="vermont",'45',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="virginia",'46',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="washington",'47',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="west virginia",'48',state1))

JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="wisconsin",'49',state1))
JH_db_new_ct1 <-JH_db_new_ct1 %>% mutate(state1=ifelse(state1=="wyoming",'50',state1))





#we decided to separate vars with missing data, run mice RF, then bring it back
#first we create breakln (var used to join tables) and clean
JH_db_new_ct1 <- JH_db_new_ct1 %>% unite('BREAKln_y_state', newyear, state1, remove = FALSE)
JH_db_new_ct1$BREAKln_y_state <- sub("_", "", JH_db_new_ct1$BREAKln_y_state)
JH_db_new_ct1$BREAKln_y_state <- as.double(JH_db_new_ct1$BREAKln_y_state)


#separating
forest_db <- subset(JH_db_new_ct1, select = c(BREAKln_y_state, 
                                              tot_JH_total_victims,
                                              cit_total_victims,
                                              pol_total_victims,
                                              arrests, 
                                              police, poverty_rate, homs_total_victims, unemp_rate, popstate, pop_20_39))



# running mice
mice_imp <- complete(mice(forest_db,                                 # Predictive mean matching imputation
                          m = 25, maxit = 50,
                          method = "rf"))
head(mice_imp)


#renaming vars
mice_imp <- mice_imp %>% rename_with( ~"tot_JH_total_victims_imp", "tot_JH_total_victims")
mice_imp <- mice_imp %>% rename_with( ~"cit_total_victims_imp", "cit_total_victims")
mice_imp <- mice_imp %>% rename_with( ~"pol_total_victims_imp", "pol_total_victims")

#renaming vars
mice_imp <- mice_imp %>% rename_with( ~"poverty_rate_imp", "poverty_rate")
mice_imp <- mice_imp %>% rename_with( ~"police_imp", "police")
mice_imp <- mice_imp %>% rename_with( ~"arrests_imp", "arrests")
mice_imp <- mice_imp %>% rename_with( ~"homs_total_victims_imp", "homs_total_victims")
mice_imp <- mice_imp %>% rename_with( ~"unemp_rate_imp", "unemp_rate")
mice_imp <- mice_imp %>% rename_with( ~"popstate_imp", "popstate")
mice_imp <- mice_imp %>% rename_with( ~"pop_20_39_imp", "pop_20_39")

#join back again
JH_db_new_ct1 <- left_join(JH_db_new_ct1, mice_imp, by = "BREAKln_y_state")


#remove unnecessary vars
JH_db_new_ct1 <- subset(JH_db_new_ct1, select = -c(state1, BREAKln_y_state))


#save a copy
#write.csv(JH_db_new_ct1, "C:/Users/sarah/Desktop/lookinto/CEOE/_official_tables/JH_June_RFimput_dirty.csv", row.names=FALSE)

####
#now we clean the table
####

#create copy with new name
JH_db_June <- JH_db_new_ct1

#reshuffle main vars
JH_db_June <- JH_db_June %>%
  relocate(year, .after = BREAKln)
JH_db_June <- JH_db_June %>%
  relocate(state, .after = BREAKln)




#we add total of homicides with JH and have the total of homs and JH because JH is in fact a homicide as well
JH_db_June$homs_plus_JH_imp <- JH_db_June$homs_total_victims_imp + JH_db_June$tot_JH_total_victims_imp

#calculate murder rate per 100,000 population
JH_db_June$murder_rate_imp <- ifelse (is.na((JH_db_June$homs_plus_JH_imp / JH_db_June$popstate_imp) * 100000), NA, (JH_db_June$homs_plus_JH_imp / JH_db_June$popstate_imp) * 100000)

#calculate police rate per 100,000 population
JH_db_June$police_rate_imp <- (JH_db_June$police_imp / JH_db_June$popstate_imp) * 100000


#rename and relocate several variables
JH_db_June <- JH_db_June %>% rename_with( ~"JH_cit_imp", "cit_total_victims_imp")
JH_db_June <- JH_db_June %>%
  relocate(JH_cit_imp, .after = year)
JH_db_June <- JH_db_June %>% rename_with( ~"JH_pol_imp", "pol_total_victims_imp")
JH_db_June <- JH_db_June %>%
  relocate(JH_pol_imp, .after = year)
JH_db_June <- JH_db_June %>% rename_with( ~"JH_tot_imp", "tot_JH_total_victims_imp")
JH_db_June <- JH_db_June %>%
  relocate(JH_tot_imp, .after = year)
JH_db_June <- JH_db_June %>%
  relocate(murder_rate_imp, .after = year)
JH_db_June <- JH_db_June %>%
  relocate(arrests_imp, .after = year)
JH_db_June <- JH_db_June %>%
  relocate(homs_plus_JH_imp, .after = year)

#calculate arrest rate per 100,000 population and do logs
JH_db_June$arrest_rate_imp <- ifelse (is.na((JH_db_June$arrests_imp / JH_db_June$popstate_imp) * 100000), NA, (JH_db_June$arrests_imp / JH_db_June$popstate_imp) * 100000)
JH_db_June$log_murder_rate_imp <- log(JH_db_June$murder_rate_imp + 1)
JH_db_June$log_arrest_rate_imp <- log(JH_db_June$arrest_rate_imp + 1)
JH_db_June$log_popstate_imp <- log(JH_db_June$popstate_imp)
JH_db_June$log_police_rate_imp <- log(JH_db_June$police_rate_imp + 1)

#reshuffle
JH_db_June <- JH_db_June %>%
  relocate(arrest_rate_imp, .after = year)
JH_db_June <- JH_db_June %>%
  relocate(log_murder_rate_imp, .after = year)
JH_db_June <- JH_db_June %>%
  relocate(log_arrest_rate_imp, .after = year)
view(JH_db_June)
table(JH_db_June$newstate)

#save copy of clean table with arrests
#write.csv(JH_db_June, "C:/Users/sarah/Desktop/lookinto/CEOE/_official_tables/JH_July_RFimput_cleaner.csv", row.names=FALSE)


#load tables created with MICE RF algorithm
JH_db_July <- read_csv('C:/Users/sarah/Desktop/lookinto/CEOE/_official_tables/JH_July_RFimput_cleaner.csv')




#########################
#########
#########
######### code below added to retrieve var of black pop rate on 27/09/2024
#########
#########
#############################
#below is done because we needed to retrieve the blackPOPrate var, and then we run mice again just for this one var
JH_db_bPOP <- read_csv('C:/Users/sarah/Desktop/lookinto/CEOE/JH_db_old_nov.csv')

#make column lower caps
JH_db_bPOP$BREAKln <- tolower(JH_db_bPOP$BREAKln)

#now we clean it
JH_db_bPOP$percentBLACK <- as.double(JH_db_bPOP$percentBLACK)
JH_db_bPOP <- JH_db_bPOP %>% rename_with( ~'blackpop_rate', 'percentBLACK')
JH_db_bPOP <- subset(JH_db_bPOP, select = c(BREAKln, blackpop_rate))

#join these two old tables to retrieve black pop rate var
JH_db_addingBpop <- left_join(JH_db_July, JH_db_bPOP, by = "BREAKln")
JH_db_addingBpop$state <- tolower(JH_db_addingBpop$state)

#################################################################
#
#
#
#
###   code below is done to run mice RF imputation
####
#####

#we do this to make it easier to separate vars for mice

JH_db_addingBpop$state1 <- JH_db_addingBpop$state

#we perform the following to make it easier to join tables
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="alabama",'1',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="alaska",'2',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="arizona",'3',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="arkansas",'4',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="california",'5',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="colorado",'6',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="connecticut",'7',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="delaware",'8',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="florida",'9',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="georgia",'10',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="hawaii",'11',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="idaho",'12',state1))

JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="illinois",'13',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="indiana",'14',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="iowa",'15',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="kansas",'16',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="kentucky",'17',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="louisiana",'18',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="maine",'19',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="maryland",'20',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="massachusetts",'21',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="michigan",'22',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="minnesota",'23',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="mississippi",'24',state1))

JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="missouri",'25',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="montana",'26',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="nebraska",'27',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="nevada",'28',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="new hampshire",'29',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="new jersey",'30',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="new mexico",'31',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="new york",'32',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="north carolina",'33',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="north dakota",'34',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="ohio",'35',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="oklahoma",'36',state1))

JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="oregon",'37',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="pennsylvania",'38',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="rhode island",'39',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="south carolina",'40',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="south dakota",'41',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="tennessee",'42',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="texas",'43',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="utah",'44',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="vermont",'45',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="virginia",'46',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="washington",'47',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="west virginia",'48',state1))

JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="wisconsin",'49',state1))
JH_db_addingBpop <-JH_db_addingBpop %>% mutate(state1=ifelse(state1=="wyoming",'50',state1))


#we decided to separate vars with missing data, run mice RF, then bring it back
#first we create breakln (var used to join tables) and clean
JH_db_addingBpop <- JH_db_addingBpop %>% unite('BREAKln_y_state', year, state1, remove = FALSE)
JH_db_addingBpop$BREAKln_y_state <- sub("_", "", JH_db_addingBpop$BREAKln_y_state)
JH_db_addingBpop$BREAKln_y_state <- as.double(JH_db_addingBpop$BREAKln_y_state)


#separating
forest_db_bPOP <- subset(JH_db_addingBpop, select = c(BREAKln_y_state, blackpop_rate))


# running mice
mice_imp_bPOP <- complete(mice(forest_db_bPOP,                                 # Predictive mean matching imputation
                          m = 25, maxit = 50,
                          method = "rf"))
head(mice_imp_bPOP)

#renaming vars
mice_imp_bPOP <- mice_imp_bPOP %>% rename_with( ~"black_pop_rate_imp", "blackpop_rate")

#join back again
JH_db_final_September <- left_join(JH_db_addingBpop, mice_imp_bPOP, by = "BREAKln_y_state")


#save dirty file for the record
#write.csv(JH_db_final_September, "C:/Users/sarah/Desktop/lookinto/CEOE/_official_tables/JH_Sept_dirty.csv", row.names=FALSE)

#clean and save again the official table
JH_db_final_September <- subset(JH_db_final_September, select = -c(BREAKln_y_state, newyear, newstate, state1))

#save clean file for the record
#write.csv(JH_db_final_September, "C:/Users/sarah/Desktop/lookinto/CEOE/_official_tables/JH_Sept_clean.csv", row.names=FALSE)



#load tables created with MICE RF algorithm
JH_db_final_September <- read_csv('C:/Users/sarah/Desktop/lookinto/CEOE/_official_tables/JH_Sept_clean.csv')




#########################
#########
#########
######### added code above in 9/27/24
#########
#########
#############################



######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
################################                                            ##########################
################################    below we get descriptive statistics     ##########################
################################          and run NB tests                  ##########################
################################                                            ##########################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################


#descriptive stats

#descriptive stats for CJH
Hmisc::describe(JH_db_final_September$JH_cit_imp)
stat.desc(JH_db_final_September$JH_cit_imp)
#percentage of zeroes for JH cit
sumCol <- JH_db_final_September$JH_cit_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for PJH
Hmisc::describe(JH_db_final_September$JH_pol_imp)
stat.desc(JH_db_final_September$JH_pol_imp)
#percentage of zeroes for JH pol
sumCol <- JH_db_final_September$JH_pol_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for TJH
Hmisc::describe(JH_db_final_September$JH_tot_imp)
stat.desc(JH_db_final_September$JH_tot_imp)
#percentage of zeroes for JH tot
sumCol <- JH_db_final_September$JH_tot_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for log of murder rate
Hmisc::describe(JH_db_final_September$log_murder_rate_imp)
stat.desc(JH_db_final_September$log_murder_rate_imp)
#percentage of zeroes for log of murder rate
sumCol <- JH_db_final_September$log_murder_rate_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for population from 20 through 39 years
Hmisc::describe(JH_db_final_September$pop_20_39_imp)
stat.desc(JH_db_final_September$pop_20_39_imp)
#percentage of zeroes for population from 20 through 39 years
sumCol <- JH_db_final_September$pop_20_39_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for log of state population
Hmisc::describe(JH_db_final_September$log_popstate_imp)
stat.desc(JH_db_final_September$log_popstate_imp)
#percentage of zeroes for log of state population
sumCol <- JH_db_final_September$log_popstate_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for log of arrest rate
Hmisc::describe(JH_db_final_September$log_arrest_rate_imp)
stat.desc(JH_db_final_September$log_arrest_rate_imp)
#percentage of zeroes for log of arrest rate
sumCol <- JH_db_final_September$log_arrest_rate_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for log of police rate
Hmisc::describe(JH_db_final_September$log_police_rate_imp)
stat.desc(JH_db_final_September$log_police_rate_imp)
#percentage of zeroes for log of police rate
sumCol <- JH_db_final_September$log_police_rate_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for poverty rate
Hmisc::describe(JH_db_final_September$poverty_rate_imp)
stat.desc(JH_db_final_September$poverty_rate_imp)
#percentage of zeroes for poverty rate
sumCol <- JH_db_final_September$poverty_rate_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for unemployment rate
Hmisc::describe(JH_db_final_September$unemp_rate_imp)
stat.desc(JH_db_final_September$unemp_rate_imp)
#percentage of zeroes for unemployment rate
sumCol <- JH_db_final_September$unemp_rate_imp
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res


#descriptive stats for SYG LAW
Hmisc::describe(JH_db_final_September$syg_law)
stat.desc(JH_db_final_September$syg_law)
#percentage of zeroes for SYG LAW
sumCol <- JH_db_final_September$syg_law
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res

#descriptive stats for RTC LAW
Hmisc::describe(JH_db_final_September$shall_issue)
stat.desc(JH_db_final_September$shall_issue)
#percentage of zeroes for RTC LAW
sumCol <- JH_db_final_September$shall_issue
sumCol <- as.data.frame(sumCol)

res <- colSums(sumCol[ , 1, drop = FALSE]==0, na.rm=TRUE)/nrow(sumCol)*100
res




#here we create a correlation table
covariates <- subset(JH_db_final_September, select = c(shall_issue, syg_law, unemp_rate_imp, log_murder_rate_imp, log_police_rate_imp, log_arrest_rate_imp, poverty_rate_imp, popstate_imp, black_pop_rate_imp))

round(cor(covariates,  use = "complete.obs"),
      digits = 2 # rounded to 2 decimals
)






######################################################################################
#
#   BELOW WE FINALLY RUN random NB tests
#    #hausman tests below indicated random effects tests so we placed random effects in the article
#     #however, the BP test indicated fixed effects (we ran again and the results from random effects to fixed effects did not change)
#       #so we simply placed a note about this in the article
#
plmtest(fixed, c("time"), type=("bp"))
plmtest(fixed, c("individual"), type=("bp"))


#hausman test
fixed <- plm(model_tot, data=JH_db_final_September, index=c("state", "year"), model="within")  #fixed model
random <- plm(model_tot, data=JH_db_final_September, index=c("state", "year"), model="random")  #random model
phtest(fixed,random) #Hausman test
#hausman is not significant, thus we use random effects negbin

######################
#citizen test
#random
model_cit <- JH_cit_imp ~ shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp
model_cit.glm.nb <- glm.nb(model_cit, data=JH_db_final_September)
summary(model_cit.glm.nb)

#fe
model_cit <- JH_cit_imp ~ shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp
model_cit.fenegbin <- fenegbin(model_cit, data=JH_db_final_September)
summary(model_cit.fenegbin)


#citizen robustness check
#random
model_cit_test <- JH_cit_imp ~ shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp + pop_20_39_imp
model_cit_test.glm.nb <- glm.nb(model_cit_test, data=JH_db_final_September)
summary(model_cit_test.glm.nb)

#fe
model_cit_test <- JH_cit_imp ~ shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp + pop_20_39_imp
model_cit_test.fenegbin <- fenegbin(model_cit_test, data=JH_db_final_September)
summary(model_cit_test.fenegbin)


#########################
#police test
#random
model_pol <- JH_pol_imp ~ shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp
model_pol.glm.nb <- glm.nb(model_pol, data=JH_db_final_September)
summary(model_pol.glm.nb)

#fe
model_pol <- JH_pol_imp ~ shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp
model_pol.fenegbin <- fenegbin(model_pol, data=JH_db_final_September)
summary(model_pol.fenegbin)

#police robustness check
#random
model_pol_test <- JH_pol_imp ~ shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp + pop_20_39_imp
model_pol_test.glm.nb <- glm.nb(model_pol_test, data=JH_db_final_September)
summary(model_pol_test.glm.nb)

#fe
model_pol_test <- JH_pol_imp ~ shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp + pop_20_39_imp
model_pol_test.fenegbin <- fenegbin(model_pol_test, data=JH_db_final_September)
summary(model_pol_test.fenegbin)



#################
#total test
#random
model_tot <- JH_tot_imp ~ shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp
model_tot.glm.nb <- glm.nb(model_tot, data=JH_db_final_September)
summary(model_tot.glm.nb)

#fe
model_tot <- JH_tot_imp ~ shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp
model_tot.fenegbin <- fenegbin(model_tot, data=JH_db_final_September)
summary(model_tot.fenegbin)


#total robustness check
#random
model_tot_test <- JH_tot_imp ~ shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp + pop_20_39_imp
model_tot_test.glm.nb <- glm.nb(model_tot_test, data=JH_db_final_September)
summary(model_tot_test.glm.nb)

#fe
model_tot_test <- JH_tot_imp ~ shall_issue + syg_law + unemp_rate_imp + log_murder_rate_imp + log_police_rate_imp + log_arrest_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp + pop_20_39_imp
model_tot_test.fenegbin <- fenegbin(model_tot_test, data=JH_db_final_September)
summary(model_tot_test.fenegbin)












######################################################################################
#
#   BELOW WE RUN additional tests removing the murder rate and arrest rate covariates
#    using random NB tests
#     #did not place these results on article yet (wanted to make sure the other parts were correct first)
#



#hausman test
fixed <- plm(model_cit, data=JH_db_final_September, index=c("state", "year"), model="within")  #fixed model
random <- plm(model_cit, data=JH_db_final_September, index=c("state", "year"), model="random")  #random model
phtest(fixed,random) #Hausman test
#hausman is not significant, thus we use random effects negbin


######################
#citizen test
#random
model_cit <- JH_cit_imp ~ shall_issue + syg_law + unemp_rate_imp + log_police_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp
model_cit.glm.nb <- glm.nb(model_cit, data=JH_db_final_September)
summary(model_cit.glm.nb)

#citizen robustness check
#random
model_cit_test <- JH_cit_imp ~ shall_issue + syg_law + unemp_rate_imp + log_police_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp + pop_20_39_imp
model_cit_test.glm.nb <- glm.nb(model_cit_test, data=JH_db_final_September)
summary(model_cit_test.glm.nb)


#########################
#police test
#random
model_pol <- JH_pol_imp ~ shall_issue + syg_law + unemp_rate_imp + log_police_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp
model_pol.glm.nb <- glm.nb(model_pol, data=JH_db_final_September)
summary(model_pol.glm.nb)

#police robustness check
#random
model_pol_test <- JH_pol_imp ~ shall_issue + syg_law + unemp_rate_imp + log_police_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp + pop_20_39_imp
model_pol_test.glm.nb <- glm.nb(model_pol_test, data=JH_db_final_September)
summary(model_pol_test.glm.nb)


#################
#total test
#random
model_tot <- JH_tot_imp ~ shall_issue + syg_law + unemp_rate_imp + log_police_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp
model_tot.glm.nb <- glm.nb(model_tot, data=JH_db_final_September)
summary(model_tot.glm.nb)

#total robustness check
#random
model_tot_test <- JH_tot_imp ~ shall_issue + syg_law + unemp_rate_imp + log_police_rate_imp + poverty_rate_imp + log_popstate_imp + black_pop_rate_imp + pop_20_39_imp
model_tot_test.glm.nb <- glm.nb(model_tot_test, data=JH_db_final_September)
summary(model_tot_test.glm.nb)


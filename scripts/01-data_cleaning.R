#### Preamble ####
# Purpose: Clean the survey data downloaded from https://search1.odesi.ca/#/details?uri=%2Fodesi%2FCTADS-82M0020-E-2017-person.xml
# Author: Yijun Shen
# Data: 3 January 2021
# Contact: yijun.shen@mail.utoroto.ca 
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!
# - Change these to yours
# Any other information needed?


#### Workspace setup ####
# Use R Projects, not setwd().
library(tidyverse)
library(ggplot2)
library(tibble)
library(knitr)
library(gridExtra)
# Read in the raw data. 
raw_data <- read.csv("CTADS_2017_person.csv")
              
# Just keep some variables that may be of interest (change 
# this depending on your interests)
data_cleaned <- raw_data %>% 
  select(AGE0014, AGE1524, AGE2544, AGE45PL, HHTYPE, HS_10, HS_20, HS_30, HV_01, HV_02, DVAGE, SEX, HWB_01, HWB_10, SS_10, EHG1_01, DVYRSSMK, UNDERAGE, WTPP, EX_130, ALC_10, ALC_20, ALC_30, LP_01, ALCSTAT3, PS_30, CA_10, LP_01, ALCSTAT3, CA_10, HDSIZE, SUN, MON, TUES, WED, THURS, FRI, SAT, REFMONTH, PROV, DVURBAN)

data_cleaned2 <- rename(data_cleaned, "-14" = AGE0014, "15-24" = AGE1524, "25-44" = AGE2544, "45-" = AGE45PL, num_peo_smok = HS_20, peo_smok = HS_10, num_smok = HS_30, Houshold_typ = HHTYPE, Num_e_smok = HV_01, Day_peo_esmok = HV_02, resd_age = DVAGE, resd_sex = SEX, Health = HWB_01, Mental_health = HWB_10, Frequency_smoke = SS_10, edu_back = EHG1_01, Year_smoke = DVYRSSMK, drink_unerage = UNDERAGE, num_sec_moke = EX_130, drink_beverage = ALC_10, drink = ALC_20, age_of_drink = ALC_30, language = LP_01, drink_status = ALCSTAT3, first_smoke = PS_30, first_smoke_from = CA_10, num_hosehold = HDSIZE, drink_SUN = SUN, deink_MON = MON, drink_TUES = TUES, drink_WED = WED, drink_THURS = THURS, drink_FRI = FRI, drink_SAT = SAT)
         
data_clean3 <- data_cleaned2%>%
  filter(drink_SUN < 90)%>%
  mutate(household_type = case_when(Houshold_typ == 1 ~ "18-19 only",
                                    Houshold_typ == 2 ~ "20-24 only",
                                    Houshold_typ == 3 ~ "25-44 only",
                                    Houshold_typ == 4 ~ "45-64 only",
                                    Houshold_typ == 5 ~ "65- only",
                                    Houshold_typ == 6 ~ "adult and child 0-11",
                                    Houshold_typ == 7 ~ "adult and child 12-19",
                                    Houshold_typ == 8 ~ "adult and young child 0-19",
                                    Houshold_typ == 9 ~ "other"))%>%
  mutate(people_smoke = case_when(peo_smok == 1 ~ "Yes",
                                  peo_smok == 2 ~ "No",
                                  peo_smok == 7 ~ "Dont know"))%>%
  mutate(num_cigarette_daily = case_when(num_smok == 1 ~ "1-10",
                                         num_smok == 2 ~ "11-20",
                                         num_smok == 3 ~ "21-30",
                                         num_smok == 4 ~ "31-40",
                                         num_smok == 5 ~ "41-",
                                         num_smok == 6 ~ "Skip",
                                         num_smok == 7 ~ "Dont know",
                                         num_smok == 8 ~ "Refuse",
                                         num_smok == 9 ~ "No stated"))%>%
  mutate(people_ecig = case_when(Num_e_smok == 1 ~ "Yes",
                                     Num_e_smok == 2 ~ "No",
                                     Num_e_smok == 7 ~ "Dont know",
                                     Num_e_smok == 8 ~ "Refuse"))%>%
  mutate(Gender = case_when(resd_sex == 1 ~ "Male",
                            resd_sex == 2 ~ "Female"))%>%
  mutate(Health_status = case_when(Health == 1 ~ "Excellent",
                                   Health == 2 ~ "Very Good",
                                   Health == 3 ~ "Good",
                                   Health == 4 ~ "Fair",
                                   Health == 5 ~ "Poor",
                                   Health == 7 ~ "Dont know",
                                   Health == 8 ~ "Refusal"))%>%
  mutate(Mentalhealth_status = case_when(Mental_health == 1 ~ "Excellent",
                                         Mental_health == 2 ~ "Very Good",
                                         Mental_health == 3 ~ "Good",
                                         Mental_health == 4 ~ "Fair",
                                         Mental_health == 5 ~ "Poor",
                                         Mental_health == 7 ~ "Dont know",
                                         Mental_health == 8 ~ "Refusal"))%>%
  mutate(frequency_ofsmoke = case_when(Frequency_smoke == 1 ~ "Every day",
                                       Frequency_smoke == 2 ~ "Occasionally",
                                       Frequency_smoke == 3 ~ "Not at all"))%>%
  mutate(education = case_when(edu_back == 1 ~ "Lower High school",
                               edu_back == 2 ~ "High school",
                               edu_back == 3 ~ "Trade",
                               edu_back == 4 ~ "College",
                               edu_back == 5 ~ "University",
                               edu_back == 6 ~ "Bachelor",
                               edu_back == 7 ~ "Above Bachelor",
                               edu_back == 97 ~ "Dontknow",
                               edu_back == 98 ~ "Refusal",
                               edu_back == 99 ~ "Not stated"))%>%
  mutate(drink_underage = case_when(drink_unerage == 1 ~ "Yes",
                                    drink_unerage == 2 ~ "No"))%>%
  mutate(monthly_sec_smok = case_when(num_sec_moke == 1 ~ "Everyday",
                                      num_sec_moke == 2 ~ "Almost everyday",
                                      num_sec_moke == 3 ~ "Once a week",
                                      num_sec_moke == 4 ~ "Once a month",
                                      num_sec_moke == 5 ~ "Never",
                                      num_sec_moke == 7 ~ "Dont Know",
                                      num_sec_moke == 8 ~ "Refusal",
                                      num_sec_moke == 9 ~ "Not stated"))%>%
  mutate(drinked_before = case_when(drink == 1 ~ "Yes",
                                     drink == 2 ~ "No",
                                     drink == 6 ~ "Skip",
                                     drink == 7 ~ "Dont Know",
                                     drink == 8 ~ "Refusal",
                                     drink == 9 ~ "Not stated"))%>%
  mutate(yearly_drinking_beverage = case_when(drink_beverage == 1 ~ "Everyday",
                                       drink_beverage == 2 ~ "4-5 per week",
                                       drink_beverage == 3 ~ "2-3 per week",
                                       drink_beverage == 4 ~ "Once per week",
                                       drink_beverage == 5 ~ "2-3 per month",
                                       drink_beverage == 6 ~ "Once per month",
                                       drink_beverage == 7 ~ "Less than once per month",
                                       drink_beverage == 8 ~ "Never",
                                       drink_beverage == 97 ~ "Dont Know",
                                       drink_beverage == 98 ~ "Refusal",
                                       drink_beverage == 99 ~ "Not stated"))%>%
  mutate(Language = case_when(language == 1 ~ "English",
                              language == 2 ~ "French",
                              language == 9 ~ "Not stated"))%>%
  mutate(Drink_status = case_when(drink_status == 1 ~ "Abstainer",
                                  drink_status == 2 ~ "Former Drinker",
                                  drink_status == 3 ~ "Current Drinker",
                                  drink_status == 9 ~ "Not stated"))%>%
  mutate(Household_size = case_when(num_hosehold == 1 ~ "1 person",
                                    num_hosehold == 2 ~ "2 people",
                                    num_hosehold == 3 ~ "3 people",
                                    num_hosehold == 4 ~ "4 people",
                                    num_hosehold == 5 ~ "5 or more people",
                                    num_hosehold == 6 ~ "Skip"))%>%
  mutate(Month = case_when(REFMONTH == 2 ~ "Feb",
                           REFMONTH == 3 ~ "MAR-APR",
                           REFMONTH == 5 ~ "MAY-JUN",
                           REFMONTH == 7 ~ "JUL-AUG",
                           REFMONTH == 9 ~ "SEP-OCT",
                           REFMONTH == 11 ~ "NOV-DEC"))%>%
  mutate(Region = case_when(PROV == 10 ~ "NL",
                             PROV == 11 ~ "PE",
                             PROV == 12 ~ "NS",
                             PROV == 13 ~ "NB",
                             PROV == 24 ~ "QC",
                             PROV == 35 ~ "ON",
                             PROV == 46 ~ "MB",
                             PROV == 47 ~ "SK",
                             PROV == 48 ~ "AB",
                             PROV == 59 ~ "BC"))%>%
  mutate(Dvurabn = case_when(DVURBAN == 1 ~ "Popolation centre",
                             DVURBAN == 2 ~ "Rural"))%>%
  select(-c(PROV, REFMONTH, num_hosehold, drink_status, language, drink_beverage, drink, num_sec_moke, drink_unerage, edu_back, Frequency_smoke, Mental_health, Health, resd_sex, Num_e_smok, num_smok, peo_smok, Houshold_typ, DVURBAN))

#### What's next? ####

Then, the rest of the work will show in the output of the final project.


         

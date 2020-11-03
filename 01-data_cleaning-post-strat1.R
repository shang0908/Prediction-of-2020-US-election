#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####

library(tidyverse)
# Read in the raw data.
census <- read.csv("usa_00002.csv")
survey <- read.csv("ns20200625.csv")

# Just keep some variables that may be of interest (change 
# this depending on your interests)


reduced_census <- 
  census %>% 
  select(perwt,
         statefip,
         sex,
         age, 
         race,
         educd,
         labforce)

reduced_census <- na.omit(reduced_census)

#change the age

reduced_census <- 
  reduced_census %>% 
  filter(age >= 18) 

reduced_census$age[reduced_census$age >= 18 & reduced_census$age <= 39] <- "18-29"
reduced_census$age[reduced_census$age >= 30 & reduced_census$age <= 44] <- "30-44"
reduced_census$age[reduced_census$age >= 45 & reduced_census$age <= 64] <- "45-64"
reduced_census$age[reduced_census$age >= 65] <- "65+"

#change race of two dataset


reduced_census %>%
  count(race) 

reduced_census$race[reduced_census$race == "black/african american/negro"] <- "black"
reduced_census$race[reduced_census$race == "chinese"|
                    reduced_census$race == "japanese"|
                    reduced_census$race == "other asian or pacific islander"] <- "asians"
reduced_census$race[reduced_census$race == "american indian or alaska native"|
                    reduced_census$race == "other race, nec"|
                    reduced_census$race == "three or more major races"|
                    reduced_census$race == "two major races"] <- "others"

#change education

names(reduced_census)[names(reduced_census) == "educd"] <- "education"



reduced_census %>%
  count(education) 

reduced_census$education[reduced_census$education == "12th grade, no diploma"|
                           reduced_census$education == "grade 1"|
                           reduced_census$education == "grade 2"|
                           reduced_census$education == "grade 3"|
                           reduced_census$education == "grade 4"|
                           reduced_census$education == "grade 5"|
                           reduced_census$education == "grade 6"|
                           reduced_census$education == "grade 7"|
                           reduced_census$education == "grade 8"|
                           reduced_census$education == "grade 9"|
                           reduced_census$education == "grade 10"|
                           reduced_census$education == "grade 11"|
                           reduced_census$education == "kindergarten"|
                           reduced_census$education == "no schooling completed"|
                           reduced_census$education == "nursery school, preschool"] <- "Don't graduate from HS"

reduced_census$education[reduced_census$education == "ged or alternative credential"|
                           reduced_census$education == "regular high school diploma"] <- "High school graduate"

reduced_census$education[reduced_census$education == "1 or more years of college credit, no degree"|
                           reduced_census$education == "associate's degree, type not specified"|
                           reduced_census$education == "some college, but less than 1 year"] <- "some college"

reduced_census$education[reduced_census$education == "bachelor's degree"|
                           reduced_census$education == "doctoral degree"|
                           reduced_census$education == "professional degree beyond a bachelor's degree"|
                           reduced_census$education == "master's degree"] <- "college graduation and above"
            
#change state
names(reduced_census)[names(reduced_census) == "statefip"] <- "state"

reduced_survey %>%
  count(state) 

reduced_census %>%
  count(state) 

reduced_census$state[reduced_census$state == "alabama"] <- "AL"
reduced_census$state[reduced_census$state == "alaska"] <- "AK"
reduced_census$state[reduced_census$state == "arizona"] <- "AZ"
reduced_census$state[reduced_census$state == "arkansas"] <- "AR"
reduced_census$state[reduced_census$state == "california"] <- "CA"
reduced_census$state[reduced_census$state == "colorado"] <- "CO"
reduced_census$state[reduced_census$state == "connecticut"] <- "CT"
reduced_census$state[reduced_census$state == "delaware"] <- "DE"
reduced_census$state[reduced_census$state == "district of columbia"] <- "DC"
reduced_census$state[reduced_census$state == "florida"] <- "FL"
reduced_census$state[reduced_census$state == "georgia"] <- "GA"
reduced_census$state[reduced_census$state == "hawaii"] <- "HI"
reduced_census$state[reduced_census$state == "idaho"] <- "ID"
reduced_census$state[reduced_census$state == "illinois"] <- "IL"
reduced_census$state[reduced_census$state == "indiana"] <- "IN"
reduced_census$state[reduced_census$state == "iowa"] <- "IA"
reduced_census$state[reduced_census$state == "kansas"] <- "KS"
reduced_census$state[reduced_census$state == "kentucky"] <- "KY"
reduced_census$state[reduced_census$state == "louisiana"] <- "LA"
reduced_census$state[reduced_census$state == "maine"] <- "ME"
reduced_census$state[reduced_census$state == "maryland"] <- "MD"
reduced_census$state[reduced_census$state == "massachusetts"] <- "MA"
reduced_census$state[reduced_census$state == "michigan"] <- "MI"
reduced_census$state[reduced_census$state == "minnesota"] <- "MN"
reduced_census$state[reduced_census$state == "mississippi"] <- "MS"
reduced_census$state[reduced_census$state == "missouri"] <- "MO"
reduced_census$state[reduced_census$state == "montana"] <- "MT"
reduced_census$state[reduced_census$state == "nebraska"] <- "NE"
reduced_census$state[reduced_census$state == "nevada"] <- "NV"
reduced_census$state[reduced_census$state == "new hampshire"] <- "NH"
reduced_census$state[reduced_census$state == "new jersey"] <- "NJ"
reduced_census$state[reduced_census$state == "new mexico"] <- "NM"
reduced_census$state[reduced_census$state == "new york"] <- "NY"
reduced_census$state[reduced_census$state == "north dakota"] <- "ND"
reduced_census$state[reduced_census$state == "north carolina"] <- "NC"
reduced_census$state[reduced_census$state == "ohio"] <- "OH"
reduced_census$state[reduced_census$state == "oklahoma"] <- "OK"
reduced_census$state[reduced_census$state == "oregon"] <- "OR"
reduced_census$state[reduced_census$state == "pennsylvania"] <- "PA"
reduced_census$state[reduced_census$state == "rhode island"] <- "RI"
reduced_census$state[reduced_census$state == "south carolina"] <- "SC"
reduced_census$state[reduced_census$state == "tennessee"] <- "TN"
reduced_census$state[reduced_census$state == "texas"] <- "TX"
reduced_census$state[reduced_census$state == "utah"] <- "UT"
reduced_census$state[reduced_census$state == "vermont"] <- "VT"
reduced_census$state[reduced_census$state == "virginia"] <- "VA"
reduced_census$state[reduced_census$state == "washington"] <- "WA"
reduced_census$state[reduced_census$state == "west virginia"] <- "WV"
reduced_census$state[reduced_census$state == "wyoming"] <- "WY"
reduced_census$state[reduced_census$state == "wisconsin"] <- "WI"
reduced_census$state[reduced_census$state == "south dakota"] <- "SD"

write_csv(reduced_census, "census_data1.csv")





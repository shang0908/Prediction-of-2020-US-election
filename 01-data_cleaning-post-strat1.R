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

reduced_census_1 %>%
  group_by(state) %>%
  summarise(trump = sum(trumpestimate*cellcount)/sum(cellcount)) -> predict_1

reduced_census_1 %>%
  group_by(state) %>%
  summarise(biden = sum(bidenestimate*cellcount)/sum(cellcount)) -> predict_2

predict <- cbind(predict_1, predict_2$biden)
names(predict)[names(predict) == "predict_2$biden"] <- "biden"

library(tidyverse)
# Read in the raw data.
census <- read.csv("census_data(1).csv")
survey <- read.csv("survey_data.csv")

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_survey <- 
  survey %>% 
  select(vote_2020,
         race_ethnicity,
         education,
         gender,
         state,
         age,
         employment)

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
reduced_survey <- na.omit(reduced_survey)



reduced_survey<-
  reduced_survey %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))

reduced_survey<-
  reduced_survey %>%
  mutate(vote_biden = 
           ifelse(vote_2020=="Joe Biden", 1, 0))



#change the age
reduced_survey <- 
  reduced_survey %>% 
  filter(age >= 18) 

reduced_census <- 
  reduced_census %>% 
  filter(age >= 18) 

reduced_survey$age[reduced_survey$age >= 18 & reduced_survey$age <= 39] <- "18-29"
reduced_survey$age[reduced_survey$age >= 30 & reduced_survey$age <= 44] <- "30-44"
reduced_survey$age[reduced_survey$age >= 45 & reduced_survey$age <= 64] <- "45-64"
reduced_survey$age[reduced_survey$age >= 65] <- "65+"

reduced_census$age[reduced_census$age >= 18 & reduced_census$age <= 39] <- "18-29"
reduced_census$age[reduced_census$age >= 30 & reduced_census$age <= 44] <- "30-44"
reduced_census$age[reduced_census$age >= 45 & reduced_census$age <= 64] <- "45-64"
reduced_census$age[reduced_census$age >= 65] <- "65+"

#change the sex
names(reduced_survey)[names(reduced_survey) == "gender"] <- "sex"
reduced_survey$sex[reduced_survey$sex == "Female"] <- "female"
reduced_survey$sex[reduced_survey$sex == "Male"] <- "male"

#change race of two dataset
names(reduced_survey)[names(reduced_survey) == "race_ethnicity"] <- "race"

reduced_survey %>%
  count(race) 

reduced_census %>%
  count(race) 

reduced_survey$race[reduced_survey$race == "White"] <- "white"
reduced_survey$race[reduced_survey$race == "Black, or African American"] <- "black"
reduced_survey$race[reduced_survey$race == "Asian (Asian Indian)"|
                      reduced_survey$race == "Asian (Chinese)"|
                      reduced_survey$race == "Asian (Filipino)"|
                      reduced_survey$race == "Asian (Vietnamese)"|
                      reduced_survey$race == "Asian (Japanese)"|
                      reduced_survey$race == "Asian (Korean)"|
                      reduced_survey$race == "Asian (Other)" |                     
                      reduced_survey$race == "Pacific Islander (Guamanian)"|
                      reduced_survey$race == "Pacific Islander (Native Hawaiian)"|
                      reduced_survey$race == "Pacific Islander (Other)"|
                      reduced_survey$race == "Pacific Islander (Samoan)"] <- "asians"
reduced_survey$race[reduced_survey$race == "American Indian or Alaska Native"|
                      reduced_survey$race == "Some other race"] <- "others"

reduced_census$race[reduced_census$race == "black/african american/negro"] <- "black"
reduced_census$race[reduced_census$race == "chinese"|
                    reduced_census$race == "japanese"|
                    reduced_census$race == "other asian or pacific islander"] <- "asians"
reduced_census$race[reduced_census$race == "american indian or alaska native"|
                    reduced_census$race == "other race, nec"|
                    reduced_census$race == "three or more major races"|
                    reduced_census$race == "two major races"] <- "others"

#change labor force
reduced_survey %>%
  count(employment) 

reduced_census %>%
  count(labforce) 

names(reduced_survey)[names(reduced_survey) == "employment"] <- "labforce"

reduced_census <- 
  reduced_census %>% 
  filter(labforce != 'n/a') 


reduced_survey$labforce[reduced_survey$labforce == "Full-time employed"|
                          reduced_survey$labforce == "Part-time employed"|
                          reduced_survey$labforce == "Self-employed"|
                          reduced_survey$labforce == "Homemaker"|
                          reduced_survey$labforce == "Unemployed or temporarily on layoff"] <- "yes, in the labor force"

reduced_survey$labforce[reduced_survey$labforce == "Student"|
                          reduced_survey$labforce == "Permanently disabled"|
                          reduced_survey$labforce == "Other:"|
                          reduced_survey$labforce == "Retired"] <- "no, not in the labor force"



#change education

names(reduced_census)[names(reduced_census) == "educd"] <- "education"

reduced_survey %>%
  count(education) 

reduced_census %>%
  count(education) 

reduced_survey$education[reduced_survey$education == "3rd Grade or less"|
                           reduced_survey$education == "Middle School - Grades 4 - 8"|
                           reduced_survey$education == "Completed some high school"] <- "Don't graduate from HS"

reduced_survey$education[reduced_survey$education == "High school graduate"] <- "High school graduate"

reduced_survey$education[reduced_survey$education == "Associate Degree"|
                           reduced_survey$education == "Completed some college, but no degree"|
                           reduced_survey$education == "Other post high school vocational training"] <- "some college"

reduced_survey$education[reduced_survey$education == "College Degree (such as B.A., B.S.)"|
                           reduced_survey$education == "Completed some graduate, but no degree"|
                           reduced_survey$education == "Doctorate degree"|
                           reduced_survey$education == "Masters degree"] <- "college graduation and above"


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
write_csv(reduced_survey, "survey_data1.csv")


                                                                                
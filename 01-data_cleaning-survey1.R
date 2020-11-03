#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(tidyverse)
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read.csv("ns20200625.csv")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_survey <- 
  survey %>% 
  select(vote_2020,
         race_ethnicity,
         education,
         gender,
         state,
         age,
         employment)

reduced_survey <- na.omit(reduced_survey)

#### What else???? ####
# Maybe make some age-groups?
# Maybe check the values?
# Is vote a binary? If not, what are you going to do?

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

reduced_survey$age[reduced_survey$age >= 18 & reduced_survey$age <= 39] <- "18-29"
reduced_survey$age[reduced_survey$age >= 30 & reduced_survey$age <= 44] <- "30-44"
reduced_survey$age[reduced_survey$age >= 45 & reduced_survey$age <= 64] <- "45-64"
reduced_survey$age[reduced_survey$age >= 65] <- "65+"

#change the sex
names(reduced_survey)[names(reduced_survey) == "gender"] <- "sex"
reduced_survey$sex[reduced_survey$sex == "Female"] <- "female"
reduced_survey$sex[reduced_survey$sex == "Male"] <- "male"

#change race 

names(reduced_survey)[names(reduced_survey) == "race_ethnicity"] <- "race"

reduced_survey %>%
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

#change labor force
reduced_survey %>%
  count(employment) 

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
reduced_survey %>%
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


write_csv(reduced_survey, "survey_data1.csv")

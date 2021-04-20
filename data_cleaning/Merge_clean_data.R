

# script to join cleaned data together. 
# (except Q8 to Q13, who are saved in long format separately)

library(rio)
library(tidyr)
library(dplyr)

Q1_Q7 <- rio::import("../data/clean/clean_Q1-Q7.rds") %>% 
  arrange(ID) # just to be sure that all the tables that we will bind are in the same order
Q14_Q23 <- rio::import("../data/clean/clean_Q13-Q23.csv") %>% 
  select(-V1, # csv artifact on my computer
         -other_technology_13, # handled separately
  ) %>% 
  select(
    ID, # ID as first column
    everything(),
  ) %>%
  relocate(company_size_23, .after = last_col()) %>% # send this one to be last.
  arrange(ID) # just to be sure that all the tables that we will bind are in the same order
Q24_Q33 <- rio::import("../data/clean/clean_Q24-Q33.rds") %>% 
  arrange(ID) # just to be sure that all the tables that we will bind are in the same order
Q34_Q44 <- rio::import("../data/clean/clean_Q34-Q44.rds") %>% 
  arrange(ID) # just to be sure that all the tables that we will bind are in the same order

clean_survey_data <- cbind(Q1_Q7, Q14_Q23, Q24_Q33, Q34_Q44)
clean_survey_data <- clean_survey_data[!duplicated(as.list(clean_survey_data))] # remove duplicate ID columns
clean_survey_data <- clean_survey_data %>%
  select(ID,
         start_time,
         completion_time,
         survey_duration,
         everything(),
         -ID.1, # not suer why this happens??? (why it was renamed to ID.1 and not removed in previous step?)
  )

# export the cleaned data;
rio::export(clean_survey_data, "../data/clean/clean_all_Qs.csv")
rio::export(clean_survey_data, "../data/clean/clean_all_Qs.rds")




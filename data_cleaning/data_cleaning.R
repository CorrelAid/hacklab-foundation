library(rio)
library(janitor)
library(tidyr)
library(knitr)
library(dplyr)
library(dbplyr)

raw_data <- import("~/CorrelAid/hacklab-foundation/data/raw/census-base-anonymized-2020.xlsx")
cleaned_data <- clean_names(raw_data)

#Removing names, mails and times
head(cleaned_data)
cleaned_data <- cleaned_data[-c(4:5)]
colnames(cleaned_data)

#separate dataframe for tools 
skills <- cbind(cleaned_data$id, cleaned_data[,9:62])
head(skills)
colnames(skills)[1] <- "id"
colnames(skills)

#transforming into long format
skills <- pivot_longer(skills, cols = 2:55, names_to = "tool", values_to = "level")
head(skills)
#maybe we should replace n/a by 0?


#Cleaning the individual answers

#Creating a dataframe without the individual skills 
data_questions <- cbind(cleaned_data[1:10], cleaned_data[65:96])
colnames(data_questions)
#Changing the column names
colnames(data_questions)[4:42] <- c("developer", "hobby", "employment", "region", "city", "age_range", 
                          "learning", "new_tools", "operating_system", "operating_system_2", 
                          "influence", "discovery", "education", "study", "importance_ed", 
                          "change_ed", "job_sat", "orga_size", "work_hours", "overtime", 
                          "on-boarding", "improve_on-boarding", "salary", "job_seeking", "jobsearch", 
                          "info_sources", "communities_1", "communities_2", "age", "gender", 
                          "trans", "sexuality", "ethnicity", "disability", "psychiatric_disorder", 
                          "care", "length", "difficulty", "comments")
colnames(data_questions)

#Employment
data_questions$employment <- tolower(data_questions$employment)

data_questions %>% 
  group_by(employment) %>%
  tally() %>%
  kable()

data_questions$employment_cleaned <- data_questions$employment

data_questions$employment_cleaned[grep("student", data_questions$employment_cleaned)] <- "student"
data_questions$employment_cleaned[startsWith(data_questions$employment_cleaned, "national")] <- "national service"
data_questions$employment_cleaned[startsWith(data_questions$employment_cleaned, "i run")] <- "independent contractor, freelancer, or self-employed"

data_questions %>% 
  group_by(employment_cleaned) %>%
  tally() %>%
  kable()


#City
data_questions %>% 
  group_by(tolower(city)) %>%
  tally() %>%
  kable()


data_questions$city[grep("accra", tolower(data_questions$city))] <- "accra"
data_questions$city[grep("ablekuma", tolower(data_questions$city))] <- "ablekuma"
data_questions$city[startsWith(tolower(data_questions$city), "adenta")] <- "adenta"
data_questions$city[startsWith(tolower(data_questions$city), "awoshie")] <- "awoshie"
data_questions$city[startsWith(tolower(data_questions$city), "kasoa")] <- "kasoa"
data_questions$city[grep("tema", tolower(data_questions$city))] <- "tema"
data_questions$city[startsWith(tolower(data_questions$city), "teshie")] <- "teshie"

data_questions %>% 
  group_by(tolower(city)) %>%
  tally() %>%
  kable()



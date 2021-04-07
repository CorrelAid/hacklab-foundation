library(rio)
library(janitor)
library(tidyr)
library(knitr)
library(dplyr)
library(dbplyr)
library(tmaptools)
library(tmap)

raw_data <- import("~/CorrelAid/hacklab-foundation/data/raw/census-base-anonymized-2020.xlsx")
raw_data <- clean_names(raw_data)

#Removing names and mails
head(raw_data)
raw_data <- raw_data[-c(4:5)]
colnames(raw_data)

#separate dataframe for tools 
skills <- cbind(raw_data$id, raw_data[,11:64])
head(skills)
colnames(skills)[1] <- "id"
colnames(skills)

#transforming into long format
skills <- pivot_longer(skills, cols = 2:55, names_to = "tool", values_to = "level")
head(skills)
#maybe we should replace n/a by 0?


#Creating a dataframe without the skills 
data_questions <- cbind(raw_data[1:10], raw_data[65:96])
colnames(data_questions)
#Changing the column names
##Adapt to Francois 
colnames(data_questions)[4:42] <- c("developer", "hobby", "employment", "region", "city", "age_range", 
                          "learning", "new_tools", "operating_system", "operating_system_2", 
                          "influence", "discovery", "education", "study", "importance_ed", 
                          "change_ed", "job_sat", "orga_size", "work_hours", "overtime", 
                          "on-boarding", "improve_on-boarding", "salary", "job_seeking", "jobsearch", 
                          "info_sources", "communities_1", "communities_2", "age", "gender", 
                          "trans", "sexuality", "ethnicity", "disability", "psychiatric_disorder", 
                          "care", "length", "difficulty", "comments")
colnames(data_questions)

#Extracting the columns I will clean
cleaned_data <- data_questions[1:9]

#Times
#Creating a column including the completion time
cleaned_data$time_s <- cleaned_data$completion_time - cleaned_data$start_time
cleaned_data$time_m <- cleaned_data$time_s/60

head(cleaned_data)

#Developer
cleaned_data %>% 
  group_by(developer) %>%
  tally() 

#some cleaning needed (forming clear categories)

#Employment
cleaned_data$employment <- tolower(cleaned_data$employment)

cleaned_data %>% 
  group_by(employment) %>%
  tally() 

#Some cleaning in a new row 
cleaned_data$employment_cleaned <- cleaned_data$employment

cleaned_data$employment_cleaned[grep("student", cleaned_data$employment_cleaned)] <- "student"
cleaned_data$employment_cleaned[startsWith(cleaned_data$employment_cleaned, "national")] <- "national service"
cleaned_data$employment_cleaned[startsWith(cleaned_data$employment_cleaned, "i run")] <- "independent contractor, freelancer, or self-employed"

cleaned_data %>% 
  group_by(employment_cleaned) %>%
  tally() 

#Question: Transferring "I prefer not to say" to n/a?

#City
cleaned_data %>% 
  group_by(tolower(city)) %>%
  tally() 

cleaned_data$city[grep("accra", tolower(cleaned_data$city))] <- "accra"
cleaned_data$city[grep("ablekuma", tolower(cleaned_data$city))] <- "ablekuma"
cleaned_data$city[startsWith(tolower(cleaned_data$city), "adenta")] <- "adenta"
cleaned_data$city[startsWith(tolower(cleaned_data$city), "awoshie")] <- "awoshie"
cleaned_data$city[startsWith(tolower(cleaned_data$city), "kasoa")] <- "kasoa"
cleaned_data$city[grep("tema", tolower(cleaned_data$city))] <- "tema"
cleaned_data$city[startsWith(tolower(cleaned_data$city), "teshie")] <- "teshie"

cleaned_data %>% 
  group_by(tolower(city)) %>%
  tally() %>%
  kable()

#Geocoding the cities
geocode_OSM("Ghana")

city_geo <- geocode_OSM(cleaned_data$city, as.sf = TRUE, keep.unfound = TRUE)
#No results: 
#Kutunse = kuntunse? 
#Awomaso 
#Tema-Afariwaa
#Outside Ghana = exclude
#Adenta municipality = District of Accra
#Pigfarm = exclude
#Kasoa - Peace town = Kasoa?
#Mamprobi - Banana Inn = mamprobi? 
#La = Los Angeles
#Ajiringanor = district of Madina
#Tema, Prampram = seem to be two cities
#Mampong Akuapem = Maybe Mampong is the city? 
#Larterbiokorshie = District of Accra? 

cleaned_data$city[cleaned_data$city == "kutunse"] <- "kuntunse"
cleaned_data$city[cleaned_data$city == "Adenta municipality"] <- "accra"
cleaned_data$city[cleaned_data$city == "Larterbiokorshie"] <- "accra"
cleaned_data$city[cleaned_data$city == "Kasoa - Peace town"] <- "kasoa"
cleaned_data$city[cleaned_data$city == "Mamprobi - Banana Inn"] <- "mamprobi"
cleaned_data$city[cleaned_data$city == "Ajiringanor"] <- "Madina"

city_geo <- geocode_OSM(cleaned_data$city, as.sf = TRUE, keep.unfound = FALSE)

#Showing the geocoded cities on a map
current.mode <- tmap_mode("view")

tm_shape(city_geo) +
  tm_dots(col = "red")

#According to tmap, we have a lot of developers outside of Ghana. Maybe excluding all of them is not appropriate. 

# restore current mode
tmap_mode(current.mode)


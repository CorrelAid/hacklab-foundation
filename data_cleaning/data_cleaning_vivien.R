library(rio)
library(janitor)
library(tidyr)
library(knitr)
library(dplyr)
library(dbplyr)
library(tmaptools)
library(tmap)

raw_data <- import("~/CorrelAid/hacklab-foundation/data/raw/census-base-anonymized-2020.xlsx")

####General preprocessing ----
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

#Extracting the columns I will clean
cleaned_data <- data_questions[1:10]

#Changing the column names
colnames(cleaned_data) <- c("ID", "start_time", "completion_time", 
                            "profession_1", 
                            "hobby_coding_2", 
                            "employment_3", 
                            "region_4", 
                            "city_5", 
                            "age_range_6", 
                            "new_tool_7")
#Transforming into factors
cleaned_data$profession_1 <- as.factor(cleaned_data$profession_1)
cleaned_data$hobby_coding_2 <- as.factor(cleaned_data$hobby_coding_2)
cleaned_data$employment_3 <- as.factor(cleaned_data$employment_3)
cleaned_data$region_4 <- as.factor(cleaned_data$region_4)
cleaned_data$age_range_6 <- as.factor(cleaned_data$age_range_6)
cleaned_data$new_tool_7 <- as.factor(cleaned_data$new_tool_7)



#Times ----
#Creating a column including the time needed for survey completion
cleaned_data$survey_duration <- cleaned_data$completion_time - cleaned_data$start_time

head(cleaned_data)

#Developer ----
cleaned_data %>% 
  group_by(profession_1) %>%
  tally() 

#Recoding factor levels
cleaned_data$profession_1 <- recode(cleaned_data$profession_1, 
                                    "A Developer" = "I am a developer by profession", 
                                    "Currently in Transition to Dveloper" = "None of these", 
                                    "I am a business admin student learning how to code both as a hobby and towards a career." = 
                                      "I am a student who is learning to code", 
                                    "I am a developer and a student" = "I am a student who is learning to code", 
                                    "I am studying to occupy a data scientist role" = "I am a student who is learning to code", 
                                    "I want to keanr how to cods" = "I am a student who is learning to code", 
                                    "I work closely with Developers" = "None of these" )
#reordering factor levels
cleaned_data$profession_1 <- factor(cleaned_data$profession_1, 
                                    levels(cleaned_data$profession_1)[c(1,4,3,5,6,2)])

#Hobby ----
cleaned_data %>% 
  group_by(hobby_coding_2) %>%
  tally() 

#reordering
cleaned_data$hobby_coding_2 <- factor(cleaned_data$hobby_coding_2, 
                                    levels(cleaned_data$hobby_coding_2)[c(2,1)])


#Employment ----
cleaned_data %>% 
  group_by(employment_3) %>%
  tally() 

#Some cleaning 
cleaned_data$employment_3 <- recode(cleaned_data$employment_3, 
                                    "I run my own business" = "Independent contractor, freelancer, or self-employed", 
                                    "National Service" = "National service", 
                                    "National Service Personnel" = "National service", 
                                    "Student and working" = "Student", 
                                    "Both student and a part time worker" = "Student")

cleaned_data$employment_3 <- factor(cleaned_data$employment_3, 
                                    levels(cleaned_data$employment_3)[c(2,3,5,6,1,7,4)])

#Region ----
cleaned_data %>% 
  group_by(region_4) %>%
  tally() 

cleaned_data$region_4 <- recode(cleaned_data$region_4, 
                                "From Ghana but live outside Ghana" = "Not in Ghana", 
                                "India" = "Not in Ghana", 
                                "Outside Ghana" = "Not in Ghana", 
                                "USA" = "Not in Ghana", 
                                "Central Region" = "Central Region, Ghana", 
                                "Western Region" = "Western Region, Ghana", 
                                "Eastern Region" = "Eastern Region, Ghana", 
                                "Northern Region" = "Northern Region, Ghana")

#Geocoding the regions?
cleaned_data$region_4 <- as.character(cleaned_data$region_4)
region_geo <- geocode_OSM(cleaned_data$region_4, as.sf = TRUE, keep.unfound = FALSE)

current.mode <- tmap_mode("view")

tm_shape(region_geo) +
  tm_dots(col = "red")
tmap_mode(current.mode)


#City ----
cleaned_data %>% 
  group_by(city_5) %>%
  tally() %>%
  kable()

cleaned_data$city_5[grep("accra", tolower(cleaned_data$city_5))] <- "accra"
cleaned_data$city_5[grep("ablekuma", tolower(cleaned_data$city_5))] <- "ablekuma"
cleaned_data$city_5[startsWith(tolower(cleaned_data$city_5), "adenta")] <- "adenta"
cleaned_data$city_5[startsWith(tolower(cleaned_data$city_5), "awoshie")] <- "awoshie"
cleaned_data$city_5[startsWith(tolower(cleaned_data$city_5), "kasoa")] <- "kasoa"
cleaned_data$city_5[grep("tema", tolower(cleaned_data$city_5))] <- "tema"
cleaned_data$city_5[startsWith(tolower(cleaned_data$city_5), "teshie")] <- "teshie"
cleaned_data$city_5[cleaned_data$city_5 == "Adenta municipality"] <- "accra"
cleaned_data$city_5[cleaned_data$city_5 == "Larterbiokorshie"] <- "accra"
cleaned_data$city_5[cleaned_data$city_5 == "Kasoa - Peace town"] <- "kasoa"
cleaned_data$city_5[cleaned_data$city_5 == "Mamprobi - Banana Inn"] <- "mamprobi"
cleaned_data$city_5[cleaned_data$city_5 == "Ajiringanor"] <- "Madina"
cleaned_data$city_5[cleaned_data$city_5 == "Mampong Akuapem"] <- "Mampong"

#Appending ", Ghana" to all cities so we will have only locations in Ghana if there are cities with the same names
cleaned_data$city_5 <- paste0(cleaned_data$city_5, ", Ghana")

#Changing the names of the locations that are not in Ghana
cleaned_data$city_5 <- recode(cleaned_data$city_5, 
                              "Outside Ghana, Ghana" = "0", 
                              "Pigfarm, Ghana" = "0", 
                              "Pune, Ghana" = "Pune, India", 
                              "Cincinnati, OH, Ghana" = "Cincinnati, OH")

#Geocoding
cleaned_data$city_5 <- as.character(cleaned_data$city_5)
city_geo <- geocode_OSM(cleaned_data$city_5, as.sf = TRUE, keep.unfound = FALSE)

#No results for: 
#Kutunse & Awomaso (both seem to be cities in Ghana, maybe ask Hamzah)

#Showing the geocoded cities on a map
current.mode <- tmap_mode("view")

tm_shape(city_geo) +
  tm_dots(col = "red")

# restore current mode
tmap_mode(current.mode)

#We have two respondents outside of Ghana 


##Age Range ---- 
levels(cleaned_data$age_range_6)
#ordering the levels
cleaned_data$age_range_6 <- factor(cleaned_data$age_range_6, 
                                    levels(cleaned_data$age_range_6)[c(6,1,2,3,4,5)])


#New tools
cleaned_data %>% 
  group_by(new_tool_7) %>%
  tally() 

cleaned_data$new_tool_7 <- recode(cleaned_data$new_tool_7, 
                                  "As and when it's necessary for a project" = 
                                    "This varies depending on work and projects", 
                                  "As needed for work and research" = 
                                    "This varies depending on work and projects", 
                                  "I don’t remember the last time" = "Never", 
                                  "I started learning not long ago and so far I've learnt only one language." = 
                                    "Other", 
                                  "i stick to a selected few." = "Never", 
                                  "Project dependent" = 
                                    "This varies depending on work and projects", 
                                  "Usually not...stuck with python for a long time now" = "Never", 
                                  "When I feel the need to" = "This varies depending on work and projects", 
                                  "When i have someone to help me learn it" ="Other", 
                                  "When Neccessary" = "This varies depending on work and projects", 
                                  "When the need arises" = "This varies depending on work and projects", 
                                  "When there is a need to"= "This varies depending on work and projects", 
                                  "When work requires it" = "This varies depending on work and projects")

#ordering the levels
cleaned_data$new_tool_7 <- factor(cleaned_data$new_tool_7, 
                                   levels(cleaned_data$new_tool_7)[c(2,6,7,5,1,4,8)])

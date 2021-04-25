library(rio)
library(tidyverse)
library(tmaptools)
library(leaflet)
library(scales)


clean_data <- import("~/CorrelAid/hacklab-foundation/data/clean/clean_all_Qs.rds")

#Visualizing demographic data

###Profession ----
clean_data %>% 
  count(profession_1) %>%
  ggplot(aes(x = reorder(profession_1, n), y = n)) + 
  geom_col() + 
  coord_flip()

#Only for developers in Ghana
data_ghana <- clean_data %>%
  filter(region_4 != "Not in Ghana")

data_ghana %>% 
  count(profession_1) %>%
  ggplot(aes(x = reorder(profession_1, n), y = n)) + 
  geom_col() + 
  coord_flip()
#Filtering for country does not really make sense, because there are only 4 respondents outside of Ghana

#Hobby----
clean_data %>% 
  group_by(hobby_coding_2, profession_1) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(profession_1, n), y = n, fill = hobby_coding_2)) + 
  geom_bar(stat = "identity", position="dodge") + 
  coord_flip() 
  
#Employment ----
clean_data %>% 
  count(employment_3) %>%
  ggplot(aes(x = reorder(employment_3, n), y = n)) + 
  geom_col() + 
  coord_flip()

colnames(clean_data)

###Region ----
#Map for regions
cleaned_data$region_4 <- as.character(cleaned_data$region_4)
region_geo <- geocode_OSM(cleaned_data$region_4, as.sf = TRUE, keep.unfound = FALSE)
region_geo
m_regions <- leaflet(data = region_geo) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(~ lon, ~ lat)
m_regions

#Barplot
clean_data %>% 
  count(region_4) %>%
  ggplot(aes(x = reorder(region_4, n), y = n)) + 
  geom_col() +
  coord_flip()

##Cities ----

data_city <- cleaned_data %>%
  select(ID, city_5) %>%
  filter(!city_5 %in% c("0", "Cincinnati, OH", "Pune, India"))

city_geo <- geocode_OSM(data_city$city_5, as.sf = TRUE, keep.unfound = FALSE)
map_cities <- leaflet(data = city_geo) %>%
  addTiles() %>% 
  addMarkers(~ lon, ~ lat, 
             clusterOptions = markerClusterOptions())
map_cities

colnames(clean_data)

###age ---
clean_data %>% 
  count(age_range_6) %>%
  ggplot(aes(x = reorder(age_range_6, n), y = n)) + 
  geom_col() +
  coord_flip()

#Most developers are between 20 and 25

#profession grouped by age range
clean_data %>% 
  group_by(profession_1, age_range_6) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = age_range_6, y = n, fill = profession_1)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

#Most of the younger respondents were students, but there are also a lot of developers at ages 20 to 25

###Gender ----
clean_data %>%
  ggplot(aes(x = gender_35)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  coord_flip()

#Employment grouped by gender
gender_data %>% 
  group_by(gender_35, employment_3) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(employment_3, n), y = n, fill = gender_35)) + 
  geom_bar(stat = "identity") + 
  coord_flip() 
#less women responded the survey, women seem to be self-employed less often

#Only developers 
clean_data %>%
  filter(profession_1 == "I am a developer by profession" & gender_35 != "NA") %>%
  ggplot(aes(x = gender_35)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)))+ 
  coord_flip()

#Only students
levels(clean_data$profession_1)
clean_data %>%
  filter(profession_1 == "I am a student who is learning to code") %>%
  ggplot(aes(x = gender_35)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  coord_flip()

#There is a greater share of women in students 
#May indicate that Ghana's developer community is becoming more representative

#Transgender
clean_data %>%
  count(transgender_36) %>%
  ggplot(aes(x = transgender_36, y = n)) + 
  geom_col() + 
  coord_flip()

#Sexual orientation ----
clean_data %>%
  filter(sexual_orientation_37 != "NA" & sexual_orientation_37 != "Prefer not to say") %>%
  count(sexual_orientation_37) %>%
  ggplot(aes(x = reorder(sexual_orientation_37, n), y = n)) + 
  geom_col() + 
  coord_flip()

#Ethnicity --- 
clean_data %>% 
  filter(ethnicity_38 != "NA" & ethnicity_38 != "I prefer not to answer") %>%
  count(ethnicity_38) %>%
  ggplot(aes(x = reorder(ethnicity_38, n), y = n)) + 
  geom_col()+ 
  coord_flip()

#Disabilities ---- 
clean_data %>%
  count(disabilitys_39) %>%
  ggplot(aes(x = disabilitys_39, y = n))+ 
  geom_col() + 
  coord_flip()

clean_data %>%
  count(mental_illness_40) %>%
  ggplot(aes(x = mental_illness_40, y = n))+ 
  geom_col() + 
  coord_flip()

#Unsure what to do with these variables
#maybe have a graph with one bar for disabilities and one for psychiatric disorders? 
#Cleaning needed 
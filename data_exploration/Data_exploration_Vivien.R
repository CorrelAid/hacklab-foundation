library(rio)
library(tidyverse)
library(tmaptools)
library(leaflet)


clean_data <- import("~/CorrelAid/hacklab-foundation/data/clean/clean_all_Qs.rds")

#Visualizing demographic data

###Profession ----
clean_data %>% 
  count(profession_1) %>%
  ggplot(aes(x = reorder(profession_1, n), y = n)) + 
  geom_col() + 
  coord_flip()

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

#profession grouped by age range
clean_data %>% 
  group_by(profession_1, age_range_6) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(profession_1, n), y = n, fill = age_range_6)) + 
  geom_bar(stat = "identity") + 
  coord_flip()

###Gender ----
#Employment grouped by gender
clean_data %>% 
  group_by(gender_35, employment_3) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(employment_3, n), y = n, fill = gender_35)) + 
  geom_bar(stat = "identity") + 
  coord_flip() 

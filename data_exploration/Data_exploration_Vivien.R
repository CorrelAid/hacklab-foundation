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
clean_data$region_4 <- as.character(clean_data$region_4)
region_geo <- geocode_OSM(clean_data$region_4, as.sf = TRUE, keep.unfound = FALSE)

#Including number of occurences for every region
region_geo_test <- region_geo %>%
  group_by(query) %>%
  summarize(n = n(), lat = mean(lat), lon = mean(lon))

#map for regions (very unbalanced)
m_regions <- leaflet(data = region_geo_test) %>%
  addTiles() %>%
  addCircles(~ lon, ~ lat, 
             weight= 1, 
             radius = ~n*500, 
             label = ~query)
m_regions

#Barplot
clean_data %>% 
  count(region_4) %>%
  ggplot(aes(x = reorder(region_4, n), y = n)) + 
  geom_col() +
  coord_flip()

##Cities ----

#Selecting relevant variables and respondents from Ghana
data_city <- clean_data %>%
  select(ID, city_5, profession_1) %>%
  filter(!city_5 %in% c("0", "Cincinnati, OH", "Pune, India", "kutunse, Ghana", "Awomaso, Ghana")) 

#Geocoding
data_city_geo <- data_city %>%
  mutate(
    geocode = geocode_OSM(data_city$city_5, as.sf = TRUE, keep.unfound = TRUE)
  )

#Overall map with clusters (individual icons in violet)
#maybe we should change the color of the clusters (depends of overall design)
violet_icon <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-violet.png", 
  iconWidth = 24,
  iconHeight = 32) 

map_cities <- leaflet(data = data_city_geo) %>%
  addTiles() %>% 
  addMarkers(~ geocode$lon, ~ geocode$lat, 
             clusterOptions = markerClusterOptions(), 
             icon = violet_icon)
map_cities

#Map grouped for profession 
data_geo_prof <- data_city_geo %>%
  filter(profession_1 != "None of these")
  
data_geo_prof$profession_1 <- recode(data_geo_prof$profession_1, 
                        "I am a developer by profession" = "Professional Developers", 
                        "I am not primarily a developer, but I write code sometimes as part of my work" = "Coding at Work",
                        "I am a student who is learning to code" = "Students", 
                        "I code primarily as a hobby" = "Hobby", 
                        "I used to be a developer by profession, but no longer am" = "Retired")


Icons_prof <- icons(
  iconUrl = ifelse(data_geo_prof$profession_1 == "Professional Developers", 
                   "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-violet.png", 
                   "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png"),
  iconWidth = 16, iconHeight = 20
)

#Creating a map with different icons for professionals and non-professionals
prof_map <- leaflet(data = data_geo_prof) %>%
  addTiles() %>%
  addMarkers(~geocode$lon, ~geocode$lat, 
             icon = ~Icons_prof, 
             label = ~profession_1)
prof_map


##Map only for professional developers
city_geo_prof <- data_city_geo %>%
  filter(profession_1 == "I am a developer by profession")

map_professional <- leaflet(data = city_geo_prof) %>%
  addTiles() %>% 
  addMarkers(~ geocode$lon, ~ geocode$lat, 
             clusterOptions = markerClusterOptions())
map_professional

#Map only for students
city_geo_students <- data_city_geo %>%
  filter(profession_1 == "I am a student who is learning to code")

map_students <- leaflet(data = city_geo_students) %>%
  addTiles() %>%
  addMarkers(~geocode$lon, ~geocode$lat, 
            clusterOptions = markerClusterOptions())
map_students


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

clean_data %>% 
  group_by(mental_illness_40) %>%
  tally() 

clean_data %>% 
  group_by(disabilitys_39) %>%
  tally() 
#Unsure what to do with these variables
#maybe have a graph with one bar for disabilities and one for psychiatric disorders? 
#Cleaning needed 

##Dependents ----
levels(clean_data$dependents_41)
basic_barplot(my_df = compute_perc(clean_data, dependents_41),
               a_factor = dependents_41, its_values = perc, labels = perc_label, 
               title = "Q41: Do you have any dependents (e.g. children, elders, or others) that you care for?")

#Grouping by gender
clean_data %>% 
  filter(dependents_41 != "NA" & dependents_41 != "I prefer not to say") %>%
  group_by(gender_35, dependents_41) %>%
  summarise(n= n()) %>%
  ggplot(aes(x = reorder(dependents_41, n), y = n, fill = gender_35)) + 
  geom_bar(stat = "identity") + 
  coord_flip()


###Survey length ----
colnames(clean_data)
basic_barplot(my_df = compute_perc(clean_data, survey_length_42),
              a_factor = survey_length_42, 
              its_values = perc,
              labels = perc_label, 
              title = "How long was the survey?")

basic_barplot(my_df = compute_perc(clean_data, survey_difficulty_43),
              a_factor = survey_difficulty_43, 
              its_values = perc,
              labels = perc_label, 
              title = "How difficult was the survey?")

###Are we using the any final thoughts question? 


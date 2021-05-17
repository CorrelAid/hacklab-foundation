# geocodes the data

library(tidyverse)
library(tmaptools)

Qs <- rio::import("../data/clean/clean_all_Qs.rds")

data_city <- Qs %>%
  select(ID, city_5, profession_1) %>%
  filter(!city_5 %in% c("0", "Cincinnati, OH", "Pune, India", "kutunse, Ghana", "Awomaso, Ghana")) 

#Geocoding:
data_city_geo <- data_city %>%
  mutate(
    geocode = geocode_OSM(data_city$city_5, as.sf = TRUE, keep.unfound = TRUE)
  )

data_city_geo$profession_1 <- recode(data_city_geo$profession_1, 
                                     "I am a developer by profession" = "Professional Developers", 
                                     "I am not primarily a developer, but I write code sometimes as part of my work" = "Coding at Work",
                                     "I am a student who is learning to code" = "Students", 
                                     "I code primarily as a hobby" = "Hobby", 
                                     "I used to be a developer by profession, but no longer am" = "Retired")

rio::export(data_city_geo, "../data/clean/data_city_geo.rds")



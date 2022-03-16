####
#### Binding on user and date taken data to the prediction outputs from Bumsuk
#### So that we can calculate Activity Photo user-days (APUD)
#### Writes out: FlickrMiddlefork_predicted_users.geojson
####
#### 6/25/20

library(tidyverse)
library(sf)

setwd("~/Documents/Recquity/MiddleFork/")

# read in photos
photos <- read_sf("Activities/CNNModel/FinalPredictions20210420/FlickrMiddlefork_predicted.shp")
#photos <- read_sf("Activities/CNNModel/FinalPredictions20210420/FlickrMBSMtLoop_predicted.shp")
photos

# read in userids (global table)
users <- read_csv("Activities/PhotoData/TwitterFlickr_AOI_allyrs.csv")
users

# ok. I want date and userid bound on to the predictd photos

photos_users <- photos %>%
  dplyr::select(-latitude, -longitude) %>%
  left_join(users, by = c("PhotoID" = "photo_id"))

# I'd like to anonymize this
user_list <- tibble(owner_name = photos_users$owner_name)
user_key <- user_list %>%
  distinct() %>%
  mutate(userid = paste0("User", sprintf("%03d", seq(1:730)))) # 932 for mtn loop, 730 for mf

# ok, now use the new anonymized userid to replace owner_name
photos_users_anon <- photos_users %>%
  left_join(user_key, by = "owner_name") %>%
  dplyr::select(-owner_name, -accuracy)

# Write it out
write_sf(photos_users_anon, "Activities/CNNModel/FinalPredictions20210420/FlickrMiddlefork_predicted_users.geojson")
#write_sf(photos_users_anon, "Activities/CNNModel/FinalPredictions20210420/FlickrMBSMtLoop_predicted_users.geojson")

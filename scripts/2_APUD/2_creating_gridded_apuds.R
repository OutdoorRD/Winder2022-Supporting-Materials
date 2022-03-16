###
### Creating gridded APUD shapefiles
### 5/11/21
###
### Forked from MappingPhotoActivities.R on 5/11/21
###
### Requires: AOI and output from `addingUserData.R`

library(tidyverse)
library(sf)
library(raster)
library(lubridate)

setwd("~/Documents/Recquity/MiddleFork/")
dddd <- gsub(Sys.Date(), pattern="-", replacement="")

# comment one of these out
#location <- "MiddleFork"
location <- "MtnLoop"

if(location == "MiddleFork"){
  photos <- read_sf("Activities/CNNModel/FinalPredictions20210420/FlickrMiddlefork_predicted_users.geojson")
  aoi <- read_sf("GIS/middlefork_AOI_v2.shp")
}else if(location == "MtnLoop"){
  photos <- read_sf("Activities/CNNModel/FinalPredictions20210420/FlickrMBSMtLoop_predicted_users.geojson")
  aoi <- read_sf("GIS/MtLoopAOI_coarse.shp")
}else{
  print("Check your location spelling")
}

# reproject both to WGS84 UTM10N
photos_10 <- st_transform(photos, crs = 32610)
aoi_10 <- st_transform(aoi, crs = 32610)

top1_pts <- photos_10 %>% dplyr::select(Top1, userid, date_taken)

# drop out noactivity and NA, and flooding
# as of 5/18/21, also removing "fishing" and "trailrunning" because of poor model performance
# and "horseriding" on 7/15/21
top1_pts_sub <- top1_pts %>% 
  filter(!Top1 %in% c("noactivity", "pplnoactivity", "flooding", "fishing", "trailrunning", "horseriding"),
                    !is.na(Top1))

# make grid
aoi_hex_sfc <- st_make_grid(aoi_10, cellsize = 2000, square = TRUE) 
# assign cids (cell ids)
cids <- 1:length(aoi_hex_sfc)
aoi_hex <- st_sf(cid = cids, geometry = aoi_hex_sfc)

#ggplot(aoi_hex_in) +geom_sf()

# remove hexes outside of aoi
aoi_hex_in <- aoi_hex[aoi_10, op = st_intersects]

# intersect with grid
top1_int <- st_intersection(top1_pts_sub, aoi_hex_in)

# calculate APUD per grid cell
top1_unique <- top1_int %>%
  st_drop_geometry() %>%
  mutate(date = date(date_taken)) %>%
  dplyr::select(-date_taken) %>%
  distinct()

daily_apud <- top1_unique %>%
  group_by(Top1, cid, date) %>%
  summarise(APUD = n())
daily_apud

annual_apud <- daily_apud %>%
  group_by(Top1, cid, year = year(date)) %>%
  summarise(APUD = sum(APUD))

total_apud <- daily_apud %>%
  group_by(Top1, cid) %>%
  summarise(APUD = sum(APUD))
total_apud


# now, make sure that I have every cid*activity combo
total_apud_com <- total_apud %>%
  complete(Top1, cid = aoi_hex_in$cid, fill = list(APUD = NA))
total_apud_com

# make total_apud spatial again
total_apud_sf <- aoi_hex %>%
  left_join(total_apud_com)
total_apud_sf

########### How about diversity of activities? #########
## Calculate number of different activities occuring in each cid (equivalent to activity "richness", ala species richness)
activities_cid <- total_apud_com %>%
  group_by(cid) %>%
  summarise(activities = sum(!is.na(APUD)),
            APUD = sum(APUD, na.rm = TRUE),
            act_p_APUD = activities/log1p(APUD))
activities_cid

activities_sf <- aoi_hex %>% 
  left_join(activities_cid)

num_act_plot <- ggplot(activities_sf) + # %>% filter(activities != 0)) +
  #geom_sf(data = aoi_hex) +
  geom_sf(aes(fill = activities), size = .1) +
  scale_fill_viridis_c() +
  labs(title = paste(location, "Number of Distinct Activities"))
num_act_plot

# Ok. Let's examine this a bit, and write it out
activities_sf %>%
  arrange(desc(activities))
# cid 45 & 141 (mtnloop) have 10 distinct activities. I think this is out of 11 possible - let's check
# middlefork: cid 45 has 11, cids 10 and 149 each have 10 activities

total_apud_com %>% filter(cid == "45") #hmm. actually out of 12. None for fishing or trailrunning
total_apud_com %>% filter(cid == "141") # missing the same 2

# write out the diversity grid
st_write(activities_sf, paste0("GIS/", location, "_activities_by_grid_", dddd, ".geojson"))




# let's at least look at the correlation between number activities and total apud
ggplot(activities_cid) +
  geom_point(aes(y = activities, x = log(APUD)))

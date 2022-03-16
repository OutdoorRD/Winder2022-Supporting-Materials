###
### Preparing predictors in order to:
### Relating number of distinct activities and/or activity diversity to
###  underlying landscape features
### MF and Mtn Loop
### 10/13/20
###

library(sf)
library(tidyverse)
library(elevatr)

dddd <- gsub(Sys.Date(), pattern="-", replacement="")

setwd("~/Documents/Recquity/MiddleFork/")
# read in the shapefiles w/ # distinct activities and activity diversity index

mtnloop <- read_sf("GIS/MtnLoop_activities_by_grid_20210715.geojson")
mf <- read_sf("GIS/MiddleFork_activities_by_grid_20210715.geojson")

# ok. Now we need some landscape features to test against
  # What do I want? Probably: roads/distance to road, rivers, lakes, trail (official), elevation, campgrounds, parking lot??
# Some of these can come from Emily Wilkin's code for downloading OSM data, 
# but others I think I already have data layers for

# first I want to join my two aois
mtn2 <- mtnloop %>%
  mutate(cid = paste0(cid, "_mtn"))
mf2 <- mf %>%
  mutate(cid = paste0(cid, "_mf"))
aoi <- rbind(mtn2, mf2) %>% rename(pid = cid)

ggplot(aoi) + geom_sf()

### Now, copying some code from mar_tourism/preparing_non_climate_predictors.R

PresAbsFunc <- function(predictor, aoi = aoi){
  predName <- substitute(predictor)            # get the name of the predictor you entered as an argument
  pred_int <- st_intersection(aoi, predictor)
  pred_pid <- pred_int$pid
  aoi_preds <- aoi %>%
    st_set_geometry(NULL) %>%
    dplyr::select(pid) %>%
    mutate("{predName}" := if_else(pid %in% pred_pid, 1, 0))
}

# and what are my p/a variables?
camping <- st_read("GIS/PredictorsCreated/camping.geojson")
lakes <- st_read("GIS/PredictorsCreated/lakes.geojson")
picnic <- st_read("GIS/PredictorsCreated/picnicking.geojson")
rivers <- st_read("GIS/PredictorsCreated/rivers.geojson")
trails <- st_read("GIS/PredictorsCreated/trails.geojson")

# Want to do distance to road
roads <- st_read("GIS/PredictorsCreated/roads.geojson")

# let's also create a predictor of only campgrounds, not "group camping"
campgrounds <- camping %>% filter(MARKERACTI == "Campground Camping")

# And want to find elevation of centroid

# Run Presence/Absence
# note that I tried to use my PresAbsFunc in a loop, but it breaks the nice "{predName}" functionality

camping_pid <- PresAbsFunc(camping, aoi)
campgrounds_pid <- PresAbsFunc(campgrounds, aoi)
lakes_pid <- PresAbsFunc(lakes, aoi) # slow
picnic_pid <- PresAbsFunc(picnic, aoi)
rivers_pid <- PresAbsFunc(rivers, aoi)
trails_pid <- PresAbsFunc(trails, aoi)


# bind them all together
predictors <- aoi %>% 
  left_join(camping_pid) %>%
  left_join(campgrounds_pid) %>%
  left_join(lakes_pid) %>%
  left_join(picnic_pid) %>%
  left_join(rivers_pid) %>%
  left_join(trails_pid) 

predictors

# ok. distance from road

########### Distance to nearest road #####

# calculate distance to nearest road
road_dists <- st_distance(predictors, roads)
road_min_dist <- apply(road_dists, 1, min)

predictors$road_min_dist <- road_min_dist
ggplot(predictors) + geom_sf(aes(fill = road_min_dist))
predictors
######

##### Elevation #####
# let's find the elevation at the centroid of each hex
aoi_centroids <- st_centroid(aoi)
aoi_centroids

# pings the USGS Elevation Point Query Service
elevs <- get_elev_point(aoi_centroids, src = "epqs")
#ggplot(elevs) + geom_sf(aes(col = elevation))

#st_write(elevs, "GIS/PredictorsCreated/elevation.geojson")

# the pinging is slow, so just read back in those elevations
elevs <- read_sf("GIS/PredictorsCreated/elevation.geojson")


# join back on to predictors
elevs_tib <- elevs %>% st_drop_geometry() %>% dplyr::select(pid, elevation, elev_units)

predictors2 <- predictors %>%
  left_join(elevs_tib)

# write it out
#st_write(predictors2, "GIS/PredictorsCreated/Predictors_20201014.geojson")

######## 10/15/20 Add Wilderness #######
#predictors2 <- read_sf("GIS/PredictorsCreated/Predictors_20201014.geojson")
wilderness <- read_sf("GIS/PredictorsCreated/wilderness.geojson")
ggplot(wilderness) + geom_sf(data = predictors2, aes()) + geom_sf()

# I want percent coverage of wilderness
aoi <- predictors2 %>% dplyr::select(pid)

# intersect
wilderness_int <- st_intersection(aoi, wilderness) # 

# calculate the area of each intersected polygon (only includes wilderness)
wilderness_int$area <- unclass(st_area(wilderness_int)) 
wilderness_areas <- wilderness_int %>%
  st_set_geometry(NULL) %>%
  group_by(pid) %>%
  summarise(wilderness_area = sum(area)) ## 

predictors2$cellarea <- unclass(st_area(predictors2))

preds_wilderness <- predictors2 %>%
  left_join(wilderness_areas, by = "pid") %>%
  mutate(prop_wilderness = if_else(is.na(wilderness_area), 0, wilderness_area/cellarea)) %>%
  dplyr::select(-wilderness_area)

ggplot(preds_wilderness) + geom_sf(aes(fill = prop_wilderness))

# write it out
#st_write(preds_wilderness, "GIS/PredictorsCreated/Predictors_20201015.geojson")

### I added campgrounds (up higher), so writing out a new version
st_write(preds_wilderness, paste0("GIS/PredictorsCreated/Predictors_", dddd, ".geojson"))

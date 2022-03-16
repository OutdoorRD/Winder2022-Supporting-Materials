##
## Preparing predictors for the activities work
## 10/14/20
##

library(sf)
library(tidyverse)

# ok. my gridded aois are in epsg 32610 (wgs84, utm 10n)
# I created a rough bbox that includes both of them, so let's clip all these component layers to that
bbox <- st_read("~/Documents/Recquity/MiddleFork/GIS/rough_bbox_both_regions.shp")


#### Roads ####
# Let's start with roads (working with shapefile that i exported from the gdb, because of downstream errors with st_intersect otherwise)
roads <- st_read("~/Documents/GIS/WA_PublicRoads/Created/WAPR.shp")

# drop z and m dimensions, then transform
roads_flat <- st_zm(roads)
roads_32 <- st_transform(roads_flat, crs = 32610)

# intersect with bbox
roads_sub <- st_intersection(roads_32, bbox)
ggplot(roads_sub) + geom_sf()

# great. write it out
#st_write(roads_sub, "~/Documents/Recquity/MiddleFork/GIS/PredictorsCreated/roads.geojson")

rm(bbox_32, roads, roads_32, roads_flat)

###########

#### Campgrounds and picnic areas #####
recopps <- st_read("~/Documents/GIS/S_USA.RECREATIONOPPORTUNITIES/S_USA.RECREATIONOPPORTUNITIES.shp")

# transfomring
recopps_32 <- st_transform(recopps, crs = 32610)
rec_sub <- st_intersection(recopps_32, bbox)

camping <- rec_sub %>%
  select(MARKERACTI) %>%
  filter(str_detect(MARKERACTI, "Camping"))

picnicking <- rec_sub %>%
  select(MARKERACTI) %>%
  filter(str_detect(MARKERACTI, "Picnicking"))

trailheads <- rec_sub %>%
  select(MARKERACTI) %>%
  filter(str_detect(MARKERACTI, "Trailhead"))

# write em out
#st_write(camping, "~/Documents/Recquity/MiddleFork/GIS/PredictorsCreated/camping.geojson")
#st_write(picnicking, "~/Documents/Recquity/MiddleFork/GIS/PredictorsCreated/picnicking.geojson")
#st_write(trailheads, "~/Documents/Recquity/MiddleFork/GIS/PredictorsCreated/trailheads.geojson")

#### Trails #####
trails <- read_sf("~/Documents/GIS/NVUM/Trails_WA.shp")
trails_32 <- st_transform(trails, crs = 32610)
trails_sub <- st_intersection(trails_32, bbox)
#ggplot(trails_sub) + geom_sf()

# write em out
#st_write(trails_sub, "~/Documents/Recquity/MiddleFork/GIS/PredictorsCreated/trails.geojson")

##### Water ######
# i exported from the gdb as shapefiles, to avoid problems here
streams <- read_sf("~/Documents/GIS/Water/Created/NHDFlowline.shp")
# subset to only include larger streams. 
rivers <- streams %>% filter(StreamOrde >= 5) %>% st_zm()

rivers_32 <- st_transform(rivers, crs = 32610)
rivers_sub <- st_intersection(rivers_32, bbox)

## lakes
lakes <- read_sf("~/Documents/GIS/Water/Created/NHDWaterbody.shp")

# for now, let's only include waterbodies > 0.05 sqkm in area
lakes2 <- lakes %>% filter(AreaSqKm > 0.05)
lakes_32 <- st_transform(lakes2, crs = 32610)
lakes_sub <- st_intersection(lakes_32, bbox)

# write em out
#rivers <- st_write(rivers_sub, "~/Documents/Recquity/MiddleFork/GIS/PredictorsCreated/rivers.geojson")
#lakes <- st_write(lakes_sub, "~/Documents/Recquity/MiddleFork/GIS/PredictorsCreated/lakes.geojson")

### Wilderness #####
wilderness <- read_sf("~/Documents/GIS/S_USA.Wilderness/S_USA.Wilderness.shp")
wild_32 <- st_transform(wilderness, crs = 32610)
wild_sub <- st_intersection(wild_32, bbox)

# write it out
#wilderness <- st_write(wild_sub, "~/Documents/Recquity/MiddleFork/GIS/PredictorsCreated/wilderness.geojson")

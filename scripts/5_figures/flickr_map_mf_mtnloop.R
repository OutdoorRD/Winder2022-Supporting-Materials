### Making map of flickr photos for activities paper
### Forked from socialmediamap_MBS.R in the `forestmap` branch of viz-experience
### Forked from the Skykomish and Darrington Ranger Districts social media maps on 11/30/20
### Forked from the MBS/NNM social media maps on 8/10/20

library(sf)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)
#library(ggsn)
library(ggspatial)
library(ggmap)
#library(maptools)
#library(tmap)
library(basemaps)

setwd("~/Documents/Recquity/MiddleFork/")
dddd <- gsub(Sys.Date(), pattern = "-", replacement = "")
#dddd <- "20201201" # because I'm just recreating a map from that day as a pdf, not adding anything new
#### Reading in map data

MBS <- read_sf("~/Documents/VisitorExperiences/viz-experience/data/gis/MBS_ranger_district_boundaries_wgs84.shp")
MBS <- MBS %>%
  mutate(Name_short = paste(str_extract(DISTRICTNA, "^.+(?=\\sR)")))

MBS <- st_make_valid(MBS)

roads <- read_sf("~/Documents/GIS/WA_PublicRoads/washington_highway.shp")
roads_primary <- roads %>% filter(TYPE %in% c("motorway", "primary", "secondary"))#, "tertiary")) 
cities <- read_sf("~/Documents/GIS/WA_cities/CityPoints/CityPoints.shp")
mf_road <- read_sf("~/Documents/GIS/WA_PublicRoads/Created/middle_fork_road.geojson")

seattle <- cities %>% filter(NAME == "Seattle")

#wilderness <- read_sf("~/Documents/GIS/S_USA.Wilderness/S_USA.Wilderness.shp")
#wilderness
#
#wild_84 <- st_transform(wilderness, crs = 4326)
#
## intersect with RDs
#mbs_wilds <- st_intersection(wild_84, MBS)
#mbs_wilds
#ggplot() + geom_sf(data = MBS) + geom_sf(data = mbs_wilds, fill = "blue", alpha = .2) 

states <- ne_states(country = 'United States of America', returnclass = "sf")
wa <- states %>% filter(postal == "WA")
#nm <- states %>% filter(postal == "NM")


## TODO: need to read in the appropriate files from "gis" first
#instag_mbs_sf <- read_sf("data/gis/instagram_mbs.geojson")
#flickr_mbs_sf <- read_sf("data/gis/flickr_mbs_anon.geojson")
#twitter_mbs_sf <- read_sf("data/gis/twitter_mbs_anon.geojson")
#alltrails_mbs_sf <- read_sf("data/gis/alltrails_mbs.geojson")
#wta_mbs_sf <- read_sf("data/gis/wta_mbs.geojson")

flickr_mf <- read_sf("Activities/CNNModel/FinalPredictions20210420/FlickrMiddlefork_predicted_users.geojson")
flickr_mtn <- read_sf("Activities/CNNModel/FinalPredictions20210420/FlickrMBSMtLoop_predicted_users.geojson")

aoi_mf <- read_sf("GIS/middlefork_AOI_v2.shp")
aoi_mtn <- read_sf("GIS/MtLoopAOI_coarse.shp")


#### Adding roads and a reference map to match the visitation maps ####
# make a reference map
wa_ref <- ggplot() +
  geom_sf(data = wa, fill = NA) +
  #geom_sf(data = MBS, fill = "gray90", size = .3) +
  geom_sf(data = aoi_mf, fill = 'black', size = .3) +
  geom_sf_text(data = aoi_mf, label = "A", nudge_x = 1) +
  geom_sf(data = aoi_mtn, fill = "black", size = .3) +
  geom_sf_text(data = aoi_mtn, label = "B", nudge_x = 1) +
  geom_sf(data = seattle, size = 2, shape = 23, fill = "black") + 
  coord_sf(datum = NA) +
  theme_void() 
wa_ref

# reference map with box
wa_ref2 <- ggplot() +
  geom_sf(data = wa, fill = NA) +
  #geom_sf(data = MBS, fill = "gray90", size = .3) +
  geom_sf(data = aoi_mf, fill = NA, size = .3) +
  #geom_sf_text(data = aoi_mf, label = "A", nudge_x = 1) +
  geom_sf(data = aoi_mtn, fill = NA, size = .3) +
  #geom_sf_text(data = aoi_mtn, label = "B", nudge_x = 1) +
  geom_spatial_rect(aes(xmin = -122.14, xmax = -121.09, ymin = 47.33, ymax =  48.33), fill = NA) +
  #geom_sf(data = seattle, size = 2, shape = 23, fill = "black") + 
  coord_sf(datum = NA) +
  theme_void() 
wa_ref2

socmedmap <- 
ggplot(MBS) +
  geom_sf(data = MBS, aes(fill = 'nonwild')) +
  geom_sf(data = roads_primary, col = "gray65") +
  geom_sf(data = seattle, size = 2, shape = 23, fill = "black") + 
  #geom_sf(data = cities %>% filter(MajorCity == "yes")) + 
  geom_sf(data = flickr_mf, aes(col = 'a'), alpha = .25, size = .5) +
  geom_sf(data = flickr_mtn, aes(col = 'a'), alpha = .25, size = .5) +
  geom_sf(data = aoi_mf, aes(color = 'b'), fill = NA, size = 1) +
  geom_sf(data = aoi_mtn, aes(color = 'b'), fill = NA, size = 1) +
  scale_colour_manual(name = NULL, 
                      values =c('a'='#7b3294','b'='black'), 
                      labels = c("Flickr photos", "Study region"),
                      guide = guide_legend(override.aes = list(linetype = c("blank",  "solid"),
                                                               shape = c(19, NA),
                                                               size = c(3, 1),
                                                               alpha = c(1,1)),
                                           order = 1)) + #, guide = "none") +
  # scale_size_continuous(range = c(.5, 6), name = "Instagram/AllTrails/WTA \nUser-days",
  #                       trans = "log10") + 
  scale_fill_manual(name = NULL, 
                    values = c('nonwild' = 'gray90'),#, 'wild' = 'white'), 
                    labels = c("Mt Baker-Snoqualmie \nNational Forest"))+
  #geom_sf_label(data = MBS, aes(label = Name_short), nudge_x = 0) +
  #scalebar(data = MBS, dist = 15, dist_unit = "km", transform = TRUE, model = "WGS84", 
  #        border.size = .1, st.size = 4, location = "bottomright") +
  annotation_scale(location = "bl", style = "ticks", pad_x = unit(.4, "cm")) +
  annotation_north_arrow(location = "br",
                         height = unit(.75, "cm"),
                         width = unit(.75, "cm")) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  #coord_sf(xlim = c(-122.1, -120.88), ylim = c(46.8, 49)) +
  coord_sf(xlim = c(-122.13, -121.13), ylim = c(47.4, 48.3)) +
  #geom_label(x = -120.98, y = 47.20, label = "I 90") +
  #geom_label(x = -120.9, y = 47.8, label = "Rt 2") +
  geom_label(x = -121.55, y = 47.66, label = "Middle Fork") +
  geom_label(x = -121.7, y = 47.95, label = "Mountain Loop") +
  geom_label(x = -122.07, y = 47.65, label = "Seattle") +
  #labs(caption = "Outdoor Recreation & Data Lab, June 2021") +
  guides(fill = guide_legend(order = 4)#, 
        # col = guide_legend(order = 2, override.aes = list(size = 2)), 
        # size = guide_legend(order = 3)
        ) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid.major = element_line(color = "transparent"), 
        plot.caption = element_text(hjust = c(2.75)), 
        legend.position = c(1.3, .5),
        legend.spacing.y = unit(-0.1, "cm"))
socmedmap

# see about drawing together
ggdraw() +
  draw_plot(socmedmap, x = -.18, y = 0) +
  draw_plot(wa_ref2, x = .61, y = .3, width = .25)

ggsave(paste0("Activities/figs/SiteMap/site_map_", dddd, ".png"), width = 6.5, height = 4.8, units = "in")
knitr::plot_crop(paste0("Activities/figs/SiteMap/site_map_", dddd, ".png"))


# save as pdf
#ggsave(paste0("Activities/figs/site_map_", dddd, ".pdf"), width = 5.4, height = 4, units = "in")

### Trying to add some basemaps and seeing if that is better

st_bbox(aoi_mf)

basemap_mf <- get_stamenmap(bbox = c(left = -121.78, right = -121.2, bottom = 47.39, top = 47.65), maptype = "terrain-background", zoom = 10)
ggmap(basemap_mf)

mf_map <- ggmap(basemap_mf) +
  #geom_sf(data = mf_road, col = "gray25", inherit.aes = FALSE) +
  geom_sf(data = roads_primary, aes(col = "roads"), inherit.aes = FALSE) +
  geom_sf(data = flickr_mf, aes(col = 'flickr'), alpha = .3, size = .4, inherit.aes = FALSE) +
  geom_sf(data = aoi_mf, aes(col = "aoi"), size = 1, fill = NA, inherit.aes = FALSE) +
  scale_color_manual(name = NULL,
                     breaks = c('flickr', 'aoi', 'roads'),
                     values = c('flickr' = "#FDE725FF",
                                'aoi' = "#481567FF",
                                'roads' = "gray25")) +
  annotation_scale(location = "bl", style = "ticks") +
  annotation_north_arrow(location = "br",   
                         height = unit(1, "cm"),
                         width = unit(1, "cm")) +
  labs(tag = "A") +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid.major = element_line(color = "transparent"),
        plot.tag.position = c(0.05,.9))

mf_map

#ggsave("Activities/figs/test.png", width = 6, height = 5, units = "in")

### # looking for a grayscale topo layer / a simpler base map
data(ext)
basemap_magick(aoi_mf, map_service = "osm", map_type = "hike")

# best so far
basemap_mf_hill <- basemap_magick(aoi_mf, map_service = "esri", map_type = "world_hillshade")
# also good
basemap_magick(aoi_mf, map_service = "esri", map_type = "world_hillshade_dark")

# this one could work
basemap_magick(aoi_mf, map_service = "esri", map_type = "world_shaded_relief")
# also fine (no lakes)
basemap_magick(aoi_mf, map_service = "esri", map_type = "world_terrain_base")

# let's transform my layers to match the new basemap
flickr_mf_38 <- st_transform(flickr_mf, crs = 3857)
aoi_mf_38 <- st_transform(aoi_mf, crs = 3857)
mf_road_38 <- st_transform(mf_road, crs = 3857)

mf_map <- basemap_ggplot(aoi_mf, map_service = "esri", map_type = "world_hillshade", map_res = .8) +
  #geom_sf(data = mf_road_38, col = "gray25", inherit.aes = FALSE) +
  geom_sf(data = flickr_mf_38, aes(col = 'a'), alpha = .3, size = .6, inherit.aes = FALSE) +
  geom_sf(data = aoi_mf_38, col = "black", fill = NA)+#, inherit.aes = FALSE) +
  scale_color_viridis_d() +
  annotation_scale(location = "bl", style = "ticks") +
  annotation_north_arrow(location = "br",   
                         height = unit(1, "cm"),
                         width = unit(1, "cm")) +
  labs(tag = "A") +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid.major = element_line(color = "transparent"),
        plot.tag.position = c(0.05,.9))
mf_map

## Mtn loop
st_bbox(aoi_mtn)

basemap_mtn <- get_stamenmap(bbox = c(left = -122.02, right = -121.15, bottom = 47.94, top = 48.32), maptype = "terrain-background", zoom = 10)

mtn_map <- ggmap(basemap_mtn) +
  geom_sf(data = roads_primary, aes(col = "roads"), inherit.aes = FALSE) +
  geom_sf(data = flickr_mtn, aes(col = 'flickr'), alpha = .3, size = .6, inherit.aes = FALSE) +
  geom_sf(data = aoi_mtn, aes(col = 'aoi'), size = 1, fill = NA, inherit.aes = FALSE) +
  scale_color_manual(name = NULL,
                     breaks = c('flickr', 'aoi', 'roads'),
                     values = c('flickr' = "#FDE725FF",
                                'aoi' = "#481567FF",
                                'roads' = "gray25"),
                     labels = c("Flickr Posts", "Study Region", "Primary Roads"),
                     guide = guide_legend(override.aes = list(linetype = c("blank", "blank", "solid"),
                                                              shape = c(16, 0, NA),
                                                              size = c(3, 8, .5),
                                                              alpha = c(1,1,1)))) +
  labs(tag = "B") +
  #theme(legend.key = element_blank()) +
  annotation_scale(location = "bl", style = "ticks") +
  annotation_north_arrow(location = "br",   
                         height = unit(1, "cm"),
                         width = unit(1, "cm")) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        panel.grid.major = element_line(color = "transparent"),
        plot.tag.position = c(0.05,.95))

mtn_map  

# ok, draw together
ggdraw() +
  coord_fixed(xlim = c(0, .85)) +
  draw_plot(mf_map + theme(legend.position = "none"), x = -.15, y = .123, width = .6) +
  draw_plot(mtn_map + theme(legend.position = "bottom"), x = .45, y = 0, width = .75) +
  #draw_label("A", x = -.04, y = .77) +
  #draw_label("B", x = .5, y = .77) +
  draw_plot(wa_ref, x = -.1, y = -.2, width = .35)

# write it out
ggsave(paste0("Activities/figs/SiteMap/site_map_panels_", dddd, ".png"), width = 10, units = "in")
knitr::plot_crop(paste0("Activities/figs/SiteMap/site_map_panels_", dddd, ".png"))


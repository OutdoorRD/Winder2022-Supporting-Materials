## 
## Mapping Activity Diversity
## Forked from MappingPhotoActivities on 5/13/21
##

### Requires: AOI and output from `creating_gridded_apuds.R`

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(lubridate)
library(cowplot)
library(ggspatial)

setwd("~/Documents/Recquity/MiddleFork/")
dddd <- gsub(Sys.Date(), pattern="-", replacement="")

# read in the gridded apuds
mf <- read_sf("GIS/MiddleFork_activities_by_grid_20210715.geojson")
mtnloop <- read_sf("GIS/MtnLoop_activities_by_grid_20210715.geojson")

# plus aois
mf_aoi <- read_sf("GIS/middlefork_AOI_v2.shp")
mt_aoi <- read_sf("GIS/MtLoopAOI_coarse.shp")

# first I want to join my two aois
mtn2 <- mtnloop %>%
  mutate(cid = paste0(cid, "_mtn"))
mf2 <- mf %>%
  mutate(cid = paste0(cid, "_mf"))
div_grid <- rbind(mtn2, mf2) %>% rename(pid = cid)

ggplot(div_grid) + geom_sf()

num_act_plot <- ggplot(div_grid) + # %>% filter(activities != 0)) +
  #geom_sf(data = aoi_hex) +
  geom_sf(aes(fill = activities), size = .1) +
  scale_fill_viridis_c() +
  labs(title =  "Number of Distinct Activities")
num_act_plot 

#ggsave(paste0("Activities/figs/act_div_", dddd, ".png"), width = 4, height = 5, units = "in")

#### Ok. actually I'd prefer to have two panels
div_grid_reg <- div_grid %>% 
  mutate(region = str_extract(pid, "(?<=_)[:alpha:]{2,3}"))

ggplot(div_grid_reg) + # %>% filter(activities != 0)) +
  #geom_sf(data = aoi_hex) +
  geom_sf(aes(fill = activities), size = .1) +
  scale_fill_viridis_c() +
  labs(title =  "Number of Distinct Activities") +
  facet_grid(~region)

# meh. better to make two separate plots and then plot them together, I think

mf_map <- ggplot(mf %>% filter(!is.na(activities))) +
  geom_sf(aes(fill = activities), size = .1) +
  geom_sf(data = mf_aoi, fill = NA, col = "white") +
  scale_fill_viridis_c(name = "Number of Activities", 
                       breaks = c(0, 3, 6, 9)#,
                       #guide = guide_colorbar(title.position = "top")
                       ) +
  annotation_scale(location = "br", style = "ticks") +
  annotation_north_arrow(location = "bl",   
                         height = unit(.75, "cm"),
                         width = unit(.75, "cm")) +
  labs(tag = "A") +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid.major = element_line(color = "transparent"),
        plot.tag.position = c(0.05,.9),
        legend.position = "bottom")
mf_map

# mtn loop
mtn_map <- ggplot(mtnloop %>% filter(!is.na(activities))) +
  geom_sf(aes(fill = activities), size = .1) +
  geom_sf(data = mt_aoi, fill = NA, col = "white") +
  scale_fill_viridis_c(#name = "Number of Activities", 
                       #breaks = c(0, 3, 6, 9),
                       guide = NULL) +
  annotation_scale(location = "bl", style = "ticks") +
  annotation_north_arrow(location = "br",   
                         height = unit(.75, "cm"),
                         width = unit(.75, "cm")) +
  labs(tag = "B") +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid.major = element_line(color = "transparent"),
        plot.tag.position = c(0.05,.9))

mtnloop %>% arrange(desc(activities))

# together
ggdraw() +
  coord_fixed(xlim = c(0, 2)) +
  draw_plot(mf_map, x = 0, y = 0, width = 1) +
  draw_plot(mtn_map, x = 1, y = .031, width = 1) 

# write it out
ggsave(paste0("Activities/figs/act_div_maps_", dddd, ".png"), width = 10, units = "in")
knitr::plot_crop(paste0("Activities/figs/act_div_maps_", dddd, ".png"))

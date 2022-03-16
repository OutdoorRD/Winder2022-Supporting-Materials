#### Subsetting the Middle Fork Survey data to look just at activities
### For Heera and Bumsuk
### Updated 12/10/19

## Note 2/12/20: Changed directory structure, so file paths likely broken

## Updated 6/10/20 to create model categories early (eg biking is mtn biking OR
##   road biking, not both combined!)

## UPdated 5/10/21 to add fishing, and 5/13/21 to add running

setwd("~/Documents/Recquity/MiddleFork/")
library(tidyverse)
library(lubridate)
library(sf)

dddd <- gsub(Sys.Date(), pattern="-", replacement="")

Combined_Survey_Data <- read_csv("Middle Fork Rec Combined Data 6_12_19.csv", na = c("NA", "N/A", ""))
Combined_Survey_Data$Date <- mdy(Combined_Survey_Data$Date)
Combined_Survey_Data <- rename(Combined_Survey_Data, 
                               ReasonVisit_safety = ReasonVisit_safty,
                               Activity_roadbiking = Activity_raodbiking,
                               Activity_fishing = Acitvity_fishing)

# removing extra columns
# and adding columns for my combined model categories and removing the components
Survey_activities <- Combined_Survey_Data %>% 
  select(Unique_ID, Site_Name, Date, destination_strd, 
         starts_with("Activity"), Primary_activity) %>%
  mutate(Activity_biking = if_else(Activity_roadbiking == 1 | Activity_trailbiking == 1,
                                   1, 0),
         Activity_camping = if_else(Activity_campground == 1 | 
                                      Activity_camproad == 1 |
                                      Activity_dispersed == 1,
                                    1, 0),
         Activity_other = if_else(Activity_collecting == 1 |
                                    Activity_foraging == 1 |
                                    Activity_hunting == 1 |
                                    Activity_picnic ==1,
                                  1, 0)) %>%
  select(-Activity_roadbiking, -Activity_trailbiking, -Activity_campground,
         -Activity_camproad, -Activity_collecting, -Activity_foraging,
         -Activity_hunting, -Activity_picnic, -Activity_dispersed)

# write it out
#write_csv(Survey_activities, paste0("Activities/SurveyActivities/survey_activities_data_", dddd, ".csv"))



#############
# summarise by destination
### This code also writes out survey_activities_areas_v2_dddd.geojson which is used in ComparingSurveytoModelUserDays

# how to separate out the multiple destinations?
Combined_destinations <-  Survey_activities %>%
  mutate(destination = str_split(destination_strd, ", ")) %>%
  unnest(cols = c(destination))

destination_table <- Combined_destinations %>%
  group_by(destination) %>%
  summarise(Respondents = n()) %>%
  arrange(desc(Respondents)) 

# NOTE: I've modified the original all_destinations.csv, so don't overwrite it! Updated here to write to "2.csv"
#write_csv(destination_table, "Activities/SurveyActivities/all_destinations2.csv")

## Bring in the csv which matches my polygons to destinations
dest_match <- read_csv("Activities/SurveyActivities/all_destinations_v2.csv")
# note that v3 appears to be identical to v2

# join on to combined destinations
Area_sums <- Combined_destinations %>%
  left_join(dest_match %>% select(destination, AreaName), by = "destination") %>%
  mutate(Respondents = 1) %>%
  group_by(AreaName) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  select(-Unique_ID)

# bring in the shapefile of desintations
areas_shp <- read_sf("GIS/survey_destinations_v2.shp")
ggplot(areas_shp) + geom_sf()

# bind em on
areas_sum_sp <- areas_shp %>% left_join(Area_sums)


## Group by siteids, which combine some of the old polygons
sites_sums <- areas_sum_sp %>%
  select(-id) %>%
  group_by(siteid) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  filter(siteid != "NA")

# change names to not include "activity"
areas_activities <- 
  sites_sums %>%
  #select(-id) %>%
  rename_at(vars(starts_with("Activity")), list(~str_extract(., "(?<=_)[:alpha:]+")))

ggplot(areas_activities) + geom_sf()

# write it out
#st_write(areas_activities, "GIS/survey_activities_areas_v2_20210510.shp")
st_write(areas_activities, paste0("GIS/survey_activities_areas_v2_", dddd, ".geojson"))
# Note: this has the 10 polygons which we want to use moving forward

# Summary plots and such that were previously here have been moved to the new comparison script



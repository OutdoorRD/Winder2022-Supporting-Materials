####
#### Comparing Survey to Model - Activity User-Days calculated from Top1
####
#### Forked from ComparingSurveytoModel.R on 6/25/20


### Requires: outputs from subsettingSurveyDatatoActivities.R and addingUserData.R

library(tidyverse)
library(lubridate)
library(sf)

dddd <- gsub(Sys.Date(), pattern="-", replacement="")

setwd("~/Documents/Recquity/MiddleFork/")

# read in activities data for all survey respondents
all_surveys <- read_csv("Activities/SurveyActivities/survey_activities_data_20210513.csv")

# read in activities by area (not actually to use the raw data, but to get the shapeoutlines)
areas_activities <- read_sf("GIS/survey_activities_areas_v2_20210513.geojson")

###################################################################
#### Comparing to Modeled Activities (Top1)
###################################################################

## reading in points
pred_activities <- read_sf("Activities/CNNModel/FinalPredictions20210420/FlickrMiddlefork_predicted_users.geojson")
pred_activities$date_taken <- date(pred_activities$date_taken)

## remove rows with NAs for filename
pred_activities <- pred_activities %>% filter(!is.na(Filename))

# intersecting with my siteids
sites <- areas_activities %>%
  select(siteid, geometry)
preds_sites <- st_intersection(pred_activities, sites)


###### Calculating User-days ############
# aggregating by site (removing noactivity)
# Calculating both the total number of posts, and the activity user days 
## This is the unique number of users posting a photo classfied as an activity in a polygon on a day, summed over the days
#posts_p_user_p_day_p_top1_p_site <- preds_sites %>%
#  st_set_geometry(NULL) %>%
#  group_by(siteid, Top1, date_taken, userid) %>%
#  summarise(posts = n()) 
#  
## now calculate the total number of posts per top1 and site, and also the Activity Photo userdays per top1 and site
#APUD_sites <- posts_p_user_p_day_p_top1_p_site %>%
#  group_by(siteid, Top1) %>%
#  summarise(totalposts = sum(posts),
#         APUD = n()) %>%
#  filter(Top1 != "noactivity") 

## And, calculate APUD for the entire MF from our sites
# using photos that fell within our 10 sites only since other photos came from other access points
# Plus, add a row for trailrunning which had 0 photos
APUD_mf <- preds_sites %>%
  st_set_geometry(NULL) %>%
  group_by(Top1, date_taken, userid) %>%
  summarise(posts = n()) %>%
  group_by(Top1) %>%
  summarise(APUD = n()) %>%
  bind_rows(tibble(Top1 = "trailrunning", APUD = 0))

APUD_mf

## summarise this a bit
## How many APUD in total?
sum(APUD_mf$APUD)
#2321

## How many which are not noactivity or pplnoactivity?
APUD_mf %>% filter(!Top1 %in% c("noactivity", "pplnoactivity")) %>%
  summarise(APUD_tot = sum(APUD))
# 1076
2321-1076
#1245 no activity

## Calculate PUD by site (ignoring activities)
PUD_sites <- preds_sites %>%
  st_set_geometry(NULL) %>%
  group_by(siteid, date_taken, userid) %>%
  tally() %>%
  group_by(siteid) %>%
  summarise(PUD = n())


######## Overall MF activity user day comparisons ######
#### First, let's look for the correlation between the number of people saying
####   they'd do each activity and the number of photos classified as being that 
####   activity in the entire MF. (So, don't worry about the destinations for this)
#### This will take advantage of all the survey results, not just those that stated a destination

# get some activity column sums
activity_totals_s <- all_surveys %>%
  summarise_at(vars(starts_with("Activity")), sum, na.rm = TRUE) %>%
  rename_at(vars(starts_with("Activity")), list(~str_extract(., "(?<=_)[:alpha:]+")))
activity_totals_s

# reshaping and renaming with activities taht match the model
activity_totals_mod_s <- activity_totals_s %>%
  gather(key = "Activity", value = "Participants") %>%
  mutate(ModelActivity = case_when(Activity == "climbing" ~ "rock climbing",
                                   Activity == "horse" ~ "horseriding",
                                   Activity == "wildlife" ~ "birdwatching",
                                   Activity == "other" ~ "otheractivities",
                                   Activity == "biking" ~ "mtn_biking",
                                   Activity == "running" ~ "trailrunning",
                                   TRUE ~ Activity)) %>%
  group_by(ModelActivity) %>%
  summarise(survey_sum = sum(Participants))
activity_totals_mod_s

# bind on modeled AUD & make it tall
# using photos that fell within our 10 sites only since other photos came from other access points
mf_activities <- activity_totals_mod_s %>%
  full_join(APUD_mf, by = c("ModelActivity" = "Top1")) %>%
  filter(!is.na(ModelActivity), !is.na(survey_sum)) #%>%
  #gather(key = "source", value = "UD", -ModelActivity)

mf_activities

# plotting survey_sum against Activity PUD for the MF
ggplot(mf_activities %>% filter(!is.na(survey_sum))) +
  geom_point(aes(x = survey_sum, y = APUD, col = ModelActivity))

# log scale and correlation?
mf_activities$logSurvey = log1p(mf_activities$survey_sum)
mf_activities$logAPUD = log1p(mf_activities$APUD)

cor.test(mf_activities$logSurvey, mf_activities$logAPUD) #+.77
# damn. fishing really messes with this. down to .59
# oof. pulling trail running out of "other" really hurts this. down to .32 CI: (-.31, .75)

## how about without fishing or trail running?
mf_act_sub <- mf_activities %>%
  filter(!ModelActivity %in% c("fishing", "trailrunning"))
cor(mf_act_sub$logSurvey, mf_act_sub$logAPUD) #+.75

# and without horseback riding either? (b/c no manual eval)
mf_act_sub2 <- mf_activities %>%
  filter(!ModelActivity %in% c("fishing", "trailrunning", "horseriding"))
cor(mf_act_sub2$logSurvey, mf_act_sub2$logAPUD) #+.73

cor.test(mf_act_sub2$logSurvey, mf_act_sub2$logAPUD) #+.73 CI: (0.12, .94)



## 
ggplot(mf_activities) +
  geom_point(aes(y = survey_sum, x = APUD, col = ModelActivity)) +
  geom_label(aes(x = 300, y = 20), label = "r = 0.32", label.size = 0) +
  scale_x_log10(name = "Activity PUD (Top1, log scale)") +
  scale_y_log10(name = "Survey respondents participating in activity (log scale)") +
  #geom_smooth(aes(x = survey_sum, y = APUD), method = "lm", se = FALSE) +
  labs(title = "Number of People Participating in each Activity Across the Entire MF \nAccording to Activity Photo User-days vs Survey") +
  theme_classic()

## Cleaned up a bit for publication
ggplot(mf_activities) +
  geom_point(aes(y = log1p(survey_sum), x = log1p(APUD), col = ModelActivity)) +
  geom_label(aes(x = 6, y = 2), label = "r = 0.32", label.size = 0) +
  scale_x_continuous(name = "Activity Photo User-days (log)") +
  scale_y_continuous(name = "Survey respondents participating in activity (log)") +
  coord_cartesian(xlim = c(0, NA), ylim = c(0, NA)) +
  #geom_smooth(aes(x = survey_sum, y = APUD), method = "lm", se = FALSE) +
  #labs(title = "Number of People Participating in each Activity Across the Entire MF \nAccording to Activity Photo User-days vs Survey") +
  theme_classic()

# write it out
#ggsave(paste0("Activities/ActivityComparisons/top1_figs/mf_APUD_v_survey_scatter_LOG_", dddd, ".png"),
 #   width = 5, height = 4, units = "in")

# Let's try a slightly different one where I have the category on the plot
## Cleaned up a bit for publication
ggplot(mf_activities) +
  geom_point(aes(y = log1p(survey_sum), x = log1p(APUD)), size = 1) +
  geom_text(aes(y = log1p(survey_sum), x = log1p(APUD), label = ModelActivity), hjust = -.14, size = 3.5) +
  geom_label(aes(x = 6, y = .5), label = "r = 0.32", label.size = 0) +
  scale_x_continuous(name = "Activity Photo User-days (log)") +
  scale_y_continuous(name = "Survey respondents participating in activity (log)") +
  coord_cartesian(xlim = c(0, 7), ylim = c(0, NA)) +
  #geom_smooth(aes(x = survey_sum, y = APUD), method = "lm", se = FALSE) +
  #labs(title = "Number of People Participating in each Activity Across the Entire MF \nAccording to Activity Photo User-days vs Survey") +
  theme_classic()

#ggsave(paste0("Activities/ActivityComparisons/top1_figs/mf_APUD_v_survey_scatter_LOG_text_", dddd, ".png"),
 #  width = 6, height = 5, units = "in")


## Can I color these by the number of training photos from within the study region that were used?
# Yes, let's do it. Pull out manually from appendix table .2 that bumsuk put in the paper,
# and then bin them
mf_activities

mf_activities_training <- mf_activities %>%
  mutate(mf_training_photos = c(144, 56, 62, 82, 3, 2092, 41, 37, 225, 58, 168, 1),
         precision = c(.17, .91, 1, .21, 0, .63, 0, .52, .55, .49, .18, NA),
         recall = c(.75, .83, 1, .5, NA, .83, NA, 1, .4, .88, 1, NA),
         #f1 = psych::harmonic.mean(c(precision, recall)),
         training_photos_bin = cut(mf_training_photos, 
                                   breaks = c(0, 5, 50, 100, 1000, 2100),
                                   labels = c("<5", "5-50", "50-100", "100-1000", ">1000")),
         training_photos_bin_4 = cut(mf_training_photos, 
                                     breaks = c(0, 5, 100, 1000, 2100),
                                     labels = c("<5", "5-100", "100-1000", ">1000"))) %>%
  mutate(ActivityClean = str_to_title(ModelActivity),
         ActivityClean = case_when(ActivityClean == "Birdwatching" ~ "Bird watching",
                                   ActivityClean == "Horseriding" ~ "Horseback riding",
                                   ActivityClean == "Mtn_biking" ~ "Mountain biking",
                                   ActivityClean == "Otheractivities" ~ "Other activities",
                                   ActivityClean == "Rock Climbing" ~ "Rock climbing",
                                   ActivityClean == "Trailrunning" ~ "Trail running",
                                   TRUE ~ ActivityClean)) %>%
  rowwise() %>%
  mutate(f1 = psych::harmonic.mean(c(precision, recall), na.rm = FALSE))
mf_activities_training
# make cleaner names for plot

mf_activities_training 

ggplot(mf_activities_training) +
  geom_point(aes(y = (survey_sum), x = (APUD), col = training_photos_bin)) +
  geom_text(aes(y = (survey_sum), x = (APUD), label = ActivityClean, col = training_photos_bin), 
            hjust = -.14, size = 3.5, show.legend = FALSE) +
  #geom_label(aes(x = 600, y = .5), label = "r = 0.32", label.size = 0) +
  scale_x_continuous(name = "Activity Photo User-days (log)", trans = "log1p",
                     breaks = c(0, 10, 100, 1000)) +
  scale_y_continuous(name = "Survey respondents participating in activity (log)", trans = "log1p",
                     breaks = c(0, 10, 100, 500)) +
  #scale_color_viridis_d(name = "Training Photos \nfrom Study Area", end = .85, direction = -1, alpha = 1) +
  #scale_color_brewer(name = "Training Photos \nfrom Study Area", palette = "Set1") +
  #scale_color_manual(values = c("#003f5c", "#58508d","#bc5090","#ff6361","#ffa600")) +
  scale_color_manual(name = "Training Photos \nfrom Study Area",
                     values = c('#ff7f00', 
                                #'#e41a1c', # original red
                                '#CF0303', # slightly duller red
                                '#984ea3','#377eb8','#4daf4a')) + # Brewer Set1 reordered
  coord_cartesian(xlim = c(0, 950), ylim = c(0, NA)) +
  #geom_smooth(aes(x = survey_sum, y = APUD), method = "lm", se = FALSE) +
  #labs(title = "Number of People Participating in each Activity Across the Entire MF \nAccording to Activity Photo User-days vs Survey") +
  theme_classic()


#ggsave(paste0("Activities/ActivityComparisons/top1_figs/mf_APUD_v_survey_scatter_LOG_text_training_", dddd, ".png"),
#       width = 7, height = 5, units = "in")



## playing with coloring by classifier certainty (via evaluation metrics)
ggplot(mf_activities_training) +
  geom_smooth(data = mf_activities_training %>% filter(!is.na(f1)), 
              aes(x = survey_sum, y = APUD), method = "lm", se = TRUE, #fullrange = TRUE,
              col = "gray", linetype = 2, size = .5, fill = "gray90") +
  geom_point(aes(y = (survey_sum), x = (APUD), col = f1)) +
  ggrepel::geom_text_repel(aes(y = (survey_sum), x = (APUD), label = ActivityClean, col = f1), 
            hjust = -.14, size = 3.5, show.legend = FALSE) +
  #geom_label(aes(x = 600, y = .5), label = "r = 0.32", label.size = 0) +
  scale_x_continuous(name = "Activity Photo User-days (log)", trans = "log1p",
                     breaks = c(0, 10, 100, 1000)) +
  scale_y_continuous(name = "Survey Respondent User-days (log)", trans = "log1p",
                     breaks = c(0, 10, 100, 500)) +
  #scale_color_viridis_d(name = "Training Photos \nfrom Study Area", end = .85, direction = -1, alpha = 1) +
  #scale_color_brewer(name = "Training Photos \nfrom Study Area", palette = "Set1") +
  #scale_color_manual(values = c("#003f5c", "#58508d","#bc5090","#ff6361","#ffa600")) +
  scale_color_viridis_c(direction = -1, end = .8, name = "F1 score") +
  coord_cartesian(xlim = c(0, 850), ylim = c(0, NA)) +
  #labs(title = "Number of People Participating in each Activity Across the Entire MF \nAccording to Activity Photo User-days vs Survey") +
  theme_classic()

## I'd like to run a little regression and add a line to the plot (maybe later)
#mf_mod <- lm(logAPUD ~ logSurvey, data = mf_activities_training %>% filter(!is.na(f1)))
#summary(mf_mod)


ggsave(paste0("Activities/ActivityComparisons/top1_figs/mf_APUD_v_survey_scatter_LOG_text_f1_", dddd, ".png"),
       width = 6, height = 4, units = "in")

# plot of # training photos vs survey respodnents by activity
ggplot(mf_activities_training) +
  geom_point(aes(x = mf_training_photos, y = survey_sum, col = ActivityClean)) +
  #geom_point(aes(x = APUD, y = survey_sum, col = ActivityClean), pch = 2) +
  scale_x_continuous(name = "Number of training photos from the Middle Fork (log)", trans = "log1p",
                     breaks = c(0, 10, 100, 1000)) +
  scale_y_continuous(name = "Survey respondents participating in activity (log)", trans = "log1p",
                     breaks = c(0, 10, 100, 500)) 

##########################################

######## Comparing total UD by site across the MF ######
## Ignoring activities for this plot

areas_long
# totrespondents is the number of survey responses from each site

# combine with PUD per site
PUD_sites

mf_sites_ud <- areas_long %>%
  st_set_geometry(NULL) %>%
  select(siteid, TotRespondents) %>%
  distinct() %>%
  left_join(PUD_sites, by = "siteid")

# TODO: again, probably want to switch the axes on these
ggplot(mf_sites_ud) +
  geom_label(aes(x = TotRespondents, y = PUD, label = siteid)) +
  labs(title = "Total PUD and Respondents by Polygon") +
  #scale_y_log10() +
  #scale_x_log10() +
  theme_classic()


# write it out
#ggsave(paste0("Activities/ActivityComparisons/total_pud_vs_survey_by_site_", dddd, ".png"),
 #      width = 9, height = 6, units = "in")




####################################
#### OLD - site-based comparisons and plots #######



##### Survey Activities Summaries ########

ggplot(areas_activities) +
  geom_sf(aes(fill = Respondents))

resp_by_area <- areas_activities %>% 
  st_set_geometry(NULL) %>%
  select(siteid, Respondents) %>%
  arrange(desc(Respondents))


# long form?
areas_long <- areas_activities %>% 
  rename(TotRespondents = Respondents) %>%
  gather(key = "Activity", value = "Participants", -siteid, -TotRespondents, -geometry)

ggplot(areas_long) +
  geom_sf(aes(fill = Participants)) +
  facet_wrap(~Activity)

#### Rename activities to match survey
## Modified 6/10/20 after I updated subsettingSurveyDatatoActivities to do more
##  correct calculations of the grouped categories. Now we just need to do some 
##  renaming of columns

# reading in the crosswalk
#joinkey <- read_csv("Activities/SurveyActivities/survey_activities_to_model_activities.csv")

# reclassifying and aggregating by activities as used in the model
areas_long_matched_acts <- areas_long %>% 
  #left_join(joinkey, by = c("Activity" = "SurveyActivity")) %>%
  mutate(ModelActivity = case_when(Activity == "climbing" ~ "rock climbing",
                                   Activity == "horse" ~ "horseriding",
                                   Activity == "wildlife" ~ "birdwatching",
                                   Activity == "other" ~ "otheractivities",
                                   Activity == "biking" ~ "mtn_biking",
                                   TRUE ~ Activity)) %>%
  select(-Activity) %>%
  group_by(siteid, ModelActivity) %>%
  mutate(Participants = sum(Participants)) #%>%
#distinct(siteid, TotRespondents, Participants, ModelActivity) # this line isn't working 4/16/21. Trying without it

areas_long_matched_acts

ggplot(areas_long_matched_acts) +
  geom_sf(aes(fill = Participants)) +
  facet_wrap(~ModelActivity)

# write it out
#ggsave("Activities/SurveyActivities/activities_by_area_121019.png", width = 11, units = "in")

####### Proportion of respondents doing each activity #######
### NOT doing these as of 6/25
### Focusing on comparisons of log userdays

#activity_props <- areas_long_matched_acts %>%
# mutate(Prop_participating = Participants/TotRespondents)

#ggplot(activity_props) +
# geom_sf(aes(fill = Prop_participating)) +
#facet_wrap(~ModelActivity)

# cool. I may want to group some of those further, but for now let's run some early comparisons
# with the model results.

# are there differences in what survey respondents say they're doing in diff sites?
#ggplot(activity_props) +
# geom_col(aes(x = reorder(ModelActivity, Participants), y = Participants)) +
#coord_flip() +
#facet_wrap(~siteid) +
#labs(title = "Activities from Survey by Stated Destination")

#ggplot(activity_props) +
# geom_col(aes(x = reorder(siteid, Participants), y = Participants, fill = ModelActivity)) +
#facet_wrap(~ModelActivity) +
#coord_flip()

########### Looking at APUD vs Surveys by Site ###########
APUD_sites
areas_long_matched_acts

APUD_surv_sites <- areas_long_matched_acts %>%
  select(-TotRespondents) %>%
  left_join(APUD_sites %>% select(-totalposts), 
            by = c("ModelActivity" = "Top1", "siteid"))

ggplot(APUD_surv_sites) +
  geom_point(aes(x = Participants, y = APUD, col = ModelActivity)) +
  scale_y_log10() +
  scale_x_log10() +
  facet_wrap(~ModelActivity) +
  labs(title = "Log Activity Photo UD vs Log Survey Respondents across ten sites") +
  theme_bw()

# write it out
#ggsave(paste0("Activities/ActivityComparisons/top1_figs/sites_APUD_v_survey_scatter_log_", dddd, ".png"),
 #      width = 9, height = 6, units = "in")












#############################################################

ggplot(preds_sites) +
 geom_sf(aes(fill = Top1))




top1_props_by_site <- top1_by_site %>%
  mutate(prop_posts = posts/totalpostsActivity)

# comparing
# bringing the two datasets together
activities_combined <- activity_props %>% 
  left_join(top1_props_by_site, by = c("siteid", "ModelActivity" = "Top1"))

# is the number of posts correlated with the number of people saying they're going there?
ggplot(activities_combined) +
  geom_label(aes(x = TotRespondents, y = totalposts, label = siteid)) +
  geom_point(aes(x = TotRespondents, y = totalpostsActivity, col = siteid)) +
  labs(title = "Posts and Respondents by Polygon")

# More people posting from MF_007 (north and east of the Upper snoqualmie) than
# saying they'll go there. But this is a very small sample - only 8 respondents.
# Could consider combining this with MF_009 (Taylor River, Snoqualmie Lake, etc)

ggplot(activities_combined) +
  geom_point(aes(x = Participants, y = posts, col = siteid)) +
  facet_wrap(~ModelActivity) +
  labs(title = "Number of Posts Classified vs Survey Respondents by Activity by Polygon")

ggplot(activities_combined) +
  geom_point(aes(x = Participants, y = posts, col = siteid)) +
  scale_y_continuous(trans = "log1p") +
  scale_x_continuous(trans = "log1p") +
  facet_wrap(~ModelActivity) +
  labs(title = "Log Number of Posts Classified vs Survey Respondents by Activity by Polygon")

# all on one
ggplot(activities_combined) +
  geom_point(aes(x = Participants, y = posts, col = ModelActivity)) +
  scale_y_continuous(trans = "log1p") +
  scale_x_continuous(trans = "log1p") +
  #facet_wrap(~ModelActivity) +
  labs(title = "Log Number of Posts Classified vs Survey Respondents by Activity by Polygon")

ggplot(activities_combined) +
  geom_point(aes(x = Prop_participating, y = prop_posts, col = siteid)) +
  facet_wrap(~ModelActivity) +
  labs(title = "Proportion of Posts Classified vs Survey Respondents by Activity by Polygon")

# all on one
ggplot(activities_combined) +
  geom_point(aes(x = Prop_participating, y = prop_posts, col = ModelActivity)) +
  #facet_wrap(~ModelActivity) +
  labs(title = "Proportion of Posts Classified vs Survey Respondents by Activity by Polygon")


# bar graph?
# reshape to longform - looking at proportions
activities_combined_long_props <- activities_combined %>%
  gather(key = "Source", value = "Proportion", Prop_participating, prop_posts) %>%
  mutate(Source = if_else(Source == "Prop_participating", "Survey", "ModelTop1"))


ggplot(activities_combined_long_props) +
  geom_col(aes(x = reorder(ModelActivity, Participants), y = Proportion, fill = Source),
           position = "dodge") +
  coord_flip() +
  facet_wrap(~siteid) +
  labs(title = "Proportion (of respondent, or of posts) showing an activity by site")
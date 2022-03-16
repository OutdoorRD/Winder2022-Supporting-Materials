####
### Preference model for activities
### 10/14/20
### 

library(sf)
library(tidyverse)
library(corrgram)
library(betareg)
library(MASS)
library(coefplot)
library(lme4)

modplot <- function(x){
  par(mfrow = c(2,2))
  plot(x, ask = F)
  par(mfrow = c(1,1))
}

setwd("~/Documents/Recquity/MiddleFork/")
dddd <- gsub(Sys.Date(), pattern="-", replacement="")

predictors <- st_read("GIS/PredictorsCreated/Predictors_20210715.geojson")
predictors
predictors_tib <- st_drop_geometry(predictors) %>% as_tibble()

# add in a "region" column
predictors_tib <- predictors_tib %>%
  mutate(region = str_extract(pid, "(?<=_)\\w+"))

corrgram(predictors_tib, diag.panel = panel.density, lower.panel = panel.cor, upper.panel = panel.pts)


# rescale to 0-1
scale_func <- function(x) (x - min(x))/(max(x) - min(x))
pred_scaled <- predictors_tib %>% 
  mutate(elevation = scale_func(elevation),
         road_min_dist = scale_func(road_min_dist),
         APUD_scaled = scale_func(APUD))
summary(pred_scaled)


### cool 

predictors_tib

### What about a combined (pooled) model?
# what about a neg bin model of activities?
mod_act <- glm.nb(activities ~ campgrounds + lakes + picnic + rivers + trails + road_min_dist + elevation + prop_wilderness,
                  data = pred_scaled)
summary(mod_act); modplot(mod_act)
coefplot(mod_act)
# (null dev - resid dev) / null dev
(mod_act$null.deviance - mod_act$deviance) / mod_act$null.deviance # .22 w/ elev and wilderness

### Make a pretty coefplot ###
# Put model estimates into temporary data.frames:
modFrame <- data.frame(Variable = rownames(summary(mod_act)$coef),
                           Coefficient = summary(mod_act)$coef[, 1],
                           SE = summary(mod_act)$coef[, 2],
                           modelName = "Pooled Model")

# Combine these data.frames
#allModelFrame <- data.frame(rbind(mtn_modFrame, mf_modFrame))  # etc.

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# add the 95% range to make a variable for stat sig or not
allModelFrameSig <- modFrame %>%
  mutate(min95 = Coefficient - SE*interval2,
         max95 = Coefficient + SE*interval2,
         sig = if_else(sign(min95) == sign(max95), 1, 0)) # setting this up to be used by alpha

library(scales)
scales::show_col(viridis_pal()(5))

# Plot
ggplot(allModelFrameSig, aes(col = modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                     ymax = Coefficient + SE*interval1,
                     alpha = (sig)),
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = Variable, y = Coefficient, ymin = min95,
                      ymax = max95, alpha = (sig)),
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") +
  scale_alpha(range = c(.25, 1), guide = FALSE) +
  scale_color_manual(guide = FALSE, values = "#440154FF") + # viridis dark purple (#440154FF)
  #scale_color_brewer(name = NULL, palette = "Set1", guide = FALSE) + #guide = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels = c("(Intercept)", "Campgrounds", "Elevation", "Lakes", "Picnic Areas", 
                              "Wilderness", "Rivers", "Distance to Road", "Trails")) +
  coord_flip() + 
  theme_classic() 

# write it out
#ggsave(paste0("Activities/figs/coef_plot_", dddd, ".png"), width = 4, height = 4, units = "in")

##############################
#### Separate regressions ####
# let's create separate regressions for the two regions
mtnloop <- pred_scaled %>% filter(region == "mtn")
midfork <- pred_scaled %>% filter(region == "mf") %>% dplyr::select(-picnic)

corrgram(mtnloop)

# not using prop_wilderness or elevation because they're both ~.6 corrleation with distnace from roads
# using neg bin b/c poisson is overdispersed

mtn_mod <- glm.nb(activities ~ campgrounds + lakes + picnic + rivers + trails + road_min_dist + elevation + prop_wilderness,
                  data = mtnloop)
summary(mtn_mod); modplot(mtn_mod)
coefplot(mtn_mod)
car::vif(mtn_mod)

# no picnic areas here
mf_mod <- glm.nb(activities ~ campgrounds + lakes + rivers + trails + road_min_dist + elevation + prop_wilderness,
                  data = midfork)
summary(mf_mod); modplot(mf_mod)
coefplot(mf_mod)
car::vif(mf_mod)


# some summaries - n and psuedo r2
midfork %>% filter(!is.na(activities)) %>% tally() # n = 148
mtnloop %>% filter(!is.na(activities)) %>% tally() # n = 406

# pseudo r2 for these models? Using the proportional reduction in deviance as in my thesis...
# (null dev - resid dev) / null dev
summary(mf_mod)
(205.95 - 170.19) / 205.95 # .17 (w/o elevation or wilderness)
(mf_mod$null.deviance - mf_mod$deviance) / mf_mod$null.deviance # .23 w/ them

summary(mtn_mod)
(535.99 - 389.63) / 535.99 # .27 (w/o elev or wilderness)
(mtn_mod$null.deviance - mtn_mod$deviance) / mtn_mod$null.deviance # .33 w/ them

## make a nice plot showing these two models


# Put model estimates into temporary data.frames:
mtn_modFrame <- data.frame(Variable = rownames(summary(mtn_mod)$coef),
                          Coefficient = summary(mtn_mod)$coef[, 1],
                          SE = summary(mtn_mod)$coef[, 2],
                          modelName = "Mountain Loop")
mf_modFrame <- data.frame(Variable = rownames(summary(mf_mod)$coef),
                          Coefficient = summary(mf_mod)$coef[, 1],
                          SE = summary(mf_mod)$coef[, 2],
                          modelName = "Middle Fork")

# Combine these data.frames
allModelFrame <- data.frame(rbind(mtn_modFrame, mf_modFrame))  # etc.

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# add the 95% range to make a variable for stat sig or not
allModelFrameSig <- allModelFrame %>%
  mutate(min95 = Coefficient - SE*interval2,
         max95 = Coefficient + SE*interval2,
         sig = if_else(sign(min95) == sign(max95), 1, 0)) # setting this up to be used by alpha

# Plot
ggplot(allModelFrameSig, aes(colour = modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                                ymax = Coefficient + SE*interval1,
                                alpha = (sig)),
                            lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = Variable, y = Coefficient, ymin = min95,
                                 ymax = max95, alpha = (sig)),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE") +
  scale_alpha(range = c(.25, 1), guide = FALSE) +
  scale_color_brewer(name = NULL, palette = "Set1", guide = guide_legend(reverse = TRUE)) +
  #scale_x_discrete(labels = c("(Intercept)", "Campgrounds", "Lakes", "Picnic Areas", "Rivers", "Distance to Road", "Trails")) +
  coord_flip() + 
  theme_classic() 

# write it out
#ggsave(paste0("Activities/figs/coef_plot_", dddd, ".png"), width = 6, height = 4, units = "in")


# times region?
mod_act2 <- glm.nb(activities ~ (campgrounds + lakes + picnic + rivers + trails + road_min_dist + elevation + prop_wilderness)*region,
                  data = pred_scaled)
summary(mod_act2); modplot(mod_act2)
coefplot(mod_act2)
(832.52 - 580.37) / 832.52 #.30
(mod_act2$null.deviance - mod_act2$deviance) / mod_act2$null.deviance # .35 w/ elev and wilderness

####### Exploratory modeling code below ###########

# switching throughout to "campgrounds" from camping

# ok. let's try modeling log(APUD) (which i think is more or less equiivalent to the number of ppl visiting)
# and also ginisimpson

mod_apud <- lm(log1p(APUD) ~ campgrounds + lakes + picnic + rivers + trails + road_min_dist + elevation + prop_wilderness,
               data = pred_scaled)
summary(mod_apud); modplot(mod_apud)
coefplot(mod_apud)
# nice. not terrible

ggplot(predictors_tib) +
  geom_density(aes(x = log1p(APUD)))

ggplot(predictors_tib) +
  geom_histogram(aes(x = activities))

# what about a neg bin model of activities?
mod_act <- glm.nb(activities ~ campgrounds + lakes + picnic + rivers + trails + road_min_dist + elevation + prop_wilderness,
                  data = pred_scaled)
summary(mod_act); modplot(mod_act)
coefplot(mod_act)
#ggsave("Activities/figs/activities_coef_plot_both_regions_mod_act.png", width = 5, height = 4, units = "in")

mod_act_pois <- glm(activities ~ campgrounds + lakes + picnic + rivers + trails + road_min_dist + elevation + prop_wilderness,
                  data = pred_scaled, family = "poisson")
summary(mod_act_pois); modplot(mod_act_pois)
coefplot(mod_act_pois)

# negbin has lower AIC than poisson.

## What about the neg bin model above, with APUD as a factor in order to "control" for popularity of sites/
mod_act2 <- glm.nb(activities ~ camping + lakes + picnic + rivers + trails + road_min_dist + elevation + prop_wilderness + log1p(APUD),
                  data = pred_scaled, control = glm.control(maxit = 5000))
summary(mod_act2); modplot(mod_act2)
coefplot(mod_act2)

plot(mod_act2$fitted.values ~ mod_act2$y)
plot(mod_act$fitted.values ~ mod_act$y)

# pseudo r2 for these models? Using the proportional reduction in deviance as in my thesis...
# (null dev - resid dev) / null dev
summary(mod_act2) # with APUD_scaled. Much higher w/ log1p(APUD)
(897-575) / 897 # .358

summary(mod_act)
(759 - 592) / 759 # .22

# what about the mod_act with a linear term? looking mostly to compare the r2
mod_act_lin <- lm(log1p(activities) ~ campgrounds + lakes + picnic + rivers + trails + road_min_dist + elevation + prop_wilderness ,#+ log1p(APUD),
                  data = pred_scaled)
summary(mod_act_lin); modplot(mod_act_lin)
coefplot(mod_act_lin)
AIC(mod_act_lin) # hmm. this AIC is actually much lower - is this a legit comparison? Using the neg bin is definitely
# more statisitcally appropriate


# how about removing picnics?
mod_act3 <- glm.nb(activities ~ campgrounds + lakes + rivers + trails + road_min_dist + elevation + prop_wilderness + log1p(APUD),
                   data = pred_scaled, control = glm.control(maxit = 5000))
summary(mod_act3); modplot(mod_act3)


# what about a random effect for region?
mod_act4 <- glmer.nb(activities ~ camping + lakes + rivers + trails + road_min_dist + elevation + prop_wilderness + (1|region),
                     data = pred_scaled)
# singular fit
# works when I remove log(apud) from the predictors
summary(mod_act4)

# how about as a fixed effect?
mod_act5 <- glm.nb(activities ~ (camping + lakes + rivers + trails + road_min_dist + elevation + prop_wilderness)*region,
                     data = pred_scaled)
summary(mod_act5)
# pseudo r2 for these models? Using the proportional reduction in deviance as in my thesis...
# (null dev - resid dev) / null dev
(897.05 - 578.67) / 897.05
#.35

# how about more simply creating separate regressions for the two regions?
mtnloop <- pred_scaled %>% filter(region == "mtn")
midfork <- pred_scaled %>% filter(region == "mf") %>% dplyr::select(-picnic)

mf_act_mod <- glm.nb(activities ~ campgrounds + lakes + rivers + trails + road_min_dist ,
                     data = midfork, control = glm.control(maxit = 5000))
summary(mf_act_mod); modplot(mf_act_mod)
# do i have any picnic areas in the mf?
summary(midfork) # no. dropped from the model above
car::vif(mf_act_mod)
corrgram(pred_scaled %>% dplyr::select(activities, campgrounds, lakes, rivers, trails, road_min_dist, elevation, prop_wilderness),
         lower.panel = panel.cor, upper.panel = panel.pts)
# hmm. road_min_dist, elevation, and prop_wilderness are all fairly well correlated

mtn_act_mod <- glm.nb(activities ~ campgrounds + lakes + rivers + trails + picnic + road_min_dist + elevation + prop_wilderness,
                     data = mtnloop, control = glm.control(maxit = 5000))
summary(mtn_act_mod); modplot(mtn_act_mod)

#### Let's do this without elevation (and maybe also without wilderness?) due to high correlations
mod_act6 <- glm.nb(activities ~ (camping + lakes + rivers + trails + road_min_dist + prop_wilderness)*region,
                   data = pred_scaled)
summary(mod_act6)
# pseudo r2 for these models? Using the proportional reduction in deviance as in my thesis...
# (null dev - resid dev) / null dev
(863.48-580.7) /863.48
# .32

###### how about diversity? #####
mod_div <- lm(ginisimpson ~ camping + lakes + picnic + rivers + trails + road_min_dist + elevation + prop_wilderness,
               data = predictors_tib)
summary(mod_div); modplot(mod_div)

# less good. And really, i should be modeling this as a beta distributed variable, i think?
summary(predictors_tib)
preds_diversity <- predictors_tib %>% filter(!is.na(ginisimpson))
mod_div_b <- betareg(ginisimpson ~ camping + lakes + picnic + rivers + trails + road_min_dist + elevation,
        data = preds_diversity)
# doesn't work. need to do some more reading of this model structure: https://cran.r-project.org/web/packages/betareg/vignettes/betareg.pdf
# Right. I think it's because I have those zeros

preds_div_pos <- preds_diversity %>% filter(ginisimpson > 0)
mod_div_b <- betareg(ginisimpson ~ campgrounds + lakes + picnic + rivers + trails + road_min_dist + elevation + prop_wilderness,
                     data = preds_div_pos)
summary(mod_div_b)
modplot(mod_div_b)
mod_div_b
# ok, interesting. come back to this for interp retation

# does it get better if I drop some of the non-sig factors?
mod_div_ba <- betareg(ginisimpson ~road_min_dist + elevation,
                      data = preds_div_pos)
summary(mod_div_ba); modplot(mod_div_ba)

## how about for the regions separately?
preds_div_pos_mf <- preds_div_pos %>% filter(region == "mf")
preds_div_pos_mtn <- preds_div_pos %>% filter(region == "mtn")

mod_div_mf <- betareg(ginisimpson ~ campgrounds + lakes + rivers + trails + road_min_dist + elevation + prop_wilderness,
                      data = preds_div_pos_mf, na.action = na.omit) # not working
summary(mod_div_mf, type = "deviance") # note that the "regular" residuals don't work
summary(preds_div_pos_mf)

mod_div_mtn <- betareg(ginisimpson ~ campgrounds + lakes + picnic + rivers + trails + road_min_dist + elevation + prop_wilderness,
                      data = preds_div_pos_mtn) 
summary(mod_div_mtn)
# nothing significant
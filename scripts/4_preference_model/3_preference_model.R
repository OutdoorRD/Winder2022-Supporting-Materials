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

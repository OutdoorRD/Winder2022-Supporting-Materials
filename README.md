# Winder2022-Supporting-Materials
Associated files and scripts for the manuscript "An open-source image classifier for characterizing recreational activities across landscapes" by Samantha G. Winder, Heera Lee, Bumsuk Seo, Emilia H. Lia, & Spencer A. Wood (In review).

# scripts
Includes scripts used for the analysis of recreational activities in Flickr photos in the Mt Baker-Snoqualmie National Forest, USA. 

## 1_image_classifier
Scripts for training, applying, and evaluating the classifier. Scripts 1 and 2 have been abstracted in the `recCNNize` repository. Author: Bumsuk Seo

`1_Middlefork_Training_CNN.py` - Build our classifier model based on pre-trained InceptionResNetV2.

`2_Middlefork_BatchTagging.py` - Applies model to Middle Fork region.

`3_1_SamplePhotosForEvaluation.R` - Sample tagged photos for manual evaluation.

`3_2_Manual_evaluation_MiddleFork_MountainLoop.R` - Creates confusion matrices and other evaluation metrics for the image classifier.
## 2_APUD
Calculating activity photo user-days. Author: Sama Winder

`1_addingUserData.R` - Adds user id and date taken to predictions from the `recCNNize` model. Reads in `FlickrMiddlefork_predicted.shp` and `FlickrMBSMtLoop_predicted.shp` (the final outputs from the CNN model), joins them to user data, and writes out `<file>_predicted_users.geojson`. 

`2_creating_gridded_apuds.R` - reads in `*predicted_users.geojson` and intersects with a grid, then calculates Activity photo user-days (APUD) per grid cell and writes out `<area>_activities_by_grid_dddd.geojson`

## 3_survey
Scripts that work with the survey data, and compare it to the `recCNNize` predictions. Author: Sama Winder

`1_subsettingSurveyDatatoActivities.R` - reads in raw survey data, renames and combines some columns to make comparable with CNN outputs. Creates `survey_activities_data_20210510.csv`, as well as some other files that are divided into sites

`2_ComparingSurveytoModelUserDays.R` - creates scatterplot and calculates correlation between APUD and survey activities across the Middle Fork (also has some older code that does similar things at a "site" level)

## 4_preference__model
Scripts to relate landscape characteristics to activity diversity. Author: Sama Winder

`1_transforming_and_clipping_predictors.R` - reads in raw predictor (landscape characterstics) data and standardizes them for use in the preference model by transforming, flattening, validating, and clipping them to the relevant area. Writes out individual geojsons for each predictor.

`2_preparing_predictors.R` intersects various predictors with the `<area>_activities_by_grid_dddd.geojson`s, then write them all out in a single `Predictors_dddd.geojson` which includes the activities numbers as well.\

`3_preference_model.R` reads in `Predictors_dddd.geojson`, runs 2 negative binomial models (one per region), and creates a coefficient plot showing them against each other.

## 5_figures
Makes maps used in the manuscript. Author: Sama Winder

`flickr_map_mf_mtnloop.R` - Creates Figure 1, showing the locations of the study areas and all Flickr photos included in the study.

`mapping_activity_diversity.R` Creates Figure 5 - maps of activity diversity across both regions. Reads in `<area>_activities_by_grid_dddd.geojson` for both regions, combines, then creates a map of activity diversity.

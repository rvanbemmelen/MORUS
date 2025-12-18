## MORUS
Maps Of Really Unique Seabirds

# Data preparation scripts

Five scripts to prepare the data before modelling

0_species_list.R = creates data.frame with study species and labels
1_data_load.r = loads and combines ESAS/MWTL data, saves as rds file
2_distance_analysis.r = performs distance analysis, saves distance model objects as ddf_list.rdata file
3_data_prep_all.r = prepares separate rds files for each study species, using distance models
4_add_covariates.r = adds covariates to the rds files generated in script 3

# Modelling scripts

Scripts to model the distribution each study species are found in the folder
0_2023_bird_map/script

Here, several scripts can be found: 
01_0_run_script_Data_exploration.R = exploration of data
04_00_run_script_period_varying_model.r = period-varying model
04_02_run_final_model.r = 
05_01_maps_IDW.r = create interpolated maps using Inverse Distance Weighing
05_01_prediction_map.r = 
05_02_plot_conditional_effects_new_period.r = 
functions_model.r = functions used for the above scripts




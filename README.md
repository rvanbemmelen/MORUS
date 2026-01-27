
## MORUS: Maps Of Really Unique Seabirds

Morus is also the name of the genus of the Nortern Gannet *Morus bassanus*, one of the most iconic seabirds of the North Sea.

This repository contains script for the project "Dichtheidskaarten zeevogels" commissioned by Rijkswaterstaat to Wageningen Marine Research (WMR) and Waardenburg Ecology (WE).

The goal is to model distribution bimonthly, across 5-year periods, for a set of seabird species. These analyse use the ESAS (European Seabirds At Sea) database; both the public data and the non-public data of aerial surveys. These aerial surveys form part of the Monitoring Waterstaatkundige Toestand des Lands (MWTL) program, run by Rijkswaterstaat, and are performed by WE.

The scripts can be separated in two sets; the first set prepares data for analysis, the second set actually fits the models.


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
*01_0_run_script_Data_exploration.R* = exploration of data
*04_00_run_script_period_varying_model.r* = period-varying model
*04_02_run_final_model.r* = **XX**
*05_01_maps_IDW.r* = create interpolated maps using Inverse Distance Weighing
*05_01_prediction_map.r* = **XX**
*05_02_plot_conditional_effects_new_period.r* = **XX**
*functions_model.r* = functions used for the above scripts

# To-do list

- include new MWTL data as prepared by Job
- incorporate new species in distance analysis and data prep scripts
- clean up script distance sampling
- clean up script data prep
- improve methods section Rmd
- clean up modelling scripts?
- 

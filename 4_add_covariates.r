
##########------------------------------------------------------------------------------##############

# Script created for project "Dichtheidskaarten zeevogels" financed by Rijkswaterstaat
# Bird data of ESAS & MWTL data is processed in * scripts 1_data_load.r, 3_distance_analysis.r & 4_data_prep_all_species *
# Source of covariates is documented in * Seabirds maps of the North Sea - A short description of  metholodogy, van Donk et al. 2024*
# 10-2023
# Contact person: S.C. van Donk
# susanne.vandonk@wur.nl

# Datasets that should be added to the github:
# ESAS_species_check.xlsx

##########------------------------------------------------------------------------------##############

# Set to working directory and check
# --> Session --> Set working directory --> To working projectory
# On the project location there should be maps that are called:
# Data --> with dataset ESAS_species_check.xlsx
# Data/Abiotics --> with the covariate data
# Data/Breeding sites --> Breeding information
# Input data --> with prepared datasets with previous scripts per species
# Output/Figures & Output/Dataset

getwd()

# Remove all data 
rm(list= ls())

# Load packages 
library(viridis)
library(tidyverse)
library(sf)
library(readxl)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
#library(rgeos)
library (RSQLite)
library(raster)
library(ggmap)
#library(gstat)
library(RANN)
library(readxl)
#library(dplyr)


# Functions 
`%notin%` <- function(x,y) !(x %in% y)

theme_s <-  function () { 
  theme_classic(base_size=12)%+replace% 
    theme(axis.text = element_text(color="grey"),
          axis.line = element_line(color="grey"),
          axis.ticks = element_line(color="grey"),
          legend.title = element_text(face = "bold"),
          strip.background = element_rect(fill = "white", colour = "grey"),
          strip.text = element_text(margin = margin(0.2,0,0.2,0, "cm")) )
}



# Map
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
Europe_utm31 <- Europe %>% st_transform(32631)
plot(Europe_utm31$geometry)

Europe_latlong <- Europe %>% st_transform(4326)

# Info_grid
load("Data/JT_grid.Rdata") 

raster_utm31 <- rasterFromXYZ(info_grid,crs = 32631)
raster_latlong  <- projectRaster(raster_utm31,crs = 4326)

# put in different crs
info_grid <- st_as_sf(info_grid, coords = c("x","y"), 
                     crs = 32631, agr = "constant")

info_grid_latlong <- info_grid %>% st_transform(4326)
info_grid_latlong <- info_grid_latlong %>% 
  dplyr::mutate(longitude = unlist(purrr::map(.$geometry,1)),
         latitude = unlist(purrr::map(.$geometry,2))) %>%  st_drop_geometry() 



# 1. Load bird data ---------

# Information on bird species - dataset to be provided!!!
species_names <- read_excel("Data/ESAS_species_check.xlsx") 
species_names <- species_names %>% dplyr::select(euring_species_code, scientific_name,english_name)


# Output of earlier scripts should be put in a map called 'Input_data'
require(plyr)

files <- list.files(pattern = ".rds",path = "Input_data")

bird_data <- data.frame()

for (file in files){

    temp_dataset <- readRDS(paste0("Input_data/",file))
    temp_dataset <- temp_dataset %>% mutate(english_name = gsub("_", " ", substr(file,1,nchar(file)-4)))
    bird_data<-rbind(bird_data, temp_dataset)
    rm(temp_dataset)
  }


unique(bird_data$english_name)
unique(species_names$english_name)

# Check for missing densities
check <- bird_data %>% filter(is.na(dens))
rm(check)

# rename herring gull so it matches the name in other files
bird_data <- bird_data %>% mutate(english_name = ifelse(english_name == "Herring Gull","European Herring Gull",english_name))

# Merge two datasets
bird_data <- left_join(bird_data,species_names,by="english_name")

#check 
bird_data %>% dplyr::filter(is.na(euring_species_code))

range(bird_data$dens,na.rm=T)
length(unique(bird_data$campaign_key))
length(unique(bird_data$datetime))
range(bird_data$datetime)

# Rename SamplingMethod = count_method & TargetTaxa = species_counted for later use
bird_data <- bird_data %>% dplyr::rename(count_method = SamplingMethod, species_counted = TargetTaxa)
str(bird_data)

# Add year_month 
bird_data <- bird_data %>% mutate(year = format(datetime,format = "%Y"),
                                  month = format(datetime,format = "%m"),
                                  year_month = paste(year,month,sep="_"))
rm(species_names)

# make two datasets: one with all information per species and the other with only the location information, to safe time
bird_data_species <- bird_data
bird_data_unique <- bird_data %>% dplyr::select(c(campaign_key,datetime,count_method,species_counted,lon,lat,x_utm,y_utm,year,month,year_month)) %>% 
                    distinct()

dim(bird_data_species)
dim(bird_data_unique)
rm(bird_data)

# Add bimonthly period and bimonthly year (in which the year of december is equated with the following januari)
bird_data_species <- bird_data_species %>% 
  dplyr::mutate(year = as.numeric(year)) %>% 
  dplyr::mutate(Year_bimonthly = ifelse(month %in% "12",year+1,year))

bird_data_species <- bird_data_species %>% 
  dplyr::mutate(Bimonthly = case_when(month %in% c("12","01") ~ "dec-jan",
                                      month %in% c("02","03") ~ "feb-mrch",
                                      month %in% c("04","05") ~ "apr-may",
                                      month %in% c("06","07") ~ "jun-jul",
                                      month %in% c("08","09") ~ "aug-sep",
                                      month %in% c("10","11") ~ "oct-nov"))



#2. Load abiotics ------------------

# *Depth & sediment --------------------------------

# Source: Working Group on Spatial Fisheries Data (outputs from 2021 meeting)
# https://www.ices.dk/community/groups/pages/wgsfd.aspx 

DepthSediment <-  readRDS("Data/Abiotics/BirdMapHabitat.rds")
DepthSediment <- as.data.frame(DepthSediment) 
str(DepthSediment)

Plot_depth <- ggplot() + 
  geom_point(data = DepthSediment,aes(x = SI_LONG, y = SI_LATI, col = depth)) + 
  scale_colour_viridis()+
  geom_sf(data = Europe_latlong)+
  coord_sf(xlim=c(-5, 10),ylim=c(50,60))+
  theme_s()

ggsave(Plot_depth,file="Output/Figures/Covariates/Depth.png", width = 28, height = 20, units = 'cm')
rm(Plot_depth)

Plot_sediment <- ggplot() + 
  geom_point(data = DepthSediment,aes(x = SI_LONG, y = SI_LATI, col = MudPercent)) + 
  scale_colour_viridis()+
  geom_sf(data = Europe_latlong)+
  coord_sf(xlim=c(-5, 10),ylim=c(50,60))+
  theme_s()

ggsave(Plot_sediment,file="Output/Figures/Covariates/Sediment_mud.png", width = 28, height = 20, units = 'cm')
rm(Plot_sediment)

Plot_distance <- ggplot() + 
  geom_point(data = DepthSediment,aes(x = SI_LONG, y = SI_LATI, col = distance_coast_avg)) + 
  geom_sf(data = Europe_latlong)+
  coord_sf(xlim=c(-5, 10),ylim=c(50,60))+
  theme_s()

ggsave(Plot_distance,file="Output/Figures/Covariates/Distance_coast.png", width = 28, height = 20, units = 'cm')
rm(Plot_distance)

# Select part of data
DepthSediment <- DepthSediment %>% 
                dplyr::select(SI_LONG,SI_LATI,MudPercent,SandPercent,depth,distance_coast_avg)  
check <- DepthSediment %>%  # check missing values
    filter(is.na(depth))
str(DepthSediment)

# change projection to utm 31
DepthSediment  <- st_as_sf(DepthSediment, coords = c("SI_LONG","SI_LATI"), 
                     crs = 4326, agr = "constant")
DepthSediment  <- DepthSediment  %>% st_transform(32631)
DepthSediment <- DepthSediment %>% 
  mutate(x = unlist(map(.$geometry,1)),
         y = unlist(map(.$geometry,2)))
DepthSediment <- DepthSediment %>% ungroup() %>% st_drop_geometry()
str(DepthSediment)


# Couple Depth to all unique bird coordinates 
# Run this seperate for depth, as this dataset has some missing values where sediment-data don't have missing values
Depth_temp <-  DepthSediment %>% 
    dplyr::select(x,y)
temp_coord <- bird_data_unique %>% dplyr::select(x_utm,y_utm)
temp_bird <-  bird_data_unique %>% dplyr::select(x_utm,y_utm,datetime)
temp <- DepthSediment %>% dplyr::select(depth) %>% filter(!is.na(depth)) %>% 
    dplyr::mutate(RowNr = row_number()) 
nearest <- nn2(Depth_temp,temp_coord,k=1)
nearest_nr <- as.data.frame(nearest[[1]])
nearest_distance <- as.data.frame(nearest[[2]])
nearest <- bind_cols(nearest_nr,nearest_distance)
nearest <- bind_cols(nearest,temp_bird)
dim(Depth_temp);dim(temp_coord);dim(nearest)
names(nearest) <- c("RowNr","Distance","x_utm","y_utm","datetime")
str(nearest)
rm(Depth_temp,temp_coord)
nearest <- nearest %>% mutate(Distance_km = Distance/1000) %>% 
    dplyr::select(!Distance)
str(nearest)  
df_depth <- left_join(nearest,temp, by=c("RowNr"))
str(df_depth)


# Couple Distance & sediment to all unique bird coordinates 
Depth_temp <-  DepthSediment %>% 
    dplyr::select(x,y)
temp_coord <- bird_data_unique %>% dplyr::select(x_utm,y_utm)
temp <- DepthSediment %>% dplyr::select(MudPercent,SandPercent,distance_coast_avg) %>% 
    dplyr::mutate(RowNr = row_number()) 
nearest <- nn2(Depth_temp,temp_coord,k=1)
nearest_nr <- as.data.frame(nearest[[1]])
nearest_distance <- as.data.frame(nearest[[2]])
nearest <- bind_cols(nearest_nr,nearest_distance)
nearest <- bind_cols(nearest,temp_bird)
dim(Depth_temp);dim(temp_coord);dim(nearest)
names(nearest) <- c("RowNr","Distance","x_utm","y_utm","datetime")
str(nearest)
rm(Depth_temp,temp_coord)
nearest <- nearest %>% mutate(Distance_km = Distance/1000) %>% 
    dplyr::select(!Distance)
str(nearest)  
df_depth_sediment <- left_join(nearest,temp, by=c("RowNr"))
str(df_depth_sediment)
hist(df_depth_sediment$Distance_km,breaks=1000)

# Because every bird-coordinate is coupled to the nearest covariate point, 
# we change values in NA that are not close to a covariate point
df_depth_sediment <-  df_depth_sediment %>% 
  mutate(MudPercent = ifelse(Distance_km > 10, NA, MudPercent),
         SandPercent = ifelse(Distance_km > 10, NA, SandPercent),
         distance_coast_avg = ifelse(Distance_km > 10, NA, distance_coast_avg)) %>% 
  dplyr::select(!c(RowNr,Distance_km))
str(df_depth_sediment)

# Couple sediment and distance to coast to depth values.
df_depth_sediment <-  left_join(df_depth_sediment,df_depth)

df_depth_sediment <-  df_depth_sediment %>% 
  mutate(depth = ifelse(is.na(SandPercent), NA, depth)) %>% 
  dplyr::select(!c(RowNr,Distance_km))

# Test-plots
df_depth_sediment %>%  
    ggplot() + 
    geom_point(aes(x=x_utm,y=y_utm,col=(distance_coast_avg)))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
    theme_s()
  
df_depth_sediment %>%  
   filter(is.na(SandPercent)) %>% 
    ggplot() + 
    geom_point(aes(x=x_utm,y=y_utm,col=SandPercent))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
    theme_s()

# Test the points with NA. These points are really close to the borders or in land. 
df_depth_sediment %>%  
    filter(is.na(depth)) %>% 
    ggplot() + 
    geom_point(aes(x=x_utm,y=y_utm,col=(depth)))+
    scale_colour_viridis_b()+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
    theme_s()

rm(temp_coord,rasValue_temp,temp_combinePointValue,DepthSediment_temp_raster,grd,df_depth)
rm(DepthSediment,DepthSediment_temp)


str(df_depth_sediment)




#*Sea Surface Temperature data-----------

#Source: Marine Copernicus
#Product ID: SST_ATL_SST_L4_REP_OBSERVATIONS_010_026
#DOI: 10.48670/moi-00153

SST <- read.csv("Data/Abiotics/SST_1982-2020_BirdDensityMaps_MonthlyMean.csv")
SST <- SST %>% filter(Year > 1990)
str(SST)
unique(SST$Year);range(SST$Year)

# Ask Eleni: is Lat & long middle of raster of left/right upper/lower corner?
range(SST$Lat)
range(SST$Long)

plot_SST <- SST %>% 
  filter(Year %in% 2000) %>% 
  ggplot() + 
  geom_tile(aes(x = Long, y = Lat, col = Analysed_meanSST_Celsius, fill = Analysed_meanSST_Celsius),size=0.1) + 
  scale_colour_viridis_b()+
  scale_fill_viridis_b()+
  geom_sf(data = Europe_latlong)+
  coord_sf(xlim=c(-5, 10),ylim=c(50,60))+
  facet_wrap(~Month)+
  ggtitle("2010")+
  theme_s()

ggsave(plot_SST,file="Output/Figures/Covariates/SST.png", width = 28, height = 20, units = 'cm')
rm(plot_SST)


# Add a year_month code to SST dataset
SST <- SST %>% dplyr::mutate(Month = as.factor(Month),
                     Month = ifelse(Month %in% c("1","2","3","4","5","6","7","8","9"),paste0(0,Month),Month),
                     Year = as.factor(Year))
SST <- SST %>% dplyr::mutate(year_month = paste(Year,Month,sep="_")) %>%
                     dplyr::select(Long,Lat,Analysed_meanSST_Celsius,year_month) 
unique(SST$year_month)




# SST has, for now, only data till 2020-12. So remove these dates from Bird_data.
range(SST$year_month)
YM <- bird_data_unique %>% filter(!year %in% c("2021","2022"))
YM <- unique(YM$year_month)

# Create empty dataset
df_SST <- data.frame()

# For-loop to couple SST data to unique bird-coordinates with same year_month code

for (i in YM ){
  
  temp <- bird_data_unique %>% filter(year_month == i)
  temp_SST <- SST %>% filter(year_month == i)
  
  temp_ras <- rasterFromXYZ(temp_SST,crs = 4326)
  temp_ras  <- projectRaster(temp_ras,crs = 32631)
  
  temp_coord <- temp
  coordinates(temp_coord)= ~ x_utm + y_utm
  crs(temp_coord) <- CRS('+init=EPSG:32631')
  
  rasValue_temp=raster::extract(temp_ras, temp_coord)
  temp_combinePointValue=cbind(temp_coord,rasValue_temp)
  
  temp_DF <- as.data.frame(temp_combinePointValue)
  temp_DF <- temp_DF %>% dplyr::select(datetime, x_utm,y_utm,Analysed_meanSST_Celsius)
  
  df_SST <- bind_rows(df_SST,temp_DF)
  
}


# check
temp_DF %>%  
    ggplot() + 
    geom_point(aes(x=x_utm,y=y_utm,col=(Analysed_meanSST_Celsius)))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
    theme_s()
  
temp_SST %>%  
    ggplot() + 
    geom_point(aes(x=Long,y=Lat,col=(Analysed_meanSST_Celsius)))+
    geom_sf(data = Europe_latlong)+
    coord_sf(xlim=c(-5, 10),ylim=c(50,60))+
    theme_s()

# check NA's 
temp_SST %>%  
    filter(is.na(Analysed_meanSST_Celsius)) %>% 
    ggplot() + 
    geom_point(aes(x=Long,y=Lat,col=(Analysed_meanSST_Celsius)))+
    geom_sf(data = Europe_latlong)+
    coord_sf(xlim=c(-5, 10),ylim=c(50,60))+
    theme_s()

rm(temp,temp_SST,temp_ras,temp_coord,rasValue_temp,temp_combinePointValue,temp_DF)
rm(SST)

# dataset with SST --> df_SST --> same number of points as bird_data
dim(df_SST)
dim(bird_data_unique)

str(df_SST)






# *Chl-a --> Copernicus --------------

# SOURCE:  Marine Copernicus 
# Range: January 1993 to June 2022

Chl_a <- readRDS("Data/Abiotics/Chlorophyll_1993-2022_BirdDensityMaps_MonthlyMean.rds")
str(Chl_a)
Chl_a <- Chl_a %>% ungroup()
unique(Chl_a$Year);range(Chl_a$Year)

plot_Chl_a <- Chl_a %>% 
  dplyr::filter(Year %in% 2000) %>% 
  ggplot() + 
  geom_tile(aes(x = Long, y = Lat, col = log(meanChlorophyll+1), fill = log(meanChlorophyll+1)),size=0.1) + 
  scale_colour_viridis_c()+
  scale_fill_viridis_c()+
  geom_sf(data = Europe_latlong)+
  coord_sf(xlim=c(-5, 10),ylim=c(50,60))+
  facet_wrap(~Month)+
  ggtitle("2010")+
  theme_s()

ggsave(plot_Chl_a,file="Output/Figures/Covariates/Chl_a.png", width = 28, height = 20, units = 'cm')
rm(plot_Chl_a)


# Add a year_month code to Chl-a dataset
Chl_a <- Chl_a %>% dplyr::mutate(Month = as.factor(Month),
                     Month = ifelse(Month %in% c("1","2","3","4","5","6","7","8","9"),paste0(0,Month),Month),
                     Year = as.factor(Year))
Chl_a <- Chl_a %>% dplyr::mutate(year_month = paste(Year,Month,sep="_")) %>%
                     dplyr::select(Long,Lat,meanChlorophyll,year_month) 
unique(Chl_a$year_month)

# Create empty dataset
df_Chl_a <- data.frame()

# Chl_a starts in 1993, so remove missing year_month's from list
range(Chl_a$year_month)
range(bird_data_unique$year_month)
YM <- bird_data_unique %>% filter(!year %in% c("1991","1992"))
YM <- YM %>% filter(!year_month %in% c("2022_07","2022_08","2022_09","2022_10","2022_11","2022_12"))
YM <- unique(YM$year_month)

# For-loop to couple Chl-a data to unique bird-coordinates with same year_month code

for (i in YM ){
  
  temp <- bird_data_unique %>% filter(year_month == i)
  temp_Chl_a <- Chl_a %>% filter(year_month == i)
  
  temp_s <- temp %>% dplyr::select(x_utm,y_utm)
  
  # Transform chlorophyll data to utm_31
  temp_Chl_a_s <- temp_Chl_a %>% dplyr::select(Long,Lat)
  temp_Chl_a_s <- st_as_sf(temp_Chl_a_s, coords = c("Long","Lat"), 
                     crs = 4326, agr = "constant")
  temp_Chl_a_s <- temp_Chl_a_s %>% st_transform(32631)
  temp_Chl_a_s <- temp_Chl_a_s %>% 
                  dplyr::mutate(x = unlist(map(.$geometry,1)),
                          y = unlist(map(.$geometry,2))) %>%  st_drop_geometry() 
  
  nearest <- nn2(temp_Chl_a_s,temp_s,k=1)
  distance <- as.data.frame(nearest[[2]])
  nearest <- as.data.frame(nearest[[1]])
  names(nearest) <- "RowNr"
  names(distance) <- "Distance"
  nearest <- cbind(nearest,distance)
  rm(temp_Chl_a_s,temp_s)
  
  temp <- bind_cols(temp,nearest)
  temp_Chl_a <- temp_Chl_a %>% dplyr::mutate(RowNr = row_number())
  
  temp <- left_join(temp,temp_Chl_a, by=c("RowNr","year_month"))

  temp_DF <- temp %>% dplyr::select(datetime, x_utm,y_utm,meanChlorophyll,Distance)
  
  df_Chl_a <- bind_rows(df_Chl_a,temp_DF)
  
}

# check
range(temp_DF$meanChlorophyll)
temp_DF %>%  
    ggplot() + 
    geom_point(aes(x=x_utm,y=y_utm,col=(meanChlorophyll)))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
    theme_s()
  
temp_Chl_a %>%  
    filter(meanChlorophyll<=max(temp_DF$meanChlorophyll)) %>% 
    ggplot() + 
    geom_point(aes(x=Long,y=Lat,col=(meanChlorophyll)))+
    geom_sf(data = Europe_latlong)+
    coord_sf(xlim=c(-5, 10),ylim=c(50,60))+
    theme_s()

# Check Distance, which is in meters
hist(df_Chl_a$Distance,xlim=c(0,10000),breaks = 10000)

df_Chl_a %>%  
    filter(datetime > 2015) %>% 
    filter(Distance > 15000) %>% 
    ggplot() + 
    geom_point(aes(x=x_utm,y=y_utm,col=Distance))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    coord_sf(xlim=c(-10000, 900000),ylim=c(5300000,7500000))+
    theme_s() 

# Remove values that are further than 15 km away from nearest chl-a measurement
dim(df_Chl_a)
df_Chl_a <- df_Chl_a %>% 
  filter(Distance <= 15000) %>% 
  dplyr::select(-Distance)
dim(df_Chl_a)

rm(temp,temp_Chl_a,temp_DF,Chl_a)





# * Breeding sites -------------------- 

# SOURCE: EBBA2 
# One number for a period between 2013-2017
# Both presence of breeding site as abundance in breeding pairs
# 50*50km grid that is provided when requesting the data
# One datapoint for a 2013-2017 period

EBBA2_breeding <- read.csv("Data/Breeding sites/ebba2_data_occurrence_50km_2013_2017.csv",sep=";",dec = ",")
EBBA2_abundance <- read.csv("Data/Breeding sites/ebba2_data_abundance_50km_2013_2017.csv",sep=";",dec = ",")
str(EBBA2_breeding)
str(EBBA2_abundance)

# combine Presence and abundance data
EBBA2_breeding <- full_join(EBBA2_breeding,EBBA2_abundance)
rm(EBBA2_abundance)

# Change codes for abundance in number of breeding pairs;
# A. 1-9 pairs
# B. 10-99 pairs
# C. 100-999 pairs
# D. 1,000-9,999 pairs
# E. 10,000-99,999 pairs
# G. More than 100,000 pairs
# NA. Not reported
unique(EBBA2_breeding$abundance)
EBBA2_breeding <- EBBA2_breeding %>% 
  dplyr::mutate(abundance = as.factor(abundance),
    Nr_breeding_pairs_closest_colony = case_when(
  abundance %in% "A" ~ "1-9",
  abundance %in% "B" ~ "10-99",
  abundance %in% "C" ~ "100-999",
  abundance %in% "D" ~ "1000-9999",
  abundance %in% "E" ~ "10000-99999",
  abundance %in% "F" ~ ">100000",
  is.na(abundance) ~ "NA"
))

str(EBBA2_breeding)

# couple to grid 50-km Universal Transversal Mercator (UTM) grid from EBBA2 website
EBBA2_grid <- st_read("Data/Breeding sites/ebba2_grid50x50_v1.shp")
# Change projection
EBBA2_grid <- EBBA2_grid %>% st_transform(32631)
EBBA2_breeding <- left_join(EBBA2_breeding,EBBA2_grid,by="cell50x50")
str(EBBA2_breeding)

# Add Species-list to data
Species <- bird_data_species %>% dplyr::select(english_name,scientific_name) %>% distinct()
EBBA2_breeding <- EBBA2_breeding %>% dplyr::rename(scientific_name = birdlife_scientific_name)
EBBA2_breeding <- EBBA2_breeding %>% dplyr::mutate(scientific_name = ifelse(scientific_name=="Catharacta skua","Stercorarius skua",scientific_name))
EBBA2_breeding <- left_join(EBBA2_breeding,Species,by="scientific_name")

plot_breeding <- EBBA2_breeding %>%  
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_sf(aes(geometry=geometry,fill=Nr_breeding_pairs_closest_colony))+
    theme_s()+
    facet_wrap(~scientific_name)+  
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))
ggsave(plot_breeding,file="Output/Figures/Covariates/breeding.png", width = 28, height = 20, units = 'cm')
rm(plot_breeding)


# Add centroid of squares to calculate distance to nest
EBBA2_breeding <- EBBA2_breeding %>% dplyr::mutate(centroid = st_centroid(geometry))

# Check
EBBA2_breeding %>%  
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_sf(aes(geometry=geometry),fill="purple")+
    geom_sf(aes(geometry=centroid),col="yellow")+
    theme_s()+
    facet_wrap(~scientific_name)+  
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))

EBBA2_breeding <- EBBA2_breeding %>% 
    mutate(x_utm= unlist(map(.$centroid,1)),
          y_utm = unlist(map(.$centroid,2))) 


# For loop to couple every coordinate per species to its closest breeding site
Species <- unique(EBBA2_breeding$english_name)
Breeding_sites_km <- data.frame()

for(i in Species){ 

df_breeding_temp <-  EBBA2_breeding %>% 
    dplyr::select(english_name,x_utm,y_utm) %>% 
    filter(english_name == i)
bird <- bird_data_species %>% dplyr::select(english_name,x_utm,y_utm) %>% 
    filter(english_name == i)
df_breeding_temp <-  df_breeding_temp %>% dplyr::select(-english_name)
bird <- bird %>% dplyr::select(-english_name)
  
nearest <- nn2(df_breeding_temp,bird,k=1)
nearest_nr <- as.data.frame(nearest[[1]])
nearest_distance <- as.data.frame(nearest[[2]])
nearest <- bind_cols(nearest_nr,nearest_distance)
dim(bird);dim(nearest)
names(nearest) <- c("RowNr","Distance_site")
rm(df_breeding_temp,bird,nearest_nr,nearest_distance)
nearest <- nearest %>% mutate(Distance_breeding_site_km = Distance_site/1000,
                              english_name = i) %>% 
    dplyr::select(!Distance_site)
str(nearest)  
bird_data <- bird_data_species %>% 
    filter(english_name == i)
temp <- bind_cols(bird_data,nearest)
temp_sites <- EBBA2_breeding %>% 
    filter(english_name == i) %>% 
    dplyr::mutate(RowNr = row_number()) %>% 
    dplyr::select(english_name,RowNr,abundance,Nr_breeding_pairs_closest_colony)

temp <- left_join(temp,temp_sites, by=c("RowNr"))
str(temp)
temp <- temp %>% dplyr::select(datetime, x_utm,y_utm,english_name,Distance_breeding_site_km,Nr_breeding_pairs_closest_colony)
  
Breeding_sites_km <- bind_rows(Breeding_sites_km,temp)
rm(temp,nearest,bird_data)

}

str(Breeding_sites_km)

# Check
plot <- Breeding_sites_km %>%  
  filter(datetime < "1993-08-14 06:04:00") %>% 
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+ 
    geom_sf(data=EBBA2_breeding,aes(geometry=geometry,fill=Nr_breeding_pairs_closest_colony))+
    geom_point(aes(x=x_utm,y=y_utm,col=Distance_breeding_site_km))+
    coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
    theme_s()+
    facet_wrap(~english_name)

ggsave(plot,file="Output/Figures/Covariates/breeding_distance.png", width = 28, height = 20, units = 'cm')
rm(plot)


Breeding_sites_km %>% filter(is.na(Breeding_sites_km$Distance_breeding_site_km))
hist(Breeding_sites_km$Distance_breeding_site_km,breaks=1000)

# Because I used the middle point to calculate distance from breeding site, set all values under 25 on 25 km.
Breeding_sites_km <- Breeding_sites_km %>% 
  dplyr::mutate(Distance_breeding_site_km = ifelse(Distance_breeding_site_km<=25,25,Distance_breeding_site_km))

hist(Breeding_sites_km$Distance_breeding_site_km,breaks=1000,xlim=c(0,50))





# Repeat what has been done above but then calculate the distance to large breeding colonies

# Not for Red-throated diver because this species had only smaller colonies
EBBA2_breeding_BIG <- EBBA2_breeding %>% filter(scientific_name %notin% c("Gavia stellata"))

# Species with relatively smaller colonies like Sandwich Tern, Great Black-backed gull and Great skua: colonies > 100 breeding pairs
# Other species: colonies > 1000
EBBA2_breeding_BIG <- EBBA2_breeding_BIG %>% 
  mutate(Nr_breeding_pairs_select = case_when(
    scientific_name %in% c("Larus marinus","Stercorarius skua","Thalasseus sandvicensis") & Nr_breeding_pairs_closest_colony %in% c("100-999","1000-9999",'10000-99999') ~ "yea",
    scientific_name %in% c("Fulmarus glacialis","Morus bassanus","Rissa tridactyla", "Larus fuscus","Larus argentatus","Fratercula arctica","Alca torda","Uria aalge") & Nr_breeding_pairs_closest_colony %in% c("1000-9999",'10000-99999') ~ "yea"))
table(EBBA2_breeding_BIG$Nr_breeding_pairs_select,EBBA2_breeding_BIG$Nr_breeding_pairs_closest_colony)
EBBA2_breeding_BIG <- EBBA2_breeding_BIG %>% filter(Nr_breeding_pairs_select %in% "yea")

EBBA2_breeding_BIG <- EBBA2_breeding_BIG %>% dplyr::rename(Nr_breeding_pairs_biggest_colony = Nr_breeding_pairs_closest_colony)

plot_breeding <- EBBA2_breeding_BIG %>%  
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_sf(aes(geometry=geometry,fill=Nr_breeding_pairs_biggest_colony))+
    theme_s()+
    facet_wrap(~scientific_name)+  
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))
ggsave(plot_breeding,file="Output/Figures/Covariates/breeding_biggest.png", width = 28, height = 20, units = 'cm')
rm(plot_breeding)



# For-loop to couple every coordinate per species to its closest breeding site
Species <- unique(EBBA2_breeding_BIG$english_name)
Breeding_sites_km2 <- data.frame()

for(i in Species){ 

df_breeding_temp <-  EBBA2_breeding_BIG %>% 
    dplyr::select(english_name,x_utm,y_utm) %>% 
    filter(english_name == i)
bird <- bird_data_species %>% dplyr::select(english_name,x_utm,y_utm) %>% 
    filter(english_name == i)
df_breeding_temp <-  df_breeding_temp %>% dplyr::select(-english_name)
bird <- bird %>% dplyr::select(-english_name)
  
nearest <- nn2(df_breeding_temp,bird,k=1)
nearest_nr <- as.data.frame(nearest[[1]])
nearest_distance <- as.data.frame(nearest[[2]])
nearest <- bind_cols(nearest_nr,nearest_distance)
dim(bird);dim(nearest)
names(nearest) <- c("RowNr","Distance_site")
rm(df_breeding_temp,bird,nearest_nr,nearest_distance)
nearest <- nearest %>% mutate(Distance_big_breeding_site_km = Distance_site/1000,
                              english_name = i) %>% 
    dplyr::select(!Distance_site)
str(nearest)  
bird_data <- bird_data_species %>% 
    filter(english_name == i)
temp <- bind_cols(bird_data,nearest)
temp_sites <- EBBA2_breeding_BIG %>% 
    filter(english_name == i) %>% 
    dplyr::mutate(RowNr = row_number()) %>% 
    dplyr::select(english_name,RowNr,abundance,Nr_breeding_pairs_biggest_colony)

temp <- left_join(temp,temp_sites, by=c("RowNr"))
str(temp)
temp <- temp %>% dplyr::select(datetime, x_utm,y_utm,english_name,Distance_big_breeding_site_km,Nr_breeding_pairs_biggest_colony)
  
Breeding_sites_km2 <- bind_rows(Breeding_sites_km2,temp)
rm(temp,nearest,bird_data)

}



str(Breeding_sites_km2)

# Check
plot <- Breeding_sites_km2 %>%  
  filter(datetime < "1993-08-14 06:04:00") %>% 
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+ 
    geom_sf(data=EBBA2_breeding_BIG,aes(geometry=geometry,fill=Nr_breeding_pairs_biggest_colony))+
    geom_point(aes(x=x_utm,y=y_utm,col=Distance_big_breeding_site_km))+
    coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
    theme_s()+
    facet_wrap(~english_name)
ggsave(plot,file="Output/Figures/Covariates/breeding_biggest_distance.png", width = 28, height = 20, units = 'cm')
rm(plot)


# Because the middle point to calculate distance from breeding site was used, set all values under 25 on 25 km.
Breeding_sites_km2 <- Breeding_sites_km2 %>% 
  dplyr::mutate(Distance_big_breeding_site_km = ifelse(Distance_big_breeding_site_km<=25,25,Distance_big_breeding_site_km))


# Couple the two datasets
Breeding_sites_km <- left_join(Breeding_sites_km,Breeding_sites_km2,by=c("datetime","x_utm","y_utm",'english_name'))
str(Breeding_sites_km)



rm(breeding_sites_non_uk,df_breeding,EBBA2_breeding,Breeding_sites_km2,EBBA2_breeding_BIG)






# *Other species ---------


# SOURCE: Data of Waggitt et al. 
Fitted_values_waggit <- read.csv("Data/Waggitt_Seabirds_Cetaceans/Fitted.csv")
str(Fitted_values_waggit)

# Data organization & calculation of total auks and total marine mammals per 10km2
Fitted_values_waggit <- Fitted_values_waggit %>% dplyr::select(Long,Lat,Fitted,Month,Species)
range(Fitted_values_waggit$Fitted,na.rm=T)
Fitted_values_waggit <- Fitted_values_waggit %>%  pivot_wider(values_from = Fitted,names_from = Species)

Fitted_values_waggit <- Fitted_values_waggit %>% 
            rowwise() %>% 
            dplyr::mutate(marine_mammals_total_10km_waggit = sum(`Bottlenose Dolphin`,`Common Dolphin`,`Fin Whale`,`Harbour Porpoise`,`Killer Whale`,`Minke Whale`,`Pilot Whale`,`Risso's Dolphin`,`Sperm Whale`,`Striped Dolphin`,`White-beaked Dolphin`,`White-sided Dolphin`, na.rm = TRUE),
                   auks_total_10km_waggit = sum(`Atlantic Puffin`,`Common Guillemot`,`Razorbill`,na.rm=TRUE))

range(Fitted_values_waggit$marine_mammals_total_10km_waggit,na.rm=T)
range(Fitted_values_waggit$auks_total_10km_waggit,na.rm=T)
range(Fitted_values_waggit$`Razorbill`,na.rm=T)


# Selection of species
Fitted_values_waggit <- Fitted_values_waggit %>% 
            dplyr::select(!c(`Bottlenose Dolphin`,`Common Dolphin`,`Fin Whale`,`Harbour Porpoise`,`Killer Whale`,`Minke Whale`,`Pilot Whale`,`Risso's Dolphin`,`Sperm Whale`,`Striped Dolphin`,`White-beaked Dolphin`,`White-sided Dolphin`))

Fitted_values_waggit <- Fitted_values_waggit %>% ungroup()
str(Fitted_values_waggit)

# Renaming
Fitted_values_waggit <- Fitted_values_waggit %>% 
  dplyr::rename(Waggit_puffin_10km = `Atlantic Puffin`,
                Waggit_kittiwake_10km = `Black-legged Kittiwake`,
                Waggit_guillemot_10km = `Common Guillemot`,
                Waggit_shag_10km = `European Shag`,
                Waggit_storm_petrel_10km = `European Storm Petrel`,
                Waggit_great_skua_10km = `Great Skua`,
                Waggit_herring_gull_10km = `Herring Gull`,
                Waggit_l_black_backed_gull_10km = `Lesser Black-backed Gull`,
                Waggit_manx_shearwater_10km = `Manx Shearwater`,
                Waggit_northern_fulmar_10km = `Northern Fulmar`,
                Waggit_gannet_10km = `Northern Gannet`,
                Waggit_razorbill_10km = `Razorbill`)

str(Fitted_values_waggit) 

# Check plots
Fitted_values_waggit %>%  
    ggplot() + 
    geom_point(aes(x=Long,y=Lat,col=marine_mammals_total_10km_waggit))+
    geom_sf(data = Europe_latlong)+
    coord_sf(xlim=c(-5, 10),ylim=c(50,60))+
  facet_wrap(~Month)+  
  theme_s()

Fitted_values_waggit %>%  
    ggplot() + 
    geom_point(aes(x=Long,y=Lat,col=auks_total_10km_waggit))+
    geom_sf(data = Europe_latlong)+
    coord_sf(xlim=c(-5, 10),ylim=c(50,60))+
  facet_wrap(~Month)+  
  theme_s()





# For-loop to couple every coordinate of the bird data to other species distribution
YM <- bird_data_unique 
YM <- unique(YM$month)

unique(Fitted_values_waggit$Month)
Fitted_values_waggit <- Fitted_values_waggit %>% 
  dplyr::mutate(month = as.factor(Month),
         month = ifelse(month %notin% c("10","11","12"),paste0("0",month),month))
unique(Fitted_values_waggit$month)

df_waggit <- data.frame()

for (i in YM ){
  
  temp <- bird_data_unique %>% filter(month == i)
  temp_waggit <- Fitted_values_waggit %>% filter(month == i)
  
  temp_s <- temp %>% dplyr::select(x_utm,y_utm)

  # Transform Waggitt data to utm_31
  temp_waggit_s <- temp_waggit %>% dplyr::select(Long,Lat)
  temp_waggit_s <- st_as_sf(temp_waggit_s, coords = c("Long","Lat"), 
                     crs = 4326, agr = "constant")
  temp_waggit_s <- temp_waggit_s %>% st_transform(32631)
  temp_waggit_s <- temp_waggit_s %>% 
                  dplyr::mutate(x = unlist(map(.$geometry,1)),
                          y = unlist(map(.$geometry,2))) %>%  st_drop_geometry() 

  nearest <- nn2(temp_waggit_s,temp_s,k=1)
  distance <- as.data.frame(nearest[[2]])
  nearest <- as.data.frame(nearest[[1]])
  names(nearest) <- "RowNr"
  names(distance) <- "Distance"
  nearest <- cbind(nearest,distance)
  rm(temp_waggit_s,temp_s)
  
  temp <- bind_cols(temp,nearest)
  temp_waggit <- temp_waggit %>% dplyr::mutate(RowNr = row_number())
  
  temp <- left_join(temp,temp_waggit, by=c("RowNr","month"))
  temp_DF <- temp %>% dplyr::select(datetime, x_utm,y_utm,Distance,Waggit_puffin_10km,Waggit_kittiwake_10km,Waggit_guillemot_10km,Waggit_shag_10km,Waggit_storm_petrel_10km,Waggit_great_skua_10km,Waggit_herring_gull_10km,Waggit_l_black_backed_gull_10km,Waggit_manx_shearwater_10km,Waggit_northern_fulmar_10km,Waggit_gannet_10km,Waggit_razorbill_10km,marine_mammals_total_10km_waggit,auks_total_10km_waggit)
  
  df_waggit <- bind_rows(df_waggit,temp_DF)
  
}



# Check Distance, which is in meters
hist(df_Chl_a$Distance,xlim=c(0,10000),breaks = 10000)

df_waggit %>%  
    filter(datetime > 2015) %>% 
    filter(Distance > 15000) %>% 
    ggplot() + 
    geom_point(aes(x=x_utm,y=y_utm,col=Distance))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    coord_sf(xlim=c(-10000, 900000),ylim=c(5300000,7500000))+
    theme_s() 

# Because I used nearest point to a point, I should remove the points that are superfar from nearest point. 
# Remove values that are further than 15 km away from a other species point
dim(df_waggit)
df_waggit <- df_waggit %>% 
  filter(Distance <= 15000) %>% 
  dplyr::select(-Distance)
dim(df_waggit)


# check
temp_DF %>%  
    ggplot() + 
    geom_point(aes(x=x_utm,y=y_utm,col=auks_total_10km_waggit))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
    theme_s()
  
Fitted_values_waggit %>%  
  filter(month %in% "06") %>% 
    ggplot() + 
    geom_point(aes(x=Long,y=Lat,col=auks_total_10km_waggit))+
    geom_sf(data = Europe_latlong)+
    coord_sf(xlim=c(-5, 10),ylim=c(50,60))+
    theme_s()

rm(temp,temp_DF,Fitted_values_waggit,nearest,YM,temp_waggit)









#3. Load human activity ---------------
# SOURCE: RWS 2017

# This part of the script takes a long time. 
# Can take up to several hours/a day depending on the computer
# Be aware!


# *Shipping lanes-----------------

Shipping_lanes <- st_read("Data/ShippingLanes/ShippingLanes_Dutch_EEZ.shp")
str(Shipping_lanes)
crs(Shipping_lanes)

Shipping_lanes_marin <- st_read("Data/ShippingLanes/Shipping lanes marin/clearways_scheepvaart.shp")
str(Shipping_lanes_marin)
crs(Shipping_lanes_marin)


# change crs
Shipping_lanes <- Shipping_lanes %>% st_transform(32631)
Shipping_lanes_marin <- Shipping_lanes_marin %>% st_transform(32631)

# Do not select all lanes
Shipping_lanes <- Shipping_lanes %>% filter(entiteit %in% c("begrenzing","junction","clearway","draaiplaats"))


plot_Shipping_lanes <- Shipping_lanes %>% 
  ggplot() + 
  geom_sf(aes(geometry=geometry),fill="pink")+
  geom_sf(data=Shipping_lanes,aes(geometry=geometry),fill="pink")+
   geom_sf(data=Shipping_lanes_marin,aes(geometry=geometry),fill="pink")+
   geom_sf(data = Europe_utm31)+
  coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
  theme_s()

ggsave(plot_Shipping_lanes,file="Output/Figures/Covariates/Shipping_lanes.png", width = 28, height = 20, units = 'cm')
rm(plot_Shipping_lanes)


# Calculate points close to shipping lanes. Only the outside lines are drawn. 
info_grid <- st_as_sf(info_grid, coords = c("x","y"), 
                     crs = 32631, agr = "constant")
pts <- bird_data_unique %>% mutate(x=x_utm,y=y_utm) 
pts <- st_as_sf(pts, coords = c("x","y"), 
                     crs = 32631, agr = "constant")


# Cut dataset in pieces as this part takes a long time
pts_1 <- pts %>% dplyr::filter(year_month <="2000_01")

pts_2  <- pts %>% dplyr::filter(year_month >"2000_01" & year_month <= "2010_01")

pts_3 <- pts %>% dplyr::filter(year_month > "2010_01")


distance_1 <- nngeo::st_nn(pts_1, Shipping_lanes, k = 1, returnDist = T)
kleave1 <- distance_1

distance_2 <- nngeo::st_nn(pts_2, Shipping_lanes, k = 1, returnDist = T)
kleave2 <- distance_2

distance_3 <- nngeo::st_nn(pts_3, Shipping_lanes, k = 1, returnDist = T)
kleave3 <- distance_3

distance_1 <- as.data.frame(distance_1[[2]])
distance_2 <- as.data.frame(distance_2[[2]])
distance_3 <- as.data.frame(distance_3[[2]])


# CHECK if number of columns are correct everywhere where there is a *
distance_1 <- distance_1 %>% 
   tidyr::pivot_longer(cols = 1:117729) # * here
distance_1 <- distance_1 %>% dplyr::select(value) %>% dplyr::rename(DistanceShippingLanesInKm = value)
distance_1 <- distance_1 %>% dplyr::mutate(DistanceShippingLanesInKm = DistanceShippingLanesInKm/1000) 
DistanceShippingLanes_1 <- cbind(pts_1,distance_1)
str(DistanceShippingLanes_1)

distance_2 <- distance_2 %>% 
   tidyr::pivot_longer(cols = 1:174279) # * here
distance_2 <- distance_2 %>% dplyr::select(value) %>% dplyr::rename(DistanceShippingLanesInKm = value)
distance_2 <- distance_2 %>% dplyr::mutate(DistanceShippingLanesInKm = DistanceShippingLanesInKm/1000) 
DistanceShippingLanes_2 <- cbind(pts_2,distance_2)
str(DistanceShippingLanes_2)

distance_3 <- distance_3 %>% 
   tidyr::pivot_longer(cols = 1:146131) # * here
distance_3 <- distance_3 %>% dplyr::select(value) %>% dplyr::rename(DistanceShippingLanesInKm = value)
distance_3 <- distance_3 %>% dplyr::mutate(DistanceShippingLanesInKm = DistanceShippingLanesInKm/1000) 
DistanceShippingLanes_3 <- cbind(pts_3,distance_3)
str(DistanceShippingLanes_3)

DistanceShippingLanes <- bind_rows(DistanceShippingLanes_1,DistanceShippingLanes_2,DistanceShippingLanes_3)


# Check 
DistanceShippingLanes %>% 
    #filter(is.na(DistanceShippingLanesInKm)) %>%   
    filter(DistanceShippingLanesInKm <= 10) %>% 
    ggplot() + 
    geom_point(aes(x=x_utm,y=y_utm,col=log(DistanceShippingLanesInKm+1)))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_sf(data = Shipping_lanes,aes(geometry=geometry))+
    coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
    theme_s()


DistanceShippingLanes <- DistanceShippingLanes %>% 
                          dplyr::select(datetime,x_utm,y_utm,DistanceShippingLanesInKm) %>% st_drop_geometry()




# Also calculate points in Ducth part with Shipping_lanes_marin.

temp <- st_as_sf(bird_data_unique, coords = c("x_utm","y_utm"), 
                     crs = 32631, agr = "constant") %>% 
    mutate(x_utm = unlist(map(.$geometry,1)),
         y_utm = unlist(map(.$geometry,2))) %>% 
    dplyr::select(geometry,x_utm,y_utm,datetime)
temp_ship_in <- sf::st_join(temp,Shipping_lanes_marin)
dim(temp_ship_in);dim(temp)

str(temp_ship_in)
temp_ship_in <- temp_ship_in %>% st_drop_geometry()
temp_ship_in <- temp_ship_in %>% 
        dplyr::mutate(InDutchPartShipLane = if_else(is.na(OBJECTID),"No","Yes"))
temp_ship_in <- temp_ship_in %>% dplyr::select(datetime,x_utm,y_utm,InDutchPartShipLane)
temp_ship_in %>% filter(InDutchPartShipLane %in% "Yes")


str(temp_ship_in)
dim(temp_ship_in)

# Check 
temp_ship_in %>% 
    ggplot() + 
    geom_point(aes(x=x_utm,y=y_utm,col=InDutchPartShipLane))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_sf(data = Shipping_lanes,aes(geometry=geometry))+
    coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
    theme_s()

DistanceShippingLanes <- left_join(DistanceShippingLanes,temp_ship_in,by=c("datetime","x_utm","y_utm")) 

# Check. Also not very helpful...
DistanceShippingLanes %>% 
    ggplot() + 
    geom_sf(data = Shipping_lanes,aes(geometry=geometry))+
    geom_sf(data = Shipping_lanes_marin,aes(geometry=geometry),fill="green")+
    geom_point(aes(x=x_utm,y=y_utm,col=InDutchPartShipLane,fill=InDutchPartShipLane))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
    theme_s()

rm(pts_1,pts_2,pts_3,distance_1,DistanceShippingLanes_1,distance_2,DistanceShippingLanes_2,distance_3,DistanceShippingLanes_3,Shipping_lanes,Shipping_lanes_marin,temp,temp_b,temp_ship_in)













# *mining --> point data ------------------


# Datasets below come from https://emodnet.ec.europa.eu/geoviewer/
# Check under human activities & oil & gas
hydrocarbons <- st_read("Data/Abiotics/hydrocarbonsPoint.shp")
hydrocarbons <- hydrocarbons %>% st_transform(32631)

activelicenses <- st_read("Data/Abiotics/activelicensesPolygon.shp") # shapefiles
activelicenses <- activelicenses %>% st_transform(32631)

platforms <- st_read("Data/Abiotics/platformsPoint.shp")
platforms <- platforms %>% st_transform(32631)


# Enkel NL...
plot_oil_gas <-oil_gas_NL %>% 
  ggplot() + 
  geom_sf(aes(geometry=geometry,col=STATUS))+
  geom_sf(data = Europe_utm31)+
  coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
  theme_s()

ggsave(plot_oil_gas,file="Output/Figures/Covariates/Oil_Gas.png", width = 28, height = 20, units = 'cm')
rm(plot_oil_gas)
rm(oil_gas_NL)

# # EMOD --> Boreholes?
# The dataset on offshore wells for Oil and Gas industry activities in the EU was created in 2014 by Cogea for the European Marine Observation and Data Network (EMODnet). It is the result of the aggregation and harmonization of datasets provided by several EU and non-EU sources. It is updated every year, and is available for viewing and download on the EMODnet web portal (Human Activities, https://emodnet.ec.europa.eu/en/human-activities). It contains points representing offshore wells drilled in the following countries: Croatia, Cyprus, Denmark, Faroe Islands, France, Germany, Greece (only for western coast), Ireland, Italy, Latvia, Malta, Montenegro, Netherlands, Norway, Poland, Portugal, Spain, and United Kingdom. Where available each point has the following attributes: status (active, abandoned, other, suspended, N/A), country, code, name, year (spud date), purpose (exploitation, exploration, other), content (crude oil, natural gas, crude oil and natural gas, natural gas and crude oil, dry, other), operator, drilling company/facility, distance to coast (metres) and water depth (metres). Compared with the previous version this new version has been updated for most of the countries that have published or sent their last release of data.
plot_hydrocarbons <-
  ggplot() + 
  geom_sf(data = hydrocarbons,aes(geometry=geometry,col=year))+
  geom_sf(data = Europe_utm31)+
  coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
  theme_s()
str(hydrocarbons)

ggsave(plot_hydrocarbons,file="Output/Figures/Covariates/hydrocarbons.png", width = 28, height = 20, units = 'cm')
rm(plot_hydrocarbons)

# EMOD --> Offshore installations?
# The dataset on offshore installations for Oil and Gas exploitation and exploration activities was created in 2015 by Cogea for the European Marine Observation and Data Network (EMODnet). It is the result of the aggregation and harmonization of datasets provided by several EU and non-EU sources. It is updated every year, and is available for viewing and download on EMODnet Human Activities web portal (https://emodnet.ec.europa.eu/en/human-activities). It contains points representing offshore installations and where available each point has the following attributes: ID, name, country, location block, operator, production start year, primary production, current status, category and function of the installation, sub-structure and topside weights (tonnes), water depth (metres), distance to coast (metres) and notes. The OSPAR commission source covers data for Germany, Ireland, Spain (Atlantic Sea), while for Italy data have been collected and harmonized from the Italian Ministry of Economic Development, for Denmark from the Danish Energy Agency, for the Netherlands from the TNO - Geological Survey of the Netherlands, for Croatia from the Croatian Hydrocarbon Agency, for Norway from the Norwegian Petroleum Directorate, for the UK from the Oil and Gas Authority (surface infrastructures), for Polish and Russian installations in the Baltic Sea from Marine Traffic and Helcom, finally from Marine Traffic come the data for Bulgarian, Russian and Ukrainian installations in the Black Sea and for Lybian and Spanish installations in the Mediterranean Sea. Compared with the previous version this new version has been updated for all countries.
plot_platforms <-
  ggplot() + 
  geom_sf(data = platforms,aes(geometry=geometry))+
  geom_sf(data = Europe_utm31)+
  coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
  theme_s()

ggsave(plot_platforms,file="Output/Figures/Covariates/platforms.png", width = 28, height = 20, units = 'cm')
rm(plot_platforms)


# Will not use this one.Unclear what it is. 
plot_activelicenses <-
  ggplot() + 
  geom_sf(data = activelicenses,aes(geometry=geometry,fill=validfrom))+
  geom_sf(data = Europe_utm31)+
  coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
  theme_s()

ggsave(plot_activelicenses,file="Output/Figures/Covariates/activelicenses.png", width = 28, height = 20, units = 'cm')
rm(plot_activelicenses)
rm(activelicenses)



str(platforms)
unique(platforms$current_st) # some are removed/decomissioned. Only select operational...? 
#Oil and gas fields generally have a lifespan ranging from 15-30 years, from first oil to abandonment. Production can last 50 years or more for the largest deposits. Deepwater fields, however, are operated just five to ten years due the very high extraction costs.
unique(platforms$production) # = production start year
table(platforms$production,platforms$current_st)
unique(platforms$valid_to)

# set production into datetime.
platforms <- platforms %>% 
  mutate(Start_date_production = parse_date_time(production, '%d/%m/%Y')) %>% 
  mutate(year = format(Start_date_production,format = "%Y"),
         month = format(Start_date_production,format = "%m"),
         year_month_start=paste(year,month,sep="_"))

platforms <- platforms %>% dplyr::select(platformid,current_st,category,function.,primary_pr,remarks,geometry,Start_date_production,year_month_start)



# Add end-date % remove for stations that are still operating/not removed
Removed <- platforms %>% filter(current_st %in% c("Removed","Decommissioned"))
# For some removing date is mentioned, but ignore, cause too much work... Add 20 years
Removed <- Removed %>% 
  mutate(End_date = Start_date_production%m+% years(20))

Not_removed <- platforms %>% filter(current_st %notin% c("Removed","Decommissioned"))
Not_removed <- Not_removed %>% 
  mutate(End_date = "01/01/2023",
         End_date = parse_date_time(End_date, '%d/%m/%Y'))

platforms <- rbind(Removed,Not_removed)
rm(Removed,Not_removed)

platforms <- platforms %>% 
  mutate(year = format(End_date,format = "%Y"),
         month = format(End_date,format = "%m"),
         year_month_end=paste(year,month,sep="_"))
platforms <- platforms %>% 
  dplyr::select(!c(year,month))

str(platforms)

# remove suspended
hydrocarbons <- hydrocarbons %>% filter(status %notin% "Suspended")

# year --> spud date. But I will use it as start date of the hydrocarbon
#The day the main drill bit begins drilling into the ground—a process called spudding in—is referred to as the "spud date." In cases of offshore oil rigs, the spud date occurs when the drill begins working on the seafloor, not when it first breaches the water.

hydrocarbons <- hydrocarbons %>% 
                mutate(year_month_start=paste(year,"01",sep="_"),
                        year_month_end="2023_01")

hydrocarbons <- hydrocarbons %>% dplyr::select(status,hydrocarbo,year_month_start,year_month_end,geometry) %>% 
                  mutate(type = "Boreholes") %>% 
                  dplyr::rename(primary_pr=hydrocarbo)

platforms <- platforms %>% dplyr::select(current_st,primary_pr,year_month_start,year_month_end,geometry) %>% 
                  mutate(type = "Offshore installations")%>% 
                  dplyr::rename(status=current_st)

Installations <- bind_rows(platforms,hydrocarbons)
rm(platforms,hydrocarbons)

str(Installations)
# Not supermany NA_NA
# Remove when "NA_NA"/"NA_01" & status removed
Installations <- Installations %>% 
  filter(!(status %in% c("Decommissioned","Removed") & year_month_start %in% c("NA_NA","NA_01")))

unique(Installations$status)

Installations %>% filter(year_month_start %in% "NA_NA") %>% 
  ggplot() + 
  geom_sf(aes(geometry=geometry))+
  geom_sf(data = Europe_utm31)+
  coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
  theme_s()

check <- Installations %>% filter(year_month_start %in% "NA_NA") # 258 van de 12287
# remove I guess
Installations <- Installations %>% filter(!year_month_start %in% "NA_NA")
range(Installations$year_month_start)
range(Installations$year_month_end)


# crs "EPSG",32631] --> is same as bird_data
crs(Installations)
crs(bird_data_unique)

# Couple to bird_data. Use data both between start and end date. 
# For every year, select the right amount of platforms. 

YM <- bird_data_unique 
YM <- unique(YM$year_month)

#installatie in gebruik op bijvoorbeeld 2008_09, dus year_month_start moet vóór de i zijn. 
#installatie afgebroken op bijvoorbeeld 2013_09, dus year_month_end moet ná de i zijn

df_platforms <- data.frame()

for (i in YM ){
  
  temp <- bird_data_unique  %>% filter(year_month == i)
  temp_platforms <- Installations %>% filter(year_month_start <= i) # Only select the installations with start year before the used year_month 
# And remove the ones that are probably gone again, so the year_month_end should be bigger than the i
  temp_platforms <- temp_platforms %>% filter(year_month_end >= i )
  temp_s <- temp %>% dplyr::select(x_utm,y_utm)
  
  # Selecteer x_utm & y_utm uit geometry
  temp_platforms_s <- temp_platforms %>% 
    mutate(x_utm= unlist(map(.$geometry,1)),    # longitude
          y_utm = unlist(map(.$geometry,2))) %>% 
    st_drop_geometry() %>% 
    dplyr::select(x_utm,y_utm) 
  
  nearest <- nn2(temp_platforms_s,temp_s,k=1)
  nearest_nr <- as.data.frame(nearest[[1]])
  nearest_distance <- as.data.frame(nearest[[2]])
  nearest <- bind_cols(nearest_nr,nearest_distance)
  dim(temp_s);dim(nearest)
  names(nearest) <- c("RowNr","Distance_platform")
  rm(temp_platforms_s,temp_s,nearest_nr,nearest_distance)
  nearest <- nearest %>% dplyr::mutate(Distance_platform_km = Distance_platform/1000) %>% 
    dplyr::select(!Distance_platform)
  
  temp <- bind_cols(temp,nearest)

  temp_platforms_2 <- temp_platforms %>% dplyr::mutate(RowNr = row_number()) %>% 
    dplyr::select(RowNr,type) %>% st_drop_geometry() %>% dplyr::rename(type_platform = type)

  temp <- left_join(temp,temp_platforms_2, by=c("RowNr"))
  #str(temp)
  
  temp_DF <- temp %>% dplyr::select(datetime, x_utm,y_utm,Distance_platform_km,type_platform)
  
  df_platforms <- bind_rows(df_platforms,temp_DF)
  
}

# check

temp_DF %>%  
    ggplot() + 
    geom_point(aes(x=x_utm,y=y_utm,col=(Distance_platform_km)))+
    geom_sf(data = temp_platforms,aes(geometry=geometry),alpha=0.2,col="pink")+  
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
    theme_s()
  
rm(temp,temp_platforms,temp_platforms_2,temp_DF,Installations)


dim(df_platforms)
dim(bird_data_unique)


# *OWF--------------------------

# Not used

# *Fishing activity--------------------------------

# Data of ICES
# https://ices-library.figshare.com/articles/report/OSPAR_request_on_the_production_of_spatial_data_layers_of_fishing_intensity_pressure/18639182
# https://ices-library.figshare.com/articles/dataset/Data_for_OSPAR_request_on_the_production_of_spatial_data_layers_of_fishing_intensity_pressure/18601508


# FP
load("Data/ICES fishing activity/FishingPressure_0,05degreeCsquare_2009_2020.RData")
str(FP)

hist(FP$Hour_cov)
hist(FP$Hour_low)
hist(FP$Hour_upp)

unique(FP$Hour_upp)
unique(FP$Hour_low)
unique(FP$Hour_cov)

str(FP)

# Tamara uses the average between Hour_low and Hour_upp, so I added a column
FP <- FP %>% dplyr::select(lat,lon,Year,Hour_low,Hour_upp,Hour_cov,geometry)
FP %>% filter(is.na(Hour_upp)|is.na(Hour_low))
FP <- FP %>% dplyr::mutate(Year = as.factor(Year)) %>% mutate(Fishing_hour_mean = (Hour_low+Hour_upp)/2)

FP <- st_as_sf(FP,coord="geometry",crs = 4326)
FP   <- FP  %>% st_transform(32631)
hist(FP$Fishing_hour_mean)

Fishing_effort <- FP %>%  
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_sf(aes(geometry=geometry,col=log(Fishing_hour_mean+1)))+
    scale_colour_viridis_c()+
    theme_s()+
    coord_sf(xlim=c(50000, 950000),ylim=c(5600000,6600000))+
    facet_wrap(~Year)
ggsave(Fishing_effort,file="Output/Figures/Covariates/Fishing_effort_OSPAR.png", width = 28, height = 20, units = 'cm')
rm(Fishing_effort)


# Calculate mid-point of geometry
FP <- FP %>% dplyr::mutate(centroid = st_centroid(geometry))

unique(FP$Hour_low)
unique(FP$Hour_upp)
unique(FP$Hour_cov)
unique(FP$Fishing_hour_mean)

FP <- FP %>% 
    mutate(x_utm = unlist(map(.$centroid,1)),
         y_utm = unlist(map(.$centroid,2)))

# Couple points of birds to FP
YM <- unique(FP$Year)
df_FP <- data.frame()

for (i in YM ){

temp <- bird_data_unique %>% filter(year == i)
temp_FP <- FP %>% filter(Year == i) %>% st_drop_geometry() %>% dplyr::rename(year=Year) %>% dplyr::select(year,Fishing_hour_mean,Hour_cov)
coords_FP <- FP %>% filter(Year == i)%>% dplyr::select(x_utm,y_utm) %>% st_drop_geometry
  
temp_s <- temp %>% dplyr::select(x_utm,y_utm)
  
nearest <- nn2(coords_FP,temp_s,k=1)
distance <- as.data.frame(nearest[[2]])
nearest <- as.data.frame(nearest[[1]])
names(nearest) <- "RowNr"
names(distance) <- "Distance_km_closest_fishing_datapoint"
nearest <- cbind(nearest,distance)
rm(coords_FP,temp_s)
  
temp <- bind_cols(temp,nearest)
temp_FP <- temp_FP %>% dplyr::mutate(RowNr = row_number())
  
temp <- left_join(temp,temp_FP, by=c("RowNr","year")) %>% dplyr::mutate(Distance_km_closest_fishing_datapoint=Distance_km_closest_fishing_datapoint/1000)

temp_DF <- temp %>% dplyr::select(datetime, x_utm,y_utm,Fishing_hour_mean,Hour_cov,Distance_km_closest_fishing_datapoint,year)
  
df_FP <- bind_rows(df_FP,temp_DF)
  

}

temp_DF %>%  
    #dplyr::filter(is.na(Hour_upp)) %>% 
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x_utm,y=y_utm,col=log(Fishing_hour_mean+1)))+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))+
    facet_wrap(~year)

df_FP  %>%  
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x_utm,y=y_utm,col=log(Fishing_hour_mean+1)))+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))+
    facet_wrap(~year)


# Also add column with average via raster
# First make raster and then couple points of raster to FP
temp_FP <- FP 
temp <- temp_FP %>%  dplyr::select(x_utm,y_utm)%>% st_drop_geometry()
temp_FP <- temp_FP %>% dplyr::select(Fishing_hour_mean,Hour_cov) %>% st_drop_geometry()

load("Data/JT_grid.Rdata") 
grid_temp <- info_grid %>% dplyr::select(x,y) 
info_grid <- info_grid %>% dplyr::select(x,y) %>% dplyr::mutate(RowNr = row_number()) 

nearest <- nn2(grid_temp,temp,k=1)
nearest_nr <- as.data.frame(nearest[[1]])
nearest_distance <- as.data.frame(nearest[[2]])
nearest <- bind_cols(nearest_nr,nearest_distance)

nearest <- bind_cols(nearest,temp_FP)
dim(temp_FP);dim(grid_temp);dim(nearest)

# temp_FP is the dataset I want to keep
names(nearest) <- c("RowNr","Distance","Fishing_hour_mean","Hour_cov")
FP_grid <- left_join(nearest,info_grid, by=c("RowNr"))
str(FP_grid)
rm(temp,grid_temp)
FP_grid <- FP_grid %>% mutate(Distance_km = Distance/1000) %>% 
    dplyr::select(!Distance) 

# Change varianles with large distance in NA
FP_grid <- FP_grid %>% dplyr::mutate(Fishing_hour_average_2009_2020 = ifelse(Distance_km > 15, 0,Fishing_hour_mean))
FP_grid <- FP_grid %>% dplyr::select(Fishing_hour_average_2009_2020,x,y)
# Calculate average per x and y
FP_grid <- FP_grid %>% group_by(x,y) %>% dplyr::summarize(Fishing_hour_average_2009_2020 = mean(Fishing_hour_average_2009_2020,na.rm=T))

FP_grid %>% 
  ggplot()+
  geom_point(aes(x=x,y=y,col=log(Fishing_hour_average_2009_2020+1)))+
  geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
  theme_s()+
  coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))
FP_grid <- FP_grid %>% ungroup()

# MAKE PLOT
Fishing_effort_mean <- FP_grid %>%  
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x,y=y,col=log(Fishing_hour_average_2009_2020+1)))+
    scale_colour_viridis_c()+
    theme_s()+
    coord_sf(xlim=c(50000, 950000),ylim=c(5600000,6600000))

ggsave(Fishing_effort_mean,file="Output/Figures/Covariates/Fishing_effort_OSPAR_mean_over_2009_2020.png", width = 28, height = 20, units = 'cm')
rm(Fishing_effort)




str(FP_grid)


# Couple average to bird-points
temp <- bird_data_unique 
temp_FP <- FP_grid %>% dplyr::select(Fishing_hour_average_2009_2020)
coords_FP <- FP_grid %>% dplyr::select(x,y) 
temp_s <- temp %>% dplyr::select(x_utm,y_utm)
  
nearest <- nn2(coords_FP,temp_s,k=1)
distance <- as.data.frame(nearest[[2]])
nearest <- as.data.frame(nearest[[1]])
names(nearest) <- "RowNr"
names(distance) <- "Distance_km_closest_fishing_datapoint"
nearest <- cbind(nearest,distance)
rm(coords_FP,temp_s)
  
temp <- bind_cols(temp,nearest)
temp_FP <- temp_FP %>% dplyr::mutate(RowNr = row_number())
  
temp <- left_join(temp,temp_FP, by=c("RowNr")) %>% 
  dplyr::mutate(Distance_km_closest_fishing_datapoint=Distance_km_closest_fishing_datapoint/1000)

temp_DF_mean_fish <- temp %>% dplyr::select(datetime, year,x_utm,y_utm,Fishing_hour_average_2009_2020)
  

temp_DF_mean_fish %>%  
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x_utm,y=y_utm,col=log(Fishing_hour_average_2009_2020+1)))+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))+
    facet_wrap(~year)

rm(FP_grid,temp,nearest)
rm(FP,temp_DF,temp,nearest)

str(temp_DF_mean_fish)
unique(temp_DF_mean_fish$year)
str(df_FP)
unique(df_FP$year)
df_FP %>% filter(is.na(Fishing_hour_mean))

df_FP <- left_join(temp_DF_mean_fish,df_FP)
str(df_FP)



# 5. Couple all data ------------------------

str(bird_data_species)
str(df_depth_sediment)
df_SST <- df_SST %>% distinct()

dim(bird_data_species)
range(df_depth_sediment$datetime)
df_depth_sediment <- df_depth_sediment %>% distinct()
bird_data_covariates <- left_join(bird_data_species,df_depth_sediment,by=c("datetime","x_utm","y_utm"))
dim(bird_data_covariates)

bird_data_covariates <- left_join(bird_data_covariates,df_SST,by=c("datetime","x_utm","y_utm"))
dim(bird_data_covariates)

bird_data_covariates <- left_join(bird_data_covariates,df_OWF_TRUE_turn,by=c("datetime","x_utm","y_utm"))
dim(bird_data_covariates)

df_Chl_a <- df_Chl_a %>% distinct()
bird_data_covariates <- left_join(bird_data_covariates,df_Chl_a,by=c("datetime","x_utm","y_utm"))
dim(bird_data_covariates)

DistanceShippingLanes <- DistanceShippingLanes %>% distinct()
bird_data_covariates <- left_join(bird_data_covariates,DistanceShippingLanes,by=c("datetime","x_utm","y_utm"))
dim(bird_data_covariates)

df_platforms <- df_platforms %>% distinct()
bird_data_covariates <- left_join(bird_data_covariates,df_platforms,by=c("datetime","x_utm","y_utm"))
dim(bird_data_covariates)

# Couple also on species here!!
str(Breeding_sites_km)
Breeding_sites_km <- Breeding_sites_km %>% distinct() 
bird_data_covariates <- left_join(bird_data_covariates,Breeding_sites_km,by=c("english_name","datetime","x_utm","y_utm"))
dim(bird_data_covariates)

df_waggit <- df_waggit %>% distinct()
bird_data_covariates <- left_join(bird_data_covariates,df_waggit,by=c("datetime","x_utm","y_utm"))
dim(bird_data_covariates)

str(df_FP)
df_FP <- df_FP %>% distinct() %>% dplyr::select(!year)
bird_data_covariates <- left_join(bird_data_covariates,df_FP,by=c("datetime","x_utm","y_utm"))
dim(bird_data_covariates)

write_rds(bird_data_covariates,"Input_data/Bird_data with covariates/data_birds_covariates_24_10_2023.rds")

str(bird_data_covariates)
unique(bird_data_covariates$euring_species_code)

check <- bird_data_covariates %>% filter(is.na(euring_species_code))
check2 <- bird_data_species %>% filter(is.na(euring_species_code))
rm(check,check2)




# 6. Correct species_counted & count_method-------------------

library(readxl)
count_species_method <- read_excel("Data/ESAS_species_check.xlsx")

count_species_method <- count_species_method %>% dplyr::select(euring_species_code,count_method,`species counted`)

countmethod <- count_species_method %>% dplyr::select(euring_species_code,count_method)
countmethod <- countmethod %>% separate(col = count_method, into=toupper(letters[1:7]))
countmethod <- as.data.frame(sapply(countmethod, as.numeric ))
countmethod <- countmethod %>% pivot_longer(cols = c("A","B","C","D","E","F","G")) %>% dplyr::select(euring_species_code,value)%>% drop_na() %>% dplyr::rename(count_method=value) 

spec_count <- count_species_method %>% dplyr::select(euring_species_code,`species counted`)
spec_count  <- spec_count  %>% separate(col = `species counted`, into=toupper(letters[1:12]))
spec_count  <- as.data.frame(sapply(spec_count , as.numeric ))
spec_count  <- spec_count  %>% pivot_longer(cols = c("A","B","C","D","E","F","G","H","I","J","K","L")) %>% dplyr::select(euring_species_code,value)%>% drop_na() %>% dplyr::rename(species_counted=value)


# Couple dataset to corrected dataset, on euring.
# Select right species per count method
str(bird_data_covariates)
str(countmethod)

Northsea_corrected_method <- bird_data_covariates %>% inner_join(countmethod, by=c("count_method","euring_species_code"))
dim(bird_data_covariates)
dim(Northsea_corrected_method) 

table(Northsea_corrected_method$count_method,Northsea_corrected_method$euring_species_code)


# select right species per species counted
Northsea_corrected_methodspecies <- Northsea_corrected_method %>% inner_join(spec_count,by=c("species_counted","euring_species_code"))
dim(Northsea_corrected_methodspecies)

# check
table(Northsea_corrected_method$species_counted,Northsea_corrected_method$euring_species_code)
table(Northsea_corrected_methodspecies$species_counted,Northsea_corrected_methodspecies$euring_species_code)


rm(countmethod,data_northsea5,effc_strip_width,spec_count,Northsea_corrected,Northsea_corrected2,Northsea_corrected_method,Northsea_corrected_zero,Northsea_corrected_zero_2)
rm(count_species_method)



write_rds(Northsea_corrected_methodspecies,"Input_data/Bird_data with covariates/data_seabirds_covariates_24_10_2023_corrected.rds")






# Making a grid for the prediction maps of 10*10km

############# GRID 10*10km----------------------------------------

# Loading all the covariates again and put them together in a grid. 
rm(list= ls())

# For plotting

library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library (RSQLite)
library(raster)
library(ggmap)
library(tidyverse)
library(sf)
library(RANN)

# map
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
Europe_utm31 <- Europe %>% st_transform(32631)
plot(Europe_utm31$geometry)

Europe_latlong <- Europe %>% st_transform(4326)

# Functies 
`%notin%` <- function(x,y) !(x %in% y)

theme_s <-  function () { 
  theme_classic(base_size=12)%+replace% 
    theme(axis.text = element_text(color="grey"),
          axis.line = element_line(color="grey"),
          axis.ticks = element_line(color="grey"),
          legend.title = element_text(face = "bold"),
          strip.background = element_rect(fill = "white", colour = "grey"),
          strip.text = element_text(margin = margin(0.2,0,0.2,0, "cm"))# face = "bold", 
    )
}


#info_grid 5*5 km
load("Data/JT_grid.Rdata") 

raster_utm31 <- rasterFromXYZ(info_grid,crs = 32631)
raster_latlong  <- projectRaster(raster_utm31,crs = 4326)

# Make raster 10*10 instead of 5*5
res(raster_utm31)
raster_utm31_10_10km <- aggregate(raster_utm31, fact=2)
res(raster_utm31_10_10km)

res(raster_latlong)
raster_latlong_10_10km <- aggregate(raster_latlong, fact=2)
res(raster_latlong_10_10km)

# dataset with x en y coordinates
info_grid_10_10km <- cbind(xyFromCell(raster_utm31_10_10km, 1:ncell(raster_utm31_10_10km)), values(raster_utm31_10_10km))
info_grid_10_10km <- as.data.frame(info_grid_10_10km)
info_grid_10_10km <- info_grid_10_10km %>% dplyr::select(c(x,y))
  
info_grid_latlong_10_10km <- cbind(xyFromCell(raster_latlong_10_10km, 1:ncell(raster_latlong_10_10km)), values(raster_latlong_10_10km))
info_grid_latlong_10_10km <- as.data.frame(info_grid_latlong_10_10km)
info_grid_latlong_10_10km <- info_grid_latlong_10_10km %>% dplyr::select(c(x,y))


# Add GRID_ID
info_grid_10_10km <- info_grid_10_10km %>% mutate(GRID_ID = 1:n())
info_grid_latlong_10_10km <- info_grid_latlong_10_10km %>% mutate(GRID_ID = 1:n())


rm(info_grid,info_grid_latlong,raster_latlong,raster_utm31)

# Depth & Sediment------------------
# Source: Working Group on Spatial Fisheries Data (outputs from 2021 meeting)
# https://www.ices.dk/community/groups/pages/wgsfd.aspx 

DepthSediment <-  readRDS("Data/Abiotics/BirdMapHabitat.rds")
DepthSediment <- as.data.frame(DepthSediment) 
str(DepthSediment)

# change to utm31
DepthSediment <- st_as_sf(DepthSediment, coords = c("SI_LONG","SI_LATI"), 
                     crs = 4326, agr = "constant")
DepthSediment  <- DepthSediment  %>% st_transform(32631)
DepthSediment  <- DepthSediment  %>% 
  mutate(x = unlist(map(.$geometry,1)),
         y = unlist(map(.$geometry,2))) %>%  st_drop_geometry() 


# Combine with closest point and distance
Depth_temp <-  DepthSediment %>% 
    dplyr::select(x,y)
grid_temp <- info_grid_10_10km %>% dplyr::select(x,y)
temp <- DepthSediment %>% dplyr::select(MudPercent,SandPercent,depth,distance_coast_avg) %>% 
    dplyr::mutate(RowNr = row_number()) 


nearest <- nn2(Depth_temp,grid_temp,k=1)
nearest_nr <- as.data.frame(nearest[[1]])
nearest_distance <- as.data.frame(nearest[[2]])
nearest <- bind_cols(nearest_nr,nearest_distance)
nearest <- bind_cols(nearest,grid_temp)
dim(Depth_temp);dim(grid_temp);dim(nearest)
names(nearest) <- c("RowNr","Distance","x","y")
str(nearest)
rm(Depth_temp,grid_temp)

nearest <- nearest %>% mutate(Distance_km = Distance/1000) %>% 
    dplyr::select(!Distance)
str(nearest)  

DepthSediment_grid <- left_join(nearest,temp, by=c("RowNr"))
str(DepthSediment_grid)

DepthSediment_grid %>%  
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x,y=y,col=distance_coast_avg))+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))
rm(temp,nearest)

hist(DepthSediment_grid$Distance_km)

# Change values with large distance in NA
DepthSediment_grid <- DepthSediment_grid %>% dplyr::mutate(MudPercent = ifelse(Distance_km > 25, NA,MudPercent),
                                                           SandPercent = ifelse(Distance_km > 25, NA,SandPercent),
                                                           depth = ifelse(Distance_km > 25, NA,depth),
                                                           distance_coast_avg = ifelse(Distance_km > 50, NA,distance_coast_avg))

DepthSediment_grid <- DepthSediment_grid %>% dplyr::select(!c(Distance_km,RowNr))


# Sea surface temperature --------------------------
#Source: Marine Copernicus
#Product ID: SST_ATL_SST_L4_REP_OBSERVATIONS_010_026
#DOI: 10.48670/moi-00153

SST <- read.csv("Data/Abiotics/SST_1982-2020_BirdDensityMaps_MonthlyMean.csv")
SST <- SST %>% filter(Year > 1990)
str(SST)
unique(SST$Year);range(SST$Year)

SST <- SST %>% dplyr::mutate(Month = as.factor(Month),
                     Month = ifelse(Month %in% c("1","2","3","4","5","6","7","8","9"),paste0(0,Month),Month),
                     Year = as.factor(Year))
SST <- SST %>% dplyr::mutate(year_month = paste(Year,Month,sep="_")) %>%
                     dplyr::select(Long,Lat,Analysed_meanSST_Celsius,year_month,Year,Month)

# change to utm31
SST <- st_as_sf(SST, coords = c("Long","Lat"), 
                     crs = 4326, agr = "constant")
SST  <- SST  %>% st_transform(32631)
SST  <- SST  %>% 
  mutate(x = unlist(map(.$geometry,1)),
         y = unlist(map(.$geometry,2))) %>%  st_drop_geometry() 

# For every year and month, make the raster
range(SST$year_month)
YM <- unique(SST$year_month)

df_SST_grid <- data.frame()

for (i in YM ){
  
  temp_SST <- SST %>% filter(year_month == i)%>% 
    dplyr::mutate(RowNr = row_number()) 
  temp <- temp_SST %>% dplyr::select(x,y)
  temp_SST <- temp_SST %>% dplyr::select(RowNr,year_month,Year,Month,Analysed_meanSST_Celsius)
  
  grid_temp <- info_grid_10_10km %>% dplyr::select(x,y)
  nearest <- nn2(temp,grid_temp,k=1)
  nearest_nr <- as.data.frame(nearest[[1]])
  nearest_distance <- as.data.frame(nearest[[2]])
  nearest <- bind_cols(nearest_nr,nearest_distance)
  nearest <- bind_cols(nearest,grid_temp)
  dim(temp_SST);dim(grid_temp);dim(nearest)
  
  names(nearest) <- c("RowNr","Distance","x","y")
  rm(temp,grid_temp)
  nearest <- nearest %>% mutate(Distance_km = Distance/1000) %>% 
    dplyr::select(!Distance) 

  SST_grid <- left_join(nearest,temp_SST, by=c("RowNr"))
  
  # Change large distance in NA
  SST_grid <- SST_grid %>% dplyr::mutate(Analysed_meanSST_Celsius = ifelse(Distance_km > 25, NA,Analysed_meanSST_Celsius))
  SST_grid <- SST_grid %>% dplyr::select(!c(Distance_km,RowNr))
 
  df_SST_grid <- bind_rows(df_SST_grid,SST_grid)
  
}

SST_grid %>%  
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x,y=y,col=Analysed_meanSST_Celsius))+
    #geom_point(data=info_grid,aes(x=x,y=y),col="purple")+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))+
    facet_wrap(~year_month)

rm(SST_grid,nearest,temp_SST,temp_grid,nearest_distance,nearest_nr)





# *Chl-a --> Copernicus --------------
# SOURCE:  Marine Copernicus 
# Range: January 1993 to June 2022

Chl_a <- readRDS("Data/Abiotics/Chlorophyll_1993-2022_BirdDensityMaps_MonthlyMean.rds")
str(Chl_a)
Chl_a <- Chl_a %>% ungroup()
unique(Chl_a$Year);range(Chl_a$Year)

Chl_a <- Chl_a %>% dplyr::mutate(Month = as.factor(Month),
                     Month = ifelse(Month %in% c("1","2","3","4","5","6","7","8","9"),paste0(0,Month),Month),
                     Year = as.factor(Year))
Chl_a <- Chl_a %>% dplyr::mutate(year_month = paste(Year,Month,sep="_")) %>%
                     dplyr::select(Long,Lat,meanChlorophyll,year_month,Year,Month) 

# change to utm31
Chl_a <- st_as_sf(Chl_a, coords = c("Long","Lat"), 
                     crs = 4326, agr = "constant")
Chl_a  <- Chl_a %>% st_transform(32631)
Chl_a  <- Chl_a  %>% 
  mutate(x = unlist(map(.$geometry,1)),
         y = unlist(map(.$geometry,2))) %>%  st_drop_geometry() 




YM <- unique(Chl_a$year_month)
df_Chl_a_grid <- data.frame()

for (i in YM ){
  
  temp_Chl_a <- Chl_a %>% filter(year_month == i)%>% 
    dplyr::mutate(RowNr = row_number()) 
  temp <- temp_Chl_a %>% dplyr::select(x,y)
  temp_Chl_a <- temp_Chl_a %>% dplyr::select(RowNr,year_month,Year,Month,meanChlorophyll)
  
  grid_temp <- info_grid_10_10km %>% dplyr::select(x,y)
  nearest <- nn2(temp,grid_temp,k=1)
  nearest_nr <- as.data.frame(nearest[[1]])
  nearest_distance <- as.data.frame(nearest[[2]])
  nearest <- bind_cols(nearest_nr,nearest_distance)
  nearest <- bind_cols(nearest,grid_temp)
  dim(temp_Chl_a);dim(grid_temp);dim(nearest)
  
  names(nearest) <- c("RowNr","Distance","x","y")
  rm(temp,grid_temp)
  nearest <- nearest %>% mutate(Distance_km = Distance/1000) %>% 
    dplyr::select(!Distance) 

  Chl_a_grid <- left_join(nearest,temp_Chl_a, by=c("RowNr"))
  
  # Change large distance in NA
  Chl_a_grid <- Chl_a_grid %>% dplyr::mutate(meanChlorophyll = ifelse(Distance_km > 25, NA,meanChlorophyll))
  Chl_a_grid <- Chl_a_grid %>% dplyr::select(!c(Distance_km,RowNr))
  
  df_Chl_a_grid <- bind_rows(df_Chl_a_grid,Chl_a_grid)
  
}

Chl_a_grid %>%  
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x,y=y,col=meanChlorophyll))+
    #geom_point(data=info_grid,aes(x=x,y=y),col="purple")+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))+
    facet_wrap(~year_month)

rm(Chl_a,nearest,Chl_a_grid)


# Should be the same length:
9373 * length(YM)
dim(df_Chl_a_grid)









# * Breeding sites ------------------- 

# EBBA2 dataset. Only confirmed. Is on a 50*50km grid and on the 2013-2017 period
EBBA2_breeding <- read.csv("Data/Breeding sites/ebba2_data_occurrence_50km_2013_2017.csv",sep=";",dec = ",")
EBBA2_abundance <- read.csv("Data/Breeding sites/ebba2_data_abundance_50km_2013_2017.csv",sep=";",dec = ",")
str(EBBA2_breeding)
str(EBBA2_abundance)

# combine
EBBA2_breeding <- full_join(EBBA2_breeding,EBBA2_abundance)
rm(EBBA2_abundance)

# Add...
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

# couple to grid 50-km Universal Transversal Mercator (UTM) grid from EBBA2 website
EBBA2_grid <- st_read("Data/Breeding sites/ebba2_grid50x50_v1.shp")
# Change projection
EBBA2_grid <- EBBA2_grid %>% st_transform(32631)
EBBA2_breeding <- left_join(EBBA2_breeding,EBBA2_grid,by="cell50x50")

EBBA2_breeding %>%  
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_sf(aes(geometry=geometry,fill=Nr_breeding_pairs_closest_colony))+  
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))+
    theme_s()+
    facet_wrap(~birdlife_scientific_name)

EBBA2_breeding <- EBBA2_breeding %>% dplyr::rename(scientific_name = birdlife_scientific_name)
EBBA2_breeding <- EBBA2_breeding %>% dplyr::mutate(scientific_name = ifelse(scientific_name=="Catharacta skua","Stercorarius skua",scientific_name))


# Add centroid of squares to calculate distance to nest
EBBA2_breeding <- EBBA2_breeding %>% dplyr::mutate(centroid = st_centroid(geometry))
EBBA2_breeding <- EBBA2_breeding %>% 
    mutate(x_utm= unlist(map(.$centroid,1)),
          y_utm = unlist(map(.$centroid,2))) 

Species <- unique(EBBA2_breeding$scientific_name)
Breeding_sites_grid <- data.frame()

for(i in Species){ 

  df_breeding_temp <-  EBBA2_breeding %>% 
    dplyr::select(scientific_name,x_utm,y_utm,Nr_breeding_pairs_closest_colony) %>% 
    filter(scientific_name == i)%>% 
    dplyr::mutate(RowNr = row_number()) 
  
  temp <- df_breeding_temp %>% dplyr::select(x_utm,y_utm)
  grid_temp <- info_grid_10_10km %>% dplyr::select(x,y)
  df_breeding_temp <-  df_breeding_temp %>% 
    dplyr::select(scientific_name,Nr_breeding_pairs_closest_colony,RowNr) 
  
  nearest <- nn2(temp,grid_temp,k=1)
  nearest_nr <- as.data.frame(nearest[[1]])
  nearest_distance <- as.data.frame(nearest[[2]])
  nearest <- bind_cols(nearest_nr,nearest_distance)
  nearest <- bind_cols(nearest,grid_temp)
  dim(grid_temp);dim(nearest)
  
  names(nearest) <- c("RowNr","Distance","x","y")
  rm(temp,grid_temp)
  nearest <- nearest %>% mutate(Distance_breeding_site_km = Distance/1000) %>% 
    dplyr::select(!Distance) 

  Breeding_grid <- left_join(nearest,df_breeding_temp, by=c("RowNr"))
  
  Breeding_grid <- Breeding_grid %>% dplyr::select(!c(RowNr))
   # Because I used the middle point to calculate distance from breeding site, set all values under 25 on 25 km.
  Breeding_grid <- Breeding_grid %>% 
    dplyr::mutate(Distance_breeding_site_km = ifelse(Distance_breeding_site_km<=25,25,Distance_breeding_site_km))

  Breeding_sites_grid <- bind_rows(Breeding_sites_grid,Breeding_grid)

}


str(Breeding_sites_grid)

# Check
Breeding_sites_grid %>%  
    ggplot() + 
    geom_point(aes(x=x,y=y,col=Nr_breeding_pairs_closest_colony))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+   
    geom_sf(data=EBBA2_breeding,aes(geometry=geometry),fill="purple")+
    coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
    theme_s()+
    facet_wrap(~scientific_name)

Breeding_sites_grid %>% filter(is.na(Distance_breeding_site_km))



# Also add a line distance to larger colonies (> 1000). Exclude Gavia stellata here. 
# Use for Stercorarius skua & Larus marinus also colonies of >= 100

EBBA2_breeding_BIG <- EBBA2_breeding %>% filter(scientific_name %notin% c("Gavia stellata"))

EBBA2_breeding_BIG <- EBBA2_breeding_BIG %>% 
  mutate(Nr_breeding_pairs_select = case_when(
    scientific_name %in% c("Larus marinus","Stercorarius skua","Thalasseus sandvicensis") & Nr_breeding_pairs_closest_colony %in% c("100-999","1000-9999",'10000-99999') ~ "yea",
    scientific_name %in% c("Fulmarus glacialis","Morus bassanus","Rissa tridactyla", "Larus fuscus","Larus argentatus","Fratercula arctica","Alca torda","Uria aalge") & Nr_breeding_pairs_closest_colony %in% c("1000-9999",'10000-99999') ~ "yea"))
table(EBBA2_breeding_BIG$Nr_breeding_pairs_select,EBBA2_breeding_BIG$Nr_breeding_pairs_closest_colony)
EBBA2_breeding_BIG <- EBBA2_breeding_BIG %>% filter(Nr_breeding_pairs_select %in% "yea")

Species <- unique(EBBA2_breeding_BIG$scientific_name)
Breeding_sites_grid2 <- data.frame()

EBBA2_breeding_BIG <- EBBA2_breeding_BIG %>% dplyr::rename(Nr_breeding_pairs_biggest_colony = Nr_breeding_pairs_closest_colony)

for(i in Species){ 

  df_breeding_temp <-  EBBA2_breeding_BIG %>% 
    dplyr::select(scientific_name,x_utm,y_utm,Nr_breeding_pairs_biggest_colony) %>% 
    filter(scientific_name == i)%>% 
    dplyr::mutate(RowNr = row_number()) 
  
  temp <- df_breeding_temp %>% dplyr::select(x_utm,y_utm)
  grid_temp <- info_grid_10_10km %>% dplyr::select(x,y)
  df_breeding_temp <-  df_breeding_temp %>% 
    dplyr::select(scientific_name,Nr_breeding_pairs_biggest_colony,RowNr) 
  
  nearest <- nn2(temp,grid_temp,k=1)
  nearest_nr <- as.data.frame(nearest[[1]])
  nearest_distance <- as.data.frame(nearest[[2]])
  nearest <- bind_cols(nearest_nr,nearest_distance)
  nearest <- bind_cols(nearest,grid_temp)
  dim(grid_temp);dim(nearest)
  
  names(nearest) <- c("RowNr","Distance","x","y")
  rm(temp,grid_temp)
  nearest <- nearest %>% mutate(Distance_big_breeding_site_km = Distance/1000) %>% 
    dplyr::select(!Distance) 

  Breeding_grid2 <- left_join(nearest,df_breeding_temp, by=c("RowNr"))
  
  Breeding_grid2 <- Breeding_grid2 %>% dplyr::select(!c(RowNr))
   # Because I used the middle point to calculate distance from breeding site, set all values under 25 on 25 km.
  Breeding_grid2 <- Breeding_grid2 %>% 
    dplyr::mutate(Distance_big_breeding_site_km = ifelse(Distance_big_breeding_site_km<=25,25,Distance_big_breeding_site_km))

  Breeding_sites_grid2 <- bind_rows(Breeding_sites_grid2,Breeding_grid2)

}


str(Breeding_sites_grid2)

# Check
Breeding_sites_grid2 %>%  
    ggplot() + 
    geom_point(aes(x=x,y=y,col=Nr_breeding_pairs_biggest_colony))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+   
    geom_sf(data=EBBA2_breeding_BIG,aes(geometry=geometry),fill="purple")+
    coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
    theme_s()+
    facet_wrap(~scientific_name)

Breeding_sites_grid2 %>% filter(is.na(Distance_big_breeding_site_km))


dim(Breeding_sites_grid)
Breeding_sites_grid <- left_join(Breeding_sites_grid,Breeding_sites_grid2,by=c("x","y",'scientific_name'))
dim(Breeding_sites_grid)

str(Breeding_sites_grid)

rm(EBBA2_breeding,EBBA2_grid,nearest,nearest_distance,nearest_nr,Breeding_grid)




# HUMAN ACTIVITY------------------------------



# Shipping lanes ---------------------------------
# SOURCE: RWS 2017

Shipping_lanes <- st_read("Data/ShippingLanes/ShippingLanes_Dutch_EEZ.shp")

unique(Shipping_lanes$entiteit)

Shipping_lanes %>%  
    ggplot() + 
    geom_sf(aes(geometry=geometry,col=entiteit))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+   
    #geom_point(data = EBBA2Breeding,aes(x=x_utm,y=y_utm),alpha=1,col="pink")+  
    coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
    theme_s()

Shipping_lanes <- Shipping_lanes %>% filter(entiteit %in% c("begrenzing","junction","clearway","draaiplaats"))

Shipping_lanes_marin <- st_read("Data/Abiotics/Shipping lanes marin/clearways_scheepvaart.shp")

# change crs
Shipping_lanes <- Shipping_lanes %>% st_transform(32631)
Shipping_lanes_marin <- Shipping_lanes_marin %>% st_transform(32631)

# Points close to shipping lanes and IN shipping lanes. Combination of the two files.
pts <- info_grid_10_10km 
pts <- st_as_sf(pts, coords = c("x","y"), 
                     crs = 32631, agr = "constant")
# Step that takes very long (hours)
distance <- nngeo::st_nn(pts, Shipping_lanes, k = 1, returnDist = T)

kleave <- distance
distance <- kleave
distance <- as.data.frame(distance[[2]])
dim(distance)

# Change colomns
distance <- distance %>% 
   tidyr::pivot_longer(cols = 1:9373)
distance <- distance %>% dplyr::select(value) %>% dplyr::rename(DistanceShippingLanesInKm = value)
distance <- distance %>% dplyr::mutate(DistanceShippingLanesInKm = DistanceShippingLanesInKm/1000) 

DistanceShippingLanes <- cbind(pts,distance)
str(DistanceShippingLanes)

DistanceShippingLanes <- DistanceShippingLanes %>% 
    mutate(x= unlist(map(.$geometry,1)),
            y = unlist(map(.$geometry,2))) %>% 
  st_drop_geometry()

# Check 
DistanceShippingLanes %>% 
    ggplot() + 
    geom_point(aes(x=x,y=y,col=log(DistanceShippingLanesInKm+1)))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_sf(data = Shipping_lanes,aes(geometry=geometry))+
    coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
    theme_s()


str(DistanceShippingLanes)

DistanceShippingLanes <- DistanceShippingLanes %>% 
                          dplyr::select(x,y,DistanceShippingLanesInKm) 


# Add points IN shipping lane in Dutch part
Shipping_lanes_marin <- Shipping_lanes_marin %>% st_transform(32631)
  
temp <- st_as_sf(info_grid_10_10km, coords = c("x","y"), 
                     crs = 32631, agr = "constant") %>% 
    mutate(x = unlist(map(.$geometry,1)),
         y = unlist(map(.$geometry,2))) %>% 
    dplyr::select(geometry,x,y)
 
temp_ship_grid <- sf::st_join(temp,Shipping_lanes_marin)
temp_ship_grid <- temp_ship_grid %>% 
        dplyr::filter(! is.na(OBJECTID)) %>% 
        st_drop_geometry() %>% 
        dplyr::mutate(Year = i,
                  InDutchPartShipLane = "Yes")
temp_ship_grid <- temp_ship_grid %>% dplyr::select(x,y,InDutchPartShipLane)

DistanceShippingLanes <- left_join(DistanceShippingLanes,temp_ship_grid,by=c("x","y")) 

# Check. 
DistanceShippingLanes %>% 
    ggplot() + 
    geom_sf(data = Shipping_lanes,aes(geometry=geometry))+
    geom_sf(data = Shipping_lanes_marin,aes(geometry=geometry),fill="green")+
    geom_point(aes(x=x,y=y,col=InDutchPartShipLane,fill=InDutchPartShipLane))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    coord_sf(xlim=c(100000, 900000),ylim=c(5500000,7000000))+
    theme_s()


rm(pts,distance,Shipping_lanes)


str(DistanceShippingLanes)










# MINING---------------------------------



library(lubridate)
# Datasets below come from https://emodnet.ec.europa.eu/geoviewer/
# Check under human activities & oil & gas
hydrocarbons <- st_read("Data/Abiotics/hydrocarbonsPoint.shp")
hydrocarbons <- hydrocarbons %>% st_transform(32631)

platforms <- st_read("Data/Abiotics/platformsPoint.shp")
platforms <- platforms %>% st_transform(32631)

# set production into datetime.
platforms <- platforms %>% 
  mutate(Start_date_production = lubridate::parse_date_time(production, '%d/%m/%Y')) %>% 
  mutate(year = format(Start_date_production,format = "%Y"),
         month = format(Start_date_production,format = "%m"),
         year_month_start=paste(year,month,sep="_"))

platforms <- platforms %>% dplyr::select(platformid,current_st,category,function.,primary_pr,remarks,geometry,Start_date_production,year_month_start)

# Add end-date % remove for stations that are still operating/not removed
Removed <- platforms %>% filter(current_st %in% c("Removed","Decommissioned"))
# For some removing date is mentioned, but ignore, cause too much work... Add 20 years
Removed <- Removed %>% 
  mutate(End_date = Start_date_production %m+% years(20))

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

# Not supermany NA_NA
# Remove when "NA_NA"/"NA_01" & status removed
Installations <- Installations %>% 
  filter(!(status %in% c("Decommissioned","Removed") & year_month_start %in% c("NA_NA","NA_01")))

Installations <- Installations %>% filter(!year_month_start %in% "NA_NA")


# Couple to grid.
str(Installations) 

# Make a dataset that had per year the oil and gas stations.
# In 1991 we have the gas stations that are built before 1991 and removed after this year
YY <- c("1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022")
YY <- paste(YY, "01", sep = "_")
df_platforms_grid <- data.frame()

for (i in YY){
  
  temp <- Installations %>% filter(year_month_start<i & year_month_end>= i)
  temp <- temp %>% 
          mutate(x_utm= unlist(map(.$geometry,1)),
                 y_utm = unlist(map(.$geometry,2))) %>% 
          st_drop_geometry() %>% 
          dplyr::select(x_utm,y_utm)
  
  grid_temp <- info_grid_10_10km %>% dplyr::select(x,y)
  nearest <- nn2(temp,grid_temp,k=1)
  nearest_nr <- as.data.frame(nearest[[1]])
  nearest_distance <- as.data.frame(nearest[[2]])
  nearest <- bind_cols(nearest_nr,nearest_distance)
  nearest <- bind_cols(nearest,grid_temp)
  dim(temp);dim(grid_temp);dim(nearest)
  
  names(nearest) <- c("RowNr","Distance","x","y")
  rm(temp,grid_temp)
  
  nearest <- nearest %>% 
    dplyr::mutate(Distance_to_platform_km = Distance/1000) %>% 
    dplyr::select(!Distance) %>% 
    dplyr::mutate(Year = substr(i, 1, 4))

  platform_grid <- nearest %>% dplyr::select(!c(RowNr))
 
  df_platforms_grid <- bind_rows(df_platforms_grid,platform_grid)
  
}



df_platforms_grid %>%  
  filter(Year %in% c("1991","2009","2022")) %>% 
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x,y=y,col=Distance_to_platform_km))+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))+
    facet_wrap(~Year)

rm(platform_grid,nearest,temp_grid,nearest_distance,nearest_nr,Installations)


9373 * length(YY)
dim(df_platforms_grid)





# Fishing activity--------------------------
# SOURCE: ICES
# https://ices-library.figshare.com/articles/report/OSPAR_request_on_the_production_of_spatial_data_layers_of_fishing_intensity_pressure/18639182
# https://ices-library.figshare.com/articles/dataset/Data_for_OSPAR_request_on_the_production_of_spatial_data_layers_of_fishing_intensity_pressure/18601508

# FP
load("Data/ICES fishing activity/FishingPressure_0,05degreeCsquare_2009_2020.RData")
str(FP)

hist(FP$Hour_cov)
hist(FP$Hour_low)
hist(FP$Hour_upp)

FP <- FP %>% dplyr::select(lat,lon,Year,Hour_low,Hour_upp,Hour_cov,geometry)
FP <- FP %>% dplyr::mutate(Year = as.factor(Year))

FP <- st_as_sf(FP,coord="geometry",crs = 4326)
FP   <- FP  %>% st_transform(32631)
hist(FP$Hour_cov)

FP %>%  
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_sf(aes(geometry=geometry,col=Hour_upp))+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))+
    facet_wrap(~Year)

# Calculate mid-point of geometry
FP <- FP %>% dplyr::mutate(centroid = st_centroid(geometry))

unique(FP$Hour_low)
unique(FP$Hour_upp)
unique(FP$Hour_cov)

FP <- FP %>% 
  mutate(Fishing_hour_mean = (Hour_low + Hour_upp)/2,
         Fishing_hour_mean = ifelse(is.na(Fishing_hour_mean) & Year %in% c("2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020"),0,Fishing_hour_mean))


FP <- FP %>% 
    mutate(x_utm = unlist(map(.$centroid,1)),
         y_utm = unlist(map(.$centroid,2)))


# change again to closest point with centroid. 


# Couple points of raster to FP
YM <- unique(FP$Year)
df_FP_grid <- data.frame()

for (i in YM ){

temp_FP <- FP %>% filter(Year == i)%>% 
    dplyr::mutate(RowNr = row_number()) %>% st_drop_geometry()
temp <- temp_FP %>%  dplyr::select(x_utm,y_utm)
temp_FP <- temp_FP %>% dplyr::select(RowNr,Year,Fishing_hour_mean,Hour_cov)

grid_temp <- info_grid_10_10km %>% dplyr::select(x,y)
nearest <- nn2(temp,grid_temp,k=1)
nearest_nr <- as.data.frame(nearest[[1]])
nearest_distance <- as.data.frame(nearest[[2]])
nearest <- bind_cols(nearest_nr,nearest_distance)
nearest <- bind_cols(nearest,grid_temp)
dim(temp_FP);dim(grid_temp);dim(nearest)
  
names(nearest) <- c("RowNr","Distance","x","y")
rm(temp,grid_temp)
nearest <- nearest %>% mutate(Distance_km = Distance/1000) %>% 
    dplyr::select(!Distance) 

FP_grid <- left_join(nearest,temp_FP, by=c("RowNr"))
  
# Change varianles with large distance in NA
FP_grid <- FP_grid %>% dplyr::mutate(Fishing_hour_mean = ifelse(Distance_km > 25, NA,Fishing_hour_mean),
                                     Hour_cov = ifelse(Distance_km > 25, NA,Hour_cov))
FP_grid <- FP_grid %>% dplyr::select(!c(Distance_km,RowNr))
  
FP_grid <- FP_grid %>% dplyr::select(Year, x,y,Fishing_hour_mean,Hour_cov)
  
df_FP_grid <- bind_rows(df_FP_grid,FP_grid)

}


df_FP_grid  %>%  
    ggplot() + 
    geom_point(aes(x=x,y=y,col=log(Fishing_hour_mean+1)))+
    scale_colour_viridis_c()+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  theme_s()+
    coord_sf(xlim=c(60000, 900000),ylim=c(5680000,6650000))+
    facet_wrap(~Year)

df_FP_grid %>%  
    filter(is.na(Fishing_hour_mean)) %>% 
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x,y=y,col=Fishing_hour_mean),col="red")+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))+
    facet_wrap(~Year)

# NA's in the middle of the sea should be zeros. How to "catch" them?
# één punt apart doen. 
df_FP_grid %>%  
    filter(is.na(Fishing_hour_mean)) %>% 
    filter(x > 250000 & x < 450000 & y > 6100000 & y < 6400000) %>% 
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x,y=y,col=Fishing_hour_mean),col="red")+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))


df_FP_grid %>%  
    filter(is.na(Fishing_hour_mean)) %>% 
    filter(x > 300000 & x < 450000 & y > 5870000 & y < 6000000) %>% 
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x,y=y,col=Fishing_hour_mean),col="red")+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))

df_FP_grid <- df_FP_grid %>%  
    mutate(Fishing_hour_mean = ifelse( is.na(Fishing_hour_mean) & x > 250000 & x < 450000 & y > 6100000 & y < 6400000 , 0 , Fishing_hour_mean ))

df_FP_grid <- df_FP_grid %>%  
    mutate(Fishing_hour_mean = ifelse( is.na(Fishing_hour_mean) & x > 300000 & x < 450000 & y > 5870000 & y < 6000000, 0 , Fishing_hour_mean ))

df_FP_grid  %>%  
    ggplot() + 
    geom_point(aes(x=x,y=y,col=log(Fishing_hour_mean+1)))+
    scale_colour_viridis_c()+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  theme_s()+
    coord_sf(xlim=c(60000, 940000),ylim=c(5650000,6650000))+
    facet_wrap(~Year)

rm(FP_grid,temp,nearest)

9373 * length(YM)
dim(df_FP_grid)





# COUPLE ALL DATA!--------------------------



# *Dataset with all variability----------------

dim(df_Chl_a_grid)
str(df_Chl_a_grid)
str(df_SST_grid)

dim(df_SST_grid)
dim(df_Chl_a_grid)
grid_predictions_year_month <- full_join(df_Chl_a_grid,df_SST_grid,by=c("x","y","year_month","Year","Month"))
dim(grid_predictions_year_month)
unique(grid_predictions_year_month$Month)

dim(grid_predictions_year_month)
grid_predictions_year_month <- left_join(grid_predictions_year_month,DepthSediment_grid,by=c("x","y"))
dim(grid_predictions_year_month)

grid_predictions_year_month <- left_join(grid_predictions_year_month,df_Chl_a_grid,by=c("x","y","year_month","Year","Month"))
dim(grid_predictions_year_month)

grid_predictions_year_month <- full_join(grid_predictions_year_month,Breeding_sites_grid,by=c("x","y"))
dim(grid_predictions_year_month)

grid_predictions_year_month <- left_join(grid_predictions_year_month,DistanceShippingLanes,by=c("x","y"))
dim(grid_predictions_year_month)

grid_predictions_year_month <- left_join(grid_predictions_year_month,df_platforms_grid,by=c("x","y","Year"))
dim(grid_predictions_year_month)

grid_predictions_year_month <- left_join(grid_predictions_year_month,df_FP_grid,by=c("x","y","Year"))
dim(grid_predictions_year_month)

str(grid_predictions_year_month)
check <- grid_predictions_year_month %>% dplyr::filter(!is.na(Fishing_hour_mean))
unique(check$Year)
rm(check)

str(grid_predictions_year_month )

grid_predictions_year_month <- grid_predictions_year_month %>% dplyr::select(!meanChlorophyll.x) %>% 
  dplyr::rename(meanChlorophyll = meanChlorophyll.y)

unique(grid_predictions_year_month$depth)

grid_predictions_year_month %>%  
  dplyr::select(x,y,Distance_big_breeding_site_km,scientific_name) %>% 
    distinct() %>% 
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x,y=y,col=Distance_big_breeding_site_km))+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))+
    facet_wrap(~scientific_name)

write_rds(grid_predictions_year_month2,"Input_data/Prediction grid/Prediction_grid_all_variability_10_10km.rds")

# *Average over period (bimonthly)-------------

grid_predictions_period_bimonthly <- grid_predictions_year_month
unique(grid_predictions_period_bimonthly$scientific_name)

grid_predictions_period_bimonthly <- grid_predictions_period_bimonthly %>% 
  dplyr::mutate(Year = as.numeric(Year)) %>% 
  dplyr::mutate(Year_bimonthly = ifelse(Month %in% "12",Year+1,Year))

grid_predictions_period_bimonthly <- grid_predictions_period_bimonthly %>% 
  dplyr::mutate(Bimonthly = case_when(Month %in% c("12","01") ~ "dec-jan",
                                      Month %in% c("02","03") ~ "feb-mrch",
                                      Month %in% c("04","05") ~ "apr-may",
                                      Month %in% c("06","07") ~ "jun-jul",
                                      Month %in% c("08","09") ~ "aug-sep",
                                      Month %in% c("10","11") ~ "oct-nov"))
# Add periodc
grid_predictions_period_bimonthly$periodc <- cut(grid_predictions_period_bimonthly$Year_bimonthly, breaks=c(1999,2005, 2010, 2015, 2020))
table(grid_predictions_period_bimonthly$periodc,grid_predictions_period_bimonthly$Year_bimonthly)

# Select only the part with periodc = not.na
dim(grid_predictions_period_bimonthly)
range(grid_predictions_period_bimonthly$Year_bimonthly)
grid_predictions_period_bimonthly <- grid_predictions_period_bimonthly %>% filter(!is.na(periodc))
dim(grid_predictions_period_bimonthly)
range(grid_predictions_period_bimonthly$Year_bimonthly)



# Takes to long too do it all together, so split.
grid_predictions_period_bimonthly_1 <- grid_predictions_period_bimonthly %>% 
                dplyr::group_by(x,y,periodc,Bimonthly,Analysed_meanSST_Celsius,meanChlorophyll) %>% 
                dplyr::distinct() %>% ungroup() %>% 
                dplyr::group_by(x,y,periodc,Bimonthly) %>% 
                dplyr::summarize(Analysed_meanSST_Celsius = mean(Analysed_meanSST_Celsius,na.rm=T),
                                 meanChlorophyll = mean(meanChlorophyll,na.rm=T))

grid_predictions_period_bimonthly_2 <- grid_predictions_period_bimonthly %>% 
                dplyr::group_by(x,y,MudPercent,SandPercent,depth,distance_coast_avg,DistanceShippingLanesInKm) %>% 
                dplyr::distinct() %>% ungroup() %>% 
                dplyr::group_by(x,y) %>% 
                dplyr::summarize(MudPercent = mean(MudPercent,na.rm=T),
                                 SandPercent = mean(SandPercent,na.rm=T),
                                 depth = mean(depth,na.rm=T),
                                 distance_coast_avg = mean(distance_coast_avg,na.rm=T),
                                 DistanceShippingLanesInKm = mean(DistanceShippingLanesInKm,na.rm=T))

grid_predictions_period_bimonthly_3 <- grid_predictions_period_bimonthly %>% 
                dplyr::group_by(x,y,periodc,Bimonthly,Distance_to_platform_km,Fishing_hour_mean,Hour_cov) %>% 
                dplyr::distinct() %>% ungroup() %>% 
                dplyr::group_by(x,y,periodc,Bimonthly) %>%
                dplyr::summarize(Distance_to_platform_km = mean(Distance_to_platform_km,na.rm=T),
                                 Fishing_hour_mean = mean(Fishing_hour_mean,na.rm=T),
                                 Fishing_hour_cov = mean(Hour_cov,na.rm=T))

grid_predictions_period_bimonthly_4 <- grid_predictions_period_bimonthly %>% 
                dplyr::group_by(x,y,scientific_name,Distance_breeding_site_km,Nr_breeding_pairs_closest_colony,Distance_big_breeding_site_km,Nr_breeding_pairs_biggest_colony) %>% 
                dplyr::distinct() %>% ungroup() %>% 
                dplyr::group_by(x,y,scientific_name) %>% 
                dplyr::summarize(Distance_breeding_site_km = mean(Distance_breeding_site_km,na.rm=T),
                                 Distance_big_breeding_site_km = mean(Distance_big_breeding_site_km,na.rm=T),
                                 Nr_breeding_pairs_closest_colony = first(Nr_breeding_pairs_closest_colony),
                                 Nr_breeding_pairs_biggest_colony = first(Nr_breeding_pairs_biggest_colony))
str(grid_predictions_period_bimonthly_4)
unique(grid_predictions_period_bimonthly_4$Nr_breeding_pairs_closest_colony)

grid_predictions_period_bimonthly_5 <- grid_predictions_period_bimonthly %>% 
                dplyr::group_by(x,y,periodc,Bimonthly,InDutchPartShipLane) %>% 
                dplyr::distinct() %>% 
                dplyr::group_by(x,y,periodc,Bimonthly) %>% 
                dplyr::summarize(InDutchPartShipLane = first(InDutchPartShipLane))


check <- grid_predictions_period_bimonthly %>% 
  filter(is.na(InWindFarm))
dim(check);rm(check)              
unique(grid_predictions_period_bimonthly$InWindFarm)

grid_predictions_period_bimonthly_1 <- grid_predictions_period_bimonthly_1 %>% ungroup()
grid_predictions_period_bimonthly_2 <- grid_predictions_period_bimonthly_2 %>% ungroup()
grid_predictions_period_bimonthly_3 <- grid_predictions_period_bimonthly_3 %>% ungroup()
grid_predictions_period_bimonthly_4 <- grid_predictions_period_bimonthly_4 %>% ungroup()
grid_predictions_period_bimonthly_5 <- grid_predictions_period_bimonthly_5 %>% ungroup()

grid_predictions_period_bimonthly_all <- grid_predictions_period_bimonthly %>% 
  ungroup() %>% 
  dplyr::select(x,y,periodc,Bimonthly) %>% dplyr::distinct()

unique(grid_predictions_period_bimonthly_1$Analysed_meanSST_Celsius)

dim(grid_predictions_period_bimonthly_all)
str(grid_predictions_period_bimonthly_1)
dim(grid_predictions_period_bimonthly_1)

grid_predictions_period_bimonthly_all <- left_join(grid_predictions_period_bimonthly_all,grid_predictions_period_bimonthly_1,by=c("x","y","periodc","Bimonthly"))
dim(grid_predictions_period_bimonthly_all)

dim(grid_predictions_period_bimonthly_all)
str(grid_predictions_period_bimonthly_2)
grid_predictions_period_bimonthly_all <- left_join(grid_predictions_period_bimonthly_all,grid_predictions_period_bimonthly_2,by=c("x","y"))
dim(grid_predictions_period_bimonthly_all)

str(grid_predictions_period_bimonthly_3)
grid_predictions_period_bimonthly_all <- left_join(grid_predictions_period_bimonthly_all,grid_predictions_period_bimonthly_3,by=c("x","y","periodc","Bimonthly"))
dim(grid_predictions_period_bimonthly_all)

str(grid_predictions_period_bimonthly_5)
grid_predictions_period_bimonthly_all <- left_join(grid_predictions_period_bimonthly_all,grid_predictions_period_bimonthly_5,by=c("x","y","periodc","Bimonthly"))
dim(grid_predictions_period_bimonthly_all)

str(grid_predictions_period_bimonthly_4)
grid_predictions_period_bimonthly_all <- left_join(grid_predictions_period_bimonthly_all,grid_predictions_period_bimonthly_4,by=c("x","y"))
dim(grid_predictions_period_bimonthly_all)
224952 * 12

unique(grid_predictions_period_bimonthly_all$scientific_name)

grid_predictions_period_bimonthly_all %>%  
  dplyr::select(x,y,Distance_big_breeding_site_km,scientific_name) %>% 
    distinct() %>% 
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x,y=y,col=Distance_big_breeding_site_km))+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))+
    facet_wrap(~scientific_name)

grid_predictions_period_bimonthly_all %>%  
  dplyr::select(x,y,Fishing_hour_mean,periodc) %>% 
    distinct() %>% 
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x,y=y,col=Fishing_hour_mean))+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))+
    facet_wrap(~periodc)

write_rds(grid_predictions_period_bimonthly_all,"Input_data/Prediction grid/Prediction_grid_all_Bimonthly_Per_5yearperiod_10_10km.rds")






# *Average everything over Year_bimontly, Month and x and y -------------------

str(grid_predictions_year_bimonthly_all)

# For distance to oil platforms I used a recent year (2020) and bimonthly "oct-nov"
table(grid_predictions_year_bimonthly_all$Bimonthly,grid_predictions_year_bimonthly_all$Year_bimonthly)

grid_predictions_oil_OWF_2020 <- grid_predictions_year_bimonthly_all %>% 
  dplyr::filter(Year_bimonthly %in% 2020) %>% 
  dplyr::filter(Bimonthly %in% "oct-nov") %>% 
  dplyr::select(x,y,InDutchPartShipLane,Distance_to_platform_km,Distance_breeding_site_km,Nr_breeding_pairs_closest_colony,Distance_big_breeding_site_km,Nr_breeding_pairs_biggest_colony ,scientific_name) %>% ungroup() 

grid_predictions_bimonthly_all <- grid_predictions_year_bimonthly_all %>% ungroup() %>% 
  dplyr::select(!c(Distance_to_platform_km)) %>% 
  dplyr::group_by(x,y,Bimonthly) %>% 
  dplyr::summarize(Analysed_meanSST_Celsius =mean(Analysed_meanSST_Celsius,na.rm=T),
            meanChlorophyll =mean(meanChlorophyll,na.rm=T),
            MudPercent  =mean(MudPercent ,na.rm=T),
            SandPercent =mean(SandPercent,na.rm=T),
            depth =mean(depth,na.rm=T),
            distance_coast_avg =mean(distance_coast_avg,na.rm=T),
            DistanceShippingLanesInKm =mean(DistanceShippingLanesInKm,na.rm=T),
            Fishing_hour_mean  =mean(Fishing_hour_mean ,na.rm=T),
            Fishing_hour_cov =mean(Fishing_hour_cov,na.rm=T)) %>% ungroup() %>% distinct()
  
grid_predictions_bimonthly_all <- left_join(grid_predictions_oil_OWF_2020,grid_predictions_bimonthly_all,by=c("x","y"))
dim(grid_predictions_bimonthly_all)
str(grid_predictions_bimonthly_all)

write_rds(grid_predictions_bimonthly_all,"Input_data/Prediction grid/Prediction_grid_all_Bimonthly_10_10km.rds")


grid_predictions_bimonthly_all %>%  
  dplyr::select(x,y,DistanceShippingLanesInKm) %>% 
    distinct() %>% 
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x,y=y,col=DistanceShippingLanesInKm))+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))

grid_predictions_bimonthly_all %>%  
  dplyr::select(x,y,InWindFarm) %>% 
    distinct() %>% 
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x,y=y,col=InWindFarm))+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))

grid_predictions_bimonthly_all %>%  
  dplyr::select(x,y,Fishing_hour_mean) %>% 
    distinct() %>% 
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x,y=y,col=Fishing_hour_mean))+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))

grid_predictions_bimonthly_all %>%  
  dplyr::select(x,y,depth) %>% 
    distinct() %>% 
    ggplot() + 
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    geom_point(aes(x=x,y=y,col=depth))+
    theme_s()+
    coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))





### ESAS dataset opwerking -------------------------

# Marinka van Puijenbroek & Susanne van Donk -------
# v1 - 2021-02-17
# v2 - 2022-10-10 (SvD)
# Information about terminology: https://ices-tools-dev.github.io/esas/tables/
#  Careful! This website is updated and can have differences with the dataset.
# For correct information check file send by VanErmen.https://docs.google.com/document/d/15tNRx-AjP3IhhPzLK-hlf5VcF7iSy3i8/edit

# Remove all data 
rm(list= ls())

# Define species
# !!!!!When black scoter is in the selection, make sure to check both euring_species_code 2130 & 2131!!!!
# Euring Species code in W:\IMARES\DATA\KEC4-0\4. Data\1. Ruwe data\ESAS_dataset\ESAS v6\EURING_codes.csv
Species <- c("Morus bassanus","Larus argentatus","Larus fuscus","Larus marinus","Rissa tridactyla","Stercorarius skua","Alca torda","Uria aalge","Fulmarus glacialis","Fratercula arctica","Gavia stellata","Thalasseus sandvicensis")
Euring_species <- c(710,5920,5910,6000,6020,5690,6360,6340,220,6540,20,6110)
length(Species) == length(Euring_species)

# Define origin (MTWL/ESAS) & data range
start_jaar <- 1991
bron <- c("MWTL","ESAS")



# Load packages 
library(tidyverse)
library(sf)
library(readxl)
library(lubridate)
library(rgdal)
# For plotting
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

# Functies 
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




# Step 1: Load dataset with all data ------------------------------

All_data <- readRDS("Data_tussenproduct/ESAS_MWTL_raw.rds") 
dim(All_data)

# Noordzee grid
GRID_shape <- st_read("W:/IMARES/DATA/KEC4-0/4. Data/1. Ruwe data/GIS bestand noordzee/GRID_north_sea.shp")


# Step 2: Select counts in Northsea subareas ------------------------------------------

All_data <- All_data[stats::complete.cases(All_data[, c("latitude","longitude")]),]
dim(All_data)

DT_sf = st_as_sf(All_data, coords = c("longitude","latitude"), 
                 crs = 4326, agr = "constant")

DT_sf_utm31 <- DT_sf %>% st_transform(32631)

data_join <- st_join(DT_sf_utm31, GRID_shape, join = st_intersects) 

# Southern Northsea and Central Northsea 
data_northsea <- data_join %>% 
  filter(SubArea %in% c("SNS", "CNS"))
dim(data_northsea)

rm(All_data,data_join,DT_sf,DT_sf_utm31)

world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
Europe_utm31 <- Europe %>% st_transform(32631)

shapefile <- rgdal::readOGR("Shapefiles/Mainland.shp")
shapefile2 <- spTransform(shapefile, CRS = "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs")

str(data_northsea)

# *Add seasons and select years -------------

# change date, add seasons & select data 
unique(data_northsea$origin)

data_northsea <- data_northsea %>% 
  mutate(origin = case_when(
    origin %in% c("ESAS v5","data update and replacement 06/11/2020","data update 06/11/2020") ~ "ESAS",
    origin %in% c("MWTL") ~ "MWTL"
  ))

data_northsea <- data_northsea %>% 
  mutate(
    #date = parse_date_time(date, c('%Y-%m-%d', '%d/%m/%Y')),# Verschillende dataformats 
    maand = month(date),
    season = case_when(
      maand %in% c(8,9)   ~ 1, 
      maand %in% c(10,11) ~ 2, 
      maand %in% c(12,1)  ~ 3, 
      maand %in% c(2,3)   ~ 4, 
      maand %in% c(4,5)   ~ 5, 
      maand %in% c(6,7)   ~ 6
    ), year = ifelse( maand == 1, year(date) - 1, year(date))
  ) 

data_northsea  %>%  group_by(origin) %>% 
  summarize(max = max(date))

dim(data_northsea)

# selection source and years
data_northsea <- data_northsea %>% 
  filter(year >= start_jaar,
         origin %in% c(bron))

dim(data_northsea)
table(data_northsea$year,data_northsea$origin)


# Add X and Y
data_northsea <- data_northsea %>% 
  mutate(X = unlist(map(.$geometry,1)),    # longitude
         Y = unlist(map(.$geometry,2)))  # latitude

data_northsea %>% summarise_all(~ sum(is.na(.))) %>% dplyr::select(X,Y)



# *Set unique poskeys apart for zeros later ------------
Uni_poskeys <- data_northsea %>% 
  #mutate(longitude = unlist(map(.$geometry,1)),
  #      latitude = unlist(map(.$geometry,2))) %>% 
  st_drop_geometry() %>% 
  group_by(tripkey, date, poskey) %>% 
  summarise(data_provider = first(data_provider), season = first(season), year = first(year), count_method = first(count_method), species_counted = first(species_counted), number_of_observers = first(number_of_observers), observer1 = first(observer1), observer2 = first(observer2), origin = first(origin), GRID_ID = first(GRID_ID), SubArea = first(SubArea),X = first(X),Y = first(Y),km_travelled = first(km_travelled), area_surveyed = first(area_surveyed))

# Data range 
data_northsea %>% dplyr::group_by(origin) %>% dplyr::summarize( min = min(date),max = max(date))




range(data_northsea$area_surveyed,na.rm=T)


# DATA CHECK #---------------------------------

# check important variabels like;
# tripkey, poskey, obskey
# transect_width, distance bins and transect

# check NA
nr_NA<- data_northsea %>% summarise_all(~ sum(is.na(.))) # 1 row without lat-long
rm(nr_NA)

# Geen dubbele waarden.Uitgevoerd zonder origin.
dubbel <- data_northsea %>% 
            st_drop_geometry() %>% 
            dplyr::select(X,Y,tripkey,poskey,obskey,euring_species_code,plumage, number, date,count_method,species_counted,km_travelled,area_surveyed,flying_height,transect_width,distance_bins,distance,transect) %>% 
            janitor::get_dupes()

# Differences between origins
table(data_northsea$origin,data_northsea$transect_width)
table(data_northsea$origin,data_northsea$distance_bins)
table(data_northsea$origin,data_northsea$distance)
table(data_northsea$origin,data_northsea$year)
table(data_northsea$origin,data_northsea$count_method)
table(data_northsea$origin,data_northsea$data_provider)
table(data_northsea$year,data_northsea$count_method)
table(data_northsea$origin,data_northsea$euring_species_code)# what is code 99999?


# Transect
# 1	Out of transect
#	2 In transect - also used when no birds are seen during an observation period.
unique(data_northsea$transect)
# more observations in 1 than in 2. 6 observations in 3
table(data_northsea$origin,data_northsea$transect) 



# MWTL  --> in old dataset 1 should be switched with 2
#       --> in new dataset NA should become 1?
# In ESAS --> many NA
# Drop 3, don't know what it is and only few observations
plot <- data_northsea %>%  
  st_drop_geometry() %>% 
  group_by(origin,year,transect) %>% 
  mutate(transect = as.factor(transect)) %>% 
  summarize(observations = n()) %>% 
  ggplot(aes(x=year,y=observations,col=transect,group=transect)) +
  theme_s()+
  geom_line(size=1.5) +
  facet_wrap(~origin,nrow=2,scales = "free")  

ggsave(plot, file=paste0("Output/Figures/Transect_time_origin.png"), width = 28, height = 20, units = 'cm')
rm(plot)

# what is going on with the NA in ESAS data?
# mostly also NA in number
# Positions were no birds are observed have transect NA. The increase between 2010-2016 is probably caused by counts from the airplane that were conducted in Germany that have counts per second (pers. comm. VanErmen). See below count_number 8
check <- data_northsea %>% 
            st_drop_geometry() %>%
            filter(origin %in% "ESAS",
                  is.na(transect)) 
table(check$species_counted) 
table(check$count_method)
table(check$number,check$year)
check %>% filter(is.na(number)) %>% 
  summarize(n())
check %>% filter(!is.na(number)) %>% 
  summarize(n())

check %>% filter(is.na(count_method)) %>% 
  summarize(n())

# Spatial ESAS view of number = NA and count_method
check %>% 
  filter(is.na(number)) %>% 
   filter(!is.na(count_method)) %>% 
  mutate(count_method = as.factor(count_method)) %>% 
  #filter(year==2001) %>% 
  ggplot() + 
  theme_s()+
  geom_point(aes(y=Y, x=X, fill = count_method,col = count_method),alpha=0.5) +
  geom_polygon(data = shapefile2, aes(y = lat, x = long, group = group), fill="lightgrey",color = "lightgrey")+
  coord_equal()+
  facet_wrap(~year)


# Spatial unique locations ESAS versus MWTL
temp_plot <- data_northsea %>%  
  st_drop_geometry() %>% 
  group_by(X,Y,origin,year,poskey) %>% 
  summarize(avg = sum(number,na.rm=T)) %>% 
  ggplot() + 
  theme_s()+
  geom_point(aes(y=Y, x=X, fill = origin,col = origin),alpha=0.5,size=0.5) +
  geom_polygon(data = shapefile2, aes(y = lat, x = long, group = group), fill="lightgrey",color = "lightgrey")+
  coord_equal()+
  facet_wrap(~year, nrow=4)+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

ggsave(temp_plot, file=paste0("Output/Figures/Spatial_over_time.png"), width = 40, height = 40, units = 'cm')


# Spatial unique locations area_surveyed
temp_plot <- data_northsea %>%  
  st_drop_geometry() %>% 
  group_by(X,Y,origin,year,poskey) %>%
  distinct() %>% 
  summarize(avg_log = log(mean(area_surveyed+1,na.rm=T))) %>% 
  ggplot() + 
  theme_s()+
  geom_point(aes(y=Y, x=X, fill = avg_log,col = avg_log),alpha=0.5,size=0.3) +
  geom_polygon(data = shapefile2, aes(y = lat, x = long, group = group), fill="lightgrey",color = "lightgrey")+
  coord_equal()+
  facet_wrap(~year, nrow=4)+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

ggsave(temp_plot, file=paste0("Output/Figures/Spatial_over_time_area_surveyed.png"), width = 40, height = 40, units = 'cm')


# Histogram area-surveyed per count_method and origin
p<-data_northsea %>%  
  st_drop_geometry() %>% 
  #filter(! (count_method %in% 8)) %>% 
  group_by(X,Y,origin,year,poskey) %>% 
  distinct() %>% 
  summarize(avg = mean(area_surveyed,na.rm=T)) %>% 
  ggplot(aes(x=avg,fill=origin)) + 
  geom_histogram(binwidth = 0.15)+
  #facet_wrap(~year, nrow=4)+
  theme_s()+
  #xlim(NA, 50)+
  xlab("Average area surveyed per position")

ggsave(p, file=paste0("Output/Figures/Histogram_area_surveyed.png"), width = 15, height = 12, units = 'cm')
ggsave(p, file=paste0("Output/Figures/Histogram_area_surveyed_all years.png"), width = 40, height = 40, units = 'cm')
ggsave(p, file=paste0("Output/Figures/Histogram_area_surveyed_all years_without_8.png"), width = 40, height = 40, units = 'cm')

# And seperate for ESAS and count_method
p <- data_northsea %>%  
  st_drop_geometry() %>% 
  filter(origin %in% "ESAS") %>% 
  filter(! is.na(count_method)) %>% 
  #filter(! (count_method %in% 8)) %>% 
  group_by(X,Y,count_method,year,poskey) %>% 
  summarize(avg = sum(area_surveyed,na.rm=T)) %>% 
  mutate(count_method = as.factor(count_method)) %>% 
  ggplot(aes(x=avg,fill=count_method)) + 
  geom_histogram(binwidth = 0.15)+
  #facet_wrap(~year, nrow=4)+
  theme_s()+
  xlim(NA, 50)+
  xlab("Average area surveyed per position")

ggsave(p, file=paste0("Output/Figures/Histogram_area_surveyed_ESAS.png"), width = 15, height = 12, units = 'cm')

# 8 has very low area_surveyed
# And seperate for ESAS and count_method
plot <- data_northsea %>%  
  st_drop_geometry() %>% 
  filter(origin %in% "ESAS",
         count_method %in% 8) %>% 
  group_by(X,Y,count_method,year,poskey) %>% 
  summarize(avg_log = log(sum(area_surveyed+1,na.rm=T))) %>% 
  ggplot() + 
  theme_s()+
  geom_point(aes(y=Y, x=X, fill = avg_log,col = avg_log),alpha=0.5,size=0.3) +
  geom_polygon(data = shapefile2, aes(y = lat, x = long, group = group), fill="lightgrey",color = "lightgrey")+
  coord_equal()+
  facet_wrap(~year, nrow=2)+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

ggsave(plot, file=paste0("Output/Figures/ESAS_count_method_9.png"), width = 25, height = 15, units = 'cm')


# 1045 values with zero or NA at area_surveyed. Remove?
zero_poskey <- Uni_poskeys %>% filter(area_surveyed %in% c(0,NA))



# Check strip zero MWTL...? What was this again? Look up later.

rm(zero_poskey,check,dubbel)

# Plotje datapunten
Spatial_species <- data_northsea %>% 
  filter(euring_species_code %in% c(710,5920,5910,6000,6020,5690,6360,6340,220,6540,20,6110)) %>%
  ggplot() + 
  theme_s()+
  geom_polygon(data = shapefile2, aes(x = long, y = lat, group = group), fill="lightgrey",color = "lightgrey")+
  geom_point(aes(y=Y, x=X,col=origin),alpha=0.5,size=0.15) +
  scale_color_manual(values=c("darkgrey", "gold"))+
  coord_sf(xlim=c(0,1000000),ylim=c(5500000,6500000))+
  facet_wrap(~euring_species_code,nrow=4,
             labeller = labeller(euring_species_code = 
    c("20" = "Red-throated diver",
      "220" = "Northern fulmar",
      "710" = "Northern gannet",
      "5690" = "Great skua",
      "5910" = "Lesser black-backed gull",
      "5920" = "European herring gull",
      "6000" = "Great black-backed gull",
      "6020" = "Black-legged kittiwake",
      "6110" = "Sandwich tern",
      "6340" = "Common guillemot",
      "6360" = "Razorbill",
      "6540" = "Atlantic puffin")))+
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())
ggsave(Spatial_species, file=paste0("Output/Figures/Spatial_species.png"), width = 40, height = 40, units = 'cm')

# Totaal aantal over tijd per soort
Number_species <- data_northsea %>% 
  filter(euring_species_code %in% c(710,5920,5910,6000,6020,5690,6360,6340,220,6540,20,6110)) %>%
  #filter(origin == "MWTL") %>% #filter(year <2014) %>% 
  group_by(euring_species_code,year,origin) %>% 
  summarize(som = sum(number,na.rm=T)) %>% 
  mutate(euring_species_code = as.factor(euring_species_code)) %>% 
  ggplot(aes(y=som,x=year, col = origin))+
  scale_color_manual(values=c("darkgrey", "gold"))+
  geom_line(size=1)+
  theme_s()+
  facet_wrap(~euring_species_code,nrow=4,scales="free",
             labeller = labeller(euring_species_code = 
    c("20" = "Red-throated diver",
      "220" = "Northern fulmar",
      "710" = "Northern gannet",
      "5690" = "Great skua",
      "5910" = "Lesser black-backed gull",
      "5920" = "European herring gull",
      "6000" = "Great black-backed gull",
      "6020" = "Black-legged kittiwake",
      "6110" = "Sandwich tern",
      "6340" = "Common guillemot",
      "6360" = "Razorbill",
      "6540" = "Atlantic puffin")))
ggsave(Number_species, file=paste0("Output/Figures/Number_over_time_species.png"), width = 25, height = 20, units = 'cm')

# Totaal over maanden
Number_species <- data_northsea %>% 
  filter(euring_species_code %in% c(710,5920,5910,6000,6020,5690,6360,6340,220,6540,20,6110)) %>%
  #filter(origin == "MWTL") %>% #filter(year <2014) %>% 
  group_by(euring_species_code,maand,origin) %>% 
  summarize(som = sum(number,na.rm=T)) %>% 
  mutate(euring_species_code = as.factor(euring_species_code)) %>% 
  ggplot(aes(y=som,x=maand, col = origin))+
  scale_color_manual(values=c("darkgrey", "gold"))+
  geom_line(size=1)+
  theme_s()+
  facet_wrap(~euring_species_code,nrow=4,scales="free",
             labeller = labeller(euring_species_code = 
    c("20" = "Red-throated diver",
      "220" = "Northern fulmar",
      "710" = "Northern gannet",
      "5690" = "Great skua",
      "5910" = "Lesser black-backed gull",
      "5920" = "European herring gull",
      "6000" = "Great black-backed gull",
      "6020" = "Black-legged kittiwake",
      "6110" = "Sandwich tern",
      "6340" = "Common guillemot",
      "6360" = "Razorbill",
      "6540" = "Atlantic puffin")))
ggsave(Number_species, file=paste0("Output/Figures/Number_over_month_species.png"), width = 25, height = 20, units = 'cm')

check <- data_northsea %>% 
  filter(euring_species_code %in% c(710,5920,5910,6000,6020,5690,6360,6340,220,6540,20,6110),
         origin %in% "MWTL") 
table(check$euring_species_code,check$maand)
check <- data_northsea %>% 
  filter(euring_species_code %in% c(710,5920,5910,6000,6020,5690,6360,6340,220,6540,20,6110),
         origin %in% "ESAS") 
table(check$euring_species_code,check$maand)












# Step 3: Adjustments based on data check ---------------

dim(data_northsea)

# adjust transect in MWTL 1991-2014 part from 1 to 2 and change code for black scoter
MWTL_northsea_1991_2013_1 <- data_northsea %>% 
  filter(origin == c("MWTL")) %>% 
  filter(count_method == 9) %>% # count_method used in this period
  mutate(transect = if_else(transect == 1, 2,1)) %>%
  mutate(euring_species_code = if_else(euring_species_code == 2131,2130,euring_species_code)) # black scoter code used

dim(MWTL_northsea_1991_2013_1)

data_northsea_1 <- data_northsea %>% 
  filter(!(origin == c("MWTL") & count_method == 9)) 
dim(data_northsea_1)


data_northsea_1 <- bind_rows(data_northsea_1,MWTL_northsea_1991_2013_1)
dim(data_northsea_1)
1866633+239904

# remove transect 3 in ESAS
data_northsea_1 <- data_northsea_1 %>% filter(transect %notin% 3)
dim(data_northsea_1)

# Select columns to keep
data_northsea_1 <- data_northsea_1 %>% 
  dplyr::select(tripkey,poskey,obskey,euring_species_code,plumage, number, date, season, year,count_method,species_counted,km_travelled,area_surveyed,flying_height,transect_width,distance_bins,distance,transect,number_of_observers, observer1,observer2,data_provider,origin,GRID_ID,SubArea,X,Y)

dim(data_northsea_1)



# 1045 values with zero or NA at area_surveyed. Remove later?
# check strip zero in MWTL 








#Step 4: Combine or splits birds species  ------------------------
# Certain bird species are combined sometimes when people could not see which species it was
# Before we combined diver-species. We will not do this now.
# An overview of all the bird codes can be found...???!!!!!!!!!


# Create grid of 50x50 km to determine fraction between two species
GRID_shape_northsea <- GRID_shape %>% 
  filter(SubArea %in% c("SNS", "CNS"))

GRID_50km <- st_make_grid(GRID_shape_northsea, cellsize = c(50000,50000)) %>% 
  st_sf() %>% 
  add_column(grid_50id = 1:399)

data_northsea2 <- st_join(data_northsea_1, GRID_50km, join = st_intersects)

# for computers with a small memory
rm(GRID_shape_northsea,GRID_50km,GRID_shape)






#* Split zeekoet (6340) en alk (6360) op locatie en seizoen ---- 
# 6345 --> zeekoet/alk observaties Uria aalge / Alca torda
data_6345_fraction<- data_northsea2 %>% 
  st_drop_geometry() %>% 
  filter(euring_species_code %in% c(6340, 6360)) %>% 
  group_by(season, grid_50id, euring_species_code) %>% 
  summarise(sum_number = sum(number))%>% 
  pivot_wider(names_from = euring_species_code, values_from = sum_number ) %>% 
  rename(sp_6340 = '6340',sp_6360 = '6360') %>% 
  mutate(sp_6340 = replace_na(sp_6340,0),
         sp_6360 = replace_na(sp_6360,0), 
         fraction_6340 = sp_6340/(sp_6360+sp_6340)) %>% # fraction zeekoet per grid
  dplyr::select(-sp_6340, -sp_6360)

data_6345 <- data_northsea2 %>% 
  filter(euring_species_code == 6345) %>% 
  left_join(data_6345_fraction, by = c("season", "grid_50id" )) %>% 
  mutate("6340" = number*fraction_6340, "6360" = number* (1- fraction_6340)) %>% 
  dplyr::select(-euring_species_code, -number, -fraction_6340) %>% 
  mutate(longitude = unlist(map(.$geometry,1)),
         latitude = unlist(map(.$geometry,2))) %>% 
  st_drop_geometry() %>% 
  pivot_longer(cols = c("6340":"6360"), 
               names_to = "euring_species_code", values_to = "number") %>% 
  mutate(euring_species_code = as.numeric(euring_species_code))%>% 
  st_as_sf(., coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")

# check
data_northsea2 %>% 
  st_drop_geometry() %>% 
  filter(euring_species_code == 6345) %>% 
  summarize(n(),length(unique(obskey)),unique(plumage))
length(unique(data_6345$obskey))

# Split 6549 --> Alcidae; zeekoet (6340), alk (6360), Papegaaiduiker (6540)

data_6549_fraction <- data_northsea2 %>% 
  st_drop_geometry() %>% 
  filter(euring_species_code %in% c(6340, 6360,6540)) %>% 
  group_by(season, grid_50id, euring_species_code) %>% 
  summarise(sum_number = sum(number)) %>% 
  pivot_wider(names_from = euring_species_code, values_from = sum_number ) %>% 
  rename(sp_6340 = '6340',sp_6360 = '6360',sp_6540 = '6540') %>% 
  mutate(sp_6340 = replace_na(sp_6340,0),
         sp_6360 = replace_na(sp_6360,0),
         sp_6540 = replace_na(sp_6540,0),
         fraction_6340 = sp_6340/(sp_6360+sp_6340+sp_6540),
         fraction_6360 = sp_6360/(sp_6360+sp_6340+sp_6540),
         fraction_6540 = sp_6540/(sp_6360+sp_6340+sp_6540)) %>% 
  dplyr::select(-sp_6340, -sp_6360,-sp_6540)

data_6549 <- data_northsea2 %>% 
  filter(euring_species_code == 6549) %>% 
  left_join(data_6549_fraction, by = c("season", "grid_50id" )) %>% 
  mutate("6340" = number*fraction_6340, 
         "6360" = number*fraction_6360,
         "6540" = number*fraction_6540) %>% 
  dplyr::select(-euring_species_code, -number, -fraction_6340,-fraction_6360,-fraction_6540) %>% 
   mutate(longitude = unlist(map(.$geometry,1)),
         latitude = unlist(map(.$geometry,2))) %>% 
  st_drop_geometry() %>% 
  pivot_longer(cols = c("6340":"6540"), 
               names_to = "euring_species_code", values_to = "number") %>% 
  mutate(euring_species_code = as.numeric(euring_species_code)) %>% 
  st_as_sf(., coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")

# check
data_northsea2 %>% 
  st_drop_geometry() %>% 
  filter(euring_species_code == 6549) %>% 
  summarize(n(),length(unique(obskey)),unique(plumage))
length(unique(data_6549$obskey))



# Combine data 
data_northsea3 <- data_northsea2 %>% 
  filter(euring_species_code %notin% c(6345,6549)) %>%
  bind_rows(data_6345,data_6549)

rm(data_6345_fraction,data_6345,data_6549,data_6549_fraction,data_northsea2)
rm(data_northsea)

#* Split Gulls ---- 

# Combine lesser black backed gull subspecies (fuscus, graelsii)

data_northsea4 <- data_northsea3 %>% 
  mutate(euring_species_code = if_else(euring_species_code %in% c(5911,5912), 5910, euring_species_code))

# combine  herring gull subspecies (argentatus argentatus)

data_northsea4 <- data_northsea4 %>% 
  mutate(euring_species_code = if_else(euring_species_code %in% c(5921), 5920, euring_species_code))

# Split lesser black backed gull (5910) / herring gull (5920)

data_northsea_5919_fraction<- data_northsea4 %>% 
  st_drop_geometry() %>% 
  filter( euring_species_code %in% c(5910, 5920)) %>% 
  group_by(season, grid_50id, euring_species_code) %>% 
  summarise(sum_number = sum(number)) %>% 
  pivot_wider(names_from = euring_species_code, values_from = sum_number ) %>% 
  rename(sp_5910 = '5910',sp_5920 = '5920') %>% 
  mutate(sp_5910 = replace_na(sp_5910,0),
         sp_5920 = replace_na(sp_5920,0), 
         fraction_5910 = sp_5910/(sp_5920+sp_5910)) %>% 
  dplyr::select(-sp_5910, -sp_5920)

hist(data_northsea_5919_fraction$fraction_5910)

data_herring_5919 <- data_northsea4 %>% 
  filter(euring_species_code == 5919) %>% 
  left_join(data_northsea_5919_fraction, by = c("season", "grid_50id" )) %>% 
  mutate("5910" = number*fraction_5910, "5920" = number* (1- fraction_5910)) %>% 
  dplyr::select(-euring_species_code, -number, -fraction_5910) %>% 
   mutate(longitude = unlist(map(.$geometry,1)),
         latitude = unlist(map(.$geometry,2))) %>% 
  st_drop_geometry() %>% 
  pivot_longer(cols = c("5910":"5920"), 
               names_to = "euring_species_code", values_to = "number") %>% 
  mutate(euring_species_code = as.numeric(euring_species_code))%>% 
  st_as_sf(., coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")

# check
data_northsea4 %>% 
  st_drop_geometry() %>% 
  filter(euring_species_code == 5919) %>% 
  summarize(n(),length(unique(obskey)),unique(plumage))
length(unique(data_herring_5919$obskey))*2
dim(data_herring_5919)


# Combine data 
data_northsea5 <- data_northsea4 %>% 
  filter(euring_species_code %notin% c(5919)) %>%
  bind_rows(data_herring_5919)

# tussendoorse check. 
check <- data_northsea4 %>% 
  filter(euring_species_code == 5919) %>% 
  full_join(data_northsea_5919_fraction, by = c("season", "grid_50id" )) %>% 
  mutate("5910" = number*fraction_5910, "5920" = number* (1- fraction_5910)) %>% dplyr::select(tripkey,euring_species_code,fraction_5910,number,"5910","5920")

rm(data_northsea3,data_northsea4,data_herring_5919,data_northsea_5919_fraction,check)
  
  
# Split Euring 6009 - Unidentified larus gull in meest voorkomende meeuwen
# Kleine mantelmeeuw (5910), Zilvermeeuw (5920), Pontische meeuw (5927), geelpootmeeuw (5926), Grote Mantelmeeuw (6000), kokmeeuw (5820) en stormmeeuw (5900)

data_6009_fraction <- data_northsea5 %>% 
  st_drop_geometry() %>% 
  filter(euring_species_code %in% c(5910,5920,5927,5926,6000,5820,5900)) %>% 
  group_by(season, grid_50id, euring_species_code) %>% 
  summarise(sum_number = sum(number)) %>% 
  pivot_wider(names_from = euring_species_code, values_from = sum_number ) %>% 
  rename(sp_5910 = '5910',sp_5920 = '5920',sp_5927 = '5927',sp_5926 = '5926',sp_6000 = '6000',sp_5820 = '5820',sp_5900 = '5900') %>% 
  mutate(sp_5910 = replace_na(sp_5910,0),
         sp_5920 = replace_na(sp_5920,0),
         sp_5927 = replace_na(sp_5927,0),
         sp_5926 = replace_na(sp_5926,0),
         sp_6000 = replace_na(sp_6000,0),
         sp_5820 = replace_na(sp_5820,0),
         sp_5900 = replace_na(sp_5900,0),
         fraction_5910 = sp_5910/(sp_5910+sp_5920+sp_5927+sp_5926+sp_6000+sp_5820+sp_5900),
         fraction_5920 = sp_5920/(sp_5910+sp_5920+sp_5927+sp_5926+sp_6000+sp_5820+sp_5900),
         fraction_5927 = sp_5927/(sp_5910+sp_5920+sp_5927+sp_5926+sp_6000+sp_5820+sp_5900),
         fraction_5926 = sp_5926/(sp_5910+sp_5920+sp_5927+sp_5926+sp_6000+sp_5820+sp_5900),
         fraction_6000 = sp_6000/(sp_5910+sp_5920+sp_5927+sp_5926+sp_6000+sp_5820+sp_5900),
         fraction_5820 = sp_5820/(sp_5910+sp_5920+sp_5927+sp_5926+sp_6000+sp_5820+sp_5900),
         fraction_5900 = sp_5900/(sp_5910+sp_5920+sp_5927+sp_5926+sp_6000+sp_5820+sp_5900)) %>% 
    dplyr::select(-sp_5910,-sp_5920,-sp_5927,-sp_5926,-sp_6000,-sp_5820,-sp_5900)

# Grootste fracties gaan naar Zilvermeeuw 5920 en Kleine 5910, Grote Mantelmeeuw 6000
hist(data_6009_fraction$fraction_5910)
hist(data_6009_fraction$fraction_5920)
hist(data_6009_fraction$fraction_5927)
hist(data_6009_fraction$fraction_5926)
hist(data_6009_fraction$fraction_6000)
hist(data_6009_fraction$fraction_5820)
hist(data_6009_fraction$fraction_5900)

data_6009 <- data_northsea5 %>% 
  filter(euring_species_code == 6009) %>% 
  left_join(data_6009_fraction, by = c("season", "grid_50id" )) %>% 
  mutate("5910" = number*fraction_5910, 
         "5920" = number*fraction_5920,
         "5927" = number*fraction_5927,
         "5926" = number*fraction_5926, 
         "6000" = number*fraction_6000,
         "5820" = number*fraction_5820,
         "5900" = number*fraction_5900) %>% 
  dplyr::select(-euring_species_code, -number, -fraction_5910,-fraction_5920,-fraction_5927,-fraction_5926,-fraction_6000,-fraction_5820,-fraction_5900) %>% 
   mutate(longitude = unlist(map(.$geometry,1)),
         latitude = unlist(map(.$geometry,2))) %>%  
  st_drop_geometry() %>% 
  pivot_longer(cols = c("5910":"5920","5927","5926","6000","5820","5900"), 
               names_to = "euring_species_code", values_to = "number") %>% 
  mutate(euring_species_code = as.numeric(euring_species_code))%>% 
  st_as_sf(., coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")

Nul <- data_6009 %>% filter(number==0)
rm(Nul)


# check
data_northsea5 %>% 
  st_drop_geometry() %>% 
  filter(euring_species_code == 6009) %>% 
  summarize(n(),length(unique(obskey)),unique(plumage))
length(unique(data_6009$obskey))*7
dim(data_6009)




# Split 6049 unidentified gull in  meest voorkomende meeuwen
# Kleine mantelmeeuw (5910), Zilvermeeuw (5920), Pontische meeuw (5927), geelpootmeeuw (5926), Grote Mantelmeeuw (6000),kokmeeuw (5820) en stormmeeuw (5900), drieteenmeeuw (6020)
data_6049_fraction <- data_northsea5 %>% 
  st_drop_geometry() %>% 
  filter(euring_species_code %in% c(5910,5920,5927,5926,6000,5820,5900,6020)) %>% 
  group_by(season, grid_50id, euring_species_code) %>% 
  summarise(sum_number = sum(number)) %>% 
  pivot_wider(names_from = euring_species_code, values_from = sum_number ) %>% 
  rename(sp_5910 = '5910',sp_5920 = '5920',sp_5927 = '5927',sp_5926 = '5926',sp_6000 = '6000',sp_5820 = '5820',sp_5900 = '5900',sp_6020 = '6020') %>% 
  mutate(sp_5910 = replace_na(sp_5910,0),
         sp_5920 = replace_na(sp_5920,0),
         sp_5927 = replace_na(sp_5927,0),
         sp_5926 = replace_na(sp_5926,0),
         sp_6000 = replace_na(sp_6000,0),
         sp_5820 = replace_na(sp_5820,0),
         sp_5900 = replace_na(sp_5900,0),
         sp_6020 = replace_na(sp_6020,0),
         fraction_5910 = sp_5910/(sp_5910+sp_5920+sp_5927+sp_5926+sp_6000+sp_5820+sp_5900+sp_6020),
         fraction_5920 = sp_5920/(sp_5910+sp_5920+sp_5927+sp_5926+sp_6000+sp_5820+sp_5900+sp_6020),
         fraction_5927 = sp_5927/(sp_5910+sp_5920+sp_5927+sp_5926+sp_6000+sp_5820+sp_5900+sp_6020),
         fraction_5926 = sp_5926/(sp_5910+sp_5920+sp_5927+sp_5926+sp_6000+sp_5820+sp_5900+sp_6020),
         fraction_6000 = sp_6000/(sp_5910+sp_5920+sp_5927+sp_5926+sp_6000+sp_5820+sp_5900+sp_6020),
         fraction_5820 = sp_5820/(sp_5910+sp_5920+sp_5927+sp_5926+sp_6000+sp_5820+sp_5900+sp_6020),
         fraction_5900 = sp_5900/(sp_5910+sp_5920+sp_5927+sp_5926+sp_6000+sp_5820+sp_5900+sp_6020),
         fraction_6020 = sp_6020/(sp_5910+sp_5920+sp_5927+sp_5926+sp_6000+sp_5820+sp_5900+sp_6020)) %>% 
    dplyr::select(-sp_5910,-sp_5920,-sp_5927,-sp_5926,-sp_6000,-sp_5820,-sp_5900,-sp_6020)

hist(data_6049_fraction$fraction_6000)
hist(data_6049_fraction$fraction_5910)
hist(data_6049_fraction$fraction_5920)

data_6049 <- data_northsea5 %>% 
  filter(euring_species_code == 6049) %>% 
  left_join(data_6049_fraction, by = c("season", "grid_50id" )) %>% 
  mutate("5910" = number*fraction_5910, 
         "5920" = number*fraction_5920,
         "5927" = number*fraction_5927,
         "5926" = number*fraction_5926, 
         "6000" = number*fraction_6000,
         "5820" = number*fraction_5820,
         "5900" = number*fraction_5900,
         "6020" = number*fraction_6020) %>% 
  dplyr::select(-euring_species_code, -number, -fraction_5910,-fraction_5920,-fraction_5927,-fraction_5926,-fraction_6000,-fraction_5820,-fraction_5900,-fraction_6020) %>% 
   mutate(longitude = unlist(map(.$geometry,1)),
         latitude = unlist(map(.$geometry,2))) %>% 
  st_drop_geometry() %>% 
  pivot_longer(cols = c("5910":"5920","5927","5926","6000","5820","5900","6020"), 
               names_to = "euring_species_code", values_to = "number") %>% 
  mutate(euring_species_code = as.numeric(euring_species_code))%>% 
  st_as_sf(., coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")

# check
data_northsea5 %>% 
  st_drop_geometry() %>% 
  filter(euring_species_code == 6049) %>% 
  summarize(n(),length(unique(obskey)),unique(plumage))
length(unique(data_6049$obskey))*8
dim(data_6049)


# Combine data 

data_northsea6 <- data_northsea5 %>% 
  filter(euring_species_code %notin% c(6009,6049)) %>%
  bind_rows(data_6009,data_6049)


rm(data_northsea5,data_6009,data_6049,data_6009_fraction,data_6049_fraction)




#* !!!!!Samenvoegen Visdief/Noordse stern ---- 


# Visdief en noordse stern samenvoegen, Visdief (6150), Noordse stern (6160) als 6169 (visdief/Noordse stern)
# 6259 (Stern sp ) en 6285 (unidentified tern) omzetten naar code 6169 (visdief/Noordse stern)

data_northsea7 <- data_northsea6 %>% 
  mutate(euring_species_code = if_else(euring_species_code %in% c(6150,6160,6259,6285), 6169, euring_species_code))

data_northsea7 %>% filter(euring_species_code==6150) 
rm(data_northsea6)


#* Splitsen 849 Aalschover sp. in Kuifaalschover (800)/Aalschover (720) ---------

data_849_fraction<- data_northsea7 %>% 
  st_drop_geometry() %>% 
  filter( euring_species_code %in% c(800, 720)) %>% 
  group_by(season, grid_50id, euring_species_code) %>% 
  summarise(sum_number = sum(number)) %>% 
  pivot_wider(names_from = euring_species_code, values_from = sum_number ) %>% 
  rename(sp_800 = '800',sp_720 = '720') %>% 
  mutate(sp_800 = replace_na(sp_800,0),
         sp_720 = replace_na(sp_720,0), 
         fraction_800 = sp_800/(sp_800+sp_720),
         fraction_720 = sp_720/(sp_800+sp_720)) %>% 
  dplyr::select(-sp_800, -sp_720)

hist(data_849_fraction$fraction_800)
hist(data_849_fraction$fraction_720)

data_849 <- data_northsea7 %>% 
  filter(euring_species_code == 849) %>% 
  left_join(data_849_fraction, by = c("season", "grid_50id" )) %>% 
  mutate("800" = number*fraction_800, "720" = number* fraction_720) %>% 
  dplyr::select(-euring_species_code, -number, -fraction_800, -fraction_720) %>% 
   mutate(longitude = unlist(map(.$geometry,1)),
         latitude = unlist(map(.$geometry,2))) %>% 
  st_drop_geometry() %>% 
  pivot_longer(cols = c("800":"720"), 
               names_to = "euring_species_code", values_to = "number") %>% 
  mutate(euring_species_code = as.numeric(euring_species_code)) %>% 
  st_as_sf(., coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")



data_northsea8 <- data_northsea7 %>% 
  filter(euring_species_code %notin% c(849)) %>%
  bind_rows(data_849)

rm(data_849,data_northsea7,data_849_fraction)


#* Samenvoegen duikers 	Roodkeelduiker (20), parelduiker (30) als Gavia sp. (59) --------

# 14-10-2022
# In an earlier version of KEC we merged euring code 20 and 30 into 59
# In the new assignment we were ask to show only density maps of red-throated divers (20)
# Therefore, we will split 59 into 20 and 30

#data_northsea9 <- data_northsea8 %>% 
#  mutate(euring_species_code = if_else(euring_species_code %in% c(20,30), 59, euring_species_code))


data_59_fraction<- data_northsea8 %>% 
  st_drop_geometry() %>% 
  filter( euring_species_code %in% c(20, 30)) %>% 
  group_by(season, grid_50id, euring_species_code) %>% 
  summarise(sum_number = sum(number)) %>% 
  pivot_wider(names_from = euring_species_code, values_from = sum_number ) %>% 
  rename(sp_20 = '20',sp_30 = '30') %>% 
  mutate(sp_20 = replace_na(sp_20,0),
         sp_30 = replace_na(sp_30,0), 
         fraction_20 = sp_20/(sp_20+sp_30),
         fraction_30 = sp_30/(sp_20+sp_30)) %>% 
  dplyr::select(-sp_20, -sp_30)

hist(data_59_fraction$fraction_20)
hist(data_59_fraction$fraction_30)

data_59 <- data_northsea8 %>% 
  filter(euring_species_code == 59) %>% 
  left_join(data_59_fraction, by = c("season", "grid_50id" )) %>% 
  mutate("20" = number*fraction_20, "30" = number* fraction_30) %>% 
  dplyr::select(-euring_species_code, -number, -fraction_20, -fraction_30) %>% 
   mutate(longitude = unlist(map(.$geometry,1)),
         latitude = unlist(map(.$geometry,2))) %>% 
  st_drop_geometry() %>% 
  pivot_longer(cols = c("20":"30"), 
               names_to = "euring_species_code", values_to = "number") %>% 
  mutate(euring_species_code = as.numeric(euring_species_code)) %>% 
  st_as_sf(., coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")

# check
data_northsea8 %>% 
  st_drop_geometry() %>% 
  filter(euring_species_code == 59) %>% 
  summarize(n(),length(unique(obskey)),unique(plumage))
length(unique(data_59$obskey))*2
dim(data_59)

data_northsea9 <- data_northsea8 %>% 
  filter(euring_species_code %notin% c(59)) %>%
  bind_rows(data_59)


rm(data_northsea8,data_59,data_59_fraction)

#* Split 5709 unidentified skua in middelste jager (5660), kleine jager (5670), kleinste jager (5680)----------

data_5709_fraction<- data_northsea9 %>% 
  st_drop_geometry() %>% 
  filter( euring_species_code %in% c(5660, 5670,5680)) %>% 
  group_by(season, grid_50id, euring_species_code) %>% 
  summarise(sum_number = sum(number)) %>% 
  pivot_wider(names_from = euring_species_code, values_from = sum_number ) %>% 
  rename(sp_5660 = '5660',sp_5670 = '5670',sp_5680 = '5680') %>% 
  mutate(sp_5660 = replace_na(sp_5660,0),
         sp_5670 = replace_na(sp_5670,0),
         sp_5680 = replace_na(sp_5680,0), 
         fraction_5660 = sp_5660/(sp_5660+sp_5670+sp_5680),
         fraction_5670 = sp_5670/(sp_5660+sp_5670+sp_5680),
         fraction_5680 = sp_5680/(sp_5660+sp_5670+sp_5680)) %>% 
  dplyr::select(-sp_5660,-sp_5670,-sp_5680)

hist(data_5709_fraction$fraction_5680)
hist(data_5709_fraction$fraction_5670)

data_5709 <- data_northsea9 %>% 
  filter(euring_species_code == 5709) %>% 
  left_join(data_5709_fraction, by = c("season", "grid_50id" )) %>% 
  mutate("5660" = number*fraction_5660, "5670" = number* fraction_5670, "5680" = number* fraction_5680) %>% 
  dplyr::select(-euring_species_code, -number, -fraction_5660, -fraction_5670, -fraction_5680) %>% 
   mutate(longitude = unlist(map(.$geometry,1)),
         latitude = unlist(map(.$geometry,2))) %>% 
  st_drop_geometry() %>% 
  pivot_longer(cols = c("5660","5670","5680" ), 
               names_to = "euring_species_code", values_to = "number") %>% 
  mutate(euring_species_code = as.numeric(euring_species_code))%>% 
  st_as_sf(., coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")

data_northsea10 <- data_northsea9 %>% 
  filter(euring_species_code %notin% c(5709)) %>%
  bind_rows(data_5709)

rm(data_5709,data_northsea9,data_5709_fraction)

# Even opzij zetten
data_northsea_speciesmerged <- data_northsea10 %>%  
  #mutate(longitude = unlist(map(.$geometry,1)),
  #       latitude = unlist(map(.$geometry,2))) %>% 
  st_drop_geometry() 

data_northsea_speciesmerged %>% summarise_all(~ sum(is.na(.))) 
str(data_northsea_speciesmerged)

# For step 5 & 6 we do not need geometry, so we remove it from the dataset to speed things up
data_northsea10 <- data_northsea10 %>% 
     #mutate(longitude = unlist(map(.$geometry,1)),
    #     latitude = unlist(map(.$geometry,2))) %>% 
  st_drop_geometry() 
# Maps for species or seasons that do not convert using Inverse Distance Weighting
# Not a single model converts for the Skua

# Look for OpenStreetMap monochrome
# Check out https://gis.stackexchange.com/questions/378369/tmap-openstreetmap-basemap-in-greyscale


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
library(ggmap)

# Setwd()
getwd()
setwd("C:/Users/0_2023_bird_map/") # Define 

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



# map
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
Europe_utm31 <- Europe %>% st_transform(32631)
plot(Europe_utm31$geometry)

GRID_shape <- st_read("GRID_north_sea.shp")

Wadden <- st_read("Waddensea_shp/worldheritagemarineprogramme.shp")
st_crs(Wadden)
Wadden <- Wadden %>% st_transform(32631)


#info_grid 5*5 km
load("Data/JT_grid.Rdata") 

raster_utm31 <- rasterFromXYZ(info_grid,crs = 32631)
raster_latlong  <- projectRaster(raster_utm31,crs = 4326)

# put in different crs
info_grid_latlong <- st_as_sf(info_grid, coords = c("x","y"), 
                     crs = 32631, agr = "constant")

info_grid_latlong <- info_grid_latlong %>% st_transform(4326)
info_grid_latlong <- info_grid_latlong %>% 
  mutate(longitude = unlist(map(.$geometry,1)),
         latitude = unlist(map(.$geometry,2))) %>%  st_drop_geometry() 

# Adding grids with larger grid size, 10km2, 20km2, 50km2

raster_utm31 <- rasterFromXYZ(info_grid,crs = 32631)

# Make raster 10*10 instead of 5*5
res(raster_utm31)
raster_utm31_10_10km <- aggregate(raster_utm31, fact=2)
res(raster_utm31_10_10km)
raster_utm31_20_20km <- aggregate(raster_utm31, fact=4)
res(raster_utm31_20_20km)

# omzetten naar dataset met x en y
info_grid_10_10km <- cbind(xyFromCell(raster_utm31_10_10km, 1:ncell(raster_utm31_10_10km)), values(raster_utm31_10_10km))
info_grid_10_10km <- as.data.frame(info_grid_10_10km)
info_grid_10_10km <- info_grid_10_10km %>% dplyr::select(c(x,y))

info_grid_20_20km <- cbind(xyFromCell(raster_utm31_20_20km, 1:ncell(raster_utm31_20_20km)), values(raster_utm31_20_20km))
info_grid_20_20km <- as.data.frame(info_grid_20_20km)
info_grid_20_20km <- info_grid_20_20km %>% dplyr::select(c(x,y))
  
  
# Toevoegen GRID_ID
info_grid_10_10km <- info_grid_10_10km %>% mutate(GRID_ID = 1:n())
info_grid_20_20km <- info_grid_20_20km %>% mutate(GRID_ID = 1:n())


# Add right colors.
color_numbers <- c("0" = "skyblue4",
                   "0-0.5"="steelblue",
                   "0.5-1"="cadetblue3",
                   "1-2"="aquamarine3",
                   "2-4"="darkseagreen3",
                   "4-8"="darkseagreen2",
                   "8-16"="darkseagreen1",
                   "16-32"="khaki",
                   "32-64"="lightgoldenrod1",
                   "64-128" = "moccasin",
                   "128-256"="tan1",
                   "256-512" ="orange",
                   "512-1024"="tomato",
                   "1024-2048" = "tomato3",
                   "2048-4096"="tomato4")




# 1. Load bird data ---------
bird_data <- readRDS("Input_data/data_seabirds_covariates_24_10_2023_corrected.rds")

str(bird_data)


bird_data <- bird_data %>% dplyr::select(!GRID_ID)

# Add time period
bird_data$periodc <- cut(bird_data$Year_bimonthly, breaks=c(1999,2005, 2010, 2015, 2020))
bird_data <- bird_data %>% filter(!is.na(periodc))


 # IDW (Inverse Distance Weighted) Great Skua-------

coordinates(info_grid_10_10km) <- ~ x+y  # spatial reference of the data
proj4string(info_grid_10_10km) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs" 
#"WGS 84 / UTM zone 31N",EPSG:32631
gridded(info_grid_10_10km) <- TRUE  
extent(info_grid_10_10km)

range(info_grid_10_10km$x);range(info_grid_10_10km$y) # 58823.81+2500 --> info + 2500 = x van idw

# Settings Jan-Tjalling
# param_dict_original = { 'DATA_TYPE' : 5, 'EXTRA' : '', 
# 'INPUT' : xy_layer_name + " " + lyr_name_start, 
# 'MAX_POINTS' : 15, 'MIN_POINTS' : 5, 'NODATA' : -9.9, 
# 'OPTIONS' : 'COMPRESS=NONE|BIGTIFF=IF_NEEDED', 'OUTPUT' : idw_out_str.replace(idw_out_ext, "_NN" + idw_out_ext), #'POWER' : 2, 'RADIUS' : 317217, 'SMOOTHING' : 0, 'Z_FIELD' : 'mean_densi' }

# Run interpolation for every season in loop
GS <- bird_data %>% filter(english_name %in% "Great Skua",platform %in% "aerial",CntryCd %in% "NL")

GS$Bimonthly <- factor(GS$Bimonthly,levels=c("oct-nov","dec-jan","feb-mrch","apr-may","jun-jul","aug-sep"))

DF.idw <- data.frame()
pred.idw <- data.frame()
#i <- "dec-jan"

for(j in unique(GS$periodc)){

  GS_2 <- GS %>% filter(periodc == j) 

for (i in unique(GS$Bimonthly) ){
  
  temp <- GS_2 %>% filter(Bimonthly == i) 
  temp <- temp %>% dplyr::select(x_utm,y_utm,dens) 
  temp <- temp[order(temp$x_utm,temp$y_utm),] #ascending
  coordinates(temp) =~ x_utm+y_utm
  proj4string(temp) <- "+proj=utm +zone=31 +datum=WGS84 +units=m +no_defs"
  
  idw.temp <- gstat::idw(formula = dens ~ 1, locations = temp, newdata=info_grid_10_10km, idp=2.0, nmax=15,nmin=5,maxdist = 317217) # What to do here...?
  
  DF.temp <- as.data.frame(idw.temp)
  DF.temp <- DF.temp %>% rename(x_utm=x,y_utm=y)
  DF.temp$Bimonthly <- i 
  DF.temp$periodc <- j

  DF.idw <- bind_rows(DF.idw,DF.temp)
  rm(DF.temp,idw.temp,temp)
  
  
  
  

   }

#GS_2 <- left_join(GS_2,DF.idw,by=c("x_utm","y_utm","Bimonthly","periodc"))
#pred.idw <- bind_rows(pred.idw,GS_2)
rm(GS_2)

} 


str(DF.idw)

range(DF.idw$var1.pred,na.rm=T)


DF.idw <- st_as_sf(DF.idw, coords = c("x_utm","y_utm"), 
                     crs = 32631, agr = "constant")
GRID_shape <- GRID_shape %>% dplyr::select(GRID_ID,CntryCd,SubArea,geometry)
Wadden <- Wadden %>% dplyr::select(full_name)

data_join <- st_join(DF.idw, GRID_shape, join = st_intersects) 
dim(data_join)
data_join <- st_join(data_join, Wadden, join = st_intersects) 



pred <- data_join %>% 
  mutate(x_utm = unlist(map(.$geometry,1)),
         y_utm = unlist(map(.$geometry,2))) %>%  st_drop_geometry() %>% 
  mutate(CntryCd = ifelse(is.na(CntryCd),"NL",CntryCd))   
  
 
plot_continious <- pred %>% 
    filter(is.na(full_name)) %>% 
    filter(CntryCd %in% "NL") %>% 
    dplyr::mutate(Bimonthly = factor(Bimonthly,levels = c("dec-jan","feb-mrch","apr-may","jun-jul","aug-sep","oct-nov"))) %>% 
    ggplot() + 
    geom_tile(aes(x=x_utm,y=y_utm,col=var1.pred,fill=var1.pred))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    scale_fill_viridis_c(name = as.expression(bquote("Density"~(ind/km^2)~"")),guide = guide_legend()) +
    scale_color_viridis_c(name = as.expression(bquote("Density"~(ind/km^2)~"")),guide = guide_legend())+ 
  theme_s()+
    coord_sf(xlim=c(450000, 800000),ylim=c(5650000,6200000))+
  ggtitle(paste("Inversed distance weighting"),
          subtitle = paste("maximum density =", round(max(pred$var1.pred,na.rm=T),digits = 2))
  )+
  theme(axis.title = element_blank(),legend.position = "bottom")+
  facet_grid(periodc~Bimonthly)

plot(plot_continious)

ggsave(plot_continious,file=paste0("Prediction/Great_Skua_IDW/Great_Skua_IDW_20_20km2.png"), width = 30, height = 30, units = 'cm')



pred <- pred %>% 
  mutate(`Predicted (nr/km2)` = case_when(
    var1.pred <= 0.001                   ~ "0",
    var1.pred > 0.001 & var1.pred < 0.5  ~ "0-0.5",
    var1.pred >= 0.5 & var1.pred < 1  ~ "0.5-1",
    var1.pred >= 1.0 & var1.pred < 1.10  ~  "1-1",
    var1.pred >= 1.10 & var1.pred < 2    ~ "1-2" ,
    var1.pred >= 2 & var1.pred < 4    ~ "2-4" ,
    var1.pred >= 4 & var1.pred < 8  ~ "4-8" ,
    var1.pred >= 8 & var1.pred < 16 ~  "8-16",
    var1.pred >= 16 & var1.pred < 32 ~  "16-32",
    var1.pred >= 32 & var1.pred < 64 ~  "32-64",
    var1.pred >= 64 & var1.pred < 128 ~  "64-128",
    var1.pred >= 128 & var1.pred < 256 ~  "128-256"))

pred$`Predicted (nr/km2)` <- factor(pred$`Predicted (nr/km2)`,levels=c("0","0-0.5","0.5-1","1-1","1-2","2-4","4-8","8-16","16-32","32-64","64-128","128-256"))  

IDW_plot <- pred %>% 
  filter(is.na(full_name)) %>% 
    filter(CntryCd %in% "NL") %>% 
    dplyr::mutate(Bimonthly = factor(Bimonthly,levels = c("dec-jan","feb-mrch","apr-may","jun-jul","aug-sep","oct-nov"))) %>% 
  ggplot() + 
  theme_s()+
  geom_point(aes(x=x_utm,y=y_utm,col=`Predicted (nr/km2)`,fill=`Predicted (nr/km2)`),size=4,shape=15) +
  geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
  coord_sf(xlim=c(450000, 800000),ylim=c(5650000,6200000))+
  scale_color_manual(values = color_numbers,name = as.expression(bquote("Density"~(ind/km^2)~"")))+
    scale_fill_manual(values = color_numbers,name = as.expression(bquote("Density"~(ind/km^2)~"")))+ 
  facet_grid(periodc~Bimonthly)+
  ggtitle(paste("Inversed distance weighting"),
          subtitle = paste("maximum density =", round(max(pred$var1.pred,na.rm=T),digits = 2))
  )+
  theme(axis.title = element_blank(),legend.position = "bottom")+
  facet_grid(periodc~Bimonthly)
  

ggsave(IDW_plot,file=paste0("Prediction/Great_Skua_IDW/Great_Skua_IDW_Category_20_20km2.png"), width = 30, height = 30, units = 'cm')

rm(DF.idw_combine,IDW_plot,DF.idw)


pred <- pred %>% 
  filter(is.na(full_name)) %>% 
    filter(CntryCd %in% "NL") 

saveRDS(pred,paste0("Prediction/Great_Skua_IDW/Great_Skua_IDW_Category_20_20km2.rds"))


# And seperate graphs per period

iseason <- unique(pred$Bimonthly)
ispe1 <- "Great_Skua_IDW"
model_type <- "IDW"
prediction_path <- "prediction/"

for(i in iseason){ 
  
temp <- pred %>% filter(Bimonthly == i)  

f1 <- temp %>%  
    ggplot() + 
    geom_tile(aes(x=x_utm,y=y_utm,col=var1.pred,fill=var1.pred))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    scale_fill_viridis_c(na.value = "yellow", limits = c(0, quantile(temp$var1.pred, 0.995)),name = as.expression(bquote("Estimated density"~(ind/km^2)~"")),guide = guide_legend()) +
    scale_color_viridis_c(na.value = "yellow", limits = c(0, quantile(temp$var1.pred, 0.995)),name = as.expression(bquote("Estimated density"~(ind/km^2)~"")),guide = guide_legend())+ 
  theme_s()+
     coord_sf(xlim=c(450000, 800000),ylim=c(5650000,6200000))+
  ggtitle(paste("Prediction (fixed effects + all random effects)",i),
          subtitle = paste("maximum estimated density =", round(max(temp$var1.pred)))
  )+
    theme(axis.title = element_blank(),legend.position = "bottom")

f2 <- facet(f1, facet.by = c(mystrata), ncol = 4, panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) 
plot(f2)

ggsave(f2, file=paste0(prediction_path,ispe1,"/Great_skua_",i, "_prediction_", model_type, "_10_10km2.png"), width = 32, height = 20, units = 'cm')


# Or categories
temp <- temp %>% 
   mutate(Est_categories = case_when(var1.pred <= 0.001  ~ "0",
                               var1.pred > 0.001 & var1.pred <=0.5 ~"0-0.5",
                               var1.pred > 0.5 & var1.pred <=1 ~"0.5-1",
                               var1.pred > 1 & var1.pred <=2 ~"1-2",
                               var1.pred > 2 & var1.pred <=4 ~"2-4",
                               var1.pred > 4 & var1.pred <=8 ~"4-8",
                               var1.pred > 8 & var1.pred <=16 ~"8-16",
                               var1.pred > 16 & var1.pred <=32 ~"16-32",
                               var1.pred > 32 & var1.pred <=64 ~"32-64",
                               var1.pred > 64 & var1.pred <=128 ~"64-128",
                               var1.pred > 128 & var1.pred <=256 ~"128-256",
                               var1.pred > 256 & var1.pred <=512 ~"256-512",
                               var1.pred > 512 & var1.pred <=1024 ~"512-1024",
                               var1.pred > 1024 & var1.pred <=2048 ~"1024-2048",
                               var1.pred > 2048 & var1.pred <=4096 ~"2048-4096"))  
temp <- temp %>% 
  mutate(Est_categories = factor(Est_categories,levels = c("0","0-0.5","0.5-1", "1-2","2-4","4-8","8-16","16-32","32-64","64-128","128-256","1024-2048","2048-4096" )))


f1 <- temp %>% 
    #filter(CntryCd %in% "NL") %>% 
    ggplot() + 
    geom_tile(aes(x=x_utm,y=y_utm,col=Est_categories,fill=Est_categories))+
    scale_color_manual(values = color_numbers,name = as.expression(bquote("Estimated density"~(ind/km^2)~"")))+
    scale_fill_manual(values = color_numbers,name = as.expression(bquote("Estimated density"~(ind/km^2)~"")))+  
  geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
  theme_s()+
    coord_sf(xlim=c(450000, 800000),ylim=c(5650000,6200000))+
    ggtitle(paste("Prediction (fixed effects + all random effects)",i),
          subtitle = paste("maximum estimated density =", round(max(temp$var1.pred))))+
    theme(axis.title = element_blank(),legend.position = "bottom")

f2 <- facet(f1, facet.by = c(mystrata), ncol = 4, panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) 
plot(f2)

ggsave(f2, file=paste0(prediction_path,ispe1,"/Great_skua_",i, "_prediction_categories_", model_type, "_10_10km2.png"), width = 32, height = 20, units = 'cm')



}



# END -------------------------------------------------------------------------------------------------







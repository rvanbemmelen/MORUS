## this is a script template to run the prediction maps and create datasets with standard deviations etc
## please copy paste this script in your own folder/or change the name and run it
## In this script you need to:

## 1) define your species and prepare the folder to store output

rm(list = ls())
library(dplyr)
library(ggplot2)
library(sdmTMB)
library(sdmTMBextra)
library(patchwork)
#library(INLA)
library(ggpubr)
library(viridis)
library(tidyverse)
library(sf)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggmap)


theme_s <-  function () { 
  theme_classic(base_size=12)%+replace% 
    theme(axis.text = element_text(color="grey"),
          axis.line = element_line(color="grey"),
          axis.ticks = element_line(color="grey"),
          legend.title = element_text(face = "bold"),
          strip.background = element_rect(fill = "white", colour = "grey"),
          strip.text = element_text(margin = margin(0.2,0,0.2,0, "cm")) )
}

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

# map
world <- ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
Europe_utm31 <- Europe %>% st_transform(32631)
plot(Europe_utm31$geometry)

Europe_latlong <- Europe %>% st_transform(4326)

# A grid for North Sea to cut out the right part of the data
GRID_shape <- st_read("GRID_north_sea.shp")
str(GRID_shape)

# A grid for Wadden Sea to cut out the right part of the data
Wadden <- st_read("Waddensea_shp/worldheritagemarineprogramme.shp")
st_crs(Wadden)
Wadden <- Wadden %>% st_transform(32631)

## list of all functions
#lsf.str("package:sdmTMB")
help(package = sdmTMB)

## bird species
ispe  <- "European_Herring_Gull"
ispe1 <- "Herring Gull"
ispe2 <- "Larus argentatus"
iplatform <- "aerial"

## Limit smoother shape
myk = 5

# DEfine to use either Year or Periodc
## year based model
#model_type <- "Year_bimonthly"
## period based model
model_type <- "periodc"

## path
project_path <- "C:/Users/0_2023_bird_map/" # Define 
script_path  <- paste0(project_path, "Script/")
data_path1   <- paste0(project_path, "Data/")
data_path    <- paste0(project_path, "Data/", ispe,"/")
fig_path     <- paste0(project_path, "Figure/", ispe, "/")
output_path  <- paste0(project_path, "model_output/", ispe, "/")
map_path     <- paste0(project_path, "Data/Prediction grid/")
prediction_path  <- paste0(project_path, "prediction/", ispe, "/")

setwd(script_path)
source(paste0(project_path, "Script/functions_model.r"))


#### 0. load data ----
## extra data processing are in extra_data_processing_gannet()
## data ready for modeling
load(file=paste0(data_path, "processed_data_", ispe, "_platform_", iplatform,"_for_modeling.RData"))
table(dat$season, dat$year)
dat$periodc <- cut(dat$Year_bimonthly, breaks=c(1999,2005, 2010, 2015, 2020))
## data description:
## dens_sqrt
## year since 2000
## covariates are standadized to help model converge, X.std, standardization mean/sd are also in data


## season
unique(dat$season)


# Checks --> should be TRUE
unique(dat$mean_depth1) == mean(dat$depth1)
unique(dat$sd_depth1) == sd(dat$depth1)




#### 1. Loop to create prediction maps per Bimonthly period ----

iseason <- unique(dat$season)
df_pred <- data.frame()

for(i in iseason){ 

dd      <- dat[dat$season == i,]

## response
dd$response <- dd$dens_sqrt


## mod
load(file=paste0(output_path, i, "_final_model_", model_type, ".RData"))
mod$formula

## mesh
cutoff <- 20
mesh <- make_mesh(dd, xy_cols = c("x_utm1", "y_utm1"), cutoff = cutoff)
mesh$mesh$n


isize    <- 10
ifishcut <- quantile(dat$Fishing_hour_average_2009_2020, prob=c(0.25, 0.5, 0.75,1))
## return processed data, yearlt model 2016-2020, period model full period
map1     <- prepare_prediction_map(model_type, map_path, i, isize=isize, ispe2, dd, ifishcut=ifishcut)

## fishing hour into categorical variable
#fishcut <- quantile(dd$Fishing_hour_average_2009_2020, prob=c(0.25, 0.5, 0.75,1))
map1$Fishing_hour_averagec <- cut(map1$Fishing_hour_mean, breaks=c(0,ifishcut))
summary(map1$Fishing_hour_mean)
table(map1$Fishing_hour_averagec, useNA="always")
map1 <- map1[!is.na(map1$Fishing_hour_averagec),]

#if (model_type == "periodc"){
#  
#}

if (model_type == "Year_bimonthly"){
  map1$yearc    <- factor(map1$Year_bimonthly, levels=unique(dd$Year_bimonthly))
}


## standardize variable
map1 <- data_standardization_map(dd, map1)

## check NA values
## coming from land locations?
#var_list1 <- c("depth1.std", "MudPercent.std", "Analysed_meanSST_Celsius.std","meanChlorophyll_sqrt.std", "Distance_big_breeding_site_km.std", "Distance_platform_sqrt.std")

var_list1 <- c("esa.std", "depth1.std", "SandPercent.std", "Analysed_meanSST_Celsius.std", "meanChlorophyll_sqrt.std","Fishing_hour_averagec","Distance_platform_sqrt.std")

sapply(map1[,var_list1], summary)
## exclude NA
map1 <- map1[!is.na(map1$meanChlorophyll_sqrt.std),]




class(mod$data[[mod$time]])
class(map1[[mod$time]])

if (model_type == "periodc") {
## for period data, 1999-2005 no data after deleting NA in fishing
## need to add a fake data otherwise error
table(map1$periodc)
map1 <- rbind(map1, map1[1,])
map1$periodc[nrow(map1)] <- "(1999,2005]"
pred  <- predict(mod, newdata = map1, 
                 type="link",
                 se_fit=FALSE,
                 re_form_iid = NULL,
                 re_form = NULL,
                 offset = NULL)
head(pred)
#map1 <- map1[-nrow(map1),]
pred <- pred[-nrow(pred),]
mystrata <- "periodc"
}



if (model_type != "periodc") {
  pred  <- predict(mod, newdata = map1, 
                   type="link",
                   se_fit=FALSE,
                   re_form_iid = NULL,
                   re_form = NULL,
                   offset = NULL)
  head(pred)
  mystrata <- "yearc"
}

dim(pred)


# Select Dutch part of the Sea with shapefile

pred_spatial <- st_as_sf(pred, coords = c("x_utm","y_utm"), 
                     crs = 32631, agr = "constant")
GRID_shape <- GRID_shape %>% dplyr::select(GRID_ID,CntryCd,SubArea,geometry)
Wadden <- Wadden %>% dplyr::select(full_name)

data_join <- st_join(pred_spatial, GRID_shape, join = st_intersects) 
dim(data_join)
data_join <- st_join(data_join, Wadden, join = st_intersects) 

pred <- data_join %>% 
  mutate(x_utm = unlist(map(.$geometry,1)),
         y_utm = unlist(map(.$geometry,2))) %>%  st_drop_geometry() 

# One block became 'NA' for no reason. Change into NL
pred <- pred %>%  
  dplyr::mutate(CntryCd = ifelse(is.na(CntryCd),"NL",CntryCd))

pred <- pred %>% filter(CntryCd %in% "NL",is.na(full_name)) %>% dplyr::select(!full_name)


# remove points in-land
dim(pred)
pred <- pred %>% 
  filter(!(x_utm <= 623823.9 & x_utm >= 623823.7 & y_utm <= 5812628 & y_utm >= 5812626)) %>% 
  filter(!(x_utm >= 553823.7 & y_utm <= 5740000)) %>% # los punt zeeland OS mond
  filter(!(x_utm >= 563823.8 & y_utm <= 5750000)) # punten zeeland
dim(pred)

f1 <- pred %>%  
    ggplot() + 
    geom_tile(aes(x=x_utm,y=y_utm,col=(exp(est))^2,fill=(exp(est))^2))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    scale_fill_viridis_c( trans = "sqrt",na.value = "yellow", limits = c(0, quantile(exp(pred$est)^2, 0.995)),name = as.expression(bquote("Estimated density"~(nr/km^2)~"")),guide = guide_legend()) +
    scale_color_viridis_c(trans = "sqrt",na.value = "yellow", limits = c(0, quantile(exp(pred$est)^2, 0.995)),name = as.expression(bquote("Estimated density"~(nr/km^2)~"")),guide = guide_legend())+ 
  theme_s()+
     coord_sf(xlim=c(450000, 800000),ylim=c(5650000,6200000))+
  ggtitle(paste("Prediction (fixed effects + all random effects)",i),
          subtitle = paste0(ispe1," maximum estimated density =", round(max(exp(pred$est))^2)))+
  theme(axis.title = element_blank(),legend.position = "bottom")
f2 <- facet(f1, facet.by = c(mystrata), ncol = 3, panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) 
plot(f2)

ggsave(f2, file=paste0(prediction_path,"Maps/Bimonthly/", ispe1,"_",i, "_prediction_", model_type, "_10_10km2.png"), width = 28, height = 20, units = 'cm')


# Or categories
pred <- pred %>% 
   mutate(Est_categories = case_when((exp(est))^2 <= 0.0001  ~ "0",
                               (exp(est))^2 > 0.0001 & (exp(est))^2 <=0.5 ~"0-0.5",
                               (exp(est))^2 > 0.5 & (exp(est))^2 <=1 ~"0.5-1",
                               (exp(est))^2 > 1 & (exp(est))^2 <=2 ~"1-2",
                               (exp(est))^2 > 2 & (exp(est))^2 <=4 ~"2-4",
                               (exp(est))^2 > 4 & (exp(est))^2 <=8 ~"4-8",
                               (exp(est))^2 > 8 & (exp(est))^2 <=16 ~"8-16",
                               (exp(est))^2 > 16 & (exp(est))^2 <=32 ~"16-32",
                               (exp(est))^2 > 32 & (exp(est))^2 <=64 ~"32-64",
                               (exp(est))^2 > 64 & (exp(est))^2 <=128 ~"64-128",
                               (exp(est))^2 > 128 & (exp(est))^2 <=256 ~"128-256",
                               (exp(est))^2 > 256 & (exp(est))^2 <=512 ~"256-512",
                               (exp(est))^2 > 512 & (exp(est))^2 <=1024 ~"512-1024",
                               (exp(est))^2 > 1024 & (exp(est))^2 <=2048 ~"1024-2048",
                               (exp(est))^2 > 2048 & (exp(est))^2 <=4096 ~"2048-4096"))  
unique(pred$Est_categories)
pred <- pred %>% 
  mutate(Est_categories = factor(Est_categories,levels = c("0","0-0.5","0.5-1", "1-2","2-4","4-8","8-16","16-32","32-64","64-128","128-256","1024-2048","2048-4096" )))


f1 <- pred %>% 
    filter(CntryCd %in% "NL") %>% 
    filter(!periodc %in% "(1999,2005]") %>% 
    ggplot() + 
    geom_tile(aes(x=x_utm,y=y_utm,col=Est_categories,fill=Est_categories))+
    scale_color_manual(values = color_numbers,name = as.expression(bquote("Estimated density"~(nr/km^2)~"")))+
    scale_fill_manual(values = color_numbers,name = as.expression(bquote("Estimated density"~(nr/km^2)~"")))+  
  geom_sf(data = Europe_utm31,aes(geometry=geometry))+    
  theme_s()+
  coord_sf(xlim=c(450000, 800000),ylim=c(5650000,6200000))+
  ggtitle(paste("Prediction (fixed effects + all random effects)",i),
          subtitle = paste0(ispe1))+
  theme(axis.title = element_blank(),legend.position = "bottom")

f2 <- facet(f1, facet.by = c(mystrata), ncol = 3, panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) 
plot(f2)

ggsave(f2, file=paste0(prediction_path,"Maps/Bimonthly/", ispe1,"_",i, "_prediction_categories_", model_type, "_10_10km2.png"), width = 28, height = 20, units = 'cm')



## fixed effect map
f1 <- pred %>%  
  filter(CntryCd %in% "NL") %>% 
  filter(!periodc %in% "(1999,2005]") %>% 
    ggplot() + 
    geom_tile(aes(x=x_utm,y=y_utm,col=exp(est_non_rf)^2,fill=exp(est_non_rf)^2))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
scale_fill_viridis_c( trans = "sqrt",na.value = "yellow", limits = c(0, quantile(exp(pred$est_non_rf)^2, 0.995)),name = as.expression(bquote("Estimated density"~(nr/km^2)~"")),guide = guide_legend()) +
scale_color_viridis_c(trans = "sqrt",na.value = "yellow", limits = c(0, quantile(exp(pred$est_non_rf)^2, 0.995)),name = as.expression(bquote("Estimated density"~(nr/km^2)~"")),guide = guide_legend())+ 
  theme_s()+
 coord_sf(xlim=c(450000, 800000),ylim=c(5650000,6200000))+
 ggtitle(paste("Prediction (fixed effects)",i),
          subtitle = paste0(ispe1," maximum estimated density =", round(max(exp(pred$est_non_rf))^2, digits = 2)))+
    theme(axis.title = element_blank(),legend.position = "bottom")

f3 <- facet(f1, facet.by = c(mystrata), ncol = 3, panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) 
plot(f3)

ggsave(f3, file=paste0(prediction_path,"Maps/Bimonthly/", ispe1,"_",i, "_prediction_fixed_only_", model_type, "_10_10km2.png"), width = 28, height = 20, units = 'cm')

## spatial random effect

f1 <- pred %>%  
  filter(CntryCd %in% "NL") %>% 
  filter(!periodc %in% "(1999,2005]") %>% 
    ggplot() + 
    geom_tile(aes(x=x_utm,y=y_utm,col=epsilon_st,fill=epsilon_st))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    scale_fill_gradient2() +
    scale_color_gradient2() +
    theme_s()+
    coord_sf(xlim=c(450000, 800000),ylim=c(5650000,6200000))+
    ggtitle(paste("Spatiotemporal random effects only",i),
          subtitle = paste0(ispe1))+
    theme(axis.title = element_blank(),legend.position = "bottom")

f3 <- facet(f1, facet.by = c(mystrata), ncol = 3, panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) 
plot(f3)

ggsave(f3, file=paste0(prediction_path,"Maps/Bimonthly/", ispe1,"_",i, "_prediction_random_only_", model_type, "_10_10km2.png"), width = 28, height = 20, units = 'cm')

df_pred <- bind_rows(df_pred,pred)
rm(pred)
rm(f1,f2,f3,pred_spatial,data_join)

}

str(df_pred)
unique(df_pred$Bimonthly)

df_pred <- df_pred %>% mutate(exp_est_km2 = exp(est)^2)

saveRDS(df_pred,paste0(prediction_path,"Datasets/DF_prediction",ispe1,"_data_periodc_20_20km2.rds"))







# 2. Plot with all the seasons and years ----------------

df_pred <- df_pred %>% 
  mutate(Est_categories = factor(Est_categories,levels = c("0","0-0.5","0.5-1", "1-2","2-4","4-8","8-16","16-32","32-64","64-128","128-256","1024-2048","2048-4096" )),
         Bimonthly = factor(Bimonthly,levels = c("oct-nov","dec-jan","feb-mrch","apr-may","jun-jul","aug-sep")))


# Max sign. Adjust maxv so that maxv_sign is shown in the graph
maxv_sign <- paste0(">",round(quantile(exp(df_pred$est)^2, 0.995)[[1]],digits=2))
maxv <- unname(quantile(exp(df_pred$est)^2, 0.9942)[[1]])

f1 <- df_pred %>%  
    ggplot() + 
    geom_tile(aes(x=x_utm,y=y_utm,col=exp_est_km2,fill=exp_est_km2))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
  scale_fill_viridis_c( trans = "sqrt",na.value = "yellow", limits = c(0, quantile(exp(df_pred$est)^2, 0.995)),
    labels = ~ ifelse(.x <= maxv, .x, maxv_sign),
    name = as.expression(bquote("Estimated density"~(nr/km^2)~"")),guide = guide_legend()) +
  scale_color_viridis_c( trans = "sqrt",na.value = "yellow", limits = c(0, quantile(exp(df_pred$est)^2, 0.995)),
    labels = ~ ifelse(.x < maxv, .x, maxv_sign),
    name = as.expression(bquote("Estimated density"~(nr/km^2)~"")),guide=guide_legend()) +
  theme_s()+
     coord_sf(xlim=c(450000, 800000),ylim=c(5650000,6200000))+
  ggtitle(paste("Prediction (fixed effects + all random effects)",ispe1))+
    theme(axis.title = element_blank(),legend.position = "bottom")+
    facet_grid(periodc~Bimonthly)  
  
plot(f1)

ggsave(f1, file=paste0(prediction_path,"Maps/", ispe1,"_prediction_", model_type, "_10_10km2.png"), width = 30, height = 30, units = 'cm')

f1 <- df_pred %>% 
    ggplot() + 
    geom_tile(aes(x=x_utm,y=y_utm,col=Est_categories,fill=Est_categories))+
    scale_color_manual(values = color_numbers,name = as.expression(bquote("Estimated density"~(nr/km^2)~"")))+
    scale_fill_manual(values = color_numbers,name = as.expression(bquote("Estimated density"~(nr/km^2)~"")))+  
  geom_sf(data = Europe_utm31,aes(geometry=geometry))+    
  theme_s()+
    coord_sf(xlim=c(450000, 800000),ylim=c(5650000,6200000))+
    ggtitle(paste("Prediction (fixed effects + all random effects)",ispe1))+
    theme(axis.title = element_blank(),legend.position = "bottom")+
    facet_grid(periodc~Bimonthly)  

plot(f1)

ggsave(f1, file=paste0(prediction_path,"Maps/", ispe1,"_prediction_categories_", model_type, "_10_10km2.png"), width = 30, height = 30, units = 'cm')



## fixed effect map
maxv_sign <- paste0(">",round(quantile(exp(df_pred$est_non_rf)^2, 0.995)[[1]],digits=2))
maxv <- unname(quantile(exp(df_pred$est_non_rf)^2, 0.99)[[1]])


f1 <- df_pred %>%  
  ggplot() + 
    geom_tile(aes(x=x_utm,y=y_utm,col=exp(est_non_rf)^2,fill=exp(est_non_rf)^2))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
  scale_fill_viridis_c( trans = "sqrt",na.value = "yellow", limits = c(0, quantile(exp(df_pred$est_non_rf)^2, 0.995)),
    labels = ~ ifelse(.x <= maxv, .x, maxv_sign),
    name = as.expression(bquote("Estimated density"~(nr/km^2)~"")),guide = guide_legend()) +
  scale_color_viridis_c( trans = "sqrt",na.value = "yellow", limits = c(0, quantile(exp(df_pred$est_non_rf)^2, 0.995)),
    labels = ~ ifelse(.x < maxv, .x, maxv_sign),
    name = as.expression(bquote("Estimated density"~(nr/km^2)~"")),guide=guide_legend()) +
theme_s()+
 coord_sf(xlim=c(450000, 800000),ylim=c(5650000,6200000))+
 ggtitle(paste("Prediction (fixed effects only)",ispe1))+
    theme(axis.title = element_blank(),legend.position = "bottom")+
    facet_grid(periodc~Bimonthly)  

plot(f1)

ggsave(f1, file=paste0(prediction_path,"Maps/", ispe1,"_prediction_fixed_only_", model_type, "_10_10km2.png"), width = 30, height = 30, units = 'cm')

## spatial random effect

f_spatial <- df_pred %>%  
    ggplot() + 
    geom_tile(aes(x=x_utm,y=y_utm,col=epsilon_st,fill=epsilon_st))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    scale_fill_gradient2() +
    scale_color_gradient2() +
    theme_s()+
    coord_sf(xlim=c(450000, 800000),ylim=c(5650000,6200000))+
    ggtitle(paste("Spatiotemporal random effects only",ispe1))+
    theme(axis.title = element_blank(),legend.position = "bottom")+
    facet_grid(periodc~Bimonthly)  

plot(f_spatial)
ggsave(f_spatial, file=paste0(prediction_path,"Maps/", ispe1,"_prediction_spatial_only_", model_type, "_10_10km2.png"), width = 30, height = 30, units = 'cm')



#### 3. prediction and CV----
## simulate posterior distribution to get CV
## from author: Predicting with missing years is done by adding fake years and removing
## them in the end, but I forgot to remove them for the nsim output option.
## sim needs to be done for each year seperately

# Make a loop here as well.

iseason <- unique(dat$Bimonthly)
df_sim <- data.frame()
df_simulation <- data.frame()

for(i in iseason){ 

dd      <- dat[dat$Bimonthly == i,]

## response
dd$response <- dd$dens_sqrt

## mod
load(file=paste0(output_path, i, "_final_model_", model_type, ".RData"))
mod$formula

## mesh
cutoff <- 20
mesh <- make_mesh(dd, xy_cols = c("x_utm1", "y_utm1"), cutoff = cutoff)
mesh$mesh$n

isize    <- 10
ifishcut <- quantile(dat$Fishing_hour_average_2009_2020, prob=c(0.25, 0.5, 0.75,1))
## return processed data, yearlt model 2016-2020, period model full period
map1     <- prepare_prediction_map(model_type, map_path, i, isize=isize, ispe2, dd, ifishcut=ifishcut)

## fishing hour into categorical variable
#fishcut <- quantile(dd$Fishing_hour_average_2009_2020, prob=c(0.25, 0.5, 0.75,1))
map1$Fishing_hour_averagec <- cut(map1$Fishing_hour_mean, breaks=c(0,ifishcut))
summary(map1$Fishing_hour_mean)
table(map1$Fishing_hour_averagec, useNA="always")
map1 <- map1[!is.na(map1$Fishing_hour_averagec),]

if (model_type == "Year_bimonthly"){
  map1$yearc    <- factor(map1$Year_bimonthly, levels=unique(dd$Year_bimonthly))
}

## standardize variable
map1 <- data_standardization_map(dd, map1)

var_list1 <- c("esa.std", "depth1.std", "SandPercent.std", "Analysed_meanSST_Celsius.std", "meanChlorophyll_sqrt.std","Fishing_hour_averagec","Distance_big_breeding_site_km.std","Distance_platform_sqrt.std")

## exclude NA
map1 <- map1[!is.na(map1$meanChlorophyll_sqrt.std),]


## pred all, CV of all, fixed effect, random effect, SD of random effects 
class(mod$data[[mod$time]])
class(map1[[mod$time]])


if (model_type == "periodc") {
## offset needs to be included if esa in the model!
## for period data, 1999-2005 no data after deleting NA in fishing
## need to add a fake data otherwise error
table(map1$periodc)
map1 <- rbind(map1, map1[1,])
map1$periodc[nrow(map1)] <- "(1999,2005]"
pred2  <- predict(mod, newdata = map1, 
                 type="link",
                 se_fit=FALSE,
                 re_form_iid = NULL,
                 re_form = NULL,
                 offset = NULL)
head(pred2)
#map1 <- map1[-nrow(map1),]
pred2 <- pred2[-nrow(pred2),]
mystrata <- "periodc"
}

if (model_type == "periodc") {
  nn <- nrow(map1)
 
  sim      <- predict(mod, newdata = map1, nsim = 1000,
                      type="link",
                      se_fit=FALSE,
                      re_form_iid = NULL,
                      re_form = NULL)
  sim <- sim[-nn,]
  map1 <- map1[-nn,]
  pred2$lwr <- apply(exp(sim), 1, quantile, probs = 0.025)
  pred2$upr <- apply(exp(sim), 1, quantile, probs = 0.975)
  pred2$sd <- round(apply(exp(sim), 1, function(x) sd(x)), 2)
  pred2$cv <- round(apply(exp(sim), 1, function(x) sd(x) / mean(x)), 2)
  sim <- as.data.frame(sim)
  sim$Bimonthly <- i
  
} 

Country <- df_pred %>% dplyr::select(x_utm,y_utm,CntryCd) %>% distinct()
dim(pred2)
pred2 <- left_join(pred2,Country, by = c("x_utm","y_utm"))
dim(pred2)
unique(pred2$CntryCd)

c1 <- pred2 %>%  
  filter(CntryCd %in% "NL") %>% 
  filter(!periodc %in% "(1999,2005]") %>% 
    ggplot() + 
    geom_tile(aes(x=x_utm,y=y_utm,col=cv,fill=cv))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    scale_fill_gradient2(low = "#132B43",high = scales::muted("red")) +
    scale_color_gradient2(low = "#132B43",  high = scales::muted("red")) +
    theme_s()+
    coord_sf(xlim=c(450000, 800000),ylim=c(5650000,6200000))+
    ggtitle(paste("Coefficient of variation",i))+
    theme(axis.title = element_blank(),legend.position = "bottom")

c3 <- facet(c1, facet.by = c(mystrata), ncol = 3, panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) 
plot(c3)

ggsave(c3, file=paste0(prediction_path,"Maps/Bimonthly/", ispe1,"_",i, "_cv_", model_type, "_10_10km2.png"), width = 28, height = 20, units = 'cm')

    
df_sim <- bind_rows(df_sim,pred2)
df_simulation <- bind_rows(df_simulation,sim)
rm(pred2,sim)

pred11 <- predict(mod, 
                  type="link",
                  se_fit=FALSE,
                  re_form_iid = NULL,
                  re_form = NULL)
head(pred11)

pred11$est1   <- exp(pred11$est)
pred11$diff1  <- (pred11$response-pred11$est1) ## (obs-est)^2
#pred11$diff1  <- pred11$response-pred11$est1
pred11$diff2  <- sqrt(abs(pred11$diff1))
pred11$diff2[pred11$diff1<0]  <- (-pred11$diff2)[pred11$diff1<0]

save(pred11, file=paste0(prediction_path,"Datasets/", i, "_prediction_", model_type, "_on_observation.RData"))

# Positive higher value mean that the observed values are higher than the estimates. 
range(pred11$diff1)
diff <- pred11 %>%  
  filter(!periodc %in% "(1999,2005]") %>% 
    filter(diff2 > 0.5 | diff2 < -0.5) %>% 
    ggplot() + 
    geom_point(aes(x=x_utm,y=y_utm,col=(diff2),fill=(diff2)))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    scale_fill_gradient2() +
    scale_color_gradient2() +
    theme_s()+
    coord_sf(xlim=c(450000, 800000),ylim=c(5650000,6200000))+
    ggtitle(paste("Difference observations and estimated density",iseason))+
    theme(axis.title = element_blank(),legend.position = "bottom")

diff2 <- facet(diff, facet.by = c(mystrata), ncol = 3, panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) 
plot(diff2)

ggsave(diff2, file=paste0(prediction_path,"Maps/Bimonthly/", ispe1,"_",i, "_difference_", model_type, "_10_10km2.png"), width = 28, height = 20, units = 'cm')

rm(diff2,pred11,mod,dd,map1)



}


c1_tot <- df_sim %>%  
  filter(CntryCd %in% "NL") %>% 
  filter(!periodc %in% "(1999,2005]") %>% 
  mutate(Bimonthly = factor(Bimonthly,levels = c("oct-nov","dec-jan","feb-mrch","apr-may","jun-jul","aug-sep"))) %>% 
    ggplot() + 
    geom_tile(aes(x=x_utm,y=y_utm,col=cv,fill=cv))+
    geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
    scale_fill_gradient2(low = "#132B43",high = scales::muted("red")) +
    scale_color_gradient2(low = "#132B43",  high = scales::muted("red")) +
    theme_s()+
    coord_sf(xlim=c(450000, 800000),ylim=c(5650000,6200000))+
    ggtitle(paste("Coefficient of variation",ispe1))+
    theme(axis.title = element_blank(),legend.position = "bottom")+
    facet_grid(periodc~Bimonthly)  

plot(c1_tot)
ggsave(c1_tot, file=paste0(prediction_path,"Maps/", ispe1,"_cv_", model_type, "_10_10km2.png"), width = 30, height = 30, units = 'cm')



dim(df_pred)
df_sim <- df_sim %>% dplyr::select(x_utm,y_utm,periodc,Bimonthly,lwr,upr,sd,cv) %>% distinct
str(df_sim)
df_pred2 <- left_join(df_pred,df_sim,by=c("x_utm","y_utm","periodc","Bimonthly"))
dim(df_pred)

saveRDS(df_pred2,paste0(prediction_path,"Datasets/DF_prediction_",ispe1,"_data_periodc_20_20km2.rds"))




# END -------------------------------------------------------------------------------------------------




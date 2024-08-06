## this is a script template to explore and prepare the data for the model
## please copy paste this script in your own folder/or change the name and run it
## In this script you need to:

## 1) define your species and prepare the folder to store output

rm(list=(ls()))

## script to process raw input data and explore data
library(ggplot2)
library(rmarkdown)
library(ggpubr)
library(sf)
library(rnaturalearth)

## bird species --> define/
ispe  <- "Atlantic_Puffin"
ispe1 <- "Atlantic Puffin"
iplatform <- "aerial"
#iplatform <- "both"

## path
project_path <- "C:/Users/0_2023_bird_map/" # Define 
script_path  <- paste0(project_path, "Script/")
data_path1   <- paste0(project_path, "Data/")
data_path    <- paste0(project_path, "Data/", ispe,"/")
fig_path     <- paste0(project_path, "Figure/", ispe, "/")
output_path  <- paste0(project_path, "Script_output/", ispe, "/")
out_filename <- paste0("01_0_data_exploration_", ispe, "_platform_", iplatform, ".docx")
out_filename1 <- paste0("01_2_relationships_among_covariates_", ispe, "_platform_", iplatform, ".docx")

setwd(script_path)
source(paste0(project_path, "Script/functions_model.r"))

#############################################
####### 0. load raw data
#############################################

## load raw data and some preprocessing and data transformation
## This dataset is created using the script '5_coupling_bird_data_with_covariates'

dat  <- readRDS(file=paste0(data_path1, "Bird_data with covariates/data_seabirds_covariates_24_10_2023_corrected.rds"))
dat  <- as.data.frame(dat)
head(dat)
names(dat)
table(dat$english_name)

## select species
dat  <- dat[dat$english_name == ispe1,]
dim(dat)
unique(dat$english_name)

## select survey platform data
## not done here yet, first have an overview of all NS space
## this helps to group the season
table(dat$platform, useNA="always")
if (iplatform == "aerial") {
  dat  <- dat[dat$platform %in% iplatform & dat$CntryCd == "NL", ]
}
plot(dat$lon, dat$lat)


#############################################
####### 1. some preliminary checking and data transformation   ------
#############################################
#old_mwtl,species_counted,
## old_mwtl column
table(dat$old_mwtl, useNA="always")

## checks
summary(dat$dens)
hist(sqrt(dat$dens))
summary(dat$datetime)
summary(dat$x_utm)
summary(dat$y_utm)
table(dat$SubArea, useNA="always")

## fisheries_active: only recorded when they see a bird
## so far not use it, maybe in hurdle model
summary(dat$dens[is.na(dat$fisheries_active)])
sum(dat$dens>0 & is.na(dat$fisheries_active))
table(dat$year[dat$dens>0 & is.na(dat$fisheries_active)])
table(dat$platform[dat$dens>0 & is.na(dat$fisheries_active)])

## InWindFarm, all NA are No
## not used
dat$InWindFarm[is.na(dat$InWindFarm)] <- "No"
table(dat$InWindFarm, useNA="always")
dat$InWindFarm <- factor(dat$InWindFarm)

## esa, log-transformed already
summary(dat$esa)
hist(dat$esa)

## month
dat$month <- factor(dat$month)
levels(dat$month)

## check NA
## depth and sedment time-invariant, other covariates could be missing years or areas
## with NA: meanChlorophyll, Average_Rotor.Diam, Analysed_meanSST_Celsius,distance_coast_avg,
## depth, SandPercent, MudPercent
## check covariates, missing values?
## esa, depth, MudPercent, Analysed_meanSST_Celsius, meanChlorophyll,Distance_breeding_site_km
## distance_coast_avg, 
## Nr_breeding_pairs, Fishing_hour_mean, Hour_cov
## Fishing_hour_mean: fishing hours (this is very coarse data…) which is calculated per year from 2008-2020, so many missing values
## Fishing_hour_average_2009_2020: averge of fishing_hour_mean
## Distance_km_closest_fishing_datapoint:
## Average_Rotor.Diam: how big the windfarm
## distance_coast_avg: NA, outside the raster, mostly north in DENmark, maybe use distance to colony
## breeding site: 
## some covariates are only useful for some month, e.g. distance to colony. 
## Nr_breeding_pairs: number of breeding pairs in the nearest breeding site, categorical
## Distance_big_breeding_site_km: distance to large breeding site(defined as >=X birds)

var_list1 <- c("esa", "depth", "MudPercent","Analysed_meanSST_Celsius", "meanChlorophyll", "Distance_breeding_site_km", "Distance_big_breeding_site_km","distance_coast_avg")
sapply(dat[,var_list1], summary)  ##--check any NA

var_list2 <- c("Fishing_hour_mean", "Fishing_hour_average_2009_2020", "fisheries_active")
sapply(dat[,var_list2], summary)  ##--check any NA


## convert datetime
nrow(dat[is.na(dat$datetime),])
nrow(dat)
table(dat$year, useNA="always")
table(dat$month, useNA="always")
#dat$yearc    <- lubridate::year(dat$datetime)
#dat$monthc   <- lubridate::month(dat$datetime)
#dat$quarter <- lubridate::quarter(dat$datetime)
#dat$yday    <- lubridate::yday(dat$datetime)

## data transformation
dat <- data_transformation(dat)
summary(dat$depth1_sqrt)

## the correlation between fishing hour and distance to dutch coast?

## save processed data:
## all seasons
save(dat, file=paste0(data_path, "processed_data_", ispe, "_platform_", iplatform,".RData"))

#############################################
####### 2. plot raw density   ------
#############################################
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Europe <- world[which(world$continent == "Europe"),]
Europe_utm31 <- Europe %>% st_transform(32631)
plot(Europe_utm31$geometry)
f1 <- ggplot(data=dat) + 
  geom_point(aes(x=x_utm,y=y_utm, col=dens), alpha=0.5)+
  geom_sf(data = Europe_utm31,aes(geometry=geometry))+  
  scale_fill_viridis_c(
    trans = "sqrt",
    # trim extreme high values to make spatial variation more visible
    na.value = "yellow", limits = c(0, quantile(dat$dens, 0.995)))+
  coord_sf(xlim=c(-100000, 900000),ylim=c(5500000,7000000))+
  facet_wrap(~month)
print(f1)

#############################################
####### 3. initial checking of covariates   ------
#############################################

load(file=paste0(data_path, "processed_data_", ispe, "_platform_", iplatform,".RData"))
out_filename
render("01_0_data_exploration.Rmd",
       output_dir = output_path,
       output_file = out_filename,
       encoding = "UTF-8")

#############################################
####### 4. define seasons   ------ 
#############################################
load(file=paste0(data_path, "processed_data_", ispe, "_platform_", iplatform,".RData"))

## define season
dat <- define_season_group1(dat)
unique(dat$season)
unique(dat$english_name)
save(dat, file=paste0(data_path, "processed_data_", ispe, "_platform_", iplatform,"_season.RData"))

#############################################
####### 5. check density distribution and esa   ------  
#############################################
load(file=paste0(data_path, "processed_data_", ispe, "_platform_", iplatform,"_season.RData"))
out_filename1 <- paste0("01_3_check_dens_esa_", ispe, "_platform_", iplatform, ".docx")
render("01_3_check_density_distribution_and_esa.Rmd",
       output_dir = output_path,
       output_file = out_filename1,
       encoding = "UTF-8")

#############################################
####### 6. check distance to breeding site   ------ 
#############################################
load(file=paste0(data_path, "processed_data_", ispe, "_platform_", iplatform,"_season.RData"))
out_filename3 <- paste0("01_4_check_distance_breeding_", ispe, "_platform_", iplatform, ".docx")
render("01_4_check_distance_to_breeding_site.Rmd",
       output_dir = output_path,
       output_file = out_filename3,
       encoding = "UTF-8")


#############################################
####### 7. run relationships among covariates  ------  
#############################################
#01_2_relatio nships_among_covariates.Rmd

load(file=paste0(data_path, "processed_data_", ispe, "_platform_", iplatform,"_season.RData"))
out_filename1
myseason <- unique(dat$season)
render("01_2_relationships_among_covariates.Rmd",
       output_dir = output_path,
       output_file = out_filename1,
       encoding = "UTF-8")

#############################################
####### 8. extra/final data processing   ------ 
#############################################
## exclude NA, species-specific
## depth
## year-month selection: WMTL since 2000, also define period
## esa, exclude extreme values?
## exclude extreme density?
load(file=paste0(data_path, "processed_data_", ispe, "_platform_", iplatform,"_season.RData"))

## any extra processing
dat <- extra_data_processing_gannet(dat)
table(dat$year)

## visualie spatial coverage per season
xlim <- range(dat$x_utm1)
ylim <- range(dat$y_utm1)
par(mfrow = c(2, 3))
iss <- "dec-jan"
plot(dat$x_utm1[dat$season == iss], dat$y_utm1[dat$season == iss], xlim=xlim, ylim=ylim, main=iss)
iss <- "feb-mrch"
plot(dat$x_utm1[dat$season == iss], dat$y_utm1[dat$season == iss], xlim=xlim, ylim=ylim, main=iss)
iss <- "apr-may"
plot(dat$x_utm1[dat$season == iss], dat$y_utm1[dat$season == iss], xlim=xlim, ylim=ylim, main=iss)
iss <- "jun-jul"
plot(dat$x_utm1[dat$season == iss], dat$y_utm1[dat$season == iss], xlim=xlim, ylim=ylim, main=iss)
iss <- "aug-sep"
plot(dat$x_utm1[dat$season == iss], dat$y_utm1[dat$season == iss], xlim=xlim, ylim=ylim, main=iss)
iss <- "oct-nov"
plot(dat$x_utm1[dat$season == iss], dat$y_utm1[dat$season == iss], xlim=xlim, ylim=ylim, main=iss)


plot(dat$x_utm1[dat$season == iss], dat$y_utm1[dat$season == iss], xlim=xlim, ylim=ylim)


## first apply exclusion of outlier, and then apply standardization
## prepare standardized covariates for modeling
dat <- data_standardization(dat)

## save data
save(dat, file=paste0(data_path, "processed_data_", ispe, "_platform_", iplatform,"_for_modeling.RData"))

#############################################
####### 9. define period?    ------
#############################################


#############################################
####### 10. final check density and covariate distributions   ------ 
#############################################

## visualize covariate distribution
## before and after standardization
library(Hmisc)
## data normalization
var_list1 <- c("esa", "depth1","depth1_sqrt","MudPercent","MudPercent_trans","Analysed_meanSST_Celsius", "meanChlorophyll", "Distance_big_breeding_site_km")
hist.data.frame(dat[,var_list1], main="aa")  
var_list2 <- c("esa", "esa.std", "depth1","depth1.std","MudPercent_trans","MudPercent.std",
               "Analysed_meanSST_Celsius", "Analysed_meanSST_Celsius.std",
               "meanChlorophyll", "meanChlorophyll.std",
               "Distance_big_breeding_site_km", "Distance_big_breeding_site_km.std")
iss <- "dec-jan"
iss <- "feb-mrch"
iss <- "apr-may"
iss <- "jun-jul"
iss <- "aug-sep"
iss <- "oct-nov"
hist.data.frame(dat[dat$season == iss,var_list2], main="aa")

hist.data.frame(dat[,c("dens","dens_sqrt")], main="aa")
hist.data.frame(dat[dat$dens>0,c("dens","dens_sqrt")], main="aa")
qq <- quantile(dat$dens, prob=c(0.99, 0.995, 0.999))



#############################################
####### 11. check spatial temporal   ------ 
#############################################
## check spatial of extreme
i=1
plot(dat$lon, dat$lat)
points(dat$lon[dat$dens>qq[i]], dat$lat[dat$dens>qq[i]], col="red")
## check temporal of extreme
sum(dat$dens==0)/nrow(dat)
qq  <- quantile(dat$dens, prob=c(0.95,0.99, 0.995, 0.999))
qq  <- quantile(dat$dens, prob=c(0.99, 0.995, 0.999))
par(mfrow = c(3, 2))
iss <- "dec-jan"
plot_extreme_value_temporal(dat[dat$season == iss,], qq)
iss <- "feb-mrch"
plot_extreme_value_temporal(dat[dat$season == iss,], qq)
iss <- "apr-may"
plot_extreme_value_temporal(dat[dat$season == iss,], qq)
iss <- "jun-jul"
plot_extreme_value_temporal(dat[dat$season == iss,], qq)
iss <- "aug-sep"
plot_extreme_value_temporal(dat[dat$season == iss,], qq)
iss <- "oct-nov"
plot_extreme_value_temporal(dat[dat$season == iss,], qq)

## sample spatial
iseason <- "dec-jan"
dd      <- dat[dat$season == iseason,]
f1 <- ggplot(dd, aes(x=x_utm1, y=y_utm1)) + 
  geom_point(size=0.1, alpha=0.5) +
  #scale_colour_gradientn(name = 'category', colours = c('blue', 'red'))
  #ggtitle("sandwich tern mean") + 
  #scale_color_viridis_d() + 
  scale_color_brewer(palette="Dark2") +
  theme(legend.position = "bottom") +
  theme_bw()+
  xlab("") + ylab("")
f2 <- facet(f1, facet.by = c("year"), scales = "fixed", panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14))
print(f2)

## number of segments
temp1 <- aggregate(dens ~ Year_bimonthly + season, FUN=length, data=dat)
f1 <- ggplot(temp1, aes(x=Year_bimonthly, y=dens)) + 
  geom_point()+
  geom_line()+
  #scale_colour_gradientn(name = 'category', colours = c('blue', 'red'))
  #ggtitle("sandwich tern mean") + 
  #scale_color_viridis_d() + 
  scale_color_brewer(palette="Dark2") +
  theme(legend.position = "bottom") +
  theme_bw()+
  xlab("") + ylab("")
f2 <- facet(f1, facet.by = c("season"), scales = "fixed", panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14))
print(f2)

#############################################
####### 12. explore SPDE  ------
#############################################
load(file=paste0(data_path, "processed_data_", ispe, "_platform_", iplatform,"_for_modeling.RData"))
out_filename4 <- paste0("03_explore_SPDE_", ispe, "_platform_", iplatform, ".docx")
render("03_explore_SPDE_mesh_2.Rmd",
       output_dir = output_path,
       output_file = out_filename4,
       encoding = "UTF-8")

#############################################
####### 3. make gif of annual spatial distribution 
#############################################
## list file names and read in
library(magick)
imgs <- list.files("C:\\Users\\chen072\\OneDrive - Wageningen University & Research\\0_2023_bird_map\\Figure\\", full.names = TRUE)
imgs1 <- imgs[grep("plot04_spatial_both", imgs)]
img_list <- lapply(imgs1, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 1)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = paste0(fig_path, "plot_04_spatial__both_annual.gif"))

#file.copy(from, to, overwrite = recursive, recursive = FALSE,
#          copy.mode = TRUE, copy.date = FALSE)

#file.remove(...)
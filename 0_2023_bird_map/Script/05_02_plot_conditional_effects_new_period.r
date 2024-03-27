## this is a script template to run the conditional effects of the final models and plot them
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
library(visreg)

## function
myplotfunc <- function(myp, ispe, iseason, covname, titlename) {
  if (!covname %in% c("yearc", "periodc")) {
    f2 <- ggplot(myp, aes(x = get(covname), y = est) ) +
      geom_line(linewidth=2, col="red") +
      geom_ribbon(aes(ymin = est-1.96*est_se, ymax = est+1.96*est_se), alpha = 0.2,col=NA, fill="red") +
      #geom_point(aes(x = get(covname), y = visregRes), data = dplot$res, size = 1, alpha = 0.1)+
      ylab("linear predictor")+
      xlab(covname)+
      theme_bw()+
      ggtitle(paste(ispe, iseason, titlename, ", conditional effect", sep=":"))
    print(f2)
  }
  if (covname %in% c("yearc", "periodc", "Fishing_hour_averagec")) {
    f2 <- ggplot(myp, aes(x = get(covname), y = est) ) +
      #geom_boxplot(color="red", linewidth=2)+
      #geom_line(color="red", linewidth=2) +
      geom_point(color="red", size=2)+
      geom_errorbar(aes(ymin=est-1.96*est_se, ymax=est+1.96*est_se), width=.2,color="red",
                    position=position_dodge(0.05))+
      #geom_point(aes(x = get(covname), y = visregRes), data = dplot$res, size = 1, alpha = 0.1)+
      ylab("linear predictor")+
      xlab(covname)+
      theme_bw()+
      ggtitle(paste(ispe, iseason, titlename, " conditional effect", sep=":"))
    print(f2)
  }
  ggsave(f2, file=paste0(prediction_path, "conditional_effect/", ispe1,"_",iseason, "_conditional_effect_", titlename, ".png"), width = 20, height = 20, units = 'cm')
}

## bird species
ispe  <- "European_Herring_Gull"
ispe1 <- "Herring Gull"

#ispe  <- "Black-legged_Kittiwake"
#ispe1 <- "Black-legged Kittiwake"

iplatform <- "aerial"


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


#### 1. load data ----
## extra data processing are in extra_data_processing_gannet()
## data ready for modeling
load(file=paste0(data_path, "processed_data_", ispe, "_platform_", iplatform,"_for_modeling.RData"))
table(dat$season, dat$year)
dat$periodc <- cut(dat$Year_bimonthly, breaks=c(1999,2005, 2010, 2015, 2020))


## save all result in p
p <- NA
## dont change here
myvar <- c("depth1.std", "esa.std", "meanChlorophyll_sqrt.std", "Analysed_meanSST_Celsius.std",
           "Distance_big_breeding_site_km.std",
           "Distance_platform_sqrt.std",
           "SandPercent.std",
           "DistanceShippingLanes_4root.std",
           "season", "periodc", "Fishing_hour_averagec",
           "covariate", "est", "est_se")
myk <- 5

## run for all seasons
myseason   <- c("dec-jan","oct-nov","feb-mrch","apr-may", "jun-jul", "aug-sep")

for (k in 1:length(myseason)) {
  iseason <- myseason[k]

dd      <- dat[dat$season == iseason,]

## response
dd$response <- dd$dens_sqrt


#### 0. load model ----
## mod
load(file=paste0(output_path, iseason, "_final_model_", model_type, ".RData"))
mod$formula

## covariates 
cov_list <- labels(terms(mod$formula[[1]]))
cov_list



## !!!for chlo, SST i used the values from the current season, because the values differ by season
## the conditional effect should be plotted based on the range of values from the current season

## 1. depth ----
if ("s(depth1.std, k = myk)" %in% cov_list) {
   ddd1 <- expand.grid(depth1.std = seq(min(dat$depth1.std), max(dat$depth1.std), 0.2),
                    esa.std = median(dat$esa.std),
                    meanChlorophyll_sqrt.std  = median(dd$meanChlorophyll_sqrt.std),
                    Analysed_meanSST_Celsius.std = median(dd$Analysed_meanSST_Celsius.std),
                    Distance_big_breeding_site_km.std = median(dat$Distance_big_breeding_site_km.std),
                    Distance_platform_sqrt.std = median(dat$Distance_platform_sqrt.std),
                    SandPercent.std = median(dat$SandPercent.std),
                    DistanceShippingLanes_4root.std = median(dat$DistanceShippingLanes_4root.std),
                    season         = as.factor(iseason),
                    periodc        = "(2015,2020]",
                    Fishing_hour_averagec   = "(0,42.9]" )

   d_fake <- ddd1[rep(1, 3), ]
   d_fake$periodc<- as.character(c("(1999,2005]","(2005,2010]","(2010,2015]"))
   
   d_fake_fish <- ddd1[rep(1, 3), ]
   d_fake_fish$Fishing_hour_averagec<- as.character(c("(42.9,106]","(106,1.29e+03]","(1.29e+03,2.87e+03]"))

   ddd1 <- rbind(d_fake, d_fake_fish, ddd1)
   ddd1$Fishing_hour_averagec <- factor(ddd1$Fishing_hour_averagec, levels=levels(dat$Fishing_hour_averagec))
   ddd1$periodc               <- factor(ddd1$periodc, levels=levels(dat$periodc))

   ## first X rows of ddd1 are fake data, needs to exclude later
   ## check if ddd1 is fine
   head(ddd1)
   p1 <- sdmTMB:::predict.sdmTMB(mod, newdata=ddd1, 
                type = "link",
                se_fit=TRUE, 
                re_form=NA, re_form_iid = NA)
   ## exclude the fake X rows added before
   p1 <- p1[7:nrow(p1),]
   p1$covariate <- "depth"
   p <- rbind(p, p1[,myvar])
   myplotfunc(p1, ispe, iseason, "depth1.std", "depth")
}


## 2. meanChlorophyll ----
if ("s(meanChlorophyll_sqrt.std, k = myk)" %in% cov_list) {
  ddd1 <- expand.grid(meanChlorophyll_sqrt.std = seq(min(dd$meanChlorophyll_sqrt.std), max(dd$meanChlorophyll_sqrt.std), 0.2),
                      esa.std = median(dat$esa.std),
                      depth1.std  = median(dat$depth1.std),
                      Analysed_meanSST_Celsius.std = median(dd$Analysed_meanSST_Celsius.std),
                      Distance_big_breeding_site_km.std = median(dat$Distance_big_breeding_site_km.std),
                      Distance_platform_sqrt.std = median(dat$Distance_platform_sqrt.std),
                      SandPercent.std = median(dat$SandPercent.std),
                      DistanceShippingLanes_4root.std = median(dat$DistanceShippingLanes_4root.std),
                      season         = as.factor(iseason),
                      periodc        = "(2015,2020]",
                      Fishing_hour_averagec   = "(0,42.9]" )
  
  
  d_fake <- ddd1[rep(1, 3), ]
  d_fake$periodc<- as.character(c("(1999,2005]","(2005,2010]","(2010,2015]"))
  
  d_fake_fish <- ddd1[rep(1, 3), ]
  d_fake_fish$Fishing_hour_averagec<- as.character(c("(42.9,106]","(106,1.29e+03]","(1.29e+03,2.87e+03]"))
  
  ddd1 <- rbind(d_fake, d_fake_fish, ddd1)
  ddd1$Fishing_hour_averagec <- factor(ddd1$Fishing_hour_averagec, levels=levels(dat$Fishing_hour_averagec))
  ddd1$periodc               <- factor(ddd1$periodc, levels=levels(dat$periodc))
  
  ## first X rows of ddd1 are fake data, needs to exclude later
  ## check if ddd1 is fine
  head(ddd1)
  p1 <- sdmTMB:::predict.sdmTMB(mod, newdata=ddd1, 
                                type = "link",
                                se_fit=TRUE, 
                                re_form=NA, re_form_iid = NA)
  ## exclude the fake X rows added before
  p1 <- p1[7:nrow(p1),]
  p1$covariate <- "Chlorophyll"
  p <- rbind(p, p1[,myvar])
  myplotfunc(p1, ispe, iseason, "meanChlorophyll_sqrt.std", "Chlorophyll")
  
}


## 3. sand ----
if ("s(SandPercent.std, k = myk)" %in% cov_list) {
  ddd1 <- expand.grid(SandPercent.std = seq(min(dat$SandPercent.std), max(dat$SandPercent.std), 0.2),
                      esa.std = median(dat$esa.std),
                      depth1.std  = median(dat$depth1.std),
                      Analysed_meanSST_Celsius.std = median(dd$Analysed_meanSST_Celsius.std),
                      Distance_big_breeding_site_km.std = median(dat$Distance_big_breeding_site_km.std),
                      Distance_platform_sqrt.std = median(dat$Distance_platform_sqrt.std),
                      meanChlorophyll_sqrt.std = median(dd$meanChlorophyll_sqrt.std),
                      DistanceShippingLanes_4root.std = median(dat$DistanceShippingLanes_4root.std),
                      season         = as.factor(iseason),
                      periodc        = "(2015,2020]",
                      Fishing_hour_averagec   = "(0,42.9]" )
  
  
  d_fake <- ddd1[rep(1, 3), ]
  d_fake$periodc<- as.character(c("(1999,2005]","(2005,2010]","(2010,2015]"))
  
  d_fake_fish <- ddd1[rep(1, 3), ]
  d_fake_fish$Fishing_hour_averagec<- as.character(c("(42.9,106]","(106,1.29e+03]","(1.29e+03,2.87e+03]"))
  
  ddd1 <- rbind(d_fake, d_fake_fish, ddd1)
  ddd1$Fishing_hour_averagec <- factor(ddd1$Fishing_hour_averagec, levels=levels(dat$Fishing_hour_averagec))
  ddd1$periodc               <- factor(ddd1$periodc, levels=levels(dat$periodc))
  
  ## first X rows of ddd1 are fake data, needs to exclude later
  ## check if ddd1 is fine
  head(ddd1)
  p1 <- sdmTMB:::predict.sdmTMB(mod, newdata=ddd1, 
                                type = "link",
                                se_fit=TRUE, 
                                re_form=NA, re_form_iid = NA)
  ## exclude the fake X rows added before
  p1 <- p1[7:nrow(p1),]
  p1$covariate <- "non-sand"
  p <- rbind(p, p1[,myvar])
  myplotfunc(p1, ispe, iseason, "SandPercent.std", "non-sand")
  
}

## 4. breeding ----
if ("s(Distance_big_breeding_site_km.std, k = myk)" %in% cov_list) {
  ddd1 <- expand.grid(Distance_big_breeding_site_km.std = seq(min(dat$Distance_big_breeding_site_km.std), max(dat$Distance_big_breeding_site_km.std), 0.2),
                      esa.std = median(dat$esa.std),
                      depth1.std  = median(dat$depth1.std),
                      Analysed_meanSST_Celsius.std = median(dd$Analysed_meanSST_Celsius.std),
                      SandPercent.std = median(dat$SandPercent.std),
                      Distance_platform_sqrt.std = median(dat$Distance_platform_sqrt.std),
                      meanChlorophyll_sqrt.std = median(dd$meanChlorophyll_sqrt.std),
                      DistanceShippingLanes_4root.std = median(dat$DistanceShippingLanes_4root.std),
                      season         = as.factor(iseason),
                      periodc        = "(2015,2020]",
                      Fishing_hour_averagec   = "(0,42.9]" )
  
  
  d_fake <- ddd1[rep(1, 3), ]
  d_fake$periodc<- as.character(c("(1999,2005]","(2005,2010]","(2010,2015]"))
  
  d_fake_fish <- ddd1[rep(1, 3), ]
  d_fake_fish$Fishing_hour_averagec<- as.character(c("(42.9,106]","(106,1.29e+03]","(1.29e+03,2.87e+03]"))
  
  ddd1 <- rbind(d_fake, d_fake_fish, ddd1)
  ddd1$Fishing_hour_averagec <- factor(ddd1$Fishing_hour_averagec, levels=levels(dat$Fishing_hour_averagec))
  ddd1$periodc               <- factor(ddd1$periodc, levels=levels(dat$periodc))
  
  ## first X rows of ddd1 are fake data, needs to exclude later
  ## check if ddd1 is fine
  head(ddd1)
  p1 <- sdmTMB:::predict.sdmTMB(mod, newdata=ddd1, 
                                type = "link",
                                se_fit=TRUE, 
                                re_form=NA, re_form_iid = NA)
  ## exclude the fake X rows added before
  p1 <- p1[7:nrow(p1),]
  p1$covariate <- "dis_breeding"
  p <- rbind(p, p1[,myvar])
  myplotfunc(p1, ispe, iseason, "Distance_big_breeding_site_km.std", "dis_big_breeding")
  
}


## 5. SST ----
if ("s(Analysed_meanSST_Celsius.std, k = myk)" %in% cov_list) {
  ddd1 <- expand.grid(Analysed_meanSST_Celsius.std = seq(min(dd$Analysed_meanSST_Celsius.std), max(dd$Analysed_meanSST_Celsius.std), 0.2),
                      esa.std = median(dat$esa.std),
                      depth1.std  = median(dat$depth1.std),
                      Distance_big_breeding_site_km.std = median(dat$Distance_big_breeding_site_km.std),
                      SandPercent.std = median(dat$SandPercent.std),
                      Distance_platform_sqrt.std = median(dat$Distance_platform_sqrt.std),
                      meanChlorophyll_sqrt.std = median(dd$meanChlorophyll_sqrt.std),
                      DistanceShippingLanes_4root.std = median(dat$DistanceShippingLanes_4root.std),
                      season         = as.factor(iseason),
                      periodc        = "(2015,2020]",
                      Fishing_hour_averagec   = "(0,42.9]" )
  
  
  d_fake <- ddd1[rep(1, 3), ]
  d_fake$periodc<- as.character(c("(1999,2005]","(2005,2010]","(2010,2015]"))
  
  d_fake_fish <- ddd1[rep(1, 3), ]
  d_fake_fish$Fishing_hour_averagec<- as.character(c("(42.9,106]","(106,1.29e+03]","(1.29e+03,2.87e+03]"))
  
  ddd1 <- rbind(d_fake, d_fake_fish, ddd1)
  ddd1$Fishing_hour_averagec <- factor(ddd1$Fishing_hour_averagec, levels=levels(dat$Fishing_hour_averagec))
  ddd1$periodc               <- factor(ddd1$periodc, levels=levels(dat$periodc))
  
  ## first X rows of ddd1 are fake data, needs to exclude later
  ## check if ddd1 is fine
  head(ddd1)
  p1 <- sdmTMB:::predict.sdmTMB(mod, newdata=ddd1, 
                                type = "link",
                                se_fit=TRUE, 
                                re_form=NA, re_form_iid = NA)
  ## exclude the fake X rows added before
  p1 <- p1[7:nrow(p1),]
  p1$covariate <- "SST"
  p <- rbind(p, p1[,myvar])
  myplotfunc(p1, ispe, iseason, "Analysed_meanSST_Celsius.std", "SST")
  
}


## 6. shipping ----
if ("s(DistanceShippingLanes_4root.std, k = myk)" %in% cov_list) {
  ddd1 <- expand.grid(DistanceShippingLanes_4root.std = seq(min(dat$DistanceShippingLanes_4root.std), max(dat$DistanceShippingLanes_4root.std), 0.2),
                      esa.std = median(dat$esa.std),
                      depth1.std  = median(dat$depth1.std),
                      Analysed_meanSST_Celsius.std = median(dd$Analysed_meanSST_Celsius.std),
                      Distance_big_breeding_site_km.std = median(dat$Distance_big_breeding_site_km.std),
                      SandPercent.std = median(dat$SandPercent.std),
                      Distance_platform_sqrt.std = median(dat$Distance_platform_sqrt.std),
                      meanChlorophyll_sqrt.std = median(dd$meanChlorophyll_sqrt.std),
                      season         = as.factor(iseason),
                      periodc        = "(2015,2020]",
                      Fishing_hour_averagec   = "(0,42.9]" )
  
  
  d_fake <- ddd1[rep(1, 3), ]
  d_fake$periodc<- as.character(c("(1999,2005]","(2005,2010]","(2010,2015]"))
  
  d_fake_fish <- ddd1[rep(1, 3), ]
  d_fake_fish$Fishing_hour_averagec<- as.character(c("(42.9,106]","(106,1.29e+03]","(1.29e+03,2.87e+03]"))
  
  ddd1 <- rbind(d_fake, d_fake_fish, ddd1)
  ddd1$Fishing_hour_averagec <- factor(ddd1$Fishing_hour_averagec, levels=levels(dat$Fishing_hour_averagec))
  ddd1$periodc               <- factor(ddd1$periodc, levels=levels(dat$periodc))
  
  ## first X rows of ddd1 are fake data, needs to exclude later
  ## check if ddd1 is fine
  head(ddd1)
  p1 <- sdmTMB:::predict.sdmTMB(mod, newdata=ddd1, 
                                type = "link",
                                se_fit=TRUE, 
                                re_form=NA, re_form_iid = NA)
  ## exclude the fake X rows added before
  p1 <- p1[ind_start:nrow(p1),]
  p1$covariate <- "SST"
  p <- rbind(p, p1[,myvar])
  myplotfunc(p1, ispe, iseason, "DistanceShippingLanes_4root.std", "dis_shiping")
  
}


## 7. yearc ----
if ("yearc" %in% cov_list) {
  ddd1 <- expand.grid(DistanceShippingLanes_4root.std = median(dat$DistanceShippingLanes_4root.std),
                      esa.std = median(dat$esa.std),
                      depth1.std  = median(dat$depth1.std),
                      Analysed_meanSST_Celsius.std = median(dd$Analysed_meanSST_Celsius.std),
                      Distance_big_breeding_site_km.std = median(dat$Distance_big_breeding_site_km.std),
                      SandPercent.std = median(dat$SandPercent.std),
                      Distance_platform_sqrt.std = median(dat$Distance_platform_sqrt.std),
                      meanChlorophyll_sqrt.std = median(dd$meanChlorophyll_sqrt.std),
                      season         = as.factor(iseason),
                      Year_bimonthly          = year_all,
                      Fishing_hour_averagec   = "(0,42.9]" )
  
  
  d_fake      <- ddd1[rep(1, Length_fake_year), ]
  d_fake$Year_bimonthly<- fake_year
  #unique(dat$Fishing_hour_averagec)
  d_fake_fish <- ddd1[rep(1, 3), ]
  d_fake_fish$Fishing_hour_averagec<- as.character(c("(42.9,106]","(106,1.29e+03]","(1.29e+03,2.87e+03]"))
  
  ddd1 <- rbind(d_fake, d_fake_fish, ddd1)
  ddd1$Fishing_hour_averagec <- factor(ddd1$Fishing_hour_averagec, levels=levels(dat$Fishing_hour_averagec))
  #ddd1$periodc               <- factor(ddd1$periodc, levels=levels(dat$periodc))
  ddd1$yearc               <- factor(ddd1$Year_bimonthly)
  
  ## first X rows of ddd1 are fake data, needs to exclude later
  ## check if ddd1 is fine
  head(ddd1)
  p1 <- sdmTMB:::predict.sdmTMB(mod, newdata=ddd1, 
                                type = "link",
                                se_fit=TRUE, 
                                re_form=NA, re_form_iid = NA)
  ## exclude the fake X rows added before
  p1 <- p1[ind_start:nrow(p1),]
  p1$covariate <- "year"
  p <- rbind(p, p1[,myvar])
  myplotfunc(p1, ispe, iseason, "yearc", "year")
  
}

## 8. periodc ----
if ("periodc" %in% cov_list) {
  ddd1 <- expand.grid(DistanceShippingLanes_4root.std = median(dat$DistanceShippingLanes_4root.std),
                      esa.std = median(dat$esa.std),
                      depth1.std  = median(dat$depth1.std),
                      Analysed_meanSST_Celsius.std = median(dd$Analysed_meanSST_Celsius.std),
                      Distance_big_breeding_site_km.std = median(dat$Distance_big_breeding_site_km.std),
                      SandPercent.std = median(dat$SandPercent.std),
                      Distance_platform_sqrt.std = median(dat$Distance_platform_sqrt.std),
                      meanChlorophyll_sqrt.std = median(dd$meanChlorophyll_sqrt.std),
                      season         = as.factor(iseason),
                      periodc        = levels(dat$periodc),
                      Fishing_hour_averagec   = "(0,42.9]" )
  
  
  d_fake <- ddd1[rep(1, 3), ]
  d_fake$periodc<- as.character(c("(1999,2005]","(2005,2010]","(2010,2015]"))
  
  d_fake_fish <- ddd1[rep(1, 3), ]
  d_fake_fish$Fishing_hour_averagec<- as.character(c("(42.9,106]","(106,1.29e+03]","(1.29e+03,2.87e+03]"))
  
  ddd1 <- rbind(d_fake, d_fake_fish, ddd1)
  ddd1$Fishing_hour_averagec <- factor(ddd1$Fishing_hour_averagec, levels=levels(dat$Fishing_hour_averagec))
  ddd1$periodc               <- factor(ddd1$periodc, levels=levels(dat$periodc))
  
  ## first X rows of ddd1 are fake data, needs to exclude later
  ## check if ddd1 is fine
  head(ddd1)
  p1 <- sdmTMB:::predict.sdmTMB(mod, newdata=ddd1, 
                                type = "link",
                                se_fit=TRUE, 
                                re_form=NA, re_form_iid = NA)
  ## exclude the fake X rows added before
  p1 <- p1[7:nrow(p1),]
  p1$covariate <- "period"
  p <- rbind(p, p1[,myvar])
  myplotfunc(p1, ispe, iseason, "periodc", "period")
  
}

## 9. fishing ----
if ("Fishing_hour_averagec" %in% cov_list) {
  ddd1 <- expand.grid(DistanceShippingLanes_4root.std = median(dat$DistanceShippingLanes_4root.std),
                      esa.std = median(dat$esa.std),
                      depth1.std  = median(dat$depth1.std),
                      Analysed_meanSST_Celsius.std = median(dd$Analysed_meanSST_Celsius.std),
                      Distance_big_breeding_site_km.std = median(dat$Distance_big_breeding_site_km.std),
                      SandPercent.std = median(dat$SandPercent.std),
                      Distance_platform_sqrt.std = median(dat$Distance_platform_sqrt.std),
                      meanChlorophyll_sqrt.std = median(dd$meanChlorophyll_sqrt.std),
                      season         = as.factor(iseason),
                      periodc        = "(2015,2020]",
                      Fishing_hour_averagec   = levels(dat$Fishing_hour_averagec) )
  
  
  d_fake <- ddd1[rep(1, 3), ]
  d_fake$periodc<- as.character(c("(1999,2005]","(2005,2010]","(2010,2015]"))
  
  d_fake_fish <- ddd1[rep(1, 3), ]
  d_fake_fish$Fishing_hour_averagec<- as.character(c("(42.9,106]","(106,1.29e+03]","(1.29e+03,2.87e+03]"))
  
  ddd1 <- rbind(d_fake, d_fake_fish, ddd1)
  ddd1$Fishing_hour_averagec <- factor(ddd1$Fishing_hour_averagec, levels=levels(dat$Fishing_hour_averagec))
  ddd1$periodc               <- factor(ddd1$periodc, levels=levels(dat$periodc))
  
  ## first X rows of ddd1 are fake data, needs to exclude later
  ## check if ddd1 is fine
  head(ddd1)
  p1 <- sdmTMB:::predict.sdmTMB(mod, newdata=ddd1, 
                                type = "link",
                                se_fit=TRUE, 
                                re_form=NA, re_form_iid = NA)
  ## exclude the fake X rows added before
  p1 <- p1[7:nrow(p1),]
  p1$covariate <- "Fishing_hour"
  p <- rbind(p, p1[,myvar])
  myplotfunc(p1, ispe, iseason, "Fishing_hour_averagec", "fishing hour")
  
}
}# end of season loop

p <- p[-1,]
save(p, file=paste0(output_path, "final_model_conditional_effect_", model_type, ".RData"))

load(file=paste0(output_path, "final_model_conditional_effect_", model_type, ".RData"))
p$season <- factor(p$season, levels=c("dec-jan", "feb-mrch", "apr-may", "jun-jul", "aug-sep", "oct-nov"))

mycol <- c("dec-jan"="#91bfdb","feb-mrch" = "#4575b4",'apr-may'= "#fee090","jun-jul" = "#fc8d59","aug-sep" = "#d73027","oct-nov"="#e0f3f8")

  
unique(p$covariate)
unique(p$season)
names(p)


## here you need to change variable name
icov    <- "depth"
covname <- "depth1.std"
f1 <- ggplot(p[p$covariate == icov,], aes(x = get(covname), y = est) ) +
  geom_line(linewidth=2, col="red") +
  geom_ribbon(aes(ymin = est-1.96*est_se, ymax = est+1.96*est_se), alpha = 0.2,col=NA, fill="red") +
  scale_colour_manual(values=mycol) +
  ylab("linear predictor")+
  xlab(covname)+
  theme_bw()+
  ggtitle(paste(ispe,  icov, ", conditional effect", sep=":"))
f2 <- facet(f1, facet.by = c("season"), ncol = 2, panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) 
plot(f2)
ggsave(f2, file=paste0(prediction_path,"conditional_effect/", ispe1,"_all season_conditional_effect_", icov, "1.png"), width = 20, height = 20, units = 'cm')

f1 <- ggplot(p[p$covariate == icov,], aes(x = get(covname), y = est, group=season, color=season) ) +
  geom_line(linewidth=2) +
  geom_ribbon(aes(ymin = est-1.96*est_se, ymax = est+1.96*est_se, fill=season), alpha = 0.2,col=NA) +
  scale_colour_manual(values=mycol) +
  scale_fill_manual(values=mycol) +
  ylab("linear predictor")+
  xlab(covname)+
  theme_bw()+
  ggtitle(paste(ispe, icov, " conditional effect", sep=":"))
print(f1)
ggsave(f1, file=paste0(prediction_path,"conditional_effect/", ispe1,"_all season_conditional_effect_", icov, ".png"), width = 20, height = 20, units = 'cm')




f1 <- p[p$covariate == "period",]

f1 <- f1 %>% filter(!is.na(periodc)) %>% 
  ggplot(aes(x = periodc, y = est) ) +
  geom_point(color="red", size=2)+
  geom_errorbar(aes(ymin=est-1.96*est_se, ymax=est+1.96*est_se), width=.2,color="red",
                position=position_dodge(0.05))+
  ylab("linear predictor")+
  xlab("period")+
  theme_bw()+
  ggtitle(paste(ispe, "period", " conditional effect", sep=":"))
f2 <- facet(f1, facet.by = c("season"), ncol = 2, panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) 
plot(f2)

ggsave(f2, file=paste0(prediction_path, "conditional_effect/",ispe1,"_season_cond_eff_", "period", "1.png"), width = 20, height = 20, units = 'cm')



f1 <- p[p$covariate == "Fishing_hour",]

f1 <- f1 %>%   
  filter(!is.na(Fishing_hour_averagec)) %>% 
  ggplot(aes(x = Fishing_hour_averagec, y = est) ) +
  geom_point(color="red", size=2)+
  geom_errorbar(aes(ymin=est-1.96*est_se, ymax=est+1.96*est_se), width=.2,color="red",
                position=position_dodge(0.05))+
  ylab("linear predictor")+
  xlab("Fishing_hour")+
  theme_bw()+
  ggtitle(paste(ispe, "Fishing_hour", " conditional effect", sep=":"))
f2 <- facet(f1, facet.by = c("season"), ncol = 2, panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) 
plot(f2)
ggsave(f2, file=paste0(prediction_path, "conditional_effect/",ispe1,"_season_cond_eff_", "Fishing", "1.png"), width = 20, height = 20, units = 'cm')




## here you need to change variable name 1] "Chlorophyll"  "SST"  
icov    <- "Chlorophyll"
covname <- "meanChlorophyll_sqrt.std"
f1 <- ggplot(p[p$covariate == icov,], aes(x = get(covname), y = est) ) +
  geom_line(linewidth=2, col="red") +
  geom_ribbon(aes(ymin = est-1.96*est_se, ymax = est+1.96*est_se), alpha = 0.2,col=NA, fill="red") +
  scale_colour_manual(values=mycol) +
  ylab("linear predictor")+
  xlab(covname)+
  theme_bw()+
  ggtitle(paste(ispe,  icov, ", conditional effect", sep=":"))
f2 <- facet(f1, facet.by = c("season"), ncol = 2, panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) 
plot(f2)
ggsave(f2, file=paste0(prediction_path,"conditional_effect/", ispe1,"_cond_effect_", icov, "1.png"), width = 20, height = 20, units = 'cm')

f1 <- ggplot(p[p$covariate == icov,], aes(x = get(covname), y = est, group=season, color=season) ) +
  geom_line(linewidth=2) +
  geom_ribbon(aes(ymin = est-1.96*est_se, ymax = est+1.96*est_se, fill=season), alpha = 0.2,col=NA) +
  scale_colour_manual(values=mycol) +
  scale_fill_manual(values=mycol) +
  ylab("linear predictor")+
  xlab(covname)+
  theme_bw()+
  ggtitle(paste(ispe, icov, " conditional effect", sep=":"))
print(f1)
ggsave(f1, file=paste0(prediction_path,"conditional_effect/", ispe1,"_cond_effect_", icov, ".png"), width = 20, height = 20, units = 'cm')


icov    <- "SST"
covname <- "Analysed_meanSST_Celsius.std"
f1 <- ggplot(p[p$covariate == icov,], aes(x = get(covname), y = est) ) +
  geom_line(linewidth=2, col="red") +
  geom_ribbon(aes(ymin = est-1.96*est_se, ymax = est+1.96*est_se), alpha = 0.2,col=NA, fill="red") +
  scale_colour_manual(values=mycol) +
  ylab("linear predictor")+
  xlab(covname)+
  theme_bw()+
  ggtitle(paste(ispe,  icov, ", conditional effect", sep=":"))
f2 <- facet(f1, facet.by = c("season"), ncol = 2, panel.labs.font.y = list(size = 14, angle=0), panel.labs.font.x = list(size = 14)) 
plot(f2)
ggsave(f2, file=paste0(prediction_path,"conditional_effect/", ispe1,"_seasons_cond_effect_", icov, "1.png"), width = 20, height = 20, units = 'cm')

f1 <- ggplot(p[p$covariate == icov,], aes(x = get(covname), y = est, group=season, color=season) ) +
  geom_line(linewidth=2) +
  geom_ribbon(aes(ymin = est-1.96*est_se, ymax = est+1.96*est_se, fill=season), alpha = 0.2,col=NA) +
  scale_colour_manual(values=mycol) +
  scale_fill_manual(values=mycol) +
  ylab("linear predictor")+
  xlab(covname)+
  theme_bw()+
  ggtitle(paste(ispe, icov, " conditional effect", sep=":"))
print(f1)
ggsave(f1, file=paste0(prediction_path,"conditional_effect/", ispe1,"_seasons_cond_effect_", icov, ".png"), width = 20, height = 20, units = 'cm')




# END -------------------------------------------------------------------------------------------------





## fit model time-invariant spatial
fit_TIV <- function(idat, imesh, iformula, iseason, imodel, iresponse) {
  ifit <- sdmTMB(
    iformula,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = imodel,
                     spatialtemporal = "time-invariant spatial",
                     formula = paste(deparse(iformula, width.cutoff = 500), collapse=""),
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     nrow   = nrow(idat),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     sigma_E = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

fit_fixed <- function(idat, iformula, iseason, imodel, iresponse) {
  ifit <- sdmTMB(
    iformula,
    data = idat,
    family = tweedie(link = "log"),
    spatial = "off"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = imodel,
                     spatialtemporal = "off",
                     formula = paste(deparse(iformula, width.cutoff = 500), collapse=""),
                     mesh = NA,
                     response = iresponse, 
                     distribution = "tweedie",
                     nrow   = nrow(idat),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     sigma_E   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  #ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  #ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

fit_TV <- function(idat, imesh, iformula, itime, itype, iseason, imodel, iresponse) {
  ifit <- sdmTMB(
    iformula,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on",
    time = itime,
    spatiotemporal = itype
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = imodel,
                     spatialtemporal = itype,
                     formula = paste(deparse(iformula, width.cutoff = 500), collapse=""),
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     nrow   = nrow(idat),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     sigma_E   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$sigma_E     <-  aa$estimate[aa$term == "sigma_E"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

fit_TV_period <- function(idat, imesh, iformula, itype, iseason, imodel, iresponse) {
  ifit <- sdmTMB(
    iformula,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on",
    time = "period",
    spatiotemporal = itype
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = imodel,
                     spatialtemporal = itype,
                     formula = paste(deparse(iformula, width.cutoff = 500), collapse=""),
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     nrow   = nrow(idat),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     sigma_E   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$sigma_E     <-  aa$estimate[aa$term == "sigma_E"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}


fit_only_spatial_TIV <- function(idat, imesh, iformula, iseason, imodel, iresponse) {
  ifit <- sdmTMB(
    iformula,
    data = idat,
    mesh = mesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = imodel,
                     formula = paste(deparse(iformula, width.cutoff = 500), collapse=""),
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     nrow   = nrow(idat),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}



## functions run model
model_validation1 <- function (myfit, dd1, data_path) {
  ires <- data.frame(MSE_ob=NA,
                     CV_ob=NA,
                     CV_pred=NA)
  # ------- 1. calculate MSE/CV on observed data ----
  pred1 <- predict(myfit, 
                   type="link",
                   se_fit=FALSE,
                   re_form_iid = NULL,
                   re_form = NULL)
  head(pred1)
  dd1$est  <- exp(pred1$est)
  dd1$diff <- (dd1$response-dd1$est)^2 ## (obs-est)^2
  ## MSE: non-parametric
  ires$MSE_ob <- mean(dd1$diff)
  
  ## simulate posterior prediction distribution
  sim      <- predict(myfit, nsim = 1000)
  dd1$cv   <- round(apply(exp(sim), 1, function(x) sd(x) / mean(x)), 2)
  ## mean CV
  ires$CV_ob <- mean(dd1$cv)
  
  # ------- 2. prepare prediction data/map ----
  ## load Prediction grid
  map1 <- readRDS(file=paste0(data_path, "Prediction grid/Prediction_grid_all_Bimonthly.rds"))
  map1  <- as.data.frame(map1)
  #head(map1)
  map1$x_utm1 <- map1$x/1000
  map1$y_utm1 <- map1$y/1000
  map1$esa    <- median(dd1$esa)  ## dont have esa value in the map
  ## exclude grid outside the range of data
  #nrow(map1)
  dnew <- map1[map1$x >= min(dd1$x_utm) & map1$x <= max(dd1$x_utm)
               & map1$y >= min(dd1$y_utm)
               & map1$y <= max(dd1$y_utm),]
  ## transformation
  dnew$distance_coast_avg_sqrt     <- sqrt(dnew$distance_coast_avg)
  dnew$Distance_breeding_site_sqrt <- sqrt(dnew$Distance_breeding_site_km)
  dnew$MudPercent_trans <- dnew$MudPercent^(1/4)
  dnew$DistanceShippingLanes_sqrt  <- sqrt(dnew$DistanceShippingLanesInKm)
  dnew$platform   <- "aerial"
  dnew$InWindFarm <- "Yes"
  
  ## exclude NA
  dnew       <- dnew[!is.na(dnew$distance_coast_avg_sqrt) & !is.na(dnew$Analysed_meanSST_Celsius) & !is.na(dnew$MudPercent_trans),]
  
  # ------- 3. CV of map ----
  #pred  <- predict(myfit, newdata = dnew, 
  #                 type="link",
  #                 se_fit=TRUE,
  #                 re_form_iid = NULL,
  #                 re_form = NULL)
  
  ## simulate posterior distribution
  sim      <- predict(myfit, newdata = dnew, nsim = 500)
  #dim(sim)
  dnew$lwr <- apply(exp(sim), 1, quantile, probs = 0.025)
  dnew$upr <- apply(exp(sim), 1, quantile, probs = 0.975)
  dnew$sd  <- round(apply(exp(sim), 1, function(x) sd(x)), 2)
  dnew$cv  <- round(apply(exp(sim), 1, function(x) sd(x) / mean(x)), 2)
  ires$CV_pred <- mean(dnew$cv)
  
  return(ires)
}

R_process <- function(ieason) {
  #rm(list = ls())
  library(dplyr)
  library(ggplot2)
  library(sdmTMB)
  library(sdmTMBextra)
  library(patchwork)
  #library(INLA)
  
  ## list of all functions
  #lsf.str("package:sdmTMB")
  help(package = sdmTMB)
  
  ## setting
  mypath         <- "C:/Users/chen072/OneDrive - Wageningen University & Research/0_2023_bird_map/"
  data_path     <- paste(mypath, "Data/", sep="")
  fig_path      <- paste(mypath, "Figure/", sep="")
  res_path      <- paste(mypath, "Results/", sep="")
  #setwd(result_path)
  
  ## functions
  lunique <- function (x) {
    length(unique(x))
  }
  num_pos <- function(x) {
    sum(x>0)
  }
  source(paste0(mypath, "Script/functions_model.r"))
  
  #### 0. load data ----
  load(file=paste0(data_path, "processed_data_kittiwakes.RData"))
  
  ## define bi-month
  myseason   <- c("Dec-Jan", "Feb-Mar", "Apr-May", "Jun-Jul", "Aug-Sep", "Oct-Nov")
  dat$season <- "Dec-Jan"
  dat$season[dat$monthc %in% c(2,3)] <- "Feb-Mar"
  dat$season[dat$monthc %in% c(4,5)] <- "Apr-May"
  dat$season[dat$monthc %in% c(6,7)] <- "Jun-Jul"
  dat$season[dat$monthc %in% c(8,9)] <- "Aug-Sep"
  dat$season[dat$monthc %in% c(10,11)] <- "Oct-Nov"
  dat$season <- factor(dat$season, levels=myseason)
  
  ## exclude outliers
  ## exclude density >1000
  dat <- dat[dat$dens<1000,]
  ## exclude esa >1.5 (all before 2000, 134 segments) and esa <0.02 (155 segments)
  dat <- dat[dat$esa<1.5 & dat$esa>0.2,]
  
  #####################################################
  #### 1.1 data all or sub ----
  data_type     <- "all"
  ## assign response variable 
  response_type <- "density,untranformed"
  dat$response  <- dat$dens
  
  #####################################################
  #### 1.2 season ----
  dd      <- dat[dat$season == iseason,]
  unique(dd$season)
  

  ## exclude NA in some covariates
  ## distance_coast_avg
  ## Analysed_meanSST_Celsius
  dd1       <- dd[!is.na(dd$distance_coast_avg_sqrt) & !is.na(dd$Analysed_meanSST_Celsius) & !is.na(dd$SandPercent),]

  
  ## selected mesh
  mesh3 <- make_mesh(dd1, xy_cols = c("x_utm1", "y_utm1"), cutoff = 15)
  #plot(mesh2)
  mesh3$mesh$n
  
  #### 3. apply model ----
  myres <- NA
  ## select mesh
  mesh   <- mesh3
  mymesh <- "mesh3"
  
  return(list(dd1, mesh))
}

## m01: time-invariant spatial random field
fit_m01_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  start_time <- Sys.time()
  ifit <- sdmTMB(
    response ~ 1,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  end_time <- Sys.time()
  run_time <- end_time-start_time
  
  ires <- data.frame(season = iseason,
                      model  = "m01: only spatial",
                      mesh = imesh$mesh$n,
                      response = iresponse, 
                      distribution = "tweedie",
                      data   = itype,
                      nrow   = nrow(dd1),
                      range = NA,
                      phi   = NA,
                      sigma_O   = NA,
                      tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  
  return(list(fit_1, ires, run_time))
}


## m02: all covariates
fit_m02_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ## distance_coast_avg_sqrt, dis shipping, mud, esa
  ## Analysed_meanSST_Celsius
  ## platform + InWindFarm
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) + s(DistanceShippingLanes_sqrt) + s(MudPercent_trans) + s(Distance_breeding_site_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m02: all",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m11: exclude mud
fit_m11_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) + s(DistanceShippingLanes_sqrt) + s(Distance_breeding_site_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m11: m02 exc mud",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}


## m12: exclude dis_shipping
fit_m12_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) +  s(MudPercent_trans) + s(Distance_breeding_site_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m12: m02 exc shipping",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m13: exclude dis_breeding
fit_m13_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) + s(DistanceShippingLanes_sqrt) + s(MudPercent_trans)  + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m13: m02 exc dis breed",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m14: exclude dis_coast
fit_m14_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(Analysed_meanSST_Celsius) + s(DistanceShippingLanes_sqrt) + s(MudPercent_trans) + s(Distance_breeding_site_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m14: m02 exc dis coast",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m15: exclude SST
fit_m15_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(DistanceShippingLanes_sqrt) + s(MudPercent_trans) + s(Distance_breeding_site_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m15: m02 exc SST",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m16: exclude esa
fit_m16_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) + s(DistanceShippingLanes_sqrt) + s(MudPercent_trans) + s(Distance_breeding_site_sqrt) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m16: m02 exc esa",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m11: exclude distance to breeding site, linear SST effect
fit_mXX_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  m11 <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + esa + Analysed_meanSST_Celsius + s(DistanceShippingLanes_sqrt) + s(MudPercent_trans) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(m11) 
  fit_1 <- run_extra_optimization(m11, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m11",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m112: m11 exclude shipping
fit_m112_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) +  s(Distance_breeding_site_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m112: m11 exc shipping",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m113: m11 exclude breeding
fit_m113_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) + s(DistanceShippingLanes_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m113: m11 exc breeding",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m114: m11 exclude coast
fit_m114_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(Analysed_meanSST_Celsius) + s(DistanceShippingLanes_sqrt) + s(Distance_breeding_site_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m114: m11 exclude coast",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}


## m115: m11 exclude SST
fit_m115_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(DistanceShippingLanes_sqrt) + s(Distance_breeding_site_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m115: m11 exclude SST",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m116: m11 exclude esa
fit_m116_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) + s(DistanceShippingLanes_sqrt) + s(Distance_breeding_site_sqrt) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m116: m11 exclude esa",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}


## m121: m12 exclude mud
fit_m121_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) + s(Distance_breeding_site_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m121: m12 exc mud",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m124: m12 exclude dis coast
fit_m124_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(Analysed_meanSST_Celsius) +  s(MudPercent_trans) + s(Distance_breeding_site_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m124: m12 exc coast",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m132: m13 exclude shipping
fit_m132_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) + s(MudPercent_trans)  + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m132: m13 exc shipping",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m131: exclude mud
fit_m131_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) + s(DistanceShippingLanes_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m131: m13 exc mud",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m134: m13 exclude coast
fit_m134_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(Analysed_meanSST_Celsius) + s(DistanceShippingLanes_sqrt) + s(MudPercent_trans)  + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m134: m13 exc coast",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m135: m13 exclude SST
fit_m135_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(DistanceShippingLanes_sqrt) + s(MudPercent_trans)  + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m135: m13 exc SST",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m136: exclude esa
fit_m136_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) + s(DistanceShippingLanes_sqrt) + s(MudPercent_trans) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m136: m13 exc esa",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}



## m1241: m124 exclude mud
fit_m1241_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(Analysed_meanSST_Celsius) +  s(Distance_breeding_site_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m1241: m124 exc mud",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m1243: m124 exclude breeding
fit_m1243_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(Analysed_meanSST_Celsius) +  s(MudPercent_trans) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m1243: m124 exc breeding",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m1245: m125 exclude SST
fit_m1245_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(MudPercent_trans) + s(Distance_breeding_site_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m1245: m124 exc SST",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m1246: m124 exclude esa
fit_m1246_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(Analysed_meanSST_Celsius) +  s(MudPercent_trans) + s(Distance_breeding_site_sqrt) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m1246: m124 exc esa",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}


## m123: m12 exclude dis breeding
fit_m123_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) +  s(MudPercent_trans) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m123: m12 exc breeding",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m125: m12 exclude dis SST
fit_m125_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(MudPercent_trans) + s(Distance_breeding_site_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m125: m12 exc SST",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m126: m12 exclude dis esa
fit_m126_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  ifit <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) +  s(MudPercent_trans) + s(Distance_breeding_site_sqrt) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(ifit) 
  fit_1 <- run_extra_optimization(ifit, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m126: m12 exc esa",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m1111: m111 exclude SST
fit_m1111_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  m1111 <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(DistanceShippingLanes_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(m1111) 
  fit_1 <- run_extra_optimization(m1111, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m1111",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m1112: m111 exclude dis shipping
fit_m1112_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  m1112 <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(m1112) 
  fit_1 <- run_extra_optimization(m1112, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m1112",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m1113: m111 exclude dis coast
fit_m1113_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  m1113 <- sdmTMB(
    response ~ s(Analysed_meanSST_Celsius) + s(DistanceShippingLanes_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(m1113) 
  fit_1 <- run_extra_optimization(m1113, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m1113",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}


## m1114: m111 exclude platform
fit_m1114_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  m1114 <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) + s(DistanceShippingLanes_sqrt) + s(esa) + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(m1114) 
  fit_1 <- run_extra_optimization(m1114, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m1114",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m1115: m111 exclude windfarm
fit_m1115_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  m1115 <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) + s(DistanceShippingLanes_sqrt) + s(esa) + platform,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(m1115) 
  fit_1 <- run_extra_optimization(m1115, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m1115",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

## m1116: m111 without spatial random effect
fit_m1116_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  m1116 <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) + s(DistanceShippingLanes_sqrt) + s(esa) + platform + InWindFarm,
    data = idat,
    family = tweedie(link = "log"),
    spatial = "off"
  )
  sanity(m1116) 
  fit_1 <- run_extra_optimization(m1116, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m1116",
                     mesh = NA,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  NA
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  NA
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}


## m42: exclude dis shipping and mud, SST nonlinear, esa linear
fit_m42_tweedie <- function(idat, imesh, iseason, itype, iresponse) {
  m42 <- sdmTMB(
    response ~ s(distance_coast_avg_sqrt) + s(Analysed_meanSST_Celsius) + s(Distance_breeding_site_sqrt) + esa + platform + InWindFarm,
    data = idat,
    mesh = imesh,
    family = tweedie(link = "log"),
    spatial = "on"
  )
  sanity(m42) 
  fit_1 <- run_extra_optimization(m42, newton_loops = 1)
  sanity(fit_1)
  
  ires <- data.frame(season = iseason,
                     model  = "m42",
                     mesh = imesh$mesh$n,
                     response = iresponse, 
                     distribution = "tweedie",
                     data   = itype,
                     nrow   = nrow(dd1),
                     AIC    = NA,
                     range = NA,
                     phi   = NA,
                     sigma_O   = NA,
                     tweedie_p = NA)
  ires$AIC <- AIC(fit_1)
  aa       <- tidy(fit_1, "ran_pars", conf.int = TRUE)
  ires$range <-  aa$estimate[aa$term == "range"]
  ires$phi   <-  aa$estimate[aa$term == "phi"]
  ires$sigma_O     <-  aa$estimate[aa$term == "sigma_O"]
  ires$tweedie_p   <-  aa$estimate[aa$term == "tweedie_p"]
  return(list(fit_1, ires))
}

prepare_prediction_map <- function(itype, ipath, iseason, isize, ispe2, dd, ifishcut) {
  if (itype == "Year_bimonthly") {
    if (isize == 5) {
      ## load Prediction grid
      map1 <- readRDS(file=paste0(ipath, "Prediction_grid_XX.rds"))
    }
    if (isize == 10) {
      ## load Prediction grid
      map1 <- readRDS(file=paste0(ipath, "Prediction_grid_all_Bimonthly_Per_Year_10_10km.rds"))
      ## too much data, only select last 5 years
      ## last data year in sample is 2020
      map1 <- map1[map1$Year_bimonthly %in% c(2016:2020),]
    }
  }
  if (itype == "periodc") {
    if (isize == 5) {
      ## load Prediction grid
      map1 <- readRDS(file=paste0(ipath, "Prediction_grid_all_XX.rds"))
    }
    if (isize == 10) {
      ## load Prediction grid
      map1 <- readRDS(file=paste0(ipath, "Prediction_grid_all_Bimonthly_Per_5yearperiod_10_10km.rds"))
    }
  }
  
  map1  <- as.data.frame(map1)
  #head(map1)
  ## select species
  map1 <- map1[map1$scientific_name == ispe2,]
  ## select iseason
  table(map1$Bimonthly)
  map1 <- map1[map1$Bimonthly == iseason,]
  
  ## exclude grid outside the range of data
  #nrow(map1)
  dnew <- map1[map1$x >= min(dd$x_utm) & map1$x <= max(dd$x_utm)
               & map1$y >= min(dd$y_utm)
               & map1$y <= max(dd$y_utm),]
  
  names(dnew)[names(dnew) == "x"] <- "x_utm"
  names(dnew)[names(dnew) == "y"] <- "y_utm"
  dnew          <- data_transformation(dnew)
  dnew$esa      <- median(dd$esa)
  #dnew$platform <- "aerial"
  
  return(dnew)
}

data_transformation <- function(dat) {
  
  ## process x/y_utm: unit of km
  dat$x_utm1 <- dat$x_utm/1000
  dat$y_utm1 <- dat$y_utm/1000
  
  ## change depth into positive number, so we can take sqrt
  ## 808 rows with positive depth, -> land, exclude
  sum(dat$depth>=0 & !is.na(dat$depth))
  dat$depth1 <- -dat$depth
  summary(dat$depth1)
  dat$depth1_sqrt <- sqrt(abs(dat$depth1))
  summary(dat$depth1_sqrt)
  dat$depth1_sqrt[dat$depth>0 & !is.na(dat$depth)] <- (-dat$depth1_sqrt[dat$depth>0 & !is.na(dat$depth)])
  
  ## extreme depth: >500, 17 rows
  ## exclude extreme depth
  ## no depth>100 in aerial data
  #plot(dat$x_utm1, dat$y_utm1, col="black")
  #points(dat$x_utm1[!is.na(dat$depth) & dat$depth<(-500)], dat$y_utm1[!is.na(dat$depth)& dat$depth<(-500)], col="red")
  nrow(dat[dat$depth<(-500) & !is.na(dat$depth),])
  dat    <- dat[dat$depth>(-500) | is.na(dat$depth),]
  
  ## sqrt transformation of distance to coast
  ## convert to unit of km
  dat$distance_coast_avg_km <- dat$distance_coast_avg/1000 
  summary(dat$distance_coast_avg_km)
  dat$distance_coast_avg_sqrt <- sqrt(dat$distance_coast_avg)
  
  ## sqrt transformation of distance to breeding site
  summary(dat$Distance_breeding_site_km)
  dat$Distance_breeding_site_sqrt <- sqrt(dat$Distance_breeding_site_km)
  
  ## sqrt transformation of distance to large breeding site
  summary(dat$Distance_big_breeding_site_km)
  dat$Distance_big_breeding_site_km_sqrt <- sqrt(dat$Distance_big_breeding_site_km)
  
  ## sqrt transformation of distance to platform
  ## name is inconsistent between input and map
  ind <- grep("platform_km", names(dat))
  summary(dat[,ind])
  dat$Distance_platform_sqrt <- sqrt(dat[,ind])
  
  ## transformation of mud/sand percentage
  summary(dat$MudPercent)
  dat$MudPercent_trans <- dat$MudPercent^(1/4)
  summary(dat$MudPercent_trans)
  summary(dat$SandPercent)
  dat$SandPercent_trans <- (100-dat$SandPercent)^(1/4)
  summary(dat$SandPercent_trans)
  
  ## other transformation
  dat$Analysed_meanSST_Celsius_sqrt  <- sqrt(dat$Analysed_meanSST_Celsius)
  dat$meanChlorophyll_sqrt   <- sqrt(dat$meanChlorophyll)
  #dat$Fishing_hour_mean_sqrt <- sqrt(dat$Fishing_hour_mean) ## too many NA, not used
  
  ## distance to shipping line, 4th root transformation?
  dat$DistanceShippingLanes_sqrt     <- sqrt(dat$DistanceShippingLanesInKm)
  dat$DistanceShippingLanes_4root    <- sqrt(dat$DistanceShippingLanesInKm)
  

  if ("Fishing_hour_average_2009_2020" %in% names(dat) ) {
    ## input data
    ## for input data (modeling), use Fishing_hour_average_2009_2020
    ## beacuse missing Fishing_hour_mean data from 2000-2007
    dat$Fishing_hour_average_2009_2020_sqrt <- sqrt(dat$Fishing_hour_average_2009_2020)
    ## fishing hour into categorical variable
    fishcut <- quantile(dat$Fishing_hour_average_2009_2020, prob=c(0.25, 0.5, 0.75,1))
    dat$Fishing_hour_averagec <- cut(dat$Fishing_hour_average_2009_2020, breaks=c(0,fishcut))
    table(dat$Fishing_hour_averagec, useNA="always")
  } 
  
  if ("dens" %in% names(dat) ) {
    ## log-transform of density
    #dat$dens_log  <- log(dat$dens+0.1)
    dat$dens_sqrt  <- sqrt(dat$dens)
    dat$dens_4root <- (dat$dens)^(1/4)
    ## presence-absence
    dat$presence <- 1
    dat$presence[dat$dens==0] <- 0
  }

  return(dat)
  
}

define_season_group1 <- function(ddd){
  ## define bi-month
  myseason <- c("dec-jan", "feb-mrch", "apr-may", "jun-jul", "aug-sep", "oct-nov")
  ddd$season <- "dec-jan"
  ddd$season[ddd$month %in% c("02","03")] <- "feb-mrch"
  ddd$season[ddd$month %in% c("04","05")] <- "apr-may"
  ddd$season[ddd$month %in% c("06","07")] <- "jun-jul"
  ddd$season[ddd$month %in% c("08","09")] <- "aug-sep"
  ddd$season[ddd$month %in% c("10","11")] <- "oct-nov"
  ddd$season <- factor(ddd$season, levels=myseason)
  return(ddd)
}

define_season_group2 <- function(ddd){
  ## define bi-month
  myseason <- c("jan-feb", "mrch-apr", "may-jun", "jul-aug", "sep-oct", "nov-dec")
  ddd$season <- "jan-feb"
  ddd$season[ddd$month %in% c("03","04")] <- "mrch-apr"
  ddd$season[ddd$month %in% c("05","06")] <- "may-jun"
  ddd$season[ddd$month %in% c("07","08")] <- "jul-aug"
  ddd$season[ddd$month %in% c("09","10")] <- "sep-oct"
  ddd$season[ddd$month %in% c("11","12")] <- "nov-dec"
  ddd$season <- factor(ddd$season, levels=myseason)
  return(ddd)
}

extra_data_processing_kittiwake <- function(ddd) {
  ## exclude NA? species specific?
  ddd1       <- ddd[!is.na(ddd$distance_coast_avg_sqrt) & !is.na(ddd$Analysed_meanSST_Celsius) & !is.na(ddd$SandPercent),]
  return(ddd1)
  
}

extra_data_processing_gannet <- function(ddd) {
  ## select period
  ## since 2014 the sampling is consistent, with distance analysis
  ## missing SST data in 2021, and also 2021 data was incomplete, missing few months
  ## use Year_bimonthly
  ddd  <- ddd[ddd$Year_bimonthly >=2000 & ddd$Year_bimonthly<=2020,]
  
  ## re-define fishing in groups, since <=2020 data was excluded
  #ddd$Fishing_hour_average_2009_2020_sqrt <- sqrt(ddd$Fishing_hour_average_2009_2020)
  ## fishing hour into categorical variable
  fishcut <- quantile(ddd$Fishing_hour_average_2009_2020, prob=c(0.25, 0.5, 0.75,1))
  ddd$Fishing_hour_averagec <- cut(ddd$Fishing_hour_average_2009_2020, breaks=c(0,fishcut))
  table(ddd$Fishing_hour_averagec, useNA="always")
  
  ## exclude few west observations
  ## those points are nolonger in >=2000 data
  #dat$side[dat$lon<2.365] <- "west"
  
  ## exclude NA? species specific?
  ## No missing data

  return(ddd)
  
}

MyStd <- function(x) { 
  (x - mean(x)) / sd(x)
}

data_standardization <- function(ddd) {
  ## standadizing covariates
  ddd$esa.std               <- MyStd(ddd$esa)
  ddd$depth1.std            <- MyStd(ddd$depth1)
  #ddd$depth1_sqrt.std            <- MyStd(ddd$depth1_sqrt)
  ddd$MudPercent.std        <- MyStd(ddd$MudPercent_trans)
  ddd$SandPercent.std        <- MyStd(ddd$SandPercent_trans)
  ddd$Analysed_meanSST_Celsius.std      <- MyStd(ddd$Analysed_meanSST_Celsius)
  ddd$meanChlorophyll_sqrt.std               <- MyStd(ddd$meanChlorophyll_sqrt)
  ddd$Distance_big_breeding_site_km.std <- MyStd(ddd$Distance_big_breeding_site_km)
  ddd$DistanceShippingLanes_4root.std   <- MyStd(ddd$DistanceShippingLanes_4root)
  ddd$Distance_platform_sqrt.std        <- MyStd(ddd$Distance_platform_sqrt)
  
  ## save parameter
  ddd$mean_esa             <- mean(ddd$esa)
  ddd$sd_esa               <- sd(ddd$esa)
  ddd$mean_depth1          <- mean(ddd$depth1)
  ddd$sd_depth1            <- sd(ddd$depth1)
  ddd$mean_MudPercent      <- mean(ddd$MudPercent_trans)
  ddd$sd_MudPercent        <- sd(ddd$MudPercent_trans)
  ddd$mean_SandPercent      <- mean(ddd$SandPercent_trans)
  ddd$sd_SandPercent        <- sd(ddd$SandPercent_trans)
  ddd$mean_SST             <- mean(ddd$Analysed_meanSST_Celsius)
  ddd$sd_SST               <- sd(ddd$Analysed_meanSST_Celsius)
  ddd$mean_Chlorophyll     <- mean(ddd$meanChlorophyll_sqrt)
  ddd$sd_Chlorophyll       <- sd(ddd$meanChlorophyll_sqrt)
  ddd$mean_dis_breed       <- mean(ddd$Distance_big_breeding_site_km)
  ddd$sd_dis_breed         <- sd(ddd$Distance_big_breeding_site_km)
  ddd$mean_dis_ship       <- mean(ddd$DistanceShippingLanes_4root)
  ddd$sd_dis_ship         <- sd(ddd$DistanceShippingLanes_4root)
  ddd$mean_dis_platform       <- mean(ddd$Distance_platform_sqrt)
  ddd$sd_dis_platform         <- sd(ddd$Distance_platform_sqrt)
  
  return(ddd)
}

data_standardization_map <- function(dd, imap) {
  ## standardizing covariates
  imap$esa.std          <- (imap$esa-unique(dd$mean_esa))/unique(dd$sd_esa)
  imap$depth1.std       <- (imap$depth1-unique(dd$mean_depth1))/unique(dd$sd_depth1)
  
  imap$MudPercent.std        <- (imap$MudPercent_trans-unique(dd$mean_MudPercent))/unique(dd$sd_MudPercent)
  imap$SandPercent.std       <- (imap$SandPercent_trans-unique(dd$mean_SandPercent))/unique(dd$sd_SandPercent)
  
  imap$Analysed_meanSST_Celsius.std      <- (imap$Analysed_meanSST_Celsius-unique(dd$mean_SST))/unique(dd$sd_SST)
  imap$meanChlorophyll_sqrt.std          <- (imap$meanChlorophyll_sqrt-unique(dd$mean_Chlorophyll))/unique(dd$sd_Chlorophyll)
  
  imap$Distance_big_breeding_site_km.std <- (imap$Distance_big_breeding_site_km-unique(dd$mean_dis_breed))/unique(dd$sd_dis_breed)
  imap$DistanceShippingLanes_4root.std   <- (imap$DistanceShippingLanes_4root-unique(dd$mean_dis_ship))/unique(dd$sd_dis_ship)
  imap$Distance_platform_sqrt.std        <- (imap$Distance_platform_sqrt-unique(dd$mean_dis_platform))/unique(dd$sd_dis_platform)
  
  return(imap)
}

plot_extreme_value_temporal <- function(ddd, qq) {
  aa1 <- data.frame(table(ddd[ddd$dens>qq[1],]$Year_bimonthly))
  plot(as.numeric(levels(aa1$Var1))[aa1$Var1], aa1$Freq, type="o", lwd=2, xlab="season year", ylab="NO. segment with extreme dens", main=iss)
  aa1 <- data.frame(table(ddd[ddd$dens>qq[2],]$Year_bimonthly))
  lines(as.numeric(levels(aa1$Var1))[aa1$Var1], aa1$Freq, type="o", lwd=2, col="red")
  aa1 <- data.frame(table(ddd[ddd$dens>qq[3],]$Year_bimonthly))
  lines(as.numeric(levels(aa1$Var1))[aa1$Var1], aa1$Freq, type="o", lwd=2, col="blue")
  aa1 <- data.frame(table(ddd[ddd$dens>qq[4],]$Year_bimonthly))
  lines(as.numeric(levels(aa1$Var1))[aa1$Var1], aa1$Freq, type="o", lwd=2, col="green")
}



plot_conditional_esa <- function(idat, ifit) {
  ddd1 <- expand.grid(esa = seq(min(idat$esa), max(idat$esa), 0.2),
                      distance_coast_avg_sqrt  = median(idat$distance_coast_avg_sqrt),
                      Analysed_meanSST_Celsius = median(idat$Analysed_meanSST_Celsius),
                      DistanceShippingLanes_sqrt = median(idat$DistanceShippingLanes_sqrt),
                      Distance_breeding_site_sqrt = median(idat$Distance_breeding_site_sqrt),
                      MudPercent_trans = median(idat$MudPercent_trans),
                      platform         = c("aerial"),
                      InWindFarm       = c("Yes"))
  p    <- predict(ifit, newdata=ddd1, 
                  type = "link",
                  se_fit=TRUE, 
                  re_form=NA, re_form_iid = NA)
  f1 <- ggplot(p, 
               aes(esa, exp(est),
                   ymin = exp(est - 1.96 * est_se),
                   ymax = exp(est + 1.96 * est_se))) +
    geom_line(size=1) +
    geom_ribbon(alpha = 0.4) +
    geom_rug(data=idat,aes(x=esa),inherit.aes = F) +
    #scale_x_continuous(breaks=seq(1, 12, 1), labels=seq(1, 12, 1)) +
    coord_cartesian(expand = F) +
    labs(x = "esa log", y = "est density (n/km2)")+
    ggtitle("esa")
  #facet_wrap(~PGM_CODE)
  return(f1)
}

plot_conditional_dis_coast <- function(idat, ifit) {
  ddd1 <- expand.grid(distance_coast_avg_sqrt  = seq(min(idat$distance_coast_avg_sqrt), max(idat$distance_coast_avg_sqrt), 5),
                      Analysed_meanSST_Celsius = median(idat$Analysed_meanSST_Celsius),
                      DistanceShippingLanes_sqrt = median(idat$DistanceShippingLanes_sqrt),
                      Distance_breeding_site_sqrt = median(idat$Distance_breeding_site_sqrt),
                      MudPercent_trans = median(idat$MudPercent_trans),
                      esa              = median(idat$esa),
                      platform         = c("aerial"),
                      InWindFarm       = c("Yes"))
  p    <- predict(ifit, newdata=ddd1, 
                  type = "link",
                  se_fit=TRUE, 
                  re_form=NA, re_form_iid = NA)
  f1 <- ggplot(p, 
               aes(distance_coast_avg_sqrt, exp(est),
                   ymin = exp(est - 1.96 * est_se),
                   ymax = exp(est + 1.96 * est_se))) +
    geom_line(size=1) +
    geom_ribbon(alpha = 0.4) +
    geom_rug(data=idat,aes(x=distance_coast_avg_sqrt),inherit.aes = F) +
    #scale_x_continuous(breaks=seq(1, 12, 1), labels=seq(1, 12, 1)) +
    coord_cartesian(expand = F) +
    labs(x = "distance_coast_avg_sqrt (km)", y = "est density (n/km2)")+
    ggtitle("dis to coast")
  #facet_wrap(~PGM_CODE)
  return(f1)
}



plot_conditional_SST <- function(idat, ifit) {
  ddd1 <- expand.grid(Analysed_meanSST_Celsius  = seq(min(idat$Analysed_meanSST_Celsius), max(idat$Analysed_meanSST_Celsius), 2),
                      distance_coast_avg_sqrt = median(idat$distance_coast_avg_sqrt),
                      DistanceShippingLanes_sqrt = median(idat$DistanceShippingLanes_sqrt),
                      Distance_breeding_site_sqrt = median(idat$Distance_breeding_site_sqrt),
                      MudPercent_trans = median(idat$MudPercent_trans),
                      esa              = median(idat$esa),
                      platform         = c("aerial"),
                      InWindFarm       = c("Yes"))
  p    <- predict(ifit, newdata=ddd1, 
                  type = "link",
                  se_fit=TRUE, 
                  re_form=NA, re_form_iid = NA)
  f1 <- ggplot(p, 
               aes(Analysed_meanSST_Celsius, exp(est),
                   ymin = exp(est - 1.96 * est_se),
                   ymax = exp(est + 1.96 * est_se))) +
    geom_line(size=1) +
    geom_ribbon(alpha = 0.4) +
    geom_rug(data=idat,aes(x=Analysed_meanSST_Celsius),inherit.aes = F) +
    #scale_x_continuous(breaks=seq(1, 12, 1), labels=seq(1, 12, 1)) +
    coord_cartesian(expand = F) +
    labs(x = "Analysed_meanSST (Celsius)", y = "est density (n/km2)")+
    ggtitle("SST")
  #facet_wrap(~PGM_CODE)
  return(f1)
}


plot_conditional_dis_shipping <- function(idat, ifit) {
  ddd1 <- expand.grid(DistanceShippingLanes_sqrt  = seq(min(idat$DistanceShippingLanes_sqrt), max(idat$DistanceShippingLanes_sqrt), 2),
                      distance_coast_avg_sqrt = median(idat$distance_coast_avg_sqrt),
                      Analysed_meanSST_Celsius = median(idat$Analysed_meanSST_Celsius),
                      Distance_breeding_site_sqrt = median(idat$Distance_breeding_site_sqrt),
                      MudPercent_trans = median(idat$MudPercent_trans),
                      esa              = median(idat$esa),
                      platform         = c("aerial"),
                      InWindFarm       = c("Yes"))
  p    <- predict(ifit, newdata=ddd1, 
                  type = "link",
                  se_fit=TRUE, 
                  re_form=NA, re_form_iid = NA)
  f1 <- ggplot(p, 
               aes(DistanceShippingLanes_sqrt, exp(est),
                   ymin = exp(est - 1.96 * est_se),
                   ymax = exp(est + 1.96 * est_se))) +
    geom_line(size=1) +
    geom_ribbon(alpha = 0.4) +
    geom_rug(data=idat,aes(x=DistanceShippingLanes_sqrt),inherit.aes = F) +
    #scale_x_continuous(breaks=seq(1, 12, 1), labels=seq(1, 12, 1)) +
    coord_cartesian(expand = F) +
    labs(x = "Distance to shipping line sqrt(km)", y = "est density (n/km2)")+
    ggtitle("dis to shipping line")
  #facet_wrap(~PGM_CODE)
  return(f1)
}


plot_conditional_dis_breeding <- function(idat, ifit) {
  ddd1 <- expand.grid(Distance_breeding_site_sqrt  = seq(min(idat$Distance_breeding_site_sqrt), max(idat$Distance_breeding_site_sqrt), 2),
                      distance_coast_avg_sqrt = median(idat$distance_coast_avg_sqrt),
                      Analysed_meanSST_Celsius = median(idat$Analysed_meanSST_Celsius),
                      DistanceShippingLanes_sqrt = median(idat$DistanceShippingLanes_sqrt),
                      MudPercent_trans = median(idat$MudPercent_trans),
                      esa              = median(idat$esa),
                      platform         = c("aerial"),
                      InWindFarm       = c("Yes"))
  p    <- predict(ifit, newdata=ddd1, 
                  type = "link",
                  se_fit=TRUE, 
                  re_form=NA, re_form_iid = NA)
  f1 <- ggplot(p, 
               aes(Distance_breeding_site_sqrt, exp(est),
                   ymin = exp(est - 1.96 * est_se),
                   ymax = exp(est + 1.96 * est_se))) +
    geom_line(size=1) +
    geom_ribbon(alpha = 0.4) +
    geom_rug(data=idat,aes(x=Distance_breeding_site_sqrt),inherit.aes = F) +
    #scale_x_continuous(breaks=seq(1, 12, 1), labels=seq(1, 12, 1)) +
    coord_cartesian(expand = F) +
    labs(x = "Distance to breeding site sqrt (km)", y = "est density (n/km2)")+
    ggtitle("dis to breeding site")
  #facet_wrap(~PGM_CODE)
  return(f1)
}


plot_conditional_mud_perc <- function(idat, ifit) {
  isetp <- (max(idat$MudPercent_trans)-min(idat$MudPercent_trans))/10
  ddd1 <- expand.grid(MudPercent_trans  = seq(min(idat$MudPercent_trans), max(idat$MudPercent_trans), isetp),
                      distance_coast_avg_sqrt = median(idat$distance_coast_avg_sqrt),
                      Analysed_meanSST_Celsius = median(idat$Analysed_meanSST_Celsius),
                      DistanceShippingLanes_sqrt = median(idat$DistanceShippingLanes_sqrt),
                      Distance_breeding_site_sqrt = median(idat$Distance_breeding_site_sqrt),
                      esa              = median(idat$esa),
                      platform         = c("aerial"),
                      InWindFarm       = c("Yes"))
  p    <- predict(ifit, newdata=ddd1, 
                  type = "link",
                  se_fit=TRUE, 
                  re_form=NA, re_form_iid = NA)
  f1 <- ggplot(p, 
               aes(MudPercent_trans, exp(est),
                   ymin = exp(est - 1.96 * est_se),
                   ymax = exp(est + 1.96 * est_se))) +
    geom_line(size=1) +
    geom_ribbon(alpha = 0.4) +
    geom_rug(data=idat,aes(x=MudPercent_trans),inherit.aes = F) +
    #scale_x_continuous(breaks=seq(1, 12, 1), labels=seq(1, 12, 1)) +
    coord_cartesian(expand = F) +
    labs(x = "Mud percentage trans (%)", y = "est density (n/km2)")+
    ggtitle("mud percentage")
  #facet_wrap(~PGM_CODE)
  return(f1)
}

plot_map <- function(dat, column) {
  ggplot(dat, aes(x_utm1, y_utm1, fill = {{ column }})) +
    geom_raster() +
    coord_fixed()
}

plot_prediction_all <- function(pred) {
  
  ## 01. predictions that incorporate all fixed effects and random effects
  f1 <- plot_map(pred, exp(est)) +
    scale_fill_viridis_c(
      trans = "sqrt",
      # trim extreme high values to make spatial variation more visible
      na.value = "yellow", limits = c(0, quantile(exp(pred$est), 0.995))
    ) +
    ggtitle("Prediction (fixed effects + all random effects)",
            subtitle = paste("maximum estimated density =", round(max(exp(pred$est))))
    )
  
  ## 02. fixed effects
  f2 <- plot_map(pred, exp(est_non_rf)) +
    scale_fill_viridis_c(trans = "sqrt") +
    ggtitle("Prediction (fixed effects only)")
  
  ## 03. spatial random effect
  f3 <- plot_map(pred, exp(omega_s)) +
    #scale_fill_gradient2() +
    scale_fill_viridis_c(trans = "sqrt") +
    ggtitle("Spatial random effects only")
  
  ## plot CV of estimates
  f4 <- ggplot(pred, aes(x_utm1, y_utm1, fill = cv)) +
    geom_raster() +
    scale_fill_viridis_c(
      trans = "sqrt",
      # trim extreme high values to make spatial variation more visible
      na.value = "yellow", limits = c(0, quantile(exp(pred$est), 0.995))
    ) +
    ggtitle("CV of fitted distribtion")
  
  
  
  (f1 | f4)/
    (f2 | f3)
  
}


plot_prediction_sub <- function(pred, iseason) {
  
  ## 01. predictions that incorporate all fixed effects and random effects
  f1 <- plot_map(pred, exp(est)) +
    scale_fill_viridis_c(
      trans = "sqrt",
      # trim extreme high values to make spatial variation more visible
      na.value = "yellow", limits = c(0, quantile(exp(pred$est), 0.995))
    ) +
    #ggtitle(paste0(iseason, ": predicted density (n/km^2)"),
    #        subtitle = paste("maximum estimated density =", round(max(exp(pred$est))))
    #)+
    ggtitle(paste0(iseason))+
    xlab("") +
    ylab("") +
    theme( legend.position = "bottom", 
           legend.title = element_text(colour="black", size=10, face="bold"))
  
  ## plot CV of estimates
  f4 <- ggplot(pred, aes(x_utm1, y_utm1, fill = cv)) +
    geom_raster() +
    #scale_fill_viridis_c(
      # trim extreme high values to make spatial variation more visible
    #  na.value = "yellow", limits = c(0, 6)
    #) +
    scale_fill_viridis_c(
      trans = "sqrt",
      # trim extreme high values to make spatial variation more visible
      na.value = "yellow", limits = c(0, quantile(exp(pred$est), 0.995))
    ) +
    xlab("") +
    ylab("") +
    ggtitle(paste0(iseason))+
    theme( legend.position = "bottom", 
           legend.title = element_text(colour="black", size=10, face="bold"))
  
  
  #(f1 | f4)
  f1 <- f1/f4
  return(f1)
  
}

plot_prediction_fixed_dis_coast <- function(dd1, ifit, ispe) {
  
  ## platform=aerial; esa=median(dd1)
  ## get grid
  map1 <- readRDS(file=paste0(data_path, "Prediction grid/Prediction_grid_all_Bimonthly.rds"))
  map1  <- as.data.frame(map1)
  head(map1)
  map1 <- map1[map1$scientific_name == ispe,]
  map1$x_utm1 <- map1$x/1000
  map1$y_utm1 <- map1$y/1000
  map1$esa    <- median(dd1$esa)  ## dont have esa value in the map
  ## exclude grid outside the range of data
  map1$platform <- "aerial"
  nrow(map1)
  dnew <- map1[map1$x >= min(dd1$x_utm) & map1$x <= max(dd1$x_utm)
               & map1$y >= min(dd1$y_utm)
               & map1$y <= max(dd1$y_utm),]
  
  ## select any season
  table(dnew$Bimonthly)
  dnew <- dnew[dnew$Bimonthly == "aug-sep",] ## anymonth is fine
  
  # data transformation and assign other levels to zero
  ## sqrt transformation of distance to coast
  dnew$distance_coast_avg_sqrt <- sqrt(dnew$distance_coast_avg)
  ## sqrt transformation of distance to breeding site
  #dnew$Distance_breeding_site_sqrt <- sqrt(dnew$Distance_breeding_site_km)
  dnew$Distance_breeding_site_sqrt <- 0
  ## transformation of mud/sand percentage
  #dnew$MudPercent_trans <- dnew$MudPercent^(1/4)
  dnew$MudPercent_trans <- 0
  ## other transformation
  #dat$Analysed_meanSST_Celsius_sqrt  <- sqrt(dat$Analysed_meanSST_Celsius) 
  dnew$Analysed_meanSST_Celsius <- 0
  #dnew$DistanceShippingLanes_sqrt     <- sqrt(dnew$DistanceShippingLanesInKm)
  dnew$DistanceShippingLanes_sqrt     <- 0
  dnew$InWindFarm                     <- "Yes"
  
  ## exclude NA
  dnew       <- dnew[!is.na(dnew$distance_coast_avg_sqrt) & !is.na(dnew$Analysed_meanSST_Celsius) & !is.na(dnew$SandPercent),]
  pred  <- predict(ifit, newdata = dnew, 
                   type="link",
                   se_fit=FALSE,
                   re_form_iid = NULL,
                   re_form = NULL)
  ## plot prediction from this fixed effect, given aerial, esa
  f2 <- plot_map(pred, exp(est_non_rf)) +
    scale_fill_viridis_c(trans = "sqrt") +
    ggtitle("effect_dis_coast")
  
  return(list(pred,f2))
  
}
plot_prediction_fixed_SST <- function(dd1, ifit, ispe) {
  
  ## get grid
  map1  <- readRDS(file=paste0(data_path, "Prediction grid/Prediction_grid_all_Bimonthly.rds"))
  map1  <- as.data.frame(map1)
  head(map1)
  map1  <- map1[map1$scientific_name == ispe,]
  map1$x_utm1 <- map1$x/1000
  map1$y_utm1 <- map1$y/1000
  map1$esa    <- median(dd1$esa)  ## dont have esa value in the map
  ## exclude grid outside the range of data
  map1$platform <- "aerial"
  nrow(map1)
  dnew <- map1[map1$x >= min(dd1$x_utm) & map1$x <= max(dd1$x_utm)
               & map1$y >= min(dd1$y_utm)
               & map1$y <= max(dd1$y_utm),]
  
  ## select iseason
  table(dnew$Bimonthly)
  dnew <- dnew[dnew$Bimonthly == "aug-sep",] ## anymonth is fine
  
  # data transformation
  ## sqrt transformation of distance to coast
  #dnew$distance_coast_avg_sqrt <- sqrt(dnew$distance_coast_avg)
  dnew$distance_coast_avg_sqrt <- 0
  ## sqrt transformation of distance to breeding site
  #dnew$Distance_breeding_site_sqrt <- sqrt(dnew$Distance_breeding_site_km)
  dnew$Distance_breeding_site_sqrt <- 0
  ## transformation of mud/sand percentage
  #dnew$MudPercent_trans <- dnew$MudPercent^(1/4)
  dnew$MudPercent_trans <- 0
  ## other transformation
  #dat$Analysed_meanSST_Celsius_sqrt  <- sqrt(dat$Analysed_meanSST_Celsius) 
  #dnew$DistanceShippingLanes_sqrt     <- sqrt(dnew$DistanceShippingLanesInKm)
  dnew$DistanceShippingLanes_sqrt     <- 0
  dnew$InWindFarm                     <- "Yes"
  
  ## exclude NA
  dnew       <- dnew[!is.na(dnew$distance_coast_avg_sqrt) & !is.na(dnew$Analysed_meanSST_Celsius) & !is.na(dnew$SandPercent),]
  pred  <- predict(ifit, newdata = dnew, 
                   type="link",
                   se_fit=FALSE,
                   re_form_iid = NULL,
                   re_form = NULL)
  ## plot prediction from this fixed effect, given aerial, esa
  f2 <- plot_map(pred, exp(est_non_rf)) +
    scale_fill_viridis_c(trans = "sqrt") +
    ggtitle("effect_SST")
  
  return(list(pred,f2))
  
}
plot_prediction_fixed_mud <- function(dd1, ifit, ispe) {
  
  ## get grid
  map1  <- readRDS(file=paste0(data_path, "Prediction grid/Prediction_grid_all_Bimonthly.rds"))
  map1  <- as.data.frame(map1)
  head(map1)
  map1  <- map1[map1$scientific_name == ispe,]
  map1$x_utm1 <- map1$x/1000
  map1$y_utm1 <- map1$y/1000
  map1$esa    <- median(dd1$esa)  ## dont have esa value in the map
  ## exclude grid outside the range of data
  map1$platform <- "aerial"
  nrow(map1)
  dnew <- map1[map1$x >= min(dd1$x_utm) & map1$x <= max(dd1$x_utm)
               & map1$y >= min(dd1$y_utm)
               & map1$y <= max(dd1$y_utm),]
  
  ## select iseason
  table(dnew$Bimonthly)
  dnew <- dnew[dnew$Bimonthly == "aug-sep",] ## anymonth is fine
  
  # data transformation
  ## sqrt transformation of distance to coast
  #dnew$distance_coast_avg_sqrt <- sqrt(dnew$distance_coast_avg)
  dnew$distance_coast_avg_sqrt <- 0
  ## sqrt transformation of distance to breeding site
  #dnew$Distance_breeding_site_sqrt <- sqrt(dnew$Distance_breeding_site_km)
  dnew$Distance_breeding_site_sqrt <- 0
  ## transformation of mud/sand percentage
  dnew$MudPercent_trans <- dnew$MudPercent^(1/4)
  #dnew$MudPercent_trans <- 0
  ## other transformation
  #dat$Analysed_meanSST_Celsius_sqrt  <- sqrt(dat$Analysed_meanSST_Celsius) 
  dnew$Analysed_meanSST_Celsius <- 0
  #dnew$DistanceShippingLanes_sqrt     <- sqrt(dnew$DistanceShippingLanesInKm)
  dnew$DistanceShippingLanes_sqrt     <- 0
  dnew$InWindFarm                     <- "Yes"
  
  ## exclude NA
  dnew       <- dnew[!is.na(dnew$distance_coast_avg_sqrt) & !is.na(dnew$Analysed_meanSST_Celsius) & !is.na(dnew$SandPercent),]
  pred  <- predict(ifit, newdata = dnew, 
                   type="link",
                   se_fit=FALSE,
                   re_form_iid = NULL,
                   re_form = NULL)
  ## plot prediction from this fixed effect, given aerial, esa
  f2 <- plot_map(pred, exp(est_non_rf)) +
    scale_fill_viridis_c(trans = "sqrt") +
    ggtitle("effect_mud")
  
  return(list(pred,f2))
  
}
plot_prediction_fixed_windfarm <- function(dd1, ifit, ispe) {
  
  ## get grid
  map1  <- readRDS(file=paste0(data_path, "Prediction grid/Prediction_grid_all_Bimonthly.rds"))
  map1  <- as.data.frame(map1)
  head(map1)
  map1  <- map1[map1$scientific_name == ispe,]
  map1$x_utm1 <- map1$x/1000
  map1$y_utm1 <- map1$y/1000
  map1$esa    <- median(dd1$esa)  ## dont have esa value in the map
  ## exclude grid outside the range of data
  map1$platform <- "aerial"
  nrow(map1)
  dnew <- map1[map1$x >= min(dd1$x_utm) & map1$x <= max(dd1$x_utm)
               & map1$y >= min(dd1$y_utm)
               & map1$y <= max(dd1$y_utm),]
  
  ## select iseason
  table(dnew$Bimonthly)
  dnew <- dnew[dnew$Bimonthly == "aug-sep",] ## anymonth is fine
  
  # data transformation
  ## sqrt transformation of distance to coast
  #dnew$distance_coast_avg_sqrt <- sqrt(dnew$distance_coast_avg)
  dnew$distance_coast_avg_sqrt <- 0
  ## sqrt transformation of distance to breeding site
  #dnew$Distance_breeding_site_sqrt <- sqrt(dnew$Distance_breeding_site_km)
  dnew$Distance_breeding_site_sqrt <- 0
  ## transformation of mud/sand percentage
  #dnew$MudPercent_trans <- dnew$MudPercent^(1/4)
  dnew$MudPercent_trans <- 0
  ## other transformation
  #dat$Analysed_meanSST_Celsius_sqrt  <- sqrt(dat$Analysed_meanSST_Celsius) 
  #dnew$DistanceShippingLanes_sqrt     <- sqrt(dnew$DistanceShippingLanesInKm)
  dnew$DistanceShippingLanes_sqrt     <- 0
  dnew$Analysed_meanSST_Celsius       <- 0
 
  
  ## exclude NA
  dnew       <- dnew[!is.na(dnew$distance_coast_avg_sqrt) & !is.na(dnew$Analysed_meanSST_Celsius) & !is.na(dnew$SandPercent),]
  pred  <- predict(ifit, newdata = dnew, 
                   type="link",
                   se_fit=FALSE,
                   re_form_iid = NULL,
                   re_form = NULL)
  ## plot prediction from this fixed effect, given aerial, esa
  f2 <- plot_map(pred, exp(est_non_rf)) +
    scale_fill_viridis_c(trans = "sqrt") +
    ggtitle("effect_windfarm")
  
  return(list(pred,f2))
  
}


plot_map2 <- function(dat) {
  dat1 <- st_as_sf(dat, coords = c("x","y"), 
                   crs = 32631, agr = "constant")
  ylim_min <- min(dat$y, na.rm = T)
  ylim_max <- max(dat$y, na.rm = T)
  xlim_min <- min(dat$x, na.rm = T)
  xlim_max <- max(dat$x, na.rm = T)
  xlim <- c(xlim_min - 10000, xlim_max + 10000)
  ylim <- c(ylim_min - 10000, ylim_max + 10000)
  #dat1 <-  st_transform(dat1, crs = 4326)
  #aa <- sfheaders::sf_to_df(dat1)
  #dat$lon <- aa$x
  #dat$lat <- aa$y 
  ggplot() +
    #geom_raster(pred1, aes(x, y, fill = exp(est))) +
    geom_sf(data = world) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    #geom_sf(data = dat1, aes(geometry = geometry, fill = exp(est)))
    geom_raster(data = dat, aes(x=x, y=y,fill = exp(est))) +
    scale_fill_viridis_c(
      trans = "sqrt",
      # trim extreme high values to make spatial variation more visible
      na.value = "yellow", limits = c(0, quantile(exp(dat$est), 0.995))
    ) + 
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
}


plot_CV2 <- function(dat) {
  #dat1 <- st_as_sf(dat, coords = c("x","y"), 
  #                 crs = 32631, agr = "constant")
  ylim_min <- min(dat$y, na.rm = T)
  ylim_max <- max(dat$y, na.rm = T)
  xlim_min <- min(dat$x, na.rm = T)
  xlim_max <- max(dat$x, na.rm = T)
  xlim <- c(xlim_min - 10000, xlim_max + 10000)
  ylim <- c(ylim_min - 10000, ylim_max + 10000)
  #dat1 <-  st_transform(dat1, crs = 4326)
  #aa <- sfheaders::sf_to_df(dat1)
  #dat$lon <- aa$x
  #dat$lat <- aa$y 
  ggplot() +
    geom_sf(data = world) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    geom_raster(data = dat, aes(x=x, y=y,fill = cv)) +
    scale_fill_viridis_c(
      # trim extreme high values to make spatial variation more visible
      na.value = "yellow", limits = c(0, 6)
    )+ 
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
}


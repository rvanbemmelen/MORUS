## this is a script template to run model
## please copy paste this script in your own folder/or change the name and run it
## In this script you need to:

## 1) define your species and prepare the folder to store output
## 2) define the full covariates list you want to test 
## 3) Look at the results and decide on a second or third round etc. 

rm(list = ls())


# Variables --------------------------------------------------------------------
iplatform <- "aerial"

## Fill in species. FOr instance Herring gull
ispe  <- "European_Herring_Gull"
ispe1 <- "European_Herring_Gull"

## Limit smoother shape to number of knots you want
myk = 5

# Directories ------------------------------------------------------------------
# Make sure to create these locations in the direction you use
project_path <- "C:/Users/0_2023_bird_map/" # Define 
script_path  <- paste0(project_path, "Script/")
data_path1   <- paste0(project_path, "Data/")
data_path    <- paste0(project_path, "Data/", ispe1,"/")
fig_path     <- paste0(project_path, "Figure/", ispe1, "/")
output_path  <- paste0(project_path, "model_output/", ispe1, "/")
setwd(script_path)
source(paste0(script_path, "functions_model.r"))

# Libraries --------------------------------------------------------------------
library(parallel)
library(dplyr)
library(ggplot2)
library(sdmTMB)
library(sdmTMBextra)
library(patchwork)

# Run model function -----------------------------------------------------------
run_model <- function(i) {
  tryCatch({
    ## add yearc covarates
    mystr1      <- mymodels$covariates[i]
    myformula1  <- as.formula(mystr1)
    mod1        <- mymodels$mod[i]
    type1       <- mymodels$type[i]
    if (mymodels$spatiotemporal[i] == "year-varying"){
      time1   <- "Year_bimonthly"
      myfit   <- fit_TV(dd, mesh, myformula1, itime=time1, itype=type1, iseason, imodel=mod1, iresponse="sqrt_dens")
      sanity(myfit[[1]])
      return(myfit[[2]])
    }
    if (mymodels$spatiotemporal[i] == "period-varying"){
      time1   <- "periodc"
      myfit   <- fit_TV(dd, mesh, myformula1, itime=time1, itype=type1, iseason, imodel=mod1, iresponse="sqrt_dens")
      sanity(myfit[[1]])
      return(myfit[[2]])
    }
    if (mymodels$spatiotemporal[i] == "off"){
      myfit   <- fit_fixed(dd, myformula1, iseason, mod1, iresponse="sqrt_dens")
      sanity(myfit[[1]])
      return(myfit[[2]])
    }
  }, error = function(e) -999)
}


# 0. Round 1 -------------------------------------------------------------------
## Start with all covariates
## Load Data ready for modelling
load(file=paste0(data_path, "processed_data_", ispe, "_platform_", iplatform,"_for_modeling.RData"))
dat$periodc <- cut(dat$Year_bimonthly, breaks=c(1999,2005, 2010, 2015, 2020))
table(dat$periodc, dat$Year_bimonthly, useNA="always")
dat$period  <- as.integer(dat$periodc)

myvar = c("s(Analysed_meanSST_Celsius.std, k = myk)", "s(meanChlorophyll_sqrt.std, k = myk)","Fishing_hour_averagec")

## Select season
seasons = unique(dat$season)
#seasons = seasons[c(1,4,5)]
seasons = droplevels(seasons)
unique(seasons)

## Run for each season
start = Sys.time()

for(iseason in seasons){
  dd <- dat[dat$season == iseason,]
  
  ## Response
  dd$response <- dd$dens_sqrt
  
  ## Arrange year period variables
  dd$yearc    <- factor(dd$Year_bimonthly)
  #levels(dd$yearc)
  
  ## Mesh
  cutoff <- 20
  mesh <- make_mesh(dd, xy_cols = c("x_utm1", "y_utm1"), cutoff = cutoff)
  #plot(mesh)
  mesh$mesh$n
  
  ## Full formula: myvar
  mystr <- paste0("response ~ ", paste0(myvar, collapse = "+"))
  
  # b00 only spatiotemporal # not use b00 and b01
  #mymodels <- data.frame(mod   = "b00", covariates = "response ~ 1") 
  # 01 only fixed effects
  ## full formula
  #temmod <- data.frame(mod   = "b01", covariates = mystr)
  #mymodels <- rbind(mymodels, temmod)
  # b02 full effects
  temmod <- data.frame(mod   = "b02", covariates = mystr)
  mymodels = data.frame()
  mymodels <- rbind(mymodels, temmod)
  
  # b1X - delete one covariate
  for (i in 1:length(myvar)) {
    if (i==1) {
      idel   <- paste0(myvar[i], "+")
      mystr1 <- gsub(idel, "", mystr, fixed =T)
    } else {
      idel   <- paste0("+", myvar[i])
      mystr1 <- gsub(idel, "", mystr, fixed =T)
    }
    imod   <- paste0("b1",i)
    temmod <- data.frame(mod   = imod, covariates = mystr1)
    mymodels <- rbind(mymodels, temmod)
  }
  mymodels$spatiotemporal   <-  "period-varying"
  mymodels$type             <- "iid"
  #mymodels$spatiotemporal[mymodels$mod == "b01"] <- "off" # not use
  mymodels$type[mymodels$mod == "b01"] <- "off"
  
  ## Add yearc and depth covariates that always stay in
  mymodels$covariates <-  paste0(mymodels$covariates, "+periodc", "+s(depth1.std, k = myk)")
  
  ## Save model definition
  save(mymodels, file=paste0(output_path, iseason, "_models_definition_", Sys.Date(), ".RData"))
  
  system.time({
    mysave <- mclapply(1:nrow(mymodels), run_model)
  })
  
  
  res <- NA
  for (i in c(1:length(mysave))){
    if (length(mysave[[i]])>1)
      res <- rbind(res, mysave[[i]])
  }
  res <- res[-1,]
  
  ## Save model definition
  write.csv(res, file=paste0(output_path, iseason, "_models_fitting_results_period_round1_", Sys.Date(), ".csv"), row.names = F)

  }
end = Sys.time()
end - start # ~14min
gc()
rm(iseason, res, mymodels, mysave, dd, temmod, mesh, mystr)





## Round 2 ----------------------------------------------------------
# A second round for dec-jan, apr-may, jun-jul

`myvar_dec-jan` = c("s(Analysed_meanSST_Celsius.std, k = myk)", "Fishing_hour_averagec")
`myvar_apr-may` = c("s(Analysed_meanSST_Celsius.std, k = myk)", "s(meanChlorophyll_sqrt.std, k = myk)")
`myvar_jun-jul` = c("s(Analysed_meanSST_Celsius.std, k = myk)", "s(meanChlorophyll_sqrt.std, k = myk)")

## Select season
seasons = unique(dat$season)
seasons = seasons[c(3,4,5)]
seasons = droplevels(seasons)
unique(seasons)

## Run for each season
start = Sys.time()

for(iseason in seasons){
  dd <- dat[dat$season == iseason,]
  
  ## Response
  dd$response <- dd$dens_sqrt
  
  ## Arrange year period variables
  dd$yearc    <- factor(dd$Year_bimonthly)
  #levels(dd$yearc)
  
  ## Mesh
  cutoff <- 20
  mesh <- make_mesh(dd, xy_cols = c("x_utm1", "y_utm1"), cutoff = cutoff)
  #plot(mesh)
  mesh$mesh$n
  
  # myvar per Bimonthly period
if (iseason == "oct-nov") {myvar <- `myvar_oct-nov`}
if (iseason == "dec-jan") {myvar <- `myvar_dec-jan`}
if (iseason == "feb-mrch") {myvar <- `myvar_feb-mrch`}
if (iseason == "apr-may") {myvar <- `myvar_apr-may`}
if (iseason == "jun-jul") {myvar <- `myvar_jun-jul`}
if (iseason == "aug-sep") {myvar <- `myvar_aug-sep`}
  
  ## Full formula: myvar
  mystr <- paste0("response ~ ", paste0(myvar, collapse = "+"))
  
  # b00 only spatiotemporal # not use b00 and b01
  #mymodels <- data.frame(mod   = "b00", covariates = "response ~ 1") 
  # 01 only fixed effects
  ## full formula
  #temmod <- data.frame(mod   = "b01", covariates = mystr)
  #mymodels <- rbind(mymodels, temmod)
  # b02 full effects
  temmod <- data.frame(mod   = "b02", covariates = mystr)
  mymodels = data.frame()
  mymodels <- rbind(mymodels, temmod)
  
  # b1X - delete one covariate
  for (i in 1:length(myvar)) {
    if (i==1) {
      idel   <- paste0(myvar[i], "+")
      mystr1 <- gsub(idel, "", mystr, fixed =T)
    } else {
      idel   <- paste0("+", myvar[i])
      mystr1 <- gsub(idel, "", mystr, fixed =T)
    }
    imod   <- paste0("b1",i)
    temmod <- data.frame(mod   = imod, covariates = mystr1)
    mymodels <- rbind(mymodels, temmod)
  }
  mymodels$spatiotemporal   <-  "period-varying"
  mymodels$type             <- "iid"
  #mymodels$spatiotemporal[mymodels$mod == "b01"] <- "off" # not use
  mymodels$type[mymodels$mod == "b01"] <- "off"
  
  ## Add yearc and depth covariates that always stay in
  mymodels$covariates <-  paste0(mymodels$covariates, "+periodc", "+s(depth1.std, k = myk)")
  
  ## Save model definition
  save(mymodels, file=paste0(output_path, iseason, "_models_definition_round2", Sys.Date(), ".RData"))
  
  system.time({
    mysave <- mclapply(1:nrow(mymodels), run_model)
  })
  
  
  res <- NA
  for (i in c(1:length(mysave))){
    if (length(mysave[[i]])>1)
      res <- rbind(res, mysave[[i]])
  }
  res <- res[-1,]
  
  ## Save model definition
  write.csv(res, file=paste0(output_path, iseason, "_models_fitting_results_period_round2_", Sys.Date(), ".csv"), row.names = F)

  }
end = Sys.time()
end - start # ~14min
gc()
rm(iseason, res, mymodels, mysave, dd, temmod, mesh, mystr)

rm(`myvar_jun-jul`)

## Round 3  ----------------------------------------------------------
# A third round for jun-jul (bring depth to above to solve problem)

`myvar_jun-jul` = c("s(meanChlorophyll_sqrt.std, k = myk)","s(depth1.std, k = myk)")

## Select season
seasons = unique(dat$season)
seasons = seasons[c(3)]
seasons = droplevels(seasons)
unique(seasons)

## Run for each season
start = Sys.time()

for(iseason in seasons){
  dd <- dat[dat$season == iseason,]
  
  ## Response
  dd$response <- dd$dens_sqrt
  
  ## Arrange year period variables
  dd$yearc    <- factor(dd$Year_bimonthly)
  #levels(dd$yearc)
  
  ## Mesh
  cutoff <- 20
  mesh <- make_mesh(dd, xy_cols = c("x_utm1", "y_utm1"), cutoff = cutoff)
  #plot(mesh)
  mesh$mesh$n
  
  # myvar per Bimonthly period
if (iseason == "oct-nov") {myvar <- `myvar_oct-nov`}
if (iseason == "dec-jan") {myvar <- `myvar_dec-jan`}
if (iseason == "feb-mrch") {myvar <- `myvar_feb-mrch`}
if (iseason == "apr-may") {myvar <- `myvar_apr-may`}
if (iseason == "jun-jul") {myvar <- `myvar_jun-jul`}
if (iseason == "aug-sep") {myvar <- `myvar_aug-sep`}
  
## Full formula: myvar
mystr <- paste0("response ~ ", paste0(myvar, collapse = "+"))
  
  # b00 only spatiotemporal # not use b00 and b01
  #mymodels <- data.frame(mod   = "b00", covariates = "response ~ 1") 
  # 01 only fixed effects
  ## full formula
  #temmod <- data.frame(mod   = "b01", covariates = mystr)
  #mymodels <- rbind(mymodels, temmod)
  # b02 full effects
  temmod <- data.frame(mod   = "b02", covariates = mystr)
  mymodels = data.frame()
  mymodels <- rbind(mymodels, temmod)
  
  # b1X - delete one covariate
  for (i in 1:length(myvar)) {
    if (i==1) {
      idel   <- paste0(myvar[i], "+")
      mystr1 <- gsub(idel, "", mystr, fixed =T)
    } else {
      idel   <- paste0("+", myvar[i])
      mystr1 <- gsub(idel, "", mystr, fixed =T)
    }
    imod   <- paste0("b1",i)
    temmod <- data.frame(mod   = imod, covariates = mystr1)
    mymodels <- rbind(mymodels, temmod)
  }
  mymodels$spatiotemporal   <-  "period-varying"
  mymodels$type             <- "iid"
  #mymodels$spatiotemporal[mymodels$mod == "b01"] <- "off" # not use
  mymodels$type[mymodels$mod == "b01"] <- "off"
  
  ## Add yearc and depth covariates that always stay in
  mymodels$covariates <-  paste0(mymodels$covariates, "+periodc")
  
  ## Save model definition
  save(mymodels, file=paste0(output_path, iseason, "_models_definition_round3_", Sys.Date(), ".RData"))
  
  system.time({
    mysave <- mclapply(1:nrow(mymodels), run_model)
  })
  
  
  res <- NA
  for (i in c(1:length(mysave))){
    if (length(mysave[[i]])>1)
      res <- rbind(res, mysave[[i]])
  }
  res <- res[-1,]
  
  ## Save model definition
  write.csv(res, file=paste0(output_path, iseason, "_models_fitting_results_period_round3_", Sys.Date(), ".csv"), row.names = F)

  }
end = Sys.time()
end - start # ~14min
gc()
rm(iseason, res, mymodels, mysave, dd, temmod, mesh, mystr)



















end = Sys.time()
end - start

# END --------------------------------------------------------------------------
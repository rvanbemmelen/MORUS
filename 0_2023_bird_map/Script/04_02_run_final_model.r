## this is a script template to run the final model
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

## list of all functions
#lsf.str("package:sdmTMB")
help(package = sdmTMB)

## here you need to input:
## bird species
ispe  <- "European_Herring_Gull"
ispe1 <- "European_Herring_Gull"
iplatform <- "aerial"

## Limit smoother shape to number of knots you want
myk = 5

# Model selection variables that were defined in script 04_00
# apr_may  --> depth,     , SST, Chlorophyll, 
# jun_jul  --> depth,     ,    , Chlorophyll,
# aug_sep  --> depth,     , SST, Chlorophyll,         , fishing
# oct_nov  --> depth,     , SST, Chlorophyll,         , fishing
# dec_jan  --> depth,     , SST,            ,         , fishing
# feb_mrch --> depth,     , SST, Chlorophyll,         , fishing
#myvar     <- c("s(esa.std)", "s(depth1.std)", "s(SandPercent.std)", "s(Analysed_meanSST_Celsius.std)", "s(meanChlorophyll_sqrt.std)","Fishing_hour_averagec","s(Distance_platform_sqrt.std)","s(Distance_big_breeding_site_km.std)")


`myvar_apr-may` <- c("s(depth1.std, k = myk)", "s(Analysed_meanSST_Celsius.std, k = myk)", "s(meanChlorophyll_sqrt.std, k = myk)")

`myvar_jun-jul` <- c("s(depth1.std, k = myk)",  "s(meanChlorophyll_sqrt.std, k = myk)")

`myvar_aug-sep` <- c("s(depth1.std, k = myk)", "s(Analysed_meanSST_Celsius.std, k = myk)", "s(meanChlorophyll_sqrt.std, k = myk)","Fishing_hour_averagec")

`myvar_oct-nov` <- c("s(depth1.std, k = myk)", "s(Analysed_meanSST_Celsius.std, k = myk)", "s(meanChlorophyll_sqrt.std, k = myk)","Fishing_hour_averagec")

`myvar_dec-jan` <- c("s(depth1.std, k = myk)", "s(Analysed_meanSST_Celsius.std, k = myk)", "Fishing_hour_averagec")

`myvar_feb-mrch`<- c("s(depth1.std, k = myk)", "s(Analysed_meanSST_Celsius.std, k = myk)", "s(meanChlorophyll_sqrt.std, k = myk)","Fishing_hour_averagec")






## path
project_path <- "C:/Users/0_2023_bird_map/" # Define 
script_path  <- paste0(project_path, "Script/")
data_path1   <- paste0(project_path, "Data/")
data_path    <- paste0(project_path, "Data/", ispe,"/")
fig_path     <- paste0(project_path, "Figure/", ispe, "/")
output_path  <- paste0(project_path, "model_output/", ispe, "/")

setwd(script_path)
source(paste0(project_path, "Script/functions_model.r"))

#### 1. load data ----
## data ready for modeling
load(file=paste0(data_path, "processed_data_", ispe, "_platform_", iplatform,"_for_modeling.RData"))
table(dat$season, dat$year)
table(dat$season, dat$Year_bimonthly)
## data description:
## dens_sqrt
## breeding year 2000-2020
## covariates are standadized to help model converge, X.std, standardization mean/sd are also in data

dat$periodc <- cut(dat$Year_bimonthly, breaks=c(1999,2005, 2010, 2015, 2020))
table(dat$periodc, dat$Year_bimonthly, useNA="always")
dat$period  <- as.integer(dat$periodc)



# Loop to run final model for every season 

iseason <- unique(dat$Bimonthly)

for(i in iseason){

## select season
unique(dat$season)
dd      <- dat[dat$Bimonthly == i,]
unique(dd$Bimonthly)

## response
dd$response <- dd$dens_sqrt

## arrange year period variables
dd$yearc    <- factor(dd$Year_bimonthly)
levels(dd$yearc)

## mesh
cutoff <- 20
mesh <- make_mesh(dd, xy_cols = c("x_utm1", "y_utm1"), cutoff = cutoff)
#plot(mesh)
mesh$mesh$n

##### run final model ----

# myvar per Bimonthly period
if (i == "oct-nov") {myvar <- `myvar_oct-nov`}
if (i == "dec-jan") {myvar <- `myvar_dec-jan`}
if (i == "feb-mrch") {myvar <- `myvar_feb-mrch`}
if (i == "apr-may") {myvar <- `myvar_apr-may`}
if (i == "jun-jul") {myvar <- `myvar_jun-jul`}
if (i == "aug-sep") {myvar <- `myvar_aug-sep`}

## year based model
#model_type <- "Year_bimonthly"
## period based model
model_type <- "periodc"

## final model covariates
## check whether to use yearc or periodc

if (model_type == "Year_bimonthly") {
  myvar     <- c(myvar, "yearc")
}

if (model_type == "periodc") {
  myvar     <- c(myvar, "periodc")
}
myvar
##  formula: myvar
mystr <- paste0("response ~ ", paste0(myvar, collapse = "+"))

mod1            <- "final"
myformula       <- as.formula(mystr)
myformula
type1           <- "iid"
time1           <- model_type
myfit           <- fit_TV(dd, mesh, myformula, itime=time1, itype=type1, iseason, imodel=mod1, iresponse="sqrt_dens")

mod             <- myfit[[1]]
gc()
save(mod, file=paste0(output_path, i, "_final_model_", model_type, ".RData"))
rm(mod,myvar,i)

}





# END --------------------------------------------------------------------------













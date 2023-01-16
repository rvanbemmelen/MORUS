### ESAS dataset opwerking -------------------------
#---------------------------------------------------
# Marinka van Puijenbroek & Susanne van Donk -------
# v1 - 2021-02-17
# v2 -2022-10-10
# Information about terminology: https://ices-tools-dev.github.io/esas/tables/
# Careful! This website is updated and can have differences with the dataset.
# For correct information check file send by VanErmen.https://docs.google.com/document/d/15tNRx-AjP3IhhPzLK-hlf5VcF7iSy3i8/edit

# Load packages and functions ------------

# Remove all data 
rm(list= ls())

# Load packages 
library(tidyverse)
library(sf)
library(readxl)
library(lubridate)

# Functies 
`%notin%` <- function(x,y) !(x %in% y)

# Ggplot theme 

theme_s <-  function () { 
  theme_classic(base_size=12)%+replace% 
    theme(axis.text = element_text(color="grey"),
          axis.line = element_line(color="grey"),
          axis.ticks = element_line(color="grey"),
          legend.title = element_text(face = "bold"),
          strip.background = element_rect(fill = "white", colour = "grey"),
          strip.text = element_text(margin = margin(0.2,0,0.2,0, "cm")) )
}


# Step 1: Combine data: SKIP if dataset already saved, go to script 2 -----------------------------
#* Load ESAS data ------------------------

getwd()
# Working directory raw data ESAS:
# W:/IMARES/DATA/KEC4-0/4. Data/1. Ruwe data/ESAS_dataset/ESAS v6
#setwd("W:/IMARES/DATA/KEC4-0/4. Data/1. Ruwe data/ESAS_dataset/ESAS v6")

# Topdown
# tripkey = unique value per trip
# poskey = unique value per position
# Obskey = unique value per observation

temp = list.files(pattern="*v6.0.csv",path="W:/IMARES/DATA/KEC4-0/4. Data/1. Ruwe data/ESAS_dataset/ESAS v6")
names <-str_sub(temp,1,-5)

###Load all files
for(i in names){
  filepath <- file.path(paste("W:/IMARES/DATA/KEC4-0/4. Data/1. Ruwe data/ESAS_dataset/ESAS v6/",i,".csv",sep=""))
  assign(i, read.csv(filepath))
}

str(TRP_ESAS_v6.0)
str(POS_ESAS_v6.0)
str(OBS_ESAS_v6.0) # observations

OBS_ESAS_v6.0 %>% filter(is.na(transect)) %>% summarize(n()) # 71 observations without transect

# There are more positions than observations. These are probably empty observations. 
length(unique(TRP_ESAS_v6.0$tripkey));length(unique(POS_ESAS_v6.0$tripkey));length(unique(OBS_ESAS_v6.0$tripkey))
length(unique(TRP_ESAS_v6.0$poskey));length(unique(POS_ESAS_v6.0$poskey));length(unique(OBS_ESAS_v6.0$poskey))
length(unique(TRP_ESAS_v6.0$obskey));length(unique(POS_ESAS_v6.0$obskey));length(unique(OBS_ESAS_v6.0$obskey))

ESAS_all <- full_join(TRP_ESAS_v6.0, POS_ESAS_v6.0, by = "tripkey") %>% 
  full_join(OBS_ESAS_v6.0, by ="poskey")  %>% 
  mutate(across(.cols = c("tripkey", "poskey", "campaign_key", "observer1"), as.character))

check <- ESAS_all %>% filter(is.na(poskey))
unique(check$date) # missing latitude & longitude/ Only dates in august 2015

# check if no MWTL data
unique(ESAS_all$origin)

rm(i,names,filepath,temp,TRP_ESAS_v6.0, POS_ESAS_v6.0,OBS_ESAS_v6.0)

#* Load MWTL data --------------------------

# Load MWTL 1991 - 2014
# Load MWTL 1991 - 2014
temp <- Sys.glob("W:/IMARES/DATA/KEC4-0/4. Data/1. Ruwe data/MWTL/buwa-62b54e/datalevering_feb2021_MardikLeopold/exports_esas_1991-2014/*.xlsx")
names <-str_sub(temp,119,-6)

mwtl <- map2(temp, names, function(i,t) {
  assign(t, read_excel(i))
})

names(mwtl) <- names
list2env(mwtl, .GlobalEnv)

# Fix date
NL_2014_TRP_ESAS_aerial <- NL_2014_TRP_ESAS_aerial %>% 
  unite(date, day:year, sep = "/") 

# Fix time 
NL_2014_POS_ESAS_aerial <- NL_2014_POS_ESAS_aerial %>% 
  unite(time, hours:seconds, sep = ":") 

mwtl_1991_2014 <- full_join(NL_2014_TRP_ESAS_aerial, NL_2014_POS_ESAS_aerial, by = "tripkey") %>% 
  full_join(NL_2014_SPC_ESAS_aerial, by ="poskey")

rm(NL_2014_TRP_ESAS_aerial, NL_2014_POS_ESAS_aerial,NL_2014_SPC_ESAS_aerial)





# Load MWTL > 2014
# nieuwe dataset, ontvangen op 13-10-2022, augustus 2014-june 2021 
temp = list.files(pattern="*.csv",path="W:/IMARES/DATA/KEC4-0/4. Data/1. Ruwe data/MWTL/mwtl tm juni 2021")
names <-str_sub(temp,1,-5)

###Load all files
for(i in names){
  filepath <- file.path(paste("W:/IMARES/DATA/KEC4-0/4. Data/1. Ruwe data/MWTL/mwtl tm juni 2021/",i,".csv",sep=""))
  assign(i, read.csv(filepath,sep = ";"))
}

# Waarom meer observaties dan hiervoor...?
mwtl_2014 <- full_join(esas_export_trip_20221012, esas_export_position_20221012, by = "tripkey") %>% 
  full_join(esas_export_observations_20221012, by ="poskey")  %>% 
  mutate(across(.cols = c("tripkey", "poskey", "campaign_key", "observer1"), as.character))

str(mwtl_2014)
unique(mwtl_2014$date)


MWTL <- bind_rows(mwtl_2014,mwtl_1991_2014) %>% 
  mutate(across(.cols = c("distance"), as.character)) %>% 
  add_column(origin = "MWTL")

unique(MWTL$date)

All_data <- bind_rows(ESAS_all, MWTL)

dim(ESAS_all);dim(MWTL)
rm(list=setdiff(ls(), "All_data"))

# FIX DATE
All_data <- All_data %>% 
  mutate(date = parse_date_time(date, c('%Y-%m-%d', '%d/%m/%Y')))# Verschillende dataformats 

saveRDS(All_data,"Data_tussenproduct/ESAS_MWTL_raw.rds")
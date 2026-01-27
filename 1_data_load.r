
### ESAS/MWTL dataset opwerking -------------------------

#' Marinka van Puijenbroek & Susanne van Donk & Rob van Bemmelen
#' v1 - 2021-02-17
#' v2 - 2022-10-10
#' v3 - 2025-12-18

#' ESAS public data is downloaded from 
#' https://www.ices.dk/data/data-portals/Pages/European-Seabirds-at-sea.aspx
#' MWTL restricted data is obtained in ESAS-format from Job de Jong (Waardenburg Ecology)

#' Information about terminology: https://ices-tools-dev.github.io/esas/tables/
#' Careful! This website is updated and can have differences with the dataset.
#' For correct information check file send by Nicolas VanErmen. 
#' https://docs.google.com/document/d/15tNRx-AjP3IhhPzLK-hlf5VcF7iSy3i8/edit

# remove all data
rm(list = ls())

# load packages and functions ------------

# load packages 
library(dplyr)
library(lubridate)
library(sf)

# Functions
`%!in%` <- function(x,y) !(x %in% y)

# set raw data directories ------------

# dir_ESAS <- "W:/IMARES/DATA/KEC4-0/4. Data/1. Ruwe data/ESAS_dataset/ESAS v6"
dir_ESAS <- "/Users/robvb/Documents/data/ESAS/ESAS_1209172811"
dir_MWTL <- "/Users/robvb/Documents/projects/25-0618 KEC6 kaarten/data"

#' date of last download is:
as.Date(file.info(list.files(dir_ESAS, ".csv", full.names = TRUE))$ctime[1])

# load ESAS public data ------------------------

# list csv files
c_ESAS_files <- list.files(
  pattern = ".csv",
  path = dir_ESAS)
c_ESAS_names <- stringr::str_sub(c_ESAS_files, 1, -5)

# load csv files
for(i in c_ESAS_names){
  assign(
    paste0("d_ESAS_", stringr::str_sub(tolower(i), 1, 3)), 
    read.csv(
      file.path(
        dir_ESAS,
        paste0(
          i, ".csv")
      ),
      na.strings = ""
      ))
  }

#' four data.frames with this hierarchy:
#' d_ESAS_cam = campaigns
#' d_ESAS_sam = samples
#' d_ESAS_pos = positions
#' d_ESAS_obs = observations


# load MWTL non-public data ------------------------

#' check if no MWTL data: 5045 Bureau Waardenburg BV (BUWA)
"5045" %!in% unique(d_ESAS_cam$datarightsholder)

# list MWTL csv files
c_MWTL_files <- list.files(
  pattern = ".csv",
  path = dir_MWTL)
c_MWTL_names <- stringr::str_sub(c_MWTL_files, 1, -5)

# load csv files
for(i in c_MWTL_names){
  assign(
    paste0(
      "d_MWTL_", 
      substr(i, 1, 3)
      ),
    read.csv(
      file.path(
        dir_MWTL,
        paste0(
          i, ".csv")
      ),
      sep = ";"
      ))
}

# combine ESAS and MWTL data ------------------------

#' make lowercase column names
colnames(d_ESAS_cam) <- tolower(colnames(d_ESAS_cam))
colnames(d_ESAS_sam) <- tolower(colnames(d_ESAS_sam))
colnames(d_ESAS_pos) <- tolower(colnames(d_ESAS_pos))
colnames(d_ESAS_obs) <- tolower(colnames(d_ESAS_obs))

#colnames(d_MWTL_cam) <- tolower(colnames(d_MWTL_cam)) # FIX!
colnames(d_MWTL_tri) <- tolower(colnames(d_MWTL_tri)) # FIX! name to _sam
colnames(d_MWTL_pos) <- tolower(colnames(d_MWTL_pos))
colnames(d_MWTL_obs) <- tolower(colnames(d_MWTL_obs))

#' convert transect and position ids in ESAS to character
d_ESAS_sam$sampleid <- as.character(d_ESAS_sam$sampleid)
d_ESAS_pos$sampleid <- as.character(d_ESAS_pos$sampleid)
d_ESAS_obs$sampleid <- as.character(d_ESAS_obs$sampleid)
d_ESAS_pos$positionid <- as.character(d_ESAS_pos$positionid)
d_ESAS_obs$positionid <- as.character(d_ESAS_obs$positionid)
d_ESAS_obs$groupid <- as.numeric(
  stringr::str_replace_all(
    string = d_ESAS_obs$groupid, 
    pattern = "\\?", replacement = ""))

#' convert base side in MWTL to character and lat/lon to double
d_MWTL_tri$base_side <- ifelse(d_MWTL_tri$base_side == 1, "left", "right")
d_MWTL_pos$km_travelled <- as.double(d_MWTL_pos$km_travelled)
d_MWTL_pos$area_surveyed <- as.double(d_MWTL_pos$area_surveyed)
d_MWTL_obs$transect <- ifelse(d_MWTL_obs$transect == 2, "True", "False")
d_MWTL_obs$distance <- as.character(d_MWTL_obs$distance) ## NOTE; 
d_MWTL_obs$prey <- as.integer(d_MWTL_obs$prey)
d_MWTL_obs$association <- as.character(d_MWTL_obs$association)
d_MWTL_obs$behaviour <- as.character(d_MWTL_obs$behaviour)
d_MWTL_tri$campaign_key <- stringr::str_sub(d_MWTL_tri$campaign_key, -7, -1)

#' fix column names in MWTL (should be redundant when Job/August fix this)
d_MWTL_cam <- d_MWTL_tri %>%
  dplyr::select(
    data_provider, campaign_key, notes
  ) %>%
  rename(
    datarightsholder = data_provider,
    campaignid = campaign_key
  ) %>%
  distinct(.keep_all = TRUE) %>%
  left_join(
    x = .,
    y = d_MWTL_tri %>% 
      group_by(tripkey) %>%
      summarise(
        startdate = first(date),
        enddate = last(date)
      ) %>%
      mutate(
        country = "NL",
        dataaccess = "Restricted"
      ) %>%
      rename(
        campaignid = tripkey
      ),
    by = "campaignid"
  ) %>%
  mutate(
    datarightsholder = as.character(datarightsholder)
  )

# MWTL samples; extract from trip df
d_MWTL_sam <- d_MWTL_tri %>%
  rename(
    campaignid = campaign_key,
    platformcode = platform_code,
    platformclass	= platform_type,
    platformside = base_side,
    platformheight = flying_height,
    transectwidth = transect_width,
    samplingmethod = count_method,
    primarysampling = behaviour_recording,
    targettaxa = species_counted,
    distancebins = distance_bins,
    useofbinoculars = use_of_binoculars,
    numberofobservers = number_of_observers
  ) %>%
  full_join( # get transect_id (sampleid) from position file
    x = .,
    y = d_MWTL_pos %>%
      dplyr::select(
        tripkey, transect_id
      ) %>%
      rename(
        sampleid = transect_id
      ) %>%
      group_by(tripkey, sampleid) %>%
      distinct(),
    by = "tripkey"
  ) %>%
  mutate(
    primarysampling = "True"
  ) %>%
  dplyr::select(
    -route, 
    -data_provider,
    -direction_of_travel_type,
    -observer_role,
    -observer1,
    -observer2,
    -observer3,
    -tripkey
  )

d_MWTL_pos <- left_join(
    d_MWTL_pos,
    d_MWTL_tri %>%
      dplyr::select(campaign_key, tripkey) %>%
      distinct(),
    by = "tripkey"
  ) %>%
  rename(
    campaignid = campaign_key,
    sampleid = transect_id,
    positionid = poskey,
    distance = km_travelled,
    area = area_surveyed,
    windforce	= beaufort,
    sunangle = sun_angle,
    cloudcover = cloud_cover,
    icecover = ice,
    observationconditions	= observation_quality
  ) %>%
  dplyr::select(
    -notes, 
    -tripkey
  )
  

d_MWTL_obs <- d_MWTL_obs %>%
  dplyr::select(-activity) %>% # not clear what this is and if it corresponds to a column in ESAS data
  rename(
    positionid = poskey,
    observationid	= obskey,
    groupid	= group,
    transect = transect,
    speciescode	= euring_species_code,
    count	= number,
    observationdistance	= distance,
    lifestage	= age_class,
    traveldirection	= direction_of_travel
  ) %>%
  left_join(
    x = .,
    y = d_MWTL_pos %>%
      dplyr::select(
        campaignid, sampleid, positionid
      ) %>%
      distinct(),
    by = "positionid"
  ) %>%
  mutate(
    speciescodetype = "ESAS"
  )

#' check if column names are identical; note that sequence does not have to be identical
colnames(d_ESAS_cam) %in% colnames(d_MWTL_cam)
colnames(d_ESAS_sam) %in% colnames(d_MWTL_sam)
colnames(d_ESAS_pos) %in% colnames(d_MWTL_pos)
colnames(d_ESAS_obs) %in% colnames(d_MWTL_obs)

#' add column for origin
d_ESAS_cam$origin <- "ESAS"
d_MWTL_cam$origin <- "MWTL_new"

#' combine with ESAS data
d_cam <- bind_rows(d_ESAS_cam, d_MWTL_cam)
d_sam <- bind_rows(d_ESAS_sam, d_MWTL_sam)
d_pos <- bind_rows(d_ESAS_pos, d_MWTL_pos)
d_obs <- bind_rows(d_ESAS_obs, d_MWTL_obs)

#' check number of campaignIDs, sampleIDs, positionsIDs and observationIDs
length(unique(d_cam$campaignid)); length(unique(d_pos$campaignid)); length(unique(d_sam$campaignid)); length(unique(d_obs$campaignid)) #' less in observations: some campaigns with no sightings...
length(unique(d_sam$sampleid)); length(unique(d_pos$sampleid)); length(unique(d_obs$sampleid)) # same in d_sam and d_pos, smaller in d_obs as some samples with no observations
length(unique(d_pos$positionid)); length(unique(d_obs$positionid)) #' some positions with no observations
length(unique(d_obs$observationid)) # only in d_obs; smaller than nrow(d_obs) because some sightings concern mixed flocks

# full join of all tables ------------------------

#' full join
d_all <- full_join(
    d_cam, 
    d_sam, 
    by = "campaignid"
  ) %>% 
  full_join(
    d_pos,
    by = c("campaignid", "sampleid")
  ) %>% 
  full_join(
    d_obs,
    by = c("campaignid", "sampleid", "positionid")#,
    #relationship = "many-to-many" # silence warning; multiple rows in y for each x are expected and other way around
  ) %>% 
  mutate(
    across(
      .cols = c("campaignid", "sampleid", "positionid"), as.character),
    date = parse_date_time(
      date, "%Y-%m-%d"),
    datetime = parse_date_time(
      paste(date, time),
      "%Y-%m-%d %H:%M:%S"
    )
    )

#' number of positions with no observations
n_no_obs <- length(unique(d_pos$positionid)) - length(unique(d_obs$positionid))

#' dimensions should be the number of all observations + all positions with no observations
nrow(d_all) == nrow(d_obs) + n_no_obs

#' check for NAs in important columns
d_all %>% 
  summarise(
    n_cam_NA = sum(is.na(campaignid)),
    n_sam_NA = sum(is.na(sampleid)),
    n_sam_NA = sum(is.na(positionid)),
    n_obs_NA = sum(is.na(observationid)),
    n_eur_NA = sum(is.na(speciescode)), # species euring code
    n_dis_NA = sum(is.na(distance)), # travelled distance
    n_obsdis_NA = sum(is.na(observationdistance)), # distance (band)
    n_siz_NA = sum(is.na(count)) # cluster size
  )
#' NAs in obsid and euring must match number of positions with no observations: n_no_obs


# old MWTL ####
#' this is the file that was prepared for the 2022-2024 project, and contains
#' a full join of 
d_MWTL_old_0 <- readRDS(
  file.path(
    dirname(rstudioapi::getSourceEditorContext()$path),
    "output",
    "dataset",
    "ESAS_MWTL_raw_20220000.rds")
  ) %>%
  filter(
    data_provider == 140
  )  
colnames(d_MWTL_old_0) <- tolower(colnames(d_MWTL_old_0))

d_MWTL_old <- d_MWTL_old_0 %>%
  st_transform(4326) %>%
  mutate(
    longitude = st_coordinates(.)[,1],
    latitude = st_coordinates(.)[,2],
    datetime = as_datetime(
      paste(date, time)
    ),
    speciescodetype	= "ESAS",
    country	= "NL", # not entirely correct; some transects in UK
    base_side = ifelse(base_side == 2, "right", "left"),
    behaviour_recording = ifelse(behaviour_recording == 1 & !is.na(behaviour_recording), 
                                 'True', NA),
    transect = ifelse(transect == 2 & !is.na(transect), "True", "False"),
    observationdistance	= distance,
    distance = km_travelled,
    campaignid = paste(year, season, sep = "_")
  ) %>%
  st_drop_geometry() %>%
  rename(
    datarightsholder = data_provider,
    notes.x = notes.x,
    sampleid = transect_id,
    platformcode = platform_code,
    platformclass	= platform_type,
    platformside = base_side,
    platformheight = flying_height,
    transectwidth	= transect_width,
    samplingmethod = count_method,
    primarysampling = behaviour_recording,
    targettaxa = species_counted,
    distancebins = distance_bins,
    useofbinoculars = use_of_binoculars,
    numberofobservers = number_of_observers,
    positionid = poskey,
    area = area_surveyed,
    windforce = beaufort,
    sunangle = sun_angle,
    cloudcover = cloud_cover,
    icecover = ice,
    observationconditions	= observation_quality,
    observationid	= obskey,
    groupid	= group,
    speciescode	= euring_species_code,
    count	= number,
    lifestage	= age_class,
    traveldirection	= direction_of_travel,
  )

d_MWTL_old <- left_join(
    x = d_MWTL_old,
    y = d_MWTL_old_0 %>%
      st_drop_geometry() %>%
      rename(
        campaignid = tripkey
      ) %>%
      group_by(campaignid) %>%
      summarise(
        startdate = min(date),
        enddate = max(date)
      ),
    by = "campaignid"
  ) %>%
  dplyr::select(
    -public_data,
    -direction_of_travel_type,
    -observer_role,
    -observer1,
    -observer2,
    -observer3,
    -origin,
    -activity,
    -posmark,
    -id,
    -grid_id,
    -shp_lng,
    -shap_ar,
    -land,
    -subarea,
    -actv4br,
    -cntrycd,
    -vgshow,
    -estuarn,
    -maand,
    -season,
    -year,
    -x,
    -y,
    -route,
    -campaign_key,
    -tripkey,
    -km_travelled
  )

# correct formats
d_MWTL_old <- d_MWTL_old %>%
  mutate(
    datarightsholder = as.character(datarightsholder),
    platformcode = as.character(platformcode),
    glare = as.character(glare),
    observationconditions = as.integer(NA),
    association = as.character(association),
    behaviour = as.character(behaviour),
    origin = "MWTL_old"
  )
d_all <- d_all %>%
  mutate(
    startdate = as.POSIXct(as_date(startdate)),
    enddate = as.POSIXct(as_date(enddate))
  )


# check if column numbers and names are OK now
ncol(d_all) == ncol(d_MWTL_old) # FALSE, but is due to some unimportant column in d_all that are not present in d_MWTL_old
table(colnames(d_all) %in% colnames(d_MWTL_old))
table(colnames(d_MWTL_old) %in% colnames(d_all))

# join to d_all
d_all <- bind_rows(
  d_all,
  d_MWTL_old
)

#' remove one error for Atlantic Puffin that has been lingering in the database forever...
d_all$count[d_all$speciescode == 6540 & d_all$count == 40 & d_all$platformclass == 3] <- 4

#' export file ------------------------

#' save file with date of creation
saveRDS(
  d_all, 
  file = file.path(
    dirname(rstudioapi::getSourceEditorContext()$path),
    "output",
    "dataset",
    paste0(
      "ESAS_MWTL_raw_", gsub("-", "", Sys.Date()),".rds")
    )
  )

#' remove all other files
rm(i, 
   c_ESAS_files, c_ESAS_names, 
   c_MWTL_files, c_MWTL_names, 
   d_ESAS_cam, d_ESAS_sam, d_ESAS_pos, d_ESAS_obs,
   d_MWTL_cam, d_MWTL_sam, d_MWTL_pos, d_MWTL_obs,
   d_MWTL_old_0, d_MWTL_old)

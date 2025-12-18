
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

# Functions
`%!in%` <- function(x,y) !(x %in% y)

# set raw data directory ------------

# dir_dat <- "W:/IMARES/DATA/KEC4-0/4. Data/1. Ruwe data/ESAS_dataset/ESAS v6"
dir_dat <- "/Users/robvb/Documents/data/ESAS/ESAS_1209172811"

#' date of last download is:
as.Date(file.info(list.files(dir_dat, ".csv", full.names = TRUE))$ctime[1])

# load ESAS public data ------------------------

# list csv files
c_ESAS_files <- list.files(
  pattern = ".csv",
  path = dir_dat)
c_ESAS_names <- stringr::str_sub(c_ESAS_files, 1, -5)

# load csv files
for(i in c_ESAS_names){
  assign(
    paste0("d_ESAS_", stringr::str_sub(tolower(i), 1, 3)), 
    read.csv(
      file.path(
        dir_dat,
        paste0(
          i, ".csv")
      )))
  }

#' four data.frames with this hierarchy:
#' d_ESAS_cam = campaigns
#' d_ESAS_sam = samples
#' d_ESAS_pos = positions
#' d_ESAS_obs = observations


# load MWTL non-public data ------------------------

#' check if no MWTL data: 5045 Bureau Waardenburg BV (BUWA)
"5045" %!in% unique(ESAS_all$datarightsholder)

# list csv files
c_MWTL_files <- list.files(
  pattern = ".csv",
  path = dir_dat)
c_MWTL_names <- stringr::str_sub(c_MWTL_files, 1, -5)

# load csv files
for(i in c_MWTL_names){
  assign(
    paste0("d_MWTL_", stringr::str_sub(tolower(i), 1, 3)), 
    read.csv(
      file.path(
        dir_dat,
        paste0(
          i, ".csv")
      )))
}

# combine ESAS and MWTL data ------------------------

#' check if column names are identical
colnames(d_ESAS_cam) == colnames(d_MWTL_cam)
colnames(d_ESAS_sam) == colnames(d_MWTL_sam)
colnames(d_ESAS_pos) == colnames(d_MWTL_pos)
colnames(d_ESAS_obs) == colnames(d_MWTL_obs)

#' combine with ESAS data
d_cam <- bind_rows(d_ESAS_cam, d_MWTL_cam)
d_sam <- bind_rows(d_ESAS_sam, d_MWTL_sam)
d_pos <- bind_rows(d_ESAS_pos, d_MWTL_pos)
d_obs <- bind_rows(d_ESAS_obs, d_MWTL_obs)

#' make all column names lowercase
colnames(d_cam) <- tolower(colnames(d_cam))
colnames(d_pos) <- tolower(colnames(d_pos))
colnames(d_obs) <- tolower(colnames(d_obs))
colnames(d_sam) <- tolower(colnames(d_sam))

#' check number of campaignIDs, sampleIDs, positionsIDs and observationIDs
length(unique(d_cam$campaignid)); length(unique(d_pos$campaignid)); length(unique(d_sam$campaignid)); length(unique(d_obs$campaignid)) #' less in observations: some campaigns with no sightings...
length(unique(d_sam$sampleid)); length(unique(d_pos$sampleid)); length(unique(d_obs$sampleid)) # same in d_sam and d_pos, smaller in d_obs as some samples with no observations
length(unique(d_pos$positionid)); length(unique(d_obs$positionid)) #' some positions with no observations
length(unique(d_obs$observationid)) # only in d_obs; smaller than nrow(d_obs) because some sightings concern mixed flocks

# full join of all tables ------------------------

# full join
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
    by = c("campaignid", "sampleid", "positionid")
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
    n_dis_NA = sum(is.na(distance)), # distance (band)
    n_siz_NA = sum(is.na(count)) # cluster size
  )
#' NAs in obsid and euring match number of positions with no observations: n_no_obs

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
rm(i, c_files, c_names, 
   d_ESAS_cam, d_ESAS_sam, d_ESAS_pos, d_ESAS_obs,
   d_MWTL_cam, d_MWTL_sam, d_MWTL_pos, d_MWTL_obs)

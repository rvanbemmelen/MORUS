
### Distance analysis ESAS/MWTL data ------------------------
#' Rob van Bemmelen
#' v1 - 2022
#' v2 - 18 December 2025

# directories
dir_git <- dirname(rstudioapi::getSourceEditorContext()$path)
dir_dat <- file.path(git_dir, "data")
dir_out <- file.path(git_dir, "output")

# setting
load_models <- FALSE

# libraries
library(dplyr)
library(mrds)

# source functions
source("/Users/robvb/Documents/github/basic_stuff/base_functions.r")
source_dir("/Users/robvb/Documents/github/MWTL_surveys/R/")

# load data ------------------------
load(
  file = file.path(
    dir_dat,
    "species_list.rdata"
  ),
  verbose = TRUE
)

# list of strata/covariates (from Waggitt)
#' 1. species
#' 2. method (line transect versus ESAS)
#'  - check distance and distance_bins columns
#' 3. behaviour (flight vs not flying, only for ship-based I guess) 
#'    (no detection loss assumed for birds in flight during ESAS)
#' 4. platform (airplane vs ship) 
#'  - platform_type == 3 for airplane, 1 for ship
#' 5. platform height (continuous, unclear where Waggitt got that from...)
#' 6. seastate (continuous)

# latest data file
c_rds <- list.files(file.path(dir_out, "dataset"), ".rds")
c_rds_dates <- as.numeric(stringr::str_sub(c_rds, 15, nchar(c_rds)-4))

# load processed data
d <- readRDS(
  file.path(
    dir_out, 
    "dataset", 
    c_rds[which.max(c_rds_dates)])
  )

# exploration
## distance bins
table(d$distance_bins)
table(d$distance[is.na(d$distance_bins)]) # what is U?
# W = water
# what are the distance bans 

## ESAS distance bins within the transect (transect == 2) are distance bands A-D. Use midpoints for distance analysis:
d$transect_width[d$transect_width==500] <- 600
d$transect_width[d$transect_width==200] <- 300
d$distance_bins[d$distance %in% LETTERS[1:6] & !is.na(d$distance)] <- "0|50|100|200|300"
d$distance[d$distance=="A" & d$distance_bins=="0|50|100|200|300" & !is.na(d$distance)] <- 25
d$distance[d$distance=="B" & d$distance_bins=="0|50|100|200|300" & !is.na(d$distance)] <- 75
d$distance[d$distance=="C" & d$distance_bins=="0|50|100|200|300" & !is.na(d$distance)] <- 150
d$distance[d$distance=="D" & d$distance_bins=="0|50|100|200|300" & !is.na(d$distance)] <- 250
d$distance_bins[is.na(d$distance_bins) & d$distance %in% c(89,117, 145, 256)] <- "75|103|131|159|353"
d$distance_bins[is.na(d$distance_bins) & d$distance %in% c("U", "W")] <- "0|50|100|200|300"
d$distance_bins[is.na(d$distance_bins) & d$transect_width %in% c(300,600)] <- "0|50|100|200|300"

# c(89,117, 145, 256)
# 75 | 103 | 131 | 159 | 353 # LEFT TRUNCATED at 75m, apparently...?

# data selection
d_sel <- d %>% 
  select(-geometry) %>%
  data.frame() %>%
  mutate(
    euring_species_code = replace(
      euring_species_code, 
      euring_species_code==59, 
      20), # unID divers to RTDivers
    euring_species_code = replace(
      euring_species_code, 
      euring_species_code %in% c(6150, 6160), 6169), # Common/Arctic Terns to commic terns
    distance_bins = replace(
      distance_bins, 
      distance_bins == "44|91|163", 
      "44|91|163|432")
  ) %>%
  filter(
    euring_species_code %in% species_list$euring,
    transect == 2, # within the transect
    !is.na(number), # 
    !distance %in% c("E", "U", "W", "F") # remove records with no specific info on distance or in flight
    ) %>%
  select(
    tripkey, data_provider, season, date, 
    platform_type, count_method, species_counted, 
    euring_species_code, distance, distance_bins, 
    number, beaufort) %>%
  mutate(
    distance = as.numeric(distance)
  )

# run through species, platforms and distance bins...
d_dist <- d_sel %>% 
  group_by(
    euring_species_code, platform_type, distance_bins
  ) %>%
  summarise(
    n_ind = sum(number),
    n_clusters = sum(number > 0 & !is.na(number)),
    .groups = 'drop'
  )
  
if(load_models) {
  load(file = file.path(out_dir, "ddf_list.rdata"))
} else {
  ddf_list <- list()
  for (i in 1:nrow(d_dist)) {
    sp_dat <- d_sel %>%
      filter(
        euring_species_code == d_dist$euring_species_code[i],
        platform_type == d_dist$platform_type[i],
        distance_bins == d_dist$distance_bins[i],
        #!is.na(beaufort)
        distance != "E"
      ) %>%
      rename(
        seastate = beaufort,
        size = number
      )
    
    # covariate check
    if(sum(is.na(sp_dat$seastate)) / nrow(sp_dat) > 0.5) {
      covs <- NULL
    } else {
      covs <- c('seastate')
      sp_dat <- sp_dat %>%
        filter(!is.na(seastate))
    }
    # recalc sample sizes
    d_dist$n_ind[i] <- sum(sp_dat$size)
    d_dist$n_clusters[i] <- nrow(sp_dat)
    
    # fit models
    cps <- as.numeric(unlist(strsplit(
      d_dist$distance_bins[i], split = "|", fixed = TRUE)[[1]]))
    
    if (1252 %in% cps) {
      cps <- cps[1:(length(cps)-1)]
      sp_dat <- sp_dat %>% 
        filter(
          distance != 761
        )
    }
    if(353 == last(cps) & sum(sp_dat$distance == 353) == 0) {
      cps <- cps[-length(cps)]
    }
    
    ddf_list[[i]] <- distance_curve(
      d_flat = sp_dat,
      covs = covs,
      keys = c('hn', 'hr'),
      adjs = c('none', 'cos'),
      cps = cps
    )
    beepr::beep()
    save(
      ddf_list, d_dist, d_sel, 
      file = file.path(out_dir, "ddf_list.rdata")
    )
  }
}

# fix because running euring 6450 took forever; taken from older version of ddf_list
# ddf_list_now <- ddf_list
# d_dist_now <- d_dist
# d_sel_now <- d_sel
# load(file = "/Users/robvb/Documents/tmp/MORUS/Output/ddf_list.rdata", verbose = TRUE)
# 
# ddf_list_now[57:60] <- ddf_list[45:48]
# ddf_list <- ddf_list_now
# d_dist <- d_dist_now
# d_sel <- d_sel_now
# error in i == c(4, )

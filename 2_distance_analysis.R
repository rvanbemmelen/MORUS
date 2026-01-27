
### Distance analysis ESAS/MWTL data ------------------------
#' Rob van Bemmelen
#' v1 - 2022
#' v2 - 18 December 2025

# directories
dir_git <- dirname(rstudioapi::getSourceEditorContext()$path)
dir_dat <- file.path(dir_git, "data")
dir_out <- file.path(dir_git, "output")

# setting
load_models <- FALSE

# libraries
library(dplyr)
library(mrds)

# source functions
source("/Users/robvb/Documents/github/basic_stuff/base_functions.r")
source_dir("/Users/robvb/Documents/github/MWTL_surveys/R/")

# load data ------------------------
# species list
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
#'  - check distance and distancebins columns
#' 3. behaviour (flight vs not flying, only for ship-based I guess) 
#'    (no detection loss assumed for birds in flight during ESAS)
#' 4. platform (airplane vs ship) 
#'  - platform_type == 3 for airplane, 1 for ship
#' 5. platform height (continuous, unclear where Waggitt got that from...)
#' 6. seastate (continuous)

# latest data file
c_rds <- list.files(file.path(dir_out, "dataset"), ".rds")
c_rds_dates <- as.numeric(stringr::str_sub(c_rds, 15, -4))

# load processed data and filter missing distance records
d <- readRDS(
  file.path(
    dir_out, 
    "dataset", 
    c_rds[which.max(c_rds_dates)])
  ) %>%
  mutate(
    km_travelled = distance # to avoid confusion with distancebins/observationdistance!
  ) %>%
  filter(
    !is.na(observationdistance)
  )
# exploration
## distance bins
table(is.na(d$distancebins)) # still many missing values in distancebins...
table(d$distancebins)
table(d$observationdistance[is.na(d$distancebins)])
table(d$observationdistance)
# W = water

## ESAS distance bins within the transect (transect == 2) are distance bands A-D. Use midpoints for distance analysis:
d <- d %>%
  filter(
    # remove records with no info on 'exact' observation distance, in flight or out of the transect
    !observationdistance %in% c("", ">1000","E", "U", "W", "F")
  ) %>%
  mutate(
    distancebins = replace(
      distancebins,
      is.na(distancebins) & !is.na(observationdistance),
      "0|50|100|200|300"
    ),
    # to make clear that these are two- or one-sided; will not be used in distance
    transectwidth = replace(
      transectwidth,
      transectwidth == 500, 
      600),
    transectwidth = replace(
      transectwidth,
      transectwidth == 200,
      300),
    distancebins = replace(
      distancebins,
      observationdistance %in% LETTERS[1:6] & !is.na(observationdistance),
      "0|50|100|200|300"
    ),
    observationdistance = replace(
      observationdistance,
      observationdistance == "A" & distancebins == "0|50|100|200|300",
      25),
    observationdistance = replace(
      observationdistance,
      observationdistance == "B" & distancebins == "0|50|100|200|300",
      75),
    observationdistance = replace(
      observationdistance,
      observationdistance == "C" & distancebins == "0|50|100|200|300",
      150),
    observationdistance = replace(
      observationdistance,
      observationdistance == "D" & distancebins == "0|50|100|200|300", 
      250),
    distancebins = replace(
      distancebins,
      is.na(distancebins) & observationdistance %in% c("89", "117", "145", "256"),
      "75|103|131|159|353"),
    distancebins = replace(
      distancebins,
      is.na(distancebins) & observationdistance %in% c("U", "W"), 
      "0|50|100|200|300"),
    distancebins = replace(
      distancebins,
      is.na(distancebins) & transectwidth %in% c(300,600),
      "0|50|100|200|300"),
    distancebins = replace(
      distancebins, 
      distancebins == "44|91|163", 
      "44|91|163|432")
    )

# data selection: only Dutch aerial survey data
d_sel <- d %>% 
  filter(
    distancebins == "0|35|54|91|165|449|1252"
  ) %>%
  mutate(
    # unID divers to RTDivers
    speciescode = replace(
      speciescode, 
      speciescode == 59, 
      20), 
    # Common/Arctic Terns to commic terns
    speciescode = replace(
      speciescode, 
      speciescode %in% c(6150, 6160), 6169),
    observationdistance = as.numeric(observationdistance)
  ) %>%
  filter(
    speciescode %in% d_sps$euring,
    transect == "True", # within the transect
    !is.na(count),
    ) %>%
  select(
    campaignid, datarightsholder, date, # season, 
    platformclass, platformcode, targettaxa, # count_method, 
    speciescode, observationdistance, distancebins, 
    count, windforce) %>%
  rename(
    distance = observationdistance,
    seastate = windforce,
    size = count
  )

#' check that all target species occur in the selected data
all(d_sps$euring %in% d_sel$speciescode)

# run through species, platforms and distance bins...
d_dist <- d_sel %>% 
  group_by(
    speciescode, distancebins #, platformclass , 
  ) %>%
  summarise(
    n_ind = sum(size),
    n_clusters = sum(size > 0 & !is.na(size)),
    .groups = 'drop'
  )
  
if(load_models) {
  load(file = file.path(out_dir, "ddf_list.rdata"))
} else {
  ddf_list <- list()
  for (i in 1:nrow(d_dist)) {
    print(i)
    print(d_dist$speciescode[i])
    print(Sys.time())
    sp_dat <- d_sel %>%
      filter(
        speciescode == d_dist$speciescode[i],
        #platformtype == d_dist$platformtype[i],
        #distancebins == d_dist$distancebins[i],
        #!is.na(windforce)
        #distance != "E"
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
      d_dist$distancebins[i], split = "|", fixed = TRUE)[[1]]))
    
    if (1252 %in% cps) {
      cps <- cps[1:(length(cps)-1)]
      sp_dat <- sp_dat %>% 
        filter(
          distance != 761
        )
    }
    if(353 == last(cps) & sum(sp_dat$observationdistance == 353) == 0) {
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
      file = file.path(dir_out, "distance", "ddf_list.rdata")
    )
  }
}


# directories
git_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
dat_dir <- file.path(git_dir, 'Data')
out_dir <- file.path(git_dir, "Output")

# libraries
library(dplyr)
library(mrds)

source("/Users/robvb/Documents/scripts/github/basic_stuff/base_functions.r")
source_dir("/Users/robvb/Documents/scripts/github/MWTL_surveys/R/")

# data
d <- readRDS(file.path(out_dir, "Dataset", "ESAS_MWTL_raw.rds"))

# species list
species_list <- data.frame(
  ID = 1:12,
  euring = c(710, 5920, 5910, 6000, 6020, 5690, 6360, 6340, 220, 6540, 20, 6110),
  name_uk = c(
    "Northern Gannet",
    "Herring Gull",
    "Lesser Black-backed Gull",
    "Great Black-backed Gull",
    "Black-legged Kittiwake",
    "Great Skua",
    "Razorbill",
    "Common Guillemot",
    "Northern Fulmar",
    "Atlantic Puffin",
    "Red-throated Diver",
    "Sandwich Tern"
  ),
  name_sctf = c(
    "Morus bassanus",
    "Larus argentatus",
    "Larus fuscus",
    "Larus marinus",
    "Rissa tridactyla",
    "Stercorarius skua",
    "Alca torda",
    "Uria aalge",
    "Fulmarus glacialis",
    "Fratercula arctica",
    "Gavia stellata",
    "Thalasseus sandvicensis")
)
#' Based on xlsx file, these species are considered
#' Razorbill, Black-legged Kittiwake, Great skua,	Great black-backed gull,
#' Sandwich tern, Northern gannet, Lesser black-backed gull,	Northern fulmar,
#' Atlantic puffin, Red-throated diver,	Common guillemot,	Herring gull
#' so in total, 12 species
#' For the distance analysis, only individuals identified to species level are
#' considered, except for Red-throated Diver, for which also unidentified divers 
#' were included.

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
    euring_species_code = replace(euring_species_code, euring_species_code==59, 20), # unID divers to RTDivers
    distance_bins = replace(distance_bins, distance_bins == "44|91|163", "44|91|163|432")
  ) %>%
  filter(
    euring_species_code %in% species_list$euring,
    transect == 2, # within the transect
    !is.na(number), # 
    !distance %in% c("E", "U", "W", "F") # remove records with no specific info on distance or in flight
    ) %>%
  select(
    tripkey, data_provider, season, date, platform_type, count_method, species_counted, 
    euring_species_code, distance, distance_bins, number, beaufort) %>%
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
  
load(file = file.path(out_dir, "ddf_list.rdata"))
#ddf_list <- list()
for (i in 1:nrow(d_dist)) {
#for (i in which(d_dist$euring_species_code==710)) {  
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
  if(sum(is.na(sp_dat$seastate))/nrow(sp_dat) > 0.5) {
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
  if(353 == last(cps) & sum(sp_dat$distance==353)==0) {
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

# error in i == c(4, )

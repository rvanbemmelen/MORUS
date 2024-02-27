
# data preparations all species
# Rob van Bemmelen
# June 2023

# prepare data for INLA

#' Data notes: 
#' ESAS data via Mardik
#' MWTL data via Job
#' - both have same structure/column names, but these differ from the more recent files at these links:
#'   https://esas.ices.dk/inventoryen 
#'   https://ices-tools-dev.github.io/esas/tables/
#' - combined files for ESAS and MWTL
#' - full join of effort and sightings data
#' - sf object
#' - CRS EPSG: 32631 (UTM 31N)
#' - CRS EPSG: 4326 (lat/lon)
#' - some data has strip width of 600m and is thus double-sided
#' - other double-sided data (OWEZ/PAWP) is probably included with different poskeys
#' - some aerial survey data has 1s intervals and needs to be resampled 
#' - other data is retained as original and needs to be aggregated to poskey level
#' - some weird positions (end points only of transects?) around the Brown Ridge with tripkey 120073977, with NAs in campaign_key... and where the data_provider (120) cannot be retrieved... Data from 2014-02-27 only. Same shit in raw data from esas.ices.dk repository.

#' Codes die wij gesplit hebben in verschillende soorten, maar nu nog niet gedaan:
#' 6345 --> zeekoet/alk,
#' 6549 --> Alcidae; zeekoet (6340), alk (6360), Papegaaiduiker (6540),
#' 5919 --> lesser black backed gull (5910) / herring gull (5920)

#' Split Euring 6009 - Unidentified larus gull in meest voorkomende meeuwen: Kleine mantelmeeuw (5910), Zilvermeeuw (5920), Pontische meeuw (5927), geelpootmeeuw (5926), Grote Mantelmeeuw (6000), kokmeeuw (5820) en stormmeeuw (5900)
#' Split 6049 unidentified gull in  meest voorkomende meeuwen Kleine mantelmeeuw (5910), Zilvermeeuw (5920), Pontische meeuw (5927), geelpootmeeuw (5926), Grote Mantelmeeuw (6000),kokmeeuw (5820) en stormmeeuw (5900), drieteenmeeuw (6020)
#' split 59 (divers) into 20 and 30

#' 1) Razorbill vs Common Guillemot. This is the most problematic species pair, as some survey methods don't allow identification of these to species level. As we are to report each species separately, such surveys need to be excluded from the analysis. This could be done by calculating the % of unidentified large auks; when this exceeds a certain threshold (a few percent), that method needs to be excluded.



# directories
git_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
dat_dir <- file.path(git_dir, 'Data')
out_dir <- file.path(git_dir, "Output")
fun_dir <- file.path(git_dir, "Functions")
esas_dir <- "/Users/robvb/Documents/data/ESAS/ESAS_0516085655/" # https://esas.ices.dk/inventory

# libraries
library(dplyr)
library(sf)
library(mrds)

# functions
# source(file.path(fun_dir))

#' Job:
#' Voor ship surveys wordt alles omgezet naar 5 minuut-poskeys, voor 
#' vliegtuigtellingen naar 1 minuut poskeys. Per nieuwe positie worden 
#' de aantallen per soort gesommeerd. Als de data nog te veel nullen 
#' bevat kan je misschien aggregeren naar 10 minuten ofzo. En als je 
#' nog info wilt bewaren over gedrag, leeftijd etc dan moet je die 
#' kolommen toevoegen in select() en group_by().

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
    "Thalasseus sandvicensis"),
  label = c(
    "Northern_Gannet",
    "Herring_Gull",
    "Lesser_Black-backed_Gull",
    "Great_Black-backed_Gull",
    "Black-legged_Kittiwake",
    "Great_Skua",
    "Razorbill",
    "Common_Guillemot",
    "Northern_Fulmar",
    "Atlantic_Puffin",
    "Red-throated_Diver",
    "Sandwich_Tern"
  )
)

# list of target taxa codes to be included for each species
target_taxa <- list(
  c(1, 2, 3, 4, 7, 10, 11, 12, 13), # "Northern_Gannet"
  c(1, 3, 8, 10, 11, 12, 13, 14, 15), # "Herring_Gull"
  c(1, 3, 8, 10, 11, 13, 14, 15), # "Lesser_Black-backed_Gull"
  c(1, 3, 8, 10, 11, 12, 13, 14, 15), # "Great_Black-backed_Gull"
  c(1, 2, 3, 8, 10, 12, 13, 15), # "Black-legged_Kittiwake"
  c(1, 2, 3, 4, 7, 8, 10, 11, 12, 13, 14, 15), # "Great_Skua"
  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 17), # "Razorbill"
  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 17), # "Common_Guillemot"
  c(1, 2, 7, 8, 10, 11, 12, 13), # "Northern_Fulmar"
  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15), # "Atlantic_Puffin"
  c(1, 2, 3, 4, 7, 8, 11, 12, 14, 15, 17), # "Red-throated_Diver"
  c(1, 2, 3, 4, 7, 8, 10, 11, 12, 13, 14, 15) # "Sandwich_Tern"
)

#' start for loop voor het laden van de data om zeker te weten dat er geen legacy versies van objecten zijn...

run_these <- which(species_list$euring %in% c(5910, 5920, 6000, 6020, 6340, 6360))

#for (sp in 1:nrow(species_list)) {
for (sp in run_these) {
  print(species_list$name_uk[sp])
  
  # load raw data
  # df_observations <- read.csv("observations.csv", sep = ',')
  # df_positions <- read.csv("positions.csv", sep = ',')
  # df_campaign<- read.csv("campaigns.csv", sep = ',')
  # df_samples<- read.csv("samples.csv", sep = ',')
  
  # ESAS data ####
  cat('load and prepare ESAS data...\n')
  esas_obs <- read.csv(file.path(esas_dir, "Observations.csv"))
  esas_eff <- read.csv(file.path(esas_dir, "Positions.csv"))
  esas_sam <- read.csv(file.path(esas_dir, "Samples.csv"))
  esas_cam <- read.csv(file.path(esas_dir, "Campaigns.csv"))
  
  # fix error in esas_sam$PlatformSide
  esas_sam_these <- which(duplicated(data.frame(
    as.character(esas_sam$CampaignID), 
    esas_sam$PlatformCode, 
    esas_sam$Date,
    esas_sam$PlatformSide)))
  esas_sam$PlatformSide[esas_sam_these] <- "left"
  esas_sam$Date <- as.Date(esas_sam$Date)
  
  esas_eff <- left_join(
    esas_eff, 
    esas_sam[,c('SampleID','Date', 'PlatformClass', 
                'PlatformCode', 'TransectWidth', 'PlatformSide',
                'SamplingMethod', 'TargetTaxa', 'DistanceBins')], 
    by = "SampleID"
  ) %>%
    mutate(
      DistanceBins = replace(
        DistanceBins, 
        DistanceBins == "" & PlatformClass == 30, 
        "0|50|100|200|300"),
      PlatformSide = replace(PlatformSide, PlatformSide=="left_back", "left"),
      PlatformSide = replace(PlatformSide, PlatformSide=="right_front", "right"),
      datetime = as.POSIXct(strptime(paste0(Date, Time), format = "%Y-%m-%d %H:%M:%S"), tz = "UTC"),
      platform = ifelse(
        PlatformClass == 30,
        "ship", "aerial"
      )
    ) %>%
    filter(
      DistanceBins != "",
      SamplingMethod != 3,
      TargetTaxa %in% target_taxa[[sp]] # NB: change for other species!! This is for kittiwake
    ) %>% 
    group_by(CampaignID, Date) %>%
    arrange(datetime) %>%
    mutate(dtime = as.vector(difftime(datetime, lag(datetime), units = 'mins'))) %>%
    ungroup() %>%
    select(
      CampaignID,
      PositionID,
      datetime,
      Longitude,
      Latitude,
      Distance,
      Area,
      SamplingMethod,
      TargetTaxa,
      platform,
      PlatformSide,
      DistanceBins,
      TransectWidth,
      WindForce
    ) %>%
    mutate( # NOTE: was rename but this is problematic for species with unID-ed individuals, for which we need to link tables.
      campaign_key = CampaignID,
      poskey = PositionID,
      lon = Longitude,
      lat = Latitude,
      km_travelled = Distance,
      area_surveyed = Area,
      distance_bins = DistanceBins,
      beaufort = WindForce
    )
  esas_ships <- esas_obs %>%
    filter(
      Association == 26
    ) %>%
    distinct(
      PositionID
    ) %>%
    mutate(
      fisheries_active = TRUE
    ) %>%
    rename(
      poskey = PositionID
    )
  esas_asso_birds <- esas_obs %>% # list of poskeys with no birds that potentially follow fishery vessels
    filter(
      SpeciesCode > 220, # after Northern Fulmar (excluding divers/grebes)
      ! SpeciesCode %in% 1010:5659, # excluding anatidae, waders, etc
      SpeciesCode < 6340 # before Common Guillemot (excluding auks, doves, passerines, etc)
    ) %>%
    group_by(
      PositionID
    ) %>%
    summarise(
      n_ass_birds = sum(Count, na.rm = TRUE)
    ) %>% 
    rename(
      poskey = PositionID
    )
  
  esas_eff <- left_join(
    x = esas_eff,
    y = esas_ships,
    by = "poskey"
  ) %>%
    mutate(
      fisheries_active = ifelse(is.na(fisheries_active), FALSE, TRUE),
      esw = NA,
      campaign_key = as.character(campaign_key),
      poskey = as.character(poskey),
      old_mwtl = FALSE
    ) %>%
    mutate(
      fisheries_active = ifelse(poskey %in% esas_asso_birds$poskey, fisheries_active, NA)
    )
  
  esas_obs <- left_join(
    esas_obs,
    esas_eff[,c("CampaignID", "PositionID", "datetime")], 
    by = c("CampaignID", "PositionID")
  ) %>%
    mutate(
      date = as.Date(datetime)
    )
  
  # esas_obs addition for unidentified gulls
  if( species_list$euring[sp] %in% c(5910, 5920, 6000, 6020)) {
    # Split Gulls
    # 5919: Herring + Lesser Black-backed Gull
    # 6009: Kleine mantelmeeuw (5910), Zilvermeeuw (5920), Pontische meeuw (5927), geelpootmeeuw (5926), Grote Mantelmeeuw (6000), kokmeeuw (5820) en stormmeeuw (5900)
    # 6049: Kleine mantelmeeuw (5910), Zilvermeeuw (5920), Pontische meeuw (5927), geelpootmeeuw (5926), Grote Mantelmeeuw (6000), kokmeeuw (5820), stormmeeuw (5900) en drieteenmeeuw (6020)

    # 5919: Herring or Lesser Black-backed Gull
    n_5919 <- esas_obs %>%
      filter(
        SpeciesCode == 5919
      )
    frac_5919 <- esas_obs %>% 
      mutate(
        SpeciesCode = if_else(SpeciesCode %in% c(5911, 5912), 5910, SpeciesCode), # LBBG ssp to LBBG
        SpeciesCode = if_else(SpeciesCode %in% c(5921), 5920, SpeciesCode) # HG ssp to HG
      ) %>%
      filter(SpeciesCode %in% c(5910, 5920)) %>% 
      group_by(CampaignID, date, SpeciesCode) %>% 
      summarise(sum_number = sum(Count), .groups = "drop") %>% 
      tidyr::pivot_wider(names_from = SpeciesCode, values_from = sum_number) %>% 
      rename(
        sp_5910 = '5910',
        sp_5920 = '5920'
      ) %>% 
      mutate(
        sp_5910 = tidyr::replace_na(sp_5910,0),
        sp_5920 = tidyr::replace_na(sp_5920,0), 
        n_ided = sp_5910 + sp_5920,
        fraction_5910 = sp_5910/n_ided,
        fraction_5920 = sp_5920/n_ided) %>% 
      dplyr::select(-sp_5910, -sp_5920)
    
    # 6009: unidentified large gulls
    n_6009 <- esas_obs %>%
      filter(
        SpeciesCode == 6009
      )
    frac_6009 <- esas_obs %>% 
      mutate(
        SpeciesCode = if_else(SpeciesCode %in% c(5911, 5912), 5910, SpeciesCode), # LBBG ssp to LBBG
        SpeciesCode = if_else(SpeciesCode %in% c(5921), 5920, SpeciesCode) # HG ssp to HG
      ) %>%
      filter(SpeciesCode %in% c(5820, 5900, 5910, 5920, 5927, 5926, 6000)) %>% 
      group_by(CampaignID, date, SpeciesCode) %>% 
      summarise(
        sum_number = sum(Count),
        .groups = "drop") %>% 
      tidyr::pivot_wider(names_from = SpeciesCode, values_from = sum_number) %>% 
      rename(
        sp_5820 = '5820',
        sp_5900 = '5900',
        sp_5910 = '5910', 
        sp_5920 = '5920', 
        sp_5926 = '5926',
        sp_5927 = '5927',
        sp_6000 = '6000'
      ) %>% 
      mutate(
        sp_5910 = tidyr::replace_na(sp_5910,0),
        sp_5920 = tidyr::replace_na(sp_5920,0),
        sp_5927 = tidyr::replace_na(sp_5927,0),
        sp_5926 = tidyr::replace_na(sp_5926, 0),
        sp_6000 = tidyr::replace_na(sp_6000, 0),
        sp_5820 = tidyr::replace_na(sp_5820, 0),
        sp_5900 = tidyr::replace_na(sp_5900, 0),
        n_ided = (sp_5910 + sp_5920 + sp_5927 + sp_5926 + sp_6000 + sp_5820 + sp_5900),
        fraction_5910 = sp_5910 / n_ided,
        fraction_5920 = sp_5920 / n_ided,
        fraction_5927 = sp_5927 / n_ided,
        fraction_5926 = sp_5926 / n_ided,
        fraction_6000 = sp_6000 / n_ided,
        fraction_5820 = sp_5820 / n_ided,
        fraction_5900 = sp_5900 / n_ided) %>% 
      dplyr::select(
        -sp_5910,-sp_5920,-sp_5927,-sp_5926,-sp_6000,-sp_5820,-sp_5900)
    
    
    # 6049: unidentified gulls including small gulls
    n_6049 <- esas_obs %>% 
      filter(
        SpeciesCode == 6049
      )
    frac_6049 <- esas_obs %>% 
      mutate(
        SpeciesCode = if_else(SpeciesCode %in% c(5911, 5912), 5910, SpeciesCode), # LBBG ssp to LBBG
        SpeciesCode = if_else(SpeciesCode %in% c(5921), 5920, SpeciesCode) # HG ssp to HG
      ) %>%
      filter(SpeciesCode %in% c(5910,5920,5927,5926,6000,5820,5900,6020)) %>% 
      group_by(CampaignID, date, SpeciesCode) %>% 
      summarise(sum_number = sum(Count), .groups = "drop") %>% 
      tidyr::pivot_wider(names_from = SpeciesCode, values_from = sum_number) %>% 
      rename(
        sp_5910 = '5910',
        sp_5920 = '5920',
        sp_5927 = '5927',
        sp_5926 = '5926',
        sp_6000 = '6000',
        sp_5820 = '5820',
        sp_5900 = '5900',
        sp_6020 = '6020') %>% 
      mutate(
        sp_5910 = tidyr::replace_na(sp_5910,0),
        sp_5920 = tidyr::replace_na(sp_5920, 0),
        sp_5927 = tidyr::replace_na(sp_5927, 0),
        sp_5926 = tidyr::replace_na(sp_5926, 0),
        sp_6000 = tidyr::replace_na(sp_6000, 0),
        sp_5820 = tidyr::replace_na(sp_5820, 0),
        sp_5900 = tidyr::replace_na(sp_5900, 0),
        sp_6020 = tidyr::replace_na(sp_6020, 0),
        n_ided = (sp_5910+sp_5920+sp_5927+sp_5926+sp_6000+sp_5820+sp_5900+sp_6020),
        fraction_5910 = sp_5910/n_ided,
        fraction_5920 = sp_5920 / n_ided,
        fraction_5927 = sp_5927 / n_ided,
        fraction_5926 = sp_5926 / n_ided,
        fraction_6000 = sp_6000 / n_ided,
        fraction_5820 = sp_5820 / n_ided,
        fraction_5900 = sp_5900 / n_ided,
        fraction_6020 = sp_6020 / n_ided) %>% 
      dplyr::select(
        -sp_5910,-sp_5920,-sp_5927,-sp_5926,-sp_6000,-sp_5820,-sp_5900,-sp_6020
      )
    
    # collect data per speciesgroup
    data_5919 <- esas_obs %>% 
      filter(SpeciesCode == 5919) %>% 
      left_join(frac_5919, by = c("CampaignID", "date")) %>% 
      mutate("5910" = round(Count * fraction_5910), 
             "5920" = round(Count * fraction_5920)
      ) %>% 
      dplyr::select(
        -SpeciesCode, -Count, 
        -fraction_5910,-fraction_5920
      ) %>% 
      tidyr::pivot_longer(
        cols = c("5910":"5920"), 
        names_to = "SpeciesCode", values_to = "Count") %>% 
      mutate(SpeciesCode = as.numeric(SpeciesCode))
    data_6009 <- esas_obs %>% 
      filter(SpeciesCode == 6009) %>% 
      left_join(frac_6009, by = c("CampaignID", "date")) %>% 
      mutate("5910" = round(Count * fraction_5910), 
             "5920" = round(Count * fraction_5920),
             "5927" = round(Count * fraction_5927),
             "5926" = round(Count * fraction_5926), 
             "6000" = round(Count * fraction_6000),
             "5820" = round(Count * fraction_5820),
             "5900" = round(Count * fraction_5900)) %>% 
      dplyr::select(
        -SpeciesCode, -Count, 
        -fraction_5910,-fraction_5920,-fraction_5927,-fraction_5926,-fraction_6000,-fraction_5820,-fraction_5900
      ) %>%
      tidyr::pivot_longer(
        cols = c("5910":"5920","5927","5926","6000","5820","5900"), 
        names_to = "SpeciesCode", values_to = "Count") %>% 
      mutate(
        SpeciesCode = as.numeric(SpeciesCode)
      )
    data_6049 <- esas_obs %>% 
      filter(SpeciesCode == 6049) %>% 
      left_join(frac_6049, by = c("CampaignID", "date")) %>% 
      mutate("5910" = round(Count * fraction_5910), 
             "5920" = round(Count * fraction_5920),
             "5927" = round(Count * fraction_5927),
             "5926" = round(Count * fraction_5926), 
             "6000" = round(Count * fraction_6000),
             "5820" = round(Count * fraction_5820),
             "5900" = round(Count * fraction_5900),
             "6020" = round(Count * fraction_6020)) %>% 
      dplyr::select(
        -SpeciesCode, -Count, 
        -fraction_5910,-fraction_5920,-fraction_5927,-fraction_5926,
        -fraction_6000,-fraction_5820,-fraction_5900,-fraction_6020
      ) %>% 
      tidyr::pivot_longer(
        cols = c("5910":"5920","5927","5926","6000","5820","5900","6020"), 
        names_to = "SpeciesCode", values_to = "Count") %>% 
      mutate(SpeciesCode = as.numeric(SpeciesCode))
    
    # thus, data is in 
    # - data_6049 - relevant for HR, LBBG, GBBG, BLK
    # - data_6009 - relevant for HR, LBBG, GBBG
    # - data_5919 - relevant for HR and LBBG

    # data_5919: Herring + Lesser Black-backed Gull
    # data_6009: Kleine mantelmeeuw (5910), Zilvermeeuw (5920), Grote Mantelmeeuw (6000)
    # data_6049: Kleine mantelmeeuw (5910), Zilvermeeuw (5920), Grote Mantelmeeuw (6000) en drieteenmeeuw (6020)
    if (species_list$euring[sp] %in% c(5910, 5920)) { # LBBG, HG or GBBG
      esas_obs <- bind_rows(esas_obs, data_5919, data_6009, data_6049)
    }
    if (species_list$euring[sp] %in% c(6000)) { # LBBG, HG or GBBG
      esas_obs <- bind_rows(esas_obs, data_6009, data_6049)
    }
    if (species_list$euring[sp] %in% c(6020)) { # LBBG, HG or GBBG
      esas_obs <- bind_rows(esas_obs, data_6049)
    }
  }
  if (species_list$euring[sp] %in% c(6340, 6360)) {
    # 6345: Common Guillemot and Razorbill
    n_6345 <- esas_obs %>%
      filter(
        SpeciesCode == 6345
      )
    frac_6345 <- esas_obs %>% 
      filter(SpeciesCode %in% c(6340, 6360)) %>% 
      group_by(CampaignID, date, SpeciesCode) %>% 
      summarise(sum_number = sum(Count), .groups = "drop") %>% 
      tidyr::pivot_wider(names_from = SpeciesCode, values_from = sum_number) %>% 
      rename(
        sp_6340 = '6340',
        sp_6360 = '6360'
      ) %>% 
      mutate(
        sp_6340 = tidyr::replace_na(sp_6340,0),
        sp_6360 = tidyr::replace_na(sp_6360,0), 
        n_ided = sp_6340 + sp_6360,
        fraction_6340 = sp_6340/n_ided,
        fraction_6360 = sp_6360/n_ided) %>% 
      dplyr::select(-sp_6340, -sp_6360)
    data_6345 <- esas_obs %>% 
      filter(SpeciesCode == 6345) %>% 
      left_join(frac_6345, by = c("CampaignID", "date")) %>% 
      mutate("6340" = round(Count * fraction_6340), 
             "6360" = round(Count * fraction_6360)
      ) %>% 
      dplyr::select(
        -SpeciesCode, -Count, 
        -fraction_6340,-fraction_6360
      ) %>% 
      tidyr::pivot_longer(
        cols = c("6340":"6360"), 
        names_to = "SpeciesCode", values_to = "Count") %>% 
      mutate(SpeciesCode = as.numeric(SpeciesCode))

    # combine    
    esas_obs <- bind_rows(esas_obs, data_6345)

  }
    
  # esas_obs for identified birds
  esas_obs <- esas_obs %>%
    filter(
      SpeciesCode == species_list$euring[sp],
      PositionID %in% esas_eff$poskey,
      Transect == "True"
    ) %>%
    rename(
      poskey = PositionID
    ) %>%
    group_by(
      poskey
    ) %>%
    summarise(
      count = sum(Count, na.rm = TRUE),
      count_flying = sum(Count[ObservationDistance == "F"], na.rm = TRUE),
      count_swimming = count - count_flying
    ) %>%
    mutate(
      poskey = as.character(poskey)
    )
  
  # MWTL data ####
  cat('load and prepare MWTL data...\n')
  
  # from Susanne, via Mardik + Job 
  d <- readRDS(file.path(out_dir, "Dataset", "ESAS_MWTL_raw.rds"))
  mwtl <- d %>%
    filter(origin == "MWTL") %>%
    mutate(
      old_mwtl = ifelse(
        is.na(km_travelled),
        TRUE, FALSE),
      campaign_key = tripkey,
      PlatformSide = ifelse(base_side == 1, "left", "right"),
      km_travelled = ifelse(
        is.na(km_travelled),
        area_surveyed/0.1,
        km_travelled
      )
    ) %>%
    group_by(
      campaign_key, date
    ) %>%
    mutate(
      datetime = as.POSIXct(
        ifelse(
          time == "NA:NA:NA",
          seq(
            first(as.POSIXct(
              paste0(date, " 08:30:00"), tz = "UTC", format = "%Y-%m-%d %H:%M:%S"
            )),
            first(as.POSIXct(
              paste0(date, " 08:30:00"), tz = "UTC", format = "%Y-%m-%d %H:%M:%S"
            )) + n()*90,
            90
          ),
          as.POSIXct(paste0(date, " ", time), tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
        ),
        tz = "UTC", origin = "1970-01-01 00:00.00 UTC")
    ) %>%
    ungroup() %>%
    st_transform(crs = 4326) %>%
    mutate(lon = sf::st_coordinates(.)[, 1],
           lat = sf::st_coordinates(.)[, 2]) %>%
    st_drop_geometry()
  
  mwtl_eff <- mwtl %>%
    select(
      campaign_key,
      old_mwtl,
      poskey,
      datetime,
      lon,
      lat,
      km_travelled,
      area_surveyed,
      platform_type,
      count_method,
      distance_bins,
      beaufort) %>%
    distinct(
      campaign_key,
      platform_type,
      poskey,
      datetime,
      lon,
      lat,
      .keep_all = TRUE) %>% 
    mutate(
      platform = ifelse(
        platform_type == 3,
        "aerial",
        "ship"
      ),
      distance_bins = ifelse(
        is.na(distance_bins),
        c("0|100"),
        distance_bins)
    ) %>%
    rename(
      SamplingMethod = count_method
    )
  
  # code for unidentified birds
  if (species_list$euring[sp] %in% c(5910, 5920, 6000, 6020)) {
    # 5919: Herring or Lesser Black-backed Gull
    n_5919 <- mwtl %>%
      filter(
        euring_species_code == 5919
      )
    frac_5919 <- mwtl %>% 
      mutate(
        euring_species_code = if_else(euring_species_code %in% c(5911, 5912), 5910, euring_species_code), # LBBG ssp to LBBG
        euring_species_code = if_else(euring_species_code %in% c(5921), 5920, euring_species_code) # HG ssp to HG
      ) %>%
      filter(euring_species_code %in% c(5910, 5920)) %>% 
      group_by(tripkey, date, euring_species_code) %>% 
      summarise(sum_number = sum(number), .groups = "drop") %>% 
      tidyr::pivot_wider(names_from = euring_species_code, values_from = sum_number) %>% 
      rename(
        sp_5910 = '5910',
        sp_5920 = '5920'
      ) %>% 
      mutate(
        sp_5910 = tidyr::replace_na(sp_5910,0),
        sp_5920 = tidyr::replace_na(sp_5920,0), 
        n_ided = sp_5910 + sp_5920,
        fraction_5910 = sp_5910/n_ided,
        fraction_5920 = sp_5920/n_ided) %>% 
      dplyr::select(-sp_5910, -sp_5920)
    
    # 6009: unidentified large gulls
    n_6009 <- mwtl %>%
      filter(
        euring_species_code == 6009
      )
    frac_6009 <- mwtl %>% 
      mutate(
        euring_species_code = if_else(euring_species_code %in% c(5911, 5912), 5910, euring_species_code), # LBBG ssp to LBBG
        euring_species_code = if_else(euring_species_code %in% c(5921), 5920, euring_species_code) # HG ssp to HG
      ) %>%
      filter(euring_species_code %in% c(5820, 5900, 5910, 5920, 5927, 6000)) %>% 
      group_by(tripkey, date, euring_species_code) %>% 
      summarise(
        sum_number = sum(number),
        .groups = "drop") %>% 
      tidyr::pivot_wider(names_from = euring_species_code, values_from = sum_number) %>% 
      rename(
        sp_5820 = '5820',
        sp_5900 = '5900',
        sp_5910 = '5910', 
        sp_5920 = '5920', 
        sp_5927 = '5927',
        sp_6000 = '6000'
      ) %>% 
      mutate(
        sp_5910 = tidyr::replace_na(sp_5910,0),
        sp_5920 = tidyr::replace_na(sp_5920,0),
        sp_5927 = tidyr::replace_na(sp_5927,0),
        sp_6000 = tidyr::replace_na(sp_6000, 0),
        sp_5820 = tidyr::replace_na(sp_5820, 0),
        sp_5900 = tidyr::replace_na(sp_5900, 0),
        n_ided = (sp_5910 + sp_5920 + sp_5927 + sp_6000 + sp_5820 + sp_5900),
        fraction_5910 = sp_5910 / n_ided,
        fraction_5920 = sp_5920 / n_ided,
        fraction_5927 = sp_5927 / n_ided,
        fraction_6000 = sp_6000 / n_ided,
        fraction_5820 = sp_5820 / n_ided,
        fraction_5900 = sp_5900 / n_ided) %>% 
      dplyr::select(
        -sp_5910,-sp_5920,-sp_5927,-sp_6000,-sp_5820,-sp_5900)
    
    
    # 6049: unidentified gulls including small gulls
    n_6049 <- mwtl %>% 
      filter(
        euring_species_code == 6049
      )
    frac_6049 <- mwtl %>% 
      mutate(
        euring_species_code = if_else(euring_species_code %in% c(5911, 5912), 5910, euring_species_code), # LBBG ssp to LBBG
        euring_species_code = if_else(euring_species_code %in% c(5921), 5920, euring_species_code) # HG ssp to HG
      ) %>%
      filter(euring_species_code %in% c(5910,5920,5927,6000,5820,5900,6020)) %>% 
      group_by(tripkey, date, euring_species_code) %>% 
      summarise(sum_number = sum(number), .groups = "drop") %>% 
      tidyr::pivot_wider(names_from = euring_species_code, values_from = sum_number ) %>% 
      rename(
        sp_5910 = '5910',
        sp_5920 = '5920',
        sp_5927 = '5927',
        sp_6000 = '6000',
        sp_5820 = '5820',
        sp_5900 = '5900',
        sp_6020 = '6020') %>% 
      mutate(
        sp_5910 = tidyr::replace_na(sp_5910,0),
        sp_5920 = tidyr::replace_na(sp_5920, 0),
        sp_5927 = tidyr::replace_na(sp_5927, 0),
        sp_6000 = tidyr::replace_na(sp_6000, 0),
        sp_5820 = tidyr::replace_na(sp_5820, 0),
        sp_5900 = tidyr::replace_na(sp_5900, 0),
        sp_6020 = tidyr::replace_na(sp_6020, 0),
        n_ided = (sp_5910+sp_5920+sp_5927+sp_6000+sp_5820+sp_5900+sp_6020),
        fraction_5910 = sp_5910/n_ided,
        fraction_5920 = sp_5920 / n_ided,
        fraction_5927 = sp_5927 / n_ided,
        fraction_6000 = sp_6000 / n_ided,
        fraction_5820 = sp_5820 / n_ided,
        fraction_5900 = sp_5900 / n_ided,
        fraction_6020 = sp_6020 / n_ided) %>% 
      dplyr::select(
        -sp_5910,-sp_5920,-sp_5927,-sp_6000,-sp_5820,-sp_5900,-sp_6020
      )
    
    # collect data per speciesgroup
    data_5919 <- mwtl %>% 
      filter(euring_species_code == 5919) %>% 
      left_join(frac_5919, by = c("tripkey", "date")) %>% 
      mutate("5910" = round(number * fraction_5910), 
             "5920" = round(number * fraction_5920)
      ) %>% 
      dplyr::select(
        -euring_species_code, -number, 
        -fraction_5910,-fraction_5920
      ) %>% 
      tidyr::pivot_longer(
        cols = c("5910":"5920"), 
        names_to = "euring_species_code", values_to = "number") %>% 
      mutate(euring_species_code = as.numeric(euring_species_code))
    data_6009 <- mwtl %>% 
      filter(euring_species_code == 6009) %>% 
      left_join(frac_6009, by = c("tripkey", "date")) %>% 
      mutate("5910" = round(number * fraction_5910), 
             "5920" = round(number * fraction_5920),
             "5927" = round(number * fraction_5927),
             "6000" = round(number * fraction_6000),
             "5820" = round(number * fraction_5820),
             "5900" = round(number * fraction_5900)) %>% 
      dplyr::select(
        -euring_species_code, -number, 
        -fraction_5910,-fraction_5920,-fraction_5927,-fraction_6000,-fraction_5820,-fraction_5900
      ) %>%
      tidyr::pivot_longer(
        cols = c("5910":"5920","5927","6000","5820","5900"), 
        names_to = "euring_species_code", values_to = "number") %>% 
      mutate(
        euring_species_code = as.numeric(euring_species_code)
      )
    data_6049 <- mwtl %>% 
      filter(euring_species_code == 6049) %>% 
      left_join(frac_6049, by = c("tripkey", "date")) %>% 
      mutate("5910" = round(number * fraction_5910), 
             "5920" = round(number * fraction_5920),
             "5927" = round(number * fraction_5927),
             "6000" = round(number * fraction_6000),
             "5820" = round(number * fraction_5820),
             "5900" = round(number * fraction_5900),
             "6020" = round(number * fraction_6020)) %>% 
      dplyr::select(
        -euring_species_code, -number, 
        -fraction_5910,-fraction_5920,-fraction_5927,
        -fraction_6000,-fraction_5820,-fraction_5900,-fraction_6020
      ) %>% 
      tidyr::pivot_longer(
        cols = c("5910":"5920","5927","6000","5820","5900","6020"), 
        names_to = "euring_species_code", values_to = "number") %>% 
      mutate(euring_species_code = as.numeric(euring_species_code))
    
    # thus, data is in 
    # - data_6049 - relevant for HR, LBBG, GBBG, BLK
    # - data_6009 - relevant for HR, LBBG, GBBG
    # - data_5919 - relevant for HR and LBBG
    if (species_list$euring[sp] %in% c(5910, 5920)) {
      mwtl <- bind_rows(mwtl, data_5919, data_6009, data_6049)
    }
    if (species_list$euring[sp] == 6000) {
      mwtl <- bind_rows(mwtl, data_6009, data_6049)
    }
    if (species_list$euring[sp] == 6020) {
      mwtl <- bind_rows(mwtl, data_6049)
    }
    
  }
  if (species_list$euring[sp] %in% c(6340, 6360)) {
    # 6345: Herring or Lesser Black-backed Gull
    n_6345 <- mwtl %>%
      filter(
        euring_species_code == 6345
      )
    frac_6345 <- mwtl %>% 
      mutate(
        euring_species_code = if_else(euring_species_code %in% c(5911, 5912), 6340, euring_species_code), # LBBG ssp to LBBG
        euring_species_code = if_else(euring_species_code %in% c(5921), 6360, euring_species_code) # HG ssp to HG
      ) %>%
      filter(euring_species_code %in% c(6340, 6360)) %>% 
      group_by(tripkey, date, euring_species_code) %>% 
      summarise(sum_number = sum(number), .groups = "drop") %>% 
      tidyr::pivot_wider(names_from = euring_species_code, values_from = sum_number) %>% 
      rename(
        sp_6340 = '6340',
        sp_6360 = '6360'
      ) %>% 
      mutate(
        sp_6340 = tidyr::replace_na(sp_6340,0),
        sp_6360 = tidyr::replace_na(sp_6360,0), 
        n_ided = sp_6340 + sp_6360,
        fraction_6340 = sp_6340/n_ided,
        fraction_6360 = sp_6360/n_ided) %>% 
      dplyr::select(-sp_6340, -sp_6360)
    # collect data per speciesgroup
    data_6345 <- mwtl %>% 
      filter(euring_species_code == 6345) %>% 
      left_join(frac_6345, by = c("tripkey", "date")) %>% 
      mutate("6340" = round(number * fraction_6340), 
             "6360" = round(number * fraction_6360)
      ) %>% 
      dplyr::select(
        -euring_species_code, -number, 
        -fraction_6340,-fraction_6360
      ) %>% 
      tidyr::pivot_longer(
        cols = c("6340":"6360"), 
        names_to = "euring_species_code", values_to = "number") %>% 
      mutate(euring_species_code = as.numeric(euring_species_code))
    
    mwtl <- bind_rows(mwtl, data_6345)
  }
  
  mwtl_obs <- mwtl %>%
    filter(
      euring_species_code == species_list$euring[sp]
    ) %>%
    group_by(poskey) %>%
    summarise(
      count = sum(number, na.rm = TRUE),
      count_flying = 0,
      count_swimming = sum(number, na.rm = TRUE)
    )
  
  mwtl_ships <- mwtl %>%
    filter(
      association == 26
    ) %>%
    distinct(
      poskey
    ) %>%
    mutate(
      fisheries_active = TRUE
    )
  mwtl_asso_birds <- mwtl %>% # NUMBER of potentially associating birds per poskey
    filter(
      euring_species_code > 220, # after Northern Fulmar (excluding divers/grebes)
      ! euring_species_code %in% 1010:5659, # excluding anatidae, waders, etc
      euring_species_code < 6340 # before Common Guillemot (excluding auks, doves, passerines, etc)
    ) %>%
    group_by(
      poskey
    ) %>%
    summarise(
      n_ass_birds = sum(number, na.rm = TRUE)
    )
  
  mwtl_eff <- left_join(
    x = mwtl_eff,
    y = mwtl_ships,
    by = "poskey"
  ) %>%
    mutate(
      fisheries_active = ifelse(is.na(fisheries_active), FALSE, TRUE),
      TargetTaxa = 1,
      TransectWidth = NA
    ) %>%
    mutate(
      fisheries_active = ifelse(poskey %in% mwtl_asso_birds$poskey, fisheries_active, NA)
    )
  
  # combine ####
  cat('combine ESAS and MWTL data...\n')
  eff <- bind_rows(
    esas_eff,
    mwtl_eff
  )
  obs <- bind_rows(
    esas_obs,
    mwtl_obs
  )
  
  dat <- left_join(
    x = eff,
    y = obs,
    by = "poskey"
  ) %>%
    group_by( # there are some 'duplicates', with several poskeys at the same position/time. This will inflate the km_travelled...
      campaign_key, old_mwtl, datetime, lon, lat, km_travelled, area_surveyed,
      SamplingMethod, TargetTaxa, platform, distance_bins, PlatformSide, 
      TransectWidth, beaufort, fisheries_active, 
      esw
    ) %>%
    summarise(
      count = sum(count, na.rm = TRUE),
      count_flying = sum(count_flying, na.rm = TRUE),
      count_swimming = sum(count_swimming, na.rm = TRUE),
      .groups = 'drop'
    )
  #table(duplicated(dat$campaign_key, dat$datetime))
  
  # sampling intervals ####
  n_per_hour <- eff %>%
    mutate(
      date = as.Date(datetime),
      hour = as.POSIXlt(datetime)$hour
    ) %>%
    group_by(
      campaign_key,
      old_mwtl,
      platform,
      PlatformSide,
      date,
      hour
    ) %>%
    summarise(
      n = n(),
      .groups = "drop"
    )
  
  too_long_aerial <- n_per_hour %>%
    filter(n > 65, platform == "aerial")
  too_long_ship <- n_per_hour %>%
    filter(n > 12, platform == "ship")
  
  # fix very short intervals (old code, needs adjustment) ####
  cat("resample ship data...\n")
  df_ship_5min <- dat %>%
    filter(
      campaign_key %in% too_long_ship$campaign_key &
        as.Date(datetime) %in% too_long_ship$date &
        platform == "ship"
    ) %>%
    arrange(datetime) %>%
    mutate(
      round_time = lubridate::round_date(datetime, "5 minutes")
    ) %>%
    group_by(
      campaign_key, old_mwtl, round_time, TargetTaxa, fisheries_active,
      platform, SamplingMethod, distance_bins, PlatformSide, TransectWidth, beaufort,
      esw
    ) %>%
    summarize(
      lon = mean(lon),
      lat = mean(lat),
      km_travelled = sum(km_travelled, na.rm = TRUE),
      area_surveyed = sum(area_surveyed, na.rm = TRUE),
      count = sum(count, na.rm = TRUE),
      count_flying = sum(count_flying, na.rm = TRUE),
      count_swimming = count - count_flying,
      .groups = 'drop'
    ) %>%
    rename(
      datetime = round_time
    )
  #sf_ship_5min <- st_as_sf(x = df_ship_5min, coords = c("lon", "lat"), crs = 4326)
  
  # resample platform_type == 3, data_provider==55!
  
  # Select aerial surveys
  cat("resample aerial data...\n")
  df_aerial_1min <- dat %>% 
    filter(
      campaign_key %in% too_long_aerial$campaign_key &
        as.Date(datetime) %in% too_long_aerial$date &
        platform == "aerial"
    ) %>%
    arrange(datetime) %>%
    mutate(
      round_time = lubridate::round_date(datetime, "1 minutes")
    ) %>%
    st_drop_geometry()  %>%
    group_by(
      campaign_key, old_mwtl, round_time, TargetTaxa, fisheries_active,
      platform, SamplingMethod, distance_bins, PlatformSide, TransectWidth, beaufort,
      esw
    ) %>%
    summarize(
      lon = mean(lon),
      lat = mean(lat),
      km_travelled = sum(km_travelled, na.rm = TRUE),
      area_surveyed = sum(area_surveyed, na.rm = TRUE),
      count = sum(count, na.rm = TRUE),
      count_flying = sum(count_flying, na.rm = TRUE),
      count_swimming = count - count_flying,
      .groups = 'drop'
    ) %>%
    rename(
      datetime = round_time
    )
  #sf_aerial_1min <- st_as_sf(x = df_aerial_1min, coords = c("lon", "lat"), crs = 4326)
  
  #' select all other data but don't resample
  df_all_other <- dat %>% 
    st_drop_geometry()  %>%
    filter(
      # !(platform_type == 3 & data_provider == 55),
      # !key %in% d_int_summary$key[d_int_summary$p_below_1 > 0.8]
      !(campaign_key %in% too_long_aerial$campaign_key & 
          as.Date(datetime) %in% too_long_aerial$date &
          platform == "aerial"),
      !(campaign_key %in% too_long_ship$campaign_key &
          as.Date(datetime) %in% too_long_ship$date &
          platform == "ship")
    )
  #sf_all_other <- st_as_sf(x = df_all_other, coords = c("lon", "lat"), crs = 4326)
  
  # combine
  cat("combine again and add ESW estimates...\n")
  sf_combined <- bind_rows(
    df_all_other, df_aerial_1min, df_ship_5min) %>%
    select(-esw)
  
  # join with distance sampling output ####
  load(file.path(out_dir, "ddf_list.rdata"), verbose = TRUE)
  esw_ids <- which(d_dist$euring_species_code == species_list$euring[sp])
  esw_res <- ddf_list[esw_ids]
  esw_df <- d_dist %>%
    filter(
      euring_species_code == species_list$euring[sp]
    ) %>%
    mutate(
      platform = ifelse(platform_type==1,"ship", "aerial")
    )
  
  esw_df$esw <- unlist(lapply(esw_res, function(x) {
    if(!is.null(x$mod_sel$ESW[1])){
      out <- x$mod_sel$ESW[1]
    } else {
      out <- NA
    }
  }))
  esw_df <- bind_rows(
    esw_df, 
    data.frame(
      euring_species_code = rep(species_list$euring[sp], 2),
      platform = c("aerial", "ship"),
      distance_bins = rep("0|100", 2),
      esw = rep(100, 2)
    ))
  # some models did not fit for Puffin, very low sample size...
  if (sp==10) {
    esw_df$esw[3] <- 100
  }
  
  # group/fix distance_bins
  sf_combined$distance_bins[sf_combined$distance_bins=="0|50|100|200"] <- "0|50|100|200|300"
  sf_combined$distance_bins[sf_combined$distance_bins=="0|50|100"] <- "0|100"
  sf_combined$distance_bins[sf_combined$distance_bins=="44|91|163"] <- "44|91|163|432"
  
  sf_combined <- left_join(
    x = sf_combined,
    y = esw_df %>% select(platform, distance_bins, esw), 
    by = c('platform', 'distance_bins')
  )
  
  # add UTM coordinates
  st_combined_utm <- sf_combined %>%
    st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
    st_transform(crs = 32631)
  sf_combined <- sf_combined %>%
    mutate(
      x_utm = sf::st_coordinates(st_combined_utm)[,1],
      y_utm = sf::st_coordinates(st_combined_utm)[,2],
      TransectWidth = ifelse(is.na(TransectWidth), 1, TransectWidth), # dummy value
      n_sides = ifelse(
        TransectWidth == 600, 2, 1
      )
    )
  
  
  # calculate densities and aggregate left and right sides
  cat("calculate densities...\n")
  df_combined <- sf_combined %>%
    st_drop_geometry() %>%
    mutate(
      esa = km_travelled * (esw/1000) * n_sides, # km2
      dens_flying = ifelse(
        old_mwtl, 
        count_flying / area_surveyed,
        count_flying / esa), # n/km2,
      dens_swimming = ifelse(
        old_mwtl, 
        count_swimming / area_surveyed,
        count_swimming / esa), # n/km2
    ) %>% 
    group_by(
      campaign_key, old_mwtl, datetime, SamplingMethod, TargetTaxa, platform
    ) %>%
    summarise(
      fisheries_active = max(as.numeric(fisheries_active)),
      lon = mean(lon),
      lat = mean(lat),
      x_utm = mean(x_utm),
      y_utm = mean(y_utm),
      esa = ifelse(old_mwtl[1], sum(area_surveyed), sum(esa)),
      dens = sum(dens_flying) + sum(dens_swimming),
      .groups = 'drop'
    ) %>%
    mutate(
      euring = species_list$euring[sp] # add euring_code
    )
  
  #apply(df_combined, 2, function(x){sum(is.na(x))})
  # save ####
  cat("save results...\n\n")
  filename = paste0(species_list$label[sp], ".rds")
  saveRDS(df_combined, file = file.path(out_dir, "Local", filename))
  #loadRDS(file.path(out_dir, "Local", filename))
  
  print(Sys.time())
}



#' data preparations all species
#'
#' Rob van Bemmelen
#' January 2026
#' modified version after 3_data_prep.R, only focusing on MWTL data

# prepare data for spatial analyses ####

#' Data notes: 
#' ESAS data via Mardik
#' MWTL data via Job
#' - both have same structure/column names, but these differ from the more recent files at these links:
#'   https://esas.ices.dk/inventoryen 
#'   https://ices-tools-dev.github.io/esas/tables/
#' - combined files for ESAS and MWTL
#' - full join of effort and sightings data
#' - sf object
#' - CRS used are
#'  - EPSG: 32631 (UTM 31N)
#'  - EPSG: 4326 (lat/lon)
#' - some data has strip width of 600m and is thus double-sided
#' - other double-sided data (OWEZ/PAWP) is probably included with different positionids
#' - some aerial survey data has 1s intervals and needs to be resampled at longer intervals 
#' - other data is retained as original and needs to be aggregated to positionid level
#' - some weird positions (end points only of transects?) around the Brown Ridge with campaignid 120073977, with NAs in campaign_key... and where the data_provider (120) cannot be retrieved... Data from 2014-02-27 only. Same problems occur in raw data from esas.ices.dk repository.

#' Codes that we split in different species during MWTL monitoring, but that are combined here:

#' 59 --> unidentified divers are considered Red-throated Divers

#' 6345 --> zeekoet/alk,
#' 6549 --> Alcidae; zeekoet (6340), alk (6360), Papegaaiduiker (6540),
#' 5919 --> lesser black backed gull (5910) / herring gull (5920)

#' Split 6009 - Unidentified *Larus* gull in the most common gulls: Kleine mantelmeeuw (5910), Zilvermeeuw (5920), Pontische meeuw (5927), geelpootmeeuw (5926), Grote Mantelmeeuw (6000), kokmeeuw (5820) en stormmeeuw (5900)
#' Split 6049 unidentified gull in  meest voorkomende meeuwen Kleine mantelmeeuw (5910), Zilvermeeuw (5920), Pontische meeuw (5927), geelpootmeeuw (5926), Grote Mantelmeeuw (6000),kokmeeuw (5820) en stormmeeuw (5900), drieteenmeeuw (6020)
#' split 59 (divers) into 20 and 30

#' 1) Razorbill vs Common Guillemot. This is the most problematic species pair, as some survey methods don't allow identification of these to species level. As we are to report each species separately, such surveys need to be excluded from the analysis. This could be done by calculating the % of unidentified large auks; when this exceeds a certain threshold (a few percent), that method needs to be excluded.

# directories
dir_git <- dirname(rstudioapi::getSourceEditorContext()$path)
dir_dat <- file.path(dir_git, 'data')
dir_out <- file.path(dir_git, "output")

# libraries
library(dplyr)
library(sf)

# species list
load(
  file = file.path(
    dirname(rstudioapi::getSourceEditorContext()$path),
    "data",
    "species_list.rdata"
  ),
  verbose = TRUE
)

#' start for loop voor het laden van de data om zeker te weten dat er 
#' geen legacy versies van objecten zijn...

#' loop through species ####
for(sp in 1:nrow(d_sps)) {
  print(d_sps$name_uk[sp])
  
  # load MWTL data ####
  cat('load and prepare MWTL data...\n')
  
  #' old and new MLTL data combined in 1_data_load.r
  #' Notes: 
  #' - old MWTL data is via Mardik + Job in 2022
  #' - new MWTL data is via Job in 2026
  c_rds_files <- list.files(
    file.path(
      dirname(rstudioapi::getSourceEditorContext()$path), 
      "output",
      "dataset"
    )
  )
  d_mwtl <- readRDS(
    file.path(
      dirname(rstudioapi::getSourceEditorContext()$path), 
      "output",
      "dataset", 
      last(c_rds_files)
      )
  ) %>%
    filter(
      origin != "ESAS",
      platformclass == 3
    ) %>%
    rename(
      km_travelled = distance,
      number = count, # to avoid thing going awry in summarize, calling 'number' twice
      lon = longitude,
      lat = latitude
    ) %>%
    mutate(
      km_travelled = if_else(
        is.na(km_travelled) & origin == "MWTL_old",
        area/0.1,
        km_travelled
      )
    )
  
  ## effort ####  
  d_mwtl_eff <- d_mwtl %>%
    select(
      campaignid,
      origin,
      positionid,
      platformside,
      datetime,
      lon,
      lat,
      km_travelled,
      area,
      samplingmethod,
      distancebins,
      windforce) %>%
    distinct(
      campaignid,
      positionid,
      datetime,
      lon,
      lat,
      .keep_all = TRUE) %>% 
    mutate(
      distancebins = if_else(
        is.na(distancebins) & origin == "MWTL_old",
        c("0|100"),
        distancebins)
    )

  ## unidentified birds ####    
  ### unidentified gulls ####
  if (d_sps$euring[sp] %in% c(5910, 5920, 6000, 6020)) {
    # 5919: Herring or Lesser Black-backed Gull
    n_5919 <- d_mwtl %>%
      filter(
        speciescode == 5919
      )
    frac_5919 <- d_mwtl %>% 
      mutate(
        speciescode = if_else(speciescode %in% c(5911, 5912), 5910, speciescode), # LBBG ssp to LBBG
        speciescode = if_else(speciescode %in% c(5921), 5920, speciescode) # HG ssp to HG
      ) %>%
      filter(speciescode %in% c(5910, 5920)) %>% 
      group_by(campaignid, date, speciescode) %>% 
      summarise(sum_number = sum(number), .groups = "drop") %>% 
      tidyr::pivot_wider(
        names_from = speciescode, 
        values_from = sum_number
        ) %>% 
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
    n_6009 <- d_mwtl %>%
      filter(
        speciescode == 6009
      )
    frac_6009 <- d_mwtl %>% 
      mutate(
        speciescode = if_else(speciescode %in% c(5911, 5912), 5910, speciescode), # LBBG ssp to LBBG
        speciescode = if_else(speciescode %in% c(5921), 5920, speciescode) # HG ssp to HG
      ) %>%
      filter(speciescode %in% c(5820, 5900, 5910, 5920, 5927, 6000)) %>% 
      group_by(campaignid, date, speciescode) %>% 
      summarise(
        sum_number = sum(number),
        .groups = "drop") %>% 
      tidyr::pivot_wider(names_from = speciescode, values_from = sum_number) %>% 
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
    n_6049 <- d_mwtl %>% 
      filter(
        speciescode == 6049
      )
    frac_6049 <- d_mwtl %>% 
      mutate(
        speciescode = if_else(speciescode %in% c(5911, 5912), 5910, speciescode), # LBBG ssp to LBBG
        speciescode = if_else(speciescode %in% c(5921), 5920, speciescode) # HG ssp to HG
      ) %>%
      filter(speciescode %in% c(5910,5920,5927,6000,5820,5900,6020)) %>% 
      group_by(campaignid, date, speciescode) %>% 
      summarise(sum_number = sum(number), .groups = "drop") %>% 
      tidyr::pivot_wider(names_from = speciescode, values_from = sum_number ) %>% 
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
    data_5919 <- d_mwtl %>% 
      filter(speciescode == 5919) %>% 
      left_join(frac_5919, by = c("campaignid", "date")) %>% 
      mutate("5910" = round(number * fraction_5910), 
             "5920" = round(number * fraction_5920)
      ) %>% 
      dplyr::select(
        -speciescode, -number, 
        -fraction_5910, -fraction_5920
      ) %>% 
      tidyr::pivot_longer(
        cols = c("5910":"5920"), 
        names_to = "speciescode", values_to = "number") %>% 
      mutate(speciescode = as.numeric(speciescode))
    data_6009 <- d_mwtl %>% 
      filter(speciescode == 6009) %>% 
      left_join(frac_6009, by = c("campaignid", "date")) %>% 
      mutate("5910" = round(number * fraction_5910), 
             "5920" = round(number * fraction_5920),
             "5927" = round(number * fraction_5927),
             "6000" = round(number * fraction_6000),
             "5820" = round(number * fraction_5820),
             "5900" = round(number * fraction_5900)) %>% 
      dplyr::select(
        -speciescode, -number, 
        -fraction_5910,-fraction_5920,-fraction_5927,-fraction_6000,-fraction_5820,-fraction_5900
      ) %>%
      tidyr::pivot_longer(
        cols = c("5910":"5920","5927","6000","5820","5900"), 
        names_to = "speciescode", values_to = "number") %>% 
      mutate(
        speciescode = as.numeric(speciescode)
      )
    data_6049 <- d_mwtl %>% 
      filter(speciescode == 6049) %>% 
      left_join(frac_6049, by = c("campaignid", "date")) %>% 
      mutate("5910" = round(number * fraction_5910), 
             "5920" = round(number * fraction_5920),
             "5927" = round(number * fraction_5927),
             "6000" = round(number * fraction_6000),
             "5820" = round(number * fraction_5820),
             "5900" = round(number * fraction_5900),
             "6020" = round(number * fraction_6020)) %>% 
      dplyr::select(
        -speciescode, -number, 
        -fraction_5910,-fraction_5920,-fraction_5927,
        -fraction_6000,-fraction_5820,-fraction_5900,-fraction_6020
      ) %>% 
      tidyr::pivot_longer(
        cols = c("5910":"5920","5927","6000","5820","5900","6020"), 
        names_to = "speciescode", values_to = "number") %>% 
      mutate(speciescode = as.numeric(speciescode))
    
    # thus, data is in 
    # - data_6049 - relevant for HR, LBBG, GBBG, BLK
    # - data_6009 - relevant for HR, LBBG, GBBG
    # - data_5919 - relevant for HR and LBBG
    if (d_sps$euring[sp] %in% c(5910, 5920)) {
      d_mwtl <- bind_rows(d_mwtl, data_5919, data_6009, data_6049)
    }
    if (d_sps$euring[sp] == 6000) {
      d_mwtl <- bind_rows(d_mwtl, data_6009, data_6049)
    }
    if (d_sps$euring[sp] == 6020) {
      d_mwtl <- bind_rows(d_mwtl, data_6049)
    }
    
  }
  
  ### unidentified alcids ####
  if (d_sps$euring[sp] %in% c(6340, 6360)) {
    # 6345: Razorbill or Common Guillemot
    n_6345 <- d_mwtl %>%
      filter(
        speciescode == 6345
      )
    # fractions for both razorbills and guillemots
    frac_6345 <- d_mwtl %>% 
      filter(speciescode %in% c(6340, 6360)) %>% 
      group_by(campaignid, date, speciescode) %>% 
      summarise(sum_number = sum(number), .groups = "drop") %>% 
      tidyr::pivot_wider(names_from = speciescode, values_from = sum_number) %>% 
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
    data_6345 <- d_mwtl %>% 
      filter(speciescode == 6345) %>% 
      left_join(frac_6345, by = c("campaignid", "date")) %>% 
      mutate("6340" = round(number * fraction_6340), 
             "6360" = round(number * fraction_6360)
      ) %>% 
      dplyr::select(
        -speciescode, -number, 
        -fraction_6340,-fraction_6360
      ) %>% 
      tidyr::pivot_longer(
        cols = c("6340":"6360"), 
        names_to = "speciescode", values_to = "number") %>% 
      mutate(speciescode = as.numeric(speciescode))
    
    d_mwtl <- bind_rows(d_mwtl, data_6345)
  }
  
  ### unidentified 'commic' terns ####
  #' here we use all Common and Arctic Terns
  if (d_sps$euring[sp] == 6169) {
    d_mwtl <- d_mwtl %>%
      mutate(
        speciescode = if_else(speciescode %in% c(6150, 6160), 6169, speciescode)
      )
  }
  
  ## collect observations ####  
  d_mwtl_obs <- d_mwtl %>%
    filter(
      speciescode == d_sps$euring[sp],
      transect == "True"
    ) %>%
    group_by(positionid) %>%
    summarise(
      count = sum(number, na.rm = TRUE)#,
#      count_flying = 0,
#      count_swimming = sum(number, na.rm = TRUE)
    )
  
  ## ship-associations / fishing activity nearby ####
  d_mwtl_ships <- d_mwtl %>%
    filter(
      association == 26
    ) %>%
    distinct(
      positionid
    ) %>%
    mutate(
      fisheries_active = TRUE
    )
  
  #' If there are no POTENTIALLY associating birds present (e.g. fulmars, gannets, gulls), 
  #' then it is impossible to determine if there is a fishery activity present from this data.
  #' Therefore, fisheries_active should be set to NA when records have no sightings of such birds
  d_mwtl_asso_birds <- d_mwtl %>% # NUMBER of POTENTIALLY associating birds per positionid
    filter(
      speciescode >= 220, # from Northern Fulmar (excluding divers/grebes)
      !speciescode %in% 1010:5659, # excluding anatidae, waders, etc
      speciescode < 6540 # before Atlantic Puffin (excluding auks, doves, passerines, etc)
    ) %>%
    group_by(
      positionid
    ) %>%
    summarise(
      n_ass_birds = sum(number, na.rm = TRUE)
    )
  
  # join 'fisheries_active' and 'mwtl_asso_birds' to effort df
  d_mwtl_eff <- left_join(
    x = d_mwtl_eff,
    y = d_mwtl_ships,
    by = "positionid"
  ) %>%
    mutate(
      fisheries_active = if_else(is.na(fisheries_active), FALSE, TRUE),
      fisheries_active = if_else(
        positionid %in% d_mwtl_asso_birds$positionid, 
        fisheries_active, NA) 
    )
  
  # combine ####
  # cat('combine ESAS and MWTL data...\n')
  # eff <- bind_rows(
  #   esas_eff,
  #   mwtl_eff
  # )
  # obs <- bind_rows(
  #   esas_obs,
  #   mwtl_obs
  # )
  d_eff <- d_mwtl_eff
  d_obs <- d_mwtl_obs

  # check some effort stuff
  d_com <- left_join(
    x = d_eff,
    y = d_obs,
    by = "positionid"
  ) %>%
  mutate(
    count = if_else(is.na(count), 0, count)#,
    #count_flying = if_else(is.na(count_flying), 0, count_flying),
    #count_swimming = if_else(is.na(count_swimming), 0, count_swimming)
  )
  #table(duplicated(dat$campaign_key, dat$datetime))
  
  # sampling intervals ####
  #' more than 60 indicates more than 1 position per minute
  n_per_hour <- d_eff %>%
    mutate(
      date = as.Date(datetime),
      hour = as.POSIXlt(datetime)$hour
    ) %>%
    group_by(
      campaignid,
      origin,
      date,
      hour
    ) %>%
    summarise(
      n = n(),
      n_lon = length(unique(lon)),
      n_dt = length(unique(datetime)),
      .groups = "drop"
    )
  
  too_many <- n_per_hour %>%
    filter(n > 60 & hour != 0)
  #' most old MWTL-hours have 0-30 positions per hour; some have 30-60 positions per hour, 
  #' a very small part have > 60 positions. However, this is (partly?) due to imprecise 
  #' datetime data: only date, but no time

  # join with distance sampling output ####
  load(
    file = file.path(
      dirname(rstudioapi::getSourceEditorContext()$path),
      "output",
      "distance",
      "ddf_list.rdata"), 
    verbose = TRUE)
  
  esw_ids <- which(d_dist$speciescode == d_sps$euring[sp])
  esw_res <- ddf_list[esw_ids]
  esw_df <- d_dist %>%
    filter(
      speciescode == d_sps$euring[sp]
    )
  
  esw_df$esw <- unlist(lapply(esw_res, function(x) {
    if(!is.null(x$mod_sel$ESW[1])){
      out <- x$mod_sel$ESW[1]
    } else {
      out <- NA
    }
  }))
  esw_df <- bind_rows(
    esw_df %>% 
      mutate(
        origin = "MWTL_new"
      ), 
    # add this for old MWTL
    data.frame(
      speciescode = d_sps$euring[sp],
      origin = "MWTL_old",
      distancebins = "0|100",
      esw = 100
    ))
  # some models did not fit for Puffin, very low sample size...
  # if (sp==10) {
  #   esw_df$esw[3] <- 100
  # }
  
  d_com <- left_join(
    x = d_com,
    y = esw_df %>% 
      select(origin, distancebins, esw), 
    by = c('origin', 'distancebins')
  )
  
  # add UTM coordinates
  d_com <- d_com %>%
    mutate(
      longitude = lon,
      latitude = lat
    ) %>% 
    st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>%
    st_transform(crs = 32631) %>%
    mutate(
      x_utm = st_coordinates(.)[,1],
      y_utm = st_coordinates(.)[,2]
    ) %>%
    st_drop_geometry()
  
  # calculate densities and aggregate left and right sides
  cat("calculate densities...\n")
  d_com <- d_com %>%
    mutate(
      esa = if_else(
        origin == "MWTL_new", 
        km_travelled * (esw/1000), # note: all is one-sided!
        area
        ), # km2
      log_esa = log(esa),
      dens = count / esa,
      euring = d_sps$euring[sp] # add euring_code
    )
  
  #apply(d_com, 2, function(x){sum(is.na(x))})

  # save ####
  cat("save results...\n\n")
  saveRDS(
    d_com, 
    file = file.path(
      dirname(rstudioapi::getSourceEditorContext()$path),
      "output",
      "local", 
      paste0(d_sps$label[sp], ".rds"))
    )

  # remove ####
  rm(
    d_com,
    d_eff, d_obs
  )
  rm(list = ls(envir = .GlobalEnv, pattern = "^d_mwtl_"),
     envir = .GlobalEnv)
  
  print(Sys.time())
}

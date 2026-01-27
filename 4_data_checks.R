
#' double check data for all species
#'
#' Rob van Bemmelen
#' January 2026

#' libraries
library(dplyr)
library(sf)
library(ggplot2)
library(patchwork)

#' functions
plot_effort <- function(d_plot) {
  ggplot(
    d_plot,
    aes(
      x = x_utm,
      y = y_utm
    )
  ) + 
    geom_point(size = .1) + 
    facet_grid(rows = vars(year), cols = vars(season)) + 
    coord_equal() + 
    labs(
      x = "",
      y = ""
    ) + 
    theme_bw() + 
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank())
}
plot_sightings <- function(d_plot, title) {
  ggplot(
    d_plot,
    aes(
      x = x_utm,
      y = y_utm
    )
  ) + 
    geom_point(size = .1) + 
    geom_point(
      d_plot %>% 
        filter(
          count >0
        ),
      mapping = aes(
        x = x_utm,
        y = y_utm,
        size = count
      ),
      shape = 1,
      col = "red"
    ) + 
    facet_grid(rows = vars(year), cols = vars(season)) + 
    coord_equal() + 
    labs(
      x = "",
      y = "",
      title = title
    ) + 
    theme_bw() + 
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank())
}



#' load species list
load(
  file = file.path(
    dirname(rstudioapi::getSourceEditorContext()$path),
    "data",
    "species_list.rdata"
  ),
  verbose = TRUE
)

#' load Red-throated Diver for general checks
d <- readRDS(
  file = file.path(
    dirname(rstudioapi::getSourceEditorContext()$path),
    "output",
    "local", 
    paste0("Red-throated_Diver", ".rds"))
) %>%
  mutate(
    year = as.numeric(substr(campaignid, 1, 4)),
    season = ifelse(
      origin == "MWTL_old", 
      unlist(lapply(strsplit(campaignid, split = "_"), last)),
      unlist(lapply(strsplit(campaignid, split = "-"), last))
    )
  )

#' check: dimensions and missing values
dim(d)
apply(d, 2, function(x){sum(is.na(x))}) # missing values in platformside (for old MWTL), windforce (for old MWTL), fisheries_active (for records where no fishery-associating species were observed)
length(unique(d$campaignid)) # 196

#' check: distribution of effort
p_eff_old_1 <- plot_effort(
  d %>% 
    filter(
      origin == "MWTL_old",
      year %in% 1991:1997
    )
)
p_eff_old_2 <- plot_effort(
  d %>% 
    filter(
      origin == "MWTL_old",
      year %in% 1997:2002
    ))
p_eff_old_3 <- plot_effort(
  d %>% 
    filter(
      origin == "MWTL_old",
      year %in% 2003:2008
    ))
p_eff_old_4 <- plot_effort(
  d %>% 
    filter(
      origin == "MWTL_old",
      year %in% 2009:2014
    ))
p_eff_new_1 <- plot_effort(
  d %>% 
    filter(
      origin == "MWTL_new",
      year %in% 2014:2019
    )
)
p_eff_new_2 <- plot_effort(
  d %>% 
    filter(
      origin == "MWTL_new",
      year %in% 2020:2025
    )
)

#' check: distribution of effectively surveyed area
p_hist_eff <- ggplot(
  d,
  aes(x = esa)
) + 
  geom_histogram(bins = 50) + 
  facet_wrap(~origin, ncol = 1) + 
  theme_bw()

#' check: sample sizes per species (group)
l_ss <- list()

#' loop through species
for(sp in 4:nrow(d_sps)) {
  print(d_sps$name_uk[sp])
  
  #' load original data
  d <- readRDS(
    file = file.path(
      dirname(rstudioapi::getSourceEditorContext()$path),
      "output",
      "local", 
      paste0(d_sps$label[sp], ".rds"))
  ) %>%
    mutate(
      year = as.numeric(substr(campaignid, 1, 4)),
      season = ifelse(
        origin == "MWTL_old", 
        unlist(lapply(strsplit(campaignid, split = "_"), last)),
        unlist(lapply(strsplit(campaignid, split = "-"), last))
      )
    )
  
  l_ss[[sp]] <- d %>%
    group_by(
      origin
    ) %>%
    summarise(
      p_zero = sum(count == 0)/n(), # overall percentage of zeros
      n_sig = sum(count > 0), # positions with sightings
      n_ind = sum(count), # total number of individuals
      n_mean = mean(count), # mean number
      n_max = max(count), # max number per poskey
      n_95 = quantile(count, probs = 0.95), # upper 95% quantile
      d_mean = mean(dens), # mean density
      d_median = median(dens), # median density
      d_max = max(dens)
    ) %>%
    mutate(
      species = d_sps$name_uk[sp]
    )

  #' check: raw distribution maps
  p_sig_old_1 <- plot_sightings(
    d %>% 
      filter(
        origin == "MWTL_old",
        year %in% 1991:1996
      ),
    title = d_sps$name_uk[sp]
  )
  p_sig_old_2 <- plot_sightings(
    d %>% 
      filter(
        origin == "MWTL_old",
        year %in% 1997:2002
      ),
    title = d_sps$name_uk[sp]
    )
  p_sig_old_3 <- plot_sightings(
    d %>% 
      filter(
        origin == "MWTL_old",
        year %in% 2003:2008
      ),
    title = d_sps$name_uk[sp]
    )
  p_sig_old_4 <- plot_sightings(
    d %>% 
      filter(
        origin == "MWTL_old",
        year %in% 2009:2014
      ),
    title = d_sps$name_uk[sp]
    )
  p_sig_new_1 <- plot_sightings(
    d %>% 
      filter(
        origin == "MWTL_new",
        year %in% 2014:2019
      ),
    title = d_sps$name_uk[sp]
  )
  p_sig_new_2 <- plot_sightings(
    d %>% 
      filter(
        origin == "MWTL_new",
        year %in% 2020:2025
      ),
    title = d_sps$name_uk[sp]
  )

  #' check: distribution of numbers per positionid
  p_hist_sig <- ggplot(
    d %>%
      filter(
        count > 0
      ),
    aes(
      x = count
    )
  ) + 
    geom_histogram(breaks = seq(0.5, max(d$count) + 0.5, 1)) + 
    facet_wrap(~origin, ncol = 1) + 
    theme_bw()

  # save figs
  dir_fig <- file.path(
    dirname(rstudioapi::getSourceEditorContext()$path),
    "output",
    "figures",
    d_sps$label[sp]
  )
  if(!dir.exists(dir_fig)) {
    dir.create(dir_fig)
  }
  png(
    filename = file.path(
      dir_fig, 
      paste0(d_sps$label[sp], "_hist_sightings.png")),
    res = 300, width = 6.5, height = 3, units = "in"
  )
  print(p_hist_sig)
  dev.off()
  
  png(
    filename = file.path(
      dir_fig, 
      paste0(d_sps$label[sp], "_map_sig_1991-1996.png")),
    res = 300, width = 6.5, height = 8, units = "in"
  )
  print(p_sig_old_1)
  dev.off()
  
  png(
    filename = file.path(
      dir_fig, 
      paste0(d_sps$label[sp], "_map_sig_1997-2002.png")),
    res = 300, width = 6.5, height = 8, units = "in"
  )
  print(p_sig_old_2)
  dev.off()
  
  png(
    filename = file.path(
      dir_fig, 
      paste0(d_sps$label[sp], "_map_sig_2003-2008.png")),
    res = 300, width = 6.5, height = 8, units = "in"
  )
  print(p_sig_old_3)
  dev.off()
  
  png(
    filename = file.path(
      dir_fig, 
      paste0(d_sps$label[sp], "_map_sig_2009-2014.png")),
    res = 300, width = 6.5, height = 8, units = "in"
  )
  print(p_sig_old_4)
  dev.off()
  
  png(
    filename = file.path(
      dir_fig, 
      paste0(d_sps$label[sp], "_map_sig_2015-2019.png")),
    res = 300, width = 6.5, height = 8, units = "in"
  )
  print(p_sig_new_1)
  dev.off()
  
  png(
    filename = file.path(
      dir_fig, 
      paste0(d_sps$label[sp], "_map_sig_2020-2025.png")),
    res = 300, width = 6.5, height = 8, units = "in"
  )
  print(p_sig_new_2)
  dev.off()
  
  rm(d, dir_fig) # for the next species...
}

# collect sample size info
d_ss <- do.call('rbind', l_ss)
                
# plot sample size shizl
p_ss_pzero <- ggplot(
  d_ss,
  aes(
    x = species,
    y = p_zero
  )
) + 
  geom_point() + 
  facet_wrap(~origin, ncol = 1) + 
  labs(
    x = "",
    y = "overall fraction zero counts"
  ) + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p_ss_nsig <- ggplot(
  d_ss,
  aes(
    x = species,
    y = n_sig
  )
) + 
  geom_point() + 
  facet_wrap(~origin, ncol = 1) + 
  labs(
    x = "",
    y = "n segments with sightings"
  ) + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p_ss_nind <- ggplot(
  d_ss,
  aes(
    x = species,
    y = n_ind
  )
) + 
  geom_point() + 
  facet_wrap(~origin, ncol = 1) + 
  labs(
    x = "",
    y = "n individuals"
  ) + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dir_fig <- file.path(
  dirname(rstudioapi::getSourceEditorContext()$path),
  "output",
  "figures"
)


png(
  filename = file.path(
    dir_fig, 
    paste0("p_ss_pzero.png")),
  res = 300, width = 6.5, height = 4, units = "in"
)
print(p_ss_pzero)
dev.off()

png(
  filename = file.path(
    dir_fig, 
    paste0("p_ss_nsig.png")),
  res = 300, width = 6.5, height = 4, units = "in"
)
print(p_ss_nsig)
dev.off()

png(
  filename = file.path(
    dir_fig, 
    paste0("p_ss_nind.png")),
  res = 300, width = 6.5, height = 4, units = "in"
)
print(p_ss_nind)
dev.off()
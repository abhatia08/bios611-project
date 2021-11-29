# SETUP ----

## 1. Load packages ----
library(dplyr)
library(here)
library(ggExtra)
library(maps)
library(ggplot2)
library(patchwork)
library(viridis)
library(cowplot)

## 2. Run util.R ----
source(here::here("scripts", "util.R"))

ensure_directory(here::here("figures"))

## 3. Import analytic data ---- 
analytic_df <-
  readr::read_csv(here::here("derived_data", "analytic_data.csv"))

## 4. Prep base map layer and plotting df ----

analytic_df <-
  dplyr::rename (analytic_df, c("state_abbr" = "state"))

## County level data (for completeness/future-proof)
counties_df <-
  sf::st_as_sf(maps::map("county", plot = FALSE, fill = TRUE),
               crs = 4326) %>%
  mutate(ID = as.character(ID)) %>%
  left_join(maps::county.fips, c(ID = "polyname"))

## Fips is int here, and I have a preference for leading zeros being included
## convert to char and add zeros for the 285 counties that need it
counties_df <- counties_df %>%
  mutate(fips = as.character(fips))

counties_df$fips[1:285] <- paste0('0', counties_df$fips[1:285])

## Create a df with geometry and data

plotting_df <- left_join(counties_df, analytic_df, "fips")
plotting_df <-
  plotting_df %>% separate(ID, c('state', 'county'), sep = ',')

plotting_df <- plotting_df %>%
  dplyr::rename ("abbrev" = "state_abbr", "name" = "state")


## Appending state abbreviations
plotting_df$state_name <- stringr::str_to_title(plotting_df$name)

plotting_df$abbrev <- state.abb[match(plotting_df$state_name, state.name)]

## DC does not match automatically
plotting_df$abbrev[plotting_df$name == "district of columbia"] <-
  "DC"

## Drop duplicate state name
plotting_df <- plotting_df[-c(5)]

## Drop geometry to use usmap
plotting_df <- plotting_df %>% sf::st_drop_geometry()

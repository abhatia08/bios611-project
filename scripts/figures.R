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

## County level data (for completeness/future-proof)

data(fips_codes)
fips_codes$fips <-
  paste0(fips_codes$state_code, fips_codes$county_code)

plotting_df <-
  left_join(fips_codes,
            analytic_df,
            by = c("fips" = "fips"))

plotting_df <- plotting_df[c("state.x",
                         "state_name.x",
                         "county.x",
                         "fips",
                         "p65older",
                         "p_nonwhite",
                         "p_poverty",
                         "p_business_permil",
                         "api_index",
                         "gini_coef"
                         )]



plotting_df <- plotting_df %>%
  dplyr::rename ("abbrev" = "state.x", "name" = "state_name.x", "county" = "county.x")


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



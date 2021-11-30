# SETUP ----

## 1. Load required packages ----
library(plyr)
library(jsonlite)
library(data.table)
library(tidyverse)
library(tidygeocoder)
library(usmap)
library(here)
library(fs)
library(janitor)
library(tidycensus)
library(labelled)

## 2. Declare `here`
here::i_am("scripts/05_clean_merged.R")

## 2. Run Util.R ----
source(here::here("scripts", "util.R"))

## 3. Create necessary directories ----
ensure_directory(here::here("derived_data"))

# A. IMPORT DATA ----

## 1. Yelp Data ----
yelp_df <-
  read_csv(here::here("derived_data", "yelp_tidy.csv"))

## 2. Population (n) Data ----
pop_df <-
  read_csv(here::here("derived_data", "population_by_age.csv"))

## 3. Non-white Population Data ----
nonwhite_df <-
  read_csv(here::here("derived_data", "percent_nonwhite_pop.csv"))

## 4. AHRF Data ----
ahrf_df <-
  read_csv(here::here("derived_data", "ahrf_subset.csv"))

## 5. API Data ----
api_df <-
  read_csv(here::here("derived_data", "api.csv"))

# B. JOIN DATA ----
## The plotting df will be joined using the API data first, such that all the counties are present with complete data other than Yelp data (plotting_df)

## 1. Setting up the base df ----

plotting_df <- api_df

## 2. Population data ----

pop_wide <- pop_df %>%
  tidyr::spread(age, pop, sep = "")

plotting_df <- plotting_df %>% dplyr::left_join(pop_wide)

plotting_df <- plotting_df %>%
  dplyr::mutate(
    n_pop_2018 = age0 + age5 + age10 + age15 + age20 +
      age25 + age30 + age35 + age40 + age45 + age50 +
      age55 + age60 + age65 + age70 + age75 + age80 +
      age85
  ) %>% dplyr::mutate(p65older = ((age65 + age70 + age75 + age80 +
                                     age85) * 100 / n_pop_2018))

## 3. Non-white population data ----

plotting_df <- plotting_df %>%
  dplyr::left_join(nonwhite_df)

## 4. AHRF data ----

plotting_df <- plotting_df %>%
  dplyr::left_join(ahrf_df)

## 5. Yelp data ----

plotting_df <- plotting_df %>%
  dplyr::left_join(yelp_df) %>%
  dplyr::mutate(p_business_permil = ((n_business * 1000000) / n_pop_2018))

# C. FINAL DATASET WRANGLING ----

## 1 Fill County-level NAs ----
## (for completeness/future-proof)

data(fips_codes)
fips_codes$fips <-
  paste0(fips_codes$state_code, fips_codes$county_code)

plotting_df <-
  left_join(fips_codes,
            plotting_df,
            by = c("fips" = "fips"))

##2 Subset and rename vars ----
plotting_df <- plotting_df[c(
  "state.x",
  "state_name.x",
  "county.x",
  "fips",
  "api_index",
  "p65older",
  "p_nonwhite",
  "p_poverty",
  "p_business_permil",
  "n_pop_2018"
)]

plotting_df <- plotting_df %>%
  dplyr::rename ("abbrev" = "state.x",
                 "name" = "state_name.x",
                 "county" = "county.x")


# D. WRITE DATASET TO DIRECTORY ----

readr::write_csv(plotting_df,
                 here::here("derived_data", "plotting_data.csv"))


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
here::i_am("scripts/cleanup.R")

## 2. Run Util.R ----
source(here::here("scripts", "util.R"))

## 3. Create necessary directories ----
ensure_directory(here::here("derived_data"))

# A. IMPORT DATA ----

## 1. Yelp Data ----
yelp_tidy <-
  read_csv(here::here("derived_data", "yelp_geocoded.csv"))

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
## NOTE: Treating the Yelp Dataset as the primary dataset gives us 30 counties with complete data (analytic_df)
## Alternatively, the plotting df will be joined using the API data first, such that all the counties are present with complete data other than Yelp data (plotting_df)

## 1. Analytic data (30 counties) ----

### 1.1. Setting up the base df ----

analytic_df <- yelp_tidy

### 1.2. Population data ----

pop_wide <- pop_df %>%
  tidyr::spread(age, pop, sep = "")

analytic_df <- analytic_df %>% dplyr::left_join(pop_wide)


analytic_df <- analytic_df %>%
  dplyr::mutate(
    n_pop_2018 = age0 + age5 + age10 + age15 + age20 +
      age25 + age30 + age35 + age40 + age45 + age50 +
      age55 + age60 + age65 + age70 + age75 + age80 +
      age85
  ) %>% dplyr::mutate(p65older = ((age65 + age70 + age75 + age80 +
                                     age85) * 100 / n_pop_2018))

### 1.3. Non-white population data ----

analytic_df <- analytic_df %>%
  dplyr::left_join(nonwhite_df)

### 1.4. AHRF data ----

analytic_df <- analytic_df %>%
  dplyr::left_join(ahrf_subset) %>%
  dplyr::mutate(p_business_permil = ((n_business * 1000000) / n_pop_2018))

### 1.5. API data ----

analytic_df <- analytic_df %>%
  dplyr::left_join(api_df)


## 2. Plotting Data (All counties) ----

### 2.1. Setting up the base df ----

plotting_df <- api_df

### 2.2. Population data ----

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

### 2.3. Non-white population data ----

plotting_df <- plotting_df %>%
  dplyr::left_join(nonwhite_df)

### 2.4. AHRF data ----

plotting_df <- plotting_df %>%
  dplyr::left_join(ahrf_subset)

### 2.5. GINI coef ----

plotting_df <- plotting_df %>%
  dplyr::left_join(gini_df)


### 2.6. Yelp data ----

plotting_df <- plotting_df %>%
  dplyr::left_join(yelp_tidy) %>%
  dplyr::mutate(p_business_permil = ((n_business * 1000000) / n_pop_2018))

# G. FINAL DATASET WRANGLING ----

## 1. Analytic DF ----
### 1.1 Subset ----
analytic_df <- analytic_df %>% select(
  -c(
    name,
    age0,
    age5,
    age10,
    age15,
    age20,
    age25,
    age30,
    age35,
    age40,
    age45,
    age50,
    age55,
    age60,
    age65,
    age70,
    age75,
    age80,
    age85,
    p_white,
  )
)

## 2. Plotting DF ----
#### 1.1 Fill County-level NAs ----
#### (for completeness/future-proof)

data(fips_codes)
fips_codes$fips <-
  paste0(fips_codes$state_code, fips_codes$county_code)

plotting_df <-
  left_join(fips_codes,
            plotting_df,
            by = c("fips" = "fips"))

#### 1.2 Subset and rename vars ----
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
  "gini_coef",
  "n_pop_2018"
)]

plotting_df <- plotting_df %>%
  dplyr::rename ("abbrev" = "state.x",
                 "name" = "state_name.x",
                 "county" = "county.x")


# H. WRITE DATASETS TO DIRECTORY ----

readr::write_csv(analytic_df,
                 here::here("derived_data", "analytic_data.csv"))


readr::write_csv(plotting_df,
                 here::here("derived_data", "plotting_data.csv"))


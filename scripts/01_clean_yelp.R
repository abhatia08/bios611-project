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

## 2. Declare `here`
here::i_am("scripts/cleanup.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. Create necessary directories ----
ensure_directory(here::here("derived_data"))

# 01. YELP DATA ----


## If data has already been geocoded, load the geocoded file. ----
## Note: Geocoding is a resource intensive process, and a geocoded dataset was provided to allow repeated runs

if (file.exists(here::here("source_data", "yelp_geocoded.csv"))) {
  yelp_geocoded <-
    read_csv(here::here("source_data", "yelp_geocoded.csv"),
             lazy = FALSE)
} else {
  ## 1. Import data as tibble ----
  
  yelp_data <- as_tibble(do.call(rbind, lapply(readLines(
    here::here("source_data",
               "yelp_academic_dataset_business.json")
  ),
  fromJSON)))
  
  ## 2. Subset the data (exclude unnecessary variables) ----
  yelp_data <-
    yelp_data[c(
      "business_id",
      "name",
      "address",
      "city",
      "state",
      "postal_code",
      "latitude",
      "longitude",
      "categories"
    )]
  
  ## 3. Fix variable types ----
  
  yelp_data <- yelp_data %>%
    mutate(business_id = as.character(business_id)) %>%
    mutate(name = as.character(name)) %>%
    mutate(address = as.character(address)) %>%
    mutate(city = as.character(city)) %>%
    mutate(state = as.character(state)) %>%
    mutate(postal_code = as.character(postal_code)) %>%
    mutate(categories = as.character(categories)) %>%
    mutate(latitude = as.numeric(latitude)) %>%
    mutate(longitude = as.numeric(longitude))
  
  
  ## 4. Including only related categories, and removing all BC sites ----
  yelp_data <-
    yelp_data %>%
    filter (
      str_detect(
        categories,
        "Outdoor Gear|Bicycles|Bike Shop|Bikes|Hunting & Fishing Supplies|Military Surplus|Ski & Snowboard Shops|Sporting Goods"
      )
    ) %>% filter(str_detect(state, "BC", negate = TRUE))
  
  
  ## 5. Reverse Geocoding ----
  ## Note: This is process takes time
  yelp_geocoded <- yelp_data %>%
    reverse_geocode(
      lat = latitude,
      long = longitude,
      method = 'osm',
      address = address_found,
      full_results = TRUE
    )
  
  
  yelp_geocoded <-
    yelp_geocoded %>% select(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 22, 24))
  yelp_geocoded <- yelp_geocoded[c(
    "business_id",
    "name",
    "address",
    "city...4",
    "county",
    "postcode",
    "state...5",
    "latitude",
    "longitude",
    "categories"
  )]
  colnames(yelp_geocoded)[4] <- ("city")
  colnames(yelp_geocoded)[7] <- ("state")
  
  ## 6. Write geocoded data to directory ----
  write_csv(yelp_geocoded,
            here::here("derived_data", "yelp_geocoded.csv"))
  
}


## 7. County Level Business Aggregation ----
yelp_tidy <-
  yelp_geocoded %>% group_by(state, county) %>% tally() %>% dplyr::rename(n_business = n)

## 8. Import and join in FIPS codes ----
data(fips_codes)
fips_codes$fips <-
  paste0(fips_codes$state_code, fips_codes$county_code)

yelp_tidy <-
  left_join(yelp_tidy,
            fips_codes,
            by = c("state" = "state", "county" = "county"))

yelp_tidy <- yelp_tidy[c("state",
                         "state_name",
                         "county",
                         "fips",
                         "n_business")]


## 9. Write tidy data to directory ----
write_csv(yelp_tidy,
          here::here("derived_data", "yelp_tidy.csv"))

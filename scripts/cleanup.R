# Load required packages ----
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
options(tigris_use_cache = TRUE)

# A. YELP DATA ----
#
# # 1. Import data as tibble
#
# yelp_data <- as_tibble(do.call(rbind, lapply(
#   readLines(
#     "source_data/yelp_academic_dataset_business.json"
#   ),
#   fromJSON
# )))
#
# # 2. Subset the data (exclude unnecessary variables)
# yelp_tidy <-
#   yelp_data[c(
#     "business_id",
#     "name",
#     "address",
#     "city",
#     "state",
#     "postal_code",
#     "latitude",
#     "longitude",
#     "categories"
#   )]
#
# # 3. Fix variable types
#
# yelp_tidy <- yelp_tidy %>%
#   mutate(business_id = as.character(business_id)) %>%
#   mutate(name = as.character(name)) %>%
#   mutate(address = as.character(address)) %>%
#   mutate(city = as.character(city)) %>%
#   mutate(state = as.character(state)) %>%
#   mutate(postal_code = as.character(postal_code)) %>%
#   mutate(categories = as.character(categories)) %>%
#   mutate(latitude = as.numeric(latitude)) %>%
#   mutate(longitude = as.numeric(longitude))
#
#
# # 4. Subsetting out places that have related categories in it, and removing all BC sites
# yelp_tidy <-
#   yelp_tidy %>%
#   filter (
#     str_detect(
#       categories,
#       "Outdoor Gear|Bicycles|Bike Shop|Bikes|Hunting & Fishing Supplies|Military Surplus|Ski & Snowboard Shops|Sporting Goods"
#     )
#   ) %>% filter(str_detect(state, "BC", negate = TRUE))
#
#
# # 5. REVERSE GEOCODING-- only do once
# yelp_tidy <- yelp_tidy %>%
#   reverse_geocode(lat = latitude,
#     long = longitude,
#     method = 'osm',
#     address = address_found,
#     full_results = TRUE
#   )
#
# yelp_tidy <- yelp_coded %>% select(c(1,2,3,4,5,6,7,8,9,10,22,24))
# yelp_tidy <- yelp_tidy[c(
#   "business_id",
#   "name",
#   "address",
#   "city...4",
#   "county",
#   "postcode",
#   "state...5",
#   "latitude",
#   "longitude",
#   "categories"
#   )]
# colnames(yelp_tidy)[4]<-("city")
# colnames(yelp_tidy)[7]<-("state")
#
#
# write_csv(yelp_tidy, "source_data/yelp_geocoded.csv")

## TEMPORARILY IMPORT YELP DATA ####
### Import tidied Yelp file
yelp_geocoded <-
  read_csv(here("source_data", "yelp_geocoded.csv"), lazy = FALSE)

### County Level Business Aggregation
yelp_tidy <-
  yelp_geocoded %>% group_by(state, county) %>% tally() %>% dplyr::rename(n_business = n)

### Import and join in FIPS codes
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

### Write data to directory
write_csv(yelp_tidy,
          here::here("derived_data", "yelp_tidy.csv"))

# 2. POPULATION DATA ----
## Note that the code to clean and reshape this data is based on mkiang's repo:
## https://github.com/mkiang/county_preparedness/blob/master/code/01_get_population_data.R

## Download and reshape the 2018 NCHS bridged race population file.

utils::download.file(url = "https://www.cdc.gov/nchs/nvss/bridged_race/pcen_v2018_y18.txt.zip",
                     destfile = here::here(
                       data_path,
                       "source_data",
                       basename(
                         "https://www.cdc.gov/nchs/nvss/bridged_race/pcen_v2018_y18.txt.zip"
                       )
                     ))


orig_pop_df <-
  readr::read_fwf(here::here(
    data_path,
    "source_data",
    basename(
      "https://www.cdc.gov/nchs/nvss/bridged_race/pcen_v2018_y18.txt.zip"
    )
  ),
  readr::fwf_widths(
    c(4, 4, 1, 5, 2, 1, 1, 8),
    c(
      "series",
      "year",
      "month",
      "fips",
      "age",
      "racesex",
      "hispanic",
      "pop"
    )
  ))  %>%
  dplyr::filter(year == 2018, month == 7)

pop_df <- orig_pop_df %>%
  dplyr::select(age, fips, pop) %>%
  dplyr::mutate(age = (cut(
    age,
    c(0, seq(5, 85, 5), Inf),
    include.lowest = TRUE,
    right = FALSE,
    labels = FALSE
  ) - 1) * 5) %>%
  dplyr::group_by(fips, age) %>%
  dplyr::summarize(pop = sum(pop)) %>%
  dplyr::ungroup()


readr::write_csv(pop_df,
                 here::here("derived_data", "population_by_age.csv"))

## Get percentage of non-NHW population ----
nhw_df <- orig_pop_df %>%
  dplyr::select(age, fips, pop, racesex, hispanic) %>%
  dplyr::filter(racesex %in% 1:2, hispanic == 1) %>%
  dplyr::mutate(age = (cut(
    age,
    c(0, seq(5, 85, 5), Inf),
    include.lowest = TRUE,
    right = FALSE,
    labels = FALSE
  ) - 1) * 5) %>%
  dplyr::group_by(fips, age) %>%
  dplyr::summarize(nhw_pop = sum(pop)) %>%
  dplyr::ungroup()

non_white_perc <- pop_df %>%
  dplyr::left_join(nhw_df) %>%
  dplyr::group_by(fips) %>%
  dplyr::summarize(pop = sum(pop),
                   nhw_pop = sum(nhw_pop)) %>%
  dplyr::transmute(
    fips = fips,
    p_nonwhite = (pop - nhw_pop) / pop * 100,
    p_white = nhw_pop / pop * 100
  )

readr::write_csv(non_white_perc,
                 here::here("derived_data", "percent_nonwhite_pop.csv"))

#### 3. AHRF Data ####
## Clean up AHRF files and extract the subset of columns we need.
##
## Note that this code is almost entirely from jjchern's great ahrf repo:
##  https://github.com/mkiang/ahrf/blob/master/data-raw/prep_county.R

## Imports ----

RAW_SRC <- here::here(data_path,
                      "source_data",
                      "AHRF_2018-2019",
                      "DATA",
                      "AHRF2019.asc")
DOC_SRC <- here::here(
  data_path,
  "source_data",
  "AHRF_2018-2019",
  "DOC",
  "AHRF 2018-2019 Technical Documentation.xlsx"
)

## Check if file is unzipped ----
if (!fs::file_exists(RAW_SRC)) {
  utils::unzip(here::here("source_data", "AHRF_2018-2019.zip"),
               exdir = here::here("source_data"))
}

## Get the FWF layout ----
bgn_line <- readxl::read_excel(DOC_SRC) %>%
  dplyr::pull(`...1`) %>%
  grepl("F00001", .) %>%
  which()

## Import the excel as a df of FWF info ----
ahrf_county_layout <- readxl::read_excel(
  DOC_SRC,
  col_names = c(
    "field",
    "col_col",
    "year_of_data",
    "var_label",
    "characteristics",
    "source",
    "date_on"
  ),
  skip = bgn_line
) %>%
  dplyr::filter(grepl("^F[0-9]", field)) %>%
  tidyr::separate(col_col, c("col_start", "col_end")) %>%
  dplyr::mutate_at(c("col_start", "col_end"), as.integer)

## Import the county-level AHRF file ----
ahrf_county <- readr::read_fwf(
  file = RAW_SRC,
  col_positions = readr::fwf_positions(
    start = ahrf_county_layout$col_start,
    end = ahrf_county_layout$col_end,
    col_names = ahrf_county_layout$field
  )
)

labelled::var_label(ahrf_county) <- ahrf_county_layout %>%
  dplyr::select(field, var_label) %>%
  tibble::deframe() %>%
  as.list()

## Extract scaling factor ----
ahrf_county_layout <- ahrf_county_layout %>%
  dplyr::mutate(scaling_factor = stringr::str_extract(characteristics, "\\(.[0-1]{1,2}\\)")) %>%
  dplyr::mutate(scaling_factor = as.numeric(gsub("\\(|\\)", "", scaling_factor)))

## Rescale columns ----
for (s in unique(ahrf_county_layout$scaling_factor)) {
  if (!is.na(s)) {
    ahrf_county <- ahrf_county %>%
      dplyr::mutate_at(dplyr::vars(
        ahrf_county_layout %>%
          dplyr::filter(scaling_factor == s) %>%
          dplyr::pull(field)
      ),
      function(x)
        as.numeric(x) * s)
  }
}

## Subset data to keep what we need
ahrf_list <- c(
  "fips_st" = "F00011",
  "fips_ct" = "F00012",
  "name" = "F00010",
  "n_people_below_poverty_level_2017" =  "F14419-13",
  "n_pop_2017" = "F13182-17"
)

ahrf_subset <- ahrf_county %>%
  dplyr::select(ahrf_list) %>%
  dplyr::mutate(fips = paste0(fips_st, fips_ct)) %>%
  dplyr::group_by(fips, name) %>%
  dplyr::mutate_at(dplyr::vars(-dplyr::group_cols()), function(x)
    as.numeric(x)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(p_poverty_level_2017 = (n_people_below_poverty_level_2017 /
                                          n_pop_2017)) %>%
  dplyr::select(-fips_st,
                -fips_ct,
                -n_people_below_poverty_level_2017,
                -n_pop_2017) %>%
  dplyr::select(fips,
                name,
                dplyr::everything())



## Save ----
readr::write_csv(ahrf_subset,
                 here::here("derived_data", "ahrf_subset.csv"))
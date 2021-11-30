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

# A. YELP DATA ----


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

# B. POPULATION DATA ----
## Note that the code to clean and reshape this data is based on mkiang's repo:
## https://github.com/mkiang/county_preparedness/blob/master/code/01_get_population_data.R

## 1. Download the data ----

utils::download.file(url = "https://www.cdc.gov/nchs/nvss/bridged_race/pcen_v2018_y18.txt.zip",
                     destfile = here::here(
                       "source_data",
                       basename(
                         "https://www.cdc.gov/nchs/nvss/bridged_race/pcen_v2018_y18.txt.zip"
                       )
                     ))

## 2. Reshape the 2018 NCHS bridged race population file ----
orig_pop_df <-
  readr::read_fwf(here::here(
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

## 3. Write population data to directory ----
readr::write_csv(pop_df,
                 here::here("derived_data", "population_by_age.csv"))

## 4. Get percentage of non-NHW population ----
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

nonwhite_df <- pop_df %>%
  dplyr::left_join(nhw_df) %>%
  dplyr::group_by(fips) %>%
  dplyr::summarize(pop = sum(pop),
                   nhw_pop = sum(nhw_pop)) %>%
  dplyr::transmute(
    fips = fips,
    p_nonwhite = (pop - nhw_pop) / pop * 100,
    p_white = nhw_pop / pop * 100
  )
## 5. Write data to directory ----

readr::write_csv(nonwhite_df,
                 here::here("derived_data",
                            "percent_nonwhite_pop.csv"))

# C. AHRF DATA  ----
## Note that this code is almost entirely from jjchern's ahrf repo:
##  https://github.com/mkiang/ahrf/blob/master/data-raw/prep_county.R

## 1. Import sources ----

RAW_SRC <- here::here("source_data",
                      "AHRF_2018-2019",
                      "DATA",
                      "AHRF2019.asc")
DOC_SRC <- here::here(
  "source_data",
  "AHRF_2018-2019",
  "DOC",
  "AHRF 2018-2019 Technical Documentation.xlsx"
)

## 2. Check if file is unzipped ----
if (!fs::file_exists(RAW_SRC)) {
  utils::unzip(here::here("source_data", "AHRF_2018-2019.zip"),
               exdir = here::here("source_data"))
}



## 3. Get the FWF layout ----
bgn_line <- readxl::read_excel(DOC_SRC) %>%
  dplyr::pull(`...1`) %>%
  grepl("F00001", .) %>%
  which()

## 4. Import the excel as a df of FWF info ----
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

## 5. Import the county-level AHRF file ----
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

## 6. Extract scaling factor ----
ahrf_county_layout <- ahrf_county_layout %>%
  dplyr::mutate(scaling_factor = stringr::str_extract(characteristics, "\\(.[0-1]{1,2}\\)")) %>%
  dplyr::mutate(scaling_factor = as.numeric(gsub("\\(|\\)", "", scaling_factor)))

## 7. Rescale columns ----
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

## 8. Subset data to keep relevant variables ----
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
  dplyr::mutate(p_poverty = ((100 * n_people_below_poverty_level_2017) /
                               n_pop_2017)) %>%
  dplyr::select(-fips_st,
                -fips_ct,
                -n_people_below_poverty_level_2017,
                -n_pop_2017) %>%
  dplyr::select(fips,
                name,
                dplyr::everything())



## 9. Write data to directory ----
readr::write_csv(ahrf_subset,
                 here::here("derived_data", "ahrf_subset.csv"))

# D. ACS GINI COEFFICIENT DATA ----

## 1. Import data ----

gini_df <- readr::read_csv(
  here::here(
    "source_data",
    "ACSDT5Y2018.B19083_2020-03-20T172559",
    "ACSDT5Y2018.B19083_data_with_overlays_2020-03-20T172555.csv"
  )
)

## 2. Clean to only keep fips and numeric coefficient----
gini_df <- gini_df %>%
  dplyr::slice(-1) %>%
  dplyr::select(geoid = GEO_ID,
                gini_coef = B19083_001E) %>%
  dplyr::mutate(fips = substr(geoid, nchar(geoid) - 4, nchar(geoid))) %>%
  dplyr::select(-geoid) %>%
  dplyr::filter(gini_coef != "null",
                fips != "000US") %>%
  dplyr::mutate(gini_coef = as.numeric(gini_coef))

## 3. Write data to directory ----

readr::write_csv(gini_df,
                 here::here("derived_data",
                            "gini.csv"))



# E. CDC ACCESS TO PARKS (API) DATA ----
## 1. Download and read in the data ----

utils::download.file(url = "https://ephtracking.cdc.gov:443/apigateway/api/v1/getCoreHolder/428/2/all/all/2015/0/0",
                     destfile = here::here(
                       "source_data",
                       basename(
                         "https://ephtracking.cdc.gov:443/apigateway/api/v1/getCoreHolder/428/2/all/all/2015/0/0"
                       )
                     ))


file.rename(here::here("source_data",
                       "0"),
            here::here("source_data",
                       "api_raw.json"))


api_df <- jsonlite::read_json(here::here("source_data",
                                         "api_raw.json"))

## 2. Unnest, restructure and subset the data ----

api_df <- as.data.frame(do.call(cbind, api_df))

api_df <- api_df$tableResult

api_df <- as.data.frame(do.call(rbind, api_df))

api_df <-
  api_df[c("geoId",
           "dataValue")]

api_df <-
  api_df %>%
  mutate(dataValue = as.numeric(dataValue)) %>%
  mutate(geoId = as.character(geoId)) %>%
  dplyr::rename(fips = geoId) %>%
  dplyr::rename(api_index = dataValue)

## 3. Write data to directory ----
readr::write_csv(api_df,
                 here::here("derived_data", "api.csv"))


# F. JOIN DATA ----
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

### 1.5. GINI coef ----

analytic_df <- analytic_df %>%
  dplyr::left_join(gini_df)


### 1.6. API data ----

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


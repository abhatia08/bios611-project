# SETUP ----

## 1. Load required packages ----
library(plyr)
library(data.table)
library(tidyverse)
library(here)
library(fs)
library(janitor)
library(labelled)

## 2. Declare `here`
here::i_am("scripts/cleanup.R")

## 3. Run Util.R ----
source(here::here("scripts", "util.R"))

## 4. Create necessary directories ----
ensure_directory(here::here("derived_data"))

# 04. CDC ACCESS TO PARKS (API) DATA ----
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


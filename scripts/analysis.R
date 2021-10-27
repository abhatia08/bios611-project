## Reshaping and combining derived data

# Load required packages ----
library(tidyverse)
library(here)

# Import Datasets
yelp_df <-
  readr::read_csv(here::here("derived_data", "yelp_tidy.csv"))
pop_df <-
  readr::read_csv(here::here("derived_data", "population_by_age.csv"))


nonwhite_df <- readr::read_csv(here::here("derived_data",
                                          "percent_nonwhite_pop.csv"))

ahrf_df <-
  read_csv(here::here("derived_data", "ahrf_subset.csv"))

## Reshape population ----

pop_wide <- pop_df %>%
  tidyr::spread(age, pop, sep = "")

data_wide <- yelp_df %>% dplyr::left_join(pop_wide)


data_wide <- data_wide %>%
  dplyr::mutate(
    n_pop_2018 = age0 + age5 + age10 + age15 + age20 +
      age25 + age30 + age35 + age40 + age45 + age50 +
      age55 + age60 + age65 + age70 + age75 + age80 +
      age85
  ) %>% dplyr::mutate(p65older = ((age65 + age70 + age75 + age80 +
                                     age85) * 100 / n_pop_2018))
## Non-white population (percentage) ----
data_wide <- data_wide %>%
  dplyr::left_join(nonwhite_df)

## Join with AHRF subset variables ----
data_wide <- data_wide %>%
  dplyr::left_join(ahrf_df) %>%
  dplyr::mutate(p_business_pop10000 = ((n_business * 10000) / n_pop_2018))

# Subset the data ----
analytic_df <- data_wide %>% select(
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
    age85
  )
)

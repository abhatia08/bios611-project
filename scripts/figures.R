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


# A. BIVARIATE PLOTS (Main) ----

## Outdoor recreation covariates we are going to use:
##  1. Number of retail businesses per million people people
##  2. API index

## And how they intersect with socio-demographic covariates
##  1. Percent over 70
##  2. Percent in poverty
##  3. GINI coefficent
##  4. Percent non-white
##
## Which will result 8 plots.
##  Figure 1
##  1. Number of retail businesses per million people people (x) vs Percent over 65 (y)
##  2. Number of retail businesses per million people people (x) vs Percent in poverty (y)
##  3. Number of retail businesses per million people people (x) vs GINI coefficent (y)
##  4. Number of retail businesses per million people people (x) vs Percent non-white (y)
##
##  Figure 2
##  1. API index (x) vs Percent over 65 (y)
##  2. API index (x) vs Percent in poverty (y)
##  3. API index (x) vs GINI coefficent (y)
##  4. API index (x) vs Percent non-white (y)
##
## Thresholds will be defined by using (rounded) IQR as medium range.

## 1. Percent over 65 vs percent in poverty ----
p1 <- mega_plot_bivariate(
  plotting_df = plotting_df,
  return_data = TRUE,
  x_var = p_business_permil,
  x_high = return_rounded_iqr(plotting_df$p_business_permil)[2],
  x_low = return_rounded_iqr(plotting_df$p_business_permil)[1],
  x_label = "Number of outdoor\nrecreation businesses\n(per 1 million people)",
  y_var = p65older,
  y_high = return_rounded_iqr(plotting_df$p65older)[2],
  y_low = return_rounded_iqr(plotting_df$p65older)[1],
  y_label = "Population over 65\nyears of age (%)"
)

## 2. Percent non-white vs Percent in poverty ----
p2 <- mega_plot_bivariate(
  plotting_df = plotting_df,
  return_data = TRUE,
  x_var = p_business_permil,
  x_high = return_rounded_iqr(plotting_df$p_business_permil)[2],
  x_low = return_rounded_iqr(plotting_df$p_business_permil)[1],
  x_label = "Number of outdoor\nrecreation businesses\n(per 1 million people)",
  y_var = p_poverty,
  y_high = return_rounded_iqr(plotting_df$p_poverty)[2],
  y_low = return_rounded_iqr(plotting_df$p_poverty)[1],
  y_label = "Households under\npoverty-line (%)"
)

## 3. Percent non-white vs percent over 65 ----
p3 <- mega_plot_bivariate(
  plotting_df = plotting_df,
  return_data = TRUE,
  x_var = p_business_permil,
  x_high = return_rounded_iqr(plotting_df$p_business_permil)[2],
  x_low = return_rounded_iqr(plotting_df$p_business_permil)[1],
  x_label = "Number of outdoor\nrecreation businesses\n(per 1 million people)",
  y_var = gini_coef,
  y_high = return_rounded_iqr(plotting_df$gini_coef)[2],
  y_low = return_rounded_iqr(plotting_df$gini_coef)[1],
  y_label = "Gini coefficient"
)

p4 <- mega_plot_bivariate(
  plotting_df = plotting_df,
  return_data = TRUE,
  x_var = p_business_permil,
  x_high = return_rounded_iqr(plotting_df$p_business_permil)[2],
  x_low = return_rounded_iqr(plotting_df$p_business_permil)[1],
  x_label = "Number of outdoor\nrecreation businesses\n(per 1 million people)",
  y_var = p_nonwhite,
  y_high = return_rounded_iqr(plotting_df$p_nonwhite)[2],
  y_low = return_rounded_iqr(plotting_df$p_nonwhite)[1],
  y_label = "Population non-white (%)"
)


## Save ----
## Figure 1 all ----
p1_all <- cowplot::plot_grid(
  p1$legend_only,
  p1$map_only,
  p2$legend_only,
  p2$map_only,
  p3$legend_only,
  p3$map_only,
  p4$legend_only,
  p4$map_only,
  ncol = 2,
  rel_widths = c(.5, 1),
  labels = c("A", "", "B", "", "C", "", "D"),
  scale = rep(c(.78, 1), 3)
)
ggplot2::ggsave(
  here::here("figures", "fig01_covariates.pdf"),
  p1_all,
  device = grDevices::cairo_pdf,
  width = 6,
  height = 8,
  scale = 1.2
)
ggplot2::ggsave(
  here::here("figures", "fig01_covariates.jpg"),
  p1_all,
  dpi = 300,
  width = 6,
  height = 8,
  scale = 1.2
)


## Save all the plots as individual plots ----
ggplot2::ggsave(
  here::here("figures", "fig01-1.pdf"),
  p1$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "fig01-1.jpg"),
  p1$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)

ggplot2::ggsave(
  here::here("figures", "fig01-2.pdf"),
  p2$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "fig01-2.jpg"),
  p2$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)

ggplot2::ggsave(
  here::here("figures", "fig01-3.pdf"),
  p3$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "fig01-3.jpg"),
  p3$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "fig01-4.pdf"),
  p3$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "fig01-4.jpg"),
  p3$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)


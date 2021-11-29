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
  readr::read_csv(here::here("derived_data", "yelp_tidy.csv"))

## 4. Prep base map layer and plotting df ----

analytic_df <-
  dplyr::rename (analytic_df, c("state_abbr" = "state"))
analytic_df$fips <- as.numeric(analytic_df$fips)

## County level geometry
counties_df <-
  sf::st_as_sf(maps::map("county", plot = FALSE, fill = TRUE),
               crs = 4326) %>%
  mutate(ID = as.character(ID)) %>%
  left_join(maps::county.fips, c(ID = "polyname"))

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



# A. UNIVARIATE PLOTS ----

# # Shiny Notes
# 
#     "US county-level variation in characteristics influencing participation in outdoor recreation"
#   ),
#   theme = shinytheme("slate"),
#   selectInput(
#     "characteristic",
#     label = "Characteristic",
#     choices = c(
#       "% population over 65" = "p65older",
#       "% households under the poverty line" = "p_poverty_level_2017",
#       "% population non-Hispanic and non-White" = "p_nonwhite",
#       "number of outdoor recreation retail businesses per 10000 individuals" = "p_business_pop10000",
#       "population in 2018" = "n_pop_2018"

## 1. p_65_older ----


p1 <- plot_counties(plotting_df, "p65older") +
  ggplot2::scale_fill_viridis_c(
    "Percent of population over 65",
    trans = "log1p",
    direction = -1,
    guide = ggplot2::guide_colorbar(
      title.position = "top",
      barheight = ggplot2::unit(.5, "cm"),
      barwidth = ggplot2::unit(12.5, "cm")
    )
  )  +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
  here::here("figures", "figS01_p_65yo.pdf"),
  p1,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 6,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "figS01_p_65yo.jpg"),
  p1,
  dpi = 300,
  width = 7,
  height = 6,
  scale = 1.1
)


## 2. Percentage of households living in poverty, 2016 ----
p1 <- plot_counties(plotting_df, "p_poverty") +
  ggplot2::scale_fill_viridis_c(
    "Percent of population below poverty level",
    trans = "log1p",
    direction = -1,
    guide = ggplot2::guide_colorbar(
      title.position = "top",
      barheight = ggplot2::unit(.5, "cm"),
      barwidth = ggplot2::unit(12.5, "cm")
    )
  )  +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
  here::here("figures", "figS02_poverty.pdf"),
  p1,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 6,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "figS02_poverty.jpg"),
  p1,
  dpi = 300,
  width = 7,
  height = 6,
  scale = 1.1
)

## 3. Percentage of population non-Hispanic and non-White ----
p1 <- plot_counties(plotting_df, "p_nonwhite") +
  ggplot2::scale_fill_viridis_c(
    "Percent of population non-Hispanic and non-White",
    direction = -1,
    # trans = "log1p",
    guide = ggplot2::guide_colorbar(
      title.position = "top",
      barheight = ggplot2::unit(.5, "cm"),
      barwidth = ggplot2::unit(12.5, "cm")
    )
  )  +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
  here::here("figures", "figS04_p_nonwhite_nonhispanic.pdf"),
  p1,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 6,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "figS04_p_nonwhite_nonhispanic.jpg"),
  p1,
  dpi = 300,
  width = 7,
  height = 6,
  scale = 1.1
)

## 4. Population (log 10), 2018 ----
p1 <- plot_counties(plotting_df, "n_pop_2018") +
  ggplot2::scale_fill_viridis_c(
    "Population (log10)",
    trans = "log10",
    direction = 1,
    guide = ggplot2::guide_colorbar(
      title.position = "top",
      barheight = ggplot2::unit(.5, "cm"),
      barwidth = ggplot2::unit(12.5, "cm")
    ),
    breaks = 10 ^ (2:7),
    labels = c("100", "1,000", "10,000", "100,000", "1,000,000", "10,000,000")
  )  +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
  here::here("figures", "figS17_population.pdf"),
  p1,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 6,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "figS17_population.jpg"),
  p1,
  dpi = 300,
  width = 7,
  height = 6,
  scale = 1.1
)

# B. MATT'S BIVARIATE CODE ----
## 05_primary_figures.R ----
##
## Create the primary figures of the paper

## Imports ----


## Covariates we are going to use:
##  1. Percent over 70
##  2. Percent in poverty
##  3. Percent in group quarters
##
##  + 4. how racial/ethnic composition intersects with all of them.
##
## Which will result 6 plots.
##  Figure 1
##  1. Percent over 70 (x) vs percent in poverty (y)
##  2. Percent in poverty (x) vs percent in group quarters (y)
##  3. Percent over 70 (x) vs percent in group quarters (y)
##
##  Figure 2
##  4. Percent non-white (x) vs percent over 70 (y)
##  5. Percent non-white (x) vs percent in poverty (y)
##  6. Percent non-white (x) vs percent in group quarters (y)
##
## Thresholds will be defined by using (rounded) IQR as medium range.

## 1. Percent over 70 vs percent in poverty ----
p1 <- mega_plot_bivariate(
  plotting_df = plotting_df,
  return_data = TRUE,
  x_var = p65older,
  x_high = return_rounded_iqr(plotting_df$p65older)[2],
  x_low = return_rounded_iqr(plotting_df$p65older)[1],
  x_label = "Population over 65\nyears of age (%)",
  y_var = p_poverty_level_2017,
  y_high = return_rounded_iqr(plotting_df$p_poverty_level_2017)[2],
  y_low = return_rounded_iqr(plotting_df$p_poverty_level_2017)[1],
  y_label = "Households under\npoverty-line (%)"
)


mega_plot_bivariate(
  plotting_df = plotting_df,
  return_data = TRUE,
  x_var = p65older,
  x_high = return_rounded_iqr(plotting_df$p65older)[2],
  x_low = return_rounded_iqr(plotting_df$p65older)[1],
  x_label = "Population over 65\nyears of age (%)",
  y_var = p_poverty_level_2017,
  y_high = return_rounded_iqr(plotting_df$p_poverty_level_2017)[2],
  y_low = return_rounded_iqr(plotting_df$p_poverty_level_2017)[1],
  y_label = "Households under\npoverty-line (%)"
)



## 2. Percent in poverty vs percent in group quarters ----
p2 <- mega_plot_bivariate(
  plotting_df = plotting_df,
  return_data = TRUE,
  x_var = p_poverty,
  x_high = return_rounded_iqr(plotting_df$p_poverty)[2],
  x_low = return_rounded_iqr(plotting_df$p_poverty)[1],
  x_label = "Households under\npoverty-line (%)",
  y_var = p_group_quarters,
  y_high = return_rounded_iqr(plotting_df$p_group_quarters)[2],
  y_low = return_rounded_iqr(plotting_df$p_group_quarters)[1],
  y_label = "Population living\nin group quarters (%)",
)

## 3. Percent over 70 vs percent in group quarters ----
p3 <- mega_plot_bivariate(
  plotting_df = plotting_df,
  return_data = TRUE,
  x_var = p70older,
  x_high = return_rounded_iqr(plotting_df$p70older)[2],
  x_low = return_rounded_iqr(plotting_df$p70older)[1],
  x_label = "Population over 70\nyears of age (%)",
  y_var = p_group_quarters,
  y_high = return_rounded_iqr(plotting_df$p_group_quarters)[2],
  y_low = return_rounded_iqr(plotting_df$p_group_quarters)[1],
  y_label = "Population living\nin group quarters (%)",
)

## 4. Percent non-white vs percent over 70 ----
p4 <- mega_plot_bivariate(
  plotting_df = plotting_df,
  return_data = TRUE,
  x_var = p_nonwhite,
  x_high = return_rounded_iqr(plotting_df$p_nonwhite)[2],
  x_low = return_rounded_iqr(plotting_df$p_nonwhite)[1],
  x_label = "Population non-white (%)",
  y_var = p70older,
  y_high = return_rounded_iqr(plotting_df$p70older)[2],
  y_low = return_rounded_iqr(plotting_df$p70older)[1],
  y_label = "Population over 70\nyears of age (%)"
)

## 5. Percent non-white vs percent in poverty ----
p5 <- mega_plot_bivariate(
  plotting_df = plotting_df,
  return_data = TRUE,
  x_var = p_nonwhite,
  x_high = return_rounded_iqr(plotting_df$p_nonwhite)[2],
  x_low = return_rounded_iqr(plotting_df$p_nonwhite)[1],
  x_label = "Population non-white (%)",
  y_var = p_poverty,
  y_high = return_rounded_iqr(plotting_df$p_poverty)[2],
  y_low = return_rounded_iqr(plotting_df$p_poverty)[1],
  y_label = "Households under\npoverty-line (%)"
)

## 6. Percent non-white vs percent in group quarters ----
p6 <- mega_plot_bivariate(
  plotting_df = plotting_df,
  return_data = TRUE,
  x_var = p_nonwhite,
  x_high = return_rounded_iqr(plotting_df$p_nonwhite)[2],
  x_low = return_rounded_iqr(plotting_df$p_nonwhite)[1],
  x_label = "Population non-white (%)",
  y_var = p_group_quarters,
  y_high = return_rounded_iqr(plotting_df$p_group_quarters)[2],
  y_low = return_rounded_iqr(plotting_df$p_group_quarters)[1],
  y_label = "Population living\nin group quarters (%)",
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
  ncol = 2,
  rel_widths = c(.5, 1),
  labels = c("A", "", "B", "", "C"),
  scale = rep(c(.78, 1), 3)
)
ggplot2::ggsave(
  here::here("plots", "fig01_covariates.pdf"),
  p1_all,
  device = grDevices::cairo_pdf,
  width = 6,
  height = 8,
  scale = 1.2
)
ggplot2::ggsave(
  here::here("plots", "fig01_covariates.jpg"),
  p1_all,
  dpi = 300,
  width = 6,
  height = 8,
  scale = 1.2
)

## Figure 1 all ----
p2_all <- cowplot::plot_grid(
  p4$legend_only,
  p4$map_only,
  p5$legend_only,
  p5$map_only,
  p6$legend_only,
  p6$map_only,
  ncol = 2,
  rel_widths = c(.5, 1),
  labels = c("A", "", "B", "", "C"),
  scale = rep(c(.78, 1), 3)
)
ggplot2::ggsave(
  here::here("plots", "fig02_raceethnicity.pdf"),
  p2_all,
  device = grDevices::cairo_pdf,
  width = 6,
  height = 8,
  scale = 1.2
)
ggplot2::ggsave(
  here::here("plots", "fig02_raceethnicity.jpg"),
  p2_all,
  dpi = 300,
  width = 6,
  height = 8,
  scale = 1.2
)

## Save all the plots as individual plots ----
ggplot2::ggsave(
  here::here("plots", "fig01-1.pdf"),
  p1$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("plots", "fig01-1.jpg"),
  p1$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)

ggplot2::ggsave(
  here::here("plots", "fig01-2.pdf"),
  p2$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("plots", "fig01-2.jpg"),
  p2$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)

ggplot2::ggsave(
  here::here("plots", "fig01-3.pdf"),
  p3$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("plots", "fig01-3.jpg"),
  p3$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)

ggplot2::ggsave(
  here::here("plots", "fig02-1.pdf"),
  p4$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("plots", "fig02-1.jpg"),
  p4$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)

ggplot2::ggsave(
  here::here("plots", "fig02-2.pdf"),
  p5$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("plots", "fig02-2.jpg"),
  p5$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)

ggplot2::ggsave(
  here::here("plots", "fig02-3.pdf"),
  p6$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("plots", "fig02-3.jpg"),
  p6$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)

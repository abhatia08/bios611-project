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
ensure_directory(here::here("figures", "bivariate"))
ensure_directory(here::here("figures", "univariate"))

## 3. Import plotting data ----
plotting_df <-
  readr::read_csv(here::here("derived_data", "plotting_data.csv"))


# 07. BIVARIATE PLOTS (API INDEX) ----

## Outdoor recreation covariates we are going to use:
##  2. API index

## And how they intersect with socio-demographic covariates
##  1. Percent over 70
##  2. Percent in poverty
##  3. Percent non-white
##
## Which will result 3 plots.
##  Figure 2
##  1. API index (x) vs Percent over 65 (y)
##  2. API index (x) vs Percent in poverty (y)
##  3. API index (x) vs Percent non-white (y)
##
## Thresholds will be defined by using (rounded) IQR as medium range.

## 2. FIGURE 2 (API DATA) ----
### 2.1. vs Percent over 65 (y) ----
p4 <- mega_plot_bivariate(
  plotting_df = plotting_df,
  return_data = TRUE,
  x_var = api_index,
  x_high = return_rounded_iqr(plotting_df$api_index)[2],
  x_low = return_rounded_iqr(plotting_df$api_index)[1],
  x_label = "API Score",
  y_var = p65older,
  y_high = return_rounded_iqr(plotting_df$p65older)[2],
  y_low = return_rounded_iqr(plotting_df$p65older)[1],
  y_label = "Population over 65\nyears of age (%)"
)

### 2.2. vs Percent in poverty (y) ----
p5 <- mega_plot_bivariate(
  plotting_df = plotting_df,
  return_data = TRUE,
  x_var = api_index,
  x_high = return_rounded_iqr(plotting_df$api_index)[2],
  x_low = return_rounded_iqr(plotting_df$api_index)[1],
  x_label = "API Score",
  y_var = p_poverty,
  y_high = return_rounded_iqr(plotting_df$p_poverty)[2],
  y_low = return_rounded_iqr(plotting_df$p_poverty)[1],
  y_label = "Households under\npoverty-line (%)"
)


### 2.3. vs Percent non-white (y) ----
### NOTE: Uses the gini version of labels and plot function
p6 <- mega_plot_bivariate(
  plotting_df = plotting_df,
  return_data = TRUE,
  x_var = api_index,
  x_high = return_rounded_iqr(plotting_df$api_index)[2],
  x_low = return_rounded_iqr(plotting_df$api_index)[1],
  x_label = "API Score",
  y_var = p_nonwhite,
  y_high = return_rounded_iqr(plotting_df$p_nonwhite)[2],
  y_low = return_rounded_iqr(plotting_df$p_nonwhite)[1],
  y_label = "Population non-white (%)"
)



## SAVE ALL PLOTS ----

### 3. Figures 2 ----
ggplot2::ggsave(
  here::here("figures", "bivariate", "figB04.pdf"),
  p4$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "bivariate", "figB04.png"),
  p4$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)

ggplot2::ggsave(
  here::here("figures", "bivariate", "figB05.pdf"),
  p5$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "bivariate", "figB05.png"),
  p5$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)

ggplot2::ggsave(
  here::here("figures", "bivariate", "figB06.pdf"),
  p6$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "bivariate", "figB06.png"),
  p6$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)



### 4. Figure 2 stitched ----
p2_all <- cowplot::plot_grid(
  p4$legend_only,
  p4$map_only,
  p5$legend_only,
  p5$map_only,
  p6$legend_only,
  p6$map_only,
  ncol = 2,
  rel_widths = c(.5, 1),
  labels = c("A", "", "B", "", "C", "", "D"),
  scale = rep(c(.78, 1), 3)
)
ggplot2::ggsave(
  here::here("figures", "bivariate", "fig_bivariate_api.pdf"),
  p2_all,
  device = grDevices::cairo_pdf,
  width = 6,
  height = 8,
  scale = 1.2
)
ggplot2::ggsave(
  here::here("figures", "bivariate", "fig_bivariate_api.png"),
  p2_all,
  dpi = 300,
  width = 6,
  height = 8,
  scale = 1.2
)


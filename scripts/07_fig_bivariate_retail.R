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


# 07. BIVARIATE PLOTS (RETAIL) ----

## Outdoor recreation covariates we are going to use:
##  1. Outdoor recreation retail stores (per 1 million people)

## And how they intersect with socio-demographic covariates
##  1. Percent over 70
##  2. Percent in poverty
##  3. Percent non-white
##
## Which will result 3 plots.
##  Figure 1
##  1. Outdoor recreation retail stores (per 1 million people) (x) vs Percent over 65 (y)
##  2. Outdoor recreation retail stores (per 1 million people (x) vs Percent in poverty (y)
##  3. Outdoor recreation retail stores (per 1 million people) (x) vs Percent non-white (y)
##
## Thresholds will be defined by using (rounded) IQR as medium range.

## 1. FIGURE 1 (Businesses) ----
### 1.1. vs Percent over 65 (y) ----
p1 <- mega_plot_bivariate(
  plotting_df = plotting_df,
  return_data = TRUE,
  x_var = p_business_permil,
  x_high = return_rounded_iqr(plotting_df$p_business_permil)[2],
  x_low = return_rounded_iqr(plotting_df$p_business_permil)[1],
  x_label = "Outdoor recreation retail stores\n(per 1 million people)",
  y_var = p65older,
  y_high = return_rounded_iqr(plotting_df$p65older)[2],
  y_low = return_rounded_iqr(plotting_df$p65older)[1],
  y_label = "Population over 65\nyears of age (%)"
)

### 1.2. vs Percent in poverty (y) ----
p2 <- mega_plot_bivariate(
  plotting_df = plotting_df,
  return_data = TRUE,
  x_var = p_business_permil,
  x_high = return_rounded_iqr(plotting_df$p_business_permil)[2],
  x_low = return_rounded_iqr(plotting_df$p_business_permil)[1],
  x_label = "Outdoor recreation retail stores\n(per 1 million people)",
  y_var = p_poverty,
  y_high = return_rounded_iqr(plotting_df$p_poverty)[2],
  y_low = return_rounded_iqr(plotting_df$p_poverty)[1],
  y_label = "Households under\npoverty-line (%)"
)

### 1.3. vs Percent non-white (y) ----
p3 <- mega_plot_bivariate(
  plotting_df = plotting_df,
  return_data = TRUE,
  x_var = p_business_permil,
  x_high = return_rounded_iqr(plotting_df$p_business_permil)[2],
  x_low = return_rounded_iqr(plotting_df$p_business_permil)[1],
  x_label = "Outdoor recreation retail stores\n(per 1 million people)",
  y_var = p_nonwhite,
  y_high = return_rounded_iqr(plotting_df$p_nonwhite)[2],
  y_low = return_rounded_iqr(plotting_df$p_nonwhite)[1],
  y_label = "Population non-white (%)"
)

## SAVE ALL PLOTS ----
### 1. Figures 1 ----
ggplot2::ggsave(
  here::here("figures", "bivariate", "figB01.pdf"),
  p1$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "bivariate", "figB01.jpg"),
  p1$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)

ggplot2::ggsave(
  here::here("figures", "bivariate", "figB02.pdf"),
  p2$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "bivariate", "figB02.jpg"),
  p2$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)

ggplot2::ggsave(
  here::here("figures", "bivariate", "figB03.pdf"),
  p3$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "bivariate", "figB03.jpg"),
  p3$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)

### 2. Figure 1 stitched ----
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
  here::here("figures", "bivariate", "fig_bivariate_retail.pdf"),
  p1_all,
  device = grDevices::cairo_pdf,
  width = 6,
  height = 8,
  scale = 1.2
)
ggplot2::ggsave(
  here::here("figures", "bivariate", "fig_bivariate_retail.jpg"),
  p1_all,
  dpi = 300,
  width = 6,
  height = 8,
  scale = 1.2
)

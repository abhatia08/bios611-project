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


# A. BIVARIATE PLOTS (Main) ----

## Outdoor recreation covariates we are going to use:
##  1. Outdoor recreation retail stores (per 1 million people)
##  2. API index

## And how they intersect with socio-demographic covariates
##  1. Percent over 70
##  2. Percent in poverty
##  3. GINI coefficent
##  4. Percent non-white
##
## Which will result 8 plots.
##  Figure 1
##  1. Outdoor recreation retail stores (per 1 million people) (x) vs Percent over 65 (y)
##  2. Outdoor recreation retail stores (per 1 million people (x) vs Percent in poverty (y)
##  3. Outdoor recreation retail stores (per 1 million people) (x) vs Percent non-white (y)
##
##  Figure 2
##  1. API index (x) vs Percent over 65 (y)
##  2. API index (x) vs Percent in poverty (y)
##  3. API index (x) vs Percent non-white (y)
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
### NOTE: Uses the gini version of labels and plot function
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


## 3. SAVE ALL PLOTS ----
### 1. Figures 1 ----
ggplot2::ggsave(
  here::here("figures", "bivariate", "fig01-1.pdf"),
  p1$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "bivariate", "fig01-1.jpg"),
  p1$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)

ggplot2::ggsave(
  here::here("figures", "bivariate", "fig01-2.pdf"),
  p2$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "bivariate", "fig01-2.jpg"),
  p2$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)

ggplot2::ggsave(
  here::here("figures", "bivariate", "fig01-3.pdf"),
  p3$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "bivariate", "fig01-3.jpg"),
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
  here::here("figures", "bivariate", "fig01_retail.pdf"),
  p1_all,
  device = grDevices::cairo_pdf,
  width = 6,
  height = 8,
  scale = 1.2
)
ggplot2::ggsave(
  here::here("figures", "bivariate", "fig01_retail.jpg"),
  p1_all,
  dpi = 300,
  width = 6,
  height = 8,
  scale = 1.2
)


### 3. Figures 2 ----
ggplot2::ggsave(
  here::here("figures", "bivariate", "fig02-1.pdf"),
  p4$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "bivariate", "fig02-1.jpg"),
  p4$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)

ggplot2::ggsave(
  here::here("figures", "bivariate", "fig02-2.pdf"),
  p5$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "bivariate", "fig02-2.jpg"),
  p5$plot,
  dpi = 300,
  width = 7,
  height = 4,
  scale = 1.1
)

ggplot2::ggsave(
  here::here("figures", "bivariate", "fig02-3.pdf"),
  p6$plot,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 4,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "bivariate", "fig02-3.jpg"),
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
  here::here("figures", "bivariate", "fig02_api.pdf"),
  p2_all,
  device = grDevices::cairo_pdf,
  width = 6,
  height = 8,
  scale = 1.2
)
ggplot2::ggsave(
  here::here("figures", "bivariate", "fig02_api.jpg"),
  p2_all,
  dpi = 300,
  width = 6,
  height = 8,
  scale = 1.2
)


# B. UNIVARIATE PLOTS ----

## 1. Figure U1. Percentage of population 65 years or older, 2018 ----
p1 <- plot_counties(plotting_df, "p65older") +
  ggplot2::scale_fill_viridis_c(
    "Percent of population over 65",
    trans = "log1p",
    direction = -1,
    guide = ggplot2::guide_colorbar(
      title.position = "top",
      barheight = ggplot2::unit(.5, "cm"),
      barwidth = ggplot2::unit(8, "cm")
    )
  )  +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
  here::here("figures", "univariate", "figU01_p_65yo.pdf"),
  p1,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 6,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "univariate", "figU01_p_65yo.jpg"),
  p1,
  dpi = 300,
  width = 7,
  height = 6,
  scale = 1.1
)

## 2. Figure U2. Percentage of households living in poverty, 2016 ----
p2 <- plot_counties(plotting_df, "p_poverty") +
  ggplot2::scale_fill_viridis_c(
    "Percent of population below poverty level",
    trans = "log1p",
    direction = -1,
    guide = ggplot2::guide_colorbar(
      title.position = "top",
      barheight = ggplot2::unit(.5, "cm"),
      barwidth = ggplot2::unit(8, "cm")
    )
  )  +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
  here::here("figures", "univariate", "figU02_poverty.pdf"),
  p1,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 6,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "univariate", "figU02_poverty.jpg"),
  p1,
  dpi = 300,
  width = 7,
  height = 6,
  scale = 1.1
)

## 3. Figure U3. Percentage of population non-Hispanic and non-White ----
p3 <- plot_counties(plotting_df, "p_nonwhite") +
  ggplot2::scale_fill_viridis_c(
    "Percent of population non-Hispanic and non-White",
    direction = -1,
    guide = ggplot2::guide_colorbar(
      title.position = "top",
      barheight = ggplot2::unit(.5, "cm"),
      barwidth = ggplot2::unit(8, "cm")
    )
  )  +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
  here::here("figures", "univariate", "figU03_p_nonwhite_nonhispanic.pdf"),
  p1,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 6,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "univariate", "figU03_p_nonwhite_nonhispanic.jpg"),
  p1,
  dpi = 300,
  width = 7,
  height = 6,
  scale = 1.1
)

## 4. Figure U4. Population (log 10), 2018 ----
p4 <- plot_counties(plotting_df, "n_pop_2018") +
  ggplot2::scale_fill_viridis_c(
    "Population (log10)",
    trans = "log10",
    direction = 1,
    guide = ggplot2::guide_colorbar(
      title.position = "top",
      barheight = ggplot2::unit(.5, "cm"),
      barwidth = ggplot2::unit(8, "cm")
    ),
    breaks = 10 ^ (2:7),
    labels = c("100", "1,000", "10,000", "100,000", "1,000,000", "10,000,000")
  )  +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
  here::here("figures", "univariate", "figU04_population.pdf"),
  p1,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 6,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "univariate", "figU04_population.jpg"),
  p1,
  dpi = 300,
  width = 7,
  height = 6,
  scale = 1.1
)

## 5. Figure U5. API Index ----
p5 <- plot_counties(plotting_df, "api_index") +
  ggplot2::scale_fill_viridis_c(
    "Access to Park Index",
    direction = -1,
    trans = "log1p",
    na.value = "grey50",
    guide = ggplot2::guide_colorbar(
      title.position = "top",
      barheight = ggplot2::unit(.5, "cm"),
      barwidth = ggplot2::unit(8, "cm")
    )
  )  +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
  here::here("figures", "univariate", "figU05_api.pdf"),
  p1,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 6,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "univariate", "figU05_api.jpg"),
  p1,
  dpi = 300,
  width = 7,
  height = 6,
  scale = 1.1
)

## 6. Figure U6. Outdoor recreation retail stores (per 1 million people)  ----
p6 <- plot_counties(plotting_df, "p_business_permil") +
  ggplot2::scale_fill_viridis_c(
    "Outdoor recreation retail stores (per 1 million people)",
    direction = -1,
    trans = "log1p",
    na.value = "grey50",
    guide = ggplot2::guide_colorbar(
      title.position = "top",
      barheight = ggplot2::unit(.5, "cm"),
      barwidth = ggplot2::unit(8, "cm")
    )
  )  +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
  here::here("figures", "univariate", "figU06_n_businesses_permil.pdf"),
  p1,
  device = grDevices::cairo_pdf,
  width = 7,
  height = 6,
  scale = 1.1
)
ggplot2::ggsave(
  here::here("figures", "univariate", "figU06_n_businesses_permil.jpg"),
  p1,
  dpi = 300,
  width = 7,
  height = 6,
  scale = 1.1
)


## 7. Univariate plots stitched ----

u_all <- cowplot::plot_grid(
  p1,
  p2,
  p3,
  p4,
  p5,
  p6,
  ncol = 2,
  labels = c("A", "B", "C", "D", "E", "F"),
  scale = rep(1, 3)
)
ggplot2::ggsave(
  here::here("figures", "univariate", "figU_stitched.pdf"),
  u_all,
  device = grDevices::cairo_pdf,
  width = 6,
  height = 8,
  scale = 1.2
)
ggplot2::ggsave(
  here::here("figures", "univariate", "figU_stitched.jpg"),
  u_all,
  dpi = 300,
  width = 6,
  height = 8,
  scale = 1.2
)

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


# 06. UNIVARIATE PLOTS ----

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
  here::here("figures", "univariate", "fig_univariate_all.pdf"),
  u_all,
  device = grDevices::cairo_pdf,
  width = 6,
  height = 8,
  scale = 1.2
)
ggplot2::ggsave(
  here::here("figures", "univariate", "fig_univariate_all.jpg"),
  u_all,
  dpi = 300,
  width = 6,
  height = 8,
  scale = 1.2
)

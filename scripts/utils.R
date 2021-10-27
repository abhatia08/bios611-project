## Loard Libraries ----

library(patchwork)
library(tidyverse)
library(here)
library(viridis)

## Patchwork Univariate county plots (Shiny) ----
plot_counties <-
  function(plotting_df, fill_var) {
    p_co <- plotting_df %>%
      filter(state == "colorado") %>%
      ggplot() +
      geom_sf(aes_string(fill = fill_var),
              color = "darkgrey",
              size = 0.2) +
      scale_color_viridis(
        fill_var,
        direction = -1,
        na.value = "grey92",
        discrete = TRUE
      ) +
      ggthemes::theme_map() + coord_sf() +
      ggtitle('Colorado') + theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray95"),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 8),
        panel.background = element_rect(fill = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key = element_rect(fill = NA, color = NA),
        legend.key.width = unit(1.5, 'cm'),
        legend.background = element_rect(fill = "transparent", color = NA)
      ) + labs(fill = NULL)
    
    
    p_fl <- plotting_df %>%
      filter(state == "florida") %>%
      ggplot() +
      geom_sf(aes_string(fill = fill_var),
              color = "darkgrey",
              size = 0.2) +
      scale_color_viridis(
        fill_var,
        direction = -1,
        na.value = "grey92",
        discrete = TRUE
      ) +
      ggthemes::theme_map() + coord_sf() +
      ggtitle('Florida') + theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray95"),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 8),
        panel.background = element_rect(fill = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key = element_rect(fill = NA, color = NA),
        legend.key.width = unit(1.5, 'cm'),
        legend.background = element_rect(fill = "transparent", color = NA)
      ) + labs(fill = NULL)
    
    p_ga <- plotting_df %>%
      filter(state == "georgia") %>%
      ggplot() +
      geom_sf(aes_string(fill = fill_var),
              color = "darkgrey",
              size = 0.2) +
      scale_color_viridis(
        fill_var,
        direction = -1,
        na.value = "grey92",
        discrete = TRUE
      ) +
      ggthemes::theme_map() + coord_sf() +
      ggtitle('Georgia') + theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray95"),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 8),
        panel.background = element_rect(fill = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key = element_rect(fill = NA, color = NA),
        legend.key.width = unit(1.5, 'cm'),
        legend.background = element_rect(fill = "transparent", color = NA)
      ) + labs(fill = NULL)
    
    p_ma <-
      plotting_df %>%
      filter(state == "massachusetts") %>%
      ggplot() +
      geom_sf(aes_string(fill = fill_var),
              color = "darkgrey",
              size = 0.2) +
      scale_color_viridis(
        fill_var,
        direction = -1,
        na.value = "grey92",
        discrete = TRUE
      ) +
      ggthemes::theme_map() + coord_sf() +
      ggtitle('Massachussetts') + theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray95"),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 8),
        panel.background = element_rect(fill = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key = element_rect(fill = NA, color = NA),
        legend.key.width = unit(1.5, 'cm'),
        legend.background = element_rect(fill = "transparent", color = NA)
      ) + labs(fill = NULL)
    
    
    p_oh <- plotting_df %>%
      filter(state == "ohio") %>%
      ggplot() +
      geom_sf(aes_string(fill = fill_var),
              color = "darkgrey",
              size = 0.2) +
      scale_color_viridis(
        fill_var,
        direction = -1,
        na.value = "grey92",
        discrete = TRUE
      ) +
      ggthemes::theme_map() + coord_sf() +
      ggtitle('Ohio') + theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray95"),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 8),
        panel.background = element_rect(fill = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key = element_rect(fill = NA, color = NA),
        legend.key.width = unit(1.5, 'cm'),
        legend.background = element_rect(fill = "transparent", color = NA)
      ) + labs(fill = NULL)
    
    
    p_or <- plotting_df %>%
      filter(state == "oregon") %>%
      ggplot() +
      geom_sf(aes_string(fill = fill_var),
              color = "darkgrey",
              size = 0.2) +
      scale_color_viridis(
        fill_var,
        direction = -1,
        na.value = "grey92",
        discrete = TRUE
      ) +
      ggthemes::theme_map() + coord_sf() +
      ggtitle('Oregon') + theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray95"),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 8),
        panel.background = element_rect(fill = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key = element_rect(fill = NA, color = NA),
        legend.key.width = unit(1.5, 'cm'),
        legend.background = element_rect(fill = "transparent", color = NA)
      ) + labs(fill = NULL)
    
    
    p_tx <- plotting_df %>%
      filter(state == "texas") %>%
      ggplot() +
      geom_sf(aes_string(fill = fill_var),
              color = "darkgrey",
              size = 0.2) +
      scale_color_viridis(
        fill_var,
        direction = -1,
        na.value = "grey92",
        discrete = TRUE
      ) +
      ggthemes::theme_map() + coord_sf() +
      ggtitle('Texas') + theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray95"),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 8),
        panel.background = element_rect(fill = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key = element_rect(fill = NA, color = NA),
        legend.key.width = unit(1.5, 'cm'),
        legend.background = element_rect(fill = "transparent", color = NA)
      ) + labs(fill = NULL)
    
    p_wa <-
      plotting_df %>%
      filter(state == "washington") %>%
      ggplot() +
      geom_sf(aes_string(fill = fill_var),
              color = "darkgrey",
              size = 0.2) +
      scale_color_viridis(
        fill_var,
        direction = -1,
        na.value = "grey92",
        discrete = TRUE
      ) +
      ggthemes::theme_map() + coord_sf() +
      ggtitle('Washington') + theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major = element_line(colour = "gray90"),
        panel.grid.minor = element_line(colour = "gray95"),
        axis.text = element_text(size = 7),
        legend.text = element_text(size = 8),
        panel.background = element_rect(fill = NA),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key = element_rect(fill = NA, color = NA),
        legend.key.width = unit(1.5, 'cm'),
        legend.background = element_rect(fill = "transparent", color = NA)
      ) + labs(fill = NULL)
    
    
    p_co + p_fl + p_ga + p_ma + p_oh + p_or + p_tx + p_wa + plot_layout(ncol = 3)
  }

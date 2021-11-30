## Load Libraries

library(patchwork)
library(tidyverse)
library(viridis)
library(ggthemes)
library(sf)
# sf::sf_use_s2(FALSE)

# ENSURE DIRECTORY ----
## Create directory if it doesn't exist
ensure_directory <- function(directory) {
  if (!dir.exists(directory)) {
    dir.create(directory)
    
  }
}


# BIVARIATE PLOTS ----

discretize_variable <-
  function(plotting_df, d_var, high_val, low_val) {
    d_var <- rlang::enquo(d_var)
    new_var <- paste0(rlang::quo_name(d_var), "_discrete")
    
    plotting_df %>%
      dplyr::transmute(
        fips,
        abbrev,
        name,!!new_var := dplyr::case_when(
          !!d_var > high_val ~ sprintf("high >%0.2f", high_val),!!d_var < low_val ~ sprintf("low <%0.2f", low_val),
          TRUE ~ sprintf("mid %0.2f to %0.2f", low_val, high_val)
        )
      )
  }

gen_color_legend <- function(rev_x = FALSE, rev_y = FALSE) {
  var_x = c("low_x", "mid_x", "high_x")
  var_y  = c("low_y", "mid_y", "high_y")
  
  if (rev_x) {
    var_x <- rev(var_x)
  }
  if (rev_y) {
    var_y <- rev(var_y)
  }
  
  c_legend <-
    expand.grid(var_x = var_x,
                var_y = var_y,
                stringsAsFactors = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      color_hex = c(
        "#e8e6f2",
        "#b5d3e7",
        "#4fadd0",
        "#e5b4d9",
        "#b8b3d8",
        "#3983bb",
        "#d34fa6",
        "#b03598",
        "#2a1a8a"
      ),
      color_hex_a = c(
        "#e8e8e8",
        "#e4acac",
        "#c85a5a",
        "#b0d5df",
        "#ad9ea5",
        "#985356",
        "#64acbe",
        "#627f8c",
        "#574249"
      )
    )
  
  return(c_legend)
}


create_bivariate_df <- function(plotting_df,
                                x_var,
                                x_high,
                                x_low,
                                y_var,
                                y_high,
                                y_low,
                                rev_x = FALSE,
                                rev_y = FALSE) {
  new_x_var <-
    paste0(rlang::quo_name(rlang::enquo(x_var)), "_discrete")
  new_y_var <-
    paste0(rlang::quo_name(rlang::enquo(y_var)), "_discrete")
  
  result_df <- dplyr::left_join(
    discretize_variable(plotting_df, {
      {
        x_var
      }
    }, x_high, x_low),
    discretize_variable(plotting_df, {
      {
        y_var
      }
    }, y_high, y_low)
  )
  
  result_df$var_x <- NA_character_
  result_df[grepl("mid", result_df[[rlang::sym(new_x_var)]]), "var_x"] <-
    "mid_x"
  
  if (rev_x) {
    result_df[grepl("low", result_df[[rlang::sym(new_x_var)]]), "var_x"] <-
      "high_x"
    result_df[grepl("high", result_df[[rlang::sym(new_x_var)]]), "var_x"] <-
      "low_x"
  } else {
    result_df[grepl("low", result_df[[rlang::sym(new_x_var)]]), "var_x"] <-
      "low_x"
    result_df[grepl("high", result_df[[rlang::sym(new_x_var)]]), "var_x"] <-
      "high_x"
  }
  
  
  result_df$var_y <- NA_character_
  result_df[grepl("mid", result_df[[rlang::sym(new_y_var)]]), "var_y"] <-
    "mid_y"
  
  if (rev_y) {
    result_df[grepl("low", result_df[[rlang::sym(new_y_var)]]), "var_y"] <-
      "high_y"
    result_df[grepl("high", result_df[[rlang::sym(new_y_var)]]), "var_y"] <-
      "low_y"
  } else {
    result_df[grepl("low", result_df[[rlang::sym(new_y_var)]]), "var_y"] <-
      "low_y"
    result_df[grepl("high", result_df[[rlang::sym(new_y_var)]]), "var_y"] <-
      "high_y"
  }
  
  result_df %>%
    dplyr::left_join(gen_color_legend())
}

gen_hotspots_legend <- function(rev_x = FALSE, rev_y = FALSE) {
  ## Create the plot
  p_legend <-
    ggplot2::ggplot(
      gen_color_legend(rev_x = rev_x, rev_y = rev_y),
      ggplot2::aes(x = var_x, y = var_y, fill = color_hex)
    ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_identity(na.value = "grey92") +
    mk_nytimes(
      panel.grid.major = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 8),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        vjust = 1
      ),
      axis.line = ggplot2::element_line(
        arrow = ggplot2::arrow(
          length = ggplot2::unit(.15, "inches"),
          type = "open",
          angle = 20
        )
      ),
      plot.subtitle = ggplot2::element_text(size = 10, hjust =
                                              0.5)
    ) +
    ggplot2::coord_equal()
  
  return(p_legend)
}

plot_counties <-
  function(counties_df, fill_var = "trunc_weighted_time") {
    usmap::plot_usmap(
      regions = "counties",
      data = counties_df,
      values = fill_var,
      color = NA
    ) +
      ggplot2::geom_polygon(
        data = usmap::us_map(region = "states"),
        ggplot2::aes(x = x, y = y, group = group),
        color = "black",
        size = .3,
        fill = NA
      )
  }

## BIVARIATE PLOT
plot_bivariate <- function(bivariate_df) {
  usmap::plot_usmap(
    regions = "counties",
    data = bivariate_df,
    values = "color_hex",
    color = NA
  ) +
    ggplot2::geom_polygon(
      data = usmap::us_map(region = "states"),
      ggplot2::aes(x = x, y = y, group = group),
      color = "black",
      size = .3,
      fill = NA
    ) +
    ggplot2::scale_fill_identity() +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
}

return_rounded_iqr <- function(x) {
  round(quantile(x, c(.25, .75), na.rm = TRUE, names = FALSE))
}

mega_plot_bivariate <-
  function(plotting_df,
           x_var,
           x_high,
           x_low,
           x_label,
           rev_x = FALSE,
           y_var,
           y_high,
           y_low,
           y_label,
           rev_y = FALSE,
           return_data = FALSE,
           use_layout = FALSE,
           as_strings = FALSE) {
    ## We need to recast as symbols because Shiny keeps them as chars
    if (as_strings) {
      x_var <- rlang::sym(x_var)
    }
    if (as_strings) {
      y_var <- rlang::sym(y_var)
    }
    
    x_labels <- c(
      sprintf("Low: <%i", round(x_low)),
      sprintf("Moderate: %i-%i", round(x_low), round(x_high)),
      sprintf("High: >%i", round(x_high))
    )
    y_labels <- c(
      sprintf("Low: <%i", round(y_low)),
      sprintf("Moderate: %i-%i", round(y_low), round(y_high)),
      sprintf("High: >%i", round(y_high))
    )
    
    if (rev_x) {
      x_labels <- rev(x_labels)
    }
    if (rev_y) {
      y_labels <- rev(y_labels)
    }
    
    p1 <- gen_hotspots_legend(rev_x = rev_x, rev_y = rev_y) +
      ggplot2::scale_x_discrete(x_label,
                                expand = c(0.05, 0),
                                labels = x_labels) +
      ggplot2::scale_y_discrete(y_label,
                                expand = c(0.05, 0),
                                labels = y_labels) +
      theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
    
    discrete_df <- create_bivariate_df(plotting_df,
                                       {
                                         {
                                           x_var
                                         }
                                       },
                                       x_high,
                                       x_low,
                                       {
                                         {
                                           y_var
                                         }
                                       },
                                       y_high,
                                       y_low,
                                       rev_x = rev_x,
                                       rev_y = rev_y)
    
    p2 <- plot_bivariate(discrete_df)
    
    if (use_layout) {
      layout <- "
            ##BBBBBB
            ##BBBBBB
            AABBBBBB
            ##BBBBBB
            ##BBBBBB
        "
      p3 <- p1 + p2 + patchwork::plot_layout(design = layout)
    } else {
      p3 <- p1 + p2 + patchwork::plot_layout(widths = c(3, 10), ncol = 2)
    }
    
    if (return_data) {
      x <- list(
        counts = discrete_df %>%
          dplyr::group_by_at(dplyr::vars(dplyr::one_of(
            names(discrete_df)[4:8]
          ))) %>%
          dplyr::count(),
        data = discrete_df,
        plot = p3,
        map_only = p2,
        legend_only = p1
      )
      x
    } else {
      p3
    }
  }


# MKIANG'S NYTIMES AESTHETIC ----
## This theme was adapted by mkiang here:
## https://github.com/mkiang/county_preparedness/blob/master/code/mk_nytimes.R

mk_nytimes <- function(...) {
  ## http://minimaxir.com/2015/02/ggplot-tutorial/
  ## paste0('https://timogrossenbacher.ch/2016/12/',
  ##        'beautiful-thematic-maps-with-ggplot2-only/')
  ## https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r
  
  ## Colors - stick with the ggplot2() greys
  c_bg    <- "white"
  c_grid  <- "grey80"
  c_btext <- "grey5"
  c_mtext <- "grey30"
  
  # Begin construction of chart
  theme_bw(base_size = 12, base_family = "Arial Narrow") +
    
    # Region
    theme(
      panel.background = element_rect(fill = c_bg, color = c_bg),
      plot.background  = element_rect(fill = c_bg, color = c_bg),
      panel.border     = element_rect(color = c_bg)
    ) +
    
    # Grid
    theme(
      panel.grid.major = element_line(color = c_grid, size = .25),
      panel.grid.minor = element_blank(),
      axis.ticks       = element_blank()
    ) +
    
    # Legend
    theme(
      legend.position = c(0, 1),
      legend.justification = c(0, 1),
      legend.key           = element_rect(fill = NA, color = NA),
      legend.background    = element_rect(fill = "transparent", color = NA),
      legend.text          = element_text(color = c_mtext)
    ) +
    
    # Titles, labels, etc.
    theme(
      plot.title     = element_text(
        color = c_btext,
        vjust = 1.25,
        face = "bold",
        size = 18
      ),
      axis.text      = element_text(size = 10, color = c_mtext),
      axis.title.x   = element_text(
        size = 10,
        color = c_mtext,
        hjust = 1
      ),
      axis.title.y   = element_text(
        size = 10,
        color = c_mtext,
        hjust = 1
      )
    ) +
    # Facets
    theme(
      strip.background = element_rect(fill = c_bg, color = c_btext),
      strip.text = element_text(size = 10, color = c_btext)
    ) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm")) +
    
    # Additionals
    theme(...)
}

## Load Libraries

library(patchwork)
library(tidyverse)
library(viridis)
library(ggthemes)
library(sf)
# sf::sf_use_s2(FALSE)

# ENSURE DIRECTORY ----
## Create directory if it doesn't exist
ensure_directory <- function(directory){
    if(!dir.exists(directory)){
        dir.create(directory);
    }
}

# UNIVARIATE COUNTY PLOTS (SHINY) ----
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


# MATT'S CODE ----


discretize_variable <-
  function(plotting_df, d_var, high_val, low_val) {
    d_var <- rlang::enquo(d_var)
    new_var <- paste0(rlang::quo_name(d_var), "_discrete")
    
    plotting_df %>%
      dplyr::transmute(
        fips,
        abbrev,
        name,
        !!new_var := dplyr::case_when(
          !!d_var > high_val ~ sprintf("high >%0.2f", high_val),
          !!d_var < low_val ~ sprintf("low <%0.2f", low_val),
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
    ggplot2::scale_fill_identity(na.value = "grey50") + 
    mk_nytimes(
      panel.grid.major = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 11),
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
      plot.subtitle = ggplot2::element_text(size = 13, hjust =
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
    
    discrete_df <- create_bivariate_df(
      plotting_df,
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
      rev_y = rev_y
    )
    
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

return_dict <- function() {
  list(
    "p65older" = list(
      low = 10,
      high = 15,
      reverse = FALSE,
      label = "% population\nover 65 years old",
      min = 0,
      max = 45,
      step = 1,
      transform = "identity"
    ),
     "p_poverty_level_2017" = list(
      low = 10,
      high = 20,
      reverse = FALSE,
      label = "% households\nin poverty",
      min = 0,
      max = 40,
      step = 1,
      transform = "identity"
    ),
      "gini_coef" = list(
      low = .3,
      high = .5,
      reverse = FALSE,
      label = "Gini coefficient",
      min = .25,
      max = .75,
      step = .05,
      transform = "identity"
    ),
    "ice_wb_income" = list(
      low = -.25,
      high = .25,
      reverse = FALSE,
      label = "ICE BW Income",
      min = -.5,
      max = .5,
      step = .05,
      transform = "identity"
    ),
    "premature_mort" = list(
      low = 7500,
      high = 10000,
      reverse = FALSE,
      label = "Age-adjusted years of\npotential life lost before age 75",
      min = 5000,
      max = 15000,
      step = 500,
      transform = "identity"
    ),
    "severe_housing_problems" = list(
      low = 5,
      high = 15,
      reverse = FALSE,
      label = "% households with\nsevere housing problems",
      min = 0,
      max = 30,
      step = 1,
      transform = "identity"
    ),
    "severe_housing_cost_burden" = list(
      low = 5,
      high = 15,
      reverse = FALSE,
      label = "% households with severe\nhousing cost burden",
      min = 0,
      max = 25,
      step = 1,
      transform = "identity"
    ),
    "p_crowded_housing" = list(
      low = 2,
      high = 5,
      reverse = FALSE,
      label = "% crowded housing",
      min = 0,
      max = 15,
      step = .1,
      transform = "identity"
    ),
    # "high_housing_cost" = list(
    #     low = 25,
    #     high = 50,
    #     reverse = FALSE,
    #     label = "% households with\nno broadband internet",
    #     min = 0,
    #     max = 50,
    #     step = 1
    # ),
    "p_nonwhite" = list(
      low = 25,
      high = 50,
      reverse = FALSE,
      label = "% of the population\nnon-Hispanic and non-White",
      min = 0,
      max = 100,
      step = 1,
      transform = "identity"
    ),
    "empower_rate" = list(
      low = 25,
      high = 50,
      reverse = FALSE,
      label = "Medicare beneficiaries with DME\nper 1000 (?) Medicare beneficiaries",
      min = 0,
      max = 200,
      step = 10,
      transform = "identity"
    )
  )
}

return_low <- function(x) {
  return_dict()[[x]]$low
}

return_high <- function(x) {
  return_dict()[[x]]$high
}

return_label <- function(x) {
  return_dict()[[x]]$label
}

return_reverse <- function(x) {
  return_dict()[[x]]$reverse
}

return_min <- function(x) {
  return_dict()[[x]]$min
}

return_max <- function(x) {
  return_dict()[[x]]$max
}

return_step <- function(x) {
  return_dict()[[x]]$step
}

return_transform <- function(x) {
  return_dict()[[x]]$transform
}

return_nonNA <- function(plotting_df, col_x) {
  sum(!is.na(plotting_df[[col_x]]))
}

return_median_val <- function(plotting_df, col_x) {
  sprintf("%0.2f", stats::median(plotting_df[[col_x]], na.rm = TRUE))
}

return_middle_N <- function(plotting_df, col_x, range_x) {
  sum(dplyr::between(plotting_df[[col_x]], range_x[1], range_x[2]), na.rm = TRUE)
}

return_lower_counties <- function(plotting_df, col_x, range_x) {
  sum(plotting_df[[col_x]] < range_x[1], na.rm = TRUE)
}

return_upper_counties <- function(plotting_df, col_x, range_x) {
  sum(plotting_df[[col_x]] > range_x[2], na.rm = TRUE)
}

plot_ranked_covariate <-
  function(plotting_df,
           col_x,
           fips_x = NA,
           rev_x,
           trans_x,
           color_x = "red") {
    sub_df <- plotting_df %>%
      dplyr::select(abbrev, name, fips, col_x) %>%
      dplyr::mutate(name_x = sprintf("%s, %s (%s)", name, abbrev, fips))
    
    if (!rev_x) {
      sub_df$x_rank <-
        dplyr::row_number(dplyr::desc(plotting_df[[col_x]]))
    } else {
      sub_df$x_rank <- dplyr::row_number(plotting_df[[col_x]])
    }
    
    p1 <-
      ggplot2::ggplot(sub_df, ggplot2::aes(x = x_rank, y = !!rlang::sym(col_x))) +
      ggplot2::geom_point(size = .5) +
      ggplot2::scale_x_continuous("Counties by rank (highest to lowest risk)") +
      mk_nytimes(axis.text.x = ggplot2::element_blank()) +
      ggplot2::scale_y_continuous(return_label(col_x),
                                  trans = trans_x)
    
    if (!is.na(fips_x)) {
      p1 <- p1 +
        ggplot2::geom_point(
          data = sub_df %>% dplyr::filter(fips == fips_x),
          color = color_x,
          size = 2.5
        )
    }
    
    p1
  }

generate_county_text <- function(plotting_df, county_x, rf1, rf2) {
  sub_df <- plotting_df %>%
    dplyr::filter(fips == sprintf("%05i", as.integer(county_x)))
  
  COUNTY_NAME <- sprintf("%s, %s", sub_df$name, sub_df$abbrev)
  POP <- sub_df$n_pop_2018
  RISKFACTOR1 <- gsub("\n", " ", return_label(rf1))
  rf1_val <- sub_df[[rf1]]
  HIGHER_COUNTIES1 <-
    sum(plotting_df[[rf1]] < rf1_val, na.rm = TRUE)
  LOWER_COUNTIES1 <-
    sum(plotting_df[[rf1]] > rf1_val, na.rm = TRUE)
  RISKFACTOR2 <- gsub("\n", " ", return_label(rf2))
  rf2_val <- sub_df[[rf2]]
  HIGHER_COUNTIES2 <-
    sum(plotting_df[[rf2]] < rf2_val, na.rm = TRUE)
  LOWER_COUNTIES2 <-
    sum(plotting_df[[rf2]] > rf2_val, na.rm = TRUE)
  
  glue::glue(
    "<b>{COUNTY_NAME}</b> (highlighted in red below) has a population of about <b>{POP}</b> (2018). ",
    "For <b>{RISKFACTOR1}</b>, it has a value of <b>{RF1_VAL}</b>, which is ",
    "higher than <b>{HIGHER_COUNTIES1}</b> counties and lower ",
    "than <b>{LOWER_COUNTIES1}</b> counties. For <b>{RISKFACTOR2}</b>, it has a value ",
    "of <b>{RF2_VAL}</b>, which is higher than <b>{HIGHER_COUNTIES2}</b> counties ",
    "and lower than <b>{LOWER_COUNTIES2}</b> counties.<p><p>",
    COUNTY_NAME = COUNTY_NAME,
    POP = POP,
    RISKFACTOR1 = RISKFACTOR1,
    RF1_VAL =  sprintf("%0.2f", rf1_val),
    HIGHER_COUNTIES1 = HIGHER_COUNTIES1,
    LOWER_COUNTIES1 = LOWER_COUNTIES1,
    RISKFACTOR2 = RISKFACTOR2,
    RF2_VAL = sprintf("%0.2f", rf2_val),
    HIGHER_COUNTIES2 = HIGHER_COUNTIES2,
    LOWER_COUNTIES2 = LOWER_COUNTIES2
  )
}

return_rounded_iqr <- function(x) {
  round(quantile(x, c(.25, .75), na.rm = TRUE, names = FALSE))
}


# MATT'S NYTIMES AESTHETIC ----

mk_nytimes <- function(...) {
  ## http://minimaxir.com/2015/02/ggplot-tutorial/
  ## paste0('https://timogrossenbacher.ch/2016/12/',
  ##        'beautiful-thematic-maps-with-ggplot2-only/')
  ## https://github.com/hrbrmstr/hrbrthemes/blob/master/R/theme-ipsum.r
  
  ## Colos - stick with the ggplot2() greys
  c_bg    <- "white"
  c_grid  <- "grey80"
  c_btext <- "grey5"
  c_mtext <- "grey30"
  
  # Begin construction of chart
  theme_bw(base_size = 12, base_family = "Arial Narrow") +
    
    # Region
    theme(panel.background = element_rect(fill = c_bg, color = c_bg),
          plot.background  = element_rect(fill = c_bg, color = c_bg),
          panel.border     = element_rect(color = c_bg)) +
    
    # Grid
    theme(panel.grid.major = element_line(color = c_grid, size = .25),
          panel.grid.minor = element_blank(),
          axis.ticks       = element_blank()) +
    
    # Legend
    theme(legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.key           = element_rect(fill = NA, color = NA),
          legend.background    = element_rect(fill = "transparent", color = NA),
          legend.text          = element_text(color = c_mtext)) +
    
    # Titles, labels, etc.
    theme(plot.title     = element_text(color = c_btext, vjust = 1.25,
                                        face = "bold", size = 18),
          axis.text      = element_text(size = 10, color = c_mtext),
          axis.title.x   = element_text(size = 12, color = c_mtext,
                                        hjust = 1),
          axis.title.y   = element_text(size = 12, color = c_mtext,
                                        hjust = 1)) +
    # Facets
    theme(strip.background = element_rect(fill = c_bg, color = c_btext),
          strip.text = element_text(size = 10, color = c_btext)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm")) +
    
    # Additionals
    theme(...)
}

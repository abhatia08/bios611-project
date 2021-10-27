# SETUP ----

## Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(here)
library(ggExtra)
library(maps)
library(ggplot2)
library(patchwork)
library(viridis)

# ## Run the cleanup and analysis scripts
source(here::here("scripts", "cleanup.R"))
source(here::here("scripts", "analysis.R"))
source(here::here("scripts", "utils.R"))

## Remove unnecessary dfs
rm(
  ahrf_county,
  ahrf_county_layout,
  ahrf_subset,
  nhw_df,
  non_white_perc,
  orig_pop_df,
  ahrf_df,
  data_wide,
  fips_codes,
  nonwhite_df,
  pop_df,
  pop_wide,
  yelp_df,
  yelp_geocoded,
  yelp_tidy
)

## Wrangle data ----

### Merge in shapefiles
analytic_df <- rename (analytic_df, state_abbr = state)
analytic_df$fips <- as.numeric(analytic_df$fips)

counties_sf <-
  sf::st_as_sf(maps::map("county", plot = FALSE, fill = TRUE),
               crs = 4326) %>%
  mutate(ID = as.character(ID)) %>%
  left_join(maps::county.fips, c(ID = "polyname"))

plotting_df <- left_join(counties_sf, analytic_df, "fips")
plotting_df <-
  plotting_df %>% separate(ID, c('state', 'county'), sep = ',')


## HTML Text Info ----
footer_tag  <- HTML(
  "Created in <a href='https://shiny.rstudio.com/'>Shiny</a>.
      Source code is available on
      <a href='https://github.com/abhatia08/bios611-project'>Github</a>."
)

## UI----

ui <- shinyUI(fluidPage(
  titlePanel(
    "US county-level variation in characteristics influencing participation in outdoor recreation"
  ),
  theme = shinytheme("slate"),
  selectInput(
    "characteristic",
    label = "Characteristic",
    choices = c(
      "% population over 65" = "p65older",
      "% households under the poverty line" = "p_poverty_level_2017",
      "% population non-Hispanic and non-White" = "p_nonwhite",
      "number of outdoor recreation retail businesses per 10000 individuals" = "p_business_pop10000",
      "population in 2018" = "n_pop_2018"
    )
  ),
  plotOutput("charMap")
))

## SERVER ----

server <- function(input, output) {
  output$charMap <- renderPlot({
    plot_counties(plotting_df, input$characteristic)
  }, height = 1000, width = 1400)
}


## Start the app
shinyApp(
  ui = ui,
  server = server,
  options = list(port = 8080, host = "0.0.0.0")
)

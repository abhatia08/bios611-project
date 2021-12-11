# SETUP ----

## 1. Load packages ----
library(shiny)
library(tidyverse)
library(shinythemes)
library(dplyr)
library(here)
library(ggExtra)
library(maps)
library(ggplot2)
library(patchwork)
library(viridis)
library(rlang)
library(glue)
library(ggExtra)

## 2. Run util.R ----
source(here::here("scripts", "util.R"))

## 3. Import data ----
county_choice <- readRDS(here("source_data", "county_choice.RDS"))
main_df <- read_csv(here("derived_data", "plotting_data.csv"))


# A. TEXTUAL CONSTANTS ----
## Textual constants ----
footer_tag  <- HTML(
  "Created in <a href='https://shiny.rstudio.com/'>Shiny</a>.
      Source code is available on
      <a href='https://github.com/abhatia08/bios611-project'>
       Github </a>. Credit to <a href='https://github.com/mkiang/'>
       @mkiang </a> whose <a href='http://dx.doi.org/10.1136/bmjopen-2020-039886'>paper in BMJ </a> inspired this analysis"
)

## More info
more_info <- list(
  h4("About the Nature Gap"),
  HTML(
    "The term 'Nature Gap' sheds light on the racial and economic disparities in access to greenspace, unequal distribution of nature, and the unjust experience of people of color in the outdoors across the United States. Systematic practices such as economic segregation, redlining, forced migration, racial violence, and intimidation in the outdoors have been prevalent for decades, have perpetuated the racial divide. Contemporary examples during the pandemic of Christian Cooper 'Birding While Black' in Central Park, and Ahmaud Arbery murdered while jogging down a boulevard in Georgia show the risk and difficulty endured by people of color while in outdoor spaces.
    Access to public open spaces provides communities with the opportunity to engage in physical activity and build community while incentivizing the conservation of biodiversity during the looming climate crisis. Factors typically influencing the use of these spaces include (but are not limited to) population demographics, proximity, and community recreational expenditure."
  )
)

page_title <-
  "Exploring the Nature Gap- US-county level variation access to parks and outdoor recreation retail services"
page_subtitle   <-
  "About this dashboard"
page_desc  <- HTML(
  "This shiny app was adapted using scripts developed for <a href='http://dx.doi.org/10.1136/bmjopen-2020-039886'> this <i> BMJ Open</i> paper</a>.
  Select two covariates of interest from the dropdown boxes to the right. For each covariate, select thresholds for the moderate category.
  Skewed covariates can be transformed using the Axis Transformation option. <p><p>The bivariate map shows which counties fall into each category defined by the legend.
  The scatterplot shows the relationship based on the raw data with the univariate distribution shown on the margins."
)

## Helper functions ----
risk_factor_selector <- function(value_id = "factor1",
                                 label = "Variable 1",
                                 selected_x = "p_nonwhite") {
  selectInput(
    value_id,
    label = label,
    choices = list(
      "access to parks indicator" = "api_index",
      "number of outdoor recreation retail stores (per million population)" = "p_business_permil",
      "% population non-Hispanic and non-White" = "p_nonwhite",
      "% population over 65" = "p65older",
      "% households under the poverty line" = "p_poverty",
      "population (2018)" = "n_pop_2018"
    ),
    selected = selected_x
  )
}



# B. UI ----

ui <- shinyUI(
  fluidPage(
    titlePanel(page_title),
    
    ## Top row ----
    fluidRow(column(width = 6,
                    h4(page_subtitle),
                    p(page_desc)),
             column(width = 6,
                    more_info)),
    
    hr(),
    
    ## Risk Factor Selector ----
    fluidRow(
      column(
        width = 3,
        h4("Variable 1 (x-axis)"),
        risk_factor_selector(
          value_id = "riskfactor1",
          label = NULL,
          selected_x = "api_index"
        ),
        sliderInput(
          inputId = "rf1slider",
          label = "Threshold for moderate level",
          min = 0,
          max = 150,
          step = 1,
          value = c(10, 25)
        ),
        selectInput(
          inputId = "rf1transform",
          label = "Axis Transformation",
          choices = list(
            "None" = "identity",
            "Natural log" = "log1p",
            "Log10" = "log10"
          ),
          selected = "log1p"
        ),
      ),
      column(
        width = 3,
        h4("Variable 2 (y-axis)"),
        risk_factor_selector(
          value_id = "riskfactor2",
          label = NULL,
          selected_x = "p_nonwhite"
        ),
        sliderInput(
          inputId = "rf2slider",
          label = "Threshold for moderate level",
          min = 0,
          max = 50,
          step = 1,
          value = c(10, 15)
        ),
        selectInput(
          inputId = "rf2transform",
          label = "Axis Transformation",
          choices = list(
            "None" = "identity",
            "Natural log" = "log1p",
            "Log10" = "log10"
          ),
          selected = "log1p"
        ),
      ),
      column(
        width = 4,
        offset = 0,
        h4("Highlight a county"),
        selectizeInput(
          inputId = "county",
          label = NULL,
          choices = county_choice,
          selected = 37135,
          multiple = FALSE
        ),
        htmlOutput("county_text")
      ),
    ),
    hr(),
    
    ## Plotting section ----
    fluidRow(
      column(
        width = 2,
        offset = 0,
        h3("Numbers in context"),
        htmlOutput("context_text")
      ),
      column(
        width = 6,
        offset = 0,
        h3("Bivariate Risk Map"),
        align = "center",
        plotOutput("bivariate_map", height = "800px")
      ),
      column(
        width = 4,
        offset = 0,
        h3("Scatterplot"),
        align = "center",
        plotOutput("scatterplot", height = "600px")
      )
    ),
    hr(),
    
    ## Univariate section ----
    fluidRow(
      column(
        width = 4,
        offset = 2,
        plotOutput("risk1_map", height = "300px")
      ),
      column(
        width = 4,
        offset = -2,
        plotOutput("risk2_map", height = "300px")
      ),
    ),
    hr(),
    
    ## Footer ----
    fluidRow(p(),
             br(),
             column(
               width = 12,
               align = 'center',
               footer_tag,
               br(),
               p()
             ))
  )
)



# C. SERVER ----


# Define server
server <- function(input, output, session) {
  observe({
    rf1_val <- input$riskfactor1
    updateSliderInput(
      session,
      "rf1slider",
      value = c(return_low(rf1_val), return_high(rf1_val)),
      min = return_min(rf1_val),
      max = return_max(rf1_val),
      step = return_step(rf1_val)
    )
  })
  
  observe({
    rf2_val <- input$riskfactor2
    updateSliderInput(
      session,
      "rf2slider",
      value = c(return_low(rf2_val), return_high(rf2_val)),
      min = return_min(rf2_val),
      max = return_max(rf2_val),
      step = return_step(rf2_val)
    )
  })
  
  observe({
    rf1_trans_val <- input$rf1transform
    updateSelectInput(session,
                      "rf1transform",
                      selected = return_transform(rf1_trans_val))
  })
  
  observe({
    rf2_trans_val <- input$rf2transform
    updateSelectInput(session,
                      "rf2transform",
                      selected = return_transform(rf2_trans_val))
  })
  
  plotting_df <- reactive({
    main_df
  })
  
  p1 <- reactive({
    mega_plot_bivariate(
      plotting_df = plotting_df(),
      x_var = input$riskfactor1,
      x_high = as.numeric(input$rf1slider[2]),
      x_low = as.numeric(input$rf1slider[1]),
      x_label = return_label(input$riskfactor1),
      rev_x = return_reverse(input$riskfactor1),
      y_var = input$riskfactor2,
      y_high = as.numeric(input$rf2slider[2]),
      y_low = as.numeric(input$rf2slider[1]),
      y_label = return_label(input$riskfactor2),
      rev_y = return_reverse(input$riskfactor2),
      return_data = TRUE,
      use_layout = TRUE,
      as_strings = TRUE
    )
  })
  
  output$bivariate_map <- renderCachedPlot({
    p1()$plot
  },
  cacheKeyExpr = list(
    input$riskfactor1,
    input$riskfactor2,
    input$rf1slider,
    input$rf2slider
  ),
  res = round(72 * 1.5))
  
  output$risk1_label <- renderText(return_label(input$riskfactor1))
  output$risk2_label <- renderText(return_label(input$riskfactor2))
  
  output$scatterplot <- renderCachedPlot({
    ggExtra::ggMarginal(
      ggplot(
        data = left_join(plotting_df(),
                         p1()$data %>%
                           select(fips, color_hex)),
        aes(
          x = !!rlang::sym(input$riskfactor1),
          y = !!rlang::sym(input$riskfactor2),
          color = color_hex
        )
      ) + geom_point(alpha = .8, size = 2) +
        scale_color_identity() +
        mk_nytimes() +
        scale_x_continuous(return_label(input$riskfactor1),
                           trans = input$rf1transform) +
        scale_y_continuous(return_label(input$riskfactor2),
                           trans = input$rf2transform),
      "density"
    )
  },
  cacheKeyExpr = list(
    input$riskfactor1,
    input$riskfactor2,
    input$rf1slider,
    input$rf2slider,
    input$rf1transform,
    input$rf2transform
  ),
  res = round(72 * 1.5))
  
  output$risk1_map <- renderCachedPlot({
    plot_counties(plotting_df(), input$riskfactor1) +
      scale_fill_viridis_c(
        return_label(input$riskfactor1),
        trans = input$rf1transform,
        direction = -1,
        guide = guide_colorbar(
          title.position = "top",
          barheight = unit(.5, "cm"),
          barwidth = unit(8.5, "cm")
        )
      )  +
      theme(legend.position = "bottom")
  },
  cacheKeyExpr = list(input$riskfactor1,
                      input$rf1transform),
  res = round(72 * 1.5))
  
  output$risk2_map <- renderCachedPlot({
    plot_counties(plotting_df(), input$riskfactor2) +
      scale_fill_viridis_c(
        return_label(input$riskfactor2),
        trans = input$rf2transform,
        direction = -1,
        guide = guide_colorbar(
          title.position = "top",
          barheight = unit(.5, "cm"),
          barwidth = unit(8.5, "cm")
        )
      )  +
      theme(legend.position = "bottom")
  },
  cacheKeyExpr = list(input$riskfactor2,
                      input$rf2transform),
  res = round(72 * 1.5))
  
  output$county_text <- renderText({
    generate_county_text(
      plotting_df(),
      county_x = sprintf("%05i", as.numeric(input$county)),
      rf1 = input$riskfactor1,
      rf2 = input$riskfactor2
    )
  })
  
  output$context_text <- renderText({
    paste0(
      glue(
        "The median US county has a value of <b>{MEDIAN}</b> ",
        "for <b>{LABEL}</b> (N={COUNTIES}). The moderate range of ",
        "<b>{LOWER} to {UPPER}</b> includes {MIDDLECOUNTIES} counties. There are ",
        "{LOWERCOUNTIES} counties with values below <b>{LOWER}</b> and {UPPERCOUNTIES} ",
        "counties with values above <b>{UPPER}</b>.<p>",
        MEDIAN = return_median_val(plotting_df(), input$riskfactor1),
        LABEL = return_label(input$riskfactor1),
        COUNTIES = return_nonNA(plotting_df(), input$riskfactor1),
        MIDDLECOUNTIES = return_middle_N(plotting_df(), input$riskfactor1, input$rf1slider),
        LOWERCOUNTIES = return_lower_counties(plotting_df(), input$riskfactor1, input$rf1slider),
        LOWER = input$rf1slider[1],
        UPPER = input$rf1slider[2],
        UPPERCOUNTIES = return_upper_counties(plotting_df(), input$riskfactor1, input$rf1slider)
      ),
      
      glue(
        "<p>For <b>{LABEL}</b> (N={COUNTIES}), the median US county has a ",
        "value of <b>{MEDIAN}</b>. ",
        "The moderate range of ",
        "<b>{LOWER} to {UPPER}</b> includes {MIDDLECOUNTIES} counties. There are ",
        "{LOWERCOUNTIES} counties with values below <b>{LOWER}</b> and {UPPERCOUNTIES} ",
        "counties with values above <b>{UPPER}</b>.<p>",
        "<p>Of all counties (N={COUNTIES}), there are {HIGHRISK} counties with <font color='#2a5a5b'><b> high values for both factors.</b></font>",
        MEDIAN = return_median_val(plotting_df(), input$riskfactor2),
        LABEL = return_label(input$riskfactor2),
        COUNTIES = return_nonNA(plotting_df(), input$riskfactor2),
        MIDDLECOUNTIES = return_middle_N(plotting_df(), input$riskfactor2, input$rf2slider),
        LOWERCOUNTIES = return_lower_counties(plotting_df(), input$riskfactor2, input$rf2slider),
        LOWER = input$rf2slider[1],
        UPPER = input$rf2slider[2],
        UPPERCOUNTIES = return_upper_counties(plotting_df(), input$riskfactor2, input$rf2slider),
        HIGHRISK = p1()$counts %>% filter(color_hex == "#2a5a5b") %>% pull(n)
      )
    )
    
  })
  
}




# D. Start the app ----

shinyApp(
  ui = ui,
  server = server,
  options = list(port = 8080, host = "0.0.0.0")
)

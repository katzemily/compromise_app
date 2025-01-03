# Practicing with Shiny Modules

#### Set-up ----
library(shiny)
library(tidyverse)
library(bslib)
library(shinyWidgets)
library(shinyalert)
library(googlesheets4)

# Get data
gs4_deauth()

traits_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1wBqv1IN2N1lfn0RjHbYddVvcsmverv7O0nee__xkye4/",
                         sheet = "Traits")


tab_UI <- function(id, traits_raw, i) {
  ns <- NS(id)
  tagList(
    title = traits_raw$tab[i],
    h5("Please allocate your 20 points across the following categories."),
    card(
      card_header("Points Summary"),
      layout_columns(
        uiOutput(ns("points_used_box")),
        uiOutput(ns("points_left_box"))
      ),
      card_body(
        progressBar(ns("progress"),
                    value = 20,
                    total = 20, 
                    status = "primary")
      )
    ),
    card(
      layout_columns(
        card(
          sliderInput(ns("var1"), traits_raw$var1[i], 0, 20, 0, step = 1),
          sliderInput(ns("var2"), traits_raw$var2[i], 0, 20, 0, step = 1),
          sliderInput(ns("var3"), traits_raw$var3[i], 0, 20, 0, step = 1),
        ),
        card(
          sliderInput(ns("var4"), traits_raw$var4[i], 0, 20, 0, step = 1),
          sliderInput(ns("var5"), traits_raw$var5[i], 0, 20, 0, step = 1),
          sliderInput(ns("var6"), traits_raw$var6[i], 0, 20, 0, step = 1),
        )
      )
    )
  )
}

tab_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # Calculate the number of points used so far
      points_used <- reactive({
        sum(input$var1, 
            input$var2, 
            input$var3, 
            input$var4, 
            input$var5, 
            input$var6)
      })
      
      # Calculate how many points are left
      points_left <- reactive({
        20 - points_used()
      })
      
      # Create a dynamic value box for points left
      output$points_left_box <- renderUI({
        remaining <- points_left()
        theme <- if (remaining < 0) "danger" else "secondary"
        
        value_box(title = "Points Left",
                  value =   textOutput("points_left"),
                  theme = theme)
      })
      
      
    }
  )
}

#### UI ----
ui <- fluidPage(
  titlePanel("Compromise App"),
  tabsetPanel(
    # Create the different tabs from the traits dataset
    lapply(seq_len(nrow(traits_raw))), function(i) {
      tabPanel(
        title = traits_raw$tab[i],
        h5("Please allocate your 20 points across the following categories."),
        card(
          card_header("Points Summary"),
          layout_columns(
            uiOutput(str_glue("{traits_raw$tab[i]}_points_used_box")),
            uiOutput(str_glue("{traits_raw$tab[i]}_points_left_box"))
          ),
          card_body(
            progressBar(str_glue("{traits_raw$tab[i]}_progress"),
                        value = 20,
                        total = 20, 
                        status = "primary")
          )
        ),
        card(
          layout_columns(
            card(
              sliderInput(str_glue("var1_{traits_raw$tab[i]}"), traits_raw$var1[i], 0, 20, 0, step = 1),
              sliderInput(str_glue("var2_{traits_raw$tab[i]}"), traits_raw$var2[i], 0, 20, 0, step = 1),
              sliderInput(str_glue("var3_{traits_raw$tab[i]}"), traits_raw$var3[i], 0, 20, 0, step = 1),
            ),
            card(
              sliderInput(str_glue("var4_{traits_raw$tab[i]}"), traits_raw$var4[i], 0, 20, 0, step = 1),
              sliderInput(str_glue("var5_{traits_raw$tab[i]}"), traits_raw$var5[i], 0, 20, 0, step = 1),
              sliderInput(str_glue("var6_{traits_raw$tab[i]}"), traits_raw$var6[i], 0, 20, 0, step = 1),
            )
          )
        )
      )
    }
  )
)


server <- function(input, output, session) {
  ## Logistics
  logistics_dataset <- traits_raw |> 
    filter(tab == "Logistics")
  # Calculate the number of points used so far
  logistics_points_used <- reactive({
    sum(input$var1_logistics, input$var2_logistics, input$var3_logistics, input$var4_logistics, input$var5_logistics, input$var6_logistics)
  })
  
  # Calculate how many points are left
  logistics_points_left <- reactive({
    20 - logistics_points_used()
  })
  
  # Create a dynamic value box for points left
  output$logistics_points_left_box <- renderUI({
    logistics_remaining <- logistics_points_left()
    theme <- if (logistics_remaining < 0) "danger" else "secondary"
    
    value_box(title = "Points Left",
              value =   textOutput("logistics_points_left"),
              theme = theme)
  })
  
  # Create a dynamic value box for points used
  output$logistics_points_used_box <- renderUI({
    logistics_used <- logistics_points_used()
    logistics_theme <- if (logistics_used == 20) "success" else "secondary"
    
    value_box(title = "Points Used",
              value =   textOutput("logistics_points_used"),
              theme = logistics_theme)
  })
  
  # Update progress bar
  observe({
    logistics_remaining <- logistics_points_left()
    
    logistics_display_value <- if (logistics_remaining < 0) 0 else logistics_remaining
    
    updateProgressBar(
      session = session,
      id = "logistics_progress",
      value = logistics_display_value,
      total = 20
    )
    
    # Send an error message pop-up when they use too many points
    if (logistics_remaining < 0) {
      shinyalert("Too many points!",
                 "Please remove some points to get back to 0 before submitting.",
                 type = "error")
    }
  })
  
  output$logistics_points_used <- renderText({
    logistics_points_used()
  })
  
  output$logistics_points_left <- renderText({
    logistics_points_left()
  })
  
  ## Personality
  
  # Calculate the number of points used so far
  personality_points_used <- reactive({
    sum(input$var1_personality, input$var2_personality, input$var3_personality, input$var4_personality, input$var5_personality, input$var6_personality)
  })
  
  # Calculate how many points are left
  personality_points_left <- reactive({
    20 - personality_points_used()
  })
  
  # Create a dynamic value box for points left
  output$personality_points_left_box <- renderUI({
    personality_remaining <- personality_points_left()
    theme <- if (personality_remaining < 0) "danger" else "secondary"
    
    value_box(title = "Points Left",
              value =   textOutput("personality_points_left"),
              theme = theme)
  })
  
  # Create a dynamic value box for points used
  output$personality_points_used_box <- renderUI({
    personality_used <- personality_points_used()
    personality_theme <- if (personality_used == 20) "success" else "secondary"
    
    value_box(title = "Points Used",
              value =   textOutput("personality_points_used"),
              theme = personality_theme)
  })
  
  # Update progress bar
  observe({
    personality_remaining <- personality_points_left()
    
    personality_display_value <- if (personality_remaining < 0) 0 else personality_remaining
    
    updateProgressBar(
      session = session,
      id = "personality_progress",
      value = personality_display_value,
      total = 20
    )
    
    # Send an error message pop-up when they use too many points
    if (personality_remaining < 0) {
      shinyalert("Too many points!",
                 "Please remove some points to get back to 0 before submitting.",
                 type = "error")
    }
  })
  
  output$personality_points_used <- renderText({
    personality_points_used()
  })
  
  output$personality_points_left <- renderText({
    personality_points_left()
  })
  
}

shinyApp(ui, server)
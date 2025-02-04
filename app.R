library(shiny)
library(tidyverse)
library(bslib)
library(shinyWidgets)
library(shinyalert)
library(googlesheets4)

gs4_deauth()


traits_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1wBqv1IN2N1lfn0RjHbYddVvcsmverv7O0nee__xkye4/",
           sheet = "Traits")


ui <- fluidPage(
  titlePanel("Compromise App"),
  tabsetPanel(
    tabPanel(
      title = "Logistics",
      h5("Please allocate your 20 points across the following categories."),
      card(
        card_header("Points Summary"),
        layout_columns(
          uiOutput("logistics_points_used_box"),
          uiOutput("logistics_points_left_box")
        ),
        card_body(
          progressBar("logistics_progress",
                      value = 20,
                      total = 20, 
                      status = "primary")
        )
      ),
      card(
        layout_columns(
          card(
            sliderInput("var1_logistics", "Good Planner", 0, 20, 0, step = 1),
            sliderInput("var2_logistics", "Reliable Texter", 0, 20, 0, step = 1),
            sliderInput("var3_logistics", "Clean/Organized", 0, 20, 0, step = 1)
          ),
          card(
            sliderInput("var4_logistics", "On Time", 0, 20, 0, step = 1),
            sliderInput("var5_logistics", "Easy Schedule", 0, 20, 0, step = 1),
            sliderInput("var6_logistics", "Lives Nearby", 0, 20, 0, step = 1)
          )
        )
      )
    ),
    tabPanel(
      title = "Personality",
      h5("Please allocate your 20 points across the following categories."),
      card(
        card_header("Points Summary"),
        layout_columns(
          uiOutput("personality_points_used_box"),
          uiOutput("personality_points_left_box")
        ),
        card_body(
          progressBar("personality_progress",
                      value = 20,
                      total = 20, 
                      status = "primary")
        )
      ),
      card(
        layout_columns(
          card(
            sliderInput("var1_personality", "Funny/Sense of Humor", 0, 20, 0, step = 1),
            sliderInput("var2_personality", "Charming/Charismatic", 0, 20, 0, step = 1),
            sliderInput("var3_personality", "Kind", 0, 20, 0, step = 1)
          ),
          card(
            sliderInput("var4_personality", "Confident", 0, 20, 0, step = 1),
            sliderInput("var5_personality", "Outgoing", 0, 20, 0, step = 1),
            sliderInput("var6_personality", "Ambitious", 0, 20, 0, step = 1)
          )
        )
      )
    ),
    tabPanel(
      title = "Background"
    ),
    tabPanel(
      title = "Lifestyle"
    ),
    tabPanel(
      title = "Physical"
    ),
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
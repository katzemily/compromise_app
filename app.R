library(shiny)
library(tidyverse)
library(bslib)
library(shinyWidgets)
library(shinyalert)


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
            sliderInput("planner", "Good Planner", 0, 20, 0, step = 1),
            sliderInput("texter", "Reliable Texter", 0, 20, 0, step = 1),
            sliderInput("clean", "Clean/Organized", 0, 20, 0, step = 1)
          ),
          card(
            sliderInput("time", "On Time", 0, 20, 0, step = 1),
            sliderInput("schedule", "Easy Schedule", 0, 20, 0, step = 1),
            sliderInput("nearby", "Lives Nearby", 0, 20, 0, step = 1)
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
            sliderInput("funny", "Funny/Sense of Humor", 0, 20, 0, step = 1),
            sliderInput("charisma", "Charming/Charismatic", 0, 20, 0, step = 1),
            sliderInput("kind", "Kind", 0, 20, 0, step = 1),
            sliderInput("confident", "Confident", 0, 20, 0, step = 1),
            sliderInput("outgoing", "Outgoing", 0, 20, 0, step = 1),
            sliderInput("ambitious", "Ambitious", 0, 20, 0, step = 1),
            sliderInput("curiosity", "Curiosity", 0, 20, 0, step = 1)
          ),
          card(
            sliderInput("mystery", "Mysterious", 0, 20, 0, step = 1),
            sliderInput("spontaneous", "Spontaneous", 0, 20, 0, step = 1),
            sliderInput("smart", "Smart", 0, 20, 0, step = 1),
            sliderInput("talented", "Multi-Talented", 0, 20, 0, step = 1),
            sliderInput("calm", "Calmness", 0, 20, 0, step = 1),
            sliderInput("emotional_intellgience", "Emotional Intelligence", 0, 20, 0, step = 1)
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
  # Calculate the number of points used so far
  logistics_points_used <- reactive({
    sum(input$planner, input$texter, input$clean, input$time, input$schedule, input$nearby)
  })
  
  # Calculate how many points are left
  logistics_points_left <- reactive({
    20 - logistics_points_used()
  })
  
  # Create a dynamic value box for points left
  output$logistics_points_left_box <- renderUI({
    remaining <- logistics_points_left()
    theme <- if (remaining < 0) "danger" else "secondary"
    
    value_box(title = "Points Left",
              value =   textOutput("points_left"),
              theme = theme)
  })
  
  # Create a dynamic value box for points used
  output$logistics_points_used_box <- renderUI({
    used <- logistics_points_used()
    theme <- if (used == 20) "success" else "secondary"
    
    value_box(title = "Points Used",
              value =   textOutput("logistics_points_used"),
              theme = theme)
  })
  
  # Update progress bar
  observe({
    remaining <- logistics_points_left()
    
    display_value <- if (remaining < 0) 0 else remaining
    
    updateProgressBar(
      session = session,
      id = "logistics_progress",
      value = display_value,
      total = 20
    )
    
    # Send an error message pop-up when they use too many points
    if (remaining < 0) {
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
  
}

shinyApp(ui, server)
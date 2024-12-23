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
          uiOutput("points_used_box"),
          uiOutput("points_left_box")
        ),
        card_body(
          progressBar("progress",
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
      title = "Personality"
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
  points_used <- reactive({
    sum(input$planner, input$texter, input$clean, input$time, input$schedule, input$nearby)
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
  
  # Create a dynamic value box for points used
  output$points_used_box <- renderUI({
    used <- points_used()
    theme <- if (used == 20) "success" else "secondary"
    
    value_box(title = "Points Used",
              value =   textOutput("points_used"),
              theme = theme)
  })
  
  # Update progress bar
  observe({
    remaining <- points_left()
    
    display_value <- if (remaining < 0) 0 else remaining
    
    updateProgressBar(
      session = session,
      id = "progress",
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
  
  output$points_used <- renderText({
    points_used()
  })
  
  output$points_left <- renderText({
    points_left()
  })
  
}

shinyApp(ui, server)
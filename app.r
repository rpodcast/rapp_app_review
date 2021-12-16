# load packages ----
library(shiny)
library(purrr)
library(ggplot2)

# define utility functions ----
plot_function <- function(b_length) {
  x <- faithful[, 2]
  p <- ggplot(aes(x = waiting), data = faithful) +
    geom_histogram(bins = b_length)
  return(p)
}

# define module ----
create_UI <- function(id) {
  ns <- NS(id)

  sidebarLayout(
      sidebarPanel(
        sliderInput(
          ns("bins"),
          "Number of bins:",
          min = 1,
          max = 50,
          value = 30
        ),
        actionButton(
          ns("eval_btn"), 
          "Reevaluate!", 
          width = "100%"
        ),
        actionButton(
          ns("chg_btn"), 
          "Change view", 
          width = "100%"
        )
      ),
      
      mainPanel(
        textOutput(ns("demo_text")), # Counter text added
        plotOutput(ns("dist_plot"))
      )
  )
}

create_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      counter <- reactiveVal(value = 0)
      plot_r <- reactiveVal(NULL)
      change_trigger <- reactiveVal(NA)

      observeEvent(input$eval_btn, {
        tmp <- counter() # save current value of counter
        counter(tmp + 1) # update counter

        tmp_plot <- plot_function(b_length = isolate(input$bins))
        plot_r(tmp_plot)
      })

      observeEvent(input$chg_btn, {
        change_trigger(rnorm(1))
      })

      output$dist_plot <- renderPlot({
        req(plot_r())
        plot_r()
      })

      output$demo_text <- renderText({
        req(counter())
        paste("You have clicked the draw button", counter(), "times. Congrats!")
      })

      change_trigger
    }
  )
}

# main ui & server code ----
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "superhero"),
  titlePanel("Old Faithful Geyser Data"),
  tabsetPanel(
    id = "my_tabs",
    selected = "panel1",
    type = "hidden",
    tabPanel(
      "panel1",
      create_UI("panel1")
    ),
    tabPanel(
      "panel2", 
      create_UI("panel2")
    )
  )
)

server <- function(input, output, session) {

  p1 <- create_server("panel1")
  p2 <- create_server("panel2")

  observeEvent(p1(), {
    updateTabsetPanel(inputId = "my_tabs", selected = "panel2")
  })

  observeEvent(p2(), {
    updateTabsetPanel(inputId = "my_tabs", selected = "panel1")
  })
}

shinyApp(ui, server)
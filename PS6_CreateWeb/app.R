#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(stringr)
library(readr)
library(dplyr)

t <- read_csv("Numbers of threatened species by major groups of organisms (2000-2022) - Numbers of threatened species by major groups of organisms (2000-2022).csv")



# Define UI for application that draws a histogram
ui <- fluidPage(theme = ("lumen"),

    # Application title
    titlePanel("Past Years Endangered Animals"),

    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel(
              "About",
              p("This page")
            ),
            tabPanel(
              "Plot",
              p("This app records numbers of threatened species by major groups of organisms from 1996 to 2022 (a few years are missing).")
            ),
           plotOutput("distPlot")
        )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

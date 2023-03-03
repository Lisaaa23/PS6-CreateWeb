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
ui <- fluidPage(

    # Application title
    titlePanel("Years of Threanted Animals"),
    
        tabsetPanel(
          tabPanel(
            "About",
            mainPanel(
            em("This app records the numbers of"), 
            strong("assessed and threanted species"),
            em("by major groups of organisms from 2000 to 2022 (one year is missing),","in total of", n_distinct(t$Year), "years."),
            p("The dataset contains 42 observations and 29 variables."),
            p("Here is a small random sample of data:"),
            dataTableOutput("table"),
            tableOutput("t2")
          )
        ),
        
          tabPanel(
            "Plot",
            sidebarLayout(
              sidebarPanel(
                selectInput("kind",
                            "Choose a kind of species",
                            choices = c("Mammals","Birds","Reptiles","Amphibians","Fishes","Subtotal","Insects","Molluscs","Crustaceans","Corals","Arachnids","Velvet worms","Horseshoe crabs","Other invertebrates","Subtotal(Invertebrates)","Mosses","Ferns & allies","Gymnosperms","Flowering plants","Green algae","Red algae","Subtotal(Plants)","Lichens","Mushrooms","Brown algae","Subtotal(Fungi&protists)","total")),
                selectInput("palette", "Select a palette",
                            choices = c("Dark" = "Dark2", "Bright" = "Set1")),
                ),
                mainPanel(
                  plotOutput("plot"),
                  textOutput("text1")
        )
      )
    ),
    
          tabPanel(
            "Table",
            sidebarLayout(
              sidebarPanel(
                selectInput("type",
                            "Choose a group",
                            choices = c("Vertebrates" = "v", "Invertebrates" = "i","Plants" = "p","Fungi&protists" = "f")
                            )
                ),
              
          
              mainPanel(
                p("This panel displays the total assessed and threatened subgroup spcies inside the four major groups (2000-2022)."),
                textOutput("text2"),
                tableOutput("table2")
          )
        )
)
)
)
   
      




# Define server logic required to draw a histogram
server <- function(input, output) {
  output$t2 <- renderTable({
    t %>%
      filter(Year != "") %>% 
      sample_n(5)
  })  
  output$plot <- renderPlot({
    k <- input$kind
    palette <- input$palette
        ggplot(data = t, (aes(Year, t[[k]], col = Category)))+
        geom_point()+
        geom_line()+
        scale_color_brewer(palette = palette)+
        labs(x = "Year", y = "Numbers")
  })
  output$text1 <- renderText({
    paste("The average number of species that are threanted from 2000-2022 is: ", input$kind,
          mean(t[[input$kind]][t$Category == "Total threatened"]))
  })
  output$text2 <- renderText({
    a <- input$type
    if(a == "v"){
      paste("Increase of the total threatened from 2000-2022: 10739-3507 = ", 10739-3507)
    }
    else if(a == "i"){
      paste("Increase of the total threatened from 2000-2022: 6161-1928 = ", 6161-1928)
    }
    else if(a == "p"){
      paste("Increase of the total threatened from 2000-2022: 24914-5611 = ", 24914-5611)
    }
    else if(a == "f"){
      paste("Increase of the total threatened from 2000-2022: 294-0 = ", 294-0)
    }
  })
  
  output$table2 <- renderTable({
    g <- input$type
      if(g == "v"){
        t %>% 
          select(Year, Category, Mammals, Birds, Reptiles, Amphibians, Fishes)
      }
      else if(g == "i"){
        t %>% 
          select(Year, Category, Insects, Molluscs, Crustaceans, Corals, Arachnids, Velvet_worms, Horseshoe_crabs, Other_invertebrates)
      }
      else if(g == "p"){
        t %>% 
          select(Year, Category, Mosses, Ferns_and_allies, Gymnosperms, Flowering_plants, Green_algae, Red_algae)
      }
      else if(g == "f"){
        t %>% 
          select(Year, Category, Lichens, Mushrooms, Brown_algae)
      }

      
}    
      


)
 } 
# Run the application 
shinyApp(ui = ui, server = server)

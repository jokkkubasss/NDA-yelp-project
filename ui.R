#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)



# Define UI for application that draws a histogram
shinyUI(fluidPage(tags$head(
               # Include custom CSS
               includeCSS("www/styles.css")),

  # Application title
  tags$div(class = "title", titlePanel("Las Vegas Party Planning")),
  navbarPage(
    "",
    tabPanel(title = "Welcome!"),
    
    #Plan your trip tab (map)
    tabPanel(
             title = "Plan Your Trip",
             leafletOutput('lv_map'),
               absolutePanel(id = "controls", fixed = FALSE,
                             top = 160, left = "auto", right = 20, bottom = "auto",
                             width = 300, height = "auto",
                 selectInput(
                   "categories",
                   label = "Enter categories",
                   choices = cats.for.select,
                   multiple = TRUE,
                   selectize = TRUE
                 ),
                 sliderInput(
                   "rating",
                   "Minimum stars ratings:",
                   min = 1,
                   max = 5,
                   value = 4,
                   step = 0.1
                 ),
                 checkboxGroupInput(
                   "price_cats", 
                   label = "Price category $",
                   choices = c("Low", "Medium", "High"),
                   inline = TRUE
                   )
                 )
               
             ),
    # Explore the data tab
    tabPanel(title = "Explore The Data"
             
             
             
             ),
    tabPanel(title = "See The Network"),
    tabPanel(title = "Predict your next party!")
  )
)
)

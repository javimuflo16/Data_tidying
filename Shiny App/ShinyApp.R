# "Shinny App"
# "Data Tyding and Reporting"
# Authors: Sara Dovalo del Río and Javier Muñoz Flores"

if (!require("shiny")) install.packages("shiniy")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("readr")) install.packages("readr")
if (!require("tidyverse")) install.packages("tidyverse")
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)

data2 <- read_csv("final_data.csv")

ui <- fluidPage(
  
  # Application title
  titlePanel("Mortality in New York according to sex and race between years 2007-2014"),
  
  # Sidebar with a 3 inputs 
  sidebarLayout(
    sidebarPanel(
      # selectInput(): , our choices are simply a list of all of the years represented in our dataset
      selectInput(inputId = "year",
                  label = "Select Year:",
                  choices = c("2007",
                              "2008",
                              "2009",
                              "2010",
                              "2011",
                              "2012",
                              "2013",
                              "2014")),
      radioButtons(inputId = "sex",
                   label = "Sex:",
                   choices = c("Female" = "F", "Male" = "M")),
      radioButtons(inputId = "race",
                   label = "Race/Ethnicity:",
                   choices = unique(data2$race_ethnicity))
    ),
    # Show plot and table
    # mainPanel(): we're telling Shiny where they'll go once we've created them
    mainPanel(
      plotOutput("mortalityPlot"),
      DT::dataTableOutput("mortalityTable")
    )
  )
)


server = function(input, output) {
  # reactive(): we first require the three inputs that we created in the UI
  # These names should align exactly with the inputId names that were created in the UI
  #align exactly with the inputId names that were created in the UI
  user = reactive({
    req(input$year)
    req(input$sex)
    req(input$race)
    filter(data2, year == input$year) %>%
      filter(sex %in% input$sex) %>%
      filter(race_ethnicity %in% input$race)
  })
  output$mortalityPlot = renderPlot({
    # reorder(): it makes more sense to have the 
    #leading causes of death arranged by decreasing numbers of deaths
    # leading_cause variable should be ordered by decreasing 
    #(specified with the "-") numbers of deaths
    # "stat = 'identity': we will provide the y-values directly 
    #through our death variable
    ggplot(data = user(), aes(x = reorder(leading_cause, -deaths), y = deaths)) +
      geom_bar(stat = 'identity', color = 'lightpink1', fill = 'lightpink1') +
      labs(
        title = "Leading Causes of Death",
        x = "Causes",
        y = "Number of Deaths"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust=1))
  })
  
  output$mortalityTable = 
    DT::renderDataTable({
      # ordered based on deaths in descending order
      DT::datatable(user()[,c("leading_cause", "deaths", "death_rate", "age_adjusted_death_rate")],
                    colnames = c("Leading Cause of Death", "Number of Deaths", "Death Rate", "Age-Adjusted Death Rate"),
                    options = list(order = list(2, 'des')),
                    rownames = FALSE,
      )
      
    })
}

shinyApp(ui = ui, server = server)

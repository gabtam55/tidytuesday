# Load packages-----------------------------------------------------------------
library(tidyverse)
library(shiny)
library(shinythemes)
library(shinyWidgets)


# Build UI----------------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel("Mental Illness & Socioeconomic Status"),
  
  fluidRow(
    column(5, offset = 1,
           sliderTextInput("income", "Household Income",  # Thanks Avision
                           choices = c("$0-$9,999", "$10,000-$24,999", "$25,000-$49,999", "$50,000-$74,999", "$75,000-$99,999",
                                       "$100,000-$124,999", "$125,000-$149,999", "$150,000-$174,999", "$175,000-$199,999", "$200,000+"),
                           selected = c("$0-$9,999", "$200,000+")),
           selectInput("education", "Education", choices = c("All", levels(survey_data$Education)),
                       multiple = TRUE),
           switchInput("employment", "Unemployed",
                       onLabel = "Yes", offLabel = "No", value = FALSE,
                       size = "small", labelWidth = "100%"),
           switchInput("foodstamp", "Food Stamp",
                       onLabel = "Yes", offLabel = "No", value = FALSE,
                       size = "small", labelWidth = "100%"),
           switchInput("housing", "Section 8 Housing",
                       onLabel = "Yes", offLabel = "No", value = FALSE,
                       size = "small", labelWidth = "100%"),
    ),
    
    column(5, offset = 1,
           sliderTextInput("income", "Household Income",
                           choices = c("$0-$9,999", "$10,000-$24,999", "$25,000-$49,999", "$50,000-$74,999", "$75,000-$99,999",
                                       "$100,000-$124,999", "$125,000-$149,999", "$150,000-$174,999", "$175,000-$199,999", "$200,000+"),
                           selected = c("$0-$9,999", "$200,000+")),
           selectInput("education_2", "Education", choices = c("All", levels(survey_data$Education)),
                       multiple = TRUE),
           switchInput("employment_2", "Unemployed",
                       onLabel = "Yes", offLabel = "No", value = FALSE,
                       size = "small", labelWidth = "100%"),
           switchInput("foodstamp_2", "Food Stamp",
                       onLabel = "Yes", offLabel = "No", value = FALSE,
                       size = "small", labelWidth = "100%"),
           switchInput("housing_2", "Section 8 Housing",
                       onLabel = "Yes", offLabel = "No", value = FALSE,
                       size = "small", labelWidth = "100%")
    )
  ),
  
  fluidRow(
    column(6,
           tabsetPanel(
             tabPanel("Plot", plotOutput("plot")),
             tabPanel("Table", plotOutput("plot"))
           )
    ),
    column(6,
           tabsetPanel(
             tabPanel("Plot_2", plotOutput("plot")),
             tabPanel("Table_2", plotOutput("plot"))
           )
    )
  )
)

# Build Server------------------------------------------------------------------
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    survey_data %>%
      filter()
  })
}

# chr [1:2] "June" "September" ---- how do I use the output from slidertextinput


# Build UI----------------------------------------------------------------------
shinyApp(ui, server)

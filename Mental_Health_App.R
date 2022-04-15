options(scipen = 999)
library('tidyverse')
library('stringr')
library('shinydashboard')
library('plotly')
library('shiny')
library('shinyjs')
library('DT')
library('dplyr')
library('markdown')
library('tidyr')
library('ggplot2')
library('hrbrthemes')
library('leaflet')
library('rsconnect')
#install.packages('rsconnect')

#Import dataset
data <- read.csv("cleanSurvey.csv")

colnames(data)
ui <- (fluidPage(
  titlePanel(h1("Mental Health Survey - 2014")),
  #Side Bar
  sidebarLayout(
    sidebarPanel(
      #select country filter
      selectInput(inputId ="countrySelection", 
                  label = "Country Selection", 
                  choices = c(unique(data$Country)), 
                  multiple = TRUE, selected = "United States"),
      
      #Filter - techComp
      selectInput(inputId = "techComp", 
                  label = "Are you working in a tech company?", 
                  choices = c("Yes", "No"),
                  selected =  "Yes"),  
      
      #Input - wellnessProg
      radioButtons(inputId = "wellnessProg", 
                   label = "Did you aware the Wellness Program?", 
                   choices = c("Yes", "No", "Don't know"),  
                   selected = "No",
                   inline = FALSE,
                   width = NULL),
      
      #Input - seekHelp
      radioButtons(inputId = "seekHelp", 
                   label = "Did you seek for help", 
                   choices = c("Yes", "No", "Don't know"), 
                   selected = "No",
                   inline = FALSE,
                   width = NULL),
      
      #Input - treatment
      radioButtons(inputId = "treatment", 
                   label = "Do you have any treatment?", 
                   choices = c("Yes", "No"), 
                   selected = "No",
                   inline = FALSE,
                   width = NULL)
    ),
    ## Assign 5 output plots
    mainPanel(
      plotOutput("thePlot1"), 
      plotOutput("thePlot2"), 
      plotOutput("thePlot3"), 
      plotOutput("thePlot4"), 
      plotOutput("thePlot5")
    )
  )
)) 


### Set up server
server <- function(input, output) {
  set.seed(100)
  histdata <- rnorm(500)
  
  colnames(data)
   
  # Barplot on mental_vs_physical and remote_work
  output$thePlot1 <- renderPlot({ggplot(data %>% 
      filter(Country %in% input$countrySelection) %>%
      filter(tech_company == input$techComp) %>% 
      filter(wellness_program == input$wellnessProg)  %>% 
      filter(seek_help == input$seekHelp)  %>% 
      filter(treatment == input$treatment) ) + 
      geom_bar(mapping=aes(x=mental_vs_physical, fill=remote_work)) 
})
  # Barplot on mental_vs_physical and remote_work
  output$thePlot2 <- renderPlot({ggplot(data %>% 
                                          filter(Country %in% input$countrySelection) %>%
                                          filter(tech_company == input$techComp) %>% 
                                          filter(wellness_program == input$wellnessProg)  %>% 
                                          filter(seek_help == input$seekHelp)  %>% 
                                          filter(treatment == input$treatment) ) + 
      geom_bar(mapping=aes(x=no_employees, fill=remote_work)) 
  })
  
  # Barplot on mental_vs_physical and remote_work
  output$thePlot3 <- renderPlot({ggplot(data %>% 
                                          filter(Country %in% input$countrySelection) %>%
                                          filter(tech_company == input$techComp) %>% 
                                          filter(wellness_program == input$wellnessProg)  %>% 
                                          filter(seek_help == input$seekHelp)  %>% 
                                          filter(treatment == input$treatment) ) + 
      geom_bar(mapping=aes(x=benefits, fill=coworkers)) 
  })
  
  # Barplot on mental_vs_physical and remote_work
  output$thePlot4 <- renderPlot({ggplot(data %>% 
                                          filter(Country %in% input$countrySelection) %>%
                                          filter(tech_company == input$techComp) %>% 
                                          filter(wellness_program == input$wellnessProg)  %>% 
                                          filter(seek_help == input$seekHelp)  %>% 
                                          filter(treatment == input$treatment) ) + 
      geom_bar(mapping=aes(x=mental_health_interview, fill=Age)) 
  })
  
  # Barplot on mental_vs_physical and remote_work
  output$thePlot5 <- renderPlot({ggplot(data %>% 
                                          filter(Country %in% input$countrySelection) %>%
                                          filter(tech_company == input$techComp) %>% 
                                          filter(wellness_program == input$wellnessProg)  %>% 
                                          filter(seek_help == input$seekHelp)  %>% 
                                          filter(treatment == input$treatment) ) + 
      geom_bar(mapping=aes(x=phys_health_interview, fill=Age)) 
  })
}

shinyApp(ui, server)
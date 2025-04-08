library(shiny)
library(tidyverse)

# Load the dataset
#Data from https://www.kaggle.com/datasets/uom190346a/sleep-health-and-lifestyle-dataset/data
sleep_data <- read.csv("Sleep_health_and_lifestyle_dataset.csv")

# Convert categorical variables to factors
sleep_data <- sleep_data %>%
  mutate(
    Gender = factor(Gender),
    Occupation = factor(Occupation),
    BMI.Category = factor(BMI.Category),
    Sleep.Disorder = factor(Sleep.Disorder)
  )

ui <- fluidPage(
  titlePanel("Sleep Health Data Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("gender", "Gender", choices = c("All", levels(sleep_data$Gender))), #select gender
      sliderInput("ageRange", "Age Range", min = min(sleep_data$Age), max = max(sleep_data$Age),
                  value = c(min(sleep_data$Age), max(sleep_data$Age))), #slider to select age range
      selectInput("bmi", "BMI Category", choices = c("All", levels(sleep_data$BMI.Category))), #select BMI
      selectInput("metric", "Select Metric to Explore", #other metrics to be selected
                  choices = c("Sleep Duration (hours)" = "SleepDuration",
                              "Quality of Sleep (1–10)" = "SleepQuality",
                              "Physical Activity (min/day)" = "PhysicalActivity",
                              "Stress Level (1–10)" = "StressLevel",
                              "Heart Rate (bpm)" = "HeartRate",
                              "Daily Steps" = "DailySteps"))
    ),
    
    mainPanel(
      
      #summary statistic display
      fluidRow(
        column(12,
               h4("Data Summary"),
               verbatimTextOutput("summaryStats")
        )
      ),
      
      tabsetPanel(
        #plot for histogram
        tabPanel("Distribution", plotOutput("histPlot")),
        
        #plot for correlation matrix
        tabPanel("Correlation Matrix", plotOutput("corrPlot")),
        
        #plot for pie chart
        tabPanel("Sleep Disorder Breakdown", plotOutput("disorderPlot")),
        
        #plot for boxplot
        tabPanel("Boxplot by Disorder", plotOutput("boxPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Filtered dataset
  filteredData <- reactive({
    df <- sleep_data %>%
      filter(Age >= input$ageRange[1], Age <= input$ageRange[2]) #filter for selecting age range
    
    if (input$gender != "All") df <- df %>% filter(Gender == input$gender) #choose all or male or female gender
    if (input$bmi != "All") df <- df %>% filter(BMI.Category == input$bmi) #choose all or specific bmi category
    
    df
  })
  
  
  #Output for summary statistics table
  output$summaryStats <- renderPrint({
    df <- filteredData()
    
    cat("Number of Individuals:", nrow(df), "\n")
    cat("Average Sleep Duration:", round(mean(df$Sleep.Duration, na.rm = TRUE), 2), "hours\n")
    cat("Average Stress Level:", round(mean(df$Stress.Level, na.rm = TRUE), 2), "\n")
    
    common_disorder <- df %>% 
      filter(!is.na(Sleep.Disorder)) %>%
      count(Sleep.Disorder, sort = TRUE) %>%
      slice(1) %>%
      pull(Sleep.Disorder)%>%
      as.character()
    
    cat("Most Common Sleep Disorder:", common_disorder, "\n")
  })
  
  
  
  # Histogram of selected metric
  
  # Correlation matrix

  # Sleep Disorder Breakdown

  # Boxplot of selected metric by Sleep Disorder
  
  
}

shinyApp(ui = ui, server = server)

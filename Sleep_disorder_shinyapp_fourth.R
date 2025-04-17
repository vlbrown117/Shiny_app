#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load required libraries
library(shiny)
library(ggplot2)
library(DT)
library(shinyWidgets)
library(dplyr)
library(wesanderson)
library(plotly)
library(broom)
library(pROC)
library(shinydashboard)
library(ggcorrplot)

# Load the dataset
#data <- read.csv("/Users/vleary71/Desktop/BHDS2010/Shiny_app/Shiny_csv_files/Sleep_health_and_lifestyle_dataset.csv")
data <- read.csv("Sleep_health_and_lifestyle_dataset.csv")

# Add binary indicators for logistic regression
data$Apnea_Disorder <- ifelse(data$Sleep.Disorder == "Sleep Apnea", 1, 0)
data$Insomnia_Disorder <- ifelse(data$Sleep.Disorder == "Insomnia", 1, 0)

# Convert categorical variables to factors
data <- data %>%
  mutate(
    Gender = factor(Gender),
    Occupation = factor(Occupation),
    BMI.Category = factor(BMI.Category),
    Sleep.Disorder = factor(Sleep.Disorder)
  )

#recode the normal weights to just normal
data <- data %>%
  mutate(BMI.Category = recode(BMI.Category,
                               "Normal Weight" = "Normal"))

# Define UI
ui <- fluidPage(
  titlePanel("Sleep Disorder Risk Explorer"), #Title at top
  
  #Left side sidebar
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "predictor",
        label = "Select a predictor to visualize:",
        choices = c("Physical.Activity.Level", "Stress.Level", "Sleep.Duration", "Age", "Heart.Rate", "Daily.Steps"),
        selected = "Physical.Activity.Level",
        multiple = FALSE,
        options = list(style = "btn-info")
      ),
    
      #model predictors for regression
        checkboxGroupInput("modelVars", "Select Predictors for Regression:",
                         choices = c("Age", "Gender", "BMI.Category", "Physical.Activity.Level"),
                         selected = c("Age", "Gender", "BMI.Category", "Physical.Activity.Level")),
     
      #Select a disorder to be used for modeling
       radioButtons("disorderType", "Select Disorder Outcome for Modeling:",
                   choices = c("Sleep Apnea" = "Apnea_Disorder", "Insomnia" = "Insomnia_Disorder"),
                   selected = "Apnea_Disorder", inline = TRUE),
     
      #age filter
       sliderInput("ageRange", "Filter by Age:",
                  min = min(data$Age, na.rm = TRUE), max = max(data$Age, na.rm = TRUE),
                  value = c(min(data$Age, na.rm = TRUE), max(data$Age, na.rm = TRUE)), step = 1),
      
      #Make it so that the gender and bmi filters don't show up if "Logistic Model" is selected
      conditionalPanel(
        condition = "input.mainTabset !== 'Logistic Model'",
        selectInput("gender", "Gender", choices = c("All", levels(data$Gender)), selected = "All"),
        selectInput("bmi", "BMI Category", choices = c("All", levels(data$BMI.Category)), selected = "All"),
      ),
      
      #download filtered data button
      downloadButton("downloadData", "Download Filtered Data")
    ),
    
    mainPanel(
      tabsetPanel(
        
        id = "mainTabset",  # Assign an ID
        
        #first tab - overview
        tabPanel("Overview", plotlyOutput("summaryPlot", height = "500px")),
        
        #second tab - interactive table
        tabPanel("Interactive Table", DTOutput("dataTable")),
        
        #third tab - averages
        tabPanel("Averages",
                 fluidRow(
                   valueBoxOutput("nIndividuals"),
                   valueBoxOutput("avgSleep"),
                   valueBoxOutput("avgStress")
                 ),
                 fluidRow(
                   valueBoxOutput("avgActivity"),
                   valueBoxOutput("commonDisorder")
                 )
        ),
        
        #fourth tab - correlation matrix
        tabPanel("Correlation Matrix",
                 checkboxGroupInput("corVars", "Select Numeric Variables for Correlation:",
                                    choices = names(data)[sapply(data, is.numeric)],
                                    selected = names(data)[sapply(data, is.numeric)]),
                 plotOutput("corPlot", height = "600px")
        ),
        
        #fifth tab - logistic model
        tabPanel("Logistic Model", 
                 verbatimTextOutput("logregSummary"),
                 plotlyOutput("coefPlot", height = "400px"),
                 plotOutput("rocPlot", height = "300px")),
        
       
        
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  #filtering data frame based on user input
  filteredData <- reactive({
    
    #age filter
    df <- data %>% filter(Age >= input$ageRange[1], Age <= input$ageRange[2])
    
    #gender filter
    if (!is.null(input$gender) && input$gender != "All") df <- df %>% filter(Gender == input$gender) #choose all or male or female gender
    
    #bmi category filter
    if (!is.null(input$bmi) && input$bmi != "All") df <- df %>% filter(BMI.Category == input$bmi) #choose all or specific bmi category
    df
  })
  
  #Overview tab boxplots
  output$summaryPlot <- renderPlotly({
    df <- filteredData()
    pal <- wes_palette("Zissou1", n = length(unique(df$Sleep.Disorder)), type = "continuous")
    p <- ggplot(df, aes_string(x = "Sleep.Disorder", y = input$predictor, fill = "Sleep.Disorder")) +
      geom_boxplot(alpha = 0.8, outlier.shape = NA) +
      scale_fill_manual(values = pal) +
      theme_minimal(base_family = "Helvetica") +
      theme(legend.position = "bottom") +
      labs(title = paste("Distribution of", input$predictor, "by Sleep Disorder"),
           x = "Sleep Disorder", y = input$predictor, fill = "Disorder")
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  #interactive table
  output$dataTable <- renderDT({
    datatable(filteredData(), options = list(pageLength = 10, scrollX = TRUE))
  })
  

  
  #Download filtered data 
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_sleep_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
  
  
  ###Averages tab
  
  #N of individuals
  output$nIndividuals <- renderValueBox({
    valueBox(
      value = nrow(filteredData()),
      subtitle = "Number of Individuals",
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  #average sleep duration
  output$avgSleep <- renderValueBox({
    valueBox(
      value = paste0(round(mean(filteredData()$Sleep.Duration, na.rm = TRUE), 2), " hrs"),
      subtitle = "Avg Sleep Duration",
      icon = icon("bed"),
      color = "purple"
    )
  })
  
  #average stress level
  output$avgStress <- renderValueBox({
    valueBox(
      value = round(mean(filteredData()$Stress.Level, na.rm = TRUE), 2),
      subtitle = "Avg Stress Level",
      icon = icon("heartbeat"),
      color = "red"
    )
  })
  
  #average daily activity time
  output$avgActivity <- renderValueBox({
    valueBox(
      value = paste0(round(mean(filteredData()$Physical.Activity.Level, na.rm = TRUE), 2), " min"),
      subtitle = "Avg Daily Activity",
      icon = icon("walking"),
      color = "green"
    )
  })
  
  #Most common disorder
  output$commonDisorder <- renderValueBox({
    disorder <- filteredData() %>%
      filter(!is.na(Sleep.Disorder)) %>%
      count(Sleep.Disorder, sort = TRUE) %>%
      slice(1) %>%
      pull(Sleep.Disorder) %>%
      as.character()
    
    valueBox(
      value = disorder,
      subtitle = "Most Common Disorder",
      icon = icon("exclamation-triangle"),
      color = "orange"
    )
  })
  
  
  #Correlation Matrix tab
  output$corPlot <- renderPlot({
    df <- filteredData()
    selected_vars <- input$corVars
    if (length(selected_vars) < 2) {
      validate(need(FALSE, "Please select at least two variables."))
    }
    
    cor_data <- df[, selected_vars, drop = FALSE]
    cor_data <- na.omit(cor_data)
    
    cor_matrix <- cor(cor_data)
    
    ggcorrplot(cor_matrix,
               hc.order = TRUE,
               type = "lower",
               lab = TRUE,
               colors = c("#6D9EC1", "white", "#E46726"),
               title = "Correlation Matrix",
               ggtheme = theme_minimal())
  })
  
  ###Logistic Model Tab
  
  output$coefPlot <- renderPlotly({
    df <- filteredData()
    outcome <- input$disorderType
    predictors <- paste(input$modelVars, collapse = " + ")
    formula <- as.formula(paste(outcome, "~", predictors))
    model <- glm(formula, data = df, family = binomial)
    tidy_mod <- tidy(model, conf.int = TRUE)
    
    p <- ggplot(tidy_mod[-1,], aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
      geom_pointrange(color = wes_palette("Zissou1")[3], size = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
      theme_minimal() +
      labs(title = "Logistic Regression Coefficients with 95% CI", x = "Predictor", y = "Log Odds Estimate") +
      coord_flip()
    ggplotly(p)
  })
  
  output$rocPlot <- renderPlot({
    df <- filteredData()
    outcome <- input$disorderType
    predictors <- paste(input$modelVars, collapse = " + ")
    formula <- as.formula(paste(outcome, "~", predictors))
    model <- glm(formula, data = df, family = binomial)
    roc_obj <- roc(df[[outcome]], predict(model, type = "response"))
    plot(roc_obj, col = wes_palette("Zissou1")[1], main = "ROC Curve")
    abline(a = 0, b = 1, lty = 2, col = "gray")
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)

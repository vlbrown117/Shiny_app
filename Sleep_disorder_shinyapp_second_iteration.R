
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

# Define UI
ui <- fluidPage(
  titlePanel("Sleep Disorder Risk Explorer"),

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
      checkboxGroupInput("modelVars", "Select Predictors for Regression:",
                         choices = c("Age", "Gender", "BMI.Category", "Physical.Activity.Level"),
                         selected = c("Age", "Gender", "BMI.Category", "Physical.Activity.Level")),
      radioButtons("disorderType", "Select Disorder Outcome for Modeling:",
                   choices = c("Sleep Apnea" = "Apnea_Disorder", "Insomnia" = "Insomnia_Disorder"),
                   selected = "Apnea_Disorder", inline = TRUE),
      sliderInput("ageRange", "Filter by Age:",
                  min = min(data$Age, na.rm = TRUE), max = max(data$Age, na.rm = TRUE),
                  value = c(min(data$Age, na.rm = TRUE), max(data$Age, na.rm = TRUE)), step = 1),
      selectInput("gender", "Filter by Gender:", choices = c("All", levels(data$Gender))), #select gender
      selectInput("bmi", "Filter by BMI Category:", choices = c("All", levels(data$BMI.Category))), #select BMI
      downloadButton("downloadData", "Download Filtered Data")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Overview", plotlyOutput("summaryPlot", height = "500px")),
        tabPanel("Interactive Table", DTOutput("dataTable")),
        tabPanel("Logistic Model", 
                 verbatimTextOutput("logregSummary"),
                 plotlyOutput("coefPlot", height = "400px"),
                 plotOutput("rocPlot", height = "300px")),
        tabPanel("Summary Statistics",
                 fluidRow(
                   valueBoxOutput("nIndividuals"),
                   valueBoxOutput("avgSleep"),
                   valueBoxOutput("avgStress")
                 ),
                 fluidRow(
                   valueBoxOutput("avgActivity"),
                   valueBoxOutput("commonDisorder")
                 )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {

  filteredData <- reactive({
    df <- data %>% filter(Age >= input$ageRange[1], Age <= input$ageRange[2])
    if (input$gender != "All") df <- df %>% filter(Gender == input$gender) #choose all or male or female gender
    if (input$bmi != "All") df <- df %>% filter(BMI.Category == input$bmi) #choose all or specific bmi category
    df
  })

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

  output$dataTable <- renderDT({
    datatable(filteredData(), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$logregSummary <- renderPrint({
    df <- filteredData()
    outcome <- input$disorderType
    predictors <- paste(input$modelVars, collapse = " + ")
    formula <- as.formula(paste(outcome, "~", predictors))
    model <- glm(formula, data = df, family = binomial)
    summary(model)
  })

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

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_sleep_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
  output$nIndividuals <- renderValueBox({
    valueBox(
      value = nrow(filteredData()),
      subtitle = "Number of Individuals",
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  output$avgSleep <- renderValueBox({
    valueBox(
      value = paste0(round(mean(filteredData()$Sleep.Duration, na.rm = TRUE), 2), " hrs"),
      subtitle = "Avg Sleep Duration",
      icon = icon("bed"),
      color = "purple"
    )
  })
  
  output$avgStress <- renderValueBox({
    valueBox(
      value = round(mean(filteredData()$Stress.Level, na.rm = TRUE), 2),
      subtitle = "Avg Stress Level",
      icon = icon("heartbeat"),
      color = "red"
    )
  })
  
  output$avgActivity <- renderValueBox({
    valueBox(
      value = paste0(round(mean(filteredData()$Physical.Activity.Level, na.rm = TRUE), 2), " min"),
      subtitle = "Avg Daily Activity",
      icon = icon("walking"),
      color = "green"
    )
  })
  
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
}

# Run the app
shinyApp(ui = ui, server = server)

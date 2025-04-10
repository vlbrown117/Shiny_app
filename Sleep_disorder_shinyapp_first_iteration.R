
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

# Load the dataset
data <- read.csv("/Users/vleary71/Desktop/BHDS2010/Shiny_app/Shiny_csv_files/Sleep_health_and_lifestyle_dataset.csv")

# Add binary indicators for logistic regression
data$Apnea_Disorder <- ifelse(data$Sleep.Disorder == "Sleep Apnea", 1, 0)
data$Insomnia_Disorder <- ifelse(data$Sleep.Disorder == "Insomnia", 1, 0)

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
      downloadButton("downloadData", "Download Filtered Data")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Overview", plotlyOutput("summaryPlot", height = "500px")),
        tabPanel("Interactive Table", DTOutput("dataTable")),
        tabPanel("Logistic Model", 
                 verbatimTextOutput("logregSummary"),
                 plotlyOutput("coefPlot", height = "400px"),
                 plotOutput("rocPlot", height = "300px"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {

  filteredData <- reactive({
    df <- data %>% filter(Age >= input$ageRange[1], Age <= input$ageRange[2])
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
}

# Run the app
shinyApp(ui = ui, server = server)


# ================================================================
# Sleep Disorder Risk Explorer Shiny App
# Authors: Brandon Yee, Veronica Leary
# Description:
#   This Shiny application explores behavioral and demographic
#   predictors of sleep disorders using logistic regression,
#   summary statistics, correlation plots, and ROC curves.
# ================================================================

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

# ================================================================
# Load the dataset
# ================================================================
  data <- read.csv("/Users/vleary71/Desktop/BHDS2010/Shiny_app/Shiny_csv_files/Sleep_health_and_lifestyle_dataset.csv")
# data <- read.csv("/Users/veronicaleary/Desktop/BHDS2010/Shiny_app/Shiny_csv_files/Sleep_health_and_lifestyle_dataset.csv")
# data <- read.csv("Sleep_health_and_lifestyle_dataset.csv")

# ================================================================
# Data preprocessing
# Create factor versions and binary indicators
# ================================================================
data <- data %>%
  mutate(
    Gender = factor(Gender),
    Occupation = factor(Occupation),
    BMI.Category = recode(factor(BMI.Category), "Normal Weight" = "Normal"),
    Sleep.Disorder = factor(Sleep.Disorder),
    SleepDisorder = factor(Sleep.Disorder, levels = c("Insomnia", "Sleep Apnea", "None")),
    Apnea_Disorder = ifelse(Sleep.Disorder == "Sleep Apnea", 1, 0),
    Insomnia_Disorder = ifelse(Sleep.Disorder == "Insomnia", 1, 0)
  )

# ================================================================
# UI layout with detailed sidebar and multiple interactive tabs
# ================================================================
ui <- fluidPage(
  titlePanel("Sleep Disorder Risk Explorer"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("predictor", "Select a predictor to visualize:",
        choices = c("Physical.Activity.Level", "Stress.Level", "Sleep.Duration", "Age", "Heart.Rate", "Daily.Steps"),
        selected = "Physical.Activity.Level", multiple = FALSE,
        options = list(style = "btn-info")
      ),
      checkboxGroupInput("modelVars", "Select Predictors for Regression:",
        choices = c("Age", "Gender", "BMI.Category", "Physical.Activity.Level"),
        selected = c("Age", "Gender", "BMI.Category", "Physical.Activity.Level")
      ),
      radioButtons("disorderType", "Select Disorder Outcome for Modeling:",
        choices = c("Sleep Apnea" = "Apnea_Disorder", "Insomnia" = "Insomnia_Disorder"),
        selected = "Apnea_Disorder", inline = TRUE
      ),
      sliderInput("ageRange", "Filter by Age:",
        min = min(data$Age, na.rm = TRUE), max = max(data$Age, na.rm = TRUE),
        value = c(min(data$Age, na.rm = TRUE), max(data$Age, na.rm = TRUE)), step = 1
      ),
      selectInput("gender", "Gender", choices = c("All", levels(data$Gender)), selected = "All"),
      selectInput("bmi", "BMI Category", choices = c("All", levels(data$BMI.Category)), selected = "All"),
      selectInput("varSelect", "Select Variable for Summary:", choices = names(data), selected = "Age"),
      downloadButton("downloadData", "Download Filtered Data")
    ),
    mainPanel(
      tabsetPanel(id = "mainTabset",
                  
                  # -------------------------------
                  # Overview Tab:
                  # Displays a boxplot of the selected predictor variable
                  # grouped by sleep disorder category. Helps users visualize
                  # distribution differences across groups.
                  # -------------------------------
                  tabPanel("Overview", plotlyOutput("summaryPlot", height = "500px")),
                  
                  # -------------------------------
                  # Interactive Table Tab:
                  # Displays the filtered dataset as an interactive table using DT.
                  # Allows users to scroll, sort, and explore the raw data.
                  # -------------------------------        
                  tabPanel("Interactive Table", DTOutput("dataTable")),
                  
                  # -------------------------------
                  # Averages Tab:
                  # Presents summary metrics using value boxes.
                  # Includes:
                  # - Total number of filtered individuals
                  # - Average sleep duration
                  # - Average stress level
                  # - Average physical activity level
                  # - Most common sleep disorder
                  # -------------------------------       
                  tabPanel("Averages",
                           fluidRow(valueBoxOutput("nIndividuals"), valueBoxOutput("avgSleep"), valueBoxOutput("avgStress")),
                           fluidRow(valueBoxOutput("avgActivity"), valueBoxOutput("commonDisorder"))
                  ),
                  
                  # -------------------------------
                  # Correlation Matrix Tab:
                  # Enables users to select numeric variables to generate a correlation
                  # matrix. The matrix helps detect multicollinearity and variable associations.
                  # -------------------------------        
                  tabPanel("Correlation Matrix",
                           checkboxGroupInput("corVars", "Select Numeric Variables for Correlation:",
                                              choices = names(data)[sapply(data, is.numeric)],
                                              selected = names(data)[sapply(data, is.numeric)]
                           ),
                           plotOutput("corPlot", height = "600px")
                  ),
                  
                  # -------------------------------
                  # Summary Stats Tab:
                  # Produces univariate summary statistics for a selected variable.
                  # For numeric variables: mean, median, SD, min, max.
                  # For categorical variables: frequency counts.
                  # -------------------------------        
                  tabPanel("Summary Stats",
                           h3("Summary Statistics"),
                           tableOutput("summaryStats")
                  ),
                  
                  # -------------------------------
                  # Custom Logistic Explorer Tab:
                  # Lets users run a simple logistic regression with a selected
                  # predictor variable against a selected sleep disorder.
                  # Outputs model summary and either a probability line (if numeric)
                  # or a bar chart (if categorical).
                  # -------------------------------       
                  tabPanel("Custom Logistic Explorer",
                           radioButtons("disorderSelect", "Select Disorder to Predict:",
                                        choices = c("Sleep Apnea", "Insomnia"), selected = "Sleep Apnea"),
                           selectInput("customVarSelect", "Select Predictor Variable:",
                                       choices = names(data)[sapply(data, function(x) is.numeric(x) || is.factor(x))]),
                           actionButton("runCustomModel", "Run Logistic Regression"),
                           verbatimTextOutput("customModelSummary"),
                           plotOutput("customRegressionPlot")
                  ),
                  
                  # -------------------------------
                  # Log Odds Tab:
                  # Runs a multivariable logistic regression using selected predictors
                  # and outputs both the model summary and two key visuals:
                  # - Coefficient plot with confidence intervals
                  # - ROC curve to evaluate model performance
                  # -------------------------------        
                  tabPanel("Log Odds",
                           verbatimTextOutput("logregSummary"),
                           plotlyOutput("coefPlot", height = "400px"),
                           plotOutput("rocPlot", height = "300px")
                  )
      )
    )
  )
)

# ================================================================
# Server logic with model fitting, data filtering, and plotting
# ================================================================
server <- function(input, output, session) {
  filteredData <- reactive({
    df <- data %>% filter(Age >= input$ageRange[1], Age <= input$ageRange[2])
    if (input$gender != "All") df <- df %>% filter(Gender == input$gender)
    if (input$bmi != "All") df <- df %>% filter(BMI.Category == input$bmi)
    df
  })

  
  # ----------------------------------------
  # Output: summaryPlot
  # Description: Interactive boxplot showing the distribution of the selected predictor
  # across sleep disorder categories. Uses ggplot2 and plotly for interactivity.
  # ----------------------------------------
  output$summaryPlot <- renderPlotly({
    df <- filteredData()
    pal <- wes_palette("Zissou1", n = length(unique(df$Sleep.Disorder)), type = "continuous")
    p <- ggplot(df, aes_string(x = "Sleep.Disorder", y = input$predictor, fill = "Sleep.Disorder")) +
      geom_boxplot(alpha = 0.8, outlier.shape = NA) +
      scale_fill_manual(values = pal) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(title = paste("Distribution of", input$predictor, "by Sleep Disorder"),
           x = "Sleep Disorder", y = input$predictor)
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })

  
  # ----------------------------------------
  # Output: dataTable
  # Description: Renders the filtered dataset in a scrollable, sortable interactive table.
  # Useful for manual data inspection and verification.
  # ----------------------------------------
  output$dataTable <- renderDT({
    datatable(filteredData(), options = list(pageLength = 10, scrollX = TRUE))
  })

  
  # ----------------------------------------
  # Output: downloadData
  # Description: Enables users to download the currently filtered dataset as a CSV file.
  # ----------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() { paste("filtered_sleep_data", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(filteredData(), file, row.names = FALSE) }
  )

  
  # ----------------------------------------
  # Output: nIndividuals
  # Description: Displays the count of individuals in the current filtered dataset.
  # ----------------------------------------
  output$nIndividuals <- renderValueBox({
    valueBox(nrow(filteredData()), "Number of Individuals", icon = icon("users"), color = "aqua")
  })

  
  # ----------------------------------------
  # Output: avgSleep
  # Description: Displays the average sleep duration in hours, using filtered data.
  # ----------------------------------------
  output$avgSleep <- renderValueBox({
    valueBox(paste0(round(mean(filteredData()$Sleep.Duration, na.rm = TRUE), 2), " hrs"),
             "Avg Sleep Duration", icon = icon("bed"), color = "purple")
  })

  
  # ----------------------------------------
  # Output: avgStress
  # Description: Displays the average reported stress level from the filtered dataset.
  # ----------------------------------------
  output$avgStress <- renderValueBox({
    valueBox(round(mean(filteredData()$Stress.Level, na.rm = TRUE), 2),
             "Avg Stress Level", icon = icon("heartbeat"), color = "red")
  })

  
  # ----------------------------------------
  # Output: avgActivity
  # Description: Displays average physical activity level (in minutes).
  # ----------------------------------------
  output$avgActivity <- renderValueBox({
    valueBox(paste0(round(mean(filteredData()$Physical.Activity.Level, na.rm = TRUE), 2), " min"),
             "Avg Daily Activity", icon = icon("walking"), color = "green")
  })

  
  # ----------------------------------------
  # Output: commonDisorder
  # Description: Displays the most frequently reported sleep disorder in the current data subset.
  # ----------------------------------------
  output$commonDisorder <- renderValueBox({
    disorder <- filteredData() %>% filter(!is.na(Sleep.Disorder)) %>%
      count(Sleep.Disorder, sort = TRUE) %>% slice(1) %>%
      pull(Sleep.Disorder) %>% as.character()
    valueBox(disorder, "Most Common Disorder", icon = icon("exclamation-triangle"), color = "orange")
  })

  
  # ----------------------------------------
  # Output: corPlot
  # Description: Generates a correlation matrix heatmap using selected numeric variables.
  # ----------------------------------------
  output$corPlot <- renderPlot({
    df <- filteredData()
    selected_vars <- input$corVars
    if (length(selected_vars) < 2) {
      validate(need(FALSE, "Please select at least two variables."))
    }
    cor_data <- na.omit(df[, selected_vars])
    cor_matrix <- cor(cor_data)
    ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE,
               colors = c("#6D9EC1", "white", "#E46726"),
               title = "Correlation Matrix", ggtheme = theme_minimal())
  })

  
  # ----------------------------------------
  # Output: coefPlot
  # Description: Displays logistic regression coefficients with 95% confidence intervals.
  # Interactive plot using plotly.
  # ----------------------------------------
  output$coefPlot <- renderPlotly({
    df <- filteredData()
    if (nrow(df) < 10 || length(input$modelVars) == 0) return(NULL)
    formula <- as.formula(paste(input$disorderType, "~", paste(input$modelVars, collapse = " + ")))
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

  
  # ----------------------------------------
  # Output: rocPlot
  # Description: Plots the ROC curve for evaluating logistic model performance.
  # ----------------------------------------
  output$rocPlot <- renderPlot({
    df <- filteredData()
    if (nrow(df) < 10 || length(input$modelVars) == 0) return(NULL)
    formula <- as.formula(paste(input$disorderType, "~", paste(input$modelVars, collapse = " + ")))
    model <- glm(formula, data = df, family = binomial)
    roc_obj <- roc(df[[input$disorderType]], predict(model, type = "response"))
    plot(roc_obj, col = wes_palette("Zissou1")[1], main = "ROC Curve")
    abline(a = 0, b = 1, lty = 2, col = "gray")
  })

  
  # ----------------------------------------
  # Output: summaryStats
  # Description: Renders summary statistics table (numeric or frequency) for selected variable.
  # ----------------------------------------
  output$summaryStats <- renderTable({
    req(input$varSelect)
    var <- input$varSelect
    df <- filteredData()
    if (is.numeric(df[[var]])) {
      data.frame(Variable = var, Mean = mean(df[[var]], na.rm = TRUE), Median = median(df[[var]], na.rm = TRUE), SD = sd(df[[var]], na.rm = TRUE), Min = min(df[[var]], na.rm = TRUE), Max = max(df[[var]], na.rm = TRUE))
    } else {
      as.data.frame(table(df[[var]]))
    }
  })

  observeEvent(input$runCustomModel, {
    df_model <- data
    var <- input$customVarSelect
    disorder <- input$disorderSelect
    df_model$Outcome <- ifelse(df_model$SleepDisorder == disorder, 1, 0)
    formula <- as.formula(paste0("Outcome ~ `", var, "`"))
    model <- glm(formula, data = df_model, family = binomial())

    
    # ----------------------------------------
    # Output: customModelSummary
    # Description: Prints summary output of a logistic regression model using selected predictor.
    # ----------------------------------------
    output$customModelSummary <- renderPrint({
      summary(model)
    })

    if (is.numeric(df_model[[var]])) {
      new_data <- data.frame(seq(min(df_model[[var]], na.rm = TRUE),
                                 max(df_model[[var]], na.rm = TRUE),
                                 length.out = 100))
      names(new_data) <- var
      new_data$Predicted <- predict(model, newdata = new_data, type = "response")

      
      # ----------------------------------------
      # Output: customRegressionPlot (numeric predictor)
      # Description: Shows predicted probability line + jittered data points for numeric variable.
      # ----------------------------------------
      
      # ----------------------------------------
      # Output: customRegressionPlot (categorical predictor)
      # Description: Displays proportion bar chart of sleep disorder outcome by factor level.
      # ----------------------------------------
      output$customRegressionPlot <- renderPlot({
        ggplot(df_model, aes(x = .data[[var]], y = Outcome)) +
          geom_jitter(height = 0.05, width = 0.3, alpha = 0.5) +
          geom_line(data = new_data, aes_string(x = var, y = "Predicted"),
                    color = "blue", size = 1.2) +
          labs(title = paste("Predicted Probability of", disorder, "by", var),
               x = var, y = paste("Probability of", disorder)) +
          theme_minimal()
      })
    } else {
      
      # ----------------------------------------
      # Output: customRegressionPlot (numeric predictor)
      # Description: Shows predicted probability line + jittered data points for numeric variable.
      # ----------------------------------------
      
      # ----------------------------------------
      # Output: customRegressionPlot (categorical predictor)
      # Description: Displays proportion bar chart of sleep disorder outcome by factor level.
      # ----------------------------------------
      output$customRegressionPlot <- renderPlot({
        ggplot(df_model, aes(x = .data[[var]], fill = factor(Outcome))) +
          geom_bar(position = "fill") +
          labs(title = paste("Proportion of", disorder, "by", var),
               x = var, y = "Proportion", fill = disorder) +
          theme_minimal()
      })
    }
  })
}

# ================================================================
# Launch the Shiny application
# ================================================================
shinyApp(ui = ui, server = server)

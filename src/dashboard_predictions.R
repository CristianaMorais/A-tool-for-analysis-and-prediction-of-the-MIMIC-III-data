library(DBI)
library(bigrquery)
library(readr)
library(shiny)
library(dplyr)
library(shinyBS)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(eeptools)
library(tibble)
library(DT)
library(ggplot2)
library(data.table)
#install.packages("mice")
library(mice)
#install.packages("gtsummary")
library(gtsummary)
library(gt)
library(caret)
#install.packages("caTools")
library(caTools)
#install.packages("heatmaply")
library(heatmaply)
library(ggcorrplot)
library(xgboost)
library(randomForest)
library(glmnet)
library(pROC)

# Create the BigQuery connection
#con <- dbConnect(
#  bigrquery::bigquery(),
#  project = "physionet-data",
#  dataset = "mimiciii_clinical",
#  billing = "clinical-entity-extraction"
#)

setwd("~/Github/A-tool-for-analysis-and-prediction-of-the-MIMIC-III-data")
source("~/Github/A-tool-for-analysis-and-prediction-of-the-MIMIC-III-data/src/sepsis3.R")

sepsis3_basic <- sepsis3
columns_to_remove <- c("excluded", "suspected_infection_time_poe")
sepsis3_basic <- sepsis3[, !(names(sepsis3) %in% columns_to_remove)]


# Impute the mean
imputar_media <- function(sepsis3_basic) {
  for (coluna in names(sepsis3_basic)) {
    if (is.numeric(sepsis3_basic[[coluna]])) {  # Verifica se a coluna é numérica
      media_coluna <- mean(sepsis3_basic[[coluna]], na.rm = TRUE)  # Calcula a média excluindo valores ausentes
      sepsis3_basic[[coluna]][is.na(sepsis3_basic[[coluna]])] <- media_coluna  # Imputa a média
    }
  }
  return(sepsis3_basic)
}


dados_imputados <- imputar_media(sepsis3_basic)
sepsis3_basic <- dados_imputados
#summary(sepsis3_basic)

sepsis_predf <- sepsis3_basic

stats <- sepsis_predf
stats <- stats[, !names(stats) %in% c('icustay_id', 'hadm_id', 'ethnicity', 'gender')]
selectF <- stats[, !names(stats) %in% c('HOSPITAL_EXPIRE_FLAG', 'THIRTYDAY_EXPIRE_FLAG')]

#sepsis3_basic <- sepsis3_basic[complete.cases(sepsis3_basic), ]

#source("cohort.R")
#source("infoButton.R")

# Get table names from MIMIC-III
#table_names <- dbListTables(con)

# Function to get field names for a given table
#get_table_fields <- function(table) {
#  fields <- dbListFields(con, table)
#  fields <- fields[!grepl("_ID$", fields)]
#  return(fields)
#}

#check_field_in_table <- function(table, field) {
#  fields <- get_table_fields(table)
#  return(field %in% fields)
#}

ui <- dashboardPage(
  dashboardHeader(title = "MIMIC-III"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "home", icon = icon("home")),
      menuItem("Patients", tabName = "patients", icon = icon("hospital-user")),
      menuItem("Admissions", tabName = "admissions", icon = icon("book-medical")),
      menuItem("Diagnoses", tabName = "diagnoses", icon = icon("stethoscope")),
      menuItem("ICU", tabName = "icu", icon = icon("bed")),
      menuItem("Predictions", tabName = "predictions", icon = icon("magnifying-glass-chart"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "predictions",
              tabsetPanel(
                tabPanel("Data Extraction", 
                         fluidRow(
                           box(
                             title = "Select variable of Prediction",
                             width = 12,
                             status = "primary",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             fluidRow(
                               column(width = 6, selectInput("selecPredType", "Select the type of outcome factor", choices = c("", "Mortality", "30-days mortality"))),
                               column(width = 6, selectInput("selecTypeDise", "Select the Type of Disease", choices = c("", "sepsis-3")))
                             )
                           )
                         ),
                         
                         conditionalPanel(
                           condition = "!(input.selecPredType === '' || input.selecTypeDise === '')",
                           fluidRow(
                             box(
                               width = 12,
                               status = "primary",
                               DT::dataTableOutput("sepsis3_table")
                             )
                           )
                         )
                ),
                
                tabPanel("Summary",
                         tabsetPanel(
                           tabPanel("Baseline statistics",
                                    fluidRow(
                                      div(
                                        style = "overflow-y: auto; max-height: 500px;",
                                        box(
                                          width = 12,
                                          status = "primary",
                                          gt_output('table_summary')
                                        )
                                      )
                                    )
                           ),
                           
                           tabPanel("Baseline Statistics with Range and Quartiles", 
                                    fluidRow(
                                      div(
                                        style = "overflow-y: auto; max-height: 500px;",  # Add CSS for scroll and set a max-height
                                        box(
                                          width = 12,
                                          status = "primary",
                                          gt_output('table_summary2')
                                        )
                                      )
                                    )
                           )
                         )
                         
                ),
                
                tabPanel("Features",
                         
                         tabsetPanel(
                           tabPanel("Features Selection",
                                    fluidRow(
                                      div(
                                        box(
                                          style = "max-height: 500px; overflow-y: auto;",
                                          width = 4,
                                          status = "primary",
                                          checkboxGroupInput("features", "Select Features:",
                                                             choices = colnames(selectF), selected = c("urineoutput", "lactate_min", "bun_mean", "Sysbp_min", 
                                                                                                                 "metastatic_cancer", "inr_max", "age", "sodium_max", 
                                                                                                                 "aniongap_max", "creatinine_min", "SpO2_mean")),
                                        ),
                                        
                                        #box(verbatimTextOutput("selectedFeaturesOutput")),
                                        
                                        box(
                                          title = "Feature Correlation",
                                          width = 8,
                                          status = "primary",
                                          fluidRow(
                                            box(
                                              DTOutput("correlation_table"),
                                              width = 12
                                            )
                                          ),
                                          
                                          box(
                                            plotlyOutput("correlation_plot"),
                                            width = 12
                                          )
                                          
                                        )
                                      )
                                    )
                           ),
                           
                           tabPanel("Plots",
                                    
                                    fluidRow(
                                      box(
                                        selectInput("variableHist", "Select variable to visualize", choices = colnames(selectF)),
                                        plotlyOutput("histogramPlot"),
                                        width = 6,
                                        status = "primary"
                                        
                                      ),
                                      
                                      box(
                                        selectInput("variablePieChar", "Select variable to visualize", choices = colnames(selectF)),
                                        plotlyOutput("pie_chart"),
                                        width = 6,
                                        status = "primary"
                                        
                                      ),
                                      
                                      box(
                                        selectInput("variableBarplot", "Select variable to visualize", choices = colnames(selectF)),
                                        plotlyOutput("barplot"),
                                        width = 6,
                                        status = "primary"
                                      ),
                                      
                                      box(
                                        selectInput("variableScatterplot", "Select variable to visualize", choices = colnames(selectF)),
                                        plotlyOutput("scatter_plot"),
                                        width = 6,
                                        status = "primary"
                                      )
                                    )
                           )
                         )
                ),
                
                
                
                tabPanel("Train Model",
                         # Split Data, model selection and model training, model Evaluation
                         fluidRow(
                           box(
                             title = "Select a model to use:",
                             selectInput("Select_model", 
                                         label = NULL, 
                                         choices = c("","XGBoost", "Random Forest", "Logistic Regression"),
                                         
                             ),
                             
                             actionButton("train_button", "Train Model"),
                             solidHeader = TRUE,
                             width = 6,
                             height = "175",
                             status = "primary"
                           ),
                           
                           box(
                             title = "Train/Test Split %",
                             sliderInput(
                               "Slider1",
                               label = NULL,
                               min = 0,
                               max = 100,
                               value = 70
                             ),
                             textOutput("cntTrain"),
                             textOutput("cntTest"),
                             solidHeader = TRUE,
                             width = 6,
                             height = "175",
                             status = "primary"
                           ),
                           
                           conditionalPanel(condition = "input.train_button > 0",
                                            
                                            box(
                                              title = "Model Performance",
                                              solidHeader = TRUE,
                                              width = 12,
                                              status = "primary",
                                              fluidRow(
                                                box(
                                                  textOutput("accuracy_text"),
                                                  textOutput("precision_text"),
                                                  textOutput("recall_text"),
                                                  textOutput("f1_text"),
                                                  textOutput("roc_auc_text"),
                                                  plotOutput("roc_plot")
                                                  #solidHeader = TRUE,
                                                  #status = "primary"
                                                ),
                                                
                                                box(
                                                  verbatimTextOutput("confusion_matrix")
                                                )
                                              )
                                            )
                           )
                         )
                ),
                tabPanel("Model Comparison",
                         fluidRow(
                           box(title = "Selected model:" ,
                               textOutput("selectedModel"),
                               solidHeader = TRUE,
                               width = 6,
                               status = "primary"
                           ),
                           
                           box(
                             selectInput("compare", "Select a model to compare with:", choices = ""),
                             solidHeader = TRUE,
                             width = 6,
                             status = "primary"
                           )
                         ),
                         
                         fluidRow(
                           
                           box(title = "Already trained model Evaluation",
                               solidHeader = TRUE,
                               width = 6,
                               status = "primary",
                               box(
                                 textOutput("accuracy_trained"),
                                 textOutput("precision_trained"),
                                 textOutput("recall_trained"),
                                 textOutput("f1_trained"),
                                 textOutput("roc_auc_trained"),
                                 plotOutput("roc_plot_trained"),
                                 width = 12
                               ),
                              
                              box(
                                verbatimTextOutput("confusion_matrix_trained"),
                                width = 12
                              )
                           ),
                           
                           box(title = "Selected model Evaluation" ,
                             
                             solidHeader = TRUE,
                             width = 6,
                             status = "primary",
                             box(
                               textOutput("accuracy_compare"),
                               textOutput("precision_compare"),
                               textOutput("recall_compare"),
                               textOutput("f1_compare"),
                               textOutput("roc_auc_compare"),
                               plotOutput("roc_plot_compare"),
                               width = 12
                             ),
                             
                             box(
                               verbatimTextOutput("confusion_matrix_compare"),
                               width = 12
                             )
                           )
                         )
                ),
                tabPanel("Make Prediction",
                         fluidRow(
                           box(
                             uiOutput("numeric_inputs"),
                             actionButton("predict_button", "Predict"),
                             style = "max-height: 500px; overflow-y: auto;",
                             solidHeader = TRUE,
                             width = 6,
                             status = "primary"
                           ),
                           
                           box(
                             title = "Prediction Result:",
                             textOutput("prediction_result_box"),
                             solidHeader = TRUE,
                             width = 6,
                             status = "primary"
                           )
                         )
                )
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
  var <- ""
  trained_model <- NULL
  
  selected_features <- reactive({
    input$features
  })
  
  # Data Preparation
  
  output$sepsis3_table <- DT::renderDataTable({
    
    selectedPredType <- input$selecPredType
    selectedTypeDise <- input$selecTypeDise
    
    if(selectedPredType == "Mortality" && selectedTypeDise == "sepsis-3") {
      sepsis_predf <- sepsis_predf %>% select(-THIRTYDAY_EXPIRE_FLAG)  
    }
    
    else if(selectedPredType == "30-days mortality" && selectedTypeDise == "sepsis-3") {
      sepsis_predf <- sepsis_predf %>% select(-HOSPITAL_EXPIRE_FLAG)  
    }
    
    DT::datatable(sepsis_predf, options = list(scrollX = TRUE)) %>%
      formatStyle(names(sepsis_predf), textAlign = 'center')
  })
  
  # Data analysis
  
  #exclude <- c("icustay_id", "hadm_id", "is_male", "THIRTYDAY_EXPIRE_FLAG")
  #sepsis3_statistics <- sepsis3_statistics[, !(names(sepsis3_statistics) %in% exclude)]
  #column_names_c <- as.character(names(sepsis3_statistics))
  #table1 <- 
  #  sepsis3_statistics %>%
  #  tbl_summary(include = column_names_c)
  
  output$table_summary <- render_gt({
    
    selectedPredType <- input$selecPredType
    selectedTypeDise <- input$selecTypeDise
    
    stats_aux <- stats
    
    if(selectedPredType == "Mortality" && selectedTypeDise == "sepsis-3") {
      stats_aux$HOSPITAL_EXPIRE_FLAG <- ifelse(stats_aux$HOSPITAL_EXPIRE_FLAG == 1, "Death", "Alive")
      stats_aux <- stats_aux[, !names(stats_aux) %in% c('THIRTYDAY_EXPIRE_FLAG')]
      var = "HOSPITAL_EXPIRE_FLAG"
    }
    
    else {
      stats_aux$THIRTYDAY_EXPIRE_FLAG <- ifelse(stats_aux$THIRTYDAY_EXPIRE_FLAG == 1, "Death", "Alive")
      stats_aux <- stats_aux[, !names(stats_aux) %in% c('HOSPITAL_EXPIRE_FLAG')]
      var = "THIRTYDAY_EXPIRE_FLAG"
    }
    
    # Increase workspace size
    # options(expressions = 10000)
    
    table_summary <- 
      tbl_summary(
        data = stats_aux,
        by = all_of(var),
        statistic = all_continuous() ~ "{mean} ({sd})"
      ) %>%
      add_p() %>% as_gt()
  })
  
  output$table_summary2 <- render_gt({
    
    selectedPredType <- input$selecPredType
    selectedTypeDise <- input$selecTypeDise
    stats_aux <- stats
    
    if(selectedPredType == "Mortality" && selectedTypeDise == "sepsis-3") {
      stats_aux$HOSPITAL_EXPIRE_FLAG <- ifelse(stats_aux$HOSPITAL_EXPIRE_FLAG == 1, "Death", "Alive")
      var = "HOSPITAL_EXPIRE_FLAG"
    }
    
    else {
      stats_aux$THIRTYDAY_EXPIRE_FLAG <- ifelse(stats_aux$THIRTYDAY_EXPIRE_FLAG == 1, "Death", "Alive")
      var = "THIRTYDAY_EXPIRE_FLAG"
    }
    
    # Increase workspace size
    # options(expressions = 10000)
    
    table_summary <- 
      tbl_summary(
        data = stats_aux,
        by = all_of(var),
        statistic = all_continuous() ~ "{median} ({p25}, {p75}), {min}, {max}"
      ) %>%
      add_p() %>% as_gt()
  })
  
  # Feature Selection
  
  observe({
    features_s <- input$features
    stats_feat <- stats[, features_s]
    numeric_data <- stats_feat[sapply(stats_feat, is.numeric)]
    
    correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
    correlation_matrix <- round(correlation_matrix, 2)
    correlation_df <- as.data.frame(correlation_matrix)
    
    output$correlation_table <- renderDT({
      datatable(
        correlation_df,
        options = list(scrollX = TRUE),
        callback = JS(
          "table.columns().every(function() {
          var column = this;
          var columnIndex = column.index();
          var colData = column.data();
          var colName = colData[0];
          var cellIdx = table.column(columnIndex).data().indexOf(colName);
          table.cell(cellIdx, columnIndex).nodes().to$().css({ 'text-align': 'center' });
        });"
        )
      )
    })
    
    cor_matrix <- cor(numeric_data)
    
    output$correlation_plot <- renderPlotly({
      heatmap <- plot_ly(
        x = colnames(cor_matrix),
        y = colnames(cor_matrix),
        z = cor_matrix,
        type = "heatmap",
        colorscale = "Viridis"
      )
      
      heatmap <- heatmap %>% layout(
        title = "Correlation Matrix",
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
      
      heatmap
    })
  })
  
  ################## Plots ################
  
  selected_variable_hist <- reactive({
    input$variableHist
  })
  
  selected_variable_pie <- reactive({
    input$variablePieChar
  })
  
  selected_variable_barplot <- reactive({
    input$variableBarplot
  })
  
  selected_variable_Scatterplot <- reactive({
    input$variableScatterplot
  })
  
  output$histogramPlot <- renderPlotly({
    selected_col <- input$variableHist 
    hist_data <- selectF[[selected_col]]
    selectedTypeDise <- input$selecTypeDise
    
    title_text <- paste("Histogram of MIMIC-III patients with", selectedTypeDise, "by", selected_col)
    xaxis_label <- paste("Value of", selected_col)
    
    plot_ly(x = ~hist_data, type = "histogram", name = "Histogram") %>%
      layout(
        title = title_text,
        xaxis = list(title = xaxis_label),
        barmode = "group",  # Add this line to group bars
        bargap = 0.2  # Adjust the gap as needed (0.2 for 20% gap)
      )
  })
  
  output$pie_chart <- renderPlotly({
    selected_col <- input$variablePieChar
    data <- selectF
    selectedTypeDise <- input$selecTypeDise
    
    # Define the number of age groups you want for numeric variables
    num_groups_numeric <- 7 
    
    # Check if the variable contains only 0s and 1s
    if (is.numeric(data[[selected_col]]) && all(data[[selected_col]] %in% c(0, 1))) {
      # For binary numeric variables, treat them as categorical
      unique_values <- as.character(unique(data[[selected_col]]))
      
      breaks <- seq_along(unique_values)
      labels <- unique_values
      
      data$group <- factor(as.character(data[[selected_col]]), levels = unique_values)
    } 
    
    else if (is.numeric(data[[selected_col]])) {
      # For other numeric variables, create breaks and labels based on range
      min_val <- min(data[[selected_col]], na.rm = TRUE)
      max_val <- max(data[[selected_col]], na.rm = TRUE)
      
      # Check if min_val and max_val are finite
      if (is.finite(min_val) && is.finite(max_val)) {
        breaks <- seq(min_val, max_val, length.out = num_groups_numeric + 1)
        
        labels <- character(num_groups_numeric)
        for (i in 1:num_groups_numeric) {
          labels[i] <- paste(round(breaks[i], 2), round(breaks[i + 1], 2), sep = "-")
        }
        
        data$group <- cut(data[[selected_col]], breaks = breaks, labels = labels)
      }
    } 
    
    else {
      # For categorical variables, create breaks and labels based on unique values
      unique_values <- unique(data[[selected_col]])
      
      breaks <- seq_along(unique_values)
      labels <- unique_values
      
      data$group <- factor(data[[selected_col]], levels = unique_values)
    }
    
    # Create a table of counts
    var_counts <- table(data$group)
    
    pie_chart_data <- data.frame(Group = as.character(names(var_counts)), Count = as.numeric(var_counts))
    
    title_text <- paste("Pie Chart of MIMIC-III patients with", selectedTypeDise, "by", selected_col)
    
    plot_ly(pie_chart_data, labels = ~Group, values = ~Count, type = "pie") %>%
      layout(title = title_text)
  })
  
  
  output$barplot <- renderPlotly({
    selected_col <- input$variableBarplot
    data <- selectF
    selectedTypeDise <- input$selecTypeDise
    
    # Define the number of age groups you want for numeric variables
    num_groups_numeric <- 7 
    
    # Check if the variable contains only 0s and 1s
    if (is.numeric(data[[selected_col]]) && all(data[[selected_col]] %in% c(0, 1))) {
      # For binary numeric variables, treat them as categorical
      unique_values <- as.character(unique(data[[selected_col]]))
      
      breaks <- seq_along(unique_values)
      labels <- unique_values
      
      data$group <- factor(as.character(data[[selected_col]]), levels = unique_values)
    } 
    
    else if (is.numeric(data[[selected_col]])) {
      # For other numeric variables, create breaks and labels based on range
      min_val <- min(data[[selected_col]], na.rm = TRUE)
      max_val <- max(data[[selected_col]], na.rm = TRUE)
      
      # Check if min_val and max_val are finite
      if (is.finite(min_val) && is.finite(max_val)) {
        breaks <- seq(min_val, max_val, length.out = num_groups_numeric + 1)
        
        labels <- character(num_groups_numeric)
        for (i in 1:num_groups_numeric) {
          labels[i] <- paste(round(breaks[i], 2), round(breaks[i + 1], 2), sep = "-")
        }
        
        data$group <- cut(data[[selected_col]], breaks = breaks, labels = labels)
      }
    } 
    
    else {
      # For categorical variables, create breaks and labels based on unique values
      unique_values <- unique(data[[selected_col]])
      
      breaks <- seq_along(unique_values)
      labels <- unique_values
      
      data$group <- factor(data[[selected_col]], levels = unique_values)
    }
    
    # Create a table of counts
    var_counts <- table(data$group)
    
    barplot_data <- data.frame(Group = as.character(names(var_counts)), Count = as.numeric(var_counts))
    
    title_text <- paste("Bar Plot of MIMIC-III patients with", selectedTypeDise, "by", selected_col)
    
    plot_ly(barplot_data, x = ~Group, y = ~Count, type = "bar", marker = list(color = "blue")) %>%
      layout(
        title = title_text,
        xaxis = list(title = selected_col),
        yaxis = list(title = "Frequency")
      )
  })
  
  output$scatter_plot <- renderPlotly({
    selected_col <- input$variableScatterplot
    selectedTypeDise <- input$selecTypeDise
    data <- selectF
    bar_data <- data[[selected_col]]
    
    title_text <- paste("Scatter Plot of MIMIC-III patients with", selectedTypeDise, "by", selected_col)
    
    plot_ly(x = ~bar_data, type = "scatter", mode = "markers", marker = list(color = "blue")) %>%
      layout(
        title = title_text,
        xaxis = list(title = selected_col),
        yaxis = list(title = "Frequency")
      )
  })
  
  #observe({
  #  features_selected <- input$features
  #})
  
  # Observe the changes in selected features
  #output$selectedFeaturesOutput <- renderText({
  #  selected_features <- input$features
  #  if (length(selected_features) > 0) {
  # 
  #    selected_features_string <- paste(selected_features, collapse = ", ")
  #    return(paste("Selected Features: ", selected_features_string))
  #  } else {
  #    return("No features selected")
  #  }
  #})
  
  # Train Model
  
  output$cntTrain <- renderText({
    selected_value <- input$Slider1 # Calculate the train percentage
    paste("Selected Train Percentage:", selected_value, "%")
  })
  
  output$cntTest <- renderText({
    selected_value <- 100 - input$Slider1  # Calculate the test percentage
    paste("Selected Test Percentage:", selected_value, "%")
  })
  
  selected_features <- reactive({
    # Convert selected features to a character vector
    selected_feature_names <- as.character(input$features)
    
    # Create a data frame with only the selected features
    selected_data <- stats[, c(selected_feature_names)]
    
    return(selected_data)
  })
  
  trainedModel <- NULL
  
  # Define a function to train the model
  train_model <- function(model_to_train, selectedF_aux, split_percentage) {
    
    set.seed(1502)
    
    selectedPredType <- input$selecPredType
    selectedTypeDise <- input$selecTypeDise
    trained_model <- NULL
    dtrain <- NULL
    dteste <- NULL
    test_labels <- NULL
    selected_data <- selected_features()
    
    if (selectedPredType == "Mortality" && selectedTypeDise == "sepsis-3") {
      
      selectedF_aux$HOSPITAL_EXPIRE_FLAG <- stats$HOSPITAL_EXPIRE_FLAG
      target_variable <- selectedF_aux$HOSPITAL_EXPIRE_FLAG
      
      if (model_to_train == "XGBoost") {
        
        numberOfTrainingSamples <- round(length(target_variable) * split_percentage)
        dataMdoel <- data.matrix(selected_data)
        
        # training data
        train_data <- dataMdoel[1:numberOfTrainingSamples,]
        train_labels <- target_variable[1:numberOfTrainingSamples]
        
        # testing data
        test_data <- dataMdoel[-(1:numberOfTrainingSamples),]
        test_labels <- target_variable[-(1:numberOfTrainingSamples)]
        
        dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
        dtest <- xgb.DMatrix(data = test_data, label= test_labels)
        
        xgb_model <- xgboost(data = dtrain, nround = 10, objective = "binary:logistic")
        
        trained_model <- xgb_model
      } 
      
      else if (model_to_train == "Random Forest") {
        
        target_variable <- as.factor(target_variable)
        
        selectedF_aux$HOSPITAL_EXPIRE_FLAG = factor(selectedF_aux$HOSPITAL_EXPIRE_FLAG, levels = c(0, 1))
        
        num_samples <- nrow(selectedF_aux)
        num_train_samples <- round(num_samples * split_percentage)
        permuted_indices <- sample(num_samples)
        
        train_indices <- permuted_indices[1:num_train_samples]
        test_indices <- permuted_indices[(num_train_samples + 1):num_samples]
        
        training_set <- selectedF_aux[train_indices, ]
        test_set <- selectedF_aux[test_indices, ]
        
        rf <- randomForest(x = training_set[-ncol(training_set)], 
                           y = training_set$HOSPITAL_EXPIRE_FLAG,
                           ntree = 500, 
                           random_state = 0)
        
        trained_model <- rf
        dtest <- test_set
        
      } 
      
      else if (model_to_train == "Logistic Regression") {
        target_variable <- as.factor(target_variable)
        
        selectedF_aux$HOSPITAL_EXPIRE_FLAG = factor(selectedF_aux$HOSPITAL_EXPIRE_FLAG, levels = c(0, 1))
        
        num_samples <- nrow(selectedF_aux)
        num_train_samples <- round(num_samples * split_percentage)
        permuted_indices <- sample(num_samples)
        
        train_indices <- permuted_indices[1:num_train_samples]
        test_indices <- permuted_indices[(num_train_samples + 1):num_samples]
        
        training_set <- selectedF_aux[train_indices, ]
        test_set <- selectedF_aux[test_indices, ]
        
        model <- glm(HOSPITAL_EXPIRE_FLAG ~ ., data = training_set, family = binomial(link = "logit"))
        
        trained_model <- model
        dtest <- test_set
      } 
      
      
      predictions <- predict(trained_model, dtest, type = "response")
      
      if (model_to_train == "XGBoost") {
        confusion_matrix <- confusionMatrix(data = as.factor(ifelse(predictions > 0.5, 1, 0)), reference = as.factor(test_labels))
        roc <- roc(test_labels, predictions)
      }
      
      else if(model_to_train == "Random Forest") {
        
        confusion_matrix <- confusionMatrix(predictions, dtest$HOSPITAL_EXPIRE_FLAG)
        true_labels <- ifelse(dtest$HOSPITAL_EXPIRE_FLAG == "1", 1, 0)
        roc <- roc(true_labels, as.numeric(predictions))
        
      }
      
      else if (model_to_train == "Logistic Regression") {
        threshold <- 0.5
        binary_predictions <- ifelse(predictions > threshold, 1, 0)
        
        confusion_matrix <- confusionMatrix(data = factor(binary_predictions, levels = c(0, 1)), 
                                            reference = dtest$HOSPITAL_EXPIRE_FLAG)
        
        true_labels <- ifelse(dtest$HOSPITAL_EXPIRE_FLAG == "1", 1, 0)
        roc <- roc(true_labels, as.numeric(predictions))
      }
      
      return(list(
        trained_model = trained_model,
        dtest = dtest,
        test_labels = test_labels,
        confusion_matrix = confusion_matrix,
        predictions = predictions,
        roc = roc
      ))
    } 
    
    else if (selectedPredType == "30-days mortality" && selectedTypeDise == "sepsis-3") {
      target_variable <- stats$THIRTYDAY_EXPIRE_FLAG
      selectedF_aux$THIRTYDAY_EXPIRE_FLAG <- stats$THIRTYDAY_EXPIRE_FLAG
      
      if (model_to_train == "XGBoost") {
        
        numberOfTrainingSamples <- round(length(target_variable) * split_percentage)
        dataMdoel <- data.matrix(selected_data)
        
        # training data
        train_data <- dataMdoel[1:numberOfTrainingSamples,]
        train_labels <- target_variable[1:numberOfTrainingSamples]
        
        # testing data
        test_data <- dataMdoel[-(1:numberOfTrainingSamples),]
        test_labels <- target_variable[-(1:numberOfTrainingSamples)]
        
        dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
        dtest <- xgb.DMatrix(data = test_data, label= test_labels)
        
        xgb_model <- xgboost(data = dtrain, nround = 10, objective = "binary:logistic")
        
        trained_model <- xgb_model
      } 
      
      else if (model_to_train == "Random Forest") {
        target_variable <- as.factor(target_variable)
        selectedF_aux$THIRTYDAY_EXPIRE_FLAG = factor(selectedF_aux$THIRTYDAY_EXPIRE_FLAG, levels = c(0, 1))
        
        num_samples <- nrow(selectedF_aux)
        num_train_samples <- round(num_samples * split_percentage)
        permuted_indices <- sample(num_samples)
        
        train_indices <- permuted_indices[1:num_train_samples]
        test_indices <- permuted_indices[(num_train_samples + 1):num_samples]
        
        training_set <- selectedF_aux[train_indices, ]
        test_set <- selectedF_aux[test_indices, ]
        
        rf <- randomForest(x = training_set[-ncol(training_set)], 
                           y = training_set$THIRTYDAY_EXPIRE_FLAG,
                           ntree = 500, 
                           random_state = 0)
        
        trained_model <- rf
        dtest <- test_set
        
      } 
      
      else if (model_to_train == "Logistic Regression") {
        target_variable <- as.factor(target_variable)
        
        selectedF_aux$THIRTYDAY_EXPIRE_FLAG = factor(selectedF_aux$THIRTYDAY_EXPIRE_FLAG, levels = c(0, 1))
        
        num_samples <- nrow(selectedF_aux)
        num_train_samples <- round(num_samples * split_percentage)
        permuted_indices <- sample(num_samples)
        
        train_indices <- permuted_indices[1:num_train_samples]
        test_indices <- permuted_indices[(num_train_samples + 1):num_samples]
        
        training_set <- selectedF_aux[train_indices, ]
        test_set <- selectedF_aux[test_indices, ]
        
        model <- glm(THIRTYDAY_EXPIRE_FLAG ~ ., data = training_set, family = binomial(link = "logit"))
        
        trained_model <- model
        dtest <- test_set
        
      } 
      
      
      predictions <- predict(trained_model, dtest, type = "response")
      
      if (model_to_train == "XGBoost") {
        confusion_matrix <- confusionMatrix(data = as.factor(ifelse(predictions > 0.5, 1, 0)), reference = as.factor(test_labels))
        roc <- roc(test_labels, predictions)
      }
      
      else if(model_to_train == "Random Forest") {
        
        confusion_matrix <- confusionMatrix(predictions, dtest$THIRTYDAY_EXPIRE_FLAG)
        true_labels <- ifelse(dtest$THIRTYDAY_EXPIRE_FLAG == "1", 1, 0)
        roc <- roc(true_labels, as.numeric(predictions))
      }
      
      else if (model_to_train == "Logistic Regression") {
        threshold <- 0.5
        binary_predictions <- ifelse(predictions > threshold, 1, 0)
        
        confusion_matrix <- confusionMatrix(data = factor(binary_predictions, levels = c(0, 1)), 
                                            reference = dtest$THIRTYDAY_EXPIRE_FLAG)
        
        true_labels <- ifelse(dtest$THIRTYDAY_EXPIRE_FLAG == "1", 1, 0)
        roc <- roc(true_labels, as.numeric(predictions))
      }
      
      return(list(
        trained_model = trained_model,
        dtest = dtest,
        test_labels = test_labels,
        confusion_matrix = confusion_matrix,
        predictions = predictions,
        roc = roc
      ))
      
    }
    
    return(NULL)
  }
  
  evaluateModel <- function(confusion_matrix, roc) {
    accuracy <- confusion_matrix$overall["Accuracy"]
    precision <- confusion_matrix$byClass["Pos Pred Value"]
    recall <- confusion_matrix$byClass["Sensitivity"]
    f1_score <- confusion_matrix$byClass["F1"]
    
    
    output$accuracy_text <- renderText({
      paste("Accuracy:", round(accuracy, 4))
    })
    
    output$precision_text <- renderText({
      paste("Precision:", round(precision, 4))
    })
    
    output$recall_text <- renderText({
      paste("Recall:", round(recall, 4))
    })
    
    output$f1_text <- renderText({
      paste("F1-Score:", round(f1_score, 4))
    })
    
    auc_score <- auc(roc)  # Calculate ROC-AUC
    
    output$roc_auc_text <- renderText({
      paste("ROC-AUC:", round(auc_score, 4))
    })
    
    output$roc_plot <- renderPlot({
      plot(roc, main = "Receiver Operating Characteristic (ROC) Curve")
    })
    
    output$confusion_matrix <- renderPrint({
      confusion_matrix
    })
    
    output$accuracy_trained <- renderText({
      paste("Accuracy:", round(accuracy, 4))
    })
    
    output$precision_trained <- renderText({
      paste("Precision:", round(precision, 4))
    })
    
    output$recall_trained <- renderText({
      paste("Recall:", round(recall, 4))
    })
    
    output$f1_trained <- renderText({
      paste("F1-Score:", round(f1_score, 4))
    })
    
    auc_score <- auc(roc)  # Calculate ROC-AUC
    
    output$roc_auc_trained <- renderText({
      paste("ROC-AUC:", round(auc_score, 4))
    })
    
    output$roc_plot_trained <- renderPlot({
      plot(roc, main = "Receiver Operating Characteristic (ROC) Curve")
    })
    
    output$confusion_matrix_trained <- renderPrint({
      confusion_matrix
    })
    
  }
  
  observeEvent(input$train_button, {
    
    selected_model <- input$Select_model
    selectedF_aux <- selected_features()
    #split_percentage <- input$Slider1 / 100
    split_percentage <- as.numeric(input$Slider1) / 100
    
    training_results <- train_model(selected_model, selectedF_aux, split_percentage)
    trainedModel <<- training_results
    
    if (!is.null(training_results)) {
      trained_model <- training_results$trained_model
      dtest <- training_results$dtest
      test_labels <- training_results$test_labels
      confusion_matrix <- training_results$confusion_matrix
      predictions <- training_results$predictions
      roc <- training_results$roc
      
      if (!is.null(trained_model)) {
        evaluateModel(confusion_matrix, roc)
        
      }
    }
  })
  
  
  # Model Comparison
  
  observe({
    selected_model <- input$Select_model
    
    output$selectedModel <- renderText({
      if (selected_model == "") {
        "No model trained"
      } 
      
      else {
        paste("Selected model:", selected_model)
      }
    })
    
    updateSelectInput(session, "compare", choices = setdiff(c("XGBoost", "Random Forest", "Logistic Regression"), selected_model))
  })
  
  observeEvent(input$compare, {
    selected_model <- input$compare
    selectedF_aux <- selected_features()
    split_percentage <- as.numeric(input$Slider1) / 100
    
    training <- train_model(selected_model, selectedF_aux, split_percentage)
    if (!is.null(training)) {
      # Calculate ROC for the compared model
      roc_compare <- training$roc
      confusion_matrix <- training$confusion_matrix
      
      accuracy <- confusion_matrix$overall["Accuracy"]
      precision <- confusion_matrix$byClass["Pos Pred Value"]
      recall <- confusion_matrix$byClass["Sensitivity"]
      f1_score <- confusion_matrix$byClass["F1"]
      
      # Render metrics for the compared model
      output$accuracy_compare <- renderText({
        paste("Accuracy for", selected_model, ":", round(accuracy, 4))
      })
      
      output$precision_compare <- renderText({
        paste("Precision for", selected_model, ":", round(precision, 4))
      })
      
      output$recall_compare <- renderText({
        paste("Recall for", selected_model, ":", round(recall, 4))
      })
      
      output$f1_compare <- renderText({
        paste("F1-Score for", selected_model, ":", round(f1_score, 4))
      })
      
      # Calculate ROC-AUC for compared model
      auc_score1 <- auc(roc_compare)
      output$roc_auc_compare <- renderText({
        paste("ROC-AUC for", selected_model, ":", round(auc_score1, 4))
      })
      
      roc_main <- paste("ROC Curve for", selected_model)
      
      # Render ROC plot for compared model
      output$roc_plot_compare <- renderPlot({
        plot(roc_compare, main = roc_main)
      })
      
      # Render confusion matrix for compared model
      output$confusion_matrix_compare <- renderPrint({
        confusion_matrix
      })
    }
    
  })
  
  # Model Comparison
  
  observe({
    selected_model <- input$Select_model
    
    output$selectedModel <- renderText({
      if (selected_model == "") {
        "No model trained"
      } 
      
      else {
        paste("Selected model:", selected_model)
      }
    })
    
    updateSelectInput(session, "compare", choices = setdiff(c("XGBoost", "Random Forest", "Logistic Regression"), selected_model))
  })
  
  observeEvent(input$compare, {
    selected_model <- input$compare
    selectedF_aux <- selected_features()
    split_percentage <- as.numeric(input$Slider1) / 100
    
    training <- train_model(selected_model, selectedF_aux, split_percentage)
    if (!is.null(training)) {
      # Calculate ROC for the compared model
      roc_compare <- training$roc
      confusion_matrix <- training$confusion_matrix
      
      accuracy <- confusion_matrix$overall["Accuracy"]
      precision <- confusion_matrix$byClass["Pos Pred Value"]
      recall <- confusion_matrix$byClass["Sensitivity"]
      f1_score <- confusion_matrix$byClass["F1"]
      
      # Render metrics for the compared model
      output$accuracy_compare <- renderText({
        paste("Accuracy for", selected_model, ":", round(accuracy, 4))
      })
      
      output$precision_compare <- renderText({
        paste("Precision for", selected_model, ":", round(precision, 4))
      })
      
      output$recall_compare <- renderText({
        paste("Recall for", selected_model, ":", round(recall, 4))
      })
      
      output$f1_compare <- renderText({
        paste("F1-Score for", selected_model, ":", round(f1_score, 4))
      })
      
      # Calculate ROC-AUC for compared model
      auc_score1 <- auc(roc_compare)
      output$roc_auc_compare <- renderText({
        paste("ROC-AUC for", selected_model, ":", round(auc_score1, 4))
      })
      
      roc_main <- paste("ROC Curve for", selected_model)
      
      # Render ROC plot for compared model
      output$roc_plot_compare <- renderPlot({
        plot(roc_compare, main = roc_main)
      })
      
      # Render confusion matrix for compared model
      output$confusion_matrix_compare <- renderPrint({
        confusion_matrix
      })
    }
    
  })
  
  # Make predictions

  output$numeric_inputs <- renderUI({
    selected_features_list <- input$features
    
    if (is.null(selected_features_list) || length(selected_features_list) == 0) {
      return(NULL)
    }
    
    numeric_inputs <- lapply(selected_features_list, function(feature_name) {
      
      feature_values <- stats[[feature_name]]
      feature_min <- min(feature_values)
      feature_max <- max(feature_values)
      
      numericInput(
        inputId = paste0("input_", feature_name),
        label = feature_name,
        value = feature_min,
        min = feature_min,
        max = feature_max
      )
    })
    
    do.call(tagList, numeric_inputs)
  })
  
  observeEvent(input$predict_button, {
    
    selectedPredType <- input$selecPredType
    selectedTypeDise <- input$selecTypeDise
    pred <- NULL
    
    if (!is.null(trainedModel$trained_model)) {
      input_values <- list()
      
      # Retrieve the values from the numeric inputs
      selected_features_list <- input$features
      for (feature_name in selected_features_list) {
        input_id <- paste0("input_", feature_name)
        input_values[[feature_name]] <- as.numeric(input[[input_id]])
      }
      
      if (input$Select_model == "XGBoost") {
        input_matrix <- do.call(cbind, input_values)
        
        input_data <- xgb.DMatrix(data = input_matrix)
        
        predictions <- predict(trainedModel$trained_model, input_data, type = "response")
        pred <- predictions
        
      } 
      
      else {
        input_val <- as.data.frame(input_values)
        input_tibble <- as_tibble(input_val)
        predictions <- predict(trainedModel$trained_model, input_tibble, type = "response")
        pred <- predictions
      }

      
      output$prediction_result_box <- renderText({

        if (is.null(trainedModel) || is.null(trainedModel$trained_model)) {
          return("Please train a model before making predictions.")
        }
        
        if (selectedPredType == "Mortality" && selectedTypeDise == "sepsis-3") {
          result <- ifelse(predictions >= 0.5, "The model predicts sepsis mortality. Positive (1).", "The model predicts no sepsis mortality. Negative (0).")
        } 
        
        else if (selectedPredType == "30-days mortality" && selectedTypeDise == "sepsis-3") {
          result <- ifelse(predictions >= 0.5, "The model predicts sepsis mortality within 30 days. Positive (1).", "The model predicts no sepsis mortality within 30 days. Negative (0).")
        }
        
        return(result)
      })
      
    }
    
    else {
      output$prediction_result_box <- renderText({
        "Please train a model before making predictions."
      })
    }
  })
  
  session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
  })
  
}

shinyApp(ui, server)
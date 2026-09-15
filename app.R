library(shiny)

find_project_root <- function() {
  candidates <- unique(c(
    getwd(),
    normalizePath(getwd(), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), "."), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE)
  ))
  
  for (cand in candidates) {
    if (dir.exists(file.path(cand, "plots")) ||
        dir.exists(file.path(cand, "tables")) ||
        dir.exists(file.path(cand, "models"))) {
      return(cand)
    }
  }
  
  getwd()
}

project_root <- find_project_root()
plots_dir <- file.path(project_root, "plots")
tables_dir <- file.path(project_root, "tables")
models_dir <- file.path(project_root, "models")

resolve_path <- function(relative_path) {
  file.path(project_root, relative_path)
}

# Plot list
plot_files <- c(
  "Target Distribution Plots" = "plots/target_distribution_plots.png",
  "Sample Variable Histograms" = "plots/sample_variable_histograms.png",
  "Sample Variable Boxplots" = "plots/sample_variable_boxplots.png",
  "All Predictors Boxplot" = "plots/all_predictors_boxplot.png",
  "Correlation Plot Top 20" = "plots/correlation_plot_top20.png",
  "Top 20 Absolute Correlations Barplot" = "plots/top20_absolute_correlations_barplot.png",
  "Top Correlated Scatterplots" = "plots/top_correlated_scatterplots.png",
  "PCA Scree Plot" = "plots/pca_scree_plot.png",
  "Train-Test Target Boxplot" = "plots/train_test_target_boxplot.png",
  "Train-Test Target Histograms" = "plots/train_test_target_histograms.png",
  "RF Top 20 Importance" = "plots/rf_top20_importance.png",
  "Model Comparison Barplots" = "plots/model_comparison_barplots.png",
  "Combined Actual vs Predicted" = "plots/combined_actual_vs_predicted.png",
  "Actual vs Predicted Boxplots" = "plots/actual_vs_pred_boxplots.png",
  "Actual vs Predicted - Baseline" = "plots/actual_vs_pred_baseline.png",
  "Actual vs Predicted - Linear Regression" = "plots/actual_vs_pred_lm.png",
  "Actual vs Predicted - Random Forest" = "plots/actual_vs_pred_rf.png",
  "Actual vs Predicted - ANN" = "plots/actual_vs_pred_ann.png",
  "Residual Analysis - Linear Regression" = "plots/residual_analysis_lm.png",
  "Residual Analysis - Random Forest" = "plots/residual_analysis_rf.png",
  "Residual Analysis - ANN" = "plots/residual_analysis_ann.png"
)

# Table Labels/List
preferred_table_labels <- c(
  "Variable List (TXT)" = "tables/variable_list.txt",
  "EDA Summary Statistics" = "tables/eda_summary_statistics.csv",
  "Correlation with Target" = "tables/correlation_with_target.csv",
  "Common Columns Train vs Unique" = "tables/common_columns_train_unique.csv",
  "Main Only Columns" = "tables/main_only_columns.csv",
  "Unique Only Columns" = "tables/unique_only_columns.csv",
  "Missing Values - Train" = "tables/missing_values_train.csv",
  "Missing Values - Unique_m" = "tables/missing_values_unique_m.csv",
  "NZV Removed Variables" = "tables/nzv_removed_variables.csv",
  "High Correlation Removed Variables" = "tables/high_correlation_removed_variables.csv",
  "Reduced Predictor List Initial" = "tables/reduced_predictor_list_initial.csv",
  "PCA Importance" = "tables/pca_importance.csv",
  "Final LM Variables" = "tables/final_lm_variables.csv",
  "LM Summary Table" = "tables/lm_summary_table.csv",
  "LM Coefficients Table" = "tables/lm_coefficients_table.csv",
  "RF Variable Importance" = "tables/rf_variable_importance.csv",
  "RF Top 10 Importance" = "tables/rf_top10_importance.csv",
  "RF Summary Table" = "tables/rf_summary_table.csv",
  "RF Tuning Results" = "tables/rf_tuning_results.csv",
  "RF Best Tune" = "tables/rf_best_tune.csv",
  "ANN Summary Table" = "tables/ann_summary_table.csv",
  "ANN Tuning Results" = "tables/ann_tuning_results.csv",
  "ANN Best Tune" = "tables/ann_best_tune.csv",
  "Model Comparison Results" = "tables/model_comparison_results.csv",
  "Model Notes" = "tables/model_notes.csv",
  "Train vs Unique_m Summary" = "tables/train_unique_m_summary.csv",
  "Train-Test Sizes" = "tables/train_test_sizes.csv",
  "Residuals and Predictions - LM" = "tables/residuals_predictions_lm.csv",
  "Residuals and Predictions - RF" = "tables/residuals_predictions_rf.csv",
  "Residuals and Predictions - ANN" = "tables/residuals_predictions_ann.csv"
)

preferred_model_labels <- c(
  "Final Linear Model" = "models/final_linear_model.rds",
  "Final Random Forest Model" = "models/final_rf_model.rds",
  "Final ANN Model" = "models/final_ann_model.rds",
  "ANN Preprocess Object" = "models/ann_preprocess.rds",
  "PCA Model" = "models/pca_model.rds"
)

make_pretty_label <- function(path) {
  file_name <- basename(path)
  file_name <- sub("\\.[^.]+$", "", file_name)
  file_name <- gsub("_", " ", file_name)
  tools::toTitleCase(file_name)
}

build_file_choices <- function(folder_path, folder_name, preferred_labels, patterns) {
  existing_relative <- character(0)
  
  if (dir.exists(folder_path)) {
    matched <- unlist(lapply(patterns, function(pat) {
      list.files(folder_path, pattern = pat, full.names = FALSE, ignore.case = TRUE)
    }))
    matched <- unique(matched)
    existing_relative <- file.path(folder_name, matched)
  }
  
  ordered <- preferred_labels[preferred_labels %in% existing_relative]
  remaining <- setdiff(existing_relative, unname(ordered))
  
  if (length(remaining) > 0) {
    extra_named <- setNames(remaining, vapply(remaining, make_pretty_label, character(1)))
    ordered <- c(ordered, extra_named)
  }
  
  if (length(ordered) == 0) {
    ordered <- c("No files found" = "")
  }
  
  ordered
}

table_files <- build_file_choices(
  folder_path = tables_dir,
  folder_name = "tables",
  preferred_labels = preferred_table_labels,
  patterns = c("\\.csv$", "\\.txt$")
)

model_files <- build_file_choices(
  folder_path = models_dir,
  folder_name = "models",
  preferred_labels = preferred_model_labels,
  patterns = c("\\.rds$")
)

safe_read_csv <- function(relative_path) {
  if (identical(relative_path, "") || !nzchar(relative_path)) {
    return(data.frame(Message = "No file selected.", stringsAsFactors = FALSE))
  }
  
  full_path <- resolve_path(relative_path)
  
  if (!file.exists(full_path)) {
    return(data.frame(Message = paste("Missing file:", relative_path), stringsAsFactors = FALSE))
  }
  
  tryCatch(
    read.csv(full_path, stringsAsFactors = FALSE),
    error = function(e) data.frame(Error = e$message, stringsAsFactors = FALSE)
  )
}

safe_read_text <- function(relative_path) {
  if (identical(relative_path, "") || !nzchar(relative_path)) {
    return("No file selected.")
  }
  
  full_path <- resolve_path(relative_path)
  
  if (!file.exists(full_path)) {
    return(paste("Missing file:", relative_path))
  }
  
  tryCatch(
    paste(readLines(full_path, warn = FALSE), collapse = "\n"),
    error = function(e) paste("Could not read file:", e$message)
  )
}

img_or_message <- function(id, relative_path, height = "700px") {
  full_path <- resolve_path(relative_path)
  
  if (file.exists(full_path)) {
    div(
      class = "plot-wrap",
      imageOutput(id, width = "100%", height = height)
    )
  } else {
    div(
      class = "plot-wrap",
      tags$p(style = "color: red;", paste("Missing file:", relative_path))
    )
  }
}

get_image_info <- function(relative_path) {
  full_path <- resolve_path(relative_path)
  list(
    src = normalizePath(full_path, winslash = "/", mustWork = TRUE),
    contentType = "image/png"
  )
}

get_model_summary_text <- function(relative_path) {
  if (identical(relative_path, "") || !nzchar(relative_path)) {
    return("No model file selected.")
  }
  
  full_path <- resolve_path(relative_path)
  
  if (!file.exists(full_path)) {
    return(paste("Missing model file:", relative_path))
  }
  
  obj <- tryCatch(readRDS(full_path), error = function(e) e)
  
  if (inherits(obj, "error")) {
    return(paste("Could not read model file:", obj$message))
  }
  
  lines <- c(
    paste("File:", relative_path),
    paste("Class:", paste(class(obj), collapse = ", ")),
    ""
  )
  
  if (inherits(obj, "lm")) {
    lines <- c(
      lines,
      "Model type: Linear Regression",
      paste("Number of coefficients:", length(coef(obj))),
      "",
      "Formula:",
      paste(deparse(formula(obj)), collapse = " "),
      "",
      "Coefficient names:",
      paste(names(coef(obj)), collapse = ", ")
    )
    return(paste(lines, collapse = "\n"))
  }
  
  if ("train" %in% class(obj)) {
    lines <- c(lines, "Model type: caret train object")
    if (!is.null(obj$method)) lines <- c(lines, paste("Method:", obj$method))
    if (!is.null(obj$modelType)) lines <- c(lines, paste("Model type label:", obj$modelType))
    if (!is.null(obj$metric)) lines <- c(lines, paste("Primary metric:", obj$metric))
    if (!is.null(obj$bestTune)) lines <- c(lines, "", "Best tuning parameters:", capture.output(print(obj$bestTune)))
    if (!is.null(obj$results)) lines <- c(lines, "", "Training results:", capture.output(print(obj$results)))
    if (!is.null(obj$trainingData)) {
      lines <- c(
        lines,
        "",
        paste("Training rows:", nrow(obj$trainingData)),
        paste("Training columns:", ncol(obj$trainingData))
      )
    }
    return(paste(lines, collapse = "\n"))
  }
  
  if ("preProcess" %in% class(obj)) {
    lines <- c(lines, "Object type: ANN preprocessing object")
    if (!is.null(obj$method)) lines <- c(lines, paste("Methods:", paste(obj$method, collapse = ", ")))
    if (!is.null(obj$mean)) lines <- c(lines, paste("Number of centred variables:", length(obj$mean)))
    if (!is.null(obj$std)) lines <- c(lines, paste("Number of scaled variables:", length(obj$std)))
    return(paste(lines, collapse = "\n"))
  }
  
  if (inherits(obj, "prcomp")) {
    lines <- c(lines, "Object type: PCA model")
    if (!is.null(obj$sdev)) {
      prop_var <- (obj$sdev^2) / sum(obj$sdev^2)
      cum_var <- cumsum(prop_var)
      lines <- c(lines, paste("Number of components:", length(obj$sdev)), "", "Variance explained (first 10 PCs):")
      for (i in seq_len(min(10, length(prop_var)))) {
        lines <- c(
          lines,
          paste0(
            "PC", i, ": ",
            round(prop_var[i] * 100, 3), "% | Cumulative: ",
            round(cum_var[i] * 100, 3), "%"
          )
        )
      }
    }
    return(paste(lines, collapse = "\n"))
  }
  
  lines <- c(lines, "Generic object preview:", capture.output(str(obj, max.level = 1)))
  paste(lines, collapse = "\n")
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .plot-wrap {
        overflow-x: auto;
        margin-bottom: 30px;
      }
      .dataTables_wrapper {
        margin-top: 10px;
        margin-bottom: 30px;
      }
      h3 {
        margin-top: 15px;
        margin-bottom: 12px;
      }
      .small-note {
        color: #666666;
        font-size: 12px;
        margin-top: 8px;
      }
    "))
  ),
  
  titlePanel("DS7003: Superconducting Critical Temperature Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Project Summary"),
      p("This app presents the saved outputs from the DS7003 machine learning project."),
      p("Main models: Baseline Mean, Linear Regression, Random Forest, and ANN."),
      p("The layout follows the report structure: EDA, model comparison, diagnostics, and interpretation."),
      div(class = "small-note", paste("Project folder:", normalizePath(project_root, winslash = "/", mustWork = FALSE))),
      width = 4
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          br(),
          h3("Model Comparison Results"),
          dataTableOutput("overview_model_results"),
          br(),
          h3("Dataset Summary"),
          dataTableOutput("overview_dataset_summary"),
          br(),
          h3("Train-Test Sizes"),
          dataTableOutput("overview_train_test"),
          br(),
          h3("Top 10 Random Forest Variables"),
          dataTableOutput("overview_rf_top10"),
          br(),
          h3("Model Notes"),
          dataTableOutput("overview_model_notes")
        ),
        
        tabPanel(
          "EDA",
          br(),
          h3("Target Variable"),
          uiOutput("eda_target_dist"),
          
          h3("Sample Variable Histograms"),
          uiOutput("eda_sample_hist"),
          
          h3("Sample Variable Boxplots"),
          uiOutput("eda_sample_box"),
          
          h3("All Predictor Variables"),
          uiOutput("eda_all_predictors"),
          
          h3("Correlation Plot of Top Variables"),
          uiOutput("eda_corr_top20"),
          
          h3("Top Absolute Correlations"),
          uiOutput("eda_corr_bar"),
          
          h3("Top Correlated Scatterplots"),
          uiOutput("eda_scatter"),
          
          h3("PCA Check"),
          uiOutput("eda_pca"),
          
          h3("Train-Test Target Boxplot"),
          uiOutput("eda_train_test_box"),
          
          h3("Train-Test Target Histograms"),
          uiOutput("eda_train_test_hist")
        ),
        
        tabPanel(
          "Model Comparison",
          br(),
          h3("Overall Metric Comparison"),
          uiOutput("mc_barplots"),
          
          h3("Combined Actual vs Predicted"),
          uiOutput("mc_combined_actual_pred"),
          
          h3("Actual vs Predicted Boxplots"),
          uiOutput("mc_actual_pred_box"),
          
          h3("Individual Actual vs Predicted Plots"),
          uiOutput("mc_actual_pred_baseline"),
          uiOutput("mc_actual_pred_lm"),
          uiOutput("mc_actual_pred_rf"),
          uiOutput("mc_actual_pred_ann")
        ),
        
        tabPanel(
          "Diagnostics",
          br(),
          h3("Residual Analysis: Linear Regression"),
          uiOutput("diag_lm"),
          
          h3("Residual Analysis: Random Forest"),
          uiOutput("diag_rf"),
          
          h3("Residual Analysis: ANN"),
          uiOutput("diag_ann")
        ),
        
        tabPanel(
          "Variable Importance",
          br(),
          h3("Random Forest Importance"),
          uiOutput("vi_rf_top20"),
          
          h3("Top Absolute Correlations"),
          uiOutput("vi_corr_bar"),
          
          h3("Correlation Plot of Top Variables"),
          uiOutput("vi_corr_plot"),
          
          h3("Top 20 Random Forest Variables Table"),
          dataTableOutput("vi_rf_table"),
          
          h3("Top Correlations with Critical Temperature"),
          dataTableOutput("vi_corr_table")
        ),
        
        tabPanel(
          "Tables / Extra Outputs",
          br(),
          selectInput(
            "table_choice",
            "Select a table or text file:",
            choices = table_files,
            selected = unname(table_files[[1]])
          ),
          br(),
          uiOutput("table_panel")
        ),
        
        tabPanel(
          "Saved Models",
          br(),
          selectInput(
            "model_choice",
            "Select a saved model/object:",
            choices = model_files,
            selected = unname(model_files[[1]])
          ),
          br(),
          verbatimTextOutput("model_summary_text")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  render_saved_image <- function(ui_id, image_id, relative_path, height) {
    output[[ui_id]] <- renderUI({
      img_or_message(image_id, relative_path, height)
    })
    
    output[[image_id]] <- renderImage({
      req(file.exists(resolve_path(relative_path)))
      get_image_info(relative_path)
    }, deleteFile = FALSE)
  }
  
  output$overview_model_results <- renderDataTable({
    safe_read_csv("tables/model_comparison_results.csv")
  }, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE))
  
  output$overview_dataset_summary <- renderDataTable({
    safe_read_csv("tables/train_unique_m_summary.csv")
  }, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE))
  
  output$overview_train_test <- renderDataTable({
    safe_read_csv("tables/train_test_sizes.csv")
  }, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE))
  
  output$overview_rf_top10 <- renderDataTable({
    safe_read_csv("tables/rf_top10_importance.csv")
  }, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE))
  
  output$overview_model_notes <- renderDataTable({
    safe_read_csv("tables/model_notes.csv")
  }, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE))
  
  render_saved_image("eda_target_dist", "plot_eda_target_dist", "plots/target_distribution_plots.png", "500px")
  render_saved_image("eda_sample_hist", "plot_eda_sample_hist", "plots/sample_variable_histograms.png", "650px")
  render_saved_image("eda_sample_box", "plot_eda_sample_box", "plots/sample_variable_boxplots.png", "650px")
  render_saved_image("eda_all_predictors", "plot_eda_all_predictors", "plots/all_predictors_boxplot.png", "700px")
  render_saved_image("eda_corr_top20", "plot_eda_corr_top20", "plots/correlation_plot_top20.png", "950px")
  render_saved_image("eda_corr_bar", "plot_eda_corr_bar", "plots/top20_absolute_correlations_barplot.png", "650px")
  render_saved_image("eda_scatter", "plot_eda_scatter", "plots/top_correlated_scatterplots.png", "750px")
  render_saved_image("eda_pca", "plot_eda_pca", "plots/pca_scree_plot.png", "500px")
  render_saved_image("eda_train_test_box", "plot_eda_train_test_box", "plots/train_test_target_boxplot.png", "500px")
  render_saved_image("eda_train_test_hist", "plot_eda_train_test_hist", "plots/train_test_target_histograms.png", "500px")
  
  render_saved_image("mc_barplots", "plot_mc_barplots", "plots/model_comparison_barplots.png", "450px")
  render_saved_image("mc_combined_actual_pred", "plot_mc_combined_actual_pred", "plots/combined_actual_vs_predicted.png", "850px")
  render_saved_image("mc_actual_pred_box", "plot_mc_actual_pred_box", "plots/actual_vs_pred_boxplots.png", "700px")
  render_saved_image("mc_actual_pred_baseline", "plot_mc_actual_pred_baseline", "plots/actual_vs_pred_baseline.png", "500px")
  render_saved_image("mc_actual_pred_lm", "plot_mc_actual_pred_lm", "plots/actual_vs_pred_lm.png", "500px")
  render_saved_image("mc_actual_pred_rf", "plot_mc_actual_pred_rf", "plots/actual_vs_pred_rf.png", "500px")
  render_saved_image("mc_actual_pred_ann", "plot_mc_actual_pred_ann", "plots/actual_vs_pred_ann.png", "500px")
  
  render_saved_image("diag_lm", "plot_diag_lm", "plots/residual_analysis_lm.png", "450px")
  render_saved_image("diag_rf", "plot_diag_rf", "plots/residual_analysis_rf.png", "450px")
  render_saved_image("diag_ann", "plot_diag_ann", "plots/residual_analysis_ann.png", "450px")
  
  render_saved_image("vi_rf_top20", "plot_vi_rf_top20", "plots/rf_top20_importance.png", "850px")
  render_saved_image("vi_corr_bar", "plot_vi_corr_bar", "plots/top20_absolute_correlations_barplot.png", "650px")
  render_saved_image("vi_corr_plot", "plot_vi_corr_plot", "plots/correlation_plot_top20.png", "950px")
  
  output$vi_rf_table <- renderDataTable({
    head(safe_read_csv("tables/rf_variable_importance.csv"), 20)
  }, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE))
  
  output$vi_corr_table <- renderDataTable({
    head(safe_read_csv("tables/correlation_with_target.csv"), 20)
  }, options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE))
  
  output$table_panel <- renderUI({
    if (identical(input$table_choice, "") || !nzchar(input$table_choice)) {
      return(verbatimTextOutput("missing_table_text"))
    }
    
    if (grepl("\\.txt$", input$table_choice, ignore.case = TRUE)) {
      verbatimTextOutput("selected_text_file")
    } else {
      dataTableOutput("selected_table_file")
    }
  })
  
  output$missing_table_text <- renderText({
    if (identical(input$table_choice, "") || !nzchar(input$table_choice)) {
      "No table or text file was found in the tables folder."
    } else {
      paste("Missing table/text file:", input$table_choice)
    }
  })
  
  output$selected_text_file <- renderText({
    safe_read_text(input$table_choice)
  })
  
  output$selected_table_file <- renderDataTable({
    safe_read_csv(input$table_choice)
  }, options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE))
  
  output$model_summary_text <- renderText({
    get_model_summary_text(input$model_choice)
  })
}

shinyApp(ui = ui, server = server)
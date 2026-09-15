# SET WORKING DIRECTORY
setwd(dirname(file.choose()))
getwd()

# LOAD PACKAGES
required_packages <- c(
  "caret",
  "corrplot",
  "psych",
  "car",
  "MASS",
  "ranger",
  "nnet"
)

installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed)) install.packages(pkg, dependencies = TRUE)
}

library(caret)
library(corrplot)
library(psych)
library(car)
library(MASS)
library(ranger)
library(nnet)

set.seed(123)
options(scipen = 999)

# GET DATA
train_file <- "train.csv"
unique_file <- "unique_m.csv"

if (!file.exists(train_file)) stop("train.csv not found in working directory")
if (!file.exists(unique_file)) stop("unique_m.csv not found in working directory")

data <- read.csv(train_file, stringsAsFactors = FALSE)
unique_m <- read.csv(unique_file, stringsAsFactors = FALSE)

# CREATE OUTPUT FOLDERS
dir.create("plots", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)
dir.create("models", showWarnings = FALSE)

# HELPER FUNCTIONS
rmse_value <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}
mae_value <- function(actual, predicted) {
  mean(abs(actual - predicted))
}
r2_value <- function(actual, predicted) {
  sse <- sum((actual - predicted)^2)
  sst <- sum((actual - mean(actual))^2)
  1 - (sse / sst)
}
save_plot <- function(filename, plot_fun, width = 900, height = 700) {
  if (interactive()) {
    plot_fun()
  }
  png(file.path("plots", filename), width = width, height = height)
  plot_fun()
  dev.off()
}
target_var <- "critical_temp"
if (!(target_var %in% names(data))) {
  stop("Target variable 'critical_temp' not found in train.csv")
}

# INITIAL INSPECTION
cat("Main dataset dimensions:", dim(data), "\n")
cat("Supporting dataset dimensions:", dim(unique_m), "\n")

cat("\n--- HEAD OF MAIN DATASET ---\n")
print(head(data))

cat("\n--- HEAD OF SUPPORTING DATASET ---\n")
print(head(unique_m))

cat("\n--- STRUCTURE OF MAIN DATASET ---\n")
str(data)

cat("\n--- STRUCTURE OF SUPPORTING DATASET ---\n")
str(unique_m)

cat("\n--- SUMMARY OF MAIN DATASET ---\n")
print(summary(data))

cat("\n--- SUMMARY OF SUPPORTING DATASET ---\n")
print(summary(unique_m))

cat("\n--- COLUMN NAMES OF MAIN DATASET ---\n")
print(names(data))

cat("\n--- COLUMN NAMES OF SUPPORTING DATASET ---\n")
print(names(unique_m))

# DATA QUALITY CHECKS
main_missing <- colSums(is.na(data))
unique_missing <- colSums(is.na(unique_m))

main_duplicates <- sum(duplicated(data))
unique_duplicates <- sum(duplicated(unique_m))

cat("\n--- MISSING VALUES PER COLUMN IN MAIN DATASET ---\n")
print(main_missing)
cat("\n--- TOTAL MISSING VALUES IN MAIN DATASET ---\n")
print(sum(main_missing))

cat("\n--- MISSING VALUES PER COLUMN IN SUPPORTING DATASET ---\n")
print(unique_missing)
cat("\n--- TOTAL MISSING VALUES IN SUPPORTING DATASET ---\n")
print(sum(unique_missing))

cat("\n--- DUPLICATE ROWS IN MAIN DATASET ---\n")
print(main_duplicates)

cat("\n--- DUPLICATE ROWS IN SUPPORTING DATASET ---\n")
print(unique_duplicates)

if (main_duplicates > 0) {
  data <- data[!duplicated(data), ]
  cat("\nDuplicates removed from main dataset. New dimensions:\n")
  print(dim(data))
}
if (unique_duplicates > 0) {
  unique_m <- unique_m[!duplicated(unique_m), ]
  cat("\nDuplicates removed from supporting dataset. New dimensions:\n")
  print(dim(unique_m))
}
if (sum(main_missing) > 0) {
  data <- na.omit(data)
  cat("\nMissing rows removed from main dataset. New dimensions:\n")
  print(dim(data))
}
if (sum(unique_missing) > 0) {
  unique_m <- na.omit(unique_m)
  cat("\nMissing rows removed from supporting dataset. New dimensions:\n")
  print(dim(unique_m))
}
non_numeric_main <- names(data)[!sapply(data, is.numeric)]
non_numeric_unique <- names(unique_m)[!sapply(unique_m, is.numeric)]
if (length(non_numeric_main) > 0) {
  cat("\n--- NON-NUMERIC COLUMNS IN MAIN DATASET ---\n")
  print(non_numeric_main)
  stop("Please inspect non-numeric columns in train.csv before proceeding")
}
if (length(non_numeric_unique) > 0) {
  cat("\n--- NON-NUMERIC COLUMNS IN SUPPORTING DATASET ---\n")
  print(non_numeric_unique)
}

# COMPARISON OF TRAIN AND UNIQUE_M
common_cols <- intersect(names(data), names(unique_m))
main_only_cols <- setdiff(names(data), names(unique_m))
unique_only_cols <- setdiff(names(unique_m), names(data))

cat("\n--- DATASET COMPARISON ---\n")
cat("Number of common columns:", length(common_cols), "\n")
cat("Number of main-only columns:", length(main_only_cols), "\n")
cat("Number of unique_m-only columns:", length(unique_only_cols), "\n")

cat("\n--- FIRST COMMON COLUMNS ---\n")
print(head(common_cols, 20))

write.csv(data.frame(common_columns = common_cols),
          "tables/common_columns_train_unique.csv", row.names = FALSE)
write.csv(data.frame(main_only_columns = main_only_cols),
          "tables/main_only_columns.csv", row.names = FALSE)
write.csv(data.frame(unique_only_columns = unique_only_cols),
          "tables/unique_only_columns.csv", row.names = FALSE)
write.csv(data.frame(variable = names(main_missing),
                     missing_count = as.integer(main_missing)),
          "tables/missing_values_train.csv", row.names = FALSE)
write.csv(data.frame(variable = names(unique_missing),
                     missing_count = as.integer(unique_missing)),
          "tables/missing_values_unique_m.csv", row.names = FALSE)

# VARIABLE GROUPING
predictor_vars <- setdiff(names(data), target_var)
grouped_variables <- list(
  number_of_elements = grep("^number_of_elements$", predictor_vars, value = TRUE),
  atomic_mass = grep("atomic_mass", predictor_vars, value = TRUE),
  fie = grep("_fie$", predictor_vars, value = TRUE),
  atomic_radius = grep("atomic_radius", predictor_vars, value = TRUE),
  density = grep("Density", predictor_vars, value = TRUE),
  electron_affinity = grep("ElectronAffinity", predictor_vars, value = TRUE),
  fusion_heat = grep("FusionHeat", predictor_vars, value = TRUE),
  thermal_conductivity = grep("ThermalConductivity", predictor_vars, value = TRUE),
  valence = grep("Valence", predictor_vars, value = TRUE)
)

cat("\n--- VARIABLE GROUPS ---\n")
print(grouped_variables)

writeLines(c(
  "Target variable:",
  target_var,
  "",
  "Predictor variables:",
  predictor_vars
), "tables/variable_list.txt")

# EXPLORATORY DATA ANALYSIS
cat("\n--- TARGET SUMMARY ---\n")
print(summary(data[[target_var]]))

eda_summary <- data.frame(
  variable = names(data),
  mean = sapply(data, mean, na.rm = TRUE),
  sd = sapply(data, sd, na.rm = TRUE),
  min = sapply(data, min, na.rm = TRUE),
  q1 = sapply(data, quantile, probs = 0.25, na.rm = TRUE),
  median = sapply(data, median, na.rm = TRUE),
  q3 = sapply(data, quantile, probs = 0.75, na.rm = TRUE),
  max = sapply(data, max, na.rm = TRUE)
)
write.csv(eda_summary, "tables/eda_summary_statistics.csv", row.names = FALSE)

save_plot(
  "target_distribution_plots.png",
  function() {
    par(mfrow = c(1, 2), mar = c(5, 5, 3, 2))
    hist(data[[target_var]],
         breaks = 40,
         main = "Histogram of Critical Temperature",
         xlab = "Critical Temperature",
         col = "grey80",
         border = "white")
    boxplot(data[[target_var]],
            main = "Boxplot of Critical Temperature",
            ylab = "Critical Temperature",
            col = "grey85")
    par(mfrow = c(1, 1))
  },
  width = 1400,
  height = 600
)

sample_vars <- unique(c(target_var, head(predictor_vars, 4)))
save_plot(
  "sample_variable_histograms.png",
  function() {
    par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
    for (v in sample_vars) {
      hist(data[[v]],
           breaks = 30,
           main = paste("Histogram of", v),
           xlab = v,
           col = "grey80",
           border = "white")
    }
    par(mfrow = c(1, 1))
  },
  width = 1400,
  height = 800
)
save_plot(
  "sample_variable_boxplots.png",
  function() {
    par(mfrow = c(2, 3), mar = c(6, 4, 3, 1))
    for (v in sample_vars) {
      boxplot(data[[v]],
              main = paste("Boxplot of", v),
              ylab = v,
              col = "grey85")
    }
    par(mfrow = c(1, 1))
  },
  width = 1400,
  height = 800
)
save_plot(
  "all_predictors_boxplot.png",
  function() {
    par(mar = c(10, 4, 3, 1))
    boxplot(data[, predictor_vars],
            las = 2,
            col = "grey90",
            main = "All Predictor Variables",
            cex.axis = 0.5)
  },
  width = 1800,
  height = 900
)

# CORRELATION ANALYSIS
cor_all <- cor(data, use = "pairwise.complete.obs")
cor_with_target <- sort(cor_all[, target_var], decreasing = TRUE)

cat("\n--- TOP POSITIVE CORRELATIONS WITH TARGET ---\n")
print(head(cor_with_target, 15))

cat("\n--- TOP NEGATIVE CORRELATIONS WITH TARGET ---\n")
print(tail(cor_with_target, 15))

write.csv(
  data.frame(variable = names(cor_with_target),
             correlation_with_target = as.numeric(cor_with_target)),
  "tables/correlation_with_target.csv",
  row.names = FALSE
)

abs_target_cor <- sort(abs(cor_with_target[names(cor_with_target) != target_var]),
                       decreasing = TRUE)
top20_corr_vars <- names(abs_target_cor)[1:min(20, length(abs_target_cor))]
corr_subset <- cor(data[, c(top20_corr_vars, target_var)],
                   use = "pairwise.complete.obs")
save_plot(
  "correlation_plot_top20.png",
  function() {
    corrplot(corr_subset,
             method = "circle",
             type = "lower",
             tl.cex = 0.7,
             tl.col = "black",
             tl.srt = 45)
  },
  width = 1100,
  height = 950
)
save_plot(
  "top20_absolute_correlations_barplot.png",
  function() {
    par(mar = c(10, 5, 3, 1))
    barplot(rev(abs_target_cor[1:min(20, length(abs_target_cor))]),
            names.arg = rev(names(abs_target_cor[1:min(20, length(abs_target_cor))])),
            horiz = TRUE,
            las = 1,
            col = "grey75",
            main = "Top Absolute Correlations with Critical Temperature",
            xlab = "Absolute Correlation")
  },
  width = 1200,
  height = 800
)
scatter_vars <- names(abs_target_cor)[1:min(4, length(abs_target_cor))]
save_plot(
  "top_correlated_scatterplots.png",
  function() {
    par(mfrow = c(2, 2), mar = c(5, 5, 3, 1))
    for (v in scatter_vars) {
      plot(data[[v]], data[[target_var]],
           main = paste(v, "vs", target_var),
           xlab = v,
           ylab = target_var,
           pch = 16,
           cex = 0.6,
           col = rgb(0, 0, 0, 0.4))
      abline(lm(data[[target_var]] ~ data[[v]]), col = 2, lwd = 2)
    }
    par(mfrow = c(1, 1))
  },
  width = 1400,
  height = 1000
)

# VARIABLE REDUCTION
nzv_indices <- nearZeroVar(data[, predictor_vars])
nzv_vars <- predictor_vars[nzv_indices]

cat("\n--- NEAR ZERO VARIANCE VARIABLES ---\n")
print(nzv_vars)

data_reduced <- data[, !(names(data) %in% nzv_vars), drop = FALSE]

predictors_after_nzv <- setdiff(names(data_reduced), target_var)
corr_matrix <- cor(data_reduced[, predictors_after_nzv], use = "pairwise.complete.obs")

high_corr_indices <- findCorrelation(corr_matrix, cutoff = 0.90)
high_corr_vars <- predictors_after_nzv[high_corr_indices]

cat("\n--- HIGHLY CORRELATED VARIABLES REMOVED (cutoff = 0.90) ---\n")
print(high_corr_vars)

data_reduced <- data_reduced[, !(names(data_reduced) %in% high_corr_vars), drop = FALSE]
final_predictors_initial <- setdiff(names(data_reduced), target_var)

write.csv(data.frame(nzv_removed = nzv_vars),
          "tables/nzv_removed_variables.csv", row.names = FALSE)
write.csv(data.frame(high_correlation_removed = high_corr_vars),
          "tables/high_correlation_removed_variables.csv", row.names = FALSE)
write.csv(data.frame(final_predictors_initial = final_predictors_initial),
          "tables/reduced_predictor_list_initial.csv", row.names = FALSE)

cat("\nOriginal predictor count:", length(predictor_vars), "\n")
cat("After NZV and correlation filtering:", length(final_predictors_initial), "\n")

# PCA CHECK
scaled_for_pca <- scale(data_reduced[, final_predictors_initial])

cat("\n--- KMO TEST ---\n")
kmo_result <- KMO(scaled_for_pca)
print(kmo_result)

pca_model <- prcomp(scaled_for_pca, center = TRUE, scale. = TRUE)
pca_importance <- summary(pca_model)$importance

cat("\n--- PCA IMPORTANCE ---\n")
print(round(pca_importance[, 1:min(10, ncol(pca_importance))], 4))
write.csv(as.data.frame(pca_importance),
          "tables/pca_importance.csv")
explained_var <- pca_importance[2, ]
cumulative_var <- pca_importance[3, ]

save_plot(
  "pca_scree_plot.png",
  function() {
    par(mfrow = c(1, 2), mar = c(5, 5, 3, 1))
    plot(explained_var,
         type = "b",
         pch = 16,
         main = "PCA Scree Plot",
         xlab = "Principal Component",
         ylab = "Proportion of Variance")
    plot(cumulative_var,
         type = "b",
         pch = 16,
         main = "Cumulative Variance Explained",
         xlab = "Principal Component",
         ylab = "Cumulative Proportion")
    abline(h = 0.90, col = 2, lty = 2)
    abline(h = 0.95, col = 4, lty = 2)
    par(mfrow = c(1, 1))
  },
  width = 1200,
  height = 500
)

# TRAIN / TEST SPLIT
set.seed(123)
train_index <- createDataPartition(data_reduced[[target_var]], p = 0.7, list = FALSE)
train_data <- data_reduced[train_index, , drop = FALSE]
test_data <- data_reduced[-train_index, , drop = FALSE]

cat("\n--- TRAIN TEST SPLIT ---\n")
cat("Train dimensions:", dim(train_data), "\n")
cat("Test dimensions:", dim(test_data), "\n")
write.csv(
  data.frame(dataset = c("All", "Train", "Test"),
             n = c(nrow(data_reduced), nrow(train_data), nrow(test_data))),
  "tables/train_test_sizes.csv",
  row.names = FALSE
)

cat("\n--- TARGET SUMMARY: ALL / TRAIN / TEST ---\n")
print(summary(data_reduced[[target_var]]))
print(summary(train_data[[target_var]]))
print(summary(test_data[[target_var]]))

save_plot(
  "train_test_target_boxplot.png",
  function() {
    boxplot(data_reduced[[target_var]],
            train_data[[target_var]],
            test_data[[target_var]],
            names = c("All", "Train", "Test"),
            main = "Critical Temperature Distribution",
            ylab = "Critical Temperature",
            col = c("grey90", "grey80", "grey70"))
  },
  width = 900,
  height = 600
)
save_plot(
  "train_test_target_histograms.png",
  function() {
    par(mfrow = c(1, 3), mar = c(5, 5, 3, 1))
    hist(data_reduced[[target_var]],
         breaks = 30, main = "All Data", xlab = target_var,
         col = "grey85", border = "white")
    hist(train_data[[target_var]],
         breaks = 30, main = "Train Data", xlab = target_var,
         col = "grey80", border = "white")
    hist(test_data[[target_var]],
         breaks = 30, main = "Test Data", xlab = target_var,
         col = "grey75", border = "white")
    par(mfrow = c(1, 1))
  },
  width = 1400,
  height = 450
)

# BASELINE MODEL
mean_baseline <- mean(train_data[[target_var]], na.rm = TRUE)
pred_baseline <- rep(mean_baseline, nrow(test_data))
actual_baseline <- test_data[[target_var]]

cat("\n--- BASELINE MEAN ---\n")
print(mean_baseline)

# LINEAR REGRESSION
lm_predictors <- setdiff(names(train_data), target_var)
lm_data <- train_data[, c(target_var, lm_predictors), drop = FALSE]
lm_data[] <- lapply(lm_data, function(x) {
  x[!is.finite(x)] <- NA
  x
})
lm_data <- na.omit(lm_data)
cat("\n--- LINEAR MODEL DATA DIMENSIONS AFTER CLEANING ---\n")
print(dim(lm_data))
lm_formula_full <- as.formula(
  paste(target_var, "~", paste(setdiff(names(lm_data), target_var), collapse = " + "))
)
lm_full <- lm(lm_formula_full, data = lm_data, na.action = na.fail)

cat("\n--- INITIAL LINEAR MODEL SUMMARY ---\n")
print(summary(lm_full))

reduce_vif <- function(model, threshold = 10, data, target) {
  current_model <- model
  repeat {
    vif_values <- tryCatch(vif(current_model), error = function(e) NULL)
    
    if (is.null(vif_values)) break
    max_vif <- max(vif_values)
    if (max_vif < threshold) break
    var_to_remove <- names(which.max(vif_values))
    cat("Removing due to high VIF:", var_to_remove, " VIF =", max_vif, "\n")
    remaining_vars <- setdiff(names(coef(current_model))[-1], var_to_remove)
    if (length(remaining_vars) == 0) break
    new_formula <- as.formula(
      paste(target, "~", paste(remaining_vars, collapse = " + "))
    )
    current_model <- lm(new_formula, data = data, na.action = na.fail)
  }
  current_model
}
lm_final <- reduce_vif(
  lm_full,
  threshold = 10,
  data = lm_data,
  target = target_var
)
cat("\n--- FINAL LINEAR MODEL AFTER VIF REDUCTION ---\n")
print(summary(lm_final))

final_lm_vars <- names(coef(lm_final))[-1]

cat("\n--- FINAL LINEAR MODEL VARIABLES ---\n")
print(final_lm_vars)

write.csv(data.frame(final_lm_variables = final_lm_vars),
          "tables/final_lm_variables.csv",
          row.names = FALSE)

# LINEAR REGRESSION SUMMARY TABLES
lm_coef_table <- as.data.frame(coef(summary(lm_final)))
lm_coef_table$Variable <- rownames(lm_coef_table)
rownames(lm_coef_table) <- NULL
lm_coef_table <- lm_coef_table[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]

write.csv(
  lm_coef_table,
  "tables/lm_coefficients_table.csv",
  row.names = FALSE
)

# RANDOM FOREST REGRESSION
ctrl_rf <- trainControl(
  method = "cv",
  number = 5
)

rf_grid <- expand.grid(
  mtry = unique(pmax(1, c(
    floor(sqrt(length(final_predictors_initial))),
    floor(length(final_predictors_initial) / 3),
    floor(length(final_predictors_initial) / 2)
  ))),
  splitrule = "variance",
  min.node.size = c(5, 10)
)

set.seed(123)
rf_model <- train(
  x = train_data[, final_predictors_initial, drop = FALSE],
  y = train_data[[target_var]],
  method = "ranger",
  trControl = ctrl_rf,
  tuneGrid = rf_grid,
  importance = "permutation",
  num.trees = 500,
  metric = "RMSE"
)

cat("\n--- RANDOM FOREST MODEL ---\n")
print(rf_model)

rf_importance <- varImp(rf_model, scale = FALSE)$importance
rf_importance_df <- data.frame(
  variable = rownames(rf_importance),
  Overall = rf_importance$Overall,
  row.names = NULL
)
rf_importance_df <- rf_importance_df[order(rf_importance_df$Overall, decreasing = TRUE), ]

cat("\n--- TOP RANDOM FOREST VARIABLE IMPORTANCE ---\n")
print(head(rf_importance_df, 20))

write.csv(
  rf_importance_df,
  "tables/rf_variable_importance.csv",
  row.names = FALSE
)

write.csv(
  head(rf_importance_df, 10),
  "tables/rf_top10_importance.csv",
  row.names = FALSE
)

write.csv(
  rf_model$results,
  "tables/rf_tuning_results.csv",
  row.names = FALSE
)

write.csv(
  rf_model$bestTune,
  "tables/rf_best_tune.csv",
  row.names = FALSE
)

save_plot(
  "rf_top20_importance.png",
  function() {
    par(mar = c(10, 5, 3, 1))
    barplot(
      rev(head(rf_importance_df$Overall, 20)),
      names.arg = rev(head(rf_importance_df$variable, 20)),
      horiz = TRUE,
      las = 1,
      col = "grey75",
      main = "Top 20 Random Forest Variable Importance",
      xlab = "Importance"
    )
  },
  width = 1200,
  height = 900
)

# ANN REGRESSION
preproc_ann <- preProcess(
  train_data[, final_predictors_initial, drop = FALSE],
  method = c("center", "scale")
)

train_ann_x <- predict(preproc_ann, train_data[, final_predictors_initial, drop = FALSE])
test_ann_x <- predict(preproc_ann, test_data[, final_predictors_initial, drop = FALSE])
train_ann <- cbind(train_ann_x, critical_temp = train_data[[target_var]])

ctrl_ann <- trainControl(
  method = "cv",
  number = 5
)

ann_grid <- expand.grid(
  size = c(3, 5, 7),
  decay = c(0.001, 0.01, 0.1)
)

set.seed(123)
ann_model <- train(
  critical_temp ~ .,
  data = train_ann,
  method = "nnet",
  trControl = ctrl_ann,
  tuneGrid = ann_grid,
  linout = TRUE,
  trace = FALSE,
  maxit = 500,
  metric = "RMSE"
)

cat("\n--- ANN MODEL ---\n")
print(ann_model)

write.csv(
  ann_model$results,
  "tables/ann_tuning_results.csv",
  row.names = FALSE
)

write.csv(
  ann_model$bestTune,
  "tables/ann_best_tune.csv",
  row.names = FALSE
)

# PREDICTIONS
lm_test_data <- test_data[, c(target_var, final_lm_vars), drop = FALSE]
lm_test_data[] <- lapply(lm_test_data, function(x) {
  x[!is.finite(x)] <- NA
  x
})
lm_test_data <- na.omit(lm_test_data)

pred_lm <- predict(lm_final, newdata = lm_test_data)
actual_lm <- lm_test_data[[target_var]]

pred_rf <- predict(rf_model, newdata = test_data[, final_predictors_initial, drop = FALSE])
actual_rf <- test_data[[target_var]]

pred_ann <- predict(ann_model, newdata = test_ann_x)
actual_ann <- test_data[[target_var]]

# EVALUATION METRICS
calc_metrics <- function(actual, predicted) {
  mse <- mean((actual - predicted)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(actual - predicted))
  r2 <- r2_value(actual, predicted)
  
  data.frame(
    RMSE = rmse,
    MSE = mse,
    MAE = mae,
    R2 = r2
  )
}

metrics_baseline <- calc_metrics(actual_baseline, pred_baseline)
metrics_lm <- calc_metrics(actual_lm, pred_lm)
metrics_rf <- calc_metrics(actual_rf, pred_rf)
metrics_ann <- calc_metrics(actual_ann, pred_ann)

results_table <- rbind(
  cbind(Model = "Baseline Mean", metrics_baseline),
  cbind(Model = "Linear Regression", metrics_lm),
  cbind(Model = "Random Forest", metrics_rf),
  cbind(Model = "ANN", metrics_ann)
)

results_table <- as.data.frame(results_table, stringsAsFactors = FALSE)
results_table$RMSE <- as.numeric(results_table$RMSE)
results_table$MSE <- as.numeric(results_table$MSE)
results_table$MAE <- as.numeric(results_table$MAE)
results_table$R2 <- as.numeric(results_table$R2)
results_table <- results_table[order(results_table$RMSE), ]

cat("\n--- MODEL COMPARISON RESULTS ---\n")
print(results_table)

write.csv(
  results_table,
  "tables/model_comparison_results.csv",
  row.names = FALSE
)

# HELPERS TO PULL FINAL TEST METRICS
get_model_metrics <- function(model_name, results_df) {
  row <- results_df[results_df$Model == model_name, , drop = FALSE]
  list(
    RMSE = row$RMSE[1],
    MSE = row$MSE[1],
    MAE = row$MAE[1],
    R2 = row$R2[1]
  )
}

lm_metrics <- get_model_metrics("Linear Regression", results_table)
rf_metrics <- get_model_metrics("Random Forest", results_table)
ann_metrics <- get_model_metrics("ANN", results_table)

# FINAL LM SUMMARY TABLE
lm_summary_table <- data.frame(
  Item = c(
    "Library",
    "Main function",
    "Model type",
    "Residual standard error",
    "Multiple R-squared",
    "Adjusted R-squared",
    "Number of predictors retained",
    "Test RMSE",
    "Test MSE",
    "Test MAE",
    "Test R-squared"
  ),
  Details = c(
    "stats, car",
    "lm()",
    "Linear Regression",
    round(summary(lm_final)$sigma, 6),
    round(summary(lm_final)$r.squared, 6),
    round(summary(lm_final)$adj.r.squared, 6),
    length(coef(lm_final)) - 1,
    round(lm_metrics$RMSE, 6),
    round(lm_metrics$MSE, 6),
    round(lm_metrics$MAE, 6),
    round(lm_metrics$R2, 6)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  lm_summary_table,
  "tables/lm_summary_table.csv",
  row.names = FALSE
)

# FINAL RF SUMMARY TABLE
rf_summary_table <- data.frame(
  Item = c(
    "Library",
    "Main function",
    "Model type",
    "Method",
    "Training rows",
    "Number of predictors",
    "Cross-validation",
    "Number of trees",
    "Best mtry",
    "Best splitrule",
    "Best min.node.size",
    "Test RMSE",
    "Test MSE",
    "Test MAE",
    "Test R-squared"
  ),
  Details = c(
    "caret, ranger",
    "train(..., method = 'ranger')",
    "Random Forest",
    "ranger",
    nrow(train_data),
    length(final_predictors_initial),
    "5-fold CV",
    500,
    rf_model$bestTune$mtry,
    as.character(rf_model$bestTune$splitrule),
    rf_model$bestTune$min.node.size,
    round(rf_metrics$RMSE, 6),
    round(rf_metrics$MSE, 6),
    round(rf_metrics$MAE, 6),
    round(rf_metrics$R2, 6)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  rf_summary_table,
  "tables/rf_summary_table.csv",
  row.names = FALSE
)

# FINAL ANN SUMMARY TABLE
ann_summary_table <- data.frame(
  Item = c(
    "Library",
    "Main function",
    "Model type",
    "Method",
    "Training rows",
    "Number of predictors",
    "Cross-validation",
    "Preprocessing",
    "Best size",
    "Best decay",
    "Maximum iterations",
    "Test RMSE",
    "Test MSE",
    "Test MAE",
    "Test R-squared"
  ),
  Details = c(
    "caret, nnet",
    "train(..., method = 'nnet')",
    "Artificial Neural Network",
    "nnet",
    nrow(train_ann),
    length(final_predictors_initial),
    "5-fold CV",
    "center and scale",
    ann_model$bestTune$size,
    ann_model$bestTune$decay,
    500,
    round(ann_metrics$RMSE, 6),
    round(ann_metrics$MSE, 6),
    round(ann_metrics$MAE, 6),
    round(ann_metrics$R2, 6)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  ann_summary_table,
  "tables/ann_summary_table.csv",
  row.names = FALSE
)

save_plot(
  "model_comparison_barplots.png",
  function() {
    par(mfrow = c(1, 3), mar = c(8, 5, 3, 1))
    barplot(results_table$RMSE,
            names.arg = results_table$Model,
            las = 2,
            col = "grey75",
            main = "RMSE by Model",
            ylab = "RMSE")
    barplot(results_table$MAE,
            names.arg = results_table$Model,
            las = 2,
            col = "grey75",
            main = "MAE by Model",
            ylab = "MAE")
    barplot(results_table$R2,
            names.arg = results_table$Model,
            las = 2,
            col = "grey75",
            main = "R-squared by Model",
            ylab = "R-squared")
    par(mfrow = c(1, 1))
  },
  width = 1400,
  height = 450
)

# ACTUAL VS PREDICTED PLOTS
plot_actual_vs_pred <- function(actual, predicted, model_name, file_name) {
  save_plot(
    file_name,
    function() {
      plot(actual, predicted,
           main = paste("Actual vs Predicted -", model_name),
           xlab = "Actual Critical Temperature",
           ylab = "Predicted Critical Temperature",
           pch = 16,
           cex = 0.7,
           col = rgb(0, 0, 0, 0.4))
      abline(0, 1, col = 2, lwd = 2, lty = 2)
    },
    width = 850,
    height = 700
  )
}
plot_actual_vs_pred(actual_baseline, pred_baseline, "Baseline Mean", "actual_vs_pred_baseline.png")
plot_actual_vs_pred(actual_lm, pred_lm, "Linear Regression", "actual_vs_pred_lm.png")
plot_actual_vs_pred(actual_rf, pred_rf, "Random Forest", "actual_vs_pred_rf.png")
plot_actual_vs_pred(actual_ann, pred_ann, "ANN", "actual_vs_pred_ann.png")

save_plot(
  "combined_actual_vs_predicted.png",
  function() {
    par(mfrow = c(2, 2), mar = c(5, 5, 3, 1))
    
    plot(actual_baseline, pred_baseline,
         main = "Baseline Mean",
         xlab = "Actual", ylab = "Predicted",
         pch = 16, cex = 0.7, col = rgb(0, 0, 0, 0.4))
    abline(0, 1, col = 2, lwd = 2, lty = 2)
    
    plot(actual_lm, pred_lm,
         main = "Linear Regression",
         xlab = "Actual", ylab = "Predicted",
         pch = 16, cex = 0.7, col = rgb(0, 0, 0, 0.4))
    abline(0, 1, col = 2, lwd = 2, lty = 2)
    
    plot(actual_rf, pred_rf,
         main = "Random Forest",
         xlab = "Actual", ylab = "Predicted",
         pch = 16, cex = 0.7, col = rgb(0, 0, 0, 0.4))
    abline(0, 1, col = 2, lwd = 2, lty = 2)
    
    plot(actual_ann, pred_ann,
         main = "ANN",
         xlab = "Actual", ylab = "Predicted",
         pch = 16, cex = 0.7, col = rgb(0, 0, 0, 0.4))
    abline(0, 1, col = 2, lwd = 2, lty = 2)
    
    par(mfrow = c(1, 1))
  },
  width = 1400,
  height = 1000
)

save_plot(
  "actual_vs_pred_boxplots.png",
  function() {
    par(mfrow = c(2, 2), mar = c(5, 5, 3, 1))
    
    boxplot(actual_baseline, pred_baseline,
            names = c("Actual", "Predicted"),
            main = "Baseline Mean",
            col = c("grey90", "grey75"))
    
    boxplot(actual_lm, pred_lm,
            names = c("Actual", "Predicted"),
            main = "Linear Regression",
            col = c("grey90", "grey75"))
    
    boxplot(actual_rf, pred_rf,
            names = c("Actual", "Predicted"),
            main = "Random Forest",
            col = c("grey90", "grey75"))
    
    boxplot(actual_ann, pred_ann,
            names = c("Actual", "Predicted"),
            main = "ANN",
            col = c("grey90", "grey75"))
    
    par(mfrow = c(1, 1))
  },
  width = 1200,
  height = 900
)

# RESIDUAL ANALYSIS
save_residual_plots <- function(actual, predicted, model_name, prefix) {
  residual_df <- data.frame(
    actual = actual,
    predicted = predicted,
    residual = actual - predicted
  )
  write.csv(residual_df,
            file.path("tables", paste0("residuals_predictions_", prefix, ".csv")),
            row.names = FALSE)
  cat("\n--- RESIDUAL SUMMARY:", model_name, "---\n")
  print(summary(residual_df$residual))
  
  save_plot(
    paste0("residual_analysis_", prefix, ".png"),
    function() {
      par(mfrow = c(1, 3), mar = c(5, 5, 3, 1))
      
      plot(residual_df$predicted, residual_df$residual,
           main = paste("Residuals vs Predicted -", model_name),
           xlab = "Predicted",
           ylab = "Residuals",
           pch = 16,
           cex = 0.7,
           col = rgb(0, 0, 0, 0.4))
      abline(h = 0, col = 2, lwd = 2, lty = 2)
      
      hist(residual_df$residual,
           breaks = 30,
           main = paste("Residual Histogram -", model_name),
           xlab = "Residuals",
           col = "grey80",
           border = "white")
      rug(residual_df$residual, side = 3)
      
      qqnorm(residual_df$residual,
             main = paste("QQ Plot -", model_name),
             pch = 16,
             cex = 0.7)
      qqline(residual_df$residual, col = 2, lwd = 2)
      
      par(mfrow = c(1, 1))
    },
    width = 1400,
    height = 450
  )
}
save_residual_plots(actual_lm, pred_lm, "Linear Regression", "lm")
save_residual_plots(actual_rf, pred_rf, "Random Forest", "rf")
save_residual_plots(actual_ann, pred_ann, "ANN", "ann")

# MODEL NOTES
model_notes <- data.frame(
  Model = c("Baseline Mean", "Linear Regression", "Random Forest", "ANN"),
  Strength = c(
    "Simple benchmark for judging improvement",
    "Most interpretable supervised model",
    "Captures complex non-linear relationships and gives variable importance",
    "Flexible non-linear model after scaling"
  ),
  Weakness = c(
    "No feature learning or structure",
    "Sensitive to multicollinearity and linearity assumptions",
    "Less interpretable than linear regression",
    "Sensitive to tuning and less interpretable"
  )
)
write.csv(model_notes, "tables/model_notes.csv", row.names = FALSE)

# TRAIN AND UNIQUE_M SUMMARY
train_unique_summary <- data.frame(
  item = c(
    "train_rows",
    "train_columns",
    "unique_m_rows",
    "unique_m_columns",
    "common_columns",
    "main_only_columns",
    "unique_only_columns"
  ),
  value = c(
    nrow(data),
    ncol(data),
    nrow(unique_m),
    ncol(unique_m),
    length(common_cols),
    length(main_only_cols),
    length(unique_only_cols)
  )
)
write.csv(train_unique_summary,
          "tables/train_unique_m_summary.csv",
          row.names = FALSE)

# SAVE IMPORTANT MODELS
saveRDS(lm_final, "models/final_linear_model.rds")
saveRDS(rf_model, "models/final_rf_model.rds")
saveRDS(ann_model, "models/final_ann_model.rds")
saveRDS(preproc_ann, "models/ann_preprocess.rds")
saveRDS(pca_model, "models/pca_model.rds")

# FINAL CONSOLE SUMMARY
cat("\n--- FINAL OUTPUTS CREATED ---\n")
cat("tables/variable_list.txt\n")
cat("tables/eda_summary_statistics.csv\n")
cat("tables/correlation_with_target.csv\n")
cat("tables/common_columns_train_unique.csv\n")
cat("tables/main_only_columns.csv\n")
cat("tables/unique_only_columns.csv\n")
cat("tables/nzv_removed_variables.csv\n")
cat("tables/high_correlation_removed_variables.csv\n")
cat("tables/reduced_predictor_list_initial.csv\n")
cat("tables/pca_importance.csv\n")
cat("tables/final_lm_variables.csv\n")
cat("tables/lm_coefficients_table.csv\n")
cat("tables/lm_summary_table.csv\n")
cat("tables/rf_variable_importance.csv\n")
cat("tables/rf_top10_importance.csv\n")
cat("tables/rf_tuning_results.csv\n")
cat("tables/rf_best_tune.csv\n")
cat("tables/rf_summary_table.csv\n")
cat("tables/ann_tuning_results.csv\n")
cat("tables/ann_best_tune.csv\n")
cat("tables/ann_summary_table.csv\n")
cat("tables/model_comparison_results.csv\n")
cat("tables/model_notes.csv\n")
cat("tables/train_unique_m_summary.csv\n")
cat("tables/residuals_predictions_lm.csv\n")
cat("tables/residuals_predictions_rf.csv\n")
cat("tables/residuals_predictions_ann.csv\n")
cat("plots/target_distribution_plots.png\n")
cat("plots/sample_variable_histograms.png\n")
cat("plots/sample_variable_boxplots.png\n")
cat("plots/all_predictors_boxplot.png\n")
cat("plots/correlation_plot_top20.png\n")
cat("plots/top20_absolute_correlations_barplot.png\n")
cat("plots/top_correlated_scatterplots.png\n")
cat("plots/pca_scree_plot.png\n")
cat("plots/train_test_target_boxplot.png\n")
cat("plots/train_test_target_histograms.png\n")
cat("plots/rf_top20_importance.png\n")
cat("plots/model_comparison_barplots.png\n")
cat("plots/combined_actual_vs_predicted.png\n")
cat("plots/actual_vs_pred_boxplots.png\n")
cat("plots/actual_vs_pred_baseline.png\n")
cat("plots/actual_vs_pred_lm.png\n")
cat("plots/actual_vs_pred_rf.png\n")
cat("plots/actual_vs_pred_ann.png\n")
cat("plots/residual_analysis_lm.png\n")
cat("plots/residual_analysis_rf.png\n")
cat("plots/residual_analysis_ann.png\n")
cat("models/final_linear_model.rds\n")
cat("models/final_rf_model.rds\n")
cat("models/final_ann_model.rds\n")
cat("models/ann_preprocess.rds\n")
cat("models/pca_model.rds\n")

# REMOVE ALL VARIABLES FROM THE ENVIRONMENT
rm(list=ls())

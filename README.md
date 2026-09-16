# Superconductivity Critical Temperature Prediction

**Predicting superconducting critical temperature with machine learning in R.**

This project uses material descriptors to predict superconducting critical temperature (`critical_temp`). It compares Linear Regression, Random Forest and an Artificial Neural Network (ANN) against a mean baseline, combining exploratory analysis, feature reduction, model evaluation and interpretation.

Developed for **DS7003 — Advanced Decision Making: Predictive Analytics and Machine Learning**, MSc Data Science, University of East London.

## Main result

Random Forest achieved the strongest performance in the submitted report, with a test **RMSE of 9.773 K** and **R² of 0.919**. The results suggest that flexible models capture relationships in this dataset more effectively than a linear model.

| Model | RMSE (K) | MAE (K) | R² |
|---|---:|---:|---:|
| Random Forest | 9.773 | 5.431 | 0.919 |
| Artificial Neural Network | 14.773 | 9.884 | 0.815 |
| Linear Regression | 20.363 | 15.944 | 0.648 |
| Mean baseline | 34.337 | 29.426 | Approximately 0 |

These are the held-out test results reported in the coursework, not results from a new execution of this repository. Numerical results may vary with software versions and execution environment.

## Dataset

The project uses the UCI Superconductivity Data dataset, contributed by Kam Hamidieh.

| File | Role | Rows | Columns |
|---|---|---:|---:|
| `train.csv` | Main modelling data: 81 predictors and the target | 21,263 | 82 |
| `unique_m.csv` | Supporting composition and material information | 21,263 | 88 |

**`unique_m.csv` is not a test dataset.** The script creates training and test partitions from the main dataset.

Both supplied files have no missing values. The main dataset contains 66 duplicate rows; removing them leaves 21,197 observations. The report describes reducing the 81 predictors to 46 using near-zero variance and high-correlation filtering.

Predictors describe properties such as atomic mass, atomic radius, density, valence, electron affinity, first ionisation energy, fusion heat and thermal conductivity.

Dataset source: [UCI Superconductivity Data](https://archive.ics.uci.edu/dataset/464/superconductivty+data).

Related publication: Hamidieh, K. (2018), *A data-driven statistical model for predicting the critical temperature of a superconductor*, Computational Materials Science, 154, 346–354. [DOI](https://doi.org/10.1016/j.commatsci.2018.07.052).

## Repository files

Keep these files together in the main repository folder:

| File | Purpose |
|---|---|
| `superconductivity-temperature-prediction-R-File.R` | Complete analysis and modelling workflow |
| `train.csv` | Main dataset |
| `unique_m.csv` | Supporting dataset |
| `app.R` | Optional Shiny interface for reviewing generated results |
| `README.md` | Project overview and execution instructions |

Generated outputs are created locally. There is no need to upload plots, tables or saved models to run the analysis from the supplied data.

## Analysis workflow

1. Inspect the datasets, missing values, duplicate records and variable types.
2. Explore target distributions, predictor relationships and correlations.
3. Reduce predictors using near-zero variance and correlation filtering.
4. Use PCA to explore the reduced feature space; PCA components are not the final model inputs.
5. Create a 70:30 training–test split using a random seed of 123.
6. Fit a mean baseline, Linear Regression, Random Forest and ANN.
7. Tune Random Forest and ANN using five-fold cross-validation.
8. Evaluate held-out predictions using RMSE, MSE, MAE and R².
9. Export feature importance, diagnostic plots, result tables and fitted models.

Random Forest uses the `ranger` engine with 500 trees and a tuning grid for `mtry` and minimum node size. The ANN uses `nnet`, with hidden-layer sizes of 3, 5 and 7 and weight-decay values of 0.001, 0.01 and 0.1. ANN predictors are centred and scaled using preprocessing fitted on the training partition.

## How to run

### 1. Prepare the files

Download the repository using **Code → Download ZIP**, then extract it. Keep both CSV files beside the main R script. Install R and open the script in RStudio.

### 2. Install the packages

Run the following in the R console:

```r
install.packages(c(
  "caret", "corrplot", "psych", "car", "MASS",
  "ranger", "nnet", "shiny"
))
```

The modelling script also checks for and installs missing modelling packages. `shiny` is needed only for the optional results viewer. Package versions are not locked, so an identical software environment is not guaranteed.

### 3. Run the modelling script

Open `superconductivity-temperature-prediction-R-File.R` and click **Source** in RStudio.

The first line of setup opens a file picker:

```r
setwd(dirname(file.choose()))
```

**Select `train.csv` from the extracted repository folder.** This sets the working directory to the folder containing both datasets. Let the script finish before launching the app; cross-validation and ANN training can take time.

The script creates these folders automatically:

| Folder | Generated content |
|---|---|
| `tables/` | Data checks, model comparisons, tuning results, importance and prediction tables |
| `plots/` | Exploratory charts, PCA plots, model comparisons and residual diagnostics |
| `models/` | Fitted models and preprocessing objects saved as `.rds` files |

The file picker requires an interactive session. For unattended execution, replace that setup line with an explicit project working directory first.

### 4. Review the results

Open `tables/model_comparison_results.csv` to compare model performance. Additional outputs include Random Forest feature importance, actual-versus-predicted plots and residual diagnostics.

### 5. Launch the optional Shiny app

If `app.R` is included, run this from the project working directory after model training finishes:

```r
shiny::runApp(".")
```

The app reads the generated `tables/`, `plots/` and `models/` folders. It presents existing results; it does not train models or generate missing outputs. Without the completed modelling run, result panels may show missing-file messages.

## Interpretation and limitations

Thermal conductivity, atomic mass, valence and density descriptors were among the influential Random Forest predictors in the report. Feature importance describes model behaviour and does not establish physical causation.

The evaluation uses a random split of one dataset. It does not demonstrate performance on entirely new material families or replace experimental validation. The current workflow performs initial feature filtering before the train–test split, and ANN scaling before cross-validation; a stricter validation design would fit preprocessing separately within each training split and fold. Results should therefore be interpreted as coursework findings rather than an independently validated materials-discovery system.

## Author

**Ramchandra Paudel**  
MSc Data Science, University of East London

[GitHub profile](https://github.com/iamramchandra)

# Pollen-classification-model

R scripts to build and evaluate a Random Forest model for pollen classification using flow cytometry data.

**Publication:** *(link to article)*

**Datasets ready for training (step2 outputs) and trained models (step3 outputs):** *https://doi.org/10.6084/m9.figshare.30870641*

---

## Scripts

| File | Description |
|------|-------------|
| [Step1_FCStoCSV.R](Step1_FCStoCSV.R) | Converts raw cytometer FCS files to CSV format |
| [Step2_PrepData_genus.R](Step2_PrepData_genus.R) | Prepares training data at genus level |
| [Step2_PrepData_species.R](Step2_PrepData_species.R) | Prepares training data at species level |
| [Step3_RF.R](Step3_RF.R) | Trains and tests the Random Forest model |
| [Step4_performance_metrics.R](Step4_performance_metrics.R) | Calculates and plots model performance metrics (F1-score) |
| [Contribution_Variables.r](Contribution_Variables.r) | Computes variable importance for the model |
| [Graph_distribution.r](Graph_distribution.r) | Plots the distribution of the top contributing variables |
| [Heatmap_confusionmatrices.r](Heatmap_confusionmatrices.r) | Generates heatmaps of the confusion matrices |

## Workflow

`Step1` → `Step2` → `Step3` → `Step4` + visualisation scripts

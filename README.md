# SKPJS_Data_Analysis
R script for modelling and analysis of rolling stock control system data.

📊 Dataset
The dataset consists of sensor readings and alarm records from the Bulgarian Rolling Stock Control System (СКПЖПС).
It includes both locomotive and wagon data, covering multiple checkpoints (Пзк, Стм, Ткл, Сп) and various alarm types (gauge deviations, axle overloads, hot bearings, etc.).

Data are aggregated from two primary Excel sources:
СКПЖПС Вагони.xlsx
СКПЖПС Локомотиви.xlsx

Auxiliary R scripts enrich the dataset with reference information:
loc_goods.R – mapping between locomotives and freight types;
loc_years.R – manufacturing years of locomotives;
vag_years.R – manufacturing years of wagons;
pjps_speed.R – operational speed reference by rolling stock type.

📂 Project Structure
📁 SKPJS_Data_Analysis/
│
├── main_script.R       # Full R workflow for data preprocessing, modeling and evaluation
├── helpers/
│   ├── loc_goods.R
│   ├── loc_years.R
│   ├── vag_years.R
│   └── pjps_speed.R
└── README.md

⚙️ Workflow Overview
🧱 1. Data Loading & Preprocessing

Description:
Import of the two Excel datasets (wagons and locomotives), unification of structure, cleanup of invalid or missing entries, normalization of column names, and initial data transformation.

Key steps:
Merge sources via bind_rows();
Extract and standardize checkpoint locations;
Derive fields: Rolling stock type, Vehicle type, Axle load, Gauge deviation, Hotbox temperature;
Impute missing values proportionally by observed frequency.

🚨 2. Alarm Event Identification
Description:
Filtering observations with triggered alarm conditions and labeling them by severity.
Numeric thresholds are applied to identify abnormal sensor readings.

Key steps:
Binary encoding of alarm presence (0/1);
Separate datasets created for each alarm category;
Exploratory visualization of alarm frequency per checkpoint.

🧩 3. Feature Enrichment
Description:
Incorporates external reference data on manufacturing year, operational speed, and locomotive usage class to extend the feature set for modeling.

Key steps:
Join auxiliary R data sources;
Compute derived variables and lag indicators;
Normalize numeric features and apply encoding for categorical ones.

🤖 4. Classification Modeling
Description:
Seven supervised learning algorithms are trained and compared to predict alarm events.
Each model uses the same preprocessed feature matrix, evaluated via 5-fold cross-validation and ROC–AUC metrics.

Models:
Logistic Regression
XGBoost
Random Forest
Decision Tree
Support Vector Machine (SVM)
k-Nearest Neighbors (kNN)
Multilayer Perceptron (MLP, caret::nnet)

Outputs:
Confusion matrices, ROC curves, feature importance plots
Aggregated model performance summary (AUC, Accuracy, F1, Recall, Precision)

🧠 5. Hypothesis Testing & Feature Significance
Description:
Statistical testing of key predictors to assess their significance in triggering alarm events.
Coefficients are interpreted to identify the most influential parameters for predictive accuracy.

📦 R Libraries Used
tidyverse, caret, xgboost, randomForest, rpart, glmnet, nnet, pROC, DiagrammeR, PRROC, lubridate, forcats, broom

🔬 Key Insights
Alarm frequency varies significantly by checkpoint and rolling stock type.
Axle load and side-gauge deviations are strong predictors of mechanical anomalies.
Nonlinear models (XGBoost, MLP) outperform linear baselines in precision and recall.
Feature enrichment using manufacturing year and speed improves classification performance.

🧰 Environment Setup
This project uses the [`renv`](https://rstudio.github.io/renv/) package to ensure a fully reproducible R environment.  
All package versions used in the analysis are stored in the `renv.lock` file.

To recreate the exact setup on another system:

# 1️⃣ Install renv if not already installed
install.packages("renv")

# 2️⃣ Restore the project environment
renv::restore()

📁 Access to Code
The full R script and auxiliary files are available in this repository.
To reproduce the analysis:
source("main_script.R")

✍️ Author
Sergey Filipov
📍 Technical University – Sofia
📅 2025

🎓 Master’s Thesis Project: Predictive Modelling of Alarm Events in Railway Rolling Stock Systems

🔥 This project demonstrates a complete end-to-end data analysis workflow — from raw railway system data to interpretable machine learning models for predictive maintenance.

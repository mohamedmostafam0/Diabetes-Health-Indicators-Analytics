# Diabetes Health Indicators Analysis

## 📌 Project Overview
This project applies **Big Data Analytics** techniques to the **CDC Diabetes Health Indicators Dataset (BRFSS 2015)**. The goal is to identify key lifestyle and demographic risk factors for diabetes and build predictive models to stratify risk without invasive clinical tests.

The analysis is structured into a modular pipeline covering:
1.  **Data Understanding & Preprocessing**
2.  **Hypothesis Testing** (Statistical validation of risk factors)
3.  **Exploratory Data Analysis (EDA)** (Visualization of patterns)
4.  **Modeling** (Predictive analytics using Machine Learning)

## 📂 Dataset
*   **Source**: [UCI Machine Learning Repository](https://archive.ics.uci.edu/dataset/891/cdc+diabetes+health+indicators)
*   **Description**: 253,680 survey responses from the Behavioral Risk Factor Surveillance System (BRFSS).
*   **Key Attributes**:
    *   **Target**: `Diabetes_012` (0=Healthy, 1=Pre-diabetic, 2=Diabetic)
    *   **Features**: `HighBP`, `HighChol`, `BMI`, `Smoker`, `PhysActivity`, `GeneraHealth`, `Age`, `Education`, etc.

## 🛠️ Project Structure
```
├── Dataset/
│   └── diabetes_012_health_indicators_BRFSS2015.csv  # Raw dataset
├── models/                                           # Saved ML models (.rds)
├── outputs/
│   └── plots/                                        # Generated visualizations
├── scripts/
│   ├── 0_setup.R                # Environment setup & libraries
│   ├── 1_data_understanding.R   # Data loading & summary stats
│   ├── 2_data_preprocessing.R   # Cleaning & feature engineering
│   ├── 3_hypothesis_testing.R   # Statistical tests (Chi-square, Kruskal-Wallis)
│   ├── 4_eda_visualization.R    # ggplot2 visualizations
│   ├── 5_modeling.R             # ML models (Apriori, ID3, K-means, Logistic Reg)
│   └── project.R                # Orchestrator script to run the full pipeline
└── README.md
```

## 🚀 Usage

### Prerequisites
*   **R** (version 4.0 or higher)
*   Required packages: `tidyverse`, `caret`, `rpart`, `e1071`, `arules`, `ROSE`, etc.
    *   *Note: First-time setup handles automatic package installation in `0_setup.R`.*

### Running the Analysis
You can execute the entire pipeline using the master script:

```r
# Open the project in RStudio or run from terminal
Rscript scripts/project.R
```

This will sequentially:
1.  Load and clean the data.
2.  Run statistical tests for defined hypotheses.
3.  Generate plots in `outputs/plots/`.
4.  Train and evaluate models, printing accuracies and saving objects to `models/`.

## 🔬 Methodology

### 1. Hypothesis Testing
We rigorously tested 6 hypotheses to validate risk factors:
*   **H1**: High Blood Pressure $\leftrightarrow$ Diabetes (Chi-square)
*   **H2**: BMI differences across groups (Kruskal-Wallis)
*   **H3**: Physical Activity $\leftrightarrow$ Reduced Risk (Chi-square)
*   **H4**: Age Trend (Kruskal-Wallis)
*   **H5**: Education Level $\leftrightarrow$ Diabetes Risk (Chi-square)
*   **H6**: High Cholesterol $\leftrightarrow$ Diabetes (Chi-square)

### 2. Modeling Techniques
We implemented four distinct analytics techniques:
1.  **Apriori Algorithm**: Discovered association rules (e.g., `{HighBP=Yes, HighChol=Yes} => {Diabetes=Diabetic}`).
2.  **ID3 Decision Tree**: Built a flow-chart like model for interpretability.
3.  **K-Means Clustering**: Explored natural groupings in the patient population.
4.  **Multinomial Logistic Regression**: Estimated the probability of Pre-diabetes vs. Diabetes.

## 📊 Key Findings
*   **High Blood Pressure** and **High Cholesterol** are the strongest predictors of diabetes.
*   **BMI** shows a clear graded relationship: higher median BMI correlates strictly with diabetes severity.
*   **Socioeconomic status** (Education) plays a protective role; higher education correlates with lower risk.
*   **Age** is a critical non-modifiable risk factor, with prevalence peaking in seniors.

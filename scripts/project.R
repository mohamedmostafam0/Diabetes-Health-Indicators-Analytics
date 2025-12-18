# ============================================================================
#  BIG DATA ANALYTICS PROJECT - MAIN SCRIPT
#  Dataset: CDC Diabetes Health Indicators (BRFSS 2015)
# ============================================================================

rm(list = ls())
cat("Starting Project Execution...\n")

# Use relative paths assuming execution from project root
# Source each file in order
cat("Running 0_setup.R...\n")
source("scripts/0_setup.R")

cat("Running 1_data_understanding.R...\n")
source("scripts/1_data_understanding.R")

cat("Running 2_data_preprocessing.R...\n")
source("scripts/2_data_preprocessing.R")

cat("Running 3_hypothesis_testing.R...\n")
source("scripts/3_hypothesis_testing.R")

cat("Running 4_eda_visualization.R...\n")
source("scripts/4_eda_visualization.R")

cat("Running 5_modeling.R...\n")
source("scripts/5_modeling.R")

cat("\n")
cat("========================================================================\n")
cat(" ALL PROJECT TASKS COMPLETED SUCCESSFULLY \n")
cat("========================================================================\n")

rm(list = ls())

# ------------------------------------------------------------
#  Initialize environment, load libraries, define paths & constants
# ------------------------------------------------------------

# ---- 1. Install packages ----
# install.packages(c(
#   "tidyverse", "VIM", "caret", "rpart", "e1071",
#   "corrplot", "pROC", "arules", "car", "nnet", "ROSE"
# ))

# ---- 2. Load libraries ----
library(tidyverse)    # dplyr, ggplot2, readr, tidyr, etc.
library(caret)        # Data splitting, preprocessing, evaluation
library(rpart)        # Decision Trees (ID3/CART)
library(e1071)        # kmeans (and SVM if needed)
library(corrplot)     # Correlation matrix visualization
library(pROC)         # ROC/AUC for classification eval
library(arules)       # Apriori association rule mining
library(car)          # For LeveneTest for ANOVA
library(ROSE)         # For handling imbalanced data
library(nnet)         # For multinomial logistic regression

# ---- 3. Set Random Seed ----
set.seed(210000)        # Fixed seed for splits, sampling, kmeans init

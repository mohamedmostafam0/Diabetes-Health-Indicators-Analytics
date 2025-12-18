# ============================================================
# 5. MODELING
# Build 4 models: Apriori, ID3 Decision Tree, K-means, Logistic Regression
# Dataset: diabetes_df_specific
# ============================================================

cat("\n========================================\n")
cat("  MODELING PIPELINE\n")
cat("========================================\n\n")

# ---- 5.1 Data Preparation for Modeling ----
cat("5.1 Preparing data for modeling...\n")

# Select features and target
modeling_df <- diabetes_df_specific %>%
  select(Diabetes_012, HighBP, HighChol, BMI, Smoker, PhysActivity,
         Fruits, Veggies, PhysHlth, GenHlth, Age, Education)

# Create binary target for logistic regression (Diabetic vs Non-Diabetic)
modeling_df$Diabetes_Binary <- ifelse(modeling_df$Diabetes_012 == 2, 1, 0)

# Split data: 60% train, 20% val, 20% test
set.seed(210000)
train_index <- createDataPartition(modeling_df$Diabetes_012, p = 0.6, list = FALSE)
train_data <- modeling_df[train_index,]
remaining_data <- modeling_df[-train_index,]

val_index <- createDataPartition(remaining_data$Diabetes_012, p = 0.5, list = FALSE)
val_data <- remaining_data[val_index,]
test_data <- remaining_data[-val_index,]

cat("Train set:", nrow(train_data), "| Val set:", nrow(val_data), "| Test set:", nrow(test_data), "\n\n")

# ---- 5.2 Model 1: Apriori Association Rules ----
cat("5.2 Building Apriori Model...\n")

# Prepare data for apriori (convert to transactions)
apriori_df <- train_data %>%
  select(Diabetes_012, HighBP, HighChol, Smoker, PhysActivity,
         Fruits, Veggies, GenHlth, Age) %>%
  mutate(
    Diabetes = ifelse(Diabetes_012 == 2, "Diabetic",
                      ifelse(Diabetes_012 == 1, "PreDiabetic", "Healthy")),
    HighBP = factor(ifelse(HighBP == 1, "Yes", "No")),
    HighChol = factor(ifelse(HighChol == 1, "Yes", "No")),
    Smoker = factor(ifelse(Smoker == 1, "Yes", "No")),
    PhysActivity = factor(ifelse(PhysActivity == 1, "Yes", "No")),
    Fruits = factor(ifelse(Fruits == 1, "Yes", "No")),
    Veggies = factor(ifelse(Veggies == 1, "Yes", "No")),
    GenHlth = factor(paste0("GenHlth=", GenHlth)),
    Age = factor(paste0("Age=", Age)),
    Diabetes = factor(Diabetes)
  ) %>%
  select(-Diabetes_012)

# Convert to transactions
trans <- as(apriori_df, "transactions")

# Mine rules
apriori_rules <- apriori(trans,
                         parameter = list(supp = 0.01, conf = 0.6, minlen = 2),
                         appearance = list(rhs = c("Diabetes=Diabetic",
                                                   "Diabetes=PreDiabetic",
                                                   "Diabetes=Healthy")))

# Sort by lift and inspect top rules
apriori_rules <- sort(apriori_rules, by = "lift", decreasing = TRUE)
cat("Total rules found:", length(apriori_rules), "\n")
cat("Top 10 rules by lift:\n")
inspect(head(apriori_rules, 10))
cat("\n")


# ---- 5.3 Model 2: ID3 Decision Tree ----
cat("5.3 Building ID3 Decision Tree...\n")

# Create binary target: Healthy (0) vs Diabetic (1=pre-diabetic, 2=diabetic)
train_data$Diabetes_Binary <- ifelse(train_data$Diabetes_012 == 0, "Healthy", "Diabetic")
val_data$Diabetes_Binary <- ifelse(val_data$Diabetes_012 == 0, "Healthy", "Diabetic")
test_data$Diabetes_Binary <- ifelse(test_data$Diabetes_012 == 0, "Healthy", "Diabetic")

train_data$Diabetes_Binary <- as.factor(train_data$Diabetes_Binary)
val_data$Diabetes_Binary <- as.factor(val_data$Diabetes_Binary)
test_data$Diabetes_Binary <- as.factor(test_data$Diabetes_Binary)

# Handle class imbalance with ROSE (balance training data)
train_bal <- ROSE(Diabetes_Binary ~ HighBP + HighChol + BMI + Smoker + PhysActivity +
                    Fruits + Veggies + PhysHlth + GenHlth + Age + Education,
                  data = train_data, seed = 210000)$data

# Train decision tree using rpart (CART algorithm, similar to ID3) on balanced data
tree_model <- rpart(Diabetes_Binary ~ HighBP +
  HighChol +
  BMI +
  Smoker +
  PhysActivity +
  Fruits +
  Veggies +
  PhysHlth +
  GenHlth +
  Age +
  Education,
                    data = train_bal,
                    method = "class",
                    control = rpart.control(minsplit = 20, cp = 0.001))

cat("Plotting decision tree...\n")
par(mar = c(2, 2, 4, 2))  # Reduce margins to fit larger trees
plot(tree_model, uniform = TRUE, compress = TRUE, main = "Decision Tree: Healthy vs Diabetic")
text(tree_model, use.n = TRUE, all = TRUE, cex = 0.6)  # Smaller text for better fit

# Alternative: Use rpart.plot for better visualization (if available)
if (requireNamespace("rpart.plot", quietly = TRUE)) {
  library(rpart.plot)
  rpart.plot(tree_model, main = "Decision Tree: Healthy vs Diabetic",
             extra = 104, box.palette = "RdYlGn", shadow.col = "gray")
}

# Predictions on validation set
tree_pred_val <- predict(tree_model, val_data, type = "class")
tree_conf_val <- confusionMatrix(tree_pred_val, val_data$Diabetes_Binary)

# Predictions on test set
tree_pred <- predict(tree_model, test_data, type = "class")
tree_conf <- confusionMatrix(tree_pred, test_data$Diabetes_Binary)

cat("Decision Tree Validation Accuracy:", round(tree_conf_val$overall['Accuracy'], 4), "\n")
cat("Decision Tree Test Accuracy:", round(tree_conf$overall['Accuracy'], 4), "\n")
print(tree_conf$table)
cat("\n")

# Additional metrics: ROC-AUC, Sensitivity, Specificity, F1 for Decision Tree (binary)
probs <- predict(tree_model, test_data, type = "prob")[, "Diabetic"]
roc_obj <- roc(test_data$Diabetes_Binary, probs)
auc_val <- auc(roc_obj)
cat("Decision Tree Test AUC:", round(as.numeric(auc_val), 4), "\n")
cm_metrics <- confusionMatrix(tree_pred, test_data$Diabetes_Binary, positive = "Diabetic")
print(cm_metrics$byClass[c("Sensitivity", "Specificity", "F1")])
cat("\n")


# ---- 5.4 Model 3: K-means Clustering ----
cat("5.4 Building K-means Clustering Model...\n")

# Select numeric features for clustering
kmeans_df <- diabetes_df_specific %>%
  select(HighBP, HighChol, BMI, Smoker, PhysActivity,
         Fruits, Veggies, PhysHlth) %>%
  scale()

# Determine optimal k using elbow method (k=3 for 3 diabetes categories)
set.seed(210000)
kmeans_model <- kmeans(kmeans_df, centers = 3, nstart = 25, iter.max = 100)

# Add cluster assignments
diabetes_df_specific$Cluster <- kmeans_model$cluster

# Evaluate cluster-diabetes association
cluster_table <- table(Cluster = diabetes_df_specific$Cluster,
                       Diabetes = diabetes_df_specific$Diabetes_012)
cat("Cluster vs Diabetes Status:\n")
print(cluster_table)

# Calculate cluster purity
cluster_purity <- sum(apply(cluster_table, 1, max)) / sum(cluster_table)
cat("Cluster Purity:", round(cluster_purity, 4), "\n\n")

# ---- 5.5 Model 4: Multinomial Logistic Regression ----
cat("5.5 Building Multinomial Logistic Regression...\n")

# Train multinomial logistic regression
logit_model <- multinom(Diabetes_012 ~ HighBP +
  HighChol +
  BMI +
  Smoker +
  PhysActivity +
  Fruits +
  Veggies +
  PhysHlth +
  GenHlth +
  Age +
  Education,
                        data = train_data,
                        maxit = 200)

# Predictions on validation set
logit_pred_val <- predict(logit_model, val_data)
logit_conf_val <- confusionMatrix(logit_pred_val, as.factor(val_data$Diabetes_012))

# Predictions on test set
logit_pred <- predict(logit_model, test_data)
logit_conf <- confusionMatrix(logit_pred, as.factor(test_data$Diabetes_012))

cat("Logistic Regression Validation Accuracy:", round(logit_conf_val$overall['Accuracy'], 4), "\n")
cat("Logistic Regression Test Accuracy:", round(logit_conf$overall['Accuracy'], 4), "\n")
print(logit_conf$table)
cat("\n")

# Macro-F1 for multinomial logistic regression (test set)
macro_f1 <- mean(logit_conf$byClass[, "F1"], na.rm = TRUE)
cat("Multinomial Logistic Regression Macro-F1:", round(macro_f1, 4), "\n\n")

# ---- 5.6 Model Comparison ----
cat("5.6 Model Performance Summary\n")
cat("========================================\n")
cat("Decision Tree Validation Accuracy:      ", round(tree_conf_val$overall['Accuracy'], 4), "\n")
cat("Decision Tree Test Accuracy:            ", round(tree_conf$overall['Accuracy'], 4), "\n")
cat("Logistic Regression Validation Accuracy:", round(logit_conf_val$overall['Accuracy'], 4), "\n")
cat("Logistic Regression Test Accuracy:      ", round(logit_conf$overall['Accuracy'], 4), "\n")
cat("K-means Cluster Purity:                 ", round(cluster_purity, 4), "\n")
cat("Apriori Rules Found:                    ", length(apriori_rules), "\n")
cat("========================================\n\n")

# Save models
cat("Saving models...\n")
dir.create("models", showWarnings = FALSE)
saveRDS(tree_model, "models/decision_tree.rds")
saveRDS(logit_model, "models/logistic_regression.rds")
saveRDS(kmeans_model, "models/kmeans.rds")
saveRDS(apriori_rules, "models/apriori_rules.rds")
cat("Models saved successfully.\n")
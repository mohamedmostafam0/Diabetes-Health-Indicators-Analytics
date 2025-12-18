
glimpse(diabetes_df_specific)

# Missing values
cat("\nMissing values:\n")
print(colSums(is.na(diabetes_df_specific)))

# Range checks
cat("\nRange checks:\n")
cat("- BMI range:", paste(range(diabetes_df_specific$BMI), collapse = " to "), "\n")
cat("- PhysHlth range:", paste(range(diabetes_df_specific$PhysHlth), collapse = " to "), "\n")
cat("- Illogical BMI (<12 or >100):",
    sum(diabetes_df_specific$BMI < 12 | diabetes_df_specific$BMI > 100), "rows\n")

# Binary validation
binary_vars <- c("HighBP", "HighChol", "Smoker", "PhysActivity", "Fruits", "Veggies")
cat("\nBinary variables valid (0/1 only)?\n")
print(sapply(diabetes_df_specific[binary_vars], function(x) all(x %in% c(0, 1))))

# Convert key categorical variables to factors
diabetes_df_specific <- diabetes_df_specific %>%
  mutate(
    GenHlth = as.factor(GenHlth),
    Age = as.factor(Age),
    Education = as.factor(Education),
    Diabetes_012 = as.factor(Diabetes_012)
  )

# Add descriptive label for target
diabetes_df_specific$Diabetes_012 <- as.numeric(as.character(diabetes_df_specific$Diabetes_012))
diabetes_df_specific$Diabetes_Label <- factor(
  diabetes_df_specific$Diabetes_012,
  levels = c(0, 1, 2),
  labels = c("Healthy", "Pre-diabetic", "Diabetic")
)

# Check for duplicates
cat("Duplicated rows:", sum(duplicated(diabetes_df_specific)),"\n")


# Remove duplicate rows
n_before <- nrow(diabetes_df_specific)
diabetes_df_specific <- diabetes_df_specific %>%
  distinct()
n_after <- nrow(diabetes_df_specific)
cat("Removed", n_before - n_after, "duplicate rows.\n")
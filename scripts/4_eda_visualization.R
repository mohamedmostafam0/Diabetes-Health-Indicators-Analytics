# Create output dir
dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)
# ============================================================
# 1. Stacked Bar: HighBP vs Diabetes (H1)
# Justification: Shows proportion of diabetes status within BP groups.
# Observation: Diabetic group has much higher % with HighBP.
# Interpretation: High BP strongly associated with diabetes — supports H1.
# ============================================================
p1 <- ggplot(diabetes_df_specific, aes(x = factor(HighBP), fill = Diabetes_Label)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Healthy" = "skyblue", "Pre-diabetic" = "orange", "Diabetic" = "red")) +
  labs(x = "High Blood Pressure (0=No, 1=Yes)", y = "Proportion", title = "Diabetes Status by High BP") +
  theme_minimal()
ggsave("outputs/plots/1_bar_highbp_diabetes.png", p1, width = 6, height = 4)

# ============================================================
# 2. Boxplot: BMI by Diabetes (H2)
# Justification: Compares BMI distribution across ordered diabetes groups.
# Observation: Median BMI increases: Healthy → Pre-diabetic → Diabetic.
# Interpretation: BMI is a strong, graded risk factor — ideal for ID3 splitting.
# ============================================================
p2 <- ggplot(diabetes_df_specific, aes(x = Diabetes_Label, y = BMI, fill = Diabetes_Label)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Healthy" = "skyblue", "Pre-diabetic" = "orange", "Diabetic" = "red")) +
  labs(x = "Diabetes Status", y = "BMI", title = "BMI Distribution by Diabetes Status") +
  theme_minimal()
ggsave("outputs/plots/2_box_bmi_diabetes.png", p2, width = 6, height = 4)

# ============================================================
# 3. Grouped Bar: % Diabetic by Physical Activity (H3)
# Justification: Directly compares diabetic prevalence in active vs inactive.
# Observation: Lower % diabetic in active group (e.g., 15.9% vs 22.3%).
# Interpretation: Physical activity reduces diabetes risk — supports public health interventions.
# ============================================================
df_h3 <- diabetes_df_specific %>%
  group_by(PhysActivity) %>%
  summarise(pct_diabetic = mean(Diabetes_012 == 2) * 100)

p3 <- ggplot(df_h3, aes(x = factor(PhysActivity), y = pct_diabetic, fill = factor(PhysActivity))) +
  geom_col() +
  scale_fill_manual(values = c("0" = "gray70", "1" = "forestgreen")) +
  labs(x = "Physical Activity (0=No, 1=Yes)", y = "% Diabetic", title = "Diabetic Prevalence by Physical Activity") +
  theme_minimal()
ggsave("outputs/plots/3_bar_physact_diabetes.png", p3, width = 5, height = 4)

# ============================================================
# 4. Line Plot: % Diabetic by Age Group (H4)
# Justification: Shows trend across ordinal age groups.
# Observation: Steady increase in diabetic % with age.
# Interpretation: Age is a strong non-modifiable risk factor — screening should target older adults.
# ============================================================
df_h4 <- diabetes_df_specific %>%
  group_by(Age) %>%
  summarise(pct_diabetic = mean(Diabetes_012 == 2) * 100) %>%
  arrange(Age)

p4 <- ggplot(df_h4, aes(x = Age, y = pct_diabetic, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(x = "Age Group (1=18-24, ..., 13=80+)", y = "% Diabetic", title = "Diabetic Prevalence by Age Group") +
  theme_minimal()
ggsave("outputs/plots/4_line_age_diabetes.png", p4, width = 6, height = 4)


# ============================================================
# 5. Scatter (jitter): PhysHlth vs MentHlth, colored by Diabetes
# Justification: Reveals joint morbidity patterns.
# Observation: Diabetics cluster in high PhysHlth + high MentHlth (poor health days).
# Interpretation: Diabetes associates with both physical and mental health burden.
# ============================================================
p5 <- ggplot(diabetes_df_specific, aes(x = PhysHlth, y = MentHlth, color = Diabetes_Label)) +
  geom_jitter(alpha = 0.3, size = 0.5, width = 0.3, height = 0.3) +
  scale_color_manual(values = c("Healthy" = "skyblue", "Pre-diabetic" = "orange", "Diabetic" = "red")) +
  labs(x = "Poor Physical Health Days", y = "Poor Mental Health Days", title = "Health Days: Physical vs Mental") +
  theme_minimal()
ggsave("outputs/plots/5_scatter_phys_ment.png", p5, width = 6, height = 5)

# ============================================================
# 6. Mosaic Plot (approx. with geom_tile): Smoker vs Diabetes
# Justification: Visualizes association in contingency table.
# Observation: Slightly higher diabetic % in smokers.
# Interpretation: Smoking is a modest risk factor — less impactful than BP or BMI.
# ============================================================
tbl_smoke <- diabetes_df_specific %>%
  count(Smoker, Diabetes_Label) %>%
  group_by(Smoker) %>%
  mutate(pct = n / sum(n) * 100)

p7 <- ggplot(tbl_smoke, aes(x = factor(Smoker), y = pct, fill = Diabetes_Label)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("Healthy" = "skyblue", "Pre-diabetic" = "orange", "Diabetic" = "red")) +
  labs(x = "Smoker (0=No, 1=Yes)", y = "%", title = "Diabetes Status by Smoking Status") +
  theme_minimal()
ggsave("outputs/plots/6_bar_smoker_diabetes.png", p7, width = 5, height = 4)

# ============================================================
# 7. Dodged Bar: % Fruit/Veggie Consumers by Diabetes
# Justification: Tests dietary habit differences.
# Observation: Lower % of fruit/veggie consumers in diabetic group.
# Interpretation: Healthy diet correlates with lower diabetes risk.
# ============================================================
df_diet <- diabetes_df_specific %>%
  pivot_longer(cols = c(Fruits, Veggies), names_to = "Food", values_to = "Consumes") %>%
  group_by(Food, Diabetes_Label) %>%
  summarise(pct = mean(Consumes == 1) * 100)

p8 <- ggplot(df_diet, aes(x = Diabetes_Label, y = pct, fill = Food)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Fruits" = "gold", "Veggies" = "green")) +
  labs(x = "Diabetes Status", y = "% Consumers", title = "Healthy Eating Habits by Diabetes Status") +
  theme_minimal()
ggsave("outputs/plots/7_bar_diet_diabetes.png", p8, width = 6, height = 4)

# ============================================================
# 8. Heatmap: Correlation Matrix
# Justification: Identifies multicollinearity for feature selection.
# Observation: BMI ↔ GenHlth (neg), Age ↔ PhysHlth (pos).
# Interpretation: Avoid highly correlated pairs in regression; OK for tree-based models.
# ============================================================
# Ensure all variables are numeric
num_vars <- diabetes_df_specific %>%
  select(BMI, Age, PhysHlth, MentHlth, GenHlth) %>%
  mutate(
    BMI = as.numeric(BMI),
    Age = as.numeric(Age),
    PhysHlth = as.numeric(PhysHlth),
    MentHlth = as.numeric(MentHlth),
    GenHlth = as.numeric(GenHlth)
  )

cor_mat <- cor(num_vars)

# Convert correlation matrix to long format using tidyverse
cor_long <- cor_mat %>%
  as.data.frame() %>%
  rownames_to_column("Var2") %>%
  pivot_longer(cols = -Var2, names_to = "Var1", values_to = "value")

p9 <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = round(value, 2)), size = 3) + # Round to 2 decimal places
  labs(title = "Correlation Heatmap") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(vjust = 0.5)
  )

ggsave("outputs/plots/8_heatmap_corr.png", p9, width = 5, height = 5)
# ============================================================
# 9. Bar: Univariate Association Strength (Cramer's V)
# Justification: Ranks predictors by association with diabetes (pre-modeling).
# Observation: HighBP, BMI, Age top 3.
# Interpretation: These should be prioritized in ID3/Apriori.
# ============================================================
binary_vars <- c("HighBP", "HighChol", "Smoker", "PhysActivity", "Fruits", "Veggies")
assoc <- tibble(
  variable = binary_vars,
  cramers_v = sapply(binary_vars, function(v) {
    tbl <- table(diabetes_df_specific[[v]], diabetes_df_specific$Diabetes_012)
    chi <- chisq.test(tbl)$statistic
    sqrt(chi / (sum(tbl) * (min(dim(tbl)) - 1)))
  })
) %>% arrange(desc(cramers_v))

p10 <- ggplot(assoc, aes(x = reorder(variable, cramers_v), y = cramers_v)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = "Predictor", y = "Cramer's V (Association Strength)", title = "Univariate Association with Diabetes") +
  theme_minimal()
ggsave("outputs/plots/9_bar_association.png", p10, width = 6, height = 4)

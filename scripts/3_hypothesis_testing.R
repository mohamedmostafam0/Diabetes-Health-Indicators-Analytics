# ============================================================
# H1: People with high blood pressure have higher diabetes rates
# H₀: Proportion of diabetic individuals is same in HighBP=0 and HighBP=1
# H₁: Proportion is higher in HighBP=1
# Test: Chi-square test of independence (2×3 table)
# ============================================================

cat("H1: High Blood Pressure and Diabetes\n")
tbl_h1 <- table(HighBP = diabetes_df_specific$HighBP, Diabetes = diabetes_df_specific$Diabetes_012)
print(tbl_h1)
# Effect size: Diabetic prevalence (%) by HighBP group
pct_diabetic_h1 <- prop.table(tbl_h1, margin = 1)[, "2"] * 100
cat("Diabetic prevalence: HighBP=0 =", round(pct_diabetic_h1["0"], 1), "%, ",
    "HighBP=1 =", round(pct_diabetic_h1["1"], 1), "%\n")

chi_h1 <- chisq.test(tbl_h1)
cat("Chi-square test: X^2 =", round(chi_h1$statistic, 2),
    ", df =", chi_h1$parameter,
    ", p ", format.pval(chi_h1$p.value), "\n")

if (chi_h1$p.value < 0.05) {
  cat("Reject Ho. Significant association between HighBP and diabetes status.\n")
} else {
  cat("Fail to reject Ho. No significant association.\n")
}
cat("---\n")

# ============================================================
# H2: Mean BMI differs across diabetes groups
# H₀: μ_Healthy = μ_Pre = μ_Diabetic
# H₁: At least one mean differs
# Test: Kruskal-Wallis (non-parametric, robust to non-normality)
# ============================================================

cat("🔍 H2: BMI and Diabetes Status\n")
# Check normality
kw_h2 <- kruskal.test(BMI ~ Diabetes_012, data = diabetes_df_specific)
cat("Kruskal-Wallis test: H =", round(kw_h2$statistic, 2),
    ", df =", kw_h2$parameter,
    ", p ", format.pval(kw_h2$p.value), "\n")

if (kw_h2$p.value < 0.05) {
  cat("Reject Ho. BMI distributions differ significantly across groups.\n")
  # Post-hoc: Show group medians
  medians <- diabetes_df_specific %>% group_by(Diabetes_012) %>% summarise(median_BMI = median(BMI))
  cat("Median BMI by group:\n")
  print(medians)
} else {
  cat("Fail to reject Ho. No significant difference in BMI.\n")
}
cat("---\n")

# ============================================================
# H3: Physical activity reduces diabetes risk
# H₀: Proportion diabetic is same in PhysActivity=0 and =1
# H₁: Proportion is lower in PhysActivity=1
# Test: Chi-square (2×3) or focus on Diabetic (2) vs others
# ============================================================

cat("H3: Physical Activity and Diabetes\n")
tbl_h3 <- table(PhysActivity = diabetes_df_specific$PhysActivity, Diabetes = diabetes_df_specific$Diabetes_012)
print(tbl_h3)

# Effect size: Diabetic prevalence (%) by PhysActivity group
pct_diabetic_h3 <- prop.table(tbl_h3, margin = 1)[, "2"] * 100
cat("Diabetic prevalence: Inactive =", round(pct_diabetic_h3["0"], 1), "%, ",
    "Active =", round(pct_diabetic_h3["1"], 1), "%\n")

chi_h3 <- chisq.test(tbl_h3)
cat("Chi-square test: x^2 =", round(chi_h3$statistic, 2),
    ", df =", chi_h3$parameter,
    ", p ", format.pval(chi_h3$p.value), "\n")

if (chi_h3$p.value < 0.05) {
  cat("Reject Ho. Significant association between physical activity and diabetes.\n")
} else {
  cat("Fail to reject Ho.\n")
}
cat("---\n")

# ============================================================
# H4: Diabetes prevalence increases with age
# H₀: No trend between Age (ordinal) and Diabetes_012
# H₁: Prevalence increases with Age group
# Test: Kruskal-Wallis (Age as ordinal predictor)
# ============================================================

cat("H4: Age and Diabetes Status\n")
kw_h4 <- kruskal.test(Age ~ Diabetes_012, data = diabetes_df_specific)
cat("Kruskal-Wallis test: H =", round(kw_h4$statistic, 2),
    ", df =", kw_h4$parameter,
    ", p ", format.pval(kw_h4$p.value), "\n")

if (kw_h4$p.value < 0.05) {
  cat("Reject Ho. Significant association between age and diabetes status.\n")
  # Show trend: % diabetic by age group
  age_risk <- diabetes_df_specific %>%
    group_by(Age) %>%
    summarise(
      n = n(),
      pct_diabetic = mean(Diabetes_012 == 2) * 100
    ) %>%
    arrange(Age)
  cat("Diabetic prevalence (%) by age group:\n")
  print(age_risk)
} else {
  cat("Fail to reject Ho.\n")
}
cat("---\n")

# Additional trend check (optional): Spearman correlation (ordinal to ordinal)
cat("Spearman rank correlation between Age group and Diabetes status (ordinal encodings)\n")
spearman_h4 <- cor.test(as.numeric(diabetes_df_specific$Age),
                        as.numeric(diabetes_df_specific$Diabetes_012),
                        method = "spearman")
cat("Spearman rho =", round(as.numeric(spearman_h4$estimate), 3),
    ", p ", format.pval(spearman_h4$p.value), "\n")
cat("---\n")

# ============================================================
# H5: Higher education is associated with lower diabetes risk
# H₀: Diabetes prevalence is independent of Education level
# H₁: Diabetes prevalence varies by Education level
# Test: Chi-square test
# ============================================================

cat("H5: Education Level and Diabetes\n")
tbl_h5 <- table(Education = diabetes_df_specific$Education, Diabetes = diabetes_df_specific$Diabetes_012)
print(tbl_h5)

pct_diabetic_h5 <- prop.table(tbl_h5, margin = 1)[, "2"] * 100
cat("Diabetic prevalence by Education Level (1=Low, 6=High):\n")
print(round(pct_diabetic_h5, 1))

chi_h5 <- chisq.test(tbl_h5)
cat(
  "Chi-square test: X^2 =", round(chi_h5$statistic, 2),
  ", df =", chi_h5$parameter,
  ", p ", format.pval(chi_h5$p.value), "\n"
)

if (chi_h5$p.value < 0.05) {
  cat("Reject Ho. Significant association between Education and diabetes.\n")
} else {
  cat("Fail to reject Ho.\n")
}
cat("---\n")

# ============================================================
# H6: High Cholesterol increases diabetes risk
# H₀: Proportion of diabetic is same for HighChol=0 and HighChol=1
# H₁: Proportion is higher for HighChol=1
# Test: Chi-square test
# ============================================================

cat("H6: High Cholesterol and Diabetes\n")
tbl_h6 <- table(HighChol = diabetes_df_specific$HighChol, Diabetes = diabetes_df_specific$Diabetes_012)
print(tbl_h6)

pct_diabetic_h6 <- prop.table(tbl_h6, margin = 1)[, "2"] * 100
cat(
  "Diabetic prevalence: NormalChol =", round(pct_diabetic_h6["0"], 1), "%, ",
  "HighChol =", round(pct_diabetic_h6["1"], 1), "%\n"
)

chi_h6 <- chisq.test(tbl_h6)
cat(
  "Chi-square test: X^2 =", round(chi_h6$statistic, 2),
  ", df =", chi_h6$parameter,
  ", p ", format.pval(chi_h6$p.value), "\n"
)

if (chi_h6$p.value < 0.05) {
  cat("Reject Ho. High Cholesterol is significantly associated with diabetes.\n")
} else {
  cat("Fail to reject Ho.\n")
}
cat("---\n")

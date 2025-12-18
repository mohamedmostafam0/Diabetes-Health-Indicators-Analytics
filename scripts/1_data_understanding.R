
# ---- Load Data ----
# Dataset source: https://archive.ics.uci.edu/dataset/891/cdc+diabetes+health+indicators
# Description: CDC BRFSS 2015 survey of U.S. adults — self-reported health indicators
diabetes_df <- read.csv("Dataset/diabetes_012_health_indicators_BRFSS2015.csv")

str(diabetes_df)

summary(diabetes_df[c("BMI", "GenHlth", "MentHlth", "PhysHlth", "Age", "Education", "Income")])

print(paste("Number of Rows:",nrow(diabetes_df)))
print(paste("Number of Columns:", ncol(diabetes_df)))

colnames(diabetes_df)

head(diabetes_df)

diabetes_df %>%
  count(Diabetes_012) %>%
  rename(class = Diabetes_012, percentage = n) %>%
  mutate(percentage = round((percentage / sum(percentage) * 100), 2))

diabetes_df_specific <- diabetes_df %>%
  select(
    HighBP, HighChol, BMI, Smoker, PhysHlth, MentHlth,
    PhysActivity, Fruits, Veggies, GenHlth, Age, Education, Diabetes_012
  )

feature_info <- tribble(
  ~Variable,          ~Type,        ~Encoding,                                              ~Relevance,
  "HighBP",           "Binary",     "1 = Yes, 0 = No",                                      "Strong comorbidity",
  "HighChol",         "Binary",     "1 = Yes, 0 = No",                                      "Metabolic syndrome link",
  "BMI",              "Continuous", "Body Mass Index (kg/m²)",                              "Key modifiable risk",
  "Smoker",           "Binary",     "1 = ≥100 cigarettes lifetime, 0 = No",                 "Oxidative stress contributor",
  "PhysHlth",         "Ordinal",    "Days poor physical health (0–30)",                     "Proxy for morbidity",
  "MentHlth",         "Ordinal",    "Days poor mental health (0–30)",                       "Psychosocial risk factor",
  "PhysActivity",     "Binary",     "1 = Activity in past 30 days, 0 = No",                "Protective behavior",
  "Fruits",           "Binary",     "1 = Ate fruit ≥1x/day, 0 = No",                        "Diet quality indicator",
  "Veggies",          "Binary",     "1 = Ate veggies ≥1x/day, 0 = No",                      "Diet quality indicator",
  "GenHlth",          "Ordinal",    "1=Excellent, 2=Very Good, 3=Good, 4=Fair, 5=Poor",    "Self-rated health predictor",
  "Age",              "Ordinal",    "1=18–24, ..., 13=80+",                                 "Strong non-modifiable predictor",
  "Education",        "Ordinal",    "1=No school, ..., 6=College grad",                     "Socioeconomic access proxy",
  "Diabetes_012",     "Target",     "0=Healthy, 1=Pre-diabetic, 2=Diabetic",               "Outcome of interest"
)


# ============================================================
# Problem & Objectives
# ============================================================
# Scientific Problem:
#   "Can self-reported lifestyle and demographic indicators be used to
#    stratify diabetes risk in population-level surveys—without clinical lab tests?"

# Data Science Objectives:
# 1. Explore associations between modifiable behaviors (e.g., smoking, activity)
#    and diabetes status using visualization and summary stats.
# 2. Test statistical hypotheses (e.g., BMI differs across diabetes groups).
# 3. Apply ID3 decision tree to generate human-interpretable risk rules.
# 4. Identify top 3 actionable risk factors for public health targeting.
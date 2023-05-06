## Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(rstatix)
library(tidyverse)

## Data import
finaldata_tbl <- readRDS("../data/finaldata_tbl.RDS")

#Test of H1: "There is a relationship between monthly pay and performance rating." Use a correlation and significance test with a scatterplot and fit line
correlation <- cor_test(
  finaldata_tbl, 
  vars= "MonthlyIncome",
  vars2 = "PerformanceRating",
  alternative = "two.sided",
  conf.level = 0.95,
)
correlation

#Test of H2: "Monthly pay differs by department" ANOVA and significance tests, with a boxplot split by department. Include traditional ANOVA summary table (component names, SS, df, MS, F, p)
ANOVA <- anova_test(
  finaldata_tbl,
  formula = MonthlyIncome ~ Department,
  detailed = T
)
get_anova_table (ANOVA)

#Test of H3: "Tenure can be predicted from relationship satisfaction, and this relationship is moderated by gender." Regression and significance tests, with scatterplots and fit lines. Note that you'll need to plot predicted values (i.e. marginal effects), not raw data. Include a table of coefficients, t-tests, and p-values only (no SEs), with meaningful tables 
##### come back and make sure this model is correct 
model_three <- lm(YearsAtCompany ~ RelationshipSatisfaction * Gender, data = finaldata_tbl)
summary(model_three)
model_three_predictive <- fitted(model_three)

#Visualization
#Scatterplot of H1
(ggplot(finaldata_tbl, aes(MonthlyIncome, PerformanceRating)) +
  geom_point( position = "jitter") +
    geom_smooth(method = "lm", se =F) +
    labs(x = "Monthly Income in Dollars", y = "Performance Rating",
         title ="Figure 1. Relationship between Monthly pay and Performance")
    ) %>%
  ggsave(filename = "../figs/H1.png")
  
#Boxplot of H2
(ggplot(finaldata_tbl, aes(Department, MonthlyIncome))+
    geom_boxplot() +
    labs(x= "Department", y="Monthly Income In Dollars", 
         title = "Figure 2. Monthly Pay by Department")) %>%
ggsave(filename = "../figs/H2.png")

#Scatterplot of H3
(ggplot(finaldata_tbl, aes(x=YearsAtCompany, y=model_three_predictive, color=Gender, fill=Gender))+
    geom_smooth(method = "lm", se =F)+
  labs(x= "Relationship Satisfaction", y = "Tenure",
       title ="Figure 3. Tenure by Relationship Satisfaction moderated by gender")
  )%>%
ggsave(filename = "../figs/H3.png")


##Publication

# Publication Results for H1
paste0(
  "The correlation between monthly income and performance ratings was r(",
  nrow(finaldata_tbl)-2, 
  ") = ",
  str_remove(
    format(
      round(correlation$cor,2), 
      nsmall=2),
    "0"),
  ", p = ",
  str_remove(
    format(
      round(correlation$p,2), 
      nsmall=2),
    "^0"),
  ". This test was ",
  ifelse(correlation$p > .05, "not", ""),
  " statistically significant."
)

# Publication Results for H2 (sentence generation)
paste0(
  "The ANOVA test indicated that there was ",
  ifelse(ANOVA$p > 0.05, "not ", ""),
  "a statistically significant difference in monthly income among different departments (",
  "F(", 
  ANOVA$DFn, 
  ", ",
  ANOVA$DFd,
  ") = ",
  format(round(ANOVA$F, 2), nsmall = 2),
  ", p = ",
  str_remove(
    format(
      round(ANOVA$p, 2), 
      nsmall = 2),
    "^0"),
  ").",
  "Therefore, hypothesis 2 was ",
  ifelse(ANOVA$p > 0.05, "not ", ""),
  "supported."
)

#Publication Results for H2 (Table generation)
H2_summary_table <- tibble(
  "Component" = c("Department",
                  "Error",
                  "Total"),
  "SS" = c(ANOVA$SSn,
                       ANOVA$SSd,
                       sum(ANOVA$SSn + ANOVA$SSd)),
  "df" = c(ANOVA$DFn,
           ANOVA$DFd,
           sum(ANOVA$DFn + ANOVA$DFd)),
  "MS" = c(ANOVA$SSn/ANOVA$DFn,
           ANOVA$SSd/ANOVA$DFd,
           NA),
  "F statistic" =c(format(round(ANOVA$F, 2), nsmall = 2),
                   NA,
                   NA),
  "p value" = c(str_remove(
    format(
      round(ANOVA$p, 2), 
      nsmall = 2),
    "^0"),
    NA,
    NA)
)

#creating CSV for H2 output table
write_csv(H2_summary_table, "../out/H2.csv")

# Publication Results for H3
H3_summary_table <- tibble(
  "Coefficients" = c("Intercept",
                     "Relationship Satisfaction",
                     "Gender",
                     "Relationship Satisfaction * Gender")
)
## Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(rstatix)
library(tidyverse)

## Data Import and Cleaning 
finaldata_tbl <- readRDS("../data/finaldata_tbl.RDS")

## Analysis

#Test of H1: "There is a relationship between monthly pay and performance rating." Use a correlation and significance test with a scatterplot and fit line.
#uses rstatix to run a correlation test. Saves the result of the correlation test so that can be called in later visualization and publication section. 
H1_test <- cor_test(
  finaldata_tbl, 
  vars= "MonthlyIncome",
  vars2 = "PerformanceRating",
  alternative = "two.sided",
  conf.level = 0.95,
)
H1_test

#Test of H2: "Monthly pay differs by department" ANOVA and significance tests, with a boxplot split by department. Include traditional ANOVA summary table (component names, SS, df, MS, F, p)
#uses rstatix to run ANOVA. Saves the result of the ANOVA so that can be called in later visualization and publication section. 
ANOVA <- anova_test(
  finaldata_tbl,
  formula = MonthlyIncome ~ Department,
  detailed = T
)
get_anova_table (ANOVA)

##Should I add anything to this, like posthoc test? Come back. 

#Test of H3: "Tenure can be predicted from relationship satisfaction, and this relationship is moderated by gender." Regression and significance tests, with scatterplots and fit lines. Note that you'll need to plot predicted values (i.e. marginal effects), not raw data. Include a table of coefficients, t-tests, and p-values only (no SEs), with meaningful tables 
model_three <- lm(YearsAtCompany ~ RelationshipSatisfaction * Gender, data = finaldata_tbl)
summary_model_three <- summary(model_three)
#creates predictive values 
predictive_values <- predict(model_three)
#Adds predictive values to be called in later visualization and publication 
model_three_predictive_tbl <- finaldata_tbl %>%
  mutate(predictive_values=predictive_values)



##Visualization

#Scatterplot of H1 
(ggplot(finaldata_tbl, aes(MonthlyIncome, PerformanceRating)) +
  geom_point( position = "jitter") +
    geom_smooth(method = "lm", se =F) +
    labs(x = "Monthly Income in Dollars", y = "Performance Rating",
         title ="Figure 1. Relationship between Monthly pay and Performance")
    ) %>%
  ggsave(filename = "../figs/H1.png")
#Decided to leave Monthly income on the x-axis and performance rating on the y-axis because the directions just said that there was a relationship and did not specify which was the dependent or independent variable. I think makes logical sense that an employee's income might depend on their performance rating thus suggesting that the axes should be flipped with performance rating as the variable on the x-axis typically reserved for the dependent variable, however, it also seems possible that those with higher monthly incomes tend to work harder and thus recieve higher performance ratings thus suggesting that the monthly income should remain on the x-axis. Ultimately, because the directions did not specify and just stated there was a relationship, the current display was retained. 
  
#Boxplot of H2
(ggplot(finaldata_tbl, aes(Department, MonthlyIncome))+
    geom_boxplot() +
    labs(x= "Department", y="Monthly Income In Dollars", 
         title = "Figure 2. Monthly Pay by Department")) %>%
ggsave(filename = "../figs/H2.png")

#Scatterplot of H3
(ggplot(model_three_predictive_tbl, aes(x=RelationshipSatisfaction, y=predictive_values, group=Gender,color=Gender))+
    geom_jitter()+
    geom_smooth(method = "lm", se =F)+
  labs(x= "Relationship Satisfaction", y = "Tenure",
       title ="Figure 3. Tenure by Relationship Satisfaction moderated by gender")
  )%>%
ggsave(filename = "../figs/H3.png")


##Publication

# Publication Results for H1 (sentence generation)
paste0(
  "The correlation between monthly income and performance ratings was r(",
  nrow(finaldata_tbl)-2, 
  ") = ",
  str_remove(
    format(
      round(H1_test$cor,2), 
      nsmall=2),
    "0"),
  ", p = ",
  str_remove(
    format(
      round(H1_test$p,2), 
      nsmall=2),
    "^0"),
  ". This test was ",
  ifelse(H1_test$p > .05, "not", ""),
  " statistically significant."
)

#Come back do I need a table for H!?

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

#Come back and fix the decimal places and zeros on this 

#Publication Results for H2 (Table generation)
H2_summary_table <- tibble(
  "Component" = ANOVA$Effect, 
  "SSn" = ANOVA$SSn, 
  "SSd" = ANOVA$SSd, 
  "DFn" = ANOVA$DFn, 
  "DFd" = ANOVA$DFd, 
  "F-Statistic" = ANOVA$F, 
  "p-value" = ANOVA$p) %>%
  mutate(across(c(2:7), ~ str_remove(format(round(., 2), nsmall = 2), "^0")))

#creating CSV for H2 output table
write_csv(H2_summary_table, "../out/H2.csv")

#Publication Results for H3 (Table generation)
H3_summary_table <- tibble(
  "Variable" = c("Intercept", "Relationship Satisfaction", "Gender", "Interaction:Gender*Rel. Sat."), 
  "Estimate" = summary_model_three$coefficients[,"Estimate"],
  "t-value" = summary_model_three$coefficients[,"t value"], 
  "p-value"= summary_model_three$coefficients[,"Pr(>|t|)"]) %>%
  mutate(across(c(2:4), ~ str_remove(format(round(., 2), nsmall = 2), "^0")))

#creating CSV for H3 output table
write_csv(H3_summary_table, "../out/H3.csv")
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
model_three <- lm(YearsAtCompany ~ 1 + RelationshipSatisfaction + RelationshipSatisfaction * Gender, data = finaldata_tbl)
summary(model_three)

#Visualization
#Scatterplot of H1
(ggplot(finaldata_tbl, aes(MonthlyIncome, PerformanceRating)) +
  geom_point( position = "jitter") +
    geom_smooth(method = "lm", se =F) +
    labs(x = "Monthly Income in Dollars", y = "Performance Rating",
         title ="Figure 1. Relationship between Monthly pay and Performance")
    )
  
#Boxplot of H2
(ggplot(finaldata_tbl, aes(Department, MonthlyIncome))+
    geom_boxplot() +
    labs(x= "Department", y="Monthly Income In Dollars", 
         title = "Figure 2. Monthly Pay by Department"))

#Scatterplot of H3
(ggplot(finaldata_tbl, aes(x=RelationshipSatisfaction, y=YearsAtCompany, color=Gender, fill=Gender))+
    geom_smooth(method = "lm", se =F)+
  labs(x= "Relationship Satisfaction", y = "Tenure",
       title ="Figure 3. Tenure by Relationship Satisfaction moderated by gender")
  )
#Something wrong with this one because need to have predicted values (marginal effects)

##Publication


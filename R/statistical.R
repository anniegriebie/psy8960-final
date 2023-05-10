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
#did not do a posthoc analysis because was not specified in the final directions. Additionally, the sample sizes for each department group are not equal, so considered using the "games_howell_test" from the rstatix package because rstatix documentation explains that it serves as an improved version of the Tukey-Kramer method that is applicable when group sample sizes are not equivalent but chose to not because doing so was not specified within the directions. Additionally, the rstatix documentation specifies that the Games-Howell method is best applied when the number of samples is six or more yet the department variable only has three different groups. 

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
  labs(x= "Relationship Satisfaction", y = "Predicted Tenure in Years",
       title ="Figure 3. Tenure by Relationship Satisfaction Moderated by Employee Gender")
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


#Publication Results for H2 (Table generation)
H2_summary_table <- tibble(
  "Component" = ANOVA$Effect, 
  "SSn" = str_remove(round(ANOVA$SSn,2),"^0"), 
  "SSd" = str_remove(round(ANOVA$SSd,2), "^0"), 
  "DFn" = str_remove(round(ANOVA$DFn, 2), "^0"), 
  "DFd" = str_remove(round(ANOVA$DFd,2), "^0"), 
  "F-Statistic" = str_remove(round(ANOVA$F,2), "^0"), 
  "p-value" = str_remove(round(ANOVA$p, 2), "^0")) 

#creating CSV for H2 output table
write_csv(H2_summary_table, "../out/H2.csv")

# Publication Results for H2 (sentence generation)
paste0(
  "The results of the ANOVA shows a",
  ifelse(ANOVA$p > 0.05, "not ", ""),
  "statistically significant difference in monthly income across different departments (",
  "F(", ANOVA$DFn, ", ",
  ANOVA$DFd, ") = ", format(round(ANOVA$F, 2), nsmall = 2),
  ", p = ",str_remove(format(round(ANOVA$p, 2),nsmall = 2),"^0"), ").",
  "Therefore, hypothesis 2, that monthly pay differs by department, was ",
  ifelse(ANOVA$p > 0.05, "not ", ""),
  "supported.")

#Publication Results for H3 (Table generation)
H3_summary_table <- tibble(
  "Variable" = c("Intercept", "Relationship Satisfaction", "Gender", "Interaction:Gender*Relationship Satisfaction"), 
  "Estimate" = str_remove(round(summary_model_three$coefficients[,"Estimate"],2),"0"), 
  't-value' = str_remove(round(summary_model_three$coefficients[,"t value"],2), "0"),
  'p-value' = str_remove(format(round(summary_model_three$coefficients[,"Pr(>|t|)"],2)), "^0+"))
#wrapping in p-value input in format keeps the trailing zeros while still removing the leading zeros. If do not include "format" then will remove the 0.00 p-value completely leaving a blank cell. 

#creating CSV for H3 output table
write_csv(H3_summary_table, "../out/H3.csv")

# Publication Results for H3 (sentence generation)
#ended up making a sentence for each row of the regression table to interpret and tried to make into a paragraph. Final directions stated it was ok to have explanation in plain text so because of the length and complexity of this model I decided to not try to dynmically generate "not significant" vs. "significant" for each of the variables and instead only dynamically generated the numbers. 
paste0(
  "Simple linear regression was used to test if tenure could be predicted from relationship satisfaction with the relationship moderated by employee gender, the results shows a significant effect for the intercept (",
  "t(", (nrow(finaldata_tbl)-4), " ",
   ") = ",(H3_summary_table$`t-value`[1]),
  ", p = ",(H3_summary_table$`p-value`[1]), ").",
  " ",
  "Additionally, the results show a non-significant effect for Relationship Satisfaction (",
  "t(", (nrow(finaldata_tbl)-4), " ",
  ") = ",(H3_summary_table$`t-value`[2]),
  ", p = ",(H3_summary_table$`p-value`[2]), ").",
  " ",
  "Additionally, the results show a non-significant effect for gender (",
  "t(", (nrow(finaldata_tbl)-4), " ",
  ") = ",(H3_summary_table$`t-value`[3]),
  ", p = ",(H3_summary_table$`p-value`[3]), ").",
  " ",
  "Additionally the results show a non-significant effect for the interaction between Relationship Satisfaction and Gender  (",
  "t(", (nrow(finaldata_tbl)-4), " ",
  ") = ",(H3_summary_table$`t-value`[4]),
  ", p = ",(H3_summary_table$`p-value`[4]), ").",
  "",
  "Therefore, hypothesis 3, that the relationship between relationship satisfaction and tenure is moderated by gender is not supported at an alpha value of .05")



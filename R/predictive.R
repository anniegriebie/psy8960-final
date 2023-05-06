## Script Settings and Reources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(tm)
library(qdap)
library(textstem)
library(RWeka)
library(ldatuning)
library(topicmodels)
library(tidytext)
library(wordcloud)
library(psych)
library(doParallel)

## Data import and cleaning
finaldata_tbl <- readRDS("../data/finaldata_tbl.RDS")

## Analysis 

#set seed for reproducibility
set.seed(331)

#Turnover = Attrition 
#All available variables and all available cases, 

#Transforming data 
cleaned_tbl <- finaldata_tbl%>%
  mutate(Attrition = as_factor(recode(Attrition, "No" = 0, "Yes" = 1)),
         BusinessTravel = (recode(BusinessTravel, "Non-Travel" = 0,"Travel_Rarely" = 1,"Travel_Frequently" = 2)),
         Department = (recode(Department, "Human Resources" = 1, "Research & Development" = 2, "Sales" = 3)),
         EducationField = (recode(EducationField, "Human Resources" = 1, "Life Sciences" = 2, "Marketing" = 3, "Medical" = 4, "Technical Degree" = 5,"Other" = 6)),
         Gender = (recode(Gender, "Male" = 1, "Female" =2)),
         JobRole = (recode(JobRole, "Healthcare Representative" = 1, "Human Resources" = 2, "Laboratory Technician" = 3, "Manager" = 4, "Manufacturing Director" = 5, "Research Director" = 6, "Research Scientist" = 7, "Sales Executive" = 8,"Sales Representative" = 10)),
         MaritalStatus = (recode(MaritalStatus, "Single" = 1, "Married" = 2, "Divorced" = 3)),
         Over18 = (recode(Over18, "N" = 0, "Y"=1)),
         OverTime = (recode(OverTime, "No" = 0, "Yes" = 1)
  )) %>%
  drop_na(good,bad)

## NLP for the good and bad comments columns

#writing preprocessing function 
corpus_function <- function(corpus) {
  corpus %>%
  tm_map(content_transformer(replace_abbreviation)) %>%
  tm_map(content_transformer(replace_contraction)) %>%
  tm_map(removeWords, "IO") %>%
  tm_map(content_transformer(str_to_lower)) %>%
  tm_map(removeWords, "[^[:alnum: ]]") %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(content_transformer(lemmatize_words)) 
  }

#breaking into good and bad corpuses so that can run NLP on both the good and bad comments rather than together, tried it together and got more unhelpful words as topics due to prevelance of words like "like" etc.
positive_corpus <- VCorpus(VectorSource(cleaned_tbl$good))
negative_corpus <- VCorpus(VectorSource(cleaned_tbl$bad))

#running preprocessing 
positive_cleaned <- corpus_function(positive_corpus)
negative_cleaned <- corpus_function(negative_corpus)

#tokenizer
twogram <- function(x) { NGramTokenizer(x, Weka_control(min=1, max=2)) }

positive_dtm <- DocumentTermMatrix(positive_cleaned, control=list(tokenizer=twogram))

negative_dtm <- DocumentTermMatrix(negative_cleaned, control=list(tokenizer=twogram))

#turning on parallelization to make run faster
local_cluster = makeCluster(7)   
registerDoParallel(local_cluster)

#LDA model for topics within good reviews, not including "Griffiths 2004" because does not run on my Mac
tuning_good <- FindTopicsNumber(
  positive_dtm,
  topics = seq(2,10,1),
  metrics = c("CaoJuan2009",
              "Arun2010",
              "Deveaud2014"),
  verbose = T
)
FindTopicsNumber_plot(tuning_good)

tuning_bad <- FindTopicsNumber(
  negative_dtm,
  topics = seq(2,10,1),
  metrics = c("CaoJuan2009",
              "Arun2010",
              "Deveaud2014"),
  verbose = T
)
FindTopicsNumber_plot(tuning_bad)

#Stop parallelization
stopCluster(local_cluster)
registerDoSEQ()

#Results Good Reviews
lda_results_good <- LDA(positive_dtm, 5)
lda_gammas_good <- tidy(lda_results_good, matrix="gamma")%>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  slice(1) %>%
  ungroup %>%
  mutate(employee_id = as.numeric(document)) %>%
  arrange(employee_id)

#Results Bad Reviews
lda_results_bad <- LDA(negative_dtm, 5)
lda_gammas_bad <- tidy(lda_results_bad, matrix="gamma")%>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  slice(1) %>%
  ungroup %>%
  mutate(employee_id = as.numeric(document)) %>%
  arrange(employee_id)

#Merging to create larger data set
complete_predictive_tbl <- cleaned_tbl %>%
  left_join(y=lda_gammas_good, by="employee_id") %>%
  left_join(y=lda_gammas_bad, by = "employee_id")%>%
  #renaming topics so better able to track
  mutate(topic_positive =topic.x, topic_negative = topic.y) %>%
  #removing unnecessary/duplicated columns
  select(-document.x, -document.y, -good, -bad, -topic.x, -topic.y, -gamma.x, -gamma.y)
  #Could remove all variables that all have the same value (i.e. EmployeeCount is 1 for all employees and StandardHours is 80 for all employees)
  #select(-EmployeeCount, -StandardHours) but left this in for now because directions say to use all available cases and all available variables in soem way.

## Analysis

#removing satisfaction reviews columns for non-text models
notext_predictive_tbl <-complete_predictive_tbl %>%
  select(-topic_positive, -topic_negative)

#using ML to test models to see which is likely to best predict turnover 
#for complete tbl
train_cases <- sample(1:nrow(complete_predictive_tbl), .75*nrow(complete_predictive_tbl))
predictive_train_tbl <- complete_predictive_tbl[train_cases, ]
predictive_test_tbl <- complete_predictive_tbl[-train_cases, ]
training_folds <- createFolds(predictive_train_tbl$Attrition,
                              k=10)
#for non-text models
notext_train_cases <- sample(1:nrow(notext_predictive_tbl), .75*nrow(notext_predictive_tbl))
notext_train_tbl <- notext_predictive_tbl[notext_train_cases, ]
notext_test_tbl <- notext_predictive_tbl[-notext_train_cases, ]
notext_training_folds <- createFolds(notext_train_tbl$Attrition,
                              k=10)

#Starting parallelization
local_cluster <- makeCluster(7)
registerDoParallel(local_cluster)

#Decided to not include "lm" model because Accuracy metric not available for regression models 

#Elastic Net model with comments
EN_model <- train(
  Attrition ~ .,
  data = predictive_train_tbl, 
  method = "glmnet",
  tuneLength = 3,
  na.action = "na.pass", 
  preProcess = c("nzv", "center", "scale", "bagImpute"),
  trControl =  trainControl(
    method = "cv",
    indexOut = training_folds,
    verboseIter = TRUE
  )
)

#Elastic Net model without comments
EN_model_nocomment <- train(
  Attrition ~ .,
  data = notext_train_tbl, 
  method = "glmnet",
  tuneLength = 3,
  na.action = "na.pass", 
  preProcess = c("nzv", "center", "scale", "bagImpute"),
  trControl =  trainControl(
    method = "cv",
    indexOut = training_folds,
    verboseIter = TRUE
  )
)

#Random forest model with comments
RF_model <- train(
  Attrition ~ .,
  data = predictive_train_tbl, 
  method="ranger",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)

#Random forest model without comments
RF_model_nocomment <- train(
  Attrition ~ .,
  data = notext_train_tbl, 
  method="ranger",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)

#Extreme Gradient Boost Model with comments
XGBT_model <- train(
  Attrition ~ .,
  data = predictive_train_tbl, 
  method="xgbTree",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)

#Extreme Gradient Boost Model without comments
XGBT_model_nocomment <- train(
  Attrition ~ .,
  data = notext_train_tbl, 
  method="xgbTree",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)

#Stopping parallelization
stopCluster(local_cluster) 
registerDoSEQ()


summary(resamples(list(EN_model, RF_model, XGBT_model)))
resample_sum <- summary(resamples(list(EN_model, RF_model, XGBT_model)))
dotplot(resamples(list(EN_model, RF_model, XGBT_model)))


#assessing the accuracy of each model
resample_sum$

# Publication
Publication_tbl <-tibble(
  algo = c("glmnet", "ranger", "xgbTree"),
  cv_rsq = str_remove(round(
    resample_sum$statistics$Rsquared[,"Mean"],2), "^0"
  ),
  ho_rsq = str_remove(c(
    format(round(hocv_cor_EN, 2), nsmall =2),
    format(round(hocv_cor_RF,2), nsmall=2),
    format(round(hocv_cor_XGBT, 2), nsmall=2)
  ), "^0"),
  cv_accuracy = str_remove(round(c(EN_model$results$Accuracy, RF_model$results$Accuracy, XGBT_model$results$Accuracy), 2), nsmall=2)
  )

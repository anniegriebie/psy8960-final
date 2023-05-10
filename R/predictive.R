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
library(tictoc)


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
    #replaces abbreviations with long form
  tm_map(content_transformer(replace_abbreviation)) %>%
    #replaces contractions with long form
  tm_map(content_transformer(replace_contraction)) %>%
    #converts to lowercase 
  tm_map(content_transformer(str_to_lower)) %>%
  tm_map(removeWords, "[^[:alnum: ]]") %>%
    #removes numbers
  tm_map(removeNumbers) %>%
    #removes punctuation
  tm_map(removePunctuation) %>%
    #removes standard english stopwords
  tm_map(removeWords, c(stopwords("en"))) %>% 
    #removes whitespace
  tm_map(stripWhitespace) %>%
    #groups words to be analyzed 
  tm_map(content_transformer(lemmatize_words)) 
  }

#breaking into good and bad corpuses so that can run NLP on both the good and bad comments rather than together, tried it together and got more unhelpful words as topics due to prevelance of words like "like" etc.
positive_corpus <- VCorpus(VectorSource(cleaned_tbl$good))
negative_corpus <- VCorpus(VectorSource(cleaned_tbl$bad))

#running preprocessing 
positive_cleaned <- corpus_function(positive_corpus)
negative_cleaned <- corpus_function(negative_corpus)

#bigram tokenizer, chose to not do a remove sparse terms command because would result in zero entry warning  
twogram <- function(x) { NGramTokenizer(x, Weka_control(min=1, max=2)) }
#for positive reviews
positive_dtm <- DocumentTermMatrix(positive_cleaned, control=list(tokenizer=twogram))
#for negative reviews
negative_dtm <- DocumentTermMatrix(negative_cleaned, control=list(tokenizer=twogram))

#turning on parallelization to make run faster
local_cluster = makeCluster(7)   
registerDoParallel(local_cluster)

#LDA model for topics within good reviews, not including "Griffiths 2004" because does not run on my Mac. I chose to do topic modeling in order to organize the data because the number of reviews are large and I thought this would help focus the relevant material 
tuning_good <- FindTopicsNumber(
  positive_dtm,
  topics = seq(2,10,1),
  metrics = c("CaoJuan2009",
              "Arun2010",
              "Deveaud2014"),
  verbose = T
)
FindTopicsNumber_plot(tuning_good)

#LDA model for topics within good reviews, not including "Griffiths 2004" because does not run on my Mac. I chose to do topic modeling in order to organize the data because the number of reviews are large and I thought this would help focus the relevant material 
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
  #select(-EmployeeCount, -StandardHours) but left this in for now because directions say to use all available cases and all available variables in some way.


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
tic()
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
toc_EN_model<-toc()
#holdout EN
hoEN <-predict(EN_model, predictive_test_tbl, na.action = na.pass)

#Elastic Net model without comments
tic()
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
toc_EN_model_nocomments <-toc()

#holdout EN no comments
hoEN_nocomments<-predict(EN_model_nocomment, notext_test_tbl, na.action=na.pass)

#Random forest model with comments
tic()
RF_model <- train(
  Attrition ~ .,
  data = predictive_train_tbl, 
  method="ranger",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_RF_model <- toc()
#holdout RF
hoRF <-predict(RF_model, predictive_test_tbl, na.action = na.pass)

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
tic()
XGBT_model <- train(
  Attrition ~ .,
  data = predictive_train_tbl, 
  method="xgbTree",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_XGBT_model <- toc()
#holdout XGBT
hoXGBT <-predict(XGBT_model, predictive_test_tbl, na.action = na.pass)

#Extreme Gradient Boost Model without comments
tic()
XGBT_model_nocomment <- train(
  Attrition ~ .,
  data = notext_train_tbl, 
  method="xgbTree",
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_XGBT_model_nocomments <-toc()

#holdout XGBT no comments
hoXGBT_nocomments<-predict(XGBT_model_nocomment, notext_test_tbl, na.action=na.pass)

#Stopping parallelization
stopCluster(local_cluster) 
registerDoSEQ()

#running confusionMatrix to get holdoutaccuracy for models.
ENholdout <- confusionMatrix(hoEN, predictive_test_tbl$Attrition)
RFholdout <- confusionMatrix(hoRF, predictive_test_tbl$Attrition)
XGBTholdout <- confusionMatrix(hoXGBT, predictive_test_tbl$Attrition)
ENholdout_nocomments <- confusionMatrix(hoEN_nocomments,notext_test_tbl$Attrition )

#compiling models to call in later tbl. 
summary(resamples(list(EN_model, RF_model, XGBT_model)))
resample_list <- summary(resamples(list(EN_model, RF_model, XGBT_model)))
dotplot(resamples(list(EN_model, RF_model, XGBT_model)))

comparison_list <-summary(resamples(list(EN_model, EN_model_nocomment)))

## Publication

# Publication Table contrasting information considered in deciding which model to use as final model
Publication_tbl <-tibble(
  algo = c("glmnet", "ranger", "xgbTree"),
  cv_accuracy = str_remove(format(round(resample_list$statistics$Accuracy[,"Mean"],2), nsmall=2),"^0"),
  ho_accuracy= str_remove(c(
    format(round(ENholdout$overall["Accuracy"], 2), nsmall = 2), 
    format(round(RFholdout$overall["Accuracy"], 2), nsmall=2),
    format(round(XGBTholdout$overall["Accuracy"], 2), nsmall=2)
  ),"^0"),
  run_time =c(
    format(round(toc_EN_model$toc - toc_EN_model$tic,2),nsmall=2),
    format(round(toc_RF_model$toc -toc_RF_model$tic,2), nsmall=2),
    format(round(toc_XGBT_model$toc - toc_XGBT_model$tic,2), nsmall=2))
)

#Question #1. Based on this table it looks like the best final model to pick is the Elastic Net model. I examined the accuracy and the time it took to run the models. Table is output as "PublicationPart2"in output folder. Based on this comparison table it appears that the Random Forest model (.98) and the XGBTree model (.99) have the highest cv accuracy, however, the Elastic Net model (.89) still has a high cv acccuracy value. The reason I chose to go with the Elastic Net model as the final model given these differences is because the Elastic Net model has the highest holdout accuracy value of the three models relating to the test data (.87) thus suggesting it is the most accurate for further test data. The Elastic Net model did take the longest time to run (41.79 seconds) compared to the other two models which was a downside, however, the time was still less than a minute with the parallel processing on thus suggesting it does not take an unreasonable amount of time. Two cited advantage of using the Elastic Net model is that it uses both the lasso and ridge penalty and is able to effectively deal with highly correlated variables, I think these features of the Elastic Net model contributed to maximizing it's performance compared to the other models. 

#creating CSV for publication output table
write_csv(Publication_tbl, "../out/PublicationPart2.csv")

#Summary table comparing predictive accuracy of final model with and without text-derived predictors
Summary_tbl <- tibble(
  algo = c("EN (Text Data)", "EN (No Text Data"),
  cv_accuracy = str_remove(format(round(comparison_list$statistics$Accuracy[,"Mean"],2), nsmall=2),"^0"),
  ho_accuracy = str_remove(c(
    format(round(ENholdout$overall["Accuracy"],2), nsmall=2),
    format(round(ENholdout_nocomments$overall["Accuracy"], 2), nsmall=2)
  ), "^0"),
  run_time =c(
    format(round(toc_EN_model$toc - toc_EN_model$tic,2), nsmall=2),
    format(round(toc_EN_model_nocomments$toc - toc_EN_model_nocomments$tic,2), nsmall=2)
  )
)

#creating CSV for summary output table
write_csv(Summary_tbl, "../out/SummaryPart2.csv")

#Question #2. Table is output as "SummaryPart2" in the out folder. Based on this table comparing the elastic net model with both text data and no text data, the results seem to suggest that removing the text data increased decreased the cv accuracy of the model but increased the holdout accuracy of the model. Additionally, removing the text data ultimately reduced the run time for the model as well from 41.79 seconds to 35.70 seconds. These results suggest little difference between the two models with and without text data, however, if one needed to make a decision I would suggest because the holdout accuracy value is higher (.88 for the non-text model vs. .87 for the text data model) that one should use the no-text data model. It is worth noting that the accuracy between the two models is very similar and therefore making it more difficult to make a definitive comment on the incremental predictive accuracy of including the text data within the elastic net model. 

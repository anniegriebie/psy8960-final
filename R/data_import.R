## Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

## Data Import and Cleaning
#loading in first dataset 
dataset_proj<-read_delim("../data/dataset.csv", delim ="+")%>%
  #adding in employee_id column
  mutate(employee_id = row_number())

#loading in second dataset, naming column names of the reviews so that can be called in later R files
satisfaction_reviews <- read_delim("../data/satisfaction_reviews.csv", delim = ".", col_names = c("good", "bad", "employee_id"))

#creating final dataset
finaldata_tbl <- left_join(dataset_proj, satisfaction_reviews, join_by(employee_id))

#Saving data into data folder to be used in later files 
saveRDS(finaldata_tbl, "../data/finaldata_tbl.RDS")

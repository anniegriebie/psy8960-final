## Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

#loading in datasets
dataset_proj<-read_delim("../data/dataset.csv", delim ="+")%>%
  mutate(employee_id = row_number())

satisfaction_reviews <- read_delim("../data/satisfaction_reviews.csv", delim = ".", col_names = c("good", "bad", "employee_id"))

#creating dataset
finaldata_tbl <- left_join(dataset_proj, satisfaction_reviews, join_by(employee_id))

saveRDS(finaldata_tbl, "../data/finaldata_tbl.RDS")

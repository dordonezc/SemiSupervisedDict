setwd("C:/Users/dordo/Documents/Daniel/LSE/MY 459/Proyecto")
library(tidyverse)
library(quanteda)

## 
word_list <- map(c("NegativeModifiers.csv", "NegativeWordsMC.csv",
      "PositiveModifiers.csv", "PositiveWordsMC.csv"), 
    ~as.character(read_csv(str_c("Data/", .x))[[1]]))
names(word_list) <- c("NegativeModifiers", "NegativeWords", "PositiveModifiers",
                      "PositiveWords")

## Dictionary
mc_dict <- quanteda::dictionary(word_list)

## Read data
data <- read_rds("DataOil.Rdata") 

## Convert to Corpus
data_c <- corpus(data, docid_field = "Title", text_field = "Text")

## Apply Mcdonald Dictionary
scores <- dfm(data_c, dictionary = mc_dict)

## By sentiment
neg_scores <- dfm_keep(scores, "Negative", valuetype = "regex") %>% rowSums()
pos_scores <- dfm_keep(scores, "Positive", valuetype = "regex") %>% rowSums()

## Total
total <- (pos_scores - neg_scores)/(neg_scores + pos_scores)

## Export
out <- tibble("Title"=names(total), "Score"=total)

write_csv(out, "data/McDonaldPolarities.csv")

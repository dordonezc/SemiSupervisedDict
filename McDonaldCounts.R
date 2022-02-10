setwd("C:/Users/dordo/Documents/Daniel/LSE/MY 459/Proyecto")
library(tidyverse)

## Read McDonald
check_words <- str_to_lower(read_csv("Data/CheckWordsMC.csv")[[1]])
pos_modifiers <- str_to_lower(read_csv("Data/PositiveModifiers.csv")[[1]])
neg_modifiers <- str_to_lower(read_csv("Data/NegativeModifiers.csv")[[1]])

## Load RData
data <- read_rds("DataOil.RData")

##-------------------------------------------------------------------------------------##
## Function
search_txt <- function(x, word_pool, pos, neg){
  tks <- tokens(x, remove_numbers = T, remove_symbols = T,
                remove_punct = T) 
  tks <- tokens_remove(tks, stopwords("en"))
  search_wrds <- function(tks, x, pos, neg){
    aux <- kwic(tks, x)
    if(length(aux[[1]]) > 0){
      context <- c(str_split(aux$pre, " ", simplify = T), str_split(aux$post, " ", simplify = T))
      posc <- sum(pos %in% context)
      negc <- sum(neg %in% context)
      c(posc, negc)
    } else{
      c(0, 0)
    }
  }
  structure(colSums(do.call(rbind, lapply(word_pool, search_wrds, tks = tks, pos = pos, neg = neg))),
            names = c("Positive", "Negative"))
}

##system.time({search_txt(data$Text[[1]], check_words, pos_modifiers, neg_modifiers)})
##search_txt(data$Text[[10]], check_words, pos_modifiers, neg_modifiers)
res <- lapply(data$Text, search_txt, check_words, pos_modifiers, neg_modifiers)
mat <- do.call(rbind, res)
out_data <- bind_cols(data, "Positive"=mat[,1], "Negative"=mat[,2])
colnames(out_data)
write_csv(out_data, "Data/McDonaldOriginal.csv")

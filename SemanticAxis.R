library(tidyverse)
library(word2vec)
setwd("C:/Users/dordo/Documents/Daniel/LSE/MY 459/Proyecto")
model <- word2vec::read.word2vec("ModelWV.bin")

## record vocabulary
vocab <- summary(model)

#a <- as.matrix(model)
#b <- data.frame("name"=rownames(a), a)
##------------------------------------------------------------------------------##
## Check for good seeds
# root_seed <- c("succes", "excel", "^profit", "benefic", "improv",
#                "positiv", "^gain")

## A la McDonald
root_seed <- c("^discov", "glut", "^overpro", "^oversup", "recess",
                "^repair", "^surpl")

## Matches in the vocabulary
root_words <- unlist(lapply(root_seed, str_subset, string = vocab))
#write_csv(as_tibble(root_words), "data/rootGoodMc.csv")

## Embedding for root words
good_embedding <- predict(model, root_words, "embedding")

## Check: Exploratory Factor Analysis of Data Matrices With More Variables Than Observations
good <- colMeans(good_embedding)
hist(good_embedding[,3])

##------------------------------------------------------------------------------##
## Check for bad seeds
# root_seed_bad <- c("^los", "volati", "wron", "damag", "^bad", "litiga",
#   "fail", "negat", "downg", "lock[^c]", "down[swt].",
#   "^[^ld].+down$")

root_seed_bad <- c("attack", "bomb", "^closur", "^concer", "confli",
                   "delay", "^dispu", "^disrup", "^explos", "fire",
                   "^hurri", "instab", "outa", "probl", "recover",
                   "sabo", "shorta", "storm", "strike", "turm",
                   "unres", "withd")

# new_roots <- read_csv("Data/NegativeWordsMC.csv")
# new_roots %>% pull() %>% str_to_lower() %>% view()

root_words_bad <- unlist(lapply(root_seed_bad, str_subset, string = vocab))
#write_csv(as_tibble(root_words_bad), "data/rootBadMc.csv")

## Embedding for root words
bad_embedding <- predict(model, root_words_bad, "embedding")
bad <- colMeans(bad_embedding)
hist(bad_embedding[,3])

##---------------------------------------------------------------------------------##
## Calculate semantic axis
sem_axis <- good-bad
dot_p <- t(sem_axis %*% t(as.matrix(model)))/50
new_dict <- tibble("Word"=rownames(dot_p), "Sim"=dot_p[,1]) %>% arrange(desc(Sim))

write_csv(new_dict, "data/SemanticAxisPolaritiesMc.csv")

## Final
predict(model, "success", type = "nearest")


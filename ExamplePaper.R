library(tidyverse)
library(readxl)
library(quanteda)
library(e1071)
library(lubridate)
setwd("C:/Users/dordo/Documents/Daniel/LSE/MY 459/Proyecto")


# Example Oil Corpus ------------------------------------------------------

## Read Returns with direction 
oil_fws <- read_excel("Data/OilPrices.xlsx") %>% 
  mutate(DPrice = ifelse(c(NA, diff(Price))> 0, 1, -1)) %>% 
  fill(Price, .direction = "down") #%>%

## Read Corpus
data <- read_rds("DataOil.Rdata") %>% 
  filter(year(Date) > 2018, year(Date) < 2021)

## Paste corpus with label
all_data <- data %>% mutate(Date = round_date(Date, "day")) %>% 
  left_join(oil_fws) %>% 
  filter(!is.na(Price)) %>% 
  mutate(id = row_number())

## Read Embeddings
embed <- read_csv("Embed.csv")

## Creating Corpus + DFM
corp <- corpus(all_data %>% select(id, Text, Title, DPrice), 
               docid_field = "id", text_field = "Text")
tks_use <- corp %>%  tokens() %>% dfm(select=pull(embed[,1]))
wts_use <- tks_use %>% dfm_weight(scheme = "prop")

# Filtering Corpus --------------------------------------------------------
wts_mat <- wts_use %>% convert("matrix")
label_vct <- docvars(wts_use)[,2]
embed <- embed %>% 
  arrange(name) %>% 
  filter(name %in% colnames(wts_mat))
wts_mat <- wts_mat[,embed$name]
embed_mat <- as.matrix(embed[, -1])

rm(list = setdiff(ls(), c("embed","embed_mat", "wts_mat", "label_vct")))

##-------------------------------------------------------------------------##
## Start


(res <- directed_weights(wts_mat, embed_mat, label_vct))
directed_svm_weights(wts_mat, embed_mat, label_vct)


# for (i in 1:k){
#   lambda[i]*embed_mat[i,]
#   print(i)
#}

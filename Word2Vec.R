library(tidyverse)
library(quanteda)
library(word2vec)
library(parallel)

## Parallel
sock<-makePSOCKcluster(3L) ##3 Procesadores!
clusterSetRNGStream(cl=sock, 10)
clusterEvalQ(sock,{library(quanteda) ; library(tidyverse)})

## Read Data
setwd("C:/Users/dordo/Documents/Daniel/LSE/MY 459/Proyecto")
data <- read_csv("Data/AllData.csv") %>%
  separate(Date, into = c("Date", "Author"), sep = "\\|") %>% 
  mutate(Date = ymd_hms(as.character(strptime(str_squish(Date), format = "%B %d, %Y at %H:%M"))),
         Author = str_squish(Author)) %>% 
  filter(URL != "https://oilprice.com/Latest-Energy-News/World-News/Russias-Lukoil-Ships-Arctic-Oil-To-China-In-Unusual-Trade-Flow10244.html")

## Example
ex <- data$Text

## Basic Cleaner Function
easy_parse <- function(piece){
  x <- str_remove(piece, "[Oo]il[Pp]rice\\.com.+")
  x <- str_replace_all(x, '[\\n""]', "")
  writer <- str_extract(x, "By [A-Z][a-z]+ [A-Z][a-z]+ for")
  writer <- ifelse(is.na(writer), "NONE", writer)
  x <- str_remove(x, writer) %>% str_to_lower()
  writer <-str_remove_all(writer, "By|for") %>% str_squish()
  bow <- quanteda::tokens(x, remove_punct = T, remove_symbols = T, remove_numbers =T)
  out <- str_c(bow[[1]], collapse = " ") %>% str_replace_all("[\\.]", " ")
  out <- str_replace_all(out, "u s", "u.s")
  list("Text"= out, "Editor"=writer)
}

## Export
clusterExport(sock, ls())

## Doc clean
system.time({new_docs <- parLapply(sock, ex, easy_parse)})
new_text <- unlist(lapply(new_docs, `[[`, 1))

## 
data$Text <- new_text
data <- data %>% distinct()
write_rds(data , "DataOil.Rdata")
##--------------------------------------------------------------------------------------------##
## Word2Vec
model <- word2vec(x = new_text, dim = 50, iter = 20)
emb   <- as.matrix(model)
head(emb)

## Vocabulary
vocab <- summary(model, type = "vocabulary")
vocab

## Relations
emb_p <- predict(model, c("reservoir"), type = "nearest")
emb_p

## Save the model to hard disk
path <- "mymodel.bin"
write.word2vec(model, file = "C:/Users/dordo/Documents/Daniel/LSE/MY 459/ProyectoModelWV.bin")


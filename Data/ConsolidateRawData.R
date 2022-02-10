setwd("C:/Users/dordo/Dropbox/Capstone Project/Data/Others")
library(tidyverse)
data <- read_csv("OilNews1.csv") %>% select(-1)
data2 <- read_csv("OilNews2.csv") 
header <- read_csv("OilTest.csv") %>% select(-1)

data2_n <- cbind(data2 %>% filter(!is.na(`0`)) %>% select(-`0_1`),
      data2 %>% filter(is.na(`0`)) %>% slice(-1) %>% select(`0_1`))
data2_n <- data2_n[,-1]

## 
colnames(data) <- colnames(data2_n) <- c("Text", "URL")
colnames(header) <- c("URL", "Title", "Date")

all_data <- bind_rows(header %>% inner_join(data),
          header %>% inner_join(data2_n))

write_csv(all_data, "AllData.csv")

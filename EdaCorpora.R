library(tidyverse)
library(quanteda)
setwd("C:/Users/dordo/Documents/Daniel/LSE/MY 459/Proyecto")

## Read RData
data <- read_rds("DataOil.Rdata")

## Filter only relevant articles
check_data <- data %>% filter(year(Date) > 2018, year(Date) < 2021)

## Count by Author
check_data %>% distinct(Author)

##----------------------------------------------------------------------##
## Attention Plot
tot_date <- tibble("Date"=ymd(seq.Date(from = as.Date("2019/01/01"), 
                                       to = as.Date("2020/12/31"), "days")))

sum_data <- check_data %>% mutate(Date = floor_date(Date, unit = "day")) %>%
  group_by(Date) %>% summarise("Count"=n())

sum_data <- tot_date %>% left_join(sum_data) %>% 
  mutate(Missing = ifelse(is.na(Count), 1L , 0L))

sum_data %>% ggplot(aes(x=Date, y= Count)) + geom_line(color = "red") +
  ggtitle("Daily Count of Articles") + theme_minimal()

colrs <- c("#00FF00", "#FF0000")
sum_data %>% mutate(Year = as.factor(year(Date))) %>% 
  ggplot(aes(x=Count, fill = Year)) + geom_bar() + facet_wrap(~Year) + 
  xlab("# of articles") + theme_minimal() +
  scale_fill_manual(values = colrs) 
          
##----------------------------------------------------------------------##     
## Number of tokens by news
corp <- corpus(check_data %>% mutate(Year=year(Date)), text_field = "Text")
tks <- tokens(corp, remove_punct = T, remove_symbols = T, remove_numbers = T)
tks <- tokens_remove(tks, c(stopwords("en"), "oil", "curde", "said", "u."))
dfmat <- dfm(tks, stem = T, groups = "Year")
dfmat

## WordCloud
quanteda::textplot_wordcloud(dfm_subset(dfmat, Year == 2019), max_words = 100, color = "darkgreen")
quanteda::textplot_wordcloud(dfm_subset(dfmat, Year == 2020), max_words = 100, color = "red")


##-----------------------------------------------------------------------##
## Summary
sum_tks <- textstat_summary(tks) %>% select(tokens, types) %>% 
  mutate(Id=seq_along(tokens)) %>%
  pivot_longer(cols = c(tokens, types))

ggplot(sum_tks %>% filter(name == "tokens"), aes(x=log(value))) +
  geom_histogram(fill = "red") +
  theme_minimal()

statk <- textstat_keyness(dfmat, target = "2020") 

data_key <- bind_rows(statk %>% slice_max(chi2, n = 20),
          statk %>% slice_min(chi2, n = 20) %>% arrange(desc(chi2))) %>%
  as_tibble() %>%
  select(feature, chi2) %>% 
  mutate(id = seq_along(feature),
         Year = ifelse(id < 21, "2020", "2019"))

## 
ggplot(data_key, aes(x=id, y=chi2, fill = Year)) + geom_col() + 
  geom_text(aes(label=feature),  angle = 45, size = 3, 
            position = position_nudge(x=0, y = c(rep(20, 20), rep(-20, 20)))) +
  facet_wrap(~Year, scales = "free") + theme_minimal() +
  scale_fill_manual(values = colrs) + theme(axis.title.x=element_blank(),
                                            axis.text.x=element_blank(),
                                            axis.ticks.x=element_blank())

#textstat_lexdiv(tks)

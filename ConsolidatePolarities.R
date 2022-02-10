setwd("C:/Users/dordo/Documents/Daniel/LSE/MY 459/Proyecto")
library(tidyverse)
library(quanteda)
library(lubridate)
##--------------------------------------------------------------------------------------##
## Read Polarity Scores

## Semantic Axis 
data_sem <- read_csv("data/SemanticAxisPolarities.csv") %>% rename(SemanticAxis = Sim)
data_semMc <- read_csv("data/SemanticAxisPolaritiesMc.csv") %>% rename(SemanticAxisMc = Sim)

## Sentiprop
data_senti <- read_csv("data/SentiPropPolarities.csv") %>% rename(Sentiprop = polarity)
data_sentiMc <- read_csv("data/SentiPropPolaritiesMc.csv") %>% rename(SentipropMc = polarity)

## McDonald
#data_mcdof <- read_csv("data/McDonaldPolarities.csv") %>% rename(McDonald = Score)
#data_mcdo <- read_csv("data/McDonaldOriginal.csv") %>% rename(McDonald = Score)

## Join
scaler <- function(x){
  #(x-min(x))/(max(x)-min(x))
  as.numeric(scale(x))
}
pol_data <- inner_join(inner_join(data_sem, data_semMc),
                       inner_join(data_senti, data_sentiMc),
                       by =c("Word"="words")) %>%
  mutate(across(!c(Word), scaler))

rm(list=ls(pattern="^data"))

##--------------------------------------------------------------------------------------------##
## Exploratory plots
#z <- "Sentiprop"
get_keyw <- function(z, pol_data){
  bind_rows(pol_data %>% select(Word, .data[[z]]) %>% slice_max(.data[[z]], n = 10),
            pol_data %>% select(Word, .data[[z]])  %>% slice_min(.data[[z]], n = 10)) %>% 
    arrange(desc(.data [[z]]))
}

nams <- c("Sentiprop", "SentipropMc", "SemanticAxis", "SemanticAxisMc")
keyword <- lapply(nams, get_keyw,
       pol_data = pol_data)

tab <- do.call(cbind, map(keyword, 1))
colnames(tab) <- nams
xtable::xtable(tab[1:10,])


## Incoherency
pair_comp <- function(x, y, pol_data){
  use_data <- pol_data %>% select(Word, .data[[x]], .data[[y]]) 
  tab <- use_data %>%
    mutate(across(!Word, sign)) %>%
    rowwise() %>% 
    mutate(p = prod(c_across(!Word))) %>%
    count(p)
  list("Incoherency"=tab[[2]][1]/(tab[[2]][1]+tab[[2]][2]),
  "Cor"=cor(use_data[[2]], use_data[[3]]))
}

incs <- c(pair_comp("Sentiprop", "SentipropMc", pol_data)$Incoherency,
  pair_comp("Sentiprop", "SemanticAxis", pol_data)$Incoherency,
  pair_comp("Sentiprop", "SemanticAxisMc", pol_data)$Incoherency,
  pair_comp("SentipropMc", "SemanticAxis", pol_data)$Incoherency,
  pair_comp("SentipropMc", "SemanticAxisMc", pol_data)$Incoherency,
  pair_comp("SemanticAxis", "SemanticAxisMc", pol_data)$Incoherency)

incs_mat <- matrix(NA, 4, 4)
incs_mat[upper.tri(incs_mat)] <- incs
xtable::xtable(incs_mat)

pair_comp("SemanticAxisMc", "SemanticAxis", pol_data = pol_data)

library(GGally)
ggpairs(pol_data %>% select(-Word))


##--------------------------------------------------------------------------------------##
## Read news data
data <- read_rds("DataOil.Rdata") 

## Quanteda
datos <- corpus(data, docid_field = "Title", text_field = "Text")

dfm_oil <- dfm(datos, select = pol_data$Word) %>%
  dfm_weight(scheme = "prop")

weight_fun <- function(x, data){
  # weight by the weights
  dfm_c <- data  %>%   
    dfm_weight(weights = structure(pol_data[[x]], names=pol_data$Word))
  sents <- rowSums(dfm_c)
  out <- tibble(names(sents), sents)
  colnames(out) <- c("Title", x)
  out
}

## Use function above
vars <- c("Sentiprop", "SentipropMc", "SemanticAxis", "SemanticAxisMc")
res <- lapply(vars, weight_fun, data = dfm_oil)

## Reduce to inner join
data_score <- data %>% select(Date, Title, Author) %>% 
  left_join(reduce(res, left_join))

##---------------------------------------------------------------------------------##
## Append Mcdonald info
data_mcd <- left_join(read_csv("Data/McDonaldPolarities.csv") %>%
                        mutate(Score = ifelse(is.na(Score), 0, Score)),
                      read_csv("Data/McDonaldOriginal.csv") %>% 
                        select(Title, Positive, Negative) %>%
                        mutate(Total = (Positive - Negative)/(Negative + Positive)) %>%
                        mutate(Total = ifelse(is.na(Total), 0, Total)) %>%
                        select(Title, Total)) %>%
  rename(McDonaldF = Score, McDonald = Total)

data_score <- data_score %>% left_join(data_mcd)

##----------------------------------------------------------------------------------##
## Aggregate
pol_scores <- data_score %>% mutate(Date=round_date(Date, "day")) %>%
  group_by(Date) %>% 
  summarise(across(where(is.numeric), sum),
            N = n()) %>%
  select(Date, everything()) %>%
  arrange(Date)

## Save
#write_csv(pol_scores, "data/PolScores.csv")


## Plots

## By week 
res <- pol_scores %>% 
  mutate(Date = round_date(Date, unit = "week")) %>%
  group_by(Date) %>% 
  summarise(across(everything(),sum)) %>%
  filter(year(Date)>2018, year(Date)<2021) 

res %>% select(-N) %>%
  mutate(across(!Date, ~Get_trend(.x, type = "Henderson", m=4)),
         Date = as.Date(Date)) %>%
  pivot_longer(Sentiprop:McDonald, names_to = "Method") %>%
  ggplot(aes(x=Date, y=value, color = Method)) + geom_line() + 
  scale_x_date(date_labels = "%m-%y") + 
  facet_wrap(~Method, scales = "free", nrow = 3) + 
  theme_minimal()

# ggplot(pol_scores %>% 
#          pivot_longer(Sentiprop:SemanticAxisMc, names_to = "Method", values_to = "Score"), 
#        aes(x=Date, y=Score, col = Method)) + geom_line() + facet_wrap(~Method)
# 
# ggplot(pol_scores, aes(x=SemanticAxisMc, y=SemanticAxis)) + geom_point()
# ggplot(pol_scores, aes(x=Sentiprop, y=SentipropMc)) + geom_point()


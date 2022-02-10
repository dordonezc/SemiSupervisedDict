setwd("C:/Users/dordo/Documents/Daniel/LSE/MY 459/Proyecto")
library(tidyverse)
library(readxl)

## Read Polarity Scores
pol_scores <- read_csv("Data/PolScores.csv")

## Read controls
cntrls <- read_excel("Data/Controls.xlsx") %>%
  mutate(US10=ifelse(US10 == 0, NA, US10),
         Vix=ifelse(Vix == 0, NA, Vix),
         Gold = ifelse(Gold==0, NA, Gold)) %>%
  mutate(US10 = c(NA, diff(US10)),
         Gold = c(NA, diff(log(Gold))),
         Vix = c(NA, diff(Vix)))


## Read Oil Scores Forwards
oil_fws <- read_excel("Data/OilPrices.xlsx") %>% 
  mutate(Price = ifelse(Price > 0, Price, 0.01)) %>%
  fill(Price, .direction = "down") %>%
  mutate(DLPrice = c(NA,diff(log(Price))))

# oil <- read_excel("Data/OilPrices.xlsx", sheet=2)  %>% 
#   mutate(Price = ifelse(Price > 0, Price, 0)) %>%
#   fill(Price, .direction = "down") %>%
#   mutate(DLPrice = c(NA,diff(log(Price))))

#data_oil <- oil_fws %>% full_join(oil, by = "Date") %>% arrange(Date)
#lm(data_oil$Price.x ~ data_oil$Price.y) %>% summary()

##---------------------------------------------------------------------##
## Consolidate Dataset

data <- pol_scores %>% left_join(oil_fws) %>%
  filter(!is.na(Price)) %>% 
  mutate(DumCovid = as.numeric(year(Date) == 2020 & month(Date) > 2),
         DLPriceDir = ifelse(sign(DLPrice) == 1, 1, 0),
         McDonald = ifelse(!is.na(McDonald), McDonald, 0),
         Outlier = ifelse(Date == ymd("2020-04-20"), 1, 0),
         Outlier = ifelse(Date == ymd("2020-04-21"), -1, Outlier)) %>% 
  left_join(cntrls)

#"2020-04-21""2020-04-20"
## Modelling outliers
model <- forecast::auto.arima(ts(data$DLPrice, freq=5), xreg = data$Outlier)
data_use <- bind_cols(data, "Ehat"=residuals(model)) %>%
  filter(year(Date) > 2018 & year(Date) < 2021)


##--------------------------------------------------------------------##
## Basic Cross Correlation Analysis
ccf(data_use$DLPrice, data_use$McDonald, na.action = na.omit)


write_csv(data_use, "data/DataReg.csv")


##--------------------------------------------------------------------##
## Modelling

## Direction
# model_l <- glm(DLPriceDir ~ lag(SemanticAxisMc, 1), family = "binomial", data_use)
# summary(model_l)

## Regression
# model <- lm(Ehat ~ US10 + Gold + Vix + lag(McDonaldF, 1) + N + lag(Ehat, 2) + lag(Ehat, 3),
#             data_use) 
# summary(model)
# forecast::checkresiduals(model)

## Arima
# arima(data$DLPrice, order = c(0,0,7),
#       fixed = c(0, 0, 0, 0, 0, 0, NA, NA, NA), xreg = lag(data$McDonald, 1))
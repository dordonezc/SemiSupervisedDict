setwd("C:/Users/dordo/Documents/Daniel/LSE/MY 459/Proyecto")
library(tidyverse)
library(caret)
library(glmnet)

## Read csv
data <- read_csv("data/DataReg.csv")

## Auxiliary function for lags
get_lags <- function(x, y, max_l){
  x_use <- x %>% select(2)
  aux <- do.call(cbind,map(max_l:0,~lag(x_use,.x)))
  colnames(aux) <- str_c(y, str_c("_Lag", max_l:0))
  cbind(aux, "Date"=x[[1]])
}

go_pred <- function(z, data, max_lag = 1){
  ## Calculate Lags
  test <- data %>% select(Date, Ehat, .data[[z]], N, US10, Gold, Vix) %>%
    pivot_longer(Ehat:Vix) %>%
    group_nest(name)
  
  ## Generate lags
  test_df <- test %>%
    mutate(data = map2(data, test$name, get_lags, max_l = max_lag)) %>% 
    select(data) %>%
    pull() %>%
    reduce(left_join) %>%
    as_tibble() %>%
    select(Date, everything())
  
  ## Find missings and filter out
  nas <- rowwise(test_df) %>% summarise(s = sum(is.na(c_across(!Date))))
  X_mat <- as.matrix(test_df %>% select(-Date) %>% filter(nas == 0))
  
  set.seed(5)
  test_ind <- sample(1:nrow(X_mat), 50)
  
  ## Calculate model
  id <- (max_lag + 1)
  
  ## GLMNET
  # model <- cv.glmnet(X_mat[-test_ind, id], x = X_mat[-test_ind,-id])
  # yhat <- predict(model, X_mat[test_ind, -id], s = "lambda.min")
  
  ## Using leaps
  model <- regsubsets(x=X_mat[-test_ind,-id], y=X_mat[-test_ind, id])
  summ <- summary(model)
  use_reg<-summ$which[which.min(summ$bic),]
  new_mat <- cbind("Ehat"=X_mat[, id],X_mat[, -id][, use_reg[-1]])
  model_2 <- lm(Ehat ~ ., data = as.data.frame(new_mat[-test_ind,]))
  yhat <- predict(model_2, as.data.frame(new_mat[test_ind,]))
  
  rmse_t <- sqrt(mean((X_mat[test_ind, id] - yhat)^2))
  list(model_2, rmse_t, X_mat)
}

z <- c("Sentiprop", "SentipropMc", "SemanticAxis",
  "SemanticAxisMc", "McDonaldF", "McDonald")

res <- lapply(z, go_pred, data = data, max_lag = 2)
lapply(res, `[[`, 1)
lapply(res, `[[`, 2)


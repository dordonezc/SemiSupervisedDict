## 
pos_words <- readClipboard()
pos_modifiers <- pos_words %>% str_split(" ", simplify = T)
pos_modifiers <- pos_modifiers[pos_modifiers != ""]
pos_all <- c(pos_modifiers[,1], pos_modifiers[1:27,2])
pos_all <- c(pos_all, pos_modifiers)

pos_all <- pos_all[pos_all != ""]


write_csv(tibble("Word"=pos_all), "Data/CheckWordsMC.csv")

wts <- bind_rows(read_csv("Data/PositiveWordsMC.csv"),
          read_csv("Data/NegativeWordsMC.csv"))

a <- pos_modifiers %>% as.character()
a <- a[a!=""]
pos_all <- a[!a %in% wts$Word]

setwd("C:/Users/montg/Documents/active_projects/fantasy_football/expert_ranks")
library(dplyr)
file_names <- dir() 

df <- do.call(rbind,lapply(file_names,read.csv))
df$Overall <- gsub("\\s*\\([^\\)]+\\)","",as.character(df$Overall))
df$Overall <- gsub(x = df$Overall, pattern = paste(stopwords, collapse = "|"), replacement = "")
df$Overall <- sapply(df$Overall, toupper)
player_list <- unique(df$Overall)
stopwords <- c(" Jr.", " II", " III", " IV")



player_dist <- list()
best <- c()
worst<- c()
a <- 1
for (i in player_list){
  best[a] <- df %>% filter (Overall == i) %>% select(Best)
  worst[a] <- df %>% filter (Overall == i) %>% select(Worst)
  player_dist[[i]] <- c(worst[[a]], best[[a]])
  a <- a + 1
}

write.csv(player_list, "../player_list.csv")
save(player_dist, file = "../player_distributions.RData")

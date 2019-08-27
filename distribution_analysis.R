setwd("C:/Users/montg/Documents/active_projects/fantasy_football_algorithm")
load("player_distributions.RData")

mock <- read.csv("mock_results.csv")
mock[is.na(mock)] <- 175


mock_dist <- list()
for (i in mock$name){
  
  mock_dist[[i]] <-  density(as.numeric(mock[mock$name == i, 3:55]))
  
}



plot (mock_dist[["PATRICK MAHOMES"]])

save(mock_dist, file = "mock_distributions.RData")


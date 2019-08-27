setwd("C:/Users/montg/Documents/active_projects/fantasy_football_algorithm")
load("player_distributions.RData")

mock <- read.csv("mock_results.csv")
proj <- read.csv('projections.csv')



proj[!proj$name %in% mock$name & proj$rank < 200, 'name']

proj[!proj$name %in% names(player_dist), 'name']



mock[is.na(mock)] <- 175
mock <- mock[,-1]

mock_dist <- list()
for (i in mock$name){
  
  mock_dist[[i]] <-  (density(as.numeric(mock[mock$name == i, 3:54])))
  
}



save(mock_dist, file = "mock_distributions.RData")


proj[!proj$name %in% names(mock_dist) & proj$rank < 400, ]
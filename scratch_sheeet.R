setwd("C:/Users/montg/Documents/active_projects/fantasy_football")
load("player_distributions.RData")

library(dplyr)



#Step 1: Calculate player probabilities at each pick.
i <- 6
round <- 1
picks <- c()
for (n in seq(1:14))
{
  if (round %% 2 != 0){
    picks[n] <- ((round-1) * 12) + i 
    round <- round + 1
  }
  else{
    picks[n] <- picks[n - 1] + 2*(12-i) + 1
    round <- round + 1
  }
  
}

probabilities <- list()
for (i in names(player_dist)){
  pick_probs <- c()
  a <- 1
  for (j in picks){
    
    prob <- integrate(approxfun (density (player_dist[[i]]), rule = 2, method = "constant"),subdivisions=2000, lower = 0, upper = j, abs.tol = .0002)
    pick_probs[a] <- prob$value
    a <- a + 1
  }
probabilities[[i]] <- pick_probs 
}

probabilities[1]

df <- read.csv("projections.csv")
df <- df[df$name %in% names(probabilities),]

probabilities[1]

df_prob <- data.frame(matrix(ncol = length(picks) + 1, nrow = 1))
x <- c("name", "rd1", "rd2", "rd3", "rd4", "rd5", "rd6", "rd7", "rd8", "rd9", "rd10", "rd11", "rd12", "rd13", "rd14")
colnames(df_prob) <- x

for (i in 1:length(probabilities)){
  df_prob <- rbind(df_prob, c(names(probabilities[i]), probabilities[[i]]))
  
}
df_prob <- na.omit(df_prob)
df_prob <- merge(df[,c("points", "name", "pos")], df_prob)
df_prob[,2:16] <- sapply( df_prob[,2:16], as.numeric )
df_prob$name <- as.character(df_prob$name)

drop_players <- c("CINCINNATI", "OAKLAND")
df_prob <- df_prob[!(df_prob$name %in% drop_players),]
df_prob <- arrange(df_prob, desc(points))
#write.csv(df_prob, "vorp_table.csv")

round <- 1

vorp_frame <- data.frame(matrix(ncol = 5, nrow = 1))
x <- c( "DST", "QB", "RB", "TE", "WR")
colnames(vorp_frame) <- x
agg_points <- list()
agg_pots <- list()

for (i in picks){
  expected_pos_points <- c()
  for (j in unique(df_prob$pos)) {
    
  #prob_take <- Oarray :: Oarray(0 : nrow(df_prob[df_prob$pos == j,])     , offset = 0)
  prob_take <- c(0)  
  prob_available <- c()
  player <- c()
  points <- c()
  
  for (q in 1:nrow(df_prob[df_prob$pos == j,])) {
    prob_available[q] <- (1 - as.numeric(df_prob[ df_prob$pos == j, 3 + round][q]))
    prob_take[q] <- prob_available[q] * (1 - sum (prob_take )) 
    player[q] <- df_prob[df_prob$pos == j, "name"][q - 1]
    points[q] <- prob_take[q] * df_prob[df_prob$pos == j, 'points'][q]

    
  } 

  expected_pos_points[j] <- sum(points) 
  }
  vorp_frame <- rbind (vorp_frame, expected_pos_points)
  round <- round + 1
  
}
vorp_frame <- na.omit(vorp_frame)


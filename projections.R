library("ffanalytics")
setwd("C:/Users/montg/Documents/active_projects/fantasy_football")


my_scrape <- scrape_data(
                         pos = c("QB", "RB", "WR", "TE", "DST"),
                         src = c("CBS", "ESPN", "FantasyPros", "Yahoo", "NFL"),
                         season = 2019, week = 0)

my_projections <- projections_table(my_scrape)

player_names <- player_table %>% select(id, last_name, first_name, position) %>% rename(pos = position)

my_projections <- merge(my_projections, player_names)

my_projections <- my_projections %>% select(last_name,first_name, pos, id, points, rank, pos_rank, sd_pts, avg_type ) %>%
  filter (avg_type == "robust")


stopwords <- c(" Jr.", " II", " III", " IV")

my_projections$name <- paste(my_projections$first_name, my_projections$last_name, sep = " ")
my_projections$name <- ifelse(my_projections$pos == "DST", my_projections$first_name, my_projections$name)
my_projections$name <- gsub(x = my_projections$name, pattern = paste(stopwords, collapse = "|"), replacement = "")
my_projections$name <- sapply(my_projections$name, toupper)
my_projections$name <- ifelse(my_projections$name == "MITCHELL TRUBISKY", "MITCH TRUBISKY", my_projections$name)

write.csv(my_projections, "projections.csv")

library(dplyr)
library("jsonlite")

secrets <- function() {
  path <- "./secrets.json"
  if (!file.exists(path)) {
    stop("Can't find settings file")
  }
  jsonlite::read_json(path)
}

settings <- secrets()

setwd <- function(dir) {
  if (missing(dir) || is.null(dir) || dir == "") {
    dir <- settings[[5]]
  }
  base::setwd(dir)
}


## Home Page Data
## Save (~8MB of load)

setwd()
## Load Data
load_database <- function() {
  db <- read.csv('./data/detailed_stats.csv')
  return(db)
}
db <- load_database()

## Calculate total number of wins.
calc_wins <- function(database) {
  database$win <- NA
  if(length(database[database$margin_time > 0, ]$win) > 0) {
    database[database$margin_time > 0, ]$win <- 1
  }
  if(length(database[database$margin_time < 0, ]$win) > 0) {
    database[database$margin_time < 0, ]$win <- 0
  }
  if(length(database[database$margin_time == 0, ]$win) > 0) {
    database[database$margin_time == 0, ]$win <- 0
  }
  return(database)
}

db <- calc_wins(db)

## Get prem only and sum stats
db_summarised <- db %>% dplyr::filter(div=="prem") %>% dplyr::group_by(nickname) %>% dplyr::summarize(
  count = n(),
  kills = sum(kills),
  dpm = mean(dpm),
  assists = sum(assists),
  deaths = sum(deaths),
  gamescore = mean(gamescore),
  ubers = sum(ubers),
  drops = sum(drops),
  playoffs = sum(final),
  total_minutes = sum(mins_total),
  kills_per_map = sum(kills)/n(),
  deaths_per_map = sum(deaths)/n(),
  total_damage = sum(dpm*mins_total),
  total_caps = sum(cpm*mins_total),
  career_kd = sum(kills)/sum(deaths),
  wins = sum(win),
  damage_taken = sum(dtm*mins_total)
)

## Get current season
db_season <- db %>% dplyr::group_by(nickname, div) %>% dplyr::filter(season==31) %>% dplyr::summarize(
  count = n(),
  kills = sum(kills),
  dpm = mean(dpm),
  assists = sum(assists),
  deaths = sum(deaths),
  gamescore = mean(gamescore),
  ubers = sum(ubers),
  drops = sum(drops),
  playoffs = sum(final),
  total_minutes = sum(mins_total),
  kills_per_map = sum(kills)/n(),
  deaths_per_map = sum(deaths)/n(),
  total_damage = sum(dpm*mins_total),
  total_caps = sum(cpm*mins_total),
  season_kd = sum(kills)/sum(deaths)
)


## Semantic Dash
semantic_dash <- as.data.frame(0)
semantic_dash$no_seasons <- length(unique(db$season))
semantic_dash$no_maps <- length(unique(db$log_id))
semantic_dash$no_players <- length(unique(db$nickname))

setwd()
setwd("./data")
write.csv(db_season, "home_db_season.csv")
write.csv(db_summarised, "home_db_summarised.csv")
write.csv(semantic_dash, "home_semantic_dash.csv")

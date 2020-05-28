setwd()
setwd("./scripts")
library(rlist)
library(plotly)

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

cols <- c('nickname', 'class_primary', 'kills', 'assists', 'deaths', 'dpm', 'dtm', 'deathspm', 'kpm', 'cpm', 'gamescore', 'impact', 'survive',
          'efficiency', 'objective', 'perc_dpm', 'perc_kad', 'perc_dtm', 'perc_deathspm', 'perc_cpm', 'perc_kd', 'perc_kpm', 'perc_damageperheal',
          'perc_damageperdt', 'season', 'week', 'ranking_game', 'log_id', 'map', 'mins_total', 'div', 'ubers','drops', 'margin_time', 'final', 'hpm', 'healing')

adjust_med_stats <- function(class) {
  temp <- class
  med_vars <- c("dpm", "damageperdt", "kpm", "damageperheal","kd")
  for (var in med_vars) {
    perc_var <- paste("perc_",var,sep="")
    temp[[perc_var]] <- 0
  }
  return(temp)
}


run_detailed_analysis <- function(division) {
  player.data <- player.data %>% dplyr::filter(div == division)
  analysed = list()
  damage_classes <- c("scout", "demoman", "soldier")
  for (class in damage_classes) {
    analysed[[class]] <- player.data[player.data$class_primary == class, ]

    analysed[[class]] <- set_blank_exp(analysed[[class]])
    analysed[[class]] <- predict_stats(analysed[[class]], class)
    analysed[[class]] <- percentile_stats(analysed[[class]], class)
    ##GET SCORES - PROBABLY NEED TO FUNCTIONALLY PROGRAM THIS WHOLE THING.
    analysed[[class]]$impact <- (0.5*analysed[[class]]$perc_dpm + 0.1*analysed[[class]]$perc_kad + 0.2*analysed[[class]]$perc_kd + 0.2*analysed[[class]]$perc_kpm)
    analysed[[class]]$survive <- ((1-analysed[[class]]$perc_dtm) + (1-analysed[[class]]$perc_deathspm))/2
    analysed[[class]]$efficiency <- calc_effic_vec(analysed[[class]]$season, analysed[[class]]$perc_damageperdt, analysed[[class]]$perc_damageperheal)
    analysed[[class]]$perc_margin <- pnorm(analysed[[class]]$margin_time,0,sd(analysed[[class]]$margin_time))
    analysed[[class]]$objective <- (0.75*analysed[[class]]$perc_margin + 0.25*analysed[[class]]$perc_cpm)
    ##FIX SCORES TO HAVE MEAN OF 50%
    analysed[[class]]$impact <- ((analysed[[class]]$impact - 0.5)*analysed[[class]]$game_weight) + 0.5
    analysed[[class]]$survive <- ((analysed[[class]]$survive - 0.5)*analysed[[class]]$game_weight) + 0.5
    analysed[[class]]$efficiency <- ((analysed[[class]]$efficiency - 0.5)*analysed[[class]]$game_weight) + 0.5
    analysed[[class]]$objective <- ((analysed[[class]]$objective - 0.5)*analysed[[class]]$game_weight) + 0.5
    ##FIX OUTLIERS
    tryCatch({ analysed[[class]][analysed[[class]]$impact < 0.1,]$impact <- 0.1}, error = function(err) {print(err)}, finally = {})
    tryCatch({ analysed[[class]][analysed[[class]]$survive < 0.1,]$survive <- 0.1}, error = function(err) {print(err)}, finally = {})
    tryCatch({ analysed[[class]][analysed[[class]]$efficiency < 0.1,]$efficiency <- 0.1}, error = function(err) {print(err)}, finally = {})
    tryCatch({ analysed[[class]][analysed[[class]]$objective < 0.1,]$objective <- 0.1}, error = function(err) {print(err)}, finally = {})

    cap_vars <- c("impact", "survive", "efficiency", "objective")
    analysed[[class]] <- cap_one(analysed[[class]],cap_vars)
    ##GAME SCORE
    analysed[[class]]$gamescore <- (50*analysed[[class]]$impact) + (20*analysed[[class]]$efficiency) + (20*analysed[[class]]$survive) + (10*analysed[[class]]$objective)
    ##normalise
    analysed[[class]]$gamescore <- 15*((analysed[[class]]$gamescore - mean(analysed[[class]]$gamescore)))/sd(analysed[[class]]$gamescore) + 50
  }
  scout_tc <- analysed[["scout"]] %>% distinct()
  scout_tc <- scout_tc[, cols]

  soldier_tc <- analysed[["soldier"]] %>% distinct()
  soldier_tc <- soldier_tc[, cols]

  demoman_tc <- analysed[["demoman"]] %>% distinct()
  demoman_tc <- demoman_tc[, cols]

  analysed[["medic"]] <- player.data[player.data$class_primary == "medic", ]
  analysed[["medic"]] <- set_blank_med_exp(analysed[["medic"]])
  analysed[["medic"]] <- percentile_med_stats(analysed[["medic"]], "medic")

  analysed[["medic"]]$impact <- (0.5*analysed[["medic"]]$perc_healing + 0.3*analysed[["medic"]]$perc_chargespm + 0.2*analysed[["medic"]]$perc_kad)
  analysed[["medic"]]$survive <- 0.2*(1-analysed[["medic"]]$perc_dtm) + 0.5*(1-analysed[["medic"]]$perc_deathspm) + 0.3*(1-analysed[["medic"]]$perc_dropspm)
  analysed[["medic"]]$perc_margin <- pnorm(analysed[["medic"]]$margin_time,0,sd(analysed[["medic"]]$margin_time))
  analysed[["medic"]]$objective <- (0.8*analysed[["medic"]]$perc_margin + 0.2*analysed[["medic"]]$perc_cpm)
  analysed[["medic"]]$efficiency <- (0.2*(analysed[["medic"]]$perc_ubersperheal)) + (0.4*(1-analysed[["medic"]]$perc_deathsperdt)) + (0.4*(1-analysed[["medic"]]$perc_dropsperuber))
  ##SET MEAN
  analysed[["medic"]]$impact <- ((analysed[["medic"]]$impact - 0.5)*analysed[["medic"]]$game_weight) + 0.5
  analysed[["medic"]]$survive <- ((analysed[["medic"]]$survive - 0.5)*analysed[["medic"]]$game_weight) + 0.5
  analysed[["medic"]]$efficiency <- ((analysed[["medic"]]$efficiency - 0.5)*analysed[["medic"]]$game_weight) + 0.5
  analysed[["medic"]]$objective <- ((analysed[["medic"]]$objective - 0.5)*analysed[["medic"]]$game_weight) + 0.5
  ##FIX OUTLIERS
  tryCatch({ analysed[["medic"]][analysed[["medic"]]$impact < 0.1,]$impact <- 0.1 }, error = function(err) {print(err)}, finally = {})
  tryCatch({ analysed[["medic"]][analysed[["medic"]]$survive < 0.1,]$survive <- 0.1}, error = function(err) {print(err)}, finally = {})
  tryCatch({ analysed[["medic"]][analysed[["medic"]]$efficiency < 0.1,]$efficiency <- 0.1}, error = function(err) {print(err)}, finally = {})
  tryCatch({ analysed[["medic"]][analysed[["medic"]]$objective < 0.1,]$objective <- 0.1}, error = function(err) {print(err)}, finally = {})
  cap_vars <- c("impact", "survive", "efficiency", "objective")
  analysed[["medic"]] <- cap_one(analysed[["medic"]], cap_vars)

  analysed[["medic"]]$gamescore <- (50*analysed[["medic"]]$impact) + (20*analysed[["medic"]]$efficiency) + (20*analysed[["medic"]]$survive) + (10*analysed[["medic"]]$objective)

  med_tc <- analysed[["medic"]] %>% distinct()
  med_tc$gamescore <- 7*((med_tc$gamescore - mean(med_tc$gamescore))/sd(med_tc$gamescore)) + 47
  med_tc <- adjust_med_stats(med_tc)
  med_tc <- med_tc[, cols]

  return(rbind(scout_tc, soldier_tc, demoman_tc, med_tc) %>% distinct())

}

detailed_results_prem <- run_detailed_analysis("prem")
detailed_results_inter <- run_detailed_analysis("inter")
detailed_results_high <- run_detailed_analysis("high")
detailed_results <- rbind(detailed_results_prem, detailed_results_inter, detailed_results_high)
setwd()
setwd("./data")
write.csv(detailed_results, 'detailed_stats.csv')


##Filter to relevant

update_ranking_weight <- function(dataframe, current_game) {
  dataframe$ranking_game <- 0
  dataframe$ranking_weight <- 0
  dataframe$prev_ranking_weight <- 0
  dataframe$ranking_contr <- 0
  dataframe$prev_ranking_contr <- 0

  dataframe$ranking_game <- set_game_vec(dataframe$season, dataframe$week)
  dataframe$ranking_weight <- set_ranking_weight_vec(dataframe$ranking_game, current_game)
  return(dataframe)
}

detailed_results <- update_ranking_weight(detailed_results, last_game)

detailed_results <- detailed_results %>% dplyr::filter(ranking_weight > 0)

summed_dr <- detailed_results %>% group_by(nickname, class_primary) %>% summarize(
  total_games = n(),
  kills = mean(kills),
  assists = mean(assists),
  deaths = mean(deaths),
  dpm = mean(dpm),
  hpm = mean(hpm),
  kad = (mean(kills) + mean(assists)) / mean(deaths),
  kd = mean(kills) / mean(deaths),
  dtm = mean(dtm),
  deathspm = mean(deathspm),
  killspm = mean(kpm),
  cpm = mean(cpm),
  gamescore = mean(gamescore),
  impact = mean(impact),
  survive = mean(survive),
  efficiency = mean(efficiency),
  objective = mean(objective),
  perc_dpm = mean(perc_dpm),
  perc_kad = mean(perc_kad),
  perc_dtm = mean(perc_dtm),
  perc_deathspm = mean(perc_deathspm),
  perc_cpm = mean(perc_cpm),
  perc_kd = mean(perc_kd),
  perc_kpm = mean(perc_kpm),
  perc_damageperheal = mean(perc_damageperheal),
  perc_damageperdt = mean(perc_damageperdt)
)
setwd()
setwd("./data")
write.csv(summed_dr, 'summarised_stats.csv')

##Dependencies
library("ggplot2")
library("dplyr")
library("dbConnect")
library("fitdistrplus")
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


##Include External Files
source(file="generate_models.R")
source(file="generate_distributions.R")

##Connect to the database -
con <- dbConnect(RPostgres::Postgres(), dbname = "tf2",
                 host = settings[[1]], port = settings[[2]],
                 user = settings[[3]], password = settings[[4]])

##Get relevant database views
res <- dbSendQuery(con, "select * from public.player_match_database")
player.data <- dbFetch(res)

##Rename maps
rename_maps <- function(dataframe) {
  dataframe[dataframe$map %in% c('cp_granary_pro_rc8','cp_granary_pro_b10', 'cp_granary_pro_rc3', 'cp_granary_pro_rc4'), ]$map <- 'granary'
  dataframe[dataframe$map %in% c('cp_gullywash_final1'), ]$map <- 'gullywash'
  dataframe[dataframe$map %in% c('cp_process_final'),]$map <- 'process'
  dataframe[dataframe$map %in% c('koth_product_rc9','koth_product_rcx','cp_product_rcx','koth_product_rc8'),]$map <- 'product'
  dataframe[dataframe$map %in% c('cp_snakewater_final1','cp_snakewater_u13'),]$map <- 'snakewater'
  dataframe[dataframe$map %in% c('cp_kalinka_rc5'),]$map <- 'kalinka'
  dataframe[dataframe$map %in% c('cp_metalworks'),]$map <- 'metalworks'
  dataframe[dataframe$map %in% c('cp_prolands_b6','cp_prolands_rc2p','cp_badlands','cp_prolands_b2b','cp_prolands_b4b'),]$map <- 'prolands'
  dataframe[dataframe$map %in% c('cp_logjam_rc10a','cp_logjam_rc5','cp_logjam_rc11','cp_logjam_rc11x', 'cp_logjam_rc12f','cp_logjam_rc12'),]$map <- 'logjam'
  dataframe[dataframe$map %in% c('cp_sunshine','cp_sunshine_rc8','cp_sunshine_rc9'),]$map <- 'sunshine'
  dataframe[dataframe$map %in% c('cp_reckoner_rc6','cp_reckoner_rc4a','cp_reckoner_rc5','cp_reckoner_b2a','cp_reckoner_b3a','cp_reckoner_rc1','cp_reckoner_rc2'),]$map <- 'reckoner'
  dataframe[dataframe$map %in% c('koth_bagel_fall_b3'),]$map <- 'bagel'
  tryCatch({ dataframe[dataframe$map %in% c('cp_villa_b16a'),]$map <- 'villa'}, error = function(err) {print(err)}, finally = {})
  return(dataframe)
}

player.data <- rename_maps(player.data)

##Convert time & produce required stats
player.data$mins_total <- player.data$mins_total / 60
player.data$dpm <- player.data$damage / player.data$mins_total
player.data$hpm <- player.data$heals_received / player.data$mins_total
player.data$healing <- player.data$healing_done / player.data$mins_total
player.data$kad <- (player.data$kills + player.data$assists) / player.data$deaths
player.data$dtm <- player.data$damage_taken / player.data$mins_total
player.data$deathspm <- player.data$deaths / player.data$mins_total
player.data$cpm <- player.data$caps / player.data$mins_total
player.data$damageperheal <- player.data$damage / player.data$heals_received
player.data$damageperdt <- player.data$damage / player.data$damage_taken
player.data$kpm <- player.data$kills / player.data$mins_total
player.data$chargespm <- player.data$ubers / player.data$mins_total
player.data$dropspm <- player.data$drops / player.data$mins_total
player.data$dropsperuber <- player.data$drops / player.data$ubers
player.data$margin_time <- player.data$margin / player.data$mins_total
player.data$kd <- player.data$kills / player.data$deaths
player.data$ubersperheal <- player.data$chargespm / player.data$healing
player.data$deathsperdt <- player.data$deathspm / player.data$dtm

tryCatch({##Filter unrealistic results (is 700 the right number? - need to check this)
  player.data <- player.data[!player.data$dpm > 700, ]
  player.data[player.data$deathspm < 0.1,]$deathspm <- 0.1
  player.data[is.infinite(player.data$dropsperuber), ]$dropsperuber <- player.data[is.infinite(player.data$dropsperuber), ]$drops
  player.data[is.nan(player.data$dropsperuber), ]$dropsperuber <- 0
  ##Remove infinite values.
  player.data[is.infinite(player.data$kad), ]$kad <- player.data[is.infinite(player.data$kad), ]$kills + player.data[is.infinite(player.data$kad), ]$assists
  player.data[player.data$kad > 5, ]$kad <- 5
  player.data[is.infinite(player.data$kd), ]$kd <- player.data[is.infinite(player.data$kd), ]$kills
  player.data[is.nan(player.data$kd),]$kd <- 0
  player.data[is.infinite(player.data$damageperheal), ]$damageperheal <- 5
  player.data[is.nan(player.data$damageperheal),]$damageperheal <- 0
  player.data[player.data$damageperheal > 5, ]$damageperheal <- 5}, error = function(err) {print(err)}, finally = {})

##FIX NEGATIVE VALUES IF EXIST

player.data <- player.data %>% distinct()
##scale values for finals.
player.data$final <- 0
player.data[player.data$week > 7,]$final <- 1

adjust_finals_vars <- function(alldata, vars) {
  combat <- alldata[alldata$class_primary != "medic", ]
  finals_games <- combat[combat$final == 1,]
  season_games <- combat[combat$final == 0,]
  to_save <- data.frame(finalvar=0,
                        regmean=0,
                        finalsmean=0,
                        regsd=0,
                        finalsd=0)[numeric(0),]
  for(var in vars) {
    regmean <- mean(combat[combat$final == 0, ][[var]])
    finalsmean <- mean(combat[combat$final == 1, ][[var]])
    regsd <- sd(combat[combat$final == 0, ][[var]])
    finalsd <- sd(combat[combat$final == 1, ][[var]])
    finals_games[[var]] <- ((regsd)*((finals_games[[var]]-finalsmean)/(finalsd))) + regmean
    ##Save for use in log calculator.
    vals <- data.frame(
      finalvar=var,
      regmean=regmean,
      finalsmean=finalsmean,
      regsd=regsd,
      finalsd=finalsd
    )
    to_save <- rbind(to_save,vals)
  }
  combat <- rbind(finals_games,season_games)
  setwd()
  setwd("./data")
  write.csv(to_save, "finals_vars.csv")
  return(combat)
}


fix_negative <- function(combat, vars) {
  for(var in vars) {
    if(length(combat[combat[[var]] < 0 , ][[var]])) {
      combat[combat[[var]] < 0 ,][[var]] <- 0
    } else {}
  }
  return(combat)
}



cap_one <- function(data, vars) {
  for(var in vars) {
    if(length(data[data[[var]] > 1 , ][[var]])) {
      data[data[[var]] > 1 ,][[var]] <- 1
    } else {}
  }
  return(data)
}


finals_vars <- c("dpm", "dtm", "kpm", "kad", "kd", "cpm", "damageperdt", "damageperheal", "hpm")

combat.class <- adjust_finals_vars(player.data, finals_vars)
combat.class <- fix_negative(combat.class, finals_vars)
medic <- player.data[player.data$class_primary == "medic", ]
player.data <- rbind(combat.class,medic)


##method for weighting games based on time / margin
#normal dist, using limits -0.15 to 0.15 are 100%, cap at 75% reduction (0.5)

normal_dist <- function(mean, sd, x) {
  val <- (1/(sd*sqrt(2 * pi))) * exp(-0.5 * ((x-mean)/sd)^2)
  return(val)
}

game_weight <- function(week,margin_time) {
  if(week == 8) {
    week_weight <- 1.1
  } else if (week == 9) {
    week_weight <- 1.2
  } else if (week == 10) {
    week_weight <- 1.3
  } else {
    week_weight <- 1
  }

  ##set that for margin between 0.15/-0.15 (close ish game) - 100% worth
  ##for games with margin greater than 0.25/-0.25 reduce game to 60% worth
  scale100 <- normal_dist(0, sd(player.data$margin_time), 0.15)
  scale75 <- normal_dist(0, sd(player.data$margin_time), 0.25)
  if (margin_time >= -0.15 & margin_time <= 0.15) {
    return(week_weight * 1)
  } else if (margin_time >= 0.25 | margin_time <= -0.25) {
    return(week_weight * 0.6)
  }else {
    testmargin <- normal_dist(0,sd(player.data$margin_time),margin_time)
    weight <- 1 - ((scale100 - testmargin) / (scale100-scale75))*.4
    return(week_weight * weight)
  }
}

game_weight_vec <- Vectorize(game_weight, vectorize.args = c("week","margin_time"))

player.data$game_weight <- game_weight_vec(player.data$week, player.data$margin_time)

###
### MAIN ANALYSIS
###

run_analysis <- function(division) {
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
  scout_tc <- analysed[["scout"]] %>% dplyr::select(
    nickname, season, week, class_primary, log_id, gamescore,
    impact, survive, efficiency, objective, margin_time, game_weight) %>%
    distinct()

  soldier_tc <- analysed[["soldier"]] %>% dplyr::select(
    nickname, season, week, class_primary, log_id, gamescore,
    impact, survive, efficiency, objective, margin_time, game_weight) %>%
    distinct()

  demoman_tc <- analysed[["demoman"]] %>% dplyr::select(
    nickname, season, week, class_primary, log_id, gamescore,
    impact, survive, efficiency, objective, margin_time, game_weight) %>%
    distinct()

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

    med_tc <- analysed[["medic"]] %>% dplyr::select(
      nickname, season, week, class_primary, log_id, gamescore,
      impact, survive, efficiency, objective, margin_time, game_weight) %>%
      distinct()
    med_tc$gamescore <- 7*((med_tc$gamescore - mean(med_tc$gamescore))/sd(med_tc$gamescore)) + 47

  return(rbind(scout_tc, soldier_tc, demoman_tc,med_tc) %>% distinct())

}

##Game weight in current ranking
game_decay <- function(week) {
  l <- -(log(0.5))/10
  exp(-l * (week))
}

##Function assign "ranking game" to games based on season and game
set_game <- function(season, week) {
  game <- 10*(season - 14) + week
  return(game)
}
set_game_vec <- Vectorize(set_game, vectorize.args=c("season","week"))

player.data$ranking_game <- 0
player.data$ranking_game <- set_game_vec(player.data$season, player.data$week)
last_game <- max(player.data$ranking_game)

set_ranking_weight <- function(ranking_game,last_game) {
  if((ranking_game <= (last_game)) & (ranking_game > (last_game - 10))) {
    ranking_weight <- 1
  } else if ((ranking_game <= (last_game - 10)) & (ranking_game > (last_game - 20))) {
    ranking_weight <- game_decay(last_game - ranking_game - 9)
  } else  {
    ranking_weight <- 0
  }
  return(ranking_weight)
}

set_ranking_weight_vec <- Vectorize(set_ranking_weight, vectorize.args=c("ranking_game"))

player.data$ranking_weight <- 0

### GET MOST PLAYED CLASS
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


###
### FULL SEASON STATS
###
season_means <- function(dataframe) {
  seasons <- unique(dataframe$season)
  tmp <- dataframe
  for(season in seasons) {
    seasonmean <- mean(tmp[tmp$season == season, ]$gamescore)
    seasonsd <- sd(tmp[tmp$season == season, ]$gamescore)
    tmp[tmp$season == season, ]$gamescore <- ((10)*((tmp[tmp$season == season, ]$gamescore - seasonmean)/(seasonsd))) + 50
  }
  return(tmp)
}

generate_full_season_ranks <- function(all_games, filename) {
  season_ranks <- all_games %>% dplyr::group_by(nickname, season) %>% dplyr::summarize(
    gamescore = mean(gamescore),
    impact = mean(impact),
    survive = mean(survive),
    efficiency = mean(efficiency),
    objective = mean(objective),
    n = n(),
    classes = getmode(paste(class_primary,sep=""))
  ) %>% arrange(-gamescore)

  season_ranks <- season_means(season_ranks)
  season_ranks <- season_ranks %>% arrange(-gamescore)
  setwd()
  setwd("./data")
  write.csv(season_ranks, filename)
  print("Finished generating season rankings")
  return(season_ranks)
}

##Comparative Rankings
rating_reduction <- function(games_played) {
  l <- -log(0.9)
  red <- exp(-l*games_played)
  return(red)
}

reduce_rating <- function(ranking, n) {
  if (is.na(n)) {
    return(NA)
  }
  else if (n>=20) {
    return (ranking)
  } else {
    red <- rating_reduction(20 - n)
    return (ranking * red)
  }
}

reduce_rating_vec <- Vectorize(reduce_rating, vectorize.args=c("ranking","n"))

reduce_rating_non_prem <- function(ranking, n) {
  if (is.na(n)) {
    return(NA)
  }
  else if (n>=10) {
    return (ranking)
  } else {
    red <- rating_reduction(10 - n)
    return (ranking * red)
  }
}

reduce_rating_non_prem_vec <- Vectorize(reduce_rating_non_prem, vectorize.args=c("ranking","n"))

generate_rankings <- function(dataframe, current_game, string, division) {
  dataframe$ranking_game <- 0
  dataframe$ranking_weight <- 0
  dataframe$prev_ranking_weight <- 0
  dataframe$ranking_contr <- 0
  dataframe$prev_ranking_contr <- 0

  dataframe$ranking_game <- set_game_vec(dataframe$season, dataframe$week)
  dataframe$ranking_weight <- set_ranking_weight_vec(dataframe$ranking_game, current_game)
  dataframe$prev_ranking_weight <- set_ranking_weight_vec(dataframe$ranking_game, current_game - 1)

  dataframe$ranking_contr <- dataframe$gamescore * dataframe$ranking_weight
  dataframe$prev_ranking_contr <- dataframe$gamescore * dataframe$prev_ranking_weight

  games_on_record <- dataframe %>% filter(dataframe$ranking_contr > 0)
  prev_games_on_record <- dataframe %>% filter(dataframe$prev_ranking_contr > 0)

  previous_games <- prev_games_on_record %>% dplyr::group_by(nickname) %>% dplyr::summarize(
    prev_ranking_points = sum(prev_ranking_contr)/sum(prev_ranking_weight),
    prev_games = n()
  )

  current_rankings <- games_on_record %>% dplyr::group_by(nickname) %>% dplyr::summarize(
    ranking_points = sum(ranking_contr)/sum(ranking_weight),
    career_avg = mean(gamescore),
    n = n(),
    total_rank_weight = sum(ranking_weight),
    total_rank_contr = sum(ranking_contr),
    classes = getmode(paste(class_primary,sep="")),
    impact = mean(impact),
    survive = mean(survive),
    efficiency = mean(efficiency),
    objective = mean(objective),
  )
  if(division == "prem") {
    current_rankings$ranking_points <- reduce_rating_vec(current_rankings$ranking_points, current_rankings$n)
  } else {
    current_rankings$ranking_points <- reduce_rating_non_prem_vec(current_rankings$ranking_points, current_rankings$n)
  }
  if(nrow(current_rankings) > 0) {
    current_rankings$rank <- NA
    current_rankings$rank[order(-current_rankings$ranking_points)] <- 1:nrow(current_rankings)
  }
  if(nrow(previous_games) > 0) {
    if(division == "prem") {
      previous_games$prev_ranking_points <- reduce_rating_vec(previous_games$prev_ranking_points, previous_games$prev_games)
    } else {
      previous_games$prev_ranking_points <- reduce_rating_non_prem_vec(previous_games$prev_ranking_points, previous_games$prev_games)
    }
    previous_games$prev_rank <- NA
    previous_games$prev_rank[order(-previous_games$prev_ranking_points)] <- 1:nrow(previous_games)
  }
  
  current_rankings <- merge(current_rankings, previous_games, by="nickname", all=TRUE)
  if (nrow(previous_games) > 0) {
    current_rankings$rank_change <- current_rankings$prev_rank - current_rankings$rank
  } else if (is.null(string)) {
    #Do nothing when checking for peak rankings.
  } else {
    current_rankings$rank_change <- NA
    current_rankings$prev_rank <- NA
  }
  current_rankings <- current_rankings %>% dplyr::arrange(-ranking_points) %>% filter(rank > 0)

  setwd()
  setwd("./data")
  if(!is.null(string)) {
    write.csv(current_rankings,string)
    print("Finished generating current rankings")
  }
  return(current_rankings)
}

###SCRIPTING
##GENERATE DISTS & MODELS
generate_models(player.data)
generate_dists(player.data)
##ANALYSE PERFORMANCE
all_games <- run_analysis("prem")
all_games_inter <- run_analysis("inter")
all_games_high <- run_analysis("high")
##GENERATE RANKINGS
season_rankings <- generate_full_season_ranks(all_games, "season_ranks.csv")
season_rankings_inter <- generate_full_season_ranks(all_games_inter, "season_ranks_inter.csv")
season_rankings_high <- generate_full_season_ranks(all_games_high, "season_ranks_high.csv")
season_rankings$div <- "prem"
season_rankings_inter$div <- "inter"
season_rankings_high$div <- "high"
season_rankings_combined <- rbind(season_rankings, season_rankings_inter, season_rankings_high)
season_rankings_combined <- season_rankings_combined %>% arrange(-gamescore)
write.csv(season_rankings_combined, "season_rankings_combined.csv")

current_rankings <- generate_rankings(all_games, last_game, "current_rankings.csv", "prem")
current_rankings_inter <- generate_rankings(all_games_inter, last_game, "current_rankings_inter.csv", "inter")
current_rankings_high <- generate_rankings(all_games_high, last_game, "current_rankings_high.csv", "high")

get_peak <- function(peak_rank, new_rank) {
  if(is.na(peak_rank)) {
    val <- new_rank
  } else if(is.na(new_rank)) {
    val <- peak_rank
  } else {
    val <- min(peak_rank, new_rank)
    return(val)
  }
}

get_peak_vec <- Vectorize(get_peak, vectorize.args=c("peak_rank", "new_rank"))

##PEAK RANKINGS
gen_peaks <- function(current_rankings,last_x_games) {
  print("generating peaks")
  peak_rankings <- current_rankings %>% dplyr::group_by(nickname) %>% dplyr::select(nickname, rank)
  peak_rankings$peak_rank <- peak_rankings$rank
  peak_rankings <- peak_rankings[, c("nickname", "peak_rank")]

  for(i in 1:last_x_games) {
    this_ranking <- generate_rankings(all_games, last_game - i, NULL, "prem")
    this_ranking <- this_ranking %>% dplyr::select(nickname, rank)
    peak_rankings <- merge(peak_rankings, this_ranking, by="nickname", all=TRUE)
    peak_rankings$peak_rank <- get_peak_vec(peak_rankings$peak_rank, peak_rankings$rank)
    peak_rankings <- peak_rankings[,c("nickname", "peak_rank")]
  }
  print("peak rankings generated")
  
  current_rankings <- merge(current_rankings, peak_rankings, by="nickname", all.x=TRUE)
  current_rankings <- current_rankings %>% dplyr::arrange(-ranking_points)
  setwd()
  setwd("./data")
  write.csv(current_rankings,"current_rankings.csv")
  write.csv(peak_rankings, "peak_rankings.csv")
  print("Updated current rankings with peaks")
  return(current_rankings)
}

##Do it for the end of season 14 onward.
current_rankings <- gen_peaks(current_rankings, last_game-10)

##
##PEAK INTER RANKINGS
gen_peaks_inter <- function(current_rankings,last_x_games) {
  print("generating peaks inter")
  peak_rankings <- current_rankings %>% dplyr::group_by(nickname) %>% dplyr::select(nickname, rank)
  peak_rankings$peak_rank <- peak_rankings$rank
  peak_rankings <- peak_rankings[, c("nickname", "peak_rank")]

  for(i in 1:last_x_games) {
    this_ranking <- generate_rankings(all_games_inter, last_game - i, NULL, "inter")
    this_ranking <- this_ranking %>% dplyr::select(nickname, rank)
    peak_rankings <- merge(peak_rankings, this_ranking, by="nickname", all=TRUE)
    peak_rankings$peak_rank <- get_peak_vec(peak_rankings$peak_rank, peak_rankings$rank)
    peak_rankings <- peak_rankings[,c("nickname", "peak_rank")]
  }
  print("inter peak rankings generated")
  current_rankings <- merge(current_rankings, peak_rankings, by="nickname", all.x=TRUE)
  current_rankings <- current_rankings %>% dplyr::arrange(-ranking_points)
  setwd()
  setwd("./data")
  write.csv(current_rankings,"current_rankings_inter.csv")
  print("Updated current inter rankings with peaks")
  return(current_rankings)
}

##Do it for the end of season 25 onward.
current_rankings_inter <- gen_peaks_inter(current_rankings_inter, last_game-101)

##
##PEAK HIGH RANKINGS
gen_peaks_high <- function(current_rankings,last_x_games) {
  print("generating peaks high")
  peak_rankings <- current_rankings %>% dplyr::group_by(nickname) %>% dplyr::select(nickname, rank)
  peak_rankings$peak_rank <- peak_rankings$rank
  peak_rankings <- peak_rankings[, c("nickname", "peak_rank")]
  
  for(i in 1:last_x_games) {
    this_ranking <- generate_rankings(all_games_high, last_game - i, NULL, "high")
    this_ranking <- this_ranking %>% dplyr::select(nickname, rank)
    peak_rankings <- merge(peak_rankings, this_ranking, by="nickname", all=TRUE)
    peak_rankings$peak_rank <- get_peak_vec(peak_rankings$peak_rank, peak_rankings$rank)
    peak_rankings <- peak_rankings[,c("nickname", "peak_rank")]
  }
  print("high peak rankings generated")
  
  current_rankings <- merge(current_rankings, peak_rankings, by="nickname", all.x=TRUE)
  current_rankings <- current_rankings %>% dplyr::arrange(-ranking_points)
  setwd()
  setwd("./data")
  write.csv(current_rankings,"current_rankings_high.csv")
  print("Updated current high rankings with peaks")
  return(current_rankings)
}

##Do it for the end of season 25 onward.
current_rankings_high <- gen_peaks_high(current_rankings_high, last_game - 141)

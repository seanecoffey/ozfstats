## app.R ##
library(shiny)
library(Cairo)
library(dplyr)
library(reactable)
library(waiter)
library(shiny.semantic)
options(shiny.usecairo=T)

##Load Data
load_database <- function() {
  db <- read.csv('../../data/detailed_stats.csv')
  return(db)
}
db_summarised <- read.csv('../../data/home_db_summarised.csv')
db_season <- read.csv('../../data/home_db_season.csv')

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

#Get current season + week (~40kb load)
gamedata <- read.csv("../../data/gamedata.csv")
gamedata<-gamedata[order(-gamedata$season,-gamedata$week),]
currentseason<-head(gamedata,1)$season
currentweek<-head(gamedata,1)$week

ranking_column <- function(maxWidth = 125, class = NULL,...) {
  colDef(maxWidth = maxWidth, align = "center", class = paste("number", class), ...)
}
make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}

ranking_color <-  make_color_pal(c("#f14d4d", "#f8fcf8", "#44ab43"), bias = 0.8)
ranking_color_green <-  make_color_pal(c("#c0d292", "#44ab43"), bias = 2)
ranking_color_red <-  make_color_pal(c("#f27e7e","#f14d4d"), bias = 2)

###
# NEW TABLES -------------------------------------------------------
gen_count_table <- function(updateProgress = NULL){
  if(is.function(updateProgress)) {
    text <- "Generating ranking table"
    updateProgress(detail = text)
  }
  tbl <- reactable(
    db_summarised %>% top_n(5,count) %>% dplyr::select(nickname, count) %>% dplyr::arrange(-count),
    pagination=FALSE,
    highlight=TRUE,
    defaultColGroup = colGroup(headerClass = "header"),
    defaultColDef = colDef(headerClass = "header col-header"),
    columns = list(
      nickname = colDef(name = "Player Name",
                        headerStyle=list(fontWeight=700),
                        minWidth = 75,
                        cell = function(value, index) {
                          profile_url <- sprintf('https://app.ozfstats.com/playerProfiles/?_inputs_&classFilter="All"&player="%s"',value)
                          if (index == 1) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='first.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else if (index == 2) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='second.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )

                          } else if (index == 3) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='third.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else {
                            div(
                              class = "name",
                              htmltools::tags$img(src=""),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          }
                        }
      ),
      count = ranking_column(header = (span("Maps Played", title = "Total Maps Played from S14 onwards")),
                             format = colFormat(digits=0),
                             cell = function(value) {
                               value <- format(round(value, 0), nsmall = 0)
                               div(class = "peak_rank", value)
                             }
      )
    ),
    class = "standings-table"
  )
  if(is.function(updateProgress)) {
    text <- "Rendering"
    updateProgress(detail = text)
  }
  waiter_hide()
  return(tbl)
}

gen_kill_table <- function(the_database, updateProgress = NULL){
  if(is.function(updateProgress)) {
    text <- "Generating ranking table"
    updateProgress(detail = text)
  }
  tbl <- reactable(
    the_database %>% top_n(5,kills) %>% dplyr::select(nickname, kills) %>% dplyr::arrange(-kills),
    pagination=FALSE,
    highlight=TRUE,
    defaultColGroup = colGroup(headerClass = "header"),
    defaultColDef = colDef(headerClass = "header col-header"),
    columns = list(
      nickname = colDef(name = "Player Name",
                        headerStyle=list(fontWeight=700),
                        minWidth = 75,
                        cell = function(value, index) {
                          profile_url <- sprintf('https://app.ozfstats.com/playerProfiles/?_inputs_&classFilter="All"&player="%s"',value)
                          if (index == 1) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='first.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else if (index == 2) {
                            div(
                              class = "name",
                              div(class = "player-name",
                                  htmltools::tags$img(src='second.png'),
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )

                          } else if (index == 3) {
                            div(
                              class = "name",
                              div(class = "player-name",
                                  htmltools::tags$img(src='third.png'),
                                  htmltools::tags$a(href = profile_url, target = "_blank", value)),
                            )
                          } else {
                            div(
                              class = "name",
                              htmltools::tags$img(src=""),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          }
                        }
      ),
      kills = ranking_column(header = (span("Kills", title = "Total Kills from S14 onwards")),
                             format = colFormat(digits=0),
                             cell = function(value) {
                               value <- format(round(value, 0), nsmall = 0, big.mark=",")
                               div(class = "peak_rank", value)
                             }
      )
    ),
    class = "standings-table"
  )
  if(is.function(updateProgress)) {
    text <- "Rendering"
    updateProgress(detail = text)
  }
  waiter_hide()
  return(tbl)
}

gen_damage_table <- function(the_database, updateProgress = NULL){
  if(is.function(updateProgress)) {
    text <- "Generating ranking table"
    updateProgress(detail = text)
  }
  tbl <- reactable(
    the_database %>% top_n(5,total_damage) %>% dplyr::select(nickname, total_damage) %>% dplyr::arrange(-total_damage),
    pagination=FALSE,
    highlight=TRUE,
    defaultColGroup = colGroup(headerClass = "header"),
    defaultColDef = colDef(headerClass = "header col-header"),
    columns = list(
      nickname = colDef(name = "Player Name",
                        headerStyle=list(fontWeight=700),
                        minWidth = 75,
                        cell = function(value, index) {
                          profile_url <- sprintf('https://app.ozfstats.com/playerProfiles/?_inputs_&classFilter="All"&player="%s"',value)
                          if (index == 1) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='first.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else if (index == 2) {
                            div(
                              class = "name",
                              div(class = "player-name",
                                  htmltools::tags$img(src='second.png'),
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )

                          } else if (index == 3) {
                            div(
                              class = "name",
                              div(class = "player-name",
                                  htmltools::tags$img(src='third.png'),
                                  htmltools::tags$a(href = profile_url, target = "_blank", value)),
                            )
                          } else {
                            div(
                              class = "name",
                              htmltools::tags$img(src=""),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          }
                        }
      ),
      total_damage = ranking_column(header = (span("Damage Done", title = "")),
                             format = colFormat(digits=0),
                             cell = function(value) {
                               value <- format(round(value, 0), nsmall = 0, big.mark=",")
                               div(class = "peak_rank", value)
                             }
      )
    ),
    class = "standings-table"
  )
  if(is.function(updateProgress)) {
    text <- "Rendering"
    updateProgress(detail = text)
  }
  waiter_hide()
  return(tbl)
}

gen_damage_taken_table <- function(the_database, updateProgress = NULL){
  if(is.function(updateProgress)) {
    text <- "Generating ranking table"
    updateProgress(detail = text)
  }
  tbl <- reactable(
    the_database %>% top_n(5,damage_taken) %>% dplyr::select(nickname, damage_taken) %>% dplyr::arrange(-damage_taken),
    pagination=FALSE,
    highlight=TRUE,
    defaultColGroup = colGroup(headerClass = "header"),
    defaultColDef = colDef(headerClass = "header col-header"),
    columns = list(
      nickname = colDef(name = "Player Name",
                        headerStyle=list(fontWeight=700),
                        minWidth = 75,
                        cell = function(value, index) {
                          profile_url <- sprintf('https://app.ozfstats.com/playerProfiles/?_inputs_&classFilter="All"&player="%s"',value)
                          if (index == 1) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='first.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else if (index == 2) {
                            div(
                              class = "name",
                              div(class = "player-name",
                                  htmltools::tags$img(src='second.png'),
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )

                          } else if (index == 3) {
                            div(
                              class = "name",
                              div(class = "player-name",
                                  htmltools::tags$img(src='third.png'),
                                  htmltools::tags$a(href = profile_url, target = "_blank", value)),
                            )
                          } else {
                            div(
                              class = "name",
                              htmltools::tags$img(src=""),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          }
                        }
      ),
      damage_taken = ranking_column(header = (span("Damage Taken", title = "")),
                                    format = colFormat(digits=0),
                                    cell = function(value) {
                                      value <- format(round(value, 0), nsmall = 0, big.mark=",")
                                      div(class = "peak_rank", value)
                                    }
      )
    ),
    class = "standings-table"
  )
  if(is.function(updateProgress)) {
    text <- "Rendering"
    updateProgress(detail = text)
  }
  waiter_hide()
  return(tbl)
}

gen_wins_table <- function(the_database, updateProgress = NULL){
  if(is.function(updateProgress)) {
    text <- "Generating ranking table"
    updateProgress(detail = text)
  }
  tbl <- reactable(
    the_database %>% top_n(5,wins) %>% dplyr::select(nickname, wins) %>% dplyr::arrange(-wins),
    pagination=FALSE,
    highlight=TRUE,
    defaultColGroup = colGroup(headerClass = "header"),
    defaultColDef = colDef(headerClass = "header col-header"),
    columns = list(
      nickname = colDef(name = "Player Name",
                        headerStyle=list(fontWeight=700),
                        minWidth = 75,
                        cell = function(value, index) {
                          profile_url <- sprintf('https://app.ozfstats.com/playerProfiles/?_inputs_&classFilter="All"&player="%s"',value)
                          if (index == 1) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='first.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else if (index == 2) {
                            div(
                              class = "name",
                              div(class = "player-name",
                                  htmltools::tags$img(src='second.png'),
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )

                          } else if (index == 3) {
                            div(
                              class = "name",
                              div(class = "player-name",
                                  htmltools::tags$img(src='third.png'),
                                  htmltools::tags$a(href = profile_url, target = "_blank", value)),
                            )
                          } else {
                            div(
                              class = "name",
                              htmltools::tags$img(src=""),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          }
                        }
      ),
      wins = ranking_column(header = (span("Wins", title = "")),
                                    format = colFormat(digits=0),
                                    cell = function(value) {
                                      value <- format(round(value, 0), nsmall = 0, big.mark=",")
                                      div(class = "peak_rank", value)
                                    }
      )
    ),
    class = "standings-table"
  )
  if(is.function(updateProgress)) {
    text <- "Rendering"
    updateProgress(detail = text)
  }
  waiter_hide()
  return(tbl)
}

gen_playoffs_table <- function(the_database, updateProgress = NULL){
  if(is.function(updateProgress)) {
    text <- "Generating ranking table"
    updateProgress(detail = text)
  }
  tbl <- reactable(
    the_database %>% top_n(5,playoffs) %>% dplyr::select(nickname, playoffs) %>% dplyr::arrange(-playoffs),
    pagination=FALSE,
    highlight=TRUE,
    defaultColGroup = colGroup(headerClass = "header"),
    defaultColDef = colDef(headerClass = "header col-header"),
    columns = list(
      nickname = colDef(name = "Player Name",
                        headerStyle=list(fontWeight=700),
                        minWidth = 75,
                        cell = function(value, index) {
                          profile_url <- sprintf('https://app.ozfstats.com/playerProfiles/?_inputs_&classFilter="All"&player="%s"',value)
                          if (index == 1) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='first.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else if (index == 2) {
                            div(
                              class = "name",
                              div(class = "player-name",
                                  htmltools::tags$img(src='second.png'),
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )

                          } else if (index == 3) {
                            div(
                              class = "name",
                              div(class = "player-name",
                                  htmltools::tags$img(src='third.png'),
                                  htmltools::tags$a(href = profile_url, target = "_blank", value)),
                            )
                          } else {
                            div(
                              class = "name",
                              htmltools::tags$img(src=""),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          }
                        }
      ),
      playoffs = ranking_column(header = (span("Wins", title = "")),
                            format = colFormat(digits=0),
                            cell = function(value) {
                              value <- format(round(value, 0), nsmall = 0, big.mark=",")
                              div(class = "peak_rank", value)
                            }
      )
    ),
    class = "standings-table"
  )
  if(is.function(updateProgress)) {
    text <- "Rendering"
    updateProgress(detail = text)
  }
  waiter_hide()
  return(tbl)
}

gen_mins_table <- function(the_database, updateProgress = NULL){
  if(is.function(updateProgress)) {
    text <- "Generating ranking table"
    updateProgress(detail = text)
  }
  tbl <- reactable(
    the_database %>% top_n(5,total_minutes) %>% dplyr::select(nickname, total_minutes) %>% dplyr::arrange(-total_minutes),
    pagination=FALSE,
    highlight=TRUE,
    defaultColGroup = colGroup(headerClass = "header"),
    defaultColDef = colDef(headerClass = "header col-header"),
    columns = list(
      nickname = colDef(name = "Player Name",
                        headerStyle=list(fontWeight=700),
                        minWidth = 75,
                        cell = function(value, index) {
                          profile_url <- sprintf('https://app.ozfstats.com/playerProfiles/?_inputs_&classFilter="All"&player="%s"',value)
                          if (index == 1) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='first.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else if (index == 2) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='second.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )

                          } else if (index == 3) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='third.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else {
                            div(
                              class = "name",
                              htmltools::tags$img(src=""),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          }
                        }
      ),
      total_minutes = ranking_column(header = (span("Minutes Played", title = "Total Minutes Played from S14 onwards")),
                             format = colFormat(digits=0),
                             cell = function(value) {
                               value <- format(round(value, 0), nsmall = 0, big.mark=",")
                               div(class = "peak_rank", value)
                             }
      )
    ),
    class = "standings-table"
  )
  if(is.function(updateProgress)) {
    text <- "Rendering"
    updateProgress(detail = text)
  }
  waiter_hide()
  return(tbl)
}

gen_uber_table <- function(the_database, updateProgress = NULL){
  if(is.function(updateProgress)) {
    text <- "Generating ranking table"
    updateProgress(detail = text)
  }
  tbl <- reactable(
    the_database %>% top_n(5,ubers) %>% dplyr::select(nickname, ubers) %>% dplyr::arrange(-ubers),
    pagination=FALSE,
    highlight=TRUE,
    defaultColGroup = colGroup(headerClass = "header"),
    defaultColDef = colDef(headerClass = "header col-header"),
    columns = list(
      nickname = colDef(name = "Player Name",
                        headerStyle=list(fontWeight=700),
                        minWidth = 75,
                        cell = function(value, index) {
                          profile_url <- sprintf('https://app.ozfstats.com/playerProfiles/?_inputs_&classFilter="All"&player="%s"',value)
                          if (index == 1) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='first.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else if (index == 2) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='second.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )

                          } else if (index == 3) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='third.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else {
                            div(
                              class = "name",
                              htmltools::tags$img(src=""),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          }
                        }
      ),
      ubers = ranking_column(header = (span("Charges", title = "Total Ubers from S14 onwards")),
                                     format = colFormat(digits=0),
                                     cell = function(value) {
                                       value <- format(round(value, 0), nsmall = 0, big.mark=",")
                                       div(class = "peak_rank", value)
                                     }
      )
    ),
    class = "standings-table"
  )
  if(is.function(updateProgress)) {
    text <- "Rendering"
    updateProgress(detail = text)
  }
  waiter_hide()
  return(tbl)
}

gen_death_table <- function(the_database, updateProgress = NULL){
  if(is.function(updateProgress)) {
    text <- "Generating ranking table"
    updateProgress(detail = text)
  }
  tbl <- reactable(
    the_database %>% top_n(5,deaths) %>% dplyr::select(nickname, deaths) %>% dplyr::arrange(-deaths),
    pagination=FALSE,
    highlight=TRUE,
    defaultColGroup = colGroup(headerClass = "header"),
    defaultColDef = colDef(headerClass = "header col-header"),
    columns = list(
      nickname = colDef(name = "Player Name",
                        headerStyle=list(fontWeight=700),
                        minWidth = 75,
                        cell = function(value, index) {
                          profile_url <- sprintf('https://app.ozfstats.com/playerProfiles/?_inputs_&classFilter="All"&player="%s"',value)
                          if (index == 1) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='first.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else if (index == 2) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='second.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )

                          } else if (index == 3) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='third.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else {
                            div(
                              class = "name",
                              htmltools::tags$img(src=""),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          }
                        }
      ),
      deaths = ranking_column(header = (span("Deaths", title = "Total Deaths from S14 onwards")),
                             format = colFormat(digits=0),
                             cell = function(value) {
                               value <- format(round(value, 0), nsmall = 0, big.mark=",")
                               div(class = "peak_rank", value)
                             }
      )
    ),
    class = "standings-table"
  )
  if(is.function(updateProgress)) {
    text <- "Rendering"
    updateProgress(detail = text)
  }
  waiter_hide()
  return(tbl)
}

gen_assist_table <- function(the_database, updateProgress = NULL){
  if(is.function(updateProgress)) {
    text <- "Generating ranking table"
    updateProgress(detail = text)
  }
  tbl <- reactable(
    the_database %>% top_n(5,assists) %>% dplyr::select(nickname, assists) %>% dplyr::arrange(-assists),
    pagination=FALSE,
    highlight=TRUE,
    defaultColGroup = colGroup(headerClass = "header"),
    defaultColDef = colDef(headerClass = "header col-header"),
    columns = list(
      nickname = colDef(name = "Player Name",
                        headerStyle=list(fontWeight=700),
                        minWidth = 75,
                        cell = function(value, index) {
                          profile_url <- sprintf('https://app.ozfstats.com/playerProfiles/?_inputs_&classFilter="All"&player="%s"',value)
                          if (index == 1) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='first.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else if (index == 2) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='second.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )

                          } else if (index == 3) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='third.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else {
                            div(
                              class = "name",
                              htmltools::tags$img(src=""),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          }
                        }
      ),
      assists = ranking_column(header = (span("Assists", title = "Total Assists from S14 onwards")),
                              format = colFormat(digits=0),
                              cell = function(value) {
                                value <- format(round(value, 0), nsmall = 0, big.mark=",")
                                div(class = "peak_rank", value)
                              }
      )
    ),
    class = "standings-table"
  )
  if(is.function(updateProgress)) {
    text <- "Rendering"
    updateProgress(detail = text)
  }
  waiter_hide()
  return(tbl)
}

gen_capture_table <- function(the_database, updateProgress = NULL){
  if(is.function(updateProgress)) {
    text <- "Generating ranking table"
    updateProgress(detail = text)
  }
  tbl <- reactable(
    the_database %>% top_n(5,total_caps) %>% dplyr::select(nickname, total_caps) %>% dplyr::arrange(-total_caps),
    pagination=FALSE,
    highlight=TRUE,
    defaultColGroup = colGroup(headerClass = "header"),
    defaultColDef = colDef(headerClass = "header col-header"),
    columns = list(
      nickname = colDef(name = "Player Name",
                        headerStyle=list(fontWeight=700),
                        minWidth = 75,
                        cell = function(value, index) {
                          profile_url <- sprintf('https://app.ozfstats.com/playerProfiles/?_inputs_&classFilter="All"&player="%s"',value)
                          if (index == 1) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='first.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else if (index == 2) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='second.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )

                          } else if (index == 3) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='third.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else {
                            div(
                              class = "name",
                              htmltools::tags$img(src=""),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          }
                        }
      ),
      total_caps = ranking_column(header = (span("Points Captured", title = "Caps from S14 onwards")),
                               format = colFormat(digits=0),
                               cell = function(value) {
                                 value <- format(round(value, 0), nsmall = 0, big.mark=",")
                                 div(class = "peak_rank", value)
                               }
      )
    ),
    class = "standings-table"
  )
  if(is.function(updateProgress)) {
    text <- "Rendering"
    updateProgress(detail = text)
  }
  waiter_hide()
  return(tbl)
}

gen_drop_table <- function(the_database, updateProgress = NULL){
  if(is.function(updateProgress)) {
    text <- "Generating ranking table"
    updateProgress(detail = text)
  }
  tbl <- reactable(
    the_database %>% top_n(5,drops) %>% dplyr::select(nickname, drops) %>% dplyr::arrange(-drops),
    pagination=FALSE,
    highlight=TRUE,
    defaultColGroup = colGroup(headerClass = "header"),
    defaultColDef = colDef(headerClass = "header col-header"),
    columns = list(
      nickname = colDef(name = "Player Name",
                        headerStyle=list(fontWeight=700),
                        minWidth = 75,
                        cell = function(value, index) {
                          profile_url <- sprintf('https://app.ozfstats.com/playerProfiles/?_inputs_&classFilter="All"&player="%s"',value)
                          if (index == 1) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='first.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else if (index == 2) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='second.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )

                          } else if (index == 3) {
                            div(
                              class = "name",
                              htmltools::tags$img(src='third.png'),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          } else {
                            div(
                              class = "name",
                              htmltools::tags$img(src=""),
                              div(class = "player-name",
                                  htmltools::tags$a(href = profile_url, target = "_blank", value))
                            )
                          }
                        }
      ),
      drops = ranking_column(header = (span("Drops", title = "Total drops from S14 onwards")),
                                       format = colFormat(digits=0),
                                       cell = function(value) {
                                         value <- format(round(value, 0), nsmall = 0, big.mark=",")
                                         div(class = "peak_rank", value)
                                       }
      )
    ),
    class = "standings-table"
  )
  if(is.function(updateProgress)) {
    text <- "Rendering"
    updateProgress(detail = text)
  }
  waiter_hide()
  return(tbl)
}

##UI
ui <- fluidPage(
  use_waiter(),
  waiter_show_on_load(
    spin_loaders(37),
    color="#212529"
  ),
  tags$link(href = "https://fonts.googleapis.com/css?family=Karla:400,700|Fira+Mono&display=fallback", rel = "stylesheet"),
  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?rnd132"),
  column(12,
         br(),
         fluidRow(column(width=12, h3("A statistics-based ranking system for Australia's Team Fortress 2 competitive league, ozfortress"), align = "center")),
         br(),
         fluidRow(column(width=12, uiOutput('statRow'), align="center")),
         hr(),
         fluidRow(column(width=12,
                         div(class = "title", h1(paste("Season ", currentseason, " Premier Division Leaders", sep="")))
                         ), align = "center"
         ),
         fluidRow(column(3,
                         div(class = "title", h2(htmltools::tags$img(src="https://img.icons8.com/nolan/64/accuracy.png"), "Kills")),
                         reactableOutput("premseasonkills")),
                  column(3,
                         div(class = "title", h2(htmltools::tags$img(src="https://img.icons8.com/nolan/64/helping-hand.png"), "Assists")),
                         reactableOutput("premseasonassists")),
                  column(3,
                         div(class = "title", h2(htmltools::tags$img(src="https://img.icons8.com/nolan/64/explosion.png"), "Damage")),
                         reactableOutput("premseasondamage")),
                  column(3,
                         div(class = "title", h2(htmltools::tags$img(src="https://img.icons8.com/nolan/64/lightning-bolt.png"), "Charges")),
                         reactableOutput("premseasonubers"))
         ),
         fluidRow(column(width=12,
                         div(class = "title", h1(paste("Season ", currentseason, " Intermediate Division Leaders", sep="")))
              ), align = "center"
         ),
         fluidRow(column(3,
                         div(class = "title", h2(htmltools::tags$img(src="https://img.icons8.com/nolan/64/accuracy.png"), "Kills")),
                         reactableOutput("interseasonkills")),
                  column(3,
                         div(class = "title", h2(htmltools::tags$img(src="https://img.icons8.com/nolan/64/helping-hand.png"), "Assists")),
                         reactableOutput("interseasonassists")),
                  column(3,
                         div(class = "title", h2(htmltools::tags$img(src="https://img.icons8.com/nolan/64/explosion.png"), "Damage")),
                         reactableOutput("interseasondamage")),
                  column(3,
                         div(class = "title", h2(htmltools::tags$img(src="https://img.icons8.com/nolan/64/lightning-bolt.png"), "Charges")),
                         reactableOutput("interseasonubers"))
         ),
         br(),
         fluidRow(column(width=12,
                         div(class = "title", h1("Premier Division Career Leaders"))
                         ), align = "center"
                  ),
         fluidRow(column(3,
                         div(class = "title", h2(htmltools::img(src="https://img.icons8.com/nolan/64/map.png"), "Maps")),
                         reactableOutput("careermaps")),
                  column(3,
                         div(class = "title", h2(htmltools::img(src="https://img.icons8.com/nolan/64/trophy.png"), "Map Wins")),
                         reactableOutput("careerwins")),
                  column(3,
                         div(class = "title", h2(htmltools::img(src="https://img.icons8.com/nolan/64/future.png"), "Playtime")),
                         reactableOutput("careerminutes")),
                  column(3,
                         div(class = "title", h2(htmltools::img(src="https://img.icons8.com/nolan/64/diamond.png"), "Playoffs")),
                         reactableOutput("careerplayoffs"))
                  ),

         fluidRow(
                  column(3,
                          div(class = "title", h2(htmltools::tags$img(src="https://img.icons8.com/nolan/64/accuracy.png"), "Kills")),
                          reactableOutput("careerkills")),
                  column(3,
                         div(class = "title", h2(htmltools::tags$img(src="https://img.icons8.com/nolan/64/helping-hand.png"), "Assists")),
                         reactableOutput("careerassists")),
                  column(3,
                         div(class = "title", h2(htmltools::tags$img(src="https://img.icons8.com/nolan/64/explosion.png"), "Damage")),
                         reactableOutput("careerdamage")),
                  column(3,
                         div(class = "title", h2(htmltools::tags$img(src="https://img.icons8.com/nolan/64/lightning-bolt.png"), "Charges")),
                         reactableOutput("careerubers"))
         ),
         fluidRow(
                  column(3,
                        div(class = "title", h2(htmltools::img(src="https://img.icons8.com/nolan/64/skull.png"), "Deaths")),
                        reactableOutput("careerdeaths")),
                  column(3,
                         div(class = "title", h2(htmltools::img(src="https://img.icons8.com/nolan/64/flash-bang.png"), "Damage Taken")),
                         reactableOutput("careerdamagetaken")
                         ),
                  column(3,
                         div(class = "title", h2(htmltools::img(src="https://img.icons8.com/nolan/64/place-marker.png"), "Points Captured")),
                         reactableOutput("careercaptures")
                         ),
                  column(3,
                         div(class = "title", h2(htmltools::img(src="https://img.icons8.com/nolan/64/mushroom-cloud.png"), "Drops")),
                         reactableOutput("careerdrops")
                         )
         ),
         fluidRow(column(12,
                         fluidRow(column(10,
                                         fluidRow(textOutput("edit_date")),
                                         )
                                  )
                         )
                  ), "class"="standings")
  )

## SERVER
server <- function(input,output,session) {

  output$edit_date <- renderText({
    paste("Rankings as at end of Season ",currentseason, " week ", currentweek, ". Last update was at: ", toString(file.info("../../data/current_rankings.csv")$mtime), sep="")
  })

  output$careermaps <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_count_table(updateProgress)
    })

  output$careerkills <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_kill_table(db_summarised, updateProgress)
  })

  output$careerminutes <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_mins_table(db_summarised, updateProgress)
  })

  output$careerubers <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_uber_table(db_summarised, updateProgress)
  })

  output$careerdeaths <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_death_table(db_summarised, updateProgress)
  })

  output$careerwins <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_wins_table(db_summarised, updateProgress)
  })

  output$careerplayoffs <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_playoffs_table(db_summarised, updateProgress)
  })

  output$careerassists <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_assist_table(db_summarised, updateProgress)
  })

  output$careerdamage <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_damage_table(db_summarised, updateProgress)
  })

  output$careerdamagetaken <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_damage_taken_table(db_summarised, updateProgress)
  })

  output$careercaptures <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_capture_table(db_summarised, updateProgress)
  })


  output$careerdrops <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_drop_table(db_summarised, updateProgress)
  })

  ##PREM SEASON

  output$premseasonkills <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_kill_table(db_season %>% dplyr::filter(div=="prem") %>% dplyr::ungroup() , updateProgress)
  })

  output$premseasondamage <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_damage_table(db_season %>% dplyr::filter(div=="prem") %>% dplyr::ungroup(), updateProgress)
  })


  output$premseasonassists <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_assist_table(db_season %>% dplyr::filter(div=="prem") %>% dplyr::ungroup(), updateProgress)
  })

  output$premseasonubers <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_uber_table(db_season %>% dplyr::filter(div=="prem") %>% dplyr::ungroup(), updateProgress)
  })

  ##HIGH STATS
  output$highseasonkills <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_kill_table(db_season %>% dplyr::filter(div=="high") %>% dplyr::ungroup() , updateProgress)
  })

  output$highseasondamage <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_damage_table(db_season %>% dplyr::filter(div=="high") %>% dplyr::ungroup(), updateProgress)
  })


  output$highseasonassists <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_assist_table(db_season %>% dplyr::filter(div=="high") %>% dplyr::ungroup(), updateProgress)
  })

  output$highseasonubers <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_uber_table(db_season %>% dplyr::filter(div=="high") %>% dplyr::ungroup(), updateProgress)
  })

  ##INTER STATS
  output$interseasonkills <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_kill_table(db_season %>% dplyr::filter(div=="inter") %>% dplyr::ungroup() , updateProgress)
  })

  output$interseasondamage <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_damage_table(db_season %>% dplyr::filter(div=="inter") %>% dplyr::ungroup(), updateProgress)
  })


  output$interseasonassists <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_assist_table(db_season %>% dplyr::filter(div=="inter") %>% dplyr::ungroup(), updateProgress)
  })

  output$interseasonubers <- renderReactable({
    progress <- shiny::Progress$new()
    progress$set(message = "Loading...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    gen_uber_table(db_season %>% dplyr::filter(div=="inter") %>% dplyr::ungroup(), updateProgress)
  })

  ##OTHER

  output$statRow <- renderUI({
    semanticPage(
      div(class = "ui statistics  statistic-bottom",
          div(class = "blue statistic statistic-even",
              div(class = "value", length(unique(db$season))),
              div(class = "label", "Seasons")
          ),
          div(class = "teal statistic statistic-even",
              div(class = "value", format(round(as.numeric(length(unique(db$log_id)))/2.061417, 0), nsmall=0, big.mark=",")),
              div(class = "label", "Matches")
          ),
          div(class = "purple statistic statistic-even",
              div(class = "value", format(round(as.numeric(length(unique(db$log_id))), 1), nsmall=0, big.mark=",")),
              div(class = "label", "Maps")
          ),
          div(class = "pink statistic statistic-even",
              div(class = "value", length(unique(db$nickname))),
              div(class = "label", "Players")
          )
      )
    )
  })

  output$careerDivider <- renderUI({
    semanticPage(
      div(class = "ui horizontal divider header",
          div(class = "tag icon", "Premier Division Career Leaders (S14 Onwards)")
          )
    )
  })

}

shinyApp(ui, server)

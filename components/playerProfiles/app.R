## TO DO
##Division indicators prem/inter / etc.
##magic sauce calculation.....
##Fix y-axis labelling on main plot.

#PlayerProfiles
library(shiny)
library(plotly)
library(rlist)
library(Cairo)
library(reactable)
library(dplyr)
library(shinyFeedback)
library(shiny.semantic)
options(shiny.usecairo=T)

##LOAD
player_names <- read.csv('../../data/players.csv')
prem_rankings <- read.csv('../../data/current_rankings.csv')
peak_rankings <- read.csv('../../data/peak_rankings.csv')
##TEMP
magic_sauce <- as.data.frame(floor(runif(nrow(player_names),min=1, max=99)))
colnames(magic_sauce) <- c("magic_sauce")

player_names <- cbind(player_names, magic_sauce)

##functions
get_current_ranking <- function(player) {
  rank <- prem_rankings[prem_rankings$nickname == player,]$rank
  if(length(rank) == 0) {
    rank <- "-"
  }
  return(rank)
}

get_peak_ranking <- function(player) {
  rank <- peak_rankings[peak_rankings$nickname == player,]$peak_rank
  if(length(rank) == 0) {
    rank <- "-"
  }
  return(rank)
}

convert_steam_id3 <- function(steamid) {
  steamid64ident <- bit64::as.integer64(76561197960265728)
  commid <- gsub('\\[','', steamid)
  commid <- gsub('\\]','', commid)
  commid <- unlist(strsplit(commid, ":"))[[3]]
  commid <- bit64::as.integer64(commid)
  commid <- commid + steamid64ident
  return(commid)
}

load_database <- function() {
  db <- read.csv('../../data/detailed_stats.csv')
  return(db)
}

get_names <- function(db) {
  db <- db %>% arrange(nickname)
  names <- unique(db$nickname)
  return(names)
}

player_filter <- function(db, player, class = NULL, division = NULL) {
  filtered <- db %>% dplyr::filter(nickname == player)
  if(!is.null(class)) {
    if(class == 'All') {
      class <- c('scout', 'soldier', 'demoman', 'medic')
    }
    filtered <- filtered %>% filter(class_primary %in% class)
  }

  if(!is.null(division)) {
    if (division == 'All') {
      division <- c('prem', 'high', 'inter')
    }
    filtered <- filtered %>% filter(div %in% division)
  }
  return(filtered)
}

count_games <- function(filtered_db) {
  games <- length(unique(filtered_db$ranking_game))
  return(games)
}

count_maps <- function(filtered_db) {
  maps <- nrow(filtered_db)
  return(maps)
}

count_seasons <- function(filtered_db) {
  seasons <- length(unique(filtered_db$season))
  return(seasons)
}

count_class <- function(filtered_db, class) {
  class_filtered <- filtered_db %>% filter(class_primary == class)
  games_played <- length(unique(class_filtered$ranking_game))
  return(games_played)
}

win_percentage <- function(filtered_db) {
  if(nrow(filtered_db) == 0) {
    return(0)
  }
  filtered_db$win <- NA
  if(length(filtered_db[filtered_db$margin_time > 0 ,]$win) > 0) {
    filtered_db[filtered_db$margin_time > 0 ,]$win <- 1
  }
  if (length(filtered_db[filtered_db$margin_time < 0 ,]$win) > 0) {
    filtered_db[filtered_db$margin_time < 0 ,]$win <- 0
  }
  if (length(filtered_db[filtered_db$margin_time == 0 ,]$win) > 0) {
    filtered_db[filtered_db$margin_time == 0 ,]$win <- 0
  }
  wins <- filtered_db %>% group_by(win) %>% dplyr::summarize(count = n())
  map_wins <- wins[wins$win==1,]$count
  if(length(map_wins >0)) {
    map_wins <- wins[wins$win==1,]$count[[1]]
  } else if(length(map_wins == 0)) {
    map_wins <- 0
  }
  total_games <- nrow(filtered_db)

  win_perc <- map_wins/total_games
  if(length(win_perc) == 0) {
    win_perc <- 0
  }
  return(win_perc)
}

win_perc_colour <- function(win_perc) {
  if(win_perc < 0.2) {
    colour <- "red"
  }
  else if(win_perc < 0.35) {
    colour <- "orange"
  }
  else if(win_perc < 0.55) {
    colour <- "yellow"
  }
  else if(win_perc < 0.70) {
    colour <- "olive"
  }
  else {
    colour <- "green"
  }
  return(colour)
}

career_avg_colour <- function(career_avg) {
  if(career_avg < 31.2) {
    colour <- "red"
  }
  else if(career_avg < 38.7) {
    colour <- "orange"
  }
  else if(career_avg < 46.2) {
    colour <- "yellow"
  }
  else if(career_avg < 53.7) {
    colour <- "olive"
  }
  else if(career_avg < 61.2) {
    colour <- "green"
  }
  else {
    colour <- "teal"
  }
  return(colour)
}

rank_colour <- function(rank) {
  if(rank < 5) {
    colour <- "teal"
  }
  else if(rank < 15) {
    colour <- "green"
  }
  else if(rank < 35) {
    colour <- "olive"
  }
  else if(rank < 50) {
    colour <- "yellow"
  }
  else {
    colour <- "orange"
  }
  return(colour)
}

per_map_stats <- function(filtered_db, class = NULL) {
  if(nrow(filtered_db) > 0) {
    by_map <- filtered_db %>% group_by(map) %>% summarise(
      "Times Played" = n(),
      "Average Game Score" = round(mean(gamescore), digits=1)
    ) %>% arrange(-`Average Game Score`)
    return(reactable(by_map, pagination=FALSE))
  } else {
    by_map <- data.frame()
    return(reactable(by_map))
  }
}

get_most_played_class <- function(player_db) {
  classes <- unique(player_db$class_primary)
  most_played <- classes[which.max(tabulate(match(player_db$class_primary, classes)))]
  return(most_played)
}

season_averages <- function(player_db) {
  by_season <- player_db %>% dplyr::group_by(season) %>% dplyr::summarize(
    ranking_game = mean(ranking_game),
    gamescore = mean(gamescore)
  )
  return(by_season)
}

count_career_avgs <- function(filtered_db) {
  temp_db <- filtered_db %>% group_by(nickname) %>% dplyr::summarize(
    kd = mean(kd),
    kad = mean(kad),
    dpm = mean(dpm)
  )

  tbl <- reactable(temp_db, pagination=FALSE)

  return(tbl)
}

get_career_ave <- function(filtered_db) {
  tmp <- filtered_db %>% dplyr::summarize(
    gamescore = mean(gamescore)
  )
  return(tmp)
}

##STEAM AVATRS
get_steam_avatar <- function(player) {
  steam_id <- player_names[player_names$nickname == player, 'steam_id']
  steam_api_url <- paste('http://api.steampowered.com/ISteamUser/GetPlayerSummaries/v0002/?key=', '7E54536F93792611900A39138198FA0A', '&steamids=',convert_steam_id3(steam_id), sep="")
  steam_json <- jsonlite::fromJSON(steam_api_url)
  avatar_url <- steam_json[[1]]$players[["avatarfull"]]
  return(avatar_url)
}

get_magic_sauce <- function(player) {
  magic_sauce <- player_names[player_names$nickname == player, 'magic_sauce']
  return(magic_sauce)
}

get_playoff_games <- function(filtered_db) {
  playoffs <- filtered_db %>% dplyr::filter(final == 1)
  playoffs <- nrow(playoffs)
  return(playoffs)
}

time_performance_graph <- function(filtered_db, variable, last_game, player, updateProgress = NULL) {
  if(is.function(updateProgress)) {
    text <- "Destabilizing Orbital Payloads"
    updateProgress(detail = text)
    Sys.sleep(0.1)
  }
  steam_id <- player_names[player_names$nickname == player, 'steam_id']
  steam_id <- convert_steam_id3(steam_id)

  #TO DO: ADD TOGGLE FOR SERIES/MAP
  #filtered_db <- filtered_db %>% group_by(ranking_game) %>% summarize(mean_variable = mean(get(variable)))
  #TO DO ADD INPUT/FILTER TO CHANGE VARIABLE
  #TO DO TOOLTIP LOGS.TF LINK
  last_game <- last_game + 1
  season_avgs <- season_averages(filtered_db)
  if(is.function(updateProgress)) {
    text <- "Preparing Captive Simulators"
    updateProgress(detail = text)
    Sys.sleep(0.1)
  }

  fig <- plot_ly(data = filtered_db, type='scatter', mode="markers", hoverinfo='y', hoverlabel=list(namelength=0))

  fig <- fig %>% add_trace(x=~ranking_game, y=~get(variable), color=~class_primary, mode="markers", marker = list(size = 8),
                           text = paste("Season ", filtered_db$season, " Week ", filtered_db$week, sep=""),
                           hovertemplate = paste(
                             "<b>%{text}</b><br>",
                             sprintf("%s<br>", filtered_db$map),
                             sprintf("<br>Game Score: %s<br>", round(filtered_db$gamescore),digits=3),
                             sprintf("Impact: %s<br>", round(100*filtered_db$impact), digits=3),
                             sprintf("Survivability: %s<br>", round(100*filtered_db$survive), digits=3),
                             sprintf("Efficiency: %s<br>", round(100*filtered_db$efficiency), digits=3),
                             sprintf("Objective: %s<br><br>", round(100*filtered_db$objective), digits=3),
                             "<i>Click marker for logs.tf</i>"
                           )
  )

  fig <- fig %>% add_trace(data = season_avgs, x=~ranking_game, y=~gamescore, mode='lines+markers', name = "Season Average",
                           line = list(color="rgba(129, 33, 255, 0.5"),marker = list(color="rgba(129, 33, 255, 0.5"))

  fig <- fig %>% layout(xaxis=list(range = c(0,last_game+5), showticklabels=FALSE, title="Season"), yaxis=list(range=c(0,99), title=variable),
                        shapes = list(
                          list(type = "rect", layer="below",fillcolor = '#edec9e', line = list(color="#edec9e"), opacity=0.2,
                               x0=0, x1=last_game+5,xref="x", y0=42.5, y1=57.5, yref="y"),
                          list(type = "rect", layer="below",fillcolor = '#f4a770', line = list(color="#f4a770"), opacity=0.2,
                               x0=0, x1=last_game+5,xref="x", y0=30, y1=42.5, yref="y"),
                          list(type = "rect", layer="below",fillcolor = '#c0d292', line = list(color="#c0d292"), opacity=0.2,
                               x0=0, x1=last_game+5,xref="x", y0=57.5, y1=70, yref="y"),
                          list(type = "rect", layer="below",fillcolor = '#f14d4d', line = list(color="#f14d4d"), opacity=0.2,
                               x0=0, x1=last_game+5,xref="x", y0=0, y1=30, yref="y"),
                          list(type = "rect", layer="below",fillcolor = '#35b0ab', line = list(color="#35b0ab"), opacity=0.2,
                               x0=0, x1=last_game+5,xref="x", y0=70, y1=100, yref="y", name="Elite")
                        )
  )
  if(is.function(updateProgress)) {
    text <- "Partitioning Social Network"
    updateProgress(detail = text)
    Sys.sleep(0.1)
  }
  fig <- fig %>% add_annotations(x = seq(5,last_game+5,10), y=5, text=seq(14,29), showArrow=FALSE, xref="x", yref="y", ax=0, ay=0)

  fig <- fig %>% add_annotations(x =~filtered_db$ranking_game, y=~filtered_db$gamescore, text=paste("<a href='https://logs.tf/",filtered_db$log_id,'#', steam_id, "'>  </a>",sep=""), xref="x", yref="y", showarrow=FALSE, ax=0, ay=0)

  fig <- fig %>% add_text( x = (last_game/2), y=63.75, text = "Top 70% of Game Scores",textfont=list(color='rgba(0,0,0,0.3)'), showlegend=FALSE)
  fig <- fig %>% add_text( x = (last_game/2), y=82.5, text = "Top 90% of Game Scores", textfont=list(color='rgba(0,0,0,0.3)'), showlegend=FALSE)
  fig <- fig %>% add_text( x = (last_game/2), y=36.25, text = "Bottom 30% of Game Scores", textfont=list(color='rgba(0,0,0,0.3)'), showlegend=FALSE)
  fig <- fig %>% add_text( x = (last_game/2), y=17.5, text = "Bottom 10% of Game Scores", textfont=list(color='rgba(0,0,0,0.3)'), showlegend=FALSE)
  fig <- fig %>% config(displaylogo = FALSE)
  if(is.function(updateProgress)) {
    text <- "Blurring Reality Lines"
    updateProgress(detail = text)
    Sys.sleep(0.1)
  }
  return(fig)
}

variable_performance_graph <- function(filtered_db, variable, last_game, player, updateProgress = NULL) {
  if(is.function(updateProgress)) {
    text <- "Destabilizing Orbital Payloads"
    updateProgress(detail = text)
    Sys.sleep(0.1)
  }
  filtered_db$kd <- filtered_db$kills/filtered_db$deaths
  filtered_db$kad <- (filtered_db$kills + filtered_db$assists) / filtered_db$deaths
  steam_id <- player_names[player_names$nickname == player, 'steam_id']
  steam_id <- convert_steam_id3(steam_id)

  #TO DO: ADD TOGGLE FOR SERIES/MAP
  #filtered_db <- filtered_db %>% group_by(ranking_game) %>% summarize(mean_variable = mean(get(variable)))
  last_game <- last_game + 1
  season_avgs <- season_averages(filtered_db)
  if(is.function(updateProgress)) {
    text <- "Preparing Captive Simulators"
    updateProgress(detail = text)
    Sys.sleep(0.1)
  }

  fig <- plot_ly(data = filtered_db, type='scatter', mode="markers", hoverinfo='y', hoverlabel=list(namelength=0))

  fig <- fig %>% add_trace(x=~ranking_game, y=~get(variable), color=~class_primary, mode="markers", marker = list(size = 8),
                           text = paste("Season ", filtered_db$season, " Week ", filtered_db$week, sep=""),
                           hovertemplate = paste(
                             "<b>%{text}</b><br>",
                             sprintf("%s<br>", filtered_db$map),
                             sprintf("<br>Game Score: %s<br>", round(filtered_db$gamescore),digits=3),
                             sprintf("Impact: %s<br>", round(100*filtered_db$impact), digits=3),
                             sprintf("Survivability: %s<br>", round(100*filtered_db$survive), digits=3),
                             sprintf("Efficiency: %s<br>", round(100*filtered_db$efficiency), digits=3),
                             sprintf("Objective: %s<br><br>", round(100*filtered_db$objective), digits=3),
                             "<i>Click marker for logs.tf</i>"
                           )
  )

  fig <- fig %>% layout(xaxis=list(range = c(0,last_game+5), showticklabels=FALSE, title="Season"), yaxis=list(title=variable))
  if(is.function(updateProgress)) {
    text <- "Partitioning Social Network"
    updateProgress(detail = text)
    Sys.sleep(0.1)
  }
  if(is.numeric(filtered_db[[variable]])) {
    min_y = min(filtered_db[[variable]])
    if (min_y < 1) {
      annot_y <- 0.1
    } else if(min_y < 10) {
      annot_y <- 1
    } else {
      annot_y <- 5
    }
  } else {
    annot_y <- 0
  }
  fig <- fig %>% add_annotations(x = seq(5,last_game+5,10), y=annot_y, text=seq(14,29), showArrow=FALSE, xref="x", yref="y", ax=0, ay=0)

  fig <- fig %>% add_annotations(x =~filtered_db$ranking_game, y=~filtered_db[[variable]], text=paste("<a href='https://logs.tf/",filtered_db$log_id,'#', steam_id, "'>  </a>",sep=""), xref="x", yref="y", showarrow=FALSE, ax=0, ay=0)

  fig <- fig %>% config(displaylogo = FALSE)
  if(is.function(updateProgress)) {
    text <- "Blurring Reality Lines"
    updateProgress(detail = text)
    Sys.sleep(0.1)
  }
  return(fig)
}

class_table <- function(player_db) {
  class_tbl <- player_db %>% group_by(class_primary) %>% dplyr::summarize(
    maps = n(),
    gamescore = mean(gamescore),
    impact = mean(impact),
    survive = mean(survive),
    efficiency = mean(efficiency),
    objective = mean(objective),
    kills = mean(kills),
    assists = mean(assists),
    deaths = mean(deaths),
    dpm = mean(dpm),
    hrpm = mean(hpm),
    hpm = mean(healing)
  )

  tbl <- reactable(class_tbl, pagination=FALSE, bordered = TRUE, striped = TRUE, highlight = TRUE, columns = list(
    class_primary = colDef(name = "class"),
    gamescore = colDef(format = colFormat(digits=1)),
    impact = colDef(cell = function(value) {round(100*value,1)}),
    survive = colDef(cell = function(value) {round(100*value,1)}),
    efficiency = colDef(cell = function(value) {round(100*value,1)}),
    objective = colDef(cell = function(value) {round(100*value,1)}),
    kills = colDef(format = colFormat(digits=1)),
    assists = colDef(format = colFormat(digits=1)),
    deaths = colDef(format = colFormat(digits=1)),
    dpm = colDef(format = colFormat(digits=1)),
    hrpm = colDef(format = colFormat(digits=1)),
    hpm = colDef(format = colFormat(digits=1))
  ), defaultSorted = list(gamescore = "desc"))
}

map_table <- function(player_db) {
  class_tbl <- player_db %>% group_by(map) %>% dplyr::summarize(
    maps = n(),
    gamescore = mean(gamescore),
    impact = mean(impact),
    survive = mean(survive),
    efficiency = mean(efficiency),
    objective = mean(objective),
    kills = mean(kills),
    assists = mean(assists),
    deaths = mean(deaths),
    dpm = mean(dpm),
    hrpm = mean(hpm),
    hpm = mean(healing)
  )

  tbl <- reactable(class_tbl, pagination=FALSE, bordered = TRUE, striped = TRUE, highlight = TRUE, columns = list(
    maps = colDef(name = "played"),
    gamescore = colDef(format = colFormat(digits=1)),
    impact = colDef(cell = function(value) {round(100*value,1)}),
    survive = colDef(cell = function(value) {round(100*value,1)}),
    efficiency = colDef(cell = function(value) {round(100*value,1)}),
    objective = colDef(cell = function(value) {round(100*value,1)}),
    kills = colDef(format = colFormat(digits=1)),
    assists = colDef(format = colFormat(digits=1)),
    deaths = colDef(format = colFormat(digits=1)),
    dpm = colDef(format = colFormat(digits=1)),
    hrpm = colDef(format = colFormat(digits=1)),
    hpm = colDef(format = colFormat(digits=1))
  ), defaultSorted = list(gamescore = "desc"))
}

season_table <- function(player_db) {
  class_tbl <- player_db %>% group_by(season) %>% dplyr::summarize(
    maps = n(),
    gamescore = mean(gamescore),
    impact = mean(impact),
    survive = mean(survive),
    efficiency = mean(efficiency),
    objective = mean(objective),
    kills = mean(kills),
    assists = mean(assists),
    deaths = mean(deaths),
    dpm = mean(dpm),
    hrpm = mean(hpm),
    hpm = mean(healing)
  )
  tbl <- reactable(class_tbl,pagination=FALSE, bordered = TRUE, striped = TRUE, highlight = TRUE, columns = list(
    gamescore = colDef(format = colFormat(digits=1)),
    impact = colDef(cell = function(value) {round(100*value,1)}),
    survive = colDef(cell = function(value) {round(100*value,1)}),
    efficiency = colDef(cell = function(value) {round(100*value,1)}),
    objective = colDef(cell = function(value) {round(100*value,1)}),
    kills = colDef(format = colFormat(digits=1)),
    assists = colDef(format = colFormat(digits=1)),
    deaths = colDef(format = colFormat(digits=1)),
    dpm = colDef(format = colFormat(digits=1)),
    hrpm = colDef(format = colFormat(digits=1)),
    hpm = colDef(format = colFormat(digits=1))
  ), defaultSorted = list(gamescore = "desc"))
}

plot_stat_choices <- c('kills', 'kd', 'kad', 'assists', 'deaths', 'dpm', 'dtm', 'deathspm', 'kpm', 'cpm', 'gamescore', 'impact', 'survive', 'efficiency', 'objective', 'ubers', 'drops', 'mins_total', 'heals received per minute' = 'hpm', 'healing done per minute' = 'healing', 'division' = 'div','class_primary')
plot_stat_choices <- sort(plot_stat_choices)

##UI
ui <- function(req) {
  fluidPage(
    tags$title("Player Profiles | ozfstats"),
    tags$link(href = "https://fonts.googleapis.com/css?family=Karla:400,700|Fira+Mono&display=fallback", rel = "stylesheet"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles-new.css?rnd132"),
    tags$script('
        if(self != top) {
        var style = document.createElement("style");
        style.innerHTML = `
          #url-back-button {
            display:none !important;
          }
        `;
        document.head.appendChild(style)
        }
    '),
    br(),

    ##TOP BAR
    fluidRow(
      column(3,
             uiOutput('header'),
             class="mainTitle"
      ),
      column(2,
             uiOutput('nameSelect')
      ),
      column(2,
             uiOutput('classSelectFilter')
      ),
      column(2,
             uiOutput('divisionSelectFilter')
             )
    ),
    hr(),

    fluidRow(
      column(1,
             htmlOutput('classImage')
             ),
      column(1,
             htmlOutput('playerName'),
             htmlOutput('warzoneLink')
             ),
      column(8,
             fluidRow(
               uiOutput('statRow')
               )
             )
    ),
    br(),

    ##MAIN PANEL
    fluidRow(
      column(12,
           fluidRow(
             tabsetPanel(
               tabPanel("Game History",
                        br(),
                        plotlyOutput('timeseries')
               ),
               tabPanel("Class Stats",
                        br(),
                        column(12,reactableOutput('class_table'))
               ),
               tabPanel("Map Stats",
                        br(),
                        column(12,reactableOutput('map_table'))
               ),
               tabPanel("Season Stats",
                        br(),
                        column(12,reactableOutput('season_table'))
               ),
               tabPanel("Plot Stats",
                        br(),
                        selectInput('yVar', 'Y-Variable', choices = plot_stat_choices, selected='kills'),
                        plotlyOutput('variable_graph')
               )
             )
           ),
           class="mainPanel"
      )
    ),
    fluidRow(column(6,
                    br(),
                    a(href="https://ozfstats.com/profiles", "Back to ozfstats.com")
                    ), class="back-button", id="url-back-button"
             )
    )
}

##server
server <- function(input, output, session) {
  db <- load_database()
  names <- get_names(db)
  last_game <- max(db$ranking_game)

  #bookmark on input change
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  #update url on input change
  onBookmarked(function(url) {
    updateQueryString(url)
  })

  ##Remove plotly inputs from bookmarking URL
  plotly_verbose <- c('plotly_hover', 'plotly_click', 'plotly_selected', 'plotly_relayout')
  setBookmarkExclude(c(
    '.clientValue-default-plotlyCrosstalkOpts', 'yVar',
    sprintf('.clientValue-%s-pop_hist', plotly_verbose),
    'plotly_afterplot-A', 'plotly_relayout-A', 'plotly_hover-A', 'shareUrl', 'plotly_clickannotation-A'
  ))

  ##Render UI Filters
  output$nameSelect <- renderUI({
    selectInput('player', 'Player',choices=names, multiple = F, selected=NULL)
  })

  output$classSelectFilter <- renderUI({
    selectInput('classFilter', 'Class', choices = c(
      'All',
      'scout',
      'soldier',
      'demoman',
      'medic'
    ), selected='All', multiple = FALSE)
  })

  output$divisionSelectFilter <- renderUI({
    selectInput('divisionFilter', 'Division', choices = c(
      'All',
      'Premier' = 'prem',
      'High' = 'high',
      'Intermediate' = 'inter'
    ))
  })

  ##OBSERVE PLAYER CHANGES
  observeEvent(input$player,{
    player_db <- player_filter(db, input$player, input$classFilter, input$divisionFilter)

    steam_id <- player_names[player_names$nickname == input$player, 'steam_id']
    warzone_url <- sprintf('http://warzone.ozfortress.com/users/steam_id/%s', convert_steam_id3(steam_id))
    steam_url <- sprintf('https://steamcommunity.com/profiles/%s', convert_steam_id3(steam_id))

    output$warzoneLink <- renderText({
      paste("<br><a href=",warzone_url," target='_blank'> warzone </a> <br> <a href=",steam_url," target='_blank'> steam </a>",sep="")
    })

    ##Player Info
    output$classImage <- renderText({
      avatar_url <- get_steam_avatar(input$player)
      c('<img src=',avatar_url, 'class=class-image','>')
    })

    output$header <- renderUI({
      h2(paste("Player Profiles | ",input$player, sep=""))
    })

    output$playerName <- renderText({
      if(input$classFilter != "All") {
        most_played <- input$classFilter
      } else {
        most_played <- get_most_played_class(player_db)
      }
      paste("<b>",input$player,"</b><br>", most_played ,sep="")
    })

    win_perc_class <- paste(win_perc_colour(win_percentage(player_db))," statistic statistic-even", sep="")
    win_perc <- round(100*win_percentage(player_db),0)
    win_perc_text <- paste(win_perc, "%", sep="")

    career_avg <- round(get_career_ave(player_db)$gamescore,1)
    if(is.nan(career_avg)) {
      career_avg <- "-"
    }

    career_avg_class <- paste(career_avg_colour(career_avg), " statistic statistic-even", sep="")

    current_rank_colour <- paste(rank_colour(get_current_ranking(input$player))," statistic statistic-even", sep="")
    peak_rank_colour <- paste(rank_colour(get_peak_ranking(input$player))," statistic statistic-even", sep="")

    magic_sauce_val <- paste(get_magic_sauce(input$player), "%", sep="")
    magic_sauce_colour <- paste(career_avg_colour(get_magic_sauce(input$player))," statistic statistic-even", sep="")

    output$statRow <- renderUI({
      semanticPage(
        div(class = "ui statistics  statistic-bottom",
            div(class = "blue statistic statistic-even",
                div(class = "label", "Matches"),
                div(class = "value", count_maps(player_db))
            ),
            div(class = win_perc_class,
                div(class = "label", "Win Rate"),
                div(class = "value", win_perc_text)
            ),
            div(class = career_avg_class,
                div(class = "label", "Career Avg"),
                div(class = "value", career_avg)
            ),
            div(class = current_rank_colour,
                div(class = "label", "Current Rank"),
                div(class = "value", get_current_ranking(input$player))
            ),
            div(class = peak_rank_colour,
                div(class = "label", "Peak Rank"),
                div(class = "value", get_peak_ranking(input$player))
            ),
            div(class = magic_sauce_colour,
                div(class = "label", "X-Factor"),
                div(class = "value", magic_sauce_val)
            )

        )
      )
    })

    output$timeseries <- renderPlotly({
      progress <- shiny::Progress$new()
      progress$set(message = "... ", value = 0)
      on.exit(progress$close())


      updateProgress <- function(value = NULL, detail = NULL) {
        if(is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value)/5
        }
        progress$set(value = value, detail = detail)
      }

      text <- "Reticulating Splines"
      updateProgress(detail = text)
      Sys.sleep(0.3)

      time_performance_graph(player_db, "gamescore", last_game, input$player, updateProgress)})

    output$variable_graph <- renderPlotly({
      progress <- shiny::Progress$new()
      progress$set(message = "... ", value = 0)
      on.exit(progress$close())


      updateProgress <- function(value = NULL, detail = NULL) {
        if(is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value)/5
        }
        progress$set(value = value, detail = detail)
      }

      text <- "Reticulating Splines"
      updateProgress(detail = text)
      Sys.sleep(0.3)

      variable_performance_graph(player_db, input$yVar, last_game, input$player, updateProgress)})


    output$class_table <- renderReactable({
      class_table(player_db)
    })

    output$map_table <- renderReactable({
      map_table(player_db)
    })

    output$season_table <- renderReactable({
      season_table(player_db)
    })

  })

  ##OBSERVE CLASS CHANGES
  observeEvent(input$classFilter, {

    player_db <- player_filter(db, input$player, input$classFilter, input$divisionFilter)

    win_perc_class <- paste(win_perc_colour(win_percentage(player_db))," statistic statistic-even", sep="")
    win_perc <- round(100*win_percentage(player_db),0)
    win_perc_text <- paste(win_perc, "%", sep="")

    career_avg <- round(get_career_ave(player_db)$gamescore,1)
    if(is.nan(career_avg)) {
      career_avg <- "-"
    }
    career_avg_class <- paste(career_avg_colour(career_avg), " statistic statistic-even", sep="")

    output$playerName <- renderText({
      if(input$classFilter != "All") {
        most_played <- input$classFilter
      } else {
        most_played <- get_most_played_class(player_db)
      }
      paste("<b>",input$player,"</b><br>", most_played ,sep="")
    })

    current_rank_colour <- paste(rank_colour(get_current_ranking(input$player))," statistic statistic-even", sep="")
    peak_rank_colour <- paste(rank_colour(get_peak_ranking(input$player))," statistic statistic-even", sep="")

    magic_sauce_val <- paste(get_magic_sauce(input$player), "%", sep="")
    magic_sauce_colour <- paste(career_avg_colour(get_magic_sauce(input$player))," statistic statistic-even", sep="")

    output$statRow <- renderUI({
      semanticPage(
        div(class = "ui statistics  statistic-bottom",
            div(class = "blue statistic statistic-even",
                div(class = "label", "Matches"),
                div(class = "value", count_maps(player_db))
            ),
            div(class = win_perc_class,
                div(class = "label", "Win Rate"),
                div(class = "value", win_perc_text)
            ),
            div(class = career_avg_class,
                div(class = "label", "Career Avg"),
                div(class = "value", career_avg)
            ),
            div(class = current_rank_colour,
                div(class = "label", "Current Rank"),
                div(class = "value", get_current_ranking(input$player))
            ),
            div(class = peak_rank_colour,
                div(class = "label", "Peak Rank"),
                div(class = "value", get_peak_ranking(input$player))
            ),
            div(class = magic_sauce_colour,
                div(class = "label", "X-Factor"),
                div(class = "value", magic_sauce_val)
            )

        )
      )
    })

    output$timeseries <- renderPlotly({
      progress <- shiny::Progress$new()
      progress$set(message = "... ", value = 0)
      on.exit(progress$close())

      updateProgress <- function(value = NULL, detail = NULL) {
        if(is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value)/5
        }
        progress$set(value = value, detail = detail)
      }

      text <- "Reticulating Splines"
      updateProgress(detail = text)
      Sys.sleep(0.3)

      time_performance_graph(player_db, "gamescore", last_game, input$player, updateProgress)})

    output$variable_graph <- renderPlotly({
      progress <- shiny::Progress$new()
      progress$set(message = "... ", value = 0)
      on.exit(progress$close())


      updateProgress <- function(value = NULL, detail = NULL) {
        if(is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value)/5
        }
        progress$set(value = value, detail = detail)
      }

      text <- "Reticulating Splines"
      updateProgress(detail = text)
      Sys.sleep(0.3)

      variable_performance_graph(player_db, input$yVar, last_game, input$player, updateProgress)})

    output$class_table <- renderReactable({
      class_table(player_db)
    })

    output$map_table <- renderReactable({
      map_table(player_db)
    })

    output$season_table <- renderReactable({
      season_table(player_db)
    })


  })

  observeEvent(input$divisionFilter, {

    player_db <- player_filter(db, input$player, input$classFilter, input$divisionFilter)

    win_perc_class <- paste(win_perc_colour(win_percentage(player_db))," statistic statistic-even", sep="")
    win_perc <- round(100*win_percentage(player_db),0)
    win_perc_text <- paste(win_perc, "%", sep="")


    career_avg <- round(get_career_ave(player_db)$gamescore,1)
    if(is.nan(career_avg)) {
      career_avg <- "-"
    }

    career_avg_class <- paste(career_avg_colour(career_avg), " statistic statistic-even", sep="")

    output$playerName <- renderText({
      if(input$classFilter != "All") {
        most_played <- input$classFilter
      } else {
        most_played <- get_most_played_class(player_db)
      }
      paste("<b>",input$player,"</b><br>", most_played ,sep="")
    })

    current_rank_colour <- paste(rank_colour(get_current_ranking(input$player))," statistic statistic-even", sep="")
    peak_rank_colour <- paste(rank_colour(get_peak_ranking(input$player))," statistic statistic-even", sep="")

    magic_sauce_val <- paste(get_magic_sauce(input$player), "%", sep="")
    magic_sauce_colour <- paste(career_avg_colour(get_magic_sauce(input$player))," statistic statistic-even", sep="")

    output$statRow <- renderUI({
      semanticPage(
        div(class = "ui statistics  statistic-bottom",
            div(class = "blue statistic statistic-even",
                div(class = "label", "Matches"),
                div(class = "value", count_maps(player_db))
            ),
            div(class = win_perc_class,
                div(class = "label", "Win Rate"),
                div(class = "value", win_perc_text)
            ),
            div(class = career_avg_class,
                div(class = "label", "Career Avg"),
                div(class = "value", career_avg)
            ),
            div(class = current_rank_colour,
                div(class = "label", "Current Rank"),
                div(class = "value", get_current_ranking(input$player))
            ),
            div(class = peak_rank_colour,
                div(class = "label", "Peak Rank"),
                div(class = "value", get_peak_ranking(input$player))
            ),
            div(class = magic_sauce_colour,
                div(class = "label", "X-Factor"),
                div(class = "value", magic_sauce_val)
            )

        )
      )
    })

    output$timeseries <- renderPlotly({
      progress <- shiny::Progress$new()
      progress$set(message = "... ", value = 0)
      on.exit(progress$close())

      updateProgress <- function(value = NULL, detail = NULL) {
        if(is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value)/5
        }
        progress$set(value = value, detail = detail)
      }

      text <- "Reticulating Splines"
      updateProgress(detail = text)
      Sys.sleep(0.3)

      time_performance_graph(player_db, "gamescore", last_game, input$player, updateProgress)})

    output$variable_graph <- renderPlotly({
      progress <- shiny::Progress$new()
      progress$set(message = "... ", value = 0)
      on.exit(progress$close())


      updateProgress <- function(value = NULL, detail = NULL) {
        if(is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value)/5
        }
        progress$set(value = value, detail = detail)
      }

      text <- "Reticulating Splines"
      updateProgress(detail = text)
      Sys.sleep(0.3)

      variable_performance_graph(player_db, input$yVar, last_game, input$player, updateProgress)

      })

    output$class_table <- renderReactable({
      class_table(player_db)
    })

    output$map_table <- renderReactable({
      map_table(player_db)
    })

    output$season_table <- renderReactable({
      season_table(player_db)
    })
  })

  observeEvent(input$variableSelect, {

    player_db <- player_filter(db, input$player, input$classFilter, input$divisionFilter)

    output$variable_graph <- renderPlotly({
      progress <- shiny::Progress$new()
      progress$set(message = "... ", value = 0)
      on.exit(progress$close())

      updateProgress <- function(value = NULL, detail = NULL) {
        if(is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value)/5
        }
        progress$set(value = value, detail = detail)
      }

      text <- "Reticulating Splines"
      updateProgress(detail = text)
      Sys.sleep(0.3)

      variable_performance_graph(player_db, input$yVar, last_game, input$player, updateProgress)})
  })
}

shinyApp(ui,server, enableBookmarking="url")

######### JUNK ABOVE
#PlayerProfiles
library(shiny)
library(plotly)
library(rlist)
library(Cairo)
library(reactable)
library(dplyr)
options(shiny.usecairo=T)

##functions
player_names <- read.csv('../../data/players.csv')

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

player_filter <- function(db, player, class = NULL) {
  filtered <- db %>% dplyr::filter(nickname == player)
  if(!is.null(class)) {
    if(class == 'All') {
      class <- c('scout', 'soldier', 'demoman', 'medic')
    }
    filtered <- filtered %>% filter(class_primary %in% class)
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
  wins <- filtered_db %>% group_by(win) %>% dplyr::summarize(count = n())
  if (is.na(wins[2,][['count']])) {
    wins[2,][['count']] <- 0
  }
  if (is.na(wins[1,][['count']])) {
    wins[1,][['count']] <- 0
  }
  win_perc <- (wins[2,][['count']] / (wins[2,][['count']] + wins[1,][['count']] ))
  return(win_perc)
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

time_performance_graph <- function(filtered_db, variable, last_game) {
  #TO DO: ADD TOGGLE FOR SERIES/MAP
  #filtered_db <- filtered_db %>% group_by(ranking_game) %>% summarize(mean_variable = mean(get(variable)))
  #TO DO ADD INPUT/FILTER TO CHANGE VARIABLE
  #TO DO TOOLTIP LOGS.TF LINK
  last_game <- last_game + 1
  season_avgs <- season_averages(filtered_db)
  
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

  fig <- fig %>% layout(xaxis=list(range = c(0,last_game), showticklabels=FALSE, title="Season"), yaxis=list(range=c(0,95), title=variable),
                        shapes = list(
                                      list(type = "rect", layer="below",fillcolor = '#edec9e', line = list(color="#edec9e"), opacity=0.2,
                                      x0=0, x1=last_game,xref="x", y0=42.5, y1=57.5, yref="y"),
                                      list(type = "rect", layer="below",fillcolor = '#f4a770', line = list(color="#f4a770"), opacity=0.2,
                                           x0=0, x1=last_game,xref="x", y0=30, y1=42.5, yref="y"),
                                      list(type = "rect", layer="below",fillcolor = '#c0d292', line = list(color="#c0d292"), opacity=0.2,
                                           x0=0, x1=last_game,xref="x", y0=57.5, y1=70, yref="y"),
                                      list(type = "rect", layer="below",fillcolor = '#f14d4d', line = list(color="#f14d4d"), opacity=0.2,
                                           x0=0, x1=last_game,xref="x", y0=0, y1=30, yref="y"),
                                      list(type = "rect", layer="below",fillcolor = '#35b0ab', line = list(color="#35b0ab"), opacity=0.2,
                                           x0=0, x1=last_game,xref="x", y0=70, y1=100, yref="y", name="Elite")
                                      )
                        )
  fig <- fig %>% add_annotations(x = seq(5,last_game,10), y=5, text=seq(14,27), showArrow=FALSE, xref="x", yref="y", ax=0, ay=0)
  
  fig <- fig %>% add_annotations(x =~filtered_db$ranking_game, y=~filtered_db$gamescore, text=paste("<a href='https://logs.tf/",filtered_db$log_id,"'>  </a>",sep=""), xref="x", yref="y", showarrow=FALSE, ax=0, ay=0)

  fig <- fig %>% add_text( x = (last_game/2), y=63.75, text = "Top 70% of Game Scores",textfont=list(color='rgba(0,0,0,0.3)'), showlegend=FALSE)
  fig <- fig %>% add_text( x = (last_game/2), y=82.5, text = "Top 90% of Game Scores", textfont=list(color='rgba(0,0,0,0.3)'), showlegend=FALSE)
  fig <- fig %>% add_text( x = (last_game/2), y=36.25, text = "Bottom 30% of Game Scores", textfont=list(color='rgba(0,0,0,0.3)'), showlegend=FALSE)
  fig <- fig %>% add_text( x = (last_game/2), y=17.5, text = "Bottom 10% of Game Scores", textfont=list(color='rgba(0,0,0,0.3)'), showlegend=FALSE)
  fig <- fig %>% config(displaylogo = FALSE)
  return(fig)
}

##UI
ui <- function(req) {
  fluidPage(
    tags$title('Player Profiles | ozfstats'),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?rnd132"),
    tags$head(includeHTML("google-analytics.html")),
    tags$link(href = "https://fonts.googleapis.com/css?family=Karla:400,700|Fira+Mono&display=fallback", rel = "stylesheet"),
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
    uiOutput('titleheader'),
    hr(),
    fluidRow(
      column(2,
             fluidRow(
               column(12, htmlOutput('classImage'))
               )
             ),
      column(2,
             fluidRow(
               column(12,uiOutput('nameSelect'))
               ),
             fluidRow(
               column(12,uiOutput('classSelectFilter'))
               ),
             fluidRow(
               column(12,
                      htmlOutput('warzoneLink'))
               ),
             hr(),
             fluidRow(
               column(12,
                      textOutput('totalGames')
                      ),
               ),
             fluidRow(
               column(12,
                      textOutput('winPercentage')
                      )
               ),
             fluidRow(
               column(12,
                      textOutput('seasonsPlayed')
                      )
               )
             ),
      column(8,
             fluidRow(
               column(12,reactableOutput('mapTable'))
               )
             )
      ),
    hr(),
    fluidRow(
      plotlyOutput('timeseries')
      ),
    fluidRow(column(6,
                    a(href="https://ozfstats.com/", "Back to ozfstats.com")
    ), class="back-button", id="url-back-button"
    ),
    br()
    )
}

##SERVER

server <- function(input, output, session) {
  db <- load_database()
  names <- get_names(db)
  last_game <- max(db$ranking_game)
  
  plotly_verbose <- c('plotly_hover', 'plotly_click', 'plotly_selected', 'plotly_relayout')
  setBookmarkExclude(c(
    '.clientValue-default-plotlyCrosstalkOpts',
    sprintf('.clientValue-%s-pop_hist', plotly_verbose),
    'plotly_afterplot-A', 'plotly_relayout-A', 'plotly_hover-A', 'shareUrl', 'plotly_clickannotation-A'
  ))
  
  #bookmark on input change
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  
  #update url on input change
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  ##Render UI only once database is loaded.
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



  #Change outputs when player is selected
  observeEvent(input$player ,{
    output$titleheader <- renderUI({
      text <- paste("Player Profile  |  ", input$player, sep="")
      h2(text)
    })

    player_db <- player_filter(db, input$player, input$classFilter)

    output$winPercentage <- renderText({
      winp <- win_percentage(player_db)
      paste("Win Rate: ",100*round(winp, digits=2),"%", sep="")
    })

    output$totalGames <- renderText({
      games <- count_games(player_db)
      maps <- count_maps(player_db)
      paste("Total Games Played (Total Maps): ", games, " (",maps,")", sep="")
    })

    output$seasonsPlayed <- renderText({
      seasons <- count_seasons(player_db)
      paste("Seasons Played: ", seasons, " (of ", 14, ")",sep="")
    })
    steam_id <- player_names[player_names$nickname == input$player, 'steam_id']
    warzone_url <- sprintf('http://warzone.ozfortress.com/users/steam_id/%s', convert_steam_id3(steam_id))
    steam_url <- sprintf('https://steamcommunity.com/profiles/%s', convert_steam_id3(steam_id))
    
    output$warzoneLink <- renderText({
      paste("<a href=",warzone_url," target='_blank'> ozfortress warzone profile </a> <br> <a href=",steam_url," target='_blank'> steam profile </a>",sep="")
    })

    output$classGames2 <- renderText({
      paste("Scout Games: ", count_class(player_db, "scout"),"\n",
            "Soldier Games: ", count_class(player_db, "soldier"),"\n",
            "Demoman Games: ", count_class(player_db, "demoman"),"\n",
            "Medic Games: ", count_class(player_db, "medic"),"\n",
            sep="")
    })

    output$mapTable <- renderReactable({
      per_map_stats(player_db, class=input$classFilter)
    })

    output$classImage <- renderText({
      tf2class <- get_most_played_class(player_db)
      if(is.na(tf2class)) {
        tf2class <- 'hale'
      }
      imgpath <- paste(tf2class,'-card.jpg',sep="")
      c('<img src=',imgpath, 'class=class-image','>')
    })

    output$timeseries <- renderPlotly({time_performance_graph(player_db, "gamescore", last_game)})
    
    updateSelectInput(session, "classFilter", selected = "All")
  })

  ##Change outputs when class filter is changed.
  observeEvent(input$classFilter, {
    player_db <- player_filter(db, input$player, input$classFilter)
    output$mapTable <- renderReactable({
      per_map_stats(player_db, class=input$classFilter)
    })

    output$winPercentage <- renderText({
      winp <- win_percentage(player_db)
      if (is.numeric(winp)) {
        paste("Win Rate: ",100*round(winp, digits=2),"%", sep="")
      } else {
        paste("Win Rate: ", "-", sep="")
      }
    })

    output$totalGames <- renderText({
      games <- count_games(player_db)
      maps <- count_maps(player_db)
      paste("Total Games Played (Total Maps): ", games, " (",maps,")", sep="")
    })

    output$seasonsPlayed <- renderText({
      seasons <- count_seasons(player_db)
      paste("Seasons Played: ", seasons, " (of ", 14, ")", sep="")
    })
    output$classImage <- renderText({
      tf2class <- get_most_played_class(player_db)
      if(is.na(tf2class)) {
        tf2class <- 'hale'
      }
      imgpath <- paste(tf2class,'-card.jpg',sep="")
      c('<img src=',imgpath, 'class=class-image','>')
    })

    output$timeseries <- renderPlotly({time_performance_graph(player_db, "gamescore", last_game)})
  })
}

shinyApp(ui,server, enableBookmarking = "url")

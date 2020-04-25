## app.R ##
library(shiny)
library(Cairo)
library(reactable)
library(htmltools)
library(waiter)
library(dplyr)
options(shiny.usecairo=T)

##Load Data
gamedata <- read.csv("../../data/gamedata.csv")
gamedata<-gamedata[order(-gamedata$season,-gamedata$week),]
currentseason<-head(gamedata,1)$season
currentweek<-head(gamedata,1)$week
labels<-read.csv("../../data/labels.csv")
season_ranks <- read.csv("../../data/season_rankings_combined.csv") %>% filter(n>1)
player_names <- sort(season_ranks$nickname)

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
           fluidRow(column(width=12,
                              div(class = "title",
                                  h2("All Season Rankings"),
                                  "Season scores for all players from Season 14, the class next to each player is the one they have played the most that season.",
                                  "Medic stats for before Season 22 should be taken with a grain of salt,
                                   due to the innaccurate healing done statistics as a result of ozfortress servers not counting buffs at the time
                                  (but every other server counting buffs...)."
                              )
              )
              ),
              fluidRow(
                column(width=2,selectizeInput("rankseasonfilter", label = "Season", choices = c(14:27),multiple=TRUE, width="100%")),
                column(width=2,selectizeInput("rankplayerfilter", label = "Player", choices = player_names ,multiple=TRUE, width="100%")),
                column(width=2,selectizeInput("rankclassfilter", label = "Class", choices = c("scout", "soldier", "demoman", "medic"),multiple=TRUE, width="100%")),
                column(width=2,selectizeInput("rankdivfilter", label="Division", choices = unique(season_ranks$div),multiple=TRUE, width="100%")),
                column(width=4,sliderInput("ranknfilter", "Min. Maps Played",min=2, max=14, value = 8))
              ),
              fluidRow(
                 reactableOutput("seasontable"),
                )
              )
           , "class"="standings")


server <- function(input,output,session) {
  waiter_show(
    spin_loaders(37),
    color="#212529"
  )
  # RANKING TABLE DETAILS -------------------------------------------------------
  min_p2 <- min(season_ranks[,c("impact", "survive", "efficiency", "objective")])
  max_p2 <- max(season_ranks[,c("impact", "survive", "efficiency", "objective")])

  format_pct <- function(value) {
    if (value == 0) " \u2013 "    # en dash
    else if (value == 1) "\u2713"  # checkmark
    else if (value < 0.01) "<1%"
    else if (value > 0.99) ">99%"
    else formatC(paste0(round(value * 100), "%"), width = 4)
  }

  format_change <- function(value) {
    if (is.na(value)) "NEW"
    else if (value == 0) "\u2013 "
    else if (value > 0) paste("\u2191", value, sep="")
    else if (value < 0) paste("\u2193", abs(value), sep="")
    else ""
  }

  make_color_pal <- function(colors, bias = 1) {
    get_color <- colorRamp(colors, bias = bias)
    function(x) rgb(get_color(x), maxColorValue = 255)
  }

  ranking_color <-  make_color_pal(c("#f14d4d", "#f8fcf8", "#44ab43"), bias = 0.8)
  ranking_color_green <-  make_color_pal(c("#c0d292", "#44ab43"), bias = 2)
  ranking_color_red <-  make_color_pal(c("#f27e7e","#f14d4d"), bias = 2)

  ranking_column <- function(maxWidth = 100, class = NULL,...) {
    colDef(maxWidth = maxWidth, align = "center", class = paste("number", class), ...)
  }

  score_color <- make_color_pal(c("#ff2700", "#f4a770", "#edec9e", "#c0d292", "#35b0ab"), bias = 1)
  dump <- make_color_pal(c("#f14d4d", "#f4a770", "#edec9e", "#c0d292", "#35b0ab"), bias = 2)
  score_column <- function(maxWidth=100, class = NULL, minp = min_p, maxp = max_p, ...) {
    colDef(
      cell = format_pct,
      maxWidth = maxWidth,
      class = paste("number", class),
      style = function(value) {
        if (value < 0.01) {
          list(color = "#aaa")
        } else {
          scaled <- (value - minp)/(maxp - minp)
          list(color = "#111", background = score_color(scaled))
        }
      },
      ...
    )
  }

  score_cols <- c("impact", "survive", "efficiency", "objective")

  change_column <- function(maxWidth=60, class = NULL, ...) {
    colDef(
      cell = format_change,
      maxWidth = maxWidth,
      align = "center",
      class = paste("number","border-left", class),
      style = function(value) {

        scaled <- (value - min(current_rankings[!is.na(current_rankings$rank_change),]$rank_change)) / (max(current_rankings[!is.na(current_rankings$rank_change),]$rank_change) - min(current_rankings[!is.na(current_rankings$rank_change),]$rank_change))
        if (is.na(value)) {
          list(color = "#111", background = "#FFD700")
        } else if (value == 0 ) {
          list(color = "#aaa")
        } else if (value > 0) {
          list(color = "#111", background = ranking_color(scaled))
        } else if (value < 0) {
          list(color = "#111", background = ranking_color(scaled))
        } else {list(color="#111")}
      },...
    )
  }

  # ALL SEASON RANKINGS -----------------------------------------------------
  total_seasons <- nrow(season_ranks)
  gen_season_table <- function(data, updateProgress = NULL) {

    if(is.function(updateProgress)) {
      text <- "Generating ranking table"
      updateProgress(detail = text)
    }

    season_table <- reactable(
      data,
      showPageSizeOptions=TRUE,
      pageSizeOptions= c(20, 50, 100, 500, 1000),
      defaultPageSize=100,
      highlight=TRUE,
      defaultSorted="rank",
      defaultSortOrder="asc",
      defaultColGroup = colGroup(headerClass = "header"),
      defaultColDef = colDef(headerClass = "header col-header"),
      columnGroups = list(
        colGroup(name = "Scores", columns = score_cols)
      ),
      columns = list(
        rank = ranking_column(name = "Ranking",
                           headerStyle = list(fontWeight=700),
                           cell = function(value, index) {
                             div(
                               class = "ranking",
                               div(class = "ranking-val", value)
                             )
                           }
        ),
        nickname = colDef(name = "Player Name",
                          headerStyle = list(fontWeight = 700),
                          minWidth = 200,
                          cell = function(value, index) {
                          profile_url <- sprintf('https://app.ozfstats.com/playerProfiles/?_inputs_&classFilter="All"&player="%s"',value)
                            div(
                              class = "name",
                              div(class = "player-name", htmltools::tags$a(href = profile_url, target = "_blank", value)),
                              div(class = "class", sprintf("%s", data[index,"classes"]))
                            )
                          }
        ),
        classes = colDef(show = FALSE),
        season = ranking_column (name = "Season"),
        div = ranking_column (name="Division"),
        gamescore = ranking_column(header = (span("Season Score")), format = colFormat(digits = 1),
                                   cell = function(value) {
                                     scaled <- (value - min(season_ranks$gamescore)) / (max(season_ranks$gamescore) - min(season_ranks$gamescore))
                                     color <- ranking_color(scaled)
                                     value <- format(round(value,1), nsmall=1)
                                     div(class = "spi-rating", style = list(background = color), value)
                                   }
        ),
        n = ranking_column(name = "Maps Played (Season)"),
        impact = score_column(header = (span("Impact", title = "Considers DPM, KA/D, K/D, KPM, HPM, CHARGES")), minp = min_p2, maxp = max_p2),
        survive = score_column(header = (span("Survivability", title = "Considers DT/M, DEATHS/M, DROPS/M")), minp = min_p2, maxp = max_p2),
        efficiency = score_column(header = (span("Efficiency", title = "Considers D/DT, D/HR, DROPS/UBER, K/HR")), minp = min_p2, maxp = max_p2),
        objective = score_column(header = (span("Objective", title = "Considers WIN-LOSS, GAME MARGIN, CAPS")), minp = min_p2, maxp = max_p2)
      ),
      class = "standings-table"
    )
    if(is.function(updateProgress)) {
      text <- "Rendering table..."
      updateProgress(detail = text)
    }
    waiter_hide()
    return(season_table)
  }

  output$seasontable <- renderReactable({

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

    season_data <- season_ranks %>% filter(season_ranks$n >= input$ranknfilter)
    if(!is.null(input$rankseasonfilter)) {
      season_data <- season_data %>% filter(season_data$season %in% input$rankseasonfilter)
    }
    if(!is.null(input$rankplayerfilter)) {
      season_data <- season_data %>% filter(season_data$nickname %in% input$rankplayerfilter)
    }
    if(!is.null(input$rankclassfilter)) {
      season_data <- season_data %>% filter(season_data$classes %in% input$rankclassfilter)
    }
    if(!is.null(input$rankdivfilter)) {
      season_data <- season_data %>% filter(season_data$div %in% input$rankdivfilter)
    }
    season_data$rank <- NA
    season_data$rank[order(-season_data$gamescore)] <- 1:nrow(season_data)
    columns = c("rank", "nickname", "classes", "season","div","gamescore", "impact", "survive", "efficiency", "objective", "n")
    season_data <- season_data[, columns]

    gen_season_table(season_data, updateProgress)
  })
}

shinyApp(ui, server)

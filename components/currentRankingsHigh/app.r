## app.R ##
library(shiny)
library(Cairo)
library("dqshiny")
library("reactable")
library("plotly")
library(htmltools)
library(knitr)
library(rmarkdown)
library(waiter)
options(shiny.usecairo=T)

##Load Data
gamedata <- read.csv("../../data/gamedata.csv")
gamedata<-gamedata[order(-gamedata$season,-gamedata$week),]
currentseason<-head(gamedata,1)$season
currentweek<-head(gamedata,1)$week

# NEW RANKING TABLE -------------------------------------------------------
current_rankings <- read.csv("../../data/current_rankings_high.csv")

columns = c("X", "nickname", "rank_change", "peak_rank", "ranking_points", "career_avg", "n", "classes", "impact", "survive", "efficiency", "objective", "prev_rank")
min_p <- min(current_rankings[,c("impact", "survive", "efficiency", "objective")])
max_p <- max(current_rankings[,c("impact", "survive", "efficiency", "objective")])

current_rankings <- current_rankings[, columns]

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

min_score <- min(current_rankings$ranking_points)
max_score <- max(current_rankings$ranking_points)

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

gen_ranking_table <- function(updateProgress = NULL){
  if(is.function(updateProgress)) {
    text <- "Generating ranking table"
    updateProgress(detail = text)
  }
  tbl <- reactable(
    current_rankings,
    pagination=FALSE,
    highlight=TRUE,
    defaultSorted="X",
    defaultSortOrder = "asc",
    defaultColGroup = colGroup(headerClass = "header"),
    defaultColDef = colDef(headerClass = "header col-header"),
    columnGroups = list(
      colGroup(header = (span("Scores", title = "Refer to 'About' for details on the scores below")), columns = score_cols)
    ),
    columns = list(
      X = ranking_column(header = (span(style = list(fontWeight=700), "Ranking")),
                         cell = function(value, index) {
                           peak <- current_rankings[index, "peak_rank"]
                           pr <- current_rankings[index, "prev_rank"]
                           cr <- current_rankings[index, "rank_change"]
                           scaled <- (cr - min(current_rankings[!is.na(current_rankings$rank_change),]$rank_change)) / (max(current_rankings[!is.na(current_rankings$rank_change),]$rank_change) - min(current_rankings[!is.na(current_rankings$rank_change),]$rank_change))
                           div(
                             class = "ranking",
                             div(class = "ranking-val", value, style = list(color="#111")
                                 ),
                             div(class = "change-sub", sprintf("%s", format_change(current_rankings[index, "rank_change"])),
                                 style =
                                 if (is.na(cr)) {
                                   list(color="#FFD700")
                                 } else if (cr == 0 ) {
                                   list(color = "#d9d9d9")
                                 } else if (cr > 0) {
                                   scaled <- (cr) / (max(current_rankings[!is.na(current_rankings$rank_change),]$rank_change))
                                   list(color = ranking_color_green(scaled))
                                 } else if (cr < 0) {
                                   scaled <- (cr) / (min(current_rankings[!is.na(current_rankings$rank_change),]$rank_change))
                                   list(color = ranking_color_red(scaled))
                                 } else {list(color="#111")}
                                 )
                           )
                         }
      ),
      nickname = colDef(name = "Player Name",
                        headerStyle=list(fontWeight=700),
                        minWidth = 200,
                        cell = function(value, index) {
                          profile_url <- sprintf('https://app.ozfstats.com/playerProfiles/?_inputs_&classFilter="All"&player="%s"',value)
                          div(
                            class = "name",
                            div(class = "player-name",
                                htmltools::tags$a(href = profile_url, target = "_blank", value)),
                            div(class = "class", sprintf("%s", current_rankings[index, "classes"]))
                          )
                        }
      ),
      classes = colDef(show=FALSE),
      ranking_points = ranking_column(header = (span("Ranking Points", title = "Used to compare players, refer to 'About' for more info.")),
                                      format = colFormat(digits=1),
                                      cell = function(value) {
                                        scaled <- (value - min_score) / (max_score - min_score)
                                        color <- ranking_color(scaled)
                                        value <- format(round(value, 1), nsmall = 1)
                                        div(class = "spi-rating", style = list(background = color), value)
                                      }
      ),
      rank_change = change_column(show=FALSE,header = (span("Change", title = "Change in rank from the previous week")), class = "rank-change"),
      n = ranking_column(show=FALSE,header = (span("Maps Played", title = "Number of maps played in 2 season rolling window.")), class = "border-left"),
      peak_rank = ranking_column(header = (span("Peak Rank", title = "Peak rank since Season 14")), maxWidth=60,
                                 cell = function(value, index) {
                                   cr <- current_rankings[index, "X"]
                                   pr <- current_rankings[index, "prev_rank"]
                                   if (is.na(pr)){
                                     div(class = "peak_rank", value)
                                   } else if(value == cr & value != pr) {
                                     div(class = "peak_rank", style = list(color = "#daa520"), value)
                                   } else {
                                    div(class = "peak_rank", value)
                                   }
                                 }
                                 ),
      prev_rank = colDef(show=FALSE),
      career_avg = ranking_column(header = (span("Avg Pts. (No Weight/Decay)", title = "Averge ranking points per game BEFORE old game decay and participation weighting")), format = colFormat(digits=1),
                                  cell = function(value) {
                                    scaled <- (value - min(current_rankings$career_avg)) / (max(current_rankings$career_avg) - min(current_rankings$career_avg))
                                    color <- ranking_color(scaled)
                                    value <- format(round(value, 1), nsmall = 1)
                                    div(class = "spi-rating", style = list(background = color), value)
                                  }
      ),
      impact = score_column(header = (span("Impact", title = "Considers DPM, KA/D, K/D, KPM, HPM, CHARGES")), class = "border-left"),
      survive = score_column(header = (span("Survivability", title = "Considers DT/M, DEATHS/M, DROPS/M"))),
      efficiency = score_column(header = (span("Efficiency", title = "Considers D/DT, D/HR, DROPS/UBER, K/HR"))),
      objective = score_column(header = (span("Objective", title = "Considers WIN-LOSS, GAME MARGIN, CAPS")))
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
         fluidRow(column(width=12,
                         div(class = "title", h2("Current High Player Rankings"))
                         )
                  ),
         fluidRow(column(12,
                         reactableOutput("rankingtable"))
                  ),
         fluidRow(column(12,
                         fluidRow(column(10,
                                         fluidRow(textOutput("edit_date")),
                                         fluidRow(
                                           div(class = "title",
                                               p("Current weighted player rankings using a 2 season rolling window. Click player name for link to player statistics profile."),
                                               p("Note: 'Peak' rankings for High Division may not reflect the previous weeks' rankings, this is due to the back-calculated nature of the peak rankings and the small dataset for High divison, this will become more stable over the season.
                                                 An example of this is that when more information comes in about how sunshine is played over week 2, the results of week 1 may be slightly adjusted, which retrospectively means that a player at rank 5 in week 1 may only historically be 
                                                 rank 6 for that same week upon reflection of the actual division-wide performance on sunshine.
                                                 "),
                                               p("See the about section for details on how the rankings are calculated.")
                                               )
                                           )
                                         ),
                                  column(width=2, div(
                                    div(" >70%    Elite",class="r1"),
                                    div(" >60%    Above Average",class="r2"),
                                    div(" 60%-40%  Average",class="r3"),
                                    div(" <40%    Below Average",class="r4"),
                                    div(" <30%    Poor",class="r5")
                                    ), "align"="right")
                                  )
                         )
                  ), "class"="standings")
  )

server <- function(input,output,session) {

  output$edit_date <- renderText({
    paste("Rankings as at end of Season ",currentseason, " week ", currentweek, ". Last update was at: ", toString(file.info("../../data/current_rankings_high.csv")$mtime), sep="")
  })

  output$rankingtable <- renderReactable({
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

    gen_ranking_table(updateProgress)
    })
}

shinyApp(ui, server)

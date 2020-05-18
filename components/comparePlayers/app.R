#comparePlayers app.R
library(shiny)
library(plotly)
library(rlist)
library(Cairo)
library(reactable)
options(shiny.usecairo=T)
library(shinythemes)

##Read sum stats for comparison.
summed_dr <- read.csv('../../data/summarised_stats.csv')
names <- unique(summed_dr$nickname)
classes <- unique(summed_dr$class_primary)

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

  df[,nums] <- round(df[,nums], digits = digits)

  (df)
}

ui <- fluidPage(
  shinythemes::themeSelector(),
  tags$link(href = "https://fonts.googleapis.com/css?family=Karla:400,700|Fira+Mono&display=fallback",
            rel = "stylesheet"),
  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?rnd132"),
  titlePanel("Player Comparison"),
  fluidRow(column(5,
                  plotlyOutput('comparePlot')
                  ),
           column(7,
                  plotlyOutput('compare_bars')
                  )), hr(),
  fluidRow(column(12,
                   reactableOutput("tableData")
                   )),
  hr(),
  fluidRow(
    column(2,
           selectizeInput('player_a', 'Player 1',choices=names,multiple=F, selected='AiShou'),
           selectizeInput('class_a',label='Player 1 Class', choices=classes,multiple=F, selected='scout'),
           textOutput("player_a_games_played")
           ),

    column(2,
           selectizeInput('player_b', 'Player 2',choices=names,multiple=F, selected='krollic'),
           selectizeInput('class_b',label='Player 2 Class', choices=classes,multiple=F, selected='scout'),
           textOutput("player_b_games_played")
           ),
    column(6,offset=1,
           p('Uses the same 2 season rolling window that is used in the current rankings. Data in the bars are means of all officials.
             Radial plot uses percentiles.
             ')
           )
    )
)

server <- function(input, output, session) {
  polar_comparison <- function(player_a, class_a, player_b, class_b) {
    stats <- c('perc_dpm', 'perc_kad', 'perc_kd', 'perc_cpm', 'perc_kpm', 'perc_damageperheal','perc_damageperdt')

    df <- summed_dr %>% dplyr::filter(nickname == player_a & class_primary == class_a)
    vals <- df[, stats]
    vals <- as.list(t(vals))
    vals <- list.append(vals, vals[1])

    df_b <- summed_dr %>% dplyr::filter(nickname == player_b & class_primary == class_b)
    vals_b <- df_b[, stats]
    vals_b <- as.list(t(vals_b))
    vals_b <- list.append(vals_b, vals_b[1])

    stats_names <- c('DPM', 'KA/D', 'K/D', 'CAPS/MIN', 'KILLS/MIN', 'DAMAGE/HEAL','DAMAGE/DT')


    fig <- plot_ly(type='scatterpolar', mode = 'markers',
                   fill='toself')
    fig <- fig %>% add_trace(
      r = vals,
      theta = stats_names,
      name = paste(player_a, class_a)
    )
    fig <- fig %>% add_trace(
      r = vals_b,
      theta = stats_names,
      name = paste(player_b,class_b)
    )

    fig <- fig %>% layout(
      polar= list(
        radialaxis = list(
          visible=F,
          range=(c(0,1))
        )
      )
    )
    return(fig)
  }

  output$comparePlot <- renderPlotly({
    progress <- shiny::Progress$new()
    progress$set(message = "Generating radial plot...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    polar_comparison(input$player_a, input$class_a, input$player_b, input$class_b) %>% config(displaylogo = FALSE)
  })

  output$player_a_games_played <- renderText({
    tmp <- summed_dr %>% dplyr::filter(nickname==input$player_a & class_primary == input$class_a)
    games_played <- tmp$total_games
    if(nrow(tmp)==0) {
      games_played <- 0
    }
    paste("Total of",games_played,"games played in the comparison window")
  })

  output$player_b_games_played <- renderText({
    tmp <- summed_dr %>% dplyr::filter(nickname==input$player_b & class_primary == input$class_b)
    games_played <- tmp$total_games
    if(nrow(tmp)==0) {
      games_played <- 0
    }
    paste("Total of",games_played,"games played in the comparison window")
  })

  #####
  #### BAR PLOT FOR A PLAYER
  damage_bar_comparison <- function(player_a, class_a, player_b, class_b) {
    stats <- c('dpm', 'dtm', 'hpm')
    df_a <- summed_dr %>% dplyr::filter(nickname == player_a & class_primary == class_a)
    df_a <- df_a[,stats]
    df_a <- as.list(t(df_a))

    df_b <- summed_dr %>% dplyr::filter(nickname == player_b & class_primary == class_b)
    df_b <- df_b[,stats]
    df_b <- as.list(t(df_b))

    if(length(df_a)!=0 & length(df_b)!=0) {
      df1 <- data.frame(stats,df_a, df_b) %>% distinct()
      fig <- plot_ly(df1, x=~stats, y=~df_a, type='bar', name=player_a, marker = list(color = 'rgb(255, 127, 14)'))
      fig <- fig %>% add_trace(y=~df_b, name=player_b, marker = list(color = 'rgb(44, 160, 44)'))
      fig <- fig %>% layout(barmode='group', showlegend=F)
    } else if (length(df_a)==0 & length(df_b) != 0) {
      df1 <- data.frame(stats, df_b) %>% distinct()
      fig <- plot_ly(df1, x=~stats, y=0, type='bar', name=player_b, marker = list(color = 'rgb(255, 127, 14)'))
      fig <- fig %>% add_trace(y=~df_b, name=player_b, marker = list(color = 'rgb(44, 160, 44)'))
      fig <- fig %>% layout(barmode='group', showlegend=F)
    } else if (length(df_a)!=0 & length(df_b) == 0) {
      df1 <- data.frame(stats, df_a) %>% distinct()
      fig <- plot_ly(df1, x=~stats, y=~df_a, type='bar', name=player_b, marker = list(color = 'rgb(255, 127, 14)'))
      fig <- fig %>% add_trace(y=0, name=player_b, marker = list(color = 'rgb(44, 160, 44)'))
      fig <- fig %>% layout(barmode='group', showlegend=F)
    } else {
      df1 <- as.data.frame(stats,0,0)
      fig <- plot_ly(df1, type='bar')
      fig <- fig %>% layout(barmode='group', showlegend=F)
    }

    return(fig)
  }

  #### BAR PLOT FOR A PLAYER
  kills_bar_comparison <- function(player_a, class_a, player_b, class_b) {
    stats <- c('kills', 'assists', 'deaths')
    df_a <- summed_dr %>% dplyr::filter(nickname == player_a & class_primary == class_a)
    df_a <- df_a[,stats]
    df_a <- as.list(t(df_a))

    df_b <- summed_dr %>% dplyr::filter(nickname == player_b & class_primary == class_b)
    df_b <- df_b[,stats]
    df_b <- as.list(t(df_b))

    if(length(df_a)!=0 & length(df_b)!=0) {
      df1 <- data.frame(stats,df_a, df_b) %>% distinct()
      fig <- plot_ly(df1, x=~stats, y=~df_a, type='bar', name=player_a, marker = list(color = 'rgb(255, 127, 14)'))
      fig <- fig %>% add_trace(y=~df_b, name=player_b, marker = list(color = 'rgb(44, 160, 44)'))
      fig <- fig %>% layout(barmode='group', showlegend=F)
    } else if (length(df_a)==0 & length(df_b) != 0) {
      df1 <- data.frame(stats, df_b) %>% distinct()
      fig <- plot_ly(df1, x=~stats, y=0, type='bar', name=player_b, marker = list(color = 'rgb(255, 127, 14)'))
      fig <- fig %>% add_trace(y=~df_b, name=player_b, marker = list(color = 'rgb(44, 160, 44)'))
      fig <- fig %>% layout(barmode='group', showlegend=F)
    } else if (length(df_a)!=0 & length(df_b) == 0) {
      df1 <- data.frame(stats, df_a) %>% distinct()
      fig <- plot_ly(df1, x=~stats, y=~df_a, type='bar', name=player_b, marker = list(color = 'rgb(255, 127, 14)'))
      fig <- fig %>% add_trace(y=0, name=player_b, marker = list(color = 'rgb(44, 160, 44)'))
      fig <- fig %>% layout(barmode='group', showlegend=F)
    } else {
      df1 <- as.data.frame(stats,0,0)
      fig <- plot_ly(df1, type='bar')
      fig <- fig %>% layout(barmode='group', showlegend=F)
    }

    return(fig)
  }

  #### BAR PLOT FOR A PLAYER
  pm_bar_comparison <- function(player_a, class_a, player_b, class_b) {
    stats <- c('killspm', 'deathspm', 'cpm')
    df_a <- summed_dr %>% dplyr::filter(nickname == player_a & class_primary == class_a)
    df_a <- df_a[,stats]
    df_a <- as.list(t(df_a))

    df_b <- summed_dr %>% dplyr::filter(nickname == player_b & class_primary == class_b)
    df_b <- df_b[,stats]
    df_b <- as.list(t(df_b))

    if(length(df_a)!=0 & length(df_b)!=0) {
      df1 <- data.frame(stats,df_a, df_b) %>% distinct()
      fig <- plot_ly(df1, x=~stats, y=~df_a, type='bar', name=player_a, marker = list(color = 'rgb(255, 127, 14)'))
      fig <- fig %>% add_trace(y=~df_b, name=player_b, marker = list(color = 'rgb(44, 160, 44)'))
      fig <- fig %>% layout(barmode='group', showlegend=F)
    } else if (length(df_a)==0 & length(df_b) != 0) {
      df1 <- data.frame(stats, df_b) %>% distinct()
      fig <- plot_ly(df1, x=~stats, y=0, type='bar', name=player_b, marker = list(color = 'rgb(255, 127, 14)'))
      fig <- fig %>% add_trace(y=~df_b, name=player_b, marker = list(color = 'rgb(44, 160, 44)'))
      fig <- fig %>% layout(barmode='group', showlegend=F)
    } else if (length(df_a)!=0 & length(df_b) == 0) {
      df1 <- data.frame(stats, df_a) %>% distinct()
      fig <- plot_ly(df1, x=~stats, y=~df_a, type='bar', name=player_b, marker = list(color = 'rgb(255, 127, 14)'))
      fig <- fig %>% add_trace(y=0, name=player_b, marker = list(color = 'rgb(44, 160, 44)'))
      fig <- fig %>% layout(barmode='group', showlegend=F)
    } else {
      df1 <- as.data.frame(stats,0,0)
      fig <- plot_ly(df1, type='bar')
      fig <- fig %>% layout(barmode='group', showlegend=F)
    }

    return(fig)
  }


  combine_plots <- function(player_a, class_a, player_b, class_b) {
    fig1 <- damage_bar_comparison(player_a, class_a, player_b, class_b)
    fig2 <- kills_bar_comparison(player_a, class_a, player_b, class_b)
    fig3 <- pm_bar_comparison(player_a, class_a, player_b, class_b)
    fig_bar <- subplot(fig1, fig2, fig3) %>% layout(showlegend=F)

    return(fig_bar)
  }

  #### TABLE
  comparison_table<- function(player_a, class_a, player_b, class_b) {
    stats <- c('nickname', 'total_games', 'kills', 'assists', 'deaths', 'dpm', 'hpm', 'kad', 'kd', 'dtm', 'deathspm', 'killspm', 'cpm')
    df_a <- summed_dr %>% dplyr::filter(nickname == player_a & class_primary == class_a)
    df_a <- df_a[,stats]
    df_b <- summed_dr %>% dplyr::filter(nickname == player_b & class_primary == class_b)
    df_b <- df_b[,stats]

    tbl <- rbind(df_a, df_b)
    tbl <- round_df(tbl, 2)
    tbl <- reactable(tbl,
                     columns = list(
                     dpm = colDef(name = "DPM",
                       cell = function(value) {
                         width <- paste0(value * 100 / max(tbl$dpm), "%")
                         value <- format(value, big.mark=",")
                         value <- format(value, width=9, justify = "right")
                         bar <- div(
                           class = "bar-chart",
                           style = list(marginRight = "6px"),
                           div(class = "bar", style = list(width=width, backgroundColor = "#3fc1c9"))
                         )
                         div(class = "bar-cell", span(class = "number", value), bar)
                       }
                     ),
                     hpm = colDef(name = "HPM",
                       cell = function(value) {
                         width <- paste0(value * 100 / max(tbl$hpm), "%")
                         value <- format(value, big.mark=",")
                         value <- format(value, width=9, justify = "right")
                         bar <- div(
                           class = "bar-chart",
                           style = list(marginRight = "6px"),
                           div(class = "bar", style = list(width=width, backgroundColor = "#3fc1c9"))
                         )
                         div(class = "bar-cell", span(class = "number", value), bar)
                       }
                     ),
                     nickname = colDef(name = "Player", width=150
                                       )
                     )
                     )
    return(tbl)
  }

  output$compare_bars <- renderPlotly({
    progress <- shiny::Progress$new()
    progress$set(message = "Generating bar plots...", value = 0)
    on.exit(progress$close())

    updateProgress <- function(value = NULL, detail = NULL) {
      if(is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value)/5
      }
      progress$set(value = value, detail = detail)
    }
    combine_plots(input$player_a, input$class_a, input$player_b, input$class_b) %>% config(displaylogo = FALSE)
  })

  output$tableData <- renderReactable({
    comparison_table(input$player_a, input$class_a, input$player_b, input$class_b)
  })


}

shinyApp(ui, server)

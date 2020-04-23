#LogAnalyser app.R
library("dplyr")
library("jsonlite")
library("reactable")

secrets <- function(prepend=NULL) {
  path <- "./secrets.json"
  if(!is.null(prepend)) {
    path <- paste(prepend,"/secrets.json",sep="")
  }
  if (!file.exists(path)) {
    stop("Can't find settings file")
  }
  jsonlite::read_json(path)
}

##Need to redirect for the log analyser.
if(!grepl('scripts', getwd(), fixed=TRUE)) {
  settings <- secrets('../../scripts')
} else {
  settings <- secrets()
}

reset_path <- paste(settings[[5]],"/components/logAnalyser", sep="")

source(file="../../scripts/generate_models.R")
source(file="../../scripts/generate_distributions.R")
finals_adjust <- read.csv("../../data/finals_vars.csv")

####----------------------- FUNCTIONS
fetch_log <- function(log_id) {
  log_url <- paste("http://logs.tf/api/v1/log/",toString(log_id), sep="")
  log_data <- jsonlite::fromJSON(log_url)
  return(log_data)
}

get_info <- function(log) {
  temp <- as.data.frame(0)
  temp$time <- log$info$total_length
  temp$map <- log$info$map
  temp$red_score <- log$teams$Red$score
  temp$blu_score <- log$teams$Blue$score
  return(temp)
}

empty_log <- function() {
  the_log <- data.frame(steam_id=NA, nickname=NA,log_id=NA,map=NA,class_primary=NA,team_colour=NA,mins_total=NA,kills=NA,
                        assists=NA, deaths=NA, damage=NA, damage_taken=NA, heals_received=NA, caps=NA,
                        healing_done=NA, charges=NA, drops=NA, margin=NA
  )[numeric(0),]
}

map_rename <- function(full_name) {
  if(full_name %in% c('cp_granary_pro_rc8','cp_granary_pro_b10', 'cp_granary_pro_rc3', 'cp_granary_pro_rc4')) {
    return("granary")
  } else if(full_name %in% c('cp_gullywash_final1')) {
    return("gullywash")
  } else if(full_name %in% c('cp_process_final')) {
    return("process")
  } else if(full_name %in% c('koth_product_rc9','koth_product_rcx','cp_product_rcx','koth_product_rc8')) {
    return("product")
  }else if(full_name %in% c('cp_gullywash_final1')) {
    return("kalinka")
  }else if(full_name %in% c('cp_gullywash_final1')) {
    return("metalworks")
  }else if(full_name %in% c('cp_prolands_b6','cp_prolands_rc2p','cp_badlands','cp_prolands_b2b','cp_prolands_b4b')) {
    return("prolands")
  }else if(full_name %in% c('cp_logjam_rc10a','cp_logjam_rc5','cp_logjam_rc11','cp_logjam_rc11x')) {
    return("logjam")
  }else if(full_name %in% c('cp_sunshine','cp_sunshine_rc8','cp_sunshine_rc9')) {
    return("sunshine")
  }else if(full_name %in% c('cp_reckoner_rc4a','cp_reckoner_rc5','cp_reckoner_b2a','cp_reckoner_b3a','cp_reckoner_rc1','cp_reckoner_rc2')) {
    return("reckoner")
  }else if(full_name %in% c('koth_bagel_fall_b3')) {
    return("bagel")
  }else if(full_name %in% c('cp_snakewater_final1','cp_snakewater_u13')) {
    return("snakewater")
  } else {
    return("kalinka")
  }

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

populate_log <- function(log_id, updateProgress=NULL) {
  if(is.function(updateProgress)) {
    text <- "Fetching Log from logs.tf..."
    updateProgress(detail = text)
  }
  test <- fetch_log(log_id)
  the_log <- empty_log()

  names <- test$names
  playerdata <- test$players
  steam_ids <- names(playerdata)
  time <- test$length
  map_name <- test$info$map
  map_name <- map_rename(map_name)

  for (p in steam_ids) {
    if(playerdata[[p]]$team == "Blue") {
      ##CHANGE FROM "TEST"
      marg <- (test$teams$Blue$score - test$teams$Red$score)
    } else if(playerdata[[p]]$team == "Red") {
      marg <- (test$teams$Red$score - test$teams$Blue$score)
    }

    player <- data.frame(steam_id=convert_steam_id3(p),
                         nickname=names[[p]],
                         map = map_name,
                         mins_total=time/60,
                         team_colour=playerdata[[p]]$team,
                         class_primary=playerdata[[p]]$class_stats[[1]][[1]],
                         kills=playerdata[[p]]$kills,
                         assists=playerdata[[p]]$assists,
                         deaths=playerdata[[p]]$deaths,
                         damage=playerdata[[p]]$dmg,
                         damage_taken=playerdata[[p]]$dt,
                         heals_received=playerdata[[p]]$hr,
                         caps=playerdata[[p]]$cpc,
                         healing_done=playerdata[[p]]$heal,
                         charges=playerdata[[p]]$ubers,
                         drops=playerdata[[p]]$drops,
                         margin=marg
    )
    the_log <- rbind(the_log,player)
  }
  return(the_log)
}

adjust_finals_vars <- function(alldata) {
  finals_vars <- finals_adjust$finalvar
  combat <- alldata[alldata$class_primary != "medic", ]

  for(var in finals_vars) {
    regmean <- finals_adjust[finals_adjust$finalvar==var,]$regmean
    finalsmean <- finals_adjust[finals_adjust$finalvar==var,]$finalsmean
    regsd <- finals_adjust[finals_adjust$finalvar==var,]$regsd
    finalsd <- finals_adjust[finals_adjust$finalvar==var,]$finalsd
    combat[[var]] <- ((regsd)*((combat[[var]]-finalsmean)/(finalsd))) + regmean
    ##Save for use in log calculator.
  }
  return(combat)
}

pm_stats <- function(the_log, final_input) {
  the_log$dpm <- the_log$damage / the_log$mins_total
  the_log$hpm <- the_log$heals_received / the_log$mins_total
  the_log$healing <- the_log$healing_done / the_log$mins_total
  the_log$kad <- (the_log$kills + the_log$assists) / the_log$deaths
  the_log$dtm <- the_log$damage_taken / the_log$mins_total
  the_log$deathspm <- the_log$deaths / the_log$mins_total
  the_log$cpm <- the_log$caps / the_log$mins_total
  the_log$damageperheal <- the_log$damage / the_log$heals_received
  the_log$damageperdt <- the_log$damage / the_log$damage_taken
  the_log$kpm <- the_log$kills / the_log$mins_total
  the_log$chargespm <- the_log$charges / the_log$mins_total
  the_log$dropspm <- the_log$drops / the_log$mins_total
  the_log$dropsperuber <- the_log$drops / the_log$charges
  the_log$margin_time <- the_log$margin / the_log$mins_total
  the_log$kd <- the_log$kills / the_log$deaths
  the_log$ubersperheal <- the_log$chargespm / the_log$healing
  the_log$deathsperdt <- the_log$deathspm / the_log$dtm

  tryCatch({the_log[is.nan(the_log$dropsperuber), ]$dropsperuber <- 0
  ##Remove infinite values.
  the_log[is.infinite(the_log$kad), ]$kad <- the_log[is.infinite(the_log$kad), ]$kills + the_log[is.infinite(the_log$kad), ]$assists
  the_log[is.infinite(the_log$kd), ]$kd <- the_log[is.infinite(the_log$kd), ]$kills
  the_log[is.nan(the_log$kd),]$kd <- 0
  the_log[is.infinite(the_log$damageperheal), ]$damageperheal <- 5
  the_log[is.nan(the_log$damageperheal),]$damageperheal <- 0},error=function(err){print(err)},finally={})
  final=final_input
  if (final) {
    med_log <- the_log[the_log$class_primary == "medic", ]
    the_log <- adjust_finals_vars(the_log)
    the_log <- rbind(med_log, the_log)
  }

  return(the_log)
}

cap_one <- function(data, vars) {
  for(var in vars) {
    if(length(data[data[[var]] > 1 , ][[var]])) {
      data[data[[var]] > 1 ,][[var]] <- 1
    } else {}
  }
  return(data)
}

run_analysis_log <- function(class) {
  ##GET SCORES - PROBABLY NEED TO FUNCTIONALLY PROGRAM THIS WHOLE THING.
  class$impact <- (0.5*class$perc_dpm + 0.1*class$perc_kad + 0.2*class$perc_kd + 0.2*class$perc_kpm)
  class$survive <- ((1-class$perc_dtm) + (1-class$perc_deathspm))/2
  class$efficiency <- calc_effic_vec(27, class$perc_damageperdt, class$perc_damageperheal)
  class$perc_margin <- pnorm(class$margin_time,0,sd(class$margin_time))
  class$objective <- (0.75*class$perc_margin + 0.25*class$perc_cpm)
  ##FIX SCORES TO HAVE MEAN OF 50%
  class$impact <- ((class$impact - 0.5)) + 0.5
  class$survive <- ((class$survive - 0.5)) + 0.5
  class$efficiency <- ((class$efficiency - 0.5)) + 0.5
  class$objective <- ((class$objective - 0.5)) + 0.5
  ##FIX OUTLIERS
  tryCatch({ class[class$impact < 0.05,]$impact <- 0.05}, error = function(err) {print(err)}, finally = {})
  tryCatch({ class[class$survive < 0.05,]$survive <- 0.05}, error = function(err) {print(err)}, finally = {})
  tryCatch({ class[class$efficiency < 0.05,]$efficiency <- 0.05}, error = function(err) {print(err)}, finally = {})
  tryCatch({ class[class$objective < 0.05,]$objective <- 0.05}, error = function(err) {print(err)}, finally = {})

  cap_vars <- c("impact", "survive", "efficiency", "objective")
  class <- cap_one(class,cap_vars)
  ##GAME SCORE
  class$gamescore <- (50*class$impact) + (20*class$efficiency) + (20*class$survive) + (10*class$objective)
  class$gamescore <- 15*((class$gamescore - 49.9)/16.3) + 50

  class <- class %>% dplyr::select(
    steam_id, team_colour, nickname, class_primary, gamescore, kills, assists,
    deaths, dpm, kad, kd, dtm, hpm, impact, survive, efficiency, objective) %>%
    distinct()

  return(class)
}

run_analysis_log_med <- function(class) {
  class$impact <- (0.5*class$perc_healing + 0.3*class$perc_chargespm + 0.2*class$perc_kad)
  class$survive <- 0.2*(1-class$perc_dtm) + 0.5*(1-class$perc_deathspm) + 0.3*(1-class$perc_dropspm)
  class$perc_margin <- pnorm(class$margin_time,0,sd(class$margin_time))
  class$objective <- (0.8*class$perc_margin + 0.2*class$perc_cpm)
  class$efficiency <- (0.2*(class$perc_ubersperheal)) + (0.4*(1-class$perc_deathsperdt)) + (0.4*(1-class$perc_dropsperuber))
  ##SET MEAN
  class$impact <- ((class$impact - 0.5)) + 0.5
  class$survive <- ((class$survive - 0.5)) + 0.5
  class$efficiency <- ((class$efficiency - 0.5)) + 0.5
  class$objective <- ((class$objective - 0.5)) + 0.5
  ##FIX OUTLIERS
  tryCatch({ class[class$impact < 0.05,]$impact <- 0.05 }, error = function(err) {print(err)}, finally = {})
  tryCatch({ class[class$survive < 0.05,]$survive <- 0.05}, error = function(err) {print(err)}, finally = {})
  tryCatch({ class[class$efficiency < 0.05,]$efficiency <- 0.05}, error = function(err) {print(err)}, finally = {})
  tryCatch({ class[class$objective < 0.05,]$objective <- 0.05}, error = function(err) {print(err)}, finally = {})
  cap_vars <- c("impact", "survive", "efficiency", "objective")
  class <- cap_one(class, cap_vars)

  class$gamescore <- (50*class$impact) + (20*class$efficiency) + (20*class$survive) + (10*class$objective)
  class$gamescore <- 7*((class$gamescore - 47.4)/15) + 47

  class <- class %>% dplyr::select(
    steam_id, team_colour, nickname, class_primary, gamescore, kills, assists,
    deaths, dpm, kad, kd, dtm, hpm, impact, survive, efficiency, objective) %>%
    distinct()

  return(class)
}
score_log <- function(the_log, final_input,updateProgress=NULL) {
  if(is.function(updateProgress)) {
    text <- "Analysing log..."
    updateProgress(detail = text)
  }
  scored_log <- pm_stats(the_log, final_input)
  scored_log <- set_blank_exp(scored_log)

  scout <- scored_log %>% dplyr::filter(class_primary == "scout")
  soldier <- scored_log %>% dplyr::filter(class_primary == "soldier")
  demoman <- scored_log %>% dplyr::filter(class_primary == "demoman")
  medic  <- scored_log %>% dplyr::filter(class_primary == "medic")

  scout <- percentile_stats(scout, "scout")
  soldier <- percentile_stats(soldier, "soldier")
  demoman <- percentile_stats(demoman, "demoman")
  medic <- set_blank_med_exp(medic)
  medic <- percentile_med_stats(medic, "medic")


  test1 <- run_analysis_log(scout)
  test2 <- run_analysis_log(demoman)
  test3 <- run_analysis_log(soldier)
  test4 <- run_analysis_log_med(medic)

  test <- rbind(test1, test2, test3,test4)

  test$impact <- test$impact
  test$survive <- test$survive
  test$objective <- test$objective
  test$efficiency <- test$efficiency

  for (column in names(test)) {
    if (is.numeric(test[[column]])) {
      test[[column]] <- round(test[[column]],2)
    }

  }
  return(test)
}

parse_url <- function(url) {
  if(grepl("/", url, fixed=TRUE)) {
    end <- strsplit(url, "/")[[1]][[4]]
  } else {
    end <- url
  }
  if(grepl("#", end, fixed=TRUE)) {
    log_id <- strtoi(strsplit(end, "#")[[1]][[1]])
  } else {
    log_id <- strtoi(end)
  }
  return(log_id)
}

#####-----------------------------UI---------------------------------------

ui <- function(req) {
    setwd(reset_path)
    fluidPage(
    tags$head(includeHTML("google-analytics.html")),
    tags$link(href = "https://fonts.googleapis.com/css?family=Karla:400,700|Fira+Mono&display=fallback", rel = "stylesheet"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?rnd132"),
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
    titlePanel("Log Analyser"),
    fluidRow(column(12,
        p('Enter a logs.tf url and hit Analyse Log to run any 6\'s game through the ozfstats data models developed from data accrued from 15 season of Premier ozfortress games.'),
        p('If the screen goes grey, the log is not recognised or not supported (i.e. does not work with highlander or other game modes). If this happens refresh the page, I will add input control later...')
    )
    ),
    fluidRow(
      column(2,
            textInput("log_id", "Logs.tf URL", value="")
      ),
      column(6,
             br(),checkboxInput("final", "Is a playoff game?", value=FALSE))
      ),
    fluidRow(
      column(6,
             actionButton("run", "Analyse Log")
      )
    ),
    hr(),
    fluidRow(column(12,
                    reactableOutput('analysed')
                    )
    ),
    fluidRow(
    column(3,
           textOutput("logid")
    ),
    column(3,
           textOutput("map")
    ),
    column(3,
           textOutput("time")
    ),
    column(3,
           textOutput("score")
    )
    ),
    fluidRow(column(6,br(),
                    uiOutput("bookmark_button")
                    )
             ),
    fluidRow(column(6,
                    br(),
                    a(href="https://ozfstats.com/loganalyser.html", "Back to ozfstats.com")
    ), class="back-button", id="url-back-button"
    )
  )
}

#####----------------------------SERVER---------------------------------------

server <- function(input,output,session) {
  
  observeEvent(input$run, {
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

    #### REACTABLE SETTINGS FROM OTHER TABLES ####
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



    log_id <- parse_url(input$log_id)
    ##Get Meta Info
    tmp <- fetch_log(log_id)
    log_info <- get_info(tmp)

    the_log <- populate_log(log_id,updateProgress)
    the_log <- score_log(the_log, input$final, updateProgress)
    selected <- the_log %>% dplyr::select(team_colour, steam_id, nickname, class_primary,gamescore, impact, survive, efficiency, objective)
    selected <- selected %>% arrange(-gamescore)

    min_p <- min(selected[,c("impact", "survive", "efficiency", "objective")])
    max_p <- max(selected[,c("impact", "survive", "efficiency", "objective")])
    min_score <- min(selected$gamescore)
    max_score <- max(selected$gamescore)


    output$analysed <- renderReactable({
      reactable(selected, pagination=FALSE, highlight=TRUE, columns = list(

        nickname = colDef(name="Player", cell = function(value, index) {
          url <- sprintf("http://logs.tf/profile/%s", selected[index, "steam_id"])
          htmltools::tags$a(href = url, target="_blank", as.character(value))
        }),

        steam_id = colDef(show=FALSE),

        team_colour = colDef(name="Team", maxWidth = 60, cell = function(value) {

        },
        style = function(value) {
          if(value == 'Blue') {
            assigned_colour <- "#5B818F"
          } else if(value == 'Red') {
            assigned_colour <- "#A75D50"
          } else {
            assigned_colour <- "White"
          }
          list(background = assigned_colour)
        }),
        gamescore = ranking_column(header = (span("Game Score", title = "Used to compare players, refer to 'About' for more info.")),
                                        format = colFormat(digits=1),
                                        cell = function(value) {
                                          scaled <- (value - min_score) / (max_score - min_score)
                                          color <- ranking_color(scaled)
                                          value <- format(round(value, 1), nsmall = 1)
                                          div(class = "spi-rating", style = list(background = color), value)
                                        }
        ),
        impact = score_column(header = (span("Impact", title = "Considers DPM, KA/D, K/D, KPM, HPM, CHARGES")), class = "border-left"),
        survive = score_column(header = (span("Survivability", title = "Considers DT/M, DEATHS/M, DROPS/M"))),
        efficiency = score_column(header = (span("Efficiency", title = "Considers D/DT, D/HR, DROPS/UBER, K/HR"))),
        objective = score_column(header = (span("Objective", title = "Considers WIN-LOSS, GAME MARGIN, CAPS"))),
        class_primary = colDef(name="Class")
        ), class = "standings-table"
      )
    })

    output$logid <- renderText({
      paste("logs.tf ID:", log_id, sep=" ")
    })
    output$map <- renderText({
      paste("Map:", log_info$map, sep=" ")
    })
    output$time <- renderText({
      paste("Map Time:", round(log_info$time/60, digits=1), "mins.", sep=" ")
    })
    output$score <- renderText({
      paste("BLU:", log_info$blu_score, "RED:", log_info$red_score, sep=" ")
    })

    output$bookmark_button <- renderUI ({
      bookmarkButton(label="Shareable Link")
    })
  })
}
shinyApp(ui, server, enableBookmarking = "url")

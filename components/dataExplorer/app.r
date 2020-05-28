## DATA EXPLORER app.R ##
library(shiny)
library(Cairo)
library("reactable")
library("plotly")
library(reactable)
library(htmltools)
options(shiny.usecairo=T)

##Load data from CSV
Demoman <- read.csv("../../data/allDemoFilter1.csv")
Scout <- read.csv("../../data/allScoutFilter1.csv")
Soldier <- read.csv("../../data/allSoldierFilter1.csv")
Medic <- read.csv("../../data/allmedicFilter1.csv")
All <- rbind(Demoman,Scout,Soldier)
alldata <- read.csv("../../data/allSeasons.csv")
gamedata <- read.csv("../../data/gamedata.csv")
gamedata<-gamedata[order(-gamedata$season,-gamedata$week),]
currentseason<-head(gamedata,1)$season
currentweek<-head(gamedata,1)$week
labels<-read.csv("../../data/labels.csv")
season_ranks <- read.csv("../../data/season_ranks.csv") %>% filter(n>1)
player_names <- sort(season_ranks$nickname)

ui <- fluidPage(
  ## Body content
    tags$script('
        setHeight = function() {
          var window_height = $(window).height();
          var header_height = $(".main-header").height();
          var window_width = $(window).width();
          var boxHeight = window_height - 200;
          var setHeight = Math.min([boxHeight,window_width])
          $("#sidebar-div").height(boxHeight);
          $("#plot-div").height(setHeight);
        };

        // Set input$box_height when the connection is established
        $(document).on("shiny:connected", function(event) {
          setHeight();
        });

        // Refresh the box height on every window resize event
        $(window).on("resize", function(){
          setHeight();
        });
      '),
    tags$link(href = "https://fonts.googleapis.com/css?family=Karla:400,700|Fira+Mono&display=fallback", rel = "stylesheet"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?rnd132"),
              sidebarLayout(
                sidebarPanel(id='sidebar-div',
                             fluidRow(
                               column(2,selectInput("seasonInput","Season",
                                                    choices = c("All","Last5",28:14),selected = 28)),
                               column(5,selectInput("ystat","Y Statistic",
                                                    choices = c("Damage per Minute"="damage_per_minute",
                                                                "KA/D"="ka_d",
                                                                "K/D"="kills_per_death",
                                                                "Kills per Minute"="kills_per_minute",
                                                                "Total Minutes Played"="total_mins_played",
                                                                "Heals Received per Minute" = "heals_per_minute",
                                                                "Damage Taken per Minute" = "dt_per_minute",
                                                                "Caps per Minute" = "caps_per_minute",
                                                                "Deaths per Minute" = "deaths_per_minute",
                                                                "Damage Done per Heal Received" = "damage_per_heal",
                                                                "Damage Done per Damage Taken" = "damage_per_dt",
                                                                "Damage Taken per Heal Received" = "dt_per_heal",
                                                                "Ubers per Minute" = "ubers_per_minute",
                                                                "Healing per Minute" = "healing_per_minute",
                                                                "Drops per Minute" = "drops_per_minute",
                                                                "Drops per Uber" = "drops_per_uber"
                                                    ))),
                               column(5,selectInput("xstat","X Statistic",
                                                    choices = c("Heals Received per Minute"="heals_per_minute",
                                                                "KA/D"="ka_d",
                                                                "K/D"="kills_per_death",
                                                                "Kills per Minute"="kills_per_minute",
                                                                "Total Minutes Played"="total_mins_played",
                                                                "Damage per Minute" = "damage_per_minute",
                                                                "Damage Taken per Minute" = "dt_per_minute",
                                                                "Caps per Minute" = "caps_per_minute",
                                                                "Deaths per Minute" = "deaths_per_minute",
                                                                "Damage Done per Heal Received" = "damage_per_heal",
                                                                "Damage Done per Damage Taken" = "damage_per_dt",
                                                                "Damage Taken per Heal Received" = "dt_per_heal",
                                                                "Ubers per Minute" = "ubers_per_minute",
                                                                "Healing per Minute" = "healing_per_minute",
                                                                "Drops per Minute" = "drops_per_minute",
                                                                "Drops per Uber" = "drops_per_uber"
                                                    ))),
                               column(width=12,
                                 actionButton("runbutton", "Plot as Bar Graph (Y Stat)", "width" = "100%"),
                                actionButton("resetbutton", "Reset", "width" = "100%")),
                               'align'='left'),
                             hr(),
                             fluidRow(
                               column(6,selectInput("classInput", "Classes",
                                                    choices = c("Demoman","Scout","Soldier","Medic","All"),
                                                    selected = "Demoman",)),
                               column(6,selectInput("gametype","Game Type", choices = c("All Games"="All","Regular Season Only" = "Regular", "Playoff Games Only" = "Playoff")))
                             ),
                             hr(),
                             ##actionButton("showtable","Show All data in a Table","width"="100%"),
                             hr(),
                             p("Data sourced for official season matches only directly from ",
                               a("logs.tf",
                                 href = "https://logs.tf/")),
                             p(paste("The most recent data in the database is season ", currentseason," week ", currentweek,sep="")),
                             p("Heal stats for any season prior to 22 are unreliable due to ozfortress servers at this time not counting buffs as heals.  These have been normalised to better reflect the likely actual heal stats if buffs were counted, but are still considered to be an approximation at best"),
                             tags$em(p("Alpha Version - Made with Love by ",
                                       a("Sean.",
                                         href = "https://ozfortress.com/users/50"),'align'="center"))
                ),
                mainPanel(fluidRow(column(width=12,
                                          id='plot-div',
                                          plotlyOutput("theplot", height="800px"),
                                          br(), br()
                ),))
              )
)


server <- function(input,output,session) {
  output$theplot <- renderPlotly({
    ##Create loading popup
    withProgress(message = 'Loading Data', value = 0, {
      n <- 5 ##use this to increase or decrease loading time
      for (i in 1:n) {
        incProgress(1/n, detail = paste("..."))
        Sys.sleep(0.1)
      }
    })
    games<-c(1:20)
    if(input$gametype=="Regular") {
      games<-c(1:7)
    } else if(input$gametype=="Playoff") {
      games<-c(8:12)
    }

    ##All Season Graphs

    if(input$seasonInput=="All"){
      filtered<-get(input$classInput)
      if(input$xstat==input$ystat) {
        if(input$classInput == "All") {
          filtered$name<-paste(filtered$name," - ",filtered$class_primary," (S",filtered$season,")",sep="")
        } else {
          filtered$name<-paste(filtered$name," (S",filtered$season,")",sep="")
        }
        p<-ggplot(filtered, aes(x=reorder(name,get(input$xstat)), y=get(input$xstat), fill=factor(filtered$season))) + geom_bar(stat="identity") + coord_flip() +
          labs(title=toupper(paste("ozfortress premier",input$classInput,"seasons 14-28",sep=" ")), y=toString(labels[which(labels$lookup==input$xstat),]$string), x="", fill="Season")
        ggplotly(p)%>% config(displaylogo = FALSE)
      } else {
        p<-ggplot(filtered, aes(x=get(input$xstat), y=get(input$ystat)
                                , label=name)) + geom_point(aes(colour=factor(filtered$season))) + stat_smooth(method="glm", size=1) + geom_text(check_overlap=TRUE) +
          labs(title=toupper(paste("ozfortress premier",input$classInput,"seasons 14-28",sep=" ")), x=toString(labels[which(labels$lookup==input$xstat),]$string), y=toString(labels[which(labels$lookup==input$ystat),]$string), colour="season") + theme_minimal()
        ggplotly(p, tooltip="label")%>% config(displaylogo = FALSE)
      }
    } else if (input$seasonInput=="Last5") {
      filtered<-get(input$classInput) %>%
        filter(season > (currentseason - 5))
      if(input$xstat==input$ystat) {
        if(input$classInput == "All") {
          filtered$name<-paste(filtered$name," - ",filtered$class_primary," (S",filtered$season,")",sep="")
        } else {
          filtered$name<-paste(filtered$name," (S",filtered$season,")",sep="")
        }
        p<-ggplot(filtered, aes(x=reorder(name,get(input$xstat)), y=get(input$xstat), fill=factor(filtered$season))) + geom_bar(stat="identity") + coord_flip() +
          labs(title=toupper(paste("ozfortress premier",input$classInput,"seasons",currentseason-4,"-",currentseason,sep=" ")), y=toString(labels[which(labels$lookup==input$xstat),]$string), x="", fill="Season")
        ggplotly(p)%>% config(displaylogo = FALSE)
      } else {
        p<-ggplot(filtered, aes(x=get(input$xstat), y=get(input$ystat)
                                , label=name)) + geom_point(aes(colour=factor(filtered$season))) + stat_smooth(method="glm", size=1) + geom_text(check_overlap=TRUE) +
          labs(title=toupper(paste("ozfortress premier",input$classInput,"seasons",currentseason-4,"-",currentseason,sep=" ")), x=toString(labels[which(labels$lookup==input$xstat),]$string), y=toString(labels[which(labels$lookup==input$ystat),]$string), colour="season") + theme_minimal()
        ggplotly(p, tooltip="label")%>% config(displaylogo = FALSE)
      }
    } else {

      ##Specific Season Graphs

      filtered <-
        get(input$classInput) %>%
        filter(season==input$seasonInput)
      if(input$xstat==input$ystat) {
        if (input$classInput == "All") {
          filtered$name<-paste(filtered$name," - ",filtered$class_primary,sep="")
        } else {
          filtered$name<-paste(filtered$name,sep="")
        }
        p<-ggplot(filtered, aes(x=reorder(name,get(input$xstat)), y=get(input$xstat), fill=get(input$xstat))) + geom_bar(stat="identity") + coord_flip() +
          labs(title=toupper(paste("ozfortress premier season",input$seasonInput,input$classInput,"stats",sep=" ")), y=toString(labels[which(labels$lookup==input$xstat),]$string), x="") + theme(legend.position="none")
        ggplotly(p) %>% config(displaylogo = FALSE)
      } else {
        p<-ggplot(filtered, aes(x=get(input$xstat), y=get(input$ystat), label=name))  + geom_point(aes(colour=factor(name))) + stat_smooth(method="glm") + geom_text(check_overlap=TRUE) +
          ggtitle(toupper(paste("ozfortress premier season",input$seasonInput,input$classInput,"stats",sep=" "))) + xlab(toString(labels[which(labels$lookup==input$xstat),]$string)) + ylab(toString(labels[which(labels$lookup==input$ystat),]$string)) + theme(legend.position="none")
        ggplotly(p, tooltip = "label") %>% config(displaylogo = FALSE)
      }
    }
  })
  observeEvent(input$runbutton, {
    updateSelectInput(session, "xstat", selected=input$ystat)
  })
  observeEvent(input$resetbutton, {
    updateSelectInput(session, "seasonInput", selected=28)
    updateSelectInput(session, "ystat", selected="heals_per_minute")
    updateSelectInput(session, "xstat", selected="damage_per_minute")
    updateSelectInput(session, "classInput", selected="Demoman")
    updateSelectInput(session, "gametype", selected="All")
  })

}

shinyApp(ui, server)

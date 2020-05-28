library(reactable)
library(htmltools)
library(shiny)
library(Cairo)
options(shiny.usecairo=T)

demos <- read.csv('../../data/demos.csv')
colnames(demos) <- c('player', 'role', 'map', 'link', 'comments')

demo_table <- function() {
  t <- reactable(demos, columns = list(
  player = colDef(name = "Player",
                  cell = function(value, index) {
                    url <- demos[index, 'link']
                    div(
                      class = "name",
                      htmltools::tags$a(href = url, target = "_blank", value)
                    )
                    }
                  ),
  link = colDef(show = FALSE)
  ), rowStyle = JS("
      function(rowInfo, state) {
      // Add horizontal separators between groups when sorting by role
        var firstSorted = state.sorted[0]
        if (firstSorted && firstSorted.id === 'map') {
          var nextRow = state.pageRows[rowInfo.viewIndex + 1]
          if (nextRow && rowInfo.row['map'] !== nextRow['map']) {
            // Use box-shadow to add a 2px border without taking extra space
            return { boxShadow: 'inset 0 -2px 0 rgba(0, 0, 0, 0.1)' }
          }
        }
        if (firstSorted && firstSorted.id === 'role') {
          var nextRow = state.pageRows[rowInfo.viewIndex + 1]
          if (nextRow && rowInfo.row['role'] !== nextRow['role']) {
            // Use box-shadow to add a 2px border without taking extra space
            return { boxShadow: 'inset 0 -2px 0 rgba(0, 0, 0, 0.1)' }
          }
        }
      }
    "),
  pagination = FALSE,
  filterable = TRUE,
  striped = TRUE,
  bordered = TRUE
  )

  return(t)
}

###UI
ui <- fluidPage (
  tags$title("OZF POV Demo Database"),
  tags$link(href = "https://fonts.googleapis.com/css?family=Karla:400,700|Fira+Mono&display=fallback", rel = "stylesheet"),
  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?rnd132"),

  column(12,
         fluidRow(
           column(12,
                  div(class = "title", h2("POV Demo Database")),
                  div(class = "description", p("Click player name to download POV demo. Thank you to all the players who have provided these demos!"),
                      "Some of these demos use old versions of maps, you should be able to", tags$a(href = "https://drive.google.com/open?id=1PS2weyjZlcY6m5i6uj_QypXhpBLowwmu", "find them here", target="_blank"),
                      br(),br(),h4("If you are unfamiliar with how to play POV demos:"),
                      tags$ol(
                        tags$li("Download the desired .dem file and place it in the root of your tf directory (C:/Program Files (x86)/Steam/SteamApps/common/Team Fortress 2/tf)"),
                        tags$li("Type 'playdemo <demoname>.dem' into TF2 console"),
                        tags$li("Use Shift+F2 to bring up the Demo UI"),
                        tags$li("NOTE: Demos recorded using Valve's recorder (not PREC) will record all the junk before an official starts. This is the case for some of these demos, you will have to use the tick skip/fast forward to jump to the start of the game.")
                      ),
                      h4("Some other notes about these demos:"),
                      tags$ol(
                        tags$li("You can only learn so much from watching someone else's POV - you do not have team comms or the thoughts of the player."),
                        tags$li("Every team composition is different, and the role each player is playing within the team might not be the same."),
                        tags$li("To make the most of these demos, think critically and ask yourself why the player is doing the things they are doing."),
                        tags$li("Constantly think about the uber situation while watching these demos, and how the players change their behaviour during different situations."),
                        tags$li("Do not be afraid to DM these players and ask them to explain things or for general advice!")
                      )
                      )
                         )),
         fluidRow(
           column(12,
                  reactableOutput("demotable")
                  )
         ),
         fluidRow(
           column(12)
         )
         )
)

###SERVER

server <- function(input, output, session) {
  output$demotable <- renderReactable({
    demo_table()
  })
}

shinyApp(ui,server)

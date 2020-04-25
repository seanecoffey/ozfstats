##Run to update the data after new information is added to the database.

setwd("./scripts")
secrets <- function() {
  path <- "secrets.json"
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

setwd()
setwd("./scripts")
source(file="generate_rankings.R")
setwd()
setwd("./scripts")
source(file="generate_player_profiles.R")
setwd()
setwd("./scripts")
source(file="generate_data_explorer.R")
setwd()
setwd("./scripts")
source(file="generate_player_names.R")

rm(list=ls())

library(shiny)
library(profvis)

##Profile different apps to check for load errors.
##profvis({
##  runApp("app.R")
##  })

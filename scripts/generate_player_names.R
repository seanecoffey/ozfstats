##generate_player_names.R
##Dependencies
library("dplyr")
library("dbConnect")
library("jsonlite")
library("bit64")

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

if(!grepl('scripts', getwd(), fixed=TRUE)) {
  settings <- secrets('./scripts')
} else {
  settings <- secrets()
}

setwd <- function(dir) {
  if (missing(dir) || is.null(dir) || dir == "") {
    dir <- settings[[5]]
  }
  base::setwd(dir)
}

##Connect to the database -
con <- dbConnect(RPostgres::Postgres(), dbname = "tf2",
                 host = settings[[1]], port = settings[[2]],
                 user = settings[[3]], password = settings[[4]])

##Get relevant database views
res <- dbSendQuery(con, "select * from public.players")
players <- dbFetch(res)

setwd()
setwd('./data')
write.csv(players, 'players.csv')

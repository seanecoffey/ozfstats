library(plumber)
library(reactable)
source(file = 'LogAnalyserApi.R')

#* @apiTitle LogAnalyserAPI

#' @get /hello
#' @html
function(){
  "<html><h1>hello world</h1></html>"
}

#' @param log_id
#' @get /log
#' @serializer htmlwidget
function (log_id) {
  log_id <- as.integer(log_id)
  t <- generate_table(log_id)
  return(t)
}

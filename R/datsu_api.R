#' Build the api url for given service
#'
#' utility to build an api url with optional query arguemnts
#'
#' @param service the name of the service
#' @param ... name arguments will be added as queries
#'
#' @examples
#'
#' datsu_api("hi", bye = 21)
#' datsu_api("getDataverIDs")
#' 
#' @export
#' @importFrom httr parse_url build_url GET
datsu_api <- function(service, ...) {
  url <- paste0(api_url(), "/", service)
  url <- parse_url(url)
  url$query <- list(...)
  url <- build_url(url)

  url
}

#' @importFrom icesConnect ices_get_jwt
#' @importFrom httr content
datsu_get <- function(url, retry = TRUE, quiet = FALSE, verbose = FALSE, content = TRUE) {

  resp <-
    ices_get_jwt(
      url,
      retry = retry, quiet = quiet, verbose = verbose
    )

  if (content) {
    out <- content(resp, simplifyVector = TRUE)
    trimws_df(out)
  } else {
    resp
  }
}

#' @importFrom icesConnect ices_post_jwt
datsu_post <- function(url, body = list(), retry = TRUE, verbose = FALSE) {

  out <- ices_post_jwt(url, body,
    encode = "multipart",
    retry = retry,
    verbose = verbose
  )

  return(out)
}

api_url <- function() {
  # make an option?
  "https://datsu.ices.dk/api"
}

trimws_df <- function(df) {
  df[] <-
    lapply(
      df,
      function(x) if (is.character(x)) trimws(x) else x
    )

  df
}

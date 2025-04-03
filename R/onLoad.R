
#' @importFrom memoise memoise
#' @importFrom cachem cache_mem
#' @importFrom rlang hash

# Expire items in cache after 15 minutes
getcache <- cachem::cache_mem(max_age = 15 * 60)

.onLoad <- function(libname, pkgname) {

  # set functions to use caching
  datsu_get_cached <<-
    memoise::memoise(
      datsu_get_cached,
      cache = getcache,
      hash = function(x) hash(x$url)
    )

  invisible()
}

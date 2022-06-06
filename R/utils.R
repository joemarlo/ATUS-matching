`%notin%` <- Negate(`%in%`)
scale_01 <- function(x) (x - min(x))/diff(range(x))

#' Format minutes as hour:minute
#'
#' @param min a numeric representing a count of minutes
#'
#' @return
#' @export
#'
#' @examples
#' format_hour_minute(230)
format_hour_minute <- function(min) {
  H <- floor(min / 60)
  M <- min %% 60
  M <- ifelse(nchar(M) == 1, paste0('0', M), as.character(M))
  formatted <- paste0(H, ":", M)
  return(formatted)
}


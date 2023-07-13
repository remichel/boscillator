#' bosc
#'
#' @description \code{bosc}
#' Creates an empty bosc object for further processing with boscillator functions.
#'
#'
#' @return A bosc object
#' @export bosc
#' @name bosc
#'
#' @examples
#' bosc = bosc()
#'

bosc <- function() {

  # create bosc object
  bosc <- list(
    timepoints = NULL,
    data = list(
      single_trial = NULL,
      ss = NULL,
      ga = NULL
    ),
    tests = NULL,
    hist = ""
  )
  class(bosc) <- "BOSC-Object"

  return(bosc)
}

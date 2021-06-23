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
#' @author René Michel

bosc <- function(){

  # create bosc object
  bosc = list(timepoints = NULL,
              data = list(single_trial = NULL,
                          single_subject = NULL,
                          grand_average = NULL),
              preprocessing = NULL,
              tests = NULL)
  class(bosc) = "BOSC-Object"

  return(bosc)

}

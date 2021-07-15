#' split_string_arg
#'
#' #' @description \code{split_string_arg}
#' Simple helper to split and unlist a string
#'
#' @param string A string, e.g. "ss-ga"
#' @param sep A separator, e.g. "-"
#'
#' @return a vector of strings
#' @noRd
#'
#' @examples
#' string_vec = split_string_arg("ss-ga", "-")
#'
#' #' @author René Michel
#'
split_string_arg <- function(string, sep){

  string_list = strsplit(string, sep)
  string_vec = unlist(string_list)
  return(string_vec)

}

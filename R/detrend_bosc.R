#' detrend_bosc
#'
#' @description \code{detrend_bosc}
#' Detrends time courses in an BOSC-Object.
#'
#' @param bosc BOSC-Object
#' @param types Which data should be detrended? choose between "real" and "surrogate" or concatenate to "real-surrogate" to perform aggregation on both.
#' @param levels Which levels of data need to be detrended? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
#' @param order Choose an order of the polynomial fit which will then be removed (0 = demean)
#'
#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export detrend_bosc
#' @name detrend_bosc
#'
#' @examples
#'
detrend_bosc <- function(bosc, types = "real-surrogate", levels = "ss-ga", order = 0) {

  # get levels
  if(!is.character(levels)){
    stop("Argument levels must be a character.")
  }else{
    level_list <- split_string_arg(levels, "-")
  }

  # get types
  if(!is.character(types)){
    stop("Argument types must be a character.")
  }else{
    type_list <- split_string_arg(types, "-")
  }

  # check order
  if(order < 0){
    stop("Order must be a positive integer.")
  }else if((order %% 1) != 0){
    stop("Order must be an integer.")
  }else{
    poly_order <- max(order,1)
  }

  message("Start detrending...")
  # loop through all conditions
  for (type in type_list) {
    for (level in level_list) {

      message(paste("Detrending", level, type, "..."))

      # check if required data exists
      if(is.null(bosc$data[[level]][[type]]$data)){
        message(paste("No data found in ", level, type, ".\nWill continue with next type/level..."))
        next
      }

      # check whether detrending was already applied for the condition at hand
      if(!is.null(bosc$data[[level]][[type]]$preprocessing)){
        if("DETRENDED" %in% split_string_arg(bosc$data[[level]][[type]]$preprocessing, "_")){
          reply = utils::menu(c("Yes", "No"), title = paste("Data in", level, type, "was already detrended. Are you sure you want to continue with yet another detrending?"))
          if(reply == 2){
            stop("Execution stopped.")
          }
        }
      }

      # detrend
      if (type == "real") {

        if (level == "ss") {
          bosc$data[[level]][[type]]$data <- bosc$data[[level]][[type]]$data %>%
            dplyr::group_by(.data$subj) %>%
            dplyr::mutate(hr = dplyr::case_when(
              !!order > 0 ~ stats::lm(.data$hr ~ stats::poly(.data$time, !!poly_order))$residuals,
              !!order <= 0 ~ .data$hr - mean(.data$hr)
            ))
        } else if (level == "ga") {
          bosc$data[[level]][[type]]$data <- bosc$data[[level]][[type]]$data %>%
            dplyr::mutate(hr = dplyr::case_when(
              !!order > 0 ~ stats::lm(.data$hr ~ stats::poly(.data$time, !!poly_order))$residuals,
              !!order <= 0 ~ .data$hr - mean(.data$hr)
            ))
        }
      } else if (type == "surrogate") {

        if (level == "ss") {
          bosc$data[[level]][[type]]$data <- bosc$data[[level]][[type]]$data %>%
            dplyr::group_by(.data$subj, .data$n_surr) %>%
            dplyr::mutate(hr = dplyr::case_when(
              !!order > 0 ~ stats::lm(.data$hr ~ stats::poly(.data$time, !!poly_order))$residuals,
              !!order <= 0 ~ .data$hr - mean(.data$hr)
            ))
        }
        else if (level == "ga") {
        bosc$data[[level]][[type]]$data <- bosc$data[[level]][[type]]$data %>%
          dplyr::group_by(.data$n_surr) %>%
          dplyr::mutate(hr = dplyr::case_when(
            !!order > 0 ~ stats::lm(.data$hr ~ stats::poly(.data$time, !!poly_order))$residuals,
            !!order <= 0 ~ .data$hr - mean(.data$hr)
          ))
        }
      }

      # add preprocessing step to documentation
      if (is.null(bosc$data[[level]][[type]]$preprocessing)) {
        bosc$data[[level]][[type]]$preprocessing <- paste0("DETRENDED_ORDER:", order)
      } else {
        bosc$data[[level]][[type]]$preprocessing <- paste(bosc$data[[level]][[type]]$preprocessing, paste0("DETRENDED_ORDER:", order), sep = "_")
      }

    }
  }

  # add executed command to history
  bosc$hist <- paste0(bosc$hist, "detrend_")

  message("Detrending completed.")
  return(bosc)
}

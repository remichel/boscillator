#' detrend_bosc
#'
#' @description \code{detrend_bosc}
#' Detrends time courses in an BOSC-Object.
#'
#' @param bosc BOSC-Object
#' @param types Which data should be aggregated? choose between "real" and "surrogate" or concatenate to "real-surrogate" to perform aggregation on both.
#' @param levels Which levels of aggregation need to be created? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
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
detrend_bosc <- function(bosc, types = "real", levels = "ss-ga", order = 0) {

  # get levels
  level_list <- split_string_arg(levels, "-")

  # get types
  type_list <- split_string_arg(types, "-")

  # set order
  poly_order <- max(order,1)

  # loop through all conditions
  for (type in type_list) {
    for (level in level_list) {

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
      } else if (level == "ga") {
        bosc$data[[level]][[type]]$data <- bosc$data[[level]][[type]]$data %>%
          dplyr::group_by(.data$n_surr) %>%
          dplyr::mutate(hr = dplyr::case_when(
            !!order > 0 ~ stats::lm(.data$hr ~ stats::poly(.data$time, !!poly_order))$residuals,
            !!order <= 0 ~ .data$hr - mean(.data$hr)
          ))
      }

      # add preprocessing step to documentation
      if (is.null(bosc$data$ga[[type]]$preprocessing)) {
        bosc$data$ga[[type]]$preprocessing <- paste0("DETRENDED_ORDER:", order)
      } else {
        bosc$data$ga[[type]]$preprocessing <- paste(bosc$data$ga[[type]]$preprocessing, paste0("DETRENDED_ORDER:", order), sep = "---")
      }
    }
  }
  return(bosc)
}

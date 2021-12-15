#' detrend_bosc
#'
#' @description \code{detrend_bosc}
#' Detrends time courses in an BOSC-Object.
#'
#' @param bosc BOSC-Object
#' @param types Which data should be detrended? choose between "real" and "surrogate" or concatenate to "real-surrogate" to perform aggregation on both.
#' @param levels Which levels of data need to be detrended? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
#' @param order Choose an order of the polynomial fit which will then be removed (0 = demean). Order = -1 will lead to skipping detrending.
#' @param verbose defaults to T
#'
#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export detrend_bosc
#' @name detrend_bosc
#'
#' @examples
#' bosc = simulate_experiment()
#' bosc = detrend_bosc(bosc, types = "real", levels = "ss", order = 1)
#'
detrend_bosc <- function(bosc,
                         types = c("real", "surrogate"),
                         levels = c("ss", "ga"),
                         order = 0,
                         verbose = TRUE) {

  # get levels
  if (!is.character(levels)) {
    stop("Argument levels must be a character.")
  }

  # get types
  if (!is.character(types)) {
    stop("Argument types must be a character.")
  }

  # check order
  if (order < 0) {
    stop("Negative order. No detrending will be applied...")
  } else if ((order %% 1) != 0) {
    stop("Order must be an integer. No detrending will be applied...")
  } else {
    poly_order <- max(order, 1)
  }


  if (verbose == TRUE) message("Start detrending...")

  # loop through all conditions
  for (iType in types) {
    for (iLevel in levels) {
      if (verbose == TRUE) message(paste("Detrending", iLevel, iType, "..."))

      # check if required data exists
      if (is.null(bosc$data[[iLevel]][[iType]]$data)) {
        if (verbose == TRUE) message(paste("No data found in ", iLevel, iType, ".\nWill continue with next iType/iLevel..."))
        next
      }

      # check whether detrending was already applied for the condition at hand
      if (!is.null(bosc$data[[iLevel]][[iType]]$preprocessing)) {
        if ("DETRENDED" %in% split_string_arg(bosc$data[[iLevel]][[iType]]$preprocessing, "_")) {
          reply <- utils::menu(c("Yes", "No"), title = paste("Data in", iLevel, iType, "was already detrended. Are you sure you want to continue with yet another detrending?"))
          if (reply == 2) {
            next
          }
        }
      }

      # define group vars for the following step
      group_vars <- NULL
      # for single subject data, group by subject
      if (iLevel == "ss") {
        group_vars <- c(group_vars, "subj")
      }
      # for surrogate data, group by n_surr
      if (iType == "surrogate") {
        group_vars <- c(group_vars, "n_surr")
      }

      # de-trending
      bosc$data[[iLevel]][[iType]]$data <- bosc$data[[iLevel]][[iType]]$data %>%
        dplyr::group_by_at(group_vars) %>%
        dplyr::mutate(hr = dplyr::case_when(
          !!order > 0 ~ stats::lm(.data$hr ~ stats::poly(.data$time, !!poly_order))$residuals,
          !!order <= 0 ~ .data$hr - mean(.data$hr)
        ))



      # add preprocessing step to documentation
      if (is.null(bosc$data[[iLevel]][[iType]]$preprocessing)) {
        bosc$data[[iLevel]][[iType]]$preprocessing <- paste0("DETRENDED_ORDER:", order)
      } else {
        bosc$data[[iLevel]][[iType]]$preprocessing <- paste(bosc$data[[iLevel]][[iType]]$preprocessing, paste0("DETRENDED_ORDER:", order), sep = "_")
      }
    }
  }


  # add executed command to history
  bosc$hist <- paste0(bosc$hist, "detrend_")


  if (verbose == TRUE) message("Detrending completed.")
  return(bosc)
}

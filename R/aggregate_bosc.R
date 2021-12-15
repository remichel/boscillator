#' aggregate_bosc
#'
#'
#' #' @description \code{aggregate_bosc}
#' Aggregates various level of data from a BOSC-Object.
#'
#' @param bosc BOSC-Object
#' @param levels Which levels of aggregation need to be created? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
#' @param overwrite whether to overwrite existing data. defaults to F.
#' @param types Which data should be aggregated? choose between "real" and "surrogate" or concatenate to "real-surrogate" to perform aggregation on both.
#'
#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export aggregate_bosc
#' @name aggregate_bosc
#'
#' @examples
#' bosc = simulate_experiment(n = 10, n_timepoints = 10, n_trials = 10)
#' bosc = aggregate_bosc(bosc, types = "real", levels = "ss", overwrite = TRUE)
#'
aggregate_bosc <- function(bosc,
                           types = c("real"),
                           levels = c("ss", "ga"),
                           overwrite = FALSE) {

  # check for bosc object
  if (class(bosc) != "BOSC-Object") {
    stop("No object of class 'BOSC-Object' found. Consider using 'bosc()' to generate a BOSC object before calling this function.")
  }

  # get levels
  if (!is.character(levels)) {
    stop("Argument levels must be a character.")
  }

  # get types
  if (!is.character(types)) {
    stop("Argument types must be a character.")
  }


  # loop through all conditions
  for (iType in types) {
    for (iLevel in levels) {
      if (is.null(bosc$data[[iLevel]][[iType]]) | overwrite == TRUE) {

        # check for existing data
        if (!is.null(bosc$data[[iLevel]][[iType]])) message("Data already exists and will be overwritten...")


        # which levels of data should be aggregated?
        if (iLevel == "ss") {
          group_subj <- "subj"
          input_level <- "single_trial"
          output_level <- "ss"
          var <- "resp"
        } else if (iLevel == "ga") {
          group_subj <- NULL
          input_level <- "ss"
          output_level <- "ga"
          var <- "hr"
        }

        # define grouping variables based on the type of data
        if (iType == "surrogate") {
          group_vars <- c(group_subj, "time", "n_surr")
        } else {
          group_vars <- c(group_subj, "time")
        }

        # aggregate
        bosc$data[[output_level]][[iType]]$data <- bosc$data[[input_level]][[iType]]$data %>%
          dplyr::group_by_at(group_vars) %>%
          dplyr::summarise(hr = mean(!!as.name(var)))
      } else {
        stop("Data already exists. If you want to overwrite, please specify as overwrite = T.")
      }
    }
  }

  # add executed command to history
  bosc$hist <- paste0(bosc$hist, "aggregate_")

  return(bosc)
}

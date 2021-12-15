#' scale_bosc
#'
#' @description \code{scale_bosc}
#' z-scales time courses in an BOSC-Object.
#'
#' @param bosc BOSC-Object
#' @param types Which data should be scaled? choose between "real" and "surrogate" or concatenate to "real-surrogate" to perform aggregation on both.
#' @param levels Which levels of data need to be scaled? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
#' @param method defaults to z
#' @param verbose defaults to T
#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export scale_bosc
#' @name scale_bosc
#'
#' @examples
#' bosc = simulate_experiment()
#' bosc = scale_bosc(bosc, types = "real", levels = "ga")
#'
scale_bosc <- function(bosc,
                       types = c("real", "surrogate"),
                       levels = c("ss", "ga"),
                       method = "z",
                       verbose = TRUE) {

  # get levels
  if (!is.character(levels)) {
    stop("Argument levels must be a character.")
  }

  # get types
  if (!is.character(types)) {
    stop("Argument types must be a character.")
  }

  # get method
  if (method == "none") {
    if (verbose == TRUE) message("Will skip scaling...")
    skip <- TRUE
  } else if (method == "z") {
    skip <- FALSE
  } else {
    stop("There is no other method than z-scaling (or no scaling) implemented yet.")
  }


  if (skip == FALSE) {
    if (verbose == TRUE) message("Start Scaling...")

    # loop through all conditions
    for (iType in types) {
      for (iLevel in levels) {
        if (verbose == TRUE) message(paste("Scaling", iLevel, iType, "..."))

        # check if required data exists
        if (is.null(bosc$data[[iLevel]][[iType]]$data)) {
          if (verbose == TRUE) message(paste("No data found in ", iLevel, iType, ".\nWill continue with next iType/iLevel..."))
          next
        }

        # check whether padding was already applied for the condition at hand
        if (!is.null(bosc$data[[iLevel]][[iType]]$preprocessing)) {
          if ("SCALED" %in% split_string_arg(bosc$data[[iLevel]][[iType]]$preprocessing, "_")) {
            reply <- utils::menu(c("Yes", "No"), title = paste("Data in", iLevel, iType, "was already scaled. Are you sure you want to continue with yet another scaling?"))
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


        # scaling
        bosc$data[[iLevel]][[iType]]$data <- bosc$data[[iLevel]][[iType]]$data %>%
          dplyr::group_by_at(group_vars) %>%
          dplyr::mutate(hr = scale(.data$hr))

        # add preprocessing step to documentation
        if (is.null(bosc$data[[iLevel]][[iType]]$preprocessing)) {
          bosc$data[[iLevel]][[iType]]$preprocessing <- paste0("SCALED_METHOD:", method)
        } else {
          bosc$data[[iLevel]][[iType]]$preprocessing <- paste(bosc$data[[iLevel]][[iType]]$preprocessing, paste0("SCALED_METHOD:", method), sep = "_")
        }
      }
    }

    # add executed command to history
    bosc$hist <- paste0(bosc$hist, "scaled_")
  }

  if (verbose == TRUE) message("Scaling completed.")
  return(bosc)
}

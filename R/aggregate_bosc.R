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
aggregate_bosc <- function(bosc, types = "real", levels = "ss-ga", overwrite = FALSE){

  # check for bosc object
  if(class(bosc) != "BOSC-Object"){
    stop("No object of class 'BOSC-Object' found. Consider using 'bosc()' to generate a BOSC object before calling this function.")
  }

  # get levels
  level_list = split_string_arg(levels, "-")

  # get types
  type_list = split_string_arg(types, "-")

  # loop through all conditions
  for(type in type_list){

    for(level in level_list){

        if(is.null(bosc$data[[level]][[type]]) | overwrite == TRUE){

        # check for existing data
        if(!is.null(bosc$data[[level]][[type]])) message("Data already exists and will be overwritten...")

        # single subject
        if(level == "ss"){

          # define grouping variables based on the type of data
          if(type == "surrogate"){
            group_vars = rlang::syms(c("subj", "time", "n_surr"))
          }else{
            group_vars = rlang::syms(c("subj", "time"))
          }

          # aggregate
          bosc$data[[level]][[type]]$data <- bosc$data$single_trial[[type]]$data %>%
            dplyr::group_by(!!!group_vars) %>%
            dplyr::summarise(hr = mean(.data$resp))
        }

        # grand average
        if(level == "ga"){

          # define grouping variables based on the type of data
          if(type == "surrogate"){
            group_vars = rlang::syms(c("time", "n_surr"))
          }else{
            group_vars = rlang::syms(c("time"))
          }

          # aggregate
          bosc$data[[level]][[type]]$data <- bosc$data$ss[[type]]$data %>%
            dplyr::group_by(!!!group_vars) %>%
            dplyr::summarise(hr = mean(.data$hr))
        }

      }else{

        stop(paste("Data already exists. If you want to overwrite, please specify as overwrite = T."))

      }
    }

  }

  # add executed command to history
  bosc$hist <- paste0(bosc$hist, "aggregate_")

  return(bosc)
}

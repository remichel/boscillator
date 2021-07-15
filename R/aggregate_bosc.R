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
  levels = split_string_arg(levels, "-")

  # get types
  types = split_string_arg(types, "-")

  for(type in types){

    # single subject time courses
    if("ss" %in% levels){


      if(is.null(bosc$data$single_subject[[type]]) | overwrite == TRUE){

        # check for existing data
        if(!is.null(bosc$data$single_subject[[type]])) message(paste("Single subject", type, "already exists and will be overwritten..."))

        # define grouping variables based on the type of data
        if(type == "surrogate"){
          group_vars = rlang::syms(c("subj", "time", "n_surr"))
        }else{
          group_vars = rlang::syms(c("subj", "time"))
        }

        # aggregate
        bosc$data$single_subject[[type]]$data <- bosc$data$single_trial[[type]]$data %>%
          dplyr::group_by(!!!group_vars) %>%
          dplyr::summarise(hr = mean(.data$resp))

      }else{

        stop(paste("Single subject", type, "already exists. If you want to overwrite, please specify as overwrite = T."))

      }
    }

    # grand average
    if("ga" %in% levels){

      if(is.null(bosc$data$grand_average[[type]]) | overwrite == TRUE){

        # check for existing data
        if(!is.null(bosc$data$grand_average[[type]])) message(paste("Grand average", type, " already exists and will be overwritten..."))

        # define grouping variables based on the type of data
        if(type == "surrogate"){
          group_vars = rlang::syms(c("time", "n_surr"))
        }else{
          group_vars = rlang::syms(c("time"))
        }

        # aggregate
        bosc$data$grand_average[[type]]$data <- bosc$data$single_subject[[type]]$data %>%
          dplyr::group_by(!!!group_vars) %>%
          dplyr::summarise(hr = mean(.data$hr))

      }else{

        stop(paste("Grand average", type, "already exists. If you want to overwrite, please specify as overwrite = T."))

      }
    }
  }

  return(bosc)
}

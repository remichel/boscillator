#' aggregate_bosc
#'
#'
#' #' @description \code{aggregate_bosc}
#' Aggregates various level of data from a BOSC-Object.
#'
#' @param bosc BOSC-Object
#' @param type Which data needs to be aggregated? You can choose between "real" and "surrogate". Concatenate multiple levels with "-", e.g. "real-surrogate".
#' @param levels Which levels of aggregation need to be created? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
#' @param overwrite
#'
#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export aggregate_bosc
#' @name aggregate_bosc
#'
#' @examples
#' bosc = aggregate_bosc(bosc, types = "real-surrogate", levels = "ss-ga", overwrite = T)
#'
aggregate_bosc <- function(bosc, types = "real", levels = "ss-ga", overwrite = F){

  # check for bosc object
  if(class(bosc) != "BOSC-Object"){
    stop("No object of class 'BOSC-Object' found. Consider using 'bosc()' to generate a BOSC object before calling this function.")
  }

  # get levels
  levels = .split_string_arg(levels, "-")

  # get types
  types = .split_string_arg(types, "-")

  for(type in types){

    # use correct grouping variables depending on the type of data
    if(type == "real"){
      group_vars = dplyr::quos(.data$subj, .data$time, .data$n_surr)
    }else if(type == "surrogate"){
      group_vars = dplyr::quos(.data$subj, .data$time)
    }

    # single subject time courses
    if("ss" %in% levels){

      if(is.null(bosc$data$single_subject[[type]]) | overwrite == T){

        if(!is.null(bosc$data$single_subject[[type]])) message(paste("Single subject", type, "already exists and will be overwritten..."))




        bosc$data$single_subject[[type]]$data <- bosc$data$single_trial[[type]]$data %>%
          dplyr::group_by(.data$subj, .data$time) %>%
          dplyr::summarise(hr = mean(.data$resp))

      }else{

        stop(paste("Single subject", type, "already exists. If you want to overwrite, please specify as overwrite = T."))

      }
    }

    # grand average
    if("ga" %in% levels){

      if(is.null(bosc$data$grand_average[[type]]) | overwrite == T){

        if(!is.null(bosc$data$grand_average[[type]])) message(paste("Grand average", type, " already exists and will be overwritten..."))

        bosc$data$grand_average[[type]]$data <- bosc$data$single_subject[[type]]$data %>%
          dplyr::group_by(.data$time) %>%
          dplyr::summarise(hr = mean(.data$hr))

      }else{

        stop(paste("Grand average", type, "already exists. If you want to overwrite, please specify as overwrite = T."))

      }
    }
  }

  return(bosc)
}

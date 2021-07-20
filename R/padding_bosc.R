#' padding_bosc
#'
#' @description \code{padding_bosc}
#' Adds pads to the time courses in an BOSC-Object.
#'
#' @param bosc BOSC-Object
#' @param types Which data should be padded? choose between "real" and "surrogate" or concatenate to "real-surrogate" to perform aggregation on both.
#' @param levels Which levels of data need to be padded? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
#' @param method Padding method? "zero" or "mean"
#' @param n_pads Number of pads
#'
#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export padding_bosc
#' @name padding_bosc
#'
#' @examples
#' bosc = simulate_experiment()
#' bosc = padding_bosc(bosc, types = "real", levels = "ga", method = "zero")
#'
padding_bosc <- function(bosc, types = "real-surrogate", levels = "ss-ga", method = "zero", n_pads = length(bosc$timepoints)) {

  # get levels
  if(!is.character(levels)){
    stop("Argument levels must be a character.")
  }else{
    iLevel_list <- split_string_arg(levels, "-")
  }

  # get types
  if(!is.character(types)){
    stop("Argument types must be a character.")
  }else{
    iType_list <- split_string_arg(types, "-")
  }

  # check method
  if(!is.character(method)){
    stop("Method must be a character.")
  }

  # determine pad lengths
  n_prior = n_pads/2
  n_after = n_pads - n_prior

  # determine sampling interval
  s_interval = 1/bosc$data$single_trial$real$spec$sfreq

  # create pad vectors
  pads_prior = seq(max(bosc$timepoints) + s_interval,
                   max(bosc$timepoints) + n_prior * s_interval,
                   s_interval)

  pads_after = seq(min(bosc$timepoints) - n_after * s_interval,
                   min(bosc$timepoints) - s_interval,
                   s_interval)


  message("Start Padding...")

  # loop through all conditions
  for (iType in iType_list) {
    for (iLevel in iLevel_list) {

      message(paste("Padding", iLevel, iType, "..."))

      # check if required data exists
      if(is.null(bosc$data[[iLevel]][[iType]]$data)){
        message(paste("No data found in ", iLevel, iType, ".\nWill continue with next iType/iLevel..."))
        next
      }

      # check whether padding was already applied for the condition at hand
      if(!is.null(bosc$data[[iLevel]][[iType]]$preprocessing)){
        if("PADDED" %in% split_string_arg(bosc$data[[iLevel]][[iType]]$preprocessing, "_")){
          reply = utils::menu(c("Yes", "No"), title = paste("Data in", iLevel, iType, "was already padded. Are you sure you want to continue with yet another padding?"))
          if(reply == 2){
            stop("Execution stopped.")
          }
        }
      }



      # padding
      if (iType == "real") {

        if (iLevel == "ss") {

          # create pads
          pads <- expand.grid(unique(bosc$data[[iLevel]][[iType]]$data$subj),
                              c(pads_prior, pads_after),
                              0) %>%
            stats::setNames(names(bosc$data[[iLevel]][[iType]]$data))

          # add pads to datasets
          bosc$data[[iLevel]][[iType]]$data <- bosc$data[[iLevel]][[iType]]$data %>%
            dplyr::ungroup() %>%
            dplyr::add_row(!!pads) %>%
            dplyr::arrange(.data$subj, .data$time)

        }else if(iLevel == "ga"){
          # create pads
          pads <- expand.grid(c(pads_prior, pads_after),
                              0) %>%
            stats::setNames(names(bosc$data[[iLevel]][[iType]]$data))

          # add pads to datasets
          bosc$data[[iLevel]][[iType]]$data <- bosc$data[[iLevel]][[iType]]$data %>%
            dplyr::ungroup() %>%
            dplyr::add_row(!!pads) %>%
            dplyr::arrange(.data$time)
        }
      } else if (iType == "surrogate") {

        if (iLevel == "ss") {
          # create pads
          pads <- expand.grid(unique(bosc$data[[iLevel]][[iType]]$data$subj),
                              unique(bosc$data[[iLevel]][[iType]]$data$n_surr),
                              c(pads_prior, pads_after),
                              0) %>%
            stats::setNames(names(bosc$data[[iLevel]][[iType]]$data))

          # add pads to datasets
          bosc$data[[iLevel]][[iType]]$data <- bosc$data[[iLevel]][[iType]]$data %>%
            dplyr::ungroup() %>%
            dplyr::add_row(!!pads) %>%
            dplyr::arrange(.data$subj, .data$n_surr, .data$time)
        }
        else if (iLevel == "ga") {
          # create pads
          pads <- expand.grid(unique(bosc$data[[iLevel]][[iType]]$data$n_surr),
                              c(pads_prior, pads_after),
                              0) %>%
            stats::setNames(names(bosc$data[[iLevel]][[iType]]$data))

          # add pads to datasets
          bosc$data[[iLevel]][[iType]]$data <- bosc$data[[iLevel]][[iType]]$data %>%
            dplyr::ungroup() %>%
            dplyr::add_row(!!pads) %>%
            dplyr::arrange(.data$n_surr, .data$time)
        }
      }

      # add preprocessing step to documentation
      if (is.null(bosc$data[[iLevel]][[iType]]$preprocessing)) {
        bosc$data[[iLevel]][[iType]]$preprocessing <- paste0("PADDED_METHOD:", method)
      } else {
        bosc$data[[iLevel]][[iType]]$preprocessing <- paste(bosc$data[[iLevel]][[iType]]$preprocessing, paste0("PADDED_METHOD:", method), sep = "_")
      }

    }
  }

  # add executed command to history
  bosc$hist <- paste0(bosc$hist, "padd_")

  message("Padding completed.")
  return(bosc)
}

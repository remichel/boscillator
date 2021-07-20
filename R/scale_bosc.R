#' scale_bosc
#'
#' @description \code{scale_bosc}
#' z-scales time courses in an BOSC-Object.
#'
#' @param bosc BOSC-Object
#' @param types Which data should be scaled? choose between "real" and "surrogate" or concatenate to "real-surrogate" to perform aggregation on both.
#' @param levels Which levels of data need to be scaled? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
#' @param method defaults to z
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
scale_bosc <- function(bosc, types = "real-surrogate", levels = "ss-ga", method = "z") {

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

  # get method
  if(method != "z"){
    stop("There is no other method than z-scaling implemented yet.")
  }


  message("Start Scaling...")

  # loop through all conditions
  for (iType in iType_list) {
    for (iLevel in iLevel_list) {

      message(paste("Scaling", iLevel, iType, "..."))

      # check if required data exists
      if(is.null(bosc$data[[iLevel]][[iType]]$data)){
        message(paste("No data found in ", iLevel, iType, ".\nWill continue with next iType/iLevel..."))
        next
      }

      # check whether padding was already applied for the condition at hand
      if(!is.null(bosc$data[[iLevel]][[iType]]$preprocessing)){
        if("SCALED" %in% split_string_arg(bosc$data[[iLevel]][[iType]]$preprocessing, "_")){
          reply = utils::menu(c("Yes", "No"), title = paste("Data in", iLevel, iType, "was already scaled. Are you sure you want to continue with yet another scaling?"))
          if(reply == 2){
            stop("Execution stopped.")
          }
        }
      }

      # scaling
      if (iType == "real") {

        if (iLevel == "ss") {

          bosc$data[[iLevel]][[iType]]$data <- bosc$data[[iLevel]][[iType]]$data %>%
            dplyr::group_by(.data$subj) %>%
            dplyr::mutate(hr = scale(.data$hr))

        }else if(iLevel == "ga"){

          bosc$data[[iLevel]][[iType]]$data <- bosc$data[[iLevel]][[iType]]$data %>%
            dplyr::mutate(hr = scale(.data$hr))

        }
      } else if (iType == "surrogate") {

        if (iLevel == "ss") {

          bosc$data[[iLevel]][[iType]]$data <- bosc$data[[iLevel]][[iType]]$data %>%
            dplyr::group_by(.data$subj, .data$n_surr) %>%
            dplyr::mutate(hr = scale(.data$hr))

        }
        else if (iLevel == "ga") {

          bosc$data[[iLevel]][[iType]]$data <- bosc$data[[iLevel]][[iType]]$data %>%
            dplyr::group_by(.data$n_surr) %>%
            dplyr::mutate(hr = scale(.data$hr))

        }
      }

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

  message("Scaling completed.")
  return(bosc)
}

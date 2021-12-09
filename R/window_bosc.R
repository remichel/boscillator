#' window_bosc
#'
#' @description \code{window_bosc}
#' Applies a window function to the time courses in an BOSC-Object.
#'
#' @param bosc BOSC-Object
#' @param types Which data should be windowed? choose between "real" and "surrogate" or concatenate to "real-surrogate" to perform aggregation on both.
#' @param levels Which levels of data need to be windowed? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
#' @param method Which window function should be used? defaults to "hann" for a hanning window
#' @param r Additional parameter for the tukey window function
#' @param alpha Additional parameter for the hamming, cosine and kaiser window function
#' @param verbose defaults to T
#'
#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export window_bosc
#' @name window_bosc
#'
#' @examples
#' bosc = simulate_experiment()
#' bosc = window_bosc(bosc, types = "real", levels = "ga", method = "hann")
#'
window_bosc <- function(bosc,
                        types = "real-surrogate",
                        levels = "ss-ga",
                        method = "hann",
                        r = .1,
                        alpha = .54,
                        verbose = T) {

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

  # check order
  if(!is.character(method)){
    stop("Method must be a character.")
  }

  if(verbose == T) message("Start windowing...")

  # loop through all conditions
  for (iType in iType_list) {
    for (iLevel in iLevel_list) {

      if(verbose == T) message(paste("Windowing", iLevel, iType, "..."))

      # check if required data exists
      if(is.null(bosc$data[[iLevel]][[iType]]$data)){
        if(verbose == T) message(paste("No data found in ", iLevel, iType, ".\nWill continue with next iType/iLevel..."))
        next
      }

      # check whether detrending was already applied for the condition at hand
      if(!is.null(bosc$data[[iLevel]][[iType]]$preprocessing)){
        if("WINDOWED" %in% split_string_arg(bosc$data[[iLevel]][[iType]]$preprocessing, "_")){
          reply = utils::menu(c("Yes", "No"), title = paste("Data in", iLevel, iType, "was already windowed. Are you sure you want to continue with yet another windowing?"))
          if(reply == 2){
            next
          }
        }
      }

      # determine window length
      if(length(bosc$timepoints) == length(unique(bosc$data[[iLevel]][[iType]]$data$time))){
        n = length(bosc$timepoints)
      }else{
        if(verbose == T) message("Number of timepoints in dataset seems to deviate from original number of timepoints, possibly due to padding. Continue with number the number of timepoints in dataset, which is ",
                                 length(unique(bosc$data[[iLevel]][[iType]]$data$time)), "...")
        n = length(unique(bosc$data[[iLevel]][[iType]]$data$time))
      }


      # define group vars for the following step
      group_vars = NULL
      # for single subject data, group by subject
      if(iLevel == "ss"){group_vars = c(group_vars, "subj")}
      # for surrogate data, group by n_surr
      if(iType == "surrogate"){group_vars = c(group_vars, "n_surr")}

      # windowing
      if(method != "none"){
        bosc$data[[iLevel]][[iType]]$data <- bosc$data[[iLevel]][[iType]]$data %>%
          dplyr::group_by_at(group_vars) %>%
          dplyr::mutate(hr = dplyr::case_when(
            !!method == "hann" ~ .data$hr * bspec::hannwindow(!!n),
            !!method == "hamm" ~ .data$hr * bspec::hammingwindow(!!n, !!alpha),
            !!method == "tukey" ~ .data$hr * bspec::tukeywindow(!!n, !!r),
            !!method == "triangle" ~ .data$hr * bspec::trianglewindow(!!n),
            !!method == "cosine" ~ .data$hr * bspec::cosinewindow(!!n, !!alpha),
            !!method == "kaiser" ~ .data$hr * bspec::kaiserwindow(!!n, !!alpha)
          ))
      }else{
        if(verbose == T) message("No window will be applied...")
      }


      # add preprocessing step to documentation
      if (is.null(bosc$data[[iLevel]][[iType]]$preprocessing)) {
        bosc$data[[iLevel]][[iType]]$preprocessing <- paste0("WINDOWED_METHOD:", method)
      } else {
        bosc$data[[iLevel]][[iType]]$preprocessing <- paste(bosc$data[[iLevel]][[iType]]$preprocessing, paste0("WINDOWED_METHOD:", method), sep = "_")
      }

    }
  }

  # add executed command to history
  bosc$hist <- paste0(bosc$hist, "window_")

  if(verbose == T) message("Windowing completed.")
  return(bosc)
}

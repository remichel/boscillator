#' windowing_bosc
#'
#' @description \code{windowing_bosc}
#' Applies a window function to the time courses in an BOSC-Object.
#'
#' @param bosc BOSC-Object
#' @param types Which data should be windowed? choose between "real" and "surrogate" or concatenate to "real-surrogate" to perform aggregation on both.
#' @param levels Which levels of data need to be windowed? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
#' @param method Which window function should be used? defaults to "hann" for a hanning window
#' @param r Additional parameter for the tukey window function
#' @param alpha Additional parameter for the hamming, cosine and kaiser window function
#'
#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export windowing_bosc
#' @name windowing_bosc
#'
#' @examples
#'
windowing_bosc <- function(bosc, types = "real-surrogate", levels = "ss-ga", method = "hann", r = .1, alpha = .54) {

  # get levels
  if(!is.character(levels)){
    stop("Argument levels must be a character.")
  }else{
    level_list <- split_string_arg(levels, "-")
  }

  # get types
  if(!is.character(types)){
    stop("Argument types must be a character.")
  }else{
    type_list <- split_string_arg(types, "-")
  }

  # check order
  if(!is.character(method)){
    stop("Method must be a character.")
  }

  message("Start windowing...")
  # loop through all conditions
  for (type in type_list) {
    for (level in level_list) {

      message(paste("Windowing", level, type, "..."))

      # check if required data exists
      if(is.null(bosc$data[[level]][[type]]$data)){
        message(paste("No data found in ", level, type, ".\nWill continue with next type/level..."))
        next
      }

      # check whether detrending was already applied for the condition at hand
      if(!is.null(bosc$data[[level]][[type]]$preprocessing)){
        if("WINDOWED" %in% split_string_arg(bosc$data[[level]][[type]]$preprocessing, "_")){
          reply = utils::menu(c("Yes", "No"), title = paste("Data in", level, type, "was already windowed. Are you sure you want to continue with yet another detrending?"))
          if(reply == 2){
            stop("Execution stopped.")
          }
        }
      }

      # determine window length
      if(length(bosc$timepoints) == length(unique(bosc$data[[level]][[type]]$data$time))){
        n = length(bosc$timepoints)
      }else{
        message("Number of timepoints in dataset seems to deviate from original number of timepoints, possibly due to padding. Continue with number the number of timepoints in dataset, which is ", length(unique(bosc$data[[level]][[type]]$data$time)), "...")
        n = length(unique(bosc$data[[level]][[type]]$data$time))
      }


      # detrend
      if (type == "real") {

        if (level == "ss") {
          bosc$data[[level]][[type]]$data <- bosc$data[[level]][[type]]$data %>%
            dplyr::group_by(.data$subj) %>%
            dplyr::mutate(hr = dplyr::case_when(
              !!method == "hann" ~ .data$hr * bspec::hannwindow(!!n),
              !!method == "hamm" ~ .data$hr * bspec::hammingwindow(!!n, !!alpha),
              !!method == "tukey" ~ .data$hr * bspec::tukeywindow(!!n, !!r),
              !!method == "triangle" ~ .data$hr * bspec::trianglewindow(!!n),
              !!method == "cosine" ~ .data$hr * bspec::cosinewindow(!!n, !!alpha),
              !!method == "kaiser" ~ .data$hr * bspec::kaiserwindow(!!n, !!alpha)
            ))
        } else if (level == "ga") {
          bosc$data[[level]][[type]]$data <- bosc$data[[level]][[type]]$data %>%
            dplyr::mutate(hr = dplyr::case_when(
              !!method == "hann" ~ .data$hr * bspec::hannwindow(!!n),
              !!method == "hamm" ~ .data$hr * bspec::hammingwindow(!!n, !!alpha),
              !!method == "tukey" ~ .data$hr * bspec::tukeywindow(!!n, !!r),
              !!method == "triangle" ~ .data$hr * bspec::trianglewindow(!!n),
              !!method == "cosine" ~ .data$hr * bspec::cosinewindow(!!n, !!alpha),
              !!method == "kaiser" ~ .data$hr * bspec::kaiserwindow(!!n, !!alpha)
            ))
        }
      } else if (type == "surrogate") {

        if (level == "ss") {
          bosc$data[[level]][[type]]$data <- bosc$data[[level]][[type]]$data %>%
            dplyr::group_by(.data$subj, .data$n_surr) %>%
            dplyr::mutate(hr = dplyr::case_when(
              !!method == "hann" ~ .data$hr * bspec::hannwindow(!!n),
              !!method == "hamm" ~ .data$hr * bspec::hammingwindow(!!n, !!alpha),
              !!method == "tukey" ~ .data$hr * bspec::tukeywindow(!!n, !!r),
              !!method == "triangle" ~ .data$hr * bspec::trianglewindow(!!n),
              !!method == "cosine" ~ .data$hr * bspec::cosinewindow(!!n, !!alpha),
              !!method == "kaiser" ~ .data$hr * bspec::kaiserwindow(!!n, !!alpha)
            ))
        }
        else if (level == "ga") {
          bosc$data[[level]][[type]]$data <- bosc$data[[level]][[type]]$data %>%
            dplyr::group_by(.data$n_surr) %>%
            dplyr::mutate(hr = dplyr::case_when(
              !!method == "hann" ~ .data$hr * bspec::hannwindow(!!n),
              !!method == "hamm" ~ .data$hr * bspec::hammingwindow(!!n, !!alpha),
              !!method == "tukey" ~ .data$hr * bspec::tukeywindow(!!n, !!r),
              !!method == "triangle" ~ .data$hr * bspec::trianglewindow(!!n),
              !!method == "cosine" ~ .data$hr * bspec::cosinewindow(!!n, !!alpha),
              !!method == "kaiser" ~ .data$hr * bspec::kaiserwindow(!!n, !!alpha)
            ))
        }
      }

      # add preprocessing step to documentation
      if (is.null(bosc$data[[level]][[type]]$preprocessing)) {
        bosc$data[[level]][[type]]$preprocessing <- paste0("WINDOWED_METHOD:", method)
      } else {
        bosc$data[[level]][[type]]$preprocessing <- paste(bosc$data[[level]][[type]]$preprocessing, paste0("WINDOWED_METHOD:", method), sep = "_")
      }

    }
  }

  # add executed command to history
  bosc$hist <- paste0(bosc$hist, "window_")

  message("Windowing completed.")
  return(bosc)
}

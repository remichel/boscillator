#' fft_bosc
#'
#' @description \code{fft_bosc}
#' Performs an FFT on a time courses in an BOSC-Object.
#'
#' @param bosc BOSC-Object
#' @param types Which data should be padded? choose between "real" and "surrogate" or concatenate to "real-surrogate" to perform aggregation on both.
#' @param levels Which levels of data need to be padded? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
#' @param overwrite defaults to F
#'
#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export fft_bosc
#' @name fft_bosc
#'
#' @examples
#' bosc = simulate_experiment()
#' bosc = fft_bosc(bosc, types = "real", levels = "ga")
#'
fft_bosc <- function(bosc, types = "real-surrogate", levels = "ss-ga", overwrite = FALSE) {

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


  # loop through all conditions to check whether time series all have the same length
  len = matrix(NA, length(iType_list), length(iLevel_list))
  for (iType in 1:length(iType_list)) {
    for (iLevel in 1:length(iLevel_list)) {
      # get length of time series
      len[iLevel, iType] = length(unique(bosc$data[[iLevel_list[iLevel]]][[iType_list[iType]]]$data$time))
    }
  }
  # if all conditions have same length, then determine variables relevant for FFT
  if(length(unique(as.vector(len))) == 1){
    # get length of time series
    len = unique(as.vector(len))
    # get sampling frequency
    sfreq = bosc$data$single_trial$real$spec$sfreq
    # determine frequency resolution
    fres = sfreq/len
    # determine nyquist
    nyquist = sfreq/2
    # frequency bins
    fbins = seq(fres, nyquist, fres)
  }else{
    stop("Datasets differ in length of the time series. Probably padding was applied only to a subset of datasets.")
  }


  message("Start FFT...")

  # loop through all conditions
  for (iType in iType_list) {
    for (iLevel in iLevel_list) {

      message(paste("FFTing", iLevel, iType, "..."))

      # check if required data exists
      if(is.null(bosc$data[[iLevel]][[iType]]$data)){
        message(paste("No data found in ", iLevel, iType, ".\nWill continue with next iType/iLevel..."))
        next
      }

      # check if FFT data exists
      if(!is.null(bosc$data[[iLevel]][[iType]]$fft)){
        if(overwrite == TRUE){
          message("FFT Data already exists. Will overwrite...")
        }else{
          message("FFT Data already exists. Will skip to next dataset without performing the FFT...")
          next
        }
      }

      # define group vars
      # define group vars
      if (iType == "real") {
        if(iLevel == "ss"){
          group_vars = dplyr::sym("subj")
        }else{
          group_vars = dplyr::syms(NULL)
        }
      }else if(iType == "surrogate"){
        if(iLevel == "ss"){
          group_vars = dplyr::syms(c("subj", "n_surr"))
        }else{
          group_vars = dplyr::sym("n_surr")
        }
      }

      # FFT
      bosc$data[[iLevel]][[iType]]$fft = bosc$data[[iLevel]][[iType]]$data %>%
        dplyr::group_by(!!!group_vars) %>%
        dplyr::summarize(complex = stats::fft(.data$hr)[2:(length(fbins)+1)]) %>%
        dplyr::mutate(amp = Mod(.data$complex)) %>%
        dplyr::mutate(phase = Arg(.data$complex)) %>%
        dplyr::mutate(f = !!fbins) %>%
        dplyr::relocate(.data$f, .before = .data$complex)

    }
  }

  # add executed command to history
  bosc$hist <- paste0(bosc$hist, "fft_")

  message("FFT completed.")
  return(bosc)
}

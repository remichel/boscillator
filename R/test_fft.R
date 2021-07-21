#' test_fft
#'
#' @description \code{test_fft}
#' Performs an FFT on a time courses in an BOSC-Object.
#'
#' @param bosc BOSC-Object
#' @param levels Which levels of data need to be padded? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
#' @param test  Which test to perform?
#' @param alpha Which alpha level to apply?
#' @param overwrite defaults to F
#'
#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export test_fft
#' @name test_fft
#'
#' @examples
#' bosc = simulate_experiment()
#' bosc = generate_surrogates(bosc)
#' bosc = fft_bosc(bosc)
#' bosc = test_fft(bosc, levels = "ga")
#'
test_fft <- function(bosc, levels = "ss-ga", test = "amp", alpha = .05, overwrite = FALSE) {

  # get levels
  if(!is.character(levels)){
    stop("Argument levels must be a character.")
  }else{
    iLevel_list <- split_string_arg(levels, "-")
  }

  # loop through all conditions

  for (iLevel in iLevel_list) {


    message(paste("Test FFT on", iLevel, "..."))

    # check if required data exists
    if(is.null(bosc$data[[iLevel]]$real$fft) | is.null(bosc$data[[iLevel]]$surrogate$fft)){
      message(paste("No data found in ", iLevel, ".\nWill continue with next iType/iLevel..."))
      next
    }

    # check if test data exists
    if(!is.null(bosc$test[[iLevel]])){
      if(overwrite == TRUE){
        message("Test already exists. Will overwrite...")
      }else{
        warning("Test already exists. Will skip to next dataset without performing the FFT...")
        next
      }
    }


    # get correct group vars depending on the analysis level
    if (iLevel == "ss") {
      group_vars = dplyr::syms(c("subj", "f"))
    }else{
      group_vars = dplyr::syms("f")
    }

    # Perform test
    bosc$tests$fft[[iLevel]] = bosc$data[[iLevel]]$surrogate$fft %>%
      dplyr::group_by(.data$n_surr) %>%
      dplyr::mutate(observed =  !!bosc$data[[iLevel]]$real$fft$amp) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(!!!group_vars) %>%
      dplyr::summarize(crit_value = stats::quantile(.data$amp, probs = 1-!!alpha),
                       p = 1-stats::ecdf(.data$amp)(.data$observed),
                       observed = .data$observed) %>%
      dplyr::distinct() %>%
      dplyr::mutate(alpha = !!alpha) %>%
      dplyr::relocate(.data$alpha, .after = .data$f) %>%
      dplyr::mutate(sig = dplyr::case_when(.data$observed > .data$crit_value ~ 1,
                                           .data$observed <= .data$crit_value ~ 0))


  }

  # add executed command to history
  bosc$hist <- paste0(bosc$hist, "test_")

  message("Test completed.")
  return(bosc)
}

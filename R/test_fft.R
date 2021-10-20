#' test_fft
#'
#' @description \code{test_fft}
#' Performs statistical test on frequency spectra obtained from fft_bosc.
#'
#' @param bosc BOSC-Object
#' @param levels Which levels of data need to be padded? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
#' @param tests  Which test to perform?
#' @param alpha_amp Which alpha level to apply for amplitude test
#' @param alpha_complex Which alpha level to apply for complex plane test
#' @param alpha_phase Which alpha level to apply for rayleigh test
#' @param overwrite defaults to F
#' @param verbose defaults to T
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
#' bosc = test_fft(bosc, levels = "ga", tests = "amp-complex")
#'
test_fft <- function(bosc, levels = "ss-merged_spectra-ga", tests = "amp-complex-phase", alpha_amp = .05, alpha_complex = alpha_amp, alpha_phase = alpha_amp, overwrite = FALSE, verbose = T) {

  # get levels
  if(!is.character(levels)){
    stop("Argument levels must be a character.")
  }else{
    iLevel_list <- split_string_arg(levels, "-")
  }

  # get tests
  if(!is.character(tests)){
    stop("Argument levels must be a character.")
  }else{
    iTest_list <- split_string_arg(tests, "-")
  }

  if(verbose == T) message("Starting tests...")


  # loop through all conditions
  for (iLevel in iLevel_list) {

    if(verbose == T) message("\nTest on ", iLevel, ' level...\n')

    for(iTest in iTest_list){


      # check if required data exists
      if(is.null(bosc$data[[iLevel]]$real$fft) | is.null(bosc$data[[iLevel]]$surrogate$fft)){
        if(verbose == T) message(paste("No data found in ", iLevel, ".\nWill continue with next iType/iLevel..."))
        next
      }

      # check if test data exists
      if(!is.null(bosc$tests$fft[[iLevel]][[iTest]])){
        if(overwrite == TRUE){
          if(verbose == T) message("Test already exists. Will overwrite...")
        }else{
          warning("Test already exists. Will skip to next dataset without performing the FFT...")
          next
        }
      }



      # Perform test
      if(iTest == "amp"){

        # get correct group vars depending on the analysis level
        if (iLevel == "ss") {
          group_vars = dplyr::syms(c("subj", "f"))
        }else if(iLevel == "ga" | iLevel == "merged_spectra"){
          group_vars = dplyr::syms("f")
        }

        if(verbose == T) message("Comparing Observed FFT Amplitudes against Permutations (alpha = ", alpha_amp,")...")

        bosc$tests$fft[[iLevel]][[iTest]]$results = bosc$data[[iLevel]]$surrogate$fft %>%
          dplyr::group_by(.data$n_surr) %>%
          dplyr::mutate(observed =  !!bosc$data[[iLevel]]$real$fft$amp) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(!!!group_vars) %>%
          dplyr::summarize(crit_value = unname(stats::quantile(.data$amp, probs = 1-!!alpha_amp, na.rm = T)), # is na.rm = T causing any harm here??
                           p = 1-stats::ecdf(.data$amp)(.data$observed),
                           observed = .data$observed) %>%
          dplyr::distinct() %>%
          dplyr::mutate(alpha = !!alpha_amp) %>%
          dplyr::relocate(.data$alpha, .after = .data$f) %>%
          dplyr::mutate(sig = dplyr::case_when(.data$observed > .data$crit_value ~ 1,
                                               .data$observed <= .data$crit_value ~ 0))



      }else if(iTest == "complex"){

        group_vars = dplyr::syms(c("subj", "f"))

        if(iLevel == "ga" | iLevel == "merged_spectra"){
          if(verbose == T) message("Note: Complex vector analysis needs to be performed on single subject data. Will skip and proceed with next test...")
          next
        }else if(iLevel == "ss"){
          if(verbose == T) message("Compare FFT complex vectors against permutations (alpha = ", alpha_complex,") ...")
        }

        # save single surrogate data for 2d plot
        bosc$tests$fft[[iLevel]][[iTest]]$data = bosc$data[[iLevel]]$surrogate$fft %>%
          dplyr::group_by(.data$n_surr) %>%
          dplyr::mutate(observed = !!bosc$data[[iLevel]]$real$fft$complex) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(.data$n_surr, .data$f) %>%
          dplyr::summarize(observed = mean(.data$observed),
                           surrogate = mean(.data$complex)) %>%
          dplyr::mutate(observed_length = Mod(.data$observed),
                        surrogate_length = Mod(.data$surrogate),
                        observed_phase = Arg(.data$observed),
                        surrogate_phase = Arg(.data$surrogate)) %>%
          dplyr::ungroup()

        # determine critical vector length for each frequency
        bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$tests$fft[[iLevel]][[iTest]]$data %>%
          dplyr::group_by(.data$f) %>%
          dplyr::summarize(crit_length = unname(stats::quantile(.data$surrogate_length, probs = 1-!!alpha_complex)),
                           p = 1-stats::ecdf(.data$surrogate_length)(.data$observed_length),
                           observed_length = .data$observed_length) %>%
          dplyr::distinct() %>%
          dplyr::mutate(alpha = !!alpha_complex) %>%
          dplyr::relocate(.data$alpha, .after = .data$f) %>%
          dplyr::mutate(sig = dplyr::case_when(.data$observed_length > .data$crit_length ~ 1,
                                               .data$observed_length <= .data$crit_length ~ 0))

      }else if(iTest == "phase"){

        # skip grand average level
        if(iLevel == "ga" | iLevel == "merged_spectra"){
          if(verbose == T) message("Note: Phase analysis needs to be performed on single subject data. Will skip and proceed with next test...")
          next
        }

        if(verbose == T) message("Rayleigh test on phase angles (alpha = ", alpha_phase,")...")

        # rayleigh test
        bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$data$ss$real$fft %>%
          dplyr::group_by(.data$f) %>%
          dplyr::summarize(mean_phase = circular::mean.circular(circular::circular(.data$phase, units = c("radians"))),
                           statistic = circular::rayleigh.test(circular::circular(.data$phase, units = c("radians")), mu = NULL)$statistic,
                           p = circular::rayleigh.test(circular::circular(.data$phase, units = c("radians")), mu = NULL)$p.value,
                           alpha = !!alpha_phase) %>%
          dplyr::mutate(sig = dplyr::case_when(.data$p < .data$alpha ~ 1,
                                        .data$p >= .data$alpha ~ 0))

      }


    }
  }

  # add executed command to history
  bosc$hist <- paste0(bosc$hist, "test_")

  if(verbose == T) message("\nTest completed.")
  return(bosc)
}

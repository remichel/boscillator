#' sinmod_bosc
#'
#' @description \code{sinmod_bosc}
#' Fits sinusoidal model to a time courses in an BOSC-Object using nls.multstart. If not specified otherwise in fixed_f, only the winner model according to nls.multstart (across all frequencies tested by the algorithm) will be reported.
#'
#' @param bosc BOSC-Object
#' @param types Which data should be modelled? choose between "real" and "surrogate" or concatenate to "real-surrogate" to perform aggregation on both.
#' @param levels Which levels of data need to be modelled? Use "ss" for single subject and "ga" for grand average. Multiple arguments need to be passed as a vector.
#' @param fixed_f NULL to test all possible frequencies between 0 and nyquist, or specify a vector of frequencies
#' @param niter number of iterations for fitting, is piped into iter argument in nls.multstart function
#' @param overwrite defaults to F
#' @param verbose defaults to T
#'
#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export sinmod_bosc
#' @name sinmod_bosc
#'
#' @examples
#' bosc = simulate_experiment()
#' bosc = sinmod_bosc(bosc, types = "real", levels = "ga")
#'
sinmod_bosc <- function(bosc, types = c("real", "surrogate"), levels = c("ss", "ga"), fixed_f = NULL, niter = 100, overwrite = FALSE, verbose = T) {

  # get levels
  if(!is.character(levels)){
    stop("Argument levels must be a character.")
  }

  # get types
  if(!is.character(types)){
    stop("Argument types must be a character.")
  }


  # loop through all conditions to check whether time series all have the same length
  len = matrix(NA, length(types), length(levels))
  for (iType in 1:length(types)) {
    for (iLevel in 1:length(levels)) {
      # get length of time series
      len[iType, iLevel] = length(unique(bosc$data[[levels[iLevel]]][[types[iType]]]$data$time))
    }
  }

  # if all conditions have same length, then determine variables relevant for sinmod
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


  if(verbose == T) message("Start sinusoidal modelling...")

  # loop through all conditions
  for (iType in types) {
    for (iLevel in levels) {

      if(verbose == T) message(paste("Modelling", iLevel, iType, "..."))

      # check if required data exists
      if(is.null(bosc$data[[iLevel]][[iType]]$data)){
        if(verbose == T) message(paste("No data found in ", iLevel, iType, ".\nWill continue with next iType/iLevel..."))
        next
      }

      # check if sinmod data exists
      if(!is.null(bosc$data[[iLevel]][[iType]]$sinmod)){
        if(overwrite == TRUE){
          if(verbose == T) message("Sinusoidal modelling Data already exists. Will overwrite...")
        }else{
          if(verbose == T) message("Sinusoidal modelling Data already exists. Will skip to next dataset without performing the modelling...")
          next
        }
      }


      # sinmod
      sinModel <- function(t, intercept, a, f, phi) {
        intercept + a * sin(2 * pi * t * f + phi)
      }

      if(is.null(fixed_f)){


        # define group vars for the following step
        group_vars = NULL
        # for single subject data, group by subject
        if(iLevel == "ss"){group_vars = c(group_vars, "subj")}
        # for surrogate data, group by n_surr
        if(iType == "surrogate"){group_vars = c(group_vars, "n_surr")}

        # define lower bounds of fitting procedure (those values were chosen for HR, i.e. for values ranging from 0 to 1)
        # maybe in later version, allow lower / upper to be passed as argument by user
        lower = c(intercept = 0, a = 0, f = fres, phi = 0)
        upper = c(intercept = 1, a = 1, f = nyquist, phi = 2 * pi)

        # fit sinmod for every group_var
          bosc$data[[iLevel]][[iType]]$sinmod <- bosc$data[[iLevel]][[iType]]$data %>%
            dplyr::group_by_at(group_vars) %>%
            tidyr::nest() %>%
            dplyr::mutate(fit = purrr::map(.data$data, ~ nls.multstart::nls_multstart(hr ~ sinModel(time, intercept, a, f, phi),
                                                                                data = .x,
                                                                                lower = lower,
                                                                                upper = upper,
                                                                                start_lower = lower,
                                                                                start_upper = upper,
                                                                                iter = niter,
                                                                                supp_errors = "Y")),
                          estimates = purrr::map(.data$fit, broom::tidy), # store all estimates here
                          predictions = purrr::map(.data$fit, broom::augment),
                          gof = purrr::map(.data$fit, broom::glance),
                          f = purrr::map(.data$fit, broom::tidy), # to unnest it later and extract only f
                          r2 = unlist(purrr::map(.x = .data$fit, .y = .data$data, ~ modelr::rsquare(model = .x, data = .y)))) %>%
            tidyr::unnest(cols = c(.data$f)) %>%
            dplyr::select(-c(.data$data, .data$std.error, .data$statistic, .data$p.value)) %>%
            dplyr::filter(.data$term == "f")


      }else{

        # define group vars for the following step
        group_vars = "fixed_f"
        # for single subject data, group by subject
        if(iLevel == "ss"){group_vars = c(group_vars, "subj")}
        # for surrogate data, group by n_surr
        if(iType == "surrogate"){group_vars = c(group_vars, "n_surr")}


        # fixed frequencies dataframe to join it with dataset
        freqs = as.data.frame(fixed_f)
        freqs$helper = 1


        # define lower bounds of fitting procedure (those values were chosen for HR, i.e. for values ranging from 0 to 1)
        # maybe in later version, allow lower / upper to be passed as argument by user
        lower = c(intercept = 0, a = 0, phi = 0)
        upper = c(intercept = 1, a = 1, phi = 2 * pi)

        options(dplyr.nest.inform = FALSE)
        # fit sinmod for every fixed frequency
        bosc$data[[iLevel]][[iType]]$sinmod <- bosc$data[[iLevel]][[iType]]$data %>%
          dplyr::mutate(helper = 1) %>%
          dplyr::full_join(y = freqs, by = .data$helper) %>%
          dplyr::select(-.data$helper) %>%
          dplyr::mutate(f = .data$fixed_f) %>%
          dplyr::group_by_at(group_vars) %>%
          tidyr::nest() %>%
          dplyr::mutate(fit = purrr::map(.data$data, ~ nls.multstart::nls_multstart(hr ~ sinModel(time, intercept, a, f, phi),
                                                                              data = .x,
                                                                              lower = lower,
                                                                              upper = upper,
                                                                              start_lower = lower,
                                                                              start_upper = upper,
                                                                              iter = niter,
                                                                              supp_errors = "Y")),
                        estimates = purrr::map(.data$fit, broom::tidy),
                        gof = purrr::map(.data$fit, broom::glance),
                        r2 = unlist(purrr::map(.x = .data$fit, .y = .data$data, ~ modelr::rsquare(model = .x, data = .y)))) %>%
          tidyr::unnest(cols = c(.data$estimates, .data$gof)) %>%
          dplyr::select(-c(.data$data, .data$fit))

      }


      }

    }

  # add executed command to history
  bosc$hist <- paste0(bosc$hist, "sinmod_")

  if(verbose == T) message("Sinusoidal modelling completed.")
  return(bosc)
}

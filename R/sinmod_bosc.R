#' sinmod_bosc
#'
#' @description \code{sinmod_bosc}
#' Fits sinusoidal model to a time courses in an BOSC-Object using nls.multstart.
#'
#' @param bosc BOSC-Object
#' @param types Which data should be modelled? choose between "real" and "surrogate" or concatenate to "real-surrogate" to perform aggregation on both.
#' @param levels Which levels of data need to be modelled? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
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
sinmod_bosc <- function(bosc, types = "real-surrogate", levels = "ss-ga", fixed_f = NULL, niter = 500, overwrite = FALSE, verbose = T) {

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
      len[iType, iLevel] = length(unique(bosc$data[[iLevel_list[iLevel]]][[iType_list[iType]]]$data$time))
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
  for (iType in iType_list) {
    for (iLevel in iLevel_list) {

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


        # fit sinmod for every group_var
        if(length(group_vars) > 0){
          bosc$data[[iLevel]][[iType]]$sinmod = bosc$data[[iLevel]][[iType]]$data %>%
            dplyr::group_by(!!!group_vars) %>%
            tidyr::nest() %>%
            dplyr::mutate(fit = purrr::map(.data$data, ~ nls.multstart::nls_multstart(hr ~ sinModel(time, intercept, a, f, phi),
                                                                                data = .x,
                                                                                lower = c(intercept = -Inf, a = 0, f = fres, phi = 0),
                                                                                upper = c(intercept = Inf, a = Inf, f = nyquist, phi = 2 * pi),
                                                                                start_lower = c(intercept = .2, a = .2, f = fbins[floor(length(fbins)/4)], phi = 1 / 2 * pi),
                                                                                start_upper = c(intercept = .8, a = .8, f = floor(3*length(fbins)/4), phi = 3 / 2 * pi),
                                                                                iter = niter,
                                                                                supp_errors = "Y")),
                          estimates = purrr::map(.data$fit, broom::tidy),
                          gof = purrr::map(.data$fit, broom::glance),
                          r2 = purrr::map(.x = .data$fit, .y = .data$data, ~ modelr::rsquare(model = .x, data = .y))) %>%
            tidyr::unnest(cols = c(.data$estimates, .data$gof)) %>%
            dplyr::select(-c(.data$data, .data$fit))
      }else{
        bosc$data[[iLevel]][[iType]]$sinmod = bosc$data[[iLevel]][[iType]]$data %>%
          dplyr::group_by(!!!group_vars) %>%
          tidyr::nest(data = dplyr::everything()) %>%
          dplyr::mutate(fit = purrr::map(.data$data, ~ nls.multstart::nls_multstart(hr ~ sinModel(time, intercept, a, f, phi),
                                                                              data = .x,
                                                                              lower = c(intercept = -Inf, a = 0, f = fres, phi = 0),
                                                                              upper = c(intercept = Inf, a = Inf, f = nyquist, phi = 2 * pi),
                                                                              start_lower = c(intercept = .2, a = .2, f = fbins[floor(length(fbins)/4)], phi = 1 / 2 * pi),
                                                                              start_upper = c(intercept = .8, a = .8, f = floor(3*length(fbins)/4), phi = 3 / 2 * pi),
                                                                              iter = niter,
                                                                              supp_errors = "Y")),
                        estimates = purrr::map(.data$fit, broom::tidy),
                        gof = purrr::map(.data$fit, broom::glance),
                        r2 = purrr::map(.x = .data$fit, .y = .data$data, ~ modelr::rsquare(model = .x, data = .y))) %>%
          tidyr::unnest(cols = c(.data$estimates, .data$gof)) %>%
          dplyr::select(-c(.data$data, .data$fit))
        }

      }else{

        # define group vars
        if (iType == "real") {
          if(iLevel == "ss"){
            group_vars = dplyr::syms(c("subj", "fixed_f"))
          }else{
            group_vars = dplyr::sym("fixed_f")
          }
        }else if(iType == "surrogate"){
          if(iLevel == "ss"){
            group_vars = dplyr::syms(c("subj", "n_surr", "fixed_f"))
          }else{
            group_vars = dplyr::syms(c("n_surr", "fixed_f"))
          }
        }

        # fixed frequencies dataframe to join it with dataset
        freqs = as.data.frame(fixed_f)
        freqs$helper = 1

        options(dplyr.nest.inform = FALSE)
        # fit sinmod for every fixed frequency
        bosc$data[[iLevel]][[iType]]$sinmod <- bosc$data[[iLevel]][[iType]]$data %>%
          dplyr::mutate(helper = 1) %>%
          dplyr::full_join(y = freqs, by = .data$helper) %>%
          dplyr::select(-.data$helper) %>%
          dplyr::mutate(f = .data$fixed_f) %>%
          dplyr::group_by(!!!group_vars) %>%
          tidyr::nest() %>%
          dplyr::mutate(fit = purrr::map(.data$data, ~ nls.multstart::nls_multstart(hr ~ sinModel(time, intercept, a, f, phi),
                                                                              data = .x,
                                                                              lower = c(intercept = -Inf, a = 0, phi = 0),
                                                                              upper = c(intercept = Inf, a = Inf, phi = 2 * pi),
                                                                              start_lower = c(intercept = .2, a = .2, phi = 1 / 2 * pi),
                                                                              start_upper = c(intercept = .8, a = .8, phi = 3 / 2 * pi),
                                                                              iter = niter,
                                                                              supp_errors = "Y")),
                        estimates = purrr::map(.data$fit, broom::tidy),
                        gof = purrr::map(.data$fit, broom::glance),
                        r2 = purrr::map(.x = .data$fit, .y = .data$data, ~ modelr::rsquare(model = .x, data = .y))) %>%
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

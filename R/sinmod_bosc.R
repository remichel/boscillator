#' sinmod_bosc
#'
#' @description \code{sinmod_bosc}
#' Fits sinusoidal model to a time courses in an BOSC-Object using nls.multstart. If not specified otherwise in fixed_f, only the winner model according to nls.multstart (across all frequencies tested by the algorithm) will be reported.
#'
#' @param bosc BOSC-Object
#' @param types Which data should be modelled? choose between "real" and "surrogate" or concatenate to "real-surrogate" to perform aggregation on both.
#' @param levels Which levels of data need to be modelled? Use "ss" for single subject and "ga" for grand average. Multiple arguments need to be passed as a vector.
#' @param fixed_f NULL to test all possible frequencies between 0 and nyquist, or specify a vector of frequencies
#' @param niter number of iterations for fitting, is piped into iter argument in nls.multstart function if NULL, a grid search approach will be used
#' @param grid  number of start values per parameter, will be equally distributed within parameter boundaries, e.g. grid = 5 will create a 5x5x5x... grid, argument will be ignored if iter != NULL, if grid = NULL and iter = NULL, the grid will automatically be generated with grid = ceiling((nyquist-fres)/2)
#' @param lower lower boundaries for model estimates. if NULL is chosen for f, it will automatically be set to the lowest possible frequency given the frequency resolution in the dataset
#' @param upper upper boundaries for model estimates. if NULL is chosen for f, it will automatically be set to the nyquist frequency in the dataset
#' @param overwrite defaults to F
#' @param verbose defaults to T
#' @param convergence_count number of iterations until model is declared winner model, is piped into iter argument in nls.multstart function, will be ignored if grid search approach is used
#' @param supp_errors defaults to 'Y', suppresses error messages from nls.multstart?
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
sinmod_bosc <- function(bosc,
                        types = c("real", "surrogate"),
                        levels = c("ss", "ga"),
                        fixed_f = NULL,
                        niter = NULL,
                        grid = 3,
                        convergence_count = FALSE,
                        supp_errors = "Y",
                        lower = list(intercept = 0, a = 0, f = NULL, phi = 0),
                        upper = list(intercept = 1, a = 1, f = NULL, phi = 2 * pi),
                        overwrite = FALSE,
                        verbose = TRUE) {

  # get levels
  if (!is.character(levels)) {
    stop("Argument levels must be a character.")
  }

  # get types
  if (!is.character(types)) {
    stop("Argument types must be a character.")
  }


  # loop through all conditions to check whether time series all have the same length
  len <- matrix(NA, length(types), length(levels))
  for (iType in 1:length(types)) {
    for (iLevel in 1:length(levels)) {
      # get length of time series
      len[iType, iLevel] <- length(unique(bosc$data[[levels[iLevel]]][[types[iType]]]$data$time))
    }
  }

  # if all conditions have same length, then determine variables relevant for sinmod
  if (length(unique(as.vector(len))) == 1) {
    # get length of time series
    len <- unique(as.vector(len))
    # get sampling frequency
    sfreq <- bosc$data$single_trial$real$spec$sfreq
    # determine frequency resolution
    fres <- sfreq / len
    # determine nyquist
    nyquist <- sfreq / 2
    # frequency bins
    fbins <- seq(fres, nyquist, fres)
  } else {
    stop("Datasets differ in length of the time series. Probably padding was applied only to a subset of datasets.")
  }


  if (verbose == TRUE) message("Start sinusoidal modelling...")

  # loop through all conditions
  for (iType in types) {
    for (iLevel in levels) {
      if (verbose == TRUE) message(paste("Modelling", iLevel, iType, "..."))

      # check if required data exists
      if (is.null(bosc$data[[iLevel]][[iType]]$data)) {
        if (verbose == TRUE) message(paste("No data found in ", iLevel, iType, ".\nWill continue with next iType/iLevel..."))
        next
      }

      # check if sinmod data exists
      if (!is.null(bosc$data[[iLevel]][[iType]]$sinmod)) {
        if (overwrite == TRUE) {
          if (verbose == TRUE) message("Sinusoidal modelling Data already exists. Will overwrite...")
        } else {
          if (verbose == TRUE) message("Sinusoidal modelling Data already exists. Will skip to next dataset without performing the modelling...")
          next
        }
      }


      # sinmod
      sinModel <- function(t, intercept, a, f, phi) {
        intercept + a * sin(2 * pi * t * f + phi)
      }

      if (is.null(fixed_f)) {


        # define group vars for the following step
        group_vars <- NULL
        # for single subject data, group by subject
        if (iLevel == "ss") {
          group_vars <- c(group_vars, "subj")
        }
        # for surrogate data, group by n_surr
        if (iType == "surrogate") {
          group_vars <- c(group_vars, "n_surr")
        }

        # define lower and upper bounds of fitting procedure
        if(is.null(lower$f)){lower$f <- fres}
        if(is.null(upper$f)){upper$f <- nyquist}

        #get number of parameters for grid search specification
        nparams <- length(lower)

        # which search approach will be used?
        if(!is.null(niter)){
          message("Random search approach using ", niter, " combinations of starting parameters will be used.")
          iter <- niter
        }else{

          # if grid is NULL, automatically create a grid based on current data
          if(is.null(grid)){
            grid <- ceiling((nyquist-fres)/2)
          }

          # get number of parameters for grid search specification
          nparams <- length(lower)

          # initialize grid
          iter <- rep(grid, nparams)


          message("Grid search approach using a ", grid, "x", nparams, " grid of starting parameters will be used.")
        }



        # fit sinmod for every group_var
        bosc$data[[iLevel]][[iType]]$sinmod <- bosc$data[[iLevel]][[iType]]$data %>%
          dplyr::group_by_at(group_vars) %>%
          tidyr::nest() %>%
          dplyr::mutate(
            fit = purrr::map(.data$data, ~ nls.multstart::nls_multstart(hr ~ sinModel(time, intercept, a, f, phi),
              data = .x,
              lower = c(intercept = lower$intercept, a = lower$a, f = lower$f, phi = lower$phi),
              upper = c(intercept = upper$intercept, a = upper$a, f = upper$f, phi = upper$phi),
              start_lower = c(intercept = lower$intercept, a = lower$a, f = lower$f, phi = lower$phi),
              start_upper = c(intercept = upper$intercept, a = upper$a, f = upper$f, phi = upper$phi),
              iter = iter,
              convergence_count = convergence_count,
              supp_errors = supp_errors
            )),
            estimates = purrr::map(.data$fit, broom::tidy), # store all estimates here
            predictions = purrr::map(.data$fit, broom::augment),
            gof = purrr::map(.data$fit, broom::glance),
            f = purrr::map(.data$fit, broom::tidy), # to unnest it later and extract only f
            r2 = unlist(purrr::map(.x = .data$fit, .y = .data$data, ~ modelr::rsquare(model = .x, data = .y)))
          ) %>%
          tidyr::unnest(cols = c(.data$f)) %>%
          dplyr::select(-c(.data$data, .data$std.error, .data$statistic, .data$p.value)) %>%
          dplyr::filter(.data$term == "f")
      } else {

        # define group vars for the following step
        group_vars <- "fixed_f"
        # for single subject data, group by subject
        if (iLevel == "ss") {
          group_vars <- c(group_vars, "subj")
        }
        # for surrogate data, group by n_surr
        if (iType == "surrogate") {
          group_vars <- c(group_vars, "n_surr")
        }


        # fixed frequencies dataframe to join it with dataset
        freqs <- as.data.frame(fixed_f)
        freqs$helper <- 1


        # eliminate f from boundary lists, as it is now fixed
        lower$f <- NULL
        upper$f <- NULL


        #get number of parameters for grid search specification
        nparams <- length(lower)

        # which search approach will be used?
        if(!is.null(niter)){
          message("Random search approach using ", niter, " combinations of starting parameters will be used.")
        }else{

          if(is.null(grid)){
            grid <- ceiling((nyquist-fres)/2)
          }
          message("Grid search approach using a ", grid, "x", nparams, " grid of starting parameters will be used.")
        }


        options(dplyr.nest.inform = FALSE)
        # fit sinmod for every fixed frequency
        bosc$data[[iLevel]][[iType]]$sinmod <- bosc$data[[iLevel]][[iType]]$data %>%
          dplyr::mutate(helper = 1) %>%
          dplyr::full_join(y = freqs, by = .data$helper) %>%
          dplyr::select(-.data$helper) %>%
          dplyr::mutate(f = .data$fixed_f) %>%
          dplyr::group_by_at(group_vars) %>%
          tidyr::nest() %>%
          dplyr::mutate(
            fit = purrr::map(.data$data, ~ nls.multstart::nls_multstart(hr ~ sinModel(time, intercept, a, f, phi),
              data = .x,
              lower = c(lower$intercept, lower$a, lower$phi),
              upper = c(upper$intercept, upper$a, upper$phi),
              start_lower = c(lower$intercept, lower$a, lower$phi),
              start_upper = c(upper$intercept, upper$a, upper$phi),
              iter = ifelse(is.null(niter), rep(grid, nparams), niter),
              convergence_count = convergence_count,
              supp_errors = supp_errors
            )),
            estimates = purrr::map(.data$fit, broom::tidy),
            gof = purrr::map(.data$fit, broom::glance),
            r2 = unlist(purrr::map(.x = .data$fit, .y = .data$data, ~ modelr::rsquare(model = .x, data = .y)))
          ) %>%
          tidyr::unnest(cols = c(.data$estimates, .data$gof)) %>%
          dplyr::select(-c(.data$data, .data$fit))
      }
    }
  }

  # add executed command to history
  bosc$hist <- paste0(bosc$hist, "sinmod_")

  if (verbose == TRUE) message("Sinusoidal modelling completed.")
  return(bosc)
}

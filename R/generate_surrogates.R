#' generate_surrogates
#'
#' @description \code{generate_surrogates}
#' Generates surrogate datasets from a dense sampling study
#'
#' @param bosc BOSC-Object
#' @param n_surr Number of to-be-created surrogate datasets
#' @param method permutation ("perm") or autoregression-model ("ar")
#' @param seed_num number of the seed
#' @param overwrite overwrite existing surrogates? defaults to F
#' @param aggregate aggregate surrogated datasets? defaults to T
#'
#' @return A list
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export generate_surrogates
#' @name generate_surrogates
#' @examples
#' bosc = simulate_experiment(n = 10, n_timepoints = 10, n_trials = 10)
#' bosc = generate_surrogates(bosc, n_surr = 100, method = "perm")
#'
#' @author René Michel

generate_surrogates <-
  function(bosc,
           n_surr = 100,
           method = "perm",
           seed_num = NULL,
           overwrite = F,
           aggregate = T) {

    # check for bosc object
    if (class(bosc) != "BOSC-Object") {
      stop("No object of class 'BOSC-Object' found. Consider using 'bosc()' to generate a BOSC object before calling this function.")
    }

    # check for already existing surrogates
    if (is.null(bosc$data$single_trial$surrogate) == F & overwrite == F) {
      stop("BOSC-Object seems to already contain surrogate data. Please use 'overwrite == T' if you really want to create new surrogate data.")
    }

    # set seed
    if (is.null(seed_num)) {
      seed_num <- stats::runif(1, 1, 2^20)
    }
    set.seed(seed_num)

    # create surrogates
    if (method == "perm") {

      # create permutations
      bosc$data$single_trial$surrogate$data <- bosc$data$single_trial$real$data %>%
        # repeat dataset n_surr times
        dplyr::slice(rep(1:dplyr::n(), each = !!n_surr)) %>%
        # add an identifier variable for each surrogated dataset per subject
        dplyr::group_by(.data$subj, .data$time, .data$trial) %>%
        dplyr::mutate(n_surr = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        # shuffle the time vector within each subject and each surrogate dataset
        dplyr::group_by(.data$subj, .data$n_surr) %>%
        dplyr::mutate(time = sample(.data$time))

      # save information about surrogates in a spec file
      bosc$data$single_trial$surrogate$spec <- list(
        seed_num = seed_num,
        method = method,
        n_surr = n_surr
      )

      # aggregate surrogates
      if (aggregate == TRUE) {
        bosc <- aggregate_bosc(bosc, types = "surrogate", levels = c("ss", "ga"))
      }
    } else if (method == "ar") {
      bosc$data$ss$surrogate$data <- bosc$data$ss$real$data %>%
        # fit an AR model to each subject's time course
        dplyr::group_by(.data$subj) %>%
        tidyr::nest() %>%
        dplyr::mutate(model = purrr::map(.data$data, ~ forecast::Arima(
          y = .x$hr,
          order = c(1, 0, 0),
          method = "ML"
        ))) %>%
        # replicate the dataset to generate n_surr rows per subjects
        dplyr::slice(rep(1:dplyr::n(), each = !!n_surr)) %>%
        dplyr::group_by(.data$subj) %>%
        dplyr::mutate(n_surr = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        # simulate a time course from the AR model for each subj and n_surr
        dplyr::mutate(sim = purrr::map(.data$model, ~ stats::simulate(.x, n_sim = !!bosc$timepoints))) %>%
        tidyr::unnest(cols = c(.data$data, .data$sim)) %>%
        dplyr::select(-c(.data$hr, .data$model)) %>%
        dplyr::rename(hr = .data$sim)

      bosc$data$ss$surrogate$spec <- list(
        seed_num = seed_num,
        method = method,
        n_surr = n_surr
      )

      # AR1 models from grand average (this might be easier & faster using purrr:map as in simulate_experiment)
      bosc$data$ga$surrogate$data <- bosc$data$ga$real$data %>%
        # fit an AR model to the GA time course
        tidyr::nest(data = tidyr::everything()) %>%
        dplyr::mutate(model = purrr::map(.data$data, ~ forecast::Arima(
          y = .x$hr,
          order = c(1, 0, 0),
          method = "ML"
        ))) %>%
        # replicate the dataset to generate n_surr rows per subjects
        dplyr::slice(rep(1:dplyr::n(), each = !!n_surr)) %>%
        dplyr::mutate(n_surr = dplyr::row_number()) %>%
        # simulate a time course from the AR model for each n_surr
        dplyr::mutate(sim = purrr::map(.data$model, ~ stats::simulate(.x, n_sim = !!bosc$timepoints))) %>%
        tidyr::unnest(cols = c(.data$data, .data$sim)) %>%
        dplyr::select(-c(.data$hr, .data$model)) %>%
        dplyr::rename(hr = .data$sim)

      # save information about surrogates in a spec file
      bosc$data$ga$surrogate$spec <- list(
        seed_num = seed_num,
        method = method,
        n_surr = n_surr
      )
    } else {
      stop("Method unknown.")
    }

    # add executed command to history
    bosc$hist <- paste0(bosc$hist, "gen-surr_")


    return(bosc)
  }

#' generate_surrogates
#'
#' @description \code{generate_surrogates}
#' Generates surrogate datasets from a dense sampling study
#'
#' @param bosc
#' @param n_surr
#' @param method
#' @param n_seed
#' @param overwrite
#' @param aggregate
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
#' #' @author René Michel

generate_surrogates <-
  function(bosc,
           n_surr = 100,
           method = "perm",
           n_seed = 1,
           overwrite = F,
           aggregate = T) {

    # check for bosc object
    if(class(bosc) != "BOSC-Object"){
      stop("No object of class 'BOSC-Object' found. Consider using 'bosc()' to generate a BOSC object before calling this function.")
    }

    # check for already existing surrogates
    if (is.null(bosc$data$single_trial$surrogate) == F & overwrite == F) {
      stop("BOSC-Object seems to already contain surrogate data. Please use 'overwrite == T' if you really want to create new surrogate data.")
    }

    # set seed
    set.seed(n_seed)

    # create surrogates
    if (method == "perm") {

      # create permutations

      # create empty list (for loop version)
      #bosc$data$single_trial$surrogate$data <- vector(mode = "list", length = n_surr)
      #names(bosc$data$single_trial$surrogate$data) <- paste("S", 1:n_surr, sep = "_")

      # # slower loop version
      # for (iPerm in 1:n_surr) {
      #   bosc$data$single_trial$surrogate$data[[names(bosc$data$single_trial$surrogate$data)[iPerm]]] <- bosc$data$single_trial$data %>%
      #     dplyr::group_by(.data$subj) %>%
      #     dplyr::mutate(time = sample(.data$time)) %>%
      #     dplyr::mutate(n_surr = iPerm)
      # }
      # # bind all perms to a single dataframe (required for loop version)
      # bosc$data$single_trial$surrogate$data = do.call(rbind.data.frame, bosc$data$single_trial$surrogate$data)

      bosc$data$single_trial$surrogate$data <- bosc$data$single_trial$real$data %>%
        dplyr::slice(rep(1:dplyr::n(), each = !!n_surr)) %>%
        dplyr::group_by(.data$subj, .data$time, .data$trial) %>%
        dplyr::mutate(n_surr = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(.data$subj, .data$n_surr) %>%
        dplyr::mutate(time = sample(.data$time))


      bosc$data$single_trial$surrogate$spec <- list(
        n_seed = n_seed,
        method = method,
        n_surr = n_surr)

        # aggregate surrogates
        if (aggregate == T) {

          bosc = aggregate_bosc(bosc, type = "surrogate", levels = "ss-ga")

          # aggregate to single subject time courses
          # ss <- bosc$data$single_trial$surrogate$data %>%
          #   dplyr::group_by(.data$n_surr, .data$time, .data$subj) %>%
          #   dplyr::summarise(hr = mean(.data$resp))
          #
          # bosc$data$single_subject$surrogate$data <- ss
          # bosc$data$single_subject$surrogate$spec <- bosc$data$single_trial$surrogate$spec
          #
          # # aggregate to grand average
          # ga <- ss %>%
          #   dplyr::group_by(.data$n_surr, .data$time) %>%
          #   dplyr::summarise(hr = mean(.data$hr))
          #
          # bosc$data$grand_average$surrogate$data <- ga
          # bosc$data$grand_average$surrogate$spec <- bosc$data$single_trial$surrogate$spec

        }

    }else if(method == "ar"){

        # AR1 models for single participants
        #ss = bosc$data$single_subject$data %>%
        #    dplyr::group_by(.data$subj) %>%
        #    dplyr::mutate(ar1 = simulate(Arima(.data$hr, order = c(1, 0, 0)), nsim = length(!!bosc$timepoints)))

        ss = bosc$data$single_subject$real$data %>%
          dplyr::slice(rep(1:dplyr::n(), each = !!n_surr)) %>%
          dplyr::group_by(.data$subj, .data$time) %>%
          dplyr::mutate(n_surr = dplyr::row_number()) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(.data$subj, .data$n_surr) %>%
          dplyr::mutate(hr = stats::simulate(forecast::Arima(.data$hr, order = c(1, 0, 0)), nsim = length(!!bosc$timepoints)))
        #  dplyr::mutate(ar1 = stats::arima.sim(stats::arima(.data$hr, order = c(1, 0, 0)), n = length(!!bosc$timepoints)))

        bosc$data$single_subject$surrogate$data <- ss
        bosc$data$single_subject$surrogate$spec <- list(
          n_seed = n_seed,
          method = method,
          n_surr = n_surr)

        # AR1 models from grand average
        ga = bosc$data$grand_average$real$data %>%
          dplyr::slice(rep(1:dplyr::n(), each = !!n_surr)) %>%
          dplyr::group_by(.data$time) %>%
          dplyr::mutate(n_surr = dplyr::row_number()) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(.data$n_surr) %>%
          dplyr::mutate(hr = stats::simulate(forecast::Arima(.data$hr, order = c(1, 0, 0)), nsim = length(!!bosc$timepoints)))
        #  dplyr::mutate(ar1 = stats::arima.sim(stats::arima(.data$hr, order = c(1, 0, 0)), n = length(!!bosc$timepoints)))

        bosc$data$grand_average$surrogate$data <- ga
        bosc$data$grand_average$surrogate$spec <- list(
          n_seed = n_seed,
          method = method,
          n_surr = n_surr)


    }else{

      stop("Method unknown.")

    }



    return(bosc)
  }

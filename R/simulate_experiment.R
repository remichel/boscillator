#' simulate_experiment
#'
#' @param n_sub Number of subjects
#' @param n_timepoints Number of timepoints, e.g. number of SOAs probed
#' @param n_trials Number of trials per timepoint and participant
#' @param sfreq Sampling frequency of timepoints (in Hz)
#' @param osc_params intercept (mean performance of the subject), amplitude, frequency, and phase of the underlying oscillation (freq in Hz, phase = 0 to 2pi)
#' @param phase_jitter sd of phase distribution across trials within subjects and across subjects
#' @param amplitude_jitter sd of amp distribution across trials within subjects and across subjects
#' @param freq_jitter sd of freq distribution across trials within subjects and across subjects
#' @param intercept_jitter sd of mean performance across subjects
#' @param transient "none", "hanning", or "exponential"
#' @param transient_expModel_params model parameters for exponential transient
#' @param trend "none", "linear" or "exponential"
#' @param trend_linModel_params model parameters for linear trend in performance
#' @param trend_expModel_params model parameters for exponential trend in performance
#' @param aggregate aggregate single trial data to single subject and grand average time series? defaults to T
#' @param seed_num seed number. if not specified, a random seed will be used and saved alongside with the data.
#'
#' @description \code{simulate_experiment}
#' Simulates the raw dataset of a dense sampling study.
#'
#' @return A data frame
#' @importFrom dplyr %>%
#' @importFrom stats rnorm
#' @importFrom rlang .data
#' @export simulate_experiment
#' @name simulate_experiment
#'
#' @examples
#' bosc = simulate_experiment()
#'
#' @author René Michel
#'

simulate_experiment <-
  function(n_sub = 14,
           n_timepoints = 20,
           n_trials = 100,
           sfreq = 25,
           osc_params = c(0.5, .1, 4, 0),
           phase_jitter = c(0, 0),
           amplitude_jitter = c(0, 0),
           freq_jitter = c(0, 0),
           intercept_jitter = 0,
           transient = "none",
           transient_expModel_params = c(0, 1, .3),
           trend = "none",
           trend_linModel_params = c(osc_params[1], osc_params[2]),
           trend_expModel_params = c(0, 1 - 2 * osc_params[2], .6),
           aggregate = TRUE,
           seed_num = NULL) {

    # ------------------------------------------------
    # exponential model for transient window / trend
    # ------------------------------------------------

    expModel <- function(t, intercept, n0, tau) {
      intercept + n0 * exp(-t / tau)
    }

    # ------------------------------------------------
    # linear model for transient window / trend
    # ------------------------------------------------

    linModel <- function(t, b0, b1) {
      b0 + t * b1
    }

    # ------------------------------------------------
    # generative sinusoidal model
    # ------------------------------------------------

    sin_model <- function(t, intercept, amplitude, frequency, phi) {

      # window fctn to simulate transient
      if(transient == "hanning"){
        win_fun <- bspec::hannwindow(n_timepoints)
      }else if(transient == "exponential"){
        win_fun <- expModel(t, transient_expModel_params[1], transient_expModel_params[2], transient_expModel_params[3])
      }else if(transient == "none"){
        win_fun <- rep(1, n_timepoints)
      }else{
        stop(transient, " is no valid option for argument transient.")
      }

      #  simulate trend in data
      if(trend == "linear"){
        trend_fun <- linModel(t, trend_linModel_params[1], trend_linModel_params[2])
      }else if(trend == "exponential"){
        trend_fun <- expModel(t, trend_expModel_params[1], trend_expModel_params[2], trend_expModel_params[3])
      }else if(trend == "none"){
        trend_fun <- intercept
      }else{
        stop(trend, " is no valid option for argument trend.")
      }

      # oscillation
      osc_amplitude <- stats::rnorm(1, amplitude, amplitude_jitter[1])
      osc_freq      <- stats::rnorm(1, frequency, freq_jitter[1])
      osc_phase     <- stats::rnorm(1, phi, phase_jitter[1])
      oscillation   <- osc_amplitude * sin(2 * pi * t * osc_freq + osc_phase)

      # generate model
      trend_fun + oscillation * win_fun
    }

    # ------------------------------------------------
    # set seed
    # ------------------------------------------------
    if (is.null(seed_num)) {
      seed_num <- stats::runif(1, 1, 2^20)
    }
    set.seed(seed_num)

    # ------------------------------------------------
    # create time vector (.e.g. SOAs)
    # ------------------------------------------------
    t <- seq(0, (n_timepoints - 1) / sfreq, 1 / sfreq)

    # ------------------------------------------------
    # create grid of subjects x time points
    # ------------------------------------------------
    data <- expand.grid(list(
      subj = as.factor(c(1:n_sub)),
      time = t
    ))

    # ------------------------------------------------
    # simulate single trial data
    # ------------------------------------------------
    data <- data %>%
      dplyr::group_by(.data$subj) %>%
      # for each subject, determine the probability for a hit for each point in "t" from the underlying sinusoidal model
      # max() is used to avoid negative input values for the sin_model fctn which might occur due to high jitter values
      dplyr::mutate(osc = sin_model(
        t = .data$time,
        intercept = max(0, !!osc_params[1] + stats::rnorm(1, 0, !!intercept_jitter)),
        amplitude = max(0, !!osc_params[2] + stats::rnorm(1, 0, !!amplitude_jitter[2])),
        frequency = max(0, !!osc_params[3] + stats::rnorm(1, 0, !!freq_jitter[2])),
        phi = !!osc_params[4] + stats::rnorm(1, 0, !!phase_jitter[2])
      )) %>%
      # apply limiter to the probabilities to stay within probability space [0 1]
      dplyr::mutate(osc = dplyr::case_when(
        .data$osc > 1 ~ 1,
        .data$osc < 0 ~ 0,
        TRUE ~ .data$osc
      )) %>%
      dplyr::ungroup() %>%
      # based on the probabilites saved in .data$osc, simulate single trial responses for n_trials by drawing from a binom distr
      dplyr::mutate(trial = purrr::map(.data$osc, function(x) {
        stats::rbinom(x, n = !!n_trials, size = 1) %>%
          t() %>%
          dplyr::as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE))
      })) %>%
      tidyr::unnest(cols = .data$trial) %>%
      # rename the columns into "trial numbers"
      dplyr::rename_with(~ seq(1, n_trials, 1), .cols = paste0("...", 1:!!n_trials)) %>%
      # bring into long format
      tidyr::pivot_longer(
        col = "1":as.character(!!n_trials),
        names_to = "trial",
        values_to = "resp"
      ) %>%
      dplyr::mutate(trial = as.numeric(.data$trial)) %>%
      # eliminate underlying probability values
      dplyr::select(-.data$osc)


    # ------------------------------------------------
    # create BOSC object
    # ------------------------------------------------
    bosc <- bosc()
    # save timepoints
    bosc$timepoints <- t
    # save simulated data
    bosc$data$single_trial$real$data <- data
    # save information about simulation parameters
    bosc$data$single_trial$real$spec <- list(
      seed_num = seed_num,
      n_sub = n_sub,
      n_timepoints = n_timepoints,
      n_trials = n_trials,
      sfreq = sfreq,
      osc_params = osc_params,
      phase_jitter = phase_jitter,
      amplitude_jitter = amplitude_jitter,
      freq_jitter = freq_jitter,
      intercept_jitter = intercept_jitter,
      transient = transient,
      transient_expModel_params = transient_expModel_params,
      trend = trend,
      trend_linModel_params = trend_linModel_params,
      trend_expModel_params = trend_expModel_params
    )

    # ------------------------------------------------
    # add executed command to history
    # ------------------------------------------------
    bosc$hist <- paste0(bosc$hist, "sim-exp_")

    # ------------------------------------------------
    # if desired, aggregate the single trial data
    # ------------------------------------------------
    if (aggregate == TRUE) {
      bosc <- aggregate_bosc(bosc, types = "real", levels = c("ss", "ga"))
    }

    return(bosc)
  }

#' simulate_experiment
#'
#' @param n_sub Number of subjects
#' @param n_timepoints Number of timepoints, e.g. number of SOAs probed
#' @param n_trials Number of trials per timepoint and participant
#' @param sfreq Sampling frequency of timepoints (in Hz)
#' @param intercept mean performance of the subject
#' @param amplitude amplitude of the underlying oscillation
#' @param freq frequency of the underlying oscillation
#' @param phi phase of the underlying oscillation (0 to 2pi)
#' @param phase_jitter_within_subj sd of phase distribution across trials within subjects
#' @param phase_jitter_across_subj sd of phase distribution across subjects
#' @param amplitude_jitter_within_subj sd of amp distribution across trials within subjects
#' @param amplitude_jitter_across_subj sd of amp distribution across subjects
#' @param freq_jitter_within_subj sd of freq distribution across trials within subjects
#' @param freq_jitter_across_subj sd of freq distribution across subjects
#' @param intercept_jitter_across_subj sd of mean performance across subjects
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
           intercept = 0.5,
           amplitude = .1,
           freq = 4,
           phi = 0,
           phase_jitter_within_subj = 0,
           phase_jitter_across_subj = 0,
           amplitude_jitter_within_subj = 0,
           amplitude_jitter_across_subj = 0,
           freq_jitter_within_subj = 0,
           freq_jitter_across_subj = 0,
           intercept_jitter_across_subj = 0,
           transient = "none",
           transient_expModel_params = c(0, 1, .3),
           trend = "none",
           trend_linModel_params = c(intercept, amplitude),
           trend_expModel_params = c(0, 1 - 2 * amplitude, .6),
           aggregate = T,
           seed_num = NULL) {
    # set seed
    if (is.null(seed_num)) {
      seed_num <- sample(1:1000, 1)
    }
    set.seed(seed_num)

    # create time vector
    t <- seq(0, (n_timepoints - 1) * 1 / sfreq, 1 / sfreq)

    # exponential model for transient window / trend
    expModel <- function(t, intercept, n0, tau) {
      intercept + n0 * exp(-t / tau)
    }
    linModel <- function(t, b0, b1) {
      b0 + t * b1
    }



    # generative sinusoidal model
    sin_model <- function(t, intercept, amplitude, frequency, phi) {
      `if`(
        trend == "linear", linModel(t, trend_linModel_params[1], trend_linModel_params[2]),
        `if`(
          trend == "exponential", expModel(t, trend_expModel_params[1], trend_expModel_params[2], trend_expModel_params[3]),
          intercept
        )
      ) + ((amplitude + stats::rnorm(length(t), 0, amplitude_jitter_within_subj)) * sin(2 * pi * t * (frequency + stats::rnorm(
        length(t), 0, freq_jitter_within_subj
      )) + phi + stats::rnorm(length(t), 0, phase_jitter_within_subj))) * `if`(
        transient == "hanning", bspec::hannwindow(n_timepoints),
        `if`(
          transient == "exponential", expModel(t, transient_expModel_params[1], transient_expModel_params[2], transient_expModel_params[3]),
            rep(1, n_timepoints)
          )
        )


      # add a AR1 model here as well
      # add random walks here as well
      # noise terms
    }

    # sample single trial data
    data <- expand.grid(as.factor(c(1:n_sub)), t)
    colnames(data) <- c("subj", "time")

    # simulate probabilities for hits based on oscillation
    data <- data %>%
      dplyr::group_by(.data$subj) %>%
      dplyr::mutate(osc = sin_model(
        .data$time,
        !!intercept + stats::rnorm(1, 0, !!intercept_jitter_across_subj),
        !!amplitude + stats::rnorm(1, 0, !!amplitude_jitter_across_subj),
        !!freq + stats::rnorm(1, 0, !!freq_jitter_across_subj),
        !!phi + stats::rnorm(1, 0, !!phase_jitter_across_subj)
      )) %>%
      dplyr::ungroup()

    # simulate single trial responses
    data <- cbind(data, t(
      sapply(data$osc, stats::rbinom, n = n_trials, size = 1)
    ))

    # bring into long format
    data <- data %>%
      tidyr::pivot_longer(
        col = "1":as.character(!!n_trials),
        names_to = "trial",
        values_to = "resp"
      ) %>%
      dplyr::mutate(trial = as.numeric(.data$trial)) %>%
      dplyr::select(-.data$osc)

    # create bosc object
    bosc <- bosc()
    bosc$timepoints <- t
    bosc$data$single_trial$real$data <- data
    bosc$data$single_trial$real$spec <- list(
      seed = seed_num,
      n_sub = n_sub,
      n_timepoints = n_timepoints,
      n_trials = n_trials,
      sfreq = sfreq,
      intercept = intercept,
      amplitude = amplitude,
      freq = freq,
      phi = phi,
      phase_jitter_within_subj = phase_jitter_within_subj,
      phase_jitter_across_subj = phase_jitter_across_subj,
      amplitude_jitter_within_subj = amplitude_jitter_within_subj,
      amplitude_jitter_across_subj = amplitude_jitter_across_subj,
      freq_jitter_within_subj = freq_jitter_within_subj,
      freq_jitter_across_subj = freq_jitter_across_subj,
      intercept_jitter_across_subj = intercept_jitter_across_subj
    )
    class(bosc) <- "BOSC-Object"

    # add executed command to history
    bosc$hist <- paste0(bosc$hist, "sim-exp_")

    if(aggregate == TRUE){
      bosc = aggregate_bosc(bosc, types = "real", levels = "ss-ga")
    }

    return(bosc)
  }

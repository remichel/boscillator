#' simulate_experiment
#'
#' @param n_sub
#' @param n_timepoints
#' @param n_trials
#' @param sfreq
#' @param intercept
#' @param amplitude
#' @param freq
#' @param phi
#' @param phase_jitter_within_subj
#' @param phase_jitter_across_subj
#' @param amplitude_jitter_within_subj
#' @param amplitude_jitter_across_subj
#' @param freq_jitter_within_subj
#' @param freq_jitter_across_subj
#' @param intercept_jitter_across_subj
#' @param transient
#' @param transient_expModel_params
#' @param trend
#' @param trend_linModel_params
#' @param trend_expModel_params
#' @param aggregate
#' @param seed_num
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
    bosc$data$single_trial$real$sim_spec <- list(
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

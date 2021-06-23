#' simulate_experiment
#'
#' @description \code{simulate_experiment}
#' Simulates the raw dataset of a dense sampling study.
#'
#' @param n_samples Number of samples
#'
#' @return A data frame
#' @importFrom dplyr %>%
#' @importFrom stats rnorm
#' @export simulate_experiment
#' @name simulate_experiment
#'
#' @examples
#' data = simulate_experiment()
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
           seed_num = NULL) {
    # set seed
    if (is.null(seed_num)) {
      seed_num <- sample(1:1000, 1)
    }
    set.seed(seed_num)

    # create time vector
    t <- seq(0, (n_timepoints - 1) * 1 / sfreq, 1 / sfreq)

    # generative sinusoidal model
    sin_model <- function(t, intercept, amplitude, frequency, phi) {
      intercept + (amplitude + stats::rnorm(length(t), 0, amplitude_jitter_within_subj)) * sin(2 * pi * t * (frequency + stats::rnorm(
        length(t), 0, freq_jitter_within_subj
      )) + phi + stats::rnorm(length(t), 0, phase_jitter_within_subj))
    }

    # sample single trial data
    data <- expand.grid(as.factor(c(1:n_sub)), t)
    colnames(data) <- c("subj", "time")

    # simulate probabilities for hits based on oscillation
    data <- data %>%
      dplyr::group_by(subj) %>%
      dplyr::mutate(osc = sin_model(
        time,
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
      dplyr::mutate(trial = as.numeric(trial)) %>%
      dplyr::select(-osc)

    # create bosc object
    bosc <- bosc()
    bosc$timepoints <- t
    bosc$data$single_trial$data <- data
    bosc$data$single_trial$sim_spec <- list(
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

    return(bosc)
  }

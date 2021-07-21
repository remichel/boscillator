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
           phase_jitter = c(0,0),
           amplitude_jitter = c(0,0),
           freq_jitter = c(0,0),
           intercept_jitter = 0,
           transient = "none",
           transient_expModel_params = c(0, 1, .3),
           trend = "none",
           trend_linModel_params = c(osc_params[1], osc_params[2]),
           trend_expModel_params = c(0, 1 - 2 * osc_params[2], .6),
           aggregate = T,
           seed_num = NULL) {

    # exponential model for transient window / trend
    expModel <- function(t, intercept, n0, tau) {
      intercept + n0 * exp(-t / tau)
    }

    # linear model for transient window / trend
    linModel <- function(t, b0, b1) {
      b0 + t * b1
    }

    # generative sinusoidal model
    sin_model <- function(t, intercept, amplitude, frequency, phi) {
      `if`(trend == "linear", linModel(t, trend_linModel_params[1], trend_linModel_params[2]),
        `if`(trend == "exponential", expModel(t, trend_expModel_params[1], trend_expModel_params[2], trend_expModel_params[3]),
          intercept)
        ) + ((amplitude + stats::rnorm(length(t), 0, amplitude_jitter[1])) * sin(2 * pi * t * (frequency + stats::rnorm(
        length(t), 0, freq_jitter[1])) + phi + stats::rnorm(length(t), 0, phase_jitter[1]))
        ) * `if`(transient == "hanning", bspec::hannwindow(n_timepoints),
                 `if`(transient == "exponential", expModel(t, transient_expModel_params[1], transient_expModel_params[2], transient_expModel_params[3]),rep(1, n_timepoints)
        )
      )


      # add a AR1 model here as well
      # add random walks here as well
      # noise terms
    }


    # set seed
    if (is.null(seed_num)) {
      seed_num <- sample(1:1000, 1)
    }
    set.seed(seed_num)

    # create time vector
    t <- seq(0, (n_timepoints - 1) * 1 / sfreq, 1 / sfreq)

    # create grid of subjects x time points
    data <- expand.grid(as.factor(c(1:n_sub)), t)
    colnames(data) <- c("subj", "time")

    # simulate "oscillating" hit probabilities across time points for each subject
    data <- data %>%
      dplyr::group_by(.data$subj) %>%
      dplyr::mutate(osc = sin_model(.data$time,
                                    !!osc_params[1] + stats::rnorm(1, 0, !!intercept_jitter),
                                    !!osc_params[2] + stats::rnorm(1, 0, !!amplitude_jitter[2]),
                                    !!osc_params[3] + stats::rnorm(1, 0, !!freq_jitter[2]),
                                    !!osc_params[4] + stats::rnorm(1, 0, !!phase_jitter[2]))) %>%
      dplyr::ungroup()

    # simulate single trial responses
    data <- cbind(data, t(sapply(data$osc, stats::rbinom, n = n_trials, size = 1)))

    # bring into long format
    data <- data %>%
      tidyr::pivot_longer(
        col = "1":as.character(!!n_trials),
        names_to = "trial",
        values_to = "resp"
      ) %>%
      dplyr::mutate(trial = as.numeric(.data$trial)) %>%
      dplyr::select(-.data$osc)

    # create BOSC object
    bosc <- bosc()
    bosc$timepoints <- t
    bosc$data$single_trial$real$data <- data
    bosc$data$single_trial$real$spec <- list(
      seed = seed_num,
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
    class(bosc) <- "BOSC-Object"

    # add executed command to history
    bosc$hist <- paste0(bosc$hist, "sim-exp_")

    if(aggregate == TRUE){
      bosc = aggregate_bosc(bosc, types = "real", levels = "ss-ga")
    }

    return(bosc)
  }




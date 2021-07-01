#' simulate_time_course
#'
#' @description \code{simulate_time_course}
#' Simulates a time course from a dense sampling study.
#'
#' @param n_samples Number of samples
#' @param t
#' @param sfreq
#' @param start
#' @param seed_num
#' @param snratio
#' @param range
#' @param intercept
#' @param amplitude
#' @param frequency
#' @param phi
#'
#' @return A vector
#' @export simulate_time_course
#' @name simulate_time_course
#'
#' @examples
#' tc = simulate_time_course()
#'
#' @author René Michel

simulate_time_course <- function(t = NULL, n_samples = 20, sfreq = 24, start = .192,
                                 seed_num = NULL,
                                 snratio = 1, range = c(0,1),
                                 intercept = 0.5, amplitude = .1, frequency = 4, phi = 0){

  # set seed
  if(is.null(seed_num)){
    seed_num = sample(1:1000,1)
  }
  set.seed(seed_num)

  # sampling points
  if(is.null(t)){
    t = seq(start, start + 1/sfreq * (n_samples - 1) , 1/sfreq)
  }

  # generative sinusoidal model
  sin_model <- function(t, intercept, amplitude , frequency, phi){
      intercept + amplitude * sin(2 * pi * t * frequency + phi)
    }

  # simulate raw oscillation
  raw_oscillation = sin_model(t, intercept, amplitude, frequency, phi)

  # add noise to oscillation
  time_course = raw_oscillation + stats::rnorm(length(t), 0, snratio * stats::sd(raw_oscillation))

  # rescale to 0:1 or min/max of time_course to stay within plausible values for accuracies / hit rates
  time_course = scales::rescale(time_course, c(max(range[1], min(time_course)), min(max(time_course), range[2])))

  # create bosc object
  bosc = bosc()

  bosc$timepoints = t
  bosc$data$grand_average = time_course
  bosc$data$seed = seed_num
  class(bosc) = "BOSC-Object"

  return(bosc)

}

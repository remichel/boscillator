#' bosc_sim_default
#'
#' @description \code{bosc_sim_default}
#' Wrapper function for bosc to simulate, preprocess and statistically test an experiment with default values.
#'
#' @param surrogates want to generate surrogates?
#' @param detrend want to detrend?
#' @param window want to window?
#' @param scale want to scale?
#' @param pad want to pad?
#' @param fft want to fft?
#' @param sinmod want sin modelling?
#' @param fft_test want stat test for fft?
#' @param sinmod_test want stat test for modelling?
#'
#' @return A BOSC-Object
#' @export bosc_sim_default
#' @name bosc_sim_default
#'
#' @examples
#' bosc <- bosc_sim_default()
#'

bosc_sim_default <- function(surrogates = T,
                             detrend = T,
                             window = T,
                             scale = F,
                             pad = F,
                             fft = F,
                             sinmod = F,
                             fft_test = F,
                             sinmod_test = F){

  bosc <- boscillator::simulate_experiment()

  if(surrogates){bosc <- boscillator::generate_surrogates(bosc, n_surr = 10)}
  if(detrend){bosc <- boscillator::detrend_bosc(bosc)}
  if(window){bosc <- boscillator::window_bosc(bosc)}
  if(scale){bosc <- scale_bosc(bosc)}
  if(pad){bosc <- pad_bosc(bosc)}
  if(fft){bosc <- fft_bosc(bosc)}
  if(sinmod){bosc <- sinmod_bosc(bosc)}
  if(fft_test){bosc <- test_fft(bosc)}
  if(sinmod_test){bosc <- test_sinmod(bosc)}

  return(bosc)
}

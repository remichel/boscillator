#' bosc_sim_default
#'
#' @description \code{bosc_sim_default}
#' Simulates and preprocesses an experiment with default values.
#'
#' @param surrogates want to generate surrogates?
#' @param detrend want to detrend?
#' @param window want to window?
#' @param scale want to scale?
#' @param pad want to pad?
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
                             pad = F){

  bosc <- boscillator::simulate_experiment()

  if(surrogates){
    bosc <- boscillator::generate_surrogates(bosc, n_surr = 10)
  }

  if(detrend){
    bosc <- boscillator::detrend_bosc(bosc)
  }
  if(window){
    bosc <- boscillator::window_bosc(bosc)
  }
  if(scale){
    bosc <- scale_bosc(bosc)
  }
  if(pad){
    bosc <- pad_bosc(bosc)
  }

  return(bosc)
}

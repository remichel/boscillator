#' bosc_import
#'
#' @description \code{bosc_import}
#' Imports a dense sampling study's data frame into boscillator.
#'
#' @param data A dataframe with dense sampling data
#' @param sfreq in Hz
#' @param vars The index variables in your data frame that identify (1) subjects, (2) timepoints, (3) trials (only for single trial), and (4) the responses. Insert the names as a vector in the order specified before (ie. c = ("subj", "time", "trial", "resp")), regardless of the order they occur in your dataset.
#' @param level Specify the level of your data, e.g. "single_trial", "ss" or "ga"
#' @param n_timepoints Number of unique timepoints in dataset
#' @param aggregate Shall single trial data be averaged to "ss" and "ga"?
#'
#' @return bosc-Object
#' @export bosc_import
#'
bosc_import <- function(data, sfreq, n_timepoints, vars = c("subj", "time", "trial", "resp"), level = "single_trial", aggregate = T){

  #create bosc object
  bosc = bosc()

  if(level == "single_trial"){
    # check index variables
    index_vars = c("subj", "time", "trial", "resp")

    for(iVar in 1:length(vars)){
      if(vars[iVar] != index_vars[iVar]){
        message("Warning: Variable ", vars[iVar], ' will be renamed to ', index_vars[iVar])
      }
    }

    # subset to relevant variables
    data <- data %>%
      dplyr::select(!!vars)
    # rename variables
    colnames(data) <- index_vars

    # assign timepoints
    bosc$timepoints = sort(unique(data$time))
    bosc$data$single_trial$real$spec$n_timepoints = n_timepoints

    # save sfreq
    bosc$data$single_trial$real$spec$sfreq = sfreq

    # save data in bosc
    bosc$data$single_trial$real$data = data

  }else if(level == "ss"){

    # check index variables
    index_vars = c("subj", "time", "hr")

    for(iVar in 1:length(vars)){
      if(vars[iVar] != index_vars[iVar]){
        message("Warning: Variable ", vars[iVar], ' will be renamed to ', index_vars[iVar])
      }
    }

    # subset to relevant variables
    data <- data %>%
      dplyr::select(!!vars)
    # rename variables
    colnames(data) <- index_vars

    # assign timepoints
    bosc$timepoints = sort(unique(data$time))
    bosc$data$single_trial$real$spec$n_timepoints = n_timepoints

    # save sfreq
    bosc$data$single_trial$real$spec$sfreq = sfreq

    # save data in bosc
    bosc$data$ss$real$data = data
  }else{
    stop("Your desired level is not implemented in boscillator yet... :(")
  }


  # calculate mean sfreq from differences between time points
  timepoints <- sort(unique(data$time))
  est_sfreq <-  1/mean(diff(timepoints))

  # check if timepoints and sfreq can be reproduced in data
  if(length(timepoints) !=  n_timepoints){
    warning("Number of timepoints doesn't match number of timepoints found in dataset")
  }
  if(est_sfreq != sfreq){
    warning("Estimated sfreq within dataset (derived from spacing between time points) is ", est_sfreq, "Hz but ", sfreq, "Hz was piped into sfreq-argument.")
  }



  message("Found ", length(unique(data$subj)),
          " subjects.\nFound ", length(timepoints),
          " timepoints ranging from ", timepoints[1] ," to ", timepoints[n_timepoints] ,
          ".\nFound ", stats::xtabs(~data$subj+data$time)[1], " trials per condition.")


  # add executed command to history
  bosc$hist <- paste0(bosc$hist, "import_")

  # aggregate
  if(aggregate == T){
    if(level = "single_trial"){
      bosc = aggregate_bosc(bosc, types = "real", levels = c("ss", "ga"))
    }else if(level = "ss"){
      bosc = aggregate_bosc(bosc, types = "real", levels = c("ga"))
    }
  }

  return(bosc)

}

#' bosc_import
#'
#' @description \code{bosc_import}
#'
#' @param bosc A bosc-object
#' @param data A dataframe with dense sampling data
#' @param sfreq in Hz
#' @param vars The index variables in your data frame that identify, for single trials, (1) subjects, (2) timepoints, (3) trials (only for single trial), (4) the responses and (5) number of surrogate dataset, or for single subject data, (1) subjects, (2) timepoints,  (3) number of surrogate dataset, and (4) the hitrate (or aggregated measure). Insert the names as a vector in the order specified before (e.g.. c = ("subj", "time", "n_surr", "hr")), regardless of the order they occur in your dataset.
#' @param level Specify the level of your imported data, e.g. "single_trial", "ss" or "ga"
#' @param n_timepoints Number of unique timepoints in dataset
#' @param aggregate Shall single trial data be averaged to "ss" and "ga"?
#' @param overwrite Argument passed to bosc_aggregate, defaults to F
#'
#' @return bosc-Object
#' @export bosc_import
#'
bosc_import_surrogates <- function(bosc, data, sfreq, n_timepoints, vars = c("subj", "time", "n_surr", "hr"), level = "ss", aggregate = TRUE, overwrite = F){


  if (level == "single_trial") {
    # check index variables
    index_vars <- c("subj", "time", "trial", "resp", "n_surr")

    for (iVar in 1:length(vars)) {
      if (vars[iVar] != index_vars[iVar]) {
        message("Warning: Variable ", vars[iVar], " will be renamed to ", index_vars[iVar])
      }
    }

    # subset to relevant variables
    data <- data %>%
      dplyr::select(!!vars)
    # rename variables
    colnames(data) <- index_vars

    # save data in bosc
    bosc$data$single_trial$surrogate$data <- data

  } else if (level == "ss") {

    # check index variables
    index_vars <- c("subj", "time", "n_surr", "hr")

    for (iVar in 1:length(vars)) {
      if (vars[iVar] != index_vars[iVar]) {
        message("Warning: Variable ", vars[iVar], " will be renamed to ", index_vars[iVar])
      }
    }

    # subset to relevant variables
    data <- data %>%
      dplyr::select(!!vars)
    # rename variables
    colnames(data) <- index_vars

    # save data in bosc
    bosc$data$ss$surrogate$data <- data

  } else {
    stop("Your desired level is not implemented in boscillator yet... :(")
  }

  # check if timepoints and sfreq can be reproduced in data
  if (length(unique(data$time)) != n_timepoints) {
    warning("Number of timepoints doesn't match number of timepoints found in dataset")
  }


  message("Found ", length(unique(data$subj)), " subjects.\nFound ", length(unique(data$time)), " timepoints ranging from ", sort(unique(data$time))[1], " to ", sort(unique(data$time))[n_timepoints], ".\nFound ", stats::xtabs(~ data$subj + data$time)[1], " surrogated datasets.")

  # add executed command to history
  bosc$hist <- paste0(bosc$hist, "import_")

  # aggregate
  if (aggregate == TRUE) {

    # which levels need to be created?
    if(level == "single_trial"){
      agg_level = c("ss", "ga")
    }else if(level == "ss"){
      agg_level = c("ga")
    }

    message("Aggregating ", level, " surrogate data to ", paste(agg_level, collapse = " & "), ".")
    # aggregate
    bosc <- aggregate_bosc(bosc, levels = agg_level, types = "surrogate", overwrite = overwrite)
  }

  return(bosc)

}

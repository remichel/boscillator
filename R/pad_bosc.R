#' pad_bosc
#'
#' @description \code{pad_bosc}
#' Adds pads to the time courses in an BOSC-Object.
#'
#' @param bosc BOSC-Object
#' @param types Which data should be padded? choose between "real" and "surrogate" or concatenate to "real-surrogate" to perform aggregation on both.
#' @param levels Which levels of data need to be padded? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
#' @param method Padding method? "zero" or "mean"
#' @param n_pads Number of pads. n_pads = 0 will skip padding
#' @param verbose defaults to T
#'
#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export pad_bosc
#' @name pad_bosc
#'
#' @examples
#' bosc = simulate_experiment()
#' bosc = pad_bosc(bosc, types = "real", levels = "ga", method = "zero")
#'
pad_bosc <- function(bosc,
                     types = "real-surrogate",
                     levels = "ss-ga",
                     method = "zero",
                     n_pads = length(bosc$timepoints),
                     verbose = T) {

  # get levels
  if(!is.character(levels)){
    stop("Argument levels must be a character.")
  }else{
    iLevel_list <- split_string_arg(levels, "-")
  }

  # get types
  if(!is.character(types)){
    stop("Argument types must be a character.")
  }else{
    iType_list <- split_string_arg(types, "-")
  }

  # check method
  if(!is.character(method)){
    stop("Method must be a character.")
  }

  # check n_pads
  if(n_pads < 0){
    #stop("Number of pads needs to be either positive or zero (for skipping zero-padding")
    message("Number of pads is negative. Will use default option n_pads = length(bosc$timepoints) instead...")
    n_pads = length(bosc$timepoints)
    skip = F
  }else if(n_pads == 0){
    skip = T
  }else{
    skip = F
  }

  if(skip == F){

    # determine pad lengths
    n_prior = ceiling(n_pads/2)
    n_after = n_pads - n_prior

    if(n_prior + n_after != n_pads) warning(paste(n_pads, "were submitted to pad_bosc, but", n_prior + n_after, "pads will be added..."))

    # determine sampling interval
    s_interval = 1 / bosc$data$single_trial$real$spec$sfreq

    # equally split the number of pads into two vectors, to add them prior to / after the actual time vector respectively
    pads_prior = seq(max(bosc$timepoints) + s_interval,
                     max(bosc$timepoints) + n_prior * s_interval,
                     s_interval)

    pads_after = seq(min(bosc$timepoints) - n_after * s_interval,
                     min(bosc$timepoints) - s_interval,
                     s_interval)

    # create pad time vector
    pads_time = c(pads_prior, pads_after)


    if(verbose == T) message("Start Padding...")

    # loop through all conditions
    for (iType in iType_list) {
      for (iLevel in iLevel_list) {

        if(verbose == T) message(paste("Padding", iLevel, iType, "..."))

        # check if required data exists
        if(is.null(bosc$data[[iLevel]][[iType]]$data)){
          if(verbose == T) message(paste("No data found in ", iLevel, iType, ".\nWill continue with next iType/iLevel..."))
          next
        }

        # check whether padding was already applied for the condition at hand
        if(!is.null(bosc$data[[iLevel]][[iType]]$preprocessing)){
          if("PADDED" %in% split_string_arg(bosc$data[[iLevel]][[iType]]$preprocessing, "_")){
            reply = utils::menu(c("Yes", "No"), title = paste("Data in", iLevel, iType, "was already padded. Are you sure you want to continue with yet another padding?"))
            if(reply == 2){
              next
            }
          }
        }

        # define group vars
        if (iType == "real") {
          if(iLevel == "ss"){
            group_vars = dplyr::sym("subj")
          }else{
            group_vars = dplyr::syms(NULL)
          }
        }else if(iType == "surrogate"){
          if(iLevel == "ss"){
            group_vars = dplyr::syms(c("subj", "n_surr"))
          }else{
            group_vars = dplyr::sym("n_surr")
          }
        }

        # add pads to data
        bosc$data[[iLevel]][[iType]]$data <- bosc$data[[iLevel]][[iType]]$data %>%
          dplyr::group_by(!!!group_vars) %>%
          # add the padded time vector to each group (and fill hr with NA first)
          dplyr::group_modify(~ dplyr::add_row(., time = !!pads_time,
                                               hr = NA)) %>%
          # now, fill the NAs with zeros or mean, depending on the desired method
          dplyr::mutate(hr = dplyr::case_when(.data$time %in% !!pads_time & !!method == "mean" ~ mean(.data$hr, na.rm = T),
                                              .data$time %in% !!pads_time & !!method == "zero" ~ 0,
                                              TRUE ~ .data$hr)
          ) %>%
          dplyr::arrange(!!!group_vars, .data$time) %>%
          dplyr::ungroup()


        # add pre-processing step to documentation
        if (is.null(bosc$data[[iLevel]][[iType]]$preprocessing)) {
          bosc$data[[iLevel]][[iType]]$preprocessing <- paste0("PADDED_METHOD:", method)
        } else {
          bosc$data[[iLevel]][[iType]]$preprocessing <- paste(bosc$data[[iLevel]][[iType]]$preprocessing, paste0("PADDED_METHOD:", method), sep = "_")
        }

      }
    }


    # add executed command to history
    bosc$hist <- paste0(bosc$hist, "padd_")
  }

  if(verbose == T) message("Padding completed.")
  return(bosc)
}

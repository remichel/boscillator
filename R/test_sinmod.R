#' test_sinmod
#'
#' @description \code{test_sinmod}
#' Performs statistical test on sinusoidal models obtained from sinmod_bosc.
#'
#' @param bosc BOSC-Object
#' @param levels Which levels of data need to be padded? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
#' @param tests  Which test to perform?
#' @param alpha_r2 Which alpha level to apply for R² permutation test
#' @param overwrite defaults to F
#' @param verbose defaults to T
#'
#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export test_sinmod
#' @name test_sinmod
#'
#' @examples
#' bosc = simulate_experiment()
#' bosc = generate_surrogates(bosc)
#' bosc = sinmod_bosc(bosc)
#' bosc = test_sinmod(bosc, levels = "ga", tests = "r2")
#'
test_sinmod <- function(bosc, levels = "ss-ga", tests = "r2", alpha_r2 = .05, overwrite = FALSE, verbose = T) {

  # get levels
  if(!is.character(levels)){
    stop("Argument levels must be a character.")
  }else{
    iLevel_list <- split_string_arg(levels, "-")
  }

  # get tests
  if(!is.character(tests)){
    stop("Argument levels must be a character.")
  }else{
    iTest_list <- split_string_arg(tests, "-")
  }

  if(verbose == T) message("Starting tests...")


  # loop through all conditions
  for (iLevel in iLevel_list) {

    if(verbose == T) message("\nTest on ", iLevel, ' level...\n')

    for(iTest in iTest_list){


      # check if required data exists
      if(is.null(bosc$data[[iLevel]]$real$sinmod) | is.null(bosc$data[[iLevel]]$surrogate$sinmod)){
        if(verbose == T) message(paste("No data found in ", iLevel, ".\nWill continue with next iType/iLevel..."))
        next
      }

      # check if test data exists
      if(!is.null(bosc$tests$sinmod[[iLevel]][[iTest]])){
        if(overwrite == TRUE){
          if(verbose == T) message("Test already exists. Will overwrite...")
        }else{
          warning("Test already exists. Will skip to next dataset without performing the FFT...")
          next
        }
      }



      # Perform test
      if(iTest == "r2"){

        # get correct group vars and join vars depending on the analysis level
        if("fixed_f" %in% colnames(bosc$data[[iLevel]]$real$sinmod)){
          if (iLevel == "ss") {
            group_vars = dplyr::syms(c("subj", "fixed_f"))
            join_vars = c("subj","fixed_f")
          }else if(iLevel == "ga"){
            group_vars = dplyr::sym("fixed_f")
            join_vars = c("fixed_f")
          }
        }
        else{
          if (iLevel == "ss") {
            group_vars = dplyr::sym("subj")
            join_vars = c("subj")
          }else if(iLevel == "ga"){
            group_vars = dplyr::syms(NULL)
            join_vars = NULL
          }
        }

        if(verbose == T) message("Comparing Observed R² against Permutations (alpha = ", alpha_r2,")...")


        # join permutations and observed r2 and determine crit value & p value
        if("fixed_f" %in% colnames(bosc$data[[iLevel]]$real$sinmod)){
          bosc$tests$sinmod[[iLevel]][[iTest]]$results <- bosc$data[[iLevel]]$surrogate$sinmod %>%
            dplyr::left_join(bosc$data[[iLevel]]$real$sinmod, by = c(join_vars, "term"), suffix = c("_surr", "_observed")) %>%
            dplyr::select(.data$fixed_f, .data$r2_surr, .data$r2_observed) %>%
            dplyr::distinct() %>%
            dplyr::ungroup() %>%
            dplyr::group_by(!!!group_vars) %>%
            dplyr::mutate(n_surrogates = max(.data$n_surr),
                          crit_value = unname(stats::quantile(.data$r2_surr, probs = 1-!!alpha_r2, na.rm = T)), # is na.rm = T causing any harm here??
                          p = 1-stats::ecdf(.data$r2_surr)(.data$r2_observed)) %>%
            dplyr::distinct(.data$crit_value, .keep_all = T) %>%
            dplyr::select(-.data$n_surr, -.data$r2_surr) %>%
            dplyr::relocate(c(.data$fixed_f, .data$r2_observed), .before = n_surrogates) %>%
            dplyr::mutate(alpha = !!alpha_r2,
                          sig = dplyr::case_when(.data$r2_observed > .data$crit_value ~ 1,
                                                 .data$r2_observed <= .data$crit_value ~ 0))
        }else{
          bosc$tests$sinmod[[iLevel]][[iTest]]$results <- bosc$data[[iLevel]]$surrogate$sinmod %>%
            dplyr::left_join(bosc$data[[iLevel]]$real$sinmod, by = c(join_vars, "term"), suffix = c("_surr", "_observed")) %>%
            dplyr::filter(term == "f") %>%
            dplyr::select(.data$estimate_observed, .data$r2_surr, .data$r2_observed) %>%
            dplyr::ungroup() %>%
            dplyr::group_by(!!!group_vars) %>%
            dplyr::mutate(n_surrogates = max(.data$n_surr),
                          crit_value = unname(stats::quantile(.data$r2_surr, probs = 1-!!alpha_r2, na.rm = T)), # is na.rm = T causing any harm here??
                          p = 1-stats::ecdf(.data$r2_surr)(.data$r2_observed)) %>%
            dplyr::distinct(.data$crit_value, .keep_all = T) %>%
            dplyr::select(-.data$n_surr, -.data$r2_surr) %>%
            dplyr::relocate(c(.data$estimate_observed, .data$r2_observed), .before = n_surrogates) %>%
            dplyr::mutate(alpha = !!alpha_r2,
                          sig = dplyr::case_when(.data$r2_observed > .data$crit_value ~ 1,
                                                 .data$r2_observed <= .data$crit_value ~ 0))
        }




      }
    }
  }


  # add executed command to history
  bosc$hist <- paste0(bosc$hist, "test_")

  if(verbose == T) message("\nTest completed.")
  return(bosc)
}

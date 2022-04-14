#' test_sinmod
#'
#' @description \code{test_sinmod}
#' Performs statistical test on sinusoidal models obtained from sinmod_bosc.
#'
#' @param bosc BOSC-Object
#' @param levels Which levels of data need to be padded? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
#' @param tests  Which test to perform?
#' @param alpha Vector of alpha levels to apply. If you want to use multiple alpha levels, make sure to set mcc to "none"
#' @param overwrite defaults to F
#' @param verbose defaults to T
#' @param mcc choose between "bonferroni", "fdr" and "none". if != "none", the first entry in alpha will be considered the family wise alpha error and all other entries in alpha will be ignored

#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @export test_sinmod
#' @name test_sinmod
#'
#' @examples
#' bosc = simulate_experiment(n_sub = 3)
#' bosc = generate_surrogates(bosc, n_surr = 10)
#' bosc = sinmod_bosc(bosc, levels = "ga")
#' bosc = test_sinmod(bosc, levels = "ga", tests = "r2")
#'
test_sinmod <- function(bosc,
                        levels = c("ss", "ga"),
                        tests = c("r2"),
                        alpha = .05,
                        mcc = c("bonferroni", "fdr"),
                        overwrite = FALSE,
                        verbose = TRUE) {

  # ------------------------------------------------------------------------
  # get levels
  # ------------------------------------------------------------------------
  if (!is.character(levels)) {
    stop("Argument levels must be a character.")
  }

  # ------------------------------------------------------------------------
  # get tests
  # ------------------------------------------------------------------------
  if (!is.character(tests)) {
    stop("Argument levels must be a character.")
  }

  # ------------------------------------------------------------------------
  # get mccs
  # ------------------------------------------------------------------------
  if (!is.character(mcc)) {
    stop("Argument mcc must be a character.")
  } else {
    if (length(mcc) > 0 & mcc[1] != "none") {
      message("Multiple correction methods ", paste(mcc, collapse = " & "), " were chosen. All alpha values except ", alpha[1], " will be ignored. \nInstead, ", alpha[1], " will be considered the desired family-wise alpha level for all MCC..\n")
      alpha <- alpha[1]
    }
  }


  if (verbose == TRUE) message("Starting tests...")


  # ------------------------------------------------------------------------
  # loop through all conditions
  # ------------------------------------------------------------------------
  for (iLevel in levels) {

    if (verbose == TRUE) message("\nTest on ", iLevel, " level...\n")

    for (iTest in tests) {

      # ------------------------------------------------------------------------
      # check if required data exists
      # ------------------------------------------------------------------------
      if (is.null(bosc$data[[iLevel]]$real$sinmod) | is.null(bosc$data[[iLevel]]$surrogate$sinmod)) {
        if (verbose == TRUE) message(paste("No data found in ", iLevel, ".\nWill continue with next iType/iLevel..."))
        next
      }
      # ------------------------------------------------------------------------
      # check if test results already exist
      # ------------------------------------------------------------------------
      if (!is.null(bosc$tests$sinmod[[iLevel]][[iTest]])) {
        if (overwrite == TRUE) {
          if (verbose == TRUE) message("Test already exists. Will overwrite...")
        } else {
          warning("Test already exists. Will skip to next dataset without performing the FFT...")
          next
        }
      }


      # ------------------------------------------------------------------------
      # Perform test
      # ------------------------------------------------------------------------
      if (iTest == "r2") {

        # ------------------------------------------------------------------------
        # define group vars for the following step
        # ------------------------------------------------------------------------
        group_vars <- NULL
        # for fixed freq test, group by frequencies
        if ("fixed_f" %in% colnames(bosc$data[[iLevel]]$real$sinmod)) {
          group_vars <- c(group_vars, "fixed_f")
        }
        # for single subject data, additionally group by subject
        if (iLevel == "ss") {
          group_vars <- c(group_vars, "subj")
        }


        if (verbose == TRUE) message("Comparing Observed R2 against Permutations (alpha = ", paste(alpha, collapse = ", "), ")...")


        # ------------------------------------------------------------------------
        # fixed frequency test
        # ------------------------------------------------------------------------

        if ("fixed_f" %in% colnames(bosc$data[[iLevel]]$real$sinmod)) {


          # ------------------------------------------------------------------------
          # join permutations and observed r2 and determine crit value & p value
          # ------------------------------------------------------------------------

          bosc$tests$sinmod[[iLevel]][[iTest]]$results <- bosc$data[[iLevel]]$surrogate$sinmod %>%
            # merge observed and surrogate data values
            dplyr::left_join(bosc$data[[iLevel]]$real$sinmod, by = c(group_vars, "term"), suffix = c("_surr", "_observed")) %>%
            # reduce dataset to relevant vars
            dplyr::select(.data$fixed_f, .data$r2_surr, .data$r2_observed) %>%
            dplyr::distinct() %>%
            dplyr::ungroup() %>%
            # add alpha levels
            dplyr::left_join(as.data.frame(alpha), copy = TRUE, by = character()) %>%
            # for each alpha level and each group var level, get critical value
            dplyr::group_by_at(c(group_vars, "alpha")) %>%
            dplyr::mutate(
              n_surrogates = max(.data$n_surr),
              crit_value = unname(stats::quantile(.data$r2_surr, probs = 1 - alpha, na.rm = TRUE)), # is na.rm = TRUE causing any harm here??
              p = 1 - stats::ecdf(.data$r2_surr)(.data$r2_observed)
            ) %>%
            # get rid of duplicate lines, only keep one row per fit
            dplyr::distinct(.data$fixed_f, .keep_all = TRUE) %>%
            # get rid of unused columns
            dplyr::select(-.data$n_surr, -.data$r2_surr) %>%
            # restructure dataset
            dplyr::relocate(c(.data$fixed_f, .data$r2_observed), .before = .data$n_surrogates)

          # ------------------------------------------------------------------------
          # MCC
          # ------------------------------------------------------------------------

          # add max_freq MCC here!

          if (length(mcc) > 0 & !("none" %in% mcc)) {

            # define group vars for MCC
            group_vars <- NULL
            # for single subject data, group by subject
            if (iLevel == "ss") {
              group_vars <- c(group_vars, "subj")
            }

            # apply all MCC corrections
            for (iMCC in mcc) {
              bosc$tests$sinmod[[iLevel]][[iTest]]$results <- bosc$tests$sinmod[[iLevel]][[iTest]]$results %>%
                dplyr::group_by_at(c(group_vars, "alpha")) %>%
                dplyr::mutate(!!iMCC := stats::p.adjust(.data$p, method = !!iMCC))
            }

            # convert to long format and erase unused columns
            bosc$tests$sinmod[[iLevel]][[iTest]]$results <- bosc$tests$sinmod[[iLevel]][[iTest]]$results %>%
              dplyr::group_by_at(c(group_vars, "alpha")) %>%
              dplyr::rename(uncorrected = .data$p) %>%
              tidyr::pivot_longer(cols = c(.data$uncorrected, !!mcc), names_to = "mcc_method", values_to = "p") %>%
              dplyr::select(-.data$crit_value) # misleading if mcc is used, as it refers to uncorrected alpha
          }

          # ------------------------------------------------------------------------
          # add significance column
          # ------------------------------------------------------------------------
          bosc$tests$sinmod[[iLevel]][[iTest]]$results <- bosc$tests$sinmod[[iLevel]][[iTest]]$results %>%
            dplyr::mutate(sig = dplyr::case_when(
              .data$alpha > .data$p ~ 1,
              .data$alpha <= .data$p ~ 0
            ))



        } else {
          # --------------------------------------------------------------------
          # free frequency fits test
          # --------------------------------------------------------------------

          if (!("none" %in% mcc) & length(mcc) > 0) message("No fixed frequencies were found. No multiple comparison correction will be applied...")

          # --------------------------------------------------------------------
          # join permutations and observed r2 and determine crit value & p value
          # --------------------------------------------------------------------
          bosc$tests$sinmod[[iLevel]][[iTest]]$results <- bosc$data[[iLevel]]$surrogate$sinmod %>%
            # merge observed and surrogate data values
            dplyr::left_join(bosc$data[[iLevel]]$real$sinmod, by = c(group_vars, "term"), suffix = c("_surr", "_observed")) %>%
            # reduce dataset to relevant vars
            dplyr::filter(.data$term == "f") %>%
            dplyr::select(.data$estimate_observed, .data$r2_surr, .data$r2_observed) %>%
            dplyr::ungroup() %>%
            # add alpha levels
            dplyr::left_join(as.data.frame(alpha), copy = TRUE, by = character()) %>%
            # for each alpha level and each group var level, get critical value
            dplyr::group_by_at(c(group_vars, "alpha")) %>%
            dplyr::mutate(
              n_surrogates = max(.data$n_surr),
              crit_value = unname(stats::quantile(.data$r2_surr, probs = 1 - alpha, na.rm = TRUE)), # is na.rm = TRUE causing any harm here??
              p = 1 - stats::ecdf(.data$r2_surr)(.data$r2_observed)
            ) %>%
            # get rid of duplicate lines, only keep one row per fit
            dplyr::distinct(.data$estimate_observed, .keep_all = TRUE) %>%
            # get rid of unused columns
            dplyr::select(-.data$n_surr, -.data$r2_surr) %>%
            # restructure dataset
            dplyr::relocate(c(.data$estimate_observed, .data$r2_observed), .before = .data$n_surrogates) %>%
            # significance variable
            dplyr::mutate(sig = dplyr::case_when(
              .data$r2_observed > .data$crit_value ~ 1,
              .data$r2_observed <= .data$crit_value ~ 0
            ))
        }
      }
    }
  }


  # add executed command to history
  bosc$hist <- paste0(bosc$hist, "test_")

  if (verbose == TRUE) message("\nTest completed.")
  return(bosc)
}

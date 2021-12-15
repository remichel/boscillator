#' test_fft
#'
#' @description \code{test_fft}
#' Performs statistical test on frequency spectra obtained from fft_bosc.
#'
#' @param bosc BOSC-Object
#' @param levels Which levels of data need to be padded? Use "ss" for single subject and "ga" for grand average. Concatenate multiple levels with "-", e.g. "ss-ga"
#' @param tests  Which test to perform?
#' @param alpha Vector of alpha levels to apply. If you want to use multiple alpha levels, make sure to set mcc to "none"
#' @param overwrite defaults to F
#' @param verbose defaults to T
#' @param mcc choose between all methods available for stats::p.adjust, e.g."bonferroni", "fdr", and "maxfreq" or "none". if != "none", the first entry in alpha will be considered the family wise alpha error and all other entries in alpha will be ignored
#'
#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @export test_fft
#' @name test_fft
#'
#' @examples
#' bosc = simulate_experiment()
#' bosc = generate_surrogates(bosc)
#' bosc = fft_bosc(bosc)
#' bosc = test_fft(bosc, levels = "ga", tests = "amp-complex")
#'
test_fft <- function(bosc,
                     levels = c("ss", "merged", "ga"),
                     tests = c("amp", "complex", "phase"),
                     alpha = .05,
                     mcc = c("bonferroni", "fdr", "maxfreq"),
                     overwrite = FALSE,
                     verbose = TRUE) {

  # get levels
  if (!is.character(levels)) {
    stop("Argument levels must be a character.")
  }
  # get tests
  if (!is.character(tests)) {
    stop("Argument levels must be a character.")
  }


  if (verbose == TRUE) message("Starting tests...")

  # get MCCs
  if (!is.character(mcc)) {
    stop("Argument mcc must be a character.")
  } else {
    if (length(mcc) > 0 & mcc[1] != "none") {
      message(
        "Multiple correction methods ", paste(mcc, collapse = " & "),
        " were chosen. All alpha values except ", alpha[1],
        " will be ignored. \nInstead, ", alpha[1],
        " will be considered the desired family-wise alpha level for all MCC..\n"
      )
      alpha <- alpha[1]
    }
  }


  # loop through all conditions
  for (iLevel in levels) {
    if (verbose == TRUE) message("\nTest on ", iLevel, " level...\n")

    for (iTest in tests) {


      # check if required data exists
      if (iTest == "ss" | iTest == "ga") {
        if (is.null(bosc$data[[iLevel]]$real$fft) | is.null(bosc$data[[iLevel]]$surrogate$fft)) {
          if (verbose == TRUE) {
            message(paste(
              "No data found in ", iLevel,
              ".\nWill continue with next Type/Level..."
            ))
          }
          next
        }
      } else {
        if (is.null(bosc$data$ss$real$fft) | is.null(bosc$data$ss$surrogate$fft)) {
          if (verbose == TRUE) message(paste("No data found in ss.\nWill continue with next Type/Level..."))
          next
        }
      }

      # check if test data exists
      if (!is.null(bosc$tests$fft[[iLevel]][[iTest]])) {
        if (overwrite == TRUE) {
          if (verbose == TRUE) message("Test already exists. Will overwrite...")
        } else {
          warning("Test already exists. Will skip to next dataset without performing the FFT...")
          next
        }
      }



      # Perform test
      if (iTest == "amp") {


        # get correct group vars depending on the analysis level
        group_vars <- "f"
        # for surrogate data, group by n_surr
        if (iLevel == "ss") {
          group_vars <- c(group_vars, "subj")
        }


        if (verbose == TRUE) {
          message(
            "Comparing Observed FFT Amplitudes against Permutations (alpha = ",
            paste(alpha, collapse = ", "), ")..."
          )
        }


        bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$data[[iLevel]]$surrogate$fft %>%
          dplyr::group_by(.data$n_surr) %>%
          # add observed amplitudes in a separate column to the surrogate dataset
          dplyr::mutate(observed = !!bosc$data[[iLevel]]$real$fft$amp) %>%
          dplyr::ungroup() %>%
          # add all alpha levels
          dplyr::left_join(as.data.frame(alpha), copy = TRUE, by = character()) %>%
          # group by all grouping vars (e.g. frequency and subject) and alpha levels to perform tests on each of these groups separately
          dplyr::group_by_at(c(group_vars, "alpha")) %>%
          # based on alpha, find the critical values for each group within the surrogated amplitudes, extract the p-value and save the observed amplitude again (otherwise its lost after summarize)
          dplyr::summarize(
            crit_value = unname(stats::quantile(.data$amp, probs = 1 - alpha, na.rm = T)), # is na.rm = T causing any harm here??
            p = 1 - stats::ecdf(.data$amp)(.data$observed),
            observed = .data$observed
          ) %>%
          # eliminate dublicates
          dplyr::distinct() %>%
          dplyr::ungroup()


        if ("maxfreq" %in% mcc) {


          # get correct group vars depending on the analysis level
          group_vars <- NULL
          # for surrogate data, group by n_surr
          if (iLevel == "ss") {
            group_vars <- c(group_vars, "subj")
          }


          # get p values for max freq approach
          maxfreq <- bosc$data[[iLevel]]$surrogate$fft %>%
            dplyr::group_by(.data$n_surr) %>%
            # add observed amplitudes in a separate column to the surrogate dataset
            dplyr::mutate(observed = !!bosc$data[[iLevel]]$real$fft$amp) %>%
            dplyr::ungroup() %>%
            # add all alpha levels
            dplyr::left_join(as.data.frame(alpha), copy = TRUE, by = character()) %>%
            # group by all grouping vars (e.g. frequency and subject), alpha levels AND n_surr to extract the max amplitude from the surrogated datasets
            dplyr::group_by_at(c(group_vars, "n_surr", "alpha")) %>%
            # for each group, extract the maximum amplitude observed in the surrogates
            dplyr::summarise(
              max = max(.data$amp),
              observed = .data$observed,
              f = .data$f
            ) %>%
            # group by all grouping vars (e.g. frequency and subject), alpha levels, and extract the critical value from the max amplitude distribution for each group
            dplyr::group_by_at(c(group_vars, "alpha")) %>%
            dplyr::summarize(
              crit_value = unname(stats::quantile(.data$max, probs = 1 - alpha, na.rm = T)), # is na.rm = T causing any harm here??
              p = 1 - stats::ecdf(.data$max)(.data$observed),
              observed = .data$observed,
              f = .data$f
            ) %>%
            dplyr::relocate(.data$f, .before = .data$alpha) %>%
            # eliminate dublicates
            dplyr::distinct()

          # add max freq p values to results table
          bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$tests$fft[[iLevel]][[iTest]]$results %>%
            dplyr::mutate(maxfreq = !!maxfreq$p)


          # if no other MCC method needs to be applied, clean dataset now
          if (length(mcc) == 1) {

            # pivot longer
            bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$tests$fft[[iLevel]][[iTest]]$results %>%
              dplyr::group_by_at(c(group_vars, "alpha")) %>%
              dplyr::rename(uncorrected = .data$p) %>%
              # bring dataset into long format to have one row per result
              tidyr::pivot_longer(
                cols = c(.data$uncorrected, !!mcc),
                names_to = "mcc_method",
                values_to = "p"
              ) %>%
              dplyr::select(-.data$crit_value) # delecte crit value, as it refers to uncorrected alpha and is thus misleading if mcc is used
          }
        }



        #  if other MCC method needs to be applied, do it now
        if (length(mcc) > 0 & !("none" %in% mcc)) {

          # exclude maxfreq from the "to-be-computed" list, but keep mcc for the pivot longer cols argument
          if ("maxfreq" %in% mcc) {
            padjust_list <- mcc[-which(mcc == "maxfreq")]
          } else {
            padjust_list <- mcc
          }

          # get correct group vars depending on the analysis level
          group_vars <- NULL
          # for surrogate data, group by n_surr
          if (iLevel == "ss") {
            group_vars <- c(group_vars, "subj")
          }

          # apply all MCC corrections
          for (iMCC in padjust_list) {
            bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$tests$fft[[iLevel]][[iTest]]$results %>%
              dplyr::group_by_at(c(group_vars, "alpha")) %>%
              # apply all desired MCCs and save them in separate columns named after the correction method
              dplyr::mutate(!!iMCC := stats::p.adjust(.data$p, method = !!iMCC))
          }

          # convert to long format and erase unused columns
          bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$tests$fft[[iLevel]][[iTest]]$results %>%
            dplyr::group_by_at(c(group_vars, "alpha")) %>%
            dplyr::rename(uncorrected = .data$p) %>%
            # bring dataset into long format to have one row per result
            tidyr::pivot_longer(
              cols = c(.data$uncorrected, !!mcc),
              names_to = "mcc_method",
              values_to = "p"
            ) %>%
            dplyr::select(-.data$crit_value) # delecte crit value, as it refers to uncorrected alpha and is thus misleading if mcc is used
        }


        # add significance column
        bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$tests$fft[[iLevel]][[iTest]]$results %>%
          dplyr::mutate(sig = dplyr::case_when(
            .data$alpha > .data$p ~ 1,
            .data$alpha <= .data$p ~ 0
          ))
      } else if (iTest == "complex") {
        if (iLevel == "ga" | iLevel == "ss") {
          if (verbose == TRUE) message("Note: Complex vector analysis needs to be performed with level = merged. Will skip and proceed with next test...")
          next
        } else if (iLevel == "merged") {
          if (verbose == TRUE) message("Compare FFT complex vectors against permutations (alpha = ", paste(alpha, collapse = ", "), ") ...")
        }

        # save single surrogate data for 2d plot
        bosc$tests$fft[[iLevel]][[iTest]]$data <- bosc$data$ss$surrogate$fft %>%
          dplyr::group_by(.data$n_surr) %>%
          # add observed complex vectors to surrogate dataset
          dplyr::mutate(observed = !!bosc$data$ss$real$fft$complex) %>%
          dplyr::ungroup() %>%
          dplyr::group_by(.data$n_surr, .data$f) %>%
          # summarize complex vectors for observed and surrogate datasets
          dplyr::summarize(
            observed = mean(.data$observed),
            surrogate = mean(.data$complex)
          ) %>%
          # extract observed vector length and directions
          dplyr::mutate(
            observed_length = Mod(.data$observed),
            surrogate_length = Mod(.data$surrogate),
            observed_phase = Arg(.data$observed),
            surrogate_phase = Arg(.data$surrogate)
          ) %>%
          dplyr::ungroup()




        # determine critical vector length for each frequency
        bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$tests$fft[[iLevel]][[iTest]]$data %>%
          # add all desired alpha levels
          dplyr::left_join(as.data.frame(alpha), copy = TRUE, by = character()) %>%
          dplyr::group_by(.data$f, .data$alpha) %>%
          # determine critical vector length within surrogated datasets and determine p value of observed length
          dplyr::summarize(
            crit_length = unname(stats::quantile(.data$surrogate_length, probs = 1 - alpha, na.rm = T)), # does it cause harm here?
            p = 1 - stats::ecdf(.data$surrogate_length)(.data$observed_length),
            observed_length = .data$observed_length
          ) %>%
          # eliminate duplicates
          dplyr::distinct() %>%
          dplyr::relocate(.data$alpha, .after = .data$f) %>%
          dplyr::ungroup()



        if ("maxfreq" %in% mcc) {

          # get p values for max freq approach
          maxfreq <- bosc$tests$fft[[iLevel]][[iTest]]$data %>%
            # add all desired alpha levels
            dplyr::left_join(as.data.frame(alpha), copy = TRUE, by = character()) %>%
            dplyr::group_by(.data$n_surr, .data$alpha) %>%
            # determine max vector length within each surrogated dataset
            dplyr::summarise(
              max = max(.data$surrogate_length),
              observed_length = .data$observed_length,
              f = .data$f
            ) %>%
            dplyr::group_by(.data$alpha) %>%
            # determine critical maximum vector length within surrogated datasets and determine p value of observed length
            dplyr::summarize(
              crit_length = unname(stats::quantile(.data$max, probs = 1 - alpha, na.rm = T)), # does it cause harm here?
              p = 1 - stats::ecdf(.data$max)(.data$observed_length),
              observed_length = .data$observed_length,
              f = .data$f
            ) %>%
            # eliminate duplicates
            dplyr::distinct() %>%
            dplyr::relocate(.data$f, .data$alpha, .before = .data$observed_length)

          # add max freq p values to results table
          bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$tests$fft[[iLevel]][[iTest]]$results %>%
            dplyr::mutate(maxfreq = !!maxfreq$p)


          # if no other mcc is applied, clean dataset now
          if (length(mcc) == 1) {

            # pivot longer
            bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$tests$fft[[iLevel]][[iTest]]$results %>%
              dplyr::group_by(.data$alpha) %>%
              dplyr::rename(uncorrected = .data$p) %>%
              # bring dataset into long format to have one row per result
              tidyr::pivot_longer(
                cols = c(.data$uncorrected, !!mcc),
                names_to = "mcc_method",
                values_to = "p"
              ) %>%
              dplyr::select(-.data$crit_value) # misleading if mcc is used, as it refers to uncorrected alpha
          }
        }




        # MCC
        if (length(mcc) > 0 & !("none" %in% mcc)) {


          # exclude maxfreq from the "to-be-computed" list, but keep mcc for the pivot longer cols argument
          if ("maxfreq" %in% mcc) {
            padjust_list <- mcc[-which(mcc == "maxfreq")]
          } else {
            padjust_list <- mcc
          }

          # apply all MCC corrections
          for (iMCC in padjust_list) {
            bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$tests$fft[[iLevel]][[iTest]]$results %>%
              dplyr::group_by(.data$alpha) %>%
              # apply all desired MCCs and save them in separate columns named after the correction method
              dplyr::mutate(!!iMCC := stats::p.adjust(.data$p, method = !!iMCC))
          }

          # convert to long format and erase unused columns
          bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$tests$fft[[iLevel]][[iTest]]$results %>%
            dplyr::group_by(.data$alpha) %>%
            dplyr::rename(uncorrected = .data$p) %>%
            # bring dataset into long format to have one row per result
            tidyr::pivot_longer(
              cols = c(.data$uncorrected, !!mcc),
              names_to = "mcc_method",
              values_to = "p"
            ) %>%
            dplyr::select(-.data$crit_length) # misleading if mcc is used, as it refers to uncorrected alpha
        }

        # add significance column
        bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$tests$fft[[iLevel]][[iTest]]$results %>%
          dplyr::mutate(sig = dplyr::case_when(
            .data$alpha > .data$p ~ 1,
            .data$alpha <= .data$p ~ 0
          ))
      } else if (iTest == "phase") {


        # skip grand average level
        if (iLevel == "ga" | iLevel == "merged") {
          if (verbose == TRUE) message("Note: Phase analysis needs to be performed on single subject data. Will skip and proceed with next test...")
          next
        }

        if (verbose == TRUE) message("Rayleigh test on phase angles (alpha = ", paste(alpha, collapse = ", "), ")...")

        # rayleigh test
        bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$data$ss$real$fft %>%
          dplyr::ungroup() %>%
          # add all desired alpha levels
          dplyr::left_join(as.data.frame(alpha), copy = T, by = character()) %>%
          dplyr::group_by(.data$f, .data$alpha) %>%
          # transform phase into radians and determine mean phase, compute rayleigh test statistic and extract p val
          dplyr::summarize(
            mean_phase = circular::mean.circular(circular::circular(.data$phase, units = c("radians"))),
            statistic = circular::rayleigh.test(circular::circular(.data$phase, units = c("radians")), mu = NULL)$statistic,
            p = circular::rayleigh.test(circular::circular(.data$phase, units = c("radians")), mu = NULL)$p.value
          ) %>%
          dplyr::ungroup()


        # MCC
        if (length(mcc) > 0 & !("none" %in% mcc)) {

          # check if maxfreq is in mcclist and exclude it
          if ("maxfreq" %in% mcc) {
            phase_mcc <- mcc[-which(mcc == "maxfreq")]
            message("Max Freq MCC method not applicable to phase test. Will be skipped...")
          } else {
            phase_mcc <- mcc
          }

          # apply all MCC corrections
          for (iMCC in phase_mcc) {
            bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$tests$fft[[iLevel]][[iTest]]$results %>%
              dplyr::group_by(.data$alpha) %>%
              # apply all desired MCCs and save them in separate columns named after the correction method
              dplyr::mutate(!!iMCC := stats::p.adjust(.data$p, method = !!iMCC))
          }

          # convert to long format and erase unused columns
          bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$tests$fft[[iLevel]][[iTest]]$results %>%
            dplyr::group_by(.data$alpha) %>%
            dplyr::rename(uncorrected = .data$p) %>%
            # bring dataset into long format to have one row per result
            tidyr::pivot_longer(
              cols = c(.data$uncorrected, !!phase_mcc),
              names_to = "mcc_method",
              values_to = "p"
            )
        }

        # add significance column

        bosc$tests$fft[[iLevel]][[iTest]]$results <- bosc$tests$fft[[iLevel]][[iTest]]$results %>%
          dplyr::mutate(sig = dplyr::case_when(
            .data$alpha > .data$p ~ 1,
            .data$alpha <= .data$p ~ 0
          ))
      }
    }
  }

  # add executed command to history
  bosc$hist <- paste0(bosc$hist, "test_")

  if (verbose == TRUE) message("\nTest completed.")
  return(bosc)
}

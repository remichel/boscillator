#' Test the boscillator function 'simulate_experiment' that simulates the raw dataset of a dense
#' sampling study.

###################################################################################################
# TO DO:
# - test for all cases of parameters, test_matrix dementsprechend erweitern
#   (trend & transient!)
# - test_matrix schöner & values per loop unpacken
# -> vielleicht sinnvoller als Liste anstatt data.frame? Dann muss tidyverse auch nicht extra laden
###################################################################################################
source("test_values.R") # sources test matrix
source("defaults.R") # sources defaults (here only n_sub, n_timepoints & n_trials & sfreq used)

# loop through test_matrix rows for all other parameters
for (i in 1:nrow(test_matrix)){
  test_n_sub                      <- test_matrix$n_sub[i]
  test_n_timepoints               <- test_matrix$n_timepoints[i]
  test_n_trials                   <- test_matrix$n_trials[i]
  test_sfreq                      <- test_matrix$sfreq[i]
  test_osc_params                 <- test_matrix$osc_params[[i]]
  test_phase_jitter               <- test_matrix$phase_jitter[[i]]
  test_amplitude_jitter           <- test_matrix$amplitude_jitter[[i]]
  test_freq_jitter                <- test_matrix$freq_jitter[[i]]
  test_intercept_jitter           <- test_matrix$intercept_jitter[i]
  test_transient                  <- test_matrix$transient[i]
  test_transient_expModel_params  <- test_matrix$transient_expModel_params[[i]]
  test_trend                      <- test_matrix$trend[i]
  test_trend_linModel_params      <- test_matrix$trend_linModel_params[[i]]
  test_trend_polyModel_params     <- test_matrix$trend_polyModel_params[[i]]
  test_seed_num                   <- test_matrix$seed_num

  # test class
  test_that("function returns an object of the 'bosc' class & logs simulation in history of object", {
    bosc <- simulate_experiment()
    expect_identical(class(bosc), "BOSC-Object")
  })

  # test aggregate function application
  test_that("aggregating to ss/ga level", {
    # no aggregate
    bosc <- simulate_experiment(aggregate = F)
    expect_no_match(bosc$hist, "aggregate")
    # data is aggregated (as set in defaults)
    bosc <- simulate_experiment(aggregate = T)
    expect_match(bosc$hist, "_aggregate_")
    # aggregation itself is tested in test_aggregate
  })


  # test n_sub, n_timepoints, n_trials & spatial frequency parameters
  test_that("parameter n_sub, n_timepoints, n_trials and sfreq", {
    bosc <- simulate_experiment(test_n_sub, n_timepoints = test_n_timepoints,
                                n_trials = test_n_trials, sfreq = test_sfreq)
    expect_identical(bosc$data$single_trial$real$spec$n_sub, test_n_sub)
    expect_identical(bosc$timepoints, seq(0, (test_n_timepoints - 1) / test_sfreq, 1 / test_sfreq))
    expect_identical(bosc$data$single_trial$real$spec$n_trials, test_n_trials)
    expect_identical(bosc$data$single_trial$real$spec$sfreq, test_sfreq)
    #check, if data contains correct subject numbers
    expect_true(all(c(1:test_n_sub) %in% bosc$data$single_trial$real$data$subj))
    #check, if trials are correct
    expect_true(all(c(1:test_n_trials) %in% bosc$data$single_trial$real$data$trial))
    #check, if single trial data is a grid of subjects x n_trials x timepoints
    expect_equal(nrow(bosc$data$single_trial$real$data), test_n_sub * test_n_trials * test_n_timepoints)
    #check, if single trial data column 'time' contains correct timepoints according to spatial frequency
    expect_equal(bosc$data$single_trial$real$data$time,
                 sort(rep(seq(0, (test_n_timepoints - 1) / test_sfreq, 1 / test_sfreq), test_n_trials * test_n_sub)))
    #check, if parameters are combined correctly -> unique combination
    expect_equal(unique(xtabs(~bosc$data$single_trial$real$data$subj +
                                bosc$data$single_trial$real$data$time + bosc$data$single_trial$real$data$trial)),1)
  })

  # test oscillation parameters
  test_that("parameter osc_params", {
    bosc <- simulate_experiment(osc_params = test_osc_params)
    expect_identical(bosc$data$single_trial$real$spec$osc_params, test_osc_params)
    #################################################################################################
    # TEST MISSING: verändert sich das Modell bei Veränderung der oscillation parameters
    #               sinngemäß & quantifizierbar richtig?
    #               mögliche Lösungen:
    #               a) 1 Fall festlegen; anhand dessen die Veränderung der Daten durch Veränderung der
    #                  Parameter mit tatsächlichen Zahlenwerten quantifizieren, d.h. tatsächliche
    #                  Zahlen zum Testen verwenden
    #               b) Gegen das default-Modell basierend auf den default-Werten testen und nur einen
    #                  heuristischen Vergleich anstellen
    #                  (ändert die Veränderung eines Parameters das Modell auf entsprechend richtige Weise?)
    #################################################################################################
  })

  # test phase jitter parameter
  test_that("parameter phase_jitter", {
    bosc <- simulate_experiment(phase_jitter = test_phase_jitter)
    expect_identical(bosc$data$single_trial$real$spec$phase_jitter, test_phase_jitter)
    #################################################################################################
    # TEST MISSING: verändert der phase jitter die Daten sinngemäß & quantifizierbar richtig?
    #               mögliche Lösungen:
    #               a) 1 Fall festlegen; anhand dessen die Veränderung der Daten durch den jitter mit
    #                  tatsächlichen Zahlenwerten quantifizieren, d.h. tatsächliche
    #                  Zahlen zum Testen verwenden
    #               b) Gegen das default-Modell basierend auf den default-Werten testen und nur einen
    #                  heuristischen Vergleich anstellen
    #                  (z.B. führt ein Modell mit phase jitter zu stärker verschobenen Phasen
    #                  im Vergleich zu dem auf den defaults basierenden Modell)
    #################################################################################################
  })

  # test amplitude jitter parameter
  test_that("parameter amplitude_jitter", {
    bosc <- simulate_experiment(amplitude_jitter = test_amplitude_jitter)
    expect_identical(bosc$data$single_trial$real$spec$amplitude_jitter, test_amplitude_jitter)
    #################################################################################################
    # TEST MISSING: verändert der amplitude jitter die Daten sinngemäß & quantifizierbar richtig?
    #               mögliche Lösungen:
    #               a) 1 Fall festlegen; anhand dessen die Veränderung der Daten durch den jitter mit
    #                  tatsächlichen Zahlenwerten quantifizieren, d.h. tatsächliche
    #                  Zahlen zum Testen verwenden
    #               b) Gegen das default-Modell basierend auf den default-Werten testen und nur einen
    #                  heuristischen Vergleich anstellen
    #                  (z.B. führt ein Modell mit amplitude jitter zu stärker unterschiedlich hohen
    #                  Amplituden im Vergleich mit den Amplituden des Modells, das auf den
    #                  defaults basiert)
    #################################################################################################
  })

  # test frequency jitter parameter
  test_that("parameter freq_jitter", {
    bosc <- simulate_experiment(freq_jitter = test_freq_jitter)
    expect_identical(bosc$data$single_trial$real$spec$freq_jitter, test_freq_jitter)
    #################################################################################################
    # TEST MISSING: verändert der frequency jitter die Daten sinngemäß & quantifizierbar richtig?
    #               mögliche Lösungen:
    #               a) 1 Fall festlegen; anhand dessen die Veränderung der Daten durch den jitter mit
    #                  tatsächlichen Zahlenwerten quantifizieren, d.h. tatsächliche
    #                  Zahlen zum Testen verwenden
    #               b) Gegen das default-Modell basierend auf den default-Werten testen und nur einen
    #                  heuristischen Vergleich anstellen
    #                  (z.B. führt ein Modell mit frequency jitter zu stärker verstreuten Frequenzen
    #                  im Vergleich zu den Frequenzen des Modells, das auf den defaults basiert)
    #################################################################################################
  })

  # test intercept jitter parameter
  test_that("parameter intercept_jitter", {
    bosc <- simulate_experiment(intercept_jitter = test_intercept_jitter)
    expect_identical(bosc$data$single_trial$real$spec$intercept_jitter, test_intercept_jitter)
    #################################################################################################
    # TEST MISSING: verändert der intercept jitter die Daten sinngemäß & quantifizierbar richtig?
    #               mögliche Lösungen:
    #               a) 1 Fall festlegen; anhand dessen die Veränderung der Daten durch den jitter mit
    #                  tatsächlichen Zahlenwerten quantifizieren, d.h. tatsächliche
    #                  Zahlen zum Testen verwenden
    #               b) Gegen das default-Modell basierend auf den default-Werten testen und nur einen
    #                  heuristischen Vergleich anstellen
    #                  (z.B. führt ein Modell mit intercept jitter zu stärker verstreuten intercepts
    #                  im Vergleich zu den intercepts des Modells, das auf den defaults basiert)
    #################################################################################################
  })

  # test transient parameters
  test_that("parameter transient", {
    bosc <- simulate_experiment(transient = test_transient, transient_expModel_params = test_transient_expModel_params)
    expect_identical(bosc$data$single_trial$real$spec$transient, test_transient)
    expect_identical(bosc$data$single_trial$real$spec$transient_expModel_params, test_transient_expModel_params)
    #################################################################################################
    # TEST MISSING: verändert der transient die Daten sinngemäß & quantifizierbar richtig?
    #               mögliche Lösungen:
    #               a) 1 Fall festlegen; anhand dessen die Veränderung der Daten durch den transient mit
    #                  tatsächlichen Zahlenwerten quantifizieren, d.h. tatsächliche
    #                  Zahlen zum Testen verwenden
    #               b) Gegen das default-Modell basierend auf den default-Werten in beide Richtungen
    #                  testen und nur einen heuristischen Vergleich anstellen
    #                  (z.B. ist der positive exponentielle transient im Modell mit Fall
    #                  "positiver-exponentieller-transient" größer als im Modell basierend auf
    #                   den defaults)
    #################################################################################################
  })

  # test trend parameters
  test_that("parameter trend", {
    bosc <- simulate_experiment(trend = test_trend, trend_linModel_params = test_trend_linModel_params,
                                trend_polyModel_params = test_trend_polyModel_params)
    expect_identical(bosc$data$single_trial$real$spec$trend, test_trend)
    expect_identical(bosc$data$single_trial$real$spec$trend_linModel_params, test_trend_linModel_params)
    expect_identical(bosc$data$single_trial$real$spec$trend_polyModel_params, test_trend_polyModel_params)
    #################################################################################################
    # TEST MISSING: verändert der trend die Daten sinngemäß & quantifizierbar richtig?
    #               mögliche Lösungen:
    #               a) 1 Fall festlegen; anhand dessen die Veränderung der Daten durch den trend mit
    #                  tatsächlichen Zahlenwerten quantifizieren, d.h. tatsächliche
    #                  Zahlen zum Testen verwenden
    #               b) Gegen das default-Modell basierend auf den default-Werten in beide Richtungen
    #                  testen und nur einen heuristischen Vergleich anstellen
    #                  (z.B. ist der positive lineare trend im Modell mit Fall
    #                  "positiver-linearer-trend" größer als im Modell basierend auf den defaults)
    #################################################################################################
  })

  # test seed number parameter
  test_that("parameter seed_num", {
    bosc <- simulate_experiment(seed_num = test_seed_num)
    expect_identical(bosc$data$single_trial$real$spec$seed_num, test_seed_num)
  })
}

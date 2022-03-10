#' Test the boscillator function 'fft_bosc' that performs a fft on the time courses of a dense
#' sampling study.
###################################################################################################
# TO DO:
# - Test ob Amp & Phase richtig sind? (ggf. mit fixed object?)
###################################################################################################
source("defaults.R") # sources defaults

test_that("function call fft added to history of object", {
  fftd_bosc <- fft_bosc(no_model_bosc)
  expect_match(fftd_bosc$hist, "_fft_")
})

test_that("fft real data", {
  fftd_bosc <- fft_bosc(no_model_bosc)
  # no padding -> time series of length 20 (def_n_timepoints)
  # ss
  expect_equal(nrow(fftd_bosc$data$ss$real$fft),
               length(seq(def_s_freq/def_n_timepoints, def_s_freq/2, def_s_freq/def_n_timepoints))*def_n_sub)
  expect_equal(as.numeric(fftd_bosc$data$ss$real$fft$f), rep(def_bins, def_n_sub))
  # ga
  expect_equal(nrow(fftd_bosc$data$ga$real$fft),
               length(seq(def_s_freq/def_n_timepoints, def_s_freq/2, def_s_freq/def_n_timepoints)))
  expect_equal(as.numeric(fftd_bosc$data$ga$real$fft$f), def_bins)
})

test_that("fft surrogates", {
  fftd_bosc <- fft_bosc(no_model_bosc)
  # no padding -> time series of length 20 (def_n_timepoints)
  # ss
  expect_equal(nrow(fftd_bosc$data$ss$surrogate$fft),
               length(seq(def_s_freq/def_n_timepoints, def_s_freq/2, def_s_freq/def_n_timepoints))
               *def_n_sub*def_n_surr)
  expect_equal(as.numeric(fftd_bosc$data$ss$surrogate$fft$f), rep(def_bins, def_n_sub*def_n_surr))

  # ga
  expect_equal(nrow(fftd_bosc$data$ss$surrogate$fft),
               length(seq(def_s_freq/def_n_timepoints, def_s_freq/2, def_s_freq/def_n_timepoints))*def_n_surr)
  expect_equal(as.numeric(fftd_bosc$data$ss$surrogate$fft$f), rep(def_bins, def_n_surr))
})

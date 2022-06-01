#' Test the boscillator function 'fft_bosc' that performs a fft on the time courses of a dense
#' sampling study.

source("defaults.R")
load("surr_bosc.RData")
load("fftcomp_bosc.RData")

test_that("function call fft added to history of object", {
  fftd_bosc <- fft_bosc(no_model_bosc, verbose = F)
  expect_match(fftd_bosc$hist, "_fft_")
})

test_that("fft real data", {
  fftd_bosc <- fft_bosc(no_model_bosc, verbose = F)
  # no padding -> time series of length 20 (def_n_timepoints)
  # ss
  expect_equal(nrow(fftd_bosc$data$ss$real$fft),
               length(seq(def_sfreq/def_n_timepoints, def_sfreq/2, def_sfreq/def_n_timepoints))*def_n_sub)
  expect_equal(as.numeric(fftd_bosc$data$ss$real$fft$f), rep(def_bins, def_n_sub))
  # ga
  expect_equal(nrow(fftd_bosc$data$ga$real$fft),
               length(seq(def_sfreq/def_n_timepoints, def_sfreq/2, def_sfreq/def_n_timepoints)))
  expect_equal(as.numeric(fftd_bosc$data$ga$real$fft$f), def_bins)
})

test_that("fft surrogates", {
  fftd_bosc <- fft_bosc(no_model_bosc, verbose = F)
  # no padding -> time series of length 20 (def_n_timepoints)
  # ss
  expect_equal(nrow(fftd_bosc$data$ss$surrogate$fft),
               length(seq(def_sfreq/def_n_timepoints, def_sfreq/2, def_sfreq/def_n_timepoints))
               *def_n_sub*def_n_surr)
  expect_equal(as.numeric(fftd_bosc$data$ss$surrogate$fft$f), rep(def_bins, def_n_sub*def_n_surr))

  # ga
  expect_equal(nrow(fftd_bosc$data$ga$surrogate$fft),
               length(seq(def_sfreq/def_n_timepoints, def_sfreq/2, def_sfreq/def_n_timepoints))*def_n_surr)
  expect_equal(as.numeric(fftd_bosc$data$ga$surrogate$fft$f), rep(def_bins, def_n_surr))
})

test_that("fft merged", {
  fftd_bosc <- fft_bosc(no_model_bosc, verbose = F)
  # real
  expect_equal(nrow(fftd_bosc$data$merged$real$fft),
               length(seq(def_sfreq/def_n_timepoints, def_sfreq/2, def_sfreq/def_n_timepoints)))
  expect_equal(fftd_bosc$data$merged$real$fft$f, def_bins)
  # surrogate
  expect_equal(nrow(fftd_bosc$data$merged$surrogate$fft),
               length(seq(def_sfreq/def_n_timepoints, def_sfreq/2, def_sfreq/def_n_timepoints))*def_n_surr)
  expect_equal(fftd_bosc$data$merged$surrogate$fft$f, sort(rep(def_bins, def_n_surr)))
})

test_that("failsafe", {
  fftd = fft_bosc(surr_bosc, verbose = F)
  expect_equal(fftd, fftcomp_bosc)
})


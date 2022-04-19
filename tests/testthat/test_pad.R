#' Test the boscillator function 'pad_bosc' that pads the time courses of a dense
#' sampling study.

source("defaults.R")
load("surr_bosc.RData")
load("pad0_bosc.RData")
load("padme_bosc.RData")

test_that("function call pad added to history of object", {
  pad_bosc <- pad_bosc(no_x_bosc, verbose = F)
  expect_match(pad_bosc$hist, "_padd_")
})

test_that("zero padding", {
  pad_bosc <- pad_bosc(no_x_bosc, n_pads = def_n_pads, method = "zero", verbose = F)
  # real data
  # ss
  expect_equal(nrow(pad_bosc$data$ss$real$data), def_n_sub * def_n_timepoints + def_n_pads * def_n_sub)
  # spot check subject 1
  expect_equal(pad_bosc$data$ss$real$data$hr[1:(def_n_pads/2)], rep(0, def_n_pads/2))
  expect_equal(pad_bosc$data$ss$real$data$hr
               [(def_n_pads/2 + def_n_timepoints + 1):(def_n_pads + def_n_timepoints)],
               rep(0, def_n_pads/2))
  # ga
  expect_equal(nrow(pad_bosc$data$ga$real$data), def_n_timepoints + def_n_pads)
  expect_equal(pad_bosc$data$ga$real$data$hr[1:(def_n_pads/2)], rep(0, def_n_pads/2))
  expect_equal(pad_bosc$data$ga$real$data$hr[(def_n_pads/2 + def_n_timepoints + 1):(def_n_pads + def_n_timepoints)], rep(0, def_n_pads/2))
  # surrogate data
  # ss
  expect_equal(nrow(pad_bosc$data$ss$surrogate$data),
               def_n_sub * def_n_timepoints * def_n_surr + def_n_pads * def_n_sub * def_n_surr)
  # spot check subject 1 surrogate 1
  expect_equal(pad_bosc$data$ss$surrogate$data$hr[1:(def_n_pads/2)], rep(0, def_n_pads/2))
  expect_equal(pad_bosc$data$ss$surrogate$data$hr
               [(def_n_pads/2 + def_n_timepoints + 1):(def_n_pads + def_n_timepoints)],
               rep(0, def_n_pads/2))
  # ga
  expect_equal(nrow(pad_bosc$data$ga$surrogate$data), def_n_surr * def_n_timepoints + def_n_pads * def_n_surr)
  # spot check surrogate 1
  expect_equal(pad_bosc$data$ga$surrogate$data$hr[1:(def_n_pads/2)], rep(0, def_n_pads/2))
  expect_equal(pad_bosc$data$ga$surrogate$data$hr
               [(def_n_pads/2 + def_n_timepoints + 1):(def_n_pads + def_n_timepoints)],
               rep(0, def_n_pads/2))
})


test_that("mean padding", {
  avg_pad_bosc <- pad_bosc(no_x_bosc, n_pads = 50, method = "mean", verbose = F)
  # real data
  # ss
  expect_equal(nrow(avg_pad_bosc$data$ss$real$data), def_n_sub * def_n_timepoints + def_n_pads * def_n_sub)
  # spot check subject 1
  avg <- mean(no_x_bosc$data$ss$real$data$hr[no_x_bosc$data$ss$real$data$subj == 1])
  expect_equal(avg_pad_bosc$data$ss$real$data$hr[1:(def_n_pads/2)], rep(avg, def_n_pads/2))
  expect_equal(avg_pad_bosc$data$ss$real$data$hr
               [(def_n_pads/2 + def_n_timepoints + 1):(def_n_pads + def_n_timepoints)],
               rep(avg, def_n_pads/2))
  # ga
  avg <- mean(no_x_bosc$data$ga$real$data$hr)
  expect_equal(nrow(avg_pad_bosc$data$ga$real$data), def_n_timepoints + def_n_pads)
  expect_equal(avg_pad_bosc$data$ga$real$data$hr[1:(def_n_pads/2)], rep(avg, def_n_pads/2))
  expect_equal(avg_pad_bosc$data$ga$real$data$hr[(def_n_pads/2 + def_n_timepoints + 1):(def_n_pads + def_n_timepoints)],
               rep(avg, def_n_pads/2))
  # surrogate data
  # ss
  expect_equal(nrow(avg_pad_bosc$data$ss$surrogate$data),
               def_n_sub * def_n_timepoints * def_n_surr + def_n_pads * def_n_sub * def_n_surr)
  # spot check subject 1 surrogate 1
  avg <- mean(no_x_bosc$data$ss$surrogate$data$hr[no_x_bosc$data$ss$surrogate$data$subj == 1 & no_x_bosc$data$ss$surrogate$data$n_surr == 1])
  expect_equal(avg_pad_bosc$data$ss$surrogate$data$hr[1:(def_n_pads/2)], rep(avg, def_n_pads/2))
  expect_equal(avg_pad_bosc$data$ss$surrogate$data$hr
               [(def_n_pads/2 + def_n_timepoints + 1):(def_n_pads + def_n_timepoints)],
               rep(avg, def_n_pads/2))
  # ga
  expect_equal(nrow(avg_pad_bosc$data$ga$surrogate$data), def_n_surr * def_n_timepoints + def_n_pads * def_n_surr)
  # spot check surrogate 1
  avg <- mean(no_x_bosc$data$ga$surrogate$data$hr[no_x_bosc$data$ga$surrogate$data$n_surr == 1])
  expect_equal(avg_pad_bosc$data$ga$surrogate$data$hr[1:(def_n_pads/2)], rep(avg, def_n_pads/2))
  expect_equal(avg_pad_bosc$data$ga$surrogate$data$hr
               [(def_n_pads/2 + def_n_timepoints + 1):(def_n_pads + def_n_timepoints)],
               rep(avg, def_n_pads/2))
})

test_that("failsafe", {
  pad0 = pad_bosc(surr_bosc, method = "zero", n_pads = 50, verbose = F)
  padme = pad_bosc(surr_bosc, method = "mean", n_pads = 50, verbose = F)
  # zero
  expect_equal(pad0$data$ss$real$data$hr, pad0_bosc$data$ss$real$data$hr)
  expect_equal(pad0$data$ga$real$data$hr, pad0_bosc$data$ga$real$data$hr)
  expect_equal(pad0$data$ss$surrogate$data$hr, pad0_bosc$data$ss$surrogate$data$hr)
  expect_equal(pad0$data$ga$surrogate$data$hr, pad0_bosc$data$ga$surrogate$data$hr)
  # mean
  expect_equal(padme$data$ss$real$data$hr, padme_bosc$data$ss$real$data$hr)
  expect_equal(padme$data$ga$real$data$hr, padme_bosc$data$ga$real$data$hr)
  expect_equal(padme$data$ss$surrogate$data$hr, padme_bosc$data$ss$surrogate$data$hr)
  expect_equal(padme$data$ga$surrogate$data$hr, padme_bosc$data$ga$surrogate$data$hr)
})

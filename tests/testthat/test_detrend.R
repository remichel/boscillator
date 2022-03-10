#' Test the boscillator function 'detrend bosc' that detrends time courses of a dense
#' sampling study.
###################################################################################################
# TO DO:
# - add loop through test matrix to test detrend on different trends in data?
# - exponential detrending does not detrend real data, why?
# - more tests besides spot tests?

# requires package funtimes
###################################################################################################
source("defaults.R") # sources defaults

test_that("function call detrend added to history of object", {
  detr_bosc <- detrend_bosc(no_detr_bosc_lin)
  expect_match(detr_bosc$hist, "_detrend_")
})

test_that("detrending linear trend", {
  detr_bosc_lin <- detrend_bosc(no_detr_bosc_lin)
  # real data detrending
  # level ss - spot check of subject 1
  p <- notrend_test(detr_bosc_lin$data$ss$real$data[detr_bosc_lin$data$ss$real$data$subj == 1,]$hr)$p.value
  expect_gt(p, 0.05)
  # level ga
  expect_gt(notrend_test(detr_bosc_lin$data$ga$real$data$hr)$p.value, 0.05)

  # surrogate data detrending
  # level ss spot check of surrogate 1 of subject 1
  p <- notrend_test(detr_bosc_lin$data$ss$surrogate$data[detr_bosc_lin$data$ss$surrogate$data$subj == 1
                                                           & detr_bosc_lin$data$ss$surrogate$data$n_surr == 1,]$hr)$p.value
  expect_gt(p, 0.05)
  # level ga - spot check of surrogate 1
  p <- notrend_test(detr_bosc_lin$data$ga$surrogate$data[detr_bosc_lin$data$ga$surrogate$data$n_surr == 1,]$hr)$p.value
  expect_gt(p, 0.05)
})


test_that("detrending exponential trend", {
  detr_bosc_exp <- detrend_bosc(no_detr_bosc_exp)
  # real data detrending
  # level ss - spot check of subject 1
  p <- notrend_test(detr_bosc_exp$data$ss$real$data[detr_bosc_exp$data$ss$real$data$subj == 1,]$hr, test = "MK")$p.value
  expect_gt(p, 0.05)
  # level ga
  expect_gt(notrend_test(detr_bosc_exp$data$ga$real$data$hr, test = "MK")$p.value, 0.05)

  # surrogate data detrending
  # level ss - spot check of surrogate 1 of subject 1
  p <- notrend_test(detr_bosc_exp$data$ss$surrogate$data[detr_bosc_exp$data$ss$surrogate$data$subj == 1
                                                           & detr_bosc_exp$data$ss$surrogate$data$n_surr == 1,]$hr, test = "MK")$p.value
  expect_gt(p, 0.05)
  # level ga - spot check of surrogate 1
  p <- notrend_test(detr_bosc_exp$data$ga$surrogate$data[detr_bosc_exp$data$ga$surrogate$data$n_surr == 1,]$hr, test = "MK")$p.value
  expect_gt(p, 0.05)
})

#' Test the boscillator function 'pad_bosc' that pads the time courses of a dense
#' sampling study.

###################################################################################################
# TO DO:
# - expect equal sometimes throws error in mean padding
#   check because of small numerical differences -> still under construction
###################################################################################################

source("defaults.R") # sources defaults

test_that("function call pad added to history of object", {
  pad_bosc <- pad_bosc(no_x_bosc)
  expect_match(pad_bosc$hist, "_padd_")
})

test_that("zero padding", {
  pad_bosc <- pad_bosc(no_x_bosc, n_pads = def_n_pads, method = "zero")
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
  avg_pad_bosc <- pad_bosc(no_x_bosc, n_pads = 50, method = "mean")
  avg <- mean(no_x_bosc$data$ss$real$data$hr)
  # real data
  # ss
  expect_equal(nrow(avg_pad_bosc$data$ss$real$data), def_n_sub * def_n_timepoints + def_n_pads * def_n_sub)
  # spot check subject 1
  expect_equal(avg_pad_bosc$data$ss$real$data$hr[1:(def_n_pads/2)], rep(avg, def_n_pads/2))
  expect_equal(avg_pad_bosc$data$ss$real$data$hr
               [(def_n_pads/2 + def_n_timepoints + 1):(def_n_pads + def_n_timepoints)],
               rep(avg, def_n_pads/2))
  # ga
  expect_equal(nrow(avg_pad_bosc$data$ga$real$data), def_n_timepoints + def_n_pads)
  expect_equal(avg_pad_bosc$data$ga$real$data$hr[1:(def_n_pads/2)], rep(avg, def_n_pads/2))
  expect_equal(avg_pad_bosc$data$ga$real$data$hr[(def_n_pads/2 + def_n_timepoints + 1):(def_n_pads + def_n_timepoints)],
               rep(avg, def_n_pads/2))
  # surrogate data
  # ss
  expect_equal(nrow(avg_pad_bosc$data$ss$surrogate$data),
               def_n_sub * def_n_timepoints * def_n_surr + def_n_pads * def_n_sub * def_n_surr)
  # spot check subject 1 surrogate 1
  expect_equal(avg_pad_bosc$data$ss$surrogate$data$hr[1:(def_n_pads/2)], rep(avg, def_n_pads/2))
  expect_equal(avg_pad_bosc$data$ss$surrogate$data$hr
               [(def_n_pads/2 + def_n_timepoints + 1):(def_n_pads + def_n_timepoints)],
               rep(avg, def_n_pads/2))
  # ga
  expect_equal(nrow(avg_pad_bosc$data$ga$surrogate$data), def_n_surr * def_n_timepoints + def_n_pads * def_n_surr)
  # spot check surrogate 1
  expect_equal(avg_pad_bosc$data$ga$surrogate$data$hr[1:(def_n_pads/2)], rep(avg, def_n_pads/2))
  expect_equal(avg_pad_bosc$data$ga$surrogate$data$hr
               [(def_n_pads/2 + def_n_timepoints + 1):(def_n_pads + def_n_timepoints)],
               rep(0.5010714286, def_n_pads/2))
})

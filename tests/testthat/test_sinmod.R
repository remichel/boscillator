#' Test the boscillator function 'sinmod_bosc' that fits a sinusoidal model on the time courses of
#' a dense sampling study.
#'
source("defaults.R") # sources defaults

suppressWarnings(sin_bosc <- sinmod_bosc(no_model2_bosc, verbose = F))

test_that("function call sinmod added to history of object", {
  expect_match(sin_bosc$hist, "_sinmod_")
})

test_that("sinmod real data", {
  # ss
  expect_equal(nrow(sin_bosc$data$ss$real$sinmod), sinm_sub)
  # did all estimates yield good p values & right frequency (here:4)?
  for (i in sin_bosc$data$ss$real$sinmod$estimates) {
    for (j in nrow(i[[5]])){
      expect_lt(j, 0.001)
    }
    expect_equal(round(i$estimate[3]), sinm_f)
  }
  # ga
  expect_equal(nrow(sin_bosc$data$ga$real$sinmod), 1)
  # did all estimates yield good p values & right frequency (here:4)?
  for (i in nrow(sin_bosc$data$ga$real$sinmod$estimates)) {
    expect_lt(i[[5]], 0.001)
    expect_equal(round(i$estimate[3]), sinm_f)
  }
})

test_that("sinmod surrogate data", {
  # ss
  expect_equal(nrow(sin_bosc$data$ss$surrogate$sinmod), sinm_sub*sinm_surr)
  # did all estimates yield good p values?
  for (i in sin_bosc$data$ss$surrogate$sinmod$estimates) {
    for (j in nrow(i[[5]])){
      expect_lt(j, 0.001)
    }
  }
  # ga
  expect_equal(nrow(sin_bosc$data$ga$surrogate$sinmod), sinm_surr)
  # did all estimates yield good p values?
  for (i in sin_bosc$data$ga$surrogate$sinmod$estimates) {
    for (j in nrow(i[[5]])){
      expect_lt(j, 0.001)
    }
  }
})



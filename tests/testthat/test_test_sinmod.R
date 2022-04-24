#' Test the boscillator function 'test_sinmod' that performs a statistical test on sinusoidal models
#' obtained by function sinmod_bosc.

source("defaults.R")

# test sinmod results
suppressWarnings(testedsinmod <- test_sinmod(testsinmod_bosc, verbose = F))

test_that("function call _test_ added to history of object", {
  expect_match(testedsinmod$hist, "_test_")
})

test_that("sinmod test on ss level is done correctly", {
  # structure
  expect_equal(nrow(testedsinmod$tests$sinmod$ss$r2$results), sinm_sub)
  # all estimates close to simulated frequency (here: 6Hz) significant?
  for (i in 1:nrow(testedsinmod$tests$sinmod$ss$r2$results)) {
    estimate = as.numeric(testedsinmod$tests$sinmod$ss$r2$results[i, 3])
    if (abs(estimate - sinm_f) <= 0.25) { # 0.5Hz margin allowed
      expect_equal(as.numeric(testedsinmod$tests$sinmod$ss$r2$results[i, 8]), 1)
    }
  }
})

test_that("sinmod test on ga level is done correctly", {
 # is estimate close to simulated frequency & if yes, significant?
  if(abs(testedsinmod$tests$sinmod$ga$r2$results$estimate_observed - sinm_f) <= 0.5) { # 0.5Hz margin allowed
    expect_equal(testedsinmod$tests$sinmod$ga$r2$results$sig, 1)
  }
  else {
    expect_equal(testedsinmod$tests$sinmod$ga$r2$results$sig, 0)
  }
})

# add tests for fixed frequency test!!


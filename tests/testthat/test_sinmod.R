#' Test the boscillator function 'sinmod_bosc' that fits a sinusoidal model on the time courses of
#' a dense sampling study.

sourcePartial("defaults.R",
              startTag = '# simulate_experiment defaults',
              endTag = '# test sinmod defaults')

#loop through possible methods
methods <- list("fixed_i", "grid_search", "fixed_bounds")

for (i in methods){

  if (i == "fixed_i"){
    suppressWarnings(sin_bosc <- sinmod_bosc(no_model2_bosc, niter = 25,
                                             convergence_count = 20, verbose = F))
  }
  if (i == "grid_search"){
    suppressWarnings(sin_bosc <- sinmod_bosc(no_model2_bosc, grid = 5, verbose = F))
  }
  if (i == "fixed_bounds"){
    suppressWarnings(sin_bosc <- sinmod_bosc(no_model2_bosc,
                                             lower = list(intercept = 0, a = 0, f = 4, phi = 0),
                                             upper = list(intercept = 1, a = 1, f = 4, phi = 2 * pi),
                                             verbose = F))
  }

  #function call
  test_that("function call sinmod added to history of object", {
    expect_match(sin_bosc$hist, "_sinmod_")
  })

  #real data
  test_that(paste("sinmod real data ", i), {
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

  #surrogates
  test_that(paste("sinmod surrogate data ", i), {
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
}

# piping fixed frequencies------------------------------------------------------------------
suppressWarnings(sin_bosc <- sinmod_bosc(no_model2_bosc, grid = 8,
                                         fixed_f = c(4,6), verbose = F))

test_that("sinmod real data fixed_f", {
  # ss
  expect_equal(nrow(sin_bosc$data$ss$real$sinmod), sinm_sub * 3 * fixed_freqs)
  expect_equal(as.numeric(sin_bosc$data$ss$real$sinmod$subj), sort(rep(1:sinm_sub, 3*fixed_freqs)))
  # expect deviance < 0.05 & r2 > 0.9
  expect_true(!is.null(sin_bosc$data$ss$real$sinmod %>% filter(deviance >= 0.05 | r2 <= 0.9)))
  # ga
  expect_equal(nrow(sin_bosc$data$ga$real$sinmod), 3 * fixed_freqs)
  # expect deviance < 0.05 & r2 > 0.9
  expect_true(!is.null(sin_bosc$data$ga$real$sinmod %>% filter(deviance >= 0.05 | r2 <= 0.9)))
})

test_that("sinmod surrogate data fixed_f", {
  # ss
  expect_equal(nrow(sin_bosc$data$ss$surrogate$sinmod), sinm_sub * sinm_surr * 3 * fixed_freqs)
  expect_equal(as.numeric(sin_bosc$data$ss$surrogate$sinmod$subj),
               sort(rep(1:sinm_sub, sinm_surr*3*fixed_freqs)))
  # expect deviance < 0.05 & r2 > 0.9
  expect_true(!is.null(sin_bosc$data$ss$surrogate$sinmod %>% filter(deviance >= 0.05 | r2 <= 0.9)))
  # ga
  expect_equal(nrow(sin_bosc$data$ga$surrogate$sinmod), 3*sinm_surr*fixed_freqs)
  # expect deviance < 0.05 & r2 > 0.9
  expect_true(!is.null(sin_bosc$data$ga$surrogate$sinmod %>% filter(deviance >= 0.05 | r2 <= 0.9)))
})

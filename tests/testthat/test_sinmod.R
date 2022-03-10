#' Test the boscillator function 'sinmod_bosc' that fits a sinusoidal model on the time courses of
#' a dense sampling study.
###################################################################################################
# TO DO:
# - modelling throws 'data = everything()' warning!
# - zwecks Test ob fbins richtig sind einmal printen lassen (nur als sanity check)
# - Randbeispiele als manuelle spot checks (mit Loop)?
# - falls Heuristik dazu vorhanden:
#   -> check, ob alle AIC/BIC in bestimmter range liegen?
#   -> check, ob residual-sum-of-squares unter bestimmtem Wert?
#   -> sonst eine Möglichkeit zur Überprüfung, ob Modelling richtig durchgeführt wird?
###################################################################################################
source("defaults.R") # sources defaults

test_that("function call sinmod added to history of object", {
  suppressWarnings({
  sin_bosc <- sinmod_bosc(no_x_bosc)
  expect_match(sin_bosc$hist, "_sinmod_")
  })
})

test_that("sinmod real data", {
  suppressWarnings({
  sin_bosc <- sinmod_bosc(no_model_bosc)
  })
  # ss
  expect_equal(nrow(sin_bosc$data$ss$real$sinmod), def_n_sub)
  # did all estimates yield good p values?
  for (i in sin_bosc$data$ss$real$sinmod$estimates) {
    for (j in nrow(i[[5]])){
      expect_lt(j, 0.001)
    }
  }
  # ga
  expect_equal(nrow(sin_bosc$data$ga$real$sinmod), 1)
  # did all estimates yield good p values?
  for (i in nrow(sin_bosc$data$ga$real$sinmod$estimates)) {
    expect_lt(i[[5]], 0.001)
  }
  # frequency bins?

})

test_that("sinmod surrogate data", {
  suppressWarnings({
  sin_bosc <- sinmod_bosc(no_model_bosc)
  })
  # ss
  expect_equal(nrow(sin_bosc$data$ss$surrogate$sinmod), def_n_sub * def_n_surr)
  # did all estimates yield good p values?
  for (i in sin_bosc$data$ss$surrogate$sinmod$estimates) {
    for (j in nrow(i[[5]])){
      expect_lt(j, 0.001)
    }
  }
  # ga
  expect_equal(nrow(sin_bosc$data$ga$surrogate$sinmod), def_n_surr)
  # did all estimates yield good p values?
  for (i in sin_bosc$data$ga$surrogate$sinmod$estimates) {
    for (j in nrow(i[[5]])){
      expect_lt(j, 0.001)
    }
  }
})



#' Test the boscillator function 'generate surrogates' that simulates a surrogate dataset of a dense
#' sampling study.
###################################################################################################
# INFO:
# - aggregate_bosc deckt aggregation ab, deshalb nur schneller Test ob Zeilenanzahl stimmt hier
###################################################################################################
source("defaults.R") # sources defaults

test_that("function call gen-surr added to history of object", {
  surr_bosc <- generate_surrogates(no_surr_bosc, n_surr = def_n_surr)
  expect_match(surr_bosc$hist, "_gen-surr_")
})

test_that("surrogates on single trial level created correctly", {
  surr_bosc <- generate_surrogates(no_surr_bosc, n_surr = def_n_surr)
  expect_length(surr_bosc$data$single_trial$surrogate$data$subj,
               def_n_sub * def_n_surr * def_n_timepoints * def_n_trials)
  # check, if all subjects contain 100 trials per all 20 timepoints, each times 20 for surrogates
  expect_equal(as.integer(surr_bosc$data$single_trial$surrogate$data$trial),
               rep(sort(rep(seq(1:def_n_trials), def_n_surr)), def_n_sub * def_n_timepoints))
  # check, if all timepoints occur with equal frequency
  expect_equal(0, as.integer(var(surr_bosc$data$single_trial$surrogate$data %>% ungroup() %>% count(time) %>% select(n))))
})

test_that("quick test of aggregation", {
  surr_bosc <- generate_surrogates(no_surr_bosc, n_surr = def_n_surr)
  expect_length(surr_bosc$data$ss$surrogate$data$subj, def_n_sub * def_n_timepoints * def_n_surr)
  expect_length(surr_bosc$data$ga$surrogate$data$time, def_n_timepoints * def_n_surr)
})

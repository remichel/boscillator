#' Test the boscillator function 'generate surrogates' that simulates a surrogate dataset of a dense
#' sampling study.
#' Two possible methods: perm or auto-regression model (ss vs. ss_ga)

source("defaults.R")
load("agg_bosc.RData")
load("surr_bosc.RData")
load("surr_ss_bosc.RData")
load("surr_ga_bosc.RData")

test_that("function call gen-surr added to history of object", {
  surr_bosc <- generate_surrogates(no_surr_bosc, n_surr = def_n_surr, method = "perm")
  expect_match(surr_bosc$hist, "_gen-surr_")
})

# perm --------------------------------------------------------------------------------------------
test_that("perm surrogates on single trial level created correctly", {
  surr_bosc <- generate_surrogates(no_surr_bosc, n_surr = def_n_surr, method = "perm")
  expect_length(surr_bosc$data$single_trial$surrogate$data$subj,
               def_n_sub * def_n_surr * def_n_timepoints * def_n_trials)
  # check, if all subjects contain 100 trials per all 20 timepoints, each times 20 for surrogates
  expect_equal(as.integer(surr_bosc$data$single_trial$surrogate$data$trial),
               rep(sort(rep(seq(1:def_n_trials), def_n_surr)), def_n_sub * def_n_timepoints))
  # check, if all timepoints occur with equal frequency
  expect_equal(0, as.integer(var(surr_bosc$data$single_trial$surrogate$data %>%
                                   ungroup() %>% count(time) %>% select(n))))
})

test_that("quick test of perm aggregation", {
  surr_bosc <- generate_surrogates(no_surr_bosc, n_surr = def_n_surr, method = "perm")
  expect_length(surr_bosc$data$ss$surrogate$data$subj, def_n_sub * def_n_timepoints * def_n_surr)
  expect_length(surr_bosc$data$ga$surrogate$data$time, def_n_timepoints * def_n_surr)
})

test_that("failsafe perm", {
  surr = generate_surrogates(agg_bosc, n_surr = 10, seed_num = 389493.1)
  expect_equal(surr, surr_bosc)
})

# auto-regression model, ss only ------------------------------------------------------------------
test_that("ar_ss surrogates on single trial level created correctly", {
  surr_bosc <- generate_surrogates(no_surr_bosc, n_surr = def_n_surr, method = "ar_ss")
  expect_length(surr_bosc$data$ss$surrogate$data$subj,
                def_n_sub * def_n_surr * def_n_timepoints)
  # check, if all subjects contain 20 surrogates
  expect_equal(as.integer(surr_bosc$data$ss$surrogate$data$n_surr),
               rep(sort(rep(seq(1:def_n_surr), def_n_timepoints)), def_n_sub))
  # check, if all timepoints occur with equal frequency
  expect_equal(0, as.integer(var(surr_bosc$data$ss$surrogate$data %>% ungroup()
                                 %>% count(time) %>% select(n))))
})

test_that("quick test of ar_ss aggregation", {
  surr_bosc <- generate_surrogates(no_surr_bosc, n_surr = def_n_surr, method = "ar_ss")
  expect_length(surr_bosc$data$ga$surrogate$data$time, def_n_timepoints * def_n_surr)
})

test_that("failsafe ar_ss", {
  surr = generate_surrogates(agg_bosc, n_surr = 10, method = "ar_ss", seed_num = 389493.1)
  expect_equal(surr, surr_ss_bosc)
})

# auto-regression model, ss_ga --------------------------------------------------------------------
test_that("ar_ss_ga surrogates on single trial level created correctly", {
  surr_bosc <- generate_surrogates(no_surr_bosc, n_surr = def_n_surr, method = "ar_ss_ga")
  expect_length(surr_bosc$data$ss$surrogate$data$subj,
                def_n_sub * def_n_surr * def_n_timepoints)
  # check, if all subjects contain 20 surrogates
  expect_equal(as.integer(surr_bosc$data$ss$surrogate$data$n_surr),
               rep(sort(rep(seq(1:def_n_surr), def_n_timepoints)), def_n_sub))
  # check, if all timepoints occur with equal frequency
  expect_equal(0, as.integer(var(surr_bosc$data$ss$surrogate$data %>% ungroup() %>% count(time) %>% select(n))))
})

test_that("ar_ss_ga surrogates on ga level created correctly", {
  surr_bosc <- generate_surrogates(no_surr_bosc, n_surr = def_n_surr, method = "ar_ss_ga")
  expect_length(surr_bosc$data$ga$surrogate$data$time,
                def_n_timepoints * def_n_surr)
  # check timepoints vector
  expect_equal(surr_bosc$data$ga$surrogate$data$time,
               rep(seq(0, 0.76, by = 0.04), def_n_surr))
})

test_that("failsafe ar_ss_ga", {
  surr = generate_surrogates(agg_bosc, n_surr = 10, method = "ar_ss_ga", seed_num = 389493.1)
  expect_equal(surr, surr_ga_bosc)
})

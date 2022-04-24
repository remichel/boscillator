#' Test the boscillator function 'aggregate' that aggregates various levels of data in a bosc object.

# load needed defaults & test values
sourcePartial("defaults.R",
              startTag = '# simulate_experiment defaults',
              endTag = '# detrend defaults')
load("bosc.RData")
load("agg_bosc.RData")

test_that("function call aggregate_bosc added to history of object", {
  agg_bosc <- aggregate_bosc(no_agg_bosc)
  expect_match(agg_bosc$hist, "_aggregate_")
})

test_that("aggregates real data correctly", {
  agg_bosc <- aggregate_bosc(no_agg_bosc)
  # level ss
  expect_equal(nrow(agg_bosc$data$ss$real$data), def_n_sub * def_n_timepoints)
  expect_equal(agg_bosc$data$ss$real$data$subj, as.factor(sort(rep(1:def_n_sub, def_n_timepoints))))
  expect_equal(agg_bosc$data$ss$real$data$time, rep(seq(0, (def_n_timepoints - 1) / def_sfreq, 1 / def_sfreq), def_n_sub))
  #check, if parameters are combined correctly -> unique combination
  expect_equal(unique(xtabs(~agg_bosc$data$ss$real$data$subj + agg_bosc$data$ss$real$data$time)), 1)
  # level ga
  expect_equal(nrow(agg_bosc$data$ga$real$data), def_n_timepoints)
  expect_equal(agg_bosc$data$ga$real$data$time, seq(0, (def_n_timepoints - 1) / def_sfreq, 1 / def_sfreq))
})

test_that("aggregates surrogate data correctly", {
  surr_agg_bosc <- generate_surrogates(no_agg_bosc, n_surr = def_n_surr, method = "perm")
  # level ss
  expect_equal(nrow(surr_agg_bosc$data$ss$surrogate$data), def_n_sub * def_n_timepoints * def_n_surr)
  expect_equal(surr_agg_bosc$data$ss$surrogate$data$subj, as.factor(sort(rep(rep(1:def_n_sub, def_n_timepoints), def_n_surr))))
  expect_equal(surr_agg_bosc$data$ss$surrogate$data$time, rep(sort(rep(seq(0, (def_n_timepoints - 1) / def_sfreq, 1 / def_sfreq), def_n_surr)), def_n_sub))
  # check, if parameters are combined correctly -> unique combination
  expect_equal(unique(xtabs(~surr_agg_bosc$data$ss$surrogate$data$subj + surr_agg_bosc$data$ss$surrogate$data$time + surr_agg_bosc$data$ss$surrogate$data$n_surr)), 1)
  # level ga
  expect_equal(nrow(surr_agg_bosc$data$ga$surrogate$data), def_n_timepoints * def_n_surr)
  expect_equal(surr_agg_bosc$data$ga$surrogate$data$time, sort(rep(seq(0, (def_n_timepoints - 1) / def_sfreq, 1 / def_sfreq), def_n_surr)))
})

test_that("failsafe", {
  agg = aggregate_bosc(bosc)
  expect_equal(agg, agg_bosc)
})


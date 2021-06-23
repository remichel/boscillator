test_that("experiment simulation works", {

  n = 5
  ntrial = 100
  tps = 20
  sfreq = 25
  test = simulate_experiment(n_sub = n,
                             n_trials = ntrial,
                             n_timepoints = tps,
                             sfreq = sfreq,
                             phase_jitter_across_subj = 0,
                             intercept_jitter_across_subj = 0)


  # check class
  expect_equal(class(test), "BOSC-Object")
  # check spec sheet
  expect_equal(test$data$single_trial$sim_spec$n_sub, n)
  expect_equal(test$data$single_trial$sim_spec$n_trials, ntrial)
  expect_equal(test$data$single_trial$sim_spec$n_timepoints, tps)
  expect_equal(test$data$single_trial$sim_spec$phase_jitter_across_subj, 0)
  expect_equal(test$data$single_trial$sim_spec$intercept_jitter_across_subj, 0)
  # check dataset itself
  expect_equal(is.factor(test$data$single_trial$data$subj), TRUE)
  expect_equal(is.numeric(test$data$single_trial$data$time), TRUE)
  expect_equal(is.numeric(test$data$single_trial$data$trial), TRUE)
  expect_equal(is.numeric(test$data$single_trial$data$resp), TRUE)

  expect_equal(max(test$data$single_trial$data$trial), ntrial)
  expect_equal(max(as.numeric(test$data$single_trial$data$subj)), n)
  expect_equal(max(test$data$single_trial$data$time), (tps - 1) * 1 / sfreq)

  expect_equal(length(unique(test$data$single_trial$data$subj)), n)
  expect_equal(length(unique(test$data$single_trial$data$trial)), ntrial)
  expect_equal(length(unique(test$data$single_trial$data$time)), tps)

})

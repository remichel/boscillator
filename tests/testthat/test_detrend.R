#' Test the boscillator function 'detrend bosc' that detrends time courses of a dense
#' sampling study.

sourcePartial("defaults.R",
              startTag = '# simulate_experiment defaults',
              endTag = '# window/pad/scale defaults')
load("sim1_bosc.RData")
load("sim2_bosc.RData")
load("lin_detrend_bosc.RData")
load("exp_detrend_bosc.RData")

test_that("function call detrend added to history of object", {
  detr_bosc <- detrend_bosc(no_detr_bosc_lin, verbose = F)
  expect_match(detr_bosc$hist, "_detrend_")
})

test_that("detrending linear trend", {
  detr_bosc_lin <- detrend_bosc(no_detr_bosc_lin, order = 1, verbose = F)
  # real data detrending
  # level ss - spot check of subject 1
  p <- notrend_test(detr_bosc_lin$data$ss$real$data[detr_bosc_lin$data$ss$real$data$subj == 1,]$hr)$p.value
  expect_gt(p, 0.05)
  # level ga
  p <- notrend_test(detr_bosc_lin$data$ga$real$data$hr)$p.value
  expect_gt(p, 0.05)

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
  detr_bosc_exp <- detrend_bosc(no_detr_bosc_exp, order = 2, verbose = F)
  # real data detrending
  # level ss - spot check of subject 1
  p <- notrend_test(detr_bosc_exp$data$ss$real$data[detr_bosc_exp$data$ss$real$data$subj == 1,]$hr, test = "MK")$p.value
  expect_gt(p, 0.05)
  # level ga
  p <- notrend_test(detr_bosc_exp$data$ga$real$data$hr, test = "MK")$p.value
  expect_gt(p, 0.05)

  # surrogate data detrending
  # level ss - spot check of surrogate 1 of subject 1
  p <- notrend_test(detr_bosc_exp$data$ss$surrogate$data[detr_bosc_exp$data$ss$surrogate$data$subj == 1
                                                           & detr_bosc_exp$data$ss$surrogate$data$n_surr == 1,]$hr, test = "MK")$p.value
  expect_gt(p, 0.05)
  # level ga - spot check of surrogate 1
  p <- notrend_test(detr_bosc_exp$data$ga$surrogate$data[detr_bosc_exp$data$ga$surrogate$data$n_surr == 1,]$hr, test = "MK")$p.value
  expect_gt(p, 0.05)
})

test_that("failsafe", {
  sim1_bosc = generate_surrogates(sim1_bosc, n_surr = 50, seed_num = 389493.1)
  sim2_bosc = generate_surrogates(sim2_bosc, n_surr = 50, seed_num = 389493.1)
  lin_detrend = detrend_bosc(sim1_bosc, order = 1, verbose = F)
  exp_detrend = detrend_bosc(sim2_bosc, order = 2, verbose = F)
  expect_equal(lin_detrend, lin_detrend_bosc)
  expect_equal(exp_detrend, exp_detrend_bosc)
})

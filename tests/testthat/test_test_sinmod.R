#' Test the boscillator function 'test_sinmod' that performs a statistical test on sinusoidal models
#' obtained by function sinmod_bosc.

source("defaults.R")

# test sinmod results, no fixed frequencies--------------------------------------------------------
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

# test sinmod results, fixed frequencies-----------------------------------------------------------
suppressWarnings(testedsinmod <- test_sinmod(testsinmod2_bosc, verbose = F))

test_that("fixed f sinmod test on ss level is done correctly", {
  # structure
  expect_equal(nrow(testedsinmod$tests$sinmod$ss$r2$results), sinm_sub * 3 * fixed_freqs)
  expect_equal(testedsinmod$tests$sinmod$ss$r2$results$mcc_method,
               rep(c("uncorrected", "bonferroni", "fdr"), sinm_sub * fixed_freqs))
  # observed r2 > 0.9?
  expect_true(!is.null(testedsinmod$tests$sinmod$ss$r2$results %>% filter(r2_observed <= 0.9)))
  # did all tests reach significance in fixed_f that was the simulated frequency?
  expect_equal(unique(testedsinmod$tests$sinmod$ss$r2$results %>% filter(fixed_f == sinm_f) %>% pull(sig)), 1)
  # are all tests performed on other fixed frequencies not significant?
  expect_equal(unique(testedsinmod$tests$sinmod$ss$r2$results %>% filter(fixed_f != sinm_f) %>% pull(sig)), 0)
  # uncorrected always smaller p than other tests?
  expect_equal(unique(testedsinmod$tests$sinmod$ss$r2$results %>% group_by(subj, fixed_f)
                      %>% slice(which.min(p)) %>% pull(mcc_method)), "uncorrected")
  # fdr <= bonferroni?
  expect_equal(unique(testedsinmod$tests$sinmod$ss$r2$results %>%
                        filter(mcc_method == "fdr" | mcc_method == "bonferroni") %>%
                        group_by(subj, fixed_f) %>% mutate(tmp = p[mcc_method == "fdr"]) %>%
                        mutate(comp = ifelse(p >= tmp, 1, 0)) %>% pull(comp)), 1)
})

test_that("fixed f sinmod test on ga level is done correctly", {
  # structure
  expect_equal(nrow(testedsinmod$tests$sinmod$ga$r2$results), 3 * fixed_freqs)
  expect_equal(testedsinmod$tests$sinmod$ga$r2$results$mcc_method,
               rep(c("uncorrected", "bonferroni", "fdr"), fixed_freqs))
  # observed r2 > 0.9?
  expect_true(!is.null(testedsinmod$tests$sinmod$ga$r2$results %>% filter(r2_observed <= 0.9)))
  # did all tests reach significance in fixed_f that was the simulated frequency?
  expect_equal(unique(testedsinmod$tests$sinmod$ga$r2$results %>% filter(fixed_f == sinm_f) %>% pull(sig)), 1)
  # are all tests performed on other fixed frequencies not significant?
  expect_equal(unique(testedsinmod$tests$sinmod$ga$r2$results %>% filter(fixed_f != sinm_f) %>% pull(sig)), 0)
  # uncorrected always smaller p than other tests?
  expect_equal(unique(testedsinmod$tests$sinmod$ss$r2$results
                      %>% slice(which.min(p)) %>% pull(mcc_method)), "uncorrected")
  # fdr <= bonferroni?
  expect_equal(unique(testedsinmod$tests$sinmod$ga$r2$results %>%
                        filter(mcc_method == "fdr" | mcc_method == "bonferroni") %>%
                        group_by(fixed_f) %>%
                        mutate(tmp = p[mcc_method == "fdr"]) %>%
                        mutate(comp = ifelse(p >= tmp, 1, 0)) %>% pull(comp)), 1)
})


#' Test the boscillator function 'test_fft' that performs a statistical test on frequency spectra
#' obtained by function fft_bosc.

sourcePartial("defaults.R",
              startTag = '# simulate_experiment defaults',
              endTag = '# sinmod defaults')
load("fftcomp_bosc.RData")
load("fftest.RData")

# test fft results
suppressWarnings(testedfft <- test_fft(testfft_bosc, verbose = F))

# find bin closest to simulated frequency
closest_bin = testedfft$tests$fft$ss$amp$results$f[which.min(abs(testedfft$tests$fft$ss$amp$results$f - fft_f))]

test_that("function call _test_ added to history of object", {
  expect_match(testedfft$hist, "_test_")
})

test_that("fft test on ss level is done correctly", {
  # structure
  expect_equal(nrow(testedfft$tests$fft$ss$amp$results), def_n_sub * length(def_bins) * n_tests)
  expect_equal(testedfft$tests$fft$ss$amp$results$f, rep(sort(rep(def_bins, n_tests)), def_n_sub))
  # did all tests reach significance in bin closest to simulated frequency?
  expect_equal(unique(testedfft$tests$fft$ss$amp$results %>% filter(f == closest_bin) %>% pull(sig)), 1)
  # uncorrected always smaller p than other tests?
  expect_equal(unique(testedfft$tests$fft$ss$amp$results %>% group_by(subj, f)
                      %>% slice(which.min(p)) %>% pull(mcc_method)), "uncorrected")
  # fdr <= bonferroni?
  expect_equal(unique(testedfft$tests$fft$ss$amp$results %>% filter(mcc_method == "fdr" | mcc_method == "bonferroni") %>%
                        group_by(subj, f) %>% mutate(tmp = p[mcc_method == "fdr"]) %>%
                        mutate(comp = ifelse(p >= tmp, 1, 0)) %>% pull(comp)), 1)
})

test_that("fft test on ga level is done correctly", {
  # structure
  expect_equal(nrow(testedfft$tests$fft$ga$amp$results), length(def_bins) * n_tests)
  expect_equal(testedfft$tests$fft$ga$amp$results$f, sort(rep(def_bins, n_tests)))
  # did all tests reach significance in bin closest to simulated frequency?
  expect_equal(unique(testedfft$tests$fft$ga$amp$results %>% filter(f == closest_bin) %>% pull(sig)), 1)
  # uncorrected always smaller p than other tests?
  expect_equal(unique(testedfft$tests$fft$ga$amp$results %>% group_by(f)
                      %>% slice(which.min(p)) %>% pull(mcc_method)), "uncorrected")
  # fdr <= bonferroni?
  expect_equal(unique(testedfft$tests$fft$ga$amp$results %>% filter(mcc_method == "fdr" | mcc_method == "bonferroni") %>%
                        group_by(f) %>% mutate(tmp = p[mcc_method == "fdr"]) %>%
                        mutate(comp = ifelse(p >= tmp, 1, 0)) %>% pull(comp)), 1)
})

test_that("fft test on merged level done correctly", {
  # amp
  # structure
  expect_equal(nrow(testedfft$tests$fft$merged$amp$results), length(def_bins) * n_tests)
  expect_equal(testedfft$tests$fft$merged$amp$results$f, sort(rep(def_bins, n_tests)))
  # did all tests reach significance in bin closest to simulated frequency?
  expect_equal(unique(testedfft$tests$fft$merged$amp$results %>% filter(f == closest_bin) %>% pull(sig)), 1)
  # fdr <= bonferroni?
  expect_equal(unique(testedfft$tests$fft$merged$amp$results %>% filter(mcc_method == "fdr" | mcc_method == "bonferroni") %>%
                        group_by(f) %>% mutate(tmp = p[mcc_method == "fdr"]) %>%
                        mutate(comp = ifelse(p >= tmp, 1, 0)) %>% pull(comp)), 1)
  # complex
  # structure
  expect_equal(nrow(testedfft$tests$fft$merged$complex$results), length(def_bins) * n_tests)
  expect_equal(testedfft$tests$fft$merged$complex$results$f, sort(rep(def_bins, n_tests)))
  # did all tests reach significance in bin closest to simulated frequency?
  expect_equal(unique(testedfft$tests$fft$merged$complex$results %>% filter(f == closest_bin) %>% pull(sig)), 1)
  # fdr <= bonferroni?
  expect_equal(unique(testedfft$tests$fft$merged$complex$results %>% filter(mcc_method == "fdr" | mcc_method == "bonferroni") %>%
                        group_by(f) %>% mutate(tmp = p[mcc_method == "fdr"]) %>%
                        mutate(comp = ifelse(p >= tmp, 1, 0)) %>% pull(comp)), 1)
})

test_that("failsafe", {
  fftcomp_tested = test_fft(fftcomp_bosc, verbose = F)
  expect_equal(fftcomp_tested, fftest)
})

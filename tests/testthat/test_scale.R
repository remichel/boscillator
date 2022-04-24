#' Test the boscillator function 'scale_bosc' that z_scales the time courses of a dense
#' sampling study.

sourcePartial("defaults.R",
              startTag = '# simulate_experiment defaults',
              endTag = '# fft defaults')
load("scaled_bosc.RData")
load("surr_bosc.RData")

test_that("function call scale added to history of object", {
  scaled_bosc <- scale_bosc(no_x_bosc, verbose = F)
  expect_match(scaled_bosc$hist, "_scaled_")
})

test_that("scaling", {
  scaled_bosc <- scale_bosc(no_x_bosc, verbose = F)
  # real data
  # ss
  expect_equal(as.numeric(scaled_bosc$data$ss$real$data$hr),
               no_x_bosc$data$ss$real$data %>%
                                        mutate (hr = (hr - mean(hr))/sd(hr)) %>%
                                        pull(hr))
  # ga
  expect_equal(as.numeric(scaled_bosc$data$ga$real$data$hr),
               no_x_bosc$data$ga$real$data %>%
                                   mutate(hr = (hr - mean(hr))/sd(hr)) %>%
                                   pull(hr))
  # surrogates
  # ss
  expect_equal(as.numeric(scaled_bosc$data$ss$surrogate$data$hr),
               no_x_bosc$data$ss$surrogate$data %>%
                                   group_by(subj, n_surr) %>%
                                   mutate (hr = (hr - mean(hr))/sd(hr)) %>%
                                   pull(hr))
  # ga
  expect_equal(as.numeric(scaled_bosc$data$ga$surrogate$data$hr),
               no_x_bosc$data$ga$surrogate$data %>%
                                    group_by(n_surr) %>%
                                    mutate(hr = (hr - mean(hr))/sd(hr)) %>%
                                    pull(hr))

})

test_that("failsafe", {
  scaled = scale_bosc(surr_bosc, verbose = F)
  expect_equal(scaled$data$ss$real$data$hr, scaled_bosc$data$ss$real$data$hr)
  expect_equal(scaled$data$ga$real$data$hr, scaled_bosc$data$ga$real$data$hr)
  expect_equal(scaled$data$ss$surrogate$data$hr, scaled_bosc$data$ss$surrogate$data$hr)
  expect_equal(scaled$data$ga$surrogate$data$hr, scaled_bosc$data$ga$surrogate$data$hr)
})

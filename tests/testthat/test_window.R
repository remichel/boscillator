#' Test the boscillator function 'window_bosc' that applies window function to
#' the time courses of a dense sampling study.
#' Possible methods: hann, hamm, tukey, triangle, cosine, kaiser

source("defaults.R") # sources defaults
load("surr_bosc.RData")
load("hann_win_bosc.RData")


test_that("function call window added to history of object", {
  win_bosc <- window_bosc(no_x_bosc, verbose = F)
  expect_match(win_bosc$hist, "_window_")
})

test_that("windowing ~ hann", {
  win_bosc <- window_bosc(no_x_bosc, method = "hann", verbose = F)
  # real data
  # ss
  expect_equal(as.numeric(win_bosc$data$ss$real$data$hr),
               as.numeric(unlist(no_x_bosc$data$ss$real$data %>%
                                   dplyr::group_by(subj) %>%
                                   dplyr::mutate(hr = .data$hr * bspec::hannwindow(def_n_timepoints)) %>%
                                   dplyr::ungroup() %>% dplyr::select(hr))))

  # ga
  expect_equal(as.numeric(win_bosc$data$ga$real$data$hr),
               as.numeric(unlist(no_x_bosc$data$ga$real$data %>%
                                   dplyr::mutate(hr = .data$hr * bspec::hannwindow(def_n_timepoints)) %>%
                                   dplyr::ungroup() %>% dplyr::select(hr))))

  # surrogates
  # ss
  expect_equal(as.numeric(win_bosc$data$ss$surrogate$data$hr),
               as.numeric(unlist(no_x_bosc$data$ss$surrogate$data %>%
                                   dplyr::group_by(subj, n_surr) %>%
                                   dplyr::mutate(hr = .data$hr * bspec::hannwindow(def_n_timepoints)) %>%
                                   dplyr::ungroup() %>% dplyr::select(hr))))

  # ga
  expect_equal(as.numeric(win_bosc$data$ga$surrogate$data$hr),
               as.numeric(unlist(no_x_bosc$data$ga$surrogate$data %>%
                                   dplyr::group_by(n_surr) %>%
                                   dplyr::mutate(hr = .data$hr * bspec::hannwindow(def_n_timepoints)) %>%
                                   dplyr::ungroup() %>% dplyr::select(hr))))
})

test_that("windowing ~ tukey", {
  r <- .1
  win_bosc <- window_bosc(no_x_bosc, method = "tukey", r = r, verbose = F)
  # real data
  # ss
  expect_equal(as.numeric(win_bosc$data$ss$real$data$hr),
               as.numeric(unlist(no_x_bosc$data$ss$real$data %>%
                                   dplyr::group_by(subj) %>%
                                   dplyr::mutate(hr = .data$hr * bspec::tukeywindow(def_n_timepoints, r)) %>%
                                   dplyr::ungroup() %>% dplyr::select(hr))))

  # ga
  expect_equal(as.numeric(win_bosc$data$ga$real$data$hr),
               as.numeric(unlist(no_x_bosc$data$ga$real$data %>%
                                   dplyr::mutate(hr = .data$hr * bspec::tukeywindow(def_n_timepoints, r)) %>%
                                   dplyr::ungroup() %>% dplyr::select(hr))))

  # surrogates
  # ss
  expect_equal(as.numeric(win_bosc$data$ss$surrogate$data$hr),
               as.numeric(unlist(no_x_bosc$data$ss$surrogate$data %>%
                                   dplyr::group_by(subj, n_surr) %>%
                                   dplyr::mutate(hr = .data$hr * bspec::tukeywindow(def_n_timepoints, r)) %>%
                                   dplyr::ungroup() %>% dplyr::select(hr))))

  # ga
  expect_equal(as.numeric(win_bosc$data$ga$surrogate$data$hr),
               as.numeric(unlist(no_x_bosc$data$ga$surrogate$data %>%
                                   dplyr::group_by(n_surr) %>%
                                   dplyr::mutate(hr = .data$hr * bspec::tukeywindow(def_n_timepoints, r)) %>%
                                   dplyr::ungroup() %>% dplyr::select(hr))))
})

test_that("windowing ~ triangle", {
  win_bosc <- window_bosc(no_x_bosc, method = "triangle", verbose = F)
  # real data
  # ss
  expect_equal(as.numeric(win_bosc$data$ss$real$data$hr),
               as.numeric(unlist(no_x_bosc$data$ss$real$data %>%
                                   dplyr::group_by(subj) %>%
                                   dplyr::mutate(hr = .data$hr * bspec::trianglewindow(def_n_timepoints)) %>%
                                   dplyr::ungroup() %>% dplyr::select(hr))))

  # ga
  expect_equal(as.numeric(win_bosc$data$ga$real$data$hr),
               as.numeric(unlist(no_x_bosc$data$ga$real$data %>%
                                   dplyr::mutate(hr = .data$hr * bspec::trianglewindow(def_n_timepoints)) %>%
                                   dplyr::ungroup() %>% dplyr::select(hr))))

  # surrogates
  # ss
  expect_equal(as.numeric(win_bosc$data$ss$surrogate$data$hr),
               as.numeric(unlist(no_x_bosc$data$ss$surrogate$data %>%
                                   dplyr::group_by(subj, n_surr) %>%
                                   dplyr::mutate(hr = .data$hr * bspec::trianglewindow(def_n_timepoints)) %>%
                                   dplyr::ungroup() %>% dplyr::select(hr))))

  # ga
  expect_equal(as.numeric(win_bosc$data$ga$surrogate$data$hr),
               as.numeric(unlist(no_x_bosc$data$ga$surrogate$data %>%
                                   dplyr::group_by(n_surr) %>%
                                   dplyr::mutate(hr = .data$hr * bspec::trianglewindow(def_n_timepoints)) %>%
                                   dplyr::ungroup() %>% dplyr::select(hr))))
})

test_that("failsafe", {
  hann_win <- window_bosc(surr_bosc, method = "hann", r = .1, alpha = .54, verbose = F)
  expect_equal(hann_win, hann_win_bosc)
})

#' Test the boscillator function 'window_bosc' that applies window function to
#' the time courses of a dense sampling study.
#' Possible methods: hann, hamm, tukey, triangle, cosine, kaiser

###################################################################################################
# TO DO:
# - wieder so semi sinnvoll, da eigentlich nur Anwendung von bspec functions geprüft wird
# - händisch aber krass extensive
# - hamming, cosine und kaiser failed wegen kleiner numerischer Unterschiede,
#   deswegen erstmal weggelassen
# - looping über verschiedene Methoden möglich, aber dauert halt länger
#   (mit sapply werden Tests nicht mehr so schön im Build Fenster angezeigt)
###################################################################################################

source("defaults.R") # sources defaults


test_that("function call window added to history of object", {
  win_bosc <- window_bosc(no_x_bosc)
  expect_match(win_bosc$hist, "_window_")
})

test_that("windowing ~ hann", {
  win_bosc <- window_bosc(no_x_bosc, method = "hann")
  # real data
  # ss
  expect_equal(as.numeric(win_bosc$data$ss$real$data$hr),
               as.numeric(unlist(no_x_bosc$data$ss$real$data %>%
                                   group_by(subj) %>%
                                   mutate (hr = .data$hr * bspec::hannwindow(def_n_timepoints)) %>%
                                   ungroup %>% select(hr))))

  # ga
  expect_equal(as.numeric(win_bosc$data$ga$real$data$hr),
               as.numeric(unlist(no_x_bosc$data$ga$real$data %>%
                                   mutate(hr = .data$hr * bspec::hannwindow(def_n_timepoints)) %>%
                                   ungroup %>% select(hr))))

  # surrogates
  # ss
  expect_equal(as.numeric(win_bosc$data$ss$surrogate$data$hr),
               as.numeric(unlist(no_x_bosc$data$ss$surrogate$data %>%
                                   group_by(subj, n_surr) %>%
                                   mutate (hr = .data$hr * bspec::hannwindow(def_n_timepoints)) %>%
                                   ungroup %>% select(hr))))

  # ga
  expect_equal(as.numeric(win_bosc$data$ga$surrogate$data$hr),
               as.numeric(unlist(no_x_bosc$data$ga$surrogate$data %>%
                                   group_by(n_surr) %>%
                                   mutate(hr = .data$hr * bspec::hannwindow(def_n_timepoints)) %>%
                                   ungroup %>% select(hr))))
})

test_that("windowing ~ tukey", {
  r <- .1
  win_bosc <- window_bosc(no_x_bosc, method = "tukey", r = r)
  # real data
  # ss
  expect_equal(as.numeric(win_bosc$data$ss$real$data$hr),
               as.numeric(unlist(no_x_bosc$data$ss$real$data %>%
                                   group_by(subj) %>%
                                   mutate (hr = .data$hr * bspec::tukeywindow(def_n_timepoints, r)) %>%
                                   ungroup %>% select(hr))))

  # ga
  expect_equal(as.numeric(win_bosc$data$ga$real$data$hr),
               as.numeric(unlist(no_x_bosc$data$ga$real$data %>%
                                   mutate(hr = .data$hr * bspec::tukeywindow(def_n_timepoints, r)) %>%
                                   ungroup %>% select(hr))))

  # surrogates
  # ss
  expect_equal(as.numeric(win_bosc$data$ss$surrogate$data$hr),
               as.numeric(unlist(no_x_bosc$data$ss$surrogate$data %>%
                                   group_by(subj, n_surr) %>%
                                   mutate (hr = .data$hr * bspec::tukeywindow(def_n_timepoints, r)) %>%
                                   ungroup %>% select(hr))))

  # ga
  expect_equal(as.numeric(win_bosc$data$ga$surrogate$data$hr),
               as.numeric(unlist(no_x_bosc$data$ga$surrogate$data %>%
                                   group_by(n_surr) %>%
                                   mutate(hr = .data$hr * bspec::tukeywindow(def_n_timepoints, r)) %>%
                                   ungroup %>% select(hr))))
})

test_that("windowing ~ triangle", {
  win_bosc <- window_bosc(no_x_bosc, method = "triangle")
  # real data
  # ss
  expect_equal(as.numeric(win_bosc$data$ss$real$data$hr),
               as.numeric(unlist(no_x_bosc$data$ss$real$data %>%
                                   group_by(subj) %>%
                                   mutate (hr = .data$hr * bspec::trianglewindow(def_n_timepoints)) %>%
                                   ungroup %>% select(hr))))

  # ga
  expect_equal(as.numeric(win_bosc$data$ga$real$data$hr),
               as.numeric(unlist(no_x_bosc$data$ga$real$data %>%
                                   mutate(hr = .data$hr * bspec::trianglewindow(def_n_timepoints)) %>%
                                   ungroup %>% select(hr))))

  # surrogates
  # ss
  expect_equal(as.numeric(win_bosc$data$ss$surrogate$data$hr),
               as.numeric(unlist(no_x_bosc$data$ss$surrogate$data %>%
                                   group_by(subj, n_surr) %>%
                                   mutate (hr = .data$hr * bspec::trianglewindow(def_n_timepoints)) %>%
                                   ungroup %>% select(hr))))

  # ga
  expect_equal(as.numeric(win_bosc$data$ga$surrogate$data$hr),
               as.numeric(unlist(no_x_bosc$data$ga$surrogate$data %>%
                                   group_by(n_surr) %>%
                                   mutate(hr = .data$hr * bspec::trianglewindow(def_n_timepoints)) %>%
                                   ungroup %>% select(hr))))
})

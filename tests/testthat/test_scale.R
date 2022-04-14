#' Test the boscillator function 'scale_bosc' that z_scales the time courses of a dense
#' sampling study.

###################################################################################################
# TO DO:
# - besser manuell scalen über x-mean(x))/sd(x)?
###################################################################################################

source("defaults.R") # sources defaults

test_that("function call scale added to history of object", {
  scaled_bosc <- scale_bosc(no_x_bosc)
  expect_match(scaled_bosc$hist, "_scaled_")
})

test_that("scaling", {
  scaled_bosc <- scale_bosc(no_x_bosc)
  # real data
  # ss
  expect_equal(as.numeric(scaled_bosc$data$ss$real$data$hr),
               as.numeric(unlist(no_x_bosc$data$ss$real$data %>%
                                   dplyr::group_by(subj) %>%
                                   dplyr::mutate (hr = scale(.data$hr)) %>%
                                   dplyr::ungroup() %>% dplyr::select(hr))))

  # ga
  expect_equal(as.numeric(scaled_bosc$data$ga$real$data$hr),
               as.numeric(unlist(no_x_bosc$data$ga$real$data %>%
                                   dplyr::mutate(hr = scale(.data$hr)) %>%
                                   dplyr::ungroup() %>% dplyr::select(hr))))

  # surrogates
  # ss
  expect_equal(as.numeric(scaled_bosc$data$ss$surrogate$data$hr),
               as.numeric(unlist(no_x_bosc$data$ss$surrogate$data %>%
                                   dplyr::group_by(subj, n_surr) %>%
                                   dplyr::mutate (hr = scale(.data$hr)) %>%
                                   dplyr::ungroup() %>% dplyr::select(hr))))

  # ga
  expect_equal(as.numeric(scaled_bosc$data$ga$surrogate$data$hr),
               as.numeric(unlist(no_x_bosc$data$ga$surrogate$data %>%
                                   dplyr::group_by(n_surr) %>%
                                   dplyr::mutate(hr = scale(.data$hr)) %>%
                                   dplyr::ungroup() %>% dplyr::select(hr))))

})


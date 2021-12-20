#' BOSC TESTS TEST VALUES FILE
#' stores all test matrices for unit testing boscillator functions

# define test matrix
test_matrix <- data.frame(n_sub = c(20),
                          n_timepoints = c(30),
                          n_trials = c(200),
                          sfreq = c(20),
                          osc_params = I(list(c(0.5, .1, 4, 0))),
                          phase_jitter = I(list(c(0, pi/2))),
                          amplitude_jitter = I(list(c(0, 0))),
                          freq_jitter = I(list(c(0, 0))),
                          intercept_jitter = c(0),
                          transient = c("hanning"),
                          transient_expModel_params = I(list(c(0, 1, .3))),
                          trend = c("linear"),
                          trend_linModel_params = I(list(c(0.5, 0.1))),
                          trend_expModel_params = I(list(c(0, 1 - 2 * 0.1, .6))),
                          aggregate = c(T),
                          seed_num = c(872957.7))

# add values
test_matrix <- test_matrix %>% add_row(n_sub = c(10),
                        n_timepoints = c(150),
                        n_trials = c(100),
                        sfreq = c(25),
                        osc_params = I(list(c(0.5, .1, 4, 0))),
                        phase_jitter = I(list(c(0, pi/2))),
                        amplitude_jitter = I(list(c(0, 0))),
                        freq_jitter = I(list(c(0, 0))),
                        intercept_jitter = c(0),
                        transient = c("hanning"),
                        transient_expModel_params = I(list(c(0, 1, .3))),
                        trend = c("linear"),
                        trend_linModel_params = I(list(c(0.5, 0.1))),
                        trend_expModel_params = I(list(c(0, 1 - 2 * 0.1, .6))),
                        aggregate = c(T),
                        seed_num = c(872957.7))

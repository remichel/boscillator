#'BOSC TESTS DEFAULT FILE
#'stores all relevant defaults used for testthat testing of bosc package

# simulate_experiment defaults
def_n_sub = 14
def_n_timepoints = 20
def_n_trials = 100
def_sfreq = 25
def_osc_params = c(0.5, .1, 4, 0)
def_phase_jitter = c(0, 0)
def_amplitude_jitter = c(0, 0)
def_freq_jitter = c(0, 0)
def_intercept_jitter = 0
def_transient = "none"
def_transient_expModel_params = c(0, 0, 0)
def_trend = "none"
def_trend_linModel_params = c(0, 0)
def_trend_expModel_params = c(0, 0, 0)
def_aggregate = T
def_seed_num = NULL

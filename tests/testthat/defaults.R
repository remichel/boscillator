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
def_trend_polyModel_params = c(0, 0, 0)
def_aggregate = T
def_seed_num = NULL

# generate surrogates defaults
def_n_surr = 20
no_surr_bosc = simulate_experiment()

# aggregate defaults
no_agg_bosc = simulate_experiment(aggregate = F)
def_types = c("real")
def_levels = c("ss", "ga")
def_overwrite = FALSE

# detrend defaults
no_detr_bosc_lin = simulate_experiment(trend = "linear", trend_linModel_params = c(0.3, 0.2))
no_detr_bosc_lin = generate_surrogates(no_detr_bosc_lin)

no_detr_bosc_exp = simulate_experiment(trend = "2ndorder", trend_polyModel_params = c(0, 1 - 2 * 0.1, .6))
no_detr_bosc_exp = generate_surrogates(no_detr_bosc_exp)

# window/pad/scale defaults
no_x_bosc = simulate_experiment()
no_x_bosc = generate_surrogates(no_x_bosc, n_surr = def_n_surr)

def_n_pads = 50

# fft defaults
no_model_bosc = simulate_experiment(osc_params = c(c(0.5, .2, 4, 0)))
no_model_bosc = generate_surrogates(no_model_bosc, n_surr = def_n_surr)

def_bins = c(1.25, 2.50, 3.75, 5.00, 6.25, 7.50, 8.75, 10.00, 11.25, 12.50)

# test fft defaults
testfft_bosc = fft_bosc(no_model_bosc, verbose = F)
fft_f = 4
n_tests = 4

# sinmod defaults
no_model2_bosc = simulate_experiment(n_sub = 5, osc_params = c(c(0.5, .2, 4, 0)))
no_model2_bosc = generate_surrogates(no_model2_bosc, n_surr = 20)

sinm_sub = 5
sinm_surr = 20
sinm_f = 4

# test sinmod defaults
suppressWarnings(testsinmod_bosc <- sinmod_bosc(no_model2_bosc, niter = 25, convergence_count = 20, verbose = F))

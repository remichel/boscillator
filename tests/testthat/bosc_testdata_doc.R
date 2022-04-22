#' Documentation file for creating the bosc testing objects
#' last update: 20.04.2022

# basic bosc-object
bosc = simulate_experiment(n_sub = 20,
                    n_timepoints = 25,
                    n_trials = 200,
                    sfreq = 20,
                    osc_params = c(0.5, .1, 6, 0),
                    phase_jitter = c(0, 0),
                    amplitude_jitter = c(0, 0),
                    freq_jitter = c(0, 0),
                    intercept_jitter = 0,
                    transient = "none",
                    transient_expModel_params = c(0, 1, .4),
                    trend = "none",
                    trend_linModel_params = c(0.4, 0.09),
                    trend_polyModel_params = c(0, 1 - 2 * 0.09, .6),
                    aggregate = F,
                    seed_num = 292585.9)
save(bosc, file = "bosc.RData")

# simulate experiment
sim1_bosc = simulate_experiment(n_sub = 20,
                           n_timepoints = 25,
                           n_trials = 200,
                           sfreq = 20,
                           osc_params = c(0.5, .1, 6, 0),
                           phase_jitter = c(0.3, 0.1),
                           amplitude_jitter = c(0.1, 0.3),
                           freq_jitter = c(0.3, 0.1),
                           intercept_jitter = 0.1,
                           transient = "exponential",
                           transient_expModel_params = c(0, 1, .4),
                           trend = "linear",
                           trend_linModel_params = c(0.4, 0.09),
                           trend_polyModel_params = c(0, 1 - 2 * 0.09, .6),
                           aggregate = T,
                           seed_num = 292585.9)
save(sim1_bosc, file = "sim1_bosc.RData")

sim2_bosc = simulate_experiment(n_sub = 20,
                                n_timepoints = 25,
                                n_trials = 200,
                                sfreq = 20,
                                osc_params = c(0.5, .1, 6, 0),
                                phase_jitter = c(0.3, 0.1),
                                amplitude_jitter = c(0.1, 0.3),
                                freq_jitter = c(0.3, 0.1),
                                intercept_jitter = 0.1,
                                transient = "hanning",
                                transient_expModel_params = c(0, 1, .4),
                                trend = "2ndorder",
                                trend_linModel_params = c(0.4, 0.09),
                                trend_polyModel_params = c(0, 1 - 2 * 0.09, .6),
                                aggregate = T,
                                seed_num = 292585.9)
save(sim2_bosc, file = "sim2_bosc.RData")

# aggregate
agg_bosc = aggregate_bosc(bosc)
save(agg_bosc, file = "agg_bosc.RData")

# surrogates - perm
surr_bosc = generate_surrogates(agg_bosc, n_surr = 10, method = "perm", seed_num = 389493.1)
save(surr_bosc, file = "surr_bosc.RData")

# surrogates - ar_ss
surr_ss_bosc = generate_surrogates(agg_bosc, n_surr = 10, method = "ar_ss", seed_num = 389493.1)
save(surr_ss_bosc, file = "surr_ss_bosc.RData")

# surrogates - ar_ss_ga
surr_ga_bosc = generate_surrogates(agg_bosc, n_surr = 10, method = "ar_ss_ga", seed_num = 389493.1)
save(surr_ga_bosc, file = "surr_ga_bosc.RData")

# pad
pad0_bosc = pad_bosc(surr_bosc, method = "zero", n_pads = 50)
padme_bosc = pad_bosc(surr_bosc, method = "mean", n_pads = 50)
save(pad0_bosc, file = "pad0_bosc.RData")
save(padme_bosc, file = "padme_bosc.RData")

# scale
scaled_bosc = scale_bosc(surr_bosc)
save(scaled_bosc, file = "scaled_bosc.RData")

# window
hann_win_bosc = window_bosc(surr_bosc, method = "hann", r = .1, alpha = .54)
save(hann_win_bosc, file = "hann_win_bosc.RData")
hamm_win_bosc = window_bosc(surr_bosc, method = "hamm", r = .1, alpha = .54)
tukey_win_bosc = window_bosc(surr_bosc, method = "tukey", r = .1, alpha = .54)
triangle_win_bosc = window_bosc(surr_bosc, method = "triangle", r = .1, alpha = .54)
cosine_win_bosc = window_bosc(surr_bosc, method = "cosine", r = .1, alpha = .54)
kaiser_win_bosc = window_bosc(surr_bosc, method = "kaiser", r = .1, alpha = .54)

# detrend
sim1_bosc = generate_surrogates(sim1_bosc, n_surr = 50, seed_num = 389493.1)
sim2_bosc = generate_surrogates(sim2_bosc, n_surr = 50, seed_num = 389493.1)
lin_detrend_bosc = detrend_bosc(sim1_bosc, order = 1)
save(lin_detrend_bosc, file = "lin_detrend_bosc.RData")
exp_detrend_bosc = detrend_bosc(sim2_bosc, order = 2)
save(exp_detrend_bosc, file = "exp_detrend_bosc.RData")

# fft
fftcomp_bosc = fft_bosc(surr_bosc)
save(fftcomp_bosc, file = "fftcomp_bosc.RData")

# test fft
fftest = test_fft(fftcomp_bosc)
save(fftest, file = "fftest.RData")




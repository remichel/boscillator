library(devtools)
load_all()

library(ggplot2)
library(dplyr)

# simulate experiment
bosc = simulate_experiment(phase_jitter_across_subj = 0)
#bosc = simulate_experiment(phase_jitter_across_subj = 0, transient = "exponential", transient_expModel_params = c(0, 1, .3))
#bosc = simulate_experiment(phase_jitter_across_subj = 0, trend = "exponential", trend_expModel_params = c(0, .5, .6))
#bosc = simulate_experiment(phase_jitter_across_subj = 0, transient = "hanning")
# generate surrogates
bosc = generate_surrogates(bosc) # method = "ar"

# plots simulations

# single subjects simulations
ggplot(bosc$data$single_subject$data, aes(x = time, y = hr, group = subj))+
  geom_line(aes(color = subj))+
  theme(legend.position = "none")
# grand average simulations
ggplot(bosc$data$grand_average$data, aes(x = time, y = hr))+
  geom_line()

# plot a single subject simulations and its surrogates
ggplot(NULL)+
  geom_line(data = bosc$data$single_subject$data %>% filter(subj == 2),
            aes(x = time, y = hr), color = "black", size = 3) +
  geom_line(data = bosc$data$single_subject$surrogate$data %>% filter(subj == 2),
            aes(x = time, y = hr, group = n_surr, color = n_surr))+
  theme(legend.position = "none")

# plot grand average simulations and its surrogates
ggplot(NULL)+
  geom_line(data = bosc$data$grand_average$data, aes(x = time, y = hr), color = "black", size = 3) +
  geom_line(data = bosc$data$grand_average$surrogate$data, aes(x = time, y = hr, group = n_surr, color = n_surr))+
  theme(legend.position = "none")



library(devtools)
install()

library(ggplot2)
library(dplyr)

# simulate experiment
d = simulate_experiment(n = 10, n_trials = 50, n_timepoints = 30)


# plot simulations

# single subjects
ggplot(d$data$single_subject$data, aes(x = time, y = hr, group = subj))+
  geom_line(aes(color = subj))
# grand average
ggplot(d$data$grand_average$data, aes(x = time, y = hr))+
  geom_line()

# generate surrogates
d = generate_surrogates(d, n_surr = 10, method = "ar")

# convert to factor for plotting
d$data$single_subject$surrogate$data$n_surr = as.factor(d$data$single_subject$surrogate$data$n_surr)

# plot a single subject and its surrogates
ggplot(NULL)+
  geom_line(data = d$data$single_subject$data %>% filter(subj == 2),
            aes(x = time, y = hr), color = "black", size = 3) +
  geom_line(data = d$data$single_subject$surrogate$data %>% filter(subj == 2),
            aes(x = time, y = ar1, group = n_surr, color = n_surr))


# convert to factor for plotting
d$data$grand_average$surrogate$data$n_surr = as.factor(d$data$grand_average$surrogate$data$n_surr)

# plot grand average and its surrogates
ggplot(NULL)+
  geom_line(data = d$data$grand_average$data, aes(x = time, y = hr), color = "black", size = 3) +
  geom_line(data = d$data$grand_average$surrogate$data, aes(x = time, y = ar1, group = n_surr, color = n_surr))



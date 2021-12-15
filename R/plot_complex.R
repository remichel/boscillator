#' plot_complex
#'
#' @description \code{plot_complex}
#' Plots the results of an FFT complex test.
#'
#' @param bosc BOSC-Object
#' @param freqs A vector of frequencies (in Hz) to plot, defaults to all frequencies available
#'
#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export plot_complex
#' @name plot_complex
#'

#'
plot_complex <- function(bosc, freqs = unique(bosc$tests$fft$ss$complex$data$f)) {



  # subset to frequencies of interest
  d <- bosc$tests$fft$ss$complex$data %>%
    dplyr::filter(.data$f %in% !!freqs)

  test <- bosc$tests$fft$ss$complex$results %>%
    dplyr::filter(.data$f %in% !!freqs)

  # ´determine suitable ymax for plot
  yMax <- max(d$observed_length, d$surrogate_length)

  # plot
  tmpPlot <- ggplot2::ggplot(d, ggplot2::aes(x = .data$surrogate_phase, y = .data$surrogate_length)) +
    # y axis line
    ggplot2::geom_hline(yintercept = seq(0, yMax, yMax / 10), color = "grey90", size = 0.5) +
    # surrogate vectors
    ggplot2::geom_segment(ggplot2::aes(x = .data$surrogate_phase, xend = .data$surrogate_phase, y = 0, yend = .data$surrogate_length), size = 1, color = "grey90", alpha = .8) +
    ggplot2::geom_point(color = "grey20", alpha = .2) +
    # data vector
    ggplot2::geom_segment(ggplot2::aes(x = .data$observed_phase, xend = .data$observed_phase, y = 0, yend = .data$observed_length), size = 1.75, color = "black") +
    ggplot2::geom_point(ggplot2::aes(x = .data$observed_phase, y = .data$observed_length), size = 3, color = "black") +
    # critical vector length
    ggplot2::geom_segment(data = test, ggplot2::aes(x = -pi, xend = pi, y = .data$crit_length, yend = .data$crit_length), color = "red") +
    # initialize polar plot
    ggplot2::coord_polar(start = pi / 2, direction = -1) +
    # split by frequency
    ggplot2::facet_wrap(~ .data$f) +
    # scale and label axes
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(-pi, pi), breaks = seq(-pi, pi / 2, pi / 2), labels = c("pi", "3/2pi", "0", "pi/2")) +
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = seq(0, yMax, yMax / 10), limits = c(0, yMax)) +
    ggplot2::ylab("Amplitude") +
    ggplot2::xlab("Phase Angle") +
    # theme
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank())

  print(tmpPlot)
}

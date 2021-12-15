#' plot_phase
#'
#' @description \code{plot_phase}
#' Plots the phase distributions
#'
#' @param bosc BOSC-Object
#' @param freqs A vector of frequencies (in Hz) to plot, defaults to all frequencies available
#'
#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export plot_phase
#' @name plot_phase
#'

#'
plot_phase <- function(bosc, freqs = unique(bosc$data$ss$real$fft$f)) {



  # subset to frequencies of interest
  d <- bosc$data$ss$real$fft %>%
    dplyr::filter(.data$f %in% !!freqs)

  # get mean vector of phases by summing unit vectors in complex plane (ignoring amplitudes)
  l <- bosc$data$ss$real$fft %>%
    dplyr::filter(.data$f %in% !!freqs) %>%
    dplyr::mutate(complex = complex(modulus = 1, argument = .data$phase)) %>%
    dplyr::group_by(.data$f) %>%
    dplyr::summarize(complex = mean(.data$complex)) %>%
    dplyr::mutate(
      length = Mod(.data$complex),
      phase = Arg(.data$complex)
    )

  yMax <- 1.01

  # plot
  tmpPlot <- ggplot2::ggplot(d, ggplot2::aes(x = .data$phase, y = 1)) +
    # single subject vectors
    ggplot2::geom_segment(ggplot2::aes(x = .data$phase, xend = .data$phase, y = 0, yend = 1),
      size = 1, color = "grey40", alpha = 1,
      arrow = ggplot2::arrow(type = "closed", length = ggplot2::unit(yMax / 20, "npc"))
    ) +
    # mean unit phase vectors
    ggplot2::geom_segment(
      data = l, ggplot2::aes(x = .data$phase, xend = .data$phase, y = 0, yend = length), size = 2.25, color = "black",
      arrow = ggplot2::arrow(type = "closed", length = ggplot2::unit(yMax / 20, "npc"))
    ) +
    # initialize polar plot
    ggplot2::coord_polar(start = pi / 2, direction = -1) +
    # split by frequency
    ggplot2::facet_wrap(~ .data$f) +
    # scale and label axes
    ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(-pi, pi), breaks = seq(-pi, pi / 2, pi / 2), labels = c("pi", "3/2pi", "0", "pi/2")) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, yMax)) +
    ggplot2::ylab("Amplitude") +
    ggplot2::xlab("Phase Angle") +
    # theme
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 12)
    )

  print(tmpPlot)
}

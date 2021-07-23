#' plot_spectrum
#'
#' @description \code{plot_spectrum}
#' Plots the FFT frequency spectrum.
#'
#' @param bosc BOSC-Object
#' @param levels Which level shall be plotted? "ss", "ga" or "merged_spectra"
#'
#' @return A BOSC-Object
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @export plot_spectrum
#' @name plot_spectrum
#'

plot_spectrum <- function(bosc, levels = "ss-merged_spectra-ga") {


  # get tests
  if(!is.character(levels)){
    stop("Argument levels must be a character.")
  }else{
    iLevel_list <- split_string_arg(levels, "-")
  }


  for(iLevel in iLevel_list){

    if(iLevel != "ss"){
      tmpPlot = ggplot2::ggplot(data = bosc$tests$fft[[iLevel]]$amp$results,
                                ggplot2::aes(x = .data$f, y = .data$observed)) +
        # data
        ggplot2::geom_line(size = 2) +
        ggplot2::geom_point(size = 3) +
        # threshold
        ggplot2::geom_line(ggplot2::aes(x = .data$f, y = .data$crit_value), size = 1, linetype = "dotted")+
        # scale and label axes
        ggplot2::scale_x_continuous(expand = c(0,0))+
        ggplot2::scale_y_continuous(expand = c(0,0))+
        ggplot2::ylab('Frequency') +
        ggplot2::xlab('Amplitude') +
        ggplot2::ggtitle(iLevel) +
        # theme
        ggplot2::theme_minimal()
    }else{
      tmpPlot = ggplot2::ggplot(data = bosc$tests$fft[[iLevel]]$amp$results,
                                ggplot2::aes(x = .data$f, y = .data$observed, group = .data$subj)) +
        # data
        ggplot2::geom_line(ggplot2::aes(color = .data$subj), size = 1.5) +
        ggplot2::geom_point(ggplot2::aes(color = .data$subj), size = 2.5) +
        # threshold
        ggplot2::geom_line(ggplot2::aes(x = .data$f, y = .data$crit_value, color = .data$subj), size = 1, linetype = "dotted")+
        # scale and label axes
        ggplot2::scale_x_continuous(expand = c(0,0))+
        ggplot2::scale_y_continuous(expand = c(0,0))+
        ggplot2::ylab('Frequency') +
        ggplot2::xlab('Amplitude') +
        ggplot2::ggtitle(iLevel) +
        # theme
        ggplot2::theme_minimal()
    }

    print(tmpPlot)
  }


}

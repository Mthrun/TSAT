ShortTimeFT = function(Datavec, LenWindow = 8, TypeWindow = 'gaussian', GaussianApha = 1, PlotIt = TRUE, Window, ...) {
  # Always use the supplied signal. A free variable can read unrelated global data.
  if (!is.logical(PlotIt) || length(PlotIt) != 1L || is.na(PlotIt))
    stop("PlotIt must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(LenWindow) || length(LenWindow) != 1L || !is.finite(LenWindow) ||
    LenWindow < 2L || LenWindow > .Machine$integer.max || LenWindow != floor(LenWindow))
    stop("LenWindow must be one integer >= 2.", call. = FALSE)
  LenWindow <- as.integer(LenWindow)
  if (!is.numeric(Datavec) || any(!is.finite(Datavec)) || length(Datavec) < LenWindow)
    stop("Supply a finite signal at least as long as LenWindow.")
  switch(TypeWindow,
    hanning = {
      if (!requireNamespace("e1071", quietly = TRUE))
        stop("Install the optional package 'e1071' to use this method.", call. = FALSE)
      out = e1071::stft(Datavec, win = LenWindow, ...)
      Spec = out$values
    },
    gaussian = {
      # GaborTransformation
      if (!requireNamespace("signal", quietly = TRUE))
        stop("Install the optional package 'signal' to use this method.", call. = FALSE)
      Window = signal::gausswin(n = LenWindow, w = GaussianApha)
      out = signal::specgram(Datavec, window = Window, ...)
      Spec = abs(out$S)
      Spec = t(Spec)
    },
    specific = {
      if (!requireNamespace("signal", quietly = TRUE))
        stop("Install the optional package 'signal' to use this method.", call. = FALSE)
      out = signal::specgram(Datavec, window = Window, ...)
      Spec = abs(out$S)
      Spec = t(Spec)
    },
    {
      if (!requireNamespace("signal", quietly = TRUE))
        stop("Install the optional package 'signal' to use this method.", call. = FALSE)
      out = signal::specgram(Datavec, n = LenWindow, ...)
      Spec = abs(out$S)
      Spec = t(Spec)
    }
  )
  if (PlotIt) {
    if (!requireNamespace("colorRamps", quietly = TRUE))
      stop("Install the optional package 'colorRamps' to use this method.", call. = FALSE)
    ColorRamp = colorRamps::blue2green2red(1024)
    graphics::image(Spec, axes = F, col = ColorRamp, xlab = 'Time', ylab = 'Frequency')
  }

  return(list(Amplitude = Spec, Spectrogram = out))
}

# Plot the Pareto density for each calendar group.
PlotSeasonalDensity <- function(Time, Data, Period = c("month", "weekday", "week"),
                                ParetoRadius = NULL, Scale = c("density", "height"),
                                Colors = NULL, Xlab = "Value", Title = NULL,
                                TimeZone = NULL, PlotIt = TRUE) {
  Period <- match.arg(Period)
  Scale <- match.arg(Scale)
  if (!is.logical(PlotIt) || length(PlotIt) != 1L || is.na(PlotIt))
    stop("PlotIt must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(Data) || is.complex(Data) || !is.null(dim(Data)) ||
    !length(Data))
    stop("Data must be a nonempty numeric vector.", call. = FALSE)
  if (!inherits(Time, c("Date", "POSIXct", "POSIXlt")) ||
    length(Time) != length(Data))
    stop("Time must contain one Date or POSIXt value per observation.", call. = FALSE)
  if (!is.null(ParetoRadius) &&
    (!is.numeric(ParetoRadius) || is.complex(ParetoRadius) ||
      length(ParetoRadius) != 1L || !is.finite(ParetoRadius) || ParetoRadius <= 0))
    stop("ParetoRadius must be NULL or one positive finite number.", call. = FALSE)
  if (is.null(TimeZone)) {
    zone <- attr(Time, "tzone")
    TimeZone <- if (length(zone) && nzchar(zone[1L])) zone[1L] else "UTC"
  }
  if (!is.character(TimeZone) || length(TimeZone) != 1L || is.na(TimeZone) ||
    !TimeZone %in% c("UTC", "GMT", OlsonNames()))
    stop("TimeZone must be a valid time zone name.", call. = FALSE)
  for (label in list(Xlab = Xlab, Title = Title)) {
    if (!is.null(label) &&
      (!is.character(label) || length(label) != 1L || is.na(label)))
      stop("Xlab and Title must be NULL or one character string.", call. = FALSE)
  }
  if (!requireNamespace("DataVisualizations", quietly = TRUE))
    stop("Install the optional package 'DataVisualizations' to use this method.", call. = FALSE)

  # Convert clock times in the selected zone before assigning calendar groups.
  # A UTC conversion can put an observation in the wrong day or month.
  dates <- if (inherits(Time, "Date")) Time else
    as.Date(as.POSIXct(Time), tz = TimeZone)
  keep <- is.finite(as.numeric(dates)) & is.finite(Data)
  omitted <- which(!keep)
  if (!any(keep))
    stop("No finite observations have valid dates.", call. = FALSE)
  if (length(omitted))
    warning(length(omitted), " observation(s) with invalid dates or values were omitted.",
      call. = FALSE
    )
  dates <- dates[keep]
  values <- Data[keep]
  group <- switch(Period,
    month = as.integer(format(dates, "%m")),
    weekday = as.integer(format(dates, "%u")),
    week = as.integer(format(dates, "%V"))
  )
  labels <- switch(Period,
    month = month.name,
    weekday = c(
      "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
      "Saturday", "Sunday"
    ),
    week = sprintf("Week %02d", 1:53)
  )
  nGroups <- length(labels)
  if (is.null(Colors)) {
    Colors <- if (Period == "month") {
      c(
        "#507FB4", "#76ACC9", "#A2CFDC", "#CEE3DD", "#F3E5A6", "#FCBB79",
        "#ED7156", "#E96C52", "#F7B878", "#DCE8B8", "#91C1D0", "#507FB4"
      )
    } else grDevices::colorRampPalette(c("#507FB4", "#A2CFDC", "#F3C780"))(nGroups)
  }
  if (!is.character(Colors) || !length(Colors) || anyNA(Colors) ||
    !length(Colors) %in% c(1L, nGroups))
    stop("Colors must contain one color or one color per calendar group.", call. = FALSE)
  tryCatch(grDevices::col2rgb(Colors),
    error = function(e) stop("Colors contains an invalid color.", call. = FALSE)
  )
  Colors <- rep(Colors, length.out = nGroups)
  names(Colors) <- labels

  groups <- data.frame(
    Group = seq_len(nGroups), Label = labels,
    N = tabulate(group, nbins = nGroups),
    Unique = integer(nGroups), Status = "empty",
    ParetoRadius = NA_real_
  )
  estimates <- vector("list", nGroups)
  points <- vector("list", nGroups)
  for (i in seq_len(nGroups)) {
    x <- values[group == i]
    distinct <- sort(unique(x))
    groups$Unique[i] <- length(distinct)
    if (!length(x)) next
    # A constant series has a point mass, not a continuous density.
    # Small samples use points because an automatic radius can be unstable.
    if (length(x) < 10L || length(distinct) < 5L) {
      groups$Status[i] <- if (length(distinct) == 1L) "constant" else "insufficient"
      points[[i]] <- data.frame(
        Group = i, Value = distinct,
        Baseline = nGroups + 1L - i
      )
      next
    }
    estimate <- DataVisualizations::ParetoDensityEstimation(
      x,
      paretoRadius = ParetoRadius, PlotIt = FALSE, Silent = TRUE
    )
    grid <- as.numeric(estimate$kernels)
    density <- as.numeric(estimate$paretoDensity)
    if (length(grid) < 2L || length(density) != length(grid) ||
      any(!is.finite(grid)) || any(diff(grid) <= 0) ||
      any(!is.finite(density)) || any(density < 0) || max(density) <= 0)
      stop("Pareto density estimation failed for ", labels[i], ".", call. = FALSE)
    groups$Status[i] <- "estimated"
    groups$ParetoRadius[i] <- estimate$paretoRadius
    # Keep the native grid and density. Further smoothing would change the estimate.
    estimates[[i]] <- data.frame(Group = i, Value = grid, Density = density)
  }
  density <- do.call(rbind, estimates)
  if (is.null(density))
    density <- data.frame(Group = integer(), Value = numeric(), Density = numeric())
  density$Baseline <- nGroups + 1L - density$Group
  if (nrow(density)) {
    # One multiplier preserves differences in density height between groups.
    # Equal peak heights are available only through an explicit Scale choice.
    divisor <- if (Scale == "density") max(density$Density) else
      stats::ave(density$Density, density$Group, FUN = max)
    density$Top <- density$Baseline + 0.85 * density$Density / divisor
  } else density$Top <- numeric()
  density$Label <- factor(labels[density$Group], levels = labels)
  points <- do.call(rbind, points)
  if (is.null(points))
    points <- data.frame(Group = integer(), Value = numeric(), Baseline = numeric())
  points$Label <- factor(labels[points$Group], levels = labels)

  if (is.null(Title)) Title <- switch(Period,
    month = "Monthly distributions",
    weekday = "Weekday distributions",
    week = "Weekly distributions"
  )
  caption <- if (Scale == "density") "All curves use the same density scale." else
    "Each curve is scaled to the same peak height."
  if (nrow(points))
    caption <- paste(caption, "\nDots: fewer than 10 observations or 5 distinct values.")
  # Local bindings prevent false global-variable reports from R CMD check.
  Value <- Baseline <- Top <- Label <- NULL
  plot <- ggplot2::ggplot() +
    ggplot2::geom_hline(
      yintercept = seq_len(nGroups), colour = "#D5D8DC",
      linewidth = 0.3
    ) +
    ggplot2::geom_ribbon(
      data = density,
      ggplot2::aes(x = Value, ymin = Baseline, ymax = Top, group = Label, fill = Label),
      colour = NA, alpha = 0.95
    ) +
    ggplot2::geom_point(
      data = points,
      ggplot2::aes(x = Value, y = Baseline, colour = Label), size = 2
    ) +
    ggplot2::scale_fill_manual(values = Colors, limits = labels, drop = FALSE) +
    ggplot2::scale_colour_manual(values = Colors, limits = labels, drop = FALSE) +
    ggplot2::scale_y_continuous(
      breaks = seq_len(nGroups),
      labels = rev(paste0(labels, "  (n=", groups$N, ")")),
      limits = c(0.7, nGroups + 1), expand = c(0, 0)
    ) +
    ggplot2::labs(
      x = Xlab, y = NULL, title = Title,
      subtitle = "Pareto density estimation", caption = caption
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_rect(fill = "#F5F6F7", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "#DEE1E4", linewidth = 0.3),
      axis.text.y = ggplot2::element_text(colour = "#333333"),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0)
    )
  if (PlotIt) print(plot)
  invisible(list(
    Plot = plot, Density = density, Groups = groups, Points = points,
    Omitted = omitted, Period = Period, TimeZone = TimeZone, Scale = Scale
  ))
}

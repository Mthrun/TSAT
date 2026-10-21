\name{PlotSeasonalDensity}
\alias{PlotSeasonalDensity}
\title{Plot Pareto Densities by Calendar Group}
\description{
Draw one filled density curve for each month, weekday, or ISO week.
Use \code{DataVisualizations::ParetoDensityEstimation} to estimate each curve.
}
\usage{
PlotSeasonalDensity(Time, Data, Period = c("month", "weekday", "week"),
  ParetoRadius = NULL, Scale = c("density", "height"),
  Colors = NULL, Xlab = "Value", Title = NULL,
  TimeZone = NULL, PlotIt = TRUE)
}
\arguments{
\item{Time}{A Date, POSIXct, or POSIXlt vector. Supply one date per observation.}
\item{Data}{A numeric vector of observations.}
\item{Period}{Calendar groups. Use \code{"month"} for January to December.
Use \code{"weekday"} for Monday to Sunday. Use \code{"week"} for ISO weeks
01 to 53. Groups are shown from top to bottom in calendar order.}
\item{ParetoRadius}{A positive radius for all estimated curves.
The default, \code{NULL}, selects a radius separately for each group.}
\item{Scale}{Use \code{"density"} for one vertical scale across all curves.
Use \code{"height"} to give each curve the same peak height.}
\item{Colors}{One color, or one color per calendar group, in calendar order.
Supply 12 colors for months, 7 for weekdays, or 53 for weeks.
The default uses a fixed palette.}
\item{Xlab}{A label for the horizontal axis. Include the measurement unit.}
\item{Title}{A plot title. The default title identifies the calendar groups.}
\item{TimeZone}{A time zone name for POSIXt input. The default uses the
time zone stored in \code{Time}, or UTC if no time zone is stored.
Date input is unchanged.}
\item{PlotIt}{If TRUE, display the plot. Always return the plot and estimates.}
}
\details{
This display is also called a ridgeline plot. Pool observations from all
years within each calendar group. Do not aggregate the observations into
monthly or weekly means. Repeated observations retain their full weight.
ISO weeks start on Monday. Dates at a year boundary can belong to an ISO
week in the adjacent year.

Omit observations with missing or infinite values or dates. Give a warning
and return the omitted row indices. Stop if no valid observations remain.
Show every calendar group, including empty groups. Each label includes the
number of valid observations.

Estimate a density only if a group has at least 10 observations and 5
distinct values. Show distinct observed values as dots for smaller groups
and constant groups. Do not replace a constant group with a continuous
density. These rules also apply when a radius is supplied.

Keep the density grid returned by ParetoDensityEstimation. Do not apply
additional smoothing. With \code{Scale = "density"}, all curves use one
vertical multiplier. With \code{Scale = "height"}, compare curve shapes;
their displayed peak heights no longer represent relative density.
Colors identify calendar groups. They do not encode a measured value.

The returned plot is a ggplot object. Use ggplot2 functions to change its
style or save it. A plot of all 53 weeks needs a tall output page.
}
\value{
An invisible list with these components:
\item{Plot}{The ggplot object.}
\item{Density}{A data frame with \code{Group}, \code{Value}, \code{Density},
\code{Baseline}, \code{Top}, and \code{Label}. The density values are the
unscaled estimates. Baseline and Top are display coordinates.}
\item{Groups}{A data frame with the group number, label, observation count
\code{N}, distinct value count \code{Unique}, \code{Status}, and selected
\code{ParetoRadius}. Status is \code{"estimated"}, \code{"empty"},
\code{"constant"}, or \code{"insufficient"}. The radius is NA for groups
without an estimated density.}
\item{Points}{The distinct values displayed for groups without an estimated
density. Columns are \code{Group}, \code{Value}, \code{Baseline}, and \code{Label}.}
\item{Omitted}{Indices of omitted input rows.}
\item{Period}{The selected calendar grouping.}
\item{TimeZone}{The selected time zone.}
\item{Scale}{The selected display scale.}
}
\seealso{\code{\link[DataVisualizations]{ParetoDensityEstimation}},
\code{\link{TempMelbourneAustralia}}}
\examples{
if (requireNamespace("DataVisualizations", quietly = TRUE)) {
  data(TempMelbourneAustralia)
  # Use two years to keep the package example small.
  d <- head(TempMelbourneAustralia, 730)
  monthly <- PlotSeasonalDensity(d$Time, d$Temp,
    Xlab = "Daily minimum temperature (degrees C)")
  stopifnot(sum(monthly$Groups$N) == nrow(d))

  weekdays <- PlotSeasonalDensity(d$Time, d$Temp,
    Period = "weekday", PlotIt = FALSE)
  stopifnot(nrow(weekdays$Groups) == 7L)
  # Save the plot with ggplot2::ggsave(filename, plot = monthly$Plot).
}
}
